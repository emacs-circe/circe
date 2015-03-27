;; -*-lexical-binding: t-*-

(require 'buttercup)
(require 'irc)

;;;;;;;;;;;;;;;;;;;;;;;
;;; Connection function

(describe "The `irc-connect' function"
  (before-each
    (spy-on 'make-tls-process :and-return-value t)
    (spy-on 'make-network-process :and-return-value t))

  (it "should call `make-network-process' if tls was not requested"
    (irc-connect :host "irc.local"
                 :service 6667)

    (expect 'make-network-process
            :to-have-been-called-with
            :name "irc.local" :host "irc.local" :service 6667
            :coding '(undecided . utf-8) :nowait t :noquery t
            :filter #'irc--filter :sentinel #'irc--sentinel
            :plist '(:host "irc.local" :service 6667) :keepalive t))

  (it "should call `make-tls-process' if tls was requested"
    (irc-connect :host "irc.local"
                 :service 6667
                 :tls t)

    (expect 'make-tls-process
            :to-have-been-called))

  (it "should return nil even when using non-tls connections"
    (expect (irc-connect :host "irc.local"
                         :service 6667)
            :to-equal
            nil)))

(describe "Connection options"
  (let (proc)
    (before-each
      (setq proc (start-process "test" nil "cat")))

    (after-each
      (ignore-errors
        (delete-process proc)))

    (it "should retrieve options set"
      (irc-connection-put proc :key "value")

      (expect (irc-connection-get proc :key)
              :to-equal
              "value"))))

(describe "The `irc--sentinel' function"
  (before-each
    (spy-on 'irc-event-emit))

  (it "should emit conn.failed for a failed event"
    (irc--sentinel 'proc "failed to do something\n")

    (expect 'irc-event-emit
            :to-have-been-called-with
            'proc "conn.failed"))

  (it "should emit conn.connected on an open event"
    (irc--sentinel 'proc "open\n")

    (expect 'irc-event-emit
            :to-have-been-called-with
            'proc "conn.connected"))

  (it "should emit conn.disconnected for a broken connection"
    (irc--sentinel 'proc "connection broken by remote peer\n")

    (expect 'irc-event-emit
            :to-have-been-called-with
            'proc "conn.disconnected"))

  (it "should emit conn.disconnected for a killed process"
    (irc--sentinel 'proc "killed\n")

    (expect 'irc-event-emit
            :to-have-been-called-with
            'proc "conn.disconnected"))

  (it "should emit conn.disconnected for a finished process"
    (irc--sentinel 'proc "finished\n")

    (expect 'irc-event-emit
            :to-have-been-called-with
            'proc "conn.disconnected"))

  (it "should ignore deleted processes"
    (irc--sentinel 'proc "deleted\n")

    (expect 'irc-event-emit
            :not :to-have-been-called))

  (it "should raise an error for unknown events"
    (expect (lambda () (irc--sentinel 'proc "bla bla\n"))
            :to-throw)))

(describe "The `irc--filter' function"
  (let (proc)
    (before-each
      (spy-on 'irc--handle-line)
      (setq proc (start-process "test" nil "cat")))

    (after-each
      (ignore-errors
        (delete-process proc)))

    (it "should handle single lines"
      (irc--filter proc "line\n")

      (expect 'irc--handle-line
              :to-have-been-called-with
              proc "line"))

    (it "should handle multiple lines at once"
      (irc--filter proc "line1\nline2\nline3\n")

      (expect (spy-calls-all-args 'irc--handle-line)
              :to-equal
              `((,proc "line1")
                (,proc "line2")
                (,proc "line3"))))

    (it "should handle partial lines"
      (irc--filter proc "line1\nli")

      (expect 'irc--handle-line
              :to-have-been-called-with
              proc "line1")

      (spy-calls-reset 'irc--handle-line)

      (irc--filter proc "ne2\n")

      (expect 'irc--handle-line
              :to-have-been-called-with
              proc "line2"))))

(describe "The `irc--handle-line' function"
  (before-each
    (spy-on 'irc-event-emit))

  (it "should emit an event for the command"
    (irc--handle-line 'proc ":sender COMMAND arg1 arg2")

    (expect 'irc-event-emit
            :to-have-been-called-with
            'proc "COMMAND" "sender" "arg1" "arg2")))

(describe "The `irc--parse' function"
  (it "should parse a command without anything else"
    (expect (irc--parse "COMMAND")
            :to-equal
            '(nil "COMMAND")))

  (it "should parse a command with a single argument"
    (expect (irc--parse "COMMAND arg")
            :to-equal
            '(nil "COMMAND" "arg")))

  (it "should parse a command with two arguments"
    (expect (irc--parse "COMMAND arg1 arg2")
            :to-equal
            '(nil "COMMAND" "arg1" "arg2")))

  (it "should parse a command with rest argument"
    (expect (irc--parse "COMMAND arg1 arg2 :arg3 still arg3")
            :to-equal
            '(nil "COMMAND" "arg1" "arg2" "arg3 still arg3")))

  (it "should parse a command with sender and no arguments"
    (expect (irc--parse ":sender COMMAND")
            :to-equal
            '("sender" "COMMAND")))

  (it "should parse a command with sender and a single argument"
    (expect (irc--parse ":sender COMMAND arg")
            :to-equal
            '("sender" "COMMAND" "arg")))

  (it "should parse a command with sender and two arguments"
    (expect (irc--parse ":sender COMMAND arg1 arg2")
            :to-equal
            '("sender" "COMMAND" "arg1" "arg2")))

  (it "should parse a command with sender and rest argument"
    (expect (irc--parse ":sender COMMAND arg1 arg2 :arg3 still arg3")
            :to-equal
            '("sender" "COMMAND" "arg1" "arg2" "arg3 still arg3"))))

(describe "The `irc-event-emit' function"
  (let (proc handler-table)
    (before-each
      (setq proc (start-process "test" nil "cat")
            handler-table (irc-handler-table))
      (irc-connection-put proc :handler-table handler-table))

    (it "should run the irc-handler for the event"
      (let ((called-with nil))
        (irc-handler-add handler-table "the.event"
                         (lambda (&rest args)
                           (setq called-with args)))

        (irc-event-emit proc "the.event" 1 2 3)

        (expect called-with
                :to-equal
                `(,proc "the.event" 1 2 3))))

    (it "should run the irc-handler for nil"
      (let ((called-with nil))
        (irc-handler-add handler-table nil
                         (lambda (&rest args)
                           (setq called-with args)))

        (irc-event-emit proc "the.event" 1 2 3)

        (expect called-with
                :to-equal
                `(,proc "the.event" 1 2 3))))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Event handler table

(describe "The event handler table API"
  (it "should run an event that was added"
    (let ((table (irc-handler-table))
          (called-with nil))
      (irc-handler-add table "the.event" (lambda (&rest args)
                                           (setq called-with args)))

      (irc-handler-run table "the.event" 1 2 3)

      (expect called-with :to-equal '(1 2 3)))))

;;;;;;;;;;;
;;; Sending

;; irc-send-raw
;; ? irc-send--queue
;; ? irc-send--internal
;; irc-send-command
;; ? irc--format-command
;; irc-send-AUTHENTICATE
;; irc-send-CAP
;; irc-send-NICK
;; irc-send-PASS
;; irc-send-PONG
;; irc-send-USER

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handler: Registration

;; This should have a useful abstraction for "server sends A, B, C,
;; expect client to have sent D, E, F"

;; irc-handle-registration
;; irc-connection-state

;;;;;;;;;;;;;;;;;;;;;;
;;; Handler: Ping-Pong

;; irc-handle-ping-pong
