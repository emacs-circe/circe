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

(describe "The `irc-send-raw' function"
  (let (proc current-time)
    (before-each
      (setq proc (start-process "test" nil "cat")
            current-time (float-time))
      (spy-on 'process-send-string)
      (spy-on 'run-at-time)
      (spy-on 'float-time :and-call-fake (lambda ()
                                           current-time)))

    (after-each
      (ignore-errors
        (delete-process proc)))

   (it "should send single messages immediately"
     (irc-send-raw proc "the line")

     (expect 'process-send-string
             :to-have-been-called-with
             proc "the line\n"))

   (it "should not create a timer for a single message"
     (irc-send-raw proc "the line")

     (expect 'run-at-time
             :not :to-have-been-called))

   (it "should prevent flooding"
     (dolist (line '("line1" "line2" "line3"
                     "line4" "line5" "line6"))
       (irc-send-raw proc line))

     (expect (spy-context-args
              (spy-calls-most-recent 'process-send-string))
             :to-equal
             `(,proc "line4\n")))

   (it "should continue sending after a delay"
     (dolist (line '("line1" "line2" "line3"
                     "line4" "line5" "line6"))
       (irc-send-raw proc line))
     (expect 'run-at-time
             :to-have-been-called)

     ;; Two minutes later
     (setq current-time (+ current-time 120))
     (irc-send--queue proc)

     (expect (spy-context-args
              (spy-calls-most-recent 'process-send-string))
             :to-equal
             `(,proc "line6\n")))

   (it "should drop lines if the flood queue is full and :drop is given"
     (dolist (line '("line1" "line2" "line3"
                     "line4" "line5" "line6"))
       (irc-send-raw proc line))

     (irc-send-raw proc "dropped" :drop)
     (setq current-time (+ current-time 120))
     (irc-send--queue proc)

     (expect (spy-context-args
              (spy-calls-most-recent 'process-send-string))
             :to-equal
             `(,proc "line6\n")))

   (it "should send items immediately if :nowait is given"
     (dolist (line '("line1" "line2" "line3"
                     "line4" "line5" "line6"))
       (irc-send-raw proc line))

     (irc-send-raw proc "priority" :nowait)

     (expect (spy-context-args
              (spy-calls-most-recent 'process-send-string))
             :to-equal
             `(,proc "priority\n")))))

(describe "The `irc-send-command'"
  (before-each
    (spy-on 'irc-send-raw))

  (it "should send properly-formatted commands"
    (irc-send-command 'proc "PRIVMSG" "#emacs" "Hello, World!")

    (expect 'irc-send-raw
            :to-have-been-called-with
            'proc "PRIVMSG #emacs :Hello, World!"))

  (it "should fail if any argument is not a string"
    (expect (lambda ()
              (irc-send-command 'proc "PRIVMSG" 23 "Hi!"))
            :to-throw))

  (it "should fail if any argument but the last has a space"
    (expect (lambda ()
              (irc-send-command 'proc "PRIVMSG" "#my channel" "Hello"))
            :to-throw)))

(describe "The send function"
  (before-each
    (spy-on 'irc-send-raw))

  (describe "`irc-send-AUTHENTICATE'"
    (it "should send an AUTHENTICATE message"
      (irc-send-AUTHENTICATE 'proc "1234567890abcdef")

      (expect 'irc-send-raw
              :to-have-been-called-with
              'proc "AUTHENTICATE 1234567890abcdef")))

  (describe "`irc-send-CAP'"
    (it "should send a CAP message"
      (irc-send-CAP 'proc "LS")

      (expect 'irc-send-raw
              :to-have-been-called-with
              'proc "CAP LS")))

  (describe "`irc-send-NICK'"
    (it "should send a NICK message"
      (irc-send-NICK 'proc "New_Nick")

      (expect 'irc-send-raw
              :to-have-been-called-with
              'proc "NICK New_Nick")))

  (describe "`irc-send-PASS'"
    (it "should send a PASS message"
      (irc-send-PASS 'proc "top-secret-password")

      (expect 'irc-send-raw
              :to-have-been-called-with
              'proc "PASS top-secret-password")))

  (describe "`irc-send-PONG'"
    (it "should send a PONG message to a single server"
      (irc-send-PONG 'proc "server1")

      (expect 'irc-send-raw
              :to-have-been-called-with
              'proc "PONG server1"))

    (it "should send a PONG message to another server"
      (irc-send-PONG 'proc "server1" "server2")

      (expect 'irc-send-raw
              :to-have-been-called-with
              'proc "PONG server1 server2")))

  (describe "`irc-send-USER'"
    (it "should send a USER message"
      (irc-send-USER 'proc "username" 8 "My Real Name (honest)")

      (expect 'irc-send-raw
              :to-have-been-called-with
              'proc "USER username 8 * :My Real Name (honest)"))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handler: Registration

;; This should have a useful abstraction for "server sends A, B, C,
;; expect client to have sent D, E, F"

;; irc-handle-registration
;; irc-connection-state

;;;;;;;;;;;;;;;;;;;;;;
;;; Handler: Ping-Pong

;; irc-handle-ping-pong
