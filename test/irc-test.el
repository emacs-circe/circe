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
            :family nil
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
              proc "line2"))

    (it "should not handle a line received while others are processed"
      ;; If you wonder what this is about, see the docstring of
      ;; `irc--filter-running-p'
      (spy-on 'irc--handle-line :and-call-fake
              (lambda (proc line)
                (when (equal line "line1")
                  (irc--filter proc "line3\n"))))

      (irc--filter proc "line1\nline2\n")

      (expect (spy-calls-all-args 'irc--handle-line)
              :to-equal
              `((,proc "line1")
                (,proc "line2")
                (,proc "line3"))))))

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

(defun client-messages ()
  (mapcar #'cadr (spy-calls-all-args 'irc-send-raw)))

(describe "The registration handler"
  (let (proc table)
    (before-each
      (setq proc (start-process "test" nil "cat")
            table (irc-handler-table))
      (irc-connection-put proc :handler-table table)
      (irc-connection-put proc :nick "My_Nick")
      (irc-connection-put proc :user "username")
      (irc-connection-put proc :mode 8)
      (irc-connection-put proc :realname "My Real Name")

      (spy-on 'irc-send-raw)

      (irc-handle-registration table))

    (describe "on conn.connected"
      (it "should send the standard registration on connect"
        (irc-event-emit proc "conn.connected")

        (expect (client-messages)
                :to-equal
                '("NICK My_Nick"
                  "USER username 8 * :My Real Name")))

      (it "should set the connection state to connected"
        (expect (irc-connection-state proc)
                :not :to-be
                'connected)

        (irc-event-emit proc "conn.connected")

        (expect (irc-connection-state proc)
                :to-be
                'connected))

      (it "should send a PASS message if a password is given"
        (irc-connection-put proc :pass "top-secret")

        (irc-event-emit proc "conn.connected")

        (expect (client-messages)
                :to-equal
                '("PASS top-secret"
                  "NICK My_Nick"
                  "USER username 8 * :My Real Name")))

      (it "should send a CAP request if the connection specifies it"
        (irc-connection-put proc :cap-req '("sasl"))

        (irc-event-emit proc "conn.connected")

        (expect (client-messages)
                :to-equal
                '("CAP LS"
                  "NICK My_Nick"
                  "USER username 8 * :My Real Name"))))

    (describe "on conn.disconnected"
      (it "should set the connection state to disconnected"
        (expect (irc-connection-state proc)
                :not :to-be
                'disconnected)

        (irc-event-emit proc "conn.disconnected")

        (expect (irc-connection-state proc)
                :to-be
                'disconnected)))

    (describe "on 001 RPL_WELCOME"
      (it "should set the connection stat to registered"
        (expect (irc-connection-state proc)
                :not :to-be
                'registered)

        (irc-event-emit proc "001" "irc.server" "My_Nick" "Welcome!")

        (expect (irc-connection-state proc)
                :to-be
                'registered))

      (it "should emit the irc.registered event"
        (let ((registered nil))
          (irc-handler-add table "irc.registered"
                           (lambda (conn event my-nick)
                             (setq registered my-nick)))

          (irc-event-emit proc "001" "irc.server" "My_Nick" "Welcome!")

          (expect registered :to-equal "My_Nick"))))

    (describe "on a CAP message"
      (it "should do the full negotiation"
        (irc-connection-put proc :cap-req '("multi-prefix"))
        (irc-event-emit proc "conn.registered")
        (spy-calls-reset 'irc-send-raw)
        (irc-event-emit proc "CAP" "irc.server" "*" "LS" "multi-prefix")
        (irc-event-emit proc "CAP" "irc.server" "*" "ACK" "multi-prefix")

        (expect (client-messages)
                :to-equal
                '("CAP REQ multi-prefix"
                  "CAP END"))))

    (describe "on SASL authentication"
      (it "should do the full negotiation"
        (irc-connection-put proc :cap-req '("sasl"))
        (irc-connection-put proc :sasl-username "my_nick")
        (irc-connection-put proc :sasl-password "top-secret")
        (irc-event-emit proc "conn.registered")
        (spy-calls-reset 'irc-send-raw)
        (irc-event-emit proc "CAP" "irc.server" "*" "LS" "sasl")
        (irc-event-emit proc "CAP" "irc.server" "*" "ACK" "sasl")
        (irc-event-emit proc "AUTHENTICATE" nil "+")

        (expect (client-messages)
                :to-equal
                '("CAP REQ sasl"
                  "AUTHENTICATE PLAIN"
                  "AUTHENTICATE bXlfbmljawBteV9uaWNrAHRvcC1zZWNyZXQ="
                  "CAP END"))))))

(describe "The `irc-connection-state' function"
  (let (proc)
    (before-each
      (setq proc (start-process "test" nil "cat")))

    (it "should return the connection state"
      (irc-connection-put proc :connection-state 'registered)

      (expect (irc-connection-state proc)
              :to-be
              'registered))

    (it "should return connecting if nothing was set"
      (expect (irc-connection-state proc)
              :to-be
              'connecting))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Handler: Ping-Pong

;; irc-handle-ping-pong
(describe "The registration handler"
  (let (proc table)
    (before-each
      (setq proc (start-process "test" nil "cat")
            table (irc-handler-table))
      (irc-connection-put proc :handler-table table)

      (spy-on 'irc-send-raw)

      (irc-handle-ping-pong table))

    (it "should send PONG on a PING"
      (irc-event-emit proc "PING" "irc.server" "arg")

      (expect (client-messages)
              :to-equal
              '("PONG arg")))))
