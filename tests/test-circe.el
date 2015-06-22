;; -*-lexical-binding: t-*-

(require 'circe)

(describe "Circe chat mode"
  (describe "creation function"
    (it "should have circe-server-buffer set in the mode hook"
      (let* ((csb-value nil)
             (circe-server-killed-confirmation nil)
             (circe-chat-mode-hook (list
                                    (lambda ()
                                      (setq csb-value circe-server-buffer))))
             buf)
        (with-temp-buffer
          (circe-server-mode)
          (spy-on 'irc-isupport--case-fold :and-return-value "foo")

          (setq buf (circe-server-create-chat-buffer "foo" 'circe-chat-mode))
          (kill-buffer buf)

          (expect csb-value :to-equal (current-buffer)))))))

(describe "The `circe-version' command"
  (it "should display the current version"
    (spy-on 'message)
    (spy-on 'circe--version :and-return-value "23.5")

    (call-interactively 'circe-version)

    (expect 'message
            :to-have-been-called-with "Circe %s" "23.5")))

(describe "The `circe-duration-string' function"
  (it "should handle very short amounts of time"
    (expect (circe-duration-string 0)
            :to-equal "a moment"))

  (it "should support second granularity"
    (expect (circe-duration-string 1)
            :to-equal "1 second")
    (expect (circe-duration-string 2)
            :to-equal "2 seconds"))

  (it "should support minute granularity"
    (expect (circe-duration-string 60)
            :to-equal "1 minute")
    (expect (circe-duration-string 61)
            :to-equal "1 minute 1 second")
    (expect (circe-duration-string 62)
            :to-equal "1 minute 2 seconds")
    (expect (circe-duration-string 122)
            :to-equal "2 minutes 2 seconds"))

  (it "should support monthly granularity"
    (expect (circe-duration-string (+ (* 24 60 60 30)
                                      120))
            :to-equal "1 month 2 minutes")))

(describe "Circe's completion facility"
  (let (proc channel-buffer server-buffer)
    (before-each
      (setq server-buffer (generate-new-buffer "*Test Server*"))
      (set-buffer server-buffer)
      (circe-server-mode)
      (setq proc (start-process "test" nil "cat")
            circe-server-process proc)
      (setq circe-server-killed-confirmation nil)
      (setq channel-buffer (circe-server-create-chat-buffer
                            "test" 'circe-channel-mode))
      (set-buffer channel-buffer)
      (setq circe-channel-killed-confirmation nil)
      (spy-on 'circe-server-nick :and-return-value "mynick")
      (spy-on 'circe-channel-nicks :and-return-value '("testnick"))
      (spy-on 'irc-connection-channel))

    (after-each
      (delete-process proc)
      (kill-buffer channel-buffer)
      (kill-buffer server-buffer))

    (it "should complete nicks with colon at the beginning of the input"
      (insert "TESTNICK")
      (completion-at-point)
      (expect (buffer-substring lui-input-marker (point-max))
              :to-equal "testnick: "))

    (it "should complete nicks without colon later in the input"
      (insert "some stuff TESTNICK")
      (completion-at-point)
      (expect (buffer-substring lui-input-marker (point-max))
              :to-equal "some stuff testnick "))))

(describe "Display of"
  (before-each
    (spy-on 'circe-display)
    (set-buffer (get-buffer-create "*Test*"))
    (spy-on 'circe-server-last-active-buffer
            :and-return-value (current-buffer)))

  (after-each
    (kill-buffer (current-buffer)))

  (describe "RPL_WHOISREPLY"
    (it "should show idle time"
      (circe-display-317 "sender" nil "317" "target" "nick"
                         "23" "seconds idle")

      (expect 'circe-display
              :to-have-been-called-with
              'circe-format-server-whois-idle
              :whois-nick "nick"
              :idle-seconds 23
              :idle-duration "23 seconds"))

    (it "should show idle time and signon time"
      (spy-on 'float-time :and-return-value (+ 1434995549 5))

      (circe-display-317 "sender" nil "317" "target" "nick"
                         "23" "1434995549" "seconds idle, signon time")

      (expect 'circe-display
              :to-have-been-called-with
              'circe-format-server-whois-idle-with-signon
              :whois-nick "nick"
              :idle-seconds 23
              :idle-duration "23 seconds"
              :signon-time 1434995549
              :signon-date (current-time-string
                            (seconds-to-time 1434995549))
              :signon-ago "5 seconds"))))
