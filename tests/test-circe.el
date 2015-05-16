;; -*-lexical-binding: t-*-

(require 'circe)

(describe "The `circe-version' command"
  (let (circe--version)
    (it "should display the current version"
      (spy-on 'message)

      (fset 'circe--version (lambda () "23.5"))
      (call-interactively 'circe-version)

      (expect 'message
              :to-have-been-called-with "Circe %s" "23.5"))))

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
  (let (channel-buffer server-buffer)
    (before-each
      (setq channel-buffer (generate-new-buffer "#test")
            server-buffer (generate-new-buffer "*Test Server*"))
      (set-buffer server-buffer)
      (circe-server-mode)
      (setq circe-server-killed-confirmation nil)
      (set-buffer channel-buffer)
      (circe-channel-mode "#test" server-buffer)
      (setq circe-channel-killed-confirmation nil)
      (spy-on 'circe-server-nick :and-return-value "mynick")
      (spy-on 'circe-channel-nicks :and-return-value '("testnick"))
      (spy-on 'irc-connection-channel))

    (after-each
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
