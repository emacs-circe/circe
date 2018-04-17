;; -*-lexical-binding: t-*-

(require 'circe)
(require 'circe-color-nicks)

(describe "The `circe-color-nicks-message-blacklist' variable"
  (before-each
    (spy-on 'circe-nick :and-return-value "mynick")
    (spy-on 'circe-channel-nicks :and-return-value '("a" "b" "c" "d" "mynick")))

  (it "Should return all other nicks."
    (expect (circe-nick-color-nick-list)
            :to-equal '("a" "b" "c" "d")))
  (it "Should not include entries in the blacklist"
    (let ((circe-color-nicks-message-blacklist '("b" "d")))
      (expect (circe-nick-color-nick-list)
              :to-equal '("a" "c")))))

