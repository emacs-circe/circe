;;; Automated tests for circe.el

(require 'ert)
(require 'circe)

(ert-deftest circe-duration-string ()
  "Tests for `circe-duration-string'."
  (should (equal (circe-duration-string 1)
                 "1 second"))
  (should (equal (circe-duration-string 2)
                 "2 seconds"))
  (should (equal (circe-duration-string 60)
                 "1 minute"))
  (should (equal (circe-duration-string 61)
                 "1 minute 1 second"))
  (should (equal (circe-duration-string 62)
                 "1 minute 2 seconds"))
  (should (equal (circe-duration-string 122)
                 "2 minutes 2 seconds"))
  (should (equal (circe-duration-string (+ (* 24 60 60 30)
                                           120))
                 "1 month 2 minutes"))
  (should (equal (circe-duration-string 0)
                 "a moment")))
