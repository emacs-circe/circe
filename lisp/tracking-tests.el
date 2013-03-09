;;; Automated tests for shorten.el

(require 'ert)
(require 'tracking)

(ert-deftest tracking-shorten ()
  "Tests for `tracking-shorten'"
  (with-temp-buffer
    (should
     (text-properties-at
      0
      (car (tracking-shorten
            (list (propertize (buffer-name) 'face 'foo))))))))
