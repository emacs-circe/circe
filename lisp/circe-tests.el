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

(ert-deftest emacs-completion-system ()
  "Tests for Emacs' completion code.

This broke so often, it got annoying."
  (with-temp-buffer
    (cl-flet ((test/completion-at-point ()
                 (list (copy-marker (point-min))
                       (copy-marker (point))
                       'test/completion-table))
              (test/completion-table (string pred action)
                 (if (eq action 'lambda)
                     nil
                   "test: ")))
      (let ((completion-at-point-functions '(test/completion-at-point)))
        (insert "TEST")
        (completion-at-point)
        (should (equal (buffer-string)
                       "test: "))))))

(ert-deftest circe-completion ()
  (with-temp-buffer
    (circe-channel-mode
     "#test"
     (with-temp-buffer
       (circe-server-mode)
       (setq circe-server-killed-confirmation nil)
       (current-buffer)))
    (circe-channel-add-user "test")
    (goto-char (point-max))
    (insert "TEST")
    (completion-at-point)
    ;; Colon after the first word
    (should (equal (buffer-substring lui-input-marker (point-max))
                   "test: "))
    (delete-region lui-input-marker (point-max))
    (insert "firstword TEST")
    (completion-at-point)
    ;; No colon after the second
    (should (equal (buffer-substring lui-input-marker (point-max))
                   "firstword test "))))
