;;; Automated tests for shorten.el

(require 'ert)
(require 'shorten)

(ert-deftest shorten-one ()
  "Tests for `shorten-one'."
  (should (equal (let ((lst (list "foo" "bar" "baz" "quux")))
                   (shorten-one (car lst) lst))
                 "f"))
  (should (equal (let ((lst (list "foo" "fig")))
                   (shorten-one (car lst) lst))
                 "fo"))
  (should (equal (let ((lst (list "foo" "fig" "foot")))
                   (shorten-one (car lst) lst))
                 "foo"))
  (should (equal (let ((shorten-validate-component-function
                        (lambda (x) (> (length x) 1)))
                       (lst (list "foo" "bar" "baz" "quux")))
                   (shorten-one (car lst) lst))
                 "fo")))

(ert-deftest shorten-make-tree ()
  "Tests for `shorten-make-tree'."
  (should (equal (shorten-make-tree (list "foo"))
                 '(("foo" nil "foo" nil))))
  (should (equal (let ((shorten-split-function
                        (lambda (s) (split-string s "-"))))
                   (shorten-make-tree (list "foo" "bar")))
                 '(("bar" nil "bar" nil)
                   ("foo" nil "foo" nil))))
  (should (equal (let ((shorten-split-function
                        (lambda (s) (split-string s "-"))))
                   (shorten-make-tree (list "foo" "foo-bar")))
                 '(("foo" nil "foo"
                    ("bar" nil "foo-bar" nil)))))
  (should (equal (shorten-make-tree (list))
                 nil))
  (should (equal (shorten-make-tree (list "foo-bar" "foo"))
                 '(("foo" nil "foo" ("-" nil nil ("bar" nil "foo-bar" nil)))))))

(ert-deftest shorten-walk ()
  "Tests for `shorten-walk'."
  (should (equal (shorten-walk '())
                 nil))
  (should (equal (shorten-walk '(("foo" nil "foo" nil)))
                 '(("foo" . "f")))))

(ert-deftest shorten-strings ()
  "Tests for `shorten-strings'."
  (should (equal (shorten-strings (list))
                 nil))
  (should (equal (shorten-strings (list "foo"))
                 '(("foo" . "f"))))
  (should (equal (shorten-strings (list "foo" "bar"))
                 '(("foo" . "f") ("bar" . "b"))))
  (should (equal (shorten-strings (list "foo" "foo-bar"))
                 '(("foo-bar" . "f-b") ("foo" . "f"))))
  (should (equal (shorten-strings (list "fo" "f"))
                 '(("fo" . "fo") ("f" . "f"))))
  (should (equal (shorten-strings (list "foo-foo" "foo-bar" "foo-baz" "foo-quux"
                                    "bar-foo" "bar-bar" "bar-baz" "bar-quux"))
                 '(("foo-foo" . "f-f")
                   ("foo-bar" . "f-bar")
                   ("foo-baz" . "f-baz")
                   ("foo-quux" . "f-q")
                   ("bar-foo" . "b-f")
                   ("bar-bar" . "b-bar")
                   ("bar-baz" . "b-baz")
                   ("bar-quux" . "b-q")))))

(defun shorten-tests-tail-count-join-function (lst tail-count)
  (concat (shorten-join lst)
          "{" (number-to-string tail-count) "}"))

(ert-deftest shorten-strings-tail-count ()
  (let ((shorten-join-function #'shorten-tests-tail-count-join-function))
    (should (equal (shorten-strings (list "foo" "foo-bar"))
                   `(("foo-bar" . "f-b{1}")
                     ("foo" . "f{0}"))))
    (should (equal (shorten-strings (list "foo" "foo-bar" "foo-bar-baz"))
                   `(("foo-bar-baz" . "f-b-b{1}")
                     ("foo-bar" . "f-b{0}")
                     ("foo" . "f{0}"))))))
