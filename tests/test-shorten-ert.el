;;; test-shorten-ert.el --- Shortening correctness and scaling -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'shorten)

(ert-deftest shorten-ert-preserves-prefix-results ()
  "Compare optimized shortening against the exhaustive prefix algorithm."
  (let ((strings '("" "!" "!a" "!ab" "a" "aa" "ab" "aba" "abb"
                   "b" "ba" "bar" "baz" "foo" "foot" "é" "éa" "éb"))
        (shorten-split-function #'list))
    (dolist (validator (list #'shorten-validate-component
                            (lambda (_prefix) t)
                            (lambda (prefix) (> (length prefix) 2))))
      (let ((shorten-validate-component-function validator))
        (dotimes (mask 256)
          (let* ((sample (cl-loop for string in strings for i from 0
                                  when (or (> i 7)
                                           (not (zerop (logand mask (ash 1 i)))))
                                  collect string))
                 (actual (shorten-strings sample)))
            (should (equal (mapcar #'car actual) sample))
            (dolist (string sample)
              (should (equal (cdr (assoc string actual))
                             (shorten-one string sample))))))))))

(ert-deftest shorten-ert-preserves-component-order-and-tails ()
  "Preserve component splitting, result order, and custom join tail counts."
  (let ((shorten-join-function
         (lambda (parts count)
           (format "%s{%s}" (shorten-join parts) count))))
    (should (equal (shorten-strings '("foo" "foo-bar" "foo-bar-baz"))
                   '(("foo-bar-baz" . "f-b-b{1}")
                     ("foo-bar" . "f-b{0}") ("foo" . "f{0}")))))
  (should (equal (shorten-strings '("foo-foo" "foo-bar" "foo-baz" "foo-quux"
                                   "bar-foo" "bar-bar" "bar-baz" "bar-quux"))
                 '(("foo-foo" . "f-f") ("foo-bar" . "f-bar")
                   ("foo-baz" . "f-baz") ("foo-quux" . "f-q")
                   ("bar-foo" . "b-f") ("bar-bar" . "b-bar")
                   ("bar-baz" . "b-baz") ("bar-quux" . "b-q")))))

(ert-deftest shorten-ert-bounds-prefix-comparisons ()
  "Compare each sibling token with at most two other tokens."
  (let ((original (symbol-function 'shorten-one))
        (calls 0)
        (shorten-split-function #'list)
        (strings (cl-loop for i below 3000 collect (format "buffer-%04d" i))))
    (cl-letf (((symbol-function 'shorten-one)
               (lambda (string others)
                 (cl-incf calls)
                 (should (<= (length others) 2))
                 (funcall original string others))))
      (should (= (length (shorten-strings strings)) 3000))
      (should (= calls 3000)))))

(provide 'test-shorten-ert)
;;; test-shorten-ert.el ends here
