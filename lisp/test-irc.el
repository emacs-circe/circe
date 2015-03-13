(require 'irc)

(defmacro with-process (var &rest body)
  (declare (indent 1))
  `(let ((,var (start-process "test" nil "cat")))
     (unwind-protect
         (progn
           ,@body)
      (delete-process proc))))

(ert-deftest test-irc--filter ()
  "Make sure irc--filter handles multiple lines correctly."
  (cl-letf* ((called-procs nil)
             (called-lines nil)
             ((symbol-function 'irc--handle-line)
              (lambda (proc line)
                (push proc called-procs)
                (push line called-lines))))
    ;; Single line
    (with-process proc
      (irc--filter proc "line\n")
      (should (equal called-procs (list proc)))
      (should (equal called-lines (list "line"))))

    (setq called-procs nil
          called-lines nil)
    ;; Multi-line
    (with-process proc
      (irc--filter proc "line1\nline2\nline3\n")
      (should (equal called-procs (list proc proc proc)))
      (should (equal called-lines (list "line3" "line2" "line1"))))

    (setq called-procs nil
          called-lines nil)
    ;; Partial line
    (with-process proc
      (irc--filter proc "line1\nli")
      (should (equal called-procs (list proc)))
      (should (equal called-lines (list "line1")))
      (irc--filter proc "ne2\n")
      (should (equal called-procs (list proc proc)))
      (should (equal called-lines (list "line2" "line1"))))
    ))

(ert-deftest test-irc--parse ()
  "Parse IRC lines correctly."
  (should (equal (irc--parse "COMMAND")
                 '(nil "COMMAND")))
  (should (equal (irc--parse "COMMAND arg")
                 '(nil "COMMAND" "arg")))
  (should (equal (irc--parse "COMMAND arg1 arg2")
                 '(nil "COMMAND" "arg1" "arg2")))
  (should (equal (irc--parse "COMMAND arg1 arg2 :arg3 still arg3")
                 '(nil "COMMAND" "arg1" "arg2" "arg3 still arg3")))
  (should (equal (irc--parse ":sender COMMAND")
                 '("sender" "COMMAND")))
  (should (equal (irc--parse ":sender COMMAND arg")
                 '("sender" "COMMAND" "arg")))
  (should (equal (irc--parse ":sender COMMAND arg1 arg2")
                 '("sender" "COMMAND" "arg1" "arg2")))
  (should (equal (irc--parse ":sender COMMAND arg1 arg2 :arg3 still arg3")
                 '("sender" "COMMAND" "arg1" "arg2" "arg3 still arg3")))
  )
