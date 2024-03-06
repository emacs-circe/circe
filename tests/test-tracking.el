;;; test-tracking.el --- Automated tests for tracking.el -*- lexical-binding: t; -*-

(require 'tracking)

(describe "The `tracking-shorten' function"
  (it "should retain text properties"
    (expect (text-properties-at
             0
             (car (tracking-shorten
                   (list (propertize (buffer-name) 'face 'foo)))))
            :to-equal '(face foo))))

(describe "The `tracking-status' function"
  (it "should display 2 entries in the modeline if `tracking-max-mode-line-entries' is `nil'"
    (let* ((tracking-buffers (list "someBuffer1" "someBuffer2"))
           (buf1 (get-buffer-create (car tracking-buffers)))
           (buf2 (get-buffer-create (cadr tracking-buffers)))
           (tracking-max-mode-line-entries nil)
           (test-trace (prin1-to-string (tracking-status))))
      (expect test-trace :to-match "someBuffer1")
      (expect test-trace :to-match "someBuffer2")
      (kill-buffer buf1)
      (kill-buffer buf2)))

  (it "should display 1 entry in the modeline and a "+1" if `tracking-max-mode-line-entries' is `1'"
      (let* ((tracking-buffers (list "someBuffer1" "someBuffer2"))
             (buf1 (get-buffer-create (car tracking-buffers)))
             (buf2 (get-buffer-create (cadr tracking-buffers)))
             (tracking-max-mode-line-entries 1)
             (test-trace (prin1-to-string (tracking-status))))
        (expect test-trace :to-match "someBuffer1")
        (expect test-trace :to-match "\\+1")
        (expect test-trace :not :to-match "someBuffer2")
        (kill-buffer buf1)
        (kill-buffer buf2)))

  (it "should display nothing in the modeline if `tracking-max-mode-line-entries' is `0'"
    (let* ((tracking-buffers (list "someBuffer1" "someBuffer2"))
           (buf1 (get-buffer-create (car tracking-buffers)))
           (buf2 (get-buffer-create (cadr tracking-buffers)))
           (tracking-max-mode-line-entries 0))
      (expect (tracking-status) :to-equal "")
      (kill-buffer buf1)
      (kill-buffer buf2))))
