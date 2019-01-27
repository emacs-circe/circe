;;; Automated tests for tracking.el

(require 'tracking)

(describe "The `tracking-shorten' function"
  (it "should retain text properties"
    (expect (text-properties-at
             0
             (car (tracking-shorten
                   (list (propertize (buffer-name) 'face 'foo))
                   (list (buffer-name)))))
            :to-equal '(face foo))))

(describe "The `tracking-all-buffers' function"
  (it "should keep lui-mode buffers"
    (let ((lui-mode-buffer (get-buffer-create "will-be-lui-mode")))
      (with-current-buffer lui-mode-buffer
        (lui-mode)
        (expect (mapcar #'buffer-name (tracking-all-buffers))
                :to-equal '("will-be-lui-mode")))
      (kill-buffer lui-mode-buffer)))

  (it "should ignore non-lui-mode buffers"
    (let ((non-lui-mode-buffer (get-buffer-create "won't-be-lui-mode")))
      (expect (mapcar #'buffer-name (tracking-all-buffers))
              :to-equal '()))))
