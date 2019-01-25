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

(describe "The `tracking-add-buffer' function"
  (it "should add buffers to `tracking-all-buffers'"
    (let ((tracking-all-buffers ()))
      (tracking-add-buffer (get-buffer-create "my-cool-buffer"))
      (expect tracking-all-buffers
              :to-equal '("my-cool-buffer"))))
  (it "should not add buffers to `tracking-all-buffers' if already present"
    (let ((tracking-all-buffers '("my-cool-buffer")))
      (tracking-add-buffer (get-buffer-create "my-cool-buffer"))
      (expect tracking-all-buffers
              :to-equal '("my-cool-buffer")))))
