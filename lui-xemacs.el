;; XEmacs compatibility for Lui
;; Thanks a lot to Brian Palmer!

(require 'overlay)

(unless (fboundp 'propertize)
  (defun propertize (string &rest props)
    (let ((str (copy-sequence string)))
      (while props
        (put-text-property 0 (length string)
                           (car props)
                           (cadr props)
                           str)
        (setq props (cddr props)))
      str)))

(unless (fboundp 'float-time)
  (require 'time-date)
  (defun float-time (&optional specified-time)
    (time-to-seconds (or specified-time
                         (current-time)))))


(require 'lui)

(provide 'lui-xemacs)
