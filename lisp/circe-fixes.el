;;; Backports of bugfixes from Emacs bzr -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion-at-point and changing buffer
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=12619
(defun completion--cache-all-sorted-completions (comps)
  (add-hook 'after-change-functions
            'completion--flush-all-sorted-completions-after-change nil t)
  (setq completion-all-sorted-completions comps))

(defun completion--flush-all-sorted-completions-after-change (change-start change-end pre-change-length)
  (let ((start (nth 1 completion-in-region--data))
        (end (nth 2 completion-in-region--data)))
    (when (or
           ;; We don't even have completion data
           (not start)
           (not end)
           ;; Change completely before our completion, but our
           ;; completion didn't use markers.
           (and (<= change-end start)
                (not (and (markerp start)
                          (markerp end))))
           ;; Change overlaps our completion, regardless of markers.
           (not (or (< change-end start)
                    (< end change-start))))
      (completion--flush-all-sorted-completions))))

(defun completion--flush-all-sorted-completions ()
  (remove-hook 'after-change-functions
               'completion--flush-all-sorted-completions-after-change t)
  (setq completion-cycling nil)
  (setq completion-all-sorted-completions nil))

(defun completion-at-point ()
  "Perform completion on the text around point.
The completion method is determined by `completion-at-point-functions'."
  (interactive)
  (let ((res (run-hook-wrapped 'completion-at-point-functions
                               #'completion--capf-wrapper 'all)))
    (pcase res
      (`(,_ . ,(and (pred functionp) f)) (funcall f))
      (`(,hookfun . (,start ,end ,collection . ,plist))
       (let* ((completion-extra-properties plist)
              (completion-in-region-mode-predicate
               (lambda ()
                 (let ((old-start (car-safe (funcall hookfun))))
                   ;; We're still in the same completion field.
                   (condition-case error
                       (= old-start start)
                     (error
                      (eq old-start start)))))))
         (completion-in-region start end collection
                               (plist-get plist :predicate))))
      ;; Maybe completion already happened and the function returned t.
      (_ (cdr res)))))


(provide 'circe-fixes)
