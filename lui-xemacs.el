;; XEmacs compatibility for Lui
;; Thanks a lot to Brian Palmer!

(require 'overlay)

(unless (fboundp 'propertize)
  (defun propertize (string &rest props)
    ;; stolen from erc's erc-propertize
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

(unless (fboundp 'remove-overlays)
  (defun remove-overlays (&optional beg end name val)
    "Clear BEG and END of overlays whose property NAME has value VAL.
Overlays might be moved and or split. "
    ;; Stolen from planner as planner-remove-overlays
    (if (< end beg)
        (setq beg (prog1 end (setq end beg))))
    (save-excursion
      (dolist (o (overlays-in beg end))
        (when (eq (overlay-get o name) val)
          ;; Either push this overlay outside beg...end
          ;; or split it to exclude beg...end
          ;; or delete it entirely (if it is contained in beg...end).
          (if (< (overlay-start o) beg)
              (if (> (overlay-end o) end)
                  (progn
                    (move-overlay (planner-copy-overlay o)
                                  (overlay-start o) beg)
                    (move-overlay o end (overlay-end o)))
                (move-overlay o (overlay-start o) beg))
            (if (> (overlay-end o) end)
                (move-overlay o end (overlay-end o))
              (delete-overlay o))))))))

(defvar lui-string-to-number-sentinel nil
  "Internal variable used to track whether inside lui-string-to-number")
(defadvice string-to-number (around lui-string-to-number activate)    
  (if lui-string-to-number-sentinel
      ad-do-it
    (let* ((lui-string-to-number-sentinel t)
	   (data (ad-get-arg 0))
	   (positive-number-regexp "^\\s *\\+?[0-9.]")
	   (posnump (string-match positive-number-regexp data))
	   (number ad-do-it))
      (setq ad-return-value (if (or (floatp number)
				    (zerop number))
				number
			      (if (or (and posnump (< number 0))
				      (and (not posnump) (> number 0)))
				  (string-to-number (concat data ".0"))
				number))))))


;; We have to deal with the situation that, in emacs,
;; beginning-of-line takes you to the beginning of a region with the
;; `field' text property set. We install lui-beginning-of-line to
;; provide similar functionality in lui modes
(defun lui-beginning-of-line (&optional N)
  "Beginning of line, special cased to deal with lui input lines."
  (interactive "p")
  (declare (ignore N))
  (let ((current (point))
	(input   (marker-position lui-input-marker)))
    (if (< current input)
	(call-interactively 'beginning-of-line)
      (goto-char lui-input-marker) )))

(eval-after-load "lui"
  '(progn
     (define-key lui-mode-map (kbd "C-a") 'lui-beginning-of-line)
     (define-key lui-mode-map (kbd "<beginning>") 'lui-beginning-of-line)))

(provide 'lui-xemacs)
