;;; Bugfixes of various types.

;; Some compatibility stuff for xemacs.
(when (featurep 'xemacs)
  (require 'circe-fix-xemacs))

;; completion-at-point and changing buffer
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=12619
(when (and (require 'cl-lib nil t)
           (not (boundp 'completion--all-sorted-completions-location)))
  (require 'circe-fix-minibuffer))

(provide 'circe-fixes)
