;;; Bugfixes of various types.

;; Some compatibility stuff for xemacs.
(when (featurep 'xemacs)
  (require 'circe-fix-xemacs))

(provide 'circe-fixes)
