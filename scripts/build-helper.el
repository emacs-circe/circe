(add-to-list 'load-path ".")

(defun bh-emacs-install-version ()
  "Print the version for the Emacs installation directory."
  (if (string-match "^\\(.*\\)\\.[0-9]+$" emacs-version)
      (message "%s" (match-string 1 emacs-version))
    (message "%s" emacs-version)))

(defun bh-generate-autoloads ()
  "Generate the autoload file on the command line."
  (let ((file (car command-line-args-left)))
    (require 'autoload)
    (let ((generated-autoload-file file))
      (setq command-line-args-left (cdr command-line-args-left))
      (batch-update-autoloads))))

(defun bh-elint-files ()
  "Elint all .el files in the current directory."
  (require 'elint)
  (elint-initialize)
  (mapc (lambda (file)
          (find-file file)
          (elint-current-buffer)
          (with-current-buffer (elint-get-log-buffer)
            (message "%s" (buffer-string)))
          (kill-buffer (current-buffer)))
        command-line-args-left))
