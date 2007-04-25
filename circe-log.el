;;; circe-log.el --- Logging for circe

;; Copyright (C) 2007 Tassilo Horn <tassilo@member.fsf.org>

;; Version: 1.0
;; Keywords: Circe, IRC
;; Author: Tassilo Horn <tassilo@member.fsf.org>
;; URL: http://www.nongnu.org/circe/

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301  USA

;;; Commentary:

;; Logs all irc buffers matching `circe-log-buffer-regexp' and not matching
;; `circe-log-exlude-buffer-regexp' when they are killed.

;; To use it, put the following into your .emacs:

;; (require 'circe-log)
;; (enable-circe-log)

;;; Code:

(require 'circe)

(defcustom circe-log-buffer-regexp ".*"
  "Only buffers matching this regexp and not matching
`circe-log-exlude-buffer-regexp' will be logged."
 :type  'string
 :group 'circe)

(defcustom circe-log-exlude-buffer-regexp
  "^\\([Cc][Hh][Aa][Nn]\\|[Nn][Ii][Cc][Kk]\\)[Ss][Ee][Rr][Vv]"
  "Only buffers not matching this regexp and matching
`circe-log-buffer-regexp' will be logged. By default query
buffers for ChanServ and NickServ will be ignored."
 :type  'string
 :group 'circe)

(defcustom circe-log-directory "~/.circe-logs"
  "The directory in which circe's log files should reside."
 :type  'string
 :group 'circe)

;;;###autoload
(defun enable-circe-log ()
  "Enables automatic logging for all buffers matching
`circe-log-buffer-regexp' and not matching
`circe-log-exlude-buffer-regexp'."
  (interactive)
  (mapc (lambda (buf)
          (with-current-buffer buf
            (when (eq major-mode 'circe-chat-mode)
              (add-circe-log-buffer))))
        (buffer-list))
  (add-hook 'circe-chat-mode-hook
            'add-circe-log-buffer)
  (add-hook 'circe-chat-mode-hook
            'circe-log-insert))

(defun disable-circe-log ()
  "Disables automatic logging."
  (interactive)
  (mapc (lambda (buf)
          (with-current-buffer buf
            (when (eq major-mode 'circe-chat-mode)
              (remove-circe-log-buffer))))
        (buffer-list))
  (remove-hook 'circe-chat-mode-hook
               'add-circe-log-buffer)
  (remove-hook 'circe-chat-mode-hook
               'circe-log-insert))

(defun add-circe-log-buffer ()
  "Enables logging for the current buffer."
  (let ((buf-name (buffer-name (current-buffer))))
    (when (and (string-match circe-log-buffer-regexp buf-name)
               (not (string-match circe-log-exlude-buffer-regexp buf-name))) 
      (add-hook 'kill-buffer-hook 'circe-log-save-buffer t t))))

(defun remove-circe-log-buffer ()
  "Disables logging for the current buffer."
  (remove-hook 'kill-buffer-hook 'circe-log-save-buffer t))

(defun circe-log-file-name ()
  "Returns the file name of the logfile associated with the
current buffer."
  (let ((circe-buffer (buffer-name)))
    (concat circe-log-directory
            "/" circe-buffer
            "@" (with-circe-server-buffer circe-server-network))))

(defun circe-log-save-buffer ()
  "Saves the current buffer in a log file."
  (interactive)
  (when (not (file-exists-p circe-log-directory))
    (make-directory circe-log-directory))
  (write-file (circe-log-file-name)))

(define-key circe-channel-mode-map (kbd "C-x C-s") ' circe-log-save-buffer)

(defun circe-log-insert ()
  "Inserts the log file's content into the current buffer."
  (let ((log-buffer   " *circe log*")
        (logfile-name (circe-log-file-name))
        (circe-buffer (buffer-name)))
    (when (file-exists-p logfile-name)
      (set-buffer (get-buffer-create log-buffer))
      (insert-file-contents logfile-name)
      (goto-char (point-max))
      (insert (concat "\n\n;;\n"
                      ";; " (format-time-string "%c")
                      "\n;;\n"))
      (let ((str (buffer-string)))
        (set-buffer circe-buffer)
        (let ((buffer-read-only nil))
          (lui-insert str)))
      (kill-buffer log-buffer))))

(provide 'circe-log)
;;; circe-log.el ends here
