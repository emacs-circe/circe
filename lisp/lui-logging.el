;;; lui-logging.el --- Logging support for lui

;; Copyright (C) 2006  Jorgen Schaefer

;; Version: 1.0
;; Keywords: Lui, Circe, IRC
;; Author: Jorgen Schaefer <forcer@forcix.cx>
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

;; This lui module enables logging. Lui applications can change the
;; values of `lui-logging-format-arguments' to provide further
;; possibilities of customizing `lui-logging-file-format' for users.

;;; Code:

(require 'lui-format)

(defgroup lui-logging nil
  "Logging support."
  :prefix "lui-logging-"
  :group 'lui)

(defcustom lui-logging-format "[%T] {text}"
  "The format used for log file entries.
This is first passed through `format-time-string' and then through
`lui-format'. The following format strings exist:

  {text} - the text to be logged"
  :type 'string
  :group 'lui-logging)

(defcustom lui-logging-directory "~/.logs"
  "The directory where log files are stored."
  :type 'directory
  :group 'lui-logging)

(defcustom lui-logging-file-format "{buffer}_%Y-%m-%d.txt"
  "The format to be used for the log file name.
This is first passed through `format-time-string', and then
through `lui-format'. Possible lui format strings are:

  {buffer} - the buffer name where the logging happened.

Lui applications can provide further format strings. See
`lui-logging-format-arguments' in the appropriate buffer."
  :type 'string
  :group 'lui-logging)

(defvar lui-logging-format-arguments nil
  "A list of arguments to be passed to `lui-format'.
This can be used to extend the formatting possibilities of the
file name for lui applications.")
(make-variable-buffer-local 'lui-logging-format-arguments)

(defvar lui-logging-queue nil
  "The queue of pending log messages, flushed every n seconds by a timer.")
(make-variable-buffer-local 'lui-logging-queue)

(defvar lui-logging-flush-timer nil
  "The timer used to flush lui-logged buffers")
(make-variable-buffer-local 'lui-logging-flush-timer)

(defvar lui-logging-file-name ""
  "Holds the name of the current log file")
(make-variable-buffer-local 'lui-logging-file-name)

(defun enable-lui-logging ()
  "Enable lui logging."
  (interactive)
  (add-hook 'lui-pre-output-hook 'lui-logging)
  (add-hook 'kill-buffer-hook 'disable-lui-logging)
  (setq lui-logging-timer
        (run-with-timer (random 30) 30 #'lui-logging-flush)))

(defun disable-lui-logging ()
  "Disable lui logging."
  (interactive)
  (remove-hook 'lui-pre-output-hook 'lui-logging)
  (cancel-timer lui-logging-timer)
  (lui-logging-flush))


(defun lui-logging-flush ()
  "Flush out the lui-logging queue."
  (message (format-time-string "%T lui-logging-flush"))
  (let* ((file lui-logging-file-name)
         (dir (file-name-directory file))
         (queue lui-logging-queue))
    (when (not (file-directory-p dir))
      (make-directory dir t))
    (with-temp-buffer
      (when queue
        (write-region
         (apply #'concat ;; yes, i know using nreverse is fucking gross. i don't care. 8)
                (nreverse queue))
         nil file t 'nomessage)))
    (setq lui-logging-queue nil)))

(defun lui-logging ()
  "Append the to-be-logged string to the output queue.
This should be added to `lui-pre-output-hook'."
  (message (format-time-string "%T lui-logging"))
  (let ((log-format lui-logging-format)
          (concat lui-logging-directory "/"
          (downcase
           (apply 'lui-format
                  (format-time-string lui-logging-file-format)
                  :buffer (buffer-name (current-buffer))
                  lui-logging-format-arguments)))
        (text (buffer-string)))
    (with-temp-buffer
      (push
       (with-output-to-string
           (princ (apply #'lui-format
                   (format-time-string log-format)
                   :text text
                   lui-logging-format-arguments)))
       lui-logging-queue))))

(provide 'lui-logging)
;;; lui-logging.el ends here
