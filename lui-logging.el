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

(defun enable-lui-logging ()
  "Enable lui logging."
  (interactive)
  (add-hook 'lui-pre-output-hook 'lui-logging))

(defun disable-lui-logging ()
  "Disable lui logging."
  (interactive)
  (remove-hook 'lui-pre-output-hook 'lui-logging))

(defun lui-logging ()
  "Emit the current buffer contents as a log file entry.
This should be added to `lui-pre-output-hook'."
  (let ((file (concat lui-logging-directory
                      "/"
                      (apply 'lui-format
                             (format-time-string lui-logging-file-format)
                             :buffer (buffer-name (current-buffer))
                             lui-logging-format-arguments)))
        (log-format lui-logging-format)
        (text (buffer-string)))
    (let ((dir (file-name-directory file)))
      (when (not (file-directory-p dir))
        (make-directory dir t)))
    (with-temp-buffer
      (insert (lui-format (format-time-string log-format)
                          :text text))
      (write-region (point-min)
                    (point-max)
                    file t 'nomessage))))

(provide 'lui-logging)
;;; lui-logging.el ends here
