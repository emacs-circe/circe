;;; circe-highlight-all-nicks.el --- Highlight all nicks in the current channel

;; Copyright (C) 2005  Jorgen Schaefer

;; Author: Jorgen Schaefer <forcer@forcix.cx>

;; This file is part of Circe.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
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

;; This Circe module adds the ability to highlight every occurance of
;; a nick in the current channel in a message by other people.

;; To use it, put the following into your .emacs:

;; (require 'circe-highlight-all-nicks)
;; (enable-circe-highlight-all-nicks)

;;; Code:

(require 'circe)

(defface circe-highlight-all-nicks-face
  '((t (:foreground "green")))
  "The face used for nicks from the current channel.
See `enable-circe-highlight-all-nicks'."
  :group 'circe)

;;;###autoload
(defun enable-circe-highlight-all-nicks ()
  "Enable the Highlight Nicks module for Circe.
This module highlights all occurances of nicks in the current
channel in messages of other people."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'circe-channel-mode)
        (add-circe-highlight-all-nicks))))
  (add-hook 'circe-channel-mode-hook
            'add-circe-highlight-all-nicks))

(defun disable-circe-highlight-all-nicks ()
  "Disable the Highlight Nicks module for Circe.
See `enable-circe-highlight-all-nicks'."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'circe-channel-mode)
        (remove-circe-highlight-all-nicks))))
  (remove-hook 'circe-channel-mode-hook
               'add-circe-highlight-all-nicks))

(defun add-circe-highlight-all-nicks ()
  "Add `circe-highlight-all-nicks' to `lui-pre-output-hook'."
  (add-hook 'lui-pre-output-hook 'circe-highlight-all-nicks
            nil t))

(defun remove-circe-highlight-all-nicks ()
  "Remove `circe-highlight-all-nicks' from `lui-pre-output-hook'."
  (remove-hook 'lui-pre-output-hook 'circe-highlight-all-nicks
               t))

(defun circe-highlight-all-nicks ()
  "Highlight all occurances of nicks of the current channel in the message."
  (when (eq major-mode 'circe-channel-mode)
    (let ((body (text-property-any (point-min) (point-max)
                                   'lui-format-argument 'body))
          (nicks '())
          (regex nil))
      (when body
        (let ((channel-nicks (circe-channel-nicks)))
          (when channel-nicks
            (mapc (lambda (nick)
                    (when (not (circe-server-my-nick-p nick))
                      (setq nicks (cons nick nicks))))
                  channel-nicks)))
        (setq regex (regexp-opt nicks 'words))
        (goto-char body)
        (while (re-search-forward regex nil t)
          (add-text-properties (match-beginning 0)
                               (match-end 0)
                               '(face circe-highlight-all-nicks-face)))))))

(provide 'circe-highlight-all-nicks)
;;; circe-highlight-all-nicks.el ends here
