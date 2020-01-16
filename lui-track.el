;;; lui-track.el --- Provides a bar to track the last read position

;; Copyright (C) 2016, 2019 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>

;; This file is part of LUI.

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

;; This allows you to track where you've last left off a buffer.

;; Use (enable-lui-track) to enable this mode globally. You can
;; customize `lui-track-behavior' to change when the track indicator
;; moves and `lui-track-indicator' to choose between a bar or a fringe
;; indicator. You can also use M-x lui-track-move to move the track
;; indicator manually, and M-x lui-track-jump-to-indicator to move to
;; the line marked by the indicator.

;;; Code:

(require 'lui)
(require 'tracking)
(require 'cl-macs)

(defgroup lui-track nil
  "Last read position tracking for LUI"
  :prefix "lui-track-"
  :group 'lui)

(defcustom lui-track-behavior 'before-switch-to-buffer
  "When to move the track bar.

The following values are possible.

before-switch-to-buffer (default)
  Move the bar to the bottom of the buffer when switching away
  from a buffer.

before-tracking-next-buffer
  Move the bar when switching to the next buffer using
  \\[tracking-next-buffer].

after-send
  Move the bar after sending a message."
  :type '(choice (const :tag "Before switching buffers"
                        before-switch-to-buffer)
                 (const :tag "Before tracking switch"
                        before-tracking-next-buffer)
                 (const :tag "After sending"
                        after-send))
  :group 'lui-track)

(defcustom lui-track-indicator 'bar
  "What indicator to use: either a fringe indicator or a full
line to mark last position."
  :type '(choice (const :tag "Full bar" bar)
                 (const :tag "Fringe indicator" fringe))
  :group 'lui-track)

(defface lui-track-bar
  '((((type graphic) (background light))
     :inherit default :background "dim gray" :height 0.1)
    (((type graphic) (background dark))
     :inherit default :background "light gray" :height 0.1)
    (((type tty))
     :inherit (font-lock-comment-face default) :underline t))
  "Track bar face"
  :group 'lui-track)

(defvar lui-track-bar-overlay nil)
(make-variable-buffer-local 'lui-track-bar-overlay)

(defun lui-track--move-pre-input ()
  (when (eq lui-track-behavior 'after-send)
    (lui-track-move)))

(defun lui-track-move ()
  "Move the track indicator down."
  (interactive)
  (when (derived-mode-p 'lui-mode)
    (cl-case lui-track-indicator
      ('fringe (when (not overlay-arrow-position)
                 (setq-local overlay-arrow-position (make-marker)))
               (set-marker overlay-arrow-position
                           (marker-position lui-output-marker)))
      ('bar (when (not lui-track-bar-overlay)
              (setq lui-track-bar-overlay
                    (make-overlay (point-min) (point-min)))
              (overlay-put lui-track-bar-overlay 'after-string
                           (propertize "\n" 'face 'lui-track-bar)))
            (move-overlay lui-track-bar-overlay
                          lui-output-marker lui-output-marker)))))

;;;###autoload
(defun enable-lui-track ()
  "Enable a bar or fringe indicator in Lui buffers that shows
where you stopped reading."
  (interactive)
  (defadvice switch-to-buffer (before lui-track activate)
    (when (and (eq lui-track-behavior 'before-switch-to-buffer)
               ;; Do not move the indicator if the buffer is displayed still
               (<= (length (get-buffer-window-list (current-buffer)))
                   1))
      (lui-track-move)))
  (defadvice tracking-next-buffer (before lui-track activate)
    (when (eq lui-track-behavior 'before-tracking-next-buffer)
      (lui-track-move)))
  (add-hook 'lui-pre-input-hook 'lui-track--move-pre-input))

;;;###autoload
(defun lui-track-jump-to-indicator ()
  "Move the point to the first unread line in this buffer.

If point is already there, jump back to the end of the buffer."
  (interactive)
  (let ((ipos (cl-case lui-track-indicator
                ('bar (when lui-track-bar-overlay
                        (overlay-start lui-track-bar-overlay)))
                ('fringe (when overlay-arrow-position
                           (marker-position overlay-arrow-position))))))
    (cond ((null ipos) (message "No unread messages"))
          ((= ipos (point)) (goto-char (point-max)))
          (t (goto-char ipos)))))

(provide 'lui-track)
;;; lui-track.el ends here
