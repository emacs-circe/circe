;;; lui-track-bar.el --- Provides a bar to track the last read position

;; Copyright (C) 2016 Vasilij Schneidermann <v.schneidermann@gmail.com>

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

;; Use (enable-lui-track-bar) to enable this mode globally. You can
;; customize `lui-track-bar-behavior' to change when the track bar
;; moves. You can also use M-x lui-track-bar-move to move the track
;; bar manually.

;;; Code:

(require 'lui)
(require 'lui-track)
(require 'tracking)

(defgroup lui-track-bar nil
  "Last read position tracking for LUI"
  :prefix "lui-track-bar-"
  :group 'lui)

(define-obsolete-variable-alias 'lui-track-bar-behavior 'lui-track-behavior
  "Circe 2.12")

;;;###autoload
(defun enable-lui-track-bar ()
  "Enable a bar indicator in Lui buffers that shows
where you stopped reading."
  (interactive)
  (setq lui-track-indicator 'bar)
  (enable-lui-track))
(make-obsolete 'enable-lui-track-bar 'enable-lui-track "Circe 2.12")

(define-obsolete-function-alias 'lui-track-bar-move 'lui-track-move
  "Circe 2.12")

(provide 'lui-track-bar)
;;; lui-track-bar.el ends here
