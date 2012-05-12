;;; circe-color-nicks.el --- Color nicks in the channel

;; Copyright (C) 2012  Taylan Ulrich Bayırlı/Kammer

;; Version: 1.0
;; Keywords: Circe, IRC
;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; URL: https://github.com/TaylanUB/circe

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

;; This Circe module adds the ability to assign a color to each
;; nick in a channel.

;; Some ideas/code copied from rcirc-colors.el.

;; To use it, put the following into your .emacs:

;; (require 'circe-color-nicks)
;; (enable-circe-color-nicks)

;;; Code:

(require 'circe)

;;;###autoload
(defun enable-circe-color-nicks ()
  "Enable the Color Nicks module for Circe.
This module colors all encountered nicks in a cross-server fashion."
  (interactive)
  (mapc (lambda (buf)
          (with-current-buffer buf
            (when (eq major-mode 'circe-channel-mode)
              (add-circe-color-nicks))))
        (buffer-list))
  (add-hook 'circe-channel-mode-hook
            'add-circe-color-nicks))

(defun disable-circe-color-nicks ()
  "Disable the Color Nicks module for Circe.
See `enable-circe-color-nicks'."
  (interactive)
  (mapc (lambda (buf)
          (with-current-buffer buf
            (when (eq major-mode 'circe-channel-mode)
              (remove-circe-color-nicks))))
        (buffer-list))
  (remove-hook 'circe-channel-mode-hook
               'add-circe-color-nicks))

(defun add-circe-color-nicks ()
  "Add `circe-color-nicks' to `lui-pre-output-hook'."
  (add-hook 'lui-pre-output-hook 'circe-color-nicks))

(defun remove-circe-color-nicks ()
  "Remove `circe-color-nicks' from `lui-pre-output-hook'."
  (remove-hook 'lui-pre-output-hook 'circe-color-nicks))

(defun circe-color-distance (color1 color2)
  "Compute the difference between two colors
using the weighted Euclidean distance formula proposed on
<http://www.compuphase.com/cmetric.htm>.
Remember that every component for the formula is in the range of 0-xFF
and `color-values' will return a range of 0-FFFF. Thus, divide everything
by 256. This also helps preventing integer overflow."
  (let* ((dr (/ (- (nth 0 (color-values color1))
                   (nth 0 (color-values color2))) 256))
         (dg (/ (- (nth 1 (color-values color1))
                   (nth 1 (color-values color2))) 256))
         (db (/ (- (nth 2 (color-values color1))
                   (nth 2 (color-values color2))) 256))
         (red-mean (/ (+ (nth 0 (color-values color1))
                         (nth 0 (color-values color2)))
                      2 256)))
    (sqrt (+ (ash (* (+ 512 red-mean) dr dr) -8)
             (* 4 dg dg)
             (ash (* (- 767 red-mean) dr dr) -8)))))

(defvar circe-colors
  (let ((min-distance 200); heuristics
        (bg (face-background 'default))
        candidates)
    (dolist (item color-name-rgb-alist)
      (let ((color (car item)))
        (when (and (not (color-gray-p color))
                   (> (circe-color-distance color bg) min-distance))
          (setq candidates (cons color candidates)))))
    candidates)
  "Colors to use for nicks in circe.
By default, all the non-grey colors that are very different from
the default background are candidates.  This uses `circe-color-distance'
to compute distance between colors.

To check out the list, evaluate (list-colors-display circe-colors).")

(defvar circe-color-mapping (make-hash-table :test 'equal)
  "Hash-map mapping nicks to color names.")

(defun circe-color-nicks ()
  "Color all occurances of all nicks in the current channel."
  (when (eq major-mode 'circe-channel-mode)
    (let ((nickstart (text-property-any (point-min) (point-max)
                                      'lui-format-argument 'nick)))
      (when nickstart
        (goto-char nickstart)
        (let* ((nickend (next-property-change nickstart))
               (nick (buffer-substring-no-properties nickstart nickend)))
          (when (not (circe-server-my-nick-p nick))
            (let ((color (gethash nick circe-color-mapping)))
              (when (not color)
                (setq color (elt circe-colors (random (length circe-colors))))
                (puthash nick color circe-color-mapping))
              (put-text-property nickstart nickend 'face `(:foreground ,color)))))))
    (let ((body (text-property-any (point-min) (point-max)
                                   'lui-format-argument 'body))
          (nicks '())
          (regex nil))
      (when body
        (maphash (lambda (nick _)
                   (when (not (circe-server-my-nick-p nick))
                     (setq nicks (cons nick nicks))))
                 circe-color-mapping)
        (setq regex (regexp-opt nicks 'words))
        (goto-char body)
        (while (re-search-forward regex nil t)
          (put-text-property (match-beginning 0)
                             (match-end 0)
                             'face `(:foreground
                                     ,(gethash (match-string-no-properties 0)
                                               circe-color-mapping))))))))

(provide 'circe-color-nicks)
;;; circe-color-nicks.el ends here
