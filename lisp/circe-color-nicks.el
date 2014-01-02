;;; circe-color-nicks.el --- Color nicks in the channel

;; Copyright (C) 2012  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>

;; This file is part of Circe.

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
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'circe-channel-mode)
        (add-circe-color-nicks))))
  (add-hook 'circe-channel-mode-hook
            'add-circe-color-nicks))

(defun disable-circe-color-nicks ()
  "Disable the Color Nicks module for Circe.
See `enable-circe-color-nicks'."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'circe-channel-mode)
        (remove-circe-color-nicks))))
  (remove-hook 'circe-channel-mode-hook
               'add-circe-color-nicks))

(defun add-circe-color-nicks ()
  "Add `circe-color-nicks' to `lui-pre-output-hook'."
  (add-hook 'lui-pre-output-hook 'circe-color-nicks))

(defun remove-circe-color-nicks ()
  "Remove `circe-color-nicks' from `lui-pre-output-hook'."
  (remove-hook 'lui-pre-output-hook 'circe-color-nicks))

(defun circe-color-values (color)
  "Like `color-values', but also handle \"unspecified-bg\" and
\"unspecified-fg\"."
  (let ((values (color-values color)))
    (cond
     (values values)
     ((equal color "unspecified-bg") '(0 0 0))
     ((equal color "unspecified-fg") '(255 255 255)))))

(defun circe-color-distance (color1 color2)
  "Compute the difference between two colors using the weighted
Euclidean distance formula proposed on
<http://www.compuphase.com/cmetric.htm>.  Remember that every
component for the formula is in the range of 0-xFF and
`color-values' will return a range of 0-FFFF. Thus, divide
everything by 256. This also helps preventing integer overflow."
  (let* ((color1-values (circe-color-values color1))
         (color2-values (circe-color-values color2))
         (dr (/ (- (nth 0 color1-values)
                   (nth 0 color2-values)) 256))
         (dg (/ (- (nth 1 color1-values)
                   (nth 1 color2-values)) 256))
         (db (/ (- (nth 2 color1-values)
                   (nth 2 color2-values)) 256))
         (red-mean (/ (+ (nth 0 color1-values)
                         (nth 0 color2-values))
                      2 256)))
    (sqrt (+ (ash (* (+ 512 red-mean) dr dr) -8)
             (* 4 dg dg)
             (ash (* (- 767 red-mean) dr dr) -8)))))

(defsubst circe-perceived-brightness (color)
  (apply '+
         (mapcar* '* 
                  (color-values color)
                  '(0.299 0.587 0.114))))

(defun circe-generate-nick-color ()
  "Compute a suitable random nick color. Suitable means
1) Not a shade of gray
2) Not similar to foreground, background, or my-message colors
Similarity is computed with `circe-color-distance'
3) Perceived brightness difference between background and foreground is high enough"
  (let* ((min-distance 200)
         (min-brightness-difference 30000)
         (fg (face-foreground 'default))
         (bg (face-background 'default))
         (nick (face-foreground 'circe-my-message-face))
         (color (car (elt color-name-rgb-alist (random (length color-name-rgb-alist)))))
         (color-perceived-brightness (circe-perceived-brightness color))
         (bg-perceived-brightness (circe-perceived-brightness bg)))
    (if (and (not (color-gray-p color))
           (> (circe-color-distance color bg) min-distance)
           (> (circe-color-distance color fg) min-distance)
           (or (null nick) (> (circe-color-distance color nick) min-distance))
           (> (abs (- color-perceived-brightness bg-perceived-brightness)) min-brightness-difference))
        color
      (circe-generate-nick-color))))

(defvar circe-nick-color-mapping (make-hash-table :test 'equal)
  "Hash-map mapping nicks to color names.")

(defcustom circe-color-nicks-everywhere nil
  "Whether nicks should be colored in message bodies too."
  :group 'circe)

(defun circe-color-nicks ()
  "Color nicks on this lui output line."
  (when (eq major-mode 'circe-channel-mode)
    (let ((nickstart (text-property-any (point-min) (point-max)
                                        'lui-format-argument 'nick)))
      (when nickstart
        (goto-char nickstart)
        (let* ((nickend (next-property-change nickstart))
               (nick (buffer-substring-no-properties nickstart nickend)))
          (when (not (circe-server-my-nick-p nick))
            (let ((color (gethash nick circe-nick-color-mapping)))
              (when (not color)
                (setq color (circe-generate-nick-color))
                (puthash nick color circe-nick-color-mapping))
              (put-text-property nickstart nickend 'face `(:foreground ,color)))))))
    (when circe-color-nicks-everywhere
      (let ((body (text-property-any (point-min) (point-max)
                                     'lui-format-argument 'body))
            (nicks '())
            (regex nil))
        (when body
          (maphash (lambda (nick _)
                     (when (not (circe-server-my-nick-p nick))
                       (setq nicks (cons nick nicks))))
                   circe-nick-color-mapping)
          (setq regex (regexp-opt nicks 'words))
          (goto-char body)
          (while (re-search-forward regex nil t)
            (put-text-property (match-beginning 0)
                               (match-end 0)
                               'face `(:foreground
                                       ,(gethash (match-string-no-properties 0)
                                                 circe-nick-color-mapping)))))))))

(provide 'circe-color-nicks)
;;; circe-color-nicks.el ends here
