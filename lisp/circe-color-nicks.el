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

(defgroup circe-color-nicks nil
  "Nicks colorization for Circe"
  :prefix "circe-color-nicks-"
  :group 'circe)

(defsubst circe-color-values (color)
  "Like `color-values', but also handle \"unspecified-bg\" and
\"unspecified-fg\"."
  (let ((values (color-values color)))
    (cond
     (values values)
     ((equal color "unspecified-bg") '(0 0 0))
     ((equal color "unspecified-fg") '(255 255 255)))))

(defsubst circe-xyz-helper (values matrix)
  (mapcar (lambda (row)
            (cl-reduce '+ (cl-mapcar '* values row)))
          matrix))

(defsubst circe-rgb-to-xyz (rgb)
  (let ((cie-rgb '((0.412453 0.357580 0.180423)
                   (0.212671 0.715160 0.072169)
                   (0.019334 0.119193 0.950227))))
    (circe-xyz-helper rgb cie-rgb)))

(defsubst circe-xyz-to-rgb (xyz)
  (let ((cie-rgb '(( 3.240479 -1.537150 -0.498535)
                   (-0.969256  1.875992  0.041556)
                   ( 0.055648 -0.204043  1.057311))))
    (circe-xyz-helper xyz cie-rgb)))

(defsubst circe-color-to-xyz (color)
  (circe-rgb-to-xyz (mapcar (lambda (x) (/ x 65535.0))
                            (circe-color-values color))))

(defsubst circe-xyz-to-color (xyz)
  (circe-color-from-values (circe-xyz-to-rgb xyz)))

(defsubst circe-color-from-values (values)
  (apply 'concat 
         (cons "#" 
               (mapcar (lambda (val) 
                         (format "%02x" 
                                 (* (cond ((< val 0) 0)
                                          ((> val 1) 1)
                                          (t val))
                                    255)))
                       values))))

(random t)
(setq circe-color-nicks-rand-state (random 8))

(defsubst circe-color-nicks-rand ()
  "Generates quasi-random value in range from 0 to 1
Uses 3 bits of state to make it less random"
  (let ((rand (/ (random 65535) 65535.0 7)))
    (setq circe-color-nicks-rand-state (1+ circe-color-nicks-rand-state))
    (+ rand (- (/ (logand circe-color-nicks-rand-state 7) 7.0)
               (/ 1 7.0)))))


(defsubst circe-color-nicks-rand-exclude (val distance rand)
  "Excludes distance around val"
  (let* ((upper-distance (if (< (+ val distance) 1)
                             distance
                           (- 1 val)))
         (lower-distance (if (> (- val distance) 0)
                             distance
                           val))
         (total-distance (+ lower-distance upper-distance))
         (less-rand (* rand (- 1 total-distance))))
    (if (> less-rand (- val lower-distance))
        (+ total-distance less-rand)
      less-rand)))

(defsubst circe-color-nicks-rand-scale (val distance rand)
  "Scales to a distance around val"
  (let* ((upper-distance (if (< (+ val distance) 1)
                             distance
                           (- 1 val)))
         (lower-distance (if (> (- val distance) 0)
                             distance
                           val))
         (total-distance (+ lower-distance upper-distance)))
    (+ (- val lower-distance)
       (* total-distance
          rand))))

(defcustom circe-color-nicks-min-color-distance 0.05
  "Minimal color distance between foreground and nick colors, 0 to 1."
  :group 'circe-color-nicks)

(defcustom circe-color-nicks-max-color-distance 0.1
  "Maximum color distance between foreground and nick colors, 0 to 1.
Only for cie-1931-xyz function."
  :group 'circe-color-nicks)

(defcustom circe-color-nicks-min-brightness-difference 0.45
  "Minimal brightness difference between background and nick colors, 0 to 1."
  :group 'circe-color-nicks)

(defcustom circe-color-nicks-persistent-colors nil
  "Make colors persistent for nicks.
Only for cie-1931-xyz function."
  :group 'circe-color-nicks)

(defcustom circe-color-nicks-colors '(green)
  "List of colors to use for nicks
Only for pick-nick-color function."
  :group 'circe-color-nicks
  :type '(repeat color))

(defcustom circe-color-nicks-get-nick-color-function 'circe-generate-nick-color
  "A function to colorize nicks
This function will be called with one argument, nick, 
and could store a color in circe-nick-color-mapping hash,
so it will be used next time instead of calling the function.

Built-in functions are:
circe-generate-nick-color - pick a color from color-name-rgb-alist, using weighted Euclidian distance wrt to settings
circe-get-nick-color-cie-1931-xyz - generate a color, using CIE 1931 XYZ wrt to settings
circe-pick-nick-color - just get a color from circe-color-nicks-colors"
  :group 'circe-color-nicks
  :type 'symbol)


(defvar circe-color-nicks-colors-current nil)

(defun circe-pick-nick-color (nickname)
  "Just get a color from a list"
  (if (not circe-color-nicks-colors-current)
      (setq circe-color-nicks-colors-current circe-color-nicks-colors))
  (let ((color (car circe-color-nicks-colors-current)))
    (setq circe-color-nicks-colors-current (cdr circe-color-nicks-colors-current))
    (puthash nickname color circe-nick-color-mapping)
    color))

(defun circe-get-nick-color-cie-1931-xyz (nickname)
  "Compute a suitable random nick color. Suitable means
1) Luminance differs enough from background's
2) Neither too close to nor too far from foreground
by mix of cone response curves and blue stimulation"
  (if circe-color-nicks-persistent-colors
      (setq circe-color-nicks-rand-state (mod (cl-reduce '+ (string-to-list nickname)) 8)))
  (let* ((bg (face-background 'default))
         (fg (face-foreground 'default))
         (bg-xyz (circe-color-to-xyz bg))
         (fg-xyz (circe-color-to-xyz fg))
         (bg-luminance (cadr bg-xyz))
         (fg-mix-of-cone-response-curves (car fg-xyz))
         (fg-blue-stimulation (caddr fg-xyz))
         (color (circe-xyz-to-color 
                 (list 
                  ;; at first we are getting a random value:
                  ;; 0 <------------------------------------------------> 1
                  ;; then scaling it (f - foreground, d - distance):
                  ;; 0 -------------------------------f------------------ 1
                  ;; 0 --------------------------<-d->-<-d->------------- 1
                  ;; 0 --------------------------<--------->------------- 1
                  ;; then excluding some distance around f (e - excluded):
                  ;; 0 ---------------------<-d-><-e->f<-e-><-d->-------- 1
                  ;; and getting something like this in the end:
                  ;; 0 ---------------------<--->-----------<--->-------- 1
                  (circe-color-nicks-rand-exclude fg-mix-of-cone-response-curves
                                                  circe-color-nicks-min-color-distance
                                                  (circe-color-nicks-rand-scale fg-mix-of-cone-response-curves
                                                                                circe-color-nicks-max-color-distance
                                                                                (circe-color-nicks-rand)))
                  (circe-color-nicks-rand-exclude bg-luminance
                                                  circe-color-nicks-min-brightness-difference
                                                  (circe-color-nicks-rand))
                  (circe-color-nicks-rand-exclude fg-blue-stimulation 
                                                  circe-color-nicks-min-color-distance
                                                  (circe-color-nicks-rand-scale fg-blue-stimulation
                                                                                circe-color-nicks-max-color-distance
                                                                                (circe-color-nicks-rand)))))))
    (progn
      (if (not circe-color-nicks-persistent-colors)
          (puthash nickname color circe-nick-color-mapping))
      color)))


(defsubst circe-color-distance (color1 color2)
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
             (ash (* (- 767 red-mean) db db) -8)))))


(defsubst circe-perceived-brightness (color)
  (apply '+
         (cl-mapcar '* 
                    (color-values color)
                    '(0.299 0.587 0.114))))

(defun circe-generate-nick-color (nickname)
  "Compute a suitable random nick color. Suitable means
1) Not a shade of gray
2) Not similar to foreground or my-message colors
Similarity is computed with `circe-color-distance'
3) Perceived brightness difference between background and foreground is high enough"
  (let* ((fg (face-foreground 'default))
         (bg (face-background 'default))
         (nick (face-foreground 'circe-my-message-face))
         (color (car (elt color-name-rgb-alist (random (length color-name-rgb-alist)))))
         (color-perceived-brightness (circe-perceived-brightness color))
         (bg-perceived-brightness (circe-perceived-brightness bg)))
    (if (and (not (color-gray-p color))
             (> (circe-color-distance color fg) (* 765 circe-color-nicks-min-color-distance))
             (or (null nick) (> (circe-color-distance color nick) (* 765 circe-color-nicks-min-color-distance)))
             (> (abs (- color-perceived-brightness 
                        bg-perceived-brightness))
                (* circe-color-nicks-min-brightness-difference
                   65535)))
        (progn 
          (puthash nickname color circe-nick-color-mapping)
          color)
      (circe-generate-nick-color nickname))))


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
                (setq color (funcall circe-color-nicks-get-nick-color-function nick)))
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
