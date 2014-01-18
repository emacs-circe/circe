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


;; customization

(defgroup circe-color-nicks nil
  "Nicks colorization for Circe"
  :prefix "circe-color-nicks-"
  :group 'circe)

(defcustom circe-color-nicks-min-contrast-ratio 7
  "Minimum contrast ratio for generated colors; 
recommended is 7:1, or at least 4.5:1 (7 stands for 7:1 here).
Lower value allows higher color spread, but could lead to less readability."
  :group 'circe-color-nicks)

(defcustom circe-color-nicks-everywhere nil
  "Whether nicks should be colored in message bodies too."
  :group 'circe)


;; helper functions

(defsubst circe-color-values (color)
  "Like `color-values', but also handle \"unspecified-bg\" and
\"unspecified-fg\"."
  (let ((values (color-values color)))
    (cond
     (values values)
     ((equal color "unspecified-bg") '(0 0 0))
     ((equal color "unspecified-fg") '(255 255 255)))))

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

(defvar circe-color-nicks-rand-state 
  (random 8)
  "circe-color-nicks random state")

(defsubst circe-color-nicks-rand ()
  "Generates quasi-random value in range from 0 to 1
Uses 3 bits of state to make it less random"
  (let ((rand (/ (random 65536) 65535.0 7)))
    (setq circe-color-nicks-rand-state (1+ circe-color-nicks-rand-state))
    (+ rand (- (/ (logand circe-color-nicks-rand-state 7) 7.0)
               (/ 1 7.0)))))


;; implementation of http://www.w3.org/TR/2013/NOTE-WCAG20-TECHS-20130905/G18

(defsubst circe-w3-contrast-c-to-l (c)
  (if (<= c 0.03928)
      (/ c 12.92)
    (expt (/ (+ c 0.055) 1.055) 2.4)))

(defsubst circe-w3-contrast-relative-luminance (rgb)
  (apply '+
         (cl-mapcar (lambda (color coefficient)
                      (* coefficient
                         (circe-w3-contrast-c-to-l color)))
                    rgb
                    '(0.2126 0.7152 0.0722))))

(defsubst circe-w3-contrast-contrast-ratio (color1 color2)
  (let ((l1 (+ 0.05 (circe-w3-contrast-relative-luminance color1)))
        (l2 (+ 0.05 (circe-w3-contrast-relative-luminance color2))))
    (if (> l1 l2)
        (/ l1 l2)
        (/ l2 l1))))

;; generation of 7:1 ratio colors

(defsubst circe-w3-contrast-l-to-c (m)
  (if (<= m (/ 0.03928 12.92))
      (* m 12.92)
      (- (* (expt m (/ 1 2.4))
            1.055)
         0.055)))

(defsubst circe-w3-contrast-nn (n)
  (cond ((< n 0) 0)
        ((> n 1) 1)
        (t n)))

(defsubst circe-w3-contrast-color-with-luminance-higher-than (N)
  (let* ((Rc 0.2126)
         (Gc 0.7152)
         (Bc 0.0722)

         (R-min-lum (circe-w3-contrast-nn (/ (- N Gc Bc) Rc)))
         (R-min-color (circe-w3-contrast-l-to-c R-min-lum))
         (R-color (+ R-min-color (* (circe-color-nicks-rand) (- 1 R-min-color))))
         (R-lum (* Rc (circe-w3-contrast-c-to-l R-color)))

         (G-min-lum (circe-w3-contrast-nn (/ (- N R-lum Bc) Gc)))
         (G-min-color (circe-w3-contrast-l-to-c G-min-lum))
         (G-color (+ G-min-color (* (circe-color-nicks-rand) (- 1 G-min-color))))
         (G-lum (* Gc (circe-w3-contrast-c-to-l G-color)))
         
         (B-min-lum (circe-w3-contrast-nn (/ (- N R-lum G-lum) Bc)))
         (B-min-color (circe-w3-contrast-l-to-c B-min-lum))
         (B-color (+ B-min-color (* (circe-color-nicks-rand) (- 1 B-min-color))))
         (B-lum (* Bc (circe-w3-contrast-c-to-l B-color))))
    (list R-color G-color B-color)))

(defsubst circe-w3-contrast-color-with-luminance-lower-than (N)
  (let* ((Rc 0.2126)
         (Gc 0.7152)
         (Bc 0.0722)

         (R-max-lum (circe-w3-contrast-nn (/ N Rc)))
         (R-max-color (circe-w3-contrast-l-to-c R-max-lum))
         (R-color (* R-max-color (circe-color-nicks-rand)))
         (R-lum (* Rc (circe-w3-contrast-c-to-l R-color)))

         (G-max-lum (circe-w3-contrast-nn (/ (- N R-lum) Gc)))
         (G-max-color (circe-w3-contrast-l-to-c G-max-lum))
         (G-color (* G-max-color (circe-color-nicks-rand)))
         (G-lum (* Gc (circe-w3-contrast-c-to-l G-color)))

         (B-max-lum (circe-w3-contrast-nn (/ (- N R-lum G-lum) Bc)))
         (B-max-color (circe-w3-contrast-l-to-c B-max-lum))
         (B-color (* B-max-color (circe-color-nicks-rand)))
         (B-lum (* Bc (circe-w3-contrast-c-to-l B-color))))
    (list R-color G-color B-color)))

(defsubst circe-w3-contrast-generate-contrast-color (color ratio)
  (let ((color-lum (circe-w3-contrast-relative-luminance color)))
    (if (< color-lum (- (/ 1.0 ratio) 0.05))
        (circe-w3-contrast-color-with-luminance-higher-than (+ (* (+ color-lum 0.05) ratio) 0.05))
        (circe-w3-contrast-color-with-luminance-lower-than (- (/ (+ color-lum 0.05) ratio) 0.05)))))

(defun circe-w3-nick-color (nick)
  (let* ((bg-color-srgb (color-name-to-rgb (face-background 'default)))
         (fg-color-srgb (color-name-to-rgb (face-foreground 'default)))
         (color-srgb (circe-w3-contrast-generate-contrast-color
                      bg-color-srgb
                      circe-color-nicks-min-contrast-ratio))
         (color (circe-color-from-values color-srgb)))
    (if (> (color-cie-de2000 (apply 'color-srgb-to-lab fg-color-srgb)
                             (apply 'color-srgb-to-lab color-srgb))
           10)
        (progn (puthash nick color circe-nick-color-mapping)
               color)
      (circe-w3-nick-color nick))))


(defvar circe-nick-color-mapping (make-hash-table :test 'equal)
  "Hash-map mapping nicks to color names.")

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
                (setq color (circe-w3-nick-color nick)))
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
