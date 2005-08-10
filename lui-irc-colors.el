;;; lui-irc-colors.el --- Add IRC color support to LUI

;; Copyright (C) 2005  Jorgen Schaefer

;; Version: 1.0
;; Keywords: Lui, EIRCC, IRC
;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; URL: http://www.nongnu.org/eircc/

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

;; This tells LUI how to display IRC colors:
;; ^B - Bold
;; ^_ - Underline
;; ^V - Inverse
;; ^O - Return to normal
;; ^C1,2 - Colors

;; Colors don't work yet. If you care and find out why, send me a
;; patch.

;;; Code:

(defgroup lui-irc-colors nil
  "LUI IRC colors faces."
  :group 'eircc)

(defface lui-irc-colors-inverse-face
  '((t (:inverse-video t)))
  "Face used for inverse test."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-0-face
  '((t (:foreground "White")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-1-face
  '((t (:foreground "black")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-2-face
  '((t (:foreground "blue")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-3-face
  '((t (:foreground "green")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-4-face
  '((t (:foreground "red")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-5-face
  '((t (:foreground "brown")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-6-face
  '((t (:foreground "purple")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-7-face
  '((t (:foreground "orange")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-8-face
  '((t (:foreground "yellow")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-9-face
  '((t (:foreground "lightgreen")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-10-face
  '((t (:foreground "teal")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-11-face
  '((t (:foreground "lightcyan")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-12-face
  '((t (:foreground "lightblue")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-13-face
  '((t (:foreground "pink")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-14-face
  '((t (:foreground "grey")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-15-face
  '((t (:foreground "lightsilver")))
  "Face used for foreground IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-0-face
  '((t (:background "White")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-1-face
  '((t (:background "black")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-2-face
  '((t (:background "blue")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-3-face
  '((t (:background "green")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-4-face
  '((t (:background "red")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-5-face
  '((t (:background "brown")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-6-face
  '((t (:background "purple")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-7-face
  '((t (:background "orange")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-8-face
  '((t (:background "yellow")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-9-face
  '((t (:background "lightgreen")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-10-face
  '((t (:background "teal")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-11-face
  '((t (:background "lightcyan")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-12-face
  '((t (:background "lightblue")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-13-face
  '((t (:background "pink")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-14-face
  '((t (:background "grey")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-15-face
  '((t (:background "lightgrey")))
  "Face used for background IRC color 0"
  :group 'lui-irc-colors)

(defvar lui-irc-colors-regex
  "\\(\\|\\|\\|\\|\\)"
  "A regular expression matching IRC control codes.")

(defun lui-irc-colors ()
  "Add color faces for IRC colors.
This is an appropriate function for `lui-pre-output-hook'."
  (goto-char (point-min))
  (let ((start (point))
        (boldp nil)
        (inversep nil)
        (underlinep nil)
        (fg nil)
        (bg nil))
    (while (re-search-forward lui-irc-colors-regex nil t)
      (lui-irc-propertize start (point)
                          boldp inversep underlinep
                          fg bg)
      (let ((code (match-string 1)))
        (replace-match "")
        (setq start (point))
        (cond
         ((string= code "")
          (setq boldp (not boldp)))
         ((string= code "")
          (setq inversep (not inversep)))
         ((string= code "")
          (setq underlinep (not underlinep)))
         ((string= code "")
          (setq boldp nil
                inversep nil
                underlinep nil
                fg nil
                bg nil))
         ((string= code "")
          (if (looking-at "\\([0-9][0-9]?\\)\\(,\\([0-9][0-9]?\\)\\)?")
              (progn
                (setq fg (string-to-number (match-string 1))
                      bg (if (match-string 2)
                             (string-to-number (match-string 3))
                           bg))
                (replace-match ""))
            (setq fg nil
                  bg nil)))
         (t
          (error "lui-irc-colors: Can't happen!")))))
    (lui-irc-propertize (point) (point-max)
                        boldp inversep underlinep fg bg)))

(defun lui-irc-propertize (start end bolp inversep underlinep fg bg)
  "Propertize the region between START and END."
  (font-lock-prepend-text-property
   start end
   'face (append (if boldp
                     '(bold)
                   nil)
                 (if inversep
                     '(lui-irc-colors-inverse-face)
                   nil)
                 (if underlinep
                     '(underline)
                   nil)
                 (if fg
                     (list (lui-irc-color-face 'fg fg))
                   nil)
                 (if bg
                     (list (lui-irc-color-face 'bg bg))
                   nil))))

(defun lui-irc-color-face (type n)
  "Return a face appropriate for face number N.
TYPE is either 'fg or 'bg."
  (if (and (<= 0 n)
           (<= n 15))
      (intern (format "lui-irc-color-%s-%s-face" type n))
    'default-face))

(provide 'lui-irc-colors)
;;; lui-irc-colors.el ends here
