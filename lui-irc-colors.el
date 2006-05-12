;;; lui-irc-colors.el --- Add IRC color support to LUI

;; Copyright (C) 2005  Jorgen Schaefer

;; Version: 1.0
;; Keywords: Lui, CIRCE, IRC
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

;; This tells LUI how to display IRC colors:
;; ^B - Bold
;; ^_ - Underline
;; ^V - Inverse
;; ^O - Return to normal
;; ^C1,2 - Colors

;; The colors are documented at http://www.mirc.co.uk/help/color.txt

;;; Code:

(require 'lui)
(require 'font-lock) ; for font-lock-prepend-text-property

(defgroup lui-irc-colors nil
  "LUI IRC colors faces."
  :group 'circe)

(defface lui-irc-colors-inverse-face
  '((t (:inverse-video t)))
  "Face used for inverse test."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-0-face
  '((((class color) (min-colors 88)) (:foreground "white"))
    (((class color) (min-colors 8)) (:foreground "white" :weight bold))
    (t (:foreground "white")))
  "Face used for foreground IRC color 0 (white)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-1-face
  '((t (:foreground "black")))
  "Face used for foreground IRC color 1 (black)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-2-face
  '((((class color) (min-colors 88)) (:foreground "blue4"))
    (((class color) (min-colors 8)) (:foreground "blue"))
    (t (:foreground "blue")))  "Face used for foreground IRC color 2 (blue)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-3-face
  '((t (:foreground "green4")))
  "Face used for foreground IRC color 3 (green)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-4-face
  '((((class color) (min-colors 88)) (:foreground "red"))
    (((class color) (min-colors 8)) (:foreground "red" :weight bold))
    (t (:foreground "red")))
  "Face used for foreground IRC color 4 (red)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-5-face
  '((((class color) (min-colors 88)) (:foreground "red4"))
    (((class color) (min-colors 8)) (:foreground "red"))
    (t (:foreground "red")))
  "Face used for foreground IRC color 5 (brown)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-6-face
  '((((class color) (min-colors 88)) (:foreground "magenta4"))
    (((class color) (min-colors 8)) (:foreground "magenta"))
    (t (:foreground "magenta")))
  "Face used for foreground IRC color 6 (purple)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-7-face
  '((((class color) (min-colors 88)) (:foreground "yellow4"))
    (((class color) (min-colors 8)) (:foreground "yellow"))
    (t (:foreground "yellow")))
  "Face used for foreground IRC color 7 (orange)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-8-face
  '((((class color) (min-colors 88)) (:foreground "yellow"))
    (((class color) (min-colors 8)) (:foreground "yellow" :weight bold))
    (t (:foreground "yellow")))
  "Face used for foreground IRC color 8 (yellow)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-9-face
  '((((class color) (min-colors 88)) (:foreground "green"))
    (((class color) (min-colors 8)) (:foreground "green" :weight bold))
    (t (:foreground "green")))
  "Face used for foreground IRC color 9 (light green)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-10-face
  '((((class color) (min-colors 88)) (:foreground "cyan4"))
    (((class color) (min-colors 8)) (:foreground "cyan"))
    (t (:foreground "cyan")))
  "Face used for foreground IRC color 10 (teal)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-11-face
  '((((class color) (min-colors 88)) (:foreground "cyan"))
    (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
    (t (:foreground "cyan")))
  "Face used for foreground IRC color 11 (light cyan)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-12-face
  '((((class color) (min-colors 88)) (:foreground "blue"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:foreground "blue")))
  "Face used for foreground IRC color 12 (light blue)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-13-face
  '((((class color) (min-colors 88)) (:foreground "magenta"))
    (((class color) (min-colors 8)) (:foreground "magenta" :weight bold))
    (t (:foreground "magenta")))
  "Face used for foreground IRC color 13 (pink)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-14-face
  '((((class color) (min-colors 88)) (:foreground "dimgray"))
    (((class color) (min-colors 8)) (:foreground "white"))
    (t (:foreground "gray")))
  "Face used for foreground IRC color 14 (grey)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-fg-15-face
  '((((class color) (min-colors 88)) (:foreground "gray"))
    (((class color) (min-colors 8)) (:foreground "white"))
    (t (:foreground "gray")))
  "Face used for foreground IRC color 15 (light grey)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-0-face
  '((((class color) (min-colors 88)) (:background "white"))
    (((class color) (min-colors 8)) (:background "white" :weight bold))
    (t (:background "white")))
  "Face used for background IRC color 0 (white)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-1-face
  '((t (:background "black")))
  "Face used for background IRC color 1 (black)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-2-face
  '((((class color) (min-colors 88)) (:background "blue4"))
    (((class color) (min-colors 8)) (:background "blue"))
    (t (:background "blue")))  "Face used for background IRC color 2 (blue)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-3-face
  '((t (:background "green4")))
  "Face used for background IRC color 3 (green)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-4-face
  '((t (:background "red")))
  "Face used for background IRC color 4 (red)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-5-face
  '((((class color) (min-colors 88)) (:background "red4"))
    (((class color) (min-colors 8)) (:background "red"))
    (t (:background "red")))
  "Face used for background IRC color 5 (brown)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-6-face
  '((((class color) (min-colors 88)) (:background "magenta4"))
    (((class color) (min-colors 8)) (:background "magenta"))
    (t (:background "magenta")))
  "Face used for background IRC color 6 (purple)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-7-face
  '((((class color) (min-colors 88)) (:background "yellow4"))
    (((class color) (min-colors 8)) (:background "yellow"))
    (t (:background "yellow")))
  "Face used for background IRC color 7 (orange)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-8-face
  '((t (:background "yellow")))
  "Face used for background IRC color 8 (yellow)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-9-face
  '((t (:background "green")))
  "Face used for background IRC color 9 (light green)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-10-face
  '((((class color) (min-colors 88)) (:background "cyan4"))
    (((class color) (min-colors 8)) (:background "cyan"))
    (t (:background "cyan")))
  "Face used for background IRC color 10 (teal)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-11-face
  '((t (:background "cyan")))
  "Face used for background IRC color 11 (light cyan)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-12-face
  '((t (:background "blue")))
  "Face used for background IRC color 12 (light blue)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-13-face
  '((t (:background "magenta")))
  "Face used for background IRC color 13 (pink)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-14-face
  '((((class color) (min-colors 88)) (:background "dimgray"))
    (((class color) (min-colors 8)) (:background "white"))
    (t (:background "gray")))
  "Face used for background IRC color 14 (grey)."
  :group 'lui-irc-colors)

(defface lui-irc-colors-bg-15-face
  '((((class color) (min-colors 88)) (:background "gray"))
    (((class color) (min-colors 8)) (:background "white"))
    (t (:background "gray")))
  "Face used for background IRC color 15 (light grey)."
  :group 'lui-irc-colors)

(defvar lui-irc-colors-regex
  "\\(\\|\\|\\|\\|\\)"
  "A regular expression matching IRC control codes.")

;;;###autoload
(defun enable-lui-irc-colors ()
  "Enable IRC color interpretation for Lui."
  (interactive)
  (add-hook 'lui-pre-output-hook 'lui-irc-colors))

(defun disable-lui-irc-colors ()
  "Disable IRC color interpretation for Lui."
  (interactive)
  (remove-hook 'lui-pre-output-hook 'lui-irc-colors))

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
                (setq fg (if (and fg (not (= fg 99))) (mod fg 16) nil)          
                      bg (if (and bg (not (= bg 99))) (mod bg 16) nil))
                (replace-match ""))
            (setq fg nil
                  bg nil)))
         (t
          (error "lui-irc-colors: Can't happen!")))))
    (lui-irc-propertize (point) (point-max)
                        boldp inversep underlinep fg bg)))

(defun lui-irc-propertize (start end boldp inversep underlinep fg bg)
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
                     (list (lui-irc-colors-face 'fg fg))
                   nil)
                 (if bg
                     (list (lui-irc-colors-face 'bg bg))
                   nil))))

(defun lui-irc-colors-face (type n)
  "Return a face appropriate for face number N.
TYPE is either 'fg or 'bg."
  (if (and (<= 0 n)
           (<= n 15))
      (intern (format "lui-irc-colors-%s-%s-face" type n))
    'default-face))

(provide 'lui-irc-colors)
;;; lui-irc-colors.el ends here
