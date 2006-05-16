;;; lui-links.el --- Automatic links in a LUI buffer

;; Copyright (C) 2006  Jorgen Schaefer

;; Version: 1.0
;; Keywords: LUI
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

;; This file provides a LUI module for automatic annotation of links
;; in a LUI buffer so you can just hit RET on one of them, and be
;; taken to the link target.

;;; Code:

(defcustom lui-links-list
  '(("\\(http\\|ftp\\|irc\\)://[^ \n]*[a-zA-Z0-9/]" 0 browse-url)
    ("`\\([A-Za-z0-9+=*/-]+\\)'" 1 lui-links-elisp-symbol)
    ("RFC ?\\([0-9]+\\)" 0 lui-links-rfc)
    ("SRFI[- ]?\\([0-9]+\\)" 0 lui-links-srfi))
  "A list of links to recognize.
Each entry consists of a regular expression, a submatch to be
highlighted, and a function be called with that submatch as an
argument."
  :group 'lui
  :type '(repeat (list regex integer function)))

(defface lui-links-face
  '((((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:underline t)))
  "The face of links in LUI."
  :group 'lui)

(defvar lui-links-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'lui-links-follow)
    map)
  "The keymap used on links.")

(defun enable-lui-links ()
  "Enable automatic links in LUI buffers."
  (interactive)
  (mapc (lambda (entry)
          (add-to-list 'lui-highlight-keywords
                       (list (nth 0 entry)
                             (nth 1 entry)
                             (list 'face 'lui-links-face
                                   'keymap lui-links-keymap
                                   'lui-links-function (nth 2 entry)))))
        lui-links-list))

(defun disable-lui-links ()
  "Disable automatic links in LUI buffers."
  (interactive)
  (mapc (lambda (entry)
          (setq lui-highlight-keywords
                (delete (list (nth 0 entry)
                              (nth 1 entry)
                              (list 'face 'lui-links-face
                                    'keymap lui-links-keymap
                                    'lui-links-function (nth 2 entry)))
                        lui-highlight-keywords)))
      lui-links-list))

(defun lui-links-follow ()
  "Call the appropriate function for the link at point.
The link is propertized with a lui-links-function property whose
value is the function to call."
  (interactive)
  (let ((fun (get-text-property (point)
                                'lui-links-function)))
    (when (not fun)
      (error "No link at point"))
    (let ((beg (save-excursion
                 (previous-single-property-change (point)
                                                  'lui-links-function)))
          (end (save-excursion
                 (next-single-property-change (point)
                                              'lui-links-function))))
      (if (and beg end)
          (funcall fun (buffer-substring-no-properties beg end))
        (error "Invalid link at point")))))

(defun lui-links-elisp-symbol (str)
  "Show the documentation of the symbol named STR."
  (let ((sym (intern-soft str)))
    (cond
     ((not sym)
      (error "No such symbol %s" str))
     ((functionp sym)
      (describe-function sym))
     (t
      (describe-variable sym)))))

(defun lui-links-rfc (str)
  "Show the RFC mentioned in STR."
  (if (string-match "RFC ?\\([0-9]+\\)" str)
      (browse-url (format "http://www.ietf.org/rfc/rfc%s.txt"
                          (match-string 1 str)))
    (error "Bad RFC syntax")))

(defun lui-links-srfi (str)
  "Show the SRFI mentioned in STR."
  (if (string-match "SRFI[ -]?\\([0-9]+\\)" str)
      (let ((num (match-string 1 str)))
        (browse-url (format "http://srfi.schemers.org/srfi-%s/srfi-%s.html"
                            num num)))
    (error "Bad SRFI syntax")))

(provide 'lui-links)
;;; lui-links.el ends here
