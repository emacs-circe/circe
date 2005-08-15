;;; lui-format.el --- A formatting function for use with Lui

;; Copyright (C) 2005  Jorgen Schaefer

;; Version: 1.0
;; Keywords: Lui, formatting
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

;; An improved kind of formatting function for applications using Lui.
;; This might eventually be included in lui.el
;; See the docstring of `lui-format' for more details.

;;; Code:

(require 'lui)

(defun lui-display (format not-tracked-p &rest keywords)
  "Insert FORMAT formatted with KEYWORDS in the current Lui buffer.
See `lui-format' for a description of the arguments.
When NOT-TRACKED-P is non-nil, this is not considered to be tracked."
  (lui-insert (lui-format format keywords)
              not-tracked-p))

(defun lui-format (format &rest keywords)
  "Display FORMAT formatted with KEYWORDS.
FORMAT should be a symbol whose value is taken. If the value is a
procedure, the keyword list is passed as a single argument to it,
and it should return the formatted string. If the value is a
string, it is formatted according to the rules below.

KEYWORDS is a plist of keywords and strings, or symbols and
strings. They are used as format arguments.

The string is taken verbatim, unless there is a backslash or a
opening brace. A backslash before a character means it is taken
literally. This is only necessary (but then also required) in
front of braces or backslashes.

A word inside braces is looked up in the keywords plist. If the
format is either a number, a number followed by a dash, or two
numbers with a dash in between them, this is taken as a special
format that is looked up in the list given using the list
argument to the :indexed-args keyword.

{1} refers to the second element (element 1)
{1-} refers to the second and all following elements
{1-3} refers to the second through fourth element

If more than one element is selected, the elements are separated
by a single space character.

All named arguments receive a property of `lui-format-argument'
with the respective name as value. The whole string receives a
`lui-format-type' property with the FORMAT symbol as a value."
  (let ((str (symbol-value format))
        (plist (mapcar (lambda (entry)
                         (if (keywordp entry)
                             ;; Keyword -> symbol
                             (intern (substring (symbol-name entry)
                                                1))
                           entry))
                       (if (and (not (null keywords))
                                (null (cdr keywords)))
                           (car keywords)
                         keywords))))
    (propertize (if (functionp str)
                    (funcall str plist)
                  (lui-format-internal str plist))
                'lui-format-type format)))

(defun lui-format-internal (fmt keywords)
  "Format a prepared and normalized entry.
See `lui-format'."
  (with-temp-buffer
    (insert fmt)
    (goto-char (point-min))
    (while (re-search-forward "\\(\\\\\\(.\\)\\|{\\([^}]+\\)}\\)" nil t)
      (let ((quoted (match-string 2))
            (tag (match-string 3)))
        (if quoted
            (replace-match quoted)
          (replace-match (save-match-data
                           (lui-format-single tag keywords))
                         t t)
          (add-text-properties (match-beginning 0)
                               (point) ; Left there by `replace-match'
                               `(lui-format-argument ,(intern tag))))))
    (buffer-string)))

(defun lui-format-single (entry keywords)
  "Format a single braced entry according to KEYWORDS.
See `lui-format' for details."
  (cond
   ((string-match "^\\([0-9]+\\)\\(-\\([0-9]+\\)?\\)?$" entry)
    (let ((from (match-string 1 entry))
          (rangep (match-string 2 entry))
          (to (match-string 3 entry))
          (indexed-args (plist-get keywords 'indexed-args)))
      (if rangep
          (mapconcat #'identity
                     (lui-sublist indexed-args
                                  (string-to-number from)
                                  (when to (string-to-number to)))
                     " ")
        (or (nth (string-to-number from)
                 indexed-args)
            (error "Index for format out of range: %S" from)))))
   (t
    (or (plist-get keywords (intern entry))
        (error "Unknown keyword argument %S" entry)))))

(defun lui-sublist (list from &optional to)
  "Return the sublist from LIST starting at FROM and ending at TO."
  (if (not to)
      (nthcdr from list)
    (let ((from-list (nthcdr from list))
          (i (- to from))
          (to-list nil))
      (while (>= i 0)
        (when (null from-list)
          (error "Argument out of range: %S" to))
        (setq to-list (cons (car from-list)
                            to-list)
              i (- i 1)
              from-list (cdr from-list)))
      (nreverse to-list))))

(provide 'lui-format)
;;; lui-format.el ends here
