;;; lui.el --- Linewise User Interface

;; Copyright (C) 2005  Jorgen Schaefer

;; Version: 2
;; Keywords: User-interface
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

;; Lui-mode is a major mode that provides a user interface for
;; applications. The user interface is quite trivial - consisting of
;; an input line, a prompt, and some output area - but Lui includes a
;; lot of options, like time stamps, filling, etc.

;; Application programs should create modes derived from lui-mode.

;; The application API consists of:

;; lui-mode
;; lui-set-prompt
;; lui-insert
;; lui-input-function
;; lui-completion-function
;; And the 'lui-ignored text property.

;;; Code:

(defvar lui-version "2"
  "Lui version string.")

(require 'incomplete)
(require 'ring)
(require 'flyspell)
(require 'ispell)

(when (featurep 'xemacs)
  (require 'lui-xemacs))


;;;;;;;;;;;;;;;;;;;;;
;;; Customization ;;;
;;;;;;;;;;;;;;;;;;;;;

(defgroup lui nil
  "The Linewise User Interface."
  :prefix "lui-"
  :group 'applications)

(defcustom lui-track-shorten-buffer-names-p t
  "*Whether to shorten buffer names in the mode line.
A non-nil value will cause Lui to shorten the buffer names as
much as possible to stay unambiguous when displaying them in the
mode line."
  :type 'boolean
  :group 'lui)

(defcustom lui-scroll-to-bottom-p t
  "*Non-nil if Lui should keep the input line at the end of the window."
  :type 'boolean
  :group 'lui)

(defcustom lui-flyspell-p nil
  "*Non-nil if Lui should spell-check your input.
See `flyspell-mode' for more information."
  :type 'boolean
  :group 'lui)

(defcustom lui-flyspell-alist nil
  "*Alist of buffer dictionaries.
This is a list of mappings from buffers to dictionaries to use
for `flyspell-mode'. The appropriate dictionary is automatically
used when Lui is activated in a buffer with a matching buffer
name.

The entries are of the form (REGEXP DICTIONARY), where REGEXP
must match a buffer name, and DICTIONARY specifies an existing
dictionary for `flyspell-mode'. See
`ispell-local-dictionary-alist' and `ispell-dictionary-alist' for
a valid list of dictionaries."
  :type 'string
  :group 'lui)

(defcustom lui-highlight-keywords nil
  "*A list of keywords to highlight.
This specifies a list of keywords that Lui should highlight. Each
entry is of one of the following forms (similar to
`font-lock-keywords'):

  REGEXP
    Highlight every match in `lui-highlight-face'
  (REGEXP SUBMATCH)
    Highlight the SUBMATCH (a number) in REGEXP in
    `lui-highlight-face'
  (REGEXP FACE)
    Highlight everything matching REGEXP in FACE (a symbol)
  (REGEXP SUBMATCH FACE)
    Highlight the SUBMATCH in REGEXP in FACE"
  :type '(repeat (choice
                  (string :tag "Regular Expression")
                  (list :tag "Submatch"
                        (string :tag "Regular Expression")
                        (integer :tag "Submatch"))
                  (list :tag "Regular Expression in Specific Face"
                        (string :tag "Regular Expression")
                        (face :tag "Face"))
                  (list :tag "Submatch in Specific Face"
                        (string :tag "Regular Expression")
                        (integer :tag "Submatch")
                        (face :tag "Face"))))
  :group 'lui)

(defcustom lui-fill-type "    "
  "*How Lui should fill its output.
This can be one of the following values:

  A string
      This is used as the fill prefix
  'variable
      The first sequence of non-whitespace characters in the
      output is used as an alignment, and the rest is filled with
      spaces.
  A number
      The first sequence of non-whitespace characters is
      right-aligned at this column, and the rest is filled to
      this column.
  nil
      Turn filling off."
  :type '(choice (string :tag "Fill Prefix")
                 (const :tag "Variable Fill Prefix" variable)
                 (integer :tag "Fill Column")
                 (const :tag "No filling" nil))
  :group 'lui)

(defcustom lui-fill-column 70
  "*The column at which Lui should break output.
See `fill-column'."
  :type 'integer
  :group 'lui)

(defcustom lui-fill-remove-face-from-newline t
  "*Non-nil when filling should remove faces from newlines.
Faces on a newline extend to the end of the displayed line, which
is often not was is wanted."
  :type 'boolean
  :group 'lui)

(defcustom lui-time-stamp-format "[%H:%M]"
  "*The format of time stamps.
See `format-time-string' for a full description of available
formatting directives."
  :type 'string
  :group 'lui)

(defcustom lui-time-stamp-position 'right
  "*Where Lui should put time-stamps.
This can be one of the following values:

  A number
      At this column of the first line of output
  'right
      At a column just right to `lui-fill-column'
  'left
      At the left side of the output. The output is thereby moved
      to the right.
  nil
      Do not add any time stamp."
  :type '(choice (const :tag "Right" right)
                 (integer :tag "Column")
                 (const :tag "Left" left)
                 (const :tag "None" nil))
  :group 'lui)

(defcustom lui-time-stamp-only-when-changed-p t
  "*Non-nil if Lui should only add a time-stamp when the time changes.
If `lui-time-stamp-position' is 'left, this will still add the
necessary whitespace."
  :type 'boolean
  :group 'lui)

(defcustom lui-read-only-output-p t
  "*Non-nil if Lui should make the output read-only.
Switching this off makes copying (by killing) easier for some."
  :type 'boolean
  :group 'lui)

(defcustom lui-max-buffer-size nil
  "*Non-nil if Lui should truncate the buffer if it grows too much.
If the buffer size (in characters) exceeds this number, it is
truncated at the top."
  :type '(choice (const :tag "Never Truncate" nil)
                 (integer :tag "Maximum Buffer Size"))
  :group 'lui)

(defcustom lui-input-ring-size 32
  "*The size of the input history of Lui.
This is the size of the input history used by
\\[lui-previous-input] and \\[lui-next-input]."
  :type 'integer
  :group 'lui)

(defcustom lui-track-all-frames-p t
  "*Non-nil when Lui should look at all frames for tracking.
If this is non-nil, Lui will look whether the appropriate buffer
is visible in any frame, not just the current one."
  :type 'boolean
  :group 'lui)

(defcustom lui-track-position 'before-modes
  "*Where tracked buffers should appear in the mode line.

  'before-modes
      Before the mode indicators
  'after-modes
      After the mode indicators
  'end
      At the end of the mode line"
  :type '(choice (const :tag "Before the Mode Indicators" before-modes)
                 (const :tag "Afterthe Mode Indicators" after-modes)
                 (const :tag "At the End of the Mode Line" end))
  :group 'lui)

(defcustom lui-track-faces-priorities nil
  "*A list of faces which should be shown by LUI tracking in the mode line.
The first face found in this list is used."
  :type '(repeat face)
  :group 'lui)

(defcustom lui-track-ignored-buffers nil
  "*A list of buffers that are never tracked.
Each element of this list has one of the following forms:

  regexp - Any buffer matching won't be tracked.
  (regexp faces ...) - Any buffer matching won't be tracked,
      unless it has a face in FACES ... associated with it.
      If no faces are given, `lui-track-faces-priorities' is
      used."
  :type '(repeat (choice regexp
                         (list regexp
                               (repeat face))))
  :group 'lui)

(defcustom lui-mode-hook nil
  "*The hook run when Lui is started."
  :type 'hook
  :group 'lui)

(defcustom lui-pre-input-hook nil
  "*A hook run before Lui interprets the user input.
It is called with the buffer narrowed to the input line.
Functions can modify the input if they really want to, but the
user won't see the modifications, so that's a bad idea."
  :type 'hook
  :group 'lui)

(defcustom lui-pre-output-hook nil
  "*The hook run before output is formatted."
  :type 'hook
  :group 'lui)

(defcustom lui-post-output-hook nil
  "*The hook run after output has been formatted."
  :type 'hook
  :group 'lui)

(defvar lui-time-stamp-face 'lui-time-stamp-face
  "Face name for time stamps.")
(defface lui-time-stamp-face
  '((t (:foreground "SlateBlue" :weight bold)))
  "*Lui mode face used for time stamps."
  :group 'lui)

(defvar lui-highlight-face 'lui-highlight-face
  "Face name for highlighting.")
(defface lui-highlight-face
  ;; Taken from `font-lock-keyword-face'
  '((((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan1"))
    (t (:weight bold)))
  "*Lui mode face used for highlighting."
  :group 'lui)


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Client interface ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defvar lui-input-function nil
  "The function to be called for Lui input.
This function is called with a single argument, the input
string.")
(make-variable-buffer-local 'lui-input-function)

(defvar lui-completion-function nil
  "A function called to retrieve current completions.
This receives one argument, which is non-nil when the completion
happens at the beginning of a line.

It is often a good idea to make this variable buffer-local.")


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private variables ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar lui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'lui-send-input)
    (define-key map (kbd "TAB") 'incomplete)
    (define-key map (kbd "M-p") 'lui-previous-input)
    (define-key map (kbd "M-n") 'lui-next-input)
    (define-key map (kbd "C-c C-u") 'lui-kill-to-beginning-of-line)
    map)
  "The key map used in Lui modes.")

(global-set-key (kbd "C-c C-SPC") 'lui-track-next-buffer)
(global-set-key (kbd "C-c C-@") 'lui-track-next-buffer)

(defvar lui-input-marker nil
  "The marker where input should be inserted.")
(make-variable-buffer-local 'lui-input-marker)

(defvar lui-output-marker nil
  "The marker where output should be inserted.
Use `lui-insert' instead of accessing this marker directly.")
(make-variable-buffer-local 'lui-output-marker)

(defvar lui-input-ring nil
  "The input history ring.")
(make-variable-buffer-local 'lui-input-ring)

(defvar lui-input-ring-index nil
  "The index to the current item in the input ring.")
(make-variable-buffer-local 'lui-input-ring-index)


;;;;;;;;;;;;;;;;;;
;;; Major Mode ;;;
;;;;;;;;;;;;;;;;;;

(defun lui-mode ()
  "The Linewise User Interface mode.
This can be used as a user interface for various applications.
Those should define derived modes of this, so this function
should never be called directly.

It can be customized for an application by specifying a
`lui-input-function', and possibly `lui-completion-function'."
  (kill-all-local-variables)
  (setq major-mode 'lui-mode
        mode-name "LUI")
  (use-local-map lui-mode-map)
  ;; Buffer-local variables
  (setq lui-input-marker (make-marker)
        lui-output-marker (make-marker)
        lui-input-ring (make-ring lui-input-ring-size)
        lui-input-ring-index nil
        flyspell-generic-check-word-p 'lui-flyspell-verify)
  (set-marker lui-input-marker (point-max))
  (set-marker lui-output-marker (point-max))
  (add-hook 'window-scroll-functions
            'lui-scroll-to-bottom
            nil t)
  (when (fboundp 'make-local-hook)
    ;; needed for xemacs, as it does not treat the LOCAL argument to
    ;; `add-hook' the same as GNU Emacs. It's obsolete in GNU Emacs
    ;; sind 21.1.
    (make-local-hook 'change-major-mode-hook))
  (add-hook 'change-major-mode-hook
            'lui-change-major-mode
            nil t)
  (lui-track-initialize)
  (when lui-flyspell-p
    (require 'flyspell)
    (lui-flyspell-change-dictionary))
  (set (make-local-variable 'incomplete-function)
       'lui-incomplete)
  (run-hooks 'lui-mode-hook))

(defun lui-scroll-to-bottom (window display-start)
  "Scroll the input line to the bottom of the window."
  (when (and window
             (window-live-p window)
             lui-scroll-to-bottom-p)
    (let ((resize-mini-windows nil))
      ;; This is to prevent an XEmacs byte compilation warning
      ;; "variable bound but not referred to". XEmacs is trying to be
      ;; too intelligent.
      (when (featurep 'xemacs)
        (declare (special resize-mini-windows)))
      (save-selected-window
        (select-window window)
        (save-restriction
          (widen)
          (when (>= (point) lui-input-marker)
            (save-excursion
              (goto-char (point-max))
              (recenter -1)
              (sit-for 0))))))))

(defun lui-change-major-mode ()
  "Assure that the user really wants to change the major mode.
This is a good value for a buffer-local `change-major-mode-hook'."
  (when (not (y-or-n-p "Really change major mode in a Lui buffer? "))
    (error "User disallowed mode change.")))


;;;;;;;;;;;;;
;;; Input ;;;
;;;;;;;;;;;;;

(defun lui-send-input ()
  "Send the current input to the Lui application.
If point is not in the input area, self-insert."
  (interactive)
  (if (< (point) lui-input-marker)
      (self-insert-command 1)
    (save-restriction
      (narrow-to-region lui-input-marker (point-max))
      (run-hooks 'lui-pre-input-hook))
    (let ((input (buffer-substring lui-input-marker (point-max))))
      (delete-region lui-input-marker (point-max))
      (ring-insert lui-input-ring input)
      (setq lui-input-ring-index nil)
      (if lui-input-function
          (funcall lui-input-function input)
        (error "No input function specified")))))


;;;;;;;;;;;;;;;;;;
;;; Completion ;;;
;;;;;;;;;;;;;;;;;;

(defun lui-incomplete ()
  "Return the string to be completed at point."
  (let ((end (point))
        (begin (save-excursion (if (not (re-search-backward "\\s-"
                                                            lui-input-marker
                                                            t))
                                   lui-input-marker
                                 (forward-char)
                                 (point)))))
    (cons (buffer-substring-no-properties begin end)
          (funcall lui-completion-function (= begin
                                              lui-input-marker)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input Line Killing ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lui-kill-to-beginning-of-line ()
  "Kill the input from point to the beginning of the input."
  (interactive)
  (let* ((beg (point-at-bol))
         (end (point))
         (str (buffer-substring beg end)))
    (delete-region beg end)
    (kill-new str)))


;;;;;;;;;;;;;;;;;;;;;
;;; Input History ;;;
;;;;;;;;;;;;;;;;;;;;;

;; FIXME!
;; These need some better algorithm. They clobber input when it is not
;; in the ring!
(defun lui-previous-input ()
  "Cycle through the input history to the last input."
  (interactive)
  (when (> (ring-length lui-input-ring) 0)
    (if (and lui-input-ring-index
             (= (1- (ring-length lui-input-ring))
                lui-input-ring-index))
        ;; last item - insert a single empty line
        (progn
          (lui-replace-input "")
          (setq lui-input-ring-index nil))
      ;; If any input is left, store it in the input ring
      (when (and (null lui-input-ring-index)
                 (> (point-max) lui-input-marker))
        (ring-insert lui-input-ring
                     (buffer-substring lui-input-marker (point-max)))
        (setq lui-input-ring-index 0))
      ;; Increment the index
      (setq lui-input-ring-index
            (if lui-input-ring-index
                (ring-plus1 lui-input-ring-index (ring-length lui-input-ring))
              0))
      ;; And insert the last input
      (lui-replace-input (ring-ref lui-input-ring lui-input-ring-index))
      (goto-char (point-max)))))

(defun lui-next-input ()
  "Cycle through the input history to the next input."
  (interactive)
  (when (> (ring-length lui-input-ring) 0)
    (if (and lui-input-ring-index
             (= 0 lui-input-ring-index))
        ;; first item - insert a single empty line
        (progn
          (lui-replace-input "")
          (setq lui-input-ring-index nil))
      ;; If any input is left, store it in the input ring
      (when (and (null lui-input-ring-index)
                 (> (point-max) lui-input-marker))
        (ring-insert lui-input-ring
                     (buffer-substring lui-input-marker (point-max)))
        (setq lui-input-ring-index 0))
      ;; Decrement the index
      (setq lui-input-ring-index (ring-minus1 (or lui-input-ring-index 0)
                                              (ring-length lui-input-ring)))
      ;; And insert the next input
      (lui-replace-input (ring-ref lui-input-ring lui-input-ring-index))
      (goto-char (point-max)))))

(defun lui-replace-input (str)
  "Replace input with STR."
  (save-excursion
    (goto-char lui-input-marker)
    (delete-region lui-input-marker (point-max))
    (insert str)))

(defvar lui-ignored-showing-p nil
  "Whether we currently show text which is ignored.")

(defun lui-ignore ()
  "Hide ignored text in the current buffer.
If an application sets the text property `lui-ignored', this text
is considered as ignored, and will normally not be shown."
  (when (not lui-ignored-showing-p)
    (lui-ignore-propertize (point-min) (point-max)
                           '(invisible t))))

(defun lui-toggle-ignored ()
  "This toggles displaying text marked as ignored."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (lui-ignore-propertize (point-min) (point-max)
                             (if lui-ignored-showing-p
                                 '(invisible nil)
                               '(invisible t)))
      (setq lui-ignored-showing-p (not lui-ignored-showing-p)))))

(defun lui-ignore-propertize (from to properties)
  "Add PROPERTIES to any ignored text between BEG and END.
Ignored text is text with a non-nil `lui-ignored' property."
  (let ((beg (text-property-any from to
                                'lui-ignored t))
        (end nil))
    (while beg
      (setq end (or (next-single-property-change beg 'lui-ignored)
                    to))
      (when (and (progn (goto-char beg)
			(bolp))
                 (progn (goto-char end)
                        (eolp)))
        (setq end (+ 1 end)))
      (add-text-properties beg end properties)
      (setq beg (text-property-any end to 'lui-ignored t)))))

(defun lui-flyspell-change-dictionary (&optional dictionary)
  "*Change flyspell to DICTIONARY.
If DICTIONARY is nil, set a default dictionary according to
`lui-flyspell-alist'.
If it is \"\", disable flyspell."
  (interactive (list (completing-read
                      "Use new dictionary (RET for none, SPC to complete): "
                      (and (fboundp 'ispell-valid-dictionary-list)
                           (mapcar 'list (ispell-valid-dictionary-list)))
                      nil t)))
  (cond
   ((not (fboundp 'flyspell-mode))
    (error "Flyspell mode is not loaded"))
   ((string= dictionary "")
    (flyspell-mode 0))
   (t
    (let ((dictionary (or dictionary
                          (lui-find-dictionary (buffer-name)))))
      (when dictionary
        (when (or (not (boundp 'flyspell-mode))
                  (not flyspell-mode))
          (flyspell-mode 1))
        (setq ispell-local-dictionary dictionary))))))

(defun lui-find-dictionary (buffer-name)
  "Return a dictionary appropriate for BUFFER-NAME."
  (let ((lis lui-flyspell-alist)
        (result nil))
    (while lis
      (if (string-match (caar lis) buffer-name)
          (setq result (cadr (car lis))
                lis nil)
         (setq lis (cdr lis))))
    result))

(defun lui-flyspell-verify ()
  "Return non-nil when flyspell should verify at this position.
This is the value of Lui for `flyspell-generic-check-word-p'."
  (> (point)
     lui-input-marker))


;;;;;;;;;;;;;;
;;; Output ;;;
;;;;;;;;;;;;;;

(defun lui-insert (str &optional not-tracked-p)
  "Insert STR into the current Lui buffer."
  ;; Don't modify the undo list. The undo list is for the user's
  ;; input only.
  (let ((old-output-marker (marker-position lui-output-marker)))
    (let ((buffer-undo-list t))
      (save-excursion
        (save-restriction
          (let ((inhibit-read-only t)
                (faces nil))
            (widen)
            (goto-char lui-output-marker)
            (let ((beg (point))
                  (end nil))
              (insert str "\n")
              (setq end (point))
              (set-marker lui-output-marker (point))
              (narrow-to-region beg end))
            (goto-char (point-min))
            (run-hooks 'lui-pre-output-hook)
            (lui-highlight-keywords)
            (lui-fill)
            (lui-time-stamp)
            (goto-char (point-min))
            (run-hooks 'lui-post-output-hook)
            (lui-ignore)
            (goto-char (point-min))
            (setq faces (lui-faces-in-region (point-min)
                                             (point-max)))
            (widen)
            (lui-truncate)
            (lui-read-only)
            (when (and (not not-tracked-p)
                       (not (get-buffer-window (current-buffer)
                                               (if lui-track-all-frames-p
                                                   'visible
                                                 nil))))
              (lui-track-set-modified-status (buffer-name (current-buffer))
                                             t
                                             faces))))))
    (setq buffer-undo-list (lui-adjust-undo-list buffer-undo-list
                                                 old-output-marker
                                                 (- lui-output-marker
                                                    old-output-marker)))
    nil))

(defun lui-adjust-undo-list (list old-begin shift)
  "Adjust undo positions in LIST by SHIFT.
LIST is in the format of `buffer-undo-list'.
Only positions after OLD-BEGIN are affected."
  ;; This is necessary because the undo-list keeps exact buffer
  ;; positions.
  ;; Thanks to ERC for the idea of the code.
  ;; ERC's code doesn't take care of an OLD-BEGIN value, which is
  ;; necessary if you allow modification of the buffer.
  (let* ((adjust-position (lambda (pos)
                            (cond
                             ;; Negative: From end
                             ((< pos 0)
                              (- pos shift))
                             ;; Before the boundary
                             ((< pos old-begin)
                              pos)
                             ;; After the boundary
                             (t
                              (+ pos shift)))))
         (adjust (lambda (entry)
                   (cond
                    ;; POSITION
                    ((numberp entry)
                     (funcall adjust-position entry))
                    ((not (consp entry))
                     entry)
                    ;; (BEG . END)
                    ((numberp (car entry))
                     (cons (funcall adjust-position (car entry))
                           (funcall adjust-position (cdr entry))))
                    ;; (TEXT . POSITION)
                    ((stringp (car entry))
                     (cons (car entry)
                           (funcall adjust-position (cdr entry))))
                    ;; (nil PROPERTY VALUE BEG . END)
                    ((not (car entry))
                     `(nil ,(nth 1 entry)
                           ,(nth 2 entry)
                           ,(funcall adjust-position (nth 3 entry))
                           .
                           ,(funcall adjust-position (nthcdr 4 entry))))
                    ;; (apply DELTA BEG END FUN-NAME . ARGS)
                    ((eq 'apply (car entry))
                     `(apply ,(nth 1 entry)
                             ,(funcall adjust-position (nth 2 entry))
                             ,(funcall adjust-position (nth 3 entry))
                             ,(nth 4 entry)
                             .
                             ,(nthcdr 5 entry)))
                    ;; XEmacs: (<extent> start end)
                    ((and (fboundp 'extentp)
                          (extentp (car entry)))
                     (list (nth 0 entry)
                           (funcall adjust-position (nth 1 entry))
                           (funcall adjust-position (nth 2 entry))))
                    (t
                     entry)))))
    (mapcar adjust list)))

(defvar lui-prompt-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<end>") 'lui-prompt-end-of-line)
    (define-key map (kbd "C-e") 'lui-prompt-end-of-line)
    map)
  "Keymap for Lui prompts.
Since \\[end-of-line] can't move out of fields, this DTRT for an
unexpecting user.")

(defun lui-set-prompt (prompt)
  "Set PROMPT as the current Lui prompt."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char lui-output-marker)
      (insert prompt)
      (if (> lui-input-marker (point))
          (delete-region (point) lui-input-marker)
        (set-marker lui-input-marker (point)))
      (add-text-properties lui-output-marker lui-input-marker
                           `(read-only t
                             rear-nonsticky t
                             field lui-prompt
                             keymap ,lui-prompt-map)))))

(defun lui-prompt-end-of-line (&optional N)
  "Move past the prompt, and then to the end of the line.
This uses `end-of-line'."
  (interactive "p")
  (goto-char lui-input-marker)
  (call-interactively 'end-of-line))

(defun lui-faces-in-region (beg end)
  "Returns a face that describes the region between BEG and END."
  (goto-char beg)
  (let ((faces nil))
    (while (not (= (point) end))
      (let ((face (get-text-property (point) 'face)))
        (mapc (lambda (face)
                (when (and face
                           (facep face)
                           (face-differs-from-default-p face))
                  (add-to-list 'faces face)))
              (if (consp face)
                  face
                (list face)))
        (goto-char (next-single-property-change (point) 'face
                                                nil end))))
    faces))



;;;;;;;;;;;;;;;;;;;;
;;; Highlighting ;;;
;;;;;;;;;;;;;;;;;;;;

(defun lui-highlight-keywords ()
  "Highlight the entries in `lui-highlight-keywords' in buffer.
This is called automatically when new text is inserted."
  (let ((highlights lui-highlight-keywords)
        (regex (lambda (entry)
                 (if (stringp entry)
                     entry
                   (car entry))))
        (submatch (lambda (entry)
                    (if (or (stringp entry)
                            (symbolp (cadr entry)))
                        0
                      (cadr entry))))
        (face (lambda (entry)
                (if (or (stringp entry)
                        (and (numberp (cadr entry))
                             (null (cddr entry))))
                    'lui-highlight-face
                  (if (numberp (cadr entry))
                      (nth 2 entry)
                    (cadr entry))))))
    (mapc (lambda (entry)
            (goto-char (point-min))
            (while (re-search-forward (funcall regex entry) nil t)
              (let* ((exp (funcall submatch entry))
                     (beg (match-beginning exp))
                     (end (match-end exp)))
                (when (not (text-property-any beg end 'lui-highlight-fontified-p t))
                  (add-text-properties beg end `(face ,(funcall face entry)
                                                      lui-highlight-fontified-p t))))))
          highlights)))


;;;;;;;;;;;;;;;
;;; Filling ;;;
;;;;;;;;;;;;;;;

(defun lui-fill ()
  "Fill the text in the buffer.
This is called automatically when new text is inserted. See
`lui-fill-type' and `lui-fill-column' on how to customize this
function."
  (cond
   ((stringp lui-fill-type)
    (let ((fill-prefix lui-fill-type)
          (fill-column (or lui-fill-column
                           fill-column)))
      (fill-region (point-min) (point-max)
                   nil t)))
   ((eq lui-fill-type 'variable)
    (let ((fill-prefix (save-excursion
                         (goto-char (point-min))
                         (let ((beg (point)))
                           (re-search-forward "\\s-" nil t)
                           (make-string (- (point) beg) ? ))))
          (fill-column (or lui-fill-column
                           fill-column)))
      (fill-region (point-min) (point-max)
                   nil t)))
   ((numberp lui-fill-type)
    (let ((right-end (save-excursion
                       (goto-char (point-min))
                       (let ((beg (point)))
                         (re-search-forward "\\s-" nil t)
                         (- (point)
                            (point-at-bol))))))
      (goto-char (point-min))
      (when (< right-end lui-fill-type)
        (insert (make-string (- lui-fill-type
                                right-end)
                             ? )))
      (let ((fill-prefix (make-string lui-fill-type ? ))
            (fill-column (or lui-fill-column
                             fill-column)))
        (fill-region (point-min) (point-max)
                     nil t)))))
  (when lui-fill-remove-face-from-newline
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (put-text-property (match-beginning 0)
                         (match-end 0)
                         'face
                         nil))))


;;;;;;;;;;;;;;;;;;;
;;; Time Stamps ;;;
;;;;;;;;;;;;;;;;;;;

(defvar lui-time-stamp-last nil
  "The last time stamp.")
(make-variable-buffer-local 'lui-time-stamp-last)

(defun lui-time-stamp ()
  "Add a time stamp to the current buffer."
  (let ((ts (format-time-string lui-time-stamp-format)))
    (cond
     ;; Timestamps right
     ((or (numberp lui-time-stamp-position)
          (eq lui-time-stamp-position 'right))
      (when (or (not lui-time-stamp-only-when-changed-p)
                (not lui-time-stamp-last)
                (not (string= ts lui-time-stamp-last)))
        (goto-char (point-min))
        (goto-char (point-at-eol))
        (insert " ") ; Overlay alone would prevent us from marking this line
        (let* ((ov (make-overlay (point) (point)))
               (curcol (current-column))
               (col (if (numberp lui-time-stamp-position)
                        lui-time-stamp-position
                      (+ 2 (or lui-fill-column
                               fill-column
                               (point)))))
               (indent (if (> col curcol)
                           (- col curcol)
                         1)))
          (overlay-put ov 'field 'lui-time-stamp)
          (overlay-put ov 'after-string
                       (concat (make-string indent ? )
                                (propertize
                                 ts
                                 'face 'lui-time-stamp-face))))))
     ;; Timestamps left
     ((eq lui-time-stamp-position 'left)
      (let ((indent-string (make-string (length ts) ? )))
        (goto-char (point-min))
        (cond
         ;; Time stamp
         ((or (not lui-time-stamp-only-when-changed-p)
              (not lui-time-stamp-last)
              (not (string= ts lui-time-stamp-last)))
          (let ((ov (make-overlay (point) (point))))
            (overlay-put ov 'field 'lui-time-stamp)
            (overlay-put ov 'before-string
                         (propertize ts 'face 'lui-time-stamp-face))))
         ;; Just indentation
         (t
          (let ((ov (make-overlay (point) (point))))
            (overlay-put ov 'field 'lui-time-stamp-indentation)
            (overlay-put ov 'before-string indent-string))))
        (forward-line 1)
        (while (< (point) (point-max))
          (let ((ov (make-overlay (point) (point))))
            (overlay-put ov 'field 'lui-time-stamp-indentation)
            (overlay-put ov 'before-string indent-string))
          (forward-line 1)))))
    (setq lui-time-stamp-last ts)))


;;;;;;;;;;;;;;;;;;
;;; Truncating ;;;
;;;;;;;;;;;;;;;;;;

(defun lui-truncate ()
  "Truncate the current buffer if it exceeds `lui-max-buffer-size'."
  (when (and lui-max-buffer-size
             (> (point-max)
                lui-max-buffer-size))
    (goto-char (- (point-max)
                  lui-max-buffer-size))
    (forward-line 0)
    (let ((inhibit-read-only t))
      (remove-overlays (point-min) (point))
      (delete-region (point-min) (point)))))


;;;;;;;;;;;;;;;;;
;;; Read-Only ;;;
;;;;;;;;;;;;;;;;;

(defun lui-read-only ()
  "Make the current output read-only if `lui-read-only-output-p' is non-nil."
  (when lui-read-only-output-p
    (add-text-properties (point-min) lui-output-marker
                         '(read-only t
                           front-sticky t))))


;;;;;;;;;;;;;;;;
;;; Tracking ;;;
;;;;;;;;;;;;;;;;

(defvar lui-track-initialized-p nil
  "Non-nil when Lui tracking has been initialized.")

(defvar lui-track-buffers nil
  "The list of currently tracked buffers.")

(defvar lui-track-mode-line-buffers ""
  "The entry to the mode line.")
(put 'lui-track-mode-line-buffers 'risky-local-variable t)

(defun lui-track-initialize ()
  "Initialize Lui tracking."
  (when (not lui-track-initialized-p)
    (cond
     ((and (boundp 'mode-line-modes)
           (eq lui-track-position 'before-modes))
      (add-to-list 'mode-line-modes
                   '(t lui-track-mode-line-buffers)))
     ((and (boundp 'mode-line-modes)
           (eq lui-track-position 'after-modes))
      (add-to-list 'mode-line-modes
                   '(t lui-track-mode-line-buffers)
                   t))
     (t
      (add-to-list 'global-mode-string
                   'lui-track-mode-line-buffers
                   t)))
    (add-hook 'window-configuration-change-hook
              'lui-track-active)
    (setq lui-track-initialized-p t)))

(defun lui-track-set-modified-status (buffer status &optional faces)
  "Set the modified status of BUFFER to STATUS.
When STATUS is non-nil, the buffer is considered to be modified.
If STATUS is nil, the buffer is considered unmodified.

Modified buffers are shown in the mode line, and
\\[lui-track-next-buffer] cycles through these.

If FACES is given, it's the faces that might be appropriate for
BUFFER in the mode line."
  (if (not status)
      (setq lui-track-buffers (delete buffer lui-track-buffers))
    (when (not (lui-track-ignored-p buffer faces))
      (let* ((entry (member buffer lui-track-buffers)))
        (if entry
            (setcar entry (lui-faces-merge (car entry)
                                           faces))
          (setq lui-track-buffers
                (nconc lui-track-buffers
                       (list (lui-faces-merge buffer
                                              faces))))))))
  (setq lui-track-mode-line-buffers (lui-track-status))
  (sit-for 0) ;; Update mode line
  )

(defun lui-track-ignored-p (buffer faces)
  "Return non-nil when BUFFER with FACES shouldn't be tracked.
This uses `lui-track-ignored-buffers'."
  (catch 'return
    (mapc (lambda (entry)
            (if (and (stringp entry)
                     (string-match entry buffer))
                (throw 'return t)
              (when (and (string-match (car entry) buffer)
                         (not (lui-any-in (or (cdr entry)
                                              lui-track-faces-priorities)
                                          faces)))
                (throw 'return t))))
          lui-track-ignored-buffers)
    nil))

(defun lui-any-in (lista listb)
  "Return non-nil when any element in LISTA is in LISTB"
  (catch 'return
    (mapc (lambda (entry)
            (when (memq entry listb)
              (throw 'return t)))
          lista)
    nil))

(defun lui-track-status ()
  "Return the current track status."
  (let ((shortened (lui-track-shorten lui-track-buffers)))
    (if shortened
        (concat " [" (mapconcat #'identity shortened ",") "] ")
      "")))

(defun lui-track-shorten (buffers)
  "Shorten BUFFERS according to `lui-track-shorten-buffer-names-p'."
  (if lui-track-shorten-buffer-names-p
      (let ((all-buffers (mapcar #'buffer-name (buffer-list))))
        (mapcar (lambda (buffer)
                  (lui-track-shorten-single buffer
                                            (remove buffer all-buffers)))
                buffers))
    buffers))

(defun lui-track-shorten-single (str list)
  "Return the shortest form of STR which is unambiguous in LIST."
  (let ((prefix (substring str 0 1))
        (prefix-length 1)
        (str-length (length str)))
    (catch 'return
      (while (< prefix-length
                str-length)
        (setq prefix (substring str 0 prefix-length))
        (when (not (lui-track-find-prefix prefix list))
          (throw 'return prefix))
        (setq prefix-length (+ 1 prefix-length)))
      str)))

(defun lui-track-find-prefix (prefix list)
  "Return non-nil when a string in LIST begins with PREFIX."
  (let ((rx (concat "^" (regexp-quote prefix))))
    (catch 'return
      (while list
        (when (string-match rx (car list))
          (throw 'return t))
        (setq list (cdr list)))
      nil)))

(defun lui-track-active ()
  "Remove visible buffers from the tracked buffers.
This is usually called via `window-configuration-changed-hook'."
  (interactive)
  (mapc (lambda (buffer)
          (when (or (not (get-buffer buffer))
                    (get-buffer-window buffer
                                       (if lui-track-all-frames-p
                                           'visible
                                         nil)))
            (lui-track-set-modified-status buffer nil)))
        lui-track-buffers)
  (setq lui-track-mode-line-buffers (lui-track-status))
  )

(defvar lui-track-start-buffer nil
  "The buffer we started from when cycling through the active buffers.")

(defun lui-track-next-buffer ()
  "Switch to the next active buffer."
  (interactive)
  (cond
   ((and (not lui-track-buffers)
         lui-track-start-buffer)
    (let ((buf lui-track-start-buffer))
      (setq lui-track-start-buffer nil)
      (switch-to-buffer buf)))
   ((not lui-track-buffers)
    nil)
   (t
    (when (not lui-track-start-buffer)
      (setq lui-track-start-buffer (current-buffer)))
    (let ((new (car lui-track-buffers)))
      (setq lui-track-buffers (cdr lui-track-buffers)
            lui-track-mode-line-buffers (lui-track-status))
      (switch-to-buffer new)
      (sit-for 0) ;; Update mode line
      ))))

(defun lui-track-previous-buffer ()
  "Switch to the last active buffer."
  (interactive)
  (when lui-track-buffers
    (switch-to-buffer (car (last lui-track-buffers)))))

(defun lui-faces-merge (string faces)
  "Merge faces into string, adhering to `lui-track-faces-priorities'.
This returns STRING with the new face."
  (let ((faces (cons (get-text-property 0 'face string)
                     faces)))
    (catch 'return
      (mapc (lambda (candidate)
              (when (memq candidate faces)
                (throw 'return
                       (propertize string 'face candidate))))
            lui-track-faces-priorities)
      string)))


(provide 'lui)
;;; lui.el ends here
