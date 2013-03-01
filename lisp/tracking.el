;;; tracking.el --- Buffer modification tracking

;; Copyright (C) 2006, 2012  Jorgen Schaefer

;; Version: 1.2
;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; URL: https://github.com/jorgenschaefer/circe/wiki/Tracking

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; tracking.el is a library for other Emacs Lisp programs not useful
;; by itself.

;; The library provides a way to globally register buffers as being
;; modified and scheduled for user review. The user can cycle through
;; the buffers using C-c C-SPC. This is especially useful for buffers
;; that interact with external sources, such as chat clients and
;; similar programs.

;;; Code:

(require 'easy-mmode)

;;; User customization
(defgroup tracking nil
  "Tracking of buffer activities."
  :prefix "tracking-"
  :group 'applications)

(defcustom tracking-shorten-buffer-names-p t
  "Whether to shorten buffer names in the mode line.
A non-nil value will cause tracked buffer names to be shortened
as much as possible to stay unambiguous when displaying them in
the mode line."
  :type 'boolean
  :group 'tracking)

(defcustom tracking-frame-behavior 'visible
  "How to deal with frams to determine visibility of buffers.
This is passed as the second argument to `get-buffer-window',
see there for further explanation."
  :type '(choice (const :tag "All visible frames" visible)
                 (const :tag "Visible and iconified frames" 0)
                 (const :tag "All frames" t)
                 (const :tag "Selected frame only" nil))
  :group 'tracking)

(defcustom tracking-position 'before-modes
  "Where tracked buffers should appear in the mode line.

  'before-modes
      Before the mode indicators
  'after-modes
      After the mode indicators
  'end
      At the end of the mode line"
  :type '(choice (const :tag "Before the Mode Indicators" before-modes)
                 (const :tag "Afterthe Mode Indicators" after-modes)
                 (const :tag "At the End of the Mode Line" end))
  :group 'tracking)

(defcustom tracking-faces-priorities nil
  "A list of faces which should be shown by tracking in the mode line.
The first face found in this list is used."
  :type '(repeat face)
  :group 'tracking)

(defcustom tracking-ignored-buffers nil
  "A list of buffers that are never tracked.
Each element of this list has one of the following forms:

  regexp - Any buffer matching won't be tracked.
  function - Any buffer matching won't be tracked.
  (regexp faces ...) - Any buffer matching won't be tracked,
      unless it has a face in FACES ... associated with it.
      If no faces are given, `tracking-faces-priorities' is
      used.
  (function faces ...) - As per above, but with a function
      as predicate instead of a regexp."
  :type '(repeat (choice regexp
                         function
                         (list (choice regexp function)
                               (repeat face))))
  :group 'tracking)

(defcustom tracking-most-recent-first nil
  "When non-nil, newly tracked buffers will go to the front of the
list, rather than to the end."
  :type 'boolean
  :group 'tracking)


;;; Internal variables
(defvar tracking-buffers nil
  "The list of currently tracked buffers.")

(defvar tracking-mode-line-buffers ""
  "The entry to the mode line.")
(put 'tracking-mode-line-buffers 'risky-local-variable t)

(defvar tracking-start-buffer nil
  "The buffer we started from when cycling through the active buffers.")

(defvar tracking-last-buffer nil
  "The buffer we last switched to with `tracking-next-buffer'.
When this is not the current buffer when we continue switching, a
new `tracking-start-buffer' is created.")

(defvar tracking-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-SPC") 'tracking-next-buffer)
    (define-key map (kbd "C-c C-@") 'tracking-next-buffer)
    map)
  "The keymap used for tracking mode.")

;;;###autoload
(define-minor-mode tracking-mode
  "Allow cycling through modified buffers.
This mode in itself does not track buffer modification, but
provides an API for programs to add buffers as modified (using
`tracking-add-buffer').

Once this mode is active, modified buffers are shown in the mode
line. The user can cycle through them using
\\[tracking-next-buffer]."
  :group 'tracking
  :global t
  (cond
   (tracking-mode
    (cond
     ((and (boundp 'mode-line-modes)
           (eq tracking-position 'before-modes))
      (add-to-list 'mode-line-modes
                   '(t tracking-mode-line-buffers)))
     ((and (boundp 'mode-line-modes)
           (eq tracking-position 'after-modes))
      (add-to-list 'mode-line-modes
                   '(t tracking-mode-line-buffers)
                   t))
     (t
      ;; Bug in Emacs 21.3, must not have a single symbol alone in
      ;; `global-mode-string'.
      (when (not global-mode-string)
        (setq global-mode-string '("")))
      (add-to-list 'global-mode-string
                   'tracking-mode-line-buffers
                   t)))
    (add-hook 'window-configuration-change-hook
              'tracking-remove-visible-buffers))
   (t
    (when (boundp 'mode-line-modes)
      (setq mode-line-modes (delete '(t tracking-mode-line-buffers)
                                    mode-line-modes)))
    (setq global-mode-string (delq 'tracking-mode-line-buffers
                                   global-mode-string))
    (remove-hook 'window-configuration-change-hook
                 'tracking-remove-visible-buffers))))

;;;###autoload
(defun tracking-add-buffer (buffer &optional faces)
  "Add BUFFER as being modified with FACES.
This does check whether BUFFER is currently visible.

If FACES is given, it lists the faces that might be appropriate
for BUFFER in the mode line. The highest-priority face of these
and the current face of the buffer, if any, is used. Priority is
decided according to `tracking-faces-priorities'."
  (when (and (not (get-buffer-window buffer tracking-frame-behavior))
             (not (tracking-ignored-p buffer faces)))
    (let* ((entry (member (buffer-name buffer)
                          tracking-buffers)))
      (if entry
          (setcar entry (tracking-faces-merge (car entry)
                                              faces))
        (setq tracking-buffers
              (if tracking-most-recent-first
                  (cons (tracking-faces-merge (buffer-name buffer)
                                              faces)
                        tracking-buffers)
                  (nconc tracking-buffers
                         (list (tracking-faces-merge (buffer-name buffer)
                                                     faces)))))))
    (setq tracking-mode-line-buffers (tracking-status))
    (sit-for 0) ;; Update mode line
    ))

;;;###autoload
(defun tracking-remove-buffer (buffer)
  "Remove BUFFER from being tracked."
  (setq tracking-buffers (delete (buffer-name buffer)
                                 tracking-buffers))
  (setq tracking-mode-line-buffers (tracking-status))
  (sit-for 0) ;; Update mode line
  )

;;;###autoload
(defun tracking-next-buffer ()
  "Switch to the next active buffer."
  (interactive)
  (cond
   ((and (not tracking-buffers)
         tracking-start-buffer)
    (let ((buf tracking-start-buffer))
      (setq tracking-start-buffer nil)
      (if (buffer-live-p buf)
          (switch-to-buffer buf)
        (message "Original buffer does not exist anymore")
        (ding))))
   ((not tracking-buffers)
    nil)
   (t
    (when (not (eq tracking-last-buffer
                   (current-buffer)))
      (setq tracking-start-buffer (current-buffer)))
    (let ((new (car tracking-buffers)))
      (setq tracking-buffers (cdr tracking-buffers)
            tracking-mode-line-buffers (tracking-status))
      (if (buffer-live-p (get-buffer new))
          (switch-to-buffer new)
        (message "Buffer %s does not exist anymore" new)
        (ding)
        (setq tracking-mode-line-buffers (tracking-status))))
    (setq tracking-last-buffer (current-buffer))
    (sit-for 0) ;; Update mode line
    )))

;;;###autoload
(defun tracking-previous-buffer ()
  "Switch to the last active buffer."
  (interactive)
  (when tracking-buffers
    (switch-to-buffer (car (last tracking-buffers)))))

(defun tracking-ignored-p (buffer faces)
  "Return non-nil when BUFFER with FACES shouldn't be tracked.
This uses `tracking-ignored-buffers'.  Actual returned value is
the entry from tracking-ignored-buffers that causes this buffer
to be ignored."
  (catch 'return
    (let ((buffer-name (buffer-name buffer)))
      (dolist (entry tracking-ignored-buffers)
        (cond
         ((stringp entry)
          (and (string-match entry buffer-name)
               (throw 'return entry)))
         ((functionp entry)
          (and (funcall entry buffer-name)
               (throw 'return entry)))
         ((or (and (stringp (car entry))
                   (string-match (car entry) buffer-name))
              (and (functionp (car entry))
                   (funcall (car entry) buffer-name)))
          (when (not (tracking-any-in (or (cdr entry)
                                          tracking-faces-priorities)
                                      faces))
            (throw 'return entry))))))
    nil))

(defun tracking-status ()
  "Return the current track status."
  (let ((shortened (tracking-shorten tracking-buffers)))
    (if shortened
        (concat " [" (mapconcat #'identity shortened ",") "] ")
      "")))

(defun tracking-remove-visible-buffers ()
  "Remove visible buffers from the tracked buffers.
This is usually called via `window-configuration-changed-hook'."
  (interactive)
  (dolist (buffer-name tracking-buffers)
    (let ((buffer (get-buffer buffer-name)))
      (when (or (not buffer)
                (get-buffer-window buffer
                                   tracking-frame-behavior))
        (tracking-remove-buffer buffer))))
  (setq tracking-mode-line-buffers (tracking-status)))

;;; Helper functions
(defun tracking-shorten (buffers)
  "Shorten BUFFERS according to `tracking-shorten-buffer-names-p'."
  (if tracking-shorten-buffer-names-p
      (let ((all-buffers (mapcar #'buffer-name (buffer-list))))
        (mapcar (lambda (buffer)
                  (tracking-shorten-single buffer
                                           (remove buffer all-buffers)))
                buffers))
    buffers))

(defun tracking-shorten-single (str list)
  "Return the shortest form of STR which is unambiguous in LIST."
  (let ((prefix (substring str 0 1))
        (prefix-length 1)
        (str-length (length str)))
    (catch 'return
      (while (< prefix-length
                str-length)
        (setq prefix (substring str 0 prefix-length))
        (when (not (tracking-find-prefix prefix list))
          (throw 'return prefix))
        (setq prefix-length (+ 1 prefix-length)))
      str)))

(defun tracking-find-prefix (prefix list)
  "Return non-nil when a string in LIST begins with PREFIX."
  (let ((rx (concat "^" (regexp-quote prefix))))
    (catch 'return
      (while list
        (when (string-match rx (car list))
          (throw 'return t))
        (setq list (cdr list)))
      nil)))

(defun tracking-any-in (lista listb)
  "Return non-nil when any element in LISTA is in LISTB"
  (catch 'return
    (dolist (entry lista)
      (when (memq entry listb)
        (throw 'return t)))
    nil))

(defun tracking-faces-merge (string faces)
  "Merge faces into string, adhering to `tracking-faces-priorities'.
This returns STRING with the new face."
  (let ((faces (cons (get-text-property 0 'face string)
                     faces)))
    (catch 'return
      (dolist (candidate tracking-faces-priorities)
        (when (memq candidate faces)
          (throw 'return
                 (propertize string 'face candidate))))
      string)))

(provide 'tracking)
;;; tracking.el ends here
