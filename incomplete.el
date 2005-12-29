;;; incomplete.el --- Intelligent Completion

;; Copyright (C) 2005  Jorgen Schaefer

;; Version: 1.0
;; Keywords: Completion, pcomplete
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

;; Intelligent in-buffer completion which is not specialized on
;; commands with arguments.

;; See `incomplete-function' on how to use it.
;; Just bind TAB to 'incomplete.

;;; Code:

(defcustom incomplete-show-completions-duration 3
  "How many seconds to show the completion buffer."
  :type 'integer
  :group 'emacs)

(defvar incomplete-function nil
  "A function to be called to return possible completions.
Of the returned list, the first element must be the prefix at point.")

;;; Internal variabls:

(defvar incomplete-completion-timer nil
  "Timer to hide the *Completions* buffer again.")

(defvar incomplete-saved-window-configuration nil
  "The last window configuration.")

(defvar incomplete-new-window-configuration nil
  "The window configuration with the completions buffer.")

(defun incomplete ()
  "incomplete the word at point.
This calls all functions in `incomplete-function' for possible
completions."
  (interactive)
  (if (and (eq last-command this-command)
           (incomplete-completion-help-live-p))
      (incomplete-scroll-completion-window)
    (let ((completions (funcall incomplete-function)))
      (incomplete-here (car completions)
                       (cdr completions)))))

(defun incomplete-here (prefix completions)
  "Complete PREFIX at point.
PREFIX should be any kind of string before point.
COMPLETIONS is a list of strings which can be completed."
  (let ((completion (try-completion prefix
                                    ;; Make it an alist, for backwards
                                    ;; compatibility with GNU Emacs 21
                                    ;; and XEmacs.
                                    (mapcar #'list completions))))
    (cond
     ((eq completion t)
      (incomplete-hide-completion-window))
     ((null completion)
      (incomplete-hide-completion-window)
      (message "No completions for `%s'" prefix)
      (ding))
     ((not (string= prefix completion))
      (delete-region (- (point)
                        (length prefix))
                     (point))
      (insert completion)
      (when (incomplete-completion-help-live-p)
        (incomplete-show-completion-window completion completions)))
     (t
      (incomplete-show-completion-window prefix completions)))))

(defun incomplete-completion-help-live-p ()
  "Return non-nil when the *Completions* buffer is visible."
  (let ((window (get-buffer-window "*Completions*")))
    (and window
         (window-live-p window)
         ;; Why the checks below? Copied from lisp-mode...
         (window-buffer window)
         (buffer-name (window-buffer window)))))

(defun incomplete-show-completion-window (prefix completions)
  "Show a completion buffer.
The buffer shows all completions for PREFIX in COMPLETIONS."
  (let ((all-completions (all-completions prefix
                                          ;; Make it an alist, for backwards
                                          ;; compatibility with GNU Emacs 21
                                          ;; and XEmacs.
                                          (mapcar #'list completions))))
    (if (null (cdr all-completions))
        (progn
          (message "Unique completion")
          (incomplete-hide-completion-window))
      (when (not (incomplete-completion-help-live-p))
        (setq incomplete-saved-window-configuration
              (current-window-configuration)))
      (when incomplete-completion-timer
        (cancel-timer incomplete-completion-timer))
      (message "Making completion list")
      (with-output-to-temp-buffer "*Completions*"
        (display-completion-list (sort all-completions #'string<)))
      (message "Making completion list...done")
      (setq incomplete-new-window-configuration
            (current-window-configuration))
      (let ((event (read-event "SPC to hide completion buffer")))
        (if (eq event ? )
            (incomplete-hide-completion-window)
          (incomplete-initialize-timer))
        (setq unread-command-events (list event))))))

(defun incomplete-scroll-completion-window ()
  "Scroll the *Completions* buffer."
  (let ((window (get-buffer-window "*Completions*")))
    (with-current-buffer (window-buffer window)
      (if (pos-visible-in-window-p (point-max) window)
          (set-window-start window (point-min))
        (save-selected-window
          (select-window window)
          (scroll-up))))
    (incomplete-initialize-timer)))

(defun incomplete-hide-completion-window ()
  "Hide the completion window.
This restores a previous window configuration if it exists."
  (when incomplete-completion-timer
    (cancel-timer incomplete-completion-timer)
    (setq incomplete-completion-timer nil))
  (when (and incomplete-new-window-configuration
             incomplete-saved-window-configuration
             (compare-window-configurations
              incomplete-new-window-configuration
              (current-window-configuration)))
    (set-window-configuration incomplete-saved-window-configuration)))

(defun incomplete-initialize-timer ()
  "Initialize the timer to remove the completions window."
  (when incomplete-completion-timer
    (cancel-timer incomplete-completion-timer))
  (setq incomplete-completion-timer
        (run-at-time incomplete-show-completions-duration
                     nil
                     'incomplete-hide-completion-window)))


;;; Compatibility defines

; XEmacs:
;; (defun read-event (prompt)
;;   "Read an event."
;;   ;; incomplete is only interested in spaces
;;   (if (event-matches-key-specifier-p (aref (read-key-sequence "Foo")
;;                                            0)
;;                                      ? )
;;       ?
;;     ?f))

(provide 'incomplete)
;;; incomplete.el ends here
