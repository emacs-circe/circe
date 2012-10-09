;;; circe-lagmon.el --- Lag Monitor for Circe

;; Copyright (C) 2011  John J Foerch <jjfoerch@earthlink.net>

;; Author: John J Foerch <jjfoerch@earthlink.net>

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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:
;;;
;;;   Circe-lagmon-mode monitors the amount of lag on your connection to
;;; each server, and displays the lag time in seconds in the mode-line.
;;; It works by managing two timers.  Timer1 sends CTCP LAGMON to yourself
;;; on each server every 60 seconds.  Each time around, timer1 starts
;;; timer2 to monitor for timeouts of these messages.  Timer2 cancels
;;; itself when all of the pings in the round have been answered.
;;;

;;; Code:

(require 'circe)


;;; User variables
;;;
(defgroup circe-lagmon nil
  "Lag Monitor for Circe"
  :prefix "circe-lagmon-"
  :group 'circe)

(defcustom circe-lagmon-interval 60
  "Interval in seconds at which to check lag."
  :type 'number
  :group 'circe-lagmon)

(defcustom circe-lagmon-mode-line-format-string "lag:%.4s"
  "Format string for displaying the lag in the mode-line."
  :type 'string
  :group 'circe-lagmon)

(defcustom circe-lagmon-reconnect-timeout 20
  "Seconds after which to automatically attempt reconnect upon a
timeout of a lag monitor message.  A value of nil disables the
feature."
  :type '(choice (const :tag "Disable auto-reconnect" nil)
                 number)
  :group 'circe-lagmon)


;;; Internal variables
;;;
(defvar circe-lagmon-timer nil)

(defvar circe-lagmon-server-lag '\?)
(make-variable-buffer-local 'circe-lagmon-server-lag)

(defvar circe-lagmon-last-check-time nil)
(make-variable-buffer-local 'circe-lagmon-last-check-time)


;;; Utils
;;;
(defun circe-lagmon-force-mode-line-update ()
  "Call force-mode-line-update on a circe server buffer and all
of its chat buffers."
  (force-mode-line-update)
  (circe-mapc-chat-buffers
   (lambda (b)
     (with-current-buffer b
       (force-mode-line-update)))))

(defun circe-lagmon-format-mode-line-entry ()
  "Format the mode-line entry for displaying the lag."
  (cond
    ((eq major-mode 'circe-server-mode)
     (format circe-lagmon-mode-line-format-string circe-lagmon-server-lag))
    (circe-server-buffer
     (with-current-buffer circe-server-buffer
       (format circe-lagmon-mode-line-format-string circe-lagmon-server-lag)))))


;;; Timer functions
;;;
(defun circe-lagmon-timeout-count ()
  "Watch and report timeouts of the CTCP LAGMON messages."
  (let ((lagging nil))
    (mapc
     (lambda (b)
       (with-current-buffer b
         (when (and (eq major-mode 'circe-server-mode)
                    circe-lagmon-last-check-time)
           (setq lagging t)
           (setq circe-lagmon-server-lag
                 (/ (- (float-time) circe-lagmon-last-check-time) 2))
           (circe-lagmon-force-mode-line-update)
           (when (and circe-lagmon-reconnect-timeout
                      (> circe-lagmon-server-lag circe-lagmon-reconnect-timeout))
             (setq circe-lagmon-last-check-time nil)
             (circe-reconnect)))))
     (buffer-list))
    (unless lagging
      (cancel-function-timers 'circe-lagmon-timeout-count))))

(defun circe-lagmon-send-check ()
  "Send CTCP LAGMON to current nick on each open server, bearing
a timestamp."
  (let ((time (float-time))
        (circe-running nil))
    (run-at-time t 2 'circe-lagmon-timeout-count)
    (mapc
     (lambda (b)
       (with-current-buffer b
         (when (eq major-mode 'circe-server-mode)
           (setq circe-running t)
           (circe-server-send (format "PRIVMSG %s :\C-aLAGMON %s\C-a"
                                      circe-server-nick time))
           (unless circe-lagmon-last-check-time
             (setq circe-lagmon-last-check-time time)))))
     (buffer-list))
    (unless circe-running
      (cancel-timer circe-lagmon-timer)
      (setq circe-lagmon-timer nil))))


;;; CTCP LAGMON handlers
;;;
(defun circe-lagmon-ctcp-LAGMON-handler (nick user host command args)
  "Handle CTCP LAGMON, updating the mode-line display, and
cancelling any ongoing timeout count for this server."
  (when (and (string= command "CTCP-LAGMON")
             (equal nick circe-server-nick))
    (let ((lag (/ (- (float-time)
                     (string-to-number (cadr args)))
                  2)))
      (setq circe-lagmon-server-lag lag
            circe-lagmon-last-check-time nil)
      (circe-lagmon-force-mode-line-update))))

(defun circe-lagmon-ctcp-LAGMON-display-handler (nick user host command args)
  "Suppress the default display of the CTCP LAGMON message.  This
can be overridden for debugging."
  '(let ((lag circe-lagmon-server-lag))
    (with-current-buffer (circe-server-last-active-buffer)
      (circe-server-message (format "Lag: %ss" lag)))))


;;; Minor Mode
;;;
(defun circe-lagmon-init ()
  "Initialize the values of the lag monitor for one server, and
start the lag monitor if it has not been started."
  (setq circe-lagmon-server-lag '\?
        circe-lagmon-last-check-time nil)
  (circe-lagmon-force-mode-line-update)
  (unless circe-lagmon-timer
    (setq circe-lagmon-timer
          (run-at-time nil circe-lagmon-interval 'circe-lagmon-send-check))))

;;;###autoload
(define-minor-mode circe-lagmon-mode
    "Circe-lagmon-mode monitors the amount of lag on your
connection to each server, and displays the lag time in seconds
in the mode-line."
  :global t
  (let ((mode-line-entry '(:eval (circe-lagmon-format-mode-line-entry))))
    (remove-hook 'mode-line-modes mode-line-entry)
    (remove-hook 'circe-receive-message-functions 'circe-lagmon-ctcp-LAGMON-handler)
    (circe-set-display-handler "CTCP-LAGMON" nil)
    (when circe-lagmon-timer
      (cancel-timer circe-lagmon-timer)
      (setq circe-lagmon-timer nil))
    (when circe-lagmon-mode
      (add-hook 'mode-line-modes mode-line-entry)
      (add-hook 'circe-receive-message-functions 'circe-lagmon-ctcp-LAGMON-handler)
      (circe-set-display-handler "CTCP-LAGMON" 'circe-lagmon-ctcp-LAGMON-display-handler)
      (mapc
       (lambda (b)
         (with-current-buffer b
           (when (eq major-mode 'circe-server-mode)
             (setq circe-lagmon-server-lag '\?)
             (when circe-server-registered-p
               (circe-lagmon-init)))))
       (buffer-list))
      (add-hook 'circe-server-connected-hook 'circe-lagmon-init))))


(provide 'circe-lagmon)
;;; circe-lagmon.el ends here
