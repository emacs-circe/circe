;;; circe-lagmon.el --- Lag Monitor for Circe

;; Copyright (C) 2011-2012 Jorgen Schaefer

;; Author: John J Foerch <jjfoerch@earthlink.net>,
;;         Jorgen Schaefer

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

(defcustom circe-lagmon-timer-tick 5
  "How often to check for lag.

Increase this to improve performance at the cost of accuracy."
  :type 'number
  :group 'circe-lagmon)

(defcustom circe-lagmon-check-interval 60
  "Interval in seconds at which to send the CTCP message."
  :type 'number
  :group 'circe-lagmon)

(defcustom circe-lagmon-reconnect-interval 40
  "Seconds after which to automatically reconnect upon a timeout
of a lag monitor message. A value of nil disables the feature."
  :type '(choice (const :tag "Disable auto-reconnect" nil)
                 number)
  :group 'circe-lagmon)

(defcustom circe-lagmon-mode-line-format-string "lag:%.1f "
  "Format string for displaying the lag in the mode-line."
  :type 'string
  :group 'circe-lagmon)

(defcustom circe-lagmon-mode-line-unknown-lag-string "lag:? "
  "Indicator string for displaying unknown lag in the mode-line."
  :type 'string
  :group 'circe-lagmon)

(defvar circe-lagmon-disabled nil
  "A boolean value if lagmon should be disabled on this network.

Don't set this by hand, use `circe-network-options'.")
(make-variable-buffer-local 'circe-lagmon-disabled)


;;; Internal variables
;;;
(defvar circe-lagmon-timer nil)

(defvar circe-lagmon-server-lag nil)
(make-variable-buffer-local 'circe-lagmon-server-lag)

(defvar circe-lagmon-last-send-time nil)
(make-variable-buffer-local 'circe-lagmon-last-send-time)

(defvar circe-lagmon-last-receive-time nil)
(make-variable-buffer-local 'circe-lagmon-last-receive-time)

(defun circe-lagmon-timer-tick ()
  "Function run periodically to check lag.

This will call `circe-lagmon-server-check' in every active server
buffer. You can call it yourself if you like to force an update,
there is no harm in running it too often, but it really should be
run sufficiently often with the timer."
  (dolist (buffer (circe-server-buffers))
    (with-current-buffer buffer
      (when (and (eq major-mode 'circe-server-mode)
                 circe-server-process
                 (eq (irc-connection-state circe-server-process)
                     'registered)
                 (not circe-lagmon-disabled))
        (circe-lagmon-server-check)))))

(defun circe-lagmon-server-check ()
  "Check the current server for lag.

This will reconnect if we haven't heard back for too long, or
send a request if it's time for that. See
`circe-lagmon-reconnect-interval' and
`circe-lagmon-check-interval' to configure the behavior.."
  (let ((now (float-time)))
    (cond
     ;; No answer so far...
     ((and circe-lagmon-last-send-time
           (not circe-lagmon-last-receive-time))
      ;; Count up until the answer comes.
      (let ((lag (/ (- now circe-lagmon-last-send-time) 2)))
        (when (or (not circe-lagmon-server-lag)
                  (> lag circe-lagmon-server-lag))
          (setq circe-lagmon-server-lag lag)
          (circe-lagmon-force-mode-line-update)))
      ;; Check for timeout.
      (when (and circe-lagmon-reconnect-interval
                 (> now
                    (+ circe-lagmon-last-send-time
                       circe-lagmon-reconnect-interval)))
        (setq circe-lagmon-last-send-time nil
              circe-lagmon-last-receive-time nil)
        (circe-reconnect)))
     ;; Nothing sent so far, or last send was too long ago.
     ((or (not circe-lagmon-last-send-time)
          (> now
             (+ circe-lagmon-last-send-time
                circe-lagmon-check-interval)))
      (irc-send-raw (circe-server-process)
                    (format "PRIVMSG %s :\C-aLAGMON %s\C-a"
                            (circe-server-nick) now)
                    :nowait)
      (setq circe-lagmon-last-send-time now
            circe-lagmon-last-receive-time nil))
     )))

(defun circe-lagmon-ctcp-LAGMON-display-handler (nick user host command args)
  "Suppress the default display of the CTCP LAGMON message."
  'ignored)

(defun circe-lagmon-message-handler (nick user host command args)
  "Handle relevant message to lagmon

On a CTCP LAGMON message, store the time and updating the
mode-line display.

On a NICK change for us, reset the timer as well, as might be
missing a CTCP response sent to the old nick."
  (cond
   ((and (string= command "CTCP-LAGMON")
         (circe-server-my-nick-p nick))
    (let* ((now (float-time))
           (lag (/ (- now (string-to-number (cadr args)))
                   2)))
      (setq circe-lagmon-server-lag lag
            circe-lagmon-last-receive-time now)
      (circe-lagmon-force-mode-line-update)))
   ((and (string= command "NICK")
         (circe-server-my-nick-p nick))
    (setq circe-lagmon-last-send-time nil))))

(defun circe-lagmon-force-mode-line-update ()
  "Call force-mode-line-update on a circe server buffer and all
of its chat buffers."
  (force-mode-line-update)
  (dolist (b (circe-chat-buffers))
    (with-current-buffer b
      (force-mode-line-update))))

(defun circe-lagmon-format-mode-line-entry ()
  "Format the mode-line entry for displaying the lag."
  (let ((buf (cond
              ((eq major-mode 'circe-server-mode)
               (current-buffer))
              (circe-server-buffer
               circe-server-buffer)
              (t
               nil))))
    (when buf
      (with-current-buffer buf
        (cond
         (circe-lagmon-disabled
          nil)
         (circe-lagmon-server-lag
          (format circe-lagmon-mode-line-format-string
                  circe-lagmon-server-lag))
         (t
          circe-lagmon-mode-line-unknown-lag-string))))))

(defun circe-lagmon-init ()
  "Initialize the values of the lag monitor for one server, and
start the lag monitor if it has not been started."
  (setq circe-lagmon-server-lag nil
        circe-lagmon-last-send-time nil
        circe-lagmon-last-receive-time nil)
  (circe-lagmon-force-mode-line-update)
  (unless circe-lagmon-timer
    (setq circe-lagmon-timer
          (run-at-time nil circe-lagmon-timer-tick
                       'circe-lagmon-timer-tick))))

;;;###autoload
(define-minor-mode circe-lagmon-mode
  "Circe-lagmon-mode monitors the amount of lag on your
connection to each server, and displays the lag time in seconds
in the mode-line."
  :global t
  (let ((mode-line-entry '(:eval (circe-lagmon-format-mode-line-entry))))
    (remove-hook 'circe-server-connected-hook 'circe-lagmon-init)
    (remove-hook 'mode-line-modes mode-line-entry)
    (remove-hook 'circe-receive-message-functions
                 'circe-lagmon-ctcp-LAGMON-handler)
    (circe-set-display-handler "CTCP-LAGMON" nil)
    (when circe-lagmon-timer
      (cancel-timer circe-lagmon-timer)
      (setq circe-lagmon-timer nil))
    (when circe-lagmon-mode
      (add-hook 'mode-line-modes mode-line-entry)
      (add-hook 'circe-receive-message-functions
                'circe-lagmon-message-handler)
      (circe-set-display-handler "CTCP-LAGMON"
                                 'circe-lagmon-ctcp-LAGMON-display-handler)
      (dolist (buffer (circe-server-buffers))
        (with-current-buffer buffer
          (setq circe-lagmon-server-lag nil)
          (when (and circe-server-process
                     (eq (irc-connection-state circe-server-process)
                         'registered))
            (circe-lagmon-init))))
      (add-hook 'circe-server-connected-hook 'circe-lagmon-init))))

(provide 'circe-lagmon)
;;; circe-lagmon.el ends here
