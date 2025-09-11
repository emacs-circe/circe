;;; circe-pingmon.el --- PING lag monitor for Circe -*- lexical-binding: t; -*-

;; Copyright (C) Vasilij Schneidermann

;; Author: Vasilij Schneidermann

;; This file is part of Circe.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
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

;; circe-pingmon-mode monitors the amount of lag on your connection to
;; each server by periodically sending a PING with a unique token and
;; awaiting the matching PONG containing the same token. The
;; roundtrip-time is considered the amount of lag. If it exceeds a
;; certain threshold, a reconnect is triggered.

;;; Code:

(require 'circe)

;;; User variables

(defgroup circe-pingmon nil
  "PING lag monitor for Circe"
  :prefix "circe-pingmon-"
  :group 'circe)

(defcustom circe-pingmon-timer-tick 5
  "How often to check for lag.

Increase this to improve performance at the cost of accuracy."
  :type 'number
  :group 'circe-pingmon)

(defcustom circe-pingmon-check-interval 60
  "Interval in seconds at which to send the CTCP message."
  :type 'number
  :group 'circe-pingmon)

(defcustom circe-pingmon-reconnect-interval 120
  "Seconds after which to automatically reconnect upon a timeout
of a lag monitor message. A value of nil disables the feature."
  :type '(choice (const :tag "Disable auto-reconnect" nil)
                 number)
  :group 'circe-pingmon)

(defcustom circe-pingmon-mode-line-format-string "ping:%.2f "
  "Format string for displaying the lag in the mode-line."
  :type 'string
  :group 'circe-pingmon)

(defcustom circe-pingmon-mode-line-unknown-lag-string "ping:? "
  "Indicator string for displaying unknown lag in the mode-line."
  :type 'string
  :group 'circe-pingmon)

(defvar circe-pingmon-disabled nil
  "A boolean value if pingmon should be disabled on this network.

Don't set this by hand, use `circe-network-options'.")
(make-variable-buffer-local 'circe-pingmon-disabled)

;;; Internal variables

(defvar circe-pingmon-timer nil)

(defvar circe-pingmon-token nil)
(make-variable-buffer-local 'circe-pingmon-token)

(defvar circe-pingmon-server-lag nil)
(make-variable-buffer-local 'circe-pingmon-server-lag)

(defvar circe-pingmon-last-send-time nil)
(make-variable-buffer-local 'circe-pingmon-last-send-time)

(defvar circe-pingmon-last-receive-time nil)
(make-variable-buffer-local 'circe-pingmon-last-receive-time)

;;; Actual logic

(defun circe-pingmon-timer-tick ()
  "Function run periodically to check lag.

This will call `circe-pingmon-server-check' in every active server
buffer. You can call it yourself if you like to force an update,
there is no harm in running it too often, but it really should be
run sufficiently often with the timer."
  (dolist (buffer (circe-server-buffers))
   (with-current-buffer buffer
      (when (and (eq major-mode 'circe-server-mode)
                 circe-server-process
                 (eq (irc-connection-state circe-server-process)
                     'registered)
                 (not circe-pingmon-disabled))
        (circe-pingmon-server-check)))))

(defun circe-pingmon-server-check ()
  "Check the current server for lag.

Currently, this will just send a PING with `circe-pingmon-token' if it's
been too long."
  (let ((now (float-time)))
    (cond
     ;; No answer so far
     ((and circe-pingmon-last-send-time
           (not circe-pingmon-last-receive-time))
      ;; Count up until the answer comes.
      (let ((lag (/ (- now circe-pingmon-last-send-time) 2)))
        (when (or (not circe-pingmon-server-lag)
                  (> lag circe-pingmon-server-lag))
          (setq circe-pingmon-server-lag lag)
          (circe-pingmon-force-mode-line-update)))
      ;; Check for timeout.
      (when (and circe-pingmon-reconnect-interval
                 (> now
                    (+ circe-pingmon-last-send-time
                       circe-pingmon-reconnect-interval)))
        (setq circe-pingmon-last-send-time nil
              circe-pingmon-last-receive-time nil)
        (circe-reconnect)))
     ;; Nothing sent so far, or last send was too long ago.
     ((or (not circe-pingmon-last-send-time)
          (> now
             (+ circe-pingmon-last-send-time
                circe-pingmon-check-interval)))
      (irc-send-raw (circe-server-process)
                    (format "PING %s" circe-pingmon-token)
                    :nowait)
      (setq circe-pingmon-last-send-time now
            circe-pingmon-last-receive-time nil))
     )))

(defun circe-pingmon-force-mode-line-update ()
  "Call `force-mode-line-update' on all relevant Circe buffers.
This includes the Circe server buffer and all of its chat buffers."
  (force-mode-line-update)
  (dolist (b (circe-server-chat-buffers))
    (with-current-buffer b
      (force-mode-line-update))))

(defun circe-pingmon-format-mode-line-entry ()
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
         (circe-pingmon-disabled
          nil)
         (circe-pingmon-server-lag
          (format circe-pingmon-mode-line-format-string
                  circe-pingmon-server-lag))
         (t
          circe-pingmon-mode-line-unknown-lag-string))))))

(random t) ; more randomness please

(defun circe-pingmon-generate-token ()
  (secure-hash 'sha256 (format "%s%08x" (buffer-name) (random #x100000000))))

(defun circe-pingmon-init ()
  "Initialize all variables and kick off the timer."
  (setq circe-pingmon-token (circe-pingmon-generate-token)
        circe-pingmon-server-lag nil
        circe-pingmon-last-send-time nil
        circe-pingmon-last-receive-time nil)
  (circe-pingmon-force-mode-line-update)
  (unless circe-pingmon-timer
    (setq circe-pingmon-timer
          (run-at-time nil circe-pingmon-timer-tick
                       'circe-pingmon-timer-tick))))

(defun circe-pingmon--rpl-welcome-handler (conn &rest _ignored)
  (with-current-buffer (irc-connection-get conn :server-buffer)
    (circe-pingmon-init)))

(defun circe-pingmon--PONG-handler (conn _event _sender _server token)
  (with-current-buffer (irc-connection-get conn :server-buffer)
    (when (and token circe-pingmon-token
               (equal token circe-pingmon-token))
      (let* ((now (float-time))
             (lag (/ (- now circe-pingmon-last-send-time) 2)))
        (setq circe-pingmon-server-lag lag
              circe-pingmon-last-receive-time now)
        (circe-pingmon-force-mode-line-update)))))

(define-minor-mode circe-pingmon-mode
  "Circe-pingmon-mode monitors the amount of lag on your
connection to each server, and displays the lag time in seconds in the
mode-line. Unlike `circe-lagmon-mode', it sends out a PING with a tag
repeatedly and awaits the matching PONG. This avoids spamming CTCP
notices in other attached clients, as PING/PONG are not considered
worthy of a notification."
  :global t
  (let ((mode-line-entry '(:eval (circe-pingmon-format-mode-line-entry))))
    (remove-hook 'mode-line-modes mode-line-entry)
    (let ((table (circe-irc-handler-table)))
      (irc-handler-remove table "001" 'circe-pingmon--rpl-welcome-handler)
      (irc-handler-remove table "PONG" 'circe-pingmon--PONG-handler))
    (when circe-pingmon-timer
      (cancel-timer circe-pingmon-timer)
      (setq circe-pingmon-timer nil))
    (when circe-pingmon-mode
      (add-hook 'mode-line-modes mode-line-entry)
      (let ((table (circe-irc-handler-table)))
        (irc-handler-add table "001" 'circe-pingmon--rpl-welcome-handler)
        (irc-handler-add table "PONG" 'circe-pingmon--PONG-handler))
      (dolist (buffer (circe-server-buffers))
        (with-current-buffer buffer
          (setq circe-pingmon-server-lag nil)
          (when (and circe-server-process
                     (eq (irc-connection-state circe-server-process)
                         'registered))
            (circe-pingmon-init)))))))
