;; -*- lexical-binding: t -*-

;;; circe-tls.el --- Non-blocking TLS

;; Copyright (C) 2014 defanor

;; Author: defanor <at uberspace.net>

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
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301  USA

;;; Commentary:

;; It's an optionally non-blocking replacement for standard
;; `open-tls-stream', and is more similar to `make-network-process'.

;; tls-success is not correct by default in Emacs 24.3.1 with openssl
;; 1.0.1e 11 Feb 2013, so it should be customized to "- Handshake was
;; completed\|Verify return code".


;;; Code:

(require 'tls)

(defcustom circe-tls-failed (concat "*** Fatal error\\|"
                                    "New, (NONE), Cipher is (NONE)\\|"
                                    "Cannot connect to\\|"
                                    "connect:errno\\|"
                                    "Cannot resolve")
  "Regular expression indicating failed TLS connections.
The default is what GnuTLS's \"gnutls-cli\" or OpenSSL's
\"openssl s_client\" outputs."
  :version "24.3"
  :type 'regexp
  :group 'circe)


(defun circe-tls-make-stream (&rest properties)
  "Open a TLS connection, tries to mimic make-network-process.
Wraps `circe-tls-open-stream' and `open-tls-stream'.

Accepts a plist with keywords.
Required properties are: host, service, success-func.
Other properties: nowait, name, fail-thunk, buffer, coding, filter, sentinel."
  (let* ((host (plist-get properties :host))
         (name (or (plist-get properties :name) host))
         (service (plist-get properties :service))
         (success-func (plist-get properties :success-func))
         (fail-thunk (plist-get properties :fail-thunk))
         (buffer (or (plist-get properties :buffer) (current-buffer)))
         (coding (plist-get properties :coding))
         (filter (plist-get properties :filter))
         (sentinel (plist-get properties :sentinel))
         (query-on-exit-flag (plist-get properties :query-on-exit-flag))
         (nowait (plist-get properties :nowait)))
    (if (not (and host service success-func))
        (message (concat "circe-tls-make-stream: host, "
                         "service and success-func are required"))
      (if (not nowait)
          (open-tls-stream name nil host service)
        (circe-tls-open-stream 
         name
         host
         service
         #'(lambda (process)
             (if (buffer-live-p buffer)
                 ;; our buffer is still there
                 (with-current-buffer buffer
                   (funcall success-func process)
                   (when filter
                     (set-process-filter process filter))
                   (when sentinel
                     (set-process-sentinel process sentinel))
                   (when coding
                     (set-process-coding-system process
                                                coding
                                                coding))
                   (when query-on-exit-flag
                     (set-process-query-on-exit-flag process
                                                     query-on-exit-flag))
                   (when buffer
                     (set-process-buffer process buffer))
                   (when sentinel
                     (funcall sentinel process "open")))
               ;; there's no buffer, kill the process and that's it
               (when process
                 (delete-process process))))
         #'(lambda ()
             ;; process should be dead here,
             ;; so we are just checking if buffer is not
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when sentinel
                   (funcall sentinel buffer "failed"))
                 (when fail-thunk
                   (funcall fail-thunk))))))))))


(defun circe-tls-open-stream (name host port success-func fail-thunk)
  "Tweaked `open-tls-stream', this one is non-blocking.

Arguments: name, host, port, success-func, fail-thunk."
  (message "Opening TLS connection to `%s'..." host)
  (circe-tls-try-different-commands tls-program
                                    name
                                    host
                                    port
                                    success-func
                                    fail-thunk))


(defun circe-tls-try-different-commands 
  (cmds name host port success-func fail-thunk)
  "Tries different commands to connect.

Arguments: list of commands, name, host, port, success-func,
fail-thunk."
  (if cmds
      (circe-tls-try-single-command 
       (car cmds)
       name
       host
       port
       success-func
       (lambda () (circe-tls-try-different-commands 
                   (cdr cmds)
                   name
                   host
                   port
                   success-func
                   fail-thunk)))
    (funcall fail-thunk)))


(defun circe-tls-try-single-command 
  (cmd name host port success-func fail-thunk)
  "Tries to connect with a single command.

Arguments: command, name, host, port, success-func, fail-thunk."
  (let ((process-connection-type tls-process-connection-type)
        (formatted-cmd
         (format-spec
          cmd
          (format-spec-make
           ?h host
           ?p (if (integerp port)
                  (int-to-string port)
                port))))
        process)
    (message "Opening TLS connection with `%s'..." formatted-cmd)
    (setq process (start-process
                   name nil shell-file-name shell-command-switch
                   formatted-cmd))
    (if process
        (set-process-filter process
                            (circe-tls-make-connection-awaiting-filter 
                             host
                             formatted-cmd
                             success-func
                             fail-thunk
                             ""))
      (funcall fail-thunk))))


(defun circe-tls-make-connection-awaiting-filter
  (host formatted-cmd success-func fail-thunk acc)
  "A process filter generator: returned filter waits for success
or failure, and then calls success-func or fail-thunk.

Arguments: host, formatted cmd, success-func, fail-thunk,
accumulator."
  (lambda (process output)
    (if (and process
             (memq (process-status process) '(open run)))
        ;; process is alive
        (let ((out (concat acc output)))
          (if (string-match tls-success out)
              ;; connected
              (if (string-match tls-end-of-info out)
                  ;; and it's end-of-info already
                  (circe-tls-final-check host
                                         formatted-cmd
                                         process
                                         success-func
                                         fail-thunk
                                         out)
                ;; read until end-of-info
                (message (concat "Opening TLS connection with `%s'..."
                                 "skipping information output")
                         formatted-cmd)
                (set-process-filter process
                                    (circe-tls-make-information-skipping-filter
                                     host
                                     formatted-cmd
                                     success-func
                                     fail-thunk
                                     out)))
            ;; no success (yet)
            (if (or (string-match tls-end-of-info out)
                    (string-match circe-tls-failed out))
                ;; failed -> failed; end of info, but not success -> failed
                (progn
                  (message "Opening TLS connection with `%s'... failed"
                           formatted-cmd)
                  (when (and process
                             (memq (process-status process) '(open run)))
                    (delete-process process))
                  (funcall fail-thunk))
              ;; wait more
              (set-process-filter process
                                  (circe-tls-make-connection-awaiting-filter
                                   host
                                   formatted-cmd
                                   success-func
                                   fail-thunk
                                   out)))))
      ;; process is dead
      (when (and process
                 (memq (process-status process) '(open run)))
        (delete-process process))
      (message "Opening TLS connection with `%s'... failed" formatted-cmd)
      (funcall fail-thunk))))

(defun circe-tls-make-information-skipping-filter
  (host formatted-cmd success-func fail-thunk acc)
  "A process filter generator: returned filter just skips
information output after success, but checks for a failure
too (just in case); then calls success-func or fail-thunk.

Arguments: host, formatted cmd, success-func, fail-thunk,
accumulator."
  (lambda (process output)
    (if (and process
             (memq (process-status process) '(open run)))
        (let ((out (concat acc output)))
          (if (string-match tls-end-of-info out)
              (circe-tls-final-check host
                                     formatted-cmd
                                     process
                                     success-func
                                     fail-thunk
                                     out)
            (set-process-filter process
                                (circe-tls-make-information-skipping-filter
                                 host
                                 formatted-cmd
                                 success-func
                                 fail-thunk
                                 out))))
      (when (and process
                 (memq (process-status process) '(open run)))
        (delete-process process))
      (message "Opening TLS connection with `%s'... failed" formatted-cmd)
      (funcall fail-thunk))))


(defun circe-tls-final-check
  (host formatted-cmd process success-func fail-thunk output)
  "A function to verify TLS connection certificate.

Arguments: host, formatted cmd, process, success-func,
fail-thunk, output."
  (if (or
       (and tls-checktrust
            (string-match tls-untrusted output)
            (or
             (and (not (eq tls-checktrust 'ask))
                  (message "The certificate presented by `%s' is NOT trusted."
                           host))
             (not (yes-or-no-p
                   (format (concat "The certificate presented by `%s' is"
                                   " NOT trusted. Accept anyway? ")
                           host)))))
       (and tls-hostmismatch
            (string-match tls-hostmismatch output)
            (not (yes-or-no-p
                  (format (concat "Host name in certificate doesn't match `%s'."
                                  " Connect anyway? ")
                          host)))))
      ;; something is wrong
      (progn
        (message "Opening TLS connection with `%s'... failed" formatted-cmd)
        (set-process-filter process nil)
        (when (and process
                   (memq (process-status process) '(open run)))
          (delete-process process))
        (funcall fail-thunk))
    (message "Opening TLS connection with `%s'... done" formatted-cmd)
    (set-process-filter process nil)
    (funcall success-func process)))

(provide 'circe-tls)
;;; circe-tls.el ends here
