;;; make-tls-process.el --- A non-blocking TLS connection function

;; Copyright (C) 2015  Jorgen Schaefer <contact@jorgenschaefer.de>

;; Author: Jorgen Schaefer <contact@jorgenschaefer.de>
;; URL: https://github.com/jorgenschaefer/circe

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A `make-tls-process' function like `make-network-process', in
;; particular supporting non-blocking connects.

;;; Code:

(defvar tls-debug-output nil
  "Non-nil if you want to see lots of debug messages.")

(defun tls--debug (format-string &rest args)
  "Display a message if debug output is enabled.

If `tls-debug-output' is non-nil, this acts like `message'.
Otherwise, it's a no-op."
  (when tls-debug-output
    (apply #'message format-string args)))

(defun make-tls-process (&rest args)
  "Create a TLS client process.

A TLS network process is a command process that runs a command
line program like gnutls or openssl, not a full network process.
Network communication should work as usual, but the sentinel
might receive process-specific events.

Different from a process sentinel, but like a network sentinel,
the sentinel is called with an event \"open\\n\" when the
connection is established.

This function does NOT return a process object, as we try
multiple commands to establish a tls connection (see
`tls-command'). Use the sentinel to get notified of when the
connection succeeds.

Arguments are specified as keyword/argument pairs, similar to
`make-network-process'. The following arguments are defined:

:name NAME -- NAME is name for process.  It is modified if necessary
to make it unique.

:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
with the process.  Process output goes at end of that buffer, unless
you specify an output stream or filter function to handle the output.
BUFFER may be also nil, meaning that this process is not associated
with any buffer.

:host HOST -- HOST is name of the host to connect to, or its IP
address.  The symbol `local' specifies the local host.  If specified
for a server process, it must be a valid name or address for the local
host, and only clients connecting to that address will be accepted.

:service SERVICE -- SERVICE is name of the service desired, or an
integer specifying a port number to connect to.  If SERVICE is t,
a random port number is selected for the server.  (If Emacs was
compiled with getaddrinfo, a port number can also be specified as a
string, e.g. "80", as well as an integer.  This is not portable.)

:coding CODING -- If CODING is a symbol, it specifies the coding
system used for both reading and writing for this process.  If CODING
is a cons (DECODING . ENCODING), DECODING is used for reading, and
ENCODING is used for writing.

:noquery BOOL -- Query the user unless BOOL is non-nil, and process is
running when Emacs is exited.

:filter FILTER -- Install FILTER as the process filter.

:sentinel SENTINEL -- Install SENTINEL as the process sentinel.

:plist PLIST -- Install PLIST as the new process's initial plist."
  (let* ((name (plist-get args :name))
         (host (plist-get args :host))
         (service (plist-get args :service))
         (commands tls-program))
    (tls--make-single-process name commands host service args)
    nil))

(defun tls--sentinel (proc event)
  "The default sentinel for TLS connections.

Try the next command in the list, or fail if there are none
left."
  (tls--debug "tls--sentinel %S %S"
              (process-status proc)
              event)
  (tls--debug "Failed TLS output: %s"
              (process-get proc :tls-data))
  (if (eq (process-status proc)
          'exit)
      (let ((commands (process-get proc :tls-remaining-commands)))
        (if (not commands)
            (let ((sentinel (plist-get (process-get proc :tls-args)
                                       :sentinel)))
              (when sentinel
                (funcall sentinel proc "failed\n")))
          (tls--make-single-process (process-get proc :tls-name)
                                     commands
                                     (process-get proc :tls-host)
                                     (process-get proc :tls-service)
                                     (process-get proc :tls-args))))
    (error "Unexpected event in tls sentinel: %S" event)))

(defun tls--filter (proc data)
  "The default filter for TLS connections.

We wait until both `tls-success' and `tls-end-of-info' have been
received. Once that happens, we are done and we can switch over
to the real connection."
  (let ((data (concat (or (process-get proc :tls-data)
                          "")
                      data)))
    (if (and (string-match tls-success data)
             (string-match tls-end-of-info data))
        (let* ((remaining-data (substring data (match-end 0)))
               (args (process-get proc :tls-args))
               (buffer (plist-get args :buffer))
               (coding (plist-get args :coding))
               (noquery (plist-get args :noquery))
               (filter (plist-get args :filter))
               (sentinel (plist-get args :sentinel))
               (plist (plist-get args :plist)))
          (set-process-plist proc plist)
          (set-process-sentinel proc sentinel)
          (set-process-filter proc filter)
          (set-process-buffer proc buffer)
          (if (consp coding)
              (set-process-coding-system proc (car coding) (cdr coding))
            (set-process-coding-system proc coding coding))
          (set-process-query-on-exit-flag proc (not noquery))
          (funcall sentinel proc "open\n")
          (when (not (equal remaining-data ""))
            (funcall filter proc remaining-data)))
      (process-put proc :tls-data data))))

(defun tls--make-single-process (name commands host service args)
  "Open a single process using the first argument of COMMANDS."
  (let ((proc (tls--start-process name (car commands) host service)))
    (process-put proc :tls-name name)
    (process-put proc :tls-remaining-commands (cdr commands))
    (process-put proc :tls-host host)
    (process-put proc :tls-service service)
    (process-put proc :tls-args args)
    (set-process-sentinel proc #'tls--sentinel)
    (set-process-filter proc #'tls--filter)))

(defun tls--start-process (name cmd host port)
  "Start a single process for network communication.

This code is mostly taken from tls.el."
  (let ((process-connection-type tls-process-connection-type)
        (formatted-cmd
         (format-spec
          cmd
          (format-spec-make
           ?h host
           ?p (if (integerp port)
                  (int-to-string port)
                port)))))
    (tls--debug "TLS starting process: %s" formatted-cmd)
    (start-process name nil
                   shell-file-name shell-command-switch
                   formatted-cmd)))

(provide 'make-tls-process)
;;; make-tls-process.el ends here
