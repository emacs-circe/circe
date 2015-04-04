;;; irc.el --- Library to handle IRC connections

;; Copyright (C) 2015  Jorgen Schaefer <contact@jorgenschaefer.de>

;; Version: 0.5
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

;; The main entry function is `irc-connect'. This creates a new
;; connection to an IRC server, and also takes an event handler table
;; which is used to run various event handlers. Handlers receive a
;; connection object which can be used for other API calls.

;; IRC connection objects also accept connection options. These can be
;; queried using `irc-connection-get', and are set by `irc-connect' or
;; later using `irc-connection-put'.

;; Event handler tables are simple maps of names to functions. See
;; `irc-handler-table', `irc-handler-add' and `irc-handler-run' for
;; the API.

;; To send commands to the server, use `irc-send-raw' or
;; `irc-send-command'.

;; The rest of the library are handler packs that add support for
;; various IRC features.

;;; Code:

(require 'make-tls-process)

(defvar irc-version "0.1"
  "The version of irc.el")

(defcustom irc-debug-log nil
  "Emit protocol debug info if this is non-nil.")

;;;;;;;;;;;;;;;;;;;;;;;
;;; Connection function

(defun irc-connect (&rest keywords)
  "Connect to an IRC server.

Supported keyword arguments:

:name NAME -- The name for the process
:host HOST -- The host to connect to
:service SERVICE -- The service or port to connect to
:tls BOOL -- Whether to use TLS
:family IP-FAMILY -- Force using of ipv4 or ipv6
:handler-table HANDLER -- The event handler table to send events to.

The following events are supported:

conn.connected conn -- The connection was established
conn.failed conn -- The connection could not be established
conn.disconnected conn -- A previously established connection was lost

NNN conn sender args... -- A numeric reply from IRC was received
COMMAND conn sender args... -- An IRC command message was received
irc.ctcp conn sender target verb args... -- A CTCP message was received
irc.ctcp.VERB conn sender target args... -- A CTCP message was received"
  (funcall (if (plist-get keywords :tls)
               #'make-tls-process
             #'make-network-process)
           :name (or (plist-get keywords :name)
                     (plist-get keywords :host))
           :host (or (plist-get keywords :host)
                     (error "Must specify a :host to connect to"))
           :service (or (plist-get keywords :service)
                        (error "Must specify a :service to connect to"))
           :family (plist-get keywords :family)
           :coding '(undecided . utf-8)
           :nowait t
           :noquery t
           :filter #'irc--filter
           :sentinel #'irc--sentinel
           :plist keywords
           :keepalive t)
  ;; Intentional: We do not want to return the process object.
  nil)

(defun irc-connection-get (conn propname)
  "Return the value of CONN's PROPNAME property."
  (process-get conn propname))

(defun irc-connection-put (conn propname value)
  "Change CONN's PROPNAME property to VALUE."
  (process-put conn propname value))

(defun irc--sentinel (proc event)
  (cond
   ((string-match "\\`failed " event)
    (irc-event-emit proc "conn.failed"))
   ((string-match "\\`open" event)
    (irc-event-emit proc "conn.connected"))
   ((string-match "\\`\\(connection broken\\|killed\\|finished\\)"
                  event)
    (irc-event-emit proc "conn.disconnected"))
   ((string-match "\\`deleted" event)
    nil)
   (t
    (error "Unknown event in IRC sentinel: %S" event))))

(defvar irc--filter-running-p nil
  "Non-nil when we're currently processing a message.

Yep, this is a mutex. Why would one need a mutex in Emacs, a
single-threaded application, you ask? Easy!

When, during the execution of a process filter, any piece of code
waits for process output - e.g. because they started a some
external program - Emacs will process any input from external
processes. Including the one for the filter that is currently
running.

If that process does emit output, the filter is run again, while
it is already running. If the filter is not careful, this can
cause data to arrive out of order, or get lost.")

(defun irc--filter (proc data)
  "Handle data from the process."
  (irc-connection-put proc :conn-data
                      (concat (or (irc-connection-get proc :conn-data)
                                  "")
                              data))
  (when (not irc--filter-running-p)
    (let ((irc--filter-running-p t)
          (data (irc-connection-get proc :conn-data)))
      (while (string-match "\\`\\(.*\\)\n\\(\\(:?.\\|\n\\)*\\)\\'" data)
        (let ((line (match-string 1 data)))
          (setq data (match-string 2 data))
          (irc-connection-put proc :conn-data data)
          (irc--handle-line proc line)
          (setq data (irc-connection-get proc :conn-data)))))))

(defun irc--handle-line (proc line)
  "Handle a single line from the IRC server.

The command is simply passed to the event handler of the IRC
connection."
  (irc-debug-out proc "S: %s" line)
  (let* ((parsed (irc--parse line))
         (sender (car parsed))
         (command (cadr parsed))
         (args (cddr parsed)))
    (apply #'irc-event-emit proc command sender args)))

(defun irc--parse (line)
  "Parse a line from IRC.

Returns a list: (sender command args...)

A line from IRC is a space-separated list of arguments. If the
first word starts with a colon, that's the sender. The first or
second word is the command. All further words are arguments. The
first word to start with a colon ends the argument list.

Examples:

COMMAND
COMMAND arg
COMMAND arg1 arg2
COMMAND arg1 arg2 :arg3 still arg3
:sender COMMAND arg1 arg2 :arg3 still arg3"
  (with-temp-buffer
    (insert line)
    (goto-char (point-min))
    (let ((sender nil)
          (args nil))
      (when (looking-at ":\\([^ ]*\\) +")
        (setq sender (match-string 1))
        (goto-char (match-end 0)))
      (while (re-search-forward ":\\(.*\\)\\|\\([^ ]+\\)" nil t)
        (push (or (match-string 1)
                  (match-string 2))
              args))
      (cons sender (nreverse args)))))

(defun irc-event-emit (conn event &rest args)
  "Run the event handlers for EVENT in CONN with ARGS."
  (irc-debug-out conn
                 "E: %S %s"
                 event
                 (mapconcat (lambda (elt) (format "%S" elt))
                            args
                            " "))
  (let ((handler-table (irc-connection-get conn :handler-table)))
    (when handler-table
      (apply #'irc-handler-run handler-table event conn event args)
      (apply #'irc-handler-run handler-table nil conn event args))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Event handler table

(defun irc-handler-table ()
  "Return a new event handler table."
  (make-hash-table :test 'equal))

(defun irc-handler-add (table event handler)
  "Add HANDLER for EVENT to the event handler table TABLE."
  (puthash event
           (append (gethash event table)
                   (list handler))
           table))

(defun irc-handler-run (table event &rest args)
  "Run the handlers for EVENT in TABLE, passing ARGS to each."
  (dolist (handler (gethash event table))
    (if debug-on-error
        (apply handler args)
      (condition-case err
          (apply handler args)
        (error
         (message "Error running event %S handler %S: %s"
                  event handler err))))))

;;;;;;;;;;;
;;; Sending

(defun irc-send-raw (conn line &optional flood-handling)
  "Send a line LINE to the IRC connection CONN.

LINE should not include the trailing newline.

FLOOD-HANDLING defines how to handle the situation when we are
sending too  much data. It can have three values:

nil -- Add the message to a queue and send it later
:nowait -- Send the message immediately, circumventing flood protection
:drop -- Send the message only if we are not flooding, and drop it if
   we have queued up messages.

The flood protection algorithm works like the one detailed in RFC
2813, section 5.8 \"Flood control of clients\".

  * If `flood-last-message' is less than the current
    time, set it equal.
  * While `flood-last-message' is less than `flood-margin'
    seconds ahead of the current time, send a message, and
    increase `flood-last-message' by `flood-penalty'."
  (cond
   ((null flood-handling)
    (irc-connection-put conn
                        :flood-queue
                        (append (irc-connection-get conn :flood-queue)
                                (list line)))
    (irc-send--queue conn))
   ((eq flood-handling :nowait)
    (irc-send--internal conn line))
   ((eq flood-handling :drop)
    (let ((queue (irc-connection-get conn :flood-queue)))
      (when (not queue)
        (irc-connection-put conn :flood-queue (list line))
        (irc-send--queue conn))))))

(defun irc-send--queue (conn)
  "Send messages from the flood queue in CONN.

See `irc-send-raw' for the algorithm."
  (let ((queue (irc-connection-get conn :flood-queue))
        (last-message (or (irc-connection-get conn :flood-last-message)
                          0))
        (margin (or (irc-connection-get conn :flood-margin)
                    10))
        (penalty (or (irc-connection-get conn :flood-penalty)
                     3))
        (now (float-time)))
    (when (< last-message now)
      (setq last-message now))
    (while (and queue
                (< last-message (+ now margin)))
      (irc-send--internal conn (car queue))
      (setq queue (cdr queue)
            last-message (+ last-message penalty)))
    (irc-connection-put conn :flood-queue queue)
    (irc-connection-put conn :flood-last-message last-message)
    (let ((timer (irc-connection-get conn :flood-timer)))
      (when timer
        (cancel-timer timer)
        (irc-connection-put conn :flood-timer nil))
      (when queue
        (irc-connection-put conn
                            :flood-timer
                            (run-at-time 1 nil #'irc-send--queue conn))))))

(defun irc-send--internal (conn line)
  "Send LINE to CONN."
  (irc-debug-out conn "C: %s" line)
  (process-send-string conn (concat line "\n")))

(defun irc-send-command (conn command &rest args)
  "Send COMMAND with ARGS to IRC connection CONN."
  (irc-send-raw conn (apply #'irc--format-command command args)))

(defun irc--format-command (command &rest args)
  "Format COMMAND and ARGS for IRC.

The last value in ARGS will be escaped with a leading colon if it
contains a space. All other arguments are checked to make sure
they do not contain a space."
  (dolist (arg (cons command args))
    (when (not (stringp arg))
      (error "Argument must be a string")))
  (let* ((prefix (cons command (butlast args)))
         (last (last args)))
    (dolist (arg prefix)
      (when (string-match " " arg)
        (error "IRC protocol error: Argument %S must not contain space"
               arg)))
    (when (and last (or (string-match " " (car last))
                        (equal "" (car last))))
      (setcar last (concat ":" (car last))))
    (mapconcat #'identity
               (append prefix last)
               " ")))

(defun irc-send-AUTHENTICATE (conn arg)
  "Send an AUTHENTICATE message."
  (irc-send-command conn "AUTHENTICATE" arg))

;; irc-send-AWAY conn &optional text

(defun irc-send-CAP (conn &rest args)
  "Send a CAP message."
  (apply #'irc-send-command conn "CAP" args))

;; irc-send-INVITE conn nickname channel
;; irc-send-JOIN conn channel-list &optional key-list
;; irc-send-NAMES conn &optional channel-list target

(defun irc-send-NICK (conn nick)
  "Send a NICK message."
  (irc-send-command conn "NICK" nick))

;; irc-send-PART conn channel-list part-message

(defun irc-send-PASS (conn password)
  "Send a PASS message."
  (irc-send-command conn "PASS" password))

(defun irc-send-PONG (conn server &optional server2)
  "Send a PONG message."
  (if server2
      (irc-send-command conn "PONG" server server2)
    (irc-send-command conn "PONG" server)))

;; irc-send-PRIVMSG conn msgtarget text-to-be-sent
;; irc-send-QUIT conn quit-message
;; irc-send-TOPIC conn channel &optional topic

(defun irc-send-USER (conn user mode realname)
  "Send a USER message.

MODE should be an integer as per RFC 2812"
  (irc-send-command conn "USER" user (format "%s" mode) "*" realname))

;; irc-send-WHOIS conn target mask-list
;; irc-send-WHOWAS conn nickname-list &optional count target

;;;;;;;;;;;;;;;
;;; Debug stuff

(defun irc-debug-out (conn fmt &rest args)
  (when irc-debug-log
    (let ((name (format "*IRC Protocol %s:%s*"
                        (irc-connection-get conn :host)
                        (irc-connection-get conn :service))))
      (with-current-buffer (get-buffer-create name)
        (save-excursion
          (goto-char (point-max))
          (insert (apply #'format fmt args) "\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handler: Registration

(defun irc-handle-registration (table)
  "Add command handlers to TABLE to handle registration.

This will send the usual startup messages after we are connected.

Events emitted:

\"irc.registered\" current-nick -- We have successfully
  registered with the IRC server. Most commands can be used now.
  In particular, joining channels is only possible now.

Connection options used:

:nick -- The nick to use to register with the server
:user -- The user name to use
:mode -- The initial mode to use; an integer. See RFC 2812 for
   the meaning.
:realname -- The realname to use for the registration
:pass -- The server password to send
:cap-req -- CAP protocol capabilities to request, if available
:sasl-username -- The SASL username to send, if sasl is available
:sasl-password -- The SASL password to send, if sasl is available

Connection options set:

:connection-state -- One of nil, connected, registered, disconnected
  See `irc-connection-state' for an interface to this.
:cap-supported-p -- Non-nil if the server supports the CAP protocol
:cap-ack -- The list of active capabilities negotiated with the server"
  (irc-handler-add table "conn.connected"
                   #'irc-handle-registration--connected)
  (irc-handler-add table "conn.disconnected"
                   #'irc-handle-registration--disconnected)
  (irc-handler-add table "001" ;; RPL_WELCOME
                   #'irc-handle-registration--rpl-welcome)
  (irc-handler-add table "CAP"
                   #'irc-handle-registration--cap)
  (irc-handler-add table "AUTHENTICATE"
                   #'irc-handle-registration--authenticate))

(defun irc-handle-registration--connected (conn event)
  (irc-connection-put conn :connection-state 'connected)
  (when (irc-connection-get conn :cap-req)
    (irc-send-CAP conn "LS"))
  (let ((password (irc-connection-get conn :pass)))
    (when password
      (irc-send-PASS conn password)))
  (irc-send-NICK conn (irc-connection-get conn :nick))
  (irc-send-USER conn
                 (irc-connection-get conn :user)
                 (irc-connection-get conn :mode)
                 (irc-connection-get conn :realname)))

(defun irc-handle-registration--disconnected (conn event)
  (irc-connection-put conn :connection-state 'disconnected))

(defun irc-handle-registration--rpl-welcome (conn event sender target text)
  (irc-connection-put conn :connection-state 'registered)
  (irc-event-emit conn "irc.registered" target))

(defun irc-handle-registration--cap (conn event sender target subcommand arg)
  (cond
   ((equal subcommand "LS")
    (let ((supported (split-string arg))
          (wanted nil))
      (dolist (cap (irc-connection-get conn :cap-req))
        (when (member cap supported)
          (setq wanted (append wanted (list cap)))))
      (irc-send-CAP conn "REQ" (mapconcat #'identity wanted " "))))
   ((equal subcommand "ACK")
    (let ((acked (split-string arg)))
      (irc-connection-put conn :cap-ack acked)
      (if (and (member "sasl" acked)
               (irc-connection-get conn :sasl-username)
               (irc-connection-get conn :sasl-password))
          (irc-send-AUTHENTICATE conn "PLAIN")
        (irc-send-CAP conn "END"))))
   (t
    (message "Unknown CAP response from server: %s %s" subcommand arg))))

(defun irc-handle-registration--authenticate (conn event sender arg)
  (if (equal arg "+")
      (let ((username (irc-connection-get conn :sasl-username))
            (password (irc-connection-get conn :sasl-password)))
        (irc-send-AUTHENTICATE conn (base64-encode-string
                                     (format "%s\x00%s\x00%s"
                                             username username password)))
        (irc-send-CAP conn "END"))
    (message "Unknown AUTHENTICATE response from server: %s" arg)))

(defun irc-connection-state (conn)
  "connecting connected registered disconnected"
  (let ((state (irc-connection-get conn :connection-state)))
    (if (null state)
        'connecting
      state)))

;;;;;;;;;;;;;;;;;;;;;;
;;; Handler: Ping-Pong

(defun irc-handle-ping-pong (table)
  "Add command handlers to respond to PING requests."
  (irc-handler-add table "PING" #'irc-handle-ping-pong--ping))

(defun irc-handle-ping-pong--ping (conn event sender argument)
  (irc-send-PONG conn argument))

;;;;;;;;;;;;;;;;;;;;;
;;; Handler: ISUPPORT

;; Events aught:
;; - 005 RPL_ISUPPORT => Store isupport info

;; Connection options set:
;; - :isupport

;; irc-isupport conn type => string | t | nil
;; irc-string-equal-p conn s1 s2 => nil|t
;; irc-channel-name-p conn string

;; ISUPPORT message is CHANTYPES=# EXCEPTS INVEX CHANMODES=eIbq,k,flj,....
;; So split at space, then at =, and leave it at that

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handler: Current nick tracking

;; Events caught:
;; - 001 RPL_WELCOME => set our own nick
;; - NICK => set our own nick

;; Connection options set:
;; - :current-nick

;; irc-current-nick conn => current nick
;; irc-current-nick-p conn string

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handler: Initial nick acquisition

;; Events caught:
;; - 432 ERR_ERRONEUSNICKNAME
;; - 437 ERR_UNAVAILRESOURCE
;; - 433 ERR_NICKNAMEINUSE
;;   => set alternate nicks when registering

;; Connection options used:
;; - :nick-alternatives => alternative nicks in case the first is not available
;; - :nick-generator => when alternatives used up, a function to generate
;;   random nicks

;;;;;;;;;;;;;;;;;
;;; Handler: CTCP

(defun irc-handle-ctcp (table)
  "Add command handlers to TABLE to handle the CTCP protocol.

Events emitted:

\"irc.message\" sender target body -- A non-CTCP PRIVMSG
\"irc.notice\" sender target body -- A non-CTCP NOTICE
\"irc.ctcp\" sender target argument -- A CTCP request. ARGUMENT
  can be nil if there was no argument, or the empty string if the
  argument was empty.
\"irc.ctcpreply\" sender target argument -- A CTCP reply.
  ARGUMENT is similar to above."
  (irc-handler-add table "PRIVMSG"
                   #'irc-handle-ctcp--privmsg)
  (irc-handler-add table "NOTICE"
                   #'irc-handle-ctcp--notice))

(defun irc-handle-ctcp--privmsg (conn event sender target body)
  (if (string-match "\\`\x01\\([^ ]+\\)\\(?: \\(.*\\)\\)?\x01\\'"
                    body)
      (irc-event-emit conn "irc.ctcp" sender target
                      (match-string 1 body)
                      (match-string 2 body))
    (irc-event-emit conn "irc.message" sender target body)))

(defun irc-handle-ctcp--notice (conn event sender target body)
  (if (string-match "\\`\x01\\([^ ]+\\)\\(?: \\(.*\\)\\)?\x01\\'"
                    body)
      (irc-event-emit conn "irc.ctcpreply" sender target
                      (match-string 1 body)
                      (match-string 2 body))
    (irc-event-emit conn "irc.notice" sender target body)))

;; Events caught:
;; - irc.ctcp.CLIENTINFO
;; - irc.ctcp.PING
;; - irc.ctcp.SOURCE
;; - irc.ctcp.TIME
;; - irc.ctcp.USERINFO
;; - irc.ctcp.VERSION

;; Events emitted:
;; - irc.ctcp.VERB
;; - irc.ctcpreply.VERB

;; Connection options used:
;; - :ctcp-clientinfo (list of supported CTCP commands)
;; - :ctcp-source
;; - :ctcp-time
;; - :ctcp-userinfo
;; - :ctcp-version

;; irc-send-ctcp conn target verb &optional argument
;; irc-send-ctcp-PING conn target
;; irc-send-ctcp-reply conn target verb &optional argument
;; irc-send-ctcp-reply-PING conn target argument
;; irc-send-ctcp-reply-VERSION conn target argument

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handler: Channel and user tracking

;; Events caught:
;; - JOIN => Remember which channels we are on, and who is on those channels
;; - NICK => Rename users on channels
;; - QUIT => Remove users from channels
;; - PART => Remove user from channels
;; - 353 RPL_NAMREPLY, 366 RPL_ENDOFNAMES => Store users on channel

;; Connection options set:
;; - :irc-channels
;; - :irc-users

;; irc-channel-users conn channel => users
;; irc-user conn nick => user

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handler: Channel topic tracking

;; Events caught:
;; - TOPIC, 331 RPL_NOTOPIC, 332 RPL_TOPIC
;;   => Remember current topic of channels

;; Events emitted:
;; - irc.topic.changed channel old new

;; Options set:
;; - :channel-topics
;;   - Or rather, re-use :irc-channels

;;;;;;;;;;;;;;;;;;;;;;
;;; Handler: Auto-Join

;; Connection options used:
;; - auto-join-after-registration
;; - auto-join-after-host-hiding
;; - auto-join-after-nick-acquisition

;; Events caught:
;; - 001 RPL_WELCOME
;; - 396 RPL_HOSTHIDDEN
;; - NICK => When we regain our preferred nick

;;;;;;;;;;;;;;,;;;;;;
;;; Handler: NickServ

;; Events caught:
;; - PRIVMSG, NOTICE => Handle nickserv messages
;; - irc.registered => Nick not ours? Send ghost message

;; Events emitted:
;; - nickserv.registered
;; - nickserv.failed

;; Connection options used:
;; - :nickserv-*

(provide 'irc)
;;; irc.el ends here
