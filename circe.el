;;; circe.el --- Client for IRC in Emacs

;; Copyright (C) 2005  Jorgen Schaefer

;; Version: 0
;; Keywords: IRC, chat
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

;; This is yet another IRC client for Emacs. It is not as small as
;; rcirc, and not as bulky as ERC.

;; It owns a lot to both clients (primarily ERC, which I used for a
;; long time, and helped to develop). Thanks to the authors of both.

;; To use, run M-x circe

;;; Code:

(defvar circe-version "0"
  "Circe version string.")

(require 'lui)
(require 'lui-format)

(defgroup circe nil
  "Yet Another Emacs IRC Client."
  :prefix "circe-"
  :group 'applications)


;;;;;;;;;;;;;
;;; Faces ;;;
;;;;;;;;;;;;;

;; These come before the `defcustom's since they are used for some of
;; the default values.

(defvar circe-prompt-face 'circe-prompt-face
  "The face for the Circe prompt.")
(defface circe-prompt-face
  '((t (:weight bold :foreground "Black" :background "LightSeaGreen")))
  "The face for the Circe prompt."
  :group 'circe)

(defvar circe-server-face 'circe-server-face
  "The face used to highlight server messages.")
(defface circe-server-face
  '((((type tty)) :foreground "blue" :weight bold)
    (t (:foreground "SteelBlue")))
  "The face used to highlight server messages."
  :group 'circe)

(defvar circe-highlight-nick-face 'circe-highlight-nick-face
  "The face used to highlight messages directed to us.")
(defface circe-highlight-nick-face
  '((((type tty)) (:foreground "cyan" :weight bold))
    (t (:foreground "CadetBlue3" :weight bold)))
  "The face used to highlight messages directed to us."
  :group 'circe)

(defvar circe-my-message-face 'circe-my-message-face
  "The face used to highlight our own messages.")
(defface circe-my-message-face '((t))
  "The face used to highlight our own messages."
  :group 'circe)

(defvar circe-originator-face 'circe-originator-face
  "The face used to highlight the originator of a message.")
(defface circe-originator-face '((t))
  "The face used to highlight the originator of a message."
  :group 'circe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization Variables ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom circe-default-nick (user-login-name)
  "*The default nick for circe."
  :type 'string
  :group 'circe)

(defcustom circe-default-user circe-default-nick
  "*The default user name for circe."
  :type 'string
  :group 'circe)

(defcustom circe-default-realname (if (string= (user-full-name) "")
                                      circe-default-nick
                                    (user-full-name))
  "*The default real name for circe."
  :type 'string
  :group 'circe)

(defcustom circe-new-buffer-behavior 'display
  "*How new buffers should be treated.

  'display  - Show them, but don't select them
  'switch   - Switch to that buffer
  'ignore   - Just open it"
  :type '(choice (const :tag "Display" display)
                 (const :tag "Switch" switch)
                 (const :tag "Ignore" ignore))
  :group 'circe)

(defcustom circe-auto-query-p t
  "*Non-nil if queries should be opened automatically."
  :type 'boolean
  :group 'circe)

(defcustom circe-auto-query-max 23
  "*The maximum number of queries which are opened automatically.
If more messages arrive - typically in a flood situation - they
are displayed as if `circe-auto-query-p' was nil."
  :type 'integer
  :group 'circe)

(defcustom circe-prompt-string (concat (propertize ">"
                                                   'face 'circe-prompt-face)
                                       " ")
  "*The string to use for the prompt."
  :type 'string
  :group 'circe)

(defcustom circe-highlight-nick-type 'sender
  "*How to highlight occurances of our own nick.

  'sender    - Highlight the nick of the sender
  'occurance - Highlight the occurances of the nick
  'all       - Highlight the whole line"
  :type '(choice (const :tag "Sender" sender)
                 (const :tag "Occurances" occurance)
                 (const :tag "Whole line" all))
  :group 'circe)

(defcustom circe-ignore-list nil
  "*List of regular expressions to ignore.
Messages from such people are still inserted, but not shown. They
can be displayed using \\[lui-toggle-ignored].

If this ever poses a problem in combination with buffer
truncation, please notify the author."
  :type '(repeat regexp)
  :group 'circe)

(defcustom circe-ignore-functions nil
  "*A list of functions to check whether we should ignore a message.
These functions get five arguments: NICK, USER, HOST, COMMAND, and ARGS.
If one of them returns a non-nil value, the message is ignored."
  :type 'hook
  :group 'circe)

(defcustom circe-split-line-length 440
  "*The maximum length of a single message.
If a message exceeds this size, it is broken into multiple ones.

IRC allows for lines up to 512 bytes. Two of them are CR LF.
And a typical message looks like this:

  :nicky!uhuser@host212223.dialin.fnordisp.net PRIVMSG #lazybastards :Hello!

You can limit here the maximum length of the \"Hello!\" part.
Good luck."
  :type 'integer
  :group 'circe)

(defcustom circe-server-flood-margin 10
  "*A margin on how much excess data we send.
The flood protection algorithm of Circe works like the one
detailed in RFC 2813, section 5.8 \"Flood control of clients\".

  * If `circe-server-flood-last-message' is less than the current
    time, set it equal.
  * While `circe-server-flood-last-message' is less than
    `circe-server-flood-margin' seconds ahead of the current
    time, send a message, and increase
    `circe-server-flood-last-message' by
    `circe-server-flood-penalty' for each message."
  :type 'integer
  :group 'circe)

(defcustom circe-server-flood-penalty 3
  "How much we penalize a message.
See `circe-server-flood-margin' for an explanation of the flood
protection algorithm."
  :type 'integer
  :group 'circe)

(defcustom circe-show-server-modes-p nil
  "Whether Circe should show server modes.
This is disabled by default, since server mode changes are almost
always channel modes after a split."
  :type 'boolean
  :group 'circe)

(defcustom circe-server-max-reconnect-attempts 5
  "How often Circe should attempt to reconnect to the server.
If this is 0, Circe will not reconnect at all. If this is nil,
it will try to reconnect forever (not recommended)."
  :type 'integer
  :group 'circe)

(defcustom circe-server-coding-system nil
  "*Coding systems to use for IRC.
This is either a coding system, which is then used both for
encoding and decoding, or a cons cell with the encoding in the
car and the decoding coding system in the cdr."
  :type '(cons (coding-system :tag "Encoding")
               (coding-system :tag "Decoding"))
  :group 'circe)

(defcustom circe-netsplit-delay 60
  "*The number of seconds a netsplit may be dormant.
If anything happens with a netsplit after this amount of time,
the user is re-notified."
  :type 'number
  :group 'circe)

(defcustom circe-nick-next-function 'circe-nick-next
  "*A function that maps a nick to a new nick.
This is used when the initial nicks are not used. The default
just appends dashes as long as possible, and then generates
random nicks."
  :type 'function
  :group 'circe)

(defcustom circe-receive-message-functions nil
  "*Functions called when a message from the IRC server arrives.
Each function is called with 5 arguments: NICK, USER, HOST,
COMMAND, and ARGS."
  :type 'hook
  :group 'circe)

(defcustom circe-server-connected-hook nil
  "*Hook run when we successfully connected to a server.
This is run from a 001 (RPL_WELCOME) message handler."
  :type 'hook
  :group 'circe)

(defcustom circe-server-mode-hook nil
  "*Hook run when circe connects to a server."
  :type 'hook
  :group 'circe)

(defcustom circe-track-faces-priorities '(circe-highlight-nick-face
                                          circe-my-message-face
                                          circe-server-face)
  "A list of faces which should show up in the tracking.
The first face is kept if the new message has only lower faces,
or faces that don't show up at all."
  :type '(repeat face)
  :group 'circe)

;;;;;;;;;;;;;;;
;;; Formats ;;;
;;;;;;;;;;;;;;;

(defgroup circe-format nil
  "Format strings for Circe.
All these formats always allow the {mynick} and {target} format
strings."
  :prefix "circe-format-"
  :group 'circe)

(defcustom circe-format-not-tracked '(circe-format-server-message
                                      circe-format-server-notice
                                      circe-format-server-numeric)
  "*A list of formats that should not trigger tracking."
  :type '(repeat symbol)
  :group 'circe-format)

(defcustom circe-format-server-message "*** {body}"
  "*The format for generic server messages.
{body} - The body of the message."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-self-say "> {body}"
  "*The format for messages to queries or channels.
{body} - The body of the message."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-self-action "* {mynick} {body}"
  "*The format for actions to queries or channels.
{body} - The body of the action."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-self-message "-> *{target}* {body}"
  "*The format for messages sent to other people outside of queries.
{target} - The target nick.
{body} - The body of the message."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-action "* {nick} {body}"
  "*The format for actions in queries or channels.
{nick} - The nick doing the action.
{body} - The body of the action."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-message-action "* *{nick}* {body}"
  "*The format for actions in messages outside of queries.
{nick} - The nick doing the action.
{body} - The body of the action."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-say "<{nick}> {body}"
  "*The format for normal channel or query talk.
{nick} - The nick talking.
{body} - The message."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-message "*{nick}* {body}"
  "*The format for a message outside of a query.
{nick} - The originator.
{body} - The message."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-notice "-{nick}- {body}"
  "*The format for a notice.
{nick} - The originator.
{body} - The notice."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-server-notice "-Server Notice- {body}"
  "*The format for a server notice.
{body} - The notice."
  :type 'string
  :group 'circe-format)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private variables ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar circe-server-name nil
  "The name of the server we're currently connected to.")
(make-variable-buffer-local 'circe-server-name)

(defvar circe-server-service nil
  "The service name or port of the server we're currently connected to.")
(make-variable-buffer-local 'circe-server-service)

(defvar circe-server-network nil
  "The network name of the server we're currently connected to.")
(make-variable-buffer-local 'circe-server-network)

(defvar circe-server-nick nil
  "Our current nick.")
(make-variable-buffer-local 'circe-server-nick)

(defvar circe-server-user nil
  "The current user name.")
(make-variable-buffer-local 'circe-server-user)

(defvar circe-server-realname nil
  "The current real name.")
(make-variable-buffer-local 'circe-server-realname)

(defvar circe-server-pass nil
  "The password for the current server.
This is required for reconnecting.")
(make-variable-buffer-local 'circe-server-pass)

(defvar circe-server-process nil
  "The process of the server connection.")
(make-variable-buffer-local 'circe-server-process)

(defvar circe-server-registered-p nil
  "Non-nil when we have registered with the server.")
(make-variable-buffer-local 'circe-server-registered-p)

(defvar circe-server-last-active-buffer nil
  "The last active circe buffer.")
(make-variable-buffer-local 'circe-server-last-active-buffer)

(defvar circe-server-chat-buffers nil
  "A hash of chat buffers associated with this server.")
(make-variable-buffer-local 'circe-server-chat-buffers)

(defvar circe-server-filter-data nil
  "The data that arrived from the server
but has not been processed yet.")
(make-variable-buffer-local 'circe-server-filter-data)

(defvar circe-server-processing-p nil
  "Non-nil when we're currently processing a message.
Yep, this is a mutex. Why would one need a mutex in Emacs, a
single-threaded application, you ask? Easy!

When Circe receives a private message, it sets up a new buffer
for this query. This involves calling the LUI setup functions.
These in turn, though, do start flyspell. This involves starting
an external process, in which case Emacs will wait - and when it
waits, it does accept other stuff from, say, network exceptions.
So, if someone sends you two messages quickly after each other,
ispell is started for the first, but might take long enough for
the second message to be processed first. Nice, isn't it.")
(make-variable-buffer-local 'circe-server-processing-p)

(defvar circe-display-table nil
  "A hash table mapping commands to their display functions.")

;;;;;;;;;;;;;;;;;;;
;;; Server Mode ;;;
;;;;;;;;;;;;;;;;;;;

(defun circe-server-mode ()
  "The mode for circe server buffers.
Don't use this function directly, use `circe' instead."
  (lui-mode)
  (make-local-variable 'lui-pre-output-hook)
  (add-hook 'lui-pre-output-hook 'circe-highlight-nick)
  (setq major-mode 'circe-server-mode
        mode-name "Circe Server"
        lui-input-function 'circe-chat-input)
  (lui-set-prompt circe-prompt-string)
  (goto-char (point-max))
  (setq circe-server-last-active-buffer (current-buffer))
  ;; Tab completion should be case-insensitive
  (set (make-local-variable 'completion-ignore-case)
       t)
  (add-hook 'kill-buffer-hook
            'circe-buffer-killed)
  (run-hooks 'circe-server-mode-hook))

(defun circe (host service &optional network pass nick user realname)
  "Connect to the IRC server HOST at SERVICE.
NETWORK is the shorthand used for indicating where we're connected
to. (defaults to HOST)
PASS is the password.
NICK is the nick name to use (defaults to `circe-default-nick')
USER is the user name to use (defaults to `circe-default-user')
REALNAME is the real name to use (defaults to `circe-default-realname')"
  (interactive "sHost: \nsPort: ")
  (let* ((buffer-name (format "%s:%s" host service))
         (server-buffer (get-buffer-create buffer-name)))
    (with-current-buffer server-buffer
      (circe-server-mode)
      (setq circe-server-name host
            circe-server-service service
            circe-server-network (or network
                                     host)
            circe-server-nick (or nick
                                  circe-default-nick)
            circe-server-user (or user
                                  circe-default-user)
            circe-server-realname (or realname
                                      circe-default-realname)
            circe-server-pass pass)
      (circe-reconnect))
    (when (interactive-p)
      (switch-to-buffer server-buffer))))

(defvar circe-server-buffer nil
  "The buffer of the server associated with the current chat buffer.")
(make-variable-buffer-local 'circe-server-buffer)

(defmacro with-circe-server-buffer (&rest body)
  "Run BODY with the current buffer being the current server buffer."
  `(let ((XXserver (cond
                    ((eq major-mode 'circe-server-mode)
                     (current-buffer))
                    (circe-server-buffer
                     circe-server-buffer)
                    (t
                     (error "`with-circe-server-buffer' outside of an circe buffer.")))))
     (when (and XXserver ;; Might be dead!
                (bufferp XXserver)
                (buffer-live-p XXserver))
       (with-current-buffer XXserver
         ,@body))))
(put 'with-circe-server-buffer 'lisp-indent-function 0)

(defmacro with-circe-chat-buffer (name &rest body)
  "Run BODY with the current buffer the chat buffer of NAME.
If no such buffer exists, do nothing."
  `(let ((XXbuf (circe-server-get-chat-buffer ,name)))
     (when XXbuf
       (with-current-buffer XXbuf
         ,@body))))
(put 'with-circe-chat-buffer 'lisp-indent-function 1)

(defvar circe-server-reconnect-attempts 0
  "The number of reconnect attempts that Circe has done so far.
See `circe-server-max-reconnect-attempts'.")
(make-variable-buffer-local 'circe-server-reconnect-attempts)

(defun circe-reconnect ()
  "Reconnect the current server."
  (interactive)
  (with-circe-server-buffer
    (when (or (interactive-p) ;; Always reconnect if the user wants it
              (not circe-server-max-reconnect-attempts)
              (< circe-server-reconnect-attempts
                 circe-server-max-reconnect-attempts))
      (setq circe-server-reconnect-attempts (+ circe-server-reconnect-attempts
                                               1))
      (when circe-server-process
        (delete-process circe-server-process))
      (setq circe-server-registered-p nil
            circe-server-process (open-network-stream circe-server-name
                                                      (current-buffer)
                                                      circe-server-name
                                                      circe-server-service))
      (set-process-filter circe-server-process
                          #'circe-server-filter-function)
      (set-process-sentinel circe-server-process
                            #'circe-server-sentinel)
      (set-process-coding-system circe-server-process
                                 (if (consp circe-server-coding-system)
                                     (cdr circe-server-coding-system)
                                   circe-server-coding-system)
                                 (if (consp circe-server-coding-system)
                                     (car circe-server-coding-system)
                                   circe-server-coding-system))
      (when circe-server-pass
        (circe-server-send (format "PASS %s" circe-server-pass)))
      (circe-server-send (format "NICK %s" circe-server-nick))
      (circe-server-send (format "USER %s 8 * :%s"
                                 circe-server-user
                                 circe-server-realname)))))

(defun circe-server-filter-function (process string)
  "The process filter for the circe server."
  (with-current-buffer (process-buffer process)
    ;; First of all, if we use 'undecided as a coding system for
    ;; processes, this seems to change the coding system after Emacs
    ;; decided the first time what to use. So, we, uh, just set it
    ;; again.
    (set-process-coding-system process
                               (if (consp circe-server-coding-system)
                                   (cdr circe-server-coding-system)
                                 circe-server-coding-system)
                               (if (consp circe-server-coding-system)
                                   (car circe-server-coding-system)
                                 circe-server-coding-system))

    ;; If you think this is written in a weird way - please refer to the
    ;; docstring of `circe-server-processing-p'
    (if circe-server-processing-p
        (setq circe-server-filter-data
              (if circe-server-filter-data
                  (concat circe-server-filter-data string)
                string))
      ;; This will be true even if another process is spawned!
      (let ((circe-server-processing-p t))
        (setq circe-server-filter-data (if circe-server-filter-data
                                           (concat circe-server-filter-data
                                                   string)
                                         string))
        (while (and circe-server-filter-data
                    (string-match "[\n\r]+" circe-server-filter-data))
          (let ((line (substring circe-server-filter-data
                                 0 (match-beginning 0))))
            (setq circe-server-filter-data
                  (if (= (match-end 0)
                         (length circe-server-filter-data))
                      nil
                    (substring circe-server-filter-data
                               (match-end 0))))
            (circe-server-handler line)))))))

(defun circe-server-sentinel (process event)
  "The process sentinel for the server."
  (with-current-buffer (process-buffer process)
    (setq circe-server-registered-p nil)
    (circe-mapc-chat-buffers
     (lambda (buf)
       (with-current-buffer buf
         (circe-chat-disconnected))))
    (when (not (string-match "^deleted" event)) ; Buffer kill
      (circe-reconnect))))

(defvar circe-server-flood-last-message 0
  "When we sent the last message.
See `circe-server-flood-margin' for an explanation of the flood
protection algorithm.")
(make-variable-buffer-local 'circe-server-flood-last-message)

(defvar circe-server-flood-queue nil
  "The queue of messages waiting to be sent to the server.
See `circe-server-flood-margin' for an explanation of the flood
protection algorithm.")
(make-variable-buffer-local 'circe-server-flood-queue)

(defvar circe-server-flood-timer nil
  "The timer to resume sending.")
(make-variable-buffer-local 'circe-server-flood-timer)

(defun circe-server-send (string &optional forcep)
  "Send STRING to the current server.
If FORCEP is non-nil, no flood protection is done - the string is
sent directly. This might cause the messages to arrive in a wrong
order.

See `circe-server-flood-margin' for an explanation of the flood
protection algorithm."
  (with-circe-server-buffer
    (let ((str (concat string "\r\n")))
      (if forcep
          (progn
            (setq circe-server-flood-last-message
                  (+ circe-server-flood-penalty
                     circe-server-flood-last-message))
            (process-send-string circe-server-process str))
        (setq circe-server-flood-queue (append circe-server-flood-queue
                                               (list str)))
        (circe-server-send-queue (current-buffer))))))

(defun circe-server-send-queue (buffer)
  "Send messages in `circe-server-flood-queue'.
See `circe-server-flood-margin' for an explanation of the flood
protection algorithm."
  (with-current-buffer buffer
    (let ((now (float-time)))
      (when circe-server-flood-timer
        (cancel-timer circe-server-flood-timer)
        (setq circe-server-flood-timer nil))
      (when (< circe-server-flood-last-message
               now)
        (setq circe-server-flood-last-message now))
      (while (and circe-server-flood-queue
                  (< circe-server-flood-last-message
                     (+ now circe-server-flood-margin)))
        (let ((msg (car circe-server-flood-queue)))
          (setq circe-server-flood-queue (cdr circe-server-flood-queue)
                circe-server-flood-last-message
                (+ circe-server-flood-last-message
                   circe-server-flood-penalty))
          (process-send-string circe-server-process msg)))
      (when circe-server-flood-queue
        (setq circe-server-flood-timer
              (run-at-time (+ 0.2 circe-server-flood-penalty) ; So we get a free spot
                           nil #'circe-server-send-queue buffer))))))

(defun circe-buffer-killed ()
  "The current buffer is being killed. Do the necessary bookkeeping for circe."
  (cond
   ((eq major-mode 'circe-channel-mode)
    (circe-channel-killed))
   ((eq major-mode 'circe-query-mode)
    (circe-query-killed))
   ((eq major-mode 'circe-server-mode)
    (unwind-protect
        (circe-server-send "QUIT :Server buffer killed")
      t))
    ;; This is done by the sentinel:
    ;;(circe-mapc-chat-buffers
    ;; (lambda (buf)
    ;;   (with-current-buffer buf
    ;;     (circe-chat-disconnected))))
    ))

(defun circe-server-last-active-buffer ()
  "Return the last active buffer of this server."
  (with-circe-server-buffer
    (if (and circe-server-last-active-buffer
             (bufferp circe-server-last-active-buffer)
             (buffer-live-p circe-server-last-active-buffer))
        circe-server-last-active-buffer
      (current-buffer))))

;; Yes, this does not work for the RFC 1459 encoding, but that is
;; being phased out. Hopefully. And problems should be rare.
(defun circe-server-my-nick-p (nick)
  "Return non-nil when NICK is our current nick."
  (with-circe-server-buffer
    (eq t
        (compare-strings nick nil nil
                         circe-server-nick nil nil
                         t))))

(defun circe-server-set-my-nick (newnick)
  "Set our current nick to NEWNICK."
  (with-circe-server-buffer
    (setq circe-server-nick newnick)))

(defun circe-server-nick ()
  "Return our current nick."
  (with-circe-server-buffer
    circe-server-nick))

(defun circe-nick-next (oldnick)
  "Generate a new nick from OLDNICK."
  (if (< (length oldnick) 9)
      (concat oldnick "-")
    (mapconcat (lambda (_)
                 (make-string 1 (+ ?a (random 26))))
               '(1 2 3 4 5 6 7 8 9)
               "")))

(defun circe-server-message (message)
  "Display MESSAGE as a server message."
  (circe-display 'circe-format-server-message
                 :body (propertize message
                                   'face 'circe-server-face)))

(defun circe-display (format &rest keywords)
  "Display FORMAT formatted with KEYWORDS in the current Circe buffer.
See `lui-format' for a description of the format.

If FORMAT contains the word server, the resulting string receives
a `circe-server-face'. If FORMAT contains the word self, the
whole string receives a `circe-my-message-face'. If FORMAT is in
`circe-format-not-tracked', a message of this type is never
tracked by Lui.

Keywords with the name :nick receive a `circe-originator-face'.

It is always possible to use the mynick or target formats."
  (let* ((name (symbol-name format))
         (face (cond
                ((string-match "\\<server\\>" name)
                 'circe-server-face)
                ((string-match "\\<self\\>" name)
                 'circe-my-message-face)))
         (keywords (append `(:mynick ,(circe-server-nick)
                             :target ,circe-chat-target)
                           (circe-display-add-nick-property
                            (if (and (not (null keywords))
                                     (null (cdr keywords)))
                                (car keywords)
                              keywords))))
         (text (lui-format format keywords)))
    (lui-insert (if face
                    (propertize text 'face face)
                  text)
                (memq format circe-format-not-tracked))))

(defun circe-display-add-nick-property (keywords)
  "Add a face to the value of the :nick property in KEYWORDS."
  (let ((keyword nil))
    (mapcar (lambda (entry)
              (cond
               ((or (eq keyword :nick)
                    (eq keyword 'nick))
                (setq keyword nil)
                (propertize entry 'face 'circe-originator-face))
               (t
                (setq keyword entry)
                entry)))
            keywords)))

;;; There really ought to be a hook for this!
(defadvice select-window (after circe-server-track-select-window
                                (window &optional norecord))
  "Remember the current buffer as the last active buffer.
This is used by Circe to know where to put spurious messages."
  (with-current-buffer (window-buffer window)
    (when (or (eq major-mode 'circe-channel-mode)
              (eq major-mode 'circe-query-mode)
              (eq major-mode 'circe-server-mode))
      (let ((buf (current-buffer)))
        (with-circe-server-buffer
          (setq circe-server-last-active-buffer buf))))))
(ad-activate 'select-window)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nick Highlighting ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun circe-highlight-nick ()
  "Highlight the nick of the user in the buffer."
  (let ((body (text-property-any (point-min) (point-max)
                                 'lui-format-argument 'body)))
    (when body
      (goto-char body)
      (cond
       ((eq circe-highlight-nick-type 'sender)
        (when (search-forward (circe-server-nick) nil t)
          (circe-highlight-extend-properties
           (point-min) (point-max)
           'face 'circe-originator-face
           '(face circe-highlight-nick-face))))
       ((eq circe-highlight-nick-type 'occurance)
        (while (search-forward (circe-server-nick) nil t)
          (add-text-properties (match-beginning 0)
                               (match-end 0)
                               '(face circe-highlight-nick-face))))
       ((eq circe-highlight-nick-type 'all)
        (when (search-forward (circe-server-nick) nil t)
          (add-text-properties (point-min)
                               (point-max)
                               '(face circe-highlight-nick-face))))))))

(defun circe-highlight-extend-properties (from to prop val properties)
  "Extend property PROP with value VAL with PROPERTIES between FROM and TO.
Between FROM and TO, on every region of text which has the
property PROP set to VAL, add PROPERTIES."
  (let ((beg (text-property-any from to prop val)))
    (while beg
      (let ((end (next-single-property-change beg prop)))
        (add-text-properties beg end properties)
        (setq beg (text-property-any end to prop val))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chat Buffer Manager ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Case-insensitive hash table
;; This does not work for the RFC 1459 encoding.
(defun circe-case-fold-string= (a b)
  "Compare two strings case-insensitively."
  (eq t
      (compare-strings a nil nil b nil nil t)))

(defun circe-case-fold-string-hash (a)
  "Return a hash value for the string A."
  (sxhash (upcase a)))

(define-hash-table-test 'circe-case-fold
  'circe-case-fold-string=
  'circe-case-fold-string-hash)

(defun circe-case-fold-table ()
  "Return a new hash table for chat buffers."
  (make-hash-table :test 'circe-case-fold))

;; Manager interface

(defun circe-server-add-chat-buffer (target buf)
  "Add BUF as a chat buffer for TARGET."
  (let ((name (if (bufferp buf)
                  buf
                (get-buffer buf))))
    (with-circe-server-buffer
      (when (not circe-server-chat-buffers)
        (setq circe-server-chat-buffers (circe-case-fold-table)))
      (puthash target name circe-server-chat-buffers))))

(defun circe-server-remove-chat-buffer (target)
  "Remove TARGET from the chat buffers."
  (with-circe-server-buffer
    (when circe-server-chat-buffers
      (remhash target circe-server-chat-buffers))))

(defun circe-server-get-chat-buffer (target &optional create)
  "Return the chat buffer associated with TARGET.
If CREATE is non-nil, it is a function which is used to
initialize a new buffer if none exists."
  (with-circe-server-buffer
    (let ((entry (when circe-server-chat-buffers
                   (gethash target circe-server-chat-buffers))))
      (cond
       (entry
        entry)
       (create
        (let ((buf (generate-new-buffer target))
              (server (current-buffer)))
          (circe-server-add-chat-buffer target buf)
          (with-current-buffer buf
            (funcall create target server))
          (cond
           ((eq circe-new-buffer-behavior 'display)
            (display-buffer buf))
           ((eq circe-new-buffer-behavior 'switch)
            (switch-to-buffer buf)))
          buf))
       (t
        nil)))))

(defun circe-mapc-chat-buffers (fun)
  "Apply FUN to every chat buffer of the current server."
  (let ((hash (with-circe-server-buffer
                circe-server-chat-buffers)))
    (when hash
      (maphash (lambda (XXkey XXvalue) ;; XX to prevent accidental capturing
                 (funcall fun XXvalue))
               hash))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Ignore Handling ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun circe-ignored-p (nick user host command args)
  (or (run-hook-with-args-until-success circe-ignore-functions
                                        nick user host
                                        command args)
      (let ((string (concat nick "!" user "@" host))
            (target (when (and (string= command "PRIVMSG")
                               (not (circe-server-my-nick-p (car args)))
                               (string-match "^\\([^ ]*\\)[:, ]" (cadr args)))
                      (match-string 1 (cadr args)))))
        (catch 'return
          (mapc (lambda (regex)
                  (when (string-match regex string)
                    (throw 'return t))
                  (when (and (stringp target)
                             (string-match regex target))
                    (throw 'return t)))
                circe-ignore-list)
          nil))))

(defun circe-command-IGNORE (line)
  "Add the regex on LINE to the `circe-ignore-list'."
  (with-current-buffer (circe-server-last-active-buffer)
    (cond
     ((string-match "\\S-+" line)
      (let ((regex (match-string 0 line)))
        (add-to-list 'circe-ignore-list regex)
        (circe-server-message (format "Ignore list, meet %s"
                                      regex))))
     ((not circe-ignore-list)
      (circe-server-message "Your ignore list is empty"))
     (t
      (circe-server-message "Your ignore list:")
      (mapc (lambda (regex)
              (circe-server-message (format "- %s" regex)))
            circe-ignore-list)))))

(defun circe-command-UNIGNORE (line)
  "Remove an entry from `circe-ignore-list'."
  (with-current-buffer (circe-server-last-active-buffer)
    (cond
     ((string-match "\\S-+" line)
      (let ((regex (match-string 0 line)))
        (setq circe-ignore-list (delete regex circe-ignore-list))
        (circe-server-message (format "Ignore list forgot about %s"
                                      regex))))
     (t
      (circe-server-message
       "Who do you want to unignore? UNIGNORE requires one argument")))))


;;;;;;;;;;;;;;;;;;;;
;;; Chat Buffers ;;;
;;;;;;;;;;;;;;;;;;;;

(defvar circe-chat-target nil
  "The current target for the buffer.
This is either a channel or a nick name.")
(make-variable-buffer-local 'circe-chat-target)

(defvar circe-chat-mode-hook nil
  "The hook run after `circe-chat-mode' is initialized.")

(defun circe-chat-mode (target server-buffer)
  "The circe chat major mode.
This is the common mode used for both queries and channels.
It should not be used directly.
TARGET is the default target to send data to.
SERVER-BUFFER is the server-buffer of this chat buffer."
  (lui-mode)
  (make-local-variable 'lui-pre-output-hook)
  (add-hook 'lui-pre-output-hook 'circe-highlight-nick)
  (setq major-mode 'circe-chat-mode
        mode-name "Circe Chat"
        lui-input-function 'circe-chat-input
        circe-chat-target target
        circe-server-buffer server-buffer)
  (set (make-local-variable 'lui-track-faces-priorities)
       circe-track-faces-priorities)
  ;; Tab completion should be case-insensitive
  (set (make-local-variable 'completion-ignore-case)
       t)
  (lui-set-prompt circe-prompt-string)
  (goto-char (point-max))
  (let ((identifier (with-circe-server-buffer
                      circe-server-network)))
    (make-local-variable 'mode-line-buffer-identification)
    (setq mode-line-buffer-identification
          (list (format "%%b@%-8s" identifier))))
  (run-hooks 'circe-chat-mode-hook))

(defun circe-chat-disconnected ()
  "The current buffer got disconnected."
  (circe-server-message "Disconnected"))

(defun circe-chat-input (str)
  "Process STR as input."
  (cond
   ((string= str "")
    nil)
   ((string-match "\\`/\\([^ ]*\\) ?\\([^\n]*\\)\\'" str)
    (let* ((command (match-string 1 str))
           (args (match-string 2 str))
           (handler (intern-soft (format "circe-command-%s"
                                         (upcase command)))))
      (if handler
          (funcall handler args)
        (circe-server-message (format "Unknown command: %s"
                                      command)))))
   (t
    (mapc #'circe-command-SAY (split-string str "\n")))))

;;;;;;;;;;;;;;;;
;;; Channels ;;;
;;;;;;;;;;;;;;;;

(defvar circe-channel-mode-hook nil
  "Hook run when channel mode is activated.")

(defvar circe-channel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'circe-command-NAMES)
    (define-key map (kbd "C-c C-t") 'circe-command-CHTOPIC)
    map)
  "The key map for channel mode buffers.")

(defun circe-channel-mode (target server-buffer)
  "The circe channel chat major mode.
It should not be used directly.
TARGET is the default target to send data to.
SERVER-BUFFER is the server-buffer of this chat buffer."
  (circe-chat-mode target server-buffer)
  (setq major-mode 'circe-channel-mode
        mode-name "Circe Channel")
  (use-local-map circe-channel-mode-map)
  (set-keymap-parent circe-channel-mode-map
                     lui-mode-map)
  (set (make-local-variable 'lui-completion-function)
       'circe-channel-completions)
  (run-hooks 'circe-channel-mode-hook))

(defun circe-channel-killed ()
  "Called when the channel buffer got killed."
  (when (buffer-live-p circe-server-buffer)
    (unwind-protect
        (circe-server-send (format "PART %s :Channel buffer killed"
                                   circe-chat-target))
      t)
    (circe-server-remove-chat-buffer circe-chat-target)))

(defvar circe-channel-users nil
  "A hash table of channel users.")
(make-variable-buffer-local 'circe-channel-users)

(defvar circe-channel-receiving-names-p nil
  "Non-nil when we're currently receving a NAMES list.")
(make-variable-buffer-local 'circe-server-receiving-names-p)

(defvar circe-channel-nick-prefixes "@+%"
  "The list of nick prefixes this server knows about.
From 005 RPL_ISUPPORT.")
(make-variable-buffer-local 'circe-server-nick-prefixes)

(defun circe-channel-completions (bolp)
  "Return a list of possible completions for the current buffer.
This is used for `lui-completion-function' in channel buffers."
  (when circe-channel-users
    (let ((nicks '()))
      (maphash (lambda (nick ignored)
                 (setq nicks (cons (concat nick (if bolp
                                                    ": "
                                                  " "))
                                   nicks)))
               circe-channel-users)
      (if bolp
          (append (circe-commands-list)
                  nicks)
        nicks))))

(defun circe-channel-message-handler (nick user host command args)
  "Update the users of a channel as appropriate."
  (cond
   ((string= command "NICK")
    (circe-mapc-chat-buffers
     (lambda (buf)
       (with-current-buffer buf
         (when (and (eq major-mode 'circe-channel-mode)
                    (circe-channel-user-p nick))
           (circe-channel-remove-user nick)
           (circe-channel-add-user (car args)))
         (when (and (eq major-mode 'circe-query-mode)
                    (circe-case-fold-string= nick
                                             circe-chat-target))
           (setq circe-chat-target (car args)))))))
   ((string= command "QUIT")
    (circe-mapc-chat-buffers
     (lambda (buf)
       (with-current-buffer buf
         (when (and (eq major-mode 'circe-channel-mode)
                    (circe-channel-user-p nick))
           (circe-channel-remove-user nick))))))
   ((string= command "JOIN")
    (with-circe-chat-buffer (car args)
      (circe-channel-add-user nick)))
   ((string= command "PART")
    (with-circe-chat-buffer (car args)
      (circe-channel-remove-user nick)))
   ((string= command "KICK")
    (with-circe-chat-buffer (car args)
      (circe-channel-remove-user (cadr args))))
   ((string= command "005")             ; RPL_ISUPPORT
    (catch 'exit
      (mapc (lambda (setting)
              (when (string-match "PREFIX=([^)]*)\\(.*\\)" setting)
                (setq circe-channel-nick-prefixes (match-string 1 setting))))
            (cdr args))))
   ((string= command "353")             ; RPL_NAMREPLY
    (with-circe-chat-buffer (nth 2 args)
      (when (not circe-channel-receiving-names-p)
        (setq circe-channel-users nil
              circe-channel-receiving-names-p t))
      (mapc #'circe-channel-add-user
            (circe-channel-parse-names (nth 3 args)))))
   ((string= command "366")             ; RPL_ENDOFNAMES
    (setq circe-channel-receiving-names-p nil))
   ))

;;; User management

(defun circe-channel-add-user (user)
  "Add USER as a channel user."
  (when (not circe-channel-users)
    (setq circe-channel-users (circe-case-fold-table)))
  (puthash user user circe-channel-users))

(defun circe-channel-remove-user (user)
  "Remove USER as a chanel user."
  (when circe-channel-users
    (remhash user circe-channel-users)))

(defun circe-channel-user-p (user)
  "Return non-nil when USER is a channel user."
  (cond
   (circe-channel-users
    (gethash user circe-channel-users))
   ((eq major-mode 'circe-query-mode)
    (circe-case-fold-string= user circe-chat-target))))

(defun circe-channel-parse-names (name-string)
  "Parse the NAMES reply in NAME-STRING.
This uses `circe-channel-nick-prefixes'."
  (delete ""
          (split-string name-string
                        (format "\\(^\\| \\)[%s]*" circe-channel-nick-prefixes))))

(defun circe-mapc-user-channels (user fun)
  "Return a list of channels for USER."
  ;; ;; The trivial implementation doesn't work due to Emacs' dynamic
  ;; ;; scoping. Please wait while I puke outside.
  ;; (circe-mapc-chat-buffers
  ;;  (lambda (XXbuf) ; XX to prevent accidental capturing
  ;;    (when (with-current-buffer XXbuf
  ;;            (circe-channel-user-p user))
  ;;      (funcall fun XXbuf)))))
  (let ((hash (with-circe-server-buffer
                circe-server-chat-buffers)))
    (when hash
      (maphash (lambda (XXignored XXbuf)
                 (when (with-current-buffer XXbuf
                         (circe-channel-user-p user))
                   (funcall fun XXbuf)))
               hash))))
(put 'circe-mapc-user-channels 'lisp-indent-function 1)

;;;;;;;;;;;;;;;
;;; Queries ;;;
;;;;;;;;;;;;;;;

(defvar circe-query-mode-hook nil
  "Hook run when query mode is activated.")

(defun circe-query-mode (target server-buffer)
  "The circe query chat major mode.
It should not be used directly.
TARGET is the default target to send data to.
SERVER-BUFFER is the server-buffer of this chat buffer."
  (circe-chat-mode target server-buffer)
  (setq major-mode 'circe-query-mode
        mode-name "Circe Query")
  (set (make-local-variable 'lui-completion-function)
       'circe-query-completions)
  (run-hooks 'circe-query-mode-hook))

(defun circe-query-killed ()
  "Called when the query buffer got killed."
  (when (buffer-live-p circe-server-buffer)
    (circe-server-remove-chat-buffer circe-chat-target)))

(defun circe-query-completions (bolp)
  "Return a list of possible completions in a query.
That is, commands, our nick, and the other nick.
This is used for `lui-completion-function' in query buffers."
  (append (list (circe-server-nick)
                circe-chat-target)
          (circe-commands-list)))

(defun circe-server-auto-query-buffer (who)
  "Return a buffer for a query with `WHO'.
This adheres to `circe-auto-query-p' and `circe-auto-query-max'."
  (or (circe-server-get-chat-buffer who)
      (when (and circe-auto-query-p
                 (< (circe-server-query-count)
                    circe-auto-query-max))
        (circe-server-get-chat-buffer who 'circe-query-mode))))

(defun circe-server-query-count ()
  "Return the number of queries on the current server."
  (let ((num 0))
    (circe-mapc-chat-buffers
     (lambda (buf)
       (with-current-buffer buf
         (when (eq major-mode 'circe-query-mode)
           (setq num (+ num 1))))))
    num))

;;;;;;;;;;;;;;;;
;;; Commands ;;;
;;;;;;;;;;;;;;;;

(defun circe-commands-list ()
  "Return a list of possible Circe commands."
  (mapcar (lambda (symbol)
            (let ((str (symbol-name symbol)))
              (if (string-match "^circe-command-\\(.*\\)" str)
                  (concat "/" (match-string 1 str) " ")
                str)))
          (apropos-internal "^circe-command-")))

(defun circe-command-SAY (line)
  "Say LINE to the current target."
  (interactive "sSay: ")
  (if (not circe-chat-target)
      (circe-server-message "No target for current buffer")
    (mapc (lambda (line)
            (circe-display 'circe-format-self-say
                           :body line)
            (circe-server-send (format "PRIVMSG %s :%s"
                                       circe-chat-target
                                       line)))
          (circe-split-line line))))

(defun circe-split-line (longline)
  "Return a list of lines which are not too long for poor IRC.
The length is specified in `circe-split-line-length'."
  (if (< (length longline)
         circe-split-line-length)
      (list longline)
    (with-temp-buffer
      (insert longline)
      (let ((fill-column circe-split-line-length))
        (fill-region (point-min) (point-max)
                     nil t))
      (split-string (buffer-string) "\n"))))

(defun circe-command-ME (line)
  "Make LINE as an action"
  (interactive "s* forcer ")
  (if (not circe-chat-target)
      (circe-server-message "No target for current buffer")
    (circe-display 'circe-format-self-action
                   :body line)
    (circe-server-send (format "PRIVMSG %s :\C-aACTION %s\C-a"
                               circe-chat-target line))))

(defun circe-command-MSG (who &optional what)
  "Send a message.
If WHAT is given, WHO is the nick. Else, WHO contains both the
nick and the message."
  (when (not what)
    (if (string-match "^\\([^ ]*\\) \\(.*\\)" who)
        (setq what (match-string 2 who)
              who (match-string 1 who))
      (circe-server-message "Usage: /MSG <who> <what>")))
  (when what
    (let ((buf (circe-server-auto-query-buffer who)))
      (if buf
          (with-current-buffer buf
            (circe-command-SAY what))
        (with-current-buffer (circe-server-last-active-buffer)
          (circe-server-send (format "PRIVMSG %s :%s" who what))
          (circe-display 'circe-format-self-message
                         :target who
                         :body what))))))

(defun circe-command-QUERY (who)
  "Open a query with WHO."
  (interactive "sQuery with: ")
  (let* ((who (when (string-match "[^ ]+" who)
                (match-string 0 who)))
         (circe-new-buffer-behavior 'ignore) ; We do this manually
         (buf (circe-server-get-chat-buffer who 'circe-query-mode))
         (window (get-buffer-window buf)))
    (if window
        (select-window window)
      (switch-to-buffer buf))))

(defun circe-command-JOIN (channel)
  "Join CHANNEL. This can also contain a key."
  (interactive "sChannel: ")
  (circe-server-send (format "JOIN %s" channel)))

(defun circe-command-PART (reason)
  "Part the current channel because of REASON."
  (interactive "sReason: ")
  (if (not circe-chat-target)
      (circe-server-message "No target for current buffer")
    (circe-server-send (format "PART %s :%s"
                               circe-chat-target
                               reason))))

(defun circe-command-WHOIS (whom)
  "Request WHOIS information about WHOM."
  (interactive "sWhois: ")
  (circe-server-send (format "WHOIS %s" whom)))

(defun circe-command-WHOWAS (whom)
  "Request WHOWAS information about WHOM."
  (interactive "sWhois: ")
  (circe-server-send (format "WHOWAS %s" whom)))

(defun circe-command-WHOAMI (&optional ignored)
  "Request WHOIS information about yourself."
  (interactive "sWhois: ")
  (circe-server-send (format "WHOIS %s" (circe-server-nick))))

(defun circe-command-NICK (newnick)
  "Change nick to NEWNICK."
  (interactive "sNew nick: ")
  (circe-server-send (format "NICK %s" newnick)))

(defun circe-command-NAMES (&optional channel)
  "List the names of the current channel or CHANNEL."
  (interactive)
  (if (not circe-chat-target)
      (circe-server-message "No target for current buffer")
    (circe-server-send (format "NAMES %s"
                               (if (and channel
                                        (string-match "[^ ]" channel))
                                   channel
                                 circe-chat-target)))))

(defun circe-command-PING (target)
  "Send a CTCP PING request to TARGET."
  (interactive "sWho: ")
  (circe-server-send (format "PRIVMSG %s :\C-aPING %s\C-a"
                             target
                             (float-time))))

(defun circe-command-QUOTE (line)
  "Send LINE verbatim to the server."
  (interactive "Line: ")
  (circe-server-send line)
  (with-current-buffer (circe-server-last-active-buffer)
    (circe-server-message (format "Sent to server: %s"
                                  line))))

(defun circe-command-CTCP (who &optional command argument)
  "Send a CTCP message COMMAND to WHO, with ARGUMENT.
If COMMAND is not given, WHO is parsed to contain all of who,
command and argument."
  (when (not command)
    (if (string-match "^\\([^ ]*\\) \\([^ ]*\\) ?\\(.*\\)" who)
        (setq command (match-string 2 who)
              argument (match-string 3 who)
              who (match-string 1 who))
      (circe-server-message "Usage: /CTCP <who> <what>")))
  (when command
    (with-current-buffer (or (circe-server-get-chat-buffer who)
                             (circe-server-last-active-buffer))
      (circe-server-send (format "PRIVMSG %s :\C-a%s%s%s\C-a"
                                 who
                                 command
                                 (if argument
                                     " "
                                   "")
                                 (or argument
                                     ""))))))

(defun circe-command-SV (&optional ignored)
  "Tell the current channel about your client and Emacs version."
  (interactive)
  (circe-command-SAY (format (concat "I'm using Circe version %s "
                                     "with %s %s (of %s)")
                             circe-version
                             (if (featurep 'xemacs)
                                 "XEmacs" ; I have no idea how
                               "GNU Emacs")
                             emacs-version
                             (format-time-string "%Y-%m-%d"
                                                 emacs-build-time))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display Handlers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun circe-set-display-handler (command function)
  "Store FUNCTION as the display function for COMMAND.
This uses `circe-display-table'."
  (when (not circe-display-table)
    (setq circe-display-table (make-hash-table :test 'equal)))
  (puthash command function circe-display-table))

(defun circe-display-handler (command)
  "Return the display function for COMMAND.
This uses `circe-display-table'."
  (when circe-display-table
    (gethash command circe-display-table)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Message Handlers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun circe-server-handler (line)
  "Handle LINE from the server."
  (let* ((parsed (circe-server-parse-line line))
         (nick (aref parsed 0))
         (user (aref parsed 1))
         (host (aref parsed 2))
         (command (aref parsed 3))
         (args (aref parsed 4))
         (ignoredp (circe-ignored-p nick user host command args)))
    (if (and (or (string= command "PRIVMSG")
                 (string= command "NOTICE"))
             (string-match "^\C-a\\([^ ]*\\)\\( \\(.*\\)\\)?\C-a$"
                           (cadr args)))
        (when (not ignoredp)
          (let ((ctcp (match-string 1 (cadr args)))
                (arg (match-string 3 (cadr args))))
            (circe-server-ctcp-handler nick user host
                                       (string= command "PRIVMSG")
                                       ctcp
                                       (car args)
                                       (or arg
                                           ""))))
      (unwind-protect
          (progn
            (run-hook-with-args 'circe-receive-message-functions
                                nick user host command args)
            (when (not ignoredp)
              (circe-server-display nick user host command args)))
        (circe-server-internal-handler nick user host command args)))))

(defun circe-server-display (nick user host command args)
  "Display the message."
  (when (not (circe-ignored-p nick user host command args))
    (let ((display (circe-display-handler command)))
      (if display
          (funcall display nick user host command args)
        (or (circe-server-default-display-command nick user host
                                                  command args)
            (with-current-buffer (circe-server-last-active-buffer)
              (circe-server-message
               (format "[%s from %s%s] %s"
                       command
                       nick
                       (if (or user host)
                           (format " (%s@%s)" user host)
                         "")
                       (mapconcat #'identity
                                  args
                                  " ")))))))))

(defun circe-server-ctcp-handler (nick user host requestp ctcp target text)
  "Handle a CTCP message."
  (let ((command (concat "CTCP-" ctcp (if requestp
                                          ""
                                        "-REPLY")))
        (args (list target text)))
    (run-hook-with-args 'circe-receive-message-functions
                        nick user host command args)
    (let ((display (circe-display-handler command)))
      (if display
          (funcall display nick user host command args)
        (with-current-buffer (circe-server-last-active-buffer)
          (if requestp
              (circe-server-message
               (format "Unknown CTCP request %s from %s (%s@%s): %s"
                       ctcp
                       nick user host
                       text))
            (circe-server-message (format "CTCP %s reply from %s (%s@%s): %s"
                                          ctcp nick user host text))))))))

(defun circe-server-parse-line (line)
  "Parse an IRC line.
This returns a vector with five elements: The nick, user, host,
command, and args of the message."
  (let ((nick nil)
        (user nil)
        (host nil)
        (command nil)
        (args nil))
    (with-temp-buffer
      (insert line)
      (goto-char (point-min))
      (cond
       ((looking-at "^:\\([^! ]*\\)!\\([^@ ]*\\)@\\([^ ]*\\) ")
        (setq nick (match-string 1)
              user (match-string 2)
              host (match-string 3))
        (replace-match ""))
       ((looking-at "^:\\([^ ]*\\) ")
        (setq nick (match-string 1))
        (replace-match "")))
      (when (looking-at "[^ ]*")
        (setq command (match-string 0))
        (replace-match ""))
      (if (re-search-forward " :\\(.*\\)" nil t)
          (progn
            (setq args (list (match-string 1)))
            (replace-match ""))
        (goto-char (point-max)))
      (while (re-search-backward " " nil t)
        (setq args (cons (buffer-substring (+ 1 (point))
                                           (point-max))
                         args))
        (delete-region (point) (point-max))))
    (vector nick user host command args)))

(defun circe-server-internal-handler (nick user host command args)
  "Handle this message for internal bookkeeping."
  (cond
   ;; Stay connected. Priority reply.
   ((string= command "PING")
    (circe-server-send (format "PONG %s" (car args))
                       t))
   ;; Remember my nick
   ((and (string= command "NICK")
         (circe-server-my-nick-p nick))
    (circe-server-set-my-nick (car args)))
   ;; Quitting
   ((string= command "QUIT")
    (when (circe-server-my-nick-p nick)
      (circe-mapc-chat-buffers
       (lambda (buf)
         (with-current-buffer buf
           (circe-chat-disconnected))))))
   ;; Create new channel buffers
   ((string= command "JOIN")
    (when (circe-server-my-nick-p nick)
      (circe-server-add-chat-buffer
       (car args)
       (circe-server-get-chat-buffer (car args)
                                     'circe-channel-mode))))
   ;; Initialization
   ((string= command "001")             ; RPL_WELCOME
    (circe-server-set-my-nick (car args))
    (setq circe-server-registered-p t
          circe-server-reconnect-attempts 0)
    (run-hooks 'circe-server-connected-hook))
   ;; If we didn't get our nick yet...
   ((and (not circe-server-registered-p)
         (or (string= command "433")   ; ERR_NICKNAMEINUSE
             (string= command "437"))) ; ERRL_UNAVAILRESOURCE
    (circe-server-send (format "NICK %s" (funcall circe-nick-next-function
                                                  (cadr args))))))
  (circe-channel-message-handler nick user host command args)
  )

;;;;;;;;;;;;;;;;;;;;;;
;;; CTCP Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;

(circe-set-display-handler "CTCP-ACTION" 'circe-ctcp-display-ACTION)
(defun circe-ctcp-display-ACTION (nick user host command args)
  "Show an ACTION."
  (if (circe-server-my-nick-p (car args)) ; Query
      (let ((buf (circe-server-auto-query-buffer nick)))
        (if buf
            (with-current-buffer buf
              (circe-display 'circe-format-action
                             :nick nick
                             :body (cadr args)))
          (with-current-buffer (circe-server-last-active-buffer)
            (circe-display 'circe-format-message-action
                           :nick nick
                           :body (cadr args)))))
    (with-current-buffer (circe-server-get-chat-buffer (car args)
                                                       'circe-channel-mode)
      (circe-display 'circe-format-action
                     :nick nick
                     :body (cadr args)))))

(add-hook 'circe-receive-message-functions 'circe-ctcp-VERSION-handler)
(defun circe-ctcp-VERSION-handler (nick user host command args)
  "Handle a CTCP VERSION request."
  (when (string= command "CTCP-VERSION")
    (circe-server-send
     (format "NOTICE %s :\C-aVERSION Circe: Client for IRC in Emacs, version %s\C-a"
             nick circe-version))))

(circe-set-display-handler "CTCP-VERSION" 'circe-ctcp-display-VERSION)
(defun circe-ctcp-display-VERSION (nick user host command args)
  "Show a CTCP VERSION."
  (with-current-buffer (circe-server-last-active-buffer)
    (if (circe-server-my-nick-p (car args))
        (circe-server-message (format "CTCP VERSION request from %s (%s@%s)"
                                      nick user host))
      (circe-server-message
       (format "CTCP VERSION request from %s (%s@%s) to %s"
               nick user host (car args))))))

(add-hook 'circe-receive-message-functions 'circe-ctcp-PING-handler)
(defun circe-ctcp-PING-handler (nick user host command args)
  "Handle a CTCP PING request."
  (when (string= command "CTCP-PING")
    (circe-server-send (format "NOTICE %s :\C-aPING %s\C-a"
                               nick (cadr args)))))

(circe-set-display-handler "CTCP-PING" 'circe-ctcp-display-PING)
(defun circe-ctcp-display-PING (nick user host command args)
  "Show a CTCP PING request."
  (with-current-buffer (circe-server-last-active-buffer)
    (circe-server-message (format "CTCP PING request%s from %s (%s@%s): %s%s"
                                  (if (circe-server-my-nick-p (car args))
                                      ""
                                    (format " to %s" (car args)))
                                  nick user host
                                  (cadr args)
                                  (let ((number (string-to-number
                                                 (cadr args))))
                                    (if number
                                        (format " (%.2f seconds ago)"
                                                (- (float-time)
                                                   number))
                                      ""))))))

(circe-set-display-handler "CTCP-PING-REPLY" 'circe-ctcp-display-PING-reply)
(defun circe-ctcp-display-PING-reply (nick user host command args)
  "Show a CTCP PING reply."
  (with-current-buffer (circe-server-last-active-buffer)
    (let ((ping-time (string-to-number (cadr args))))
      (if ping-time
          (circe-server-message
           (format (concat "CTCP PING reply from %s (%s@%s):"
                           " %.2f seconds")
                   nick user host
                   (- (float-time)
                      ping-time)))
        (circe-server-message (format (concat "CTCP PING reply (unparseable)"
                                              " from %s (%s@%s): %s"
                                              nick user host
                                              (cadr args))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(circe-set-display-handler "PING" 'circe-display-PING)
(defun circe-display-PING (nick user host command args)
  "(Don't) Show a PING message."
  t)

(circe-set-display-handler "PRIVMSG" 'circe-display-PRIVMSG)
(defun circe-display-PRIVMSG (nick user host command args)
  "Show a PRIVMSG message."
  (cond
   ((circe-server-my-nick-p (car args)) ; Query
    (let ((buf (circe-server-auto-query-buffer nick)))
      (if buf
          (with-current-buffer buf
            (circe-display 'circe-format-say
                           :nick nick
                           :body (cadr args)))
        (with-current-buffer (circe-server-last-active-buffer)
          (circe-display 'circe-format-message
                         :nick nick
                         :body (cadr args))))))
   (t                                   ; Channel talk
    (with-current-buffer (circe-server-get-chat-buffer (car args)
                                                       'circe-channel-mode)
      (circe-display 'circe-format-say
                     :nick nick
                     :body (cadr args))))))

(circe-set-display-handler "NOTICE" 'circe-display-NOTICE)
(defun circe-display-NOTICE (nick user host command args)
  "Show a NOTICE message."
  (let ((queryp (circe-server-my-nick-p (car args))))
    (if nick
        (with-current-buffer (or (circe-server-get-chat-buffer (if queryp
                                                                   nick
                                                                 (car args)))
                                 (circe-server-last-active-buffer))
          (circe-display 'circe-format-notice
                         :nick nick
                         :body (cadr args)))
      (with-circe-server-buffer
        (circe-display 'circe-format-server-notice
                       :body (cadr args))))))

(circe-set-display-handler "NICK" 'circe-display-NICK)
(defun circe-display-NICK (nick user host command args)
  "Show a NICK message."
  (circe-mapc-user-channels nick
    (lambda (buf)
      (with-current-buffer buf
        (circe-server-message
         (format "Nick change: %s (%s@%s) is now known as %s"
                 nick user host (car args)))))))

(circe-set-display-handler "MODE" 'circe-display-MODE)
(defun circe-display-MODE (nick user host command args)
  "Show a MODE message."
  (when (or circe-show-server-modes-p
            user) ; If this is set, it is not a server mode
    (with-current-buffer (circe-server-get-chat-buffer (car args))
      (circe-server-message
       (format "Mode change: %s by %s%s"
               (mapconcat #'identity (cdr args) " ")
               nick
               (if user
                   (format " (%s@%s)"
                           user
                           (or host
                               "(unknown)"))
                 ""))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Netsplit Handling ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar circe-netsplit-list nil
  "A list of recorded netsplits.
Every item is a list with four elements:
- The quit message for this split.
- The time when last we heard about a join in this split
- The time when last we heard about a quit in this split
- A hash table noting which nicks did leave")
(make-variable-buffer-local 'circe-netsplit-list)

(defun circe-netsplit-reason-p (reason)
  "Return non-nil if REASON is the quit message of a netsplit.
This is true when it contains exactly two hosts, with a single
space in between them. The hosts must include at least one dot,
and must not include colons or slashes (else they might be
URLs). (Thanks to irssi for this criteria list)"
  (if (string-match "^[^ :/]+\\.[^ :/]* [^ :/]+\\.[^ :/]*$"
                    reason)
      t
    nil))

(defun circe-netsplit-join (nick)
  "Search for NICK in the netsplit lists.
This either returns a pair whose car is the quit message of this
split, and the cadr the time we last heard anything of the split
of that user. If the NICK isn't split, this returns nil."
  (with-circe-server-buffer
    (catch 'return
      (mapc (lambda (entry)
              (let ((table (nth 3 entry)))
                (when (gethash nick table)
                  (let ((name (nth 0 entry))
                        (time (nth 1 entry)))
                    (remhash nick table)
                    (when (= 0 (hash-table-count table))
                      (setq circe-netsplit-list
                            (delq entry circe-netsplit-list)))
                    (setcar (cdr entry)
                            (float-time))
                    (throw 'return (list name time))))))
            circe-netsplit-list)
      nil)))

(defun circe-netsplit-quit (reason nick)
  "Add NICK as splitted when REASON indicates a netsplit.
This either returns the time when last we heard about this split,
or nil when this isn't a split."
  (when (circe-netsplit-reason-p reason)
    (with-circe-server-buffer
      (let ((entry (assoc reason circe-netsplit-list)))
        (if entry
            (let ((time (nth 2 entry))
                  (table (nth 3 entry)))
              (setcar (cddr entry)
                      (float-time))
              (puthash nick nick table)
              time)
          ;; New split!
          (let ((table (circe-case-fold-table)))
            (puthash nick nick table)
            (setq circe-netsplit-list
                  (cons (list reason 0 (float-time) table)
                        circe-netsplit-list))
            0))))))

(circe-set-display-handler "QUIT" 'circe-display-QUIT)
(defun circe-display-QUIT (nick user host command args)
  "Show a QUIT message."
  (let* ((split (circe-netsplit-quit (car args)
                                     nick))
         (message (if split
                      (if (< (+ split circe-netsplit-delay)
                             (float-time))
                          (format "Netsplit: %s (Use /WL to see who left)"
                                  (car args))
                        nil)
                    (format "Quit: %s (%s@%s) has left IRC: %s"
                            nick user host (car args)))))
    (when message
      (circe-mapc-user-channels nick
        (lambda (buf)
          (with-current-buffer buf
            (circe-server-message message)))))))

(circe-set-display-handler "JOIN" 'circe-display-JOIN)
(defun circe-display-JOIN (nick user host command args)
  "Show a JOIN message."
  (let* ((split (circe-netsplit-join nick))
         (message (if split
                      (if (< (+ (cadr split) circe-netsplit-delay)
                             (float-time))
                          (format "Netmerge: %s (Use /WL to see who's still missing)"
                                  (car split))
                        nil)
                    (format "Join: %s (%s@%s) is now on the channel"
                            nick user host))))
    (when message
      (with-current-buffer (circe-server-get-chat-buffer (car args)
                                                         'circe-channel-mode)
        (circe-server-message message)))))

(defun circe-command-WL (&optional split)
  "Show the people who left in a netsplit.
Without any arguments, shows shows the current netsplits and how
many people are missing. With an argument, which must be a
number, it shows the missing people due to that split."
  (let ((circe-netsplit-list (with-circe-server-buffer
                               circe-netsplit-list)))
    (if (or (not split)
            (and (stringp split)
                 (string= split "")))
        (if (null circe-netsplit-list)
            (circe-server-message "No net split at the moment")
          (let ((n 0))
            (mapc (lambda (entry)
                    (circe-server-message (format "(%d) Missing %d people due to %s"
                                                  n
                                                  (hash-table-count (nth 3 entry))
                                                  (car entry)))
                    (setq n (+ n 1)))
                  circe-netsplit-list)))
      (let* ((index (if (numberp split)
                        split
                      (string-to-number split)))
             (entry (nth index circe-netsplit-list)))
        (if (not entry)
            (circe-server-message (format "No split number %s - use /WL to see a list"
                                          split))
          (circe-server-message (format "Missing people due to %s:"
                                        (car entry)))
          (maphash (lambda (key value)
                     (circe-server-message
                      (format "- %s" value)))
                   (nth 3 entry)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default Formatting ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom circe-format-strings
  '(("PART" 0 "Part: {origin}: {1}")
    ("TOPIC" 0 "Topic change: {origin}: {1}")
    ("INVITE" active "Invite: {origin} invites you to {1}")
    ("KICK" 0 "Kick: {1} kicked by {origin}: {2}")
    ("ERROR" active "Error: {0-}")
    ("001" active "{1}")
    ("002" active "{1}")
    ("003" active "{1}")
    ("004" active "{1-}")
    ("005" active "{1-}")
    ("302" active "User hosts: {1}")
    ("303" active "Users online: {1}")
    ("301" 0 "User away: {1}")
    ("305" active "{1}")
    ("306" active "{1}")
    ("311" active "{1} is {2}@{3} ({5})")
    ("312" active "{1} is on {2} ({3})")
    ("313" active "{1} {2}")
    ("317" active "{1} is {2} seconds idle (Signon {3})")
    ("318" active "{2}")
    ("319" active "{1} is on {2}")
    ("314" active "{1} was {2}@{3} ({5})")
    ("369" active "{1} {2}")
    ("322" active "{1-}")
    ("323" active "{1-}")
    ("325" 1 "Unique operator on {1} is {2}")
    ("324" 1 "Channel mode for {1}: {2-}")
    ("331" 1 "No topic for {1} set")
    ("332" 1 "Topic for {1}: {2}")
    ("333" 1 "Topic for {1} set by {2} at {3}")
    ("341" active "Inviting {2} to {1}")
    ("346" 1 "Invite mask: {2}")
    ("347" 1 "{2}")
    ("348" 1 "Except mask: {2}")
    ("349" 1 "{2}")
    ("351" active "{1-}")
    ("352" active "{5} ({2}@{3}) in {1} on {4}: {6-}")
    ("315" active "{2}")
    ("353" 2 "Names: {3}")
    ("366" 1 "{2}")
    ("364" active "{1-}")
    ("365" active "{1-}")
    ("367" 1 "Ban mask: {2}")
    ("368" 1 "{2}")
    ("401" active "No such nick: {1}")
    ("402" active "No such server: {1}")
    ("403" active "No such channel: {1}")
    ("404" 1 "Can not send to channel {1}")
    ("405" active "Can not join {1}: {2}")
    ("406" active "{1-}")
    ("407" active "{1-}")
    ("408" active "No such service: {1}")
    ;; I got bored, someone else add the rest
    ("422" active "{1}")
    ("432" active "Erroneous nick name: {1}")
    ("433" active "Nick name in use: {1}")
    ("437" active "Nick/channel is temporarily unavailable: {1}")
    ("441" 2 "User not not channel: {1}")
    ("442" active "You are not on {1}")
    ("443" 2 "User {1} is already on channel {2}")
    ("467" 1 "{2}")
    ("471" 1 "{2}")
    ("472" active "{1-}")
    ("473" active "{1-}")
    ("474" active "{1-}")
    ("475" active "{1-}")
    ("476" active "{1-}")
    ("477" active "{1-}")
    ("481" 1 "{2-}")
    ("484" active "{1-}")
    ("371" active "{1}")
    ("374" active "{1}")
    ("375" active "{1}")
    ("372" active "{1}")
    ("376" active "{1}")
    ("381" active "{1}")
    ("382" active "{1-}")
    ("391" active "Time on {1}: {2}")
    ("200" active "{1-}")
    ("201" active "{1-}")
    ("203" active "{1-}")
    ("204" active "{1-}")
    ("205" active "{1-}")
    ("206" active "{1-}")
    ("207" active "{1-}")
    ("208" active "{1-}")
    ("209" active "{1-}")
    ("261" active "{1-}")
    ("262" active "{1-}")
    ("211" active "{1-}")
    ("212" active "{1-}")
    ("219" active "{1-}")
    ("242" active "{1}")
    ("243" active "{1-}")
    ("221" active "User mode: {1-}")
    ("234" active "Service: {1-}")
    ("235" active "{1-}")
    ("250" active "{1}")
    ("251" active "{1}")
    ("252" active "{1-}")
    ("253" active "{1-}")
    ("254" active "{1-}")
    ("255" active "{1}")
    ("256" active "{1-}")
    ("257" active "{1}")
    ("258" active "{1}")
    ("259" active "{1}")
    ("263" active "{1-}")
    ("265" active "{1-}")
    ("266" active "{1-}")
    )
  "A list of strings used to format IRC message.
Each element of the list consists of four parts:

  command  - A string naming the command this applies to
  target   - The target of this message (see below)
  format   - The format for this string (see below)
  trackedp - Optional boolean saying that this message
             should cause Lui tracking in the mode line

The target can be any of:

  'active  - The last active buffer of this server
  'nick    - The nick who sent this message
  number   - The index of the argument of the target

The strings itself are formatted using `lui-format'. Possible
format strings are {mynick}, {target}, {nick}, {user}, {host},
{origin}, {command}, {target}, and indexed arguments for the
arguments to the IRC message."
  :type '(repeat (list (string :tag "Message")
                       (choice :tag "Destination Window"
                               (const :tag "Active Window" active)
                               (const :tag "Originating Nick" nick)
                               (number :tag "Index"))
                       (string :tag "Format")))
  :group 'circe)

(defun circe-server-default-display-command (nick user host command args)
  "Show a default message according to `circe-format-strings'."
  (let ((spec (assoc command circe-format-strings)))
    (when spec
      (let* ((target+name (circe-display-target spec nick user host
                                                command args))
             (target (car target+name))
             (name (cdr target+name))
             (format (nth 2 spec))
             (origin (if (or user host)
                         (format "%s (%s@%s)"
                                 (or nick "(unknown)")
                                 (or user "(unknown)")
                                 (or host "(unknown)"))
                       (or nick "(unknown)"))))
        (with-current-buffer (or target
                                 (circe-server-last-active-buffer))
          (let ((circe-format-server-numeric
                 (if target
                     (format "*** %s" format)
                   (format "*** [%s] %s" name format))))
            (circe-display 'circe-format-server-numeric
                           :nick nick :user user :host host
                           :origin origin
                           :command command
                           :target name
                           :indexed-args args))))
      t)))

(defun circe-display-target (spec nick user host command args)
  "Return the target buffer and name.
The buffer might be nil if it is not alive."
  (cond
   ((eq (nth 1 spec) 'nick)
    (cons (circe-server-get-chat-buffer nick)
          nick))
   ((numberp (nth 1 spec))
    (let ((name (nth (nth 1 spec)
                     args)))
      (cons (circe-server-get-chat-buffer name)
            name)))
   ((eq (nth 1 spec) 'active)
    (let ((buf (circe-server-last-active-buffer)))
      (cons buf
            (buffer-name buf))))
   (t
    (error "Bad target in format string: %s" (nth 1 spec)))))

;;;;;;;;;;;;;;;;;;
;;; Extensions ;;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;
;;; Auto-Join
(defcustom circe-server-auto-join-channels nil
  "*The default channels to join.
Each element in this list has a regular expression matching a
server or network name, and a list of channels to join.
The car can also be a symbol, which is called as a function and
should return non-nil if we should join the appropriate
channels."
  :type '(repeat (cons (regexp :tag "Server or Network")
                       (repeat :tag channels
                               string)))
  :group 'circe)

(add-hook 'circe-server-connected-hook 'circe-auto-join)
(defun circe-auto-join ()
  "Join default channels, as per `circe-server-auto-join-channels'."
  (catch 'exit
    (mapc (lambda (entry)
            (when (if (symbolp (car entry))
                      (funcall (car entry))
                    (string-match (car entry) circe-server-network))
              (mapc #'circe-command-JOIN
                    (cdr entry))
              (throw 'exit t)))
          circe-server-auto-join-channels)))

;;;;;;;;;;;;;;;;;;;
;;; Topic Handling
(defvar circe-channel-topic ""
  "The current topic of the channel.")
(make-variable-buffer-local 'circe-channel-topic)

(defun circe-command-TOPIC (newtopic)
  "Change the topic of the current channel to NEWTOPIC."
  (interactive "sNew topic: ")
  (cond
   ((not circe-chat-target)
    (circe-server-message "No target for current buffer"))
   ((string= newtopic "")
    (circe-server-send (format "TOPIC %s" circe-chat-target)))
   (t
    (circe-server-send (format "TOPIC %s :%s"
                               circe-chat-target
                               newtopic)))))

(defun circe-command-CHTOPIC (&optional ignored)
  "Insert the topic of the current channel."
  (interactive)
  (if (not circe-chat-target)
      (circe-server-message "No target for current buffer")
    (lui-replace-input (format "/TOPIC %s" circe-channel-topic))
    (goto-char (point-max))))

(add-hook 'circe-receive-message-functions 'circe-topic-handler)
(defun circe-topic-handler (nick user host command args)
  "Manage the topic."
  (cond
   ((string= command "331")             ; RPL_NOTOPIC
    (with-circe-chat-buffer (cadr args)
      (setq circe-channel-topic "")))
   ((string= command "332")             ; RPL_TOPIC
    (with-circe-chat-buffer (cadr args)
      (setq circe-channel-topic (nth 2 args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nickserv Authentication
(defcustom circe-nickserv-alist
  '(("freenode"
     "NickServ" "NickServ" "services."
     "/msg\\s-NickServ\\s-\C-bIDENTIFY\C-b\\s-<password>"
     "PRIVMSG NickServ :IDENTIFY %s"))
  "*A list of nickserv configurations.
Each element of this list is a list with the following items:

  NETWORK   - A regular expression matching the network or server name
  NICK      - The nick of the nickserv
  USER      - The user name of the nickserv
  HOST      - The hostname of the nickserv
  NOTICE    - A regular expression matching the message from nickserv
  REPLY     - The message sent to the nickserv, where %s is the password

See also `circe-nickserv-password'."
  :type '(repeat (list (regexp :tag "Network")
                       (string :tag "Nick")
                       (string :tag "User")
                       (string :tag "Host")
                       (regexp :tag "Notice")
                       (string :tag "Reply")))
  :group 'circe)

(defcustom circe-nickserv-passwords nil
  "*A list of nickserv passwords.
Each entry consists of two elements, the network name and the
password for this network."
  :type '(repeat (list (string :tag "Network")
                       (string :tag "Password")))
  :group 'circe)

(defvar circe-nickserv-registered-p nil
  "Non-nil when we did register with nickserv here.")
(make-variable-buffer-local 'circe-nickserv-registered-p)

(add-hook 'circe-receive-message-functions 'circe-nickserv-handler)
(defun circe-nickserv-handler (nick user host command args)
  "Register automatically with nickserv."
  (with-circe-server-buffer
    (when (string= command "001")
      ;; Reconnect!
      (setq circe-nickserv-registered-p nil))
    (when (and (not circe-nickserv-registered-p)
               circe-nickserv-passwords
               (string= command "NOTICE"))
      (catch 'return
        (mapc (lambda (entry)
                (when (and (string= (nth 0 entry)
                                    circe-server-network)
                           (string= (nth 1 entry)
                                    nick)
                           (string= (nth 2 entry)
                                    user)
                           (string= (nth 3 entry)
                                    host)
                           (string-match (nth 4 entry)
                                         (cadr args)))
                  (let ((pass (assoc circe-server-network
                                     circe-nickserv-passwords)))
                    (when pass
                      (circe-server-send (format (nth 5 entry)
                                                 (cadr pass)))
                      (setq circe-nickserv-registered-p t)
                      (throw 'return t)))))
              circe-nickserv-alist)))))

(provide 'circe)
;;; circe.el ends here
