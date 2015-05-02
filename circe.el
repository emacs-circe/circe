;;; circe.el --- Client for IRC in Emacs

;; Copyright (C) 2005 - 2015  Jorgen Schaefer

;; Version: 1.6
;; Keywords: IRC, chat
;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; URL: https://github.com/jorgenschaefer/circe

;; This file is part of Circe.

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

;; Circe is a Client for IRC in Emacs. It integrates well with the rest
;; of the editor, using standard Emacs key bindings and indicating
;; activity in channels in the status bar so it stays out of your way
;; unless you want to use it.

;;; Code:

(defvar circe-version "1.6"
  "Circe version string.")

(require 'ring)
(require 'timer)
(require 'lui)
(require 'lui-format)
(require 'lcs)
(require 'irc)

;; Used to be optional. But sorry, we're in the 21st century already.
(require 'lui-irc-colors)
(enable-lui-irc-colors)

(defgroup circe nil
  "Yet Another Emacs IRC Client."
  :prefix "circe-"
  :group 'applications)

;;;;;;;;;;;;;
;;; Faces ;;;
;;;;;;;;;;;;;

;; These come before the `defcustom's since they are used for some of
;; the default values.

(defface circe-prompt-face
  '((t (:weight bold :foreground "Black" :background "LightSeaGreen")))
  "The face for the Circe prompt."
  :group 'circe)

(defface circe-server-face
  '((((type tty)) :foreground "blue" :weight bold)
    (t (:foreground "SteelBlue")))
  "The face used to highlight server messages."
  :group 'circe)

(defface circe-highlight-nick-face
  '((((type tty)) (:foreground "cyan" :weight bold))
    (t (:foreground "CadetBlue3" :weight bold)))
  "The face used to highlight messages directed to us."
  :group 'circe)

(defface circe-my-message-face '((t))
  "The face used to highlight our own messages."
  :group 'circe)

(defface circe-originator-face '((t))
  "The face used to highlight the originator of a message."
  :group 'circe)

(defface circe-topic-diff-new-face '((t (:background "DarkGreen")))
  "The face used for text added to a topic.
See the {topic-diff} parameter to `circe-format-server-topic'."
  :group 'circe)

(defface circe-topic-diff-removed-face '((t (:background "DarkRed")))
  "The face used for text removed from a topic.
See the {topic-diff} parameter to `circe-format-server-topic'."
  :group 'circe)

(defface circe-fool-face
  '((((type tty)) (:foreground "grey40" :bold t))
    (((type x)) (:foreground "grey40")))
  "The face used for fools.
See `circe-fool-list'."
  :group 'circe)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization Variables ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom circe-default-nick (user-login-name)
  "The default nick for circe."
  :type 'string
  :group 'circe)

(defcustom circe-default-user circe-default-nick
  "The default user name for circe."
  :type 'string
  :group 'circe)

(defcustom circe-default-realname (if (string= (user-full-name) "")
                                      circe-default-nick
                                    (user-full-name))
  "The default real name for circe."
  :type 'string
  :group 'circe)

(defcustom circe-default-ip-family nil
  "Default IP family to use.

  'nil  - Use either IPv4 or IPv6.

  'ipv4 - Use IPv4

  'ipv6 - Use IPv6"
  :type '(choice (const :tag "Both" nil)
                 (const :tag "IPv4" ipv4)
                 (const :tag "IPv6" ipv6))
  :group 'circe)

(defcustom circe-default-directory "~/"
  "The value of `default-directory' for Circe buffers."
  :type 'string
  :group 'circe)

(defcustom circe-network-options nil
  "Network options.

This alist maps network names to respective options.

Common options:

  :pass - The IRC server password to use for this network or a function to fetch it.
  :nick - The nick name to use (defaults to `circe-default-nick')
  :user - The user name to use (defaults to `circe-default-user')
  :realname - The real name to use (defaults to `circe-default-realname')
  :channels - A plist of channels to join
              (see `circe-server-auto-join-channels').
  :server-buffer-name - Format to be used for the server buffer name
                        (see `circe-server-buffer-name')

  :host - The host name of the server to connect to.
  :service - The service name or port for the server.
  :tls - A boolean indicating as to whether to use TLS or
         not (defaults to nil). If you set this, you'll likely
         have to set :service as well.
  :family - Option to enforce a specific IP version
            (defaults to `circe-default-ip-family')

  :nickserv-nick - The nick to authenticate with to nickserv, if configured.
                   (defaults to the value of :nick)
  :nickserv-password - The password to use for nickserv
                       authentication or a function to fetch it.

  :sasl-username - The username for SASL authentication.
  :sasl-password - The password for SASL authentication."
  :type '(alist :key-type string :value-type plist)
  :group 'circe)

(defvar circe-networks
  '(("Freenode" :host "irc.freenode.net" :port (6667 . 6697)
     :nickserv-mask "^NickServ!NickServ@services\\.$"
     :nickserv-identify-challenge "\C-b/msg\\s-NickServ\\s-identify\\s-<password>\C-b"
     :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {nick} {password}"
     :nickserv-identify-confirmation "^You are now identified for .*\\.$"
     :nickserv-ghost-command "PRIVMSG NickServ :GHOST {nick} {password}"
     :nickserv-ghost-confirmation "has been ghosted\\.$\\|is not online\\.$"
     )
    ("Coldfront" :host "irc.coldfront.net" :port 6667
     :nickserv-mask "^NickServ!services@coldfront\\.net$"
     :nickserv-identify-challenge "/msg\\s-NickServ\\s-IDENTIFY\\s-\C-_password\C-_"
     :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {password}"
     )
    ("Bitlbee" :host "localhost"
     :nickserv-mask "\\(bitlbee\\|root\\)!\\(bitlbee\\|root\\)@"
     :nickserv-identify-challenge "use the \x02identify\x02 command to identify yourself"
     :nickserv-identify-command "PRIVMSG &bitlbee :identify {password}"
     :nickserv-identify-confirmation "Password accepted, settings and accounts loaded"
     )
    ("OFTC" :host "irc.oftc.net" :port (6667 . 6697)
     :nickserv-mask "^NickServ!services@services\\.oftc\\.net$"
     :nickserv-identify-challenge "This nickname is registered and protected."
     :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {password} {nick}"
     :nickserv-identify-confirmation "^You are successfully identified as .*\\.$"
     )
    )
  "Alist of networks and connection settings.

See the `circe' command for details of this variable.")

(defcustom circe-default-quit-message "Using Circe, the loveliest of all IRC clients"
  "The default quit message when no other is given.

This is sent when the server buffer is killed or when /QUIT is
given with no argument."
  :type 'string
  :group 'circe)

(defcustom circe-default-part-message "Using Circe, the loveliest of all IRC clients"
  "How to part when a channel buffer is killed, or when no
argument is given to /PART."
  :type 'string
  :group 'circe)

(defcustom circe-new-buffer-behavior 'display
  "How new buffers should be treated.

  'display  - Show them, but don't select them
  'switch   - Switch input to that buffer
  'ignore   - Open them in the background

This does NOT affect buffers created with /join or /query.

Also see `circe-new-buffer-behavior-ignore-auto-joins'."
  :type '(choice (const :tag "Display" display)
                 (const :tag "Switch" switch)
                 (const :tag "Ignore" ignore))
  :group 'circe)

(defcustom circe-new-buffer-behavior-ignore-auto-joins nil
  "Don't show channel buffers when they were auto-joined.

When t, the value of `circe-new-buffer-behavior' will be ignored
for auto-joined channel buffers."
  :type 'boolean
  :group 'circe)

(defcustom circe-auto-query-p t
  "Non-nil if queries should be opened automatically."
  :type 'boolean
  :group 'circe)

(defcustom circe-auto-query-max 23
  "The maximum number of queries which are opened automatically.
If more messages arrive - typically in a flood situation - they
are displayed as if `circe-auto-query-p' was nil."
  :type 'integer
  :group 'circe)

(defcustom circe-use-cycle-completion nil
  "Whether Circe should use cycle completion.

If this is not nil, Circe will set `completion-cycle-threshold'
to t locally in Circe buffers, enabling cycle completion for
nicks no matter what completion style you use in the rest of
Emacs. If you set this to nil, Circe will not touch your default
completion style.

NOTE: Emacs 24.2 and before have a bug related to cycle
completion. If you enable cycle completion in those Emacsen, you
should also (require 'circe-fix-minibuffer). This is a backported
minibuffer.el from 24.3, though, so do this at your own
discretion.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=12619 for a
discussion of the bug."
  :type 'boolean
  :group 'circe)

(defcustom circe-reduce-lurker-spam nil
  "If enabled, Circe will stop showing some messages.

This means that JOIN, PART, QUIT and NICK messages are not shown
for users on channels that have not spoken yet (\"lurker\"), or
haven't spoken in `circe-active-users-timeout' seconds. When they
speak for the first time, Circe displays their join time."
  :type 'boolean
  :group 'circe)

(defcustom circe-active-users-timeout nil
  "When non-nil, should be the number of seconds after which
active users are regarded as inactive again after speaking."
  :type 'integer
  :group 'circe)

(defcustom circe-channel-recent-users-timeout (* 20 60)
  "Forget about parted users after this many seconds.

If nil, parted users are immediately forgotten about."
  :type 'integer
  :group 'circe)

(defcustom circe-prompt-string (concat (propertize ">"
                                                   'face 'circe-prompt-face)
                                       " ")
  "The string to initialize the prompt with.
To change the prompt dynamically or just in specific buffers, use
`lui-set-prompt' in the appropriate hooks."
  :type 'string
  :group 'circe)

(defcustom circe-highlight-nick-type 'sender
  "How to highlight occurrences of our own nick.

  'sender     - Highlight the nick of the sender
  'occurrence - Highlight the occurrences of the nick
  'message    - Highlight the message without the sender
  'all        - Highlight the whole line"
  :type '(choice (const :tag "Sender" sender)
                 (const :tag "Occurrences" occurrence)
                 (const :tag "Message" message)
                 (const :tag "Whole line" all))
  :group 'circe)

(defcustom circe-completion-suffix ": "
  "A suffix for completed nicks at the beginning of a line."
  :type '(choice (const :tag "The standard suffix" ": "))
  :group 'circe)

(defcustom circe-ignore-list nil
  "List of regular expressions to ignore.

Each regular expression is matched against nick!user@host."
  :type '(repeat regexp)
  :group 'circe)

(defcustom circe-fool-list nil
  "List of regular expressions for fools.

Each regular expression is matched against nick!user@host.

Messages from such people are still inserted, but not shown. They
can be displayed using \\[lui-toggle-ignored]. If this is not
wanted, set `circe-ignore-hard-p'."
  :type '(repeat regexp)
  :group 'circe)

(defcustom circe-ignore-functions nil
  "A list of functions to check whether we should ignore a message.
These functions get five arguments: NICK, USER, HOST, COMMAND, and ARGS.
If one of them returns a non-nil value, the message is ignored."
  :type 'hook
  :group 'circe)

(defcustom circe-split-line-length 440
  "The maximum length of a single message.
If a message exceeds this size, it is broken into multiple ones.

IRC allows for lines up to 512 bytes. Two of them are CR LF.
And a typical message looks like this:

  :nicky!uhuser@host212223.dialin.fnordisp.net PRIVMSG #lazybastards :Hello!

You can limit here the maximum length of the \"Hello!\" part.
Good luck."
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

(defcustom circe-netsplit-delay 60
  "The number of seconds a netsplit may be dormant.
If anything happens with a netsplit after this amount of time,
the user is re-notified."
  :type 'number
  :group 'circe)

(defcustom circe-server-killed-confirmation 'ask
  "How to ask for confirmation when a server buffer is killed.
This can be one of the following values:
  ask - Ask the user for confirmation
  ask-and-kill-all - Ask the user, and kill all associated buffers
  nil - Kill first, ask never"
  :type '(choice (const :tag "Ask before killing" ask)
                 (const :tag "Ask, then kill all associated buffers"
                        ask-and-kill-all)
                 (const :tag "Don't ask" nil))
  :group 'circe)

(defcustom circe-channel-killed-confirmation 'ask
  "How to ask for confirmation when a channel buffer is killed.
This can be one of the following values:
  ask - Ask the user for confirmation
  nil - Don't ask, just kill"
  :type '(choid (const :tag "Ask before killing" ask)
                (const :tag "Don't ask" nil))
  :group 'circe)

(defcustom circe-track-faces-priorities '(circe-highlight-nick-face
                                          lui-highlight-face
                                          circe-my-message-face
                                          circe-server-face)
  "A list of faces which should show up in the tracking.
The first face is kept if the new message has only lower faces,
or faces that don't show up at all."
  :type '(repeat face)
  :group 'circe)

(defcustom circe-server-send-unknown-command-p nil
  "Non-nil when Circe should just pass on commands it doesn't know.
E.g. /fnord foo bar would then just send \"fnord foo bar\" to the
server."
  :type 'boolean
  :group 'circe)

(defcustom circe-receive-message-functions nil
  "Functions called when a message from the IRC server arrives.
Each function is called with 5 arguments: NICK, USER, HOST,
COMMAND, and ARGS."
  :type 'hook
  :group 'circe)

(defcustom circe-server-connected-hook nil
  "Hook run when we successfully connected to a server.
This is run from a 001 (RPL_WELCOME) message handler."
  :type 'hook
  :group 'circe)

(defcustom circe-server-mode-hook nil
  "Hook run when circe connects to a server."
  :type 'hook
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
                                      circe-format-server-numeric
                                      circe-format-server-topic
                                      circe-format-server-rejoin
                                      circe-format-server-lurker-activity)
  "A list of formats that should not trigger tracking."
  :type '(repeat symbol)
  :group 'circe-format)

(defcustom circe-format-server-message "*** {body}"
  "The format for generic server messages.
{body} - The body of the message."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-self-say "> {body}"
  "The format for messages to queries or channels.
{nick} - Your nick.
{body} - The body of the message."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-self-action "* {nick} {body}"
  "The format for actions to queries or channels.
{nick} - Your nick.
{body} - The body of the action."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-self-message "-> *{target}* {body}"
  "The format for messages sent to other people outside of queries.
{target} - The target nick.
{body} - The body of the message."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-action "* {nick} {body}"
  "The format for actions in queries or channels.
{nick} - The nick doing the action.
{body} - The body of the action."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-message-action "* *{nick}* {body}"
  "The format for actions in messages outside of queries.
{nick} - The nick doing the action.
{body} - The body of the action."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-say "<{nick}> {body}"
  "The format for normal channel or query talk.
{nick} - The nick talking.
{body} - The message."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-message "*{nick}* {body}"
  "The format for a message outside of a query.
{nick} - The originator.
{body} - The message."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-notice "-{nick}- {body}"
  "The format for a notice.
{nick} - The originator.
{body} - The notice."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-server-notice "-Server Notice- {body}"
  "The format for a server notice.
{body} - The notice."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-server-topic "*** Topic change by {origin}: {new-topic}"
  "The format for topic changes.
{channel} - Where the topic change happened.
{new-topic} - The new topic.
{old-topic} - The previous topic.
{topic-diff} - A colorized diff of the topics."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-server-lurker-activity
  "*** First activity: {nick} joined {joindelta} ago."
  "The format for the first-activity notice of a user.
{nick} - The originator.
{jointime} - The join time of the user (in seconds).
{joindelta} - The duration from joining until now."
  :type 'string
  :group 'circe-format)

(defcustom circe-format-server-rejoin
  "*** Re-join: {nick} ({nick}@{host})"
  "The format for the re-join notice of a user.
{nick} - The originator.
{user} - User name of the originator.
{host} - Host name of the originator.
{departuretime} - Time in seconds when the originator had left.
{departuredelta} - Description of the time delta since the originator left."
  :type 'string
  :group 'circe-format)

(defcustom circe-server-buffer-name "{host}:{service}"
  "The format for the server buffer name.

The following format arguments are available:

  network  - The name of the network
  host     - The host name of the server
  service  - The service or port number
  port     - Alias for service"
  :type 'string
  :group 'circe-format)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private variables ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar circe-source-url "https://github.com/jorgenschaefer/circe"
  "URL to Circe's source repository")

(defvar circe-server-name nil
  "The name of the server we're currently connected to.")
(make-variable-buffer-local 'circe-server-name)

(defvar circe-server-service nil
  "The service name or port of the server we're currently connected to.")
(make-variable-buffer-local 'circe-server-service)

(defvar circe-server-network nil
  "The network name of the server we're currently connected to.")
(make-variable-buffer-local 'circe-server-network)

(defvar circe-server-ip-family nil
  "The IP family in use.
See `make-network-process' and :family for valid values.")
(make-variable-buffer-local 'circe-server-ip-family)

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
  "The password for the current server or a function to recall it.

If a function is set it will be called with the value of `circe-server-name'.")
(make-variable-buffer-local 'circe-server-pass)

(defvar circe-sasl-username nil
  "The username for SASL authentication.")
(make-variable-buffer-local 'circe-sasl-username)

(defvar circe-sasl-password nil
  "The password for SASL authentication.

If a function is set it will be called with the value of
`circe-server-name'.")
(make-variable-buffer-local 'circe-sasl-password)

(defvar circe-server-use-tls nil
  "If non-nil, use `open-tls-stream' to connect to the server.")
(make-variable-buffer-local 'circe-server-use-tls)

(defvar circe-server-process nil
  "The process of the server connection.")
(make-variable-buffer-local 'circe-server-process)

(defvar circe-server-last-active-buffer nil
  "The last active circe buffer.")
(make-variable-buffer-local 'circe-server-last-active-buffer)

(defvar circe-server-chat-buffers nil
  "A hash of chat buffers associated with this server.")
(make-variable-buffer-local 'circe-server-chat-buffers)

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
the word checker is started for the first, but might take long
enough for the second message to be processed first. Nice, isn't
it.")
(make-variable-buffer-local 'circe-server-processing-p)

(defvar circe-display-table nil
  "A hash table mapping commands to their display functions.")

(defvar circe-message-handler-table nil
  "A hash table mapping commands to their handler function lists.")

(defvar circe-irc-handler-table nil
  "The handler table for Circe's IRC connections")

(defvar circe-server-quitting-p nil
  "Non-nil when quitting from the server.
This is only non-nil when the user is quitting the current
server. See `circe-command-QUIT'.")
(make-variable-buffer-local 'circe-server-quitting-p)

(defvar circe-chat-target nil
  "The current target for the buffer.
This is either a channel or a nick name.")
(make-variable-buffer-local 'circe-chat-target)

(defvar circe-nick-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table))
        (special (string-to-list "[]\`_^{}|-"))
        (digits (string-to-list "0123456789")))
    (dolist (char special)
      (modify-syntax-entry char "w" table))
    table)
  "Syntax table to treat nicks as words.
This is not entirely accurate, as exact chars constituting a nick
can vary between networks.")

(defvar circe-base-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lui-mode-map)
    (define-key map (kbd "C-c C-j") 'circe-command-JOIN)
    (define-key map (kbd "C-c C-r") 'circe-reconnect)
    map)
  "The base keymap for all Circe modes (server, channel, query)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User Utility Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun circe-version ()
  "Display Circe's version."
  (interactive)
  (message "Circe %s" circe-version))

;;;;;;;;;;;;;;;;;;;
;;; Server Mode ;;;
;;;;;;;;;;;;;;;;;;;

(defvar circe-server-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map circe-base-mode-map)
    map)
  "The key map for server mode buffers.")

(defun circe-server-mode ()
  "The mode for circe server buffers.
This buffer represents a server connection. When you kill it, the
server connection is closed. This will make all associated
buffers unusable. Be sure to use \\[circe-reconnect] if you want
to reconnect to the server.

\\{circe-server-mode-map}"
  (lui-mode)
  (add-hook 'lui-pre-output-hook 'circe-highlight-nick
            nil t)
  (setq major-mode 'circe-server-mode
        mode-name "Circe Server"
        lui-input-function 'circe-chat-input
        default-directory (expand-file-name circe-default-directory))
  (use-local-map circe-server-mode-map)
  (lui-set-prompt circe-prompt-string)
  (goto-char (point-max))
  (setq circe-server-last-active-buffer (current-buffer))
  (add-hook 'completion-at-point-functions 'circe-completion-at-point
            nil t)
  (when circe-use-cycle-completion
    (set (make-local-variable 'completion-cycle-threshold)
         t))
  ;; Tab completion should be case-insensitive
  (set (make-local-variable 'completion-ignore-case)
       t)
  (goto-char (point-max))
  (setq circe-server-last-active-buffer (current-buffer))
  (add-hook 'kill-buffer-hook
            'circe-buffer-killed)
  (run-hooks 'circe-server-mode-hook))

(defun circe-read-network ()
  "Read a host or network name with completion.

This uses `circe-networks' and `circe-network-options' for
network names."
  (let ((default-network (if (null circe-network-options)
                             (caar circe-networks)
                           (caar circe-network-options)))
        (networks nil)
        (completion-ignore-case t))
    (dolist (network-spec (append circe-network-options
                                  circe-networks))
      (push (car network-spec) networks))
    (completing-read "Network or host: "
                     (sort networks 'string-lessp)
                     nil nil nil nil
                     default-network)))

(defun circe-parse-options (network-or-server options)
  "Turn the arguments to `circe' into hash mapping variable names to values.

NETWORK-OR-SERVER is a string selection a network name. If it's
not found as a network name, it's assumed to be the name of a
server.

OPTIONS is a property list with keyword options. See
`circe-network-options' for a list and further explanation."
  (let ((options (append options
                         (cdr (assoc-string network-or-server
                                            circe-network-options
                                            t))
                         (cdr (assoc-string network-or-server
                                            circe-networks
                                            t))))
        (variables (make-hash-table :test 'eq))
        (not-in-hash (make-symbol "not-in-hash/uninterned")))
    (while options
      (when (not (keywordp (car options)))
        (error "Bad options to `circe', expected plist with keywords"))
      (let ((keyword (car options))
            (value (cadr options))
            (variable nil))
        (cond
         ((eq keyword :host)
          (setq variable 'circe-server-name))
         ((memq keyword '(:service :port))
          (setq variable 'circe-server-service))
         ((memq keyword '(:tls :use-tls))
          (setq variable 'circe-server-use-tls))
         ((eq keyword :family)
          (setq variable 'circe-server-ip-family))
         ((eq keyword :channels)
          (setq variable 'circe-server-auto-join-channels))
         ((memq keyword '(:pass :nick :user :realname :network))
          (setq variable (intern (concat "circe-server-"
                                         (substring (symbol-name keyword)
                                                    1)))))
         (t
          (setq variable (intern (concat "circe-"
                                         (substring (symbol-name keyword)
                                                    1))))))
        (when (eq not-in-hash (gethash variable variables not-in-hash))
          (puthash variable value variables)))
      (setq options (cddr options)))
    ;; Host and network defaults from the main argument
    (when (not (gethash 'circe-server-network variables))
      (puthash 'circe-server-network network-or-server variables))
    (when (not (gethash 'circe-server-name variables))
      (puthash 'circe-server-name network-or-server variables))
    ;; Other defaults from global variables
    (when (not (gethash 'circe-server-nick variables))
      (puthash 'circe-server-nick circe-default-nick variables))
    (when (not (gethash 'circe-server-user variables))
      (puthash 'circe-server-user circe-default-user variables))
    (when (not (gethash 'circe-server-realname variables))
      (puthash 'circe-server-realname circe-default-realname variables))
    (when (not (gethash 'circe-server-ip-family variables))
      (puthash 'circe-server-ip-family circe-default-ip-family variables))
    ;; Default values that depend on other variables
    (let ((service (gethash 'circe-server-service variables))
          (use-tls (gethash 'circe-server-use-tls variables)))
      (when (consp service)
        (puthash 'circe-server-service
                 (if use-tls
                     (cdr service)
                   (car service))
                 variables)))
    (when (not (gethash 'circe-nickserv-nick variables))
      (puthash 'circe-nickserv-nick
               (gethash 'circe-server-nick variables)
               variables))
    variables))

;;;###autoload
(defun circe (network-or-server &rest options)
  "Connect to IRC.

Connect to the given network specified by NETWORK-OR-SERVER.

When this function is called, it collects options from the
OPTIONS argument, the user variable `circe-network-options', and
the defaults found in `circe-networks', in this order.

If NETWORK-OR-SERVER is not found in any of these variables, the
argument is assumed to be the host name for the server, and all
relevant settings must be passed via OPTIONS.

All OPTIONS are treated as variables by getting the string
\"circe-\" prepended to their name. This variable is then set
locally in the server buffer.

See `circe-network-options' for a list of common options."
  (interactive (list (circe-read-network)))
  (let ((variables (circe-parse-options network-or-server options))
        host service)
    (setq host (gethash 'circe-server-name variables)
          service (gethash 'circe-server-service variables))
    (when (not service)
      (if (called-interactively-p 'any)
          (let* ((input (read-from-minibuffer "Port: " "6667"))
                 (port (string-to-number input)))
            (setq service (if (= port 0)
                              input
                            port)))
        (setq service 6667))
      (puthash 'circe-server-service service variables))
    (let* ((buffer-name-format (or (gethash 'circe-server-buffer-name
                                            variables)
                                   circe-server-buffer-name))
           (buffer-name (lui-format buffer-name-format
                                    :network network-or-server
                                    :host host
                                    :service service
                                    :port service))
           (server-buffer (generate-new-buffer buffer-name)))
      (with-current-buffer server-buffer
        (circe-server-mode)
        (let ((unknown-variables nil))
          (maphash (lambda (variable value)
                     (if (not (boundp variable))
                         (setq unknown-variables (cons variable
                                                       unknown-variables))
                       (set (make-local-variable variable)
                            value)))
                   variables)
          (dolist (var unknown-variables)
            (circe-server-message (format "Unknown variable %s, re-check your configuration."
                                          var))))
        (circe-reconnect)
        (switch-to-buffer server-buffer)))))

(defvar circe-server-buffer nil
  "The buffer of the server associated with the current chat buffer.")
(make-variable-buffer-local 'circe-server-buffer)

(defmacro with-circe-server-buffer (&rest body)
  "Run BODY with the current buffer being the current server buffer."
  (let ((server (make-symbol "server")))
    `(let ((,server (cond
                      ((eq major-mode 'circe-server-mode)
                       (current-buffer))
                      (circe-server-buffer
                       circe-server-buffer)
                      (t
                       (error "`with-circe-server-buffer' outside of an circe buffer")))))
       (when (and ,server ;; Might be dead!
                  (bufferp ,server)
                  (buffer-live-p ,server))
         (with-current-buffer ,server
           ,@body)))))
(put 'with-circe-server-buffer 'lisp-indent-function 0)

(defmacro with-circe-chat-buffer (name &rest body)
  "Switch to the chat buffer NAME and run BODY there.

If no such buffer exists, do nothing."
  (let ((buf (make-symbol "buf")))
    `(let ((,buf (circe-server-get-chat-buffer ,name)))
       (when ,buf
         (with-current-buffer ,buf
           ,@body)))))
(put 'with-circe-chat-buffer 'lisp-indent-function 1)

(defvar circe-server-reconnect-attempts 0
  "The number of reconnect attempts that Circe has done so far.
See `circe-server-max-reconnect-attempts'.")
(make-variable-buffer-local 'circe-server-reconnect-attempts)

(defun circe-reconnect ()
  "Reconnect the current server."
  (interactive)
  (with-circe-server-buffer
    (when (or (called-interactively-p 'any) ;; Always reconnect if the user wants it
              (not circe-server-max-reconnect-attempts)
              (<= circe-server-reconnect-attempts
                  circe-server-max-reconnect-attempts))
      (setq circe-server-reconnect-attempts (+ circe-server-reconnect-attempts
                                               1))
      (when (and circe-server-process
                 (process-live-p circe-server-process))
        (delete-process circe-server-process))
      (when (not circe-irc-handler-table)
        (setq circe-irc-handler-table (circe-irc-handler-table)))
      (circe-server-message "Connecting...")
      (dolist (buf (circe-chat-buffers))
        (with-current-buffer buf
          (circe-server-message "Connecting...")))
      (setq circe-server-process
            (irc-connect
             :host circe-server-name
             :service circe-server-service
             :tls circe-server-use-tls
             :ip-family circe-server-ip-family
             :handler-table circe-irc-handler-table
             :server-buffer (current-buffer)
             :nick circe-server-nick
             :nick-alternatives (list (circe-nick-next circe-server-nick)
                                      (circe-nick-next
                                       (circe-nick-next circe-server-nick)))
             :user circe-server-user
             :mode 8
             :realname circe-server-realname
             :pass (if (functionp circe-server-pass)
                       (funcall circe-server-pass circe-server-name)
                     circe-server-pass)
             :cap-req (when (and circe-sasl-username
                                 circe-sasl-password)
                        '("sasl"))
             :sasl-username circe-sasl-username
             :sasl-password (if (functionp circe-sasl-password)
                                (funcall circe-sasl-password
                                         circe-server-name)
                              circe-sasl-password)
             :ctcp-version (format "Circe: Client for IRC in Emacs, version %s"
                                   circe-version)
             :ctcp-source circe-source-url
             :ctcp-clientinfo "CLIENTINFO PING SOURCE TIME VERSION"
             )))))

(defun circe-reconnect-all ()
  "Reconnect all Circe connections."
  (dolist (buf (circe-server-buffers))
    (with-current-buffer buf
      (if (called-interactively-p 'any)
          (call-interactively 'circe-reconnect)
        (circe-reconnect)))))

(defun circe-server-process ()
  "Return the current server process."
  (with-circe-server-buffer
    circe-server-process))

(defun circe-buffer-killed ()
  "The current buffer is being killed. Do the necessary bookkeeping for circe."
  (cond
   ((eq major-mode 'circe-channel-mode)
    (circe-channel-killed))
   ((eq major-mode 'circe-query-mode)
    (circe-query-killed))
   ((eq major-mode 'circe-server-mode)
    (circe-server-killed))))

(defun circe-server-killed ()
  "Run when the server buffer got killed.

This will IRC, and ask the user whether to kill all of the
server's chat buffers."
  (when circe-server-killed-confirmation
    (when (not (y-or-n-p
                (if (eq circe-server-killed-confirmation 'ask-and-kill-all)
                    "Really kill all buffers of this server? (if not, try `circe-reconnect') "
                  "Really kill the IRC connection? (if not, try `circe-reconnect') ")))
      (error "Buffer not killed as per user request")))
  (setq circe-server-quitting-p t)
  (ignore-errors
    (irc-send-QUIT circe-server-process circe-default-quit-message))
  (ignore-errors
    (delete-process circe-server-process))
  (when (eq circe-server-killed-confirmation 'ask-and-kill-all)
    (dolist (buf (circe-chat-buffers))
      (let ((circe-channel-killed-confirmation nil))
        (kill-buffer buf)))))

(defun circe-server-last-active-buffer ()
  "Return the last active buffer of this server."
  (with-circe-server-buffer
    (if (and circe-server-last-active-buffer
             (bufferp circe-server-last-active-buffer)
             (buffer-live-p circe-server-last-active-buffer))
        circe-server-last-active-buffer
      (current-buffer))))

(defun circe-server-buffers ()
  "Return a list of all server buffers in this Emacs instance."
  (let ((result nil))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (when (eq major-mode 'circe-server-mode)
         (setq result (cons buf result)))))
   (nreverse result)))

(defun circe-server-my-nick-p (nick)
  "Return non-nil when NICK is our current nick."
  (irc-current-nick-p (circe-server-process) nick))

(defun circe-server-nick ()
  "Return our current nick."
  (let ((proc (circe-server-process)))
    (if proc
        (irc-current-nick proc)
      nil)))

(defun circe-nick-next (oldnick)
  "Return a new nick to try for OLDNICK."
  (cond
   ;; If the nick ends with -+, replace those with _
   ((string-match "^\\(.*[^-]\\)\\(-+\\)$" oldnick)
    (concat (match-string 1 oldnick)
            (make-string (- (match-end 2)
                            (match-beginning 2))
                         ?_)))
   ;; If the nick is 9 chars long, take prefix and rotate.
   ((>= (length oldnick)
        9)
    (when (string-match "^\\(.*[^-_]\\)[-_]*$" oldnick)
      (let ((nick (match-string 1 oldnick)))
        (concat (substring nick 1)
                (string (aref nick 0))))))
   ;; If the nick ends with _+ replace those with - and add one
   ((string-match "^\\(.*[^_]\\)\\(_+\\)$" oldnick)
    (concat (match-string 1 oldnick)
            (make-string (- (match-end 2)
                            (match-beginning 2))
                         ?-)
            "-"))
   ;; Else, just append -
   (t
    (concat oldnick "-"))))

(defun circe-server-message (message)
  "Display MESSAGE as a server message."
  (circe-display 'circe-format-server-message
                 :body message))

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
    (let ((seq (circe-message-option 'text-properties)))
      (while seq
        (let ((key (car seq))
              (val (cadr seq)))
          (if (eq 'face key)
              ;; Faces are special. We want to set the default face,
              ;; not override other faces.
              (font-lock-append-text-property 0 (length text)
                                              'face val text)
            (put-text-property 0 (length text) key val text))
          (setq seq (cddr seq)))))
    (when face
      (font-lock-append-text-property 0 (length text)
                                      'face face
                                      text))
    (lui-insert text
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

;; There really ought to be a hook for this!
(defadvice select-window (after circe-server-track-select-window
                                (window &optional norecord))
  "Remember the current buffer as the last active buffer.
This is used by Circe to know where to put spurious messages."
  (with-current-buffer (window-buffer window)
    (when (memq major-mode '(circe-channel-mode
                             circe-query-mode
                             circe-server-mode))
      (let ((buf (current-buffer)))
        (with-circe-server-buffer
          (setq circe-server-last-active-buffer buf))))))
(ad-activate 'select-window)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nick Highlighting ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun circe-highlight-nick ()
  "Highlight the nick of the user in the buffer."
  (goto-char (or (text-property-any (point-min) (point-max)
                                    'lui-format-argument 'body)
                 (point-min)))
  (when (circe-server-nick)
    ;; Can't use \<...\> because that won't match for \<forcer-\> We
    ;; might eventually use \_< ... \_> if we define symbols to be
    ;; nicks \\= is necessary, because it might be found right where we
    ;; are, and that might not be the beginning of a line... (We start
    ;; searching from the beginning of the body)
    (let ((nick-regex (concat "\\(?:^\\|\\W\\|\\=\\)"
                              "\\(" (regexp-quote (circe-server-nick)) "\\)"
                              "\\(?:$\\|\\W\\)")))
      (cond
       ((eq circe-highlight-nick-type 'sender)
        (if (text-property-any (point-min)
                               (point-max)
                               'face 'circe-originator-face)
            (when (re-search-forward nick-regex nil t)
              (circe-highlight-extend-properties
               (point-min) (point-max)
               'face 'circe-originator-face
               '(face circe-highlight-nick-face)))
          (let ((circe-highlight-nick-type 'occurrence))
            (circe-highlight-nick))))
       ((eq circe-highlight-nick-type 'occurrence)
        (while (re-search-forward nick-regex nil t)
          (add-text-properties (match-beginning 1)
                               (match-end 1)
                               '(face circe-highlight-nick-face))))
       ((eq circe-highlight-nick-type 'message)
        (when (re-search-forward nick-regex nil t)
          (let* ((start (text-property-any (point-min)
                                           (point-max)
                                           'lui-format-argument 'body))
                 (end (when start
                        (next-single-property-change start
                                                     'lui-format-argument))))
            (when (and start end)
              (add-text-properties start end
                                   '(face circe-highlight-nick-face))))))
       ((eq circe-highlight-nick-type 'all)
        (when (re-search-forward nick-regex nil t)
          (add-text-properties (point-min)
                               (point-max)
                               '(face circe-highlight-nick-face))))))))

(defun circe-highlight-extend-properties (from to prop val properties)
  "Extend property values.

In the text between FROM and TO, find any text that has property
PROP set to VAL, and extend the properties of that range of text
with all properties in PROPERTIES."
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
  "Compare the two strings A and B case-insensitively."
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
  "For target TARGET, add BUF as a chat buffer."
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
           ((and circe-new-buffer-behavior-ignore-auto-joins
                 (member-ignore-case target circe-auto-joins))
            (setq circe-auto-joins
                  (remove (downcase target) circe-auto-joins)))
           ((eq circe-new-buffer-behavior 'display)
            (display-buffer buf))
           ((eq circe-new-buffer-behavior 'switch)
            (switch-to-buffer buf)))
          buf))
       (t
        nil)))))

(defun circe-chat-buffers ()
  "Return a list of all chat buffer of the current server."
  (let ((hash (with-circe-server-buffer
                circe-server-chat-buffers))
        (result nil))
    (when hash
      (maphash (lambda (key value)
                 (setq result (cons value result)))
               hash))
    result))

(defun circe-channel-buffers ()
  "Return a list of all channel buffers of the current server."
  (let ((buffers nil))
    (dolist (buf (circe-chat-buffers))
      (with-current-buffer buf
        (when (eq major-mode 'circe-channel-mode)
          (setq buffers (cons buf buffers)))))
    buffers))

;;;;;;;;;;;;;;;;;;;;
;;; Chat Buffers ;;;
;;;;;;;;;;;;;;;;;;;;

(defvar circe-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map circe-base-mode-map)
    map)
  "Base key map for all Circe chat buffers (channel, query).")

;; Defined here as we use it, but do not necessarily want to use the
;; full module.
(defvar lui-logging-format-arguments nil
  "A list of arguments to be passed to `lui-format'.
This can be used to extend the formatting possibilities of the
file name for lui applications.")
(make-variable-buffer-local 'lui-logging-format-arguments)

(defvar circe-chat-mode-hook nil
  "The hook run after `circe-chat-mode' is initialized.")

(defun circe-chat-mode (target server-buffer)
  "The circe chat major mode.
This is the common mode used for both queries and channels.
It should not be used directly.
TARGET is the default target to send data to.
SERVER-BUFFER is the server buffer of this chat buffer."
  (lui-mode)
  (add-hook 'lui-pre-output-hook 'circe-highlight-nick
            nil t)
  (setq major-mode 'circe-chat-mode
        mode-name "Circe Chat"
        lui-input-function 'circe-chat-input
        circe-chat-target target
        circe-server-buffer server-buffer
        default-directory (expand-file-name circe-default-directory))
  (set (make-local-variable 'tracking-faces-priorities)
       circe-track-faces-priorities)
  (add-hook 'completion-at-point-functions 'circe-completion-at-point
            nil t)
  (when circe-use-cycle-completion
    (set (make-local-variable 'completion-cycle-threshold)
         t))
  ;; Tab completion should be case-insensitive
  (set (make-local-variable 'completion-ignore-case)
       t)
  (setq flyspell-generic-check-word-p 'circe-flyspell-check-word-p)
  (lui-set-prompt circe-prompt-string)
  (goto-char (point-max))
  (let ((network (with-circe-server-buffer
                   circe-server-network)))

    (make-local-variable 'mode-line-buffer-identification)
    (setq mode-line-buffer-identification
          (list (format "%%b@%-8s" network)))
    (setq lui-logging-format-arguments
          `(:target ,target :network ,network)))
  (run-hooks 'circe-chat-mode-hook))

(defun circe-chat-disconnected ()
  "The current buffer got disconnected."
  (circe-server-message "Disconnected"))

(defun circe-chat-input (str)
  "Process STR as input."
  (set-text-properties 0 (length str) nil str)
  (cond
   ((string= str "")
    nil)
   ;; Ignore commands in multiline input
   ((and (not (string-match "\n" str))
         (string-match "\\`/\\([^/ ][^ ]*\\|[^/ ]*\\) ?\\([^\n]*\\)\\'" str))
    (let* ((command (match-string 1 str))
           (args (match-string 2 str))
           (handler (intern-soft (format "circe-command-%s"
                                         (upcase command)))))
      (cond
       ((string= command "")
        (circe-command-SAY args))
       (handler
        (funcall handler args))
       (circe-server-send-unknown-command-p
        (irc-send-raw (circe-server-process)
                      (format "%s %s"
                              (upcase command)
                              args)))
       (t
        (circe-server-message (format "Unknown command: %s"
                                      command))))))
   (t
    (mapc #'circe-command-SAY
          (circe-list-drop-right (split-string str "\n")
                                 "^ *$")))))

(defun circe-list-drop-right (list pattern)
  "Drop elements from the right of LIST that match PATTERN.

LIST should be a list of strings, and PATTERN is used as a
regular expression."
  (let ((list (reverse list)))
    (while (and list
                (string-match pattern (car list)))
      (setq list (cdr list)))
    (nreverse list)))

(defun circe-flyspell-check-word-p ()
  "Return a true value if flyspell check the word before point.

This is a suitable value for `flyspell-generic-check-word-p'. It
will also call `lui-flyspell-check-word-p'."
  (cond
   ((not (lui-flyspell-check-word-p))
    nil)
   ((circe-channel-user-p (circe-nick-before-point))
    nil)
   (t
    t)))

(defun circe-nick-before-point ()
  "Return the IRC nick before point"
  (with-syntax-table circe-nick-syntax-table
    (let (beg end)
      (save-excursion
        (forward-word -1)
        (setq beg (point))
        (forward-word 1)
        (setq end (point)))
      (buffer-substring beg end))))

;;;;;;;;;;;;;;;;
;;; Channels ;;;
;;;;;;;;;;;;;;;;

(defvar circe-channel-mode-hook nil
  "Hook run when channel mode is activated.")

(defvar circe-channel-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map circe-chat-mode-map)
    (define-key map (kbd "C-c C-n") 'circe-command-NAMES)
    (define-key map (kbd "C-c C-t") 'circe-command-CHTOPIC)
    map)
  "The key map for channel mode buffers.")

(defun circe-channel-mode (target server-buffer)
  "The circe channel chat major mode.
This mode represents a channel you are talking in.

TARGET is the default target to send data to.
SERVER-BUFFER is the server buffer of this chat buffer.

\\{circe-channel-mode-map}"
  (circe-chat-mode target server-buffer)
  (setq major-mode 'circe-channel-mode
        mode-name "Circe Channel")
  (use-local-map circe-channel-mode-map)
  (run-hooks 'circe-channel-mode-hook))

(defun circe-channel-killed ()
  "Called when the channel buffer got killed.

If we are not on the channel being killed, do nothing. Otherwise,
if the server is live, and the user wants to kill the buffer,
send PART to the server and clean up the channel's remaining
state."
  (when (buffer-live-p circe-server-buffer)
    (when (and circe-channel-killed-confirmation
               (not (y-or-n-p "Really leave this channel? ")))
      (error "Channel not left."))
    (when (circe-channel-user-p (circe-server-nick))
      (ignore-errors
        (irc-send-PART (circe-server-process)
                       circe-chat-target
                       circe-default-part-message))))
    (circe-server-remove-chat-buffer circe-chat-target))

(defvar circe-channel-receiving-names nil
  "A hash when we're currently receving a NAMES list. nil if not.")
(make-variable-buffer-local 'circe-server-receiving-names)

(defun circe-channel-message-handler (nick user host command args)
  "Update the users of a channel as appropriate.

This handles nick changes and channel joins and departures, and
updates the variable `circe-channel-users' accordingly.

NICK, USER, HOST, COMMAND and ARGS should be the command
received."
  (cond
   ((string= command "NICK")
    (dolist (buf (circe-chat-buffers))
      (with-current-buffer buf
        (when (and (eq major-mode 'circe-channel-mode)
                   (circe-channel-user-p nick))
          (circe-channel-rename-user nick (car args)))
        (when (and (eq major-mode 'circe-query-mode)
                   (circe-case-fold-string= nick
                                            circe-chat-target))
          (setq circe-chat-target (car args))
          (rename-buffer circe-chat-target t)
          (with-circe-server-buffer
            (circe-server-remove-chat-buffer nick)
            (circe-server-add-chat-buffer (car args)
                                          buf))))))
   ((string= command "QUIT")
    (dolist (buf (circe-chat-buffers))
      (with-current-buffer buf
        (when (and (eq major-mode 'circe-channel-mode)
                   (circe-channel-user-p nick))
          (circe-channel-remove-user nick)))))
   ((string= command "JOIN")
    (with-circe-chat-buffer (car args)
      (circe-channel-add-user nick)))
   ((string= command "PART")
    (with-circe-chat-buffer (car args)
      (circe-channel-remove-user nick)))
   ((string= command "KICK")
    (with-circe-chat-buffer (car args)
      (circe-channel-remove-user (cadr args))))
   ((string= command "353")             ; RPL_NAMREPLY
    (with-circe-chat-buffer (nth 2 args)
      (when (not circe-channel-receiving-names)
        (setq circe-channel-receiving-names (circe-case-fold-table)))
      (dolist (nick (circe-channel-parse-names (nth 3 args)))
        (puthash nick t circe-channel-receiving-names))))
   ((string= command "366")             ; RPL_ENDOFNAMES
    (with-circe-chat-buffer (nth 1 args)
      (circe-channel-users-synchronize circe-channel-receiving-names)
      (setq circe-channel-receiving-names nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Channel user management ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar circe-channel-users nil
  "A hash table of channel users.")
(make-variable-buffer-local 'circe-channel-users)

(defvar circe-channel-recent-users nil
  "Per-channel hash-table of recently parted/quit users.

This list is regularly cleaned up, see
`circe-channel-recent-users-timeout'.")
(make-variable-buffer-local 'circe-channel-recent-users)

(defvar circe-channel-users-synchronized nil
  "True if we have synchronized with channel users.")
(make-variable-buffer-local 'circe-channel-users-synchronized)

(defun circe-channel-add-user (nick)
  "Add NICK as a channel user."
  (when (not circe-channel-users)
    (setq circe-channel-users (circe-case-fold-table)))
  (circe-channel-expire-recent-users)
  (let ((data (or (gethash nick circe-channel-recent-users)
                  (make-hash-table))))
    (puthash 'joined (float-time) data)
    (remhash 'seen-on-initial-names data)
    (puthash nick data circe-channel-users))
  (remhash nick circe-channel-recent-users))

(defun circe-channel-remove-user (nick)
  "Remove NICK as a channel user."
  (when circe-channel-users
    (if (circe-server-my-nick-p nick)
        (setq circe-channel-users nil
              circe-channel-recent-users nil
              circe-channel-users-synchronized nil)
      (circe-channel-expire-recent-users)
      (let ((data (gethash nick circe-channel-users)))
        (when data
          (puthash 'departed (float-time) data)
          (puthash nick data circe-channel-recent-users)))
      (remhash nick circe-channel-users))))

(defun circe-channel-rename-user (old new)
  "Rename the user from nick OLD to NEW."
  (when circe-channel-users
    (circe-channel-expire-recent-users)
    (let (new-data)
      (cond
       ;; The new nick has recently left, assume this user returned
       ;; with a new nick and just got the old one back.
       ((circe-channel-user-nick-regain-p old new)
        (setq new-data (gethash new circe-channel-recent-users))
        (remhash new circe-channel-recent-users)
        (remhash 'departure new-data)
        ;; But they might have spoken with the new nick already
        (let ((new-last-active (gethash 'last-active new-data))
              (old-last-active (circe-channel-user-info old 'last-active)))
          (when (or (and new-last-active
                         old-last-active
                         (> old-last-active new-last-active))
                    (and (not new-last-active)
                         old-last-active))
            (puthash 'last-active old-last-active new-data))))
       ;; The new nick is not known, so just copy the old data
       ((gethash old circe-channel-users)
        (setq new-data (gethash old circe-channel-users)))
       ;; This shouldn't happen
       (t
        (error "Renaming %S to %S: Not found in current users?" old new)))
      ;; Now put the new value into the channel users table
      (remhash old circe-channel-users)
      (puthash new new-data circe-channel-users))))

(defun circe-channel-user-nick-regain-p (old new)
  "Return true if a nick change from OLD to NEW constitutes a nick regain.

Currently, this uses only the existence of OLD in the recent
users, which is a pretty rough heuristic, but it works."
  (and (circe-channel-user-p old)
       (circe-channel-recent-user-p new)))

(defun circe-channel-expire-recent-users ()
  "Clean up old users from the recent users table.

Any user who has a departure-time older than
`circe-channel-recent-users-timeout' will be removed from the table."
  (when (not circe-channel-recent-users)
    (setq circe-channel-recent-users (circe-case-fold-table)))
  (when circe-channel-recent-users-timeout
    (maphash (lambda (nick data)
               (when (> (- (float-time)
                           (gethash 'departed data 0))
                        circe-channel-recent-users-timeout)
                 (remhash nick circe-channel-recent-users)))
             circe-channel-recent-users)))

(defun circe-channel-user-p (nick)
  "Return non-nil when NICK belongs to a channel user."
  (cond
   (circe-channel-users
    (gethash nick circe-channel-users))
   ((eq major-mode 'circe-query-mode)
    (circe-case-fold-string= nick circe-chat-target))))

(defun circe-channel-recent-user-p (nick)
  "Return non-nil when NICK belongs to a user who recently left."
  (when circe-channel-recent-users
    (gethash nick circe-channel-recent-users)))

(defun circe-channel-user-info (nick option &optional default)
  "Return option OPTION for the nick NICK in the current channel.

Return DEFAULT if no option is set or the nick is not known."
  (if (not circe-channel-users)
      default
    (let ((table (gethash nick circe-channel-users nil)))
      (if (not table)
          default
        (gethash option table default)))))

(defun circe-channel-recent-user-info (nick option &optional default)
  "Return option OPTION for the nick NICK in the current channel.

Return DEFAULT if no option is set or the nick is not known."
  (if (not circe-channel-recent-users)
      default
    (let ((table (gethash nick circe-channel-recent-users nil)))
      (if (not table)
          default
        (gethash option table default)))))

(defun circe-channel-user-set-info (nick option value)
  "Set option OPTION to VALUE for this NICK in the current channel"
  (when circe-channel-users
    (let ((table (gethash nick circe-channel-users nil)))
      (when table
        (puthash option value table)))))

(defun circe-channel-nicks ()
  "Return a list of nicks in the current channel."
  (let ((nicks nil))
    (maphash (lambda (nick options)
               (setq nicks (cons nick nicks)))
             circe-channel-users)
    nicks))

(defun circe-channel-parse-names (name-string)
  "Parse the NAMES reply in NAME-STRING."
  (with-circe-server-buffer
    (mapcar (lambda (nick)
              (irc-nick-without-prefix circe-server-process nick))
            (split-string name-string))))

(defun circe-user-channels (nick)
  "Return a list of channels for the user named NICK."
  (let ((result nil))
   (dolist (buf (circe-chat-buffers))
     (when (with-current-buffer buf
             (circe-channel-user-p nick))
       (setq result (cons buf result))))
   result))

(defun circe-channel-users-synchronize (new-users)
  "Synchronize the channel user list with a current known state.

NEW is a hash table with currently active nicks in the channel.
This will ensure they are all in the `circe-channel-users' table,
and no other nicks are."
  (when (null circe-channel-users)
    (setq circe-channel-users (circe-case-fold-table)))
  (maphash (lambda (nick ignored)
             (when (not (gethash nick circe-channel-users))
               (circe-channel-add-user nick)
               (when (not circe-channel-users-synchronized)
                 (circe-channel-user-set-info nick 'seen-on-initial-names t))))
           new-users)
  (maphash (lambda (nick ignored)
             (when (not (gethash nick new-users))
               (circe-channel-remove-user nick)))
           circe-channel-users)
  (setq circe-channel-users-synchronized t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Join/Part Spam Protection

(defun circe-lurker-p (nick)
  "Return a true value if this nick is regarded inactive."
  (let ((last-active (circe-channel-user-info nick 'last-active)))
    (cond
     ;; If we do not track lurkers, no one is ever a lurker.
     ((not circe-reduce-lurker-spam)
      nil)
     ;; We ourselves are never lurkers (in this sense).
     ((circe-server-my-nick-p nick)
      nil)
     ;; Someone who isn't even on the channel (e.g. NickServ) isn't a
     ;; lurker, either.
     ((not (circe-channel-user-p nick))
      nil)
     ;; If someone has never been active, they most definitely *are* a
     ;; lurker.
     ((not last-active)
      t)
     ;; But if someone has been active, and we mark active users
     ;; inactive again after a timeout ...
     (circe-active-users-timeout
      ;; They are still lurkers if their activity has been too long
      ;; ago.
      (> (- (float-time)
            last-active)
         circe-active-users-timeout))
     ;; Otherwise, they have been active and we don't mark active
     ;; users inactive again, so nope, not a lurker.
     (t
      nil))))

(defun circe-lurker-display-active (nick user host)
  "Show that this user is active if they are a lurker."
  (when (and (circe-lurker-p nick)
             ;; If we saw them when we joined the channel, no need to
             ;; say "they're suddenly active!!111one".
             (not (circe-channel-user-info nick 'seen-on-initial-names)))
    (let ((joined (circe-channel-user-info nick 'joined)))
      (circe-display 'circe-format-server-lurker-activity
                     :nick nick
                     :user user
                     :host host
                     :jointime joined
                     :joindelta (circe-duration-string
                                 (- (float-time)
                                    joined))))))



;;;;;;;;;;;;;;;
;;; Queries ;;;
;;;;;;;;;;;;;;;

(defvar circe-query-mode-hook nil
  "Hook run when query mode is activated.")

(defvar circe-query-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map circe-chat-mode-map)
    map)
  "The key map for query mode buffers.")

(defun circe-query-mode (target server-buffer)
  "The circe query chat major mode.
This mode represents a query you are talking in.

TARGET is the default target to send data to.
SERVER-BUFFER is the server buffer of this chat buffer.

\\{circe-query-mode-map}"
  (circe-chat-mode target server-buffer)
  (setq major-mode 'circe-query-mode
        mode-name "Circe Query")
  (use-local-map circe-channel-mode-map)
  (run-hooks 'circe-query-mode-hook))

(defun circe-query-killed ()
  "Called when the query buffer got killed."
  (when (buffer-live-p circe-server-buffer)
    (circe-server-remove-chat-buffer circe-chat-target)))

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
    (dolist (buf (circe-chat-buffers))
      (with-current-buffer buf
        (when (eq major-mode 'circe-query-mode)
          (setq num (+ num 1)))))
    num))

;;;;;;;;;;;;;;;;;;
;;; Completion ;;;
;;;;;;;;;;;;;;;;;;

(defun circe-completion-at-point ()
  "Return a list of possible completions for the current buffer.
This is used in `completion-at-point-functions'."
  (let ((start (make-marker))
        (end (make-marker)))
    (set-marker end (point))
    (set-marker start
                (save-excursion
                  (when (or (looking-back circe-completion-suffix)
                            (looking-back " "))
                    (goto-char (match-beginning 0)))
                  (cond
                   ((<= (point) lui-input-marker)
                    lui-input-marker)
                   ((re-search-backward "\\s-" lui-input-marker t)
                    (1+ (point)))
                   (t
                    lui-input-marker))))
    (list start end 'circe-completion-table)))

(defun circe-completion-table (string pred action)
  "Completion table to use for Circe buffers.

See `minibuffer-completion-table' for details."
  (cond
   ;; Best completion of STRING
   ((eq action nil)
    (try-completion string
                    (circe-completion-candidates
                     (if (= (- (point) (length string))
                            lui-input-marker)
                         circe-completion-suffix
                       " "))
                    pred))
   ;; A list of possible completions of STRING
   ((eq action t)
    (all-completions string
                     (circe-completion-candidates
                      (if (= (- (point) (length string))
                             lui-input-marker)
                          circe-completion-suffix
                        " "))
                     pred))
   ;; t iff STRING is a valid completion as it stands
   ((eq action 'lambda)
    (test-completion string
                     (circe-completion-candidates
                      (if (= (- (point) (length string))
                             lui-input-marker)
                          circe-completion-suffix
                        " "))
                     pred))
   ;; Boundaries
   ((eq (car-safe action) 'boundaries)
    `(boundaries 0 . ,(length (cdr action))))
   ;; Metadata
   ((eq action 'metadata)
    '(metadata (cycle-sort-function . circe-completion-sort)))))

(defun circe-completion-clean-nick (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-max))
    (when (or (looking-back circe-completion-suffix)
              (looking-back " "))
      (replace-match ""))
    (buffer-string)))

(defun circe-completion-sort (collection)
  "Sort the COLLECTION by channel activity for nicks."
  (let* ((decorated (mapcar (lambda (entry)
                              (list (circe-channel-user-info
                                     (circe-completion-clean-nick entry)
                                     'last-active)
                                    (length entry)
                                    entry))
                            collection))
         (sorted (sort decorated
                       (lambda (a b)
                         (cond
                          ((and (car a)
                                (car b))
                           (> (car a)
                              (car b)))
                          ((and (not (car a))
                                (not (car b)))
                           (< (cadr a)
                              (cadr b)))
                          ((car a)
                           t)
                          (t
                           nil))))))
    (mapcar (lambda (entry)
              (nth 2 entry))
            sorted)))


;; If we switch Circe to lexical scoping, we can replace this with
;; something more sane.
(defvar circe-completion-old-completion-all-sorted-completions nil
  "Variable to know if we can return a cached result.")
(make-variable-buffer-local
 'circe-completion-old-completion-all-sorted-completions)
(defvar circe-completion-cache nil
  "The results we can cache.")
(make-variable-buffer-local 'circe-completion-cache)

(defun circe-completion-candidates (nick-suffix)
  (if (and circe-completion-old-completion-all-sorted-completions
           (eq completion-all-sorted-completions
               circe-completion-old-completion-all-sorted-completions))
      circe-completion-cache
    (let ((completions (append (circe-commands-list)
                               (mapcar (lambda (buf)
                                         (with-current-buffer buf
                                           circe-chat-target))
                                       (circe-channel-buffers)))))
      (cond
       ;; In a server buffer, complete all nicks in all channels
       ((eq major-mode 'circe-server-mode)
        (dolist (buf (circe-channel-buffers))
          (with-current-buffer buf
            (dolist (nick (circe-channel-nicks))
              (setq completions (cons (concat nick nick-suffix)
                                      completions))))))
       ;; In a channel buffer, only complete nicks in this channel
       ((eq major-mode 'circe-channel-mode)
        (setq completions (append (delete (concat (circe-server-nick)
                                                  nick-suffix)
                                          (mapcar (lambda (nick)
                                                    (concat nick nick-suffix))
                                                  (circe-channel-nicks)))
                                  completions)))
       ;; In a query buffer, only complete this query partner
       ((eq major-mode 'circe-query-mode)
        (setq completions (cons (concat circe-chat-target nick-suffix)
                                completions)))
       ;; Else, we're doing something wrong
       (t
        (error "`circe-possible-completions' called outside of Circe")))
      (setq circe-completion-old-completion-all-sorted-completions
            completion-all-sorted-completions
            circe-completion-cache completions)
      completions)))

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

(defun circe-command-HELP (&optional ignored)
  "Display a list of recognized commands, nicely formatted."
  (circe-server-message
   (concat "Recognized commands are: "
	   (mapconcat (lambda (s) s) (circe-commands-list) ""))))

(defun circe-command-SAY (line)
  "Say LINE to the current target."
  (interactive "sSay: ")
  (if (not circe-chat-target)
      (circe-server-message "No target for current buffer")
    (dolist (line (circe-split-line line))
      (circe-display 'circe-format-self-say
                     :body line
                     :nick (circe-server-nick))
      (irc-send-PRIVMSG (circe-server-process)
                        circe-chat-target
                        ;; Some IRC servers give an error if there is
                        ;; no text at all.
                        (if (string= line "")
                            " "
                          line)))))

(defun circe-split-line (longline)
  "Splits LONGLINE into smaller components.

IRC silently truncates long lines. This splits a long line into
parts that each are not longer than `circe-split-line-length'."
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
  "Send LINE to IRC as an action."
  (interactive "sAction: ")
  (if (not circe-chat-target)
      (circe-server-message "No target for current buffer")
    (circe-display 'circe-format-self-action
                   :body line
                   :nick (circe-server-nick))
    (irc-send-ctcp (circe-server-process)
                   circe-chat-target
                   "ACTION" line)))

(defun circe-command-MSG (who &optional what)
  "Send a message.

Send WHO a message containing WHAT.

If WHAT is not given, WHO should contain both the nick and the
message separated by a space."
  (when (not what)
    (if (string-match "^\\([^ ]*\\) \\(.*\\)" who)
        (setq what (match-string 2 who)
              who (match-string 1 who))
      (circe-server-message "Usage: /MSG <who> <what>")))
  (when what
    (let ((buf (circe-server-auto-query-buffer who)))
      (if buf
          (with-current-buffer buf
            (circe-command-SAY what)
            (ring-insert lui-input-ring what) ; Horrible breakage of abstraction!
            )
        (with-current-buffer (circe-server-last-active-buffer)
          (irc-send-PRIVMSG (circe-server-process)
                            who what)
          (circe-display 'circe-format-self-message
                         :target who
                         :body what))))))

(defun circe-command-QUERY (who)
  "Open a query with WHO."
  (interactive "sQuery with: ")
  (let ((circe-new-buffer-behavior 'ignore)
        (who (string-trim who)))
    (pop-to-buffer
     (circe-server-get-chat-buffer who 'circe-query-mode))))

(defun circe-command-JOIN (channel)
  "Join CHANNEL. This can also contain a key."
  (interactive "sChannel: ")
  (let ((circe-new-buffer-behavior 'ignore)
        (channel (string-trim channel)))
    (pop-to-buffer
     (circe-server-get-chat-buffer channel 'circe-channel-mode))
    (irc-send-JOIN (circe-server-process) channel)))

(defun circe-command-PART (reason)
  "Part the current channel because of REASON."
  (interactive "sReason: ")
  (if (not circe-chat-target)
      (circe-server-message "No target for current buffer")
    (irc-send-PART (circe-server-process)
                   circe-chat-target
                   (if (equal "" reason)
                       circe-default-part-message
                     reason))))

(defun circe-command-WHOIS (whom)
  "Request WHOIS information about WHOM."
  (interactive "sWhois: ")
  (let* ((whom-server-name (split-string whom))
         (whom (car whom-server-name))
         (server-or-name (cadr whom-server-name)))
    (irc-send-WHOIS (circe-server-process) whom server-or-name)))

(defun circe-command-WHOWAS (whom)
  "Request WHOWAS information about WHOM."
  (interactive "sWhois: ")
  (let ((whom (string-trim whom)))
    (irc-send-WHOWAS (circe-server-process) whom)))

(defun circe-command-WHOAMI (&optional ignored)
  "Request WHOIS information about yourself.

Arguments are IGNORED."
  (interactive "sWhois: ")
  (irc-send-WHOIS (circe-server-process)
                  (circe-server-nick)))

(defun circe-command-NICK (newnick)
  "Change nick to NEWNICK."
  (interactive "sNew nick: ")
  (let ((newnick (string-trim newnick)))
    (irc-send-NICK (circe-server-process) newnick)))

(defun circe-command-NAMES (&optional channel)
  "List the names of the current channel or CHANNEL."
  (interactive)
  (let ((target (when channel
                  (string-trim channel))))
    (when (or (not target)
              (equal target ""))
      (setq target circe-chat-target))
    (if (not target)
        (circe-server-message "No target for current buffer")
      (irc-send-NAMES (circe-server-process)
                      target))))

(defun circe-command-PING (target)
  "Send a CTCP PING request to TARGET."
  (interactive "sWho: ")
  (let ((target (string-trim target)))
    (irc-send-ctcp (circe-server-process)
                   target
                   "PING" (format "%s" (float-time)))))

(defun circe-command-QUOTE (line)
  "Send LINE verbatim to the server."
  (interactive "Line: ")
  (irc-send-raw (circe-server-process) line)
  (with-current-buffer (circe-server-last-active-buffer)
    (circe-server-message (format "Sent to server: %s"
                                  line))))

(defun circe-command-CTCP (who &optional command argument)
  "Send a CTCP message to WHO containing COMMAND with ARGUMENT.
If COMMAND is not given, WHO is parsed to contain all of who,
command and argument.
If ARGUMENT is nil, it is interpreted as no argument."
  (when (not command)
    (if (string-match "^\\([^ ]*\\) *\\([^ ]*\\) *\\(.*\\)" who)
        (setq command (upcase (match-string 2 who))
              argument (match-string 3 who)
              who (match-string 1 who))
      (circe-server-message "Usage: /CTCP <who> <what>")))
  (when (not (string= "" command))
    (irc-send-ctcp (circe-server-process)
                   who
                   command
                   (if (and argument (not (equal argument "")))
                       argument
                     nil))))

(defun circe-command-AWAY (reason)
  "Set yourself away with REASON."
  (interactive "sReason: ")
  (irc-send-AWAY (circe-server-process) reason))

(defun circe-command-BACK (&optional ignored)
  "Mark yourself not away anymore.

Arguments are IGNORED."
  (interactive)
  (irc-send-AWAY (circe-server-process)))

(defun circe-command-QUIT (reason)
  "Quit the current server giving REASON."
  (interactive "sReason: ")
  (with-circe-server-buffer
    (setq circe-server-quitting-p t)
    (irc-send-QUIT (circe-server-process)
                   (if (equal "" reason)
                       circe-default-quit-message
                     reason))))

(defun circe-command-GAWAY (reason)
  "Set yourself away on all servers with reason REASON."
  (interactive "sReason: ")
  (dolist (buf (circe-server-buffers))
    (with-current-buffer buf
      (when (eq (process-status circe-server-process)
                'open)
        (irc-send-AWAY circe-server-process reason)))))

(defun circe-command-GQUIT (reason)
  "Quit all servers with reason REASON."
  (interactive "sReason: ")
  (dolist (buf (circe-server-buffers))
    (with-current-buffer buf
      (when (eq (process-status circe-server-process)
                'open)
        (irc-send-QUIT circe-server-process reason)))))

(defun circe-command-INVITE (nick &optional channel)
  "Invite NICK to CHANNEL.
When CHANNEL is not given, NICK is assumed to be a string
consisting of two words, the nick and the channel."
  (interactive "sInvite who: \nsWhere: ")
  (when (not channel)
    (if (string-match "^\\([^ ]+\\) +\\([^ ]+\\)" nick)
        (setq channel (match-string 2 nick)
              nick (match-string 1 nick))
      (when (or (string= "" nick) (null nick))
        (circe-server-message "Usage: /INVITE <nick> <channel>"))))
  (irc-send-INVITE (circe-server-process)
                   nick
                   (if (and (null channel)
                            (not (null nick)))
                       circe-chat-target
                     channel)))

(defun circe-command-SV (&optional ignored)
  "Tell the current channel about your client and Emacs version.

Arguments are IGNORED."
  (interactive)
  (circe-command-SAY (format (concat "I'm using Circe version %s "
                                     "with %s %s (of %s)")
                             circe-version
                             "GNU Emacs"
                             emacs-version
                             (format-time-string "%Y-%m-%d"
                                                 emacs-build-time))))

(defun circe-command-CLEAR (&optional ignored)
  "Delete all buffer contents before the lui prompt."
  (let ((inhibit-read-only t))
    (delete-region (point-min) lui-output-marker)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IRC Protocol Handling ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun circe-irc-handler-table ()
  (let ((table (irc-handler-table)))
    (irc-handler-add table nil #'circe-irc-legacy-bridge)
    (irc-handler-add table "conn.disconnected" #'circe-irc-conn-disconnected)
    (irc-handle-registration table)
    (irc-handle-ping-pong table)
    (irc-handle-isupport table)
    (irc-handle-current-nick-tracking table)
    (irc-handle-initial-nick-acquisition table)
    (irc-handle-ctcp table)
    table))

(defun circe-irc-conn-disconnected (conn event)
  (with-current-buffer (irc-connection-get conn :server-buffer)
    (dolist (buf (circe-chat-buffers))
      (with-current-buffer buf
        (circe-chat-disconnected)))

    (when (not circe-server-quitting-p)
      (circe-reconnect))

    (setq circe-server-quitting-p nil)))

(defun circe-irc-legacy-bridge (conn event &optional sender &rest args)
  (let ((case-fold-search nil))
    (cond
     ;; Ignore PRIVMSG and NOTICE
     ((member event '("PRIVMSG" "NOTICE"))
      (setq event nil))
     ;; because we handle the irc events directly
     ((equal event "irc.message")
      (setq event "PRIVMSG"))
     ((equal event "irc.notice")
      (setq event "NOTICE"))
     ((equal event "irc.ctcp")
      (setq event (format "CTCP-%s" (elt args 1))
            args (list (elt args 0)
                       (or (elt args 2)
                           ""))))
     ((equal event "irc.ctcpreply")
      (setq event (format "CTCP-%s-REPLY" (elt args 1))
            args (list (elt args 0)
                       (or (elt args 2)
                           "")))))
    (when (and event
               (string-match "\\`[0-9][0-9][0-9]\\'\\|\\`[A-Z-]+\\'" event))
      (let* ((sender (circe-parse-sender (or sender "")))
             (nick (elt sender 0))
             (user (elt sender 1))
             (host (elt sender 2))
             (command event))
        (with-current-buffer (irc-connection-get conn :server-buffer)
          (unwind-protect
              (let* ((circe-current-message (list nick user host command args))
                     (circe-message-option-cache (make-hash-table
                                                  :test 'equal)))
                (circe-server-handle-message nick user host
                                             command args)
                (when (not (circe-message-option 'dont-display))
                  (circe-server-display-message nick user host
                                                command args)))
            (circe-server-handle-message-internal nick user host
                                                  command args)))))))

(defun circe-parse-sender (sender)
  (if (string-match "\\`\\([^!]*\\)!\\([^@]*\\)@\\(.*\\)\\'" sender)
      (list (match-string 1 sender)
            (match-string 2 sender)
            (match-string 3 sender))
    (list sender nil nil)))

(defun circe-server-display-message (nick user host command args)
  "Display an IRC message.

NICK, USER and HOST specify the originator of COMMAND with ARGS
as arguments."
  (let ((display (circe-get-display-handler command)))
    (cond
     ;; Functions get called
     ((functionp display)
      (funcall display nick user host command args))
     ;; Lists describe patterns
     ((consp display)
      (let* ((target+name (circe-display-target (car display)
                                                nick user host
                                                command args))
             (target (car target+name))
             (name (cdr target+name))
             (format (nth 1 display))
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
                           :nick (or nick "(unknown)")
                           :user (or user "(unknown)")
                           :host (or host "(unknown)")
                           :origin origin
                           :command command
                           :target name
                           :indexed-args args)))))
     ;; No configured display handler, show a default
     (t
      (with-current-buffer (circe-server-last-active-buffer)
        (let ((target (if (circe-server-my-nick-p (car args))
                          ""
                        (format " to %s" (car args)))))
          (cond
           ((string-match "CTCP-\\(.*\\)-REPLY" command)
            (circe-server-message
             (format "CTCP %s reply from %s (%s@%s)%s: %s"
                     (match-string 1 command) nick user host target (cadr args))))
           ((string-match "CTCP-\\(.*\\)" command)
            (circe-server-message
             (format "Unknown CTCP request %s from %s (%s@%s)%s: %s"
                     (match-string 1 command) nick user host target (cadr args))))
           (t
            (circe-server-message
             (format "[%s from %s%s] %s"
                     command
                     nick
                     (if (or user host)
                         (format " (%s@%s)" user host)
                       "")
                     (mapconcat #'identity args " ")))))))))))

(defun circe-display-target (target nick user host command args)
  "Return the target buffer and name.
The buffer might be nil if it is not alive.

See `circe-set-display-handler' for a description of target.

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
  (cond
   ((eq target 'nick)
    (cons (circe-server-get-chat-buffer nick)
          nick))
   ((numberp target)
    (let ((name (nth target
                     args)))
      (cons (circe-server-get-chat-buffer name)
            name)))
   ((eq target 'active)
    (let ((buf (circe-server-last-active-buffer)))
      (cons buf
            (buffer-name buf))))
   ((eq target 'server)
    (cons (current-buffer) (buffer-name)))
   (t
    (error "Bad target in format string: %s" target))))

(defun circe-server-handle-message (nick user host command args)
  "Handle an IRC message.

Message handlers are meant to process IRC messages in a way that
primarily does not display anything, as to avoid multiple
displays for the same message. This allows for bookkeeping and
other such handlers.

Please note that message handlers are called even if the user is
ignored."
  (dolist (function (circe-get-message-handlers command))
    (funcall function nick user host command args))
  (run-hook-with-args 'circe-receive-message-functions
                      nick user host command args))

(defun circe-server-handle-message-internal (nick user host command args)
  "Handle this message for internal bookkeeping.

This does mandatory client-side bookkeeping of the server state.

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
  (cond
   ;; Quitting
   ((string= command "QUIT")
    (when (circe-server-my-nick-p nick)
      (dolist (buf (circe-chat-buffers))
        (with-current-buffer buf
          (circe-chat-disconnected)))))
   ;; Create new channel buffers
   ((string= command "JOIN")
    (when (circe-server-my-nick-p nick)
      (circe-server-add-chat-buffer
       (car args)
       (circe-server-get-chat-buffer (car args)
                                     'circe-channel-mode))))
   ;; Initialization
   ((string= command "001")             ; RPL_WELCOME
    (setq circe-server-reconnect-attempts 0)
    (run-hooks 'circe-server-connected-hook)))
  (circe-channel-message-handler nick user host command args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accessing Display Handlers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun circe-set-display-handler (command handler)
  "Set the display handler for COMMAND to HANDLER.

A handler is either a function or a list.

A function gets called in the server buffer with five arguments,
NICK, USER, HOST, COMMAND and ARGS, and is expected to display
this message however it wants.

Alternatively, the handler can be a list of two elements:

  target   - The target of this message
  format   - The format for this string

The target can be any of:

  'active  - The last active buffer of this server
  'nick    - The nick who sent this message
  'server  - The server buffer for this server
  number   - The index of the argument of the target

The format is passed to `lui-format'. Possible format string
substitutions are {mynick}, {target}, {nick}, {user}, {host},
{origin}, {command}, {target}, and indexed arguments for the
arguments to the IRC message."
  (when (not circe-display-table)
    (setq circe-display-table (make-hash-table :test 'equal)))
  (puthash command handler circe-display-table))

(defun circe-get-display-handler (command)
  "Return the display handler for COMMAND.

See `circe-set-display-handler' for more information."
  (when circe-display-table
    (gethash command circe-display-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accessing Message Handlers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun circe-add-message-handler (command handler)
  "Add HANDLER to the list of functions called on COMMAND.

Each HANDLER is called in a server buffer with five arguments,
NICK, USER, HOST, COMMAND and ARGS."
  (when (not circe-message-handler-table)
    (setq circe-message-handler-table (make-hash-table :test 'equal)))
  (when (not (member handler (circe-get-message-handlers command)))
    (puthash command
             (append (gethash command circe-message-handler-table)
                     (list handler))
             circe-message-handler-table)))

(defun circe-get-message-handlers (command)
  "Return the list of handler functions for COMMAND.

See `circe-add-message-handler' for more information."
  (when circe-message-handler-table
    (gethash command circe-message-handler-table)))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Message Options ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defvar circe-message-option-functions nil
  "A list of functions to call to get options for a message.

Each function receives five arguments: NICK, USER, HOST, COMMAND
and ARGS. It should return an alist mapping option symbols to
values.

Possible options:

  dont-reply
      Boolean. t if Circe should never reply to this (as for CTCP queries).

  dont-display
      Boolean. t if Circe should not display this message.

  hide
      Boolean. t if Circe should hide this message by default,
      but allow the user to show it on demand.

  text-properties
      Property list. Default text properties for this message.")

(defvar circe-message-option-cache nil
  "A caching table for the current message's options.

This should not be set globally.")

(defvar circe-current-message nil
  "The current message.

A list of NICK, USER, HOST, COMMAND and ARGS, or nil if no
current message.")

(defun circe-message-options ()
  "Return the table for the current message options."
  (when circe-current-message
    (let ((cached (gethash (current-buffer) circe-message-option-cache)))
      (when (not cached)
        (setq cached (make-hash-table :test 'equal))
        (puthash (current-buffer) cached circe-message-option-cache)
        (circe-message-options-update cached))
      cached)))

(defun circe-message-option (name)
  "Return the value of the option NAME, or nil if not set."
  (let ((table (circe-message-options)))
    (when table
      (gethash name table))))

(defun circe-message-options-update (table)
  "Add current message options to TABLE."
  (dolist (fun circe-message-option-functions)
    (dolist (elt (apply fun circe-current-message))
      (let ((key (car elt))
            (value (cdr elt)))
        (if (not (eq key 'text-properties))
            (puthash key value table)
          (puthash 'text-properties
                   (circe-merge-text-properties
                    (gethash 'text-properties table)
                    value)
                   table))))))

(defun circe-merge-text-properties (plist1 plist2)
  "Merge two property lists and return the merged list.

PLIST1 should have priority over PLIST2, except for faces, where
the two face lists are merged."
  (let ((new (append plist1 nil))) ; Copy list
    (while plist2
      (if (not (eq 'face (car plist2)))
          (setq new (plist-put new (car plist2) (cadr plist2)))
        ;; Faces should be merged, not overridden.
        (let* ((face1 (plist-get new 'face))
               (face2 (cadr plist2))
               new-face)
          (cond
           ((not face1)
            (setq new-face face2))
           ((not face2)
            (setq new-face face1))
           (t
            (setq new-face (append (if (consp face1)
                                       face1
                                     (list face1))
                                   (if (consp face2)
                                       face2
                                     (list face2))))))
          (setq new (plist-put new 'face new-face))))
      (setq plist2 (cddr plist2)))
    new))

(add-hook 'circe-message-option-functions 'circe-message-option-ignored)
(defun circe-message-option-ignored (nick user host command args)
  "Return appropriate properties when a user is ignored."
  (when (circe-ignored-p nick user host command args)
    '((dont-reply . t)
      (dont-display . t))))

(add-hook 'circe-message-option-functions 'circe-message-option-fool)
(defun circe-message-option-fool (nick user host command args)
  "Return appropriate properties when a user should not be shown by default."
  (when (circe-fool-p nick user host command args)
    '((hide . t)
      (text-properties . (face circe-fool-face
                          lui-fool t)))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Ignore Handling ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun circe-ignore-matches-p (nick user host command args patterns)
  "Check if a given command does match an ignore pattern.

A pattern matches if it either matches the user NICK!USER@HOST,
or if COMMAND is a PRIVMSG and it matches the first word in the
argument text in ARGS.

PATTERNS should be the list of regular expressions."
  (let ((string (concat nick "!" user "@" host))
        (target (when (and (string= command "PRIVMSG")
                           (not (circe-server-my-nick-p (car args)))
                           (string-match "^\\([^ ]*\\)[:, ]" (cadr args)))
                  (match-string 1 (cadr args)))))
    (catch 'return
      (dolist (regex patterns)
        (when (string-match regex string)
          (throw 'return t))
        (when (and (stringp target)
                   (string-match regex target))
          (throw 'return t)))
      nil)))

(defun circe-ignored-p (nick user host command args)
  "True if this user or message is being ignored.

See `circe-ignore-functions' and `circe-ignore-list'.

NICK, USER and HOST should be the sender of a the command
COMMAND, which had the arguments ARGS."
  (or (run-hook-with-args-until-success 'circe-ignore-functions
                                        nick user host
                                        command args)
      (circe-ignore-matches-p nick user host command args
                              circe-ignore-list)))

(defun circe-fool-p (nick user host command args)
  "True if this user or message is a fool.

See `circe-fool-list'.

NICK, USER and HOST should be the sender of a the command
COMMAND, which had the arguments ARGS."
  (circe-ignore-matches-p nick user host command args
                          circe-fool-list))

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
      (dolist (regex circe-ignore-list)
        (circe-server-message (format "- %s" regex)))))))

(defun circe-command-UNIGNORE (line)
  "Remove the entry LINE from `circe-ignore-list'."
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

(defun circe-command-FOOL (line)
  "Add the regex on LINE to the `circe-fool-list'."
  (with-current-buffer (circe-server-last-active-buffer)
    (cond
     ((string-match "\\S-+" line)
      (let ((regex (match-string 0 line)))
        (add-to-list 'circe-fool-list regex)
        (circe-server-message (format "Recognizing %s as a fool"
                                      regex))))
     ((not circe-fool-list)
      (circe-server-message "Your do not know any fools"))
     (t
      (circe-server-message "Your list of fools:")
      (dolist (regex circe-fool-list)
        (circe-server-message (format "- %s" regex)))))))

(defun circe-command-UNFOOL (line)
  "Remove the entry LINE from `circe-fool-list'."
  (with-current-buffer (circe-server-last-active-buffer)
    (cond
     ((string-match "\\S-+" line)
      (let ((regex (match-string 0 line)))
        (setq circe-fool-list (delete regex circe-fool-list))
        (circe-server-message (format "Assuming %s is not a fool anymore"
                                      regex))))
     (t
      (circe-server-message
       "No one is not a fool anymore? UNFOOL requires one argument")))))

;;;;;;;;;;;;;;;;;;;;;
;;; CTCP Handling ;;;
;;;;;;;;;;;;;;;;;;;;;

(defun circe-ctcp-display-general (nick user host command args)
  "Show a CTCP request that does not require special handling.

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
  (with-current-buffer (circe-server-last-active-buffer)
    (let ((ctcp (substring command 5))
          (target (if (circe-server-my-nick-p (car args))
                      ""
                    (format " to %s" (car args))))
          (argstring (if (equal (cadr args) "")
                         ""
                       (concat ": " (cadr args)))))
      (circe-server-message (format "CTCP %s request%s from %s (%s@%s)%s"
                                    ctcp target nick user host argstring)))))

(circe-set-display-handler "CTCP-ACTION" 'circe-ctcp-display-ACTION)
(defun circe-ctcp-display-ACTION (nick user host command args)
  "Show an ACTION.

NICK, USER and HOST are the originators, COMMAND the command and
ARGS the arguments to the command."
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
      (circe-lurker-display-active nick user host)
      (circe-channel-user-set-info nick 'last-active (float-time))
      (circe-display 'circe-format-action
                     :nick nick
                     :body (cadr args)))))

(circe-set-display-handler "CTCP-VERSION" 'circe-ctcp-display-general)
(circe-set-display-handler "CTCP-TIME" 'circe-ctcp-display-general)
(circe-set-display-handler "CTCP-CLIENTINFO" 'circe-ctcp-display-general)
(circe-set-display-handler "CTCP-SOURCE" 'circe-ctcp-display-general)

(circe-set-display-handler "CTCP-PING" 'circe-ctcp-display-PING)
(defun circe-ctcp-display-PING (nick user host command args)
  "Show a CTCP PING request.

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
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
  "Show a CTCP PING reply.

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
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

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display Handlers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(circe-set-display-handler "PING" 'circe-display-ignore)
(circe-set-display-handler "PONG" 'circe-display-ignore)
(circe-set-display-handler "CAP" 'circe-display-ignore)
(circe-set-display-handler "AUTHENTICATE" 'circe-display-ignore)
(defun circe-display-ignore (nick user host command args)
  "Don't show a this message.

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
  t)

(circe-set-display-handler "PRIVMSG" 'circe-display-PRIVMSG)
(defun circe-display-PRIVMSG (nick user host command args)
  "Show a PRIVMSG message.

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
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
      (circe-lurker-display-active nick user host)
      (circe-channel-user-set-info nick 'last-active (float-time))
      (circe-display 'circe-format-say
                     :nick nick
                     :body (cadr args))))))

(circe-set-display-handler "NOTICE" 'circe-display-NOTICE)
(defun circe-display-NOTICE (nick user host command args)
  "Show a NOTICE message.

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
  (let ((queryp (circe-server-my-nick-p (car args))))
    (if (and nick (not (string-match "\\." nick)))
        (with-current-buffer (or (circe-server-get-chat-buffer (if queryp
                                                                   nick
                                                                 (car args)))
                                 (circe-server-last-active-buffer))
          (when (eq major-mode 'circe-channel-mode)
            (circe-lurker-display-active nick user host)
            (circe-channel-user-set-info nick 'last-active (float-time)))
          (circe-display 'circe-format-notice
                         :nick nick
                         :body (cadr args)))
      (with-circe-server-buffer
        (circe-display 'circe-format-server-notice
                       :body (cadr args))))))

(circe-set-display-handler "NICK" 'circe-display-NICK)
(defun circe-display-NICK (nick user host command args)
  "Show a NICK message.

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
  (when (circe-server-my-nick-p nick)
    (with-circe-server-buffer
      (circe-server-message
       (format "Nick change: You are now known as %s"
               (car args)))))
  (dolist (buf (circe-user-channels nick))
    (with-current-buffer buf
      (cond
       ((circe-lurker-p nick)
        nil)
       ((circe-channel-user-nick-regain-p nick (car args))
        (circe-server-message
         (format "Nick re-gain: %s (%s@%s) is now known as %s"
                 nick user host (car args))))
       (t
        (circe-server-message
         (format "Nick change: %s (%s@%s) is now known as %s"
                 nick user host (car args))))))))

(circe-set-display-handler "MODE" 'circe-display-MODE)
(defun circe-display-MODE (nick user host command args)
  "Show a MODE message.

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
  (when (or circe-show-server-modes-p
            user) ; If this is set, it is not a server mode
    (with-current-buffer (or (circe-server-get-chat-buffer (car args))
                             circe-server-last-active-buffer
                             circe-server-buffer)
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

(circe-set-display-handler "PART" 'circe-display-PART)
(defun circe-display-PART (nick user host command args)
  "Show a PART message.

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
  (let ((buf (circe-server-get-chat-buffer (car args))))
    (when buf
      (with-current-buffer buf
        (when (not (circe-lurker-p nick))
          (circe-server-message
           (if (null (cdr args))
               (format "Part: %s (%s@%s)" nick user host)
             (format "Part: %s (%s@%s) - %s"
                     nick user host (cadr args)))))))))

(defun circe-duration-string (duration)
  "Return a description of a DURATION in seconds."
  (let ((parts `((,(* 12 30 24 60 60) "year")
                 (,(* 30 24 60 60) "month")
                 (,(* 24 60 60) "day")
                 (,(* 60 60) "hour")
                 (60 "minute")
                 (1 "second")))
        (duration (round duration))
        (result nil))
    (dolist (part parts)
      (let* ((seconds-per-part (car part))
             (description (cadr part))
             (count (/ duration seconds-per-part)))
        (when (not (zerop count))
          (setq result (cons (format "%d %s%s"
                                     count description
                                     (if (= count 1) "" "s"))
                             result)))
        (setq duration (- duration (* count seconds-per-part)))))
    (if result
        (mapconcat #'identity
                   (nreverse result)
                   " ")
      "a moment")))

(circe-set-display-handler "329" 'circe-display-329)
(defun circe-display-329 (nick user host command args)
  "Show a 329 numeric (topic set on...).

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
  (with-current-buffer (circe-server-get-chat-buffer (cadr args))
    (let ((time (string-to-number (nth 2 args))))
      (circe-server-message
       (format "Topic set on %s (%s ago)"
               (current-time-string (seconds-to-time time))
               (circe-duration-string (- (float-time)
                                         time)))))))

(circe-set-display-handler "333" 'circe-display-333)
(defun circe-display-333 (nick user host command args)
  "Show a 333 numeric (topic set by...).

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
  (with-current-buffer (or (circe-server-get-chat-buffer (cadr args))
                           (circe-server-last-active-buffer))
    (let* ((time (string-to-number (nth 3 args)))
           (timestring (if (> time 0)
                           (format "on %s (%s ago)"
                                   (current-time-string (seconds-to-time time))
                                   (circe-duration-string (- (float-time)
                                                             time)))
                         "at the beginning of time"))
           (prefix (if (circe-server-get-chat-buffer (cadr args))
                       ""
                     (format "[%s] " (cadr args)))))
      (circe-server-message
       (format "%sTopic set by %s %s"
               prefix
               (nth 2 args)
               timestring)))))

(circe-set-display-handler "317" 'circe-display-317)
(defun circe-display-317 (nick user host command args)
  "Show a 317 numeric (idle since).

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
  (with-current-buffer (circe-server-last-active-buffer)
    (let ((idle (string-to-number (nth 2 args)))
          (time (string-to-number (nth 3 args))))
      (circe-server-message
       (format "%s is %s idle (Signon on %s, %s ago)"
               (nth 1 args)
               (circe-duration-string idle)
               (current-time-string (seconds-to-time time))
               (circe-duration-string (- (float-time)
                                         time)))))))

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
      (dolist (entry circe-netsplit-list)
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
      nil)))

(defun circe-netsplit-quit (reason nick)
  "If REASON indicates a netsplit, mark NICK as splitted.
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
  "Show a QUIT message.

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
  (let ((split (circe-netsplit-quit (car args)
                                    nick)))
    (dolist (buf (circe-user-channels nick))
      (with-current-buffer buf
        (cond
         (split
          (when (< (+ split circe-netsplit-delay)
                   (float-time))
            (circe-server-message
             (format "Netsplit: %s (Use /WL to see who left)"
                     (car args)))))
         ((not (circe-lurker-p nick))
          (circe-server-message
           (format "Quit: %s (%s@%s) - %s"
                   nick user host (car args)))))))))

(circe-set-display-handler "JOIN" 'circe-display-JOIN)
(defun circe-display-JOIN (nick user host command args)
  "Show a JOIN message.

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
  ;; First, channel buffers for this user.
  (let ((split (circe-netsplit-join nick)))
    (with-current-buffer (circe-server-get-chat-buffer (car args)
                                                       'circe-channel-mode)
      (cond
       (split
        (when (< (+ (cadr split) circe-netsplit-delay)
                 (float-time))
          (circe-server-message
           (format "Netmerge: %s (Use /WL to see who's still missing)"
                   (car split)))))
       ((and circe-reduce-lurker-spam
             (circe-channel-recent-user-info nick 'last-active))
        (let ((departed (circe-channel-recent-user-info nick 'departed)))
          (circe-display 'circe-format-server-rejoin
                         :nick nick
                         :user user
                         :host host
                         :departuretime departed
                         :departuredelta (circe-duration-string
                                          (- (float-time)
                                             departed)))))
       ((not circe-reduce-lurker-spam)
        (circe-server-message
         (format "Join: %s (%s@%s)" nick user host))))))
  ;; Next, query buffers. We do this even when the message should be
  ;; ignored by a netsplit, since this can't flood.
  (dolist (buf (circe-user-channels nick))
    (with-current-buffer buf
      (when (eq major-mode 'circe-query-mode)
        (circe-server-message
         (format "Join: %s (%s@%s) is now on %s"
                 nick user host (car args)))))))

(defun circe-command-WL (&optional split)
  "Show the people who left in a netsplit.
Without any arguments, shows shows the current netsplits and how
many people are missing. With an argument SPLIT, which must be a
number, it shows the missing people due to that split."
  (let ((circe-netsplit-list (with-circe-server-buffer
                               circe-netsplit-list)))
    (if (or (not split)
            (and (stringp split)
                 (string= split "")))
        (if (null circe-netsplit-list)
            (circe-server-message "No net split at the moment")
          (let ((n 0))
            (dolist (entry circe-netsplit-list)
              (circe-server-message (format "(%d) Missing %d people due to %s"
                                            n
                                            (hash-table-count (nth 3 entry))
                                            (car entry)))
              (setq n (+ n 1)))))
      (let* ((index (if (numberp split)
                        split
                      (string-to-number split)))
             (entry (nth index circe-netsplit-list)))
        (if (not entry)
            (circe-server-message (format "No split number %s - use /WL to see a list"
                                          split))
          (let ((missing nil))
            (maphash (lambda (key value)
                       (setq missing (cons value missing)))
                     (nth 3 entry))
            (circe-server-message
             (format "Missing people due to %s: %s"
                     (car entry)
                     (mapconcat 'identity
                                (sort missing
                                      (lambda (a b)
                                        (string< (downcase a)
                                                 (downcase b))))
                                ", ")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple Format Specifiers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (fmt '(("INVITE" active "Invite: {origin} invites you to {1}")
               ("KICK" 0 "Kick: {1} kicked by {origin}: {2}")
               ("ERROR" active "Error: {0-}")
               ("001" server "{1}")
               ("002" server "{1}")
               ("003" server "{1}")
               ("004" server "{1-}")
               ("005" server "{1-}")
               ;; IRCnet: * Please wait while we process your connection.
               ("020" server "{0-}")
               ;; IRCnet
               ("042" server "Your unique ID is {1}")
               ("200" active "{1-}")
               ("201" active "{1-}")
               ("203" active "{1-}")
               ("204" active "{1-}")
               ("205" active "{1-}")
               ("206" active "{1-}")
               ("207" active "{1-}")
               ("208" active "{1-}")
               ("209" active "{1-}")
               ("211" active "{1-}")
               ("212" active "{1-}")
               ("219" active "{1-}")
               ("221" active "User mode: {1-}")
               ("234" active "Service: {1-}")
               ("235" active "{1-}")
               ("242" active "{1}")
               ("243" active "{1-}")
               ("250" server "{1}")
               ("251" server "{1}")
               ("252" server "{1-}")
               ("253" server "{1-}")
               ("254" server "{1-}")
               ("255" server "{1}")
               ("256" active "{1-}")
               ("257" active "{1}")
               ("258" active "{1}")
               ("259" active "{1}")
               ("261" active "{1-}")
               ("262" active "{1-}")
               ("263" active "{1-}")
               ("265" server "{1-}")
               ("266" server "{1-}")
               ;; This is returned on both WHOIS and PRIVMSG. It
               ;; should go to the active window for the former, and
               ;; the query window for the latter. Oh well.
               ("301" active "User away: {1}")
               ("302" active "User hosts: {1}")
               ("303" active "Users online: {1}")
               ("305" active "{1}")
               ("306" active "{1}")
               ("307" active "{1-}")
               ;; Coldfront: 310 <nick> is available for help.
               ("310" active "{1-}")
               ("311" active "{1} is {2}@{3} ({5})")
               ("312" active "{1} is on {2} ({3})")
               ("313" active "{1} {2}")
               ("314" active "{1} was {2}@{3} ({5})")
               ("315" active "{2}")
               ("318" active "{2}")
               ("319" active "{1} is on {2}")
               ("320" active "{1-}")
               ("322" active "{1-}")
               ("323" active "{1-}")
               ("324" 1 "Channel mode for {1}: {2-}")
               ("325" 1 "Unique operator on {1} is {2}")
               ("328" 1 "Channel homepage for {1}: {2-}")
               ("330" active "{1} is logged in as {2}")
               ("331" 1 "No topic for {1} set")
               ("332" 1 "Topic for {1}: {2}")
               ("341" active "Inviting {1} to {2}")
               ("346" 1 "Invite mask: {2}")
               ("347" 1 "{2}")
               ("348" 1 "Except mask: {2}")
               ("349" 1 "{2}")
               ("351" active "{1-}")
               ("352" active "{5} ({2}@{3}) in {1} on {4}: {6-}")
               ("353" 2 "Names: {3}")
               ("364" active "{1-}")
               ("365" active "{1-}")
               ("366" 1 "{2}")
               ("367" 1 "Ban mask: {2}")
               ("368" 1 "{2}")
               ("369" active "{1} {2}")
               ("371" active "{1}")
               ("372" server "{1}")
               ("374" active "{1}")
               ("375" server "{1}")
               ("376" server "{1}")
               ("378" active "{1-}")
               ("381" active "{1}")
               ("382" active "{1-}")
               ("391" active "Time on {1}: {2}")
               ("401" active "No such nick: {1}")
               ("402" active "No such server: {1}")
               ("403" active "No such channel: {1}")
               ("404" 1 "Can not send to channel {1}")
               ("405" active "Can not join {1}: {2}")
               ("406" active "{1-}")
               ("407" active "{1-}")
               ("408" active "No such service: {1}")
               ("422" active "{1}")
               ("432" active "Erroneous nick name: {1}")
               ("433" active "Nick name in use: {1}")
               ("437" active "Nick/channel is temporarily unavailable: {1}")
               ("441" 2 "User not on channel: {1}")
               ("442" active "You are not on {1}")
               ("443" 2 "User {1} is already on channel {2}")
               ;; Coldfront: 451 * :You have not registered
               ("451" active "{1-}")
               ("467" 1 "{2}")
               ("470" 1 "{1} made you join {2}: {3-}")
               ("471" 1 "{2}")
               ("472" active "{1-}")
               ("473" active "{1-}")
               ("474" active "{1-}")
               ("475" active "{1-}")
               ("476" active "{1-}")
               ("477" active "{1-}")
               ("481" 1 "{2-}")
               ("484" active "{1-}")
               ;; Coldfront: 671 <nick> is using a Secure Connection
               ("671" active "{1-}")
               ("728" 1 "Quiet mask: {3}")
               ("729" 1 "{3-}")
               ;; Freenode SASL auth
               ("900" active "SASL: {3-}")
               ("903" active "{1-}")))
  (circe-set-display-handler (car fmt) (cdr fmt)))

(defun circe-set-message-target (command target)
  "Set the target of TYPE in `circe-format-strings' to TARGET."
  (let ((handler (circe-get-display-handler command)))
    (when (not (consp handler))
      (error "Handler of command %s is not a list" command))
    (setcar handler target)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(when (not (fboundp 'string-trim))
  (defun string-trim (string)
    "Remove leading and trailing whitespace from STRING."
    (if (string-match "\\` *\\(.*[^[:space:]]\\) *\\'" string)
        (match-string 1 string)
      string)))

;;;;;;;;;;;;;;;;;;
;;; Extensions ;;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;
;;; Auto-Join

(defcustom circe-server-auto-join-default-type :immediate
  "The default auto-join type to use.

Possible options:

:immediate - Immediately after registering on the server
:after-auth - After nickserv authentication succeeded
:after-cloak - After we have acquired a cloaked host name
:after-nick - After we regained our preferred nick. See
              `circe-nickserv-ghost-style'.


See `circe-server-auto-join-channels' for more details."
  :type '(choice (const :tag "Immediately" :immediate)
                 (const :tag "After Authentication" :after-auth)
                 (const :tag "After Cloaking" :after-cloak)
                 (const :tag "After Nick Regain" :after-nick))
  :group 'circe)

(defvar circe-server-auto-join-channels nil
  "The default channels to join on this server.

Don't set this variable by hand, use `circe-network-options'.

The value should be a list of channels to join, with optional
keywords to configure the behavior of the following channels.

Best explained in an example:

\(\"#emacs\" :after-auth \"#channel\" \"#channel2\")

Possible keyword options are:

:immediate - Immediately after registering on the server
:after-auth - After nickserv authentication succeeded
:after-cloak - After we have acquired a cloaked host name
:after-nick - After we regained our preferred nick. See
              `circe-nickserv-ghost-style'.

The default is set in `circe-server-auto-join-default-type'.

A keyword in the first position of the channels list overrides
`circe-server-auto-join-default-type' for re-joining manually
joined channels.")
(make-variable-buffer-local 'circe-server-auto-join-channels)

(defvar circe-auto-joins ()
  "Internal variable, listing channels in the process of being
auto-joined, book-keeping for
`circe-new-buffer-behavior-ignore-auto-joins'.")
(make-variable-buffer-local 'circe-auto-joins)

(add-hook 'circe-server-connected-hook 'circe-auto-join-immediate)
(defun circe-auto-join-immediate ()
  "Join channels as per `circe-server-auto-join-channels'."
  (circe-auto-join :immediate))

(add-hook 'circe-nickserv-authenticated-hook 'circe-auto-join-after-auth)
(defun circe-auto-join-after-auth ()
"Join channels as per `circe-server-auto-join-channels'."
  (circe-auto-join :after-auth))

(add-hook 'circe-acquired-preferred-nick-hook
          'circe-auto-join-after-acquired-preferred-nick)
(defun circe-auto-join-after-acquired-preferred-nick ()
"Join channels as per `circe-server-auto-join-channels'."
  (circe-auto-join :after-nick))

(defun circe-auto-join (type)
  "Join channels as specified by TYPE.

See `circe-server-auto-join-channels' for details on TYPE."
  (dolist (channel (circe-auto-join-channels type))
    (unless (circe-server-get-chat-buffer channel)
      (add-to-list 'circe-auto-joins (downcase channel)))
    (irc-send-JOIN (circe-server-process) channel)))

(defun circe-auto-join-channels (type)
  "Return a list of channels as configured for auto-join type TYPE.

See `circe-server-auto-join-channels' for details. Channels
joined manually which are not in that list are added to the
front.  If the first item in the channels list is an auto-join
type keyword, that keyword is also used for manually joined
channels; otherwise, `circe-server-auto-join-default-type' is
used."
  (let* ((all-channels circe-server-auto-join-channels)
         (current-type (if (keywordp (car all-channels))
                           (car all-channels)
                           circe-server-auto-join-default-type))
         (result nil))
    (setq all-channels (append (circe-auto-join-manually-joined-channels
                                all-channels)
                               all-channels))
    (dolist (channel all-channels)
      (cond
       ((stringp channel)
        (when (eq type current-type)
          (setq result (cons channel result))))
       ((keywordp channel)
        (setq current-type channel))))
    (nreverse result)))

(defun circe-auto-join-manually-joined-channels (channels)
  "Return a list of already-joined channels not in CHANNELS."
  (let ((known (circe-case-fold-table))
        (result nil))
    (dolist (channel channels)
      (when (stringp channel)
        (puthash channel t known)))
    (when circe-server-chat-buffers
      (maphash (lambda (key channel)
                 (with-current-buffer channel
                   (when (and (eq major-mode 'circe-channel-mode)
                              (not (gethash circe-chat-target known nil)))
                     (setq result (cons circe-chat-target result)))))
               circe-server-chat-buffers))
    result))

;;;;;;;;;;;;;;;;;;;
;;; Topic Handling

(defvar circe-channel-topic ""
  "The current topic of the channel.")
(make-variable-buffer-local 'circe-channel-topic)

(defvar circe-channel-topic-old ""
  "The previous topic of the channel.")
(make-variable-buffer-local 'circe-channel-topic-old)

(defun circe-command-TOPIC (channel &optional newtopic)
  "Change the topic of CHANNEL to NEWTOPIC."
  (interactive "sChannel: \nsNew topic: ")
  (when (string-match "^\\s-*$" channel)
    (setq channel nil))
  (when (and channel
             (not newtopic)
             (string-match "^\\s-*\\(\\S-+\\)\\( \\(.*\\)\\)?" channel))
    (setq newtopic (match-string 3 channel)
          channel (match-string 1 channel)))
  (cond
   ((and channel newtopic)
    (irc-send-TOPIC (circe-server-process) channel newtopic))
   (channel
    (irc-send-TOPIC (circe-server-process) channel))
   (circe-chat-target
    (irc-send-TOPIC (circe-server-process) circe-chat-target))
   (t
    (circe-server-message "No channel given, and no default target."))))

(defun circe-command-CHTOPIC (&optional ignored)
  "Insert the topic of the current channel.

Arguments are IGNORED."
  (interactive)
  (if (not circe-chat-target)
      (circe-server-message "No target for current buffer")
    (lui-replace-input (format "/TOPIC %s %s" circe-chat-target circe-channel-topic))
    (goto-char (point-max))))

(circe-add-message-handler "TOPIC" 'circe-handle-TOPIC)
(defun circe-handle-TOPIC (nick user host command args)
  "Handle TOPIC messages."
  (with-circe-chat-buffer (car args)
    (setq circe-channel-topic-old circe-channel-topic
          circe-channel-topic (cadr args))))

(circe-add-message-handler "331" 'circe-handle-331)
(defun circe-handle-331 (nick user host command args)
  "Handle 331 RPL_NOTOPIC messages."
  (with-circe-chat-buffer (cadr args)
    (setq circe-channel-topic "")))

(circe-add-message-handler "332" 'circe-handle-332)
(defun circe-handle-332 (nick user host command args)
  "Handle 332 RPL_TOPIC messages."
  (with-circe-chat-buffer (cadr args)
    (setq circe-channel-topic (nth 2 args)
          circe-channel-topic-old circe-channel-topic)))

(circe-set-display-handler "TOPIC" 'circe-display-topic)
(defun circe-display-topic (nick user host command args)
  "Show a TOPIC message.

NICK, USER, and HOST are the originator of COMMAND which had ARGS
as arguments."
  (with-circe-chat-buffer (car args)
    (let ((old circe-channel-topic-old)
          (new (cadr args)))
      (circe-display 'circe-format-server-topic
                     :nick (or nick "(unknown)")
                     :user (or user "(unknown)")
                     :host (or host "(unknown)")
                     :origin (if (or user host)
                                 (format "%s (%s@%s)"
                                         (or nick "(unknown)")
                                         (or user "(unknown)")
                                         (or host "(unknown)"))
                               (or nick "(unknown)"))
                     :target (car args)
                     :channel (car args)
                     :new-topic new
                     :old-topic old
                     :topic-diff (circe-topic-diff old new)))))

(defun circe-topic-diff (old new)
  "Return a colored topic diff between OLD and NEW."
  (mapconcat (lambda (elt)
               (cond
                ((eq '+ (car elt))
                 (let ((s (cadr elt)))
                   (font-lock-prepend-text-property 0 (length s)
                                                    'face 'circe-topic-diff-new-face
                                                    s)
                   s))
                ((eq '- (car elt))
                 (let ((s (cadr elt)))
                   (font-lock-prepend-text-property 0 (length s)
                                                    'face 'circe-topic-diff-removed-face
                                                    s)
                   s))
                (t
                 (cadr elt))))
             (lcs-unified-diff (circe-topic-diff-split old)
                               (circe-topic-diff-split new)
                               'string=)
             ""))

(defun circe-topic-diff-split (str)
  "Split STR into a list of components.
The list consists of words and spaces."
  (let ((lis nil))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (< (point)
                (point-max))
        (if (or (looking-at "\\w+\\W*")
                (looking-at ".\\s-*"))
            (progn
              (setq lis (cons (match-string 0)
                              lis))
              (replace-match ""))
          (error "Can't happen"))))
    (nreverse lis)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nickserv Authentication

(defcustom circe-nickserv-authenticated-hook nil
  "Functions called after nickserv authenticated succeeded.
This is run every time nickserv confirms nick authentication,
which can happen multiple times per connection."
  :type 'hook
  :group 'circe)

(defcustom circe-acquired-preferred-nick-hook nil
  "Hook run after we're sure we have the nick we want.
Only used when auto-regain is enabled. See
`circe-nickserv-ghost-style'."
  :type 'hook
  :group 'circe)

(defcustom circe-nickserv-ghost-style nil
  "Whether and when circe should automatically regain your nick.
'immediate means to attempt regain immediately upon connect.
'after-auth means to attempt regain after authenticating with
nickserv."
  :type '(choice (const :tag "Immediate" 'immediate)
                 (const :tag "After Auth" 'after-auth)
                 (const :tag "Off" nil))
  :group 'circe)

(defvar circe-nickserv-mask nil
  "The regular expression to identify the nickserv on this network.

Matched against nick!user@host.")
(make-variable-buffer-local 'circe-nickserv-mask)

(defvar circe-nickserv-identify-challenge nil
  "A regular expression matching the nickserv challenge to identify.")
(make-variable-buffer-local 'circe-nickserv-identify-challenge)

(defvar circe-nickserv-identify-command nil
  "The IRC command to send to identify with nickserv.

This must be a full IRC command. It accepts the following
formatting options:

 {nick} - The nick to identify as
 {password} - The configured nickserv password")
(make-variable-buffer-local 'circe-nickserv-identify-command)

(defvar circe-nickserv-identify-confirmation nil
  "A regular expression matching a confirmation of authentication.")
(make-variable-buffer-local 'circe-nickserv-identify-confirmation)

(defvar circe-nickserv-ghost-command nil
  "The IRC command to send to regain/ghost your nick.

This must be a full IRC command. It accepts the following
formatting options:

  {nick} - The nick to ghost
  {password} - The configured nickserv password")
(make-variable-buffer-local 'circe-nickserv-ghost-command)

(defvar circe-nickserv-ghost-confirmation nil
  "A regular expression matching a confirmation for the GHOST command.

This is used to know when we can set our nick to the regained one
Leave nil if regaining automatically sets your nick")
(make-variable-buffer-local 'circe-nickserv-ghost-confirmation)

(defvar circe-nickserv-nick nil
  "The nick we are registered with for nickserv.

Do not set this variable directly. Use `circe-network-options' or
pass an argument to the `circe' function for this.")
(make-variable-buffer-local 'circe-nickserv-nick)

(defvar circe-nickserv-password nil
  "The password we use for nickserv on this network.

Can be either a string or a unary function of the nick returning
a string.

Do not set this variable directly. Use `circe-network-options' or
pass an argument to the `circe' function for this.")
(make-variable-buffer-local 'circe-nickserv-password)

(defun circe-nickserv-identify ()
  "Authenticate with nickserv."
  (with-circe-server-buffer
    (when (and circe-nickserv-identify-command
               circe-nickserv-nick
               circe-nickserv-password)
      (irc-send-raw
       (circe-server-process)
       (lui-format circe-nickserv-identify-command
                   :nick circe-nickserv-nick
                   :password (if (functionp circe-nickserv-password)
                                 (funcall circe-nickserv-password
                                          circe-nickserv-nick)
                               circe-nickserv-password))))))

(defun circe-nickserv-ghost ()
  "Regain/reclaim/ghost your nick if necessary."
  (with-circe-server-buffer
    (when (and circe-nickserv-ghost-command
               circe-nickserv-nick
               circe-nickserv-password)
      (irc-send-raw
       (circe-server-process)
       (lui-format circe-nickserv-ghost-command
                   :nick circe-nickserv-nick
                   :password circe-nickserv-password)))))

(defvar circe-auto-regain-awaiting-nick-change nil
  "Set to t when circe is awaiting confirmation for a regain request.")
(make-variable-buffer-local 'circe-auto-regain-awaiting-nick-change)

(add-hook 'circe-server-connected-hook 'circe-nickserv-ghost-immediately)
(defun circe-nickserv-ghost-immediately ()
  (when (eql 'immediate circe-nickserv-ghost-style)
    (if (circe-server-my-nick-p circe-nickserv-nick)
        (run-hooks 'circe-acquired-preferred-nick-hook)
      (circe-nickserv-ghost)
      (setq circe-auto-regain-awaiting-nick-change t))))

(circe-add-message-handler "PRIVMSG" 'circe-nickserv-handle-PRIVMSG)
(defun circe-nickserv-handle-PRIVMSG (nick user host command args)
  "Handle PRIVMSG messages from nickserv, which is unusual.

But bitlbee uses this."
  (circe-nickserv-handle-NOTICE nick user host command args))

(circe-add-message-handler "NOTICE" 'circe-nickserv-handle-NOTICE)
(defun circe-nickserv-handle-NOTICE (nick user host command args)
  "React to messages relevant to nickserv authentication and auto-regain."
  (when (and circe-nickserv-mask
             (string-match circe-nickserv-mask
                           (format "%s!%s@%s" nick user host)))
    (cond
     ;; Identify challenge
     ((and circe-nickserv-identify-challenge
           (string-match circe-nickserv-identify-challenge (cadr args)))
      (circe-nickserv-identify))
     ;; Confirmation
     ((and circe-nickserv-identify-confirmation
           circe-nickserv-nick
           (string-match circe-nickserv-identify-confirmation (cadr args)))
      (run-hooks 'circe-nickserv-authenticated-hook)
      (when (eq 'after-auth circe-nickserv-ghost-style)
        (if (circe-server-my-nick-p circe-nickserv-nick)
            (run-hooks 'circe-acquired-preferred-nick-hook)
          (circe-nickserv-ghost)
          (setq circe-auto-regain-awaiting-nick-change t))))
     ;; Nick got ghosted
     ((and circe-auto-regain-awaiting-nick-change
           circe-nickserv-ghost-confirmation
           circe-nickserv-nick
           (string-match circe-nickserv-ghost-confirmation
                         (cadr args)))
      (circe-command-NICK circe-nickserv-nick)))))

(circe-add-message-handler "NICK" 'circe-nickserv-handle-NICK)
(defun circe-nickserv-handle-NICK (nick user host command args)
  "Handle NICK messages in relation to nickserv commands.

This is used to run `circe-acquired-preferred-nick-hook' when
we're waiting for our nick change."
  (when (and circe-auto-regain-awaiting-nick-change
             (circe-server-my-nick-p nick)
             circe-nickserv-nick
             (string= (car args) circe-nickserv-nick))
    (setq circe-auto-regain-awaiting-nick-change nil)
    (run-hooks 'circe-acquired-preferred-nick-hook)))

(circe-add-message-handler "396" 'circe-handle-396)
(defun circe-handle-396 (nick user host command args)
  "Handle 396 RPL_HOSTHIDDEN messages."
  (circe-auto-join :after-cloak))

(provide 'circe)
;;; circe.el ends here
