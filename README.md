Circe is a Client for IRC in Emacs

Project homepage: http://www.jorgenschaefer.de/software/circe/
Github: https://github.com/jorgenschaefer/circe


# Overview

Circe was initially written in three or four days because the author
got terribly annoyed at bugs in ERC. Nontheless, many ideas are taken
directly from ERC, and that client has served the author well for a
long time. Thanks to all the people who worked on ERC!


# Lui

Circe uses Lui, the Linewise User Interface. This provides many
features with a clean interface so that Circe doesn't have to worry
about the whole displaying stuff. Lui can also be used by other
clients, so that for example the tracking feature is extensible.


# Installation

To install Circe, put all files in a directory, say, ~/.elisp/circe/
Then add the following code to your .emacs:

  (add-to-list 'load-path "~/.elisp/circe/lisp/")
  (autoload 'circe "circe" "Connect to an IRC server" t)

Now you can run M-x circe and connect to an IRC server of your choice.


# Sample Configuration

Circe is extensively configurable, and quite easily extensible. What
follows is the configuration of the author.


```Lisp
(autoload 'circe "circe" "Connect to an IRC server" t)

(when (file-directory-p "~/.elisp/circe")
  (add-to-list 'load-path "~/.elisp/circe"))

;; This defines the password variables below
(when (file-exists-p "~/.private.el")
  (load-file "~/.private.el"))

(setq circe-default-realname "http://www.forcix.cx/"
      circe-ignore-list nil
      circe-server-coding-system '(latin-1 . undecided)
      circe-server-auto-join-channels
      '(("^freenode$" "#emacs" "#scheme" "#scsh" "#darcs" "#wiki" "#css"
                      "#latex" "#ideologies" "##linguistics")
        ("^IRCnet" "#StarWars" "+linux.de"))
      circe-nickserv-auth-info `(("freenode" "freenode" "FreenodeNick" ,freenode-passwd)))

(setq lui-max-buffer-size 30000
      lui-flyspell-p t
      lui-flyspell-alist '(("+linux.de" "german8")
                           ("." "american")))

(eval-after-load "circe"
  '(progn
     (require 'lui-irc-colors)
     (add-to-list 'lui-pre-output-hook 'lui-irc-colors)
     (add-to-list 'circe-receive-message-functions
                  'fc-bitlbee-auth)))

(defun fc-bitlbee-auth (nick user host command args)
  "Authenticate to a bitlbee server."
  (when (and (string= command "JOIN")
             (circe-server-my-nick-p nick))
    (with-circe-server-buffer
      (when (string= circe-server-network "bitlbee")
        (circe-server-send (format "PRIVMSG #bitlbee :identify %s"
                                   bitlbee-passwd))))))

(defun irc ()
  "Connect to IRC."
  (interactive)
  (circe "irc.freenode.net" "6667" "freenode")
  (circe "sarg" "23523" "IRCnet" muh-passwd)
  (circe "localhost" "6668" "bitlbee"))
```


# Frequently Asked Questions about Circe

## Why doesn't Circe re-use old channel buffers after the server buffer got killed?

It is not possible to find out whether we can use that channel buffer
or not. Doing heuristics will lead to severe confusion when the user
connects to the same server twice. So we don't do this. Use the
`circe-reconnect` command in any Circe buffer to reconnect to this
server.

## Can Circe do logging?

Yes. Set `lui-logging-file-format` and `lui-logging-format` to values
of your preference; Circe adds the two format strings `{target}` and
`{network}` to the possibilities for `lui-logging-file-format`. To
actually enable logging, to `enable-lui-logging` on
`circe-chat-mode-hook`.

## Why does switching to some Circe buffers hang Emacs for a moment?

That's because you enabled flyspell for Circe, and ispell.el is so
intelligent as to start a new spelling process each time the
dictionary changes. Aspell takes much longer to start than ispell, so
you could just use ispell...

## How can I avoid to tell Lui twice about my friends, both when setting the dictionary and when adding them to `lui-highlight-keywords`?

Use the following function (thanks to paolo on Freenode for the idea):

```Lisp
(defun lui-add-pals (&rest pals)
  "Add PALS to highlighting and correct dictionary.
Every PAL is a cons cell of the dictionary to use, and a list of
nick names."
  (add-to-list 'lui-highlight-keywords
               (regexp-opt (apply #'append (mapcar #'cdr pals))
                           'words))
  (mapc (lambda (dictnicks)
          (add-to-list 'lui-flyspell-alist
                       (list (regexp-opt (cdr dictnicks)
                                         'words)
                             (car dictnicks))))
        pals))

(lui-add-pals '("american" "leary" "wilson")
              '("italian" "tonio" "paolo" "pablo"))
```

## Does Circe run on XEmacs?

Yes, she does. Grudgingly, but she does.

## Why can't I use the (circe "irc.freenode.net" "6667") on XEmacs?

XEmacs doesn't like a port number as a string argument there. Please
use an integer for this in that environment:

```Lisp
(circe "irc.freenode.net" 6667)
```

