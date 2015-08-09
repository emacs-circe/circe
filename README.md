# Circe, a Client for IRC in Emacs

[![Build Status](https://api.travis-ci.org/jorgenschaefer/circe.png?branch=master)](https://travis-ci.org/jorgenschaefer/circe)

## Overview

![Logo](images/circe.jpg)

Circe is a Client for IRC in Emacs. It tries to have sane defaults,
and integrates well with the rest of the editor, using standard Emacs
key bindings and indicating activity in channels in the status bar so
it stays out of your way unless you want to use it.

Complexity-wise, it is somewhere between rcirc (very minimal) and ERC
(very complex).

## Screenshot

![Screenshot](images/screenshot.png)

## Installation

### `package.el`

Make sure you have marmalade added to your package sources. To your
.emacs, add this:

```Lisp
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)
```

Then, use `package-install` to install Circe:

```
M-x package-install RET circe RET
```

After this, `M-x circe` should work.

### Development Version

In a shell:

```Shell
mkdir -d ~/.emacs.d/lisp/
cd ~/.emacs.d/lisp
git clone git://github.com/jorgenschaefer/circe.git
```

Then add the following to your `.emacs` file:

```Lisp
(add-to-list 'load-path "~/.emacs.d/lisp/circe")
(require 'circe)
```

The next time you start your Emacs, you should be able to use
`M-x circe` to connect to IRC.

## Connecting to IRC

To connect to IRC, simply use `M-x circe RET irc.freenode.net RET
RET`. This will connect you to Freenode. You can join us on
`#emacs-circe` by using `/join #emacs-circe` in the server buffer.

A more elaborate setup would require you to edit your init file and
add something like the following:

```Lisp
(setq circe-network-options
      '(("Freenode"
         :tls t
         :nick "my-nick"
         :sasl-username "my-nick"
         :sasl-password "my-password"
         :channels ("#emacs-circe")
         )))

```

With this in your configuration, you can use `M-x circe RET Freenode
RET` to connect to Freenode using these settings.

## Features

- Tab completion
- Nick highlighting
- Ignore feature that also hides users who talk to users on your
  ignore list
- Ignored messages can be toggled so they show up and then hidden
  again
- SSL support
- SASL authentication support
- Nickserv authentication, automatic ghosting, and nick re-gain
- Auto-join
- Ability to reduce join/part/quit spam from lurkers
- Automatic splitting of long lines at word boundaries
- Netsplit handling
- Activity tracking in the mode line
- Fully customizeable message display

## Documentation

Please see the Wiki for further information:

https://github.com/jorgenschaefer/circe/wiki
