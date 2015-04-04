# Circe, a Client for IRC in Emacs

[![Build Status](https://api.travis-ci.org/jorgenschaefer/circe.png?branch=master)](https://travis-ci.org/jorgenschaefer/circe)

Project homepage: https://github.com/jorgenschaefer/circe/wiki

Sources: https://github.com/jorgenschaefer/circe


## Overview

Circe is a Client for IRC in Emacs. It integrates well with the rest
of the editor, using standard Emacs key bindings and indicating
activity in channels in the status bar so it stays out of your way
unless you want to use it.

Complexity-wise, it is somewhere between rcirc (very minimal) and ERC
(very complex).


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

### Stable Version

Download the latest stable version from
https://github.com/jorgenschaefer/circe/releases

Unpack the tar.gz and add the `circe-X.Y/lisp/` directory to your
`load-path` like so:

```Lisp
(add-to-list 'load-path "~/.emacs.d/lisp/circe-X.Y/lisp")
(require 'circe)
```

The next time you start your Emacs, you should be able to use
`M-x circe` to connect to IRC.

### Development Version

In a shell:

```Shell
mkdir -d ~/.emacs.d/lisp/
cd ~/.emacs.d/lisp
git clone git://github.com/jorgenschaefer/circe.git
```

Then add the following to your `.emacs` file:

```Lisp
(add-to-list 'load-path "~/.emacs.d/lisp/circe/lisp")
(require 'circe)
```

The next time you start your Emacs, you should be able to use
`M-x circe` to connect to IRC.


## Documentation

Please see the Wiki for further information:

https://github.com/jorgenschaefer/circe/wiki
