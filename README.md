# Circe, a Client for IRC in Emacs

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
