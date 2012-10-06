#!/bin/sh

if [ ! -f "lisp/circe.el" ]
then
    echo "Please call this from the Circe main directory" >&2
    exit 1
fi

mkdir -p build
cp lisp/*.el build/

AUTOLOAD="$(pwd)/build/circe-auto.el"

emacs -q --batch --eval \
  "(let ((generated-autoload-file \"$AUTOLOAD\"))
     (batch-update-autoloads))" \
  build/

rm build/*.el~

emacs -q --batch --eval "(add-to-list 'load-path \"`pwd`/build/\")" \
      -f batch-byte-compile build/*.el
