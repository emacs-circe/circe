#!/bin/sh

if [ ! -f "lisp/circe.el" ]
then
    echo "Please run this script form the Circe repository." >&2
    exit 1
fi

# Circe developers, feel free to adapt to your local setups.
if [ -d "$HOME/Programs/emacsen" ]
then
    EMACSEN="$HOME/Programs/emacsen/*24*/src/emacs $HOME/Programs/Emacs/src/emacs"
else
    EMACSEN="emacs"
fi

cd lisp
find -name '*.elc' -exec rm {} +

for EMACS in $EMACSEN
do
    echo "*** Running " $("$EMACS" --version | head -1)
    for TESTFILE in *-tests.el
    do
        echo "*** Testing $TESTFILE"
        $EMACS -q -batch -L `pwd` -l ert -l "$TESTFILE" \
            -f ert-run-tests-batch-and-exit
    done
done
