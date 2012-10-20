#!/bin/sh

set -e

if [ ! -f "lisp/circe.el" ]
then
    echo "Please call this from the Circe main directory" >&2
    exit 1
fi

generate_autoloads () {
    local PACKAGE="$1"
    local DEST="$2"
    local AUTOLOAD="$DEST/$PACKAGE-autoloads.el"
    emacs -q --batch --eval \
      "(let ((generated-autoload-file \"$AUTOLOAD\"))
         (batch-update-autoloads))" \
      "$DEST"
    rm "$DEST"/*.el~
}

compile_elisp () {
    local DEST="$1"
    emacs -q --batch --eval "(add-to-list 'load-path \"$DEST\")" \
          -f batch-byte-compile "$DEST"/*.el
}

elisp_version () {
    sed -ne 's/^;; *Version: \([0-9.]*\).*/\1/p' "$1"
}

elisp_defvar_version () {
    sed -ne 's/(defvar circe-version "\(.*\)"/\1/p' "$1"
}

generate_elpa_readme () {
    local LISPFILE="$1"
    local README="$2"
    cat "$LISPFILE" \
    | sed -e '1,/^;;;.*Commentary:.*/d' \
          -e '/^;;; Code:/,$d' \
    | sed -e '1d' -e '$d' \
          -e 's/^;* *//' \
    > "$README"
}

if [ "$1" = "release" ]
then
    CIRCE_VERSION=$(elisp_version lisp/circe.el)
    LUI_VERSION=$(elisp_version lisp/lui.el)
    TRACKING_VERSION=$(elisp_version lisp/tracking.el)
    LCS_VERSION=$(elisp_version lisp/lcs.el)

    CIRCE_DEFVAR_VERSION=$(elisp_defvar_version lisp/circe.el)
    if [ "$CIRCE_VERSION" != "$CIRCE_DEFVAR_VERSION" ]
    then
        echo "Version mismatch!"
        echo "circe.el's Version: header says this is version \"$CIRCE_VERSION\""
        echo "circe.el's circe-version says this is \"$CIRCE_DEFVAR_VERSION\""
        echo "This should match."
        exit 1
    fi

    if ! git diff-index HEAD --quiet --
    then
        echo "This repository is dirty. Please clean up for a release."
        exit 1
    fi

    echo "The following versions have been detected:"
    echo
    echo "  Circe $CIRCE_VERSION"
    echo "  Lui $LUI_VERSION"
    echo "  tracking $TRACKING_VERSION"
    echo "  lcs $LCS_VERSION"
    echo
    echo -n "Tag this as v$CIRCE_VERSION? [y/n] "
    read correct
    if [ "$correct" != "y" ]
    then
        echo "Aborting"
        exit 1
    fi

    echo
    echo -n "Running git tag v$CIRCE_VERSION ... "
    git tag "v$CIRCE_VERSION"
    echo "ok."

    mkdir -p release

    # Main release
    echo -n "Creating circe-$CIRCE_VERSION.tar.gz ... "
    DEST="$(pwd)/release/circe-$CIRCE_VERSION/"
    mkdir -p "$DEST"
    cp -r LICENSE README.md lisp/ "$DEST"
    generate_autoloads "circe" "$DEST/lisp/" 2>/dev/null
    compile_elisp "$DEST/lisp" 2>/dev/null
    tar -C release/ -c "circe-$CIRCE_VERSION" \
    | gzip -9 > "release/circe-$CIRCE_VERSION.tar.gz"
    rm -rf "$DEST"
    echo "ok."

    # Circe for elpa
    echo -n "Creating circe-$CIRCE_VERSION.tar for elpa ... "
    DEST="$(pwd)/release/circe-$CIRCE_VERSION/"
    mkdir -p "$DEST"
    cp -r LICENSE lisp/* "$DEST"
    generate_elpa_readme "$DEST/circe.el" "$DEST/README"
    generate_autoloads "circe" "$DEST" 2>/dev/null
    compile_elisp "$DEST" 2>/dev/null
    tar -C release/ -c "circe-$CIRCE_VERSION" \
    > "release/circe-$CIRCE_VERSION.tar"
    rm -rf "$DEST"
    echo "ok."

    # Lui for elpa
    echo -n "Creating lui-$LUI_VERSION.tar for elpa ... "
    DEST="$(pwd)/release/lui-$LUI_VERSION/"
    mkdir -p "$DEST"
    cp -r LICENSE lisp/* "$DEST"
    generate_elpa_readme "$DEST/lui.el" "$DEST/README"
    generate_autoloads "lui" "$DEST" 2>/dev/null
    compile_elisp "$DEST" 2>/dev/null
    tar -C release/ -c "lui-$CIRCE_VERSION" \
    > "release/lui-$CIRCE_VERSION.tar"
    rm -rf "$DEST"
    echo "ok."

    # tracking for elpa
    echo -n "Creating tracking.el for elpa ... "
    cp lisp/tracking.el release/
    echo "ok."

    # lcs for elpa
    echo -n "Creating lcs.el for elpa ... "
    cp lisp/lcs.el release/
    echo "ok."

    echo
    echo "All done. Now, recheck things and then do this:"
    echo
    echo "- Push the version tag with: git push --tags"
    echo "- Upload release/circe-$CIRCE_VERSION.tar.gz to"
    echo "  https://github.com/jorgenschaefer/circe/downloads"
    echo "- Upload the release/*.tar and release/*.el files to"
    echo "  http://marmalade-repo.org/"
elif [ "$1" = "build" ] || [ "$1" = "" ]
then
    rm -rf build/
    mkdir -p build/
    cp lisp/*.el build/
    generate_autoloads $(pwd)/build/
    compile_elisp $(pwd)/build/
    echo
    echo "Compiled files can be found in build/"
fi
