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
    sed -ne 's/(defvar [^ ]*-version "\(.*\)"/\1/p' "$1"
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

package_short_description () {
    sed -ne '1s/.*--- //p' "$1"
}

generate_pkg_file () {
    local PACKAGE="$1"
    local VERSION="$2"
    local DESC="$3"
    local DEPENDS="$4"
    local PKGFILE="$5"

    (echo "(define-package \"$PACKAGE\" \"$VERSION\" \"$DESC\" $DEPENDS)"
     echo ";; no-byte-compile: t"
    ) > "$PKGFILE"
}

make_elpa_package () {
    local PACKAGE="$1"
    local VERSION="$2"
    local DEPENDS="$3"
    echo -n "Creating $PACKAGE-$VERSION.tar for elpa ... "
    DEST="$(pwd)/release/$PACKAGE-$VERSION/"
    mkdir -p "$DEST"
    cp -r LICENSE "lisp/$PACKAGE"* "$DEST"
    DESC="$(package_short_description lisp/$PACKAGE.el)"
    generate_elpa_readme "$DEST/$PACKAGE.el" "$DEST/README"
    generate_autoloads "$PACKAGE" "$DEST" 2>/dev/null
    generate_pkg_file "$PACKAGE" "$VERSION" "$DESC" "$DEPENDS" \
        "$DEST/$PACKAGE-pkg.el"
    tar -C release/ -c "$PACKAGE-$VERSION" \
    > "release/$PACKAGE-$VERSION.tar"
    rm -rf "$DEST"
    echo "ok."
}

make_main_release () {
    local VERSION="$1"
    # Main release
    echo -n "Creating circe-$VERSION.tar.gz ... "
    DEST="$(pwd)/release/circe-$VERSION/"
    mkdir -p "$DEST"
    cp -r LICENSE README.md lisp/ "$DEST"
    generate_autoloads "circe" "$DEST/lisp/" 2>/dev/null
    compile_elisp "$DEST/lisp" 2>/dev/null
    tar -C release/ -c "circe-$VERSION" \
    | gzip -9 > "release/circe-$VERSION.tar.gz"
    rm -rf "$DEST"
    echo "ok."
}

files_changed () {
    local START="$1"
    local PATTERN="$2"
    git diff --name-only "$START" | grep -q "$PATTERN"
}


if [ "$1" = "release" ]
then
    if ! git diff-index HEAD --quiet --
    then
        echo "This repository is dirty. Please clean up for a release."
        exit 1
    fi

    LAST_RELEASE="$(git describe --tags HEAD --abbrev=0)"
    CIRCE_VERSION="$(elisp_version lisp/circe.el)"
    LUI_VERSION="$(elisp_version lisp/lui.el)"
    TRACKING_VERSION="$(elisp_version lisp/tracking.el)"
    LCS_VERSION="$(elisp_version lisp/lcs.el)"

    CIRCE_DEFVAR_VERSION="$(elisp_defvar_version lisp/circe.el)"
    if [ "$CIRCE_VERSION" != "$CIRCE_DEFVAR_VERSION" ]
    then
        echo "Version mismatch!"
        echo "circe.el's Version: header says this is version \"$CIRCE_VERSION\""
        echo "circe.el's circe-version says this is \"$CIRCE_DEFVAR_VERSION\""
        echo "This should match."
        exit 1
    fi

    LUI_DEFVAR_VERSION="$(elisp_defvar_version lisp/lui.el)"
    if [ "$LUI_VERSION" != "$LUI_DEFVAR_VERSION" ]
    then
        echo "Version mismatch!"
        echo "lui.el's Version: header says this is version \"$LUI_VERSION\""
        echo "lui.el's lui-version says this is \"$LUI_DEFVAR_VERSION\""
        echo "This should match."
        exit 1
    fi

    echo "The following versions have been detected:"
    echo
    echo "  Circe $CIRCE_VERSION"
    echo "  Lui $LUI_VERSION"
    echo "  tracking $TRACKING_VERSION"
    echo "  lcs $LCS_VERSION"
    echo
    echo "The last release was tagged as $LAST_RELEASE"
    echo
    echo -n "Tag the current repository as v$CIRCE_VERSION? [y/n] "
    read correct
    if [ "$correct" != "y" ]
    then
        echo "Aborting."
        exit 1
    fi

    echo
    echo -n "Running git tag -a v$CIRCE_VERSION ... "
    git tag -a -m "Released version v$CIRCE_VERSION" "v$CIRCE_VERSION" HEAD
    echo "ok."

    rm -rf release
    mkdir -p release

    if files_changed "$LAST_RELEASE" "^lisp/circe"
    then
        make_main_release "$CIRCE_VERSION"
        # Circe for elpa
        make_elpa_package "circe" "$CIRCE_VERSION" \
            "'(\"lui\" \"lcs\")"
    else
        echo "Circe has not changed since $LAST_RELEASE, skipped"
    fi

    if files_changed "$LAST_RELEASE" "^lisp/lui"
    then
        # Lui for elpa
        make_elpa_package "lui" "$LUI_VERSION" \
            "'(\"tracking\")"
    else
        echo "Lui has not changed since $LAST_RELEASE, skipped"
    fi

    if files_changed "$LAST_RELEASE" "^lisp/tracking.el"
    then
        # tracking for elpa
        echo -n "Creating tracking.el for elpa ... "
        cp lisp/tracking.el release/
        echo "ok."
    else
        echo "tracking has not changed since $LAST_RELEASE, skipped"
    fi

    if files_changed "$LAST_RELEASE" "^lisp/lcs.el"
    then
        # lcs for elpa
        echo -n "Creating lcs.el for elpa ... "
        cp lisp/lcs.el release/
        echo "ok."
    else
        echo "lcs has not changed since $LAST_RELEASE, skipped"
    fi

    echo
    echo "All done. Now, recheck things and then do this:"
    echo
    echo "- Push the version tag with: git push --tags"
    echo "- Upload release/*.tar.gz to"
    echo "  https://github.com/jorgenschaefer/circe/downloads"
    echo "- Upload release/*.tar and release/*.el to"
    echo "  http://marmalade-repo.org/"
elif [ "$1" = "build" ] || [ "$1" = "" ]
then
    rm -rf build/
    mkdir -p build/
    cp lisp/*.el build/
    generate_autoloads "$(pwd)/build/"
    compile_elisp "$(pwd)/build/"
    echo
    echo "Compiled files can be found in build/"
fi
