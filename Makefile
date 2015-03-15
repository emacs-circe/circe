.PHONY: all test tar circe lui tracking lcs shorten

VERSION=$(shell sed -ne 's/^;; Version: \(.*\)/\1/p' lisp/circe.el)

all: test

test:
	cask exec ert-runner -L lisp -L test -l test/*-test.el

tar: clean circe lui tracking lcs shorten

circe:
	mkdir -p "build/circe-$(VERSION)"
	cp lisp/circe*.el "build/circe-$(VERSION)/"
	cat "build/circe-$(VERSION)/circe.el" \
	  | sed -e '1,/^;;;.*Commentary:.*/d' \
	        -e '/^;;; Code:/,$$d' \
	  | sed -e '1d' -e '$$d' \
	        -e 's/^;* *//' \
          > "build/circe-$(VERSION)/README"
	tar -C "build" -c "circe-$(VERSION)" > "build/circe-$(VERSION).tar"
	rm -rf "build/circe-$(VERSION)"

lui:
	mkdir -p "build/lui-$(VERSION)"
	cp lisp/lui*.el "build/lui-$(VERSION)/"
	cat "build/lui-$(VERSION)/lui.el" \
	  | sed -e '1,/^;;;.*Commentary:.*/d' \
	        -e '/^;;; Code:/,$$d' \
	  | sed -e '1d' -e '$$d' \
	        -e 's/^;* *//' \
          > "build/lui-$(VERSION)/README"
	tar -C "build" -c "lui-$(VERSION)" > "build/lui-$(VERSION).tar"
	rm -rf "build/lui-$(VERSION)"

tracking:
	cp "lisp/tracking.el" "build/tracking-$(VERSION).el"

lcs:
	cp "lisp/lcs.el" "build/lcs-$(VERSION).el"

shorten:
	cp "lisp/shorten.el" "build/shorten-$(VERSION).el"

clean:
	rm -rf build
	find -name '*.elc' -delete
