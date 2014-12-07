VERSION=$(shell sed -ne 's/^;; Version: \(.*\)/\1/p' lisp/circe.el)

tar: clean circe lui tracking # lcs shorten

circe:
	mkdir -p "build/circe-$(VERSION)"
	cp lisp/circe*.el "build/circe-$(VERSION)/"
	cat "build/circe-$(VERSION)/circe.el" \
	  | sed -e '1,/^;;;.*Commentary:.*/d' \
	        -e '/^;;; Code:/,$$d' \
	  | sed -e '1d' -e '$$d' \
	        -e 's/^;* *//' \
          > "build/circe-$(VERSION)/README"
	cat "lisp/circe-pkg.el.in" \
	  | sed -e 's/$$VERSION/$(VERSION)/g' \
	        -e 's/"0"/"$(VERSION)"/g' \
	  > "build/circe-$(VERSION)/circe-pkg.el"
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
	cat "lisp/lui-pkg.el.in" \
	  | sed -e 's/$$VERSION/$(VERSION)/g' \
	        -e 's/"0"/"$(VERSION)"/g' \
	  > "build/lui-$(VERSION)/lui-pkg.el"
	tar -C "build" -c "lui-$(VERSION)" > "build/lui-$(VERSION).tar"
	rm -rf "build/lui-$(VERSION)"

tracking:
	cp "lisp/tracking.el" "build/tracking-$(VERSION).el"

# These change basically never

# lcs:
# 	cp "lisp/lcs.el" "build/lcs-$(VERSION).el"

# shorten:
# 	cp "lisp/shorten.el" "build/shorten-$(VERSION).el"

clean:
	find -name '*.elc' -delete
