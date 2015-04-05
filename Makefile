.PHONY: all test release clean

VERSION=$(shell sed -ne 's/^;; Version: \(.*\)/\1/p' lisp/circe.el)

all: test

test:
	cask exec ert-runner -L .
	cask exec buttercup -L .

release: clean test
	mkdir -p "dist/circe-$(VERSION)"
	cp lisp/*.el "dist/circe-$(VERSION)/"
	tar -C "dist" -c "circe-$(VERSION)" > "dist/circe-$(VERSION).tar"
	rm -rf "dist/circe-$(VERSION)"

clean:
	rm -rf dist
	find -name '*.elc' -delete
