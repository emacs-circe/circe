.PHONY: all test compile release clean

EMACS ?= emacs
VERSION=$(shell sed -ne 's/^;; Version: \(.*\)/\1/p' circe.el)

all: test

test:
	cask exec ert-runner -L .
	cask exec buttercup -L .

compile:
	$(EMACS) -batch -L . -f batch-byte-compile *.el

release: clean test
	mkdir -p "dist/circe-$(VERSION)"
	tar -c *.el README.md --transform "s,^,circe-$(VERSION)/," --transform 's/README.md/README.txt/' > "dist/circe-$(VERSION).tar"

clean:
	rm -rf dist
	find -name '*.elc' -delete
