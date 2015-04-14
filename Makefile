.PHONY: all test test-all cask compile release clean

EMACS ?= emacs
VERSION=$(shell sed -ne 's/^;; Version: \(.*\)/\1/p' circe.el)

all: test

test:
	cask exec ert-runner -L .
	cask exec buttercup -L .

test-all: clean cask
	make test
	make EMACS=emacs-24.1 test
	make EMACS=emacs-24.2 test
	make EMACS=emacs-24.3 test
	make EMACS=emacs-24.4 test
	make EMACS=emacs-24.5 test

cask:
	cask install
	EMACS=emacs-24.1 cask install
	EMACS=emacs-24.2 cask install
	EMACS=emacs-24.3 cask install
	EMACS=emacs-24.4 cask install
	EMACS=emacs-24.5 cask install

compile:
	$(EMACS) -batch -L . -f batch-byte-compile *.el

release: clean test-all
	mkdir -p "dist"
	tar -c *.el README.md --transform "s,^,circe-$(VERSION)/," --transform 's/README.md/README.txt/' > "dist/circe-$(VERSION).tar"

clean:
	rm -rf .cask dist
	find -name '*.elc' -delete
