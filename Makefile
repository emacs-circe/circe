.PHONY: compile doc install clean uninstall dist

include Makefile.defs

EL  = $(subst $(PACKAGE)-auto.el,,$(wildcard *.el)) $(PACKAGE)-auto.el
ELC = $(patsubst %.el,%.elc,$(wildcard *.el))

TEXI = $(PACKAGE).texi
INFO = $(patsubst %.texi,%.info,$(TEXI))
HTML = $(patsubst %.texi,%.html,$(TEXI))
PDF  = $(patsubst %.texi,%.pdf,$(TEXI))

all: compile doc

### Compiling
compile: $(ELC)

%.elc: %.el
	$(EMACS) -q $(SITEFLAG) -batch -l ./scripts/build-helper.el \
		-f batch-byte-compile $<

$(PACKAGE)-auto.el:
	cp $(PACKAGE)-auto.el.in $(PACKAGE)-auto.el
	$(EMACS) -q $(SITEFLAG) -batch -l ./scripts/build-helper.el \
		-f bh-generate-autoloads `pwd`/$(PACKAGE)-auto.el .

### Documentation
doc: $(INFO) $(HTML) $(PDF)

%.info: %.texi
	$(MAKEINFO) --no-split $<

%.html: %.texi
	$(MAKEINFO) --html --no-split $<

%.pdf: %.texi
	$(TEXI2DVI) -p -c -b $<

### Installation
install: compile doc
	$(INSTALL) -d $(ELCDIR) $(ELDIR) $(DOCDIR)
	$(INSTALL) -m 644 *.elc $(ELCDIR)
	$(INSTALL) -m 644 *.el $(ELDIR)
	$(INSTALL) -m 644 $(AUXDOC) $(HTML) $(PDF) $(DOCDIR)
	$(INSTALLINFO) $(INFO)

uninstall:
	rm -rf $(ELCDIR) $(ELDIR) $(DOCDIR)
	$(INSTALLINFO) --remove $(INFO)

### Cleaning the directory
clean:
	rm -f $(ELC) $(PACKAGE)-auto.el $(INFO) $(HTML) $(PDF)

### Creating a release
dist: $(PACKAGE)-auto.el
	$(MKDIR) ../$(PACKAGE)-$(VERSION)
	$(MKDIR) ../$(PACKAGE)-$(VERSION)/scripts
	$(CP) $(EL) $(TEXI) $(AUXDOC) ../$(PACKAGE)-$(VERSION)
	$(CP) scripts/build-helper.el ../$(PACKAGE)-$(VERSION)/scripts
	$(TEST) -d debian/ && cp -r debian/ ../$(PACKAGE)-$(VERSION)/ || true
	$(TAR) -C ../ -c $(PACKAGE)-$(VERSION) | gzip -9 > ../$(PACKAGE)-$(VERSION).tar.gz
	test -d debian && (cd ../$(PACKAGE)-$(VERSION) ; dpkg-buildpackage -rfakeroot) || true
	test -d debian && linitian ../$(PACKAGE)*_$(VERSION)*.deb || true
