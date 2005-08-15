.PHONY: all lisp doc clean realclean distclean fullclean install test dist release debclean debrelease circe-build.elc
.PRECIOUS: %.elc

include Makefile.defs

EL  = $(wildcard *.el)
ELC = $(patsubst %.el,%.elc,$(wildcard *.el))

all: lisp $(MANUAL).info

lisp: $(ELC)

circe-build.elc: ./scripts/circe-build.el
	@echo circe-build.el is not byte-compiled

%.elc: %.el
	@$(EMACS) -q $(SITEFLAG) -batch -l ./scripts/circe-build.el \
		-f batch-byte-compile $<

%.info: %.texi
	 makeinfo $<

%.html: %.texi
	 makeinfo --html --no-split $<

doc: $(MANUAL).info $(MANUAL).html

clean:
	-rm -f *.elc *~

realclean fullclean: clean
	-rm -f $(MANUAL).info $(MANUAL).html

install: $(ELC) $(MANUAL).info
	install -d $(ELISPDIR)
	install -m 0644 $(EL) $(wildcard *.elc) $(ELISPDIR)
	install -d $(INFODIR)
	install -m 0644 $(MANUAL).info $(INFODIR)/$(PROJECT)
	$(INSTALLINFO) $(INFODIR)/$(PROJECT)

test: $(ELC)
	$(EMACS) -q $(SITEFLAG) -batch -l ./scripts/circe-build.el \
		-f circe-elint-files $(EL)

distclean: realclean
	-rm -fr ../$(PROJECT)-$(VERSION)

dist: distclean
	(DIR=`pwd` ; cd .. && cp -r $$DIR $(PROJECT)-$(VERSION))
	-rm -fr ../$(PROJECT)-$(VERSION)/CVS
	-rm -fr ../$(PROJECT)-$(VERSION)/debian ../$(PROJECT)-$(VERSION)/test

release: dist
	(cd .. \
	 && tar -c $(PROJECT)-$(VERSION) \
	    | gzip -9 > $(PROJECT)-$(VERSION).tar.gz )

debclean:
	-rm -f ../$(DEBNAME)_$(VERSION)*

debrelease: dist debclean
	-rm -fr ../$(DEBNAME)-$(VERSION)
	mv ../$(PROJECT)-$(VERSION) ../$(DEBNAME)-$(VERSION)
	(cd .. && tar -czf $(DEBNAME)_$(VERSION).orig.tar.gz \
	          $(DEBNAME)-$(VERSION))
	cp -r debian ../$(DEBNAME)-$(VERSION)
	-rm -fr ../$(DEBNAME)-$(VERSION)/debian/.arch-ids
	(cd ../$(DEBNAME)-$(VERSION) && \
	  dpkg-buildpackage -v$(LASTUPLOAD) $(BUILDOPTS) \
	    -us -uc -rfakeroot && \
	  echo "Running lintian ..." && \
	  lintian -i ../$(DEBNAME)_$(VERSION)*.deb || : && \
	  echo "Done running lintian." && \
	  debsign)
	cp ../$(DEBNAME)_$(VERSION)* ../../dist

upload:
	(cd .. && gpg --detach $(PROJECT)-$(VERSION).tar.gz && \
	  echo open ftp://savannah.nongnu.org > upload.lftp ; \
	  echo cd /incoming/savannah/emacs-wiki >> upload.lftp ; \
	  echo mput $(PROJECT)-$(VERSION).tar.gz* >> upload.lftp ; \
	  echo close >> upload.lftp ; \
	  lftp -f upload.lftp ; \
	  rm -f upload.lftp)
