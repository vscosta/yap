################################################################
# Makefile template for SWI-Prolog PROSQLITE interface
#
# This template is used by configure to create Makefile.  See
# the file INSTALL for further installation instructions.
#
# License: LGPL
#
# Author: Nicos Angelopoulos & Jan Wielemaker (jan@swi.psy.uva.nl)
################################################################

PACKAGE=prosqlite
DOC=prosqlite
include ../Makefile.defs

CFLAGS+= -I/usr/include -I.
LDSOFLAGS+= -L/usr/lib

LIBS=-lgmp -lreadline -lncurses -lpthread -lresolv -lnss_dns -lnss_files -lcrypt -lstdc++ -lm  -L/u/vitor/lib -ldl -lnsl
NETLIBS=@NETLIBS@

LIBPL=		prolog/prosqlite.pl
TARGETS=	prosqlite.so

PROSQLITEOBJ=	prosqlite.o

all:		$(TARGETS)

nolib::
		@echo "WARNING: Could not find sqlite library; skipped"


prosqlite.o:	$(srcdir)/c/prosqlite.c
		$(CC) -c $(CFLAGS) $< -o $@

prosqlite.so:	$(PROSQLITEOBJ)
		$(LD) $(LDSOFLAGS) -o $@ $(AROBJ) -lsqlite3 -lgmp -lreadline -lncurses -lpthread -lresolv -lnss_dns -lnss_files -lcrypt -lstdc++ -lm  -L/u/vitor/lib -ldl -lnsl $(LIBPLSO)

install:	$(TARGETS) $(addprefix $(srcdir)/, $(LIBPL)) install-examples
		mkdir -p $(DESTDIR)$(SOLIBDIR)
		for f in $(TARGETS); do \
		  [ "$$f" = nolib ] || $(INSTALL_PROGRAM) $$f $(DESTDIR)$(SOLIBDIR); \
		done
		mkdir -p $(DESTDIR)$(PLLIBDIR)
			for f in $(LIBPL); do \
		  $(INSTALL_DATA) $(srcdir)/$$f $(DESTDIR)$(PLLIBDIR); \
		done
		$(MKINDEX)

ln-install::
		@$(MAKE) INSTALL_DATA='../ln-install' INSTALL_PROGRAM='../ln-install' install

rpm-install:	install

html-install::
		mkdir -p $(DESTDIR)$(PKGDOCDIR)
		$(INSTALL) -m 644 $(DOC).html $(DESTDIR)$(PKGDOCDIR)

pdf-install::
		mkdir -p $(DESTDIR)$(PKGDOCDIR)
		$(INSTALL) -m 644 $(DOC).pdf $(DESTDIR)$(PKGDOCDIR)

nnuninstall::
		(cd $(SOLIBDIR) && rm -f $(TARGETS))
		(cd $(PLLIBDIR) && rm -f $(LIBPL))
		$(PL) -f none -g make -t halt

################################################################
# Check
################################################################

check::


################################################################
# Clean
################################################################

clean:
		rm -f $(AROBJ) *~ *.o *% a.out core config.log

distclean:	clean
		rm -f $(TARGETS) config.cache config.h config.status Makefile
		rm -f $(DOC).aux $(DOC).log $(DOC).out $(DOC).toc
		rm -rf autom4te.cache
