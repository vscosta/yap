################################################################
# Makefile template for SWI-Prolog CLIB package
#
# This template is used by configure to create Makefile.  See
# the file INSTALL for further installation instructions.
#
# License: LGPL
#
# Author: Jan Wielemaker (wielemak@science.uva.nl)
################################################################

.SUFFIXES: .tex .dvi .doc .pl

PL=../swipl.sh
PLBASE=/home/vsc/Yap/Prologs/pl-devel/lib/swipl-5.9.11
PLARCH=x86_64-linux
PLINCL=/home/vsc/Yap/Prologs/pl-devel/include
PKGDOC=$(PLBASE)/doc/packages
PCEHOME=../xpce

CC=../swipl-ld.sh
COFLAGS=-O2 -fno-strict-aliasing
CWFLAGS=-Wall
CMFLAGS=-fPIC
CIFLAGS=-I. -Imaildrop/rfc2045 -Imaildrop/rfc822
CFLAGS=$(COFLAGS) $(CWFLAGS) $(CMFLAGS) $(CIFLAGS) -DHAVE_CONFIG_H
LIBS=
NETLIBS=
CRYPTLIBS=-lcrypt 
MIMELIBS=maildrop/rfc2045/librfc2045.a maildrop/rfc822/librfc822.a

PUBLICPL=swipl
DOCTOTEX=$(PCEHOME)/bin/doc2tex
PLTOTEX=$(PUBLICPL) -q -s pltotex.pl -g pltotex --
DOC=clib
TEX=$(DOC).tex process.tex uri.tex
DVI=$(DOC).dvi
PDF=$(DOC).pdf
RUNTEX=../../man/runtex

LD=../swipl-ld.sh
LDFLAGS=-shared

INSTALL=/usr/bin/install -c
INSTALL_PROGRAM=${INSTALL}
INSTALL_DATA=/usr/bin/install -c -m 644

LIBPL=		random.pl unix.pl socket.pl cgi.pl memfile.pl filesex.pl mime.pl crypt.pl time.pl rlimit.pl streampool.pl streaminfo.pl process.pl prolog_server.pl sha.pl \
		uri.pl
TARGETS=	random.so unix.so socket.so cgi.so memfile.so files.so mime.so crypt.so time.so rlimit.so readutil.so streaminfo.so process.so \
		sha4pl.so uri.so

UNXOBJ=		error.o unix.o
SOCKOBJ=	error.o socket.o nonblockio.o
CGIOBJ=		error.o form.o cgi.o
CRYPTOBJ=	error.o crypt.o md5.o md5passwd.o
MEMOBJ=		error.o memfile.o
MIMEOBJ=	error.o mime.o
TIMEOBJ=	error.o time.o
FILESOBJ=	error.o files.o
RLIMITOBJ=	error.o rlimit.o
URIOBJ=		uri.o
RANDOMOBJ=	random.o
READUTILOBJ=	readutil.o
PROCESSOBJ=	error.o process.o
STREAMINFOOBJ=	error.o streaminfo.o
SHAOBJ=		error.o sha4pl.o sha1/sha1.o sha1/sha2.o \
		sha1/hmac_sha1.o sha1/hmac_sha256.o

all:		$(TARGETS)

random.so:	$(RANDOMOBJ)
		$(LD) $(LDFLAGS) -o $@ $(RANDOMOBJ)
unix.so:	$(UNXOBJ)
		$(LD) $(LDFLAGS) -o $@ $(UNXOBJ)
socket.so:	$(SOCKOBJ)
		$(LD) $(LDFLAGS) -o $@ $(SOCKOBJ) $(NETLIBS)
cgi.so:	$(CGIOBJ)
		$(LD) $(LDFLAGS) -o $@ $(CGIOBJ)
crypt.so:	$(CRYPTOBJ)
		$(LD) $(LDFLAGS) -o $@ $(CRYPTOBJ) $(CRYPTLIBS)
memfile.so:	$(MEMOBJ)
		$(LD) $(LDFLAGS) -o $@ $(MEMOBJ) $(LIBS)
mime.so:	$(MIMEOBJ) $(MIMELIBS)
		$(LD) $(LDFLAGS) -o $@ $(MIMEOBJ) $(MIMELIBS)
time.so:	$(TIMEOBJ)
		$(LD) $(LDFLAGS) -o $@ $(TIMEOBJ) $(LIBS)
files.so:	$(FILESOBJ)
		$(LD) $(LDFLAGS) -o $@ $(FILESOBJ) $(LIBS)
rlimit.so:	$(RLIMITOBJ)
		$(LD) $(LDFLAGS) -o $@ $(RLIMITOBJ) $(LIBS)
readutil.so:	$(READUTILOBJ)
		$(LD) $(LDFLAGS) -o $@ $(READUTILOBJ)
process.so:	$(PROCESSOBJ)
		$(LD) $(LDFLAGS) -o $@ $(PROCESSOBJ)
streaminfo.so: $(STREAMINFOOBJ)
		$(LD) $(LDFLAGS) -o $@ $(STREAMINFOOBJ)
sha4pl.so:	$(SHAOBJ)
		$(LD) $(LDFLAGS) -o $@ $(SHAOBJ)
uri.so:	$(URIOBJ)
		$(LD) $(LDFLAGS) -o $@ $(URIOBJ)

socket.o:	socket.c sockcommon.c

sha1/hmac_sha1.o:
		$(CC) -c $(CFLAGS) -DUSE_SHA1 -o $@ sha1/hmac.c
sha1/hmac_sha256.o:
		$(CC) -c $(CFLAGS) -DUSE_SHA256 -o $@ sha1/hmac.c

maildrop/rfc822/librfc822.a:
		(cd maildrop/rfc822 && $(MAKE))
maildrop/rfc2045/librfc2045.a:
		(cd maildrop/rfc2045 && $(MAKE))


install:	$(TARGETS) $(LIBPL)
		mkdir -p $(DESTDIR)$(PLBASE)/lib/$(PLARCH)
		for f in $(TARGETS); do \
		  $(INSTALL_PROGRAM) $$f $(DESTDIR)$(PLBASE)/lib/$(PLARCH); \
		done
		mkdir -p $(DESTDIR)$(PLBASE)/library
		for f in $(LIBPL); do \
		  $(INSTALL_DATA) $$f $(DESTDIR)$(PLBASE)/library; \
		done
		$(PL) -f none -g make -t halt

ln-install::
		@$(MAKE) INSTALL_DATA='../ln-install' INSTALL_PROGRAM='../ln-install' install

rpm-install:	install

html-install::
		mkdir -p $(DESTDIR)$(PKGDOC)
		$(INSTALL) -m 644 clib.html $(DESTDIR)$(PKGDOC)

pdf-install::
		mkdir -p $(DESTDIR)$(PKGDOC)
		$(INSTALL) -m 644 clib.pdf $(DESTDIR)$(PKGDOC)

uninstall::
		(cd $(PLBASE)/lib/$(PLARCH) && rm -f $(TARGETS))
		(cd $(PLBASE)/library && rm -f $(LIBPL))
		$(PL) -f none -g make -t halt

################################################################
# Testing
################################################################

check:		check-socket check-memfile check-unit check-process \
		check-read check-uri check-cgi

torture:	torture-socket

check-socket::
		$(PL) -q -f test_socket.pl -F none -g test_socket,halt -t 'halt(1)'

torture-socket::
		$(PL) -q -f stresssocket.pl -F none -g test,halt -t 'halt(1)'

check-memfile::
		$(PL) -q -f test_memfile.pl -F none -g test_memfile,halt -t 'halt(1)'

check-unit::
		$(PL) -q -f test.pl -g run_tests,halt -t 'halt(1)'

check-uri::
		$(PL) -q -f test_uri.pl -g true -t test_uri

check-cgi::
		$(PL) -q -f test_cgi.pl -g true -t test_cgi

check-process::
		$(PL) -q -f test_process.pl -g true -t test_process

check-read::
		$(PL) -q -f test_readutil.pl -g true -t test_readutil

################################################################
# Documentation
################################################################

pdf:		$(PDF)

$(DVI):		$(TEX)
		$(RUNTEX) $(DOC)

$(PDF):		$(TEX)
		$(RUNTEX) --pdf $(DOC)

html:		$(TEX)
		latex2html $(DOC)
		mv html/index.html clib.html
		rm -r html

$(TEX):		$(DOCTOTEX)

uri.tex:	uri.pl
		$(PLTOTEX) 'library(uri)'
process.tex:	process.pl
		$(PLTOTEX) 'library(process)'

.doc.tex:
		$(DOCTOTEX) $*.doc > $*.tex


################################################################
# Clean
################################################################

clean:
		rm -f $(OBJ) *~ *.o *% a.out core config.log sha1/*.o
		rm -f $(TEX)
		-(cd maildrop/rfc822 && $(MAKE) clean)
		-(cd maildrop/rfc2045 && $(MAKE) clean)

distclean:	clean
		rm -f $(TARGETS) config.cache config.h config.status Makefile
		rm -f clib.aux clib.log clib.out clib.toc
		rm -rf html
		-(cd maildrop/rfc822 && $(MAKE) distclean)
		-(cd maildrop/rfc2045 && $(MAKE) distclean)
