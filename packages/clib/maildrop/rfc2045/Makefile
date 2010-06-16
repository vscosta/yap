################################################################
# Makefile for librfc2045.a
#
# Author: Jan Wielemaker (wielemak@science.uva.nl)
################################################################

CC=../../../swipl-ld.sh
CFLAGS=-fPIC -Wall -I./.. -DHAVE_CONFIG_H
LIBS=
AR=ar
RANLIB=ranlib

OBJ=	rfc2045.o rfc2045acchk.o rfc2045acprep.o \
	rfc2045appendurl.o rfc2045cdecode.o rfc2045decode.o \
	rfc2045enomem.o \
	rfc2045_base64encode.o rfc2045_fromfd.o \
	rfc2045find.o rfc2045mkboundary.o rfc2045rewrite.o \
	rfc2045tryboundary.o rfc2045xdump.o

all:	librfc2045.a

librfc2045.a: $(OBJ)
	rm -f librfc2045.a
	$(AR) r librfc2045.a $(OBJ)
	$(RANLIB) librfc2045.a


################################################################
# Clean
################################################################

clean:
		rm -f $(OBJ) *~ *.o *% a.out core config.log

distclean:	clean
		rm -f Makefile config.status config.h librfc2045.a
		rm -f rfc2045charset.h stamp-h1
		rm -rf autom4te.cache
