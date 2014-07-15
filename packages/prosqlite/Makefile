SOBJ=	$(PACKSODIR)/prosqlite.$(SOEXT)
CFLAGS+=-std=c99
LIBS=	-lsqlite3

all:	$(SOBJ)

$(SOBJ): c/prosqlite.o
	mkdir -p $(PACKSODIR)
	$(LD) $(LDSOFLAGS) -o $@ $(SWISOLIB) $< $(LIBS)

check::
install::
clean:
	rm -f c/prosqlite.o
distclean: clean
	rm -f $(SOBJ)
