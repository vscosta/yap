#
# default base directory for YAP installation
# (EROOT for architecture-dependent files)
#
GCC = yes
prefix = /usr/local
exec_prefix = ${prefix}
ROOTDIR = $(prefix)
EROOTDIR = ${prefix}
abs_top_builddir = /home/tiago/yap
#
# where the binary should be
#
BINDIR = $(EROOTDIR)/bin
#
# where YAP should look for libraries
#
LIBDIR=${exec_prefix}/lib
YAPLIBDIR=${exec_prefix}/lib/Yap
#
#
CC=gcc
CXX=g++
CXXFLAGS= -shared -fPIC -DBP_FREE -O3 -fomit-frame-pointer -Wall -g -O2 $(YAP_EXTRAS) $(DEFS) -D_YAP_NOT_INSTALLED_=1 -I$(srcdir) -I../../../.. -I$(srcdir)/../../../../include  
#
#
# You shouldn't need to change what follows.
#
INSTALL=/usr/bin/install -c
INSTALL_DATA=${INSTALL} -m 644
INSTALL_PROGRAM=${INSTALL}
SHELL=/bin/sh
RANLIB=ranlib
srcdir=.
SO=so
#4.1VPATH=.:./OPTYap
CWD=$(PWD)


HEADERS = \
	$(srcdir)/BayesianNetwork.h \
	$(srcdir)/BayesianNode.h \
	$(srcdir)/BpNetwork.h \
	$(srcdir)/BpNode.h \
	$(srcdir)/Distribution.h \
	$(srcdir)/CptEntry.h \
	$(srcdir)/BifInterface.h \
	$(srcdir)/xmlParser/xmlParser.h
	
CPP_SOURCES = \
	$(srcdir)/BayesianNetwork.cpp \
	$(srcdir)/BayesianNode.cpp \
	$(srcdir)/BpNetwork.cpp \
	$(srcdir)/BpNode.cpp \
 	$(srcdir)/Distribution.cpp \
	$(srcdir)/CptEntry.cpp \
	$(srcdir)/Horus.cpp \
	$(srcdir)/BifInterface.cpp \
	$(srcdir)/BifTest.cpp \
	$(srcdir)/xmlParser/xmlParser.cpp

OBJS = \
	BayesianNetwork.o \
	BayesianNode.o \
	BpNetwork.o \
	BpNode.o \
    Distribution.o \
	CptEntry.o \
	Horus.o

BIF_OBJS = \
	BayesianNetwork.o \
	BayesianNode.o \
    BpNetwork.o \
    BpNode.o \
    Distribution.o \
	CptEntry.o \
    BifInterface.o \
    BifTest.o \
    xmlParser/xmlParser.o

SOBJS=horus.so


all: $(SOBJS) biftest

# default rule
%.o : $(srcdir)/%.cpp
	$(CXX) -c $(CXXFLAGS) $< -o $@

horus.so: $(OBJS)
	g++ -shared -export-dynamic -o horus.so $(OBJS)

biftest: $(BIF_OBJS)
	$(CXX) -o biftest $(BIF_OBJS)


install: all
	$(INSTALL_PROGRAM) $(SOBJS) $(DESTDIR) $(YAPLIBDIR)


clean:
	rm -f *.o *~ $(OBJS) $(SOBJS) *.BAK biftest xmlParser/*.o


depend: $(HEADERS) $(CPP_SOURCES)
	-@if test "$(GCC)" = yes; then\
	  $(CC) -MM -MG $(CFLAGS) -I$(srcdir) -I$(srcdir)/../../../../include -I$(srcdir)/../../../../H $(CPP_SOURCES) >> Makefile;\
	else\
          makedepend -f - -- $(CFLAGS) -I$(srcdir)/../../../../H -I$(srcdir)/../../../../include -- $(CPP_SOURCES) |\
          sed 's|.*/\([^:]*\):|\1:|' >> Makefile ;\
	fi

# DO NOT DELETE THIS LINE -- make depend depends on it.

