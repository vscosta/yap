#
# default base directory for YAP installation
# (EROOT for architecture-dependent files)
#
GCC = yes
prefix = /home/tgomes
exec_prefix = ${prefix}
ROOTDIR = $(prefix)
EROOTDIR = ${prefix}
abs_top_builddir = /home/tgomes/yap-6.3
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

# normal
#CXXFLAGS= -std=c++0x -shared -fPIC -O3 -fomit-frame-pointer -Wall -g -O2 $(YAP_EXTRAS) $(DEFS) -D_YAP_NOT_INSTALLED_=1 -I$(srcdir) -I../../../.. -I$(srcdir)/../../../../include  -DNDEBUG

# debug 
CXXFLAGS= -std=c++0x -shared -fPIC -O3 -fomit-frame-pointer -Wall -g -O2 $(YAP_EXTRAS) $(DEFS) -D_YAP_NOT_INSTALLED_=1 -I$(srcdir) -I../../../.. -I$(srcdir)/../../../../include  -g -O0 -Wextra


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
	$(srcdir)/GraphicalModel.h \
	$(srcdir)/BayesNet.h \
	$(srcdir)/BayesNode.h \
	$(srcdir)/ElimGraph.h \
	$(srcdir)/FactorGraph.h \
	$(srcdir)/Factor.h \
	$(srcdir)/CFactorGraph.h \
	$(srcdir)/ConstraintTree.h \
	$(srcdir)/Solver.h \
	$(srcdir)/VarElimSolver.h \
	$(srcdir)/BnBpSolver.h \
	$(srcdir)/FgBpSolver.h \
	$(srcdir)/CbpSolver.h \
	$(srcdir)/FoveSolver.h \
	$(srcdir)/VarNode.h \
	$(srcdir)/Indexer.h \
	$(srcdir)/Parfactor.h \
	$(srcdir)/ProbFormula.h \
	$(srcdir)/Histogram.h \
	$(srcdir)/ParfactorList.h \
	$(srcdir)/LiftedUtils.h \
	$(srcdir)/TinySet.h \
	$(srcdir)/Util.h \
	$(srcdir)/Horus.h \
	$(srcdir)/xmlParser/xmlParser.h
	
CPP_SOURCES = \
	$(srcdir)/BayesNet.cpp \
	$(srcdir)/BayesNode.cpp \
	$(srcdir)/ElimGraph.cpp \
	$(srcdir)/FactorGraph.cpp \
	$(srcdir)/Factor.cpp \
	$(srcdir)/CFactorGraph.cpp \
	$(srcdir)/ConstraintTree.cpp \
	$(srcdir)/VarNode.cpp \
	$(srcdir)/Solver.cpp \
	$(srcdir)/VarElimSolver.cpp \
	$(srcdir)/BnBpSolver.cpp \
	$(srcdir)/FgBpSolver.cpp \
	$(srcdir)/CbpSolver.cpp \
	$(srcdir)/FoveSolver.cpp \
	$(srcdir)/Parfactor.cpp \
	$(srcdir)/ProbFormula.cpp \
	$(srcdir)/Histogram.cpp \
	$(srcdir)/ParfactorList.cpp \
	$(srcdir)/LiftedUtils.cpp \
	$(srcdir)/Util.cpp \
	$(srcdir)/HorusYap.cpp \
	$(srcdir)/HorusCli.cpp \
	$(srcdir)/xmlParser/xmlParser.cpp

OBJS = \
	BayesNet.o \
	BayesNode.o \
	ElimGraph.o \
	FactorGraph.o \
	Factor.o \
	CFactorGraph.o \
	ConstraintTree.o \
	VarNode.o \
	Solver.o \
	VarElimSolver.o \
	BnBpSolver.o \
	FgBpSolver.o \
	CbpSolver.o \
	FoveSolver.o \
  Parfactor.o \
  ProbFormula.o \
	Histogram.o \
  ParfactorList.o \
	LiftedUtils.o \
	Util.o \
	HorusYap.o

HCLI_OBJS = \
	BayesNet.o \
	BayesNode.o \
	ElimGraph.o \
	FactorGraph.o \
	Factor.o \
	CFactorGraph.o \
	ConstraintTree.o \
	VarNode.o \
	Solver.o \
	VarElimSolver.o \
	BnBpSolver.o \
	FgBpSolver.o \
	CbpSolver.o \
  FoveSolver.o \
  Parfactor.o \
  ProbFormula.o \
	Histogram.o \
  ParfactorList.o \
	LiftedUtils.o \
	Util.o \
	xmlParser/xmlParser.o \
	HorusCli.o

SOBJS=horus.so


all: $(SOBJS) hcli

# default rule
%.o : $(srcdir)/%.cpp
	$(CXX) -c $(CXXFLAGS) $< -o $@


xmlParser/xmlParser.o : $(srcdir)/xmlParser/xmlParser.cpp
	$(CXX) -c $(CXXFLAGS) $< -o $@


horus.so: $(OBJS)
	g++ -shared -export-dynamic -o horus.so $(OBJS)  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)


hcli: $(HCLI_OBJS)
	$(CXX) -o hcli $(HCLI_OBJS)


install: all
	$(INSTALL_PROGRAM) $(SOBJS) $(DESTDIR) $(YAPLIBDIR)


clean:
	rm -f *.o *~ $(OBJS) $(SOBJS) *.BAK hcli xmlParser/*.o


erase_dots:
	rm -f *.dot *.png


depend: $(HEADERS) $(CPP_SOURCES)
	-@if test "$(GCC)" = yes; then\
	  $(CC) -std=c++0x -MM -MG $(CFLAGS) -I$(srcdir) -I$(srcdir)/../../../../include -I$(srcdir)/../../../../H $(CPP_SOURCES) >> Makefile;\
	else\
          makedepend -f - -- $(CFLAGS) -I$(srcdir)/../../../../H -I$(srcdir)/../../../../include -- $(CPP_SOURCES) |\
          sed 's|.*/\([^:]*\):|\1:|' >> Makefile ;\
	fi

# DO NOT DELETE THIS LINE -- make depend depends on it.

BayesNet.o: BayesNet.cpp xmlParser/xmlParser.h BayesNet.h \
 GraphicalModel.h VarNode.h Horus.h BayesNode.h Util.h
BayesNode.o: BayesNode.cpp BayesNode.h VarNode.h Horus.h
ElimGraph.o: ElimGraph.cpp ElimGraph.h FactorGraph.h GraphicalModel.h \
 VarNode.h Horus.h Factor.h Util.h BayesNet.h BayesNode.h
FactorGraph.o: FactorGraph.cpp FactorGraph.h GraphicalModel.h VarNode.h \
 Horus.h Factor.h Util.h BayesNet.h BayesNode.h
Factor.o: Factor.cpp Factor.h VarNode.h Horus.h Util.h Indexer.h
CFactorGraph.o: CFactorGraph.cpp CFactorGraph.h FactorGraph.h \
 GraphicalModel.h VarNode.h Horus.h Factor.h Util.h
ConstraintTree.o: ConstraintTree.cpp ConstraintTree.h TinySet.h \
 LiftedUtils.h Util.h Horus.h
VarNode.o: VarNode.cpp VarNode.h Horus.h GraphicalModel.h
Solver.o: Solver.cpp Solver.h GraphicalModel.h VarNode.h Horus.h Util.h
VarElimSolver.o: VarElimSolver.cpp VarElimSolver.h Solver.h \
 GraphicalModel.h VarNode.h Horus.h FactorGraph.h Factor.h Util.h \
 BayesNet.h BayesNode.h ElimGraph.h
BnBpSolver.o: BnBpSolver.cpp BnBpSolver.h Solver.h GraphicalModel.h \
 VarNode.h Horus.h BayesNet.h BayesNode.h Util.h Indexer.h
FgBpSolver.o: FgBpSolver.cpp FgBpSolver.h Solver.h GraphicalModel.h \
 VarNode.h Horus.h Factor.h Util.h FactorGraph.h Indexer.h
CbpSolver.o: CbpSolver.cpp CbpSolver.h FgBpSolver.h Solver.h \
 GraphicalModel.h VarNode.h Horus.h Factor.h Util.h FactorGraph.h \
 CFactorGraph.h
FoveSolver.o: FoveSolver.cpp FoveSolver.h ParfactorList.h Parfactor.h \
 ProbFormula.h ConstraintTree.h TinySet.h LiftedUtils.h Util.h Horus.h \
 Histogram.h
Parfactor.o: Parfactor.cpp Parfactor.h ProbFormula.h ConstraintTree.h \
 TinySet.h LiftedUtils.h Util.h Horus.h Histogram.h Indexer.h VarNode.h
ProbFormula.o: ProbFormula.cpp ProbFormula.h ConstraintTree.h TinySet.h \
 LiftedUtils.h Util.h Horus.h
Histogram.o: Histogram.cpp Histogram.h Util.h Horus.h
ParfactorList.o: ParfactorList.cpp ParfactorList.h Parfactor.h \
 ProbFormula.h ConstraintTree.h TinySet.h LiftedUtils.h Util.h Horus.h
LiftedUtils.o: LiftedUtils.cpp LiftedUtils.h TinySet.h Util.h Horus.h \
 ConstraintTree.h
Util.o: Util.cpp Util.h Horus.h Indexer.h VarNode.h GraphicalModel.h
HorusYap.o: HorusYap.cpp ../../../../include/YapInterface.h \
 ../../../../include/yap_structs.h ParfactorList.h Parfactor.h \
 ProbFormula.h ConstraintTree.h TinySet.h LiftedUtils.h Util.h Horus.h \
 BayesNet.h GraphicalModel.h VarNode.h BayesNode.h FactorGraph.h Factor.h \
 FoveSolver.h VarElimSolver.h Solver.h BnBpSolver.h FgBpSolver.h \
 CbpSolver.h CFactorGraph.h ElimGraph.h
HorusCli.o: HorusCli.cpp BayesNet.h GraphicalModel.h VarNode.h Horus.h \
 BayesNode.h FactorGraph.h Factor.h Util.h VarElimSolver.h Solver.h \
 BnBpSolver.h FgBpSolver.h CbpSolver.h CFactorGraph.h
xmlParser.o: xmlParser/xmlParser.cpp xmlParser/xmlParser.h
