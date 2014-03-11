

			README for YAP6


This directory contains a release of the YAP 6.3.* Prolog system,
originally developed at the Universidade do Porto by Luis Damas and
Vitor Santos Costa. YAP contains the SWI-Prolog I/O library anad a
number of SWI-Prolog packages, originally developed by Jan Wielemaker
and other. It includes contributions from the Edinburgh Prolog
library, the C-Prolog manual authors, Ricardo Lopes, Ricardo Rocha,
M. Hermenegildo, D. Cabeza, Eric Alphonse, Paulo Moura, Nuno Fonseca,
Jan Wielemaker, Paul Singleton, Fred Dushin, Markus Triska, and many
others. You should read the rest of this file for information on what
YAP is and for instructions on how to build it.

YAP 6 has been built with several versions on GCC on a variety of
Linux, BDS, and MacOSX configurations. It has been built on Windows7
using the mingw toolkit and cygwin from Cygnus Solutions.

The main core of the YAP distribution is distributed under a dual
license: the Perl Artistic license 2 and the FSF's LGPL. The YAP
distribution also contains files distributed under the LGPL
exclusively, and under the GPL.

The YAP distribution includes several packages ported to YAP. We would
like to take the opportunity to thank the developers of these packages
for their generosity in allowing YAP to distribute these packages. Any
bugs in these packages are probably our fault.

If you have a question about this software, desire to add code, found
a bug, want to request a feature, or wonder how to get further
assistance, please send e-mail to yap-users@lists.sourceforge.net. To
subscribe to the mailing list or access the list archives, please see
http://lists.sourceforge.net/lists/listinfo/yap-users

Online documentation is available for YAP at:

	http://www.dcc.fc.up.pt/~vsc/YAP/

Recent versions of YAP, including both source and selected binaries,
can be found from this same URL.

1. What is YAP

The YAP Prolog System is a high-performance Prolog compiler developed
at LIACC, Universidade do Porto. YAP provides several important
features:

 o speed: YAP is widely considered one of the fastest available Prolog
systems.

 o functionality: it supports stream I/O, sockets, modules,
exceptions, Prolog debugger, C-interface, dynamic code, internal
database, DCGs, saved states, co-routining, arrays.

 o we explicitly allow both commercial and non-commercial use of YAP.

YAP is based on the David H. D. Warren's WAM (Warren Abstract
Machine), with several optimizations for better performance. YAP
follows the Edinburgh tradition, and was originally designed to be
largely compatible with DEC-10 Prolog, Quintus Prolog, and especially
with C-Prolog.

YAP implements most of the ISO-Prolog standard. We are striving at
full compatibility, and the manual describes what is still
missing. The manual also includes a (largely incomplete) comparison
with SICStus Prolog.
 
2. Obtaining YAP's development sources.

YAP is now being maintained using the git source management system. A
public repository is available at 

http://sourceforge.net/p/yap/yap-6.3/ci/master/tree/

Please use

git clone git://git.code.sf.net/p/yap/yap-6.3

to obtain a copy of the current YAP tree.

3. How to compile YAP

First, make sure you have gmp and readline development packages
installed. If you plan to use ProbLog, also check for cudd.

Now, to compile YAP from the source directory just do:

 (1) ./configure

 (2) check the Makefile for any extensions or changes you want to make.

 (3) make

 (4) If the compilation succeeds, try ./yap

 (5) Happy? "make install"

 (6) "make install_info" will create the info files in the standard
     info directory.

 (7) "make html" will create documentation in html format in the
     current directory.

In most systems you will need to be superuser in order to do "make
install" and "make info" on the standard system directories.

3.1 Where to install YAP

 YAP uses autoconf. Recent versions of YAP try to follow GNU
conventions on where to place software. By default, this location is
/usr/local on Unix, Linux, and OS/X machines. 

You can use the --prefix configure option to set the ROOTDIR macro and
the --exec-prefix option to set the EROOTDIR macro (for
architecture-dependent files). EROOTDIR defaults to ROOTDIR.

  o The main executable is placed at BINDIR, which defaults at
$(EROOTDIR)/bin.

  o SHAREDIR is the directory where the Prolog libraries are stored.
Its default value is $(ROOTDIR)/share. The Prolog libraries are
machine-independent text files that only need to be installed once,
even on systems where multiple copies of YAP of the same version are
installed. (e.g. with different configurations or for different
architectures sharing a file server.)

  o LIBDIR is the directory where binary libraries are stored. It is
set to $(EROOTDIR)/lib by default. YAPLIBDIR is a subdirectory (by
default $(EROOTDIR)/lib/YAP) that contains the Prolog engine and the
binary Prolog libraries.

  o INCLUDEDIR is used if you want to use YAP as a library.

  o INFODIR is where the info help files will be stored. It defaults
to $(SHAREDIR)/info.

3.2 Which YAP to compile

Compiling YAP with the standard options give you a plain vanilla
Prolog. You can tune YAP to use extra functionality by using the
following options to configure:

Compilation options:

 o --enable-tablingt=yes allows tabled evaluation (default option)

 o --enable-depth-limit=yes allows depth limited evaluation, say for
implementing iterative deepening. It is required by the ILP system
Aleph. (default option)

 o --enable-use-malloc=yes makes YAP use the system's C-library
 allocation routines for all memory allocation (default).

 o --enable-threads=yes enables POSIX thread support.

 o --enable-pthread-locking=yes always use POSIX p-thread routines for locking.

 o --enable-low-level-tracer=yes allows support for tracing all calls,
retries, and backtracks in the system. This can help in debugging your
application, but results in performance loss.

 o --enable-wam-profile=yes allows profiling of abstract machine
instructions. This is useful when developing YAP, should not be very
useful for normal users.

 o --enable-or-parallelism={env-copy,sba,a-cow} allows or-parallelism
supported by one of these three forms. This is still experimental.

 o --enable-dynamic-loading compile YAP as as a shared library.

 o --enable-cygwin compile YAP as a CYGWIN environment application, and not as stand-alone application.

 o --with-gmp[=GMP_PATH] provide a path for GMP, in case GMP is not
 installed in the system. In a Linux machine, make sure you have
 installed the gmp-dev package. Binary versions of GMP are available
 for WIN32.

 o --enable-condor compile YAP as a static program that can be run by
 condor. condor_compile must be in the system path.

Packages and Interface Libraries

 o --with-cudd[=CUDD_PATH] enables compilation of the CUDD library,
 used by packages such as PFL/CLP(BN) and ProbLog. The source of the
 CUDD package is availablr from:

  http://vlsi.colorado.edu/~fabio/CUDD

 Fedora Linux and MacPorts have cudd packages. In the case of Fedora,
 install cudd-devel. Ask vsc@dcc.fc.up.pt for a WIN32/WIN64 port 

 o --with-java[=JAVA_PATH] enables the Java Interface Library JPL.

 o --enable-chr installs the Leuven CHR language implementation
 (enabled by default, if package/chr exists)

 o --enable-clpqr installs the Leuven CLPQR port, originally developed
 for SWI-Prolog (enabled by default, if Cpackage/clpqe exists)

 o --enable-myddas[=PATH] installs the MYDDAS MYSQL and/or ODBC interface
 package (enabled by default, if ODBC and/or MySql
 development files can be found). It also enables support for the
 SWI-Prolog ODBC package.

 o --with-matlab[=PATH] installs the MATLAB interface (not
 default). You need to provide the full path of the MATLAB libraries.

 o --with-mpi[=PATH] installs the  MPI interface. This is default,
 if the development libraries can be found.

 o --with-gecode[=PATH] installs the  GeCode Solver interface. This is default,
 if gecode libraries can be found.

 o --with-R[=PATH] installs the Real interface to the R language.

 o --with-python[=PATH] installs a Python interface

