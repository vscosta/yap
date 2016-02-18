

README for YAP6
====================

NOTE: this version of YAP is stil experimental, documentation may be out of date.

This directory contains a release of the YAP 6.3.* Prolog system,
originally developed at the Universidade do Porto by Luis Damas and
Vitor Santos Costa. YAP contains the SWI-Prolog I/O library anad a
number of SWI-Prolog packages, originally developed by Jan Wielemaker
and other. It includes contributions from the Edinburgh Prolog
library, the C-Prolog manual authors, Ricardo Lopes, Ricardo Rocha,
M. Hermenegildo, D. Cabeza, Eric Alphonse, Paulo Moura, Nuno Fonseca,
Jan Wielemaker, Paul Singleton, Fred Dushin, Markus Triska, Ulrich
Neumerkel, and many others. You should read the rest of this file for
information on what YAP is and for instructions on how to build it.

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

## What is YAP

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
full compatibility. Moreover, we are working on integrating YAP on
multi-programming language environments.
 
## Obtaining YAP's development sources.

YAP is now being maintained using the git source management system. A
public repository is available at 

https://github.com/vscosta/yap-6.3

Please use

git clone git://git.code.sf.net/p/yap/yap-6.3

to obtain a copy of the current YAP tree.

Notice that the current version of YAP does not use submodules

3. How to compile YAP

First, make sure you have gmp and readline *development* packages
installed (that is, the headers and libraries). If you are in a WIN32
environment you will still need GMP.

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

In most systems you will need to be superuser so that  "make
install"  can write on the standard system directories.

# Where to install YAP

 YAP now uses cmake. Still, YAP try to follow GNU conventions on where
to place software. By default, the home location is /usr/local on Unix,
Linux, and OS/X machines.

You can use the `CMAKE_INSTALL_PREFIX` variable to set the YAP default
home directory. Use the graphical interface to cmake, or from the command line

~~~~
cmake -DCMAKE_INSTALL_PREFIX=/home/xpto
~~~~


# Which YAP to compile

Compiling YAP with the standard options give you a Prolog with
tabling. You can tune YAP to use extra functionality by using the
following options to configure:

Compilation options:

   * -`DWITH_Threads=ON` enables POSIX thread support. The threads library
 mostly follows the SWI design: each thread has its own stack, and
 they all share the same data-base.

  *   -`DWITH_CALL_TRACER=ON` allows support for tracing all calls,
retries, and backtracks in the system. This can help in debugging a
application, but results in performance loss. It is default in debug mode.

 
# Packages and Interface Libraries

YAP includes a number of interface libraries. Some, like the Java
 interface JPL, have been ported from other Prologs (often from
 SWI-Prolog). Others, like the python and swig interface, have been
 developed within YAP.

As an example:
 
 `-DWITH_CUDD` enables compilation of the CUDD library, used by
 packages such as PFL/CLP(BN) and ProbLog. The source of the CUDD
 package is available from:

  http://vlsi.colorado.edu/~fabio/CUDD

 Fedora Linux and MacPorts have cudd packages. In the case of Fedora,
 install cudd-devel. Ask vsc@dcc.fc.up.pt for a WIN32/WIN64 port 

The cmake graphical interface lists all currently available external packages.



