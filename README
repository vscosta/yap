

			README for Yap 5.1


This directory contains a release of the Yap 5.1.* Prolog system,
originally developed at the Universidade do Porto by Luis Damas and
Vitor Santos Costa, with contributions from the Edinburgh Prolog
library, the C-Prolog manual authors, Ricardo Lopes, Ricardo Rocha,
M. Hermenegildo, D. Cabeza, Eric Alphonse, Paulo Moura, Nuno Fonseca,
Jan Wielemaker, Paul Singleton, Fred Dushin, Jan Wielemaker, Markus
Triska, and many others. You should read the rest of this file for
information on what Yap is and for instructions on how to build it.

YAP 52 is known to build with many versions of gcc (<= gcc-2.7.2, >=
gcc-2.8.1, >= egcs-1.0.1, gcc-2.95.*) and on a variety of Linux,
MacOSX, and Unix'es: SunOS 4.1, Solaris 2.*, Irix 5.2, HP-UX 10.20,
Dec Alpha Unix, Linux 1.2 and Linux 2.* (RedHat 4.0 through 5.2,
Debian 2.*) in both the x86 and alpha platforms. It has been built on
Windows XP using cygwin/mingw from Cygnus Solutions. and on MaxO

The main core of the YAP distribution is distributed under a dual
license: the Perl Artistic license 2 and the FSF's LGPL. The YAP
distribution also contains files distributed under the LGPL
exclusively, and under the GPL. 

The YAP distribution includes several packages ported to Yap, such as
Pillow, JPL, CLP(R) and CHR. We would like to take the opportunity to
thank the developers of these packages for their generosity in
allowing YAP to distribute these packages. Any bugs in these packages
are probably our fault.

If you have a question about this software, desire to add code, found
a bug, want to request a feature, or wonder how to get further
assistance, please send e-mail to yap-users@lists.sourceforge.net. To
subscribe to the mailing list or access the list archives, please see
http://lists.sourceforge.net/lists/listinfo/yap-users

Online documentation is available for Yap at:

	http://www.ncc.up.pt/~vsc/Yap/

Recent versions of Yap, including both source and selected binaries,
can be found from this same URL.

1. What is YAP

The Yap Prolog System is a high-performance Prolog compiler developed
at LIACC, Universidade do Porto. Yap provides several important
features:

 o speed: Yap is widely considered one of the fastest available Prolog
systems.

 o functionality: it supports stream I/O, sockets, modules,
exceptions, Prolog debugger, C-interface, dynamic code, internal
database, DCGs, saved states, co-routining, arrays.

 o we explicitly allow both commercial and non-commercial use of Yap.

Yap is based on the David H. D. Warren's WAM (Warren Abstract
Machine), with several optimizations for better performance. Yap
follows the Edinburgh tradition, and was originally designed to be
largely compatible with DEC-10 Prolog, Quintus Prolog, and especially
with C-Prolog.

Yap implements most of the ISO-Prolog standard. We are striving at
full compatibility, and the manual describes what is still
missing. The manual also includes a (largely incomplete) comparison
with SICStus Prolog.
 
2. How to compile YAP

To compile YAP just do:

 (1) mkdir arch

 (2) cd arch

 (3) ../configure

 (4) check the Makefile for any extensions or changes you want to make.

 (5) make

 (6) If the compilation succeeds, try ./yap

 (7) Happy? make install

 (8) "make install_info" will create the info files in the standard
     info directory.

 (9) "make html" will create documentation in html format in the
     current directory.

In most systems you will need to be superuser in order to do "make
install" and "make info" on the standard directories.

2.1 Where to install Yap

 YAP uses autoconf. Recent versions of Yap try to follow GNU
conventions on where to place software. You can use the --prefix
configure option to set the ROOTDIR macro and the --exec-prefix option
to set the EROOTDIR macro (for architecture-dependent files). EROOTDIR
defaults to ROOTDIR.

  o The main executable is placed at BINDIR, which defaults at
$(EROOTDIR)/bin. This executable is actually a script that calls the
Prolog engine, stored at YAPLIBDIR.

  o SHAREDIR is the directory where the Prolog libraries are stored.
Its default value is $(ROOTDIR)/share. The Prolog libraries are
machine-independent text files that only need to be installed once,
even on systems where multiple copies of Yap of the same version are
installed. (e.g. with different configurations or for different
architectures sharing a file server.)

  o LIBDIR is the directory where binary libraries are stored. It is
set to $(EROOTDIR)/lib by default. YAPLIBDIR is a subdirectory (by
default $(EROOTDIR)/lib/Yap) that contains the Prolog engine and the
binary Prolog libraries.

  o INCLUDEDIR is used if you want to use Yap as a library.

  o INFODIR is where the info help files will be stored. It defaults
to $(SHAREDIR)/info.

2.2 Which Yap to compile

Compiling Yap with the standard options give you a plain vanilla
Prolog. You can tune Yap to use extra functionality by using the
following options to configure:

 o --enable-rational-trees=yes gives you support for infinite rational
trees (enabled by default).

 o --enable-coroutining=yes gives you support for coroutining,
including freezing of goals, attributed variables, and
constraints. This will also enable support for infinite rational
trees  (enabled by default).

 o --enable-depth-limit=yes allows depth limited evaluation, say for
implementing iterative deepening.

 o --enable-low-level-tracer=yes allows support for tracing all calls,
retries, and backtracks in the system. This can help in debugging your
application, but results in performance loss.

 o --enable-wam-profile=yes allows profiling of abstract machine
instructions. This is useful when developing YAP, should not be very
useful for normal users.

 o --enable-parallelism={env-copy,sba,a-cow} allows or-parallelism
supported by one of these three forms. This is still highly experimental.

 o --enable-tabling=yes allows tabling support. This is still experimental.

2.3 Porting Yap

The system has been mainly tested with GCC, but we have been able to
compile versions of Yap under lcc in Linux, Sun's cc compiler, IBM's
xlc, SGI's cc, HP's cc, and Microsoft's Visual C++ 6.0.

2.3.1 Yap and GCC

Yap has been developed to take advantage of GCC (but not to depend on
it). The major advantage of GCC is threaded code and register
reservation. 

YAP is set by default to compile with the best compilation flags we
know. Even so, a few specific options can be used depending on the
architecture:

2.3.1.1 x86

The flag:

	YAP_EXTRAS= ... -DBP_FREE=1

tells us to use the %bp register (frame-pointer) as the emulator's
program counter. This seems to be stable and should become default
real soon now.

2.3.1.2 Sparc/Solaris2

Use:

	YAP_EXTRAS= ...   -mno-app-regs -DOPTIMISE_ALL_REGS_FOR_SPARC=1

and YAP will get two extra registers! This trick does not work on
SunOS 4 machines.

2.3.1.3 Tuning GCC/Yap for your machine model

Versions of GCC can be tweaked for different processors, eg, 486,
Pentium, PentiumPro, Ultrasparc, Supersparc. Unfortunately, some of
these tweaks do may make Yap run slower or not at all in other machines
with the same instruction set, so they cannot be made default.

The best options also depends on the version of GCC you are using, and
it is a good idea to consult the GCC manual under the menus "Invoking
GCC"/"Submodel Options". We next describe a few:

(a) GCC up to 2.7.*:

	o 486:

		YAP_EXTRAS= ... -m486 -DBP_FREE=1

	to take advantage of 486 specific optimisations in GCC 2.7.*.

	o Pentium:

		YAP_EXTRAS= ... -m486 -malign-loops=2 -malign-jumps=2 -malign-functions=2

	o PentiumPros are known not to require alignment.

	o Super and UltraSparcs:
		 YAP_EXTRAS= ... -msupersparc

(b) GCC 2.8.*, EGCS, GCC 2.95.*

	o Check -march=XXX for fun.

