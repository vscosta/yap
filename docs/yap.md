	
This file documents the YAP Prolog System version 6.3.4, a high-performance Prolog compiler developed at LIACC, Universidade do Porto. YAP is based on David H. D. Warren's WAM (Warren Abstract Machine), with several optimizations for better performance. YAP follows the Edinburgh tradition, and is largely compatible with DEC-10 Prolog, Quintus Prolog, and especially with C-Prolog.

  + @subpage Download where to download YAP for your platform.
  
  + @subpage Install discusses how to compile and install YAP.

  + @subpage Syntax describes the syntax of YAP.

  + @subpage Run describes how to invoke YAP

  + @subpage Loading_Programs presents the main predicates and
  directives available to load files and to control the Prolog environment.
        + @subpage abs_file_name explains how to find a file full path.

  + @subpage BuilthYins describes predicates providing core YAP
    functionality. Examples include 
        + @subpage Arithmetic describes the arithmetic predicates

        + @subpage Control describes the predicates for controlling the execution of Prolog programs.

        + @subpage Testing_Terms describes the main predicates on terms

        + @subpage  Input_Output goes into Input/Ouput.

        + @subpage Database discusses the clausal data-base

        + @subpage Sets Collecting Solutions to a Goal

        + @subpage Grammars presents Grammar rules in Prolog that are
          both a  convenient way to express definite clause grammars and
          an extension of the well known context-free grammars.

        + @subpage OS discusses access to Operating System
        functionality

        + @subpage Term_Modification Global and Mutable Terms
	   
        + @subpage Profiling Profiling Prolog Programs
	   

  + Libraries
        + @subpage maplist introduces macros to apply an operation over
          all elements of a list

        + @subpage Apply Apply Macros
		
        + @subpage Association_Lists Association Lists
		
        + @subpage AVL_Trees AVL Trees
		
        + @subpage  Exo_Intervals Exo Intervals
		
        + @subpage  Gecode Gecode Interface
		
        + @subpage Heaps Heaps
		
        + @subpage Lists List Manipulation 
		
        + @subpage LineUtilities Line Manipulation Utilities
		
        + @subpage matrix Matrix Library
		
        + @subpage NonhYBacktrackable_Data_Structures Non-Backtrackable Data Structures
		
        + @subpage Ordered_Sets Ordered Sets
		
        + @subpage Pseudo_Random Pseudo Random Number Integer Generator
		
        + @subpage Queues Queues
		
        + @subpage Random Random Number Generator
		
        + @subpage Read_Utilities Read Utilities
		
        + @subpage RedhYBlack_Trees Red-Black Trees
		
        + @subpage RegExp Regular Expressions
		
        + @subpage shlib SWI-Prolog's shlib library
		
        + @subpage Splay_Trees Splay Trees
		
        + @subpage String_InputOutput Reading From and Writing To Strings
		
        + @subpage System Calling The Operating System from YAP
		
        + @subpage Terms Utilities On Terms
		
        + @subpage Tries Trie DataStructure
		
        + @subpage Cleanup Call Cleanup
		
        + @subpage Timeout Calls With Timeout
		
        + @subpage Trees Updatable Binary Trees
		
        + @subpage UGraphs Unweighted Graphs
		
        + @subpage DGraphs Directed Graphs
		
        + @subpage UnDGraphs Undirected Graphs
		
        + @subpage DBUsage Memory Usage in Prolog Data-Base
		
        + @subpage Lambda Lambda Expressions
		
        + @subpage LAM LAM
		
        + @subpage BDDs Binary Decision Diagrams and Friends
		
        + @subpage Block_Diagram Block Diagram
		
        + @subpage  Invoking_Predicates_on_all_Members_of_a_List Invoking Predicates on all Members of a List
		
        + @subpage  Forall Forall
		
\author Vitor Santos Costa,
\author Luís Damas,
\author Rogério Reis
\author Rúben Azevedo


© 1989-2014 L. Damas, V. Santos Costa and Universidade
do Porto.
Permission is granted to make and distribute verbatim copies of this manual provided the copyright notice and this permission notice are preserved on all copies.
Permission is granted to copy and distribute modified versions of this manual under the conditions for verbatim copying, provided that the entire resulting derived work is distributed under the terms of a permission notice identical to this one.
Permission is granted to copy and distribute translations of this manual into another language, under the above conditions for modified versions.

\htmlonly

This file contains extracts of the SWI-Prolog manual, as written by Jan
Wielemaker. Our thanks to the author for his kind permission in allowing
us to include his text in this document.

\endhtmlonly

@section Intro Introduction

This document provides User information on version 6.3.4 of
YAP (<em>Yet Another Prolog</em>). The YAP Prolog System is a
high-performance Prolog compiler developed at LIACC, Universidade do
Porto. YAP provides several important features:

<ul>
 <li>Speed: YAP is widely considered one of the fastest available
Prolog systems.

</li>
 <li>Functionality: it supports stream Input/Output, sockets, modules,
exceptions, Prolog debugger, C-interface, dynamic code, internal
database, DCGs, saved states, co-routining, arrays, threads.

</li>
 <li>We explicitly allow both commercial and non-commercial use of YAP.
</li>
</ul>

YAP is based on the David H. D. Warren's WAM (Warren Abstract Machine),
with several optimizations for better performance. YAP follows the
Edinburgh tradition, and was originally designed to be largely
compatible with DEC-10 Prolog, Quintus Prolog, and especially with
C-Prolog.

YAP implements most of the ISO-Prolog standard. We are striving at
full compatibility, and the manual describes what is still
missing. The manual also includes a (largely incomplete) comparison
with SICStus Prolog.

The document is intended neither as an introduction to Prolog nor to the
implementation aspects of the compiler. A good introduction to
programming in Prolog is the book @cite TheArtOfProlog , by
L. Sterling and E. Shapiro, published by "The MIT Press, Cambridge
MA". Other references should include the classical @cite ProgrammingInProlog , by W.F. Clocksin and C.S. Mellish, published by
Springer-Verlag.

YAP 4.3 is known to build with many versions of gcc (\<= gcc-2.7.2, \>=
gcc-2.8.1, \>= egcs-1.0.1, gcc-2.95.\*) and on a variety of Unixen:
SunOS 4.1, Solaris 2.\*, Irix 5.2, HP-UX 10, Dec Alpha Unix, Linux 1.2
and Linux 2.\* (RedHat 4.0 thru 5.2, Debian 2.\*) in both the x86 and
alpha platforms. It has been built on Windows NT 4.0 using Cygwin from
Cygnus Solutions (see README.nt) and using Visual C++ 6.0.

The overall copyright and permission notice for YAP4.3 can be found in
the Artistic file in this directory. YAP follows the Perl Artistic
license, and it is thus non-copylefted freeware.

If you have a question about this software, desire to add code, found a
bug, want to request a feature, or wonder how to get further assistance,
please send e-mail to <yap-users AT lists.sourceforge.net>.  To
subscribe to the mailing list, visit the page
<https://lists.sourceforge.net/lists/listinfo/yap-users>.

On-line documentation is available for YAP at:

<http://www.ncc.up.pt/~vsc/YAP/>

Recent versions of YAP, including both source and selected binaries,
can be found from this same URL.

This manual was written by Vítor Santos Costa,
Luís Damas, Rogério Reis, and Rúben Azevedo. The
manual is largely based on the DECsystem-10 Prolog User's Manual by
D.L. Bowen, L. Byrd, F. C. N. Pereira, L. M. Pereira, and
D. H. D. Warren. We have  used comments from the Edinburgh Prolog
library written by R. O'Keefe. Documentation from many built-ins is
originally from the SWI-Prolog manual, with the gracious uathorization
from
Jan Wielemaker. We would also like to gratefully
acknowledge the contributions from Ashwin Srinivasian.

We are happy to include in YAP several excellent packages developed
under separate licenses. Our thanks to the authors for their kind
authorization to include these packages.

The packages are, in alphabetical order:

<ul>
 <li>The CHR package developed by Tom Schrijvers,
Christian Holzbaur, and Jan Wielemaker.

</li>
<li>The CLP(BN) package and Horus toolkit developed by Tiago Gomes, and Vítor Santos Costa.

</li>
 <li>The CLP(R) package developed by Leslie De Koninck, Bart Demoen, Tom
Schrijvers, and Jan Wielemaker, based on the CLP(Q,R) implementation
by Christian Holzbaur.

</li>
<li>The CPLint package developed by Fabrizio Riguzzi's research
laboratory at the University of Ferrara. Please see

<http://www.ing.unife.it/Docenti/FabrizioRiguzzi/>


</li>
<li>The CUDA interface package developed by Carlos Martínez, Jorge
Buenabad, Inês Dutra and Vítor Santos Costa.

</li>
<li>The GECODE interface package developed by  Denys Duchier and Vítor Santos Costa.

</li>
<li>The JPL (Java-Prolog Library) package developed by .

</li>
<li>The Logtalk Object-Oriented system is developed at the University 
	of Beira Interior, Portugal, by Paulo Moura:

	<http://logtalk.org/>

	Logtalk is no longer distributed with YAP. Please use the Logtalk standalone 
	installer for a smooth integration with YAP.

	</li>
 <li>The minisat SAT solver interface developed by Michael Codish,
 Vitaly Lagoon, and Peter J. Stuckey.

 <li>The MYDDAS relational data-base interface developed at the
 Universidade do Porto by Tiago Soares, Michel Ferreira, and Ricardo Rocha.

</li>  <li>The PRISM logic-based
programming system for statistical modeling developed at the Sato
Research Laboratory, TITECH, Japan.

</li> <li>The ProbLog 1 system developed by the ProbLog team in the
DTAI group of KULeuven. For general information on ProbLog 1 and 2, please see
    
    <http://dtai.cs.kuleuven.be/problog>

</li>
<li>The real R interface package developed by 	Nicos Angelopoulos,
Vítor Santos Costa, João Azevedo, Jan Wielemaker, and Rui Camacho.
</li>

<li>YAP includes the yap2swi library that ports to YAP code from
 of SWI's PL interface. This includes the Input/Output Layer, the SWI
 Foreign Language Interface, and the RDF, archive, clib, http, odbc, plunit,
 semweb, sgml, and zlib packages written by Jan Wielemaker. Please do refer to the SWI-Prolog home page:

<http://www.swi-prolog.org>

for more information on SWI-Prolog and the SWI packages. 

</li>

</ul>

@page Download Downloading YAP

The latest development version of Yap-6 is yap-6.3.4 and can be
obtained from the repositories

<http://sourceforge.net/p/yap/yap-6.3>

and

<https://github.com/vscosta/yap-6.3>

Several packages are shared with SWI-Prolog and need to be obtained
from separate repositories. Proceed as follows:

~~~~~~
cd yap-6.3
git submodule init
git submodule update
~~~~~~

Most of these repositories are basically copies of the original
repositories at the SWI-Prolog site. YAP-6 will work either with or
without these packages.


@page Install Installing YAP

To compile YAP it should be sufficient to:

<ol>
 <li>`autoconf`. Recent versions of YAP try to follow GNU
conventions on where to place software.

<ul>
 <li>The main executable is placed at  _$BINDIR_. This executable is
actually a script that calls the Prolog engine, stored at  _$LIBDIR_.

</li>
 <li>_$LIBDIR_ is the directory where libraries are stored. YAPLIBDIR is a
subdirectory that contains the Prolog engine and a Prolog library.

</li>
 <li>_$INCLUDEDIR_ is used if you want to use YAP as a library.

</li>
 <li>_$INFODIR_ is where to store `info` files. Usually
/usr/local/info, /usr/info, or /usr/share/info.
</li>
</ul>

</li>
 <li>`make`.

</li>
 <li>If the compilation succeeds, try `./yap`.

</li>
 <li>If you feel satisfied with the result, do `make install`.

</li>
 <li>`make install-info` will create the info files in the
standard info directory.

</li>
 <li>`make html` will create documentation in html format in the
predefined directory.

</li>
</ol>
In most systems you will need to be superuser in order to do
`make install` and `make info` on the standard directories.

@section Configuration_Options Tuning the Functionality of YAP 

Compiling YAP with the standard options give you a plain vanilla
Prolog. You can tune YAP to include extra functionality by calling
`configure` with the appropriate options:

<ul>
 <li>`--enable-rational-trees=yes` gives you support for infinite
rational trees.

</li>
 <li>`--enable-coroutining=yes` gives you support for coroutining,
including freezing of goals, attributed variables, and
constraints. This will also enable support for infinite rational
trees.

</li>
 <li>`--enable-depth-limit=yes` allows depth limited evaluation, say for
implementing iterative deepening.

</li>
 <li>`--enable-low-level-tracer=yes` allows support for tracing all calls,
retries, and backtracks in the system. This can help in debugging your
application, but results in performance loss.

</li>
 <li>`--enable-wam-profile=yes` allows profiling of abstract machine
instructions. This is useful when developing YAP, should not be so
useful for normal users.

</li>
 <li>`--enable-condor=yes` allows using the Condor system that
support High Throughput Computing (HTC) on large collections of
distributively owned computing resources.

</li>
 <li>`--enable-tabling=yes` allows tabling support. This option
is still experimental.

</li>
 <li>`--enable-parallelism={env-copy,sba,a-cow}` allows
or-parallelism supported by one of these three forms. This option is
still highly experimental.

</li>
 <li>`--with-max-workers` allows definition of the maximum 
number of parallel processes (its value can be consulted at runtime 
using the flag `max_workers`).

</li>
 <li>`--with-gmp[=DIR]` give a path to where one can find the
`GMP` library if not installed in the default path.

</li>
 <li>`--enable-threads` allows using of the multi-threading 
predicates provided by YAP. Depending on the operating system, the 
option `--enable-pthread-locking` may also need to be used.

</li>
 <li>`--with-max-threads` allows definition of the maximum 
number of threads (the default value is 1024; its value can be consulted 
at runtime using the flag [max_threads](@ref max_threads)).

</li>
</ul>

Next section discusses machine dependent details.

@section Machine_Options Tuning YAP for a Particular Machine and Compiler

The default options should give you best performance under
`GCC`. Although the system is tuned for this compiler
we have been able to compile versions of YAP under lcc in Linux,
Sun's cc compiler, IBM's xlc, SGI's cc, and Microsoft's Visual C++
6.0.

@section Tuning_for_GCC Tuning YAP for `GCC`.

YAP has been developed to take advantage of `GCC` (but not to
depend on it). The major advantage of `GCC` is threaded code and
explicit register reservation.

YAP is set by default to compile with the best compilation flags we
know. Even so, a few specific options reduce portability.  The option 

<ul>
 <li>`--enable-max-performance=yes` will try to support the best
available flags for a specific architectural model. Currently, the option
assumes a recent version of `GCC`.
</li>
 <li>`--enable-debug-yap` compiles YAP so that it can be debugged
by tools such as `dbx` or `gdb`.
</li>
</ul>

Here follow a few hints:

On x86 machines the flags:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
YAP_EXTRAS= ... -DBP_FREE=1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tells us to use the `%bp` register (frame-pointer) as the emulator's
program counter. This seems to be stable and is now default.

On  Sparc/Solaris2 use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
YAP_EXTRAS= ...   -mno-app-regs -DOPTIMISE_ALL_REGS_FOR_SPARC=1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and YAP will get two extra registers! This trick does not work on
SunOS 4 machines.

Note that versions of GCC can be tweaked to recognize different
processors within the same instruction set, e.g. 486, Pentium, and
PentiumPro for the x86; or Ultrasparc, and Supersparc for
Sparc. Unfortunately, some of these tweaks do may make YAP run slower or
not at all in other machines with the same instruction set, so they
cannot be made default.

Last, the best options also depends on the version of GCC you are using, and
it is a good idea to consult the GCC manual under the menus "Invoking
GCC"/"Submodel Options". Specifically, you should check
`-march=XXX` for recent versions of GCC/EGCS. In the case of
`GCC2.7` and other recent versions of `GCC` you can check:

<ul>

 <li>486:
In order to take advantage of 486 specific optimizations in GCC 2.7.\*:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
YAP_EXTRAS= ... -m486 -DBP_FREE=1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>Pentium:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
YAP_EXTRAS= ... -m486 -malign-loops=2 -malign-jumps=2 \
                      -malign-functions=2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>PentiumPro and other recent Intel and AMD machines:
PentiumPros are known not to require alignment. Check your version of
`GCC` for the best `-march` option.

</li>
 <li>Super and UltraSparcs:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
YAP_EXTRAS= ... -msupersparc
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>MIPS: if have a recent machine and you need a 64 bit wide address
space you can use the abi 64 bits or eabi option, as in:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CC="gcc -mabi=64" ./configure --...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Be careful. At least for some versions of `GCC`, compiling with
`-g` seems to result in broken code.

</li>
 <li>WIN32: GCC is distributed in the MINGW32 and CYGWIN packages.

The Mingw32 environment is available from the URL:

<http://www.mingw.org>

You will need to install the `msys` and `mingw`
packages. You should be able to do configure, make and make install.

If you use mingw32 you may want to search the contributed packages for
the `gmp` multi-precision arithmetic library. If you do setup YAP
with `gmp` note that libgmp.dll must be in the path,
otherwise YAP will not be able to execute.

The CygWin environment is available from the URL:

<http://www.cygwin.com>

and mirrors. We suggest using recent versions of the cygwin shell. The
compilation steps under the cygwin shell are as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mkdir cyg
$YAPSRC/configure --enable-coroutining \\
                  --enable-depth-limit \\
                  --enable-max-performance
make
make install
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, YAP will use the `-mno-cygwin` option to
disable the use of the cygwin dll and to enable the mingw32 subsystem
instead. YAP thus will not need the cygwin dll. It instead accesses
the system's CRTDLL.DLL `C` run time library supplied with
Win32 platforms through the mingw32 interface. Note that some older
WIN95 systems may not have CRTDLL.DLL, in this case it should
be sufficient to import the file from a newer WIN95 or WIN98 machine.

You should check the default installation path which is set to
/YAP in the standard Makefile. This string will usually
be expanded into c:\\YAP by Windows.

The cygwin environment does not provide <tt>gmp</tt> on the MINGW
subsystem. You can fetch a dll for the <tt>gmp</tt> library from
<http://www.sf.net/projects/mingwrep>.

It is also possible to configure YAP to be a part of the cygwin
environment. In this case you should use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mkdir cyg
$YAPSRC/configure --enable-max-performance \\
                  --enable-cygwin=yes
make
make install
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
YAP will then compile using the cygwin library and will be installed
in cygwin's /usr/local. You can use YAP from a cygwin console,
or as a standalone application as long as it can find
cygwin1.dll in its path. Note that you may use to use
`--enable-depth-limit` for Aleph compatibility, and that you may
want to be sure that GMP is installed.

</li>
</ul>

@subsection Compiling_Under_Visual_C Compiling Under Visual C++

YAP used to compile cleanly under Microsoft's Visual C++ release 6.0. We next
give a step-by-step review on how the core YAP compiled manually using this
environment.

First, it is a good idea to build YAP as a DLL:

<ol>

 <li>create a project named yapdll using File.New. The project will be a
DLL project, initially empty.

Notice that either the project is named yapdll or you must replace the
preprocessors variable  _$YAPDLL_EXPORTS_ to match your project names
in the files YAPInterface.h and `c_interface.c`.

</li>
 <li>add all .c files in the $YAPSRC/C directory and in the
$YAPSRC\\OPTYAP directory to the Project's `Source Files` (use
FileView).

</li>
 <li>add all .h files in the  _$YAPSRC/H_ directory,
 _$YAPSRC\\include_ directory and in the  _$YAPSRC\\OPTYAP_
subdirectory to the Project's `Header Files`.

</li>
 <li>Ideally, you should now use `m4` to generate extra  .h from .m4 files and use
`configure` to create a `config.h`. Or, you can be lazy, and
fetch these files from  _$YAPSRC\\VC\\include_.

</li>
 <li>You may want to go to `Build.Set Active Configuration` and
set `Project Type` to `Release`

</li>
 <li>To use YAP's own include directories you have to set the Project
option  `Project.Project Settings.C/C++.Preprocessor.Additional Include Directories` to include the directories  _$YAPSRC\\H_,
 _$YAPSRC\\VC\\include_,  _$YAPSRC\\OPTYAP_ and
 _$YAPSRC\\include_.  The syntax is:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$YAPSRC\H, $YAPSRC\VC\include, $YAPSRC\OPTYAP, $YAPSRC\include
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>Build: the system should generate an yapdll.dll and an yapdll.lib.

</li>
 <li>Copy the file yapdll.dll to your path. The file
yapdll.lib should also be copied to a location where the linker can find it.
</li>
</ol>

Now you are ready to create a console interface for YAP:

<ol>
 <li>create a second project  say `wyap` with `File.New`. The project will be a
WIN32 console project, initially empty.

</li>
 <li>add  _$YAPSRC\\console\\yap.c_ to the `Source Files`.

</li>
 <li>add  _$YAPSRC\\VC\\include\\config.h_ and the files in  _$YAPSRC\\include_ to
the `Header Files`.

</li>
 <li>You may want to go to `Build.Set Active Configuration` and set
`Project Type` to `Release`.

</li>
 <li>you will eventually need to bootstrap the system by booting from
`boot.yap`, so write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -b $YAPSRC\pl\boot.yap
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

in `Project.Project Settings.Debug.Program Arguments`.

</li>
 <li>You need the sockets and yap libraries. Add

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ws2_32.lib yapdll.lib
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

to `Project.Project Settings.Link.Object/Library Modules`

You may also need to set the `Link Path` so that VC++ will find `yapdll.lib`.

</li>
 <li>set `Project.Project Settings.C/C++.Preprocessor.Additional Include Directories` to include the 
 _$YAPSRC/VC/include_ and
 _$YAPSRC/include_.

The syntax is:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$YAPSRC\VC\include, $YAPSRC\include
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>Build the system.

</li>
 <li>Use `Build.Start Debug` to boot the system, and then create the saved state with

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
['$YAPSRC\\pl\\init'].
qsave_program('startup.yss').
^Z
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

That's it, you've got YAP and the saved state!
</li>
</ol>

The $YAPSRC\\VC directory has the make files to build YAP4.3.17 under VC++ 6.0.

@subsection Tuning_for_SGI_cc Compiling Under SGI's cc

YAP should compile under the Silicon Graphic's `cc` compiler,
although we advise using the GNUCC compiler, if available.

<ul>
 <li>64 bit
Support for 64 bits should work by using (under Bourne shell syntax):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CC="cc -64" $YAP_SRC_PATH/configure --...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
</li>
</ul>

@page Run Running YAP

We next describe how to invoke YAP in Unix systems.

@section Running_YAP_Interactively Running YAP Interactively

Most often you will want to use YAP in interactive mode. Assuming that
YAP is in the user's search path, the top-level can be invoked under
Unix with the following command:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
yap [-s n] [-h n] [-a n] [-c IP_HOST port ] [filename]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All the arguments and flags are optional and have the following meaning:

<ul>
 <li>-?
print a short error message.
</li>
 <li>-s _Size_
allocate  _Size_ KBytes for local and global stacks. The user may
specify <tt>M</tt> bytes.
</li>
 <li>-h _Size_
allocate  _Size_ KBytes for heap and auxiliary stacks
</li>
 <li>-t _Size_
allocate  _Size_ KBytes for the trail stack
</li>
 <li>-L _Size_ 
SWI-compatible option to allocate  _Size_ K bytes for local and global stacks, the local stack
cannot be expanded. To avoid confusion with the load option,  _Size_
must immediately follow the letter `L`.
</li>
 <li>-G _Size_
SWI-compatible option to allocate  _Size_ K bytes for local and global stacks; the global
stack cannot be expanded
</li>
 <li>-T _Size_
SWI-compatible option to allocate  _Size_ K bytes for the trail stack; the trail cannot be expanded.
</li>
 <li>-l  _YAP_FILE_
compile the Prolog file  _YAP_FILE_ before entering the top-level.
</li>
 <li>-L  _YAP_FILE_
compile the Prolog file  _YAP_FILE_ and then halt. This option is
useful for implementing scripts.
</li>
 <li>-g  _Goal_
run the goal  _Goal_ before top-level. The goal is converted from
an atom to a Prolog term.
</li>
 <li>-z  _Goal_
run the goal  _Goal_ as top-level. The goal is converted from
an atom to a Prolog term.
</li>
 <li>-b  _BOOT_FILE_
boot code is in Prolog file  _BOOT_FILE_. The filename must define
the predicate `'$live'/0`.
</li>
 <li>-c <tt>IP_HOST</tt> <tt>port</tt>
connect standard streams to host <tt>IP_HOST</tt> at port <tt>port</tt>
</li>
 <li>filename
restore state saved in the given file
</li>
 <li>-f
do not consult initial files
</li>
 <li>-q
do not print informational messages
</li>
 <li>--
separator for arguments to Prolog code. These arguments are visible
through the [unix/1](@ref unix) built-in predicate.
</li>
</ul>

Note that YAP will output an error message on the following conditions:

<ul>
 <li>
a file name was given but the file does not exist or is not a saved
YAP state;
</li>
 <li>
the necessary amount of memory could not be allocated;
</li>
 <li>
the allocated memory is not enough to restore the state.
</li>
</ul>

When restoring a saved state, YAP will allocate the
same amount of memory as that in use when the state was saved, unless a
different amount is specified by flags in the command line. By default,
YAP restores the file startup.yss from the current directory or from
the YAP library.

<ul>
 <li>
YAP usually boots from a saved state. The saved state will use the default
installation directory to search for the YAP binary unless you define
the environment variable YAPBINDIR.

</li>
 <li>
YAP always tries to find saved states from the current directory
first. If it cannot it will use the environment variable YAPLIBDIR, if
defined, or search the default library directory.

</li>
 <li>
YAP will try to find library files from the YAPSHAREDIR/library
directory.
</li>
</ul>

@section Running_Prolog_Files Running Prolog Files

YAP can also be used to run Prolog files as scripts, at least in
Unix-like environments. A simple example is shown next (do not forget
that the shell comments are very important):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#!/usr/local/bin/yap -L --
#
# Hello World script file using YAP
#
# put a dot because of syntax errors .

:- write('Hello World'), nl.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `#!`  characters specify that the script should call the binary
file YAP. Notice that many systems will require the complete path to the
YAP binary. The `-L` flag indicates that YAP should consult the
current file when booting and then halt. The remaining arguments are
then passed to YAP. Note that YAP will skip the first lines if they
start with `#` (the comment sign for Unix's shell). YAP will
consult the file and execute any commands.

A slightly more sophisticated example is:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#!/usr/bin/yap -L --
#
# Hello World script file using YAP
# .

:- initialization(main).

main :- write('Hello World'), nl.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `initialization` directive tells YAP to execute the goal main
after consulting the file. Source code is thus compiled and `main`
executed at the end. The `.` is useful while debugging the script
as a Prolog program: it guarantees that the syntax error will not
propagate to the Prolog code.

Notice that the `--` is required so that the shell passes the extra
arguments to YAP.  As an example, consider the following script
`dump_args`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#!/usr/bin/yap -L --
#.

main( [] ).
main( [H|T] ) :-
        write( H ), nl,
        main( T ).

:- unix( argv(AllArgs) ), main( AllArgs ).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you this run this script with the arguments:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
./dump_args -s 10000
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
the script will start an YAP process with stack size `10MB`, and
the list of arguments to the process will be empty.

Often one wants to run the script as any other program, and for this it
is convenient to ignore arguments to YAP. This is possible by using
`L --` as in the next version of `dump_args`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#!/usr/bin/yap -L --

main( [] ).
main( [H|T] ) :-
        write( H ), nl,
        main( T ).

:- unix( argv(AllArgs) ), main( AllArgs ).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `--` indicates the next arguments are not for YAP. Instead,
they must be sent directly to the [argv](@ref argv) built-in. Hence, running

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
./dump_args test
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
will write `test` on the standard output.

@page Syntax Syntax

We will describe the syntax of YAP at two levels. We first will
describe the syntax for Prolog terms. In a second level we describe
the \a tokens from which Prolog \a terms are
built.

@section Formal_Syntax Syntax of Terms

Below, we describe the syntax of YAP terms from the different
classes of tokens defined above. The formalism used will be <em>BNF</em>,
extended where necessary with attributes denoting integer precedence or
operator type.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 term       ---->     subterm(1200)   end_of_term_marker

 subterm(N) ---->     term(M)         [M <= N]

 term(N)    ---->     op(N, fx) subterm(N-1)
             |        op(N, fy) subterm(N)
             |        subterm(N-1) op(N, xfx) subterm(N-1)
             |        subterm(N-1) op(N, xfy) subterm(N)
             |        subterm(N) op(N, yfx) subterm(N-1)
             |        subterm(N-1) op(N, xf)
             |        subterm(N) op(N, yf)

 term(0)   ---->      atom '(' arguments ')'
             |        '(' subterm(1200)  ')'
             |        '{' subterm(1200)  '}'
             |        list
             |        string
             |        number
             |        atom
             |        variable

 arguments ---->      subterm(999)
             |        subterm(999) ',' arguments

 list      ---->      '[]'
             |        '[' list_expr ']'

 list_expr ---->      subterm(999)
             |        subterm(999) list_tail

 list_tail ---->      ',' list_expr
             |        ',..' subterm(999)
             |        '|' subterm(999)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Notes:

<ul>

 <li>
\a op(N,T) denotes an atom which has been previously declared with type
\a T and base precedence \a N.

</li>
 <li>
Since ',' is itself a pre-declared operator with type \a xfy and
precedence 1000, is \a subterm starts with a '(', \a op must be
followed by a space to avoid ambiguity with the case of a functor
followed by arguments, e.g.:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
+ (a,b)        [the same as '+'(','(a,b)) of arity one]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
versus

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
+(a,b)         [the same as '+'(a,b) of arity two]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>
In the first rule for term(0) no blank space should exist between
\a atom and '('.

</li>
 <li>
Each term to be read by the YAP parser must end with a single
dot, followed by a blank (in the sense mentioned in the previous
paragraph). When a name consisting of a single dot could be taken for
the end of term marker, the ambiguity should be avoided by surrounding the
dot with single quotes.

</li>
</ul>

@section Tokens Prolog Tokens

Prolog tokens are grouped into the following categories:

@subsection Numbers Numbers

Numbers can be further subdivided into integer and floating-point numbers.

@subsubsection Integers

Integer numbers
are described by the following regular expression:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
<integer> := {<digit>+<single-quote>|0{xXo}}<alpha_numeric_char>+
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where {...} stands for optionality, \a + optional repetition (one or
more times), \a \\\<digit\\\> denotes one of the characters 0 ... 9, \a |
denotes or, and \a \\\<single-quote\\\> denotes the character "'". The digits
before the \a \\\<single-quote\\\> character, when present, form the number
basis, that can go from 0, 1 and up to 36. Letters from `A` to
`Z` are used when the basis is larger than 10.

Note that if no basis is specified then base 10 is assumed. Note also
that the last digit of an integer token can not be immediately followed
by one of the characters 'e', 'E', or '.'.

Following the ISO standard, YAP also accepts directives of the
form `0x` to represent numbers in hexadecimal base and of the form
`0o` to represent numbers in octal base. For usefulness,
YAP also accepts directives of the form `0X` to represent
numbers in hexadecimal base.

Example:
the following tokens all denote the same integer

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
10  2'1010  3'101  8'12  16'a  36'a  0xa  0o12
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Numbers of the form `0'a` are used to represent character
constants. So, the following tokens denote the same integer:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
0'd  100
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

YAP (version 6.3.4) supports integers that can fit
the word size of the machine. This is 32 bits in most current machines,
but 64 in some others, such as the Alpha running Linux or Digital
Unix. The scanner will read larger or smaller integers erroneously.

@subsubsection Floats

Floating-point numbers are described by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   <float> := <digit>+{<dot><digit>+}
               <exponent-marker>{<sign>}<digit>+
            |<digit>+<dot><digit>+
               {<exponent-marker>{<sign>}<digit>+}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where \a \\\<dot\\\> denotes the decimal-point character '.',
\a \\\<exponent-marker\\\> denotes one of 'e' or 'E', and \a \\\<sign\\\> denotes
one of '+' or '-'.

Examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
10.0   10e3   10e-3   3.1415e+3
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Floating-point numbers are represented as a double in the target
machine. This is usually a 64-bit number.

@subsection Strings Character Strings

Strings are described by the following rules:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  string --> '"' string_quoted_characters '"'

  string_quoted_characters --> '"' '"' string_quoted_characters
  string_quoted_characters --> '\'
                          escape_sequence string_quoted_characters
  string_quoted_characters -->
                          string_character string_quoted_characters

  escape_sequence --> 'a' | 'b' | 'r' | 'f' | 't' | 'n' | 'v'
  escape_sequence --> '\' | '"' | ''' | '`'
  escape_sequence --> at_most_3_octal_digit_seq_char '\'
  escape_sequence --> 'x' at_most_2_hexa_digit_seq_char '\'
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where `string_character` in any character except the double quote
and escape characters.

Examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
""   "a string"   "a double-quote:""" 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first string is an empty string, the last string shows the use of
double-quoting. The implementation of YAP represents strings as
lists of integers. Since YAP 4.3.0 there is no static limit on string
size.

Escape sequences can be used to include the non-printable characters
`a` (alert), `b` (backspace), `r` (carriage return),
`f` (form feed), `t` (horizontal tabulation), `n` (new
line), and `v` (vertical tabulation). Escape sequences also be
include the meta-characters `\\`, `"`, `'`, and
```. Last, one can use escape sequences to include the characters
either as an octal or hexadecimal number.

The next examples demonstrates the use of escape sequences in YAP:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"\x0c\" "\01\" "\f" "\\" 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first three examples return a list including only character 12 (form
feed). The last example escapes the escape character.

Escape sequences were not available in C-Prolog and in original
versions of YAP up to 4.2.0. Escape sequences can be disable by using:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- yap_flag(character_escapes,false).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection Atoms Atoms

Atoms are defined by one of the following rules:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   atom --> solo-character
   atom --> lower-case-letter name-character*
   atom --> symbol-character+
   atom --> single-quote  single-quote
   atom --> ''' atom_quoted_characters '''

  atom_quoted_characters --> ''' ''' atom_quoted_characters
  atom_quoted_characters --> '\' atom_sequence string_quoted_characters
  atom_quoted_characters --> character string_quoted_characters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   <solo-character>     denotes one of:    ! ;
   <symbol-character>   denotes one of:    # & * + - . / : < 
                                           = > ? @ \ ^ ~ `
   <lower-case-letter>  denotes one of:    a...z
   <name-character>     denotes one of:    _ a...z A...Z 0....9
   <single-quote>       denotes:           '
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and `string_character` denotes any character except the double quote
and escape characters. Note that escape sequences in strings and atoms
follow the same rules.

Examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a   a12x   '$a'   !   =>  '1 2'
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Version `4.2.0` of YAP removed the previous limit of 256
characters on an atom. Size of an atom is now only limited by the space
available in the system.

@subsection Variables Variables

Variables are described by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   <variable-starter><variable-character>+
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  <variable-starter>   denotes one of:    _ A...Z
  <variable-character> denotes one of:    _ a...z A...Z
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If a variable is referred only once in a term, it needs not to be named
and one can use the character `_` to represent the variable. These
variables are known as anonymous variables. Note that different
occurrences of `_` on the same term represent <em>different</em>
anonymous variables. 

@subsection Punctuation_Tokens Punctuation Tokens

Punctuation tokens consist of one of the following characters:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
( ) , [ ] { } |
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These characters are used to group terms.

@subsection Layout Layout
Any characters with ASCII code less than or equal to 32 appearing before
a token are ignored.

All the text appearing in a line after the character \a % is taken to
be a comment and ignored (including \a %).  Comments can also be
inserted by using the sequence `/\*` to start the comment and
`\*` followed by `/` to finish it. In the presence of any sequence of comments or
layout characters, the YAP parser behaves as if it had found a
single blank character. The end of a file also counts as a blank
character for this purpose.

@section Encoding Wide Character Support

YAP now implements a SWI-Prolog compatible interface to wide
characters and the Universal Character Set (UCS). The following text
was adapted from the SWI-Prolog manual.

YAP now  supports wide characters, characters with character
codes above 255 that cannot be represented in a single byte.
<em>Universal Character Set</em> (UCS) is the ISO/IEC 10646 standard
that specifies a unique 31-bits unsigned integer for any character in
any language.  It is a superset of 16-bit Unicode, which in turn is
a superset of ISO 8859-1 (ISO Latin-1), a superset of US-ASCII.  UCS
can handle strings holding characters from multiple languages and
character classification (uppercase, lowercase, digit, etc.) and
operations such as case-conversion are unambiguously defined.

For this reason YAP, following SWI-Prolog, has two representations for
atoms. If the text fits in ISO Latin-1, it is represented as an array
of 8-bit characters.  Otherwise the text is represented as an array of
wide chars, which may take 16 or 32 bits.  This representational issue
is completely transparent to the Prolog user.  Users of the foreign
language interface sometimes need to be aware of these issues though.

Character coding comes into view when characters of strings need to be
read from or written to file or when they have to be communicated to
other software components using the foreign language interface. In this
section we only deal with I/O through streams, which includes file I/O
as well as I/O through network sockets.

@subsection Stream_Encoding Wide character encodings on streams

Although characters are uniquely coded using the UCS standard
internally, streams and files are byte (8-bit) oriented and there are a
variety of ways to represent the larger UCS codes in an 8-bit octet
stream. The most popular one, especially in the context of the web, is
UTF-8. Bytes 0...127 represent simply the corresponding US-ASCII
character, while bytes 128...255 are used for multi-byte
encoding of characters placed higher in the UCS space. Especially on
MS-Windows the 16-bit Unicode standard, represented by pairs of bytes is
also popular.

Prolog I/O streams have a property called <em>encoding</em> which
specifies the used encoding that influence `get_code/2` and
`put_code/2` as well as all the other text I/O predicates.

The default encoding for files is derived from the Prolog flag
`encoding`, which is initialised from the environment.  If the
environment variable `LANG` ends in "UTF-8", this encoding is
assumed. Otherwise the default is `text` and the translation is
left to the wide-character functions of the C-library (note that the
Prolog native UTF-8 mode is considerably faster than the generic
`mbrtowc()` one).  The encoding can be specified explicitly in
[load_files/2](@ref load_files) for loading Prolog source with an alternative
encoding, `open/4` when opening files or using `set_stream/2` on
any open stream (not yet implemented). For Prolog source files we also
provide the `encoding/1` directive that can be used to switch
between encodings that are compatible to US-ASCII (`ascii`,
`iso_latin_1`, `utf8` and many locales).  



For
additional information and Unicode resources, please visit
<http://www.unicode.org/>.

YAP currently defines and supports the following encodings:

<ul>
 <li>octet
Default encoding for <em>binary</em> streams.  This causes
the stream to be read and written fully untranslated.

</li>
 <li>ascii
7-bit encoding in 8-bit bytes.  Equivalent to `iso_latin_1`,
but generates errors and warnings on encountering values above
127.

</li>
 <li>iso_latin_1
8-bit encoding supporting many western languages.  This causes
the stream to be read and written fully untranslated.

</li>
 <li>text
C-library default locale encoding for text files.  Files are read and
written using the C-library functions `mbrtowc()` and
`wcrtomb()`.  This may be the same as one of the other locales,
notably it may be the same as `iso_latin_1` for western
languages and `utf8` in a UTF-8 context.

</li>
 <li>utf8
Multi-byte encoding of full UCS, compatible to `ascii`.
See above.

</li>
 <li>unicode_be
Unicode Big Endian.  Reads input in pairs of bytes, most
significant byte first.  Can only represent 16-bit characters.

</li>
 <li>unicode_le
Unicode Little Endian.  Reads input in pairs of bytes, least
significant byte first.  Can only represent 16-bit characters.
</li>
</ul>

Note that not all encodings can represent all characters. This implies
that writing text to a stream may cause errors because the stream
cannot represent these characters. The behaviour of a stream on these
errors can be controlled using `open/4` or `set_stream/2` (not
implemented). Initially the terminal stream write the characters using
Prolog escape sequences while other streams generate an I/O exception.

@subsection BOM BOM: Byte Order Mark

From [Stream Encoding](@ref Stream_Encoding), you may have got the impression that
text-files are complicated. This section deals with a related topic,
making live often easier for the user, but providing another worry to
the programmer.   *BOM* or <em>Byte Order Marker</em> is a technique
for identifying Unicode text-files as well as the encoding they
use. Such files start with the Unicode character `0xFEFF`, a
non-breaking, zero-width space character. This is a pretty unique
sequence that is not likely to be the start of a non-Unicode file and
uniquely distinguishes the various Unicode file formats. As it is a
zero-width blank, it even doesn't produce any output. This solves all
problems, or ...

Some formats start of as US-ASCII and may contain some encoding mark to
switch to UTF-8, such as the `encoding="UTF-8"` in an XML header.
Such formats often explicitly forbid the the use of a UTF-8 BOM. In
other cases there is additional information telling the encoding making
the use of a BOM redundant or even illegal.

The BOM is handled by the `open/4` predicate. By default, text-files are
probed for the BOM when opened for reading. If a BOM is found, the
encoding is set accordingly and the property `bom(true)` is
available through [stream_property/2](@ref stream_property). When opening a file for
writing, writing a BOM can be requested using the option
`bom(true)` with `open/4`.

@page Loading_Programs Loading and Manipulating Programs

Next, we present the main predicates and directives available to load
files and to control the Prolog environment.

@section Compiling Program loading and updating

<ul>

 <li>consult(+ _F_) @anchor consult


Adds the clauses written in file  _F_ or in the list of files  _F_
to the program.

In YAP [consult/1](@ref consult) does not remove previous clauses for
the procedures defined in  _F_. Moreover, note that all code in YAP
is compiled.

</li>
 <li>reconsult(+ _F_) @anchor reconsult


Updates the program replacing the
previous definitions for the predicates defined in  _F_.

</li>
 <li>[+ _F_] @anchor nil

[]/1
The same as `consult(F)`.

</li>
 <li>[-+ _F_] @anchor dash_nil

[-]/1
The same as `reconsult(F)`

Example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- [file1, -file2, -file3, file4].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
will consult `file1` `file4` and reconsult `file2` and
`file3`.

</li>
 <li>compile(+ _F_) @anchor compile


In YAP, the same as [reconsult/1](@ref reconsult).

</li>
 <li>load_files(+ _Files_, + _Options_) @anchor load_files


General implementation of `consult`. Execution is controlled by the
following flags:

<ul>
 <li>autoload(+ _Autoload_)
SWI-compatible option where if  _Autoload_ is `true` predicates
are loaded on first call. Currently
not supported.
</li>
 <li>derived_from(+ _File_)
SWI-compatible option to control make. Currently
not supported.
</li>
 <li>encoding(+ _Encoding_)
Character encoding used in consulting files. Please  (see [Encoding](@ref Encoding)) for
supported encodings.

</li>
 <li>expand(+ _Bool_)
Not yet implemented. In SWI-Prolog, if `true`, run the
filenames through [expand_file_name/2](@ref expand_file_name) and load the returned
files. Default is false, except for [consult/1](@ref consult) which is
intended for interactive use.

</li>
 <li>if(+ _Condition_)
Load the file only if the specified  _Condition_ is
satisfied. The value `true` the file unconditionally,
`changed` loads the file if it was not loaded before, or has
been modified since it was loaded the last time, `not_loaded`
loads the file if it was not loaded before.

</li>
 <li>imports(+ _ListOrAll_)
If `all` and the file is a module file, import all public
predicates. Otherwise import only the named predicates. Each
predicate is referred to as `\<name\>/\<arity\>`. This option has
no effect if the file is not a module file.

</li>
 <li>must_be_module(+ _Bool_)
If true, raise an error if the file is not a module file. Used by
`use_module/[1,2]`.

</li>
 <li>silent(+ _Bool_)
If true, load the file without printing a message. The specified value is the default for all files loaded as a result of loading the specified files.

</li>
 <li>stream(+ _Input_)
This SWI-Prolog extension compiles the data from the stream
 _Input_. If this option is used,  _Files_ must be a single
atom which is used to identify the source-location of the loaded
clauses as well as remove all clauses if the data is re-consulted.

This option is added to allow compiling from non-file locations such as databases, the web, the user (see consult/1) or other servers. 

</li>
 <li>compilation_mode(+ _Mode_)
This extension controls how procedures are compiled. If  _Mode_
is `compact` clauses are compiled and no source code is stored;
if it is `source` clauses are compiled and source code is stored;
if it is `assert_all` clauses are asserted into the data-base.

</li>
 <li>comnsult(+ _Mode_)
This extension controls the type of file to load. If  _Mode_
is `consult`, clauses are added to the data-base,
is `reconsult`, clauses are recompiled,
is `db`, these are facts that need to be added to the data-base,
is `exo`, these are facts with atoms and integers that need a very compact representation.
</li>
</ul>

</li>
 <li>ensure_loaded(+ _F_) [ISO] @anchor ensure_loaded


When the files specified by  _F_ are module files,
[ensure_loaded/1](@ref ensure_loaded) loads them if they have note been previously
loaded, otherwise advertises the user about the existing name clashes
and prompts about importing or not those predicates. Predicates which
are not public remain invisible.

When the files are not module files, [ensure_loaded/1](@ref ensure_loaded) loads them
if they have not been loaded before, does nothing otherwise.

 _F_ must be a list containing the names of the files to load.

</li>
 <li>load_db(+ _Files_) @anchor load_db


Load a database of facts with equal structure.

</li>
 <li>exo_files(+ _Files_) @anchor exo_files


Load compactly a database of facts with equal structure. Useful when wanting to
read in a very compact way database tables.

</li>
 <li>make @anchor make


SWI-Prolog built-in to consult all source files that have been
changed since they were consulted. It checks all loaded source
files. make/0 can be combined with the compiler to speed up the
development of large packages. In this case compile the package
using

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    sun% pl -g make -o my_program -c file ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If `my_program' is started it will first reconsult all source files
that have changed since the compilation. 

</li>
 <li>include(+ _F_) [ISO] @anchor include


The `include` directive includes the text files or sequence of text
files specified by  _F_ into the file being currently consulted.

</li>
</ul>

@section Setting_the_Compiler Looking for Files 

@ref abs_file_name


@section Changing_the_CompilerbAs_Behavior Changing the Compiler's Behavior

This section presents a set of built-ins predicates designed to set the 
environment for the compiler.

<ul>

 <li>source_mode(- _O_,+ _N_) @anchor source_mode


The state of source mode can either be on or off. When the source mode
is on, all clauses are kept both as compiled code and in a "hidden"
database.  _O_ is unified with the previous state and the mode is set
according to  _N_.

</li>
 <li>source @anchor source


After executing this goal, YAP keeps information on the source
of the predicates that will be consulted. This enables the use of
[listing/0](@ref listing), `listing/1` and [clause/2](@ref clause) for those
clauses.

The same as `source_mode(_,on)` or as declaring all newly defined
static procedures as `public`.

</li>
 <li>no_source @anchor no_source


The opposite to `source`.

The same as `source_mode(_,off)`.

</li>
 <li>compile_expressions @anchor compile_expressions


After a call to this predicate, arithmetical expressions will be compiled.
(see example below). This is the default behavior.

</li>
 <li>do_not_compile_expressions @anchor do_not_compile_expressions


After a call to this predicate, arithmetical expressions will not be compiled.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- source, do_not_compile_expressions.
yes
?- [user].
| p(X) :- X is 2 * (3 + 8).
| :- end_of_file.
?- compile_expressions.
yes
?- [user].
| q(X) :- X is 2 * (3 + 8).
| :- end_of_file.
:- listing.

p(A):-
      A is 2 * (3 + 8).

q(A):-
      A is 22.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>hide(+ _Atom_) @anchor hide


Make atom  _Atom_ invisible.

</li>
 <li>unhide(+ _Atom_) @anchor unhide


Make hidden atom  _Atom_ visible.

</li>
 <li>hide_predicate(+ _Pred_) @anchor hide_predicate


Make predicate  _Pred_ invisible to `current_predicate/2`,
`listing`, and friends.

</li>
 <li>stash_predicate(+ _Pred_) @anchor stash_predicate


Make predicate  _Pred_ invisible to new code, and to `current_predicate/2`,
`listing`, and friends. New predicates with the same name and
functor can be declared.

</li>
 <li>expand_exprs(- _O_,+ _N_) @anchor expand_exprs


Puts YAP in state  _N_ (`on` or `off`) and unify
 _O_ with the previous state, where  _On_ is equivalent to
`compile_expressions` and `off` is equivalent to
`do_not_compile_expressions`. This predicate was kept to maintain
compatibility with C-Prolog.

</li>
 <li>style_check(+ _X_) @anchor style_check


Turns on style checking according to the attribute specified by  _X_,
which must be one of the following:

<ul>
 <li>single_var
Checks single occurrences of named variables in a clause.
</li>
 <li>discontiguous
Checks non-contiguous clauses for the same predicate in a file.
</li>
 <li>multiple
Checks the presence of clauses for the same predicate in more than one
file when the predicate has not been declared as `multifile`
</li>
 <li>all
Performs style checking for all the cases mentioned above.
</li>
</ul>
By default, style checking is disabled in YAP unless we are in
`sicstus` or `iso` language mode.

The [style_check/1](@ref style_check) built-in is now deprecated. Please use the
`set_prolog_flag/1` instead.

</li>
 <li>no_style_check(+ _X_) @anchor no_style_check


Turns off style checking according to the attribute specified by
 _X_, which has the same meaning as in [style_check/1](@ref style_check).

The [no_style_check/1](@ref no_style_check) built-in is now deprecated. Please use the
`set_prolog_flag/1` instead.

</li>
 <li>multifile  _P_ [ISO] @anchor multifile


Instructs the compiler about the declaration of a predicate  _P_ in
more than one file. It must appear in the first of the loaded files
where the predicate is declared, and before declaration of any of its
clauses.

Multifile declarations affect [reconsult/1](@ref reconsult) and [compile/1](@ref compile):
when a multifile predicate is reconsulted, only the clauses from the
same file are removed.

Since YAP4.3.0 multifile procedures can be static or dynamic.

</li>
 <li>discontiguous(+ _G_) [ISO] @anchor discontiguous



Declare that the arguments are discontiguous procedures, that is,
clauses for discontigous procedures may be separated by clauses from
other procedures.

</li>
 <li>initialization(+ _G_) [ISO] @anchor initialization


The compiler will execute goals  _G_ after consulting the current
file.

</li>
 <li>initialization(+ _Goal_,+ _When_)

Similar to [initialization/1](@ref initialization), but allows for specifying when
 _Goal_ is executed while loading the program-text:

<ul>
 <li>now
Execute  _Goal_ immediately. 
</li>
 <li>after_load
Execute  _Goal_ after loading program-text. This is the same as initialization/1. 
</li>
 <li>restore
Do not execute  _Goal_ while loading the program, but only when
restoring a state (not implemented yet). 
</li>
</ul>

</li>
 <li>prolog_to_os_filename(+ _PrologPath_,- _OsPath_) @anchor prolog_to_os_filename



This is an SWI-Prolog built-in. Converts between the internal Prolog
pathname conventions and the operating-system pathname conventions. The
internal conventions are Unix and this predicates is equivalent to =/2
(unify) on Unix systems. On DOS systems it will change the
directory-separator, limit the filename length map dots, except for the
last one, onto underscores.

</li>
 <li>expand_file_name(+ _WildCard_,- _List_) @anchor expand_file_name



This is an SWI-Prolog built-in. Unify  _List_ with a sorted list of
files or directories matching  _WildCard_. The normal Unix wildcard
constructs <tt>?</tt>, <tt>\\\*</tt>, <tt>[ ... ]</tt> and <tt>{...}</tt> are recognised. The
interpretation of <tt>{...}</tt> is interpreted slightly different from the
C shell (csh(1)). The comma separated argument can be arbitrary
patterns, including <tt>{...}</tt> patterns. The empty pattern is legal as
well: <tt>{.pl,}</tt> matches either <tt>.pl</tt> or the empty string.

If the pattern contains wildcard characters, only existing files and
directories are returned. Expanding a <em>pattern'</em> without wildcard
characters returns the argument, regardless on whether or not it exists.

Before expanding wildcards, the construct $var is expanded to the value
of the environment variable var and a possible leading ~ character is
expanded to the user's home directory. In Windows, the home directory is
determined as follows: if the environment variable `HOME` exists,
this is used. If the variables `HOMEDRIVE` and `HOMEPATH`
exist (Windows-NT), these are used. At initialisation, the system will
set the environment variable `HOME` to point to the YAP home
directory if neither `HOME` nor `HOMEPATH` and
`HOMEDRIVE` are defined.

</li>
 <li>public  _P_ [ISO extension] @anchor public


Instructs the compiler that the source of a predicate of a list of
predicates  _P_ must be kept. This source is then accessible through
the [clause/2](@ref clause) procedure and through the `listing` family of
built-ins.

Note that all dynamic procedures are public. The `source` directive
defines all new or redefined predicates to be public.

Since YAP4.3.0 multifile procedures can be static or dynamic.

</li>
</ul>

@section Conditional_Compilation Conditional Compilation 

Conditional compilation builds on the same principle as
[term_expansion/2](@ref term_expansion), `goal_expansion/2` and the expansion of
grammar rules to compile sections of the source-code
conditionally. One of the reasons for introducing conditional
compilation is to simplify writing portable code.



Note that these directives can only be appear as separate terms in the
input.  Typical usage scenarios include:

<ul>
 <li>Load different libraries on different dialects
</li>
 <li>Define a predicate if it is missing as a system predicate
</li>
 <li>Realise totally different implementations for a particular
part of the code due to different capabilities.
</li>
 <li>Realise different configuration options for your software.
</li>
</ul>

<ul>
 <li>if(+ _Goal_) @anchor if


Compile subsequent code only if  _Goal_ succeeds.  For enhanced
portability,  _Goal_ is processed by `expand_goal/2` before execution.
If an error occurs, the error is printed and processing proceeds as if
 _Goal_ has failed.

</li>
 <li>else @anchor else


Start `else' branch.

</li>
 <li>endif @anchor endif


End of conditional compilation.

</li>
 <li>elif(+ _Goal_) @anchor elif


Equivalent to `:- else. :-if(Goal) ... :- endif.`  In a sequence
as below, the section below the first matching elif is processed, If
no test succeeds the else branch is processed.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- if(test1).
section_1.
:- elif(test2).
section_2.
:- elif(test3).
section_3.
:- else.
section_else.
:- endif.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
</ul>

@section Saving Saving and Loading Prolog States

<ul>
 <li>save(+ _F_) @anchor save


Saves an image of the current state of YAP in file  _F_. From
 *YAP4.1.3* onwards, YAP saved states are executable
files in the Unix ports.

</li>
 <li>save(+ _F_,- _OUT_)

Saves an image of the current state of YAP in file  _F_. From
 *YAP4.1.3* onwards, YAP saved states are executable
files in the Unix ports.

Unify  _OUT_ with 1 when saving the file and  _OUT_ with 0 when
restoring the saved state.

</li>
 <li>save_program(+ _F_) @anchor save_program


Saves an image of the current state of the YAP database in file
 _F_.

</li>
 <li>save_program(+ _F_, : _G_)

Saves an image of the current state of the YAP database in file
 _F_, and guarantee that execution of the restored code will start by
trying goal  _G_.

</li>
 <li>qsave_program(+ _F_, + _ListOfOpts_) @anchor qsave_program



Saves the current state of the program to the file  _File_. The
result is a resource archive containing a saved state that expresses
all Prolog data from the running program and all user-defined
resources. Depending on the stand_alone option, the resource is headed
by the emulator, a Unix shell script or nothing. Options is a list of
additional options:

<ul>
 <li>stack(+ _KBytes_)
Limit for the local and global stack.
</li>
 <li>trail(+ _KBytes_)
Limit for the trail stack.
</li>
 <li>goal(: _Callable_)
Initialization goal for the new executable (see -g).


</li>
 <li>init_file(+ _Atom_)
Default initialization file for the new executable. See -f.














</li>
</ul>

</li>
 <li>restore(+ _F_) @anchor restore


Restores a previously saved state of YAP from file  _F_.

YAP always tries to find saved states from the current directory
first. If it cannot it will use the environment variable [YAPLIBDIR](@ref YAPLIBDIR), if
defined, or search the default library directory.
</li>
</ul>

@section Modules The Module System

Module systems are quite important for the development of large
applications. YAP implements a module system compatible with the Quintus
Prolog module system.

The YAP module system is predicate-based. This means a module consists
of a set of predicates (or procedures), such that some predicates are
public and the others are local to a module. Atoms and terms in general
are global to the system. Moreover, the module system is flat, meaning
that we do not support a hierarchy of modules. Modules can
automatically import other modules, though. For compatibility with other
module systems the YAP module system is non-strict, meaning both that
there is a way to access predicates private to a module and that it
is possible to declare predicates for a module from some other module.

YAP allows one to ignore the module system if one does not want to use
it. Last note that using the module system does not introduce any
significant overheads.

@subsection Module_Concepts Module Concepts

The YAP module system applies to predicates. All predicates belong to a
module. System predicates belong to the module `primitives`, and by
default new predicates belong to the module `user`. Predicates from
the module `primitives` are automatically visible to every module.

Every predicate must belong to a module. This module is called its
<em>source module</em>.

By default, the source module for a clause occurring in a source file
with a module declaration is the declared module. For goals typed in 
a source file without module declarations, their module is the module
the file is being loaded into. If no module declarations exist, this is
the current <em>type-in module</em>. The default type-in module is
`user`, but one can set the current module by using the built-in
`module/1`.

Note that in this module system one can explicitly specify the source
mode for a clause by prefixing a clause with its module, say:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
user:(a :- b).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In fact, to specify the source module for a clause it is sufficient to
specify the source mode for the clause's head:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
user:a :- b.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The rules for goals are similar. If a goal appears in a text file with a
module declaration, the goal's source module is the declared
module. Otherwise, it is the module the file is being loaded into or the
type-in module.

One can override this rule by prefixing a goal with the module it is
supposed to be executed in, say:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nasa:launch(apollo,13).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
will execute the goal `launch(apollo,13)` as if the current source
module was `nasa`.

Note that this rule breaks encapsulation and should be used with care.

@subsection Defining_Modules Defining a New Module

A new module is defined by a `module` declaration:

<ul>

 <li>module(+ _M_,+ _L_) @anchor module


This directive defines the file where it appears as a module file; it
must be the first declaration in the file.
 _M_ must be an atom specifying the module name;  _L_ must be a list
containing the module's public predicates specification, in the form
`[predicate_name/arity,...]`.

The public predicates of a module file can be made accessible by other
files through the directives [use_module/1](@ref use_module), `use_module/2`,
[ensure_loaded/1](@ref ensure_loaded) and the predicates [consult/1](@ref consult) or
[reconsult/1](@ref reconsult). The non-public predicates
of a module file are not visible by other files; they can, however, be
accessed by prefixing the module name with the
`:/2` operator.

</li>
</ul>

The built-in `module/1` sets the current source module:

<ul>

 <li>module(+ _M_,+ _L_, + _Options_)

Similar to [module/2](@ref module), this directive defines the file where it
appears in as a module file; it must be the first declaration in the file.
 _M_ must be an atom specifying the module name;  _L_ must be a
list containing the module's public predicates specification, in the
form `[predicate_name/arity,...]`.

The last argument  _Options_ must be a list of options, which can be:

<ul>
 <li>filename
the filename for a module to import into the current module.

</li>
 <li>library(file)
a library file to import into the current module.

</li>
 <li>hide( _Opt_)
if  _Opt_ is `false`, keep source code for current module, if
`true`, disable.
</li>
</ul>

</li>
 <li>module(+ _M_)

Defines  _M_ to be the current working or type-in module. All files
which are not bound to a module are assumed to belong to the working
module (also referred to as type-in module). To compile a non-module
file into a module which is not the working one, prefix the file name
with the module name, in the form ` _Module_: _File_`, when
loading the file.

</li>
 <li>export(+ _PredicateIndicator_) @anchor export



Add predicates to the public list of the context module. This implies
the predicate will be imported into another module if this module is
imported with `use_module/[1,2]`. Note that predicates are normally
exported using the directive [module/2](@ref module). [export/1](@ref export) is meant
to handle export from dynamically created modules. The directive argument
may also be a list of predicates.

</li>
 <li>export_list(? _Mod_,? _ListOfPredicateIndicator_) @anchor export_list



The list  _ListOfPredicateIndicator_ contains all predicates exported
by module  _Mod_.

</li>
</ul>

@subsection Using_Modules Using Modules

By default, all procedures to consult a file will load the modules
defined therein. The two following declarations allow one to import a
module explicitly. They differ on whether one imports all predicate
declared in the module or not.

<ul>

 <li>use_module(+ _F_) @anchor use_module


Loads the files specified by  _F_, importing all their public
predicates. Predicate name clashes are resolved by asking the user about
importing or not the predicate. A warning is displayed when  _F_ is
not a module file.

</li>
 <li>use_module(+ _F_,+ _L_)

Loads the files specified by  _F_, importing the predicates specified
in the list  _L_. Predicate name clashes are resolved by asking the
user about importing or not the predicate. A warning is displayed when
 _F_ is not a module file.

</li>
 <li>use_module(? _M_,? _F_,+ _L_)

If module  _M_ has been defined, import the procedures in  _L_ to
the current module. Otherwise, load the files specified by  _F_,
importing the predicates specified in the list  _L_. 
</li>
</ul>

@subsection MetahYPredicates_in_Modules Meta-Predicates and Modules

The module system must know whether predicates operate on goals or
clauses. Otherwise, such predicates would call a goal in the module they
were defined, instead of calling it in the module they are currently
executing. So, for instance, consider a file example.pl:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(example,[a/1]).

a(G) :- call(G)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We import this module with `use_module(example)` into module
`user`.  The expected behavior for a goal `a(p)` is to
execute goal `p` within the module `user`. However,
`a/1` will call `p` within module `example`.

The [meta_predicate/1](@ref meta_predicate) declaration informs the system that some
arguments of a predicate are goals, clauses, clauses heads or other
terms related to a module, and that these arguments must be prefixed
with the current source module:

<ul>

 <li>meta_predicate  _G1_,...., _Gn_ @anchor meta_predicate


Each  _Gi_ is a mode specification.

If the argument is `:`, it does not refer directly to a predicate
but must be module expanded. If the argument is an integer, the argument
is a goal or a closure and must be expanded. Otherwise, the argument is 
not expanded. Note that the system already includes declarations for all 
built-ins.

For example, the declaration for [call/1](@ref call) and [setof/3](@ref setof) are:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- meta_predicate call(0), setof(?,0,?).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
</ul>

The previous example is expanded to the following code which explains,
why the goal `a(p)` calls `p` in `example` and not in
`user`.  The goal `call(G)` is expanded because of the
meta-predicate declaration for [call/1](@ref call).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(example,[a/1]).

a(G) :- call(example:G)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By adding a meta-predicate declaration for `a/1`, the goal
`a(p)` in module user will be expanded to `a(user:p)`
thereby preserving the module information.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(example,[a/1]).

:- meta_predicate a(:).
a(G) :- call(G)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An alternate mechanism is the directive [module_transparent/1](@ref module_transparent)
offered for compatibility with SWI-Prolog.

<ul>

 <li>module_transparent + _Preds_ @anchor module_transparent


 _Preds_ is a comma separated sequence of name/arity predicate
indicators (like
[dynamic/1](@ref dynamic)). Each goal associated with a transparent declared
predicate will inherit the context module from its parent goal.
</li>
</ul>

@subsection RehYExporting_Modules Re-Exporting Predicates From Other Modules

It is sometimes convenient to re-export predicates originally defined in
a different module. This is often useful if you are adding to the
functionality of a module, or if you are composing a large module with
several small modules. The following declarations can be used for that purpose:

<ul>

 <li>reexport(+ _F_) @anchor reexport


Export all predicates defined in file  _F_ as if they were defined in
the current module.

</li>
 <li>reexport(+ _F_,+ _Decls_)

Export predicates defined in file  _F_ according to  _Decls_. The
declarations may be of the form:

<ul>
 <li>A list of predicate declarations to be exported. Each declaration
may be a predicate indicator or of the form `` _PI_ `as`
 _NewName_'', meaning that the predicate with indicator  _PI_ is
to be exported under name  _NewName_.
</li>
 <li>`except`( _List_) 
In this case, all predicates not in  _List_ are exported. Moreover,
if ` _PI_ `as`  _NewName_` is found, the predicate with
indicator  _PI_ is to be exported under name  _NewName_ as
before.
</li>
</ul>
</li>
</ul>

Re-exporting predicates must be used with some care. Please, take into
account the following observations:

<ul>
 <li>
The `reexport` declarations must be the first declarations to
follow the  `module` declaration.
</li>
 <li>
It is possible to use both `reexport` and `use_module`, but
all predicates reexported are automatically available for use in the
current module.
</li>
 <li>
In order to obtain efficient execution, YAP compiles dependencies
between re-exported predicates. In practice, this means that changing a
`reexport` declaration and then  *just* recompiling the file
may result in incorrect execution.
</li>
</ul>

@page BuilthYins Built-In Predicates Library

@section Control Control Predicates

This chapter describes the predicates for controlling the execution of
Prolog programs.

In the description of the arguments of functors the following notation
will be used:

<ul>
 <li>
a preceding plus sign will denote an argument as an "input argument" -
it cannot be a free variable at the time of the call; 
</li>
 <li>
a preceding minus sign will denote an "output argument";
</li>
 <li>
an argument with no preceding symbol can be used in both ways.
</li>
</ul>

<ul>

 <li>+ _P_, + _Q_ [ISO] @anchor cO


Conjunction of goals (and).

Example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 p(X) :- q(X), r(X).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

should be read as "p( _X_) if q( _X_) and r( _X_)".

</li>
 <li>+ _P_ ; + _Q_ [ISO] @anchor mM


Disjunction of goals (or).

Example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 p(X) :- q(X); r(X).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
should be read as "p( _X_) if q( _X_) or r( _X_)".

</li>
 <li>true [ISO] @anchor true


Succeeds once.

</li>
 <li>fail [ISO] @anchor fail


Always fails.

</li>
 <li>false [ISO] @anchor false


The same as fail.

</li>
 <li>! [ISO] @anchor eX


Read as "cut". Cuts any choices taken in the current procedure.
When first found "cut" succeeds as a goal, but if backtracking should
later return to it, the parent goal (the one which matches the head of
the clause containing the "cut", causing the clause activation) will
fail. This is an extra-logical predicate and cannot be explained in
terms of the declarative semantics of Prolog.

example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 member(X,[X|_]).
 member(X,[_|L]) :- member(X,L).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With the above definition

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ?- member(X,[1,2,3]).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

will return each element of the list by backtracking. With the following
definition:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 member(X,[X|_]) :- !.
 member(X,[_|L]) :- member(X,L).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

the same query would return only the first element of the 
list, since backtracking could not "pass through" the cut.

</li>
 <li>\\+ + _P_ [ISO] @anchor bQ


Goal  _P_ is not provable. The execution of this predicate fails if
and only if the goal  _P_ finitely succeeds. It is not a true logical
negation, which is impossible in standard Prolog, but
"negation-by-failure".

This predicate might be defined as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 \+(P) :- P, !, fail.
 \+(_).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if  _P_ did not include "cuts".

</li>
 <li>not + _P_ @anchor not


Goal  _P_ is not provable. The same as `'\\+  _P_'`.

This predicate is kept for compatibility with C-Prolog and previous
versions of YAP. Uses of [not/1](@ref not) should be replace by
`(\\+)/1`, as YAP does not implement true negation.

</li>
 <li>+ _P_ -\> + _Q_ [ISO] @anchor hYgG


Read as "if-then-else" or "commit". This operator is similar to the
conditional operator of imperative languages and can be used alone or
with an else part as follows:

<ul>
 <li>+P -\> +Q
"if P then Q".
</li>
 <li>+P -\> +Q; +R
"if P then Q else R".
</li>
</ul>

These two predicates could be defined respectively in Prolog as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 (P -> Q) :- P, !, Q.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 (P -> Q; R) :- P, !, Q.
 (P -> Q; R) :- R.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if there were no "cuts" in  _P_,  _Q_ and  _R_.

Note that the commit operator works by "cutting" any alternative
solutions of  _P_.

Note also that you can use chains of commit operators like:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    P -> Q ; R -> S ; T.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that `(-\>)/2` does not affect the scope of cuts in its
arguments.

</li>
 <li>+ _Condition_ \*-\> + _Action_ ; + _Else_ @anchor sThYgG


This construct implements the so-called <em>soft-cut</em>. The control is
defined as follows: If  _Condition_ succeeds at least once, the
semantics is the same as ( _Condition_,  _Action_). If
 _Condition_ does not succeed, the semantics is that of (\\+
 _Condition_,  _Else_). In other words, If  _Condition_
succeeds at least once, simply behave as the conjunction of
 _Condition_ and  _Action_, otherwise execute  _Else_.

The construct  _A \*-\> B_, i.e. without an  _Else_ branch, is
translated as the normal conjunction  _A_,  _B_.

</li>
 <li>repeat [ISO] @anchor repeat


Succeeds repeatedly.

In the next example, `repeat` is used as an efficient way to implement
a loop. The next example reads all terms in a file:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 a :- repeat, read(X), write(X), nl, X=end_of_file, !.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
the loop is effectively terminated by the cut-goal, when the test-goal
`X=end` succeeds. While the test fails, the goals `read(X)`,
`write(X)`, and `nl` are executed repeatedly, because
backtracking is caught by the `repeat` goal.

The built-in `repeat/1` could be defined in Prolog by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 repeat.
 repeat :- repeat.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>call(+ _P_) [ISO] @anchor call


If  _P_ is instantiated to an atom or a compound term, the goal
`call( _P_)` is executed as if the value of `P` was found
instead of the call to [call/1](@ref call), except that any "cut" occurring in
 _P_ only cuts alternatives in the execution of  _P_.

</li>
 <li>incore(+ _P_) @anchor incore


The same as [call/1](@ref call).

</li>
 <li>call(+ _Closure_,...,? _Ai_,...) [ISO] @anchor calln


Meta-call where  _Closure_ is a closure that is converted into a goal by 
appending the  _Ai_ additional arguments. The number of arguments varies 
between 0 and 10.

</li>
 <li>call_with_args(+ _Name_,...,? _Ai_,...) @anchor call_with_argsn


Meta-call where  _Name_ is the name of the procedure to be called and
the  _Ai_ are the arguments. The number of arguments varies between 0
and 10. New code should use `call/N` for better portability.

If  _Name_ is a complex term, then [call_with_args/n](@ref call_with_argsn) behaves as
[call/n](@ref calln):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
call(p(X1,...,Xm), Y1,...,Yn) :- p(X1,...,Xm,Y1,...,Yn).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>+ _P_ @anchor var_call

The same as `call( _P_)`. This feature has been kept to provide
compatibility with C-Prolog. When compiling a goal, YAP
generates a `call( _X_)` whenever a variable  _X_ is found as
a goal.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 a(X) :- X.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is converted to:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 a(X) :- call(X).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>if(? _G_,? _H_,? _I_)

Call goal  _H_ once per each solution of goal  _H_. If goal
 _H_ has no solutions, call goal  _I_.

The built-in `if/3` is similar to `-\>/3`, with the difference
that it will backtrack over the test goal. Consider the following
small data-base:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a(1).        b(a).          c(x).
a(2).        b(b).          c(y).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Execution of an `if/3` query will proceed as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- if(a(X),b(Y),c(Z)).

X = 1,
Y = a ? ;

X = 1,
Y = b ? ;

X = 2,
Y = a ? ;

X = 2,
Y = b ? ;

no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The system will backtrack over the two solutions for `a/1` and the
two solutions for `b/1`, generating four solutions.

Cuts are allowed inside the first goal  _G_, but they will only prune
over  _G_.

If you want  _G_ to be deterministic you should use if-then-else, as
it is both more efficient and more portable.

</li>
 <li>once(: _G_) [ISO] @anchor once


Execute the goal  _G_ only once. The predicate is defined by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 once(G) :- call(G), !.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that cuts inside [once/1](@ref once) can only cut the other goals inside
[once/1](@ref once).

</li>
 <li>forall(: _Cond_,: _Action_) @anchor forall


For all alternative bindings of  _Cond_  _Action_ can be
proven. The example verifies that all arithmetic statements in the list
 _L_ are correct. It does not say which is wrong if one proves wrong.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- forall(member(Result = Formula, [2 = 1 + 1, 4 = 2 * 2]),
                 Result =:= Formula).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>ignore(: _Goal_) @anchor ignore


Calls  _Goal_ as [once/1](@ref once), but succeeds, regardless of whether
`Goal` succeeded or not. Defined as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ignore(Goal) :-
        Goal, !.
ignore(_).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>abort @anchor abort


Abandons the execution of the current goal and returns to top level. All
break levels (see [break/0](@ref break) below) are terminated. It is mainly
used during debugging or after a serious execution error, to return to
the top-level.

</li>
 <li>break @anchor break


Suspends the execution of the current goal and creates a new execution
level similar to the top level, displaying the following message:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 [ Break (level <number>) ]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
telling the depth of the break level just entered. To return to the
previous level just type the end-of-file character or call the
end_of_file predicate.  This predicate is especially useful during
debugging.

</li>
 <li>halt [ISO] @anchor halt


Halts Prolog, and exits to the calling application. In YAP,
[halt/0](@ref halt) returns the exit code `0`.

</li>
 <li>halt(+  _I_) [ISO]

Halts Prolog, and exits to the calling application returning the code
given by the integer  _I_.

</li>
 <li>catch(+ _Goal_,+ _Exception_,+ _Action_) [ISO] @anchor catch


The goal `catch( _Goal_, _Exception_, _Action_)` tries to
execute goal  _Goal_. If during its execution,  _Goal_ throws an
exception  _E'_ and this exception unifies with  _Exception_, the
exception is considered to be caught and  _Action_ is executed. If
the exception  _E'_ does not unify with  _Exception_, control
again throws the exception.

The top-level of YAP maintains a default exception handler that
is responsible to capture uncaught exceptions.

</li>
 <li>throw(+ _Ball_) [ISO] @anchor throw


The goal `throw( _Ball_)` throws an exception. Execution is
stopped, and the exception is sent to the ancestor goals until reaching
a matching [catch/3](@ref catch), or until reaching top-level.

</li>
 <li>garbage_collect @anchor garbage_collect


The goal `garbage_collect` forces a garbage collection.

</li>
 <li>garbage_collect_atoms @anchor garbage_collect_atoms


The goal `garbage_collect` forces a garbage collection of the atoms
in the data-base. Currently, only atoms are recovered.

</li>
 <li>gc @anchor gc


The goal `gc` enables garbage collection. The same as
`yap_flag(gc,on)`.

</li>
 <li>nogc @anchor nogc


The goal `nogc` disables garbage collection. The same as
`yap_flag(gc,off)`.

</li>
 <li>grow_heap(+ _Size_) @anchor grow_heap


Increase heap size  _Size_ kilobytes.

</li>
 <li>grow_stack(+ _Size_) @anchor grow_stack


Increase stack size  _Size_ kilobytes.

</li>
</ul>

@section Undefined_Procedures Handling Undefined Procedures

A predicate in a module is said to be undefined if there are no clauses
defining the predicate, and if the predicate has not been declared to be
dynamic. What YAP does when trying to execute undefined predicates can
be specified in three different ways:

<ul>
 <li>By setting an YAP flag, through the [yap_flag/2](@ref yap_flag) or
[set_prolog_flag/2](@ref set_prolog_flag) built-ins. This solution generalizes the
ISO standard.
</li>
 <li>By using the [unknown/2](@ref unknown) built-in (this solution is
compatible with previous releases of YAP).
</li>
 <li>By defining clauses for the hook predicate
`user:unknown_predicate_handler/3`. This solution is compatible
with SICStus Prolog.
</li>
</ul>

In more detail:

<ul>
 <li>unknown(- _O_,+ _N_) @anchor unknown


Specifies an handler to be called is a program tries to call an
undefined static procedure  _P_.

The arity of  _N_ may be zero or one. If the arity is `0`, the
new action must be one of `fail`, `warning`, or
`error`. If the arity is `1`,  _P_ is an user-defined
handler and at run-time, the argument to the handler  _P_ will be
unified with the undefined goal. Note that  _N_ must be defined prior
to calling [unknown/2](@ref unknown), and that the single argument to  _N_ must
be unbound.

In YAP, the default action is to `fail` (note that in the ISO
Prolog standard the default action is `error`).

After defining `undefined/1` by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
undefined(A) :- format('Undefined predicate: ~w~n',[A]), fail.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and executing the goal:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unknown(U,undefined(X)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a call to a predicate for which no clauses were defined will result in
the output of a message of the form:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Undefined predicate: user:xyz(A1,A2)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
followed by the failure of that call.

</li>
 <li>yap_flag(unknown,+ _SPEC_) @anchor yap_flag_unknown

Alternatively, one can use [yap_flag/2](@ref yap_flag),
[current_prolog_flag/2](@ref current_prolog_flag), or [set_prolog_flag/2](@ref set_prolog_flag), to set this
functionality. In this case, the first argument for the built-ins should
be `unknown`, and the second argument should be either
`error`, `warning`, `fail`, or a goal.

</li>
 <li>user:unknown_predicate_handler(+G,+M,?NG) @anchor unknown_predicate_handler


The user may also define clauses for
`user:unknown_predicate_handler/3` hook predicate. This
user-defined procedure is called before any system processing for the
undefined procedure, with the first argument  _G_ set to the current
goal, and the second  _M_ set to the current module. The predicate
 _G_ will be called from within the user module.

If `user:unknown_predicate_handler/3` succeeds, the system will
execute  _NG_. If  `user:unknown_predicate_handler/3` fails, the
system will execute default action as specified by [unknown/2](@ref unknown).

</li>
 <li>exception(+ _Exception_, + _Context_, - _Action_) @anchor exception


Dynamic predicate, normally not defined. Called by the Prolog system on run-time exceptions that can be repaired `just-in-time'. The values for  _Exception_ are described below. See also [catch/3](@ref catch) and [throw/1](@ref throw).
If this hook predicate succeeds it must instantiate the  _Action_ argument to the atom `fail` to make the operation fail silently, `retry` to tell Prolog to retry the operation or `error` to make the system generate an exception. The action `retry` only makes sense if this hook modified the environment such that the operation can now succeed without error.

<ul>
 <li>undefined_predicate
 _Context_ is instantiated to a predicate-indicator ( _Module:Name/Arity_). If the predicate fails Prolog will generate an existence_error exception. The hook is intended to implement alternatives to the SWI built-in autoloader, such as autoloading code from a database. Do not use this hook to suppress existence errors on predicates. See also `unknown`.
</li>
 <li>undefined_global_variable
 _Context_ is instantiated to the name of the missing global variable. The hook must call [nb_setval/2](@ref nb_setval) or [b_setval/2](@ref b_setval) before returning with the action retry.
</li>
</ul>

</li>
</ul>

@section Messages Message Handling

The interaction between YAP and the user relies on YAP's ability to
portray messages. These messages range from prompts to error
information. All message processing is performed through the builtin
[print_message/2](@ref print_message), in two steps:

<ul>
 <li>The message is processed into a list of commands 
</li>
 <li>The commands in the list are sent to the `format/3` builtin
in sequence.
</li>
</ul>

The first argument to [print_message/2](@ref print_message) specifies the importance of
the message. The options are:

<ul>
 <li>error
error handling
</li>
 <li>warning
compilation and run-time warnings,
</li>
 <li>informational
generic informational messages
</li>
 <li>help 
help messages (not currently implemented in YAP)
</li>
 <li>query
query 	used in query processing (not currently implemented in YAP)
</li>
 <li>silent
messages that do not produce output but that can be intercepted by hooks.
</li>
</ul>

The next table shows the main predicates and hooks associated to message
handling in YAP:

<ul>
 <li>print_message(+ _Kind_,  _Term_) @anchor print_message


The predicate print_message/2 is used to print messages, notably from
exceptions in a human-readable format.  _Kind_ is one of
`informational`, `banner`, `warning`, `error`,
`help` or `silent`. A human-readable message is printed to
the stream [user_error](@ref user_error).

If the Prolog flag [verbose](@ref verbose) is `silent`, messages with
 _Kind_ `informational`, or `banner` are treated as
silent.@c  See \\cmdlineoption{-q}.

This predicate first translates the  _Term_ into a list of `message
lines' (see [print_message_lines/3](@ref print_message_lines) for details).  Next it will
call the hook [message_hook/3](@ref message_hook) to allow the user intercepting the
message.  If [message_hook/3](@ref message_hook) fails it will print the message unless
 _Kind_ is silent.

If you need to report errors from your own predicates, we advise you to
stick to the existing error terms if you can; but should you need to
invent new ones, you can define corresponding error messages by
asserting clauses for `prolog:message/2`. You will need to declare
the predicate as multifile.

</li>
 <li>print_message_lines(+ _Stream_, + _Prefix_, + _Lines_) @anchor print_message_lines


Print a message (see [print_message/2](@ref print_message)) that has been translated to
a list of message elements.  The elements of this list are:

<ul>
 <li>`\<Format\>`-`\<Args\>`
Where  _Format_ is an atom and  _Args_ is a list
of format argument.  Handed to `format/3`.
</li>
 <li>`flush`
If this appears as the last element,  _Stream_ is flushed
(see `flush_output/1`) and no final newline is generated.
</li>
 <li>`at_same_line`
If this appears as first element, no prefix is printed for
the first line and the line-position is not forced to 0
(see `format/1`, `~N`).
</li>
 <li>`\<Format\>`
Handed to `format/3` as `format(Stream, Format, [])`.
</li>
 <li>nl
A new line is started and if the message is not complete
the  _Prefix_ is printed too.
</li>
</ul>

</li>
 <li>user:message_hook(+ _Term_, + _Kind_, + _Lines_) @anchor message_hook


Hook predicate that may be define in the module `user` to intercept
messages from [print_message/2](@ref print_message).  _Term_ and  _Kind_ are the
same as passed to [print_message/2](@ref print_message).  _Lines_ is a list of
format statements as described with [print_message_lines/3](@ref print_message_lines).

This predicate should be defined dynamic and multifile to allow other
modules defining clauses for it too.

</li>
 <li>message_to_string(+ _Term_, - _String_) @anchor message_to_string


Translates a message-term into a string object. Primarily intended for SWI-Prolog emulation.
</li>
</ul>

@section Testing_Terms Predicates on terms

<ul>

 <li>var( _T_) [ISO] @anchor var


Succeeds if  _T_ is currently a free variable, otherwise fails. 

</li>
 <li>atom( _T_) [ISO] @anchor atom


Succeeds if and only if  _T_ is currently instantiated to an  atom.

</li>
 <li>atomic(T) [ISO] @anchor atomic


Checks whether  _T_ is an atomic symbol (atom or number).

</li>
 <li>compound( _T_) [ISO] @anchor compound


Checks whether  _T_ is a compound term.

</li>
 <li>db_reference( _T_) @anchor db_reference1C


Checks whether  _T_ is a database reference.

</li>
 <li>float( _T_) [ISO] @anchor float


Checks whether  _T_ is a floating point number.

</li>
 <li>rational( _T_) @anchor rational


Checks whether `T` is a rational number.

</li>
 <li>integer( _T_) [ISO] @anchor integer


Succeeds if and only if  _T_ is currently instantiated to an  integer.

</li>
 <li>nonvar( _T_) [ISO] @anchor nonvar


The opposite of `var( _T_)`.

</li>
 <li>number( _T_) [ISO] @anchor number


Checks whether `T` is an integer, rational or a float.

</li>
 <li>primitive( _T_) @anchor primitive


Checks whether  _T_ is an atomic term or a database reference.

</li>
 <li>simple( _T_) @anchor simple


Checks whether  _T_ is unbound, an atom, or a number.

</li>
 <li>callable( _T_) [ISO] @anchor callable


Checks whether  _T_ is a callable term, that is, an atom or a
compound term.

</li>
 <li>numbervars( _T_,+ _N1_,- _Nn_) @anchor numbervars


Instantiates each variable in term  _T_ to a term of the form:
`'$VAR'( _I_)`, with  _I_ increasing from  _N1_ to  _Nn_.

</li>
 <li>unnumbervars( _T_,+ _NT_) @anchor unnumbervars


Replace every `'$VAR'( _I_)` by a free variable.

</li>
 <li>ground( _T_) [ISO] @anchor ground


Succeeds if there are no free variables in the term  _T_.

</li>
 <li>acyclic_term( _T_) [ISO] @anchor acyclic_term


Succeeds if there are loops in the term  _T_, that is, it is an infinite term.

</li>
 <li>arg(+ _N_,+ _T_, _A_) [ISO] @anchor arg


Succeeds if the argument  _N_ of the term  _T_ unifies with
 _A_. The arguments are numbered from 1 to the arity of the term.

The current version will generate an error if  _T_ or  _N_ are
unbound, if  _T_ is not a compound term, of if  _N_ is not a positive
integer. Note that previous versions of YAP would fail silently
under these errors.

</li>
 <li>functor( _T_, _F_, _N_) [ISO] @anchor functor


The top functor of term  _T_ is named  _F_ and has  arity  _N_.

When  _T_ is not instantiated,  _F_ and  _N_ must be. If
 _N_ is 0,  _F_ must be an atomic symbol, which will be unified
with  _T_. If  _N_ is not 0, then  _F_ must be an atom and
 _T_ becomes instantiated to the most general term having functor
 _F_ and arity  _N_. If  _T_ is instantiated to a term then
 _F_ and  _N_ are respectively unified with its top functor name
and arity.

In the current version of YAP the arity  _N_ must be an
integer. Previous versions allowed evaluable expressions, as long as the
expression would evaluate to an integer. This feature is not available
in the ISO Prolog standard.

</li>
 <li>_T_ =..  _L_ [ISO] @anchor qQdOdO


The list  _L_ is built with the functor and arguments of the term
 _T_. If  _T_ is instantiated to a variable, then  _L_ must be
instantiated either to a list whose head is an atom, or to a list
consisting of just a number.

</li>
 <li>_X_ =  _Y_ [ISO] @anchor qQ


Tries to unify terms  _X_ and  _Y_.

</li>
 <li>_X_ \\=  _Y_ [ISO] @anchor bQqQ


Succeeds if terms  _X_ and  _Y_ are not unifiable.

</li>
 <li>unify_with_occurs_check(?T1,?T2) [ISO] @anchor unify_with_occurs_check


Obtain the most general unifier of terms  _T1_ and  _T2_, if there
is one.

This predicate implements the full unification algorithm. An example:n

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unify_with_occurs_check(a(X,b,Z),a(X,A,f(B)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
will succeed with the bindings `A = b` and `Z = f(B)`. On the
other hand:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unify_with_occurs_check(a(X,b,Z),a(X,A,f(Z)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
would fail, because `Z` is not unifiable with `f(Z)`. Note that
`(=)/2` would succeed for the previous examples, giving the following
bindings `A = b` and `Z = f(Z)`.

</li>
 <li>copy_term(? _TI_,- _TF_) [ISO] @anchor copy_term


Term  _TF_ is a variant of the original term  _TI_, such that for
each variable  _V_ in the term  _TI_ there is a new variable  _V'_
in term  _TF_. Notice that:

<ul>
 <li>suspended goals and attributes for attributed variables in
 _TI_ are also duplicated;
</li>
 <li>ground terms are shared between the new and the old term.
</li>
</ul>

If you do not want any sharing to occur please use
[duplicate_term/2](@ref duplicate_term).

</li>
 <li>duplicate_term(? _TI_,- _TF_) @anchor duplicate_term


Term  _TF_ is a variant of the original term  _TI_, such that
for each variable  _V_ in the term  _TI_ there is a new variable
 _V'_ in term  _TF_, and the two terms do not share any
structure. All suspended goals and attributes for attributed variables
in  _TI_ are also duplicated.

Also refer to [copy_term/2](@ref copy_term).

</li>
 <li>is_list(+ _List_) @anchor is_list


True when  _List_ is a proper list. That is,  _List_
is bound to the empty list (nil) or a term with functor '.' and arity 2.

</li>
 <li>? _Term1_ =@= ? _Term2_ @anchor qQaAqQ



Same as [variant/2](@ref variant), succeeds if  _Term1_ and  _Term2_ are variant terms.

</li>
 <li>subsumes_term(? _Subsumer_, ? _Subsumed_) @anchor subsumes_term



Succeed if  _Submuser_ subsumes  _Subsuned_ but does not bind any
variable in  _Subsumer_.

</li>
 <li>term_subsumer(? _T1_, ? _T2_, ? _Subsumer_) @anchor term_subsumer



Succeed if  _Subsumer_ unifies with the least general
generalization over  _T1_ and
 _T2_.

</li>
 <li>term_variables(? _Term_, - _Variables_) [ISO] @anchor term_variables



Unify  _Variables_ with the list of all variables of term
 _Term_.  The variables occur in the order of their first
appearance when traversing the term depth-first, left-to-right.

</li>
 <li>rational_term_to_tree(? _TI_,- _TF_) @anchor rational_term_to_tree


The term  _TF_ is a tree representation (without cycles) for the
Prolog term  _TI_. Loops are replaced by terms of the form
`_LOOP_( _LevelsAbove_)` where  _LevelsAbove_ is the size of
the loop.

</li>
 <li>tree_to_rational_term(? _TI_,- _TF_) @anchor tree_to_rational_term


Inverse of above. The term  _TI_ is a tree representation (without
cycles) for the Prolog term  _TF_. Loops replace terms of the form
`_LOOP_( _LevelsAbove_)` where  _LevelsAbove_ is the size of
the loop.

</li>
</ul>

@section Predicates_on_Atoms Predicates on Atoms

The following predicates are used to manipulate atoms:

<ul>
 <li>name( _A_, _L_) @anchor name


The predicate holds when at least one of the arguments is ground
(otherwise, an error message will be displayed). The argument  _A_ will
be unified with an atomic symbol and  _L_ with the list of the ASCII
codes for the characters of the external representation of  _A_.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 name(yap,L).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
will return:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 L = [121,97,112].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 name(3,L).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
will return:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 L = [51].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>atom_chars(? _A_,? _L_) [ISO] @anchor atom_chars


The predicate holds when at least one of the arguments is ground
(otherwise, an error message will be displayed). The argument  _A_ must
be unifiable with an atom, and the argument  _L_ with the list of the
characters of  _A_.

</li>
 <li>atom_codes(? _A_,? _L_) [ISO] @anchor atom_codes


The predicate holds when at least one of the arguments is ground
(otherwise, an error message will be displayed). The argument  _A_ will
be unified with an atom and  _L_ with the list of the ASCII
codes for the characters of the external representation of  _A_.

</li>
 <li>atom_concat(+ _As_,? _A_) @anchor atom_concat


The predicate holds when the first argument is a list of atoms, and the
second unifies with the atom obtained by concatenating all the atoms in
the first list.

</li>
 <li>atomic_concat(+ _As_,? _A_) @anchor atomic_concat


The predicate holds when the first argument is a list of atomic terms, and
the second unifies with the atom obtained by concatenating all the
atomic terms in the first list. The first argument thus may contain
atoms or numbers.

</li>
 <li>atomic_list_concat(+ _As_,? _A_) @anchor atomic_list_concat


The predicate holds when the first argument is a list of atomic terms, and
the second unifies with the atom obtained by concatenating all the
atomic terms in the first list. The first argument thus may contain
atoms or numbers.

</li>
 <li>atomic_list_concat(? _As_,+ _Separator_,? _A_)

Creates an atom just like [atomic_list_concat/2](@ref atomic_list_concat), but inserts
 _Separator_ between each pair of atoms. For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- atomic_list_concat([gnu, gnat], ', ', A).

A = 'gnu, gnat'
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

YAP emulates the SWI-Prolog version of this predicate that can also be
used to split atoms by instantiating  _Separator_ and  _Atom_ as
shown below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- atomic_list_concat(L, -, 'gnu-gnat').

L = [gnu, gnat]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>atom_length(+ _A_,? _I_) [ISO] @anchor atom_length


The predicate holds when the first argument is an atom, and the second
unifies with the number of characters forming that atom.

</li>
 <li>atom_concat(? _A1_,? _A2_,? _A12_) [ISO]

The predicate holds when the third argument unifies with an atom, and
the first and second unify with atoms such that their representations
concatenated are the representation for  _A12_.

If  _A1_ and  _A2_ are unbound, the built-in will find all the atoms
that concatenated give  _A12_.

</li>
 <li>number_chars(? _I_,? _L_) [ISO] @anchor number_chars



The predicate holds when at least one of the arguments is ground
(otherwise, an error message will be displayed). The argument  _I_ must
be unifiable with a number, and the argument  _L_ with the list of the
characters of the external representation of  _I_.

</li>
 <li>number_codes(? _A_,? _L_) [ISO] @anchor number_codes


The predicate holds when at least one of the arguments is ground
(otherwise, an error message will be displayed). The argument  _A_
will be unified with a number and  _L_ with the list of the ASCII
codes for the characters of the external representation of  _A_.

</li>
 <li>atom_number(? _Atom_,? _Number_) @anchor atom_number


The predicate holds when at least one of the arguments is ground
(otherwise, an error message will be displayed). If the argument
 _Atom_ is an atom,  _Number_ must be the number corresponding
to the characters in  _Atom_, otherwise the characters in
 _Atom_ must encode a number  _Number_.

</li>
 <li>number_atom(? _I_,? _L_) @anchor number_atom



The predicate holds when at least one of the arguments is ground
(otherwise, an error message will be displayed). The argument  _I_ must
be unifiable with a number, and the argument  _L_ must be unifiable
with an atom representing the number.

</li>
 <li>sub_atom(+ _A_,? _Bef_, ? _Size_, ? _After_, ? _At_out_) [ISO] @anchor sub_atom


True when  _A_ and  _At_out_ are atoms such that the name of
 _At_out_ has size  _Size_ and is a sub-string of the name of
 _A_, such that  _Bef_ is the number of characters before and
 _After_ the number of characters afterwards.

Note that  _A_ must always be known, but  _At_out_ can be unbound when
calling this built-in. If all the arguments for [sub_atom/5](@ref sub_atom) but  _A_
are unbound, the built-in will backtrack through all possible
sub-strings of  _A_.

</li>
</ul>

@section Predicates_on_Characters Predicates on Characters

The following predicates are used to manipulate characters:

<ul>
 <li>char_code(? _A_,? _I_) [ISO] @anchor char_code


The built-in succeeds with  _A_ bound to character represented as an
atom, and  _I_ bound to the character code represented as an
integer. At least, one of either  _A_ or  _I_ must be bound before
the call.

</li>
 <li>char_type(? _Char_, ? _Type_) @anchor char_type


Tests or generates alternative  _Types_ or  _Chars_. The
character-types are inspired by the standard `C`
`\<ctype.h\>` primitives.

<ul>
 <li>alnum
 _Char_ is a letter (upper- or lowercase) or digit.

</li>
 <li>alpha
 _Char_ is a letter (upper- or lowercase).

</li>
 <li>csym
 _Char_ is a letter (upper- or lowercase), digit or the underscore (_). These are valid C- and Prolog symbol characters.

</li>
 <li>csymf
 _Char_ is a letter (upper- or lowercase) or the underscore (_). These are valid first characters for C- and Prolog symbols

</li>
 <li>ascii
 _Char_ is a 7-bits ASCII character (0..127).

</li>
 <li>white
 _Char_ is a space or tab. E.i. white space inside a line.

</li>
 <li>cntrl
 _Char_ is an ASCII control-character (0..31).

</li>
 <li>digit
 _Char_ is a digit.

</li>
 <li>digit( _Weight_)
 _Char_ is a digit with value
 _Weight_. I.e. `char_type(X, digit(6))` yields `X =  '6'`. Useful for parsing numbers.

</li>
 <li>xdigit( _Weight_)
 _Char_ is a hexa-decimal digit with value  _Weight_. I.e. char_type(a, xdigit(X) yields X = '10'. Useful for parsing numbers.

</li>
 <li>graph
 _Char_ produces a visible mark on a page when printed. Note that the space is not included!

</li>
 <li>lower
 _Char_ is a lower-case letter.

</li>
 <li>lower(Upper)
 _Char_ is a lower-case version of  _Upper_. Only true if
 _Char_ is lowercase and  _Upper_ uppercase.

</li>
 <li>to_lower(Upper)
 _Char_ is a lower-case version of Upper. For non-letters, or letter without case,  _Char_ and Lower are the same. See also upcase_atom/2 and downcase_atom/2.

</li>
 <li>upper
 _Char_ is an upper-case letter.

</li>
 <li>upper(Lower)
 _Char_ is an upper-case version of Lower. Only true if  _Char_ is uppercase and Lower lowercase.

</li>
 <li>to_upper(Lower)
 _Char_ is an upper-case version of Lower. For non-letters, or letter without case,  _Char_ and Lower are the same. See also upcase_atom/2 and downcase_atom/2.

</li>
 <li>punct
 _Char_ is a punctuation character. This is a graph character that is not a letter or digit.

</li>
 <li>space
 _Char_ is some form of layout character (tab, vertical-tab, newline, etc.).

</li>
 <li>end_of_file
 _Char_ is -1.

</li>
 <li>end_of_line
 _Char_ ends a line (ASCII: 10..13).

</li>
 <li>newline
 _Char_ is a the newline character (10).

</li>
 <li>period
 _Char_ counts as the end of a sentence (.,!,?).

</li>
 <li>quote
 _Char_ is a quote-character (", ', `).

</li>
 <li>paren(Close)
 _Char_ is an open-parenthesis and Close is the corresponding close-parenthesis. 
</li>
</ul>

</li>
 <li>code_type(? _Code_, ? _Type_) @anchor code_type


As [char_type/2](@ref char_type), but uses character-codes rather than
one-character atoms. Please note that both predicates are as
flexible as possible. They handle either representation if the
argument is instantiated and only will instantiate with an integer
code or one-character atom depending of the version used. See also
the prolog-flag [double_quotes](@ref double_quotes) and the built-in predicates 
[atom_chars/2](@ref atom_chars) and [atom_codes/2](@ref atom_codes).

</li>
</ul>

@section Comparing_Terms Comparing Terms

The following predicates are used to compare and order terms, using the
standard ordering:

<ul>
 <li>
variables come before numbers, numbers come before atoms which in turn
come before compound terms, i.e.: variables @\< numbers @\< atoms @\<
compound terms.
</li>
 <li>
Variables are roughly ordered by "age" (the "oldest" variable is put
first);
</li>
 <li>
Floating point numbers are sorted in increasing order;
</li>
 <li>
Rational numbers are sorted in increasing order;
</li>
 <li>
Integers are sorted in increasing order;
</li>
 <li>
Atoms are sorted in lexicographic order;
</li>
 <li>
Compound terms are ordered first by arity of the main functor, then by
the name of the main functor, and finally by their arguments in
left-to-right order.
</li>
</ul>

<ul>

 <li>compare( _C_, _X_, _Y_) [ISO] @anchor compare


As a result of comparing  _X_ and  _Y_,  _C_ may take one of
the following values:

<ul>
 <li>
`=` if  _X_ and  _Y_ are identical;
</li>
 <li>
`\<` if  _X_ precedes  _Y_ in the defined order;
</li>
 <li>
`\>` if  _Y_ precedes  _X_ in the defined order;
</li>
</ul>

</li>
 <li>_X_ ==  _Y_ [ISO] @anchor qQqQ


Succeeds if terms  _X_ and  _Y_ are strictly identical. The
difference between this predicate and [=/2](@ref qQ) is that, if one of the
arguments is a free variable, it only succeeds when they have already
been unified.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- X == Y.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fails, but,

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- X = Y, X == Y.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
succeeds.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- X == 2.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fails, but,

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- X = 2, X == 2.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
succeeds.

</li>
 <li>_X_ \\==  _Y_ [ISO] @anchor bQqQqQ


Terms  _X_ and  _Y_ are not strictly identical.

</li>
 <li>_X_ @\<  _Y_ [ISO] @anchor aAaAsS


Term  _X_ precedes term  _Y_ in the standard order.

</li>
 <li>_X_ @=\<  _Y_ [ISO] @anchor aAaAqQsS


Term  _X_ does not follow term  _Y_ in the standard order.

</li>
 <li>_X_ @\>  _Y_ [ISO] @anchor aAaAgG


Term  _X_ follows term  _Y_ in the standard order.

</li>
 <li>_X_ @\>=  _Y_ [ISO] @anchor aAaAgGqQ


Term  _X_ does not precede term  _Y_ in the standard order.

</li>
 <li>sort(+ _L_,- _S_) [ISO] @anchor sort


Unifies  _S_ with the list obtained by sorting  _L_ and  merging
identical (in the sense of `==`) elements.

</li>
 <li>keysort(+ _L_, _S_) [ISO] @anchor keysort


Assuming L is a list of the form ` _Key_- _Value_`,
`keysort(+ _L_, _S_)` unifies  _S_ with the list obtained
from  _L_, by sorting its elements according to the value of
 _Key_.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- keysort([3-a,1-b,2-c,1-a,1-b],S).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
would return:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
S = [1-b,1-a,1-b,2-c,3-a]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>predsort(+ _Pred_, + _List_, - _Sorted_) @anchor predsort


Sorts similar to sort/2, but determines the order of two terms by
calling  _Pred_(- _Delta_, + _E1_, + _E2_) . This call must
unify  _Delta_ with one of `\<`, `\>` or `=`. If
built-in predicate compare/3 is used, the result is the same as
sort/2.

</li>
 <li>length(? _L_,? _S_) @anchor length


Unify the well-defined list  _L_ with its length. The procedure can
be used to find the length of a pre-defined list, or to build a list
of length  _S_.

</li>
</ul>

@section Arithmetic Arithmetic

@copydoc arithmetic 

  * See @ref arithmetic_preds for the predicates that implement arithment

  * See @ref arithmetic_cmps for the arithmetic comparisons supported in YAP

  * See @ref arithmetic_operators for how to call arithmetic operations in YAP 

@section InputOutput Input/Output Predicates

Some of the Input/Output predicates described below will in certain conditions
provide error messages and abort only if the file_errors flag is set.
If this flag is cleared the same predicates will just fail. Details on
setting and clearing this flag are given under 7.7.

@section Streams_and_Files Handling Streams and Files

<ul>

 <li>open(+ _F_,+ _M_,- _S_) [ISO] @anchor open


Opens the file with name  _F_ in mode  _M_ ('read', 'write' or
'append'), returning  _S_ unified with the stream name.

At most, there are 17 streams  opened at the same time. Each stream is
either an input or an output stream but not both. There are always 3
open streams:  [user_input](@ref user_input) for reading, [user_output](@ref user_output) for writing
and [user_error](@ref user_error) for writing. If there is no  ambiguity, the atoms
[user_input](@ref user_input) and [user_output](@ref user_output) may be referred to as  `user`.

The `file_errors` flag controls whether errors are reported when in
mode 'read' or 'append' the file  _F_ does not exist or is not
readable, and whether in mode 'write' or 'append' the file is not
writable.

</li>
 <li>open(+ _F_,+ _M_,- _S_,+ _Opts_) [ISO]

Opens the file with name  _F_ in mode  _M_ ('read',  'write' or
'append'), returning  _S_ unified with the stream name, and following
these options:

<ul>

 <li>type(+ _T_) [ISO]
Specify whether the stream is a `text` stream (default), or a
`binary` stream.

</li>
 <li>reposition(+ _Bool_) [ISO]
Specify whether it is possible to reposition the stream (`true`), or
not (`false`). By default, YAP enables repositioning for all
files, except terminal files and sockets.

</li>
 <li>eof_action(+ _Action_) [ISO]
Specify the action to take if attempting to input characters from a
stream where we have previously found an `end_of_file`. The possible
actions are `error`, that raises an error, `reset`, that tries to
reset the stream and is used for `tty` type files, and `eof_code`,
which generates a new `end_of_file` (default for non-tty files).

</li>
 <li>alias(+ _Name_) [ISO]
Specify an alias to the stream. The alias <tt>Name</tt> must be an atom. The
alias can be used instead of the stream descriptor for every operation
concerning the stream.

The operation will fail and give an error if the alias name is already
in use. YAP allows several aliases for the same file, but only
one is returned by [stream_property/2](@ref stream_property)

</li>
 <li>bom(+ _Bool_)
If present and `true`, a BOM (<em>Byte Order Mark</em>) was
detected while opening the file for reading or a BOM was written while
opening the stream. See [BOM](@ref BOM) for details.

</li>
 <li>encoding(+ _Encoding_)
Set the encoding used for text.  See [Encoding](@ref Encoding) for an overview of
wide character and encoding issues.

</li>
 <li>representation_errors(+ _Mode_)
Change the behaviour when writing characters to the stream that cannot
be represented by the encoding.  The behaviour is one of `error`
(throw and Input/Output error exception), `prolog` (write `\\u...\\`
escape code or `xml` (write `\&#...;` XML character entity).
The initial mode is `prolog` for the user streams and
`error` for all other streams. See also [Encoding](@ref Encoding).

</li>
 <li>expand_filename(+ _Mode_)
If  _Mode_ is `true` then do filename expansion, then ask Prolog
to do file name expansion before actually trying to opening the file:
this includes processing `~` characters and processing `$`
environment variables at the beginning of the file. Otherwise, just try
to open the file using the given name.

The default behavior is given by the Prolog flag
[open_expands_filename](@ref open_expands_filename).

</li>
</ul>

</li>
 <li>close(+ _S_) [ISO] @anchor close


Closes the stream  _S_. If  _S_ does not stand for a stream
currently opened an error is reported. The streams [user_input](@ref user_input),
[user_output](@ref user_output), and [user_error](@ref user_error) can never be closed.

</li>
 <li>close(+ _S_,+ _O_) [ISO]

Closes the stream  _S_, following options  _O_. 

The only valid options are `force(true)` and `force(false)`.
YAP currently ignores these options.

</li>
 <li>time_file(+ _File_,- _Time_) @anchor time_file


Unify the last modification time of  _File_ with
 _Time_.  _Time_ is a floating point number expressing the seconds
elapsed since Jan 1, 1970.

</li>
 <li>access_file(+ _F_,+ _M_) @anchor access_file

Is the file accessible?

</li>
 <li>file_base_name(+ _Name_,- _FileName_) @anchor file_base_name


Give the path a full path  _FullPath_ extract the  _FileName_.

</li>
 <li>file_name_extension(? _Base_,? _Extension_, ? _Name_) @anchor file_name_extension



This predicate is used to add, remove or test filename extensions. The
main reason for its introduction is to deal with different filename
properties in a portable manner. If the file system is
case-insensitive, testing for an extension will be done
case-insensitive too.  _Extension_ may be specified with or
without a leading dot (.). If an  _Extension_ is generated, it
will not have a leading dot.

</li>
 <li>current_stream( _F_, _M_, _S_) @anchor current_stream


Defines the relation: The stream  _S_ is opened on the file  _F_
in mode  _M_. It might be used to obtain all open streams (by
backtracking) or to access the stream for a file  _F_ in mode
 _M_, or to find properties for a stream  _S_. Notice that some
streams might not be associated to a file: in this case YAP tries to
return the file number. If that is not available, YAP unifies  _F_
with  _S_.

</li>
 <li>is_stream( _S_) @anchor is_stream


Succeeds if  _S_ is a currently open stream.

</li>
 <li>flush_output [ISO] @anchor flush_output


Send out all data in the output buffer of the current output stream.

</li>
 <li>flush_output(+ _S_) [ISO]

Send all data in the output buffer for stream  _S_.

</li>
 <li>set_input(+ _S_) [ISO] @anchor set_input


Set stream  _S_ as the current input stream. Predicates like [read/1](@ref read)
and [get/1](@ref get) will start using stream  _S_.

</li>
 <li>set_output(+ _S_) [ISO] @anchor set_output


Set stream  _S_ as the current output stream. Predicates like
[write/1](@ref write) and [put/1](@ref put) will start using stream  _S_.

</li>
 <li>stream_select(+ _STREAMS_,+ _TIMEOUT_,- _READSTREAMS_) @anchor stream_select


Given a list of open  _STREAMS_ opened in read mode and a  _TIMEOUT_
return a list of streams who are now available for reading. 

If the  _TIMEOUT_ is instantiated to `off`,
[stream_select/3](@ref stream_select) will wait indefinitely for a stream to become
open. Otherwise the timeout must be of the form `SECS:USECS` where
`SECS` is an integer gives the number of seconds to wait for a timeout
and `USECS` adds the number of micro-seconds.

This built-in is only defined if the system call `select` is
available in the system.

</li>
 <li>current_input(- _S_) [ISO] @anchor current_input


Unify  _S_ with the current input stream.

</li>
 <li>current_output(- _S_) [ISO] @anchor current_output


Unify  _S_ with the current output stream.

</li>
 <li>at_end_of_stream [ISO] @anchor at_end_of_stream


Succeed if the current stream has stream position end-of-stream or
past-end-of-stream.

</li>
 <li>at_end_of_stream(+ _S_) [ISO]

Succeed if the stream  _S_ has stream position end-of-stream or
past-end-of-stream. Note that  _S_ must be a readable stream.

</li>
 <li>set_stream_position(+ _S_, + _POS_) [ISO] @anchor set_stream_position


Given a stream position  _POS_ for a stream  _S_, set the current
stream position for  _S_ to be  _POS_.

</li>
 <li>stream_property(? _Stream_,? _Prop_) [ISO] @anchor stream_property



Obtain the properties for the open streams. If the first argument is
unbound, the procedure will backtrack through all open
streams. Otherwise, the first argument must be a stream term (you may
use `current_stream` to obtain a current stream given a file name).

The following properties are recognized:

<ul>

 <li>file_name( _P_)
An atom giving the file name for the current stream. The file names are
[user_input](@ref user_input), [user_output](@ref user_output), and [user_error](@ref user_error) for the
standard streams.

</li>
 <li>mode( _P_)
The mode used to open the file. It may be one of `append`,
`read`, or `write`.

</li>
 <li>input
The stream is readable.

</li>
 <li>output
The stream is writable.

</li>
 <li>alias( _A_)
ISO-Prolog primitive for stream aliases. <tt>YAP</tt> returns one of the
existing aliases for the stream.

</li>
 <li>position( _P_)
A term describing the position in the stream.

</li>
 <li>end_of_stream( _E_)
Whether the stream is `at` the end of stream, or it has found the
end of stream and is `past`, or whether it has `not` yet
reached the end of stream.

</li>
 <li>eof_action( _A_)
The action to take when trying to read after reaching the end of
stream. The action may be one of `error`, generate an error,
`eof_code`, return character code `-1`, or `reset` the
stream.

</li>
 <li>reposition( _B_)
Whether the stream can be repositioned or not, that is, whether it is
seekable.

</li>
 <li>type( _T_)
Whether the stream is a `text` stream or a `binary` stream.

</li>
 <li>bom(+ _Bool_)
If present and `true`, a BOM (<em>Byte Order Mark</em>) was
detected while opening the file for reading or a BOM was written while
opening the stream. See [BOM](@ref BOM) for details.

</li>
 <li>encoding(+ _Encoding_)
Query the encoding used for text.  See [Encoding](@ref Encoding) for an
overview of wide character and encoding issues in YAP.

</li>
 <li>representation_errors(+ _Mode_)
Behaviour when writing characters to the stream that cannot be
represented by the encoding.  The behaviour is one of `error`
(throw and Input/Output error exception), `prolog` (write `\\u...\\`
escape code or `xml` (write `\&#...;` XML character entity).
The initial mode is `prolog` for the user streams and
`error` for all other streams. See also [Encoding](@ref Encoding) and
`open/4`.

</li>
</ul>

</li>
 <li>current_line_number(- _LineNumber_) @anchor current_line_number


Unify  _LineNumber_ with the line number for the current stream.

</li>
 <li>current_line_number(+ _Stream_,- _LineNumber_)

Unify  _LineNumber_ with the line number for the  _Stream_. 

</li>
 <li>line_count(+ _Stream_,- _LineNumber_) @anchor line_count


Unify  _LineNumber_ with the line number for the  _Stream_.

</li>
 <li>character_count(+ _Stream_,- _CharacterCount_) @anchor character_count


Unify  _CharacterCount_ with the number of characters written to or
read to  _Stream_.

</li>
 <li>line_position(+ _Stream_,- _LinePosition_) @anchor line_position


Unify  _LinePosition_ with the position on current text stream
 _Stream_.

</li>
 <li>stream_position(+ _Stream_,- _StreamPosition_) @anchor stream_position


Unify  _StreamPosition_ with the packaged information of position on
current stream  _Stream_. Use [stream_position_data/3](@ref stream_position_data) to
retrieve information on character or line count.

</li>
 <li>stream_position_data(+ _Field_,+ _StreamPosition_,- _Info_) @anchor stream_position_data


Given the packaged stream position term  _StreamPosition_, unify
 _Info_ with  _Field_ `line_count`, `byte_count`, or
`char_count`.

</li>
</ul>

@section ChYProlog_File_Handling C-Prolog File Handling

<ul>

 <li>tell(+ _S_) @anchor tell


If  _S_ is a currently opened stream for output, it becomes the
current output stream. If  _S_ is an atom it is taken to be a
filename.  If there is no output stream currently associated with it,
then it is opened for output, and the new output stream created becomes
the current output stream. If it is not possible to open the file, an
error occurs.  If there is a single opened output stream currently
associated with the file, then it becomes the current output stream; if
there are more than one in that condition, one of them is chosen.

Whenever  _S_ is a stream not currently opened for output, an error
may be reported, depending on the state of the file_errors flag. The
predicate just fails, if  _S_ is neither a stream nor an atom.

</li>
 <li>telling(- _S_) @anchor telling


The current output stream is unified with  _S_.

</li>
 <li>told @anchor told


Closes the current output stream, and the user's terminal becomes again
the current output stream. It is important to remember to close streams
after having finished using them, as the maximum number of
simultaneously opened streams is 17.

</li>
 <li>see(+ _S_) @anchor see


If  _S_ is a currently opened input stream then it is assumed to be
the current input stream. If  _S_ is an atom it is taken as a
filename. If there is no input stream currently associated with it, then
it is opened for input, and the new input stream thus created becomes
the current input stream. If it is not possible to open the file, an
error occurs.  If there is a single opened input stream currently
associated with the file, it becomes the current input stream; if there
are more than one in that condition, then one of them is chosen.

When  _S_ is a stream not currently opened for input, an error may be
reported, depending on the state of the `file_errors` flag. If
 _S_ is neither a stream nor an atom the predicates just fails.

</li>
 <li>seeing(- _S_) @anchor seeing


The current input stream is unified with  _S_.

</li>
 <li>seen @anchor seen


Closes the current input stream (see 6.7.).

</li>
</ul>

@section InputOutput_of_Terms Handling Input/Output of Terms

<ul>

 <li>read(- _T_) [ISO] @anchor read


Reads the next term from the current input stream, and unifies it with
 _T_. The term must be followed by a dot ('.') and any blank-character
as previously defined. The syntax of the term must match the current
declarations for operators (see op). If the end-of-stream is reached, 
 _T_ is unified with the atom `end_of_file`. Further reads from of 
the same stream may cause an error failure (see [open/3](@ref open)).

</li>
 <li>read_term(- _T_,+ _Options_) [ISO] @anchor read_term


Reads term  _T_ from the current input stream with execution
controlled by the following options:

<ul>

 <li>term_position(- _Position_) @anchor term_position

Unify  _Position_ with a term describing the position of the stream
at the start of parse. Use [stream_position_data/3](@ref stream_position_data) to obtain extra
information.

</li>
 <li>singletons(- _Names_) @anchor singletons

Unify  _Names_ with a list of the form  _Name=Var_, where
 _Name_ is the name of a non-anonymous singleton variable in the
original term, and `Var` is the variable's representation in
YAP.
The variables occur in left-to-right traversal order.

</li>
 <li>syntax_errors(+ _Val_) @anchor syntax_errors

Control action to be taken after syntax errors. See [yap_flag/2](@ref yap_flag)
for detailed information.

</li>
 <li>variable_names(- _Names_) @anchor variable_names

Unify  _Names_ with a list of the form  _Name=Var_, where  _Name_ is
the name of a non-anonymous variable in the original term, and  _Var_
is the variable's representation in YAP.
The variables occur in left-to-right traversal order.

</li>
 <li>variables(- _Names_) @anchor variables

Unify  _Names_ with a list of the variables in term  _T_.
The variables occur in left-to-right traversal order.

</li>
</ul>

</li>
 <li>char_conversion(+ _IN_,+ _OUT_) [ISO] @anchor char_conversion


While reading terms convert unquoted occurrences of the character
 _IN_ to the character  _OUT_. Both  _IN_ and  _OUT_ must be
bound to single characters atoms.

Character conversion only works if the flag `char_conversion` is
on. This is default in the `iso` and `sicstus` language
modes. As an example, character conversion can be used for instance to
convert characters from the ISO-LATIN-1 character set to ASCII.

If  _IN_ is the same character as  _OUT_, [char_conversion/2](@ref char_conversion)
will remove this conversion from the table.

</li>
 <li>current_char_conversion(? _IN_,? _OUT_) [ISO] @anchor current_char_conversion


If  _IN_ is unbound give all current character
translations. Otherwise, give the translation for  _IN_, if one
exists.

</li>
 <li>write( _T_) [ISO] @anchor write


The term  _T_ is written to the current output stream according to
the operator declarations in force.

</li>
 <li>writeln( _T_) [ISO] @anchor writeln


Same as [write/1](@ref write) followed by [nl/0](@ref nl).

</li>
 <li>display(+ _T_) @anchor display


Displays term  _T_ on the current output stream. All Prolog terms are
written in standard parenthesized prefix notation.

</li>
 <li>write_canonical(+ _T_) [ISO] @anchor write_canonical


Displays term  _T_ on the current output stream. Atoms are quoted
when necessary, and operators are ignored, that is, the term is written
in standard parenthesized prefix notation.

</li>
 <li>write_term(+ _T_, + _Opts_) [ISO] @anchor write_term


Displays term  _T_ on the current output stream, according to the
following options:

<ul>
 <li>quoted(+ _Bool_) [ISO]
If `true`, quote atoms if this would be necessary for the atom to
be recognized as an atom by YAP's parser. The default value is
`false`.

</li>
 <li>ignore_ops(+ _Bool_) [ISO]
If `true`, ignore operator declarations when writing the term. The
default value is `false`.

</li>
 <li>numbervars(+ _Bool_) [ISO]
If `true`, output terms of the form
`'$VAR'(N)`, where  _N_ is an integer, as a sequence of capital
letters. The default value is `false`.

</li>
 <li>portrayed(+ _Bool_)
If `true`, use <tt>portray/1</tt> to portray bound terms. The default
value is `false`.

</li>
 <li>portray(+ _Bool_)
If `true`, use <tt>portray/1</tt> to portray bound terms. The default
value is `false`.

</li>
 <li>max_depth(+ _Depth_)
If `Depth` is a positive integer, use <tt>Depth</tt> as
the maximum depth to portray a term. The default is `0`, that is,
unlimited depth.

</li>
 <li>priority(+ _Piority_)
If `Priority` is a positive integer smaller than `1200`, 
give the context priority. The default is `1200`.

</li>
 <li>cycles(+ _Bool_)
Do not loop in rational trees (default).
</li>
</ul>

</li>
 <li>writeq( _T_) [ISO] @anchor writeq


Writes the term  _T_, quoting names to make the result acceptable to
the predicate 'read' whenever necessary.

</li>
 <li>print( _T_) @anchor print


Prints the term  _T_ to the current output stream using [write/1](@ref write)
unless T is bound and a call to the user-defined  predicate
`portray/1` succeeds. To do pretty  printing of terms the user should
define suitable clauses for `portray/1` and use [print/1](@ref print).

</li>
 <li>format(+ _T_,+ _L_) @anchor format


Print formatted output to the current output stream. The arguments in
list  _L_ are output according to the string or atom  _T_.

A control sequence is introduced by a `w`. The following control
sequences are available in YAP:

<ul>

 <li>'~~'
Print a single tilde.

</li>
 <li>'~a'
The next argument must be an atom, that will be printed as if by `write`.

</li>
 <li>'~Nc'
The next argument must be an integer, that will be printed as a
character code. The number  _N_ is the number of times to print the
character (default 1).

</li>
 <li>'~Ne'
</li>
 <li>'~NE'
</li>
 <li>'~Nf'
</li>
 <li>'~Ng'
</li>
 <li>'~NG'
The next argument must be a floating point number. The float  _F_, the number
 _N_ and the control code `c` will be passed to `printf` as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    printf("%s.Nc", F)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As an example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("~8e, ~8E, ~8f, ~8g, ~8G~w",
          [3.14,3.14,3.14,3.14,3.14,3.14]).
3.140000e+00, 3.140000E+00, 3.140000, 3.14, 3.143.14
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>'~Nd'
The next argument must be an integer, and  _N_ is the number of digits
after the decimal point. If  _N_ is `0` no decimal points will be
printed. The default is  _N = 0_.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("~2d, ~d",[15000, 15000]).
150.00, 15000
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>'~ND'
Identical to `'~Nd'`, except that commas are used to separate groups
of three digits.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("~2D, ~D",[150000, 150000]).
1,500.00, 150,000
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>'~i'
Ignore the next argument in the list of arguments:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format('The ~i met the boregrove',[mimsy]).
The  met the boregrove
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>'~k'
Print the next argument with `write_canonical`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("Good night ~k",a+[1,2]).
Good night +(a,[1,2])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>'~Nn'
Print  _N_ newlines (where  _N_ defaults to 1).

</li>
 <li>'~NN'
Print  _N_ newlines if at the beginning of the line (where  _N_
defaults to 1).

</li>
 <li>'~Nr'
The next argument must be an integer, and  _N_ is interpreted as a
radix, such that `2 \<= N \<= 36` (the default is 8).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("~2r, 0x~16r, ~r",
          [150000, 150000, 150000]).
100100100111110000, 0x249f0, 444760
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the letters `a-z` denote digits larger than 9.

</li>
 <li>'~NR'
Similar to '~NR'. The next argument must be an integer, and  _N_ is
interpreted as a radix, such that `2 \<= N \<= 36` (the default is 8).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("~2r, 0x~16r, ~r",
          [150000, 150000, 150000]).
100100100111110000, 0x249F0, 444760
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The only difference is that letters `A-Z` denote digits larger than 9.

</li>
 <li>'~p'
Print the next argument with [print/1](@ref print):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("Good night ~p",a+[1,2]).
Good night a+[1,2]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>'~q'
Print the next argument with [writeq/1](@ref writeq):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("Good night ~q",'Hello'+[1,2]).
Good night 'Hello'+[1,2]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>'~Ns'
The next argument must be a list of character codes. The system then
outputs their representation as a string, where  _N_ is the maximum
number of characters for the string ( _N_ defaults to the length of the
string).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("The ~s are ~4s",["woods","lovely"]).
The woods are love
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>'~w'
Print the next argument with [write/1](@ref write):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("Good night ~w",'Hello'+[1,2]).
Good night Hello+[1,2]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
</ul>
The number of arguments, `N`, may be given as an integer, or it
may be given as an extra argument. The next example shows a small
procedure to write a variable number of `a` characters:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_many_as(N) :-
        format("~*c",[N,0'a]).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The [format/2](@ref format) built-in also allows for formatted output.  One can
specify column boundaries and fill the intermediate space by a padding
character: 

<ul>
 <li>'~N|'
Set a column boundary at position  _N_, where  _N_ defaults to the
current position.

</li>
 <li>'~N+'
Set a column boundary at  _N_ characters past the current position, where
 _N_ defaults to `8`.

</li>
 <li>'~Nt'
Set padding for a column, where  _N_ is the fill code (default is
`SPC`).

</li>
</ul>

The next example shows how to align columns and padding. We first show
left-alignment:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- format("~n*Hello~16+*~n",[]).
*Hello          *
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that we reserve 16 characters for the column.

The following example shows how to do right-alignment:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- format("*~tHello~16+*~n",[]).
*          Hello*

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `~t` escape sequence forces filling before `Hello`. 

We next show how to do centering:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- format("*~tHello~t~16+*~n",[]).
*     Hello     *
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The two `~t` escape sequence force filling both before and after
`Hello`. Space is then evenly divided between the right and the
left sides.

</li>
 <li>format(+ _T_)

Print formatted output to the current output stream.

</li>
 <li>format(+ _S_,+ _T_,+ _L_)

Print formatted output to stream  _S_.

</li>
 <li>with_output_to(+ _Ouput_,: _Goal_) @anchor with_output_to


Run  _Goal_ as [once/1](@ref once), while characters written to the current
output are sent to  _Output_. The predicate is SWI-Prolog
specific.

Applications should generally avoid creating atoms by breaking and
concatenating other atoms as the creation of large numbers of
intermediate atoms generally leads to poor performance, even more so in
multi-threaded applications. This predicate supports creating
difference-lists from character data efficiently. The example below
defines the DCG rule `term/3` to insert a term in the output:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 term(Term, In, Tail) :-
        with_output_to(codes(In, Tail), write(Term)).

?- phrase(term(hello), X).

X = [104, 101, 108, 108, 111]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<ul>
 <li>A Stream handle or alias
Temporary switch current output to the given stream. Redirection using with_output_to/2 guarantees the original output is restored, also if Goal fails or raises an exception. See also call_cleanup/2. 
</li>
 <li>atom(- _Atom_)
Create an atom from the emitted characters. Please note the remark above. 
</li>
 <li>string(- _String_)
Create a string-object (not supported in YAP). 
</li>
 <li>codes(- _Codes_)
Create a list of character codes from the emitted characters, similar to atom_codes/2. 
</li>
 <li>codes(- _Codes_, - _Tail_)
Create a list of character codes as a difference-list. 
</li>
 <li>chars(- _Chars_)
Create a list of one-character-atoms codes from the emitted characters, similar to atom_chars/2. 
</li>
 <li>chars(- _Chars_, - _Tail_)
Create a list of one-character-atoms as a difference-list. 
</li>
</ul>

</li>
</ul>

@section InputOutput_of_Characters Handling Input/Output of Characters

<ul>

 <li>put(+ _N_) @anchor put


Outputs to the current output stream the character whose ASCII code is
 _N_. The character  _N_ must be a legal ASCII character code, an
expression yielding such a code, or a list in which case only the first
element is used.

</li>
 <li>put_byte(+ _N_) [ISO] @anchor put_byte


Outputs to the current output stream the character whose code is
 _N_. The current output stream must be a binary stream.

</li>
 <li>put_char(+ _N_) [ISO] @anchor put_char


Outputs to the current output stream the character who is used to build
the representation of atom `A`. The current output stream must be a
text stream.

</li>
 <li>put_code(+ _N_) [ISO] @anchor put_code


Outputs to the current output stream the character whose ASCII code is
 _N_. The current output stream must be a text stream. The character
 _N_ must be a legal ASCII character code, an expression yielding such
a code, or a list in which case only the first element is used.

</li>
 <li>get(- _C_) @anchor get


The next non-blank character from the current input stream is unified
with  _C_. Blank characters are the ones whose ASCII codes are not
greater than 32. If there are no more non-blank characters in the
stream,  _C_ is unified with -1. If `end_of_stream` has already
been reached in the previous reading, this call will give an error message.

</li>
 <li>get0(- _C_) @anchor get0


The next character from the current input stream is consumed, and then
unified with  _C_. There are no restrictions on the possible
values of the ASCII code for the character, but the character will be
internally converted by YAP.

</li>
 <li>get_byte(- _C_) [ISO] @anchor get_byte


If  _C_ is unbound, or is a character code, and the current stream is a
binary stream, read the next byte from the current stream and unify its
code with  _C_.

</li>
 <li>get_char(- _C_) [ISO] @anchor get_char


If  _C_ is unbound, or is an atom representation of a character, and
the current stream is a text stream, read the next character from the
current stream and unify its atom representation with  _C_.

</li>
 <li>get_code(- _C_) [ISO] @anchor get_code


If  _C_ is unbound, or is the code for a character, and
the current stream is a text stream, read the next character from the
current stream and unify its code with  _C_.

</li>
 <li>peek_byte(- _C_) [ISO] @anchor peek_byte


If  _C_ is unbound, or is a character code, and the current stream is a
binary stream, read the next byte from the current stream and unify its
code with  _C_, while leaving the current stream position unaltered.

</li>
 <li>peek_char(- _C_) [ISO] @anchor peek_char


If  _C_ is unbound, or is an atom representation of a character, and
the current stream is a text stream, read the next character from the
current stream and unify its atom representation with  _C_, while
leaving the current stream position unaltered.

</li>
 <li>peek_code(- _C_) [ISO] @anchor peek_code


If  _C_ is unbound, or is the code for a character, and
the current stream is a text stream, read the next character from the
current stream and unify its code with  _C_, while
leaving the current stream position unaltered.

</li>
 <li>skip(+ _N_) @anchor skip


Skips input characters until the next occurrence of the character with
ASCII code  _N_. The argument to this predicate can take the same forms
as those for `put` (see 6.11).

</li>
 <li>tab(+ _N_) @anchor tab


Outputs  _N_ spaces to the current output stream.

</li>
 <li>nl [ISO] @anchor nl


Outputs a new line to the current output stream.

</li>
</ul>

@section InputOutput_for_Streams Input/Output Predicates applied to Streams

<ul>

 <li>read(+ _S_,- _T_) [ISO]

Reads term  _T_ from the stream  _S_ instead of from the current input
stream.

</li>
 <li>read_term(+ _S_,- _T_,+ _Options_) [ISO]

Reads term  _T_ from stream  _S_ with execution controlled by the
same options as [read_term/2](@ref read_term).

</li>
 <li>write(+ _S_, _T_) [ISO]

Writes term  _T_ to stream  _S_ instead of to the current output
stream.

</li>
 <li>write_canonical(+ _S_,+ _T_) [ISO]

Displays term  _T_ on the stream  _S_. Atoms are quoted when
necessary, and operators are ignored.

</li>
 <li>write_term(+ _S_, + _T_, + _Opts_) [ISO]

Displays term  _T_ on the current output stream, according to the same
options used by `write_term/3`.

</li>
 <li>writeq(+ _S_, _T_) [ISO]

As [writeq/1](@ref writeq), but the output is sent to the stream  _S_.

</li>
 <li>display(+ _S_, _T_)

Like [display/1](@ref display), but using stream  _S_ to display the term.

</li>
 <li>print(+ _S_, _T_)

Prints term  _T_ to the stream  _S_ instead of to the current output
stream.

</li>
 <li>put(+ _S_,+ _N_)

As `put(N)`, but to stream  _S_.

</li>
 <li>put_byte(+ _S_,+ _N_) [ISO]

As `put_byte(N)`, but to binary stream  _S_.

</li>
 <li>put_char(+ _S_,+ _A_) [ISO]

As `put_char(A)`, but to text stream  _S_.

</li>
 <li>put_code(+ _S_,+ _N_) [ISO]

As `put_code(N)`, but to text stream  _S_.

</li>
 <li>get(+ _S_,- _C_)

The same as `get(C)`, but from stream  _S_.

</li>
 <li>get0(+ _S_,- _C_)

The same as `get0(C)`, but from stream  _S_.

</li>
 <li>get_byte(+ _S_,- _C_) [ISO]

If  _C_ is unbound, or is a character code, and the stream  _S_ is a
binary stream, read the next byte from that stream and unify its
code with  _C_.

</li>
 <li>get_char(+ _S_,- _C_) [ISO]

If  _C_ is unbound, or is an atom representation of a character, and
the stream  _S_ is a text stream, read the next character from that
stream and unify its representation as an atom with  _C_.

</li>
 <li>get_code(+ _S_,- _C_) [ISO]

If  _C_ is unbound, or is a character code, and the stream  _S_ is a
text stream, read the next character from that stream and unify its
code with  _C_.

</li>
 <li>peek_byte(+ _S_,- _C_) [ISO]

If  _C_ is unbound, or is a character code, and  _S_ is a binary
stream, read the next byte from the current stream and unify its code
with  _C_, while leaving the current stream position unaltered.

</li>
 <li>peek_char(+ _S_,- _C_) [ISO]

If  _C_ is unbound, or is an atom representation of a character, and
the stream  _S_ is a text stream, read the next character from that
stream and unify its representation as an atom with  _C_, while leaving
the current stream position unaltered.

</li>
 <li>peek_code(+ _S_,- _C_) [ISO]

If  _C_ is unbound, or is an atom representation of a character, and
the stream  _S_ is a text stream, read the next character from that
stream and unify its representation as an atom with  _C_, while leaving
the current stream position unaltered.

</li>
 <li>skip(+ _S_,- _C_)

Like [skip/1](@ref skip), but using stream  _S_ instead of the current
input stream.

</li>
 <li>tab(+ _S_,+ _N_)

The same as [tab/1](@ref tab), but using stream  _S_.

</li>
 <li>nl(+ _S_) [ISO]

Outputs a new line to stream  _S_.

</li>
</ul>

@section ChYProlog_to_Terminal Compatible C-Prolog predicates for Terminal Input/Output

<ul>

 <li>ttyput(+ _N_) @anchor ttyput


As `put(N)` but always to [user_output](@ref user_output).

</li>
 <li>ttyget(- _C_) @anchor ttyget


The same as `get(C)`, but from stream [user_input](@ref user_input).

</li>
 <li>ttyget0(- _C_) @anchor ttyget0


The same as `get0(C)`, but from stream [user_input](@ref user_input).

</li>
 <li>ttyskip(- _C_) @anchor ttyskip


Like [skip/1](@ref skip), but always using stream [user_input](@ref user_input).
stream.

</li>
 <li>ttytab(+ _N_) @anchor ttytab


The same as [tab/1](@ref tab), but using stream [user_output](@ref user_output).

</li>
 <li>ttynl @anchor ttynl


Outputs a new line to stream [user_output](@ref user_output).

</li>
</ul>

@section InputOutput_Control Controlling Input/Output

<ul>

 <li>exists(+ _F_) @anchor exists


Checks if file  _F_ exists in the current directory.

</li>
 <li>nofileerrors @anchor nofileerrors


Switches off the file_errors flag, so that the predicates [see/1](@ref see),
[tell/1](@ref tell), [open/3](@ref open) and [close/1](@ref close) just fail, instead of producing
an error message and aborting whenever the specified file cannot be
opened or closed.

</li>
 <li>fileerrors @anchor fileerrors


Switches on the file_errors flag so that in certain error conditions
Input/Output predicates will produce an appropriated message and abort.

</li>
 <li>always_prompt_user @anchor always_prompt_user


Force the system to prompt the user even if the [user_input](@ref user_input) stream
is not a terminal. This command is useful if you want to obtain
interactive control from a pipe or a socket.

</li>
</ul>

@section Sockets Using Sockets From YAP

YAP includes a SICStus Prolog compatible socket interface. In YAP-6.3
this uses the `clib` package to emulate the old low level interface that
provides direct access to the major socket system calls. These calls
can be used both to open a new connection in the network or connect to
a networked server. Socket connections are described as read/write
streams, and standard Input/Output built-ins can be used to write on or read
from sockets. The following calls are available:

<ul>

 <li>socket(+ _DOMAIN_,+ _TYPE_,+ _PROTOCOL_,- _SOCKET_) @anchor socket


Corresponds to the BSD system call `socket`. Create a socket for
domain  _DOMAIN_ of type  _TYPE_ and protocol
 _PROTOCOL_. Both  _DOMAIN_ and  _TYPE_ should be atoms,
whereas  _PROTOCOL_ must be an integer.
The new socket object is
accessible through a descriptor bound to the variable  _SOCKET_.

The current implementation of YAP  accepts socket
domains `'AF_INET'` and `'AF_UNIX'`. 
Socket types depend on the
underlying operating system, but at least the following types are
supported: `'SOCK_STREAM'` and `'SOCK_DGRAM'` (untested in 6.3).

</li>
 <li>socket(+ _DOMAIN_,- _SOCKET_)


Call [socket/4](@ref socket) with  _TYPE_ bound to `'SOCK_STREAM'` and
 _PROTOCOL_ bound to `0`.

</li>
 <li>socket_close(+ _SOCKET_) @anchor socket_close



Close socket  _SOCKET_. Note that sockets used in
`socket_connect` (that is, client sockets) should not be closed with
`socket_close`, as they will be automatically closed when the
corresponding stream is closed with [close/1](@ref close) or `close/2`.

</li>
 <li>socket_bind(+ _SOCKET_, ? _PORT_) @anchor socket_bind



Interface to system call `bind`, as used for servers: bind socket
to a port. Port information depends on the domain:

<ul>
 <li>'AF_UNIX'(+ _FILENAME_) (unsupported)
</li>
 <li>'AF_FILE'(+ _FILENAME_)
use file name  _FILENAME_ for UNIX or local sockets.

</li>
 <li>'AF_INET'(? _HOST_,?PORT)
If  _HOST_ is bound to an atom, bind to host  _HOST_, otherwise
if unbound bind to local host ( _HOST_ remains unbound). If port
 _PORT_ is bound to an integer, try to bind to the corresponding
port. If variable  _PORT_ is unbound allow operating systems to
choose a port number, which is unified with  _PORT_.

</li>
</ul>

</li>
 <li>socket_connect(+ _SOCKET_, + _PORT_, - _STREAM_) @anchor socket_connect



Interface to system call `connect`, used for clients: connect
socket  _SOCKET_ to  _PORT_. The connection results in the
read/write stream  _STREAM_.

Port information depends on the domain:

<ul>
 <li>'AF_UNIX'(+ _FILENAME_)
</li>
 <li>'AF_FILE'(+ _FILENAME_)
connect to socket at file  _FILENAME_.

</li>
 <li>'AF_INET'(+ _HOST_,+ _PORT_)
Connect to socket at host  _HOST_ and port  _PORT_.
</li>
</ul>

</li>
 <li>socket_listen(+ _SOCKET_, + _LENGTH_) @anchor socket_listen


Interface to system call `listen`, used for servers to indicate
willingness to wait for connections at socket  _SOCKET_. The
integer  _LENGTH_ gives the queue limit for incoming connections,
and should be limited to `5` for portable applications. The socket
must be of type `SOCK_STREAM` or `SOCK_SEQPACKET`.

</li>
 <li>socket_accept(+ _SOCKET_, - _CLIENT_, - _STREAM_) @anchor socket_accept


Interface to system call `accept`, used for servers to wait for
connections at socket  _SOCKET_. The stream descriptor  _STREAM_
represents the resulting connection.  If the socket belongs to the
domain `'AF_INET'`,  _CLIENT_ unifies with an atom containing
the IP address for the client in numbers and dots notation.

</li>
 <li>socket_accept(+ _SOCKET_, - _STREAM_)

Accept a connection but do not return client information.

</li>
 <li>socket_buffering(+ _SOCKET_, - _MODE_, - _OLD_, + _NEW_) @anchor socket_buffering


Set buffering for  _SOCKET_ in `read` or `write`
 _MODE_.  _OLD_ is unified with the previous status, and  _NEW_
receives the new status which may be one of `unbuf` or
`fullbuf`.

</li>
 <li>socket_select(+ _SOCKETS_, - _NEWSTREAMS_, + _TIMEOUT_, @anchor socket_select

+ _STREAMS_, - _READSTREAMS_) [unsupported in YAP-6.3]

Interface to system call `select`, used for servers to wait for
connection requests or for data at sockets. The variable
 _SOCKETS_ is a list of form  _KEY-SOCKET_, where  _KEY_ is
an user-defined identifier and  _SOCKET_ is a socket descriptor. The
variable  _TIMEOUT_ is either `off`, indicating execution will
wait until something is available, or of the form  _SEC-USEC_, where
 _SEC_ and  _USEC_ give the seconds and microseconds before
[socket_select/5](@ref socket_select) returns. The variable  _SOCKETS_ is a list of
form  _KEY-STREAM_, where  _KEY_ is an user-defined identifier
and  _STREAM_ is a stream descriptor

Execution of [socket_select/5](@ref socket_select) unifies  _READSTREAMS_ from
 _STREAMS_ with readable data, and  _NEWSTREAMS_ with a list of
the form  _KEY-STREAM_, where  _KEY_ was the key for a socket
with pending data, and  _STREAM_ the stream descriptor resulting
from accepting the connection.  

</li>
 <li>current_host(? _HOSTNAME_) @anchor current_host

Unify  _HOSTNAME_ with an atom representing the fully qualified
hostname for the current host. Also succeeds if  _HOSTNAME_ is bound
to the unqualified hostname.

</li>
 <li>hostname_address(? _HOSTNAME_,? _IP_ADDRESS_) @anchor hostname_address

 _HOSTNAME_ is an host name and  _IP_ADDRESS_ its IP
address in number and dots notation.

</li>
</ul>

@section Database Using the Clausal Data Base

Predicates in YAP may be dynamic or static. By default, when
consulting or reconsulting, predicates are assumed to be static:
execution is faster and the code will probably use less space.
Static predicates impose some restrictions: in general there can be no 
addition or removal of  clauses for a procedure if it is being used in the
current execution.

Dynamic predicates allow programmers to change the Clausal Data Base with
the same flexibility as in C-Prolog. With dynamic predicates it is
always possible to add or remove clauses during execution and the
semantics will be the same as for C-Prolog. But the programmer should be
aware of the fact that asserting or retracting are still expensive operations, 
and therefore he should try to avoid them whenever possible.

<ul>

 <li>dynamic + _P_ @anchor dynamic


Declares predicate  _P_ or list of predicates [ _P1_,..., _Pn_]
as a dynamic predicate.  _P_ must be written in form:
 _name/arity_.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- dynamic god/1.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
a more convenient form can be used:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- dynamic son/3, father/2, mother/2.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

or, equivalently,

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- dynamic [son/3, father/2, mother/2].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note:

a predicate is assumed to be dynamic when 
asserted before being defined.

</li>
 <li>dynamic_predicate(+ _P_,+ _Semantics_) @anchor dynamic_predicate


Declares predicate  _P_ or list of predicates [ _P1_,..., _Pn_]
as a dynamic predicate following either `logical` or
`immediate` semantics.

</li>
 <li>compile_predicates(: _ListOfNameArity_) @anchor compile_predicates



Compile a list of specified dynamic predicates (see [dynamic/1](@ref dynamic) and
[assert/1](@ref assert) into normal static predicates. This call tells the
Prolog environment the definition will not change anymore and further
calls to [assert/1](@ref assert) or [retract/1](@ref retract) on the named predicates
raise a permission error. This predicate is designed to deal with parts
of the program that is generated at runtime but does not change during
the remainder of the program execution.

</li>
</ul>

@section Modifying_the_Database Modification of the Data Base

These predicates can be used either for static or for dynamic
predicates:

<ul>

 <li>assert(+ _C_) @anchor assert


Same as [assertz/1](@ref assertz). Adds clause  _C_ to the program. If the predicate is undefined,
declare it as dynamic. New code should use [assertz/1](@ref assertz) for better portability.

Most Prolog systems only allow asserting clauses for dynamic
predicates. This is also as specified in the ISO standard. YAP allows
asserting clauses for static predicates, as long as the predicate is not
in use and the language flag is <tt>cprolog</tt>. Note that this feature is
deprecated, if you want to assert clauses for static procedures you
should use [assert_static/1](@ref assert_static).

</li>
 <li>asserta(+ _C_) [ISO] @anchor asserta


Adds clause  _C_ to the beginning of the program. If the predicate is
undefined, declare it as dynamic.

</li>
 <li>assertz(+ _C_) [ISO] @anchor assertz


Adds clause  _C_ to the end of the program. If the predicate is
undefined, declare it as dynamic.

Most Prolog systems only allow asserting clauses for dynamic
predicates. This is also as specified in the ISO standard. YAP allows
asserting clauses for static predicates. The current version of YAP
supports this feature, but this feature is deprecated and support may go
away in future versions.

</li>
 <li>abolish(+ _PredSpec_) [ISO] @anchor abolish


Deletes the predicate given by  _PredSpec_ from the database. If
 _PredSpec_ is an unbound variable, delete all predicates for the
current module. The
specification must include the name and arity, and it may include module
information. Under <tt>iso</tt> language mode this built-in will only abolish
dynamic procedures. Under other modes it will abolish any procedures. 

</li>
 <li>abolish(+ _P_,+ _N_)

Deletes the predicate with name  _P_ and arity  _N_. It will remove
both static and dynamic predicates.

</li>
 <li>assert_static(: _C_) @anchor assert_static


Adds clause  _C_ to a static procedure. Asserting a static clause
for a predicate while choice-points for the predicate are available has
undefined results.

</li>
 <li>asserta_static(: _C_) @anchor asserta_static


Adds clause  _C_ to the beginning of a static procedure. 

</li>
 <li>assertz_static(: _C_) @anchor assertz_static


Adds clause  _C_ to the end of a static procedure.  Asserting a
static clause for a predicate while choice-points for the predicate are
available has undefined results.

</li>
</ul>

The following predicates can be used for dynamic predicates and for
static predicates, if source mode was on when they were compiled:

<ul>

 <li>clause(+ _H_, _B_) [ISO] @anchor clause


A clause whose head matches  _H_ is searched for in the
program. Its head and body are respectively unified with  _H_ and
 _B_. If the clause is a unit clause,  _B_ is unified with
 _true_.

This predicate is applicable to static procedures compiled with
`source` active, and to all dynamic procedures.

</li>
 <li>clause(+ _H_, _B_,- _R_)

The same as [clause/2](@ref clause), plus  _R_ is unified with the
reference to the clause in the database. You can use [instance/2](@ref instance)
to access the reference's value. Note that you may not use
[erase/1](@ref erase) on the reference on static procedures.

</li>
 <li>nth_clause(+ _H_, _I_,- _R_) @anchor nth_clause


Find the  _I_th clause in the predicate defining  _H_, and give
a reference to the clause. Alternatively, if the reference  _R_ is
given the head  _H_ is unified with a description of the predicate
and  _I_ is bound to its position.

</li>
</ul>

The following predicates can only be used for dynamic predicates:

<ul>

 <li>retract(+ _C_) [ISO] @anchor retract


Erases the first clause in the program that matches  _C_. This
predicate may also be used for the static predicates that have been
compiled when the source mode was `on`. For more information on
[source/0](@ref source) ( (see [Setting the Compiler](@ref Setting_the_Compiler))).

</li>
 <li>retractall(+ _G_) [ISO] @anchor retractall


Retract all the clauses whose head matches the goal  _G_. Goal
 _G_ must be a call to a dynamic predicate.

</li>
</ul>

@section Looking_at_the_Database Looking at the Data Base

<ul>

 <li>listing @anchor listing


Lists in the current output stream all the clauses for which source code
is available (these include all clauses for dynamic predicates and
clauses for static predicates compiled when source mode was `on`).

</li>
 <li>listing(+ _P_)

Lists predicate  _P_ if its source code is available.

</li>
 <li>portray_clause(+ _C_) @anchor portray_clause


Write clause  _C_ as if written by [listing/0](@ref listing).

</li>
 <li>portray_clause(+ _S_,+ _C_)

Write clause  _C_ on stream  _S_ as if written by [listing/0](@ref listing).

</li>
 <li>current_atom( _A_) @anchor current_atom


Checks whether  _A_ is a currently defined atom. It is used to find all
currently defined atoms by backtracking.

</li>
 <li>current_predicate( _F_) [ISO] @anchor current_predicate


 _F_ is the predicate indicator for a currently defined user or
library predicate.  _F_ is of the form  _Na/Ar_, where the atom
 _Na_ is the name of the predicate, and  _Ar_ its arity.

</li>
 <li>current_predicate( _A_, _P_)

Defines the relation:  _P_ is a currently defined predicate whose
name is the atom  _A_.

</li>
 <li>system_predicate( _A_, _P_) @anchor system_predicate


Defines the relation:   _P_ is a built-in predicate whose name
is the atom  _A_.

</li>
 <li>predicate_property( _P_, _Prop_) [ISO] @anchor predicate_property


For the predicates obeying the specification  _P_ unify  _Prop_
with a property of  _P_. These properties may be:

<ul>
 <li>built_in @anchor built_in

true for built-in predicates,
</li>
 <li>dynamic
true if the predicate is dynamic
</li>
 <li>static @anchor static

true if the predicate is static
</li>
 <li>meta_predicate( _M_) @anchor meta_predicate_flag

true if the predicate has a meta_predicate declaration  _M_.
</li>
 <li>multifile @anchor multifile_flag

true if the predicate was declared to be multifile
</li>
 <li>imported_from( _Mod_) @anchor imported_from

true if the predicate was imported from module  _Mod_.
</li>
 <li>exported @anchor exported

true if the predicate is exported in the current module.
</li>
 <li>public
true if the predicate is public; note that all dynamic predicates are
public.
</li>
 <li>tabled @anchor tabled

true if the predicate is tabled; note that only static predicates can
be tabled in YAP.
</li>
 <li>source (predicate_property flag) @anchor source_flag

true if source for the predicate is available.
</li>
 <li>number_of_clauses( _ClauseCount_) @anchor number_of_clauses

Number of clauses in the predicate definition. Always one if external
or built-in.
</li>
</ul>

</li>
 <li>predicate_statistics( _P_, _NCls_, _Sz_, _IndexSz_)  @anchor predicate_statistics


Given predicate  _P_,  _NCls_ is the number of clauses for
 _P_,  _Sz_ is the amount of space taken to store those clauses
(in bytes), and  _IndexSz_ is the amount of space required to store
indices to those clauses (in bytes).

</li>
 <li>predicate_erased_statistics( _P_, _NCls_, _Sz_, _IndexSz_)  @anchor predicate_erased_statistics


Given predicate  _P_,  _NCls_ is the number of erased clauses for
 _P_ that could not be discarded yet,  _Sz_ is the amount of space
taken to store those clauses (in bytes), and  _IndexSz_ is the amount
of space required to store indices to those clauses (in bytes).

</li>
</ul>

@section Database_References Using Data Base References

Data Base references are a fast way of accessing terms. The predicates
[erase/1](@ref erase) and `instance/1` also apply to these references and may
sometimes be used instead of [retract/1](@ref retract) and [clause/2](@ref clause).

<ul>

 <li>assert(+ _C_,- _R_)

The same as `assert(C)` ( (see [Modifying the Database](@ref Modifying_the_Database))) but
unifies  _R_ with the  database reference that identifies the new
clause, in a one-to-one way. Note that `asserta/2` only works for dynamic
predicates. If the predicate is undefined, it will automatically be
declared dynamic.

</li>
 <li>asserta(+ _C_,- _R_)

The same as `asserta(C)` but unifying  _R_ with
the  database reference that identifies the new clause, in a 
one-to-one way. Note that `asserta/2` only works for dynamic
predicates. If the predicate is undefined, it will automatically be
declared dynamic.

</li>
 <li>assertz(+ _C_,- _R_)

The same as `assertz(C)` but unifying  _R_ with
the  database reference that identifies the new clause, in a 
one-to-one way. Note that `asserta/2` only works for dynamic
predicates. If the predicate is undefined, it will automatically be
declared dynamic.

</li>
 <li>retract(+ _C_,- _R_)

Erases from the program the clause  _C_ whose 
database reference is  _R_. The predicate must be dynamic.

</li>
</ul>

@section Internal_Database Internal Data Base

Some programs need global information for, e.g. counting or collecting 
data obtained by backtracking. As a rule, to keep this information, the
internal data base should be used instead of asserting and retracting
clauses (as most novice programmers  do), .
In YAP (as in some other Prolog systems) the internal data base (i.d.b. 
for short) is faster, needs less space and provides a better insulation of 
program and data than using asserted/retracted clauses.
The i.d.b. is implemented as a set of terms, accessed by keys that 
unlikely what happens in (non-Prolog) data bases are not part of the 
term. Under each key a list of terms is kept. References are provided so that 
terms can be identified: each term in the i.d.b. has a unique reference 
(references are also available for clauses of dynamic predicates).

<ul>

 <li>recorda(+ _K_, _T_,- _R_) @anchor recorda


Makes term  _T_ the first record under key  _K_ and  unifies  _R_
with its reference.

</li>
 <li>recordz(+ _K_, _T_,- _R_) @anchor recordz


Makes term  _T_ the last record under key  _K_ and unifies  _R_
with its reference.

</li>
 <li>recorda_at(+ _R0_, _T_,- _R_) @anchor recorda_at


Makes term  _T_ the record preceding record with reference
 _R0_, and unifies  _R_ with its reference.

</li>
 <li>recordz_at(+ _R0_, _T_,- _R_) @anchor recordz_at


Makes term  _T_ the record following record with reference
 _R0_, and unifies  _R_ with its reference.

</li>
 <li>recordaifnot(+ _K_, _T_,- _R_) @anchor recordaifnot


If a term equal to  _T_ up to variable renaming is stored under key
 _K_ fail. Otherwise, make term  _T_ the first record under key
 _K_ and unify  _R_ with its reference.

</li>
 <li>recordzifnot(+ _K_, _T_,- _R_) @anchor recordzifnot


If a term equal to  _T_ up to variable renaming is stored under key
 _K_ fail. Otherwise, make term  _T_ the first record under key
 _K_ and unify  _R_ with its reference.

This predicate is YAP specific.

</li>
 <li>recorded(+ _K_, _T_, _R_) @anchor recorded


Searches in the internal database under the key  _K_, a term that
unifies with  _T_ and whose reference matches  _R_. This
built-in may be used in one of two ways:

<ul>
 <li>_K_ may be given, in this case the built-in will return all
elements of the internal data-base that match the key.
</li>
 <li>_R_ may be given, if so returning the key and element that
match the reference.
</li>
</ul>

</li>
 <li>erase(+ _R_) @anchor erase


The term referred to by  _R_ is erased from the internal database. If
reference  _R_ does not exist in the database, `erase` just fails.

</li>
 <li>erased(+ _R_) @anchor erased


Succeeds if the object whose database reference is  _R_ has been
erased.

</li>
 <li>instance(+ _R_,- _T_) @anchor instance


If  _R_ refers to a clause or a recorded term,  _T_ is unified
with its most general instance. If  _R_ refers to an unit clause
 _C_, then  _T_ is unified with ` _C_ :- true`. When
 _R_ is not a reference to an existing clause or to a recorded term,
this goal fails.

</li>
 <li>eraseall(+ _K_) @anchor eraseall


All terms belonging to the key `K` are erased from the internal
database. The predicate always succeeds.

</li>
 <li>current_key(? _A_,? _K_) @anchor current_key


Defines the relation:  _K_ is a currently defined database key whose
name is the atom  _A_. It can be used to generate all the keys for
the internal data-base.

</li>
 <li>nth_instance(? _Key_,? _Index_,? _R_) @anchor nth_instance


Fetches the  _Index_nth entry in the internal database under the key
 _Key_. Entries are numbered from one. If the key  _Key_ or the
 _Index_ are bound, a reference is unified with  _R_. Otherwise,
the reference  _R_ must be given, and YAP will find
the matching key and index.

</li>
 <li>nth_instance(? _Key_,? _Index_, _T_,? _R_)

Fetches the  _Index_nth entry in the internal database under the key
 _Key_. Entries are numbered from one. If the key  _Key_ or the
 _Index_ are bound, a reference is unified with  _R_. Otherwise,
the reference  _R_ must be given, and YAP will find
the matching key and index.

</li>
 <li>key_statistics(+ _K_,- _Entries_,- _Size_,- _IndexSize_) @anchor key_statistics


Returns several statistics for a key  _K_. Currently, it says how
many entries we have for that key,  _Entries_, what is the
total size spent on entries,  _Size_, and what is the amount of
space spent in indices.

</li>
 <li>key_statistics(+ _K_,- _Entries_,- _TotalSize_)

Returns several statistics for a key  _K_. Currently, it says how
many entries we have for that key,  _Entries_, what is the
total size spent on this key.

</li>
 <li>get_value(+ _A_,- _V_) @anchor get_value


In YAP, atoms can be associated with constants. If one such
association exists for atom  _A_, unify the second argument with the
constant. Otherwise, unify  _V_ with `[]`.

This predicate is YAP specific.

</li>
 <li>set_value(+ _A_,+ _C_) @anchor set_value


Associate atom  _A_ with constant  _C_.

The `set_value` and `get_value` built-ins give a fast alternative to
the internal data-base. This is a simple form of implementing a global
counter.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       read_and_increment_counter(Value) :-
                get_value(counter, Value),
                Value1 is Value+1,
                set_value(counter, Value1).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This predicate is YAP specific.

</li>
</ul>

There is a strong analogy between the i.d.b. and the way dynamic 
predicates are stored. In fact, the main i.d.b. predicates might be 
implemented using dynamic predicates:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
recorda(X,T,R) :- asserta(idb(X,T),R).
recordz(X,T,R) :- assertz(idb(X,T),R).
recorded(X,T,R) :- clause(idb(X,T),R).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We can take advantage of this, the other way around, as it is quite 
easy to write a simple Prolog interpreter, using the i.d.b.:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
asserta(G) :- recorda(interpreter,G,_).
assertz(G) :- recordz(interpreter,G,_).
retract(G) :- recorded(interpreter,G,R), !, erase(R).
call(V) :- var(V), !, fail.
call((H :- B)) :- !, recorded(interpreter,(H :- B),_), call(B).
call(G) :- recorded(interpreter,G,_).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In YAP, much attention has been given to the implementation of the 
i.d.b., especially to the problem of accelerating the access to terms kept in 
a large list under the same key. Besides using the key, YAP uses an internal 
lookup function, transparent to the user, to find only the terms that might 
unify. For instance, in a data base containing the terms

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
b
b(a)
c(d)
e(g)
b(X)
e(h)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stored under the key k/1, when executing the query 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- recorded(k(_),c(_),R).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`recorded` would proceed directly to the third term, spending almost the 
time as if `a(X)` or `b(X)` was being searched.
The lookup function uses the functor of the term, and its first three
arguments (when they exist). So, `recorded(k(_),e(h),_)` would go
directly to the last term, while `recorded(k(_),e(_),_)` would find
first the fourth term, and then, after backtracking, the last one.

This mechanism may be useful to implement a sort of hierarchy, where 
the functors of the terms (and eventually the first arguments) work as 
secondary keys.

In the YAP's i.d.b. an optimized representation is used for 
terms without free variables. This results in a faster retrieval of terms 
and better space usage. Whenever possible, avoid variables in terms in terms stored in the  i.d.b.

@section BlackBoard The Blackboard

YAP implements a blackboard in the style of the SICStus Prolog
blackboard. The blackboard uses the same underlying mechanism as the
internal data-base but has several important differences:

<ul>
 <li>It is module aware, in contrast to the internal data-base.
</li>
 <li>Keys can only be atoms or integers, and not compound terms.
</li>
 <li>A single term can be stored per key.
</li>
 <li>An atomic update operation is provided; this is useful for
parallelism.
</li>
</ul>

<ul>
 <li>bb_put(+ _Key_,? _Term_) @anchor bb_put


Store term table  _Term_ in the blackboard under key  _Key_. If a
previous term was stored under key  _Key_ it is simply forgotten.

</li>
 <li>bb_get(+ _Key_,? _Term_) @anchor bb_get


Unify  _Term_ with a term stored in the blackboard under key
 _Key_, or fail silently if no such term exists.

</li>
 <li>bb_delete(+ _Key_,? _Term_) @anchor bb_delete


Delete any term stored in the blackboard under key  _Key_ and unify
it with  _Term_. Fail silently if no such term exists.

</li>
 <li>bb_update(+ _Key_,? _Term_,? _New_) @anchor bb_update


Atomically  unify a term stored in the blackboard under key  _Key_
with  _Term_, and if the unification succeeds replace it by
 _New_. Fail silently if no such term exists or if unification fails.

</li>
</ul>

@section Sets Collecting Solutions to a Goal

When there are several solutions to a goal, if the user wants to collect all
the solutions he may be led to use the data base, because backtracking will
forget previous solutions.

YAP allows the programmer to choose from several system
predicates instead of writing his own routines.  [findall/3](@ref findall) gives you
the fastest, but crudest solution. The other built-in predicates
post-process the result of the query in several different ways:

<ul>

 <li>findall( _T_,+ _G_,- _L_) [ISO] @anchor findall


Unifies  _L_ with a list that contains all the instantiations of the
term  _T_ satisfying the goal  _G_.

With the following program:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a(2,1).
a(1,1).
a(2,2).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
the answer to the query

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
findall(X,a(X,Y),L).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
would be:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
X = _32
Y = _33
L = [2,1,2];
no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>findall( _T_,+ _G_,+ _L_,- _L0_)

Similar to [findall/3](@ref findall), but appends all answers to list  _L0_.

</li>
 <li>all( _T_,+ _G_,- _L_) @anchor all


Similar to `findall( _T_, _G_, _L_)` but eliminate
repeated elements. Thus, assuming the same clauses as in the above
example, the reply to the query

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all(X,a(X,Y),L).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
would be:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
X = _32
Y = _33
L = [2,1];
no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that [all/3](@ref all) will fail if no answers are found.

</li>
 <li>bagof( _T_,+ _G_,- _L_) [ISO] @anchor bagof


For each set of possible instances of the free variables occurring in
 _G_ but not in  _T_, generates the list  _L_ of the instances of
 _T_ satisfying  _G_. Again, assuming the same clauses as in the
examples above, the reply to the query

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bagof(X,a(X,Y),L).

would be:
X = _32
Y = 1
L = [2,1];
X = _32
Y = 2
L = [2];
no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>setof( _X_,+ _P_,- _B_) [ISO] @anchor setof


Similar to `bagof( _T_, _G_, _L_)` but sorts list
 _L_ and keeping only one copy of each element.  Again, assuming the
same clauses as in the examples above, the reply to the query

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setof(X,a(X,Y),L).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
would be:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
X = _32
Y = 1
L = [1,2];
X = _32
Y = 2
L = [2];
no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
</ul>

@section Grammars Grammar Rules

Grammar rules in Prolog are both a convenient way to express definite
clause grammars and  an extension of the well known context-free grammars.

A grammar rule is of the form:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head --> body
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where both \a head and \a body are sequences of one or more items
linked by the standard conjunction operator ','.

<em>Items can be:</em>

<ul>
 <li>
a <em>non-terminal</em> symbol may be either a complex term or an atom.
</li>
 <li>
a <em>terminal</em> symbol may be any Prolog symbol. Terminals are
written as Prolog lists.
</li>
 <li>
an <em>empty body</em> is written as the empty list '[ ]'.
</li>
 <li>
<em>extra conditions</em> may be inserted as Prolog procedure calls, by being
written inside curly brackets '{' and '}'.
</li>
 <li>
the left side of a rule consists of a nonterminal and an optional list
of terminals.
</li>
 <li>
alternatives may be stated in the right-hand side of the rule by using
the disjunction operator ';'.
</li>
 <li>
the <em>cut</em> and <em>conditional</em> symbol ('-\>') may be inserted in the 
right hand side of a grammar rule
</li>
</ul>

Grammar related built-in predicates:

<ul>

 <li>expand_term( _T_,- _X_) @anchor expand_term



This predicate is used by YAP for preprocessing each top level
term read when consulting a file and before asserting or executing it.
It rewrites a term  _T_ to a term  _X_ according to the following
rules: first try [term_expansion/2](@ref term_expansion)  in the current module, and then try to use the user defined predicate
`user:term_expansion/2`. If this call fails then the translating process
for DCG rules is applied, together with the arithmetic optimizer
whenever the compilation of arithmetic expressions is in progress.

</li>
 <li>_CurrentModule_:term_expansion( _T_,- _X_),  user:term_expansion( _T_,- _X_) @anchor term_expansion


This user-defined predicate is called by `expand_term/3` to
preprocess all terms read when consulting a file. If it succeeds:

<ul>
 <li>
If  _X_ is of the form `:- G` or `?- G`, it is processed as
a directive.
</li>
 <li>
If  _X_ is of the form `'$source_location'( _File_, _Line_): _Clause_` it is processed as if from `File` and line `Line`.

</li>
 <li>
If  _X_ is a list, all terms of the list are asserted or processed
as directives.
</li>
 <li>The term  _X_ is asserted instead of  _T_.
</li>
</ul>

</li>
 <li>_CurrentModule_:goal_expansion(+ _G_,+ _M_,- _NG_), user:goal_expansion(+ _G_,+ _M_,- _NG_) @anchor goal_expansion


YAP now supports [goal_expansion/3](@ref goal_expansion). This is an user-defined
procedure that is called after term expansion when compiling or
asserting goals for each sub-goal in a clause. The first argument is
bound to the goal and the second to the module under which the goal
 _G_ will execute. If [goal_expansion/3](@ref goal_expansion) succeeds the new
sub-goal  _NG_ will replace  _G_ and will be processed in the same
way. If [goal_expansion/3](@ref goal_expansion) fails the system will use the default
rules.

</li>
 <li>phrase(+ _P_, _L_, _R_) @anchor phrase


This predicate succeeds when the difference list ` _L_- _R_`
is a phrase of type  _P_.

</li>
 <li>phrase(+ _P_, _L_)

This predicate succeeds when  _L_ is a phrase of type  _P_. The
same as `phrase(P,L,[])`.

Both this predicate and the previous are used as a convenient way to
start execution of grammar rules.

</li>
 <li>'C'( _S1_, _T_, _S2_) @anchor C


This predicate is used by the grammar rules compiler and is defined as
`'C'([H|T],H,T)`.

</li>
</ul>

@section OS Access to Operating System Functionality

The following built-in predicates allow access to underlying
Operating System functionality: 

<ul>

 <li>cd(+ _D_) @anchor cd


Changes the current directory (on UNIX environments).

</li>
 <li>cd

Changes the current directory (on UNIX environments) to the user's home directory.

</li>
 <li>environ(+ _E_,- _S_) @anchor environ





Given an environment variable  _E_ this predicate unifies the second argument  _S_ with its value.

</li>
 <li>getcwd(- _D_) @anchor getcwd


Unify the current directory, represented as an atom, with the argument
 _D_.

</li>
 <li>pwd @anchor pwd


Prints the current directory.

</li>
 <li>ls @anchor ls


Prints a list of all files in the current directory.

</li>
 <li>putenv(+ _E_,+ _S_) @anchor putenv


Set environment variable  _E_ to the value  _S_. If the
environment variable  _E_ does not exist, create a new one. Both the
environment variable and the value must be atoms.

</li>
 <li>rename(+ _F_,+ _G_) @anchor rename


Renames file  _F_ to  _G_.

</li>
 <li>sh @anchor sh


Creates a new shell interaction.

</li>
 <li>system(+ _S_) @anchor system


Passes command  _S_ to the Bourne shell (on UNIX environments) or the
current command interpreter in WIN32 environments.

</li>
 <li>unix(+ _S_) @anchor unix


Access to Unix-like functionality:

<ul>
 <li>argv/1
Return a list of arguments to the program. These are the arguments that
follow a `--`, as in the usual Unix convention.
</li>
 <li>cd/0
Change to home directory.
</li>
 <li>cd/1
Change to given directory. Acceptable directory names are strings or
atoms.
</li>
 <li>environ/2
If the first argument is an atom, unify the second argument with the
value of the corresponding environment variable.
</li>
 <li>getcwd/1
Unify the first argument with an atom representing the current directory.
</li>
 <li>putenv/2
Set environment variable  _E_ to the value  _S_. If the
environment variable  _E_ does not exist, create a new one. Both the
environment variable and the value must be atoms.
</li>
 <li>shell/1
Execute command under current shell. Acceptable commands are strings or
atoms.
</li>
 <li>system/1
Execute command with `/bin/sh`. Acceptable commands are strings or
atoms.
</li>
 <li>shell/0
Execute a new shell.
</li>
</ul>

</li>
 <li>working_directory(- _CurDir_,? _NextDir_) @anchor working_directory


Fetch the current directory at  _CurDir_. If  _NextDir_ is bound
to an atom, make its value the current working directory.

</li>
 <li>alarm(+ _Seconds_,+ _Callable_,+ _OldAlarm_) @anchor alarm


Arranges for YAP to be interrupted in  _Seconds_ seconds, or in
[ _Seconds_| _MicroSeconds_]. When interrupted, YAP will execute
 _Callable_ and then return to the previous execution. If
 _Seconds_ is `0`, no new alarm is scheduled. In any event,
any previously set alarm is canceled.

The variable  _OldAlarm_ unifies with the number of seconds remaining
until any previously scheduled alarm was due to be delivered, or with
`0` if there was no previously scheduled alarm.

Note that execution of  _Callable_ will wait if YAP is
executing built-in predicates, such as Input/Output operations.

The next example shows how  _alarm/3_ can be used to implement a
simple clock:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
loop :- loop.

ticker :- write('.'), flush_output,
          get_value(tick, yes),
          alarm(1,ticker,_).

:- set_value(tick, yes), alarm(1,ticker,_), loop.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The clock, `ticker`, writes a dot and then checks the flag
`tick` to see whether it can continue ticking. If so, it calls
itself again. Note that there is no guarantee that the each dot
corresponds a second: for instance, if the YAP is waiting for
user input, `ticker` will wait until the user types the entry in.

The next example shows how [alarm/3](@ref alarm) can be used to guarantee that
a certain procedure does not take longer than a certain amount of time:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
loop :- loop.

:-   catch((alarm(10, throw(ball), _),loop),
        ball,
        format('Quota exhausted.~n',[])).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In this case after `10` seconds our `loop` is interrupted,
`ball` is thrown,  and the handler writes `Quota exhausted`.
Execution then continues from the handler.

Note that in this case `loop/0` always executes until the alarm is
sent. Often, the code you are executing succeeds or fails before the
alarm is actually delivered. In this case, you probably want to disable
the alarm when you leave the procedure. The next procedure does exactly so:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
once_with_alarm(Time,Goal,DoOnAlarm) :-
   catch(execute_once_with_alarm(Time, Goal), alarm, DoOnAlarm).

execute_once_with_alarm(Time, Goal) :-
        alarm(Time, alarm, _),
        ( call(Goal) -> alarm(0, alarm, _) ; alarm(0, alarm, _), fail).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The procedure `once_with_alarm/3` has three arguments:
the  _Time_ to wait before the alarm is
sent; the  _Goal_ to execute; and the goal  _DoOnAlarm_ to execute
if the alarm is sent. It uses [catch/3](@ref catch) to handle the case the
`alarm` is sent. Then it starts the alarm, calls the goal
 _Goal_, and disables the alarm on success or failure.

</li>
 <li>on_signal(+ _Signal_,? _OldAction_,+ _Callable_) @anchor on_signal


Set the interrupt handler for soft interrupt  _Signal_ to be
 _Callable_.  _OldAction_ is unified with the previous handler.

Only a subset of the software interrupts (signals) can have their
handlers manipulated through [on_signal/3](@ref on_signal).
Their POSIX names, YAP names and default behavior is given below.
The "YAP name" of the signal is the atom that is associated with
each signal, and should be used as the first argument to
[on_signal/3](@ref on_signal). It is chosen so that it matches the signal's POSIX
name.

[on_signal/3](@ref on_signal) succeeds, unless when called with an invalid
signal name or one that is not supported on this platform. No checks
are made on the handler provided by the user.

<ul>
 <li>sig_up (Hangup)
SIGHUP in Unix/Linux; Reconsult the initialization files
~/.yaprc, ~/.prologrc and ~/prolog.ini.
</li>
 <li>sig_usr1 and sig_usr2 (User signals)
SIGUSR1 and SIGUSR2 in Unix/Linux; Print a message and halt.
</li>
</ul>

A special case is made, where if  _Callable_ is bound to
`default`, then the default handler is restored for that signal.

A call in the form `on_signal( _S_, _H_, _H_)` can be used
to retrieve a signal's current handler without changing it.

It must be noted that although a signal can be received at all times,
the handler is not executed while YAP is waiting for a query at the
prompt. The signal will be, however, registered and dealt with as soon
as the user makes a query.

Please also note, that neither POSIX Operating Systems nor YAP guarantee
that the order of delivery and handling is going to correspond with the
order of dispatch.

</li>
</ul>

@section Term_Modification Term Modification

It is sometimes useful to change the value of instantiated
variables. Although, this is against the spirit of logic programming, it
is sometimes useful. As in other Prolog systems, YAP has
several primitives that allow updating Prolog terms. Note that these
primitives are also backtrackable.

The `setarg/3` primitive allows updating any argument of a Prolog
compound terms. The `mutable` family of predicates provides
<em>mutable variables</em>. They should be used instead of `setarg/3`,
as they allow the encapsulation of accesses to updatable
variables. Their implementation can also be more efficient for long
deterministic computations.

<ul>
 <li>setarg(+ _I_,+ _S_,? _T_) @anchor setarg3n


Set the value of the  _I_th argument of term  _S_ to term  _T_. 

</li>
 <li>create_mutable(+ _D_,- _M_) @anchor create_mutable


Create new mutable variable  _M_ with initial value  _D_.

</li>
 <li>is_mutable(? _D_) @anchor is_mutable


Holds if  _D_ is a mutable term.

</li>
 <li>get_mutable(? _D_,+ _M_) @anchor get_mutable


Unify the current value of mutable term  _M_ with term  _D_.

</li>
 <li>update_mutable(+ _D_,+ _M_) @anchor update_mutable


Set the current value of mutable term  _M_ to term  _D_.
</li>
</ul>

@section Global_Variables Global Variables

Global variables are associations between names (atoms) and
terms. They differ in various ways from storing information using
[assert/1](@ref assert) or [recorda/3](@ref recorda).

<ul>
 <li>The value lives on the Prolog (global) stack. This implies that
lookup time is independent from the size of the term. This is
particularly interesting for large data structures such as parsed XML
documents or the CHR global constraint store. 

</li>
 <li>They support both global assignment using [nb_setval/2](@ref nb_setval) and
backtrackable assignment using [b_setval/2](@ref b_setval).

</li>
 <li>Only one value (which can be an arbitrary complex Prolog term)
can be associated to a variable at a time. 

</li>
 <li>Their value cannot be shared among threads. Each thread has its own
namespace and values for global variables.
</li>
</ul>

Currently global variables are scoped globally. We may consider module
scoping in future versions.   Both [b_setval/2](@ref b_setval) and
[nb_setval/2](@ref nb_setval) implicitly create a variable if the referenced name
does not already refer to a variable.

Global variables may be initialised from directives to make them
available during the program lifetime, but some considerations are
necessary for saved-states and threads. Saved-states to not store
global variables, which implies they have to be declared with
[initialization/1](@ref initialization) to recreate them after loading the saved
state. Each thread has its own set of global variables, starting with
an empty set. Using `thread_initialization/1` to define a global
variable it will be defined, restored after reloading a saved state
and created in all threads that are created after the
registration. Finally, global variables can be initialised using the
exception hook called [exception/3](@ref exception). The latter technique is used
by CHR.

<ul>
 <li>b_setval(+ _Name_, + _Value_)  @anchor b_setval


Associate the term  _Value_ with the atom  _Name_ or replaces
the currently associated value with  _Value_. If  _Name_ does
not refer to an existing global variable a variable with initial value
[] is created (the empty list). On backtracking the assignment is
reversed. 

</li>
 <li>b_getval(+ _Name_, - _Value_)  @anchor b_getval


Get the value associated with the global variable  _Name_ and unify
it with  _Value_. Note that this unification may further
instantiate the value of the global variable. If this is undesirable
the normal precautions (double negation or [copy_term/2](@ref copy_term)) must be
taken. The [b_getval/2](@ref b_getval) predicate generates errors if  _Name_ is not
an atom or the requested variable does not exist. 

Notice that for compatibility with other systems  _Name_ <em>must</em> be already associated with a term: otherwise the system will generate an error.

</li>
 <li>nb_setval(+ _Name_, + _Value_)  @anchor nb_setval


Associates a copy of  _Value_ created with [duplicate_term/2](@ref duplicate_term) with
the atom  _Name_. Note that this can be used to set an initial
value other than `[]` prior to backtrackable assignment.

</li>
 <li>nb_getval(+ _Name_, - _Value_)  @anchor nb_getval


The [nb_getval/2](@ref nb_getval) predicate is a synonym for [b_getval/2](@ref b_getval),
introduced for compatibility and symmetry. As most scenarios will use
a particular global variable either using non-backtrackable or
backtrackable assignment, using [nb_getval/2](@ref nb_getval) can be used to
document that the variable is used non-backtrackable.

</li>
 <li>nb_linkval(+ _Name_, + _Value_)  @anchor nb_linkval


Associates the term  _Value_ with the atom  _Name_ without
copying it. This is a fast special-purpose variation of [nb_setval/2](@ref nb_setval)
intended for expert users only because the semantics on backtracking
to a point before creating the link are poorly defined for compound
terms. The principal term is always left untouched, but backtracking
behaviour on arguments is undone if the original assignment was
trailed and left alone otherwise, which implies that the history that
created the term affects the behaviour on backtracking. Please
consider the following example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
demo_nb_linkval :-
        T = nice(N),
        (   N = world,
            nb_linkval(myvar, T),
            fail
        ;   nb_getval(myvar, V),
            writeln(V)
        ).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>nb_set_shared_val(+ _Name_, + _Value_)  @anchor nb_set_shared_val


Associates the term  _Value_ with the atom  _Name_, but sharing
non-backtrackable terms. This may be useful if you want to rewrite a
global variable so that the new copy will survive backtracking, but
you want to share structure with the previous term.

The next example shows the differences between the three built-ins:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- nb_setval(a,a(_)),nb_getval(a,A),nb_setval(b,t(C,A)),nb_getval(b,B).
A = a(_A),
B = t(_B,a(_C)) ? 

?- nb_setval(a,a(_)),nb_getval(a,A),nb_set_shared_val(b,t(C,A)),nb_getval(b,B).

?- nb_setval(a,a(_)),nb_getval(a,A),nb_linkval(b,t(C,A)),nb_getval(b,B).
A = a(_A),
B = t(C,a(_A)) ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>nb_setarg(+{Arg], + _Term_, + _Value_) @anchor nb_setarg



Assigns the  _Arg_-th argument of the compound term  _Term_ with
the given  _Value_ as setarg/3, but on backtracking the assignment
is not reversed. If  _Term_ is not atomic, it is duplicated using
duplicate_term/2. This predicate uses the same technique as
[nb_setval/2](@ref nb_setval). We therefore refer to the description of
[nb_setval/2](@ref nb_setval) for details on non-backtrackable assignment of
terms. This predicate is compatible to GNU-Prolog
`setarg(A,T,V,false)`, removing the type-restriction on
 _Value_. See also [nb_linkarg/3](@ref nb_linkarg). Below is an example for
counting the number of solutions of a goal. Note that this
implementation is thread-safe, reentrant and capable of handling
exceptions. Realising these features with a traditional implementation
based on assert/retract or flag/3 is much more complicated.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    succeeds_n_times(Goal, Times) :-
            Counter = counter(0),
            (   Goal,
                arg(1, Counter, N0),
                N is N0 + 1,
                nb_setarg(1, Counter, N),
                fail
            ;   arg(1, Counter, Times)
            ).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>nb_set_shared_arg(+ _Arg_, + _Term_, + _Value_)  @anchor nb_set_shared_arg



As [nb_setarg/3](@ref nb_setarg), but like [nb_linkval/2](@ref nb_linkval) it does not
duplicate the global sub-terms in  _Value_. Use with extreme care
and consult the documentation of [nb_linkval/2](@ref nb_linkval) before use.

</li>
 <li>nb_linkarg(+ _Arg_, + _Term_, + _Value_)  @anchor nb_linkarg



As [nb_setarg/3](@ref nb_setarg), but like [nb_linkval/2](@ref nb_linkval) it does not
duplicate  _Value_. Use with extreme care and consult the
documentation of [nb_linkval/2](@ref nb_linkval) before use.

</li>
 <li>nb_current(? _Name_, ? _Value_)  @anchor nb_current


Enumerate all defined variables with their value. The order of
enumeration is undefined. 

</li>
 <li>nb_delete(+ _Name_)  @anchor nb_delete


Delete the named global variable. 
</li>
</ul>

Global variables have been introduced by various Prolog
implementations recently. We follow the implementation of them in
SWI-Prolog, itself based on hProlog by Bart Demoen.

GNU-Prolog provides a rich set of global variables, including
arrays. Arrays can be implemented easily in YAP and SWI-Prolog using
[functor/3](@ref functor) and `setarg/3` due to the unrestricted arity of
compound terms.

@section Profiling Profiling Prolog Programs

YAP includes two profilers. The count profiler keeps information on the
number of times a predicate was called. This information can be used to
detect what are the most commonly called predicates in the program.  The
count profiler can be compiled by setting YAP's flag [profiling](@ref profiling)
to `on`. The time-profiler is a `gprof` profiler, and counts
how many ticks are being spent on specific predicates, or on other
system functions such as internal data-base accesses or garbage collects.

The YAP profiling sub-system is currently under
development. Functionality for this sub-system will increase with newer
implementation.

@section The_Count_Profiler The Count Profiler

 *Notes:*

The count profiler works by incrementing counters at procedure entry or
backtracking. It provides exact information:

<ul>
 <li>Profiling works for both static and dynamic predicates.
</li>
 <li>Currently only information on entries and retries to a predicate
are maintained. This may change in the future.
</li>
 <li>As an example, the following user-level program gives a list of
the most often called procedures in a program. The procedure
`list_profile` shows all procedures, irrespective of module, and
the procedure `list_profile/1` shows the procedures being used in
a specific module.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list_profile :-
        % get number of calls for each profiled procedure
        setof(D-[M:P|D1],(current_module(M),profile_data(M:P,calls,D),profile_data(M:P,retries,D1)),LP),
        % output so that the most often called
        % predicates will come last:
        write_profile_data(LP).

list_profile(Module) :-
        % get number of calls for each profiled procedure
        setof(D-[Module:P|D1],(profile_data(Module:P,calls,D),profile_data(Module:P,retries,D1)),LP),
        % output so that the most often called
        % predicates will come last:
        write_profile_data(LP).

write_profile_data([]).
write_profile_data([D-[M:P|R]|SLP]) :-
        % swap the two calls if you want the most often
        %  called predicates first.
        format('~a:~w: ~32+~t~d~12+~t~d~12+~n', [M,P,D,R]),
        write_profile_data(SLP).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
</li>
</ul>

These are  the current predicates to access and clear profiling data:

<ul>
 <li>profile_data(? _Na/Ar_, ? _Parameter_, - _Data_) @anchor profile_data


Give current profile data on  _Parameter_ for a predicate described
by the predicate indicator  _Na/Ar_. If any of  _Na/Ar_ or
 _Parameter_ are unbound, backtrack through all profiled predicates
or stored parameters. Current parameters are:

<ul>
 <li>calls
Number of times a procedure was called.

</li>
 <li>retries
Number of times a call to the procedure was backtracked to and retried.
</li>
</ul>

</li>
 <li>profile_reset @anchor profiled_reset


Reset all profiling information.

</li>
</ul>

@section Tick_Profiler Tick Profiler

The tick profiler works by interrupting the Prolog code every so often
and checking at each point the code was. The profiler must be able to
retrace the state of the abstract machine at every moment. The major
advantage of this approach is that it gives the actual amount of time
being spent per procedure, or whether garbage collection dominates
execution time. The major drawback is that tracking down the state of
the abstract machine may take significant time, and in the worst case
may slow down the whole execution.

The following procedures are available:

<ul>
 <li>profinit @anchor profinit


Initialise the data-structures for the profiler. Unnecessary for
dynamic profiler.

</li>
 <li>profon @anchor profon


Start profiling.

</li>
 <li>profoff @anchor profoff


Stop profiling.

</li>
 <li>showprofres @anchor showprofres


Show profiling info.

</li>
 <li>showprofres( _N_)

Show profiling info for the top-most  _N_ predicates.

</li>
</ul>

The [showprofres/0](@ref showprofres) and `showprofres/1` predicates call a user-defined multifile hook predicate, `user:prolog_predicate_name/2`, that can be used for converting a possibly explicitly-qualified callable term into an atom that will used when printing the profiling information.

@section Call_Counting Counting Calls

Predicates compiled with YAP's flag [call_counting](@ref call_counting) set to
`on` update counters on the numbers of calls and of
retries. Counters are actually decreasing counters, so that they can be
used as timers.  Three counters are available:

<ul>
 <li>`calls`: number of predicate calls since execution started or since
system was reset; 
</li>
 <li>`retries`: number of retries for predicates called since
execution started or since counters were reset;
</li>
 <li>`calls_and_retries`: count both on predicate calls and
retries.
</li>
</ul>
These counters can be used to find out how many calls a certain
goal takes to execute. They can also be used as timers.

The code for the call counters piggybacks on the profiling
code. Therefore, activating the call counters also activates the profiling
counters.

These are  the predicates that access and manipulate the call counters:

<ul>
 <li>call_count_data(- _Calls_, - _Retries_, - _CallsAndRetries_) @anchor call_count_data


Give current call count data. The first argument gives the current value
for the  _Calls_ counter, next the  _Retries_ counter, and last
the  _CallsAndRetries_ counter.

</li>
 <li>call_count_reset @anchor call_count_reset


Reset call count counters. All timers are also reset.

</li>
 <li>call_count(? _CallsMax_, ? _RetriesMax_, ? _CallsAndRetriesMax_) @anchor call_count


Set call count counter as timers. YAP will generate an exception
if one of the instantiated call counters decreases to 0. YAP will ignore
unbound arguments:

<ul>
 <li>_CallsMax_: throw the exception `call_counter` when the
counter `calls` reaches 0;
</li>
 <li>_RetriesMax_: throw the exception `retry_counter` when the
counter `retries` reaches 0;
</li>
 <li>_CallsAndRetriesMax_: throw the exception
`call_and_retry_counter` when the counter `calls_and_retries`
reaches 0.
</li>
</ul>
</li>
</ul>

Next, we show a simple example of how to use call counters:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- yap_flag(call_counting,on), [-user]. l :- l. end_of_file. yap_flag(call_counting,off).

yes

yes
   ?- catch((call_count(10000,_,_),l),call_counter,format("limit_exceeded.~n",[])).

limit_exceeded.

yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that we first compile the looping predicate `l/0` with
[call_counting](@ref call_counting) `on`. Next, we [catch/3](@ref catch) to handle an
exception when `l/0` performs more than 10000 reductions.

@section Arrays Arrays

The YAP system includes experimental support for arrays. The
support is enabled with the option `YAP_ARRAYS`.

There are two very distinct forms of arrays in YAP. The
<em>dynamic arrays</em> are a different way to access compound terms
created during the execution. Like any other terms, any bindings to
these terms and eventually the terms themselves will be destroyed during
backtracking. Our goal in supporting dynamic arrays is twofold. First,
they provide an alternative to the standard [arg/3](@ref arg)
built-in. Second, because dynamic arrays may have name that are globally
visible, a dynamic array can be visible from any point in the
program. In more detail, the clause

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
g(X) :- array_element(a,2,X).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
will succeed as long as the programmer has used the built-in <tt>array/2</tt>
to create an array term with at least 3 elements in the current
environment, and the array was associated with the name `a`.  The
element `X` is a Prolog term, so one can bind it and any such
bindings will be undone when backtracking. Note that dynamic arrays do
not have a type: each element may be any Prolog term.

The <em>static arrays</em> are an extension of the database. They provide
a compact way for manipulating data-structures formed by characters,
integers, or floats imperatively. They can also be used to provide
two-way communication between YAP and external programs through
shared memory.

In order to efficiently manage space elements in a static array must
have a type. Currently, elements of static arrays in YAP should
have one of the following predefined types:

<ul>
 <li>`byte`: an 8-bit signed character.
</li>
 <li>`unsigned_byte`: an 8-bit unsigned character.
</li>
 <li>`int`: Prolog integers. Size would be the natural size for
the machine's architecture.
</li>
 <li>`float`: Prolog floating point number. Size would be equivalent
to a double in `C`.
</li>
 <li>`atom`: a Prolog atom.
</li>
 <li>`dbref`: an internal database reference.
</li>
 <li>`term`: a generic Prolog term. Note that this will term will
not be stored in the array itself, but instead will be stored in the
Prolog internal database.
</li>
</ul>

Arrays may be <em>named</em> or <em>anonymous</em>. Most arrays will be
<em>named</em>, that is associated with an atom that will be used to find
the array. Anonymous arrays do not have a name, and they are only of
interest if the `TERM_EXTENSIONS` compilation flag is enabled. In
this case, the unification and parser are extended to replace
occurrences of Prolog terms of the form `X[I]` by run-time calls to
[array_element/3](@ref array_element), so that one can use array references instead of
extra calls to [arg/3](@ref arg). As an example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
g(X,Y,Z,I,J) :- X[I] is Y[J]+Z[I].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
should give the same results as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
G(X,Y,Z,I,J) :-
        array_element(X,I,E1),
        array_element(Y,J,E2),  
        array_element(Z,I,E3),  
        E1 is E2+E3.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the only limitation on array size are the stack size for
dynamic arrays; and, the heap size for static (not memory mapped)
arrays. Memory mapped arrays are limited by available space in the file
system and in the virtual memory space.

The following predicates manipulate arrays:

<ul>

 <li>array(+ _Name_, + _Size_) @anchor array


Creates a new dynamic array. The  _Size_ must evaluate to an
integer. The  _Name_ may be either an atom (named array) or an
unbound variable (anonymous array).

Dynamic arrays work as standard compound terms, hence space for the
array is recovered automatically on backtracking.

</li>
 <li>static_array(+ _Name_, + _Size_, + _Type_) @anchor static_array


Create a new static array with name  _Name_. Note that the  _Name_
must be an atom (named array). The  _Size_ must evaluate to an
integer.  The  _Type_ must be bound to one of types mentioned
previously.

</li>
 <li>reset_static_array(+ _Name_) @anchor reset_static_array


Reset static array with name  _Name_ to its initial value.

</li>
 <li>static_array_location(+ _Name_, - _Ptr_) @anchor static_array_location


Give the location for  a static array with name
 _Name_.

</li>
 <li>static_array_properties(? _Name_, ? _Size_, ? _Type_) @anchor static_array_properties


Show the properties size and type of a static array with name
 _Name_. Can also be used to enumerate all current
static arrays. 

This built-in will silently fail if the there is no static array with
that name.

</li>
 <li>static_array_to_term(? _Name_, ? _Term_) @anchor static_array_to_term


Convert a static array with name
 _Name_ to a compound term of name  _Name_.

This built-in will silently fail if the there is no static array with
that name.

</li>
 <li>mmapped_array(+ _Name_, + _Size_, + _Type_, + _File_) @anchor mmapped_array


Similar to [static_array/3](@ref static_array), but the array is memory mapped to file
 _File_. This means that the array is initialized from the file, and
that any changes to the array will also be stored in the file. 

This built-in is only available in operating systems that support the
system call `mmap`. Moreover, mmapped arrays do not store generic
terms (type `term`).

</li>
 <li>close_static_array(+ _Name_) @anchor close_static_array


Close an existing static array of name  _Name_. The  _Name_ must
be an atom (named array). Space for the array will be recovered and
further accesses to the array will return an error. 

</li>
 <li>resize_static_array(+ _Name_, - _OldSize_, + _NewSize_) @anchor resize_static_array


Expand or reduce a static array, The  _Size_ must evaluate to an
integer. The  _Name_ must be an atom (named array). The  _Type_
must be bound to one of `int`, `dbref`, `float` or
`atom`.

Note that if the array is a mmapped array the size of the mmapped file
will be actually adjusted to correspond to the size of the array.

</li>
 <li>array_element(+ _Name_, + _Index_, ? _Element_) @anchor array_element


Unify  _Element_ with  _Name_[ _Index_]. It works for both
static and dynamic arrays, but it is read-only for static arrays, while
it can be used to unify with an element of a dynamic array.

</li>
 <li>update_array(+ _Name_, + _Index_, ? _Value_)  @anchor update_array


Attribute value  _Value_ to  _Name_[ _Index_]. Type
restrictions must be respected for static arrays. This operation is
available for dynamic arrays if `MULTI_ASSIGNMENT_VARIABLES` is
enabled (true by default). Backtracking undoes  _update_array/3_ for
dynamic arrays, but not for static arrays.

Note that [update_array/3](@ref update_array) actually uses `setarg/3` to update
elements of dynamic arrays, and `setarg/3` spends an extra cell for
every update. For intensive operations we suggest it may be less
expensive to unify each element of the array with a mutable terms and
to use the operations on mutable terms.

</li>
 <li>add_to_array_element(+ _Name_, + _Index_, , + _Number_, ? _NewValue_)  @anchor add_to_array_element


Add  _Number_  _Name_[ _Index_] and unify  _NewValue_ with
the incremented value. Observe that  _Name_[ _Index_] must be an
number. If  _Name_ is a static array the type of the array must be
`int` or `float`. If the type of the array is `int` you
only may add integers, if it is `float` you may add integers or
floats. If  _Name_ corresponds to a dynamic array the array element
must have been previously bound to a number and `Number` can be
any kind of number.

The `add_to_array_element/3` built-in actually uses
`setarg/3` to update elements of dynamic arrays. For intensive
operations we suggest it may be less expensive to unify each element
of the array with a mutable terms and to use the operations on mutable
terms.

</li>
</ul>

@section Preds Predicate Information

Built-ins that return information on the current predicates and modules:

<ul>

 <li>current_module( _M_) @anchor current_module


Succeeds if  _M_ are defined modules. A module is defined as soon as some
predicate defined in the module is loaded, as soon as a goal in the
module is called, or as soon as it becomes the current type-in module.

</li>
 <li>current_module( _M_, _F_)

Succeeds if  _M_ are current modules associated to the file  _F_.

</li>
</ul>

@section Misc Miscellaneous

<ul>

 <li>statistics/0 @anchor statistics


Send to the current user error stream general information on space used and time
spent by the system.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- statistics.
memory (total)        4784124 bytes
   program space      3055616 bytes:    1392224 in use,      1663392 free
                                                             2228132  max
   stack space        1531904 bytes:        464 in use,      1531440 free
     global stack:                           96 in use,       616684  max
      local stack:                          368 in use,       546208  max
   trail stack         196604 bytes:          8 in use,       196596 free

       0.010 sec. for 5 code, 2 stack, and 1 trail space overflows
       0.130 sec. for 3 garbage collections which collected 421000 bytes
       0.000 sec. for 0 atom garbage collections which collected 0 bytes
       0.880 sec. runtime
       1.020 sec. cputime
      25.055 sec. elapsed time

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The example shows how much memory the system spends. Memory is divided
into Program Space, Stack Space and Trail. In the example we have 3MB
allocated for program spaces, with less than half being actually
used. YAP also shows the maximum amount of heap space having been used
which was over 2MB.

The stack space is divided into two stacks which grow against each
other. We are in the top level so very little stack is being used. On
the other hand, the system did use a lot of global and local stack
during the previous execution (we refer the reader to a WAM tutorial in
order to understand what are the global and local stacks).

YAP also shows information on how many memory overflows and garbage
collections the system executed, and statistics on total execution
time. Cputime includes all running time, runtime excludes garbage
collection and stack overflow time.

</li>
 <li>statistics(? _Param_,- _Info_)

Gives statistical information on the system parameter given by first
argument:

<ul>

 <li>atoms @anchor atoms

`[ _NumberOfAtoms_, _SpaceUsedBy Atoms_]`


This gives the total number of atoms `NumberOfAtoms` and how much
space they require in bytes,  _SpaceUsedBy Atoms_.

</li>
 <li>cputime @anchor cputime

`[ _Time since Boot_, _Time From Last Call to Cputime_]`


This gives the total cputime in milliseconds spent executing Prolog code,
garbage collection and stack shifts time included.

</li>
 <li>dynamic_code @anchor dynamic_code

`[ _Clause Size_, _Index Size_, _Tree Index Size_, _Choice Point Instructions Size_, _Expansion Nodes Size_, _Index Switch Size_]`


Size of static code in YAP in bytes:  _Clause Size_, the number of
bytes allocated for clauses, plus
 _Index Size_, the number of bytes spent in the indexing code. The
indexing code is divided into main tree,  _Tree Index Size_, 
tables that implement choice-point manipulation,  _Choice xsPoint Instructions Size_, tables that cache clauses for future expansion of the index
tree,  _Expansion Nodes Size_, and 
tables such as hash tables that select according to value,   _Index Switch Size_.

</li>
 <li>garbage_collection @anchor garbage_collection

`[ _Number of GCs_, _Total Global Recovered_, _Total Time Spent_]`


Number of garbage collections, amount of space recovered in kbytes, and
total time spent doing garbage collection in milliseconds. More detailed
information is available using `yap_flag(gc_trace,verbose)`.

</li>
 <li>global_stack @anchor global_stack

`[ _Global Stack Used_, _Execution Stack Free_]`


Space in kbytes currently used in the global stack, and space available for
expansion by the local and global stacks.

</li>
 <li>local_stack @anchor local_stack

`[ _Local Stack Used_, _Execution Stack Free_]`


Space in kbytes currently used in the local stack, and space available for
expansion by the local and global stacks.

</li>
 <li>heap @anchor heap

`[ _Heap Used_, _Heap Free_]`


Total space in kbytes not recoverable
in backtracking. It includes the program code, internal data base, and,
atom symbol table.

</li>
 <li>program @anchor program

`[ _Program Space Used_, _Program Space Free_]`


Equivalent to [heap](@ref heap).

</li>
 <li>runtime @anchor runtime

`[ _Time since Boot_, _Time From Last Call to Runtime_]`


This gives the total cputime in milliseconds spent executing Prolog
code, not including garbage collections and stack shifts. Note that
until YAP4.1.2 the [runtime](@ref runtime) statistics would return time spent on
garbage collection and stack shifting.

</li>
 <li>stack_shifts @anchor stack_shifts

`[ _Number of Heap Shifts_, _Number of Stack Shifts_, _Number of Trail Shifts_]`


Number of times YAP had to
expand the heap, the stacks, or the trail. More detailed information is
available using `yap_flag(gc_trace,verbose)`.

</li>
 <li>static_code @anchor static_code

`[ _Clause Size_, _Index Size_, _Tree Index Size_, _Expansion Nodes Size_, _Index Switch Size_]`


Size of static code in YAP in bytes:  _Clause Size_, the number of
bytes allocated for clauses, plus
 _Index Size_, the number of bytes spent in the indexing code. The
indexing code is divided into a main tree,  _Tree Index Size_, table that cache clauses for future expansion of the index
tree,  _Expansion Nodes Size_, and and 
tables such as hash tables that select according to value,   _Index Switch Size_.

</li>
 <li>trail @anchor trail

`[ _Trail Used_, _Trail Free_]`


Space in kbytes currently being used and still available for the trail.

</li>
 <li>walltime @anchor walltime

`[ _Time since Boot_, _Time From Last Call to Walltime_]`


This gives the clock time in milliseconds since starting Prolog.

</li>
</ul>

</li>
 <li>time(: _Goal_) @anchor time


Prints the CPU time and the wall time for the execution of  _Goal_.
Possible choice-points of  _Goal_ are removed. Based on the SWI-Prolog 
definition (minus reporting the number of inferences, which YAP currently
does not support).

</li>
 <li>yap_flag(? _Param_,? _Value_) @anchor yap_flag


Set or read system properties for  _Param_:

<ul>

 <li>argv @anchor argv


Read-only flag. It unifies with a list of atoms that gives the
arguments to YAP after `--`.

</li>
 <li>agc_margin @anchor agc_margin

An integer: if this amount of atoms has been created since the last
atom-garbage collection, perform atom garbage collection at the first
opportunity. Initial value is 10,000. May be changed. A value of 0
(zero) disables atom garbage collection.

</li>
 <li>associate @anchor associate



Read-write flag telling a suffix for files associated to Prolog
sources. It is `yap` by default.

</li>
 <li>bounded [ISO] @anchor bounded



Read-only flag telling whether integers are bounded. The value depends
on whether YAP uses the GMP library or not.

</li>
 <li>profiling @anchor call_counting



If `off` (default) do not compile call counting information for
procedures. If `on` compile predicates so that they calls and
retries to the predicate may be counted. Profiling data can be read through the
[call_count_data/3](@ref call_count_data) built-in.

</li>
 <li>char_conversion [ISO]


Writable flag telling whether a character conversion table is used when
reading terms. The default value for this flag is `off` except in
`sicstus` and `iso` language modes, where it is `on`.

</li>
 <li>character_escapes [ISO] @anchor character_escapes


Writable flag telling whether a character escapes are enables,
`true`, or disabled, `false`. The default value for this flag is
`on`.

</li>
 <li>debug [ISO] @anchor debug



If  _Value_ is unbound, tell whether debugging is `true` or
`false`. If  _Value_ is bound to `true` enable debugging, and if
it is bound to `false` disable debugging.

</li>
 <li>debugger_print_options @anchor debugger_print_options



If bound, set the argument to the `write_term/3` options the
debugger uses to write terms. If unbound, show the current options.

</li>
 <li>dialect @anchor dialect



Read-only flag that always returns `yap`.

</li>
 <li>discontiguous_warnings @anchor discontiguous_warnings



If  _Value_ is unbound, tell whether warnings for discontiguous
predicates are `on` or
`off`. If  _Value_ is bound to `on` enable these warnings,
and if it is bound to `off` disable them. The default for YAP is
`off`, unless we are in `sicstus` or `iso` mode.

</li>
 <li>dollar_as_lower_case @anchor dollar_as_lower_case



If `off` (default)  consider the character '$' a control character, if
`on` consider '$' a lower case character.

</li>
 <li>double_quotes [ISO] @anchor double_quotes



If  _Value_ is unbound, tell whether a double quoted list of characters
token is converted to a list of atoms, `chars`, to a list of integers,
`codes`, or to a single atom, `atom`. If  _Value_ is bound, set to
the corresponding behavior. The default value is `codes`.

</li>
 <li>executable @anchor executable


Read-only flag. It unifies with an atom that gives the
original program path.

</li>
 <li>fast @anchor fast



If `on` allow fast machine code, if `off` (default) disable it. Only
available in experimental implementations.

</li>
 <li>fileerrors


If `on` `fileerrors` is `on`, if `off` (default)
`fileerrors` is disabled.

</li>
 <li>float_format @anchor float_format


C-library `printf()` format specification used by [write/1](@ref write) and
friends to determine how floating point numbers are printed. The
default is `%.15g`. The specified value is passed to `printf()`
without further checking. For example, if you want less digits
printed, `%g` will print all floats using 6 digits instead of the
default 15.

</li>
 <li>gc


If `on` allow garbage collection (default), if `off` disable it.

</li>
 <li>gc_margin @anchor gc_margin



Set or show the minimum free stack before starting garbage
collection. The default depends on total stack size. 

</li>
 <li>gc_trace @anchor gc_trace


If `off` (default) do not show information on garbage collection
and stack shifts, if `on` inform when a garbage collection or stack
shift happened, if [verbose](@ref verbose) give detailed information on garbage
collection and stack shifts. Last, if `very_verbose` give detailed
information on data-structures found during the garbage collection
process, namely, on choice-points.

</li>
 <li>generate_debugging_info @anchor generate_debugging_info


If `true` (default) generate debugging information for
procedures, including source mode. If `false` predicates no
information is generated, although debugging is still possible, and
source mode is disabled.

</li>
 <li>host_type @anchor host_type


Return `configure` system information, including the machine-id
for which YAP was compiled and Operating System information. 

</li>
 <li>index @anchor index_yap_flag


If `on` allow indexing (default), if `off` disable it, if
`single` allow on first argument only.

</li>
 <li>index_sub_term_search_depth @anchor index_sub_term_yap_flag



Maximum bound on searching sub-terms for indexing, if `0` (default) no bound.

</li>
 <li>informational_messages @anchor informational_messages



If `on` allow printing of informational messages, such as the ones
that are printed when consulting. If `off` disable printing
these messages. It is `on` by default except if YAP is booted with
the `-L` flag.

</li>
 <li>integer_rounding_function [ISO] @anchor integer_rounding_function



Read-only flag telling the rounding function used for integers. Takes the value
`toward_zero` for the current version of YAP.

</li>
 <li>language @anchor language



Choose whether YAP is closer to C-Prolog, `cprolog`, iso-prolog,
`iso` or SICStus Prolog, `sicstus`. The current default is
`cprolog`. This flag affects update semantics, leashing mode,
style checking, handling calls to undefined procedures, how directives
are interpreted, when to use dynamic, character escapes, and how files
are consulted.

</li>
 <li>max_arity [ISO] @anchor max_arity



Read-only flag telling the maximum arity of a functor. Takes the value
`unbounded` for the current version of YAP.

</li>
 <li>max_integer [ISO] @anchor max_integer



Read-only flag telling the maximum integer in the
implementation. Depends on machine and Operating System
architecture, and on whether YAP uses the `GMP` multi-precision
library. If [bounded](@ref bounded) is false, requests for [max_integer](@ref max_integer)
will fail.

</li>
 <li>max_tagged_integer  @anchor max_tagged_integer



Read-only flag telling the maximum integer we can store as a single
word. Depends on machine and Operating System
architecture. It can be used to find the word size of the current machine.

</li>
 <li>min_integer [ISO] @anchor min_integer


Read-only flag telling the minimum integer in the
implementation. Depends on machine and Operating System architecture,
and on whether YAP uses the `GMP` multi-precision library. If
[bounded](@ref bounded) is false, requests for [min_integer](@ref min_integer) will fail.

</li>
 <li>min_tagged_integer  @anchor min_tagged_integer



Read-only flag telling the minimum integer we can store as a single
word. Depends on machine and Operating System
architecture.

</li>
 <li>n_of_integer_keys_in_bb @anchor n_of_integer_keys_in_bb



Read or set the size of the hash table that is used for looking up the
blackboard when the key is an integer.

</li>
 <li>occurs_check @anchor occurs_check



Current read-only and set to `false`.

</li>
 <li>n_of_integer_keys_in_db @anchor n_of_integer_keys_in_db



Read or set the size of the hash table that is used for looking up the
internal data-base when the key is an integer.

</li>
 <li>open_expands_filename @anchor open_expands_filename



If `true` the [open/3](@ref open) builtin performs filename-expansion
before opening a file (SICStus Prolog like). If `false` it does not
(SWI-Prolog like).

</li>
 <li>open_shared_object @anchor open_shared_object



If true, `open_shared_object/2` and friends are implemented,
providing access to shared libraries (`.so` files) or to dynamic link
libraries (`.DLL` files).

</li>
 <li>profiling @anchor profiling



If `off` (default) do not compile profiling information for
procedures. If `on` compile predicates so that they will output
profiling information. Profiling data can be read through the
[profile_data/3](@ref profile_data) built-in.

</li>
 <li>prompt_alternatives_on(atom, changeable) @anchor prompt_alternatives_on

SWI-Compatible option, determines prompting for alternatives in the Prolog toplevel. Default is <tt>groundness</tt>, YAP prompts for alternatives if and only if the query contains variables. The alternative, default in SWI-Prolog is <tt>determinism</tt> which implies the system prompts for alternatives if the goal succeeded while leaving choicepoints.

</li>
 <li>redefine_warnings @anchor redefine_warnings



If  _Value_ is unbound, tell whether warnings for procedures defined
in several different files are `on` or
`off`. If  _Value_ is bound to `on` enable these warnings,
and if it is bound to `off` disable them. The default for YAP is
`off`, unless we are in `sicstus` or `iso` mode.

</li>
 <li>shared_object_search_path @anchor shared_object_search_path

Name of the environment variable used by the system to search for shared
objects.

</li>
 <li>shared_object_extension @anchor shared_object_extension

Suffix associated with loadable code.

</li>
 <li>single_var_warnings @anchor single_var_warnings



If  _Value_ is unbound, tell whether warnings for singleton variables
are `on` or `off`. If  _Value_ is bound to `on` enable
these warnings, and if it is bound to `off` disable them. The
default for YAP is `off`, unless we are in `sicstus` or
`iso` mode.

</li>
 <li>strict_iso @anchor strict_iso



If  _Value_ is unbound, tell whether strict ISO compatibility mode
is `on` or `off`. If  _Value_ is bound to `on` set
language mode to `iso` and enable strict mode. If  _Value_ is
bound to `off` disable strict mode, and keep the current language
mode. The default for YAP is `off`.

Under strict ISO Prolog mode all calls to non-ISO built-ins generate an
error. Compilation of clauses that would call non-ISO built-ins will
also generate errors. Pre-processing for grammar rules is also
disabled. Module expansion is still performed.

Arguably, ISO Prolog does not provide all the functionality required
from a modern Prolog system. Moreover, because most Prolog
implementations do not fully implement the standard and because the
standard itself gives the implementor latitude in a few important
questions, such as the unification algorithm and maximum size for
numbers there is no guarantee that programs compliant with this mode
will work the same way in every Prolog and in every platform. We thus
believe this mode is mostly useful when investigating how a program
depends on a Prolog's platform specific features.

</li>
 <li>stack_dump_on_error @anchor stack_dump_on_error



If `on` show a stack dump when YAP finds an error. The default is
`off`.

</li>
 <li>syntax_errors


Control action to be taken after syntax errors while executing [read/1](@ref read),
`read/2`, or `read_term/3`:

<ul>

 <li>dec10


Report the syntax error and retry reading the term.

</li>
 <li>fail


Report the syntax error and fail (default).

</li>
 <li>error


Report the syntax error and generate an error.

</li>
 <li>quiet


Just fail
</li>
</ul>

</li>
 <li>system_options @anchor system_options


This read only flag tells which options were used to compile
YAP. Currently it informs whether the system supports `big_numbers`,
`coroutining`, `depth_limit`, `low_level_tracer`,
`or-parallelism`, `rational_trees`, `readline`, `tabling`,
`threads`, or the `wam_profiler`.

</li>
 <li>tabling_mode

Sets or reads the tabling mode for all tabled predicates. Please
 (see [Tabling](@ref Tabling)) for the list of options.

</li>
 <li>to_chars_mode @anchor to_chars_modes


Define whether YAP should follow `quintus`-like
semantics for the `atom_chars/1` or `number_chars/1` built-in,
or whether it should follow the ISO standard (`iso` option).

</li>
 <li>toplevel_hook @anchor toplevel_hook



+If bound, set the argument to a goal to be executed before entering the
top-level. If unbound show the current goal or `true` if none is
presented. Only the first solution is considered and the goal is not
backtracked into.

</li>
 <li>toplevel_print_options @anchor toplevel_print_options



+If bound, set the argument to the `write_term/3` options used to write
terms from the top-level. If unbound, show the current options.

</li>
 <li>typein_module @anchor typein_module



If bound, set the current working or type-in module to the argument,
which must be an atom. If unbound, unify the argument with the current
working module.

</li>
 <li>unix

Read-only Boolean flag that unifies with `true` if YAP is
running on an Unix system.  Defined if the C-compiler used to compile
this version of YAP either defines `__unix__` or `unix`.

</li>
 <li>unknown [ISO]


Corresponds to calling the [unknown/2](@ref unknown) built-in. Possible values 
are `error`, `fail`, and `warning`.

</li>
 <li>update_semantics @anchor update_semantics



Define whether YAP should follow `immediate` update
semantics, as in C-Prolog (default), `logical` update semantics,
as in Quintus Prolog, SICStus Prolog, or in the ISO standard. There is
also an intermediate mode, `logical_assert`, where dynamic
procedures follow logical semantics but the internal data base still
follows immediate semantics.

</li>
 <li>user_error @anchor user_error



If the second argument is bound to a stream, set [user_error](@ref user_error) to
this stream. If the second argument is unbound, unify the argument with
the current [user_error](@ref user_error) stream.

By default, the [user_error](@ref user_error) stream is set to a stream
corresponding to the Unix `stderr` stream.

The next example shows how to use this flag:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- open( '/dev/null', append, Error,
           [alias(mauri_tripa)] ).

Error = '$stream'(3) ? ;

no
   ?- set_prolog_flag(user_error, mauri_tripa).

close(mauri_tripa).

yes
   ?- 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We execute three commands. First, we open a stream in write mode and
give it an alias, in this case `mauri_tripa`. Next, we set
[user_error](@ref user_error) to the stream via the alias. Note that after we did so
prompts from the system were redirected to the stream
`mauri_tripa`. Last, we close the stream. At this point, YAP
automatically redirects the [user_error](@ref user_error) alias to the original
`stderr`.

</li>
 <li>user_flags @anchor user_flags



Define the behaviour of [set_prolog_flag/2](@ref set_prolog_flag) if the flag is not known. Values are `silent`, `warning` and `error`. The first two create the flag on-the-fly, with `warning` printing a message. The value `error` is consistent with ISO: it raises an existence error and does not create the flag. See also `create_prolog_flag/3`. The default is`error`, and developers are encouraged to use `create_prolog_flag/3` to create flags for their library.

</li>
 <li>user_input @anchor user_input



If the second argument is bound to a stream, set [user_input](@ref user_input) to
this stream. If the second argument is unbound, unify the argument with
the current [user_input](@ref user_input) stream.

By default, the [user_input](@ref user_input) stream is set to a stream
corresponding to the Unix `stdin` stream.

</li>
 <li>user_output @anchor user_output



If the second argument is bound to a stream, set [user_output](@ref user_output) to
this stream. If the second argument is unbound, unify the argument with
the current [user_output](@ref user_output) stream.

By default, the [user_output](@ref user_output) stream is set to a stream
corresponding to the Unix `stdout` stream.

</li>
 <li>verbose @anchor verbose



If `normal` allow printing of informational and banner messages,
such as the ones that are printed when consulting. If `silent`
disable printing these messages. It is `normal` by default except if
YAP is booted with the `-q` or `-L` flag.

</li>
 <li>verbose_load @anchor verbose_load


If `true` allow printing of informational messages when
consulting files. If `false` disable printing these messages. It
is `normal` by default except if YAP is booted with the `-L`
flag.

</li>
 <li>version @anchor version


Read-only flag that returns an atom with the current version of
YAP.

</li>
 <li>version_data @anchor version_data


Read-only flag that reads a term of the form
`yap`( _Major_, _Minor_, _Patch_, _Undefined_), where
 _Major_ is the major version,  _Minor_ is the minor version,
and  _Patch_ is the patch number.

</li>
 <li>windows @anchor windoes



Read-only boolean flag that unifies with tr `true` if YAP is
running on an Windows machine.

</li>
 <li>write_strings @anchor write_strings


Writable flag telling whether the system should write lists of
integers that are writable character codes using the list notation. It
is `on` if enables or `off` if disabled. The default value for
this flag is `off`.

</li>
 <li>max_workers @anchor max_workers


Read-only flag telling the maximum number of parallel processes.

</li>
 <li>max_threads @anchor max_threads


Read-only flag telling the maximum number of Prolog threads that can 
be created.

</li>
</ul>

</li>
 <li>current_prolog_flag(? _Flag_,- _Value_) [ISO] @anchor current_prolog_flag



Obtain the value for a YAP Prolog flag. Equivalent to calling
[yap_flag/2](@ref yap_flag) with the second argument unbound, and unifying the
returned second argument with  _Value_.

</li>
 <li>prolog_flag(? _Flag_,- _OldValue_,+ _NewValue_) @anchor prolog_flag



Obtain the value for a YAP Prolog flag and then set it to a new
value. Equivalent to first calling [current_prolog_flag/2](@ref current_prolog_flag) with the
second argument  _OldValue_ unbound and then calling
[set_prolog_flag/2](@ref set_prolog_flag) with the third argument  _NewValue_.

</li>
 <li>set_prolog_flag(+ _Flag_,+ _Value_) [ISO] @anchor set_prolog_flag



Set the value for YAP Prolog flag `Flag`. Equivalent to
calling [yap_flag/2](@ref yap_flag) with both arguments bound.

</li>
 <li>create_prolog_flag(+ _Flag_,+ _Value_,+ _Options_) @anchor create_prolog_flag



Create a new YAP Prolog flag.  _Options_ include `type(+Type)` and `access(+Access)` with  _Access_
one of `read_only` or `read_write` and  _Type_ one of `boolean`, `integer`, `float`, `atom`
and `term` (that is, no type).

</li>
 <li>op(+ _P_,+ _T_,+ _A_) [ISO] @anchor op


Defines the operator  _A_ or the list of operators  _A_ with type
 _T_ (which must be one of `xfx`, `xfy`,`yfx`,
`xf`, `yf`, `fx` or `fy`) and precedence  _P_
(see appendix iv for a list of predefined operators).

Note that if there is a preexisting operator with the same name and
type, this operator will be discarded. Also, `','` may not be defined
as an operator, and it is not allowed to have the same for an infix and
a postfix operator.

</li>
 <li>current_op( _P_, _T_, _F_) [ISO] @anchor current_op


Defines the relation:  _P_ is a currently defined  operator of type
 _T_ and precedence  _P_.

</li>
 <li>prompt(- _A_,+ _B_) @anchor prompt


Changes YAP input prompt from  _A_ to  _B_.

</li>
 <li>initialization

Execute the goals defined by initialization/1. Only the first answer is
considered.

</li>
 <li>prolog_initialization( _G_) @anchor prolog_initialization


Add a goal to be executed on system initialization. This is compatible
with SICStus Prolog's [initialization/1](@ref initialization).

</li>
 <li>version

Write YAP's boot message. 

</li>
 <li>version(- _Message_)

Add a message to be written when yap boots or after aborting. It is not
possible to remove messages.

</li>
 <li>prolog_load_context(? _Key_, ? _Value_) @anchor prolog_load_context


Obtain information on what is going on in the compilation process. The
following keys are available:

<ul>

 <li>directory @anchor directory_prolog_load_context



Full name for the directory where YAP is currently consulting the
file.

</li>
 <li>file @anchor file_prolog_load_context



Full name for the file currently being consulted. Notice that included
filed are ignored.

</li>
 <li>module @anchor module_prolog_load_context



Current source module.

</li>
 <li>source (prolog_load_context/2 option) @anchor source_prolog_load_context



Full name for the file currently being read in, which may be consulted,
reconsulted, or included.

</li>
 <li>stream @anchor stream_prolog_load_context



Stream currently being read in.

</li>
 <li>term_position @anchor term_position_prolog_load_context



Stream position at the stream currently being read in. For SWI
compatibility, it is a term of the form
`'$stream_position'(0,Line,0,0,0)`.
</li>
</ul>

</li>
 <li>source_location(? _FileName_, ? _Line_) @anchor source_location


SWI-compatible predicate. If the last term has been read from a physical file (i.e., not from the file user or a string), unify File with an absolute path to the file and Line with the line-number in the file. Please use [prolog_load_context/2](@ref prolog_load_context).

</li>
 <li>source_file(? _File_) @anchor source_file


SWI-compatible predicate. True if  _File_ is a loaded Prolog source file.

</li>
 <li>source_file(? _ModuleAndPred_,? _File_)

SWI-compatible predicate. True if the predicate specified by  _ModuleAndPred_ was loaded from file  _File_, where  _File_ is an absolute path name (see `absolute_file_name/2`).

</li>
</ul>

@page Library Library Predicates

Library files reside in the library_directory path (set by the
`LIBDIR` variable in the Makefile for YAP). Currently,
most files in the library are from the Edinburgh Prolog library. 

@section Aggregate Aggregate
This is the SWI-Prolog library based on  the Quintus and SICStus 4
library.   @c To be done - Analysing the aggregation template




This library provides aggregating operators over the solutions of a
predicate. The operations are a generalisation of the [bagof/3](@ref bagof),
[setof/3](@ref setof) and [findall/3](@ref findall) built-in predicates. The defined
aggregation operations are counting, computing the sum, minimum,
maximum, a bag of solutions and a set of solutions. We first give a
simple example, computing the country with the smallest area:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
smallest_country(Name, Area) :-
        aggregate(min(A, N), country(N, A), min(Area, Name)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are four aggregation predicates, distinguished on two properties.

<ul>

 <li>aggregate vs. aggregate_all
The aggregate predicates use setof/3 (aggregate/4) or bagof/3
(aggregate/3), dealing with existential qualified variables
( _Var_/\\ _Goal_) and providing multiple solutions for the
remaining free variables in  _Goal_. The aggregate_all/3
predicate uses findall/3, implicitly qualifying all free variables
and providing exactly one solution, while aggregate_all/4 uses
sort/2 over solutions and Distinguish (see below) generated using
findall/3. 
</li>
 <li>The  _Distinguish_ argument
The versions with 4 arguments provide a  _Distinguish_ argument
that allow for keeping duplicate bindings of a variable in the
result. For example, if we wish to compute the total population of
all countries we do not want to lose results because two countries
have the same population. Therefore we use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
        aggregate(sum(P), Name, country(Name, P), Total)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
</ul>

All aggregation predicates support the following operator below in
 _Template_. In addition, they allow for an arbitrary named compound
term where each of the arguments is a term from the list below. I.e. the
term `r(min(X), max(X))` computes both the minimum and maximum
binding for  _X_.

<ul>

 <li>count
Count number of solutions. Same as `sum(1)`. 
</li>
 <li>sum( _Expr_)
Sum of  _Expr_ for all solutions. 
</li>
 <li>min( _Expr_)
Minimum of  _Expr_ for all solutions. 
</li>
 <li>min( _Expr_,  _Witness_)
A term min( _Min_,  _Witness_), where  _Min_ is the minimal version of  _Expr_
over all Solution and  _Witness_ is any other template applied to
Solution that produced  _Min_. If multiple solutions provide the same
minimum,  _Witness_ corresponds to the first solution. 
</li>
 <li>max( _Expr_)
Maximum of  _Expr_ for all solutions. 
</li>
 <li>max( _Expr_,  _Witness_)
As min( _Expr_,  _Witness_), but producing the maximum result. 
</li>
 <li>set( _X_)
An ordered set with all solutions for  _X_. 
</li>
 <li>bag( _X_)
A list of all solutions for  _X_. 
</li>
</ul>

The predicates are:

<ul>

 <li>[nondet]aggregate(+ _Template_, : _Goal_, - _Result_) @anchor aggregate


Aggregate bindings in  _Goal_ according to  _Template_. The
aggregate/3 version performs bagof/3 on  _Goal_.
</li>
 <li>[nondet]aggregate(+ _Template_, + _Discriminator_, : _Goal_, - _Result_)

Aggregate bindings in  _Goal_ according to  _Template_. The
aggregate/3 version performs setof/3 on  _Goal_.
</li>
 <li>[semidet]aggregate_all(+ _Template_, : _Goal_, - _Result_) @anchor aggregate_all


Aggregate bindings in  _Goal_ according to  _Template_. The
aggregate_all/3 version performs findall/3 on  _Goal_.
</li>
 <li>[semidet]aggregate_all(+ _Template_, + _Discriminator_, : _Goal_, - _Result_)

Aggregate bindings in  _Goal_ according to  _Template_. The
aggregate_all/3 version performs findall/3 followed by sort/2 on
 _Goal_.
</li>
 <li>foreach(:Generator, : _Goal_) @anchor foreach


True if the conjunction of instances of  _Goal_ using the
bindings from Generator is true. Unlike forall/2, which runs a
failure-driven loop that proves  _Goal_ for each solution of
Generator, foreach creates a conjunction. Each member of the
conjunction is a copy of  _Goal_, where the variables it shares
with Generator are filled with the values from the corresponding
solution.

The implementation executes forall/2 if  _Goal_ does not contain
any variables that are not shared with Generator.

Here is an example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    ?- foreach(between(1,4,X), dif(X,Y)), Y = 5.
    Y = 5
    ?- foreach(between(1,4,X), dif(X,Y)), Y = 3.
    No
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Notice that  _Goal_ is copied repeatedly, which may cause
problems if attributed variables are involved.

</li>
 <li>[det]free_variables(:Generator, + _Template_, +VarList0, -VarList) @anchor free_variables


In order to handle variables properly, we have to find all the universally quantified variables in the Generator. All variables as yet unbound are universally quantified, unless

<ol>
 <li>they occur in the template
</li>
 <li>they are bound by X/\\P, setof, or bagof
</li>
</ol>

`free_variables(Generator, Template, OldList, NewList)` finds this set, using OldList as an accumulator.
</li>
</ul>

The original author of this code was Richard O'Keefe. Jan Wielemaker
made some SWI-Prolog enhancements, sponsored by SecuritEase,
http://www.securitease.com. The code is public domain (from DEC10 library).




@section Apply Apply Macros

This library provides a SWI-compatible set of utilities for applying a
predicate to all elements of a list. The library just forwards
definitions from the `maplist` library.

@section Association_Lists Association Lists

The following association list manipulation predicates are available
once included with the `use_module(library(assoc))` command. The
original library used Richard O'Keefe's implementation, on top of
unbalanced binary trees. The current code utilises code from the
red-black trees library and emulates the SICStus Prolog interface.

<ul>
 <li>assoc_to_list(+ _Assoc_,? _List_) @anchor assoc_to_list


Given an association list  _Assoc_ unify  _List_ with a list of
the form  _Key-Val_, where the elements  _Key_ are in ascending
order.

</li>
 <li>del_assoc(+ _Key_, + _Assoc_, ? _Val_, ? _NewAssoc_) @anchor del_assoc


Succeeds if  _NewAssoc_ is an association list, obtained by removing
the element with  _Key_ and  _Val_ from the list  _Assoc_.

</li>
 <li>del_max_assoc(+ _Assoc_, ? _Key_, ? _Val_, ? _NewAssoc_) @anchor del_max_assoc


Succeeds if  _NewAssoc_ is an association list, obtained by removing
the largest element of the list, with  _Key_ and  _Val_ from the
list  _Assoc_.

</li>
 <li>del_min_assoc(+ _Assoc_, ? _Key_, ? _Val_, ? _NewAssoc_) @anchor del_min_assoc


Succeeds if  _NewAssoc_ is an association list, obtained by removing
the smallest element of the list, with  _Key_ and  _Val_
from the list  _Assoc_.

</li>
 <li>empty_assoc(+ _Assoc_) @anchor empty_assoc


Succeeds if association list  _Assoc_ is empty.

</li>
 <li>gen_assoc(+ _Assoc_,? _Key_,? _Value_) @anchor gen_assoc


Given the association list  _Assoc_, unify  _Key_ and  _Value_
with two associated elements. It can be used to enumerate all elements
in the association list.

</li>
 <li>get_assoc(+ _Key_,+ _Assoc_,? _Value_) @anchor get_next_assoc


If  _Key_ is one of the elements in the association list  _Assoc_,
return the associated value.

</li>
 <li>get_assoc(+ _Key_,+ _Assoc_,? _Value_,+ _NAssoc_,? _NValue_) @anchor get_assoc


If  _Key_ is one of the elements in the association list  _Assoc_,
return the associated value  _Value_ and a new association list
 _NAssoc_ where  _Key_ is associated with  _NValue_.

</li>
 <li>get_prev_assoc(+ _Key_,+ _Assoc_,? _Next_,? _Value_) @anchor get_prev_assoc


If  _Key_ is one of the elements in the association list  _Assoc_,
return the previous key,  _Next_, and its value,  _Value_.

</li>
 <li>get_next_assoc(+ _Key_,+ _Assoc_,? _Next_,? _Value_)

If  _Key_ is one of the elements in the association list  _Assoc_,
return the next key,  _Next_, and its value,  _Value_.

</li>
 <li>is_assoc(+ _Assoc_) @anchor is_assoc


Succeeds if  _Assoc_ is an association list, that is, if it is a
red-black tree.

</li>
 <li>list_to_assoc(+ _List_,? _Assoc_) @anchor list_to_assoc


Given a list  _List_ such that each element of  _List_ is of the
form  _Key-Val_, and all the  _Keys_ are unique,  _Assoc_ is
the corresponding association list.

</li>
 <li>map_assoc(+ _Pred_,+ _Assoc_) @anchor map_assoc


Succeeds if the unary predicate name  _Pred_( _Val_) holds for every
element in the association list.

</li>
 <li>map_assoc(+ _Pred_,+ _Assoc_,? _New_)

Given the binary predicate name  _Pred_ and the association list
 _Assoc_,  _New_ in an association list with keys in  _Assoc_,
and such that if  _Key-Val_ is in  _Assoc_, and  _Key-Ans_ is in
 _New_, then  _Pred_( _Val_, _Ans_) holds.

</li>
 <li>max_assoc(+ _Assoc_,- _Key_,? _Value_) @anchor max_assoc


Given the association list
 _Assoc_,  _Key_ in the largest key in the list, and  _Value_
the associated value.

</li>
 <li>min_assoc(+ _Assoc_,- _Key_,? _Value_) @anchor min_assoc


Given the association list
 _Assoc_,  _Key_ in the smallest key in the list, and  _Value_
the associated value.

</li>
 <li>ord_list_to_assoc(+ _List_,? _Assoc_) @anchor ord_list_to_assoc


Given an ordered list  _List_ such that each element of  _List_ is
of the form  _Key-Val_, and all the  _Keys_ are unique,  _Assoc_ is
the corresponding association list.

</li>
 <li>put_assoc(+ _Key_,+ _Assoc_,+ _Val_,+ _New_) @anchor put_assoc


The association list  _New_ includes and element of association
 _key_ with  _Val_, and all elements of  _Assoc_ that did not
have key  _Key_.

</li>
</ul>

@section AVL_Trees AVL Trees

AVL trees are balanced search binary trees. They are named after their
inventors, Adelson-Velskii and Landis, and they were the first
dynamically balanced trees to be proposed. The YAP AVL tree manipulation
predicates library uses code originally written by Martin van Emdem and
published in the Logic Programming Newsletter, Autumn 1981.  A bug in
this code was fixed by Philip Vasey, in the Logic Programming
Newsletter, Summer 1982. The library currently only includes routines to
insert and lookup elements in the tree. Please try red-black trees if
you need deletion.

<ul>
 <li>avl_new(+ _T_) @anchor avl_new


Create a new tree.

</li>
 <li>avl_insert(+ _Key_,? _Value_,+ _T0_,- _TF_) @anchor avl_insert


Add an element with key  _Key_ and  _Value_ to the AVL tree
 _T0_ creating a new AVL tree  _TF_. Duplicated elements are
allowed.

</li>
 <li>avl_lookup(+ _Key_,- _Value_,+ _T_) @anchor avl_lookup


Lookup an element with key  _Key_ in the AVL tree
 _T_, returning the value  _Value_.

</li>
</ul>

@section Exo_Intervals Exo Intervals
This package assumes you use exo-compilation, that is, that you loaded
the pedicate using the `exo` option to [load_files/2](@ref load_files), In this
case, YAP includes a package for improved search on  intervals of
integers.

The package is activated by `udi` declarations that state what is
the argument of interest:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
:- udi(diagnoses(exo_interval,?,?)).

:- load_files(db, [consult(exo)]).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is designed to optimise the following type of queries:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- max(X, diagnoses(X, 9, Y), X).

?- min(X, diagnoses(X, 9, 36211117), X).

?- X #< Y, min(X, diagnoses(X, 9, 36211117), X ), diagnoses(Y, 9, _).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The first argument gives the time, the second the patient, and the
third the condition code. The first query should find the last time
the patient 9 had any code reported, the second looks for the first
report of code 36211117, and the last searches for reports after this
one. All queries run in  constant or log(n) time.

@section Gecode Gecode Interface

The gecode library intreface was designed and implemented by Denis
Duchier, with recent work by Vítor Santos Costa to port it to version 4
of gecode and to have an higher level interface,

@subsection The_Gecode_Interface The Gecode Interface

This text is due to Denys Duchier. The gecode interface requires

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
:- use_module(library(gecode)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Several example programs are available with the distribution.

<ul>
 <li>CREATING A SPACE

A space is gecodes data representation for a store of constraints:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Space := space
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>CREATING VARIABLES

Unlike in Gecode, variable objects are not bound to a specific Space.  Each one
actually contains an index with which it is possible to access a Space-bound
Gecode variable.  Variables can be created using the following expressions:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
   IVar := intvar(Space,SPEC...)
   BVar := boolvar(Space)
   SVar := setvar(Space,SPEC...)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where SPEC... is the same as in Gecode.  For creating lists of variables use
the following variants:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
   IVars := intvars(Space,N,SPEC...)
   BVars := boolvars(Space,N,SPEC...)
   SVars := setvars(Space,N,SPEC...)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where N is the number of variables to create (just like for XXXVarArray in
Gecode).  Sometimes an IntSet is necessary:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
   ISet := intset([SPEC...])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where each SPEC is either an integer or a pair (I,J) of integers.  An IntSet
describes a set of ints by providing either intervals, or integers (which stand
for an interval of themselves).  It might be tempting to simply represent an
IntSet as a list of specs, but this would be ambiguous with IntArgs which,
here, are represented as lists of ints.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
   Space += keep(Var)
   Space += keep(Vars)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Variables can be marked as "kept".  In this case, only such variables will be
explicitly copied during search.  This could bring substantial benefits in
memory usage.  Of course, in a solution, you can then only look at variables
that have been "kept".  If no variable is marked as "kept", then they are all
kept.  Thus marking variables as "kept" is purely an optimization.

</li>
 <li>CONSTRAINTS AND BRANCHINGS

all constraint and branching posting functions are available just like in
Gecode.  Wherever a XXXArgs or YYYSharedArray is expected, simply use a list.
At present, there is no support for minimodel-like constraint posting.
Constraints and branchings are added to a space using:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Space += CONSTRAINT
    Space += BRANCHING
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Space += rel(X,'IRT_EQ',Y)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

arrays of variables are represented by lists of variables, and constants are
represented by atoms with the same name as the Gecode constant
(e.g. 'INT_VAR_SIZE_MIN').

</li>
 <li>SEARCHING FOR SOLUTIONS

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    SolSpace := search(Space)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a backtrackable predicate that enumerates all solution spaces
(SolSpace).  It may also take options:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    SolSpace := search(Space,Options)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Options is a list whose elements maybe:

<ul>
 <li>restart
to select the Restart search engine
</li>
 <li>threads=N
to activate the parallel search engine and control the number of
workers (see Gecode doc)
</li>
 <li>c_d=N
to set the commit distance for recomputation
</li>
 <li>a_d=N
to set the adaptive distance for recomputation

</li>
</ul>

</li>
 <li>EXTRACTING INFO FROM A SOLUTION

An advantage of non Space-bound variables, is that you can use them both to
post constraints in the original space AND to consult their values in
solutions.  Below are methods for looking up information about variables.  Each
of these methods can either take a variable as argument, or a list of
variables, and returns resp. either a value, or a list of values:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Val := assigned(Space,X)

    Val := min(Space,X)
    Val := max(Space,X)
    Val := med(Space,X)
    Val := val(Space,X)
    Val := size(Space,X)
    Val := width(Space,X)
    Val := regret_min(Space,X)
    Val := regret_max(Space,X)

    Val := glbSize(Space,V)
    Val := lubSize(Space,V)
    Val := unknownSize(Space,V)
    Val := cardMin(Space,V)
    Val := cardMax(Space,V)
    Val := lubMin(Space,V)
    Val := lubMax(Space,V)
    Val := glbMin(Space,V)
    Val := glbMax(Space,V)
    Val := glb_ranges(Space,V)
    Val := lub_ranges(Space,V)
    Val := unknown_ranges(Space,V)
    Val := glb_values(Space,V)
    Val := lub_values(Space,V)
    Val := unknown_values(Space,V)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>DISJUNCTORS

Disjunctors provide support for disjunctions of clauses, where each clause is a
conjunction of constraints:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    C1 or C2 or ... or Cn
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each clause is executed "speculatively": this means it does not affect the main
space.  When a clause becomes failed, it is discarded.  When only one clause
remains, it is committed: this means that it now affects the main space.

Example:

Consider the problem where either X=Y=0 or X=Y+(1 or 2) for variable X and Y
that take values in 0..3.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Space := space,
    [X,Y] := intvars(Space,2,0,3),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First, we must create a disjunctor as a manager for our 2 clauses:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Disj := disjunctor(Space),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can now create our first clause:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    C1 := clause(Disj),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This clause wants to constrain X and Y to 0.  However, since it must be
executed "speculatively", it must operate on new variables X1 and Y1 that
shadow X and Y:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    [X1,Y1] := intvars(C1,2,0,3),
    C1 += forward([X,Y],[X1,Y1]),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The forward(...) stipulation indicates which global variable is shadowed by
which clause-local variable.  Now we can post the speculative clause-local
constraints for X=Y=0:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    C1 += rel(X1,'IRT_EQ',0),
    C1 += rel(Y1,'IRT_EQ',0),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We now create the second clause which uses X2 and Y2 to shadow X and Y:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    C2 := clause(Disj),
    [X2,Y2] := intvars(C2,2,0,3),
    C2 += forward([X,Y],[X2,Y2]),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

However, this clause also needs a clause-local variable Z2 taking values 1 or
2 in order to post the clause-local constraint X2=Y2+Z2:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Z2 := intvar(C2,1,2),
    C2 += linear([-1,1,1],[X2,Y2,Z2],'IRT_EQ',0),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Finally, we can branch and search:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Space += branch([X,Y],'INT_VAR_SIZE_MIN','INT_VAL_MIN'),
    SolSpace := search(Space),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and lookup values of variables in each solution:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    [X_,Y_] := val(SolSpace,[X,Y]).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
</ul>

@subsection Gecode_and_ClPbBFDbC Programming Finite Domain Constraints in YAP/Gecode

The gecode/clp(fd) interface is designed to use the GECODE functionality
in a more CLP like style. It requires

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
:- use_module(library(gecode/clpfd)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Several example programs are available with the distribution.

Integer variables are declared as:

<ul>
 <li>_V_ in  _A_.. _B_
declares an integer variable  _V_ with range  _A_ to  _B_.
</li>
 <li>_Vs_ ins  _A_.. _B_
declares a set of integer variabless  _Vs_ with range  _A_ to  _B_.
</li>
 <li>boolvar( _V_)
declares a  boolean variable.
</li>
 <li>boolvars( _Vs_)
declares a set of  boolean variable.
</li>
</ul>

Constraints supported are:

<ul>
 <li>_X_ #=  _Y_
equality
</li>
 <li>_X_ #\\=  _Y_
disequality
</li>
 <li>_X_ #\>  _Y_
larger
</li>
 <li>_X_ #\>=  _Y_
larger or equal
</li>
 <li>_X_ #=\<  _Y_
smaller
</li>
 <li>_X_ #\<  _Y_
smaller or equal

Arguments to this constraint may be an arithmetic expression with <tt>+</tt>,
<tt>-</tt>, <tt>\\\*</tt>, integer division <tt>/</tt>, <tt>min</tt>, <tt>max</tt>, <tt>sum</tt>,
<tt>count</tt>, and
<tt>abs</tt>. Boolean variables support conjunction (/\\), disjunction (\\/),
implication (=\>), equivalence (\<=\>), and xor. The <tt>sum</tt> constraint allows  a two argument version using the
`where` conditional, in Zinc style. 

The send more money equation may be written as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
          1000*S + 100*E + 10*N + D +
          1000*M + 100*O + 10*R + E #=
10000*M + 1000*O + 100*N + 10*E + Y,
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This example uses `where` to select from
column  _I_ the elements that have value under  _M_:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
OutFlow[I] #= sum(J in 1..N where D[J,I]<M, X[J,I])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The <tt>count</tt> constraint counts the number of elements that match a
certain constant or variable (integer sets are not available).

</li>
 <li>all_different( _Vs_    )
</li>
 <li>all_distinct( _Vs_)
</li>
 <li>all_different( _Cs_,  _Vs_)
</li>
 <li>all_distinct( _Cs_,  _Vs_)
verifies whether all elements of a list are different. In the second
case, tests if all the sums between a list of constants and a list of
variables are different.

This is a formulation of the queens problem that uses both versions of `all_different`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
queens(N, Queens) :-
    length(Queens, N),
    Queens ins 1..N,
    all_distinct(Queens),
    foldl(inc, Queens, Inc, 0, _), % [0, 1, 2, .... ]
    foldl(dec, Queens, Dec, 0, _), % [0, -1, -2, ... ]
    all_distinct(Inc,Queens),
    all_distinct(Dec,Queens),
    labeling([], Queens).

inc(_, I0, I0, I) :-
    I is I0+1.

dec(_, I0, I0, I) :-
    I is I0-1.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The next example uses `all_different/1` and the functionality of the matrix package to verify that all squares in
sudoku have a different value:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    foreach( [I,J] ins 0..2 ,
           all_different(M[I*3+(0..2),J*3+(0..2)]) ),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>scalar_product(+ _Cs_, + _Vs_, + _Rel_, ? _V_    )

The product of constant  _Cs_ by  _Vs_ must be in relation
 _Rel_ with  _V_ .

</li>
 <li>_X_ #= 
all elements of  _X_  must take the same value
</li>
 <li>_X_ #\\= 
not all elements of  _X_  take the same value
</li>
 <li>_X_ #\> 
elements of  _X_  must be increasing
</li>
 <li>_X_ #\>= 
elements of  _X_  must be increasinga or equal
</li>
 <li>_X_ #=\< 
elements of  _X_  must be decreasing
</li>
 <li>_X_ #\< 
elements of  _X_  must be decreasing or equal

</li>
 <li>_X_ #\<==\>  _B_
reified equivalence
</li>
 <li>_X_ #==\>  _B_
reified implication
</li>
 <li>_X_ #\<  _B_
reified implication 

As an example. consider finding out the people who wanted to sit
next to a friend and that are are actually sitting together:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
preference_satisfied(X-Y, B) :-
    abs(X - Y) #= 1 #<==> B.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that not all constraints may be reifiable.

</li>
 <li>element( _X_,  _Vs_    )
 _X_ is an element of list  _Vs_

</li>
 <li>clause( _Type_,  _Ps_ ,  _Ns_,  _V_     )
If  _Type_ is `and` it is the conjunction of boolean variables
 _Ps_ and the negation of boolean variables  _Ns_ and must have
result  _V_. If  _Type_ is `or` it is a disjunction.

</li>
 <li>DFA
the interface allows creating and manipulation deterministic finite
automata. A DFA has a set of states, represented as integers
and is initialised with an initial state, a set of transitions from the
first to the last argument emitting the middle argument, and a final
state.

The swedish-drinkers protocol is represented as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    A = [X,Y,Z],
    dfa( 0, [t(0,0,0),t(0,1,1),t(1,0,0),t(-1,0,0)], [0], C),
    in_dfa( A, C ),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This code will enumeratae the valid tuples of three emissions.

</li>
 <li>extensional constraints
Constraints can also be represented as lists of tuples. 

The previous example
would be written as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    extensional_constraint([[0,0,0],[0,1,0],[1,0,0]], C),
    in_relation( A, C ),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>minimum( _X_,  _Vs_)
</li>
 <li>min( _X_,  _Vs_)
First Argument is the least element of a list.

</li>
 <li>maximum( _X_,  _Vs_)
</li>
 <li>max( _X_,  _Vs_)
First Argument is the greatest element of a list.

</li>
 <li>lex_order( _Vs_)
All elements must be ordered.

</li>
</ul>

The following predicates control search:

<ul>
 <li>labeling( _Opts_,  _Xs_)
performs labeling, several variable and value selection options are
available. The defaults are `min` and `min_step`.

Variable selection options are as follows:

<ul>
 <li>leftmost
choose the first variable
</li>
 <li>min
choose one of the variables with smallest minimum value
</li>
 <li>max
choose one of the variables with greatest maximum value
</li>
 <li>ff
choose one of the most constrained variables, that is, with the smallest
domain.
</li>
</ul>

Given that we selected a variable, the values chosen for branching may
be:

<ul>
 <li>min_step
smallest value
</li>
 <li>max_step
largest value
</li>
 <li>bisect
median
</li>
 <li>enum
all value starting from the minimum.
</li>
</ul>

</li>
 <li>maximize( _V_)
maximise variable  _V_

</li>
 <li>minimize(<tt>V</tt>) 
minimise variable  _V_

</li>
</ul>

@section Heaps Heaps

A heap is a labelled binary tree where the key of each node is less than
or equal to the keys of its sons.  The point of a heap is that we can
keep on adding new elements to the heap and we can keep on taking out
the minimum element.  If there are N elements total, the total time is
O(NlgN).  If you know all the elements in advance, you are better off
doing a merge-sort, but this file is for when you want to do say a
best-first search, and have no idea when you start how many elements
there will be, let alone what they are.

The following heap manipulation routines are available once included
with the `use_module(library(heaps))` command. 

<ul>

 <li>add_to_heap(+ _Heap_,+ _key_,+ _Datum_,- _NewHeap_) @anchor add_to_heap


Inserts the new  _Key-Datum_ pair into the heap. The insertion is not
stable, that is, if you insert several pairs with the same  _Key_ it
is not defined which of them will come out first, and it is possible for
any of them to come out first depending on the  history of the heap.

</li>
 <li>empty_heap(? _Heap_) @anchor empty_heap


Succeeds if  _Heap_ is an empty heap.

</li>
 <li>get_from_heap(+ _Heap_,- _key_,- _Datum_,- _Heap_) @anchor get_from_heap


Returns the  _Key-Datum_ pair in  _OldHeap_ with the smallest
 _Key_, and also a  _Heap_ which is the  _OldHeap_ with that
pair deleted.

</li>
 <li>heap_size(+ _Heap_, - _Size_) @anchor heap_size


Reports the number of elements currently in the heap.

</li>
 <li>heap_to_list(+ _Heap_, - _List_) @anchor heap_to_list


Returns the current set of  _Key-Datum_ pairs in the  _Heap_ as a
 _List_, sorted into ascending order of  _Keys_.

</li>
 <li>list_to_heap(+ _List_, - _Heap_) @anchor list_to_heap


Takes a list of  _Key-Datum_ pairs (such as keysort could be used to sort)
and forms them into a heap.

</li>
 <li>min_of_heap(+ _Heap_,  - _Key_,  - _Datum_) @anchor min_of_heap


Returns the Key-Datum pair at the top of the heap (which is of course
the pair with the smallest Key), but does not remove it from the heap.

</li>
 <li>min_of_heap(+ _Heap_,  - _Key1_,  - _Datum1_,
- _Key2_,  - _Datum2_)

Returns the smallest (Key1) and second smallest (Key2) pairs in the
heap, without deleting them.
</li>
</ul>

@section Lists List Manipulation

The following list manipulation routines are available once included
with the `use_module(library(lists))` command. 

<ul>

 <li>append(? _Prefix_,? _Suffix_,? _Combined_) @anchor append


True when all three arguments are lists, and the members of
 _Combined_ are the members of  _Prefix_ followed by the members of  _Suffix_.
It may be used to form  _Combined_ from a given  _Prefix_,  _Suffix_ or to take
a given  _Combined_ apart.

</li>
 <li>append(? _Lists_,? _Combined_)

Holds if the lists of  _Lists_ can be concatenated as a
 _Combined_ list.

</li>
 <li>delete(+ _List_, ? _Element_, ? _Residue_) @anchor delete


True when  _List_ is a list, in which  _Element_ may or may not
occur, and  _Residue_ is a copy of  _List_ with all elements
identical to  _Element_ deleted.

</li>
 <li>flatten(+ _List_, ? _FlattenedList_) @anchor flatten


Flatten a list of lists  _List_ into a single list
 _FlattenedList_.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- flatten([[1],[2,3],[4,[5,6],7,8]],L).

L = [1,2,3,4,5,6,7,8] ? ;

no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>last(+ _List_,? _Last_) @anchor last


True when  _List_ is a list and  _Last_ is identical to its last element.

</li>
 <li>list_concat(+ _Lists_,? _List_) @anchor list_concat


True when  _Lists_ is a list of lists and  _List_ is the
concatenation of  _Lists_.

</li>
 <li>member(? _Element_, ? _Set_) @anchor member


True when  _Set_ is a list, and  _Element_ occurs in it.  It may be used
to test for an element or to enumerate all the elements by backtracking.

</li>
 <li>memberchk(+ _Element_, + _Set_) @anchor memberchk


As [member/2](@ref member), but may only be used to test whether a known
 _Element_ occurs in a known Set.  In return for this limited use, it
is more efficient when it is applicable.

</li>
 <li>nth0(? _N_, ? _List_, ? _Elem_) @anchor nth0


True when  _Elem_ is the Nth member of  _List_,
counting the first as element 0.  (That is, throw away the first
N elements and unify  _Elem_ with the next.)  It can only be used to
select a particular element given the list and index.  For that
task it is more efficient than [member/2](@ref member)

</li>
 <li>nth1(? _N_, ? _List_, ? _Elem_) @anchor nth1


The same as [nth0/3](@ref nth0), except that it counts from
1, that is `nth(1, [H|_], H)`.

</li>
 <li>nth(? _N_, ? _List_, ? _Elem_) @anchor nth


The same as [nth1/3](@ref nth1).

</li>
 <li>nth0(? _N_, ? _List_, ? _Elem_, ? _Rest_)

Unifies  _Elem_ with the Nth element of  _List_,
counting from 0, and  _Rest_ with the other elements.  It can be used
to select the Nth element of  _List_ (yielding  _Elem_ and  _Rest_), or to
insert  _Elem_ before the Nth (counting from 1) element of  _Rest_, when
it yields  _List_, e.g. `nth0(2, List, c, [a,b,d,e])` unifies List with
`[a,b,c,d,e]`.  `nth/4` is the same except that it counts from 1.  `nth0/4`
can be used to insert  _Elem_ after the Nth element of  _Rest_.

</li>
 <li>nth1(? _N_, ? _List_, ? _Elem_, ? _Rest_)

Unifies  _Elem_ with the Nth element of  _List_, counting from 1,
and  _Rest_ with the other elements.  It can be used to select the
Nth element of  _List_ (yielding  _Elem_ and  _Rest_), or to
insert  _Elem_ before the Nth (counting from 1) element of
 _Rest_, when it yields  _List_, e.g. `nth(3, List, c, [a,b,d,e])` unifies List with `[a,b,c,d,e]`.  `nth/4`
can be used to insert  _Elem_ after the Nth element of  _Rest_.

</li>
 <li>nth(? _N_, ? _List_, ? _Elem_, ? _Rest_)

Same as `nth1/4`.

</li>
 <li>permutation(+ _List_,? _Perm_) @anchor permutation


True when  _List_ and  _Perm_ are permutations of each other.

</li>
 <li>remove_duplicates(+ _List_, ? _Pruned_) @anchor remove_duplicates


Removes duplicated elements from  _List_.  Beware: if the  _List_ has
non-ground elements, the result may surprise you.

</li>
 <li>reverse(+ _List_, ? _Reversed_) @anchor reverse


True when  _List_ and  _Reversed_ are lists with the same elements
but in opposite orders. 

</li>
 <li>same_length(? _List1_, ? _List2_) @anchor same_length


True when  _List1_ and  _List2_ are both lists and have the same number
of elements.  No relation between the values of their elements is
implied.
Modes `same_length(-,+)` and `same_length(+,-)` generate either list given
the other; mode `same_length(-,-)` generates two lists of the same length,
in which case the arguments will be bound to lists of length 0, 1, 2, ...

</li>
 <li>select(? _Element_, ? _List_, ? _Residue_) @anchor select


True when  _Set_ is a list,  _Element_ occurs in  _List_, and
 _Residue_ is everything in  _List_ except  _Element_ (things
stay in the same order).

</li>
 <li>selectchk(? _Element_, ? _List_, ? _Residue_) @anchor selectchk


Semi-deterministic selection from a list. Steadfast: defines as

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
selectchk(Elem, List, Residue) :-
        select(Elem, List, Rest0), !,
        Rest = Rest0.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>sublist(? _Sublist_, ? _List_) @anchor sublist


True when both `append(_,Sublist,S)` and `append(S,_,List)` hold.

</li>
 <li>suffix(? _Suffix_, ? _List_) @anchor suffix


Holds when `append(_,Suffix,List)` holds.

</li>
 <li>sum_list(? _Numbers_, ? _Total_) @anchor sum_list


True when  _Numbers_ is a list of numbers, and  _Total_ is their sum.

</li>
 <li>sum_list(? _Numbers_, + _SoFar_, ? _Total_)

True when  _Numbers_ is a list of numbers, and  _Total_ is the sum of their total plus  _SoFar_.

</li>
 <li>sumlist(? _Numbers_, ? _Total_) @anchor sumlist


True when  _Numbers_ is a list of integers, and  _Total_ is their
sum. The same as [sum_list/2](@ref sum_list), please do use [sum_list/2](@ref sum_list)
instead.

</li>
 <li>max_list(? _Numbers_, ? _Max_) @anchor max_list


True when  _Numbers_ is a list of numbers, and  _Max_ is the maximum.

</li>
 <li>min_list(? _Numbers_, ? _Min_) @anchor min_list


True when  _Numbers_ is a list of numbers, and  _Min_ is the minimum.

</li>
 <li>numlist(+ _Low_, + _High_, + _List_) @anchor numlist


If  _Low_ and  _High_ are integers with  _Low_ =\<
 _High_, unify  _List_ to a list `[Low, Low+1, ...High]`. See
also [between/3](@ref between).

</li>
 <li>intersection(+ _Set1_, + _Set2_, + _Set3_) @anchor intersection


Succeeds if  _Set3_ unifies with the intersection of  _Set1_ and
 _Set2_.  _Set1_ and  _Set2_ are lists without duplicates. They
need not be ordered.

</li>
 <li>subtract(+ _Set_, + _Delete_, ? _Result_) @anchor subtract


Delete all elements from  _Set_ that   occur  in  _Delete_ (a set)
and unify the  result  with   _Result_.   Deletion  is  based  on
unification using [memberchk/2](@ref memberchk). The complexity is
`|Delete|\*|Set|`.

See [ord_subtract/3](@ref ord_subtract).
</li>
</ul>

@section LineUtilities Line Manipulation Utilities

This package provides a set of useful predicates to manipulate
sequences of characters codes, usually first read in as a line. It is
available by loading the library `library(lineutils)`.

<ul>

 <li>search_for(+ _Char_,+ _Line_) @anchor search_for



Search for a character  _Char_ in the list of codes  _Line_.

</li>
 <li>search_for(+ _Char_,+ _Line_,- _RestOfine_)


Search for a character  _Char_ in the list of codes  _Line_,
 _RestOfLine_ has the line to the right.

</li>
 <li>scan_natural(? _Nat_,+ _Line_,+ _RestOfLine_) @anchor scan_natural



Scan the list of codes  _Line_ for a natural number  _Nat_, zero
or a positive integer, and unify  _RestOfLine_ with the remainder
of the line.

</li>
 <li>scan_integer(? _Int_,+ _Line_,+ _RestOfLine_) @anchor scan_integer



Scan the list of codes  _Line_ for an integer  _Nat_, either a
positive, zero, or negative integer, and unify  _RestOfLine_ with
the remainder of the line.

</li>
 <li>split(+ _Line_,+ _Separators_,- _Split_) @anchor split



Unify  _Words_ with a set of strings obtained from  _Line_ by
using the character codes in  _Separators_ as separators. As an
example, consider:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- split("Hello * I am free"," *",S).

S = ["Hello","I","am","free"] ?

no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>split(+ _Line_,- _Split_)


Unify  _Words_ with a set of strings obtained from  _Line_ by
using the blank characters  as separators.

</li>
 <li>fields(+ _Line_,+ _Separators_,- _Split_) @anchor fields



Unify  _Words_ with a set of strings obtained from  _Line_ by
using the character codes in  _Separators_ as separators for
fields. If two separators occur in a row, the field is considered
empty. As an example, consider:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- fields("Hello  I am  free"," *",S).

S = ["Hello","","I","am","","free"] ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>fields(+ _Line_,- _Split_)


Unify  _Words_ with a set of strings obtained from  _Line_ by
using the blank characters  as field separators.

</li>
 <li>glue(+ _Words_,+ _Separator_,- _Line_) @anchor glue



Unify  _Line_ with  string obtained by glueing  _Words_ with
the character code  _Separator_.

</li>
 <li>copy_line(+ _StreamInput_,+ _StreamOutput_) @anchor copy_line



Copy a line from  _StreamInput_ to  _StreamOutput_.

</li>
 <li>process(+ _StreamInp_, + _Goal_) @anchor process



For every line  _LineIn_ in stream  _StreamInp_, call
`call(Goal,LineIn)`.

</li>
 <li>filter(+ _StreamInp_, + _StreamOut_, + _Goal_) @anchor filter



For every line  _LineIn_ in stream  _StreamInp_, execute
`call(Goal,LineIn,LineOut)`, and output  _LineOut_ to
stream  _StreamOut_.

</li>
 <li>file_filter(+ _FileIn_, + _FileOut_, + _Goal_) @anchor file_filter



For every line  _LineIn_ in file  _FileIn_, execute
`call(Goal,LineIn,LineOut)`, and output  _LineOut_ to file
 _FileOut_.

</li>
 <li>file_filter(+ _FileIn_, + _FileOut_, + _Goal_, @anchor file_filter_with_init

+ _FormatCommand_,   + _Arguments_)


Same as [file_filter/3](@ref file_filter), but before starting the filter execute
`format/3` on the output stream, using  _FormatCommand_ and
 _Arguments_.

</li>
</ul>

@section matrix Matrix Library

This package provides a fast implementation of multi-dimensional
matrices of integers and floats. In contrast to dynamic arrays, these
matrices are multi-dimensional and compact. In contrast to static
arrays. these arrays are allocated in the stack. Matrices are available
by loading the library `library(matrix)`. They are multimensional
objects of type:

<ul>
 <li><tt>terms</tt>: Prolog terms
</li>
 <li><tt>ints</tt>: bounded integers, represented as an opaque term. The
maximum integer depends on hardware, but should be obtained from the
natural size of the machine.
</li>
 <li><tt>floats</tt>: floating-poiny numbers, represented as an opaque term.
</li>
</ul>

Matrix elements can be accessed through the `matrix_get/2`
predicate or through an <tt>R</tt>-inspired access notation (that uses the ciao
style extension to `[]`.  Examples include:

<ul>
 <li>_E_ \<==  _X_[2,3]
Access the second row, third column of matrix <tt>X</tt>. Indices start from
`0`,
</li>
 <li>_L_ \<==  _X_[2,_]
Access all the second row, the output is a list ofe elements.
</li>
 <li>_L_ \<==  _X_[2..4,_]
Access all the second, thrd and fourth rows, the output is a list of elements.
</li>
 <li>_L_ \<==  _X_[2..4+3,_]
Access all the fifth, sixth and eight rows, the output is a list of elements.
</li>
</ul>

The matrix library also supports a B-Prolog/ECliPSe inspired `foreach` ITerator to iterate over
elements of a matrix:

<ul>
 <li>foreach(I in 0..N1, X[I] \<== Y[I])
Copies a vector, element by element.
</li>
 <li>foreach([I in 0..N1, J in I..N1], Z[I,J] \<== X[I,J] - X[I,J])
The lower-triangular matrix  _Z_ is the difference between the
lower-triangular and upper-triangular parts of  _X_.
</li>
 <li>foreach([I in 0..N1, J in 0..N1], plus(X[I,J]), 0, Sum)
Add all elements of a matrix by using  _Sum_ as an accumulator.
</li>
</ul>

Notice that the library does not support all known matrix operations. Please
contact the YAP maintainers if you require extra functionality.

<ul>

 <li>_X_ = array[ _Dim1_,..., _Dimn_] of  _Objects_ @anchor of


The [of/2](@ref of) operator can be used to create a new array of
 _Objects_. The objects supported are:

<ul>
 <li>Unbound Variable
create an array of free variables
</li>
 <li>ints 
create an array of integers
</li>
 <li>floats 
create an array of floating-point numbers
</li>
 <li>_I_: _J_
create an array with integers from  _I_ to  _J_
</li>
 <li>[..]
create an array from the values in a list
</li>
</ul>

The dimensions can be given as an integer, and the matrix will be
indexed `C`-style from  `0..( _Max_-1)`, or can be given
as  an interval ` _Base_.. _Limit_`. In the latter case,
matrices of integers and of floating-point numbers should have the same
 _Base_ on every dimension.

</li>
 <li>? _LHS_ \<==  _RHS_ @anchor sSqQqQ


General matrix assignment operation. It evaluates the right-hand side
and then acts different according to the
left-hand side and to the matrix:

<ul>
 <li>if  _LHS_ is part of an integer or floating-point matrix,
perform non-backtrackable assignment.
</li>
 <li>other unify left-hand side and right-hand size.
</li>
</ul>

The right-hand side supports the following operators: 

<ul>
 <li>[]/2
written as  _M_[ _Offset_]: obtain an element or list of elements
of matrix  _M_ at offset  _Offset_.
</li>
 <li>matrix/1
create a vector from a list
</li>
 <li>matrix/2
create a matrix from a list. Oprions are:

<ul>
 <li>dim=
a list of dimensiona
</li>
 <li>type=
integers, floating-point or terms
</li>
 <li>base=
a list of base offsets per dimension (all must be the same for arrays of
integers and floating-points
</li>
</ul>
</li>
 <li>matrix/3
create matrix giving two options
</li>
 <li>dim/1
list with matrix dimensions
</li>
 <li>nrow/1
number of rows in bi-dimensional matrix
</li>
 <li>ncol/1
number of columns in bi-dimensional matrix
</li>
 <li>length/1
size of a matrix
</li>
 <li>size/1
size of a matrix
</li>
 <li>max/1
maximum element of a numeric matrix
</li>
 <li>maxarg/1
argument of maximum element of a numeric matrix
</li>
 <li>min/1
minimum element of a numeric matrix
</li>
 <li>minarg/1
argument of minimum element of a numeric matrix
</li>
 <li>list/1
represent matrix as a list
</li>
 <li>lists/2
represent matrix as list of embedded lists
</li>
 <li>../2
 _I_.. _J_ generates a list with all integers from  _I_ to
 _J_, included.
</li>
 <li>+/2
add two numbers, add two matrices element-by-element, or add a number to
all elements of a matrix or list
</li>
 <li>-/2 
subtract two numbers, subtract two matrices or lists element-by-element, or subtract a number from
all elements of a matrix or list
</li>
 <li>\* /2 
multiply two numbers, multiply two matrices or lists element-by-element, or multiply a number from
all elements of a matrix or list
</li>
 <li>log/1 
natural logarithm of a number, matrix or list
</li>
 <li>exp/1 
natural exponentiation of a number, matrix or list
</li>
</ul>

</li>
 <li>foreach( _Sequence_,  _Goal_) @anchor foreach_matrix


Deterministic iterator. The ranges are given by  _Sequence_ that is
either ` _I_ in  _M_.. _N_`, or of the form 
`[ _I_, _J_] ins  _M_.. _N_`, or a list of the above conditions. 

Variables in the goal are assumed to be global, ie, share a single value
in the execution. The exceptions are the iteration indices. Moreover, if
the goal is of the form ` _Locals_^ _G_` all variables
occurring in  _Locals_ are marked as local. As an example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
foreach([I,J] ins 1..N, A^(A <==M[I,J], N[I] <== N[I] + A*A) )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
the variables  _I_,  _J_ and  _A_ are duplicated for every
call (local), whereas the matrices  _M_ and  _N_ are shared
throughout the execution (global).

</li>
 <li>foreach( _Sequence_,  _Goal_,  _Acc0_,  _AccF_)

Deterministic iterator with accumulator style arguments.

</li>
 <li>matrix_new(+ _Type_,+ _Dims_,- _Matrix_) @anchor matrix_new



Create a new matrix  _Matrix_ of type  _Type_, which may be one of
`ints` or `floats`, and with a list of dimensions  _Dims_.
The matrix will be initialised to zeros.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- matrix_new(ints,[2,3],Matrix).

Matrix = {..}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that currently YAP will always write a matrix of numbers as `{..}`.

</li>
 <li>matrix_new(+ _Type_,+ _Dims_,+ _List_,- _Matrix_)


Create a new matrix  _Matrix_ of type  _Type_, which may be one of
`ints` or `floats`, with dimensions  _Dims_, and
initialised from list  _List_.

</li>
 <li>matrix_new_set(? _Dims_,+ _OldMatrix_,+ _Value_,- _NewMatrix_) @anchor matrix_new_set



Create a new matrix  _NewMatrix_ of type  _Type_, with dimensions
 _Dims_. The elements of  _NewMatrix_ are set to  _Value_.

</li>
 <li>matrix_dims(+ _Matrix_,- _Dims_) @anchor matrix_dims



Unify  _Dims_ with a list of dimensions for  _Matrix_.

</li>
 <li>matrix_ndims(+ _Matrix_,- _Dims_) @anchor matrix_ndims



Unify  _NDims_ with the number of dimensions for  _Matrix_.

</li>
 <li>matrix_size(+ _Matrix_,- _NElems_) @anchor matrix_size



Unify  _NElems_ with the number of elements for  _Matrix_.

</li>
 <li>matrix_type(+ _Matrix_,- _Type_) @anchor matrix_type



Unify  _NElems_ with the type of the elements in  _Matrix_.

</li>
 <li>matrix_to_list(+ _Matrix_,- _Elems_) @anchor matrix_to_list



Unify  _Elems_ with the list including all the elements in  _Matrix_.

</li>
 <li>matrix_get(+ _Matrix_,+ _Position_,- _Elem_) @anchor matrix_get



Unify  _Elem_ with the element of  _Matrix_ at position
 _Position_.

</li>
 <li>matrix_get(+ _Matrix_[+ _Position_],- _Elem_)


Unify  _Elem_ with the element  _Matrix_[ _Position_].

</li>
 <li>matrix_set(+ _Matrix_,+ _Position_,+ _Elem_) @anchor matrix_set



Set the element of  _Matrix_ at position
 _Position_ to   _Elem_.

</li>
 <li>matrix_set(+ _Matrix_[+ _Position_],+ _Elem_)


Set the element of  _Matrix_[ _Position_] to   _Elem_.

</li>
 <li>matrix_set_all(+ _Matrix_,+ _Elem_) @anchor matrix_set_all



Set all element of  _Matrix_ to  _Elem_.

</li>
 <li>matrix_add(+ _Matrix_,+ _Position_,+ _Operand_) @anchor matrix_add



Add  _Operand_ to the element of  _Matrix_ at position
 _Position_.

</li>
 <li>matrix_inc(+ _Matrix_,+ _Position_) @anchor matrix_inc



Increment the element of  _Matrix_ at position  _Position_.

</li>
 <li>matrix_inc(+ _Matrix_,+ _Position_,- _Element_)


Increment the element of  _Matrix_ at position  _Position_ and
unify with  _Element_.

</li>
 <li>matrix_dec(+ _Matrix_,+ _Position_) @anchor matrix_dec



Decrement the element of  _Matrix_ at position  _Position_.

</li>
 <li>matrix_dec(+ _Matrix_,+ _Position_,- _Element_)


Decrement the element of  _Matrix_ at position  _Position_ and
unify with  _Element_.

</li>
 <li>matrix_arg_to_offset(+ _Matrix_,+ _Position_,- _Offset_) @anchor matrix_arg_to_offset



Given matrix  _Matrix_ return what is the numerical  _Offset_ of
the element at  _Position_.

</li>
 <li>matrix_offset_to_arg(+ _Matrix_,- _Offset_,+ _Position_) @anchor matrix_offset_to_arg



Given a position  _Position _ for matrix  _Matrix_ return the
corresponding numerical  _Offset_ from the beginning of the matrix.

</li>
 <li>matrix_max(+ _Matrix_,+ _Max_) @anchor matrix_max



Unify  _Max_ with the maximum in matrix   _Matrix_.

</li>
 <li>matrix_maxarg(+ _Matrix_,+ _Maxarg_) @anchor matrix_maxarg



Unify  _Max_ with the position of the maximum in matrix   _Matrix_.

</li>
 <li>matrix_min(+ _Matrix_,+ _Min_) @anchor matrix_min



Unify  _Min_ with the minimum in matrix   _Matrix_.

</li>
 <li>matrix_minarg(+ _Matrix_,+ _Minarg_) @anchor matrix_minarg



Unify  _Min_ with the position of the minimum in matrix   _Matrix_.

</li>
 <li>matrix_sum(+ _Matrix_,+ _Sum_) @anchor matrix_sum



Unify  _Sum_ with the sum of all elements in matrix   _Matrix_.

</li>
 <li>matrix_agg_lines(+ _Matrix_,+ _Aggregate_) @anchor matrix_agg_lines



If  _Matrix_ is a n-dimensional matrix, unify  _Aggregate_ with
the n-1 dimensional matrix where each element is obtained by adding all
Matrix elements with same last n-1 index.

</li>
 <li>matrix_agg_cols(+ _Matrix_,+ _Aggregate_) @anchor matrix_agg_cols



If  _Matrix_ is a n-dimensional matrix, unify  _Aggregate_ with
the one dimensional matrix where each element is obtained by adding all
Matrix elements with same  first index.

</li>
 <li>matrix_op(+ _Matrix1_,+ _Matrix2_,+ _Op_,- _Result_) @anchor matrix_op



 _Result_ is the result of applying  _Op_ to matrix  _Matrix1_
and  _Matrix2_. Currently, only addition (`+`) is supported.

</li>
 <li>matrix_op_to_all(+ _Matrix1_,+ _Op_,+ _Operand_,- _Result_) @anchor matrix_op_to_all



 _Result_ is the result of applying  _Op_ to all elements of
 _Matrix1_, with  _Operand_ as the second argument. Currently,
only addition (`+`), multiplication (`\*`), and division
(`/`) are supported.

</li>
 <li>matrix_op_to_lines(+ _Matrix1_,+ _Lines_,+ _Op_,- _Result_) @anchor matrix_op_to_lines



 _Result_ is the result of applying  _Op_ to all elements of
 _Matrix1_, with the corresponding element in  _Lines_ as the
second argument. Currently, only division (`/`) is supported.

</li>
 <li>matrix_op_to_cols(+ _Matrix1_,+ _Cols_,+ _Op_,- _Result_) @anchor matrix_op_to_cols



 _Result_ is the result of applying  _Op_ to all elements of
 _Matrix1_, with the corresponding element in  _Cols_ as the
second argument. Currently, only addition (`+`) is
supported. Notice that  _Cols_ will have n-1 dimensions.

</li>
 <li>matrix_shuffle(+ _Matrix_,+ _NewOrder_,- _Shuffle_) @anchor matrix_shuffle



Shuffle the dimensions of matrix  _Matrix_ according to
 _NewOrder_. The list  _NewOrder_ must have all the dimensions of
 _Matrix_, starting from 0.

</li>
 <li>matrix_transpose(+ _Matrix_,- _Transpose_) @anchor matrix_reorder



Transpose matrix  _Matrix_ to   _Transpose_. Equivalent to:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
matrix_transpose(Matrix,Transpose) :-
        matrix_shuffle(Matrix,[1,0],Transpose).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>matrix_expand(+ _Matrix_,+ _NewDimensions_,- _New_) @anchor matrix_expand



Expand  _Matrix_ to occupy new dimensions. The elements in
 _NewDimensions_ are either 0, for an existing dimension, or a
positive integer with the size of the new dimension.

</li>
 <li>matrix_select(+ _Matrix_,+ _Dimension_,+ _Index_,- _New_) @anchor matrix_select



Select from  _Matrix_ the elements who have  _Index_ at
 _Dimension_.

</li>
 <li>matrix_row(+ _Matrix_,+ _Column_,- _NewMatrix_) @anchor matrix_row



Select from  _Matrix_ the row matching  _Column_ as new matrix  _NewMatrix_.  _Column_ must have one less dimension than the original matrix.
 _Dimension_.

</li>
</ul>

@section MATLAB MATLAB Package Interface

The MathWorks MATLAB is a widely used package for array
processing. YAP now includes a straightforward interface to MATLAB. To
actually use it, you need to install YAP calling `configure` with
the `--with-matlab=DIR` option, and you need to call
`use_module(library(lists))` command.

Accessing the matlab dynamic libraries can be complicated. In Linux
machines, to use this interface, you may have to set the environment
variable <tt>LD_LIBRARY_PATH</tt>. Next, follows an example using bash in a
64-bit Linux PC:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
export LD_LIBRARY_PATH=''$MATLAB_HOME"/sys/os/glnxa64:''$MATLAB_HOME"/bin/glnxa64:''$LD_LIBRARY_PATH"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where `MATLAB_HOME` is the directory where matlab is installed
at. Please replace `ax64` for `x86` on a 32-bit PC.

<ul>

 <li>start_matlab(+ _Options_) @anchor start_matlab


Start a matlab session. The argument  _Options_ may either be the
empty string/atom or the command to call matlab. The command may fail.

</li>
 <li>close_matlab @anchor close_matlab


Stop the current matlab session.

</li>
 <li>matlab_on @anchor matlab_on


Holds if a matlab session is on.

</li>
 <li>matlab_eval_string(+ _Command_) @anchor matlab_eval_string


Holds if matlab evaluated successfully the command  _Command_.

</li>
 <li>matlab_eval_string(+ _Command_, - _Answer_)

MATLAB will evaluate the command  _Command_ and unify  _Answer_
with a string reporting the result.

</li>
 <li>matlab_cells(+ _Size_, ? _Array_) @anchor matlab_cells


MATLAB will create an empty vector of cells of size  _Size_, and if
 _Array_ is bound to an atom, store the array in the matlab
variable with name  _Array_. Corresponds to the MATLAB command `cells`.

</li>
 <li>matlab_cells(+ _SizeX_, + _SizeY_, ? _Array_)

MATLAB will create an empty array of cells of size  _SizeX_ and
 _SizeY_, and if  _Array_ is bound to an atom, store the array
in the matlab variable with name  _Array_.  Corresponds to the
MATLAB command `cells`.

</li>
 <li>matlab_initialized_cells(+ _SizeX_, + _SizeY_, + _List_, ? _Array_) @anchor matlab_initialized_cells


MATLAB will create an array of cells of size  _SizeX_ and
 _SizeY_, initialized from the list  _List_, and if  _Array_
is bound to an atom, store the array in the matlab variable with name
 _Array_.

</li>
 <li>matlab_matrix(+ _SizeX_, + _SizeY_, + _List_, ? _Array_) @anchor matlab_matrix


MATLAB will create an array of floats of size  _SizeX_ and  _SizeY_,
initialized from the list  _List_, and if  _Array_ is bound to
an atom, store the array in the matlab variable with name  _Array_.

</li>
 <li>matlab_set(+ _MatVar_, + _X_, + _Y_, + _Value_) @anchor matlab_set


Call MATLAB to set element  _MatVar_( _X_,  _Y_) to
 _Value_. Notice that this command uses the MATLAB array access
convention.

</li>
 <li>matlab_get_variable(+ _MatVar_, - _List_) @anchor matlab_get_variable


Unify MATLAB variable  _MatVar_ with the List  _List_.

</li>
 <li>matlab_item(+ _MatVar_, + _X_, ? _Val_) @anchor matlab_item


Read or set MATLAB  _MatVar_( _X_) from/to  _Val_. Use
`C` notation for matrix access (ie, starting from 0).

</li>
 <li>matlab_item(+ _MatVar_, + _X_, + _Y_, ? _Val_)

Read or set MATLAB  _MatVar_( _X_, _Y_) from/to  _Val_. Use
`C` notation for matrix access (ie, starting from 0).

</li>
 <li>matlab_item1(+ _MatVar_, + _X_, ? _Val_) @anchor matlab_item1


Read or set MATLAB  _MatVar_( _X_) from/to  _Val_. Use
MATLAB notation for matrix access (ie, starting from 1).

</li>
 <li>matlab_item1(+ _MatVar_, + _X_, + _Y_, ? _Val_)

Read or set MATLAB  _MatVar_( _X_, _Y_) from/to  _Val_. Use
MATLAB notation for matrix access (ie, starting from 1).

</li>
 <li>matlab_sequence(+ _Min_, + _Max_, ? _Array_) @anchor matlab_sequence


MATLAB will create a sequence going from  _Min_ to  _Max_, and
if  _Array_ is bound to an atom, store the sequence in the matlab
variable with name  _Array_.

</li>
 <li>matlab_vector(+ _Size_, + _List_, ? _Array_) @anchor matlab_vector


MATLAB will create a vector of floats of size  _Size_, initialized
from the list  _List_, and if  _Array_ is bound to an atom,
store the array in the matlab variable with name  _Array_.

</li>
 <li>matlab_zeros(+ _Size_, ? _Array_) @anchor matlab_zeros


MATLAB will create a vector of zeros of size  _Size_, and if
 _Array_ is bound to an atom, store the array in the matlab
variable with name  _Array_. Corresponds to the MATLAB command
`zeros`.

</li>
 <li>matlab_zeros(+ _SizeX_, + _SizeY_, ? _Array_)

MATLAB will create an array of zeros of size  _SizeX_ and
 _SizeY_, and if  _Array_ is bound to an atom, store the array
in the matlab variable with name  _Array_.  Corresponds to the
MATLAB command `zeros`.

</li>
 <li>matlab_zeros(+ _SizeX_, + _SizeY_, + _SizeZ_, ? _Array_)

MATLAB will create an array of zeros of size  _SizeX_,  _SizeY_,
and  _SizeZ_. If  _Array_ is bound to an atom, store the array
in the matlab variable with name  _Array_.  Corresponds to the
MATLAB command `zeros`.

</li>
</ul>

@section NonhYBacktrackable_Data_Structures Non-Backtrackable Data Structures

The following routines implement well-known data-structures using global
non-backtrackable variables (implemented on the Prolog stack). The
data-structures currently supported are Queues, Heaps, and Beam for Beam
search. They are allowed through `library(nb)`. 

<ul>
 <li>nb_queue(- _Queue_) @anchor nb_queue


Create a  _Queue_.

</li>
 <li>nb_queue_close(+ _Queue_, - _Head_, ? _Tail_) @anchor nb_queue_close


Unify the queue   _Queue_ with a difference list
 _Head_- _Tail_. The queue will now be empty and no further
elements can be added.

</li>
 <li>nb_queue_enqueue(+ _Queue_, + _Element_) @anchor nb_queue_enqueue


Add  _Element_ to the front of the queue   _Queue_.

</li>
 <li>nb_queue_dequeue(+ _Queue_, - _Element_) @anchor nb_queue_dequeue


Remove  _Element_ from the front of the queue   _Queue_. Fail if
the queue is empty.

</li>
 <li>nb_queue_peek(+ _Queue_, - _Element_) @anchor nb_queue_peek


 _Element_ is the front of the queue   _Queue_. Fail if
the queue is empty.

</li>
 <li>nb_queue_size(+ _Queue_, - _Size_) @anchor nb_queue_size


Unify  _Size_ with the number of elements in the queue   _Queue_.

</li>
 <li>nb_queue_empty(+ _Queue_) @anchor nb_queue_empty


Succeeds if   _Queue_ is empty.

</li>
 <li>nb_heap(+ _DefaultSize_,- _Heap_) @anchor nb_heap


Create a  _Heap_ with default size  _DefaultSize_. Note that size
will expand as needed.

</li>
 <li>nb_heap_close(+ _Heap_) @anchor nb_heap_close


Close the heap  _Heap_: no further elements can be added.

</li>
 <li>nb_heap_add(+ _Heap_, + _Key_, + _Value_) @anchor nb_heap_add


Add  _Key_- _Value_ to the heap  _Heap_. The key is sorted on
 _Key_ only.

</li>
 <li>nb_heap_del(+ _Heap_, - _Key_, - _Value_) @anchor nb_heap_del


Remove element  _Key_- _Value_ with smallest  _Value_ in heap
 _Heap_. Fail if the heap is empty.

</li>
 <li>nb_heap_peek(+ _Heap_, - _Key_, - _Value_)) @anchor nb_heap_peek


 _Key_- _Value_ is the element with smallest  _Key_ in the heap
 _Heap_. Fail if the heap is empty.

</li>
 <li>nb_heap_size(+ _Heap_, - _Size_) @anchor nb_heap_size


Unify  _Size_ with the number of elements in the heap   _Heap_.

</li>
 <li>nb_heap_empty(+ _Heap_) @anchor nb_heap_empty


Succeeds if   _Heap_ is empty.

</li>
 <li>nb_beam(+ _DefaultSize_,- _Beam_) @anchor nb_beam


Create a  _Beam_ with default size  _DefaultSize_. Note that size
is fixed throughout.

</li>
 <li>nb_beam_close(+ _Beam_) @anchor nb_beam_close


Close the beam  _Beam_: no further elements can be added.

</li>
 <li>nb_beam_add(+ _Beam_, + _Key_, + _Value_) @anchor nb_beam_add


Add  _Key_- _Value_ to the beam  _Beam_. The key is sorted on
 _Key_ only.

</li>
 <li>nb_beam_del(+ _Beam_, - _Key_, - _Value_) @anchor nb_beam_del


Remove element  _Key_- _Value_ with smallest  _Value_ in beam
 _Beam_. Fail if the beam is empty.

</li>
 <li>nb_beam_peek(+ _Beam_, - _Key_, - _Value_)) @anchor nb_beam_peek


 _Key_- _Value_ is the element with smallest  _Key_ in the beam
 _Beam_. Fail if the beam is empty.

</li>
 <li>nb_beam_size(+ _Beam_, - _Size_) @anchor nb_beam_size


Unify  _Size_ with the number of elements in the beam   _Beam_.

</li>
 <li>nb_beam_empty(+ _Beam_) @anchor nb_beam_empty


Succeeds if   _Beam_ is empty.

</li>
</ul>

@section Ordered_Sets Ordered Sets

The following ordered set manipulation routines are available once
included with the `use_module(library(ordsets))` command.  An
ordered set is represented by a list having unique and ordered
elements. Output arguments are guaranteed to be ordered sets, if the
relevant inputs are. This is a slightly patched version of Richard
O'Keefe's original library.

<ul>
 <li>list_to_ord_set(+ _List_, ? _Set_) @anchor list_to_ord_set


Holds when  _Set_ is the ordered representation of the set
represented by the unordered representation  _List_.

</li>
 <li>merge(+ _List1_, + _List2_, - _Merged_) @anchor merge


Holds when  _Merged_ is the stable merge of the two given lists.

Notice that [merge/3](@ref merge) will not remove duplicates, so merging
ordered sets will not necessarily result in an ordered set. Use
`ord_union/3` instead.

</li>
 <li>ord_add_element(+ _Set1_, + _Element_, ? _Set2_) @anchor ord_add_element


Inserting  _Element_ in  _Set1_ returns  _Set2_.  It should give
exactly the same result as `merge(Set1, [Element], Set2)`, but a
bit faster, and certainly more clearly. The same as [ord_insert/3](@ref ord_insert).

</li>
 <li>ord_del_element(+ _Set1_, + _Element_, ? _Set2_) @anchor ord_del_element


Removing  _Element_ from  _Set1_ returns  _Set2_.

</li>
 <li>ord_disjoint(+ _Set1_, + _Set2_) @anchor ord_disjoint


Holds when the two ordered sets have no element in common.

</li>
 <li>ord_member(+ _Element_, + _Set_) @anchor ord_member


Holds when  _Element_ is a member of  _Set_.

</li>
 <li>ord_insert(+ _Set1_, + _Element_, ? _Set2_) @anchor ord_insert


Inserting  _Element_ in  _Set1_ returns  _Set2_.  It should give
exactly the same result as `merge(Set1, [Element], Set2)`, but a
bit faster, and certainly more clearly. The same as [ord_add_element/3](@ref ord_add_element).

</li>
 <li>ord_intersect(+ _Set1_, + _Set2_) @anchor ord_intersect


Holds when the two ordered sets have at least one element in common.

</li>
 <li>ord_intersection(+ _Set1_, + _Set2_, ? _Intersection_)

Holds when Intersection is the ordered representation of  _Set1_
and  _Set2_.

</li>
 <li>ord_intersection(+ _Set1_, + _Set2_, ? _Intersection_, ? _Diff_)

Holds when Intersection is the ordered representation of  _Set1_
and  _Set2_.  _Diff_ is the difference between  _Set2_ and  _Set1_.

</li>
 <li>ord_seteq(+ _Set1_, + _Set2_) @anchor ord_seteq


Holds when the two arguments represent the same set.

</li>
 <li>ord_setproduct(+ _Set1_, + _Set2_, - _Set_) @anchor ord_setproduct


If Set1 and Set2 are ordered sets, Product will be an ordered
set of x1-x2 pairs.

</li>
 <li>ord_subset(+ _Set1_, + _Set2_) @anchor ordsubset


Holds when every element of the ordered set  _Set1_ appears in the
ordered set  _Set2_.

</li>
 <li>ord_subtract(+ _Set1_, + _Set2_, ? _Difference_) @anchor ord_subtract


Holds when  _Difference_ contains all and only the elements of  _Set1_
which are not also in  _Set2_.

</li>
 <li>ord_symdiff(+ _Set1_, + _Set2_, ? _Difference_) @anchor ord_symdiff


Holds when  _Difference_ is the symmetric difference of  _Set1_
and  _Set2_.

</li>
 <li>ord_union(+ _Sets_, ? _Union_) @anchor ord_union


Holds when  _Union_ is the union of the lists  _Sets_.

</li>
 <li>ord_union(+ _Set1_, + _Set2_, ? _Union_)

Holds when  _Union_ is the union of  _Set1_ and  _Set2_.

</li>
 <li>ord_union(+ _Set1_, + _Set2_, ? _Union_, ? _Diff_)

Holds when  _Union_ is the union of  _Set1_ and  _Set2_ and
 _Diff_ is the difference.

</li>
</ul>

@section Pseudo_Random Pseudo Random Number Integer Generator

The following routines produce random non-negative integers in the range
0 .. 2^(w-1) -1, where w is the word size available for integers, e.g.
32 for Intel machines and 64 for Alpha machines. Note that the numbers
generated by this random number generator are repeatable. This generator
was originally written by Allen Van Gelder and is based on Knuth Vol 2.

<ul>
 <li>rannum(- _I_) @anchor rannum


Produces a random non-negative integer  _I_ whose low bits are not
all that random, so it should be scaled to a smaller range in general.
The integer  _I_ is in the range 0 .. 2^(w-1) - 1. You can use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rannum(X) :- yap_flag(max_integer,MI), rannum(R), X is R/MI.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
to obtain a floating point number uniformly distributed between 0 and 1.

</li>
 <li>ranstart @anchor ranstart


Initialize the random number generator using a built-in seed. The
[ranstart/0](@ref ranstart) built-in is always called by the system when loading
the package.

</li>
 <li>ranstart(+ _Seed_)

Initialize the random number generator with user-defined  _Seed_. The
same  _Seed_ always produces the same sequence of numbers.

</li>
 <li>ranunif(+ _Range_,- _I_) @anchor ranunif


[ranunif/2](@ref ranunif) produces a uniformly distributed non-negative random
integer  _I_ over a caller-specified range  _R_.  If range is  _R_,
the result is in 0 ..  _R_-1.

</li>
</ul>

@section Queues Queues

The following queue manipulation routines are available once
included with the `use_module(library(queues))` command. Queues are
implemented with difference lists.

<ul>

 <li>make_queue(+ _Queue_) @anchor make_queue


Creates a new empty queue. It should only be used to create a new queue.

</li>
 <li>join_queue(+ _Element_, + _OldQueue_, - _NewQueue_) @anchor join_queue


Adds the new element at the end of the queue.

</li>
 <li>list_join_queue(+ _List_, + _OldQueue_, - _NewQueue_) @anchor list_join_queue


Ads the new elements at the end of the queue.

</li>
 <li>jump_queue(+ _Element_, + _OldQueue_, - _NewQueue_) @anchor jump_queue


Adds the new element at the front of the list.

</li>
 <li>list_jump_queue(+ _List_, + _OldQueue_, + _NewQueue_) @anchor list_jump_queue


Adds all the elements of  _List_ at the front of the queue.

</li>
 <li>head_queue(+ _Queue_, ? _Head_) @anchor head_queue


Unifies Head with the first element of the queue.

</li>
 <li>serve_queue(+ _OldQueue_, + _Head_, - _NewQueue_) @anchor serve_queue


Removes the first element of the queue for service.

</li>
 <li>empty_queue(+ _Queue_) @anchor empty_queue


Tests whether the queue is empty.

</li>
 <li>length_queue(+ _Queue_, - _Length_) @anchor length_queue


Counts the number of elements currently in the queue.

</li>
 <li>list_to_queue(+ _List_, - _Queue_) @anchor list_to_queue


Creates a new queue with the same elements as  _List._

</li>
 <li>queue_to_list(+ _Queue_, - _List_) @anchor queue_to_list


Creates a new list with the same elements as  _Queue_.

</li>
</ul>

@section Random Random Number Generator

The following random number operations are included with the
`use_module(library(random))` command. Since YAP-4.3.19 YAP uses
the O'Keefe public-domain algorithm, based on the "Applied Statistics"
algorithm AS183.

<ul>

 <li>getrand(- _Key_) @anchor getrand


Unify  _Key_ with a term of the form `rand(X,Y,Z)` describing the
current state of the random number generator.

</li>
 <li>random(- _Number_) @anchor random


Unify  _Number_ with a floating-point number in the range `[0...1)`.

</li>
 <li>random(+ _LOW_, + _HIGH_, - _NUMBER_)

Unify  _Number_ with a number in the range
`[LOW...HIGH)`. If both  _LOW_ and  _HIGH_ are
integers then  _NUMBER_ will also be an integer, otherwise
 _NUMBER_ will be a floating-point number.

</li>
 <li>randseq(+ _LENGTH_, + _MAX_, - _Numbers_) @anchor randseq


Unify  _Numbers_ with a list of  _LENGTH_ unique random integers
in the range `[1... _MAX_)`.

</li>
 <li>randset(+ _LENGTH_, + _MAX_, - _Numbers_) @anchor randset


Unify  _Numbers_ with an ordered list of  _LENGTH_ unique random
integers in the range `[1... _MAX_)`.

</li>
 <li>setrand(+ _Key_) @anchor setrand


Use a term of the form `rand(X,Y,Z)` to set a new state for the
random number generator. The integer `X` must be in the range
`[1...30269)`, the integer `Y` must be in the range
`[1...30307)`, and the integer `Z` must be in the range
`[1...30323)`.

</li>
</ul>

@section Read_Utilities Read Utilities

The `readutil` library contains primitives to read lines, files,
multiple terms, etc.

<ul>
 <li>read_line_to_codes(+ _Stream_, - _Codes_) @anchor read_line_to_codes



Read the next line of input from  _Stream_ and unify the result with
 _Codes_ <em>after</em> the line has been read.  A line is ended by a
newline character or end-of-file. Unlike `read_line_to_codes/3`,
this predicate removes trailing newline character.

On end-of-file the atom `end_of_file` is returned.  See also
`at_end_of_stream/[0,1]`.

</li>
 <li>read_line_to_codes(+ _Stream_, - _Codes_, ? _Tail_)

Difference-list version to read an input line to a list of character
codes.  Reading stops at the newline or end-of-file character, but
unlike [read_line_to_codes/2](@ref read_line_to_codes), the newline is retained in the
output.  This predicate is especially useful for reading a block of
lines upto some delimiter.  The following example reads an HTTP header
ended by a blank line:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_header_data(Stream, Header) :-
    read_line_to_codes(Stream, Header, Tail),
    read_header_data(Header, Stream, Tail).

read_header_data("\r\n", _, _) :- !.
read_header_data("\n", _, _) :- !.
read_header_data("", _, _) :- !.
read_header_data(_, Stream, Tail) :-
    read_line_to_codes(Stream, Tail, NewTail),
    read_header_data(Tail, Stream, NewTail).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>read_stream_to_codes(+ _Stream_, - _Codes_) @anchor read_stream_to_codes


Read all input until end-of-file and unify the result to  _Codes_.

</li>
 <li>read_stream_to_codes(+ _Stream_, - _Codes_, ? _Tail_)

Difference-list version of [read_stream_to_codes/2](@ref read_stream_to_codes).

</li>
 <li>read_file_to_codes(+ _Spec_, - _Codes_, + _Options_) @anchor read_file_to_codes


Read a file to a list of character codes. Currently ignores
 _Options_.

</li>
 <li>read_file_to_terms(+ _Spec_, - _Terms_, + _Options_) @anchor read_file_to_terms


Read a file to a list of Prolog terms (see read/1). @c  _Spec_ is a






</li>
</ul>

@section RedhYBlack_Trees Red-Black Trees

Red-Black trees are balanced search binary trees. They are named because
nodes can be classified as either red or black. The code we include is
based on "Introduction to Algorithms", second edition, by Cormen,
Leiserson, Rivest and Stein.  The library includes routines to insert,
lookup and delete elements in the tree.

<ul>
 <li>rb_new(? _T_) @anchor rb_new


Create a new tree.

</li>
 <li>rb_empty(? _T_) @anchor rb_empty


Succeeds if tree  _T_ is empty.

</li>
 <li>is_rbtree(+ _T_) @anchor is_rbtree


Check whether  _T_ is a valid red-black tree.

</li>
 <li>rb_insert(+ _T0_,+ _Key_,? _Value_,+ _TF_) @anchor rb_insert


Add an element with key  _Key_ and  _Value_ to the tree
 _T0_ creating a new red-black tree  _TF_. Duplicated elements are not
allowed.

Add a new element with key  _Key_ and  _Value_ to the tree
 _T0_ creating a new red-black tree  _TF_. Fails is an element
with  _Key_ exists in the tree.

</li>
 <li>rb_lookup(+ _Key_,- _Value_,+ _T_) @anchor rb_lookup


Backtrack through all elements with key  _Key_ in the red-black tree
 _T_, returning for each the value  _Value_.

</li>
 <li>rb_lookupall(+ _Key_,- _Value_,+ _T_) @anchor rb_lookupall


Lookup all elements with key  _Key_ in the red-black tree
 _T_, returning the value  _Value_.

</li>
 <li>rb_delete(+ _T_,+ _Key_,- _TN_) @anchor rb_delete


Delete element with key  _Key_ from the tree  _T_, returning a new
tree  _TN_.

</li>
 <li>rb_delete(+ _T_,+ _Key_,- _Val_,- _TN_)

Delete element with key  _Key_ from the tree  _T_, returning the
value  _Val_ associated with the key and a new tree  _TN_.

</li>
 <li>rb_del_min(+ _T_,- _Key_,- _Val_,- _TN_) @anchor rb_del_min


Delete the least element from the tree  _T_, returning the key
 _Key_, the value  _Val_ associated with the key and a new tree
 _TN_.

</li>
 <li>rb_del_max(+ _T_,- _Key_,- _Val_,- _TN_) @anchor rb_del_max


Delete the largest element from the tree  _T_, returning the key
 _Key_, the value  _Val_ associated with the key and a new tree
 _TN_.

</li>
 <li>rb_update(+ _T_,+ _Key_,+ _NewVal_,- _TN_) @anchor rb_update


Tree  _TN_ is tree  _T_, but with value for  _Key_ associated
with  _NewVal_. Fails if it cannot find  _Key_ in  _T_.

</li>
 <li>rb_apply(+ _T_,+ _Key_,+ _G_,- _TN_) @anchor rb_apply


If the value associated with key  _Key_ is  _Val0_ in  _T_, and
if `call(G,Val0,ValF)` holds, then  _TN_ differs from
 _T_ only in that  _Key_ is associated with value  _ValF_ in
tree  _TN_. Fails if it cannot find  _Key_ in  _T_, or if
`call(G,Val0,ValF)` is not satisfiable.

</li>
 <li>rb_visit(+ _T_,- _Pairs_) @anchor rb_visit


 _Pairs_ is an infix visit of tree  _T_, where each element of
 _Pairs_ is of the form   _K_- _Val_.

</li>
 <li>rb_size(+ _T_,- _Size_) @anchor rb_size


 _Size_ is the number of elements in  _T_.

</li>
 <li>rb_keys(+ _T_,+ _Keys_) @anchor rb_keys


 _Keys_ is an infix visit with all keys in tree  _T_. Keys will be
sorted, but may be duplicate.

</li>
 <li>rb_map(+ _T_,+ _G_,- _TN_) @anchor rb_map


For all nodes  _Key_ in the tree  _T_, if the value associated with
key  _Key_ is  _Val0_ in tree  _T_, and if
`call(G,Val0,ValF)` holds, then the value associated with  _Key_
in  _TN_ is  _ValF_. Fails if or if `call(G,Val0,ValF)` is not
satisfiable for all  _Var0_.

</li>
 <li>rb_partial_map(+ _T_,+ _Keys_,+ _G_,- _TN_) @anchor rb_partial_map


For all nodes  _Key_ in  _Keys_, if the value associated with key
 _Key_ is  _Val0_ in tree  _T_, and if `call(G,Val0,ValF)`
holds, then the value associated with  _Key_ in  _TN_ is
 _ValF_. Fails if or if `call(G,Val0,ValF)` is not satisfiable
for all  _Var0_. Assumes keys are not repeated.

</li>
 <li>rb_fold(+ _T_,+ _G_,+ _Acc0_, - _AccF_) @anchor rb_fold


For all nodes  _Key_ in the tree  _T_, if the value
associated with key  _Key_ is  _V_ in tree  _T_, if
`call(G,V,Acc1,Acc2)` holds, then if  _VL_ is value of the
previous node in inorder, `call(G,VL,_,Acc0)` must hold, and if
 _VR_ is the value of the next node in inorder,
`call(G,VR,Acc1,_)` must hold.

</li>
 <li>rb_key_fold(+ _T_,+ _G_,+ _Acc0_, - _AccF_) @anchor rb_key_fold


For all nodes  _Key_ in the tree  _T_, if the value
associated with key  _Key_ is  _V_ in tree  _T_, if
`call(G,Key,V,Acc1,Acc2)` holds, then if  _VL_ is value of the
previous node in inorder, `call(G,KeyL,VL,_,Acc0)` must hold, and if
 _VR_ is the value of the next node in inorder,
`call(G,KeyR,VR,Acc1,_)` must hold.

</li>
 <li>rb_clone(+ _T_,+ _NT_,+ _Nodes_) @anchor rb_clone


``Clone'' the red-back tree into a new tree with the same keys as the
original but with all values set to unbound values. Nodes is a list
containing all new nodes as pairs  _K-V_.

</li>
 <li>rb_min(+ _T_,- _Key_,- _Value_) @anchor rb_min


 _Key_  is the minimum key in  _T_, and is associated with  _Val_.

</li>
 <li>rb_max(+ _T_,- _Key_,- _Value_) @anchor rb_max


 _Key_  is the maximal key in  _T_, and is associated with  _Val_.

</li>
 <li>rb_next(+ _T_, + _Key_,- _Next_,- _Value_) @anchor rb_next


 _Next_ is the next element after  _Key_ in  _T_, and is
associated with  _Val_.

</li>
 <li>rb_previous(+ _T_, + _Key_,- _Previous_,- _Value_) @anchor rb_previous


 _Previous_ is the previous element after  _Key_ in  _T_, and is
associated with  _Val_.

</li>
 <li>ord_list_to_rbtree(+ _L_, - _T_) @anchor list_to_rbtree


 _T_ is the red-black tree corresponding to the mapping in ordered
list  _L_.
</li>
</ul>

@section RegExp Regular Expressions

This library includes routines to determine whether a regular expression
matches part or all of a string. The routines can also return which
parts parts of the string matched the expression or subexpressions of
it. This library relies on Henry Spencer's `C`-package and is only
available in operating systems that support dynamic loading. The
`C`-code has been obtained from the sources of FreeBSD-4.0 and is
protected by copyright from Henry Spencer and from the Regents of the
University of California (see the file library/regex/COPYRIGHT for
further details).

Much of the description of regular expressions below is copied verbatim
from Henry Spencer's manual page.

A regular expression is zero or more branches, separated by ``|''.  It
matches anything that matches one of the branches.

A branch is zero or more pieces, concatenated.  It matches a match for
the first, followed by a match for the second, etc.

A piece is an atom possibly followed by ``\*'', ``+'', or ``?''.  An atom
followed by ``\*'' matches a sequence of 0 or more matches of the atom.
An atom followed by ``+'' matches a sequence of 1 or more matches of the
atom.  An atom followed by ``?'' matches a match of the atom, or the
null string.

An atom is a regular expression in parentheses (matching a match for the
regular expression), a range (see below), ``.''  (matching any single
character), ``^'' (matching the null string at the beginning of the
input string), ``$'' (matching the null string at the end of the input
string), a ``\\'' followed by a single character (matching that
character), or a single character with no other significance (matching
that character).

A range is a sequence of characters enclosed in ``[]''.  It normally
matches any single character from the sequence.  If the sequence begins
with ``^'', it matches any single character not from the rest of the
sequence.  If two characters in the sequence are separated by ``-'',
this is shorthand for the full list of ASCII characters between them
(e.g. ``[0-9]'' matches any decimal digit).  To include a literal ``]''
in the sequence, make it the first character (following a possible
``^'').  To include a literal ``-'', make it the first or last
character.

<ul>

 <li>regexp(+ _RegExp_,+ _String_,+ _Opts_) @anchor regexp



Match regular expression  _RegExp_ to input string  _String_
according to options  _Opts_. The options may be:

<ul>
 <li>`nocase`: Causes upper-case characters  in   _String_ to
be treated  as  lower case during the matching process.
</li>
</ul>

</li>
 <li>regexp(+ _RegExp_,+ _String_,+ _Opts_,? _SubMatchVars_)


Match regular expression  _RegExp_ to input string  _String_
according to options  _Opts_. The variable  _SubMatchVars_ should
be originally unbound or a list of unbound variables all will contain a
sequence of matches, that is, the head of  _SubMatchVars_ will
contain the characters in  _String_ that matched the leftmost
parenthesized subexpression within  _RegExp_, the next head of list
will contain the characters that matched the next parenthesized
subexpression to the right in  _RegExp_, and so on.

The options may be:

<ul>
 <li>`nocase`: Causes upper-case characters  in   _String_ to
be treated  as  lower case during the matching process.
</li>
 <li>`indices`: Changes what  is  stored  in
 _SubMatchVars_. Instead  of storing the matching characters from
 _String_, each variable will contain a term of the form  _IO-IF_
giving the indices in  _String_ of the first and last characters  in
the  matching range of characters.

</li>
</ul>

In general there may be more than one way to match a regular expression
to an input string.  For example,  consider the command

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  regexp("(a*)b*","aabaaabb", [], [X,Y])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Considering only the rules given so far,  _X_ and  _Y_ could end up
with the values `"aabb"` and `"aa"`, `"aaab"` and
`"aaa"`, `"ab"` and `"a"`, or any of several other
combinations.  To resolve this potential ambiguity `regexp` chooses among
alternatives using the rule ``first then longest''.  In other words, it
considers the possible matches in order working from left to right
across the input string and the pattern, and it attempts to match longer
pieces of the input string before shorter ones.  More specifically, the
following rules apply in decreasing order of priority:

<ol>
 <li>If a regular expression could match  two  different parts of an
input string then it will match the one that begins earliest.

</li>
 <li>If a regular expression contains "|"  operators  then the leftmost matching sub-expression is chosen.

</li>
 <li>In \*, +, and ? constructs, longer matches are chosen in preference to shorter ones.

</li>
 <li>In sequences of expression  components  the  components are considered from left to right.
</li>
</ol>

In the example from above, `"(a\*)b\*"` matches `"aab"`: the
`"(a\*)"` portion of the pattern is matched first and it consumes
the leading `"aa"`; then the `"b\*"` portion of the pattern
consumes the next `"b"`.  Or, consider the following example: 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  regexp("(ab|a)(b*)c",  "abc", [], [X,Y,Z])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After this command  _X_ will be `"abc"`,  _Y_ will be
`"ab"`, and  _Z_ will be an empty string.  Rule 4 specifies that
`"(ab|a)"` gets first shot at the input string and Rule 2 specifies
that the `"ab"` sub-expression is checked before the `"a"`
sub-expression.  Thus the `"b"` has already been claimed before the
`"(b\*)"` component is checked and `(b\*)` must match an empty string.

</li>
</ul>

@section shlib SWI-Prolog's shlib library

This section discusses the functionality of the (autoload)
`library(shlib)`, providing an interface to manage shared
libraries.

One of the files provides a global function `install_mylib()` that
initialises the module using calls to `PL_register_foreign()`. Here is a
simple example file `mylib.c`, which creates a Windows MessageBox:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
#include <windows.h>
#include <SWI-Prolog.h>

static foreign_t
pl_say_hello(term_t to)
{ char *a;

  if ( PL_get_atom_chars(to, &a) )
  { MessageBox(NULL, a, "DLL test", MB_OK|MB_TASKMODAL);

    PL_succeed;
  }

  PL_fail;
}

install_t
install_mylib()
{ PL_register_foreign("say_hello", 1, pl_say_hello, 0);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now write a file mylib.pl:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(mylib, [ say_hello/1 ]).
:- use_foreign_library(foreign(mylib)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The file mylib.pl can be loaded as a normal Prolog file and provides the predicate defined in C.

<ul>
 <li>load_foreign_library(: _FileSpec_) is det @anchor load_foreign_library


</li>
 <li>load_foreign_library(: _FileSpec_, + _Entry_:atom) is det

Load a shared object or DLL. After loading the  _Entry_ function is
called without arguments. The default entry function is composed
from `install_`, followed by the file base-name. E.g., the
load-call below calls the function `install_mylib()`. If the platform
prefixes extern functions with `_`, this prefix is added before
calling.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ...
          load_foreign_library(foreign(mylib)),
          ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 _FileSpec_ is a specification for
[absolute_file_name/3](@ref absolute_file_name). If searching the file fails, the plain
name is passed to the OS to try the default method of the OS for
locating foreign objects. The default definition of
[file_search_path/2](@ref file_search_path) searches \<prolog home\>/lib/Yap.

See also
`use_foreign_library/1,2` are intended for use in
directives. 

</li>
 <li>[det] use_foreign_library(+ _FileSpec_),  use_foreign_library(+ _FileSpec_, + _Entry_:atom) @anchor use_foreign_library



Load and install a foreign library as [load_foreign_library/1](@ref load_foreign_library)
and `load_foreign_library/2` and
register the installation using `initialization/2` with the option
now. This is similar to using:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    :- initialization(load_foreign_library(foreign(mylib))).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

but using the [initialization/1](@ref initialization) wrapper causes the library to
be loaded after loading of the file in which it appears is
completed, while [use_foreign_library/1](@ref use_foreign_library) loads the library
immediately. I.e. the difference is only relevant if the remainder
of the file uses functionality of the `C`-library. 

</li>
 <li>[det]unload_foreign_library(+ _FileSpec_)
</li>
 <li>[det]unload_foreign_library(+ _FileSpec_, + _Exit_:atom)  @anchor unload_foreign_library




Unload a shared
object or DLL. After calling the  _Exit_ function, the shared object is
removed from the process. The default exit function is composed from
`uninstall_`, followed by the file base-name.

</li>
 <li>current_foreign_library(? _File_, ? _Public_)  @anchor current_foreign_library



Query currently
loaded shared libraries.  

</li>
</ul>

@section Splay_Trees Splay Trees

Splay trees are explained in the paper "Self-adjusting Binary Search
Trees", by D.D. Sleator and R.E. Tarjan, JACM, vol. 32, No.3, July 1985,
p. 668. They are designed to support fast insertions, deletions and
removals in binary search trees without the complexity of traditional
balanced trees. The key idea is to allow the tree to become
unbalanced. To make up for this, whenever we find a node, we move it up
to the top. We use code by Vijay Saraswat originally posted to the Prolog
mailing-list.

<ul>

 <li>splay_access(- _Return_,+ _Key_,? _Val_,+ _Tree_,- _NewTree_) @anchor splay_access


If item  _Key_ is in tree  _Tree_, return its  _Val_ and
unify  _Return_ with `true`. Otherwise unify  _Return_ with
`null`. The variable  _NewTree_ unifies with the new tree.

</li>
 <li>splay_delete(+ _Key_,? _Val_,+ _Tree_,- _NewTree_) @anchor splay_delete


Delete item  _Key_ from tree  _Tree_, assuming that it is present
already. The variable  _Val_ unifies with a value for key  _Key_,
and the variable  _NewTree_ unifies with the new tree. The predicate
will fail if  _Key_ is not present.

</li>
 <li>splay_init(- _NewTree_) @anchor splay_init


Initialize a new splay tree.

</li>
 <li>splay_insert(+ _Key_,? _Val_,+ _Tree_,- _NewTree_) @anchor splay_insert


Insert item  _Key_ in tree  _Tree_, assuming that it is not
there already. The variable  _Val_ unifies with a value for key
 _Key_, and the variable  _NewTree_ unifies with the new
tree. In our implementation,  _Key_ is not inserted if it is
already there: rather it is unified with the item already in the tree.

</li>
 <li>splay_join(+ _LeftTree_,+ _RighTree_,- _NewTree_) @anchor splay_join


Combine trees  _LeftTree_ and  _RighTree_ into a single
tree _NewTree_ containing all items from both trees. This operation
assumes that all items in  _LeftTree_ are less than all those in
 _RighTree_ and destroys both  _LeftTree_ and  _RighTree_.

</li>
 <li>splay_split(+ _Key_,? _Val_,+ _Tree_,- _LeftTree_,- _RightTree_) @anchor splay_split


Construct and return two trees  _LeftTree_ and  _RightTree_,
where  _LeftTree_ contains all items in  _Tree_ less than
 _Key_, and  _RightTree_ contains all items in  _Tree_
greater than  _Key_. This operations destroys  _Tree_.

</li>
</ul>

@section String_InputOutput Reading From and Writing To Strings

From Version 4.3.2 onwards YAP implements SICStus Prolog compatible
String Input/Output. The library allows users to read from and write to a memory
buffer as if it was a file. The memory buffer is built from or converted
to a string of character codes by the routines in library. Therefore, if
one wants to read from a string the string must be fully instantiated
before the library built-in opens the string for reading. These commands
are available through the `use_module(library(charsio))` command.

<ul>

 <li>format_to_chars(+ _Form_, + _Args_, - _Result_) @anchor format_to_chars



Execute the built-in procedure [format/2](@ref format) with form  _Form_ and
arguments  _Args_ outputting the result to the string of character
codes  _Result_.

</li>
 <li>format_to_chars(+ _Form_, + _Args_, - _Result_, - _Result0_)


Execute the built-in procedure [format/2](@ref format) with form  _Form_ and
arguments  _Args_ outputting the result to the difference list of
character codes  _Result-Result0_.

</li>
 <li>write_to_chars(+ _Term_, - _Result_) @anchor write_to_chars



Execute the built-in procedure [write/1](@ref write) with argument  _Term_
outputting the result to the string of character codes  _Result_.

</li>
 <li>write_to_chars(+ _Term_, - _Result0_, - _Result_)


Execute the built-in procedure [write/1](@ref write) with argument  _Term_
outputting the result to the difference list of character codes
 _Result-Result0_.

</li>
 <li>atom_to_chars(+ _Atom_, - _Result_) @anchor atom_to_chars



Convert the atom  _Atom_ to the string of character codes
 _Result_.

</li>
 <li>atom_to_chars(+ _Atom_, - _Result0_, - _Result_)


Convert the atom  _Atom_ to the difference list of character codes
 _Result-Result0_.

</li>
 <li>number_to_chars(+ _Number_, - _Result_) @anchor number_to_chars



Convert the number  _Number_ to the string of character codes
 _Result_.

</li>
 <li>number_to_chars(+ _Number_, - _Result0_, - _Result_)


Convert the atom  _Number_ to the difference list of character codes
 _Result-Result0_.

</li>
 <li>atom_to_term(+ _Atom_, - _Term_, - _Bindings_) @anchor atom_to_term


Use  _Atom_ as input to [read_term/2](@ref read_term) using the option `variable_names` and return the read term in  _Term_ and the variable bindings in  _Bindings_.  _Bindings_ is a list of `Name = Var` couples, thus providing access to the actual variable names. See also [read_term/2](@ref read_term). If Atom has no valid syntax, a syntax_error exception is raised.

</li>
 <li>term_to_atom(? _Term_, ? _Atom_) @anchor term_to_atom


True if  _Atom_ describes a term that unifies with  _Term_. When
 _Atom_ is instantiated  _Atom_ is converted and then unified with
 _Term_. If  _Atom_ has no valid syntax, a syntax_error exception
is raised. Otherwise  _Term_ is ``written'' on  _Atom_ using
[write_term/2](@ref write_term) with the option quoted(true).

</li>
 <li>read_from_chars(+ _Chars_, - _Term_) @anchor read_from_chars



Parse the list of character codes  _Chars_ and return the result in
the term  _Term_. The character codes to be read must terminate with
a dot character such that either (i) the dot character is followed by
blank characters; or (ii) the dot character is the last character in the
string.

</li>
 <li>open_chars_stream(+ _Chars_, - _Stream_) @anchor open_chars_stream



Open the list of character codes  _Chars_ as a stream  _Stream_.

</li>
 <li>with_output_to_chars(? _Goal_, - _Chars_) @anchor with_output_to_chars



Execute goal  _Goal_ such that its standard output will be sent to a
memory buffer. After successful execution the contents of the memory
buffer will be converted to the list of character codes  _Chars_.

</li>
 <li>with_output_to_chars(? _Goal_, ? _Chars0_, - _Chars_)


Execute goal  _Goal_ such that its standard output will be sent to a
memory buffer. After successful execution the contents of the memory
buffer will be converted to the difference list of character codes
 _Chars-Chars0_.

</li>
 <li>with_output_to_chars(? _Goal_, - _Stream_, ? _Chars0_, - _Chars_)


Execute goal  _Goal_ such that its standard output will be sent to a
memory buffer. After successful execution the contents of the memory
buffer will be converted to the difference list of character codes
 _Chars-Chars0_ and  _Stream_ receives the stream corresponding to
the memory buffer.

</li>
</ul>

The implementation of the character IO operations relies on three YAP
built-ins:

<ul>

 <li>charsio:open_mem_read_stream(+ _String_, - _Stream_)
Store a string in a memory buffer and output a stream that reads from this
memory buffer.

</li>
 <li>charsio:open_mem_write_stream(- _Stream_)
Create a new memory buffer and output a stream that writes to  it.

</li>
 <li>charsio:peek_mem_write_stream(- _Stream_, L0, L)
Convert the memory buffer associated with stream  _Stream_ to the
difference list of character codes  _L-L0_.

</li>
</ul>
These built-ins are initialized to belong to the module `charsio` in
`init.yap`. Novel procedures for manipulating strings by explicitly
importing these built-ins.

YAP does not currently support opening a `charsio` stream in
`append` mode, or seeking in such a stream.

@section System Calling The Operating System from YAP

YAP now provides a library of system utilities compatible with the
SICStus Prolog system library. This library extends and to some point
replaces the functionality of Operating System access routines. The
library includes Unix/Linux and Win32 `C` code. They
are available through the `use_module(library(system))` command.

<ul>

 <li>datime(datime(- _Year_, - _Month_, - _DayOfTheMonth_, @anchor datime

- _Hour_, - _Minute_, - _Second_)

The [datime/1](@ref datime) procedure returns the current date and time, with
information on  _Year_,  _Month_,  _DayOfTheMonth_,
 _Hour_,  _Minute_, and  _Second_. The  _Hour_ is returned
on local time. This function uses the WIN32
`GetLocalTime` function or the Unix `localtime` function.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- datime(X).

X = datime(2001,5,28,15,29,46) ? 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>mktime(datime(+ _Year_, + _Month_, + _DayOfTheMonth_, @anchor mktime

+ _Hour_, + _Minute_, + _Second_), - _Seconds_)

The `mktime/1` procedure returns the number of  _Seconds_
elapsed since 00:00:00 on January 1, 1970, Coordinated Universal Time
(UTC).  The user provides information on  _Year_,  _Month_,
 _DayOfTheMonth_,  _Hour_,  _Minute_, and  _Second_. The
 _Hour_ is given on local time. This function uses the WIN32
`GetLocalTime` function or the Unix `mktime` function.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- mktime(datime(2001,5,28,15,29,46),X).

X = 991081786 ? ;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>delete_file(+ _File_) @anchor delete_file


The [delete_file/1](@ref delete_file) procedure removes file  _File_. If
 _File_ is a directory, remove the directory <em>and all its subdirectories</em>.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- delete_file(x).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>delete_file(+ _File_,+ _Opts_)

The `delete_file/2` procedure removes file  _File_ according to
options  _Opts_. These options are `directory` if one should
remove directories, `recursive` if one should remove directories
recursively, and `ignore` if errors are not to be reported.

This example is equivalent to using the [delete_file/1](@ref delete_file) predicate:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- delete_file(x, [recursive]).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>directory_files(+ _Dir_,+ _List_) @anchor directory_files


Given a directory  _Dir_,  [directory_files/2](@ref directory_files) procedures a
listing of all files and directories in the directory:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ?- directory_files('.',L), writeq(L).
['Makefile.~1~','sys.so','Makefile','sys.o',x,..,'.']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The predicates uses the `dirent` family of routines in Unix
environments, and `findfirst` in WIN32.

</li>
 <li>file_exists(+ _File_) @anchor file_exists


The atom  _File_ corresponds to an existing file.

</li>
 <li>file_exists(+ _File_,+ _Permissions_)

The atom  _File_ corresponds to an existing file with permissions
compatible with  _Permissions_. YAP currently only accepts for
permissions to be described as a number. The actual meaning of this
number is Operating System dependent.

</li>
 <li>file_property(+ _File_,? _Property_) @anchor file_property


The atom  _File_ corresponds to an existing file, and  _Property_
will be unified with a property of this file. The properties are of the
form `type( _Type_)`, which gives whether the file is a regular
file, a directory, a fifo file, or of unknown type;
`size( _Size_)`, with gives the size for a file, and
`mod_time( _Time_)`, which gives the last time a file was
modified according to some Operating System dependent
timestamp; `mode( _mode_)`, gives the permission flags for the
file, and `linkto( _FileName_)`, gives the file pointed to by a
symbolic link. Properties can be obtained through backtracking:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- file_property('Makefile',P).

P = type(regular) ? ;

P = size(2375) ? ;

P = mod_time(990826911) ? ;

no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>make_directory(+ _Dir_) @anchor make_directory


Create a directory  _Dir_. The name of the directory must be an atom.

</li>
 <li>rename_file(+ _OldFile_,+ _NewFile_) @anchor rename_file


Create file  _OldFile_ to  _NewFile_. This predicate uses the
`C` built-in function `rename`.

</li>
 <li>environ(? _EnvVar_,+ _EnvValue_) @anchor sys_environ


Unify environment variable  _EnvVar_ with its value  _EnvValue_,
if there is one. This predicate is backtrackable in Unix systems, but
not currently in Win32 configurations.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- environ('HOME',X).

X = 'C:\\cygwin\\home\\administrator' ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>host_id(- _Id_) @anchor host_id



Unify  _Id_ with an identifier of the current host. YAP uses the
`hostid` function when available, 

</li>
 <li>host_name(- _Name_) @anchor host_name



Unify  _Name_ with a name for the current host. YAP uses the
`hostname` function in Unix systems when available, and the
`GetComputerName` function in WIN32 systems. 

</li>
 <li>kill( _Id_,+ _SIGNAL_) @anchor kill



Send signal  _SIGNAL_ to process  _Id_. In Unix this predicate is
a direct interface to `kill` so one can send signals to groups of
processes. In WIN32 the predicate is an interface to
`TerminateProcess`, so it kills  _Id_ independently of  _SIGNAL_.

</li>
 <li>mktemp( _Spec_,- _File_) @anchor mktemp



Direct interface to `mktemp`: given a  _Spec_, that is a file
name with six  _X_ to it, create a file name  _File_. Use
[tmpnam/1](@ref tmpnam) instead.

</li>
 <li>pid(- _Id_) @anchor pid



Unify  _Id_ with the process identifier for the current
process. An interface to the <tt>getpid</tt> function.

</li>
 <li>tmpnam(- _File_) @anchor tmpnam



Interface with  _tmpnam_: obtain a new, unique file name  _File_.

</li>
 <li>tmp_file(- _File_) @anchor tmp_file



Create a name for a temporary file.  _Base_ is an user provided
identifier for the category of file. The  _TmpName_ is guaranteed to
be unique. If the system halts, it will automatically remove all created
temporary files.

</li>
 <li>exec(+ _Command_,[+ _InputStream_,+ _OutputStream_,+ _ErrorStream_],- _PID_) @anchor exec


Execute command  _Command_ with its streams connected to
 _InputStream_,  _OutputStream_, and  _ErrorStream_. The
process that executes the command is returned as  _PID_. The
command is executed by the default shell `bin/sh -c` in Unix.

The following example demonstrates the use of [exec/3](@ref exec) to send a
command and process its output:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exec(ls,[std,pipe(S),null],P),repeat, get0(S,C), (C = -1, close(S) ! ; put(C)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The streams may be one of standard stream, `std`, null stream,
`null`, or `pipe(S)`, where  _S_ is a pipe stream. Note
that it is up to the user to close the pipe.

</li>
 <li>popen(+ _Command_, + _TYPE_, - _Stream_) @anchor popen


Interface to the <tt>popen</tt> function. It opens a process by creating a
pipe, forking and invoking  _Command_ on the current shell. Since a
pipe is by definition unidirectional the  _Type_ argument may be
`read` or `write`, not both. The stream should be closed
using [close/1](@ref close), there is no need for a special `pclose`
command.

The following example demonstrates the use of [popen/3](@ref popen) to process
the output of a command, as [exec/3](@ref exec) would do:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
   ?- popen(ls,read,X),repeat, get0(X,C), (C = -1, ! ; put(C)).

X = 'C:\\cygwin\\home\\administrator' ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The WIN32 implementation of [popen/3](@ref popen) relies on [exec/3](@ref exec).

</li>
 <li>shell @anchor shell


Start a new shell and leave YAP in background until the shell
completes. YAP uses the shell given by the environment variable
`SHELL`. In WIN32 environment YAP will use `COMSPEC` if
`SHELL` is undefined.

</li>
 <li>shell(+ _Command_)

Execute command  _Command_ under a new shell. YAP will be in
background until the command completes. In Unix environments YAP uses
the shell given by the environment variable `SHELL` with the option
`" -c "`. In WIN32 environment YAP will use `COMSPEC` if
`SHELL` is undefined, in this case with the option `" /c "`.

</li>
 <li>shell(+ _Command_,- _Status_)

Execute command  _Command_ under a new shell and unify  _Status_
with the exit for the command. YAP will be in background until the
command completes. In Unix environments YAP uses the shell given by the
environment variable `SHELL` with the option `" -c "`. In
WIN32 environment YAP will use `COMSPEC` if `SHELL` is
undefined, in this case with the option `" /c "`.

</li>
 <li>sleep(+ _Time_) @anchor sleep


Block the current thread for  _Time_ seconds. When YAP is compiled 
without multi-threading support, this predicate blocks the YAP process. 
The number of seconds must be a positive number, and it may an integer 
or a float. The Unix implementation uses `usleep` if the number of 
seconds is below one, and `sleep` if it is over a second. The WIN32 
implementation uses `Sleep` for both cases.

</li>
 <li>system

Start a new default shell and leave YAP in background until the shell
completes. YAP uses `/bin/sh` in Unix systems and `COMSPEC` in
WIN32.

</li>
 <li>system(+ _Command_,- _Res_)

Interface to `system`: execute command  _Command_ and unify
 _Res_ with the result.

</li>
 <li>wait(+ _PID_,- _Status_) @anchor wait


Wait until process  _PID_ terminates, and return its exits  _Status_.

</li>
</ul>

@section Terms Utilities On Terms

The next routines provide a set of commonly used utilities to manipulate
terms. Most of these utilities have been implemented in `C` for
efficiency. They are available through the
`use_module(library(terms))` command.

<ul>

 <li>cyclic_term(? _Term_) @anchor cyclic_term


Succeed if the argument  _Term_ is not a cyclic term.

</li>
 <li>term_hash(+ _Term_, ? _Hash_) @anchor term_hash



If  _Term_ is ground unify  _Hash_ with a positive integer
calculated from the structure of the term. Otherwise the argument
 _Hash_ is left unbound. The range of the positive integer is from
`0` to, but not including, `33554432`.

</li>
 <li>term_hash(+ _Term_, + _Depth_, + _Range_, ? _Hash_)


Unify  _Hash_ with a positive integer calculated from the structure
of the term.  The range of the positive integer is from `0` to, but
not including,  _Range_. If  _Depth_ is `-1` the whole term
is considered. Otherwise, the term is considered only up to depth
`1`, where the constants and the principal functor have depth
`1`, and an argument of a term with depth  _I_ has depth  _I+1_. 

</li>
 <li>variables_within_term(+ _Variables_,? _Term_, - _OutputVariables_) @anchor variables_within_term



Unify  _OutputVariables_ with the subset of the variables  _Variables_ that occurs in  _Term_.

</li>
 <li>new_variables_in_term(+ _Variables_,? _Term_, - _OutputVariables_) @anchor new_variables_in_term



Unify  _OutputVariables_ with all variables occurring in  _Term_ that are not in the list  _Variables_.

</li>
 <li>variant(? _Term1_, ? _Term2_) @anchor variant



Succeed if  _Term1_ and  _Term2_ are variant terms.

</li>
 <li>subsumes(? _Term1_, ? _Term2_) @anchor subsumes



Succeed if  _Term1_ subsumes  _Term2_.  Variables in term
 _Term1_ are bound so that the two terms become equal.

</li>
 <li>subsumes_chk(? _Term1_, ? _Term2_) @anchor subsumes_chk



Succeed if  _Term1_ subsumes  _Term2_ but does not bind any
variable in  _Term1_.

</li>
 <li>variable_in_term(? _Term_,? _Var_) @anchor variable_in_term


Succeed if the second argument  _Var_ is a variable and occurs in
term  _Term_.

</li>
 <li>unifiable(? _Term1_, ? _Term2_, - _Bindings_) @anchor unifiable



Succeed if  _Term1_ and  _Term2_ are unifiable with substitution
 _Bindings_.

</li>
</ul>

@section Tries Trie DataStructure

The next routines provide a set of utilities to create and manipulate
prefix trees of Prolog terms. Tries were originally proposed to
implement tabling in Logic Programming, but can be used for other
purposes. The tries will be stored in the Prolog database and can seen
as alternative to `assert` and `record` family of
primitives. Most of these utilities have been implemented in `C`
for efficiency. They are available through the
`use_module(library(tries))` command.

<ul>
 <li>trie_open(- _Id_) @anchor trie_open



Open a new trie with identifier  _Id_.

</li>
 <li>trie_close(+ _Id_) @anchor trie_close



Close trie with identifier  _Id_.

</li>
 <li>trie_close_all @anchor trie_close_all



Close all available tries.

</li>
 <li>trie_mode(? _Mode_) @anchor trie_mode



Unify  _Mode_ with trie operation mode. Allowed values are either
`std` (default) or `rev`.

</li>
 <li>trie_put_entry(+ _Trie_,+ _Term_,- _Ref_) @anchor trie_put_entry



Add term  _Term_ to trie  _Trie_. The handle  _Ref_ gives
a reference to the term.

</li>
 <li>trie_check_entry(+ _Trie_,+ _Term_,- _Ref_) @anchor trie_check_entry



Succeeds if a variant of term  _Term_ is in trie  _Trie_. An handle
 _Ref_ gives a reference to the term.

</li>
 <li>trie_get_entry(+ _Ref_,- _Term_) @anchor trie_get_entry


Unify  _Term_ with the entry for handle  _Ref_.

</li>
 <li>trie_remove_entry(+ _Ref_) @anchor trie_remove_entry



Remove entry for handle  _Ref_.

</li>
 <li>trie_remove_subtree(+ _Ref_) @anchor trie_remove_subtree



Remove subtree rooted at handle  _Ref_.

</li>
 <li>trie_save(+ _Trie_,+ _FileName_) @anchor trie_save


Dump trie  _Trie_ into file  _FileName_.

</li>
 <li>trie_load(+ _Trie_,+ _FileName_) @anchor trie_load


Load trie  _Trie_ from the contents of file  _FileName_.

</li>
 <li>trie_stats(- _Memory_,- _Tries_,- _Entries_,- _Nodes_) @anchor trie_stats


Give generic statistics on tries, including the amount of memory,
 _Memory_, the number of tries,  _Tries_, the number of entries,
 _Entries_, and the total number of nodes,  _Nodes_.

</li>
 <li>trie_max_stats(- _Memory_,- _Tries_,- _Entries_,- _Nodes_) @anchor trie_max_stats


Give maximal statistics on tries, including the amount of memory,
 _Memory_, the number of tries,  _Tries_, the number of entries,
 _Entries_, and the total number of nodes,  _Nodes_.

</li>
 <li>trie_usage(+ _Trie_,- _Entries_,- _Nodes_,- _VirtualNodes_) @anchor trie_usage


Give statistics on trie  _Trie_, the number of entries,
 _Entries_, and the total number of nodes,  _Nodes_, and the
number of  _VirtualNodes_.

</li>
 <li>trie_print(+ _Trie_) @anchor trie_print


Print trie  _Trie_ on standard output.

</li>
</ul>

@section Cleanup Call Cleanup

<tt>call_cleanup/1</tt> and <tt>call_cleanup/2</tt> allow predicates to register
code for execution after the call is finished. Predicates can be
declared to be <tt>fragile</tt> to ensure that <tt>call_cleanup</tt> is called
for any Goal which needs it. This library is loaded with the
`use_module(library(cleanup))` command.

<ul>
 <li>:- fragile  _P_,...., _Pn_ @anchor fragile


Declares the predicate  _P_=<tt>[module:]name/arity</tt> as a fragile
predicate, module is optional, default is the current
typein_module. Whenever such a fragile predicate is used in a query
it will be called through call_cleanup/1.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
:- fragile foo/1,bar:baz/2.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>call_cleanup(: _Goal_) @anchor call_cleanup


Execute goal  _Goal_ within a cleanup-context. Called predicates
might register cleanup Goals which are called right after the end of
the call to  _Goal_. Cuts and exceptions inside Goal do not prevent the
execution of the cleanup calls. <tt>call_cleanup</tt> might be nested.

</li>
 <li>call_cleanup(: _Goal_, : _CleanUpGoal_)

This is similar to <tt>call_cleanup/1</tt> with an additional
 _CleanUpGoal_ which gets called after  _Goal_ is finished.

</li>
 <li>setup_call_cleanup(: _Setup_,: _Goal_, : _CleanUpGoal_) @anchor setup_call_cleanup


Calls `(Setup, Goal)`. For each sucessful execution of  _Setup_, calling  _Goal_, the
cleanup handler  _Cleanup_ is guaranteed to be called exactly once.
This will happen after  _Goal_ completes, either through failure,
deterministic success, commit, or an exception.   _Setup_ will
contain the goals that need to be protected from asynchronous interrupts
such as the ones received from `call_with_time_limit/2` or [thread_signal/2](@ref thread_signal).  In
most uses,  _Setup_ will perform temporary side-effects required by
 _Goal_ that are finally undone by  _Cleanup_.

Success or failure of  _Cleanup_ is ignored and choice-points it
created are destroyed (as [once/1](@ref once)). If  _Cleanup_ throws an exception,
this is executed as normal.

Typically, this predicate is used to cleanup permanent data storage
required to execute  _Goal_, close file-descriptors, etc. The example
below provides a non-deterministic search for a term in a file, closing
the stream as needed.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
term_in_file(Term, File) :-
    setup_call_cleanup(open(File, read, In),
               term_in_stream(Term, In),
               close(In) ).

term_in_stream(Term, In) :-
    repeat,
    read(In, T),
    (   T == end_of_file
    ->  !, fail
    ;   T = Term
    ).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that it is impossible to implement this predicate in Prolog other than
by reading all terms into a list, close the file and call [member/2](@ref member).
Without [setup_call_cleanup/3](@ref setup_call_cleanup) there is no way to gain control if the
choice-point left by `repeat` is removed by a cut or an exception.

`setup_call_cleanup/2` can also be used to test determinism of a goal:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- setup_call_cleanup(true,(X=1;X=2), Det=yes).

X = 1 ;

X = 2,
Det = yes ;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This predicate is under consideration for inclusion into the ISO standard.
For compatibility with other Prolog implementations see `call_cleanup/2`.

</li>
 <li>setup_call_catcher_cleanup(: _Setup_,: _Goal_, + _Catcher_,: _CleanUpGoal_) @anchor setup_call_catcher_cleanup


Similar to `setup_call_cleanup( _Setup_,  _Goal_,  _Cleanup_)` with
additional information on the reason of calling  _Cleanup_.  Prior
to calling  _Cleanup_,  _Catcher_ unifies with the termination
code.  If this unification fails,  _Cleanup_ is
 *not* called.

</li>
 <li>on_cleanup(+ _CleanUpGoal_) @anchor on_cleanup


Any Predicate might registers a  _CleanUpGoal_. The
 _CleanUpGoal_ is put onto the current cleanup context. All such
CleanUpGoals are executed in reverse order of their registration when
the surrounding cleanup-context ends. This call will throw an exception
if a predicate tries to register a  _CleanUpGoal_ outside of any
cleanup-context.

</li>
 <li>cleanup_all @anchor cleanup_all


Calls all pending CleanUpGoals and resets the cleanup-system to an
initial state. Should only be used as one of the last calls in the
main program.

</li>
</ul>

There are some private predicates which could be used in special
cases, such as manually setting up cleanup-contexts and registering
CleanUpGoals for other than the current cleanup-context.
Read the Source Luke.

@section Timeout Calls With Timeout

The <tt>time_out/3</tt> command relies on the <tt>alarm/3</tt> built-in to
implement a call with a maximum time of execution. The command is
available with the `use_module(library(timeout))` command.

<ul>

 <li>time_out(+ _Goal_, + _Timeout_, - _Result_) @anchor time_out


Execute goal  _Goal_ with time limited  _Timeout_, where
 _Timeout_ is measured in milliseconds. If the goal succeeds, unify
 _Result_ with success. If the timer expires before the goal
terminates, unify  _Result_ with <tt>time_out</tt>.

This command is implemented by activating an alarm at procedure
entry. If the timer expires before the goal completes, the alarm will
throw an exception  _timeout_.

One should note that [time_out/3](@ref time_out) is not reentrant, that is, a goal
called from `time_out` should never itself call
[time_out/3](@ref time_out). Moreover, [time_out/3](@ref time_out) will deactivate any previous
alarms set by [alarm/3](@ref alarm) and vice-versa, hence only one of these
calls should be used in a program.

Last, even though the timer is set in milliseconds, the current
implementation relies on <tt>alarm/3</tt>, and therefore can only offer
precision on the scale of seconds.

</li>
</ul>

@section Trees Updatable Binary Trees

The following queue manipulation routines are available once
included with the `use_module(library(trees))` command.

<ul>

 <li>get_label(+ _Index_, + _Tree_, ? _Label_) @anchor get_label


Treats the tree as an array of  _N_ elements and returns the
 _Index_-th.

</li>
 <li>list_to_tree(+ _List_, - _Tree_) @anchor list_to_tree


Takes a given  _List_ of  _N_ elements and constructs a binary
 _Tree_.

</li>
 <li>map_tree(+ _Pred_, + _OldTree_, - _NewTree_) @anchor map_tree


Holds when  _OldTree_ and  _NewTree_ are binary trees of the same shape
and `Pred(Old,New)` is true for corresponding elements of the two trees.

</li>
 <li>put_label(+ _Index_, + _OldTree_, + _Label_, - _NewTree_) @anchor put_label


constructs a new tree the same shape as the old which moreover has the
same elements except that the  _Index_-th one is  _Label_.

</li>
 <li>tree_size(+ _Tree_, - _Size_) @anchor tree_size


Calculates the number of elements in the  _Tree_.

</li>
 <li>tree_to_list(+ _Tree_, - _List_) @anchor tree_to_list


Is the converse operation to list_to_tree.

</li>
</ul>

@section UGraphs Unweighted Graphs

The following graph manipulation routines are based in code originally
written by Richard O'Keefe. The code was then extended to be compatible
with the SICStus Prolog ugraphs library. The routines assume directed
graphs, undirected graphs may be implemented by using two edges. Graphs
are represented in one of two ways:

<ul>
 <li>The P-representation of a graph is a list of (from-to) vertex
pairs, where the pairs can be in any old order.  This form is
convenient for input/output.

</li>
 <li>The S-representation of a graph is a list of (vertex-neighbors)
pairs, where the pairs are in standard order (as produced by keysort)
and the neighbors of each vertex are also in standard order (as
produced by sort).  This form is convenient for many calculations.
</li>
</ul>

These built-ins are available once included with the
`use_module(library(ugraphs))` command.

<ul>

 <li>vertices_edges_to_ugraph(+ _Vertices_, + _Edges_, - _Graph_) @anchor vertices_edges_to_ugraph


Given a graph with a set of vertices  _Vertices_ and a set of edges
 _Edges_,  _Graph_ must unify with the corresponding
s-representation. Note that the vertices without edges will appear in
 _Vertices_ but not in  _Edges_. Moreover, it is sufficient for a
vertex to appear in  _Edges_.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- vertices_edges_to_ugraph([],[1-3,2-4,4-5,1-5],L).

L = [1-[3,5],2-[4],3-[],4-[5],5-[]] ? 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In this case all edges are defined implicitly. The next example shows
three unconnected edges:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- vertices_edges_to_ugraph([6,7,8],[1-3,2-4,4-5,1-5],L).

L = [1-[3,5],2-[4],3-[],4-[5],5-[],6-[],7-[],8-[]] ? 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>vertices(+ _Graph_, - _Vertices_) @anchor vertices


Unify  _Vertices_ with all vertices appearing in graph
 _Graph_. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- vertices([1-[3,5],2-[4],3-[],4-[5],5-[]], V).

L = [1,2,3,4,5]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>edges(+ _Graph_, - _Edges_) @anchor edges


Unify  _Edges_ with all edges appearing in graph
 _Graph_. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- vertices([1-[3,5],2-[4],3-[],4-[5],5-[]], V).

L = [1,2,3,4,5]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>add_vertices(+ _Graph_, + _Vertices_, - _NewGraph_) @anchor add_vertices


Unify  _NewGraph_ with a new graph obtained by adding the list of
vertices  _Vertices_ to the graph  _Graph_. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- add_vertices([1-[3,5],2-[4],3-[],4-[5],
                 5-[],6-[],7-[],8-[]],
                [0,2,9,10,11],
                   NG).

NG = [0-[],1-[3,5],2-[4],3-[],4-[5],5-[],
      6-[],7-[],8-[],9-[],10-[],11-[]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>del_vertices(+ _Graph_, + _Vertices_, - _NewGraph_) @anchor del_vertices


Unify  _NewGraph_ with a new graph obtained by deleting the list of
vertices  _Vertices_ and all the edges that start from or go to a
vertex in  _Vertices_ to the graph  _Graph_. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- del_vertices([2,1],[1-[3,5],2-[4],3-[],
                 4-[5],5-[],6-[],7-[2,6],8-[]],NL).

NL = [3-[],4-[5],5-[],6-[],7-[6],8-[]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>add_edges(+ _Graph_, + _Edges_, - _NewGraph_) @anchor add_edges


Unify  _NewGraph_ with a new graph obtained by adding the list of
edges  _Edges_ to the graph  _Graph_. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- add_edges([1-[3,5],2-[4],3-[],4-[5],5-[],6-[],
              7-[],8-[]],[1-6,2-3,3-2,5-7,3-2,4-5],NL).

NL = [1-[3,5,6],2-[3,4],3-[2],4-[5],5-[7],6-[],7-[],8-[]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>del_edges(+ _Graph_, + _Edges_, - _NewGraph_) @anchor del_edges


Unify  _NewGraph_ with a new graph obtained by removing the list of
edges  _Edges_ from the graph  _Graph_. Notice that no vertices
are deleted. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- del_edges([1-[3,5],2-[4],3-[],4-[5],5-[],
              6-[],7-[],8-[]],
             [1-6,2-3,3-2,5-7,3-2,4-5,1-3],NL).

NL = [1-[5],2-[4],3-[],4-[],5-[],6-[],7-[],8-[]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>transpose(+ _Graph_, - _NewGraph_) @anchor transpose


Unify  _NewGraph_ with a new graph obtained from  _Graph_ by
replacing all edges of the form  _V1-V2_ by edges of the form
 _V2-V1_. The cost is `O(|V|^2)`. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- transpose([1-[3,5],2-[4],3-[],
              4-[5],5-[],6-[],7-[],8-[]], NL).

NL = [1-[],2-[],3-[1],4-[2],5-[1,4],6-[],7-[],8-[]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that an undirected graph is its own transpose.

</li>
 <li>neighbors(+ _Vertex_, + _Graph_, - _Vertices_) @anchor neighbors


Unify  _Vertices_ with the list of neighbors of vertex  _Vertex_
in  _Graph_. If the vertice is not in the graph fail. In the next
example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- neighbors(4,[1-[3,5],2-[4],3-[],
                4-[1,2,7,5],5-[],6-[],7-[],8-[]],
             NL).

NL = [1,2,7,5]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>neighbours(+ _Vertex_, + _Graph_, - _Vertices_) @anchor neighbours


Unify  _Vertices_ with the list of neighbours of vertex  _Vertex_
in  _Graph_. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- neighbours(4,[1-[3,5],2-[4],3-[],
                 4-[1,2,7,5],5-[],6-[],7-[],8-[]], NL).

NL = [1,2,7,5]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>complement(+ _Graph_, - _NewGraph_) @anchor complement


Unify  _NewGraph_ with the graph complementary to  _Graph_.
In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- complement([1-[3,5],2-[4],3-[],
               4-[1,2,7,5],5-[],6-[],7-[],8-[]], NL).

NL = [1-[2,4,6,7,8],2-[1,3,5,6,7,8],3-[1,2,4,5,6,7,8],
      4-[3,5,6,8],5-[1,2,3,4,6,7,8],6-[1,2,3,4,5,7,8],
      7-[1,2,3,4,5,6,8],8-[1,2,3,4,5,6,7]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>compose(+ _LeftGraph_, + _RightGraph_, - _NewGraph_) @anchor compose


Compose the graphs  _LeftGraph_ and  _RightGraph_ to form  _NewGraph_.
In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- compose([1-[2],2-[3]],[2-[4],3-[1,2,4]],L).

L = [1-[4],2-[1,2,4],3-[]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>top_sort(+ _Graph_, - _Sort_) @anchor top_sort


Generate the set of nodes  _Sort_ as a topological sorting of graph
 _Graph_, if one is possible.
In the next example we show how topological sorting works for a linear graph:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- top_sort([_138-[_219],_219-[_139], _139-[]],L).

L = [_138,_219,_139]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>top_sort(+ _Graph_, - _Sort0_, - _Sort_)

Generate the difference list  _Sort_- _Sort0_ as a topological
sorting of graph  _Graph_, if one is possible.

</li>
 <li>transitive_closure(+ _Graph_, + _Closure_) @anchor transitive_closure


Generate the graph  _Closure_ as the transitive closure of graph
 _Graph_.
In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- transitive_closure([1-[2,3],2-[4,5],4-[6]],L).

L = [1-[2,3,4,5,6],2-[4,5,6],4-[6]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>reachable(+ _Node_, + _Graph_, - _Vertices_) @anchor reachable


Unify  _Vertices_ with the set of all vertices in graph
 _Graph_ that are reachable from  _Node_. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- reachable(1,[1-[3,5],2-[4],3-[],4-[5],5-[]],V).

V = [1,3,5]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
</ul>

@section DGraphs Directed Graphs

The following graph manipulation routines use the red-black tree library
to try to avoid linear-time scans of the graph for all graph
operations. Graphs are represented as a red-black tree, where the key is
the vertex, and the associated value is a list of vertices reachable
from that vertex through an edge (ie, a list of edges). 

<ul>

 <li>dgraph_new(+ _Graph_) @anchor dgraph_new


Create a new directed graph. This operation must be performed before
trying to use the graph.

</li>
 <li>dgraph_vertices(+ _Graph_, - _Vertices_) @anchor dgraph_vertices


Unify  _Vertices_ with all vertices appearing in graph
 _Graph_.

</li>
 <li>dgraph_edge(+ _N1_, + _N2_, + _Graph_) @anchor dgraph_edge


Edge  _N1_- _N2_ is an edge in directed graph  _Graph_.

</li>
 <li>dgraph_edges(+ _Graph_, - _Edges_) @anchor dgraph_edges


Unify  _Edges_ with all edges appearing in graph
 _Graph_.

</li>
 <li>dgraph_add_vertices(+ _Graph_, + _Vertex_, - _NewGraph_) @anchor dgraph_add_vertex


Unify  _NewGraph_ with a new graph obtained by adding
vertex  _Vertex_ to the graph  _Graph_.

</li>
 <li>dgraph_add_vertices(+ _Graph_, + _Vertices_, - _NewGraph_) @anchor dgraph_add_vertices


Unify  _NewGraph_ with a new graph obtained by adding the list of
vertices  _Vertices_ to the graph  _Graph_.

</li>
 <li>dgraph_del_vertex(+ _Graph_, + _Vertex_, - _NewGraph_) @anchor dgraph_del_vertex


Unify  _NewGraph_ with a new graph obtained by deleting vertex
 _Vertex_ and all the edges that start from or go to  _Vertex_ to
the graph  _Graph_.

</li>
 <li>dgraph_del_vertices(+ _Graph_, + _Vertices_, - _NewGraph_) @anchor dgraph_del_vertices


Unify  _NewGraph_ with a new graph obtained by deleting the list of
vertices  _Vertices_ and all the edges that start from or go to a
vertex in  _Vertices_ to the graph  _Graph_.

</li>
 <li>dgraph_add_edge(+ _Graph_, + _N1_, + _N2_, - _NewGraph_) @anchor dgraph_add_edge


Unify  _NewGraph_ with a new graph obtained by adding the edge
 _N1_- _N2_ to the graph  _Graph_.

</li>
 <li>dgraph_add_edges(+ _Graph_, + _Edges_, - _NewGraph_) @anchor dgraph_add_edges


Unify  _NewGraph_ with a new graph obtained by adding the list of
edges  _Edges_ to the graph  _Graph_.

</li>
 <li>dgraph_del_edge(+ _Graph_, + _N1_, + _N2_, - _NewGraph_) @anchor dgraph_del_edge


Succeeds if  _NewGraph_ unifies with a new graph obtained by
removing the edge  _N1_- _N2_ from the graph  _Graph_. Notice
that no vertices are deleted.

</li>
 <li>dgraph_del_edges(+ _Graph_, + _Edges_, - _NewGraph_) @anchor dgraph_del_edges


Unify  _NewGraph_ with a new graph obtained by removing the list of
edges  _Edges_ from the graph  _Graph_. Notice that no vertices
are deleted.

</li>
 <li>dgraph_to_ugraph(+ _Graph_, - _UGraph_) @anchor dgraph_to_ugraph


Unify  _UGraph_ with the representation used by the  _ugraphs_
unweighted graphs library, that is, a list of the form
 _V-Neighbors_, where  _V_ is a node and  _Neighbors_ the nodes
children.

</li>
 <li>ugraph_to_dgraph( + _UGraph_, - _Graph_) @anchor ugraph_to_dgraph


Unify  _Graph_ with the directed graph obtain from  _UGraph_,
represented in the form used in the  _ugraphs_ unweighted graphs
library.

</li>
 <li>dgraph_neighbors(+ _Vertex_, + _Graph_, - _Vertices_) @anchor dgraph_neighbors


Unify  _Vertices_ with the list of neighbors of vertex  _Vertex_
in  _Graph_. If the vertice is not in the graph fail.

</li>
 <li>dgraph_neighbours(+ _Vertex_, + _Graph_, - _Vertices_) @anchor dgraph_neighbours


Unify  _Vertices_ with the list of neighbours of vertex  _Vertex_
in  _Graph_.

</li>
 <li>dgraph_complement(+ _Graph_, - _NewGraph_) @anchor dgraph_complement


Unify  _NewGraph_ with the graph complementary to  _Graph_.

</li>
 <li>dgraph_transpose(+ _Graph_, - _Transpose_) @anchor dgraph_transpose


Unify  _NewGraph_ with a new graph obtained from  _Graph_ by
replacing all edges of the form  _V1-V2_ by edges of the form
 _V2-V1_. 

</li>
 <li>dgraph_compose(+ _Graph1_, + _Graph2_, - _ComposedGraph_) @anchor dgraph_compose


Unify  _ComposedGraph_ with a new graph obtained by composing
 _Graph1_ and  _Graph2_, ie,  _ComposedGraph_ has an edge
 _V1-V2_ iff there is a  _V_ such that  _V1-V_ in  _Graph1_
and  _V-V2_ in  _Graph2_.

</li>
 <li>dgraph_transitive_closure(+ _Graph_, - _Closure_) @anchor dgraph_transitive_closure


Unify  _Closure_ with the transitive closure of graph  _Graph_.

</li>
 <li>dgraph_symmetric_closure(+ _Graph_, - _Closure_) @anchor dgraph_symmetric_closure


Unify  _Closure_ with the symmetric closure of graph  _Graph_,
that is,  if  _Closure_ contains an edge  _U-V_ it must also
contain the edge  _V-U_.

</li>
 <li>dgraph_top_sort(+ _Graph_, - _Vertices_) @anchor dgraph_top_sort


Unify  _Vertices_ with the topological sort of graph  _Graph_.

</li>
 <li>dgraph_top_sort(+ _Graph_, - _Vertices_, ? _Vertices0_)

Unify the difference list  _Vertices_- _Vertices0_ with the
topological sort of graph  _Graph_.

</li>
 <li>dgraph_min_path(+ _V1_, + _V1_, + _Graph_, - _Path_, ? _Costt_) @anchor dgraph_min_path


Unify the list  _Path_ with the minimal cost path between nodes
 _N1_ and  _N2_ in graph  _Graph_. Path  _Path_ has cost
 _Cost_.

</li>
 <li>dgraph_max_path(+ _V1_, + _V1_, + _Graph_, - _Path_, ? _Costt_) @anchor dgraph_max_path


Unify the list  _Path_ with the maximal cost path between nodes
 _N1_ and  _N2_ in graph  _Graph_. Path  _Path_ has cost
 _Cost_.

</li>
 <li>dgraph_min_paths(+ _V1_, + _Graph_, - _Paths_) @anchor dgraph_min_paths


Unify the list  _Paths_ with the minimal cost paths from node
 _N1_ to the nodes in graph  _Graph_.

</li>
 <li>dgraph_isomorphic(+ _Vs_, + _NewVs_, + _G0_, - _GF_) @anchor dgraph_isomorphic


Unify the list  _GF_ with the graph isomorphic to  _G0_ where 
vertices in  _Vs_ map to vertices in  _NewVs_.

</li>
 <li>dgraph_path(+ _Vertex_, + _Graph_, ? _Path_) @anchor dgraph_path


The path  _Path_ is a path starting at vertex  _Vertex_ in graph
 _Graph_.

</li>
 <li>dgraph_path(+ _Vertex_, + _Vertex1_, + _Graph_, ? _Path_)

The path  _Path_ is a path starting at vertex  _Vertex_ in graph
 _Graph_ and ending at path  _Vertex2_.

</li>
 <li>dgraph_reachable(+ _Vertex_, + _Graph_, ? _Edges_) @anchor dgraph_reachable


The path  _Path_ is a path starting at vertex  _Vertex_ in graph
 _Graph_.

</li>
 <li>dgraph_leaves(+ _Graph_, ? _Vertices_) @anchor dgraph_leaves


The vertices  _Vertices_ have no outgoing edge in graph
 _Graph_.

</li>
</ul>

@section UnDGraphs Undirected Graphs

The following graph manipulation routines use the red-black tree graph
library to implement undirected graphs. Mostly, this is done by having
two directed edges per undirected edge.

<ul>

 <li>undgraph_new(+ _Graph_) @anchor undgraph_new


Create a new directed graph. This operation must be performed before
trying to use the graph.

</li>
 <li>undgraph_vertices(+ _Graph_, - _Vertices_) @anchor undgraph_vertices


Unify  _Vertices_ with all vertices appearing in graph
 _Graph_.

</li>
 <li>undgraph_edge(+ _N1_, + _N2_, + _Graph_) @anchor undgraph_edge


Edge  _N1_- _N2_ is an edge in undirected graph  _Graph_.

</li>
 <li>undgraph_edges(+ _Graph_, - _Edges_) @anchor undgraph_edges


Unify  _Edges_ with all edges appearing in graph
 _Graph_.

</li>
 <li>undgraph_add_vertices(+ _Graph_, + _Vertices_, - _NewGraph_) @anchor undgraph_add_vertices


Unify  _NewGraph_ with a new graph obtained by adding the list of
vertices  _Vertices_ to the graph  _Graph_.

</li>
 <li>undgraph_del_vertices(+ _Graph_, + _Vertices_, - _NewGraph_) @anchor undgraph_del_vertices


Unify  _NewGraph_ with a new graph obtained by deleting the list of
vertices  _Vertices_ and all the edges that start from or go to a
vertex in  _Vertices_ to the graph  _Graph_.

</li>
 <li>undgraph_add_edges(+ _Graph_, + _Edges_, - _NewGraph_) @anchor undgraph_add_edges


Unify  _NewGraph_ with a new graph obtained by adding the list of
edges  _Edges_ to the graph  _Graph_.

</li>
 <li>undgraph_del_edges(+ _Graph_, + _Edges_, - _NewGraph_) @anchor undgraph_del_edges


Unify  _NewGraph_ with a new graph obtained by removing the list of
edges  _Edges_ from the graph  _Graph_. Notice that no vertices
are deleted.

</li>
 <li>undgraph_neighbors(+ _Vertex_, + _Graph_, - _Vertices_) @anchor undgraph_neighbors


Unify  _Vertices_ with the list of neighbors of vertex  _Vertex_
in  _Graph_. If the vertice is not in the graph fail.

</li>
 <li>undgraph_neighbours(+ _Vertex_, + _Graph_, - _Vertices_) @anchor undgraph_neighbours


Unify  _Vertices_ with the list of neighbours of vertex  _Vertex_
in  _Graph_.

</li>
 <li>undgraph_complement(+ _Graph_, - _NewGraph_) @anchor undgraph_complement


Unify  _NewGraph_ with the graph complementary to  _Graph_.

</li>
 <li>dgraph_to_undgraph( + _DGraph_, - _UndGraph_) @anchor dgraph_to_undgraph


Unify  _UndGraph_ with the undirected graph obtained from the
directed graph  _DGraph_.

</li>
</ul>

@section DBUsage Memory Usage in Prolog Data-Base

This library provides a set of utilities for studying memory usage in YAP.
The following routines are available once included with the
`use_module(library(dbusage))` command.

<ul>
 <li>db_usage @anchor db_usage


Give general overview of data-base usage in the system.

</li>
 <li>db_static @anchor db_static


List memory usage for every static predicate.

</li>
 <li>db_static(+ _Threshold_)

List memory usage for every static predicate. Predicate must use more
than  _Threshold_ bytes.

</li>
 <li>db_dynamic @anchor db_dynamic


List memory usage for every dynamic predicate.

</li>
 <li>db_dynamic(+ _Threshold_)

List memory usage for every dynamic predicate. Predicate must use more
than  _Threshold_ bytes.

</li>
</ul>

@section Lambda Lambda Expressions

This library, designed and implemented by Ulrich Neumerkel, provides
lambda expressions to simplify higher order programming based on `call/N`.

Lambda expressions are represented by ordinary Prolog terms.  There are
two kinds of lambda expressions:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Free+\X1^X2^ ..^XN^Goal

         \X1^X2^ ..^XN^Goal
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The second is a shorthand for` t+\\X1^X2^..^XN^Goal`, where `Xi` are the parameters.

 _Goal_ is a goal or continuation (Syntax note:  _Operators_ within  _Goal_
require parentheses due to the low precedence of the `^` operator).

Free contains variables that are valid outside the scope of the lambda
expression. They are thus free variables within.

All other variables of  _Goal_ are considered local variables. They must
not appear outside the lambda expression. This restriction is
currently not checked. Violations may lead to unexpected bindings.

In the following example the parentheses around `X\>3` are necessary.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- use_module(library(lambda)).
?- use_module(library(apply)).

?- maplist(\X^(X>3),[4,5,9]).
true.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the following  _X_ is a variable that is shared by both instances
of the lambda expression. The second query illustrates the cooperation
of continuations and lambdas. The lambda expression is in this case a
continuation expecting a further argument.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- Xs = [A,B], maplist(X+\Y^dif(X,Y), Xs).
Xs = [A, B],
dif(X, A),
dif(X, B).

?- Xs = [A,B], maplist(X+\dif(X), Xs).
Xs = [A, B],
dif(X, A),
dif(X, B).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following queries are all equivalent. To see this, use
the fact `f(x,y)`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- call(f,A1,A2).
?- call(\X^f(X),A1,A2).
?- call(\X^Y^f(X,Y), A1,A2).                                                                                                            
?- call(\X^(X+\Y^f(X,Y)), A1,A2).
?- call(call(f, A1),A2).
?- call(f(A1),A2).
?- f(A1,A2).
A1 = x,
A2 = y.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Further discussions
at Ulrich Neumerker's page in <http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/ISO-Hiord>.

@section LAM LAM

This library provides a set of utilities for interfacing with LAM MPI.
The following routines are available once included with the
`use_module(library(lam_mpi))` command. The yap should be
invoked using the LAM mpiexec or mpirun commands (see LAM manual for
more details).

<ul>
 <li>mpi_init @anchor mpi_init


Sets up the mpi environment. This predicate should be called before any other MPI predicate.

</li>
 <li>mpi_finalize @anchor mpi_finalize


Terminates the MPI execution environment. Every process must call this predicate before  exiting.

</li>
 <li>mpi_comm_size(- _Size_) @anchor mpi_comm_size


Unifies  _Size_ with the number of processes in the MPI environment.

</li>
 <li>mpi_comm_rank(- _Rank_) @anchor mpi_comm_rank


Unifies  _Rank_ with the rank of the current process in the MPI environment.

</li>
 <li>mpi_version(- _Major_,- _Minor_) @anchor mpi_version


Unifies  _Major_ and  _Minor_ with, respectively, the major and minor version of the MPI.

</li>
 <li>mpi_send(+ _Data_,+ _Dest_,+ _Tag_) @anchor mpi_send



Blocking communication predicate. The message in  _Data_, with tag
 _Tag_, is sent immediately to the processor with rank  _Dest_.
The predicate succeeds after the message being sent.

</li>
 <li>mpi_isend(+ _Data_,+ _Dest_,+ _Tag_,- _Handle_) @anchor mpi_isend



Non blocking communication predicate. The message in  _Data_, with
tag  _Tag_, is sent whenever possible to the processor with rank
 _Dest_. An  _Handle_ to the message is returned to be used to
check for the status of the message, using the `mpi_wait` or
`mpi_test` predicates. Until `mpi_wait` is called, the
memory allocated for the buffer containing the message is not
released.

</li>
 <li>mpi_recv(? _Source_,? _Tag_,- _Data_) @anchor mpi_recv



Blocking communication predicate. The predicate blocks until a message
is received from processor with rank  _Source_ and tag  _Tag_.
The message is placed in  _Data_.

</li>
 <li>mpi_irecv(? _Source_,? _Tag_,- _Handle_) @anchor mpi_irecv



Non-blocking communication predicate. The predicate returns an
 _Handle_ for a message that will be received from processor with
rank  _Source_ and tag  _Tag_. Note that the predicate succeeds
immediately, even if no message has been received. The predicate
`mpi_wait_recv` should be used to obtain the data associated to
the handle.

</li>
 <li>mpi_wait_recv(? _Handle_,- _Status_,- _Data_) @anchor mpi_wait_recv



Completes a non-blocking receive operation. The predicate blocks until
a message associated with handle  _Hanlde_ is buffered. The
predicate succeeds unifying  _Status_ with the status of the
message and  _Data_ with the message itself. 

</li>
 <li>mpi_test_recv(? _Handle_,- _Status_,- _Data_) @anchor mpi_test_recv



Provides information regarding a handle. If the message associated
with handle  _Hanlde_ is buffered then the predicate succeeds
unifying  _Status_ with the status of the message and  _Data_
with the message itself. Otherwise, the predicate fails.

</li>
 <li>mpi_wait(? _Handle_,- _Status_) @anchor mpi_wait



Completes a non-blocking operation. If the operation was a
`mpi_send`, the predicate blocks until the message is buffered
or sent by the runtime system. At this point the send buffer is
released. If the operation was a `mpi_recv`, it waits until the
message is copied to the receive buffer.  _Status_ is unified with
the status of the message.

</li>
 <li>mpi_test(? _Handle_,- _Status_) @anchor mpi_test



Provides information regarding the handle  _Handle_, ie., if a
communication operation has been completed.  If the operation
associate with  _Hanlde_ has been completed the predicate succeeds
with the completion status in  _Status_, otherwise it fails.

</li>
 <li>mpi_barrier @anchor mpi_barrier



Collective communication predicate.  Performs a barrier
synchronization among all processes. Note that a collective
communication means that all processes call the same predicate. To be
able to use a regular `mpi_recv` to receive the messages, one
should use `mpi_bcast2`.

</li>
 <li>mpi_bcast2(+ _Root_, ? _Data_) @anchor mpi_bcast



Broadcasts the message  _Data_ from the process with rank  _Root_
to all other processes.

</li>
 <li>mpi_bcast3(+ _Root_, + _Data_, + _Tag_)


Broadcasts the message  _Data_ with tag  _Tag_ from the process with rank  _Root_
to all other processes.

</li>
 <li>mpi_ibcast(+ _Root_, + _Data_, + _Tag_) @anchor mpi_ibcast



Non-blocking operation. Broadcasts the message  _Data_ with tag  _Tag_
from the process with rank  _Root_ to all other processes.

</li>
 <li>mpi_default_buffer_size(- _OldBufferSize_, ? _NewBufferSize_) @anchor mpi_default_buffer_size



The  _OldBufferSize_ argument unifies with the current size of the
MPI communication buffer size and sets the communication buffer size
 _NewBufferSize_. The buffer is used for assynchronous waiting and
for broadcast receivers. Notice that buffer is local at each MPI
process.

</li>
 <li>mpi_msg_size( _Msg_, - _MsgSize_) @anchor mpi_msg_size


Unify  _MsgSize_ with the number of bytes YAP would need to send the
message  _Msg_.

</li>
 <li>mpi_gc @anchor mpi_gc



Attempts to perform garbage collection with all the open handles
associated with send and non-blocking broadcasts. For each handle it
tests it and the message has been delivered the handle and the buffer
are released.

</li>
</ul>

@section BDDs Binary Decision Diagrams and Friends

This library provides an interface to the BDD package CUDD. It requires
CUDD compiled as a dynamic library. In Linux this is available out of
box in Fedora, but can easily be ported to other Linux
distributions. CUDD is available in the ports OSX package, and in
cygwin. To use it, call `:-use_module(library(bdd))`.

The following predicates construct a BDD:

<ul>
 <li>bbd_new(? _Exp_, - _BddHandle_) @anchor bdd_new

create a new BDD from the logical expression  _Exp_. The expression
may include:

<ul>
 <li>Logical Variables:
a leaf-node can be a logical variable.
</li>
 <li>Constants 0 and 1
a leaf-node can also be one of these two constants.
</li>
 <li>or( _X_,  _Y_),  _X_ \\/  _Y_,  _X_ +  _Y_
disjunction
</li>
 <li>and( _X_,  _Y_),  _X_ /\\  _Y_,  _X_ \*  _Y_
conjunction
</li>
 <li>nand( _X_,  _Y_)
negated conjunction@
</li>
 <li>nor( _X_,  _Y_)
negated disjunction
</li>
 <li>xor( _X_,  _Y_)
exclusive or
</li>
 <li>not( _X_), - _X_
negation
</li>
</ul>

</li>
 <li>bdd_from_list(? _List_, - _BddHandle_) @anchor bdd_from_list

Convert a  _List_ of logical expressions of the form above into a BDD
accessible through  _BddHandle_.

</li>
 <li>mtbdd_new(? _Exp_, - _BddHandle_) @anchor mtbdd_new

create a new algebraic decision diagram (ADD) from the logical
expression  _Exp_. The expression may include:

<ul>
 <li>Logical Variables:
a leaf-node can be a logical variable, or <em>parameter</em>.
</li>
 <li>Number
a leaf-node can also be any number
</li>
 <li>_X_ \*  _Y_
product
</li>
 <li>_X_ +  _Y_
sum
</li>
 <li>_X_ -  _Y_
subtraction
</li>
 <li>or( _X_,  _Y_),  _X_ \\/  _Y_
logical or
</li>
</ul>

</li>
 <li>bdd_tree(+ _BDDHandle_,  _Term_) @anchor bdd_tree

Convert the BDD or ADD represented by  _BDDHandle_ to a Prolog term
of the form `bdd( _Dir_,  _Nodes_,  _Vars_)` or `mtbdd( _Nodes_,  _Vars_)`, respectively. The arguments are:

<ul>
 <li>
 _Dir_ direction of the BDD, usually 1
</li>
 <li>
 _Nodes_ list of nodes in the BDD or ADD. 

In a BDD nodes may be <tt>pp</tt> (both terminals are positive) or <tt>pn</tt>
(right-hand-side is negative), and have four arguments: a logical
variable that will be bound to the value of the node, the logical
variable corresponding to the node, a logical variable, a 0 or a 1 with
the value of the left-hand side, and a logical variable, a 0 or a 1
with the right-hand side.

</li>
 <li>
 _Vars_ are the free variables in the original BDD, or the parameters of the BDD/ADD.
</li>
</ul>
As an example, the BDD for the expression `X+(Y+X)\*(-Z)` becomes:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bdd(1,[pn(N2,X,1,N1),pp(N1,Y,N0,1),pn(N0,Z,1,1)],vs(X,Y,Z))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>bdd_eval(+ _BDDHandle_,  _Val_) @anchor bdd_eval

Unify  _Val_ with the value of the logical expression compiled in
 _BDDHandle_ given an assignment to its  variables.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bdd_new(X+(Y+X)*(-Z), BDD), 
[X,Y,Z] = [0,0,0], 
bdd_eval(BDD, V), 
writeln(V).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
would write 0 in the standard output stream.

The  Prolog code equivalent to <tt>bdd_eval/2</tt> is:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Tree = bdd(1, T, _Vs),
    reverse(T, RT),
    foldl(eval_bdd, RT, _, V).

eval_bdd(pp(P,X,L,R), _, P) :-
    P is ( X/\L ) \/ ( (1-X) /\ R ).
eval_bdd(pn(P,X,L,R), _, P) :-
    P is ( X/\L ) \/ ( (1-X) /\ (1-R) ).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
First, the nodes are reversed to implement bottom-up evaluation. Then,
we use the `foldl` list manipulation predicate to walk every node,
computing the disjunction of the two cases and binding the output
variable. The top node gives the full expression value. Notice that
`(1- _X_)`  implements negation.

</li>
 <li>bdd_size(+ _BDDHandle_, - _Size_) @anchor bdd_size

Unify  _Size_ with the number of nodes in  _BDDHandle_.

</li>
 <li>bdd_print(+ _BDDHandle_, + _File_) @anchor bdd_print

Output bdd  _BDDHandle_ as a dot file to  _File_.

</li>
 <li>bdd_to_probability_sum_product(+ _BDDHandle_, - _Prob_) @anchor bdd_to_probability_sum_product

Each node in a BDD is given a probability  _Pi_. The total
probability of a corresponding sum-product network is  _Prob_.

</li>
 <li>bdd_to_probability_sum_product(+ _BDDHandle_, - _Probs_, - _Prob_)
Each node in a BDD is given a probability  _Pi_. The total
probability of a corresponding sum-product network is  _Prob_, and
the probabilities of the inner nodes are  _Probs_.

In Prolog, this predicate would correspond to computing the value of a
BDD. The input variables will be bound to probabilities, eg
`[ _X_, _Y_, _Z_] = [0.3.0.7,0.1]`, and the previous
`eval_bdd` would operate over real numbers:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Tree = bdd(1, T, _Vs),
    reverse(T, RT),
    foldl(eval_prob, RT, _, V).

eval_prob(pp(P,X,L,R), _, P) :-
    P is  X * L +  (1-X) * R.
eval_prob(pn(P,X,L,R), _, P) :-
    P is  X * L + (1-X) * (1-R).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
</li>
 <li>bdd_close( _BDDHandle_) @anchor bdd_close

close the BDD and release any resources it holds.

</li>
</ul>

@section Block_Diagram Block Diagram

This library provides a way of visualizing a prolog program using
modules with blocks.  To use it use:
`:-use_module(library(block_diagram))`.

<ul>
 <li>make_diagram(+inputfilename, +ouputfilename) @anchor make_diagram



This will crawl the files following the use_module, ensure_loaded directives withing the inputfilename.
The result will be a file in dot format.
You can make a pdf at the shell by asking `dot -Tpdf filename \> output.pdf`.

</li>
 <li>make_diagram(+inputfilename, +ouputfilename, +predicate, +depth, +extension)


The same as [make_diagram/2](@ref make_diagram) but you can define how many of the imported/exporeted predicates will be shown with predicate, and how deep the crawler is allowed to go with depth. The extension is used if the file use module directives do not include a file extension.

</li>
</ul>

@page SWIhYProlog_Emulation SWI-Prolog Emulation

This library provides a number of SWI-Prolog builtins that are not by
default in YAP. This support is loaded with the
`expects_dialect(swi)` command.

<ul>

 <li>append(? _List1_,? _List2_,? _List3_) @anchor swi_append


Succeeds when  _List3_ unifies with the concatenation of  _List1_
and  _List2_. The predicate can be used with any instantiation
pattern (even three variables).

</li>
 <li>between(+ _Low_,+ _High_,? _Value_) @anchor swi_between



 _Low_ and  _High_ are integers,  _High_ less or equal than
 _Low_. If  _Value_ is an integer,  _Low_ less or equal than
 _Value_ less or equal than  _High_.  When  _Value_ is a
variable it is successively bound to all integers between  _Low_ and
 _High_.  If  _High_ is `inf`, [between/3](@ref between) is true iff
 _Value_ less or equal than  _Low_, a feature that is particularly
interesting for generating integers from a certain value.

</li>
 <li>chdir(+ _Dir_) @anchor chdir



Compatibility predicate.  New code should use [working_directory/2](@ref working_directory).

</li>
 <li>concat_atom(+ _List_,- _Atom_) @anchor concat_atom



 _List_ is a list of atoms, integers or floating point numbers. Succeeds
if  _Atom_ can be unified with the concatenated elements of  _List_. If
 _List_ has exactly 2 elements it is equivalent to `atom_concat/3`,
allowing for variables in the list.

</li>
 <li>concat_atom(? _List_,+ _Separator_,? _Atom_)


Creates an atom just like concat_atom/2, but inserts  _Separator_
between each pair of atoms.  For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- concat_atom([gnu, gnat], ', ', A).

A = 'gnu, gnat'
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(Unimplemented) This predicate can also be used to split atoms by
instantiating  _Separator_ and  _Atom_:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- concat_atom(L, -, 'gnu-gnat').

L = [gnu, gnat]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>nth1(+ _Index_,? _List_,? _Elem_) @anchor swi_nth1


Succeeds when the  _Index_-th element of  _List_ unifies with
 _Elem_. Counting starts at 1.

Set environment variable.   _Name_ and  _Value_ should be
instantiated to atoms or integers.  The environment variable will be
passed to `shell/[0-2]` and can be requested using `getenv/2`.
They also influence [expand_file_name/2](@ref expand_file_name).

</li>
 <li>setenv(+ _Name_,+ _Value_) @anchor swi_setenv


Set environment variable.   _Name_ and  _Value_ should be
instantiated to atoms or integers.  The environment variable will be
passed to `shell/[0-2]` and can be requested using `getenv/2`.
They also influence [expand_file_name/2](@ref expand_file_name).

</li>
 <li>term_to_atom(? _Term_,? _Atom_) @anchor swi_term_to_atom


Succeeds if  _Atom_ describes a term that unifies with  _Term_. When
 _Atom_ is instantiated  _Atom_ is converted and then unified with
 _Term_.  If  _Atom_ has no valid syntax, a `syntax_error`
exception is raised. Otherwise  _Term_ is ``written'' on  _Atom_
using [write/1](@ref write).

</li>
 <li>working_directory(- _Old_,+ _New_) @anchor swi_working_directory



Unify  _Old_ with an absolute path to the current working directory
and change working directory to  _New_.  Use the pattern
`working_directory(CWD, CWD)` to get the current directory.  See
also `absolute_file_name/2` and [chdir/1](@ref chdir).

</li>
 <li>@ _Term1_ =@= @ _Term2_ @anchor qQaAaAqQ



True iff  _Term1_ and  _Term2_ are structurally equivalent. I.e. if  _Term1_ and  _Term2_ are variants of each other.

</li>
</ul>

@section Invoking_Predicates_on_all_Members_of_a_List Invoking Predicates on all Members of a List


All the predicates in this section call a predicate on all members of a
list or until the predicate called fails.  The predicate is called via
`call/[2..]`, which implies common arguments can be put in
front of the arguments obtained from the list(s). For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- maplist(plus(1), [0, 1, 2], X).

X = [1, 2, 3]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

we will phrase this as `` _Predicate_ is applied on ...''

<ul>

 <li>maplist(+ _Pred_,+ _List_) @anchor swi_maplist


 _Pred_ is applied successively on each element of  _List_ until
the end of the list or  _Pred_ fails. In the latter case 
`maplist/2` fails.

</li>
 <li>maplist(+ _Pred_,+ _List1_,+ _List2_)

Apply  _Pred_ on all successive pairs of elements from
 _List1_ and
 _List2_. Fails if  _Pred_ can not be applied to a
pair. See the example above.

</li>
 <li>maplist(+ _Pred_,+ _List1_,+ _List2_,+ _List4_)

Apply  _Pred_ on all successive triples of elements from  _List1_,
 _List2_ and  _List3_. Fails if  _Pred_ can not be applied to a
triple. See the example above.

</li>
</ul>

@section Forall Forall			


<ul>
 <li>forall(+ _Cond_,+ _Action_) @anchor swi_forall




For all alternative bindings of  _Cond_  _Action_ can be proven.
The next example verifies that all arithmetic statements in the list
 _L_ are correct. It does not say which is wrong if one proves wrong.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- forall(member(Result = Formula, [2 = 1 + 1, 4 = 2 * 2]),
                 Result =:= Formula).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
</ul>

@page SWIhYProlog_Global_Variables SWI Global variables


SWI-Prolog global variables are associations between names (atoms) and
terms.  They differ in various ways from storing information using
[assert/1](@ref assert) or [recorda/3](@ref recorda).

<ul>
 <li>The value lives on the Prolog (global) stack.  This implies 
that lookup time is independent from the size of the term.
This is particulary interesting for large data structures
such as parsed XML documents or the CHR global constraint
store.

</li>
 <li>They support both global assignment using [nb_setval/2](@ref nb_setval) and
backtrackable assignment using [b_setval/2](@ref b_setval).

</li>
 <li>Only one value (which can be an arbitrary complex Prolog
term) can be associated to a variable at a time.

</li>
 <li>Their value cannot be shared among threads.  Each thread
has its own namespace and values for global variables.

</li>
 <li>Currently global variables are scoped globally.  We may
consider module scoping in future versions.
</li>
</ul>

Both [b_setval/2](@ref b_setval) and [nb_setval/2](@ref nb_setval) implicitly create a variable if the
referenced name does not already refer to a variable.

Global variables may be initialised from directives to make them
available during the program lifetime, but some considerations are
necessary for saved-states and threads. Saved-states to not store global
variables, which implies they have to be declared with [initialization/1](@ref initialization)
to recreate them after loading the saved state.  Each thread has
its own set of global variables, starting with an empty set.  Using
`thread_inititialization/1` to define a global variable it will be
defined, restored after reloading a saved state and created in all
threads that are created <em>after</em> the registration.

<ul>
 <li>b_setval(+ _Name_,+ _Value_) @anchor swi_b_setval


Associate the term  _Value_ with the atom  _Name_ or replaces
the currently associated value with  _Value_.  If  _Name_ does
not refer to an existing global variable a variable with initial value
`[]` is created (the empty list).  On backtracking the
assignment is reversed.

</li>
 <li>b_getval(+ _Name_,- _Value_) @anchor swi_b_getval


Get the value associated with the global variable  _Name_ and unify
it with  _Value_. Note that this unification may further instantiate
the value of the global variable. If this is undesirable the normal
precautions (double negation or [copy_term/2](@ref copy_term)) must be taken. The
[b_getval/2](@ref b_getval) predicate generates errors if  _Name_ is not an atom or
the requested variable does not exist.

</li>
 <li>nb_setval(+ _Name_,+ _Value_) @anchor swi_nb_setval


Associates a copy of  _Value_ created with [duplicate_term/2](@ref duplicate_term)
with the atom  _Name_.  Note that this can be used to set an
initial value other than `[]` prior to backtrackable assignment.

</li>
 <li>nb_getval(+ _Name_,- _Value_) @anchor swi_nb_getval


The [nb_getval/2](@ref nb_getval) predicate is a synonym for b_getval/2, introduced for
compatibility and symmetry.  As most scenarios will use a particular
global variable either using non-backtrackable or backtrackable
assignment, using [nb_getval/2](@ref nb_getval) can be used to document that the 
variable is used non-backtrackable.

</li>
 <li>nb_current(? _Name_,? _Value_) @anchor swi_nb_current


Enumerate all defined variables with their value. The order of
enumeration is undefined.

</li>
 <li>nb_delete(? _Name_)

Delete the named global variable.
</li>
</ul>

@section Compatibility_of_Global_Variables Compatibility of Global Variables

Global variables have been introduced by various Prolog
implementations recently.  YAP follows their implementation in SWI-Prolog, itself
based on hProlog by Bart Demoen. Jan and Bart
decided that the semantics if hProlog [nb_setval/2](@ref nb_setval), which is
equivalent to [nb_linkval/2](@ref nb_linkval) is not acceptable for normal Prolog
users as the behaviour is influenced by how builtin predicates
constructing terms ([read/1](@ref read), [=../2](@ref qQdOdO), etc.) are implemented.

GNU-Prolog provides a rich set of global variables, including arrays.
Arrays can be implemented easily in SWI-Prolog using [functor/3](@ref functor) and
`setarg/3` due to the unrestricted arity of compound terms.

@page Extensions Extensions to Prolog

YAP includes a number of extensions over the original Prolog
language. Next, we discuss support to the most important ones.

@section Rational_Trees Rational Trees

Prolog unification is not a complete implementation. For efficiency
considerations, Prolog systems do not perform occur checks while
unifying terms. As an example, `X = a(X)` will not fail but instead
will create an infinite term of the form `a(a(a(a(a(...)))))`, or
<em>rational tree</em>.

Rational trees are now supported by default in YAP. In previous
versions, this was not the default and these terms could easily lead
to infinite computation. For example, `X = a(X), X = X` would
enter an infinite loop.

The `RATIONAL_TREES` flag improves support for these
terms. Internal primitives are now aware that these terms can exist, and
will not enter infinite loops. Hence, the previous unification will
succeed. Another example, `X = a(X), ground(X)` will succeed
instead of looping. Other affected built-ins include the term comparison
primitives, [numbervars/3](@ref numbervars), [copy_term/2](@ref copy_term), and the internal
data base routines. The support does not extend to Input/Output routines
or to [assert/1](@ref assert) YAP does not allow directly reading
rational trees, and you need to use `write_depth/2` to avoid
entering an infinite cycle when trying to write an infinite term.

@section CohYroutining Co-routining

Prolog uses a simple left-to-right flow of control. It is sometimes
convenient to change this control so that goals will only be executed
when conditions are fulfilled. This may result in a more "data-driven"
execution, or may be necessary to correctly implement extensions such as
negation by default.

The `COROUTINING` flag enables this option. Note that the support for
coroutining  will in general slow down execution.

The following declaration is supported:

<ul>
 <li>block/1
The argument to `block/1` is a condition on a goal or a conjunction
of conditions, with each element separated by commas. Each condition is
of the form `predname( _C1_,..., _CN_)`, where  _N_ is the
arity of the goal, and each  _CI_ is of the form `-`, if the
argument must suspend until the first such variable is bound, or
`?`, otherwise.

</li>
 <li>wait/1
The argument to `wait/1` is a predicate descriptor or a conjunction
of these predicates. These predicates will suspend until their first
argument is bound.
</li>
</ul>

The following primitives are supported:

<ul>
 <li>dif( _X_, _Y_) @anchor dif


Succeed if the two arguments do not unify. A call to [dif/2](@ref dif) will
suspend if unification may still succeed or fail, and will fail if they
always unify.

</li>
 <li>freeze(? _X_,: _G_) @anchor freeze


Delay execution of goal  _G_ until the variable  _X_ is bound.

</li>
 <li>frozen( _X_, _G_) @anchor frozen


Unify  _G_ with a conjunction of goals suspended on variable  _X_,
or `true` if no goal has suspended.

</li>
 <li>when(+ _C_,: _G_) @anchor when


Delay execution of goal  _G_ until the conditions  _C_ are
satisfied. The conditions are of the following form:

<ul>
 <li>_C1_, _C2_
Delay until both conditions  _C1_ and  _C2_ are satisfied.
</li>
 <li>_C1_; _C2_
Delay until either condition  _C1_ or condition  _C2_ is satisfied.
</li>
 <li>?=( _V1_, _C2_)
Delay until terms  _V1_ and  _V1_ have been unified.
</li>
 <li>nonvar( _V_)
Delay until variable  _V_ is bound.
</li>
 <li>ground( _V_)
Delay until variable  _V_ is ground.
</li>
</ul>

Note that [when/2](@ref when) will fail if the conditions fail.

</li>
 <li>call_residue(: _G_, _L_) @anchor call_residue



Call goal  _G_. If subgoals of  _G_ are still blocked, return
a list containing these goals and the variables they are blocked in. The
goals are then considered as unblocked. The next example shows a case
where [dif/2](@ref dif) suspends twice, once outside [call_residue/2](@ref call_residue),
and the other inside:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- dif(X,Y),
       call_residue((dif(X,Y),(X = f(Z) ; Y = f(Z))), L).

X = f(Z),
L = [[Y]-dif(f(Z),Y)],
dif(f(Z),Y) ? ;

Y = f(Z),
L = [[X]-dif(X,f(Z))],
dif(X,f(Z)) ? ;

no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The system only reports one invocation of [dif/2](@ref dif) as having
suspended. 

</li>
 <li>call_residue_vars(: _G_, _L_) @anchor call_residue_vars



Call goal  _G_ and unify  _L_ with a list of all constrained variables created <em>during</em> execution of  _G_:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ?- dif(X,Z), call_residue_vars(dif(X,Y),L).
dif(X,Z), call_residue_vars(dif(X,Y),L).
L = [Y],
dif(X,Z),
dif(X,Y) ? ;

no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
</ul>

@section Attributed_Variables Attributed Variables

YAP supports attributed variables, originally developed at OFAI by
Christian Holzbaur. Attributes are a means of declaring that an
arbitrary term is a property for a variable. These properties can be
updated during forward execution. Moreover, the unification algorithm is
aware of attributed variables and will call user defined handlers when
trying to unify these variables.

Attributed variables provide an elegant abstraction over which one can
extend Prolog systems. Their main application so far has been in
implementing constraint handlers, such as Holzbaur's CLPQR, Fruewirth
and Holzbaur's CHR, and CLP(BN). 

Different Prolog systems implement attributed variables in different
ways. Traditionally, YAP has used the interface designed by SICStus
Prolog. This interface is still
available in the <tt>atts</tt> library, but from YAP-6.0.3 we recommend using
the hProlog, SWI style interface. The main reason to do so is that 
most packages included in YAP that use attributed variables, such as CHR, CLP(FD), and CLP(QR),
rely on the SWI-Prolog interface.

@section New_Style_Attribute_Declarations hProlog and SWI-Prolog style Attribute Declarations

The following documentation is taken from the SWI-Prolog manual.

Binding an attributed variable schedules a goal to be executed at the
first possible opportunity. In the current implementation the hooks are
executed immediately after a successful unification of the clause-head
or successful completion of a foreign language (built-in) predicate. Each
attribute is associated to a module and the hook [attr_unify_hook/2](@ref attr_unify_hook) is
executed in this module.  The example below realises a very simple and
incomplete finite domain reasoner.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(domain,
      [ domain/2            % Var, ?Domain
      ]).
:- use_module(library(ordsets)).

domain(X, Dom) :-
    var(Dom), !,
    get_attr(X, domain, Dom).
domain(X, List) :-
    list_to_ord_set(List, Domain),
    put_attr(Y, domain, Domain),
    X = Y.

%    An attributed variable with attribute value Domain has been
%    assigned the value Y

attr_unify_hook(Domain, Y) :-
    (   get_attr(Y, domain, Dom2)
    ->  ord_intersection(Domain, Dom2, NewDomain),
        (   NewDomain == []
        ->    fail
        ;    NewDomain = [Value]
        ->    Y = Value
        ;    put_attr(Y, domain, NewDomain)
        )
    ;   var(Y)
    ->  put_attr( Y, domain, Domain )
    ;   ord_memberchk(Y, Domain)
    ).

%    Translate attributes from this module to residual goals

attribute_goals(X) -->
    { get_attr(X, domain, List) },
    [domain(X, List)].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Before explaining the code we give some example queries:

The predicate `domain/2` fetches (first clause) or assigns
(second clause) the variable a <em>domain</em>, a set of values it can
be unified with.  In the second clause first associates the domain
with a fresh variable and then unifies X to this variable to deal
with the possibility that X already has a domain. The
predicate [attr_unify_hook/2](@ref attr_unify_hook) is a hook called after a variable with
a domain is assigned a value.  In the simple case where the variable
is bound to a concrete value we simply check whether this value is in
the domain. Otherwise we take the intersection of the domains and either
fail if the intersection is empty (first example), simply assign the
value if there is only one value in the intersection (second example) or
assign the intersection as the new domain of the variable (third
example). The nonterminal `attribute_goals/3` is used to translate
remaining attributes to user-readable goals that, when executed, reinstate
these attributes.

<ul>

 <li>put_attr(+ _Var_,+ _Module_,+ _Value_) @anchor put_attr



If  _Var_ is a variable or attributed variable, set the value for the
attribute named  _Module_ to  _Value_. If an attribute with this
name is already associated with  _Var_, the old value is replaced.
Backtracking will restore the old value (i.e., an attribute is a mutable
term. See also `setarg/3`). This predicate raises a representation error if
 _Var_ is not a variable and a type error if  _Module_ is not an atom.

</li>
 <li>get_attr(+ _Var_,+ _Module_,- _Value_) @anchor get_attr



Request the current  _value_ for the attribute named  _Module_.  If
 _Var_ is not an attributed variable or the named attribute is not
associated to  _Var_ this predicate fails silently.  If  _Module_
is not an atom, a type error is raised.

</li>
 <li>del_attr(+ _Var_,+ _Module_) @anchor del_attr



Delete the named attribute.  If  _Var_ loses its last attribute it
is transformed back into a traditional Prolog variable.  If  _Module_
is not an atom, a type error is raised. In all other cases this
predicate succeeds regardless whether or not the named attribute is
present.

</li>
 <li>attr_unify_hook(+ _AttValue_,+ _VarValue_) @anchor attr_unify_hook



Hook that must be defined in the module an attributed variable refers
to. Is is called <em>after</em> the attributed variable has been
unified with a non-var term, possibly another attributed variable.
 _AttValue_ is the attribute that was associated to the variable
in this module and  _VarValue_ is the new value of the variable.
Normally this predicate fails to veto binding the variable to
 _VarValue_, forcing backtracking to undo the binding.  If
 _VarValue_ is another attributed variable the hook often combines
the two attribute and associates the combined attribute with
 _VarValue_ using [put_attr/3](@ref put_attr).

</li>
 <li>attr_portray_hook(+ _AttValue_,+ _Var_) @anchor attr_portray_hook



Called by [write_term/2](@ref write_term) and friends for each attribute if the option
`attributes(portray)` is in effect.  If the hook succeeds the
attribute is considered printed.  Otherwise  `Module = ...` is
printed to indicate the existence of a variable.

</li>
 <li>attribute_goals(+ _Var_,- _Gs_,+ _GsRest_) @anchor attribute_goals



This nonterminal, if it is defined in a module, is used by  _copy_term/3_
to project attributes of that module to residual goals. It is also
used by the toplevel to obtain residual goals after executing a query.
</li>
</ul>

Normal user code should deal with [put_attr/3](@ref put_attr), [get_attr/3](@ref get_attr) and [del_attr/2](@ref del_attr).
The routines in this section fetch or set the entire attribute list of a
variables. Use of these predicates is anticipated to be restricted to
printing and other special purpose operations.

<ul>

 <li>get_attrs(+ _Var_,- _Attributes_) @anchor get_attrs



Get all attributes of  _Var_.  _Attributes_ is a term of the form
`att( _Module_,  _Value_,  _MoreAttributes_)`, where  _MoreAttributes_ is
`[]` for the last attribute.

</li>
 <li>put_attrs(+ _Var_,+ _Attributes_) @anchor put_attrs


Set all attributes of  _Var_.  See [get_attrs/2](@ref get_attrs) for a description of
 _Attributes_.

</li>
 <li>del_attrs(+ _Var_) @anchor del_attrs


If  _Var_ is an attributed variable, delete <em>all</em> its
attributes.  In all other cases, this predicate succeeds without
side-effects.

</li>
 <li>term_attvars(+ _Term_,- _AttVars_) @anchor term_attvars


 _AttVars_ is a list of all attributed variables in  _Term_ and
its attributes. I.e., [term_attvars/2](@ref term_attvars) works recursively through
attributes.  This predicate is Cycle-safe.

</li>
 <li>copy_term(? _TI_,- _TF_,- _Goals_) 

Term  _TF_ is a variant of the original term  _TI_, such that for
each variable  _V_ in the term  _TI_ there is a new variable  _V'_
in term  _TF_ without any attributes attached.  Attributed
variables are thus converted to standard variables.   _Goals_ is
unified with a list that represents the attributes.  The goal
`maplist(call, _Goals_)` can be called to recreate the
attributes.

Before the actual copying, `copy_term/3` calls
`attribute_goals/1` in the module where the attribute is
defined.

</li>
 <li>copy_term_nat(? _TI_,- _TF_)  @anchor copy_term_nat


As [copy_term/2](@ref copy_term).  Attributes however, are <em>not</em> copied but replaced
by fresh variables.

</li>
</ul>

@section Old_Style_Attribute_Declarations SICStus Prolog style Attribute Declarations

Old style attribute declarations are activated through loading the library <tt>atts</tt> . The command

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| ?- use_module(library(atts)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
enables this form of use of attributed variables. The package provides the
following functionality:

<ul>
 <li>Each attribute must be declared first. Attributes are described by a functor
and are declared per module. Each Prolog module declares its own sets of
attributes. Different modules may have different functors with the same
module.
</li>
 <li>The built-in [put_atts/2](@ref put_atts) adds or deletes attributes to a
variable. The variable may be unbound or may be an attributed
variable. In the latter case, YAP discards previous values for the
attributes.
</li>
 <li>The built-in [get_atts/2](@ref get_atts) can be used to check the values of
an attribute associated with a variable.
</li>
 <li>The unification algorithm calls the user-defined predicate
<tt>verify_attributes/3</tt> before trying to bind an attributed
variable. Unification will resume after this call.
</li>
 <li>The user-defined predicate
<tt>attribute_goal/2</tt> converts from an attribute to a goal.
</li>
 <li>The user-defined predicate
<tt>project_attributes/2</tt> is used from a set of variables into a set of
constraints or goals. One application of <tt>project_attributes/2</tt> is in
the top-level, where it is used to output the set of
floundered constraints at the end of a query.
</li>
</ul>

@subsection Attribute_Declarations Attribute Declarations

Attributes are compound terms associated with a variable. Each attribute
has a <em>name</em> which is <em>private</em> to the module in which the
attribute was defined. Variables may have at most one attribute with a
name. Attribute names are defined with the following declaration:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- attribute AttributeSpec, ..., AttributeSpec.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where each  _AttributeSpec_ has the form ( _Name_/ _Arity_).
One single such declaration is allowed per module  _Module_.

Although the YAP module system is predicate based, attributes are local
to modules. This is implemented by rewriting all calls to the
built-ins that manipulate attributes so that attribute names are
preprocessed depending on the module.  The `user:goal_expansion/3`
mechanism is used for this purpose.

@subsection Attribute_Manipulation Attribute Manipulation

The  attribute manipulation predicates always work as follows:

<ol>
 <li>The first argument is the unbound variable associated with
attributes,
</li>
 <li>The second argument is a list of attributes. Each attribute will
be a Prolog term or a constant, prefixed with the <tt>+</tt> and <tt>-</tt> unary
operators. The prefix <tt>+</tt> may be dropped for convenience.
</li>
</ol>

The following three procedures are available to the user. Notice that
these built-ins are rewritten by the system into internal built-ins, and
that the rewriting process <em>depends</em> on the module on which the
built-ins have been invoked.

<ul>
 <li>_Module_:get_atts( _-Var_, _?ListOfAttributes_) @anchor get_atts


Unify the list  _?ListOfAttributes_ with the attributes for the unbound
variable  _Var_. Each member of the list must be a bound term of the
form `+( _Attribute_)`, `-( _Attribute_)` (the <tt>kbd</tt>
prefix may be dropped). The meaning of <tt>+</tt> and <tt>-</tt> is:
</li>
 <li>+( _Attribute_)
Unifies  _Attribute_ with a corresponding attribute associated with
 _Var_, fails otherwise.

</li>
 <li>-( _Attribute_)
Succeeds if a corresponding attribute is not associated with
 _Var_. The arguments of  _Attribute_ are ignored.

</li>
 <li>_Module_:put_atts( _-Var_, _?ListOfAttributes_) @anchor put_atts


Associate with or remove attributes from a variable  _Var_. The
attributes are given in  _?ListOfAttributes_, and the action depends
on how they are prefixed:
</li>
 <li>+( _Attribute_)
Associate  _Var_ with  _Attribute_. A previous value for the
attribute is simply replace (like with `set_mutable/2`).

</li>
 <li>-( _Attribute_)
Remove the attribute with the same name. If no such attribute existed,
simply succeed.
</li>
</ul>

@subsection Attributed_Unification Attributed Unification

The user-predicate predicate [verify_attributes/3](@ref verify_attributes) is called when
attempting to unify an attributed variable which might have attributes
in some  _Module_.

<ul>
 <li>_Module_:verify_attributes( _-Var_,  _+Value_,  _-Goals_) @anchor verify_attributes



The predicate is called when trying to unify the attributed variable
 _Var_ with the Prolog term  _Value_. Note that  _Value_ may be
itself an attributed variable, or may contain attributed variables.  The
goal <tt>verify_attributes/3</tt> is actually called before  _Var_ is
unified with  _Value_.

It is up to the user to define which actions may be performed by
<tt>verify_attributes/3</tt> but the procedure is expected to return in
 _Goals_ a list of goals to be called <em>after</em>  _Var_ is
unified with  _Value_. If <tt>verify_attributes/3</tt> fails, the
unification will fail.

Notice that the <tt>verify_attributes/3</tt> may be called even if  _Var_\<
has no attributes in module <tt>Module</tt>. In this case the routine should
simply succeed with  _Goals_ unified with the empty list.

</li>
 <li>attvar( _-Var_) @anchor attvar


Succeed if  _Var_ is an attributed variable.
</li>
</ul>

@subsection Displaying_Attributes Displaying Attributes

Attributes are usually presented as goals. The following routines are
used by built-in predicates such as [call_residue/2](@ref call_residue) and by the
Prolog top-level to display attributes:

<ul>
 <li>_Module_:attribute_goal( _-Var_,  _-Goal_) @anchor attribute_goal


User-defined procedure, called to convert the attributes in  _Var_ to
a  _Goal_. Should fail when no interpretation is available.

</li>
</ul>

@subsection Projecting_Attributes Projecting Attributes

Constraint solvers must be able to project a set of constraints to a set
of variables. This is useful when displaying the solution to a goal, but
may also be used to manipulate computations. The user-defined
[project_attributes/2](@ref project_attributes) is responsible for implementing this
projection.

<ul>
 <li>_Module_:project_attributes( _+QueryVars_,  _+AttrVars_) @anchor project_attributes


Given a list of variables  _QueryVars_ and list of attributed
variables  _AttrVars_, project all attributes in  _AttrVars_ to
 _QueryVars_. Although projection is constraint system dependent,
typically this will involve expressing all constraints in terms of
 _QueryVars_ and considering all remaining variables as existentially
quantified.
</li>
</ul>

Projection interacts with [attribute_goal/2](@ref attribute_goal) at the Prolog top
level. When the query succeeds, the system first calls
[project_attributes/2](@ref project_attributes). The system then calls
[attribute_goal/2](@ref attribute_goal) to get a user-level representation of the
constraints. Typically, [attribute_goal/2](@ref attribute_goal) will convert from the
original constraints into a set of new constraints on the projection,
and these constraints are the ones that will have an
[attribute_goal/2](@ref attribute_goal) handler.

@subsection Attribute_Examples Attribute Examples

The following two examples example is taken from the SICStus Prolog manual. It
sketches the implementation of a simple finite domain ``solver''.  Note
that an industrial strength solver would have to provide a wider range
of functionality and that it quite likely would utilize a more efficient
representation for the domains proper.  The module exports a single
predicate `domain( _-Var_, _?Domain_)` which associates
 _Domain_ (a list of terms) with  _Var_.  A variable can be
queried for its domain by leaving  _Domain_ unbound.

We do not present here a definition for [project_attributes/2](@ref project_attributes).
Projecting finite domain constraints happens to be difficult.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(domain, [domain/2]).

:- use_module(library(atts)).
:- use_module(library(ordsets), [
        ord_intersection/3,
        ord_intersect/2,
        list_to_ord_set/2
   ]).

:- attribute dom/1.

verify_attributes(Var, Other, Goals) :-
        get_atts(Var, dom(Da)), !,          % are we involved?
        (   var(Other) ->                   % must be attributed then
            (   get_atts(Other, dom(Db)) -> %   has a domain?
                ord_intersection(Da, Db, Dc),
                Dc = [El|Els],              % at least one element
                (   Els = [] ->             % exactly one element
                    Goals = [Other=El]      % implied binding
                ;   Goals = [],
                    put_atts(Other, dom(Dc))% rescue intersection
                )
            ;   Goals = [],
                put_atts(Other, dom(Da))    % rescue the domain
            )
        ;   Goals = [],
            ord_intersect([Other], Da)      % value in domain?
        ).
verify_attributes(_, _, []).                % unification triggered
                                            % because of attributes
                                            % in other modules

attribute_goal(Var, domain(Var,Dom)) :-     % interpretation as goal
        get_atts(Var, dom(Dom)).

domain(X, Dom) :-
        var(Dom), !,
        get_atts(X, dom(Dom)).
domain(X, List) :-
        list_to_ord_set(List, Set),
        Set = [El|Els],                     % at least one element
        (   Els = [] ->                     % exactly one element
            X = El                          % implied binding
        ;   put_atts(Fresh, dom(Set)),
            X = Fresh                       % may call
                                            % verify_attributes/3
        ).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the ``implied binding'' `Other=El` was deferred until after
the completion of `verify_attribute/3`.  Otherwise, there might be a
danger of recursively invoking `verify_attribute/3`, which might bind
`Var`, which is not allowed inside the scope of `verify_attribute/3`.
Deferring unifications into the third argument of `verify_attribute/3`
effectively serializes the calls to `verify_attribute/3`.

Assuming that the code resides in the file domain.yap, we
can use it via:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| ?- use_module(domain).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's test it:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| ?- domain(X,[5,6,7,1]), domain(Y,[3,4,5,6]), domain(Z,[1,6,7,8]).

domain(X,[1,5,6,7]),
domain(Y,[3,4,5,6]),
domain(Z,[1,6,7,8]) ? 

yes
| ?- domain(X,[5,6,7,1]), domain(Y,[3,4,5,6]), domain(Z,[1,6,7,8]), 
     X=Y.

Y = X,
domain(X,[5,6]),
domain(Z,[1,6,7,8]) ? 

yes
| ?- domain(X,[5,6,7,1]), domain(Y,[3,4,5,6]), domain(Z,[1,6,7,8]),
     X=Y, Y=Z.

X = 6,
Y = 6,
Z = 6
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To demonstrate the use of the  _Goals_ argument of
[verify_attributes/3](@ref verify_attributes), we give an implementation of
[freeze/2](@ref freeze).  We have to name it `myfreeze/2` in order to
avoid a name clash with the built-in predicate of the same name.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(myfreeze, [myfreeze/2]).

:- use_module(library(atts)).

:- attribute frozen/1.

verify_attributes(Var, Other, Goals) :-
        get_atts(Var, frozen(Fa)), !,       % are we involved?
        (   var(Other) ->                   % must be attributed then
            (   get_atts(Other, frozen(Fb)) % has a pending goal?
            ->  put_atts(Other, frozen((Fa,Fb))) % rescue conjunction
            ;   put_atts(Other, frozen(Fa)) % rescue the pending goal
            ),
            Goals = []
        ;   Goals = [Fa]
        ).
verify_attributes(_, _, []).

attribute_goal(Var, Goal) :-                % interpretation as goal
        get_atts(Var, frozen(Goal)).

myfreeze(X, Goal) :-
        put_atts(Fresh, frozen(Goal)),
        Fresh = X.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Assuming that this code lives in file myfreeze.yap,
we would use it via:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| ?- use_module(myfreeze).
| ?- myfreeze(X,print(bound(x,X))), X=2.

bound(x,2)                      % side effect
X = 2                           % bindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The two solvers even work together:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| ?- myfreeze(X,print(bound(x,X))), domain(X,[1,2,3]),
     domain(Y,[2,10]), X=Y.

bound(x,2)                      % side effect
X = 2,                          % bindings
Y = 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The two example solvers interact via bindings to shared attributed
variables only.  More complicated interactions are likely to be found
in more sophisticated solvers.  The corresponding
[verify_attributes/3](@ref verify_attributes) predicates would typically refer to the
attributes from other known solvers/modules via the module prefix in
` _Module_:get_atts/2`.

@page Constraint_Logic_Programming_over_Reals Constraint Logic Programming over Reals

YAP now uses the CLP(R) package developed by <em>Leslie De Koninck</em>,
K.U. Leuven as part of a thesis with supervisor Bart Demoen and daily
advisor Tom Schrijvers, and distributed with SWI-Prolog.

This CLP(R) system is a port of the CLP(Q,R) system of Sicstus Prolog
and YAP by Christian Holzbaur: Holzbaur C.: OFAI clp(q,r) Manual,
Edition 1.3.3, Austrian Research Institute for Artificial
Intelligence, Vienna, TR-95-09, 1995,
<http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09> This
port only contains the part concerning real arithmetics. This manual
is roughly based on the manual of the above mentioned  *CLP(QR)*
implementation.

Please note that the clpr library is <em>not</em> an
`autoload` library and therefore this library must be loaded
explicitely before using it:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- use_module(library(clpr)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@section CLPR_Solver_Predicates Solver Predicates


The following predicates are provided to work with constraints:

<ul>
 <li>{+ _Constraints_}
Adds the constraints given by  _Constraints_ to the constraint store.

</li>
 <li>entailed(+ _Constraint_)
Succeeds if  _Constraint_ is necessarily true within the current
constraint store. This means that adding the negation of the constraint
to the store results in failure.

</li>
 <li>inf(+ _Expression_,- _Inf_)
Computes the infimum of  _Expression_ within the current state of the
constraint store and returns that infimum in  _Inf_. This predicate
does not change the constraint store.

</li>
 <li>inf(+ _Expression_,- _Sup_)
Computes the supremum of  _Expression_ within the current state of
the constraint store and returns that supremum in  _Sup_. This
predicate does not change the constraint store.

</li>
 <li>min(+ _Expression_)
Minimizes  _Expression_ within the current constraint store. This is
the same as computing the infimum and equation the expression to that
infimum.

</li>
 <li>max(+ _Expression_)
Maximizes  _Expression_ within the current constraint store. This is
the same as computing the supremum and equating the expression to that
supremum.

</li>
 <li>bb_inf(+ _Ints_,+ _Expression_,- _Inf_,- _Vertext_,+ _Eps_)
Computes the infimum of  _Expression_ within the current constraint
store, with the additional constraint that in that infimum, all
variables in  _Ints_ have integral values.  _Vertex_ will contain
the values of  _Ints_ in the infimum.  _Eps_ denotes how much a
value may differ from an integer to be considered an integer. E.g. when
 _Eps_ = 0.001, then X = 4.999 will be considered as an integer (5 in
this case).  _Eps_ should be between 0 and 0.5.

</li>
 <li>bb_inf(+ _Ints_,+ _Expression_,- _Inf_)
The same as bb_inf/5 but without returning the values of the integers
and with an eps of 0.001.

</li>
 <li>dump(+ _Target_,+ _Newvars_,- _CodedAnswer_)
Returns the constraints on  _Target_ in the list  _CodedAnswer_
where all variables of  _Target_ have veen replaced by  _NewVars_.
This operation does not change the constraint store. E.g. in

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dump([X,Y,Z],[x,y,z],Cons)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 _Cons_ will contain the constraints on  _X_,  _Y_ and
 _Z_ where these variables have been replaced by atoms `x`, `y` and `z`.

</li>
</ul>

@section CLPR_Syntax Syntax of the predicate arguments


The arguments of the predicates defined in the subsection above are
defined in the following table. Failing to meet the syntax rules will
result in an exception.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
<Constraints> ---> <Constraint>				\\ single constraint \\
	      | <Constraint> , <Constraints>		\\ conjunction \\
	      | <Constraint> ; <Constraints>		\\ disjunction \\

<Constraint> ---> <Expression> {<} <Expression>		\\ less than \\
	     | <Expression> {>} <Expression>		\\ greater than \\
	     | <Expression> {=<} <Expression>	\\ less or equal \\
	     | {<=}(<Expression>, <Expression>)	\\ less or equal \\
	     | <Expression> {>=} <Expression>	\\ greater or equal \\
	     | <Expression> {=\=} <Expression>	\\ not equal \\
	     | <Expression> =:= <Expression>		\\ equal \\
	     | <Expression> = <Expression>		\\ equal \\

<Expression> --->  <Variable>				\\ Prolog variable \\
	     | <Number>				\\ Prolog number (float, integer) \\
	     | +<Expression>				\\ unary plus \\
	     | -<Expression>				\\ unary minus \\
	     | <Expression> + <Expression>		\\ addition \\
	     | <Expression> - <Expression>		\\ substraction \\
	     | <Expression> * <Expression>		\\ multiplication \\
	     | <Expression> / <Expression>		\\ division \\
	     | abs(<Expression>)			\\ absolute value \\
	     | sin(<Expression>)			\\ sine \\
	     | cos(<Expression>)			\\ cosine \\
	     | tan(<Expression>)			\\ tangent \\
	     | exp(<Expression>)			\\ exponent \\
	     | pow(<Expression>)			\\ exponent \\
	     | <Expression> {^} <Expression>		\\ exponent \\
	     | min(<Expression>, <Expression>)	\\ minimum \\
	     | max(<Expression>, <Expression>)	\\ maximum \\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@section CLPR_Unification Use of unification

Instead of using the `{}/1` predicate, you can also use the standard
unification mechanism to store constraints. The following code samples
are equivalent:

<ul>
 <li>Unification with a variable

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{X =:= Y}
{X = Y}
X = Y
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>Unification with a number

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{X =:= 5.0}
{X = 5.0}
X = 5.0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
</ul>

@section CLPR_NonhYlinear_Constraints Non-Linear Constraints


In this version, non-linear constraints do not get solved until certain
conditions are satisfied. We call these conditions the isolation axioms.
They are given in the following table.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A = B * C         when B or C is ground	or		 // A = 5 * C or A = B * 4 \\
	                      A and (B or C) are ground	 // 20 = 5 * C or 20 = B * 4 \\

A = B / C      when C is ground or			// A = B / 3 
	                      A and B are ground		// 4 = 12 / C 

X = min(Y,Z)     when Y and Z are ground or	// X = min(4,3) 
X = max(Y,Z)         Y and Z are ground		// X = max(4,3) 
X = abs(Y)                Y is ground			// X = abs(-7) 

X = pow(Y,Z)   when X and Y are ground or		// 8 = 2 ^ Z 
X = exp(Y,Z)           X and Z are ground		// 8 = Y ^ 3 
X = Y ^ Z            Y and Z are ground		// X = 2 ^ 3 

X = sin(Y)	    when X is ground or			// 1 = sin(Y) 
X = cos(Y)	               Y is ground			// X = sin(1.5707) 
X = tan(Y)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@page CHRcC_Constraint_Handling_Rules_ CHR: Constraint Handling Rules 


This chapter is written by Tom Schrijvers, K.U. Leuven for the hProlog
system. Adjusted by Jan Wielemaker to fit the SWI-Prolog documentation
infrastructure and remove hProlog specific references.

The CHR system of SWI-Prolog is the K.U.Leuven CHR system.  The runtime
environment is written by Christian Holzbaur and Tom Schrijvers while the
compiler is written by Tom Schrijvers. Both are integrated with SWI-Prolog
and licenced under compatible conditions with permission from the authors.

The main reference for SWI-Prolog's CHR system is:

<ul>
 <li>T. Schrijvers, and B. Demoen, <em>The K.U.Leuven CHR System: Implementation and Application</em>, First Workshop on Constraint Handling Rules: Selected
Contributions (Fruwirth, T. and Meister, M., eds.), pp. 1--5, 2004.
</li>
</ul>

@section CHR_Introduction Introduction


Constraint Handling Rules (CHR) is a committed-choice bottom-up language
embedded in Prolog. It is designed for writing constraint solvers and is
particularily useful for providing application-specific constraints.
It has been used in many kinds of applications, like scheduling,
model checking, abduction, type checking among many others.

CHR has previously been implemented in other Prolog systems (SICStus,
Eclipse, Yap), Haskell and Java. This CHR system is based on the
compilation scheme and runtime environment of CHR in SICStus.

In this documentation we restrict ourselves to giving a short overview
of CHR in general and mainly focus on elements specific to this
implementation. For a more thorough review of CHR we refer the reader to
[Freuhwirth:98]. More background on CHR can be found at the CHR web site.

@section CHR_Syntax_and_Semantics Syntax and Semantics



@subsection CHR_Syntax CHR Syntax


The syntax of CHR rules in hProlog is the following:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rules --> rule, rules.
rules --> [].

rule --> name, actual_rule, pragma, [atom('.')].

name --> atom, [atom('@')].
name --> [].

actual_rule --> simplification_rule.
actual_rule --> propagation_rule.
actual_rule --> simpagation_rule.

simplification_rule --> constraints, [atom('<=>')], guard, body.
propagation_rule --> constraints, [atom('==>')], guard, body.
simpagation_rule --> constraints, [atom('\')], constraints, [atom('<=>')], 
                     guard, body.

constraints --> constraint, constraint_id.
constraints --> constraint, [atom(',')], constraints.

constraint --> compound_term.

constraint_id --> [].
constraint_id --> [atom('#')], variable.

guard --> [].
guard --> goal, [atom('|')].

body --> goal.

pragma --> [].
pragma --> [atom('pragma')], actual_pragmas.

actual_pragmas --> actual_pragma.
actual_pragmas --> actual_pragma, [atom(',')], actual_pragmas.

actual_pragma --> [atom('passive(')], variable, [atom(')')].

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Additional syntax-related terminology:

<ul>
 <li>*head:* the constraints in an `actual_rule` before
the arrow (either `\<=\>` or `==\>`)
</li>
</ul>

@subsection Semantics Semantics


In this subsection the operational semantics of CHR in Prolog are presented
informally. They do not differ essentially from other CHR systems.

When a constraint is called, it is considered an active constraint and
the system will try to apply the rules to it. Rules are tried and executed
sequentially in the order they are written. 

A rule is conceptually tried for an active constraint in the following
way. The active constraint is matched with a constraint in the head of
the rule. If more constraints appear in the head they are looked for
among the suspended constraints, which are called passive constraints in
this context. If the necessary passive constraints can be found and all
match with the head of the rule and the guard of the rule succeeds, then
the rule is committed and the body of the rule executed. If not all the
necessary passive constraint can be found, the matching fails or the
guard fails, then the body is not executed and the process of trying and
executing simply continues with the following rules. If for a rule,
there are multiple constraints in the head, the active constraint will
try the rule sequentially multiple times, each time trying to match with
another constraint.

This process ends either when the active constraint disappears, i.e. it
is removed by some rule, or after the last rule has been processed. In
the latter case the active constraint becomes suspended.

A suspended constraint is eligible as a passive constraint for an active
constraint. The other way it may interact again with the rules, is when
a variable appearing in the constraint becomes bound to either a nonvariable
or another variable involved in one or more constraints. In that case the
constraint is triggered, i.e. it becomes an active constraint and all
the rules are tried.

@subsubsection Rule_Types


There are three different kinds of rules, each with their specific semantics:

<ul>
 <li>simplification
The simplification rule removes the constraints in its head and calls its body.

</li>
 <li>propagation
The propagation rule calls its body exactly once for the constraints in
its head.

</li>
 <li>simpagation
The simpagation rule removes the constraints in its head after the
`\\` and then calls its body. It is an optimization of
simplification rules of the form: \\[constraints_1, constraints_2 \<=\>
constraints_1, body \\] Namely, in the simpagation form: 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
constraints1 \ constraints2 <=> body
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 _constraints1_
constraints are not called in the body.
</li>
</ul>

@subsubsection Rule_Names

Naming a rule is optional and has no semantical meaning. It only functions
as documentation for the programmer.

@subsubsection Pragmas

The semantics of the pragmas are:

<ul>
 <li>passive(Identifier)
The constraint in the head of a rule  _Identifier_ can only act as a
passive constraint in that rule.
</li>
</ul>

Additional pragmas may be released in the future.

@subsubsection Options

It is possible to specify options that apply to all the CHR rules in the module.
Options are specified with the `option/2` declaration:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                option(Option,Value).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Available options are:

<ul>
 <li>check_guard_bindings
This option controls whether guards should be checked for illegal
variable bindings or not. Possible values for this option are
`on`, to enable the checks, and `off`, to disable the
checks.

</li>
 <li>optimize
This is an experimental option controlling the degree of optimization.
Possible values are `full`, to enable all available
optimizations, and `off` (default), to disable all optimizations.  
The default is derived from the SWI-Prolog flag `optimise`, where
`true` is mapped to `full`.  Therefore the commandline
option `-O` provides full CHR optimization.
If optimization is enabled, debugging should be disabled.

</li>
 <li>debug
This options enables or disables the possibility to debug the CHR code.
Possible values are `on` (default) and `off`. See
`debugging` for more details on debugging.  The default is
derived from the prolog flag `generate_debug_info`, which
is `true` by default.  See `-nodebug`.
If debugging is enabled, optimization should be disabled.

</li>
 <li>mode
This option specifies the mode for a particular constraint. The
value is a term with functor and arity equal to that of a constraint.
The arguments can be one of `-`, `+` or `?`.
The latter is the default. The meaning is the following:

<ul>
 <li>-
The corresponding argument of every occurrence
of the constraint is always unbound.
</li>
 <li>+ 
The corresponding argument of every occurrence
of the constraint is always ground.
</li>
 <li>?
The corresponding argument of every occurrence
of the constraint can have any instantiation, which may change
over time. This is the default value.
</li>
</ul>
The declaration is used by the compiler for various optimizations. 
Note that it is up to the user the ensure that the mode declaration
is correct with respect to the use of the constraint.
This option may occur once for each constraint.

</li>
 <li>type_declaration
This option specifies the argument types for a particular constraint. The
value is a term with functor and arity equal to that of a constraint.
The arguments can be a user-defined type or one of
the built-in types:

<ul>
 <li>int
The corresponding argument of every occurrence
of the constraint is an integer number.
</li>
 <li>float
...{} a floating point number.
</li>
 <li>number
...{} a number.
</li>
 <li>natural
...{} a positive integer.
</li>
 <li>any
The corresponding argument of every occurrence
of the constraint can have any type. This is the default value.
</li>
</ul>

Currently, type declarations are only used to improve certain
optimizations (guard simplification, occurrence subsumption, ...{}).

</li>
 <li>type_definition
This option defines a new user-defined type which can be used in
type declarations. The value is a term of the form
`type(` _name_`,` _list_`)`, where
 _name_ is a term and  _list_ is a list of alternatives.
Variables can be used to define generic types. Recursive definitions
are allowed. Examples are 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type(bool,[true,false]).
type(complex_number,[float + float * i]).
type(binary_tree(T),[ leaf(T) | node(binary_tree(T),binary_tree(T)) ]).
type(list(T),[ [] | [T | list(T)]).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
</ul>

The mode, type_declaration and type_definition options are provided
for backward compatibility. The new syntax is described below.

@section CHR_in_YAP_Programs CHR in YAP Programs	



@subsection Embedding_in_Prolog_Programs Embedding in Prolog Programs

The CHR constraints defined in a particulary chr file are
associated with a module. The default module is `user`. One should
never load different chr files with the same CHR module name.

@subsection Constraint_declaration Constraint declaration

Every constraint used in CHR rules has to be declared.
There are two ways to do this. The old style is as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
option(type_definition,type(list(T),[ [] , [T|list(T)] ]).
option(mode,foo(+,?)).
option(type_declaration,foo(list(int),float)).
:- constraints foo/2, bar/0.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The new style is as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- chr_type list(T) ---> [] ; [T|list(T)].
:- constraints foo(+list(int),?float), bar.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection Compilation Compilation

The SWI-Prolog CHR compiler exploits term_expansion/2 rules to translate
the constraint handling rules to plain Prolog. These rules are loaded
from the library chr.   They are activated if the compiled file
has the chr extension or after finding a declaration of the
format below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- constraints ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is adviced to define CHR rules in a module file, where the module
declaration is immediately followed by including the chr
library as examplified below:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(zebra, [ zebra/0 ]).
:- use_module(library(chr)).

:- constraints ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Using this style CHR rules can be defined in ordinary Prolog
pl files and the operator definitions required by CHR do not
leak into modules where they might cause conflicts.

@section CHR_Debugging Debugging



The CHR debugging facilities are currently rather limited. Only tracing
is currently available. To use the CHR debugging facilities for a CHR
file it must be compiled for debugging. Generating debug info is
controlled by the CHR option [debug](@ref debug), whose default is derived
from the SWI-Prolog flag `generate_debug_info`.  Therefore debug
info is provided unless the `-nodebug` is used.

@subsection Ports Ports



For CHR constraints the four standard ports are defined:

<ul>
 <li>call
A new constraint is called and becomes active.
</li>
 <li>exit
An active constraint exits: it has either been inserted in the store after
trying all rules or has been removed from the constraint store.
</li>
 <li>fail
An active constraint fails.
</li>
 <li>redo
An active constraint starts looking for an alternative solution.
</li>
</ul>

In addition to the above ports, CHR constraints have five additional
ports:

<ul>
 <li>wake
A suspended constraint is woken and becomes active.
</li>
 <li>insert
An active constraint has tried all rules and is suspended in
the constraint store.
</li>
 <li>remove
An active or passive constraint is removed from the constraint
store, if it had been inserted.
</li>
 <li>try
An active constraints tries a rule with possibly
some passive constraints. The try port is entered
just before committing to the rule.
</li>
 <li>apply
An active constraints commits to a rule with possibly
some passive constraints. The apply port is entered
just after committing to the rule.
</li>
</ul>

@subsection Tracing Tracing


Tracing is enabled with the chr_trace/0 predicate
and disabled with the chr_notrace/0 predicate.

When enabled the tracer will step through the `call`,
`exit`, `fail`, `wake` and `apply` ports,
accepting debug commands, and simply write out the other ports.

The following debug commans are currently supported:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        CHR debug options:

                <cr>    creep           c       creep
		s	skip
		g	ancestors
                n       nodebug
		b	break
                a       abort
                f       fail
                ?       help            h       help
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Their meaning is:

<ul>
 <li>creep
Step to the next port.
</li>
 <li>skip
Skip to exit port of this call or wake port.
</li>
 <li>ancestors
Print list of ancestor call and wake ports.
</li>
 <li>nodebug
Disable the tracer.
</li>
 <li>break
Enter a recursive Prolog toplevel.  See break/0.
</li>
 <li>abort
Exit to the toplevel.  See abort/0.
</li>
 <li>fail
Insert failure in execution.
</li>
 <li>help
Print the above available debug options.
</li>
</ul>

@subsection CHR_Debugging_Predicates CHR Debugging Predicates



The chr module contains several predicates that allow
inspecting and printing the content of the constraint store.

<ul>
 <li>chr_trace/0
Activate the CHR tracer.  By default the CHR tracer is activated and
deactivated automatically by the Prolog predicates trace/0 and
notrace/0.

</li>
 <li>chr_notrace/0
De-activate the CHR tracer.  By default the CHR tracer is activated and
deactivated automatically by the Prolog predicates trace/0 and
notrace/0.

</li>
 <li>chr_leash/0 

Define the set of CHR ports on which the CHR
tracer asks for user intervention (i.e. stops).  _Spec_ is either a
list of ports or a predefined `alias'. Defined aliases are:
`full` to stop at all ports, `none` or `off` to never
stop, and `default` to stop at the `call`, `exit`,
`fail`, `wake` and `apply` ports.  See also leash/1.

</li>
 <li>chr_show_store(+ _Mod_)
Prints all suspended constraints of module  _Mod_ to the standard
output. This predicate is automatically called by the SWI-Prolog toplevel at
the end of each query for every CHR module currently loaded.  The prolog-flag
`chr_toplevel_show_store` controls whether the toplevel shows the
constraint stores. The value `true` enables it.  Any other value
disables it.

</li>
</ul>

@section CHR_Examples Examples



Here are two example constraint solvers written in CHR.

<ul>
 <li>
The program below defines a solver with one constraint, 
`leq/2`, which is a less-than-or-equal constraint.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(leq,[cycle/3, leq/2]).
:- use_module(library(chr)).

:- constraints leq/2.
reflexivity  @ leq(X,X) <=> true.
antisymmetry @ leq(X,Y), leq(Y,X) <=> X = Y.
idempotence  @ leq(X,Y) \ leq(X,Y) <=> true.
transitivity @ leq(X,Y), leq(Y,Z) ==> leq(X,Z).

cycle(X,Y,Z):-
        leq(X,Y),
        leq(Y,Z),
        leq(Z,X).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>
The program below implements a simple finite domain
constraint solver.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(dom,[dom/2]).
:- use_module(library(chr)).

:- constraints dom/2. 

dom(X,[]) <=> fail.
dom(X,[Y]) <=> X = Y.
dom(X,L1), dom(X,L2) <=> intersection(L1,L2,L3), dom(X,L3).

intersection([],_,[]).
intersection([H|T],L2,[H|L3]) :-
        member(H,L2), !,
        intersection(T,L2,L3).
intersection([_|T],L2,L3) :-
        intersection(T,L2,L3).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
</ul>

@section CHR_Compatibility Compatibility with SICStus CHR



There are small differences between CHR in SWI-Prolog and newer
YAPs and SICStus and older versions of YAP.  Besides differences in
available options and pragmas, the following differences should be
noted:

<ul>
 <li>[The handler/1 declaration]
In SICStus every CHR module requires a `handler/1`
declaration declaring a unique handler name. This declaration is valid
syntax in SWI-Prolog, but will have no effect. A warning will be given
during compilation.

</li>
 <li>[The rules/1 declaration]
In SICStus, for every CHR module it is possible to only enable a subset
of the available rules through the `rules/1` declaration. The
declaration is valid syntax in SWI-Prolog, but has no effect. A
warning is given during compilation.

</li>
 <li>[Sourcefile naming]
SICStus uses a two-step compiler, where chr files are
first translated into pl files.  For SWI-Prolog CHR
rules may be defined in a file with any extension.
</li>
</ul>

@section CHR_Guidelines Guidelines



In this section we cover several guidelines on how to use CHR to write
constraint solvers and how to do so efficiently.

<ul>
 <li>[Set semantics]
The CHR system allows the presence of identical constraints, i.e.
multiple constraints with the same functor, arity and arguments. For
most constraint solvers, this is not desirable: it affects efficiency
and possibly termination. Hence appropriate simpagation rules should be
added of the form:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{constraint \ constraint <=> true}.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>[Multi-headed rules]
Multi-headed rules are executed more efficiently when the constraints
share one or more variables.

</li>
 <li>[Mode and type declarations]
Provide mode and type declarations to get more efficient program execution.
Make sure to disable debug (`-nodebug`) and enable optimization
(`-O`).
</li>
</ul>

@section Logtalk Logtalk

The Logtalk object-oriented extension is available after running its 
standalone installer by using the `yaplgt` command in POSIX 
systems or by using the `Logtalk - YAP` shortcut in the Logtalk 
program group in the Start Menu on Windows systems. For more information 
please see the URL <http://logtalk.org/>.

@section MYDDAS MYDDAS

The MYDDAS database project was developed within a FCT project aiming at
the development of a highly efficient deductive database system, based
on the coupling of the MySQL relational database system with the Yap
Prolog system. MYDDAS was later expanded to support the ODBC interface.

@section Requirements_and_Installation_Guide Requirements and Installation Guide

Next, we describe how to usen of the YAP with the MYDDAS System.  The
use of this system is entirely depend of the MySQL development libraries
or the ODBC development libraries. At least one of the this development
libraries must be installed on the computer system, otherwise MYDDAS
will not compile. The MySQL development libraries from MySQL 3.23 an
above are know to work. We recommend the usage of MySQL versusODBC,
but it is possible to have both options installed

At the same time, without any problem. The MYDDAS system automatically
controls the two options. Currently, MYDDAS is know to compile without
problems in Linux. The usage of this system on Windows has not been
tested yet.  MYDDAS must be enabled at configure time. This can be done
with the following options: 

<ul>

 <li>--enable-myddas
This option will detect which development libraries are installed on the computer system, MySQL, ODBC or both, and will compile the Yap system with the support for which libraries it detects;
</li>
 <li>--enable-myddas-stats
This option is only available in MySQL. It includes code to get
statistics from the MYDDAS system;
</li>
 <li>--enable-top-level
This option is only available in MySQL.  It enables the option to interact with the MySQL server in
two different ways. As if we were on the MySQL Client Shell, and as if
we were using Datalog. 
</li>
</ul>

@section MYDDAS_Architecture MYDDAS Architecture

The system includes four main blocks that are put together through the
MYDDAS interface: the Yap Prolog compiler, the MySQL database system, an
ODBC layer and a Prolog to SQL compiler. Current effort is put on the
MySQL interface rather than on the ODBC interface. If you want to use
the full power of the MYDDAS interface we recommend you to use a MySQL
database. Other databases, such as Oracle, PostGres or Microsoft SQL
Server, can be interfaced through the ODBC layer, but with limited
performance and features support.  

The main structure of the MYDDAS interface is simple. Prolog queries
involving database goals are translated to SQL using the Prolog to SQL
compiler; then the SQL expression is sent to the database system, which
returns the set of tuples satisfying the query; and finally those tuples
are made available to the Prolog engine as terms. For recursive queries
involving database goals, the YapTab tabling engine provides the
necessary support for an efficient evaluation of such queries.

An important aspect of the MYDDAS interface is that for the programmer
the use of predicates which are defined in database relations is
completely transparent. An example of this transparent support is the
Prolog cut operator, which has exactly the same behaviour from
predicates defined in the Prolog program source code, or from predicates
defined in database as relations.

@section Loading_MYDDAS Loading MYDDAS

Begin by starting YAP and loading the library
`use_module(library(myddas))`.  This library already includes the
Prolog to SQL Compiler described in [2] and [1]. In MYDDAS this compiler
has been extended to support further constructs which allow a more
efficient SQL translation.  

@section Connecting_to_and_disconnecting_from_a_Database_Server Connecting to and disconnecting from a Database Server

<ul>
 <li>db open(+,+,+,+,+).  @anchor db_open



</li>
 <li>db open(+,+,+,+). 


</li>
 <li>db close(+).  @anchor db_close



</li>
</ul>

Assuming the MySQL server is running and we have an account, we can
login to MySQL by invoking [db_open/5](@ref db_open) as one of the following:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_open(mysql,Connection,Host/Database,User,Password). 
?- db_open(mysql,Connection,Host/Database/Port,User,Password).
?- db_open(mysql,Connection,Host/Database/UnixSocket,User,Password). 
?- db_open(mysql,Connection,Host/Database/Port/UnixSocket,User,Password).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the login is successful, there will be a response of `yes`. For
instance:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_open(mysql,con1,localhost/guest_db,guest,'').
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uses the MySQL native interface, selected by the first argument, to open
a connection identified by the `con1` atom, to an instance of a
MySQL server running on host `localhost`, using database guest `db`
and user `guest` with empty `password`.  To disconnect from the `con1`
connection we use: 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_close(con1).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Alternatively, we can use `db_open/4` and `db_close/0,` without an argument
to identify the connection. In this case the default connection is used,
with atom `myddas`. Thus using 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_open(mysql,localhost/guest_db,guest,''). 
?- db_close.  
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
or

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_open(mysql,myddas,localhost/guest_db,guest,''). 
?- db_close(myddas). 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is exactly the same.

MYDDAS also supports ODBC. To connect to a database using an ODBC driver
you must have configured on your system a ODBC DSN. If so, the `db_open/4`
and [db_open/5](@ref db_open) have the following mode:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ?- db_open(odbc,Connection,ODBC_DSN,User,Password). 
 ?- db_open(odbc,ODBC_DSN,User,Password).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For instance, if you do `db_open(odbc,odbc_dsn,guest,'')`. it will connect
to a database, through ODBC, using the definitions on the `odbc_dsn` DSN
configured on the system. The user will be the user `guest` with no
password.

@section Accessing_a_Relation Accessing a Relation

<ul>
 <li>db_import(+Conn,+RelationName,+PredName).  @anchor db_import



</li>
 <li>db_import(+RelationName,+PredName).  

</li>
</ul>

Assuming you have access permission for the relation you wish to import,
you can use [db_import/3](@ref db_import) or `db_import/2` as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_import(Conn,RelationName,PredName).
?- db_import(RelationName,PredName).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where  _RelationName_, is the name of
relation we wish to access,  _PredName_ is the name of the predicate we
wish to use to access the relation from YAP.  _Conn_, is the connection
identifier, which again can be dropped so that the default myddas connection
is used. For instance, if we want to access the relation phonebook,
using the predicate `phonebook/3` we write: 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_import(con1,phonebook,phonebook). 
yes
?- phonebook(Letter,Name,Number).
Letter = 'D',
Name = 'John Doe',
Number = 123456789 ? 
yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Backtracking can then be used to retrieve the next row
of the relation phonebook.  Records with particular field values may be
selected in the same way as in Prolog. (In particular, no mode
specification for database predicates is required). For instance: 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- phonebook(Letter,'John Doe',Letter). 
Letter = 'D', 
Number = 123456789 ?
yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
generates the query

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SELECT A.Letter , 'John Doe' , A.Number 
FROM 'phonebook' A 
WHERE A.Name = 'John Doe';
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@section View_Level_Interface View Level Interface

<ul>
 <li>db view(+,+,+). @anchor db_view



</li>
 <li>db view(+,+).

</li>
</ul>
If we import a database relation, such as an edge relation representing the edges of a directed graph, through

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_import('Edge',edge). 
yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and we then write a query to retrieve all the direct cycles in the
graph, such as

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- edge(A,B), edge(B,A). 
A = 10, 
B = 20 ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
this is clearly inefficient [3], because of relation-level
access. Relation-level access means that a separate SQL query will be
generated for every goal in the body of the clause. For the second
`edge/2` goal, a SQL query is generated using the variable bindings that
result from the first `edge/2` goal execution. If the second
`edge/2` goal
fails, or if alternative solutions are demanded, backtracking access the
next tuple for the first `edge/2` goal and another SQL query will be
generated for the second `edge/2` goal. The generation of this large
number of queries and the communication overhead with the database
system for each of them, makes the relation-level approach inefficient.
To solve this problem the view level interface can be used for the
definition of rules whose bodies includes only imported database
predicates.  One can use the view level interface through the predicates
[db_view/3](@ref db_view) and `db_view/2`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_view(Conn,PredName(Arg_1,...,Arg_n),DbGoal).  
?- db_view(PredName(Arg_1,...,Arg_n),DbGoal).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All arguments are standard Prolog terms.  _Arg1_ through  _Argn_
define the attributes to be retrieved from the database, while
 _DbGoal_ defines the selection restrictions and join
conditions.  _Conn_ is the connection identifier, which again can be
dropped. Calling predicate `PredName/n` will retrieve database
tuples using a single SQL query generated for the  _DbGoal_.  We next show
an example of a view definition for the direct cycles discussed
above. Assuming the declaration: 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_import('Edge',edge). 
yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
we
write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_view(direct_cycle(A,B),(edge(A,B), edge(B,A))). 
yes 
?- direct_cycle(A,B)). 
A = 10, 
B = 20 ?  
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This call generates the SQL
statement:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SELECT A.attr1 , A.attr2
FROM Edge A , Edge B 
WHERE B.attr1 = A.attr2 AND B.attr2 = A.attr1;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Backtracking, as in relational level interface, can be used to retrieve the next row of the view.
The view interface also supports aggregate function predicates such as
`sum`, `avg`, `count`, `min` and `max`. For
instance:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_view(count(X),(X is count(B, B^edge(10,B)))).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
generates the query :

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SELECT COUNT(A.attr2) 
FROM Edge A WHERE A.attr1 = 10;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To know how to use db `view/3`, please refer to Draxler's Prolog to
SQL Compiler Manual. 

@section Accessing_Tables_in_Data_Sources_Using_SQL Accessing Tables in Data Sources Using SQL 

<ul>
 <li>db_sql(+,+,?).  @anchor db_sql



</li>
 <li>db_sql(+,?).

</li>
</ul>

It is also possible to explicitly send a SQL query to the database server using

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_sql(Conn,SQL,List).
?- db_sql(SQL,List).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where  _SQL_ is an arbitrary SQL expression, and  _List_ is a list
holding the first tuple of result set returned by the server. The result
set can also be navigated through backtracking.

Example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_sql('SELECT * FROM phonebook',LA).
LA = ['D','John Doe',123456789] ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@section Insertion_of_Rows Insertion of Rows 

<ul>
 <li>db_assert(+,+).  @anchor db_assert



</li>
 <li>db_assert(+).


</li>
</ul>

Assuming you have imported the related base table using
`db_import/2` or [db_import/3](@ref db_import), you can insert to that table
by using [db_assert/2](@ref db_assert) predicate any given fact.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_assert(Conn,Fact).
?- db_assert(Fact).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The second argument must be declared with all of its arguments bound to
constants. For example assuming `helloWorld` is imported through
`db_import/2`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_import('Hello World',helloWorld).
yes
?- db_assert(helloWorld('A' ,'Ana',31)). 
yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This, would generate the following query 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
INSERT INTO helloWorld
VALUES ('A','Ana',3)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
which would insert into the helloWorld, the following row:
`A,Ana,31`. If we want to insert `NULL`  values into the
relation, we call [db_assert/2](@ref db_assert) with a uninstantiated variable in
the data base imported predicate. For example, the following query on
the YAP-prolog system:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_assert(helloWorld('A',NULL,31)).
yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Would insert the row: `A,null value,31` into the relation
`Hello World`, assuming that the second row allows null values.

<ul>
 <li>db insert(+,+,+).  @anchor db_insert



</li>
 <li>db insert(+,+).  

</li>
</ul>

This predicate would create a new database predicate, which will insert
any given tuple into the database.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_insert(Conn,RelationName,PredName).
?- db_insert(RelationName,PredName).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This would create a new predicate with name  _PredName_, that will
insert tuples into the relation  _RelationName_. is the connection
identifier. For example, if we wanted to insert the new tuple
`('A',null,31)` into the relation `Hello World`, we do: 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_insert('Hello World',helloWorldInsert). 
yes
?- helloWorldInsert('A',NULL,31).
yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@section Types_of_Attributes Types of Attributes

<ul>
 <li>db_get_attributes_types(+,+,?). @anchor db_get_attributes_types



</li>
 <li>db_get_attributes_types(+,?). 


</li>
</ul>

The prototype for this predicate is the following: 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_get_attributes_types(Conn,RelationName,ListOfFields).
?- db_get_attributes_types(RelationName,ListOfFields). 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can use the
predicate `db_get_attributes types/2` or [db_get_attributes_types/3](@ref db_get_attributes_types), to
know what are the names and attributes types of the fields of a given
relation. For example: 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_get_attributes_types(myddas,'Hello World',LA).
LA = ['Number',integer,'Name',string,'Letter',string] ? 
yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where <tt>Hello World</tt> is the name of the relation and <tt>myddas</tt> is the
connection identifier. 

@section Number_of_Fields Number of Fields

<ul>
 <li>db_number_of_fields(+,?). @anchor db_number_of_fields



</li>
 <li>db_number_of_fields(+,+,?).

</li>
</ul>

The prototype for this
predicate is the following:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ?- db_number_of_fields(Conn,RelationName,Arity).
 ?- db_number_of_fields(RelationName,Arity).  
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You can use the predicate [db_number_of_fields/2](@ref db_number_of_fields) or
`db_number_of_fields/3` to know what is the arity of a given
relation. Example: 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_number_of_fields(myddas,'Hello World',Arity). 
Arity = 3 ? 
yes 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where `Hello World` is the name of the
relation and `myddas` is the connection identifier.

@section Describing_a_Relation Describing a Relation

<ul>
 <li>db_datalog_describe(+,+). @anchor db_datalog_describe



</li>
 <li>db_datalog_describe(+).

</li>
</ul>

The db `datalog_describe/2` predicate does not really returns any
value. It simply prints to the screen the result of the MySQL describe
command, the same way as `DESCRIBE` in the MySQL prompt would.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_datalog_describe(myddas,'Hello World'). 
+----------+----------+------+-----+---------+-------+ 
|   Field  |  Type    | Null | Key | Default | Extra |
+----------+----------+------+-----+---------+-------+
+  Number  | int(11)  | YES  |     |   NULL  |       |
+  Name    | char(10) | YES  |     |   NULL  |       |
+  Letter  | char(1)  | YES  |     |   NULL  |       |
+----------+----------+------+-----+---------+-------+
yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<ul>
 <li>db_describe(+,+). @anchor db_describe



</li>
 <li>db_describe(+).


</li>
</ul>

The `db_describe/3` predicate does the same action as
[db_datalog_describe/2](@ref db_datalog_describe) predicate but with one major
difference. The results are returned by backtracking. For example, the
last query:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ?- db_describe(myddas,'Hello World',Term). 
Term = tableInfo('Number',int(11),'YES','',null(0),'') ? ;
Term = tableInfo('Name',char(10),'YES','',null(1),'' ? ; 
Term = tableInfo('Letter',char(1),'YES','',null(2),'') ? ;
no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@section Enumerating_Relations Enumeration Relations

<ul>
 <li>db_datalog_show_tables(+).
</li>
 <li>db_datalog_show_tables
</li>
</ul>

If we need to know what relations exists in a given MySQL Schema, we can use
the `db_datalog_show_tables/1` predicate. As <tt>db_datalog_describe/2</tt>,
it does not returns any value, but instead prints to the screen the result of the
`SHOW TABLES` command, the same way as it would be in the MySQL prompt. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_datalog_show_tables(myddas).
+-----------------+
| Tables_in_guest |
+-----------------+
|   Hello World   |
+-----------------+ 
yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<ul>
 <li>db_show_tables(+, ?). @anchor db_show_tables



</li>
 <li>db_show_tables(?)


</li>
</ul>

The [db_show_tables/2](@ref db_show_tables) predicate does the same action as
`db_show_tables/1` predicate but with one major difference. The
results are returned by backtracking. For example, given the last query:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_show_tables(myddas,Table).
Table = table('Hello World') ? ;
no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@section The_MYDDAS_MySQL_Top_Level The MYDDAS MySQL Top Level

<ul>
 <li>db_top_level(+,+,+,+,+).  @anchor db_top_level



</li>
 <li>db_top_level(+,+,+,+).


</li>
</ul>

Through MYDDAS is also possible to access the MySQL Database Server, in
the same wthe mysql client. In this mode, is possible to query the
SQL server by just using the standard SQL language. This mode is exactly the same as 
different from the standard mysql client. We can use this
mode, by invoking the db top level/5. as one of the following:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_top_level(mysql,Connection,Host/Database,User,Password). 
?- db_top_level(mysql,Connection,Host/Database/Port,User,Password). 
?- db_top_level(mysql,Connection,Host/Database/UnixSocket,User,Password). 
?- db_top_level(mysql,Connection,Host/Database/Port/UnixSocket,User,Password).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Usage is similar as the one described for the [db_open/5](@ref db_open) predicate
discussed above. If the login is successful, automatically the prompt of
the mysql client will be used.  For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ?- db_top_level(mysql,con1,localhost/guest_db,guest,''). 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
opens a
connection identified by the `con1` atom, to an instance of a MySQL server
running on host `localhost`, using database guest `db` and user `guest` with
empty password. After this is possible to use MYDDAS as the mysql
client.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ?- db_top_level(mysql,con1,localhost/guest_db,guest,''). 
Reading table information for completion of table and column names
You can turn off this feature to get a quicker startup with -A

Welcome to the MySQL monitor.
Commands end with ; or \g.

Your MySQL connection id is 4468 to server version: 4.0.20
Type 'help;' or '\h' for help.
Type '\c' to clear the buffer. 
mysql> exit
Bye 
yes
?- 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@section Other_MYDDAS_Properties Other MYDDAS Properties 

<ul>
 <li>db_verbose(+). 
</li>
 <li>db_top_level(+,+,+,+).
</li>
</ul>

When we ask a question to YAP, using a predicate asserted by
[db_import/3](@ref db_import), or by [db_view/3](@ref db_view), this will generate a SQL
`QUERY`. If we want to see that query, we must to this at a given
point in our session on YAP.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_verbose(1).
yes 
?- 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we want to
disable this feature, we must call the `db_verbose/1` predicate with the value 0.

<ul>
 <li>db_module(?).  @anchor db_module



</li>
</ul>

When we create a new database predicate, by using [db_import/3](@ref db_import),
[db_view/3](@ref db_view) or [db_insert/3](@ref db_insert), that predicate will be asserted
by default on the `user` module. If we want to change this value, we can
use the [db_module/1](@ref db_module) predicate to do so.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_module(lists).
yes
?-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
By executing this predicate, all of the predicates asserted by the
predicates enumerated earlier will created in the lists module.
If we want to put back the value on default, we can manually put the
value user. Example: 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_module(user).
yes
?-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can also see in what module the predicates are being asserted by doing:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_module(X). 
X=user
yes
 ?-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<ul>
 <li>db_my_result_set(?). @anchor db_my_result_set



</li>
</ul>

The MySQL C API permits two modes for transferring the data generated by
a query to the client, in our case YAP. The first mode, and the default
mode used by the MYDDAS-MySQL, is to store the result. This mode copies all the
information generated to the client side.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_my_result_set(X).
X=store_result
yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The other mode that we can use is use result. This one uses the result
set created directly from the server. If we want to use this mode, he
simply do

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ?- db_my_result_set(use_result). 
yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
After this command, all
of the database predicates will use use result by default. We can change
this by doing again `db_my_result_set(store_result)`.  

<ul>
 <li>db_my_sql_mode(+Conn,?SQL_Mode). @anchor db_my_sql_mode



</li>
 <li>db_my_sql_mode(?SQL_Mode).


</li>
</ul>

The MySQL server allows the user to change the SQL mode. This can be
very useful for debugging proposes. For example, if we want MySQL server
not to ignore the INSERT statement warnings and instead of taking
action, report an error, we could use the following SQL mode.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ?-db_my_sql_mode(traditional). yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You can see the available SQL Modes at the MySQL homepage at
<http://www.mysql.org>.

@page Real Real::   Talking to the R language 

@copydoc real

@page Threads Threads

YAP implements a SWI-Prolog compatible multithreading
library. Like in SWI-Prolog, Prolog threads have their own stacks and
only share the Prolog <em>heap</em>: predicates, records, flags and other
global non-backtrackable data.  The package is based on the POSIX thread
standard (Butenhof:1997:PPT) used on most popular systems except
for MS-Windows.

@section Creating_and_Destroying_Prolog_Threads Creating and Destroying Prolog Threads

<ul>

 <li>thread_create(: _Goal_, - _Id_, + _Options_) @anchor thread_create



Create a new Prolog thread (and underlying C-thread) and start it
by executing  _Goal_.  If the thread is created successfully, the
thread-identifier of the created thread is unified to  _Id_.
 _Options_ is a list of options.  Currently defined options are:

<ul>
 <li>stack
Set the limit in K-Bytes to which the Prolog stacks of
this thread may grow.  If omitted, the limit of the calling thread is
used.  See also the  commandline `-S` option.

</li>
 <li>trail
Set the limit in K-Bytes to which the trail stack of this thread may
grow.  If omitted, the limit of the calling thread is used. See also the
commandline option `-T`.

</li>
 <li>alias
Associate an alias-name with the thread.  This named may be used to
refer to the thread and remains valid until the thread is joined
(see [thread_join/2](@ref thread_join)).

</li>
 <li>at_exit
Define an exit hook for the thread.  This hook is called when the thread
terminates, no matter its exit status.

</li>
 <li>detached
If `false` (default), the thread can be waited for using
[thread_join/2](@ref thread_join). [thread_join/2](@ref thread_join) must be called on this thread
to reclaim the all resources associated to the thread. If `true`,
the system will reclaim all associated resources automatically after the
thread finishes. Please note that thread identifiers are freed for reuse
after a detached thread finishes or a normal thread has been joined.
See also [thread_join/2](@ref thread_join) and [thread_detach/1](@ref thread_detach).
</li>
</ul>

The  _Goal_ argument is <em>copied</em> to the new Prolog engine.
This implies further instantiation of this term in either thread does
not have consequences for the other thread: Prolog threads do not share
data from their stacks.

</li>
 <li>thread_create(: _Goal_, - _Id_)


Create a new Prolog thread using default options. See [thread_create/3](@ref thread_create).

</li>
 <li>thread_create(: _Goal_)


Create a new Prolog detached thread using default options. See [thread_create/3](@ref thread_create).

</li>
 <li>thread_self(- _Id_) @anchor thread_self


Get the Prolog thread identifier of the running thread.  If the thread
has an alias, the alias-name is returned.

</li>
 <li>thread_join(+ _Id_, - _Status_) @anchor thread_join


Wait for the termination of thread with given  _Id_.  Then unify the
result-status of the thread with  _Status_.  After this call,
 _Id_ becomes invalid and all resources associated with the thread
are reclaimed.  Note that threads with the attribute `detached`
`true` cannot be joined.  See also [current_thread/2](@ref current_thread).

A thread that has been completed without [thread_join/2](@ref thread_join) being
called on it is partly reclaimed: the Prolog stacks are released and the
C-thread is destroyed. A small data-structure representing the
exit-status of the thread is retained until [thread_join/2](@ref thread_join) is called on
the thread.  Defined values for  _Status_ are:

<ul>
 <li>true
The goal has been proven successfully.

</li>
 <li>false
The goal has failed.

</li>
 <li>exception( _Term_)
The thread is terminated on an
exception.  See [print_message/2](@ref print_message) to turn system exceptions into
readable messages.

</li>
 <li>exited( _Term_)
The thread is terminated on [thread_exit/1](@ref thread_exit) using the argument  _Term_.
</li>
</ul>

</li>
 <li>thread_detach(+ _Id_) @anchor thread_detach


Switch thread into detached-state (see `detached` option at
[thread_create/3](@ref thread_create) at runtime.   _Id_ is the identifier of the thread
placed in detached state.

One of the possible applications is to simplify debugging. Threads that
are created as `detached` leave no traces if they crash. For
not-detached threads the status can be inspected using
[current_thread/2](@ref current_thread).  Threads nobody is waiting for may be created
normally and detach themselves just before completion.  This way they
leave no traces on normal completion and their reason for failure can be
inspected.

</li>
 <li>thread_yield @anchor thread_yield


Voluntarily relinquish the processor.

</li>
 <li>thread_exit(+ _Term_) @anchor thread_exit


Terminates the thread immediately, leaving `exited( _Term_)` as
result-state for [thread_join/2](@ref thread_join).  If the thread has the attribute
`detached` `true` it terminates, but its exit status cannot be
retrieved using [thread_join/2](@ref thread_join) making the value of  _Term_
irrelevant.  The Prolog stacks and C-thread are reclaimed.

</li>
 <li>thread_at_exit(: _Term_) @anchor thread_at_exit


Run  _Goal_ just before releasing the thread resources. This is to
be compared to `at_halt/1`, but only for the current
thread. These hooks are ran regardless of why the execution of the
thread has been completed. As these hooks are run, the return-code is
already available through [thread_property/2](@ref thread_property) using the result of
[thread_self/1](@ref thread_self) as thread-identifier. If you want to guarantee the 
execution of an exit hook no matter how the thread terminates (the thread 
can be aborted before reaching the [thread_at_exit/1](@ref thread_at_exit) call), consider
using instead the `at_exit/1` option of [thread_create/3](@ref thread_create). 

</li>
 <li>thread_setconcurrency(+ _Old_, - _New_) @anchor thread_setconcurrency


Determine the concurrency of the process, which is defined as the
maximum number of concurrently active threads. `Active' here means
they are using CPU time. This option is provided if the
thread-implementation provides
`pthread_setconcurrency()`. Solaris is a typical example of this
family. On other systems this predicate unifies  _Old_ to 0 (zero)
and succeeds silently.

</li>
 <li>thread_sleep(+ _Time_) @anchor thread_sleep


Make current thread sleep for  _Time_ seconds.  _Time_ may be an
integer or a floating point number. When time is zero or a negative value 
the call succeeds and returns immediately. This call should not be used if
alarms are also being used.
</li>
</ul>

@section Monitoring_Threads Monitoring Threads

Normal multi-threaded applications should not need these the predicates
from this section because almost any usage of these predicates is
unsafe. For example checking the existence of a thread before signalling
it is of no use as it may vanish between the two calls. Catching
exceptions using [catch/3](@ref catch) is the only safe way to deal with
thread-existence errors.

These predicates are provided for diagnosis and monitoring tasks.

<ul>
 <li>thread_property(? _Id_, ? _Property_) @anchor thread_property


Enumerates the properties of the specified thread.
Calling [thread_property/2](@ref thread_property) does not influence any thread.  See also
[thread_join/2](@ref thread_join).  For threads that have an alias-name, this name can
be used in  _Id_ instead of the numerical thread identifier.
 _Property_ is one of:

<ul>
 <li>status( _Status_)
The thread status of a thread (see below).

</li>
 <li>alias( _Alias_)
The thread alias, if it exists.

</li>
 <li>at_exit( _AtExit_)
The thread exit hook, if defined (not available if the thread is already terminated).

</li>
 <li>detached( _Boolean_)
The detached state of the thread.

</li>
 <li>stack( _Size_)
The thread stack data-area size.

</li>
 <li>trail( _Size_)
The thread trail data-area size.

</li>
 <li>system( _Size_)
The thread system data-area size.
</li>
</ul>

</li>
 <li>current_thread(+ _Id_, - _Status_) @anchor current_thread


Enumerates identifiers and status of all currently known threads.
Calling [current_thread/2](@ref current_thread) does not influence any thread.  See also
[thread_join/2](@ref thread_join).  For threads that have an alias-name, this name is
returned in  _Id_ instead of the numerical thread identifier.
 _Status_ is one of:

<ul>
 <li>running
The thread is running.  This is the initial status of a thread.  Please
note that threads waiting for something are considered running too.

</li>
 <li>false
The  _Goal_ of the thread has been completed and failed.

</li>
 <li>true
The  _Goal_ of the thread has been completed and succeeded.

</li>
 <li>exited( _Term_)
The  _Goal_ of the thread has been terminated using [thread_exit/1](@ref thread_exit)
with  _Term_ as argument.  If the underlying native thread has
exited (using pthread_exit())  _Term_ is unbound.

</li>
 <li>exception( _Term_)
The  _Goal_ of the thread has been terminated due to an uncaught
exception (see [throw/1](@ref throw) and [catch/3](@ref catch)).
</li>
</ul>

</li>
 <li>thread_statistics(+ _Id_, + _Key_, - _Value_) @anchor thread_statistics


Obtains statistical information on thread  _Id_ as `statistics/2`
does in single-threaded applications.  This call returns all keys
of `statistics/2`, although only information statistics about the
stacks and CPU time yield different values for each thread.

</li>
 <li>mutex_statistics @anchor mutex_statistics


Print usage statistics on internal mutexes and mutexes associated
with dynamic predicates.  For each mutex two numbers are printed:
the number of times the mutex was acquired and the number of
collisions: the number times the calling thread has to
wait for the mutex.  The collision-count is not available on
Windows as this would break portability to Windows-95/98/ME or
significantly harm performance.  Generally collision count is
close to zero on single-CPU hardware.

</li>
 <li>threads @anchor threads


Prints a table of current threads and their status.
</li>
</ul>

@section Thread_Communication Thread communication

@subsection Message_Queues Message Queues

Prolog threads can exchange data using dynamic predicates, database
records, and other globally shared data. These provide no suitable means
to wait for data or a condition as they can only be checked in an
expensive polling loop. <em>Message queues</em> provide a means for
threads to wait for data or conditions without using the CPU.

Each thread has a message-queue attached to it that is identified
by the thread. Additional queues are created using
`message_queue_create/2`.

<ul>

 <li>thread_send_message(+ _Term_) @anchor thread_send_message


Places  _Term_ in the message-queue of the thread running the goal. 
Any term can be placed in a message queue, but note that the term is 
copied to the receiving thread and variable-bindings are thus lost. 
This call returns immediately.

</li>
 <li>thread_send_message(+ _QueueOrThreadId_, + _Term_)

Place  _Term_ in the given queue or default queue of the indicated
thread (which can even be the message queue of itself (see
[thread_self/1](@ref thread_self)). Any term can be placed in a message queue, but note that
the term is copied to the receiving thread and variable-bindings are
thus lost. This call returns immediately.

If more than one thread is waiting for messages on the given queue and
at least one of these is waiting with a partially instantiated
 _Term_, the waiting threads are <em>all</em> sent a wakeup signal,
starting a rush for the available messages in the queue.  This behaviour
can seriously harm performance with many threads waiting on the same
queue as all-but-the-winner perform a useless scan of the queue. If
there is only one waiting thread or all waiting threads wait with an
unbound variable an arbitrary thread is restarted to scan the queue.




</li>
 <li>thread_get_message(? _Term_) @anchor thread_get_message


Examines the thread message-queue and if necessary blocks execution
until a term that unifies to  _Term_ arrives in the queue.  After
a term from the queue has been unified unified to  _Term_, the
term is deleted from the queue and this predicate returns.

Please note that not-unifying messages remain in the queue.  After
the following has been executed, thread 1 has the term `gnu`
in its queue and continues execution using  _A_ is `gnat`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   <thread 1>
   thread_get_message(a(A)),

   <thread 2>
   thread_send_message(b(gnu)),
   thread_send_message(a(gnat)),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See also [thread_peek_message/1](@ref thread_peek_message).

</li>
 <li>message_queue_create(? _Queue_) @anchor message_queue_create


If  _Queue_ is an atom, create a named queue.  To avoid ambiguity
on `thread_send_message/2`, the name of a queue may not be in use
as a thread-name.  If  _Queue_ is unbound an anonymous queue is
created and  _Queue_ is unified to its identifier.

</li>
 <li>message_queue_destroy(+ _Queue_) @anchor message_queue_destroy


Destroy a message queue created with [message_queue_create/1](@ref message_queue_create).  It is
<em>not</em> allows to destroy the queue of a thread.  Neither is it
allowed to destroy a queue other threads are waiting for or, for
anonymous message queues, may try to wait for later.

</li>
 <li>thread_get_message(+ _Queue_, ? _Term_)

As [thread_get_message/1](@ref thread_get_message), operating on a given queue. It is allowed to
peek into another thread's message queue, an operation that can be used
to check whether a thread has swallowed a message sent to it.

</li>
 <li>thread_peek_message(? _Term_) @anchor thread_peek_message


Examines the thread message-queue and compares the queued terms
with  _Term_ until one unifies or the end of the queue has been
reached.  In the first case the call succeeds (possibly instantiating
 _Term_.  If no term from the queue unifies this call fails.

</li>
 <li>thread_peek_message(+ _Queue_, ? _Term_)

As [thread_peek_message/1](@ref thread_peek_message), operating on a given queue. It is allowed to
peek into another thread's message queue, an operation that can be used
to check whether a thread has swallowed a message sent to it.

</li>
</ul>

Explicit message queues are designed with the <em>worker-pool</em> model
in mind, where multiple threads wait on a single queue and pick up the
first goal to execute.  Below is a simple implementation where the
workers execute arbitrary Prolog goals.  Note that this example provides
no means to tell when all work is done. This must be realised using
additional synchronisation.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%    create_workers(+Id, +N)
%    
%    Create a pool with given Id and number of workers.

create_workers(Id, N) :-
    message_queue_create(Id),
    forall(between(1, N, _),
           thread_create(do_work(Id), _, [])).

do_work(Id) :-
    repeat,
      thread_get_message(Id, Goal),
      (   catch(Goal, E, print_message(error, E))
      ->  true
      ;   print_message(error, goal_failed(Goal, worker(Id)))
      ),
    fail.

%    work(+Id, +Goal)
%    
%    Post work to be done by the pool

work(Id, Goal) :-
    thread_send_message(Id, Goal).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection Signalling_Threads Signalling Threads

These predicates provide a mechanism to make another thread execute some
goal as an <em>interrupt</em>.  Signalling threads is safe as these
interrupts are only checked at safe points in the virtual machine.
Nevertheless, signalling in multi-threaded environments should be
handled with care as the receiving thread may hold a <em>mutex</em>
(see [with_mutex/2](@ref with_mutex)).  Signalling probably only makes sense to start
debugging threads and to cancel no-longer-needed threads with [throw/1](@ref throw),
where the receiving thread should be designed carefully do handle
exceptions at any point.

<ul>
 <li>thread_signal(+ _ThreadId_, : _Goal_) @anchor thread_signal


Make thread  _ThreadId_ execute  _Goal_ at the first
opportunity.  In the current implementation, this implies at the first
pass through the <em>Call-port</em>. The predicate [thread_signal/2](@ref thread_signal)
itself places  _Goal_ into the signalled-thread's signal queue
and returns immediately.

Signals (interrupts) do not cooperate well with the world of
multi-threading, mainly because the status of mutexes cannot be
guaranteed easily.  At the call-port, the Prolog virtual machine
holds no locks and therefore the asynchronous execution is safe.

 _Goal_ can be any valid Prolog goal, including [throw/1](@ref throw) to make
the receiving thread generate an exception and [trace/0](@ref trace) to start
tracing the receiving thread.

</li>
</ul>

@subsection Threads_and_Dynamic_Predicates Threads and Dynamic Predicates

Besides queues threads can share and exchange data using dynamic
predicates. The multi-threaded version knows about two types of
dynamic predicates. By default, a predicate declared <em>dynamic</em>
(see [dynamic/1](@ref dynamic)) is shared by all threads. Each thread may
assert, retract and run the dynamic predicate. Synchronisation inside
Prolog guarantees the consistency of the predicate. Updates are
<em>logical</em>: visible clauses are not affected by assert/retract
after a query started on the predicate. In many cases primitive from
thread synchronisation should be used to ensure application invariants on
the predicate are maintained.

Besides shared predicates, dynamic predicates can be declared with the
[thread_local/1](@ref thread_local) directive. Such predicates share their
attributes, but the clause-list is different in each thread.

<ul>
 <li>thread_local( _+Functor/Arity_)  @anchor thread_local


related to the dynamic/1 directive.  It tells the system that the
predicate may be modified using [assert/1](@ref assert), [retract/1](@ref retract),
etc, during execution of the program.  Unlike normal shared dynamic
data however each thread has its own clause-list for the predicate.
As a thread starts, this clause list is empty.  If there are still
clauses as the thread terminates these are automatically reclaimed by
the system.  The `thread_local` property implies
the property `dynamic`.

Thread-local dynamic predicates are intended for maintaining
thread-specific state or intermediate results of a computation.

It is not recommended to put clauses for a thread-local predicate into
a file as in the example below as the clause is only visible from the
thread that loaded the source-file.  All other threads start with an
empty clause-list.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- thread_local
    foo/1.

foo(gnat).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
</ul>

@section Thread_Synchronisation Thread Synchronisation

All internal Prolog operations are thread-safe. This implies two Prolog
threads can operate on the same dynamic predicate without corrupting the
consistency of the predicate. This section deals with user-level
<em>mutexes</em> (called <em>monitors</em> in ADA or
<em>critical-sections</em> by Microsoft).  A mutex is a
<em>MUT</em>ual <em>EX</em>clusive device, which implies at most one thread
can <em>hold</em> a mutex.

Mutexes are used to realise related updates to the Prolog database.
With `related', we refer to the situation where a `transaction' implies
two or more changes to the Prolog database.  For example, we have a
predicate `address/2`, representing the address of a person and we want
to change the address by retracting the old and asserting the new
address.  Between these two operations the database is invalid: this
person has either no address or two addresses, depending on the
assert/retract order.

Here is how to realise a correct update:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- initialization
    mutex_create(addressbook).

change_address(Id, Address) :-
    mutex_lock(addressbook),
    retractall(address(Id, _)),
    asserta(address(Id, Address)),
    mutex_unlock(addressbook).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<ul>
 <li>mutex_create(? _MutexId_) @anchor mutex_create


Create a mutex.  if  _MutexId_ is an atom, a <em>named</em> mutex is
created.  If it is a variable, an anonymous mutex reference is returned.
There is no limit to the number of mutexes that can be created.

</li>
 <li>mutex_destroy(+ _MutexId_) @anchor mutex_destroy


Destroy a mutex.  After this call,  _MutexId_ becomes invalid and
further references yield an `existence_error` exception.

</li>
 <li>with_mutex(+ _MutexId_, : _Goal_) @anchor with_mutex


Execute  _Goal_ while holding  _MutexId_.  If  _Goal_ leaves
choicepoints, these are destroyed (as in [once/1](@ref once)).  The mutex is unlocked
regardless of whether  _Goal_ succeeds, fails or raises an exception.
An exception thrown by  _Goal_ is re-thrown after the mutex has been
successfully unlocked.  See also `mutex_create/2`.

Although described in the thread-section, this predicate is also
available in the single-threaded version, where it behaves simply as
[once/1](@ref once).

</li>
 <li>mutex_lock(+ _MutexId_) @anchor mutex_lock


Lock the mutex.  Prolog mutexes are <em>recursive</em> mutexes: they
can be locked multiple times by the same thread.  Only after unlocking
it as many times as it is locked, the mutex becomes available for
locking by other threads. If another thread has locked the mutex the
calling thread is suspended until to mutex is unlocked.

If  _MutexId_ is an atom, and there is no current mutex with that
name, the mutex is created automatically using [mutex_create/1](@ref mutex_create).  This
implies named mutexes need not be declared explicitly.

Please note that locking and unlocking mutexes should be paired
carefully. Especially make sure to unlock mutexes even if the protected
code fails or raises an exception. For most common cases use
[with_mutex/2](@ref with_mutex), which provides a safer way for handling Prolog-level
mutexes.

</li>
 <li>mutex_trylock(+ _MutexId_) @anchor mutex_trylock


As mutex_lock/1, but if the mutex is held by another thread, this
predicates fails immediately.

</li>
 <li>mutex_unlock(+ _MutexId_) @anchor mutex_unlock


Unlock the mutex. This can only be called if the mutex is held by the
calling thread. If this is not the case, a `permission_error`
exception is raised.

</li>
 <li>mutex_unlock_all @anchor mutex_unlock_all


Unlock all mutexes held by the current thread.  This call is especially
useful to handle thread-termination using [abort/0](@ref abort) or exceptions.  See
also [thread_signal/2](@ref thread_signal).

</li>
 <li>current_mutex(? _MutexId_, ? _ThreadId_, ? _Count_) @anchor current_mutex


Enumerates all existing mutexes.  If the mutex is held by some thread,
 _ThreadId_ is unified with the identifier of the holding thread and
 _Count_ with the recursive count of the mutex. Otherwise,
 _ThreadId_ is `[]` and  _Count_ is 0.
</li>
</ul>

@section Parallelism Parallelism

There has been a sizeable amount of work on an or-parallel
implementation for YAP, called  *YAPOr*. Most of this work has
been performed by Ricardo Rocha. In this system parallelism is exploited
implicitly by running several alternatives in or-parallel. This option
can be enabled from the `configure` script or by checking the
system's `Makefile`.

 *YAPOr* is still a very experimental system, going through rapid
development. The following restrictions are of note:

<ul>
 <li>*YAPOr* currently only supports the Linux/X86 and SPARC/Solaris
platforms. Porting to other Unix-like platforms should be straightforward.

</li>
 <li>*YAPOr* does not support parallel updates to the
data-base.

</li>
 <li>*YAPOr* does not support opening or closing of streams during
parallel execution.

</li>
 <li>Garbage collection and stack shifting are not supported in
 *YAPOr*.  

</li>
 <li>Built-ins that cause side-effects can only be executed when
left-most in the search-tree. There are no primitives to provide
asynchronous or cavalier execution of these built-ins, as in Aurora or
Muse.

</li>
 <li>YAP does not support voluntary suspension of work.
</li>
</ul>

We expect that some of these restrictions will be removed in future
releases.

@section Tabling Tabling

 *YAPTab* is the tabling engine that extends YAP's execution
model to support tabled evaluation for definite programs. YAPTab was
implemented by Ricardo Rocha and its implementation is largely based
on the ground-breaking design of the XSB Prolog system, which
implements the SLG-WAM. Tables are implemented using tries and YAPTab
supports the dynamic intermixing of batched scheduling and local
scheduling at the subgoal level. Currently, the following restrictions
are of note:

<ul>
 <li>YAPTab does not handle tabled predicates with loops through negation (undefined behaviour).
</li>
 <li>YAPTab does not handle tabled predicates with cuts (undefined behaviour).
</li>
 <li>YAPTab does not support coroutining (configure error).
</li>
 <li>YAPTab does not support tabling dynamic predicates (permission error).
</li>
</ul>

To experiment with YAPTab use `--enable-tabling` in the configure
script or add `-DTABLING` to `YAP_EXTRAS` in the system's
`Makefile`. We next describe the set of built-ins predicates
designed to interact with YAPTab and control tabled execution:

<ul>
 <li>table + _P_ @anchor table


Declares predicate  _P_ (or a list of predicates
 _P1_,..., _Pn_ or [ _P1_,..., _Pn_]) as a tabled
predicate.  _P_ must be written in the form
 _name/arity_. Examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- table son/3.
:- table father/2.
:- table mother/2.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 or

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- table son/3, father/2, mother/2.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 or

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- table [son/3, father/2, mother/2].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>is_tabled(+ _P_) @anchor is_tabled


Succeeds if the predicate  _P_ (or a list of predicates
 _P1_,..., _Pn_ or [ _P1_,..., _Pn_]), of the form
 _name/arity_, is a tabled predicate.

</li>
 <li>tabling_mode(+ _P_,? _Mode_) @anchor tabling_mode


Sets or reads the default tabling mode for a tabled predicate  _P_
(or a list of predicates  _P1_,..., _Pn_ or
[ _P1_,..., _Pn_]). The list of  _Mode_ options includes:

<ul>
 <li>batched
Defines that, by default, batched scheduling is the scheduling
strategy to be used to evaluated calls to predicate  _P_.
</li>
 <li>local
Defines that, by default, local scheduling is the scheduling
strategy to be used to evaluated calls to predicate  _P_.
</li>
 <li>exec_answers
Defines that, by default, when a call to predicate  _P_ is
already evaluated (completed), answers are obtained by executing
compiled WAM-like code directly from the trie data
structure. This reduces the loading time when backtracking, but
the order in which answers are obtained is undefined.
</li>
 <li>load_answers
Defines that, by default, when a call to predicate  _P_ is
already evaluated (completed), answers are obtained (as a
consumer) by loading them from the trie data structure. This
guarantees that answers are obtained in the same order as they
were found. Somewhat less efficient but creates less choice-points.
</li>
</ul>
The default tabling mode for a new tabled predicate is `batched`
and `exec_answers`. To set the tabling mode for all predicates at
once you can use the [yap_flag/2](@ref yap_flag) predicate as described next.

</li>
 <li>yap_flag(tabling_mode,? _Mode_)
Sets or reads the tabling mode for all tabled predicates. The list of
 _Mode_ options includes:

<ul>
 <li>default
Defines that (i) all calls to tabled predicates are evaluated
using the predicate default mode, and that (ii) answers for all
completed calls are obtained by using the predicate default mode.
</li>
 <li>batched
Defines that all calls to tabled predicates are evaluated using
batched scheduling. This option ignores the default tabling mode
of each predicate.
</li>
 <li>local
Defines that all calls to tabled predicates are evaluated using
local scheduling. This option ignores the default tabling mode
of each predicate.
</li>
 <li>exec_answers
Defines that answers for all completed calls are obtained by
executing compiled WAM-like code directly from the trie data
structure. This option ignores the default tabling mode
of each predicate.
</li>
 <li>load_answers
Defines that answers for all completed calls are obtained by
loading them from the trie data structure. This option ignores
the default tabling mode of each predicate.
</li>
</ul>

</li>
 <li>abolish_table(+ _P_) @anchor abolish_table


Removes all the entries from the table space for predicate  _P_ (or
a list of predicates  _P1_,..., _Pn_ or
[ _P1_,..., _Pn_]). The predicate remains as a tabled predicate.

</li>
 <li>abolish_all_tables/0 @anchor abolish_all_tables


Removes all the entries from the table space for all tabled
predicates. The predicates remain as tabled predicates.

</li>
 <li>show_table(+ _P_) @anchor show_table


Prints table contents (subgoals and answers) for predicate  _P_
(or a list of predicates  _P1_,..., _Pn_ or
[ _P1_,..., _Pn_]).

</li>
 <li>table_statistics(+ _P_) @anchor table_statistics


Prints table statistics (subgoals and answers) for predicate  _P_
(or a list of predicates  _P1_,..., _Pn_ or
[ _P1_,..., _Pn_]).

</li>
 <li>tabling_statistics/0 @anchor tabling_statistics


Prints statistics on space used by all tables.
</li>
</ul>

@section Low_Level_Tracing Tracing at Low Level

It is possible to follow the flow at abstract machine level if
YAP is compiled with the flag `LOW_LEVEL_TRACER`. Note
that this option is of most interest to implementers, as it quickly generates
an huge amount of information.

Low level tracing can be toggled from an interrupt handler by using the
option `T`. There are also two built-ins that activate and
deactivate low level tracing:

<ul>
 <li>start_low_level_trace @anchor start_low_level_trace


Begin display of messages at procedure entry and retry.

</li>
 <li>stop_low_level_trace @anchor stop_low_level_trace


Stop display of messages at procedure entry and retry.
</li>
</ul>

Note that this compile-time option will slow down execution.

@section Low_Level_Profiling Profiling the Abstract Machine

Implementors may be interested in detecting on which abstract machine
instructions are executed by a program. The `ANALYST` flag can give
WAM level information. Note that this option slows down execution very
substantially, and is only of interest to developers of the system
internals, or to system debuggers.

<ul>
 <li>reset_op_counters @anchor reset_op_counters


Reinitialize all counters.

</li>
 <li>show_op_counters(+ _A_) @anchor show_op_counters


Display the current value for the counters, using label  _A_. The
label must be an atom.

</li>
 <li>show_ops_by_group(+ _A_) @anchor show_ops_by_group


Display the current value for the counters, organized by groups, using
label  _A_. The label must be an atom.

</li>
</ul>

@section Debugging Debugging

@section Deb_Preds Debugging Predicates

The following predicates are available to control the debugging of
programs:

<ul>
 <li>debug

Switches the debugger on.

</li>
 <li>debugging @anchor debugging


Outputs status information about the debugger which includes the leash
mode and the existing spy-points, when the debugger is on.

</li>
 <li>nodebug @anchor nodebug


Switches the debugger off.

</li>
 <li>spy + _P_ @anchor spy


Sets spy-points on all the predicates represented by
 _P_.  _P_ can either be a single specification or a list of 
specifications. Each one must be of the form  _Name/Arity_ 
or  _Name_. In the last case all predicates with the name 
 _Name_ will be spied. As in C-Prolog, system predicates and 
predicates written in C, cannot be spied.

</li>
 <li>nospy + _P_ @anchor nospy


Removes spy-points from all predicates specified by  _P_.
The possible forms for  _P_ are the same as in `spy P`.

</li>
 <li>nospyall @anchor nospyall


Removes all existing spy-points.

</li>
 <li>leash(+ _M_) @anchor leash


Sets leashing mode to  _M_.
The mode can be specified as:

<ul>
 <li>full
prompt on Call, Exit, Redo and Fail
</li>
 <li>tight
prompt on Call, Redo and Fail
</li>
 <li>half
prompt on Call and Redo
</li>
 <li>loose
prompt on Call
</li>
 <li>off
never prompt
</li>
 <li>none
never prompt, same as `off`
</li>
</ul>
The initial leashing mode is `full`.

The user may also specify directly the debugger ports 
where he wants to be prompted. If the argument for leash 
is a number  _N_, each of lower four bits of the number is used to
control prompting at one the ports of the box model. The debugger will 
prompt according to the following conditions:

<ul>
 <li>
if `N/\\ 1 =\\= 0`  prompt on fail 
</li>
 <li>
if `N/\\ 2 =\\= 0` prompt on redo
</li>
 <li>
if `N/\\ 4 =\\= 0` prompt on exit
</li>
 <li>
if `N/\\ 8 =\\= 0` prompt on call
</li>
</ul>
Therefore, `leash(15)` is equivalent to `leash(full)` and
`leash(0)` is equivalent to `leash(off)`.

Another way of using `leash` is to give it a list with the names of
the ports where the debugger should stop. For example,
`leash([call,exit,redo,fail])` is the same as `leash(full)` or
`leash(15)` and `leash([fail])` might be used instead of
`leash(1)`.

</li>
 <li>spy_write(+ _Stream_,Term) @anchor spy_write


If defined by the user, this predicate will be used to print goals by
the debugger instead of `write/2`.

</li>
 <li>trace @anchor trace


Switches on the debugger and starts tracing.

</li>
 <li>notrace @anchor notrace


Ends tracing and exits the debugger. This is the same as
[nodebug/0](@ref nodebug).

</li>
</ul>

@section Deb_Interaction Interacting with the debugger

Debugging with YAP is similar to debugging with C-Prolog. Both systems
include a procedural debugger, based on Byrd's four port model. In this
model, execution is seen at the procedure level: each activation of a
procedure is seen as a box with control flowing into and out of that
box.

In the four port model control is caught at four key points: before 
entering the procedure, after exiting the procedure (meaning successful 
evaluation of all queries activated by the procedure), after backtracking but 
before trying new alternative to the procedure and after failing the 
procedure. Each one of these points is named a port:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@group
           *--------------------------------------*
   Call    |                                      |    Exit
---------> +  descendant(X,Y) :- offspring(X,Y).  + --------->
           |                                      |
           |  descendant(X,Z) :-                  |
<--------- +     offspring(X,Y), descendant(Y,Z). + <---------
   Fail    |                                      |    Redo
           *--------------------------------------*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<ul>

 <li>Call
The call port is activated before initial invocation of
procedure. Afterwards, execution will try to match the goal with the
head of existing clauses for the procedure.
</li>
 <li>Exit
This port is activated if the procedure succeeds.
Control will  now leave the procedure and return to its ancestor.
</li>
 <li>Redo
if the goal, or goals, activated after the call port
fail  then backtracking will eventually return control to this procedure
through  the redo port.
</li>
 <li>Fail
If all clauses for this predicate fail, then the
invocation fails,  and control will try to redo the ancestor of this
invocation.
</li>
</ul>

To start debugging, the user will either call `trace` or spy the
relevant procedures, entering debug mode, and start execution of the
program. When finding the first spy-point, YAP's debugger will take
control and show a message of the form:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* (1)  call:  quicksort([1,2,3],_38) ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The debugger message will be shown while creeping, or at spy-points,
and it includes four or five fields:

<ul>
 <li>
The first three characters are used to point out special states of the
debugger. If the port is exit and the first character is '?', the
current call is non-deterministic, that is, it still has alternatives to
be tried. If the second character is a `\*`, execution is at a
spy-point. If the third character is a `\>`, execution has returned
either from a skip, a fail or a redo command.
</li>
 <li>
The second field is the activation number, and uniquely identifies the
activation. The number will start from 1 and will be incremented for
each activation found by the debugger.
</li>
 <li>
In the third field, the debugger shows the active port.
</li>
 <li>
The fourth field is the goal. The goal is written by
`write_term/3` on the standard error stream, using the options
given by [debugger_print_options](@ref debugger_print_options).
</li>
</ul>

If the active port is leashed, the debugger will prompt the user with a
`?`, and wait for a command. A debugger command is just a
character, followed by a return. By default, only the call and redo
entries are leashed, but the [leash/1](@ref leash) predicate can be used in
order to make the debugger stop where needed.

There are several commands available, but the user only needs to 
remember the help command, which is `h`. This command shows all the 
available options, which are:

<ul>
 <li>c - creep
this command makes YAP continue execution and stop at the next
leashed port.
</li>
 <li>return - creep
the same as c
</li>
 <li>l - leap
YAP will execute until it meets a port for a spied predicate; this mode
keeps all computation history for debugging purposes, so it is more
expensive than standard execution. Use <tt>k</tt> or <tt>z</tt> for fast execution.
</li>
 <li>k - quasi-leap
similar to leap but faster since the computation history is
not kept; useful when leap becomes too slow.
</li>
 <li>z - zip
same as <tt>k</tt>
</li>
 <li>s - skip
YAP will continue execution without showing any messages until
returning to the current activation. Spy-points will be  ignored in this
mode. Note that this command keeps all debugging history, use <tt>t</tt> for fast execution. This command is meaningless, and therefore illegal, in the fail
and exit ports.
</li>
 <li>t - fast-skip
similar to skip but faster since computation history is not
kept; useful if skip becomes slow.
</li>
 <li>f [ _GoalId_] - fail
If given no argument, forces YAP to fail the goal, skipping the fail
port and backtracking to the parent. 
If <tt>f</tt> receives a goal number as
the argument, the command fails all the way to the goal. If goal  _GoalId_ has completed execution, YAP fails until meeting the first active ancestor.
</li>
 <li>r [ _GoalId_] - retry
This command forces YAP to jump back call to the port. Note that any
side effects of the goal cannot be undone. This command is not available
at the call port.  If <tt>f</tt> receives a goal number as the argument, the
command retries goal  _GoalId_ instead. If goal  _GoalId_ has
completed execution, YAP fails until meeting the first active ancestor.

</li>
 <li>a - abort
execution will be aborted, and the interpreter will return to the
top-level. YAP disactivates debug mode, but spypoints are not removed.
</li>
 <li>n - nodebug
stop debugging and continue execution. The command will not clear active
spy-points.
</li>
 <li>e - exit
leave YAP.
</li>
 <li>h - help
show the debugger commands.
</li>
 <li>! Query
execute a query. YAP will not show the result of the query.
</li>
 <li>b - break
break active execution and launch a break level. This is  the same as `!break`.
</li>
 <li>+ - spy this goal
start spying the active goal. The same as `! spy  G` where  _G_
is the active goal.
</li>
 <li>- - nospy this goal
stop spying the active goal. The same as `! nospy G` where  _G_ is
the active goal.
</li>
 <li>p - print
shows the active goal using print/1
</li>
 <li>d - display
shows the active goal using display/1
</li>
 <li>\<Depth - debugger write depth
sets the maximum write depth, both for composite terms and lists, that
will be used by the debugger. For more
information about `write_depth/2` ( (see [Input/Output Control](@ref InputOutput_Control))).
</li>
 <li>\< - full term
resets to the default of ten the debugger's maximum write depth. For
more information about `write_depth/2` ( (see [Input/Output Control](@ref InputOutput_Control))).
</li>
 <li>A - alternatives
show the list of backtrack points in the current execution. 
</li>
 <li>g [ _N_] 
show the list of ancestors in the current debugging environment. If it
receives  _N_, show the first  _N_ ancestors.
</li>
</ul>

The debugging information, when fast-skip `quasi-leap` is used, will
be lost.

@page Efficiency Efficiency Considerations

We next discuss several issues on trying to make Prolog programs run
fast in YAP. We assume two different programming styles:

<ul>
 <li>Execution of <em>deterministic</em> programs often
boils down to a recursive loop of the form:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
loop(Env) :-
        do_something(Env,NewEnv),
        loop(NewEnv).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
</ul>

@section Indexing Indexing

The indexation mechanism restricts the set of clauses to be tried in a
procedure by using information about the status of the instantiated
arguments of the goal.  These arguments are then used as a key,
selecting a restricted set of a clauses from all the clauses forming the
procedure.

As an example, the two clauses for concatenate:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
concatenate([],L,L).
concatenate([H|T],A,[H|NT]) :- concatenate(T,A,NT).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the first argument for the goal is a list, then only the second clause 
is of interest. If the first argument is the nil atom, the system needs to 
look only for the first clause. The indexation generates instructions that 
test the value of the first argument, and then proceed to a selected clause, 
or group of clauses.

Note that if the first argument was a free variable, then both clauses 
should be tried. In general, indexation will not be useful if the first 
argument is a free variable.

When activating a predicate, a Prolog system needs to store state 
information. This information, stored in a structure known as choice point 
or fail point, is necessary when backtracking to other clauses for the 
predicate. The operations of creating and using a choice point are very 
expensive, both in the terms of space used and time spent.
Creating a choice point is not necessary if there is only a clause for 
the predicate as there are no clauses to backtrack to. With indexation, this 
situation is extended: in the example, if the first argument was the atom 
nil, then only one clause would really be of interest, and it is pointless to 
create a choice point. This feature is even more useful if the first argument 
is a list: without indexation, execution would try the first clause, creating 
a choice point. The clause would fail, the choice point would then be used to 
restore the previous state of the computation and the second clause would 
be tried. The code generated by the indexation mechanism would behave 
much more efficiently: it would test the first argument and see whether it 
is a list, and then proceed directly to the second clause.

An important side effect concerns the use of "cut". In the above 
example, some programmers would use a "cut" in the first clause just to 
inform the system that the predicate is not backtrackable and force the 
removal the choice point just created. As a result, less space is needed but 
with a great loss in expressive power: the "cut" would prevent some uses of 
the procedure, like generating lists through backtracking. Of course, with 
indexation the "cut" becomes useless: the choice point is not even created.

Indexation is also very important for predicates with a large number 
of clauses that are used like tables:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logician(aristoteles,greek).
logician(frege,german).
logician(russel,english).
logician(godel,german).
logician(whitehead,english).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An interpreter like C-Prolog, trying to answer the query:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- logician(godel,X).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

would blindly follow the standard Prolog strategy, trying first the
first clause, then the second, the third and finally finding the
relevant clause.  Also, as there are some more clauses after the
important one, a choice point has to be created, even if we know the
next clauses will certainly fail. A "cut" would be needed to prevent
some possible uses for the procedure, like generating all logicians.  In
this situation, the indexing mechanism generates instructions that
implement a search table. In this table, the value of the first argument
would be used as a key for fast search of possibly matching clauses. For
the query of the last example, the result of the search would be just
the fourth clause, and again there would be no need for a choice point.

If the first argument is a complex term, indexation will select clauses
just by testing its main functor. However, there is an important
exception: if the first argument of a clause is a list, the algorithm
also uses the list's head if not a variable. For instance, with the
following clauses,

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rules([],B,B).
rules([n(N)|T],I,O) :- rules_for_noun(N,I,N), rules(T,N,O).
rules([v(V)|T],I,O) :- rules_for_verb(V,I,N), rules(T,N,O).
rules([q(Q)|T],I,O) :- rules_for_qualifier(Q,I,N), rules(T,N,O).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if the first argument of the goal is a list, its head will be tested, and only 
the clauses matching it will be tried during execution.

Some advice on how to take a good advantage of this mechanism:

<ul>

 <li>
Try to make the first argument an input argument.

</li>
 <li>
Try to keep together all clauses whose first argument is not a 
variable, that will decrease the number of tests since the other clauses are 
always tried.

</li>
 <li>
Try to avoid predicates having a lot of clauses with the same key. 
For instance, the procedure:

</li>
</ul>

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type(n(mary),person).
type(n(john), person).
type(n(chair),object).
type(v(eat),active).
type(v(rest),passive).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

becomes more efficient with:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type(n(N),T) :- type_of_noun(N,T).
type(v(V),T) :- type_of_verb(V,T).

type_of_noun(mary,person).
type_of_noun(john,person).
type_of_noun(chair,object).

type_of_verb(eat,active).
type_of_verb(rest,passive).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@page ChYInterface C Language interface to YAP

YAP provides the user with three facilities for writing
predicates in a language other than Prolog. Under Unix systems,
most language implementations were linkable to `C`, and the first interface exported  the YAP machinery to the C language. YAP also implements most of the SWI-Prolog foreign language interface.
This gives portability with a number of SWI-Prolog packages. Last, a new C++ based interface is 
being designed to work with the swig (@url(www.swig.org}) interface compiler.

<ul>
<li> The original YAP C-interface exports the YAP engine.
</li>
<li>The @ref swi-c-interface emulates Jan Wielemaker's SWI foreign language interface.
</li>
<li>The @ref  yap-cplus-interface is desiged to interface with Object-Oriented systems.
</li>
</ul>


Before describing in full detail how to interface to C code, we will examine 
a brief example.

Assume the user requires a predicate `my_process_id(Id)` which succeeds
when  _Id_ unifies with the number of the process under which YAP is running.

In this case we will create a `my_process.c` file containing the
C-code described below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
#include "YAP/YapInterface.h"

static int my_process_id(void) 
{
     YAP_Term pid = YAP_MkIntTerm(getpid());
     YAP_Term out = YAP_ARG1;
     return(YAP_Unify(out,pid));
}

void init_my_predicates()
{
     YAP_UserCPredicate("my_process_id",my_process_id,1);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The commands to compile the above file depend on the operating
system. Under Linux (i386 and Alpha) you should use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      gcc -c -shared -fPIC my_process.c
      ld -shared -o my_process.so my_process.o
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under WIN32 in a MINGW/CYGWIN environment, using the standard
installation path you should use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      gcc -mno-cygwin  -I "c:/Yap/include" -c my_process.c
      gcc -mno-cygwin "c:/Yap/bin/yap.dll" --shared -o my_process.dll my_process.o
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under WIN32 in a pure CYGWIN environment, using the standard
installation path, you should use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      gcc -I/usr/local -c my_process.c
      gcc -shared -o my_process.dll my_process.o /usr/local/bin/yap.dll
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under Solaris2 it is sufficient to use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      gcc  -fPIC -c my_process.c
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under SunOS it is sufficient to use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      gcc -c my_process.c
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under Digital Unix you need to create a `so` file. Use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      gcc tst.c -c -fpic
      ld my_process.o -o my_process.so -shared -expect_unresolved '*'
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and replace my `process.so` for my `process.o` in the
remainder of the example.
And could be loaded, under YAP, by executing the following Prolog goal

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      load_foreign_files(['my_process'],[],init_my_predicates).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that since YAP4.3.3 you should not give the suffix for object
files. YAP will deduce the correct suffix from the operating system it
is running under.

After loading that file the following Prolog goal

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       my_process_id(N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
would unify N with the number of the process under which YAP is running.

Having presented a full example, we will now examine in more detail the
contents of the C source code file presented above.

The include statement is used to make available to the C source code the
macros for the handling of Prolog terms and also some YAP public
definitions.

The function `my_process_id` is the implementation, in C, of the
desired predicate.  Note that it returns an integer denoting the success
of failure of the goal and also that it has no arguments even though the
predicate being defined has one.
In fact the arguments of a Prolog predicate written in C are accessed
through macros, defined in the include file, with names  _YAP_ARG1_,
 _YAP_ARG2_, ...,  _YAP_ARG16_ or with  _YAP_A_( _N_)
where  _N_ is the argument number (starting with 1).  In the present
case the function uses just one local variable of type `YAP_Term`, the
type used for holding YAP terms, where the integer returned by the
standard unix function `getpid()` is stored as an integer term (the
conversion is done by `YAP_MkIntTerm(Int))`. Then it calls the
pre-defined routine `YAP_Unify(YAP_Term, YAP_Term)` which in turn returns an
integer denoting success or failure of the unification.

The role of the procedure `init_my_predicates` is to make known to
YAP, by calling [YAP_UserCPredicate](@ref YAP_UserCPredicate), the predicates being
defined in the file.  This is in fact why, in the example above,
`init_my_predicates` was passed as the third argument to
`load_foreign_files/3`.

The rest of this appendix describes exhaustively how to interface C to YAP.

@section Manipulating_Terms Terms

This section provides information about the primitives available to the C
programmer for manipulating Prolog terms.

Several C typedefs are included in the header file `yap/YAPInterface.h` to
describe, in a portable way, the C representation of Prolog terms.
The user should write is programs using this macros to ensure portability of
code across different versions of YAP.

The more important typedef is  _YAP_Term_ which is used to denote the
type of a Prolog term.

Terms, from a point of view of the C-programmer,  can be classified as
follows

<ul>
 <li>uninstantiated variables
</li>
 <li>instantiated variables
</li>
 <li>integers
</li>
 <li>floating-point numbers
</li>
 <li>database references
</li>
 <li>atoms
</li>
 <li>pairs (lists)
</li>
 <li>compound terms
</li>
</ul>

The primitive

<ul>
 <li>YAP_Bool YAP_IsVarTerm(YAP_Term  _t_) @anchor YAP_IsVarTerm

returns true iff its argument is an uninstantiated variable. Conversely the
primitive
</li>
 <li>YAP_Bool YAP_NonVarTerm(YAP_Term  _t_) @anchor YAP_IsNonVarTerm

returns true iff its argument is not a variable.
</li>
</ul>
 

The user can create a new uninstantiated variable using the primitive

<ul>
 <li>YAP_Term  YAP_MkVarTerm()
</li>
</ul>

The following primitives can be used to discriminate among the different types
of non-variable terms:

<ul>
 <li>YAP_Bool YAP_IsIntTerm(YAP_Term  _t_) @anchor YAP_IsIntTerm

</li>
 <li>YAP_Bool YAP_IsFloatTerm(YAP_Term  _t_) @anchor YAP_IsFloatTerm

</li>
 <li>YAP_Bool YAP_IsDbRefTerm(YAP_Term  _t_) @anchor YAP_IsDBRefTerm

</li>
 <li>YAP_Bool YAP_IsAtomTerm(YAP_Term  _t_) @anchor YAP_IsAtomTerm

</li>
 <li>YAP_Bool YAP_IsPairTerm(YAP_Term  _t_) @anchor YAP_IsPairTerm

</li>
 <li>YAP_Bool YAP_IsApplTerm(YAP_Term  _t_) @anchor YAP_IsApplTerm

</li>
 <li>YAP_Bool YAP_IsCompoundTerm(YAP_Term  _t_) @anchor YAP_IsCompoundTerm

</li>
</ul>

The next primitive gives the type of a Prolog term:

<ul>
 <li>YAP_tag_t YAP_TagOfTerm(YAP_Term  _t_)
</li>
</ul>
The set of possible values is an enumerated type, with the following values:

<ul>
 <li>`YAP_TAG_ATT`: an attributed variable
</li>
 <li>`YAP_TAG_UNBOUND`: an unbound variable
</li>
 <li>`YAP_TAG_REF`: a reference to a term
</li>
 <li>`YAP_TAG_PAIR`: a list
</li>
 <li>`YAP_TAG_ATOM`: an atom
</li>
 <li>`YAP_TAG_INT`: a small integer
</li>
 <li>`YAP_TAG_LONG_INT`: a word sized integer
</li>
 <li>`YAP_TAG_BIG_INT`: a very large integer
</li>
 <li>`YAP_TAG_RATIONAL`: a rational number
</li>
 <li>`YAP_TAG_FLOAT`: a floating point number
</li>
 <li>`YAP_TAG_OPAQUE`: an opaque term
</li>
 <li>`YAP_TAG_APPL`: a compound term
</li>
</ul>

Next, we mention the primitives that allow one to destruct and construct
terms. All the above primitives ensure that their result is
\a dereferenced, i.e. that it is not a pointer to another term.

The following primitives are provided for creating an integer term from an
integer and to access the value of an integer term.

<ul>
 <li>YAP_Term YAP_MkIntTerm(YAP_Int   _i_) @anchor YAP_MkIntTerm

</li>
 <li>YAP_Int  YAP_IntOfTerm(YAP_Term  _t_) @anchor YAP_IntOfTerm

</li>
</ul>
where `YAP_Int` is a typedef for the C integer type appropriate for
the machine or compiler in question (normally a long integer). The size
of the allowed integers is implementation dependent but is always
greater or equal to 24 bits: usually 32 bits on 32 bit machines, and 64
on 64 bit machines.

The two following primitives play a similar role for floating-point terms

<ul>
 <li>YAP_Term YAP_MkFloatTerm(YAP_flt  _double_) @anchor YAP_MkFloatTerm


</li>
 <li>YAP_flt  YAP_FloatOfTerm(YAP_Term  _t_) @anchor YAP_FloatOfTerm

</li>
</ul>
where `flt` is a typedef for the appropriate C floating point type,
nowadays a `double`

The following primitives are provided for verifying whether a term is
a big int, creating a term from a big integer and to access the value
of a big int from a term.

<ul>
 <li>YAP_Bool YAP_IsBigNumTerm(YAP_Term  _t_) @anchor YAP_IsBigNumTerm

</li>
 <li>YAP_Term YAP_MkBigNumTerm(void  \* _b_) @anchor YAP_MkBigNumTerm

</li>
 <li>void \*YAP_BigNumOfTerm(YAP_Term  _t_, void \* _b_) @anchor YAP_BigNumOfTerm

</li>
</ul>
YAP must support bignum for the configuration you are using (check the
YAP configuration and setup). For now, YAP only supports the GNU GMP
library, and `void \*` will be a cast for `mpz_t`. Notice
that [YAP_BigNumOfTerm](@ref YAP_BigNumOfTerm) requires the number to be already
initialised. As an example, we show how to print a bignum:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static int
p_print_bignum(void)
{
  mpz_t mz;
  if (!YAP_IsBigNumTerm(YAP_ARG1))
    return FALSE;

  mpz_init(mz);
  YAP_BigNumOfTerm(YAP_ARG1, mz);
  gmp_printf("Shows up as %Zd\n", mz);
  mpz_clear(mz);
  return TRUE;
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Currently, no primitives are supplied to users for manipulating data base
references. 

A special typedef `YAP_Atom` is provided to describe Prolog
\a atoms (symbolic constants). The two following primitives can be used
to manipulate atom terms

<ul>
 <li>YAP_Term YAP_MkAtomTerm(YAP_Atom at) @anchor YAP_MkAtomTerm

</li>
 <li>YAP_Atom YAP_AtomOfTerm(YAP_Term  _t_) @anchor YAP_AtomOfTerm

</li>
</ul>
The following primitives are available for associating atoms with their
names 

<ul>
 <li>YAP_Atom  YAP_LookupAtom(char \*  _s_) @anchor YAP_LookupAtom

</li>
 <li>YAP_Atom  YAP_FullLookupAtom(char \*  _s_) @anchor YAP_FullLookupAtom

</li>
 <li>char     \*YAP_AtomName(YAP_Atom  _t_) @anchor YAP_AtomName

</li>
</ul>
The function [YAP_LookupAtom](@ref YAP_LookupAtom) looks up an atom in the standard hash
table. The function [YAP_FullLookupAtom](@ref YAP_FullLookupAtom) will also search if the
atom had been "hidden": this is useful for system maintenance from C
code. The functor [YAP_AtomName](@ref YAP_AtomName) returns a pointer to the string
for the atom.

The following primitives handle constructing atoms from strings with
wide characters, and vice-versa:

<ul>
 <li>YAP_Atom  YAP_LookupWideAtom(wchar_t \*  _s_) @anchor YAP_LookupWideAtom

</li>
 <li>wchar_t  \*YAP_WideAtomName(YAP_Atom  _t_) @anchor YAP_WideAtomName

</li>
</ul>

The following primitive tells whether an atom needs wide atoms in its
representation:

<ul>
 <li>int  YAP_IsWideAtom(YAP_Atom  _t_) @anchor YAP_IsIsWideAtom

</li>
</ul>

The following primitive can be used to obtain the size of an atom in a
representation-independent way: 

<ul>
 <li>int      YAP_AtomNameLength(YAP_Atom  _t_) @anchor YAP_AtomNameLength

</li>
</ul>

The next routines give users some control over  the atom
garbage collector. They allow the user to guarantee that an atom is not
to be garbage collected (this is important if the atom is hold
externally to the Prolog engine, allow it to be collected, and call a
hook on garbage collection:

<ul>
 <li>int  YAP_AtomGetHold(YAP_Atom  _at_) @anchor YAP_AtomGetHold

</li>
 <li>int  YAP_AtomReleaseHold(YAP_Atom  _at_) @anchor YAP_AtomReleaseHold

</li>
 <li>int  YAP_AGCRegisterHook(YAP_AGC_hook  _f_) @anchor YAP_AGCHook

</li>
</ul>

A \a pair is a Prolog term which consists of a tuple of two Prolog
terms designated as the \a head and the \a tail of the term. Pairs are
most often used to build <em>lists</em>. The following primitives can be
used to manipulate pairs:

<ul>
 <li>YAP_Term  YAP_MkPairTerm(YAP_Term  _Head_, YAP_Term  _Tail_) @anchor YAP_MkPairTerm

</li>
 <li>YAP_Term  YAP_MkNewPairTerm(void) @anchor YAP_MkNewPairTerm

</li>
 <li>YAP_Term  YAP_HeadOfTerm(YAP_Term  _t_) @anchor YAP_HeadOfTerm

</li>
 <li>YAP_Term  YAP_TailOfTerm(YAP_Term  _t_) @anchor YAP_TailOfTerm

</li>
 <li>YAP_Term  YAP_MkListFromTerms(YAP_Term \* _pt_, YAP_Int \* _sz_) @anchor YAP_MkListFromTerms

</li>
</ul>
One can construct a new pair from two terms, or one can just build a
pair whose head and tail are new unbound variables. Finally, one can
fetch the head or the tail.

The last function supports the common operation of constructing a list from an
array of terms of size  _sz_ in a simple sweep.

Notice that the list constructors can call the garbage collector if
there is not enough space in the global stack. 

A \a compound term consists of a \a functor and a sequence of terms with
length equal to the \a arity of the functor. A functor, described in C by
the typedef `Functor`, consists of an atom and of an integer.
The following primitives were designed to manipulate compound terms and 
functors

<ul>
 <li>YAP_Term     YAP_MkApplTerm(YAP_Functor  _f_, unsigned long int  _n_, YAP_Term[]  _args_) @anchor YAP_MkApplTerm

</li>
 <li>YAP_Term     YAP_MkNewApplTerm(YAP_Functor  _f_, int  _n_) @anchor YAP_MkNewApplTerm

</li>
 <li>YAP_Term     YAP_ArgOfTerm(int argno,YAP_Term  _ts_) @anchor YAP_ArgOfTerm

</li>
 <li>YAP_Term    \*YAP_ArgsOfTerm(YAP_Term  _ts_) @anchor YAP_ArgsOfTerm

</li>
 <li>YAP_Functor  YAP_FunctorOfTerm(YAP_Term  _ts_) @anchor YAP_FunctorOfTerm

</li>
</ul>
The [YAP_MkApplTerm](@ref YAP_MkApplTerm) function constructs a new term, with functor
 _f_ (of arity  _n_), and using an array  _args_ of  _n_
terms with  _n_ equal to the arity of the
functor. [YAP_MkNewApplTerm](@ref YAP_MkNewApplTerm) builds up a compound term whose
arguments are unbound variables. [YAP_ArgOfTerm](@ref YAP_ArgOfTerm) gives an argument
to a compound term. `argno` should be greater or equal to 1 and
less or equal to the arity of the functor.  [YAP_ArgsOfTerm](@ref YAP_ArgsOfTerm)
returns a pointer to an array of arguments.

Notice that the compound term constructors can call the garbage
collector if there is not enough space in the global stack.

YAP allows one to manipulate the functors of compound term. The function
[YAP_FunctorOfTerm](@ref YAP_FunctorOfTerm) allows one to obtain a variable of type
`YAP_Functor` with the functor to a term. The following functions
then allow one to construct functors, and to obtain their name and arity. 

<ul>
 <li>YAP_Functor  YAP_MkFunctor(YAP_Atom  _a_,unsigned long int  _arity_)
</li>
 <li>YAP_Atom     YAP_NameOfFunctor(YAP_Functor  _f_)
</li>
 <li>YAP_Int      YAP_ArityOfFunctor(YAP_Functor  _f_)
</li>
</ul>

Note that the functor is essentially a pair formed by an atom, and
arity.

Constructing terms in the stack may lead to overflow. The routine

<ul>
 <li>int          YAP_RequiresExtraStack(size_t  _min_) @anchor YAP_Unify

</li>
</ul>
verifies whether you have at least  _min_ cells free in the stack,
and it returns true if it has to ensure enough memory by calling the
garbage collector and or stack shifter. The routine returns false if no
memory is needed, and a negative number if it cannot provide enough
memory.

You can set  _min_ to zero if you do not know how much room you need
but you do know you do not need a big chunk at a single go. Usually, the routine
would usually be called together with a long-jump to restart the
code. Slots can also be used if there is small state.

@section Unifying_Terms Unification

YAP provides a single routine to attempt the unification of two Prolog
terms. The routine may succeed or fail:

<ul>
 <li>Int      YAP_Unify(YAP_Term  _a_, YAP_Term  _b_) @anchor YAP_StringToBuffer

</li>
</ul>
The routine attempts to unify the terms  _a_ and
 _b_ returning `TRUE` if the unification succeeds and `FALSE`
otherwise.

@section Manipulating_Strings Strings

The YAP C-interface now includes an utility routine to copy a string
represented as a list of a character codes to a previously allocated buffer

<ul>
 <li>int YAP_StringToBuffer(YAP_Term  _String_, char \* _buf_, unsigned int  _bufsize_) @anchor YAP_BufferToString

</li>
</ul>
The routine copies the list of character codes  _String_ to a
previously allocated buffer  _buf_. The string including a
terminating null character must fit in  _bufsize_ characters,
otherwise the routine will simply fail. The  _StringToBuffer_ routine
fails and generates an exception if  _String_ is not a valid string.

The C-interface also includes utility routines to do the reverse, that
is, to copy a from a buffer to a list of character codes, to a
difference list,  or to a list of
character atoms. The routines work either on strings of characters or
strings of wide characters:

<ul>
 <li>YAP_Term YAP_BufferToString(char \* _buf_)
</li>
 <li>YAP_Term YAP_NBufferToString(char \* _buf_, size_t  _len_)
</li>
 <li>YAP_Term YAP_WideBufferToString(wchar_t \* _buf_)
</li>
 <li>YAP_Term YAP_NWideBufferToString(wchar_t \* _buf_, size_t  _len_)
</li>
 <li>YAP_Term YAP_BufferToAtomList(char \* _buf_)
</li>
 <li>YAP_Term YAP_NBufferToAtomList(char \* _buf_, size_t  _len_)
</li>
 <li>YAP_Term YAP_WideBufferToAtomList(wchar_t \* _buf_)
</li>
 <li>YAP_Term YAP_NWideBufferToAtomList(wchar_t \* _buf_, size_t  _len_) @anchor YAP_ReadBuffer

</li>
</ul>
Users are advised to use the  _N_ version of the routines. Otherwise,
the user-provided string must include a terminating null character.

The C-interface function calls the parser on a sequence of characters
stored at  _buf_ and returns the resulting term.

<ul>
 <li>YAP_Term YAP_ReadBuffer(char \* _buf_,YAP_Term \* _error_) @anchor YAP_IntsToList

</li>
</ul>
The user-provided string must include a terminating null
character. Syntax errors will cause returning `FALSE` and binding
 _error_ to a Prolog term.

These C-interface functions are useful when converting chunks of data to Prolog:

<ul>
 <li>YAP_Term YAP_FloatsToList(double \* _buf_,size_t  _sz_)
</li>
 <li>YAP_Term YAP_IntsToList(YAP_Int \* _buf_,size_t  _sz_) @anchor YAP_ListToInts

</li>
</ul>
Notice that they are unsafe, and may call the garbage collector. They
return 0 on error.

These C-interface functions are useful when converting Prolog lists to arrays:

<ul>
 <li>YAP_Int YAP_IntsToList(YAP_Term t, YAP_Int \* _buf_,size_t  _sz_)
</li>
 <li>YAP_Int YAP_FloatsToList(YAP_Term t, double \* _buf_,size_t  _sz_) @anchor YAP_AllocSpaceFromYAP

</li>
</ul>
They return the number of integers scanned, up to a maximum of <tt>sz</tt>,
and <tt>-1</tt> on error.

@section Memory_Allocation Memory Allocation

The next routine can be used to ask space from the Prolog data-base:

<ul>
 <li>void      \*YAP_AllocSpaceFromYAP(int  _size_) @anchor YAP_FreeSpaceFromYAP

</li>
</ul>
The routine returns a pointer to a buffer allocated from the code area,
or `NULL` if sufficient space was not available.

The space allocated with [YAP_AllocSpaceFromYAP](@ref YAP_AllocSpaceFromYAP) can be released
back to YAP by using:

<ul>
 <li>void      YAP_FreeSpaceFromYAP(void \* _buf_) @anchor YAP_StreamToFileNo

</li>
</ul>
The routine releases a buffer allocated from the code area. The system
may crash if `buf` is not a valid pointer to a buffer in the code
area.

@section Controlling_Streams Controlling YAP Streams from `C`

The C-Interface also provides the C-application with a measure of
control over the YAP Input/Output system. The first routine allows one
to find a file number given a current stream:

<ul>
 <li>int      YAP_StreamToFileNo(YAP_Term  _stream_) @anchor YAP_CloseAllOpenStreams

</li>
</ul>
This function gives the file descriptor for a currently available
stream. Note that null streams and in memory streams do not have
corresponding open streams, so the routine will return a
negative. Moreover, YAP will not be aware of any direct operations on
this stream, so information on, say, current stream position, may become
stale.

A second routine that is sometimes useful is:

<ul>
 <li>void      YAP_CloseAllOpenStreams(void) @anchor YAP_FlushAllStreams

</li>
</ul>
This routine closes the YAP Input/Output system except for the first
three streams, that are always associated with the three standard Unix
streams. It is most useful if you are doing `fork()`.

Last, one may sometimes need to flush all streams:

<ul>
 <li>void      YAP_CloseAllOpenStreams(void) @anchor YAP_OpenStream

</li>
</ul>
It is also useful before you do a `fork()`, or otherwise you may
have trouble with unflushed output.

The next routine allows a currently open file to become a stream. The
routine receives as arguments a file descriptor, the true file name as a
string, an atom with the user name, and a set of flags:

<ul>
 <li>void      YAP_OpenStream(void \* _FD_, char \* _name_, YAP_Term  _t_, int  _flags_) @anchor YAP_Record

</li>
</ul>
The available flags are `YAP_INPUT_STREAM`,
`YAP_OUTPUT_STREAM`, `YAP_APPEND_STREAM`,
`YAP_PIPE_STREAM`, `YAP_TTY_STREAM`, `YAP_POPEN_STREAM`,
`YAP_BINARY_STREAM`, and `YAP_SEEKABLE_STREAM`. By default, the
stream is supposed to be at position 0. The argument  _name_ gives
the name by which YAP should know the new stream.

@section Utility_Functions Utility Functions in `C`

The C-Interface  provides the C-application with a a number of utility
functions that are useful.

The first provides a way to insert a term into the data-base

<ul>
 <li>void      \*YAP_Record(YAP_Term  _t_) @anchor YAP_Recorded

</li>
</ul>
This function returns a pointer to a copy of the term in the database
(or to <tt>NULL</tt> if the operation fails.

The next functions provides a way to recover the term from the data-base:

<ul>
 <li>YAP_Term      YAP_Recorded(void \* _handle_) @anchor YAP_Erase

</li>
</ul>
Notice that the semantics are the same as for [recorded/3](@ref recorded): this
function creates a new copy of the term in the stack, with fresh
variables. The function returns <tt>0L</tt> if it cannot create a new term.

Last, the next function allows one to recover space:

<ul>
 <li>int      YAP_Erase(void \* _handle_) @anchor YAP_ExactlyEqual

</li>
</ul>
Notice that any accesses using  _handle_ after this operation may
lead to a crash.

The following functions are often required to compare terms.

Succeed if two terms are actually the same term, as in
[==/2](@ref qQqQ):

<ul>
 <li>int      YAP_ExactlyEqual(YAP_Term t1, YAP_Term t2)
</li>
</ul>

The next function succeeds if two terms are variant terms, and returns
0 otherwise, as
[=@=/2](@ref qQaAqQ):

<ul>
 <li>int      YAP_Variant(YAP_Term t1, YAP_Term t2)
</li>
</ul>

The next functions deal with numbering variables in terms:

<ul>
 <li>int      YAP_NumberVars(YAP_Term t, YAP_Int first_number)
</li>
 <li>YAP_Term YAP_UnNumberVars(YAP_Term t)
</li>
 <li>int      YAP_IsNumberedVariable(YAP_Term t)
</li>
</ul>

The next one returns the length of a well-formed list  _t_, or
`-1` otherwise:

<ul>
 <li>Int      YAP_ListLength(YAP_Term t)
</li>
</ul>

Last, this function succeeds if two terms are unifiable:
[=@=/2](@ref qQaAqQ):

<ul>
 <li>int      YAP_Unifiable(YAP_Term t1, YAP_Term t2)
</li>
</ul>

The second function computes a hash function for a term, as in
`term_hash/4`.

<ul>
 <li>YAP_Int    YAP_TermHash(YAP_Term t, YAP_Int range, YAP_Int depth, int  ignore_variables)); @anchor YAP_RunGoal

</li>
</ul>
The first three arguments follow `term_has/4`. The last argument
indicates what to do if we find a variable: if `0` fail, otherwise
ignore the variable. 

@section Calling_YAP_From_C From `C` back to Prolog

There are several ways to call Prolog code from C-code. By default, the
`YAP_RunGoal()` should be used for this task. It assumes the engine
has been initialised before:

<ul>
 <li>YAP_Int YAP_RunGoal(YAP_Term Goal)
</li>
</ul>
Execute query  _Goal_ and return 1 if the query succeeds, and 0
otherwise. The predicate returns 0 if failure, otherwise it will return
an  _YAP_Term_. 

Quite often, one wants to run a query once. In this case you should use
 _Goal_:

<ul>
 <li>YAP_Int YAP_RunGoalOnce(YAP_Term Goal)
</li>
</ul>
The  `YAP_RunGoal()` function makes sure to recover stack space at
the end of execution.

Prolog terms are pointers: a problem users often find is that the term
 _Goal_ may actually <em>be moved around</em> during the execution of
`YAP_RunGoal()`, due to garbage collection or stack shifting. If
this is possible,  _Goal_ will become invalid after executing
`YAP_RunGoal()`. In this case, it is a good idea to save  _Goal_
<em>slots</em>, as shown next:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  long sl = YAP_InitSlot(scoreTerm);

  out = YAP_RunGoal(t);
  t = YAP_GetFromSlot(sl);
  YAP_RecoverSlots(1);
  if (out == 0) return FALSE;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@copydoc real

The following functions complement  _YAP_RunGoal_:

<ul>
 <li>`int` YAP_RestartGoal(`void`) @anchor YAP_RestartGoal

Look for the next solution to the current query by forcing YAP to
backtrack to the latest goal. Notice that slots allocated since the last
[YAP_RunGoal](@ref YAP_RunGoal) will become invalid.

@Item  `int` YAP_Reset(`void`)
Reset execution environment (similar to the [abort/0](@ref abort)
built-in). This is useful when you want to start a new query before
asking all solutions to the previous query.

</li>
 <li>`int` YAP_ShutdownGoal(`int backtrack`) @anchor YAP_ShutdownGoal

Clean up the current goal. If
`backtrack` is true, stack space will be recovered and bindings
will be undone. In both cases, any slots allocated since the goal was
created will become invalid.

</li>
 <li>`YAP_Bool` YAP_GoalHasException(`YAP_Term \*tp`) @anchor YAP_GoalHasException

Check if the last goal generated an exception, and if so copy it to the
space pointed to by  _tp_

</li>
 <li>`void` YAP_ClearExceptions(`void`) @anchor YAP_ClearExceptions

Reset any exceptions left over by the system.
</li>
</ul>

The  _YAP_RunGoal_ interface is designed to be very robust, but may
not be the most efficient when repeated calls to the same goal are made
and when there is no interest in processing exception. The
 _YAP_EnterGoal_ interface should have lower-overhead:

<ul>
 <li>`YAP_PredEntryPtr` YAP_FunctorToPred(`YAP_Functor`  _f_, @anchor YAP_FunctorToPred

Return the predicate whose main functor is  _f_.

</li>
 <li>`YAP_PredEntryPtr` YAP_AtomToPred(`YAP_Atom`  _at_ @anchor YAP_AtomToPred

Return the arity 0 predicate whose name is  _at_.

</li>
 <li>`YAP_PredEntryPtr` @anchor YAP_FunctorToPredInModule

YAP_FunctorToPredInModule(`YAP_Functor`  _f_, `YAP_Module`  _m_),
Return the predicate in module  _m_ whose main functor is  _f_.

</li>
 <li>`YAP_PredEntryPtr` YAP_AtomToPred(`YAP_Atom`  _at_, `YAP_Module`  _m_), @anchor YAP_AtomToPredInModule

Return the arity 0 predicate in module  _m_ whose name is  _at_.

</li>
 <li>`YAP_Bool` YAP_EnterGoal(`YAP_PredEntryPtr`  _pe_, @anchor YAP_EnterGoal

`YAP_Term \*`  _array_, `YAP_dogoalinfo \*`  _infop_)
Execute a  query for predicate  _pe_. The query is given as an
array of terms  _Array_.  _infop_ is the address of a goal
handle that can be used to backtrack and to recover space. Succeeds if
a solution was found.

Notice that you cannot create new slots if an YAP_EnterGoal goal is open.

</li>
 <li>`YAP_Bool` YAP_RetryGoal(`YAP_dogoalinfo \*`  _infop_) @anchor YAP_RetryGoal


Backtrack to a query created by [YAP_EnterGoal](@ref YAP_EnterGoal). The query is
given by the handle  _infop_. Returns whether a new solution could
be be found.

</li>
 <li>`YAP_Bool` YAP_LeaveGoal(`YAP_Bool`  _backtrack_, @anchor YAP_LeaveGoal

`YAP_dogoalinfo \*`  _infop_)
Exit a query query created by [YAP_EnterGoal](@ref YAP_EnterGoal). If
`backtrack` is `TRUE`, variable bindings are undone and Heap
space is recovered.  Otherwise, only stack space is recovered, ie,
`LeaveGoal` executes a cut.
</li>
</ul>
Next, follows an example of how to use [YAP_EnterGoal](@ref YAP_EnterGoal):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void
runall(YAP_Term g)
{
    YAP_dogoalinfo goalInfo;
    YAP_Term *goalArgs = YAP_ArraysOfTerm(g);
    YAP_Functor *goalFunctor = YAP_FunctorOfTerm(g);
    YAP_PredEntryPtr goalPred = YAP_FunctorToPred(goalFunctor);

    result = YAP_EnterGoal( goalPred, goalArgs, &goalInfo );
    while (result)
       result = YAP_RetryGoal( &goalInfo );
    YAP_LeaveGoal(TRUE, &goalInfo);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

YAP allows calling a  *new* Prolog interpreter from `C`. One
way is to first construct a goal `G`, and then it is sufficient to
perform:

<ul>
 <li>YAP_Bool      YAP_CallProlog(YAP_Term  _G_)
</li>
</ul>
the result will be `FALSE`, if the goal failed, or `TRUE`, if
the goal succeeded. In this case, the variables in  _G_ will store
the values they have been unified with. Execution only proceeds until
finding the first solution to the goal, but you can call
[findall/3](@ref findall) or friends if you need all the solutions.

Notice that during execution, garbage collection or stack shifting may
have moved the terms 

@section Module_Manipulation_in_C Module Manipulation in C

YAP allows one to create a new module from C-code. To create the new
code it is sufficient to call:

<ul>
 <li>YAP_Module      YAP_CreateModule(YAP_Atom  _ModuleName_)
</li>
</ul>
Notice that the new module does not have any predicates associated and
that it is not the current module. To find the current module, you can call:

<ul>
 <li>YAP_Module      YAP_CurrentModule()
</li>
</ul>

Given a module, you may want to obtain the corresponding name. This is
possible by using:

<ul>
 <li>YAP_Term      YAP_ModuleName(YAP_Module mod)
</li>
</ul>
Notice that this function returns a term, and not an atom. You can
[YAP_AtomOfTerm](@ref YAP_AtomOfTerm) to extract the corresponding Prolog atom.

@section Miscellaneous_ChYFunctions Miscellaneous C Functions

<ul>
 <li>`void` YAP_Throw(`YAP_Term exception`)
</li>
 <li>`void` YAP_AsyncThrow(`YAP_Term exception`) @anchor YAP_Throw


Throw an exception with term   _exception_, just like if you called
`throw/2`. The function <tt>YAP_AsyncThrow</tt> is supposed to be used
from interrupt handlers.


</li>
 <li>`int` YAP_SetYAPFlag(`yap_flag_t flag, int value`) @anchor YAP_SetYAPFlag


This function allows setting some YAP flags from `C` .Currently,
only two boolean flags are accepted: `YAPC_ENABLE_GC` and
`YAPC_ENABLE_AGC`.  The first enables/disables the standard garbage
collector, the second does the same for the atom garbage collector.`

</li>
 <li>`YAP_TERM` YAP_AllocExternalDataInStack(`size_t bytes`)
</li>
 <li>`void \*` YAP_ExternalDataInStackFromTerm(`YAP_Term t`)
</li>
 <li>`YAP_Bool` YAP_IsExternalDataInStackTerm(`YAP_Term t`) @anchor YAP_AllocExternalDataInStack


The next routines allow one to store external data in the Prolog
execution stack. The first routine reserves space for  _sz_ bytes
and returns an opaque handle. The second routines receives the handle
and returns a pointer to the data.  The last routine checks if a term
is an opaque handle.

Data will be automatically reclaimed during
backtracking. Also, this storage is opaque to the Prolog garbage compiler,
so it should not be used to store Prolog terms. On the other hand, it
may be useful to store arrays in a compact way, or pointers to external objects.

</li>
 <li>`int` YAP_HaltRegisterHook(`YAP_halt_hook f, void \*closure`) @anchor YAP_HaltRegisterHook


Register the function  _f_ to be called if YAP is halted. The
function is called with two arguments: the exit code of the process
(`0` if this cannot be determined on your operating system) and
the closure argument  _closure_.


</li>
 <li>`int` YAP_Argv(`char \*\*\*argvp`) @anchor YAP_Argv

Return the number of arguments to YAP and instantiate argvp to point to the list of such arguments.

</li>
</ul>

@section Writing_C Writing predicates in C

We will distinguish two kinds of predicates:

<ul>
 <li>\a deterministic predicates which either fail or succeed but are not
backtrackable, like the one in the introduction;
</li>
 <li>\a backtrackable
predicates which can succeed more than once.
</li>
</ul>

The first kind of predicates should be implemented as a C function with
no arguments which should return zero if the predicate fails and a
non-zero value otherwise. The predicate should be declared to
YAP, in the initialization routine, with a call to

<ul>
 <li>void YAP_UserCPredicate(char \* _name_, YAP_Bool \* _fn_(), unsigned long int  _arity_);
where  _name_ is a string with the name of the predicate,  _init_,
 _cont_,  _cut_ are the C functions used to start, continue and
when pruning the execution of the predicate,  _arity_ is the
predicate arity, and  _sizeof_ is the size of the data to be
preserved in the stack.

For the second kind of predicates we need three C functions. The first one
is called when the predicate is first activated; the second one
is called on backtracking to provide (possibly) other solutions; the
last one is called on pruning. Note
also that we normally also need to preserve some information to find out
the next solution.

In fact the role of the two functions can be better understood from the
following Prolog definition

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       p :- start.
       p :- repeat,
                continue.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where `start` and `continue` correspond to the two C functions
described above.

The interface works as follows:

<ul>
 <li>void YAP_UserBackCutCPredicate(char \* _name_, int \* _init_(), int \* _cont_(), int \* _cut_(), unsigned long int  _arity_, unsigned int  _sizeof_) @anchor YAP_UserBackCutCPredicate

describes a new predicate where  _name_ is the name of the predicate,
 _init_,  _cont_, and  _cut_ are the C functions that implement
the predicate and  _arity_ is the predicate's arity.

</li>
 <li>void YAP_UserBackCPredicate(char \* _name_, int \* _init_(), int \* _cont_(), unsigned long int  _arity_, unsigned int  _sizeof_) @anchor YAP_UserBackCPredicate

describes a new predicate where  _name_ is the name of the predicate,
 _init_, and  _cont_ are the C functions that implement the
predicate and  _arity_ is the predicate's arity.

</li>
 <li>void YAP_PRESERVE_DATA( _ptr_,  _type_); @anchor YAP_PRESERVE_DATA


</li>
 <li>void YAP_PRESERVED_DATA( _ptr_,  _type_); @anchor YAP_PRESERVED_DATA


</li>
 <li>void YAP_PRESERVED_DATA_CUT( _ptr_,  _type_); @anchor YAP_PRESERVED_DATA_CUT


</li>
 <li>void YAP_cut_succeed( void ); @anchor YAP_cut_succeed


</li>
 <li>void YAP_cut_fail( void ); @anchor YAP_cut_fail


</li>
</ul>

As an example we will consider implementing in C a predicate `n100(N)`
which, when called with an instantiated argument should succeed if that
argument is a numeral less or equal to 100, and, when called with an
uninstantiated argument, should provide, by backtracking, all the positive
integers less or equal to 100.

To do that we first declare a structure, which can only consist
of Prolog terms, containing the information to be preserved on backtracking
and a pointer variable to a structure of that type.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#include "YAPInterface.h"

static int start_n100(void);
static int continue_n100(void);

typedef struct {
    YAP_Term next_solution; 
   } n100_data_type;

n100_data_type *n100_data;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We now write the `C` function to handle the first call:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static int start_n100(void)
{
      YAP_Term t = YAP_ARG1;
      YAP_PRESERVE_DATA(n100_data,n100_data_type);
      if(YAP_IsVarTerm(t)) {
          n100_data->next_solution = YAP_MkIntTerm(0);
          return continue_n100();
       }
      if(!YAP_IsIntTerm(t) || YAP_IntOfTerm(t)<0 || YAP_IntOfTerm(t)>100) {
          YAP_cut_fail();
      } else {
          YAP_cut_succeed();
      }
}

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The routine starts by getting the dereference value of the argument.
The call to [YAP_PRESERVE_DATA](@ref YAP_PRESERVE_DATA) is used to initialize the memory
which will hold the information to be preserved across
backtracking. The first argument is the variable we shall use, and the
second its type. Note that we can only use [YAP_PRESERVE_DATA](@ref YAP_PRESERVE_DATA)
once, so often we will want the variable to be a structure. This data
is visible to the garbage collector, so it should consist of Prolog
terms, as in the example. It is also correct to store pointers to
objects external to YAP stacks, as the garbage collector will ignore
such references.

If the argument of the predicate is a variable, the routine initializes the 
structure to be preserved across backtracking with the information
required to provide the next solution, and exits by calling
`continue_n100` to provide that solution.

If the argument was not a variable, the routine then checks if it was an
integer, and if so, if its value is positive and less than 100. In that
case it exits, denoting success, with [YAP_cut_succeed](@ref YAP_cut_succeed), or
otherwise exits with [YAP_cut_fail](@ref YAP_cut_fail) denoting failure.

The reason for using for using the functions [YAP_cut_succeed](@ref YAP_cut_succeed) and
[YAP_cut_fail](@ref YAP_cut_fail) instead of just returning a non-zero value in the
first case, and zero in the second case, is that otherwise, if
backtracking occurred later, the routine `continue_n100` would be
called to provide additional solutions.

The code required for the second function is

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static int continue_n100(void)
{
      int n;
      YAP_Term t;
      YAP_Term sol = YAP_ARG1;
      YAP_PRESERVED_DATA(n100_data,n100_data_type);
      n = YAP_IntOfTerm(n100_data->next_solution);
      if( n == 100) {
           t = YAP_MkIntTerm(n);
           YAP_Unify(sol,t);
           YAP_cut_succeed();
        }
       else {
           YAP_Unify(sol,n100_data->next_solution);
           n100_data->next_solution = YAP_MkIntTerm(n+1);
           return(TRUE);
        }
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that again the macro [YAP_PRESERVED_DATA](@ref YAP_PRESERVED_DATA) is used at the
beginning of the function to access the data preserved from the previous
solution.  Then it checks if the last solution was found and in that
case exits with [YAP_cut_succeed](@ref YAP_cut_succeed) in order to cut any further
backtracking.  If this is not the last solution then we save the value
for the next solution in the data structure and exit normally with 1
denoting success. Note also that in any of the two cases we use the
function `YAP_unify` to bind the argument of the call to the value
saved in ` n100_state-\>next_solution`.

Note also that the only correct way to signal failure in a backtrackable
predicate is to use the [YAP_cut_fail](@ref YAP_cut_fail) macro.

Backtrackable predicates should be declared to YAP, in a way
similar to what happened with deterministic ones, but using instead a
call to

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In this example, we would have something like

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void
init_n100(void)
{
  YAP_UserBackCutCPredicate("n100", start_n100, continue_n100, cut_n100, 1, 1);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The argument before last is the predicate's arity. Notice again the
last argument to the call. function argument gives the extra space we
want to use for `PRESERVED_DATA`. Space is given in cells, where
a cell is the same size as a pointer. The garbage collector has access
to this space, hence users should use it either to store terms or to
store pointers to objects outside the stacks.

The code for `cut_n100` could be:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static int cut_n100(void)
{
  YAP_PRESERVED_DATA_CUT(n100_data,n100_data_type*);

  fprintf("n100 cut with counter %ld\n", YAP_IntOfTerm(n100_data->next_solution));
  return TRUE;
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that we have to use [YAP_PRESERVED_DATA_CUT](@ref YAP_PRESERVED_DATA_CUT): this is
because the Prolog engine is at a different state during cut.

If no work is required at cut, we can use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void
init_n100(void)
{
  YAP_UserBackCutCPredicate("n100", start_n100, continue_n100, NULL, 1, 1);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
in this case no code is executed at cut time.

@section Loading_Objects Loading Object Files

The primitive predicate

<ul>
 <li>load_foreign_files( _Files_, _Libs_, _InitRoutine_)
</li>
</ul>
should be used, from inside YAP, to load object files produced by the C
compiler. The argument  _ObjectFiles_ should be a list of atoms
specifying the object files to load,  _Libs_ is a list (possibly
empty) of libraries to be passed to the unix loader (`ld`) and
InitRoutine is the name of the C routine (to be called after the files
are loaded) to perform the necessary declarations to YAP of the
predicates defined in the files. 

YAP will search for  _ObjectFiles_ in the current directory first. If
it cannot find them it will search for the files using the environment
variable:

<ul>
 <li>YAPLIBDIR
</li>
</ul>
if defined, or in the default library.

YAP also supports the SWI-Prolog interface to loading foreign code:

<ul>
 <li>open_shared_object(+ _File_, - _Handle_)

File is the name of a shared object file (called dynamic load
library in MS-Windows). This file is attached to the current process
and  _Handle_ is unified with a handle to the library. Equivalent to
`open_shared_object(File, [], Handle)`. See also
[load_foreign_library/1](@ref load_foreign_library) and `load_foreign_library/2`.

On errors, an exception `shared_object`( _Action_,
 _Message_) is raised.  _Message_ is the return value from
dlerror().

</li>
 <li>open_shared_object(+ _File_, - _Handle_, + _Options_)

As `open_shared_object/2`, but allows for additional flags to
be passed.  _Options_ is a list of atoms. `now` implies the
symbols are 
resolved immediately rather than lazily (default). `global` implies
symbols of the loaded object are visible while loading other shared
objects (by default they are local). Note that these flags may not
be supported by your operating system. Check the documentation of
`dlopen()` or equivalent on your operating system. Unsupported
flags  are silently ignored. 

</li>
 <li>close_shared_object(+ _Handle_) @anchor close_shared_object


Detach the shared object identified by  _Handle_. 

</li>
 <li>call_shared_object_function(+ _Handle_, + _Function_) @anchor call_shared_object_function


Call the named function in the loaded shared library. The function
is called without arguments and the return-value is
ignored. In SWI-Prolog, normally this function installs foreign
language predicates using calls to `PL_register_foreign()`.
</li>
</ul>

@section SavebQeERest Saving and Restoring

YAP4 currently does not support `save` and `restore` for object code
loaded with `load_foreign_files/3`. We plan to support save and restore
in future releases of YAP.

@section YAP4_Notes Changes to the C-Interface in YAP4

YAP4 includes several changes over the previous `load_foreign_files/3`
interface. These changes were required to support the new binary code
formats, such as ELF used in Solaris2 and Linux.

<ul>
 <li>All Names of YAP objects now start with  _YAP__. This is
designed to avoid clashes with other code. Use `YAPInterface.h` to
take advantage of the new interface. `c_interface.h` is still
available if you cannot port the code to the new interface.

</li>
 <li>Access to elements in the new interface always goes through
<em>functions</em>. This includes access to the argument registers,
`YAP_ARG1` to `YAP_ARG16`. This change breaks code such as
`unify(\&ARG1,\&t)`, which is nowadays:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
   YAP_Unify(ARG1, t);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>`cut_fail()` and `cut_succeed()` are now functions.

</li>
 <li>The use of `Deref` is deprecated. All functions that return
Prolog terms, including the ones that access arguments, already
dereference their arguments.

</li>
 <li>Space allocated with PRESERVE_DATA is ignored by garbage
collection and stack shifting. As a result, any pointers to a Prolog
stack object, including some terms, may be corrupted after garbage
collection or stack shifting. Prolog terms should instead be stored as
arguments to the backtrackable procedure.

</li>
</ul>

@section YAPLibrary Using YAP as a Library

YAP can be used as a library to be called from other
programs. To do so, you must first create the YAP library:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make library
make install_library
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This will install a file `libyap.a` in  _LIBDIR_ and the Prolog
headers in  _INCLUDEDIR_. The library contains all the functionality
available in YAP, except the foreign function loader and for
`YAP`'s startup routines.

To actually use this library you must follow a five step process:

<ol>
 <li>
You must initialize the YAP environment. A single function,
`YAP_FastInit` asks for a contiguous chunk in your memory space, fills
it in with the data-base, and sets up YAP's stacks and
execution registers. You can use a saved space from a standard system by
calling [save_program/1](@ref save_program).

</li>
 <li>You then have to prepare a query to give to
YAP. A query is a Prolog term, and you just have to use the same
functions that are available in the C-interface.

</li>
 <li>You can then use `YAP_RunGoal(query)` to actually evaluate your
query. The argument is the query term `query`, and the result is 1
if the query succeeded, and 0 if it failed.

</li>
 <li>You can use the term destructor functions to check how
arguments were instantiated.

</li>
 <li>If you want extra solutions, you can use
`YAP_RestartGoal()` to obtain the next solution.

</li>
</ol>

The next program shows how to use this system. We assume the saved
program contains two facts for the procedure <tt>b</tt>:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#include <stdio.h>
#include "YAP/YAPInterface.h"

int
main(int argc, char *argv[]) {
  if (YAP_FastInit("saved_state") == YAP_BOOT_ERROR)
    exit(1);
  if (YAP_RunGoal(YAP_MkAtomTerm(YAP_LookupAtom("do")))) {
    printf("Success\n");
    while (YAP_RestartGoal())
      printf("Success\n");
  }
  printf("NO\n");
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The program first initializes YAP, calls the query for the
first time and succeeds, and then backtracks twice. The first time
backtracking succeeds, the second it fails and exits.

To compile this program it should be sufficient to do:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cc -o exem -I../YAP4.3.0 test.c -lYAP -lreadline -lm
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You may need to adjust the libraries and library paths depending on the
Operating System and your installation of YAP.

Note that YAP4.3.0 provides the first version of the interface. The
interface may change and improve in the future.

The following C-functions are available from YAP:

<ul>
 <li>YAP_CompileClause(`YAP_Term`  _Clause_)
Compile the Prolog term  _Clause_ and assert it as the last clause
for the corresponding procedure.

</li>
 <li>`int` YAP_ContinueGoal(`void`)
Continue execution from the point where it stopped.

</li>
 <li>`void` YAP_Error(`int`  _ID_,`YAP_Term`  _Cause_,`char \*`  _error_description_)
Generate an YAP System Error with description given by the string
 _error_description_.  _ID_ is the error ID, if known, or
`0`.  _Cause_ is the term that caused the crash.

</li>
 <li>`void` YAP_Exit(`int`  _exit_code_)
Exit YAP immediately. The argument  _exit_code_ gives the error code
and is supposed to be 0 after successful execution in Unix and Unix-like
systems.

</li>
 <li>`YAP_Term` YAP_GetValue(`Atom`  _at_)
Return the term  _value_ associated with the atom  _at_. If no
such term exists the function will return the empty list.

</li>
 <li>YAP_FastInit(`char \*`  _SavedState_)
Initialize a copy of YAP from  _SavedState_. The copy is
monolithic and currently must be loaded at the same address where it was
saved. `YAP_FastInit` is a simpler version of `YAP_Init`.

</li>
 <li>YAP_Init( _InitInfo_)
Initialize YAP. The arguments are in a `C`
structure of type `YAP_init_args`.

The fields of  _InitInfo_ are `char \*`  _SavedState_,
`int`  _HeapSize_, `int`  _StackSize_, `int`
 _TrailSize_, `int`  _NumberofWorkers_, `int`
 _SchedulerLoop_, `int`  _DelayedReleaseLoad_, `int`
 _argc_, `char \*\*`  _argv_, `int`  _ErrorNo_, and
`char \*`  _ErrorCause_. The function returns an integer, which
indicates the current status. If the result is `YAP_BOOT_ERROR`
booting failed.

If  _SavedState_ is not NULL, try to open and restore the file
 _SavedState_. Initially YAP will search in the current directory. If
the saved state does not exist in the current directory YAP will use
either the default library directory or the directory given by the
environment variable [YAPLIBDIR](@ref YAPLIBDIR). Note that currently
the saved state must be loaded at the same address where it was saved.

If  _HeapSize_ is different from 0 use  _HeapSize_ as the minimum
size of the Heap (or code space). If  _StackSize_ is different from 0
use  _HeapSize_ as the minimum size for the Stacks. If
 _TrailSize_ is different from 0 use  _TrailSize_ as the minimum
size for the Trails.

The  _NumberofWorkers_,  _NumberofWorkers_, and
 _DelayedReleaseLoad_ are only of interest to the or-parallel system.

The argument count  _argc_ and string of arguments  _argv_
arguments are to be passed to user programs as the arguments used to
call YAP.

If booting failed you may consult `ErrorNo` and `ErrorCause`
for the cause of the error, or call
`YAP_Error(ErrorNo,0L,ErrorCause)` to do default processing. 

</li>
 <li>`void` YAP_PutValue(`Atom`  _at_, `YAP_Term`  _value_)
Associate the term  _value_ with the atom  _at_. The term
 _value_ must be a constant. This functionality is used by YAP as a
simple way for controlling and communicating with the Prolog run-time.

</li>
 <li>`YAP_Term` YAP_Read(`IOSTREAM \*Stream`)
Parse a  _Term_ from the stream  _Stream_.

</li>
 <li>`YAP_Term` YAP_Write(`YAP_Term`  _t_)
Copy a Term  _t_ and all associated constraints. May call the garbage
collector and returns `0L` on error (such as no space being
available).

</li>
 <li>`void` YAP_Write(`YAP_Term`  _t_, `IOSTREAM`  _stream_, `int`  _flags_)
Write a Term  _t_ using the stream  _stream_ to output
characters. The term is written according to a mask of the following
flags in the `flag` argument: `YAP_WRITE_QUOTED`,
`YAP_WRITE_HANDLE_VARS`, `YAP_WRITE_USE_PORTRAY`,  and `YAP_WRITE_IGNORE_OPS`.

</li>
 <li>`int` YAP_WriteBuffer(`YAP_Term`  _t_, `char \*`  _buff_, `size_t`  _size_, `int`  _flags_)
Write a YAP_Term  _t_ to buffer  _buff_ with size
 _size_. The term is written
according to a mask of the following flags in the `flag`
argument: `YAP_WRITE_QUOTED`, `YAP_WRITE_HANDLE_VARS`,
`YAP_WRITE_USE_PORTRAY`, and `YAP_WRITE_IGNORE_OPS`. The
function will fail if it does not have enough space in the buffer.

</li>
 <li>`char \*` YAP_WriteDynamicBuffer(`YAP_Term`  _t_, `char \*`  _buff_, `size_t`  _size_, `size_t`  _\*lengthp_, `size_t`  _\*encodingp_, `int`  _flags_)
Write a YAP_Term  _t_ to buffer  _buff_ with size
 _size_. The code will allocate an extra buffer if  _buff_ is
`NULL` or if `buffer` does not have enough room. The
variable `lengthp` is assigned the size of the resulting buffer,
and `encodingp` will receive the type of encoding (currently only `PL_ENC_ISO_LATIN_1` and `PL_ENC_WCHAR` are supported)

</li>
 <li>`void` YAP_InitConsult(`int`  _mode_, `char \*`  _filename_)
Enter consult mode on file  _filename_. This mode maintains a few
data-structures internally, for instance to know whether a predicate
before or not. It is still possible to execute goals in consult mode.

If  _mode_ is `TRUE` the file will be reconsulted, otherwise
just consulted. In practice, this function is most useful for
bootstrapping Prolog, as otherwise one may call the Prolog predicate
[compile/1](@ref compile) or [consult/1](@ref consult) to do compilation.

Note that it is up to the user to open the file  _filename_. The
`YAP_InitConsult` function only uses the file name for internal
bookkeeping.

</li>
 <li>`void` YAP_EndConsult(`void`)
Finish consult mode.

</li>
</ul>

Some observations:

<ul>
 <li>The system will core dump if you try to load the saved state in a
different address from where it was made. This may be a problem if
your program uses `mmap`. This problem will be addressed in future
versions of YAP.

</li>
 <li>Currently, the YAP library will pollute the name
space for your program.

</li>
 <li>The initial library includes the complete YAP system. In
the future we plan to split this library into several smaller libraries
(e.g. if you do not want to perform Input/Output).

</li>
 <li>You can generate your own saved states. Look at  the
`boot.yap` and `init.yap` files.

</li>
</ul>

@page Compatibility Compatibility with Other Prolog systems

YAP has been designed to be as compatible as possible with
other Prolog systems, and initially with C-Prolog. More recent work on
YAP has included features initially proposed for the Quintus
and SICStus Prolog systems.

Developments since `YAP4.1.6` we have striven at making
YAP compatible with the ISO-Prolog standard. 

@section ChYProlog Compatibility with the C-Prolog interpreter

@subsection Major_Differences_with_ChYProlog Major Differences between YAP and C-Prolog.

YAP includes several extensions over the original C-Prolog system. Even
so, most C-Prolog programs should run under YAP without changes.

The most important difference between YAP and C-Prolog is that, being
YAP a compiler, some changes should be made if predicates such as
`assert`, `clause` and `retract` are used. First
predicates which will change during execution should be declared as
`dynamic` by using commands like:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- dynamic f/n.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 where `f` is the predicate name and n is the arity of the
predicate. Note that  several such predicates can be declared in a
single command:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 :- dynamic f/2, ..., g/1.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Primitive predicates such as `retract` apply only to dynamic
predicates.  Finally note that not all the C-Prolog primitive predicates
are implemented in YAP. They can easily be detected using the
`unknown` system predicate provided by YAP.

Last, by default YAP enables character escapes in strings. You can
disable the special interpretation for the escape character by using:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- yap_flag(character_escapes,off).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
or by using:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- yap_flag(language,cprolog).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection Fully_ChYProlog_Compatible YAP predicates fully compatible with C-Prolog

These are the Prolog built-ins that are fully compatible in both
C-Prolog and YAP:

@subsection Not_Strictly_ChYProlog_Compatible YAP predicates not strictly compatible with C-Prolog

These are YAP built-ins that are also available in C-Prolog, but
that are not fully compatible:

@subsection Not_in_ChYProlog YAP predicates not available in C-Prolog

These are YAP built-ins not available in C-Prolog.

@subsection Not_in_YAP YAP predicates not available in C-Prolog

These are C-Prolog built-ins not available in YAP:

<ul>
 <li>'LC'
The following Prolog text uses lower case letters.

</li>
 <li>'NOLC'
The following Prolog text uses upper case letters only.
</li>
</ul>

@section SICStus_Prolog Compatibility with the Quintus and SICStus Prolog systems

The Quintus Prolog system was the first Prolog compiler to use Warren's
Abstract Machine. This system was very influential in the Prolog
community. Quintus Prolog implemented compilation into an abstract
machine code, which was then emulated. Quintus Prolog also included
several new built-ins, an extensive library, and in later releases a
garbage collector. The SICStus Prolog system, developed at SICS (Swedish
Institute of Computer Science), is an emulator based Prolog system
largely compatible with Quintus Prolog. SICStus Prolog has evolved
through several versions. The current version includes several
extensions, such as an object implementation, co-routining, and
constraints.

Recent work in YAP has been influenced by work in Quintus and
SICStus Prolog. Wherever possible, we have tried to make YAP
compatible with recent versions of these systems, and specifically of
SICStus Prolog. You should use 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- yap_flag(language, sicstus).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for maximum compatibility with SICStus Prolog.

@subsection Major_Differences_with_SICStus Major Differences between YAP and SICStus Prolog.

Both YAP and SICStus Prolog obey the Edinburgh Syntax and are based on
the WAM. Even so, there are quite a few important differences:

<ul>
 <li>Differently from SICStus Prolog, YAP does not have a
notion of interpreted code. All code in YAP is compiled.

</li>
 <li>YAP does not support an intermediate byte-code
representation, so the `fcompile/1` and `load/1` built-ins are
not available in YAP.

</li>
 <li>YAP implements escape sequences as in the ISO standard. SICStus
Prolog implements Unix-like escape sequences.

</li>
 <li>YAP implements [initialization/1](@ref initialization) as per the ISO
standard. Use [prolog_initialization/1](@ref prolog_initialization) for the SICStus Prolog
compatible built-in.

</li>
 <li>Prolog flags are different in SICStus Prolog and in YAP.

</li>
 <li>The SICStus Prolog `on_exception/3` and
`raise_exception` built-ins correspond to the ISO built-ins
[catch/3](@ref catch) and [throw/1](@ref throw).

</li>
 <li>The following SICStus Prolog v3 built-ins are not (currently)
implemented in YAP (note that this is only a partial list):
[file_search_path/2](@ref file_search_path),
`stream_interrupt/3`, `reinitialize/0`, `help/0`,
`help/1`, `trimcore/0`, `load_files/1`,
[load_files/2](@ref load_files), and `require/1`.

The previous list is incomplete. We also cannot guarantee full
compatibility for other built-ins (although we will try to address any
such incompatibilities). Last, SICStus Prolog is an evolving system, so
one can be expect new incompatibilities to be introduced in future
releases of SICStus Prolog.

</li>
 <li>YAP allows asserting and abolishing static code during
execution through the [assert_static/1](@ref assert_static) and [abolish/1](@ref abolish)
built-ins. This is not allowed in Quintus Prolog or SICStus Prolog.

</li>
 <li>The socket predicates, although designed to be compatible with
SICStus Prolog, are built-ins, not library predicates, in YAP.

</li>
 <li>This list is incomplete.

</li>
</ul>

The following differences only exist if the [language](@ref language) flag is set
to `yap` (the default):

<ul>
 <li>The [consult/1](@ref consult) predicate in YAP follows C-Prolog
semantics. That is, it adds clauses to the data base, even for
preexisting procedures. This is different from [consult/1](@ref consult) in
SICStus Prolog or SWI-Prolog.

</li>
 <li>
By default, the data-base in YAP follows "logical update semantics", as
Quintus Prolog or SICStus Prolog do.  Previous versions followed
"immediate update semantics". The difference is depicted in the next
example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- dynamic a/1.

?- assert(a(1)).

?- retract(a(X)), X1 is X +1, assertz(a(X)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With immediate semantics, new clauses or entries to the data base are
visible in backtracking. In this example, the first call to
[retract/1](@ref retract) will succeed. The call to  *assertz/1* will then
succeed. On backtracking, the system will retry
[retract/1](@ref retract). Because the newly asserted goal is visible to
[retract/1](@ref retract), it can be retracted from the data base, and
`retract(a(X))` will succeed again. The process will continue
generating integers for ever. Immediate semantics were used in C-Prolog.

With logical update semantics, any additions or deletions of clauses
for a goal 
<em>will not affect previous activations of the goal</em>. In the example,
the call to [assertz/1](@ref assertz) will not see the 
update performed by the [assertz/1](@ref assertz), and the query will have a
single solution.

Calling `yap_flag(update_semantics,logical)` will switch
YAP to use logical update semantics.

</li>
 <li>[dynamic/1](@ref dynamic) is a built-in, not a directive, in YAP.

</li>
 <li>By default, YAP fails on undefined predicates. To follow default
SICStus Prolog use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- yap_flag(unknown,error).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

</li>
 <li>By default, directives in YAP can be called from the top level.

</li>
</ul>

@subsection Fully_SICStus_Compatible YAP predicates fully compatible with SICStus Prolog

These are the Prolog built-ins that are fully compatible in both SICStus
Prolog and YAP:

@subsection Not_Strictly_SICStus_Compatible YAP predicates not strictly compatible with SICStus Prolog

These are YAP built-ins that are also available in SICStus Prolog, but
that are not fully compatible:

@subsection Not_in_SICStus_Prolog YAP predicates not available in SICStus Prolog

These are YAP built-ins not available in SICStus Prolog.

@section ISO_Prolog Compatibility with the ISO Prolog standard

The Prolog standard was developed by ISO/IEC JTC1/SC22/WG17, the
international standardization working group for the programming language
Prolog. The book "Prolog: The Standard" by Deransart, Ed-Dbali and
Cervoni gives a complete description of this standard. Development in
YAP from YAP4.1.6 onwards have striven at making YAP
compatible with ISO Prolog. As such:

<ul>
 <li>YAP now supports all of the built-ins required by the
ISO-standard, and,
</li>
 <li>Error-handling is as required by the standard.
</li>
</ul>

YAP by default is not fully ISO standard compliant. You can set the 
[language](@ref language) flag to `iso` to obtain very good
compatibility. Setting this flag changes the following:

<ul>
 <li>By default, YAP uses "immediate update semantics" for its
database, and not "logical update semantics", as per the standard,
( (see [SICStus Prolog](@ref SICStus_Prolog))). This affects [assert/1](@ref assert),
[retract/1](@ref retract), and friends.

Calling `set_prolog_flag(update_semantics,logical)` will switch
YAP to use logical update semantics.

</li>
 <li>By default, YAP implements the 
[atom_chars/2](@ref atom_chars)( (see [Testing Terms](@ref Testing_Terms))), and 
[number_chars/2](@ref number_chars), ( (see [Testing Terms](@ref Testing_Terms))), 
built-ins as per the original Quintus Prolog definition, and
not as per the ISO definition.

Calling `set_prolog_flag(to_chars_mode,iso)` will switch
YAP to use the ISO definition for
[atom_chars/2](@ref atom_chars) and [number_chars/2](@ref number_chars).

</li>
 <li>By default, YAP allows executable goals in directives. In ISO mode
most directives can only be called from top level (the exceptions are
[set_prolog_flag/2](@ref set_prolog_flag) and [op/3](@ref op)).

</li>
 <li>Error checking for meta-calls under ISO Prolog mode is stricter
than by default.

</li>
 <li>The [strict_iso](@ref strict_iso) flag automatically enables the ISO Prolog
standard. This feature should disable all features not present in the
standard.

</li>
</ul>

The following incompatibilities between YAP and the ISO standard are
known to still exist:

<ul>

 <li>Currently, YAP does not handle overflow errors in integer
operations, and handles floating-point errors only in some
architectures. Otherwise, YAP follows IEEE arithmetic.

</li>
</ul>

Please inform the authors on other incompatibilities that may still
exist.

@section Operators Summary of YAP Predefined Operators

The Prolog syntax caters for operators of three main kinds:

<ul>
 <li>
prefix;
</li>
 <li>
infix;
</li>
 <li>
postfix.
</li>
</ul>

Each operator has precedence in the range 1 to 1200, and this 
precedence is used to disambiguate expressions where the structure of the 
term denoted is not made explicit using brackets. The operator of higher 
precedence is the main functor.

If there are two operators with the highest precedence, the ambiguity 
is solved analyzing the types of the operators. The possible infix types are: 
 _xfx_,  _xfy_, and  _yfx_.

With an operator of type  _xfx_ both sub-expressions must have lower 
precedence than the operator itself, unless they are bracketed (which 
assigns to them zero precedence). With an operator type  _xfy_ only the  
left-hand sub-expression must have lower precedence. The opposite happens 
for  _yfx_ type.

A prefix operator can be of type  _fx_ or  _fy_. 
A postfix operator can be of type  _xf_ or  _yf_. 
The meaning of the notation is analogous to the above.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a + b * c
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
means

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a + (b * c)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as + and \* have the following types and precedences:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:-op(500,yfx,'+').
:-op(400,yfx,'*').
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now defining

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:-op(700,xfy,'++').
:-op(700,xfx,'=:=').
a ++ b =:= c
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 means

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a ++ (b =:= c)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following is the list of the declarations of the predefined operators:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:-op(1200,fx,['?-', ':-']).
:-op(1200,xfx,[':-','-->']).
:-op(1150,fx,[block,dynamic,mode,public,multifile,meta_predicate,
              sequential,table,initialization]).
:-op(1100,xfy,[';','|']).
:-op(1050,xfy,->).
:-op(1000,xfy,',').
:-op(999,xfy,'.').
:-op(900,fy,['\+', not]).
:-op(900,fx,[nospy, spy]).
:-op(700,xfx,[@>=,@=<,@<,@>,<,=,>,=:=,=\=,\==,>=,=<,==,\=,=..,is]).
:-op(500,yfx,['\/','/\','+','-']).
:-op(500,fx,['+','-']).
:-op(400,yfx,['<<','>>','//','*','/']).
:-op(300,xfx,mod).
:-op(200,xfy,['^','**']).
:-op(50,xfx,same).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Predicate Index ##


## Concept Index ##


