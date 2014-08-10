	
This file documents the YAP Prolog System version 6.3.4, a high-performance Prolog compiler developed at LIACC, Universidade do Porto. YAP is based on David H. D. Warren's WAM (Warren Abstract Machine), with several optimizations for better performance. YAP follows the Edinburgh tradition, and is largely compatible with DEC-10 Prolog, Quintus Prolog, and especially with C-Prolog.

  + @subpage Download where to download YAP for your platform.
  
  + @subpage Install discusses how to compile and install YAP.

  + @subpage Syntax describes the syntax of YAP.

  + @subpage Run describes how to invoke YAP

  + @subpage YAPProgramming presents the main predicates and
    directives available to load files and to control the Prolog environment.
        + @ref YAPModules introduces the YAP module system and meta-predicates.

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

    + Speed: YAP is widely considered one of the fastest available
Prolog systems.

    + Functionality: it supports stream Input/Output, sockets, modules,
exceptions, Prolog debugger, C-interface, dynamic code, internal
database, DCGs, saved states, co-routining, arrays, threads.

    + We explicitly allow both commercial and non-commercial use of YAP.


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

    + The CHR package developed by Tom Schrijvers,
Christian Holzbaur, and Jan Wielemaker.

  + The CLP(BN) package and Horus toolkit developed by Tiago Gomes, and Vítor Santos Costa.

    + The CLP(R) package developed by Leslie De Koninck, Bart Demoen, Tom
Schrijvers, and Jan Wielemaker, based on the CLP(Q,R) implementation
by Christian Holzbaur.

  + The CPLint package developed by Fabrizio Riguzzi's research
laboratory at the University of Ferrara. Please see

<http://www.ing.unife.it/Docenti/FabrizioRiguzzi/>


  + The CUDA interface package developed by Carlos Martínez, Jorge
Buenabad, Inês Dutra and Vítor Santos Costa.

  + The GECODE interface package developed by  Denys Duchier and Vítor Santos Costa.

  + The JPL (Java-Prolog Library) package developed by .

  + The Logtalk Object-Oriented system is developed at the University 
	of Beira Interior, Portugal, by Paulo Moura:

	<http://logtalk.org/>

	Logtalk is no longer distributed with YAP. Please use the Logtalk standalone 
	installer for a smooth integration with YAP.

	    + The minisat SAT solver interface developed by Michael Codish,
 Vitaly Lagoon, and Peter J. Stuckey.

	    + The MYDDAS relational data-base interface developed at the
 Universidade do Porto by Tiago Soares, Michel Ferreira, and Ricardo Rocha.

	    + The PRISM logic-based
programming system for statistical modeling developed at the Sato
Research Laboratory, TITECH, Japan.

	    + The ProbLog 1 system developed by the ProbLog team in the
DTAI group of KULeuven. For general information on ProbLog 1 and 2, please see
    
    <http://dtai.cs.kuleuven.be/problog>

	    + The real R interface package developed by 	Nicos Angelopoulos,
Vítor Santos Costa, João Azevedo, Jan Wielemaker, and Rui Camacho.
</li>

	    + YAP includes the yap2swi library that ports to YAP code from
 of SWI's PL interface. This includes the Input/Output Layer, the SWI
 Foreign Language Interface, and the RDF, archive, clib, http, odbc, plunit,
 semweb, sgml, and zlib packages written by Jan Wielemaker. Please do refer to the SWI-Prolog home page:

<http://www.swi-prolog.org>

for more information on SWI-Prolog and the SWI packages. 


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

     1 `autoconf`. Recent versions of YAP try to follow GNU
      conventions on where to place software.

         + The main executable is placed at  _$BINDIR_. This executa§ble is
            actually a script that calls the Prolog engine, stored at  _$LIBDIR_.

         + _$LIBDIR_ is the directory where libraries are stored. YAPLIBDIR is a
            subdirectory that contains the Prolog engine and a Prolog library.

         + _$INCLUDEDIR_ is used if you want to use YAP as a library.

         + _$INFODIR_ is where to store `info` files. Usually /usr/local/info, /usr/info, or /usr/share/info.

    2 `make`.

    3 If the compilation succeeds, try `./yap`.

    4  If you feel satisfied with the result, do `make install`.

    5 In most systems you will need to be superuser in order to do
     `make install` and `make info` on the standard directories.

@section Configuration_Options Tuning the Functionality of YAP 

Compiling YAP with the standard options give you a plain vanilla
Prolog. You can tune YAP to include extra functionality by calling
`configure` with the appropriate options:

    + `--enable-rational-trees=yes` gives you support for infinite
     rational trees.

    + `--enable-coroutining=yes` gives you support for coroutining,
    including freezing of goals, attributed variables, and
constraints. This will also enable support for infinite rational
trees.

    + `--enable-depth-limit=yes` allows depth limited evaluation, say for
implementing iterative deepening.

    + `--enable-low-level-tracer=yes` allows support for tracing all calls,
retries, and backtracks in the system. This can help in debugging your
application, but results in performance loss.

    + `--enable-wam-profile=yes` allows profiling of abstract machine
instructions. This is useful when developing YAP, should not be so
useful for normal users.

    + `--enable-condor=yes` allows using the Condor system that
support High Throughput Computing (HTC) on large collections of
distributively owned computing resources.

    + `--enable-tabling=yes` allows tabling support. This option
is still experimental.

    + `--enable-parallelism={env-copy,sba,a-cow}` allows
or-parallelism supported by one of these three forms. This option is
still highly experimental.

    + `--with-max-workers` allows definition of the maximum 
number of parallel processes (its value can be consulted at runtime 
using the flag `max_workers`).

    + `--with-gmp[=DIR]` give a path to where one can find the
`GMP` library if not installed in the default path.

    + `--enable-threads` allows using of the multi-threading 
predicates provided by YAP. Depending on the operating system, the 
option `--enable-pthread-locking` may also need to be used.

    + `--with-max-threads` allows definition of the maximum 
number of threads (the default value is 1024; its value can be consulted 
at runtime using the flag [max_threads](@ref max_threads)).



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

    + `--enable-max-performance=yes` will try to support the best
available flags for a specific architectural model. Currently, the option
assumes a recent version of `GCC`.
    + `--enable-debug-yap` compiles YAP so that it can be debugged
by tools such as `dbx` or `gdb`.


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

    + 486:
In order to take advantage of 486 specific optimizations in GCC 2.7.\*:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
YAP_EXTRAS= ... -m486 -DBP_FREE=1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + Pentium:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
YAP_EXTRAS= ... -m486 -malign-loops=2 -malign-jumps=2 \
                      -malign-functions=2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + PentiumPro and other recent Intel and AMD machines:
PentiumPros are known not to require alignment. Check your version of
`GCC` for the best `-march` option.

    + Super and UltraSparcs:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
YAP_EXTRAS= ... -msupersparc
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + MIPS: if have a recent machine and you need a 64 bit wide address
space you can use the abi 64 bits or eabi option, as in:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CC="gcc -mabi=64" ./configure --...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Be careful. At least for some versions of `GCC`, compiling with
`-g` seems to result in broken code.

    + WIN32: GCC is distributed in the MINGW32 and CYGWIN packages.

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



@subsection Compiling_Under_Visual_C Compiling Under Visual C++

YAP used to compile cleanly under Microsoft's Visual C++ release 6.0. We next
give a step-by-step review on how the core YAP compiled manually using this
environment.

First, it is a good idea to build YAP as a DLL:

    + create a project named yapdll using File.New. The project will be a
DLL project, initially empty.

Notice that either the project is named yapdll or you must replace the
preprocessors variable  _$YAPDLL_EXPORTS_ to match your project names
in the files YAPInterface.h and `c_interface.c`.

    + add all .c files in the $YAPSRC/C directory and in the
$YAPSRC\\OPTYAP directory to the Project's `Source Files` (use
FileView).

    + add all .h files in the  _$YAPSRC/H_ directory,
 _$YAPSRC\\include_ directory and in the  _$YAPSRC\\OPTYAP_
subdirectory to the Project's `Header Files`.

    + Ideally, you should now use `m4` to generate extra  .h from .m4 files and use
`configure` to create a `config.h`. Or, you can be lazy, and
fetch these files from  _$YAPSRC\\VC\\include_.

    + You may want to go to `Build.Set Active Configuration` and
set `Project Type` to `Release`

    + To use YAP's own include directories you have to set the Project
option  `Project.Project Settings.C/C++.Preprocessor.Additional Include Directories` to include the directories  _$YAPSRC\\H_,
 _$YAPSRC\\VC\\include_,  _$YAPSRC\\OPTYAP_ and
 _$YAPSRC\\include_.  The syntax is:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$YAPSRC\H, $YAPSRC\VC\include, $YAPSRC\OPTYAP, $YAPSRC\include
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + Build: the system should generate an yapdll.dll and an yapdll.lib.

    + Copy the file yapdll.dll to your path. The file
    yapdll.lib should also be copied to a location where the linker can find it.


Now you are ready to create a console interface for YAP:

<ol>
 <li>create a second project  say `wyap` with `File.New`. The project will be a
WIN32 console project, initially empty.

    + add  _$YAPSRC\\console\\yap.c_ to the `Source Files`.

    + add  _$YAPSRC\\VC\\include\\config.h_ and the files in  _$YAPSRC\\include_ to
the `Header Files`.

    + You may want to go to `Build.Set Active Configuration` and set
`Project Type` to `Release`.

    + you will eventually need to bootstrap the system by booting from
`boot.yap`, so write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -b $YAPSRC\pl\boot.yap
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

in `Project.Project Settings.Debug.Program Arguments`.

    + You need the sockets and yap libraries. Add

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ws2_32.lib yapdll.lib
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

to `Project.Project Settings.Link.Object/Library Modules`

You may also need to set the `Link Path` so that VC++ will find `yapdll.lib`.

    + set `Project.Project Settings.C/C++.Preprocessor.Additional Include Directories` to include the 
 _$YAPSRC/VC/include_ and
 _$YAPSRC/include_.

The syntax is:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$YAPSRC\VC\include, $YAPSRC\include
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + Build the system.

    + Use `Build.Start Debug` to boot the system, and then create the saved state with

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

    + 64 bit
Support for 64 bits should work by using (under Bourne shell syntax):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CC="cc -64" $YAP_SRC_PATH/configure --...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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

    + -?
print a short error message.
    + -s _Size_
allocate  _Size_ KBytes for local and global stacks. The user may
specify <tt>M</tt> bytes.
    + -h _Size_
allocate  _Size_ KBytes for heap and auxiliary stacks
    + -t _Size_
allocate  _Size_ KBytes for the trail stack
    + -L _Size_ 
SWI-compatible option to allocate  _Size_ K bytes for local and global stacks, the local stack
cannot be expanded. To avoid confusion with the load option,  _Size_
must immediately follow the letter `L`.
    + -G _Size_
SWI-compatible option to allocate  _Size_ K bytes for local and global stacks; the global
stack cannot be expanded
    + -T _Size_
SWI-compatible option to allocate  _Size_ K bytes for the trail stack; the trail cannot be expanded.
    + -l  _YAP_FILE_
compile the Prolog file  _YAP_FILE_ before entering the top-level.
    + -L  _YAP_FILE_
compile the Prolog file  _YAP_FILE_ and then halt. This option is
useful for implementing scripts.
    + -g  _Goal_
run the goal  _Goal_ before top-level. The goal is converted from
an atom to a Prolog term.
    + -z  _Goal_
run the goal  _Goal_ as top-level. The goal is converted from
an atom to a Prolog term.
    + -b  _BOOT_FILE_
boot code is in Prolog file  _BOOT_FILE_. The filename must define
the predicate `'$live'/0`.
    + -c <tt>IP_HOST</tt> <tt>port</tt>
connect standard streams to host <tt>IP_HOST</tt> at port <tt>port</tt>
    + filename
restore state saved in the given file
    + -f
do not consult initial files
    + -q
do not print informational messages
    + --
separator for arguments to Prolog code. These arguments are visible
through the [unix/1](@ref unix) built-in predicate.


Note that YAP will output an error message on the following conditions:

    + 
a file name was given but the file does not exist or is not a saved
YAP state;
    + 
the necessary amount of memory could not be allocated;
    + 
the allocated memory is not enough to restore the state.


When restoring a saved state, YAP will allocate the
same amount of memory as that in use when the state was saved, unless a
different amount is specified by flags in the command line. By default,
YAP restores the file startup.yss from the current directory or from
the YAP library.

    + 
YAP usually boots from a saved state. The saved state will use the default
installation directory to search for the YAP binary unless you define
the environment variable YAPBINDIR.

    + 
YAP always tries to find saved states from the current directory
first. If it cannot it will use the environment variable YAPLIBDIR, if
defined, or search the default library directory.

    + 
YAP will try to find library files from the YAPSHAREDIR/library
directory.


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
they must be sent directly to the argv built-in. Hence, running

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

     + \a op(N,T) denotes an atom which has been previously declared with type
\a T and base precedence \a N.

    + Since ',' is itself a pre-declared operator with type \a xfy and
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

    + 
In the first rule for term(0) no blank space should exist between
\a atom and '('.

    + 
Each term to be read by the YAP parser must end with a single
dot, followed by a blank (in the sense mentioned in the previous
paragraph). When a name consisting of a single dot could be taken for
the end of term marker, the ambiguity should be avoided by surrounding the
dot with single quotes.



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
load_files/2 for loading Prolog source with an alternative
encoding, `open/4` when opening files or using `set_stream/2` on
any open stream (not yet implemented). For Prolog source files we also
provide the `encoding/1` directive that can be used to switch
between encodings that are compatible to US-ASCII (`ascii`,
`iso_latin_1`, `utf8` and many locales).  



For
additional information and Unicode resources, please visit
<http://www.unicode.org/>.

YAP currently defines and supports the following encodings:

    + octet
Default encoding for <em>binary</em> streams.  This causes
the stream to be read and written fully untranslated.

    + ascii
7-bit encoding in 8-bit bytes.  Equivalent to `iso_latin_1`,
but generates errors and warnings on encountering values above
127.

    + iso_latin_1
8-bit encoding supporting many western languages.  This causes
the stream to be read and written fully untranslated.

    + text
C-library default locale encoding for text files.  Files are read and
written using the C-library functions `mbrtowc()` and
`wcrtomb()`.  This may be the same as one of the other locales,
notably it may be the same as `iso_latin_1` for western
languages and `utf8` in a UTF-8 context.

    + utf8
Multi-byte encoding of full UCS, compatible to `ascii`.
See above.

    + unicode_be
Unicode Big Endian.  Reads input in pairs of bytes, most
significant byte first.  Can only represent 16-bit characters.

    + unicode_le
Unicode Little Endian.  Reads input in pairs of bytes, least
significant byte first.  Can only represent 16-bit characters.


Note that not all encodings can represent all characters. This implies
that writing text to a stream may cause errors because the stream
cannot represent these characters. The behaviour of a stream on these
errors can be controlled using `open/4` or `set_stream/2` (not
implemented). Initially the terminal stream write the characters using
Prolog escape sequences while other streams generate an I/O exception.

@subsection BOM BOM: Byte Order Mark

From Stream Encoding, you may have got the impression that
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
available through stream_property/2. When opening a file for
writing, writing a BOM can be requested using the option
`bom(true)` with `open/4`.

@defgroup YAPProgramming Loading and Manipulating Programs in YAP

@page  Loading and Manipulating Programs in YAP


Next, we present the main predicates and directives available to load
files and to control the Prolog environment.

   + \subpage YAPConsulting

   + \subpage YAPAbsoluteFileName

   + \subpage YAPModules

   + \subpage YAPSaving


This chapter describes the predicates for controlling the execution of
Prolog programs.

In the description of the arguments of functors the following notation
will be used:

  + a preceding plus sign will denote an argument as an "input argument" - it cannot be a free variable at the time of the call; 
  + a preceding minus sign will denote an "output argument";
  + an argument with no preceding symbol can be used in both ways.


@defgroup YAPBuiltins Built-In Predicates

@page  BIPs Built-In Predicates

@subpage YAPControl

@subpage Undefined_Procedures


@subpage Messages


@subpage Testing_Terms


@subpage Predicates_on_Atoms


@subpage Predicates_on_Characters


@subpage Comparing_Terms


@subpage Arithmetic


@subpage InputOutput


@subpage Streams_and_Files


@subpage ChYProlog_File_Handling


@subpage InputOutput_of_Terms


@subpage InputOutput_of_Characters


@subpage InputOutput_for_Streams


@subpage ChYProlog_to_Terminal


@subpage InputOutput_Control


@subpage Sockets


@subpage Database


@subpage Modifying_the_Database


@subpage Looking_at_the_Database


@subpage Database_References


@subpage Internal_Database


@subpage BlackBoard


@subpage Sets


@subpage Grammars


@subpage OS


@subpage Term_Modification


@subpage Global_Variables


@subpage Profiling


@subpage The_Count_Profiler


@subpage Tick_Profiler


@subpage Call_Counting

@subpage Arrays


@subpage Preds


@subpage Misc


@page  pYapLibraries Libraries


@subpage Aggregate


@subpage Apply


@subpage Association_Lists


@subpage AVL_Trees


@subpage Exo_Intervals


@subpage Gecode


@subpage Heaps


@subpage Lists


@subpage LineUtilities


@subpage matrix


@subpage MATLAB


@subpage NonhYBacktrackable_Data_Structures


@subpage Ordered_Sets


@subpage Pseudo_Random


@subpage Queues


@subpage Random


@subpage Read_Utilities


@subpage RedhYBlack_Trees


@subpage RegExp


@subpage shlib


@subpage Splay_Trees


@subpage String_InputOutput


@subpage System


@subpage Terms


@subpage Tries


@subpage Cleanup


@subpage Timeout


@subpage Trees


@subpage UGraphs


@subpage DGraphs


@subpage UnDGraphs


@subpage DBUsage


@subpage Lambda

@page  pYapPacks YAP Packages

@subpage LAM


@subpage BDDs


@subpage Block_Diagram


@subpage Invoking_Predicates_on_all_Members_of_a_List


@subpage Forall


@subpage Compatibility_of_Global_Variables


@subpage Rational_Trees


@subpage CohYroutining


@subpage Attributed_Variables


@subpage New_Style_Attribute_Declarations


@subpage Old_Style_Attribute_Declarations


@subpage CLPR_Solver_Predicates


@subpage CLPR_Syntax


@subpage CLPR_Unification


@subpage CLPR_NonhYlinear_Constraints


@subpage CHR_Introduction


@subpage CHR_Syntax_and_Semantics


@subpage CHR_in_YAP_Programs


@subpage CHR_Debugging


@subpage CHR_Examples


@subpage CHR_Compatibility


@subpage CHR_Guidelines


@subpage Logtalk


@subpage Creating_and_Destroying_Prolog_Threads


@subpage Monitoring_Threads


@subpage Thread_Communication


@subpage Thread_Synchronisation


@subpage Parallelism


@subpage Tabling


@subpage Low_Level_Tracing


@subpage Low_Level_Profiling


@subpage Debugging


@subpage Deb_Preds


@subpage Deb_Interaction


@subpage Indexing


@subpage Loading_Objects


@subpage SavebQeERest



Next, we present the main predicates and directives available to load
files and to control the Prolog environment.



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

    + 'LC'
The following Prolog text uses lower case letters.

    + 'NOLC'
The following Prolog text uses upper case letters only.


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

    + Differently from SICStus Prolog, YAP does not have a
notion of interpreted code. All code in YAP is compiled.

    + YAP does not support an intermediate byte-code
representation, so the `fcompile/1` and `load/1` built-ins are
not available in YAP.

    + YAP implements escape sequences as in the ISO standard. SICStus
Prolog implements Unix-like escape sequences.

    + YAP implements initialization/1 as per the ISO
standard. Use prolog_initialization/1 for the SICStus Prolog
compatible built-in.

    + Prolog flags are different in SICStus Prolog and in YAP.

    + The SICStus Prolog `on_exception/3` and
`raise_exception` built-ins correspond to the ISO built-ins
catch/3 and throw/1.

    + The following SICStus Prolog v3 built-ins are not (currently)
implemented in YAP (note that this is only a partial list):
file_search_path/2,
`stream_interrupt/3`, `reinitialize/0`, `help/0`,
`help/1`, `trimcore/0`, `load_files/1`,
load_files/2, and `require/1`.

The previous list is incomplete. We also cannot guarantee full
compatibility for other built-ins (although we will try to address any
such incompatibilities). Last, SICStus Prolog is an evolving system, so
one can be expect new incompatibilities to be introduced in future
releases of SICStus Prolog.

    + YAP allows asserting and abolishing static code during
execution through the assert_static/1 and abolish/1
built-ins. This is not allowed in Quintus Prolog or SICStus Prolog.

    + The socket predicates, although designed to be compatible with
SICStus Prolog, are built-ins, not library predicates, in YAP.

    + This list is incomplete.



The following differences only exist if the language flag is set
to `yap` (the default):

    + The consult/1 predicate in YAP follows C-Prolog
semantics. That is, it adds clauses to the data base, even for
preexisting procedures. This is different from consult/1 in
SICStus Prolog or SWI-Prolog.

    + 
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
retract/1 will succeed. The call to  *assertz/1* will then
succeed. On backtracking, the system will retry
retract/1. Because the newly asserted goal is visible to
retract/1, it can be retracted from the data base, and
`retract(a(X))` will succeed again. The process will continue
generating integers for ever. Immediate semantics were used in C-Prolog.

With logical update semantics, any additions or deletions of clauses
for a goal 
<em>will not affect previous activations of the goal</em>. In the example,
the call to assertz/1 will not see the 
update performed by the assertz/1, and the query will have a
single solution.

Calling `yap_flag(update_semantics,logical)` will switch
YAP to use logical update semantics.

    + dynamic/1 is a built-in, not a directive, in YAP.

    + By default, YAP fails on undefined predicates. To follow default
SICStus Prolog use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- yap_flag(unknown,error).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + By default, directives in YAP can be called from the top level.



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

    + YAP now supports all of the built-ins required by the
ISO-standard, and,
    + Error-handling is as required by the standard.


YAP by default is not fully ISO standard compliant. You can set the 
language flag to `iso` to obtain very good
compatibility. Setting this flag changes the following:

    + By default, YAP uses "immediate update semantics" for its
database, and not "logical update semantics", as per the standard,
( (see SICStus Prolog)). This affects assert/1,
retract/1, and friends.

Calling `set_prolog_flag(update_semantics,logical)` will switch
YAP to use logical update semantics.

    + By default, YAP implements the 
atom_chars/2( (see Testing Terms)), and 
number_chars/2, ( (see Testing Terms)), 
built-ins as per the original Quintus Prolog definition, and
not as per the ISO definition.

Calling `set_prolog_flag(to_chars_mode,iso)` will switch
YAP to use the ISO definition for
atom_chars/2 and number_chars/2.

    + By default, YAP allows executable goals in directives. In ISO mode
most directives can only be called from top level (the exceptions are
set_prolog_flag/2 and op/3).

    + Error checking for meta-calls under ISO Prolog mode is stricter
than by default.

    + The strict_iso flag automatically enables the ISO Prolog
standard. This feature should disable all features not present in the
standard.



The following incompatibilities between YAP and the ISO standard are
known to still exist:

<ul>

 <li>Currently, YAP does not handle overflow errors in integer
operations, and handles floating-point errors only in some
architectures. Otherwise, YAP follows IEEE arithmetic.



Please inform the authors on other incompatibilities that may still
exist.

@section Operators Summary of YAP Predefined Operators

The Prolog syntax caters for operators of three main kinds:

    +  prefix;
    + infix;
    + postfix.


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


