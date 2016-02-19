 YAP 6-3.4 Manual                         {#mainpage}
====================

This file documents the YAP Prolog System version 6.3.4, a high-performance Prolog compiler developed at LIACC, Universidade do Porto. YAP is based on David H. D. Warren's WAM (Warren Abstract Machine), with several optimizations for better performance. YAP follows the Edinburgh tradition, and is largely compatible with DEC-10 Prolog, Quintus Prolog, and especially with C-Prolog.

+ @ref  download

+ @ref  install

+ @ref run

+ @ref YAPSyntax

+ @ref consult

+ @ref builtins

+ @ref extensions

+ @ref library

+ @ref packages

+ @ref swi

+ @ref YAPProgramming

+ @ref fli



\author Vitor Santos Costa,
\author Luís Damas,
\author Rogério Reis
\author Rúben Azevedo


© 1989-201 L. Damas, V. Santos Costa and Universidade
do Porto.
Permission is granted to make and distribute verbatim copies of this manual provided the copyright notice and this permission notice are preserved on all copies.
Permission is granted to copy and distribute modified versions of this manual under the conditions for verbatim copying, provided that the entire resulting derived work is distributed under the terms of a permission notice identical to this one.
Permission is granted to copy and distribute translations of this manual into another language, under the above conditions for modified versions.

This manual was written by Vítor Santos Costa,
Luís Damas, Rogério Reis, and Rúben Azevedo. The
manual is largely based on the DECsystem-10 Prolog User's Manual by
D.L. Bowen, L. Byrd, F. C. N. Pereira, L. M. Pereira, and
D. H. D. Warren. We have  used comments from the Edinburgh Prolog
library written by R. O'Keefe. Documentation from many built-ins is
originally from the SWI-Prolog manual, with the gracious authorization
from
Jan Wielemaker. We would also like to gratefully
acknowledge the contributions from Ashwin Srinivasian.



Loading and Organising YAP Programs      {#consult}
===================================

  @ingroup main

  Next, we present the main predicates and directives available to load
  files and to control the Prolog environment.

  + @ref YAPConsulting

  + @ref YAPModules

  +@ref YAPSaving


  This chapter describes the predicates  controlling the execution of
  Prolog programs.

  In the description of the arguments of functors the following notation
  will be used:

  + a preceding plus sign will denote an argument as an "input
  argument" - it cannot be a free variable at the time of the call;
  + a preceding minus sign will denote an "output argument";
  + an argument with no preceding symbol can be used in both ways.

Running YAP                      {#run}
===========

We next describe how to invoke YAP in Unix systems.

Running YAP Interactively
-------------------------

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
through the unix/1 built-in predicate.


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


Prolog Scripts
--------------

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

YAP Built-ins          {#builtins}
=============

  + @ref YAPControl

  + @ref arithmetic

  + @ref YAPChars

  + @ref YAP_Terms

  + @ref InputOutput
  
  + @ref AbsoluteFileName

  + @ref YAPOS

  + @ref Internal_Database

  + @ref Sets

Extensions to core Prolog.  {#extensions}
==========================

YAP includes a number of extensions over the original Prolog
language. Next, we discuss how to use the most important ones.

  + @ref Rational_Trees

  + @ref AttributedVariables

  + @ref  DepthLimited

  + @ref  Tabling

  + @ref Threads

  + @ref Profiling

  + @ref YAPArrays

  + @ref Parallelism

The YAP Library        {#library}
===============

@defgroup library YAP library files
@{

  Library files reside in the library_directory path (set by the
  `LIBDIR` variable in the Makefile for YAP). Several files in the
  library are originally from the public-domain Edinburgh Prolog library.

- @ref apply
- @ref apply_macros
- @ref arg
- @ref Association_Lists
- @ref avl
- @ref bhash
- @ref block_diagram
- @ref c_alarms
- @ref charsio
- @ref clauses
- @ref cleanup
- @ref dbqueues
- @ref dbusage
- @ref dgraphs
- @ref exo_interval
- @ref flags
- @ref gensym
- @ref yap_hacks
- @ref heaps
- @ref lam_mpi
- @ref line_utils
- @ref swi_listing
- @ref lists
- @ref mapargs
- @ref maplist
- @ref matlab
- @ref matrix
- @ref nb
- @ref Ordered_Sets
- @ref parameters
- @ref queues
- @ref random
- @ref Pseudo_Random
- @ref rbtrees
- @ref regexp
- @ref rltrees
- @ref Splay_Trees
- @ref operating_system_support,
- @ref Terms
- @ref timeout
- @ref trees
- @ref tries
- @ref ugraphs
- @ref undgraphs
- @ref varnumbers
- @ref wdgraphs
- @ref wdgraphs
- @ref wdgraphs
- @ref wgraphs
- @ref wundgraphs
- @ref ypp
@}

The YAP Packages  {#packages}
================

@defgroup packages YAP packages files
@{

+ @ref real

+ @ref BDDs

+ @ref  Gecode

+ @ref  MYDDAS

+ @ref PFL

+ @ref ProbLog1

+ @ref python

+ @ref YAPRaptor

+ @ref YAP-LBFGS

+ @subpage yap-udi-indexers

Leuven packages ported from SWI-Prolog:

+ @subpage chr

+ @subpage clpqr

@}

Compatibility {#swi}
=============


@defgroup swi  Compatibility 
@{


YAP has been designed to be as compatible as possible with other
Prolog systems, originally with C-Prolog\cite x and SICStus
Prolog~\cite x . More recent work on YAP has striven at making YAP
compatible with the ISO-Prolog standard\cite x , and with Jan
Wielemaker's SWI-Prolog\cite x .

SWI-Prolog and YAP have collaborated at improved compatibility \cite x . This
resulted in Prolog extensions such as the `dialect` feature. YAP
currently supports most of the SWI-Prolog foreign interface. The following  SWI
libraries have worked on YAP:

  + @ref aggregate
  + @ref base64
  + @ref broadcast
  + @ref ctypes
  + @ref date
  + @ref prolog_debug
  + @ref prolog_edit
  + @ref error
  + @ref nb_set
  + @ref prolog_operator
  + @ref swi_option
  + @ref pairs
  + @ref pio
  + @ref predicate_options,
  + @ref predopts
  + @ref prolog_clause
  + @ref prolog_colour
  + @ref prolog_source
  + @ref prolog_xref
  + @ref pure_input
  + @ref quasi_quotations
  + @ref read_util
  + @ref record
  + @ref settings
  + @ref shlib
  + @ref thread_pool
  + @ref url
  + @ref utf8
  + @ref win_menu
  + @ref www_browser


Note that in general SWI code may be from an earlier version than the
one available with SWI-Prolog. SWI-Prolog are obviously not
responsible for any incompatibilities and/or bugs in the YAP port.

Please do refer to the SWI-Prolog home page:

<http://www.swi-prolog.org>

for more information on SWI-Prolog and the SWI packages.

Compatibility with the C-Prolog interpreter {#ChYProlog}
-------------------------------------------

YAP was designed so that most C-Prolog programs should run under YAP
without changes.
The most important difference between YAP and C-Prolog is that, being
YAP a compiler, some changes should be made if predicates such as
assert/1, clause/1 and retract/1 are used. First
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


Compatibility with the Quintus and SICStus Prolog systems
---------------------------------------------------------

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

Both YAP and SICStus Prolog obey the Edinburgh Syntax and are based on
the WAM. Even so, there are major important differences:

  + Differently from SICStus Prolog, both consulted and dynamic code in YAP
  are compiled, not interpreted. All code in YAP is compiled.

  + The following SICStus Prolog v3 built-ins are not (currently)
implemented in YAP (note that this is only a partial list):
stream_interrupt/3, reinitialize/0, help/0, help/1,
trimcore/0, and require/1.

  + The consult/1 predicate in YAP follows C-Prolog
semantics. That is, it adds clauses to the data base, even for
preexisting procedures. This is different from consult/1 in
SICStus Prolog or SWI-Prolog.

  + This list is incomplete.

Compatibility with the ISO Prolog standard
------------------------------------------

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
language flag to `iso` to obtain better
compatibility. Setting this flag changes the following:


  + By default, YAP implements the
atom_chars/2 (see Testing Terms), and
number_chars/2,  (see Testing Terms),
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
known to still exist (please check Ulrich Neumerkel's page for more details):

<ul>

 <li>Currently, YAP does not handle overflow errors in integer
operations, and handles floating-point errors only in some
architectures. Otherwise, YAP follows IEEE arithmetic.

Please inform the authors on other incompatibilities that may still
exist.

@}

Foreign Language interface for YAP     {#fli}
==================================

@defgroup fli Foreigd Code Interfacing

@{

YAP provides the user with three facilities for writing
predicates in a language other than Prolog. Under Unix systems,
most language implementations were linkable to `C`, and the first interface exported  the YAP machinery to the C language. YAP also implements most of the SWI-Prolog foreign language interface.
This gives portability with a number of SWI-Prolog packages and avoids garnage collection by using @ref slotInterface. Last, a new C++ based interface is
being designed to work with the swig (www.swig.orgv) interface compiler.

+ The @ref c-interface exports the YAP engine.

+ The @ref swi-c-interface emulates Jan Wielemaker's SWI foreign language interface.

+ The @ref  yap-cplus-interface is desiged to interface with the SWIG package by using Object-Oriented concepts

+ The @ref LoadInterface handles the setup of foreign files

@}

