## [Compatibility with other Prolog systems](iso_c)


YAP has been designed to be as compatible as possible with other
Prolog systems, originally with C-Prolog\cite x and SICStus
Prolog~\cite x . More recent work on YAP has striven at making YAP
compatible with the ISO-Prolog standard\cite x , and with Jan
Wielemaker's SWI-Prolog\cite x .

SWI-Prolog and YAP have collaborated at improved compatibility \cite x . This
resulted in Prolog extensions such as the `dialect` feature. YAP
currently supports most of the SWI-Prolog foreign interface. The following  SWI
libraries have been adapted to YAP:

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

@section ChYProlog Compatibility with the C-Prolog interpreter


YAP was designed so that most C-Prolog programs should run under YAP
without changes.
The most important difference between YAP and C-Prolog is that, being
YAP a compiler, some changes should be made if predicates such as
assert/1, clause/1 and retract/1 are used. First
predicates which will change during execution should be declared as
`dynamic` by using commands like:

~~~~~~
:- dynamic f/n.
~~~~~~

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


### [Compatibility with the Quintus and SICStus Prolog systems](QuiSics)

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


@section ISOCompat Compatibility with the ISO Prolog standard


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

 + Currently, YAP does not handle overflow errors in integer
operations, and handles floating-point errors only in some
architectures. Otherwise, YAP follows IEEE arithmetic.

Please inform the authors on other incompatibilities that may still
exist.
