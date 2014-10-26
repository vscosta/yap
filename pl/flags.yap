/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		flags.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	controlling YAP						 *
*									 *
*************************************************************************/

:- system_module( '$_flags', [create_prolog_flag/3,
        current_prolog_flag/2,
        no_source/0,
        prolog_flag/2,
        prolog_flag/3,
        set_prolog_flag/2,
        source/0,
        source_mode/2,
        yap_flag/2,
        yap_flag/3], []).

/** @defgroup Flags  YAP Execution Flags
    @ingroup YAPBuiltins
    @{
*/

:- use_system_module( '$_boot', ['$prompt_alternatives_on'/1]).

:- use_system_module( '$_checker', ['$syntax_check_discontiguous'/2,
        '$syntax_check_multiple'/2,
        '$syntax_check_single_var'/2]).

:- use_system_module( '$_control', ['$set_toplevel_hook'/1]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_yio', ['$default_expand'/1,
        '$set_default_expand'/1]).

/** @pred  yap_flag(? _Param_,? _Value_) 


Set or read system properties for  _Param_:


+ `argv `

    Read-only flag. It unifies with a list of atoms that gives the
arguments to YAP after `--`.

+ `agc_margin `

    An integer: if this amount of atoms has been created since the last
atom-garbage collection, perform atom garbage collection at the first
opportunity. Initial value is 10,000. May be changed. A value of 0
(zero) disables atom garbage collection.

+ `associate `

    Read-write flag telling a suffix for files associated to Prolog
sources. It is `yap` by default.

+ `arithmetic_exceptions `

    Read-write flag telling whether arithmetic exceptions generate
    Prolog exceptions. If enabled:

~~~~
 ?- X is 2/0.
     ERROR!!
     ZERO DIVISOR ERROR- X is Exp
~~~~

    If disabled:
~~~~
 ?- X is 2/0.
X = (+inf).
~~~~

    It is `true` by default, but it is disabled by packages like CLP(BN) and ProbLog.

+ `bounded` is iso

    Read-only flag telling whether integers are bounded. The value depends
on whether YAP uses the GMP library or not.

+ `profiling `

    If `off` (default) do not compile call counting information for
procedures. If `on` compile predicates so that they calls and
retries to the predicate may be counted. Profiling data can be read through the
call_count_data/3 built-in.

+ `char_conversion is iso`

    Writable flag telling whether a character conversion table is used when
reading terms. The default value for this flag is `off` except in
`sicstus` and `iso` language modes, where it is `on`.

+ `character_escapes is iso `

    Writable flag telling whether a character escapes are enables,
`true`, or disabled, `false`. The default value for this flag is
`on`.

+ `debug is iso `

    If  _Value_ is unbound, tell whether debugging is `true` or
`false`. If  _Value_ is bound to `true` enable debugging, and if
it is bound to `false` disable debugging.

+ `debugger_print_options `

    If bound, set the argument to the `write_term/3` options the
debugger uses to write terms. If unbound, show the current options.

+ `dialect `

    Read-only flag that always returns `yap`.

+ `discontiguous_warnings `

    If  _Value_ is unbound, tell whether warnings for discontiguous
predicates are `on` or
`off`. If  _Value_ is bound to `on` enable these warnings,
and if it is bound to `off` disable them. The default for YAP is
`off`, unless we are in `sicstus` or `iso` mode.

+ `dollar_as_lower_case `

    If `off` (default)  consider the character `$` a control character, if
`on` consider `$` a lower case character.

+ `double_quotes is iso `

    If  _Value_ is unbound, tell whether a double quoted list of characters
token is converted to a list of atoms, `chars`, to a list of integers,
`codes`, or to a single atom, `atom`. If  _Value_ is bound, set to
the corresponding behavior. The default value is `codes`.

+ `executable `

    Read-only flag. It unifies with an atom that gives the
original program path.

+ `fast `

    If `on` allow fast machine code, if `off` (default) disable it. Only
available in experimental implementations.

+ `fileerrors`

    If `on` `fileerrors` is `on`, if `off` (default)
`fileerrors` is disabled.

+ `float_format `

    C-library `printf()` format specification used by write/1 and
friends to determine how floating point numbers are printed. The
default is `%.15g`. The specified value is passed to `printf()`
without further checking. For example, if you want less digits
printed, `%g` will print all floats using 6 digits instead of the
default 15.

+ `gc`

    If `on` allow garbage collection (default), if `off` disable it.

+ `gc_margin `

    Set or show the minimum free stack before starting garbage
collection. The default depends on total stack size. 

+ `gc_trace `

    If `off` (default) do not show information on garbage collection
and stack shifts, if `on` inform when a garbage collection or stack
shift happened, if verbose give detailed information on garbage
collection and stack shifts. Last, if `very_verbose` give detailed
information on data-structures found during the garbage collection
process, namely, on choice-points.

+ `generate_debugging_info `

    If `true` (default) generate debugging information for
procedures, including source mode. If `false` predicates no
information is generated, although debugging is still possible, and
source mode is disabled.

+ `host_type `

    Return `configure` system information, including the machine-id
for which YAP was compiled and Operating System information. 

+ `index `

    If `on` allow indexing (default), if `off` disable it, if
`single` allow on first argument only.

+ `index_sub_term_search_depth `

    Maximum bound on searching sub-terms for indexing, if `0` (default) no bound.

+ `informational_messages `

    If `on` allow printing of informational messages, such as the ones
that are printed when consulting. If `off` disable printing
these messages. It is `on` by default except if YAP is booted with
the `-L` flag.

+ `integer_rounding_function is iso `

    Read-only flag telling the rounding function used for integers. Takes the value
`toward_zero` for the current version of YAP.

+ `language `

    Choose whether YAP is closer to C-Prolog, `cprolog`, iso-prolog,
`iso` or SICStus Prolog, `sicstus`. The current default is
`cprolog`. This flag affects update semantics, leashing mode,
style checking, handling calls to undefined procedures, how directives
are interpreted, when to use dynamic, character escapes, and how files
are consulted.

+ `max_arity is iso `

    Read-only flag telling the maximum arity of a functor. Takes the value
`unbounded` for the current version of YAP.

+ `max_integer is iso `

    Read-only flag telling the maximum integer in the
implementation. Depends on machine and Operating System
architecture, and on whether YAP uses the `GMP` multi-precision
library. If bounded is false, requests for max_integer
will fail.

+ `max_tagged_integer  `

    Read-only flag telling the maximum integer we can store as a single
word. Depends on machine and Operating System
architecture. It can be used to find the word size of the current machine.

+ `min_integer is iso `

    Read-only flag telling the minimum integer in the
implementation. Depends on machine and Operating System architecture,
and on whether YAP uses the `GMP` multi-precision library. If
bounded is false, requests for min_integer will fail.

+ `min_tagged_integer  `

    Read-only flag telling the minimum integer we can store as a single
word. Depends on machine and Operating System
architecture.

+ `n_of_integer_keys_in_bb `

    Read or set the size of the hash table that is used for looking up the
blackboard when the key is an integer.

+ `occurs_check `

    Current read-only and set to `false`.

+ `n_of_integer_keys_in_db `

    Read or set the size of the hash table that is used for looking up the
internal data-base when the key is an integer.

+ `open_expands_filename `

    If `true` the open/3 builtin performs filename-expansion
before opening a file (SICStus Prolog like). If `false` it does not
(SWI-Prolog like).

+ `open_shared_object `

    If true, `open_shared_object/2` and friends are implemented,
providing access to shared libraries (`.so` files) or to dynamic link
libraries (`.DLL` files).

+ `profiling `

    If `off` (default) do not compile profiling information for
procedures. If `on` compile predicates so that they will output
profiling information. Profiling data can be read through the
profile_data/3 built-in.

+ `prompt_alternatives_on(atom, changeable) `

    SWI-Compatible option, determines prompting for alternatives in the Prolog toplevel. Default is <tt>groundness</tt>, YAP prompts for alternatives if and only if the query contains variables. The alternative, default in SWI-Prolog is <tt>determinism</tt> which implies the system prompts for alternatives if the goal succeeded while leaving choicepoints.

+ `qcompile(+{never, auto, large, part}, changeable)`

    SWI-Prolog flag that controls whether loaded files should be also
    compiled into qfiles. The default value is `never`.

 `never`, no qcompile file is generated unless the user calls
 qsave_file/1 and friends, or sets the qcompile option in
 load_files/2; 

  `auto`, all files are qcompiled.

  `large`, files above 100KB are qcompiled.

  `part`, not supported in YAP.

+ `redefine_warnings `

    If  _Value_ is unbound, tell whether warnings for procedures defined
in several different files are `on` or
`off`. If  _Value_ is bound to `on` enable these warnings,
and if it is bound to `off` disable them. The default for YAP is
`off`, unless we are in `sicstus` or `iso` mode.

+ `shared_object_search_path `

    Name of the environment variable used by the system to search for shared
objects.

+ `shared_object_extension `

    Suffix associated with loadable code.

+ `single_var_warnings `

    If  _Value_ is unbound, tell whether warnings for singleton variables
are `on` or `off`. If  _Value_ is bound to `on` enable
these warnings, and if it is bound to `off` disable them. The
default for YAP is `off`, unless we are in `sicstus` or
`iso` mode.

+ `strict_iso `

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

+ `stack_dump_on_error `

    If `on` show a stack dump when YAP finds an error. The default is
`off`.

+ `syntax_errors`

    Control action to be taken after syntax errors while executing read/1,
`read/2`, or `read_term/3`:
  + `dec10`
Report the syntax error and retry reading the term.
  + `fail`
Report the syntax error and fail (default).
  + `error`
Report the syntax error and generate an error.
  + `quiet`
Just fail

+ `system_options `

    This read only flag tells which options were used to compile
YAP. Currently it informs whether the system supports `big_numbers`,
`coroutining`, `depth_limit`, `low_level_tracer`,
`or-parallelism`, `rational_trees`, `readline`, `tabling`,
`threads`, or the `wam_profiler`.

+ `tabling_mode`

    Sets or reads the tabling mode for all tabled predicates. Please
 (see Tabling) for the list of options.

+ `to_chars_mode `

    Define whether YAP should follow `quintus`-like
semantics for the `atom_chars/1` or `number_chars/1` built-in,
or whether it should follow the ISO standard (`iso` option).

+ `toplevel_hook `

    If bound, set the argument to a goal to be executed before entering the
top-level. If unbound show the current goal or `true` if none is
presented. Only the first solution is considered and the goal is not
backtracked into.

+ `toplevel_print_options `

    If bound, set the argument to the `write_term/3` options used to write
terms from the top-level. If unbound, show the current options.

+ `typein_module `

    If bound, set the current working or type-in module to the argument,
which must be an atom. If unbound, unify the argument with the current
working module.

+ `unix`

    Read-only Boolean flag that unifies with `true` if YAP is
running on an Unix system.  Defined if the C-compiler used to compile
this version of YAP either defines `__unix__` or `unix`.

+ `unknown is iso`

    Corresponds to calling the unknown/2 built-in. Possible values 
are `error`, `fail`, and `warning`.

+ `update_semantics `

    Define whether YAP should follow `immediate` update
semantics, as in C-Prolog (default), `logical` update semantics,
as in Quintus Prolog, SICStus Prolog, or in the ISO standard. There is
also an intermediate mode, `logical_assert`, where dynamic
procedures follow logical semantics but the internal data base still
follows immediate semantics.

+ `user_error `

    If the second argument is bound to a stream, set user_error to
this stream. If the second argument is unbound, unify the argument with
the current user_error stream.
By default, the user_error stream is set to a stream
corresponding to the Unix `stderr` stream.
The next example shows how to use this flag:

~~~{.prolog}
    ?- open( '/dev/null', append, Error,
            [alias(mauri_tripa)] ).

    Error = '$stream'(3) ? ;

    no
    ?- set_prolog_flag(user_error, mauri_tripa).

    close(mauri_tripa).

     yes
    ?- 
~~~
    We execute three commands. First, we open a stream in write mode and
give it an alias, in this case `mauri_tripa`. Next, we set
user_error to the stream via the alias. Note that after we did so
prompts from the system were redirected to the stream
`mauri_tripa`. Last, we close the stream. At this point, YAP
automatically redirects the user_error alias to the original
`stderr`.

+ `user_flags `

    Define the behaviour of set_prolog_flag/2 if the flag is not known. Values are `silent`, `warning` and `error`. The first two create the flag on-the-fly, with `warning` printing a message. The value `error` is consistent with ISO: it raises an existence error and does not create the flag. See also `create_prolog_flag/3`. The default is`error`, and developers are encouraged to use `create_prolog_flag/3` to create flags for their library.

+ `user_input `

    If the second argument is bound to a stream, set user_input to
this stream. If the second argument is unbound, unify the argument with
the current user_input stream.
By default, the user_input stream is set to a stream
corresponding to the Unix `stdin` stream.

+ `user_output `

    If the second argument is bound to a stream, set user_output to
this stream. If the second argument is unbound, unify the argument with
the current user_output stream.
By default, the user_output stream is set to a stream
corresponding to the Unix `stdout` stream.

+ `verbose `

    If `normal` allow printing of informational and banner messages,
such as the ones that are printed when consulting. If `silent`
disable printing these messages. It is `normal` by default except if
YAP is booted with the `-q` or `-L` flag.

+ `verbose_load `

    If `true` allow printing of informational messages when
consulting files. If `false` disable printing these messages. It
is `normal` by default except if YAP is booted with the `-L`
flag.

+ `version `

    Read-only flag that returns an atom with the current version of
YAP.

+ `version_data `

    Read-only flag that reads a term of the form
`yap`( _Major_, _Minor_, _Patch_, _Undefined_), where
 _Major_ is the major version,  _Minor_ is the minor version,
and  _Patch_ is the patch number.

+ `windows `

    Read-only boolean flag that unifies with tr `true` if YAP is
running on an Windows machine.

+ `write_strings `

    Writable flag telling whether the system should write lists of
integers that are writable character codes using the list notation. It
is `on` if enables or `off` if disabled. The default value for
this flag is `off`.

+ `max_workers `

    Read-only flag telling the maximum number of parallel processes.

+ `max_threads `

    Read-only flag telling the maximum number of Prolog threads that can 
be created.


 
*/
/** @pred  yap_flag(unknown,+ _SPEC_) 

Alternatively, one can use yap_flag/2,
current_prolog_flag/2, or set_prolog_flag/2, to set this
functionality. In this case, the first argument for the built-ins should
be `unknown`, and the second argument should be either
`error`, `warning`, `fail`, or a goal.

 
*/
/** @pred yap_flag(tabling_mode,? _Mode_)
Sets or reads the tabling mode for all tabled predicates. The list of
 _Mode_ options includes:

+ `default`

    Defines that (i) all calls to tabled predicates are evaluated
using the predicate default mode, and that (ii) answers for all
completed calls are obtained by using the predicate default mode.

+ `batched`

    Defines that all calls to tabled predicates are evaluated using
batched scheduling. This option ignores the default tabling mode
of each predicate.

+ `local`

    Defines that all calls to tabled predicates are evaluated using
local scheduling. This option ignores the default tabling mode
of each predicate.

+ `exec_answers`

    Defines that answers for all completed calls are obtained by
executing compiled WAM-like code directly from the trie data
structure. This option ignores the default tabling mode
of each predicate.

+ `load_answers`

    Defines that answers for all completed calls are obtained by
loading them from the trie data structure. This option ignores
the default tabling mode of each predicate.


 
*/
yap_flag(V,Out) :-
	'$user_defined_flag'(V,_,_,_),
	(nonvar(V) ->
	 !
	;
	 true
	),
	'$user_flag_value'(V, Out).

yap_flag(V,Out) :-
	( var(V) ->
	  '$swi_current_prolog_flag'(V, Out)
        ;
	  '$swi_current_prolog_flag'(V, Current)
        ->
	  (var(Out) ->
	      Current = Out
	  ;
	      '$swi_set_prolog_flag'(V, Out)
	  )
        ).
yap_flag(V,Out) :-
	var(V), !,
	'$show_yap_flag_opts'(V,Out).

% do or do not machine code
yap_flag(fast,on) :- set_value('$fast',true).
yap_flag(fast,off) :- !, set_value('$fast',[]).

% hide/unhide atoms
yap_flag(hide,Atom) :- !, hide(Atom).
yap_flag(unhide,Atom) :- !, unhide(Atom).

% control garbage collection
yap_flag(gc,V) :-
	var(V), !,
	( get_value('$gc',[]) -> V = off ; V = on).
yap_flag(gc,on) :- !, set_value('$gc',true).
yap_flag(gc,off) :- !, set_value('$gc',[]).

yap_flag(gc_margin,N) :- 
	( var(N) -> 
	    get_value('$gc_margin',N)
	;
	integer(N), N >=0  ->
	    set_value('$gc_margin',N)
	;
	    '$do_error'(domain_error(flag_value,gc_margin+X),yap_flag(gc_margin,X))
	).
yap_flag(gc_trace,V) :-
	var(V), !,
	get_value('$gc_trace',N1),
	get_value('$gc_verbose',N2),
	get_value('$gc_very_verbose',N3),
	'$yap_flag_show_gc_tracing'(N1, N2, N3, V).
yap_flag(gc_trace,on) :- !,
	set_value('$gc_trace',true),
	set_value('$gc_verbose',[]),
	set_value('$gc_very_verbose',[]).
yap_flag(gc_trace,verbose) :- !,
	set_value('$gc_trace',[]),
	set_value('$gc_verbose',true),
	set_value('$gc_very_verbose',[]).
yap_flag(gc_trace,very_verbose) :- !,
	set_value('$gc_trace',[]),
	set_value('$gc_verbose',true),
	set_value('$gc_very_verbose',true).
yap_flag(gc_trace,off) :-
	set_value('$gc_trace',[]),
	set_value('$gc_verbose',[]),
	set_value('$gc_very_verbose',[]).
yap_flag(syntax_errors, V) :- var(V), !,
	'$get_read_error_handler'(V).
yap_flag(syntax_errors, Option) :-
	'$set_read_error_handler'(Option).
% compatibility flag
yap_flag(enhanced,on) :- !, set_value('$enhanced',true).
yap_flag(enhanced,off) :- set_value('$enhanced',[]).

% compatibility flag
yap_flag(agc_margin,Margin) :-
	'$agc_threshold'(Margin).


%
% show state of $
%
yap_flag(dollar_as_lower_case,V) :-
	var(V), !,
	'$type_of_char'(36,T),
	(T = 3 -> V = on ; V = off).
%
% make $a a legit atom
%
yap_flag(dollar_as_lower_case,on) :- !,
	'$change_type_of_char'(36,3).
%
% force quoting of '$a'
%
yap_flag(dollar_as_lower_case,off) :- 
	'$change_type_of_char'(36,7).

yap_flag(call_counting,X) :- (var(X); X = on; X = off), !,
	'$is_call_counted'(X).

:- set_value('$associate',yap).

yap_flag(associate,X) :-
	var(X), !,
	get_value('$associate',X).
yap_flag(associate,X) :-
	atom(X), !,
	set_value('$associate',X).
yap_flag(associate,X) :-
	'$do_error'(type_error(atom,X),associate(X)).

% do or do not indexation
yap_flag(index,X) :- var(X),
	'$access_yap_flags'(19, X1),
	'$transl_to_index_mode'(X1,X), !.
yap_flag(index,X)  :-
	'$transl_to_index_mode'(X1,X), !,
	'$set_yap_flags'(19,X1).
yap_flag(index,X) :-
	'$do_error'(domain_error(flag_value,index+X),yap_flag(index,X)).

% do or do not indexation
yap_flag(index_sub_term_search_depth,X) :- 
    var(X),
    '$access_yap_flags'(23, X), !.
yap_flag(index_sub_term_search_depth,X)  :-
    integer(X),
    X > 0,
    '$set_yap_flags'(23,X).
yap_flag(index_sub_term_search_depth,X)  :-
    \+ integer(X),
    '$do_error'(type_error(integer,X),yap_flag(index_sub_term_search_depth,X)).
yap_flag(index_sub_term_search_depth,X) :-
    '$do_error'(domain_error(out_of_range,index_sub_term_search_depth+X),yap_flag(index_sub_term_search_depth,X)).

% tabling mode
yap_flag(tabling_mode,Options) :- 
   var(Options), !,
   '$access_yap_flags'(20,Options).
yap_flag(tabling_mode,[]) :- !,
   yap_flag(tabling_mode,default).
yap_flag(tabling_mode,[HOption|TOption]) :- !,
   yap_flag(tabling_mode,TOption),
   yap_flag(tabling_mode,HOption).
yap_flag(tabling_mode,Option) :-
   '$transl_to_yap_flag_tabling_mode'(Flag,Option), !,
   '$set_yap_flags'(20,Flag).
yap_flag(tabling_mode,Options) :-
   '$do_error'(domain_error(flag_value,tabling_mode+Options),yap_flag(tabling_mode,Options)).

yap_flag(informational_messages,X) :- var(X), !,
	 yap_flag(verbose, X).

yap_flag(version,X) :-
	var(X), !,
	get_value('$version_name',X).
yap_flag(version,X) :-
	'$do_error'(permission_error(modify,flag,version),yap_flag(version,X)).

/* ISO Core Revision DTR: new float flags

yap_flag(float_mantissa_digits,X) :-
	var(X), !,
	?????
yap_flag(float_mantissa_digits,X) :-
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,float_mantissa_digits),yap_flag(float_mantissa_digits,X)).
yap_flag(float_mantissa_digits,X) :-
	'$do_error'(domain_error(flag_value,float_mantissa_digits+X),yap_flag(float_mantissa_digits,X)).

yap_flag(float_epsilon,X) :-
	var(X), !,
	?????
yap_flag(float_epsilon,X) :-
	float(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,float_epsilon),yap_flag(float_epsilon,X)).
yap_flag(float_epsilon,X) :-
	'$do_error'(domain_error(flag_value,float_epsilon+X),yap_flag(float_epsilon,X)).

yap_flag(float_min_exponent,X) :-
	var(X), !,
	?????
yap_flag(float_min_exponent,X) :-
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,float_min_exponent),yap_flag(float_min_exponent,X)).
yap_flag(float_epsilon,X) :-
	'$do_error'(domain_error(flag_value,float_min_exponent+X),yap_flag(float_min_exponent,X)).

yap_flag(float_max_exponent,X) :-
	var(X), !,
	?????
yap_flag(float_max_exponent,X) :-
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,float_max_exponent),yap_flag(flo
									     at_max_exponent,X)).
yap_flag(float_max_exponent,X) :-
	'$do_error'(domain_error(flag_value,float_max_exponent+X),yap_flag(float_max_exponent,X)).
*/

yap_flag(n_of_integer_keys_in_db,X) :-
	var(X), !,
	'$resize_int_keys'(X).
yap_flag(n_of_integer_keys_in_db,X) :- integer(X), X > 0, !,
	'$resize_int_keys'(X).
yap_flag(n_of_integer_keys_in_db,X) :-
	'$do_error'(domain_error(flag_value,n_of_integer_keys_in_db+X),yap_flag(n_of_integer_keys_in_db,X)).

yap_flag(n_of_integer_keys_in_bb,X) :-
	var(X), !,
	'$resize_bb_int_keys'(X).
yap_flag(n_of_integer_keys_in_bb,X) :- integer(X), X > 0, !,
	'$resize_bb_int_keys'(X).
yap_flag(n_of_integer_keys_in_bb,X) :-
	'$do_error'(domain_error(flag_value,n_of_integer_keys_in_bb+X),yap_flag(n_of_integer_keys_in_bb,X)).

yap_flag(profiling,X) :- (var(X); X = on; X = off), !,
	'$is_profiled'(X).

yap_flag(strict_iso,OUT) :-
	var(OUT), !,
	'$access_yap_flags'(9,X),
	'$transl_to_on_off'(X,OUT).
yap_flag(strict_iso,on) :- !,
	yap_flag(language,iso),
	'$transl_to_on_off'(X,on),
	'$set_yap_flags'(9,X).
yap_flag(strict_iso,off) :- !,
	'$transl_to_on_off'(X,off),
	'$set_yap_flags'(9,X).
yap_flag(strict_iso,X) :-
	'$do_error'(domain_error(flag_value,strict_iso+X),yap_flag(strict_iso,X)).

yap_flag(variable_names_may_end_with_quotes,OUT) :-
	var(OUT), !,
	'$access_yap_flags'(21,X),
	'$transl_to_on_off'(X,OUT).
yap_flag(variable_names_may_end_with_quotes,on) :- !,
	'$transl_to_on_off'(X,on),
	'$set_yap_flags'(21,X).
yap_flag(variable_names_may_end_with_quotes,off) :- !,
	'$transl_to_on_off'(X,off),
	'$set_yap_flags'(21,X).
yap_flag(variable_names_may_end_with_quotes,X) :-
	'$do_error'(domain_error(flag_value,strict_iso+X),yap_flag(strict_iso,X)).

yap_flag(language,X) :-
	var(X), !,
	'$access_yap_flags'(8, X1),
	'$trans_to_lang_flag'(X1,X).
yap_flag(language,X) :-
	'$trans_to_lang_flag'(N,X), !,
	'$set_yap_flags'(8,N),
	'$adjust_language'(X).
yap_flag(language,X) :-
	'$do_error'(domain_error(flag_value,language+X),yap_flag(language,X)).

yap_flag(discontiguous_warnings,X) :-
	var(X), !,
	style_check(?(Disc)),
	( Disc = +discontiguous,
	  X = on
	;
	  Disc = -discontiguous,
	  X = off
	), !.
yap_flag(discontiguous_warnings,X) :-
	'$transl_to_on_off'(_,X), !,
	(X == on -> 
	    style_check(discontiguous)
	;
	    style_check(-discontiguous)
	).
yap_flag(discontiguous_warnings,X) :-
	'$do_error'(domain_error(flag_value,discontiguous_warnings+X),yap_flag(discontiguous_warnings,X)).

yap_flag(redefine_warnings,X) :-
	var(X), !,
	style_check(?(Disc)),
	( Disc = +multiple,
	  X = on
	;
	  Disc = -multiple,
	  X = off
	), !.
yap_flag(redefine_warnings,X) :-
	'$transl_to_on_off'(_,X), !,
	(X == on -> 
	    style_check(multiple)
	;
	    style_check(-multiple)
	).
yap_flag(redefine_warnings,X) :-
	'$do_error'(domain_error(flag_value,redefine_warnings+X),yap_flag(redefine_warnings,X)).

yap_flag(chr_toplevel_show_store,X) :-
	var(X), !,
	'$nb_getval'('$chr_toplevel_show_store', X, fail).
yap_flag(chr_toplevel_show_store,X) :-
	(X = true ; X = false), !,
	nb_setval('$chr_toplevel_show_store',X).
yap_flag(chr_toplevel_show_store,X) :-
	'$do_error'(domain_error(flag_value,chr_toplevel_show_store+X),yap_flag(chr_toplevel_show_store,X)).

yap_flag(qcompile,X) :-
	var(X), !,
	'$nb_getval'('$qcompile', X, X=never).
yap_flag(qcompile,X) :-
	(X == never ; X == auto ; X == large ; X == part), !,
	nb_setval('$qcompile',X).
yap_flag(qcompile,X) :-
	'$do_error'(domain_error(flag_value,qcompile+X),yap_flag(qcompile,X)).

yap_flag(source,X) :-
	var(X), !,
	source_mode( X, X ).
yap_flag(source,X) :-
	(X == off -> true ; X == on), !,
	source_mode( _, X ).
yap_flag(source,X) :-
	'$do_error'(domain_error(flag_value,source+X),yap_flag(source,X)).

yap_flag(open_expands_filename,Expand) :-
	var(Expand), !,
	'$default_expand'(Expand).
yap_flag(open_expands_filename,Expand) :-
	'$set_default_expand'(Expand).

yap_flag(single_var_warnings,X) :-
	var(X), !,
	style_check(?(Disc)),
	( Disc = +singletons,
	  X = on
	;
	  Disc = -singletons,
	  X = off
	), !.
yap_flag(single_var_warnings,X) :-
	'$transl_to_on_off'(_,X), !,
	(X == on ->
	     style_check(single_var)
	;
	     style_check(-single_var)
	).
yap_flag(single_var_warnings,X) :-
	'$do_error'(domain_error(flag_value,single_var_warnings+X),yap_flag(single_var_warnings,X)).

yap_flag(system_options,X) :-
	'$system_options'(X).
	
yap_flag(update_semantics,X) :-
	var(X), !,
	( '$log_upd'(I) -> '$convert_upd_sem'(I,X) ).
yap_flag(update_semantics,logical) :- !,
	'$switch_log_upd'(1).
yap_flag(update_semantics,logical_assert) :- !,
	'$switch_log_upd'(2).
yap_flag(update_semantics,immediate) :- !,
	'$switch_log_upd'(0).
yap_flag(update_semantics,X) :-
	'$do_error'(domain_error(flag_value,update_semantics+X),yap_flag(update_semantics,X)).

yap_flag(toplevel_hook,G) :-
	var(G), !,
	( recorded('$toplevel_hooks',G,_) -> G ; G = fail ).
yap_flag(toplevel_hook,G) :- !,
	'$set_toplevel_hook'(G).

yap_flag(typein_module,X) :-
	var(X), !,
	'$current_module'(X).
yap_flag(typein_module,X) :-
	module(X).

yap_flag(write_strings,OUT) :-
	var(OUT), !,
	'$access_yap_flags'(13,X),
	'$transl_to_on_off'(X,OUT).
yap_flag(write_strings,on) :- !,
	'$transl_to_on_off'(X,on),
	'$set_yap_flags'(13,X).
yap_flag(write_strings,off) :- !,
	'$transl_to_on_off'(X,off),
	'$set_yap_flags'(13,X).
yap_flag(write_strings,X) :-
	'$do_error'(domain_error(flag_value,write_strings+X),yap_flag(write_strings,X)).

yap_flag(arithmetic_exceptions,OUT) :-
	var(OUT), !,
	'$access_yap_flags'(12,X),
	'$transl_to_true_false'(X,OUT).
yap_flag(arithmetic_exceptions,true) :- !,
	'$transl_to_true_false'(X,true),
	'$set_yap_flags'(12,X).
yap_flag(arithmetic_exceptions,false) :- !,
	'$transl_to_true_false'(X,false),
	'$set_yap_flags'(12,X).
yap_flag(arithmetic_exceptions,X) :-
	'$do_error'(domain_error(flag_value,arithmetic_exceptions+X),yap_flag(arithmetic_exceptions,[true,false])).

yap_flag(prompt_alternatives_on,OUT) :-
	var(OUT), !,
	'$prompt_alternatives_on'(OUT).
yap_flag(prompt_alternatives_on,determinism) :- !,
	'$purge_clauses'('$prompt_alternatives_on'(_),prolog),
	'$compile'('$prompt_alternatives_on'(determinism),0,'$prompt_alternatives_on'(determinism),prolog).
yap_flag(prompt_alternatives_on,groundness) :- !,
	'$purge_clauses'('$prompt_alternatives_on'(_),prolog),
	'$compile'('$prompt_alternatives_on'(groundness),0,'$prompt_alternatives_on'(groundness),prolog).
yap_flag(prompt_alternatives_on,X) :-
	'$do_error'(domain_error(flag_value,prompt_alternatives_on+X),yap_flag(prompt_alternatives_on,X)).

yap_flag(stack_dump_on_error,OUT) :-
	var(OUT), !,
	'$access_yap_flags'(17,X),
	'$transl_to_on_off'(X,OUT).
yap_flag(stack_dump_on_error,on) :- !,
'$transl_to_on_off'(X,on),
	'$set_yap_flags'(17,X).
yap_flag(stack_dump_on_error,off) :- !,
	'$transl_to_on_off'(X,off),
	'$set_yap_flags'(17,X).
yap_flag(stack_dump_on_error,X) :-
	'$do_error'(domain_error(flag_value,stack_dump_on_error+X),yap_flag(stack_dump_on_error,X)).

yap_flag(user_input,OUT) :-
	var(OUT), !,
	stream_property(OUT,alias(user_input)).
yap_flag(user_input,Stream) :-
	set_stream(Stream, alias(user_input)).

yap_flag(user_output,OUT) :-
	var(OUT), !,
	stream_property(OUT,alias(user_output)).
yap_flag(user_output,Stream) :-
	set_stream(Stream, alias(user_output)).

yap_flag(user_error,OUT) :-
	var(OUT), !,
	stream_property(OUT,alias(user_error)).
yap_flag(user_error,Stream) :-
	set_stream(Stream, alias(user_error)).

yap_flag(debugger_print_options,OUT) :-
	var(OUT),
	recorded('$print_options','$debugger'(OUT),_), !.
yap_flag(debugger_print_options,Opts) :-
	recorda('$print_options','$debugger'(Opts),_).

:- recorda('$print_options','$debugger'([quoted(true),numbervars(true),portrayed(true),max_depth(10)]),_).

yap_flag(toplevel_print_options,OUT) :-
	var(OUT),
	recorded('$print_options','$toplevel'(OUT),_), !.
yap_flag(toplevel_print_options,Opts) :-
	recorda('$print_options','$toplevel'(Opts),_).

:- recorda('$print_options','$toplevel'([quoted(true),numbervars(true),portrayed(true)]),_).

yap_flag(host_type,X) :-
	'$host_type'(X).

yap_flag(host_type,X) :-
	'$host_type'(X).

yap_flag(argv,X) :-
	'$argv'(X).

yap_flag(os_argv,X) :-
    '$os_argv'(X).

yap_flag(float_format,X) :-
	var(X), !,
	'$float_format'(X).
yap_flag(float_format,X) :-
	atom(X), !,
	'$float_format'(X).
yap_flag(float_format,X) :-
	'$do_error'(type_error(atom,X),yap_flag(float_format,X)).

yap_flag(max_workers,X) :-
	var(X), !,
	'$max_workers'(X).
yap_flag(max_workers,X) :-
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,max_workers),yap_flag(max_workers,X)).
yap_flag(max_workers,X) :-
	'$do_error'(domain_error(flag_value,max_workers+X),yap_flag(max_workers,X)).

yap_flag(max_threads,X) :-
	var(X), !,
	'$max_threads'(X).
yap_flag(max_threads,X) :-
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,max_threads),yap_flag(max_threads,X)).
yap_flag(max_threads,X) :-
	'$do_error'(domain_error(flag_value,max_threads+X),yap_flag(max_threads,X)).

% should match definitions in Yap.h
'$transl_to_index_mode'(0, off).
'$transl_to_index_mode'(1, single).
'$transl_to_index_mode'(2, compact).
'$transl_to_index_mode'(3, multi).
'$transl_to_index_mode'(3, on). % default is multi argument indexing
'$transl_to_index_mode'(4, max).


% should match with code in stdpreds.c
'$transl_to_yap_flag_tabling_mode'(0,default).
'$transl_to_yap_flag_tabling_mode'(1,batched).
'$transl_to_yap_flag_tabling_mode'(2,local).
'$transl_to_yap_flag_tabling_mode'(3,exec_answers).
'$transl_to_yap_flag_tabling_mode'(4,load_answers).
'$transl_to_yap_flag_tabling_mode'(5,local_trie).
'$transl_to_yap_flag_tabling_mode'(6,global_trie).
'$transl_to_yap_flag_tabling_mode'(7,coinductive).

'$system_options'(big_numbers) :-
	'$has_bignums'.
'$system_options'(coroutining) :-
	'$yap_has_coroutining'.
'$system_options'(depth_limit) :-
	\+ '$undefined'(get_depth_limit(_), prolog).
'$system_options'(low_level_tracer) :-
	\+ '$undefined'(start_low_level_trace, prolog).
'$system_options'(or_parallelism) :-
	\+ '$undefined'('$c_yapor_start', prolog).
'$system_options'(rational_trees) :-
	'$yap_has_rational_trees'.
'$system_options'(readline) :-
	'$swi_current_prolog_flag'(readline, true).
'$system_options'(tabling) :-
	\+ '$undefined'('$c_table'(_,_,_), prolog).
'$system_options'(threads) :-
	\+ '$undefined'('$thread_join'(_), prolog).
'$system_options'(wam_profiler) :-
	\+ '$undefined'(reset_op_counters, prolog).

'$yap_system_flag'(agc_margin).
'$yap_system_flag'(arithmetic_exceptions).
'$yap_system_flag'(argv).
'$yap_system_flag'(chr_toplevel_show_store).
'$yap_system_flag'(debugger_print_options).
'$yap_system_flag'(discontiguous_warnings).
'$yap_system_flag'(dollar_as_lower_case).
%		V = fast  ;
% '$yap_system_flag'(file_name_variables).
% '$yap_system_flag'(fileerrors ).
'$yap_system_flag'(float_format).
%		V = float_mantissa_digits ;
%		V = float_epsilon ;
%		V = float_min_exponent ;
%		V = float_max_exponent ;
'$yap_system_flag'(gc   ).
'$yap_system_flag'(gc_margin   ).
'$yap_system_flag'(gc_trace    ).
%	    V = hide  ;
'$yap_system_flag'(host_type ).
'$yap_system_flag'(index).
'$yap_system_flag'(index_sub_term_search_depth).
'$yap_system_flag'(tabling_mode).
'$yap_system_flag'(informational_messages).
'$yap_system_flag'(language).
'$yap_system_flag'(max_workers).
'$yap_system_flag'(max_threads).
'$yap_system_flag'(n_of_integer_keys_in_db).
'$yap_system_flag'(open_expands_filename).
'$yap_system_flag'(os_argv).
'$yap_system_flag'(profiling).
'$yap_system_flag'(prompt_alternatives_on).
'$yap_system_flag'(redefine_warnings).
'$yap_system_flag'(single_var_warnings).
'$yap_system_flag'(source).
'$yap_system_flag'(stack_dump_on_error).
'$yap_system_flag'(strict_iso).
'$yap_system_flag'(syntax_errors).
'$yap_system_flag'(system_options).
'$yap_system_flag'(toplevel_hook).
'$yap_system_flag'(toplevel_print_options).
'$yap_system_flag'(typein_module).
'$yap_system_flag'(update_semantics).
'$yap_system_flag'(user_error).
'$yap_system_flag'(user_input).
'$yap_system_flag'(user_output).
'$yap_system_flag'(variable_names_may_end_with_quotes).
'$yap_system_flag'(version).
'$yap_system_flag'(write_strings).

'$show_yap_flag_opts'(V,Out) :-
	'$yap_system_flag'(V),
	yap_flag(V, Out).

'$trans_to_lang_flag'(0,cprolog).
'$trans_to_lang_flag'(1,iso).
'$trans_to_lang_flag'(2,sicstus).

'$adjust_language'(cprolog) :-
%	'$switch_log_upd'(0),
	'$syntax_check_mode'(_,off),
	'$syntax_check_single_var'(_,off),
	'$syntax_check_discontiguous'(_,off),
	'$syntax_check_multiple'(_,off),
	'$swi_set_prolog_flag'(character_escapes, false), % disable character escapes.
	'$set_yap_flags'(14,1),
	'$set_fpu_exceptions'(true),
	unknown(_,fail).
'$adjust_language'(sicstus) :-
	'$switch_log_upd'(1),
	leash(full),
	'$syntax_check_mode'(_,on),
	'$syntax_check_single_var'(_,on),
	'$syntax_check_discontiguous'(_,on),
	'$syntax_check_multiple'(_,on),
	'$transl_to_on_off'(X1,on),
	'$set_yap_flags'(5,X1),
	'$force_char_conversion',
	'$set_yap_flags'(14,0),
	% CHARACTER_ESCAPE
	'$swi_set_prolog_flag'(character_escapes, true), % disable character escapes.
	'$set_fpu_exceptions'(true),
	'$swi_set_prolog_flag'(fileerrors, true),
	unknown(_,error).
'$adjust_language'(iso) :-
	'$switch_log_upd'(1),
	style_check(all),
	fileerrors,
	'$transl_to_on_off'(X1,on),
	% CHAR_CONVERSION
	'$set_yap_flags'(5,X1),
	'$force_char_conversion',
	% ALLOW_ASSERTING_STATIC
	'$set_yap_flags'(14,0),
	% CHARACTER_ESCAPE
	'$swi_set_prolog_flag'(character_escapes, true), % disable character escapes.
	'$set_fpu_exceptions'(true),
	unknown(_,error).


'$convert_upd_sem'(0,immediate).
'$convert_upd_sem'(1,logical).
'$convert_upd_sem'(2,logical_assert).

'$transl_to_true_false'(0,false).
'$transl_to_true_false'(1,true).

'$transl_to_on_off'(0,off).
'$transl_to_on_off'(1,on).

'$transl_to_rounding_function'(0,toward_zero).
'$transl_to_rounding_function'(1,down).

'$yap_flag_show_gc_tracing'(true, _, _, on) :- !.
'$yap_flag_show_gc_tracing'(_, true, _, verbose) :- !.
'$yap_flag_show_gc_tracing'(_, _, on, very_verbose) :- !.
'$yap_flag_show_gc_tracing'(_, _, _, off).

'$flag_check_alias'(OUT, Alias) :-
	stream_property(OUT,alias(Alias)), !.
	
/** @pred current_prolog_flag(? _Flag_,- _Value_) is iso 

Obtain the value for a YAP Prolog flag. Equivalent to calling
yap_flag/2 with the second argument unbound, and unifying the
returned second argument with  _Value_.

 
*/
current_prolog_flag(V,Out) :-
	var(V), !,
	yap_flag(V,NOut),
	NOut = Out.
current_prolog_flag(V,Out) :-
	atom(V), !,
	yap_flag(V,NOut),
	NOut = Out.
current_prolog_flag(M:V,Out) :-
	current_module(M), atom(V), !,
	yap_flag(M:V,NOut),
	NOut = Out.
current_prolog_flag(V,Out) :-
	'$do_error'(type_error(atom,V),current_prolog_flag(V,Out)).

/** @pred set_prolog_flag(+ _Flag_,+ _Value_) is iso 



Set the value for YAP Prolog flag `Flag`. Equivalent to
calling yap_flag/2 with both arguments bound.

 
*/
set_prolog_flag(F,V) :-
	var(F), !,
	'$do_error'(instantiation_error,set_prolog_flag(F,V)).
set_prolog_flag(F,V) :-
	var(V), !,
	'$do_error'(instantiation_error,set_prolog_flag(F,V)).
set_prolog_flag(M:V,Out) :-
	current_module(M), atom(V), !,
	'$swi_set_prolog_flag'(M:V,Out).
set_prolog_flag(F,V) :-
	\+ atom(F), !,
	'$do_error'(type_error(atom,F),set_prolog_flag(F,V)).
set_prolog_flag(F, Val) :-
	'$swi_current_prolog_flag'(F, _), !,
	'$swi_set_prolog_flag'(F, Val).
set_prolog_flag(F,V) :-
	'$yap_system_flag'(F), !,
	yap_flag(F,V).
set_prolog_flag(F,V) :-
	'$swi_current_prolog_flag'(user_flags, UFlag),
	(
	 UFlag = silent ->
	 create_prolog_flag(F, V, [])
	;
	 UFlag = warning ->
	 print_message(warning,existence_error(prolog_flag, F)),
	 create_prolog_flag(F, V, [])
	;
	 UFlag = error ->
	 '$do_error'(existence_error(prolog_flag, F),set_prolog_flag(F,V))
	).

/** @pred prolog_flag(? _Flag_,- _OldValue_,+ _NewValue_) 



Obtain the value for a YAP Prolog flag and then set it to a new
value. Equivalent to first calling current_prolog_flag/2 with the
second argument  _OldValue_ unbound and then calling
set_prolog_flag/2 with the third argument  _NewValue_.

 
*/
prolog_flag(F, Old, New) :-
	var(F), !,
	'$do_error'(instantiation_error,prolog_flag(F,Old,New)).
prolog_flag(F, Old, New) :-
	current_prolog_flag(F, Old),
	set_prolog_flag(F, New).

prolog_flag(F, Old) :-
	current_prolog_flag(F, Old).

/** @pred create_prolog_flag(+ _Flag_,+ _Value_,+ _Options_) 



Create a new YAP Prolog flag.  _Options_ include `type(+Type)` and `access(+Access)` with  _Access_
one of `read_only` or `read_write` and  _Type_ one of `boolean`, `integer`, `float`, `atom`
and `term` (that is, no type).

 
*/
create_prolog_flag(Name, Value, Options) :-
	'$check_flag_name'(Name, create_prolog_flag(Name, Value, Options)),
	'$check_flag_options'(Options, Domain, RW, create_prolog_flag(Name, Value, Options)),
	'$check_flag_value'(Value, Domain, create_prolog_flag(Name, Value, Options)),
	retractall(prolog:'$user_defined_flag'(Name,_,_,_)),
	assert(prolog:'$user_defined_flag'(Name,Domain,RW,Value)).

'$check_flag_name'(V, G) :-
	var(V),
	'$do_error'(instantiation_error,G).
'$check_flag_name'(Name, _) :-
	atom(Name), !.
'$check_flag_name'(Name, G) :-
	'$do_error'(type_error(atom,Name),G).
	
'$check_flag_options'(O, _, _, G) :-
	var(O),
	'$do_error'(instantiation_error,G).
'$check_flag_options'([], _, read_write, _) :- !.
'$check_flag_options'([O1|Os], Domain, RW, G) :- !,
	'$check_flag_optionsl'([O1|Os], Domain, RW, G).
'$check_flag_options'(O, _, _, G) :-
	'$do_error'(type_error(list,O),G).


'$check_flag_optionsl'([], _, read_write, _G).
'$check_flag_optionsl'([V|_Os], _Domain, _RW, G) :-
	var(V),
	'$do_error'(instantiation_error,G).
'$check_flag_optionsl'([type(Type)|Os], Domain, RW, G) :- !,
	'$check_flag_type'(Type, Domain, G),
	'$check_flag_optionsl'(Os, _, RW, G).
'$check_flag_optionsl'([access(Access)|Os], Domain, RW, G) :- !,
	'$check_flag_access'(Access, RW, G),
	'$check_flag_optionsl'(Os, Domain, _, G).
'$check_flag_optionsl'(Os, _Domain, _RW, G) :-
	'$do_error'(domain_error(create_prolog_flag,Os),G).

'$check_flag_type'(V, _, G) :-
	var(V),
	'$do_error'(instantiation_error,G).
'$check_flag_type'(boolean, boolean, _) :- !.
'$check_flag_type'(integer, integer, _) :- !.
'$check_flag_type'(float, float, _) :- !.
'$check_flag_type'(atom, atom, _) :- !.
'$check_flag_type'(term, term, _) :- !.
'$check_flag_type'(Atom, _, G) :-
	'$do_error'(domain_error(create_prolog_flag_option(type),Atom),G).

'$check_flag_access'(V, _, G) :-
	var(V),
	'$do_error'(instantiation_error,G).
'$check_flag_access'(read_write, read_write, _) :- !.
'$check_flag_access'(read_only, read_only, _) :- !.
'$check_flag_access'(Atom, _, G) :-
	'$do_error'(domain_error(create_prolog_flag_option(access),Atom),G).

'$user_flag_value'(F, Val) :-
	var(Val), !,
	'$user_defined_flag'(F,_,_,Val).
'$user_flag_value'(F, Val) :-
	atomic(Val), !,
	prolog:'$user_defined_flag'(F,Domain,RW,V0),
	(
	 Val == V0
	->
	 true
	;
	 RW = read_only
	->
	 '$do_error'(permission_error(modify,flag,F),yap_flag(F,Val))
	;
	  '$check_flag_value'(Val, Domain, yap_flag(F,Val)),
	  retractall(prolog:'$user_defined_flag'(F,_,_,_)),
	  assert(prolog:'$user_defined_flag'(F,Domain,RW,Val))
	).
'$user_flag_value'(F, Val) :-
	'$do_error'(type_error(atomic,Val),yap_flag(F,Val)).

'$check_flag_value'(Value, _, G) :-
	\+ ground(Value), !,
	'$do_error'(instantiation_error,G).
'$check_flag_value'(Value, Domain, _G) :-
	var(Domain), !,
	'$flag_domain_from_value'(Value, Domain).
'$check_flag_value'(_, term, _) :- !.
'$check_flag_value'(Value, atom, _) :-
	atom(Value), !.
'$check_flag_value'(Value, integer, _) :-
	integer(Value), !.
'$check_flag_value'(Value, float, _) :-
	float(Value), !.
'$check_flag_value'(true, boolean, _) :- !.
'$check_flag_value'(false, boolean, _) :- !.
'$check_flag_value'(Value, Domain, G) :-
	'$do_error'(domain_error(Domain,Value),G).

'$flag_domain_from_value'(true, boolean) :- !.
'$flag_domain_from_value'(false, boolean) :- !.
'$flag_domain_from_value'(Value, integer) :- integer(Value), !.
'$flag_domain_from_value'(Value, float) :- float(Value), !.
'$flag_domain_from_value'(Value, atom) :- atom(Value), !.
'$flag_domain_from_value'(_, term).


/** 
   @pred source_mode(- _O_,+ _N_) 

The state of source mode can either be on or off. When the source mode
is on, all clauses are kept both as compiled code and in a "hidden"
database.  _O_ is unified with the previous state and the mode is set
according to  _N_.

*/


% if source_mode is on, then the source for the predicates
% is stored with the code
source_mode(Old,New) :-
	'$access_yap_flags'(11,X),
	'$transl_to_on_off'(X,Old),
	'$transl_to_on_off'(XN,New),
	'$set_yap_flags'(11,XN).

/**   @pred source

After executing this goal, YAP keeps information on the source
of the predicates that will be consulted. This enables the use of
[listing/0](@ref listing), `listing/1` and [clause/2](@ref clause) for those
clauses.

The same as `source_mode(_,on)` or as declaring all newly defined
static procedures as `public`.
*/
source :- '$set_yap_flags'(11,1).


/** @pred no_source
 The opposite to `source`.

The same as `source_mode(_,off)`.

*/
no_source :- '$set_yap_flags'(11,0).

/**
@}
*/
