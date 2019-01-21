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
* File:		load_foreign.yap					 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	Utility predicates for load_foreign			 *
*									 *
*************************************************************************/

/**
 * @file load_foreign.yap
 *
 * @brief load predicates written in C (also C++, Java, Python, R)
 */
:- system_module( '$_load_foreign', [load_foreign_files/3,
        open_shared_object/2,
        open_shared_object/3], ['$import_foreign'/3]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_modules', ['$do_import'/3]).



/**

@defgroup LoadForeign Access to Foreign Language Programs
@ingroup fli_c_cxx

@{

*/

/** @pred load_foreign_files( _Files_, _Libs_, _InitRoutine_)

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

+ YAPLIBDIR

if defined, or in the default library.
available as
YAP supports the SWI-Prolog interface to loading foreign code, the shlib package.

*/


load_foreign_files(_Objs,_Libs,Entry) :-
    '$check_embedded'(Entry),
    !.
load_foreign_files(Objs,Libs,Entry) :-
    source_module(M),
    %G = load_foreign_files(Objs,Libs,Entry),
    '$absfs'( Objs, [file_type(executable),
			     access(read),
			     expand(true),
			     file_errors(fail)], NewObjs),
    '$load_libs'( Libs ),
   '$load_foreign_files'(NewObjs,[],Entry),
    !,
    prolog_load_context(file, F),
    ignore( recordzifnot( '$load_foreign_done', [F, M], _) ).

'$absfs'([],_P,[]).
'$absfs'([F|Fs],P,[NF|NFs]) :-
    '$name_object'(F, P, NF),
    !,
    '$absfs'(Fs,P,NFs).
'$absfs'([F|Fs],P,[F|NFs]) :-
    '$absfs'(Fs,P,NFs).

'$name_object'(I, P, O) :-
    atom(I),
    !,
    absolute_file_name(foreign(I), O, P).
'$name_object'(I, P, O) :-
    absolute_file_name(I, O, P).

'$load_libs'([]).
'$load_libs'([File|Files]) :-
    open_shared_object(File, _Handle),
    '$load_libs'(Files).

/** @pred load_absolute_foreign_files( Files, Libs, InitRoutine)

Loads object files produced by the C compiler. It is useful when no search should be performed and instead one has the full paths to the _Files_ and _Libs_.

*/
load_absolute_foreign_files(Objs,Libs,Entry) :-
    source_module(M),
   '$load_foreign_files'(Objs,Libs,Entry),
    !,
    prolog_load_context(file, F),
    ignore( recordzifnot( '$load_foreign_done', [F, M], _) ).

'$checklib_prefix'(F,F) :- is_absolute_file_name(F), !.
'$checklib_prefix'(F, F) :-
	sub_atom(F, 0, _, _, lib), !.
'$checklib_prefix'(F, Lib) :-
	atom_concat(lib, F, Lib).

'$import_foreign'(F, M0, M) :-
    M \= M0,
    predicate_property(M0:P,built_in),
    predicate_property(M0:P,file(F)),
    functor(P, N, K),
    '$do_import'(N/K-N/K, M0, M),
    fail.
'$import_foreign'(_F, _M0, _M).

/** @pred open_shared_object(+ _File_, - _Handle_)

File is the name of a shared object file (called dynamic load
library in MS-Windows). This file is attached to the current process
and  _Handle_ is unified with a handle to the library. Equivalent to
`open_shared_object(File, [], Handle)`. See also
load_foreign_library/1 and `load_foreign_library/2`.

On errors, an exception `shared_object`( _Action_,
 _Message_) is raised.  _Message_ is the return value from
dlerror().


*/
open_shared_object(File, Handle) :-
	open_shared_object(File, [], Handle).

/** @pred open_shared_object(+ _File_, - _Handle_, + _Options_)

As `open_shared_object/2`, but allows for additional flags to
be passed.  _Options_ is a list of atoms. `now` implies the
symbols are
resolved immediately rather than lazily (default). `global` implies
symbols of the loaded object are visible while loading other shared
objects (by default they are local). Note that these flags may not
be supported by your operating system. Check the documentation of
`dlopen()` or equivalent on your operating system. Unsupported
flags  are silently ignored.


*/
open_shared_object(File, Opts, Handle) :-
	'$open_shared_opts'(Opts, open_shared_object(File, Opts, Handle), OptsI),
	'$open_shared_object'(File, OptsI, Handle),
	prolog_load_context(module, M),
	ignore( recordzifnot( '$foreign', M:'$swi_foreign'(File,Opts, Handle), _) ).

'$open_shared_opts'(Opts, G, _OptsI) :-
	var(Opts), !,
	'$do_error'(instantiation_error,G).
'$open_shared_opts'([], _, 0) :- !.
'$open_shared_opts'([Opt|Opts], G, V) :-
	'$open_shared_opts'(Opts, G, V0),
	'$open_shared_opt'(Opt, G, OptV),
	V0 is V \/ OptV.

'$open_shared_opt'(Opt, G, _) :-
	var(Opt), !,
	'$do_error'(instantiation_error,G).
'$open_shared_opt'(now, __, 1) :- !.
'$open_shared_opt'(global, __, 2) :- !.
'$open_shared_opt'(Opt, Goal, _) :-
	'$do_error'(domain_error(open_shared_object_option,Opt),Goal).

/** @pred call_shared_object_function(+ _Handle_, + _Function_)

Call the named function in the loaded shared library. The function is
called without arguments and the return-value is ignored. YAP supports
installing foreign language predicates using calls to 'UserCCall()`,
`PL_register_foreign()`, and friends.

 */

call_shared_object_function( Handle, Function) :-
    '$call_shared_object_function'( Handle, Function),
    prolog_load_context(module, M),
    ignore( recordzifnot( '$foreign', M:'$swi_foreign'( Handle, Function ), _) ).
%% @}

/** @pred $slave is det

Called at boot-time when Prolog is run from another language (eg, Java, Python, Android)
*/

'$slave' :-
    getenv( '__PYVENV_LAUNCHER__', _ ),
    use_module( library(python) ).
