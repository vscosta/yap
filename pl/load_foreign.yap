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

/** @defgroup LoadForeign Access to Foreign Language Programs
@ingroup YAPBuiltins
@{

*/


:- system_module( '$_load_foreign', [load_foreign_files/3,
        open_shared_object/2,
        open_shared_object/3], ['$import_foreign'/3]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_modules', ['$do_import'/3]).

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

YAP also supports the SWI-Prolog interface to loading foreign code:

 
*/
load_foreign_files(Objs,Libs,Entry) :-
    '$check_objs_for_load_foreign_files'(Objs,NewObjs,load_foreign_files(Objs,Libs,Entry)),
    '$check_libs_for_load_foreign_files'(Libs,NewLibs,load_foreign_files(Objs,Libs,Entry)),
    '$check_entry_for_load_foreign_files'(Entry,load_foreign_files(Objs,Libs,Entry)),
    ( 
	recordzifnot( '$foreign', M:'$foreign'(Objs,Libs,Entry), _)
	->
	'$load_foreign_files'(NewObjs,NewLibs,Entry),
        (
	    prolog_load_context(file, F)
	->
	    ignore( recordzifnot( '$load_foreign_done', [F, M], _) )
	;
	    true
        )
	;
	true 
   ),
   !.

'$check_objs_for_load_foreign_files'(V,_,G) :- var(V), !,
	'$do_error'(instantiation_error,G).
'$check_objs_for_load_foreign_files'([],[],_) :- !.
'$check_objs_for_load_foreign_files'([Obj|Objs],[NObj|NewObjs],G) :- !,
	'$check_obj_for_load_foreign_files'(Obj,NObj,G),
	'$check_objs_for_load_foreign_files'(Objs,NewObjs,G).
'$check_objs_for_load_foreign_files'(Objs,_,G) :-
	'$do_error'(type_error(list,Objs),G).

'$check_obj_for_load_foreign_files'(V,_,G) :- var(V), !,
	'$do_error'(instantiation_error,G).
'$check_obj_for_load_foreign_files'(Obj,NewObj,_) :- atom(Obj), !,
        ( atom(Obj), Obj1 = foreign(Obj) ; Obj1 = Obj ),
	absolute_file_name(foreign(Obj),[file_type(executable),
					 access(read),
					 expand(true),
					 file_errors(fail)
					], NewObj).
'$check_obj_for_load_foreign_files'(Obj,_,G) :-
	'$do_error'(type_error(atom,Obj),G).

'$check_libs_for_load_foreign_files'(V,_,G) :- var(V), !,
	'$do_error'(instantiation_error,G).
'$check_libs_for_load_foreign_files'([],[],_) :- !.
'$check_libs_for_load_foreign_files'([Lib|Libs],[NLib|NLibs],G) :- !,
	'$check_lib_for_load_foreign_files'(Lib,NLib,G),
	'$check_libs_for_load_foreign_files'(Libs,NLibs,G).
'$check_libs_for_load_foreign_files'(Libs,_,G) :-
	'$do_error'(type_error(list,Libs),G).

'$check_lib_for_load_foreign_files'(V,_,G) :- var(V), !,
	'$do_error'(instantiation_error,G).
'$check_lib_for_load_foreign_files'(Lib,NLib,_) :- atom(Lib), !,
	'$process_obj_suffix'(Lib,NewLib),
	'$checklib_prefix'(NewLib,NLib).
'$check_lib_for_load_foreign_files'(Lib,_,G) :-
	'$do_error'(type_error(atom,Lib),G).

'$process_obj_suffix'(Obj,Obj) :-
	'$swi_current_prolog_flag'(shared_object_extension, ObjSuffix),
	sub_atom(Obj, _, _, 0, ObjSuffix), !.
'$process_obj_suffix'(Obj,NewObj) :-
	'$swi_current_prolog_flag'(shared_object_extension, ObjSuffix),
	atom_concat([Obj,'.',ObjSuffix],NewObj).

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

'$check_entry_for_load_foreign_files'(V,G) :- var(V), !,
	'$do_error'(instantiation_error,G).
'$check_entry_for_load_foreign_files'(Entry,_) :- atom(Entry), !.
'$check_entry_for_load_foreign_files'(Entry,G) :-
	'$do_error'(type_error(atom,Entry),G).

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
%%! @}

