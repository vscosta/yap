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

load_foreign_files(_Objs,_Libs,_Entry) :-
    prolog_load_context(file, F),
    prolog_load_context(module, M),
    recorded( '$load_foreign_done', [F, M0], _), !,
    '$import_foreign'(F, M0, M).
load_foreign_files(Objs,Libs,Entry) :-
	'$check_objs_for_load_foreign_files'(Objs,NewObjs,load_foreign_files(Objs,Libs,Entry)),
	'$check_libs_for_load_foreign_files'(Libs,NewLibs,load_foreign_files(Objs,Libs,Entry)),
	'$check_entry_for_load_foreign_files'(Entry,load_foreign_files(Objs,Libs,Entry)),
	'$load_foreign_files'(NewObjs,NewLibs,Entry),
	prolog_load_context(file, F),
	prolog_load_context(module, M),
	ignore( recordzifnot( '$load_foreign_done', [F, M], _) ), !.

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
	'$process_obj_suffix'(Obj,NewObj).
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

open_shared_object(File, Handle) :-
	'$open_shared_object'(File, 0, Handle).

open_shared_object(File, Opts, Handle) :-
	'$open_shared_opts'(Opts, open_shared_object(File, Opts, Handle), OptsI),
	'$open_shared_object'(File, OptsI, Handle).

'$open_shared_opts'(Opts, G, OptsI) :-
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
	
