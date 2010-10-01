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

load_foreign_files(Objs,Libs,Entry) :-
	'$check_objs_for_load_foreign_files'(Objs,NewObjs,load_foreign_files(Objs,Libs,Entry)),
	'$check_libs_for_load_foreign_files'(Libs,NewLibs,load_foreign_files(Objs,Libs,Entry)),
	'$check_entry_for_load_foreign_files'(Entry,load_foreign_files(Objs,Libs,Entry)),
	'$load_foreign_files'(NewObjs,NewLibs,Entry).

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
	atom_codes(Obj,ObjCodes),
	'$process_obj_suffix'(ObjCodes,NewObjCodes),
	atom_codes(NewObj,NewObjCodes).
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
	atom_codes(Lib,LibCodes),
	'$process_obj_suffix'(LibCodes,NewLibCodes),
	'$checklib_prefix'(NewLibCodes,FullLibCodes),
	atom_codes(NLib,FullLibCodes).
'$check_lib_for_load_foreign_files'(Lib,_,G) :-
	'$do_error'(type_error(atom,Lib),G).

'$process_obj_suffix'(ObjCodes,ObjCodes) :-
	'$obj_suffix'(ObjSuffix),
	lists:append(_,ObjSuffix,ObjCodes), !.
'$process_obj_suffix'(ObjCodes,NewObjCodes) :-
	'$obj_suffix'(ObjSuffix),
	lists:append(ObjCodes,ObjSuffix,NewObjCodes).

'$checklib_prefix'(Cs,Cs) :- '$rooted_path'(Cs), !.
'$checklib_prefix'([0'l,0'i,0'b|NewObjCodes],[0'l,0'i,0'b|NewObjCodes]) :- !.
'$checklib_prefix'(NewObjCodes,[0'l,0'i,0'b|NewObjCodes]).

'$rooted_path'([C|_]) :- '$dir_separator'(C), !.
% windows drive z:\
'$rooted_path'([_,0':,0'\\|_]) .

'$get_drive'([0':|_]) :- !.
'$get_drive'([0'\\|_]) :- !, fail.
'$get_drive'([_|L]) :-
	'$get_drive'(L).
				      

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
'$open_shared_opts'(Opt.Opts, G, V) :-
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
	
