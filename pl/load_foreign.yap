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
	'$check_lib_for_load_foreign_files'(Libs,load_foreign_files(Objs,Libs,Entry)),
	'$check_entry_for_load_foreign_files'(Entry,load_foreign_files(Objs,Libs,Entry)),
	'$load_foreign_files'(NewObjs,Libs,Entry).

'$check_objs_for_load_foreign_files'(V,_,G) :- var(V), !,
	throw(error(instantiation_error,G)).
'$check_objs_for_load_foreign_files'([],[],_) :- !.
'$check_objs_for_load_foreign_files'([Obj|Objs],[NObj|NewObjs],G) :- !,
	'$check_obj_for_load_foreign_files'(Obj,NObj,G),
	'$check_objs_for_load_foreign_files'(Objs,NewObjs,G).
'$check_objs_for_load_foreign_files'(Objs,_,G) :-
	throw(error(type_error(list,Objs),G)).

'$check_obj_for_load_foreign_files'(V,_,G) :- var(V), !,
	throw(error(instantiation_error,G)).
'$check_obj_for_load_foreign_files'(Obj,NewObj,_) :- atom(Obj), !,
	atom_codes(Obj,ObjCodes),
	'$process_obj_suffix'(ObjCodes,NewObjCodes),
	atom_codes(NewObj,NewObjCodes).
'$check_obj_for_load_foreign_files'(Obj,_,G) :-
	throw(error(type_error(atom,Obj),G)).

'$check_libs_for_load_foreign_files'(V,G) :- var(V), !,
	throw(error(instantiation_error,G)).
'$check_libs_for_load_foreign_files'([],_) :- !.
'$check_libs_for_load_foreign_files'([Lib|Libs],G) :- !,
	'$check_lib_for_load_foreign_files'(Lib,G),
	'$check_libs_for_load_foreign_files'(Libs,G).
'$check_libs_for_load_foreign_files'(Libs,G) :-
	throw(error(type_error(list,Libs),G)).

'$check_lib_for_load_foreign_files'(V,G) :- var(V), !,
	throw(error(instantiation_error,G)).
'$check_lib_for_load_foreign_files'(Lib,_) :- atom(Lib), !.
'$check_lib_for_load_foreign_files'(Lib,G) :-
	throw(error(type_error(atom,Lib),G)).

'$check_entry_for_load_foreign_files'(V,G) :- var(V), !,
	throw(error(instantiation_error,G)).
'$check_entry_for_load_foreign_files'(Entry,_) :- atom(Entry), !.
'$check_entry_for_load_foreign_files'(Entry,G) :-
	throw(error(type_error(atom,Entry),G)).


'$process_obj_suffix'(ObjCodes,ObjCodes) :-
	'$obj_suffix'(ObjSuffix),
	'$append'(ObjCodes,ObjSuffix,ObjCodes), !.
'$process_obj_suffix'(ObjCodes,NewObjCodes) :-
	'$obj_suffix'(ObjSuffix),
	'$append'(ObjCodes,ObjSuffix,NewObjCodes).


