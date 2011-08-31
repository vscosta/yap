
/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2011	 *
*									 *
**************************************************************************
*									 *
* File:		qly.yap							 *
* Last rev:								 *
* mods:									 *
* comments:	fast save/restore					 *
*									 *
*************************************************************************/

qsave_program(File) :-
	'$save_program_status',
	open(File, write, S, [type(binary)]),
	'$qsave_program'(S),
	close(S).	

'$save_program_status' :-
	findall(F:V,yap_flag(F,V),L),
	recordz('$program_state',L,_).

'$init_state' :-
	recorded('$program_state', _, _), !,
	'$do_init_state'.
'$init_state'.

'$do_init_state' :-
	 '$init_preds',
	 fail.
'$do_init_state' :-
	recorded('$program_state',L,R),
	erase(R),
	lists:member(F:V,L),
	catch(yap_flag(F,V),_,fail),
	fail.
'$do_init_state' :-
	set_value('$user_module',user), '$protect'.
'$do_init_state' :-
	'$current_module'(prolog),
	module(user),
	fail.
'$do_init_state' :-
	'$init_system',
	fail.
'$do_init_state'.


qsave_module(Mod) :-
	recorded('$module', '$module'(F,Mod,Exps), _),
	'$fetch_parents_module'(Mod, Parents),
	'$fetch_imports_module'(Mod, Imps),
	'$fetch_multi_files_module'(Mod, MFs),
	'$fetch_meta_predicates_module'(Mod, Metas),
	'$fetch_module_transparents_module'(Mod, ModTransps),
	asserta(Mod:'@mod_info'(F, Exps, Parents, Imps, Metas, ModTransps)),
	atom_concat(Mod,'.qly',OF),
	open(OF, write, S, [type(binary)]),
	'$qsave_module_preds'(S, Mod),
	close(S),
	abolish(Mod:'@mod_info'/6),
	fail.
qsave_module(_).

qload_program(File) :-
	open(File, read, S, [type(binary)]),
	'$qload_program'(S),
	close(S).

qload_module(Mod) :-
	atom_concat(Mod,'.qly',IF),
	open(IF, read, S, [type(binary)]),
	'$qload_module_preds'(S),
	close(S),
	fail.
qload_module(Mod) :-
	'$complete_read'(Mod).

'$complete_read'(Mod) :-
	retract(Mod:'@mod_info'(F, Exps, Parents, Imps, Metas, ModTransps)),
	abolish(Mod:'$mod_info'/6),
	recorda('$module', '$module'(F,Mod,Exps), _),
	'$install_parents_module'(Mod, Parents),
	'$install_imports_module'(Mod, Imps),
	'$install_multi_files_module'(Mod, MFs),
	'$install_meta_predicates_module'(Mod, Metas),
	'$install_module_transparents_module'(Mod, ModTransps).
	
'$fetch_imports_module'(Mod, Imports) :-
	findall(Info, '$fetch_import_module'(Mod, Info), Imports).

% detect an importerator that is local to the module.
'$fetch_import_module'(Mod, '$import'(Mod0,Mod,G0,G,N,K)) :-
	recorded('$import', '$import'(Mod0,Mod,G0,G,N,K), _).

'$fetch_parents_module'(Mod, Parents) :-
	findall(Parent, prolog:'$parent_module'(Mod,Parent), Parents).

'$fetch_module_transparents_module'(Mod, Module_Transparents) :-
	findall(Info, '$fetch_module_transparent_module'(Mod, Info), Module_Transparents).

% detect an module_transparenterator that is local to the module.
'$fetch_module_transparent_module'(Mod, '$module_transparent'(F,Mod,N,P)) :-
	prolog:'$module_transparent'(F,Mod0,N,P), Mod0 == Mod.

'$fetch_meta_predicates_module'(Mod, Meta_Predicates) :-
	findall(Info, '$fetch_meta_predicate_module'(Mod, Info), Meta_Predicates).

% detect an meta_predicateerator that is local to the module.
'$fetch_meta_predicate_module'(Mod, '$meta_predicate'(F,Mod,N,P)) :-
	prolog:'$meta_predicate'(F,Mod0,N,P), Mod0 == Mod.

'$fetch_multi_files_module'(Mod, Multi_Files) :-
	findall(Info, '$fetch_multi_file_module'(Mod, Info), Multi_Files).

% detect an multi_fileerator that is local to the module.
'$fetch_multi_file_module'(Mod, '$defined'(FileName,Name,Arity,Mod)) :-
	recorded('$multifile_defs','$defined'(FileName,Name,Arity,Mod), _).

'$fetch_term_expansions_module'(Mod, Term_Expansions) :-
	findall(Info, '$fetch_term_expansion_module'(Mod, Info), Term_Expansions).

% detect an term_expansionerator that is local to the module.
'$fetch_term_expansion_module'(Mod,'$defined'(FileName,Name,Arity,Mod)) :-
	recorded('$multifile_defs','$defined'(FileName,Name,Arity,Mod), _).



'$install_ops_module'(_, []).
'$install_ops_module'(Mod, op(X,Y,Op).Ops) :-
	op(X, Y, Mod:Op),
	'$install_ops_module'(Mod, Ops).

'$install_imports_module'(_, []).
'$install_imports_module'(Mod, Import.Imports) :-
	recordz('$import', Import, _),
	'$install_imports_module'(Mod, Imports).

'$install_parents_module'(_, []).
'$install_parents_module'(Mod, Parent.Parents) :-
	assert(prolog:Parent),
	'$install_parents_module'(Mod, Parents).

'$install_module_transparents_module'(_, []).
'$install_module_transparents_module'(Mod, Module_Transparent.Module_Transparents) :-
	assert(prolog:Module_Transparent),
	'$install_module_transparents_module'(Mod, Module_Transparents).

'$install_meta_predicates_module'(_, []).
'$install_meta_predicates_module'(Mod, Meta_Predicate.Meta_Predicates) :-
	assert(prolog:Meta_Predicate),
	'$install_meta_predicates_module'(Mod, Meta_Predicates).

'$install_multi_files_module'(_, []).
'$install_multi_files_module'(Mod, Multi_File.Multi_Files) :-
	recordz('$multifile_defs',Multi_File, _).
	'$install_multi_files_module'(Mod, Multi_Files).



