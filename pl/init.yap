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
* File:		init.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	initializing the full prolog system			 *
*									 *
*************************************************************************/

% This is yap's init file
% should be consulted first step after booting

% These are pseudo declarations
% so that the user will get a redefining system predicate
otherwise.
fail :- fail.
false :- false.
!.
(:- G) :- '$execute'(G), !.
'$$!'(CP) :- '$cut_by'(CP).
[] :- true.

:- '$cut_transparent'(','(_,_)).
:- '$cut_transparent'(';'(_,_)).
:- '$cut_transparent'('|'(_,_)).
:- '$cut_transparent'('->'(_,_)).
:- '$cut_transparent'(\+ _).
:- '$cut_transparent'(not(_)).


:- '$set_value'('$doindex',true).

:- 	['errors.yap',
	 'utils.yap',
	 'arith.yap'].

:- compile_expressions.

:- [	 'yio.yap',
	 'debug.yap',
	 'consult.yap',
	 'checker.yap',
	 'depth_bound.yap',
	 'directives.yap',
	 'grammar.yap',
	 'ground.yap',
	 'listing.yap',
	 'preds.yap',
	 % modules must be after preds, otherwise we will have trouble
	 % with meta-predicate expansion being invoked
	 'modules.yap',
	 'load_foreign.yap',
	 'sockets.yap',
	 'sort.yap',
	 'setof.yap',
	 'strict_iso.yap',
	 'tabling.yap',
	 'yapor.yap'].

:-	 ['protect.yap'].

version(yap,[4,1]).

system_mode(verbose,on)  :- '$set_value'('$verbose',on).
system_mode(verbose,off) :- '$set_value'('$verbose',off).

:- op(1150,fx,(mode)).

:- dynamic 'extensions_to_present_answer'/1.

:- 	['corout.yap',
	 'arrays.yap'].

:- '$change_type_of_char'(36,7). % Make $ a symbol character

:- default_sequential(off).

:- '$set_pred_module'(get_att(_,_,_), attributes),
	'$set_pred_module'(get_all_atts(_,_), attributes),
	'$set_pred_module'(free_att(_,_), attributes),
	'$set_pred_module'(put_att(_,_,_), attributes),
	'$set_pred_module'(rm_att(_,_), attributes),
	'$set_pred_module'(inc_n_of_atts(_), attributes),
	'$set_pred_module'(n_of_atts(_), attributes),
	'$set_pred_module'(bind_attvar(_), attributes).

:- '$set_pred_module'(open_mem_read_stream(_,_), charsio),
	'$set_pred_module'(open_mem_write_stream(_), charsio),
	'$set_pred_module'(peek_mem_write_stream(_,_,_), charsio).

:- '$set_pred_module'(term_hash(_,_,_,_), terms),
   '$set_pred_module'(term_hash(_,_), terms),
   '$set_pred_module'(term_variables(_,_), terms),
   '$set_pred_module'(variant(_,_), terms),
   '$set_pred_module'(subsumes(_,_), terms),
   '$set_pred_module'(cyclic_term(_), terms),
   '$set_pred_module'(acyclic_term(_,_), terms).

:- '$set_value'('$user_module',user), '$protect'.

:- style_check([]).

%
% moved this to init_gc in gc.c to separate the alpha
%
% :- yap_flag(gc,on).

% :- yap_flag(gc_trace,verbose).

:- system_mode(verbose,on).

:- module(user).


:- multifile goal_expansion/3.

:- dynamic_predicate(goal_expansion/3, logical).

:- multifile term_expansion/2.

:- dynamic_predicate(term_expansion/2, logical).

:- multifile file_search_path/2.

:- dynamic_predicate(file_search_path/2, logical).

file_search_path(library, Dir) :-
     library_directory(Dir).
file_search_path(system, Dir) :-
     prolog_flag(host_type, Dir).

:- multifile library_directory/1.

:- dynamic_predicate(library_directory/1, logical).

:- get_value(system_library_directory,D), assert(library_directory(D)).

