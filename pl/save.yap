 /*************************************************************************
 *									 *
 *	 YAP Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2010	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		save.yap					 *
 * Last rev:	11/29/10						 *
 * mods:								 *
 * comments:	Some utility predicates to support save/restore in yap	 *
 *									 *
 *************************************************************************/

/**
  * @file   save.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 12:10:47 2017
  *
  * @brief  Old Style save
  */

 /**
  * @addtogroup QLY
  * @{
  *
*/

:- system_module( '$_save', [], []).

%%% Saving and restoring a computation
/*
save(A) :- save(A,_).

save(A,_) :- var(A), !,
	'$do_error'(instantiation_error,save(A)).
save(A,OUT) :- atom(A), !, atom_codes(A,S), '$save'(S,OUT).
save(S,OUT) :- '$save'(S,OUT).

save_program(A) :- var(A), !,
	'$do_error'(instantiation_error,save_program(A)).
save_program(A) :- atom(A), !,
	atom_codes(A,S),
	'$save_program2'(S, true).
save_program(S) :- '$save_program2'(S, true).

save_program(A, G) :- var(A), !,
	'$do_error'(instantiation_error, save_program(A,G)).
save_program(A, G) :- var(G), !,
	'$do_error'(instantiation_error, save_program(A,G)).
save_program(A, G) :- \+ callable(G), !,
	'$do_error'(type_error(callable,G), save_program(A,G)).
save_program(A, G) :-
	( atom(A) -> atom_codes(A,S) ; A = S),
	'$save_program2'(S, G),
	fail.
save_program(_,_).

'$save_program2'(S,G) :-
	(
	    G == true
        ->
	     true
	 ;
	     recorda('$restore_goal', G ,R)
	),
	(
	    '$undefined'(reload_foreign_libraries, shlib)
        ->
	     true
	 ;
	     recorda('$reload_foreign_libraries', true, R1)
	),
	'$save_program'(S),
	(
	    var(R1)
        ->
	     true
	 ;
	     erase(R1)
	),
	(
	    var(R)
        ->
	     true
	 ;
	     erase(R)
	),
	fail.
'$save_program2'(_,_).

restore(A) :- var(A), !,
	'$do_error'(instantiation_error,restore(A)).
restore(A) :- atom(A), !, name(A,S), '$restore'(S).
restore(S) :- '$restore'(S).

*/

/**
 * @}
 */
