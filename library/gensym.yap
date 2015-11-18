/**
 * @file   gensym.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 18:37:13 2015
 * 
 * @brief  Generate a new atom.
 * 
 * 
*/
:- module(gensym, [
	gensym/2,
	reset_gensym/1,
	reset_gensym/0
                  ]).

/**
* @defgroup gensym Generate a new symbol.
* @ingroup library
*
* Predicates to create new atoms based on the prefix _Atom_.
* They use a counter, stored as a
* dynamic predicate, to construct the atom's suffix. 
*
*/


:- dynamic gensym_key/2.

gensym(Atom, New) :-
	retract(gensym_key(Atom,Id)), !,
	atomic_concat(Atom,Id,New),
	NId is Id+1,
	assert(gensym_key(Atom,NId)).
gensym(Atom, New) :-
	atomic_concat(Atom,1,New),
	assert(gensym_key(Atom,2)).

reset_gensym(Atom) :-
	retract(gensym_key(Atom,_)).

reset_gensym :-
	retractall(gensym_key(_,_)).


