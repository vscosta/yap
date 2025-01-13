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
   	      init_gensym/1,
	gensym/2,
	reset_gensym/1,
	reset_gensym/0
                  ]).

/**
* @defgroup gensym Generate a new symbol.
* @ingroup YAPLibrary
* @brief Predicates to create new atoms based on a prefix _Atom_.
* @{
* The predicates use a counter, stored as a
* dynamic predicate, to construct the atom's suffix.
*
*/


:- dynamic gensym_key/2.

/**
  @pred init_gensym(Key)

  Initialize a new atom generator with prefix _Key_.
*/
init_gensym(Key) :-
    retractall(gensym_key(Key,_)),
    assert(gensym_key(Key,0) ).

/**
  @pred init_gensym(Key)

  Generate the new atom _New_ with prefix _Key_.
*/
gensym(Key, New) :-
	retract(gensym_key(Key,Id)), !,
	atomic_concat(Key,Id,New),
	NId is Id+1,
	assert(gensym_key(Key,NId)).
gensym(Atom, New) :-
	atomic_concat(Atom,0,New),
	assert(gensym_key(Atom,1)).

/**
  @pred reset_gensym(_Key_)

  Restart atoms for prefix _Key_..
*/
reset_gensym(Atom) :-
	retract(gensym_key(Atom,_)).

/**
  @pred reset_gensym

  Restart everything.
*/
reset_gensym :-
	retractall(gensym_key(_,_)).

/** @} */


