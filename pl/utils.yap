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
 * File:		utils.yap						 *
 * Last rev:	8/2/88							 *
 * mods:									 *
 * comments:	Some utility predicates available in yap		 *
 *									 *
 *************************************************************************/

/**
  * @file   utils.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 12:21:01 2017
  * 
  * @brief  Utilities
  *
  * @defgroup MixBag Diverse Utilities
  * @ingroup Builtins
  * 
  * 
  */


prolog :-
	live.

%%% current ....

/** @pred  simple( _T_)


Checks whether  _T_ is unbound, an atom, or a number.


*/
simple(V) :- var(V), !.
simple(A) :- atom(A), !.
simple(N) :- number(N).

/** @pred  nth_instance(? _Key_,? _Index_,? _R_)


Fetches the  _Index_nth entry in the internal database under the key
 _Key_. Entries are numbered from one. If the key  _Key_ or the
 _Index_ are bound, a reference is unified with  _R_. Otherwise,
the reference  _R_ must be given, and YAP will find
the matching key and index.


*/
nth_instance(Key,Index,Ref) :-
	nonvar(Key), var(Index), var(Ref), !,
	recorded(Key,_,Ref),
	'$nth_instance'(_,Index,Ref).
nth_instance(Key,Index,Ref) :-
	'$nth_instance'(Key,Index,Ref).

/** @pred  nth_instance(? _Key_,? _Index_, _T_,? _R_)

Fetches the  _Index_nth entry in the internal database under the key
 _Key_. Entries are numbered from one. If the key  _Key_ or the
 _Index_ are bound, a reference is unified with  _R_. Otherwise,
the reference  _R_ must be given, and YAP will find
the matching key and index.


*/
nth_instance(Key,Index,T,Ref) :-
	nonvar(Key), var(Index), var(Ref), !,
	recorded(Key,T,Ref),
	'$nth_instance'(_,Index,Ref).
nth_instance(Key,Index,T,Ref) :-
	'$nth_instance'(Key,Index,Ref),
	instance(Ref,T).

/** @pred  nb_current(? _Name_, ? _Value_)


Enumerate all defined variables with their value. The order of
enumeration is undefined.


*/
/** @pred nb_current(? _Name_,? _Value_)


Enumerate all defined variables with their value. The order of
enumeration is undefined.


*/
nb_current(GlobalVariable, Val) :-
	'$nb_current'(GlobalVariable),
	'__NB_getval__'(GlobalVariable, Val, _).


/** @pred  subsumes_term(? _Subsumer_, ? _Subsumed_)



Succeed if  _Submuser_ subsumes  _Subsuned_ but does not bind any
variable in  _Subsumer_.


*/
subsumes_term(A,B) :-
	\+ \+ terms:subsumes(A,B).

term_string( T, S, Opts) :-
    var( T ),
    !,
    memory_file:open_mem_read_stream( S, Stream ),
    read_term( Stream, T, Opts ),
    close( Stream ).
    term_string( T, S, _Opts) :-
        format(string(S), '~q.~n', [T]).


term_hash(T,H) :-
	term:term_hash(T, -1, 33554432, H).
/*
numbervars( T, I0, I) :-
    term_variables(T, Vs),
    bind_variables(Vs,I0,I).

bind_variables([],I,I).
bind_variables(['$VAR'(I0)|Vs],I0,I) :-
    I1 is I0+1,
    bind_variables(Vs,I1,I).
*/

