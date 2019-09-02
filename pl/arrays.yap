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
* File:		arrays.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	Array Manipulation					 *
*									 *
*************************************************************************/

/**

@file arrays.yap

@addtogroup YAPArrays
  
@{
*/
%
% These are the array built-in predicates. They will only work if
% YAP_ARRAYS is defined in Yap.h
%

/** @pred  array( +Name, +Size )

Creates a new dynamic array. The  _Size_ must evaluate to an
integer. The  _Name_ may be either an atom (named array) or an
unbound variable (anonymous array).

Dynamic arrays work as standard compound terms, hence space for the
array is recovered automatically on backtracking.


*/
array(Obj, Size) :-
	'$create_array'(Obj, Size).


% arithmetical optimization
'$c_arrays'((P:-Q),(NP:-QF)) :- !,
	'$c_arrays_body'(Q, QI),
	'$c_arrays_head'(P, NP, QI, QF).
'$c_arrays'(P, NP) :-
	'$c_arrays_fact'(P, NP).

'$c_arrays_body'(P, P) :-
	var(P), !.
'$c_arrays_body'((P0,Q0), (P,Q)) :- !,
	'$c_arrays_body'(P0, P),
	'$c_arrays_body'(Q0, Q).
'$c_arrays_body'((P0;Q0), (P;Q)) :- !,
	'$c_arrays_body'(P0, P),
	'$c_arrays_body'(Q0, Q).
'$c_arrays_body'((P0->Q0), (P->Q)) :- !,
	'$c_arrays_body'(P0, P),
	'$c_arrays_body'(Q0, Q).
'$c_arrays_body'(P, NP) :- '$c_arrays_lit'(P, NP).

%
% replace references to arrays to references to built-ins.
%
'$c_arrays_lit'(G, GL) :-
	'$array_references'(G, NG, VL),
	'$add_array_entries'(VL, NG, GL).

'$c_arrays_head'(G, NG, B, NB) :-
	'$array_references'(G, NG, VL),
	'$add_array_entries'(VL, B, NB).

'$c_arrays_fact'(G, NG) :-
	'$array_references'(G, IG, VL),
	(VL = [] -> NG = G;
	    NG = (IG :- NB), '$add_array_entries'(VL, true, NB)).

'$add_array_entries'([], NG, NG).
'$add_array_entries'([Head|Tail], G, (Head, NG)) :-
	'$add_array_entries'(Tail, G, NG).


/** @pred  static_array_properties(? _Name_, ? _Size_, ? _Type_)


Show the properties size and type of a static array with name
 _Name_. Can also be used to enumerate all current
static arrays.

This built-in will silently fail if the there is no static array with
that name.


*/
static_array_properties(Name, Size, Type) :-
	atom(Name), !,
	'$static_array_properties'(Name, Size, Type).
static_array_properties(Name, Size, Type) :-
	var(Name), !,
	current_atom(Name),
	'$static_array_properties'(Name, Size, Type).
static_array_properties(Name, Size, Type) :-
	'$do_error'(type_error(atom,Name),static_array_properties(Name,Size,Type)).

%% @}
