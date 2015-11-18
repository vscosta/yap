/**
 * @file   clauses.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 14:51:30 2015
 * 
 * @brief  Utilities for clause manipulation.
 * 
 * 
*/

:- module(clauses,
          [list2conj/2,
           conj2list/2,
	   clauselength/2]).

%%! @{

/**
 *  @defgroup clauses Clause Manipulation
 *  @ingroup library

  This library supports a number of useful utilities that come up over and
  over again when manipulating Prolog programs. This will include
  operations and conversion to other structures.

  @author Vitor Santos Costa
*/

/** conj2list( +Conj, -List) is det
  Generate a list from a conjunction of literals.

  It is often easier to apply operations on lists than on clauses
*/
conj2list( Conj, List ) :-
	conj2list( Conj, List, [] ).


conj2list( C ) -->
	{ var(C) },
	!,
	[C].
conj2list( true )  --> !.
conj2list( (C1, C2) ) -->
	!,
	conj2list( C1 ),
	conj2list( C2 ).
conj2list( C ) -->
        [C].

/** conj2list( +List, -Conj) is det
  Generate a conjunction from a list of literals.

  Notice that this relies on indexing within the list to avoid creating
  choice-points.
*/
list2conj([], true).
list2conj([Last], Last).
list2conj([Head,Next|Tail], (Head,Goals)) :-
	list2conj([Next|Tail], Goals).

/** clauselength( +Clause, -Length) is det
  Count the number of literals in a clause (head counts as one).

  Notice that this is 1+length(conj2list), as we ignore  disjunctions.
*/
clauselength( (_Head :- Conj), Length ) :-
	clauselength( Conj, Length, 1 ).


clauselength( C, I1, I ) :-
	{ var(C) },
	!,
	I1 is I+1.
clauselength( (C1, C2), I2, I ) :- !,
	clauselength( C1, I1, I ),
	clauselength( C2, I2, I1 ).
clauselength( _C, I1, I ) :-
        I1 is I+1.

