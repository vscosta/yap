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
* File:		sort.pl							 *
* Last rev:								 *
* mods:									 *
* comments:	sorting in Prolog					 *
*									 *
*************************************************************************/
:- system_module( '$_sort', [keysort/2,
        length/2,
        msort/2,
        predmerge/4,
        predmerge/7,
        predsort/3,
        predsort/5,
        sort/2,
        sort2/4], []).

:- use_system_module( '$_errors', ['$do_error'/2]).

/** @addtogroup Comparing_Terms
*/


/*  The three sorting routines are all variations of merge-sort, done by
    bisecting the list, sorting the nearly equal halves, and merging the
    results.   The half-lists aren't actually constructed, the number of
    elements is counted instead (which is why 'length' is in this file).

*/

/** @pred  sort(+ _L_,- _S_) is iso


Unifies  _S_ with the list obtained by sorting  _L_ and  merging
identical (in the sense of `==`) elements.


*/
sort(L,O) :-
	'$skip_list'(NL,L,RL),
	( RL == [] -> true ;
	  var(RL) -> '$do_error'(instantiation_error,sort(L,O)) ;
	 '$do_error'(type_error(list,L),sort(L,O))
	),
	(
         nonvar(O)
        ->
	 (
	   O == []
         ->
           L == []
	 ;
	   '$skip_list'(NO,O,RO),
	   ( RO == [] -> NO =< NL ;
	     var(RO) -> NO =< NL ;
	     '$do_error'(type_error(list,O),sort(L,O))
	   )
         )
        ; true
	),
	'$sort'(L,O).

msort(L,O) :-
	'$msort'(L,O).

/** @pred  keysort(+ _L_, _S_) is iso


Assuming L is a list of the form ` _Key_- _Value_`,
`keysort(+ _L_, _S_)` unifies  _S_ with the list obtained
from  _L_, by sorting its elements according to the value of
 _Key_.

~~~~~
?- keysort([3-a,1-b,2-c,1-a,1-b],S).
~~~~~
would return:

~~~~~
S = [1-b,1-a,1-b,2-c,3-a]
~~~~~


*/
keysort(L,O) :-
	'$skip_list'(NL,L,RL),
	( RL == [] -> true ;
	  var(RL) -> '$do_error'(instantiation_error,sort(L,O)) ;
	  '$do_error'(type_error(list,L),sort(L,O))
	),
	(
	 nonvar(O)
	->
	 '$skip_list'(NO,O,RO),
   	 ( RO == [] -> NO =:= NL ;
	   var(RO) -> NO =< NL ;
	   '$do_error'(type_error(list,O),sort(L,O))
	 )
	; true
	),
	'$keysort'(L,O).

:- meta_predicate prolog:predsort(3,+,-).

%%	predsort(:Compare, +List, -Sorted) is det.
%
%	 Sorts similar to sort/2, but determines  the order of two terms
%	 by calling Compare(-Delta, +E1,  +E2).   This  call  must unify
%	 Delta with one of <, > or =. If built-in predicate compare/3 is
%	 used, the result is the same as sort/2. See also keysort/2.

/** @pred  predsort(+ _Pred_, + _List_, - _Sorted_)


Sorts similar to sort/2, but determines the order of two terms by
calling  _Pred_(- _Delta_, + _E1_, + _E2_) . This call must
unify  _Delta_ with one of `<`, `>` or `=`. If
built-in predicate compare/3 is used, the result is the same as
sort/2.


*/
predsort(P, L, R) :-
	length(L, N),
	predsort(P, N, L, _, R1), !,
	R = R1.

predsort(P, 2, [X1, X2|L], L, R) :- !,
	call(P, Delta, X1, X2),
	sort2(Delta, X1, X2, R).
predsort(_, 1, [X|L], L, [X]) :- !.
predsort(_, 0, L, L, []) :- !.
predsort(P, N, L1, L3, R) :-
	N1 is N // 2,
	plus(N1, N2, N),
	predsort(P, N1, L1, L2, R1),
	predsort(P, N2, L2, L3, R2),
	predmerge(P, R1, R2, R).

sort2(<, X1, X2, [X1, X2]).
sort2(=, X1, _,  [X1]).
sort2(>, X1, X2, [X2, X1]).

predmerge(_, [], R, R) :- !.
predmerge(_, R, [], R) :- !.
predmerge(P, [H1|T1], [H2|T2], Result) :-
	call(P, Delta, H1, H2),
	predmerge(Delta, P, H1, H2, T1, T2, Result).

predmerge(>, P, H1, H2, T1, T2, [H2|R]) :-
	predmerge(P, [H1|T1], T2, R).
predmerge(=, P, H1, _, T1, T2, [H1|R]) :-
	predmerge(P, T1, T2, R).
predmerge(<, P, H1, H2, T1, T2, [H1|R]) :-
	predmerge(P, T1, [H2|T2], R).

%%! @}
