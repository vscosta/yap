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

/*  The three sorting routines are all variations of merge-sort, done by
    bisecting the list, sorting the nearly equal halves, and merging the
    results.   The half-lists aren't actually constructed, the number of
    elements is counted instead (which is why 'length' is in this file).

*/

%   length of a list.

length(L,M) :- var(M), !, '$$_length1'(L,0,M).
length(L,M) :- '$$_length2'(M,L).

'$$_length1'([], M, M).
'$$_length1'([_|L], N, O) :-
	M is N + 1,
	'$$_length1'(L, M, O).

'$$_length2'(N, L) :-
	( N =:= 0
		->
          L = []
        ;
	  N > 0,
	  N1 is N - 1,
	  L = [_|L1],
	  '$$_length2'(N1, L1)
        ).


sort(L,O) :-
	'$sort'(L,O).

msort(L,O) :-
	'$msort'(L,O).

keysort(L,O) :-
	'$keysort'(L,O).

:- meta_predicate prolog:predsort(3,+,-).

%%	predsort(:Compare, +List, -Sorted) is det.
%
%	 Sorts similar to sort/2, but determines  the order of two terms
%	 by calling Compare(-Delta, +E1,  +E2).   This  call  must unify
%	 Delta with one of <, > or =. If built-in predicate compare/3 is
%	 used, the result is the same as sort/2. See also keysort/2.

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

