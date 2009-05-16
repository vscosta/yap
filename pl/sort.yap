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

