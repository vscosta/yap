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
* File:		random.yap						 *
* Last rev:	5/12/99							 *
* mods:									 *
* comments:	Random operations					 *
*									 *
*************************************************************************/

:- module(random, [
	random/1,
	random/3,
	randseq/3,
	randset/3,
	getrand/1,
	setrand/1
    ]).


random(X) :- X is random.

random(X, LOW, UPPER) :- integer(LOW), integer(UPPER), !,
	X is integer(random*(UPPER-LOW))+LOW.
random(X, LOW, UPPER) :- 
	X is random*(UPPER-LOW)+LOW.

randseq(L, M, Rs) :-
	integer(L),
	L > 0,
	integer(M),
	M > 0,
	M > L,
	randseq(L, M, [], Rs).

randseq(0, _, Rs, Rs) :- !.
randseq(K, N, Set, Rs) :-
	X is integer(random*N),
	not_in(Set, X), !,
	K1 is K-1,
	randseq(K1, N, [X|Set], Rs).
randseq(K, N, Set, Rs) :-
	randseq(K, N, Set, Rs).

not_in([], _).
not_in([X|L], Y) :- X \= Y,
	not_in(L, Y).

randset(L, M, Rs) :-
	integer(L),
	L > 0,
	integer(M),
	M > 0,
	M > L,
	randset(L, M, [], Rs).

randset(0, _, Rs, Rs) :- !.
randset(K, N, Set, Rs) :-
	X is integer(random*N),
	addnew(Set, X, NSet), !,
	K1 is K-1,
	randset(K1, N, NSet, Rs).
randset(K, N, Set, Rs) :-
	randset(K, N, Set, Rs).

addnew([], Y, [Y]).
addnew([X|L], Y, [Y,X|L]) :- X > Y, !.
addnew([X|L], Y, [X|NSet]) :-
	X < Y,
	addnew(L, Y, NSet).

getrand(rand(X,Y,Z)) :-
	srandom(Seed0),
	Seed is abs(Seed0),
	X is Seed mod 30269,
	Seed1 is Seed // 30269,
	Y is Seed1 mod 30307,
	Seed2 is Seed1 // 30307,
	Z is Seed2 mod 30323.

setrand(rand(X,Y,Z)) :-
	integer(X),
	X > 1,
	X < 30269,
	integer(Y),
	Y > 1,
	Y < 30307,	
	integer(Z),
	Z > 1,
	Z < 30323,	
	Seed is X + 30269*(Y + 30307*Z),
	srandom(Seed).



