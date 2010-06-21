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

% original code from RA O'Keefe.

%   This is algorithm AS 183 from Applied Statistics.  I also have a C
%   version.  It is really very good.  It is straightforward to make a
%   version which yields 15-bit random integers using only integer
%   arithmetic.
 
:- module(random, [
	random/1,
	random/3,
	randseq/3,
	randset/3,
	getrand/1,
	setrand/1
    ]).

:- load_foreign_files([yap_random], [], init_random).


%   random(R) binds R to a new random number in [0.0,1.0)

%   random(L, U, R) binds R to a random integer in [L,U)
%   when L and U are integers (note that U will NEVER be generated),
%   or to a random floating number in [L,U) otherwise.

random(L, U, R) :- integer(L), integer(U), !,
	random(X),
	R is L+integer((U-L)*X).
random(L, U, R) :-
	number(L), number(U), !,
	random(X),
	R is L+((U-L)*X).

/*  There are two versions of this operation.
 
	randset(K, N, S)
 
    generates a random set of K integers in the range 1..N.
    The result is an ordered list, such as setof might produce.
 
	randseq(K, N, L)
 
    generates a random sequence of K integers, the order is as
    random as we can make it.
*/
 
 
randset(K, N, S) :-
	K >= 0,
	K =< N,
	randset(K, N, [], S).
 
 
randset(0, _, S, S) :- !.
randset(K, N, Si, So) :-
	random(X),
	X * N < K, !,
	J is K-1,
	M is N-1,
	randset(J, M, [N|Si], So).
randset(K, N, Si, So) :-
	M is N-1,
	randset(K, M, Si, So).
 
 
randseq(K, N, S) :-
	randseq(K, N, L, []),
	keysort(L, R),
	strip_keys(R, S).
 
randseq(0, _, S, S) :- !.
randseq(K, N, [Y-N|Si], So) :-
	random(X),
	X * N < K, !,
	random(Y),
	J is K-1,
	M is N-1,
	randseq(J, M, Si, So).
randseq(K, N, Si, So) :-
	M is N-1,
	randseq(K, M, Si, So).
 
 
strip_keys([], []) :- !.
strip_keys([_-K|L], [K|S]) :-
	strip_keys(L, S).

setrand(rand(X,Y,Z)) :-
	integer(X),
	integer(Y),
	integer(Z),
	X > 0,
	X < 30269,
	Y > 0,
	Y < 30307,	
	Z > 0,
	Z < 30323,
	setrand(X,Y,Z).

getrand(rand(X,Y,Z)) :-
	getrand(X,Y,Z).




