
:- use_module(library(gecode/clpfd)).
:- use_module(library(maplist)).

main :-
	between(1,10,N),
	I is N*100,
	statistics( runtime, _ ),
	once( queens(I, _Queens) ),
	statistics( runtime, [DT|_] ),
%	findall(Queens, queens(I, Queens), Solutions ),
%	length( Solutions, N),
	format('~d took ~w msec to find first solution.~n', [I, DT]),
	fail.
main.

queens(N, Queens) :-
	length(Queens, N),
	Queens ins 1..N,
	all_distinct(Queens),
	foldl(inc, Queens, Inc, 0, _), % [0, 1, 2, .... ]
	foldl(dec, Queens, Dec, 0, _), % [0, -1, -2, ... ]
	all_distinct(Inc,Queens),
	all_distinct(Dec,Queens),
	labeling([], Queens).

inc(_, I0, I0, I) :-
	I is I0+1.

dec(_, I0, I0, I) :-
	I is I0-1.

lqueens(N, Queens) :-
	length(Queens, N),
	Queens ins 1..N,
	all_distinct(Queens),
	lconstrain( Queens, 0 ),
	labeling([], Queens).

lconstrain([], _).
lconstrain( [Q|Queens], I0) :-
	I is I0+1,
	foldl(constrain(Q, I0), Queens, I, _),
	lconstrain( Queens, I).

constrain(Q, I, R, J, J1) :-
	J1 is J+1,
	Q + I #\= R + J,
	Q - I #\= R - J.
