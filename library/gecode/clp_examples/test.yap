
:- use_module(library(gecode/clpfd)).
:- use_module(library(maplist)).

test0(X) :-
	X in 1..10,
	X #= 2.
test1(X) :-
	X in 1..10,
	Y in 3..7,
	Z in 1..4,
	X / Y #= Z,
	labeling([], [X]).
test2(X) :-
	X in 1..10,
	X / 4 #= 2,
	labeling([], [X]).
