
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
test3(A) :-
	A = [X,Y,Z],
	A ins 1..4,
	Y #> 2,
	lex_chain(A),
	all_different(A),
	labeling([], [X,Y,Z]).
test4(A) :-
	A = [X,Y,Z],
	A ins 1..4,
	Y #> 2,
	Z #> 3,
	lex_chain(A),
	min(A, 1),
	all_different(A),
	labeling([], [X,Y,Z]).
test5(A) :-
	A = [X,Y,Z],
	A ins 0..1,
	in_relation( A, [[0,0,0],[0,1,0],[1,0,0]] ),
	X #> 0,
	labeling([], A).
test6(A+B) :-
	A = [X,Y,Z],
	B = [X1,Y1,Z1],
	A ins 0..1,
	B ins 0..1,
	extensional_constraint([[0,0,0],[0,1,0],[1,0,0]], C),
	in_relation( A, C ),
	in_relation( B, C ),
	X #> 0,
	X1 #< X,
	Y1 #\= Z1,
	labeling([], A),
	labeling([], B).
test7(A) :-
	A = [X,Y,Z],
	A ins 0..1,
	in_dfa( A, 0, [t(0,0,0),t(0,1,1),t(1,0,0),t(-1,0,0)], [0]),
	X #> 0,
	labeling([], A).
test8(A+B) :-
	A = [X,Y,Z,W],
	B = [X1,Y1,Z1,W1],
	A ins 0..1,
	B ins 0..1,
	dfa( 0, [t(0,0,0),t(0,1,1),t(1,0,0),t(-1,0,0)], [0], C),
	in_dfa( A, C ),
	in_dfa( B, C ),
	X #> 0,
	X1 #< X,
	Y1 #\= Z1,
	labeling([], A),
	labeling([], B).
