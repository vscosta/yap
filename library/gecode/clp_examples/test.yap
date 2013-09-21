
:- style_check( all ).

:- use_module(library(gecode/clpfd)).
:- use_module(library(maplist)).

t0 :-
	test0(X),
	writeln(X).

test0(X) :-
	X in 1..10,
	X #= 2.

t1 :-
	test1(X),
	writeln(X),
	fail.
t1.

test1(X) :-
	X in 1..10,
	Y in 3..7,
	Z in 1..4,
	X / Y #= Z,
	labeling([], [X]).

t2 :-
	test2(X),
	writeln(X),
	fail.
t2.
	
test2(X) :-
	X in 1..10,
	X / 4 #= 2,
	labeling([], [X]).

t3 :-
	test3(X),
	writeln(X),
	fail.
t3.
	

test3(A) :-
	A = [X,Y,Z],
	A ins 1..4,
	Y #> 2,
	lex_chain(A),
	all_different(A),
	labeling([], [X,Y,Z]).

t4 :-
	test4(X),
	writeln(X),
	fail.
t4.

test4(A) :-
	A = [X,Y,Z],
	A ins 1..4,
	Y #> 2,
	Z #> 3,
	lex_chain(A),
	min(A, 1),
	all_different(A),
	labeling([], [X,Y,Z]).

t5 :-
	test5(X),
	writeln(X),
	fail.
t5.

test5(A) :-
	A = [X,Y,Z],
	A ins 0..1,
	in_relation( A, [[0,0,0],[0,1,0],[1,0,0]] ),
	X #> 0,
	labeling([], A).

t6 :-
	test6(X),
	writeln(X),
	fail.
t6.

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

t7 :-
	test7(X),
	writeln(X),
	fail.
t7.

test7(A) :-
	A = [X,Y,Z],
	A ins 0..1,
	in_dfa( A, 0, [t(0,0,0),t(0,1,1),t(1,0,0),t(-1,0,0)], [0]),
	X #> 0,
	labeling([], A).

t8 :-
	test8(X),
	writeln(X),
	fail.
t8.


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
