
:- use_module(library(cuda)).
:- use_module(library(lists)).

:- initialization(main).

main :-
	Rule = ( db(Y, Z), db(X, Z), db(1, Z), X = Y  ),
	setof(a(X,Y), Z^Rule, L0), reverse(L0, RL0), writeln(RL0), 
	cuda_rule((a(X, Y) :- Rule ), Q),
	cuda_eval(Q, L),
	cuda_erase( Q ),
	writeln(L).

main2 :-
	Rule = ( db(Y, Z), db(X, Z), db(1, Z), X \= Y  ),
	setof(a(X,Y), Z^Rule, L0), reverse(L0, RL0), writeln(RL0), 
	cuda_rule((a(X, Y) :- Rule ), Q),
	cuda_eval(Q, L),
	cuda_erase( Q ),
	writeln(L).


db(1,a).
db(2,a).
db(5,b).
db(4,q).
db(6,w).
db(10,s).
/*
db(11,a).
db(12,a).
db(15,b).
db(14,q).
db(16,w).
db(110,s).
db(21,a).
db(22,a).
db(25,b).
db(24,q).
db(26,w).
db(210,s).
*/

:- 	cuda_extensional(db/2, _X).