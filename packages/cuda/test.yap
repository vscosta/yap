
:- use_module(library(cuda)).

:- initialization(main).

main :-
	cuda_extensional(db/2, _X),
	cuda_rule((a(X, Y) :- db(Y, Z), db(X, Z), db(1, Z) ), Q),
	cuda_eval(Q, L), writeln(here),
	writeln(L).

db(1,a).
db(2,a).
db(5,b).
db(4,q).
db(6,w).
db(10,s).
