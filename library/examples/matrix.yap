
:- use_module(library(matrix)).

t1 :-
	X <== matrix([1,2,3,4,5,6],[dims=[3,2]]),
	writeln(X).

