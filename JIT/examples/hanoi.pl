:- initialization(main).

han(N,_,_,_) :- N =< 0.

han(N,A,B,C) :- N > 0,
	       N1 is N-1,
	       han(N1,A,C,B),
	       han(N1,C,B,A).

main :-
    unix( argv([H|_]) ),
    number_atom(N,H),
    han(N,4,5,6),

    statistics,
    statistics_jit.
