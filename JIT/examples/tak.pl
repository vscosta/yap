:- initialization(main).

tak(X,Y,Z,A) :- X =< Y, 
                Z = A.

tak(X,Y,Z,A) :- X > Y, 
                X1 is X - 1, 
                tak(X1,Y,Z,A1), 
                Y1 is Y - 1, 
                tak(Y1,Z,X,A2), 
                Z1 is Z - 1, 
                tak(Z1,X,Y,A3), 
                tak(A1,A2,A3,A).

main :-
    unix( argv([H|_]) ),
    number_atom(N,H),
	R1 is N >> 3,
	R2 is N / 2,
    N2 is R2 - R1,
    N3 is N - N2,
    tak(N,N2,N3,_),
    statistics,
    statistics_jit.
