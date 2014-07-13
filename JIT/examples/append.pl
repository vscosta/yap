:- initialization(main).

:- use_module(library(random)).

generate(L, N) :-
    generate(L, [], N).

generate(L, L, 0) :- !.
generate(L, X, N) :-
    A is N - 1,
    random(0, 1000000, B),
    generate(L, [B|X], A).

append_([],L,L).
append_([X|L1],L2,[X|L3]) :-
    append_(L1,L2,L3).

main :-
    unix( argv([H|_]) ), number_atom(N,H),

    generate(List, N),

    append_(List, [], _),

    statistics,
    statistics_jit,
    halt.
