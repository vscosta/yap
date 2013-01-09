#! /home/tgomes/bin/yap -L --


:- initialization(main).


main :-
    unix(argv([N])),
    atomic_concat(['city', N, '.yap'], FileName),
    open(FileName, 'write', S),
    atom_number(N, N2),
    generate_people(S, N2, 1),
    write(S, '\n'),
    generate_evidence(S, N2, 1),
    write(S, '\n'),
    close(S).


generate_people(S, N, Counting) :-
    Counting > N, !.
generate_people(S, N, Counting) :-
    format(S, 'person(p~w, nyc).~n', [Counting]),
    Counting1 is Counting + 1,
    generate_people(S, N, Counting1).


generate_evidence(S, N, Counting) :- 
    Counting > N, !.
generate_evidence(S, N, Counting) :- !,
    format(S, 'ev(descn(p~w, t)).~n', [Counting]),
    Counting1 is Counting + 1,
    generate_evidence(S, N, Counting1).

