#!/home/tgomes/bin/yap -L --


:- initialization(main).


main :-
    unix(argv([N])),
    atomic_concat(['pop', N, '.yap'], FileName),
    open(FileName, 'write', S),
    atom_number(N, N2),
    generate_people(S, N2, 4),
    write(S, '\n'),
    close(S).


generate_people(S, N, Counting) :-
    Counting > N, !.
generate_people(S, N, Counting) :-
    format(S, 'person(p~w).~n', [Counting]),
    Counting1 is Counting + 1,
    generate_people(S, N, Counting1).

