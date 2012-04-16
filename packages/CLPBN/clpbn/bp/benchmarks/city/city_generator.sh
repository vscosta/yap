#!/home/tiago/bin/yap -L --


:- initialization(main).


main :-
    unix(argv([H])),
    generate_town(H).


generate_town(N) :-
    atomic_concat(['city_', N, '.yap'], FileName),
    open(FileName, 'write', S),
    atom_number(N, N2),
    generate_people(S, N2, 4),
    write(S, '\n'),
    generate_query(S, N2, 4),
    write(S, '\n'),
    close(S).


generate_people(S, N, Counting) :-
    Counting > N, !.
generate_people(S, N, Counting) :-
    format(S, 'people(p~w, nyc).~n', [Counting]),
    Counting1 is Counting + 1,
    generate_people(S, N, Counting1).


generate_query(S, N, Counting) :- 
    Counting > N, !.
generate_query(S, N, Counting) :- !,
    format(S, 'ev(descn(p~w, t)).~n', [Counting]),
    Counting1 is Counting + 1,
    generate_query(S, N, Counting1).

