#!/home/tgomes/bin/yap -L --

:- use_module(library(lists)).

:- initialization(main).


main :-
    unix(argv(Args)),
    nth(1, Args, NP), % number of invitees
    nth(2, Args, NA), % number of attributes
    atomic_concat(['p', NP , 'attrs', NA, '.yap'], FileName),
    open(FileName, 'write', S),
    atom_number(NP, NP2),
    atom_number(NA, NA2),
    generate_people(S, NP2, 1),
    write(S, '\n'),
    generate_attrs(S, NA2, 7),
    write(S, '\n'),
    close(S).


generate_people(S, N, Counting) :-
    Counting > N, !.
generate_people(S, N, Counting) :-
    format(S, 'person(p~w).~n', [Counting]),
    Counting1 is Counting + 1,
    generate_people(S, N, Counting1).


generate_attrs(S, N, Counting) :-
    Counting > N, !.
generate_attrs(S, N, Counting) :-
    %format(S, 'person(p~w).~n', [Counting]),
    format(S, 'markov attends(P)::[t,f], attr~w::[t,f]', [Counting]),
    format(S, '; [0.7, 0.3, 0.3, 0.3] ; [person(P)].~n',[]),
    Counting1 is Counting + 1,
    generate_attrs(S, N, Counting1).

