#!/home/tgomes/bin/yap -L --

:- use_module(library(lists)).

:- initialization(main).


main :-
    unix(argv(Args)),
    nth(1, Args, NP), % number of invitees
    nth(2, Args, NW), % number of workshops
    atomic_concat(['p', NP , 'w', NW, '.yap'], FileName),
    open(FileName, 'write', S),
    atom_number(NP, NP2),
    atom_number(NW, NW2),
    gen(S, NP2, NW2, 1),
    write(S, '\n'),
    close(S).


gen(_, NP, _, Count) :-
    Count > NP, !.
gen(S, NP, NW, Count) :-
    gen_workshops(S, Count, NW, 1),
    Count1 is Count + 1,
    gen(S, NP, NW, Count1).


gen_workshops(_, _, NW, Count) :-
    Count > NW, !.
gen_workshops(S, P, NW, Count) :-
    format(S, 'reg(p~w,w~w).~n', [P,Count]),
    Count1 is Count + 1,
    gen_workshops(S, P, NW, Count1).

