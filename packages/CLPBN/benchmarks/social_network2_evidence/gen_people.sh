#!/home/tgomes/bin/yap -L --

:- use_module(library(lists)).
:- use_module(library(random)).


:- initialization(main).

main :-
    unix(argv(Args)),
    nth(1, Args, EV), % percentage of evidence
    nth(2, Args, NP), % number of individuals
    atomic_concat(['ev', EV, 'p', NP, '.yap'], FileName),
    open(FileName, 'write', S),
    atom_number(EV, EV2),
    atom_number(NP, NP2),
    EV3 is EV2 / 100.0,
    generate_people(S, NP2, 4),
    write(S, '\n'),
    write(S, 'query(X) :- '),
    generate_evidence(S, NP2, EV3, 4),
    write(S, 'friends(p1,p2,X).\n'),
    close(S).


generate_people(S, N, Counting) :-
    Counting > N, !.
generate_people(S, N, Counting) :-
    format(S, 'person(p~w).~n', [Counting]),
    Counting1 is Counting + 1,
    generate_people(S, N, Counting1).


generate_evidence(S, N, Ev, Counting) :-
    Counting > N, !.
generate_evidence(S, N, Ev, Counting) :-
    random(X),
    (
      X < Ev
       ->
      random(Y),
     (Y > 0.5 -> Val = t ; Val = f),
     format(S, 'smokes(p~w,~w),', [Counting,Val])
       ;
     true
    ),
    Counting1 is Counting + 1,
    generate_evidence(S, N, Ev, Counting1).

