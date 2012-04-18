:- use_module(library(pfl)).

:- clpbn_horus:set_solver(fove).
%:- clpbn_horus:set_solver(hve).
%:- clpbn_horus:set_solver(bp).
%:- clpbn_horus:set_solver(cbp).

:- yap_flag(write_strings, off).


friends(P1, P2) :-
    people(P1),
    people(P2),
    P1 @< P2.

people @ 3.

markov smokes(P)::[t,f], cancer(P)::[t,f] ; [0.1, 0.2, 0.3, 0.4] ; [people(P)].

markov friend(P1,P2)::[t,f], smokes(P1)::[t,f], smokes(P2)::[t,f] ;
    [0.5, 0.6, 0.7, 0.8, 0.5, 0.6, 0.7, 0.8] ; [friends(P1, P2)].

% ?- smokes(p1, t), smokes(p2, f), friend(p1, p2, X).

