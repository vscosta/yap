
:- use_module(library(pfl)).

%:- set_pfl_flag(solver,ve).
:- set_pfl_flag(solver,bp), clpbn_bp:set_horus_flag(inf_alg,ve).
% :- set_pfl_flag(solver,fove).

:- yap_flag(write_strings, off).

friendly(P1, P2) :-
	person(P1),
	person(P2),
	P1 @> P2.

person(john).
person(maggie).
person(harry).
person(bill).
person(matt).
person(diana).
person(bob).
person(dick).
person(burr).
person(ann).

person @ 2.

markov smokes(P)::[t,f] , cancer(P)::[t,f] ; [0.1, 0.2, 0.3, 0.4] ; [person(P)].

markov friend(P1,P2)::[t,f], smokes(P1)::[t,f], smokes(P2)::[t,f] ; [0.5, 0.6, 0.7, 0.8] ; [friendly(P1, P2)].

?- smokes(person_0, t), smokes(person_1, t), friend(person_0, person_1, F).
