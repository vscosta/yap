
:- use_module(library(pfl)).

%:- set_clpbn_flag(solver,ve).
%:- set_clpbn_flag(solver,bp), clpbn_bp:set_horus_flag(inf_alg,ve).
:- set_clpbn_flag(solver,fove).

c(p1).
c(p2).
c(p3).
c(p4).
c(p5).


markov attends(P)::[t,f] , attr1::[t,f] ; [0.1, 0.2, 0.3, 0.4] ; [c(P)].

markov attends(P)::[t,f] , attr2::[t,f] ; [0.1, 0.2, 0.3, 0.4] ; [c(P)].

markov attends(P)::[t,f] , attr3::[t,f] ; [0.1, 0.2, 0.3, 0.4] ; [c(P)].

markov attends(P)::[t,f] , attr4::[t,f] ; [0.1, 0.2, 0.3, 0.4] ; [c(P)].

markov attends(P)::[t,f] , attr5::[t,f] ; [0.1, 0.2, 0.3, 0.4] ; [c(P)].

markov attends(P)::[t,f] , attr6::[t,f] ; [0.1, 0.2, 0.3, 0.4] ; [c(P)].

markov attends(P)::[t,f], series::[t,f] ; [0.5, 0.6, 0.7, 0.8] ; [c(P)].

%:- clpbn_horus:set_horus_flag(use_logarithms,true).

?- series(X).

