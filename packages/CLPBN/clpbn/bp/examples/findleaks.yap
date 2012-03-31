
:- use_module(library(pfl)).

:- set_clpbn_flag(solver,fove).

c1(x1).
c1(x2).
c1(x3).

c2(x2).
c2(x3).
c2(x4).

markov p::[t,f] ; [0.2, 0.4] ; [].
markov q::[t,f] ; [0.2, 0.4] ; [].
markov s::[t,f] , p::[t,f] ; [0.2, 0.4, 0.5, 0.1] ; [].

