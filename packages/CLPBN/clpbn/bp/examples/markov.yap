
:- use_module(library(pfl)).

:- set_clpbn_flag(solver,fove).

t(ann).
t(dave).
t(lucy).
t(john).
t(kristian).
t(sriraam).

markov p(X)::[t,f] ; [0.1, 0.3] ; [t(X)].

markov q(X)::[t,f], r(X)::[t,f] ; [0.9, 0.5, 0.5, 0.1] ; [t(X)].

markov q(X)::[t,f], p(X)::[t,f] ; [0.9, 0.5, 0.5, 0.1] ; [t(X)].

p(ann,t).

?- p(dave,X).


