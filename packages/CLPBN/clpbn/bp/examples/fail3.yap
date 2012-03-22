
:- use_module(library(pfl)).

:- set_clpbn_flag(solver,fove).


t(ann).
t(dave).

bayes p(X)::[t,f] ; [0.1, 0.3] ; [t(X)].

p(ann,t).

?- p(dave,X).


