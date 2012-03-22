
:- use_module(library(pfl)).

%:- set_clpbn_flag(solver,ve).
%:- set_clpbn_flag(solver,bp), clpbn_bp:set_horus_flag(inf_alg,ve).
:- set_clpbn_flag(solver,fove).

c(x1,y1).
%c(x1,y1).
%c(x2,y2).

bayes hot(Y)::[t,f] ; [0.2, 0.4] ; [c(_,Y)].

bayes attends(X)::[t,f] , hot(Y) ; [0.1, 0.2, 0.3, 0.4] ; [c(X,Y)].

bayes series::[t,f] , attends(X) ; [0.5, 0.6, 0.7, 0.8] ; [c(X,_)].


