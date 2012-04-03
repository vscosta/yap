
:- use_module(library(pfl)).

:- set_clpbn_flag(solver,fove).


c(x1,y1,z1).
c(x1,y1,z2).
c(x2,y2,z1).
c(x3,y2,z1).

bayes p(X)::[t,f] ; [0.2, 0.4] ; [c(X,_,_)].

bayes q(Y)::[t,f] ; [0.5, 0.6] ; [c(_,Y,_)].

bayes s(Z)::[t,f] , p(X) , q(Y) ; [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8] ; [c(X,Y,Z)].

% bayes series::[t,f] , attends(X) ; [0.5, 0.6, 0.7, 0.8] ; [c(X,_)].
