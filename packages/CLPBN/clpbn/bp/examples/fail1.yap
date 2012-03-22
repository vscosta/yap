
:- use_module(library(pfl)).

:- set_clpbn_flag(solver,fove).


c(x1,y1,z1).
c(x1,y1,z2).
c(x1,y1,z3).
c(x1,y1,z4).
c(x1,y1,z5).
c(x1,y1,z6).
c(x1,y1,z7).

c(x1,y2,z1).
c(x1,y2,z2).
c(x1,y2,z3).
c(x1,y2,z4).
c(x1,y2,z5).
c(x1,y2,z6).
c(x1,y2,z7).

c(x2,y1,z1).
c(x2,y1,z2).
c(x2,y1,z3).
c(x2,y1,z4).
c(x2,y1,z5).
c(x2,y1,z6).
c(x2,y1,z7).

c(x2,y2,z1).
c(x2,y2,z2).
c(x2,y2,z3).
c(x2,y2,z4).
c(x2,y2,z5).
c(x2,y2,z6).
c(x2,y2,z7).


bayes p(X,Y,Z)::[t,f] , q(Y), s(Z) ; cpt ; [c(X,Y,Z)].

bayes q(Y)::[t,f] ; [0.50, 0.40] ; [c(_,Y,_)].

bayes s(Z)::[t,f] ; [0.33, 0.46] ; [c(_,_,Z)].

cpt([0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8]).


?- p(x1,y1,z1,X), s(z1,t), s(z3,f).

