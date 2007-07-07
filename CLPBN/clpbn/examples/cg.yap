
%
% adapted from Hendrik Blockeel's ILP04 paper.
%

:- use_module(library(clpbn)).

cg(X,1,C):-
    father(Y,X),
    cg(Y,1,C1),cg(Y,2,C2),
    parent_cpt(cg(X,1), C1, C2, C).

cg(X,2,C):-
    mother(Y,X),
    cg(Y,1,C1),cg(Y,2,C2),
    parent_cpt(cg(X,2), C1, C2, C).



cg(f,X,C) :-
    prior_cpt(cg(f,X),C).

cg(m,X,C) :-
    prior_cpt(cg(m,X),C).


prior_cpt(CKEY, C) :-
    { C = CKEY with p([p,w], [0.5,0.5])}.

parent_cpt(CKEY, C1, C2, C) :-
    { C = CKEY with p([p,w], [   1,0.5,0.5,0.0,
                               0.0,0.5,0.5,  1],[C1,C2])}.

father(f,s).
mother(m,s).
