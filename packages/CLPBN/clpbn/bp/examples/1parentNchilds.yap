
:- use_module(library(clpbn)).

:- set_clpbn_flag(solver, bp).

%
%         R 
%       / | \
%      /  |  \ 
%     A   B   C
%


r(R) :-
    { R = r with p([t, f], [0.35, 0.65]) }.

a(A) :-
    r(R),
    child_dist(R,Dist),
    { A = a with Dist }.

b(B) :-
    r(R),
    child_dist(R,Dist),
    { B = b with Dist }.

c(C) :-
    r(R),
    child_dist(R,Dist),
    { C = c with Dist }.


child_dist(R, p([t, f], [0.3, 0.4, 0.25, 0.05], [R])).

