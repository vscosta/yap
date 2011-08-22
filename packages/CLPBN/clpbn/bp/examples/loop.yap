
:- use_module(library(clpbn)).

:- set_clpbn_flag(solver, bp).

%
%        A     E
%       / \   /
%      /   \ /
%     B     C 
%      \   /
%       \ / 
%        D
%

a(A) :-
    a_table(ADist),
    { A = a with p([a1, a2], ADist) }.

b(B) :-
    a(A),
    b_table(BDist),
    { B = b with p([b1, b2], BDist, [A]) }.

c(C) :-
    a(A),
    c_table(CDist),
    { C = c with p([c1, c2], CDist, [A]) }.

d(D) :-
    b(B),
    c(C),
    d_table(DDist),
    { D = d with p([d1, d2], DDist, [B, C]) }.

e(E) :-
    e_table(EDist),
    { E = e with p([e1, e2], EDist) }.


a_table([0.005, 0.995]).

b_table([0.02, 0.97,
         0.88, 0.03]).

c_table([0.55, 0.94,
         0.45, 0.06]).

d_table([0.192, 0.98, 0.33, 0.013,
         0.908, 0.02, 0.77, 0.987]).

e_table([0.055, 0.945]).

