
:- use_module(library(clpbn)).

:- set_clpbn_flag(solver, bp).


%        H
%       / \
%      /   \
%     B     L
%      \   / \
%       \ /   \
%        F     C


h(H) :-
    h_table(HDist),
    { H = h with p([h1, h2], HDist) }.


b(B) :-
    h(H),
    b_table(BDist),
    { B = b with p([b1, b2], BDist, [H]) }.


l(L) :-
    h(H),
    l_table(LDist),
    { L = l with p([l1, l2], LDist, [H]) }.


f(F) :-
    b(B),
    l(L),
    f_table(FDist),
    { F = f with p([f1, f2], FDist, [B, L]) }.


c(C) :-
    l(L),
    c_table(CDist),
    { C = c with p([c1, c2], CDist, [L]) }.


h_table([0.2, 0.8]).

b_table([0.25, 0.05,
         0.75, 0.95]).

l_table([0.003, 0.00005,
         0.997, 0.99995]).

f_table([0.75, 0.1, 0.5, 0.05,
         0.25, 0.9, 0.5, 0.95]).

c_table([0.6, 0.02,
         0.4, 0.98]).

