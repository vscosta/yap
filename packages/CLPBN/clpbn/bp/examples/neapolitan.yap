
:- use_module(library(clpbn)).

:- set_clpbn_flag(solver, bp).
%:- set_clpbn_flag(solver, jt).

%
%     B     F
%      \   /
%       \ / 
%        A
%


b(B) :-
    b_table(BDist),
    { B = b with p([b1, b2], BDist) }.

f(F) :-
    f_table(FDist),
    { F = f with p([f1, f2], FDist) }.

a(A) :-
    b(B),
    f(F),
    a_table(ADist),
    { A = a with p([a1, a2], ADist, [B, F]) }.


b_table([0.005, 0.995]).

f_table([0.03, 0.97]).

a_table([0.992, 0.99, 0.2, 0.003,
         0.008, 0.01, 0.8, 0.997]).

