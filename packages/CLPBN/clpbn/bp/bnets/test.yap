
:- use_module(library(clpbn)).

:- set_clpbn_flag(solver, bp).


%     B     F
%      \   /
%       \ / 
%        A


a(A) :-
    b(B),
    f(F),
    a_table(ADist),
    { A = a with p([a1, a2, a3], ADist, [B, F]) }.



b(B) :-
    b_table(BDist),
    { B = b with p([b1, b2], BDist) }.

f(F) :-
    f_table(FDist),
    { F = f with p([f1, f2], FDist) }.

b_table([0.005, 0.995]).

f_table([0.03, 0.97]).

a_table([0.992, 0.99, 0.2, 0.003,
         0.008, 0.01, 0.8, 0.997,
         0.018, 0.21, 0.2, 0.927]).

