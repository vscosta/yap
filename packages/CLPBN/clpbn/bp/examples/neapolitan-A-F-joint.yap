
:- use_module(library(clpbn)).

:- set_clpbn_flag(solver, bp).

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

d(D) :-
    a(A),
    f(F),
    d_table(DDist),
    { D = d with p([d1, d2, d3, d4], DDist, [A, F]) }.
 

b_table([0.005, 0.995]).

f_table([0.03, 0.97]).

a_table([0.992, 0.99, 0.2, 0.003,
         0.008, 0.01, 0.8, 0.997]).

d_table([1.0, 0.0, 0.0, 0.0,
         0.0, 1.0, 0.0, 0.0,
         0.0, 0.0, 1.0, 0.0,
         0.0, 0.0, 0.0, 1.0]).

%d_table([0.997, 0.001, 0.001, 0.001,
%         0.001, 0.997, 0.001, 0.001,
%         0.001, 0.001, 0.997, 0.001,
%         0.001, 0.001, 0.001, 0.997]).

%d_table([0.15, 0.1, 0.7, 0.5,
%         0.25, 0.3, 0.2, 0.25,
%         0.3, 0.15, 0.35, 0.2,
%         0.3, 0.4, 0.2, 0.1]).

