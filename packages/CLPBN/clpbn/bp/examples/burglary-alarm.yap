
:- use_module(library(clpbn)).

:- set_clpbn_flag(solver, vel).

%
%    B     E
%     \   /
%      \ / 
%       A 
%      / \
%     /   \
%    J     M
%


b(B) :-
    b_table(BDist),
    { B = b with p([b1, b2], BDist) }.

e(E) :-
    e_table(EDist),
    { E = e with p([e1, e2], EDist) }.

a(A) :-
    b(B),
    e(E),
    a_table(ADist),
    { A = a with p([a1, a2], ADist, [B, E]) }.

j(J):-
    a(A),
    j_table(JDist),
    { J = j with p([j1, j2], JDist, [A]) }.

m(M):-
    a(A),
    m_table(MDist),
    { M = m with p([m1, m2], MDist, [A]) }.
    

b_table([0.001, 0.009]).

e_table([0.002, 0.008]).

a_table([0.95, 0.94, 0.29, 0.001,
         0.05, 0.06, 0.71, 0.999]).

j_table([0.9, 0.05,
         0.1, 0.95]).

m_table([0.7, 0.01,
         0.3, 0.99]).

