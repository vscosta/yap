
:- use_module(library(clpbn)).

:- set_clpbn_flag(solver, bp).

r(R) :- r_cpt(RCpt),
        { R = r with p([r1, r2], RCpt) }.

t(T) :- t_cpt(TCpt),
        { T = t with p([t1, t2], TCpt) }.

a(A) :- r(R), t(T), a_cpt(ACpt),
        { A = a with p([a1, a2], ACpt, [R, T]) }.

j(J) :- a(A), j_cpt(JCpt),
        { J = j with p([j1, j2], JCpt, [A]) }.

m(M) :- a(A), m_cpt(MCpt),
        { M = m with p([m1, m2], MCpt, [A]) }.
    
r_cpt([0.001, 0.999]).
t_cpt([0.002, 0.998]).
a_cpt([0.95, 0.94, 0.29, 0.001,
       0.05, 0.06, 0.71, 0.999]).
j_cpt([0.9, 0.05,
       0.1, 0.95]).
m_cpt([0.7, 0.01,
       0.3, 0.99]).

