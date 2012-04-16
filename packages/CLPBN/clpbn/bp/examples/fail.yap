
:- use_module(library(pfl)).

:- set_pfl_flag(solver,fove).
%:- set_pfl_flag(solver,bp), clpbn_horus:set_horus_flag(inf_alg,ve).
%:- set_pfl_flag(solver,bp), clpbn_horus:set_horus_flag(inf_alg,bp).
%:- set_pfl_flag(solver,bp), clpbn_horus:set_horus_flag(inf_alg,cbp).


t(ann).
t(dave).

% p(ann,t).

markov p(X)::[t,f] ; [0.1, 0.3] ; [t(X)].

% use standard Prolog queries: provide evidence first.

?- p(ann,t), p(ann,X).
% ?- p(ann,X).

