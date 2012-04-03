
:- use_module(library(pfl)).

:- set_pfl_flag(solver,fove).
%:- set_pfl_flag(solver,fove).


t(ann).
t(dave).

% p(ann,t).

bayes p(X)::[t,f] ; [0.1, 0.3] ; [t(X)].

% use standard Prolog queries: provide evidence first.

?- p(dave,t), p(ann,X).
% ?- p(ann,X).

