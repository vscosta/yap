:- use_module(library(pfl)).

:- set_solver(fove).
%:- set_solver(hve).
%:- set_solver(bp).
%:- set_solver(cbp).

:- yap_flag(write_strings, off).

:- clpbn_horus:set_horus_flag(verbosity,5).

people(p1,p1).
people(p1,p2).
people(p2,p1).
people(p2,p2).

markov p(A,A)::[t,f] ; [1.0,4.5] ; [people(A,_)].

markov p(A,B)::[t,f] ; [1.0,4.5] ; [people(A,B)].

?- p(p1,p1,X).
