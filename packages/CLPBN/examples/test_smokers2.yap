:- use_module(library(pfl)).

:- set_solver(fove).
%:- set_solver(hve).
%:- set_solver(bp).
%:- set_solver(cbp).

:- yap_flag(write_strings, off).

:- clpbn_horus:set_horus_flag(verbosity,5).
:- clpbn_horus:set_horus_flag(use_logarithms,true).

:- multifile people/1.

people @ 12.

people(X,Y) :-
    people(X),
    people(Y).

markov smokes(X)::[t,f]; [1.0, 4.0]; [people(X)].

markov asthma(X)::[t,f]; [2.5, 9.0] ; [people(X)].

markov friends(X,Y)::[t,f]; [1.0, 14.0] ; [people(X,Y)].

markov asthma(X)::[t,f], smokes(X)::[t,f]; [4.5, 4.3, 2.0, 1.45] ; [people(X)].

markov asthma(X)::[t,f], friends(X,Y)::[t,f], smokes(Y)::[t,f];
    [2.1, 3.3, 7.4, 3.9, 1.6, 5.0, 4.0, 3.0] ; [people(X,Y)].

?- friends(p1,p2,X).

