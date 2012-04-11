:- use_module(library(pfl)).

%:- clpbn_horus:set_solver(fove).
%:- clpbn_horus:set_solver(hve).
:- clpbn_horus:set_solver(bp).
%:- clpbn_horus:set_solver(cbp).

:- yap_flag(write_strings, off).

people @ 3.

markov attends(P)::[t,f], attr1::[t,f] ;  [0.11, 0.2, 0.3, 0.4] ; [people(P)].

markov attends(P)::[t,f], attr2::[t,f] ;  [0.1, 0.22, 0.3, 0.4] ; [people(P)].

markov attends(P)::[t,f], attr3::[t,f] ;  [0.1, 0.2, 0.33, 0.4] ; [people(P)].

markov attends(P)::[t,f], attr4::[t,f] ;  [0.1, 0.2, 0.3, 0.44] ; [people(P)].

markov attends(P)::[t,f], attr5::[t,f] ;  [0.1, 0.2, 0.3, 0.45] ; [people(P)].

markov attends(P)::[t,f], attr6::[t,f] ;  [0.1, 0.2, 0.3, 0.46] ; [people(P)].

markov attends(P)::[t,f], series::[t,f] ; [0.5, 0.6, 0.7, 0.87] ; [people(P)].

?- series(X).

