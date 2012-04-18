:- use_module(library(pfl)).

:- clpbn_horus:set_solver(fove).
%:- clpbn_horus:set_solver(hve).
%:- clpbn_horus:set_solver(bp).
%:- clpbn_horus:set_solver(cbp).

:- yap_flag(write_strings, off).

people @ 5.

markov attends(P)::[t,f], attr1::[t,f] ;  [0.7, 0.3, 0.3, 0.3] ; [people(P)].

markov attends(P)::[t,f], attr2::[t,f] ;  [0.7, 0.3, 0.3, 0.3] ; [people(P)].

markov attends(P)::[t,f], attr3::[t,f] ;  [0.7, 0.3, 0.3, 0.3] ; [people(P)].

markov attends(P)::[t,f], attr4::[t,f] ;  [0.7, 0.3, 0.3, 0.3] ; [people(P)].

markov attends(P)::[t,f], attr5::[t,f] ;  [0.7, 0.3, 0.3, 0.3] ; [people(P)].

markov attends(P)::[t,f], attr6::[t,f] ;  [0.7, 0.3, 0.3, 0.3] ; [people(P)].

markov attends(P)::[t,f], series::[t,f] ; [0.501, 0.499, 0.499, 0.499] ; [people(P)].

% ?- series(X).

