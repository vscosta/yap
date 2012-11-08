:- use_module(library(pfl)).

%:- set_solver(lve).
%:- set_solver(hve).
%:- set_solver(bp).
%:- set_solver(cbp).

:- yap_flag(write_strings, off).

:- multifile people/1.

people @ 5.

markov attends(P)::[t,f], attr1::[t,f] ;  [0.7, 0.3, 0.3, 0.3] ; [people(P)].

markov attends(P)::[t,f], attr2::[t,f] ;  [0.7, 0.3, 0.3, 0.3] ; [people(P)].

markov attends(P)::[t,f], attr3::[t,f] ;  [0.7, 0.3, 0.3, 0.3] ; [people(P)].

markov attends(P)::[t,f], attr4::[t,f] ;  [0.7, 0.3, 0.3, 0.3] ; [people(P)].

markov attends(P)::[t,f], attr5::[t,f] ;  [0.7, 0.3, 0.3, 0.3] ; [people(P)].

markov attends(P)::[t,f], attr6::[t,f] ;  [0.7, 0.3, 0.3, 0.3] ; [people(P)].

markov attends(P)::[t,f], series::[t,f] ; [0.501, 0.499, 0.499, 0.499] ; [people(P)].

% ?- series(X).

