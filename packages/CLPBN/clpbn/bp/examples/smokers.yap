:- use_module(library(pfl)).

%:- set_solver(fove).
%:- set_solver(hve).
%:- set_solver(bp).
%:- set_solver(cbp).

:- yap_flag(write_strings, off).

:- multifile people/1.

people @ 5.

friendship(X,Y) :-
    people(X),
    people(Y).
%    X \== Y.

markov smokes(X)::[t,f] ; [1.0, 1.4] ; [people(X)].

markov asthma(X)::[t,f] ; [1.0, 2.3] ; [people(X)].

markov friends(X,Y)::[t,f] ; [1.0, 4.6] ; [friendship(X,Y)].

markov asthma(X)::[t,f], smokes(X)::[t,f] ; [1.5, 1.0, 1.5, 1.5] ; [people(X)].

markov asthma(X)::[t,f], friends(X,Y)::[t,f], smokes(Y)::[t,f] ;
    [1.1, 1.0, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1] ; [friendship(X,Y)].

% ?- smokes(p1,t), smokes(p2,t), friends(p1,p2,X)

