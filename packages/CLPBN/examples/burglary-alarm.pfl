:- use_module(library(pfl)).

%:- set_solver(fove).
%:- set_solver(hve).
%:- set_solver(bp).
%:- set_solver(cbp).

:- yap_flag(write_strings, off).

bayes burglary::[t,f] ; [0.001, 0.999] ; [].

bayes earthquake::[t,f] ; [0.002, 0.998]; [].

bayes alarm::[t,f], burglary, earthquake ;
      [0.95, 0.94, 0.29, 0.001, 0.05, 0.06, 0.71, 0.999] ;
      [].

bayes john_calls::[t,f], alarm ; [0.9, 0.05, 0.1, 0.95] ; [].

bayes mary_calls::[t,f], alarm ; [0.7, 0.01, 0.3, 0.99] ; [].

% ?- john_calls(J), mary_calls(t).

