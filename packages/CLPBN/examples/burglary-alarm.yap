:- use_module(library(pfl)).

%:- set_solver(fove).
%:- set_solver(hve).
%:- set_solver(bp).
%:- set_solver(cbp).

:- yap_flag(write_strings, off).

bayes burglary::[b1,b2] ; [0.001, 0.999] ; [].

bayes earthquake::[e1,e2] ; [0.002, 0.998]; [].

bayes alarm::[a1,a2], burglary, earthquake ;
      [0.95, 0.94, 0.29, 0.001, 0.05, 0.06, 0.71, 0.999] ;
      [].

bayes john_calls::[j1,j2], alarm ; [0.9, 0.05, 0.1, 0.95] ; [].

bayes mary_calls::[m1,m2], alarm ; [0.7, 0.01, 0.3, 0.99] ; [].

% ?- john_calls(J), mary_calls(m1).

