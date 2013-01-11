/* Learn distribution for a sprinkler database. */

:- ['../sprinkler.pfl'].

:- use_module(library(clpbn/learning/em)).

%:- set_em_solver(ve).
%:- set_em_solver(hve).
%:- set_em_solver(bdd).
%:- set_em_solver(bp).
%:- set_em_solver(cbp).

data(t,t,t,t).
data(_,t,_,t).
data(t,t,f,f).
data(t,t,f,t).
data(t,_,_,t).
data(t,f,t,t).
data(t,t,f,t).
data(t,_,f,f).
data(t,t,f,f).
data(f,f,t,t).
data(t,t,_,f).
data(t,f,f,t).
data(t,f,t,t).

main :-
	findall(X, scan_data(X), L),
	em(L, 0.01, 10, CPTs, Lik),
	writeln(Lik:CPTs).

scan_data([cloudy(C),sprinkler(S),rain(R),wet_grass(W)]) :-
	data(C, S, R, W).

