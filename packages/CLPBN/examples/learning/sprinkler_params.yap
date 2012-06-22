% learn distribution for school database.

:- ['../sprinkler.pfl'].

:- use_module(library(clpbn/learning/em)).

%% data(t,t,t,t).
data(t,f,_,t).
%% data(_,t,_,t).
%% data(t,t,f,f).
%% data(t,t,f,t).
%% data(t,_,_,t).
%% data(t,f,t,t).
%% data(t,t,f,t).
%% data(t,_,f,f).
%% data(t,t,f,f).
%% data(f,f,t,t).
%% data(t,t,_,f).
%% data(t,f,f,t).
%% data(t,f,t,t).

%:- clpbn:set_clpbn_flag(em_solver,gibbs).
%:- clpbn:set_clpbn_flag(em_solver,jt).
%:- clpbn:set_clpbn_flag(em_solver,ve).
:- clpbn:set_clpbn_flag(em_solver,bp).

timed_main :-
	statistics(runtime, _),
	main(Lik),
	statistics(runtime, [T,_]),
	format('Took ~d msec and Lik ~3f~n',[T,Lik]).

main(Lik) :-
	findall(X,scan_data(X),L),
        em(L,0.01,10,_,Lik).

scan_data(example([wet_grass(W),sprinkler(S),rain(R),cloudy(C)])) :-
	data(W, S, R, C).


