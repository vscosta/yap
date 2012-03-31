:- source.
:- style_check(all).
:- yap_flag(unknown,error).
:- yap_flag(write_strings,on).
:- use_module(library(clpbn)).
:- set_clpbn_flag(solver, bp).
:- [-schema].

lives(_joe, nyc).

run_query(Guilty) :- 
	guilty(joe, Guilty),
	witness(nyc, t),
	runall(X, ev(X)).


runall(G, Wrapper) :-
	findall(G, Wrapper, L),
	execute_all(L).


execute_all([]).
execute_all(G.L) :-
	call(G),
	execute_all(L).


ev(descn(p2, t)).
ev(descn(p3, t)).
