:- source.
:- style_check(all).
:- yap_flag(unknown,error).
:- yap_flag(write_strings,on).
:- use_module(library(clpbn)).
%:- set_clpbn_flag(solver, bp).

:- set_clpbn_flag(solver,fove).

:- [-parschema].

run_query(Guilty) :- 
    guilty(joe, Guilty),
    witness(nyc, t).
    %runall(X, ev(X)).


runall(G, Wrapper) :-
    findall(G, Wrapper, L),
    execute_all(L).


execute_all([]).
execute_all(G.L) :-
    call(G),
    execute_all(L).


%ev(descn(p2, t)).
%ev(descn(p3, t)).


city(nyc).
city(oporto).

people(joe,nyc).
people(p2,nyc).
people(p3,nyc).
%people(p4,nyc).
%people(p5,nyc).
%people(p6,nyc).
%people(p7,nyc).
%people(p8,nyc).
%people(p9,nyc).



