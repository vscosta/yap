
%parfactor(
%	[ability(P),grade(C,S), satisfaction(C,S,P)],
%        \phi = [....], 
%       [P,C,S],
%	[P \in [p1,p2,p4], C \in [c1,c3], S \in [s2,s3]]).
%	[S \= s2])


:- module(clpbn_ground_factors, [
          generate_network/5]).

:- use_module(library(bhash), [
          b_hash_new/1,
          b_hash_lookup/3,
	  b_hash_insert/4]).

:- use_module(library(lists), [
          delete/3,
	  member/2]).

:- use_module(library(pfl), [
          factor/6,
          defined_in_factor/2,
	  skolem/2]).

:- use_module(library(clpbn/dists), [
          dist/4]).

:- dynamic currently_defined/1, f/3.

generate_network(QueryVars, QueryKeys, Keys, Factors, Evidence) :-
	attributes:all_attvars(AVars),
	check_for_evidence(AVars, EVars, Evidence),
	retractall(currently_defined(_)),
	retractall(f(_,_,_)),
	initialize_evidence(EVars),
	keys(QueryVars, QueryKeys),
	run_through_factors(QueryVars),
	run_through_factors(EVars),
	findall(K, currently_defined(K), Keys),
	findall(f(FType,FKeys,FCPT), f(FType,FKeys,FCPT), Factors).

check_for_evidence(V.AVars, V.EVars, (K=E).Evidence) :-
	clpbn:get_atts(V,[key(K),evidence(E)]), !,
	check_for_evidence(AVars, EVars, Evidence).
check_for_evidence(_V.AVars, EVars, Evidence) :-
	check_for_evidence(AVars, EVars, Evidence).
check_for_evidence([], [], []).

keys([], []).
keys([Var|QueryVars], [Key|QueryKeys]) :-
	clpbn:get_atts(Var,[key(Key)]),
	keys(QueryVars, QueryKeys).

run_through_factors([]).
run_through_factors([Var|_QueryVars]) :-
          clpbn:get_atts(Var,[key(K)]),
	  find_factors(K),
	  fail.
run_through_factors([_|QueryVars]) :-
	  run_through_factors(QueryVars).

initialize_evidence([]).
initialize_evidence([V|EVars]) :-
	clpbn:get_atts(V, [key(K)]),
	assert(currently_defined(K)),
	initialize_evidence(EVars).

%
% gets key K, and collects factors that  define it
find_factors(K) :-
	assert(currently_defined(K)),
	defined_in_factor(K, ParFactor),
	add_factor(ParFactor, Ks),
	member(K1, Ks),
	\+ currently_defined(K1),
	find_factors(K1).

add_factor(factor(Type, _Id, Ks, _, CPT, Constraints), Ks) :-
	F = f(Type, Ks, CPT),
	run(Constraints),
	\+ f(Type, Ks, CPT),
	assert(F).

run([Goal|Goals]) :-
	call(user:Goal),
	run(Goals).
run([]).

