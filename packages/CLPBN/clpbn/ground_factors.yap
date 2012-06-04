
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
	  nth0/3,
	  member/2]).

:- use_module(library(pfl), [
          factor/6,
          defined_in_factor/2,
	  skolem/2]).

:- use_module(library(clpbn/dists), [
          dist/4]).

:- dynamic currently_defined/1, f/4.

generate_network(QueryVars0, QueryKeys, Keys, Factors, Evidence) :-
	attributes:all_attvars(AVars),
	keys(QueryVars0, QueryKeys0),
	check_for_evidence(AVars, EVars, QueryKeys0, QueryVars0, Evidence),
	check_for_extra_bindings(QueryVars0, QueryVars, QueryKeys0, QueryKeys),
	do_network(QueryVars, EVars, Keys, Factors).

do_network([], _, _, _) :- !.
do_network(QueryVars, EVars, Keys, Factors) :-
	retractall(currently_defined(_)),
	retractall(f(_,_,_,_)),
	run_through_factors(QueryVars),
	run_through_factors(EVars),
	findall(K, currently_defined(K), Keys),
	ground_all_keys(QueryVars, Keys),
	ground_all_keys(EVars, Keys),
	findall(f(FType,FId,FKeys,FCPT), f(FType,FId,FKeys,FCPT), Factors).

run_through_factors([]).
run_through_factors([Var|_QueryVars]) :-
        clpbn:get_atts(Var,[key(K)]),
        find_factors(K),
        fail.
run_through_factors([_|QueryVars]) :-
	run_through_factors(QueryVars).


ground_all_keys([], _).
ground_all_keys([V|GVars], AllKeys) :-
	clpbn:get_atts(V,[key(Key)]), 
	\+ ground(Key), !,
	member(Key, AllKeys),
	ground_all_keys(GVars, AllKeys).
ground_all_keys([_V|GVars], AllKeys) :-
	ground_all_keys(GVars, AllKeys).


%
% look for attributed vars with evidence (should also search the DB)
% verifiy if the evidence overlaps with query
% bind query if so.
%
check_for_evidence(V.AVars, V.EVars, QueryKeys, QueryVars, (K=E).Evidence) :-
	clpbn:get_atts(V,[key(K),evidence(E)]), !,
	check_for_evidence_in_query(K, QueryKeys, QueryVars, E),
	check_for_evidence(AVars, EVars, QueryKeys, QueryVars, Evidence).
% ignore no evidence vars
check_for_evidence(_V.AVars, EVars, QueryKeys, QueryVars, Evidence) :-
	check_for_evidence(AVars, EVars, QueryKeys, QueryVars, Evidence).
check_for_evidence([], [], _, _, []).

%
% do we still have free query variables?
%
check_for_extra_bindings([], [], [], []).
check_for_extra_bindings([V|QueryVars0], QueryVars, [_|QueryKeys0], QueryKeys) :-
	nonvar(V),!,
	check_for_extra_bindings(QueryVars0, QueryVars, QueryKeys0, QueryKeys).
check_for_extra_bindings([V|QueryVars0], [V|QueryVars], [K|QueryKeys0], [K|QueryKeys]) :-
	check_for_extra_bindings(QueryVars0, QueryVars, QueryKeys0, QueryKeys).


check_for_evidence_in_query(Key, [Key|QueryKeys], [V|QueryVars], E) :- !,
	skolem(Key, Dom),
	nth0(E, Dom, Val),
	V = Val,
	check_for_evidence_in_query(Key, QueryKeys, QueryVars, E).
check_for_evidence_in_query(Key, [_|QueryKeys], [_|QueryVars], E) :-
	check_for_evidence_in_query(Key, QueryKeys, QueryVars, E).
check_for_evidence_in_query(_Key, [], [], _E).

keys([], []).
keys([Var|QueryVars], [Key|QueryKeys]) :-
	clpbn:get_atts(Var,[key(Key)]),
	keys(QueryVars, QueryKeys).

initialize_evidence([]).
initialize_evidence([V|EVars]) :-
	clpbn:get_atts(V, [key(K)]),
	ground(K),
	assert(currently_defined(K)),
	initialize_evidence(EVars).

%
% gets key K, and collects factors that  define it
find_factors(K) :-
	\+ currently_defined(K),
	( ground(K) -> 	assert(currently_defined(K)) ; true),
	defined_in_factor(K, ParFactor),
	add_factor(ParFactor, Ks),
	member(K1, Ks),
	\+ currently_defined(K1),
	find_factors(K1).

add_factor(factor(Type, Id, Ks, _, Phi, Constraints), Ks) :-
	F = f(Type, Id, Ks, CPT),
	( is_list(Phi) -> CPT = Phi ; call(user:Phi, CPT) ),
	run(Constraints),
	\+ f(Type, Id, Ks, CPT),
	assert(F).

run([Goal|Goals]) :-
	call(user:Goal),
	run(Goals).
run([]).

