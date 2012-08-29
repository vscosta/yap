
%parfactor(
%	[ability(P),grade(C,S), satisfaction(C,S,P)],
%        \phi = [....], 
%       [P,C,S],
%	[P \in [p1,p2,p4], C \in [c1,c3], S \in [s2,s3]]).
%	[S \= s2])


:- module(pfl_ground_factors, [
          generate_network/5,
	  f/3
	]).

:- use_module(library(bhash), [
          b_hash_new/1,
          b_hash_lookup/3,
	  b_hash_insert/4,
	  b_hash_to_list/2]).

:- use_module(library(lists), [
          delete/3,
	  nth0/3,
	  member/2]).

:- use_module(library(maplist)).

:- use_module(library(pfl), [
          factor/6,
          defined_in_factor/2,
	  skolem/2]).

:- use_module(library(clpbn/dists), [
          dist/4]).

:- dynamic currently_defined/1, queue/1, f/4.

%
% as you add query vars the network grows
% until you reach the last variable.
%
generate_network(QueryVars, QueryKeys, Keys, Factors, EList) :-
	init_global_search,
	attributes:all_attvars(AVars),
	b_hash_new(Evidence0),
	include_evidence(AVars, Evidence0, Evidence),
	b_hash_to_list(Evidence, EList0), list_to_evlist(EList0, EList),
	run_through_evidence(EList),
	run_through_query(Evidence, QueryVars, QueryKeys),
	propagate,
	collect(Keys, Factors).

%
% clean global stateq
%
init_global_search :-
	  retractall(queue(_)),
	  retractall(currently_defined(_)),
	  retractall(f(_,_,_)).

list_to_evlist([], []).
list_to_evlist([K-E|EList0], [K=E|EList]) :-
	list_to_evlist(EList0, EList).

include_evidence([], Evidence0, Evidence) :-
	findall(Sk=Var, pfl:evidence(Sk,Var), Evs),
	include_static_evidence(Evs, Evidence0, Evidence).
include_evidence([V|AVars], Evidence0, Evidence) :-
	clpbn:get_atts(V,[key(K),evidence(E)]), !,
	(
	    b_hash_lookup(K, E1, Evidence0)
	->
	    (E \= E1 -> throw(clpbn:incompatible_evidence(K,E,E1)) ; EvidenceI = Evidence0)
	;
	    b_hash_insert(Evidence0, K, E, EvidenceI)
	),
	include_evidence(AVars, EvidenceI, Evidence).
include_evidence([_|AVars], Evidence0, Evidence) :-
	include_evidence(AVars, Evidence0, Evidence).

include_static_evidence([], Evidence, Evidence).
include_static_evidence([K=E|AVars], Evidence0, Evidence) :-
	(
	    b_hash_lookup(K, E1, Evidence0)
	->
	    (E \= E1 -> throw(incompatible_evidence(K,E,E1)) ; EvidenceI = Evidence0)
	;
	    b_hash_insert(Evidence0, K, E, EvidenceI)
	),
	include_evidence(AVars, EvidenceI, Evidence).


run_through_query(_, [], []).
run_through_query(Evidence, [V|QueryVars], QueryKeys) :-
	clpbn:get_atts(V,[key(K)]),
	b_hash_lookup(K, _, Evidence), !,
	run_through_query(Evidence, QueryVars, QueryKeys).
run_through_query(Evidence, [V|QueryVars], [K|QueryKeys]) :-
	clpbn:get_atts(V,[key(K)]),
	queue_in(K),
	run_through_query(Evidence, QueryVars, QueryKeys).

collect(Keys, Factors) :-
	findall(K, currently_defined(K), Keys),
	findall(f(FType,FId,FKeys), f(FType,FId,FKeys), Factors).

run_through_evidence([]).
run_through_evidence([K=_|_]) :-
        queue_in(K),
        fail.
run_through_evidence([_|Ev]) :-
	run_through_evidence(Ev).

ground_all_keys([], _).
ground_all_keys([V|GVars], AllKeys) :-
	clpbn:get_atts(V,[key(Key)]), 
	\+ ground(Key), !,
	member(Key, AllKeys),
	ground_all_keys(GVars, AllKeys).
ground_all_keys([_V|GVars], AllKeys) :-
	ground_all_keys(GVars, AllKeys).


keys([], []).
keys([Var|QueryVars], [Key|QueryKeys]) :-
	clpbn:get_atts(Var,[key(Key)]),
	keys(QueryVars, QueryKeys).

initialize_evidence([]).
initialize_evidence([V|EVars]) :-
	clpbn:get_atts(V, [key(K)]),
	ground(K),
	queue_in(K),
	initialize_evidence(EVars).


%
% gets key K, and collects factors that  define it
queue_in(K) :-
	queue(K), !.
queue_in(K) :-
	writeln(+K),
	assert(queue(K)),
	fail.
queue_in(_).

propagate :-
	retract(queue(K)),!,
	do_propagate(K).
propagate.

do_propagate(K) :-
	%writeln(-K),
	\+ currently_defined(K),
	( ground(K) -> 	assert(currently_defined(K)) ; true),
	(
	  defined_in_factor(K, ParFactor),
	  add_factor(ParFactor, Ks)
	 *->
	  true
	;
	  throw(error(no_defining_factor(K)))
	)
	,
	member(K1, Ks),
	\+ currently_defined(K1),
	queue_in(K1),
	fail.
do_propagate(_K) :-
        propagate.

add_factor(factor(Type, Id, Ks, _, Phi, Constraints), Ks) :-
	( is_list(Phi) -> CPT = Phi ; call(user:Phi, CPT) ),
	run(Constraints), !,
	\+ f(Type, Id, Ks),
	assert(f(Type, Id, Ks)).

run([Goal|Goals]) :-
	call(user:Goal),
	run(Goals).
run([]).

