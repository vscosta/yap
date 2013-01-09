
:- module(pfl_ground_factors,
		[generate_network/5,
		 f/3
		]).

:- use_module(library(bhash),
		[b_hash_new/1,
		 b_hash_lookup/3,
		 b_hash_insert/4,
		 b_hash_to_list/2
		]).

:- use_module(library(lists),
		[member/2]).

:- use_module(library(maplist)).

:- use_module(library(atts)).

:- use_module(library(pfl),
		[factor/6,
		 defined_in_factor/2,
		 skolem/2
		]).

:- use_module(library(clpbn/aggregates),
		[avg_factors/5]).

:- use_module(library(clpbn/dists),
		[dist/4]).

:- dynamic currently_defined/1, queue/1, f/4.

%
% as you add query vars the network grows
% until you reach the last variable.
%
generate_network(QueryVars, QueryKeys, Keys, Factors, EList) :-
	init_global_search,
	attributes:all_attvars(AVars),
	b_hash_new(Evidence0),
	foldl(include_evidence,AVars, Evidence0, Evidence1),
	static_evidence(Evidence1, Evidence),
	b_hash_to_list(Evidence, EList0),
	maplist(pair_to_evidence,EList0, EList),
	maplist(queue_evidence, EList),
	foldl(run_through_query(Evidence), QueryVars, [], QueryKeys),
	propagate,
	collect(Keys, Factors).

%
% clean global stateq
%
init_global_search :-
	retractall(queue(_)),
	retractall(currently_defined(_)),
	retractall(f(_,_,_)).

pair_to_evidence(K-E, K=E).

include_evidence(V, Evidence0, Evidence) :-
	clpbn:get_atts(V,[key(K),evidence(E)]), !,
	(
	  b_hash_lookup(K, E1, Evidence0)
	->
	  (E \= E1 -> throw(clpbn:incompatible_evidence(K,E,E1)) ; Evidence = Evidence0)
	;
	  b_hash_insert(Evidence0, K, E, Evidence)
	).
include_evidence(_, Evidence, Evidence).

static_evidence(Evidence0, Evidence) :-
	findall(Sk=Var, pfl:evidence(Sk,Var), Evs),
	foldl(include_static_evidence, Evs, Evidence0, Evidence).

include_static_evidence(K=E, Evidence0, Evidence) :-
	(
	  b_hash_lookup(K, E1, Evidence0)
	->
	  (E \= E1 -> throw(incompatible_evidence(K,E,E1)) ; Evidence = Evidence0)
	;
	  b_hash_insert(Evidence0, K, E, Evidence)
	).


queue_evidence(K=_) :-
	queue_in(K).

run_through_query(Evidence, V, QueryKeys, QueryKeys) :-
	clpbn:get_atts(V,[key(K)]),
	b_hash_lookup(K, _, Evidence), !.
run_through_query(_Evidence, V, QueryKeys, [K|QueryKeys]) :-
	clpbn:get_atts(V,[key(K)]),
	queue_in(K).

collect(Keys, Factors) :-
	findall(K, currently_defined(K), Keys),
	findall(f(FType,FId,FKeys), f(FType,FId,FKeys), Factors).

%
% gets key K, and collects factors that  define it
queue_in(K) :-
	queue(K), !.
queue_in(K) :-
%	writeln(q+K),
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
	( ground(K) -> assert(currently_defined(K)) ; true),
	(
	  defined_in_factor(K, ParFactor),
	  add_factor(ParFactor, Ks)
	*->
	  true
	;
	  throw(error(no_defining_factor(K)))
	),
	member(K1, Ks),
	\+ currently_defined(K1),
	queue_in(K1),
	fail.
do_propagate(_K) :-
	propagate.

add_factor(factor(Type, Id, Ks, _, _Phi, Constraints), NKs) :-
%	writeln(+Ks),
	(
	  Ks = [K,Els], var(Els)
	->
	  % aggregate factor
	  once(run(Constraints)),
	  avg_factors(K, Els, 0.0, NewKeys, NewId),
	  NKs = [K|NewKeys]
	;
	  run(Constraints),
	  NKs = Ks,
	  Id = NewId
	),
	(
	  f(Type, NewId, NKs)
	->
	  true
	;
	  assert(f(Type, NewId, NKs))
	).

run([Goal|Goals]) :-
	call(user:Goal),
	run(Goals).
run([]).

