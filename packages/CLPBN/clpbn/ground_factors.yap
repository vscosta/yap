
%parfactor(
%	[ability(P),grade(C,S), satisfaction(C,S,P)],
%        \phi = [....], 
%       [P,C,S],
%	[P \in [p1,p2,p4], C \in [c1,c3], S \in [s2,s3]]).
%	[S \= s2])


:- module(clpbn_ground_factors, [
          generate_bn/2,
          ground_parfactors/1]).

:- use_module(library(bhash), [
          b_hash_new/1,
          b_hash_lookup/3,
	  b_hash_insert/4]).

:- use_module(library(pfl), [
          factor/5,
	  skolem/2]).

:- use_module(library(clpbn/dists), [
          dist/4]).


%
% generate a CLP(BN) network that can be run in CLP(BN).
%
generate_bn(QueryVars, AllAttVars) :-
	attributes:all_attvars(AVars),
	b_hash_new(H0),
	check_for_evidence(AVars, EVars),
	run_through_factors(QueryVars, H0, H1, AllAttVars, IVars),
	run_through_factors(EVars, H1, _HF, IVars, []).

check_for_evidence(V.AVars, V.EVars) :-
	clpbn:get_atts(V,[evidence(_E)]), !,
	check_for_evidence(AVars, EVars).
check_for_evidence(_V.AVars, EVars) :-
	check_for_evidence(AVars, EVars).
check_for_evidence([], []).

run_through_factors([], H, H) --> [].
run_through_factors(V.Vars, H0, HF) -->
	{ clpbn:get_atts(V,[key(K)]), 
	  b_hash_lookup(K,V,H0)
        }, !,
	run_through_factors(Vars, H0, HF).
run_through_factors(V.Vars, H0, HF) -->
	% it is a new clpbn variable 
	[V],
	{
          % should already have a key
          clpbn:get_atts(V,[key(K)]),
	  % insert it into a table of seen variables
	  b_hash_insert(H0,K,V,HI),
	  construct_clpbn_node(K, V, HI, MoreVars, Vars)
        },
	run_through_factors(MoreVars, HI, HF).

% aggregates are special.
construct_clpbn_node(K, V, HI) -->
	% and get the corresponding factor
	{ factor(Id, [K|Ks], _, avg, Constraints) }, !,
	{
          skolem(K, Domain),
	  dist(avg(Domain, Parents), DistId, K, Parents),
          clpbn:put_atts(V,[dist(DistId,Parents)]), 
	  % we also need to set the constraints
	  % this should set all the keys to rights
	  run(Constraints)
        },
	% now let's look up parents and set up the graph
	run_bayesian_factor(Ks, HI, Parents, []).
construct_clpbn_node(K, V, HI) -->
	{
	  % and get the corresponding factor
	  factor(Id, [K|Ks], _, _Phi, Constraints),
	  factor_to_dist(Id, DistId),
	  % and the dist constraint
          clpbn:put_atts(V,[dist(DistId,Parents)]), 
	  % we also need to set the constraints
	  % this should set all the keys to rights
	  run(Constraints)
        },
	% now let's look up parents and set up the graph
	run_bayesian_factor(Ks, HI, Parents, []).

factor_to_dist(Id, NewId) :-
	factor(Id, [K|Ks], _, Phi, _Constraints),
	skolem(K, Domain),
	( is_list(Phi)
          -> 
	  CPT = Phi
          ;
	  call(user:Phi, CPT)
        ),
	keys_to_sizes(Ks, Szs),
	dist(p(Domain, CPT, Szs), NewId, K, Szs).
	
keys_to_sizes([], []).
keys_to_sizes(K.Ks, Sz.Szs) :-
	skolem(K, Domain),
	length(Domain, Sz),
	keys_to_sizes(Ks, Szs).

run([]).
run(Goal.Constraints) :-
	user:Goal, !,
	run(Constraints).

run_bayesian_factor([], _H, Vs, Vs) --> [].
run_bayesian_factor(K.Ks, H, Vs, Vs0) -->
	run_var(K, H, Vs, Vs1),
	run_bayesian_factor(Ks, H, Vs1, Vs0).

%
% this function returns a list of *new* variables
%
% collection of random variables
run_var(avg(Els), H, Vs, Vs0) --> !,
	run_vars(Els, H, Vs, Vs0).
% existing random variable
run_var(K, H, V.Vs, Vs) -->
	{  b_hash_lookup(K,V,H) }, !.
% new random variable
run_var(K, _H, V.Vs, Vs) -->
	[V],
	{
          clpbn:put_atts(V,[key(K)])
        }.

run_vars([], _H, Vs, Vs) --> [].
run_vars(K.Els, H, Vs, Vs0) -->
	run_var(K, H, Vs, VsI),
	run_vars(Els, H, VsI, Vs0).

ground_parfactors(ParFactors) :-
	findall(Factor, factor(Factor), SourceFactors),
	run_all_parfactors(SourceFactors, ParFactors).

factor(Factor) :- 
	user:parfactor(Factor).
factor(Factor) :- 
	user:bayes(Factor).

run_all_parfactors([], []).
run_all_parfactors(Source.SourceFactors, Factor.ParFactors) :-
	run_parfactors(Source, Factor),
	run_all_parfactors(SourceFactors, ParFactors).

run_parfactors((Formula ; Phi ; ConstraintGenerator), parfactor(Formula, Phi, FV, Domain, NewConstraints)) :-
	term_variables(Formula, FreeVars),
	FV =.. fv(FreeVars),
	evaluate_constraints(FV, ConstraintGenerator, NewConstraints, Domain).

evaluate_constraints(FreeVars, Constraint.ConstraintGenerators, NC, Domain) :-
	functor(FreeVars, fv, NOf),
	setof(FreeVars, user:Constraint, Bindings),
	run_free_vars(0, NOf, FreeVars, Bindings, Domain, Domain0),
	get_list_of_conditions(Domain, 0, N, Conditions),
	add_extra_constraints(N, Conditions, Bindings, NC, NC0),
	evaluate_constraints(FreeVars, ConstraintGenerators, NC0, Domain0).
evaluate_constraints(_FreeVars, [], []).

run_free_vars(N, N, _FreeVars, _Bindings) --> !.
run_free_vars(I0, N, FreeVars, Bindings) -->
	{ I is I0+1,
	 arg(I, FreeVars, V),
	 Bindings = B._,
	 arg(I, B, C), ground(C)
        }, !,
	{ setof(C, check_val(Bindings, I, C), Dom) },
	[domain(I,V,Dom)],
	run_free_vars(I, N, FreeVars, Bindings).
run_free_vars(I0, N, FreeVars, Bindings) -->
	I is I0+1,
	run_free_vars(I, N, FreeVars, Bindings).

add_extra_constraints(0, [], _Bindings) --> !.
add_extra_constraints(1, _Conditions, _Bindings) --> !.
add_extra_constraints(N, Conditions, Bindings) -->
	{ extract_unique(Conditions, NewConditions) }, !,
	{ N1 is N-1 },
	add_extra_constraints(N1, NewConditions, Bindings).
add_extra_constraints(N, [dom(I1,V1,Dom1),dom(I2,V2,Dom2)|Conditions], Bindings) -->
	{ length(Dom1, Sz), length(Dom2, Sz) }, !,
	{ N1 is N-2 },
	{ generate_map(Bindings, I1, I2, Mapping) },
	[map(V1,V2,Mapping)],
	add_extra_constraints(N1, dom(I1,V1,Dom1).Conditions, Bindings).
add_extra_constraints(_N, Conditions, Bindings) -->
	[or(Vs,Or)],
	{ gather_vs(Conditions, Vs, Indices),
	  generate(Bindings, Indices, Or) }.

% domain is a singleton constant
extract_unique(domain(_,_,[_]).Conditions, Conditions) :- !.
extract_unique(_.Conditions, NewConditions) :-
	extract_unique(Conditions, NewConditions).

get_list_of_conditions([], N, N, []).
get_list_of_conditions(Dom._, N, N, _Conditions) :- 
	var(Dom), !.
get_list_of_conditions(Dom.Domain, I0, N, Dom.Conditions) :-
	I is I0+1,
	get_list_of_conditions(Domain, I, N, Conditions).

check_val(B._Bindings, I, C) :-
	arg(I, B, C).
check_val(_.Bindings, I, C) :-
	check_val(Bindings, I, C).
	
generate_map(B.Bindings, I1, I2, [[A1|A2]|Mapping]) :-
	arg(I1, B, A1),
	arg(I2, B, A2),
	generate_map(Bindings, I1, I2, Mapping).

gather_vs([], [], []).
gather_vs(domain(I,V,_).Conditions, V.Vs, I.Indices) :-
	gather_vs(Conditions, Vs, Indices).

generate([], _, []).
generate(B.Bindings, Indices, O.Or) :-
	generate_el(B, Indices, O),
	generate(Bindings, Indices, Or).

generate_el(_B, [], []).
generate_el(B, I.Indices, A.O) :-
	arg(I, B, A),
	generate_el(B, Indices, O).
