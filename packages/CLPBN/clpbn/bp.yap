
/************************************************

  Belief Propagation in CLP(BN)
 
**************************************************/

:- module(clpbn_bp,
          [bp/3,
           check_if_bp_done/1,
           set_solver_parameter/2,
           use_log_space/0,
           init_bp_solver/4,
           run_bp_solver/3,
           finalize_bp_solver/1
          ]).


:- use_module(library('clpbn/dists'),
          [dist/4,
           get_dist_domain/2,
           get_dist_domain_size/2,
           get_dist_params/2
          ]).


:- use_module(library('clpbn/display'),
         [clpbn_bind_vals/3]).

:- use_module(library('clpbn/aggregates'),
	      [check_for_agg_vars/2]).

:- use_module(library(atts)).

:- use_module(library(charsio)).

:- load_foreign_files(['horus'], [], init_predicates).

:- attribute id/1.

:- dynamic network_counting/1.


check_if_bp_done(_Var).

network_counting(0).


:- set_solver_parameter(run_mode, normal).
%:- set_solver_parameter(run_mode, convert).
%: -set_solver_parameter(run_mode, compress).

:- set_solver_parameter(schedule, seq_fixed).
%:- set_solver_parameter(schedule, seq_random).
%:- set_solver_parameter(schedule, parallel).
%:- set_solver_parameter(schedule, max_residual).

:- set_solver_parameter(accuracy, 0.0001).

:- set_solver_parameter(max_iter, 1000).

:- set_solver_parameter(always_loopy_solver, false).

% :- use_log_space.


bp([[]],_,_) :- !.
bp([QueryVars], AllVars, Output) :-
	init_bp_solver(_, AllVars, _, BayesNet),
	run_bp_solver([QueryVars], LPs, BayesNet),
	finalize_bp_solver(BayesNet),
	clpbn_bind_vals([QueryVars], LPs, Output).


init_bp_solver(_, AllVars0, _, bp(BayesNet, DistIds, AllParFactors)) :-
	check_for_agg_vars(AllVars0, AllVars),
	%inc_network_counting,
%writeln_clpbn_vars(AllVars),
	process_ids(AllVars, 0, DistIds0),
%	generate_parfactors(AllVars, AllParFactors),
%writeln(AllParFactors),
	get_vars_info(AllVars, VarsInfo),
	sort(DistIds0, DistIds),
	%(network_counting(0) -> writeln(vars:VarsInfo) ; true),
	%(network_counting(0) -> writeln(distsids:DistIds) ; true),
	create_network(VarsInfo, BayesNet).
	%get_extra_vars_info(AllVars, ExtraVarsInfo),
	%(network_counting(0) -> writeln(extra:ExtraVarsInfo) ; true),
	%set_extra_vars_info(BayesNet, ExtraVarsInfo).

writeln_clpbn_vars(Var.AVars) :-
	clpbn:get_atts(Var, [key(Key),dist(Dist,Parents)]),
	parents_to_keys(Parents, Keys),
	writeln(Var:Key:Dist:Keys),
	writeln_clpbn_vars(AVars).
writeln_clpbn_vars([]).

parents_to_keys([], []).
parents_to_keys(Var.Parents, Key.Keys) :-
	clpbn:get_atts(Var, [key(Key)]),
	parents_to_keys(Parents, Keys).

generate_parfactors(AllVars, ParFactors) :-
	generate_factors(AllVars, Factors),
%writeln(Factors),
	% sort factors by distribution
%	sort(Factors, DistFactors),
%writeln(DistFactors),
	group(DistFactors, ParFactors).
%writeln(ParFactors).

generate_factors(Var.AllVars, f(Dist,[Var|Parents]).AllFactors) :-
	clpbn:get_atts(Var, [dist(Dist,Parents)]),
	generate_factors(AllVars, AllFactors).
generate_factors([], []).

group([], []).
group(f(Dist,Vs).DistFactors, phi(Dist,NConstraints,Domain).ParFactors) :-
	number(Dist),
	grab_similar_factors(Dist, Vs, f(Dist,Vs).DistFactors, RemainingDistFactors, Constraints),
	simplify_constraints(Constraints, NConstraints, Domain), !,
	group(RemainingDistFactors, ParFactors).

group(f(Dist,Vs).DistFactors, phi(Dist,Constraints,[]).ParFactors) :-
	grab_similar_factors(Dist, Vs, f(Dist,Vs).DistFactors, RemainingDistFactors, Constraints),
	group(RemainingDistFactors, ParFactors).

simplify_constraints([[1=El]|Constraints], [NEl], [in(1,NEl,Domain)]) :-
	functor(El,Name,1), !,
	functor(NEl,Name,1),
	constraints_to_domain(1,[[1=El]|Constraints],Domain0),
	sort(Domain0, Domain).
simplify_constraints(Constraints, NewConstraints, Ds) :-
        Constraints = [Constraint|_],
	generate_domains(Constraint, Constraints, Ds), !,
	normalize_constraints(Ds, Constraints, NewConstraints).
simplify_constraints(Constraints, Constraints, []).

normalize_constraints(Ds, Constraints, [T|GeneralizedConstraints]) :-
	unique(Ds, I, T, RemDs), !,
	remove_i(Constraints, I, ConstraintsI),
	normalize_constraints(RemDs, ConstraintsI, GeneralizedConstraints).
normalize_constraints(Ds, Constraints, [(S1,S2)|GeneralizedConstraints]) :-
	equal(Ds, I, J, RemDs, S1, S2),
	arg(1,S1,V),
	arg(1,S2,V),
%writeln(start:Ds:I:J),
	remove_eqs(Constraints, I, J, ConstraintsI), !,
	normalize_constraints(RemDs, ConstraintsI, GeneralizedConstraints).
normalize_constraints(_Ds, Constraints, []) :-
	Constraints = [[_]|_], !.
normalize_constraints(_, Constraints, Constraints).

unique([in(I,T,[_])|Ds], I, T, Ds).	
unique([D|Ds], I, T, D.NewDs) :-
	unique(Ds, I, T, NewDs).	

equal([in(I,S1,Vals)|Ds], I, J, Ds, S1, S2) :-
	equal2(Ds, Vals, J, S2), !.
equal([D|Ds], I, J, D.NewDs, S1, S2) :-
	equal(Ds, I, J, NewDs, S1, S2).	

equal2([in(J,S2,Vals)|Ds], Vals, J, S2).
equal2([D|Ds], Vals, J, S2) :-
	equal2(Ds, Vals, J, S2).	

remove_i([], _I, []).
remove_i(C.Constraints, I, NewC.ConstraintsI) :-
	remove_ic(C,I,NewC),
	remove_i(Constraints, I, ConstraintsI).

remove_ic([I=_|C], I, C) :- !.
remove_ic(El.C, I, El.NewC) :-
	remove_ic(C, I, NewC).

remove_eqs([], _I, _J, []).
remove_eqs(C.Constraints, I, J, NewC.ConstraintsI) :-
	remove_eqs2(C, I, J, NewC),
	remove_eqs(Constraints, I, J, ConstraintsI).

remove_eqs2([I=V|C], I, J, C) :- !,
	arg(1,V,A),
	check_match(C, J, A).
remove_eqs2(El.C, I, J, El.NewC) :-
	remove_eqs2(C, I, J, NewC).

check_match([J=V1|C], J, V) :- !,
	arg(1,V1,V).
check_match(El.C, J, V) :-
	check_match(C, J, V).


generate_domains([], _Constraints, []).
generate_domains([I=El|Constraint], Constraints, in(I,NEl,Domain).Ds) :-
	functor(El,Name,1), !,
	functor(NEl,Name,1),
	constraints_to_domain(I,Constraints,Domain0),
	sort(Domain0, Domain),
	generate_domains(Constraint, Constraints, Ds).	


constraints_to_domain(_,[],[]).
constraints_to_domain(I,[Constraint|Constraints],El.Domain) :-
	add_constraint_to_domain(I, Constraint, El),
	constraints_to_domain(I,Constraints,Domain).

add_constraint_to_domain(I, [I=El|_], A) :- !,
	arg(1, El, A).
add_constraint_to_domain(I, _.Constraint, El) :-
	add_constraint_to_domain(I, Constraint, El).


grab_similar_factors(Dist, Vs, f(Dist,DVs).DistFactors, RemainingDistFactors, Constraint.Constraints) :-
	grab_similar_factor(DVs, 1, Constraint), !,
	grab_similar_factors(Dist, Vs, DistFactors, RemainingDistFactors, Constraints).
grab_similar_factors(_Dist, _Vs, DistFactors, DistFactors, []).

grab_similar_factor([], _Arg, []).
grab_similar_factor(V.VDVs, Arg, (Arg=Key).Constraint) :-
	clpbn:get_atts(V,key(Key)),
	Arg1 is Arg+1,
	grab_similar_factor(VDVs, Arg1, Constraint).


process_ids([], _, []).
process_ids([V|Vs], VarId0, [DistId|DistIds]) :-
	clpbn:get_atts(V, [dist(DistId, _)]), !,
	put_atts(V, [id(VarId0)]),
	VarId is VarId0 + 1,
	process_ids(Vs, VarId, DistIds).
process_ids([_|Vs], VarId, DistIds) :-
	process_ids(Vs, VarId, DistIds).


get_vars_info([], []).
get_vars_info([V|Vs], [var(VarId, DSize, Ev, ParentIds, DistId)|VarsInfo]) :-
	clpbn:get_atts(V, [dist(DistId, Parents)]), !,
	get_atts(V, [id(VarId)]),
	get_dist_domain_size(DistId, DSize),
	get_evidence(V, Ev),
	vars2ids(Parents, ParentIds),
	get_vars_info(Vs, VarsInfo).
get_vars_info([_|Vs], VarsInfo) :-
	get_vars_info(Vs, VarsInfo).


vars2ids([], []).
vars2ids([V|QueryVars], [VarId|Ids]) :-
	get_atts(V, [id(VarId)]),
	vars2ids(QueryVars, Ids).


get_evidence(V, Ev) :-
	clpbn:get_atts(V, [evidence(Ev)]), !.
get_evidence(_V, -1). % no evidence !!!


get_extra_vars_info([], []).
get_extra_vars_info([V|Vs], [v(VarId, Label, Domain)|VarsInfo]) :-
	get_atts(V, [id(VarId)]), !,
	clpbn:get_atts(V, [key(Key),dist(DistId, _)]),
	term_to_atom(Key, Label),
	get_dist_domain(DistId, Domain0),
	numbers2atoms(Domain0, Domain),
	get_extra_vars_info(Vs, VarsInfo).
get_extra_vars_info([_|Vs], VarsInfo) :-
	get_extra_vars_info(Vs, VarsInfo).


numbers2atoms([], []).
numbers2atoms([Atom|L0], [Atom|L]) :-
	atom(Atom), !,
	numbers2atoms(L0, L).
numbers2atoms([Number|L0], [Atom|L]) :-
	number_atom(Number, Atom),
	numbers2atoms(L0, L).


run_bp_solver(QVsL0, LPs, bp(BayesNet, DistIds, _)) :-
	get_dists_parameters(DistIds, DistsParams),
	set_parameters(BayesNet, DistsParams),
	process_query_list(QVsL0, QVsL),
	%(network_counting(0) -> writeln(qvs:QVsL) ; true),
	run_solver(BayesNet, QVsL, LPs).


process_query_list([], []).
process_query_list([[V]|QueryVars], [VarId|Ids]) :- !,
	get_atts(V, [id(VarId)]),
	process_query_list(QueryVars, Ids).
process_query_list([Vs|QueryVars], [VarIds|Ids]) :-
	vars2ids(Vs, VarIds),
	process_query_list(QueryVars, Ids).


get_dists_parameters([],[]).
get_dists_parameters([Id|Ids], [dist(Id, Params)|DistsInfo]) :-
	get_dist_params(Id, Params),
	get_dists_parameters(Ids, DistsInfo).


finalize_bp_solver(bp(BayesNet, _, _)) :-
	free_bayesian_network(BayesNet).


inc_network_counting :-
	retract(network_counting(Count0)),
	Count is Count0 + 1,
	assert(network_counting(Count)).

