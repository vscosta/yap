/*******************************************************

 Interface to Horus Lifted Solvers. Used by:
	- Generalized Counting First-Order Variable Elimination (GC-FOVE)
	- Lifted First-Order Belief Propagation
	- Lifted First-Order Knowledge Compilation

********************************************************/

:- module(clpbn_horus_lifted,
		[call_horus_lifted_solver/3,
		 check_if_horus_lifted_solver_done/1,
		 init_horus_lifted_solver/4,
		 run_horus_lifted_solver/3,
		 end_horus_lifted_solver/1
		]).

:- use_module(horus,
		[cpp_create_lifted_network/3,
		 cpp_set_parfactors_params/3,
		 cpp_run_lifted_solver/3,
		 cpp_free_lifted_network/1
		]).

:- use_module(library('clpbn/display'),
		[clpbn_bind_vals/3]).

:- use_module(library(pfl),
		[factor/6,
		 skolem/2,
		 get_pfl_parameters/3
		]).

:- use_module(library(maplist)).


call_horus_lifted_solver(QueryVars, AllVars, Output) :-
	init_horus_lifted_solver(_, AllVars, _, State),
	run_horus_lifted_solver(QueryVars, Solutions, State),
	clpbn_bind_vals(QueryVars, Solutions, Output),
	end_horus_lifted_solver(State).


init_horus_lifted_solver(_, AllVars, _, state(Network, DistIds)) :-
	get_parfactors(Parfactors),
	get_observed_keys(AllVars, ObservedKeys),
	%writeln(network:(parfactors=Parfactors, evidence=ObservedKeys)), nl,
	cpp_create_lifted_network(Parfactors, ObservedKeys, Network),
	maplist(get_dist_id, Parfactors, DistIds0),
	sort(DistIds0, DistIds).


run_horus_lifted_solver(QueryVars, Solutions, state(Network, _DistIds)) :-
	maplist(get_query_keys, QueryVars, QueryKeys),
	%maplist(get_pfl_parameters, DistIds, _, DistsParams),
	%cpp_set_parfactors_params(Network, DistIds, DistsParams),
	cpp_run_lifted_solver(Network, QueryKeys, Solutions).


end_horus_lifted_solver(state(Network, _)) :-
	cpp_free_lifted_network(Network).

%
% Enumerate all parfactors and enumerate their domain as tuples.
%
:- table get_parfactors/1.

get_parfactors(Factors) :-
	findall(F, is_factor(F), Factors).


is_factor(pf(Id, Ks, Rs, Phi, Tuples)) :-
	factor(_Type, Id, Ks, Vs, Table, Constraints),
	maplist(get_range, Ks, Rs),
	Table \= avg,
	gen_table(Table, Phi),
	all_tuples(Constraints, Vs, Tuples).


get_range(K, Range) :-
	skolem(K, Domain),
	length(Domain, Range).


gen_table(Table, Phi) :-
	( is_list(Table) -> Phi = Table ; call(user:Table, Phi) ).


all_tuples(Constraints, Tuple, Tuples) :-
	findall(Tuple, run(Constraints), Tuples0),
	sort(Tuples0, Tuples).


run([]).
run([Goal|Constraints]) :-
	user:Goal,
	run(Constraints).


get_dist_id(pf(DistId, _, _, _, _), DistId).


get_observed_keys([], []).
get_observed_keys(V.AllAttVars, [K:E|ObservedKeys]) :-
	clpbn:get_atts(V,[key(K)]),
	( clpbn:get_atts(V,[evidence(E)]) ; pfl:evidence(K,E) ), !,
	get_observed_keys(AllAttVars, ObservedKeys).
get_observed_keys(_V.AllAttVars, ObservedKeys) :-
	get_observed_keys(AllAttVars, ObservedKeys).


get_query_keys([], []).
get_query_keys(V.AttVars, K.Ks) :-
	clpbn:get_atts(V,[key(K)]), !,
	get_query_keys(AttVars, Ks).

