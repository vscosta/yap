/*******************************************************

 Interface to Horus Ground Solvers. Used by:
	- Variable Elimination
	- Belief Propagation
	- Counting Belief Propagation
 
********************************************************/

:- module(clpbn_horus_ground,
		[call_horus_ground_solver/6,
		 check_if_horus_ground_solver_done/1,
		 init_horus_ground_solver/5,
		 run_horus_ground_solver/3,
		 end_horus_ground_solver/1
		]).

:- use_module(horus,
		[cpp_create_ground_network/4,
		 cpp_set_factors_params/2,
		 cpp_run_ground_solver/3,
		 cpp_free_ground_network/1,
		 cpp_set_vars_information/2
		]).

:- use_module(library('clpbn/numbers')).

:- use_module(library('clpbn/display'),
		[clpbn_bind_vals/3]).

:- use_module(library(pfl),
		[skolem/2]).

:- use_module(library(charsio),
		[term_to_atom/2]).

:- use_module(library(maplist)).


call_horus_ground_solver(QueryVars, QueryKeys, AllKeys, Factors, Evidence, Output) :-
	init_horus_ground_solver(QueryKeys, AllKeys, Factors, Evidence, State),
	run_horus_ground_solver([QueryKeys], Solutions, State),
	clpbn_bind_vals([QueryVars], Solutions, Output),
	end_horus_ground_solver(State).
	

run_horus_ground_solver(QueryKeys, Solutions, state(Network,Hash,Id)) :-
	%maplist(get_dists_parameters, DistIds, DistsParams),
	%cpp_set_factors_params(Network, DistsParams),
	lists_of_keys_to_ids(QueryKeys, QueryIds, Hash, _, Id, _),
	cpp_run_ground_solver(Network, QueryIds, Solutions).


init_horus_ground_solver(QueryKeys, AllKeys, Factors, Evidence, state(Network,Hash4,Id4)) :-
	get_factors_type(Factors, Type),
	keys_to_numbers(AllKeys, Factors, Evidence, Hash4, Id4, FactorIds, EvidenceIds),
	cpp_create_ground_network(Type, FactorIds, EvidenceIds, Network),
	%writeln(network:(Type, FactorIds, EvidenceIds, Network)), writeln(''),
	maplist(get_var_information, AllKeys, StatesNames),
	maplist(term_to_atom, AllKeys, KeysAtoms),
	cpp_set_vars_information(KeysAtoms, StatesNames).


end_horus_ground_solver(state(Network,_Hash,_Id)) :-
	cpp_free_ground_network(Network).


get_factors_type([f(bayes, _, _)|_], bayes) :- ! .
get_factors_type([f(markov, _, _)|_], markov) :- ! .


get_var_information(_:Key, Domain) :- !,
	skolem(Key, Domain).
get_var_information(Key, Domain) :-
	skolem(Key, Domain).

