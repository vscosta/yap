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
		 cpp_set_factors_params/3,
		 cpp_run_ground_solver/3,
		 cpp_free_ground_network/1,
		 cpp_set_vars_information/2
		]).

:- use_module(library('clpbn/numbers'),
		[lists_of_keys_to_ids/6,
		 keys_to_numbers/7
		]).

:- use_module(library('clpbn/display'),
		[clpbn_bind_vals/3]).

:- use_module(library(pfl),
		[get_pfl_parameters/3,
		 skolem/2
		]).

:- use_module(library(charsio),
		[term_to_atom/2]).

:- use_module(library(maplist)).


call_horus_ground_solver(QueryVars, QueryKeys, AllKeys, Factors, Evidence,
		Output) :-
	init_horus_ground_solver(QueryKeys, AllKeys, Factors, Evidence, State),
	run_horus_ground_solver([QueryKeys], Solutions, State),
	clpbn_bind_vals([QueryVars], Solutions, Output),
	end_horus_ground_solver(State).


init_horus_ground_solver(_QueryKeys, AllKeys, Factors, Evidence,
		state(Network,Hash,Id,DistIds)) :-
	factors_type(Factors, Type),
	keys_to_numbers(AllKeys, Factors, Evidence, Hash, Id, FacIds, EvIds),
	%writeln(network:(type=Type, factors=FacIds, evidence=EvIds)), nl,
	cpp_create_ground_network(Type, FacIds, EvIds, Network),
	%maplist(term_to_atom, AllKeys, VarNames),
	%maplist(get_domain, AllKeys, Domains),
	%cpp_set_vars_information(VarNames, Domains),
    maplist(get_dist_id, FacIds, DistIds0),
    sort(DistIds0, DistIds).


run_horus_ground_solver(QueryKeys, Solutions,
		state(Network,Hash,Id, _DistIds)) :-
	lists_of_keys_to_ids(QueryKeys, QueryIds, Hash, _, Id, _),
	%maplist(get_pfl_parameters, _DistIds, _, DistParams),
	%cpp_set_factors_params(Network, _DistIds, DistParams),
	cpp_run_ground_solver(Network, QueryIds, Solutions).


end_horus_ground_solver(state(Network,_Hash,_Id, _DistIds)) :-
	cpp_free_ground_network(Network).


factors_type([f(bayes, _, _)|_], bayes) :- ! .
factors_type([f(markov, _, _)|_], markov) :- ! .


get_dist_id(fn(_, _, _, DistId, _), DistId).


get_domain(_:Key, Domain) :- !,
	skolem(Key, Domain).
get_domain(Key, Domain) :-
	skolem(Key, Domain).

