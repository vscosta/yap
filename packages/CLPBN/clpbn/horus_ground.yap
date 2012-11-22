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
           run_horus_ground_solver/4,
           finalize_horus_ground_solver/1
          ]).

:- use_module(horus,
          [cpp_create_ground_network/4,
           cpp_set_factors_params/2,
           cpp_run_ground_solver/3,
           cpp_set_vars_information/2,
           cpp_free_ground_network/1,
	   set_solver/1
          ]).

:- use_module(library('clpbn/dists'),
          [dist/4,
           get_dist_domain/2,
           get_dist_domain_size/2,
           get_dist_params/2
          ]).

:- use_module(library('clpbn/display'),
          [clpbn_bind_vals/3]).

:- use_module(library(clpbn/numbers)).

:- use_module(library(charsio),
          [term_to_atom/2]).

:- use_module(library(pfl),
          [skolem/2]).

:- use_module(library(maplist)).

:- use_module(library(lists)).

:- use_module(library(atts)).

:- use_module(library(bhash)).


call_horus_ground_solver(QueryVars, QueryKeys, AllKeys, Factors, Evidence, Output) :-
  init_horus_ground_solver(QueryKeys, AllKeys, Factors, Evidence, Network),
  run_solver(Network, [QueryKeys], Solutions),
  clpbn_bind_vals([QueryVars], Solutions, Output),
  finalize_horus_ground_solver(Network).


init_horus_ground_solver(QueryKeys, AllKeys, Factors, Evidence, ground(Network,Hash4,Id4)) :-
  get_factors_type(Factors, Type),
  keys_to_numbers(AllKeys, Factors, Evidence, Hash4, Id4, FactorIds, EvidenceIds),
  cpp_create_ground_network(Type, FactorIds, EvidenceIds, Network),
  %writeln(network:(Type, FactorIds, EvidenceIds, Network)), writeln(''),
  maplist(get_var_information, AllKeys, StatesNames),
  maplist(term_to_atom, AllKeys, KeysAtoms),
  cpp_set_vars_information(KeysAtoms, StatesNames).


run_horus_ground_solver(_QueryVars, Solutions, horus(GKeys, Keys, Factors, Evidence), Solver) :- 
  set_solver(Solver),
  call_horus_ground_solver_for_probabilities(GKeys, Keys, Factors, Evidence, Solutions).


% TODO this is not beeing called!
finalize_horus_ground_solver(ground(Network,_Hash,_Id)) :-
  cpp_free_ground_network(Network).


run_solver(ground(Network,Hash,Id), QueryKeys, Solutions) :-
  %get_dists_parameters(DistIds, DistsParams),
  %cpp_set_factors_params(Network, DistsParams),
  lists_of_keys_to_ids(QueryKeys, QueryIds, Hash, _, Id, _),
  cpp_run_ground_solver(Network, QueryIds, Solutions).


get_factors_type([f(bayes, _, _)|_], bayes) :- ! .
get_factors_type([f(markov, _, _)|_], markov) :- ! .


get_var_information(_:Key, Domain) :- !,
    skolem(Key, Domain).
get_var_information(Key, Domain) :-
    skolem(Key, Domain).


%get_dists_parameters([],[]).
%get_dists_parameters([Id|Ids], [dist(Id, Params)|DistsInfo]) :-
%  get_dist_params(Id, Params),
%  get_dists_parameters(Ids, DistsInfo).

