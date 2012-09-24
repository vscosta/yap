/*******************************************************

 Interface to Horus Ground Solvers. Used by:
   - Variable Elimination
   - Belief Propagation
   - Counting Belief Propagation
 
********************************************************/

:- module(clpbn_horus_ground,
          [call_horus_ground_solver/6,
           check_if_horus_ground_solver_done/1,
           init_horus_ground_solver/4,
           run_horus_ground_solver/3,
           finalize_horus_ground_solver/1
          ]).

:- use_module(horus,
          [cpp_create_ground_network/4,
           cpp_set_factors_params/2,
           cpp_run_ground_solver/3,
           cpp_set_vars_information/2,
           cpp_free_ground_network/1
          ]).

:- use_module(library('clpbn/dists'),
          [dist/4,
           get_dist_domain/2,
           get_dist_domain_size/2,
           get_dist_params/2
          ]).

:- use_module(library('clpbn/ground_factors'),
          [generate_network/5
          ]).

:- use_module(library('clpbn/display'),
          [clpbn_bind_vals/3]).

:- use_module(library('clpbn/aggregates'),
          [check_for_agg_vars/2]).

:- use_module(library(clpbn/numbers)).

:- use_module(library(charsio),
          [term_to_atom/2]).

:- use_module(library(pfl),
          [skolem/2
          ]).

:- use_module(library(maplist)).

:- use_module(library(lists)).

:- use_module(library(atts)).

:- use_module(library(bhash)).


call_horus_ground_solver(QueryVars, QueryKeys, AllKeys, Factors, Evidence, Output) :-
	call_horus_ground_solver_for_probabilities([QueryKeys], AllKeys, Factors, Evidence, Solutions),
	clpbn_bind_vals([QueryVars], Solutions, Output).

call_horus_ground_solver_for_probabilities(QueryKeys, AllKeys, Factors, Evidence, Solutions) :-
	get_factors_type(Factors, Type),
        keys_to_numbers(AllKeys, Factors, Evidence, Hash4, Id4, FactorIds, EvidenceIds),
 	%writeln(evidence:Evidence), writeln(''),
 	%writeln(evidenceIds:EvidenceIds), writeln(''),
 	%writeln(factorIds:FactorIds), writeln(''),
	cpp_create_ground_network(Type, FactorIds, EvidenceIds, Network),
	maplist(get_var_information, AllKeys, StatesNames),
	maplist(term_to_atom, AllKeys, KeysAtoms),
	%writeln(s1:KeysAtoms:KeysAtoms:StatesNames),
	cpp_set_vars_information(KeysAtoms, StatesNames),
 	%writeln(network:(Type, FactorIds, EvidenceIds, Network)), writeln(''),
	run_solver(ground(Network,Hash4,Id4), QueryKeys, Solutions),
	cpp_free_ground_network(Network).


run_solver(ground(Network,Hash,Id), QueryKeys, Solutions) :-
	%get_dists_parameters(DistIds, DistsParams),
  	%cpp_set_factors_params(Network, DistsParams),
        lists_of_keys_to_ids(QueryKeys, QueryIds, Hash, _, Id, _),
        %writeln(queryKeys:QueryKeys), writeln(''),
 %       writeln(queryIds:QueryIds), writeln(''),
        cpp_run_ground_solver(Network, QueryIds, Solutions).

get_factors_type([f(bayes, _, _)|_], bayes) :- ! .
get_factors_type([f(markov, _, _)|_], markov) :- ! .


get_var_information(Key, Domain) :-
    skolem(Key, Domain).


finalize_horus_ground_solver(bp(Network, _)) :-
  cpp_free_ground_network(Network).

%
% QVars: all query variables?
% 
% 
init_horus_ground_solver(QueryVars, _AllVars, Ground, horus(GKeys, Keys, Factors, Evidence)) :-
	(
	    var(GKeys) ->
	    Ground = ground(GKeys, Keys, Factors, Evidence) 
	;
	    generate_network(QueryVars, GKeys, Keys, Factors, Evidence) 
	).

%
% just call horus solver.
%
run_horus_ground_solver(_QueryVars, Solutions, horus(GKeys, Keys, Factors, Evidence) ) :- !,
	call_horus_ground_solver_for_probabilities(GKeys, Keys, Factors, Evidence, Solutions).

%bp([[]],_,_) :- !.
%bp([QueryVars], AllVars, Output) :-
%  init_horus_ground_solver(_, AllVars, _, Network),
%  run_horus_ground_solver([QueryVars], LPs, Network),
%  finalize_horus_ground_solver(Network),
%  clpbn_bind_vals([QueryVars], LPs, Output).
%
%init_horus_ground_solver(_, AllVars0, _, bp(BayesNet, DistIds)) :-
%  %check_for_agg_vars(AllVars0, AllVars),
%  get_vars_info(AllVars0, VarsInfo, DistIds0),
%  sort(DistIds0, DistIds),
%  cpp_create_ground_network(VarsInfo, BayesNet),
%  true.
%
%
%run_horus_ground_solver(QueryVars, Solutions, bp(Network, DistIds)) :-
%  get_dists_parameters(DistIds, DistsParams),
%  cpp_set_factors_params(Network, DistsParams),
%  vars_to_ids(QueryVars, QueryVarsIds),
%  cpp_run_ground_solver(Network, QueryVarsIds, Solutions).


get_dists_parameters([],[]).
get_dists_parameters([Id|Ids], [dist(Id, Params)|DistsInfo]) :-
  get_dist_params(Id, Params),
  get_dists_parameters(Ids, DistsInfo).

