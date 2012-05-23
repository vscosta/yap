/*******************************************************

 Interface to Horus Ground Solvers. Used by:
   - Variable Elimination
   - Belief Propagation
   - Counting Belief Propagation
 
********************************************************/

:- module(clpbn_horus_ground,
          [bp/3,
           check_if_bp_done/1,
           init_bp_solver/4,
           run_bp_solver/3,
           call_bp_ground/6,
           finalize_bp_solver/1
          ]).

:- use_module(horus,
          [create_ground_network/4,
           set_factors_params/2,
           run_ground_solver/3,
           set_vars_information/2,
           free_ground_network/1
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

:- use_module(library(charsio),
          [term_to_atom/2]).

:- use_module(library(pfl),
          [skolem/2,
           get_pfl_parameters/2
          ]).

:- use_module(library(lists)).

:- use_module(library(atts)).

:- use_module(library(bhash)).


call_bp_ground(QueryVars, QueryKeys, AllKeys, Factors, Evidence, Output) :-
  b_hash_new(Hash0),
  keys_to_ids(AllKeys, 0, Hash0, Hash),
  get_factors_type(Factors, Type),
  evidence_to_ids(Evidence, Hash, EvidenceIds),
  factors_to_ids(Factors, Hash, FactorIds),
  %writeln(type:Type), writeln(''),
  %writeln(allKeys:AllKeys), writeln(''),
  %sort(AllKeys,SKeys),writeln(allKeys:SKeys), writeln(''),
  %writeln(factors:Factors), writeln(''),
  %writeln(factorIds:FactorIds), writeln(''),
  %writeln(evidence:Evidence), writeln(''),
  %writeln(evidenceIds:EvidenceIds), writeln(''),
  create_ground_network(Type, FactorIds, EvidenceIds, Network),
  %get_vars_information(AllKeys, StatesNames),
  %terms_to_atoms(AllKeys, KeysAtoms),
  %set_vars_information(KeysAtoms, StatesNames),
  run_solver(ground(Network,Hash), QueryKeys, Solutions),
  clpbn_bind_vals([QueryVars], Solutions, Output),
  free_ground_network(Network).


run_solver(ground(Network,Hash), QueryKeys, Solutions) :-
  %get_dists_parameters(DistIds, DistsParams),
  %set_factors_params(Network, DistsParams),
  list_of_keys_to_ids(QueryKeys, Hash, QueryIds),
  %writeln(queryKeys:QueryKeys), writeln(''),
  %writeln(queryIds:QueryIds), writeln(''),
  list_of_keys_to_ids(QueryKeys, Hash, QueryIds),
  run_ground_solver(Network, [QueryIds], Solutions).


keys_to_ids([], _, Hash, Hash).
keys_to_ids([Key|AllKeys], I0, Hash0, Hash) :-
  b_hash_insert(Hash0, Key, I0, HashI),
  I is I0+1,
  keys_to_ids(AllKeys, I, HashI, Hash).


get_factors_type([f(bayes, _, _, _)|_], bayes) :- ! .
get_factors_type([f(markov, _, _, _)|_], markov) :- ! .


list_of_keys_to_ids([], _, []).
list_of_keys_to_ids([Key|QueryKeys], Hash, [Id|QueryIds]) :-
  b_hash_lookup(Key, Id, Hash),
  list_of_keys_to_ids(QueryKeys, Hash, QueryIds).


factors_to_ids([], _, []).
factors_to_ids([f(_, DistId, Keys, CPT)|Fs], Hash, [f(Ids, Ranges, CPT, DistId)|NFs]) :-
  list_of_keys_to_ids(Keys, Hash, Ids),
  get_ranges(Keys, Ranges),
  factors_to_ids(Fs, Hash, NFs).


get_ranges([],[]).
get_ranges(K.Ks, Range.Rs) :-	!,
  skolem(K,Domain),
  length(Domain,Range),
  get_ranges(Ks, Rs).


evidence_to_ids([], _, []).
evidence_to_ids([Key=Ev|QueryKeys], Hash, [Id=Ev|QueryIds]) :-
  b_hash_lookup(Key, Id, Hash),
  evidence_to_ids(QueryKeys, Hash, QueryIds).


get_vars_information([], []).
get_vars_information(Key.QueryKeys, Domain.StatesNames) :-
  pfl:skolem(Key, Domain),
  get_vars_information(QueryKeys, StatesNames).


terms_to_atoms([], []).
terms_to_atoms(K.Ks, Atom.As) :-
  term_to_atom(K,Atom),
  terms_to_atoms(Ks,As).


finalize_bp_solver(bp(Network, _)) :-
  free_ground_network(Network).


bp([[]],_,_) :- !.
bp([QueryVars], AllVars, Output) :-
  init_bp_solver(_, AllVars, _, Network),
  run_bp_solver([QueryVars], LPs, Network),
  finalize_bp_solver(Network),
  clpbn_bind_vals([QueryVars], LPs, Output).


init_bp_solver(_, AllVars0, _, bp(BayesNet, DistIds)) :-
  %check_for_agg_vars(AllVars0, AllVars),
  get_vars_info(AllVars0, VarsInfo, DistIds0),
  sort(DistIds0, DistIds),
  create_ground_network(VarsInfo, BayesNet),
  true.


run_bp_solver(QueryVars, Solutions, bp(Network, DistIds)) :-
  get_dists_parameters(DistIds, DistsParams),
  set_factors_params(Network, DistsParams),
  vars_to_ids(QueryVars, QueryVarsIds),
  run_ground_solver(Network, QueryVarsIds, Solutions).


get_dists_parameters([],[]).
get_dists_parameters([Id|Ids], [dist(Id, Params)|DistsInfo]) :-
  get_dist_params(Id, Params),
  get_dists_parameters(Ids, DistsInfo).

