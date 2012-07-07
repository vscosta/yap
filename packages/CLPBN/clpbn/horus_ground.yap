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


call_horus_ground_solver(QueryVars, QueryKeys, AllKeys, Factors, Evidence, Output) :-
	call_horus_ground_solver_for_probabilities(QueryKeys, AllKeys, Factors, Evidence, Solutions),
	clpbn_bind_vals([QueryVars], Solutions, Output).

call_horus_ground_solver_for_probabilities(QueryKeys, _AllKeys, Factors, Evidence, Solutions) :-
	attributes:all_attvars(AVars),
	keys(AVars, AllKeys),
	b_hash_new(Hash0),
	keys_to_ids(AllKeys, 0, Hash0, Hash),
	get_factors_type(Factors, Type),
	evidence_to_ids(Evidence, Hash, EvidenceIds),
	factors_to_ids(Factors, Hash, FactorIds),
	writeln(queryKeys:QueryKeys), writeln(''),
	writeln(type:Type), writeln(''),
	writeln(allKeys:AllKeys), writeln(''),
	sort(AllKeys,SKeys),writeln(allSortedKeys:SKeys), writeln(''),
	keys_to_ids(SKeys, 0, Hash0, Hash),
	writeln(factors:Factors), writeln(''),
	writeln(factorIds:FactorIds), writeln(''),
	writeln(evidence:Evidence), writeln(''),
	writeln(evidenceIds:EvidenceIds), writeln(''),
	cpp_create_ground_network(Type, FactorIds, EvidenceIds, Network),
	get_vars_information(AllKeys, StatesNames),
	terms_to_atoms(AllKeys, KeysAtoms),
	cpp_set_vars_information(KeysAtoms, StatesNames),
	run_solver(ground(Network,Hash), QueryKeys, Solutions),
	cpp_free_ground_network(Network).


keys([], []).
keys([V|AVars], [K|AllKeys]) :-
	clpbn:get_atts(V,[key(K)]), !,
	keys(AVars, AllKeys).
keys([_V|AVars], AllKeys) :-
	keys(AVars, AllKeys).


run_solver(ground(Network,Hash), QueryKeys, Solutions) :-
  %get_dists_parameters(DistIds, DistsParams),
  %cpp_set_factors_params(Network, DistsParams),
  list_of_keys_to_ids(QueryKeys, Hash, QueryIds),
  %writeln(queryKeys:QueryKeys), writeln(''),
  %writeln(queryIds:QueryIds), writeln(''),
  cpp_run_ground_solver(Network, [QueryIds], Solutions).


keys_to_ids([], _, Hash, Hash).
keys_to_ids([Key|AllKeys], I0, Hash0, Hash) :-
  b_hash_insert(Hash0, Key, I0, HashI),
  I is I0+1,
  keys_to_ids(AllKeys, I, HashI, Hash).


get_factors_type([f(bayes, _, _, _)|_], bayes) :- ! .
get_factors_type([f(markov, _, _, _)|_], markov) :- ! .


list_of_keys_to_ids([], _, []).
list_of_keys_to_ids([List|Extra], Hash, [IdList|More]) :-
	List = [_|_], !,
	list_of_keys_to_ids(List, Hash, IdList),
	list_of_keys_to_ids(Extra, Hash, More).
list_of_keys_to_ids([Key|QueryKeys], Hash, [Id|QueryIds]) :-
	b_hash_lookup(Key, Id, Hash),
	list_of_keys_to_ids(QueryKeys, Hash, QueryIds).


factors_to_ids([], _, []).
factors_to_ids([f(_, DistId, Keys, CPT)|Fs], Hash, [f(Ids, Ranges, CPT, DistId)|NFs]) :-
  list_of_keys_to_ids(Keys, Hash, Ids),
  get_ranges(Keys, Ranges),
  factors_to_ids(Fs, Hash, NFs).


get_ranges([],[]).
get_ranges(K.Ks, Range.Rs) :- !,
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


finalize_horus_ground_solver(bp(Network, _)) :-
  cpp_free_ground_network(Network).

%
% QVars: all query variables?
% 
% 
init_horus_ground_solver(QueryVars, _AllVars, _, horus(GKeys, Keys, Factors, Evidence)) :-
	trace,
	generate_networks(QueryVars, GKeys, [], Keys, [], Factors, [], Evidence),
	writeln(qvs:QueryVars),
	writeln(Keys), !.

%
% as you add query vars the network grows
% until you reach the last variable.
%
generate_networks([QVars|QueryVars], [GK|GKeys], _K0, K, _F0, F, _E0, E) :-
	clpbn:generate_network(QVars, GK, KI, FI, EI),
	generate_networks(QueryVars, GKeys, KI, K, FI, F, EI, E).
generate_networks([], [], K, K, F, F, E, E).

%
% just call horus solver.
%
run_horus_ground_solver(_QueryVars, Solutions, horus(GKeys, Keys, Factors, Evidence) ) :- !,
	writeln(sols:Solutions),
	writeln(state:_State),
	trace,
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

