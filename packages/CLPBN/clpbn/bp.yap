
/*******************************************************

  Belief Propagation and Variable Elimination Interface
 
********************************************************/

:- module(clpbn_bp,
          [bp/3,
           check_if_bp_done/1,
           init_bp_solver/4,
           run_bp_solver/3,
           call_bp_ground/5,
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


:- use_module(library(charsio),
          [term_to_atom/2]).


:- use_module(library(pfl),
          [skolem/2,
           get_pfl_parameters/2
          ]).

:- use_module(library(clpbn/horus)).

:- use_module(library(lists)).

:- use_module(library(atts)).

:- use_module(library(bhash)).


:- use_module(horus,
          [create_ground_network/2,
           set_bayes_net_params/2,
           run_ground_solver/3,
           set_extra_vars_info/2,
           free_ground_network/1
          ]).


call_bp_ground(QueryKeys, AllKeys, Factors, Evidence, Solutions) :-
  b_hash_new(Hash0),
  keys_to_ids(AllKeys, 0, Hash0, Hash),
  %InvMap =.. [view|AllKeys],
  list_of_keys_to_ids(QueryKeys, Hash, QueryIds),
  evidence_to_ids(Evidence, Hash, EvIds),
  factors_to_ids(Factors, Hash, FactorIds),
  get_factors_type(Factors, Type),
  writeln(type:Type), writeln(''),
  writeln(allKeys:AllKeys), writeln(''),
  %writeln(allKeysIds:Hash), writeln(''),
  writeln(queryKeys:QueryKeys), writeln(''),
  writeln(queryIds:QueryIds), writeln(''),
  writeln(factors:Factors), writeln(''),
  writeln(factorIds:FactorIds), writeln(''),
  writeln(evidence:Evidence), writeln(''),
  writeln(evIds:EvIds),
  create_ground_network(Type, FactorIds, GroundNetwork).
  %run_ground_solver(Network, QueryIds, EvIds, Solutions),
  %free_graphical_model(Network).


get_factors_type([f(bayes, _, _)|_], bayes) :- ! .
get_factors_type([f(markov, _, _)|_], markov) :- ! .


keys_to_ids([], _, Hash, Hash).
keys_to_ids([Key|AllKeys], I0, Hash0, Hash) :-
  b_hash_insert(Hash0, Key, I0, HashI),
  I is I0+1,
  keys_to_ids(AllKeys, I, HashI, Hash).


list_of_keys_to_ids([], _, []).
list_of_keys_to_ids([Key|QueryKeys], Hash, [Id|QueryIds]) :-
  b_hash_lookup(Key, Id, Hash),
  list_of_keys_to_ids(QueryKeys, Hash, QueryIds).


evidence_to_ids([], _, []).
evidence_to_ids([Key=Ev|QueryKeys], Hash, [Id=Ev|QueryIds]) :-
  b_hash_lookup(Key, Id, Hash),
  evidence_to_ids(QueryKeys, Hash, QueryIds).



%evidence_to_ids([], _, [], []).
%evidence_to_ids([Key=V|QueryKeys], Hash, [Id=V|QueryIds], [Id=Name|QueryNames]) :-
%  b_hash_lookup(Key, Id, Hash),
%  pfl:skolem(Key,Dom),
%  nth0(V, Dom, Name),
%  evidence_to_ids(QueryKeys, Hash, QueryIds, QueryNames).


factors_to_ids([], _, []).
factors_to_ids([f(_, Keys, CPT)|Fs], Hash, [f(Ids, Ranges, CPT, DistId)|NFs]) :-
  list_of_keys_to_ids(Keys, Hash, Ids),
  DistId = 0,
  get_ranges(Keys, Ranges),
  factors_to_ids(Fs, Hash, NFs).


get_ranges([],[]).
get_ranges(K.Ks, Range.Rs) :-	!,
  skolem(K,Domain),
  length(Domain,Range),
  get_ranges(Ks, Rs).


bp([[]],_,_) :- !.
bp([QueryVars], AllVars, Output) :-
  init_bp_solver(_, AllVars, _, Network),
  run_bp_solver([QueryVars], LPs, Network),
  finalize_bp_solver(Network),
  clpbn_bind_vals([QueryVars], LPs, Output).


init_bp_solver(_, AllVars0, _, bp(BayesNet, DistIds)) :-
  %writeln('init_bp_solver'),
  %check_for_agg_vars(AllVars0, AllVars),
  %writeln('clpbn_vars:'), print_clpbn_vars(AllVars),
  get_vars_info(AllVars, VarsInfo, DistIds0),
  sort(DistIds0, DistIds),
  create_ground_network(VarsInfo, BayesNet),
  %get_extra_vars_info(AllVars, ExtraVarsInfo),
  %set_extra_vars_info(BayesNet, ExtraVarsInfo), 
  %writeln(extravarsinfo:ExtraVarsInfo),
  true.


run_bp_solver(QueryVars, Solutions, bp(Network, DistIds)) :-
  %writeln('-> run_bp_solver'),
  get_dists_parameters(DistIds, DistsParams),
  set_bayes_net_params(Network, DistsParams),
  vars_to_ids(QueryVars, QueryVarsIds),
  run_ground_solver(Network, QueryVarsIds, Solutions).


finalize_bp_solver(bp(Network, _)) :-
  free_ground_network(Network).


get_extra_vars_info([], []).
get_extra_vars_info([V|Vs], [v(VarId, Label, Domain)|VarsInfo]) :-
  get_atts(V, [id(VarId)]), !,
  clpbn:get_atts(V, [key(Key), dist(DistId, _)]),
  term_to_atom(Key, Label),
  get_dist_domain(DistId, Domain0),
  numbers_to_atoms(Domain0, Domain),
  get_extra_vars_info(Vs, VarsInfo).
get_extra_vars_info([_|Vs], VarsInfo) :-
  get_extra_vars_info(Vs, VarsInfo).


get_dists_parameters([],[]).
get_dists_parameters([Id|Ids], [dist(Id, Params)|DistsInfo]) :-
  get_dist_params(Id, Params),
  get_dists_parameters(Ids, DistsInfo).


numbers_to_atoms([], []).
numbers_to_atoms([Atom|L0], [Atom|L]) :-
  atom(Atom), !,
  numbers_to_atoms(L0, L).
numbers_to_atoms([Number|L0], [Atom|L]) :-
  number_atom(Number, Atom),
  numbers_to_atoms(L0, L).

