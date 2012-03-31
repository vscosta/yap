
/*******************************************************

  Belief Propagation and Variable Elimination Interface
 
********************************************************/

:- module(clpbn_bp,
          [bp/3,
           check_if_bp_done/1,
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


:- use_module(library(charsio),
          [term_to_atom/2]).


:- use_module(horus,
          [create_ground_network/2,
           set_bayes_net_params/2,
           run_ground_solver/3,
           set_extra_vars_info/2,
           free_bayesian_network/1
          ]).


:- attribute id/1.


bp([[]],_,_) :- !.
bp([QueryVars], AllVars, Output) :-
  init_bp_solver(_, AllVars, _, Network),
  run_bp_solver([QueryVars], LPs, Network),
  finalize_bp_solver(Network),
  clpbn_bind_vals([QueryVars], LPs, Output).


init_bp_solver(_, AllVars0, _, bp(BayesNet, DistIds)) :-
  %writeln('init_bp_solver'),
  check_for_agg_vars(AllVars0, AllVars),
  %writeln('clpbn_vars:'), print_clpbn_vars(AllVars),
  assign_ids(AllVars, 0),
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
  free_bayesian_network(Network).


assign_ids([], _).
assign_ids([V|Vs], Count) :-
  put_atts(V, [id(Count)]),
  Count1 is Count + 1,
  assign_ids(Vs, Count1).


get_vars_info([], [], []).
get_vars_info(V.Vs,
    var(VarId,DS,Ev,PIds,DistId).VarsInfo,
    DistId.DistIds) :-
  clpbn:get_atts(V, [dist(DistId, Parents)]), !,
  get_atts(V, [id(VarId)]),
  get_dist_domain_size(DistId, DS),
  get_evidence(V, Ev),
  vars_to_ids(Parents, PIds),
  get_vars_info(Vs, VarsInfo, DistIds).


get_evidence(V, Ev) :-
  clpbn:get_atts(V, [evidence(Ev)]), !.
get_evidence(_V, -1). % no evidence !!!


vars_to_ids([], []).
vars_to_ids([L|Vars], [LIds|Ids]) :-
  is_list(L), !,
  vars_to_ids(L, LIds),
  vars_to_ids(Vars, Ids).
vars_to_ids([V|Vars], [VarId|Ids]) :-
  get_atts(V, [id(VarId)]),
  vars_to_ids(Vars, Ids).


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


print_clpbn_vars(Var.AllVars) :-
  clpbn:get_atts(Var, [key(Key),dist(DistId,Parents)]),
  parents_to_keys(Parents, ParentKeys),
  writeln(Var:Key:ParentKeys:DistId),
  print_clpbn_vars(AllVars).
print_clpbn_vars([]).


parents_to_keys([], []).
parents_to_keys(Var.Parents, Key.Keys) :-
  clpbn:get_atts(Var, [key(Key)]),
  parents_to_keys(Parents, Keys).

