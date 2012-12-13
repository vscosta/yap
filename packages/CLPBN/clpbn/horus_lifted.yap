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
           finalize_horus_lifted_solver/1
          ]).

:- use_module(horus,
          [cpp_create_lifted_network/3,
           cpp_set_parfactors_params/2,
           cpp_run_lifted_solver/3,
           cpp_free_lifted_network/1
          ]).

:- use_module(library('clpbn/display'),
          [clpbn_bind_vals/3]).

:- use_module(library('clpbn/dists'),
          [get_dist_params/2]).

:- use_module(library(pfl),
          [factor/6,
           skolem/2,
           get_pfl_parameters/2
          ]).


call_horus_lifted_solver(QueryVars, AllVars, Output) :-
  init_horus_lifted_solver(_, AllVars, _, State),
  run_horus_lifted_solver(QueryVars, Solutions, State),
  clpbn_bind_vals(QueryVars, Solutions, Output),
  finalize_horus_lifted_solver(State).


init_horus_lifted_solver(_, AllVars, _, state(ParfactorList, DistIds)) :-
  get_parfactors(Parfactors),
  get_dist_ids(Parfactors, DistIds0),
  sort(DistIds0, DistIds),
  get_observed_vars(AllVars, ObservedVars),
  %writeln(parfactors:Parfactors:'\n'),
  %writeln(evidence:ObservedVars:'\n'),
  cpp_create_lifted_network(Parfactors, ObservedVars, ParfactorList).


run_horus_lifted_solver(QueryVars, Solutions, state(ParfactorList, DistIds)) :-
  get_query_keys(QueryVars, QueryKeys),
  get_dists_parameters(DistIds, DistsParams),
  %writeln(dists:DistsParams), writeln(''),
  cpp_set_parfactors_params(ParfactorList, DistsParams),
  cpp_run_lifted_solver(ParfactorList, QueryKeys, Solutions).


finalize_horus_lifted_solver(state(ParfactorList, _)) :-
  cpp_free_lifted_network(ParfactorList).


:- table get_parfactors/1.

%
% enumerate all parfactors and enumerate their domain as tuples.
%
% output is list of pf(
%   Id: an unique number
%   Ks: a list of keys, also known as the pf formula [a(X),b(Y),c(X,Y)]
%   Vs: the list of free variables [X,Y]
%   Phi: the table following usual CLP(BN) convention
%   Tuples: ground bindings for variables in Vs, of the form [fv(x,y)]
%
get_parfactors(Factors) :-
  findall(F, is_factor(F), Factors).


is_factor(pf(Id, Ks, Rs, Phi, Tuples)) :-
  factor(_Type, Id, Ks, Vs, Table, Constraints),
  get_ranges(Ks,Rs),
  Table \= avg,
  gen_table(Table, Phi),
  all_tuples(Constraints, Vs, Tuples).


get_ranges([],[]).
get_ranges(K.Ks, Range.Rs) :- !,
  skolem(K,Domain),
  length(Domain,Range),
  get_ranges(Ks, Rs).


gen_table(Table, Phi) :-
  ( is_list(Table)
      -> 
    Phi = Table
      ;
    call(user:Table, Phi)
  ).


all_tuples(Constraints, Tuple, Tuples) :-
  setof(Tuple, Constraints^run(Constraints), Tuples).


run([]).
run(Goal.Constraints) :-
  user:Goal,
  run(Constraints).


get_dist_ids([], []).
get_dist_ids(pf(Id, _, _, _, _).Parfactors, Id.DistIds) :-
  get_dist_ids(Parfactors, DistIds).


get_observed_vars([], []).
get_observed_vars(V.AllAttVars, [K:E|ObservedVars]) :-
  clpbn:get_atts(V,[key(K)]),
  ( clpbn:get_atts(V,[evidence(E)]) ; pfl:evidence(K,E) ), !,
  get_observed_vars(AllAttVars, ObservedVars).
get_observed_vars(V.AllAttVars, ObservedVars) :-
  clpbn:get_atts(V,[key(_K)]), !,
  get_observed_vars(AllAttVars, ObservedVars).


get_query_keys([], []).
get_query_keys(E1.L1, E2.L2) :-
  get_query_keys_2(E1,E2),
  get_query_keys(L1, L2).


get_query_keys_2([], []).
get_query_keys_2(V.AttVars, [RV|RVs]) :-
  clpbn:get_atts(V,[key(RV)]), !,
  get_query_keys_2(AttVars, RVs).


get_dists_parameters([], []).
get_dists_parameters([Id|Ids], [dist(Id, Params)|DistsInfo]) :-
  get_pfl_parameters(Id, Params),
  get_dists_parameters(Ids, DistsInfo).

