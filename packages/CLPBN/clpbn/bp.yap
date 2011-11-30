
/************************************************

  Belief Propagation in CLP(BN)
 
**************************************************/

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


:- use_module(library(atts)).

:- use_module(library(charsio)).

:- load_foreign_files(['horus'], [], init_predicates).

:- attribute id/1.

:- dynamic num_bayes_nets/1.


check_if_bp_done(_Var).

num_bayes_nets(0).


bp([[]],_,_) :- !.
bp([QueryVars], AllVars, Output) :-
    init_bp_solver(_, AllVars, _, BayesNet),
    run_bp_solver([QueryVars], LPs, BayesNet),
    finalize_bp_solver(BayesNet),
    clpbn_bind_vals([QueryVars], LPs, Output).


init_bp_solver(_, AllVars, _, (BayesNet, DistIds)) :-
    %inc_num_bayes_nets,
    %(showprofres(50) -> true ; true),
    process_ids(AllVars, 0, DistIds0),
    get_vars_info(AllVars, VarsInfo),
    sort(DistIds0, DistIds),
    %(num_bayes_nets(0) -> writeln(vars:VarsInfo) ; true),
    %(num_bayes_nets(0) -> writeln(dists:DistsInfo) ; true),
    create_network(VarsInfo, BayesNet).
    %set_extra_vars_info(BayesNet, ExtraVarsInfo).
    

process_ids([], _, []).
process_ids([V|Vs], VarId0, [DistId|DistIds]) :-
    clpbn:get_atts(V, [dist(DistId, _)]), !,
    put_atts(V, [id(VarId0)]),
    VarId is VarId0 + 1,
    process_ids(Vs, VarId, DistIds).
process_ids([_|Vs], VarId, DistIds) :-
    process_ids(Vs, VarId, DistIds).


get_vars_info([], []).
get_vars_info([V|Vs], [var(VarId, DSize, Ev, ParentIds, DistId)|VarsInfo]) :-
    clpbn:get_atts(V, [dist(DistId, Parents)]), !,
    get_atts(V, [id(VarId)]),
    get_dist_domain_size(DistId, DSize),
    get_evidence(V, Ev),
    vars2ids(Parents, ParentIds),
    get_vars_info(Vs, VarsInfo).
get_vars_info([_|Vs], VarsInfo) :-
    get_vars_info(Vs, VarsInfo).


vars2ids([], []).
vars2ids([V|QueryVars], [VarId|Ids]) :-
    get_atts(V, [id(VarId)]),
    vars2ids(QueryVars, Ids).


get_evidence(V, Ev) :-
    clpbn:get_atts(V, [evidence(Ev)]), !.
get_evidence(_V, -1). % no evidence !!!


get_extra_vars_info([], []).
get_extra_vars_info([V|Vs], [v(VarId, Label, Domain)|VarsInfo]) :-
    get_atts(V, [id(VarId)]), !,
    clpbn:get_atts(V, [key(Key),dist(DistId, _)]),
    term_to_atom(Key, Label),
    get_dist_domain(DistId, Domain0),
    numbers2atoms(Domain0, Domain),
    get_extra_vars_info(Vs, VarsInfo).


numbers2atoms([], []).
numbers2atoms([Atom|L0], [Atom|L]) :-
     atom(Atom), !,
     numbers2atoms(L0, L).
numbers2atoms([Number|L0], [Atom|L]) :-
     number_atom(Number, Atom),
     numbers2atoms(L0, L).


run_bp_solver(QVsL0, LPs, (BayesNet, DistIds)) :-
    get_dists_parameters(DistIds, DistsParams),
    set_parameters(BayesNet, DistsParams),
    process_query_list(QVsL0, QVsL),
    %writeln(' qvs':QVsL),
    %(num_bayes_nets(1506) -> writeln(qvs:QVsL) ; true),
    run_solver(BayesNet, QVsL, LPs).


process_query_list([], []).
process_query_list([[V]|QueryVars], [VarId|Ids]) :- !,
    get_atts(V, [id(VarId)]),
    process_query_list(QueryVars, Ids).
process_query_list([Vs|QueryVars], [VarIds|Ids]) :-
    vars2ids(Vs, VarIds),
    process_query_list(QueryVars, Ids).


get_dists_parameters([],[]).
get_dists_parameters([Id|Ids], [dist(Id, Params)|DistsInfo]) :-
    get_dist_params(Id, Params),
    get_dists_parameters(Ids, DistsInfo).


finalize_bp_solver((BayesNet, _)) :-
    delete_bayes_net(BayesNet).


inc_num_bayes_nets :-
    retract(num_bayes_nets(Count0)),
    Count is Count0 + 1,
    assert(num_bayes_nets(Count)).

