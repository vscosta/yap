
/***********************************

  Belief Propagation in CLP(BN)

  This should connect to C-code.
 
*********************************/

:- module(clpbn_bp, [
          bp/3,
          check_if_bp_done/1,
          init_bp_solver/4,
          run_bp_solver/3]).


:- use_module(library('clpbn/aggregates'),
          [check_for_agg_vars/2]).

:- use_module(library('clpbn/connected'),
          [init_influences/3,
           influences/5
          ]).

:- use_module(library('clpbn/dists'),
          [dist/4,
           get_dist_domain/2,
           get_dist_params/2
          ]).

:- use_module(library('clpbn/display'),
          [clpbn_bind_vals/3]).

:-use_module(library(lists),
          [append/3,
           memberchk/2
          ]).

:- load_foreign_files(['horus'], [], init_predicates).

:- attribute all_diffs/1.


check_if_bp_done(_Var).

%
% implementation of belief propagation
%
% A1 = +QueryVars -> sets of independent marginalization variables
% A2 = *AllVars   -> list of all variables
% A3 = -Output    -> output probabilities
%
% Other important variables:
%
% State0 initialized graph, is used to pass data from initialization 
% to query solving (eg, State might be the JT and be used to run
% different queries). 
%
bp([[]],_,_) :- !.
bp([QueryVars],AllVars,Output) :-
    writeln(queryVars:QueryVars),
    writeln(allVars:AllVars),
    % init_bp_solver([QueryVars], AllVars, Output, State),
    run_bp_solver([QueryVars], [AllVars], _State),
    % bind probs back to variables so that they can be output.
    clpbn_bind_vals([QueryVars],[LPs],Output).

% initialise necessary data for query solver
init_bp_solver(Qs, AllVars, _, graph(LVis)) :-
    % replace average, max, min and friends by binary nodes.
    check_for_agg_vars(AllVars, UnFoldedVars),
    % replace the variables reachable from G
    init_influences(UnfoldedVars, G, RG),
    init_bp_solver_for_questions(Qs, G, RG, _, LVis).

init_bp_solver_for_questions([], _, _, [], []).
init_bp_solver_for_questions([Vs|MVs], G, RG, [NVs|MNVs0], [NVs|LVis]) :-
    % find variables connectd to Vs
    influences(Vs, _, NVs0, G, RG),
    sort(NVs0, NVs),
    init_bp_solver_for_questions(MVs, G, RG, MNVs0, LVis).


% use a findall to recover space without needing for GC
run_bp_solver(LVs, LPs, _) :-
    findall(Ps, solve_bp(LVs, LPs, Ps), LPs).


solve_bp([LVs|_], [NVs0|_], Ps) :-
    get_vars_info(NVs0, LVi),
    get_dists_info(NVs0, Dists),
    process(LVi, Dists, LVs, Ps).
solve_bp([_|MoreLVs], [_|MoreLVis], Ps) :-
    solve_bp(MoreLVs, MoreLVis, Ps).


get_vars_info([], []).
get_vars_info([V|Vs], [var(V, Id, Parents, NParents, Ev)|LV]) :-
    clpbn:get_atts(V, [dist(Id, Parents)]), !,
    length(Parents, NParents),
    get_evidence(V, Ev),
    get_vars_info(Vs, LV).
get_vars_info([_|Vs], LV) :-
    get_vars_info(Vs, LV).


get_evidence(V, Ev) :-
    clpbn:get_atts(V, [evidence(Ev)]), !.
get_evidence(V, -1). % no evidence !!!


get_dists_info([],[]).
get_dists_info([V|Vs], [dist(Id, Domain, DSize, Params, NParams) | Dists]) :-
    clpbn:get_atts(V, [dist(Id, _)]), !,
    get_dist_domain(Id, Domain),
    length(Domain, DSize),
    get_dist_params(Id, Params),
    length(Params, NParams),
    get_dists_info(Vs, Dists).
get_dists_info([_|Vs], Dists) :-
    get_dists_info(Vs, Dists).


% +Vars  is a list containing info about every clpbn variables
% +Dists is a list containing info about distributions
% +QVs   is a list containing the query variables
% -Out   is some output term stating the probabilities
process(Vars, Dists, QVs, Out) :-
    write('vars  = '), writeln(Vars),
    order_vars(Vars, [], OrderedVars),
    write('ovars = '), writeln(OrderedVars),
    write('dists = '), writeln(Dists),
    write('qvs   = '), writeln(QVs),
    length(OrderedVars, NVars),
    length(Dists, NDists),
    %create_network(OrderedVars, NVars, Dists, NDists, BNet),
    length(QVs, NQVs),
    run_solver(BNet, QVs, NQVs, Out),
    free_memory(BNet).


order_vars([V], _, [V]) :- !.
order_vars([var(V, Id, Parents, NParents, Ev)|Vs], ParsedVars, [var(V, Id, Parents, NParents, Ev)|OrderedVars]) :-
    \+ memberchk(V, ParsedVars), 
    parents_defined(Parents, ParsedVars), !,
    order_vars(Vs, [V|ParsedVars], OrderedVars).
order_vars([var(V, Id, Parents, NParents, Ev)|Vs], ParsedVars, OrderedVars) :- 
    append(Vs, [var(V, Id, Parents, NParents, Ev)], NVs),
    order_vars(NVs, ParsedVars, OrderedVars).


parents_defined([], _) :- !.
parents_defined([Parent|Parents], ParsedVars) :- 
    memberchk(Parent, ParsedVars),
    parents_defined(Parents, ParsedVars).

% f(F), b(B). ----> FAIL
