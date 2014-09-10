/***********************************

  Variable Elimination in Prolog

  How to do it


  Three steps:
  build the graph:
    - for all variables, find out
      all tables they connect to;
      multiply their size
      order by size

*********************************/

:- module(clpbn_ve,
		[ve/3,
		 check_if_ve_done/1,
		 init_ve_solver/4,
		 run_ve_solver/3,
		 init_ve_ground_solver/5,
		 run_ve_ground_solver/3,
		 call_ve_ground_solver/6
		]).

:- use_module(library(atts)).

:- use_module(library(ordsets),
		[ord_union/3,
		 ord_member/2
		]).

:- use_module(library('clpbn/xbif'),
		[clpbn2xbif/3]).

:- use_module(library('clpbn/graphviz'),
		[clpbn2gviz/4]).

:- use_module(library('clpbn/dists'),
		[dist/4,
		 get_dist_domain_size/2,
		 get_dist_params/2,
		 get_dist_domain_size/2,
		 get_dist_matrix/5
		]).

:- use_module(library('clpbn/utils'),
		[clpbn_not_var_member/2]).

:- use_module(library('clpbn/display'),
		[clpbn_bind_vals/3]).

:- use_module(library('clpbn/connected'),
		[init_influences/3,
		 influences/4,
		 factor_influences/4
		]).

:- use_module(library(clpbn/matrix_cpt_utils)).

:- use_module(library(clpbn/numbers)).

:- use_module(library(lists),
		[member/2,
		 append/3,
		 delete/3,
		 sum_list/2
		]).

:- use_module(library(maplist)).

:- use_module(library(rbtrees)).

:- use_module(library(clpbn/vmap)).

:- use_module(library('clpbn/aggregates'),
		[check_for_agg_vars/2]).

:- attribute size/1, all_diffs/1.

%
% uses a bipartite graph where bigraph(Vs, NFs, Fs)
% Vs=map variables to lists of factors
% NFs=number of factors
% Fs=map factor id -> f(Id, Vars, Table)
%

check_if_ve_done(Var) :-
	get_atts(Var, [size(_)]), !.


%
% new PFL like interface...
%
call_ve_ground_solver(QueryVars, QueryKeys, AllKeys, Factors, Evidence, Output) :-
	call_ve_ground_solver_for_probabilities([QueryKeys], AllKeys, Factors, Evidence, Solutions),
	clpbn_bind_vals([QueryVars], Solutions, Output).

call_ve_ground_solver_for_probabilities(QueryKeys, AllKeys, Factors, Evidence, Solutions) :-
	init_ve_ground_solver(QueryKeys, AllKeys, Factors, Evidence, VE),
	run_ve_ground_solver(QueryKeys, Solutions, VE).

simulate_ve_ground_solver(_QueryVars, QueryKeys, AllKeys, Factors, Evidence, Output) :-
	simulate_ve_ground_solver_for_probabilities([QueryKeys], AllKeys, Factors, Evidence, Output).

simulate_ve_ground_solver_for_probabilities(QueryKeys, AllKeys, Factors, Evidence, Solutions) :-
	init_ve_ground_solver(QueryKeys, AllKeys, Factors, Evidence, VE),
	simulate_solver(QueryKeys, Solutions, VE).

init_ve_ground_solver(_QueryKeys, AllKeys, Factors, Evidence, VE) :-
	keys_to_numbers(AllKeys, Factors, Evidence, Hash4, Id4, FactorIds, EvidenceIds),
	init_ve(FactorIds, EvidenceIds, Hash4, Id4, VE).


%
% implementation of the well known variable elimination algorithm
%
ve([[]],_,_) :- !.
ve(LLVs,Vs0,AllDiffs) :-
	init_ve_solver(LLVs, Vs0, AllDiffs, State),
	% variable elimination proper
	run_ve_solver(LLVs, LLPs, State),
	% bind Probs back to variables so that they can be output.
	clpbn_bind_vals(LLVs,LLPs,AllDiffs).


init_ve(FactorIds, EvidenceIds, Hash, Id, ve(FactorIds, Hash, Id, Ev)) :-
	rb_new(Ev0),
	foldl(evtotree,EvidenceIds,Ev0,Ev).

evtotree(K=V,Ev0,Ev) :-
	rb_insert(Ev0, K, V, Ev).

factor_to_graph( fn(Nodes, Sizes, _Pars0, Id, Keys), Factors0, Factors, Edges0, Edges, I0, I) :-
	I is I0+1,
	pfl:get_pfl_parameters(Id, Keys, Pars0),
	init_CPT(Pars0, Sizes, CPT0),
	reorder_CPT(Nodes, CPT0, FIPs, CPT, _),
	F = f(I0, FIPs, CPT),
	rb_insert(Factors0, I0, F, Factors),
	foldl(add_f_to_nodes(I0), Nodes, Edges0, Edges).

add_f_to_nodes(I0, Node, Edges, [Node-I0|Edges]).


%
% Qs is a list of lists with all query vars (marginals)
% IQs is the corresponding list of integers
% LVis is a list of lists with all variables reachable from the query
% ILVis is the corresponding list of integers
% Vmap is the map V->I
%
init_ve_solver(Qs, Vs0, _, state(IQs, LVIs, VMap, Bigraph, Ev)) :-
	% LVi will have a  list of CLPBN variables
	init_influences(Vs0, Graph, TGraph),
	maplist(init_ve_solver_for_question(Graph, TGraph), Qs, LVs),
	init_vmap(VMap0),
	lvars_to_numbers(LVs, LVIs, VMap0, VMap1),
	lvars_to_numbers(Qs, IQs, VMap1, VMap),
	vars_to_bigraph(VMap, Bigraph, Ev).

init_ve_solver_for_question(G, RG, Vs, NVs) :-
	influences(Vs, G, RG, NVs0),
	sort(NVs0, NVs).

%
% construct a bipartite graph with vars and factors
% the nodes of the var graph just contain pointer to the factors
% the nodes of the factors contain a list of variables and a matrix
% also provide a matrix with evidence
%
vars_to_bigraph(VMap, bigraph(VInfo, IF, Fs), Evs) :-
	rb_new(Fs0),
	vmap_to_list(VMap, VIds),
	foldl3(id_to_factor(VMap), VIds, 0, IF, Fs0, Fs, [], Evs),
	factors_to_vs(Fs, VInfo).

id_to_factor(VMap, V-I, IF0, IF, Fs0, Fs, Evs0, Evs) :-
	% process evidence for variable
	clpbn:get_atts(V, [evidence(E), dist(_,Ps)]),
	checklist(noparent_of_interest(VMap), Ps), !,
	% I don't need to get a factor here
	Evs = [I=E|Evs0],
	IF = IF0,
	Fs = Fs0.
id_to_factor(VMap, V-I, IF0, IF, Fs0, Fs, Evs0, Evs) :-
	%  process distribution/factors
	(
	  clpbn:get_atts(V, [evidence(E)])
	->
	  Evs = [I=E|Evs0]
	;
	  Evs = Evs0
	),
	clpbn:get_atts(V, [dist(D, Ps)]),
	get_dist_params(D, Pars0),
	get_dist_domain_size(D, DS),
	maplist(parent_to_id(VMap), Ps, Sizes, IPs),
	init_CPT(Pars0, [DS|Sizes], CPT0),
	reorder_CPT([I|IPs], CPT0, FIPs, CPT, _),
	rb_insert(Fs0, IF0, f(IF0, FIPs, CPT), Fs),
	IF is IF0+1.

noparent_of_interest(VMap, P) :-
	\+ get_from_vmap(P, _, VMap).

parent_to_id(VMap, V, DS, I) :-
	clpbn:get_atts(V, [dist(D, _Ps)]),
	get_dist_domain_size(D, DS),
	get_from_vmap(V, I, VMap).

factors_to_vs(Fs, VInfo) :-
	rb_visit(Fs, L),
	fsvs(L, FVs, []),
	sort(FVs, SFVs),
	rb_new(VInfo0),
	add_vs(SFVs, Fs, VInfo0, VInfo).

fsvs(F-f(_, IVs, _)) -->
	fvs(IVs, F).

fvs([], _F) --> [].
fvs([I|IVs], F) -->
	[I-F],
	fvs(IVs, F).

%
% construct variable nodes
%
add_vs([], _, VInfo, VInfo).
add_vs([V-F|SFVs], Fs, VInfo0, VInfo) :-
	rb_lookup(F, FInfo, Fs),
	collect_factors(SFVs, Fs, V, Fs0, R),
	rb_insert(VInfo0, V, [FInfo|Fs0], VInfoI),
	add_vs(R, Fs, VInfoI, VInfo).

collect_factors([], _Fs, _V, [], []) :- !.
collect_factors([V-F|SFVs], Fs, V, [FInfo|FInfos], R):-
	!,
	rb_lookup(F, FInfo, Fs),
	collect_factors(SFVs, Fs, V, FInfos, R).
collect_factors(SFVs, _Fs, _V, [], SFVs).

% solve each query independently
% use a findall to recover space without needing for GC
run_ve_ground_solver(LQVs, LLPs, ve(FactorIds, Hash, Id, Ev)) :-
	rb_new(Fs0),
	foldl3(factor_to_graph, FactorIds, Fs0, Fs, [], FVs, 0, IF),
	sort(FVs, SFVs),
	rb_new(VInfo0),
	add_vs(SFVs, Fs, VInfo0, VInfo),
	BG = bigraph(VInfo, IF, Fs),
	lists_of_keys_to_ids(LQVs, LQIds, Hash, _, Id, _),
	findall(LPs, solve(LQIds, FactorIds, BG, Ev, LPs), LLPs).

solve([QVs|_], FIds, Bigraph, Evs, LPs) :-
	factor_influences(FIds, QVs, Evs, LVs),
	do_solve(QVs, LVs, Bigraph, Evs, LPs).
solve([_|LQVs], FIds, Bigraph, Ev, LPs) :-
	solve(LQVs, FIds, Bigraph, Ev, LPs).

do_solve(IQVs, IVs, bigraph(OldVs, IF, _Fs), Ev, Ps) :-
	% get only what is relevant to query,
	project_to_query_related(IVs, OldVs, SVs, Fs1),
	% and also prune using evidence
	rb_visit(Ev, EvL),
	foldl2(clean_v_ev, EvL, Fs1, Fs2, SVs, EVs),
	% eliminate
	eliminate(IQVs, digraph(EVs, IF, Fs2), Dist),
% writeln(m:Dist),matrix:matrix_to_list(Dist,LD),writeln(LD),
%exps(LD,LDE),writeln(LDE),
	% move from potentials back to probabilities
	normalise_CPT(Dist,MPs),
	list_from_CPT(MPs, Ps).

simulate_solver(LQVs, Choices, ve(FIds, Hash, Id, BG, Evs)) :-
	lists_of_keys_to_ids(LQVs, [QVs], Hash, _, Id, _),
	factor_influences(FIds, QVs, Evs, LVs),
	do_simulate(QVs, LVs, BG, Evs, Choices).

do_simulate(IQVs, IVs, bigraph(OldVs, IF, _Fs), Ev, Choices) :-
	% get only what is relevant to query,
	project_to_query_related(IVs, OldVs, SVs, Fs1),
	% and also prune using evidence
	rb_visit(Ev, EvL),
	foldl2(clean_v_ev, EvL, Fs1, Fs2, SVs, EVs),
	% eliminate
	simulate_eiminate(IQVs, digraph(EVs, IF, Fs2), Choices).

% solve each query independently
% use a findall to recover space without needing for GC
run_ve_solver(_, LLPs, state(LQVs, LVs, _VMap,  Bigraph, Ev)) :-
	findall(LPs, solve_ve(LQVs, LVs, Bigraph, Ev, LPs), LLPs).

%
% IQVs are the current marginal,
% IVs are all variables related to that
% IFVs are the factors
% SVs are the variables
%
solve_ve([IQVs|_], [IVs|_], bigraph(OldVs, IF, _Fs), Ev, Ps) :-
	% get only what is relevant to query,
	project_to_query_related(IVs, OldVs, SVs, Fs1),
	% and also prune using evidence
	foldl2(clean_v_ev, Ev, Fs1, Fs2, SVs, EVs),
	% eliminate
	eliminate(IQVs, digraph(EVs, IF, Fs2), Dist),
% writeln(m:Dist),matrix:matrix_to_list(Dist,LD),writeln(LD),
%exps(LD,LDE),writeln(LDE),
	% move from potentials back to probabilities
	normalise_CPT(Dist,MPs),
	list_from_CPT(MPs, Ps).
solve_ve([_|MoreLVs], [_|MoreLVis], Digraph, Ev, Ps) :-
	solve_ve(MoreLVs, MoreLVis, Digraph, Ev, Ps).

%
% given our input queries, sort them and obtain the subgraphs of vars and facs.
%
project_to_query_related(IVs0, OldVs, NVs, NFs) :-
	sort(IVs0, IVs),
	rb_new(Vs0),
	foldl(cp_to_vs, IVs, Vs0, AuxVs),
	rb_new(NFs0),
	foldl(simplify_graph_node(OldVs, AuxVs), IVs, VFs, NFs0, NFs),
	list_to_rbtree(VFs, NVs).

%
% auxiliary tree for fast access to vars.
%
cp_to_vs(V, Vs0, Vs) :-
	rb_insert(Vs0, V, _, Vs).

%
% construct a new, hopefully much smaller, graph
%
simplify_graph_node(OldVs, NVs, V, V-RemFs, NFs0, NFs) :-
	rb_lookup(V, Fs, OldVs),
	foldl2(check_factor(V, NVs), Fs, NFs0, NFs, [], RemFs).

%
% check if a factor belongs to the subgraph.
%
%
% Two cases: first time factor comes up: all its vars must be in subgraph
% second case: second time it comes up, it must be already in graph
%
% args: +Factor F, +current V (int), +rbtree with all Vs,
%       -Factors in new Graph, +factors in current graph, -rbtree of factors
%
%
check_factor(V, NVs, F, NFs0, NFs, RemFs, NewRemFs) :-
	F = f(IF, [V|More], _), !,
	(
	  checklist(check_v(NVs), More)
	->
	  rb_insert(NFs0, IF, F, NFs),
	  NewRemFs = [F|RemFs]
	;
	  NFs0 = NFs,
	  NewRemFs = RemFs
	).
check_factor(_V, _NVs, F, NFs, NFs, RemFs, NewRemFs) :-
	F = f(Id, _, _),
	(
	  rb_lookup(Id, F, NFs)
	->
	  NewRemFs = [F|RemFs]
	;
	  NewRemFs = RemFs
	).

check_v(NVs, V) :-
	rb_lookup(V, _, NVs).

%
% simplify a variable with evidence
%
clean_v_ev(V=E, FVs0, FVs, Vs0, Vs) :-
	rb_delete(Vs0, V, Fs, Vs1), !,
	foldl2(simplify_f_ev(V, E), Fs, FVs0, FVs, Vs1, Vs).
clean_v_ev(V-E, FVs0, FVs, Vs0, Vs) :-
	rb_delete(Vs0, V, Fs, Vs1), !,
	foldl2(simplify_f_ev(V, E), Fs, FVs0, FVs, Vs1, Vs).
% The variable is not there
clean_v_ev(_, FVs, FVs, Vs, Vs).

%
%
% tricky: clean a factor means also cleaning all back references.
%
simplify_f_ev(V, E, F, Fs0, Fs, Vs0, Vs) :-
	F =  f(Id,  FVs,  CPT),
	NF = f(Id, NFVs, NCPT),
	project_from_CPT(V, E, CPT, FVs, NCPT, NFVs),
	% update factor
	rb_update(Fs0, Id, NF, Fs),
	foldl(update_factors(F,NF), NFVs, Vs0, Vs).

% update all instances of F in var graph
update_factors(F, NF, V, Vs0, Vs) :-
	rb_update(Vs0, V, Fs, NFs, Vs),
	maplist(replace_factor(F,NF), Fs, NFs).

replace_factor(F, NF, F, NF) :- !.
replace_factor(_F,_NF,OF, OF).

eliminate(QVs, digraph(Vs0, I, Fs0), Dist) :-
	find_best(Vs0, QVs, BestV, VFs), !,
	%writeln(best:BestV:VFs),
	% delete all factors that touched the variable
	foldl2(del_fac, VFs, Fs0, Fs1, Vs0, Vs1),
	% delete current variable
	rb_delete(Vs1, BestV, Vs2),
	I1 is I+1,
	% construct new table
	multiply_and_delete(VFs, BestV, NewFVs, NewCPT),
	% insert new factor in graph
	insert_fac(I, NewFVs, NewCPT, Fs1, Fs, Vs2, Vs),
	eliminate(QVs, digraph(Vs, I1, Fs), Dist).
eliminate(_QVs, digraph(_, _, Fs), Dist) :-
	combine_factors(Fs, Dist).

find_best(Vs, QVs, BestV, VFs) :-
	rb_key_fold(best_var(QVs), Vs, i(+inf,-1,[]), i(_Cost,BestV,VFs)),
	BestV \= -1, !.

% do not eliminate marginalised variables
best_var(QVs, I, _Node, Info, Info) :-
	member(I, QVs),
	!.
% pick the variable with less factors
best_var(_Qs, I, Node, i(ValSoFar,_,_), i(NewVal,I,Node)) :-
	foldl(szfac,Node,1,NewVal),
	%length(Node, NewVal),
	NewVal < ValSoFar,
	!.
best_var(_, _I, _Node, Info, Info).

szfac(f(_,Vs,_), I0, I) :-
	length(Vs,L),
	I is I0*L.

% delete one factor, need to also touch all variables
del_fac(f(I,FVs,_), Fs0, Fs, Vs0, Vs) :-
	rb_delete(Fs0, I, Fs),
	foldl(delete_fac_from_v(I), FVs, Vs0, Vs).

delete_fac_from_v(I, FV, Vs0, Vs) :-
	rb_update(Vs0, FV, Fs, NFs, Vs),
	exclude(factor_name(I), Fs, NFs).

factor_name(I, f(I,_,_)).

% insert one factor, need to touch all corresponding variables
insert_fac(I, FVs, CPT, Fs0, Fs, Vs0, Vs) :-
	F = f(I, FVs, CPT),
	rb_insert(Fs0, I, F, Fs),
	foldl(insert_fac_in_v(F), FVs, Vs0, Vs).

insert_fac_in_v(F, FV, Vs0, Vs) :-
	rb_update(Vs0, FV, Fs, [F|Fs], Vs).

combine_factors(Fs, Dist) :-
	rb_visit(Fs,Els),
	maplist(extract_factor,Els,Factors),
	multiply(Factors, _, Dist).

extract_factor(_-Factor, Factor).

multiply_and_delete([f(I,Vs0,T0)|Fs], V, Vs, T) :-
	foldl(multiply_factor, Fs, f(I,Vs0,T0), f(_,Vs1,T1)),
	sum_out_from_CPT(V, T1, Vs1, T, Vs).

multiply([F0|Fs], Vs, T) :-
	foldl(multiply_factor, Fs, F0, f(_,Vs,T)).

multiply_factor(f(_,Vs1,T1), f(_,Vs0,T0), f(_,Vs,T)) :-
	multiply_CPTs(T1, Vs1, T0, Vs0, T, Vs).

