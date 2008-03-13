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

:- module(vel, [vel/3,
		check_if_vel_done/1]).

:- attribute size/1, all_diffs/1.

:- use_module(library(ordsets), [ord_union/3]).

:- use_module(library('clpbn/xbif'), [clpbn2xbif/3]).

:- use_module(library('clpbn/graphviz'), [clpbn2gviz/4]).

:- use_module(library('clpbn/dists'),
	      [
	       get_dist_domain_size/2,
	       get_dist_matrix/5]).

:- use_module(library('clpbn/utils'), [
	clpbn_not_var_member/2,
	check_for_hidden_vars/3]).

:- use_module(library('clpbn/display'), [
	clpbn_bind_vals/3]).

:- use_module(library('clpbn/matrix_cpt_utils'),
	      [project_from_CPT/3,
	       reorder_CPT/5,
	       multiply_CPTs/4,
	       normalise_CPT/2,
	       list_from_CPT/2]).

:- use_module(library(lists),
	      [
	       append/3
	      ]).

check_if_vel_done(Var) :-
	get_atts(Var, [size(_)]), !.

%
% implementation of the well known variable elimination algorithm
%
vel([],_,_) :- !.
vel(LVs,Vs0,AllDiffs) :-
	check_for_hidden_vars(Vs0, Vs0, Vs1),
	sort(Vs1,Vs),
	% LVi will have a  list of CLPBN variables
	% Tables0 will have the full data on each variable
	find_all_clpbn_vars(Vs, LV0, LVi, Tables0),
	% construct the graph
	find_all_table_deps(Tables0, LV0),
	(clpbn:output(xbif(XBifStream)) -> clpbn2xbif(XBifStream,vel,Vs) ; true),
	(clpbn:output(gviz(XBifStream)) -> clpbn2gviz(XBifStream,vel,Vs,LVs) ; true),
	% variable elimination proper
	process(LVi, LVs, tab(Dist,_,_)),
	% move from potentials back to probabilities
	normalise_CPT(Dist,Ps),
	% from array to list
	list_from_CPT(Ps, LPs),
	% bind Probs back to variables so that they can be output.
	clpbn_bind_vals(LVs,LPs,AllDiffs).

%
% just get a list of variables plus associated tables
%
find_all_clpbn_vars([], [], [], []) :- !.
find_all_clpbn_vars([V|Vs], [Var|LV], ProcessedVars, [table(I,Table,Parents,Sizes)|Tables]) :-
	var_with_deps(V, Table, Parents, Sizes, Ev, Vals), !,
	% variables with evidence should not be processed.
	(var(Ev) ->
	    Var = var(V,I,Sz,Vals,Parents,Ev,_,_),
	    vel_get_dist_size(V,Sz),
	    ProcessedVars = [Var|ProcessedVars0]
	;
	    ProcessedVars = ProcessedVars0
	),
	find_all_clpbn_vars(Vs, LV, ProcessedVars0, Tables).

var_with_deps(V, Table, Deps, Sizes, Ev, Vals) :-
	clpbn:get_atts(V, [dist(Id,Parents)]),
	get_dist_matrix(Id,Parents,_,Vals,TAB0),
	( clpbn:get_atts(V, [evidence(Ev)]) -> true ; true),
	% set CPT in canonical form
	reorder_CPT([V|Parents],TAB0,Deps0,TAB1,Sizes1),
	% remove evidence.
	simplify_evidence(Deps0, TAB1, Deps0, Sizes1, Table, Deps, Sizes).

find_all_table_deps(Tables0, LV) :-
	find_dep_graph(Tables0, DepGraph0),
	sort(DepGraph0, DepGraph),
	add_table_deps_to_variables(LV, DepGraph).

find_dep_graph([], []).
find_dep_graph([table(I,Tab,Deps,Sizes)|Tables], DepGraph) :-
	add_table_deps(Deps, I, Deps, Tab, Sizes, DepGraph0, DepGraph),
	find_dep_graph(Tables, DepGraph0).

add_table_deps([], _, _, _, _, DepGraph, DepGraph).
add_table_deps([V|Deps], I, Deps0, Table, Sizes, DepGraph0, [V-tab(Table,Deps0,Sizes)|DepGraph]) :-
	add_table_deps(Deps, I, Deps0, Table, Sizes, DepGraph0, DepGraph).

add_table_deps_to_variables([], []).
add_table_deps_to_variables([var(V,_,_,_,_,_,Deps,K)|LV], DepGraph) :-
	steal_deps_for_variable(DepGraph, V, NDepGraph, Deps),
	compute_size(Deps,[],K),
%	( clpbn:get_atts(V,[key(Key)]) -> format('~w:~w~n',[Key,K]) ; true),
	add_table_deps_to_variables(LV, NDepGraph).
	
steal_deps_for_variable([V-Info|DepGraph], V0, NDepGraph, [Info|Deps]) :-
	V == V0, !,
	steal_deps_for_variable(DepGraph, V0, NDepGraph, Deps).
steal_deps_for_variable(DepGraph, _, DepGraph, []).

compute_size([],Vs,K) :-
	% use sizes now
%	length(Vs,K).
	multiply_sizes(Vs,1,K).
compute_size([tab(_,Vs,_)|Tabs],Vs0,K) :-
	ord_union(Vs,Vs0,VsI),
	compute_size(Tabs,VsI,K).

multiply_sizes([],K,K).
multiply_sizes([V|Vs],K0,K) :-
	vel_get_dist_size(V, Sz),
	KI is K0*Sz,
	multiply_sizes(Vs,KI,K).

process(LV0, InputVs, Out) :-
	find_best(LV0, V0, -1, V, WorkTables, LVI, InputVs),
	V \== V0, !,
% format('1 ~w: ~w~n',[V,WorkTables]),
	multiply_tables(WorkTables, tab(Tab0,Deps0,_)),
	reorder_CPT(Deps0,Tab0,Deps,Tab,Sizes),
	Table = tab(Tab,Deps,Sizes),
% format('2 ~w: ~w~n',[V,Table]),
	project_from_CPT(V,Table,NewTable),
% format('3 ~w: ~w~n',[V,NewTable]),
	include(LVI,NewTable,V,LV2),
	process(LV2, InputVs, Out).
process(LV0, _, Out) :-
	fetch_tables(LV0, WorkTables),
	multiply_tables(WorkTables, Out).


find_best([], V, _TF, V, _, [], _).
%:-
%	clpbn:get_atts(V,[key(K)]), write(chosen:K:TF), nl.
% root_with_single_child
%find_best([var(V,I,_,_,[],Ev,[Dep],K)|LV], _, _, V, [Dep], LVF, Inputs) :- !.	
find_best([var(V,I,Sz,Vals,Parents,Ev,Deps,K)|LV], _, Threshold, VF, NWorktables, LVF, Inputs) :-
	( K < Threshold ; Threshold < 0),
	clpbn_not_var_member(Inputs, V), !,
	find_best(LV, V, K, VF, WorkTables,LV0, Inputs),
	(V == VF ->
	    LVF = LV0, Deps = NWorktables
	;
	    LVF = [var(V,I,Sz,Vals,Parents,Ev,Deps,K)|LV0], WorkTables = NWorktables
	).
find_best([V|LV], V0, Threshold, VF, WorkTables, [V|LVF], Inputs) :-
	find_best(LV, V0, Threshold, VF, WorkTables, LVF, Inputs).

multiply_tables([Table], Table) :- !.
multiply_tables([TAB1, TAB2| Tables], Out) :-
	multiply_CPTs(TAB1, TAB2, TAB, _),
	multiply_tables([TAB| Tables], Out).


simplify_evidence([], Table, Deps, Sizes, Table, Deps, Sizes).
simplify_evidence([V|VDeps], Table0, Deps0, Sizes0, Table, Deps, Sizes) :-
	clpbn:get_atts(V, [evidence(_)]), !,
	project_from_CPT(V,tab(Table0,Deps0,Sizes0),tab(NewTable,Deps1,Sizes1)),
	simplify_evidence(VDeps, NewTable, Deps1, Sizes1, Table, Deps, Sizes).
simplify_evidence([_|VDeps], Table0, Deps0, Sizes0, Table, Deps, Sizes) :-
	simplify_evidence(VDeps, Table0, Deps0, Sizes0, Table, Deps, Sizes).

fetch_tables([], []).
fetch_tables([var(_,_,_,_,_,_,Deps,_)|LV0], Tables) :-
	append(Deps,Tables0,Tables),
	fetch_tables(LV0, Tables0).
% 
include([],_,_,[]).
include([var(V,P,VSz,D,Parents,Ev,Tabs,Est)|LV],tab(T,Vs,Sz),V1,[var(V,P,VSz,D,Parents,Ev,Tabs,Est)|NLV]) :-
	clpbn_not_var_member(Vs,V), !,
	include(LV,tab(T,Vs,Sz),V1,NLV).
include([var(V,P,VSz,D,Parents,Ev,Tabs,_)|LV],Table,NV,[var(V,P,VSz,D,Parents,Ev,NTabs,NEst)|NLV]) :-
	update_tables(Tabs,NTabs,Table,NV),
	compute_size(NTabs, [], NEst),
	include(LV,Table,NV,NLV).

update_tables([],[Table],Table,_).
update_tables([tab(Tab0,Vs,Sz)|Tabs],[tab(Tab0,Vs,Sz)|NTabs],Table,V) :-
	clpbn_not_var_member(Vs,V), !,
	update_tables(Tabs,NTabs,Table,V).
update_tables([_|Tabs],NTabs,Table,V) :-
	update_tables(Tabs,NTabs,Table,V).

vel_get_dist_size(V,Sz) :-
	get_atts(V, [size(Sz)]), !.
vel_get_dist_size(V,Sz) :-
	clpbn:get_atts(V,dist(Id,_)), !,
	get_dist_domain_size(Id,Sz),
	put_atts(V, [size(Sz)]).

