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

:- attribute size/1, posterior/4, all_diffs/1.

:- use_module(library(ordsets), [ord_union/3]).

:- use_module(library('clpbn/xbif'), [clpbn2xbif/3]).

:- use_module(library('clpbn/graphviz'), [clpbn2gviz/4]).

:- use_module(library('clpbn/utils'), [
	clpbn_not_var_member/2,
	check_for_hidden_vars/3]).

:- use_module(library('clpbn/discrete_utils'), [
	project_from_CPT/3,
	reorder_CPT/5,
	get_dist_size/2]).

:- use_module(library(lists),
	      [
	       append/3,
	       member/2
	      ]).

check_if_vel_done(Var) :-
	get_atts(Var, [size(_)]), !.

vel(LVs0,Vs0,AllDiffs) :-
	sort(LVs0,LVs1),
	get_rid_of_ev_vars(LVs1,LVs),
	do_vel(LVs,Vs0,AllDiffs).
	       
do_vel([],_,_) :- !.
do_vel(LVs,Vs0,AllDiffs) :-
	check_for_hidden_vars(Vs0, Vs0, Vs1),
	sort(Vs1,Vs),
	find_all_clpbn_vars(Vs, LV0, LVi, Tables0),
	find_all_table_deps(Tables0, LV0),
	(clpbn:output(xbif(XBifStream)) -> clpbn2xbif(XBifStream,vel,Vs) ; true),
	(clpbn:output(gviz(XBifStream)) -> clpbn2gviz(XBifStream,vel,Vs,LVs) ; true),
	process(LVi, LVs, tab(Dist,_,_)),
	Dist =.. [_|Ps0],
	normalise(Ps0,Ps),
	bind_vals(LVs,Ps,AllDiffs).

%
% some variables might already have evidence in the data-base.
%
get_rid_of_ev_vars([],[]).
get_rid_of_ev_vars([V|LVs0],LVs) :-
	clpbn:get_atts(V, [evidence(Ev)]), !,
	put_atts(V, [posterior([],Ev,[],[])]), !,
	get_rid_of_ev_vars(LVs0,LVs).
get_rid_of_ev_vars([V|LVs0],[V|LVs]) :-
	get_rid_of_ev_vars(LVs0,LVs).


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
	clpbn:get_atts(V, [dist(Vals,OTable,Parents)]),
	( clpbn:get_atts(V, [evidence(Ev)]) -> true ; true),
	reorder_CPT([V|Parents],OTable,Deps0,Table0,Sizes0),
	simplify_evidence(Deps0, Table0, Deps0, Sizes0, Table, Deps, Sizes).

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
%format('1 ~w: ~w~n',[V,WorkTables]),
	multiply_tables(WorkTables, tab(Tab0,Deps0,_)),
	Tab0 =.. [_|LTab0],
	reorder_CPT(Deps0,LTab0,Deps,Tab,Sizes),
	Table = tab(Tab,Deps,Sizes),
%format('2 ~w: ~w~n',[V,Table]),
	project_from_CPT(V,Table,NewTable),
%format('3 ~w: ~w~n',[V,NewTable]),
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
multiply_tables([tab(Tab1,Deps1,Szs1), tab(Tab2,Deps2,Sz2)| Tables], Out) :-
	multiply_table(Tab1, Deps1, Szs1, Tab2, Deps2, Sz2, NTab, NDeps, NSz),
	multiply_tables([tab(NTab,NDeps,NSz)| Tables], Out).


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

multiply_table(Tab1, Deps1, Szs1, Tab2, Deps2, Szs2, NTab, NDeps, NSzs) :-
	deps_union(Deps1,Szs1,Fs10,Deps2,Szs2,Fs20,NDeps,NSzs),
	factors(NSzs, Fs, Total),
	factors(Fs10, Fs1, _),
	factors(Fs20, Fs2, _),
	elements(0, Total, Fs, Fs1, Fs2, Tab1, Tab2, LTab),
	NTab =.. [t|LTab].

deps_union([],[],[],[],[],[],[],[]) :- !.
deps_union([],[],Fs1,[V2|Deps2],[Sz|Szs2],[Sz|Szs2],[V2|Deps2],[Sz|Szs2]) :- !,
	mk_zeros([Sz|Szs2],Fs1).
deps_union([V1|Deps1],[Sz|Szs1],[Sz|Szs1],[],[],Fs2,[V1|Deps1],[Sz|Szs1]) :- !,
	mk_zeros([Sz|Szs1],Fs2).
deps_union([V1|Deps1],[Sz|Szs1],[Sz|Fs1],[V2|Deps2],[Sz|Szs2],[Sz|Fs2],[V1|NDeps],[Sz|NSzs]) :- V1 == V2, !,
	deps_union(Deps1,Szs1,Fs1,Deps2,Szs2,Fs2,NDeps,NSzs).
deps_union([V1|Deps1],[Sz1|Szs1],[Sz1|Fs1],[V2|Deps2],Szs2,[0|Fs2],[V1|NDeps],[Sz1|NSzs]) :- V1 @< V2, !,
	deps_union(Deps1,Szs1,Fs1,[V2|Deps2],Szs2,Fs2,NDeps,NSzs).
deps_union([V1|Deps1],Szs1,[0|Fs1],[V2|Deps2],[Sz|Szs2],[Sz|Fs2],[V2|NDeps],[Sz|NSzs]) :-
	deps_union([V1|Deps1],Szs1,Fs1,Deps2,Szs2,Fs2,NDeps,NSzs).

mk_zeros([],[]).
mk_zeros([_|Szs],[0|Fs]) :-
	mk_zeros(Szs,Fs).


factors([], [], 1).
factors([0|Ls], [0|NLs], Prod) :- !,
	factors(Ls, NLs, Prod).
factors([N|Ls], [Prod0|NLs], Prod) :-
	factors(Ls, NLs, Prod0),
	Prod is Prod0*N.

elements(Total, Total, _, _, _, _, _, []) :- !.
elements(I, Total, Fs, Fs1, Fs2, Tab1, Tab2, [El|Els]) :-
	element(Fs, I, 1, Fs1, 1, Fs2, Tab1, Tab2, El),	
	I1 is I+1,
	elements(I1, Total, Fs, Fs1, Fs2, Tab1, Tab2, Els).

element([], _, P1, [], P2, [], Tab1, Tab2, El) :-
	arg(P1, Tab1, El1),
	arg(P2, Tab2, El2),
	El is El1*El2.
element([F|Fs], I, P1, [F1|Fs1], P2, [F2|Fs2], Tab1, Tab2, El) :-
	FF is I // F,
	NP1 is P1+F1*FF,
	NP2 is P2+F2*FF,
	NI is I mod F,
	element(Fs, NI, NP1, Fs1, NP2, Fs2, Tab1, Tab2, El).

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

bind_vals([],_,_) :- !.
% simple case, we want a distribution on a single variable.
%bind_vals([V],Ps) :- !,
%	clpbn:get_atts(V, [dist(Vals,_,_)]),
%	put_atts(V, posterior([V], Vals, Ps)).
% complex case, we want a joint distribution, do it on a leader.
% should split on cliques ?
bind_vals(Vs,Ps,AllDiffs) :-
	get_all_combs(Vs, Vals),
	Vs = [V|_],
	put_atts(V, posterior(Vs, Vals, Ps, AllDiffs)).

get_all_combs(Vs, Vals) :-
	get_all_doms(Vs,Ds),
	findall(L,ms(Ds,L),Vals).

get_all_doms([], []).
get_all_doms([V|Vs], [D|Ds]) :-
	clpbn:get_atts(V, [dist(D,_,_)]),
	get_all_doms(Vs, Ds).

ms([], []).
ms([H|L], [El|Els]) :-
	member(El,H),
	ms(L, Els).

normalise(Ps0,Ps) :-
	add_all(Ps0,0.0,Sum),
	divide_by_sum(Ps0,Sum,Ps).

add_all([],Sum,Sum).
add_all([P|Ps0],Sum0,Sum) :-
	SumI is Sum0+P,
	add_all(Ps0,SumI,Sum).

divide_by_sum([],_,[]).
divide_by_sum([P|Ps0],Sum,[PN|Ps]) :-
	PN is P/Sum,
	divide_by_sum(Ps0,Sum,Ps).


%
% what is actually output
%
attribute_goal(V, G) :-
	get_atts(V, [posterior(Vs,Vals,Ps,AllDiffs)]),
	massage_out(Vs, Vals, Ps, G, AllDiffs, V).

massage_out([], Ev, _, V=Ev, _, V) :- !.
massage_out(Vs, [D], [P], p(CEqs)=P, AllDiffs, _) :- !,
	gen_eqs(Vs,D,Eqs),
	add_alldiffs(AllDiffs,Eqs,CEqs).
massage_out(Vs, [D|Ds], [P|Ps], (p(CEqs)=P,G) , AllDiffs, V) :-
	gen_eqs(Vs,D,Eqs),
	add_alldiffs(AllDiffs,Eqs,CEqs),
	massage_out(Vs, Ds, Ps, G, AllDiffs, V).

gen_eqs([V], [D], (V=D)) :- !.
gen_eqs([V], D, (V=D)) :- !.
gen_eqs([V|Vs], [D|Ds], ((V=D),Eqs)) :-
	gen_eqs(Vs,Ds,Eqs).

add_alldiffs([],Eqs,Eqs).
add_alldiffs(AllDiffs,Eqs,(Eqs/alldiff(AllDiffs))).


vel_get_dist_size(V,Sz) :-
	get_atts(V, [size(Sz)]), !.
vel_get_dist_size(V,Sz) :-
	get_dist_size(V,Sz), !,
	put_atts(V, [size(Sz)]).

