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

:- use_module(library(ordsets), [ord_union/3
		    ]).

:- use_module(library('clpbn/xbif'), [cplbn2xbif/3]).

:- use_module(library(lists),
	      [
	       append/3,
	       member/2
	      ]).

check_if_vel_done(Var) :-
	get_atts(Var, [size(_)]), !.

vel(LVs,Vs0,AllDiffs) :-
	check_for_hidden_vars(Vs0, Vs0, Vs1),
	sort(Vs1,Vs),
	find_all_clpbn_vars(Vs, LV0, LVi, Tables0),
	find_all_table_deps(Tables0, LV0),
	(xbif(XBifStream) -> clpbn2xbif(XBifStream,vel,Vs) ; true),
	process(LVi, LVs, tab(Dist,_,_)),
	Dist =.. [_|Ps0],
	normalise(Ps0,Ps),
	bind_vals(LVs,Ps,AllDiffs).

%
% It may happen that variables from a previous query may still be around.
% and be used in the next evaluation, so we cannot trust the list of *new*
% variables.
%
check_for_hidden_vars([], _, []).
check_for_hidden_vars([V|Vs], AllVs0, [V|NVs]) :-
	check_for_extra_variables(V,AllVs0, AllVs, Vs, IVs),
	check_for_hidden_vars(IVs, AllVs, NVs).

check_for_extra_variables(V,AllVs0, AllVs, Vs, IVs) :-
	clpbn:get_atts(V, [dist((([_|_].[V1|LV])->_))]), !,
	add_old_variables([V1|LV], AllVs0, AllVs, Vs, IVs).
check_for_extra_variables(_,AllVs, AllVs, Vs, Vs).

add_old_variables([], AllVs, AllVs, Vs, Vs).
add_old_variables([V1|LV], AllVs0, AllVs, Vs, IVs) :-
	not_var_member(AllVs0, V1), !,
	add_old_variables(LV, [V1|AllVs0], AllVs, [V1|Vs], IVs).
add_old_variables([_|LV], AllVs0, AllVs, Vs, IVs) :-
	add_old_variables(LV, AllVs0, AllVs, Vs, IVs).

find_all_clpbn_vars([], [], [], []) :- !.
find_all_clpbn_vars([V|Vs], [Var|LV], ProcessedVars, [table(I,Table,Deps,Sizes)|Tables]) :-
	var_with_deps(V, Table, Deps, Sizes, Ev, Vals), !,
	Var = var(V,I,Sz,Vals,Ev,_,_),
	get_dist_els(V,Sz),
	% variables with evidence should not be processed.
	(var(Ev) ->
	    ProcessedVars = [Var|ProcessedVars0]
	;
	    ProcessedVars = ProcessedVars0
	),
	find_all_clpbn_vars(Vs, LV, ProcessedVars0, Tables).

var_with_deps(V, Table, Deps, Sizes, Ev, Vals) :-
	clpbn:get_atts(V, [dist((D->Vals))]),
	( clpbn:get_atts(V, [evidence(Ev)]) -> true ; true),
	from_dist_get(D,Vals,OTable,VDeps),
	reorder_table([V|VDeps],Sizes0,OTable,Deps0,Table0),
	simplify_evidence(Deps0, Table0, Deps0, Sizes0, Table, Deps, Sizes).

from_dist_get(average.Vs0,Vals,Lf,Vs) :- !,
	handle_average(Vs0,Vals,Lf,Vs).
from_dist_get(sum.Vs0,Vals,Lf,Vs) :- !,
	handle_sum(Vs0, Vals, Lf, Vs).
from_dist_get(normalised_average(Max).Vs0,Vals,Lf,Vs) :- !,
	handle_normalised_average(Max,Vs0,Vals,Lf,Vs).
from_dist_get(([A|B].[C|D]),_,[A|B],[C|D]) :- !.
from_dist_get([A|B],_,[A|B],[]).

handle_average(Vs0,Vals,Lf,Vs) :-
	sort(Vs0,Vs),
	generate_indices(Vals,Inds,0,Av),
	combine_all(Vs, Inds, Cs),
	length(Vs, Max),
	average_possible_cases(0,Av,Max,Cs,Lf).

handle_sum(Vs0, Vals, Lf, Vs) :-
	sort(Vs0,Vs),
	length(Vals,Sz),
	combine_all(Vs, Cs),
	sum_possible_cases(0,Sz,Cs,Lf).

handle_normalised_average(Max,Vs0,Vals,Lf,Vs) :-
	sort(Vs0,Vs),
	generate_indices(Vals,_,0,Sz),
	combine_all(Vs, Cs),
	average_possible_cases(0,Sz,Max,Cs,Lf).


generate_indices([],[],Av,Av).
generate_indices([_|Ls],[I|Inds],I,Av) :-
	I1 is I+1,
	generate_indices(Ls,Inds,I1,Av).


combine_all([], [[]]).
combine_all([V|LV], Cs) :-
	combine_all(LV, Cs0),
	get_dist_els(V,Sz),
	generate_indices(0, Sz, Vals),
	add_vals(Vals, Cs0, Cs).

combine_all([], _, [[]]).
combine_all([_|LV], Vals, Cs) :-
	combine_all(LV, Vals, Cs0),
	add_vals(Vals, Cs0, Cs).

generate_indices(Sz,Sz,[]) :- !.
generate_indices(I0,Sz,[I0|Vals]) :-
	I is I0+1,
	generate_indices(I,Sz,Vals).


add_vals([], _, []).
add_vals([V|Vs], Cs0, Csf) :-
	add_vals(Vs, Cs0, Cs),
	add_val_to_cases(Cs0, V, Cs, Csf).

add_val_to_cases([], _, Cs, Cs).
add_val_to_cases([C|Cs], V, Cs0, [[V|C]|Csf]) :-
	add_val_to_cases(Cs, V, Cs0, Csf).

sum_all([],N,N).
sum_all([C|Cs],N0,N) :-
	X is C+N0,
	sum_all(Cs,X,N).

average_possible_cases(Av,Av,_,_,[]) :- !.
average_possible_cases(I,Av,Max,Cs,Lf) :-
	average_cases2(Cs,I,Max,Lf,L0),
	I1 is I+1,
	average_possible_cases(I1,Av,Max,Cs,L0).

average_cases2([], _, _, L, L).
average_cases2([C|Cs], I, Av, [P|Lf], L0) :-
	calculate_avg_prob(C, I, Av, P),
	average_cases2(Cs, I, Av, Lf, L0).

calculate_avg_prob(C, I, Av, 1.0) :-
	sum_all(C,0,N),
	I =:= integer(round(N/Av)), !.
calculate_avg_prob(_, _, _, 0.0).

sum_possible_cases(Av,Av,_,[]) :- !.
sum_possible_cases(I,Av,Cs,Lf) :-
	sum_cases2(Cs,I,Lf,L0),
	I1 is I+1,
	sum_possible_cases(I1,Av,Cs,L0).

sum_cases2([], _, L, L).
sum_cases2([C|Cs], I, [P|Lf], L0) :-
	calculate_sum_prob(C, I, P),
	sum_cases2(Cs, I, Lf, L0).

calculate_sum_prob(C, I, 1.0) :-
	sum_all(C,0,N),
	I =:= N, !.
calculate_sum_prob(_, _, 0.0).


get_sizes([], []).
get_sizes([V|Deps], [Sz|Sizes]) :-
	get_dist_els(V,Sz),
	get_sizes(Deps, Sizes).

get_dist_els(V,Sz) :-
	get_atts(V, [size(Sz)]), !.
get_dist_els(V,Sz) :-
	clpbn:get_atts(V, [dist((_->Vals))]), !,
	length(Vals,Sz),
	put_atts(V, [size(Sz)]).

reorder_table(Vs0, Sizes, T0, Vs, TF) :-
	get_sizes(Vs0, Szs),
	numb_vars(Vs0, Szs, _, VPs0, VLs0),
	keysort(VLs0, VLs),
	compute_new_factors(VLs, _, Vs, Sizes),
	get_factors(VLs0,Fs),
	length(T0,L),
	functor(TF,t,L),
	copy_to_new_array(T0, 0, VPs0, Fs, TF).

numb_vars([], [], 1, [], []).
numb_vars([V|Vs], [L|Ls], A0, [Ai|VPs], [V-(L,_)|VLs]) :-
	numb_vars(Vs, Ls, Ai, VPs, VLs),
	A0 is Ai*L.

compute_new_factors([], 1, [], []).
compute_new_factors([V-(L,F)|VLs], NF, [V|Vs], [L|Szs]) :-
	compute_new_factors(VLs, F, Vs, Szs),
	NF is F*L.

get_factors([],[]).
get_factors([_-(_,F)|VLs0],[F|Fs]) :-
	get_factors(VLs0,Fs).

copy_to_new_array([], _, _, _, _).
copy_to_new_array([P|Ps], I, F0s, Fs, S) :-
	convert_factor(F0s, Fs, I, N),
	I1 is I+1,
	N1 is N+1,
	arg(N1,S,P),
	copy_to_new_array(Ps, I1, F0s, Fs, S).

convert_factor([], [], _, 0).
convert_factor([F0|F0s], [F|Fs], I, OUT) :-
	X is I//F0,
	NI is I mod F0,
	NEXT is F*X,
	convert_factor(F0s, Fs, NI, OUT1),
	OUT is OUT1+NEXT.

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
add_table_deps_to_variables([var(V,_,_,_,_,Deps,K)|LV], DepGraph) :-
	steal_deps_for_variable(DepGraph, V, NDepGraph, Deps),
	compute_size(Deps,[],K),
	add_table_deps_to_variables(LV, NDepGraph).
	
steal_deps_for_variable([V-Info|DepGraph], V0, NDepGraph, [Info|Deps]) :-
	V == V0, !,
	steal_deps_for_variable(DepGraph, V0, NDepGraph, Deps).
steal_deps_for_variable(DepGraph, _, DepGraph, []).

compute_size([],Vs,K) :-
	% use sizes now
	length(Vs,K).
%	multiply_sizes(Vs,1,K).
compute_size([tab(_,Vs,_)|Tabs],Vs0,K) :-
	ord_union(Vs,Vs0,VsI),
	compute_size(Tabs,VsI,K).

multiply_sizes([],K,K).
multiply_sizes([V|Vs],K0,K) :-
	get_dist_els(V, Sz),
	KI is K0*Sz,
	multiply_sizes(Vs,KI,K).

process(LV0, InputVs, Out) :-
	find_best(LV0, V0, 10000, V, WorkTables, LVI, InputVs),
	V \== V0, !,
	multiply_tables(WorkTables, Table),
	propagate_evidence(V, Evs),
	project(V,Table,NewTable,Evs),
	include(LVI,NewTable,V,LV2),
	process(LV2, InputVs, Out).
process(LV0, _, Out) :-
	fetch_tables(LV0, WorkTables),
	multiply_tables(WorkTables, Out).

find_best([], V, _, V, _, [], _).
find_best([var(V,I,Sz,Vals,Ev,Deps,K)|LV], _, Threshold, VF, NWorktables, LVF, Inputs) :-
	K < Threshold,
	not_var_member(Inputs, V), !,
	find_best(LV, V, K, VF, WorkTables,LV0, Inputs),
	(V == VF ->
	    LVF = LV0, Deps = NWorktables
	;
	    LVF = [var(V,I,Sz,Vals,Ev,Deps,K)|LV0], WorkTables = NWorktables
	).
find_best([V|LV], V0, Threshold, VF, WorkTables, [V|LVF], Inputs) :-
	find_best(LV, V0, Threshold, VF, WorkTables, LVF, Inputs).

multiply_tables([Table], Table) :- !.
multiply_tables([tab(Tab1,Deps1,Szs1), tab(Tab2,Deps2,Sz2)| Tables], Out) :-
	multiply_table(Tab1, Deps1, Szs1, Tab2, Deps2, Sz2, NTab, NDeps, NSz),
	multiply_tables([tab(NTab,NDeps,NSz)| Tables], Out).


simplify_evidence([], Table, Deps, Sizes, Table, Deps, Sizes).
simplify_evidence([V|VDeps], Table0, Deps0, Sizes0, Table, Deps, Sizes) :-
	clpbn:get_atts(V, [evidence(Ev)]),
	clpbn:get_atts(V, [dist((_->Out))]),
	generate_szs_with_evidence(Out,Ev,Evs),
	project(V,tab(Table0,Deps0,Sizes0),tab(NewTable,Deps1,Sizes1),Evs),
	simplify_evidence(VDeps, NewTable, Deps1, Sizes1, Table, Deps, Sizes).
simplify_evidence([_|VDeps], Table0, Deps0, Sizes0, Table, Deps, Sizes) :-
	simplify_evidence(VDeps, Table0, Deps0, Sizes0, Table, Deps, Sizes).

propagate_evidence(V, Evs) :-
	clpbn:get_atts(V, [evidence(Ev),dist((_->Out))]), !,
	generate_szs_with_evidence(Out,Ev,Evs).
propagate_evidence(_, _).

generate_szs_with_evidence([],_,[]).
generate_szs_with_evidence([Ev|Out],Ev,[ok|Evs]) :- !,
	generate_szs_with_evidence(Out,Ev,Evs).
generate_szs_with_evidence([_|Out],Ev,[not_ok|Evs]) :-
	generate_szs_with_evidence(Out,Ev,Evs).


fetch_tables([], []).
fetch_tables([var(_,_,_,_,_,Deps,_)|LV0], Tables) :-
	append(Deps,Tables0,Tables),
	fetch_tables(LV0, Tables0).

not_var_member([], _).
not_var_member([V1|Vs], V) :- V1 \== V,
	not_var_member(Vs, V).

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
project(V,tab(Table,Deps,Szs),tab(NewTable,NDeps,NSzs),Evs) :-
	functor(Table,_,Max),
	find_projection_factor(Deps, V, NDeps, Szs, NSzs, F, Sz),
	OLoop is Max//(Sz*F),
	project_outer_loop(0,OLoop,F,Sz,Table,Evs,NTabl),
	NewTable =.. [t|NTabl].

find_projection_factor([V|Deps], V1, Deps, [Sz|Szs], Szs, F, Sz) :-
	V == V1, !,
	mult(Szs, 1, F).
find_projection_factor([V|Deps], V1, [V|NDeps], [Sz|Szs], [Sz|NSzs], F, NSz) :-
	find_projection_factor(Deps, V1, NDeps, Szs, NSzs, F, NSz).

mult([], F, F).
mult([Sz|Szs], Sz0, F) :-
	SzI is Sz0*Sz,
	mult(Szs, SzI, F).

project_outer_loop(OLoop,OLoop,_,_,_,_,[]) :- !.
project_outer_loop(I,OLoop,F,Sz,Table,Evs,NTabl) :-
	Base is I*Sz*F,
	project_mid_loop(0,F,Base,Sz,Table,Evs,NTabl,NTabl0),
	I1 is I+1,
	project_outer_loop(I1,OLoop,F,Sz,Table,Evs,NTabl0).

project_mid_loop(F,F,_,_,_,_,NTabl,NTabl) :- !.
project_mid_loop(I,F,Base,Sz,Table,Evs,[Ent|NTablF],NTabl0) :-
	I1 is I+1,
	NBase is I+Base,
	project_inner_loop(0,Sz,Evs,NBase,F,Table,0.0,Ent),
	project_mid_loop(I1,F,Base,Sz,Table,Evs,NTablF,NTabl0).

project_inner_loop(Sz,Sz,[],_,_,_,Ent,Ent) :- !.
project_inner_loop(I,Sz,[ok|Evs],NBase,F,Table,Ent0,Ent) :- !,
	I1 is I+1,
	Pos is NBase+I*F+1,
	arg(Pos,Table,E1),
	Ent1 is E1+Ent0,
	project_inner_loop(I1,Sz,Evs,NBase,F,Table,Ent1,Ent).
project_inner_loop(I,Sz,[_|Evs],NBase,F,Table,Ent0,Ent) :- !,
	I1 is I+1,
	project_inner_loop(I1,Sz,Evs,NBase,F,Table,Ent0,Ent).
	
	
include([],_,_,[]).
include([var(V,P,VSz,D,Ev,Tabs,Est)|LV],tab(T,Vs,Sz),V1,[var(V,P,VSz,D,Ev,Tabs,Est)|NLV]) :-
	not_var_member(Vs,V), !,
	include(LV,tab(T,Vs,Sz),V1,NLV).
include([var(V,P,VSz,D,Ev,Tabs,_)|LV],Table,NV,[var(V,P,VSz,D,Ev,NTabs,NEst)|NLV]) :-
	update_tables(Tabs,NTabs,Table,NV,[],NEst),
	include(LV,Table,NV,NLV).

update_tables([],[Table],Table,_,AVs,NS) :-
	Table = tab(_,Vs,_),
	ord_union(Vs,AVs,TVs),
	length(TVs,NS).
update_tables([tab(Tab0,Vs,Sz)|Tabs],[tab(Tab0,Vs,Sz)|NTabs],Table,V,AVs0,NS) :-
	not_var_member(Vs,V), !,
	ord_union(Vs,AVs0,AVsI),
	update_tables(Tabs,NTabs,Table,V,AVsI,NS).
update_tables([_|Tabs],NTabs,Table,V,AVs0,NS) :-
	update_tables(Tabs,NTabs,Table,V,AVs0,NS).

bind_vals([],_,_) :- !.
% simple case, we want a distribution on a single variable.
%bind_vals([V],Ps) :- !,
%	clpbn:get_atts(V, [dist((_->Vals))]),
%	put_atts(V, posterior([V], Vals, Ps)).
% complex case, we want a joint distribution, do it on a leader.
% should split on cliques ?
bind_vals(Vs,Ps,AllDiffs) :-
	get_all_combs(Vs, Vals),
	Vs = [V|_],
	put_atts(V, posterior(Vs, Vals, Ps,AllDiffs)).

get_all_combs(Vs, Vals) :-
	get_all_doms(Vs,Ds),
	findall(L,ms(Ds,L),Vals).

get_all_doms([], []).
get_all_doms([V|Vs], [D|Ds]) :-
	clpbn:get_atts(V, [dist((_->D))]),
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

attribute_goal(V, G) :-
	get_atts(V, [posterior(Vs,Vals,Ps,AllDiffs)]),
	massage_out(Vs, Vals, Ps, G, AllDiffs).

massage_out(Vs, [D], [P], p(CEqs)=P, AllDiffs) :- !,
	gen_eqs(Vs,D,Eqs),
	add_alldiffs(AllDiffs,Eqs,CEqs).
massage_out(Vs, [D|Ds], [P|Ps], (p(CEqs)=P,G) , AllDiffs) :-
	gen_eqs(Vs,D,Eqs),
	add_alldiffs(AllDiffs,Eqs,CEqs),
	massage_out(Vs, Ds, Ps, G, AllDiffs).

gen_eqs([V], [D], (V=D)) :- !.
gen_eqs([V], D, (V=D)) :- !.
gen_eqs([V|Vs], [D|Ds], ((V=D),Eqs)) :-
	gen_eqs(Vs,Ds,Eqs).

add_alldiffs([],Eqs,Eqs).
add_alldiffs(AllDiffs,Eqs,(Eqs/alldiff(AllDiffs))).


