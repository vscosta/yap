
:- module(jt,
		[jt/3,
		 init_jt_solver/4,
		 run_jt_solver/3
		]).

:- use_module(library(dgraphs),
		[dgraph_new/1,
		 dgraph_add_edges/3,
		 dgraph_add_vertex/3,
		 dgraph_add_vertices/3,
		 dgraph_edges/2,
		 dgraph_vertices/2,
		 dgraph_transpose/2,
		 dgraph_to_ugraph/2,
		 ugraph_to_dgraph/2,
		 dgraph_neighbors/3
		]).

:- use_module(library(undgraphs),
		[undgraph_new/1,
		 undgraph_add_edge/4,
		 undgraph_add_edges/3,
		 undgraph_del_vertex/3,
		 undgraph_del_vertices/3,
		 undgraph_vertices/2,
		 undgraph_edges/2,
		 undgraph_neighbors/3,
		 undgraph_edge/3,
		 dgraph_to_undgraph/2
		]).

:- use_module(library(wundgraphs),
		[wundgraph_new/1,
		 wundgraph_max_tree/3,
		 wundgraph_add_edges/3,
		 wundgraph_add_vertices/3,
		 wundgraph_to_undgraph/2
		]).

:- use_module(library(rbtrees),
		[rb_new/1,
		 rb_insert/4,
		 rb_lookup/3
		]).

:- use_module(library(ordsets),
		[ord_subset/2,
		 ord_insert/3,
		 ord_intersection/3,
		 ord_del_element/3,
		 ord_memberchk/2
		]).

:- use_module(library(lists),
		[reverse/2]).

:- use_module(library(maplist)).

:- use_module(library('clpbn/aggregates'),
		[check_for_agg_vars/2]).

:- use_module(library('clpbn/dists'),
		[get_dist_domain_size/2,
		 get_dist_domain/2,
		 get_dist_matrix/5
		]).

:- use_module(library('clpbn/matrix_cpt_utils'),
		[project_from_CPT/3,
		 reorder_CPT/5,
		 unit_CPT/2,
		 multiply_CPTs/4,
		 divide_CPTs/3,
		 normalise_CPT/2,
		 expand_CPT/4,
		 get_CPT_sizes/2,
		 reset_CPT_that_disagrees/5,
		 sum_out_from_CPT/4,
		 list_from_CPT/2
		]).

:- use_module(library('clpbn/display'),
		[clpbn_bind_vals/3]).

:- use_module(library('clpbn/connected'),
		[init_influences/3,
		 influences/4
		]).


jt([[]],_,_) :- !.
jt(LLVs,Vs0,AllDiffs) :-
	init_jt_solver(LLVs, Vs0, AllDiffs, State),
	maplist(run_jt_solver, LLVs, LLPs, State),
	clpbn_bind_vals(LLVs,LLPs,AllDiffs).


init_jt_solver(LLVs, Vs0, _, State) :-
	check_for_agg_vars(Vs0, Vs1),
	init_influences(Vs1, G, RG),
	maplist(init_jt_solver_for_question(G, RG), LLVs, State).

init_jt_solver_for_question(G, RG, LLVs, state(JTree, Evidence)) :-
	influences(LLVs, G, RG, NVs0),
	sort(NVs0, NVs),
	get_graph(NVs, BayesNet, CPTs, Evidence),
	build_jt(BayesNet, CPTs, JTree).

run_jt_solver(LVs, LPs, state(JTree, Evidence)) :-
	% JTree is a dgraph
	% now our tree has cpts
	fill_with_cpts(JTree, NewTree),
%	write_tree(0, NewTree),
	propagate_evidence(Evidence, NewTree, EvTree),
	message_passing(EvTree, MTree),
	get_margin(MTree, LVs, LPs).

get_graph(LVs, BayesNet, CPTs, Evidence) :-
	run_vars(LVs, Edges, Vertices, CPTs, Evidence),
	dgraph_new(V0),
	dgraph_add_edges(V0, Edges, V1),
	dgraph_add_vertices(V1, Vertices, V2),
	dgraph_to_ugraph(V2, BayesNet).

run_vars([], [], [], [], []).
run_vars([V|LVs], Edges, [V|Vs], [CPTVars-dist([V|Parents],Id)|CPTs], Ev) :-
	clpbn:get_atts(V, [dist(Id,Parents)]),
	add_evidence_from_vars(V, Ev, Ev0),
	sort([V|Parents],CPTVars),
	add_edges(Parents, V, Edges, Edges0),
	run_vars(LVs, Edges0, Vs, CPTs, Ev0).

add_evidence_from_vars(V, [e(V,P)|Evs], Evs) :-
	clpbn:get_atts(V, [evidence(P)]), !.
add_evidence_from_vars(_, Evs, Evs).

find_nth0([Id|_], Id, P, P) :- !.
find_nth0([_|D], Id, P0, P) :-
	P1 is P0+1,
	find_nth0(D, Id, P1, P).

add_edges([], _, Edges, Edges).
add_edges([P|Parents], V, [V-P|Edges], Edges0) :-
	add_edges(Parents, V, Edges, Edges0).

build_jt(BayesNet, CPTs, Tree) :-
	init_undgraph(BayesNet, Moral0),
	moralised(BayesNet, Moral0, Markov),
	undgraph_vertices(Markov, Vertices),
	triangulate(Vertices, Markov, Markov, _, Cliques0),
	cliques(Cliques0, EndCliques),
	wundgraph_max_tree(EndCliques, J0Tree, _),
	root(J0Tree, JTree),
	populate(CPTs, JTree, Tree).

initial_graph(_,Parents, CPTs) :-
	test_graph(0, Graph0, CPTs),
	dgraph_new(V0),
	dgraph_add_edges(V0, Graph0, V1),
	% OK, this is a bit silly, I could have written the transposed graph
	% from the very beginning.
	dgraph_transpose(V1, V2),
	dgraph_to_ugraph(V2, Parents).


problem_graph([], []).
problem_graph([V|BNet], GraphF) :-
	clpbn:get_atts(V, [dist(_,_,Parents)]),
	add_parents(Parents, V, Graph0, GraphF),
	problem_graph(BNet, Graph0).

add_parents([], _, Graph, Graph).
add_parents([P|Parents], V, Graph0, [P-V|GraphF]) :-
	add_parents(Parents, V, Graph0, GraphF).


% From David Page's lectures
test_graph(0,
	   [1-3,2-3,2-4,5-4,5-7,10-7,10-9,11-9,3-6,4-6,7-8,9-8,6-12,8-12],
	   [[1]-a,
	    [2]-b,
	    [1,2,3]-c,
	    [2,4,5]-d,
	    [5]-e,
	    [3,4,6]-f,
	    [5,7,10]-g,
	    [7,8,9]-h,
	    [9]-i,
	    [10]-j,
	    [11]-k,
	    [6,8,12]-l
	   ]).
test_graph(1,[a-b,a-c,b-d,c-e,d-f,e-f],
	   []).


init_undgraph(Parents, UndGraph) :-
	ugraph_to_dgraph(Parents, DGraph),
	dgraph_to_undgraph(DGraph, UndGraph).

get_par_keys([], []).
get_par_keys([P|Parents],[K|KPars]) :-
	clpbn:get_atts(P, [key(K)]),
	get_par_kets(Parents,KPars).

moralised([],Moral,Moral).
moralised([_-KPars|Ks],Moral0,MoralF) :-
	add_moral_edges(KPars, Moral0, MoralI),
	moralised(Ks,MoralI,MoralF).

add_moral_edges([], Moral, Moral).
add_moral_edges([_], Moral, Moral).
add_moral_edges([K1,K2|KPars], Moral0, MoralF) :-
	undgraph_add_edge(Moral0, K1, K2, MoralI),
	add_moral_edges([K1|KPars], MoralI, MoralJ),
	add_moral_edges([K2|KPars],MoralJ,MoralF).

triangulate([], _, Triangulated, Triangulated, []) :- !.
triangulate(Vertices, S0, T0, Tf, Cliques) :-
	choose(Vertices, S0, +inf, [], -1, BestVertex, _, Cliques0, Cliques, Edges),
	ord_del_element(Vertices, BestVertex, NextVertices),
	undgraph_add_edges(T0, Edges, T1),
	undgraph_del_vertex(S0, BestVertex, Si),
	undgraph_add_edges(Si, Edges, Si2),
	triangulate(NextVertices, Si2, T1, Tf, Cliques0).

choose([], _, _, NewEdges, Best, Best, Clique, Cliques0, [Clique|Cliques0], NewEdges).
choose([V|Vertices], Graph, Score0, _, _, Best, _, Cliques0, Cliques, EdgesF) :-
	undgraph_neighbors(V, Graph, Neighbors),
	ord_insert(Neighbors, V, PossibleClique),
	new_edges(Neighbors, Graph, NewEdges),
	(
	  % simplicial edge
	  NewEdges == []
	->
	  !,
	  Best = V,
	  NewEdges = EdgesF,
	  length(PossibleClique,L),
	  Cliques = [L-PossibleClique|Cliques0]
	;
%	  cliquelength(PossibleClique,1,CL),
	  length(PossibleClique,CL),
	  CL < Score0, !,
	  choose(Vertices,Graph,CL,NewEdges, V, Best, CL-PossibleClique, Cliques0,Cliques,EdgesF)
	).
choose([_|Vertices], Graph, Score0, Edges0, BestSoFar, Best, Clique, Cliques0, Cliques, EdgesF) :-
	choose(Vertices,Graph,Score0,Edges0, BestSoFar, Best, Clique, Cliques0,Cliques,EdgesF).

new_edges([], _, []).
new_edges([N|Neighbors], Graph, NewEdgesF) :-
	new_edges(Neighbors,N,Graph,NewEdges0, NewEdgesF),
	new_edges(Neighbors, Graph, NewEdges0).

new_edges([],_,_,NewEdges, NewEdges).
new_edges([N1|Neighbors],N,Graph,NewEdges0, NewEdgesF) :-
	undgraph_edge(N, N1, Graph), !,
	new_edges(Neighbors,N,Graph,NewEdges0, NewEdgesF).
new_edges([N1|Neighbors],N,Graph,NewEdges0, [N-N1|NewEdgesF]) :-
	new_edges(Neighbors,N,Graph,NewEdges0, NewEdgesF).

cliquelength([],CL,CL).
cliquelength([V|Vs],CL0,CL) :-
	clpbn:get_atts(V, [dist(Id,_)]),
	get_dist_domain_size(Id, Sz),
	CL1 is CL0*Sz,
	cliquelength(Vs,CL1,CL).


%
% This is simple stuff, I just have to remove cliques that
% are subset of the others.
%
cliques(CliqueList, CliquesF) :-
	wundgraph_new(Cliques0),
	% first step, order by size,
	keysort(CliqueList,Sort),
	reverse(Sort, Rev),
	get_links(Rev, [], Vertices, [], Edges),
	wundgraph_add_vertices(Cliques0, Vertices, CliquesI),
	wundgraph_add_edges(CliquesI, Edges, CliquesF).

% stupid quadratic algorithm, needs to be improved.
get_links([], Vertices, Vertices, Edges, Edges).
get_links([Sz-Clique|Cliques], SoFar, Vertices, Edges0, Edges) :-
	add_clique_edges(SoFar, Clique, Sz, Edges0, EdgesI), !,
	get_links(Cliques, [Clique|SoFar], Vertices, EdgesI, Edges).
get_links([_|Cliques], SoFar, Vertices, Edges0, Edges) :-
	get_links(Cliques, SoFar, Vertices, Edges0, Edges).

add_clique_edges([], _, _, Edges, Edges).
add_clique_edges([Clique1|Cliques], Clique, Sz, Edges0, EdgesF) :-
	ord_intersection(Clique1, Clique, Int),
	Int \== Clique,
	(Int = [] ->
	  add_clique_edges(Cliques, Clique, Sz, Edges0, EdgesF)
	;
	  % we connect
	  length(Int, LSz),
	  add_clique_edges(Cliques, Clique, Sz, [Clique-(Clique1-LSz)|Edges0], EdgesF)
	).

root(WTree, JTree) :-
	wundgraph_to_undgraph(WTree, Tree),
	remove_leaves(Tree, SmallerTree),
	undgraph_vertices(SmallerTree, InnerVs),
	pick_root(InnerVs, Root),
	rb_new(M0),
	build_tree(Root, M0, Tree, JTree, _).

remove_leaves(Tree, SmallerTree) :-
	undgraph_vertices(Tree, Vertices),
	Vertices = [_,_,_|_],
	get_leaves(Vertices, Tree, Leaves),
	Leaves = [_|_], !,
	undgraph_del_vertices(Tree, Leaves, NTree),
	remove_leaves(NTree, SmallerTree).
remove_leaves(Tree, Tree).

get_leaves([], _, []).
get_leaves([V|Vertices], Tree, [V|Leaves]) :-
	undgraph_neighbors(V, Tree, [_]), !,
	get_leaves(Vertices, Tree, Leaves).
get_leaves([_|Vertices], Tree, Leaves) :-
	get_leaves(Vertices, Tree, Leaves).

pick_root([V|_],V).

direct_edges([], _, [], []) :- !.
direct_edges([], NewVs, RemEdges, Directed) :-
	direct_edges(RemEdges, NewVs, [], Directed).
direct_edges([V1-V2|Edges], NewVs0, RemEdges, [V1-V2|Directed]) :-
	ord_memberchk(V1, NewVs0), !,
	ord_insert(NewVs0, V2, NewVs),
	direct_edges(Edges, NewVs, RemEdges, Directed).
direct_edges([V1-V2|Edges], NewVs0, RemEdges, [V2-V1|Directed]) :-
	ord_memberchk(V2, NewVs0), !,
	ord_insert(NewVs0, V1, NewVs),
	direct_edges(Edges, NewVs, RemEdges, Directed).
direct_edges([Edge|Edges], NewVs, RemEdges, Directed) :-
	direct_edges(Edges, NewVs, [Edge|RemEdges], Directed).


populate(CPTs, JTree, NewJTree) :-
	keysort(CPTs, KCPTs),
	populate_cliques(JTree, KCPTs, NewJTree, []).

populate_cliques(tree(Clique,Kids), CPTs, tree(Clique-MyCPTs,NewKids), RemCPTs) :-
	get_cpts(CPTs, Clique, MyCPTs, MoreCPTs),
	populate_trees_with_cliques(Kids, MoreCPTs, NewKids, RemCPTs).

populate_trees_with_cliques([], MoreCPTs, [], MoreCPTs).
populate_trees_with_cliques([Node|Kids], MoreCPTs, [NewNode|NewKids], RemCPts) :-
	populate_cliques(Node, MoreCPTs, NewNode, ExtraCPTs),
	populate_trees_with_cliques(Kids, ExtraCPTs, NewKids, RemCPts).


get_cpts([], _, [], []).
get_cpts([CPT|CPts], [], [], [CPT|CPts]) :- !.
get_cpts([[I|MCPT]-Info|CPTs], [J|Clique], MyCPTs, MoreCPTs) :-
	compare(C,I,J),
	(C == < ->
	  % our CPT cannot be a part of the clique.
	  MoreCPTs = [[I|MCPT]-Info|LeftoverCPTs],
	  get_cpts(CPTs, [J|Clique], MyCPTs, LeftoverCPTs)
	;
	  C == = ->
	    % our CPT cannot be a part of the clique.
	    get_cpt(MCPT, Clique, I, Info, MyCPTs, MyCPTs0, MoreCPTs, MoreCPTs0),
	    get_cpts(CPTs, [J|Clique], MyCPTs0, MoreCPTs0)
	  ;
	    % the first element in our CPT may not be in a clique
	    get_cpts([[I|MCPT]-Info|CPTs], Clique, MyCPTs, MoreCPTs)
	).

get_cpt(MCPT, Clique, I, Info, [[I|MCPT]-Info|MyCPTs], MyCPTs, MoreCPTs, MoreCPTs) :-
	ord_subset(MCPT, Clique), !.
get_cpt(MCPT, _, I, Info, MyCPTs, MyCPTs, [[I|MCPT]-Info|MoreCPTs], MoreCPTs).


translate_edges([], [], []).
translate_edges([E1-E2|Edges], [(E1-A)-(E2-B)|NEdges], [E1-A,E2-B|Vs]) :-
	translate_edges(Edges, NEdges, Vs).

match_vs(_,[]).
match_vs([K-A|Cls],[K1-B|KVs]) :-
	compare(C, K, K1),
	(C == = ->
	  A = B,
	  match_vs([K-A|Cls], KVs)
	;
	  C = < ->
	  match_vs(Cls,[K1-B|KVs])
	;
	  match_vs([K-A|Cls],KVs)
	).

fill_with_cpts(tree(Clique-Dists,Leafs), tree(Clique-NewDists,NewLeafs)) :-
	compile_cpts(Dists, Clique, NewDists),
	fill_tree_with_cpts(Leafs, NewLeafs).


fill_tree_with_cpts([], []).
fill_tree_with_cpts([L|Leafs], [NL|NewLeafs]) :-
	fill_with_cpts(L, NL),
	fill_tree_with_cpts(Leafs, NewLeafs).

transform([], []).
transform([Clique-Dists|Nodes],[Clique-NewDist|NewNodes]) :-
	compile_cpts(Dists, Clique, NewDist),
	transform(Nodes, NewNodes).

compile_cpts([Vs-dist(OVs,Id)|Dists], Clique, TAB) :-
	OVs = [_|Ps], !,
	get_dist_matrix(Id, Ps, _, _, TAB0),
	reorder_CPT(OVs, TAB0, Vs, TAB1, Sz1),
	multiply_dists(Dists,Vs,TAB1,Sz1,Vars2,ITAB),
	expand_CPT(ITAB,Vars2,Clique,TAB).
compile_cpts([], [V|Clique], TAB) :-
	unit_CPT(V, CPT0),
	expand_CPT(CPT0, [V], [V|Clique], TAB).

multiply_dists([],Vs,TAB,_,Vs,TAB).
multiply_dists([Vs-dist(OVs,Id)|Dists],MVs,TAB2,Sz2,FVars,FTAB) :-
	OVs = [_|Ps],
	get_dist_matrix(Id, Ps, _, _, TAB0),
	reorder_CPT(OVs, TAB0, Vs, TAB1, Sz1),
	multiply_CPTs(tab(TAB1,Vs,Sz1),tab(TAB2,MVs,Sz2),tab(TAB3,NVs,Sz),_),
	multiply_dists(Dists,NVs,TAB3,Sz,FVars,FTAB).

build_tree(Root, Leafs, WTree, tree(Root,Leaves), NewLeafs) :-
	rb_insert(Leafs, Root, [], Leafs0),
	undgraph_neighbors(Root, WTree, Children),
	build_trees(Children, Leafs0, WTree, Leaves, NewLeafs).

build_trees( [], Leafs, _, [], Leafs).
build_trees([V|Children], Leafs, WTree, NLeaves, NewLeafs) :-
	% back pointer
	rb_lookup(V, _, Leafs), !,
	build_trees(Children, Leafs, WTree, NLeaves, NewLeafs).
build_trees([V|Children], Leafs, WTree, [VT|NLeaves], NewLeafs) :-
	build_tree(V, Leafs, WTree, VT, Leafs1),
	build_trees(Children, Leafs1, WTree, NLeaves, NewLeafs).


propagate_evidence([], NewTree, NewTree).
propagate_evidence([e(V,P)|Evs], Tree0, NewTree) :-
	add_evidence_to_matrix(Tree0, V, P, Tree1), !,
	propagate_evidence(Evs, Tree1, NewTree).

add_evidence_to_matrix(tree(Clique-Dist,Kids), V, P, tree(Clique-NDist,Kids)) :-
	ord_memberchk(V, Clique), !,
	reset_CPT_that_disagrees(Dist, Clique, V, P, NDist).
add_evidence_to_matrix(tree(C,Kids), V, P, tree(C,NKids)) :-
	add_evidence_to_kids(Kids, V, P, NKids).

add_evidence_to_kids([K|Kids], V, P, [NK|Kids]) :-
	add_evidence_to_matrix(K, V, P, NK), !.
add_evidence_to_kids([K|Kids], V, P, [K|NNKids]) :-
	add_evidence_to_kids(Kids, V, P, NNKids).

message_passing(tree(Clique-Dist,Kids), tree(Clique-NDist,NKids)) :-
	get_CPT_sizes(Dist, Sizes),
	upward(Kids, Clique, tab(Dist, Clique, Sizes), IKids, ITab, 1),
	ITab = tab(NDist, _, _),
	nb_setval(cnt,0),
	downward(IKids, Clique, ITab, NKids).

upward([], _, Dist, [], Dist, _).
upward([tree(Clique1-Dist1,DistKids)|Kids], Clique, Tab, [tree(Clique1-(NewDist1,EDist1),NDistKids)|NKids], NewTab, Lev) :-
	get_CPT_sizes(Dist1, Sizes1),
	Lev1 is Lev+1,
	upward(DistKids, Clique1, tab(Dist1,Clique1,Sizes1), NDistKids, NewTab1, Lev1),
	NewTab1 = tab(NewDist1,_,_),
	ord_intersection(Clique1, Clique, Int),
	sum_out_from_CPT(Int, NewDist1, Clique1, Tab1),
	multiply_CPTs(Tab, Tab1, ITab, EDist1),
	upward(Kids, Clique, ITab, NKids, NewTab, Lev).

downward([], _, _, []).
downward([tree(Clique1-(Dist1,Msg1),DistKids)|Kids], Clique, Tab, [tree(Clique1-NDist1,NDistKids)|NKids]) :-
	get_CPT_sizes(Dist1, Sizes1),
	ord_intersection(Clique1, Clique, Int),
	Tab = tab(Dist,_,_),
	divide_CPTs(Dist, Msg1, Div),
	sum_out_from_CPT(Int, Div, Clique, STab),
	multiply_CPTs(STab, tab(Dist1, Clique1, Sizes1), NewTab, _),
	NewTab = tab(NDist1,_,_),
	downward(DistKids, Clique1, NewTab, NDistKids),
	downward(Kids, Clique, Tab, NKids).


get_margin(NewTree, LVs0, LPs) :-
	sort(LVs0, LVs),
	find_clique(NewTree, LVs, Clique, Dist),
	sum_out_from_CPT(LVs, Dist, Clique, tab(TAB,_,_)),
	reorder_CPT(LVs, TAB, LVs0, NTAB, _),
	normalise_CPT(NTAB, Ps),
	list_from_CPT(Ps, LPs).

find_clique(tree(Clique-Dist,_), LVs, Clique, Dist) :-
	ord_subset(LVs, Clique), !.
find_clique(tree(_,Kids), LVs, Clique, Dist) :-
	find_clique_from_kids(Kids, LVs, Clique, Dist).

find_clique_from_kids([K|_], LVs, Clique, Dist) :-
	find_clique(K, LVs, Clique, Dist), !.
find_clique_from_kids([_|Kids], LVs, Clique, Dist) :-
	find_clique_from_kids(Kids, LVs, Clique, Dist).


write_tree(I0, tree(Clique-(Dist,_),Leaves)) :- !,
	matrix:matrix_to_list(Dist,L),
	format('~*c ~w:~w~n',[I0,0' ,Clique,L]),
	I is I0+2,
	maplist(write_tree(I), Leaves).
write_tree(I0, tree(Clique-Dist,Leaves), I0) :-
	matrix:matrix_to_list(Dist,L),
	format('~*c ~w:~w~n',[I0,0' ,Clique, L]),
	I is I0+2,
	maplist(write_tree(I), Leaves).

write_subtree([], _).
write_subtree([Tree|Leaves], I) :-
	write_tree(Tree, I),
	write_subtree(Leaves, I).

