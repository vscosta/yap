
:- module(bnt, [do_bnt/3,
		check_if_bnt_done/1]).

:- use_module(library('clpbn/display'), [
	clpbn_bind_vals/3]).

:- use_module(library('clpbn/dists'), [
	get_dist_domain_size/2,
	get_dist_domain/2,
	get_dist_params/2]).

:- use_module(library(matlab), [start_matlab/1,
				close_matlab/0,
				matlab_on/0,
				matlab_eval_string/1,
				matlab_eval_string/2,
				matlab_matrix/4,
				matlab_sequence/3,
				matlab_initialized_cells/4,
				matlab_get_variable/2,
				matlab_call/2
			      ]).

:- use_module(library(dgraphs), [dgraph_new/1,
				dgraph_add_vertices/3,
				dgraph_add_edges/3,
				dgraph_top_sort/2,
				dgraph_vertices/2,
				dgraph_edges/2
			      ]).

:- yap_flag(write_strings,on).

% syntactic sugar for matlab_call.
:- op(800,yfx,<--).

G <-- Y :-
	matlab_call(Y,G).

:- attribute bnt_id/1.

:- dynamic bnt/1.

:- dynamic bnt_solver/1, bnt_path/1.

% belprop
bnt_solver(jtree).
% likelihood_weighting

bnt_path('/u/vitor/Yap/CLPBN/FullBNT-1.0.3/BNT').



/*****************************************

BNT uses:
  bnet
  dag
  discrete_nodes: which nodes are discrete (all by now),
  node_sizes
  engine
  evidence
  marg

*****************************************/


check_if_bnt_done(Var) :-
	get_atts(Var, [map(_)]).

do_bnt([], _, _) :- !.
do_bnt(QueryVars, AllVars, AllDiffs)  :-
	init_matlab,
	extract_graph(AllVars, Graph),
	dgraph_top_sort(Graph, SortedVertices),
	number_graph(SortedVertices, NumberedVertices, 0, Size),
	init_bnet(SortedVertices, NumberedVertices, Size),
	set_inference,
	add_evidence(SortedVertices, Size, NumberedVertices),
	marginalize(QueryVars, Ps),
	clpbn_bind_vals(QueryVars, Ps, AllDiffs).

% make sure MATLAB works.
init_matlab :-
	bnt(on), !.
init_matlab :-
	start_matlab,
	bnt_path(Path),
	atom_concat('cd ', Path, Command),
	matlab_eval_string(Command),
	matlab_eval_string('add_BNT_to_path',_),
	assert(bnt(on)).

start_matlab :-
	matlab_on, !.
start_matlab :-
	start_matlab('matlab -nojvm -nosplash').

extract_graph(AllVars, Graph) :-
	dgraph_new(Graph0),
	dgraph_add_vertices(AllVars, Graph0, Graph1),
	get_edges(AllVars,Edges),
	dgraph_add_edges(Edges, Graph1, Graph).
	
get_edges([],[]).
get_edges([V|AllVars],Edges) :-
	clpbn:get_atts(V, [dist(_,Parents)]),
	add_parent_child(Parents,V,Edges,Edges0),
	get_edges(AllVars,Edges0).

add_parent_child([],_,Edges,Edges).
add_parent_child([P|Parents],V,[P-V|Edges],Edges0) :-
	add_parent_child(Parents,V,Edges,Edges0).

number_graph([], [], I, I).
number_graph([V|SortedGraph], [I|Is], I0, IF) :-
	I is I0+1,
	put_atts(V, [bnt_id(I)]),
	number_graph(SortedGraph, Is, I, IF).
	
init_bnet(SortedGraph, NumberedGraph, Size) :-
	build_dag(SortedGraph, Size),
	matlab_sequence(1,Size,discrete_nodes),
	mksizes(SortedGraph, Size),
	bnet <-- mk_bnet(dag, node_sizes, \discrete, discrete_nodes),
	dump_cpts(SortedGraph, NumberedGraph).

build_dag(SortedVertices, Size) :-
	get_numbered_edges(SortedVertices, Edges),
	mkdag(Size, Edges).

get_numbered_edges([], []).
get_numbered_edges([V|SortedVertices], Edges) :-
	clpbn:get_atts(V, [dist(_,Ps)]),
	v2number(V,N),
	add_numbered_edges(Ps, N, Edges, Edges0),
	get_numbered_edges(SortedVertices, Edges0).

add_numbered_edges([], _, Edges, Edges).
add_numbered_edges([P|Ps], N, [PN-N|Edges], Edges0) :-
	v2number(P,PN),
	add_numbered_edges(Ps, N, Edges, Edges0).

v2number(V,N) :-
	get_atts(V,[bnt_id(N)]).

mkdag(N,Els) :-
	Tot is N*N,
	functor(Dag,dag,Tot),
	add_els(Els,N,Dag),
	Dag=..[_|L],
	addzeros(L),
	matlab_matrix(N,N,L,dag).

add_els([],_,_).
add_els([X-Y|Els],N,Dag) :-
	Pos is (X-1)*N+Y,
	arg(Pos,Dag,1),
	add_els(Els,N,Dag).

addzeros([]).
addzeros([0|L]) :- !,
	addzeros(L).
addzeros([1|L]) :-
	addzeros(L).

mksizes(SortedVertices, Size) :-
	get_szs(SortedVertices,Sizes),
	matlab_matrix(1,Size,Sizes,node_sizes).

get_szs([],[]).
get_szs([V|SortedVertices],[LD|Sizes]) :-
	clpbn:get_atts(V, [dist(Id,_)]),
	get_dist_domain_size(Id,LD),
	get_szs(SortedVertices,Sizes).

dump_cpts([], []).
dump_cpts([V|SortedGraph], [I|Is]) :-
	clpbn:get_atts(V, [dist(Id,_)]),
	get_dist_params(Id,CPT),
	mkcpt(bnet,I,CPT),
	dump_cpts(SortedGraph, Is).

mkcpt(BayesNet, V, Tab) :-
	(BayesNet.'CPD'({V})) <-- tabular_CPD(BayesNet,V,Tab).

set_inference :-
	bnt_solver(Solver),
	init_solver(Solver).

init_solver(jtree) :-
	engine <-- jtree_inf_engine(bnet).
init_solver(belprop) :-
	engine <-- belprop_inf_engine(bnet).
init_solver(likelihood_weighting) :-
	engine <-- likelihood_weighting_inf_engine(bnet).
init_solver(enumerative) :-
	engine <-- enumerative_inf_engine(bnet).
init_solver(gibbs) :-
	engine <-- gibbs_inf_engine(bnet).
init_solver(global_joint) :-
	engine <-- global_joint_inf_engine(bnet).
init_solver(pearl) :-
	engine <-- pearl_inf_engine(bnet).
init_solver(var_elim) :-
	engine <-- var_elim_inf_engine(bnet).

add_evidence(Graph, Size, Is) :-
	mk_evidence(Graph, Is, LN),
	matlab_initialized_cells( 1, Size, LN, evidence),
	[engine, loglik] <-- enter_evidence(engine, evidence).

mk_evidence([], [], []).
mk_evidence([V|L], [I|Is], [ar(1,I,Val)|LN]) :-
	clpbn:get_atts(V, [evidence(Ev),dist(Id,_)]), !,
	get_dist_domain(Id, Domain),
	evidence_val(Ev,1,Domain,Val),
	mk_evidence(L, Is, LN).
mk_evidence([_|L], [_|Is], LN) :-
	mk_evidence(L, Is, LN).
	
evidence_val(Ev,Val,[Ev|_],Val) :- !.
evidence_val(Ev,I0,[_|Domain],Val) :-
	I1 is I0+1,
	evidence_val(Ev,I1,Domain,Val).

marginalize([V], Ps) :- !,
	v2number(V,Pos),
	marg <-- marginal_nodes(engine, Pos),
	matlab_get_variable( marg.'T', Ps).

