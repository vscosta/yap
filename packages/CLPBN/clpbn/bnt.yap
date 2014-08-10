:- module(bnt,
		[do_bnt/3,
		 create_bnt_graph/2,
		 check_if_bnt_done/1
		]).

:- use_module(library('clpbn/display'),
		[clpbn_bind_vals/3]).

:- use_module(library('clpbn/dists'),
		[get_dist_domain_size/2,
		 get_dist_domain/2,
		 get_dist_params/2
		]).

:- use_module(library('clpbn/discrete_utils'),
		[reorder_CPT/5]).

:- use_module(library('atts')).

:- use_module(library(matlab),
		[start_matlab/1,
		 close_matlab/0,
		 matlab_on/0,
		 matlab_eval_string/1,
		 matlab_eval_string/2,
		 matlab_matrix/4,
		 matlab_vector/2,
		 matlab_sequence/3,
		 matlab_initialized_cells/4,
		 matlab_get_variable/2,
		 matlab_call/2
		]).

:- use_module(library(dgraphs),
		[dgraph_new/1,
		 dgraph_add_vertices/3,
		 dgraph_add_edges/3,
		 dgraph_top_sort/2,
		 dgraph_vertices/2,
		 dgraph_edges/2
		]).

:- use_module(library(lists),
		[append/3,
		 member/2,nth/3
		]).

:- use_module(library(ordsets),
		[ord_insert/3]).

:- yap_flag(write_strings,on).

% syntactic sugar for matlab_call.
:- op(800,yfx,<--).

G <-- Y :-
	matlab_call(Y,G).

:- attribute bnt_id/1.

:- dynamic bnt/1.

:- dynamic bnt_solver/1, bnt_path/1, bnt_model/1.

% belprop
bnt_solver(jtree).
% likelihood_weighting

bnt_path("$HOME/Yap/CLPBN/FullBNT-1.0.4/BNT").


%
% What BNT are we using:
%  a propositional one
%  a tied parameter one.
%
%bnt_model(propositional).
bnt_model(tied).
%bnt_model(dbn).

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
	create_bnt_graph(AllVars, _, SortedVertices, NumberedVertices, Size),
	set_inference,
	add_evidence(SortedVertices, Size, NumberedVertices),
	marginalize(QueryVars, SortedVertices, NumberedVertices, Ps),
	clpbn_bind_vals(QueryVars, Ps, AllDiffs).

create_bnt_graph(AllVars, Representatives) :-
	create_bnt_graph(AllVars, Representatives, _, _, _).

create_bnt_graph(AllVars, Representatives, SortedVertices, NumberedVertices, Size) :-
	init_matlab,
	sort_nodes(AllVars, SortedVertices),
	number_graph(SortedVertices, NumberedVertices, 0, Size),
	bnt_model(ModelType),
	init_bnet(ModelType, SortedVertices, NumberedVertices, Size, Representatives).


% make sure MATLAB works.

init_matlab :-
	bnt(on), !.
init_matlab :-
	start_matlab,
	bnt_path(Path),
	append("cd ",Path,Command),
%	atom_concat('cd ', Path, Command),
	matlab_eval_string(Command),
	matlab_eval_string('addpath(genpathKPM(pwd))',_),
	assert(bnt(on)).


start_matlab :-
	matlab_on, !.
start_matlab :-
	start_matlab('matlab -nojvm -nosplash').

sort_nodes(AllVars, SortedVertices) :-
	bnt_model(tied), !,
	extract_tied(AllVars, SortedVertices).
sort_nodes(AllVars, SortedVertices) :-
	bnt_model(propositional), !,
	extract_graph(AllVars, Graph),
	dgraph_top_sort(Graph, SortedVertices).

extract_tied(AllVars, SortedVars) :-
	extract_kvars(AllVars,KVars),
	keysort(KVars,SVars),
	split_tied_vars(SVars,TVars, Vertices),
	tied_graph(TVars,TGraph,Vertices),
	dgraph_top_sort(TGraph, Sort),
	distribute_tied_variables(Sort, TVars, 1, SortedVars).

extract_kvars([],[]).
extract_kvars([V|AllVars],[N-i(V,Parents)|KVars]) :-
	clpbn:get_atts(V, [dist(N,Parents)]),
	extract_kvars(AllVars,KVars).

split_tied_vars([],[],[]).
split_tied_vars([N-i(V,Par)|More],[N-g(Vs,Ns,Es)|TVars],[N|LNs]) :-
	get_pars(Par,N,V,NPs,[],Es0,Es),
	get_tied(More,N,Vs,[V],Ns,NPs,Es,Es0,SVars),
	split_tied_vars(SVars,TVars,LNs).

get_pars([],_,_,NPs,NPs,Es,Es).
get_pars([V|Par],N,V0,NPs,NPs0,Es,Es0) :-
	clpbn:get_atts(V, [dist(N,_)]), !,
	get_pars(Par,N,V0,NPs,NPs0,Es,[V-V0|Es0]).
get_pars([V|Par],N,V0,NPs,NPs0,Es,Es0) :-
	clpbn:get_atts(V, [dist(M,_)]),
	ord_insert(NPs0,M,NPsI),
	get_pars(Par,N,V0,NPs,NPsI,Es,Es0).

get_tied([N-i(V,Par)|More],N,Vs,Vs0,Ns,NPs,Es,Es0,SVars) :- !,
	get_pars(Par,N,V,NPsI,NPs,EsI,Es0),
	get_tied(More,N,Vs,[V|Vs0],Ns,NPsI,Es,EsI,SVars).
get_tied(More,_,Vs,Vs,Ns,Ns,Es,Es,More).

tied_graph(TVars,Graph,Vertices) :-
	dgraph_new(Graph0),
	dgraph_add_vertices(Graph0, Vertices, Graph1),
	get_tied_edges(TVars,Edges),
	dgraph_add_edges(Graph1, Edges, Graph).

get_tied_edges([],[]).
get_tied_edges([N-g(_,Vs,_)|TGraph],Edges) :-
	add_tied(Vs,N,Edges,Edges0),
	get_tied_edges(TGraph,Edges0).

add_tied([],_,Edges,Edges).
add_tied([N1|Vs],N,[N1-N|Edges],Edges0) :-
	add_tied(Vs,N,Edges,Edges0).

distribute_tied_variables([], _, _, []).
distribute_tied_variables([N|Sort], TVars, I0, SortedVars) :-
	member(N-g(Vs,_,_),TVars),
	distribute_tied(Vs,I0,In,SortedVars,SortedVars0),
	distribute_tied_variables(Sort, TVars, In, SortedVars0).

distribute_tied([],I,I,Vs,Vs).
distribute_tied([V|Vs],I0,In,[V|NVs],NVs0) :-
	I is I0+1,
	put_atts(V, [bnt_id(I0)]),
%	clpbn:get_atts(V,[key(K)]),
	distribute_tied(Vs,I,In,NVs,NVs0).

extract_graph(AllVars, Graph) :-
	dgraph_new(Graph0),
	dgraph_add_vertices(Graph0, AllVars, Graph1),
	get_edges(AllVars,Edges),
	dgraph_add_edges(Graph1, Edges, Graph).

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
%	clpbn:get_atts(V,[key(K)]),
%	write(I:K),nl,
	number_graph(SortedGraph, Is, I, IF).

init_bnet(propositional, SortedGraph, NumberedGraph, Size, []) :-
	build_dag(SortedGraph, Size),
	init_discrete_nodes(SortedGraph, Size),
	bnet <-- mk_bnet(dag, node_sizes, \discrete, discrete_nodes),
	dump_cpts(SortedGraph, NumberedGraph).

init_bnet(tied, SortedGraph, NumberedGraph, Size, Representatives) :-
	build_dag(SortedGraph, Size),
	init_discrete_nodes(SortedGraph, Size),
	dump_tied_cpts(SortedGraph, NumberedGraph, Representatives).

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

init_discrete_nodes(SortedGraph, Size) :-
	matlab_sequence(1,Size,discrete_nodes),
	mksizes(SortedGraph, Size).

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
	clpbn:get_atts(V, [dist(Id,Parents)]),
	get_dist_params(Id,CPT),
	reorder_cpt(CPT,V,Parents,Tab),
	mkcpt(bnet,I,Tab),
	dump_cpts(SortedGraph, Is).

%
% This is complicated, the BNT and we have different orders
%
reorder_cpt(CPT,_, [], CPT) :- !.
reorder_cpt(CPT,V,Parents,Tab) :-
	% get BNT label
	get_sizes_and_ids(Parents,Ids),
	% sort to BNT
	keysort(Ids,NIds),
	% get vars in order
	extract_vars(NIds, [], NParents),
	% do the actual work
	reorder_CPT([V|Parents],CPT,[V|NParents],STab,_),
	STab=..[_|Tab].

get_sizes_and_ids([],[]).
get_sizes_and_ids([V|Parents],[Id-V|Ids]) :-
	get_atts(V, [bnt_id(Id)]),
	get_sizes_and_ids(Parents,Ids).

extract_vars([], L, L).
extract_vars([_-V|NIds], NParents, Vs) :-
	extract_vars(NIds, [V|NParents], Vs).

mkcpt(BayesNet, I, Tab) :-
	(BayesNet.'CPD'({I})) <-- tabular_CPD(BayesNet,I,Tab).

dump_tied_cpts(Graph, Is, Reps) :-
	create_class_vector(Graph, Is, Classes, Reps0),
	matlab_vector(Classes, eclass),
	keysort(Reps0,Reps1),
	representatives(Reps1,Reps),
	bnet <-- mk_bnet(dag, node_sizes, \discrete, discrete_nodes, \equiv_class, eclass),
	dump_tied_cpts(Reps).

create_class_vector([], [], [],[]).
create_class_vector([V|Graph], [I|Is],  [Id|Classes], [Id-v(V,I,Parents)|Sets]) :-
	clpbn:get_atts(V, [dist(Id,Parents)]),
	create_class_vector(Graph, Is,Classes,Sets).

representatives([],[]).
representatives([Class-Rep|Reps1],[Class-Rep|Reps]) :-
	nonrepresentatives(Reps1, Class, Reps2),
	representatives(Reps2,Reps).

nonrepresentatives([Class-_|Reps1], Class, Reps2) :- !,
	nonrepresentatives(Reps1, Class, Reps2).
nonrepresentatives(Reps, _, Reps).


dump_tied_cpts([]).
dump_tied_cpts([Class-v(V,Id,Parents)|SortedGraph]) :-
	get_dist_params(Class,CPT),
	reorder_cpt(CPT,V,Parents,NCPT),
	mktiedcpt(bnet,Id,Class,NCPT),
	dump_tied_cpts(SortedGraph).

mktiedcpt(BayesNet, V, Class, Tab) :-
	(BayesNet.'CPD'({Class})) <-- tabular_CPD(BayesNet,V,Tab).

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
	engine <-- gibbs_sampling_inf_engine(bnet).
init_solver(global_joint) :-
	engine <-- global_joint_inf_engine(bnet).
init_solver(pearl) :-
	engine <-- pearl_inf_engine(bnet).
init_solver(var_elim) :-
	engine <-- var_elim_inf_engine(bnet).

add_evidence(Graph, Size, Is) :-
	mk_evidence(Graph, Is, LN),
	matlab_initialized_cells( 1, Size, LN, evidence),
	[engine_ev, loglik] <-- enter_evidence(engine, evidence).

mk_evidence([], [], []).
mk_evidence([V|L], [I|Is], [ar(1,I,EvVal1)|LN]) :-
	clpbn:get_atts(V, [evidence(EvVal)]), !,
	EvVal1 is EvVal +1,
	mk_evidence(L, Is, LN).
mk_evidence([_|L], [_|Is], LN) :-
	mk_evidence(L, Is, LN).

evidence_val(Ev,Val,[Ev|_],Val) :- !.
evidence_val(Ev,I0,[_|Domain],Val) :-
	I1 is I0+1,
	evidence_val(Ev,I1,Domain,Val).

marginalize([[V]], _SortedVars,_NunmberedVars, Ps) :- !,
	v2number(V,Pos),
	marg <-- marginal_nodes(engine_ev, Pos),
	matlab_get_variable( marg.'T', Ps).

marginalize([Vs], SortedVars, NumberedVars,Ps) :-
	bnt_solver(jtree),!,
	matlab_get_variable(loglik, Den),
	clpbn_display:get_all_combs(Vs, Vals),
	mk_evidence(SortedVars, NumberedVars, Ev),
	length(SortedVars,L),
	cycle_values(Den, Ev, Vs, L, Vals, Ps).

cycle_values(_D, _Ev, _Vs, _Size, [], []).

cycle_values(Den,Ev,Vs,Size,[H|T],[HP|TP]):-
	mk_evidence_query(Vs, H, EvQuery),
	append(EvQuery,Ev,Instantiation),
	matlab_initialized_cells( 1, Size, Instantiation, instantiation),
	[engine_ev, logll] <-- enter_evidence(engine, instantiation),
	matlab_get_variable(logll, Num),
	HP is exp(Num-Den),
	cycle_values(Den,Ev,Vs,Size,T,TP).

mk_evidence_query([], [], []).
mk_evidence_query([V|L], [H|T], [ar(1,Pos,El)|LN]) :-
	v2number(V,Pos),
	clpbn:get_atts(V, [dist(Id,_)]),
	get_dist_domain(Id,D),
	nth(El,D,H),
	mk_evidence_query(L, T, LN).

