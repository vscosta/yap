/*
cd ~/Yap/CLPBN/FullBNT-1.0.4
addpath(genpathKPM(pwd))
N = 4; 
dag = false(N,N);
C = 1; S = 2; R = 3; W = 4;
dag(C,[R S]) = 1;
dag(R,W) = 1;
dag(S,W)=1;
discrete_nodes = 1:N;
node_sizes = 2*ones(1,N); 
bnet = mk_bnet(dag, node_sizes, 'discrete', discrete_nodes);
bnet.CPD{W} = tabular_CPD(bnet, W, 'CPT', [1 0.1 0.1 0.01 0 0.9 0.9 0.99]);
bnet.CPD{C} = tabular_CPD(bnet, C, [0.5 0.5]);
bnet.CPD{R} = tabular_CPD(bnet, R, [0.8 0.2 0.2 0.8]);
bnet.CPD{S} = tabular_CPD(bnet, S, [0.5 0.9 0.5 0.1]);
engine = jtree_inf_engine(bnet);
evidence = cell(1,N);
evidence{W} = 2;
[engine, loglik] = enter_evidence(engine, evidence);
marg = marginal_nodes(engine, S);
marg.T
*/

:- ensure_loaded(library(matlab)).

:- yap_flag(write_strings, on).

% syntactic sugar for matlab_call.
:- op(800,yfx,<--).

G <-- Y :-
	matlab_call(Y,G).

do(Out,Out2) :-
	init_bnt,
	N = 4, 
	C = 1, S = 2, R = 3, W = 4,
	mkdag(N,[C-R,C-S,R-W,S-W]),
	matlab_sequence(1,N,discrete_nodes),
	mk2s(N,L), % domain has size 2
	matlab_matrix(1,N,L,node_sizes),
	bnet <-- mk_bnet(dag, node_sizes, \discrete, discrete_nodes),
	mkcpt(bnet,W,[1, 0.1, 0.1, 0.01, 0, 0.9, 0.9, 0.99]),
	mkcpt(bnet,C,[0.5, 0.5]),
	mkcpt(bnet,R,[0.8, 0.2, 0.2, 0.8]),
	mkcpt(bnet,S,[0.5, 0.9, 0.5, 0.1]),
	engine <-- jtree_inf_engine(bnet),
	mkevidence(N,[W-2]),
	marg <-- marginal_nodes(engine, S),
	matlab_get_variable( marg.'T', Out),
	add_evidence([R-2]),
	marg <-- marginal_nodes(engine, S),
	matlab_get_variable( marg.'T', Out2).

init_bnt :-
	matlab_on, !.
init_bnt :-
	getcwd(D),
	cd('~/Yap/CLPBN/FullBNT/BNT'),
	start_matlab('matlab -nojvm -nosplash'),
	matlab_eval_string("add_BNT_to_path",_),
	cd(D).
	
mk2s(0, []) :- !.
mk2s(I, [2|L]) :-
	I0 is I-1,
	mk2s(I0, L).

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

mkcpt(BayesNet, V, Tab) :-
	(BayesNet.'CPD'({V})) <-- tabular_CPD(BayesNet,V,Tab).

mkevidence(N,L) :-
	mkeventries(L,LN),
	matlab_initialized_cells( 1, N, LN, evidence),
	[engine, loglik] <-- enter_evidence(engine, evidence).

mkeventries([],[]).
mkeventries([A-V|L],[ev(1,A,V)|LN]) :-
	mkeventries(L,LN).

add_evidence(L) :-
	add_to_evidence(L),
	[engine, loglik] <-- enter_evidence(engine, evidence).

add_to_evidence([]).
add_to_evidence([Pos-Val|L]) :-
	matlab_item1(evidence,Pos,Val),
	add_to_evidence(L).
	
