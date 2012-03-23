
/************************************************

  BDDs in CLP(BN)

A variable is represented by the N possible cases it can take

V = v(Va, Vb, Vc)

The generic formula is

V <- X, Y 

Va <- P*X1*Y1 + Q*X2*Y2 + ...


 
**************************************************/

:- module(clpbn_bdd,
          [bdd/3,
           set_solver_parameter/2,
           init_bdd_solver/4,
           run_bdd_solver/3,
           finalize_bdd_solver/1,
	   check_if_bdd_done/1
          ]).


:- use_module(library('clpbn/dists'),
          [dist/4,
           get_dist_domain/2,
           get_dist_domain_size/2,
           get_dist_params/2
          ]).


:- use_module(library('clpbn/display'),
         [clpbn_bind_vals/3]).

:- use_module(library('clpbn/aggregates'),
	      [check_for_agg_vars/2]).

:- use_module(library(atts)).

:- use_module(library(lists)).

:- use_module(library(dgraphs)).

:- use_module(library(bdd)).

:- use_module(library(rbtrees)).

:- attribute id/1.

:- dynamic network_counting/1.


check_if_bdd_done(_Var).

bdd([[]],_,_) :- !.
bdd([QueryVars], AllVars, AllDiffs) :-
	init_bdd_solver(_, AllVars, _, BayesNet),
	run_bdd_solver([QueryVars], LPs, BayesNet),
	finalize_bdd_solver(BayesNet),
	clpbn_bind_vals([QueryVars], [LPs], AllDiffs).

init_bdd_solver(_, AllVars0, _, bdd(Term, Leaves)) :-
	check_for_agg_vars(AllVars0, AllVars1),
	sort_vars(AllVars1, AllVars, Leaves),
	rb_new(Vars0),
	rb_new(Pars0),
	get_vars_info(AllVars, Vars0, _Vars, Pars0, _Pars, Term, []).

sort_vars(AllVars0, AllVars, Leaves) :-
	dgraph_new(Graph0),
	build_graph(AllVars0, Graph0, Graph),
	dgraph_leaves(Graph, Leaves),
	dgraph_top_sort(Graph, RAllVars),
	reverse(RAllVars, AllVars).

build_graph([], Graph, Graph).
build_graph(V.AllVars0, Graph0, Graph) :-
	clpbn:get_atts(V, [dist(_DistId, Parents)]), !,
	dgraph_add_vertex(Graph0, V, Graph1), 
	add_parents(Parents, V, Graph1, GraphI),
	build_graph(AllVars0, GraphI, Graph).
build_graph(_V.AllVars0, Graph0, Graph) :-
	build_graph(AllVars0, Graph0, Graph).

add_parents([], _V, Graph, Graph).
add_parents(V0.Parents, V, Graph0, GraphF) :-
	dgraph_add_edge(Graph0, V0, V, GraphI), 
	add_parents(Parents, V, GraphI, GraphF).


get_vars_info([], Vs, Vs, Ps, Ps) --> [].
get_vars_info([V|MoreVs], Vs, VsF, Ps, PsF) -->
	{ clpbn:get_atts(V, [dist(DistId, Parents)]) }, !,
	[DIST],
	{ check_p(DistId, Parms, _ParmVars, Ps, Ps1),
	  unbound_parms(Parms, ParmVars),
	  check_v(V, DistId, DIST, Vs, Vs1),
	  DIST = info(V, Tree, Ev, Values, Formula, ParmVars, Parms),
	  get_parents(Parents, PVars, Vs1, Vs2),
	  cross_product(Values, Ev, PVars, ParmVars, Formula0),
	  get_evidence(V, Tree, Ev, Formula0, Formula)
%	  (numbervars(Formula,0,_),writeln(formula:Formula), fail ; true)
        },
	get_vars_info(MoreVs, Vs2, VsF, Ps1, PsF).
get_vars_info([_|MoreVs], Vs0, VsF, Ps0, PsF, VarsInfo) :-
	get_vars_info(MoreVs, Vs0, VsF, Ps0, PsF, VarsInfo).

%
% look for parameters in the rb-tree, or add a new.
% distid is the key
%
check_p(DistId, Parms, ParmVars, Ps, Ps) :-
	rb_lookup(DistId, theta(Parms, ParmVars), Ps), !.
check_p(DistId, Parms, ParmVars, Ps, PsF) :-
	get_dist_params(DistId, Parms0),
	length(Parms0, L0),
	get_dist_domain_size(DistId, Size),
	L1 is L0 div Size,
	L is L0-L1,
	initial_maxes(L1, Multipliers),
	copy(L, Multipliers, NextMults, NextMults, Parms0, Parms, ParmVars),
	rb_insert(Ps, DistId, theta(DistId, Parms, ParmVars), PsF).

%
% we are using switches by two
%
initial_maxes(0, []) :- !.
initial_maxes(Size, [1.0|Multipliers]) :- !,
	Size1 is Size-1,
	initial_maxes(Size1, Multipliers).

copy(0, [], [], _, _Parms0, [], []) :- !.
copy(N, [], [], Ms, Parms0, Parms, ParmVars) :-!,
	copy(N, Ms, NewMs, NewMs, Parms0, Parms, ParmVars).
copy(N, D.Ds, ND.NDs, New, El.Parms0, NEl.Parms, _.ParmVars) :-
	N1 is N-1,
	NEl is El/D,
	ND is D-El,
	copy(N1, Ds, NDs, New, Parms0, Parms, ParmVars).

unbound_parms([], []).
unbound_parms(_.Parms, _.ParmVars) :-
	  unbound_parms(Parms, ParmVars).

check_v(V, _, INFO, Vs, Vs) :-
	rb_lookup(V, INFO, Vs), !.
check_v(V, DistId, INFO, Vs0, Vs) :-
	get_dist_domain_size(DistId, Size),
	length(Values, Size),
	length(Ev, Size),
	INFO = info(V, _Tree, Ev, Values, _Formula, _, _),
	rb_insert(Vs0, V, INFO, Vs).

get_parents([], [], Vs, Vs).
get_parents(V.Parents, Values.PVars, Vs0, Vs) :-
	clpbn:get_atts(V, [dist(DistId, _)]),
	check_v(V, DistId, INFO, Vs0, Vs1),
	INFO = info(V, _Parent, _Ev, Values, _, _, _),
	get_parents(Parents, PVars, Vs1, Vs).

%
% construct the formula, this is the key...
%
cross_product(Values, Ev, PVars, ParmVars, Formulas) :-
	arrangements(PVars, Arranges),
	apply_parents_first(Values, Ev, ParmCombos, ParmCombos, Arranges, Formulas, ParmVars).

%
% if we have the parent variables with two values, we get
% [[XP,YP],[XP,YN],[XN,YP],[XN,YN]]
%
arrangements([], [[]]).
arrangements([L1|Ls],O) :-
	arrangements(Ls, LN),
	expand(L1, LN, O, []).

expand([], _LN) --> [].
expand([H|L1], LN) -->
	concatenate_all(H, LN),
	expand(L1, LN).

concatenate_all(_H, []) --> [].
concatenate_all(H, L.LN) -->
	[[H|L]],
	concatenate_all(H, LN).

%
% core of algorithm
%
% Values -> Output Vars for BDD
% Es -> Evidence variables
% Previous -> top of difference list with parameters used so far
% P0 -> end of difference list with parameters used so far
% Pvars -> Parents
% Eqs -> Output Equations
% Pars -> Output Theta Parameters
%
apply_parents_first([Value], [E], Previous, [], PVars, [Value=Disj*E], Parameters) :- !,
	apply_last_parent(PVars, Previous, Disj),
	flatten(Previous, Parameters).
apply_parents_first([Value|Values], [E|Ev], Previous, P0, PVars, (Value=Disj*E).Formulas, Parameters) :-
	P0 = [TheseParents|End],
	apply_first_parent(PVars, Disj, TheseParents),
	apply_parents_second(Values, Ev, Previous, End, PVars, Formulas, Parameters).

apply_parents_second([Value], [E], Previous, [], PVars, [Value=Disj*E], Parameters) :- !,
	apply_last_parent(PVars, Previous, Disj),
	flatten(Previous, Parameters).
apply_parents_second([Value|Values], [E|Ev], Previous, P0, PVars, (Value=Disj*E).Formulas, Parameters) :-
	apply_middle_parent(PVars, Previous, Disj, TheseParents),
	% this must be done after applying middle parents because of the var
	% test.
	P0 = [TheseParents|End],
	apply_parents_second(Values, Ev, Previous, End, PVars, Formulas, Parameters).

apply_first_parent([Parents], Conj, [Theta]) :- !,
	parents_to_conj(Parents,Theta,Conj).
apply_first_parent(Parents.PVars, Disj+Conj, Theta.TheseParents) :-
	parents_to_conj(Parents,Theta,Conj),
	apply_first_parent(PVars, Disj, TheseParents).

apply_last_parent([Parents], Other, Conj) :- !,
	parents_to_conj(Parents,(Theta),Conj),
	skim_for_theta(Other, Theta, _, _).
apply_last_parent(Parents.PVars, Other, Conj+Disj) :-
	parents_to_conj(Parents,(Theta),Conj),
	skim_for_theta(Other, Theta, Remaining, _),
	apply_last_parent(PVars, Remaining, Disj).

apply_middle_parent([Parents], Other, Conj, [ThetaPar]) :- !,
	skim_for_theta(Other, Theta, _, ThetaPar),
	parents_to_conj(Parents,Theta,Conj),
	commit_deterministic(ThetaPar, Theta).
apply_middle_parent(Parents.PVars, Other, Conj+Disj, ThetaPar.TheseParents) :-
	skim_for_theta(Other, Theta, Remaining, ThetaPar),
	parents_to_conj(Parents,(Theta),Conj),
	commit_deterministic(ThetaPar, Theta),
	apply_middle_parent(PVars, Remaining, Disj, TheseParents).

%
%
% simplify stuff, removing process that is cancelled by 0s
%
parents_to_conj([], Theta, Theta) :- !.
parents_to_conj(Ps, Theta, Conj*Theta) :-
	parents_to_conj2(Ps, Conj).

parents_to_conj2([P],P) :- !.
parents_to_conj2(P.Ps,Conj*P) :-
	parents_to_conj2(Ps,Conj).

%
% don't need variables for deterministic parameters of CPTs.
%
commit_deterministic(1, 1) :- !.
commit_deterministic(0, 0) :- !.
commit_deterministic(1.0, 1) :- !.
commit_deterministic(0.0, 0) :- !.
commit_deterministic(_ThetaPar, _Theta).

%
% first case we haven't reached the end of the list so we need
% to create a new parameter variable
%
skim_for_theta([[P|Other]|V], New*not(P), [Other|_], New) :- var(V), !.
%
% last theta, it is just negation of the other ones
%
skim_for_theta([[P|Other]], not(P), [Other], _) :- !.
%
% recursive case, build-up
%
skim_for_theta([[P|Other]|More], Ps*not(P), [Other|Left], New ) :-
	skim_for_theta(More, Ps, Left, New ).

get_evidence(V, Tree, Values, F0, (Tree=Outs).F0) :-
	clpbn:get_atts(V, [evidence(Pos)]), !,
	zero_pos(0, Pos, Values),
	get_outs(F0, Outs).
%% no evidence !!!
get_evidence(_V, Tree, _Values, F0, (Tree=Outs).F0) :-
	get_outs(F0, Outs).

zero_pos(_, _Pos, []).
zero_pos(Pos, Pos, 1.Values) :- !, 
	I is Pos+1,
	zero_pos(I, Pos, Values).
zero_pos(I0, Pos, 0.Values) :- 
	I is I0+1,
	zero_pos(I, Pos, Values).

get_outs([V=_F], V) :- !.
get_outs((V=_F).Outs, (V + F0)) :-
	get_outs(Outs, F0).

run_bdd_solver([[V]], LPs, bdd(Term, Leaves)) :-
	build_out_node(Term, Leaves, Node),
	findall(Prob, get_prob(Term, Node, V, Prob),TermProbs),
	sumlist(TermProbs, Sum),
	normalise(TermProbs, Sum, LPs).

build_out_node(Term, [Leaf], Top) :- !,
	find_exp(Leaf, Term, Top).
build_out_node(Term, [Leaf|Leaves], Tops*Top) :-
	find_exp(Leaf, Term, Top),
	build_out_node(Term, Leaves, Tops).

find_exp(Leaf, info(V, Top, _Ev, _Values, _Formula, _ParmVars, _Parms)._, Top) :-
	V == Leaf, !.
find_exp(Leaf, _.Term, Top) :-
	find_exp(Leaf, Term, Top).

get_prob(Term, Top, V, SP) :-
	bind_all(Term, [], Bindings, V, AllParms, AllParmValues),
	term_variables(AllParms, NVs0),
	reverse(NVs0, NVs),
	build_bdd(Bindings, NVs, AllParms, AllParmValues, Bdd),
	bdd_to_probability_sum_product(Bdd, SP),
	bdd_close(Bdd).

build_bdd(Bindings, NVs, VTheta, Theta, Bdd) :-
	bdd_from_list(Bindings, NVs, Bdd),
	bdd_tree(Bdd, bdd(_F,Tree,_Vs)), length(Tree, Len),
	VTheta = Theta,
	writeln(length=Len).
        
bind_all([], Binds, Binds, _V, [], []).
bind_all(info(V, _Tree, Ev, _Values, Formula, ParmVars, Parms).Term, Binds, BindsF, V0, ParmVars.AllParms, Parms.AllTheta) :-
	V0 == V, !,
	set_to_one_zeros(Ev),
	bind_formula(Formula, Binds, BindsI),
	bind_all(Term, BindsI, BindsF, V0, AllParms, AllTheta).
bind_all(info(_V, _Tree, Ev, _Values, Formula, ParmVars, Parms).Term, Binds, BindsF, V0, ParmVars.AllParms, Parms.AllTheta) :-
	set_to_ones(Ev),!,
	bind_formula(Formula, Binds, BindsI),
	bind_all(Term, BindsI, BindsF, V0, AllParms, AllTheta).
% evidence: no need to add any stuff.
bind_all(info(_V, _Tree, _Ev, _Values, Formula, ParmVars, Parms).Term, Binds, BindsF, V0, ParmVars.AllParms, Parms.AllTheta) :-
	bind_formula(Formula, Binds, BindsI),
	bind_all(Term, BindsI, BindsF, V0, AllParms, AllTheta).

bind_formula([], L, L).
bind_formula(B.Formula, Bs0, BsF) :-
	bind_formula(Formula, B.Bs0, BsF).

set_to_one_zeros([1|Values]) :-
	set_to_zeros(Values).
set_to_one_zeros([0|Values]) :-
	set_to_one_zeros(Values).

set_to_zeros([]).
set_to_zeros(0.Values) :-
	set_to_zeros(Values).

set_to_ones([]).
set_to_ones(1.Values) :-
	set_to_ones(Values).

normalise([], _Sum, []).
normalise(P.TermProbs, Sum, NP.LPs) :-
	NP is P/Sum,
	normalise(TermProbs, Sum, LPs).

finalize_bdd_solver(_).

