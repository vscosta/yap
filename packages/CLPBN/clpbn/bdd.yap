
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

:- use_module(library(hacks)).

:- use_module(library(lists)).

:- use_module(library(dgraphs)).

:- use_module(library(bdd)).

:- use_module(library(rbtrees)).

:- dynamic network_counting/1.

:- attribute order/1.

check_if_bdd_done(_Var).

bdd([[]],_,_) :- !.
bdd([QueryVars], AllVars, AllDiffs) :-
	init_bdd_solver(_, AllVars, _, BayesNet),
	run_bdd_solver([QueryVars], LPs, BayesNet),
	finalize_bdd_solver(BayesNet),
	clpbn_bind_vals([QueryVars], [LPs], AllDiffs).

init_bdd_solver(_, AllVars0, _, bdd(Term, Leaves, Tops)) :-
%	check_for_agg_vars(AllVars0, AllVars1),
	sort_vars(AllVars0, AllVars, Leaves),
	order_vars(AllVars, 0),
	rb_new(Vars0),
	rb_new(Pars0),
	init_tops(Leaves,Tops),
	get_vars_info(AllVars, Vars0, _Vars, Pars0, _Pars, Leaves, Tops, Term, []).

order_vars([], _).
order_vars([V|AllVars], I0) :-
	put_atts(V, [order(I0)]),
	I is I0+1,
	order_vars(AllVars, I).


init_tops([],[]).
init_tops(_.Leaves,_.Tops) :-
	init_tops(Leaves,Tops).

sort_vars(AllVars0, AllVars, Leaves) :-
	dgraph_new(Graph0),
	build_graph(AllVars0, Graph0, Graph),
	dgraph_leaves(Graph, Leaves),
	dgraph_top_sort(Graph, AllVars).

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

get_vars_info([], Vs, Vs, Ps, Ps, _, _) --> [].
get_vars_info([V|MoreVs], Vs, VsF, Ps, PsF, Lvs, Outs) -->
	{ clpbn:get_atts(V, [dist(DistId, Parents)]) }, !,
%{writeln(v:DistId:Parents)},
	[DIST],
	{  get_var_info(V, DistId, Parents, Vs, Vs2, Ps, Ps1, Lvs, Outs, DIST) },
	get_vars_info(MoreVs, Vs2, VsF, Ps1, PsF, Lvs, Outs).
get_vars_info([_|MoreVs], Vs0, VsF, Ps0, PsF, VarsInfo, Lvs, Outs) :-
	get_vars_info(MoreVs, Vs0, VsF, Ps0, PsF, VarsInfo, Lvs, Outs).

%
% let's have some fun with avg
%
get_var_info(V, avg(Domain), Parents0, Vs, Vs2, Ps, Ps, Lvs, Outs, DIST) :- !,
	reorder_vars(Parents0, Parents),
	length(Domain, DSize),
	run_though_avg(V, DSize, Domain, Parents, Vs, Vs2, Lvs, Outs, DIST).
%	bup_avg(V, DSize, Domain, Parents, Vs, Vs2, Lvs, Outs, DIST).
% standard random variable
get_var_info(V, DistId, Parents, Vs, Vs2, Ps, Ps1, Lvs, Outs, DIST) :-
% clpbn:get_atts(V, [key(K)]), writeln(V:K:DistId:Parents),
	check_p(DistId, Parms, _ParmVars, Ps, Ps1),
	unbound_parms(Parms, ParmVars),
	check_v(V, DistId, DIST, Vs, Vs1),
	DIST = info(V, Tree, Ev, Values, Formula, ParmVars, Parms),
	% get a list of form [[P00,P01], [P10,P11], [P20,P21]]
	get_parents(Parents, PVars, Vs1, Vs2),
	cross_product(Values, Ev, PVars, ParmVars, Formula0),
%	(numbervars(Formula0,0,_),writeln(formula0:Ev:Formula0), fail ; true),
	get_evidence(V, Tree, Ev, Formula0, Formula, Lvs, Outs).
%,	(numbervars(Formula,0,_),writeln(formula:Formula), fail ; true)

reorder_vars(Vs, OVs) :-
	add_pos(Vs, PVs),
	keysort(PVs, SVs),
	remove_key(SVs, OVs).

add_pos([], []).
add_pos([V|Vs], [K-V|PVs]) :-
	get_atts(V,[order(K)]),
	add_pos(Vs, PVs).

remove_key([], []).
remove_key([_-V|SVs], [V|OVs]) :-
	remove_key(SVs, OVs).

%%%%%%%%%%%%%%%%%%%%%%%%%
%
% use top-down to generate average
%
run_though_avg(V, 3, Domain, Parents, Vs, Vs2, Lvs, Outs, DIST) :-
	check_v(V, avg(Domain,Parents), DIST, Vs, Vs1),
	DIST = info(V, Tree, Ev, [V0,V1,V2], Formula, [], []),
	get_parents(Parents, PVars, Vs1, Vs2),
	length(Parents, N),
	generate_3tree(F00, PVars, 0, 0, 0, N, N0, N1, N2, R, (N1+2*N2 =< N/2), (N1+2*(N2+R) =< N/2)),
	simplify_exp(F00, F0),
%	generate_3tree(F1, PVars, 0, 0, 0, N, N0, N1, N2, R, ((N1+2*(N2+R) > N/2, N1+2*N2 < (3*N)/2))),
	generate_3tree(F20, PVars, 0, 0, 0, N, N0, N1, N2, R, (N1+2*(N2+R) >= (3*N)/2), N1+2*N2 >= (3*N)/2),
%	simplify_exp(F20, F2),
	F20=F2,
	Formula0 = [V0=F0*Ev0,V2=F2*Ev2,V1=not(F0+F2)*Ev1],
	Ev = [Ev0,Ev1,Ev2],
	get_evidence(V, Tree, Ev, Formula0, Formula, Lvs, Outs).

generate_3tree(OUT, _, I00, I10, I20, IR0, N0, N1, N2, R, _Exp, ExpF) :-
	IR is IR0-1,
	satisf(I00, I10, I20, IR, N0, N1, N2, R, ExpF),
	!,
	OUT = 1.
generate_3tree(OUT, [[P0,P1,P2]], I00, I10, I20, IR0, N0, N1, N2, R, Exp, _ExpF) :-
	IR is IR0-1,
	( satisf(I00+1, I10, I20, IR, N0, N1, N2, R, Exp) ->
	  L0 = [P0|L1]
	  ;
	  L0 = L1
	),
	( satisf(I00, I10+1, I20, IR, N0, N1, N2, R, Exp) ->
	  L1 = [P1|L2]
	  ;
	  L1 = L2
	),
	( satisf(I00, I10, I20+1, IR, N0, N1, N2, R, Exp) ->
	  L2 = [P2]
	  ;
	  L2 = []
	),
	to_disj(L0, OUT).
generate_3tree(OUT, [[P0,P1,P2]|Ps], I00, I10, I20, IR0, N0, N1, N2, R, Exp, ExpF) :-
	IR is IR0-1,
	( satisf(I00+1, I10, I20, IR, N0, N1, N2, R, Exp) ->
	  I0 is I00+1, generate_3tree(O0, Ps, I0, I10, I20, IR, N0, N1, N2, R, Exp, ExpF)
	  ->
	  L0 = [P0*O0|L1]
	  ;
	  L0 = L1
	),
	( satisf(I00, I10+1, I20, IR0, N0, N1, N2, R, Exp) ->
	  I1 is I10+1, generate_3tree(O1, Ps, I00, I1, I20, IR, N0, N1, N2, R, Exp, ExpF)
	  ->
	  L1 = [P1*O1|L2]
	  ;
	  L1 = L2
	),
	( satisf(I00, I10, I20+1, IR0, N0, N1, N2, R, Exp) ->
	  I2 is I20+1, generate_3tree(O2, Ps, I00, I10, I2, IR, N0, N1, N2, R, Exp, ExpF)
	  ->	  
	  L2 = [P2*O2]
	  ;
	  L2 = []
	),
	to_disj(L0, OUT).


satisf(I0, I1, I2, IR, N0, N1, N2, R, Exp) :-
	\+ \+  ( I0 = N0, I1=N1, I2=N2, IR=R, call(Exp) ).

not_satisf(I0, I1, I2, IR, N0, N1, N2, R, Exp) :-
	\+  ( I0 = N0, I1=N1, I2=N2, IR=R, call(Exp) ).

%%%%%%%%%%%%%%%%%%%%%%%%%
%
% use bottom-up dynamic programming to generate average
%
bup_avg(V, Size, Domain, Parents, Vs, Vs2, Lvs, Outs, DIST) :-
	check_v(V, avg(Domain,Parents), DIST, Vs, Vs1),
	DIST = info(V, Tree, Ev, OVs, Formula, [], []),
	get_parents(Parents, PVars, Vs1, Vs2),
%	generate_sums(PVars, Size, Max, Sums, F0),
	bin_sums(PVars, Sums, F00),
	reverse(F00,F0),
	length(Parents, N),
	Max is (Size-1)*N, % This should be true
	% easier to do recursion on lists
	Sums =.. [_|LSums],
	generate_avg(0, Size, 0, Max, LSums, OVs, Ev, F1, []),
	reverse(F0, RF0),
	get_evidence(V, Tree, Ev, F1, F2, Lvs, Outs),
	append(RF0, F2, Formula).

bin_sums(Vs, Sums, F) :-
	 vs_to_sums(Vs, Sums0),
	 writeln(init:Sums0),
	 bin_sums(Sums0, Sums, F, []).
	
vs_to_sums([], []).
vs_to_sums([V|Vs], [Sum|Sums0]) :-
	  Sum =.. [sum|V],
	  vs_to_sums(Vs, Sums0).

bin_sums([Sum], Sum) --> !.
bin_sums(LSums, Sums) --> 
	pack_bins(LSums, Sums1),
	bin_sums(Sums1, Sums).

pack_bins([], []) --> [].
pack_bins([Sum], [Sum]) --> [].
pack_bins([Sum1,Sum2|LSums], [Sum|NSums]) -->
	sum(Sum1, Sum2, Sum),
	pack_bins(LSums, NSums).

sum(Sum1, Sum2, Sum) -->
	  { functor(Sum1, _, M1),
	    functor(Sum2, _, M2),
	    Max is M1+M2-2,
	    Max1 is Max+1,
	    Max0 is M2-1,
	    functor(Sum, sum, Max1),
	    Sum1 =.. [_|PVals] },
	  expand_sums(PVals, 0, Max0, Max1, M2, Sum2, Sum).

generate_sums([PVals], Size, Max, Sum, []) :- !,
	Max is Size-1,
	Sum =.. [sum|PVals].
generate_sums([PVals|Parents], Size, Max, NewSums, F) :-
	generate_sums(Parents, Size, Max0, Sums, F0),
	Max is Max0+(Size-1),
	Max1 is Max+1,
	functor(NewSums, sum, Max1),
	expand_sums(PVals, 0, Max0, Max1, Size, Sums, NewSums, F, F0).

%
% outer loop: generate array of sums at level j= Sum[j0...jMax]
%
expand_sums(_Parents, Max, _, Max, _Size, _Sums, _NewSums, F0, F0) :- !.
expand_sums(Parents, I0, Max0, Max, Size, Sums, NewSums, [O=SUM|F], F0) :-
	I is I0+1,
	arg(I, NewSums, O),
	sum_all(Parents, 0, I0, Max0, Sums, List),
	to_disj(List, SUM),
        expand_sums(Parents, I, Max0, Max, Size, Sums, NewSums, F, F0).

%
%inner loop: find all parents that contribute to A_ji,
% that is generate Pk*Sum_(j-1)l and k+l st k+l = i
%
sum_all([], _, _, _, _, []).
sum_all([V|Vs], Pos, I, Max0, Sums, [V*S0|List]) :-
	J is I-Pos,
	J >= 0,
	J =< Max0, !,
	J1 is J+1,
	arg(J1, Sums, S0),
	Pos1 is Pos+1,
	sum_all(Vs, Pos1, I, Max0, Sums, List).
sum_all([_V|Vs], Pos, I, Max0, Sums, List) :-
	Pos1 is Pos+1,
	sum_all(Vs, Pos1, I, Max0, Sums, List).
	
gen_arg(J, Sums, Max, S0) :-
	gen_arg(0, Max, J, Sums, S0).
	
gen_arg(Max, Max, J, Sums, S0) :- !,
	     I is Max+1,
	     arg(I, Sums, A),
	( Max = J -> S0 = A ; S0 = not(A)).
gen_arg(I0, Max, J, Sums, S) :-
	     I is I0+1,
	     arg(I, Sums, A),
	( I0 = J -> S = A*S0 ; S = not(A)*S0),
	gen_arg(I, Max, J, Sums, S0).


generate_avg(Size, Size, _J, _Max, [], [], [], F, F).
generate_avg(I0, Size, J0, Max, LSums, [O|OVs], [Ev|Evs], [O=Ev*Disj|F], F0) :-
	I is I0+1,
	Border is (I*Max)/Size,
	fetch_for_avg(J0, Border, J, LSums, MySums, RSums),
	to_disj(MySums, Disj),
	generate_avg(I, Size, J, Max, RSums, OVs, Evs, F, F0).

fetch_for_avg(J, Border, J, RSums, [], RSums) :-
	J > Border, !.
fetch_for_avg(J0, Border, J, [S|LSums], [S|MySums], RSums) :-
	J1 is J0+1,
	fetch_for_avg(J1, Border, J, LSums, MySums, RSums).


to_disj([], 0).
to_disj([V], V).
to_disj([V,V1|Vs], Out) :-
	to_disj2([V1|Vs], V, Out).

to_disj2([V], V0, V0+V).
to_disj2([V,V1|Vs], V0, Out) :-
	to_disj2([V1|Vs], V0+V, Out).


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
%writeln(t:Size:Parms0:Parms:ParmVars),
	rb_insert(Ps, DistId, theta(Parms, ParmVars), PsF).

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
copy(N, D.Ds, ND.NDs, New, El.Parms0, NEl.Parms, V.ParmVars) :-
	N1 is N-1,
	(El == 0.0 -> 
	    NEl = 0,
	    ND = D,
	    V = NEl
	;El == 1.0 -> 
	    NEl = 1,
	    ND = 0.0,
	    V = NEl
	;El == 0 -> 
	    NEl = 0,
	    ND = D,
	    V = NEl
	;El =:= 1 -> 
	    NEl = 1,
	    ND = 0.0,
	    V = NEl
	;
	    NEl is El/D,
	    ND is D-El,
	    V = NEl
	),
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
apply_first_parent(Parents.PVars, Conj+Disj, Theta.TheseParents) :-
	parents_to_conj(Parents,Theta,Conj),
	apply_first_parent(PVars, Disj, TheseParents).

apply_middle_parent([Parents], Other, Conj, [ThetaPar]) :- !,
	skim_for_theta(Other, Theta, _, ThetaPar),
	parents_to_conj(Parents,Theta,Conj).
apply_middle_parent(Parents.PVars, Other, Conj+Disj, ThetaPar.TheseParents) :-
	skim_for_theta(Other, Theta, Remaining, ThetaPar),
	parents_to_conj(Parents,(Theta),Conj),
	apply_middle_parent(PVars, Remaining, Disj, TheseParents).

apply_last_parent([Parents], Other, Conj) :- !,
	parents_to_conj(Parents,(Theta),Conj),
	skim_for_theta(Other, Theta, _, _).
apply_last_parent(Parents.PVars, Other, Conj+Disj) :-
	parents_to_conj(Parents,(Theta),Conj),
	skim_for_theta(Other, Theta, Remaining, _),
	apply_last_parent(PVars, Remaining, Disj).

%
%
% simplify stuff, removing process that is cancelled by 0s
%
parents_to_conj([], Theta, Theta) :- !.
parents_to_conj(Ps, Theta, Theta*Conj) :-
	parents_to_conj2(Ps, Conj).

parents_to_conj2([P],P) :- !.
parents_to_conj2(P.Ps,P*Conj) :-
	parents_to_conj2(Ps,Conj).

%
% first case we haven't reached the end of the list so we need
% to create a new parameter variable
%
skim_for_theta([[P|Other]|V], not(P)*New, [Other|_], New) :- var(V), !.
%
% last theta, it is just negation of the other ones
%
skim_for_theta([[P|Other]], not(P), [Other], _) :- !.
%
% recursive case, build-up
%
skim_for_theta([[P|Other]|More], not(P)*Ps, [Other|Left], New ) :-
	skim_for_theta(More, Ps, Left, New ).

get_evidence(V, Tree, Ev, F0, F, Leaves, Finals) :-
	clpbn:get_atts(V, [evidence(Pos)]), !,
	zero_pos(0, Pos, Ev),
	insert_output(Leaves, V, Finals, Tree, Outs, SendOut),
	get_outs(F0, F, SendOut, Outs).
% hidden deterministic node, can be removed.
get_evidence(V, _Tree, Ev, F0, [], _Leaves, _Finals) :-
	clpbn:get_atts(V, [key(K)]),
	functor(K, Name, 2),
	( Name = 'AVG' ; Name = 'MAX' ; Name = 'MIN' ),
	!,
	one_list(Ev),
	eval_outs(F0).	
%% no evidence !!!
get_evidence(V, Tree, _Values, F0, F1, Leaves, Finals) :-
	insert_output(Leaves, V, Finals, Tree, Outs, SendOut),
	get_outs(F0, F1, SendOut, Outs).

zero_pos(_, _Pos, []).
zero_pos(Pos, Pos, 1.Values) :- !, 
	I is Pos+1,
	zero_pos(I, Pos, Values).
zero_pos(I0, Pos, 0.Values) :- 
	I is I0+1,
	zero_pos(I, Pos, Values).

one_list([]).
one_list(1.Ev) :-
	one_list(Ev).

%
% insert a node with the disj of all alternatives, this is only done if node ends up to be in the output 
%
insert_output([], _V, [], _Out, _Outs, []).
insert_output(V._Leaves, V0, [Top|_], Top, Outs, [Top = Outs]) :- V == V0, !.
insert_output(_.Leaves, V, _.Finals, Top, Outs, SendOut) :-
	insert_output(Leaves, V, Finals, Top, Outs, SendOut).


get_outs([V=F], [V=NF|End], End,  V) :- !,
%	writeln(f0:F),
	simplify_exp(F,NF).
get_outs((V=F).Outs, (V=NF).NOuts, End, (F0 + V)) :-
%	writeln(f0:F),
	simplify_exp(F,NF),
	get_outs(Outs, NOuts, End, F0).

eval_outs([]).
eval_outs((V=F).Outs) :-
	simplify_exp(F,NF),
	V = NF,
	eval_outs(Outs).

%simplify_exp(V,V) :- !.
simplify_exp(V,V) :- var(V), !.
simplify_exp(S1+S2,NS) :- !,
	simplify_exp(S1, SS1),
	simplify_exp(S2, SS2),
	simplify_sum(SS1, SS2, NS).
simplify_exp(S1*S2,NS) :- !,
	simplify_exp(S1, SS1),
	simplify_exp(S2, SS2),
	simplify_prod(SS1, SS2, NS).
simplify_exp(not(S),NS) :- !,
	simplify_exp(S, SS),
	simplify_not(SS, NS).
simplify_exp(S,S).

simplify_sum(V1, V2, O) :- 
	( var(V1) ->
	    ( var(V2) ->
		( V1 == V2 -> O = V1 ; O = V1+V2 ) ; /* var(V1) , var(V2) */
		( V2 == 0 -> O = V1 ; V2 == 1 -> O = 1 ; O = V1+V2 ) /* var(V1) , nonvar(V2) */
	    ) ;
	    ( var(V2) ->
		( V1 == 0 -> O = V2 ; V1 == 1 -> O = 1 ; O = V1+V2 ) ; /* nonvar(V1) , var(V2) */
		( V2 == 0 -> O = V1 ; V2 == 1 -> O = 1 ; V1 == 0 -> O = V2 ; V1 == 1 -> O = 1; O = V1+V2 ) /* nonvar(V1) , nonvar(V2) */
	    )
	).

simplify_prod(V1, V2, O) :- 
	( var(V1) ->
	    ( var(V2) ->
		( V1 == V2 -> O = V1 ; O = V1*V2 ) ; /* var(V1) , var(V2) */
		( V2 == 0 -> O = 0 ; V2 == 1 -> O = V1 ; O = V1*V2 ) /* var(V1) , nonvar(V2) */
	    ) ;
	    ( var(V2) ->
		( V1 == 0 -> O = 0 ; V1 == 1 -> O = V2 ; O = V1*V2 ) ; /* nonvar(V1) , var(V2) */
		( V2 == 0 -> O = 0 ; V2 == 1 -> O = V1 ; V1 == 0 -> O = 0 ; V1 == 1 -> O = V2; V1 == V2 -> O = V1 ; O = V1*V2 ) /* nonvar(V1) , nonvar(V2) */
	    )
	).


simplify_not(V, not(V)) :- var(V), !.
simplify_not(0, 1) :- !.
simplify_not(1, 0) :- !.
simplify_not(SS, not(SS)).


run_bdd_solver([[V]], LPs, bdd(Term, _Leaves, Nodes)) :-
	build_out_node(Nodes, Node),
	findall(Prob, get_prob(Term, Node, V, Prob),TermProbs),
	sumlist(TermProbs, Sum),
writeln(TermProbs:Sum),
	normalise(TermProbs, Sum, LPs).

build_out_node([_Top], []).
build_out_node([T,T1|Tops], [Top = T*Top]) :-
	build_out_node2(T1.Tops, Top).

build_out_node2([Top], Top).
build_out_node2([T,T1|Tops], T*Top) :-
	build_out_node2(T1.Tops, Top).


get_prob(Term, Node, V, SP) :-
	bind_all(Term, Node, Bindings, V, AllParms, AllParmValues),
%	reverse(AllParms, RAllParms),
	term_variables(AllParms, NVs),
	build_bdd(Bindings, NVs, AllParms, AllParmValues, Bdd),
	bdd_to_probability_sum_product(Bdd, SP),
	bdd_close(Bdd).

build_bdd(Bindings, NVs, VTheta, Theta, Bdd) :-
	bdd_from_list(Bindings, NVs, Bdd),
	bdd_size(Bdd, Len),
%	number_codes(Len,Codes),
%	atom_codes(Name,Codes),
%	bdd_print(Bdd, Name),
	writeln(length=Len),
	VTheta = Theta.

bind_all([], End, End, _V, [], []).
bind_all(info(V, _Tree, Ev, _Values, Formula, ParmVars, Parms).Term, End, BindsF, V0, ParmVars.AllParms, Parms.AllTheta) :-
	V0 == V, !,
	set_to_one_zeros(Ev),
	bind_formula(Formula, BindsF, BindsI),
	bind_all(Term, End, BindsI, V0, AllParms, AllTheta).
bind_all(info(_V, _Tree, Ev, _Values, Formula, ParmVars, Parms).Term, End, BindsF, V0, ParmVars.AllParms, Parms.AllTheta) :-
	set_to_ones(Ev),!,
	bind_formula(Formula, BindsF, BindsI),
	bind_all(Term, End, BindsI, V0, AllParms, AllTheta).
% evidence: no need to add any stuff.
bind_all(info(_V, _Tree, _Ev, _Values, Formula, ParmVars, Parms).Term, End, BindsF, V0, ParmVars.AllParms, Parms.AllTheta) :-
	bind_formula(Formula, BindsF, BindsI),
	bind_all(Term, End, BindsI, V0, AllParms, AllTheta).

bind_formula([], L, L).
bind_formula(B.Formula, B.BsF, Bs0) :-
	bind_formula(Formula, BsF, Bs0).

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

