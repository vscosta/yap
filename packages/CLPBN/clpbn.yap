
:- module(clpbn,
		[{}/1,
		 clpbn_flag/2,
		 set_clpbn_flag/2,
		 set_solver/1,
		 set_em_solver/1,
		 clpbn_flag/3,
		 clpbn_key/2,
		 clpbn_init_graph/1,
		 clpbn_init_solver/4,
		 clpbn_run_solver/3,
		 pfl_init_solver/5,
		 pfl_run_solver/3,
		 pfl_end_solver/1,
		 probability/2,
		 conditional_probability/3,
		 use_parfactors/1,
		 op(500, xfy, with)
		]).

:- use_module(library(atts)).


:- use_module(library(bhash)).

:- use_module(library(lists)).

:- use_module(library(terms)).

:- use_module(library(maplist)).

:- yap_flag(arithmetic_exceptions,false).

:- attribute key/1, dist/2, evidence/1.

:- use_module(clpbn/ve,
		[ve/3,
		 check_if_ve_done/1,
		 init_ve_solver/4,
		 run_ve_solver/3,
		 init_ve_ground_solver/5,
		 run_ve_ground_solver/3,
		 call_ve_ground_solver/6
		]).

:- use_module('clpbn/jt',
		[jt/3,
		 init_jt_solver/4,
		 run_jt_solver/3
		]).

:- use_module('clpbn/bdd',
		[bdd/3,
		 init_bdd_solver/4,
		 run_bdd_solver/3,
		 init_bdd_ground_solver/5,
		 run_bdd_ground_solver/3,
		 call_bdd_ground_solver/6
		]).

:- use_module('clpbn/gibbs',
		[gibbs/3,
		 check_if_gibbs_done/1,
		 init_gibbs_solver/4,
		 run_gibbs_solver/3
		]).

:- use_module('clpbn/pgrammar',
		[pcg_init_graph/0,
		 init_pcg_solver/4,
		 run_pcg_solver/3
		]).

:- use_module('clpbn/horus',
		[set_horus_flag/2]).

:- use_module('clpbn/horus_ground',
		[call_horus_ground_solver/6,
		 check_if_horus_ground_solver_done/1,
		 init_horus_ground_solver/5,
		 run_horus_ground_solver/3,
		 end_horus_ground_solver/1
		]).

:- use_module('clpbn/horus_lifted',
		[call_horus_lifted_solver/3,
		 check_if_horus_lifted_solver_done/1,
		 init_horus_lifted_solver/4,
		 run_horus_lifted_solver/3,
		 end_horus_lifted_solver/1
		]).

%% :- use_module('clpbn/bnt',
%%		[do_bnt/3,
%%		 check_if_bnt_done/1
%%		]).

:- use_module('clpbn/dists',
		[dist/4,
		 get_dist/4,
		 get_evidence_position/3,
		 get_evidence_from_position/3,
		 additive_dists/6
		]).

:- use_module('clpbn/evidence',
		[store_evidence/1,
		 add_stored_evidence/2,
		 incorporate_evidence/2,
		 check_stored_evidence/2,
		 put_evidence/2
		]).

:- use_module('clpbn/ground_factors',
		[generate_network/5]).

:- use_module('clpbn/utils',
		[sort_vars_by_key/3]).

:- use_module('clpbn/graphs',
		[clpbn2graph/1]).

:- use_module('clpbn/graphviz',
		[clpbn2gviz/4]).

%
% avoid the overhead of using goal_expansion/2.
%
:- multifile user:term_expansion/2.

:- dynamic user:term_expansion/2.

:- dynamic
	solver/1,
	em_solver/1,
	suppress_attribute_display/1,
	parameter_softening/1,
	use_parfactors/1,
	output/1,
	use/1.

:- meta_predicate probability(:,-), conditional_probability(:,:,-).


solver(hve).
em_solver(hve).
suppress_attribute_display(false).
parameter_softening(m_estimate(10)).
use_parfactors(off).
output(no).
%output(xbif(user_error)).
%output(gviz(user_error)).

ground_solver(ve).
ground_solver(hve).
ground_solver(jt).
ground_solver(bdd).
ground_solver(bp).
ground_solver(cbp).
ground_solver(gibbs).

lifted_solver(lve).
lifted_solver(lkc).
lifted_solver(lbp).


clpbn_flag(Flag, Option) :-
	clpbn_flag(Flag, Option, Option).

set_clpbn_flag(Flag,Option) :-
	clpbn_flag(Flag, _, Option).

clpbn_flag(solver,Before,After) :- !,
	retract(solver(Before)),
	assert(solver(After)).

clpbn_flag(em_solver,Before,After) :- !,
	retract(em_solver(Before)),
	assert(em_solver(After)).

clpbn_flag(bnt_solver,Before,After) :- !,
	retract(bnt:bnt_solver(Before)),
	assert(bnt:bnt_solver(After)).

clpbn_flag(bnt_path,Before,After) :- !,
	retract(bnt:bnt_path(Before)),
	assert(bnt:bnt_path(After)).

clpbn_flag(bnt_model,Before,After) :- !,
	retract(bnt:bnt_model(Before)),
	assert(bnt:bnt_model(After)).

clpbn_flag(suppress_attribute_display,Before,After) :- !,
	retract(suppress_attribute_display(Before)),
	assert(suppress_attribute_display(After)).

clpbn_flag(parameter_softening,Before,After) :- !,
	retract(parameter_softening(Before)),
	assert(parameter_softening(After)).

clpbn_flag(use_parfactors,Before,After) :- !,
	retract(use_parfactors(Before)),
	assert(use_parfactors(After)).

clpbn_flag(output,Before,After) :- !,
	retract(output(Before)),
	assert(output(After)).

clpbn_flag(HorusOption, _Before, After) :-
	set_horus_flag(HorusOption, After).

set_solver(Solver) :-
	set_clpbn_flag(solver,Solver).

set_em_solver(Solver) :-
	set_clpbn_flag(em_solver,Solver).

{_} :-
	solver(none), !.
{ Var = Key with Dist } :-
	put_atts(El,[key(Key),dist(DistInfo,Parents)]),
	dist(Dist, DistInfo, Key, Parents),
	add_evidence(Var,Key,DistInfo,El)
%	,writeln({Var = Key with Dist})
	.

%
% make sure a query variable is reachable by the garbage collector.
%
% we use a mutable variable to avoid unnecessary trailing.
%
store_var(El) :-
	nb_current(clpbn_qvars, Mutable),
	nonvar(Mutable), !,
	get_mutable(Tail, Mutable),
	update_mutable(El.Tail, Mutable).
store_var(El) :-
	init_clpbn_vars(El).

init_clpbn_vars(El) :-
	create_mutable(El, Mutable),
	b_setval(clpbn_qvars, Mutable).

check_constraint(Constraint, _, _, Constraint) :-
	var(Constraint), !.
check_constraint((A->D), _, _, (A->D)) :-
	var(A), !.
check_constraint((([A|B].L)->D), Vars, NVars, (([A|B].NL)->D)) :- !,
	check_cpt_input_vars(L, Vars, NVars, NL).
check_constraint(Dist, _, _, Dist).

check_cpt_input_vars([], _, _, []).
check_cpt_input_vars([V|L], Vars, NVars, [NV|NL]) :-
	replace_var(Vars, V, NVars, NV),
	check_cpt_input_vars(L, Vars, NVars, NL).

replace_var([], V, [], V).
replace_var([V|_], V0, [NV|_], NV) :- V == V0, !.
replace_var([_|Vars], V, [_|NVars], NV) :-
	replace_var(Vars, V, NVars, NV).

add_evidence(V,Key,Distinfo,NV) :-
	nonvar(V), !,
	get_evidence_position(V, Distinfo, Pos),
	check_stored_evidence(Key, Pos),
	store_var(NV),
	clpbn:put_atts(NV,evidence(Pos)).
add_evidence(V,K,_,V) :-
	add_stored_evidence(K,V),
	store_var(V).

clpbn_marginalise(V, Dist) :-
	attributes:all_attvars(AVars),
	project_attributes([V], AVars),
	clpbn_display:get_atts(V, posterior(_,_,Dist,_)).

%
% called by top-level
% or by call_residue/2
%
project_attributes(GVars0, _AVars0) :-
	use_parfactors(on),
	clpbn_flag(solver, Solver),
	ground_solver(Solver),
	generate_network(GVars0, GKeys, Keys, Factors, Evidence),
	b_setval(clpbn_query_variables, f(GVars0,Evidence)),
	simplify_query(GVars0, GVars),
	(
	  GKeys = []
	->
	  GVars0 = [V|_],
	  clpbn_display:put_atts(V, [posterior([],[],[],[])])
	;
	  call_ground_solver(Solver, GVars, GKeys, Keys, Factors, Evidence)
	).
project_attributes(GVars, AVars) :-
	suppress_attribute_display(false),
	AVars = [_|_],
	solver(Solver),
	( GVars = [_|_] ; Solver = graphs), !,
	% we don't pass query variables in this way
	b_setval(clpbn_query_variables, none),
	clpbn_vars(AVars, DiffVars, AllVars),
	get_clpbn_vars(GVars,CLPBNGVars0),
	simplify_query_vars(CLPBNGVars0, CLPBNGVars),
	(output(xbif(XBifStream)) -> clpbn2xbif(XBifStream,ve,AllVars) ; true),
	(output(gviz(XBifStream)) -> clpbn2gviz(XBifStream,sort,AllVars,GVars) ; true),
	(
	  Solver = graphs
	->
	  write_out(Solver, [[]], AllVars, DiffVars)
	;
	  write_out(Solver, [CLPBNGVars], AllVars, DiffVars)
	).
project_attributes(_, _).

%
% check for query variables with evidence
%
simplify_query([V|GVars0], GVars) :-
	get_atts(V, [evidence(_)]), !,
	simplify_query(GVars0, GVars).
simplify_query([V|GVars0], GVars) :-
	get_atts(V, [key(K)]),
	pfl:evidence(K, _), !,
	simplify_query(GVars0, GVars).
simplify_query([V|GVars0], [V|GVars]) :-
	simplify_query(GVars0, GVars).
simplify_query([], []).

match([], _Keys).
match([V|GVars], Keys) :-
	clpbn:get_atts(V,[key(GKey)]),
	member(GKey, Keys), ground(GKey), !,
	match(GVars, Keys).
match([_V|GVars], Keys) :-
	match(GVars, Keys).

clpbn_vars(AVars, DiffVars, AllVars) :-
	sort_vars_by_key(AVars,SortedAVars,DiffVars),
	incorporate_evidence(SortedAVars, AllVars).

get_clpbn_vars([V|GVars],[V|CLPBNGVars]) :-
	get_atts(V, [key(_)]), !,
	get_clpbn_vars(GVars,CLPBNGVars).
get_clpbn_vars([_|GVars],CLPBNGVars) :-
	get_clpbn_vars(GVars,CLPBNGVars).
get_clpbn_vars([],[]).

simplify_query_vars(LVs0, LVs) :-
	sort(LVs0,LVs1),
	get_rid_of_ev_vars(LVs1,LVs).

%
% some variables might already have evidence in the data-base.
%
get_rid_of_ev_vars([],[]).
get_rid_of_ev_vars([V|LVs0],LVs) :-
	clpbn:get_atts(V, [dist(Id,_),evidence(Pos)]), !,
	get_evidence_from_position(Ev, Id, Pos),
	clpbn_display:put_atts(V, [posterior([],Ev,[],[])]), !,
	get_rid_of_ev_vars(LVs0,LVs).
get_rid_of_ev_vars([V|LVs0],[V|LVs]) :-
	get_rid_of_ev_vars(LVs0,LVs).


% Call a solver with keys, not actual variables
call_ground_solver(ve, GVars, GoalKeys, Keys, Factors, Evidence) :- !,
	call_ve_ground_solver(GVars, GoalKeys, Keys, Factors, Evidence, _Answ).

call_ground_solver(hve, GVars, GoalKeys, Keys, Factors, Evidence) :- !,
	clpbn_horus:set_horus_flag(ground_solver, hve),
	call_horus_ground_solver(GVars, GoalKeys, Keys, Factors, Evidence, _Answ).

call_ground_solver(bdd, GVars, GoalKeys, Keys, Factors, Evidence) :- !,
	call_bdd_ground_solver(GVars, GoalKeys, Keys, Factors, Evidence, _Answ).

call_ground_solver(bp, GVars, GoalKeys, Keys, Factors, Evidence) :- !,
	clpbn_horus:set_horus_flag(ground_solver, bp),
	call_horus_ground_solver(GVars, GoalKeys, Keys, Factors, Evidence, _Answ).

call_ground_solver(cbp, GVars, GoalKeys, Keys, Factors, Evidence) :- !,
	clpbn_horus:set_horus_flag(ground_solver, cbp),
	call_horus_ground_solver(GVars, GoalKeys, Keys, Factors, Evidence, _Answ).

call_ground_solver(Solver, GVars, _GoalKeys, Keys, Factors, Evidence) :-
	% fall back to traditional solver
	b_hash_new(Hash0),
	foldl(gvar_in_hash, GVars, Hash0, HashI),
	foldl(key_to_var, Keys, AllVars, HashI, Hash1),
	foldl(evidence_to_v, Evidence, _EVars, Hash1, Hash),
	%writeln(Keys:AllVars),
	maplist(factor_to_dist(Hash), Factors),
	% evidence
	retract(use_parfactors(on)),
	write_out(Solver, [GVars], AllVars, _),
	assert(use_parfactors(on)).


% do nothing if we don't have query variables to compute.
write_out(_, GVars, AVars, _) :- 
	maplist(bound_varl(AVars), GVars), !.

write_out(_, [], _, _) :- !.

write_out(graphs, _, AVars, _) :- !,
	clpbn2graph(AVars).

write_out(ve, GVars, AVars, DiffVars) :- !,
	ve(GVars, AVars, DiffVars).

write_out(jt, GVars, AVars, DiffVars) :- !,
	jt(GVars, AVars, DiffVars).

write_out(bdd, GVars, AVars, DiffVars) :- !,
	bdd(GVars, AVars, DiffVars).

write_out(gibbs, GVars, AVars, DiffVars) :- !,
	gibbs(GVars, AVars, DiffVars).

write_out(lve, GVars, AVars, DiffVars) :- !,
	clpbn_horus:set_horus_flag(lifted_solver, lve),
	call_horus_lifted_solver(GVars, AVars, DiffVars).

write_out(lkc, GVars, AVars, DiffVars) :- !,
	clpbn_horus:set_horus_flag(lifted_solver, lkc),
	call_horus_lifted_solver(GVars, AVars, DiffVars).

write_out(lbp, GVars, AVars, DiffVars) :- !,
	clpbn_horus:set_horus_flag(lifted_solver, lbp),
	call_horus_lifted_solver(GVars, AVars, DiffVars).

write_out(bnt, GVars, AVars, DiffVars) :- !,
	do_bnt(GVars, AVars, DiffVars).

write_out(Solver, _, _, _) :-
	format("Error: solver '~w' is unknown.", [Solver]),
	fail.

bound_varl(AVars, L) :-
	maplist(bound_var(AVars), L).

bound_var(_AVars,V) :-
	var(V), !,
	get_atts(V, [key(K)]),
	( pfl:evidence(K, Ev) -> true ; get_atts(V, [key(K), evidence(Ev)]) ),
	pfl:skolem(K,D),
	once(nth0(Ev,D,V)).
bound_var(_AVars, _V).



%
% convert a PFL network (without constraints)
% into CLP(BN) for evaluation
%
gvar_in_hash(V, Hash0, Hash) :-
	get_atts(V, [key(K)]),
	b_hash_insert(Hash0, K, V, Hash).

key_to_var(K, V, Hash0, Hash0) :-
	b_hash_lookup(K, V, Hash0), !.
key_to_var(K, V,Hash0, Hash) :-
	put_atts(V, [key(K)]),
	b_hash_insert(Hash0, K, V, Hash).

evidence_to_v(K=E, V, Hash0, Hash0) :-
	b_hash_lookup(K, V, Hash0), !,
	clpbn:put_atts(V,[evidence(E)]).
evidence_to_v(K=E, V, Hash0, Hash) :-
	b_hash_insert(Hash0, K, V, Hash),
	clpbn:put_atts(V,[evidence(E)]).

factor_to_dist(Hash, f(bayes, Id, Ks)) :-
	maplist(key_to_var(Hash), Ks, [V|Parents]),
	Ks =[Key|_],
	pfl:skolem(Key, Domain),
	pfl:get_pfl_parameters(Id, Ks, CPT),
	dist(p(Domain,CPT,Parents), DistInfo, Key, Parents),
	put_atts(V,[dist(DistInfo,Parents)]).

key_to_var(Hash, K, V) :-
	b_hash_lookup(K, V, Hash).

get_bnode(Var, Goal) :-
	get_atts(Var, [key(Key),dist(Dist,Parents)]),
	get_dist(Dist,_,Domain,CPT),
	(Parents = [] -> X = tab(Domain,CPT) ; X = tab(Domain,CPT,Parents)),
	dist_goal(X, Key, Goal0),
	include_evidence(Var, Goal0, Key, Goal).

include_evidence(Var, Goal0, Key, ((Key:-Ev),Goal0)) :-
	get_atts(Var, [evidence(Ev)]), !.
include_evidence(_, Goal0, _, Goal0).

dist_goal(Dist, Key, (Key=NDist)) :-
	term_variables(Dist, DVars),
	process_vars(DVars, DKeys),
	my_copy_term(Dist,DVars, NDist,DKeys).

my_copy_term(V, DVars, Key, DKeys) :-
	find_var(DVars, V, Key, DKeys).
my_copy_term(A, _, A, _) :- atomic(A), !.
my_copy_term(T, Vs, NT, Ks) :-
	T =.. [Na|As],
	my_copy_terms(As, Vs, NAs, Ks),
	NT =.. [Na|NAs].

my_copy_terms([], _, [], _).
my_copy_terms([A|As], Vs, [NA|NAs], Ks) :-
	my_copy_term(A, Vs, NA, Ks),
	my_copy_terms(As, Vs, NAs, Ks).

find_var([V1|_], V, Key, [Key|_]) :- V1 == V, !.
find_var([_|DVars], V, Key, [_|DKeys]) :-
	find_var(DVars, V, Key, DKeys).

process_vars([], []).
process_vars([V|Vs], [K|Ks]) :-
	process_var(V, K),
	process_vars(Vs, Ks).

process_var(V, K) :- get_atts(V, [key(K)]), !.
% oops: this variable has no attributes.
process_var(V, _) :- throw(error(instantiation_error,clpbn(attribute_goal(V)))).

%
% unify a CLPBN variable with something.
%
verify_attributes(Var, T, Goal) :-
	get_atts(Var, [key(Key),dist(Dist,Parents)]), !,
	/* oops, someone trying to bind a clpbn constrained variable */
	bind_clpbn(T, Var, Key, Dist, Parents, Goal).
verify_attributes(_, _, []).


bind_clpbn(T, Var, _, _, _, do_not_bind_variable([put_evidence(T,Var)])) :-
	nonvar(T),
	!.
bind_clpbn(T, Var, Key, Dist, Parents, []) :- var(T),
	get_atts(T, [key(Key1),dist(Dist1,Parents1)]),
	(
	  bind_clpbns(Key, Dist, Parents, Key1, Dist1, Parents1)
	->
	  (
	    get_atts(T, [evidence(Ev1)]) ->
	      bind_evidence_from_extra_var(Ev1,Var)
	    ;
	      get_atts(Var, [evidence(Ev)]) ->
	        bind_evidence_from_extra_var(Ev,T)
	      ;
	        true
	  )
	;
	  fail
	).
bind_clpbn(_, Var, _, _, _, _, []) :-
	use(ve),
	check_if_ve_done(Var), !.
bind_clpbn(_, Var, _, _, _, _, []) :-
	use(hve),
	check_if_horus_ground_solver_done(Var), !.
bind_clpbn(_, Var, _, _, _, _, []) :-
	use(jt),
	check_if_ve_done(Var), !.
bind_clpbn(_, Var, _, _, _, _, []) :-
	use(bdd),
	check_if_bdd_done(Var), !.
bind_clpbn(_, Var, _, _, _, _, []) :-
	use(bp),
	check_if_horus_ground_solver_done(Var), !.
bind_clpbn(_, Var, _, _, _, _, []) :-
	use(cbp),
	check_if_horus_ground_solver_done(Var), !.
bind_clpbn(_, Var, _, _, _, _, []) :-
	use(bnt),
	check_if_bnt_done(Var), !.
bind_clpbn(T, Var, Key0, _, _, _, []) :-
	get_atts(Var, [key(Key)]), !,
	(
	  Key = Key0 -> true
	;
	  % let us not loose whatever we had.
	  put_evidence(T,Var)
	).

fresh_attvar(Var, NVar) :-
	get_atts(Var, LAtts),
	put_atts(NVar, LAtts).

% I will now allow two CLPBN variables to be bound together.
% bind_clpbns(Key, Dist, Parents, Key, Dist, Parents).
bind_clpbns(Key, Dist, Parents, Key1, Dist1, Parents1) :-
	Key == Key1, !,
	get_dist(Dist,_Type,_Domain,_Table),
	get_dist(Dist1,_Type1,_Domain1,_Table1),
	Dist = Dist1,
	Parents = Parents1.
bind_clpbns(Key, _, _, _, Key1, _, _, _) :-
	Key\=Key1, !, fail.
bind_clpbns(_, _, _, _, _, _, _, _) :-
	format(user_error, 'unification of two bayesian vars not supported~n', []).

same_parents([],[]).
same_parents([P|Parents],[P1|Parents1]) :-
	same_node(P,P1),
	same_parents(Parents,Parents1).

same_node(P,P1) :- P == P1, !.
same_node(P,P1) :-
	get_atts( P,[key(K)]),
	get_atts(P1,[key(K)]),
	P = P1.


bind_evidence_from_extra_var(Ev1,Var) :-
	get_atts(Var, [evidence(Ev0)]), !,
	Ev0 = Ev1.
bind_evidence_from_extra_var(Ev1,Var) :-
	put_atts(Var, [evidence(Ev1)]).

user:term_expansion((A :- {}), ( :- true )) :- !, % evidence
	prolog_load_context(module, M),
	store_evidence(M:A).

clpbn_key(Var,Key) :-
	get_atts(Var, [key(Key)]).


%
% only useful for probabilistic context free grammars
%
clpbn_init_graph(pcg) :- !,
	pcg_init_graph.
clpbn_init_graph(_).


%
% This is a routine to start a solver, called by the learning procedures (ie, em).
% LVs is a list of lists of variables one is interested in eventually marginalising out
% Vs0 gives the original graph
% AllDiffs gives variables that are not fully constrainted, ie, we don't fully know
% the key. In this case, we assume different instances will be bound to different
% values at the end of the day.
%
clpbn_init_solver(LVs, Vs0, VarsWithUnboundKeys, State) :-
	solver(Solver),
	clpbn_init_solver(Solver, LVs, Vs0, VarsWithUnboundKeys, State).

clpbn_init_solver(ve, LVs, Vs0, VarsWithUnboundKeys, State) :-
	init_ve_solver(LVs, Vs0, VarsWithUnboundKeys, State).

clpbn_init_solver(jt, LVs, Vs0, VarsWithUnboundKeys, State) :-
	init_jt_solver(LVs, Vs0, VarsWithUnboundKeys, State).

clpbn_init_solver(bdd, LVs, Vs0, VarsWithUnboundKeys, State) :-
	init_bdd_solver(LVs, Vs0, VarsWithUnboundKeys, State).

clpbn_init_solver(gibbs, LVs, Vs0, VarsWithUnboundKeys, State) :-
	init_gibbs_solver(LVs, Vs0, VarsWithUnboundKeys, State).

clpbn_init_solver(pcg, LVs, Vs0, VarsWithUnboundKeys, State) :-
	init_pcg_solver(LVs, Vs0, VarsWithUnboundKeys, State).

%
% LVs is the list of lists of variables to marginalise
% Vs is the full graph
% Ps are the probabilities on LVs.
%
clpbn_run_solver(LVs, LPs, State) :-
	solver(Solver),
	clpbn_run_solver(Solver, LVs, LPs, State).

clpbn_run_solver(ve, LVs, LPs, State) :-
	run_ve_solver(LVs, LPs, State).

clpbn_run_solver(jt, LVs, LPs, State) :-
	run_jt_solver(LVs, LPs, State).

clpbn_run_solver(bdd, LVs, LPs, State) :-
	run_bdd_solver(LVs, LPs, State).

clpbn_run_solver(gibbs, LVs, LPs, State) :-
	run_gibbs_solver(LVs, LPs, State).

clpbn_run_solver(pcg, LVs, LPs, State) :-
	run_pcg_solver(LVs, LPs, State).

%
% This is a routine to start a solver, called by the learning procedures (ie, em).
%
pfl_init_solver(QueryKeys, AllKeys, Factors, Evidence, State) :-
	em_solver(Solver),
	(lifted_solver(Solver) ->
	  format("Error: you cannot use a lifted solver for learning.", [Solver]), fail
	;
	  true
	),
	(ground_solver(Solver) ->
	  true
	;
	  format("Error: solver '~w' is unknown.", [Solver]), fail
	),
	pfl_init_solver(Solver, QueryKeys, AllKeys, Factors, Evidence, State).

pfl_init_solver(ve, QueryKeys, AllKeys, Factors, Evidence, State) :- !,
	init_ve_ground_solver(QueryKeys, AllKeys, Factors, Evidence, State).

pfl_init_solver(hve, QueryKeys, AllKeys, Factors, Evidence, State) :- !,
	clpbn_horus:set_horus_flag(ground_solver, hve),
	init_horus_ground_solver(QueryKeys, AllKeys, Factors, Evidence, State).

pfl_init_solver(bdd, QueryKeys, AllKeys, Factors, Evidence, State) :- !,
	init_bdd_ground_solver(QueryKeys, AllKeys, Factors, Evidence, State).

pfl_init_solver(bp, QueryKeys, AllKeys, Factors, Evidence, State) :- !,
	clpbn_horus:set_horus_flag(ground_solver, bp),
	init_horus_ground_solver(QueryKeys, AllKeys, Factors, Evidence, State).

pfl_init_solver(cbp, QueryKeys, AllKeys, Factors, Evidence, State) :- !,
	clpbn_horus:set_horus_flag(ground_solver, cbp),
	init_horus_ground_solver(QueryKeys, AllKeys, Factors, Evidence, State).

pfl_init_solver(Solver, _, _, _, _, _) :-
	format("Error: solver '~w' can't be used for learning.", [Solver]),
	fail.


pfl_run_solver(LVs, LPs, State) :-
	em_solver(Solver),
	pfl_run_solver(Solver, LVs, LPs, State).

pfl_run_solver(ve, LVs, LPs, State) :- !,
	run_ve_ground_solver(LVs, LPs, State).

pfl_run_solver(hve, LVs, LPs, State) :- !,
	run_horus_ground_solver(LVs, LPs, State).

pfl_run_solver(bdd, LVs, LPs, State) :- !,
	run_bdd_ground_solver(LVs, LPs, State).

pfl_run_solver(bp, LVs, LPs, State) :- !,
	run_horus_ground_solver(LVs, LPs, State).

pfl_run_solver(cbp, LVs, LPs, State) :- !,
	run_horus_ground_solver(LVs, LPs, State).

pfl_end_solver(State) :-
	(em_solver(hve) ; em_solver(bp) ; em_solver(cbp)),
	end_horus_ground_solver(State).
pfl_end_solver(_State).


add_keys(Key1+V1,_Key2,Key1+V1).


probability(Goal, Prob) :-
	findall(Prob, do_probability(Goal, [], Prob), [Prob]).

conditional_probability(Goal, ListOfGoals, Prob) :-
	\+ ground(Goal),
	throw(error(ground(Goal),conditional_probability(Goal, ListOfGoals, Prob))).
conditional_probability(Goal, ListOfGoals, Prob) :-
	\+ ground(ListOfGoals), !,
	throw(error(ground(ListOfGoals),conditional_probability(Goal, ListOfGoals, Prob))).
conditional_probability(Goal, ListOfGoals, Prob) :-
	findall(Prob, do_probability(Goal, ListOfGoals, Prob), [Prob]).

do_probability(Goal, ListOfGoals, Prob) :-
	evidence_to_var(Goal, C, NGoal, V),
	call_residue(run( ListOfGoals, NGoal), Vars), !,
	match_probability(Vars, NGoal, C, V, Prob).

run(ListOfGoals,Goal) :-
	do(ListOfGoals),
	call(Goal).

do(M:ListOfGoals) :-
	do(ListOfGoals, M).
do([]).

do([], _M).
do(G.ListOfGoals, M) :-
	M:G,
	do(ListOfGoals, M).

evidence_to_var(M:Goal, C, M:VItem, V) :- !,
	evidence_to_var(Goal, C, VItem, V).
evidence_to_var(Goal, C, VItem, V) :-
	Goal =.. [L|Args],
	variabilise_last(Args, C, NArgs, V),
	VItem =.. [L|NArgs].

variabilise_last([Arg], Arg, [V], V).
variabilise_last([Arg1,Arg2|Args], Arg, Arg1.NArgs, V) :-
	variabilise_last(Arg2.Args, Arg, NArgs, V).

match_probability(VPs, Goal, C, V, Prob) :-
	match_probabilities(VPs, Goal, C, V, Prob).

match_probabilities([p(V0=C)=Prob|_], _, C, V, Prob) :-
	V0 == V,
	!.
match_probabilities([_|Probs], G, C, V, Prob) :-
	match_probabilities(Probs, G, C, V, Prob).

goal_to_key(_:Goal, Skolem) :-
	goal_to_key(Goal, Skolem).
goal_to_key(Goal, Skolem) :-
	functor(Goal, Na, Ar),
	Ar1 is Ar-1,
	functor(Skolem, Na, Ar1).

:- use_parfactors(on) -> true ; assert(use_parfactors(off)).

