%
% generate explicit CPTs
%
:- module(clpbn_aggregates,
		[check_for_agg_vars/2,
		 cpt_average/6,
		 cpt_average/7,
		 cpt_max/6,
		 cpt_min/6,
		 avg_factors/5
		]).

:- use_module(library(clpbn),
		[{}/1,
		 op(500, xfy, with)]).

:- use_module(library(lists),
		[last/2,
		 sumlist/2,
		 sum_list/3,
		 max_list/2,
		 min_list/2,
		 nth0/3
		]).

:- use_module(library(matrix),
		[matrix_new/3,
		 matrix_to_list/2,
		 matrix_set/3
		]).

:- use_module(library(clpbn/dists),
		[add_dist/6,
		 get_dist_domain_size/2
		]).

:- use_module(library(clpbn/matrix_cpt_utils),
		[normalise_CPT_on_lines/3]).

:- use_module(library(pfl),
		[skolem/2,
		 add_ground_factor/5
		]).

:- use_module(library(bhash)).

:- use_module(library(maplist)).

check_for_agg_vars([], []).
check_for_agg_vars([V|Vs0], [V|Vs1]) :-
	clpbn:get_atts(V, [key(K), dist(Id,Parents)]), !,
	simplify_dist(Id, V, K, Parents, Vs0, Vs00),
	check_for_agg_vars(Vs00, Vs1).
check_for_agg_vars([V|Vs0], [V|Vs1]) :-
	check_for_agg_vars(Vs0, Vs1).

% transform aggregate distribution into tree
simplify_dist(avg(Domain), V, Key, Parents, Vs0, VsF) :- !,
	cpt_average([V|Parents], Key, Domain, NewDist, Vs0, VsF),
	NewDist = p(Dom, Tab, Ps),
	add_dist(Dom, tab, Tab, Ps, Key, Id),
	clpbn:put_atts(V, [dist(Id,Ps)]).
simplify_dist(_, _, _, _, Vs0, Vs0).

%
avg_factors(Key, Parents, _Smoothing, NewParents, Id) :-
	% we keep ev as a list
	skolem(Key, Domain),
	avg_table(Parents, Parents, Domain, Key, 0, 1.0, NewParents, [], _ExtraSkolems, Id).

% there are 4 cases:
% no evidence on top node
% evidence on top node compatible with values of parents
% evidence on top node *entailed* by values of parents (so there is no real connection)
% evidence incompatible with parents
query_evidence(Key, EvHash, MAT0, MAT, NewParents0, NewParents, Vs, IVs, NewVs) :-
	b_hash_lookup(Key, Ev, EvHash), !,
	normalise_CPT_on_lines(MAT0, MAT1, L1),
	check_consistency(L1, Ev, MAT0, MAT1, L1, MAT, NewParents0, NewParents, Vs, IVs, NewVs).
query_evidence(_, _, MAT, MAT, NewParents, NewParents, _, Vs, Vs).

hash_ev(K=V, Es0, Es) :-
	b_hash_insert(Es0, K, V, Es).

find_ev(Ev, Key, RemKeys, RemKeys, Ev0, EvF) :-
	b_hash_lookup(Key, V, Ev), !,
	EvF is Ev0+V.
find_ev(_Evs, Key, RemKeys, [Key|RemKeys], Ev, Ev).


% +Vars -> Keys without ev
% +all keys
% +domain to project to
% +ouput key
% +sum of evidence
% +softness
% +final CPT
% - New Parents
% + - list of new keys
%
avg_table(Vars, OVars, Domain, Key, TotEvidence, Softness, Vars, Vs, Vs, Id) :-
	length(Domain, SDomain),
	int_power(Vars, SDomain, 1, TabSize),
	TabSize =< 256,
	/* case gmp is not there !! */
	TabSize > 0, !,
	average_cpt(Vars, OVars, Domain, TotEvidence,  Softness, CPT),
	matrix_to_list(CPT, Mat),
	add_ground_factor(bayes, Domain, [Key|OVars], Mat, Id).
avg_table(Vars, OVars, Domain, Key, TotEvidence, Softness, [V1,V2], Vs, [V1,V2|NewVs], Id) :-
	length(Vars,L),
	LL1 is L//2,
	LL2 is L-LL1,
	list_split(LL1, Vars, L1, L2),
	Min = 0,
	length(Domain,Max1), Max is Max1-1,
	intermediate_table(LL1, sum(Min,Max), L1, V1, Key,  1.0, 0, I1, Vs, Vs1),
	intermediate_table(LL2, sum(Min,Max), L2, V2, Key, 1.0, I1, _, Vs1, NewVs),
	average_cpt([V1,V2], OVars, Domain, TotEvidence, Softness, CPT),
	matrix_to_list(CPT, Mat),
	add_ground_factor(bayes, Domain, [Key,V1,V2], Mat, Id).

intermediate_table(1,_,[V],V, _, _, I, I, Vs, Vs) :- !.
intermediate_table(2, Op, [V1,V2], V, Key, Softness, I0, If, Vs, Vs) :- !,
	If is I0+1,
	extra_key_factor(Op, 2, [V1,V2], V, Key, Softness, I0).
intermediate_table(N, Op, L, V, Key, Softness, I0, If, Vs, [V1,V2|NewVs]) :-
	LL1 is N//2,
	LL2 is N-LL1,
	list_split(LL1, L, L1, L2),
	I1 is I0+1,
	intermediate_table(LL1, Op, L1, V1, Key, Softness, I1, I2, Vs, Vs1),
	intermediate_table(LL2, Op, L2, V2, Key, Softness, I2, If, Vs1, NewVs),
	extra_key_factor(Op, N, [V1,V2], V, Key, Softness, I0).

extra_key_factor(sum(Min,Max), N, [V1,V2], V, Key, Softness, I) :-
	Lower is Min*N,
	Upper is Max*N,
	generate_list(Lower, Upper, Nbs),
	sum_cpt([V1,V2], Nbs, Softness, CPT),
	V = 'AVG'(I,Key),
	add_ground_factor(bayes, Nbs, [V,V1,V2], CPT, Id),
	assert(pfl:currently_defined(V)),
	assert(pfl:f(bayes, Id, [V,V1,V2])).

cpt_average(AllVars, Key, Els0, Tab, Vs, NewVs) :-
	cpt_average(AllVars, Key, Els0, 1.0, Tab, Vs, NewVs).

% support variables with evidence from domain. This should make everyone's life easier.
cpt_average([Ev|Vars], Key, Els0, Softness, p(Els0, TAB, NewParents), Vs, NewVs) :-
	find_evidence(Vars, 0, TotEvidence, RVars),
	build_avg_table(RVars, Vars, Els0, Key, TotEvidence, Softness, MAT0, NewParents0, Vs, IVs),
	include_qevidence(Ev, MAT0, MAT, NewParents0, NewParents, Vs, IVs, NewVs),
	matrix_to_list(MAT, TAB).

% find all fixed kids, this simplifies significantly the function.
find_evidence([], TotEvidence, TotEvidence, []).
find_evidence([V|Vars], TotEvidence0, TotEvidence, RVars) :-
	clpbn:get_atts(V,[evidence(Ev)]), !,
	TotEvidenceI is TotEvidence0+Ev,
	find_evidence(Vars, TotEvidenceI, TotEvidence, RVars).
find_evidence([V|Vars], TotEvidence0, TotEvidence, [V|RVars]) :-
	find_evidence(Vars, TotEvidence0, TotEvidence, RVars).

cpt_max([_|Vars], Key, Els0, CPT, Vs, NewVs) :-
	build_max_table(Vars, Els0, Els0, Key, 1.0, CPT, Vs, NewVs).

cpt_min([_|Vars], Key, Els0, CPT, Vs, NewVs) :-
	build_min_table(Vars, Els0, Els0, Key, 1.0, CPT, Vs, NewVs).

build_avg_table(Vars, OVars, Domain, _, TotEvidence, Softness, CPT, Vars, Vs, Vs) :-
	length(Domain, SDomain),
	int_power(Vars, SDomain, 1, TabSize),
	TabSize =< 256,
	/* case gmp is not there !! */
	TabSize > 0, !,
	average_cpt(Vars, OVars, Domain, TotEvidence,  Softness, CPT).
build_avg_table(Vars, OVars, Domain, Key, TotEvidence, Softness, CPT, [V1,V2], Vs, [V1,V2|NewVs]) :-
	length(Vars,L),
	LL1 is L//2,
	LL2 is L-LL1,
	list_split(LL1, Vars, L1, L2),
	Min = 0,
	length(Domain,Max1), Max is Max1-1,
	build_intermediate_table(LL1, sum(Min,Max), L1, V1, Key,  1.0, 0, I1, Vs, Vs1),
	build_intermediate_table(LL2, sum(Min,Max), L2, V2, Key, 1.0, I1, _, Vs1, NewVs),
	average_cpt([V1,V2], OVars, Domain, TotEvidence, Softness, CPT).

build_max_table(Vars, Domain, Softness, p(Domain, CPT, Vars), Vs, Vs) :-
	length(Domain, SDomain),
	int_power(Vars, SDomain, 1, TabSize),
	TabSize =< 16,
	/* case gmp is not there !! */
	TabSize > 0, !,
	max_cpt(Vars, Domain, Softness, CPT).
build_max_table(Vars, Domain, Softness, p(Domain, CPT, [V1,V2]), Vs, [V1,V2|NewVs]) :-
	length(Vars,L),
	LL1 is L//2,
	LL2 is L-LL1,
	list_split(LL1, Vars, L1, L2),
	build_intermediate_table(LL1, max(Domain,CPT), L1, V1, Key, 1.0,  0, I1, Vs, Vs1),
	build_intermediate_table(LL2, max(Domain,CPT), L2, V2, Key, 1.0, I1, _, Vs1, NewVs),
	max_cpt([V1,V2], Domain, Softness, CPT).

build_min_table(Vars, Domain, Softness, p(Domain, CPT, Vars), Vs, Vs) :-
	length(Domain, SDomain),
	int_power(Vars, SDomain, 1, TabSize),
	TabSize =< 16,
	/* case gmp is not there !! */
	TabSize > 0, !,
	min_cpt(Vars, Domain, Softness, CPT).
build_min_table(Vars, Domain, Softness, p(Domain, CPT, [V1,V2]), Vs, [V1,V2|NewVs]) :-
	length(Vars,L),
	LL1 is L//2,
	LL2 is L-LL1,
	list_split(LL1, Vars, L1, L2),
	build_intermediate_table(LL1, min(Domain,CPT), L1, V1, Key, 1.0,  0, I1, Vs, Vs1),
	build_intermediate_table(LL2, min(Domain,CPT), L2, V2, Key, 1.0, I1, _, Vs1, NewVs),
	min_cpt([V1,V2], Domain, Softness, CPT).

int_power([], _, TabSize, TabSize).
int_power([_|L], X, I0, TabSize) :-
	I is I0*X,
	int_power(L, X, I, TabSize).

build_intermediate_table(1,_,[V],V, _, _, I, I, Vs, Vs) :- !.
build_intermediate_table(2, Op, [V1,V2], V, Key, Softness, I0, If, Vs, Vs) :- !,
	If is I0+1,
	generate_tmp_random(Op, 2, [V1,V2], V, Key, Softness, I0).
build_intermediate_table(N, Op, L, V, Key, Softness, I0, If, Vs, [V1,V2|NewVs]) :-
	LL1 is N//2,
	LL2 is N-LL1,
	list_split(LL1, L, L1, L2),
	I1 is I0+1,
	build_intermediate_table(LL1, Op, L1, V1, Key, Softness, I1, I2, Vs, Vs1),
	build_intermediate_table(LL2, Op, L2, V2, Key, Softness, I2, If, Vs1, NewVs),
	generate_tmp_random(Op, N, [V1,V2], V, Key, Softness, I0).

% averages are transformed into sums.
generate_tmp_random(sum(Min,Max), N, [V1,V2], V, Key, Softness, I) :-
	Lower is Min*N,
	Upper is Max*N,
	generate_list(Lower, Upper, Nbs),
	sum_cpt([V1,V2], Nbs, Softness, CPT),
	generate_var('AVG'(I,Key), Nbs, CPT, [V1,V2], V).
%	write(sum(Nbs, CPT, [V1,V2])),nl, % debugging
generate_tmp_random(max(Domain,CPT), _, [V1,V2], V, Key, I) :-
	generate_var('MAX'(I,Key), Domain, CPT, [V1,V2], V).
generate_tmp_random(min(Domain,CPT), _, [V1,V2], V, Key, I) :-
	generate_var('MIN'(I,Key), Domain, CPT, [V1,V2], V).

generate_var(VKey, Domain, CPT, Parents, V) :-
	{ V = VKey with tab(Domain, CPT, Parents) }.

generate_list(M, M, [M]) :- !.
generate_list(I, M, [I|Nbs]) :-
	I1 is I+1,
	generate_list(I1, M, Nbs).

list_split(0, L, [], L) :- !.
list_split(I, [H|L], [H|L1], L2) :-
	I1 is I-1,
	list_split(I1, L, L1, L2).

%
% if we have evidence, we need to check if we are always consistent, never consistent, or can be consistent
%
include_qevidence(V, MAT0, MAT, NewParents0, NewParents, Vs, IVs, NewVs) :-
	clpbn:get_atts(V,[evidence(Ev)]), !,
	normalise_CPT_on_lines(MAT0, MAT1, L1),
	check_consistency(L1, Ev, MAT0, MAT1, L1, MAT, NewParents0, NewParents, Vs, IVs, NewVs).
include_qevidence(_, MAT, MAT, NewParents, NewParents, _, Vs, Vs).

check_consistency(L1, Ev, MAT0, MAT1, L1, MAT, NewParents0, NewParents, Vs, IVs, NewVs) :-
	sumlist(L1, Tot),
	nth0(Ev, L1, Val),
	(
	  Val == Tot
	->
	  MAT1 = MAT,
	  NewParents = [],
	  Vs = NewVs
	;
	  Val == 0.0 ->
	    throw(error(domain_error(incompatible_evidence),evidence(Ev)))
	  ;
	    MAT0 = MAT,
	    NewParents = NewParents0,
	    IVs = NewVs
	).


%
% generate actual table, instead of trusting the solver
%

average_cpt(Vs, OVars, Vals, Base, _, MCPT) :-
	get_ds_lengths(Vs,Lengs),
	length(OVars, N),
	length(Vals, SVals),
	matrix_new(floats,[SVals|Lengs],MCPT),
	fill_in_average(Lengs,N,Base,MCPT).

get_ds_lengths([],[]).
get_ds_lengths([V|Vs],[Sz|Lengs]) :-
	get_vdist_size(V, Sz),
	get_ds_lengths(Vs,Lengs).

fill_in_average(Lengs, N, Base, MCPT) :-
	generate(Lengs, Case),
	average(Case, N, Base, Val),
	matrix_set(MCPT,[Val|Case],1.0),
	fail.
fill_in_average(_,_,_,_).

generate([], []).
generate([N|Lengs], [C|Case]) :-
	from(0,N,C),
	generate(Lengs, Case).

from(I,_,I).
from(I1,M,J) :-
	I is I1+1,
	I < M,
	from(I,M,J).

average(Case, N, Base, Val) :-
	sum_list(Case, Base, Tot),
	Val is integer(round(Tot/N)).


sum_cpt(Vs,Vals,_,CPT) :-
	get_ds_lengths(Vs,Lengs),
	length(Vals,SVals),
	matrix_new(floats,[SVals|Lengs],MCPT),
	fill_in_sum(Lengs,MCPT),
	matrix_to_list(MCPT,CPT).

fill_in_sum(Lengs,MCPT) :-
	generate(Lengs, Case),
	sumlist(Case, Val),
	matrix_set(MCPT,[Val|Case],1.0),
	fail.
fill_in_sum(_,_).


max_cpt(Vs,Vals,_,CPT) :-
	get_ds_lengths(Vs,Lengs),
	length(Vals,SVals),
	matrix_new(floats,[SVals|Lengs],MCPT),
	fill_in_max(Lengs,MCPT),
	matrix_to_list(MCPT,CPT).

fill_in_max(Lengs,MCPT) :-
	generate(Lengs, Case),
	max_list(Case, Val),
	matrix_set(MCPT,[Val|Case],1.0),
	fail.
fill_in_max(_,_).


min_cpt(Vs,Vals,_,CPT) :-
	get_ds_lengths(Vs,Lengs),
	length(Vals,SVals),
	matrix_new(floats,[SVals|Lengs],MCPT),
	fill_in_max(Lengs,MCPT),
	matrix_to_list(MCPT,CPT).

fill_in_min(Lengs,MCPT) :-
	generate(Lengs, Case),
	max_list(Case, Val),
	matrix_set(MCPT,[Val|Case],1.0),
	fail.
fill_in_min(_,_).


get_vdist_size(V, Sz) :-
	var(V), !,
	clpbn:get_atts(V, [dist(Dist,_)]),
	get_dist_domain_size(Dist, Sz).
get_vdist_size(V, Sz) :-
	skolem(V, Dom),
	length(Dom, Sz).

