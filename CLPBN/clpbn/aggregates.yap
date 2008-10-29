
:- module(clpbn_aggregates, [
	cpt_average/4,
	cpt_average/5,
	cpt_max/4,
	cpt_min/4
	]).

:- use_module(library(clpbn), [{}/1]).

:- use_module(library(lists),
	[last/2,
	sumlist/2,
	max_list/2,
	min_list/2
    ]).

:- use_module(library(matrix),
	[matrix_new/3,
	matrix_to_list/2,
	matrix_set/3]).

:- use_module(dists, [get_dist_domain_size/2]).

cpt_average(Vars, Key, Els0, CPT) :-
	build_avg_table(Vars, Els0, Key, 1.0, CPT).

cpt_average(Vars, Key, Els0, Softness, CPT) :-
	build_avg_table(Vars, Els0, Key, Softness, CPT).

cpt_max(Vars, Key, Els0, CPT) :-
	build_max_table(Vars, Els0, Els0, Key, 1.0, CPT).

cpt_min(Vars, Key, Els0, CPT) :-
	build_min_table(Vars, Els0, Els0, Key, 1.0, CPT).

build_avg_table(Vars, Domain, _, Softness, p(Domain, CPT, Vars)) :-
	length(Domain, SDomain),
	int_power(Vars, SDomain, 1, TabSize),
	TabSize =< 16, 
	/* case gmp is not there !! */
	TabSize > 0, !,
	average_cpt(Vars, Domain, Softness, CPT).
build_avg_table(Vars, Domain, Key, Softness, p(Domain, CPT, [V1,V2])) :-
	length(Vars,L),
	LL1 is L//2,
	LL2 is L-LL1,
	list_split(LL1, Vars, L1, L2),
	Min = 0,
	length(Domain,Max1), Max is Max1-1,
	build_intermediate_table(LL1, sum(Min,Max), L1, V1, Key,  1.0, 0, I1),
	build_intermediate_table(LL2, sum(Min,Max), L2, V2, Key, 1.0, I1, _),
	average_cpt([V1,V2], Domain, Softness, CPT).
	
build_max_table(Vars, Domain, Softness, p(Domain, CPT, Vars)) :-
	length(Domain, SDomain),
	int_power(Vars, SDomain, 1, TabSize),
	TabSize =< 16, 
	/* case gmp is not there !! */
	TabSize > 0, !,
	max_cpt(Vars, Domain, Softness, CPT).
build_max_table(Vars, Domain, Softness, p(Domain, CPT, [V1,V2])) :-
	length(Vars,L),
	LL1 is L//2,
	LL2 is L-LL1,
	list_split(LL1, Vars, L1, L2),
	build_intermediate_table(LL1, max(Domain,CPT), L1, V1, Key, 1.0,  0, I1),
	build_intermediate_table(LL2, max(Domain,CPT), L2, V2, Key, 1.0, I1, _),
	max_cpt([V1,V2], Domain, Softness, CPT).
	
build_min_table(Vars, Domain, Softness, p(Domain, CPT, Vars)) :-
	length(Domain, SDomain),
	int_power(Vars, SDomain, 1, TabSize),
	TabSize =< 16, 
	/* case gmp is not there !! */
	TabSize > 0, !,
	min_cpt(Vars, Domain, Softness, CPT).
build_min_table(Vars, Domain, Softness, p(Domain, CPT, [V1,V2])) :-
	length(Vars,L),
	LL1 is L//2,
	LL2 is L-LL1,
	list_split(LL1, Vars, L1, L2),
	build_intermediate_table(LL1, min(Domain,CPT), L1, V1, Key, 1.0,  0, I1),
	build_intermediate_table(LL2, min(Domain,CPT), L2, V2, Key, 1.0, I1, _),
	min_cpt([V1,V2], Domain, Softness, CPT).
	
int_power([], _, TabSize, TabSize).
int_power([_|L], X, I0, TabSize) :-
	I is I0*X,
	int_power(L, X, I, TabSize).

build_intermediate_table(1,_,[V],V, _, _, I, I) :- !.
build_intermediate_table(2, Op, [V1,V2], V, Key, Softness, I0, If) :- !,
	If is I0+1,
	generate_tmp_random(Op, 2, [V1,V2], V, Key, Softness, I0).
build_intermediate_table(N, Op, L, V, Key, Softness, I0, If) :-
	LL1 is N//2,
	LL2 is N-LL1,
	list_split(LL1, L, L1, L2),
	I1 is I0+1,
	build_intermediate_table(LL1, Op, L1, V1, Key, Softness, I1, I2),
	build_intermediate_table(LL2, Op, L2, V2, Key, Softness, I2, If),
	generate_tmp_random(Op, N, [V1,V2], V, Key, Softness, I0).

% averages are transformed into sums.
generate_tmp_random(sum(Min,Max), N, [V1,V2], V, Key, Softness, I) :-
	Lower is Min*N,
	Upper is Max*N,
	generate_list(Lower, Upper, Nbs),
	sum_cpt([V1,V2], Nbs, Softness, CPT),
%	write(sum(Nbs, CPT, [V1,V2])),nl, % debugging
	{ V = 'AVG'(I,Key) with p(Nbs,CPT,[V1,V2]) }.
generate_tmp_random(max(Domain,CPT), _, [V1,V2], V, Key, I) :-
	{ V = 'MAX'(I,Key) with p(Domain,CPT,[V1,V2]) }.
generate_tmp_random(min(Domain,CPT), _, [V1,V2], V, Key, I) :-
	{ V = 'MIN'(I,Key) with p(Domain,CPT,[V1,V2]) }.

generate_list(M, M, [M]) :- !.
generate_list(I, M, [I|Nbs]) :-
	I1 is I+1,
	generate_list(I1, M, Nbs).

list_split(0, L, [], L) :- !.
list_split(I, [H|L], [H|L1], L2) :-
	I1 is I-1,
	list_split(I1, L, L1, L2).

%
% generate actual table, instead of trusting the solver
%

average_cpt(Vs,Vals,_,CPT) :-
	get_ds_lengths(Vs,Lengs),
	sumlist(Lengs, Tot),
	length(Vals,SVals),
	Factor is SVals/Tot,
	matrix_new(floats,[SVals|Lengs],MCPT),
	fill_in_average(Lengs,Factor,MCPT),
	matrix_to_list(MCPT,CPT).

get_ds_lengths([],[]).
get_ds_lengths([V|Vs],[Sz|Lengs]) :-
	get_vdist_size(V, Sz),
	get_ds_lengths(Vs,Lengs).
	
fill_in_average(Lengs,SVals,MCPT) :-
	generate(Lengs, Case),
	average(Case, SVals, Val),
	matrix_set(MCPT,[Val|Case],1.0),
	fail.
fill_in_average(_,_,_).

generate([], []).
generate([N|Lengs], [C|Case]) :-
	from(0,N,C),
	generate(Lengs, Case).

from(I,_,I).
from(I1,M,J) :-
	I is I1+1,
	I < M,
	from(I,M,J).

average(Case, SVals, Val) :-
	sumlist(Case, Tot),
	Val is integer(round(Tot*SVals)).


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
	clpbn:get_atts(V, [dist(Dist,_)]),
	get_dist_domain_size(Dist, Sz).

