
:- module(clpbn_aggregates, [
	cpt_average/4,
	cpt_average/5,
	cpt_max/4,
	cpt_min/4
	]).

:- use_module(library(clpbn), [{}/1]).

:- use_module(library(lists), [last/2]).

cpt_average(Vars, Key, Els0, CPT) :-
	check_domain(Els0, Els),
	length(Els, SDomain),
	build_avg_table(Vars, Els, SDomain, Els0, Key, 1.0, CPT).

cpt_average(Vars, Key, Els0, Softness, CPT) :-
	check_domain(Els0, Els),
	length(Els, SDomain),
	build_avg_table(Vars, Els, SDomain, Els0, Key, Softness, CPT).

cpt_max(Vars, Key, Els0, CPT) :-
	check_domain(Els0, Els),
	length(Els, SDomain),
	build_max_table(Vars, Els, SDomain, Els0, Key, CPT).

cpt_min(Vars, Key, Els0, CPT) :-
	check_domain(Els0, Els),
	length(Els, SDomain),
	build_min_table(Vars, Els, SDomain, Els0, Key, CPT).

build_avg_table(Vars, Domain, SDomain, ODomain, _, 1.0, p(ODomain, CPT, Vars)) :-
	int_power(Vars, SDomain, 1, TabSize),
	TabSize =< 16, 
	/* case gmp is not there !! */
	TabSize > 0, !,
	average_cpt(Vars, Domain, CPT).
build_avg_table(Vars, Domain, _, ODomain, Key, Softness, p(ODomain, CPT, [V1,V2])) :-
	length(Vars,L),
	LL1 is L//2,
	LL2 is L-LL1,
	list_split(LL1, Vars, L1, L2),
	Domain = [Min|Els1],
	last(Els1,Max),
	build_intermediate_table(LL1, sum(Min,Max), L1, V1, Key,  Softness, 0, I1),
	build_intermediate_table(LL2, sum(Min,Max), L2, V2, Key, Softness, I1, _),
	normalised_average_cpt(L, [V1,V2], Domain, Softness, CPT).
	
build_max_table(Vars, Domain, SDomain, ODomain, _, p(ODomain, CPT, Vars)) :-
	int_power(Vars, SDomain, 1, TabSize),
	TabSize =< 16, !,
	max_cpt(Vars, Domain, CPT).
build_max_table(Vars, Domain, _, ODomain, Key, p(ODomain, CPT, [V1,V2])) :-
	length(Vars,L),
	LL1 is L//2,
	LL2 is L-LL1,
	list_split(LL1, Vars, L1, L2),
	build_intermediate_table(LL1, max(Domain,CPT), L1, V1, Key, 1.0,  0, I1),
	build_intermediate_table(LL2, max(Domain,CPT), L2, V2, Key, 1.0, I1, _),
	max_cpt([V1,V2], Domain, CPT).
	
build_min_table(Vars, Domain, SDomain, ODomain, _, p(ODomain, CPT, Vars)) :-
	int_power(Vars, SDomain, 1, TabSize),
	TabSize =< 16, !,
	min_cpt(Vars, Domain, CPT).
build_min_table(Vars, Domain, _, ODomain, Key, p(ODomain, CPT, [V1,V2])) :-
	length(Vars,L),
	LL1 is L//2,
	LL2 is L-LL1,
	list_split(LL1, Vars, L1, L2),
	build_intermediate_table(LL1, min(Domain,CPT), L1, V1, Key, 1.0,  0, I1),
	build_intermediate_table(LL2, min(Domain,CPT), L2, V2, Key, 1.0, I1, _),
	min_cpt([V1,V2], Domain, CPT).
	
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

% allow quick description for a range.
check_domain([I0|Is], [I0|Is]) :-
	integer(I0),
	check_integer_domain(Is,I0), !.
check_domain(D, ND) :-
	normalise_domain(D, 0, ND).

check_integer_domain([],_).
check_integer_domain([I1|Is],I0) :-
	I0 < I1,
	check_integer_domain(Is,I1).

normalise_domain([], _, []).
normalise_domain([_|D], I0, [I0|ND]) :-
	I is I0+1,
	normalise_domain(D, I, ND).


%
% generate actual table, instead of trusting the solver
%
	
average_cpt(Vs,Vals,CPT) :-
	generate_indices(Vals,Inds,0,Av),
	combine_all(Vs, Inds, Cs),
	length(Vs, Max),
	average_possible_cases(0, Av, Max, Cs, 1.0, CPT).

sum_cpt(Vs, Vals, Softness, CPT) :-
	length(Vals,Sz),
	combine_all(Vs, Cs),
	sum_possible_cases(0, Sz, Cs, Softness, CPT).

normalised_average_cpt(Max, Vs, Vals, Softness, CPT) :-
	generate_indices(Vals,_,0,Sz),
	combine_all(Vs, Cs),
	average_possible_cases(0, Sz, Max, Cs, Softness, CPT).


generate_indices([],[],Av,Av).
generate_indices([_|Ls],[I|Inds],I,Av) :-
	I1 is I+1,
	generate_indices(Ls,Inds,I1,Av).


combine_all([], [[]]).
combine_all([V|LV], Cs) :-
	combine_all(LV, Cs0),
	get_dist_size(V,Sz),
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

average_possible_cases(Av,Av,_,_,_,[]) :- !.
average_possible_cases(I,Av,Max,Cs,Softness,Lf) :-
	average_cases2(Cs,I,Av,Softness,Lf,L0),
	I1 is I+1,
	average_possible_cases(I1,Av,Max,Cs,Softness,L0).

average_cases2([], _, _, _, L, L).
average_cases2([C|Cs], I, Av, Softness, [P|Lf], L0) :-
	calculate_avg_prob(C, I, Av, Softness, P),
	average_cases2(Cs, I, Av, Softness, Lf, L0).

calculate_avg_prob(C, I, Av, Softness, Softness) :-
	sum_all(C,0,N),
	I =:= integer(round(N/Av)), !.
calculate_avg_prob(_, _, Av, Softness, Comp) :-
	Comp is (1.0-Softness)/(Av-1).

sum_possible_cases(Av,Av,_, _, []) :- !.
sum_possible_cases(I,Av,Cs,Softness, Lf) :-
	sum_cases2(Cs,I, Av, Softness, Lf,L0),
	I1 is I+1,
	sum_possible_cases(I1,Av,Cs,Softness, L0).

sum_cases2([], _, _, _, L, L).
sum_cases2([C|Cs], I, Av, Softness, [P|Lf], L0) :-
	calculate_sum_prob(C, I, Av, Softness, P),
	sum_cases2(Cs, I, Av, Softness, Lf, L0).

calculate_sum_prob(C, I, _, Softness, Softness) :-
	sum_all(C,0,N),
	I =:= N, !.
calculate_sum_prob(_, _, Av, Softness, Comp) :-
	Comp is (1.0-Softness)/(Av-1).

%
% generate a CPT for max.
%
max_cpt(Vs, Domain, CPT) :-
	combinations(Vs, Domain, Combinations),
	cpt_from_domain(Domain, Combinations, Domain, max, CPT).

min_cpt(Vs, Domain, CPT) :-
	combinations(Vs, Domain, Combinations),
	cpt_from_domain(Domain, Combinations, Domain, min, CPT).

combinations(Vs, Domain, Combinations) :-
	mult_domains(Vs, Domain, Domains),
	cart(Domains, Combinations).

mult_domains([], _, []).
mult_domains([_|Vs], Domain, [Domain|Domains]) :-
	mult_domains(Vs, Domain, Domains).

cart([], [[]]).
cart([L|R], Rf) :-
	cart(R, R1),
	add(L, R1, Rf).

add([], _, []).
add([A|R], R1, RsF) :-
	add_head(R1, A, RsF, Rs0),
	add(R, R1, Rs0).

add_head([], _, Rs, Rs).
add_head([H|L], A, [[A|H]|Rs], Rs0) :-
	add_head(L, A, Rs, Rs0).

cpt_from_domain([], _, _, _, []).
cpt_from_domain([El|Domain], Combinations, Domain0, OP, CPT) :-
	cpt_from_domain_el(Combinations, El, Domain0, OP, CPT, CPT0),
	cpt_from_domain(Domain, Combinations, Domain0, OP, CPT0).

cpt_from_domain_el([], _, _, _, CPT, CPT).
cpt_from_domain_el([C|Combinations], El, Domain, OP, [P|CPT], CPT0) :-
	cpt_for_el(C, OP, El, Domain, 0.0, P),
	cpt_from_domain_el(Combinations, El, Domain, OP, CPT, CPT0).

cpt_for_el([], _, _, _, P, P).
cpt_for_el([El|Cs], MAX, El, Domain, _, P) :- !,
	cpt_for_el(Cs, MAX, El, Domain, 1.0, P).
cpt_for_el([C|_], MAX, El, Domain, _, 0.0) :-
	op_broken(MAX, C, El, Domain), !.
cpt_for_el([_|Cs], MAX, El, Domain, P0, P) :-
	cpt_for_el(Cs, MAX, El, Domain, P0, P).

op_broken(max, C, El, Domain) :-
	lg(Domain, C, El).
op_broken(min, C, El, Domain) :-
	sm(Domain, C, El).

lg([El|_], _, El) :- !.
lg([C|_], C, _) :- !, fail.
lg([_|Vs], C, El) :-
	lg(Vs, C, El).

sm([El|_], _, El) :- !, fail.
sm([V|_], V, _) :- !.
sm([_|Vs], C, El) :-
	sm(Vs, C, El).

get_dist_size(V, Sz) :-
	clpbn:get_atts(V, [dist(Vals,_,_)]),
	length(Vals, Sz).

