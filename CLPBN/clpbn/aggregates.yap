
:- module(clpbn_aggregates, [cpt_average/4]).

:- use_module(library(clpbn), [{}/1]).

:- use_module(library(lists), [last/2]).

cpt_average(Vars, Key, Els0, CPT) :-
	check_domain(Els0, Els),
	length(Els, SDomain),
	build_avg_table(Vars, Els, SDomain, Key, CPT).

build_avg_table(Vars, Els, SDomain, _, p(Els, average, Vars)) :-
	int_power(Vars, SDomain, 1, TabSize),
	TabSize =< 16, !.
build_avg_table(Vars, Els, _, Key, p(Els, normalised_average(L), [V1,V2])) :-
	length(Vars,L),
	LL1 is L//2,
	LL2 is L-LL1,
	list_split(LL1, Vars, L1, L2),
	Els = [Min|Els1],
	last(Els1,Max),
	build_intermediate_table(LL1, sum(Min,Max), L1, V1, Key,  0, I1),
	build_intermediate_table(LL2, sum(Min,Max), L2, V2, Key, I1, _).
	
int_power([], _, TabSize, TabSize).
int_power([_|L], X, I0, TabSize) :-
	I is I0*X,
	int_power(L, X, I, TabSize).

build_intermediate_table(1,_,[V],V, _, I, I) :- !.
build_intermediate_table(2, Op, [V1,V2], V, Key, I0, If) :- !,
	If is I0+1,
	generate_tmp_random(Op, 2, [V1,V2], V, Key, I0).
build_intermediate_table(N, Op, L, V, Key, I0, If) :-
	LL1 is N//2,
	LL2 is N-LL1,
	list_split(LL1, L, L1, L2),
	I1 is I0+1,
	generate_tmp_random(Op, N, [V1,V2], V, Key, I0),
	build_intermediate_table(LL1, Op, L1, V1, Key, I1, I2),
	build_intermediate_table(LL2, Op, L2, V2, Key, I2, If).

% averages are transformed into sums.
generate_tmp_random(sum(Min,Max), N, [V1,V2], V, Key, I) :-
	Lower is Min*N,
	Upper is Max*N,
	generate_list(Lower, Upper, Nbs),
%%	write(sum(Nbs,[V1,V2])),nl, % debugging
	{ V = 'AVG'(I,Key) with p(Nbs,sum,[V1,V2]) }.

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
	
