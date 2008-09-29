%
% The world famous EM algorithm, in a nutshell
%

:- module(clpbn_em, [em/6]).

:- use_module(library(lists),
	      [append/3]).

:- use_module(library('clpbn/learning/learn_utils'),
	      [run_all/1,
	       clpbn_vars/2,
	       normalise_counts/2]).

em(Items, MaxError, MaxIts, Tables, Likelihood) :-
	init_em(Items, State),
	em_loop(0, 0.0, state(AllVars,AllDists), MaxError, MaxIts, Likelihood),
	get_tables(State, Tables).

% This gets you an initial configuration. If there is a lot of evidence
% tables may be filled in close to optimal, otherwise they may be
% close to uniform.
% it also gets you a run for random variables
init_em(Items, state(AllVars, AllDists, AllDistInstances)) :-
	run_all(Items),
	different_dists(AllVars, AllDists, AllDistInstances).

% loop for as long as you want.
em_loop(MaxIts, Likelihood, State, _, _, MaxIts, Likelihood) :- !.
em_loop(Its, Likelihood0, State, MaxError, MaxIts, LikelihoodF) :-
	estimate(State),
	maximise(State, Likelihood),
	(
	 (
	     (Likelihood - Likelihood0)/Likelihood < MaxError
	 ;
	     Its == MaxIts
	 )	 
	->
	 LikelihoodF = Likelihood
	;
	 Its1 is Its+1,
	 em_loop(Its1, Likelihood, State, MaxError, MaxIts, LikelihoodF)
	).

% collect the different dists we are going to learn next.
different_dists(AllVars, AllDists, AllInfo) :-
	all_dists(AllVars, Dists0, AllInfo),
	sort(Dists0, Dists1),
	group(Dists1, AllInfo).

group([], []).
group([i(Id,V,Ps)|Dists1], [Id-[[V|Ps]|Extra]|AllInfo]) :-
	same_id(Dists1, Id, Extra, Rest),
	group(Rest, AllInfo).

same_id([i(Id,V,Ps)|Dists1], Id, [[V|Ps]|Extra], Rest) :- !,
	same_id(Dists1, Id, Extra, Rest).
same_id(Dists, _, [], Dists).

all_dists([], [], []).
all_dists([V|AllVars], Dists, [i(Id, AllInfo, Parents)|AllInfo]) :-
	clpbn:get_atts(V, [dist(Id,_)]),
	with_evidence(V, Id, Dists, Dists0), !,
	all_dists(AllVars, Dists0, AllInfo).

with_evidence(V, Id) -->
	{clpbn:get_atts(V, [evidence(Pos)]) }, !,
	{ dist_pos2bin(Pos, Id, Bin) }.
with_evidence(V, Id) -->
	[d(V,Id)].

estimate(state(Vars,Info,_)) :-
	clpbn_solve_graph(Vars, OVars),
	marg_vars(Info, Vars).

marg_vars([], _).
marg_vars([d(V,Id)|Vars], AllVs) :-
	clpbn_marginalise_in_vars(V, AllVs),
	marg_vars(Vars, AllVs).

maximise(state(_,_,DistInstances), Tables, Likelihood) :-
	compute_parameters(DistInstances, Tables, 0.0, Likelihood).

compute_parameters([], [], Lik, Lik).
compute_parameters([Id-Samples|Dists], [Tab|Tables], Lik0, Lik) :-
	empty_dist(Id, NewTable),
	add_samples(Samples, NewTable),
	normalise_table(Id, NewTable),
	compute_parameters(Dists, Tables, Lik0, Lik).

add_samples([], _).
add_samples([S|Samples], Table) :-
	run_sample(S, 1.0,  Pos, Tot),
	matrix_add(Table, Pos, Tot),
	fail.
add_samples([_|Samples], Table) :-
	add_samples(Samples, Table).
	
run_sample([], Tot,  [], Tot).
run_sample([V|S], W0,  [P|Pos], Tot) :-
	{clpbn:get_atts(V, [evidence(P)]) }, !,	
	run_sample(S, W0,  Pos, Tot).
run_sample([V|S], W0,  [P|Pos], Tot) :-
	{clpbn_display:get_atts(V, [posterior,(_,_,Ps,_)]) },
	count_cases(Ps, 0, D0, P),
	W1 is D0*W0,
	run_sample(S, W1,  Pos, Tot).
	
count_cases([D0|Ps], I0, D0, I0).
count_cases([_|Ps], I0, P, W1) :-
	I is I0+1,
	count_cases(Ps, I, P, W1).

