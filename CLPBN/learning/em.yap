%
% The world famous EM algorithm, in a nutshell
%

:- module(clpbn_em, [em/5]).

:- use_module(library(lists),
	      [append/3]).

:- use_module(library(clpbn),
	      [clpbn_init_solver/3,
	       clpbn_run_solver/3]).

:- use_module(library('clpbn/dists'),
	      [get_dist_domain_size/2,
	       empty_dist/2,
	       dist_new_table/2]).

:- use_module(library('clpbn/learning/learn_utils'),
	      [run_all/1,
	       clpbn_vars/2,
	       normalise_counts/2,
	       compute_likelihood/3]).

:- use_module(library(lists),
	      [member/2]).

:- use_module(library(matrix),
	      [matrix_add/3,
	       matrix_to_list/2]).

:- use_module(library('clpbn/utils'), [
	check_for_hidden_vars/3]).

:- meta_predicate em(:,+,+,-,-), init_em(:,-).

em(Items, MaxError, MaxIts, Tables, Likelihood) :-
	init_em(Items, State),
	em_loop(0, 0.0, State, MaxError, MaxIts, Likelihood, Tables).

% This gets you an initial configuration. If there is a lot of evidence
% tables may be filled in close to optimal, otherwise they may be
% close to uniform.
% it also gets you a run for random variables

% state collects all Info we need for the EM algorithm
% it includes the list of variables without evidence,
% the list of distributions for which we want to compute parameters,
% and more detailed info on distributions, namely with a list of all instances for the distribution.
init_em(Items, state(AllVars, AllDists, AllDistInstances, MargVars)) :-
	run_all(Items),
	attributes:all_attvars(AllVars0),
	% remove variables that do not have to do with this query.
	check_for_hidden_vars(AllVars0, AllVars0, AllVars),
	different_dists(AllVars, AllDists, AllDistInstances, MargVars),
	clpbn_init_solver(MargVars, AllVars, _).

% loop for as long as you want.
em_loop(Its, Likelihood0, State, MaxError, MaxIts, LikelihoodF, FTables) :-
	estimate(State, LPs),
	maximise(State, Tables, LPs, Likelihood),
	(
	    (
	     (Likelihood - Likelihood0)/Likelihood < MaxError
	    ;
	     Its == MaxIts
	    )	 
	->
	 ltables(Tables, FTables),
	 LikelihoodF = Likelihood
	;
	 Its1 is Its+1,
	 em_loop(Its1, Likelihood, State, MaxError, MaxIts, LikelihoodF, FTables)
	).

ltables([], []).
ltables([Id-T|Tables], [Id-LTable|FTables]) :-
	matrix_to_list(T,LTable),
	ltables(Tables, FTables).
	 


% collect the different dists we are going to learn next.
different_dists(AllVars, AllDists, AllInfo, MargVars) :-
	all_dists(AllVars, Dists0),
	sort(Dists0, Dists1),
	group(Dists1, AllDists, AllInfo, MargVars, []).

all_dists([], []).
all_dists([V|AllVars], [i(Id, [V|Parents], Cases, Hiddens)|Dists]) :-
	clpbn:get_atts(V, [dist(Id,Parents)]),
	generate_hidden_cases([V|Parents], CompactCases, Hiddens),
	uncompact_cases(CompactCases, Cases),
	all_dists(AllVars, Dists).

generate_hidden_cases([], [], []).
generate_hidden_cases([V|Parents], [P|Cases], Hiddens) :-
	clpbn:get_atts(V, [evidence(P)]), !,
	generate_hidden_cases(Parents, Cases, Hiddens).
generate_hidden_cases([V|Parents], [Cases|MoreCases], [V|Hiddens]) :-
	clpbn:get_atts(V, [dist(Id,_)]),
	get_dist_domain_size(Id, Sz),
	gen_cases(0, Sz, Cases),
	generate_hidden_cases(Parents, MoreCases, Hiddens).
	
gen_cases(Sz, Sz, []) :- !.
gen_cases(I, Sz, [I|Cases]) :-
	I1 is I+1,
	gen_cases(I1, Sz, Cases).

uncompact_cases(CompactCases, Cases) :-
	findall(Case, is_case(CompactCases, Case), Cases).

is_case([], []).
is_case([A|CompactCases], [A|Case]) :-
	integer(A), !,
	is_case(CompactCases, Case).
is_case([L|CompactCases], [C|Case]) :-
	member(C, L),
	is_case(CompactCases, Case).

group([], [], []) --> [].
group([i(Id,Ps,Cs,[])|Dists1], [Id|Ids], [Id-[i(Id,Ps,Cs,[])|Extra]|AllInfo]) --> !,
	same_id(Dists1, Id, Extra, Rest),
	group(Rest, Ids, AllInfo).
group([i(Id,Ps,Cs,Hs)|Dists1], [Id|Ids], [Id-[i(Id,Ps,Cs,Hs)|Extra]|AllInfo]) -->
	[Hs],
	same_id(Dists1, Id, Extra, Rest),
	group(Rest, Ids, AllInfo).

same_id([i(Id,Vs,Cases,[])|Dists1], Id, [i(Id, Vs, Cases, [])|Extra], Rest) --> !,
	same_id(Dists1, Id, Extra, Rest).
same_id([i(Id,Vs,Cases,Hs)|Dists1], Id, [i(Id, Vs, Cases, Hs)|Extra], Rest) --> !,
	[Hs],
	same_id(Dists1, Id, Extra, Rest).
same_id(Dists, _, [], Dists) --> [].

estimate(state(Vars, _, _, Margs), LPs) :-
	clpbn_run_solver(Margs, Vars, LPs).

maximise(state(_,_,DistInstances,_), Tables, LPs, Likelihood) :-
	compute_parameters(DistInstances, Tables, LPs, 0.0, Likelihood).

compute_parameters([], [], [], Lik, Lik).
compute_parameters([Id-Samples|Dists], [Id-NewTable|Tables], Ps, Lik0, Lik) :-
	empty_dist(Id, Table0),
	add_samples(Samples, Table0, Ps, MorePs),
	normalise_counts(Table0, NewTable),
	compute_likelihood(Table0, NewTable, DeltaLik),
	dist_new_table(Id, NewTable),
	NewLik is Lik0+DeltaLik,
	compute_parameters(Dists, Tables, MorePs, NewLik, Lik).

add_samples([], _, Ps, Ps).
add_samples([i(_,_,[Case],[])|Samples], Table, AllPs, RPs) :- !,
	matrix_add(Table,Case,1.0),
	add_samples(Samples, Table, AllPs, RPs).
add_samples([i(_,_,Cases,_)|Samples], Table, [Ps|AllPs], RPs) :-
	run_sample(Cases, Ps, Table),
	add_samples(Samples, Table, AllPs, RPs).

run_sample([], [], _).
run_sample([C|Cases], [P|Ps], Table) :-
	matrix_add(Table, C, P),
	run_sample(Cases, Ps, Table).


