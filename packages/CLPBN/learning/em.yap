%
% The world famous EM algorithm, in a nutshell
%

:- module(clpbn_em, [em/5]).

:- reexport(library(clpbn),
		[clpbn_flag/2,
		 clpbn_flag/3
		]).

:- use_module(library(clpbn),
		[clpbn_init_graph/1,
		 clpbn_init_solver/4,
		 clpbn_run_solver/3,
		 pfl_init_solver/5,
		 pfl_run_solver/3,
		 pfl_end_solver/1,
		 conditional_probability/3,
		 clpbn_flag/2
		]).

:- use_module(library('clpbn/dists'),
		[get_dist_domain_size/2,
		 empty_dist/2,
		 dist_new_table/2,
		 get_dist_key/2,
		 randomise_all_dists/0,
		 uniformise_all_dists/0
		]).

:- use_module(library('clpbn/ground_factors'),
		[generate_network/5,
		 f/3
		]).

:- use_module(library('clpbn/utils'),
		[check_for_hidden_vars/3,
		 sort_vars_by_key/3
		]).

:- use_module(library('clpbn/learning/learn_utils'),
		[run_all/1,
		 clpbn_vars/2,
		 normalise_counts/2,
		 compute_likelihood/3,
		 soften_sample/2
		]).

:- use_module(library(bhash),
		[b_hash_new/1,
		 b_hash_lookup/3,
		 b_hash_insert/4
		]).

:- use_module(library(matrix),
		[matrix_add/3,
		 matrix_to_list/2
		]).

:- use_module(library(lists),
		[member/2]).

:- use_module(library(rbtrees),
		[rb_new/1,
		 rb_insert/4,
		 rb_lookup/3
		]).

:- use_module(library(maplist)).


:- meta_predicate em(:,+,+,-,-), init_em(:,-).

em(Items, MaxError, MaxIts, Tables, Likelihood) :-
	catch(init_em(Items, State),Error,handle_em(Error)),
	em_loop(0, 0.0, State, MaxError, MaxIts, Likelihood, Tables),
	end_em(State),
	assert(em_found(Tables, Likelihood)),
	fail.
% get rid of new random variables the easy way :)
em(_, _, _, Tables, Likelihood) :-
	retract(em_found(Tables, Likelihood)).


handle_em(error(repeated_parents)) :- !,
	assert(em_found(_, -inf)),
	fail.
handle_em(Error) :-
	throw(Error).


end_em(state(_AllDists, _AllDistInstances, _MargKeys, SolverState)) :-
	clpbn:use_parfactors(on), !,
	pfl_end_solver(SolverState).
end_em(_).

% This gets you an initial configuration. If there is a lot of evidence
% tables may be filled in close to optimal, otherwise they may be
% close to uniform.
% it also gets you a run for random variables

% state collects all Info we need for the EM algorithm
% it includes the list of variables without evidence,
% the list of distributions for which we want to compute parameters,
% and more detailed info on distributions, namely with a list of all instances for the distribution.
init_em(Items, State) :-
	clpbn_flag(em_solver, Solver),
	% only used for PCGs
	clpbn_init_graph(Solver),
%	randomise_all_dists,
	% set initial values for distributions
	uniformise_all_dists,
	setup_em_network(Items, State).

setup_em_network(Items, state(AllDists, AllDistInstances, MargKeys, SolverState)) :-
	clpbn:use_parfactors(on), !,
	% get all variables to marginalise
	run_examples(Items, Keys, Factors, EList),
	% get the EM CPT connections info from the factors
	generate_dists(Factors, EList, AllDists, AllDistInstances, MargKeys),
	% setup solver, if necessary
	pfl_init_solver(MargKeys, Keys, Factors, EList, SolverState).
setup_em_network(Items, state(AllDists, AllDistInstances, MargVars, SolverState)) :-
	% create the ground network
	call_run_all(Items),
	% get all variables to marginalise
	attributes:all_attvars(AllVars0),
	% and order them
	sort_vars_by_key(AllVars0,AllVars,[]),
	% remove variables that do not have to do with this query.
	different_dists(AllVars, AllDists, AllDistInstances, MargVars),
	% setup solver by doing parameter independent work.
	clpbn_init_solver(MargVars, AllVars, _, SolverState).

run_examples(user:Exs, Keys, Factors, EList) :-
	Exs = [[_|_]|_], !,
	foldl(add_key, Exs, KExs, 1, _),
	findall(ex(EKs, EFs, EEs), run_example(KExs, EKs, EFs, EEs), VExs),
	foldl4(join_example, VExs, [], Keys, [], Factors, [], EList, 0, _).
run_examples(Items, Keys, Factors, EList) :-
	run_ex(Items, Keys, Factors, EList).
 
add_key(Ex, I:Ex, I, I1) :-
	I1 is I+1.

join_example( ex(EKs, EFs, EEs), Keys0, Keys, Factors0, Factors, EList0, EList, I0, I) :-
	I is I0+1,
	foldl(process_key(I0), EKs, Keys0, Keys),
	foldl(process_factor(I0), EFs, Factors0, Factors),
	foldl(process_ev(I0), EEs, EList0, EList).

process_key(I0, K, Keys0, [I0:K|Keys0]).

process_factor(I0, f(Type, Id, Keys), Keys0, [f(Type, Id, NKeys)|Keys0]) :-
	maplist(update_key(I0), Keys, NKeys).

update_key(I0, K, I0:K).

process_ev(I0, K=V, Es0, [(I0:K)=V|Es0]).

run_example([_:Items|_], Keys, Factors, EList) :-
	run_ex(user:Items, Keys, Factors, EList).
run_example([_|LItems], Keys, Factors, EList) :-
	run_example(LItems, Keys, Factors, EList).

run_ex(Items, Keys, Factors, EList) :-
	% create the ground network
	call_run_all(Items),
	attributes:all_attvars(AllVars0),
	% and order them
	sort_vars_by_key(AllVars0,AllVars,[]),
	% no, we are in trouble because we don't know the network yet.
	% get the ground network
	generate_network(AllVars, _, Keys, Factors, EList).

% loop for as long as you want.
em_loop(Its, Likelihood0, State, MaxError, MaxIts, LikelihoodF, FTables) :-
	estimate(State, LPs),
	maximise(State, Tables, LPs, Likelihood),
	ltables(Tables, F0Tables),
	%writeln(iteration:Its:Likelihood:Its:Likelihood0:F0Tables),
	(
	  (
	    abs((Likelihood - Likelihood0)/Likelihood) < MaxError
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
ltables([Id-T|Tables], [Key-LTable|FTables]) :-
	matrix_to_list(T,LTable),
	get_dist_key(Id, Key),
	ltables(Tables, FTables).


generate_dists(Factors, EList, AllDists, AllInfo, MargVars) :-
	b_hash_new(Ev0),
	foldl(elist_to_hash, EList, Ev0, Ev),
	maplist(process_factor(Ev), Factors, Dists0),
	sort(Dists0, Dists1),
	group(Dists1, AllDists, AllInfo, MargVars0, []),
	sort(MargVars0, MargVars).

elist_to_hash(K=V, Ev0, Ev) :-
	b_hash_insert(Ev0, K, V, Ev).

process_factor(Ev, f(bayes,Id,Ks), i(Id, Ks, Cases, NonEvs)) :-
	foldl( fetch_evidence(Ev), Ks, CompactCases, [], NonEvs),
	uncompact_cases(CompactCases, Cases).

fetch_evidence(Ev, K, E, NonEvs, NonEvs) :-
	b_hash_lookup(K, E, Ev), !.
fetch_evidence(_Ev, K, Ns, NonEvs, [K|NonEvs]) :-
	pfl:skolem(K,D),
	foldl(domain_to_number, D, Ns, 0, _).

domain_to_number(_, I0, I0, I) :-
	I is I0+1.


% collect the different dists we are going to learn next.
different_dists(AllVars, AllDists, AllInfo, MargVars) :-
	all_dists(AllVars, AllVars, Dists0),
	sort(Dists0, Dists1),
	group(Dists1, AllDists, AllInfo, MargVars0, []),
	sort(MargVars0, MargVars).

%
% V -> to Id defining V. We get:
% the random variables that are parents
% the cases that can happen, eg if we have A <- B, C
% A and B are boolean w/o evidence, and C is f, the cases could be
% [0,0,1], [0,1,1], [1,0,0], [1,1,0],
% Hiddens will be C
%
all_dists([], _, []).
all_dists([V|AllVars], AllVars0, [i(Id, [V|Parents], Cases, Hiddens)|Dists]) :-
	% V is an instance of Id
	clpbn:get_atts(V, [dist(Id,Parents)]),
	sort([V|Parents], Sorted),
	length(Sorted, LengSorted),
	length(Parents, LengParents),
	(
	  LengParents+1 =:= LengSorted
	->
	  true
	;
	  throw(error(repeated_parents))
	),
	generate_hidden_cases([V|Parents], CompactCases, Hiddens),
	uncompact_cases(CompactCases, Cases),
	all_dists(AllVars, AllVars0, Dists).

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


compact_mvars([], []).
compact_mvars([X1,X2|MargVars], CMVars) :- X1 == X2, !,
	compact_mvars([X2|MargVars], CMVars).
compact_mvars([X|MargVars], [X|CMVars]) :- !,
	compact_mvars(MargVars, CMVars).

estimate(state(_, _, Margs, SolverState), LPs) :-
	clpbn:use_parfactors(on), !,
	pfl_run_solver(Margs, LPs, SolverState).
estimate(state(_, _, Margs, SolverState), LPs) :-
	clpbn_run_solver(Margs, LPs, SolverState).

maximise(state(_,DistInstances,MargVars,_), Tables, LPs, Likelihood) :-
	rb_new(MDistTable0),
	foldl(create_mdist_table, MargVars, LPs, MDistTable0, MDistTable),
	compute_parameters(DistInstances, Tables, MDistTable, 0.0, Likelihood, LPs:MargVars).

create_mdist_table(Vs, Ps, MDistTable0, MDistTable) :-
	rb_insert(MDistTable0, Vs, Ps, MDistTable).

compute_parameters([], [], _, Lik, Lik, _).
compute_parameters([Id-Samples|Dists], [Id-NewTable|Tables], MDistTable, Lik0, Lik, LPs:MargVars) :-
	empty_dist(Id, Table0),
	add_samples(Samples, Table0, MDistTable),
%matrix_to_list(Table0,Mat), lists:sumlist(Mat, Sum), format(user_error, 'FINAL ~d ~w ~w~n', [Id,Sum,Mat]),
	soften_sample(Table0, SoftenedTable),
%	matrix:matrix_sum(Table0,TotM),
	normalise_counts(SoftenedTable, NewTable),
	compute_likelihood(Table0, NewTable, DeltaLik),
	dist_new_table(Id, NewTable),
	NewLik is Lik0+DeltaLik,
	compute_parameters(Dists, Tables, MDistTable, NewLik, Lik, LPs:MargVars).

add_samples([], _, _).
add_samples([i(_,_,[Case],[])|Samples], Table, MDistTable) :- !,
	matrix_add(Table,Case,1.0),
	add_samples(Samples, Table, MDistTable).
add_samples([i(_,_,Cases,Hiddens)|Samples], Table, MDistTable) :-
	rb_lookup(Hiddens, Ps, MDistTable),
	run_sample(Cases, Ps, Table),
%matrix_to_list(Table,M), format(user_error, '~w ~w~n', [Cases,Ps]),
	add_samples(Samples, Table, MDistTable).

run_sample([], [], _).
run_sample([C|Cases], [P|Ps], Table) :-
	matrix_add(Table, C, P),
	run_sample(Cases, Ps, Table).

call_run_all(Mod:Items) :-
	clpbn_flag(em_solver, pcg), !,
	backtrack_run_all(Items, Mod).
call_run_all(Mod:Items) :-
	run_all(Mod:Items).

backtrack_run_all([Item|_], Mod) :-
	call(Mod:Item),
	fail.
backtrack_run_all([_|Items], Mod) :-
	backtrack_run_all(Items, Mod).
backtrack_run_all([], _).

