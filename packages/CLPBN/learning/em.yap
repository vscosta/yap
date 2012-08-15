%
% The world famous EM algorithm, in a nutshell
%

:- module(clpbn_em, [em/5]).

:- use_module(library(lists),
	      [append/3,
	       delete/3]).

:- use_module(library(clpbn),
	      [clpbn_init_graph/1,
	       clpbn_init_solver/5,
	       clpbn_run_solver/4,
	       clpbn_finalize_solver/1,
	       conditional_probability/3,
	       clpbn_flag/2]).

:- use_module(library('clpbn/dists'),
	      [get_dist_domain_size/2,
	       empty_dist/2,
	       dist_new_table/2,
	       get_dist_key/2,
	       randomise_all_dists/0,
	       uniformise_all_dists/0]).

:- use_module(library(clpbn/ground_factors),
	      [generate_network/5,
	      f/3]).

:- use_module(library(bhash), [
          b_hash_new/1,
          b_hash_lookup/3,
	  b_hash_insert/4]).

:- use_module(library('clpbn/learning/learn_utils'),
	      [run_all/1,
	       clpbn_vars/2,
	       normalise_counts/2,
	       compute_likelihood/3,
	       soften_sample/2]).

:- use_module(library(lists),
	      [member/2]).

:- use_module(library(matrix),
	      [matrix_add/3,
	       matrix_to_list/2]).

:- use_module(library(rbtrees),
	      [rb_new/1,
	       rb_insert/4,
	       rb_lookup/3]).

:- use_module(library('clpbn/utils'),
	      [
	       check_for_hidden_vars/3,
	       sort_vars_by_key/3]).

:- meta_predicate em(:,+,+,-,-), init_em(:,-).

em(Items, MaxError, MaxIts, Tables, Likelihood) :-
	catch(init_em(Items, State),Error,handle_em(Error)),
	em_loop(0, 0.0, State, MaxError, MaxIts, Likelihood, Tables),
	clpbn_finalize_solver(State),
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
	% create the ground network
	call_run_all(Items),
%	randomise_all_dists,
	% set initial values for distributions
	uniformise_all_dists,
	setup_em_network(Solver, State).

setup_em_network(Solver, state( AllDists, AllDistInstances, MargVars, SolverState)) :-
	clpbn:use_parfactors(on), !,
	% get all variables to marginalise
	attributes:all_attvars(AllVars0),
	% and order them
	sort_vars_by_key(AllVars0,AllVars,[]),
	% no, we are in trouble because we don't know the network yet.
	% get the ground network
	generate_network([AllVars], _, Keys, Factors, EList),
	% get the EM CPT connections info from the factors
	generate_dists(Factors, EList, AllDists, AllDistInstances, MargVars),
	% setup solver, if necessary
	clpbn_init_solver(Solver, MargVars, _AllVars, ground(MargVars, Keys, Factors, EList), SolverState).
setup_em_network(Solver, state( AllDists, AllDistInstances, MargVars, SolverVars)) :-
	% get all variables to marginalise
	attributes:all_attvars(AllVars0),
	% and order them
	sort_vars_by_key(AllVars0,AllVars,[]),
	% remove variables that do not have to do with this query.
	different_dists(AllVars, AllDists, AllDistInstances, MargVars),
	% setup solver by doing parameter independent work.
	clpbn_init_solver(Solver, MargVars, AllVars, _, SolverVars).

% loop for as long as you want.
em_loop(Its, Likelihood0, State, MaxError, MaxIts, LikelihoodF, FTables) :-
	estimate(State, LPs),
	maximise(State, Tables, LPs, Likelihood),
	ltables(Tables, F0Tables),
	writeln(iteration:Its:Likelihood:Its:Likelihood0:F0Tables),
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
	elist_to_hash(EList, Ev0, Ev),
	process_factors(Factors, Ev, Dists0),
	sort(Dists0, Dists1),
	group(Dists1, AllDists, AllInfo, MargVars0, []),
	sort(MargVars0, MargVars).

elist_to_hash([], Ev, Ev).
elist_to_hash([K=V|EList], Ev0, Ev) :-
	b_hash_insert(Ev0, K, V, Evi),
	elist_to_hash(EList, Evi, Ev).

process_factors([], _Ev, []).
process_factors([f(bayes,Id,Ks)|Factors], Ev, [i(Id, Ks, Cases, NonEvs)|AllDistInstances]) :-
	fetch_evidence(Ks, Ev, CompactCases, NonEvs),
	uncompact_cases(CompactCases, Cases),
	process_factors(Factors, Ev, AllDistInstances).

fetch_evidence([], _Ev, [], []).
fetch_evidence([K|Ks], Ev, [E|CompactCases], NonEvs) :-
	b_hash_lookup(K, E, Ev), !,
	fetch_evidence(Ks, Ev, CompactCases, NonEvs).
fetch_evidence([K|Ks], Ev, [Ns|CompactCases], [K|NonEvs]) :-
	pfl:skolem(K,D),
	domain_to_numbers(D,0,Ns),
	fetch_evidence(Ks, Ev, CompactCases, NonEvs).

domain_to_numbers([],_,[]).
domain_to_numbers([_|D],I0,[I0|Ns]) :-
	I is I0+1,
	domain_to_numbers(D,I,Ns).
	

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

find_variables([], _AllVars0, []).
find_variables([K|PKeys], AllVars0, [Parent|Parents]) :-
	find_variable(K, AllVars0, Parent),
	find_variables(PKeys, AllVars0, Parents).

%
% in clp(bn) the whole network is constructed when you evaluate EM. In
% pfl, we want to delay execution until as late as possible.
% we just create a new variable and hope for the best.
%
%
find_variable(K, [], Parent) :-
	clpbn:put_atts(Parent, [key(K)]).
find_variable(K, [Parent|_AllVars0], Parent) :-
	clpbn:get_atts(Parent, [key(K0)]), K0 =@= K, !.
find_variable(K, [_|AllVars0], Parent) :-
	find_variable(K, AllVars0, Parent).

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
	clpbn_flag(em_solver, Solver),
	clpbn_run_solver(Solver, Margs, LPs, SolverState).

maximise(state(_,DistInstances,MargVars,_), Tables, LPs, Likelihood) :-
	rb_new(MDistTable0),
	create_mdist_table(MargVars, LPs, MDistTable0, MDistTable),
	compute_parameters(DistInstances, Tables, MDistTable, 0.0, Likelihood, LPs:MargVars).

create_mdist_table([],[],MDistTable,MDistTable).
create_mdist_table([Vs|MargVars],[Ps|LPs],MDistTable0,MDistTable) :-
	rb_insert(MDistTable0, Vs, Ps, MDistTableI),
	create_mdist_table(MargVars, LPs, MDistTableI ,MDistTable).

compute_parameters([], [], _, Lik, Lik, _).
compute_parameters([Id-Samples|Dists], [Id-NewTable|Tables],  MDistTable, Lik0, Lik, LPs:MargVars) :-
	empty_dist(Id, Table0),
	add_samples(Samples, Table0, MDistTable),
%matrix_to_list(Table0,Mat), lists:sumlist(Mat, Sum), format(user_error, 'FINAL ~d ~w ~w~n', [Id,Sum,Mat]),
	soften_sample(Table0, SoftenedTable),
%	matrix:matrix_sum(Table0,TotM),
	normalise_counts(SoftenedTable, NewTable),
	compute_likelihood(Table0, NewTable, DeltaLik),
	dist_new_table(Id, NewTable),
	NewLik is Lik0+DeltaLik,
	compute_parameters(Dists, Tables,  MDistTable, NewLik, Lik, LPs:MargVars).

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

