%
% Interface the Aleph ILP system to CLP(BN)
%
% Aleph was written by Ashwin Srinivasan
%
% this code relies on hacked version of Aleph, contact
% vsc@dcc.fc.up.pt
%
% Aleph generates clauses as usual,
% but some variables are of special type random.
%
:- module(clpbn_aleph,
		[init_clpbn_cost/0,
		 random_type/2
		]).

:- dynamic rt/2, inited/1.

:- use_module(library('clpbn'),
		[{}/1,
		 clpbn_flag/2,
		 clpbn_flag/3,
		 set_clpbn_flag/2,
		 op(500, xfy, with)
		]).

:- use_module(library('clpbn/learning/em')).

:- use_module(library('clpbn/matrix_cpt_utils'),
		[uniform_CPT_as_list/2]).

:- use_module(library('clpbn/dists'),
		[reset_all_dists/0,
		 get_dist_key/2,
		 get_dist_params/2
		]).

:- use_module(library('clpbn/table'),
		[clpbn_tabled_abolish/1,
		 clpbn_tabled_asserta/1,
		 clpbn_tabled_asserta/2,
		 clpbn_tabled_assertz/1,
		 clpbn_tabled_clause/2,
		 clpbn_tabled_clause_ref/3,
		 clpbn_tabled_number_of_clauses/2,
		 clpbn_is_tabled/1,
		 clpbn_reset_tables/0,
		 clpbn_tabled_dynamic/1
		]).

:- dynamic '$aleph_global'/2.
%
% Tell Aleph not to use default solver during saturation
%
% all work will be done by EM
%:- set_clpbn_flag(solver,none).

%
% This is the Aleph interface
% examples are stored as example(Id, Type, Example)
% CPT domains are stored as random_type(KeySkeleton, ListOfValues).
%

:- use_module(library(lists),[append/3]).

:- multifile user:cost/3.

% handle uninstantiated examples as hidden variables.
:- user:set(skolem_examples, false).

% avoid doing CLP(BN) stuff except at start
:- user:set(sat_begin_hook, clpbn_aleph:disable_solver).
:- user:set(sat_end_hook, clpbn_aleph:enable_solver).

:- user:set(reduce_begin_hook, clpbn_aleph:disable_solver).
:- user:set(reduce_end_hook, clpbn_aleph:enable_solver).

:- user:set(best_clause_hook, clpbn_aleph:add_new_clause).

disable_solver(_,_) :-
	disable_solver.
disable_solver(_) :-
	disable_solver.

disable_solver :-
	clpbn_flag(solver, Old, none),
	nb_setval(old_clpbn_solver, Old).

enable_solver(_) :-
	enable_solver.
enable_solver(_,_) :-
	enable_solver.

enable_solver :-
	nb_getval(old_clpbn_solver, Old),
	set_clpbn_flag(solver, Old).

% step 1: update distributions to better values.
add_new_clause(_,(H :- _),_,_) :-
	(
	  clpbn_is_tabled(user:H)
	->
	  update_tabled_theory(H)
	;
	  update_theory(H)
	),
	fail.
% step 2: add clause
add_new_clause(_,(_ :- true),_,_) :- !.
add_new_clause(_,(H :- B),_,_) :-
%trace,
%	user:db_usage,
%	user:db_dynamic,
	domain(H, K, V, D),
	user:cost((H :- B), _, _Cost),
	rewrite_body(B, IB, Vs, _, ( !, { V = K with p(D, CPTList, Vs) })),
	% need to remember which CPT we want
	get_dist_key(Id, K),
	get_dist_params(Id, CPTList),
	(
	  clpbn_is_tabled(user:H)
	->
	  clpbn_tabled_asserta(user:(H :- IB))
	;
	  asserta(user:(H :- IB))
	),
	user:setting(verbosity,V),
	( V >= 1 ->
	  user:p_message('CLP(BN) Theory'),
	  functor(H,N,Ar), listing(user:N/Ar)
	;
	  true
	).


update_tabled_theory(H) :-
	clpbn_tabled_clause_ref(user:H,B,Ref),
	add_correct_cpt(B,NB),
	erase(Ref),
	clpbn_tabled_assertz((user:(H:-NB))),
	fail.
update_tabled_theory(_).

update_theory(H) :-
	clause(user:H,B,Ref),
	add_correct_cpt(B,NB),
	erase(Ref),
	assert((user:H:-NB)),
	fail.
update_theory(_).

add_correct_cpt((G,B),(G,NB)) :-
	add_correct_cpt(B,NB).
add_correct_cpt((clpbn:{V = K with Tab }), ({V = K with NTab})) :-
	correct_tab(Tab,K,NTab).
add_correct_cpt(({V = K with Tab }), ({V = K with NTab})) :-
	correct_tab(Tab,K,NTab).

correct_tab(p(Vs,_),K,p(Vs,TDist)) :-
	get_dist_key(Id, K),
	get_dist_params(Id, TDist).
correct_tab(p(Vs,_,Ps),K,p(Vs,TDist,Ps)) :-
	get_dist_key(Id, K),
	get_dist_params(Id, TDist).

% user-defined cost function, Aleph knows about this (and only about this).
user:cost((H :- B),Inf,Score) :-
	domain(H, K, V, D),
	check_info(Inf),
	rewrite_body(B, IB, Vs, Ds, ( !, { V = K with p(D, CPTList, Vs) })),
	uniform_cpt([D|Ds], CPTList),
	(
	  clpbn_is_tabled(user:H)
	->
	  clpbn_reset_tables,
	  clpbn_tabled_asserta(user:(H :- IB), R)
	;
	  asserta(user:(H :- IB), R)
	),
	(
	  cpt_score(Score0)
	->
	  erase(R),
	  Score is -Score0
	;
	  % illegal clause, just get out of here.
	  erase(R),
	  fail
	).
user:cost(H,_Inf,Score) :- !,
	init_clpbn_cost(H, Score0),
	Score is -Score0.

% this is here so that Aleph will actually compute coverage. Aleph computes
% coverage only if cost actually checks Inf.
check_info(_).

init_clpbn_cost(_, Score) :-
	inited(Score), !.
init_clpbn_cost(H, Score) :-
	functor(H,N,A),
	% get rid of Aleph crap
	(
	  clpbn_is_tabled(user:H)
	->
	  clpbn_tabled_abolish(user:N/A),
	  clpbn_tabled_dynamic(user:N/A)
	;
	  abolish(user:N/A),
	  % make it easy to add and remove clauses.
	  dynamic(user:N/A)
	),
	domain(H, K, V, D),
	uniform_cpt([D], CPTList),
	% This will be the default cause, called when the other rules fail.
	(
	  clpbn_is_tabled(user:H)
	->
	  clpbn_tabled_assertz(user:(H :- !, { V = K with p(D, CPTList) }))
	 ;
	  assert(user:(H :- !, { V = K with p(D, CPTList) }))
	),
	cpt_score(Score),
	assert(inited(Score)).

% receives H, and generates a key K, a random variable RV, and a domain D.
domain(H, K, RV, D) :-
	functor(H,Name,Arity),
	functor(Pred,Name,Arity),
	(
	  recorded(aleph,modeh(_,Pred),_)
	->
	  true
	;
	  user:'$aleph_global'(modeh,modeh(_,Pred))
	),
	arg(Arity,Pred,+RType),
	rt(RType,D), !,
	key_from_head(H,K,RV).
domain(H, K, V, D) :-
	current_predicate(_,user:domain(_)),
	key_from_head(H,K,V),
	user:domain(K,D).

key_from_head(H,K,V) :-
	H =.. [Name|Args],
	(
	  clpbn_is_tabled(user:H)
	->
	  clpbn_tabled_number_of_clauses(user:H,NClauses)
	;
	  predicate_property(user:H,number_of_clauses(NClauses))
	),
	atomic_concat(Name,NClauses,NName),
	append(H0L,[V],Args),
	K =.. [NName|H0L].

% transforms_body into something that is going to be called
% receives G0, and generates a list of goals, a list of variables, and a list of domains.
% receives also a Tail with the constraint to append at the end.
rewrite_body((A,B), (user:NA,NB), [V|Vs], [D|Ds], Tail) :-
	rewrite_goal(A, V, D, NA), !,
	rewrite_body(B, NB, Vs, Ds, Tail).
rewrite_body((A,B), (user:A,NB), Vs, Ds, Tail) :- !,
	rewrite_body(B,NB, Vs, Ds, Tail).
rewrite_body(A,(user:NA,Tail), [V], [D], Tail) :-
	rewrite_goal(A, V, D, NA), !.
rewrite_body(A, (user:A,Tail), [], [], Tail).

% so they need not be rewritten.
rewrite_goal(A,V,D,NA) :-
	functor(A,Name,Arity),
	functor(Pred,Name,Arity),
	(
	  recorded(aleph,modeb(_,Pred),_)
	->
	  true
	;
	  user:'$aleph_global'(modeb,modeb(_,Pred))
	),
	arg(Arity,Pred,-RType),
	rt(RType,D), !,
	A =.. [Name|Args],
	replace_last_var(Args,V,NArgs),
	NA =.. [Name|NArgs].

replace_last_var([_],V,[V]) :- !.
replace_last_var([A|Args],V,[A|NArgs]) :-
	replace_last_var(Args,V,NArgs).


%
% This is the key
%
cpt_score(Lik) :-
	findall(user:Ex, user:example(_,pos,Ex), Exs),
	clpbn_flag(solver, Solver),
	clpbn_flag(em_solver, EMSolver),
	set_clpbn_flag(solver, EMSolver),
	reset_all_dists,
	em(Exs, 0.01, 10, _Tables, Lik),
	set_clpbn_flag(solver, Solver).

complete_clpbn_cost(_AlephClause).

random_type(A,B) :-
	assert(rt(A,B)).


uniform_cpt(Ds, CPTList) :-
	lengths(Ds, Ls),
	uniform_CPT_as_list(Ls, CPTList).

lengths([], []).
lengths([D|Ds], [L|Ls]) :-
	length(D, L),
	lengths(Ds, Ls).

