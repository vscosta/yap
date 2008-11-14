%
% Interface the Aleph ILP system to CLP(BN)
%
% Relies on the Aleph cost function.
% It assumes Aleph work as usual,  but some variables are of type random.
%
:- module(clpbn_aleph,
	  [init_clpbn_cost/0,
	  random_type/2]).

:- dynamic rt/2, inited/1.

:- use_module(library('clpbn/learning/em')).

:- use_module(library('clpbn'),
	[{}/1,
	 clpbn_flag/2,
	 clpbn_flag/3,
         set_clpbn_flag/2]).

:- use_module(library('clpbn/matrix_cpt_utils'),
	[uniform_CPT_as_list/2]).

:- use_module(library('clpbn/dists'),
	[reset_all_dists/0,
	 get_dist_key/2,
	 get_dist_params/2
     ]).

%
% Tell Aleph not to use default solver during saturation
%
% all work will be done by EM 
:- set_clpbn_flag(solver,none).

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
:- user:set(sat_start_hook, clpbn_aleph:disable_solver).
:- user:set(sat_stop_hook, clpbn_aleph:enable_solver).

:- user:set(reduce_start_hook, clpbn_aleph:disable_solver).
:- user:set(reduce_stop_hook, clpbn_aleph:enable_solver).

disable_solver(_) :-
	clpbn_flag(solver, Old, none),
	nb_setval(old_clpbn_solver, Old).
disable_solver(_,_) :-
	clpbn_flag(solver, Old, none),
	nb_setval(old_clpbn_solver, Old).

enable_solver :-
	nb_getval(old_clpbn_solver, Old),
	set_clpbn_flag(solver, Old).
enable_solver(_,_) :-
	nb_getval(old_clpbn_solver, Old),
	set_clpbn_flag(solver, Old).

:- user:set(best_clause_hook, clpbn_aleph:add_new_clause).

add_new_clause(_,(_ :- true),_,_) :- !.
add_new_clause(_,(H :- B),_,_) :-
	user:db_usage,
	user:db_dynamic,
	domain(H, K, V, D),
	rewrite_body(B, IB, Vs, _, ( !, { V = K with p(D, CPTList, Vs) })),
	% need to remember which CPT we want
	get_dist_key(Id, K),
	get_dist_params(Id, CPTList),
	asserta(user:(H :- IB)),
	user:setting(verbosity,V),
	( V >= 1 -> 
	    user:p_message('CLP(BN) Theory'),
	    functor(H,N,Ar), listing(user:N/Ar)
	;
	    true
	).


% user-defined cost function, Aleph knows about this (and only about this). 
user:cost((H :- B),Inf,Score) :- !,
	domain(H, K, V, D),
	check_info(Inf),
	rewrite_body(B, IB, Vs, Ds, ( !, { V = K with p(D, CPTList, Vs) })),
	uniform_cpt([D|Ds], CPTList),
	asserta(user:(H :- IB), R),
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
	abolish(user:N/A),
	% make it easy to add and remove clauses.
	dynamic(user:N/A),
	domain(H, K, V, D),
	uniform_cpt([D], CPTList),
	% This will be the default cause, called when the other rules fail.
	assert(user:(H :- !, { V = K with p(D, CPTList) })),
	cpt_score(Score),
	assert(inited(Score)).

% receives H, and generates a key K, a random variable RV, and a domain  D.
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
	predicate_property(user:H,number_of_clauses(NClauses)),
	atomic_concat(Name,NClauses,NName),
	append(H0L,[V],Args),
	K =.. [NName|H0L].

% transforms_body into something that is going to be called
% receives G0, and generates a list of goals, a list of variables, and a list of domains.
% receives also a Tail with the constraint to append at the end.
rewrite_body((A,B), (user:NA,NB), [V|Vs], [D|Ds], Tail) :-
	rewrite_goal(A, V, D, NA), !,
	rewrite_body(B, NB, Vs, Ds, Tail).
rewrite_body((A,B), (user:A,NB), Vs, Ds, Tail) :-
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
	findall(user:Ex, user:example(_,pos,Ex),  Exs),
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

