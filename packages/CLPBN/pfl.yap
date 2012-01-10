%
% This module defines PFL, the prolog factor language.
%
%

:- module(clpbn_parfactors, [
          factor/5,
	  skolem/2,
	  op(550,yfx,::),
	  op(1150,fx,bayes),
	  op(1150,fx,parfactor)]).

:- use_module(library(lists),
	[nth0/3,
	 append/3]).

:- dynamic factor/5, skolem_in/2, skolem/2, preprocess/3, evidence/2, id/1.

:- reexport(library(clpbn),
	[clpbn_flag/3,
	 set_clpbn_flag/2]).

:- set_clpbn_flag(use_factors,on).

user:term_expansion( bayes((Formula ; Phi ; Constraints)), clpbn_parfactors:factor(Id,FList,FV,Phi,Constraints)) :-
	!,
	term_variables(Formula, FreeVars),
	FV =.. [fv|FreeVars],
	new_id(Id),
	process_args(Formula, Id, 0, _, FList, []).
user:term_expansion( Goal, [] ) :-
	preprocess(Goal, Sk,Var), !,
	(ground(Goal) -> true ; throw(error('non ground evidence',Goal))),
%	prolog_load_context(module, M),
	assert(evidence(Sk,Var)).

id(0).

new_id(Id) :-
	retract(id(Id0)),
	Id is Id0+1,
	assert(id(Id)).

process_args((Arg1,Arg2), Id, I0, I ) --> !,
	process_args(Arg1, Id, I0, I1),
	process_args(Arg2, Id, I1, I).
process_args(Arg1, Id, I0, I ) -->
	{ I is I0+1 },
	process_arg(Arg1, Id, I).

process_arg(Sk::D, Id, _I) -->
	!,
	{
         new_skolem(Sk,D),
	 assert(skolem_in(Sk, Id))
        },
	[Sk].
process_arg(Sk, Id, _I) -->
	!,
	{
	 assert(skolem_in(Sk, Id))
        },
	[Sk].

new_skolem(Sk,D) :-
	copy_term(Sk, Sk1),
	skolem(Sk1, D1),
	Sk1 =@= Sk,
	!,
	D1 = D.
new_skolem(Sk,D) :-
	interface_predicate(Sk),
	assert(skolem(Sk, D)).

interface_predicate(Sk) :-
	Sk =.. SKAs,
	append(SKAs, [Var], ESKAs),
	ESk =.. ESKAs,
	assert(preprocess(ESk, Sk, Var)),
	assert_static((user:ESk :-
	     var(Var) -> insert_atts(Var,Sk) ; add_evidence(Sk,Var) )
	).

insert_atts(Var,Sk) :-
	clpbn:put_atts(Var,[key(Sk)]).

add_evidence(Sk,Var) :-
	skolem(Sk,D),
	once(nth0(E,D,Var)),
	clpbn:put_atts(_V,[key(Sk),evidence(E)]).


