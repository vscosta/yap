%%%%%%%%%%%%%%%%%%%%
% map utilities
%%%%%%%%%%%%%%%%%%%%

/**
  * @file maputils.yap
  *
  * @addtogroup maplist
  *
  * Auxiliary routines
  *
  *@{
*/
:- module(maputils,
	  [compile_aux/2,
	   goal_expansion_allowed/0,
	   pred_name/4,
	   aux_preds/5,
	   append_args/3]).

:- use_module(library(lists), [append/3]).

:- dynamic number_of_expansions/1.

number_of_expansions(0).

compile_aux([Clause|Clauses], Module) :-
	% compile the predicat declaration if needed
	( Clause = (Head :- _)
	; Clause = Head ),
	!,
	functor(Head, F, N),
	( current_predicate(Module:F/N)
	->
	    true
	;
%	    format("*** Creating auxiliary predicate ~q~n", [F/N]),
%	    checklist(portray_clause, [Clause|Clauses]),
	    compile_term([Clause|Clauses], Module)
	).

compile_term([], _).
compile_term([Clause|Clauses], Module) :-
	assert_static(Module:Clause),
	compile_term(Clauses, Module).

append_args(Term, Args, NewTerm) :-
	Term =.. [Meta|OldArgs],
	append(OldArgs, Args, GoalArgs),
	NewTerm =.. [Meta|GoalArgs].

aux_preds(Meta, _, _, _, _) :-
	var(Meta), !,
	fail.
aux_preds(_:Meta, MetaVars, Pred, PredVars, Proto) :- !,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto).
aux_preds(Meta, MetaVars, Pred, PredVars, Proto) :-
	Meta =.. [F|Args],
	aux_args(Args, MetaVars, PredArgs, PredVars, ProtoArgs),
	Pred =.. [F|PredArgs],
	Proto =.. [F|ProtoArgs].

aux_args([], [], [], [], []).
aux_args([Arg|Args], MVars, [Arg|PArgs], PVars, [Arg|ProtoArgs]) :-
	ground(Arg), !,
	aux_args(Args, MVars, PArgs, PVars, ProtoArgs).
aux_args([Arg|Args], [Arg|MVars], [PVar|PArgs], [PVar|PVars], ['_'|ProtoArgs]) :-
	aux_args(Args, MVars, PArgs, PVars, ProtoArgs).

pred_name(Macro, Arity, _ , Name) :-
	transformation_id(Id),
	atomic_concat(['$$$__Auxiliary_predicate__ for ',Macro,'/',Arity,'    ',Id], Name).

transformation_id(Id) :-
	retract(number_of_expansions(Id)),
	Id1 is Id+1,
	assert(number_of_expansions(Id1)).

harmless_dcgexception(instantiation_error).	% ex: phrase(([1],x:X,[3]),L)
harmless_dcgexception(type_error(callable,_)).	% ex: phrase(27,L)


%%	contains_illegal_dcgnt(+ Term) is semidet.
%
%	`True` if _Term_ contains a non-terminal   we cannot deal with using
%	goal-expansion. The test is too general approximation, but safe.

contains_illegal_dcgnt(NT) :-
	sub_term(I, NT),
	nonvar(I),
	( I = ! ; I = phrase(_,_,_) ), !.
%	write(contains_illegal_nt(NT)),		% JW: we do not want to write
%	nl.

%%	goal_expansion_allowed is semidet.
%
%	`True` if we can use
%	goal-expansion.
goal_expansion_allowed :-
	once( prolog_load_context(_, _) ), % make sure we are compiling.
	\+ current_prolog_flag(xref, true).

/**
  @}
*/