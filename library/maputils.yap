
/**
 * @file   maputils.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 22:48:58 2015
 * 
 * @brief  Auxiliary routines for map... libraries
 * 
 * 
*/
%%%%%%%%%%%%%%%%%%%%
% map utilities
%%%%%%%%%%%%%%%%%%%%

:- module(maputils,
	  [compile_aux/2,
	   goal_expansion_allowed/0,
	   pred_name/4,
	   aux_preds/5,
	   append_args/3]).

/**
* @addtogroup maplist
  *
  * Auxiliary routines
  *
  *@{
*/
:- use_module(library(lists), [append/3]).

%%	goal_expansion_allowed is semidet.
%
%	`True` if we can use
%	goal-expansion.
goal_expansion_allowed :-
	once( prolog_load_context(_, _) ), % make sure we are compiling.
	\+ current_prolog_flag(xref, true).

:- dynamic number_of_expansions/1.

number_of_expansions(0).

%
% compile auxiliary routines for term expansion
%
compile_aux([Clause|Clauses], Module) :-
	% compile the predicate declaration if needed
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

pred_name(Macro, Arity, P , Name) :-
        prolog_load_context(file, FullFileName),
	file_base_name( FullFileName, File ),
	prolog_load_context(term_position, Pos),
	stream_position_data( line_count, Pos, Line ), !,
	transformation_id(Id),
	atomic_concat(['$$$ for ',Macro,'/',Arity,', line ',Line,' in ',File,'(',P,') #',Id], Name).
pred_name(Macro, Arity, P , Name) :-
    transformation_id(Id),
	atomic_concat(['$$$__expansion__ for ',Macro,'/',Arity,'(',P,') #',Id], Name).

transformation_id(Id) :-
    retract(number_of_expansions(Id)),
    !,
    Id1 is Id+1,
    assert(number_of_expansions(Id1)).
transformation_id(0).

/**
  @}
*/
