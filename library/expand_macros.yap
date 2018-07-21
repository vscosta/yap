/**
 * @file   expand_macros.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 15:16:12 2015
 * 
 * @brief  utilities that perform macro expansion for maplist/2 and
 * friends.
 * 
 * 
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% preprocessing for meta-calls
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module( expand_macros,
           [compile_aux/2,
            pred_name/4,
            transformation_id/1,
            allowed_expansion/1,
            allowed_module/2] ).
            
       
:- use_module(library(lists), [append/3]).
:- use_module(library(charsio), [format_to_chars/3, read_from_chars/2]).
:- use_module(library(error), [must_be/2]).
:- use_module(library(occurs), [sub_term/2]).

:- multifile allowed_module/2.

:- dynamic number_of_expansions/1.

number_of_expansions(0).



%%%%%%%%%%%%%%%%%%%%
% utilities
%%%%%%%%%%%%%%%%%%%%

compile_aux([Clause|Clauses], Module) :-
	% compile the predicate declaration if needed
    (
	Clause = (Head :- _)
     ;
     Clause = Head
    ),
    !,
    functor(Head, F, N),
    ( current_predicate(Module:F/N)
     ->
	 true
     ;
     %	    format'*** Creating auxiliary predicate ~q~n', [F/N]),
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

aux_preds(Module:Meta, MetaVars, Pred, PredVars, Proto, _, OModule) :- !,
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Module, OModule).
aux_preds(Meta, MetaVars, Pred, PredVars, Proto, Module, Module) :-
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

	atomic_concat(['$$$__Auxiliary_predicate__ for',Macro,'/',Arity,'    ',Id], Name).

transformation_id(Id) :-
	retract(number_of_expansions(Id)),
	Id1 is Id+1,
	assert(number_of_expansions(Id1)).
	

harmless_dcgexception(instantiation_error).	% ex: phrase(([1],x:X,[3]),L)
harmless_dcgexception(type_error(callable,_)).	% ex: phrase(27,L)


allowed_expansion(QExpand) :-
    strip_module(QExpand, Mod, Pred),
    goal_expansion_allowed(Pred, Mod).

goal_expansion_allowed(Pred, Mod) :-
	allowed_module(Pred,Mod),
	once( prolog_load_context(_, _) ), % make sure we are compiling.



    
allowed_module(checklist(_,_),expand_macros).
allowed_module(checklist(_,_),apply_macros).
allowed_module(checklist(_,_),maplist).
allowed_module(maplist(_,_),expand_macros).
allowed_module(maplist(_,_),apply_macros).
allowed_module(maplist(_,_),maplist).
allowed_module(maplist(_,_,_),expand_macros).
allowed_module(maplist(_,_,_),apply_macros).
allowed_module(maplist(_,_,_),maplist).
allowed_module(maplist(_,_,_,_),expand_macros).
allowed_module(maplist(_,_,_,_),apply_macros).
allowed_module(maplist(_,_,_,_),maplist).
allowed_module(maplist(_,_,_,_,_),expand_macros).
allowed_module(maplist(_,_,_,_,_),apply_macros).
allowed_module(maplist(_,_,_,_,_),maplist).
allowed_module(maplist(_,_,_,_,_,_),expand_macros).
allowed_module(maplist(_,_,_,_,_,_),apply_macros).
allowed_module(maplist(_,_,_,_,_,_),maplist).
allowed_module(selectlist(_,_,_),expand_macros).
allowed_module(selectlist(_,_,_),apply_macros).
allowed_module(selectlist(_,_,_),maplist).
allowed_module(include(_,_,_),expand_macros).
allowed_module(include(_,_,_),apply_macros).
allowed_module(include(_,_,_),maplist).
allowed_module(exclude(_,_,_),expand_macros).
allowed_module(exclude(_,_,_),apply_macros).
allowed_module(exclude(_,_,_),maplist).
allowed_module(partition(_,_,_,_),expand_macros).
allowed_module(partition(_,_,_,_),apply_macros).
allowed_module(partition(_,_,_,_),maplist).
allowed_module(partition(_,_,_,_,_),expand_macros).
allowed_module(partition(_,_,_,_,_),apply_macros).
allowed_module(partition(_,_,_,_,_),maplist).
allowed_module(convlist(_,_,_),expand_macros).
allowed_module(convlist(_,_,_),apply_macros).
allowed_module(convlist(_,_,_),maplist).
allowed_module(sumlist(_,_,_,_),expand_macros).
allowed_module(sumlist(_,_,_,_),apply_macros).
allowed_module(sumlist(_,_,_,_),maplist).
allowed_module(mapargs(_,_,_),expand_macros).
allowed_module(mapargs(_,_,_),apply_macros).
allowed_module(mapargs(_,_,_),maplist).
allowed_module(sumargs(_,_,_,_),expand_macros).
allowed_module(sumargs(_,_,_,_),apply_macros).
allowed_module(sumargs(_,_,_,_),maplist).
allowed_module(mapnodes(_,_,_),expand_macros).
allowed_module(mapnodes(_,_,_),apply_macros).
allowed_module(mapnodes(_,_,_),maplist).
allowed_module(checknodes(_,_),expand_macros).
allowed_module(checknodes(_,_),apply_macros).
allowed_module(checknodes(_,_),maplist).
allowed_module(sumnodes(_,_,_,_),expand_macros).
allowed_module(sumnodes(_,_,_,_),apply_macros).
allowed_module(sumnodes(_,_,_,_),maplist).
