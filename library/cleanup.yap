/**
 * @file   cleanup.yap
 * @author Christian Thaeter
 * @date   Tue Nov 17 14:52:58 2015
 * 
 * @brief  old implementation of call_cleanup
 * 
 * 
*/


:- module( cleanup, [
		     call_cleanup/2,
		     call_cleanup/1,
		     on_cleanup/1,
		     cleanup_all/0,
		     op(1150, fx,fragile)
		    ]).

%% @defgroup cleanup Old Call Cleanup
% @ingroup library
% @{
%
% <tt>call_cleanup/1</tt> and <tt>call_cleanup/2</tt> allow predicates to register
% code for execution after the call is finished. Predicates can be
% declared to be <tt>fragile</tt> to ensure that <tt>call_cleanup</tt> is called
% for any Goal which needs it. This library is loaded with the
% `use_module(library(cleanup))` command.
%
% cleanup.yap
% Copyright (C) 2002 by Christian Thaeter
%
% public interface:
%
% :- fragile name/arity.
%       declares the predicate denoted by name/arity as fragile predicate.
%       Whenever such a fragile predicate is used in a query it will be
%       called through call_cleanup/1.
%
% call_cleanup(Goal).
% call_cleanup(Goal,CleanUpGoal).
%       Goal will be called in a cleanup-context, where any registered
%       CleanUpGoal inside of that context will be called when Goal is left,
%       either by a fail, cut or exeption.
%       It is possible to nest cleanup contexts.
%
% on_cleanup(CleanUpGoal).
%       registers CleanUpGoal to the current cleanup context.
%       CleanUpGoal's are executed in reverse order of their registration.
%       throws an exception if called outside of any cleanup-context.
%
% cleanup_all.
%       calls all pending CleanUpGoals and resets the cleanup-system to an initial state. 
%       should only be used as one of the last calls in the main program.
%
% hidden predicates:
% most private predicates could also be used in special cases, such as manually setting up cleanup-contexts.
% Read the Source.




:- multifile user:goal_expansion/3.

:- user_defined_directive(fragile(G), cleanup:cleanup_expansion(G)).

:- meta_predicate
	call_cleanup(:,:),
	call_cleanup(:),
	on_cleanup(:),
	on_cleanup(?,:),
	on_cleanupz(:),
	on_cleanupz(?,:).


:- initialization(init_cleanup).
init_cleanup :-
	bb_put(expansion_toggle,1),
	\+ bb_get(cleanup_level,_),
	bb_put(cleanup_level,0).
	% TODO: would be nice to register cleanup_all into the
	% toplevel to be called after each query is finished
init_cleanup.

% call goal G  with a cleanup CL in a cleanup context
call_cleanup(G,CL) :-
	needs_cleanup(L),
	on_cleanup(L,CL),
	(
		catch(G,X,(do_cleanup(L),throw(X)))
	;
		do_cleanup(L)
	).


% call a goal G in a cleanup context
call_cleanup(G) :-
	needs_cleanup(L),
	(
		catch(G,X,(do_cleanup(L),throw(X)))
	;
		do_cleanup(L)
	).


% begin cleanup level
needs_cleanup(CL) :-
	bb_get(cleanup_level,L),
	CL is L + 1,
	bb_put(cleanup_level,CL).


cleanup_context(CL) :-
	bb_get(cleanup_level,CL).


% leave cleanup level, call all registred cleanup predicates within
do_cleanup(CL) :-
	CN is CL - 1,
	bb_put(cleanup_level,CN),
	next_cleanup(CL).

next_cleanup(CL) :-
	!,recorded(cleanup:handle,(L,G),R),
	CL =< L,
	erase(R),
	(call(G);true),
	next_cleanup(CL).

% clean up all remaining stuff / reinitialize cleanup-module
/** @pred cleanup_all 

Calls all pending CleanUpGoals and resets the cleanup-system to an
initial state. Should only be used as one of the last calls in the
main program.

There are some private predicates which could be used in special
cases, such as manually setting up cleanup-contexts and registering
CleanUpGoals for other than the current cleanup-context.
Read the Source Luke.
 */
cleanup_all :-
	do_cleanup(1).
cleanup_all.

% register a cleanup predicate (normal reverse-order cleanup)
/** @pred on_cleanup(+ _CleanUpGoal_) 

Any Predicate might registers a  _CleanUpGoal_. The
 _CleanUpGoal_ is put onto the current cleanup context. All such
CleanUpGoals are executed in reverse order of their registration when
the surrounding cleanup-context ends. This call will throw an exception
if a predicate tries to register a  _CleanUpGoal_ outside of any
cleanup-context.
*/
on_cleanup(G) :-
	bb_get(cleanup_level,L),
	on_cleanup(L,G).

on_cleanup(L,G) :-
	L =< 0,
	throw(error(instantiation_error,no_cleanup_context(G))).
on_cleanup(L,G) :-
	callable(G),
	recorda(cleanup:handle,(L,G),_).


% register a cleanup predicate (reverse-reverse-order cleanup)
on_cleanupz(G) :-
	bb_get(cleanup_level,L),
	on_cleanupz(L,G).

on_cleanupz(L,G) :-
	L =< 0,
	throw(no_cleanup_context(G)).
on_cleanupz(L,G) :-
	callable(G),
	recordz(cleanup:handle,(L,G),_).

% helpers
cleanup_expansion(X) :-
	var(X),!,throw(error(instantiation_error,fragile(X))).
cleanup_expansion((H,T)) :- !,cleanup_expansion(H),cleanup_expansion(T).
cleanup_expansion([H,T]) :- !, cleanup_expansion(H),
	( T = [] -> true ; cleanup_expansion(T) ).
cleanup_expansion(M:G/A) :-
 	atom(G),integer(A),!,
	compose_var_goal(G/A,GG),
        \+ user:goal_expansion(GG,M,call_cleanup(M:GG)),
	assert((   user:goal_expansion(GG,M,NG)
	       :-  bb_get(expansion_toggle,1)
	       ->  bb_put(expansion_toggle,0),
		   NG=call_cleanup(M:GG)
	       ;   bb_put(expansion_toggle,1),
		   NG=M:GG )).
cleanup_expansion(G/A) :-
       !,prolog_flag(typein_module,M),cleanup_expansion(M:G/A).
cleanup_expansion(X) :-
	!,throw(error(instantiation_error,fragile(X))).

compose_var_goal(G/A,NG) :-
	arity_to_vars(A,L), NG =.. [G|L].

arity_to_vars(N,L) :-
	arity_to_vars(N,[],L).
arity_to_vars(N,L1,L2) :-
	N > 0,
	NN is N-1,
	LT = [L|L1],
	arity_to_vars(NN,LT,L2).
arity_to_vars(0,L,L).

/**
@}
*/
