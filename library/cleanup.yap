:- module( cleanup, [
		call_cleanup/2,
		call_cleanup/1,
		on_cleanup/1,
		cleanup_all/0
	]).

/*
public interface:

call_cleanup(Goal).
call_cleanup(Goal,CleanUpGoal).
	Goal will be called in a cleanup-context, where any registered
	CleanUpGoal inside of that context will be called when Goal is left,
	either by a fail, cut or exeption.
	It is possible to nest cleanup contexts.

on_cleanup(CleanUpGoal).
	registers CleanUpGoal to the current cleanup context.
	CleanUpGoal's are executed in reverse order of their registration.
	throws an exception if called outside of any cleanup-context.

cleanup_all.
	calls all pending CleanUpGoals and resets the cleanup-system to an initial state.
	should only be used as one of the last calls in the main program.


hidden predicates:
most private predicates could also be used in special cases, such as manually setting up cleanup-contexts.
Read the Source.
*/


:- meta_predicate 
	call_cleanup(:,:),
	call_cleanup(:),
	on_cleanup(:),
	on_cleanup(?,:),
	on_cleanupz(:),
	on_cleanupz(?,:).


:- initialization(init_cleanup).
init_cleanup :-
	get_value('cleanup:level',[]),
	set_value('cleanup:level',0).
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
	get_value('cleanup:level',L),
	CL is L + 1,
	set_value('cleanup:level',CL).


% leave cleanup level, call all registred cleanup predicates within
do_cleanup(CL) :-
	CN is CL - 1,
	set_value('cleanup:level',CN),
	next_cleanup(CL).

next_cleanup(CL) :-
	!,recorded(cleanup:handle,(L,G),R),
	CL =< L,
	erase(R),
	(call(G);true),
	next_cleanup(CL).


% clean up all remaining stuff / reinitialize cleanup-module
cleanup_all :- do_cleanup(1).


% register a cleanup predicate (normal reverse-order cleanup)
on_cleanup(G) :-
	get_value('cleanup:level',L),
	on_cleanup(L,G).

on_cleanup(L,G) :-
	L =< 0,
	throw(no_cleanup_context(G)).
on_cleanup(L,G) :-
	callable(G),
	recorda(cleanup:handle,(L,G),_).


% register a cleanup predicate (reverse-reverse-order cleanup)
on_cleanupz(G) :-
	get_value('cleanup:level',L),
	on_cleanupz(L,G).

on_cleanupz(L,G) :-
	L =< 0,
	throw(no_cleanup_context(G)).
on_cleanupz(L,G) :-
	callable(G),
	recordz(cleanup:handle,(L,G),_).

