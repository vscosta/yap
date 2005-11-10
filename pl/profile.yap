/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		profile.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	Some profiling predicates available in yap		 *
*									 *
*************************************************************************/

:- meta_predicate profile_data(:,+,-).

profile_data(M:D, Parm, Data) :- P = M:D, !,
	(
	  var(M) ->
	  '$do_error'(instantiation_error,profile_data(M:D, Parm, Data))
	;
	  '$profile_data'(D, Parm, Data, M)
	).
profile_data(P, Parm, Data) :-
	'$current_module'(M),
	'$profile_data'(P, Parm, Data, M).

'$profile_data'(P, Parm, Data,M) :- var(P), !,
	'$profile_data_for_var'(P, Parm, Data,M).
'$profile_data'(M:P, Parm, Data, _) :-  !,
	'$profile_data'(P, Parm, Data, M).
'$profile_data'(P, Parm, Data, M) :-
	'$profile_data2'(P, Parm, Data, M).

'$profile_data2'(Na/Ar,Parm,Data, M) :-
	functor(P, Na, Ar),
	'$profile_info'(M, P, Stats),
	'$profile_say'(Stats, Parm, Data).

'$profile_data_for_var'(Name/Arity, Parm, Data, M) :-
	'$current_predicate'(M,Name,Arity),
	functor(P,Name,Arity),
	\+ '$hidden'(Name), % don't show hidden predicates.
	'$profile_info'(M, P, Stats),
	'$profile_say'(Stats, Parm, Data).

'$profile_say'('$profile'(Entries, _, _), calls, Entries).
'$profile_say'('$profile'(_, _, Backtracks), retries, Backtracks).

profile_reset :-
	current_module(M),
	'$current_predicate'(M,Na,Ar),
	functor(P,Na,Ar),
	'$profile_reset'(M, P),
	fail.
profile_reset.

