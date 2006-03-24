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
* File:		callcount.yap						 *
* Last rev:	8/2/02							 *
* mods:									 *
* comments:	Some profiling predicates available in yap		 *
*									 *
*************************************************************************/

call_count_data(Calls, Retries, Both) :-
	'$call_count_info'(Calls, Retries, Both).

call_count_reset :-
	'$call_count_reset'.

call_count(Calls, Retries, Both) :-
	'$check_if_call_count_on'(Calls, CallsOn),
	'$check_if_call_count_on'(Retries, RetriesOn),
	'$check_if_call_count_on'(Both, BothOn),
	'$call_count_set'(Calls, CallsOn, Retries, RetriesOn, Both, BothOn).

'$check_if_call_count_on'(Calls, 1) :- integer(Calls), !.
'$check_if_call_count_on'(Calls, 0) :- var(Calls), !.
'$check_if_call_count_on'(Calls, A) :-
	'$do_error'(type_error(integer,Calls),call_count(A)).



