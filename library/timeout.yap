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
* File:		timeout.yap						 *
* Last rev:	5/12/99							 *
* mods:									 *
* comments:	Goal within timeout					 *
*									 *
*************************************************************************/

:- module(timeout, [
	time_out/3
    ]).

:- meta_predicate time_out(:,+,-).

%
% not the nicest program I've ever seen.
%
%
% I cannot use setup_call_cleanup because I can only take interrupts *after* I set up
% the catcher, so I have to do the dirty deed myself.
%
time_out(Goal, Time, Result) :-
	T is Time//1000,
	UT is (Time mod 1000)*1000,
	yap_hacks:disable_interrupts,
	% enable alarm
	alarm(T.UT,throw(time_out),_),
	% launch goal and wait for signal
	catch( run_goal(Goal, Result0),
	       time_out,
	       Result0 = time_out ),
	Result = Result0.

run_goal(Goal, Result0) :-
	% we can only enable interrupts after alarm was been enabled.
	yap_hacks:enable_interrupts,
	Result0 = success,
	call(Goal),
	!,
	% make sure we're not getting an extraneous interrupt if we terminate early....
	yap_hacks:disable_interrupts.
	alarm(0,_,_),
	true.
run_goal(Goal, Result0) :-
	yap_hacks:disable_interrupts,
	% make sure we're not getting an extraneous interrupt if we terminate early....
	alarm(0,_,_),
	fail.

complete_time_out :-
	alarm(0,_,_),
	yap_hacks:enable_interrupts.
