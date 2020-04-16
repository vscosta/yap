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

/**
 * @file   timeout.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Wed Nov 18 01:26:14 2015
 * 
 * @brief  Calls With Timeout
 * 
 * 
*/


:- module(timeout, [
	time_out/3
    ]).


/** @defgroup timeout Calls With Timeout
@ingroup library
@{

The <tt>time_out/3</tt> command relies on the <tt>alarm/3</tt> built-in to
implement a call with a maximum time of execution. The command is
available with the `use_module(library(timeout))` command.

  */

/*
  
 @pred time_out(+ _Goal_, + _Timeout_, - _Result_) 


Execute goal  _Goal_ with time limited  _Timeout_, where
 _Timeout_ is measured in milliseconds. If the goal succeeds, unify
 _Result_ with success. If the timer expires before the goal
terminates, unify  _Result_ with <tt>time_out</tt>.

This command is implemented by activating an alarm at procedure
entry. If the timer expires before the goal completes, the alarm will
throw an exception  _timeout_.

One should note that time_out/3 is not reentrant, that is, a goal
called from `time_out` should never itself call
time_out/3. Moreover, time_out/3 will deactivate any previous
alarms set by alarm/3 and vice-versa, hence only one of these
calls should be used in a program.

Last, even though the timer is set in milliseconds, the current
implementation relies on <tt>alarm/3</tt>, and therefore can only offer
precision on the scale of seconds.

 */


:- meta_predicate time_out(0,+,-).

:- use_module(library(hacks), [
	virtual_alarm/3,
	alarm/3
    ]).

%
% not the nicest program I've ever seen.
%

time_out(Goal, Time, Result) :-
	T is integer(Time/1000),
	UT is integer(Time*1000) mod 10000000,
	yap_hacks:alarm([T|UT],throw(error(time_out(Goal))),_),
	gated_call(
		true,
			Goal,
            Port,			
			exit_time_out(Port, Result)
			),
			!.

exit_time_out(exception(time_out), _) :-
	!.
exit_time_out(Port, Result) :-
	alarm(0,_,_),
	time_out_rc(Port, Result).

time_out_rc(exit, success).
time_out_rc(answer, success).
time_out_rc(fail, failure).
time_out_rc(exception(_), failure).
time_out_rc(external_exception(_), failure).
time_out_rc(redo, failure).
time_out_rc(!, success).

%% @}

