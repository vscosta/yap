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


/**
 *    @file callcount.yap
 *   @short support call counting.
 *   
 *   @defgroup Profiling Profiling Prolog Programs
 *   @brief the clock and the tick profilers.
 *   @ingroup extensions
 *   @{
 *
 * YAP includes two profilers. The count profiler keeps information on the
 * number of times a predicate was called. This information can be used to
 * detect what are the most commonly called predicates in the program.  The
 * count profiler can be compiled by setting YAP's flag profiling
 * to `on`. The time-profiler is a `gprof` profiler, and counts
 * how many ticks are being spent on specific predicates, or on other
 * system functions such as internal data-base accesses or garbage collects.
 * 
 *     + Call_Counting
 *     +
 * 
 */

/**
@}
*/


/**
 *  @defgroup Call_Counting Counting Calls
 *  @ingroup Profiling
 *  @{ 
 *  
 * Predicates compiled with YAP's flag call_counting set to
 * `on` update counters on the numbers of calls and of
 * retries. Counters are actually decreasing counters, so that they can be
 * used as timers.  Three counters are available:
 * 
 * + `calls`: number of predicate calls since execution started or since
 * system was reset; 
 * + `retries`: number of retries for predicates called since
 * execution started or since counters were reset;
 * + `calls_and_retries`: count both on predicate calls and
 * retries.
 * 
 * These counters can be used to find out how many calls a certain
 * goal takes to execute. They can also be used as timers.
 * 
 * The code for the call counters piggybacks on the profiling
 * code. Therefore, activating the call counters also activates the profiling
 * counters.
 * 
 * These are  the predicates that access and manipulate the call counters. 
 * */

:- system_module( '$_callcount', [call_count/3,
        call_count_data/3,
        call_count_reset/0], []).

:- use_system_module( '$_errors', ['$do_error'/2]).


/** @pred  call_count_data(- _Calls_, - _Retries_, - _CallsAndRetries_) 
 * 
 * 
 * Give current call count data. The first argument gives the current value
 * for the  _Calls_ counter, next the  _Retries_ counter, and last
 * the  _CallsAndRetries_ counter.
 *  
 * */
call_count_data(Calls, Retries, Both) :-
	'$call_count_info'(Calls, Retries, Both).

/** @pred  call_count_reset 
 * 
 * 
 * Reset call count counters. All timers are also reset.
 * 
 */
call_count_reset :-
	'$call_count_reset'.

/** @pred  call_count(? _CallsMax_, ? _RetriesMax_, ? _CallsAndRetriesMax_) 
 * 
 * 
 * Set call counters as timers. YAP will generate an exception
 * if one of the instantiated call counters decreases to 0:
 * 
 * + _CallsMax_
 * 
 *     throw the exception `call_counter` when the
 * counter `calls` reaches 0;
 * 
 * + _RetriesMax_
 * 
 *     throw the exception `retry_counter` when the
 * counter `retries` reaches 0;
 * 
 * + _CallsAndRetriesMax_
 * 
 *     throw the exception
 * `call_and_retry_counter` when the counter `calls_and_retries`
 * reaches 0.
 * 
 *  YAP will ignore counters that are called with unbound arguments.
 * 
 * Next, we show a simple example of how to use call counters:
 * 
 * ~~~~~
 *    
 * ?- yap_flag(call_counting,on),
 *       [-user]. 
 * l :- l. 
 * end_of_file. 
 *
 * yap_flag(call_counting,off).
 * 
 * yes
 * 
 * ?- catch(
 *      (call_count(10000,_,_),l),
 *       call_counter,format("limit_exceeded.~n",[])).
 * 
 * limit_exceeded.
 * 
 * yes
 * ~~~~~
 * Notice that we first compile the looping predicate `l/0` with
 * call_counting `on`. Next, we catch/3 to handle an
 * exception when `l/0` performs more than 10000 reductions.
 * 
 * 
 */
call_count(Calls, Retries, Both) :-
	'$check_if_call_count_on'(Calls, CallsOn),
	'$check_if_call_count_on'(Retries, RetriesOn),
	'$check_if_call_count_on'(Both, BothOn),
	'$call_count_set'(Calls, CallsOn, Retries, RetriesOn, Both, BothOn).

'$check_if_call_count_on'(Calls, 1) :- integer(Calls), !.
'$check_if_call_count_on'(Calls, 0) :- var(Calls), !.
'$check_if_call_count_on'(Calls, A) :-
	'$do_error'(type_error(integer,Calls),call_count(A)).

%% @}

