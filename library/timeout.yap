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

:- meta_predicate time_out(0,+,-).

:- use_module(library(hacks), [
	virtual_alarm/3
    ]).


%
% not the nicest program I've ever seen.
%

time_out(Goal, Time, Result) :-
	T is Time//1000,
	UT is (Time mod 1000)*1000,
	catch( ( Result0 = success,
	         setup_call_cleanup(
			virtual_alarm(T.UT,throw(time_out),_),
			Goal,
			virtual_alarm(0,_,RT)),
		 (  var(RT)
		 -> virtual_alarm(0,_,_),
		    (
		      true
		    ;
		      virtual_alarm(T.UT,throw(time_out),_),
		      fail
		    )
		 ;  true
		 )
	       ),
	       time_out,
	       Result0 = time_out ),
	Result = Result0.
