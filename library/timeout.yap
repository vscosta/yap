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

time_out(Goal, Time, Result) :-
	T is Time//1000,
	UT is (Time mod 1000)*1000,
	catch( ( Result0 = success,
	         setup_call_cleanup(
			alarm(T.UT,throw(time_out),_),
			Goal,
			alarm(0,_,RT)),
		 (  var(RT)
		 -> alarm(0,_,_),
		    ( true ; alarm(T.UT,throw(time_out),_), fail )
		 ;  true
		 )
	       ),
	       time_out,
	       Result0 = time_out ),
	Result = Result0.
