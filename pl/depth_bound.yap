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
* File:		corout.pl						 *
* Last rev:								 *
* mods:									 *
* comments:	Coroutines implementation				 *
*									 *
*************************************************************************/

%depth_bound_call(A,D) :-
%write(depth_bound_call(A,D)), nl, fail.
depth_bound_call(A,D) :-
	'$execute_under_depth_limit'(A,D).

