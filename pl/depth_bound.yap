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

/**

@defgroup DepthLimited Depth Limited Search
@{
@ingroup extensions

YAP implements various extensions to the default Prolog search. One of
the most iseful s restricting the maximum search depth.

*/
 :-
system_module( '$_depth_bound', [depth_bound_call/2], []).

%% @pred depth_bound_call(A,D)
%
% CallError _A_ with maximum stack_el depth_reduction _B_.
%
depth_bound_call(A,D) :-
	'$execute_under_depth_limit'(A,D).


%% @}
