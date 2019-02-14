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
* File:		ground.pl						 *
* Last rev:								 *
* mods:									 *
* comments:	Variables and ground					 *
*									 *
*************************************************************************/

/**
  * @file   ground.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 12:01:27 2017
  *
  * @brief  term operations
  *
  */

/**
  * @addtogroup YAPTypes
  * @{
  *
  *
*/
/*
% grounds all free variables
% as terms of the form '$VAR'(N)
_numbervars(Term, M, N) :-
	'$variables_in_term'(Term, [], L),
	'$numbermarked_vars'(L, M, N).

'$numbermarked_vars'([], M, M).
'$numbermarked_vars'([V|L], M, N) :-
	attvar(V), !,
	'$numbermarked_vars'(L, M, N).
'$numbermarked_vars'(['$VAR'(M)|L], M, N) :-
	M1 is M+1,
	'$numbermarked_vars'(L, M1, N).

*/

%% @}
