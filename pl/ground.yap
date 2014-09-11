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

/*
% grounds all free variables
% as terms of the form '$VAR'(N)

numbervars('$VAR'(M), M, N) :- !,
	succ(M, N).
numbervars(Atomic, M, M) :-
	atomic(Atomic), !.
numbervars(Term, M, N) :-
	functor(Term, _, Arity),
	'$numbervars'(0,Arity, Term, M, N).

'$numbervars'(A, A, _, N, N) :- !.
'$numbervars'(A,Arity, Term, M, N) :-
	'$succ'(A,An),
	arg(An, Term, Arg),
	numbervars(Arg, M, K), !,
	'$numbervars'(An, Arity, Term, K, N).


ground(Term) :-
	nonvar(Term),		%  This term is not a variable,
	functor(Term, _, Arity),
	'$ground'(Arity, Term).	%  and none of its arguments are.

'$ground'(0, _) :- !.
'$ground'(N, Term) :-
	'$predc'(N,M),
	arg(N, Term, ArgN),
	ground(ArgN),
	'$ground'(M, Term).

numbervars(Term, M, N) :-
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

