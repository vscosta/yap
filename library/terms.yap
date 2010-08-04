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
* File:		terms.yap						 *
* Last rev:	5/12/99							 *
* mods:									 *
* comments:	Term manipulation operations				 *
*									 *
*************************************************************************/

:- module(terms, [
		  term_hash/2,
		  term_hash/4,
		  instantiated_term_hash/4,
		  variant/2,
		  unifiable/3,
		  subsumes/2,
		  subsumes_chk/2,
		  cyclic_term/1,
		  variable_in_term/2,
		  variables_within_term/3,
		  new_variables_in_term/3
		 ]).

term_hash(T,H) :-
	term_hash(T, -1, 33554432, H).

%term_hash(X,Y) :-
%	term_hash(X,-1,16'1000000,Y).

subsumes_chk(X,Y) :-
	\+ \+ subsumes(X,Y).

unifiable(X,Y,Z) :-
	protected_unifiable(X,Y,Z), !.
unifiable(_,_,_) :- fail.



