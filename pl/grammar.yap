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
* File:		grammar.pl						 *
* Last rev:								 *
* mods:									 *
* comments:	BNF grammar for Prolog					 *
*									 *
*************************************************************************/

/*
    Variables X in grammar rule bodies are translated as
    if phrase(X) had been written, where phrase/3 is obvious.
    Also, phrase/2-3 check their first argument.
*/

'$translate_rule'((LP-->RP), H) :-
	RP == [],
	!,
	'$t_head'(LP, S, S, H).
'$translate_rule'((LP-->RP), (H:-B)):-
	'$t_head'(LP, S, SR, H),
	'$t_body'(RP, _, last, S, SR, B1),
	'$t_tidy'(B1, B).


'$t_head'((LP,List), S, SR, H):- !,
	'$append'(List, SR, List2),
	'$extend'([S,List2], LP, H).
'$t_head'(M:LP, S, SR, M:H) :- !,
	'$extend'([S,SR], LP, H).
'$t_head'(LP, S, SR, H) :-
	'$extend'([S,SR], LP, H).
	

%
% Two extra variables:
% ToFill tells whether we need to explictly close the chain of
% variables.
% Last tells whether we are the ones who should close that chain.
%
'$t_body'(Var, filled_in, _, S, S1, phrase(Var,S,S1)) :-
	var(Var),
	!.
'$t_body'(!, to_fill, last, S, S1, (!, S1 = S)) :- !.
'$t_body'(!, _, _, S, S, !) :- !.
'$t_body'([], to_fill, last, S, S1, S1=S) :- !.
'$t_body'([], _, _, S, S, true) :- !.
'$t_body'([X], filled_in, _, S, SR, 'C'(S,X,SR)) :- !.
'$t_body'([X|R], filled_in, Last, S, SR, ('C'(S,X,SR1),RB)) :- !,
	'$t_body'(R, filled_in, Last, SR1, SR, RB).
'$t_body'({T}, to_fill, last, S, S1, (T, S1=S)) :- !.
'$t_body'({T}, _, _, S, S, T) :- !.
'$t_body'((T,R), ToFill, Last, S, SR, (Tt,Rt)) :- !,
	'$t_body'(T, ToFill, not_last, S, SR1, Tt),
	'$t_body'(R, ToFill, Last, SR1, SR, Rt).
'$t_body'((T->R), ToFill, Last, S, SR, (Tt->Rt)) :- !,
	'$t_body'(T, ToFill, not_last, S, SR1, Tt),
	'$t_body'(R, ToFill, Last, SR1, SR, Rt).
'$t_body'((T;R), ToFill, _Last, S, SR, (Tt;Rt)) :- !,
	copy_term(ToFill,OtherToFill),
	'$t_body'(T, OtherToFill, last, S, SR, Tt),
	'$t_body'(R, ToFill, last, S, SR, Rt).
'$t_body'(M:G, ToFill, Last, S, SR, M:NG) :- !,
	'$t_body'(G, ToFill, Last, S, SR, NG).
'$t_body'(T, filled_in, _, S, SR, Tt) :-
	'$extend'([S,SR], T, Tt).



'$extend'(More, OldT, NewT) :-
	OldT =.. OldL,
	'$append'(OldL, More, NewL),
	NewT =.. NewL.


'$t_tidy'(P,P) :- var(P), !.
'$t_tidy'((P1;P2), (Q1;Q2)) :- !,
	'$t_tidy'(P1, Q1),
	'$t_tidy'(P2, Q2).
'$t_tidy'((P1->P2), (Q1->Q2)) :- !,
	'$t_tidy'(P1, Q1),
	'$t_tidy'(P2, Q2).
'$t_tidy'(((P1,P2),P3), Q) :-
	'$t_tidy'((P1,(P2,P3)), Q).
'$t_tidy'((true,P1), Q1) :- !,
	'$t_tidy'(P1, Q1).
'$t_tidy'((P1,true), Q1) :- !,
	'$t_tidy'(P1, Q1).
'$t_tidy'((P1,P2), (Q1,Q2)) :- !,
	'$t_tidy'(P1, Q1),
	'$t_tidy'(P2, Q2).
'$t_tidy'(A, A).


'C'([X|S],X,S).


phrase(PhraseDef, WordList) :-
	phrase(PhraseDef, WordList, []).


phrase(P, S0, S) :-
	var(P), !,
	throw(error(instantiation_error,phrase(P,S0,S))).
phrase(P, S0, S) :-
	( primitive(P), \+ atom(P) ),  !,
	throw(error(type_error(callable,P),phrase(P,S0,S))).
phrase([], S0, S) :- !,
	S0 = S.
phrase([H|T], S0, S) :- !,
	'$append'([H|T], S, S0).
phrase(Phrase, S0, S) :-
	'$t_body'(Phrase, _, last, S0, S, Goal), !,
	'$execute'(Goal).

