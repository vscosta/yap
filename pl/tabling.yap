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
* File:		tabling.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	support tabling predicates				 *
*									 *
*************************************************************************/

:- meta_predicate table(:), tabling_mode(:), abolish_trie(:), show_trie(:), show_trie_stats(:).



table(P) :- '$current_module'(M), '$table'(P,M).

'$table'(P,M) :- var(P), !, '$do_error'(instantiation_error,table(M:P)).
'$table'(M:P,_) :- !, '$table'(P,M).
'$table'([],_) :- !.
'$table'([H|T],M) :- !, '$table'(H,M), '$table'(T,M).
'$table'((P1,P2),M) :- !, '$table'(P1,M), '$table'(P2,M).
'$table'(A/N,M) :- integer(N), atom(A), !, functor(T,A,N), '$declare_tabled'(T,M).
'$table'(P,M) :- '$do_error'(type_error(callable,P),table(M:P)).

'$declare_tabled'(T,M) :- '$undefined'(T,M), !, '$do_table'(T,M).
'$declare_tabled'(T,M) :- '$flags'(T,M,F,F), F /\ 0x1991F880 =:= 0, !, '$do_table'(T,M).
'$declare_tabled'(T,M) :- functor(T,A,N), '$do_error'(permission_error(modify,table,M:A/N),table(M:A/N)).



tabling_mode(P,S) :- '$current_module'(M), '$tabling_mode'(P,M,S).

'$tabling_mode'(P,M,S) :- var(P), !, '$do_error'(instantiation_error,tabling_mode(M:P,S)).
'$tabling_mode'(M:P,_,S) :- !, '$tabling_mode'(P,M,S).
'$tabling_mode'([],_,_) :- !.
'$tabling_mode'([H|T],M,S) :- !, '$tabling_mode'(H,M,S), '$tabling_mode'(T,M,S).
'$tabling_mode'((P1,P2),M,S) :- !, '$tabling_mode'(P1,M,S), '$tabling_mode'(P2,M,S).
'$tabling_mode'(A/N,M,S) :- integer(N), atom(A), !, functor(T,A,N), '$flags'(T,M,F,F),
	(F /\ 0x000040 =\= 0, !, '$set_tabling_mode'(T,M,S)
	;
	'$do_error'(domain_error(table,M:A/N),tabling_mode(M:A/N,S))).
'$tabling_mode'(P,M,S) :- '$do_error'(type_error(callable,P),tabling_mode(M:P,S)).

'$set_tabling_mode'(T,M,S) :- var(S), !, '$do_tabling_mode'(T,M,S).
'$set_tabling_mode'(T,M,S) :- (S = local ; S = batched), !, '$do_tabling_mode'(T,M,S).
'$set_tabling_mode'(T,M,S) :- functor(T,A,N), '$do_error'(domain_error(flag_value,tabling_mode+S),tabling_mode(M:A/N,S)).



abolish_trie(P) :- '$current_module'(M), '$abolish_trie'(P,M).

'$abolish_trie'(P,M) :- var(P), !, '$do_error'(instantiation_error,abolish_trie(M:P)).
'$abolish_trie'(M:P,_) :- !, '$abolish_trie'(P,M).
'$abolish_trie'([],_) :- !.
'$abolish_trie'([H|T],M) :- !, '$abolish_trie'(H,M), '$abolish_trie'(T,M).
'$abolish_trie'((P1,P2),M) :- !, '$abolish_trie'(P1,M), '$abolish_trie'(P2,M).
'$abolish_trie'(A/N,M) :- integer(N), atom(A), !, functor(T,A,N), '$flags'(T,M,F,F),
	(F /\ 0x000040 =\= 0, !, '$do_abolish_trie'(T,M)
	;
	'$do_error'(domain_error(table,M:A/N),abolish_trie(M:A/N))).
'$abolish_trie'(P,M) :- '$do_error'(type_error(callable,P),abolish_trie(M:P)).



show_trie(P) :-	'$current_module'(M), '$show_trie'(P,M).

'$show_trie'(P,M) :- var(P), !, '$do_error'(instantiation_error,show_trie(M:P)).
'$show_trie'(M:P,_) :- !, '$show_trie'(P,M).
'$show_trie'([],_) :- !.
'$show_trie'([H|T],M) :- !, '$show_trie'(H,M), '$show_trie'(T,M).
'$show_trie'((P1,P2),M) :- !, '$show_trie'(P1,M), '$show_trie'(P2,M).
'$show_trie'(A/N,M) :- integer(N), atom(A), !,	functor(T,A,N), '$flags'(T,M,F,F),
	(F /\ 0x000040 =\= 0, !, '$do_show_trie'(T,M)
	;
	'$do_error'(domain_error(table,M:A/N),show_trie(M:A/N))).
'$show_trie'(P,M) :- '$do_error'(type_error(callable,P),show_trie(M:P)).



show_trie_stats(P) :- '$current_module'(M), '$show_trie_stats'(P,M).

'$show_trie_stats'(P,M) :- var(P), !, '$do_error'(instantiation_error,show_trie_stats(M:P)).
'$show_trie_stats'(M:P,_) :- !, '$show_trie_stats'(P,M).
'$show_trie_stats'([],_) :- !.
'$show_trie_stats'([H|T],M) :- !, '$show_trie_stats'(H,M), '$show_trie_stats'(T,M).
'$show_trie_stats'((P1,P2),M) :- !, '$show_trie_stats'(P1,M), '$show_trie_stats'(P2,M).
'$show_trie_stats'(A/N,M) :- atom(A), integer(N), !, functor(T,A,N), '$flags'(T,M,F,F),
	(F /\ 0x000040 =\= 0, !, '$do_show_trie_stats'(T,M)
	;
	'$do_error'(domain_error(table,M:A/N),show_trie_stats(M:A/N))).
'$show_trie_stats'(P,M) :- '$do_error'(type_error(callable,P),show_trie_stats(M:P)).
