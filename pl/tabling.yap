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

:- meta_predicate table(:), abolish_trie(:), show_trie(:), show_trie_stats(:).

table(M:P) :- !, '$table'(P,M).
table(P)   :- '$current_module'(M), '$table'(P,M).

'$table'(P,M) :- var(P), !, '$do_error'(instantiation_error,table).
'$table'((P1,P2),M) :- !, '$table'(P1,M), '$table'(P2,M).
'$table'(P/N,M) :- integer(N), atom(P), !,
	functor(T,P,N), '$declare_tabled'(T,M).
'$table'(P,M) :- '$do_error'(type_error(callable,P),table).

'$declare_tabled'(T,M) :- '$undefined'(T,M), !, '$do_table'(T,M).
'$declare_tabled'(T,M) :- '$flags'(T,M,F,F), 
	X is F /\ 0x1991F880, X =:= 0, !, '$do_table'(T,M).
'$declare_tabled'(T,M) :- functor(T,A,N),
	'$do_error'(permission_error(modify,static_procedure,A/N),tabled(M:A/N)).

abolish_trie(M:P) :- !,	'$abolish_trie'(P,M).
abolish_trie(P) :- '$current_module'(M), '$abolish_trie'(P,M).

'$abolish_trie'(P,M) :- var(P), !, '$do_error'(instantiation_error,abolish_trie).
'$abolish_trie'((P1,P2),M) :- !, '$abolish_trie'(P1,M), '$abolish_trie'(P2,M).
'$abolish_trie'(P/N,M) :- integer(N), atom(P), !,
	functor(T,P,N), '$flags'(T,M,F,F),
	(
	    X is F /\ 0x000040, X =\= 0, !, '$do_abolish_trie'(T,M)
	    ;
	    write(user_error, '[ PERMISSION ERROR- '),
	    write(user_error, M:P/N),
	    write(user_error, ' is not tabled ]'),
	    nl(user_error), fail
	).
'$abolish_trie'(P,_) :- '$do_error'(type_error(callable,P),abolish_trie).

show_trie(M:P) :- !, '$show_trie'(P,M).
show_trie(P) :-	'$current_module'(M), '$show_trie'(P,M).

'$show_trie'(P,M) :- var(P), !, '$do_error'(instantiation_error,show_trie).
'$show_trie'((P1,P2),M) :- !, '$show_trie'(P1,M), '$show_trie'(P2,M).
'$show_trie'(P/N, M) :- integer(N), atom(P), !,
	functor(T,P,N), '$flags'(T,M,F,F),
	(
	    X is F /\ 0x000040, X =\= 0, !, '$do_show_trie'(T,M)
	    ;
	    write(user_error, '[ PERMISSION ERROR- '),
	    write(user_error, M:P/N),
	    write(user_error, ' is not tabled ]'),
	    nl(user_error), fail
	).
'$show_trie'(P,_) :- '$do_error'(type_error(callable,P),show_trie).

show_trie_stats(M:P) :- !,'$show_trie_stats'(P,M).
show_trie_stats(P) :- '$current_module'(M),	'$show_trie_stats'(P,M).

'$show_trie_stats'(P,M) :- var(P), !, '$do_error'(instantiation_error,show_trie_stats).
'$show_trie_stats'((P1,P2),M) :- !, '$show_trie_stats'(P1,M), '$show_trie_stats'(P2,M).
'$show_trie_stats'(P/N,M) :- atom(P), integer(N), !,
	functor(T,P,N), '$flags'(T,M,F,F),
	(
	    X is F /\ 0x000040, X =\= 0, !, '$do_show_trie_stats'(T,M)
	    ;
	    write(user_error, '[ PERMISSION ERROR- '),
	    write(user_error, M:P/N),
	    write(user_error, ' is not tabled ]'),
	    nl(user_error), fail
	).
'$show_trie_stats'(P,_) :- '$do_error'(type_error(callable,P),show_trie_stats).

