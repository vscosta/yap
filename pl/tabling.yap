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

table(X) :-
	current_module(M),
	'$table'(X, M).

'$table'(X, _) :- var(X), !,
            write(user_error, '[ Error: argument to table/1 should be a predicate ]'),
            nl(user_error),
            fail.
'$table'(M:A, _) :- !, '$table'(A, M).
'$table'((A,B), M) :- !, '$table'(A, M), '$table'(B, M).
'$table'(A/N, M) :- integer(N), atom(A), !,
              functor(T,A,N), '$flags'(T,M,F,F),
              (
                X is F /\ 8'000100, X =\= 0, !,
                write(user_error, '[ Warning: '),
                write(user_error, M:A/N),
                write(user_error, ' is already declared as table ]'),
                nl(user_error)
              ;
                X is F /\ 8'170000, X =:= 0, !, '$do_table'(T, M)
              ;
                write(user_error, '[ Error: '),
                write(user_error, M:A/N),
                write(user_error, ' cannot be declared as table ]'),
                nl(user_error),
                fail
              ).
'$table'(X, _) :- write(user_error, '[ Error: '),
            write(user_error, X),
            write(user_error, ' is an invalid argument to table/1 ]'),
            nl(user_error),
            fail.

show_trie(X) :-
	'$current_module'(M),
	'$show_trie'(X, M).

'$show_trie'(X, M) :- var(X), !,
	throw(error(instantiation_error,show_trie(M:X))).
'$show_trie'((A,B), _) :- !, '$show_trie'(A, M), '$show_trie'(B, M).
'$show_trie'(M:A, _) :- !, '$show_trie'(A, M).
'$show_trie'(A/N, M) :- integer(N), atom(A), !,
                  functor(T,A,N), '$flags'(T,M,F,F),
                  (
                    X is F /\ 8'000100, X =\= 0, !, '$show_trie'(T,M,_)
                  ;
                    write(user_error, '[ Error: '),
                    write(user_error, M:A/N),
                    write(user_error, ' is not declared as table ]'),
                    nl(user_error),
                    fail
                  ).
'$show_trie'(X, M) :- write(user_error, '[ Error: '),
                write(user_error, M:X),
                write(user_error, ' is an invalid argument to trie/1 ]'),
                nl(user_error),
                fail.

abolish_trie(X)   :-
	'$current_module'(M),
	'$abolish_trie'(X, M).

'$abolish_trie'(X, _M)   :- var(X), !,
                     write(user_error, '[ Error: argument to abolish_trie/1 should be a predicate ]'),
                     nl(user_error),
                     fail.
'$abolish_trie'((A,B), _) :- !, '$abolish_trie'(A, M), '$abolish_trie'(B, M).
'$abolish_trie'(M:A, _) :- !, '$abolish_trie'(A, M).
'$abolish_trie'(A/N, M) :- integer(N), atom(A), !,
                     functor(T,A,N), '$flags'(T,M,F,F),
                     (
                       X is F /\ 8'000100, X =\= 0, !, '$do_abolish_trie'(T,M)
                     ;
                       write(user_error, '[ Error: '),
                       write(user_error, M:A/N),
                       write(user_error, ' is not declared as table ]'),
                       nl(user_error),
                       fail
                     ).
'$abolish_trie'(X,M)   :- write(user_error, '[ Error: '),
                     write(user_error, M:X),
                     write(user_error, ' is an invalid argument to abolish_trie/1 ]'),
                     nl(user_error),
                     fail.
