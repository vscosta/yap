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

table(X) :- var(X), !,
            write(user_error, '[ Error: argument to table/1 should be a predicate ]'),
            nl(user_error),
            fail.
table((A,B)) :- !, table(A), table(B).
table(A/N) :- integer(N), atom(A), !,
              functor(T,A,N), '$flags'(T,F,F),
              (
                X is F /\ 8'000100, X =\= 0, !,
                write(user_error, '[ Warning: '),
                write(user_error, A/N),
                write(user_error, ' is already declared as table ]'),
                nl(user_error)
              ;
                X is F /\ 8'170000, X =:= 0, !, '$table'(T)
              ;
                write(user_error, '[ Error: '),
                write(user_error, A/N),
                write(user_error, ' cannot be declared as table ]'),
                nl(user_error),
                fail
              ).
table(X) :- write(user_error, '[ Error: '),
            write(user_error, X),
            write(user_error, ' is an invalid argument to table/1 ]'),
            nl(user_error),
            fail.


show_trie(X) :- var(X), !,
                write(user_error, '[ Error: argument to trie/1 should be a predicate ]'),
                nl(user_error),
                fail.
show_trie(A/N) :- integer(N), atom(A), !,
                  functor(T,A,N), '$flags'(T,F,F),
                  (
                    X is F /\ 8'000100, X =\= 0, !, '$show_trie'(T,_)
                  ;
                    write(user_error, '[ Error: '),
                    write(user_error, A/N),
                    write(user_error, ' is not declared as table ]'),
                    nl(user_error),
                    fail
                  ).
show_trie(X) :- write(user_error, '[ Error: '),
                write(user_error, X),
                write(user_error, ' is an invalid argument to trie/1 ]'),
                nl(user_error),
                fail.


abolish_trie(X)   :- var(X), !,
                     write(user_error, '[ Error: argument to abolish_trie/1 should be a predicate ]'),
                     nl(user_error),
                     fail.
abolish_trie(A/N) :- integer(N), atom(A), !,
                     functor(T,A,N), '$flags'(T,F,F),
                     (
                       X is F /\ 8'000100, X =\= 0, !, '$abolish_trie'(T)
                     ;
                       write(user_error, '[ Error: '),
                       write(user_error, A/N),
                       write(user_error, ' is not declared as table ]'),
                       nl(user_error),
                       fail
                     ).
abolish_trie(X)   :- write(user_error, '[ Error: '),
                     write(user_error, X),
                     write(user_error, ' is an invalid argument to abolish_trie/1 ]'),
                     nl(user_error),
                     fail.
