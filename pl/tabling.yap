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

:- meta_predicate table(:), abolish_trie(:), show_trie(:), resume_trie(:).

table(M:X) :- !,
	'$table'(X, M).
table(X) :-
	'$current_module'(M),
	'$table'(X, M).

'$table'(X, _) :- var(X), !,
            write(user_error, '[ Error: argument to table/1 should be a predicate ]'),
            nl(user_error),
            fail.
'$table'(M:A, _) :- !, '$table'(A, M).
'$table'((A,B), M) :- !, '$table'(A, M), '$table'(B, M).
'$table'(A/N, M) :- integer(N), atom(A), !,
	functor(P, A, N),
	'$declare_tabled'(P, M).
'$table'(X, _) :- write(user_error, '[ Error: '),
        write(user_error, X),
        write(user_error, ' is an invalid argument to table/1 ]'),
        nl(user_error),
        fail.

'$declare_tabled'(P, M) :-
	'$undefined'(P, M), !,
	'$do_table'(P, M).
'$declare_tabled'(P, M) :-
	'$flags'(P,M,F,F),
	X is F /\ 0x1991F880, X =:= 0, !,
	'$do_table'(P, M).
'$declare_tabled'(P, M) :-
	functor(P, A, N),
	'$do_error'(permission_error(modify,static_procedure,A/N),tabled(Mod:A/N)).

abolish_trie(M:X)   :- !,
	'$abolish_trie'(X, M).
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
                       X is F /\ 0x000040, X =\= 0, !, '$do_abolish_trie'(T,M)
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

show_trie(M:X) :- !,
	'$show_trie'(X, M).
show_trie(X) :-
	'$current_module'(M),
	'$show_trie'(X, M).

'$show_trie'(X, M) :- var(X), !,
	'$do_error'(instantiation_error,show_trie(M:X)).
'$show_trie'((A,B), _) :- !, '$show_trie'(A, M), '$show_trie'(B, M).
'$show_trie'(M:A, _) :- !, '$show_trie'(A, M).
'$show_trie'(A/N, M) :- integer(N), atom(A), !,
                  functor(T,A,N), '$flags'(T,M,F,F),
                  (
                    X is F /\ 0x000040, X =\= 0, !, '$show_trie'(T,M,_)
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

resume_trie(M:X) :- !,
	'$resume_trie'(X, M).
resume_trie(X) :-
	'$current_module'(M),
	'$resume_trie'(X, M).


'$resume_trie'(X,_) :- var(X), !,
                    write(user_error, '[ Error: argument to trie/1 should be a predicate ]'),
                    nl(user_error),
                    fail.
'$resume_trie'(A/N,M) :- atom(A), integer(N), !,
                      functor(T,A,N), '$flags'(T,M,F,F),
                      (
                        X is F /\ 0x000040, X =\= 0, !, '$resume_trie'(T,M)
                      ;
                        write(user_error, '[ Error: '),
                        write(user_error, A/N),
                        write(user_error, ' is not declared as table ]'),
                        nl(user_error),
                        fail
                      ).
'$resume_trie'(X,M) :- write(user_error, '[ Error: '),
                    write(user_error, M:X),
                    write(user_error, ' is an invalid argument to trie/1 ]'),
                    nl(user_error),
                    fail.
