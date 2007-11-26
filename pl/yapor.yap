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
* File:		yapor.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	support or-parallelism					 *
*									 *
*************************************************************************/

'$parallel_query'(G,[]) :- !, '$start_yapor', '$execute'(G), !,
'$parallel_yes_answer'.
'$parallel_query'(G,V)  :- '$start_yapor', '$execute'(G), '$parallel_new_answer'(V).

% ***************************
% * -------- YAPOR -------- *
% ***************************

default_sequential(X) :-
	'$default_sequential'(X), !.
default_sequential(_).

'$sequential' :-
	'$default_sequential'(X),
	'$initialization'('$default_sequential'(X)),
	'$default_sequential'(on).

'$parallel' :-
	'$default_sequential'(X),
	'$initialization'('$default_sequential'(X)),
	'$default_sequential'(off).

'$sequential_directive'(X,_) :- var(X), !,
                 write(user_error, '[ Error: argument to sequential/1 should be a predicate ]'),
                 nl(user_error),
                 fail.
'$sequential_directive'((A,B),M) :- !,
	'$sequential_directive'(A,M), '$sequential_directive'(B,M).
'$sequential_directive'(M:A,_) :- !,
	'$sequential_directive'(A,M).
'$sequential_directive'(A/N,M) :- integer(N), atom(A), !,
                   functor(T,A,N),
	           '$flags'(T,M,F,F),
                   (
                     X is F /\ 0x00000020, X =\= 0, !,
                     write(user_error, '[ Warning: '),
                     write(user_error, M:A/N),
                     write(user_error, ' is already declared as sequential ]'),
                     nl(user_error)
                   ;
                     X is F /\ 0x1991F880, X =:= 0, !, '$sequential'(T,M)
                   ;
                     write(user_error, '[ Error: '),
                     write(user_error, M:A/N),
                     write(user_error, ' cannot be declared as sequential ]'),
                     nl(user_error),
                     fail
                   ).
'$sequential_directive'(X,_) :- write(user_error, '[ Error: '),
                 write(user_error, X),
                 write(user_error, ' is an invalid argument to sequential/1 ]'),
                 nl(user_error),
                 fail.

'$parallel_directive'(X,M) :- var(X), !,
	'$do_error'(instantiation_error,parallel(M:X)).
'$parallel_directive'((A,B),M) :- !,
	'$parallel_directive'(A,M),
	'parallel_directive'(B,M).
'$parallel_directive'(M:A,_) :- !,
	'$parallel_directive'(A,M).
'$parallel_directive'(A/N,M) :- integer(N), atom(A), !,
                   functor(T,A,N), '$flags'(T,M,F,F),
                   (
                     NF is F /\ 0x00000020, '$flags'(T,F,NF) ;
                     write(user_error, '[ Warning: '),
                     write(user_error, M:A/N),
                     write(user_error, ' is already declared as sequential ]'),
                     nl(user_error)
                   ;
                     X is F /\ 0x1991FC80, X =:= 0, !, '$sequential'(T)
                   ;
                     write(user_error, '[ Error: '),
                     write(user_error, M:A/N),
                     write(user_error, ' cannot be declared as parallel ]'),
                     nl(user_error),
                     fail
                   ).
'$parallel_directive'(X,_) :- write(user_error, '[ Error: '),
                 write(user_error, X),
                 write(user_error, ' is an invalid argument to parallel/1 ]'),
                 nl(user_error),
                 fail.


%
% do not try to run consult in the parallel system.
%
'$parallelizable'(_) :-
	nb_getval('$consulting_file',S), S\=[], !, fail.
'$parallelizable'((G1,G2)) :- !,
	'$parallelizable'(G1),
	'$parallelizable'(G2).
'$parallelizable'((G1;G2)) :- !,
	'$parallelizable'(G1),
	'$parallelizable'(G2).
'$parallelizable'((G1|G2)) :- !,
	'$parallelizable'(G1),
	'$parallelizable'(G2).
'$parallelizable'((G1->G2)) :- !,
	'$parallelizable'(G1),
	'$parallelizable'(G2).
'$parallelizable'([]) :- !, fail.
'$parallelizable'([_|_]) :- !, fail.
'$parallelizable'(consult(_)) :- !, fail.
'$parallelizable'(reconsult(_)) :- !, fail.
'$parallelizable'(compile(_)) :- !, fail.
'$parallelizable'(use_module(_)) :- !, fail.
'$parallelizable'(_).



