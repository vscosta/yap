%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Constraint Handling Rules			      version 2.2 %
%								  %
%  (c) Copyright 1996-98					  %
%  LMU, Muenchen						  %
%								  %
%  File:   sbag_a.pl						  %
%  Author: Christian Holzbaur		christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% bags of suspensions
%

:- module( sbag,
	[
	    iter_init/2,
	    iter_last/1,
	    iter_next/3,
	    list_to_sbag/2,
	    sbag_empty/1,
	    sbag_member/2,
	    sbag_union/3,
	    sbag_add_element/3,
	    sbag_del_element/3
	]).

:- use_module( library(assoc),
	[
	    assoc_to_list/2,
	    put_assoc/4,
	    del_assoc/4,
	    gen_assoc/3,
	    list_to_assoc/2,
	    ord_list_to_assoc/2
	]).

% -----------------------------------------------------------------
%
% *** MACROS ***
%
:- multifile
	user:goal_expansion/3.
:- dynamic
	user:goal_expansion/3.
%
user:goal_expansion( iter_init(A,B),	   _, sbag:down(A,[],B)).
user:goal_expansion( iter_last([]),	   _, true).
user:goal_expansion( iter_next([A|B],C,D), _, sbag:iter_next(A,B,D,C)).
user:goal_expansion( sbag_empty(A),        _, A==t).

% -----------------------------------------------------------------

iter_init( Assoc, Contin) :-
	down( Assoc, [], Contin).

iter_last( []).

sbag_empty( B) :- B == t.

%
% fails for empty
%
iter_next( [Node|Cont], Elem, Next) :-
	iter_next( Node, Cont, Next, Elem).

iter_next( t(K,_,_,_,R), Cont, Next, K) :-	% cf. assoc.pl
	down( R, Cont, Next).
/*
	( R==t,L==t -> Next=Cont
	; R==t ->      Next=Cont % Next=[t(K,V,0,t,t)|Cont]
	;	       down( R, Cont, Next)
	).
*/

down( t,    Ci, Ci).
down( Node, Ci, Co) :-
	Node = t(_,_,_,L,_),
	down( L, [Node|Ci], Co).

list_to_sbag( L, A) :-
	list_to_sbag( L, t, A).

list_to_sbag([],     Assoc, Assoc).
list_to_sbag([X|Xs], Assoc0, Assoc) :-
	put_assoc( X, Assoc0, 0, Assoc1),
	list_to_sbag( Xs, Assoc1, Assoc).

sbag_member( Elem, A) :-
	gen_assoc( Elem, A, _).

sbag_union( A, B, C) :-
	assoc_to_list( A, As),
	assoc_to_list( B, Bs),
	prolog:merge( As, Bs, Cs),
	ord_list_to_assoc( Cs, C).

sbag_add_element( S1, E, S2) :- put_assoc( E, S1, 0, S2).

sbag_del_element( S1, E, S2) :- del_assoc( E, S1, _, S2).


end_of_file.
% -----------------------------------------------------------------

test( N) :-
	length( L, N),
	list_to_assoc( L, A),
	down( A, [], C),
	a2l( C, L1),
	assoc_to_list( A, L2),
	( L1==L1 -> true ; raise_exception( mismatch(L1,L2))).

bug(L) :-
  A = t(c,0,0,t(b,0,-1,t(a,0,0,t,t),t),
	      t(e,0,0,t(d,0,0,t,t),
		      t(f,0,0,t,t))),
  iter_init( A, S),
  a2l( S, L).

a2l( S, []) :- iter_last( S).
a2l( S, [X|Xs]) :-
	iter_next( S, X, N),
	a2l( N, Xs).
