%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Constraint Handling Rules			      version 2.2 %
%								  %
%  (c) Copyright 1996-98					  %
%  LMU, Muenchen						  %
%								  %
%  File:   matching.pl						  %
%  Author: Christian Holzbaur		christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Head matching for SICStus
% ch, Aug. 95
%

:- module( matching, []).

:- op( 1200, xfx, ?-).

:- use_module( library(assoc),
	[
	    empty_assoc/1,
	    get_assoc/3,
	    put_assoc/4
	]).

:- multifile
	user:term_expansion/2,
	user:goal_expansion/3.

:- dynamic
	user:term_expansion/2,
	user:goal_expansion/3.
%
user:term_expansion( ?-(M:H0,B), (M:H1 :- Body)) :- !,
	functor( H0, N, A),
	functor( H1, N, A),
	subs( H0, H1, Code, [B]),
	l2conj( Code, Body).
user:term_expansion( ?-(H0,B), (H1 :- Body)) :-
	functor( H0, N, A),
	functor( H1, N, A),
	subs( H0, H1, Code, [B]),
	l2conj( Code, Body).

%
user:goal_expansion( inline_matching(Pattern,Datum), _, Exp) :-
	code( Pattern, Datum, Exp).

code( Pattern, Datum, Code) :-
	subs( Pattern, Datum, L, []),
	l2conj( L, Code).

%
% partial evaluation of subsumes( H0, H1)
%
subs( Pattern, Datum, L, Lt) :-
	empty_assoc( Dict),
	subs( Pattern, Datum, Dict,_, L, Lt).

subs( Pattern, Datum, D0,D1) --> {var(Pattern)}, !,
	{var(Datum)},
	( {get_assoc( Pattern, D0, _),D0=D1} -> % subsequent occ
	     [ Pattern == Datum ]
	;					% first occ
	     {
		 Pattern = Datum,
		 put_assoc( Pattern, D0, _, D1)
	     }
	).
subs( Pattern, Datum, D0,D1) --> {var(Datum)}, !,
	{
	    functor( Pattern, N, A),
	    functor( Skel, N, A)
	},
	[
	    nonvar( Datum),
	    Datum = Skel
	],
	subs( 1, A, Pattern, Skel, D0,D1).
subs( Pattern, Datum, D0,D1) -->
	{
	    functor( Pattern, N, A),
	    functor( Datum, N, A)
	},
	subs( 1, A, Pattern, Datum, D0,D1).

subs( N, M, _, _, D0,D0) --> {N>M}, !.
subs( N, M, G, S, D0,D2) -->
	{
	    arg( N, G, Ga),
	    arg( N, S, Sa),
	    N1 is N+1
	},
	subs( Ga, Sa, D0,D1),
	subs( N1, M, G, S, D1,D2).

l2conj( [],	true).
l2conj( [X|Xs], Conj) :-
  ( Xs = [], Conj = X
  ; Xs = [_|_], Conj = (X,Xc), l2conj( Xs, Xc)
  ).
