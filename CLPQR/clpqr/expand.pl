%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.2 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   expand.pl                                              %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Perform theory-purification in the presence of
% interpreted terms.
%

:- module( expand, [expand/0, noexpand/0]).
:- dynamic expanding/0.

:- multifile
	user:term_expansion/2,
	user:goal_expansion/3.

:- dynamic
	user:term_expansion/2,
	user:goal_expansion/3.

user:term_expansion(A,B) :-			% expands heads
	expanding,
	purify_head(A,B).

user:goal_expansion(G,M,E):-			% expands bodies
	     M \== prolog, % quite common due to internal call_cleanups etc.
	     M \== expand,			% this file may get loaded more than once
	     expanding,
	     purify(G,M,E).

noexpand :-
	retractall(expanding).

expand :-
	assert(expanding).


purify( Goal, Module, _) :- quoted( Goal, Module), !, fail.
purify( Goal, _,      Expansion) :-
  interpreted_relation( Goal, _Module),
  !,
  Expansion = {Goal}.
purify( X=Y,  _Module, Expansion) :- !,		% shortcut for =/2
  ( ( var(X) ; interpreted_term( X, Th) ),
    ( var(Y) ; interpreted_term( Y, Th) ),
    nonvar( Th) ->
       Expansion = {X=Y}
  ;
       ra( X=Y, user, Pure, ThL, ThLTail),
       ThL \== ThLTail,
       ThLTail = [Pure],
       l2conj( ThL, Expansion)
  ).
purify( Goal, _Module, Expansion) :-
  nobuiltin( Goal),
  ra( Goal, user, Pure, ThL, ThLTail),
  ThL \== ThLTail,				% ifunctors present ?
  ThLTail = [Pure],
  l2conj( ThL, Expansion).

nobuiltin( Goal) :-
  predicate_property( Goal, built_in),
  !,
  fail.
nobuiltin( _).

quoted( {_},			  _).
quoted( run(_,_),		  geler).	% late goal of verify_attributes/3
quoted( resubmit_eq(_), 	  nf).		%   some (all?) of its clients
quoted( resubmit_lt(_), 	  nf).
quoted( resubmit_le(_), 	  nf).
quoted( resubmit_ne(_), 	  nf).
quoted( wait_linear_retry(_,_,_), nf).
quoted( fm_cp_filter(_,_,_),	  linear).	% generator in findall/3
%
quoted( printf(_),	     _).
quoted( printf(_,_),	     _).
quoted( user_tout(_,_,_,_),  _).		% undef pred

%
% Identify the theory (module) involved.
%
interpreted_term( X,		_) :- var(X), !, fail.
interpreted_term( X,		linear) :- number(X).
interpreted_term( rat(_,_),	linear).	% rational constant
%
interpreted_term( #(_), 	linear).	% Monash
interpreted_term( -(_), 	linear).	% unary minus
interpreted_term( +(_), 	linear).	%	plus
interpreted_term( +(_,_),	linear).	% binary
interpreted_term( -(_,_),	linear).
interpreted_term( *(_,_),	linear).
interpreted_term( /(_,_),	linear).
interpreted_term( pow(_,_),	linear).
interpreted_term( exp(_,_),	linear).
interpreted_term( ^(_,_),	linear).
interpreted_term( sin(_),	linear).
interpreted_term( cos(_),	linear).
interpreted_term( tan(_),	linear).
interpreted_term( min(_,_),	linear).
interpreted_term( max(_,_),	linear).
interpreted_term( abs(_),	linear).

interpreted_relation( G,		_) :- var(G), !, fail.
interpreted_relation( <(_,_),		linear).
interpreted_relation( =<(_,_),		linear).
interpreted_relation( <=(_,_),		linear). % Monash
interpreted_relation( >(_,_),		linear).
interpreted_relation( >=(_,_),		linear).
interpreted_relation( =\=(_,_), 	linear).
interpreted_relation( =:=(_,_), 	linear).

% ----------------------------------------------------------------

%
% replace alien subterms by variables
% Special treatment for quote/1
%
%
%
ra( Term, _,  Pure) --> {var(Term), !, Term = Pure}.
ra( Term, Th, Pure) -->
  {
    functor( Term, N, A),
    functor( Pure, N, A)
  },
  ra( A, Term, Th, Pure).

ra( 0, _,    _,  _) --> !.
ra( N, Term, Th, Pure) -->
  {
    N1 is N-1,
    arg( N, Term, Ta),
    arg( N, Pure, Pa)
  },
  ra_one( Ta, Th, Pa),
  ra( N1, Term, Th, Pure).

ra_one( Term,		 _,	       Pure) --> {var( Term), !, Pure=Term}.
ra_one( quote(Term),	 _,	       Pure) --> {!, Pure=Term}.
ra_one( Term,		 ParentTheory, Pure) -->
  {
    ( interpreted_term( Term, Theory) ->
	true
    ;
	Theory=user
    )
  },
  ( { ParentTheory=Theory } ->
      ra( Term, Theory, Pure)
  ;
      ra_equate( Theory, Pure, ThPure),
      ra( Term, Theory, ThPure)
  ).

ra_equate( user,    A, B) --> !, { A=B }.	% now
ra_equate( _Theory, A, B) --> [ {A=B} ].		% later

% ---------------------------------------------------------------------------

purify_head( Term, NewTerm) :-
  ( prolog:dcg_expansion( Term, []/*undef layout*/, Exp, _) ->
      true
  ;
      Term = Exp
  ),
  ( Exp = ?-(_) ->				% don't touch these
      fail
  ; Exp = :-(_) ->				% expanded via goal_expansion
      fail
  ; Exp = (H:-B) ->
      ra( H, user, NewHead, Cl, []),
      Cl = [_|_],				% ifunctors in head ?
      l2conj( Cl, Thc),
      NewTerm = (NewHead:-Thc,B)
  ;						% facts
      ra( Exp, user, H, Cl, []),
      Cl = [_|_],				% ifunctors in head ?
      l2conj( Cl, B),
      NewTerm = (H:-B)
  ).

l2conj( [],	true).
l2conj( [X|Xs], Conj) :-
  ( Xs = [], Conj = X
  ; Xs = [_|_], Conj = (X,Xc), l2conj( Xs, Xc)
  ).
