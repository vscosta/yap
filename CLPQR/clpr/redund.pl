%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.2 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   redund.pl                                              %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% redundancy removal (semantic definition)
%
% done:
%	+) deal with active bounds
%	+) indep t_[lu] -> t_none invalidates invariants (fixed)
%

%
% O(n^2), use sort later
%
systems( [],	 Si, Si).
systems( [V|Vs], Si, So) :-
  ( var(V), get_atts( V, class(C)),
    not_memq( Si, C) ->
      systems( Vs, [C|Si], So)
  ;
      systems( Vs, Si, So)
  ).

not_memq( [],	  _).
not_memq( [Y|Ys], X) :-
  X \== Y,
  not_memq( Ys, X).

redundancy_systems( []).
redundancy_systems( [S|Sys]) :-
  class_allvars( S, All),
  redundancy_vs( All),
  redundancy_systems( Sys).

redundancy_vars( Vs) :- !, redundancy_vs( Vs).
redundancy_vars( Vs) :-
  statistics( runtime, [Start|_]),
    redundancy_vs( Vs),
  statistics( runtime, [End|_]),
  Duration is End-Start,
  format( user_error, "% Redundancy elimination took ~d msec~n", Duration).


%
% remove redundant bounds from variables
%
redundancy_vs( Vs) :- var( Vs), !.
redundancy_vs( []).
redundancy_vs( [V|Vs]) :-
  ( get_atts( V, [type(Type),strictness(Strict)]),
    redundant( Type, V, Strict) ->
      redundancy_vs( Vs)
  ;
      redundancy_vs( Vs)
  ).

redundant( t_l(L), X, Strict) :-
  detach_bounds( X),			% drop temporarily
  negate_l( Strict, L, X),
  red_t_l.
redundant( t_u(U), X, Strict) :-
  detach_bounds( X),
  negate_u( Strict, U, X),
  red_t_u.
redundant( t_lu(L,U), X, Strict) :-
  strictness_parts( Strict, Sl, Su),
  ( put_atts( X, [type(t_u(U)),strictness(Su)]),
    negate_l( Strict, L, X) ->
       red_t_l,
       ( redundant( t_u(U), X, Strict) -> true ; true )
  ; put_atts( X, [type(t_l(L)),strictness(Sl)]),
    negate_u( Strict, U, X) ->
       red_t_u
  ;
       true
  ).
redundant( t_L(L), X, Strict) :-
  arith_eval( -L, Bound),
  intro_at( X, Bound, t_none),		% drop temporarily
  detach_bounds( X),
  negate_l( Strict, L, X),
  red_t_L.
redundant( t_U(U), X, Strict) :-
  arith_eval( -U, Bound),
  intro_at( X, Bound, t_none),		% drop temporarily
  detach_bounds( X),
  negate_u( Strict, U, X),
  red_t_U.
redundant( t_Lu(L,U), X, Strict) :-
  strictness_parts( Strict, Sl, Su),
  ( arith_eval( -L, Bound),
    intro_at( X, Bound, t_u(U)),
    put_atts( X, strictness(Su)),
    negate_l( Strict, L, X) ->
       red_t_l,
       ( redundant( t_u(U), X, Strict) -> true ; true )
  ; put_atts( X, [type(t_L(L)),strictness(Sl)]),
    negate_u( Strict, U, X) ->
       red_t_u
  ;
       true
  ).
redundant( t_lU(L,U), X, Strict) :-
  strictness_parts( Strict, Sl, Su),
  ( put_atts( X, [type(t_U(U)),strictness(Su)]),
    negate_l( Strict, L, X) ->
       red_t_l,
       ( redundant( t_U(U), X, Strict) -> true ; true )
  ; arith_eval( -U, Bound),
    intro_at( X, Bound, t_l(L)),
    put_atts( X, strictness(Sl)),
    negate_u( Strict, U, X) ->
       red_t_u
  ;
       true
  ).

strictness_parts( Strict, Lower, Upper) :-
  Lower is Strict /\ 2'10,
  Upper is Strict /\ 2'01.

%
% encapsulation via \+ (unfolded to avoid metacall)
%
/**/
negate_l( 2'00, L, X) :- { L > X },	!, fail.
negate_l( 2'01, L, X) :- { L > X },	!, fail.
negate_l( 2'10, L, X) :- { L >= X },	!, fail.
negate_l( 2'11, L, X) :- { L >= X },	!, fail.
negate_l(    _, _, _).

negate_u( 2'00, U, X) :- { U < X },	!, fail.
negate_u( 2'01, U, X) :- { U =< X },	!, fail.
negate_u( 2'10, U, X) :- { U < X },	!, fail.
negate_u( 2'11, U, X) :- { U =< X },	!, fail.
negate_u(    _, _, _).
/**/

%
% profiling
%
red_t_l.
red_t_u.
red_t_L.
red_t_U.


