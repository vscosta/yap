% This file has been included as an YAP library by Vitor Santos Costa, 2013

% it implements a very simple interval solver designed to interact with the exo
% data-base.
% It assumes simple queries and a contiguous interval,
% and does not really expect to do non-trivial
% constraint propagation and solving.

:- module(exo_interval,
	[max/2,
	 min/2,
	 (#<)/2,
	 (#>)/2,
	 (#=)/2,
	 op(700, xfx, (#>)),
	 op(700, xfx, (#<)),
	 op(700, xfx, (#=))]).

:- meta_predicate
   max(?,0),
   min(?,0).

max(X, G) :-
	attvar(X),
	get_attr(X, exo_interval, Atts), !,
	throw(error('cannot handle combination of attributes ')).
max(X, G) :-
	var(X),
	put_attr(X, exo_interval, max),
	call(G).

min(X, G) :-
	attvar(X),
	get_attr(X, exo_interval, Atts), !,
	throw(error('cannot handle combination of attributes ')).
min(X, G) :-
       var(X),
       put_attr(X, exo_interval, min),
       call(G).

X #> Y :-
	( var(X) -> put_attr(X, exo_interval, '>'(Y) ) ; true ),
	( var(Y) -> put_attr(X, exo_interval, '<'(X) ) ; true ),
	when((nonvar(X), nonvar(Y)), X > Y).

X #< Y :-
	( var(X) -> put_attr(X, exo_interval, '<'(Y) ) ; true ),
	( var(Y) -> put_attr(X, exo_interval, '>'(X) ) ; true ),
	when((nonvar(X), nonvar(Y)), X < Y).

X #= Y :-
	X = Y,
	( var(X) -> put_attr(X, exo_interval, '='(Y) ) ; true ).

attribute_goals(X) -->
        { get_attr(X, exo_interval, Op) },
        ( { Op = max } -> [max(X)] ;
	  { Op = min } -> [min(X)] ;
          { Op = '>'(Y) } -> [X #> Y] ;
          { Op = '<'(Y) } -> [X #< Y] ;
          { Op = '='(Y) } -> [X #= Y] ).	


