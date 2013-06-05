% This file has been included as an YAP library by Vitor Santos Costa, 2013

% it implements a very simple interval solver designed to interact with the exo
% data-base.
% It assumes simple queries and a contiguous interval,
% and does not really expect to do non-trivial
% constraint propagation and solving.

:- module(exo_interval,
	[max/2,
	 min/2,
	 max/1,
	 min/1,
	 any/2,
	 any/1,
	 max/3,
	 min/3,
	 any/3,
	 (#<)/2,
	 (#>)/2,
	 (#=<)/2,
	 (#>=)/2,
	 (#=)/2,
	 op(700, xfx, (#>)),
	 op(700, xfx, (#<)),
	 op(700, xfx, (#>=)),
	 op(700, xfx, (#=<)),
	 op(700, xfx, (#=))]).

:- meta_predicate max(?,0,?), min(?,0,?), any(?,0,?).

max(X, G, X) :-
	insert_atts(X, i(_,_,max)),
	call(G).

min(X, G, X) :-
	insert_atts(X, i(_,_,min)),
	call(G).

max(X, X) :-
	insert_atts(X, i(_,_,max)).

min(X, X) :-
	insert_atts(X, i(_,_,min)).

max(X) :-
	insert_atts(X, i(_,_,max)).

any(X) :-
	insert_atts(X, i(_,_,any)).

min(X) :-
	insert_atts(X, i(_,_,min)).


X #> Y :-
	( var(X) -> insert_atts(X, i(Y,_,_))
                  ;
	          ( var(Y) -> insert_atts(Y, i(_,X,_) ) ; 
		      true
		  )
	      ;
	  var(Y) -> insert_atts(Y, i(_,X,_))
        ;
	  X > Y
        ).

X #>= Y :-
	( var(X) -> insert_atts(X, i(Y-1,_,_))
        ;
	  X >= Y
        ).

X #< Y :-
	( var(X) -> insert_atts(X, i(_,Y,_))
        ;
	  X < Y
        ).

X #=< Y :-
	( var(X) -> insert_atts(X, i(Y+1,_,_))
        ;
	  X =< Y
        ).

X #= Y :-
	( var(X) -> insert_atts(X, i(Y-1,Y+1,_)) ;
	  X =:= Y
        ).


attribute_goals(X) -->
        { get_attr(X, exo_interval, Op) },
        ( { Op = max } -> [max(X)] ;
	  { Op = min } -> [min(X)] ;
          { Op = '>'(Y) } -> [X #> Y] ;
          { Op = '<'(Y) } -> [X #< Y] ;
          { Op = range(A,B,C) } -> 
	        range_min(A,X),
		range_max(B,X),
		range_op(C, X)
	).

range_min(Y, _X) -->
	{ var(Y) }, !,
	[].
range_min(Y, X) -->
	[X #> Y].

range_max(Y, _X) -->
	{ var(Y) }, !,
	[].
range_max(Y, X) -->
	[X #< Y].

range_op(Y, _X) -->
	{ var(Y) }, !,
	[].
range_op(Y, X) -->
	{ Op =.. [Y, X] },
	[Op].

insert_atts(V, Att) :-
	( nonvar(V) ->
	    throw( error(uninstantion_error(V), exo_interval) )
	; attvar(V) ->
	    get_attr(V, exo_interval, Att0),
	    expand_atts(Att, Att0, NAtt)
	;
	  NAtt = Att
	),
	put_attr(V, exo_interval, NAtt).

expand_atts(i(A1, B1, C1), i(A2, B2, C2), i(A3,B3,C3)) :-
	expand_min(A1, A2, A3),
	expand_max(B1, B2, B3),
	expand_op(C1, C2, C3).

expand_min(A1, A2, A3) :-
	(var(A1) -> A3 = A2;
	 var(A2) -> A3 = A1;
	 ground(A1), ground(A2) -> A3 is max(A1,A2) ;
	 A3 = max(A1,A2)
        ).

expand_max(A1, A2, A3) :-
	(var(A1) -> A3 = A2;
	 var(A2) -> A3 = A1;
	 ground(A1), ground(A2) -> A3 is min(A1,A2) ;
	 A3 = min(A1,A2)
        ).

expand_op(A1, A2, A3) :-
	(var(A1) -> A3 = A2;
	 var(A2) -> A3 = A1;
	 A1 == A2 -> A3 = A1;
	 A1 == unique -> A3 = unique;
	 A2 == unique -> A3 = unique;
	 A2 == min, A1 = max -> A3 = unique;
	 A1 == min, A2 = max -> A3 = unique;
	 A1 == min -> A3 = min;	 A2 == min -> A3 = min;
	 A1 == max -> A3 = max;	 A2 == max -> A3 = max;
	 A3 = any
        ).
