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
* File:		arith.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	arithmetical optimization				 *
*									 *
*************************************************************************/

% the default mode is on

expand_exprs(Old,New) :-
	(get_value('$c_arith',true) ->
			Old = on ;
			Old = off ),
	'$set_arith_expan'(New).

'$set_arith_expan'(on) :- set_value('$c_arith',true).
'$set_arith_expan'(off) :- set_value('$c_arith',[]).

compile_expressions :- set_value('$c_arith',true).

do_not_compile_expressions :- set_value('$c_arith',[]).

'$c_built_in'(IN, M, OUT) :-
	get_value('$c_arith',true), !,
	'$do_c_built_in'(IN, M, OUT).
'$c_built_in'(IN, _, IN).


'$do_c_built_in'(G, M, OUT) :- var(G), !,
	'$do_c_built_in'(call(G), M, OUT).
'$do_c_built_in'(Mod:G, _, GN) :- !,
	'$do_c_built_in'(G, Mod, GN0),
	(GN0 = (_,_) -> GN = GN0 ; GN = Mod:GN0).
'$do_c_built_in'(\+ G, _, OUT) :-
	nonvar(G),
	G = (A = B),
	!,
	OUT = (A \= B).
'$do_c_built_in'(call(G), _, OUT) :-
	nonvar(G),
	G = (Mod:G1), !,
	'$do_c_built_metacall'(G1, Mod, OUT).
'$do_c_built_in'(call(G), Mod, OUT) :-
	var(G), !,
	'$do_c_built_metacall'(G, Mod, OUT).
'$do_c_built_in'(depth_bound_call(G,D), M, OUT) :- !,
	'$do_c_built_in'(G, M, NG),
	% make sure we don't have something like (A,B) -> $depth_next(D), A, B.
	( '$composed_built_in'(NG) ->
	    OUT = depth_bound_call(NG,D)
	;
	    OUT = ('$set_depth_limit_for_next_call'(D),NG)
	).
'$do_c_built_in'(once(G), M, (yap_hacks:current_choice_point(CP),NG,'$$cut_by'(CP))) :- !,
	'$do_c_built_in'(G,M,NG0),
	'$clean_cuts'(NG0, NG).
'$do_c_built_in'(forall(Cond,Action), M, \+((NCond, \+(NAction)))) :- !,
	'$do_c_built_in'(Cond,M,ICond),
	'$do_c_built_in'(Action,M,IAction),
	'$clean_cuts'(ICond, NCond),
	'$clean_cuts'(IAction, NAction).
'$do_c_built_in'(ignore(Goal), M, (NGoal -> true ; true)) :- !,
	'$do_c_built_in'(Goal,M,IGoal),
	'$clean_cuts'(IGoal, NGoal).
'$do_c_built_in'(if(G,A,B), M, (yap_hacks:current_choicepoint(DCP),NG,yap_hacks:cut_at(DCP),NA; NB)) :- !,
	'$do_c_built_in'(G,M,NG0),
	'$clean_cuts'(NG0, NG),
	'$do_c_built_in'(A,M,NA),
	'$do_c_built_in'(B,M,NB).
'$do_c_built_in'((G*->A), M, (NG,NA)) :- !,
	'$do_c_built_in'(G,M,NG0),
	'$clean_cuts'(NG0, NG),
	'$do_c_built_in'(A,M,NA).
'$do_c_built_in'('C'(A,B,C), _, (A=[B|C])) :- !.
'$do_c_built_in'(X is Y, M, P) :-
        primitive(X), !,
	'$do_c_built_in'(X =:= Y, M, P).
'$do_c_built_in'(X is Y, M, (P,A=X)) :-
	nonvar(X), !,
	'$do_c_built_in'(A is Y, M, P).
'$do_c_built_in'(X is Y, _, P) :-
	nonvar(Y),		% Don't rewrite variables
	!,
	(
		number(Y) ->
		P = ( X = Y); % This case reduces to an unification
		'$expand_expr'(Y, P0, X0),
		'$drop_is'(X0, X, P0, P)
	).
'$do_c_built_in'(Comp0, _, R) :-		% now, do it for comparisons
	'$compop'(Comp0, Op, E, F),
	!,
	'$compop'(Comp,  Op, U, V),
	'$expand_expr'(E, P, U),
	'$expand_expr'(F, Q, V),
	'$do_and'(P, Q, R0),
	'$do_and'(R0, Comp, R).
'$do_c_built_in'(P, _, P).

'$do_c_built_metacall'(G1, Mod, '$execute_wo_mod'(G1,Mod)) :- 
	var(Mod), !.
'$do_c_built_metacall'(G1, Mod, '$execute_in_mod'(G1,Mod)) :- 
	var(G1), atom(Mod), !.
'$do_c_built_metacall'(Mod:G1, _, OUT) :-  !,
	'$do_c_built_metacall'(G1, Mod, OUT).
'$do_c_built_metacall'(G1, Mod, '$execute_in_mod'(G1,Mod)) :-
	atom(Mod), !.
'$do_c_built_metacall'(G1, Mod, call(Mod:G1)).

'$do_and'(true, P, P) :- !.
'$do_and'(P, true, P) :- !.
'$do_and'(P, Q, (P,Q)).

% V is the result of the simplification,
% X the result of the initial expression
% and the last argument is how we are writing this result
'$drop_is'(V, V1, P0, G) :- var(V), !,		% usual case
        V = V1, P0 = G.
'$drop_is'(V, X, P0, P) :-			% atoms
        '$do_and'(P1, X is V, P).


% Table of arithmetic comparisons
'$compop'(X < Y, < , X, Y).
'$compop'(X > Y, > , X, Y).
'$compop'(X=< Y,=< , X, Y).
'$compop'(X >=Y, >=, X, Y).
'$compop'(X=:=Y,=:=, X, Y).
'$compop'(X=\=Y,=\=, X, Y).

'$composed_built_in'(V) :- var(V), !,
	fail.
'$composed_built_in'((yap_hacks:current_choice_point(_),NG,'$$cut_by'(_))) :- !,
	'$composed_built_in'(NG).
'$composed_built_in'((_,_)).
'$composed_built_in'((_;_)).
'$composed_built_in'((_|_)).
'$composed_built_in'((_->_)).
'$composed_built_in'(_:G) :-
	'$composed_built_in'(G).
'$composed_built_in'(\+G) :-
	'$composed_built_in'(G).
'$composed_built_in'(not(G)) :-
	'$composed_built_in'(G).
	
% expanding an expression:
% first argument is the expression not expanded,
% second argument the expanded expression
% third argument unifies with the result from the expression
'$expand_expr'(V, true, V) :-
	var(V), !.
'$expand_expr'([T], E, V) :- !,
	'$expand_expr'(T, E, V).
'$expand_expr'(A, true, A) :-
	atomic(A), !.
'$expand_expr'(T, E, V) :-
	T =.. [O, A], !,
	'$expand_expr'(A, Q, X),
	'$expand_expr'(O, X, V, Q, E).
'$expand_expr'(T, E, V) :-
	T =.. [O, A, B], !,
	'$expand_expr'(A, Q, X),
	'$expand_expr'(B, R, Y),
	'$expand_expr'(O, X, Y, V, Q, S),
	'$do_and'(R, S, E).

% expanding an expression of the form:
%	O is Op(X),
%	after having expanded into Q
%	and giving as result P (the last argument)
'$expand_expr'(Op, X, O, Q, Q) :-
	number(X), !,
	is( O, Op, X).
'$expand_expr'(Op, X, O, Q, P) :-
	'$unary_op_as_integer'(Op,IOp),
	'$do_and'(Q, is( O, IOp, X), P).

% expanding an expression of the form:
%	O is Op(X,Y),
%	after having expanded into Q
%	and giving as result P (the last argument)
%	included is some optimization for:
%		incrementing and decrementing,
%		the elementar arithmetic operations [+,-,*,//]
'$expand_expr'(Op, X, Y, O, Q, Q) :-
	number(X), number(Y), !,
	is( O, Op, X, Y).
'$expand_expr'(+, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$plus'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
'$expand_expr'(-, X, Y, O, Q, P) :-
	var(X), number(Y),
	Z is -Y, !,
	'$expand_expr'(+, Z, X, O, Q, P).
'$expand_expr'(-, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_non_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$minus'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
'$expand_expr'(*, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$times'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
'$expand_expr'(//, X, Y, O, Q, P) :-
	nonvar(Y), Y == 0, !,
	'$binary_op_as_integer'(//,IOp),
	'$do_and'(Q, is(O,IOp,X,Y), P).
'$expand_expr'(//, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_non_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$div'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
'$expand_expr'(/\, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$and'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
'$expand_expr'(\/, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$or'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
'$expand_expr'(<<, X, Y, O, Q, P) :-
	var(X), number(Y), Y < 0,
	Z is -Y, !,
	'$expand_expr'(>>, X, Z, O, Q, P).
'$expand_expr'(<<, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_non_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$sll'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
'$expand_expr'(>>, X, Y, O, Q, P) :-
	var(X), number(Y), Y < 0,
	Z is -Y, !,
	'$expand_expr'(<<, X, Z, O, Q, P).
'$expand_expr'(>>, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_non_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$slr'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
'$expand_expr'(Op, X, Y, O, Q, P) :-
	'$binary_op_as_integer'(Op,IOp),
	'$do_and'(Q, is(O,IOp,X,Y), P).

'$preprocess_args_for_commutative'(X, Y, X, Y, true) :-
	var(X), var(Y), !.
'$preprocess_args_for_commutative'(X, Y, X, Y, true) :-
	var(X), integer(Y), \+ '$bignum'(Y), !.
'$preprocess_args_for_commutative'(X, Y, X, Z, Z = Y) :-
	var(X), !.
'$preprocess_args_for_commutative'(X, Y, Y, X, true) :-
	integer(X), \+ '$bignum'(X), var(Y), !.
'$preprocess_args_for_commutative'(X, Y, Z, X, Z = Y) :-
	integer(X), \+ '$bignum'(X), !.
'$preprocess_args_for_commutative'(X, Y, Z, W, E) :-
	'$do_and'(Z = X, Y = W, E).

'$preprocess_args_for_non_commutative'(X, Y, X, Y, true) :-
	var(X), var(Y), !.
'$preprocess_args_for_non_commutative'(X, Y, X, Y, true) :-
	var(X), integer(Y), \+ '$bignum'(Y), !.
'$preprocess_args_for_non_commutative'(X, Y, X, Z, Z = Y) :-
	var(X), !.
'$preprocess_args_for_non_commutative'(X, Y, X, Y, true) :-
	integer(X), \+ '$bignum'(X), var(Y), !.
'$preprocess_args_for_non_commutative'(X, Y, X, Z, Z = Y) :-
	integer(X), \+ '$bignum'(X), !.
'$preprocess_args_for_non_commutative'(X, Y, Z, W, E) :-
	'$do_and'(Z = X, Y = W, E).
	
/* Arithmetics					*/

between(I,M,J) :-
	(
	 var(I)
	->
	 '$do_error'(instantiation_error,between(I,M,J))
	;
	 integer(I)
	->
	 (
	  var(M)
	 ->
	  '$do_error'(instantiation_error,between(I,M,J))
	 ;
	  integer(M)
	 ->
	  (
	   var(J)
	  ->
	   I =< M, '$between'(I,M,J)
	  ;
	   integer(J)
	  ->
	   J >= I, J =< M
	  ;
	   '$do_error'(type_error(integer, J),between(I,M,J))
	   )
	 ;
	  M == inf ->
	  (
	   var(J)
	  ->
	   '$between_inf'(I,J)
	  ;
	   integer(J)
	  ->
	   J >= I
	  ;
	   '$do_error'(type_error(integer, J),between(I,M,J))
	  )
	 ;
	  M == infinity ->
	  (
	   var(J)
	  ->
	   '$between_inf'(I,J)
	  ;
	   integer(J)
	  ->
	   J >= I
	  ;
	   '$do_error'(type_error(integer, J),between(I,M,J))
	  )
	 ;
	  '$do_error'(type_error(integer, M),between(I,M,J))
	 )
	;
	 '$do_error'(type_error(integer, I),between(I,M,J))		
	).

'$between'(I,M,I) :- (I == M -> ! ; true ).
'$between'(I0,I,J) :- I0 < I, 
	'$plus'(I0, 1, I1),
	'$between'(I1,I,J).

'$between_inf'(I,I).
'$between_inf'(I,J) :-
	'$plus'(I, 1, I1),
	'$between_inf'(I1,J).


% M and N nonnegative integers, N is the successor of M
succ(M,N) :-
	(
	 var(M)
	->
	 (
	  integer(N),
	  N > 0
	 ->
	  '$plus'(N,-1,M)
	 ;
	  '$succ_error'(M,N)
	 )
	;
	 integer(M),
	 M >= 0
	->
	 (
	  var(N)
	 ->
	 '$plus'(M,1,N)
	 ;
	  integer(N),
	  N > 0
	 ->
	 '$plus'(M,1,N)
	 ;
	  '$succ_error'(M,N)
	 )
	;
	 '$succ_error'(M,N)
	).

'$succ_error'(M,N) :-
	var(M),
	var(N), !,
	'$do_error'(instantiation_error,succ(M,N)).
'$succ_error'(M,N) :-
	nonvar(M),
	\+ integer(M),
	'$do_error'(type_error(integer, M),succ(M,N)).
'$succ_error'(M,N) :-
	nonvar(M),
	M < 0,
	'$do_error'(domain_error(not_less_than_zero, M),succ(M,N)).
'$succ_error'(M,N) :-
	nonvar(N),
	\+ integer(N),
	'$do_error'(type_error(integer, N),succ(M,N)).
'$succ_error'(M,N) :-
	nonvar(N),
	N < 0,
	'$do_error'(domain_error(not_less_than_zero, N),succ(M,N)).

plus(X, Y, Z) :-
       (
        var(X)
       ->
        (
         integer(Y), integer(Z)
        ->
         '$minus'(Z,Y,X)
        ;
         '$plus_error'(X,Y,Z)
        )
       ;
        integer(X)
       ->
        (
         var(Y)
        ->
         (
          integer(Z)
         ->
          '$minus'(Z,X,Y)
         ;
         '$plus_error'(X,Y,Z)
         )
        ;
         integer(Y)
        ->
         (
          integer(Z)
         ->
          '$minus'(Z,Y,X)
         ;
          var(Z)
         ->
          '$plus'(X,Y,Z)
         ;
          '$plus_error'(X,Y,Z)
         )
        ;
         '$plus_error'(X,Y,Z)
        )
       ;
        '$plus_error'(X,Y,Z)
       ).

'$plus_error'(X,Y,Z) :-
       nonvar(X),
       \+ integer(X),
       '$do_error'(type_error(integer, X),plus(X,Y,Z)).
'$plus_error'(X,Y,Z) :-
       nonvar(Y),
       \+ integer(Y),
       '$do_error'(type_error(integer, Y),plus(X,Y,Z)).
'$plus_error'(X,Y,Z) :-
       nonvar(Z),
       \+ integer(Z),
       '$do_error'(type_error(integer, Z),plus(X,Y,Z)).
'$plus_error'(X,Y,Z) :-
       '$do_error'(instantiation_error,plus(X,Y,Z)).



