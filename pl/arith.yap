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
	'$do_c_built_in'(A,M,NA0),
	'$clean_cuts'(NA0, NA),
	'$do_c_built_in'(B,M,NB).
'$do_c_built_in'((G*->A), M, (NG,NA)) :- !,
	'$do_c_built_in'(G,M,NG0),
	'$clean_cuts'(NG0, NG),
	'$do_c_built_in'(A,M,NA).
'$do_c_built_in'('C'(A,B.C), _, (A=[B|C])) :- !.
'$do_c_built_in'(X is Y, _, P) :-
	nonvar(Y),		% Don't rewrite variables
	!,
	(
		number(Y),
		P = ( X = Y); % This case reduces to an unification
		'$expand_expr'(Y, P0, X0),
		'$drop_is'(X0, X, P1),
		'$do_and'(P0, P1, P)
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
'$drop_is'(V, V, true) :- var(V), !.		% usual case
'$drop_is'(V, X, X is V).			% atoms

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
	'$unaryop'(T, O, A), !,
	'$expand_expr'(A, Q, X),
	'$expand_expr'(O, X, V, Q, E).
'$expand_expr'(T, E, V) :-
	'$binaryop'(T, O, A, B), !,
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
	var(X), integer(Y), \+ '$bignum'(Y), !,
	Z is -Y,
	'$do_and'(Q, '$plus'(X,Z,O), P).
'$expand_expr'(-, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_non_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$minus'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
'$expand_expr'(*, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$times'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
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
'$expand_expr'(<<, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_non_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$sll'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
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
	integer(X), \+ '$bignum'(Y), !.
'$preprocess_args_for_non_commutative'(X, Y, Z, W, E) :-
	'$do_and'(Z = X, Y = W, E).
	


% These are the unary arithmetic operators
'$unaryop'(+X		,+	,X).
'$unaryop'(-X		,-	,X).
'$unaryop'(\(X)		,\	,X).
'$unaryop'(exp(X)	,exp	,X).
'$unaryop'(log(X)	,log	,X).
'$unaryop'(log10(X)	,log10	,X).
'$unaryop'(sqrt(X)	,sqrt	,X).
'$unaryop'(sin(X)	,sin	,X).
'$unaryop'(cos(X)	,cos	,X).
'$unaryop'(tan(X)	,tan	,X).
'$unaryop'(asin(X)	,asin	,X).
'$unaryop'(acos(X)	,acos	,X).
'$unaryop'(atan(X)	,atan	,X).
'$unaryop'(atan2(X)	,atan2	,X).
'$unaryop'(sinh(X)	,sinh	,X).
'$unaryop'(cosh(X)	,cosh	,X).
'$unaryop'(tanh(X)	,tanh	,X).
'$unaryop'(asinh(X)	,asinh	,X).
'$unaryop'(acosh(X)	,acosh	,X).
'$unaryop'(atanh(X)	,atanh	,X).
'$unaryop'(floor(X)	,floor	,X).
'$unaryop'(abs(X)	,abs	,X).
'$unaryop'(float(X)	,float	,X).
'$unaryop'(+(X)		,+	,X).
'$unaryop'(integer(X)	,integer,X).
'$unaryop'(truncate(X)	,truncate,X).
'$unaryop'(round(X)	,round	,X).
'$unaryop'(ceiling(X)	,ceiling,X).
'$unaryop'(msb(X)	,msb	,X).
'$unaryop'(sign(X)	,sign	,X).

% These are the binary arithmetic operators
'$binaryop'(X+Y		,+	,X,Y).
'$binaryop'(X-Y		,-	,X,Y).
'$binaryop'(X*Y		,*	,X,Y).
'$binaryop'(X/Y		,/	,X,Y).
'$binaryop'(X mod Y	,mod	,X,Y).
'$binaryop'(X rem Y	,rem 	,X,Y).
'$binaryop'(X//Y	,//	,X,Y).
'$binaryop'(X/\Y	,/\	,X,Y).
'$binaryop'(X\/Y	,\/	,X,Y).
'$binaryop'(X#Y		,'#'	,X,Y).
'$binaryop'(X<<Y	,<<	,X,Y).
'$binaryop'(X>>Y	,>>	,X,Y).
'$binaryop'(X^Y		,^	,X,Y).
'$binaryop'(X**Y	,^	,X,Y).
'$binaryop'(exp(X,Y)	,^	,X,Y).
'$binaryop'(max(X,Y)	,max	,X,Y).
'$binaryop'(min(X,Y)	,min	,X,Y).
'$binaryop'(gcd(X,Y)	,gcd	,X,Y).


% The table number for each operation is given here
% Depends on eval.c
'$unary_op_as_integer'(+,0).
'$unary_op_as_integer'(-,1).
'$unary_op_as_integer'(\,2).
'$unary_op_as_integer'(exp,3).
'$unary_op_as_integer'(log,4).
'$unary_op_as_integer'(log10,5).
'$unary_op_as_integer'(sqrt,6).
'$unary_op_as_integer'(sin,7).
'$unary_op_as_integer'(cos,8).
'$unary_op_as_integer'(tan,9).
'$unary_op_as_integer'(sinh,10).
'$unary_op_as_integer'(cosh,11).
'$unary_op_as_integer'(tanh,12).
'$unary_op_as_integer'(asin,13).
'$unary_op_as_integer'(acos,14).
'$unary_op_as_integer'(atan,15).
'$unary_op_as_integer'(asinh,16).
'$unary_op_as_integer'(acosh,17).
'$unary_op_as_integer'(atanh,18).
'$unary_op_as_integer'(floor,19).
'$unary_op_as_integer'(ceiling,20).
'$unary_op_as_integer'(round,21).
'$unary_op_as_integer'(truncate,22).
'$unary_op_as_integer'(integer,23).
'$unary_op_as_integer'(float,24).
'$unary_op_as_integer'(abs,25).
'$unary_op_as_integer'(msb,26).
'$unary_op_as_integer'(float_fractional_part,27).
'$unary_op_as_integer'(float_integer_part,28).
'$unary_op_as_integer'(sign,29).
'$unary_op_as_integer'(lgamma,30).

'$binary_op_as_integer'(+,0).
'$binary_op_as_integer'(-,1).
'$binary_op_as_integer'(*,2).
'$binary_op_as_integer'(/,3).
'$binary_op_as_integer'(mod,4).
'$binary_op_as_integer'(rem,5).
'$binary_op_as_integer'(//,6).
'$binary_op_as_integer'(<<,7).
'$binary_op_as_integer'(>>,8).
'$binary_op_as_integer'(/\,9).
'$binary_op_as_integer'(\/,10).
'$binary_op_as_integer'('#',11).
'$binary_op_as_integer'(atan2,12).
'$binary_op_as_integer'(^,13).
'$binary_op_as_integer'('**',14).
'$binary_op_as_integer'(exp,15).
'$binary_op_as_integer'(gcd,16).
'$binary_op_as_integer'(min,17).
'$binary_op_as_integer'(max,18).
%'$binary_op_as_integer'(gcdmult,28).

/* Arithmetics					*/

% M and N nonnegative integers, N is the successor of M
succ(M,N) :- integer(M), !, '$plus'(M,1,N).
succ(M,N) :- integer(N), !,  N > 0, '$plus'(N,-1,M).
succ(0,1).

