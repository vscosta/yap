
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

%% @file arith.yap

:- system_module( '$_arith', [compile_expressions/0,
        expand_exprs/2,
        plus/3,
        succ/2], ['$c_built_in'/4]).

:- private( [do_c_built_in/3,
	     do_c_built_metacall/3,
	     expand_expr/3,
	     expand_expr/5,
	     expand_expr/6] ).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_modules', ['$clean_cuts'/2]).

/** @defgroup CompilerAnalysis Internal Clause Rewriting
    @ingroup YAPCompilerSettings
    @{

  YAP supports several clause optimisation mechanisms, that
  are designed to improve execution of arithmetic
  and term construction built-ins. In other words, during the
  compilation process a clause is rewritten twice:

  1. first, perform user-defined goal_expansion as described
  in the predicates goal_expansion/1 and goal_expansion/2.

  2. Perform expansion of some built-ins like:

     + pruning operators, like ->/2 and *->/2

     + arithmetic, including early evaluation of constant expressions

     + specialise versions for some built-ins, if we are aware of the
      run-time execution mode

  The user has  control over this process, through
  built-ins and through prolog flags.

*/


/** @pred expand_exprs(- _O_,+ _N_)
	Control term expansion during compilation.

Enables low-level optimizations. It reports the current state by
unifying _O_ with the previous state.  It then puts YAP in state _N_
(`on` or `off`)/ _On_ is equivalent to compile_expressions/0 and `off`
is equivalent to do_not_compile_expressions/0.

This predicate is useful when debugging, to ensure execution close to the original source.

*/
expand_exprs(Old,New) :-
	(get_value('$c_arith',true) ->
			Old = on ;
			Old = off ),
	'$set_arith_expan'(New).

'$set_arith_expan'(on) :- set_value('$c_arith',true).
'$set_arith_expan'(off) :- set_value('$c_arith',[]).

/**  @pred   compile_expressions

After a call to this predicate, arithmetical expressions will be compiled.
(see example below). This is the default behavior.
*/
compile_expressions :- set_value('$c_arith',true).

/**  @pred do_not_compile_expressions


After a call to this predicate, arithmetical expressions will not be compiled.

~~~~~~~
?- source, do_not_compile_expressions.
yes
?- [user].
| p(X) :- X is 2 * (3 + 8).
| :- end_of_file.
?- compile_expressions.
yes
?- [user].
| q(X) :- X is 2 * (3 + 8).
| :- end_of_file.
:- listing.

p(A):-
      A is 2 * (3 + 8).

q(A):-
      A is 22.

~~~~~~~
*/
do_not_compile_expressions :- set_value('$c_arith',[]).

'$c_built_in'(IN, M, H, OUT) :-
	get_value('$c_arith',true), !,
	'$yap_strip_module'(M:IN, M1,  G1),
	do_c_built_in(G1, M1, H, OUT).
'$c_built_in'(IN, _, _H, IN).


do_c_built_in(G1, M1, H, OUT) :-
    var(G1), !,
    do_c_built_metacall(G1, M1, H, OUT).
do_c_built_in(G1, M1, H, OUT) :-
    var(M1), !,
    do_c_built_metacall(G1, M1, H, OUT).
do_c_built_in('$do_error'( Error, Goal), M, Head,
	      throw(error(Error,[errorGoal=Goal, errorCaller=Head,prologPredFile=File,prologPredLine=Line,
	      prologPredModule=M,prologPredName=Name,prologPredArity=Arity])))
	     :-
    !,source_location(File, Line),
    functor(Head,Name,Arity).
do_c_built_in(system_error( Error, Goal), M, Head, ErrorG) :-
        !,
       do_c_built_in('$do_error'( Error, Goal), M, Head, ErrorG).
do_c_built_in(X is Y, M, H,  P) :-
        primitive(X), !,
	do_c_built_in(X =:= Y, M, H, P).
do_c_built_in(X is Y, M, H, (P,A=X)) :-
	nonvar(X), !,
	do_c_built_in(A is Y, M, H, P).
do_c_built_in(X is Y, _, _, P) :-
	nonvar(Y),		% Don't rewrite variables
	!,
	(
	    number(Y) ->
	    P = ( X = Y); % This case reduces to an unification
	    expand_expr(Y, P0, X0),
	    '$drop_is'(X0, X, P0, P)
	).
do_c_built_in(phrase(NT,Xs),  Mod, H, NTXsNil) :-
    !,
	'$_arith':do_c_built_in(phrase(NT,Xs,[]), Mod, H, NTXsNil).
do_c_built_in(phrase(NT,Xs0,Xs), Mod, _,  NewGoal) :-
    !,
    '$c_built_in_phrase'(NT, Xs0, Xs, Mod, NewGoal ).
do_c_built_in(Comp0, _, _, R) :-		% now, do it for comparisons
	'$compop'(Comp0, Op, E, F),
	!,
	'$compop'(Comp,  Op, U, V),
	expand_expr(E, P, U),
	expand_expr(F, Q, V),
	'$do_and'(P, Q, R0),
	'$do_and'(R0, Comp, R).
do_c_built_in(P, _M, _H, P).

do_c_built_metacall(G1, Mod, _, '$execute_wo_mod'(G1,Mod)) :-
    var(Mod), !.
do_c_built_metacall(G1, Mod, _, '$execute_in_mod'(G1,Mod)) :-
    atom(Mod), !.
do_c_built_metacall(G1, Mod, _, call(Mod:G1)).

'$do_and'(true, P, P) :- !.
'$do_and'(P, true, P) :- !.
'$do_and'(P, Q, (P,Q)).

% V is the result of the simplification,
% X the result of the initial expression
% and the last argument is how we are writing this result
'$drop_is'(V, V1, P0, G) :-
    var(V),
    !,		% usual case
    V = V1,
    P0 = G.
'$drop_is'(V, X, P0, P) :-			% atoms
    '$do_and'(P0, X is V, P).

% Table of arithmetic comparisons
'$compop'(X < Y, < , X, Y).
'$compop'(X > Y, > , X, Y).
'$compop'(X=< Y,=< , X, Y).
'$compop'(X >=Y, >=, X, Y).
'$compop'(X=:=Y,=:=, X, Y).
'$compop'(X=\=Y,=\=, X, Y).

'$composed_built_in'(V) :- var(V), !,
	fail.
'$composed_built_in'(('$current_choice_point'(_),NG,'$$cut_by'(_))) :- !,
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
expand_expr(V, true, V) :-
	var(V), !.
expand_expr([T], E, V) :- !,
	expand_expr(T, E, V).
expand_expr(String, _E, V) :-
	string( String ), !,
	string_codes(String, [V]).
expand_expr(A, true, A) :-
	atomic(A), !.
expand_expr(T, E, V) :-
	T =.. [O, A], !,
	expand_expr(A, Q, X),
	expand_expr(O, X, V, Q, E).
expand_expr(T, E, V) :-
	T =.. [O, A, B], !,
	expand_expr(A, Q, X),
	expand_expr(B, R, Y),
	expand_expr(O, X, Y, V, Q, S),
	'$do_and'(R, S, E).

% expanding an expression of the form:
%	O is Op(X),
%	after having expanded into Q
%	and giving as result P (the last argument)
expand_expr(Op, X, O, Q, Q) :-
    number(X),
    !,
    catch(is( O, Op, X),Error,bad_expr(Error,[Op, X])), !.
 % do not do error handling at compile time
expand_expr(Op, X, O, Q, P) :-
	'$unary_op_as_integer'(Op,IOp),
	'$do_and'(Q, is( O, IOp, X), P).

% expanding an expression of the form:
%	O is Op(X,Y),
%	after having expanded into Q
%	and giving as result P (the last argument)
%	included is some optimization for:
%		incrementing and decrementing,
%		the elementar arithmetic operations [+,-,*,//]
expand_expr(Op, X, Y, O, Q, Q) :-
	number(X), number(Y),
	catch(is( O, Op, X, Y),Error,bad_expr(Error,[Op, X, Y  ])), !.
expand_expr(+, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$plus'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
expand_expr(-, X, Y, O, Q, P) :-
	var(X), number(Y),
	Z is -Y, !,
	expand_expr(+, Z, X, O, Q, P).
expand_expr(-, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_non_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$minus'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
expand_expr(*, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$times'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
expand_expr(//, X, Y, O, Q, P) :-
	nonvar(Y), Y == 0, !,
	'$binary_op_as_integer'(//,IOp),
	'$do_and'(Q, is(O,IOp,X,Y), P).
expand_expr(//, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_non_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$div'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
expand_expr(/\, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$and'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
expand_expr(\/, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$or'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
expand_expr(<<, X, Y, O, Q, P) :-
	var(X), number(Y), Y < 0,
	Z is -Y, !,
	expand_expr(>>, X, Z, O, Q, P).
expand_expr(<<, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_non_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$sll'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
expand_expr(>>, X, Y, O, Q, P) :-
	var(X), number(Y), Y < 0,
	Z is -Y, !,
	expand_expr(<<, X, Z, O, Q, P).
expand_expr(>>, X, Y, O, Q, P) :- !,
	'$preprocess_args_for_non_commutative'(X, Y, X1, Y1, E),
	'$do_and'(E, '$slr'(X1,Y1,O), F),
	'$do_and'(Q, F, P).
expand_expr(Op, X, Y, O, Q, P) :-
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


'$goal_expansion_allowed'(phrase(NT,_Xs0,_Xs), Mod) :-
    callable(NT),
    atom(Mod).

%%	contains_illegal_dcgnt(+Term) is semidet.
%
%	True if Term contains a non-terminal   we cannot deal with using
%	goal-expansion. The test is too general an approximation, but safe.

'$contains_illegal_dcgnt'(NT) :-
    functor(NT, _, A),
    between(1, A, I),
    arg(I, NT, AI),
    nonvar(AI),
    ( AI = ! ; AI = phrase(_,_,_) ), !.
%	write(contains_illegal_nt(NT)),		% JW: we do not want to write
%	nl.

'$harmless_dcgexception'(instantiation_error).	% ex: phrase(([1],x:X,[3]),L)
'$harmless_dcgexception'(type_error(callable,_)).	% ex: phrase(27,L)

:- set_value('$c_arith',true).
/**
  @}
*/
