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
 * File:		op.yap						 *
 * Last rev:	8/2/88							 *
 * mods:									 *
 * comments:	Some utility predicates available in yap		 *
 *									 *
 *************************************************************************/

/**
 * @file   utils.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
 * @date   Thu Oct 19 12:21:01 2017
 * 
 * @brief  Utilities
 */
 
/** @defgroup Operators		Defining operators
 * @ingroup Builtins
 * @{ 
 *
 * @brief Prolog allows defining operators at run-time.
 *
 * The Prolog syntax caters for operators of three main kinds:
 *
 * + prefix;
 * + infix;
 * + postfix.
 *
 * Each operator has precedence in the range 1 to 1200, and this
 * precedence is used to disambiguate expressions where the structure of the
 * term denoted is not made explicit using brackets. The operator of higher
 * precedence is the main functor.
 * 
 * If there are two operators with the highest precedence, the ambiguity
 * is solved analyzing the types of the operators. The possible infix types are:
 *  _xfx_,  _xfy_, and  _yfx_.
 * 
 * With an operator of type  _xfx_ both sub-expressions must have lower
 * precedence than the operator itself, unless they are bracketed (which
 * assigns to them zero precedence). With an operator type  _xfy_ only the
 * left-hand sub-expression must have lower precedence. The opposite happens
 * for  _yfx_ type.
 * 
 * A prefix operator can be of type  _fx_ or  _fy_.
 * A postfix operator can be of type  _xf_ or  _yf_.
 * The meaning of the notation is analogous to the above.
 * 
 * ```
 * a + b * c
 * ```
 * means
 * 
 * ```
 * a + (b * c)
 * ```
 * as + and \* have the following types and precedences:
 * 
 * ```
 * :-op(500,yfx,'+').
 * :-op(400,yfx,'*').
 * ```
 * 
 * Now defining
 * 
 * ```
 * :-op(700,xfy,'++').
 * :-op(700,xfx,'=:=').
 * a ++ b =:= c
 * ```
 *  means
 * 
 * ```
 * a ++ (b =:= c)
 * ```
 * 
 * The following is the list of the declarations of the predefined operators:
 * ```
 * :-op(1200,fx,['?-', ':-']).
 * :-op(1200,xfx,[':-','-->']).
 * :-op(1150,fx,[block,dynamic,mode,public,multifile,meta_predicate,
 *               sequential,table,initialization]).
 * :-op(1100,xfy,[';','|']).
 * :-op(1050,xfy,->).
 * :-op(1000,xfy,',').
 * :-op(999,xfy,'.').
 * :-op(900,fy,['\+', not]).
 * :-op(900,fx,[nospy, spy]).
 * :-op(700,xfx,[@>=,@=<,@<,@>,<,=,>,=:=,=\=,\==,>=,=<,==g\=,=..,is]).
 * :-op(500,yfx,['\/','/\','+','-']).
 * :-op(500,fx,['+','-']).
 * :-op(400,yfx,['<<','>>','//','*','/']).
 * :-op(300,xfx,mod).
 * :-op(200,xfy,['^','**']).
 * :-op(50,xfx,same).
 * ```
 * 
 */




/** @pred op(+ _Precedence_,+ _Type_,+ _A_) is iso


Defines the operator  _A_ or the list of operators  _A_ with type
 _T_ (which must be one of `xfx`, `xfy`,`yfx`,
`xf`, `yf`, `fx` or `fy`) and precedence  _P_
(see appendix iv for a list of predefined operators).

Note that if there is a preexisting operator with the same name and
type, this operator will be discarded. Also, `,` may not be defined
as an operator, and it is not allowed to have the same for an infix and
a postfix operator.


*/


% just check the operator declarations for correctness.
'$check_op'(P,T,Op,G) :-
	( var(P) ; var(T); var(Op)), !,
	throw_error(instantiation_error,G).
'$check_op'(P,_,_,G) :-
	\+ integer(P), !,
	throw_error(type_error(integer,P),G).
'$check_op'(P,_,_,G) :-
	P < 0, !,
	throw_error(domain_error(operator_priority,P),G).
'$check_op'(_,T,_,G) :-
	\+ atom(T), !,
	throw_error(type_error(atom,T),G).
'$check_op'(_,T,_,G) :-
	\+  '$associativity'(T), !,
	throw_error(domain_error(operator_specifier,T),G).
'$check_op'(P,T,V,G) :-
	'$check_module_for_op'(V, G, NV),
	'$check_top_op'(P, T, NV, G).

'$check_top_op'(_, _, [], _) :- !.
'$check_top_op'(P, T, [Op|NV], G) :- !,
	'$check_ops'(P, T, [Op|NV], G).
'$check_top_op'(P, T, V, G) :-
	atom(V), !,
	'$check_op_name'(P, T, V, G).
'$check_top_op'(_P, _T, V, G) :-
	throw_error(type_error(atom,V),G).

 '$associativity'(xfx).
 '$associativity'(xfy).
 '$associativity'(yfx).
 '$associativity'(yfy).
 '$associativity'(xf).
 '$associativity'(yf).
 '$associativity'(fx).
 '$associativity'(fy).

'$check_module_for_op'(MOp, G, _) :-
	var(MOp), !,
	throw_error(instantiation_error,G).
'$check_module_for_op'(M:_V, G, _) :-
	var(M), !,
	throw_error(instantiation_error,G).
'$check_module_for_op'(M:V, G, NV) :-
	atom(M), !,
	'$check_module_for_op'(V, G, NV).
'$check_module_for_op'(M:_V, G, _) :- !,
	throw_error(type_error(atom,M),G).
'$check_module_for_op'(V, _G, V).

'$check_ops'(_P, _T, [], _G) :- !.
'$check_ops'(P, T, [Op|NV], G) :- !,
	(
	 var(NV)
	->
	 throw_error(instantiation_error,G)
	;
	 '$check_module_for_op'(Op, G, NOp),
	 '$check_op_name'(P, T, NOp, G),
	 '$check_ops'(P, T, NV, G)
	).
'$check_ops'(_P, _T, Ops, G) :-
	throw_error(type_error(list,Ops),G).

'$check_op_name'(_,_,V,G) :-
	  var(V), !,
	  throw_error(instantiation_error,G).
 '$check_op_name'(_,_,',',G) :- !,
	  throw_error(permission_error(modify,operator,','),G).
'$check_op_name'(_,_,'[]',G) :-  T \= yf, T\= xf, !,
	  throw_error(permission_error(create,operator,'[]'),G).
'$check_op_name'(_,_,'{}',G) :- T \= yf, T\= xf, !,
	  throw_error(permission_error(create,operator,'{}'),G).
'$check_op_name'(P,T,'|',G) :-
	 (
	  integer(P),
	  P < 1001, P > 0
	 ;
	  atom_codes(T,[_,_])
	 ), !,
	 throw_error(permission_error(create,operator,'|'),G).
'$check_op_name'(_,_,V,_) :-
	 atom(V), !.
'$check_op_name'(_,_,A,G) :-
	 throw_error(type_error(atom,A),G).


op(P,T,V) :-
          '$check_op'(P,T,V,op(P,T,V)),
                   '$op'(P, T, V).

'$op'(P, T, ML) :-
         strip_module(ML, M, [A|As]), !,
         '$opl'(P, T, M, [A|As]).
 '$op'(P, T, A) :-
         '$op2'(P,T,A).
 
 '$opl'(_P, _T, _, []).
 '$opl'(P, T, M, [A|As]) :-
         '$op2'(P, T, M:A),
         '$opl'(P, T, M, As).
 
 '$op2'(P,T,A) :-
         atom(A), !,
         'opdec'(P,T,A,prolog).
 '$op2'(P,T,A) :-
         strip_module(A,M,N),
         'opdec'(P,T,N,M).

op_cases(_P, _T, [], _MA) :-
    !.
op_cases(P, T, [A|AS], MA) :-
    !,
    op(P,T,MA:A),
    op_cases(P,T,AS,MA).
op_cases(P, T, A, MA) :-
   opdec(P, T, A, MA).	

/** @pred current_op( _P_, _Type_, _A_) is iso


Defines the relation: _A_ is a currently defined  operator of type
 _T_ and precedence  _P_.


*/
current_op(X,Y,V) :- var(V), !,
	'$current_module'(M),
	'$do_current_op'(X,Y,V,M).
current_op(X,Y,M:Z) :- !,
	'$current_opm'(X,Y,Z,M).
current_op(X,Y,Z) :-
	'$current_module'(M),
	'$do_current_op'(X,Y,Z,M).

'$current_opm'(X,Y,Z,M) :-
	nonvar(Y),
	\+ '$associativity'(Y),
	throw_error(domain_error(operator_specifier,Y),current_op(X,Y,M:Z)).
'$current_opm'(X,Y,Z,M) :-
	var(Z), !,
	'$do_current_op'(X,Y,Z,M).
'$current_opm'(X,Y,M:Z,_) :- !,
	'$current_opm'(X,Y,Z,M).
'$current_opm'(X,Y,Z,M) :-
	'$do_current_op'(X,Y,Z,M).

'$do_current_op'(X,Y,Z,M) :-
	nonvar(Y),
	\+ '$associativity'(Y),
	throw_error(domain_error(operator_specifier,Y),current_op(X,Y,M:Z)).
'$do_current_op'(X,Y,Z,M) :-
	atom(Z), !,
	'$current_atom_op'(Z, M1, Prefix, Infix, Posfix),
	( M1 = prolog -> true ; M1 = M ),
	(
	 '$get_prefix'(Prefix, X, Y)
	;
	 '$get_infix'(Infix, X, Y)
	;
	 '$get_posfix'(Posfix, X, Y)
	).
'$do_current_op'(X,Y,Z,M) :-
	'$current_op'(Z, M1, Prefix, Infix, Posfix),
	( M1 = prolog -> true ; M1 = M ),
	(
	 '$get_prefix'(Prefix, X, Y)
	;
	 '$get_infix'(Infix, X, Y)
	;
	 '$get_posfix'(Posfix, X, Y)
	).

'$get_prefix'(Prefix, X, Y) :-
	Prefix > 0,
	X is Prefix /\ 0xfff,
	(
         0x2000 /\ Prefix =:= 0x2000
        ->
         Y = fx
        ;
         Y = fy
        ).

'$get_infix'(Infix, X, Y) :-
	Infix > 0,
	X is Infix  /\ 0xfff,
	(
         0x3000 /\ Infix =:= 0x3000
        ->
         Y = xfx
	;
         0x1000 /\ Infix =:= 0x1000
        ->
         Y = xfy
        ;
         Y = yfx
        ).

'$get_posfix'(Posfix, X, Y) :-
	Posfix > 0,
	X is Posfix /\ 0xfff,
	(
         0x1000 /\ Posfix =:= 0x1000
        ->
         Y = xf
	;
         Y = yf
        ).


%% @}