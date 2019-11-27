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
 * File:		utils.yap						 *
 * Last rev:	8/2/88							 *
 * mods:									 *
 * comments:	Some utility predicates available in yap		 *
 *									 *
 *************************************************************************/

/**
  * @file   pl/utils.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 12:21:01 2017
  * 
  * @brief  Utilities
  *
  * @defgroup MixBag Diverse Utilities
  * @ingroup builtin
  * 
  * 
*/

:- system_module( '$_utils', [callable/1,
        current_op/3,
        nb_current/2,
        nth_instance/3,
        nth_instance/4,
        op/3,
        prolog/0,
        recordaifnot/3,
        recordzifnot/3,
        simple/1,
        subsumes_term/2], ['$getval_exception'/3]).

:- use_system_module( '$_boot', ['$live'/0]).

:- use_system_module( '$_errors', ['$do_error'/2]).


/** @pred op(+ _P_,+ _T_,+ _A_) is iso


Defines the operator  _A_ or the list of operators  _A_ with type
 _T_ (which must be one of `xfx`, `xfy`,`yfx`,
`xf`, `yf`, `fx` or `fy`) and precedence  _P_
(see appendix iv for a list of predefined operators).

Note that if there is a preexisting operator with the same name and
type, this operator will be discarded. Also, `,` may not be defined
as an operator, and it is not allowed to have the same for an infix and
a postfix operator.


*/
 op(P,T,V) :-
	 '$check_op'(P,T,V,op(P,T,V)),
	 '$op'(P, T, V).

% just check the operator declarations for correctness.
'$check_op'(P,T,Op,G) :-
	( var(P) ; var(T); var(Op)), !,
	'$do_error'(instantiation_error,G).
'$check_op'(P,_,_,G) :-
	\+ integer(P), !,
	'$do_error'(type_error(integer,P),G).
'$check_op'(P,_,_,G) :-
	P < 0, !,
	'$do_error'(domain_error(operator_priority,P),G).
'$check_op'(_,T,_,G) :-
	\+ atom(T), !,
	'$do_error'(type_error(atom,T),G).
'$check_op'(_,T,_,G) :-
	\+  '$associativity'(T), !,
	'$do_error'(domain_error(operator_specifier,T),G).
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
	'$do_error'(type_error(atom,V),G).

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
	'$do_error'(instantiation_error,G).
'$check_module_for_op'(M:_V, G, _) :-
	var(M), !,
	'$do_error'(instantiation_error,G).
'$check_module_for_op'(M:V, G, NV) :-
	atom(M), !,
	'$check_module_for_op'(V, G, NV).
'$check_module_for_op'(M:_V, G, _) :- !,
	'$do_error'(type_error(atom,M),G).
'$check_module_for_op'(V, _G, V).

'$check_ops'(_P, _T, [], _G) :- !.
'$check_ops'(P, T, [Op|NV], G) :- !,
	(
	 var(NV)
	->
	 '$do_error'(instantiation_error,G)
	;
	 '$check_module_for_op'(Op, G, NOp),
	 '$check_op_name'(P, T, NOp, G),
	 '$check_ops'(P, T, NV, G)
	).
'$check_ops'(_P, _T, Ops, G) :-
	'$do_error'(type_error(list,Ops),G).

'$check_op_name'(_,_,V,G) :-
	  var(V), !,
	  '$do_error'(instantiation_error,G).
 '$check_op_name'(_,_,',',G) :- !,
	  '$do_error'(permission_error(modify,operator,','),G).
'$check_op_name'(_,_,'[]',G) :-  T \= yf, T\= xf, !,
	  '$do_error'(permission_error(create,operator,'[]'),G).
'$check_op_name'(_,_,'{}',G) :- T \= yf, T\= xf, !,
	  '$do_error'(permission_error(create,operator,'{}'),G).
'$check_op_name'(P,T,'|',G) :-
	 (
	  integer(P),
	  P < 1001, P > 0
	 ;
	  atom_codes(T,[_,_])
	 ), !,
	 '$do_error'(permission_error(create,operator,'|'),G).
'$check_op_name'(_,_,V,_) :-
	 atom(V), !.
'$check_op_name'(_,_,A,G) :-
	 '$do_error'(type_error(atom,A),G).

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
	'$opdec'(P,T,A,prolog).
'$op2'(P,T,A) :-
	strip_module(A,M,N),
	'$opdec'(P,T,N,M).

/** @pred current_op( _P_, _T_, _F_) is iso


Defines the relation:  _P_ is a currently defined  operator of type
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
	'$do_error'(domain_error(operator_specifier,Y),current_op(X,Y,M:Z)).
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
	'$do_error'(domain_error(operator_specifier,Y),current_op(X,Y,M:Z)).
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


prolog :-
	live.

%%% current ....

/** @pred  callable( _T_) is iso


Checks whether  _T_ is a callable term, that is, an atom or a
compound term.


*/
callable(A) :-
	( var(A) -> fail ; number(A) -> fail ; true ).

/** @pred  simple( _T_)


Checks whether  _T_ is unbound, an atom, or a number.


*/
simple(V) :- var(V), !.
simple(A) :- atom(A), !.
simple(N) :- number(N).

/** @pred  nth_instance(? _Key_,? _Index_,? _R_)


Fetches the  _Index_nth entry in the internal database under the key
 _Key_. Entries are numbered from one. If the key  _Key_ or the
 _Index_ are bound, a reference is unified with  _R_. Otherwise,
the reference  _R_ must be given, and YAP will find
the matching key and index.


*/
nth_instance(Key,Index,Ref) :-
	nonvar(Key), var(Index), var(Ref), !,
	recorded(Key,_,Ref),
	'$nth_instance'(_,Index,Ref).
nth_instance(Key,Index,Ref) :-
	'$nth_instance'(Key,Index,Ref).

/** @pred  nth_instance(? _Key_,? _Index_, _T_,? _R_)

Fetches the  _Index_nth entry in the internal database under the key
 _Key_. Entries are numbered from one. If the key  _Key_ or the
 _Index_ are bound, a reference is unified with  _R_. Otherwise,
the reference  _R_ must be given, and YAP will find
the matching key and index.


*/
nth_instance(Key,Index,T,Ref) :-
	nonvar(Key), var(Index), var(Ref), !,
	recorded(Key,T,Ref),
	'$nth_instance'(_,Index,Ref).
nth_instance(Key,Index,T,Ref) :-
	'$nth_instance'(Key,Index,Ref),
	instance(Ref,T).

/** @pred  nb_current(? _Name_, ? _Value_)


Enumerate all defined variables with their value. The order of
enumeration is undefined.


*/
/** @pred nb_current(? _Name_,? _Value_)


Enumerate all defined variables with their value. The order of
enumeration is undefined.


*/
nb_current(GlobalVariable, Val) :-
	'$nb_current'(GlobalVariable),
	'__NB_getval__'(GlobalVariable, Val, _).

'$getval_exception'(GlobalVariable, _Val, Caller) :-
	user:exception(undefined_global_variable, GlobalVariable, Action),
	!,
	(
	 Action == fail
	->
	 fail
	;
	 Action == retry
	->
	 true
	;
	 Action == error
	->
	 '$do_error'(existence_error(variable, GlobalVariable),Caller)
	;
	 '$do_error'(type_error(atom, Action),Caller)
	).


/** @pred  subsumes_term(? _Subsumer_, ? _Subsumed_)



Succeed if  _Submuser_ subsumes  _Subsuned_ but does not bind any
variable in  _Subsumer_.


*/
subsumes_term(A,B) :-
	\+ \+ terms:subsumes(A,B).

term_string( T, S, Opts) :-
    var( T ),
    !,
    memory_file:open_mem_read_stream( S, Stream ),
    read_term( Stream, T, Opts ),
    close( Stream ).

%term_string( T, S, _Opts) :-
%        format(string(S), '~q.~n', [T]).


