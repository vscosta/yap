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
* File:		arithpreds.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	arithmetical predicates				 *
*									 *
*************************************************************************/


/**
  @file arithpreds.yap

  @addtogroup arithmetic_preds

 @{
*/

:- system_module(arithmetic_predicates, [
        plus/3,
        succ/2], []).

:- use_system_module( '$_errors', ['$do_error'/2]).

/** @pred succ(? _Int1_:int, ? _Int2_:int) is det
  *

  True if  _Int2_ =  _Int1_ + 1 and  _Int1_ \>= 0. At least
  one of the arguments must be instantiated to a natural number. This
  predicate raises the domain-error not_less_than_zero if called with
  a negative integer. E.g. `succ(X, 0)` fails silently and `succ(X, -1)`
  raises a domain-error. The behaviour to deal with natural numbers
  only was defined by Richard O'Keefe to support the common
  count-down-to-zero in a natural way.

  */

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

/** @pred plus(? _Int1_:int, ? _Int2_:int, ? _Int3_:int) is det

  True if  _Int3_ =  _Int1_ +  _Int2_. At least two of the
  three arguments must be instantiated to integers.

  @}

  */

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
