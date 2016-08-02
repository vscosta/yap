% taken from http://stackoverflow.com/questions/12272888/default-prolog-predicate-sort
/*  File:    order_by.pl
    Author:  Carlo,,,
    Created: Sep  5 2012
    Purpose: sort fact
*/
:- module(order_by,
      [order_by/2
      ]).

:- use_module(library(apply_macros)).

order_by(PredicateIndicator, Argument) :-
    (   PredicateIndicator = Module:Functor/Arity
    ;   PredicateIndicator = Functor/Arity, Module = user
    ),
    length(EmptyArgs, Arity),
    P =.. [Functor|EmptyArgs],
    findall(P, retract(Module:P), L),
    predsort(by_arg(Argument), L, S),
    maplist(assert_in_module(Module), S).

assert_in_module(Module, P) :-
    assertz(Module:P).

by_arg(Argument, Delta, E1, E2) :-
    arg(Argument, E1, A1),
    arg(Argument, E2, A2),
    (   A1 @< A2
    ->  Delta = <
    ;   Delta = >
    ).
