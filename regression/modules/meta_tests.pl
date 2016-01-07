
:- use_module(library(lists)).

:- meta_predicate( a(0) ).

:- dynamic a/1, b/1, c/1,  d:bd/1, g:c/1, b/0, gflags:flag_define/6.

a(1).
b(1).

test membe,
      meta_expand( lists:membe(X,[X|Y]), Clause )

      returns

      Clause =@=  ( lists:membe(X,[X|Y]) ).

test a,
      meta_expand( ( a(X) :- call(X) ) , Clause )

      returns

      Clause =@=  ( a(X) :- call(X) ).

test b,
      meta_expand( ( a(X) :- b(Y), call(X, Y) ), Clause )

      returns

      Clause =@=  ( a(X) :- b(Y), call(X, Y)  ).

test c,
      meta_expand( ( a(X) :- b(Y), call( Y) ), Clause )

      returns

      Clause =@=  ( a(X) :- b(Y), call(user:Y) ).


test d,
      meta_expand( ( c(X) :- call(X) ) , Clause)

      returns

      Clause =@=  ( c(X) :- call(user:X) ).

test e,
      meta_expand( ( c(X) :- b(Y), call(X, Y) ), Clause )

      returns

      Clause =@=  ( c(X) :- b(Y), call(user:X, Y) ).

test f,
      meta_expand( ( c(X) :- b(Y), call( Y) ), Clause )

      returns
      Clause =@=  ( c(X) :- b(Y), call(user:Y) ).

test g,
      meta_expand( ( a(X) :- b(Y), setof( A, X, L) ), Clause )

      returns
      Clause =@=  ( a(X) :- b(Y), setof( A, X, L) ).

test h,
      meta_expand( ( a(X) :- b(Y), setof( A, call(X), L) ), Clause )

      returns
      Clause =@=  ( a(X) :- b(Y), setof( A, user:call(X), L) ).


test i,
      meta_expand( ( c(X) :- b(Y), setof( A, X, L) ), Clause )

      returns
      Clause =@=   ( c(X) :- b(Y), setof( A, user:X, L) ).

test j,
      meta_expand( ( c(X) :- b(Y), setof( A, call(X), L) ), Clause )

      returns
      Clause =@=   ( c(X) :- b(Y), setof( A, user:call(X), L) ).

test k,
      meta_expand( ( c(X) :- b(Y), setof( A, X, L) ), Clause )

      returns
     Clause =@=   ( c(X) :- b(Y), setof( A, user:X, L) ).

test l,
      meta_expand( ( c(X) :- b(Y), setof( A, call(Y), L) ), Clause )

      returns
      Clause =@=   ( c(X) :- b(Y), setof( A, user:call(Y), L) ).

test m,
      meta_expand( ( a(X) :- b(Y), setof( A, Y, L) ), Clause )

      returns
      Clause =@=   ( a(X) :- b(Y), setof( A, user:Y, L) ).

test n,
      meta_expand( ( a(X) :- b(Y), setof( A, call(X), L) ), Clause )

      returns
      Clause =@=   ( a(X) :- b(Y), setof( A, user:call(X), L) ).


test o,
      meta_expand( ( a(X, L) :- append(X, L) ), Clause )

      returns
      Clause =@=   ( a(X, L) :- lists:append(X, L) ).


test  0,
      meta_expand( ( a:(d:b0(X)) :-
                    g:c(X),
                    d(X),
                    user:hello(X,Y) ), Clause )
      returns
       Clause =@= ( d:b0(X) :-
                 g:c(X),
                 user:d(X), 
                 user:hello(X,Y) ).

