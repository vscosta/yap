%%%%
%%%%  bigarray.pl -- A large one-dimensional array for B-Prolog
%%%%

%%----------------------------------------

$pp_bigarray_unit(65535).        % max_arity

%%----------------------------------------

new_bigarray(Array,N), var(Array), integer(N), N > 0 =>
    $pp_bigarray_unit(M),
    Array = $bigarray(N,Body),
    $pp_new_bigarray(Body,N,M).

new_bigarray(Array,N) =>
    $pp_new_bigarray_throw(Array,N).

$pp_new_bigarray_throw(Array,N) :-
    ( var(Array) -> true
    ; throw(error(type_error(variable,Array),new_bigarray/2))
    ),
    ( nonvar(N) -> true
    ; throw(error(instantiation_error,new_bigarray/2))
    ),
    ( integer(N) -> true
    ; throw(error(type_error(integer,N),new_bigarray/2))
    ),
    ( N > 0 -> true
    ; throw(error(domain_error(greater_than_zero,N),new_bigarray/2))
    ), !,
    fail.                       % should not reach here

$pp_new_bigarray(Body,N,M), N =< M =>
    functor(Body,array,N).

$pp_new_bigarray(Body,N,M), N > M =>
    L is (N - 1) // M + 1,
    functor(Body,outer,L),
    $pp_new_bigarray(Body,1,N,M).

$pp_new_bigarray(Body,K,N,M), N =< M =>
    arg(K,Body,SubBody),
    functor(SubBody,array,N).

$pp_new_bigarray(Body,K,N,M), N > M =>
    arg(K,Body,SubBody),
    functor(SubBody,array,M),
    K1 is K + 1,
    N1 is N - M, !,
    $pp_new_bigarray(Body,K1,N1,M).

%%----------------------------------------

is_bigarray(Array), Array = $bigarray(_,_) => true.

bigarray_length(Array,L), Array = $bigarray(N,_) => L = N.
bigarray_length(Array,_) =>
    $pp_bigarray_length_throw(Array).

$pp_bigarray_length_throw(Array) :-
    ( nonvar(Array) -> true
    ; throw(error(instantiation_error,bigarray_length/2))
    ),
    ( Array ?= $bigarray(_,_) -> true
    ; throw(error(domain_error(bigarray,Array),bigarray_length/2))
    ), !,
    fail.                   % should not reach here

%%----------------------------------------

bigarray_get(Array,I,Value),
      Array = $bigarray(N,Body),
      integer(I),
      I >= 1,
      I =< N =>
    $pp_bigarray_get(Body,I,Value).

bigarray_get(Array,I,_Value) =>
    $pp_bigarray_access_throw(Array,I,bigarray_get/3).

bigarray_put(Array,I,Value),
      Array = $bigarray(N,Body),
      integer(I),
      I >= 1,
      I =< N =>
    $pp_bigarray_put(Body,I,Value).

bigarray_put(Array,I,_Value) =>
    $pp_bigarray_access_throw(Array,I,bigarray_put/3).

$pp_bigarray_access_throw(Array,I,Source) :-
    ( nonvar(Array) -> true
    ; throw(error(instantiation_error,Source))
    ),
    ( Array = $bigarray(N,_) -> true
    ; throw(error(domain_error(bigarray,Array),Source))
    ),
    ( nonvar(I) -> true
    ; throw(error(instantiation_error,Source))
    ),
    ( integer(I) -> true
    ; throw(error(type_error(integer,I),Source))
    ),
    ( I >= 1, I =< N -> true
    ; throw(error(domain_error(bigarray_index,I),Source))
    ), !,
    fail.                       % should not reach here

$pp_bigarray_get(Body,I,Elem), functor(Body,array,_) =>
    arg(I,Body,Elem).
$pp_bigarray_get(Body,I,Elem), functor(Body,outer,_) =>
    $pp_bigarray_unit(M),
    Q is (I - 1) //  M + 1,
    R is (I - 1) mod M + 1,
    arg(Q,Body,SubBody),
    arg(R,SubBody,Elem).

$pp_bigarray_put(Body,I,Elem), functor(Body,array,_) =>
    setarg(I,Body,Elem).
$pp_bigarray_put(Body,I,Elem), functor(Body,outer,_) =>
    $pp_bigarray_unit(M),
    Q is (I - 1) //  M + 1,
    R is (I - 1) mod M + 1,
    arg(Q,Body,SubBody),
    setarg(R,SubBody,Elem).

%%----------------------------------------

list_to_bigarray(List,Array) :-
    $pp_bigarray_unit(M),
    length(List,N),
    Array = $bigarray(N,Body),
    $pp_new_bigarray(Body,N,M),
    $pp_list_to_bigarray(List,1,Body).

$pp_list_to_bigarray(Xs,_,_), Xs = [] => true.
$pp_list_to_bigarray(Xs,K,Body), Xs = [X|Xs1] =>
    $pp_bigarray_put(Body,K,X),
    K1 is K + 1, !,
    $pp_list_to_bigarray(Xs1,K1,Body).

bigarray_to_list(Array,List), Array = $bigarray(N,Body) =>
    $pp_bigarray_to_list(Body,1,N,List).

$pp_bigarray_to_list(_,K,N,Xs), K > N =>
    Xs = [].
$pp_bigarray_to_list(Body,K,N,Xs), K =< N =>
    $pp_bigarray_get(Body,K,X),
    Xs = [X|Xs1],
    K1 is K + 1, !,
    $pp_bigarray_to_list(Body,K1,N,Xs1).

%%----------------------------------------
