%%  Assumption:
%%    h(F) = h(G) where F and G are variants and h is the hash function

% In YAP use the standard routines:

$pp_hashtable_get(T,K,V) :- hashtable_get(T,K,V).
$pp_hashtable_put(T,K,V) :- hashtable_put(T,K,V).

/****** vsc: commented out for YAP

$pp_hashtable_get(T,K,V), T = $hshtb(_,_) => hashtable_get(T,K,V).
$pp_hashtable_get(T,_,_) => $pp_hashtable_throw(T,$pp_hashtable_get/3).

$pp_hashtable_put(T,K,V), T = $hshtb(N0,A) =>
    hash_code(K,H),
    functor(A,_,M),
    I is (H mod M) + 1,
    arg(I,A,L),
    member(KV,L),
    ( var(KV) ->
        KV = (K = V),
        N1 is N0 + 1,
        setarg(1,T,N1),
        ( N1 > 2 * M + 1, M < 32700 -> $hashtable_expand_buckets(T)
        ; true                     % #buckets should not exceed 65536
        )
    ; KV = (Key = _),
      variant(Key,K) -> setarg(2,KV,V)
    ), !.
$pp_hashtable_put(T,_,_) =>
    $pp_hashtable_throw(T,$pp_hashtable_put/3).

*/

$pp_hashtable_throw(T,Source) :-
    ( nonvar(T) -> true
    ; throw(error(instantiation_error,Source))
    ),
    ( T ?= $hshtb(_,_) -> true
    ; throw(error(type_error(hashtable,T),Source))
    ), !,
    fail.                       % should not reach here
