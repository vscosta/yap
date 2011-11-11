%%----------------------------------------

%
% vsc: done in prism.yap
% :- random_set_seed.

%%----------------------------------------

random_get_seed(Seed) :-
    global_get($pg_random_seed,Seed),!.

random_set_seed :-
    $pc_random_auto_seed(Seed),
    random_set_seed(Seed),!.

random_set_seed(Seed) :-
    $pp_require_random_seed(Seed,$msg(2000),random_set_seed/1),
    ( integer(Seed) -> $pc_random_init_by_seed(Seed)
    ; Seed ?= [_|_] -> $pc_random_init_by_list(Seed)
    ; %% else
      $pp_assert(fail)
    ), !,
    global_set($pg_random_seed,Seed),!.

random_get_state(State) :-
    $pc_random_get_state(State),!.

random_set_state(State) :-
    $pp_require_random_state(State,$msg(2001),random_set_state/1),
    $pc_random_set_state(State),!.

% deprecated predicates:

set_seed(Seed) :-
    $pp_raise_warning($msg(3300),[set_seed/1,random_set_seed/1]),
    random_set_seed(Seed).

set_seed_time :-
    $pp_raise_warning($msg(3300),[set_seed_time/0,random_set_seed/0]),
    random_set_seed.

set_seed_time(Seed) :-
    $pp_raise_warning($msg(3301),[set_seed_time/1]),
    random_set_seed,
    random_get_seed(Seed).

%%----------------------------------------

random_int(Max,Value) :-
    $pp_require_positive_integer(Max,$msg(2002),random_int/2),
    $pc_random_int(Max,Value).

random_int(Min,Max,Value) :-
    $pp_require_integer(Min,$msg(2003),random_int/3),
    $pp_require_integer(Max,$msg(2004),random_int/3),
    $pp_require_integer_range(Min,Max,$msg(2008),random_int/3),
    Max1 is Max - 1,
    $pc_random_int(Min,Max1,Value).

random_int_incl(Min,Max,Value) :-
    $pp_require_integer(Min,$msg(2003),random_int_incl/3),
    $pp_require_integer(Max,$msg(2004),random_int_incl/3),
    $pp_require_integer_range_incl(Min,Max,$msg(2008),random_int/3),
    $pc_random_int(Min,Max,Value).


random_int_excl(Min,Max,Value) :-
    $pp_require_integer(Min,$msg(2003),random_int_excl/3),
    $pp_require_integer(Max,$msg(2004),random_int_excl/3),
    $pp_require_integer_range_excl(Min,Max,$msg(2008),random_int/3),
    Min1 is Min + 1,
    Max1 is Max - 1,
    $pc_random_int(Min1,Max1,Value).

%%----------------------------------------

random_uniform(Value) :-
    $pc_random_float(Value).

random_uniform(Max,Value) :-
    $pp_require_positive_number(Max,$msg(2005),random_uniform/2),
    $pc_random_float(Value0),
    Value is Value0 * Max.

random_uniform(Min,Max,Value) :-
    $pp_require_number(Min,$msg(2006),random_uniform/3),
    $pp_require_number(Max,$msg(2007),random_uniform/3),
    $pp_require_number_range_excl(Min,Max,$msg(2008),random_uniform/3),
    $pc_random_float(Value0),
    Value is Value0 * (Max - Min) + Min.

random_gaussian(Value) :-
    $pc_random_gaussian(Value).

random_gaussian(Mu,Sigma,Value) :-
    $pp_require_number(Mu,$msg(2009),random_gaussian/3),
    $pp_require_positive_number(Sigma,$msg(2010),random_gaussian/3),
    $pc_random_gaussian(Value0),
    Value is Value0 * Sigma + Mu.

%%----------------------------------------

random_select(List,Value) :-
    random_select(List,uniform,Value).

random_select(List,Dist,Value) :-
    $pp_require_list(List,$msg(2011),random_select/3),
    $pp_require_distribution(Dist,$msg(0200),random_select/3),
    expand_values(List,List1),
    length(List1,L1),
    $pp_spec_to_ratio(Dist,L1,Ratio,random_select/3),
    length(Ratio,L2),
    ( L1 is L2 -> true
    ; $pp_raise_runtime_error($msg(0210),[List,Dist],unmatched_distribution,
                              random_select/3)
    ),
    sumlist(Ratio,Sum),
    random_uniform(Sum,Rand),!,
    $pp_random_select(Ratio,List1,Rand,Value).

$pp_random_select([X|Xs],[Y|Ys],R,Value) :-
    ( R >= X, Xs ?= [_|_] ->
      R1 is R - X, !, $pp_random_select(Xs,Ys,R1,Value)
    ; Y = Value
    ),!.

% deprecated predicates:

dice(List,Value) :-
    $pp_raise_warning($msg(3300),[dice/2,random_select/2]),
    random_select(List,Value).

dice(List,Dist,Value) :-
    $pp_raise_warning($msg(3300),[dice/3,random_select/3]),
    random_select(List,Dist,Value).

%%----------------------------------------

random_multiselect(List,N,Result) :-
    $pp_require_list(List,$msg(2011),random_multiselect/3),
    $pp_require_integer(N,$msg(2012),random_multiselect/3),
    length(List,L),
    ( \+ ( 1 =< N, N =< L ) ->
      $pp_raise_runtime_error($msg(2013),[N],
                              invalid_argument,random_multiselect/3)
    ; true
    ), !,
    new_bigarray(Elems,L),
    new_bigarray(Flags,L),
    $pp_random_multiselect1(1,L,Elems,Flags),
    M is L - N,
    ( N =< M ->
      $pp_random_multiselect2(1,N,L,Elems,Flags),
      $pp_random_multiselect3(1,1,Flags,List,Result)
    ; $pp_random_multiselect2(1,M,L,Elems,Flags),
      $pp_random_multiselect3(1,0,Flags,List,Result)
    ).

$pp_random_multiselect1(K,L,_,_), K > L =>
    true.
$pp_random_multiselect1(K,L,Elems,Flags), K =< L =>
    bigarray_put(Elems,K,K),
    bigarray_put(Flags,K,0),
    K1 is K + 1, !,
    $pp_random_multiselect1(K1,L,Elems,Flags).

$pp_random_multiselect2(K,N,_,_,_), K > N =>
    true.
$pp_random_multiselect2(K,N,L,Elems,Flags), K =< N =>
    random_int_incl(K,L,J),
    bigarray_get(Elems,K,VK),
    bigarray_get(Elems,J,VJ),
    bigarray_put(Elems,J,VK),
    bigarray_put(Elems,K,VJ),
    bigarray_put(Flags,VJ,1),
    K1 is K + 1, !,
    $pp_random_multiselect2(K1,N,L,Elems,Flags).

$pp_random_multiselect3(_,_,_,Xs,Ys), Xs = [] =>
    Ys = [].
$pp_random_multiselect3(K,Query,Flags,Xs,Ys), Xs = [X|Xs1] =>
    ( bigarray_get(Flags,K,Query) -> Ys = [X|Ys1] ; Ys = Ys1 ),
    K1 is K + 1, !,
    $pp_random_multiselect3(K1,Query,Flags,Xs1,Ys1).

%%----------------------------------------

random_group(List,N,Result) :-
    $pp_require_list(List,$msg(2011),random_group/3),
    $pp_require_positive_integer(N,$msg(2014),random_group/3),
    List = List1,
    new_bigarray(Array,N),
    $pp_random_group1(1,N,Array),
    $pp_random_group2(List1,N,Array),
    $pp_random_group3(1,N,Array,Result).

$pp_random_group1(K,N,_), K > N =>
    true.
$pp_random_group1(K,N,Array), K =< N =>
    bigarray_put(Array,K,Xs-Xs),
    K1 is K + 1, !,
    $pp_random_group1(K1,N,Array).

$pp_random_group2(Xs,_,_), Xs = [] =>
    true.
$pp_random_group2(Xs,N,Array), Xs = [X|Xs1] =>
    $pc_random_int(N,Z0),
    Z is Z0 + 1,
    bigarray_get(Array,Z,Ys0-Ys1),
    Ys1 = [X|Ys2],
    bigarray_put(Array,Z,Ys0-Ys2), !,
    $pp_random_group2(Xs1,N,Array).

$pp_random_group3(K,N,_,Xs), K > N =>
    Xs = [].
$pp_random_group3(K,N,Array,Xs), K =< N =>
    bigarray_get(Array,K,X-[]),
    Xs = [X|Xs1],
    K1 is K + 1, !,
    $pp_random_group3(K1,N,Array,Xs1).

%%----------------------------------------

random_shuffle(List0,List) :-
    $pp_require_list(List0,$msg(2011),random_shuffle/3),
    list_to_bigarray(List0,Array),
    bigarray_length(Array,Size),
    $pp_random_shuffle(1,Size,Array),
    bigarray_to_list(Array,List).

$pp_random_shuffle(K,N,_), K > N =>
    true.
$pp_random_shuffle(K,N,Array), K =< N =>
    random_int_incl(K,N,J),
    bigarray_get(Array,K,VK),
    bigarray_get(Array,J,VJ),
    bigarray_put(Array,J,VK),
    bigarray_put(Array,K,VJ),
    K1 is K + 1, !,
    $pp_random_shuffle(K1,N,Array).

%%----------------------------------------

$pp_require_random_seed(X,ID,Source) :-
    ( $pp_test_random_seed(X) -> true
    ; $pp_raise_on_require([X],ID,Source,$pp_error_random_seed)
    ).

$pp_test_random_seed(X), integer(X) => true.
$pp_test_random_seed(X), X = [Y],   integer(Y) => true.
$pp_test_random_seed(X), X = [Y|Z], integer(Y) =>
    Z ?= [_|_],
    $pp_test_random_seed(Z).

$pp_error_random_seed(X,instantiation_error) :-
    \+ ground(X), !.
$pp_error_random_seed(X,domain_error(random_seed,X)) :-
    \+ $pp_test_random_seed(X), !.

%%----------------------------------------

$pp_require_random_state(X,ID,Source) :-
    ( $pp_test_random_state(X) ->
      true
    ; $pp_raise_on_require([X],ID,Source,$pp_error_random_state)
    ).

$pp_test_random_state(X) :-
    functor(X,$randstate,833),
    $pp_test_random_state(X,1).

$pp_test_random_state(_,N), N >  833 => true.
$pp_test_random_state(X,N), N =< 833 =>
    arg(N,X,Arg),
    integer(Arg),
    N1 is N + 1,
    $pp_test_random_state(X,N1).

$pp_error_random_state(X,instantiation_error) :-
    \+ ground(X), !.
$pp_error_random_state(X,type_error(compound,X)) :-
    \+ compound(X), !.
$pp_error_random_state(X,domain_error(random_state,X)) :-
    \+ $pp_test_random_state(X), !.

%%----------------------------------------
