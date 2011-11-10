%%----------------------------------------

expand_probs(Dist,Probs) :-
    $pp_expand_probs(Dist,Probs,expand_probs/2).

expand_probs(Dist,N,Probs) :-
    $pp_expand_probs(Dist,N,Probs,expand_probs/3).

$pp_expand_probs(Dist,Probs,Source) :-
    $pp_require_fixed_size_distribution(Dist,$msg(0200),Source),
    $pp_spec_to_ratio(Dist,_,Ratio,Source),
    $pp_normalize_ratio(Ratio,Probs).

$pp_expand_probs(Dist,N,Probs,Source) :-
    $pp_require_distribution(Dist,$msg(0200),Source),
    $pp_require_positive_integer(N,$msg(0204),Source),
    $pp_spec_to_ratio(Dist,N,Ratio,Source),
    $pp_check_expanded_prob_size(Ratio,N,Source),
    $pp_normalize_ratio(Ratio,Probs).

$pp_normalize_ratio(Ratio,Probs) :-
    sumlist(Ratio,Denom),
    $pp_ratio_to_probs(Ratio,Denom,Probs).

$pp_ratio_to_probs([],_,[]) :- !.
$pp_ratio_to_probs([X|Xs],Denom,[Y|Ys]) :-
    Y is X / Denom,!,
    $pp_ratio_to_probs(Xs,Denom,Ys).

$pp_check_expanded_prob_size(List,N,Source) :-
    length(List,N1),
    ( N = N1 -> true
    ; $pp_raise_runtime_error($msg(0211),[List,N],unmatched_distribution,
                              Source)
    ),!.

%%----------------------------------------

$pp_spec_to_ratio(Dist,N,Ratio,Source) :-
    ( Dist = default,
      get_prism_flag(default_sw,none)
          -> $pp_raise_runtime_error($msg(0202),
                                     default_distribution_unavailable,
                                     Source)
    ; true
    ),
    $pp_spec_to_ratio1(Dist,N,Ratio,Source).

$pp_spec_to_ratio1(Dist,_N,Ps,_Source), Dist = [_|_] => Ps = Dist.

$pp_spec_to_ratio1(Dist,_N,Ps,_Source), Dist = (_+_) =>
    $pp_expr_to_list('+',Dist,Ps).

$pp_spec_to_ratio1(Dist,_N,Ratio,_Source), Dist = (_:_) =>
    $pp_expr_to_list(':',Dist,Ratio).

$pp_spec_to_ratio1(uniform,N,Ratio,_Source) =>
    $pp_gen_geom_list(N,1,1,Ratio).

$pp_spec_to_ratio1(f_geometric,N,Ratio,_Source) =>
    $pp_spec_to_ratio_fgeom(2,desc,N,Ratio).

$pp_spec_to_ratio1(f_geometric(Base),N,Ratio,_Source) =>
    $pp_spec_to_ratio_fgeom(Base,desc,N,Ratio).

$pp_spec_to_ratio1(f_geometric(Base,Type),N,Ratio,_Source) =>
    $pp_spec_to_ratio_fgeom(Base,Type,N,Ratio).

$pp_spec_to_ratio1(random,N,Ratio,_Source) =>
    $pp_gen_rand_list(N,Ratio).

$pp_spec_to_ratio1(default,N,Ratio,Source) =>
    get_prism_flag(default_sw,Flag),
    $pp_require_distribution(Flag,$msg(0200),Source),!,
    $pp_spec_to_ratio1(Flag,N,Ratio,Source).

%%----------------------------------------

expand_pseudo_counts(Spec,Cs) :-
    $pp_require_fixed_size_hyperparameters(Spec,$msg(0201),
                                           expand_pseudo_counts/2),
    $pp_expand_pseudo_counts(Spec,_,Cs,expand_pseudo_counts/2).

expand_pseudo_counts(Spec,N,Cs) :-
    Source = expand_pseudo_counts/3,
    $pp_require_hyperparameters(Spec,$msg(0201),Source),
    $pp_require_positive_integer(N,$msg(0204),Source),
    $pp_expand_pseudo_counts(Spec,N,Cs,Source),
    $pp_check_expanded_pseudo_count_size(Cs,N,Source).

$pp_expand_pseudo_counts(Spec,N,Cs,Source) :-
    ( Spec = default,
      $pp_get_default_pseudo_counts(none)
          -> $pp_raise_runtime_error($msg(0202),
                                     default_hyperparameters_unavailable,
                                     Source)
    ; true
    ),
    $pp_spec_to_pseudo_counts(Spec,N,Cs,Source).

$pp_spec_to_pseudo_counts(Spec,_N,Cs,_Source), Spec = [_|_] =>  Cs = Spec.

$pp_spec_to_pseudo_counts(Spec,N,Cs,_Source), number(Spec) =>
    C = Spec,
    $pp_gen_dup_list(N,C,Cs).

$pp_spec_to_pseudo_counts(uniform,N,Cs,Source) =>
    $pp_spec_to_pseudo_counts(uniform(1.0),N,Cs,Source).

$pp_spec_to_pseudo_counts(uniform(U),N,Cs,_Source) =>
    C is U / N,
    $pp_gen_dup_list(N,C,Cs).

$pp_spec_to_pseudo_counts(f_geometric,N,Cs,Source) =>
    $pp_spec_to_pseudo_counts(f_geometric(1.0,2.0,desc),N,Cs,Source).

$pp_spec_to_pseudo_counts(f_geometric(Base),N,Cs,Source) =>
    $pp_spec_to_pseudo_counts(f_geometric(1.0,Base,desc),N,Cs,Source).

$pp_spec_to_pseudo_counts(f_geometric(Init,Base),N,Cs,Source) =>
    $pp_spec_to_pseudo_counts(f_geometric(Init,Base,desc),N,Cs,Source).

$pp_spec_to_pseudo_counts(f_geometric(Init,Base,Type),N,Cs,_Source) =>
    $pp_spec_to_ratio_fgeom(Init,Base,Type,N,Cs).

$pp_spec_to_pseudo_counts(default,N,Cs,Source) =>
    $pp_get_default_pseudo_counts(Spec),   % get hyperparameters anyway
    $pp_require_hyperparameters(Spec,$msg(0201),Source),!,
    $pp_spec_to_pseudo_counts(Spec,N,Cs,Source).

$pp_get_default_pseudo_counts(Spec) :-
    ( get_prism_flag(default_sw_a,$disabled) ->
          get_prism_flag(default_sw_d,Spec)
    ; get_prism_flag(default_sw_a,Spec)
    ).

$pp_check_expanded_pseudo_count_size(List,N,Source) :-
    length(List,N1),
    ( N = N1 -> true
    ; $pp_raise_runtime_error($msg(0211),[List,N],unmatched_pseudo_counts,
                              Source)
    ),!.

%%----------------------------------------

$pp_spec_to_ratio_fgeom(Base,Type,N,Ratio) :-
    $pp_spec_to_ratio_fgeom(1.0,Base,Type,N,Ratio).

$pp_spec_to_ratio_fgeom(Init,Base,Type,N,Ratio) :-
    $pp_gen_geom_list(N,Init,Base,Ratio0),
    ( Type == asc -> Ratio0 = Ratio ; reverse(Ratio0,Ratio) ).

%%----------------------------------------

$pp_expr_to_list(Op,Expr,List) :-
    current_op(_,yfx,Op),!,
    $pp_expr_to_list_yfx(Op,Expr,List,[]).
$pp_expr_to_list(Op,Expr,List) :-
    current_op(_,xfy,Op),!,
    $pp_expr_to_list_xfy(Op,Expr,List,[]).

$pp_expr_to_list_yfx(Op,Expr,L0,L1), functor(Expr,Op,2) =>
    Expr =.. [Op,Expr1,X],
    L2 = [X|L1], !,
    $pp_expr_to_list_yfx(Op,Expr1,L0,L2).
$pp_expr_to_list_yfx(_ ,Expr,L0,L1) =>
    L0 = [Expr|L1].

$pp_expr_to_list_xfy(Op,Expr,L0,L1), functor(Expr,Op,2) =>
    Expr =.. [Op,X,Expr1],
    L0 = [X|L2], !,
    $pp_expr_to_list_xfy(Op,Expr1,L2,L1).
$pp_expr_to_list_xfy(_ ,Expr,L0,L1) =>
    L0 = [Expr|L1].

%%----------------------------------------

$pp_gen_geom_list(0,_,_,[]) :- !.
$pp_gen_geom_list(N,X,Base,[X|Xs1]) :-
    X1 is X * Base,
    N1 is N - 1,!,
    $pp_gen_geom_list(N1,X1,Base,Xs1).

$pp_gen_rand_list(0,[]) :- !.
$pp_gen_rand_list(N,[X|Xs1]) :-
    random_uniform(X),
    N1 is N - 1,!,
    $pp_gen_rand_list(N1,Xs1).

$pp_gen_dup_list(0,_,[]) :- !.
$pp_gen_dup_list(N,C,[C|Cs]) :-
    N1 is N - 1,!,
    $pp_gen_dup_list(N1,C,Cs).
