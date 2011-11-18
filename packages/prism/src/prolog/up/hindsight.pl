%%%%
%%%% Hindsight routine with C interface
%%%%

%%
%% hindsight(G,SubG,HProbs) :-
%%   output hindsight probs of subgoals that matches with SubG given G
%%
%% hindsight(G,SubG) :- print hindsight probs of SubG given G
%%

hindsight(G) :- hindsight(G,_).

hindsight(G,SubG) :-
    hindsight(G,SubG,HProbs),
    ( HProbs == [] -> $pp_raise_warning($msg(1404))
    ; format("hindsight probabilities:~n",[]),
      $pp_print_hindsight_probs(HProbs)
    ).

hindsight(G,SubG,HProbs) :-
    $pp_require_tabled_probabilistic_atom(G,$msg(0006),hindsight/3),
    ( nonvar(SubG) -> $pp_require_callable(SubG,$msg(1403),hindsight/3)
    ; true
    ),
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_hindsight_core(G,SubG,HProbs0),
    $pp_sort_hindsight_probs(HProbs0,HProbs),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_assert_hindsight_stats1(InfTime),!.

hindsight_agg(G,Agg) :-
    hindsight_agg(G,Agg,HProbs),
    ( HProbs == [] -> $pp_raise_warning($msg(1404))
    ; format("hindsight probabilities:~n",[]),
      $pp_print_hindsight_probs_agg(HProbs)
    ).

hindsight_agg(G,Agg,HProbs) :-
    $pp_require_tabled_probabilistic_atom(G,$msg(0006),hindsight_agg/3),
    $pp_require_hindsight_aggregate_pattern(Agg,$msg(1402),hindsight_agg/3),
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_get_subgoal_from_agg(Agg,SubG),!,
    $pp_hindsight_core(G,SubG,HProbs0),
    $pp_aggregate_hindsight_probs(Agg,HProbs0,HProbs1),
    $pp_sort_hindsight_probs_agg(HProbs1,HProbs),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_assert_hindsight_stats1(InfTime),!.

$pp_hindsight_core(G,SubG,HProbs) :-
    ground(G),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T0),
    $pp_find_explanations(G),!,
    cputime(T1),
    $pp_compute_hindsight(G,SubG,HProbs),
    cputime(T2),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T1 - T0,
    NumCompTime is T2 - T1,
    $pp_assert_hindsight_stats2(SearchTime,NumCompTime),!.

$pp_hindsight_core(G,SubG,HProbs) :-
    copy_term(G,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    Clause = (DummyGoal:-BodyGoal,
                         $pc_prism_goal_id_register(GoalCp,GId),
                         $pc_prism_goal_id_register(DummyGoal,HId),
                         $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),[Clause]),
            pred('$damon_load',0,_,_,_,[('$damon_load':-true)])],
    consult_preds([],Prog),
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T0),
    $pp_find_explanations(DummyGoal),!,
    cputime(T1),
    $pp_compute_hindsight(DummyGoal,SubG,HProbs),
    cputime(T2),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T1 - T0,
    NumCompTime is T2 - T1,
    $pp_assert_hindsight_stats2(SearchTime,NumCompTime),
    $pp_delete_tmp_out,!.

% Sws = [sw(Id,Instances,Probs,PseudoCs,Fixed,FixedH),...]
$pp_compute_hindsight(Goal,SubG,HProbs) :-
    $pp_collect_sw_info(Sws),
    $pc_export_sw_info(Sws),
    $pc_prism_goal_id_get(Goal,Gid),
    garbage_collect,
    $pc_compute_hindsight(Gid,SubG,0,HProbs0), % "0" indicates "unconditional"
    $pp_decode_hindsight(HProbs0,HProbs),!.

%%
%%  Conditional version of hindsight computation:
%%

chindsight(G) :- chindsight(G,_).

chindsight(G,SubG) :-
    chindsight(G,SubG,HProbs),
    ( HProbs == [] -> $pp_raise_warning($msg(1404))
    ; format("conditional hindsight probabilities:~n",[]),
      $pp_print_hindsight_probs(HProbs)
    ).

chindsight(G,SubG,HProbs) :-
    $pp_require_tabled_probabilistic_atom(G,$msg(0006),chindsight/3),
    ( nonvar(SubG) -> $pp_require_callable(SubG,$msg(1403),chindsight/3)
    ; true
    ),
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_chindsight_core(G,SubG,HProbs0),
    $pp_sort_hindsight_probs(HProbs0,HProbs),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_assert_hindsight_stats1(InfTime),!.

chindsight_agg(G,Agg) :-
    chindsight_agg(G,Agg,HProbs),
    ( HProbs == [] -> $pp_raise_warning($msg(1404))
    ; format("conditional hindsight probabilities:~n",[]),
      $pp_print_hindsight_probs_agg(HProbs)
    ).

chindsight_agg(G,Agg,HProbs) :-
    $pp_require_tabled_probabilistic_atom(G,$msg(0006),chindsight_agg/3),
    $pp_require_hindsight_aggregate_pattern(Agg,$msg(1402),chindsight_agg/3),
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_get_subgoal_from_agg(Agg,SubG),!,
    $pp_chindsight_core(G,SubG,HProbs0),
    $pp_aggregate_hindsight_probs(Agg,HProbs0,HProbs1),
    $pp_sort_hindsight_probs_agg(HProbs1,HProbs),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_assert_hindsight_stats1(InfTime),!.

$pp_chindsight_core(G,SubG,HProbs) :-
    ground(G),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T0),
    $pp_find_explanations(G),!,
    cputime(T1),
    $pp_compute_chindsight(G,SubG,HProbs),
    cputime(T2),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T1 - T0,
    NumCompTime is T2 - T1,
    $pp_assert_hindsight_stats2(SearchTime,NumCompTime),!.

$pp_chindsight_core(G,SubG,HProbs) :-
    copy_term(G,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    Clause = (DummyGoal:-BodyGoal,
                         $pc_prism_goal_id_register(GoalCp,GId),
                         $pc_prism_goal_id_register(DummyGoal,HId),
                         $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),[Clause]),
            pred('$damon_load',0,_,_,_,[('$damon_load':-true)])],
    consult_preds([],Prog),  % B-Prolog build-in
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T0),
    $pp_find_explanations(DummyGoal),!,
    cputime(T1),
    $pp_compute_chindsight(DummyGoal,SubG,HProbs),
    cputime(T2),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T1 - T0,
    NumCompTime is T2 - T1,
    $pp_assert_hindsight_stats2(SearchTime,NumCompTime),
    $pp_delete_tmp_out,!.

$pp_compute_chindsight(Goal,SubG,HProbs) :-
    $pp_collect_sw_info(Sws),
    $pc_export_sw_info(Sws),
    $pc_prism_goal_id_get(Goal,Gid),
    garbage_collect,
    $pc_compute_hindsight(Gid,SubG,1,HProbs0), % "1" indicates "conditional"
    $pp_decode_hindsight(HProbs0,HProbs),!.

$pp_decode_hindsight([],[]).
$pp_decode_hindsight([[Gid,P]|HProbs0],[[G,P]|HProbs]) :-
    $pc_prism_goal_term(Gid,G),!,
    $pp_decode_hindsight(HProbs0,HProbs).

$pp_get_subgoal_from_agg(Agg,SubG) :-
    Agg =.. [F|Args0],
    $pp_get_subgoal_from_agg1(Args0,Args1),
    SubG =.. [F|Args1].

$pp_get_subgoal_from_agg1([],[]).
$pp_get_subgoal_from_agg1([A0|Args0],[A1|Args1]) :-
    ( $pp_is_agg_patt(A0) -> A1 = _
    ; A1 = A0
    ),!,
    $pp_get_subgoal_from_agg1(Args0,Args1).

$pp_is_agg_patt(A) :-
    ( var(A) -> true
    ; member(A,[integer,atom,compound,length,d_length,depth,query,ignore])
    ).

$pp_aggregate_hindsight_probs(Agg,HProbs0,HProbs) :-
    $pp_group_hindsight_probs(Agg,HProbs0,HProbs1),!,
    $pp_aggregate_hindsight_probs1(Agg,HProbs1,HProbs).

$pp_group_hindsight_probs(Agg,HProbs0,HProbs) :-
    $pp_insert_group_patt(Agg,HProbs0,HProbs1),
    $pp_group_hindsight_probs1(HProbs1,HProbs2),
    $delete_group_patt(HProbs2,HProbs).

$pp_insert_group_patt(_,[],[]).
$pp_insert_group_patt(Agg,[[G,P]|HProbs0],[[GPatt,G,P]|HProbs]) :-
    $pp_get_group_patt(Agg,G,GPatt),!,
    $pp_insert_group_patt(Agg,HProbs0,HProbs).

$delete_group_patt([],[]).
$delete_group_patt([Gr0|Groups0],[Gr|Groups]) :-
    $delete_group_patt1(Gr0,Gr),!,
    $delete_group_patt(Groups0,Groups).

$delete_group_patt1([],[]).
$delete_group_patt1([[_GPatt,G,P]|HProbs0],[[G,P]|HProbs]) :- !,
    $delete_group_patt1(HProbs0,HProbs).

$pp_get_group_patt(Agg,G,GPatt) :-
    Agg =.. [F|AggArgs],
    G   =.. [F|Args],
    $pp_get_group_patt_args(AggArgs,Args,GPattArgs),
    GPatt =.. [F|GPattArgs].

$pp_get_group_patt_args([],[],[]).
$pp_get_group_patt_args([AggA|AggArgs],[A|Args],[GPA|GPattArgs]) :-
    ( nonvar(AggA) ->
      ( AggA = integer ->
          ( integer(A) -> GPA = A
          ; $pp_raise_domain_error($msg(1405),[A],[integer,A],
                                   $pp_group_hindsight_probs/3)
          )
      ; AggA = atom ->
          ( atom(A) -> GPA = A
          ; $pp_raise_domain_error($msg(1406),[A],[atom,A],
                                   $pp_group_hindsight_probs/3)
          )
      ; AggA = compound ->
          ( A = [] -> GPA = A
          ; \+ ground(A) ->
                $pp_raise_instanciation_error($msg(1407),[A],
                                              $pp_group_hindsight_probs/3)
          ; compound(A) -> GPA = A
          ; $pp_raise_domain_error($msg(1407),[A],[compound,A],
                                   $pp_group_hindsight_probs/3)
          )
      ; AggA = length ->
          ( (A = [] ; is_list(A)) -> length(A,L), GPA = length-L
          ; $pp_raise_domain_error($msg(1408),[A],[list,A],
                                   $pp_group_hindsight_probs/3)
          )
      ; AggA = d_length ->
          ( A = (D0-D1), is_list(D0), is_list(D1)
              -> length(D0,L0), length(D1,L1), L is L0 - L1, GPA = d_length-L
          ; $pp_raise_domain_error($msg(1409),[A],[d_list,A],
                                   $pp_group_hindsight_probs/3)
          )
      ; AggA = depth  -> $pc_get_term_depth(A,D), GPA = depth-D
      ; AggA = query  -> GPA = *
      ; AggA = ignore -> GPA = *
      ; GPA = A
      )
    ; GPA = *
    ),!,
    $pp_get_group_patt_args(AggArgs,Args,GPattArgs).

$pp_group_hindsight_probs1(HProbs0,HProbs) :-
    $pp_sort_remain_dup(HProbs0,HProbs1),!,
    $pp_group_hindsight_probs2(HProbs1,HProbs).

$pp_group_hindsight_probs2([],[]).
$pp_group_hindsight_probs2([U],[[U]]).
$pp_group_hindsight_probs2([U0|Us0],Us) :- !,
    $pp_group_hindsight_probs2(U0,[U0],Us0,Us).

$pp_group_hindsight_probs2(_,Us,[],[Us]).
$pp_group_hindsight_probs2(U0,Us0,[U1|Us1],Us) :-
    ( U0 = [GPatt,_,_], U1 = [GPatt,_,_] ->
        Us2 = [U1|Us0],!,
        $pp_group_hindsight_probs2(U1,Us2,Us1,Us)
    ; Us = [Us0|Us3],!,
      $pp_group_hindsight_probs2(U1,[U1],Us1,Us3)
    ).

$pp_aggregate_hindsight_probs1(Agg,HProbs0,HProbs) :-
    $pp_replace_agg_patt(Agg,HProbs0,HProbs1),!,
    $pp_aggregate_hindsight_probs2(HProbs1,HProbs).

$pp_replace_agg_patt(_,[],[]).
$pp_replace_agg_patt(Agg,[Gr0|Groups0],[Gr|Groups]) :-
    $pp_replace_agg_patt1(Agg,Gr0,Gr),!,
    $pp_replace_agg_patt(Agg,Groups0,Groups).

$pp_replace_agg_patt1(_,[],[]).
$pp_replace_agg_patt1(Agg,[[G,P]|HProbs0],[[APatt,P]|HProbs]) :-
    $pp_get_agg_patt(Agg,G,APatt),!,
    $pp_replace_agg_patt1(Agg,HProbs0,HProbs).

$pp_get_agg_patt(Agg,G,APatt) :-
    Agg =.. [F|AggArgs],
    G   =.. [F|Args],
    $pp_get_agg_patt_args(AggArgs,Args,APattArgs),
    APatt =.. [F|APattArgs].

$pp_get_agg_patt_args([],[],[]).
$pp_get_agg_patt_args([AggA|AggArgs],[A|Args],[APA|APattArgs]) :-
    ( nonvar(AggA) ->
      ( AggA = integer ->
          ( integer(A) -> APA = A
          ; $pp_raise_domain_error($msg(1405),[A],[integer,A],
                                   $pp_aggregate_hindsight_probs/3)
          )
      ; AggA = atom ->
          ( atom(A) -> APA = A
          ; $pp_raise_domain_error($msg(1406),[A],[atom,A],
                                   $pp_aggregate_hindsight_probs/3)
          )
      ; AggA = compound ->
          ( A = [] -> APA = A
          ; \+ ground(A) ->
                $pp_raise_instanciation_error($msg(1407),[A],
                                              $pp_aggregate_hindsight_probs/3)
          ; compound(A) -> APA = A
          ; $pp_raise_domain_error($msg(1407),[A],[compound,A],
                                   $pp_aggregate_hindsight_probs/3)
          )
      ; AggA = length ->
          ( (A = [] ; is_list(A)) -> length(A,L), APA = 'L'-L
          ; $pp_raise_domain_error($msg(1408),[A],[list,A],
                                   $pp_aggregate_hindsight_probs/3)
          )
      ; AggA = d_length ->
          ( A = (D0-D1), is_list(D0), is_list(D1)
              -> length(D0,L0), length(D1,L1), L is L0 - L1, APA = 'DL'-L
          ; $pp_raise_domain_error($msg(1409),[A],[d_list,A],
                                   $pp_aggregate_hindsight_probs/3)
          )
      ; AggA = depth  -> $pc_get_term_depth(A,D), APA = 'D'-D
      ; AggA = query  -> APA = A
      ; AggA = ignore -> APA = *
      ; APA = A
      )
    ; APA = *
    ),!,
    $pp_get_agg_patt_args(AggArgs,Args,APattArgs).

$pp_aggregate_hindsight_probs2([],[]).
$pp_aggregate_hindsight_probs2([Gr0|Groups0],[Gr|Groups]) :- !,
    $pp_aggregate_hindsight_probs3(Gr0,Gr),!,
    $pp_aggregate_hindsight_probs2(Groups0,Groups).

$pp_aggregate_hindsight_probs3(HProbs0,HProbs) :-
    $pp_sort_remain_dup(HProbs0,HProbs1),
    $pp_aggregate_hindsight_probs4(HProbs1,HProbs).

$pp_aggregate_hindsight_probs4(HProbs0,HProbs) :-
    ( get_prism_flag(log_scale,off) ->
        $pp_aggregate_hindsight_probs5(HProbs0,HProbs)
    ; $pp_aggregate_hindsight_probs5_log(HProbs0,HProbs)
    ).

$pp_aggregate_hindsight_probs5([],[]).
$pp_aggregate_hindsight_probs5([U],[U]).
$pp_aggregate_hindsight_probs5([[APatt,P]|Us0],Us) :- !,
    $pp_aggregate_hindsight_probs5(APatt,P,Us0,Us).

$pp_aggregate_hindsight_probs5(APatt,P,[],[[APatt,P]]).
$pp_aggregate_hindsight_probs5(APatt,P0,[[APatt1,P1]|Us1],Us) :-
    ( APatt = APatt1 ->
        P2 is P0 + P1,!,
        $pp_aggregate_hindsight_probs5(APatt,P2,Us1,Us)
    ; Us = [[APatt,P0]|Us2],!,
        $pp_aggregate_hindsight_probs5(APatt1,P1,Us1,Us2)
    ).

% log-scale computation for tiny probabilities
$pp_aggregate_hindsight_probs5_log([],[]).
$pp_aggregate_hindsight_probs5_log([U],[U]).
$pp_aggregate_hindsight_probs5_log([[APatt,P]|Us0],Us) :-
    $pp_aggregate_hindsight_probs5_log(APatt,P,1.0,Us0,Us).

$pp_aggregate_hindsight_probs5_log(APatt,P0,Q,[],[[APatt,P]]) :-
    P is P0 + log(Q),!.
$pp_aggregate_hindsight_probs5_log(APatt,P0,Q0,[[APatt1,P1]|Us1],Us) :-
    ( APatt = APatt1 ->
      ( P1 < -4096.0 ->           % P1 == -Inf, i.e. exp(P1) == 0
          Q is Q0,                % Note: exp(-4096) << Double.MIN_VALUE
          P2 = P0
      ; P0 < -4096.0 ->           % P0 == -Inf, i.e. exp(P0) == 0
          Q is 1.0,
          P2 = P1
      ; P1 - P0 > log(1.0e+280) ->
          Q is Q0 * exp(P0 - P1) + 1.0,
          P2 = P1
      ; Q is Q0 + exp(P1 - P0),
        P2 = P0
      ),!,
      $pp_aggregate_hindsight_probs5_log(APatt,P2,Q,Us1,Us)
    ; P is P0 + log(Q0),
      Us = [[APatt,P]|Us2],!,
      $pp_aggregate_hindsight_probs5_log(APatt1,P1,1.0,Us1,Us2)
    ).

$pp_sum_log_list([],0.0) :- !.
$pp_sum_log_list([LP],LP) :- !.
$pp_sum_log_list([LP|LPs],Sum) :-
    $pp_sum_log_list(LPs,LP,1.0,SumRest),!,
    Sum is LP + log(SumRest).

$pp_sum_log_list([],_,SumRest,SumRest).
$pp_sum_log_list([LP|LPs],FirstLP,SumRest0,SumRest) :-
    SumRest1 is SumRest0 + exp(LP - FirstLP),!,
    $pp_sum_log_list(LPs,FirstLP,SumRest1,SumRest).

%%%%
%%%% Sort hindsight proabilities
%%%%

$pp_sort_hindsight_probs(HProbs0,HProbs) :-
    ( get_prism_flag(sort_hindsight,by_goal) ->
        $pp_sort_remain_dup(HProbs0,HProbs)
    ; $pp_sort_hindsight_probs_by_prob(HProbs0,HProbs)
    ).

$pp_sort_hindsight_probs_by_prob(HProbs0,HProbs) :-
    $pp_swap_hindsight_pair(HProbs0,HProbs1),
    $pp_sort_remain_dup(HProbs1,HProbs2),
    reverse(HProbs2,HProbs3),
    $pp_swap_hindsight_pair(HProbs3,HProbs).

$pp_swap_hindsight_pair([],[]) :- !.
$pp_swap_hindsight_pair([[X,Y]|XYs],[[Y,X]|YXs]) :- !,
    $pp_swap_hindsight_pair(XYs,YXs).

$pp_sort_hindsight_probs_agg([],[]) :- !.
$pp_sort_hindsight_probs_agg([Gr0|Groups0],[Gr|Groups]) :-
    $pp_sort_hindsight_probs(Gr0,Gr),!,
    $pp_sort_hindsight_probs_agg(Groups0,Groups).

%%%%
%%%% Print hindsight probabilities
%%%%

$pp_print_hindsight_probs([]).
$pp_print_hindsight_probs([[G,P]|HProbs]) :-
    format("  ~w: ~15f~n",[G,P]),!,
    $pp_print_hindsight_probs(HProbs).

$pp_print_hindsight_probs_agg([]).
$pp_print_hindsight_probs_agg([Gr|Groups]) :-
    $pp_print_hindsight_probs(Gr),!,
    $pp_print_hindsight_probs_agg(Groups).

%%%% Statistics

$pp_assert_hindsight_stats1(InfTime0) :-
    InfTime is InfTime0 / 1000.0,
    assertz($ps_infer_time(InfTime)),!.

$pp_assert_hindsight_stats2(SearchTime0,NumCompTime0) :-
    SearchTime  is SearchTime0  / 1000.0,
    NumCompTime is NumCompTime0 / 1000.0,
    assertz($ps_infer_search_time(SearchTime)),
    assertz($ps_infer_calc_time(NumCompTime)),!.
