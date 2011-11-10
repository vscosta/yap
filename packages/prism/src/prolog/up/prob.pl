prob(Goal) :-
    prob(Goal,P),
    ( $pp_in_log_scale -> Text = 'Log-probability' ; Text = 'Probability' ),
    format("~w of ~w is: ~15f~n",[Text,Goal,P]).

prob(Goal,Prob) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),prob/2),
    $pp_prob(Goal,Prob).

$pp_prob(msw(Sw,V),Prob) :-
    $pp_require_ground(Sw,$msg(0101),prob/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),prob/2),
    $pp_clean_infer_stats,
    ( var(V) ->
        cputime(T0),
        ( $pp_in_log_scale -> Prob = 0.0 ; Prob = 1.0 ),
        cputime(T1),
        InfTime is T1 - T0,
        $pp_assert_prob_stats1(InfTime)
    ; % else
        cputime(T0),
        $pp_get_value_prob(Sw,V,Prob0),
        ( $pp_in_log_scale -> Prob is log(Prob0) ; Prob = Prob0 ),
        cputime(T1),
        InfTime is T1 - T0,
        $pp_assert_prob_stats1(InfTime)
    ),
    $pp_assert_prob_stats2(0.0,0.0),!.

$pp_prob(Goal,Prob) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_prob_core(Goal,Prob),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_assert_prob_stats1(InfTime),!.

log_prob(Goal) :-
    log_prob(Goal,P),format("Log-probability of ~w is: ~15f~n",[Goal,P]).
log_prob(Goal,P) :-
    $pp_prob(Goal,P0),( $pp_in_log_scale -> P = P0 ; P is log(P0) ).
    
$pp_in_log_scale :-
    get_prism_flag(log_scale,on).

$pp_prob_core(Goal,Prob) :-
    ground(Goal),
    $pp_is_tabled_probabilistic_atom(Goal),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pp_compute_inside(Goal,Prob),!,
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_prob_stats2(SearchTime,NumCompTime),!.

$pp_prob_core(Goal,Prob) :-
    copy_term(Goal,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    Clause = (DummyGoal:-BodyGoal,
                         $pc_prism_goal_id_register(GoalCp,GId),
                         $pc_prism_goal_id_register(DummyGoal,HId),
                         $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),[Clause])],
    consult_preds([],Prog),
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_inside(DummyGoal,Prob),
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_prob_stats2(SearchTime,NumCompTime),
    $pp_delete_tmp_out,!.

% Sws = [sw(Id,Instances,Probs,Deltas,FixedP,FixedH),...]
$pp_compute_inside(Goal,Prob) :-
    $pp_collect_sw_info(Sws),
    $pc_export_sw_info(Sws),
    $pc_prism_goal_id_get(Goal,Gid),
    $pc_compute_inside(Gid,Prob),!.

$pp_get_value_prob(Sw,V,Prob) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_value_prob(Values,Probs,V,Prob).

$pp_get_value_prob([V|_],[Prob0|_],V,Prob) :- !, Prob = Prob0.
$pp_get_value_prob([_|Vs],[_|Probs],V,Prob) :- !,
    $pp_get_value_prob(Vs,Probs,V,Prob).

$pp_collect_sw_info(Sws) :-
    $pc_prism_sw_count(N),
    $pp_collect_sw_info(0,N,Sws).

$pp_collect_sw_info(Sid,N,[]) :- Sid >= N,!.
$pp_collect_sw_info(Sid,N,SwInsList) :-
    $pc_prism_sw_term(Sid,Sw),
    $pp_get_parameters(Sw,Values,Pbs),
    $pp_get_hyperparameters(Sw,Values,_,Deltas),
    ( $pd_fixed_parameters(Sw)      -> FixedP = 1 ; FixedP = 0 ),
    ( $pd_fixed_hyperparameters(Sw) -> FixedH = 1 ; FixedH = 0 ),
    SwInsList = [sw(Sid,Iids,Pbs,Deltas,FixedP,FixedH)|SwInsList1],!,
    $pp_collect_sw_ins_ids(Sw,Values,Iids),
    Sid1 is Sid + 1,!,
    $pp_collect_sw_info(Sid1,N,SwInsList1).

get_subgoal_hashtable(GTab) :-
    $pp_get_subgoal_hashtable(GTab).

$pp_get_subgoal_hashtable(GTab) :-
    $pc_prism_goal_count(GC),
    new_hashtable(GTab,GC),
    $pp_get_subgoal_hashtable(0,GC,GTab).

$pp_get_subgoal_hashtable(Gid,N,_) :- Gid >= N,!.
$pp_get_subgoal_hashtable(Gid,N,GTab) :-
    $pc_prism_goal_term(Gid,G),
    hashtable_put(GTab,Gid,G),
    Gid1 is Gid + 1,!,
    $pp_get_subgoal_hashtable(Gid1,N,GTab).

get_switch_hashtable(SwTab) :-
    $pp_get_switch_hashtable(SwTab).

$pp_get_switch_hashtable(SwTab) :-
    $pc_prism_sw_ins_count(IC),
    new_hashtable(SwTab,IC),
    $pp_get_switch_hashtable(0,IC,SwTab).

$pp_get_switch_hashtable(Sid,N,_) :- Sid >= N,!.
$pp_get_switch_hashtable(Sid,N,SwTab) :-
    $pc_prism_sw_ins_term(Sid,S),
    hashtable_put(SwTab,Sid,S),
    Sid1 is Sid + 1,!,
    $pp_get_switch_hashtable(Sid1,N,SwTab).

probf(Goal) :-
    $pp_probf(Goal,Expls,1,0), \+ \+ print_graph(Expls,[lr('<=>')]).
probfi(Goal) :-
    $pp_probf(Goal,Expls,1,1), \+ \+ print_graph(Expls,[lr('<=>')]).
probfo(Goal) :-
    $pp_probf(Goal,Expls,1,2), \+ \+ print_graph(Expls,[lr('<=>')]).
probfv(Goal) :-
    $pp_probf(Goal,Expls,1,3), \+ \+ print_graph(Expls,[lr('<=>')]).
probfio(Goal) :-
    $pp_probf(Goal,Expls,1,4), \+ \+ print_graph(Expls,[lr('<=>')]).

probf(Goal,Expls) :-
    $pp_probf(Goal,Expls,1,0).
probfi(Goal,Expls) :-
    $pp_probf(Goal,Expls,1,1).
probfo(Goal,Expls) :-
    $pp_probf(Goal,Expls,1,2).
probfv(Goal,Expls) :-
    $pp_probf(Goal,Expls,1,3).
probfio(Goal,Expls) :-
    $pp_probf(Goal,Expls,1,4).

probef(Goal) :-
    $pp_probf(Goal,Expls,0,0), \+ \+ print_graph(Expls,[lr('<=>')]).
probefi(Goal) :-
    $pp_probf(Goal,Expls,0,1), \+ \+ print_graph(Expls,[lr('<=>')]).
probefo(Goal) :-
    $pp_probf(Goal,Expls,0,2), \+ \+ print_graph(Expls,[lr('<=>')]).
probefv(Goal) :-
    $pp_probf(Goal,Expls,0,3), \+ \+ print_graph(Expls,[lr('<=>')]).
probefio(Goal) :-
    $pp_probf(Goal,Expls,0,4), \+ \+ print_graph(Expls,[lr('<=>')]).

probef(Goal,Expls) :-
    $pp_probf(Goal,Expls,0,0).
probefi(Goal,Expls) :-
    $pp_probf(Goal,Expls,0,1).
probefo(Goal,Expls) :-
    $pp_probf(Goal,Expls,0,2).
probefv(Goal,Expls) :-
    $pp_probf(Goal,Expls,0,3).
probefio(Goal,Expls) :-
    $pp_probf(Goal,Expls,0,4).

probef(Goal,Expls,GoalHashTab,SwHashTab) :-
    $pp_probf(Goal,Expls,0,0),
    $pp_get_subgoal_hashtable(GoalHashTab),
    $pp_get_switch_hashtable(SwHashTab).
probefi(Goal,Expls,GoalHashTab,SwHashTab) :-
    $pp_probf(Goal,Expls,0,1),
    $pp_get_subgoal_hashtable(GoalHashTab),
    $pp_get_switch_hashtable(SwHashTab).
probefo(Goal,Expls,GoalHashTab,SwHashTab) :-
    $pp_probf(Goal,Expls,0,2),
    $pp_get_subgoal_hashtable(GoalHashTab),
    $pp_get_switch_hashtable(SwHashTab).
probefv(Goal,Expls,GoalHashTab,SwHashTab) :-
    $pp_probf(Goal,Expls,0,3),
    $pp_get_subgoal_hashtable(GoalHashTab),
    $pp_get_switch_hashtable(SwHashTab).
probefio(Goal,Expls,GoalHashTab,SwHashTab) :-
    $pp_probf(Goal,Expls,0,4),
    $pp_get_subgoal_hashtable(GoalHashTab),
    $pp_get_switch_hashtable(SwHashTab).

%% PrMode is one of 0 (none), 1 (inside), 2 (outside), 3 (viterbi) and
%% 4 (inside-outside)

$pp_probf(Goal,Expls,Decode,PrMode) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),$pp_probf/4),
    $pp_compute_expls(Goal,Expls,Decode,PrMode).

$pp_compute_expls(Goal,Expls,Decode,PrMode) :-
    Goal = msw(I,V),!,
    $pp_require_ground(I,$msg(0101),$pp_probf/4),
    $pp_require_switch_outcomes(I,$msg(0102),$pp_probf/4),
    $pp_clean_infer_stats,
    ( ground(V) -> V = VCp ; copy_term(V,VCp) ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = ($prism_expl_msw(I,VCp,Sid),
                 $pc_prism_goal_id_register(DummyGoal,Hid),
                 $prism_eg_path(Hid,[],[Sid])),
    Prog = [pred(DummyGoal,0,_,_,tabled(_,_,_,_),[(DummyGoal:-DummyBody)])],
    consult_preds([],Prog),
    cputime(T0),
    $pp_compute_expls(DummyGoal,Goal,Expls,Decode,PrMode,T0),!.

$pp_compute_expls(Goal,Expls,Decode,PrMode) :-
    $pp_is_tabled_probabilistic_atom(Goal),
    ground(Goal),!,
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_compute_expls(Goal,_,Expls,Decode,PrMode,T0),!.

$pp_compute_expls(Goal,Expls,Decode,PrMode) :-
    $pp_clean_infer_stats,
    copy_term(Goal,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) ->
      BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = (BodyGoal,
                 $pc_prism_goal_id_register(GoalCp,GId),
                 $pc_prism_goal_id_register(DummyGoal,HId),
                 $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_,_,tabled(_,_,_,_),[(DummyGoal:-DummyBody)])],
    consult_preds([],Prog),
    cputime(T0),
    $pp_compute_expls(DummyGoal,Goal,Expls,Decode,PrMode,T0),!.

$pp_compute_expls(Goal,GLabel,Expls,Decode,PrMode,T0) :-
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    garbage_collect,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pc_prism_goal_id_get(Goal,Gid),
    $pc_alloc_sort_egraph(Gid),
    cputime(T3),
    ( PrMode == 0 -> true
    ; $pp_collect_sw_info(Sws),
      $pc_export_sw_info(Sws),
      $pc_compute_probf(PrMode)
    ),
    cputime(T4),
    $pc_import_sorted_graph_size(Size),
    $pp_build_expls(Size,Decode,PrMode,GLabel,Expls),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    cputime(T5),
    SearchTime  is T2 - T1,
    NumCompTime is T4 - T3,
    InfTime     is T5 - T0,
    ( PrMode == 0 -> $pp_assert_prob_stats2(SearchTime)
    ; $pp_assert_prob_stats2(SearchTime,NumCompTime)
    ),
    $pp_assert_prob_stats1(InfTime),
    $pp_delete_tmp_out,!.

$pp_build_expls(I0,_,_,_,Expls), I0 =< 0 =>
    Expls = [].
$pp_build_expls(I0,Decode,PrMode,GLabel,Expls), I0 > 0 =>
    I is I0 - 1,
    $pc_import_sorted_graph_gid(I,Gid),
    $pc_import_sorted_graph_paths(I,Paths0),
    ( Decode == 0    -> Label = Gid
    ; nonvar(GLabel) -> Label = GLabel
    ; $pc_prism_goal_term(Gid,Label)
    ),
    ( PrMode == 0 -> Node = node(Label,Paths)  % probf
    ; PrMode == 4 ->                           % probfio
        $pp_get_gnode_probs(PrMode,Gid,Value),
        Node  = node(Label,Paths,Value),
        Value = [_,Vo]
    ; $pp_get_gnode_probs(PrMode,Gid,Value),
      Node  = node(Label,Paths,Value),
      Value = Vo
    ),
    $pp_decode_paths(Paths0,Paths,Decode,PrMode,Vo),
    Expls = [Node|Expls1],!,
    $pp_build_expls(I,Decode,PrMode,_,Expls1).



$pp_decode_paths([],[],_Decode,_PrMode,_Vo).
$pp_decode_paths([Pair|Pairs],[Path|Paths],Decode,PrMode,Vo) :-
    Pair = [Gids,Sids],
    $pp_decode_gnodes(Gids,GNodes,Decode,PrMode,Vg),
    $pp_decode_snodes(Sids,SNodes,Decode,PrMode,Vs),
    get_prism_flag(log_scale,LogScale),
    ( PrMode == 0 -> 
        Path = path(GNodes,SNodes)
    ; PrMode == 1 -> ( LogScale == on -> Vi is Vg + Vs ; Vi is Vg * Vs),
        Path = path(GNodes,SNodes,Vi)
    ; PrMode == 2 ->
        Path = path(GNodes,SNodes,Vo)
    ; PrMode == 3 -> ( LogScale == on -> Vi is Vg + Vs ; Vi is Vg * Vs),
        Path = path(GNodes,SNodes,Vi)      
    ; PrMode == 4 -> ( LogScale == on -> Vi is Vg + Vs ; Vi is Vg * Vs),
        Path = path(GNodes,SNodes,[Vi,Vo])
    ),!,
    $pp_decode_paths(Pairs,Paths,Decode,PrMode,Vo).

$pp_decode_gnodes(Gids,GNodes,Decode,PrMode,V) :-
    get_prism_flag(log_scale,LogScale),
    ( LogScale == on -> V0 = 0.0 ; V0 = 1.0 ),
    $pp_decode_gnodes(Gids,GNodes,Decode,PrMode,LogScale,V0,V).

$pp_decode_gnodes([],[],_Decode,_PrMode,_LogScale,V,V) :- !.
$pp_decode_gnodes([Gid|Gids],[GNode|GNodes],Decode,PrMode,LogScale,V0,V) :-
    ( Decode == 0 -> Gid = Label
    ; $pc_prism_goal_term(Gid,Label)
    ),
    ( PrMode == 0 -> GNode = Label
    ; $pp_get_gnode_probs(PrMode,Gid,Value),
      GNode = gnode(Label,Value),
      ( LogScale == on ->
        V1 is Value + V0
      ; V1 is Value * V0
      )
    ),!,
    $pp_decode_gnodes(Gids,GNodes,Decode,PrMode,LogScale,V1,V).

$pp_decode_snodes(Sids,SNodes,Decode,PrMode,V) :-
    get_prism_flag(log_scale,LogScale),
    ( LogScale == on -> V0 = 0.0 ; V0 = 1.0 ),
    $pp_decode_snodes(Sids,SNodes,Decode,PrMode,LogScale,V0,V).

$pp_decode_snodes([],[],_Decode,_PrMode,_LogScale,V,V) :- !.
$pp_decode_snodes([Sid|Sids],[SNode|SNodes],Decode,PrMode,LogScale,V0,V) :-
    ( Decode == 0 -> Sid = Label
    ; $pc_prism_sw_ins_term(Sid,Label)
    ),
    ( PrMode == 0 -> SNode = Label
    ; $pp_get_snode_probs(PrMode,Sid,Value),
      SNode = snode(Label,Value),
      ( LogScale == on ->
        V1 is Value + V0
      ; V1 is Value * V0
      )
    ),!,
    $pp_decode_snodes(Sids,SNodes,Decode,PrMode,LogScale,V1,V).

$pp_get_gnode_probs(1,Gid,Pi) :- $pc_get_gnode_inside(Gid,Pi),!.
$pp_get_gnode_probs(2,Gid,Po) :- $pc_get_gnode_outside(Gid,Po),!.
$pp_get_gnode_probs(3,Gid,Pv) :- $pc_get_gnode_viterbi(Gid,Pv),!.
$pp_get_gnode_probs(4,Gid,[Pi,Po]) :-
    $pc_get_gnode_inside(Gid,Pi),
    $pc_get_gnode_outside(Gid,Po),!.

$pp_get_snode_probs(1,Sid,Pi) :- $pc_get_snode_inside(Sid,Pi),!.
$pp_get_snode_probs(2,Sid,E)  :- $pc_get_snode_expectation(Sid,E),!.
$pp_get_snode_probs(3,Sid,Pi) :- $pc_get_snode_inside(Sid,Pi),!.
$pp_get_snode_probs(4,Sid,[Pi,Po]) :-
    $pc_get_snode_inside(Sid,Pi),
    $pc_get_snode_expectation(Sid,Po),!.

%%%% Statistics

$pp_assert_prob_stats1(InfTime0) :-
    InfTime is InfTime0 / 1000.0,
    assertz($ps_infer_time(InfTime)),!.

$pp_assert_prob_stats2(SearchTime0) :-
    SearchTime is SearchTime0 / 1000.0,
    assertz($ps_infer_search_time(SearchTime)),!.

$pp_assert_prob_stats2(SearchTime0,NumCompTime0) :-
    SearchTime  is SearchTime0 / 1000.0,
    NumCompTime is NumCompTime0 / 1000.0,
    assertz($ps_infer_search_time(SearchTime)),
    assertz($ps_infer_calc_time(NumCompTime)),!.

$pp_clean_infer_stats :-
    retractall($ps_infer_time(_)),
    retractall($ps_infer_search_time(_)),
    retractall($ps_infer_calc_time(_)),!.
