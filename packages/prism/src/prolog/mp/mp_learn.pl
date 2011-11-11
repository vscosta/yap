:- $pp_require_mp_mode.

%%----------------------------------------

$pp_learn_core(Mode) :-
    ( $pc_mp_master -> $pp_mpm_learn_main(Mode) ; true ).
$pp_learn_core(Mode,Goals) :-
    ( $pc_mp_master -> $pp_mpm_learn_main(Mode,Goals) ; true ).

$pp_mpm_learn_main(Mode) :-
    learn_data_file(FileName),
    load_clauses(FileName,Goals,[]),
    $pc_mpm_bcast_command($pp_mps_learn_core(Mode)),!,
    $pp_mpm_learn_core(Mode,Goals).

$pp_mpm_learn_main(Mode,Goals) :-
    $pp_learn_check_goals(Goals),
    $pc_mpm_bcast_command($pp_mps_learn_core(Mode)),!,
    $pp_mpm_learn_core(Mode,Goals).

%%----------------------------------------

% Master
$pp_mpm_learn_core(Mode,Goals) :-
    $pc_mp_sync(2,1),
    $pc_mp_wtime(Start),
    $pp_learn_clean_info,
    $pp_learn_reset_hparams(Mode),
    $pp_build_count_pairs(Goals,GoalEqCountPairs),
    $pp_learn_message(MsgS,MsgE,MsgT,MsgM),
    $pc_set_em_message(MsgE),
    $pc_mp_wtime(StartExpl),
    global_set($pg_num_goals,0),
    $pc_mpm_share_prism_flags,
    $pp_mpm_find_explanations(GoalEqCountPairs,GoalCountPairs),!,
    global_set($pg_observed_facts,GoalCountPairs),
    $pp_print_num_goals(MsgS),
    $pc_mp_wtime(EndExpl),
    TableSpace = 'N/A',
    ( MsgM == 0 -> true
    ; format("Gathering and exporting switch information ...~n",[])
    ),
    $pc_mp_recv_switches,
    $pp_mpm_export_switches,
    $pc_mpm_alloc_occ_switches,
    $pc_mp_send_swlayout,
    $pp_collect_init_switches(Sws),
    $pc_export_sw_info(Sws),
    $pc_mp_wtime(StartEM),
    $pp_mpm_em(Mode,Output),
    $pc_mp_wtime(EndEM),
    $pc_import_occ_switches(NewSws,NumSwitches,NumSwVals),
    $pp_decode_update_switches(Mode,NewSws),
    $pc_mpm_import_graph_stats(NumSubgraphs,NumGoalNodes,NumSwNodes,AvgShared),
    $pc_mp_wtime(End),
    $pp_assert_graph_stats(NumSubgraphs,NumGoalNodes,NumSwNodes,AvgShared),
    $pp_assert_learn_stats(Mode,Output,NumSwitches,NumSwVals,TableSpace,
                           Start,End,StartExpl,EndExpl,StartEM,EndEM,1),
    ( MsgT == 0 -> true ; $pp_print_learn_stats_message ),
    ( MsgM == 0 -> true ; $pp_print_learn_end_message(Mode) ),!.

% Slave
$pp_mps_learn_core(Mode) :-
    $pc_mp_sync(2,1),
    $pp_learn_clean_info,
    $pc_mps_share_prism_flags,
    $pp_mps_find_explanations(GoalCountPairs),
    global_set($pg_observed_facts,GoalCountPairs),
    $pp_collect_init_switches(_Sws),
    $pp_observed_facts(GoalCountPairs,GoalIdCountPairs,0,Len,0,NumOfGoals,-1,FailRootIndex),
    $pc_prism_prepare(GoalIdCountPairs,Len,NumOfGoals,FailRootIndex),
    $pc_mp_send_switches,
    $pc_mp_recv_swlayout,
    $pp_mps_em(Mode),
    $pc_mps_import_graph_stats,!.

%%----------------------------------------

$pp_mpm_em(params,Output) :-
    $pc_mpm_prism_em(Iterate,LogPost,LogLike,BIC,CS,ModeSmooth),
    Output = [Iterate,LogPost,LogLike,BIC,CS,ModeSmooth].
$pp_mpm_em(hparams,Output) :-
    $pc_mpm_prism_vbem(IterateVB,FreeEnergy),
    Output = [IterateVB,FreeEnergy].
$pp_mpm_em(both,Output) :-
    $pc_mpm_prism_both_em(IterateVB,FreeEnergy),
    Output = [IterateVB,FreeEnergy].

$pp_mps_em(params) :-
    $pc_mps_prism_em.
$pp_mps_em(hparams) :-
    $pc_mps_prism_vbem.
$pp_mps_em(both) :-
    $pc_mps_prism_both_em.

%%----------------------------------------

$pp_mpm_find_explanations(GoalEqCountPairs,GoalCountPairs) :-
    $pp_learn_message(MsgS,_,_,_),
    $pp_mpm_expl_goals(MsgS,GoalEqCountPairs,GoalCountPairs),
    $pc_mp_size(N),
    $pp_mpm_expl_complete(N).

$pp_mpm_expl_goals(_,[],[]).
$pp_mpm_expl_goals(MsgS,
                   [Goal=Count|GoalEqCountPairs],
                   [goal(Goal,Count)|GoalCountPairs]) :-
    $pc_mp_send_goal(Goal=Count),
    $pp_print_goal_message(MsgS),!,
    $pp_mpm_expl_goals(MsgS,GoalEqCountPairs,GoalCountPairs).

$pp_mpm_expl_complete(N) :-
    N =< 1,!.
$pp_mpm_expl_complete(N) :-
    $pc_mp_send_goal($done),
    N1 is N - 1,!,
    $pp_mpm_expl_complete(N1).

%%----------------------------------------

$pp_mps_find_explanations(GoalCountPairs) :-
    $pp_mps_expl_goals([],GoalCountPairs).

$pp_mps_expl_goals(GoalCountPairs0,GoalCountPairs) :-
    once($pc_mp_recv_goal(GoalEqCountPair)),
    GoalEqCountPair \== $done,!,
    GoalEqCountPair = (Goal=Count),
    $pp_build_dummy_goal(Goal,DummyGoal),
    ( $pp_expl_one_goal(DummyGoal) -> true
    ; mps_err_msg("Failed to find solutions for ~w.",[Goal])
    ),
    GoalCountPairs1 = [goal(DummyGoal,Count)|GoalCountPairs0],
    $pc_sleep(1), % enable this for the stability in small-scale learning
    !,
    $pp_mps_expl_goals(GoalCountPairs1,GoalCountPairs).
$pp_mps_expl_goals(GoalCountPairs,GoalCountPairs).

%%----------------------------------------

$pp_mpm_export_switches :-
    $pc_prism_sw_count(N),
    $pp_mpm_export_switches(0,N).

$pp_mpm_export_switches(Sid,N) :-
    Sid >= N,!.
$pp_mpm_export_switches(Sid,N) :-
    $pc_prism_sw_term(Sid,Sw),
    $pp_get_values(Sw,Values),
    $pp_export_switch(Sid,Sw,Values),
    Sid1 is Sid + 1,!,
    $pp_mpm_export_switches(Sid1,N).
