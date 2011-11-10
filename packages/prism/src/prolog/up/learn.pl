learn :-
    get_prism_flag(learn_mode,Mode),
    $pp_learn_main(Mode).
learn(Goals) :-
    get_prism_flag(learn_mode,Mode),
    $pp_learn_main(Mode,Goals).

learn_p :-
    $pp_learn_main(params).
learn_p(Goals) :-
    $pp_learn_main(params,Goals).
learn_h :-
    $pp_learn_main(hparams).
learn_h(Goals) :-
    $pp_learn_main(hparams,Goals).
learn_b :-
    $pp_learn_main(both).
learn_b(Goals) :-
    $pp_learn_main(both,Goals).

%% for the parallel version
$pp_learn_main(Mode) :- call($pp_learn_core(Mode)).
$pp_learn_main(Mode,Goals) :- call($pp_learn_core(Mode,Goals)).

$pp_learn_data_file(FileName) :-
    get_prism_flag(data_source,Source),
    ( Source == none ->
        $pp_raise_runtime_error($msg(1300),data_source_not_found,
                                $pp_learn_data_file/1)
    ; Source == data/1 ->
      ( current_predicate(data/1) -> data(FileName)
      ; $pp_raise_runtime_error($msg(1301),data_source_not_found,
                                $pp_learn_data_file/1)
      )
    ; Source = file(FileName)
    ; $pp_raise_unmatched_branches($pp_learn_data_file/1)
    ),!.

$pp_learn_check_goals(Goals) :-
    $pp_require_observed_data(Goals,$msg(1302),$pp_learn_core/1),
    $pp_learn_check_goals1(Goals),
    ( get_prism_flag(daem,on),
      membchk(failure,Goals)
        -> $pp_raise_runtime_error($msg(1305),daem_with_failure,
                                   $pp_learn_core/1)
    ; true
    ).

$pp_learn_check_goals1([]).
$pp_learn_check_goals1([G0|Gs]) :-
    ( (G0 = goal(G,Count) ; G0 = count(G,Count) ; G0 = (Count times G) ) ->
        $pp_require_positive_integer(Count,$msg(1306),$pp_learn_core/1)
    ; G = G0
    ),
    $pp_require_tabled_probabilistic_atom(G,$msg(1303),$pp_learn_core/1),!,
    $pp_learn_check_goals1(Gs).

$pp_learn_core(Mode) :-
    $pp_learn_data_file(FileName),
    load_clauses(FileName,Goals,[]),!,
    $pp_learn_core(Mode,Goals).

$pp_learn_core(Mode,Goals) :-
    $pp_learn_check_goals(Goals),
    $pp_learn_message(MsgS,MsgE,MsgT,MsgM),
    $pc_set_em_message(MsgE),
    cputime(Start),
    $pp_learn_clean_info,
    $pp_learn_reset_hparams(Mode),
    $pp_trans_goals(Goals,GoalCountPairs,AllGoals),!,
    global_set($pg_observed_facts,GoalCountPairs),
    cputime(StartExpl),
    global_set($pg_num_goals,0),
    $pp_find_explanations(AllGoals),!,
    $pp_print_num_goals(MsgS),
    cputime(EndExpl),
% vsc    statistics(table,[TableSpace,_]),
TableSpace = 0, % not supported in YAP (it should be).
    ( MsgM == 0 -> true
    ; format("Exporting switch information to the EM routine ... ",[])
    ),
    flush_output,
    $pp_collect_init_switches(Sws),
    $pc_export_sw_info(Sws),
    ( MsgM == 0 -> true ; format("done~n",[]) ),
    $pp_observed_facts(GoalCountPairs,GidCountPairs,
                       0,Len,0,NGoals,-1,FailRootIndex),
    $pc_prism_prepare(GidCountPairs,Len,NGoals,FailRootIndex),
    cputime(StartEM),
    $pp_em(Mode,Output),
    cputime(EndEM),
    $pc_import_occ_switches(NewSws,NSwitches,NSwVals),
    $pp_decode_update_switches(Mode,NewSws),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_delete_tmp_out,
    cputime(End),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_learn_stats(Mode,Output,NSwitches,NSwVals,TableSpace,
                           Start,End,StartExpl,EndExpl,StartEM,EndEM,1000),
    ( MsgT == 0 -> true ; $pp_print_learn_stats_message ),
    ( MsgM == 0 -> true ; $pp_print_learn_end_message(Mode) ),!.

$pp_learn_clean_info :-
    $pp_clean_dummy_goal_table,
    $pp_clean_graph_stats,
    $pp_clean_learn_stats,
    $pp_init_tables_aux,
    $pp_init_tables_if_necessary,!.

$pp_learn_reset_hparams(Mode) :-
    ( Mode == params -> true
    ; get_prism_flag(reset_hparams,on) -> set_sw_all_a(_)
    ; true
    ).

$pp_print_num_goals(MsgS) :-
    ( MsgS == 0 -> true
    ; global_get($pg_num_goals,N),format("(~w)~n",[N]),flush_output
    ).

$pp_em(params,Output) :-
    $pc_prism_em(Iterate,LogPost,LogLike,BIC,CS,ModeSmooth),
    Output = [Iterate,LogPost,LogLike,BIC,CS,ModeSmooth].
$pp_em(hparams,Output) :-
    $pc_prism_vbem(IterateVB,FreeEnergy),
    Output = [IterateVB,FreeEnergy].
$pp_em(both,Output) :-
    $pc_prism_both_em(IterateVB,FreeEnergy),
    Output = [IterateVB,FreeEnergy].
    
$pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared) :-
    NNodes is NGoalNodes + NSwNodes,
    assertz($ps_num_subgraphs(NSubgraphs)),
    assertz($ps_num_nodes(NNodes)),
    assertz($ps_num_goal_nodes(NGoalNodes)),
    assertz($ps_num_switch_nodes(NSwNodes)),
    assertz($ps_avg_shared(AvgShared)),!.

$pp_assert_learn_stats(Mode,Output,NSwitches,NSwVals,TableSpace,
                       Start,End,StartExpl,EndExpl,StartEM,EndEM,UnitsPerSec) :-
    assertz($ps_num_switches(NSwitches)),
    assertz($ps_num_switch_values(NSwVals)),
    ( integer(TableSpace) -> assertz($ps_learn_table_space(TableSpace)) ; true ),
    Time is (End - Start) / UnitsPerSec,
    assertz($ps_learn_time(Time)),
    TimeExpl is (EndExpl - StartExpl) / UnitsPerSec,
    assertz($ps_learn_search_time(TimeExpl)),
    TimeEM is (EndEM - StartEM) / UnitsPerSec,
    assertz($ps_em_time(TimeEM)),
    $pp_assert_learn_stats_sub(Mode,Output),!.

$pp_assert_learn_stats_sub(params,Output) :-
    Output = [Iterate,LogPost,LogLike,BIC,CS,ModeSmooth],
    assertz($ps_num_iterations(Iterate)),
    ( ModeSmooth > 0 -> assertz($ps_log_post(LogPost)) ; true ),
    assertz($ps_log_likelihood(LogLike)),
    assertz($ps_bic_score(BIC)),
    ( ModeSmooth > 0 -> assertz($ps_cs_score(CS)) ; true ),!.

$pp_assert_learn_stats_sub(hparams,Output) :-
    Output = [IterateVB,FreeEnergy],
    assertz($ps_num_iterations_vb(IterateVB)),
    assertz($ps_free_energy(FreeEnergy)),!.

$pp_assert_learn_stats_sub(both,Output) :-
    Output = [IterateVB,FreeEnergy],
    assertz($ps_num_iterations_vb(IterateVB)),
    assertz($ps_free_energy(FreeEnergy)),!.

$pp_print_learn_stats_message :-
    format("Statistics on learning:~n",[]),
    ( $pp_print_learn_stats_message_sub,fail ; true ),!.

$pp_print_learn_stats_message_sub :-
    ( $ps_num_nodes(L),
        format("~tGraph size: ~w~n",[L])
    ; $ps_num_switches(L),
        format("~tNumber of switches: ~w~n",[L])
    ; $ps_num_switch_values(L),
        format("~tNumber of switch instances: ~w~n",[L])
    ; $ps_num_iterations_vb(L),
        format("~tNumber of iterations: ~w~n",[L])
    ; $ps_num_iterations(L),
        format("~tNumber of iterations: ~w~n",[L])
    ; $ps_free_energy(L),
        format("~tFinal variational free energy: ~9f~n",[L])
    ; $ps_log_post(L),
        format("~tFinal log of a posteriori prob: ~9f~n",[L])
    ; $ps_log_likelihood(L), \+ $ps_log_post(_),
        format("~tFinal log likelihood: ~9f~n",[L])
    ; $ps_learn_time(L),
        format("~tTotal learning time: ~3f seconds~n",[L])
    ; $ps_learn_search_time(L),
        format("~tExplanation search time: ~3f seconds~n",[L])
    ; $ps_learn_table_space(L),
        format("~tTotal table space used: ~w bytes~n",[L])
    ).

$pp_print_learn_end_message(Mode) :-
    ( Mode == params ->
        format("Type show_sw to show the probability distributions.~n",[])
    ; Mode == hparams ->
        format("Type show_sw_a/show_sw_d to show the probability distributions.~n",[])
    ; Mode == both ->
        format("Type show_sw_pa/show_sw_pd to show the probability distributions.~n",[])
    ).

$pp_clean_graph_stats :-
    retractall($ps_num_subgraphs(_)),
    retractall($ps_num_nodes(_)),
    retractall($ps_num_goal_nodes(_)),
    retractall($ps_num_switch_nodes(_)),
    retractall($ps_avg_shared(_)),!.

$pp_clean_learn_stats :-
    retractall($ps_log_likelihood(_)),
    retractall($ps_log_post(_)),
    retractall($ps_num_switches(_)),
    retractall($ps_num_switch_values(_)),
    retractall($ps_num_iterations(_)),
    retractall($ps_num_iterations_vb(_)),
    retractall($ps_bic_score(_)),
    retractall($ps_cs_score(_)),
    retractall($ps_free_energy(_)),
    retractall($ps_learn_time(_)),
    retractall($ps_learn_search_time(_)),
    retractall($ps_em_time(_)),
    retractall($ps_learn_table_space(_)),!.

$pp_collect_init_switches(Sws) :-
    $pc_prism_sw_count(N),
    $pp_collect_init_switches(0,N,Sws).

$pp_collect_init_switches(Sid,N,SwInsList) :- Sid >= N,!,
    SwInsList = [].
$pp_collect_init_switches(Sid,N,SwInsList) :-
    $pc_prism_sw_term(Sid,Sw),
    SwInsList = [sw(Sid,Instances,Pbs,Deltas,FixedP,FixedH)|SwInsList1],
    $pp_get_parameters(Sw,Values,Pbs),!,
    $pp_get_hyperparameters(Sw,Values,_,Deltas),!,
    ( $pd_fixed_parameters(Sw)      -> FixedP = 1 ; FixedP = 0 ),
    ( $pd_fixed_hyperparameters(Sw) -> FixedH = 1 ; FixedH = 0 ),
    $pp_collect_sw_ins_ids(Sw,Values,Instances),
    Sid1 is Sid + 1,!,
    $pp_collect_init_switches(Sid1,N,SwInsList1).

$pp_collect_sw_ins_ids(_Sw,[],[]).
$pp_collect_sw_ins_ids(Sw,[V|Vs],[I|Is]) :-
    $pc_prism_sw_ins_id_get(msw(Sw,V),I),!,
    $pp_collect_sw_ins_ids(Sw,Vs,Is).

$pp_decode_update_switches(params,Sws) :-
    $pp_decode_update_switches_p(Sws).
$pp_decode_update_switches(hparams,Sws) :-
    $pp_decode_update_switches_h(Sws).
$pp_decode_update_switches(both,Sws) :-
    $pp_decode_update_switches_b(Sws).

$pp_decode_update_switches_p([]).
$pp_decode_update_switches_p([sw(_,SwInstances)|Sws]) :-
    $pp_decode_switch_name(SwInstances,Sw),
    $pp_decode_switch_instances(SwInstances,Updates),
    get_values1(Sw,Values),
    $pp_separate_updates(Values,Probs,_Deltas,Es,Updates),
    ( retract($pd_parameters(Sw,_,_)) -> true ; true ),
    assert($pd_parameters(Sw,Values,Probs)),
    ( retract($pd_expectations(Sw,_,_)) -> true ; true),
    ( retract($pd_hyperexpectations(Sw,_,_)) -> true ; true),
    assert($pd_expectations(Sw,Values,Es)),!,
    $pp_decode_update_switches_p(Sws).

$pp_decode_update_switches_h([]).
$pp_decode_update_switches_h([sw(_,SwInstances)|Sws]) :-
    $pp_decode_switch_name(SwInstances,Sw),
    $pp_decode_switch_instances(SwInstances,Updates),
    get_values1(Sw,Values),
    $pp_separate_updates(Values,_Probs,Deltas,Es,Updates),
    ( retract($pd_hyperparameters(Sw,_,_,_)) -> true ; true ),
    $pp_delta_to_alpha(Deltas,Alphas),
    assert($pd_hyperparameters(Sw,Values,Alphas,Deltas)),
    ( retract($pd_expectations(Sw,_,_)) -> true ; true),
    ( retract($pd_hyperexpectations(Sw,_,_)) -> true ; true),
    assert($pd_hyperexpectations(Sw,Values,Es)),!,
    $pp_decode_update_switches_h(Sws).

$pp_decode_update_switches_b([]).
$pp_decode_update_switches_b([sw(_,SwInstances)|Sws]) :-
    $pp_decode_switch_name(SwInstances,Sw),
    $pp_decode_switch_instances(SwInstances,Updates),
    get_values1(Sw,Values),
    $pp_separate_updates(Values,Probs,Deltas,Es,Updates),
    ( retract($pd_parameters(Sw,_,_)) -> true ; true ),
    assert($pd_parameters(Sw,Values,Probs)),
    ( retract($pd_hyperparameters(Sw,_,_,_)) -> true ; true ),
    $pp_delta_to_alpha(Deltas,Alphas),
    assert($pd_hyperparameters(Sw,Values,Alphas,Deltas)),
    ( retract($pd_hyperexpectations(Sw,_,_)) -> true ; true),
    ( retract($pd_expectations(Sw,_,_)) -> true ; true),
    assert($pd_hyperexpectations(Sw,Values,Es)),!,
    $pp_decode_update_switches_b(Sws).

$pp_decode_switch_name([sw_ins(Sid,_,_,_)|_SwInstances],Sw) :-
    $pc_prism_sw_ins_term(Sid,msw(Sw,_)).  % only uses the first element

$pp_decode_switch_instances([],[]).
$pp_decode_switch_instances([sw_ins(Sid,Prob,Delta,Expect)|SwInstances],
                            [(V,Prob,Delta,Expect)|Updates]) :-
    $pc_prism_sw_ins_term(Sid,msw(_,V)),!,
    $pp_decode_switch_instances(SwInstances,Updates).

$pp_separate_updates([],[],[],[],_Updates).
$pp_separate_updates([V|Vs],[Prob|Probs],[Delta|Deltas],[E|Es],Updates) :-
    member((V,Prob,Delta,E),Updates),!,
    $pp_separate_updates(Vs,Probs,Deltas,Es,Updates).

%% [NOTE] Non-ground goals has already been replaced by dummy goals, so all
%%        goals are ground here.

$pp_observed_facts([],[],Len,Len,NGoals,NGoals,FailRootIndex,FailRootIndex).
$pp_observed_facts([goal(Goal,Count)|GoalCountPairs],GidCountPairs,
                   Len0,Len,NGoals0,NGoals,FailRootIndex0,FailRootIndex) :-
    % fails if the goal is ground but has no proof
    ( $pc_prism_goal_id_get(Goal,Gid) ->
        ( Goal == failure ->
            NGoals1 = NGoals0,
            FailRootIndex1 = Len0
        ; NGoals1 is NGoals0 + Count,
          FailRootIndex1 = FailRootIndex0
        ),
        GidCountPairs = [goal(Gid,Count)|GidCountPairs1],
        Len1 is Len0 + 1
    ; $pp_raise_unexpected_failure($pp_observed_facts/8)
    ),!,
    $pp_observed_facts(GoalCountPairs,GidCountPairs1,
                       Len1,Len,NGoals1,NGoals,FailRootIndex1,FailRootIndex).

%% Assumption: for any pair of terms F and F' (F's variant), hash codes for
%% F and F' are equal.
%%
%% For convenience on implementation of parallel learning, $pp_trans_goals/3
%% is (internally) split into two predicates $pp_build_count_pairs/2 and
%% $pp_trans_count_pairs/3.
%%
%% The order of goal-count pairs may differ at every run due to the way of
%% implemention of hashtables.

$pp_trans_goals(Goals,GoalCountPairs,AllGoals) :-
    $pp_build_count_pairs(Goals,Pairs),
    $pp_trans_count_pairs(Pairs,GoalCountPairs,AllGoals).

$pp_build_count_pairs(Goals,Pairs) :-
    new_hashtable(Table),
    $pp_count_goals(Goals,Table),
    hashtable_to_list(Table,Pairs0),
    sort(Pairs0,Pairs).

$pp_count_goals([],_).
$pp_count_goals([G0|Goals],Table) :-
    ( G0 = goal(Goal,Count)  -> true
    ; G0 = count(Goal,Count) -> true
    ; G0 = (Count times Goal) -> true
    ; Goal = G0, Count = 1
    ),
    ( ground(Goal) -> GoalCp = Goal
    ; copy_term(Goal,GoalCp)
    ),
    ( $pp_hashtable_get(Table,GoalCp,Count0) ->
        Count1 is Count0 + Count,
        $pp_hashtable_put(Table,GoalCp,Count1)
    ; $pp_hashtable_put(Table,GoalCp,Count)
    ),!,
    $pp_count_goals(Goals,Table).

$pp_trans_count_pairs([],[],[]).
$pp_trans_count_pairs([Goal=Count|Pairs],GoalCountPairs,AllGoals) :-
    $pp_build_dummy_goal(Goal,DummyGoal),
    GoalCountPairs = [goal(DummyGoal,Count)|GoalCountPairs1],
    AllGoals = [DummyGoal|AllGoals1],!,
    $pp_trans_count_pairs(Pairs,GoalCountPairs1,AllGoals1).

$pp_build_dummy_goal(Goal,DummyGoal) :-
    ( Goal = msw(I,V) ->
        ( ground(I) -> I = ICp ; copy_term(I,ICp) ),
        ( ground(V) -> V = VCp ; copy_term(V,VCp) ),
        $pp_create_dummy_goal(DummyGoal),
        $pp_assert_dummy_goal(DummyGoal,Goal),
        Clause = (DummyGoal :- $prism_expl_msw(ICp,VCp,Sid),
                               $pc_prism_goal_id_register(DummyGoal,Hid),
                               $prism_eg_path(Hid,[],[Sid])),
        Prog = [pred(DummyGoal,0,_,_,tabled(_,_,_,_),[Clause]),
                pred($damon_load,0,_,_,_,[($damon_load:-true)])],
        consult_preds([],Prog)
    ; ground(Goal) ->
        DummyGoal = Goal        % don't create dummy goals (wrappers) for
    ;                           % ground goals to save memory.
        $pp_create_dummy_goal(DummyGoal),
        $pp_assert_dummy_goal(DummyGoal,Goal),
        ( $pp_trans_one_goal(Goal,CompGoal) -> BodyGoal = CompGoal
        ; BodyGoal = (savecp(CP),Depth=0,
                      $pp_expl_interp_goal(Goal,Depth,CP,[],_,[],_,[],_,[],_))
        ),
        Clause = (DummyGoal:-BodyGoal,
                             $pc_prism_goal_id_register(Goal,GId),
                             $pc_prism_goal_id_register(DummyGoal,HId),
                             $prism_eg_path(HId,[GId],[])),
        Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),[Clause]),
                pred($damon_load,0,_,_,_,[($damon_load:-true)])],
        consult_preds([],Prog)
    ),!.

$pp_assert_dummy_goal(DummyGoal,OrigGoal) :-
    assertz($pd_dummy_goal_table(DummyGoal,OrigGoal)),!.

$pp_clean_dummy_goal_table :-
    retractall($pd_dummy_goal_table(_,_)).

%%----------------------------------------

% just make a simple check
$pp_require_observed_data(Gs,MsgID,Source) :-
    ( $pp_test_observed_data(Gs) -> true
    ; $pp_raise_on_require([Gs],MsgID,Source,$pp_error_observed_data)
    ).

$pp_test_observed_data(Gs) :-
    nonvar(Gs),
    ( Gs = [failure] -> fail
    ; Gs = [_|_]
    ).

$pp_error_observed_data(Gs,Error) :-
    $pp_error_nonvar(Gs,Error), !.
$pp_error_observed_data(Gs,domain_error(observed_data,Gs)) :-
    ( Gs = [failure] ; Gs \= [_|_] ), !.

