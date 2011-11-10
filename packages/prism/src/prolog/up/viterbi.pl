%%%% Viterbi wrappers

viterbi(G) :-
    $pp_viterbi_wrapper(viterbi(G)).
viterbi(G,P) :-
    $pp_viterbi_wrapper(viterbi(G,P)).
viterbif(G) :-
    $pp_viterbi_wrapper(viterbif(G)).
viterbif(G,P,V) :-
    $pp_viterbi_wrapper(viterbif(G,P,V)).
viterbit(G) :-
    $pp_viterbi_wrapper(viterbit(G)).
viterbit(G,P,T) :-
    $pp_viterbi_wrapper(viterbit(G,P,T)).
n_viterbi(N,G) :-
    $pp_viterbi_wrapper(n_viterbi(N,G)).
n_viterbi(N,G,P) :-
    $pp_viterbi_wrapper(n_viterbi(N,G,P)).
n_viterbif(N,G) :-
    $pp_viterbi_wrapper(n_viterbif(N,G)).
n_viterbif(N,G,V) :-
    $pp_viterbi_wrapper(n_viterbif(N,G,V)).
n_viterbit(N,G) :-
    $pp_viterbi_wrapper(n_viterbit(N,G)).
n_viterbit(N,G,T) :-
    $pp_viterbi_wrapper(n_viterbit(N,G,T)).
viterbig(G) :-
    $pp_viterbi_wrapper(viterbig(G)).
viterbig(G,P) :-
    $pp_viterbi_wrapper(viterbig(G,P)).
viterbig(G,P,V) :-
    $pp_viterbi_wrapper(viterbig(G,P,V)).
n_viterbig(N,G) :-
    $pp_viterbi_wrapper(n_viterbig(N,G)).
n_viterbig(N,G,P) :-
    $pp_viterbi_wrapper(n_viterbig(N,G,P)).
n_viterbig(N,G,P,V) :-
    $pp_viterbi_wrapper(n_viterbig(N,G,P,V)).

$pp_viterbi_wrapper(Pred0) :-
    get_prism_flag(viterbi_mode,Mode),
    ( Mode == params -> Suffix = '_p' ; Mode == hparams -> Suffix = '_h' ),!,
    Pred0 =.. [Name0|Args],
    atom_concat(Name0,Suffix,Name1),
    Pred1 =.. [Name1|Args],!,
    call(Pred1).  % do not add cut here (n_viterbig is non-deterministic)

%%%% Viterbi routine with C interface
%%
%% viterbi_p(G) :- print the Viterbi prob
%% viterbi_p(G,P) :- output the Viterbi prob
%% viterbif_p(G) :- print the Viterbi path and the Viterbi prob
%% viterbif_p(G,P,VPath) :- output the Viterbi path and the Viterbi prob
%%
%% VPath is a list of node(G,Paths), where Paths is a list of
%% path(Gs,Sws), where Gs are subgoals of G and Sws are switches.
%%
%% Usually in VPath, node(msw(Sw,V),[]) is omitted, but optionally
%% it can be included in VPath.

% Main routine:

% viterbi family:

viterbi_p(Goal) :-
    viterbif_p(Goal,Pmax,_),
    $pp_print_viterbi_prob(Pmax).

viterbi_p(Goal,Pmax) :-
    viterbif_p(Goal,Pmax,_).

% viterbif family:

viterbif_p(Goal) :-
    viterbif_p(Goal,Pmax,VNodeL),
    format("~n",[]),
    print_graph(VNodeL,[lr('<=')]),
    $pp_print_viterbi_prob(Pmax).

viterbif_p(Goal,Pmax,VNodeL) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),viterbif_p/3),
    ( Goal = msw(I,_) ->
        $pp_require_ground(I,$msg(0101),viterbif_p/3),
        $pp_require_switch_outcomes(I,$msg(0102),viterbif_p/3)
    ; true
    ),
    $pp_viterbif_p(Goal,Pmax,VNodeL).

$pp_viterbif_p(Goal,Pmax,VNodeL) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_viterbi_core(Goal,Pmax,VNodeL),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_assert_viterbi_stats1(InfTime),!.

% viterbit family:

viterbit_p(Goal) :-
    viterbit_p(Goal,Pmax,VTreeL),
    format("~n",[]),
    print_tree(VTreeL),
    $pp_print_viterbi_prob(Pmax).

viterbit_p(Goal,Pmax,VTreeL) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),viterbit_p/3),
    $pp_viterbif_p(Goal,Pmax,VNodeL),
    viterbi_tree(VNodeL,VTreeL).

% viterbig family:

viterbig_p(Goal) :-
    ( ground(Goal) -> viterbi_p(Goal)
    ; viterbig_p(Goal,_,_)
    ).

viterbig_p(Goal,Pmax) :-
    ( ground(Goal) -> viterbi_p(Goal,Pmax)
    ; viterbig_p(Goal,Pmax,_)
    ).

viterbig_p(Goal,Pmax,VNodeL) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),viterbig_p/3),
    ( Goal = msw(I,_) ->
        $pp_require_ground(I,$msg(0101),viterbif_p/3),
        $pp_require_switch_outcomes(I,$msg(0102),viterbig_p/3)
    ; true
    ),
    $pp_viterbig_p(Goal,Pmax,VNodeL).

$pp_viterbig_p(Goal,Pmax,VNodeL) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_viterbi_core(Goal,Pmax,VNodeL),
    ( ground(Goal) -> true
    ; VNodeL = [node(_,[path([Goal1],[])])|_] -> Goal = Goal1
    ; VNodeL = [node(_,[path([],[SwIns])])|_] -> Goal = SwIns
    ),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_assert_viterbi_stats1(InfTime),!.

%% Common routine:

$pp_print_viterbi_prob(Pmax) :-
    ( get_prism_flag(log_scale,off) -> format("~nViterbi_P = ~15f~n",[Pmax])
    ; format("~nlog(Viterbi_P) = ~15f~n",[Pmax])
    ).

$pp_viterbi_core(Goal,Pmax,VNodeL) :-
    Goal = msw(I,V),!,
    $pp_require_ground(I,$msg(0101),$pp_viterbi_core/3),
    $pp_require_switch_outcomes(I,$msg(0102),$pp_viterbi_core/3),
    ( ground(V) -> V = VCp ; copy_term(V,VCp) ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = ($prism_expl_msw(I,VCp,Sid),
                 $pc_prism_goal_id_register(DummyGoal,Hid),
                 $prism_eg_path(Hid,[],[Sid])),
    Prog = [pred(DummyGoal,0,_,_,tabled(_,_,_,_),[(DummyGoal:-DummyBody)])],
    consult_preds([],Prog),
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_viterbi_p(DummyGoal,Pmax,[node(DummyGoal,Paths)|VNodeL0]),!,
    cputime(T3),
    VNodeL = [node(msw(I,V),Paths)|VNodeL0],
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),
    $pp_delete_tmp_out,!.

$pp_viterbi_core(Goal,Pmax,VNodeL) :-
    ground(Goal),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pp_compute_viterbi_p(Goal,Pmax,VNodeL),!,
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_viterbi_core(Goal,Pmax,VNodeL) :-
    copy_term(Goal,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = (BodyGoal,
                 $pc_prism_goal_id_register(GoalCp,GId),
                 $pc_prism_goal_id_register(DummyGoal,HId),
                 $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),
                 [(DummyGoal:-DummyBody)])],
    consult_preds([],Prog),
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_viterbi_p(DummyGoal,Pmax,[node(DummyGoal,Paths)|VNodeL0]),!,
    cputime(T3),
    VNodeL = [node(Goal,Paths)|VNodeL0],
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),
    $pp_delete_tmp_out,!.

% Sws = [sw(Id,Instances,Probs,PseudoCs,Fixed,FixedH),...]
$pp_compute_viterbi_p(Goal,Pmax,VNodeL) :-
    $pp_collect_sw_info(Sws),
    $pc_export_sw_info(Sws),
    $pc_prism_goal_id_get(Goal,Gid),
    garbage_collect,
    $pc_compute_viterbi(Gid,EGs,EGPaths,ESwPaths,Pmax),
    $pp_decode_viterbi_path(EGs,EGPaths,ESwPaths,VNodeL),!.

$pp_decode_viterbi_path([],[],[],[]) :- !.
$pp_decode_viterbi_path([Gid|Gids],[GPath|GPaths],[SPath|SPaths],[Node|Nodes]) :-
    $pc_prism_goal_term(Gid,G),
    ( GPath == [], SPath == [] ->
        get_prism_flag(explicit_empty_expls,V),
        ( V == off -> Node = node(G,[])
        ; Node = node(G,[path([],[])])
        )
    ; $pp_decode_gnodes(GPath,GPathDec,1,0,_Vg),
      $pp_decode_snodes(SPath,SPathDec,1,0,_Vs),
      Node = node(G,[path(GPathDec,SPathDec)])
    ),!,
    $pp_decode_viterbi_path(Gids,GPaths,SPaths,Nodes).


%%%%
%%%%  Top-N Viterbi
%%%%
%%%% n_viterbi_p(N,G) :- print the top-N Viterbi probs
%%%% n_viterbi_p(N,G,Ps) :- output the top-N Viterbi probs
%%%% n_viterbif_p(N,G) :- print the top-N Viterbi paths and the corresponding
%%%%                     Viterbi probs
%%%% n_viterbif_p(N,G,VPathL) :- output the list of top-N Viterbi paths and
%%%%                            the corresponding Viterbi probs
%%%%

% n_viterbi family

n_viterbi_p(N,Goal) :-
    n_viterbif_p(N,Goal,VPathL),
    ( member(v_expl(J,Pmax,_),VPathL),
      $pp_print_n_viterbi(J,Pmax),
      fail
    ; true
    ).

n_viterbi_p(N,Goal,Ps) :-
    n_viterbif_p(N,Goal,VPathL),!,
    findall(Pmax,member(v_expl(_,Pmax,_),VPathL),Ps).

% n_viterbif family

n_viterbif_p(N,Goal) :-
    n_viterbif_p(N,Goal,VPathL),!,
    $pp_print_n_viterbif(VPathL).

n_viterbif_p(N,Goal,VPathL) :-
    $pp_require_positive_integer(N,$msg(1400),n_viterbif_p/3),
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),n_viterbif_p/3),
    $pp_n_viterbif_p(N,Goal,VPathL).

$pp_n_viterbif_p(N,Goal,VPathL) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_n_viterbi_p_core(N,Goal,VPathL),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_assert_viterbi_stats1(InfTime),!.

% n_viterbit family

n_viterbit_p(N,Goal) :-
    n_viterbif_p(N,Goal,VPathL),!,
    $pp_print_n_viterbit(VPathL).

n_viterbit_p(N,Goal,VPathL) :-
    n_viterbif_p(N,Goal,VPathL0),!,
    $pp_build_n_viterbit(VPathL0,VPathL).

%%%% 
%%%% $pp_n_viterbig_p(N,Goal) :- the same as $pp_n_viterbig_p(N,Goal,_,_)
%%%% $pp_n_viterbig_p(N,Goal,Pmax) :- the same as $pp_n_viterbig_p(N,Goal,Pmax,_)
%%%% $pp_n_viterbig_p(N,Goal,Pmax,VNodeL) :-
%%%%      if Goal is not ground, unify Goal with the first element in the K-th
%%%%      Viterbi path VNodeL (K=0,1,2,...,(N-1) on backtracking). Pmax is the
%%%%      probability of VNodeL.
%%%%

n_viterbig_p(N,Goal) :-
    ( ground(Goal) -> n_viterbi_p(N,Goal)
    ; n_viterbig_p(N,Goal,_,_)
    ).

n_viterbig_p(N,Goal,Pmax) :-
    ( ground(Goal) -> n_viterbi_p(N,Goal,Ps),!,member(Pmax,Ps)
    ; n_viterbig_p(N,Goal,Pmax,_)
    ).

n_viterbig_p(N,Goal,Pmax,VNodeL) :-
    $pp_require_positive_integer(N,$msg(1400),n_viterbi_p/3),
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),n_viterbi_p/3),
    $pp_n_viterbig_p(N,Goal,Pmax,VNodeL).

$pp_n_viterbig_p(N,Goal,Pmax,VNodeL) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_n_viterbi_p_core(N,Goal,VPathL),!,
    cputime(T1),
    InfTime is T1 - T0,
    $pp_assert_viterbi_stats1(InfTime),!,
    ( ground(Goal) -> member(v_expl(J,Pmax,VNodeL),VPathL)
    ; Goal = msw(_,_) ->
        member(v_expl(J,Pmax,VNodeL),VPathL),
        VNodeL = [node(_,[path([],[SwIns])])|_],
        Goal = SwIns
    ; % else
        member(v_expl(J,Pmax,VNodeL),VPathL),
        VNodeL = [node(_,[path([Goal1],[])])|_],
        Goal = Goal1
    ).

%% Common routines:

$pp_print_n_viterbi(J,Pmax) :-
    ( get_prism_flag(log_scale,off) ->
          format("#~w: Viterbi_P = ~15f~n",[J,Pmax])
    ; format("#~w: log(Viterbi_P) = ~15f~n",[J,Pmax])
    ).

$pp_print_n_viterbif([]).
$pp_print_n_viterbif([v_expl(J,Pmax,VNodeL)|VPathL]) :-
    format("~n#~w~n",[J]),
    print_graph(VNodeL,[lr('<=')]),
    ( get_prism_flag(log_scale,off) -> format("~nViterbi_P = ~15f~n",[Pmax])
    ; format("~nlog(Viterbi_P) = ~15f~n",[Pmax])
    ),!,
    $pp_print_n_viterbif(VPathL).

$pp_print_n_viterbit([]).
$pp_print_n_viterbit([v_expl(J,Pmax,VNodeL)|VPathL]) :-
    format("~n#~w~n",[J]),
    viterbi_tree(VNodeL,VTreeL),
    print_tree(VTreeL),
    $pp_print_viterbi_prob(Pmax),!,
    $pp_print_n_viterbit(VPathL).

$pp_build_n_viterbit([],[]).
$pp_build_n_viterbit([v_expl(J,Pmax,VNodeL)|VPathL0],
                     [v_tree(J,Pmax,VTreeL)|VPathL1]) :-
    viterbi_tree(VNodeL,VTreeL),!,
    $pp_build_n_viterbit(VPathL0,VPathL1).

$pp_n_viterbi_p_core(N,Goal,VPathL) :-
    Goal = msw(I,V),!,
    $pp_require_ground(I,$msg(0101),$pp_viterbi_core/3),
    $pp_require_switch_outcomes(I,$msg(0102),$pp_viterbi_core/3),
    ( ground(V) -> V = VCp ; copy_term(V,VCp) ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = ($prism_expl_msw(I,VCp,Sid),
                 $pc_prism_goal_id_register(DummyGoal,Hid),
                 $prism_eg_path(Hid,[],[Sid])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),
                 [(DummyGoal:-DummyBody)])],
    consult_preds([],Prog),
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_n_viterbi_p(N,DummyGoal,VPathL0),!,
    cputime(T3),
    $pp_replace_dummy_goal(Goal,DummyGoal,VPathL0,VPathL),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),
    $pp_delete_tmp_out,!.

$pp_n_viterbi_p_core(N,Goal,VPathL) :-
    ground(Goal),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pp_compute_n_viterbi_p(N,Goal,VPathL),!,
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_n_viterbi_p_core(N,Goal,VPathL) :-
    copy_term(Goal,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = (BodyGoal,
                 $pc_prism_goal_id_register(GoalCp,GId),
                 $pc_prism_goal_id_register(DummyGoal,HId),
                 $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),
                 [(DummyGoal:-DummyBody)])],
    consult_preds([],Prog),
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_n_viterbi_p(N,DummyGoal,VPathL0),!,
    cputime(T3),
    $pp_replace_dummy_goal(Goal,DummyGoal,VPathL0,VPathL),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),
    $pp_delete_tmp_out,!.


$pp_compute_n_viterbi_p(N,Goal,VPathL) :-
    $pp_collect_sw_info(Sws),
    $pc_export_sw_info(Sws),
    $pc_prism_goal_id_get(Goal,Gid),
    garbage_collect,
    $pc_compute_n_viterbi(N,Gid,VPathL0),
    $pp_build_n_viterbi_path(VPathL0,VPathL),!.

$pp_replace_dummy_goal(_,_,[],[]).
$pp_replace_dummy_goal(Goal,DummyGoal,
                       [v_expl(J,Pmax,VNodeL0)|VPathL0],
                       [v_expl(J,Pmax,VNodeL)|VPathL]) :-
    VNodeL0 = [node(DummyGoal,Paths)|VNodeL1],
    VNodeL  = [node(Goal,Paths)|VNodeL1],!,
    $pp_replace_dummy_goal(Goal,DummyGoal,VPathL0,VPathL).

$pp_build_n_viterbi_path([],[]).
$pp_build_n_viterbi_path([v_expl(J,EGs,EGPaths,ESwPaths,Pmax)|VPathL0],
                    [v_expl(J,Pmax,VNodeL)|VPathL]) :-
    $pp_decode_viterbi_path(EGs,EGPaths,ESwPaths,VNodeL),
    $pp_build_n_viterbi_path(VPathL0,VPathL).

%%  Viterbi with reranking based on VB
%%
%% viterbi_h(G) :- the same as n_viterbi_h([1,default],G)
%% viterbi_h(G,P) :- the same as n_viterbi_h([1,default],G,P)
%% viterbif_h(G) :- the same as n_viterbif_h([1,default],G)
%% viterbif_h(G,P,VPath) :- the same as
%%                          n_viterbif_h([1,default],[v_expl(0,P,VPath)])
%%
%% n_viterbi_h(N,G) :- the same as n_viterbi_h([N,default],G)
%% n_viterbi_h(N,G,Ps) :- the same as n_viterbi_h([N,default],G,Ps)
%% n_viterbi_h([N,M],G) :- print top-N Viterbi probs selected from top-M
%%                         Viterbi probs based on ML/MAP (M > N)
%% n_viterbi_h([N,M],G,Ps) :- output top-N Viterbi probs selected from top-M
%%                            Viterbi probs based on ML/MAP (M > N)
%% n_viterbif_h(N,G) :- the same as n_viterbif_h([N,default],G)
%% n_viterbif_h(N,G,VPathL) :- the same as n_viterbif_h([N,default],G,VPathL)
%% n_viterbif_h([N,M],G) :- print the top-N Viterbi paths and the corresponding
%%                         Viterbi probs selected from the top-N Viterbi paths
%%                         based on ML/MAP (M > N)
%% n_viterbif_h([N,M],G,VPathL) :-
%%         output the list of the top-N Viterbi paths and the corresponding
%%         Viterbi probs selected from top-N Viterbi paths based on ML/MAP
%%         (M =< N)
%%
%% viterbig_h(Goal) :- the same as n_viterbig_h(1,Goal)
%% viterbig_h(Goal,Pmax) :- the same as n_viterbig_h(1,Goal,Pmax)
%% viterbig_h(Goal,Pmax,VNodeL) :- the same as n_viterbig_h(1,Goal,Pmax,VNodeL)
%%
%% n_viterbig_h(N,Goal) :- the same as n_viterbig_h(N,Goal,_,_)
%% n_viterbig_h([N,M],Goal) :- the same as n_viterbig_h([N,M],Goal,_,_)
%% n_viterbig_h(N,Goal,Pmax) :- the same as n_viterbig_h(N,Goal,Pmax,_)
%% n_viterbig_h([N,M],Goal,Pmax) :- the same as n_viterbig_h([N,M],Goal,Pmax,_)
%% n_viterbig_h(N,Goal,Pmax) :-
%%         the same as n_viterbig_h([N,default],Goal,Pmax,_)
%% n_viterbig_h(N,Goal,Pmax,VNodeL) :-
%%         the same as n_viterbig_h([N,default],Goal,Pmax,VNodeL)
%% n_viterbig_h([N,M],Goal,Pmax,VNodeL) :-
%%         If Goal is not ground, unify Goal with the first element in the K-th
%%         Viterbi path VNodeL (K=1,2,... on backtracking). Pmax is the
%%         probability of VNodeL.

viterbi_h(G)   :- n_viterbi_h([1,default],G).
viterbi_h(G,P) :- n_viterbi_h([1,default],G,[P]).
viterbif_h(G)  :- n_viterbif_h([1,default],G).
viterbif_h(G,P,VPath) :- n_viterbif_h([1,default],G,[v_expl(0,P,VPath)]).
viterbit_h(G) :- n_viterbit_h([1,default],G).
viterbit_h(G,P,VTree) :-
    n_viterbif_h([1,default],G,[v_expl(0,P,VPath)]),!,
    viterbi_tree(VPath,VTree).

n_viterbi_h([N,M],G) :- !,
    n_viterbif_h([N,M],G,VPathL),!,
    ( member(v_expl(J,Pmax,_),VPathL),
      $pp_print_n_viterbi(J,Pmax),
      fail
    ; true
    ).
n_viterbi_h(N,G) :- n_viterbi_h([N,default],G).

n_viterbi_h([N,M],G,Ps) :- !,
    n_viterbif_h([N,M],G,VPathL),!,
    findall(Pmax,member(v_expl(_,Pmax,_),VPathL),Ps).
n_viterbi_h(N,G,Ps) :- n_viterbi_h([N,default],G,Ps).

n_viterbif_h([N,M],G) :- !,
    n_viterbif_h([N,M],G,VPathL),!,
    $pp_print_n_viterbif(VPathL).
n_viterbif_h(N,G) :-
    n_viterbif_h([N,default],G).

n_viterbif_h([N,M],Goal,VPathL) :- !,
    ( M == default ->
        get_prism_flag(rerank,M1),!,
        n_viterbif_h([N,M1],Goal,VPathL)
    ; % M \== default
        $pp_require_positive_integer(N,$msg(1400),n_viterbif_h/3),
        $pp_require_positive_integer(M,$msg(1401),n_viterbif_h/3),
        $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),n_viterbif_h/3),
        ( N > M -> N1 = M ; N1 = N ),!,
        $pp_n_viterbif_h([N1,M],Goal,VPathL)
    ).

n_viterbif_h(N,G,VPathL) :-
    n_viterbif_h([N,default],G,VPathL).

$pp_n_viterbif_h([N,M],Goal,VPathL) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_n_viterbi_h_core(N,M,Goal,VPathL),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_assert_viterbi_stats1(InfTime),!.

n_viterbit_h([N,M],G) :- !,
    n_viterbif_h([N,M],G,VPathL),!,
    $pp_print_n_viterbit(VPathL).
n_viterbit_h(N,G) :-
    n_viterbit_h([N,default],G).

n_viterbit_h([N,M],G,VPathL) :- !,
    n_viterbif_h([N,M],G,VPathL0),!,
    $pp_build_n_viterbit(VPathL0,VPathL).
n_viterbit_h(N,G,VPathL) :-
    n_viterbit_h([N,default],G,VPathL).

viterbig_h(Goal) :- n_viterbig_h(1,Goal).
viterbig_h(Goal,Pmax) :- n_viterbig_h(1,Goal,Pmax).
viterbig_h(Goal,Pmax,VNodeL) :- n_viterbig_h(1,Goal,Pmax,VNodeL).

n_viterbig_h([N,M],Goal) :- !,
    ( ground(Goal) -> n_viterbi_h([N,M],Goal)
    ; n_viterbig_h([N,M],Goal,_,_)
    ).
n_viterbig_h(N,Goal) :-
    ( ground(Goal) -> n_viterbi_h(N,Goal)
    ; n_viterbig_h(N,Goal,_,_)
    ).

n_viterbig_h([N,M],Goal,Pmax) :- !,
    ( ground(Goal) -> n_viterbi_h([N,M],Goal,Ps),!,member(Pmax,Ps)
    ; n_viterbig_h([N,M],Goal,Pmax,_)
    ).
n_viterbig_h(N,Goal,Pmax) :-
    ( ground(Goal) -> n_viterbi_h(N,Goal,Ps),!,member(Pmax,Ps)
    ; n_viterbig_h(N,Goal,Pmax,_)
    ).

n_viterbig_h([N,default],Goal,Pmax,VNodeL) :- !,
    get_prism_flag(rerank,M),!,
    n_viterbig_h([N,M],Goal,Pmax,VNodeL).
n_viterbig_h([N,M],Goal,Pmax,VNodeL) :- !,
    $pp_require_positive_integer(N,$msg(1400),n_viterbig_h/3),
    $pp_require_positive_integer(M,$msg(1401),n_viterbig_h/3),
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),n_viterbig_h/3),
    ( N > M -> N1 = M ; N1 = N ),!,
    $pp_n_viterbig_h([N1,M],Goal,Pmax,VNodeL).
n_viterbig_h(N,Goal,Pmax,VNodeL) :-
    n_viterbig_h([N,default],Goal,Pmax,VNodeL).

$pp_n_viterbig_h([N,M],Goal,Pmax,VNodeL) :- !,
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_n_viterbi_h_core(N,M,Goal,VPathL),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_assert_viterbi_stats1(InfTime),!,
    ( ground(Goal) -> member(v_expl(J,Pmax,VNodeL),VPathL)
    ; Goal = msw(_,_) ->
        member(v_expl(J,Pmax,VNodeL),VPathL),
        VNodeL = [node(_,[path([],[SwIns])])|_],
        Goal = SwIns
    ; % else
        member(v_expl(J,Pmax,VNodeL),VPathL),
        VNodeL = [node(_,[path([Goal1],[])])|_],
        Goal = Goal1
    ).

%% Common routines:

$pp_n_viterbi_h_core(N,M,Goal,VPathL) :-
    Goal = msw(I,V),!,
    $pp_require_ground(I,$msg(0101),$pp_viterbi_core/3),
    $pp_require_switch_outcomes(I,$msg(0102),$pp_viterbi_core/3),
    ( ground(V) -> V = VCp ; copy_term(V,VCp) ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = ($prism_expl_msw(I,VCp,Sid),
                 $pc_prism_goal_id_register(DummyGoal,Hid),
                 $prism_eg_path(Hid,[],[Sid])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),
                 [(DummyGoal:-DummyBody)])],
    consult_preds([],Prog),
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_n_viterbi_h(N,M,DummyGoal,VPathL0),!,
    cputime(T3),
    $pp_replace_dummy_goal(Goal,DummyGoal,VPathL0,VPathL),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),
    $pp_delete_tmp_out,!.

$pp_n_viterbi_h_core(N,M,Goal,VPathL) :-
    ground(Goal),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pp_compute_n_viterbi_h(N,M,Goal,VPathL),!,
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_n_viterbi_h_core(N,M,Goal,VPathL) :-
    copy_term(Goal,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = (BodyGoal,
                 $pc_prism_goal_id_register(GoalCp,GId),
                 $pc_prism_goal_id_register(DummyGoal,HId),
                 $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),
                 [(DummyGoal:-DummyBody)])],
    consult_preds([],Prog),
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_n_viterbi_h(N,M,DummyGoal,VPathL0),!,
    cputime(T3),
    $pp_replace_dummy_goal(Goal,DummyGoal,VPathL0,VPathL),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),
    $pp_delete_tmp_out,!.

$pp_compute_n_viterbi_h(N,M,Goal,VPathL) :-
    $pp_collect_sw_info(Sws),
    $pc_export_sw_info(Sws),
    $pc_prism_goal_id_get(Goal,Gid),
    garbage_collect,
    $pc_compute_n_viterbi_rerank(N,M,Gid,VPathL0),
    $pp_build_n_viterbi_path(VPathL0,VPathL),!.

%% Statistics

$pp_assert_viterbi_stats1(InfTime0) :-
    InfTime is InfTime0 / 1000.0,
    assertz($ps_infer_time(InfTime)),!.

$pp_assert_viterbi_stats2(SearchTime0,NumCompTime0) :-
    SearchTime  is SearchTime0  / 1000.0,
    NumCompTime is NumCompTime0 / 1000.0,
    assertz($ps_infer_search_time(SearchTime)),
    assertz($ps_infer_calc_time(NumCompTime)),!.

%%----------------------------------------
%%  e-graph -> tree

viterbi_tree(EG,Tree) :-
    $pp_require_list(EG,$msg(2104),viterbi_tree/2),
    new_hashtable(HT),
    $pp_viterbi_tree(EG,Tree,HT).

$pp_viterbi_tree([],[],_).
$pp_viterbi_tree([Node|Nodes],Tree,HT), Node = node(Name,[]) =>
    Tree = Name,
    $pp_viterbi_tree_register(Name,Tree,HT),!,
    $pp_viterbi_tree(Nodes,_,HT).
$pp_viterbi_tree([Node|Nodes],Tree,HT), Node = node(Name,[path(Gs,Ss)]) =>
    Tree = [Name|L0],
    $pp_viterbi_tree_goals(Gs,L0,L1,HT),
    $pp_viterbi_tree_swits(Ss,L1,[],HT),
    $pp_viterbi_tree_register(Name,Tree,HT),!,
    $pp_viterbi_tree(Nodes,_,HT).

$pp_viterbi_tree_goals([],L,L,_).
$pp_viterbi_tree_goals([G|Gs],[Node|L0],L1,HT) :-
    $pp_viterbi_tree_register(G,Node,HT),!, % Node = free var.
    $pp_viterbi_tree_goals(Gs,L0,L1,HT).

$pp_viterbi_tree_swits([],L,L,_).
$pp_viterbi_tree_swits([S|Ss],[Node|L0],L1,HT) :-
    Node = S,!,
    $pp_viterbi_tree_swits(Ss,L0,L1,HT).

$pp_viterbi_tree_register(Name,Node,HT) :-
    hashtable_get(HT,Name,V),!,
    ( V = Node -> true
    ; $pp_raise_unmatched_branches($pp_viterbi_tree_register/3)
    ).
$pp_viterbi_tree_register(Name,Node,HT) :-
    hashtable_put(HT,Name,Node).

%%----------------------------------------
%%  e-graph -> list of subgoals, list of switches

viterbi_subgoals(VNodes,Goals) :-
    $pp_require_list(VNodes,$msg(2104),viterbi_subgoals/2),
    $pp_viterbi_subgoals(VNodes,Goals).

$pp_viterbi_subgoals([],[]).
$pp_viterbi_subgoals([node(_,[])|Nodes],Ys) :- !,
    $pp_viterbi_subgoals(Nodes,Ys).
$pp_viterbi_subgoals([node(_,[path(Xs,_)])|Nodes],Ys) :-
    append(Xs,Ys1,Ys),!,
    $pp_viterbi_subgoals(Nodes,Ys1).

viterbi_switches(VNodes,Goals) :-
    $pp_require_list(VNodes,$msg(2104),viterbi_switches/2),
    $pp_viterbi_switches(VNodes,Goals).

$pp_viterbi_switches([],[]).
$pp_viterbi_switches([node(_,[])|Nodes],Ys) :- !,
    $pp_viterbi_switches(Nodes,Ys).
$pp_viterbi_switches([node(_,[path(_,Xs)])|Nodes],Ys) :-
    append(Xs,Ys1,Ys),!,
    $pp_viterbi_switches(Nodes,Ys1).
