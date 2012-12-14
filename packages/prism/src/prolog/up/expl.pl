%%
%%  expl.pl: routines for explanation search
%%
%%  $pp_find_explanations(Goals) constructs the explanation graphs for Goals.
%%  An explanation graph is a directed hype-graph where each hype-arc takes
%%  the form of:
%%
%%  $prism_eg_path(GoalId,Children,SWs)
%%  
%%  where 
%%    GoalId: 
%%         the id of the source node (all variant subgoals have the same ID)
%%    Children:
%%         the list of nodes that are connected by the hype-arc with GoalID
%%    SWs:
%%         the list of switches associated with the arc.
%%  
%%  consider the following PRISM program:
%%
%%    values(init,[s0,s1]).
%%    values(out(_),[a,b]).
%%    values(tr(_),[s0,s1]).
%%
%%    hmm(L) :- 
%%        msw(init,Si),
%%        hmm(1,Si,L).
%%
%%    hmm(T,S,[]) :- T>3.
%%    hmm(T,S,[C|L]) :-
%%        T=<3,
%%        msw(out(S),C),
%%        msw(tr(S),NextS),
%%        T1 is T + 1,
%%        hmm(T1,NextS,L).
%%
%%
%%  The relations for the goal hmm([a,b,a]) are as follows (where goals
%%  rather than their ids are shown for description purpose):
%%
%%    goal_id(hmm([a,b,a]),0),
%%    goal_id(hmm(1,s0,[a,b,a]),1)
%%    goal_id(hmm(2,s0,[b,a]),4)]
%%    goal_id(hmm(2,s1,[b,a]),11)
%%    goal_id(hmm(3,s0,[a]),7)
%%    goal_id(hmm(3,s1,[a]),9)
%%    goal_id(hmm(3,s2,[a]),14)
%%    goal_id(observe(1,s0,a),2)
%%    goal_id(observe(2,s0,b),5)
%%    goal_id(observe(2,s1,b),12)
%%    goal_id(observe(3,s0,a),8)
%%    goal_id(observe(3,s1,a),10)
%%    goal_id(observe(3,s2,a),15)
%%    goal_id(trans(1,s0,_5b0400),3)
%%    goal_id(trans(2,s0,_5b0480),6)
%%    goal_id(trans(2,s1,_5b04f0),13)
%%
%%    $prism_eg_path(3,[],[msw(trans(s0),1,s0)]),
%%    $prism_eg_path(6,[],[msw(trans(s0),2,s0)]),
%%    $prism_eg_path(12,[],[msw(obs(s1),2,b)]),
%%    $prism_eg_path(3,[],[msw(trans(s0),1,s1)]),
%%    $prism_eg_path(6,[],[msw(trans(s0),2,s1)]),
%%    $prism_eg_path(13,[],[msw(trans(s1),2,s1)]),
%%    $prism_eg_path(0,[1],[]),
%%    $prism_eg_path(7,[8],[]),
%%    $prism_eg_path(1,[4,3,2],[]),
%%    $prism_eg_path(13,[],[msw(trans(s1),2,s2)]),
%%    $prism_eg_path(4,[7,6,5],[]),
%%    $prism_eg_path(2,[],[msw(obs(s0),1,a)]),
%%    $prism_eg_path(8,[],[msw(obs(s0),3,a)]),
%%    $prism_eg_path(5,[],[msw(obs(s0),2,b)])]
%%
%%  One of the explanations for hmm([a,b,a]) is:
%%    
%%    [msw(init,once,s0),msw(out(s0),1,a),msw(tr(s0),1,s0),msw(out(s0),2,b),...]
%%

$pp_find_explanations(Goals) :-
    $pp_expl_goals_all(Goals).

$pp_expl_failure :-    
    $pp_trans_one_goal(failure,CompGoal),!,
    call(CompGoal).
$pp_expl_failure :-
    savecp(CP),
    Depth = 0,
    $pp_expl_interp_goal(failure,Depth,CP,[],_,[],_,[],_,[],_).
    
$pp_expl_goals_all(Goals) :-
    $pp_expl_goals(Goals).

$pp_expl_goals([]) => true.
$pp_expl_goals([Goal|Goals]) =>
    $pp_learn_message(MsgS,_,_,_),
    $pp_print_goal_message(MsgS),
    ( $pp_expl_one_goal(Goal) -> true
    ; $pp_raise_runtime_error($msg(1304),[Goal],explanation_not_found,
                              $pp_find_explanations/1)
    ),!,
    $pp_expl_goals(Goals).
$pp_expl_goals(Goal) =>
    $pp_expl_one_goal(Goal).

$pp_expl_one_goal(msw(Sw,V)) :- !,
    $prism_expl_msw(Sw,V,_Id).
$pp_expl_one_goal(failure) :- !,
    $pp_expl_failure.
$pp_expl_one_goal(Goal) :-
    $pp_is_dummy_goal(Goal),!,
    ( call(Goal), fail ; true ).
$pp_expl_one_goal(Goal) :-
    % FIXME: handling non-tabled probabilistic predicate is future work
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),$pp_expl_one_goal/1),
    ( ground(Goal) -> GoalCp = Goal
    ; copy_term(Goal,GoalCp)
    ),
    ( $pp_trans_one_goal(GoalCp,CompGoal) ->
( % vsc: make this give all solutions!!                                                      
        call(CompGoal) , fail ; true)
% old code was just:         call(CompGoal)
    ; savecp(CP),
      Depth = 0,
      $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_)
    ).

% [Note] this predicate fails if Goal is not probabilistic
$pp_trans_one_goal(Goal,CompGoal) :-
    functor(Goal,F,N),
    name(F,FString),
    append("$pu_expl_",FString,NewFString),
    name(NewF,NewFString),
    N1 is N + 1,
    current_predicate(NewF/N1),!,
    Goal =.. [_|Args],
    CompGoal =.. [NewF,_|Args].

%%----------------------------------------------------------------------------

$pp_expl_interp_goal('!',_Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    cutto(CP),
    CIDs    = CIDs0,
    SWs     = SWs0,
    SimCIDs = SimCIDs0,
    SimSWs  = SimSWs0.
$pp_expl_interp_goal('_$savecp'(X),_Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    savecp(X),
    CIDs    = CIDs0,
    SWs     = SWs0,
    SimCIDs = SimCIDs0,
    SimSWs  = SimSWs0.
$pp_expl_interp_goal('_$savepcp'(X),_Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    '_$savepcp'(X),
    CIDs    = CIDs0,
    SWs     = SWs0,
    SimCIDs = SimCIDs0,
    SimSWs  = SimSWs0.
$pp_expl_interp_goal('_$cutto'(X),_Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    cutto(X),
    CIDs    = CIDs0,
    SWs     = SWs0,
    SimCIDs = SimCIDs0,
    SimSWs  = SimSWs0.
$pp_expl_interp_goal('_$initialize_var'(_Vars),_Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    CIDs    = CIDs0,
    SWs     = SWs0,
    SimCIDs = SimCIDs0,
    SimSWs  = SimSWs0.
$pp_expl_interp_goal(Goal,Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs), Goal = msw(I,V) =>
    CIDs    = CIDs0,
    SWs     = [SwId|SWs0],
    SimCIDs = SimCIDs0,
    SimSWs  = [Goal|SimSWs0],
    c_SAVE_AR(AR),
    c_next_global_call_number(CallNo),
    $eval_and_monitor_call($prism_expl_msw(I,V,SwId),Depth,CallNo,AR).
$pp_expl_interp_goal((G1,G2),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(G1,Depth,CP,
                         CIDs0,CIDs1,SWs0,SWs1,
                         SimCIDs0,SimCIDs1,SimSWs0,SimSWs1),
    $pp_expl_interp_goal(G2,Depth,CP,
                         CIDs1,CIDs,SWs1,SWs,
                         SimCIDs1,SimCIDs,SimSWs1,SimSWs).
$pp_expl_interp_goal((C->A;B),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    '_$savecp'(NewCP),
    ( eval_debug_call(C,Depth,NewCP) ->
        $pp_expl_interp_goal(A,Depth,CP,
                             CIDs0,CIDs,SWs0,SWs,
                             SimCIDs0,SimCIDs,SimSWs0,SimSWs)
    ; $pp_expl_interp_goal(B,Depth,CP,
                           CIDs0,CIDs,SWs0,SWs,
                           SimCIDs0,SimCIDs,SimSWs0,SimSWs)
    ).
$pp_expl_interp_goal((C->A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    '_$savecp'(NewCP),
    ( eval_debug_call(C,Depth,NewCP) ->
        $pp_expl_interp_goal(A,Depth,CP,
                             CIDs0,CIDs,SWs0,SWs,
                             SimCIDs0,SimCIDs,SimSWs0,SimSWs)
    ).
$pp_expl_interp_goal((A;B),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
     ( $pp_expl_interp_goal(A,Depth,CP,
                            CIDs0,CIDs,SWs0,SWs,
                            SimCIDs0,SimCIDs,SimSWs0,SimSWs)
     ; $pp_expl_interp_goal(B,Depth,CP,
                            CIDs0,CIDs,SWs0,SWs,
                            SimCIDs0,SimCIDs,SimSWs0,SimSWs)
     ).
$pp_expl_interp_goal(not(A),Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    '_$savecp'(NewCP),
    ( $pp_expl_interp_goal(A,Depth,NewCP,
                           CIDs0,CIDs,SWs0,SWs,
                           SimCIDs0,SimCIDs,SimSWs0,SimSWs) -> fail
    ; CIDs    = CIDs0,
      SWs     = SWs0,
      SimCIDs = SimCIDs0,
      SimSWs  = SimSWs0
    ).
$pp_expl_interp_goal((\+ A),Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    '_$savecp'(NewCP),
    ( $pp_expl_interp_goal(A,Depth,NewCP,
                           CIDs0,CIDs,SWs0,SWs,
                           SimCIDs0,SimCIDs,SimSWs0,SimSWs) -> fail
    ; CIDs    = CIDs0,
      SWs     = SWs0,
      SimCIDs = SimCIDs0,
      SimSWs  = SimSWs0
    ).
$pp_expl_interp_goal('_$if_then_else'(C,A,B),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    '_$savecp'(NewCP),
    ( eval_debug_call(C,Depth,NewCP) ->
        $pp_expl_interp_goal(A,Depth,CP,
                             CIDs0,CIDs,SWs0,SWs,
                             SimCIDs0,SimCIDs,SimSWs0,SimSWs)
    ; $pp_expl_interp_goal(B,Depth,CP,
                           CIDs0,CIDs,SWs0,SWs,
                           SimCIDs0,SimCIDs,SimSWs0,SimSWs)
    ).
$pp_expl_interp_goal(write_call(A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(write_call([],A),Depth,CP,
                         CIDs0,CIDs,SWs0,SWs,
                         SimCIDs0,SimCIDs,SimSWs0,SimSWs).
$pp_expl_interp_goal(write_call(Opts,A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    B = $pp_expl_interp_goal(A,Depth,CP,
                             CIDs0,CIDs,SWs0,SWs,
                             SimCIDs0,SimCIDs,SimSWs0,SimSWs),
    $pp_write_call_core(Opts,A,B).
$pp_expl_interp_goal((?? A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(write_call([],A),Depth,CP,
                         CIDs0,CIDs,SWs0,SWs,
                         SimCIDs0,SimCIDs,SimSWs0,SimSWs).
$pp_expl_interp_goal((??* A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(write_call([all],A),Depth,CP,
                         CIDs0,CIDs,SWs0,SWs,
                         SimCIDs0,SimCIDs,SimSWs0,SimSWs).
$pp_expl_interp_goal((??> A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(write_call([call],A),Depth,CP,
                         CIDs0,CIDs,SWs0,SWs,
                         SimCIDs0,SimCIDs,SimSWs0,SimSWs).
$pp_expl_interp_goal((??< A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(write_call([exit+fail],A),Depth,CP,
                         CIDs0,CIDs,SWs0,SWs,
                         SimCIDs0,SimCIDs,SimSWs0,SimSWs).
$pp_expl_interp_goal((??+ A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(write_call([exit],A),Depth,CP,
                         CIDs0,CIDs,SWs0,SWs,
                         SimCIDs0,SimCIDs,SimSWs0,SimSWs).
$pp_expl_interp_goal((??- A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(write_call([fail],A),Depth,CP,
                         CIDs0,CIDs,SWs0,SWs,
                         SimCIDs0,SimCIDs,SimSWs0,SimSWs).
$pp_expl_interp_goal(Goal,Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) :-
    functor(Goal,F,N),
    $pd_is_prob_pred(F,N),!,
    CIDs    = [Gid|CIDs0],
    SWs     = SWs0,
    SimCIDs = [Goal|SimCIDs0],
    SimSWs  = SimSWs0,
    c_SAVE_AR(AR),
    c_next_global_call_number(CallNo),
    $expl_interp_and_monitor_prob_goal(Goal,Depth,Gid,CallNo,AR).
$pp_expl_interp_goal(Goal,Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) :-
    CIDs    = CIDs0,
    SWs     = SWs0,
    SimCIDs = SimCIDs0,
    SimSWs  = SimSWs0,
    ( c_is_debug_mode ->
        eval_debug_call(Goal,Depth,CP)
    ; eval_call(Goal,CP)
    ).

%%----------------------------------------------------------------------------

$expl_interp_and_monitor_prob_goal(Call,Depth,Gid,CallNo,AR) ?=>
    c_get_dg_flag(Flag),
    $print_call(Flag,'   Call: ',Call,Depth,CallNo,AR),
    Depth1 is Depth + 1,
    $expl_interp_single_call(Call,Depth1,Gid),
    $switch_skip_off(AR),
    $eval_call_exit(Call,Depth,CallNo,AR).
$expl_interp_and_monitor_prob_goal(Call,Depth,_Gid,CallNo,AR) =>
    c_get_dg_flag(Flag),
    $print_call(Flag,'   Fail: ',Call,Depth,CallNo,AR),
    fail.

$expl_interp_single_call(Goal,Depth,Gid) :- % suppress re-computation
    savecp(CP1),
    clause(Goal,Body), 
    $pp_expl_interp_goal(Body,Depth,CP1,
                         [],BodyCIDs,[],BodySWs,
                         [],SimCIDs,[],SimSWs), 
                     % BodyCIDs is a list of children in Body
                     % BodySWs is a list of switches in Body
    $pc_prism_goal_id_register(Goal,Gid),
    ( (BodyCIDs == [], BodySWs == []) -> true
    ; c_get_dg_flag(Flag),
      c_next_global_call_number(CallNo),
      $print_call(Flag,'   Add: ',path(Goal,SimCIDs,SimSWs),Depth,CallNo,0),
      $prism_eg_path(Gid,BodyCIDs,BodySWs)
    ).

%%----------------------------------------------------------------------------

$prism_eg_path(Pid,CIDs,SWs) :- $pc_add_egraph_path(Pid,CIDs,SWs).

$prism_expl_msw(Sw,V,SwInsId) :-
    get_values1(Sw,Values),
    ( $pc_prism_sw_id_get(Sw,SwId) -> true
    ; $pc_prism_sw_id_register(Sw,SwId),
      $pp_export_switch(SwId,Sw,Values)
    ), % vsc !,
    member(V,Values),
    $pc_prism_sw_ins_id_get(msw(Sw,V),SwInsId).

%%----------------------------------------------------------------------------

$pp_export_switch(SwId,Sw,Values) :-
    $pp_encode_switch_instances(Sw,Values,SwInsIds),
    $pc_export_switch(SwId,SwInsIds).
    
$pp_encode_switch_instances(_Sw,[],[]).
$pp_encode_switch_instances(Sw,[V|Vs],[Id|Ids]) :-
    $pc_prism_sw_ins_id_register(msw(Sw,V),Id),!,
    $pp_encode_switch_instances(Sw,Vs,Ids).

%%----------------------------------------------------------------------------

$pp_print_goal_message(MsgS) :-
    MsgS > 0, !,
    get_prism_flag(search_progress,Ival),
    Ival > 0, !,
    global_get($pg_num_goals,N),
    ( N =:= 0 ->
        format("#goals: 0",[]),flush_output,
        N1 is N + 1,
        global_set($pg_num_goals,N1)
    ; N > 0 ->
        ( N mod (Ival * 10) =:= 0 -> format("~w",[N]),flush_output
        ; N mod Ival =:= 0 -> format(".",[]),flush_output
        ; true
        ),
        N1 is N + 1,
        global_set($pg_num_goals,N1)
    ; true
    ).
$pp_print_goal_message(_).
