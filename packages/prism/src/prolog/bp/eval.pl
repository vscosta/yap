/* tracer and debugger of B-Prolog,
   Neng-Fa Zhou
*/
/*********************** eval_call(Call) no trace ******************/
eval_call(Goal,_CP), var(Goal) =>
    handle_exception(illegal_predicate, Goal).
/*
eval_call((A : B),CP) =>
    eval_call(A,CP),
    '_$cutto'(CP),
    eval_call(B,CP).
eval_call((A ? B),CP) =>
    eval_call(A,CP),
    eval_call(B,CP).
*/
eval_call(true,_CP) => true.
eval_call((A,B),CP) =>
    eval_call(A,CP),
    eval_call(B,CP).
eval_call((A -> B ; C),CP) =>
    eval_if_then_else(C,CP,A,B).
eval_call((A;B),CP) =>
    eval_or(A,B,CP).
eval_call((A -> B),CP) =>
    eval_if_then(A,B,CP).
eval_call(not(A),_CP) =>
     '_$savecp'(CP1),
    eval_not(A,CP1).
eval_call(\+(A),_CP) =>
     '_$savecp'(CP1),
    eval_not(A,CP1).
eval_call('!',CP) =>
    '_$cutto'(CP).
eval_call(call(X),_CP) =>
    '_$savecp'(CP1),
    eval_call(X,CP1).
eval_call(Xs,_CP), [_|_]<=Xs =>
    consult_list(Xs).
eval_call(Goal,_CP), b_IS_CONSULTED_c(Goal) =>
    '_$savecp'(CP1),
    clause(Goal,Body),
    eval_call(Body,CP1).
eval_call(Goal,_CP) =>
    call(Goal).

%% Prism-specific part
eval_call('_$initialize_var'(_Vars),_CP) => true.
eval_call('_$if_then_else'(C,A,B),CP) => eval_call((C->A;B),CP).

eval_if_then_else(_C,CP,A,B) ?=>
    '_$savecp'(CP1),
    eval_call(A,CP1),!,
    eval_call(B,CP).
eval_if_then_else(C,CP,_A,_B) =>
    eval_call(C,CP).

eval_or(A,_B,CP) ?=>
    eval_call(A,CP).
eval_or(_A,B,CP) =>
    eval_call(B,CP).

eval_if_then(A,B,CP) =>
    '_$savecp'(CP1),
     eval_call(A,CP1),!,
    eval_call(B,CP).

eval_not(A,CP) ?=>
    eval_call(A,CP),!,
    fail.
eval_not(_A,_CP) => true.

/*********************** eval_call(Call) ******************/
$trace_call(Call), b_IS_DEBUG_MODE =>
    '_$savecp'(CP),
    eval_debug_call(Call,0,CP).
$trace_call(Call) =>
    '_$savecp'(CP),
    eval_call(Call,CP).

eval_debug_call(Goal,_Depth,_CP), var(Goal) =>
    handle_exception(illegal_predicate, Goal).
/*
eval_debug_call((A : B),Depth,CP) =>
    eval_debug_call(A,Depth,CP),
    '_$cutto'(CP),
    eval_debug_call(B,Depth,CP).
eval_debug_call((A ? B),Depth,CP) =>
    eval_debug_call(A,Depth,CP),
    eval_debug_call(B,Depth,CP).
*/
eval_debug_call((A,B),Depth,CP) =>
    eval_debug_call(A,Depth,CP),
    eval_debug_call(B,Depth,CP).
eval_debug_call((A -> B ; C),Depth,CP) =>
    eval_debug_if_then_else(C,Depth,CP,A,B).
eval_debug_call((A;B),Depth,CP) =>
    eval_debug_or(A,B,Depth,CP).
eval_debug_call((A -> B),Depth,CP) =>
    eval_debug_if_then(A,B,Depth,CP).
eval_debug_call(not(A),Depth,_CP) =>
    '_$savecp'(CP1),
    eval_debug_not(A,Depth,CP1).
eval_debug_call(\+(A),Depth,_CP) =>
    '_$savecp'(CP1),
    eval_debug_not(A,Depth,CP1).
eval_debug_call('!',_Depth,CP) =>
    '_$cutto'(CP).
eval_debug_call('_$cutto'(X),_Depth,_CP) =>
    '_$cutto'(X).
eval_debug_call($trace_call(X),_Depth,_CP) =>
    $trace_call(X).
eval_debug_call(call(X),Depth,_CP) =>
    '_$savecp'(CP1),
    eval_debug_call(X,Depth,CP1).
eval_debug_call($query(X),Depth,CP) =>
    eval_debug_call(X,Depth,CP).
eval_debug_call(true,_Depth,_CP) => true.
eval_debug_call($internal_match(X,Y),_Depth,_CP) =>
    nonvar(Y),X=Y.
eval_debug_call(trace,_Depth,_CP) => trace.
eval_debug_call(op(Prec,Fix,Op),_Depth,_CP) =>
    op(Prec,Fix,Op).
eval_debug_call(dynamic(Calls),_Depth,_CP) =>
    dynamic(Calls).
eval_debug_call(nospy,_Depth,_CP) =>
    nospy.
eval_debug_call(nospy(X),_Depth,_CP) =>
    nospy(X).
eval_debug_call(notrace,_Depth,_CP) =>
    notrace.
eval_debug_call(spy(S),_Depth,_CP) =>
    spy(S).
eval_debug_call(nospy(S),_Depth,_CP) =>
    nospy(S).
eval_debug_call(Xs,_Depth,_CP), [_|_]<=Xs =>
    consult_list(Xs).
eval_debug_call(Goal,Depth,_CP) =>
    c_SAVE_AR(AR),
    c_next_global_call_number(CallNo),
    $eval_and_monitor_call(Goal,Depth,CallNo,AR).

%% Prism-specific part
eval_debug_call(Goal,_Depth,_CP), var(Goal) =>
    handle_exception(illegal_predicate, Goal).
eval_debug_call('_$initialize_var'(_Vars),_Depth,_CP) => true.
eval_debug_call('_$if_then_else'(C,A,B),Depth,CP) =>
    eval_debug_call((C->A;B),Depth,CP).
eval_debug_call(msw(Sw,V),Depth,CP) =>
    $pp_require_ground(Sw,$msg(0101),msw/2),
    c_SAVE_AR(AR),
    c_next_global_call_number(CallNo),
    $prism_sample_msw(Sw,V,Depth,CP,CallNo,AR).

eval_debug_if_then_else(_C,Depth,CP,A,B) ?=>
    '_$savecp'(NewCP),
    eval_debug_call(A,Depth,NewCP),!,
    eval_debug_call(B,Depth,CP).
eval_debug_if_then_else(C,Depth,CP,_A,_B) =>
    eval_debug_call(C,Depth,CP).

eval_debug_or(A,_B,Depth,CP) ?=>
    eval_debug_call(A,Depth,CP).
eval_debug_or(_A,B,Depth,CP) =>
    eval_debug_call(B,Depth,CP).

eval_debug_if_then(A,B,Depth,CP) =>
    '_$savecp'(NewCP),
    eval_debug_call(A,Depth,NewCP),!,
    eval_debug_call(B,Depth,CP).

eval_debug_not(A,Depth,CP) ?=>
    eval_debug_call(A,Depth,CP),!,
    fail.
eval_debug_not(_A,_Depth,_CP) => true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
$eval_and_monitor_call(Call,Depth,CallNo,AR) ?=>
    c_get_dg_flag(Flag),
    $print_call(Flag,'   Call: ',Call,Depth,CallNo,AR),
    Depth1 is 1+Depth,
    $eval_single_call(Call,Depth1),
    $switch_skip_off(AR),
    $eval_call_exit(Call,Depth,CallNo,AR).
$eval_and_monitor_call(Call,Depth,CallNo,AR) =>
    c_get_dg_flag(Flag),
    $print_call(Flag,'   Fail: ',Call,Depth,CallNo,AR),
    fail.

$eval_call_exit(Call,Depth,CallNo,AR) ?=>
    c_get_dg_flag(Flag),
    $print_call(Flag,'   Exit: ',Call,Depth,CallNo,AR).
$eval_call_exit(Call,Depth,CallNo,AR) =>
    c_get_dg_flag(Flag),
    $print_call(Flag,'   Redo: ',Call,Depth,CallNo,AR),
    fail.

$eval_single_call(Call,Depth), b_IS_CONSULTED_c(Call) =>
    '_$savecp'(CP),
    clause(Call,Body), 
    eval_debug_call(Body,Depth,CP).
$eval_single_call(Call,_Depth) =>
    call(Call).

/*
 ---------------------------------------------  
 |repeat | skip | leap | creep | spy | debug | 
 ---------------------------------------------  
#define DG_FLAG_DEBUG 0x1
#define DG_FLAG_SPY 0x2
#define DG_FLAG_C 0x4
#define DG_FLAG_L 0x8
#define DG_FLAG_S 0x10
#define DG_FLAG_R 0x20
*/

%% Prism-specific part
$print_call(_F,_T,$pu_values(_,_),             _D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_is_prob_pred(_,_),       _D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_is_tabled_pred(_,_),     _D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_parameters(_,_,_),       _D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_hyperparameters(_,_,_,_),_D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_expectations(_,_,_),     _D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_hyperexpectations(_,_,_),_D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_fixed_parameters(_),     _D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_fixed_hyperparameters(_),_D,_CNo,_AR) => true.
$print_call(_Flag,_Type,write_call(_),  _Depth,_CallNo,_AR) => true.
$print_call(_Flag,_Type,write_call(_,_),_Depth,_CallNo,_AR) => true.
$print_call(_Flag,_Type,(??  _),        _Depth,_CallNo,_AR) => true.
$print_call(_Flag,_Type,(??* _),        _Depth,_CallNo,_AR) => true.
$print_call(_Flag,_Type,(??> _),        _Depth,_CallNo,_AR) => true.
$print_call(_Flag,_Type,(??< _),        _Depth,_CallNo,_AR) => true.
$print_call(_Flag,_Type,(??+ _),        _Depth,_CallNo,_AR) => true.
$print_call(_Flag,_Type,(??- _),        _Depth,_CallNo,_AR) => true.
$print_call(Flag,Type,$prism_expl_msw(I,V,_SwId),Depth,CallNo,AR) =>
    $print_call(Flag,Type,msw(I,V),Depth,CallNo,AR).

$print_call(Flag,Type,Call,Depth,CallNo,_AR),
      Flag /\ 2'100000 =:= 2'100000 => %repeat
    '$readl_userio'(I,O),
    tab(2*Depth),write(Type),write('('),write(CallNo),write(') '),
    print(Call),nl,
    '$readl_resetio'(I,O).
$print_call(Flag,Type,Call,Depth,CallNo,AR),
      Flag /\ 2'1000 =:= 2'1000 ?=>    %leap
    c_is_spy_point(Call),!,
    $real_print_call(Type,Call,Depth,CallNo),
    $next_monitor_instruction(Type,Call,Depth,CallNo,AR).
$print_call(Flag,Type,Call,Depth,CallNo,AR),
      Flag /\ 2'100 =:= 2'100 =>      %creap
    $real_print_call(Type,Call,Depth,CallNo),
    $next_monitor_instruction(Type,Call,Depth,CallNo,AR).
$print_call(Flag,Type,Call,Depth,CallNo,AR),
      Flag /\ 2'10000 =:= 2'10000 ?=> %skip
    c_is_skip_ar(AR),!,
    $real_print_call(Type,Call,Depth,CallNo),
    $next_monitor_instruction(Type,Call,Depth,CallNo,AR).
$print_call(_Flag,_Type,_Call,_Depth,_AR,_CallNo) => true.

$real_print_call(Type,Call,Depth,CallNo):-
    '$readl_userio'(I,O),
    tab(2*Depth),write(Type),write('('),write(CallNo),write(') '),
    print(Call),writename(' ?'),
    '$readl_resetio'(I,O).

$next_monitor_instruction(Type,Call,Depth,CallNo,AR):-
    $get_monitor_instruction(Inst),
    $process_monitor_instruction(Type,Call,Depth,CallNo,AR,Inst).

/*
#define DG_FLAG_DEBUG 0x1
#define DG_FLAG_SPY 0x2
#define DG_FLAG_C 0x4
#define DG_FLAG_L 0x8
#define DG_FLAG_S 0x10
#define DG_FLAG_R 0x20
*/
$process_monitor_instruction(_Type,_Call,_Depth,_CallNo,_AR,0'a) =>
    abort.                     % abort
$process_monitor_instruction(_Type,_Call,_Depth,_CallNo,_AR,0'r) =>
    c_set_dg_flag(2'100000).   % repeat
$process_monitor_instruction(_Type,_Call,_Depth,_CallNo,_AR,0'c) =>
    c_set_dg_flag(2'100).      % creep
$process_monitor_instruction(_Type,_Call,_Depth,_CallNo,_AR,10) =>
    c_set_dg_flag(2'100).      % return
$process_monitor_instruction(_Type,_Call,_Depth,_CallNo,_AR,0'n) =>
    c_get_dg_flag(Flag),
    NewFlag is Flag/\2'11, 
    c_init_dg_flag(NewFlag).   % no trace
$process_monitor_instruction(_Type,_Call,_Depth,_CallNo,_AR,0'l) =>
    c_set_dg_flag(2'1000).     % leap
$process_monitor_instruction(Type,Call,Depth,CallNo,AR,0's) =>
    ((Type=='   Fail: ';Type=='   Exit: ')->
      write(user_output,'Option not applicable'),nl(user_output),
      $real_print_call(Type,Call,Depth,CallNo),
      $next_monitor_instruction(Type,Call,Depth,CallNo,AR);
     c_set_dg_flag(2'10000),
     c_set_skip_ar(AR)).       % skip
$process_monitor_instruction(Type,Call,Depth,CallNo,AR,_) => % other ?
    $print_help(Type),
    $real_print_call(Type,Call,Depth,CallNo),
    $next_monitor_instruction(Type,Call,Depth,CallNo,AR).

$print_help(_Type):-
    write(user,' a     abort'),nl(user),
    write(user,' ?     help'),nl(user),
    write(user,' h     help'),nl(user),
    write(user,'<cr>   creep'),nl(user),
    write(user,' c     creep'),nl(user),
    write(user,' h     help'),nl(user),
    write(user,' l     leap'),nl(user),
    write(user,' n     nodebug'),nl(user),
    write(user,' r     repeat creep'),nl(user),
    write(user,' s     skip'),nl(user),nl(user).
    
$get_monitor_instruction(Command):-
    '$readl_userio'(I,O),
    get0(Command),
    $get_until_return(Command),
    '$readl_resetio'(I,O).

$get_until_return(10) => true.
$get_until_return(_Command) =>
    get0(X),
    $get_until_return(X).

$switch_skip_off(AR):-
    c_is_skip_ar(AR),!,
    c_set_skip_ar(0),
    c_set_dg_flag(2'100). % creep
$switch_skip_off(_) => true.
    
    
/**************trace/1 spy/1******************/
/* vsc: not supported in YAP yet
trace =>
    c_init_dg_flag(1).

spy(S), var(S) =>
    c_get_spy_points(S).
spy([X|Xs]) =>
    spy(X),
    spy(Xs).
spy([]) => true.
spy(Pred), F/N<=Pred, atom(F),integer(N) =>
    (c_CURRENT_PREDICATE(F,N)->
    '$readl_userio'(I,O),
    write('Spy point '), write(Pred), write(' has been set.'),nl,
    '$readl_resetio'(I,O),
    c_add_spy_point(F,N);
    handle_exception(predicate_not_exist, Pred)).
spy(F), atom(F) =>
    $search_preds(F,25,[],X),    
    (X\==[]->spy(X); handle_exception(predicate_not_exist, F)).
spy(F):-
    handle_exception(illegal_argument, spy(F)).

$search_preds(_X,N,P0,P), N<0 =>
    P=P0.
$search_preds(X,N,P0,P):-
    c_CURRENT_PREDICATE(X,N),!,
    N1 is N-1,
    $search_preds(X,N1,[X/N|P0],P).
$search_preds(X,N,P0,P) =>
    N1 is N-1,
    $search_preds(X,N1,P0,P).
    
notrace =>
    c_init_dg_flag(0),
    nospy.

nospy([X|Xs]) =>
    nospy(X),
    nospy(Xs).
nospy([]) => true.
nospy(F/N), atom(F), integer(N) =>
    c_remove_spy_point(F,N).
nospy(F), atom(F) =>
    $search_preds(F,25,[],X),
    nospy(X).
nospy(F) =>
    handle_exception(illegal_predicate, nospy(F)).

nospy:-
    c_remove_spy_points.

trace(Call) =>
    $trace_call(Call).
*/