:- $pp_require_mp_mode.
:- $pc_mp_master -> print_copyright ; true.

%%------------------------------------------------------------------------
%%  [[ Tags for $pc_mp_sync/2 ]]
%%------------------------------------------------------------------------
%%  01 : $pp_batch_call
%%  02 : $pp_mp_call_s_core
%%  03 : $pp_compile_load
%%  04 : $pp_foc
%%------------------------------------------------------------------------

%%----------------------------------------
%%  batch routines

main :- $pp_batch.

%$pp_batch_call(Goal) :-
%    $pc_mp_master -> $pp_mpm_batch_call(Goal) ; $pp_mps_batch_call.

$pp_batch_call(Goal) :-
    ( $pc_mp_master -> $pp_mpm_batch_call(Goal)
    ; $pp_mps_batch_call
    ).

$pp_mpm_batch_call(Goal) :-
    ( call(Goal) -> Sync = 1 ; Sync = -1 ),
    $pc_mpm_bcast_command($stop),!,
    ( $pc_mp_sync(1,Sync) -> Res = yes ; Res = no ),
    format("~n~w~n",[Res]).

$pp_mps_batch_call :-
    ( $pp_slave_loop -> Sync = 1 ; Sync = -1 ),!,
    ( $pc_mp_sync(1,Sync) ; true ).

$pp_slave_loop :-
    $pc_mps_bcast_command(Cmd),
    ( Cmd \== $stop -> call(Cmd),!,$pp_slave_loop
    ; true
    ).

%%----------------------------------------
%%  system predicates

abort :- $pc_mp_abort.

$pp_mps_err_msg(Msg) :-
    $pc_mps_revert_stdout, err_msg(Msg).
$pp_mps_err_msg(Fmt,Args) :-
    $pc_mps_revert_stdout, err_msg(Fmt,Args).

$pp_load(File) :-
    $pp_mp_call_s_core(\+ \+ $myload(File)),
    $pp_init_tables.

$pp_compile_load(File) :-
    $pp_add_out_extension(File,OutFile),
    ( $pc_mp_master -> $pp_compile(File,_DmpFile,OutFile) ; true ),!,
    $pc_mp_sync(3,1),
    $pp_load(OutFile).
$pp_compile_load(_File) :-
    $pc_mp_sync(3,-1).

$pp_foc(File1,File2) :-
    ( $pc_mp_master ->
        fo(File1,File2), format("Compilation done by FOC~n~n",[])
    ; true
    ),!,
    $pc_mp_sync(4,1).
$pp_foc(_,_) :-
    $pc_mp_sync(4,-1).

%%----------------------------------------
%%  user predicates

mp_call(Goal) :-
    $pc_mpm_bcast_command(Goal),call(Goal).
mp_call_s(Goal) :-
    $pc_mpm_bcast_command($pp_mp_call_s_core(Goal)),$pp_mp_call_s_core(Goal).
    
$pp_mp_call_s_core(Goal) :-
    $pc_mp_rank(R),
    $pc_mp_size(N),
    $pp_mp_call_s_core(Goal,R,N,0).

$pp_mp_call_s_core(_,_,N,K) :-
    K >= N,!.
$pp_mp_call_s_core(Goal,MyID,N,K) :-
    ( K =:= MyID ->
        ( call(Goal) -> Sync = K ; Sync = -1 )
    ; % else
        Sync = K
    ),
    $pc_mp_sync(2,Sync),
    K1 is K + 1,!,
    $pp_mp_call_s_core(Goal,MyID,N,K1).

%%----------------------------------------
%%  debug predicates

$pp_mp_debug(Format,Args) :-
    current_output(Stream),
    $pp_mp_debug(Stream,Format,Args).

$pp_mp_debug(Stream,Format,Args) :-
    $pc_mp_rank(R),
    append("[RANK:~w] ",Format,NewFormat),
    NewArgs = [R|Args],
    format(Stream,NewFormat,NewArgs),!.

%%----------------------------------------

