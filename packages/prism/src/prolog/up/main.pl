%% -*- Prolog -*-

%%----------------------------------------
%%  Version and copyright statement

$pp_version('2.0').
$pp_copyright('PRISM 2.0, (C) Sato Lab, Tokyo Institute of Technology, July, 2010').

get_version(V)  :- $pp_version(V).
print_version   :- $pp_version(V),     !, format("~w~n",[V]).
print_copyright :- $pp_copyright(Msg), !, format("~w~n",[Msg]).

%%----------------------------------------
%%  Operators

:- op(1160,xfx,times).

:- op(1150,fx,sample).
:- op(1150,fx,prob).
:- op(1150,fx,probf).
:- op(1150,fx,probfi).
:- op(1150,fx,probfo).
:- op(1150,fx,probfv).
:- op(1150,fx,probfio).
:- op(1150,fx,viterbi).
:- op(1150,fx,viterbif).
:- op(1150,fx,viterbig).
:- op(1150,fx,hindsight).
:- op(1150,fx,chindsight).

:- op(1150,fy,p_table).
:- op(1150,fy,p_not_table).

:- op(600,xfx,@).

:- op(950,fx,?? ).
:- op(950,fx,??*).
:- op(950,fx,??>).
:- op(950,fx,??<).
:- op(950,fx,??+).
:- op(950,fx,??-).

%%----------------------------------------
%%  Declarations

% only declarations. no effect when executed
p_table(_).
p_not_table(_).

:- table $prism_eg_path/3.
:- table $prism_expl_msw/3.
:- table $expl_interp_single_call/3.

%%----------------------------------------
%% Initializations

%
% vsc: delay until end in YAP
%
%:- ( $pc_mp_mode -> true ; print_copyright ).
%:- random_set_seed.
%:- reset_prism_flags.

%%----------------------------------------
%% Help messages

$help_mess("~nType 'prism_help' for usage.~n").   % Hook for B-Prolog

prism_help :-
    format(" prism(File)             -- compile and load a program~n",[]),
    format(" prism(Opts,File)        -- compile and load a program~n",[]),
    nl,
    format(" msw(I,V)                -- the switch I randomly outputs the value V~n",[]),
    nl,
    format(" learn(Gs)               -- learn the parameters~n",[]),
    format(" learn                   -- learn the parameters from data_source~n",[]),
    format(" sample(Goal)            -- get a sampled instance of Goal~n",[]),
    format(" prob(Goal,P)            -- compute a probability~n",[]),
    format(" probf(Goal,F)           -- compute an explanation graph~n",[]),
    format(" viterbi(Goal,P)         -- compute a Viterbi probability~n",[]),
    format(" viterbif(Goal,P,F)      -- compute a Viterbi probability with its explanation~n",[]),
    format(" hindsight(Goal,Patt,Ps) -- compute hindsight probabilities~n",[]),
    nl,
    format(" set_sw(Sw,Params)       -- set parameters of a switch~n",[]),
    format(" get_sw(Sw,SwInfo)       -- get information of a switch~n",[]),
    format(" set_prism_flag(Flg,Val) -- set a new value to a flag~n",[]),
    format(" get_prism_flag(Flg,Val) -- get the current value of a flag~n",[]),
    nl,
    format(" please consult the user's manual for details.~n",[]).

%%----------------------------------------
%% Loading a program

prism(File) :-
    prism([],File).

prism(Opts,File) :-
    $pp_require_atom(File,$msg(3000),prism/2),
    $pp_set_options(Opts),   % also aiming at the error check of options
    ( member(consult,Opts) ->
        $pp_search_file(File,File1,[".psm",""]),
        Pred = $pp_consult(File1)
    ; member(load,Opts) ->
        $pp_search_file(File,File1,[".psm.out",".out",""]),
        Pred = $pp_load(File1)
    ; ( member(dump,Opts) -> D = 1 ; D = 0 ),
        global_set($pg_dump_compiled,D),
        $pp_search_file(File,File1,[".psm",""]),
        Pred = $pp_compile_load(File1)
    ),!,
    reset_prism_flags,
    global_del(failure,0),
    global_set($pg_dummy_goal_count,0),
    call(Pred),!.
prism(_Opts,File) :-
    $pp_raise_existence_error($msg(3001),[File],
                              [prism_file,File],existence_error).

$pp_compile_load(File) :-
    $pp_add_out_extension(File,OutFile),
    $pp_clean_dynamic_preds, 
    $pp_compile(File,_DmpFile,OutFile),
    $pp_load(OutFile).

$pp_load(File) :-
    not(not($myload(File))),
    $pp_init_tables_aux,
    $pp_init_tables,!.
% We do not perform translation
%   -- the explanation search will be done by meta-interpreters
$pp_consult(File) :-
    $pp_clean_dynamic_preds, 
    new_hashtable(PPredTab),
    Info = $trans_info(_DoTable,_TPredTab,_NoDebug,PPredTab),
    $pp_bpif_read_program(Prog,File),
    $pp_extract_decls(Prog,Info),
    $pp_trans_values(Prog,Prog1),
    $pp_analyze(Prog1,Info),
    $pp_tabled_to_nontabled(Prog1,Prog2),
    assert($pd_is_tabled_pred($disabled_by_consult,0)),
    $pp_separate_demon_load(Prog2,Prog3,Prog4),
         % $damon_load/0 should be consulted after loading the entire program
    consult_preds(Prog4,_ProgCompiled),
    consult_preds(Prog3,_ProgCompiled),
    $pp_init_tables_aux,
    $pp_init_tables.


$pp_set_options([]) => true.
$pp_set_options([O|Options]) =>
    $pp_require_prism_option(O,$msg(1001),prism/2),
    $pp_set_one_option(O),!,
    $pp_set_options(Options).

$pp_set_one_option(dump)    => true.
$pp_set_one_option(consult) => true.
$pp_set_one_option(compile) => true.
$pp_set_one_option(load)    => true.
$pp_set_one_option(v)       :- set_prism_flag(verb,full).
$pp_set_one_option(verb)    :- set_prism_flag(verb,full).
$pp_set_one_option(nv)      :- set_prism_flag(verb,none).
$pp_set_one_option(noverb)  :- set_prism_flag(verb,none).
$pp_set_one_option(Att=Val) :- set_prism_flag(Att,Val).


%%----------------------------------------
%% Clean up databases

$pp_clean_dynamic_preds :-
    $pp_clean_predicate_info,
    $pp_clean_switch_info,
    $pp_clean_dummy_goal_table,
    $pp_clean_graph_stats,
    $pp_clean_learn_stats,
    $pp_clean_infer_stats.

$pp_clean_predicate_info :-
    retractall($pd_is_prob_pred(_,_)),
    retractall($pd_is_tabled_pred(_,_)),!.

$pp_clean_switch_info :-
    retractall($pd_parameters(_,_,_)),
    retractall($pd_hyperparameters(_,_,_,_)),
    retractall($pd_expectations(_,_,_)),
    retractall($pd_hyperexpectations(_,_,_)),
    retractall($pd_fixed_parameters(_)),
    retractall($pd_fixed_hyperparameters(_)),!.

$pp_init_tables :-
    initialize_table,
    $pc_prism_id_table_init,
    $pc_clean_base_egraph, % base support graph and switches
    $pc_alloc_egraph,!.    % get ready for the following steps

$pp_init_tables_if_necessary :-
    ( get_prism_flag(clean_table, on) -> $pp_init_tables
    ; true
    ),!.

$pp_init_tables_aux :-
    $pc_clean_egraph,      % derived support graphs
    $pc_clean_external_tables,!.


%%----------------------------------------
%% Show the program information

show_values :-
    format("Outcome spaces:~n",[]),!,
    findall([Sw,Vals],($pp_registered_sw(Sw),get_values1(Sw,Vals)),SwVals0),
    sort(SwVals0,SwVals1),
    $pp_show_values_list(SwVals1),!.

$pp_show_values_list([]).
$pp_show_values_list([[Sw,Vals]|SwVals]) :-
    format("  ~q: ~q~n",[Sw,Vals]),!,
    $pp_show_values_list(SwVals).

%% (Note) $pd_is_{prob,tabled}_pred/2 are dynamic, so we don't have to call
%%        current_predicate/1.  We don't check the input rigorously either
%%        for flexibility.

is_prob_pred(F/N) :- is_prob_pred(F,N).
is_prob_pred(F,N) :- $pd_is_prob_pred(F,N).

is_tabled_pred(F/N) :- is_tabled_pred(F,N).
is_tabled_pred(F,N) :- $pd_is_tabled_pred(F,N).

show_prob_preds :-
    format("Probabilistic predicates:~n",[]),!,
    findall(F0/N0,is_prob_pred(F0,N0),Preds0),
    sort(Preds0,Preds),
    ( member(F/N,Preds),
      format("  ~q/~w~n",[F,N]),
      fail
    ; true
    ),!.

show_tabled_preds :-
    $pd_is_tabled_pred($disabled_by_consult,_),!,
    $pp_raise_warning($msg(1002)).

show_tabled_preds :-
    format("Tabled probabilistic predicates:~n",[]),!,
    findall(F0/N0,is_tabled_pred(F0,N0),Preds0),
    sort(Preds0,Preds),
    ( member(F/N,Preds),
      format("  ~q/~w~n",[F,N]),
      fail
    ; true
    ),!.

%% aliases
show_prob_pred    :- show_prob_preds.
show_table_pred   :- show_tabled_preds.
show_table_preds  :- show_tabled_preds.
show_tabled_pred  :- show_tabled_preds.

%%----------------------------------------
%% Predicates for batch (non-interactive) execution

$pp_batch :-
    catch($pp_batch_core,Err,$pp_batch_error(Err)).

$pp_batch_error(Err) :-
    Err == abort,!.
$pp_batch_error(Err) :-
    Err == interrupt,!,
    format("Aborted by interruption~n",[]),
    abort.
$pp_batch_error(Err) :-
    format("Aborted by exception -- ~w~n",[Err]),
    abort.

$pp_batch_core :-
    get_main_args([Arg|Args]),!,
    $pp_batch_load(Arg,File),
    $pp_batch_main(Args,File).
$pp_batch_core :-
    $pp_raise_existence_error($msg(1003),[prism_file,unknown],$pp_batch/1).

$pp_batch_load(Arg,File) :-
    ( atom_chars(Arg,[p,r,i,s,m,  ':'|FileChars]) ->
        atom_chars(File,FileChars), FileChars \== [], prism(File)
    ; atom_chars(Arg,[p,r,i,s,m,n,':'|FileChars]) ->
        atom_chars(File,FileChars), FileChars \== [], prismn(File)
    ; atom_chars(Arg,[l,o,a,d,    ':'|FileChars]) ->
        atom_chars(File,FileChars), FileChars \== [], prism([load],File)
    ; prism(Arg), File = Arg
    ),!.

$pp_batch_main(Args,File) :-
    ( current_predicate(prism_main/1) -> Goal = prism_main(Args)
    ; current_predicate(prism_main/0) -> Goal = prism_main
    ; $pp_raise_existence_error($msg(1004),[File],[batch_predicate,File],
                                $pp_batch_main/2)
    ),!,
    %% use of call/1 is for the parallel version
    call($pp_batch_call(Goal)).

%%----------------------------------------
%% Miscellaneous routines

$pp_tabled_to_nontabled([],Prog) => Prog = [].
$pp_tabled_to_nontabled([pred(F,N,M,Delay,_Tabled,Cls)|Preds],Prog) => 
    Prog = [pred(F,N,M,Delay,_,Cls)|Prog1], !,
    $pp_tabled_to_nontabled(Preds,Prog1).


$pp_separate_demon_load([],[],[]).
$pp_separate_demon_load([pred($damon_load,0,X0,X1,X2,X3)|Prog0],
                        [pred($damon_load,0,X0,X1,X2,X3)|Prog1],
                        Prog2) :- !,
    $pp_separate_demon_load(Prog0,Prog1,Prog2).
$pp_separate_demon_load([P|Prog0],Prog1,[P|Prog2]) :- !,
    $pp_separate_demon_load(Prog0,Prog1,Prog2).


$pp_search_file(File,File1,Suffixes) :-
    member(Suffix,Suffixes),
    $pp_add_extension(File,File1,Suffix),
    exists(File1),!.


$pp_add_psm_extension(File,PsmFile) :-
    $pp_add_extension(File,PsmFile,".psm").

$pp_add_out_extension(File,OutFile) :-
    $pp_add_extension(File,OutFile,".out").

$pp_add_extension(File,File1,Extension) :-
    ( atom(File) -> name(File,FileString)
    ; File ?= [_|_] -> File = FileString
    ; $pp_raise_domain_error($msg(1000),[File],[filename,File],
                             $pp_add_extension/3)
    ),
    append(FileString,Extension,FileString1),
    name(File1,FileString1).
