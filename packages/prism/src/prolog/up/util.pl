%%----------------------------------------
%%  error/warning (obsolete)

err_msg(Msg) :-
    format("{PRISM ERROR: ",[]),write(Msg),format("}~n",[]),!,
    abort.
err_msg(Msg,Vars) :-
    format("{PRISM ERROR: ",[]),format(Msg,Vars),format("}~n",[]),!,
    abort.

warn_msg(Msg) :-
    ( get_prism_flag(warn,on) ->
        format("{PRISM WARNING: ",[]),write(Msg),format("}~n",[])
    ; true
    ).
warn_msg(Msg,Vars) :-
    ( get_prism_flag(warn,on) ->
        format("{PRISM WARNING: ",[]),format(Msg,Vars),format("}~n",[])
    ; true
    ).


%%----------------------------------------
%%  internal utils

%% probabilistic formulas

$pp_is_user_probabilistic_atom(Goal) :-
    callable(Goal),
    functor(Goal,F,N),
    $pd_is_prob_pred(F,N),!.

$pp_is_probabilistic_atom(Goal) :-
    ( nonvar(Goal), Goal ?= msw(_,_)
    ; $pp_is_user_probabilistic_atom(Goal)
    ),!.

$pp_is_extended_probabilistic_atom(Goal) :-
    ( $pp_is_probabilistic_atom(Goal)
    ; $pp_is_dummy_goal(Goal)
    ),!.

$pp_is_probabilistic_callable(Goal) :-
    callable(Goal),
    $pp_is_probabilistic_callable_aux(Goal),!.

$pp_is_probabilistic_callable_aux((G1,G2)) =>
    ( $pp_is_probabilistic_callable_aux(G1),callable(G2)
    ; callable(G1),$pp_is_probabilistic_callable_aux(G2)
    ).
$pp_is_probabilistic_callable_aux((G1;G2)) =>
    ( $pp_is_probabilistic_callable_aux(G1),callable(G2)
    ; callable(G1),$pp_is_probabilistic_callable_aux(G2)
    ).
$pp_is_probabilistic_callable_aux((C->A;B)) =>
    ( $pp_is_probabilistic_callable_aux(C),callable(A),callable(B)
    ; callable(C),$pp_is_probabilistic_callable_aux(A),callable(B)
    ; callable(C),callable(A),$pp_is_probabilistic_callable_aux(B)
    ).
$pp_is_probabilistic_callable_aux(not(G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux(\+(G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux((C->A)) =>
    ( $pp_is_probabilistic_callable_aux(C),callable(A)
    ; callable(C),$pp_is_probabilistic_callable_aux(A)
    ).
$pp_is_probabilistic_callable_aux(write_call(G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux(write_call(_Opts,G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux((?? G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux((??* G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux((??> G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux((??< G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux((??+ G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux((??- G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux(G) :- 
    $pp_is_extended_probabilistic_atom(G).

%% tabled probabilistic formulas

$pp_is_tabled_probabilistic_atom(Goal) :-
    callable(Goal),
    functor(Goal,F,N),
    $pd_is_tabled_pred(F,N),!.

%% goals that can be handled with the write_call predicates

$pp_is_write_callable(Goal) :-
   ( Goal = '!'    -> fail
   ; Goal = (A,B)  -> $pp_is_write_callable(A), $pp_is_write_callable(B)
   ; Goal = (_;_)  -> fail
   ; Goal = \+(_)  -> fail
   ; Goal = not(_) -> fail
   ; Goal = (_->_) -> fail
   ; true
   ).

%% dummy goals

$pp_create_dummy_goal(DummyGoal) :-
    global_get($pg_dummy_goal_count,N0),
    N1 is N0 + 1,
    global_set($pg_dummy_goal_count,N1),!,
    $pp_create_dummy_goal(N0,DummyGoal),!.

$pp_create_dummy_goal(N,DummyGoal) :-
    number_chars(N,NChars),
    append(['$',p,d,'_',d,u,m,m,y],NChars,DummyGoalChars),
    atom_chars(DummyGoal,DummyGoalChars).

$pp_is_dummy_goal(G) :-
    atom(G),
    atom_chars(G,GChars),
    GChars = ['$',p,d,'_',d,u,m,m,y|_].

%% option analyzer

$pp_proc_opts(Opts,Pred,Vars,Defaults,Source) :-
    $pp_require_list_or_nil(Opts,$msg(2109),Source),
    $pp_proc_opts_core(Opts,Pred,Vars,Defaults,Source).

$pp_proc_opts_core([],_,[],[],_Source) :- !.
$pp_proc_opts_core([],Pred,[Var|Vars],[Default|Defaults],Source) :-
    ( Var = Default ; true ),!,
    $pp_proc_opts_core([],Pred,Vars,Defaults,Source).
$pp_proc_opts_core([Opt|Opts],Pred,Vars,Defaults,Source) :-
    nonvar(Opt),
    Clause =.. [Pred,Opt,Pos,Val],
    call(Clause),
    nth1(Pos,Vars,Var),
    ( var(Var) -> Var = Val
    ; $pp_raise_runtime_error($msg(3003),[Opt],duplicate_option,
                              Source)
    ),!,
    $pp_proc_opts_core(Opts,Pred,Vars,Defaults,Source).
$pp_proc_opts_core([Opt|_],_,_,_,Source) :-
    $pp_raise_runtime_error($msg(3002),[Opt],unknown_option,Source).

%% sorting with duplicate elements remained

$pp_sort_remain_dup(L0,L) :- sort('=<',L0,L).


%%----------------------------------------
%%  statistics

show_goals :-
    global_get($pg_observed_facts,GoalCountPairs0),!,
    sort(GoalCountPairs0,GoalCountPairs),
    $pp_find_total_count(GoalCountPairs,0,Total),
    $pp_show_goals(GoalCountPairs,Total).
show_goals :-
    $pp_raise_runtime_error($msg(3004),observation_not_found,show_goals/0).

$pp_find_total_count([],Total,Total).
$pp_find_total_count([goal(_Goal,Count)|GoalCountPairs],Total0,Total) :-
    Total1 is Total0 + Count,!,
    $pp_find_total_count(GoalCountPairs,Total1,Total).

$pp_show_goals([],Total) :- format("Total_count=~w~n",[Total]).
$pp_show_goals([goal(DummyGoal,Count)|GoalCountPairs],Total) :-
    P is Count / Total * 100,
    ( current_predicate($pd_dummy_goal_table/2),
      $pd_dummy_goal_table(DummyGoal,Goal)
        -> true
    ; Goal = DummyGoal
    ),
    format("Goal ~w (count=~w, freq=~3f%)~n",[Goal,Count,P]),
    $pp_show_goals(GoalCountPairs,Total).

get_goals(Gs) :-
    findall(Goal,$pp_get_one_goal(Goal),Gs0),
    sort(Gs0,Gs).

$pp_get_one_goal(Goal) :-
    ( global_get($pg_observed_facts,GoalCountPairs) ->
        $pp_get_one_goal(Goal,GoalCountPairs)
    ; $pp_raise_runtime_error($msg(3004),observation_not_found,show_goals/0)
    ).

$pp_get_one_goal(Goal,[goal(DummyGoal,_Count)|_]) :-
    current_predicate($pd_dummy_goal_table/2),
    $pd_dummy_goal_table(DummyGoal,Goal).
$pp_get_one_goal(Goal,[goal(Goal,_Count)|_]).
$pp_get_one_goal(Goal,[_|Pairs]) :- $pp_get_one_goal(Goal,Pairs).

get_goal_counts(GCounts) :-
    findall([Goal,Count,Freq],$pp_get_one_goal_count(Goal,Count,Freq),GCounts0),
    sort(GCounts0,GCounts).

$pp_get_one_goal_count(Goal,Count,Freq) :-
    ( global_get($pg_observed_facts,GoalCountPairs) ->
        $pp_find_total_count(GoalCountPairs,0,Total),
        $pp_get_one_goal_count(Goal,Count,Freq,GoalCountPairs,Total)
    ; $pp_raise_runtime_error($msg(3004),observation_not_found,show_goals/0)
    ).

$pp_get_one_goal_count(Goal,Count,Freq,[goal(DummyGoal,Count)|_],Total) :-
    current_predicate($pd_dummy_goal_table/2),
    $pd_dummy_goal_table(DummyGoal,Goal),
    Freq is Count / Total * 100.
$pp_get_one_goal_count(Goal,Count,Freq,[goal(Goal,Count)|_],Total) :-
    Freq is Count / Total * 100.
$pp_get_one_goal_count(Goal,Count,Freq,[_|Pairs],Total) :-
    $pp_get_one_goal_count(Goal,Count,Freq,Pairs,Total).

prism_statistics(Name,L) :-
    ( graph_statistics(Name,L)
    ; learn_statistics(Name,L)
    ; infer_statistics(Name,L)
    ).

graph_statistics(Name,L) :-
    ( \+ $ps_num_subgraphs(_) -> fail
    ; Name = num_subgraphs,
        ( $ps_num_subgraphs(L) -> true )
    ; Name = num_nodes,
        ( $ps_num_nodes(L) -> true )
    ; Name = num_goal_nodes,
        ( $ps_num_goal_nodes(L) -> true )
    ; Name = num_switch_nodes,
        ( $ps_num_switch_nodes(L) -> true )
    ; Name = avg_shared,
        ( $ps_avg_shared(L) -> true )
    ).

learn_statistics(Name,L) :-
    ( \+ $ps_learn_time(_) -> fail
    ; Name = log_likelihood,
        ( $ps_log_likelihood(L) -> true )
    ; Name = log_post,
        ( $ps_log_post(L) -> true )
    ; Name = log_prior,
        ( $ps_log_post(LPost), $ps_log_likelihood(LogLike) -> L is LPost - LogLike )
    ; Name = lambda,
        ( ( $ps_log_post(L) ; $ps_log_likelihood(L) ) -> true )
    ; Name = num_switches,
        ( $ps_num_switches(L) -> true )
    ; Name = num_switch_values,
        ( $ps_num_switch_values(L) -> true )
    ; Name = num_parameters,
        ( $ps_num_switches(N0), $ps_num_switch_values(N1) -> L is N1 - N0 )
    ; Name = num_iterations,
        ( $ps_num_iterations(L) -> true )
    ; Name = num_iterations_vb,
        ( $ps_num_iterations_vb(L) -> true )
    ; Name = goals,
        ( is_global($pg_observed_facts) -> get_goals(L) )
    ; Name = goal_counts,
        ( is_global($pg_observed_facts) -> get_goal_counts(L) )
    ; Name = bic,
        ( $ps_bic_score(L) -> true )
    ; Name = cs,
        ( $ps_cs_score(L) -> true )
    ; Name = free_energy,
        ( $ps_free_energy(L) -> true )
    ; Name = learn_time,
        ( $ps_learn_time(L) -> true )
    ; Name = learn_search_time,
        ( $ps_learn_search_time(L) -> true )
    ; Name = em_time,
        ( $ps_em_time(L) -> true )
    ).

infer_statistics(Name,L) :-
    ( \+ $ps_infer_time(_) -> fail
    ; Name = infer_time,
        ( $ps_infer_time(L) -> true )
    ; Name = infer_search_time,
        ( $ps_infer_search_time(L) -> true )
    ; Name = infer_calc_time,
        ( $ps_infer_calc_time(L) -> true )
    ).

prism_statistics :-
    format("Statistics in PRISM:~n",[]),!,
    ( prism_statistics(Name,L),
      $pp_print_one_statistic(Name,L),
      fail
    ; true
    ),!.

learn_statistics :-
    format("Statistics on learning:~n",[]),!,
    ( learn_statistics(Name,L),
      $pp_print_one_statistic(Name,L),
      fail
    ; true
    ),!.

graph_statistics :-
    format("Statistics on the size of the explanation graphs:~n",[]),!,
    ( graph_statistics(Name,L),
      $pp_print_one_statistic(Name,L),
      fail
    ; true
    ),!.

infer_statistics :-
    format("Statistics on inference:~n",[]),!,
    ( infer_statistics(Name,L),
      $pp_print_one_statistic(Name,L),
      fail
    ; true
    ),!.

$pp_print_one_statistic(Name,L) :-
    ( Name = goals       -> format("  ~w~24|: (run show_goals/0)~n",[Name])
    ; Name = goal_counts -> format("  ~w~24|: (run show_goals/0)~n",[Name])
    ; float(L)           -> format("  ~w~24|: ~9g~n",[Name,L])
    ; format("  ~w~24|: ~w~n",[Name,L])
    ).

%%----------------------------------------
%%  clause list reader/writer

load_clauses(FileName,Clauses) :-
    load_clauses(FileName,Clauses,[]).

load_clauses(FileName,Clauses,From,Size) :-
    $pp_raise_warning($msg(3300),[load_clauses/4,load_clauses/3]),
    load_clauses(FileName,Clauses,[from(From),size(Size)]).

load_clauses(FileName,Clauses,Opts) :-
    $pp_require_atom(FileName,$msg(3000),load_clauses/3),
    $pp_proc_opts(Opts,$load_clauses_option,
                  [From,Size],
                  [0   ,max ],
                  load_clauses/3),
    open(FileName,read,Stream),
    $pp_load_clauses_core(Stream,Clauses,From,Size),
    close(Stream),!.

$load_clauses_option(from(N),1,N) :-
    integer(N),N >= 0.
$load_clauses_option(skip(N),1,N) :-
    integer(N),N >= 0.
$load_clauses_option(size(N),2,N) :-
    integer(N),N >= 0 ; N == max.

$pp_load_clauses_core(_,[],_,0).
$pp_load_clauses_core(S,Xs,K,N) :-
    $pp_load_clauses_read(S,X),!,
    ( K > 0    -> Xs = Xs1,     K1 is K - 1, N1 = N
    ; N == max -> Xs = [X|Xs1], K1 = K,      N1 = N
    ;             Xs = [X|Xs1], K1 = K,      N1 is N - 1
    ),!,
    $pp_load_clauses_core(S,Xs1,K1,N1).
$pp_load_clauses_core(_,[],K,N) :-
    ( K =< 0, N == max -> true
    ; $pp_raise_warning($msg(3008))
    ).

$pp_load_clauses_read(S,X) :-
    read(S,X),!,X \== end_of_file.

save_clauses(FileName,Clauses) :-
    save_clauses(FileName,Clauses,[]).

save_clauses(FileName,Clauses,From,Size) :-
    $pp_raise_warning($msg(3300),[save_clauses/4,save_clauses/3]),
    save_clauses(FileName,Clauses,[from(From),size(Size)]).

save_clauses(FileName,Clauses,Opts) :-
    $pp_require_atom(FileName,$msg(3000),save_clauses/3),
    $pp_require_list_or_nil(Clauses,$msg(2109),save_clauses/3),
    $pp_proc_opts(Opts,$load_clauses_option,
                  [From,Size],
                  [0   ,max ],
                  save_clauses/3),
    open(FileName,write,Stream),
    $pp_save_clauses_core(Stream,Clauses,From,Size),
    close(Stream),!.

$pp_save_clauses_core(_,_,_,0) :- !.
$pp_save_clauses_core(S,[X|Xs1],K,N) :-
    ( K > 0  ->                          K1 is K-1, N1 = N
    ; N == max -> format(S,"~q.~n",[X]), K1 = K,    N1 = N 
    ;             format(S,"~q.~n",[X]), K1 = K,    N1 is N-1
    ),!,
    $pp_save_clauses_core(S,Xs1,K1,N1).
$pp_save_clauses_core(_,[],K,N) :-
    ( K =< 0, N == max -> true
    ; $pp_raise_warning($msg(3008))
    ),!.

%%----------------------------------------
%%  csv loader [RFC 4180]

load_csv(FileName,Rows) :-
    load_csv(FileName,Rows,[]).

load_csv(FileName,Rows,Opts) :-
    $pp_require_atom(FileName,$msg(3000),load_csv/3),
    $pp_proc_opts(Opts,$pp_load_csv_option,
                  [RFrom,RSize,CFrom,CSize,Pred,Conv,Quot,Cmnt,Miss],
                  [0,max,0,max,csvrow/1,1,34,none,_],
                  load_csv/3),
    open(FileName,read,Stream),
    $pp_load_csv_core(Stream,Rows,RFrom,RSize,CFrom,CSize,Pred,Conv,Quot,Cmnt,Miss),
    close(Stream),!.

$pp_load_csv_option(row_from(N),1,N) :-
    integer(N),N >= 0.
$pp_load_csv_option(row_skip(N),1,N) :-
    integer(N),N >= 0.
$pp_load_csv_option(row_size(N),2,N) :-
    integer(N),N >= 0 ; N == max.
$pp_load_csv_option(col_from(N),3,N) :-
    integer(N),N >= 0.
$pp_load_csv_option(col_skip(N),3,N) :-
    integer(N),N >= 0.
$pp_load_csv_option(col_size(N),4,N) :-
    integer(N),N >= 0 ; N == max.

$pp_load_csv_option(pred(X),5,Pred) :-
    ( X == [] -> Pred = []/0
    ; atom(X) -> Pred = X/1
    ; X = P/N -> atom(P),(N == 1;N == n),Pred = P/N
    ).

$pp_load_csv_option(parse_number(X),6,Flag) :-
    ( X == yes -> Flag = 1 ; X == no -> Flag = 0 ).
    
$pp_load_csv_option(double_quote(X),7,Code) :-
    ( X == yes -> Code = 34 ; X == no -> Code = none ).

$pp_load_csv_option(comment(X),8,Code) :-
    atom(X),atom_length(X,1),char_code(X,Code).
$pp_load_csv_option(comment,8,35).

$pp_load_csv_option(missing(X),9,Codes) :-
    atom(X),atom_codes(X,Codes).
$pp_load_csv_option(missing,9,'').

$pp_load_csv_core(_,[],_,0,_,_,_,_,_,_,_).
$pp_load_csv_core(S,Xs,K,N,J,M,Pred,Conv,Quot,Cmnt,Miss) :-
    $pp_load_csv_read(S,Row0,Conv,Quot,Cmnt,Miss),!,
    $pp_load_csv_extract(Row0,Row,J,M),
    Pred = Name/Style,
    ( Style == 0 -> X = Row
    ; Style == 1 -> X =.. [Name,Row]
    ; Style == n -> X =.. [Name|Row]
    ),
    ( K > 0    -> Xs = Xs1,     K1 is K - 1, N1 = N
    ; N == max -> Xs = [X|Xs1], K1 = K,      N1 = N
    ;             Xs = [X|Xs1], K1 = K,      N1 is N-1
    ),!,
    $pp_load_csv_core(S,Xs1,K1,N1,J,M,Pred,Conv,Quot,Cmnt,Miss).
$pp_load_csv_core(_,[],K,N,_,_,_,_,_,_,_) :-
    ( K =< 0, N == max -> true
    ; $pp_raise_runtime_error($msg(3005),invalid_csv_format,load_csv/3)
    ).

$pp_load_csv_extract(Row0,Row1,J,M), M  == max =>
    $pp_load_csv_extract_step1(Row0,Row1,J).
$pp_load_csv_extract(Row0,Row2,J,M), M \== max =>
    $pp_load_csv_extract_step1(Row0,Row1,J),
    $pp_load_csv_extract_step2(Row1,Row2,M).

$pp_load_csv_extract_step1(Xs,Xs,0).
$pp_load_csv_extract_step1([_|Xs],Ys,J) :-
    J1 is J-1,!,$pp_load_csv_extract_step1(Xs,Ys,J1).
$pp_load_csv_extract_step1(_,_,_) :-
    $pp_raise_runtime_error($msg(3006),invalid_csv_format,load_csv/3).

$pp_load_csv_extract_step2(_,[],0).
$pp_load_csv_extract_step2([Z|Xs],[Z|Ys],M) :-
    M1 is M-1,!,$pp_load_csv_extract_step2(Xs,Ys,M1).
$pp_load_csv_extract_step2(_,_,_) :-
    $pp_raise_runtime_error($msg(3006),invalid_csv_format,load_csv/3).

$pp_load_csv_read(S,Row,Conv,Quot,Cmnt,Miss) :-
    $pp_load_csv_skip(S,Cmnt),!,$pp_load_csv_q0(S,Conv,Miss,Quot,Row-[],Any-Any).

$pp_load_csv_skip(S,Cm) :-
    peek_code(S,Code),
    ( Code == -1 -> fail
    ; Code == Cm -> $pp_load_csv_skip(S),!,$pp_load_csv_skip(S,Cm)
    ; true
    ).

$pp_load_csv_skip(S) :-
    get_code(S,Code),
    ( Code =:= -1 -> fail
    ; Code =:= 10 -> true
    ; Code =:= 13 -> $pp_load_csv_crlf(S)
    ; $pp_load_csv_skip(S)
    ).

$pp_load_csv_crlf(S) :-
    ( peek_code(S,10) -> get_code(S,10) ; true ).

%%  3rd arg. = parse numeric values?
%%  4th arg. = missing value

$pp_load_csv_done(_,Codes-[],_,M) :-
    nonvar(M),Codes = M,!.
$pp_load_csv_done(Value,Codes-[],1,_) :-
    forall(member(Code,Codes),(32=<Code,Code<128)),
    catch(number_codes(Value,Codes),_,fail),!.
$pp_load_csv_done(Value,Codes-[],_,_) :-
    atom_codes(Value,Codes).

$pp_load_csv_q0(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys0) :-
    get_code(S,Code),
    ( Code == -1 ->             % EOF
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!
    ; Code == 10 ->             % LF
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!
    ; Code == 13 ->             % CR
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!,$pp_load_csv_crlf(S)
    ; Code == 44 ->             % ,
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs1],!,
        $pp_load_csv_q0(S,Cv,Ms,Dq,Xs1-Xs0,Any-Any)
    ; Code == Dq ->             % "
        !,$pp_load_csv_q2(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys0)
    ;                           % ELSE
        Ys0 = [Code|Ys1],!,$pp_load_csv_q1(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys1)
    ).

$pp_load_csv_q1(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys0) :-
    get_code(S,Code),
    ( Code == -1 ->             % EOF
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!
    ; Code == 10 ->             % LF
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!
    ; Code == 13 ->             % CR
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!,$pp_load_csv_crlf(S)
    ; Code == 44 ->             % ,
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs1],!,
        $pp_load_csv_q0(S,Cv,Ms,Dq,Xs1-Xs0,Any-Any)
    ; Code == Dq ->             % "
        close(S),!,
        $pp_raise_runtime_error($msg(3007),invalid_csv_format,load_csv/3)
    ;                           % ELSE
        Ys0 = [Code|Ys1],!,$pp_load_csv_q1(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys1)
    ).

$pp_load_csv_q2(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys0) :-
    get_code(S,Code),
    ( Code == -1 ->             % EOF
        close(S),!,
        $pp_raise_runtime_error($msg(3007),invalid_csv_format,load_csv/3)
    ; Code == Dq ->             % "
        !,$pp_load_csv_q3(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys0)
    ;                           % ELSE
        Ys0 = [Code|Ys1],!,$pp_load_csv_q2(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys1)
    ).

$pp_load_csv_q3(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys0) :-
    get_code(S,Code),
    ( Code == -1 ->             % EOF
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!
    ; Code == 10 ->             % LF
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!
    ; Code == 13 ->             % CR
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!,$pp_load_csv_crlf(S)
    ; Code == 44 ->             % ,
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs1],!,
        $pp_load_csv_q0(S,Cv,Ms,Dq,Xs1-Xs0,Any-Any)
    ; Code == Dq ->             % "
        Ys0 = [Code|Ys1],!,$pp_load_csv_q2(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys1)
    ;                           % ELSE
        close(S),!,
        $pp_raise_runtime_error($msg(3007),invalid_csv_format,load_csv/3)
    ).


%%----------------------------------------
%%  pretty e-graph printer

print_graph(G) :-
    current_output(S),print_graph(S,G, [] ).

print_graph(G,Opts) :-
    current_output(S),print_graph(S,G,Opts).

print_graph(S,G,Opts) :-
    $pp_require_list(G,$msg(2104),print_graph/3),
    $pp_proc_opts(Opts,$pp_print_graph_option,
                  [Lr0,And,Or0],
                  ["" ,"&","v"],
                  pring_graph/3),!,
    ( Lr0 == "" -> Colon = ":" ; Colon = "" ),
    length(Lr0,LenLr),
    length(Or0,LenOr),
    PadLr is LenOr-LenLr,$pp_print_graph_pad(Lr0,Lr,PadLr),
    PadOr is LenLr-LenOr,$pp_print_graph_pad(Or0,Or,PadOr),!,
    $pp_print_graph_roots(S,G,Colon,Lr,And,Or).

$pp_print_graph_option(lr(T) ,1,S) :- $pp_print_graph_optarg(T,S).
$pp_print_graph_option(and(T),2,S) :- $pp_print_graph_optarg(T,S).
$pp_print_graph_option(or(T) ,3,S) :- $pp_print_graph_optarg(T,S).

$pp_print_graph_optarg(T,S) :-
    ( atom(T) -> atom_codes(T,S)
    ; length(T,_),forall(member(X,T),(integer(X),0=<X,X=<255)) -> T = S
    ).

$pp_print_graph_pad(Xs,Ys,N), N =< 0 => Xs = Ys.
$pp_print_graph_pad(Xs,Ys,N), N  > 0 => Ys = [32|Ys1], N1 is N-1, !, $pp_print_graph_pad(Xs,Ys1,N1).

$pp_print_graph_roots(_,[],_,_,_,_).
$pp_print_graph_roots(S,[node(L,[])|Nodes],Colon,Lr,And,Or) :-
    format(S,"~w~n",[L]),!,
    $pp_print_graph_roots(S,Nodes,Colon,Lr,And,Or).
$pp_print_graph_roots(S,[node(L,Paths)|Nodes],Colon,Lr,And,Or) :-
    format(S,"~w~s~n",[L,Colon]),
    $pp_print_graph_paths(S,Paths,Lr,And,Or),!,
    $pp_print_graph_roots(S,Nodes,Colon,Lr,And,Or).
$pp_print_graph_roots(S,[node(L,[],V)|Nodes],Colon,Lr,And,Or) :-
    ( V = [V1,V2] ->
      format(S,"~w [~6g,~6g]~n",[L,V1,V2])
    ; format(S,"~w [~6g]~n",[L,V])
    ),!,
    $pp_print_graph_roots(S,Nodes,Colon,Lr,And,Or).
$pp_print_graph_roots(S,[node(L,Paths,V)|Nodes],Colon,Lr,And,Or) :-
    ( V = [V1,V2] ->
      format(S,"~w [~6g,~6g]~s~n",[L,V1,V2,Colon])
    ; format(S,"~w [~6g]~s~n",[L,V,Colon])
    ),!,
    $pp_print_graph_paths_aux(S,Paths,Lr,And,Or),!,
    $pp_print_graph_roots(S,Nodes,Colon,Lr,And,Or).

$pp_print_graph_paths(_,[],_,_,_).
$pp_print_graph_paths(_,[path([],[])],_,_,_) :- !.
$pp_print_graph_paths(S,[path(TNodes,SNodes)|Paths],Conn,And,Or) :-
    write(S,'  '),
    append(TNodes,SNodes,Nodes),
    $pp_print_graph_nodes(S,Nodes,Conn,And),
    nl(S),!,
    $pp_print_graph_paths(S,Paths,Or,And,Or).

$pp_print_graph_nodes(_,[],_,_).
$pp_print_graph_nodes(S,[Node|Nodes],Conn,And) :-
    format(S," ~s ~w",[Conn,Node]),!,
    $pp_print_graph_nodes(S,Nodes,And,And).

$pp_print_graph_paths_aux(_,[],_,_,_).
$pp_print_graph_paths_aux(_,[path([],[],_)],_,_,_) :- !.
$pp_print_graph_paths_aux(S,[path(TNodes,SNodes,V)|Paths],Conn,And,Or) :-
    write(S,'  '),
    append(TNodes,SNodes,Nodes),
    $pp_print_graph_nodes_aux(S,Nodes,Conn,And),
    write(S,'  '),
    ( V = [V1,V2] ->
      format(S,"{~6g,~6g}",[V1,V2])
    ; format(S,"{~6g}",[V])
    ),
    nl(S),!,
    $pp_print_graph_paths_aux(S,Paths,Or,And,Or).

$pp_print_graph_nodes_aux(_,[],_,_).
$pp_print_graph_nodes_aux(S,[Node|Nodes],Conn,And) :-
    ( Node = gnode(Label,Value) ; Node = snode(Label,Value) ),
    ( Value = [Value1,Value2] ->
      format(S," ~s ~w [~6g,~6g]",[Conn,Label,Value1,Value2])
    ; format(S," ~s ~w [~6g]",[Conn,Label,Value])
    ),!,
    $pp_print_graph_nodes_aux(S,Nodes,And,And).


%%----------------------------------------
%%  pretty tree printer

print_tree(T) :-
    current_output(S),print_tree(S,T,[]).

print_tree(T,Opts) :-
    current_output(S),print_tree(S,T,Opts).

print_tree(S,T,Opts) :-
    $pp_require_list(T,$msg(2104),print_tree/3),
    $pp_proc_opts(Opts,$pp_opts_print_tree,[Indent],[3],print_tree/3),
    number_codes(Indent,Format0),
    append("~",Format0,Format1),
    append(Format1,"s",Format2),
    $pp_print_tree_root(S,T,0,Format2).

$pp_opts_print_tree(indent(N),1,N) :-
    integer(N), N >= 1.

$pp_print_tree_root(S,[L|Sibs],K,Format) :-
    $pp_print_tree_node(S,L,K,Format),
    K1 is K + 1, !,
    $pp_print_tree_sibs(S,Sibs,K1,Format).

$pp_print_tree_sibs(_,Xs,_,_), Xs = [] =>
    true.
$pp_print_tree_sibs(S,Xs,K,Format), Xs = [X|Xs1] =>
    ( X ?= [_|_] ->
      $pp_print_tree_root(S,X,K,Format)
    ; $pp_print_tree_node(S,X,K,Format)
    ), !,
    $pp_print_tree_sibs(S,Xs1,K,Format).

$pp_print_tree_node(S,L,K,_), K == 0 =>
    write(S,L), nl(S).
$pp_print_tree_node(S,L,K,Format), K > 0 =>
    format(S,Format,["|"]),
    K1 is K - 1, !,
    $pp_print_tree_node(S,L,K1,Format).


%%----------------------------------------
%%  e-graph manipulator

strip_switches(G0,G1) :-
    $pp_require_list(G0,$msg(2104),strip_switches/2),
    $pp_strip_switches(G0,G1).

$pp_strip_switches([],[]).
$pp_strip_switches([node(L,Ps0)|Ns0],[node(L,Ps1)|Ns1]) :-
    $pp_strip_switches_sub(Ps0,Ps1),!,
    $pp_strip_switches(Ns0,Ns1).

$pp_strip_switches_sub([],[]).
$pp_strip_switches_sub([path(Gs,_)|Ps0],[Gs|Ps1]) :- !,
    $pp_strip_switches_sub(Ps0,Ps1).

%%----------------------------------------
%%  debugging aid

write_call(Goal) :-
    write_call([],Goal).

write_call(Opts,Goal) :-
    $pp_write_call_core(Opts,Goal,Goal).

??(Goal)  :- write_call([],Goal).
??*(Goal) :- write_call([all],Goal).
??>(Goal) :- write_call([call],Goal).
??<(Goal) :- write_call([exit+fail],Goal).
??+(Goal) :- write_call([exit],Goal).
??-(Goal) :- write_call([fail],Goal).

disable_write_call :-
    set_prism_flag(write_call_events,off).

$pp_write_call_core(Opts,Source,Goal) :-
    $pp_require_write_callable(Goal,$msg(3200),write_call/2),
    $pp_write_call_proc_opts(Opts,Call,Exit,Redo,Fail,Indent,Marker),
    $pp_write_call_print(Call,'Call',Indent,Marker,Source),
    ( Goal, ( $pp_write_call_print(Exit,'Exit',Indent,Marker,Source)
            ; $pp_write_call_print(Redo,'Redo',Indent,Marker,Source), fail
            )
    ; $pp_write_call_print(Fail,'Fail',Indent,Marker,Source), fail
    ).

$pp_write_call_build(Opts,Source,Goal,Body) :-
    Body = ( $pp_write_call_proc_opts(Opts,Call,Exit,Redo,Fail,Indent,Marker),
             $pp_write_call_print(Call,'Call',Indent,Marker,Source),
             ( Goal,( $pp_write_call_print(Exit,'Exit',Indent,Marker,Source)
                    ; $pp_write_call_print(Redo,'Redo',Indent,Marker,Source), fail
                    )
             ; $pp_write_call_print(Fail,'Fail',Indent,Marker,Source), fail
             )
           ),!.

$pp_write_call_proc_opts(Opts,Call,Exit,Redo,Fail,Indent,Marker) :-
    get_prism_flag(write_call_events,FlagValue),
    $pp_proc_opts(Opts,$pp_write_call_option,
                  [Events,Indent,Marker],[FlagValue,0,_],
                  write_call/2),
    ( FlagValue == off ->
      Call = 0, Exit = 0, Redo = 0, Fail = 0
    ; $pp_write_call_decomp(Events,Call,Exit,Redo,Fail)
    ), !.

$pp_write_call_option(X,1,Y) :-
    $pp_write_call_events(X,Y), !, Y \== none.
$pp_write_call_option(indent(X),2,X) :- !, integer(X).
$pp_write_call_option(marker(X),3,X) :- !.

$pp_write_call_events(all,all) :- !.
$pp_write_call_events(none,none) :- !.
$pp_write_call_events(X,Y) :-
    $pp_expr_to_list('+',X,Xs),
    $pp_write_call_events(Xs,Y,0,0,0,0),!.

$pp_write_call_events(Xs0,Y,C,E,R,F), Xs0 == [] =>
    $pp_write_call_decomp(Y,C,E,R,F), Y \== none.
$pp_write_call_events(Xs0,Y,C,E,R,F), Xs0 = [X|Xs1] =>
    ( X == call, C == 0 -> $pp_write_call_events(Xs1,Y,1,E,R,F)
    ; X == exit, E == 0 -> $pp_write_call_events(Xs1,Y,C,1,R,F)
    ; X == redo, R == 0 -> $pp_write_call_events(Xs1,Y,C,E,1,F)
    ; X == fail, F == 0 -> $pp_write_call_events(Xs1,Y,C,E,R,1)
    ).

$pp_write_call_decomp(none,0,0,0,0).
$pp_write_call_decomp(call,1,0,0,0).
$pp_write_call_decomp(exit,0,1,0,0).
$pp_write_call_decomp(call+exit,1,1,0,0).
$pp_write_call_decomp(redo,0,0,1,0).
$pp_write_call_decomp(call+redo,1,0,1,0).
$pp_write_call_decomp(exit+redo,0,1,1,0).
$pp_write_call_decomp(call+exit+redo,1,1,1,0).
$pp_write_call_decomp(fail,0,0,0,1).
$pp_write_call_decomp(call+fail,1,0,0,1).
$pp_write_call_decomp(exit+fail,0,1,0,1).
$pp_write_call_decomp(call+exit+fail,1,1,0,1).
$pp_write_call_decomp(redo+fail,0,0,1,1).
$pp_write_call_decomp(call+redo+fail,1,0,1,1).
$pp_write_call_decomp(exit+redo+fail,0,1,1,1).
$pp_write_call_decomp(all,1,1,1,1).

$pp_write_call_print(1,Head,Indent,Marker,Goal), var(Marker) =>
    tab(Indent), format("[~w] ~q~n",[Head,Goal]).
$pp_write_call_print(1,Head,Indent,Marker,Goal), nonvar(Marker) =>
    tab(Indent), format("[~w:~w] ~q~n",[Head,Marker,Goal]).
$pp_write_call_print(0,_,_,_,_).

%%----------------------------------------

$pp_learn_message(S,E,T,M) :-
    get_prism_flag(learn_message,LM),
    $pp_learn_message_decomp(LM,S,E,T,M),!.

%%----------------------------------------

$pp_learn_message_events(all,all) :- !.
$pp_learn_message_events(none,none) :- !.
$pp_learn_message_events(X,Y) :-
    $pp_expr_to_list('+',X,Xs),
    $pp_learn_message_events(Xs,Y,0,0,0,0).

$pp_learn_message_events(Xs0,Y,S,E,T,M), Xs0 == [] =>
    $pp_learn_message_decomp(Y,S,E,T,M), Y \== none.
$pp_learn_message_events(Xs0,Y,S,E,T,M), Xs0 = [X|Xs1] =>
    ( X == search, S = 0 -> $pp_learn_message_events(Xs1,Y,1,E,T,M)
    ; X == em,     E = 0 -> $pp_learn_message_events(Xs1,Y,S,1,T,M)
    ; X == stats,  T = 0 -> $pp_learn_message_events(Xs1,Y,S,E,1,M)
    ; X == misc,   M = 0 -> $pp_learn_message_events(Xs1,Y,S,E,T,1)
    ).

$pp_learn_message_decomp(none,             0,0,0,0).
$pp_learn_message_decomp(search,           1,0,0,0).
$pp_learn_message_decomp(em,               0,1,0,0).
$pp_learn_message_decomp(search+em,        1,1,0,0).
$pp_learn_message_decomp(stats,            0,0,1,0).
$pp_learn_message_decomp(search+stats,     1,0,1,0).
$pp_learn_message_decomp(em+stats,         0,1,1,0).
$pp_learn_message_decomp(search+em+stats,  1,1,1,0).
$pp_learn_message_decomp(misc,             0,0,0,1).
$pp_learn_message_decomp(search+misc,      1,0,0,1).
$pp_learn_message_decomp(em+misc,          0,1,0,1).
$pp_learn_message_decomp(search+em+misc,   1,1,0,1).
$pp_learn_message_decomp(stats+misc,       0,0,1,1).
$pp_learn_message_decomp(search+stats+misc,1,0,1,1).
$pp_learn_message_decomp(em+stats+misc,    0,1,1,1).
$pp_learn_message_decomp(all,              1,1,1,1).

%%----------------------------------------
%%  for parallel mode

$pp_require_mp_mode :-
    ( $pc_mp_mode -> true
    ; $pp_raise_internal_error($msg(1005),invalid_module,$damon_load/0)
    ).

%%----------------------------------------
%%  expand the outcome space

% ?- expand_values([3,2-5@2,1-3,t],X).
% X = [3,2,4,1,2,3,t] 

expand_values(Ns,ExpandedNs) :-
    $pp_require_list_or_nil(Ns,$msg(2109),expland_values/2),
    $pp_require_ground(Ns,$msg(1105),expand_values/2),
    $pp_expand_values1(Ns,ExpandedNs).

% just fails for errorneous inputs
expand_values1(Ns,ExpandedNs) :-
    is_list(Ns),
    ground(Ns),
    $pp_expand_values1(Ns,ExpandedNs).

$pp_expand_values1([],[]).
$pp_expand_values1([N|Ns],ENs) :-
    ( N = Start-End@Step,
      integer(Start),integer(End),integer(Step),Step>0 ->
        $pp_require_integer_range_incl(Start,End,$msg(2008),expand_values/2),
        $pp_expand_values2(Start,End,Step,ENs0),
        append(ENs0,ENs1,ENs)
    ; N = Start-End,integer(Start),integer(End) ->
        $pp_require_integer_range_incl(Start,End,$msg(2008),expand_values/2),
        $pp_expand_values2(Start,End,1,ENs0),
        append(ENs0,ENs1,ENs)
    ; ENs = [N|ENs1]
    ),!,
    $pp_expand_values1(Ns,ENs1).

$pp_expand_values2(Start,End,_,[]) :- Start > End.
$pp_expand_values2(Start,End,Step,[Start|Ns]) :-
    Start1 is Start + Step,!,
    $pp_expand_values2(Start1,End,Step,Ns).


%%----------------------------------------
%%  delete temporary file

$pp_delete_tmp_out :-
    Tmp = '__tmp.out',
    ( file_exists(Tmp) -> delete_file(Tmp)
    ; true
    ),!.


%%----------------------------------------
%%  log-gamma function

lngamma(X,G) :-
    $pp_require_positive_number(X,$msg(3400),lngamma/2),
    $pc_lngamma(X,G).
