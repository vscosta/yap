%% -*- Prolog -*-

%%======================================================================
%%
%%  [Notes on translation information]
%%
%%  This translator uses a term containing the global information shared
%%  by the translation processes.  It takes the form:
%%
%%      $trans_info(DoTable,TPredTab,NoDebug,PPredTab)
%%
%%  DoTable denotes whether probabilistic predicates should be tabled
%%  by default (i.e. unless declared in the source program); it takes
%%  1 if the predicates should be tabled; 0 otherwise.  In case of an
%%  unbound variable, DoTable should be considered to be 1.
%%
%%  TPredTab is a hashtable that contains tabled/non-tabled predicates
%%  which are compatible with the default (i.e. DoTable).  The key of
%%  each entry has the form P/N; the value is ignored.  In consultation
%%  mode where all probabilistic predicates are not tabled, TPredTab is
%%  just a free variable.
%%
%%  NoDebug indicates whether "write_call" should be disabled; any non-
%%  variable disables the feature.
%%
%%  PPredTab is a hashtable that contains probabilistic predicates found
%%  in the source program.  Each entry has the form P/N={0 or 1}, where
%%  the value is 1 if the predicate is tabled and 0 otherwise.
%%
%%======================================================================

%%----------------------------------------------------------------------
%%  Entry Point
%%----------------------------------------------------------------------

$pp_compile(PsmFile,DmpFile,OutFile) :-
    $pp_bpif_read_program(Prog0,PsmFile),
    new_hashtable(TPredTab),
    new_hashtable(PPredTab),
    Info = $trans_info(_DoTable,TPredTab,_NoDebug,PPredTab),
    $pp_trans_phase1(Prog0,Prog1,Info),
    $pp_trans_phase2(Prog1,Prog2,Info),
    $pp_trans_phase3(Prog2,Prog3,Info),
    $pp_trans_phase4(Prog3,Prog4,Info),
    $pp_trans_phase5(Prog4,Prog5,Info),
    Prog = Prog5,
    % $pp_dump_program(Prog),   % for debugging
    ( $pp_valid_program(Prog)
    ; $pp_raise_internal_error($msg(9802),invalid_compilation,$pp_compile/3)
    ),
    ( var(DmpFile) -> true ; $pp_save_program(Prog,DmpFile) ),
    $pp_bpif_compile_program(Prog,OutFile),!.


%%----------------------------------------------------------------------
%%  Phase #1: Scan the queries.
%%----------------------------------------------------------------------

$pp_trans_phase1(Prog0,Prog,Info) :-
    $pp_extract_decls(Prog0,Info),
    Prog = Prog0.

$pp_extract_decls([],_) => true.
$pp_extract_decls([Pred|Preds],Info), 
      Pred = pred($damon_load,0,_,_,_,[($damon_load:-Demon0)|_]) =>
    $pp_extract_decls_from_demons(Demon0,Info),!,
    $pp_extract_decls(Preds,Info).
$pp_extract_decls([_Pred|Preds],Info) =>
    $pp_extract_decls(Preds,Info).

$pp_extract_decls_from_demons((D1,D2),Info) =>
    $pp_extract_decls_from_demons(D1,Info),!,
    $pp_extract_decls_from_demons(D2,Info).
$pp_extract_decls_from_demons($query((p_table Preds)),Info) =>
    Info = $trans_info(DoTable,TPredTab,_,_),
    ( var(TPredTab) -> true    % consult mode
    ; DoTable == 1 ->
        $pp_add_preds_to_hashtable(Preds,TPredTab)
    ; var(DoTable) ->
        $pp_add_preds_to_hashtable(Preds,TPredTab),
        DoTable = 1
    ; DoTable == 0 ->
        $pp_raise_trans_error($msg(1101),mixed_table_declarations,$pp_trans_phase1/3)
    ; $pp_raise_unmatched_branches($pp_extract_decls_from_demons/2,
                                   query)
    ).
$pp_extract_decls_from_demons($query((p_not_table Preds)),Info) =>
    Info = $trans_info(DoTable,TPredTab,_,_),
    ( var(TPredTab) -> true    % consult mode
    ; DoTable == 0 ->
        $pp_add_preds_to_hashtable(Preds,TPredTab)
    ; var(DoTable) ->
        $pp_add_preds_to_hashtable(Preds,TPredTab),
        DoTable = 0
    ; DoTable == 1 ->
        $pp_raise_trans_error($msg(1101),mixed_table_declarations,$pp_trans_phase1/3)
    ; $pp_raise_unmatched_branches($pp_extract_decls_from_demons/2,
                                   p_not_table)
    ).
$pp_extract_decls_from_demons($query(disable_write_call),Info) =>
    Info = $trans_info(_,_,NoDebug,_),
    ( NoDebug == 1 -> true
    ; var(NoDebug) -> NoDebug = 1
    ; $pp_raise_unmatched_branches($pp_extract_decls_from_demons/2,
                                   disable_write_call)
    ).
$pp_extract_decls_from_demons(_,_Info) => true.

$pp_add_preds_to_hashtable((Pred,Preds),TPredTab) :- !,
    $pp_add_one_pred_to_hashtable(Pred,TPredTab),!,
    $pp_add_preds_to_hashtable(Preds,TPredTab).
$pp_add_preds_to_hashtable(Pred,TPredTab) :-
    $pp_add_one_pred_to_hashtable(Pred,TPredTab),!.

$pp_add_one_pred_to_hashtable(Pred,TPredTab) :-
    $pp_require_predicate_indicator(Pred,$msg(1102),$pp_trans_phase1/3),
    Pred = F/N,
    ( hashtable_get(TPredTab,F/N,_) -> true
    ; hashtable_register(TPredTab,F/N,1)
    ).

%%----------------------------------------------------------------------
%%  Phase #2: Process values/2-3.
%%----------------------------------------------------------------------

% We do not refer to the information objects here.
$pp_trans_phase2(Prog0,Prog,_Info) :-
    $pp_trans_values(Prog0,Prog1),
    $pp_replace_values(Prog1,Prog).

% translate the "values" declarations
$pp_trans_values(Preds0,Preds) :-
    $pp_trans_values(Preds0,Preds1,ValCls,Demon,DemonAux),
    Preds2 = [pred($pu_values,2,_Mode,_Delay,_Tabled,ValCls)|Preds1],
    DemonCl1 = ($damon_load:-Demon,DemonAux),
    DemonCl2 = ($damon_load:-true),
    Preds = [pred($damon_load,0,_,_,_,[DemonCl1,DemonCl2])|Preds2].

$pp_trans_values([],[],[],true,true).
$pp_trans_values([pred(F,2,_,_,_,Cls0)|Preds0],
                 Preds,ValCls,Demon,DemonAux) :-
    (F = values ; F = values_x),!,
    $pp_trans_values_clauses(Cls0,Cls1),
    append(Cls1,ValCls1,ValCls),!,
    $pp_trans_values(Preds0,Preds,ValCls1,Demon,DemonAux).
$pp_trans_values([pred(F,3,_,_,_,Cls0)|Preds0],
                 Preds,ValCls,Demon,DemonAux) :-
    (F = values ; F = values_x),!,
    $pp_trans_values_demon_clauses(Cls0,Cls1,DemonAux),
    append(Cls1,ValCls1,ValCls),!,
    $pp_trans_values(Preds0,Preds,ValCls1,Demon,_).
$pp_trans_values([pred($damon_load,0,_,_,_,[($damon_load:-Demon)|_])|Preds0],
                 Preds,ValCls,Demon,DemonAux) :- !,
    $pp_trans_values(Preds0,Preds,ValCls,_,DemonAux).
$pp_trans_values([P|Preds0],[P|Preds],ValCls,Demon,DemonAux) :- !,
    $pp_trans_values(Preds0,Preds,ValCls,Demon,DemonAux).

$pp_trans_values_clauses([],[]).
$pp_trans_values_clauses([Cl0|Cls0],[Cl|Cls]) :-
    $pp_trans_values_one_clause(Cl0,Cl),!,
    $pp_trans_values_clauses(Cls0,Cls).

$pp_trans_values_one_clause(Cl0,Cl) :-
    ( Cl0 = (values(Sw,Vals0):-Body)   -> true
    ; Cl0 = (values_x(Sw,Vals0):-Body) -> true
    ; Cl0 = values(Sw,Vals0)           -> Body = true
    ; Cl0 = values_x(Sw,Vals0)         -> Body = true
    ),
    $pp_build_expand_values(Vals0,Vals,Expand),
    Cl = ($pu_values(Sw,Vals):-Body,Expand).

$pp_trans_values_demon_clauses([],[],true).
$pp_trans_values_demon_clauses([Cl0|Cls0],[Cl|Cls],Demon) :-
    ( Cl0 = (values(Sw,Vals0,Demons):-Body)   -> true
    ; Cl0 = (values_x(Sw,Vals0,Demons):-Body) -> true
    ; Cl0 = values(Sw,Vals0,Demons)           -> Body = true
    ; Cl0 = values_x(Sw,Vals0,Demons)         -> Body = true
    ),
    $pp_build_expand_values(Vals0,Vals,Expand),
    Cl = ($pu_values(Sw,Vals):-Body,Expand),
    ( ground(Sw),ground(Demons)
        -> $pp_trans_values_demons(Sw,Demons,Demon1), Demon = (Demon1,Demon2)
    ; $pp_raise_warning($msg(1104),[Sw,Demons]), Demon = Demon2
    ),!,
    $pp_trans_values_demon_clauses(Cls0,Cls,Demon2).

$pp_trans_values_demons(_Sw,true,true) :- !.
$pp_trans_values_demons(Sw,(Demon0,Demons),(Demon2,Demon1)) :- !,
    $pp_trans_values_demons(Sw,Demon0,Demon2),!,
    $pp_trans_values_demons(Sw,Demons,Demon1).
$pp_trans_values_demons(Sw,Demon0,Demon) :-
    ( Demon0 = set@Params    -> Demon = $query(set_sw(Sw,Params))
    ; Demon0 = fix@Params    -> Demon = $query(fix_sw(Sw,Params))
    ; Demon0 = a@HParams     -> Demon = $query(set_sw_a(Sw,HParams))
    ; Demon0 = d@HParams     -> Demon = $query(set_sw_d(Sw,HParams))
    ; Demon0 = h@HParams     -> Demon = $query(set_sw_d(Sw,HParams))
    ; Demon0 = set_a@HParams -> Demon = $query(set_sw_a(Sw,HParams))
    ; Demon0 = set_d@HParams -> Demon = $query(set_sw_d(Sw,HParams))
    ; Demon0 = set_h@HParams -> Demon = $query(set_sw_d(Sw,HParams))
    ; Demon0 = fix_a@HParams -> Demon = $query(fix_sw_a(Sw,HParams))
    ; Demon0 = fix_d@HParams -> Demon = $query(fix_sw_d(Sw,HParams))
    ; Demon0 = fix_h@HParams -> Demon = $query(fix_sw_d(Sw,HParams))
    ; Demon0 = Params        -> Demon = $query(set_sw(Sw,Params))
    ).

$pp_build_expand_values(Vals0,Vals,Expand) :-
    ( $pp_unexpandable_values(Vals0) -> Expand = true, Vals = Vals0
    ; Expand = expand_values1(Vals0,Vals)   % use the no-exception version
    ).

% Checks if Vals only contains ground values that cannot be expanded by
% expand_values{,1}/2:
$pp_unexpandable_values(Vals) :-
    is_list(Vals),
    ground(Vals),
    $pp_unexpandable_values1(Vals).

$pp_unexpandable_values1([]).
$pp_unexpandable_values1([V|Vals]) :-
    ( V \= _Start-_End@_Step ; V \= _Start-_End ),!,
    $pp_unexpandable_values1(Vals).


% replace all appearances of values/2 in the clause bodies with get_values/2
$pp_replace_values([],[]).
$pp_replace_values([Pred0|Preds0],[Pred|Preds]) :-
    Pred0 = pred(F,N,Mode,Delay,Tabled,Cls0),
    Pred = pred(F,N,Mode,Delay,Tabled,Cls),
    $pp_replace_values_clauses(Cls0,Cls),!,
    $pp_replace_values(Preds0,Preds).
    
$pp_replace_values_clauses([],[]).
$pp_replace_values_clauses([Cl0|Cls0],[Cl|Cls]) :-
    $pp_replace_values_one_clause(Cl0,Cl),!,
    $pp_replace_values_clauses(Cls0,Cls).

$pp_replace_values_one_clause(Cl0,Cl) :-
    ( Cl0 = (Head:-Body0) ->
        $pp_replace_values_body(Body0,Body), Cl = (Head:-Body)
    ; Cl = Cl0
    ).

$pp_replace_values_body((G1,G2),(NG1,NG2)) :- !,
    $pp_replace_values_body(G1,NG1),
    $pp_replace_values_body(G2,NG2).
$pp_replace_values_body((G1;G2),(NG1;NG2)) :- !,
    $pp_replace_values_body(G1,NG1),
    $pp_replace_values_body(G2,NG2).
$pp_replace_values_body(not(G),not(NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((\+ G),(\+ NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((C->G),(NC->NG)) :- !,
    $pp_replace_values_body(C,NC),
    $pp_replace_values_body(G,NG).
$pp_replace_values_body(write_call(G),write_call(NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body(write_call(Opts,G),write_call(Opts,NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((?? G),(?? NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((??* G),(??* NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((??> G),(??> NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((??< G),(??< NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((??+ G),(??+ NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((??- G),(??- NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body(values(Sw,Vals),get_values(Sw,Vals)) :-  !.
$pp_replace_values_body(G,G).


%%----------------------------------------------------------------------
%%  Phase #3: Find probabilistic predicates.
%%----------------------------------------------------------------------

$pp_trans_phase3(Prog0,Prog,Info) :-
    $pp_analyze(Prog0,Info),
    Prog = Prog0.

$pp_analyze(Prog,Info) :-
    Info = $trans_info(_,_,_,PPredTab),
    $pp_collect_preds(Prog,PPredTab),
    $pp_infer_prob_preds_fixpoint(Prog,Info),
    $pp_complete_prob_preds(Info),
    $pp_assert_prob_preds(Prog,Info).

% collect the predicates appearing in the program
$pp_collect_preds([],_).
$pp_collect_preds([pred($damon_load,0,_,_,_,_)|Preds],PPredTab) :- !,
    hashtable_register(PPredTab,$damon_load/0,_),!,
    $pp_collect_preds(Preds,PPredTab).
$pp_collect_preds([pred(values,2,_,_,_,_)|Preds],PPredTab) :- !,
    hashtable_register(PPredTab,values/2,_),!,
    $pp_collect_preds(Preds,PPredTab).
$pp_collect_preds([pred(F,N,_Mode,_Delay,_Tabled,_Cls)|Preds],PPredTab) :-
    hashtable_register(PPredTab,F/N,_),!,
    $pp_collect_preds(Preds,PPredTab).

$pp_infer_prob_preds_fixpoint(Prog,Info) :-
    Info = $trans_info(_,_,_,PPredTab),
    global_set($pg_prob_tab_updated,0,0),
    $pp_infer_prob_preds(Prog,PPredTab),
        % if some probabilistic predicate have been newly found, try again:
    ( global_get($pg_prob_tab_updated,0,1)
        -> $pp_infer_prob_preds_fixpoint(Prog,Info)
    ; true
    ).

$pp_infer_prob_preds([],_PPredTab) => true.
$pp_infer_prob_preds([pred(values,2,_,_,_,_)|Preds],PPredTab) =>
    $pp_infer_prob_preds(Preds,PPredTab).
$pp_infer_prob_preds([pred(F,N,_Mode,_Delay,_Tab,Cls)|Preds],PPredTab) =>
    hashtable_get(PPredTab,F/N,IsProb),
    ( var(IsProb) -> $pp_infer_prob_cls(Cls,IsProb,PPredTab),
      ( nonvar(IsProb) -> global_set($pg_prob_tab_updated,0,1)
      ; true
      )
    ; true
    ),!,
    $pp_infer_prob_preds(Preds,PPredTab).

$pp_infer_prob_cls([],_IsProb,_PPredTab) => true.
$pp_infer_prob_cls([Cl|Cls],IsProb,PPredTab) =>
    $pp_infer_prob_cl(Cl,IsProb,PPredTab),
    ( var(IsProb) -> $pp_infer_prob_cls(Cls,IsProb,PPredTab)
    ; true
    ).

$pp_infer_prob_cl((_H:-B),IsProb,PPredTab) =>
    $pp_infer_prob_body(B,IsProb,PPredTab).
$pp_infer_prob_cl(_H,_IsProb,_PPredTab) => true.

$pp_infer_prob_body((G1,G2),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab),
    ( var(IsProb) -> $pp_infer_prob_body(G2,IsProb,PPredTab)
    ; true
    ).
$pp_infer_prob_body((C->G1;G2),IsProb,PPredTab) =>
    $pp_infer_prob_body(C,IsProb,PPredTab),
    ( var(IsProb) ->
        $pp_infer_prob_body(G1,IsProb,PPredTab),
        ( var(IsProb) -> $pp_infer_prob_body(G2,IsProb,PPredTab)
        ; true
        )
      ; true
    ).
$pp_infer_prob_body((G1;G2),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab),
    ( var(IsProb) -> $pp_infer_prob_body(G2,IsProb,PPredTab)
    ; true
    ).
$pp_infer_prob_body(not(G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((\+ G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((C->G1),IsProb,PPredTab) =>    
    $pp_infer_prob_body(C,IsProb,PPredTab),
    ( var(IsProb) -> $pp_infer_prob_body(G1,IsProb,PPredTab)
    ; true
    ).
$pp_infer_prob_body(write_call(G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body(write_call(_,G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((?? G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((??* G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((??> G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((??< G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((??+ G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((??- G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body(msw(_,_,_),IsProb,_PPredTab) => IsProb = 1.
$pp_infer_prob_body(msw(_,_),IsProb,_PPredTab) => IsProb = 1.
$pp_infer_prob_body(G,IsProb,PPredTab) :-
    functor(G,F,N),
    hashtable_get(PPredTab,F/N,IsProb1),!,
    ( nonvar(IsProb1) -> IsProb = 1
    ; true
    ).
$pp_infer_prob_body(_G,_IsProb,_PPredTab).  /* G: undefined predicates */
    
$pp_complete_prob_preds(Info) :-
    Info = $trans_info(_,_,_,PPredTab),
    hashtable_keys_to_list(PPredTab,Preds),
    $pp_complete_prob_preds(Preds,PPredTab).

$pp_complete_prob_preds([],_).
$pp_complete_prob_preds([F/N|Preds],PPredTab) :-
    hashtable_get(PPredTab,F/N,IsProb),!,
    ( var(IsProb) -> IsProb = 0
    ; true
    ),!,
    $pp_complete_prob_preds(Preds,PPredTab).

$pp_assert_prob_preds([],_). 
$pp_assert_prob_preds([pred(F,N,_,_,_,_)|Preds],Info) :-
    Info = $trans_info(DoTable,TPredTab,_,PPredTab),
    hashtable_get(PPredTab,F/N,IsProb),!,
    ( IsProb = 1 ->
        $pp_abolish_compiled_pred(F,N),
        ( $pd_is_prob_pred(F,N) -> true
        ; assert($pd_is_prob_pred(F,N))
        ),
        ( $pp_is_tabled_prob_pred(F/N,DoTable,TPredTab)
            -> ( $pd_is_tabled_pred(F,N) -> true
               ; assert($pd_is_tabled_pred(F,N))
               )
        ; true
        )
    ; true
    ),!,
    $pp_assert_prob_preds(Preds,Info).

$pp_abolish_compiled_pred(F,N) :-
    $pp_trans_prob_pred_name(F,NewF),
    global_del(NewF,N),!.


%%----------------------------------------------------------------------
%%  Phase #4: Translate the probabilistic predicates.
%%----------------------------------------------------------------------

% [Note] Mode indicators in B-Prolog:
%   c (or +) : closed term
%   f (or -) : free variable
%   nv       : non-variable term
%   d (or ?) : dont-know term

$pp_trans_phase4(Prog0,Prog,Info) :-
    $pp_trans_prob_preds(Prog0,Prog,Info).

$pp_trans_prob_preds([],Prog,_Info) => Prog = [].
$pp_trans_prob_preds([Pred|Preds],Prog,Info),
      Pred = pred(F,N,Mode,Delay,Tabled,Cls) =>
    Info = $trans_info(_,_,NoDebug,_),
    ( $pd_is_prob_pred(F,N) ->
        Prog = [pred(F,N,Mode,Delay,_,Cls1),NewPred|Prog1],
        ( $pd_is_tabled_pred(F,N) ->
            NewTabled = tabled(_,_,_,_),
            ( nonvar(Mode) -> NewMode = [f|Mode] ; true),
            NewArity is N + 1
        ; % \+ $is_tabled_pred(F,N)
          ( nonvar(Mode) -> NewMode = [d,d,d,d|Mode]
          ; true
          ),
          NewArity is N + 4
        ),
        NewPred = pred(NewF,NewArity,NewMode,_,NewTabled,NewCls),
        $pp_trans_prob_pred_name(F,NewF),
        copy_term(Cls,ClsCp),   % Pred and NewPred do not share variables
        $pp_trans_prob_cls(ClsCp,NewCls,NewF,NewTabled,Info)
    ; % \+ $pd_is_prob_pred(F,N)
      Prog = [pred(F,N,Mode,Delay,Tabled,Cls1)|Prog1]
    ),
    ( var(NoDebug) -> Cls1 = Cls
    ; $pp_strip_write_call_cls(Cls,Cls1)  % just strip the write_call predicates
    ),!,
    $pp_trans_prob_preds(Preds,Prog1,Info).

$pp_trans_prob_cls([],Cls,_F,_Tabled,_Info) => Cls = [].
$pp_trans_prob_cls([(Head0:-Body0)|Cls0],Cls,F,Tabled,Info) =>
    Cls = [(Head:-Body)|Cls1],
    Head0 =.. [_|Args],
    ((nonvar(Tabled),Tabled = tabled(_,_,_,_)) ->
        Head =.. [F,Gid0|Args],
        $pp_trans_prob_body(Body0,Body1,Gids,[],Sids,[],Info),
        ( Gids == [], Sids == [] -> RegistPath = true
        ; RegistPath =
            catch($prism_eg_path(Gid0,Gids,Sids),
                  Exception,
                  ($pp_emit_message($msg(9805),[Head0]),throw(Exception)))
            % FIXME: this translation may lead to some overhead
        ),
        Body = (Body1,
                $pc_prism_goal_id_register(Head0,Gid0),
                RegistPath)
    ; % Non-tabled
      Head =.. [F,Gids,GidsR,Sids,SidsR|Args],
      $pp_trans_prob_body(Body0,Body1,Gids,GidsR,Sids,SidsR,Info),
      Body = Body1
    ),!,
    $pp_trans_prob_cls(Cls0,Cls1,F,Tabled,Info).
$pp_trans_prob_cls([Head|Cls0],Cls,F,Tabled,Info) =>
    $pp_trans_prob_cls([(Head:-true)|Cls0],Cls,F,Tabled,Info).

$pp_trans_prob_body((G1,G2),NewGoal,Gids,GidsR,Sids,SidsR,Info) =>
    NewGoal = (NG1,NG2),
    $pp_trans_prob_body(G1,NG1,Gids,Gids1,Sids,Sids1,Info),
    $pp_trans_prob_body(G2,NG2,Gids1,GidsR,Sids1,SidsR,Info).
$pp_trans_prob_body((C->A;B),NewGoal,Gids,GidsR,Sids,SidsR,Info) =>
    NewGoal = (InitVars,
               (NC->
                (NA,Gids=GidsCp1,Sids=SidsCp1,GidsR=GidsRCp1,SidsR=SidsRCp1)
               ;(NB,Gids=GidsCp2,Sids=SidsCp2,GidsR=GidsRCp2,SidsR=SidsRCp2))),
    $pp_trans_prob_body(C,NC,GidsCp1,GidsCp3,SidsCp1,SidsCp3,Info),
    $pp_trans_prob_body(A,NA,GidsCp3,GidsRCp1,SidsCp3,SidsRCp1,Info),
    $pp_trans_prob_body(B,NB,GidsCp2,GidsRCp2,SidsCp2,SidsRCp2,Info),
    vars_set((NA;NB),Vars),
    $pp_gen_initialize_var([Vars,Gids,Sids,GidsR,SidsR,
                            GidsCp1,SidsCp1,GidsRCp1,SidsRCp1,
                            GidsCp2,SidsCp2,GidsRCp2,SidsRCp2,
                            GidsCp3,SidsCp3],InitVars).
$pp_trans_prob_body((A;B),NewGoal,Gids,GidsR,Sids,SidsR,Info) =>
    NewGoal = (InitVars,
               ((NA,Gids=GidsCp1,Sids=SidsCp1,GidsR=GidsRCp1,SidsR=SidsRCp1)
               ;(NB,Gids=GidsCp2,Sids=SidsCp2,GidsR=GidsRCp2,SidsR=SidsRCp2))),
    $pp_trans_prob_body(A,NA,GidsCp1,GidsRCp1,SidsCp1,SidsRCp1,Info),
    $pp_trans_prob_body(B,NB,GidsCp2,GidsRCp2,SidsCp2,SidsRCp2,Info),
    vars_set((NA;NB),Vars),
    $pp_gen_initialize_var([Vars,Gids,Sids,GidsR,SidsR,
                            GidsCp1,SidsCp1,GidsRCp1,SidsRCp1,
                            GidsCp2,SidsCp2,GidsRCp2,SidsRCp2],InitVars).
$pp_trans_prob_body(not(G),NewGoal,Gids,GidsR,Sids,SidsR,Info) =>
    NewGoal = not(NG),
    Gids = GidsR,
    Sids = SidsR,
    $pp_trans_prob_body(G,NG,Gids,_,Sids,_,Info).
$pp_trans_prob_body(\+(G),NewGoal,Gids,GidsR,Sids,SidsR,Info) =>
    NewGoal = \+(NG),
    Gids = GidsR,
    Sids = SidsR,
    $pp_trans_prob_body(G,NG,Gids,_,Sids,_,Info).
$pp_trans_prob_body((C->A),NewGoal,Gids,GidsR,Sids,SidsR,Info) =>
    NewGoal = (NC->NA),
    $pp_trans_prob_body(C,NC,Gids,Gids1,Sids,Sids1,Info),
    $pp_trans_prob_body(A,NA,Gids1,GidsR,Sids1,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = write_call(Goal1) =>
    $pp_trans_prob_body(write_call([],Goal1),
                        NewGoal,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = write_call(Opts,Goal1) =>
    Info = $trans_info(_,_,NoDebug,_),
    ( $pp_is_write_callable(Goal1) -> true
    ; $pp_raise_trans_error($msg(1103),not_write_callable,$pp_trans_phase4/3)
    ),
    ( var(NoDebug) -> $pp_write_call_build(Opts,Goal1,NewGoal1,NewGoal)
    ; NewGoal1 = NewGoal
    ),!,
    $pp_trans_prob_body(Goal1,NewGoal1,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = (?? Goal1) =>
    $pp_trans_prob_body(write_call([],Goal1),
                        NewGoal,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = (??* Goal1) =>
    $pp_trans_prob_body(write_call([all],Goal1),
                        NewGoal,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = (??> Goal1) =>
    $pp_trans_prob_body(write_call([call],Goal1),
                        NewGoal,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = (??< Goal1) =>
    $pp_trans_prob_body(write_call([exit+fail],Goal1),
                        NewGoal,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = (??+ Goal1) =>
    $pp_trans_prob_body(write_call([exit],Goal1),
                        NewGoal,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = (??- Goal1) =>
    $pp_trans_prob_body(write_call([fail],Goal1),
                        NewGoal,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,_Info),
      Goal = msw(I,V) =>
    Gids = GidsR,
    Sids = [Sid|SidsR],
    NewGoal = $prism_expl_msw(I,V,Sid).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info) :-
    Info = $trans_info(DoTable,TPredTab,_,_),
    functor(Goal,F,N),
    $pd_is_prob_pred(F,N),!,
    Goal =.. [_|Args],
    $pp_trans_prob_pred_name(F,NewF),
    ( $pp_is_tabled_prob_pred(F/N,DoTable,TPredTab) ->
        NewGoal =.. [NewF,Gid|Args], 
        Gids = [Gid|GidsR],
        Sids = SidsR
    ; NewGoal =.. [NewF,Gids,GidsR,Sids,SidsR|Args]
    ).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,_Info) :-
    Sids = SidsR,
    Gids = GidsR,
    Goal = NewGoal.

$pp_strip_write_call_cls([],Cls)=> Cls = [].
$pp_strip_write_call_cls([(Head:-Body0)|Cls0],Cls) =>
    Cls = [(Head:-Body)|Cls1],
    $pp_strip_write_call_body(Body0,Body),!,
    $pp_strip_write_call_cls(Cls0,Cls1).
$pp_strip_write_call_cls([Head|Cls0],Cls) =>
    Cls = [Head|Cls1],!,
    $pp_strip_write_call_cls(Cls0,Cls1).

$pp_strip_write_call_body((A0,B0),Goal) =>
    Goal = (A1,B1),
    $pp_strip_write_call_body(A0,A1),
    $pp_strip_write_call_body(B0,B1).
$pp_strip_write_call_body((A0->B0;C0),Goal) =>
    Goal = (A1->B1;C1),
    $pp_strip_write_call_body(A0,A1),
    $pp_strip_write_call_body(B0,B1),
    $pp_strip_write_call_body(C0,C1).
$pp_strip_write_call_body((A0;B0),Goal) =>
    Goal = (A1;B1),
    $pp_strip_write_call_body(A0,A1),
    $pp_strip_write_call_body(B0,B1).
$pp_strip_write_call_body(not(A0),Goal) =>
    Goal = not(A1),
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body(\+(A0),Goal) =>
    Goal = \+(A1),
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body((A0->B0),Goal) =>
    Goal = (A1->B1),
    $pp_strip_write_call_body(A0,A1),
    $pp_strip_write_call_body(B0,B1).
$pp_strip_write_call_body(write_call(A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body(write_call(_,A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body((??  A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body((??* A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body((??> A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body((??< A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body((??+ A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body((??- A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body(Goal0,Goal) => Goal = Goal0.

$pp_gen_initialize_var(VarsL,InitVars):-
    flatten(VarsL,Vars0),
    sort(Vars0,Vars),
    $pp_gen_initialize_var_aux(Vars,InitVarsL),
    list_to_and(InitVarsL,InitVars).

$pp_gen_initialize_var_aux([],[]).
$pp_gen_initialize_var_aux([Var|Vars],InitVars):-
    ( var(Var) -> InitVars = ['_$initialize_var'(Var)|InitVars1]
    ; InitVars = InitVars1
    ),!,
    $pp_gen_initialize_var_aux(Vars,InitVars1).

%%----------------------------------------------------------------------
%%  Phase #5: Add assert calls to the first demon call.
%%----------------------------------------------------------------------

$pp_trans_phase5(Prog0,Prog,Info) :-
    $pp_add_assert_calls(Prog0,Prog,Info).

$pp_add_assert_calls([],[],_).
$pp_add_assert_calls([Pred|Preds],[Pred1|Preds1],Info) :-
    Pred = pred($damon_load,0,_,_,_,[($damon_load:-Demon)|DemonCls]),
    $pp_build_assert_calls(Info,AssertCalls),
    Demon1 = ($query(retractall($pd_is_prob_pred(_,_))),
              $query(retractall($pd_is_tabled_pred(_,_))),
              $query(call(AssertCalls)),
              Demon),
    Pred1 = pred($damon_load,0,_,_,_,[($damon_load:-Demon1)|DemonCls]),!,
    $pp_add_assert_calls(Preds,Preds1,Info).
$pp_add_assert_calls([Pred|Preds],[Pred|Preds1],Info) :- !,
    $pp_add_assert_calls(Preds,Preds1,Info).

$pp_build_assert_calls(Info,AssertCalls) :-
    Info = $trans_info(_,_,_,PPredTab),
    hashtable_to_list(PPredTab,Pairs),
    $pp_build_assert_calls1(Pairs,Info,AssertGs),
    list_to_and(AssertGs,AssertCalls).

$pp_build_assert_calls1([],_,[]).
$pp_build_assert_calls1([Pair|Pairs],Info,AssertGs) :-
    Info = $trans_info(DoTable,TPredTab,_,_),
    ( Pair = (F/N=V) ->
      ( V == 1 ->
          AssertGs = [assert($pd_is_prob_pred(F,N))|AssertGs2],
          ( $pp_is_tabled_prob_pred(F/N,DoTable,TPredTab) ->
              AssertGs2 = [assert($pd_is_tabled_pred(F,N))|AssertGs1]
          ; AssertGs2 = AssertGs1
          )
      ; V == 0 -> AssertGs = AssertGs1
      ; $pp_raise_unmatched_branches($pp_build_assert_calls1/3,value)
      )
    ; $pp_raise_unmatched_branches($pp_build_assert_calls1/3,pair)
    ),!,
    $pp_build_assert_calls1(Pairs,Info,AssertGs1).


%%----------------------------------------
%% Auxiliary predicates for translation

'_$initialize_var'(_).
'_$if_then_else'(C,A,B) :- (C->A;B).

%%----------------------------------------
%% Miscellaneous routines

$pp_trans_prob_pred_name(F,NewF) :-
    name(F,FString),
    append("$pu_expl_",FString,NewFString),
    name(NewF,NewFString).


$pp_is_tabled_prob_pred(F/N,DoTable,TPredTab) :-
    ( var(TPredTab) -> fail   % consult mode
    ; true
    ),!,
    ( DoTable == 1 -> hashtable_get(TPredTab,F/N,_)
    ; DoTable == 0 ->
        ( hashtable_get(TPredTab,F/N,_) -> fail
        ; true
        )
    ; var(DoTable) -> true
    ),!.


$pp_add_conj_to_list((A,B),List) =>
    $pp_add_conj_to_list(A,List),!,
    $pp_add_conj_to_list(B,List).
$pp_add_conj_to_list(A,List) =>
    $member1(A,List).
