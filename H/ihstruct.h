

























#ifdef EUROTRA
  Yap_heap_regs->term_dollar_u = MkAtomTerm(AtomDollarU);
#endif
  Yap_heap_regs->term_prolog = MkAtomTerm(AtomProlog);
  Yap_heap_regs->term_refound_var = MkAtomTerm(AtomRefoundVar);
  Yap_heap_regs->user_module = MkAtomTerm(AtomUser);
  Yap_heap_regs->idb_module = MkAtomTerm(AtomIDB);
  Yap_heap_regs->attributes_module = MkAtomTerm(AtomAttributes);
  Yap_heap_regs->charsio_module = MkAtomTerm(AtomCharsio);
  Yap_heap_regs->terms_module = MkAtomTerm(AtomTerms);
  Yap_heap_regs->system_module = MkAtomTerm(AtomSystem);
  Yap_heap_regs->readutil_module = MkAtomTerm(AtomReadutil);
  Yap_heap_regs->hacks_module = MkAtomTerm(AtomYapHacks);
  Yap_heap_regs->arg_module = MkAtomTerm(AtomArg);
  Yap_heap_regs->globals_module = MkAtomTerm(AtomNb);
  Yap_heap_regs->swi_module = MkAtomTerm(AtomSwi);



  Yap_heap_regs->current_modules = NULL;


  Yap_InitModules();


#if USE_THREADED_CODE

#endif

  Yap_heap_regs->execution_mode = INTERPRETED;

  Yap_heap_regs->execute_cpred_op_code = Yap_opcode(_execute_cpred);
  Yap_heap_regs->expand_op_code = Yap_opcode(_expand_index);
  Yap_heap_regs->fail_op = Yap_opcode(_op_fail);
  Yap_heap_regs->index_op = Yap_opcode(_index_pred);
  Yap_heap_regs->lockpred_op = Yap_opcode(_lock_pred);
  Yap_heap_regs->undef_op = Yap_opcode(_undef_p);

  InitPredHash();
#if defined(YAPOR) || defined(THREADS)

#endif



  Yap_heap_regs->creep_code = RepPredProp(PredPropByFunc(Yap_MkFunctor(AtomCreep,1),PROLOG_MODULE));
  Yap_heap_regs->undef_code = RepPredProp(PredPropByFunc(Yap_MkFunctor(AtomUndefp,1),PROLOG_MODULE));
  Yap_heap_regs->spy_code = RepPredProp(PredPropByFunc(Yap_MkFunctor(AtomSpy,1),PROLOG_MODULE));
  Yap_heap_regs->pred_fail = RepPredProp(PredPropByAtom(AtomFail,PROLOG_MODULE));
  Yap_heap_regs->pred_true = RepPredProp(PredPropByAtom(AtomTrue,PROLOG_MODULE));
#ifdef COROUTINING
  Yap_heap_regs->num_of_atts = 1;
  Yap_heap_regs->wake_up_code = RepPredProp(PredPropByFunc(Yap_MkFunctor(AtomWakeUpGoal,2),PROLOG_MODULE));
#endif
  Yap_heap_regs->pred_goal_expansion = RepPredProp(PredPropByFunc(FunctorGoalExpansion,USER_MODULE));
  Yap_heap_regs->pred_meta_call = RepPredProp(PredPropByFunc(FunctorMetaCall,PROLOG_MODULE));
  Yap_heap_regs->pred_dollar_catch = RepPredProp(PredPropByFunc(FunctorCatch,PROLOG_MODULE));
  Yap_heap_regs->pred_recorded_with_key = RepPredProp(PredPropByFunc(FunctorRecordedWithKey,PROLOG_MODULE));
  Yap_heap_regs->pred_log_upd_clause = RepPredProp(PredPropByFunc(FunctorDoLogUpdClause,PROLOG_MODULE));
  Yap_heap_regs->pred_log_upd_clause_erase = RepPredProp(PredPropByFunc(FunctorDoLogUpdClauseErase,PROLOG_MODULE));
  Yap_heap_regs->pred_log_upd_clause0 = RepPredProp(PredPropByFunc(FunctorDoLogUpdClause,PROLOG_MODULE));
  Yap_heap_regs->pred_static_clause = RepPredProp(PredPropByFunc(FunctorDoStaticClause,PROLOG_MODULE));
  Yap_heap_regs->pred_throw = RepPredProp(PredPropByFunc(FunctorThrow,PROLOG_MODULE));
  Yap_heap_regs->pred_handle_throw = RepPredProp(PredPropByFunc(FunctorHandleThrow,PROLOG_MODULE));
  Yap_heap_regs->pred_is = RepPredProp(PredPropByFunc(FunctorIs,PROLOG_MODULE));

#ifdef LOW_LEVEL_TRACER
  Yap_heap_regs->yap_do_low_level_trace = FALSE;
#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(Yap_heap_regs->low_level_trace_lock);
#endif
#endif

  Yap_heap_regs->clause_space = 0;
  Yap_heap_regs->index_space_Tree = 0;
  Yap_heap_regs->index_space_EXT = 0;
  Yap_heap_regs->index_space_SW = 0;
  Yap_heap_regs->lu_clause_space = 0;
  Yap_heap_regs->lu_index_space_Tree = 0;
  Yap_heap_regs->lu_index_space_CP = 0;
  Yap_heap_regs->lu_index_space_EXT = 0;
  Yap_heap_regs->lu_index_space_SW = 0;


  Yap_heap_regs->dummycode->opc = Yap_opcode(_op_fail);
  Yap_heap_regs->failcode->opc = Yap_opcode(_op_fail);
  Yap_heap_regs->nocode->opc = Yap_opcode(_Nstop);
  InitEnvInst(ENV_FOR_TRUSTFAIL,&TRUSTFAILCODE,_trust_fail,PredFail);

  InitEnvInst(ENV_FOR_YESCODE,&YESCODE,_Ystop,PredFail);

  InitOtaplInst(RTRYCODE,_retry_and_mark);
#ifdef BEAM
  Yap_heap_regs->beam_retry_code->opc = Yap_opcode(_beam_retry_code);
#endif /* BEAM */
#ifdef YAPOR
  Yap_heap_regs->seq_def = TRUE;
  InitOtaplInst(GETWORK,_getwork);
  InitOtaplInst(GETWORK_SEQ,_getwork_seq);
  Yap_heap_regs->getwork_first_time->opc = Yap_opcode(_getwork_first_time);
#endif /* YAPOR */
#ifdef TABLING
  InitOtaplInst(LOAD_ANSWER,_table_load_answer);
  InitOtaplInst(TRY_ANSWER,_table_try_answer);
  InitOtaplInst(ANSWER_RESOLUTION,_answer_resolution);
  InitOtaplInst(COMPLETION,_table_completion);
#endif /* TABLING */




  Yap_heap_regs->debugger_p_before_spy = NULL;

  Yap_heap_regs->retry_recordedp_code = NULL;
  Yap_heap_regs->retry_recorded_k_code = NULL;

  Yap_heap_regs->system_profiling = FALSE;
  Yap_heap_regs->system_call_counting = FALSE;
  Yap_heap_regs->system_pred_goal_expansion_all = FALSE;
  Yap_heap_regs->system_pred_goal_expansion_func = FALSE;
  Yap_heap_regs->system_pred_goal_expansion_on = FALSE;
  Yap_heap_regs->compiler_optimizer_on = TRUE;
  Yap_heap_regs->compiler_compile_mode = 0;
  Yap_heap_regs->compiler_profiling = FALSE;
  Yap_heap_regs->compiler_call_counting = FALSE;

  Yap_heap_regs->compiler_compile_arrays = FALSE;

#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(Yap_heap_regs->dbterms_list_lock);
#endif
  Yap_heap_regs->dbterms_list = NULL;

  Yap_heap_regs->expand_clauses_first = NULL;
  Yap_heap_regs->expand_clauses_last = NULL;
  Yap_heap_regs->expand_clauses = 0;
#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(Yap_heap_regs->expand_clauses_list_lock);
  INIT_LOCK(Yap_heap_regs->op_list_lock);
#endif

#ifdef DEBUG
  Yap_heap_regs->new_cps = 0L;
  Yap_heap_regs->live_cps = 0L;
  Yap_heap_regs->dirty_cps = 0L;
  Yap_heap_regs->freed_cps = 0L;
  Yap_heap_regs->expand_clauses_sz = 0L;
#endif
