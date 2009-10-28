

























#ifdef EUROTRA
  Yap_heap_regs->term_dollar_u = AtomTermAdjust(Yap_heap_regs->term_dollar_u);
#endif
  Yap_heap_regs->term_prolog = AtomTermAdjust(Yap_heap_regs->term_prolog);
  Yap_heap_regs->term_refound_var = AtomTermAdjust(Yap_heap_regs->term_refound_var);
  Yap_heap_regs->user_module = AtomTermAdjust(Yap_heap_regs->user_module);
  Yap_heap_regs->idb_module = AtomTermAdjust(Yap_heap_regs->idb_module);
  Yap_heap_regs->attributes_module = AtomTermAdjust(Yap_heap_regs->attributes_module);
  Yap_heap_regs->charsio_module = AtomTermAdjust(Yap_heap_regs->charsio_module);
  Yap_heap_regs->terms_module = AtomTermAdjust(Yap_heap_regs->terms_module);
  Yap_heap_regs->system_module = AtomTermAdjust(Yap_heap_regs->system_module);
  Yap_heap_regs->readutil_module = AtomTermAdjust(Yap_heap_regs->readutil_module);
  Yap_heap_regs->hacks_module = AtomTermAdjust(Yap_heap_regs->hacks_module);
  Yap_heap_regs->arg_module = AtomTermAdjust(Yap_heap_regs->arg_module);
  Yap_heap_regs->globals_module = AtomTermAdjust(Yap_heap_regs->globals_module);
  Yap_heap_regs->swi_module = AtomTermAdjust(Yap_heap_regs->swi_module);



  Yap_heap_regs->current_modules = ModEntryPtrAdjust(Yap_heap_regs->current_modules);





#if USE_THREADED_CODE
  Yap_heap_regs->op_rtable = OpRTableAdjust(Yap_heap_regs->op_rtable);
#endif



  Yap_heap_regs->execute_cpred_op_code = Yap_opcode(_execute_cpred);
  Yap_heap_regs->expand_op_code = Yap_opcode(_expand_index);
  Yap_heap_regs->fail_op = Yap_opcode(_op_fail);
  Yap_heap_regs->index_op = Yap_opcode(_index_pred);
  Yap_heap_regs->lockpred_op = Yap_opcode(_lock_pred);
  Yap_heap_regs->undef_op = Yap_opcode(_undef_p);

  RestorePredHash();
#if defined(YAPOR) || defined(THREADS)

#endif



  Yap_heap_regs->creep_code = PtoPredAdjust(Yap_heap_regs->creep_code);
  Yap_heap_regs->undef_code = PtoPredAdjust(Yap_heap_regs->undef_code);
  Yap_heap_regs->spy_code = PtoPredAdjust(Yap_heap_regs->spy_code);
  Yap_heap_regs->pred_fail = PtoPredAdjust(Yap_heap_regs->pred_fail);
  Yap_heap_regs->pred_true = PtoPredAdjust(Yap_heap_regs->pred_true);
#ifdef COROUTINING

  Yap_heap_regs->wake_up_code = PtoPredAdjust(Yap_heap_regs->wake_up_code);
#endif
  Yap_heap_regs->pred_goal_expansion = PtoPredAdjust(Yap_heap_regs->pred_goal_expansion);
  Yap_heap_regs->pred_meta_call = PtoPredAdjust(Yap_heap_regs->pred_meta_call);
  Yap_heap_regs->pred_dollar_catch = PtoPredAdjust(Yap_heap_regs->pred_dollar_catch);
  Yap_heap_regs->pred_recorded_with_key = PtoPredAdjust(Yap_heap_regs->pred_recorded_with_key);
  Yap_heap_regs->pred_log_upd_clause = PtoPredAdjust(Yap_heap_regs->pred_log_upd_clause);
  Yap_heap_regs->pred_log_upd_clause_erase = PtoPredAdjust(Yap_heap_regs->pred_log_upd_clause_erase);
  Yap_heap_regs->pred_log_upd_clause0 = PtoPredAdjust(Yap_heap_regs->pred_log_upd_clause0);
  Yap_heap_regs->pred_static_clause = PtoPredAdjust(Yap_heap_regs->pred_static_clause);
  Yap_heap_regs->pred_throw = PtoPredAdjust(Yap_heap_regs->pred_throw);
  Yap_heap_regs->pred_handle_throw = PtoPredAdjust(Yap_heap_regs->pred_handle_throw);
  Yap_heap_regs->pred_is = PtoPredAdjust(Yap_heap_regs->pred_is);

#ifdef LOW_LEVEL_TRACER

#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(Yap_heap_regs->low_level_trace_lock);
#endif
#endif












  Yap_heap_regs->dummycode->opc = Yap_opcode(_op_fail);
  Yap_heap_regs->failcode->opc = Yap_opcode(_op_fail);
  Yap_heap_regs->nocode->opc = Yap_opcode(_Nstop);
  RestoreEnvInst(ENV_FOR_TRUSTFAIL,&TRUSTFAILCODE,_trust_fail,PredFail);

  RestoreEnvInst(ENV_FOR_YESCODE,&YESCODE,_Ystop,PredFail);

  RestoreOtaplInst(RTRYCODE,_retry_and_mark);
#ifdef BEAM
  Yap_heap_regs->beam_retry_code->opc = Yap_opcode(_beam_retry_code);
#endif /* BEAM */
#ifdef YAPOR

  RestoreOtaplInst(GETWORK,_getwork);
  RestoreOtaplInst(GETWORK_SEQ,_getwork_seq);
  Yap_heap_regs->getwork_first_time->opc = Yap_opcode(_getwork_first_time);
#endif /* YAPOR */
#ifdef TABLING
  RestoreOtaplInst(LOAD_ANSWER,_table_load_answer);
  RestoreOtaplInst(TRY_ANSWER,_table_try_answer);
  RestoreOtaplInst(ANSWER_RESOLUTION,_answer_resolution_seq);
  RestoreOtaplInst(COMPLETION,_table_completion);
#endif /* TABLING */




  Yap_heap_regs->debugger_p_before_spy = PtoOpAdjust(Yap_heap_regs->debugger_p_before_spy);

  Yap_heap_regs->retry_recordedp_code = PtoOpAdjust(Yap_heap_regs->retry_recordedp_code);
  Yap_heap_regs->retry_recorded_k_code = PtoOpAdjust(Yap_heap_regs->retry_recorded_k_code);













#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(Yap_heap_regs->dbterms_list_lock);
#endif
  RestoreDBTermsList();


  RestoreExpandList();

#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(Yap_heap_regs->expand_clauses_list_lock);
  REINIT_LOCK(Yap_heap_regs->op_list_lock);
#endif

#ifdef DEBUG





#endif
