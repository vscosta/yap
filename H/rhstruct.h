
  /* This file, rhstruct.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/HEAPFIELDS instead */






























#if USE_DL_MALLOC


#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(Yap_heap_regs->dlmalloc_lock);
#endif
#endif
#if USE_DL_MALLOC || (USE_SYSTEM_MALLOC && HAVE_MALLINFO)
#ifndef  HeapUsed
#define  HeapUsed  Yap_givemallinfo()		
#endif

#else

#endif




#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(Yap_heap_regs->free_blocks_lock);
  REINIT_LOCK(Yap_heap_regs->heap_used_lock);
  REINIT_LOCK(Yap_heap_regs->heap_top_lock);

#endif


#if USE_THREADED_CODE
  Yap_heap_regs->op_rtable = OpRTableAdjust(Yap_heap_regs->op_rtable);
#endif

  Yap_heap_regs->execute_cpred_op_code = Yap_opcode(_execute_cpred);
  Yap_heap_regs->expand_op_code = Yap_opcode(_expand_index);
  Yap_heap_regs->fail_op = Yap_opcode(_op_fail);
  Yap_heap_regs->index_op = Yap_opcode(_index_pred);
  Yap_heap_regs->lockpred_op = Yap_opcode(_lock_pred);
  Yap_heap_regs->orlast_op = Yap_opcode(_or_last);
  Yap_heap_regs->undef_op = Yap_opcode(_undef_p);





  RestoreInvisibleAtoms();
  RestoreWideAtoms();
  RestoreAtoms();

#include "ratoms.h"
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
  Yap_heap_regs->operating_system_module = AtomTermAdjust(Yap_heap_regs->operating_system_module);
  Yap_heap_regs->readutil_module = AtomTermAdjust(Yap_heap_regs->readutil_module);
  Yap_heap_regs->hacks_module = AtomTermAdjust(Yap_heap_regs->hacks_module);
  Yap_heap_regs->arg_module = AtomTermAdjust(Yap_heap_regs->arg_module);
  Yap_heap_regs->globals_module = AtomTermAdjust(Yap_heap_regs->globals_module);
  Yap_heap_regs->swi_module = AtomTermAdjust(Yap_heap_regs->swi_module);



  Yap_heap_regs->current_modules = ModEntryPtrAdjust(Yap_heap_regs->current_modules);






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
  Yap_heap_regs->pred_safe_call_cleanup = PtoPredAdjust(Yap_heap_regs->pred_safe_call_cleanup);
  Yap_heap_regs->pred_restore_regs = PtoPredAdjust(Yap_heap_regs->pred_restore_regs);
#ifdef YAPOR
  Yap_heap_regs->pred_getwork = PtoPredAdjust(Yap_heap_regs->pred_getwork);
  Yap_heap_regs->pred_getwork_seq = PtoPredAdjust(Yap_heap_regs->pred_getwork_seq);
#endif /* YAPOR */

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

  RestoreOtaplInst(RTRYCODE,_retry_and_mark,PredFail);
#ifdef BEAM
  Yap_heap_regs->beam_retry_code->opc = Yap_opcode(_beam_retry_code);
#endif /* BEAM */
#ifdef YAPOR

  RestoreOtaplInst(GETWORK,_getwork,PredGetwork);
  RestoreOtaplInst(GETWORK_SEQ,_getwork_seq,PredGetworkSeq);
  Yap_heap_regs->getwork_first_time->opc = Yap_opcode(_getwork_first_time);
#endif /* YAPOR */
#ifdef TABLING
  RestoreOtaplInst(LOAD_ANSWER,_table_load_answer,PredFail);
  RestoreOtaplInst(TRY_ANSWER,_table_try_answer,PredFail);
  RestoreOtaplInst(ANSWER_RESOLUTION,_table_answer_resolution,PredFail);
  RestoreOtaplInst(COMPLETION,_table_completion,PredFail);
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


  RestoreUdiControlBlocks();




  RestoreIntKeys();
  RestoreIntLUKeys();
  RestoreIntBBKeys();







  RestoreDBErasedMarker();
  RestoreLogDBErasedMarker();

  RestoreDeadStaticClauses();
  RestoreDeadMegaClauses();
  RestoreDeadStaticIndices();
  RestoreDBErasedList();
  RestoreDBErasedIList();
#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(Yap_heap_regs->dead_static_clauses_lock);
  REINIT_LOCK(Yap_heap_regs->dead_mega_clauses_lock);
  REINIT_LOCK(Yap_heap_regs->dead_static_indices_lock);
#endif
#ifdef COROUTINING




#endif



  Yap_heap_regs->op_list = OpListAdjust(Yap_heap_regs->op_list);


  RestoreStreams();



  RestoreAliases();

  Yap_heap_regs->atprompt = AtomAdjust(Yap_heap_regs->atprompt);


  Yap_heap_regs->char_conversion_table = CodeCharPAdjust(Yap_heap_regs->char_conversion_table);
  Yap_heap_regs->char_conversion_table2 = CodeCharPAdjust(Yap_heap_regs->char_conversion_table2);







  Yap_heap_regs->yap_lib_dir = CodeCharPAdjust(Yap_heap_regs->yap_lib_dir);

  Yap_heap_regs->last_wtime = CodeVoidPAdjust(Yap_heap_regs->last_wtime);


#if LOW_PROF





#endif /* LOW_PROF */

  RestoreForeignCode();




  RestoreYapRecords();

  RestoreSWIAtoms();



  RestoreSWIBlobs();
