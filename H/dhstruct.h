

























#ifdef EUROTRA
#define TermDollarU Yap_heap_regs->term_dollar_u
#endif
#define TermProlog Yap_heap_regs->term_prolog
#define TermReFoundVar Yap_heap_regs->term_refound_var
#define USER_MODULE Yap_heap_regs->user_module
#define IDB_MODULE Yap_heap_regs->idb_module
#define ATTRIBUTES_MODULE Yap_heap_regs->attributes_module
#define CHARSIO_MODULE Yap_heap_regs->charsio_module
#define TERMS_MODULE Yap_heap_regs->terms_module
#define SYSTEM_MODULE Yap_heap_regs->system_module
#define READUTIL_MODULE Yap_heap_regs->readutil_module
#define HACKS_MODULE Yap_heap_regs->hacks_module
#define ARG_MODULE Yap_heap_regs->arg_module
#define GLOBALS_MODULE Yap_heap_regs->globals_module
#define SWI_MODULE Yap_heap_regs->swi_module



#define CurrentModules Yap_heap_regs->current_modules





#if USE_THREADED_CODE
#define OP_RTABLE Yap_heap_regs->op_rtable
#endif

#define Yap_ExecutionMode Yap_heap_regs->execution_mode

#define EXECUTE_CPRED_OP_CODE Yap_heap_regs->execute_cpred_op_code
#define EXPAND_OP_CODE Yap_heap_regs->expand_op_code
#define FAIL_OPCODE Yap_heap_regs->fail_op
#define INDEX_OPCODE Yap_heap_regs->index_op
#define LOCKPRED_OPCODE Yap_heap_regs->lockpred_op
#define UNDEF_OPCODE Yap_heap_regs->undef_op

#define PredHash Yap_heap_regs->pred_hash
#if defined(YAPOR) || defined(THREADS)
#define PredHashRWLock Yap_heap_regs->pred_hash_rw_lock
#endif
#define PredsInHashTable Yap_heap_regs->preds_in_hash_table
#define PredHashTableSize Yap_heap_regs->pred_hash_table_size

#define CreepCode Yap_heap_regs->creep_code
#define UndefCode Yap_heap_regs->undef_code
#define SpyCode Yap_heap_regs->spy_code
#define PredFail Yap_heap_regs->pred_fail
#define PredTrue Yap_heap_regs->pred_true
#ifdef COROUTINING
#define NUM_OF_ATTS Yap_heap_regs->num_of_atts
#define WakeUpCode Yap_heap_regs->wake_up_code
#endif
#define PredGoalExpansion Yap_heap_regs->pred_goal_expansion
#define PredMetaCall Yap_heap_regs->pred_meta_call
#define PredDollarCatch Yap_heap_regs->pred_dollar_catch
#define PredRecordedWithKey Yap_heap_regs->pred_recorded_with_key
#define PredLogUpdClause Yap_heap_regs->pred_log_upd_clause
#define PredLogUpdClauseErase Yap_heap_regs->pred_log_upd_clause_erase
#define PredLogUpdClause0 Yap_heap_regs->pred_log_upd_clause0
#define PredStaticClause Yap_heap_regs->pred_static_clause
#define PredThrow Yap_heap_regs->pred_throw
#define PredHandleThrow Yap_heap_regs->pred_handle_throw
#define PredIs Yap_heap_regs->pred_is

#ifdef LOW_LEVEL_TRACER
#define Yap_do_low_level_trace Yap_heap_regs->yap_do_low_level_trace
#if defined(YAPOR) || defined(THREADS)
#define Yap_low_level_trace_lock Yap_heap_regs->low_level_trace_lock
#endif
#endif

#define Yap_ClauseSpace Yap_heap_regs->clause_space
#define Yap_IndexSpace_Tree Yap_heap_regs->index_space_Tree
#define Yap_IndexSpace_EXT Yap_heap_regs->index_space_EXT
#define Yap_IndexSpace_SW Yap_heap_regs->index_space_SW
#define Yap_LUClauseSpace Yap_heap_regs->lu_clause_space
#define Yap_LUIndexSpace_Tree Yap_heap_regs->lu_index_space_Tree
#define Yap_LUIndexSpace_CP Yap_heap_regs->lu_index_space_CP
#define Yap_LUIndexSpace_EXT Yap_heap_regs->lu_index_space_EXT
#define Yap_LUIndexSpace_SW Yap_heap_regs->lu_index_space_SW

#define COMMA_CODE Yap_heap_regs->comma_code
#define DUMMYCODE Yap_heap_regs->dummycode
#define FAILCODE Yap_heap_regs->failcode
#define NOCODE Yap_heap_regs->nocode
#define ENV_FOR_TRUSTFAIL Yap_heap_regs->env_for_trustfail
#define TRUSTFAILCODE Yap_heap_regs->trustfailcode
#define ENV_FOR_YESCODE Yap_heap_regs->env_for_yescode
#define YESCODE Yap_heap_regs->yescode
#define RTRYCODE Yap_heap_regs->rtrycode
#ifdef BEAM
#define BEAM_RETRY_CODE Yap_heap_regs->beam_retry_code
#endif /* BEAM */
#ifdef YAPOR
#define SEQUENTIAL_IS_DEFAULT Yap_heap_regs->seq_def
#define GETWORK Yap_heap_regs->getwork_code
#define GETWORK_SEQ Yap_heap_regs->getwork_seq_code
#define GETWORK_FIRST_TIME Yap_heap_regs->getwork_first_time
#endif /* YAPOR */
#ifdef TABLING
#define LOAD_ANSWER Yap_heap_regs->table_load_answer_code
#define TRY_ANSWER Yap_heap_regs->table_try_answer_code
#define ANSWER_RESOLUTION Yap_heap_regs->table_answer_resolution_code
#define COMPLETION Yap_heap_regs->table_completion_code
#endif /* TABLING */




#define P_before_spy Yap_heap_regs->debugger_p_before_spy

#define RETRY_C_RECORDEDP_CODE Yap_heap_regs->retry_recordedp_code
#define RETRY_C_RECORDED_K_CODE Yap_heap_regs->retry_recorded_k_code

#define PROFILING Yap_heap_regs->system_profiling
#define CALL_COUNTING Yap_heap_regs->system_call_counting
#define PRED_GOAL_EXPANSION_ALL Yap_heap_regs->system_pred_goal_expansion_all
#define PRED_GOAL_EXPANSION_FUNC Yap_heap_regs->system_pred_goal_expansion_func
#define PRED_GOAL_EXPANSION_ON Yap_heap_regs->system_pred_goal_expansion_on
#define optimizer_on Yap_heap_regs->compiler_optimizer_on
#define compile_mode Yap_heap_regs->compiler_compile_mode
#define profiling Yap_heap_regs->compiler_profiling
#define call_counting Yap_heap_regs->compiler_call_counting

#define compile_arrays Yap_heap_regs->compiler_compile_arrays

#if defined(YAPOR) || defined(THREADS)
#define DBTermsListLock Yap_heap_regs->dbterms_list_lock
#endif
#define DBTermsList Yap_heap_regs->dbterms_list

#define ExpandClausesFirst Yap_heap_regs->expand_clauses_first
#define ExpandClausesLast Yap_heap_regs->expand_clauses_last
#define Yap_ExpandClauses Yap_heap_regs->expand_clauses
#if defined(YAPOR) || defined(THREADS)
#define ExpandClausesListLock Yap_heap_regs->expand_clauses_list_lock
#define OpListLock Yap_heap_regs->op_list_lock
#endif

#ifdef DEBUG
#define Yap_NewCps Yap_heap_regs->new_cps
#define Yap_LiveCps Yap_heap_regs->live_cps
#define Yap_DirtyCps Yap_heap_regs->dirty_cps
#define Yap_FreedCps Yap_heap_regs->freed_cps
#define Yap_expand_clauses_sz Yap_heap_regs->expand_clauses_sz
#endif
