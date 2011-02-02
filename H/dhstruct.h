
  /* This file, dhstruct.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/HEAPFIELDS instead */




























#define Yap_HoleSize Yap_heap_regs->hole_size
#define Yap_av Yap_heap_regs->av_
#if USE_DL_MALLOC
#define Yap_MemoryHoles Yap_heap_regs->memory_holes
#define Yap_NOfMemoryHoles Yap_heap_regs->nof_memory_holes
#if defined(YAPOR) || defined(THREADS)
#define DLMallocLock Yap_heap_regs->dlmalloc_lock
#endif
#endif
#if USE_DL_MALLOC || (USE_SYSTEM_MALLOC && HAVE_MALLINFO)
#ifndef  HeapUsed
#define  HeapUsed  Yap_givemallinfo()		
#endif
#define NotHeapUsed Yap_heap_regs->heap_used
#else
#define HeapUsed Yap_heap_regs->heap_used
#endif
#define HeapMax Yap_heap_regs->heap_max
#define HeapTop Yap_heap_regs->heap_top
#define HeapLim Yap_heap_regs->heap_lim
#define FreeBlocks Yap_heap_regs->free_blocks
#if defined(YAPOR) || defined(THREADS)
#define FreeBlocksLock Yap_heap_regs->free_blocks_lock
#define HeapUsedLock Yap_heap_regs->heap_used_lock
#define HeapTopLock Yap_heap_regs->heap_top_lock
#define HeapTopOwner Yap_heap_regs->heap_top_owner
#endif


#if USE_THREADED_CODE
#define OP_RTABLE Yap_heap_regs->op_rtable
#endif

#define EXECUTE_CPRED_OP_CODE Yap_heap_regs->execute_cpred_op_code
#define EXPAND_OP_CODE Yap_heap_regs->expand_op_code
#define FAIL_OPCODE Yap_heap_regs->fail_op
#define INDEX_OPCODE Yap_heap_regs->index_op
#define LOCKPRED_OPCODE Yap_heap_regs->lockpred_op
#define ORLAST_OPCODE Yap_heap_regs->orlast_op
#define UNDEF_OPCODE Yap_heap_regs->undef_op

#define NOfAtoms Yap_heap_regs->n_of_atoms
#define AtomHashTableSize Yap_heap_regs->atom_hash_table_size
#define WideAtomHashTableSize Yap_heap_regs->wide_atom_hash_table_size
#define NOfWideAtoms Yap_heap_regs->n_of_wide_atoms
#define INVISIBLECHAIN Yap_heap_regs->invisiblechain
#define WideHashChain Yap_heap_regs->wide_hash_chain
#define HashChain Yap_heap_regs->hash_chain


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
#define OPERATING_SYSTEM_MODULE Yap_heap_regs->operating_system_module
#define READUTIL_MODULE Yap_heap_regs->readutil_module
#define HACKS_MODULE Yap_heap_regs->hacks_module
#define ARG_MODULE Yap_heap_regs->arg_module
#define GLOBALS_MODULE Yap_heap_regs->globals_module
#define SWI_MODULE Yap_heap_regs->swi_module



#define CurrentModules Yap_heap_regs->current_modules




#define Yap_ExecutionMode Yap_heap_regs->execution_mode

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
#define PredSafeCallCleanup Yap_heap_regs->pred_safe_call_cleanup
#define PredRestoreRegs Yap_heap_regs->pred_restore_regs
#ifdef YAPOR
#define PredGetwork Yap_heap_regs->pred_getwork
#define PredGetworkSeq Yap_heap_regs->pred_getwork_seq
#endif /* YAPOR */

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
#endif
#define Yap_expand_clauses_sz Yap_heap_regs->expand_clauses_sz

#define UdiControlBlocks Yap_heap_regs->udi_control_blocks


#define STATIC_PREDICATES_MARKED Yap_heap_regs->static_predicates_marked

#define INT_KEYS Yap_heap_regs->IntKeys
#define INT_LU_KEYS Yap_heap_regs->IntLUKeys
#define INT_BB_KEYS Yap_heap_regs->IntBBKeys

#define INT_KEYS_SIZE Yap_heap_regs->int_keys_size
#define INT_KEYS_TIMESTAMP Yap_heap_regs->int_keys_timestamp
#define INT_BB_KEYS_SIZE Yap_heap_regs->int_bb_keys_size

#define UPDATE_MODE Yap_heap_regs->update_mode

#define DBErasedMarker Yap_heap_regs->db_erased_marker
#define LogDBErasedMarker Yap_heap_regs->logdb_erased_marker

#define DeadStaticClauses Yap_heap_regs->dead_static_clauses
#define DeadMegaClauses Yap_heap_regs->dead_mega_clauses
#define DeadStaticIndices Yap_heap_regs->dead_static_indices
#define DBErasedList Yap_heap_regs->db_erased_list
#define DBErasedIList Yap_heap_regs->db_erased_ilist
#if defined(YAPOR) || defined(THREADS)
#define DeadStaticClausesLock Yap_heap_regs->dead_static_clauses_lock
#define DeadMegaClausesLock Yap_heap_regs->dead_mega_clauses_lock
#define DeadStaticIndicesLock Yap_heap_regs->dead_static_indices_lock
#endif
#ifdef COROUTINING

#define NUM_OF_ATTS Yap_heap_regs->num_of_atts

#define Yap_AttsSize Yap_heap_regs->atts_size
#endif

#define yap_flags Yap_heap_regs->yap_flags_field

#define OpList Yap_heap_regs->op_list


#define Stream Yap_heap_regs->yap_streams

#define NOfFileAliases Yap_heap_regs->n_of_file_aliases
#define SzOfFileAliases Yap_heap_regs->sz_of_file_aliases
#define FileAliases Yap_heap_regs->file_aliases

#define AtPrompt Yap_heap_regs->atprompt
#define Prompt Yap_heap_regs->prompt

#define CharConversionTable Yap_heap_regs->char_conversion_table
#define CharConversionTable2 Yap_heap_regs->char_conversion_table2

#define max_depth Yap_heap_regs->maxdepth
#define max_list Yap_heap_regs->axlist
#define max_write_args Yap_heap_regs->maxwriteargs

#define ParserErrorStyle Yap_heap_regs->parser_error_style

#define Yap_LibDir Yap_heap_regs->yap_lib_dir

#define LastWtimePtr Yap_heap_regs->last_wtime

#define output_msg Yap_heap_regs->debugger_output_msg
#if LOW_PROF
#define ProfilerOn Yap_heap_regs->profiler_on
#define Yap_OffLineProfiler Yap_heap_regs->offline_profiler
#define FProf Yap_heap_regs->f_prof
#define FPreds Yap_heap_regs->f_preds
#define ProfPreds Yap_heap_regs->prof_preds
#endif /* LOW_PROF */

#define ForeignCodeLoaded Yap_heap_regs->foreign_code_loaded
#define ForeignCodeBase Yap_heap_regs->foreign_code_base
#define ForeignCodeTop Yap_heap_regs->foreign_code_top
#define ForeignCodeMax Yap_heap_regs->foreign_code_max

#define Yap_Records Yap_heap_regs->yap_records

#define SWI_Atoms Yap_heap_regs->swi_atoms
#define SWI_Functors Yap_heap_regs->swi_functors
#define SWI_ReverseHash Yap_heap_regs->swi_reverse_hash

#define SWI_Blobs Yap_heap_regs->swi_blobs
