
  /* This file, dhstruct.h, was generated automatically by "yap -L misc/buildlocalglobal"
     please do not update, update H/HEAPFIELDS instead */





























#if USE_DL_MALLOC
#if defined(YAPOR) || defined(THREADS)
#define DLMallocLock Yap_heap_regs->DLMallocLock_
#endif
#endif
#if USE_DL_MALLOC || (USE_SYSTEM_MALLOC && HAVE_MALLINFO)
#ifndef  HeapUsed
#define  HeapUsed  Yap_givemallinfo()
#endif
#define NotHeapUsed Yap_heap_regs->NotHeapUsed_
#else
#define HeapUsed Yap_heap_regs->HeapUsed_
#endif
#define HeapMax Yap_heap_regs->HeapMax_
#define HeapTop Yap_heap_regs->HeapTop_
#define HeapLim Yap_heap_regs->HeapLim_
#define FreeBlocks Yap_heap_regs->FreeBlocks_
#if defined(YAPOR) || defined(THREADS)
#define FreeBlocksLock Yap_heap_regs->FreeBlocksLock_
#define HeapUsedLock Yap_heap_regs->HeapUsedLock_
#define HeapTopLock Yap_heap_regs->HeapTopLock_
#define HeapTopOwner Yap_heap_regs->HeapTopOwner_
#endif
#define MaxStack Yap_heap_regs->MaxStack_
#define MaxTrail Yap_heap_regs->MaxTrail_


#if USE_THREADED_CODE
#define OP_RTABLE Yap_heap_regs->OP_RTABLE_
#endif

#define EXECUTE_CPRED_OP_CODE Yap_heap_regs->EXECUTE_CPRED_OP_CODE_
#define EXPAND_OP_CODE Yap_heap_regs->EXPAND_OP_CODE_
#define FAIL_OPCODE Yap_heap_regs->FAIL_OPCODE_
#define INDEX_OPCODE Yap_heap_regs->INDEX_OPCODE_
#define LOCKPRED_OPCODE Yap_heap_regs->LOCKPRED_OPCODE_
#define ORLAST_OPCODE Yap_heap_regs->ORLAST_OPCODE_
#define UNDEF_OPCODE Yap_heap_regs->UNDEF_OPCODE_
#define RETRY_USERC_OPCODE Yap_heap_regs->RETRY_USERC_OPCODE_
#define EXECUTE_CPRED_OPCODE Yap_heap_regs->EXECUTE_CPRED_OPCODE_

#define NOfAtoms Yap_heap_regs->NOfAtoms_
#define AtomHashTableSize Yap_heap_regs->AtomHashTableSize_
#define WideAtomHashTableSize Yap_heap_regs->WideAtomHashTableSize_
#define NOfWideAtoms Yap_heap_regs->NOfWideAtoms_
#define INVISIBLECHAIN Yap_heap_regs->INVISIBLECHAIN_
#define WideHashChain Yap_heap_regs->WideHashChain_
#define HashChain Yap_heap_regs->HashChain_


#ifdef EUROTRA
#define TermDollarU Yap_heap_regs->TermDollarU_
#endif

#define USER_MODULE Yap_heap_regs->USER_MODULE_
#define IDB_MODULE Yap_heap_regs->IDB_MODULE_
#define ATTRIBUTES_MODULE Yap_heap_regs->ATTRIBUTES_MODULE_
#define CHARSIO_MODULE Yap_heap_regs->CHARSIO_MODULE_
#define CHTYPE_MODULE Yap_heap_regs->CHTYPE_MODULE_
#define TERMS_MODULE Yap_heap_regs->TERMS_MODULE_
#define SYSTEM_MODULE Yap_heap_regs->SYSTEM_MODULE_
#define READUTIL_MODULE Yap_heap_regs->READUTIL_MODULE_
#define HACKS_MODULE Yap_heap_regs->HACKS_MODULE_
#define ARG_MODULE Yap_heap_regs->ARG_MODULE_
#define GLOBALS_MODULE Yap_heap_regs->GLOBALS_MODULE_
#define SWI_MODULE Yap_heap_regs->SWI_MODULE_
#define DBLOAD_MODULE Yap_heap_regs->DBLOAD_MODULE_
#define RANGE_MODULE Yap_heap_regs->RANGE_MODULE_
#define ERROR_MODULE Yap_heap_regs->ERROR_MODULE_



#define CurrentModules Yap_heap_regs->CurrentModules_




#define HIDDEN_PREDICATES Yap_heap_regs->HIDDEN_PREDICATES_



#define GLOBAL_Flags Yap_heap_regs->GLOBAL_Flags_
#define GLOBAL_flagCount Yap_heap_regs->GLOBAL_flagCount_

#define Yap_ExecutionMode Yap_heap_regs->Yap_ExecutionMode_

#define PredsInHashTable Yap_heap_regs->PredsInHashTable_
#define PredHashTableSize Yap_heap_regs->PredHashTableSize_
#define PredHash Yap_heap_regs->PredHash_
#if defined(YAPOR) || defined(THREADS)
#define PredHashRWLock Yap_heap_regs->PredHashRWLock_
#endif

#define CreepCode Yap_heap_regs->CreepCode_
#define UndefCode Yap_heap_regs->UndefCode_
#define SpyCode Yap_heap_regs->SpyCode_
#define PredFail Yap_heap_regs->PredFail_
#define PredTrue Yap_heap_regs->PredTrue_
#ifdef COROUTINING
#define WakeUpCode Yap_heap_regs->WakeUpCode_
#endif
#define PredDollarCatch Yap_heap_regs->PredDollarCatch_
#ifdef YAPOR
#define PredGetwork Yap_heap_regs->PredGetwork_
#endif /* YAPOR */
#define PredGoalExpansion Yap_heap_regs->PredGoalExpansion_
#define PredHandleThrow Yap_heap_regs->PredHandleThrow_
#define PredIs Yap_heap_regs->PredIs_
#define PredLogUpdClause Yap_heap_regs->PredLogUpdClause_
#define PredLogUpdClauseErase Yap_heap_regs->PredLogUpdClauseErase_
#define PredLogUpdClause0 Yap_heap_regs->PredLogUpdClause0_
#define PredCall Yap_heap_regs->PredCall_
#define PredMetaCall Yap_heap_regs->PredMetaCall_
#define PredProtectStack Yap_heap_regs->PredProtectStack_
#define PredRecordedWithKey Yap_heap_regs->PredRecordedWithKey_
#define PredRestoreRegs Yap_heap_regs->PredRestoreRegs_
#define PredRestoreRegs1 Yap_heap_regs->PredRestoreRegs1_
#define PredSafeCallCleanup Yap_heap_regs->PredSafeCallCleanup_
#define PredStaticClause Yap_heap_regs->PredStaticClause_
#define PredThrow Yap_heap_regs->PredThrow_
#define PredTraceMetaCall Yap_heap_regs->PredTraceMetaCall_
#define PredCommentHook Yap_heap_regs->PredCommentHook_
#define PredProcedure Yap_heap_regs->PredProcedure_
#define PredUndefinedQuery Yap_heap_regs->PredUndefinedQuery_

#ifdef LOW_LEVEL_TRACER
#define Yap_do_low_level_trace Yap_heap_regs->Yap_do_low_level_trace_
#if defined(YAPOR) || defined(THREADS)
#define Yap_low_level_trace_lock Yap_heap_regs->Yap_low_level_trace_lock_
#endif
#endif

#define Yap_ClauseSpace Yap_heap_regs->Yap_ClauseSpace_
#define Yap_IndexSpace_Tree Yap_heap_regs->Yap_IndexSpace_Tree_
#define Yap_IndexSpace_EXT Yap_heap_regs->Yap_IndexSpace_EXT_
#define Yap_IndexSpace_SW Yap_heap_regs->Yap_IndexSpace_SW_
#define Yap_LUClauseSpace Yap_heap_regs->Yap_LUClauseSpace_
#define Yap_LUIndexSpace_Tree Yap_heap_regs->Yap_LUIndexSpace_Tree_
#define Yap_LUIndexSpace_CP Yap_heap_regs->Yap_LUIndexSpace_CP_
#define Yap_LUIndexSpace_EXT Yap_heap_regs->Yap_LUIndexSpace_EXT_
#define Yap_LUIndexSpace_SW Yap_heap_regs->Yap_LUIndexSpace_SW_

#define COMMA_CODE Yap_heap_regs->COMMA_CODE_
#define DUMMYCODE Yap_heap_regs->DUMMYCODE_
#define FAILCODE Yap_heap_regs->FAILCODE_
#define NOCODE Yap_heap_regs->NOCODE_
#define ENV_FOR_TRUSTFAIL Yap_heap_regs->ENV_FOR_TRUSTFAIL_
#define TRUSTFAILCODE Yap_heap_regs->TRUSTFAILCODE_
#define ENV_FOR_YESCODE Yap_heap_regs->ENV_FOR_YESCODE_
#define YESCODE Yap_heap_regs->YESCODE_
#define ENV_FOR_BORDERCODE Yap_heap_regs->ENV_FOR_BORDERCODE_
#define BORDERCODE Yap_heap_regs->BORDERCODE_
#define RTRYCODE Yap_heap_regs->RTRYCODE_
#ifdef BEAM
#define BEAM_RETRY_CODE Yap_heap_regs->BEAM_RETRY_CODE_
#endif /* BEAM */
#ifdef YAPOR
#define GETWORK Yap_heap_regs->GETWORK_
#define GETWORK_SEQ Yap_heap_regs->GETWORK_SEQ_
#define GETWORK_FIRST_TIME Yap_heap_regs->GETWORK_FIRST_TIME_
#endif /* YAPOR */
#ifdef TABLING
#define LOAD_ANSWER Yap_heap_regs->LOAD_ANSWER_
#define TRY_ANSWER Yap_heap_regs->TRY_ANSWER_
#define ANSWER_RESOLUTION Yap_heap_regs->ANSWER_RESOLUTION_
#define COMPLETION Yap_heap_regs->COMPLETION_
#ifdef THREADS_CONSUMER_SHARING
#define ANSWER_RESOLUTION_COMPLETION Yap_heap_regs->ANSWER_RESOLUTION_COMPLETION_
#endif /* THREADS_CONSUMER_SHARING */
#endif /* TABLING */




#define P_before_spy Yap_heap_regs->P_before_spy_

#define RETRY_C_RECORDEDP_CODE Yap_heap_regs->RETRY_C_RECORDEDP_CODE_
#define RETRY_C_RECORDED_K_CODE Yap_heap_regs->RETRY_C_RECORDED_K_CODE_

#define PROFILING Yap_heap_regs->PROFILING_
#define CALL_COUNTING Yap_heap_regs->CALL_COUNTING_
#define optimizer_on Yap_heap_regs->optimizer_on_
#define compile_mode Yap_heap_regs->compile_mode_
#define profiling Yap_heap_regs->profiling_
#define call_counting Yap_heap_regs->call_counting_

#define compile_arrays Yap_heap_regs->compile_arrays_

#if defined(YAPOR) || defined(THREADS)
#define DBTermsListLock Yap_heap_regs->DBTermsListLock_
#endif
#define DBTermsList Yap_heap_regs->DBTermsList_

#define ExpandClausesFirst Yap_heap_regs->ExpandClausesFirst_
#define ExpandClausesLast Yap_heap_regs->ExpandClausesLast_
#define Yap_ExpandClauses Yap_heap_regs->Yap_ExpandClauses_
#if defined(YAPOR) || defined(THREADS)
#define ExpandClausesListLock Yap_heap_regs->ExpandClausesListLock_
#define OpListLock Yap_heap_regs->OpListLock_
#endif

#ifdef DEBUG
#define Yap_NewCps Yap_heap_regs->Yap_NewCps_
#define Yap_LiveCps Yap_heap_regs->Yap_LiveCps_
#define Yap_DirtyCps Yap_heap_regs->Yap_DirtyCps_
#define Yap_FreedCps Yap_heap_regs->Yap_FreedCps_
#endif
#define Yap_expand_clauses_sz Yap_heap_regs->Yap_expand_clauses_sz_

#define UdiControlBlocks Yap_heap_regs->UdiControlBlocks_


#define STATIC_PREDICATES_MARKED Yap_heap_regs->STATIC_PREDICATES_MARKED_

#define INT_KEYS Yap_heap_regs->INT_KEYS_
#define INT_LU_KEYS Yap_heap_regs->INT_LU_KEYS_
#define INT_BB_KEYS Yap_heap_regs->INT_BB_KEYS_

#define INT_KEYS_SIZE Yap_heap_regs->INT_KEYS_SIZE_
#define INT_KEYS_TIMESTAMP Yap_heap_regs->INT_KEYS_TIMESTAMP_
#define INT_BB_KEYS_SIZE Yap_heap_regs->INT_BB_KEYS_SIZE_

#define UPDATE_MODE Yap_heap_regs->UPDATE_MODE_

#define DBErasedMarker Yap_heap_regs->DBErasedMarker_
#define LogDBErasedMarker Yap_heap_regs->LogDBErasedMarker_

#define DeadStaticClauses Yap_heap_regs->DeadStaticClauses_
#define DeadMegaClauses Yap_heap_regs->DeadMegaClauses_
#define DeadStaticIndices Yap_heap_regs->DeadStaticIndices_
#define DBErasedList Yap_heap_regs->DBErasedList_
#define DBErasedIList Yap_heap_regs->DBErasedIList_
#if defined(YAPOR) || defined(THREADS)
#define DeadStaticClausesLock Yap_heap_regs->DeadStaticClausesLock_
#define DeadMegaClausesLock Yap_heap_regs->DeadMegaClausesLock_
#define DeadStaticIndicesLock Yap_heap_regs->DeadStaticIndicesLock_
#endif
#ifdef COROUTINING

#define NUM_OF_ATTS Yap_heap_regs->NUM_OF_ATTS_

#define Yap_AttsSize Yap_heap_regs->Yap_AttsSize_
#endif

#define setup_call_catcher_cleanup_tag Yap_heap_regs->setup_call_catcher_cleanup_tag_

#define OpList Yap_heap_regs->OpList_

#define ForeignCodeLoaded Yap_heap_regs->ForeignCodeLoaded_
#define ForeignCodeBase Yap_heap_regs->ForeignCodeBase_
#define ForeignCodeTop Yap_heap_regs->ForeignCodeTop_
#define ForeignCodeMax Yap_heap_regs->ForeignCodeMax_

#define Yap_Records Yap_heap_regs->Yap_Records_
#define EmptyWakeups Yap_heap_regs->EmptyWakeups_
#define MaxEmptyWakeups Yap_heap_regs->MaxEmptyWakeups_

#define BlobTypes Yap_heap_regs->BlobTypes_
#define Blobs Yap_heap_regs->Blobs_
#define NOfBlobs Yap_heap_regs->NOfBlobs_
#define NOfBlobsMax Yap_heap_regs->NOfBlobsMax_
#if defined(YAPOR) || defined(THREADS)
#define Blobs_Lock Yap_heap_regs->Blobs_Lock_
#endif

