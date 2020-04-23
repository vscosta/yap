static inline void
linear_sprint_block(YAP_BBs block, char **buf) {
    switch(block) {
        case ENTRY: break;
        case YAAM_DEREF_BODY_D0PT0:
            sprintf(*buf, "YAAM_DEREF_BODY_D0PT0");
            break;
        case YAAM_DEREF_BODY_D0PT1:
            sprintf(*buf, "YAAM_DEREF_BODY_D0PT1");
            break;
        case YAAM_DEREF_BODY_D0S_SREG:
            sprintf(*buf, "YAAM_DEREF_BODY_D0S_SREG");
            break;
        case YAAM_DEREF_BODY_D1PT0:
            sprintf(*buf, "YAAM_DEREF_BODY_D1PT0");
            break;
        case YAAM_DEREF_BODY_D1PT1:
            sprintf(*buf, "YAAM_DEREF_BODY_D1PT1");
            break;
        case YAAM_FAIL:
            sprintf(*buf, "YAAM_FAIL");
            break;
        case YAAM_CHECK_TRAIL_TR:
            sprintf(*buf, "YAAM_CHECK_TRAIL_TR");
            break;
	case YAAM_UNIFYBOUND:
            break;
        case NoStackExecute_Exception:
            sprintf(*buf, "NoStackExecute_Exception");
            break;
        case NoStackDExecute_Exception:
            sprintf(*buf, "NoStackDExecute_Exception");
            break;
        case NoStackCall_Exception:
            sprintf(*buf, "NoStackCall_Exception");
            break;
        case NoStackDeallocate_Exception:
            sprintf(*buf, "NoStackDeallocate_Exception");
            break;
#ifdef COROUTINING
        case NoStackFail_Exception:
            sprintf(*buf, "NoStackFail_Exception");
            break;
#endif
        case NoStackCut_Exception:
            sprintf(*buf, "NoStackCut_Exception");
            break;
        case NoStackCutT_Exception:
            sprintf(*buf, "NoStackCutT_Exception");
            break;
        case NoStackCutE_Exception:
            sprintf(*buf, "NoStackCutE_Exception");
            break;
        case NoStackCommitX_Exception:
            sprintf(*buf, "NoStackCommitX_Exception");
            break;
        case NoStackCommitY_Exception:
            sprintf(*buf, "NoStackCommitY_Exception");
            break;
        case NoStackEither_Exception:
            sprintf(*buf, "NoStackEither_Exception");
            break;
        case NoStackPExecute_Exception:
            sprintf(*buf, "NoStackPExecute_Exception");
            break;
        case NoStackPExecute2_Exception:
            sprintf(*buf, "NoStackPExecute2_Exception");
            break;
        case NoStackPTExecute_Exception:
            sprintf(*buf, "NoStackPTExecute_Exception");
            break;
        case TRY_ME_INSTINIT:
            sprintf(*buf, "TRY_ME_INSTINIT");
            break;
#ifdef YAPOR
        case TRY_ME_YAPOR:
            sprintf(*buf, "TRY_ME_YAPOR");
            break;
#endif
        case TRY_ME_END:
            sprintf(*buf, "TRY_ME_END");
            break;
        case RETRY_ME_INSTINIT:
            sprintf(*buf, "RETRY_ME_INSTINIT");
            break;
#ifdef FROZEN_STACKS
        case RETRY_ME_FROZEN:
            sprintf(*buf, "RETRY_ME_FROZEN");
            break;
#else
        case RETRY_ME_NOFROZEN:
            sprintf(*buf, "RETRY_ME_NOFROZEN");
            break;
#endif
        case RETRY_ME_END:
            sprintf(*buf, "RETRY_ME_END");
            break;
        case TRUST_ME_INSTINIT:
            sprintf(*buf, "TRUST_ME_INSTINIT");
            break;
        case TRUST_ME_IF:
            sprintf(*buf, "TRUST_ME_IF");
            break;
        case TRUST_ME_END:
            sprintf(*buf, "TRUST_ME_END");
            break;
        case ENTER_PROFILING_INSTINIT:
            sprintf(*buf, "ENTER_PROFILING_INSTINIT");
            break;
        case RETRY_PROFILED_INSTINIT:
            sprintf(*buf, "RETRY_PROFILED_INSTINIT");
            break;
        case PROFILED_RETRY_ME_INSTINIT:
            sprintf(*buf, "PROFILED_RETRY_ME_INSTINIT");
            break;
#ifdef FROZEN_STACKS
        case PROFILED_RETRY_ME_FROZEN:
            sprintf(*buf, "PROFILED_RETRY_ME_FROZEN");
            break;
#else
        case PROFILED_RETRY_ME_NOFROZEN:
            sprintf(*buf, "PROFILED_RETRY_ME_NOFROZEN");
            break;
#endif
        case PROFILED_RETRY_ME_END:
            sprintf(*buf, "PROFILED_RETRY_ME_END");
            break;
        case PROFILED_TRUST_ME_INSTINIT:
            sprintf(*buf, "PROFILED_TRUST_ME_INSTINIT");
            break;
		case PROFILED_TRUST_ME_IF:
            sprintf(*buf, "PROFILED_TRUST_ME_IF");
            break;
        case PROFILED_TRUST_ME_END:
            sprintf(*buf, "PROFILED_TRUST_ME_END");
            break;
        case PROFILED_RETRY_LOGICAL_INSTINIT:
            sprintf(*buf, "PROFILED_RETRY_LOGICAL_INSTINIT");
            break;
#ifdef THREADS
        case PROFILED_RETRY_LOGICAL_THREADS:
            sprintf(*buf, "PROFILED_RETRY_LOGICAL_THREADS");
            break;
#endif
        case PROFILED_RETRY_LOGICAL_POST_THREADS:
            sprintf(*buf, "PROFILED_RETRY_LOGICAL_POST_THREADS");
            break;
#ifdef FROZEN_STACKS
        case PROFILED_RETRY_LOGICAL_FROZEN:
            sprintf(*buf, "PROFILED_RETRY_LOGICAL_FROZEN");
            break;
#else
        case PROFILED_RETRY_LOGICAL_NOFROZEN:
            sprintf(*buf, "PROFILED_RETRY_LOGICAL_NOFROZEN");
            break;
#endif
        case PROFILED_RETRY_LOGICAL_END:
            sprintf(*buf, "PROFILED_RETRY_LOGICAL_END");
            break;
        case PROFILED_TRUST_LOGICAL_INSTINIT:
            sprintf(*buf, "PROFILED_TRUST_LOGICAL_INSTINIT");
            break;
        case PROFILED_TRUST_LOGICAL_END:
            sprintf(*buf, "PROFILED_TRUST_LOGICAL_END");
            break;
        case COUNT_CALL_INSTINIT:
            sprintf(*buf, "COUNT_CALL_INSTINIT");
            break;
		case COUNT_CALL_MIDDLE:
            sprintf(*buf, "COUNT_CALL_MIDDLE");
            break;
        case COUNT_CALL_END:
            sprintf(*buf, "COUNT_CALL_END");
            break;
        case COUNT_RETRY_INSTINIT:
            sprintf(*buf, "COUNT_RETRY_INSTINIT");
            break;
        case COUNT_RETRY_MIDDLE:
            sprintf(*buf, "COUNT_RETRY_MIDDLE");
            break;
        case COUNT_RETRY_END:
            sprintf(*buf, "COUNT_RETRY_END");
            break;
        case COUNT_RETRY_ME_INSTINIT:
            sprintf(*buf, "COUNT_RETRY_ME_INSTINIT");
            break;
		case COUNT_RETRY_ME_MIDDLE:
            sprintf(*buf, "COUNT_RETRY_ME_MIDDLE");
            break;
        case COUNT_RETRY_ME_END:
            sprintf(*buf, "COUNT_RETRY_ME_END");
            break;
        case COUNT_TRUST_ME_INSTINIT:
            sprintf(*buf, "COUNT_TRUST_ME_INSTINIT");
            break;
		case COUNT_TRUST_ME_MIDDLE:
            sprintf(*buf, "COUNT_TRUST_ME_MIDDLE");
            break;
        case COUNT_TRUST_ME_END:
            sprintf(*buf, "COUNT_TRUST_ME_END");
            break;
        case COUNT_RETRY_LOGICAL_INSTINIT:
            sprintf(*buf, "COUNT_RETRY_LOGICAL_INSTINIT");
            break;
        case COUNT_RETRY_LOGICAL_END:
            sprintf(*buf, "COUNT_RETRY_LOGICAL_END");
            break;
        case COUNT_TRUST_LOGICAL_INSTINIT:
            sprintf(*buf, "COUNT_TRUST_LOGICAL_INSTINIT");
            break;
        case COUNT_TRUST_LOGICAL_END:
            sprintf(*buf, "COUNT_TRUST_LOGICAL_END");
            break;
        case LOCK_LU_INSTINIT:
            sprintf(*buf, "LOCK_LU_INSTINIT");
            break;
        case LOCK_LU_END:
            sprintf(*buf, "LOCK_LU_END");
            break;
        case UNLOCK_LU_INSTINIT:
            sprintf(*buf, "UNLOCK_LU_INSTINIT");
            break;
#if defined(YAPOR) || defined(THREADS)
        case UNLOCK_LU_YAPOR_THREADS:
            sprintf(*buf, "UNLOCK_LU_YAPOR_THREADS");
            break;
#endif
        case UNLOCK_LU_END:
            sprintf(*buf, "UNLOCK_LU_END");
            break;
        case ALLOC_FOR_LOGICAL_PRED_INSTINIT:
            sprintf(*buf, "ALLOC_FOR_LOGICAL_PRED_INSTINIT");
            break;
#if MULTIPLE_STACKS
        case ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS:
            sprintf(*buf, "ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS");
            break;
#if PARALLEL_YAP
        case ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS_PARALLEL:
            sprintf(*buf, "ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS_PARALLEL");
            break;
#endif
        case ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS_END:
            sprintf(*buf, "ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS_END");
            break;
#else
        case ALLOC_FOR_LOGICAL_PRED_NOMULTIPLE_STACKS_INIT:
            sprintf(*buf, "ALLOC_FOR_LOGICAL_PRED_NOMULTIPLE_STACKS_INIT");
            break;
        case ALLOC_FOR_LOGICAL_PRED_NOMULTIPLE_STACKS_IF:
            sprintf(*buf, "ALLOC_FOR_LOGICAL_PRED_NOMULTIPLE_STACKS_IF");
            break;
#endif
        case ALLOC_FOR_LOGICAL_PRED_END:
            sprintf(*buf, "ALLOC_FOR_LOGICAL_PRED_END");
            break;
        case COPY_IDB_TERM_INSTINIT:
            sprintf(*buf, "COPY_IDB_TERM_INSTINIT");
            break;
        case COPY_IDB_TERM_END:
            sprintf(*buf, "COPY_IDB_TERM_END");
            break;
        case UNIFY_IDB_TERM_INSTINIT:
            sprintf(*buf, "UNIFY_IDB_TERM_INSTINIT");
            break;
        case UNIFY_IDB_TERM_END:
            sprintf(*buf, "UNIFY_IDB_TERM_END");
            break;
        case ENSURE_SPACE_INSTINIT:
            sprintf(*buf, "ENSURE_SPACE_INSTINIT");
            break;
        case ENSURE_SPACE_END:
            sprintf(*buf, "ENSURE_SPACE_END");
            break;
        case SPY_OR_TRYMARK_INSTINIT:
            sprintf(*buf, "SPY_OR_TRYMARK_INSTINIT");
            break;
        case TRY_AND_MARK_INSTINIT:
            sprintf(*buf, "TRY_AND_MARK_INSTINIT");
            break;
#if defined(YAPOR) || defined(THREADS)
#ifdef YAPOR
        case TRY_AND_MARK_YAPOR_THREADS_YAPOR:
            sprintf(*buf, "TRY_AND_MARK_YAPOR_THREADS_YAPOR");
            break;
#endif
        case TRY_AND_MARK_YAPOR_THREADS_NOYAPOR_IF:
            sprintf(*buf, "TRY_AND_MARK_YAPOR_THREADS_NOYAPOR_IF");
            break;
#endif
        case TRY_AND_MARK_NOYAPOR_NOTHREADS:
            sprintf(*buf, "TRY_AND_MARK_NOYAPOR_NOTHREADS");
            break;
#ifdef YAPOR
        case TRY_AND_MARK_SET_LOAD:
            sprintf(*buf, "TRY_AND_MARK_SET_LOAD");
            break;
#endif
        case TRY_AND_MARK_POST_SET_LOAD:
            sprintf(*buf, "TRY_AND_MARK_POST_SET_LOAD");
            break;
#if MULTIPLE_STACKS
        case TRY_AND_MARK_MULTIPLE_STACKS:
            sprintf(*buf, "TRY_AND_MARK_MULTIPLE_STACKS");
            break;
#else
        case TRY_AND_MARK_NOMULTIPLE_STACKS_IF:
            sprintf(*buf, "TRY_AND_MARK_NOMULTIPLE_STACKS_IF");
            break;
#endif
        case TRY_AND_MARK_END:
            sprintf(*buf, "TRY_AND_MARK_END");
            break;
        case COUNT_RETRY_AND_MARK_INSTINIT:
            sprintf(*buf, "COUNT_RETRY_AND_MARK_INSTINIT");
            break;
        case PROFILED_RETRY_AND_MARK_INSTINIT:
            sprintf(*buf, "PROFILED_RETRY_AND_MARK_INSTINIT");
            break;
        case RETRY_AND_MARK_INSTINIT:
            sprintf(*buf, "RETRY_AND_MARK_INSTINIT");
            break;
#ifdef YAPOR
        case RETRY_AND_MARK_YAPOR:
            sprintf(*buf, "RETRY_AND_MARK_YAPOR");
            break;
#endif
        case RETRY_AND_MARK_POST_YAPOR:
            sprintf(*buf, "RETRY_AND_MARK_POST_YAPOR");
            break;
#ifdef FROZEN_STACKS
        case RETRY_AND_MARK_FROZEN:
            sprintf(*buf, "RETRY_AND_MARK_FROZEN");
            break;
#else
        case RETRY_AND_MARK_NOFROZEN:
            sprintf(*buf, "RETRY_AND_MARK_NOFROZEN");
            break;
#endif
        case RETRY_AND_MARK_POST_FROZEN:
            sprintf(*buf, "RETRY_AND_MARK_POST_FROZEN");
            break;
#if MULTIPLE_STACKS
        case RETRY_AND_MARK_MULTIPLE_STACKS:
            sprintf(*buf, "RETRY_AND_MARK_MULTIPLE_STACKS");
            break;
#else
        case RETRY_AND_MARK_NOMULTIPLE_STACKS_IF:
            sprintf(*buf, "RETRY_AND_MARK_NOMULTIPLE_STACKS_IF");
            break;
#endif
        case RETRY_AND_MARK_END:
            sprintf(*buf, "RETRY_AND_MARK_END");
            break;
        case TRUST_FAIL_INSTINIT:
            sprintf(*buf, "TRUST_FAIL_INSTINIT");
            break;
#ifdef CUT_C
        case TRUST_FAIL_CUT_C:
            sprintf(*buf, "TRUST_FAIL_CUT_C");
            break;
#endif
#ifdef YAPOR
        case TRUST_FAIL_YAPOR:
            sprintf(*buf, "TRUST_FAIL_YAPOR");
            break;
#endif
        case TRUST_FAIL_NOYAPOR:
            sprintf(*buf, "TRUST_FAIL_NOYAPOR");
            break;
#ifdef YAPOR
        case LBL_SHARED_FAIL:
            sprintf(*buf, "LBL_SHARED_FAIL");
            break;
#endif
        case OP_FAIL_INSTINIT:
            sprintf(*buf, "OP_FAIL_INSTINIT");
            break;
        case LBL_FAIL_INSTINIT:
            sprintf(*buf, "LBL_FAIL_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case LBL_FAIL_LOW_LEVEL_TRACER:
            sprintf(*buf, "LBL_FAIL_LOW_LEVEL_TRACER");
            break;
#endif
        case LBL_FAIL_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "LBL_FAIL_POST_LOW_LEVEL_TRACER");
            break;
        case LBL_FAIL_VARTERM:
            sprintf(*buf, "LBL_FAIL_VARTERM");
            break;
        case LBL_FAIL_PAIRTERM_INIT:
            sprintf(*buf, "LBL_FAIL_PAIRTERM_INIT");
            break;
        case LBL_FAIL_PAIRTERM_END_APPL:
            sprintf(*buf, "LBL_FAIL_PAIRTERM_END_APPL");
            break;
        case LBL_FAIL_END:
            sprintf(*buf, "LBL_FAIL_END");
            break;
        case CUT_INSTINIT:
            sprintf(*buf, "CUT_INSTINIT");
            break;
#ifdef COROUTINING
        case CUT_COROUTINING:
            sprintf(*buf, "CUT_COROUTINING");
            break;
#endif
        case CUT_NOCOROUTINING:
            sprintf(*buf, "CUT_NOCOROUTINING");
            break;
        case CUT_T_INSTINIT:
            sprintf(*buf, "CUT_T_INSTINIT");
            break;
#ifdef COROUTINING
        case CUT_T_COROUTINING:
            sprintf(*buf, "CUT_T_COROUTINING");
            break;
#endif
        case CUT_T_NOCOROUTINING:
            sprintf(*buf, "CUT_T_NOCOROUTINING");
            break;
        case CUT_E_INSTINIT:
            sprintf(*buf, "CUT_E_INSTINIT");
            break;
#ifdef COROUTINING
        case CUT_E_COROUTINING:
            sprintf(*buf, "CUT_E_COROUTINING");
            break;
#endif
        case CUT_E_NOCOROUTINING:
            sprintf(*buf, "CUT_E_NOCOROUTINING");
            break;
        case SAVE_B_X_INSTINIT:
            sprintf(*buf, "SAVE_B_X_INSTINIT");
            break;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
        case SAVE_B_X_YSBA_FROZEN:
            sprintf(*buf, "SAVE_B_X_YSBA_FROZEN");
            break;
#else
        case SAVE_B_X_NOYSBA_NOFROZEN:
            sprintf(*buf, "SAVE_B_X_NOYSBA_NOFROZEN");
            break;
#endif
        case SAVE_B_X_END:
            sprintf(*buf, "SAVE_B_X_END");
            break;
        case SAVE_B_Y_INSTINIT:
            sprintf(*buf, "SAVE_B_Y_INSTINIT");
            break;
#if defined(YAPOR_SBA)
        case SAVE_B_Y_YSBA:
            sprintf(*buf, "SAVE_B_Y_YSBA");
            break;
#else
        case SAVE_B_Y_NOYSBA:
            sprintf(*buf, "SAVE_B_Y_NOYSBA");
            break;
#endif
        case SAVE_B_Y_END:
            sprintf(*buf, "SAVE_B_Y_END");
            break;
        case COMMIT_B_X_INSTINIT:
            sprintf(*buf, "COMMIT_B_X_INSTINIT");
            break;
        case COMMIT_B_X_DO_COMMIT_B_X:
            sprintf(*buf, "COMMIT_B_X_DO_COMMIT_B_X");
            break;
        case COMMIT_B_X_COMMIT_B_X_NVAR:
            sprintf(*buf, "COMMIT_B_X_COMMIT_B_X_NVAR");
            break;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
        case COMMIT_B_X_YSBA_FROZEN:
            sprintf(*buf, "COMMIT_B_X_YSBA_FROZEN");
            break;
#else
        case COMMIT_B_X_NOYSBA_NOFROZEN:
            sprintf(*buf, "COMMIT_B_X_NOYSBA_NOFROZEN");
            break;
#endif
        case COMMIT_B_X_POST_YSBA_FROZEN:
            sprintf(*buf, "COMMIT_B_X_POST_YSBA_FROZEN");
            break;
        case COMMIT_B_X_END:
            sprintf(*buf, "COMMIT_B_X_END");
            break;
        case COMMIT_B_Y_INSTINIT:
            sprintf(*buf, "COMMIT_B_Y_INSTINIT");
            break;
        case COMMIT_B_Y_DO_COMMIT_B_Y:
            sprintf(*buf, "COMMIT_B_Y_DO_COMMIT_B_Y");
            break;
        case COMMIT_B_Y_COMMIT_B_Y_NVAR:
            sprintf(*buf, "COMMIT_B_Y_COMMIT_B_Y_NVAR");
            break;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
        case COMMIT_B_Y_YSBA_FROZEN:
            sprintf(*buf, "COMMIT_B_Y_YSBA_FROZEN");
            break;
#else
        case COMMIT_B_Y_NOYSBA_NOFROZEN:
            sprintf(*buf, "COMMIT_B_Y_NOYSBA_NOFROZEN");
            break;
#endif
        case COMMIT_B_Y_POST_YSBA_FROZEN:
            sprintf(*buf, "COMMIT_B_Y_POST_YSBA_FROZEN");
            break;
        case COMMIT_B_Y_END:
            sprintf(*buf, "COMMIT_B_Y_END");
            break;
        case EXECUTE_INSTINIT:
            sprintf(*buf, "EXECUTE_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case EXECUTE_LOW_LEVEL_TRACER:
            sprintf(*buf, "EXECUTE_LOW_LEVEL_TRACER");
            break;
#endif
        case EXECUTE_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "EXECUTE_POST_LOW_LEVEL_TRACER");
            break;
        case EXECUTE_POST_NOCHECKING:
            sprintf(*buf, "EXECUTE_POST_NOCHECKING");
            break;
#ifdef DEPTH_LIMIT
        case EXECUTE_DEPTH_MINOR:
            sprintf(*buf, "EXECUTE_DEPTH_MINOR");
            break;
        case EXECUTE_DEPTH_MOFPRED:
            sprintf(*buf, "EXECUTE_DEPTH_MOFPRED");
            break;
        case EXECUTE_DEPTH_END:
            sprintf(*buf, "EXECUTE_DEPTH_END");
            break;
#endif
        case EXECUTE_END_END:
            sprintf(*buf, "EXECUTE_END_END");
            break;
        case DEXECUTE_INSTINIT:
            sprintf(*buf, "DEXECUTE_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case DEXECUTE_LOW_LEVEL_TRACER:
            sprintf(*buf, "DEXECUTE_LOW_LEVEL_TRACER");
            break;
#endif
        case DEXECUTE_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "DEXECUTE_POST_LOW_LEVEL_TRACER");
            break;
#ifdef DEPTH_LIMIT
        case DEXECUTE_DEPTH_MINOR:
            sprintf(*buf, "DEXECUTE_DEPTH_MINOR");
            break;
        case DEXECUTE_DEPTH_MOFPRED:
            sprintf(*buf, "DEXECUTE_DEPTH_MOFPRED");
            break;
        case DEXECUTE_DEPTH_END:
            sprintf(*buf, "DEXECUTE_DEPTH_END");
            break;
#endif
        case DEXECUTE_END_END:
            sprintf(*buf, "DEXECUTE_END_END");
            break;
        case FCALL_INST:
            break;
        case CALL_INSTINIT:
            sprintf(*buf, "CALL_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case CALL_LOW_LEVEL_TRACER:
            sprintf(*buf, "CALL_LOW_LEVEL_TRACER");
            break;
#endif
        case CALL_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "CALL_POST_LOW_LEVEL_TRACER");
            break;
        case CALL_POST_NO_CHECKING:
            sprintf(*buf, "CALL_POST_NO_CHECKING");
            break;
#ifdef DEPTH_LIMIT
        case CALL_DEPTH_MINOR:
            sprintf(*buf, "CALL_DEPTH_MINOR");
            break;
        case CALL_DEPTH_MOFPRED:
            sprintf(*buf, "CALL_DEPTH_MOFPRED");
            break;
        case CALL_DEPTH_END:
            sprintf(*buf, "CALL_DEPTH_END");
            break;
#endif
        case CALL_END_END:
            sprintf(*buf, "CALL_END_END");
            break;
        case PROCCEED_INSTINIT:
            sprintf(*buf, "PROCCEED_INSTINIT");
            break;
#ifdef DEPTH_LIMIT
        case PROCCEED_DEPTH:
            sprintf(*buf, "PROCCEED_DEPTH");
            break;
#endif
        case PROCCEED_END:
            sprintf(*buf, "PROCCEED_END");
            break;
        case ALLOCATE_INSTINIT:
            sprintf(*buf, "ALLOCATE_INSTINIT");
            break;
#ifdef DEPTH_LIMIT
        case ALLOCATE_DEPTH:
            sprintf(*buf, "ALLOCATE_DEPTH");
            break;
#endif
        case ALLOCATE_END:
            sprintf(*buf, "ALLOCATE_END");
            break;
        case DEALLOCATE_INSTINIT:
            sprintf(*buf, "DEALLOCATE_INSTINIT");
            break;
        case DEALLOCATE_POST_CHECK:
            sprintf(*buf, "DEALLOCATE_POST_CHECK");
            break;
#ifdef DEPTH_LIMIT
        case DEALLOCATE_DEPTH:
            sprintf(*buf, "DEALLOCATE_DEPTH");
            break;
#endif
        case DEALLOCATE_FROZEN:
            sprintf(*buf, "DEALLOCATE_FROZEN");
            break;
        case DEALLOCATE_POST_FROZEN:
            sprintf(*buf, "DEALLOCATE_POST_FROZEN");
            break;
        case DEALLOCATE_END:
            sprintf(*buf, "DEALLOCATE_END");
            break;
        case GET_X_VAR_INSTINIT:
            sprintf(*buf, "GET_X_VAR_INSTINIT");
            break;
        case GET_Y_VAR_INSTINIT:
            sprintf(*buf, "GET_Y_VAR_INSTINIT");
            break;
        case GET_YY_VAR_INSTINIT:
            sprintf(*buf, "GET_YY_VAR_INSTINIT");
            break;
        case GET_X_VAL_INSTINIT:
            sprintf(*buf, "GET_X_VAL_INSTINIT");
            break;
        case GET_X_VAL_GVALX_NONVAR:
            sprintf(*buf, "GET_X_VAL_GVALX_NONVAR");
            break;
        case GET_X_VAL_GVALX_NONVAR_NONVAR:
            sprintf(*buf, "GET_X_VAL_GVALX_NONVAR_NONVAR");
            break;
        case GET_X_VAL_GVALX_NONVAR_UNK:
            sprintf(*buf, "GET_X_VAL_GVALX_NONVAR_UNK");
            break;
        case GET_X_VAL_GVALX_UNK:
            sprintf(*buf, "GET_X_VAL_GVALX_UNK");
            break;
        case GET_X_VAL_GVALX_VAR_NONVAR:
            sprintf(*buf, "GET_X_VAL_GVALX_VAR_NONVAR");
            break;
        case GET_X_VAL_GVALX_VAR_UNK:
            sprintf(*buf, "GET_X_VAL_GVALX_VAR_UNK");
            break;
        case GET_Y_VAL_INSTINIT:
            sprintf(*buf, "GET_Y_VAL_INSTINIT");
            break;
        case GET_Y_VAL_GVALY_NONVAR:
            sprintf(*buf, "GET_Y_VAL_GVALY_NONVAR");
            break;
        case GET_Y_VAL_GVALY_NONVAR_NONVAR:
            sprintf(*buf, "GET_Y_VAL_GVALY_NONVAR_NONVAR");
            break;
        case GET_Y_VAL_GVALY_NONVAR_UNK:
            sprintf(*buf, "GET_Y_VAL_GVALY_NONVAR_UNK");
            break;
        case GET_Y_VAL_GVALY_UNK:
            sprintf(*buf, "GET_Y_VAL_GVALY_UNK");
            break;
        case GET_Y_VAL_GVALY_VAR_NONVAR:
            sprintf(*buf, "GET_Y_VAL_GVALY_VAR_NONVAR");
            break;
        case GET_Y_VAL_GVALY_VAR_UNK:
            sprintf(*buf, "GET_Y_VAL_GVALY_VAR_UNK");
            break;
        case GET_ATOM_INSTINIT:
            sprintf(*buf, "GET_ATOM_INSTINIT");
            break;
        case GET_ATOM_GATOM_NONVAR:
            sprintf(*buf, "GET_ATOM_GATOM_NONVAR");
            break;
        case GET_ATOM_GATOM_UNK:
            sprintf(*buf, "GET_ATOM_GATOM_UNK");
            break;
        case GET_2ATOMS_INSTINIT:
            sprintf(*buf, "GET_2ATOMS_INSTINIT");
            break;
        case GET_2ATOMS_GATOM_2UNK:
            sprintf(*buf, "GET_2ATOMS_GATOM_2UNK");
            break;
        case GET_2ATOMS_GATOM_2B:
            sprintf(*buf, "GET_2ATOMS_GATOM_2B");
            break;
        case GET_2ATOMS_GATOM_2BNONVAR:
            sprintf(*buf, "GET_2ATOMS_GATOM_2BNONVAR");
            break;
        case GET_2ATOMS_GATOM_2BUNK:
            sprintf(*buf, "GET_2ATOMS_GATOM_2BUNK");
            break;
        case GET_3ATOMS_INSTINIT:
            sprintf(*buf, "GET_3ATOMS_INSTINIT");
            break;
        case GET_3ATOMS_GATOM_3UNK:
            sprintf(*buf, "GET_3ATOMS_GATOM_3UNK");
            break;
        case GET_3ATOMS_GATOM_3B:
            sprintf(*buf, "GET_3ATOMS_GATOM_3B");
            break;
        case GET_3ATOMS_GATOM_3BUNK:
            sprintf(*buf, "GET_3ATOMS_GATOM_3BUNK");
            break;
        case GET_3ATOMS_GATOM_3C:
            sprintf(*buf, "GET_3ATOMS_GATOM_3C");
            break;
        case GET_3ATOMS_GATOM_3CNONVAR:
            sprintf(*buf, "GET_3ATOMS_GATOM_3CNONVAR");
            break;
        case GET_3ATOMS_GATOM_3CUNK:
            sprintf(*buf, "GET_3ATOMS_GATOM_3CUNK");
            break;
        case GET_4ATOMS_INSTINIT:
            sprintf(*buf, "GET_4ATOMS_INSTINIT");
            break;
        case GET_4ATOMS_GATOM_4UNK:
            sprintf(*buf, "GET_4ATOMS_GATOM_4UNK");
            break;
        case GET_4ATOMS_GATOM_4B:
            sprintf(*buf, "GET_4ATOMS_GATOM_4B");
            break;
        case GET_4ATOMS_GATOM_4BUNK:
            sprintf(*buf, "GET_4ATOMS_GATOM_4BUNK");
            break;
        case GET_4ATOMS_GATOM_4C:
            sprintf(*buf, "GET_4ATOMS_GATOM_4C");
            break;
        case GET_4ATOMS_GATOM_4CUNK:
            sprintf(*buf, "GET_4ATOMS_GATOM_4CUNK");
            break;
        case GET_4ATOMS_GATOM_4D:
            sprintf(*buf, "GET_4ATOMS_GATOM_4D");
            break;
        case GET_4ATOMS_GATOM_4DNONVAR:
            sprintf(*buf, "GET_4ATOMS_GATOM_4DNONVAR");
            break;
        case GET_4ATOMS_GATOM_4DUNK:
            sprintf(*buf, "GET_4ATOMS_GATOM_4DUNK");
            break;
        case GET_5ATOMS_INSTINIT:
            sprintf(*buf, "GET_5ATOMS_INSTINIT");
            break;
        case GET_5ATOMS_GATOM_5UNK:
            sprintf(*buf, "GET_5ATOMS_GATOM_5UNK");
            break;
        case GET_5ATOMS_GATOM_5B:
            sprintf(*buf, "GET_5ATOMS_GATOM_5B");
            break;
        case GET_5ATOMS_GATOM_5BUNK:
            sprintf(*buf, "GET_5ATOMS_GATOM_5BUNK");
            break;
        case GET_5ATOMS_GATOM_5C:
            sprintf(*buf, "GET_5ATOMS_GATOM_5C");
            break;
        case GET_5ATOMS_GATOM_5CUNK:
            sprintf(*buf, "GET_5ATOMS_GATOM_5CUNK");
            break;
        case GET_5ATOMS_GATOM_5D:
            sprintf(*buf, "GET_5ATOMS_GATOM_5D");
            break;
        case GET_5ATOMS_GATOM_5DUNK:
            sprintf(*buf, "GET_5ATOMS_GATOM_5DUNK");
            break;
        case GET_5ATOMS_GATOM_5E:
            sprintf(*buf, "GET_5ATOMS_GATOM_5E");
            break;
        case GET_5ATOMS_GATOM_5ENONVAR:
            sprintf(*buf, "GET_5ATOMS_GATOM_5ENONVAR");
            break;
        case GET_5ATOMS_GATOM_5EUNK:
            sprintf(*buf, "GET_5ATOMS_GATOM_5EUNK");
            break;
        case GET_6ATOMS_INSTINIT:
            sprintf(*buf, "GET_6ATOMS_INSTINIT");
            break;
        case GET_6ATOMS_GATOM_6UNK:
            sprintf(*buf, "GET_6ATOMS_GATOM_6UNK");
            break;
        case GET_6ATOMS_GATOM_6B:
            sprintf(*buf, "GET_6ATOMS_GATOM_6B");
            break;
        case GET_6ATOMS_GATOM_6BUNK:
            sprintf(*buf, "GET_6ATOMS_GATOM_6BUNK");
            break;
        case GET_6ATOMS_GATOM_6C:
            sprintf(*buf, "GET_6ATOMS_GATOM_6C");
            break;
        case GET_6ATOMS_GATOM_6CUNK:
            sprintf(*buf, "GET_6ATOMS_GATOM_6CUNK");
            break;
        case GET_6ATOMS_GATOM_6D:
            sprintf(*buf, "GET_6ATOMS_GATOM_6D");
            break;
        case GET_6ATOMS_GATOM_6DUNK:
            sprintf(*buf, "GET_6ATOMS_GATOM_6DUNK");
            break;
        case GET_6ATOMS_GATOM_6E:
            sprintf(*buf, "GET_6ATOMS_GATOM_6E");
            break;
        case GET_6ATOMS_GATOM_6EUNK:
            sprintf(*buf, "GET_6ATOMS_GATOM_6EUNK");
            break;
        case GET_6ATOMS_GATOM_6F:
            sprintf(*buf, "GET_6ATOMS_GATOM_6F");
            break;
        case GET_6ATOMS_GATOM_6FNONVAR:
            sprintf(*buf, "GET_6ATOMS_GATOM_6FNONVAR");
            break;
        case GET_6ATOMS_GATOM_6FUNK:
            sprintf(*buf, "GET_6ATOMS_GATOM_6FUNK");
            break;
        case GET_LIST_INSTINIT:
            sprintf(*buf, "GET_LIST_INSTINIT");
            break;
        case GET_LIST_GLIST_NONVAR:
            sprintf(*buf, "GET_LIST_GLIST_NONVAR");
            break;
        case GET_LIST_GLIST_UNK:
            sprintf(*buf, "GET_LIST_GLIST_UNK");
            break;
        case GET_STRUCT_INSTINIT:
            sprintf(*buf, "GET_STRUCT_INSTINIT");
            break;
        case GET_STRUCT_GSTRUCT_NONVAR:
            sprintf(*buf, "GET_STRUCT_GSTRUCT_NONVAR");
            break;
        case GET_STRUCT_GSTRUCT_UNK:
            sprintf(*buf, "GET_STRUCT_GSTRUCT_UNK");
            break;
        case GET_FLOAT_INSTINIT:
            sprintf(*buf, "GET_FLOAT_INSTINIT");
            break;
        case GET_FLOAT_GFLOAT_NONVAR:
            sprintf(*buf, "GET_FLOAT_GFLOAT_NONVAR");
            break;
        case GET_FLOAT_GFLOAT_UNK:
            sprintf(*buf, "GET_FLOAT_GFLOAT_UNK");
            break;
        case GET_LONGINT_INSTINIT:
            sprintf(*buf, "GET_LONGINT_INSTINIT");
            break;
        case GET_LONGINT_GLONGINT_NONVAR:
            sprintf(*buf, "GET_LONGINT_GLONGINT_NONVAR");
            break;
        case GET_LONGINT_GLONGINT_UNK:
            sprintf(*buf, "GET_LONGINT_GLONGINT_UNK");
            break;
#ifdef USE_GMP
        case GET_BIGINT_INSTINIT:
            sprintf(*buf, "GET_BIGINT_INSTINIT");
            break;
        case GET_BIGINT_GBIGINT_NONVAR:
            sprintf(*buf, "GET_BIGINT_GBIGINT_NONVAR");
            break;
        case GET_BIGINT_GBIGINT_UNK:
            sprintf(*buf, "GET_BIGINT_GBIGINT_UNK");
            break;
#endif
        case GET_DBTERM_INSTINIT:
            sprintf(*buf, "GET_DBTERM_INSTINIT");
            break;
        case GET_DBTERM_GDBTERM_NONVAR:
            sprintf(*buf, "GET_DBTERM_GDBTERM_NONVAR");
            break;
        case GET_DBTERM_GDBTERM_UNK:
            sprintf(*buf, "GET_DBTERM_GDBTERM_UNK");
            break;
        case GLIST_VALX_INSTINIT:
            sprintf(*buf, "GLIST_VALX_INSTINIT");
            break;
        case GLIST_VALX_GLIST_VALX_READ:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_READ");
            break;
        case GLIST_VALX_GLIST_VALX_NONVAR:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_NONVAR");
            break;
        case GLIST_VALX_GLIST_VALX_NONVAR_NONVAR:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_NONVAR_NONVAR");
            break;
        case GLIST_VALX_GLIST_VALX_NONVAR_UNK:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_NONVAR_UNK");
            break;
        case GLIST_VALX_GLIST_VALX_UNK:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_UNK");
            break;
        case GLIST_VALX_GLIST_VALX_VAR_NONVAR:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_VAR_NONVAR");
            break;
        case GLIST_VALX_GLIST_VALX_VAR_UNK:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_VAR_UNK");
            break;
        case GLIST_VALX_GLIST_VALX_WRITE:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_WRITE");
            break;
        case GLIST_VALY_INSTINIT:
            sprintf(*buf, "GLIST_VALY_INSTINIT");
            break;
        case GLIST_VALY_GLIST_VALY_READ:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_READ");
            break;
        case GLIST_VALY_GLIST_VALY_NONVAR:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_NONVAR");
            break;
        case GLIST_VALY_GLIST_VALY_NONVAR_NONVAR:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_NONVAR_NONVAR");
            break;
        case GLIST_VALY_GLIST_VALY_NONVAR_UNK:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_NONVAR_UNK");
            break;
        case GLIST_VALY_GLIST_VALY_UNK:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_UNK");
            break;
        case GLIST_VALY_GLIST_VALY_VAR_NONVAR:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_VAR_NONVAR");
            break;
        case GLIST_VALY_GLIST_VALY_VAR_UNK:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_VAR_UNK");
            break;
        case GLIST_VALY_GLIST_VALY_WRITE:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_WRITE");
            break;
        case GL_VOID_VARX_INSTINIT:
            sprintf(*buf, "GL_VOID_VARX_INSTINIT");
            break;
        case GL_VOID_VARX_GLIST_VOID_VARX_READ:
            sprintf(*buf, "GL_VOID_VARX_GLIST_VOID_VARX_READ");
            break;
        case GL_VOID_VARX_GLIST_VOID_VAR_WRITE:
            sprintf(*buf, "GL_VOID_VARX_GLIST_VOID_VAR_WRITE");
            break;
        case GL_VOID_VARY_INSTINIT:
            sprintf(*buf, "GL_VOID_VARY_INSTINIT");
            break;
        case GL_VOID_VARY_GLIST_VOID_VARY_READ:
            sprintf(*buf, "GL_VOID_VARY_GLIST_VOID_VARY_READ");
            break;
        case GL_VOID_VARY_GLIST_VOID_VARY_WRITE:
            sprintf(*buf, "GL_VOID_VARY_GLIST_VOID_VARY_WRITE");
            break;
        case GL_VOID_VALX_INSTINIT:
            sprintf(*buf, "GL_VOID_VALX_INSTINIT");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_READ:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_READ");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_NONVAR:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_NONVAR");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_NONVAR_NONVAR:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_NONVAR_NONVAR");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_NONVAR_UNK:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_NONVAR_UNK");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_UNK:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_UNK");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_VAR_NONVAR:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_VAR_NONVAR");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_VAR_UNK:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_VAR_UNK");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_WRITE:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_WRITE");
            break;
        case GL_VOID_VALY_INSTINIT:
            sprintf(*buf, "GL_VOID_VALY_INSTINIT");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_READ:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_READ");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_NONVAR:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_NONVAR");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_NONVAR_NONVAR:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_NONVAR_NONVAR");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_NONVAR_UNK:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_NONVAR_UNK");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_UNK:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_UNK");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_VAR_NONVAR:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_VAR_NONVAR");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_VAR_UNK:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_VAR_UNK");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_WRITE:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_WRITE");
            break;
        case UNIFY_X_VAR_INSTINIT:
            sprintf(*buf, "UNIFY_X_VAR_INSTINIT");
            break;
#ifdef YAPOR_SBA
        case UNIFY_X_VAR_YAPOR_SBA:
            sprintf(*buf, "UNIFY_X_VAR_YAPOR_SBA");
            break;
#endif
        case UNIFY_X_VAR_END:
            sprintf(*buf, "UNIFY_X_VAR_END");
            break;
        case UNIFY_X_VAR_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_X_VAR_WRITE_INSTINIT");
            break;
        case UNIFY_L_X_VAR_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_VAR_INSTINIT");
            break;
#ifdef YAPOR_SBA
        case UNIFY_L_X_VAR_YAPOR_SBA:
            sprintf(*buf, "UNIFY_L_X_VAR_YAPOR_SBA");
            break;
#endif
        case UNIFY_L_X_VAR_END:
            sprintf(*buf, "UNIFY_L_X_VAR_END");
            break;
        case UNIFY_L_X_VAR_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_VAR_WRITE_INSTINIT");
            break;
        case UNIFY_X_VAR2_INSTINIT:
            sprintf(*buf, "UNIFY_X_VAR2_INSTINIT");
            break;
#ifdef YAPOR_SBA
        case UNIFY_X_VAR2_YAPOR_SBA:
            sprintf(*buf, "UNIFY_X_VAR2_YAPOR_SBA");
            break;
#endif
        case UNIFY_X_VAR2_END:
            sprintf(*buf, "UNIFY_X_VAR2_END");
            break;
        case UNIFY_X_VAR2_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_X_VAR2_WRITE_INSTINIT");
            break;
        case UNIFY_L_X_VAR2_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_VAR2_INSTINIT");
            break;
        case UNIFY_L_X_VAR2_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_VAR2_WRITE_INSTINIT");
            break;
        case UNIFY_Y_VAR_INSTINIT:
            sprintf(*buf, "UNIFY_Y_VAR_INSTINIT");
            break;
        case UNIFY_Y_VAR_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_Y_VAR_WRITE_INSTINIT");
            break;
        case UNIFY_L_Y_VAR_INSTINIT:
            sprintf(*buf, "UNIFY_L_Y_VAR_INSTINIT");
            break;
        case UNIFY_L_Y_VAR_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_Y_VAR_WRITE_INSTINIT");
            break;
        case UNIFY_X_VAL_INSTINIT:
            sprintf(*buf, "UNIFY_X_VAL_INSTINIT");
            break;
        case UNIFY_X_VAL_UVALX_NONVAR:
            sprintf(*buf, "UNIFY_X_VAL_UVALX_NONVAR");
            break;
        case UNIFY_X_VAL_UVALX_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_X_VAL_UVALX_NONVAR_NONVAR");
            break;
        case UNIFY_X_VAL_UVALX_NONVAR_UNK:
            sprintf(*buf, "UNIFY_X_VAL_UVALX_NONVAR_UNK");
            break;
        case UNIFY_X_VAL_UVALX_UNK:
            sprintf(*buf, "UNIFY_X_VAL_UVALX_UNK");
            break;
        case UNIFY_X_VAL_UVALX_VAR_NONVAR:
            sprintf(*buf, "UNIFY_X_VAL_UVALX_VAR_NONVAR");
            break;
        case UNIFY_X_VAL_UVALX_VAR_UNK:
            sprintf(*buf, "UNIFY_X_VAL_UVALX_VAR_UNK");
            break;
        case UNIFY_X_VAL_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_X_VAL_WRITE_INSTINIT");
            break;
        case UNIFY_L_X_VAL_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_VAL_INSTINIT");
            break;
        case UNIFY_L_X_VAL_ULVALX_NONVAR:
            sprintf(*buf, "UNIFY_L_X_VAL_ULVALX_NONVAR");
            break;
        case UNIFY_L_X_VAL_ULVALX_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_L_X_VAL_ULVALX_NONVAR_NONVAR");
            break;
        case UNIFY_L_X_VAL_ULVALX_NONVAR_UNK:
            sprintf(*buf, "UNIFY_L_X_VAL_ULVALX_NONVAR_UNK");
            break;
        case UNIFY_L_X_VAL_ULVALX_UNK:
            sprintf(*buf, "UNIFY_L_X_VAL_ULVALX_UNK");
            break;
        case UNIFY_L_X_VAL_ULVALX_VAR_NONVAR:
            sprintf(*buf, "UNIFY_L_X_VAL_ULVALX_VAR_NONVAR");
            break;
        case UNIFY_L_X_VAL_ULVALX_VAR_UNK:
            sprintf(*buf, "UNIFY_L_X_VAL_ULVALX_VAR_UNK");
            break;
        case UNIFY_L_X_VAL_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_VAL_WRITE_INSTINIT");
            break;
        case UNIFY_Y_VAL_INSTINIT:
            sprintf(*buf, "UNIFY_Y_VAL_INSTINIT");
            break;
        case UNIFY_Y_VAL_UVALY_NONVAR:
            sprintf(*buf, "UNIFY_Y_VAL_UVALY_NONVAR");
            break;
        case UNIFY_Y_VAL_UVALY_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_Y_VAL_UVALY_NONVAR_NONVAR");
            break;
        case UNIFY_Y_VAL_UVALY_NONVAR_UNK:
            sprintf(*buf, "UNIFY_Y_VAL_UVALY_NONVAR_UNK");
            break;
        case UNIFY_Y_VAL_UVALY_UNK:
            sprintf(*buf, "UNIFY_Y_VAL_UVALY_UNK");
            break;
        case UNIFY_Y_VAL_UVALY_VAR_NONVAR:
            sprintf(*buf, "UNIFY_Y_VAL_UVALY_VAR_NONVAR");
            break;
        case UNIFY_Y_VAL_UVALY_VAR_UNK:
            sprintf(*buf, "UNIFY_Y_VAL_UVALY_VAR_UNK");
            break;
        case UNIFY_Y_VAL_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_Y_VAL_WRITE_INSTINIT");
            break;
        case UNIFY_L_Y_VAL_INSTINIT:
            sprintf(*buf, "UNIFY_L_Y_VAL_INSTINIT");
            break;
        case UNIFY_L_Y_VAL_ULVALY_NONVAR:
            sprintf(*buf, "UNIFY_L_Y_VAL_ULVALY_NONVAR");
            break;
        case UNIFY_L_Y_VAL_ULVALY_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_L_Y_VAL_ULVALY_NONVAR_NONVAR");
            break;
        case UNIFY_L_Y_VAL_ULVALY_NONVAR_UNK:
            sprintf(*buf, "UNIFY_L_Y_VAL_ULVALY_NONVAR_UNK");
            break;
        case UNIFY_L_Y_VAL_ULVALY_UNK:
            sprintf(*buf, "UNIFY_L_Y_VAL_ULVALY_UNK");
            break;
        case UNIFY_L_Y_VAL_ULVALY_VAR_NONVAR:
            sprintf(*buf, "UNIFY_L_Y_VAL_ULVALY_VAR_NONVAR");
            break;
        case UNIFY_L_Y_VAL_ULVALY_VAR_UNK:
            sprintf(*buf, "UNIFY_L_Y_VAL_ULVALY_VAR_UNK");
            break;
        case UNIFY_L_Y_VAL_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_Y_VAL_WRITE_INSTINIT");
            break;
        case UNIFY_X_LOC_INSTINIT:
            sprintf(*buf, "UNIFY_X_LOC_INSTINIT");
            break;
        case UNIFY_X_LOC_UVALX_LOC_NONVAR:
            sprintf(*buf, "UNIFY_X_LOC_UVALX_LOC_NONVAR");
            break;
        case UNIFY_X_LOC_UVALX_LOC_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_X_LOC_UVALX_LOC_NONVAR_NONVAR");
            break;
        case UNIFY_X_LOC_UVALX_LOC_NONVAR_UNK:
            sprintf(*buf, "UNIFY_X_LOC_UVALX_LOC_NONVAR_UNK");
            break;
        case UNIFY_X_LOC_UVALX_LOC_UNK:
            sprintf(*buf, "UNIFY_X_LOC_UVALX_LOC_UNK");
            break;
        case UNIFY_X_LOC_UVALX_LOC_VAR_NONVAR:
            sprintf(*buf, "UNIFY_X_LOC_UVALX_LOC_VAR_NONVAR");
            break;
        case UNIFY_X_LOC_UVALX_LOC_VAR_UNK:
            sprintf(*buf, "UNIFY_X_LOC_UVALX_LOC_VAR_UNK");
            break;
        case UNIFY_X_LOC_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_X_LOC_WRITE_INSTINIT");
            break;
        case UNIFY_X_LOC_WRITE_UNIFY_X_LOC_NONVAR:
            sprintf(*buf, "UNIFY_X_LOC_WRITE_UNIFY_X_LOC_NONVAR");
            break;
        case UNIFY_X_LOC_WRITE_UNIFY_X_LOC_UNK:
            sprintf(*buf, "UNIFY_X_LOC_WRITE_UNIFY_X_LOC_UNK");
            break;
        case UNIFY_L_X_LOC_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_LOC_INSTINIT");
            break;
        case UNIFY_L_X_LOC_ULVALX_LOC_NONVAR:
            sprintf(*buf, "UNIFY_L_X_LOC_ULVALX_LOC_NONVAR");
            break;
        case UNIFY_L_X_LOC_ULVALX_LOC_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_L_X_LOC_ULVALX_LOC_NONVAR_NONVAR");
            break;
        case UNIFY_L_X_LOC_ULVALX_LOC_NONVAR_UNK:
            sprintf(*buf, "UNIFY_L_X_LOC_ULVALX_LOC_NONVAR_UNK");
            break;
        case UNIFY_L_X_LOC_ULVALX_LOC_UNK:
            sprintf(*buf, "UNIFY_L_X_LOC_ULVALX_LOC_UNK");
            break;
        case UNIFY_L_X_LOC_ULVALX_LOC_VAR_NONVAR:
            sprintf(*buf, "UNIFY_L_X_LOC_ULVALX_LOC_VAR_NONVAR");
            break;
        case UNIFY_L_X_LOC_ULVALX_LOC_VAR_UNK:
            sprintf(*buf, "UNIFY_L_X_LOC_ULVALX_LOC_VAR_UNK");
            break;
        case UNIFY_L_X_LOC_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_LOC_WRITE_INSTINIT");
            break;
        case UNIFY_L_X_LOC_WRITE_ULNIFY_X_LOC_NONVAR:
            sprintf(*buf, "UNIFY_L_X_LOC_WRITE_ULNIFY_X_LOC_NONVAR");
            break;
        case UNIFY_L_X_LOC_WRITE_ULNIFY_X_LOC_UNK:
            sprintf(*buf, "UNIFY_L_X_LOC_WRITE_ULNIFY_X_LOC_UNK");
            break;
        case UNIFY_Y_LOC_INSTINIT:
            sprintf(*buf, "UNIFY_Y_LOC_INSTINIT");
            break;
        case UNIFY_Y_LOC_UVALY_LOC_NONVAR:
            sprintf(*buf, "UNIFY_Y_LOC_UVALY_LOC_NONVAR");
            break;
        case UNIFY_Y_LOC_UVALY_LOC_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_Y_LOC_UVALY_LOC_NONVAR_NONVAR");
            break;
        case UNIFY_Y_LOC_UVALY_LOC_NONVAR_UNK:
            sprintf(*buf, "UNIFY_Y_LOC_UVALY_LOC_NONVAR_UNK");
            break;
        case UNIFY_Y_LOC_UVALY_LOC_UNK:
            sprintf(*buf, "UNIFY_Y_LOC_UVALY_LOC_UNK");
            break;
        case UNIFY_Y_LOC_UVALY_LOC_VAR_NONVAR:
            sprintf(*buf, "UNIFY_Y_LOC_UVALY_LOC_VAR_NONVAR");
            break;
        case UNIFY_Y_LOC_UVALY_LOC_VAR_UNK:
            sprintf(*buf, "UNIFY_Y_LOC_UVALY_LOC_VAR_UNK");
            break;
        case UNIFY_Y_LOC_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_Y_LOC_WRITE_INSTINIT");
            break;
        case UNIFY_Y_LOC_WRITE_UNIFY_Y_LOC_NONVAR:
            sprintf(*buf, "UNIFY_Y_LOC_WRITE_UNIFY_Y_LOC_NONVAR");
            break;
        case UNIFY_Y_LOC_WRITE_UNIFY_Y_LOC_UNK:
            sprintf(*buf, "UNIFY_Y_LOC_WRITE_UNIFY_Y_LOC_UNK");
            break;
        case UNIFY_L_Y_LOC_INSTINIT:
            sprintf(*buf, "UNIFY_L_Y_LOC_INSTINIT");
            break;
        case UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR:
            sprintf(*buf, "UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR");
            break;
        case UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR_NONVAR");
            break;
        case UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR_UNK:
            sprintf(*buf, "UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR_UNK");
            break;
        case UNIFY_L_Y_LOC_ULVALY_LOC_UNK:
            sprintf(*buf, "UNIFY_L_Y_LOC_ULVALY_LOC_UNK");
            break;
        case UNIFY_L_Y_LOC_ULVALY_LOC_VAR_NONVAR:
            sprintf(*buf, "UNIFY_L_Y_LOC_ULVALY_LOC_VAR_NONVAR");
            break;
        case UNIFY_L_Y_LOC_ULVALY_LOC_VAR_UNK:
            sprintf(*buf, "UNIFY_L_Y_LOC_ULVALY_LOC_VAR_UNK");
            break;
        case UNIFY_L_Y_LOC_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_Y_LOC_WRITE_INSTINIT");
            break;
        case UNIFY_L_Y_LOC_WRITE_ULUNIFY_Y_LOC_NONVAR:
            sprintf(*buf, "UNIFY_L_Y_LOC_WRITE_ULUNIFY_Y_LOC_NONVAR");
            break;
        case UNIFY_L_Y_LOC_WRITE_ULUNIFY_Y_LOC_UNK:
            sprintf(*buf, "UNIFY_L_Y_LOC_WRITE_ULUNIFY_Y_LOC_UNK");
            break;
        case UNIFY_VOID_INSTINIT:
            sprintf(*buf, "UNIFY_VOID_INSTINIT");
            break;
        case UNIFY_VOID_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_VOID_WRITE_INSTINIT");
            break;
        case UNIFY_L_VOID_INSTINIT:
            sprintf(*buf, "UNIFY_L_VOID_INSTINIT");
            break;
        case UNIFY_L_VOID_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_VOID_WRITE_INSTINIT");
            break;
        case UNIFY_N_VOIDS_INSTINIT:
            sprintf(*buf, "UNIFY_N_VOIDS_INSTINIT");
            break;
        case UNIFY_N_VOIDS_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_N_VOIDS_WRITE_INSTINIT");
            break;
        case UNIFY_L_N_VOIDS_INSTINIT:
            sprintf(*buf, "UNIFY_L_N_VOIDS_INSTINIT");
            break;
        case UNIFY_L_N_VOIDS_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_N_VOIDS_WRITE_INSTINIT");
            break;
        case UNIFY_ATOM_INSTINIT:
            sprintf(*buf, "UNIFY_ATOM_INSTINIT");
            break;
        case UNIFY_ATOM_UATOM_NONVAR:
            sprintf(*buf, "UNIFY_ATOM_UATOM_NONVAR");
            break;
        case UNIFY_ATOM_UATOM_UNK:
            sprintf(*buf, "UNIFY_ATOM_UATOM_UNK");
            break;
        case UNIFY_ATOM_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_ATOM_WRITE_INSTINIT");
            break;
        case UNIFY_L_ATOM_INSTINIT:
            sprintf(*buf, "UNIFY_L_ATOM_INSTINIT");
            break;
        case UNIFY_L_ATOM_ULATOM_NONVAR:
            sprintf(*buf, "UNIFY_L_ATOM_ULATOM_NONVAR");
            break;
        case UNIFY_L_ATOM_ULATOM_UNK:
            sprintf(*buf, "UNIFY_L_ATOM_ULATOM_UNK");
            break;
        case UNIFY_L_ATOM_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_ATOM_WRITE_INSTINIT");
            break;
        case UNIFY_N_ATOMS_INSTINIT:
            sprintf(*buf, "UNIFY_N_ATOMS_INSTINIT");
            break;
        case UNIFY_N_ATOMS_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_N_ATOMS_WRITE_INSTINIT");
            break;
        case UNIFY_FLOAT_INSTINIT:
            sprintf(*buf, "UNIFY_FLOAT_INSTINIT");
            break;
        case UNIFY_FLOAT_UFLOAT_NONVAR_INIT:
            sprintf(*buf, "UNIFY_FLOAT_UFLOAT_NONVAR_INIT");
            break;
        case UNIFY_FLOAT_UFLOAT_NONVAR_D0ISFUNCTOR:
            sprintf(*buf, "UNIFY_FLOAT_UFLOAT_NONVAR_D0ISFUNCTOR");
            break;
        case UNIFY_FLOAT_UFLOAT_NONVAR_END:
            sprintf(*buf, "UNIFY_FLOAT_UFLOAT_NONVAR_END");
            break;
        case UNIFY_FLOAT_UFLOAT_UNK:
            sprintf(*buf, "UNIFY_FLOAT_UFLOAT_UNK");
            break;
        case UNIFY_FLOAT_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_FLOAT_WRITE_INSTINIT");
            break;
        case UNIFY_L_FLOAT_INSTINIT:
            sprintf(*buf, "UNIFY_L_FLOAT_INSTINIT");
            break;
        case UNIFY_L_FLOAT_D0ISAPPL:
            sprintf(*buf, "UNIFY_L_FLOAT_D0ISAPPL");
            break;
        case UNIFY_L_FLOAT_D0ISFUNC:
            sprintf(*buf, "UNIFY_L_FLOAT_D0ISFUNC");
            break;
        case UNIFY_L_FLOAT_EQUALS:
            sprintf(*buf, "UNIFY_L_FLOAT_EQUALS");
            break;
        case UNIFY_L_FLOAT_ULFLOAT_UNK:
            sprintf(*buf, "UNIFY_L_FLOAT_ULFLOAT_UNK");
            break;
        case UNIFY_L_FLOAT_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_FLOAT_WRITE_INSTINIT");
            break;
        case UNIFY_LONGINT_INSTINIT:
            sprintf(*buf, "UNIFY_LONGINT_INSTINIT");
            break;
        case UNIFY_LONGINT_D0ISAPPL:
            sprintf(*buf, "UNIFY_LONGINT_D0ISAPPL");
            break;
        case UNIFY_LONGINT_D0ISFUNC:
            sprintf(*buf, "UNIFY_LONGINT_D0ISFUNC");
            break;
        case UNIFY_LONGINT_EQUALS:
            sprintf(*buf, "UNIFY_LONGINT_EQUALS");
            break;
        case UNIFY_LONGINT_ULONGINT_UNK:
            sprintf(*buf, "UNIFY_LONGINT_ULONGINT_UNK");
            break;
        case UNIFY_LONGINT_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_LONGINT_WRITE_INSTINIT");
            break;
        case UNIFY_L_LONGINT_INSTINIT:
            sprintf(*buf, "UNIFY_L_LONGINT_INSTINIT");
            break;
        case UNIFY_L_LONGINT_D0ISAPPL:
            sprintf(*buf, "UNIFY_L_LONGINT_D0ISAPPL");
            break;
        case UNIFY_L_LONGINT_D0ISFUNC:
            sprintf(*buf, "UNIFY_L_LONGINT_D0ISFUNC");
            break;
        case UNIFY_L_LONGINT_EQUALS:
            sprintf(*buf, "UNIFY_L_LONGINT_EQUALS");
            break;
        case UNIFY_L_LONGINT_ULLONGINT_UNK:
            sprintf(*buf, "UNIFY_L_LONGINT_ULLONGINT_UNK");
            break;
        case UNIFY_L_LONGINT_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_LONGINT_WRITE_INSTINIT");
            break;
#ifdef USE_GMP
        case UNIFY_BIGINT_INSTINIT:
            sprintf(*buf, "UNIFY_BIGINT_INSTINIT");
            break;
        case UNIFY_BIGINT_D0ISAPPL:
            sprintf(*buf, "UNIFY_BIGINT_D0ISAPPL");
            break;
        case UNIFY_BIGINT_D1ISFUNC_GMP:
            sprintf(*buf, "UNIFY_BIGINT_D1ISFUNC_GMP");
            break;
        case UNIFY_BIGINT_UBIGINT_UNK:
            sprintf(*buf, "UNIFY_BIGINT_UBIGINT_UNK");
            break;
        case UNIFY_L_BIGINT_INSTINIT:
            sprintf(*buf, "UNIFY_L_BIGINT_INSTINIT");
            break;
        case UNIFY_L_BIGINT_D0ISAPPL:
            sprintf(*buf, "UNIFY_L_BIGINT_D0ISAPPL");
            break;
        case UNIFY_L_BIGINT_D0ISFUNC_GMP:
            sprintf(*buf, "UNIFY_L_BIGINT_D0ISFUNC_GMP");
            break;
        case UNIFY_L_BIGINT_ULBIGINT_UNK:
            sprintf(*buf, "UNIFY_L_BIGINT_ULBIGINT_UNK");
            break;
#endif
        case UNIFY_DBTERM_INSTINIT:
            sprintf(*buf, "UNIFY_DBTERM_INSTINIT");
            break;
        case UNIFY_DBTERM_UDBTERM_NONVAR:
            sprintf(*buf, "UNIFY_DBTERM_UDBTERM_NONVAR");
            break;
        case UNIFY_DBTERM_UDBTERM_UNK:
            sprintf(*buf, "UNIFY_DBTERM_UDBTERM_UNK");
            break;
        case UNIFY_L_DBTERM_INSTINIT:
            sprintf(*buf, "UNIFY_L_DBTERM_INSTINIT");
            break;
        case UNIFY_L_DBTERM_ULDBTERM_NONVAR:
            sprintf(*buf, "UNIFY_L_DBTERM_ULDBTERM_NONVAR");
            break;
        case UNIFY_L_DBTERM_ULDBTERM_UNK:
            sprintf(*buf, "UNIFY_L_DBTERM_ULDBTERM_UNK");
            break;
        case UNIFY_LIST_INSTINIT:
            sprintf(*buf, "UNIFY_LIST_INSTINIT");
            break;
        case UNIFY_LIST_READMODE:
            sprintf(*buf, "UNIFY_LIST_READMODE");
            break;
        case UNIFY_LIST_WRITEMODE:
            sprintf(*buf, "UNIFY_LIST_WRITEMODE");
            break;
        case UNIFY_LIST_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_LIST_WRITE_INSTINIT");
            break;
        case UNIFY_L_LIST_INSTINIT:
            sprintf(*buf, "UNIFY_L_LIST_INSTINIT");
            break;
        case UNIFY_L_LIST_READMODE:
            sprintf(*buf, "UNIFY_L_LIST_READMODE");
            break;
        case UNIFY_L_LIST_WRITEMODE:
            sprintf(*buf, "UNIFY_L_LIST_WRITEMODE");
            break;
        case UNIFY_L_LIST_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_LIST_WRITE_INSTINIT");
            break;
        case UNIFY_STRUCT_INSTINIT:
            sprintf(*buf, "UNIFY_STRUCT_INSTINIT");
            break;
        case UNIFY_STRUCT_READMODE:
            sprintf(*buf, "UNIFY_STRUCT_READMODE");
            break;
        case UNIFY_STRUCT_WRITEMODE:
            sprintf(*buf, "UNIFY_STRUCT_WRITEMODE");
            break;
        case UNIFY_STRUCT_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_STRUCT_WRITE_INSTINIT");
            break;
        case UNIFY_L_STRUC_INSTINIT:
            sprintf(*buf, "UNIFY_L_STRUC_INSTINIT");
            break;
        case UNIFY_L_STRUC_READMODE:
            sprintf(*buf, "UNIFY_L_STRUC_READMODE");
            break;
        case UNIFY_L_STRUC_WRITEMODE:
            sprintf(*buf, "UNIFY_L_STRUC_WRITEMODE");
            break;
        case UNIFY_L_STRUC_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_STRUC_WRITE_INSTINIT");
            break;
        case PUT_X_VAR_INSTINIT:
            sprintf(*buf, "PUT_X_VAR_INSTINIT");
            break;
        case PUT_Y_VAR_INSTINIT:
            sprintf(*buf, "PUT_Y_VAR_INSTINIT");
            break;
        case PUT_X_VAL_INSTINIT:
            sprintf(*buf, "PUT_X_VAL_INSTINIT");
            break;
        case PUT_XX_VAL_INSTINIT:
            sprintf(*buf, "PUT_XX_VAL_INSTINIT");
            break;
        case PUT_Y_VAL_INSTINIT:
            sprintf(*buf, "PUT_Y_VAL_INSTINIT");
            break;
        case PUT_Y_VALS_INSTINIT:
            sprintf(*buf, "PUT_Y_VALS_INSTINIT");
            break;
        case PUT_UNSAFE_INSTINIT:
            sprintf(*buf, "PUT_UNSAFE_INSTINIT");
            break;
        case PUT_UNSAFE_PUNSAFE_NONVAR:
            sprintf(*buf, "PUT_UNSAFE_PUNSAFE_NONVAR");
            break;
        case PUT_UNSAFE_PUNSAFE_UNK:
            sprintf(*buf, "PUT_UNSAFE_PUNSAFE_UNK");
            break;
        case PUT_ATOM_INSTINIT:
            sprintf(*buf, "PUT_ATOM_INSTINIT");
            break;
        case PUT_DBTERM_INSTINIT:
            sprintf(*buf, "PUT_DBTERM_INSTINIT");
            break;
        case PUT_BIGINT_INSTINIT:
            sprintf(*buf, "PUT_BIGINT_INSTINIT");
            break;
        case PUT_FLOAT_INSTINIT:
            sprintf(*buf, "PUT_FLOAT_INSTINIT");
            break;
        case PUT_LONGINT_INSTINIT:
            sprintf(*buf, "PUT_LONGINT_INSTINIT");
            break;
        case PUT_LIST_INSTINIT:
            sprintf(*buf, "PUT_LIST_INSTINIT");
            break;
        case PUT_STRUCT_INSTINIT:
            sprintf(*buf, "PUT_STRUCT_INSTINIT");
            break;
        case WRITE_X_VAR_INSTINIT:
            sprintf(*buf, "WRITE_X_VAR_INSTINIT");
            break;
        case WRITE_VOID_INSTINIT:
            sprintf(*buf, "WRITE_VOID_INSTINIT");
            break;
        case WRITE_N_VOIDS_INSTINIT:
            sprintf(*buf, "WRITE_N_VOIDS_INSTINIT");
            break;
        case WRITE_Y_VAR_INSTINIT:
            sprintf(*buf, "WRITE_Y_VAR_INSTINIT");
            break;
        case WRITE_X_VAL_INSTINIT:
            sprintf(*buf, "WRITE_X_VAL_INSTINIT");
            break;
        case WRITE_X_LOC_INSTINIT:
            sprintf(*buf, "WRITE_X_LOC_INSTINIT");
            break;
        case WRITE_X_LOC_W_X_BOUND:
            sprintf(*buf, "WRITE_X_LOC_W_X_BOUND");
            break;
        case WRITE_X_LOC_W_X_UNK:
            sprintf(*buf, "WRITE_X_LOC_W_X_UNK");
            break;
        case WRITE_Y_VAL_INSTINIT:
            sprintf(*buf, "WRITE_Y_VAL_INSTINIT");
            break;
        case WRITE_Y_LOC_INSTINIT:
            sprintf(*buf, "WRITE_Y_LOC_INSTINIT");
            break;
        case WRITE_Y_LOC_W_Y_BOUND:
            sprintf(*buf, "WRITE_Y_LOC_W_Y_BOUND");
            break;
        case WRITE_Y_LOC_W_Y_UNK:
            sprintf(*buf, "WRITE_Y_LOC_W_Y_UNK");
            break;
        case WRITE_ATOM_INSTINIT:
            sprintf(*buf, "WRITE_ATOM_INSTINIT");
            break;
        case WRITE_BIGINT_INSTINIT:
            sprintf(*buf, "WRITE_BIGINT_INSTINIT");
            break;
        case WRITE_DBTERM_INSTINIT:
            sprintf(*buf, "WRITE_DBTERM_INSTINIT");
            break;
        case WRITE_FLOAT_INSTINIT:
            sprintf(*buf, "WRITE_FLOAT_INSTINIT");
            break;
        case WRITE_LONGIT_INSTINIT:
            sprintf(*buf, "WRITE_LONGIT_INSTINIT");
            break;
        case WRITE_N_ATOMS_INSTINIT:
            sprintf(*buf, "WRITE_N_ATOMS_INSTINIT");
            break;
        case WRITE_LIST_INSTINIT:
            sprintf(*buf, "WRITE_LIST_INSTINIT");
            break;
        case WRITE_L_LIST_INSTINIT:
            sprintf(*buf, "WRITE_L_LIST_INSTINIT");
            break;
        case WRITE_STRUCT_INSTINIT:
            sprintf(*buf, "WRITE_STRUCT_INSTINIT");
            break;
        case WRITE_L_STRUC_INSTINIT:
            sprintf(*buf, "WRITE_L_STRUC_INSTINIT");
            break;
        case SAVE_PAIR_X_INSTINIT:
            sprintf(*buf, "SAVE_PAIR_X_INSTINIT");
            break;
        case SAVE_PAIR_X_WRITE_INSTINIT:
            sprintf(*buf, "SAVE_PAIR_X_WRITE_INSTINIT");
            break;
        case SAVE_PAIR_Y_INSTINIT:
            sprintf(*buf, "SAVE_PAIR_Y_INSTINIT");
            break;
        case SAVE_PAIR_Y_WRITE_INSTINIT:
            sprintf(*buf, "SAVE_PAIR_Y_WRITE_INSTINIT");
            break;
        case SAVE_APPL_X_INSTINIT:
            sprintf(*buf, "SAVE_APPL_X_INSTINIT");
            break;
        case SAVE_APPL_X_WRITE_INSTINIT:
            sprintf(*buf, "SAVE_APPL_X_WRITE_INSTINIT");
            break;
        case SAVE_APPL_Y_INSTINIT:
            sprintf(*buf, "SAVE_APPL_Y_INSTINIT");
            break;
        case SAVE_APPL_Y_WRITE_INSTINIT:
            sprintf(*buf, "SAVE_APPL_Y_WRITE_INSTINIT");
            break;
        case JUMP_INSTINIT:
            sprintf(*buf, "JUMP_INSTINIT");
            break;
        case MOVE_BACK_INSTINIT:
            sprintf(*buf, "MOVE_BACK_INSTINIT");
            break;
        case SKIP_INSTINIT:
            sprintf(*buf, "SKIP_INSTINIT");
            break;
        case EITHER_INSTINIT:
            sprintf(*buf, "EITHER_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case EITHER_LOW_LEVEL_TRACER:
            sprintf(*buf, "EITHER_LOW_LEVEL_TRACER");
            break;
#endif
        case EITHER_POST_COROUTINING:
            sprintf(*buf, "EITHER_POST_COROUTINING");
            break;
        case EITHER_FROZEN_YSBA:
            sprintf(*buf, "EITHER_FROZEN_YSBA");
            break;
        case EITHER_POST_FROZEN_YSBA:
            sprintf(*buf, "EITHER_POST_FROZEN_YSBA");
            break;
#ifdef YAPOR
        case EITHER_YAPOR:
            sprintf(*buf, "EITHER_YAPOR");
            break;
#endif
        case EITHER_END:
            sprintf(*buf, "EITHER_END");
            break;
        case OR_ELSE_INSTINIT:
            sprintf(*buf, "OR_ELSE_INSTINIT");
            break;
#ifdef DEPTH_LIMIT
        case OR_ELSE_DEPTH:
            sprintf(*buf, "OR_ELSE_DEPTH");
            break;
#endif
        case OR_ELSE_POST_DEPTH:
            sprintf(*buf, "OR_ELSE_POST_DEPTH");
            break;
#ifdef YAPOR
        case OR_ELSE_YAPOR:
            sprintf(*buf, "OR_ELSE_YAPOR");
            break;
#endif
        case OR_ELSE_END:
            sprintf(*buf, "OR_ELSE_END");
            break;
        case OR_LAST_INSTINIT:
            sprintf(*buf, "OR_LAST_INSTINIT");
            break;
#ifdef YAPOR
        case OR_LAST_IFOK_INIT:
            sprintf(*buf, "OR_LAST_IFOK_INIT");
            break;
#ifdef DEPTH_LIMIT
        case OR_LAST_IFOK_DEPTH:
            sprintf(*buf, "OR_LAST_IFOK_DEPTH");
            break;
#endif
        case OR_LAST_IFOK_END:
            sprintf(*buf, "OR_LAST_IFOK_END");
            break;
#endif
        case OR_LAST_NOIF_INIT:
            sprintf(*buf, "OR_LAST_NOIF_INIT");
            break;
#ifdef DEPTH_LIMIT
        case OR_LAST_NOIF_DEPTH:
            sprintf(*buf, "OR_LAST_NOIF_DEPTH");
            break;
#endif
        case OR_LAST_NOIF_END:
            sprintf(*buf, "OR_LAST_NOIF_END");
            break;
#ifdef YAPOR
        case OR_LAST_YAPOR:
            sprintf(*buf, "OR_LAST_YAPOR");
            break;
#else
        case OR_LAST_NOYAPOR:
            sprintf(*buf, "OR_LAST_NOYAPOR");
            break;
#endif
        case OR_LAST_END:
            sprintf(*buf, "OR_LAST_END");
            break;
        case POP_N_INSTINIT:
            sprintf(*buf, "POP_N_INSTINIT");
            break;
        case POP_N_END:
            sprintf(*buf, "POP_N_END");
            break;
        case POP_INSTINIT:
            sprintf(*buf, "POP_INSTINIT");
            break;
        case POP_END:
            sprintf(*buf, "POP_END");
            break;
        case CALL_CPRED_INSTINIT:
            sprintf(*buf, "CALL_CPRED_INSTINIT");
            break;
        case CALL_CPRED_TEST_STACK:
            sprintf(*buf, "CALL_CPRED_TEST_STACK");
            break;
#ifdef FROZEN_STACKS
        case CALL_CPRED_FROZEN_INIT:
            sprintf(*buf, "CALL_CPRED_FROZEN_INIT");
            break;
        case CALL_CPRED_TOPB:
            sprintf(*buf, "CALL_CPRED_TOPB");
            break;
#else
        case CALL_CPRED_NOFROZEN:
            sprintf(*buf, "CALL_CPRED_NOFROZEN");
            break;
#endif
#ifdef LOW_LEVEL_TRACER
        case CALL_CPRED_LOW_LEVEL_TRACER:
            sprintf(*buf, "CALL_CPRED_LOW_LEVEL_TRACER");
            break;
#endif
        case CALL_CPRED_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "CALL_CPRED_POST_LOW_LEVEL_TRACER");
            break;
#ifdef SHADOW_S
        case CALL_CPRED_SETSREG:
            sprintf(*buf, "CALL_CPRED_SETSREG");
            break;
#endif
        case CALL_CPRED_END:
            sprintf(*buf, "CALL_CPRED_END");
            break;
        case EXECUTE_CPRED_INSTINIT:
            sprintf(*buf, "EXECUTE_CPRED_INSTINIT");
            break;
        case EXECUTE_CPRED_POST_CHECK_TRAIL:
            sprintf(*buf, "EXECUTE_CPRED_POST_CHECK_TRAIL");
            break;
#ifdef FROZEN_STACKS
        case EXECUTE_CPRED_FROZEN:
            sprintf(*buf, "EXECUTE_CPRED_FROZEN");
            break;
        case EXECUTE_CPRED_TOPB:
            sprintf(*buf, "EXECUTE_CPRED_TOPB");
            break;
#else
        case EXECUTE_CPRED_NOFROZEN:
            sprintf(*buf, "EXECUTE_CPRED_NOFROZEN");
            break;
#endif
        case EXECUTE_CPRED_POST_FROZEN:
            sprintf(*buf, "EXECUTE_CPRED_POST_FROZEN");
            break;
#ifdef LOW_LEVEL_TRACER
        case EXECUTE_CPRED_LOW_LEVEL_TRACER:
            sprintf(*buf, "EXECUTE_CPRED_LOW_LEVEL_TRACER");
            break;
#endif
        case EXECUTE_CPRED_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "EXECUTE_CPRED_POST_LOW_LEVEL_TRACER");
            break;
        case EXECUTE_CPRED_SAVE_PC:
            sprintf(*buf, "EXECUTE_CPRED_SAVE_PC");
            break;
#ifdef DEPTH_LIMIT
        case EXECUTE_CPRED_DEPTH_MINOR:
            sprintf(*buf, "EXECUTE_CPRED_DEPTH_MINOR");
            break;
        case EXECUTE_CPRED_DEPTH_MOFPRED:
            sprintf(*buf, "EXECUTE_CPRED_DEPTH_MOFPRED");
            break;
        case EXECUTE_CPRED_DEPTH_END:
            sprintf(*buf, "EXECUTE_CPRED_DEPTH_END");
            break;
#endif
        case EXECUTE_CPRED_END:
            sprintf(*buf, "EXECUTE_CPRED_END");
            break;
        case CALL_USERCPRED_INSTINIT:
            sprintf(*buf, "CALL_USERCPRED_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case CALL_USERCPRED_LOW_LEVEL_TRACER:
            sprintf(*buf, "CALL_USERCPRED_LOW_LEVEL_TRACER");
            break;
#endif
        case CALL_USERCPRED_FROZEN:
            sprintf(*buf, "CALL_USERCPRED_FROZEN");
            break;
        case CALL_USERCPRED_POST_FROZEN:
            sprintf(*buf, "CALL_USERCPRED_POST_FROZEN");
            break;
        case CALL_USERCPRED_END:
            sprintf(*buf, "CALL_USERCPRED_END");
            break;
        case LOCK_PRED_INSTINIT:
            sprintf(*buf, "LOCK_PRED_INSTINIT");
            break;
        case LOCK_PRED_FIRSTIFOK:
            sprintf(*buf, "LOCK_PRED_FIRSTIFOK");
            break;
        case LOCK_PRED_SECONDTIFOK:
            sprintf(*buf, "LOCK_PRED_SECONDTIFOK");
            break;
        case LOCK_PRED_END:
            sprintf(*buf, "LOCK_PRED_END");
            break;
        case INDEX_PRED_INSTINIT:
            sprintf(*buf, "INDEX_PRED_INSTINIT");
            break;
        case INDEX_PRED_END:
            sprintf(*buf, "INDEX_PRED_END");
            break;
#if THREADS
        case THREAD_LOCAL_INSTINIT:
            sprintf(*buf, "THREAD_LOCAL_INSTINIT");
            break;
#endif
        case EXPAND_INDEX_INSTINIT:
            sprintf(*buf, "EXPAND_INDEX_INSTINIT");
            break;
#if defined(YAPOR) || defined(THREADS)
        case EXPAND_INDEX_YAPOR_THREADS_NOPP:
            sprintf(*buf, "EXPAND_INDEX_YAPOR_THREADS_NOPP");
            break;
        case EXPAND_INDEX_YAPOR_THREADS_IFOK_INIT:
            sprintf(*buf, "EXPAND_INDEX_YAPOR_THREADS_IFOK_INIT");
            break;
        case EXPAND_INDEX_YAPOR_THREADS_IFOK_IFOK:
            sprintf(*buf, "EXPAND_INDEX_YAPOR_THREADS_IFOK_IFOK");
            break;
        case EXPAND_INDEX_YAPOR_THREADS_IFOK_END:
            sprintf(*buf, "EXPAND_INDEX_YAPOR_THREADS_IFOK_END");
            break;
#endif
#ifdef SHADOW_S
        case EXPAND_INDEX_NOYAPOR_NOTHREADS_SETS:
            sprintf(*buf, "EXPAND_INDEX_NOYAPOR_NOTHREADS_SETS");
            break;
#endif
        case EXPAND_INDEX_NOYAPOR_NOTHREADS_POST_SETS:
            sprintf(*buf, "EXPAND_INDEX_NOYAPOR_NOTHREADS_POST_SETS");
            break;
#ifdef SHADOW_S
        case EXPAND_INDEX_NOYAPOR_NOTHREADS_SETSREG:
            sprintf(*buf, "EXPAND_INDEX_NOYAPOR_NOTHREADS_SETSREG");
            break;
#endif
        case EXPAND_INDEX_NOYAPOR_NOTHREADS_POST_SETSREG:
            sprintf(*buf, "EXPAND_INDEX_NOYAPOR_NOTHREADS_POST_SETSREG");
            break;
#if defined(YAPOR) || defined(THREADS)
        case EXPAND_INDEX_UNLOCK:
            sprintf(*buf, "EXPAND_INDEX_UNLOCK");
            break;
#endif
        case EXPAND_INDEX_END:
            sprintf(*buf, "EXPAND_INDEX_END");
            break;
        case EXPAND_CLAUSES_INSTINIT:
            sprintf(*buf, "EXPAND_CLAUSES_INSTINIT");
            break;
#if defined(YAPOR) || defined(THREADS)
        case EXPAND_CLAUSES_YAPOR_THREADS_NOPP:
            sprintf(*buf, "EXPAND_CLAUSES_YAPOR_THREADS_NOPP");
            break;
        case EXPAND_CLAUSES_YAPOR_THREADS_IFOK_INIT:
            sprintf(*buf, "EXPAND_CLAUSES_YAPOR_THREADS_IFOK_INIT");
            break;
        case EXPAND_CLAUSES_YAPOR_THREADS_IFOK_IFOK:
            sprintf(*buf, "EXPAND_CLAUSES_YAPOR_THREADS_IFOK_IFOK");
            break;
        case EXPAND_CLAUSES_YAPOR_THREADS_IFOK_END:
            sprintf(*buf, "EXPAND_CLAUSES_YAPOR_THREADS_IFOK_END");
            break;
#endif
        case EXPAND_CLAUSES_NOYAPOR_NOTHREADS:
            sprintf(*buf, "EXPAND_CLAUSES_NOYAPOR_NOTHREADS");
            break;
#if defined(YAPOR) || defined(THREADS)
        case EXPAND_CLAUSES_UNLOCK:
            sprintf(*buf, "EXPAND_CLAUSES_UNLOCK");
            break;
#endif
        case EXPAND_CLAUSES_END:
            sprintf(*buf, "EXPAND_CLAUSES_END");
            break;
        case UNDEF_P_INSTINIT:
            sprintf(*buf, "UNDEF_P_INSTINIT");
            break;
        case UNDEF_P_END:
            sprintf(*buf, "UNDEF_P_END");
            break;
        case SPY_PRED_INSTINIT:
            sprintf(*buf, "SPY_PRED_INSTINIT");
            break;
        case SPY_PRED_FIRSTIFOK:
            sprintf(*buf, "SPY_PRED_FIRSTIFOK");
            break;
        case SPY_PRED_SECONDIFOK_INIT:
            sprintf(*buf, "SPY_PRED_SECONDIFOK_INIT");
            break;
        case SPY_PRED_SECONDIFOK_FIRSTIFOK:
            sprintf(*buf, "SPY_PRED_SECONDIFOK_FIRSTIFOK");
            break;
        case SPY_PRED_SECONDIFOK_POST_FIRSTIF:
            sprintf(*buf, "SPY_PRED_SECONDIFOK_POST_FIRSTIF");
            break;
        case SPY_PRED_SECONDIFOK_SECONDIFOK:
            sprintf(*buf, "SPY_PRED_SECONDIFOK_SECONDIFOK");
            break;
        case SPY_PRED_SECONDIFOK_THIRDIFOK:
            sprintf(*buf, "SPY_PRED_SECONDIFOK_THIRDIFOK");
            break;
        case SPY_PRED_THIRDIFOK_INIT:
            sprintf(*buf, "SPY_PRED_THIRDIFOK_INIT");
            break;
        case SPY_PRED_THIRDIFOK_FIRSTIFOK:
            sprintf(*buf, "SPY_PRED_THIRDIFOK_FIRSTIFOK");
            break;
        case SPY_PRED_FOURTHIFOK:
            sprintf(*buf, "SPY_PRED_FOURTHIFOK");
            break;
        case SPY_PRED_POST_FOURTHIF:
            sprintf(*buf, "SPY_PRED_POST_FOURTHIF");
            break;
        case SPY_PRED_D0ISZERO:
            sprintf(*buf, "SPY_PRED_D0ISZERO");
            break;
        case SPY_PRED_D0ISNOZERO_INIT:
            sprintf(*buf, "SPY_PRED_D0ISNOZERO_INIT");
            break;
        case SPY_PRED_D0ISNOZERO_INSIDEFOR_INIT:
            sprintf(*buf, "SPY_PRED_D0ISNOZERO_INSIDEFOR_INIT");
            break;
        case SPY_PRED_D0ISNOZERO_INSIDEFOR_DOSPY_NONVAR:
            sprintf(*buf, "SPY_PRED_D0ISNOZERO_INSIDEFOR_DOSPY_NONVAR");
            break;
        case SPY_PRED_D0ISNOZERO_INSIDEFOR_SAFEVAR:
            sprintf(*buf, "SPY_PRED_D0ISNOZERO_INSIDEFOR_SAFEVAR");
            break;
        case SPY_PRED_D0ISNOZERO_INSIDEFOR_UNSAFEVAR:
            sprintf(*buf, "SPY_PRED_D0ISNOZERO_INSIDEFOR_UNSAFEVAR");
            break;
        case SPY_PRED_POST_IFS:
            sprintf(*buf, "SPY_PRED_POST_IFS");
            break;
#ifdef THREADS
        case SPY_PRED_THREADS_LOCK:
            sprintf(*buf, "SPY_PRED_THREADS_LOCK");
            break;
#endif
        case SPY_PRED_POST_LOCK:
            sprintf(*buf, "SPY_PRED_POST_LOCK");
            break;
#ifdef THREADS
        case SPY_PRED_THREADS_UNLOCK:
            sprintf(*buf, "SPY_PRED_THREADS_UNLOCK");
            break;
#endif
        case SPY_PRED_POST_UNLOCK:
            sprintf(*buf, "SPY_PRED_POST_UNLOCK");
            break;
#ifdef LOW_LEVEL_TRACER
        case SPY_PRED_LOW_LEVEL_TRACER:
            sprintf(*buf, "SPY_PRED_LOW_LEVEL_TRACER");
            break;
#endif
        case SPY_PRED_END:
            sprintf(*buf, "SPY_PRED_END");
            break;
        case TRY_CLAUSE_INSTINIT:
            sprintf(*buf, "TRY_CLAUSE_INSTINIT");
            break;
#ifdef YAPOR
        case TRY_CLAUSE_YAPOR:
            sprintf(*buf, "TRY_CLAUSE_YAPOR");
            break;
#endif
        case TRY_CLAUSE_END:
            sprintf(*buf, "TRY_CLAUSE_END");
            break;
        case TRY_CLAUSE2_INSTINIT:
            sprintf(*buf, "TRY_CLAUSE2_INSTINIT");
            break;
#ifdef YAPOR
        case TRY_CLAUSE2_YAPOR:
            sprintf(*buf, "TRY_CLAUSE2_YAPOR");
            break;
#endif
        case TRY_CLAUSE2_END:
            sprintf(*buf, "TRY_CLAUSE2_END");
            break;
        case TRY_CLAUSE3_INSTINIT:
            sprintf(*buf, "TRY_CLAUSE3_INSTINIT");
            break;
#ifdef YAPOR
        case TRY_CLAUSE3_YAPOR:
            sprintf(*buf, "TRY_CLAUSE3_YAPOR");
            break;
#endif
        case TRY_CLAUSE3_END:
            sprintf(*buf, "TRY_CLAUSE3_END");
            break;
        case TRY_CLAUSE4_INSTINIT:
            sprintf(*buf, "TRY_CLAUSE4_INSTINIT");
            break;
#ifdef YAPOR
        case TRY_CLAUSE4_YAPOR:
            sprintf(*buf, "TRY_CLAUSE4_YAPOR");
            break;
#endif
        case TRY_CLAUSE4_END:
            sprintf(*buf, "TRY_CLAUSE4_END");
            break;
        case RETRY_INSTINIT:
            sprintf(*buf, "RETRY_INSTINIT");
            break;
#ifdef FROZEN_STACKS
        case RETRY_FROZEN:
            sprintf(*buf, "RETRY_FROZEN");
            break;
#else
        case RETRY_NOFROZEN:
            sprintf(*buf, "RETRY_NOFROZEN");
            break;
#endif
        case RETRY_END:
            sprintf(*buf, "RETRY_END");
            break;
        case RETRY2_INSTINIT:
            sprintf(*buf, "RETRY2_INSTINIT");
            break;
#ifdef FROZEN_STACKS
        case RETRY2_FROZEN:
            sprintf(*buf, "RETRY2_FROZEN");
            break;
#else
        case RETRY2_NOFROZEN:
            sprintf(*buf, "RETRY2_NOFROZEN");
            break;
#endif
        case RETRY2_END:
            sprintf(*buf, "RETRY2_END");
            break;
        case RETRY3_INSTINIT:
            sprintf(*buf, "RETRY3_INSTINIT");
            break;
#ifdef FROZEN_STACKS
        case RETRY3_FROZEN:
            sprintf(*buf, "RETRY3_FROZEN");
            break;
#else
        case RETRY3_NOFROZEN:
            sprintf(*buf, "RETRY3_NOFROZEN");
            break;
#endif
        case RETRY3_END:
            sprintf(*buf, "RETRY3_END");
            break;
        case RETRY4_INSTINIT:
            sprintf(*buf, "RETRY4_INSTINIT");
            break;
#ifdef FROZEN_STACKS
        case RETRY4_FROZEN:
            sprintf(*buf, "RETRY4_FROZEN");
            break;
#else
        case RETRY4_NOFROZEN:
            sprintf(*buf, "RETRY4_NOFROZEN");
            break;
#endif
        case RETRY4_END:
            sprintf(*buf, "RETRY4_END");
            break;
        case TRUST_INSTINIT:
            sprintf(*buf, "TRUST_INSTINIT");
            break;
#ifdef YAPOR
        case TRUST_IFOK_INIT:
            sprintf(*buf, "TRUST_IFOK_INIT");
            break;
#ifdef FROZEN_STACKS
        case TRUST_IFOK_FROZEN:
            sprintf(*buf, "TRUST_IFOK_FROZEN");
            break;
#endif
        case TRUST_IFOK_END:
            sprintf(*buf, "TRUST_IFOK_END");
            break;
#endif
        case TRUST_NOIF_INIT:
            sprintf(*buf, "TRUST_NOIF_INIT");
            break;
#ifdef FROZEN_STACKS
        case TRUST_NOIF_FROZEN:
            sprintf(*buf, "TRUST_NOIF_FROZEN");
            break;
#endif
        case TRUST_END:
            sprintf(*buf, "TRUST_END");
            break;
        case TRY_IN_INSTINIT:
            sprintf(*buf, "TRY_IN_INSTINIT");
            break;
        case TRY_IN_END:
            sprintf(*buf, "TRY_IN_END");
            break;
        case USER_SWITCH_INSTINIT:
            sprintf(*buf, "USER_SWITCH_INSTINIT");
            break;
        case USER_SWITCH_END:
            sprintf(*buf, "USER_SWITCH_END");
            break;
        case SWITCH_ON_TYPE_INSTINIT:
            sprintf(*buf, "SWITCH_ON_TYPE_INSTINIT");
            break;
        case SWITCH_ON_TYPE_END:
            sprintf(*buf, "SWITCH_ON_TYPE_END");
            break;
        case SWITCH_LIST_NL_INSTINIT:
            sprintf(*buf, "SWITCH_LIST_NL_INSTINIT");
            break;
        case SWITCH_LIST_NL_END:
            sprintf(*buf, "SWITCH_LIST_NL_END");
            break;
        case SWITCH_ON_ARG_TYPE_INSTINIT:
            sprintf(*buf, "SWITCH_ON_ARG_TYPE_INSTINIT");
            break;
        case SWITCH_ON_ARG_TYPE_END:
            sprintf(*buf, "SWITCH_ON_ARG_TYPE_END");
            break;
        case SWITCH_ON_SUB_ARG_TYPE_INSTINIT:
            sprintf(*buf, "SWITCH_ON_SUB_ARG_TYPE_INSTINIT");
            break;
        case SWITCH_ON_SUB_ARG_TYPE_END:
            sprintf(*buf, "SWITCH_ON_SUB_ARG_TYPE_END");
            break;
        case JUMP_IF_VAR_INSTINIT:
            sprintf(*buf, "JUMP_IF_VAR_INSTINIT");
            break;
        case JUMP_IF_VAR_END:
            sprintf(*buf, "JUMP_IF_VAR_END");
            break;
        case JUMP_IF_NONVAR_INSTINIT:
            sprintf(*buf, "JUMP_IF_NONVAR_INSTINIT");
            break;
        case JUMP_IF_NONVAR_END:
            sprintf(*buf, "JUMP_IF_NONVAR_END");
            break;
        case IF_NOT_THEN_INSTINIT:
            sprintf(*buf, "IF_NOT_THEN_INSTINIT");
            break;
        case IF_NOT_THEN_END:
            sprintf(*buf, "IF_NOT_THEN_END");
            break;
        case SWITCH_ON_FUNC_INSTINIT:
            sprintf(*buf, "SWITCH_ON_FUNC_INSTINIT");
            break;
        case SWITCH_ON_FUNC_END:
            sprintf(*buf, "SWITCH_ON_FUNC_END");
            break;
        case SWITCH_ON_CONS_INSTINIT:
            sprintf(*buf, "SWITCH_ON_CONS_INSTINIT");
            break;
        case SWITCH_ON_CONS_END:
            sprintf(*buf, "SWITCH_ON_CONS_END");
            break;
        case GO_ON_FUNC_INSTINIT:
            sprintf(*buf, "GO_ON_FUNC_INSTINIT");
            break;
        case GO_ON_FUNC_END:
            sprintf(*buf, "GO_ON_FUNC_END");
            break;
        case GO_ON_CONS_INSTINIT:
            sprintf(*buf, "GO_ON_CONS_INSTINIT");
            break;
        case GO_ON_CONS_END:
            sprintf(*buf, "GO_ON_CONS_END");
            break;
        case IF_FUNC_INSTINIT:
            sprintf(*buf, "IF_FUNC_INSTINIT");
            break;
        case IF_FUNC_END:
            sprintf(*buf, "IF_FUNC_END");
            break;
        case IF_CONS_INSTINIT:
            sprintf(*buf, "IF_CONS_INSTINIT");
            break;
        case IF_CONS_END:
            sprintf(*buf, "IF_CONS_END");
            break;
        case INDEX_DBREF_INSTINIT:
            sprintf(*buf, "INDEX_DBREF_INSTINIT");
            break;
        case INDEX_DBREF_END:
            sprintf(*buf, "INDEX_DBREF_END");
            break;
        case INDEX_BLOB_INSTINIT:
            sprintf(*buf, "INDEX_BLOB_INSTINIT");
            break;
        case INDEX_BLOB_END:
            sprintf(*buf, "INDEX_BLOB_END");
            break;
        case INDEX_LONG_INSTINIT:
            sprintf(*buf, "INDEX_LONG_INSTINIT");
            break;
        case INDEX_LONG_END:
            sprintf(*buf, "INDEX_LONG_END");
            break;
        case JIT_HANDLER_INSTINIT:
            sprintf(*buf, "JIT_HANDLER_INSTINIT");
            break;
        case P_ATOM_X_INSTINIT:
            sprintf(*buf, "P_ATOM_X_INSTINIT");
            break;
        case P_ATOM_X_ATOM:
            sprintf(*buf, "P_ATOM_X_ATOM");
            break;
        case P_ATOM_X_NOATOM:
            sprintf(*buf, "P_ATOM_X_NOATOM");
            break;
        case P_ATOM_Y_INSTINIT:
            sprintf(*buf, "P_ATOM_Y_INSTINIT");
            break;
        case P_ATOM_Y_IFOK:
            sprintf(*buf, "P_ATOM_Y_IFOK");
            break;
        case P_ATOM_Y_NOIF:
            sprintf(*buf, "P_ATOM_Y_NOIF");
            break;
        case P_ATOM_Y_END:
            sprintf(*buf, "P_ATOM_Y_END");
            break;
        case P_ATOMIC_X_INSTINIT:
            sprintf(*buf, "P_ATOMIC_X_INSTINIT");
            break;
        case P_ATOMIC_X_NONVAR:
            sprintf(*buf, "P_ATOMIC_X_NONVAR");
            break;
        case P_ATOMIC_X_VAR:
            sprintf(*buf, "P_ATOMIC_X_VAR");
            break;
        case P_ATOMIC_X_END:
            sprintf(*buf, "P_ATOMIC_X_END");
            break;
        case P_ATOMIC_Y_INSTINIT:
            sprintf(*buf, "P_ATOMIC_Y_INSTINIT");
            break;
        case P_ATOMIC_Y_NONVAR:
            sprintf(*buf, "P_ATOMIC_Y_NONVAR");
            break;
        case P_ATOMIC_Y_VAR:
            sprintf(*buf, "P_ATOMIC_Y_VAR");
            break;
        case P_ATOMIC_Y_END:
            sprintf(*buf, "P_ATOMIC_Y_END");
            break;
        case P_INTEGER_X_INSTINIT:
            sprintf(*buf, "P_INTEGER_X_INSTINIT");
            break;
        case P_INTEGER_X_INTEGER_X_NVAR_OK:
            sprintf(*buf, "P_INTEGER_X_INTEGER_X_NVAR_OK");
            break;
		case P_INTEGER_X_INTEGER_X_NVAR_NOOK:
            sprintf(*buf, "P_INTEGER_X_INTEGER_X_NVAR_NOOK");
            break;
        case P_INTEGER_X_INTEGER_X_UNK:
            sprintf(*buf, "P_INTEGER_X_INTEGER_X_UNK");
            break;
        case P_INTEGER_Y_INSTINIT:
            sprintf(*buf, "P_INTEGER_Y_INSTINIT");
            break;
        case P_INTEGER_Y_INTEGER_Y_NVAR_OK:
            sprintf(*buf, "P_INTEGER_Y_INTEGER_Y_NVAR_OK");
            break;
        case P_INTEGER_Y_INTEGER_Y_NVAR_NOOK:
            sprintf(*buf, "P_INTEGER_Y_INTEGER_Y_NVAR_NOOK");
            break;
		case P_INTEGER_Y_INTEGER_Y_UNK:
            sprintf(*buf, "P_INTEGER_Y_INTEGER_Y_UNK");
            break;
        case P_NONVAR_X_INSTINIT:
            sprintf(*buf, "P_NONVAR_X_INSTINIT");
            break;
        case P_NONVAR_X_NONVAR:
            sprintf(*buf, "P_NONVAR_X_NONVAR");
            break;
        case P_NONVAR_X_NONONVAR:
            sprintf(*buf, "P_NONVAR_X_NONONVAR");
            break;
        case P_NONVAR_Y_INSTINIT:
            sprintf(*buf, "P_NONVAR_Y_INSTINIT");
            break;
        case P_NONVAR_Y_NONVAR:
            sprintf(*buf, "P_NONVAR_Y_NONVAR");
            break;
        case P_NONVAR_Y_NONONVAR:
            sprintf(*buf, "P_NONVAR_Y_NONONVAR");
            break;
        case P_NUMBER_X_INSTINIT:
            sprintf(*buf, "P_NUMBER_X_INSTINIT");
            break;
        case P_NUMBER_X_INT:
            sprintf(*buf, "P_NUMBER_X_INT");
            break;
        case P_NUMBER_X_FUNCTORINT:
            sprintf(*buf, "P_NUMBER_X_FUNCTORINT");
            break;
        case P_NUMBER_X_FUNCTORDEFAULT:
            sprintf(*buf, "P_NUMBER_X_FUNCTORDEFAULT");
            break;
        case P_NUMBER_X_POST_IF:
            sprintf(*buf, "P_NUMBER_X_POST_IF");
            break;
        case P_NUMBER_X_NUMBER_X_UNK:
            sprintf(*buf, "P_NUMBER_X_NUMBER_X_UNK");
            break;
        case P_NUMBER_Y_INSTINIT:
            sprintf(*buf, "P_NUMBER_Y_INSTINIT");
            break;
        case P_NUMBER_Y_INT:
            sprintf(*buf, "P_NUMBER_Y_INT");
            break;
        case P_NUMBER_Y_FUNCTORINT:
            sprintf(*buf, "P_NUMBER_Y_FUNCTORINT");
            break;
        case P_NUMBER_Y_FUNCTORDEFAULT:
            sprintf(*buf, "P_NUMBER_Y_FUNCTORDEFAULT");
            break;
        case P_NUMBER_Y_POST_IF:
            sprintf(*buf, "P_NUMBER_Y_POST_IF");
            break;
        case P_NUMBER_Y_NUMBER_Y_UNK:
            sprintf(*buf, "P_NUMBER_Y_NUMBER_Y_UNK");
            break;
        case P_VAR_X_INSTINIT:
            sprintf(*buf, "P_VAR_X_INSTINIT");
            break;
        case P_VAR_X_NONVAR:
            sprintf(*buf, "P_VAR_X_NONVAR");
            break;
        case P_VAR_X_VAR:
            sprintf(*buf, "P_VAR_X_VAR");
            break;
        case P_VAR_Y_INSTINIT:
            sprintf(*buf, "P_VAR_Y_INSTINIT");
            break;
        case P_VAR_Y_NONVAR:
            sprintf(*buf, "P_VAR_Y_NONVAR");
            break;
        case P_VAR_Y_VAR:
            sprintf(*buf, "P_VAR_Y_VAR");
            break;
        case P_DB_REF_X_INSTINIT:
            sprintf(*buf, "P_DB_REF_X_INSTINIT");
            break;
        case P_DB_REF_X_DBREF:
            sprintf(*buf, "P_DB_REF_X_DBREF");
            break;
        case P_DB_REF_X_NODBREF:
            sprintf(*buf, "P_DB_REF_X_NODBREF");
            break;
        case P_DB_REF_X_DBREF_X_UNK:
            sprintf(*buf, "P_DB_REF_X_DBREF_X_UNK");
            break;
        case P_DB_REF_Y_INSTINIT:
            sprintf(*buf, "P_DB_REF_Y_INSTINIT");
            break;
        case P_DB_REF_Y_DBREF:
            sprintf(*buf, "P_DB_REF_Y_DBREF");
            break;
        case P_DB_REF_Y_NODBREF:
            sprintf(*buf, "P_DB_REF_Y_NODBREF");
            break;
        case P_DB_REF_Y_DBREF_Y_UNK:
            sprintf(*buf, "P_DB_REF_Y_DBREF_Y_UNK");
            break;
        case P_PRIMITIVE_X_INSTINIT:
            sprintf(*buf, "P_PRIMITIVE_X_INSTINIT");
            break;
        case P_PRIMITIVE_X_PRIMITIVE:
            sprintf(*buf, "P_PRIMITIVE_X_PRIMITIVE");
            break;
        case P_PRIMITIVE_X_NOPRIMITIVE:
            sprintf(*buf, "P_PRIMITIVE_X_NOPRIMITIVE");
            break;
        case P_PRIMITIVE_X_PRIMI_X_UNK:
            sprintf(*buf, "P_PRIMITIVE_X_PRIMI_X_UNK");
            break;
        case P_PRIMITIVE_Y_INSTINIT:
            sprintf(*buf, "P_PRIMITIVE_Y_INSTINIT");
            break;
        case P_PRIMITIVE_Y_PRIMITIVE:
            sprintf(*buf, "P_PRIMITIVE_Y_PRIMITIVE");
            break;
        case P_PRIMITIVE_Y_NOPRIMITIVE:
            sprintf(*buf, "P_PRIMITIVE_Y_NOPRIMITIVE");
            break;
        case P_PRIMITIVE_Y_PRIMI_Y_UNK:
            sprintf(*buf, "P_PRIMITIVE_Y_PRIMI_Y_UNK");
            break;
        case P_COMPOUND_X_INSTINIT:
            sprintf(*buf, "P_COMPOUND_X_INSTINIT");
            break;
        case P_COMPOUND_X_PAIR:
            sprintf(*buf, "P_COMPOUND_X_PAIR");
            break;
        case P_COMPOUND_X_APPL_IFOK:
            sprintf(*buf, "P_COMPOUND_X_APPL_IFOK");
            break;
        case P_COMPOUND_X_APPL:
            sprintf(*buf, "P_COMPOUND_X_APPL");
            break;
        case P_COMPOUND_X_NOAPPL:
            sprintf(*buf, "P_COMPOUND_X_NOAPPL");
            break;
        case P_COMPOUND_X_COMPOUND_X_UNK:
            sprintf(*buf, "P_COMPOUND_X_COMPOUND_X_UNK");
            break;
        case P_COMPOUND_Y_INSTINIT:
            sprintf(*buf, "P_COMPOUND_Y_INSTINIT");
            break;
        case P_COMPOUND_Y_PAIR:
            sprintf(*buf, "P_COMPOUND_Y_PAIR");
            break;
        case P_COMPOUND_Y_APPL_IFOK:
            sprintf(*buf, "P_COMPOUND_Y_APPL_IFOK");
            break;
        case P_COMPOUND_Y_APPL:
            sprintf(*buf, "P_COMPOUND_Y_APPL");
            break;
        case P_COMPOUND_Y_NOAPPL:
            sprintf(*buf, "P_COMPOUND_Y_NOAPPL");
            break;
        case P_COMPOUND_Y_COMPOUND_Y_UNK:
            sprintf(*buf, "P_COMPOUND_Y_COMPOUND_Y_UNK");
            break;
        case P_FLOAT_X_INSTINIT:
            sprintf(*buf, "P_FLOAT_X_INSTINIT");
            break;
        case P_FLOAT_X_FLOAT:
            sprintf(*buf, "P_FLOAT_X_FLOAT");
            break;
        case P_FLOAT_X_POST_IF:
            sprintf(*buf, "P_FLOAT_X_POST_IF");
            break;
        case P_FLOAT_X_FLOAT_X_UNK:
            sprintf(*buf, "P_FLOAT_X_FLOAT_X_UNK");
            break;
        case P_FLOAT_Y_INSTINIT:
            sprintf(*buf, "P_FLOAT_Y_INSTINIT");
            break;
        case P_FLOAT_Y_FLOAT:
            sprintf(*buf, "P_FLOAT_Y_FLOAT");
            break;
        case P_FLOAT_Y_POST_IF:
            sprintf(*buf, "P_FLOAT_Y_POST_IF");
            break;
        case P_FLOAT_Y_FLOAT_Y_UNK:
            sprintf(*buf, "P_FLOAT_Y_FLOAT_Y_UNK");
            break;
        case P_PLUS_VV_INSTINIT:
            sprintf(*buf, "P_PLUS_VV_INSTINIT");
            break;
        case P_PLUS_VV_PLUS_VV_NVAR:
            sprintf(*buf, "P_PLUS_VV_PLUS_VV_NVAR");
            break;
        case P_PLUS_VV_PLUS_VV_NVAR_NVAR_INT:
            sprintf(*buf, "P_PLUS_VV_PLUS_VV_NVAR_NVAR_INT");
            break;
		case P_PLUS_VV_PLUS_VV_NVAR_NVAR_NOINT:
            sprintf(*buf, "P_PLUS_VV_PLUS_VV_NVAR_NVAR_NOINT");
            break;
        case P_PLUS_VV_PLUS_VV_UNK:
            sprintf(*buf, "P_PLUS_VV_PLUS_VV_UNK");
            break;
        case P_PLUS_VV_PLUS_VV_NVAR_UNK:
            sprintf(*buf, "P_PLUS_VV_PLUS_VV_NVAR_UNK");
            break;
        case P_PLUS_VC_INSTINIT:
            sprintf(*buf, "P_PLUS_VC_INSTINIT");
            break;
        case P_PLUS_VC_PLUS_VC_NVAR_INT:
            sprintf(*buf, "P_PLUS_VC_PLUS_VC_NVAR_INT");
            break;
		case P_PLUS_VC_PLUS_VC_NVAR_NOINT:
            sprintf(*buf, "P_PLUS_VC_PLUS_VC_NVAR_NOINT");
            break;
        case P_PLUS_VC_PLUS_VC_UNK:
            sprintf(*buf, "P_PLUS_VC_PLUS_VC_UNK");
            break;
        case P_PLUS_Y_VV_INSTINIT:
            sprintf(*buf, "P_PLUS_Y_VV_INSTINIT");
            break;
        case P_PLUS_Y_VV_PLUS_Y_VV_NVAR:
            sprintf(*buf, "P_PLUS_Y_VV_PLUS_Y_VV_NVAR");
            break;
        case P_PLUS_Y_VV_PLUS_Y_VV_NVAR_NVAR_INT:
            sprintf(*buf, "P_PLUS_Y_VV_PLUS_Y_VV_NVAR_NVAR_INT");
            break;
        case P_PLUS_Y_VV_PLUS_Y_VV_NVAR_NVAR_NOINT:
            sprintf(*buf, "P_PLUS_Y_VV_PLUS_Y_VV_NVAR_NVAR_NOINT");
            break;
        case P_PLUS_Y_VV_PLUS_Y_VV_UNK:
            sprintf(*buf, "P_PLUS_Y_VV_PLUS_Y_VV_UNK");
            break;
        case P_PLUS_Y_VV_PLUS_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_PLUS_Y_VV_PLUS_Y_VV_NVAR_UNK");
            break;
        case P_PLUS_Y_VC_INSTINIT:
            sprintf(*buf, "P_PLUS_Y_VC_INSTINIT");
            break;
        case P_PLUS_Y_VC_PLUS_Y_VC_NVAR_INT:
            sprintf(*buf, "P_PLUS_Y_VC_PLUS_Y_VC_NVAR_INT");
            break;
		case P_PLUS_Y_VC_PLUS_Y_VC_NVAR_NOINT:
            sprintf(*buf, "P_PLUS_Y_VC_PLUS_Y_VC_NVAR_NOINT");
            break;
        case P_PLUS_Y_VC_PLUS_Y_VC_UNK:
            sprintf(*buf, "P_PLUS_Y_VC_PLUS_Y_VC_UNK");
            break;
        case P_MINUS_VV_INSTINIT:
            sprintf(*buf, "P_MINUS_VV_INSTINIT");
            break;
        case P_MINUS_VV_MINUS_VV_NVAR:
            sprintf(*buf, "P_MINUS_VV_MINUS_VV_NVAR");
            break;
        case P_MINUS_VV_MINUS_VV_NVAR_NVAR_INT:
            sprintf(*buf, "P_MINUS_VV_MINUS_VV_NVAR_NVAR_INT");
            break;
		case P_MINUS_VV_MINUS_VV_NVAR_NVAR_NOINT:
            sprintf(*buf, "P_MINUS_VV_MINUS_VV_NVAR_NVAR_NOINT");
            break;
        case P_MINUS_VV_MINUS_VV_UNK:
            sprintf(*buf, "P_MINUS_VV_MINUS_VV_UNK");
            break;
        case P_MINUS_VV_MINUS_VV_NVAR_UNK:
            sprintf(*buf, "P_MINUS_VV_MINUS_VV_NVAR_UNK");
            break;
        case P_MINUS_CV_INSTINIT:
            sprintf(*buf, "P_MINUS_CV_INSTINIT");
            break;
        case P_MINUS_CV_MINUS_CV_NVAR_INT:
            sprintf(*buf, "P_MINUS_CV_MINUS_CV_NVAR_INT");
            break;
        case P_MINUS_CV_MINUS_CV_NVAR_NOINT:
            sprintf(*buf, "P_MINUS_CV_MINUS_CV_NVAR_NOINT");
            break;
        case P_MINUS_CV_MINUS_CV_UNK:
            sprintf(*buf, "P_MINUS_CV_MINUS_CV_UNK");
            break;
        case P_MINUS_Y_VV_INSTINIT:
            sprintf(*buf, "P_MINUS_Y_VV_INSTINIT");
            break;
        case P_MINUS_Y_VV_MINUS_Y_VV_NVAR:
            sprintf(*buf, "P_MINUS_Y_VV_MINUS_Y_VV_NVAR");
            break;
        case P_MINUS_Y_VV_INTTERM:
            sprintf(*buf, "P_MINUS_Y_VV_INTTERM");
            break;
        case P_MINUS_Y_VV_NOINTTERM:
            sprintf(*buf, "P_MINUS_Y_VV_NOINTTERM");
            break;
        case P_MINUS_Y_VV_D0EQUALS0L:
            sprintf(*buf, "P_MINUS_Y_VV_D0EQUALS0L");
            break;
        case P_MINUS_Y_VV_NVAR_END:
            sprintf(*buf, "P_MINUS_Y_VV_NVAR_END");
            break;
        case P_MINUS_Y_VV_MINUS_Y_VV_UNK:
            sprintf(*buf, "P_MINUS_Y_VV_MINUS_Y_VV_UNK");
            break;
        case P_MINUS_Y_VV_MINUS_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_MINUS_Y_VV_MINUS_Y_VV_NVAR_UNK");
            break;
        case P_MINUS_Y_CV_INSTINIT:
            sprintf(*buf, "P_MINUS_Y_CV_INSTINIT");
            break;
        case P_MINUS_Y_CV_MINUS_Y_CV_NVAR:
            sprintf(*buf, "P_MINUS_Y_CV_MINUS_Y_CV_NVAR");
            break;
        case P_MINUS_Y_CV_INTTERM:
            sprintf(*buf, "P_MINUS_Y_CV_INTTERM");
            break;
        case P_MINUS_Y_CV_NOINTTERM:
            sprintf(*buf, "P_MINUS_Y_CV_NOINTTERM");
            break;
        case P_MINUS_Y_CV_D0EQUALS0L:
            sprintf(*buf, "P_MINUS_Y_CV_D0EQUALS0L");
            break;
        case P_MINUS_Y_CV_NVAR_END:
            sprintf(*buf, "P_MINUS_Y_CV_NVAR_END");
            break;
        case P_MINUS_Y_CV_MINUS_Y_CV_UNK:
            sprintf(*buf, "P_MINUS_Y_CV_MINUS_Y_CV_UNK");
            break;
        case P_TIMES_VV_INSTINIT:
            sprintf(*buf, "P_TIMES_VV_INSTINIT");
            break;
        case P_TIMES_VV_TIMES_VV_NVAR:
            sprintf(*buf, "P_TIMES_VV_TIMES_VV_NVAR");
            break;
        case P_TIMES_VV_TIMES_VV_NVAR_NVAR_INT:
            sprintf(*buf, "P_TIMES_VV_TIMES_VV_NVAR_NVAR_INT");
            break;
		case P_TIMES_VV_TIMES_VV_NVAR_NVAR_NOINT:
            sprintf(*buf, "P_TIMES_VV_TIMES_VV_NVAR_NVAR_NOINT");
            break;
        case P_TIMES_VV_TIMES_VV_UNK:
            sprintf(*buf, "P_TIMES_VV_TIMES_VV_UNK");
            break;
        case P_TIMES_VV_TIMES_VV_NVAR_UNK:
            sprintf(*buf, "P_TIMES_VV_TIMES_VV_NVAR_UNK");
            break;
        case P_TIMES_VC_INSTINIT:
            sprintf(*buf, "P_TIMES_VC_INSTINIT");
            break;
        case P_TIMES_VC_TIMES_VC_NVAR_INT:
            sprintf(*buf, "P_TIMES_VC_TIMES_VC_NVAR_INT");
            break;
		case P_TIMES_VC_TIMES_VC_NVAR_NOINT:
            sprintf(*buf, "P_TIMES_VC_TIMES_VC_NVAR_NOINT");
            break;
        case P_TIMES_VC_TIMES_VC_UNK:
            sprintf(*buf, "P_TIMES_VC_TIMES_VC_UNK");
            break;
        case P_TIMES_Y_VV_INSTINIT:
            sprintf(*buf, "P_TIMES_Y_VV_INSTINIT");
            break;
        case P_TIMES_Y_VV_TIMES_Y_VV_NVAR:
            sprintf(*buf, "P_TIMES_Y_VV_TIMES_Y_VV_NVAR");
            break;
        case P_TIMES_Y_VV_INTTERM:
            sprintf(*buf, "P_TIMES_Y_VV_INTTERM");
            break;
        case P_TIMES_Y_VV_NOINTTERM:
            sprintf(*buf, "P_TIMES_Y_VV_NOINTTERM");
            break;
        case P_TIMES_Y_VV_D0EQUALS0L:
            sprintf(*buf, "P_TIMES_Y_VV_D0EQUALS0L");
            break;
        case P_TIMES_Y_VV_NVAR_END:
            sprintf(*buf, "P_TIMES_Y_VV_NVAR_END");
            break;
        case P_TIMES_Y_VV_TIMES_Y_VV_UNK:
            sprintf(*buf, "P_TIMES_Y_VV_TIMES_Y_VV_UNK");
            break;
        case P_TIMES_Y_VV_TIMES_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_TIMES_Y_VV_TIMES_Y_VV_NVAR_UNK");
            break;
        case P_TIMES_Y_VC_INSTINIT:
            sprintf(*buf, "P_TIMES_Y_VC_INSTINIT");
            break;
        case P_TIMES_Y_VC_TIMES_Y_VC_NVAR_INT:
            sprintf(*buf, "P_TIMES_Y_VC_TIMES_Y_VC_NVAR_INT");
            break;
		case P_TIMES_Y_VC_TIMES_Y_VC_NVAR_NOINT:
            sprintf(*buf, "P_TIMES_Y_VC_TIMES_Y_VC_NVAR_NOINT");
            break;
        case P_TIMES_Y_VC_NVAR_END:
            sprintf(*buf, "P_TIMES_Y_VC_NVAR_END");
            break;
        case P_TIMES_Y_VC_TIMES_Y_VC_UNK:
            sprintf(*buf, "P_TIMES_Y_VC_TIMES_Y_VC_UNK");
            break;
        case P_DIV_VV_INSTINIT:
            sprintf(*buf, "P_DIV_VV_INSTINIT");
            break;
        case P_DIV_VV_DIV_VV_NVAR:
            sprintf(*buf, "P_DIV_VV_DIV_VV_NVAR");
            break;
        case P_DIV_VV_DIV_VV_NVAR_NVAR_INT:
            sprintf(*buf, "P_DIV_VV_DIV_VV_NVAR_NVAR_INT");
            break;
		case P_DIV_VV_DIV_VV_NVAR_NVAR_NOINT:
            sprintf(*buf, "P_DIV_VV_DIV_VV_NVAR_NVAR_NOINT");
            break;
        case P_DIV_VV_DIV_VV_UNK:
            sprintf(*buf, "P_DIV_VV_DIV_VV_UNK");
            break;
        case P_DIV_VV_DIV_VV_NVAR_UNK:
            sprintf(*buf, "P_DIV_VV_DIV_VV_NVAR_UNK");
            break;
        case P_DIV_VC_INSTINIT:
            sprintf(*buf, "P_DIV_VC_INSTINIT");
            break;
        case P_DIV_VC_DIV_VC_NVAR:
            sprintf(*buf, "P_DIV_VC_DIV_VC_NVAR");
            break;
        case P_DIV_VC_INTTERM:
            sprintf(*buf, "P_DIV_VC_INTTERM");
            break;
        case P_DIV_VC_NOINTTERM:
            sprintf(*buf, "P_DIV_VC_NOINTTERM");
            break;
        case P_DIV_VC_D0EQUALS0L:
            sprintf(*buf, "P_DIV_VC_D0EQUALS0L");
            break;
        case P_DIV_VC_NVAR_END:
            sprintf(*buf, "P_DIV_VC_NVAR_END");
            break;
        case P_DIV_VC_DIV_VC_UNK:
            sprintf(*buf, "P_DIV_VC_DIV_VC_UNK");
            break;
        case P_DIV_CV_INSTINIT:
            sprintf(*buf, "P_DIV_CV_INSTINIT");
            break;
        case P_DIV_CV_DIV_CV_NVAR:
            sprintf(*buf, "P_DIV_CV_DIV_CV_NVAR");
            break;
        case P_DIV_CV_INTTERM_INIT:
            sprintf(*buf, "P_DIV_CV_INTTERM_INIT");
            break;
        case P_DIV_CV_INTTERM_DIVEQUALS0:
            sprintf(*buf, "P_DIV_CV_INTTERM_DIVEQUALS0");
            break;
        case P_DIV_CV_INTTERM_END:
            sprintf(*buf, "P_DIV_CV_INTTERM_END");
            break;
        case P_DIV_CV_NOINTTERM:
            sprintf(*buf, "P_DIV_CV_NOINTTERM");
            break;
        case P_DIV_CV_D0EQUALS0L:
            sprintf(*buf, "P_DIV_CV_D0EQUALS0L");
            break;
        case P_DIV_CV_NVAR_END:
            sprintf(*buf, "P_DIV_CV_NVAR_END");
            break;
        case P_DIV_CV_DIV_CV_UNK:
            sprintf(*buf, "P_DIV_CV_DIV_CV_UNK");
            break;
        case P_DIV_Y_VV_INSTINIT:
            sprintf(*buf, "P_DIV_Y_VV_INSTINIT");
            break;
        case P_DIV_Y_VV_DIV_Y_VV_NVAR:
            sprintf(*buf, "P_DIV_Y_VV_DIV_Y_VV_NVAR");
            break;
        case P_DIV_Y_VV_INTTERM_INIT:
            sprintf(*buf, "P_DIV_Y_VV_INTTERM_INIT");
            break;
        case P_DIV_Y_VV_INTTERM_DIVEQUALS0:
            sprintf(*buf, "P_DIV_Y_VV_INTTERM_DIVEQUALS0");
            break;
        case P_DIV_Y_VV_INTTERM_END:
            sprintf(*buf, "P_DIV_Y_VV_INTTERM_END");
            break;
        case P_DIV_Y_VV_NOINTTERM:
            sprintf(*buf, "P_DIV_Y_VV_NOINTTERM");
            break;
        case P_DIV_Y_VV_D0EQUALS0L:
            sprintf(*buf, "P_DIV_Y_VV_D0EQUALS0L");
            break;
        case P_DIV_Y_VV_NVAR_END:
            sprintf(*buf, "P_DIV_Y_VV_NVAR_END");
            break;
        case P_DIV_Y_VV_DIV_Y_VV_UNK:
            sprintf(*buf, "P_DIV_Y_VV_DIV_Y_VV_UNK");
            break;
        case P_DIV_Y_VV_DIV_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_DIV_Y_VV_DIV_Y_VV_NVAR_UNK");
            break;
        case P_DIV_Y_VC_INSTINIT:
            sprintf(*buf, "P_DIV_Y_VC_INSTINIT");
            break;
        case P_DIV_Y_VC_DIV_Y_VC_NVAR:
            sprintf(*buf, "P_DIV_Y_VC_DIV_Y_VC_NVAR");
            break;
        case P_DIV_Y_VC_INTTERM:
            sprintf(*buf, "P_DIV_Y_VC_INTTERM");
            break;
        case P_DIV_Y_VC_NOINTTERM:
            sprintf(*buf, "P_DIV_Y_VC_NOINTTERM");
            break;
        case P_DIV_Y_VC_D0EQUALS0L:
            sprintf(*buf, "P_DIV_Y_VC_D0EQUALS0L");
            break;
        case P_DIV_Y_VC_NVAR_END:
            sprintf(*buf, "P_DIV_Y_VC_NVAR_END");
            break;
        case P_DIV_Y_VC_DIV_Y_VC_UNK:
            sprintf(*buf, "P_DIV_Y_VC_DIV_Y_VC_UNK");
            break;
        case P_DIV_Y_CV_INSTINIT:
            sprintf(*buf, "P_DIV_Y_CV_INSTINIT");
            break;
        case P_DIV_Y_CV_DIV_Y_CV_NVAR:
            sprintf(*buf, "P_DIV_Y_CV_DIV_Y_CV_NVAR");
            break;
        case P_DIV_Y_CV_INTTERM_INIT:
            sprintf(*buf, "P_DIV_Y_CV_INTTERM_INIT");
            break;
        case P_DIV_Y_CV_INTTERM_DIVEQUALS0:
            sprintf(*buf, "P_DIV_Y_CV_INTTERM_DIVEQUALS0");
            break;
        case P_DIV_Y_CV_INTTERM_END:
            sprintf(*buf, "P_DIV_Y_CV_INTTERM_END");
            break;
        case P_DIV_Y_CV_NOINTTERM:
            sprintf(*buf, "P_DIV_Y_CV_NOINTTERM");
            break;
        case P_DIV_Y_CV_D0EQUALS0L:
            sprintf(*buf, "P_DIV_Y_CV_D0EQUALS0L");
            break;
        case P_DIV_Y_CV_NVAR_END:
            sprintf(*buf, "P_DIV_Y_CV_NVAR_END");
            break;
        case P_DIV_Y_CV_DIV_Y_CV_UNK:
            sprintf(*buf, "P_DIV_Y_CV_DIV_Y_CV_UNK");
            break;
        case P_AND_VV_INSTINIT:
            sprintf(*buf, "P_AND_VV_INSTINIT");
            break;
        case P_AND_VV_AND_VV_NVAR:
            sprintf(*buf, "P_AND_VV_AND_VV_NVAR");
            break;
        case P_AND_VV_AND_VV_NVAR_NVAR_INT:
            sprintf(*buf, "P_AND_VV_AND_VV_NVAR_NVAR_INT");
            break;
		case P_AND_VV_AND_VV_NVAR_NVAR_NOINT:
            sprintf(*buf, "P_AND_VV_AND_VV_NVAR_NVAR_NOINT");
            break;
        case P_AND_VV_AND_VV_UNK:
            sprintf(*buf, "P_AND_VV_AND_VV_UNK");
            break;
        case P_AND_VV_AND_VV_NVAR_UNK:
            sprintf(*buf, "P_AND_VV_AND_VV_NVAR_UNK");
            break;
        case P_AND_VC_INSTINIT:
            sprintf(*buf, "P_AND_VC_INSTINIT");
            break;
        case P_AND_VC_AND_VC_NVAR_INT:
            sprintf(*buf, "P_AND_VC_AND_VC_NVAR_INT");
            break;
		case P_AND_VC_AND_VC_NVAR_NOINT:
            sprintf(*buf, "P_AND_VC_AND_VC_NVAR_NOINT");
            break;
        case P_AND_VC_AND_VC_UNK:
            sprintf(*buf, "P_AND_VC_AND_VC_UNK");
            break;
        case P_AND_Y_VV_INSTINIT:
            sprintf(*buf, "P_AND_Y_VV_INSTINIT");
            break;
        case P_AND_Y_VV_AND_Y_VV_NVAR:
            sprintf(*buf, "P_AND_Y_VV_AND_Y_VV_NVAR");
            break;
        case P_AND_Y_VV_INTTERM:
            sprintf(*buf, "P_AND_Y_VV_INTTERM");
            break;
        case P_AND_Y_VV_NOINTTERM:
            sprintf(*buf, "P_AND_Y_VV_NOINTTERM");
            break;
        case P_AND_Y_VV_D0EQUALS0L:
            sprintf(*buf, "P_AND_Y_VV_D0EQUALS0L");
            break;
        case P_AND_Y_VV_NVAR_END:
            sprintf(*buf, "P_AND_Y_VV_NVAR_END");
            break;
        case P_AND_Y_VV_AND_Y_VV_UNK:
            sprintf(*buf, "P_AND_Y_VV_AND_Y_VV_UNK");
            break;
        case P_AND_Y_VV_AND_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_AND_Y_VV_AND_Y_VV_NVAR_UNK");
            break;
        case P_AND_Y_VC_INSTINIT:
            sprintf(*buf, "P_AND_Y_VC_INSTINIT");
            break;
        case P_AND_Y_VC_AND_Y_VC_NVAR:
            sprintf(*buf, "P_AND_Y_VC_AND_Y_VC_NVAR");
            break;
        case P_AND_Y_VC_INTTERM:
            sprintf(*buf, "P_AND_Y_VC_INTTERM");
            break;
        case P_AND_Y_VC_NOINTTERM:
            sprintf(*buf, "P_AND_Y_VC_NOINTTERM");
            break;
        case P_AND_Y_VC_D0EQUALS0L:
            sprintf(*buf, "P_AND_Y_VC_D0EQUALS0L");
            break;
        case P_AND_Y_VC_NVAR_END:
            sprintf(*buf, "P_AND_Y_VC_NVAR_END");
            break;
        case P_AND_Y_VC_AND_Y_VC_UNK:
            sprintf(*buf, "P_AND_Y_VC_AND_Y_VC_UNK");
            break;
        case P_OR_VV_INSTINIT:
            sprintf(*buf, "P_OR_VV_INSTINIT");
            break;
        case P_OR_VV_OR_VV_NVAR:
            sprintf(*buf, "P_OR_VV_OR_VV_NVAR");
            break;
        case P_OR_VV_INTTERM:
            sprintf(*buf, "P_OR_VV_INTTERM");
            break;
        case P_OR_VV_NOINTTERM:
            sprintf(*buf, "P_OR_VV_NOINTTERM");
            break;
        case P_OR_VV_D0EQUALS0L:
            sprintf(*buf, "P_OR_VV_D0EQUALS0L");
            break;
        case P_OR_VV_NVAR_END:
            sprintf(*buf, "P_OR_VV_NVAR_END");
            break;
        case P_OR_VV_OR_VV_UNK:
            sprintf(*buf, "P_OR_VV_OR_VV_UNK");
            break;
        case P_OR_VV_OR_VV_NVAR_UNK:
            sprintf(*buf, "P_OR_VV_OR_VV_NVAR_UNK");
            break;
        case P_OR_VC_INSTINIT:
            sprintf(*buf, "P_OR_VC_INSTINIT");
            break;
        case P_OR_VC_OR_VC_NVAR:
            sprintf(*buf, "P_OR_VC_OR_VC_NVAR");
            break;
        case P_OR_VC_INTTERM:
            sprintf(*buf, "P_OR_VC_INTTERM");
            break;
        case P_OR_VC_NOINTTERM:
            sprintf(*buf, "P_OR_VC_NOINTTERM");
            break;
        case P_OR_VC_D0EQUALS0L:
            sprintf(*buf, "P_OR_VC_D0EQUALS0L");
            break;
        case P_OR_VC_NVAR_END:
            sprintf(*buf, "P_OR_VC_NVAR_END");
            break;
        case P_OR_VC_OR_VC_UNK:
            sprintf(*buf, "P_OR_VC_OR_VC_UNK");
            break;
        case P_OR_Y_VV_INSTINIT:
            sprintf(*buf, "P_OR_Y_VV_INSTINIT");
            break;
        case P_OR_Y_VV_OR_Y_VV_NVAR:
            sprintf(*buf, "P_OR_Y_VV_OR_Y_VV_NVAR");
            break;
        case P_OR_Y_VV_INTTERM:
            sprintf(*buf, "P_OR_Y_VV_INTTERM");
            break;
        case P_OR_Y_VV_NOINTTERM:
            sprintf(*buf, "P_OR_Y_VV_NOINTTERM");
            break;
        case P_OR_Y_VV_D0EQUALS0L:
            sprintf(*buf, "P_OR_Y_VV_D0EQUALS0L");
            break;
        case P_OR_Y_VV_NVAR_END:
            sprintf(*buf, "P_OR_Y_VV_NVAR_END");
            break;
        case P_OR_Y_VV_OR_Y_VV_UNK:
            sprintf(*buf, "P_OR_Y_VV_OR_Y_VV_UNK");
            break;
        case P_OR_Y_VV_OR_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_OR_Y_VV_OR_Y_VV_NVAR_UNK");
            break;
        case P_OR_Y_VC_INSTINIT:
            sprintf(*buf, "P_OR_Y_VC_INSTINIT");
            break;
        case P_OR_Y_VC_OR_Y_VC_NVAR:
            sprintf(*buf, "P_OR_Y_VC_OR_Y_VC_NVAR");
            break;
        case P_OR_Y_VC_INTTERM:
            sprintf(*buf, "P_OR_Y_VC_INTTERM");
            break;
        case P_OR_Y_VC_NOINTTERM:
            sprintf(*buf, "P_OR_Y_VC_NOINTTERM");
            break;
        case P_OR_Y_VC_D0EQUALS0L:
            sprintf(*buf, "P_OR_Y_VC_D0EQUALS0L");
            break;
        case P_OR_Y_VC_NVAR_END:
            sprintf(*buf, "P_OR_Y_VC_NVAR_END");
            break;
        case P_OR_Y_VC_OR_Y_VC_UNK:
            sprintf(*buf, "P_OR_Y_VC_OR_Y_VC_UNK");
            break;
        case P_SLL_VV_INSTINIT:
            sprintf(*buf, "P_SLL_VV_INSTINIT");
            break;
        case P_SLL_VV_SLL_VV_NVAR:
            sprintf(*buf, "P_SLL_VV_SLL_VV_NVAR");
            break;
        case P_SLL_VV_INTTERM_INIT:
            sprintf(*buf, "P_SLL_VV_INTTERM_INIT");
            break;
        case P_SLL_VV_INTTERM_LESS:
            sprintf(*buf, "P_SLL_VV_INTTERM_LESS");
            break;
        case P_SLL_VV_INTTERM_GREATER:
            sprintf(*buf, "P_SLL_VV_INTTERM_GREATER");
            break;
        case P_SLL_VV_NOINTTERM:
            sprintf(*buf, "P_SLL_VV_NOINTTERM");
            break;
        case P_SLL_VV_D0EQUALS0L:
            sprintf(*buf, "P_SLL_VV_D0EQUALS0L");
            break;
        case P_SLL_VV_NVAR_END:
            sprintf(*buf, "P_SLL_VV_NVAR_END");
            break;
        case P_SLL_VV_SLL_VV_UNK:
            sprintf(*buf, "P_SLL_VV_SLL_VV_UNK");
            break;
        case P_SLL_VV_SLL_VV_NVAR_UNK:
            sprintf(*buf, "P_SLL_VV_SLL_VV_NVAR_UNK");
            break;
        case P_SLL_VC_INSTINIT:
            sprintf(*buf, "P_SLL_VC_INSTINIT");
            break;
        case P_SLL_VC_SLL_VC_NVAR:
            sprintf(*buf, "P_SLL_VC_SLL_VC_NVAR");
            break;
        case P_SLL_VC_INTTERM:
            sprintf(*buf, "P_SLL_VC_INTTERM");
            break;
        case P_SLL_VC_NOINTTERM:
            sprintf(*buf, "P_SLL_VC_NOINTTERM");
            break;
        case P_SLL_VC_D0EQUALS0L:
            sprintf(*buf, "P_SLL_VC_D0EQUALS0L");
            break;
        case P_SLL_VC_NVAR_END:
            sprintf(*buf, "P_SLL_VC_NVAR_END");
            break;
        case P_SLL_VC_SLL_VC_UNK:
            sprintf(*buf, "P_SLL_VC_SLL_VC_UNK");
            break;
        case P_SLL_CV_INSTINIT:
            sprintf(*buf, "P_SLL_CV_INSTINIT");
            break;
        case P_SLL_CV_SLL_CV_NVAR_INT:
            sprintf(*buf, "P_SLL_CV_SLL_CV_NVAR_INT");
            break;
		case P_SLL_CV_SLL_CV_NVAR_NOINT:
            sprintf(*buf, "P_SLL_CV_SLL_CV_NVAR_NOINT");
            break;
        case P_SLL_CV_SLL_CV_UNK:
            sprintf(*buf, "P_SLL_CV_SLL_CV_UNK");
            break;
        case P_SLL_Y_VV_INSTINIT:
            sprintf(*buf, "P_SLL_Y_VV_INSTINIT");
            break;
        case P_SLL_Y_VV_SLL_Y_VV_NVAR:
            sprintf(*buf, "P_SLL_Y_VV_SLL_Y_VV_NVAR");
            break;
        case P_SLL_Y_VV_INTTERM_INIT:
            sprintf(*buf, "P_SLL_Y_VV_INTTERM_INIT");
            break;
        case P_SLL_Y_VV_INTERM_LESS:
            sprintf(*buf, "P_SLL_Y_VV_INTERM_LESS");
            break;
        case P_SLL_Y_VV_INTTERM_GREATER:
            sprintf(*buf, "P_SLL_Y_VV_INTTERM_GREATER");
            break;
        case P_SLL_Y_VV_NOINTTERM:
            sprintf(*buf, "P_SLL_Y_VV_NOINTTERM");
            break;
        case P_SLL_Y_VV_D0EQUALS0L:
            sprintf(*buf, "P_SLL_Y_VV_D0EQUALS0L");
            break;
        case P_SLL_Y_VV_NVAR_END:
            sprintf(*buf, "P_SLL_Y_VV_NVAR_END");
            break;
        case P_SLL_Y_VV_SLL_Y_VV_UNK:
            sprintf(*buf, "P_SLL_Y_VV_SLL_Y_VV_UNK");
            break;
        case P_SLL_Y_VV_SLL_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_SLL_Y_VV_SLL_Y_VV_NVAR_UNK");
            break;
        case P_SLL_Y_VC_INSTINIT:
            sprintf(*buf, "P_SLL_Y_VC_INSTINIT");
            break;
        case P_SLL_Y_VC_SLL_Y_VC_NVAR:
            sprintf(*buf, "P_SLL_Y_VC_SLL_Y_VC_NVAR");
            break;
        case P_SLL_Y_VC_INTTERM:
            sprintf(*buf, "P_SLL_Y_VC_INTTERM");
            break;
        case P_SLL_Y_VC_NOINTTERM:
            sprintf(*buf, "P_SLL_Y_VC_NOINTTERM");
            break;
        case P_SLL_Y_VC_D0EQUALS0L:
            sprintf(*buf, "P_SLL_Y_VC_D0EQUALS0L");
            break;
        case P_SLL_Y_VC_NVAR_END:
            sprintf(*buf, "P_SLL_Y_VC_NVAR_END");
            break;
        case P_SLL_Y_VC_SLL_Y_VC_UNK:
            sprintf(*buf, "P_SLL_Y_VC_SLL_Y_VC_UNK");
            break;
        case P_SLL_Y_CV_INSTINIT:
            sprintf(*buf, "P_SLL_Y_CV_INSTINIT");
            break;
        case P_SLL_Y_CV_SLL_Y_CV_NVAR:
            sprintf(*buf, "P_SLL_Y_CV_SLL_Y_CV_NVAR");
            break;
        case P_SLL_Y_CV_INTTERM_INIT:
            sprintf(*buf, "P_SLL_Y_CV_INTTERM_INIT");
            break;
        case P_SLL_Y_CV_INTTERM_LESS:
            sprintf(*buf, "P_SLL_Y_CV_INTTERM_LESS");
            break;
        case P_SLL_Y_CV_INTTERM_GREATER:
            sprintf(*buf, "P_SLL_Y_CV_INTTERM_GREATER");
            break;
        case P_SLL_Y_CV_NOINTTERM:
            sprintf(*buf, "P_SLL_Y_CV_NOINTTERM");
            break;
        case P_SLL_Y_CV_D0EQUALS0L:
            sprintf(*buf, "P_SLL_Y_CV_D0EQUALS0L");
            break;
        case P_SLL_Y_CV_NVAR_END:
            sprintf(*buf, "P_SLL_Y_CV_NVAR_END");
            break;
        case P_SLL_Y_CV_SLL_Y_CV_UNK:
            sprintf(*buf, "P_SLL_Y_CV_SLL_Y_CV_UNK");
            break;
        case P_SLR_VV_INSTINIT:
            sprintf(*buf, "P_SLR_VV_INSTINIT");
            break;
        case P_SLR_VV_SLR_VV_NVAR:
            sprintf(*buf, "P_SLR_VV_SLR_VV_NVAR");
            break;
        case P_SLR_VV_INTTERM_INIT:
            sprintf(*buf, "P_SLR_VV_INTTERM_INIT");
            break;
        case P_SLR_VV_INTTERM_LESS:
            sprintf(*buf, "P_SLR_VV_INTTERM_LESS");
            break;
        case P_SLR_VV_INTTERM_GREATER:
            sprintf(*buf, "P_SLR_VV_INTTERM_GREATER");
            break;
        case P_SLR_VV_NOINTTERM:
            sprintf(*buf, "P_SLR_VV_NOINTTERM");
            break;
        case P_SLR_VV_D0EQUALS0L:
            sprintf(*buf, "P_SLR_VV_D0EQUALS0L");
            break;
        case P_SLR_VV_NVAR_END:
            sprintf(*buf, "P_SLR_VV_NVAR_END");
            break;
        case P_SLR_VV_SRL_VV_UNK:
            sprintf(*buf, "P_SLR_VV_SRL_VV_UNK");
            break;
        case P_SLR_VV_SRL_VV_NVAR_UNK:
            sprintf(*buf, "P_SLR_VV_SRL_VV_NVAR_UNK");
            break;
        case P_SLR_VC_INSTINIT:
            sprintf(*buf, "P_SLR_VC_INSTINIT");
            break;
        case P_SLR_VC_SLR_VC_NVAR_INT:
            sprintf(*buf, "P_SLR_VC_SLR_VC_NVAR_INT");
            break;
		case P_SLR_VC_SLR_VC_NVAR_NOINT:
            sprintf(*buf, "P_SLR_VC_SLR_VC_NVAR_NOINT");
            break;
        case P_SLR_VC_SRL_VC_UNK:
            sprintf(*buf, "P_SLR_VC_SRL_VC_UNK");
            break;
        case P_SLR_CV_INSTINIT:
            sprintf(*buf, "P_SLR_CV_INSTINIT");
            break;
        case P_SLR_CV_SLR_CV_NVAR:
            sprintf(*buf, "P_SLR_CV_SLR_CV_NVAR");
            break;
        case P_SLR_CV_INTTERM_INIT:
            sprintf(*buf, "P_SLR_CV_INTTERM_INIT");
            break;
        case P_SLR_CV_INTTERM_LESS:
            sprintf(*buf, "P_SLR_CV_INTTERM_LESS");
            break;
        case P_SLR_CV_INTTERM_GREATER:
            sprintf(*buf, "P_SLR_CV_INTTERM_GREATER");
            break;
        case P_SLR_CV_NOINTTERM:
            sprintf(*buf, "P_SLR_CV_NOINTTERM");
            break;
        case P_SLR_CV_D0EQUALS0L:
            sprintf(*buf, "P_SLR_CV_D0EQUALS0L");
            break;
        case P_SLR_CV_NVAR_END:
            sprintf(*buf, "P_SLR_CV_NVAR_END");
            break;
        case P_SLR_CV_SLR_CV_UNK:
            sprintf(*buf, "P_SLR_CV_SLR_CV_UNK");
            break;
        case P_SLR_Y_VV_INSTINIT:
            sprintf(*buf, "P_SLR_Y_VV_INSTINIT");
            break;
        case P_SLR_Y_VV_SLR_Y_VV_NVAR:
            sprintf(*buf, "P_SLR_Y_VV_SLR_Y_VV_NVAR");
            break;
        case P_SLR_Y_VV_INTTERM_INIT:
            sprintf(*buf, "P_SLR_Y_VV_INTTERM_INIT");
            break;
        case P_SLR_Y_VV_INTTERM_LESS:
            sprintf(*buf, "P_SLR_Y_VV_INTTERM_LESS");
            break;
        case P_SLR_Y_VV_INTTERM_GREATER:
            sprintf(*buf, "P_SLR_Y_VV_INTTERM_GREATER");
            break;
        case P_SLR_Y_VV_NOINTTERM:
            sprintf(*buf, "P_SLR_Y_VV_NOINTTERM");
            break;
        case P_SLR_Y_VV_D0EQUALS0L:
            sprintf(*buf, "P_SLR_Y_VV_D0EQUALS0L");
            break;
        case P_SLR_Y_VV_NVAR_END:
            sprintf(*buf, "P_SLR_Y_VV_NVAR_END");
            break;
        case P_SLR_Y_VV_SLR_Y_VV_UNK:
            sprintf(*buf, "P_SLR_Y_VV_SLR_Y_VV_UNK");
            break;
        case P_SLR_Y_VV_SLR_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_SLR_Y_VV_SLR_Y_VV_NVAR_UNK");
            break;
        case P_SLR_Y_VC_INSTINIT:
            sprintf(*buf, "P_SLR_Y_VC_INSTINIT");
            break;
        case P_SLR_Y_VC_SLR_Y_VC_NVAR:
            sprintf(*buf, "P_SLR_Y_VC_SLR_Y_VC_NVAR");
            break;
        case P_SLR_Y_VC_INTTERM:
            sprintf(*buf, "P_SLR_Y_VC_INTTERM");
            break;
        case P_SLR_Y_VC_NOINTTERM:
            sprintf(*buf, "P_SLR_Y_VC_NOINTTERM");
            break;
        case P_SLR_Y_VC_D0EQUALS0L:
            sprintf(*buf, "P_SLR_Y_VC_D0EQUALS0L");
            break;
        case P_SLR_Y_VC_NVAR_END:
            sprintf(*buf, "P_SLR_Y_VC_NVAR_END");
            break;
        case P_SLR_Y_VC_SLR_Y_VC_UNK:
            sprintf(*buf, "P_SLR_Y_VC_SLR_Y_VC_UNK");
            break;
        case P_SLR_Y_CV_INSTINIT:
            sprintf(*buf, "P_SLR_Y_CV_INSTINIT");
            break;
        case P_SLR_Y_CV_SLR_Y_CV_NVAR:
            sprintf(*buf, "P_SLR_Y_CV_SLR_Y_CV_NVAR");
            break;
        case P_SLR_Y_CV_INTTERM_INIT:
            sprintf(*buf, "P_SLR_Y_CV_INTTERM_INIT");
            break;
        case P_SLR_Y_CV_INTTERM_LESS:
            sprintf(*buf, "P_SLR_Y_CV_INTTERM_LESS");
            break;
        case P_SLR_Y_CV_INTTERM_GREATER:
            sprintf(*buf, "P_SLR_Y_CV_INTTERM_GREATER");
            break;
        case P_SLR_Y_CV_NOINTTERM:
            sprintf(*buf, "P_SLR_Y_CV_NOINTTERM");
            break;
        case P_SLR_Y_CV_D0EQUALS0L:
            sprintf(*buf, "P_SLR_Y_CV_D0EQUALS0L");
            break;
        case P_SLR_Y_CV_NVAR_END:
            sprintf(*buf, "P_SLR_Y_CV_NVAR_END");
            break;
        case P_SLR_Y_CV_SLR_Y_CV_UNK:
            sprintf(*buf, "P_SLR_Y_CV_SLR_Y_CV_UNK");
            break;
        case CALL_BFUNC_XX_INSTINIT:
            sprintf(*buf, "CALL_BFUNC_XX_INSTINIT");
            break;
        case CALL_BFUNC_XX_CALL_BFUNC_XX_NVAR:
            sprintf(*buf, "CALL_BFUNC_XX_CALL_BFUNC_XX_NVAR");
            break;
        case CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_INT:
            sprintf(*buf, "CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_INT");
            break;
        case CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_NOINT:
            sprintf(*buf, "CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_NOINT");
            break;
        case CALL_BFUNC_XX_CALL_BFUNC_XX_UNK:
            sprintf(*buf, "CALL_BFUNC_XX_CALL_BFUNC_XX_UNK");
            break;
        case CALL_BFUNC_YX_INSTINIT:
            sprintf(*buf, "CALL_BFUNC_YX_INSTINIT");
            break;
        case CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_INT:
            sprintf(*buf, "CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_INT");
            break;
        case CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_NOINT:
            sprintf(*buf, "CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_NOINT");
            break;
        case CALL_BFUNC_YX_CALL_BFUNC_YX_UNK:
            sprintf(*buf, "CALL_BFUNC_YX_CALL_BFUNC_YX_UNK");
            break;
        case CALL_BFUNC_XY_INSTINIT:
            sprintf(*buf, "CALL_BFUNC_XY_INSTINIT");
            break;
        case CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_INT:
            sprintf(*buf, "CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_INT");
            break;
        case CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_NOINT:
            sprintf(*buf, "CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_NOINT");
            break;
        case CALL_BFUNC_XY_CALL_BFUNC_XY_UNK:
            sprintf(*buf, "CALL_BFUNC_XY_CALL_BFUNC_XY_UNK");
            break;
        case CALL_BFUNC_YY_INSTINIT:
            sprintf(*buf, "CALL_BFUNC_YY_INSTINIT");
            break;
        case CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_INT:
            sprintf(*buf, "CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_INT");
            break;
        case CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_NOINT:
            sprintf(*buf, "CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_NOINT");
            break;
        case CALL_BFUNC_YY_CALL_BFUNC_YY_UNK:
            sprintf(*buf, "CALL_BFUNC_YY_CALL_BFUNC_YY_UNK");
            break;
        case P_EQUAL_INSTINIT:
            sprintf(*buf, "P_EQUAL_INSTINIT");
            break;
        case P_EQUAL_END:
            sprintf(*buf, "P_EQUAL_END");
            break;
        case P_DIF_INSTINIT:
            sprintf(*buf, "P_DIF_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_DIF_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_DIF_LOW_LEVEL_TRACER");
            break;
#endif
        case P_DIF_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_DIF_POST_LOW_LEVEL_TRACER");
            break;
        case P_DIF_DIF_NVAR1:
            sprintf(*buf, "P_DIF_DIF_NVAR1");
            break;
        case P_DIF_DIF_NVAR1_NVAR2:
            sprintf(*buf, "P_DIF_DIF_NVAR1_NVAR2");
            break;
            break;
        case P_DIF_DIF_UNK1:
            sprintf(*buf, "P_DIF_DIF_UNK1");
            break;
        case P_DIF_DIF_NVAR1_UNK2:
            sprintf(*buf, "P_DIF_DIF_NVAR1_UNK2");
            break;
        case P_EQ_INSTINIT:
            sprintf(*buf, "P_EQ_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_EQ_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_EQ_LOW_LEVEL_TRACER");
            break;
#endif
        case P_EQ_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_EQ_POST_LOW_LEVEL_TRACER");
            break;
        case P_EQ_P_EQ_NVAR1:
            sprintf(*buf, "P_EQ_P_EQ_NVAR1");
            break;
        case P_EQ_P_EQ_NVAR1_NVAR2:
            sprintf(*buf, "P_EQ_P_EQ_NVAR1_NVAR2");
            break;
        case P_EQ_P_EQ_NVAR1_UNK2:
            sprintf(*buf, "P_EQ_P_EQ_NVAR1_UNK2");
            break;
        case P_EQ_P_EQ_UNK1:
            sprintf(*buf, "P_EQ_P_EQ_UNK1");
            break;
        case P_EQ_P_EQ_VAR1_NVAR2:
            sprintf(*buf, "P_EQ_P_EQ_VAR1_NVAR2");
            break;
        case P_EQ_P_EQ_VAR1_UNK2_END:
            sprintf(*buf, "P_EQ_P_EQ_VAR1_UNK2_END");
            break;
        case P_ARG_VV_INSTINIT:
            sprintf(*buf, "P_ARG_VV_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_ARG_VV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_ARG_VV_LOW_LEVEL_TRACER");
            break;
#endif
        case P_ARG_VV_TEST_D0:
            sprintf(*buf, "P_ARG_VV_TEST_D0");
            break;
        case P_ARG_VV_ARG_ARG1_NVAR:
            sprintf(*buf, "P_ARG_VV_ARG_ARG1_NVAR");
            break;
        case P_ARG_VV_TEST_D1:
            sprintf(*buf, "P_ARG_VV_TEST_D1");
            break;
        case P_ARG_VV_ARG_ARG2_NVAR:
            sprintf(*buf, "P_ARG_VV_ARG_ARG2_NVAR");
            break;
        case P_ARG_VV_ARG_ARG2_UNK:
            sprintf(*buf, "P_ARG_VV_ARG_ARG2_UNK");
            break;
        case P_ARG_VV_ARG_ARG1_UNK:
            sprintf(*buf, "P_ARG_VV_ARG_ARG1_UNK");
            break;
        case P_ARG_CV_INSTINIT:
            sprintf(*buf, "P_ARG_CV_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_ARG_CV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_ARG_CV_LOW_LEVEL_TRACER");
            break;
#endif
        case P_ARG_CV_TEST_D1:
            sprintf(*buf, "P_ARG_CV_TEST_D1");
            break;
        case P_ARG_CV_ARG_ARG2_VC_NVAR:
            sprintf(*buf, "P_ARG_CV_ARG_ARG2_VC_NVAR");
            break;
        case P_ARG_CV_ARG_ARG2_VC_UNK:
            sprintf(*buf, "P_ARG_CV_ARG_ARG2_VC_UNK");
            break;
        case P_ARG_Y_VV_INSTINIT:
            sprintf(*buf, "P_ARG_Y_VV_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_ARG_Y_VV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_ARG_Y_VV_LOW_LEVEL_TRACER");
            break;
#endif
        case P_ARG_Y_VV_TEST_D0:
            sprintf(*buf, "P_ARG_Y_VV_TEST_D0");
            break;
        case P_ARG_Y_VV_ARG_Y_ARG1_NVAR:
            sprintf(*buf, "P_ARG_Y_VV_ARG_Y_ARG1_NVAR");
            break;
        case P_ARG_Y_VV_TEST_D1:
            sprintf(*buf, "P_ARG_Y_VV_TEST_D1");
            break;
        case P_ARG_Y_VV_ARG_Y_ARG2_NVAR:
            sprintf(*buf, "P_ARG_Y_VV_ARG_Y_ARG2_NVAR");
            break;
        case P_ARG_Y_VV_ARG_Y_ARG2_UNK:
            sprintf(*buf, "P_ARG_Y_VV_ARG_Y_ARG2_UNK");
            break;
        case P_ARG_Y_VV_ARG_Y_ARG1_UNK:
            sprintf(*buf, "P_ARG_Y_VV_ARG_Y_ARG1_UNK");
            break;
        case P_ARG_Y_CV_INSTINIT:
            sprintf(*buf, "P_ARG_Y_CV_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_ARG_Y_CV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_ARG_Y_CV_LOW_LEVEL_TRACER");
            break;
#endif
        case P_ARG_Y_CV_TEST_D1:
            sprintf(*buf, "P_ARG_Y_CV_TEST_D1");
            break;
        case P_ARG_Y_CV_D1APPL_INIT:
            sprintf(*buf, "P_ARG_Y_CV_D1APPL_INIT");
            break;
        case P_ARG_Y_CV_D1APPL_END:
            sprintf(*buf, "P_ARG_Y_CV_D1APPL_END");
            break;
        case P_ARG_Y_CV_D1PAIR_INIT:
            sprintf(*buf, "P_ARG_Y_CV_D1PAIR_INIT");
            break;
        case P_ARG_Y_CV_D1PAIR_LESS0:
            sprintf(*buf, "P_ARG_Y_CV_D1PAIR_LESS0");
            break;
        case P_ARG_Y_CV_D1PAIR_END:
            sprintf(*buf, "P_ARG_Y_CV_D1PAIR_END");
            break;
        case P_ARG_Y_CV_ARG_Y_ARG2_VC_UNK:
            sprintf(*buf, "P_ARG_Y_CV_ARG_Y_ARG2_VC_UNK");
            break;
        case P_FUNCTOR_INSTINIT:
            sprintf(*buf, "P_FUNCTOR_INSTINIT");
            break;
        case P_FUNCTOR_END:
            sprintf(*buf, "P_FUNCTOR_END");
            break;
        case P_FUNC2S_VV_INSTINIT:
            sprintf(*buf, "P_FUNC2S_VV_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2S_VV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2S_VV_LOW_LEVEL_TRACER");
            break;
#endif
        case P_FUNC2S_TEST_D0:
            sprintf(*buf, "P_FUNC2S_TEST_D0");
            break;
        case P_FUNC2S_VV_TEST_D1:
            sprintf(*buf, "P_FUNC2S_VV_TEST_D1");
            break;
        case P_FUNC2S_VV_D1INT:
            sprintf(*buf, "P_FUNC2S_VV_D1INT");
            break;
        case P_FUNC2S_VV_D1NOTINT:
            sprintf(*buf, "P_FUNC2S_VV_D1NOTINT");
            break;
        case P_FUNC2S_VV_D1BIGINT:
            sprintf(*buf, "P_FUNC2S_VV_D1BIGINT");
            break;
        case P_FUNC2S_VV_D1NOTBIGINT:
            sprintf(*buf, "P_FUNC2S_VV_D1NOTBIGINT");
            break;
        case P_FUNC2S_VV_D1NOTINT_END:
            sprintf(*buf, "P_FUNC2S_VV_D1NOTINT_END");
            break;
        case P_FUNC2S_VV_D0NOTATOMIC:
            sprintf(*buf, "P_FUNC2S_VV_D0NOTATOMIC");
            break;
        case P_FUNC2S_VV_FIRSTIFOK:
            sprintf(*buf, "P_FUNC2S_VV_FIRSTIFOK");
            break;
        case P_FUNC2S_VV_SECONDIFOK_D0NOTATOM:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_D0NOTATOM");
            break;
        case P_FUNC2S_VV_SECONDIFOK_D0ATOM:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_D0ATOM");
            break;
        case P_FUNC2S_VV_SECONDIFOK_POST_D0ATOM:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_POST_D0ATOM");
            break;
        case P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_INIT:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_INIT");
            break;
        case P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_IFOK:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_IFOK");
            break;
        case P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_NOIF:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_NOIF");
            break;
        case P_FUNC2S_VV_SECONDIFOK_INSIDEWHILE:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_INSIDEWHILE");
            break;
        case P_FUNC2S_VV_SECONDIFOK_END:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_END");
            break;
        case P_FUNC2S_VV_THIRDIFOK:
            sprintf(*buf, "P_FUNC2S_VV_THIRDIFOK");
            break;
        case P_FUNC2S_VV_ELSE:
            sprintf(*buf, "P_FUNC2S_VV_ELSE");
            break;
        case P_FUNC2S_VV_FUNC2S_UNK2:
            sprintf(*buf, "P_FUNC2S_VV_FUNC2S_UNK2");
            break;
        case P_FUNC2S_VV_FUNC2S_UNK:
            sprintf(*buf, "P_FUNC2S_VV_FUNC2S_UNK");
            break;
        case P_FUNC2S_CV_INSTINIT:
            sprintf(*buf, "P_FUNC2S_CV_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2S_CV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2S_CV_LOW_LEVEL_TRACER");
            break;
#endif
        case P_FUNC2S_CV_TEST_D1:
            sprintf(*buf, "P_FUNC2S_CV_TEST_D1");
            break;
        case P_FUNC2S_CV_D1INT:
            sprintf(*buf, "P_FUNC2S_CV_D1INT");
            break;
        case P_FUNC2S_CV_D1NOTINT:
            sprintf(*buf, "P_FUNC2S_CV_D1NOTINT");
            break;
        case P_FUNC2S_CV_D1NOINT_D1BIGINT:
            sprintf(*buf, "P_FUNC2S_CV_D1NOINT_D1BIGINT");
            break;
        case P_FUNC2S_CV_D1NOTBIGINT:
            sprintf(*buf, "P_FUNC2S_CV_D1NOTBIGINT");
            break;
        case P_FUNC2S_CV_POST_IF:
            sprintf(*buf, "P_FUNC2S_CV_POST_IF");
            break;
        case P_FUNC2S_CV_FIRSTIFOK:
            sprintf(*buf, "P_FUNC2S_CV_FIRSTIFOK");
            break;
        case P_FUNC2S_CV_D1GREATER_D0NOTATOM:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_D0NOTATOM");
            break;
        case P_FUNC2S_CV_D1GREATER_D0ATOM:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_D0ATOM");
            break;
        case P_FUNC2S_CV_D1GREATER_POST_IF:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_POST_IF");
            break;
        case P_FUNC2S_CV_D1GREATER_IFOK_INIT:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_IFOK_INIT");
            break;
        case P_FUNC2S_CV_D1GREATER_IFOK_IFOK:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_IFOK_IFOK");
            break;
        case P_FUNC2S_CV_D1GREATER_IFOK_NOIF:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_IFOK_NOIF");
            break;
        case P_FUNC2S_CV_D1GREATER_INSIDEWHILE:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_INSIDEWHILE");
            break;
        case P_FUNC2S_CV_D1GREATER_END:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_END");
            break;
        case P_FUNC2S_CV_D1ISZERO:
            sprintf(*buf, "P_FUNC2S_CV_D1ISZERO");
            break;
        case P_FUNC2S_CV_ELSE:
            sprintf(*buf, "P_FUNC2S_CV_ELSE");
            break;
        case P_FUNC2S_CV_END:
            sprintf(*buf, "P_FUNC2S_CV_END");
            break;
        case P_FUNC2S_VC_INSTINIT:
            sprintf(*buf, "P_FUNC2S_VC_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2S_VC_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2S_VC_LOW_LEVEL_TRACER");
            break;
#endif
        case P_FUNC2S_VC_TEST_D0:
            sprintf(*buf, "P_FUNC2S_VC_TEST_D0");
            break;
        case P_FUNC2S_VC_FUNC2S_NVAR_VC:
            sprintf(*buf, "P_FUNC2S_VC_FUNC2S_NVAR_VC");
            break;
        case P_FUNC2S_VC_D0NOATOMIC:
            sprintf(*buf, "P_FUNC2S_VC_D0NOATOMIC");
            break;
        case P_FUNC2S_VC_EQUALS:
            sprintf(*buf, "P_FUNC2S_VC_EQUALS");
            break;
        case P_FUNC2S_VC_D1ISZERO:
            sprintf(*buf, "P_FUNC2S_VC_D1ISZERO");
            break;
        case P_FUNC2S_VC_D0NOATOM:
            sprintf(*buf, "P_FUNC2S_VC_D0NOATOM");
            break;
        case P_FUNC2S_VC_D0ATOM:
            sprintf(*buf, "P_FUNC2S_VC_D0ATOM");
            break;
        case P_FUNC2S_VC_POST_ELSE:
            sprintf(*buf, "P_FUNC2S_VC_POST_ELSE");
            break;
        case P_FUNC2S_VC_IFOK_INIT:
            sprintf(*buf, "P_FUNC2S_VC_IFOK_INIT");
            break;
        case P_FUNC2S_VC_IFOK_IFOK:
            sprintf(*buf, "P_FUNC2S_VC_IFOK_IFOK");
            break;
        case P_FUNC2S_VC_IFOK_NOIF:
            sprintf(*buf, "P_FUNC2S_VC_IFOK_NOIF");
            break;
        case P_FUNC2S_VC_INSIDEWHILE:
            sprintf(*buf, "P_FUNC2S_VC_INSIDEWHILE");
            break;
        case P_FUNC2S_VC_END1:
            sprintf(*buf, "P_FUNC2S_VC_END1");
            break;
        case P_FUNC2S_VC_END2:
            sprintf(*buf, "P_FUNC2S_VC_END2");
            break;
        case P_FUNC2S_Y_VV_INSTINIT:
            sprintf(*buf, "P_FUNC2S_Y_VV_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2S_Y_VV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2S_Y_VV_LOW_LEVEL_TRACER");
            break;
#endif
        case P_FUNC2S_Y_VV_TEST_D0:
            sprintf(*buf, "P_FUNC2S_Y_VV_TEST_D0");
            break;
        case P_FUNC2S_Y_VV_TEST_D1:
            sprintf(*buf, "P_FUNC2S_Y_VV_TEST_D1");
            break;
        case P_FUNC2S_Y_VV_D1INT:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1INT");
            break;
        case P_FUNC2S_Y_VV_D1NOTINT:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1NOTINT");
            break;
        case P_FUNC2S_Y_VV_D1BIGINT:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1BIGINT");
            break;
        case P_FUNC2S_Y_VV_D1NOTBIGINT:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1NOTBIGINT");
            break;
        case P_FUNC2S_Y_VV_POST_IF:
            sprintf(*buf, "P_FUNC2S_Y_VV_POST_IF");
            break;
        case P_FUNC2S_Y_VV_D0NOATOMIC:
            sprintf(*buf, "P_FUNC2S_Y_VV_D0NOATOMIC");
            break;
        case P_FUNC2S_Y_VV_EQUALS:
            sprintf(*buf, "P_FUNC2S_Y_VV_EQUALS");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_D0NOATOM:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_D0NOATOM");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_D0ATOM:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_D0ATOM");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_POST_ELSE:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_POST_ELSE");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_IFOK_INIT:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_IFOK_INIT");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_IFOK_IFOK:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_IFOK_IFOK");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_IFOK_NOIF:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_IFOK_NOIF");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_INSIDEWHILE:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_INSIDEWHILE");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_END:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_END");
            break;
        case P_FUNC2S_Y_VV_D1ISZERO:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1ISZERO");
            break;
        case P_FUNC2S_Y_VV_ELSE:
            sprintf(*buf, "P_FUNC2S_Y_VV_ELSE");
            break;
        case P_FUNC2S_Y_VV_END1:
            sprintf(*buf, "P_FUNC2S_Y_VV_END1");
            break;
        case P_FUNC2S_Y_VV_END2:
            sprintf(*buf, "P_FUNC2S_Y_VV_END2");
            break;
        case P_FUNC2S_Y_CV_INSTINIT:
            sprintf(*buf, "P_FUNC2S_Y_CV_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2S_Y_CV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2S_Y_CV_LOW_LEVEL_TRACER");
            break;
#endif
        case P_FUNC2S_Y_CV_TEST_D1:
            sprintf(*buf, "P_FUNC2S_Y_CV_TEST_D1");
            break;
        case P_FUNC2S_Y_CV_D1INT:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1INT");
            break;
        case P_FUNC2S_Y_CV_D1NOTINT:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1NOTINT");
            break;
        case P_FUNC2S_Y_CV_D1BIGINT:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1BIGINT");
            break;
        case P_FUNC2S_Y_CV_D1NOTBIGINT:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1NOTBIGINT");
            break;
        case P_FUNC2S_Y_CV_POST_IF:
            sprintf(*buf, "P_FUNC2S_Y_CV_POST_IF");
            break;
        case P_FUNC2S_Y_CV_EQUALS:
            sprintf(*buf, "P_FUNC2S_Y_CV_EQUALS");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_D0NOATOM:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_D0NOATOM");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_D0ATOM:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_D0ATOM");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_POST_ELSE:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_POST_ELSE");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_IFOK_INIT:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_IFOK_INIT");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_IFOK_IFOK:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_IFOK_IFOK");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_IFOK_NOIF:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_IFOK_NOIF");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_INSIDEWHILE:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_INSIDEWHILE");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_END:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_END");
            break;
        case P_FUNC2S_Y_CV_D1ISZERO:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1ISZERO");
            break;
        case P_FUNC2S_Y_CV_ELSE:
            sprintf(*buf, "P_FUNC2S_Y_CV_ELSE");
            break;
        case P_FUNC2S_Y_CV_END:
            sprintf(*buf, "P_FUNC2S_Y_CV_END");
            break;
        case P_FUNC2S_Y_VC_INSTINIT:
            sprintf(*buf, "P_FUNC2S_Y_VC_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2S_Y_VC_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2S_Y_VC_LOW_LEVEL_TRACER");
            break;
#endif
        case P_FUNC2S_Y_VC_TEST_D0:
            sprintf(*buf, "P_FUNC2S_Y_VC_TEST_D0");
            break;
        case P_FUNC2S_Y_VC_FUNC2S_Y_NVAR_VC:
            sprintf(*buf, "P_FUNC2S_Y_VC_FUNC2S_Y_NVAR_VC");
            break;
        case P_FUNC2S_Y_VC_D0NOATOMIC:
            sprintf(*buf, "P_FUNC2S_Y_VC_D0NOATOMIC");
            break;
        case P_FUNC2S_Y_VC_EQUALS:
            sprintf(*buf, "P_FUNC2S_Y_VC_EQUALS");
            break;
        case P_FUNC2S_Y_VC_D1ISZERO:
            sprintf(*buf, "P_FUNC2S_Y_VC_D1ISZERO");
            break;
        case P_FUNC2S_Y_VC_D0NOATOM1:
            sprintf(*buf, "P_FUNC2S_Y_VC_D0NOATOM1");
            break;
        case P_FUNC2S_Y_VC_D0NOATOM2:
            sprintf(*buf, "P_FUNC2S_Y_VC_D0NOATOM2");
            break;
        case P_FUNC2S_Y_VC_D0ATOM:
            sprintf(*buf, "P_FUNC2S_Y_VC_D0ATOM");
            break;
        case P_FUNC2S_Y_VC_POST_ELSE:
            sprintf(*buf, "P_FUNC2S_Y_VC_POST_ELSE");
            break;
        case P_FUNC2S_Y_VC_IFOK_INIT:
            sprintf(*buf, "P_FUNC2S_Y_VC_IFOK_INIT");
            break;
        case P_FUNC2S_Y_VC_IFOK_IFOK:
            sprintf(*buf, "P_FUNC2S_Y_VC_IFOK_IFOK");
            break;
        case P_FUNC2S_Y_VC_IFOK_NOIF:
            sprintf(*buf, "P_FUNC2S_Y_VC_IFOK_NOIF");
            break;
        case P_FUNC2S_Y_VC_INSIDEWHILE:
            sprintf(*buf, "P_FUNC2S_Y_VC_INSIDEWHILE");
            break;
        case P_FUNC2S_Y_VC_END1:
            sprintf(*buf, "P_FUNC2S_Y_VC_END1");
            break;
        case P_FUNC2S_Y_VC_END2:
            sprintf(*buf, "P_FUNC2S_Y_VC_END2");
            break;
        case P_FUNC2F_XX_INSTINIT:
            sprintf(*buf, "P_FUNC2F_XX_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2F_XX_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2F_XX_LOW_LEVEL_TRACER");
            break;
#endif
        case P_FUNC2F_XX_TEST_D0:
            sprintf(*buf, "P_FUNC2F_XX_TEST_D0");
            break;
        case P_FUNC2F_XX_D0APPL:
            sprintf(*buf, "P_FUNC2F_XX_D0APPL");
            break;
        case P_FUNC2F_XX_D0APPL_D1EXTFUNC:
            sprintf(*buf, "P_FUNC2F_XX_D0APPL_D1EXTFUNC");
            break;
        case P_FUNC2F_XX_D0APPL_END:
            sprintf(*buf, "P_FUNC2F_XX_D0APPL_END");
            break;
        case P_FUNC2F_XX_D0PAIR:
            sprintf(*buf, "P_FUNC2F_XX_D0PAIR");
            break;
        case P_FUNC2F_XX_D0NOCOMPOUND:
            sprintf(*buf, "P_FUNC2F_XX_D0NOCOMPOUND");
            break;
        case P_FUNC2F_XX_END:
            sprintf(*buf, "P_FUNC2F_XX_END");
            break;
        case P_FUNC2F_XY_INSTINIT:
            sprintf(*buf, "P_FUNC2F_XY_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2F_XY_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2F_XY_LOW_LEVEL_TRACER");
            break;
#endif
        case P_FUNC2F_XY_TEST_D0:
            sprintf(*buf, "P_FUNC2F_XY_TEST_D0");
            break;
        case P_FUNC2F_XY_D0APPL:
            sprintf(*buf, "P_FUNC2F_XY_D0APPL");
            break;
        case P_FUNC2F_XY_D0APPL_D1EXTFUNC:
            sprintf(*buf, "P_FUNC2F_XY_D0APPL_D1EXTFUNC");
            break;
        case P_FUNC2F_XY_D0APPL_END:
            sprintf(*buf, "P_FUNC2F_XY_D0APPL_END");
            break;
        case P_FUNC2F_XY_D0PAIR:
            sprintf(*buf, "P_FUNC2F_XY_D0PAIR");
            break;
        case P_FUNC2F_XY_D0NOCOMPOUND:
            sprintf(*buf, "P_FUNC2F_XY_D0NOCOMPOUND");
            break;
        case P_FUNC2F_XY_END:
            sprintf(*buf, "P_FUNC2F_XY_END");
            break;
        case P_FUNC2F_YX_INSTINIT:
            sprintf(*buf, "P_FUNC2F_YX_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2F_YX_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2F_YX_LOW_LEVEL_TRACER");
            break;
#endif
        case P_FUNC2F_YX_TEST_D0:
            sprintf(*buf, "P_FUNC2F_YX_TEST_D0");
            break;
        case P_FUNC2F_YX_D0APPL:
            sprintf(*buf, "P_FUNC2F_YX_D0APPL");
            break;
        case P_FUNC2F_YX_D0APPL_D1EXTFUNC:
            sprintf(*buf, "P_FUNC2F_YX_D0APPL_D1EXTFUNC");
            break;
        case P_FUNC2F_YX_D0APPL_END:
            sprintf(*buf, "P_FUNC2F_YX_D0APPL_END");
            break;
        case P_FUNC2F_YX_D0PAIR:
            sprintf(*buf, "P_FUNC2F_YX_D0PAIR");
            break;
        case P_FUNC2F_YX_D0NOCOMPOUND:
            sprintf(*buf, "P_FUNC2F_YX_D0NOCOMPOUND");
            break;
        case P_FUNC2F_YX_END:
            sprintf(*buf, "P_FUNC2F_YX_END");
            break;
        case P_FUNC2F_YY_INSTINIT:
            sprintf(*buf, "P_FUNC2F_YY_INSTINIT");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2F_YY_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2F_YY_LOW_LEVEL_TRACER");
            break;
#endif
        case P_FUNC2F_YY_TEST_D0:
            sprintf(*buf, "P_FUNC2F_YY_TEST_D0");
            break;
        case P_FUNC2F_YY_D0APPL:
            sprintf(*buf, "P_FUNC2F_YY_D0APPL");
            break;
        case P_FUNC2F_YY_D0APPL_D1EXTFUNC:
            sprintf(*buf, "P_FUNC2F_YY_D0APPL_D1EXTFUNC");
            break;
        case P_FUNC2F_YY_D0APPL_END:
            sprintf(*buf, "P_FUNC2F_YY_D0APPL_END");
            break;
        case P_FUNC2F_YY_D0PAIR:
            sprintf(*buf, "P_FUNC2F_YY_D0PAIR");
            break;
        case P_FUNC2F_YY_D0NOCOMPOUND:
            sprintf(*buf, "P_FUNC2F_YY_D0NOCOMPOUND");
            break;
        case P_FUNC2F_YY_END:
            sprintf(*buf, "P_FUNC2F_YY_END");
            break;
    }
}

static inline void
sprint_block(YAP_BBs block, char **buf) {
    switch(block) {
        case ENTRY: break;
        case YAAM_DEREF_BODY_D0PT0:
            sprintf(*buf, "YAAM_DEREF_BODY_D0PT0\n");
            break;
        case YAAM_DEREF_BODY_D0PT1:
            sprintf(*buf, "YAAM_DEREF_BODY_D0PT1\n");
            break;
        case YAAM_DEREF_BODY_D0S_SREG:
            sprintf(*buf, "YAAM_DEREF_BODY_D0S_SREG\n");
            break;
        case YAAM_DEREF_BODY_D1PT0:
            sprintf(*buf, "YAAM_DEREF_BODY_D1PT0\n");
            break;
        case YAAM_DEREF_BODY_D1PT1:
            sprintf(*buf, "YAAM_DEREF_BODY_D1PT1\n");
            break;
        case YAAM_FAIL:
            sprintf(*buf, "YAAM_FAIL\n");
            break;
        case YAAM_CHECK_TRAIL_TR:
            sprintf(*buf, "YAAM_CHECK_TRAIL_TR\n");
            break;
	case YAAM_UNIFYBOUND:
            break;
        case NoStackExecute_Exception:
            sprintf(*buf, "NoStackExecute_Exception\n");
            break;
        case NoStackDExecute_Exception:
            sprintf(*buf, "NoStackDExecute_Exception\n");
            break;
        case NoStackCall_Exception:
            sprintf(*buf, "NoStackCall_Exception\n");
            break;
        case NoStackDeallocate_Exception:
            sprintf(*buf, "NoStackDeallocate_Exception\n");
            break;
#ifdef COROUTINING
        case NoStackFail_Exception:
            sprintf(*buf, "NoStackFail_Exception\n");
            break;
#endif
        case NoStackCut_Exception:
            sprintf(*buf, "NoStackCut_Exception\n");
            break;
        case NoStackCutT_Exception:
            sprintf(*buf, "NoStackCutT_Exception\n");
            break;
        case NoStackCutE_Exception:
            sprintf(*buf, "NoStackCutE_Exception\n");
            break;
        case NoStackCommitX_Exception:
            sprintf(*buf, "NoStackCommitX_Exception\n");
            break;
        case NoStackCommitY_Exception:
            sprintf(*buf, "NoStackCommitY_Exception\n");
            break;
        case NoStackEither_Exception:
            sprintf(*buf, "NoStackEither_Exception\n");
            break;
        case NoStackPExecute_Exception:
            sprintf(*buf, "NoStackPExecute_Exception\n");
            break;
        case NoStackPExecute2_Exception:
            sprintf(*buf, "NoStackPExecute2_Exception\n");
            break;
        case NoStackPTExecute_Exception:
            sprintf(*buf, "NoStackPTExecute_Exception\n");
            break;
        case TRY_ME_INSTINIT:
            sprintf(*buf, "TRY_ME_INSTINIT\n");
            break;
#ifdef YAPOR
        case TRY_ME_YAPOR:
            sprintf(*buf, "TRY_ME_YAPOR\n");
            break;
#endif
        case TRY_ME_END:
            sprintf(*buf, "TRY_ME_END\n");
            break;
        case RETRY_ME_INSTINIT:
            sprintf(*buf, "RETRY_ME_INSTINIT\n");
            break;
#ifdef FROZEN_STACKS
        case RETRY_ME_FROZEN:
            sprintf(*buf, "RETRY_ME_FROZEN\n");
            break;
#else
        case RETRY_ME_NOFROZEN:
            sprintf(*buf, "RETRY_ME_NOFROZEN\n");
            break;
#endif
        case RETRY_ME_END:
            sprintf(*buf, "RETRY_ME_END\n");
            break;
        case TRUST_ME_INSTINIT:
            sprintf(*buf, "TRUST_ME_INSTINIT\n");
            break;
        case TRUST_ME_IF:
            sprintf(*buf, "TRUST_ME_IF\n");
            break;
        case TRUST_ME_END:
            sprintf(*buf, "TRUST_ME_END\n");
            break;
        case ENTER_PROFILING_INSTINIT:
            sprintf(*buf, "ENTER_PROFILING_INSTINIT\n");
            break;
        case RETRY_PROFILED_INSTINIT:
            sprintf(*buf, "RETRY_PROFILED_INSTINIT\n");
            break;
        case PROFILED_RETRY_ME_INSTINIT:
            sprintf(*buf, "PROFILED_RETRY_ME_INSTINIT\n");
            break;
#ifdef FROZEN_STACKS
        case PROFILED_RETRY_ME_FROZEN:
            sprintf(*buf, "PROFILED_RETRY_ME_FROZEN\n");
            break;
#else
        case PROFILED_RETRY_ME_NOFROZEN:
            sprintf(*buf, "PROFILED_RETRY_ME_NOFROZEN\n");
            break;
#endif
        case PROFILED_RETRY_ME_END:
            sprintf(*buf, "PROFILED_RETRY_ME_END\n");
            break;
        case PROFILED_TRUST_ME_INSTINIT:
            sprintf(*buf, "PROFILED_TRUST_ME_INSTINIT\n");
            break;
		case PROFILED_TRUST_ME_IF:
            sprintf(*buf, "PROFILED_TRUST_ME_IF\n");
            break;
        case PROFILED_TRUST_ME_END:
            sprintf(*buf, "PROFILED_TRUST_ME_END\n");
            break;
        case PROFILED_RETRY_LOGICAL_INSTINIT:
            sprintf(*buf, "PROFILED_RETRY_LOGICAL_INSTINIT\n");
            break;
#ifdef THREADS
        case PROFILED_RETRY_LOGICAL_THREADS:
            sprintf(*buf, "PROFILED_RETRY_LOGICAL_THREADS\n");
            break;
#endif
        case PROFILED_RETRY_LOGICAL_POST_THREADS:
            sprintf(*buf, "PROFILED_RETRY_LOGICAL_POST_THREADS\n");
            break;
#ifdef FROZEN_STACKS
        case PROFILED_RETRY_LOGICAL_FROZEN:
            sprintf(*buf, "PROFILED_RETRY_LOGICAL_FROZEN\n");
            break;
#else
        case PROFILED_RETRY_LOGICAL_NOFROZEN:
            sprintf(*buf, "PROFILED_RETRY_LOGICAL_NOFROZEN\n");
            break;
#endif
        case PROFILED_RETRY_LOGICAL_END:
            sprintf(*buf, "PROFILED_RETRY_LOGICAL_END\n");
            break;
        case PROFILED_TRUST_LOGICAL_INSTINIT:
            sprintf(*buf, "PROFILED_TRUST_LOGICAL_INSTINIT\n");
            break;
        case PROFILED_TRUST_LOGICAL_END:
            sprintf(*buf, "PROFILED_TRUST_LOGICAL_END\n");
            break;
        case COUNT_CALL_INSTINIT:
            sprintf(*buf, "COUNT_CALL_INSTINIT\n");
            break;
		case COUNT_CALL_MIDDLE:
            sprintf(*buf, "COUNT_CALL_MIDDLE\n");
            break;
        case COUNT_CALL_END:
            sprintf(*buf, "COUNT_CALL_END\n");
            break;
        case COUNT_RETRY_INSTINIT:
            sprintf(*buf, "COUNT_RETRY_INSTINIT\n");
            break;
        case COUNT_RETRY_MIDDLE:
            sprintf(*buf, "COUNT_RETRY_MIDDLE\n");
            break;
        case COUNT_RETRY_END:
            sprintf(*buf, "COUNT_RETRY_END\n");
            break;
        case COUNT_RETRY_ME_INSTINIT:
            sprintf(*buf, "COUNT_RETRY_ME_INSTINIT\n");
            break;
		case COUNT_RETRY_ME_MIDDLE:
            sprintf(*buf, "COUNT_RETRY_ME_MIDDLE\n");
            break;
        case COUNT_RETRY_ME_END:
            sprintf(*buf, "COUNT_RETRY_ME_END\n");
            break;
        case COUNT_TRUST_ME_INSTINIT:
            sprintf(*buf, "COUNT_TRUST_ME_INSTINIT\n");
            break;
		case COUNT_TRUST_ME_MIDDLE:
            sprintf(*buf, "COUNT_TRUST_ME_MIDDLE\n");
            break;
        case COUNT_TRUST_ME_END:
            sprintf(*buf, "COUNT_TRUST_ME_END\n");
            break;
        case COUNT_RETRY_LOGICAL_INSTINIT:
            sprintf(*buf, "COUNT_RETRY_LOGICAL_INSTINIT\n");
            break;
        case COUNT_RETRY_LOGICAL_END:
            sprintf(*buf, "COUNT_RETRY_LOGICAL_END\n");
            break;
        case COUNT_TRUST_LOGICAL_INSTINIT:
            sprintf(*buf, "COUNT_TRUST_LOGICAL_INSTINIT\n");
            break;
        case COUNT_TRUST_LOGICAL_END:
            sprintf(*buf, "COUNT_TRUST_LOGICAL_END\n");
            break;
        case LOCK_LU_INSTINIT:
            sprintf(*buf, "LOCK_LU_INSTINIT\n");
            break;
        case LOCK_LU_END:
            sprintf(*buf, "LOCK_LU_END\n");
            break;
        case UNLOCK_LU_INSTINIT:
            sprintf(*buf, "UNLOCK_LU_INSTINIT\n");
            break;
#if defined(YAPOR) || defined(THREADS)
        case UNLOCK_LU_YAPOR_THREADS:
            sprintf(*buf, "UNLOCK_LU_YAPOR_THREADS\n");
            break;
#endif
        case UNLOCK_LU_END:
            sprintf(*buf, "UNLOCK_LU_END\n");
            break;
        case ALLOC_FOR_LOGICAL_PRED_INSTINIT:
            sprintf(*buf, "ALLOC_FOR_LOGICAL_PRED_INSTINIT\n");
            break;
#if MULTIPLE_STACKS
        case ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS:
            sprintf(*buf, "ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS\n");
            break;
#if PARALLEL_YAP
        case ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS_PARALLEL:
            sprintf(*buf, "ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS_PARALLEL\n");
            break;
#endif
        case ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS_END:
            sprintf(*buf, "ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS_END\n");
            break;
#else
        case ALLOC_FOR_LOGICAL_PRED_NOMULTIPLE_STACKS_INIT:
            sprintf(*buf, "ALLOC_FOR_LOGICAL_PRED_NOMULTIPLE_STACKS_INIT\n");
            break;
        case ALLOC_FOR_LOGICAL_PRED_NOMULTIPLE_STACKS_IF:
            sprintf(*buf, "ALLOC_FOR_LOGICAL_PRED_NOMULTIPLE_STACKS_IF\n");
            break;
#endif
        case ALLOC_FOR_LOGICAL_PRED_END:
            sprintf(*buf, "ALLOC_FOR_LOGICAL_PRED_END\n");
            break;
        case COPY_IDB_TERM_INSTINIT:
            sprintf(*buf, "COPY_IDB_TERM_INSTINIT\n");
            break;
        case COPY_IDB_TERM_END:
            sprintf(*buf, "COPY_IDB_TERM_END\n");
            break;
        case UNIFY_IDB_TERM_INSTINIT:
            sprintf(*buf, "UNIFY_IDB_TERM_INSTINIT\n");
            break;
        case UNIFY_IDB_TERM_END:
            sprintf(*buf, "UNIFY_IDB_TERM_END\n");
            break;
        case ENSURE_SPACE_INSTINIT:
            sprintf(*buf, "ENSURE_SPACE_INSTINIT\n");
            break;
        case ENSURE_SPACE_END:
            sprintf(*buf, "ENSURE_SPACE_END\n");
            break;
        case SPY_OR_TRYMARK_INSTINIT:
            sprintf(*buf, "SPY_OR_TRYMARK_INSTINIT\n");
            break;
        case TRY_AND_MARK_INSTINIT:
            sprintf(*buf, "TRY_AND_MARK_INSTINIT\n");
            break;
#if defined(YAPOR) || defined(THREADS)
#ifdef YAPOR
        case TRY_AND_MARK_YAPOR_THREADS_YAPOR:
            sprintf(*buf, "TRY_AND_MARK_YAPOR_THREADS_YAPOR\n");
            break;
#endif
        case TRY_AND_MARK_YAPOR_THREADS_NOYAPOR_IF:
            sprintf(*buf, "TRY_AND_MARK_YAPOR_THREADS_NOYAPOR_IF\n");
            break;
#endif
        case TRY_AND_MARK_NOYAPOR_NOTHREADS:
            sprintf(*buf, "TRY_AND_MARK_NOYAPOR_NOTHREADS\n");
            break;
#ifdef YAPOR
        case TRY_AND_MARK_SET_LOAD:
            sprintf(*buf, "TRY_AND_MARK_SET_LOAD\n");
            break;
#endif
        case TRY_AND_MARK_POST_SET_LOAD:
            sprintf(*buf, "TRY_AND_MARK_POST_SET_LOAD\n");
            break;
#if MULTIPLE_STACKS
        case TRY_AND_MARK_MULTIPLE_STACKS:
            sprintf(*buf, "TRY_AND_MARK_MULTIPLE_STACKS\n");
            break;
#else
        case TRY_AND_MARK_NOMULTIPLE_STACKS_IF:
            sprintf(*buf, "TRY_AND_MARK_NOMULTIPLE_STACKS_IF\n");
            break;
#endif
        case TRY_AND_MARK_END:
            sprintf(*buf, "TRY_AND_MARK_END\n");
            break;
        case COUNT_RETRY_AND_MARK_INSTINIT:
            sprintf(*buf, "COUNT_RETRY_AND_MARK_INSTINIT\n");
            break;
        case PROFILED_RETRY_AND_MARK_INSTINIT:
            sprintf(*buf, "PROFILED_RETRY_AND_MARK_INSTINIT\n");
            break;
        case RETRY_AND_MARK_INSTINIT:
            sprintf(*buf, "RETRY_AND_MARK_INSTINIT\n");
            break;
#ifdef YAPOR
        case RETRY_AND_MARK_YAPOR:
            sprintf(*buf, "RETRY_AND_MARK_YAPOR\n");
            break;
#endif
        case RETRY_AND_MARK_POST_YAPOR:
            sprintf(*buf, "RETRY_AND_MARK_POST_YAPOR\n");
            break;
#ifdef FROZEN_STACKS
        case RETRY_AND_MARK_FROZEN:
            sprintf(*buf, "RETRY_AND_MARK_FROZEN\n");
            break;
#else
        case RETRY_AND_MARK_NOFROZEN:
            sprintf(*buf, "RETRY_AND_MARK_NOFROZEN\n");
            break;
#endif
        case RETRY_AND_MARK_POST_FROZEN:
            sprintf(*buf, "RETRY_AND_MARK_POST_FROZEN\n");
            break;
#if MULTIPLE_STACKS
        case RETRY_AND_MARK_MULTIPLE_STACKS:
            sprintf(*buf, "RETRY_AND_MARK_MULTIPLE_STACKS\n");
            break;
#else
        case RETRY_AND_MARK_NOMULTIPLE_STACKS_IF:
            sprintf(*buf, "RETRY_AND_MARK_NOMULTIPLE_STACKS_IF\n");
            break;
#endif
        case RETRY_AND_MARK_END:
            sprintf(*buf, "RETRY_AND_MARK_END\n");
            break;
        case TRUST_FAIL_INSTINIT:
            sprintf(*buf, "TRUST_FAIL_INSTINIT\n");
            break;
#ifdef CUT_C
        case TRUST_FAIL_CUT_C:
            sprintf(*buf, "TRUST_FAIL_CUT_C\n");
            break;
#endif
#ifdef YAPOR
        case TRUST_FAIL_YAPOR:
            sprintf(*buf, "TRUST_FAIL_YAPOR\n");
            break;
#endif
        case TRUST_FAIL_NOYAPOR:
            sprintf(*buf, "TRUST_FAIL_NOYAPOR\n");
            break;
#ifdef YAPOR
        case LBL_SHARED_FAIL:
            sprintf(*buf, "LBL_SHARED_FAIL\n");
            break;
#endif
        case OP_FAIL_INSTINIT:
            sprintf(*buf, "OP_FAIL_INSTINIT\n");
            break;
        case LBL_FAIL_INSTINIT:
            sprintf(*buf, "LBL_FAIL_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case LBL_FAIL_LOW_LEVEL_TRACER:
            sprintf(*buf, "LBL_FAIL_LOW_LEVEL_TRACER\n");
            break;
#endif
        case LBL_FAIL_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "LBL_FAIL_POST_LOW_LEVEL_TRACER\n");
            break;
        case LBL_FAIL_VARTERM:
            sprintf(*buf, "LBL_FAIL_VARTERM\n");
            break;
        case LBL_FAIL_PAIRTERM_INIT:
            sprintf(*buf, "LBL_FAIL_PAIRTERM_INIT\n");
            break;
        case LBL_FAIL_PAIRTERM_END_APPL:
            sprintf(*buf, "LBL_FAIL_PAIRTERM_END_APPL\n");
            break;
        case LBL_FAIL_END:
            sprintf(*buf, "LBL_FAIL_END\n");
            break;
        case CUT_INSTINIT:
            sprintf(*buf, "CUT_INSTINIT\n");
            break;
#ifdef COROUTINING
        case CUT_COROUTINING:
            sprintf(*buf, "CUT_COROUTINING\n");
            break;
#endif
        case CUT_NOCOROUTINING:
            sprintf(*buf, "CUT_NOCOROUTINING\n");
            break;
        case CUT_T_INSTINIT:
            sprintf(*buf, "CUT_T_INSTINIT\n");
            break;
#ifdef COROUTINING
        case CUT_T_COROUTINING:
            sprintf(*buf, "CUT_T_COROUTINING\n");
            break;
#endif
        case CUT_T_NOCOROUTINING:
            sprintf(*buf, "CUT_T_NOCOROUTINING\n");
            break;
        case CUT_E_INSTINIT:
            sprintf(*buf, "CUT_E_INSTINIT\n");
            break;
#ifdef COROUTINING
        case CUT_E_COROUTINING:
            sprintf(*buf, "CUT_E_COROUTINING\n");
            break;
#endif
        case CUT_E_NOCOROUTINING:
            sprintf(*buf, "CUT_E_NOCOROUTINING\n");
            break;
        case SAVE_B_X_INSTINIT:
            sprintf(*buf, "SAVE_B_X_INSTINIT\n");
            break;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
        case SAVE_B_X_YSBA_FROZEN:
            sprintf(*buf, "SAVE_B_X_YSBA_FROZEN\n");
            break;
#else
        case SAVE_B_X_NOYSBA_NOFROZEN:
            sprintf(*buf, "SAVE_B_X_NOYSBA_NOFROZEN\n");
            break;
#endif
        case SAVE_B_X_END:
            sprintf(*buf, "SAVE_B_X_END\n");
            break;
        case SAVE_B_Y_INSTINIT:
            sprintf(*buf, "SAVE_B_Y_INSTINIT\n");
            break;
#if defined(YAPOR_SBA)
        case SAVE_B_Y_YSBA:
            sprintf(*buf, "SAVE_B_Y_YSBA\n");
            break;
#else
        case SAVE_B_Y_NOYSBA:
            sprintf(*buf, "SAVE_B_Y_NOYSBA\n");
            break;
#endif
        case SAVE_B_Y_END:
            sprintf(*buf, "SAVE_B_Y_END\n");
            break;
        case COMMIT_B_X_INSTINIT:
            sprintf(*buf, "COMMIT_B_X_INSTINIT\n");
            break;
        case COMMIT_B_X_DO_COMMIT_B_X:
            sprintf(*buf, "COMMIT_B_X_DO_COMMIT_B_X\n");
            break;
        case COMMIT_B_X_COMMIT_B_X_NVAR:
            sprintf(*buf, "COMMIT_B_X_COMMIT_B_X_NVAR\n");
            break;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
        case COMMIT_B_X_YSBA_FROZEN:
            sprintf(*buf, "COMMIT_B_X_YSBA_FROZEN\n");
            break;
#else
        case COMMIT_B_X_NOYSBA_NOFROZEN:
            sprintf(*buf, "COMMIT_B_X_NOYSBA_NOFROZEN\n");
            break;
#endif
        case COMMIT_B_X_POST_YSBA_FROZEN:
            sprintf(*buf, "COMMIT_B_X_POST_YSBA_FROZEN\n");
            break;
        case COMMIT_B_X_END:
            sprintf(*buf, "COMMIT_B_X_END\n");
            break;
        case COMMIT_B_Y_INSTINIT:
            sprintf(*buf, "COMMIT_B_Y_INSTINIT\n");
            break;
        case COMMIT_B_Y_DO_COMMIT_B_Y:
            sprintf(*buf, "COMMIT_B_Y_DO_COMMIT_B_Y\n");
            break;
        case COMMIT_B_Y_COMMIT_B_Y_NVAR:
            sprintf(*buf, "COMMIT_B_Y_COMMIT_B_Y_NVAR\n");
            break;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
        case COMMIT_B_Y_YSBA_FROZEN:
            sprintf(*buf, "COMMIT_B_Y_YSBA_FROZEN\n");
            break;
#else
        case COMMIT_B_Y_NOYSBA_NOFROZEN:
            sprintf(*buf, "COMMIT_B_Y_NOYSBA_NOFROZEN\n");
            break;
#endif
        case COMMIT_B_Y_POST_YSBA_FROZEN:
            sprintf(*buf, "COMMIT_B_Y_POST_YSBA_FROZEN\n");
            break;
        case COMMIT_B_Y_END:
            sprintf(*buf, "COMMIT_B_Y_END\n");
            break;
        case EXECUTE_INSTINIT:
            sprintf(*buf, "EXECUTE_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case EXECUTE_LOW_LEVEL_TRACER:
            sprintf(*buf, "EXECUTE_LOW_LEVEL_TRACER\n");
            break;
#endif
        case EXECUTE_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "EXECUTE_POST_LOW_LEVEL_TRACER\n");
            break;
        case EXECUTE_POST_NOCHECKING:
            sprintf(*buf, "EXECUTE_POST_NOCHECKING\n");
            break;
#ifdef DEPTH_LIMIT
        case EXECUTE_DEPTH_MINOR:
            sprintf(*buf, "EXECUTE_DEPTH_MINOR\n");
            break;
        case EXECUTE_DEPTH_MOFPRED:
            sprintf(*buf, "EXECUTE_DEPTH_MOFPRED\n");
            break;
        case EXECUTE_DEPTH_END:
            sprintf(*buf, "EXECUTE_DEPTH_END\n");
            break;
#endif
        case EXECUTE_END_END:
            sprintf(*buf, "EXECUTE_END_END\n");
            break;
        case DEXECUTE_INSTINIT:
            sprintf(*buf, "DEXECUTE_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case DEXECUTE_LOW_LEVEL_TRACER:
            sprintf(*buf, "DEXECUTE_LOW_LEVEL_TRACER\n");
            break;
#endif
        case DEXECUTE_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "DEXECUTE_POST_LOW_LEVEL_TRACER\n");
            break;
#ifdef DEPTH_LIMIT
        case DEXECUTE_DEPTH_MINOR:
            sprintf(*buf, "DEXECUTE_DEPTH_MINOR\n");
            break;
        case DEXECUTE_DEPTH_MOFPRED:
            sprintf(*buf, "DEXECUTE_DEPTH_MOFPRED\n");
            break;
        case DEXECUTE_DEPTH_END:
            sprintf(*buf, "DEXECUTE_DEPTH_END\n");
            break;
#endif
        case DEXECUTE_END_END:
            sprintf(*buf, "DEXECUTE_END_END\n");
            break;
        case FCALL_INST:
            break;
        case CALL_INSTINIT:
            sprintf(*buf, "CALL_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case CALL_LOW_LEVEL_TRACER:
            sprintf(*buf, "CALL_LOW_LEVEL_TRACER\n");
            break;
#endif
        case CALL_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "CALL_POST_LOW_LEVEL_TRACER\n");
            break;
        case CALL_POST_NO_CHECKING:
            sprintf(*buf, "CALL_POST_NO_CHECKING\n");
            break;
#ifdef DEPTH_LIMIT
        case CALL_DEPTH_MINOR:
            sprintf(*buf, "CALL_DEPTH_MINOR\n");
            break;
        case CALL_DEPTH_MOFPRED:
            sprintf(*buf, "CALL_DEPTH_MOFPRED\n");
            break;
        case CALL_DEPTH_END:
            sprintf(*buf, "CALL_DEPTH_END\n");
            break;
#endif
        case CALL_END_END:
            sprintf(*buf, "CALL_END_END\n");
            break;
        case PROCCEED_INSTINIT:
            sprintf(*buf, "PROCCEED_INSTINIT\n");
            break;
#ifdef DEPTH_LIMIT
        case PROCCEED_DEPTH:
            sprintf(*buf, "PROCCEED_DEPTH\n");
            break;
#endif
        case PROCCEED_END:
            sprintf(*buf, "PROCCEED_END\n");
            break;
        case ALLOCATE_INSTINIT:
            sprintf(*buf, "ALLOCATE_INSTINIT\n");
            break;
#ifdef DEPTH_LIMIT
        case ALLOCATE_DEPTH:
            sprintf(*buf, "ALLOCATE_DEPTH\n");
            break;
#endif
        case ALLOCATE_END:
            sprintf(*buf, "ALLOCATE_END\n");
            break;
        case DEALLOCATE_INSTINIT:
            sprintf(*buf, "DEALLOCATE_INSTINIT\n");
            break;
        case DEALLOCATE_POST_CHECK:
            sprintf(*buf, "DEALLOCATE_POST_CHECK\n");
            break;
#ifdef DEPTH_LIMIT
        case DEALLOCATE_DEPTH:
            sprintf(*buf, "DEALLOCATE_DEPTH\n");
            break;
#endif
        case DEALLOCATE_FROZEN:
            sprintf(*buf, "DEALLOCATE_FROZEN\n");
            break;
        case DEALLOCATE_POST_FROZEN:
            sprintf(*buf, "DEALLOCATE_POST_FROZEN\n");
            break;
        case DEALLOCATE_END:
            sprintf(*buf, "DEALLOCATE_END\n");
            break;
        case GET_X_VAR_INSTINIT:
            sprintf(*buf, "GET_X_VAR_INSTINIT\n");
            break;
        case GET_Y_VAR_INSTINIT:
            sprintf(*buf, "GET_Y_VAR_INSTINIT\n");
            break;
        case GET_YY_VAR_INSTINIT:
            sprintf(*buf, "GET_YY_VAR_INSTINIT\n");
            break;
        case GET_X_VAL_INSTINIT:
            sprintf(*buf, "GET_X_VAL_INSTINIT\n");
            break;
        case GET_X_VAL_GVALX_NONVAR:
            sprintf(*buf, "GET_X_VAL_GVALX_NONVAR\n");
            break;
        case GET_X_VAL_GVALX_NONVAR_NONVAR:
            sprintf(*buf, "GET_X_VAL_GVALX_NONVAR_NONVAR\n");
            break;
        case GET_X_VAL_GVALX_NONVAR_UNK:
            sprintf(*buf, "GET_X_VAL_GVALX_NONVAR_UNK\n");
            break;
        case GET_X_VAL_GVALX_UNK:
            sprintf(*buf, "GET_X_VAL_GVALX_UNK\n");
            break;
        case GET_X_VAL_GVALX_VAR_NONVAR:
            sprintf(*buf, "GET_X_VAL_GVALX_VAR_NONVAR\n");
            break;
        case GET_X_VAL_GVALX_VAR_UNK:
            sprintf(*buf, "GET_X_VAL_GVALX_VAR_UNK\n");
            break;
        case GET_Y_VAL_INSTINIT:
            sprintf(*buf, "GET_Y_VAL_INSTINIT\n");
            break;
        case GET_Y_VAL_GVALY_NONVAR:
            sprintf(*buf, "GET_Y_VAL_GVALY_NONVAR\n");
            break;
        case GET_Y_VAL_GVALY_NONVAR_NONVAR:
            sprintf(*buf, "GET_Y_VAL_GVALY_NONVAR_NONVAR\n");
            break;
        case GET_Y_VAL_GVALY_NONVAR_UNK:
            sprintf(*buf, "GET_Y_VAL_GVALY_NONVAR_UNK\n");
            break;
        case GET_Y_VAL_GVALY_UNK:
            sprintf(*buf, "GET_Y_VAL_GVALY_UNK\n");
            break;
        case GET_Y_VAL_GVALY_VAR_NONVAR:
            sprintf(*buf, "GET_Y_VAL_GVALY_VAR_NONVAR\n");
            break;
        case GET_Y_VAL_GVALY_VAR_UNK:
            sprintf(*buf, "GET_Y_VAL_GVALY_VAR_UNK\n");
            break;
        case GET_ATOM_INSTINIT:
            sprintf(*buf, "GET_ATOM_INSTINIT\n");
            break;
        case GET_ATOM_GATOM_NONVAR:
            sprintf(*buf, "GET_ATOM_GATOM_NONVAR\n");
            break;
        case GET_ATOM_GATOM_UNK:
            sprintf(*buf, "GET_ATOM_GATOM_UNK\n");
            break;
        case GET_2ATOMS_INSTINIT:
            sprintf(*buf, "GET_2ATOMS_INSTINIT\n");
            break;
        case GET_2ATOMS_GATOM_2UNK:
            sprintf(*buf, "GET_2ATOMS_GATOM_2UNK\n");
            break;
        case GET_2ATOMS_GATOM_2B:
            sprintf(*buf, "GET_2ATOMS_GATOM_2B\n");
            break;
        case GET_2ATOMS_GATOM_2BNONVAR:
            sprintf(*buf, "GET_2ATOMS_GATOM_2BNONVAR\n");
            break;
        case GET_2ATOMS_GATOM_2BUNK:
            sprintf(*buf, "GET_2ATOMS_GATOM_2BUNK\n");
            break;
        case GET_3ATOMS_INSTINIT:
            sprintf(*buf, "GET_3ATOMS_INSTINIT\n");
            break;
        case GET_3ATOMS_GATOM_3UNK:
            sprintf(*buf, "GET_3ATOMS_GATOM_3UNK\n");
            break;
        case GET_3ATOMS_GATOM_3B:
            sprintf(*buf, "GET_3ATOMS_GATOM_3B\n");
            break;
        case GET_3ATOMS_GATOM_3BUNK:
            sprintf(*buf, "GET_3ATOMS_GATOM_3BUNK\n");
            break;
        case GET_3ATOMS_GATOM_3C:
            sprintf(*buf, "GET_3ATOMS_GATOM_3C\n");
            break;
        case GET_3ATOMS_GATOM_3CNONVAR:
            sprintf(*buf, "GET_3ATOMS_GATOM_3CNONVAR\n");
            break;
        case GET_3ATOMS_GATOM_3CUNK:
            sprintf(*buf, "GET_3ATOMS_GATOM_3CUNK\n");
            break;
        case GET_4ATOMS_INSTINIT:
            sprintf(*buf, "GET_4ATOMS_INSTINIT\n");
            break;
        case GET_4ATOMS_GATOM_4UNK:
            sprintf(*buf, "GET_4ATOMS_GATOM_4UNK\n");
            break;
        case GET_4ATOMS_GATOM_4B:
            sprintf(*buf, "GET_4ATOMS_GATOM_4B\n");
            break;
        case GET_4ATOMS_GATOM_4BUNK:
            sprintf(*buf, "GET_4ATOMS_GATOM_4BUNK\n");
            break;
        case GET_4ATOMS_GATOM_4C:
            sprintf(*buf, "GET_4ATOMS_GATOM_4C\n");
            break;
        case GET_4ATOMS_GATOM_4CUNK:
            sprintf(*buf, "GET_4ATOMS_GATOM_4CUNK\n");
            break;
        case GET_4ATOMS_GATOM_4D:
            sprintf(*buf, "GET_4ATOMS_GATOM_4D\n");
            break;
        case GET_4ATOMS_GATOM_4DNONVAR:
            sprintf(*buf, "GET_4ATOMS_GATOM_4DNONVAR\n");
            break;
        case GET_4ATOMS_GATOM_4DUNK:
            sprintf(*buf, "GET_4ATOMS_GATOM_4DUNK\n");
            break;
        case GET_5ATOMS_INSTINIT:
            sprintf(*buf, "GET_5ATOMS_INSTINIT\n");
            break;
        case GET_5ATOMS_GATOM_5UNK:
            sprintf(*buf, "GET_5ATOMS_GATOM_5UNK\n");
            break;
        case GET_5ATOMS_GATOM_5B:
            sprintf(*buf, "GET_5ATOMS_GATOM_5B\n");
            break;
        case GET_5ATOMS_GATOM_5BUNK:
            sprintf(*buf, "GET_5ATOMS_GATOM_5BUNK\n");
            break;
        case GET_5ATOMS_GATOM_5C:
            sprintf(*buf, "GET_5ATOMS_GATOM_5C\n");
            break;
        case GET_5ATOMS_GATOM_5CUNK:
            sprintf(*buf, "GET_5ATOMS_GATOM_5CUNK\n");
            break;
        case GET_5ATOMS_GATOM_5D:
            sprintf(*buf, "GET_5ATOMS_GATOM_5D\n");
            break;
        case GET_5ATOMS_GATOM_5DUNK:
            sprintf(*buf, "GET_5ATOMS_GATOM_5DUNK\n");
            break;
        case GET_5ATOMS_GATOM_5E:
            sprintf(*buf, "GET_5ATOMS_GATOM_5E\n");
            break;
        case GET_5ATOMS_GATOM_5ENONVAR:
            sprintf(*buf, "GET_5ATOMS_GATOM_5ENONVAR\n");
            break;
        case GET_5ATOMS_GATOM_5EUNK:
            sprintf(*buf, "GET_5ATOMS_GATOM_5EUNK\n");
            break;
        case GET_6ATOMS_INSTINIT:
            sprintf(*buf, "GET_6ATOMS_INSTINIT\n");
            break;
        case GET_6ATOMS_GATOM_6UNK:
            sprintf(*buf, "GET_6ATOMS_GATOM_6UNK\n");
            break;
        case GET_6ATOMS_GATOM_6B:
            sprintf(*buf, "GET_6ATOMS_GATOM_6B\n");
            break;
        case GET_6ATOMS_GATOM_6BUNK:
            sprintf(*buf, "GET_6ATOMS_GATOM_6BUNK\n");
            break;
        case GET_6ATOMS_GATOM_6C:
            sprintf(*buf, "GET_6ATOMS_GATOM_6C\n");
            break;
        case GET_6ATOMS_GATOM_6CUNK:
            sprintf(*buf, "GET_6ATOMS_GATOM_6CUNK\n");
            break;
        case GET_6ATOMS_GATOM_6D:
            sprintf(*buf, "GET_6ATOMS_GATOM_6D\n");
            break;
        case GET_6ATOMS_GATOM_6DUNK:
            sprintf(*buf, "GET_6ATOMS_GATOM_6DUNK\n");
            break;
        case GET_6ATOMS_GATOM_6E:
            sprintf(*buf, "GET_6ATOMS_GATOM_6E\n");
            break;
        case GET_6ATOMS_GATOM_6EUNK:
            sprintf(*buf, "GET_6ATOMS_GATOM_6EUNK\n");
            break;
        case GET_6ATOMS_GATOM_6F:
            sprintf(*buf, "GET_6ATOMS_GATOM_6F\n");
            break;
        case GET_6ATOMS_GATOM_6FNONVAR:
            sprintf(*buf, "GET_6ATOMS_GATOM_6FNONVAR\n");
            break;
        case GET_6ATOMS_GATOM_6FUNK:
            sprintf(*buf, "GET_6ATOMS_GATOM_6FUNK\n");
            break;
        case GET_LIST_INSTINIT:
            sprintf(*buf, "GET_LIST_INSTINIT\n");
            break;
        case GET_LIST_GLIST_NONVAR:
            sprintf(*buf, "GET_LIST_GLIST_NONVAR\n");
            break;
        case GET_LIST_GLIST_UNK:
            sprintf(*buf, "GET_LIST_GLIST_UNK\n");
            break;
        case GET_STRUCT_INSTINIT:
            sprintf(*buf, "GET_STRUCT_INSTINIT\n");
            break;
        case GET_STRUCT_GSTRUCT_NONVAR:
            sprintf(*buf, "GET_STRUCT_GSTRUCT_NONVAR\n");
            break;
        case GET_STRUCT_GSTRUCT_UNK:
            sprintf(*buf, "GET_STRUCT_GSTRUCT_UNK\n");
            break;
        case GET_FLOAT_INSTINIT:
            sprintf(*buf, "GET_FLOAT_INSTINIT\n");
            break;
        case GET_FLOAT_GFLOAT_NONVAR:
            sprintf(*buf, "GET_FLOAT_GFLOAT_NONVAR\n");
            break;
        case GET_FLOAT_GFLOAT_UNK:
            sprintf(*buf, "GET_FLOAT_GFLOAT_UNK\n");
            break;
        case GET_LONGINT_INSTINIT:
            sprintf(*buf, "GET_LONGINT_INSTINIT\n");
            break;
        case GET_LONGINT_GLONGINT_NONVAR:
            sprintf(*buf, "GET_LONGINT_GLONGINT_NONVAR\n");
            break;
        case GET_LONGINT_GLONGINT_UNK:
            sprintf(*buf, "GET_LONGINT_GLONGINT_UNK\n");
            break;
#ifdef USE_GMP
        case GET_BIGINT_INSTINIT:
            sprintf(*buf, "GET_BIGINT_INSTINIT\n");
            break;
        case GET_BIGINT_GBIGINT_NONVAR:
            sprintf(*buf, "GET_BIGINT_GBIGINT_NONVAR\n");
            break;
        case GET_BIGINT_GBIGINT_UNK:
            sprintf(*buf, "GET_BIGINT_GBIGINT_UNK\n");
            break;
#endif
        case GET_DBTERM_INSTINIT:
            sprintf(*buf, "GET_DBTERM_INSTINIT\n");
            break;
        case GET_DBTERM_GDBTERM_NONVAR:
            sprintf(*buf, "GET_DBTERM_GDBTERM_NONVAR\n");
            break;
        case GET_DBTERM_GDBTERM_UNK:
            sprintf(*buf, "GET_DBTERM_GDBTERM_UNK\n");
            break;
        case GLIST_VALX_INSTINIT:
            sprintf(*buf, "GLIST_VALX_INSTINIT\n");
            break;
        case GLIST_VALX_GLIST_VALX_READ:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_READ\n");
            break;
        case GLIST_VALX_GLIST_VALX_NONVAR:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_NONVAR\n");
            break;
        case GLIST_VALX_GLIST_VALX_NONVAR_NONVAR:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_NONVAR_NONVAR\n");
            break;
        case GLIST_VALX_GLIST_VALX_NONVAR_UNK:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_NONVAR_UNK\n");
            break;
        case GLIST_VALX_GLIST_VALX_UNK:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_UNK\n");
            break;
        case GLIST_VALX_GLIST_VALX_VAR_NONVAR:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_VAR_NONVAR\n");
            break;
        case GLIST_VALX_GLIST_VALX_VAR_UNK:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_VAR_UNK\n");
            break;
        case GLIST_VALX_GLIST_VALX_WRITE:
            sprintf(*buf, "GLIST_VALX_GLIST_VALX_WRITE\n");
            break;
        case GLIST_VALY_INSTINIT:
            sprintf(*buf, "GLIST_VALY_INSTINIT\n");
            break;
        case GLIST_VALY_GLIST_VALY_READ:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_READ\n");
            break;
        case GLIST_VALY_GLIST_VALY_NONVAR:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_NONVAR\n");
            break;
        case GLIST_VALY_GLIST_VALY_NONVAR_NONVAR:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_NONVAR_NONVAR\n");
            break;
        case GLIST_VALY_GLIST_VALY_NONVAR_UNK:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_NONVAR_UNK\n");
            break;
        case GLIST_VALY_GLIST_VALY_UNK:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_UNK\n");
            break;
        case GLIST_VALY_GLIST_VALY_VAR_NONVAR:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_VAR_NONVAR\n");
            break;
        case GLIST_VALY_GLIST_VALY_VAR_UNK:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_VAR_UNK\n");
            break;
        case GLIST_VALY_GLIST_VALY_WRITE:
            sprintf(*buf, "GLIST_VALY_GLIST_VALY_WRITE\n");
            break;
        case GL_VOID_VARX_INSTINIT:
            sprintf(*buf, "GL_VOID_VARX_INSTINIT\n");
            break;
        case GL_VOID_VARX_GLIST_VOID_VARX_READ:
            sprintf(*buf, "GL_VOID_VARX_GLIST_VOID_VARX_READ\n");
            break;
        case GL_VOID_VARX_GLIST_VOID_VAR_WRITE:
            sprintf(*buf, "GL_VOID_VARX_GLIST_VOID_VAR_WRITE\n");
            break;
        case GL_VOID_VARY_INSTINIT:
            sprintf(*buf, "GL_VOID_VARY_INSTINIT\n");
            break;
        case GL_VOID_VARY_GLIST_VOID_VARY_READ:
            sprintf(*buf, "GL_VOID_VARY_GLIST_VOID_VARY_READ\n");
            break;
        case GL_VOID_VARY_GLIST_VOID_VARY_WRITE:
            sprintf(*buf, "GL_VOID_VARY_GLIST_VOID_VARY_WRITE\n");
            break;
        case GL_VOID_VALX_INSTINIT:
            sprintf(*buf, "GL_VOID_VALX_INSTINIT\n");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_READ:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_READ\n");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_NONVAR:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_NONVAR\n");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_NONVAR_NONVAR:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_NONVAR_NONVAR\n");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_NONVAR_UNK:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_NONVAR_UNK\n");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_UNK:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_UNK\n");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_VAR_NONVAR:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_VAR_NONVAR\n");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_VAR_UNK:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_VAR_UNK\n");
            break;
        case GL_VOID_VALX_GLIST_VOID_VALX_WRITE:
            sprintf(*buf, "GL_VOID_VALX_GLIST_VOID_VALX_WRITE\n");
            break;
        case GL_VOID_VALY_INSTINIT:
            sprintf(*buf, "GL_VOID_VALY_INSTINIT\n");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_READ:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_READ\n");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_NONVAR:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_NONVAR\n");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_NONVAR_NONVAR:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_NONVAR_NONVAR\n");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_NONVAR_UNK:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_NONVAR_UNK\n");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_UNK:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_UNK\n");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_VAR_NONVAR:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_VAR_NONVAR\n");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_VAR_UNK:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_VAR_UNK\n");
            break;
        case GL_VOID_VALY_GLIST_VOID_VALY_WRITE:
            sprintf(*buf, "GL_VOID_VALY_GLIST_VOID_VALY_WRITE\n");
            break;
        case UNIFY_X_VAR_INSTINIT:
            sprintf(*buf, "UNIFY_X_VAR_INSTINIT\n");
            break;
#ifdef YAPOR_SBA
        case UNIFY_X_VAR_YAPOR_SBA:
            sprintf(*buf, "UNIFY_X_VAR_YAPOR_SBA\n");
            break;
#endif
        case UNIFY_X_VAR_END:
            sprintf(*buf, "UNIFY_X_VAR_END\n");
            break;
        case UNIFY_X_VAR_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_X_VAR_WRITE_INSTINIT\n");
            break;
        case UNIFY_L_X_VAR_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_VAR_INSTINIT\n");
            break;
#ifdef YAPOR_SBA
        case UNIFY_L_X_VAR_YAPOR_SBA:
            sprintf(*buf, "UNIFY_L_X_VAR_YAPOR_SBA\n");
            break;
#endif
        case UNIFY_L_X_VAR_END:
            sprintf(*buf, "UNIFY_L_X_VAR_END\n");
            break;
        case UNIFY_L_X_VAR_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_VAR_WRITE_INSTINIT\n");
            break;
        case UNIFY_X_VAR2_INSTINIT:
            sprintf(*buf, "UNIFY_X_VAR2_INSTINIT\n");
            break;
#ifdef YAPOR_SBA
        case UNIFY_X_VAR2_YAPOR_SBA:
            sprintf(*buf, "UNIFY_X_VAR2_YAPOR_SBA\n");
            break;
#endif
        case UNIFY_X_VAR2_END:
            sprintf(*buf, "UNIFY_X_VAR2_END\n");
            break;
        case UNIFY_X_VAR2_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_X_VAR2_WRITE_INSTINIT\n");
            break;
        case UNIFY_L_X_VAR2_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_VAR2_INSTINIT\n");
            break;
        case UNIFY_L_X_VAR2_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_VAR2_WRITE_INSTINIT\n");
            break;
        case UNIFY_Y_VAR_INSTINIT:
            sprintf(*buf, "UNIFY_Y_VAR_INSTINIT\n");
            break;
        case UNIFY_Y_VAR_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_Y_VAR_WRITE_INSTINIT\n");
            break;
        case UNIFY_L_Y_VAR_INSTINIT:
            sprintf(*buf, "UNIFY_L_Y_VAR_INSTINIT\n");
            break;
        case UNIFY_L_Y_VAR_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_Y_VAR_WRITE_INSTINIT\n");
            break;
        case UNIFY_X_VAL_INSTINIT:
            sprintf(*buf, "UNIFY_X_VAL_INSTINIT\n");
            break;
        case UNIFY_X_VAL_UVALX_NONVAR:
            sprintf(*buf, "UNIFY_X_VAL_UVALX_NONVAR\n");
            break;
        case UNIFY_X_VAL_UVALX_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_X_VAL_UVALX_NONVAR_NONVAR\n");
            break;
        case UNIFY_X_VAL_UVALX_NONVAR_UNK:
            sprintf(*buf, "UNIFY_X_VAL_UVALX_NONVAR_UNK\n");
            break;
        case UNIFY_X_VAL_UVALX_UNK:
            sprintf(*buf, "UNIFY_X_VAL_UVALX_UNK\n");
            break;
        case UNIFY_X_VAL_UVALX_VAR_NONVAR:
            sprintf(*buf, "UNIFY_X_VAL_UVALX_VAR_NONVAR\n");
            break;
        case UNIFY_X_VAL_UVALX_VAR_UNK:
            sprintf(*buf, "UNIFY_X_VAL_UVALX_VAR_UNK\n");
            break;
        case UNIFY_X_VAL_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_X_VAL_WRITE_INSTINIT\n");
            break;
        case UNIFY_L_X_VAL_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_VAL_INSTINIT\n");
            break;
        case UNIFY_L_X_VAL_ULVALX_NONVAR:
            sprintf(*buf, "UNIFY_L_X_VAL_ULVALX_NONVAR\n");
            break;
        case UNIFY_L_X_VAL_ULVALX_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_L_X_VAL_ULVALX_NONVAR_NONVAR\n");
            break;
        case UNIFY_L_X_VAL_ULVALX_NONVAR_UNK:
            sprintf(*buf, "UNIFY_L_X_VAL_ULVALX_NONVAR_UNK\n");
            break;
        case UNIFY_L_X_VAL_ULVALX_UNK:
            sprintf(*buf, "UNIFY_L_X_VAL_ULVALX_UNK\n");
            break;
        case UNIFY_L_X_VAL_ULVALX_VAR_NONVAR:
            sprintf(*buf, "UNIFY_L_X_VAL_ULVALX_VAR_NONVAR\n");
            break;
        case UNIFY_L_X_VAL_ULVALX_VAR_UNK:
            sprintf(*buf, "UNIFY_L_X_VAL_ULVALX_VAR_UNK\n");
            break;
        case UNIFY_L_X_VAL_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_VAL_WRITE_INSTINIT\n");
            break;
        case UNIFY_Y_VAL_INSTINIT:
            sprintf(*buf, "UNIFY_Y_VAL_INSTINIT\n");
            break;
        case UNIFY_Y_VAL_UVALY_NONVAR:
            sprintf(*buf, "UNIFY_Y_VAL_UVALY_NONVAR\n");
            break;
        case UNIFY_Y_VAL_UVALY_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_Y_VAL_UVALY_NONVAR_NONVAR\n");
            break;
        case UNIFY_Y_VAL_UVALY_NONVAR_UNK:
            sprintf(*buf, "UNIFY_Y_VAL_UVALY_NONVAR_UNK\n");
            break;
        case UNIFY_Y_VAL_UVALY_UNK:
            sprintf(*buf, "UNIFY_Y_VAL_UVALY_UNK\n");
            break;
        case UNIFY_Y_VAL_UVALY_VAR_NONVAR:
            sprintf(*buf, "UNIFY_Y_VAL_UVALY_VAR_NONVAR\n");
            break;
        case UNIFY_Y_VAL_UVALY_VAR_UNK:
            sprintf(*buf, "UNIFY_Y_VAL_UVALY_VAR_UNK\n");
            break;
        case UNIFY_Y_VAL_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_Y_VAL_WRITE_INSTINIT\n");
            break;
        case UNIFY_L_Y_VAL_INSTINIT:
            sprintf(*buf, "UNIFY_L_Y_VAL_INSTINIT\n");
            break;
        case UNIFY_L_Y_VAL_ULVALY_NONVAR:
            sprintf(*buf, "UNIFY_L_Y_VAL_ULVALY_NONVAR\n");
            break;
        case UNIFY_L_Y_VAL_ULVALY_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_L_Y_VAL_ULVALY_NONVAR_NONVAR\n");
            break;
        case UNIFY_L_Y_VAL_ULVALY_NONVAR_UNK:
            sprintf(*buf, "UNIFY_L_Y_VAL_ULVALY_NONVAR_UNK\n");
            break;
        case UNIFY_L_Y_VAL_ULVALY_UNK:
            sprintf(*buf, "UNIFY_L_Y_VAL_ULVALY_UNK\n");
            break;
        case UNIFY_L_Y_VAL_ULVALY_VAR_NONVAR:
            sprintf(*buf, "UNIFY_L_Y_VAL_ULVALY_VAR_NONVAR\n");
            break;
        case UNIFY_L_Y_VAL_ULVALY_VAR_UNK:
            sprintf(*buf, "UNIFY_L_Y_VAL_ULVALY_VAR_UNK\n");
            break;
        case UNIFY_L_Y_VAL_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_Y_VAL_WRITE_INSTINIT\n");
            break;
        case UNIFY_X_LOC_INSTINIT:
            sprintf(*buf, "UNIFY_X_LOC_INSTINIT\n");
            break;
        case UNIFY_X_LOC_UVALX_LOC_NONVAR:
            sprintf(*buf, "UNIFY_X_LOC_UVALX_LOC_NONVAR\n");
            break;
        case UNIFY_X_LOC_UVALX_LOC_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_X_LOC_UVALX_LOC_NONVAR_NONVAR\n");
            break;
        case UNIFY_X_LOC_UVALX_LOC_NONVAR_UNK:
            sprintf(*buf, "UNIFY_X_LOC_UVALX_LOC_NONVAR_UNK\n");
            break;
        case UNIFY_X_LOC_UVALX_LOC_UNK:
            sprintf(*buf, "UNIFY_X_LOC_UVALX_LOC_UNK\n");
            break;
        case UNIFY_X_LOC_UVALX_LOC_VAR_NONVAR:
            sprintf(*buf, "UNIFY_X_LOC_UVALX_LOC_VAR_NONVAR\n");
            break;
        case UNIFY_X_LOC_UVALX_LOC_VAR_UNK:
            sprintf(*buf, "UNIFY_X_LOC_UVALX_LOC_VAR_UNK\n");
            break;
        case UNIFY_X_LOC_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_X_LOC_WRITE_INSTINIT\n");
            break;
        case UNIFY_X_LOC_WRITE_UNIFY_X_LOC_NONVAR:
            sprintf(*buf, "UNIFY_X_LOC_WRITE_UNIFY_X_LOC_NONVAR\n");
            break;
        case UNIFY_X_LOC_WRITE_UNIFY_X_LOC_UNK:
            sprintf(*buf, "UNIFY_X_LOC_WRITE_UNIFY_X_LOC_UNK\n");
            break;
        case UNIFY_L_X_LOC_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_LOC_INSTINIT\n");
            break;
        case UNIFY_L_X_LOC_ULVALX_LOC_NONVAR:
            sprintf(*buf, "UNIFY_L_X_LOC_ULVALX_LOC_NONVAR\n");
            break;
        case UNIFY_L_X_LOC_ULVALX_LOC_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_L_X_LOC_ULVALX_LOC_NONVAR_NONVAR\n");
            break;
        case UNIFY_L_X_LOC_ULVALX_LOC_NONVAR_UNK:
            sprintf(*buf, "UNIFY_L_X_LOC_ULVALX_LOC_NONVAR_UNK\n");
            break;
        case UNIFY_L_X_LOC_ULVALX_LOC_UNK:
            sprintf(*buf, "UNIFY_L_X_LOC_ULVALX_LOC_UNK\n");
            break;
        case UNIFY_L_X_LOC_ULVALX_LOC_VAR_NONVAR:
            sprintf(*buf, "UNIFY_L_X_LOC_ULVALX_LOC_VAR_NONVAR\n");
            break;
        case UNIFY_L_X_LOC_ULVALX_LOC_VAR_UNK:
            sprintf(*buf, "UNIFY_L_X_LOC_ULVALX_LOC_VAR_UNK\n");
            break;
        case UNIFY_L_X_LOC_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_X_LOC_WRITE_INSTINIT\n");
            break;
        case UNIFY_L_X_LOC_WRITE_ULNIFY_X_LOC_NONVAR:
            sprintf(*buf, "UNIFY_L_X_LOC_WRITE_ULNIFY_X_LOC_NONVAR\n");
            break;
        case UNIFY_L_X_LOC_WRITE_ULNIFY_X_LOC_UNK:
            sprintf(*buf, "UNIFY_L_X_LOC_WRITE_ULNIFY_X_LOC_UNK\n");
            break;
        case UNIFY_Y_LOC_INSTINIT:
            sprintf(*buf, "UNIFY_Y_LOC_INSTINIT\n");
            break;
        case UNIFY_Y_LOC_UVALY_LOC_NONVAR:
            sprintf(*buf, "UNIFY_Y_LOC_UVALY_LOC_NONVAR\n");
            break;
        case UNIFY_Y_LOC_UVALY_LOC_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_Y_LOC_UVALY_LOC_NONVAR_NONVAR\n");
            break;
        case UNIFY_Y_LOC_UVALY_LOC_NONVAR_UNK:
            sprintf(*buf, "UNIFY_Y_LOC_UVALY_LOC_NONVAR_UNK\n");
            break;
        case UNIFY_Y_LOC_UVALY_LOC_UNK:
            sprintf(*buf, "UNIFY_Y_LOC_UVALY_LOC_UNK\n");
            break;
        case UNIFY_Y_LOC_UVALY_LOC_VAR_NONVAR:
            sprintf(*buf, "UNIFY_Y_LOC_UVALY_LOC_VAR_NONVAR\n");
            break;
        case UNIFY_Y_LOC_UVALY_LOC_VAR_UNK:
            sprintf(*buf, "UNIFY_Y_LOC_UVALY_LOC_VAR_UNK\n");
            break;
        case UNIFY_Y_LOC_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_Y_LOC_WRITE_INSTINIT\n");
            break;
        case UNIFY_Y_LOC_WRITE_UNIFY_Y_LOC_NONVAR:
            sprintf(*buf, "UNIFY_Y_LOC_WRITE_UNIFY_Y_LOC_NONVAR\n");
            break;
        case UNIFY_Y_LOC_WRITE_UNIFY_Y_LOC_UNK:
            sprintf(*buf, "UNIFY_Y_LOC_WRITE_UNIFY_Y_LOC_UNK\n");
            break;
        case UNIFY_L_Y_LOC_INSTINIT:
            sprintf(*buf, "UNIFY_L_Y_LOC_INSTINIT\n");
            break;
        case UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR:
            sprintf(*buf, "UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR\n");
            break;
        case UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR_NONVAR:
            sprintf(*buf, "UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR_NONVAR\n");
            break;
        case UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR_UNK:
            sprintf(*buf, "UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR_UNK\n");
            break;
        case UNIFY_L_Y_LOC_ULVALY_LOC_UNK:
            sprintf(*buf, "UNIFY_L_Y_LOC_ULVALY_LOC_UNK\n");
            break;
        case UNIFY_L_Y_LOC_ULVALY_LOC_VAR_NONVAR:
            sprintf(*buf, "UNIFY_L_Y_LOC_ULVALY_LOC_VAR_NONVAR\n");
            break;
        case UNIFY_L_Y_LOC_ULVALY_LOC_VAR_UNK:
            sprintf(*buf, "UNIFY_L_Y_LOC_ULVALY_LOC_VAR_UNK\n");
            break;
        case UNIFY_L_Y_LOC_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_Y_LOC_WRITE_INSTINIT\n");
            break;
        case UNIFY_L_Y_LOC_WRITE_ULUNIFY_Y_LOC_NONVAR:
            sprintf(*buf, "UNIFY_L_Y_LOC_WRITE_ULUNIFY_Y_LOC_NONVAR\n");
            break;
        case UNIFY_L_Y_LOC_WRITE_ULUNIFY_Y_LOC_UNK:
            sprintf(*buf, "UNIFY_L_Y_LOC_WRITE_ULUNIFY_Y_LOC_UNK\n");
            break;
        case UNIFY_VOID_INSTINIT:
            sprintf(*buf, "UNIFY_VOID_INSTINIT\n");
            break;
        case UNIFY_VOID_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_VOID_WRITE_INSTINIT\n");
            break;
        case UNIFY_L_VOID_INSTINIT:
            sprintf(*buf, "UNIFY_L_VOID_INSTINIT\n");
            break;
        case UNIFY_L_VOID_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_VOID_WRITE_INSTINIT\n");
            break;
        case UNIFY_N_VOIDS_INSTINIT:
            sprintf(*buf, "UNIFY_N_VOIDS_INSTINIT\n");
            break;
        case UNIFY_N_VOIDS_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_N_VOIDS_WRITE_INSTINIT\n");
            break;
        case UNIFY_L_N_VOIDS_INSTINIT:
            sprintf(*buf, "UNIFY_L_N_VOIDS_INSTINIT\n");
            break;
        case UNIFY_L_N_VOIDS_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_N_VOIDS_WRITE_INSTINIT\n");
            break;
        case UNIFY_ATOM_INSTINIT:
            sprintf(*buf, "UNIFY_ATOM_INSTINIT\n");
            break;
        case UNIFY_ATOM_UATOM_NONVAR:
            sprintf(*buf, "UNIFY_ATOM_UATOM_NONVAR\n");
            break;
        case UNIFY_ATOM_UATOM_UNK:
            sprintf(*buf, "UNIFY_ATOM_UATOM_UNK\n");
            break;
        case UNIFY_ATOM_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_ATOM_WRITE_INSTINIT\n");
            break;
        case UNIFY_L_ATOM_INSTINIT:
            sprintf(*buf, "UNIFY_L_ATOM_INSTINIT\n");
            break;
        case UNIFY_L_ATOM_ULATOM_NONVAR:
            sprintf(*buf, "UNIFY_L_ATOM_ULATOM_NONVAR\n");
            break;
        case UNIFY_L_ATOM_ULATOM_UNK:
            sprintf(*buf, "UNIFY_L_ATOM_ULATOM_UNK\n");
            break;
        case UNIFY_L_ATOM_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_ATOM_WRITE_INSTINIT\n");
            break;
        case UNIFY_N_ATOMS_INSTINIT:
            sprintf(*buf, "UNIFY_N_ATOMS_INSTINIT\n");
            break;
        case UNIFY_N_ATOMS_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_N_ATOMS_WRITE_INSTINIT\n");
            break;
        case UNIFY_FLOAT_INSTINIT:
            sprintf(*buf, "UNIFY_FLOAT_INSTINIT\n");
            break;
        case UNIFY_FLOAT_UFLOAT_NONVAR_INIT:
            sprintf(*buf, "UNIFY_FLOAT_UFLOAT_NONVAR_INIT\n");
            break;
        case UNIFY_FLOAT_UFLOAT_NONVAR_D0ISFUNCTOR:
            sprintf(*buf, "UNIFY_FLOAT_UFLOAT_NONVAR_D0ISFUNCTOR\n");
            break;
        case UNIFY_FLOAT_UFLOAT_NONVAR_END:
            sprintf(*buf, "UNIFY_FLOAT_UFLOAT_NONVAR_END\n");
            break;
        case UNIFY_FLOAT_UFLOAT_UNK:
            sprintf(*buf, "UNIFY_FLOAT_UFLOAT_UNK\n");
            break;
        case UNIFY_FLOAT_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_FLOAT_WRITE_INSTINIT\n");
            break;
        case UNIFY_L_FLOAT_INSTINIT:
            sprintf(*buf, "UNIFY_L_FLOAT_INSTINIT\n");
            break;
        case UNIFY_L_FLOAT_D0ISAPPL:
            sprintf(*buf, "UNIFY_L_FLOAT_D0ISAPPL\n");
            break;
        case UNIFY_L_FLOAT_D0ISFUNC:
            sprintf(*buf, "UNIFY_L_FLOAT_D0ISFUNC\n");
            break;
        case UNIFY_L_FLOAT_EQUALS:
            sprintf(*buf, "UNIFY_L_FLOAT_EQUALS\n");
            break;
        case UNIFY_L_FLOAT_ULFLOAT_UNK:
            sprintf(*buf, "UNIFY_L_FLOAT_ULFLOAT_UNK\n");
            break;
        case UNIFY_L_FLOAT_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_FLOAT_WRITE_INSTINIT\n");
            break;
        case UNIFY_LONGINT_INSTINIT:
            sprintf(*buf, "UNIFY_LONGINT_INSTINIT\n");
            break;
        case UNIFY_LONGINT_D0ISAPPL:
            sprintf(*buf, "UNIFY_LONGINT_D0ISAPPL\n");
            break;
        case UNIFY_LONGINT_D0ISFUNC:
            sprintf(*buf, "UNIFY_LONGINT_D0ISFUNC\n");
            break;
        case UNIFY_LONGINT_EQUALS:
            sprintf(*buf, "UNIFY_LONGINT_EQUALS\n");
            break;
        case UNIFY_LONGINT_ULONGINT_UNK:
            sprintf(*buf, "UNIFY_LONGINT_ULONGINT_UNK\n");
            break;
        case UNIFY_LONGINT_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_LONGINT_WRITE_INSTINIT\n");
            break;
        case UNIFY_L_LONGINT_INSTINIT:
            sprintf(*buf, "UNIFY_L_LONGINT_INSTINIT\n");
            break;
        case UNIFY_L_LONGINT_D0ISAPPL:
            sprintf(*buf, "UNIFY_L_LONGINT_D0ISAPPL\n");
            break;
        case UNIFY_L_LONGINT_D0ISFUNC:
            sprintf(*buf, "UNIFY_L_LONGINT_D0ISFUNC\n");
            break;
        case UNIFY_L_LONGINT_EQUALS:
            sprintf(*buf, "UNIFY_L_LONGINT_EQUALS\n");
            break;
        case UNIFY_L_LONGINT_ULLONGINT_UNK:
            sprintf(*buf, "UNIFY_L_LONGINT_ULLONGINT_UNK\n");
            break;
        case UNIFY_L_LONGINT_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_LONGINT_WRITE_INSTINIT\n");
            break;
#ifdef USE_GMP
        case UNIFY_BIGINT_INSTINIT:
            sprintf(*buf, "UNIFY_BIGINT_INSTINIT\n");
            break;
        case UNIFY_BIGINT_D0ISAPPL:
            sprintf(*buf, "UNIFY_BIGINT_D0ISAPPL\n");
            break;
        case UNIFY_BIGINT_D1ISFUNC_GMP:
            sprintf(*buf, "UNIFY_BIGINT_D1ISFUNC_GMP\n");
            break;
        case UNIFY_BIGINT_UBIGINT_UNK:
            sprintf(*buf, "UNIFY_BIGINT_UBIGINT_UNK\n");
            break;
        case UNIFY_L_BIGINT_INSTINIT:
            sprintf(*buf, "UNIFY_L_BIGINT_INSTINIT\n");
            break;
        case UNIFY_L_BIGINT_D0ISAPPL:
            sprintf(*buf, "UNIFY_L_BIGINT_D0ISAPPL\n");
            break;
        case UNIFY_L_BIGINT_D0ISFUNC_GMP:
            sprintf(*buf, "UNIFY_L_BIGINT_D0ISFUNC_GMP\n");
            break;
        case UNIFY_L_BIGINT_ULBIGINT_UNK:
            sprintf(*buf, "UNIFY_L_BIGINT_ULBIGINT_UNK\n");
            break;
#endif
        case UNIFY_DBTERM_INSTINIT:
            sprintf(*buf, "UNIFY_DBTERM_INSTINIT\n");
            break;
        case UNIFY_DBTERM_UDBTERM_NONVAR:
            sprintf(*buf, "UNIFY_DBTERM_UDBTERM_NONVAR\n");
            break;
        case UNIFY_DBTERM_UDBTERM_UNK:
            sprintf(*buf, "UNIFY_DBTERM_UDBTERM_UNK\n");
            break;
        case UNIFY_L_DBTERM_INSTINIT:
            sprintf(*buf, "UNIFY_L_DBTERM_INSTINIT\n");
            break;
        case UNIFY_L_DBTERM_ULDBTERM_NONVAR:
            sprintf(*buf, "UNIFY_L_DBTERM_ULDBTERM_NONVAR\n");
            break;
        case UNIFY_L_DBTERM_ULDBTERM_UNK:
            sprintf(*buf, "UNIFY_L_DBTERM_ULDBTERM_UNK\n");
            break;
        case UNIFY_LIST_INSTINIT:
            sprintf(*buf, "UNIFY_LIST_INSTINIT\n");
            break;
        case UNIFY_LIST_READMODE:
            sprintf(*buf, "UNIFY_LIST_READMODE\n");
            break;
        case UNIFY_LIST_WRITEMODE:
            sprintf(*buf, "UNIFY_LIST_WRITEMODE\n");
            break;
        case UNIFY_LIST_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_LIST_WRITE_INSTINIT\n");
            break;
        case UNIFY_L_LIST_INSTINIT:
            sprintf(*buf, "UNIFY_L_LIST_INSTINIT\n");
            break;
        case UNIFY_L_LIST_READMODE:
            sprintf(*buf, "UNIFY_L_LIST_READMODE\n");
            break;
        case UNIFY_L_LIST_WRITEMODE:
            sprintf(*buf, "UNIFY_L_LIST_WRITEMODE\n");
            break;
        case UNIFY_L_LIST_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_LIST_WRITE_INSTINIT\n");
            break;
        case UNIFY_STRUCT_INSTINIT:
            sprintf(*buf, "UNIFY_STRUCT_INSTINIT\n");
            break;
        case UNIFY_STRUCT_READMODE:
            sprintf(*buf, "UNIFY_STRUCT_READMODE\n");
            break;
        case UNIFY_STRUCT_WRITEMODE:
            sprintf(*buf, "UNIFY_STRUCT_WRITEMODE\n");
            break;
        case UNIFY_STRUCT_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_STRUCT_WRITE_INSTINIT\n");
            break;
        case UNIFY_L_STRUC_INSTINIT:
            sprintf(*buf, "UNIFY_L_STRUC_INSTINIT\n");
            break;
        case UNIFY_L_STRUC_READMODE:
            sprintf(*buf, "UNIFY_L_STRUC_READMODE\n");
            break;
        case UNIFY_L_STRUC_WRITEMODE:
            sprintf(*buf, "UNIFY_L_STRUC_WRITEMODE\n");
            break;
        case UNIFY_L_STRUC_WRITE_INSTINIT:
            sprintf(*buf, "UNIFY_L_STRUC_WRITE_INSTINIT\n");
            break;
        case PUT_X_VAR_INSTINIT:
            sprintf(*buf, "PUT_X_VAR_INSTINIT\n");
            break;
        case PUT_Y_VAR_INSTINIT:
            sprintf(*buf, "PUT_Y_VAR_INSTINIT\n");
            break;
        case PUT_X_VAL_INSTINIT:
            sprintf(*buf, "PUT_X_VAL_INSTINIT\n");
            break;
        case PUT_XX_VAL_INSTINIT:
            sprintf(*buf, "PUT_XX_VAL_INSTINIT\n");
            break;
        case PUT_Y_VAL_INSTINIT:
            sprintf(*buf, "PUT_Y_VAL_INSTINIT\n");
            break;
        case PUT_Y_VALS_INSTINIT:
            sprintf(*buf, "PUT_Y_VALS_INSTINIT\n");
            break;
        case PUT_UNSAFE_INSTINIT:
            sprintf(*buf, "PUT_UNSAFE_INSTINIT\n");
            break;
        case PUT_UNSAFE_PUNSAFE_NONVAR:
            sprintf(*buf, "PUT_UNSAFE_PUNSAFE_NONVAR\n");
            break;
        case PUT_UNSAFE_PUNSAFE_UNK:
            sprintf(*buf, "PUT_UNSAFE_PUNSAFE_UNK\n");
            break;
        case PUT_ATOM_INSTINIT:
            sprintf(*buf, "PUT_ATOM_INSTINIT\n");
            break;
        case PUT_DBTERM_INSTINIT:
            sprintf(*buf, "PUT_DBTERM_INSTINIT\n");
            break;
        case PUT_BIGINT_INSTINIT:
            sprintf(*buf, "PUT_BIGINT_INSTINIT\n");
            break;
        case PUT_FLOAT_INSTINIT:
            sprintf(*buf, "PUT_FLOAT_INSTINIT\n");
            break;
        case PUT_LONGINT_INSTINIT:
            sprintf(*buf, "PUT_LONGINT_INSTINIT\n");
            break;
        case PUT_LIST_INSTINIT:
            sprintf(*buf, "PUT_LIST_INSTINIT\n");
            break;
        case PUT_STRUCT_INSTINIT:
            sprintf(*buf, "PUT_STRUCT_INSTINIT\n");
            break;
        case WRITE_X_VAR_INSTINIT:
            sprintf(*buf, "WRITE_X_VAR_INSTINIT\n");
            break;
        case WRITE_VOID_INSTINIT:
            sprintf(*buf, "WRITE_VOID_INSTINIT\n");
            break;
        case WRITE_N_VOIDS_INSTINIT:
            sprintf(*buf, "WRITE_N_VOIDS_INSTINIT\n");
            break;
        case WRITE_Y_VAR_INSTINIT:
            sprintf(*buf, "WRITE_Y_VAR_INSTINIT\n");
            break;
        case WRITE_X_VAL_INSTINIT:
            sprintf(*buf, "WRITE_X_VAL_INSTINIT\n");
            break;
        case WRITE_X_LOC_INSTINIT:
            sprintf(*buf, "WRITE_X_LOC_INSTINIT\n");
            break;
        case WRITE_X_LOC_W_X_BOUND:
            sprintf(*buf, "WRITE_X_LOC_W_X_BOUND\n");
            break;
        case WRITE_X_LOC_W_X_UNK:
            sprintf(*buf, "WRITE_X_LOC_W_X_UNK\n");
            break;
        case WRITE_Y_VAL_INSTINIT:
            sprintf(*buf, "WRITE_Y_VAL_INSTINIT\n");
            break;
        case WRITE_Y_LOC_INSTINIT:
            sprintf(*buf, "WRITE_Y_LOC_INSTINIT\n");
            break;
        case WRITE_Y_LOC_W_Y_BOUND:
            sprintf(*buf, "WRITE_Y_LOC_W_Y_BOUND\n");
            break;
        case WRITE_Y_LOC_W_Y_UNK:
            sprintf(*buf, "WRITE_Y_LOC_W_Y_UNK\n");
            break;
        case WRITE_ATOM_INSTINIT:
            sprintf(*buf, "WRITE_ATOM_INSTINIT\n");
            break;
        case WRITE_BIGINT_INSTINIT:
            sprintf(*buf, "WRITE_BIGINT_INSTINIT\n");
            break;
        case WRITE_DBTERM_INSTINIT:
            sprintf(*buf, "WRITE_DBTERM_INSTINIT\n");
            break;
        case WRITE_FLOAT_INSTINIT:
            sprintf(*buf, "WRITE_FLOAT_INSTINIT\n");
            break;
        case WRITE_LONGIT_INSTINIT:
            sprintf(*buf, "WRITE_LONGIT_INSTINIT\n");
            break;
        case WRITE_N_ATOMS_INSTINIT:
            sprintf(*buf, "WRITE_N_ATOMS_INSTINIT\n");
            break;
        case WRITE_LIST_INSTINIT:
            sprintf(*buf, "WRITE_LIST_INSTINIT\n");
            break;
        case WRITE_L_LIST_INSTINIT:
            sprintf(*buf, "WRITE_L_LIST_INSTINIT\n");
            break;
        case WRITE_STRUCT_INSTINIT:
            sprintf(*buf, "WRITE_STRUCT_INSTINIT\n");
            break;
        case WRITE_L_STRUC_INSTINIT:
            sprintf(*buf, "WRITE_L_STRUC_INSTINIT\n");
            break;
        case SAVE_PAIR_X_INSTINIT:
            sprintf(*buf, "SAVE_PAIR_X_INSTINIT\n");
            break;
        case SAVE_PAIR_X_WRITE_INSTINIT:
            sprintf(*buf, "SAVE_PAIR_X_WRITE_INSTINIT\n");
            break;
        case SAVE_PAIR_Y_INSTINIT:
            sprintf(*buf, "SAVE_PAIR_Y_INSTINIT\n");
            break;
        case SAVE_PAIR_Y_WRITE_INSTINIT:
            sprintf(*buf, "SAVE_PAIR_Y_WRITE_INSTINIT\n");
            break;
        case SAVE_APPL_X_INSTINIT:
            sprintf(*buf, "SAVE_APPL_X_INSTINIT\n");
            break;
        case SAVE_APPL_X_WRITE_INSTINIT:
            sprintf(*buf, "SAVE_APPL_X_WRITE_INSTINIT\n");
            break;
        case SAVE_APPL_Y_INSTINIT:
            sprintf(*buf, "SAVE_APPL_Y_INSTINIT\n");
            break;
        case SAVE_APPL_Y_WRITE_INSTINIT:
            sprintf(*buf, "SAVE_APPL_Y_WRITE_INSTINIT\n");
            break;
        case JUMP_INSTINIT:
            sprintf(*buf, "JUMP_INSTINIT\n");
            break;
        case MOVE_BACK_INSTINIT:
            sprintf(*buf, "MOVE_BACK_INSTINIT\n");
            break;
        case SKIP_INSTINIT:
            sprintf(*buf, "SKIP_INSTINIT\n");
            break;
        case EITHER_INSTINIT:
            sprintf(*buf, "EITHER_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case EITHER_LOW_LEVEL_TRACER:
            sprintf(*buf, "EITHER_LOW_LEVEL_TRACER\n");
            break;
#endif
        case EITHER_POST_COROUTINING:
            sprintf(*buf, "EITHER_POST_COROUTINING\n");
            break;
        case EITHER_FROZEN_YSBA:
            sprintf(*buf, "EITHER_FROZEN_YSBA\n");
            break;
        case EITHER_POST_FROZEN_YSBA:
            sprintf(*buf, "EITHER_POST_FROZEN_YSBA\n");
            break;
#ifdef YAPOR
        case EITHER_YAPOR:
            sprintf(*buf, "EITHER_YAPOR\n");
            break;
#endif
        case EITHER_END:
            sprintf(*buf, "EITHER_END\n");
            break;
        case OR_ELSE_INSTINIT:
            sprintf(*buf, "OR_ELSE_INSTINIT\n");
            break;
#ifdef DEPTH_LIMIT
        case OR_ELSE_DEPTH:
            sprintf(*buf, "OR_ELSE_DEPTH\n");
            break;
#endif
        case OR_ELSE_POST_DEPTH:
            sprintf(*buf, "OR_ELSE_POST_DEPTH\n");
            break;
#ifdef YAPOR
        case OR_ELSE_YAPOR:
            sprintf(*buf, "OR_ELSE_YAPOR\n");
            break;
#endif
        case OR_ELSE_END:
            sprintf(*buf, "OR_ELSE_END\n");
            break;
        case OR_LAST_INSTINIT:
            sprintf(*buf, "OR_LAST_INSTINIT\n");
            break;
#ifdef YAPOR
        case OR_LAST_IFOK_INIT:
            sprintf(*buf, "OR_LAST_IFOK_INIT\n");
            break;
#ifdef DEPTH_LIMIT
        case OR_LAST_IFOK_DEPTH:
            sprintf(*buf, "OR_LAST_IFOK_DEPTH\n");
            break;
#endif
        case OR_LAST_IFOK_END:
            sprintf(*buf, "OR_LAST_IFOK_END\n");
            break;
#endif
        case OR_LAST_NOIF_INIT:
            sprintf(*buf, "OR_LAST_NOIF_INIT\n");
            break;
#ifdef DEPTH_LIMIT
        case OR_LAST_NOIF_DEPTH:
            sprintf(*buf, "OR_LAST_NOIF_DEPTH\n");
            break;
#endif
        case OR_LAST_NOIF_END:
            sprintf(*buf, "OR_LAST_NOIF_END\n");
            break;
#ifdef YAPOR
        case OR_LAST_YAPOR:
            sprintf(*buf, "OR_LAST_YAPOR\n");
            break;
#else
        case OR_LAST_NOYAPOR:
            sprintf(*buf, "OR_LAST_NOYAPOR\n");
            break;
#endif
        case OR_LAST_END:
            sprintf(*buf, "OR_LAST_END\n");
            break;
        case POP_N_INSTINIT:
            sprintf(*buf, "POP_N_INSTINIT\n");
            break;
        case POP_N_END:
            sprintf(*buf, "POP_N_END\n");
            break;
        case POP_INSTINIT:
            sprintf(*buf, "POP_INSTINIT\n");
            break;
        case POP_END:
            sprintf(*buf, "POP_END\n");
            break;
        case CALL_CPRED_INSTINIT:
            sprintf(*buf, "CALL_CPRED_INSTINIT\n");
            break;
        case CALL_CPRED_TEST_STACK:
            sprintf(*buf, "CALL_CPRED_TEST_STACK\n");
            break;
#ifdef FROZEN_STACKS
        case CALL_CPRED_FROZEN_INIT:
            sprintf(*buf, "CALL_CPRED_FROZEN_INIT\n");
            break;
        case CALL_CPRED_TOPB:
            sprintf(*buf, "CALL_CPRED_TOPB\n");
            break;
#else
        case CALL_CPRED_NOFROZEN:
            sprintf(*buf, "CALL_CPRED_NOFROZEN\n");
            break;
#endif
#ifdef LOW_LEVEL_TRACER
        case CALL_CPRED_LOW_LEVEL_TRACER:
            sprintf(*buf, "CALL_CPRED_LOW_LEVEL_TRACER\n");
            break;
#endif
        case CALL_CPRED_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "CALL_CPRED_POST_LOW_LEVEL_TRACER\n");
            break;
#ifdef SHADOW_S
        case CALL_CPRED_SETSREG:
            sprintf(*buf, "CALL_CPRED_SETSREG\n");
            break;
#endif
        case CALL_CPRED_END:
            sprintf(*buf, "CALL_CPRED_END\n");
            break;
        case EXECUTE_CPRED_INSTINIT:
            sprintf(*buf, "EXECUTE_CPRED_INSTINIT\n");
            break;
        case EXECUTE_CPRED_POST_CHECK_TRAIL:
            sprintf(*buf, "EXECUTE_CPRED_POST_CHECK_TRAIL\n");
            break;
#ifdef FROZEN_STACKS
        case EXECUTE_CPRED_FROZEN:
            sprintf(*buf, "EXECUTE_CPRED_FROZEN\n");
            break;
        case EXECUTE_CPRED_TOPB:
            sprintf(*buf, "EXECUTE_CPRED_TOPB\n");
            break;
#else
        case EXECUTE_CPRED_NOFROZEN:
            sprintf(*buf, "EXECUTE_CPRED_NOFROZEN\n");
            break;
#endif
        case EXECUTE_CPRED_POST_FROZEN:
            sprintf(*buf, "EXECUTE_CPRED_POST_FROZEN\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case EXECUTE_CPRED_LOW_LEVEL_TRACER:
            sprintf(*buf, "EXECUTE_CPRED_LOW_LEVEL_TRACER\n");
            break;
#endif
        case EXECUTE_CPRED_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "EXECUTE_CPRED_POST_LOW_LEVEL_TRACER\n");
            break;
        case EXECUTE_CPRED_SAVE_PC:
            sprintf(*buf, "EXECUTE_CPRED_SAVE_PC\n");
            break;
#ifdef DEPTH_LIMIT
        case EXECUTE_CPRED_DEPTH_MINOR:
            sprintf(*buf, "EXECUTE_CPRED_DEPTH_MINOR\n");
            break;
        case EXECUTE_CPRED_DEPTH_MOFPRED:
            sprintf(*buf, "EXECUTE_CPRED_DEPTH_MOFPRED\n");
            break;
        case EXECUTE_CPRED_DEPTH_END:
            sprintf(*buf, "EXECUTE_CPRED_DEPTH_END\n");
            break;
#endif
        case EXECUTE_CPRED_END:
            sprintf(*buf, "EXECUTE_CPRED_END\n");
            break;
        case CALL_USERCPRED_INSTINIT:
            sprintf(*buf, "CALL_USERCPRED_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case CALL_USERCPRED_LOW_LEVEL_TRACER:
            sprintf(*buf, "CALL_USERCPRED_LOW_LEVEL_TRACER\n");
            break;
#endif
        case CALL_USERCPRED_FROZEN:
            sprintf(*buf, "CALL_USERCPRED_FROZEN\n");
            break;
        case CALL_USERCPRED_POST_FROZEN:
            sprintf(*buf, "CALL_USERCPRED_POST_FROZEN\n");
            break;
        case CALL_USERCPRED_END:
            sprintf(*buf, "CALL_USERCPRED_END\n");
            break;
        case LOCK_PRED_INSTINIT:
            sprintf(*buf, "LOCK_PRED_INSTINIT\n");
            break;
        case LOCK_PRED_FIRSTIFOK:
            sprintf(*buf, "LOCK_PRED_FIRSTIFOK\n");
            break;
        case LOCK_PRED_SECONDTIFOK:
            sprintf(*buf, "LOCK_PRED_SECONDTIFOK\n");
            break;
        case LOCK_PRED_END:
            sprintf(*buf, "LOCK_PRED_END\n");
            break;
        case INDEX_PRED_INSTINIT:
            sprintf(*buf, "INDEX_PRED_INSTINIT\n");
            break;
        case INDEX_PRED_END:
            sprintf(*buf, "INDEX_PRED_END\n");
            break;
#if THREADS
        case THREAD_LOCAL_INSTINIT:
            sprintf(*buf, "THREAD_LOCAL_INSTINIT\n");
            break;
#endif
        case EXPAND_INDEX_INSTINIT:
            sprintf(*buf, "EXPAND_INDEX_INSTINIT\n");
            break;
#if defined(YAPOR) || defined(THREADS)
        case EXPAND_INDEX_YAPOR_THREADS_NOPP:
            sprintf(*buf, "EXPAND_INDEX_YAPOR_THREADS_NOPP\n");
            break;
        case EXPAND_INDEX_YAPOR_THREADS_IFOK_INIT:
            sprintf(*buf, "EXPAND_INDEX_YAPOR_THREADS_IFOK_INIT\n");
            break;
        case EXPAND_INDEX_YAPOR_THREADS_IFOK_IFOK:
            sprintf(*buf, "EXPAND_INDEX_YAPOR_THREADS_IFOK_IFOK\n");
            break;
        case EXPAND_INDEX_YAPOR_THREADS_IFOK_END:
            sprintf(*buf, "EXPAND_INDEX_YAPOR_THREADS_IFOK_END\n");
            break;
#endif
#ifdef SHADOW_S
        case EXPAND_INDEX_NOYAPOR_NOTHREADS_SETS:
            sprintf(*buf, "EXPAND_INDEX_NOYAPOR_NOTHREADS_SETS\n");
            break;
#endif
        case EXPAND_INDEX_NOYAPOR_NOTHREADS_POST_SETS:
            sprintf(*buf, "EXPAND_INDEX_NOYAPOR_NOTHREADS_POST_SETS\n");
            break;
#ifdef SHADOW_S
        case EXPAND_INDEX_NOYAPOR_NOTHREADS_SETSREG:
            sprintf(*buf, "EXPAND_INDEX_NOYAPOR_NOTHREADS_SETSREG\n");
            break;
#endif
        case EXPAND_INDEX_NOYAPOR_NOTHREADS_POST_SETSREG:
            sprintf(*buf, "EXPAND_INDEX_NOYAPOR_NOTHREADS_POST_SETSREG\n");
            break;
#if defined(YAPOR) || defined(THREADS)
        case EXPAND_INDEX_UNLOCK:
            sprintf(*buf, "EXPAND_INDEX_UNLOCK\n");
            break;
#endif
        case EXPAND_INDEX_END:
            sprintf(*buf, "EXPAND_INDEX_END\n");
            break;
        case EXPAND_CLAUSES_INSTINIT:
            sprintf(*buf, "EXPAND_CLAUSES_INSTINIT\n");
            break;
#if defined(YAPOR) || defined(THREADS)
        case EXPAND_CLAUSES_YAPOR_THREADS_NOPP:
            sprintf(*buf, "EXPAND_CLAUSES_YAPOR_THREADS_NOPP\n");
            break;
        case EXPAND_CLAUSES_YAPOR_THREADS_IFOK_INIT:
            sprintf(*buf, "EXPAND_CLAUSES_YAPOR_THREADS_IFOK_INIT\n");
            break;
        case EXPAND_CLAUSES_YAPOR_THREADS_IFOK_IFOK:
            sprintf(*buf, "EXPAND_CLAUSES_YAPOR_THREADS_IFOK_IFOK\n");
            break;
        case EXPAND_CLAUSES_YAPOR_THREADS_IFOK_END:
            sprintf(*buf, "EXPAND_CLAUSES_YAPOR_THREADS_IFOK_END\n");
            break;
#endif
        case EXPAND_CLAUSES_NOYAPOR_NOTHREADS:
            sprintf(*buf, "EXPAND_CLAUSES_NOYAPOR_NOTHREADS\n");
            break;
#if defined(YAPOR) || defined(THREADS)
        case EXPAND_CLAUSES_UNLOCK:
            sprintf(*buf, "EXPAND_CLAUSES_UNLOCK\n");
            break;
#endif
        case EXPAND_CLAUSES_END:
            sprintf(*buf, "EXPAND_CLAUSES_END\n");
            break;
        case UNDEF_P_INSTINIT:
            sprintf(*buf, "UNDEF_P_INSTINIT\n");
            break;
        case UNDEF_P_END:
            sprintf(*buf, "UNDEF_P_END\n");
            break;
        case SPY_PRED_INSTINIT:
            sprintf(*buf, "SPY_PRED_INSTINIT\n");
            break;
        case SPY_PRED_FIRSTIFOK:
            sprintf(*buf, "SPY_PRED_FIRSTIFOK\n");
            break;
        case SPY_PRED_SECONDIFOK_INIT:
            sprintf(*buf, "SPY_PRED_SECONDIFOK_INIT\n");
            break;
        case SPY_PRED_SECONDIFOK_FIRSTIFOK:
            sprintf(*buf, "SPY_PRED_SECONDIFOK_FIRSTIFOK\n");
            break;
        case SPY_PRED_SECONDIFOK_POST_FIRSTIF:
            sprintf(*buf, "SPY_PRED_SECONDIFOK_POST_FIRSTIF\n");
            break;
        case SPY_PRED_SECONDIFOK_SECONDIFOK:
            sprintf(*buf, "SPY_PRED_SECONDIFOK_SECONDIFOK\n");
            break;
        case SPY_PRED_SECONDIFOK_THIRDIFOK:
            sprintf(*buf, "SPY_PRED_SECONDIFOK_THIRDIFOK\n");
            break;
        case SPY_PRED_THIRDIFOK_INIT:
            sprintf(*buf, "SPY_PRED_THIRDIFOK_INIT\n");
            break;
        case SPY_PRED_THIRDIFOK_FIRSTIFOK:
            sprintf(*buf, "SPY_PRED_THIRDIFOK_FIRSTIFOK\n");
            break;
        case SPY_PRED_FOURTHIFOK:
            sprintf(*buf, "SPY_PRED_FOURTHIFOK\n");
            break;
        case SPY_PRED_POST_FOURTHIF:
            sprintf(*buf, "SPY_PRED_POST_FOURTHIF\n");
            break;
        case SPY_PRED_D0ISZERO:
            sprintf(*buf, "SPY_PRED_D0ISZERO\n");
            break;
        case SPY_PRED_D0ISNOZERO_INIT:
            sprintf(*buf, "SPY_PRED_D0ISNOZERO_INIT\n");
            break;
        case SPY_PRED_D0ISNOZERO_INSIDEFOR_INIT:
            sprintf(*buf, "SPY_PRED_D0ISNOZERO_INSIDEFOR_INIT\n");
            break;
        case SPY_PRED_D0ISNOZERO_INSIDEFOR_DOSPY_NONVAR:
            sprintf(*buf, "SPY_PRED_D0ISNOZERO_INSIDEFOR_DOSPY_NONVAR\n");
            break;
        case SPY_PRED_D0ISNOZERO_INSIDEFOR_SAFEVAR:
            sprintf(*buf, "SPY_PRED_D0ISNOZERO_INSIDEFOR_SAFEVAR\n");
            break;
        case SPY_PRED_D0ISNOZERO_INSIDEFOR_UNSAFEVAR:
            sprintf(*buf, "SPY_PRED_D0ISNOZERO_INSIDEFOR_UNSAFEVAR\n");
            break;
        case SPY_PRED_POST_IFS:
            sprintf(*buf, "SPY_PRED_POST_IFS\n");
            break;
#ifdef THREADS
        case SPY_PRED_THREADS_LOCK:
            sprintf(*buf, "SPY_PRED_THREADS_LOCK\n");
            break;
#endif
        case SPY_PRED_POST_LOCK:
            sprintf(*buf, "SPY_PRED_POST_LOCK\n");
            break;
#ifdef THREADS
        case SPY_PRED_THREADS_UNLOCK:
            sprintf(*buf, "SPY_PRED_THREADS_UNLOCK\n");
            break;
#endif
        case SPY_PRED_POST_UNLOCK:
            sprintf(*buf, "SPY_PRED_POST_UNLOCK\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case SPY_PRED_LOW_LEVEL_TRACER:
            sprintf(*buf, "SPY_PRED_LOW_LEVEL_TRACER\n");
            break;
#endif
        case SPY_PRED_END:
            sprintf(*buf, "SPY_PRED_END\n");
            break;
        case TRY_CLAUSE_INSTINIT:
            sprintf(*buf, "TRY_CLAUSE_INSTINIT\n");
            break;
#ifdef YAPOR
        case TRY_CLAUSE_YAPOR:
            sprintf(*buf, "TRY_CLAUSE_YAPOR\n");
            break;
#endif
        case TRY_CLAUSE_END:
            sprintf(*buf, "TRY_CLAUSE_END\n");
            break;
        case TRY_CLAUSE2_INSTINIT:
            sprintf(*buf, "TRY_CLAUSE2_INSTINIT\n");
            break;
#ifdef YAPOR
        case TRY_CLAUSE2_YAPOR:
            sprintf(*buf, "TRY_CLAUSE2_YAPOR\n");
            break;
#endif
        case TRY_CLAUSE2_END:
            sprintf(*buf, "TRY_CLAUSE2_END\n");
            break;
        case TRY_CLAUSE3_INSTINIT:
            sprintf(*buf, "TRY_CLAUSE3_INSTINIT\n");
            break;
#ifdef YAPOR
        case TRY_CLAUSE3_YAPOR:
            sprintf(*buf, "TRY_CLAUSE3_YAPOR\n");
            break;
#endif
        case TRY_CLAUSE3_END:
            sprintf(*buf, "TRY_CLAUSE3_END\n");
            break;
        case TRY_CLAUSE4_INSTINIT:
            sprintf(*buf, "TRY_CLAUSE4_INSTINIT\n");
            break;
#ifdef YAPOR
        case TRY_CLAUSE4_YAPOR:
            sprintf(*buf, "TRY_CLAUSE4_YAPOR\n");
            break;
#endif
        case TRY_CLAUSE4_END:
            sprintf(*buf, "TRY_CLAUSE4_END\n");
            break;
        case RETRY_INSTINIT:
            sprintf(*buf, "RETRY_INSTINIT\n");
            break;
#ifdef FROZEN_STACKS
        case RETRY_FROZEN:
            sprintf(*buf, "RETRY_FROZEN\n");
            break;
#else
        case RETRY_NOFROZEN:
            sprintf(*buf, "RETRY_NOFROZEN\n");
            break;
#endif
        case RETRY_END:
            sprintf(*buf, "RETRY_END\n");
            break;
        case RETRY2_INSTINIT:
            sprintf(*buf, "RETRY2_INSTINIT\n");
            break;
#ifdef FROZEN_STACKS
        case RETRY2_FROZEN:
            sprintf(*buf, "RETRY2_FROZEN\n");
            break;
#else
        case RETRY2_NOFROZEN:
            sprintf(*buf, "RETRY2_NOFROZEN\n");
            break;
#endif
        case RETRY2_END:
            sprintf(*buf, "RETRY2_END\n");
            break;
        case RETRY3_INSTINIT:
            sprintf(*buf, "RETRY3_INSTINIT\n");
            break;
#ifdef FROZEN_STACKS
        case RETRY3_FROZEN:
            sprintf(*buf, "RETRY3_FROZEN\n");
            break;
#else
        case RETRY3_NOFROZEN:
            sprintf(*buf, "RETRY3_NOFROZEN\n");
            break;
#endif
        case RETRY3_END:
            sprintf(*buf, "RETRY3_END\n");
            break;
        case RETRY4_INSTINIT:
            sprintf(*buf, "RETRY4_INSTINIT\n");
            break;
#ifdef FROZEN_STACKS
        case RETRY4_FROZEN:
            sprintf(*buf, "RETRY4_FROZEN\n");
            break;
#else
        case RETRY4_NOFROZEN:
            sprintf(*buf, "RETRY4_NOFROZEN\n");
            break;
#endif
        case RETRY4_END:
            sprintf(*buf, "RETRY4_END\n");
            break;
        case TRUST_INSTINIT:
            sprintf(*buf, "TRUST_INSTINIT\n");
            break;
#ifdef YAPOR
        case TRUST_IFOK_INIT:
            sprintf(*buf, "TRUST_IFOK_INIT\n");
            break;
#ifdef FROZEN_STACKS
        case TRUST_IFOK_FROZEN:
            sprintf(*buf, "TRUST_IFOK_FROZEN\n");
            break;
#endif
        case TRUST_IFOK_END:
            sprintf(*buf, "TRUST_IFOK_END\n");
            break;
#endif
        case TRUST_NOIF_INIT:
            sprintf(*buf, "TRUST_NOIF_INIT\n");
            break;
#ifdef FROZEN_STACKS
        case TRUST_NOIF_FROZEN:
            sprintf(*buf, "TRUST_NOIF_FROZEN\n");
            break;
#endif
        case TRUST_END:
            sprintf(*buf, "TRUST_END\n");
            break;
        case TRY_IN_INSTINIT:
            sprintf(*buf, "TRY_IN_INSTINIT\n");
            break;
        case TRY_IN_END:
            sprintf(*buf, "TRY_IN_END\n");
            break;
        case USER_SWITCH_INSTINIT:
            sprintf(*buf, "USER_SWITCH_INSTINIT\n");
            break;
        case USER_SWITCH_END:
            sprintf(*buf, "USER_SWITCH_END\n");
            break;
        case SWITCH_ON_TYPE_INSTINIT:
            sprintf(*buf, "SWITCH_ON_TYPE_INSTINIT\n");
            break;
        case SWITCH_ON_TYPE_END:
            sprintf(*buf, "SWITCH_ON_TYPE_END\n");
            break;
        case SWITCH_LIST_NL_INSTINIT:
            sprintf(*buf, "SWITCH_LIST_NL_INSTINIT\n");
            break;
        case SWITCH_LIST_NL_END:
            sprintf(*buf, "SWITCH_LIST_NL_END\n");
            break;
        case SWITCH_ON_ARG_TYPE_INSTINIT:
            sprintf(*buf, "SWITCH_ON_ARG_TYPE_INSTINIT\n");
            break;
        case SWITCH_ON_ARG_TYPE_END:
            sprintf(*buf, "SWITCH_ON_ARG_TYPE_END\n");
            break;
        case SWITCH_ON_SUB_ARG_TYPE_INSTINIT:
            sprintf(*buf, "SWITCH_ON_SUB_ARG_TYPE_INSTINIT\n");
            break;
        case SWITCH_ON_SUB_ARG_TYPE_END:
            sprintf(*buf, "SWITCH_ON_SUB_ARG_TYPE_END\n");
            break;
        case JUMP_IF_VAR_INSTINIT:
            sprintf(*buf, "JUMP_IF_VAR_INSTINIT\n");
            break;
        case JUMP_IF_VAR_END:
            sprintf(*buf, "JUMP_IF_VAR_END\n");
            break;
        case JUMP_IF_NONVAR_INSTINIT:
            sprintf(*buf, "JUMP_IF_NONVAR_INSTINIT\n");
            break;
        case JUMP_IF_NONVAR_END:
            sprintf(*buf, "JUMP_IF_NONVAR_END\n");
            break;
        case IF_NOT_THEN_INSTINIT:
            sprintf(*buf, "IF_NOT_THEN_INSTINIT\n");
            break;
        case IF_NOT_THEN_END:
            sprintf(*buf, "IF_NOT_THEN_END\n");
            break;
        case SWITCH_ON_FUNC_INSTINIT:
            sprintf(*buf, "SWITCH_ON_FUNC_INSTINIT\n");
            break;
        case SWITCH_ON_FUNC_END:
            sprintf(*buf, "SWITCH_ON_FUNC_END\n");
            break;
        case SWITCH_ON_CONS_INSTINIT:
            sprintf(*buf, "SWITCH_ON_CONS_INSTINIT\n");
            break;
        case SWITCH_ON_CONS_END:
            sprintf(*buf, "SWITCH_ON_CONS_END\n");
            break;
        case GO_ON_FUNC_INSTINIT:
            sprintf(*buf, "GO_ON_FUNC_INSTINIT\n");
            break;
        case GO_ON_FUNC_END:
            sprintf(*buf, "GO_ON_FUNC_END\n");
            break;
        case GO_ON_CONS_INSTINIT:
            sprintf(*buf, "GO_ON_CONS_INSTINIT\n");
            break;
        case GO_ON_CONS_END:
            sprintf(*buf, "GO_ON_CONS_END\n");
            break;
        case IF_FUNC_INSTINIT:
            sprintf(*buf, "IF_FUNC_INSTINIT\n");
            break;
        case IF_FUNC_END:
            sprintf(*buf, "IF_FUNC_END\n");
            break;
        case IF_CONS_INSTINIT:
            sprintf(*buf, "IF_CONS_INSTINIT\n");
            break;
        case IF_CONS_END:
            sprintf(*buf, "IF_CONS_END\n");
            break;
        case INDEX_DBREF_INSTINIT:
            sprintf(*buf, "INDEX_DBREF_INSTINIT\n");
            break;
        case INDEX_DBREF_END:
            sprintf(*buf, "INDEX_DBREF_END\n");
            break;
        case INDEX_BLOB_INSTINIT:
            sprintf(*buf, "INDEX_BLOB_INSTINIT\n");
            break;
        case INDEX_BLOB_END:
            sprintf(*buf, "INDEX_BLOB_END\n");
            break;
        case INDEX_LONG_INSTINIT:
            sprintf(*buf, "INDEX_LONG_INSTINIT\n");
            break;
        case INDEX_LONG_END:
            sprintf(*buf, "INDEX_LONG_END\n");
            break;
        case JIT_HANDLER_INSTINIT:
            sprintf(*buf, "JIT_HANDLER_INSTINIT\n");
            break;
        case P_ATOM_X_INSTINIT:
            sprintf(*buf, "P_ATOM_X_INSTINIT\n");
            break;
        case P_ATOM_X_ATOM:
            sprintf(*buf, "P_ATOM_X_ATOM\n");
            break;
        case P_ATOM_X_NOATOM:
            sprintf(*buf, "P_ATOM_X_NOATOM\n");
            break;
        case P_ATOM_Y_INSTINIT:
            sprintf(*buf, "P_ATOM_Y_INSTINIT\n");
            break;
        case P_ATOM_Y_IFOK:
            sprintf(*buf, "P_ATOM_Y_IFOK\n");
            break;
        case P_ATOM_Y_NOIF:
            sprintf(*buf, "P_ATOM_Y_NOIF\n");
            break;
        case P_ATOM_Y_END:
            sprintf(*buf, "P_ATOM_Y_END\n");
            break;
        case P_ATOMIC_X_INSTINIT:
            sprintf(*buf, "P_ATOMIC_X_INSTINIT\n");
            break;
        case P_ATOMIC_X_NONVAR:
            sprintf(*buf, "P_ATOMIC_X_NONVAR\n");
            break;
        case P_ATOMIC_X_VAR:
            sprintf(*buf, "P_ATOMIC_X_VAR\n");
            break;
        case P_ATOMIC_X_END:
            sprintf(*buf, "P_ATOMIC_X_END\n");
            break;
        case P_ATOMIC_Y_INSTINIT:
            sprintf(*buf, "P_ATOMIC_Y_INSTINIT\n");
            break;
        case P_ATOMIC_Y_NONVAR:
            sprintf(*buf, "P_ATOMIC_Y_NONVAR\n");
            break;
        case P_ATOMIC_Y_VAR:
            sprintf(*buf, "P_ATOMIC_Y_VAR\n");
            break;
        case P_ATOMIC_Y_END:
            sprintf(*buf, "P_ATOMIC_Y_END\n");
            break;
        case P_INTEGER_X_INSTINIT:
            sprintf(*buf, "P_INTEGER_X_INSTINIT\n");
            break;
        case P_INTEGER_X_INTEGER_X_NVAR_OK:
            sprintf(*buf, "P_INTEGER_X_INTEGER_X_NVAR_OK\n");
            break;
		case P_INTEGER_X_INTEGER_X_NVAR_NOOK:
            sprintf(*buf, "P_INTEGER_X_INTEGER_X_NVAR_NOOK\n");
            break;
        case P_INTEGER_X_INTEGER_X_UNK:
            sprintf(*buf, "P_INTEGER_X_INTEGER_X_UNK\n");
            break;
        case P_INTEGER_Y_INSTINIT:
            sprintf(*buf, "P_INTEGER_Y_INSTINIT\n");
            break;
        case P_INTEGER_Y_INTEGER_Y_NVAR_OK:
            sprintf(*buf, "P_INTEGER_Y_INTEGER_Y_NVAR_OK\n");
            break;
        case P_INTEGER_Y_INTEGER_Y_NVAR_NOOK:
            sprintf(*buf, "P_INTEGER_Y_INTEGER_Y_NVAR_NOOK\n");
            break;
		case P_INTEGER_Y_INTEGER_Y_UNK:
            sprintf(*buf, "P_INTEGER_Y_INTEGER_Y_UNK\n");
            break;
        case P_NONVAR_X_INSTINIT:
            sprintf(*buf, "P_NONVAR_X_INSTINIT\n");
            break;
        case P_NONVAR_X_NONVAR:
            sprintf(*buf, "P_NONVAR_X_NONVAR\n");
            break;
        case P_NONVAR_X_NONONVAR:
            sprintf(*buf, "P_NONVAR_X_NONONVAR\n");
            break;
        case P_NONVAR_Y_INSTINIT:
            sprintf(*buf, "P_NONVAR_Y_INSTINIT\n");
            break;
        case P_NONVAR_Y_NONVAR:
            sprintf(*buf, "P_NONVAR_Y_NONVAR\n");
            break;
        case P_NONVAR_Y_NONONVAR:
            sprintf(*buf, "P_NONVAR_Y_NONONVAR\n");
            break;
        case P_NUMBER_X_INSTINIT:
            sprintf(*buf, "P_NUMBER_X_INSTINIT\n");
            break;
        case P_NUMBER_X_INT:
            sprintf(*buf, "P_NUMBER_X_INT\n");
            break;
        case P_NUMBER_X_FUNCTORINT:
            sprintf(*buf, "P_NUMBER_X_FUNCTORINT\n");
            break;
        case P_NUMBER_X_FUNCTORDEFAULT:
            sprintf(*buf, "P_NUMBER_X_FUNCTORDEFAULT\n");
            break;
        case P_NUMBER_X_POST_IF:
            sprintf(*buf, "P_NUMBER_X_POST_IF\n");
            break;
        case P_NUMBER_X_NUMBER_X_UNK:
            sprintf(*buf, "P_NUMBER_X_NUMBER_X_UNK\n");
            break;
        case P_NUMBER_Y_INSTINIT:
            sprintf(*buf, "P_NUMBER_Y_INSTINIT\n");
            break;
        case P_NUMBER_Y_INT:
            sprintf(*buf, "P_NUMBER_Y_INT\n");
            break;
        case P_NUMBER_Y_FUNCTORINT:
            sprintf(*buf, "P_NUMBER_Y_FUNCTORINT\n");
            break;
        case P_NUMBER_Y_FUNCTORDEFAULT:
            sprintf(*buf, "P_NUMBER_Y_FUNCTORDEFAULT\n");
            break;
        case P_NUMBER_Y_POST_IF:
            sprintf(*buf, "P_NUMBER_Y_POST_IF\n");
            break;
        case P_NUMBER_Y_NUMBER_Y_UNK:
            sprintf(*buf, "P_NUMBER_Y_NUMBER_Y_UNK\n");
            break;
        case P_VAR_X_INSTINIT:
            sprintf(*buf, "P_VAR_X_INSTINIT\n");
            break;
        case P_VAR_X_NONVAR:
            sprintf(*buf, "P_VAR_X_NONVAR\n");
            break;
        case P_VAR_X_VAR:
            sprintf(*buf, "P_VAR_X_VAR\n");
            break;
        case P_VAR_Y_INSTINIT:
            sprintf(*buf, "P_VAR_Y_INSTINIT\n");
            break;
        case P_VAR_Y_NONVAR:
            sprintf(*buf, "P_VAR_Y_NONVAR\n");
            break;
        case P_VAR_Y_VAR:
            sprintf(*buf, "P_VAR_Y_VAR\n");
            break;
        case P_DB_REF_X_INSTINIT:
            sprintf(*buf, "P_DB_REF_X_INSTINIT\n");
            break;
        case P_DB_REF_X_DBREF:
            sprintf(*buf, "P_DB_REF_X_DBREF\n");
            break;
        case P_DB_REF_X_NODBREF:
            sprintf(*buf, "P_DB_REF_X_NODBREF\n");
            break;
        case P_DB_REF_X_DBREF_X_UNK:
            sprintf(*buf, "P_DB_REF_X_DBREF_X_UNK\n");
            break;
        case P_DB_REF_Y_INSTINIT:
            sprintf(*buf, "P_DB_REF_Y_INSTINIT\n");
            break;
        case P_DB_REF_Y_DBREF:
            sprintf(*buf, "P_DB_REF_Y_DBREF\n");
            break;
        case P_DB_REF_Y_NODBREF:
            sprintf(*buf, "P_DB_REF_Y_NODBREF\n");
            break;
        case P_DB_REF_Y_DBREF_Y_UNK:
            sprintf(*buf, "P_DB_REF_Y_DBREF_Y_UNK\n");
            break;
        case P_PRIMITIVE_X_INSTINIT:
            sprintf(*buf, "P_PRIMITIVE_X_INSTINIT\n");
            break;
        case P_PRIMITIVE_X_PRIMITIVE:
            sprintf(*buf, "P_PRIMITIVE_X_PRIMITIVE\n");
            break;
        case P_PRIMITIVE_X_NOPRIMITIVE:
            sprintf(*buf, "P_PRIMITIVE_X_NOPRIMITIVE\n");
            break;
        case P_PRIMITIVE_X_PRIMI_X_UNK:
            sprintf(*buf, "P_PRIMITIVE_X_PRIMI_X_UNK\n");
            break;
        case P_PRIMITIVE_Y_INSTINIT:
            sprintf(*buf, "P_PRIMITIVE_Y_INSTINIT\n");
            break;
        case P_PRIMITIVE_Y_PRIMITIVE:
            sprintf(*buf, "P_PRIMITIVE_Y_PRIMITIVE\n");
            break;
        case P_PRIMITIVE_Y_NOPRIMITIVE:
            sprintf(*buf, "P_PRIMITIVE_Y_NOPRIMITIVE\n");
            break;
        case P_PRIMITIVE_Y_PRIMI_Y_UNK:
            sprintf(*buf, "P_PRIMITIVE_Y_PRIMI_Y_UNK\n");
            break;
        case P_COMPOUND_X_INSTINIT:
            sprintf(*buf, "P_COMPOUND_X_INSTINIT\n");
            break;
        case P_COMPOUND_X_PAIR:
            sprintf(*buf, "P_COMPOUND_X_PAIR\n");
            break;
        case P_COMPOUND_X_APPL_IFOK:
            sprintf(*buf, "P_COMPOUND_X_APPL_IFOK\n");
            break;
        case P_COMPOUND_X_APPL:
            sprintf(*buf, "P_COMPOUND_X_APPL\n");
            break;
        case P_COMPOUND_X_NOAPPL:
            sprintf(*buf, "P_COMPOUND_X_NOAPPL\n");
            break;
        case P_COMPOUND_X_COMPOUND_X_UNK:
            sprintf(*buf, "P_COMPOUND_X_COMPOUND_X_UNK\n");
            break;
        case P_COMPOUND_Y_INSTINIT:
            sprintf(*buf, "P_COMPOUND_Y_INSTINIT\n");
            break;
        case P_COMPOUND_Y_PAIR:
            sprintf(*buf, "P_COMPOUND_Y_PAIR\n");
            break;
        case P_COMPOUND_Y_APPL_IFOK:
            sprintf(*buf, "P_COMPOUND_Y_APPL_IFOK\n");
            break;
        case P_COMPOUND_Y_APPL:
            sprintf(*buf, "P_COMPOUND_Y_APPL\n");
            break;
        case P_COMPOUND_Y_NOAPPL:
            sprintf(*buf, "P_COMPOUND_Y_NOAPPL\n");
            break;
        case P_COMPOUND_Y_COMPOUND_Y_UNK:
            sprintf(*buf, "P_COMPOUND_Y_COMPOUND_Y_UNK\n");
            break;
        case P_FLOAT_X_INSTINIT:
            sprintf(*buf, "P_FLOAT_X_INSTINIT\n");
            break;
        case P_FLOAT_X_FLOAT:
            sprintf(*buf, "P_FLOAT_X_FLOAT\n");
            break;
        case P_FLOAT_X_POST_IF:
            sprintf(*buf, "P_FLOAT_X_POST_IF\n");
            break;
        case P_FLOAT_X_FLOAT_X_UNK:
            sprintf(*buf, "P_FLOAT_X_FLOAT_X_UNK\n");
            break;
        case P_FLOAT_Y_INSTINIT:
            sprintf(*buf, "P_FLOAT_Y_INSTINIT\n");
            break;
        case P_FLOAT_Y_FLOAT:
            sprintf(*buf, "P_FLOAT_Y_FLOAT\n");
            break;
        case P_FLOAT_Y_POST_IF:
            sprintf(*buf, "P_FLOAT_Y_POST_IF\n");
            break;
        case P_FLOAT_Y_FLOAT_Y_UNK:
            sprintf(*buf, "P_FLOAT_Y_FLOAT_Y_UNK\n");
            break;
        case P_PLUS_VV_INSTINIT:
            sprintf(*buf, "P_PLUS_VV_INSTINIT\n");
            break;
        case P_PLUS_VV_PLUS_VV_NVAR:
            sprintf(*buf, "P_PLUS_VV_PLUS_VV_NVAR\n");
            break;
        case P_PLUS_VV_PLUS_VV_NVAR_NVAR_INT:
            sprintf(*buf, "P_PLUS_VV_PLUS_VV_NVAR_NVAR_INT\n");
            break;
		case P_PLUS_VV_PLUS_VV_NVAR_NVAR_NOINT:
            sprintf(*buf, "P_PLUS_VV_PLUS_VV_NVAR_NVAR_NOINT\n");
            break;
        case P_PLUS_VV_PLUS_VV_UNK:
            sprintf(*buf, "P_PLUS_VV_PLUS_VV_UNK\n");
            break;
        case P_PLUS_VV_PLUS_VV_NVAR_UNK:
            sprintf(*buf, "P_PLUS_VV_PLUS_VV_NVAR_UNK\n");
            break;
        case P_PLUS_VC_INSTINIT:
            sprintf(*buf, "P_PLUS_VC_INSTINIT\n");
            break;
        case P_PLUS_VC_PLUS_VC_NVAR_INT:
            sprintf(*buf, "P_PLUS_VC_PLUS_VC_NVAR_INT\n");
            break;
		case P_PLUS_VC_PLUS_VC_NVAR_NOINT:
            sprintf(*buf, "P_PLUS_VC_PLUS_VC_NVAR_NOINT\n");
            break;
        case P_PLUS_VC_PLUS_VC_UNK:
            sprintf(*buf, "P_PLUS_VC_PLUS_VC_UNK\n");
            break;
        case P_PLUS_Y_VV_INSTINIT:
            sprintf(*buf, "P_PLUS_Y_VV_INSTINIT\n");
            break;
        case P_PLUS_Y_VV_PLUS_Y_VV_NVAR:
            sprintf(*buf, "P_PLUS_Y_VV_PLUS_Y_VV_NVAR\n");
            break;
        case P_PLUS_Y_VV_PLUS_Y_VV_NVAR_NVAR_INT:
            sprintf(*buf, "P_PLUS_Y_VV_PLUS_Y_VV_NVAR_NVAR_INT\n");
            break;
        case P_PLUS_Y_VV_PLUS_Y_VV_NVAR_NVAR_NOINT:
            sprintf(*buf, "P_PLUS_Y_VV_PLUS_Y_VV_NVAR_NVAR_NOINT\n");
            break;
        case P_PLUS_Y_VV_PLUS_Y_VV_UNK:
            sprintf(*buf, "P_PLUS_Y_VV_PLUS_Y_VV_UNK\n");
            break;
        case P_PLUS_Y_VV_PLUS_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_PLUS_Y_VV_PLUS_Y_VV_NVAR_UNK\n");
            break;
        case P_PLUS_Y_VC_INSTINIT:
            sprintf(*buf, "P_PLUS_Y_VC_INSTINIT\n");
            break;
        case P_PLUS_Y_VC_PLUS_Y_VC_NVAR_INT:
            sprintf(*buf, "P_PLUS_Y_VC_PLUS_Y_VC_NVAR_INT\n");
            break;
		case P_PLUS_Y_VC_PLUS_Y_VC_NVAR_NOINT:
            sprintf(*buf, "P_PLUS_Y_VC_PLUS_Y_VC_NVAR_NOINT\n");
            break;
        case P_PLUS_Y_VC_PLUS_Y_VC_UNK:
            sprintf(*buf, "P_PLUS_Y_VC_PLUS_Y_VC_UNK\n");
            break;
        case P_MINUS_VV_INSTINIT:
            sprintf(*buf, "P_MINUS_VV_INSTINIT\n");
            break;
        case P_MINUS_VV_MINUS_VV_NVAR:
            sprintf(*buf, "P_MINUS_VV_MINUS_VV_NVAR\n");
            break;
        case P_MINUS_VV_MINUS_VV_NVAR_NVAR_INT:
            sprintf(*buf, "P_MINUS_VV_MINUS_VV_NVAR_NVAR_INT\n");
            break;
		case P_MINUS_VV_MINUS_VV_NVAR_NVAR_NOINT:
            sprintf(*buf, "P_MINUS_VV_MINUS_VV_NVAR_NVAR_NOINT\n");
            break;
        case P_MINUS_VV_MINUS_VV_UNK:
            sprintf(*buf, "P_MINUS_VV_MINUS_VV_UNK\n");
            break;
        case P_MINUS_VV_MINUS_VV_NVAR_UNK:
            sprintf(*buf, "P_MINUS_VV_MINUS_VV_NVAR_UNK\n");
            break;
        case P_MINUS_CV_INSTINIT:
            sprintf(*buf, "P_MINUS_CV_INSTINIT\n");
            break;
        case P_MINUS_CV_MINUS_CV_NVAR_INT:
            sprintf(*buf, "P_MINUS_CV_MINUS_CV_NVAR_INT\n");
            break;
        case P_MINUS_CV_MINUS_CV_NVAR_NOINT:
            sprintf(*buf, "P_MINUS_CV_MINUS_CV_NVAR_NOINT\n");
            break;
        case P_MINUS_CV_MINUS_CV_UNK:
            sprintf(*buf, "P_MINUS_CV_MINUS_CV_UNK\n");
            break;
        case P_MINUS_Y_VV_INSTINIT:
            sprintf(*buf, "P_MINUS_Y_VV_INSTINIT\n");
            break;
        case P_MINUS_Y_VV_MINUS_Y_VV_NVAR:
            sprintf(*buf, "P_MINUS_Y_VV_MINUS_Y_VV_NVAR\n");
            break;
        case P_MINUS_Y_VV_INTTERM:
            sprintf(*buf, "P_MINUS_Y_VV_INTTERM\n");
            break;
        case P_MINUS_Y_VV_NOINTTERM:
            sprintf(*buf, "P_MINUS_Y_VV_NOINTTERM\n");
            break;
        case P_MINUS_Y_VV_D0EQUALS0L:
            sprintf(*buf, "P_MINUS_Y_VV_D0EQUALS0L\n");
            break;
        case P_MINUS_Y_VV_NVAR_END:
            sprintf(*buf, "P_MINUS_Y_VV_NVAR_END\n");
            break;
        case P_MINUS_Y_VV_MINUS_Y_VV_UNK:
            sprintf(*buf, "P_MINUS_Y_VV_MINUS_Y_VV_UNK\n");
            break;
        case P_MINUS_Y_VV_MINUS_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_MINUS_Y_VV_MINUS_Y_VV_NVAR_UNK\n");
            break;
        case P_MINUS_Y_CV_INSTINIT:
            sprintf(*buf, "P_MINUS_Y_CV_INSTINIT\n");
            break;
        case P_MINUS_Y_CV_MINUS_Y_CV_NVAR:
            sprintf(*buf, "P_MINUS_Y_CV_MINUS_Y_CV_NVAR\n");
            break;
        case P_MINUS_Y_CV_INTTERM:
            sprintf(*buf, "P_MINUS_Y_CV_INTTERM\n");
            break;
        case P_MINUS_Y_CV_NOINTTERM:
            sprintf(*buf, "P_MINUS_Y_CV_NOINTTERM\n");
            break;
        case P_MINUS_Y_CV_D0EQUALS0L:
            sprintf(*buf, "P_MINUS_Y_CV_D0EQUALS0L\n");
            break;
        case P_MINUS_Y_CV_NVAR_END:
            sprintf(*buf, "P_MINUS_Y_CV_NVAR_END\n");
            break;
        case P_MINUS_Y_CV_MINUS_Y_CV_UNK:
            sprintf(*buf, "P_MINUS_Y_CV_MINUS_Y_CV_UNK\n");
            break;
        case P_TIMES_VV_INSTINIT:
            sprintf(*buf, "P_TIMES_VV_INSTINIT\n");
            break;
        case P_TIMES_VV_TIMES_VV_NVAR:
            sprintf(*buf, "P_TIMES_VV_TIMES_VV_NVAR\n");
            break;
        case P_TIMES_VV_TIMES_VV_NVAR_NVAR_INT:
            sprintf(*buf, "P_TIMES_VV_TIMES_VV_NVAR_NVAR_INT\n");
            break;
		case P_TIMES_VV_TIMES_VV_NVAR_NVAR_NOINT:
            sprintf(*buf, "P_TIMES_VV_TIMES_VV_NVAR_NVAR_NOINT\n");
            break;
        case P_TIMES_VV_TIMES_VV_UNK:
            sprintf(*buf, "P_TIMES_VV_TIMES_VV_UNK\n");
            break;
        case P_TIMES_VV_TIMES_VV_NVAR_UNK:
            sprintf(*buf, "P_TIMES_VV_TIMES_VV_NVAR_UNK\n");
            break;
        case P_TIMES_VC_INSTINIT:
            sprintf(*buf, "P_TIMES_VC_INSTINIT\n");
            break;
        case P_TIMES_VC_TIMES_VC_NVAR_INT:
            sprintf(*buf, "P_TIMES_VC_TIMES_VC_NVAR_INT\n");
            break;
		case P_TIMES_VC_TIMES_VC_NVAR_NOINT:
            sprintf(*buf, "P_TIMES_VC_TIMES_VC_NVAR_NOINT\n");
            break;
        case P_TIMES_VC_TIMES_VC_UNK:
            sprintf(*buf, "P_TIMES_VC_TIMES_VC_UNK\n");
            break;
        case P_TIMES_Y_VV_INSTINIT:
            sprintf(*buf, "P_TIMES_Y_VV_INSTINIT\n");
            break;
        case P_TIMES_Y_VV_TIMES_Y_VV_NVAR:
            sprintf(*buf, "P_TIMES_Y_VV_TIMES_Y_VV_NVAR\n");
            break;
        case P_TIMES_Y_VV_INTTERM:
            sprintf(*buf, "P_TIMES_Y_VV_INTTERM\n");
            break;
        case P_TIMES_Y_VV_NOINTTERM:
            sprintf(*buf, "P_TIMES_Y_VV_NOINTTERM\n");
            break;
        case P_TIMES_Y_VV_D0EQUALS0L:
            sprintf(*buf, "P_TIMES_Y_VV_D0EQUALS0L\n");
            break;
        case P_TIMES_Y_VV_NVAR_END:
            sprintf(*buf, "P_TIMES_Y_VV_NVAR_END\n");
            break;
        case P_TIMES_Y_VV_TIMES_Y_VV_UNK:
            sprintf(*buf, "P_TIMES_Y_VV_TIMES_Y_VV_UNK\n");
            break;
        case P_TIMES_Y_VV_TIMES_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_TIMES_Y_VV_TIMES_Y_VV_NVAR_UNK\n");
            break;
        case P_TIMES_Y_VC_INSTINIT:
            sprintf(*buf, "P_TIMES_Y_VC_INSTINIT\n");
            break;
        case P_TIMES_Y_VC_TIMES_Y_VC_NVAR_INT:
            sprintf(*buf, "P_TIMES_Y_VC_TIMES_Y_VC_NVAR_INT\n");
            break;
		case P_TIMES_Y_VC_TIMES_Y_VC_NVAR_NOINT:
            sprintf(*buf, "P_TIMES_Y_VC_TIMES_Y_VC_NVAR_NOINT\n");
            break;
        case P_TIMES_Y_VC_NVAR_END:
            sprintf(*buf, "P_TIMES_Y_VC_NVAR_END\n");
            break;
        case P_TIMES_Y_VC_TIMES_Y_VC_UNK:
            sprintf(*buf, "P_TIMES_Y_VC_TIMES_Y_VC_UNK\n");
            break;
        case P_DIV_VV_INSTINIT:
            sprintf(*buf, "P_DIV_VV_INSTINIT\n");
            break;
        case P_DIV_VV_DIV_VV_NVAR:
            sprintf(*buf, "P_DIV_VV_DIV_VV_NVAR\n");
            break;
        case P_DIV_VV_DIV_VV_NVAR_NVAR_INT:
            sprintf(*buf, "P_DIV_VV_DIV_VV_NVAR_NVAR_INT\n");
            break;
		case P_DIV_VV_DIV_VV_NVAR_NVAR_NOINT:
            sprintf(*buf, "P_DIV_VV_DIV_VV_NVAR_NVAR_NOINT\n");
            break;
        case P_DIV_VV_DIV_VV_UNK:
            sprintf(*buf, "P_DIV_VV_DIV_VV_UNK\n");
            break;
        case P_DIV_VV_DIV_VV_NVAR_UNK:
            sprintf(*buf, "P_DIV_VV_DIV_VV_NVAR_UNK\n");
            break;
        case P_DIV_VC_INSTINIT:
            sprintf(*buf, "P_DIV_VC_INSTINIT\n");
            break;
        case P_DIV_VC_DIV_VC_NVAR:
            sprintf(*buf, "P_DIV_VC_DIV_VC_NVAR\n");
            break;
        case P_DIV_VC_INTTERM:
            sprintf(*buf, "P_DIV_VC_INTTERM\n");
            break;
        case P_DIV_VC_NOINTTERM:
            sprintf(*buf, "P_DIV_VC_NOINTTERM\n");
            break;
        case P_DIV_VC_D0EQUALS0L:
            sprintf(*buf, "P_DIV_VC_D0EQUALS0L\n");
            break;
        case P_DIV_VC_NVAR_END:
            sprintf(*buf, "P_DIV_VC_NVAR_END\n");
            break;
        case P_DIV_VC_DIV_VC_UNK:
            sprintf(*buf, "P_DIV_VC_DIV_VC_UNK\n");
            break;
        case P_DIV_CV_INSTINIT:
            sprintf(*buf, "P_DIV_CV_INSTINIT\n");
            break;
        case P_DIV_CV_DIV_CV_NVAR:
            sprintf(*buf, "P_DIV_CV_DIV_CV_NVAR\n");
            break;
        case P_DIV_CV_INTTERM_INIT:
            sprintf(*buf, "P_DIV_CV_INTTERM_INIT\n");
            break;
        case P_DIV_CV_INTTERM_DIVEQUALS0:
            sprintf(*buf, "P_DIV_CV_INTTERM_DIVEQUALS0\n");
            break;
        case P_DIV_CV_INTTERM_END:
            sprintf(*buf, "P_DIV_CV_INTTERM_END\n");
            break;
        case P_DIV_CV_NOINTTERM:
            sprintf(*buf, "P_DIV_CV_NOINTTERM\n");
            break;
        case P_DIV_CV_D0EQUALS0L:
            sprintf(*buf, "P_DIV_CV_D0EQUALS0L\n");
            break;
        case P_DIV_CV_NVAR_END:
            sprintf(*buf, "P_DIV_CV_NVAR_END\n");
            break;
        case P_DIV_CV_DIV_CV_UNK:
            sprintf(*buf, "P_DIV_CV_DIV_CV_UNK\n");
            break;
        case P_DIV_Y_VV_INSTINIT:
            sprintf(*buf, "P_DIV_Y_VV_INSTINIT\n");
            break;
        case P_DIV_Y_VV_DIV_Y_VV_NVAR:
            sprintf(*buf, "P_DIV_Y_VV_DIV_Y_VV_NVAR\n");
            break;
        case P_DIV_Y_VV_INTTERM_INIT:
            sprintf(*buf, "P_DIV_Y_VV_INTTERM_INIT\n");
            break;
        case P_DIV_Y_VV_INTTERM_DIVEQUALS0:
            sprintf(*buf, "P_DIV_Y_VV_INTTERM_DIVEQUALS0\n");
            break;
        case P_DIV_Y_VV_INTTERM_END:
            sprintf(*buf, "P_DIV_Y_VV_INTTERM_END\n");
            break;
        case P_DIV_Y_VV_NOINTTERM:
            sprintf(*buf, "P_DIV_Y_VV_NOINTTERM\n");
            break;
        case P_DIV_Y_VV_D0EQUALS0L:
            sprintf(*buf, "P_DIV_Y_VV_D0EQUALS0L\n");
            break;
        case P_DIV_Y_VV_NVAR_END:
            sprintf(*buf, "P_DIV_Y_VV_NVAR_END\n");
            break;
        case P_DIV_Y_VV_DIV_Y_VV_UNK:
            sprintf(*buf, "P_DIV_Y_VV_DIV_Y_VV_UNK\n");
            break;
        case P_DIV_Y_VV_DIV_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_DIV_Y_VV_DIV_Y_VV_NVAR_UNK\n");
            break;
        case P_DIV_Y_VC_INSTINIT:
            sprintf(*buf, "P_DIV_Y_VC_INSTINIT\n");
            break;
        case P_DIV_Y_VC_DIV_Y_VC_NVAR:
            sprintf(*buf, "P_DIV_Y_VC_DIV_Y_VC_NVAR\n");
            break;
        case P_DIV_Y_VC_INTTERM:
            sprintf(*buf, "P_DIV_Y_VC_INTTERM\n");
            break;
        case P_DIV_Y_VC_NOINTTERM:
            sprintf(*buf, "P_DIV_Y_VC_NOINTTERM\n");
            break;
        case P_DIV_Y_VC_D0EQUALS0L:
            sprintf(*buf, "P_DIV_Y_VC_D0EQUALS0L\n");
            break;
        case P_DIV_Y_VC_NVAR_END:
            sprintf(*buf, "P_DIV_Y_VC_NVAR_END\n");
            break;
        case P_DIV_Y_VC_DIV_Y_VC_UNK:
            sprintf(*buf, "P_DIV_Y_VC_DIV_Y_VC_UNK\n");
            break;
        case P_DIV_Y_CV_INSTINIT:
            sprintf(*buf, "P_DIV_Y_CV_INSTINIT\n");
            break;
        case P_DIV_Y_CV_DIV_Y_CV_NVAR:
            sprintf(*buf, "P_DIV_Y_CV_DIV_Y_CV_NVAR\n");
            break;
        case P_DIV_Y_CV_INTTERM_INIT:
            sprintf(*buf, "P_DIV_Y_CV_INTTERM_INIT\n");
            break;
        case P_DIV_Y_CV_INTTERM_DIVEQUALS0:
            sprintf(*buf, "P_DIV_Y_CV_INTTERM_DIVEQUALS0\n");
            break;
        case P_DIV_Y_CV_INTTERM_END:
            sprintf(*buf, "P_DIV_Y_CV_INTTERM_END\n");
            break;
        case P_DIV_Y_CV_NOINTTERM:
            sprintf(*buf, "P_DIV_Y_CV_NOINTTERM\n");
            break;
        case P_DIV_Y_CV_D0EQUALS0L:
            sprintf(*buf, "P_DIV_Y_CV_D0EQUALS0L\n");
            break;
        case P_DIV_Y_CV_NVAR_END:
            sprintf(*buf, "P_DIV_Y_CV_NVAR_END\n");
            break;
        case P_DIV_Y_CV_DIV_Y_CV_UNK:
            sprintf(*buf, "P_DIV_Y_CV_DIV_Y_CV_UNK\n");
            break;
        case P_AND_VV_INSTINIT:
            sprintf(*buf, "P_AND_VV_INSTINIT\n");
            break;
        case P_AND_VV_AND_VV_NVAR:
            sprintf(*buf, "P_AND_VV_AND_VV_NVAR\n");
            break;
        case P_AND_VV_AND_VV_NVAR_NVAR_INT:
            sprintf(*buf, "P_AND_VV_AND_VV_NVAR_NVAR_INT\n");
            break;
		case P_AND_VV_AND_VV_NVAR_NVAR_NOINT:
            sprintf(*buf, "P_AND_VV_AND_VV_NVAR_NVAR_NOINT\n");
            break;
        case P_AND_VV_AND_VV_UNK:
            sprintf(*buf, "P_AND_VV_AND_VV_UNK\n");
            break;
        case P_AND_VV_AND_VV_NVAR_UNK:
            sprintf(*buf, "P_AND_VV_AND_VV_NVAR_UNK\n");
            break;
        case P_AND_VC_INSTINIT:
            sprintf(*buf, "P_AND_VC_INSTINIT\n");
            break;
        case P_AND_VC_AND_VC_NVAR_INT:
            sprintf(*buf, "P_AND_VC_AND_VC_NVAR_INT\n");
            break;
		case P_AND_VC_AND_VC_NVAR_NOINT:
            sprintf(*buf, "P_AND_VC_AND_VC_NVAR_NOINT\n");
            break;
        case P_AND_VC_AND_VC_UNK:
            sprintf(*buf, "P_AND_VC_AND_VC_UNK\n");
            break;
        case P_AND_Y_VV_INSTINIT:
            sprintf(*buf, "P_AND_Y_VV_INSTINIT\n");
            break;
        case P_AND_Y_VV_AND_Y_VV_NVAR:
            sprintf(*buf, "P_AND_Y_VV_AND_Y_VV_NVAR\n");
            break;
        case P_AND_Y_VV_INTTERM:
            sprintf(*buf, "P_AND_Y_VV_INTTERM\n");
            break;
        case P_AND_Y_VV_NOINTTERM:
            sprintf(*buf, "P_AND_Y_VV_NOINTTERM\n");
            break;
        case P_AND_Y_VV_D0EQUALS0L:
            sprintf(*buf, "P_AND_Y_VV_D0EQUALS0L\n");
            break;
        case P_AND_Y_VV_NVAR_END:
            sprintf(*buf, "P_AND_Y_VV_NVAR_END\n");
            break;
        case P_AND_Y_VV_AND_Y_VV_UNK:
            sprintf(*buf, "P_AND_Y_VV_AND_Y_VV_UNK\n");
            break;
        case P_AND_Y_VV_AND_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_AND_Y_VV_AND_Y_VV_NVAR_UNK\n");
            break;
        case P_AND_Y_VC_INSTINIT:
            sprintf(*buf, "P_AND_Y_VC_INSTINIT\n");
            break;
        case P_AND_Y_VC_AND_Y_VC_NVAR:
            sprintf(*buf, "P_AND_Y_VC_AND_Y_VC_NVAR\n");
            break;
        case P_AND_Y_VC_INTTERM:
            sprintf(*buf, "P_AND_Y_VC_INTTERM\n");
            break;
        case P_AND_Y_VC_NOINTTERM:
            sprintf(*buf, "P_AND_Y_VC_NOINTTERM\n");
            break;
        case P_AND_Y_VC_D0EQUALS0L:
            sprintf(*buf, "P_AND_Y_VC_D0EQUALS0L\n");
            break;
        case P_AND_Y_VC_NVAR_END:
            sprintf(*buf, "P_AND_Y_VC_NVAR_END\n");
            break;
        case P_AND_Y_VC_AND_Y_VC_UNK:
            sprintf(*buf, "P_AND_Y_VC_AND_Y_VC_UNK\n");
            break;
        case P_OR_VV_INSTINIT:
            sprintf(*buf, "P_OR_VV_INSTINIT\n");
            break;
        case P_OR_VV_OR_VV_NVAR:
            sprintf(*buf, "P_OR_VV_OR_VV_NVAR\n");
            break;
        case P_OR_VV_INTTERM:
            sprintf(*buf, "P_OR_VV_INTTERM\n");
            break;
        case P_OR_VV_NOINTTERM:
            sprintf(*buf, "P_OR_VV_NOINTTERM\n");
            break;
        case P_OR_VV_D0EQUALS0L:
            sprintf(*buf, "P_OR_VV_D0EQUALS0L\n");
            break;
        case P_OR_VV_NVAR_END:
            sprintf(*buf, "P_OR_VV_NVAR_END\n");
            break;
        case P_OR_VV_OR_VV_UNK:
            sprintf(*buf, "P_OR_VV_OR_VV_UNK\n");
            break;
        case P_OR_VV_OR_VV_NVAR_UNK:
            sprintf(*buf, "P_OR_VV_OR_VV_NVAR_UNK\n");
            break;
        case P_OR_VC_INSTINIT:
            sprintf(*buf, "P_OR_VC_INSTINIT\n");
            break;
        case P_OR_VC_OR_VC_NVAR:
            sprintf(*buf, "P_OR_VC_OR_VC_NVAR\n");
            break;
        case P_OR_VC_INTTERM:
            sprintf(*buf, "P_OR_VC_INTTERM\n");
            break;
        case P_OR_VC_NOINTTERM:
            sprintf(*buf, "P_OR_VC_NOINTTERM\n");
            break;
        case P_OR_VC_D0EQUALS0L:
            sprintf(*buf, "P_OR_VC_D0EQUALS0L\n");
            break;
        case P_OR_VC_NVAR_END:
            sprintf(*buf, "P_OR_VC_NVAR_END\n");
            break;
        case P_OR_VC_OR_VC_UNK:
            sprintf(*buf, "P_OR_VC_OR_VC_UNK\n");
            break;
        case P_OR_Y_VV_INSTINIT:
            sprintf(*buf, "P_OR_Y_VV_INSTINIT\n");
            break;
        case P_OR_Y_VV_OR_Y_VV_NVAR:
            sprintf(*buf, "P_OR_Y_VV_OR_Y_VV_NVAR\n");
            break;
        case P_OR_Y_VV_INTTERM:
            sprintf(*buf, "P_OR_Y_VV_INTTERM\n");
            break;
        case P_OR_Y_VV_NOINTTERM:
            sprintf(*buf, "P_OR_Y_VV_NOINTTERM\n");
            break;
        case P_OR_Y_VV_D0EQUALS0L:
            sprintf(*buf, "P_OR_Y_VV_D0EQUALS0L\n");
            break;
        case P_OR_Y_VV_NVAR_END:
            sprintf(*buf, "P_OR_Y_VV_NVAR_END\n");
            break;
        case P_OR_Y_VV_OR_Y_VV_UNK:
            sprintf(*buf, "P_OR_Y_VV_OR_Y_VV_UNK\n");
            break;
        case P_OR_Y_VV_OR_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_OR_Y_VV_OR_Y_VV_NVAR_UNK\n");
            break;
        case P_OR_Y_VC_INSTINIT:
            sprintf(*buf, "P_OR_Y_VC_INSTINIT\n");
            break;
        case P_OR_Y_VC_OR_Y_VC_NVAR:
            sprintf(*buf, "P_OR_Y_VC_OR_Y_VC_NVAR\n");
            break;
        case P_OR_Y_VC_INTTERM:
            sprintf(*buf, "P_OR_Y_VC_INTTERM\n");
            break;
        case P_OR_Y_VC_NOINTTERM:
            sprintf(*buf, "P_OR_Y_VC_NOINTTERM\n");
            break;
        case P_OR_Y_VC_D0EQUALS0L:
            sprintf(*buf, "P_OR_Y_VC_D0EQUALS0L\n");
            break;
        case P_OR_Y_VC_NVAR_END:
            sprintf(*buf, "P_OR_Y_VC_NVAR_END\n");
            break;
        case P_OR_Y_VC_OR_Y_VC_UNK:
            sprintf(*buf, "P_OR_Y_VC_OR_Y_VC_UNK\n");
            break;
        case P_SLL_VV_INSTINIT:
            sprintf(*buf, "P_SLL_VV_INSTINIT\n");
            break;
        case P_SLL_VV_SLL_VV_NVAR:
            sprintf(*buf, "P_SLL_VV_SLL_VV_NVAR\n");
            break;
        case P_SLL_VV_INTTERM_INIT:
            sprintf(*buf, "P_SLL_VV_INTTERM_INIT\n");
            break;
        case P_SLL_VV_INTTERM_LESS:
            sprintf(*buf, "P_SLL_VV_INTTERM_LESS\n");
            break;
        case P_SLL_VV_INTTERM_GREATER:
            sprintf(*buf, "P_SLL_VV_INTTERM_GREATER\n");
            break;
        case P_SLL_VV_NOINTTERM:
            sprintf(*buf, "P_SLL_VV_NOINTTERM\n");
            break;
        case P_SLL_VV_D0EQUALS0L:
            sprintf(*buf, "P_SLL_VV_D0EQUALS0L\n");
            break;
        case P_SLL_VV_NVAR_END:
            sprintf(*buf, "P_SLL_VV_NVAR_END\n");
            break;
        case P_SLL_VV_SLL_VV_UNK:
            sprintf(*buf, "P_SLL_VV_SLL_VV_UNK\n");
            break;
        case P_SLL_VV_SLL_VV_NVAR_UNK:
            sprintf(*buf, "P_SLL_VV_SLL_VV_NVAR_UNK\n");
            break;
        case P_SLL_VC_INSTINIT:
            sprintf(*buf, "P_SLL_VC_INSTINIT\n");
            break;
        case P_SLL_VC_SLL_VC_NVAR:
            sprintf(*buf, "P_SLL_VC_SLL_VC_NVAR\n");
            break;
        case P_SLL_VC_INTTERM:
            sprintf(*buf, "P_SLL_VC_INTTERM\n");
            break;
        case P_SLL_VC_NOINTTERM:
            sprintf(*buf, "P_SLL_VC_NOINTTERM\n");
            break;
        case P_SLL_VC_D0EQUALS0L:
            sprintf(*buf, "P_SLL_VC_D0EQUALS0L\n");
            break;
        case P_SLL_VC_NVAR_END:
            sprintf(*buf, "P_SLL_VC_NVAR_END\n");
            break;
        case P_SLL_VC_SLL_VC_UNK:
            sprintf(*buf, "P_SLL_VC_SLL_VC_UNK\n");
            break;
        case P_SLL_CV_INSTINIT:
            sprintf(*buf, "P_SLL_CV_INSTINIT\n");
            break;
        case P_SLL_CV_SLL_CV_NVAR_INT:
            sprintf(*buf, "P_SLL_CV_SLL_CV_NVAR_INT\n");
            break;
		case P_SLL_CV_SLL_CV_NVAR_NOINT:
            sprintf(*buf, "P_SLL_CV_SLL_CV_NVAR_NOINT\n");
            break;
        case P_SLL_CV_SLL_CV_UNK:
            sprintf(*buf, "P_SLL_CV_SLL_CV_UNK\n");
            break;
        case P_SLL_Y_VV_INSTINIT:
            sprintf(*buf, "P_SLL_Y_VV_INSTINIT\n");
            break;
        case P_SLL_Y_VV_SLL_Y_VV_NVAR:
            sprintf(*buf, "P_SLL_Y_VV_SLL_Y_VV_NVAR\n");
            break;
        case P_SLL_Y_VV_INTTERM_INIT:
            sprintf(*buf, "P_SLL_Y_VV_INTTERM_INIT\n");
            break;
        case P_SLL_Y_VV_INTERM_LESS:
            sprintf(*buf, "P_SLL_Y_VV_INTERM_LESS\n");
            break;
        case P_SLL_Y_VV_INTTERM_GREATER:
            sprintf(*buf, "P_SLL_Y_VV_INTTERM_GREATER\n");
            break;
        case P_SLL_Y_VV_NOINTTERM:
            sprintf(*buf, "P_SLL_Y_VV_NOINTTERM\n");
            break;
        case P_SLL_Y_VV_D0EQUALS0L:
            sprintf(*buf, "P_SLL_Y_VV_D0EQUALS0L\n");
            break;
        case P_SLL_Y_VV_NVAR_END:
            sprintf(*buf, "P_SLL_Y_VV_NVAR_END\n");
            break;
        case P_SLL_Y_VV_SLL_Y_VV_UNK:
            sprintf(*buf, "P_SLL_Y_VV_SLL_Y_VV_UNK\n");
            break;
        case P_SLL_Y_VV_SLL_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_SLL_Y_VV_SLL_Y_VV_NVAR_UNK\n");
            break;
        case P_SLL_Y_VC_INSTINIT:
            sprintf(*buf, "P_SLL_Y_VC_INSTINIT\n");
            break;
        case P_SLL_Y_VC_SLL_Y_VC_NVAR:
            sprintf(*buf, "P_SLL_Y_VC_SLL_Y_VC_NVAR\n");
            break;
        case P_SLL_Y_VC_INTTERM:
            sprintf(*buf, "P_SLL_Y_VC_INTTERM\n");
            break;
        case P_SLL_Y_VC_NOINTTERM:
            sprintf(*buf, "P_SLL_Y_VC_NOINTTERM\n");
            break;
        case P_SLL_Y_VC_D0EQUALS0L:
            sprintf(*buf, "P_SLL_Y_VC_D0EQUALS0L\n");
            break;
        case P_SLL_Y_VC_NVAR_END:
            sprintf(*buf, "P_SLL_Y_VC_NVAR_END\n");
            break;
        case P_SLL_Y_VC_SLL_Y_VC_UNK:
            sprintf(*buf, "P_SLL_Y_VC_SLL_Y_VC_UNK\n");
            break;
        case P_SLL_Y_CV_INSTINIT:
            sprintf(*buf, "P_SLL_Y_CV_INSTINIT\n");
            break;
        case P_SLL_Y_CV_SLL_Y_CV_NVAR:
            sprintf(*buf, "P_SLL_Y_CV_SLL_Y_CV_NVAR\n");
            break;
        case P_SLL_Y_CV_INTTERM_INIT:
            sprintf(*buf, "P_SLL_Y_CV_INTTERM_INIT\n");
            break;
        case P_SLL_Y_CV_INTTERM_LESS:
            sprintf(*buf, "P_SLL_Y_CV_INTTERM_LESS\n");
            break;
        case P_SLL_Y_CV_INTTERM_GREATER:
            sprintf(*buf, "P_SLL_Y_CV_INTTERM_GREATER\n");
            break;
        case P_SLL_Y_CV_NOINTTERM:
            sprintf(*buf, "P_SLL_Y_CV_NOINTTERM\n");
            break;
        case P_SLL_Y_CV_D0EQUALS0L:
            sprintf(*buf, "P_SLL_Y_CV_D0EQUALS0L\n");
            break;
        case P_SLL_Y_CV_NVAR_END:
            sprintf(*buf, "P_SLL_Y_CV_NVAR_END\n");
            break;
        case P_SLL_Y_CV_SLL_Y_CV_UNK:
            sprintf(*buf, "P_SLL_Y_CV_SLL_Y_CV_UNK\n");
            break;
        case P_SLR_VV_INSTINIT:
            sprintf(*buf, "P_SLR_VV_INSTINIT\n");
            break;
        case P_SLR_VV_SLR_VV_NVAR:
            sprintf(*buf, "P_SLR_VV_SLR_VV_NVAR\n");
            break;
        case P_SLR_VV_INTTERM_INIT:
            sprintf(*buf, "P_SLR_VV_INTTERM_INIT\n");
            break;
        case P_SLR_VV_INTTERM_LESS:
            sprintf(*buf, "P_SLR_VV_INTTERM_LESS\n");
            break;
        case P_SLR_VV_INTTERM_GREATER:
            sprintf(*buf, "P_SLR_VV_INTTERM_GREATER\n");
            break;
        case P_SLR_VV_NOINTTERM:
            sprintf(*buf, "P_SLR_VV_NOINTTERM\n");
            break;
        case P_SLR_VV_D0EQUALS0L:
            sprintf(*buf, "P_SLR_VV_D0EQUALS0L\n");
            break;
        case P_SLR_VV_NVAR_END:
            sprintf(*buf, "P_SLR_VV_NVAR_END\n");
            break;
        case P_SLR_VV_SRL_VV_UNK:
            sprintf(*buf, "P_SLR_VV_SRL_VV_UNK\n");
            break;
        case P_SLR_VV_SRL_VV_NVAR_UNK:
            sprintf(*buf, "P_SLR_VV_SRL_VV_NVAR_UNK\n");
            break;
        case P_SLR_VC_INSTINIT:
            sprintf(*buf, "P_SLR_VC_INSTINIT\n");
            break;
        case P_SLR_VC_SLR_VC_NVAR_INT:
            sprintf(*buf, "P_SLR_VC_SLR_VC_NVAR_INT\n");
            break;
		case P_SLR_VC_SLR_VC_NVAR_NOINT:
            sprintf(*buf, "P_SLR_VC_SLR_VC_NVAR_NOINT\n");
            break;
        case P_SLR_VC_SRL_VC_UNK:
            sprintf(*buf, "P_SLR_VC_SRL_VC_UNK\n");
            break;
        case P_SLR_CV_INSTINIT:
            sprintf(*buf, "P_SLR_CV_INSTINIT\n");
            break;
        case P_SLR_CV_SLR_CV_NVAR:
            sprintf(*buf, "P_SLR_CV_SLR_CV_NVAR\n");
            break;
        case P_SLR_CV_INTTERM_INIT:
            sprintf(*buf, "P_SLR_CV_INTTERM_INIT\n");
            break;
        case P_SLR_CV_INTTERM_LESS:
            sprintf(*buf, "P_SLR_CV_INTTERM_LESS\n");
            break;
        case P_SLR_CV_INTTERM_GREATER:
            sprintf(*buf, "P_SLR_CV_INTTERM_GREATER\n");
            break;
        case P_SLR_CV_NOINTTERM:
            sprintf(*buf, "P_SLR_CV_NOINTTERM\n");
            break;
        case P_SLR_CV_D0EQUALS0L:
            sprintf(*buf, "P_SLR_CV_D0EQUALS0L\n");
            break;
        case P_SLR_CV_NVAR_END:
            sprintf(*buf, "P_SLR_CV_NVAR_END\n");
            break;
        case P_SLR_CV_SLR_CV_UNK:
            sprintf(*buf, "P_SLR_CV_SLR_CV_UNK\n");
            break;
        case P_SLR_Y_VV_INSTINIT:
            sprintf(*buf, "P_SLR_Y_VV_INSTINIT\n");
            break;
        case P_SLR_Y_VV_SLR_Y_VV_NVAR:
            sprintf(*buf, "P_SLR_Y_VV_SLR_Y_VV_NVAR\n");
            break;
        case P_SLR_Y_VV_INTTERM_INIT:
            sprintf(*buf, "P_SLR_Y_VV_INTTERM_INIT\n");
            break;
        case P_SLR_Y_VV_INTTERM_LESS:
            sprintf(*buf, "P_SLR_Y_VV_INTTERM_LESS\n");
            break;
        case P_SLR_Y_VV_INTTERM_GREATER:
            sprintf(*buf, "P_SLR_Y_VV_INTTERM_GREATER\n");
            break;
        case P_SLR_Y_VV_NOINTTERM:
            sprintf(*buf, "P_SLR_Y_VV_NOINTTERM\n");
            break;
        case P_SLR_Y_VV_D0EQUALS0L:
            sprintf(*buf, "P_SLR_Y_VV_D0EQUALS0L\n");
            break;
        case P_SLR_Y_VV_NVAR_END:
            sprintf(*buf, "P_SLR_Y_VV_NVAR_END\n");
            break;
        case P_SLR_Y_VV_SLR_Y_VV_UNK:
            sprintf(*buf, "P_SLR_Y_VV_SLR_Y_VV_UNK\n");
            break;
        case P_SLR_Y_VV_SLR_Y_VV_NVAR_UNK:
            sprintf(*buf, "P_SLR_Y_VV_SLR_Y_VV_NVAR_UNK\n");
            break;
        case P_SLR_Y_VC_INSTINIT:
            sprintf(*buf, "P_SLR_Y_VC_INSTINIT\n");
            break;
        case P_SLR_Y_VC_SLR_Y_VC_NVAR:
            sprintf(*buf, "P_SLR_Y_VC_SLR_Y_VC_NVAR\n");
            break;
        case P_SLR_Y_VC_INTTERM:
            sprintf(*buf, "P_SLR_Y_VC_INTTERM\n");
            break;
        case P_SLR_Y_VC_NOINTTERM:
            sprintf(*buf, "P_SLR_Y_VC_NOINTTERM\n");
            break;
        case P_SLR_Y_VC_D0EQUALS0L:
            sprintf(*buf, "P_SLR_Y_VC_D0EQUALS0L\n");
            break;
        case P_SLR_Y_VC_NVAR_END:
            sprintf(*buf, "P_SLR_Y_VC_NVAR_END\n");
            break;
        case P_SLR_Y_VC_SLR_Y_VC_UNK:
            sprintf(*buf, "P_SLR_Y_VC_SLR_Y_VC_UNK\n");
            break;
        case P_SLR_Y_CV_INSTINIT:
            sprintf(*buf, "P_SLR_Y_CV_INSTINIT\n");
            break;
        case P_SLR_Y_CV_SLR_Y_CV_NVAR:
            sprintf(*buf, "P_SLR_Y_CV_SLR_Y_CV_NVAR\n");
            break;
        case P_SLR_Y_CV_INTTERM_INIT:
            sprintf(*buf, "P_SLR_Y_CV_INTTERM_INIT\n");
            break;
        case P_SLR_Y_CV_INTTERM_LESS:
            sprintf(*buf, "P_SLR_Y_CV_INTTERM_LESS\n");
            break;
        case P_SLR_Y_CV_INTTERM_GREATER:
            sprintf(*buf, "P_SLR_Y_CV_INTTERM_GREATER\n");
            break;
        case P_SLR_Y_CV_NOINTTERM:
            sprintf(*buf, "P_SLR_Y_CV_NOINTTERM\n");
            break;
        case P_SLR_Y_CV_D0EQUALS0L:
            sprintf(*buf, "P_SLR_Y_CV_D0EQUALS0L\n");
            break;
        case P_SLR_Y_CV_NVAR_END:
            sprintf(*buf, "P_SLR_Y_CV_NVAR_END\n");
            break;
        case P_SLR_Y_CV_SLR_Y_CV_UNK:
            sprintf(*buf, "P_SLR_Y_CV_SLR_Y_CV_UNK\n");
            break;
        case CALL_BFUNC_XX_INSTINIT:
            sprintf(*buf, "CALL_BFUNC_XX_INSTINIT\n");
            break;
        case CALL_BFUNC_XX_CALL_BFUNC_XX_NVAR:
            sprintf(*buf, "CALL_BFUNC_XX_CALL_BFUNC_XX_NVAR\n");
            break;
        case CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_INT:
            sprintf(*buf, "CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_INT\n");
            break;
        case CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_NOINT:
            sprintf(*buf, "CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_NOINT\n");
            break;
        case CALL_BFUNC_XX_CALL_BFUNC_XX_UNK:
            sprintf(*buf, "CALL_BFUNC_XX_CALL_BFUNC_XX_UNK\n");
            break;
        case CALL_BFUNC_YX_INSTINIT:
            sprintf(*buf, "CALL_BFUNC_YX_INSTINIT\n");
            break;
        case CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_INT:
            sprintf(*buf, "CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_INT\n");
            break;
        case CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_NOINT:
            sprintf(*buf, "CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_NOINT\n");
            break;
        case CALL_BFUNC_YX_CALL_BFUNC_YX_UNK:
            sprintf(*buf, "CALL_BFUNC_YX_CALL_BFUNC_YX_UNK\n");
            break;
        case CALL_BFUNC_XY_INSTINIT:
            sprintf(*buf, "CALL_BFUNC_XY_INSTINIT\n");
            break;
        case CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_INT:
            sprintf(*buf, "CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_INT\n");
            break;
        case CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_NOINT:
            sprintf(*buf, "CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_NOINT\n");
            break;
        case CALL_BFUNC_XY_CALL_BFUNC_XY_UNK:
            sprintf(*buf, "CALL_BFUNC_XY_CALL_BFUNC_XY_UNK\n");
            break;
        case CALL_BFUNC_YY_INSTINIT:
            sprintf(*buf, "CALL_BFUNC_YY_INSTINIT\n");
            break;
        case CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_INT:
            sprintf(*buf, "CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_INT\n");
            break;
        case CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_NOINT:
            sprintf(*buf, "CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_NOINT\n");
            break;
        case CALL_BFUNC_YY_CALL_BFUNC_YY_UNK:
            sprintf(*buf, "CALL_BFUNC_YY_CALL_BFUNC_YY_UNK\n");
            break;
        case P_EQUAL_INSTINIT:
            sprintf(*buf, "P_EQUAL_INSTINIT\n");
            break;
        case P_EQUAL_END:
            sprintf(*buf, "P_EQUAL_END\n");
            break;
        case P_DIF_INSTINIT:
            sprintf(*buf, "P_DIF_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_DIF_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_DIF_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_DIF_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_DIF_POST_LOW_LEVEL_TRACER\n");
            break;
        case P_DIF_DIF_NVAR1:
            sprintf(*buf, "P_DIF_DIF_NVAR1\n");
            break;
        case P_DIF_DIF_NVAR1_NVAR2:
            sprintf(*buf, "P_DIF_DIF_NVAR1_NVAR2\n");
            break;
            break;
        case P_DIF_DIF_UNK1:
            sprintf(*buf, "P_DIF_DIF_UNK1\n");
            break;
        case P_DIF_DIF_NVAR1_UNK2:
            sprintf(*buf, "P_DIF_DIF_NVAR1_UNK2\n");
            break;
        case P_EQ_INSTINIT:
            sprintf(*buf, "P_EQ_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_EQ_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_EQ_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_EQ_POST_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_EQ_POST_LOW_LEVEL_TRACER\n");
            break;
        case P_EQ_P_EQ_NVAR1:
            sprintf(*buf, "P_EQ_P_EQ_NVAR1\n");
            break;
        case P_EQ_P_EQ_NVAR1_NVAR2:
            sprintf(*buf, "P_EQ_P_EQ_NVAR1_NVAR2\n");
            break;
        case P_EQ_P_EQ_NVAR1_UNK2:
            sprintf(*buf, "P_EQ_P_EQ_NVAR1_UNK2\n");
            break;
        case P_EQ_P_EQ_UNK1:
            sprintf(*buf, "P_EQ_P_EQ_UNK1\n");
            break;
        case P_EQ_P_EQ_VAR1_NVAR2:
            sprintf(*buf, "P_EQ_P_EQ_VAR1_NVAR2\n");
            break;
        case P_EQ_P_EQ_VAR1_UNK2_END:
            sprintf(*buf, "P_EQ_P_EQ_VAR1_UNK2_END\n");
            break;
        case P_ARG_VV_INSTINIT:
            sprintf(*buf, "P_ARG_VV_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_ARG_VV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_ARG_VV_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_ARG_VV_TEST_D0:
            sprintf(*buf, "P_ARG_VV_TEST_D0\n");
            break;
        case P_ARG_VV_ARG_ARG1_NVAR:
            sprintf(*buf, "P_ARG_VV_ARG_ARG1_NVAR\n");
            break;
        case P_ARG_VV_TEST_D1:
            sprintf(*buf, "P_ARG_VV_TEST_D1\n");
            break;
        case P_ARG_VV_ARG_ARG2_NVAR:
            sprintf(*buf, "P_ARG_VV_ARG_ARG2_NVAR\n");
            break;
        case P_ARG_VV_ARG_ARG2_UNK:
            sprintf(*buf, "P_ARG_VV_ARG_ARG2_UNK\n");
            break;
        case P_ARG_VV_ARG_ARG1_UNK:
            sprintf(*buf, "P_ARG_VV_ARG_ARG1_UNK\n");
            break;
        case P_ARG_CV_INSTINIT:
            sprintf(*buf, "P_ARG_CV_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_ARG_CV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_ARG_CV_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_ARG_CV_TEST_D1:
            sprintf(*buf, "P_ARG_CV_TEST_D1\n");
            break;
        case P_ARG_CV_ARG_ARG2_VC_NVAR:
            sprintf(*buf, "P_ARG_CV_ARG_ARG2_VC_NVAR\n");
            break;
        case P_ARG_CV_ARG_ARG2_VC_UNK:
            sprintf(*buf, "P_ARG_CV_ARG_ARG2_VC_UNK\n");
            break;
        case P_ARG_Y_VV_INSTINIT:
            sprintf(*buf, "P_ARG_Y_VV_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_ARG_Y_VV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_ARG_Y_VV_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_ARG_Y_VV_TEST_D0:
            sprintf(*buf, "P_ARG_Y_VV_TEST_D0\n");
            break;
        case P_ARG_Y_VV_ARG_Y_ARG1_NVAR:
            sprintf(*buf, "P_ARG_Y_VV_ARG_Y_ARG1_NVAR\n");
            break;
        case P_ARG_Y_VV_TEST_D1:
            sprintf(*buf, "P_ARG_Y_VV_TEST_D1\n");
            break;
        case P_ARG_Y_VV_ARG_Y_ARG2_NVAR:
            sprintf(*buf, "P_ARG_Y_VV_ARG_Y_ARG2_NVAR\n");
            break;
        case P_ARG_Y_VV_ARG_Y_ARG2_UNK:
            sprintf(*buf, "P_ARG_Y_VV_ARG_Y_ARG2_UNK\n");
            break;
        case P_ARG_Y_VV_ARG_Y_ARG1_UNK:
            sprintf(*buf, "P_ARG_Y_VV_ARG_Y_ARG1_UNK\n");
            break;
        case P_ARG_Y_CV_INSTINIT:
            sprintf(*buf, "P_ARG_Y_CV_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_ARG_Y_CV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_ARG_Y_CV_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_ARG_Y_CV_TEST_D1:
            sprintf(*buf, "P_ARG_Y_CV_TEST_D1\n");
            break;
        case P_ARG_Y_CV_D1APPL_INIT:
            sprintf(*buf, "P_ARG_Y_CV_D1APPL_INIT\n");
            break;
        case P_ARG_Y_CV_D1APPL_END:
            sprintf(*buf, "P_ARG_Y_CV_D1APPL_END\n");
            break;
        case P_ARG_Y_CV_D1PAIR_INIT:
            sprintf(*buf, "P_ARG_Y_CV_D1PAIR_INIT\n");
            break;
        case P_ARG_Y_CV_D1PAIR_LESS0:
            sprintf(*buf, "P_ARG_Y_CV_D1PAIR_LESS0\n");
            break;
        case P_ARG_Y_CV_D1PAIR_END:
            sprintf(*buf, "P_ARG_Y_CV_D1PAIR_END\n");
            break;
        case P_ARG_Y_CV_ARG_Y_ARG2_VC_UNK:
            sprintf(*buf, "P_ARG_Y_CV_ARG_Y_ARG2_VC_UNK\n");
            break;
        case P_FUNCTOR_INSTINIT:
            sprintf(*buf, "P_FUNCTOR_INSTINIT\n");
            break;
        case P_FUNCTOR_END:
            sprintf(*buf, "P_FUNCTOR_END\n");
            break;
        case P_FUNC2S_VV_INSTINIT:
            sprintf(*buf, "P_FUNC2S_VV_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2S_VV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2S_VV_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_FUNC2S_TEST_D0:
            sprintf(*buf, "P_FUNC2S_TEST_D0\n");
            break;
        case P_FUNC2S_VV_TEST_D1:
            sprintf(*buf, "P_FUNC2S_VV_TEST_D1\n");
            break;
        case P_FUNC2S_VV_D1INT:
            sprintf(*buf, "P_FUNC2S_VV_D1INT\n");
            break;
        case P_FUNC2S_VV_D1NOTINT:
            sprintf(*buf, "P_FUNC2S_VV_D1NOTINT\n");
            break;
        case P_FUNC2S_VV_D1BIGINT:
            sprintf(*buf, "P_FUNC2S_VV_D1BIGINT\n");
            break;
        case P_FUNC2S_VV_D1NOTBIGINT:
            sprintf(*buf, "P_FUNC2S_VV_D1NOTBIGINT\n");
            break;
        case P_FUNC2S_VV_D1NOTINT_END:
            sprintf(*buf, "P_FUNC2S_VV_D1NOTINT_END\n");
            break;
        case P_FUNC2S_VV_D0NOTATOMIC:
            sprintf(*buf, "P_FUNC2S_VV_D0NOTATOMIC\n");
            break;
        case P_FUNC2S_VV_FIRSTIFOK:
            sprintf(*buf, "P_FUNC2S_VV_FIRSTIFOK\n");
            break;
        case P_FUNC2S_VV_SECONDIFOK_D0NOTATOM:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_D0NOTATOM\n");
            break;
        case P_FUNC2S_VV_SECONDIFOK_D0ATOM:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_D0ATOM\n");
            break;
        case P_FUNC2S_VV_SECONDIFOK_POST_D0ATOM:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_POST_D0ATOM\n");
            break;
        case P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_INIT:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_INIT\n");
            break;
        case P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_IFOK:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_IFOK\n");
            break;
        case P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_NOIF:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_NOIF\n");
            break;
        case P_FUNC2S_VV_SECONDIFOK_INSIDEWHILE:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_INSIDEWHILE\n");
            break;
        case P_FUNC2S_VV_SECONDIFOK_END:
            sprintf(*buf, "P_FUNC2S_VV_SECONDIFOK_END\n");
            break;
        case P_FUNC2S_VV_THIRDIFOK:
            sprintf(*buf, "P_FUNC2S_VV_THIRDIFOK\n");
            break;
        case P_FUNC2S_VV_ELSE:
            sprintf(*buf, "P_FUNC2S_VV_ELSE\n");
            break;
        case P_FUNC2S_VV_FUNC2S_UNK2:
            sprintf(*buf, "P_FUNC2S_VV_FUNC2S_UNK2\n");
            break;
        case P_FUNC2S_VV_FUNC2S_UNK:
            sprintf(*buf, "P_FUNC2S_VV_FUNC2S_UNK\n");
            break;
        case P_FUNC2S_CV_INSTINIT:
            sprintf(*buf, "P_FUNC2S_CV_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2S_CV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2S_CV_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_FUNC2S_CV_TEST_D1:
            sprintf(*buf, "P_FUNC2S_CV_TEST_D1\n");
            break;
        case P_FUNC2S_CV_D1INT:
            sprintf(*buf, "P_FUNC2S_CV_D1INT\n");
            break;
        case P_FUNC2S_CV_D1NOTINT:
            sprintf(*buf, "P_FUNC2S_CV_D1NOTINT\n");
            break;
        case P_FUNC2S_CV_D1NOINT_D1BIGINT:
            sprintf(*buf, "P_FUNC2S_CV_D1NOINT_D1BIGINT\n");
            break;
        case P_FUNC2S_CV_D1NOTBIGINT:
            sprintf(*buf, "P_FUNC2S_CV_D1NOTBIGINT\n");
            break;
        case P_FUNC2S_CV_POST_IF:
            sprintf(*buf, "P_FUNC2S_CV_POST_IF\n");
            break;
        case P_FUNC2S_CV_FIRSTIFOK:
            sprintf(*buf, "P_FUNC2S_CV_FIRSTIFOK\n");
            break;
        case P_FUNC2S_CV_D1GREATER_D0NOTATOM:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_D0NOTATOM\n");
            break;
        case P_FUNC2S_CV_D1GREATER_D0ATOM:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_D0ATOM\n");
            break;
        case P_FUNC2S_CV_D1GREATER_POST_IF:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_POST_IF\n");
            break;
        case P_FUNC2S_CV_D1GREATER_IFOK_INIT:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_IFOK_INIT\n");
            break;
        case P_FUNC2S_CV_D1GREATER_IFOK_IFOK:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_IFOK_IFOK\n");
            break;
        case P_FUNC2S_CV_D1GREATER_IFOK_NOIF:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_IFOK_NOIF\n");
            break;
        case P_FUNC2S_CV_D1GREATER_INSIDEWHILE:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_INSIDEWHILE\n");
            break;
        case P_FUNC2S_CV_D1GREATER_END:
            sprintf(*buf, "P_FUNC2S_CV_D1GREATER_END\n");
            break;
        case P_FUNC2S_CV_D1ISZERO:
            sprintf(*buf, "P_FUNC2S_CV_D1ISZERO\n");
            break;
        case P_FUNC2S_CV_ELSE:
            sprintf(*buf, "P_FUNC2S_CV_ELSE\n");
            break;
        case P_FUNC2S_CV_END:
            sprintf(*buf, "P_FUNC2S_CV_END\n");
            break;
        case P_FUNC2S_VC_INSTINIT:
            sprintf(*buf, "P_FUNC2S_VC_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2S_VC_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2S_VC_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_FUNC2S_VC_TEST_D0:
            sprintf(*buf, "P_FUNC2S_VC_TEST_D0\n");
            break;
        case P_FUNC2S_VC_FUNC2S_NVAR_VC:
            sprintf(*buf, "P_FUNC2S_VC_FUNC2S_NVAR_VC\n");
            break;
        case P_FUNC2S_VC_D0NOATOMIC:
            sprintf(*buf, "P_FUNC2S_VC_D0NOATOMIC\n");
            break;
        case P_FUNC2S_VC_EQUALS:
            sprintf(*buf, "P_FUNC2S_VC_EQUALS\n");
            break;
        case P_FUNC2S_VC_D1ISZERO:
            sprintf(*buf, "P_FUNC2S_VC_D1ISZERO\n");
            break;
        case P_FUNC2S_VC_D0NOATOM:
            sprintf(*buf, "P_FUNC2S_VC_D0NOATOM\n");
            break;
        case P_FUNC2S_VC_D0ATOM:
            sprintf(*buf, "P_FUNC2S_VC_D0ATOM\n");
            break;
        case P_FUNC2S_VC_POST_ELSE:
            sprintf(*buf, "P_FUNC2S_VC_POST_ELSE\n");
            break;
        case P_FUNC2S_VC_IFOK_INIT:
            sprintf(*buf, "P_FUNC2S_VC_IFOK_INIT\n");
            break;
        case P_FUNC2S_VC_IFOK_IFOK:
            sprintf(*buf, "P_FUNC2S_VC_IFOK_IFOK\n");
            break;
        case P_FUNC2S_VC_IFOK_NOIF:
            sprintf(*buf, "P_FUNC2S_VC_IFOK_NOIF\n");
            break;
        case P_FUNC2S_VC_INSIDEWHILE:
            sprintf(*buf, "P_FUNC2S_VC_INSIDEWHILE\n");
            break;
        case P_FUNC2S_VC_END1:
            sprintf(*buf, "P_FUNC2S_VC_END1\n");
            break;
        case P_FUNC2S_VC_END2:
            sprintf(*buf, "P_FUNC2S_VC_END2\n");
            break;
        case P_FUNC2S_Y_VV_INSTINIT:
            sprintf(*buf, "P_FUNC2S_Y_VV_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2S_Y_VV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2S_Y_VV_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_FUNC2S_Y_VV_TEST_D0:
            sprintf(*buf, "P_FUNC2S_Y_VV_TEST_D0\n");
            break;
        case P_FUNC2S_Y_VV_TEST_D1:
            sprintf(*buf, "P_FUNC2S_Y_VV_TEST_D1\n");
            break;
        case P_FUNC2S_Y_VV_D1INT:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1INT\n");
            break;
        case P_FUNC2S_Y_VV_D1NOTINT:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1NOTINT\n");
            break;
        case P_FUNC2S_Y_VV_D1BIGINT:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1BIGINT\n");
            break;
        case P_FUNC2S_Y_VV_D1NOTBIGINT:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1NOTBIGINT\n");
            break;
        case P_FUNC2S_Y_VV_POST_IF:
            sprintf(*buf, "P_FUNC2S_Y_VV_POST_IF\n");
            break;
        case P_FUNC2S_Y_VV_D0NOATOMIC:
            sprintf(*buf, "P_FUNC2S_Y_VV_D0NOATOMIC\n");
            break;
        case P_FUNC2S_Y_VV_EQUALS:
            sprintf(*buf, "P_FUNC2S_Y_VV_EQUALS\n");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_D0NOATOM:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_D0NOATOM\n");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_D0ATOM:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_D0ATOM\n");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_POST_ELSE:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_POST_ELSE\n");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_IFOK_INIT:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_IFOK_INIT\n");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_IFOK_IFOK:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_IFOK_IFOK\n");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_IFOK_NOIF:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_IFOK_NOIF\n");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_INSIDEWHILE:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_INSIDEWHILE\n");
            break;
        case P_FUNC2S_Y_VV_D1GREATER_END:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1GREATER_END\n");
            break;
        case P_FUNC2S_Y_VV_D1ISZERO:
            sprintf(*buf, "P_FUNC2S_Y_VV_D1ISZERO\n");
            break;
        case P_FUNC2S_Y_VV_ELSE:
            sprintf(*buf, "P_FUNC2S_Y_VV_ELSE\n");
            break;
        case P_FUNC2S_Y_VV_END1:
            sprintf(*buf, "P_FUNC2S_Y_VV_END1\n");
            break;
        case P_FUNC2S_Y_VV_END2:
            sprintf(*buf, "P_FUNC2S_Y_VV_END2\n");
            break;
        case P_FUNC2S_Y_CV_INSTINIT:
            sprintf(*buf, "P_FUNC2S_Y_CV_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2S_Y_CV_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2S_Y_CV_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_FUNC2S_Y_CV_TEST_D1:
            sprintf(*buf, "P_FUNC2S_Y_CV_TEST_D1\n");
            break;
        case P_FUNC2S_Y_CV_D1INT:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1INT\n");
            break;
        case P_FUNC2S_Y_CV_D1NOTINT:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1NOTINT\n");
            break;
        case P_FUNC2S_Y_CV_D1BIGINT:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1BIGINT\n");
            break;
        case P_FUNC2S_Y_CV_D1NOTBIGINT:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1NOTBIGINT\n");
            break;
        case P_FUNC2S_Y_CV_POST_IF:
            sprintf(*buf, "P_FUNC2S_Y_CV_POST_IF\n");
            break;
        case P_FUNC2S_Y_CV_EQUALS:
            sprintf(*buf, "P_FUNC2S_Y_CV_EQUALS\n");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_D0NOATOM:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_D0NOATOM\n");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_D0ATOM:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_D0ATOM\n");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_POST_ELSE:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_POST_ELSE\n");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_IFOK_INIT:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_IFOK_INIT\n");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_IFOK_IFOK:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_IFOK_IFOK\n");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_IFOK_NOIF:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_IFOK_NOIF\n");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_INSIDEWHILE:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_INSIDEWHILE\n");
            break;
        case P_FUNC2S_Y_CV_D1GREATER_END:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1GREATER_END\n");
            break;
        case P_FUNC2S_Y_CV_D1ISZERO:
            sprintf(*buf, "P_FUNC2S_Y_CV_D1ISZERO\n");
            break;
        case P_FUNC2S_Y_CV_ELSE:
            sprintf(*buf, "P_FUNC2S_Y_CV_ELSE\n");
            break;
        case P_FUNC2S_Y_CV_END:
            sprintf(*buf, "P_FUNC2S_Y_CV_END\n");
            break;
        case P_FUNC2S_Y_VC_INSTINIT:
            sprintf(*buf, "P_FUNC2S_Y_VC_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2S_Y_VC_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2S_Y_VC_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_FUNC2S_Y_VC_TEST_D0:
            sprintf(*buf, "P_FUNC2S_Y_VC_TEST_D0\n");
            break;
        case P_FUNC2S_Y_VC_FUNC2S_Y_NVAR_VC:
            sprintf(*buf, "P_FUNC2S_Y_VC_FUNC2S_Y_NVAR_VC\n");
            break;
        case P_FUNC2S_Y_VC_D0NOATOMIC:
            sprintf(*buf, "P_FUNC2S_Y_VC_D0NOATOMIC\n");
            break;
        case P_FUNC2S_Y_VC_EQUALS:
            sprintf(*buf, "P_FUNC2S_Y_VC_EQUALS\n");
            break;
        case P_FUNC2S_Y_VC_D1ISZERO:
            sprintf(*buf, "P_FUNC2S_Y_VC_D1ISZERO\n");
            break;
        case P_FUNC2S_Y_VC_D0NOATOM1:
            sprintf(*buf, "P_FUNC2S_Y_VC_D0NOATOM1\n");
            break;
        case P_FUNC2S_Y_VC_D0NOATOM2:
            sprintf(*buf, "P_FUNC2S_Y_VC_D0NOATOM2\n");
            break;
        case P_FUNC2S_Y_VC_D0ATOM:
            sprintf(*buf, "P_FUNC2S_Y_VC_D0ATOM\n");
            break;
        case P_FUNC2S_Y_VC_POST_ELSE:
            sprintf(*buf, "P_FUNC2S_Y_VC_POST_ELSE\n");
            break;
        case P_FUNC2S_Y_VC_IFOK_INIT:
            sprintf(*buf, "P_FUNC2S_Y_VC_IFOK_INIT\n");
            break;
        case P_FUNC2S_Y_VC_IFOK_IFOK:
            sprintf(*buf, "P_FUNC2S_Y_VC_IFOK_IFOK\n");
            break;
        case P_FUNC2S_Y_VC_IFOK_NOIF:
            sprintf(*buf, "P_FUNC2S_Y_VC_IFOK_NOIF\n");
            break;
        case P_FUNC2S_Y_VC_INSIDEWHILE:
            sprintf(*buf, "P_FUNC2S_Y_VC_INSIDEWHILE\n");
            break;
        case P_FUNC2S_Y_VC_END1:
            sprintf(*buf, "P_FUNC2S_Y_VC_END1\n");
            break;
        case P_FUNC2S_Y_VC_END2:
            sprintf(*buf, "P_FUNC2S_Y_VC_END2\n");
            break;
        case P_FUNC2F_XX_INSTINIT:
            sprintf(*buf, "P_FUNC2F_XX_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2F_XX_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2F_XX_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_FUNC2F_XX_TEST_D0:
            sprintf(*buf, "P_FUNC2F_XX_TEST_D0\n");
            break;
        case P_FUNC2F_XX_D0APPL:
            sprintf(*buf, "P_FUNC2F_XX_D0APPL\n");
            break;
        case P_FUNC2F_XX_D0APPL_D1EXTFUNC:
            sprintf(*buf, "P_FUNC2F_XX_D0APPL_D1EXTFUNC\n");
            break;
        case P_FUNC2F_XX_D0APPL_END:
            sprintf(*buf, "P_FUNC2F_XX_D0APPL_END\n");
            break;
        case P_FUNC2F_XX_D0PAIR:
            sprintf(*buf, "P_FUNC2F_XX_D0PAIR\n");
            break;
        case P_FUNC2F_XX_D0NOCOMPOUND:
            sprintf(*buf, "P_FUNC2F_XX_D0NOCOMPOUND\n");
            break;
        case P_FUNC2F_XX_END:
            sprintf(*buf, "P_FUNC2F_XX_END\n");
            break;
        case P_FUNC2F_XY_INSTINIT:
            sprintf(*buf, "P_FUNC2F_XY_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2F_XY_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2F_XY_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_FUNC2F_XY_TEST_D0:
            sprintf(*buf, "P_FUNC2F_XY_TEST_D0\n");
            break;
        case P_FUNC2F_XY_D0APPL:
            sprintf(*buf, "P_FUNC2F_XY_D0APPL\n");
            break;
        case P_FUNC2F_XY_D0APPL_D1EXTFUNC:
            sprintf(*buf, "P_FUNC2F_XY_D0APPL_D1EXTFUNC\n");
            break;
        case P_FUNC2F_XY_D0APPL_END:
            sprintf(*buf, "P_FUNC2F_XY_D0APPL_END\n");
            break;
        case P_FUNC2F_XY_D0PAIR:
            sprintf(*buf, "P_FUNC2F_XY_D0PAIR\n");
            break;
        case P_FUNC2F_XY_D0NOCOMPOUND:
            sprintf(*buf, "P_FUNC2F_XY_D0NOCOMPOUND\n");
            break;
        case P_FUNC2F_XY_END:
            sprintf(*buf, "P_FUNC2F_XY_END\n");
            break;
        case P_FUNC2F_YX_INSTINIT:
            sprintf(*buf, "P_FUNC2F_YX_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2F_YX_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2F_YX_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_FUNC2F_YX_TEST_D0:
            sprintf(*buf, "P_FUNC2F_YX_TEST_D0\n");
            break;
        case P_FUNC2F_YX_D0APPL:
            sprintf(*buf, "P_FUNC2F_YX_D0APPL\n");
            break;
        case P_FUNC2F_YX_D0APPL_D1EXTFUNC:
            sprintf(*buf, "P_FUNC2F_YX_D0APPL_D1EXTFUNC\n");
            break;
        case P_FUNC2F_YX_D0APPL_END:
            sprintf(*buf, "P_FUNC2F_YX_D0APPL_END\n");
            break;
        case P_FUNC2F_YX_D0PAIR:
            sprintf(*buf, "P_FUNC2F_YX_D0PAIR\n");
            break;
        case P_FUNC2F_YX_D0NOCOMPOUND:
            sprintf(*buf, "P_FUNC2F_YX_D0NOCOMPOUND\n");
            break;
        case P_FUNC2F_YX_END:
            sprintf(*buf, "P_FUNC2F_YX_END\n");
            break;
        case P_FUNC2F_YY_INSTINIT:
            sprintf(*buf, "P_FUNC2F_YY_INSTINIT\n");
            break;
#ifdef LOW_LEVEL_TRACER
        case P_FUNC2F_YY_LOW_LEVEL_TRACER:
            sprintf(*buf, "P_FUNC2F_YY_LOW_LEVEL_TRACER\n");
            break;
#endif
        case P_FUNC2F_YY_TEST_D0:
            sprintf(*buf, "P_FUNC2F_YY_TEST_D0\n");
            break;
        case P_FUNC2F_YY_D0APPL:
            sprintf(*buf, "P_FUNC2F_YY_D0APPL\n");
            break;
        case P_FUNC2F_YY_D0APPL_D1EXTFUNC:
            sprintf(*buf, "P_FUNC2F_YY_D0APPL_D1EXTFUNC\n");
            break;
        case P_FUNC2F_YY_D0APPL_END:
            sprintf(*buf, "P_FUNC2F_YY_D0APPL_END\n");
            break;
        case P_FUNC2F_YY_D0PAIR:
            sprintf(*buf, "P_FUNC2F_YY_D0PAIR\n");
            break;
        case P_FUNC2F_YY_D0NOCOMPOUND:
            sprintf(*buf, "P_FUNC2F_YY_D0NOCOMPOUND\n");
            break;
        case P_FUNC2F_YY_END:
            sprintf(*buf, "P_FUNC2F_YY_END\n");
            break;
    }
}
