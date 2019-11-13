
  /* This file, rhstruct.h, was generated automatically by "yap -L misc/buildlocalglobal"
     please do not update, update H/HEAPFIELDS instead */





























#if USE_DL_MALLOC
#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(DLMallocLock);
#endif
#endif
#if USE_DL_MALLOC || (USE_SYSTEM_MALLOC && HAVE_MALLINFO)
#ifndef  HeapUsed
#define  HeapUsed  Yap_givemallinfo()
#endif

#else

#endif




#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(FreeBlocksLock);
  REINIT_LOCK(HeapUsedLock);
  REINIT_LOCK(HeapTopLock);

#endif




#if USE_THREADED_CODE
  OP_RTABLE = OpRTableAdjust(OP_RTABLE);
#endif

  EXECUTE_CPRED_OP_CODE = Yap_opcode(_execute_cpred);
  EXPAND_OP_CODE = Yap_opcode(_expand_index);
  FAIL_OPCODE = Yap_opcode(_op_fail);
  INDEX_OPCODE = Yap_opcode(_index_pred);
  LOCKPRED_OPCODE = Yap_opcode(_lock_pred);
  ORLAST_OPCODE = Yap_opcode(_or_last);
  UNDEF_OPCODE = Yap_opcode(_undef_p);
  RETRY_USERC_OPCODE = Yap_opcode(_retry_userc);
  EXECUTE_CPRED_OPCODE = Yap_opcode(_execute_cpred);





  RestoreInvisibleAtoms();
  RestoreWideAtoms();
  RestoreAtoms();

#include "ratoms.h"
#ifdef EUROTRA
  TermDollarU = AtomTermAdjust(TermDollarU);
#endif

  USER_MODULE = AtomTermAdjust(USER_MODULE);
  IDB_MODULE = AtomTermAdjust(IDB_MODULE);
  ATTRIBUTES_MODULE = AtomTermAdjust(ATTRIBUTES_MODULE);
  CHARSIO_MODULE = AtomTermAdjust(CHARSIO_MODULE);
  CHTYPE_MODULE = AtomTermAdjust(CHTYPE_MODULE);
  TERMS_MODULE = AtomTermAdjust(TERMS_MODULE);
  SYSTEM_MODULE = AtomTermAdjust(SYSTEM_MODULE);
  READUTIL_MODULE = AtomTermAdjust(READUTIL_MODULE);
  HACKS_MODULE = AtomTermAdjust(HACKS_MODULE);
  ARG_MODULE = AtomTermAdjust(ARG_MODULE);
  GLOBALS_MODULE = AtomTermAdjust(GLOBALS_MODULE);
  SWI_MODULE = AtomTermAdjust(SWI_MODULE);
  DBLOAD_MODULE = AtomTermAdjust(DBLOAD_MODULE);
  RANGE_MODULE = AtomTermAdjust(RANGE_MODULE);
  ERROR_MODULE = AtomTermAdjust(ERROR_MODULE);



  CurrentModules = ModEntryPtrAdjust(CurrentModules);




  RestoreHiddenPredicates();




  RestoreFlags(GLOBAL_flagCount);





  RestorePredHash();
#if defined(YAPOR) || defined(THREADS)

#endif

  CreepCode = PtoPredAdjust(CreepCode);
  UndefCode = PtoPredAdjust(UndefCode);
  SpyCode = PtoPredAdjust(SpyCode);
  PredFail = PtoPredAdjust(PredFail);
  PredTrue = PtoPredAdjust(PredTrue);
#ifdef COROUTINING
  WakeUpCode = PtoPredAdjust(WakeUpCode);
#endif
  PredDollarCatch = PtoPredAdjust(PredDollarCatch);
#ifdef YAPOR
  PredGetwork = PtoPredAdjust(PredGetwork);
#endif /* YAPOR */
  PredGoalExpansion = PtoPredAdjust(PredGoalExpansion);
  PredHandleThrow = PtoPredAdjust(PredHandleThrow);
  PredIs = PtoPredAdjust(PredIs);
  PredLogUpdClause = PtoPredAdjust(PredLogUpdClause);
  PredLogUpdClauseErase = PtoPredAdjust(PredLogUpdClauseErase);
  PredLogUpdClause0 = PtoPredAdjust(PredLogUpdClause0);
  PredCall = PtoPredAdjust(PredCall);
  PredMetaCall = PtoPredAdjust(PredMetaCall);
  PredProtectStack = PtoPredAdjust(PredProtectStack);
  PredRecordedWithKey = PtoPredAdjust(PredRecordedWithKey);
  PredRestoreRegs = PtoPredAdjust(PredRestoreRegs);
  PredRestoreRegs1 = PtoPredAdjust(PredRestoreRegs1);
  PredSafeCallCleanup = PtoPredAdjust(PredSafeCallCleanup);
  PredStaticClause = PtoPredAdjust(PredStaticClause);
  PredThrow = PtoPredAdjust(PredThrow);
  PredTraceMetaCall = PtoPredAdjust(PredTraceMetaCall);
  PredCommentHook = PtoPredAdjust(PredCommentHook);
  PredProcedure = PtoPredAdjust(PredProcedure);
  PredUndefinedQuery = PtoPredAdjust(PredUndefinedQuery);

#ifdef LOW_LEVEL_TRACER

#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(Yap_low_level_trace_lock);
#endif
#endif












  DUMMYCODE->opc = Yap_opcode(_op_fail);
  FAILCODE->opc = Yap_opcode(_op_fail);
  NOCODE->opc = Yap_opcode(_Nstop);
  RestoreEnvInst(ENV_FOR_TRUSTFAIL,&TRUSTFAILCODE,_trust_fail,PredFail);

  RestoreEnvInst(ENV_FOR_YESCODE,&YESCODE,_Ystop,PredFail);

  RestoreEnvInst(ENV_FOR_BORDERCODE,&BORDERCODE,_Ystop,PredFail);

  RestoreOtaplInst(RTRYCODE,_retry_and_mark,PredFail);
#ifdef BEAM
  BEAM_RETRY_CODE->opc = Yap_opcode(_beam_retry_code);
#endif /* BEAM */
#ifdef YAPOR
  RestoreOtaplInst(GETWORK,_getwork,PredGetwork);
  RestoreOtaplInst(GETWORK_SEQ,_getwork_seq,PredGetworkSeq);
  GETWORK_FIRST_TIME->opc = Yap_opcode(_getwork_first_time);
#endif /* YAPOR */
#ifdef TABLING
  RestoreOtaplInst(LOAD_ANSWER,_table_load_answer,PredFail);
  RestoreOtaplInst(TRY_ANSWER,_table_try_answer,PredFail);
  RestoreOtaplInst(ANSWER_RESOLUTION,_table_answer_resolution,PredFail);
  RestoreOtaplInst(COMPLETION,_table_completion,PredFail);
#ifdef THREADS_CONSUMER_SHARING
  RestoreOtaplInst(ANSWER_RESOLUTION_COMPLETION,_table_answer_resolution_completion,PredFail);
#endif /* THREADS_CONSUMER_SHARING */
#endif /* TABLING */




  P_before_spy = PtoOpAdjust(P_before_spy);

  RETRY_C_RECORDEDP_CODE = PtoOpAdjust(RETRY_C_RECORDEDP_CODE);
  RETRY_C_RECORDED_K_CODE = PtoOpAdjust(RETRY_C_RECORDED_K_CODE);










#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(DBTermsListLock);
#endif
  RestoreDBTermsList();


  RestoreExpandList();

#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(ExpandClausesListLock);
  REINIT_LOCK(OpListLock);
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
  REINIT_LOCK(DeadStaticClausesLock);
  REINIT_LOCK(DeadMegaClausesLock);
  REINIT_LOCK(DeadStaticIndicesLock);
#endif
#ifdef COROUTINING




#endif



  OpList = OpListAdjust(OpList);

  RestoreForeignCode();




  RestoreYapRecords();
  RestoreEmptyWakeups();


  RestoreBlobTypes();
  RestoreBlobs();


#if defined(YAPOR) || defined(THREADS)
  REINIT_LOCK(Blobs_Lock);
#endif

