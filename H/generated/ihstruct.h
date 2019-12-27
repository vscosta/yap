
  /* This file, ihstruct.h, was generated automatically by "yap -L misc/buildlocalglobal"
     please do not update, update H/HEAPFIELDS instead */





























#if USE_DL_MALLOC
#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(DLMallocLock);
#endif
#endif
#if USE_DL_MALLOC || (USE_SYSTEM_MALLOC && HAVE_MALLINFO)
#ifndef  HeapUsed
#define  HeapUsed  Yap_givemallinfo()
#endif

#else

#endif




#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(FreeBlocksLock);
  INIT_LOCK(HeapUsedLock);
  INIT_LOCK(HeapTopLock);
  HeapTopOwner = -1;
#endif
  MaxStack = 0;
  MaxTrail = 0;


#if USE_THREADED_CODE

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





  InitInvisibleAtoms();
  InitWideAtoms();
  InitAtoms();

#include "iatoms.h"
#ifdef EUROTRA
  TermDollarU = MkAtomTerm(AtomDollarU);
#endif

  USER_MODULE = MkAtomTerm(AtomUser);
  IDB_MODULE = MkAtomTerm(AtomIDB);
  ATTRIBUTES_MODULE = MkAtomTerm(AtomAttributes);
  CHARSIO_MODULE = MkAtomTerm(AtomCharsio);
  CHTYPE_MODULE = MkAtomTerm(AtomChType);
  TERMS_MODULE = MkAtomTerm(AtomTerms);
  SYSTEM_MODULE = MkAtomTerm(AtomSystem);
  READUTIL_MODULE = MkAtomTerm(AtomReadutil);
  HACKS_MODULE = MkAtomTerm(AtomYapHacks);
  ARG_MODULE = MkAtomTerm(AtomArg);
  GLOBALS_MODULE = MkAtomTerm(AtomNb);
  SWI_MODULE = MkAtomTerm(AtomSwi);
  DBLOAD_MODULE = MkAtomTerm(AtomDBLoad);
  RANGE_MODULE = MkAtomTerm(AtomRange);
  ERROR_MODULE = MkAtomTerm(AtomError);



  CurrentModules = NULL;


  Yap_InitModules();

  HIDDEN_PREDICATES = NULL;


  Yap_InitPlIO(yapi);
  GLOBAL_Flags = 0;
  Yap_InitFlags(true);

  Yap_ExecutionMode = INTERPRETED;

  PredsInHashTable = 0;
  PredHashTableSize = 0;
  InitPredHash();
#if defined(YAPOR) || defined(THREADS)

#endif

  CreepCode = RepPredProp(PredPropByFunc(Yap_MkFunctor(AtomCreep,1),PROLOG_MODULE));
  UndefCode = RepPredProp(PredPropByFunc(Yap_MkFunctor(AtomUndefp,2),PROLOG_MODULE));
  SpyCode = RepPredProp(PredPropByFunc(Yap_MkFunctor(AtomSpy,1),PROLOG_MODULE));
  PredFail = RepPredProp(PredPropByAtom(AtomFail,PROLOG_MODULE));
  PredTrue = RepPredProp(PredPropByAtom(AtomTrue,PROLOG_MODULE));
#ifdef COROUTINING
  WakeUpCode = RepPredProp(PredPropByFunc(Yap_MkFunctor(AtomWakeUpGoal,2),PROLOG_MODULE));
#endif
  PredDollarCatch = RepPredProp(PredPropByFunc(FunctorCatch,PROLOG_MODULE));
#ifdef YAPOR
  PredGetwork = RepPredProp(PredPropByAtom(AtomGetwork,PROLOG_MODULE));
#endif /* YAPOR */
  PredGoalExpansion = RepPredProp(PredPropByFunc(FunctorGoalExpansion,USER_MODULE));
  PredHandleThrow = RepPredProp(PredPropByFunc(FunctorHandleThrow,PROLOG_MODULE));
  PredIs = RepPredProp(PredPropByFunc(FunctorIs,PROLOG_MODULE));
  PredLogUpdClause = RepPredProp(PredPropByFunc(FunctorDoLogUpdClause,PROLOG_MODULE));
  PredLogUpdClauseErase = RepPredProp(PredPropByFunc(FunctorDoLogUpdClauseErase,PROLOG_MODULE));
  PredLogUpdClause0 = RepPredProp(PredPropByFunc(FunctorDoLogUpdClause,PROLOG_MODULE));
  PredCall = RepPredProp(PredPropByFunc(FunctorCall,PROLOG_MODULE));
  PredMetaCall = RepPredProp(PredPropByFunc(FunctorMetaCall,PROLOG_MODULE));
  PredProtectStack = RepPredProp(PredPropByFunc(FunctorProtectStack,PROLOG_MODULE));
  PredRecordedWithKey = RepPredProp(PredPropByFunc(FunctorRecordedWithKey,PROLOG_MODULE));
  PredRestoreRegs = RepPredProp(PredPropByFunc(FunctorRestoreRegs,PROLOG_MODULE));
PredRestoreRegs1 = RepPredProp(PredPropByFunc(FunctorRestoreRegs1,PROLOG_MODULE));
  PredSafeCallCleanup = RepPredProp(PredPropByFunc(FunctorSafeCallCleanup,PROLOG_MODULE));
  PredStaticClause = RepPredProp(PredPropByFunc(FunctorDoStaticClause,PROLOG_MODULE));
  PredThrow = RepPredProp(PredPropByFunc(FunctorThrow,PROLOG_MODULE));
  PredTraceMetaCall = RepPredProp(PredPropByFunc(FunctorTraceMetaCall,PROLOG_MODULE));
  PredCommentHook = RepPredProp(PredPropByFunc(FunctorCommentHook,PROLOG_MODULE));
  PredProcedure = Yap_MkLogPred(RepPredProp(PredPropByFunc(FunctorProcedure,PROLOG_MODULE)));
  PredUndefinedQuery = RepPredProp(PredPropByFunc(FunctorUndefinedQuery,PROLOG_MODULE));

#ifdef LOW_LEVEL_TRACER
  Yap_do_low_level_trace = FALSE;
#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(Yap_low_level_trace_lock);
#endif
#endif

  Yap_ClauseSpace = 0;
  Yap_IndexSpace_Tree = 0;
  Yap_IndexSpace_EXT = 0;
  Yap_IndexSpace_SW = 0;
  Yap_LUClauseSpace = 0;
  Yap_LUIndexSpace_Tree = 0;
  Yap_LUIndexSpace_CP = 0;
  Yap_LUIndexSpace_EXT = 0;
  Yap_LUIndexSpace_SW = 0;


  DUMMYCODE->opc = Yap_opcode(_op_fail);
  FAILCODE->opc = Yap_opcode(_op_fail);
  NOCODE->opc = Yap_opcode(_Nstop);
  InitEnvInst(ENV_FOR_TRUSTFAIL,&TRUSTFAILCODE,_trust_fail,PredFail);

  InitEnvInst(ENV_FOR_YESCODE,&YESCODE,_Ystop,PredFail);

  InitEnvInst(ENV_FOR_BORDERCODE,&BORDERCODE,_Ystop,PredFail);

  InitOtaplInst(RTRYCODE,_retry_and_mark,PredFail);
#ifdef BEAM
  BEAM_RETRY_CODE->opc = Yap_opcode(_beam_retry_code);
#endif /* BEAM */
#ifdef YAPOR
  InitOtaplInst(GETWORK,_getwork,PredGetwork);
  InitOtaplInst(GETWORK_SEQ,_getwork_seq,PredGetworkSeq);
  GETWORK_FIRST_TIME->opc = Yap_opcode(_getwork_first_time);
#endif /* YAPOR */
#ifdef TABLING
  InitOtaplInst(LOAD_ANSWER,_table_load_answer,PredFail);
  InitOtaplInst(TRY_ANSWER,_table_try_answer,PredFail);
  InitOtaplInst(ANSWER_RESOLUTION,_table_answer_resolution,PredFail);
  InitOtaplInst(COMPLETION,_table_completion,PredFail);
#ifdef THREADS_CONSUMER_SHARING
  InitOtaplInst(ANSWER_RESOLUTION_COMPLETION,_table_answer_resolution_completion,PredFail);
#endif /* THREADS_CONSUMER_SHARING */
#endif /* TABLING */




  P_before_spy = NULL;

  RETRY_C_RECORDEDP_CODE = NULL;
  RETRY_C_RECORDED_K_CODE = NULL;

  PROFILING = FALSE;
  CALL_COUNTING = FALSE;
  optimizer_on = TRUE;
  compile_mode = 0;
  profiling = FALSE;
  call_counting = FALSE;

  compile_arrays = FALSE;

#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(DBTermsListLock);
#endif
  DBTermsList = NULL;

  ExpandClausesFirst = NULL;
  ExpandClausesLast = NULL;
  Yap_ExpandClauses = 0;
#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(ExpandClausesListLock);
  INIT_LOCK(OpListLock);
#endif

#ifdef DEBUG
  Yap_NewCps = 0L;
  Yap_LiveCps = 0L;
  Yap_DirtyCps = 0L;
  Yap_FreedCps = 0L;
#endif
  Yap_expand_clauses_sz = 0L;

  UdiControlBlocks = NULL;


  STATIC_PREDICATES_MARKED = FALSE;

  INT_KEYS = NULL;
  INT_LU_KEYS = NULL;
  INT_BB_KEYS = NULL;

  INT_KEYS_SIZE = INT_KEYS_DEFAULT_SIZE;
  INT_KEYS_TIMESTAMP = 0L;
  INT_BB_KEYS_SIZE = INT_KEYS_DEFAULT_SIZE;

  UPDATE_MODE = UPDATE_MODE_LOGICAL;

  InitDBErasedMarker();
  InitLogDBErasedMarker();

  DeadStaticClauses = NULL;
  DeadMegaClauses = NULL;
  DeadStaticIndices = NULL;
  DBErasedList = NULL;
  DBErasedIList = NULL;
#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(DeadStaticClausesLock);
  INIT_LOCK(DeadMegaClausesLock);
  INIT_LOCK(DeadStaticIndicesLock);
#endif
#ifdef COROUTINING

  NUM_OF_ATTS = 1;


#endif



  OpList = NULL;

  ForeignCodeLoaded = NULL;
  ForeignCodeBase = NULL;
  ForeignCodeTop = NULL;
  ForeignCodeMax = NULL;

  Yap_Records = NULL;
  InitEmptyWakeups();
  MaxEmptyWakeups = 0;

  BlobTypes = NULL;
  Blobs = NULL;
  NOfBlobs = 0;
  NOfBlobsMax = 256;
#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(Blobs_Lock);
#endif

