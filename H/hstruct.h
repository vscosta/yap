
  /* This file, hstruct.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/HEAPFIELDS instead */



























typedef struct worker_local {

  UInt  Yap_HoleSize;
  struct malloc_state  *Yap_av;
#if USE_DL_MALLOC
  struct Yap_MemoryHoles[MAX_DLMALLOC_HOLES]  void;
  UInt  Yap_NOfMemoryHoles;
#if defined(YAPOR) || defined(THREADS)
  lockvar  DLMallocLock;
#endif
#endif
#if USE_DL_MALLOC || (USE_SYSTEM_MALLOC && HAVE_MALLINFO)
#ifndef  HeapUsed
#define  HeapUsed  Yap_givemallinfo()
#endif
  Int  NotHeapUsed;
#else
  Int  HeapUsed;
#endif
  Int  HeapMax;
  ADDR  HeapTop;
  ADDR  HeapLim;
  struct FREEB  *FreeBlocks;
#if defined(YAPOR) || defined(THREADS)
  lockvar  FreeBlocksLock;
  lockvar  HeapUsedLock;
  lockvar  HeapTopLock;
  int  HeapTopOwner;
#endif
  UInt  MaxStack;
  UInt  MaxTrail;


#if USE_THREADED_CODE
  op_entry  *OP_RTABLE;
#endif

  OPCODE  EXECUTE_CPRED_OP_CODE;
  OPCODE  EXPAND_OP_CODE;
  OPCODE  FAIL_OPCODE;
  OPCODE  INDEX_OPCODE;
  OPCODE  LOCKPRED_OPCODE;
  OPCODE  ORLAST_OPCODE;
  OPCODE  UNDEF_OPCODE;
  OPCODE  RETRY_USERC_OPCODE;
  OPCODE  EXECUTE_CPRED_OPCODE;

  UInt  NOfAtoms;
  UInt  AtomHashTableSize;
  UInt  WideAtomHashTableSize;
  UInt  NOfWideAtoms;
  AtomHashEntry  INVISIBLECHAIN;
  AtomHashEntry  *WideHashChain;
  AtomHashEntry  *HashChain;

#include "tatoms.h"
#ifdef EUROTRA
  Term  TermDollarU;
#endif

  Term  USER_MODULE;
  Term  IDB_MODULE;
  Term  ATTRIBUTES_MODULE;
  Term  CHARSIO_MODULE;
  Term  CHTYPE_MODULE;
  Term  TERMS_MODULE;
  Term  SYSTEM_MODULE;
  Term  READUTIL_MODULE;
  Term  HACKS_MODULE;
  Term  ARG_MODULE;
  Term  GLOBALS_MODULE;
  Term  SWI_MODULE;
  Term  DBLOAD_MODULE;
  Term  RANGE_MODULE;
  Term  ERROR_MODULE;



  struct mod_entry  *CurrentModules;




  Prop  HIDDEN_PREDICATES;



  union flagTerm*  GLOBAL_Flags;
  UInt  GLOBAL_flagCount;

  yap_exec_mode  Yap_ExecutionMode;

  UInt  PredsInHashTable;
  uint64_t  PredHashTableSize;
  struct pred_entry  **PredHash;
#if defined(YAPOR) || defined(THREADS)
  rwlock_t  PredHashRWLock;
#endif

  struct pred_entry  *CreepCode;
  struct pred_entry  *UndefCode;
  struct pred_entry  *SpyCode;
  struct pred_entry  *PredFail;
  struct pred_entry  *PredTrue;
#ifdef COROUTINING
  struct pred_entry  *WakeUpCode;
#endif
  struct pred_entry  *PredGoalExpansion;
  struct pred_entry  *PredMetaCall;
  struct pred_entry  *PredTraceMetaCall;
  struct pred_entry  *PredDollarCatch;
  struct pred_entry  *PredRecordedWithKey;
  struct pred_entry  *PredLogUpdClause;
  struct pred_entry  *PredLogUpdClauseErase;
  struct pred_entry  *PredLogUpdClause0;
  struct pred_entry  *PredStaticClause;
  struct pred_entry  *PredThrow;
  struct pred_entry  *PredHandleThrow;
  struct pred_entry  *PredIs;
  struct pred_entry  *PredSafeCallCleanup;
  struct pred_entry  *PredRestoreRegs;
  struct pred_entry  *PredCommentHook;
#ifdef YAPOR
  struct pred_entry  *PredGetwork;
#endif /* YAPOR */
  struct pred_entry  *PredProcedure;

#ifdef LOW_LEVEL_TRACER
  int  Yap_do_low_level_trace;
#if defined(YAPOR) || defined(THREADS)
  lockvar  Yap_low_level_trace_lock;
#endif
#endif

  UInt  Yap_ClauseSpace;
  UInt  Yap_IndexSpace_Tree;
  UInt  Yap_IndexSpace_EXT;
  UInt  Yap_IndexSpace_SW;
  UInt  Yap_LUClauseSpace;
  UInt  Yap_LUIndexSpace_Tree;
  UInt  Yap_LUIndexSpace_CP;
  UInt  Yap_LUIndexSpace_EXT;
  UInt  Yap_LUIndexSpace_SW;

  yamop  COMMA_CODE[5];
  yamop  DUMMYCODE[1];
  yamop  FAILCODE[1];
  yamop  NOCODE[1];
  yamop  ENV_FOR_TRUSTFAIL[2];
  yamop  *TRUSTFAILCODE;
  yamop  ENV_FOR_YESCODE[2];
  yamop  *YESCODE;
  yamop  RTRYCODE[1];
#ifdef BEAM
  yamop  BEAM_RETRY_CODE[1];
#endif /* BEAM */
#ifdef YAPOR
  yamop  GETWORK[1];
  yamop  GETWORK_SEQ[1];
  yamop  GETWORK_FIRST_TIME[1];
#endif /* YAPOR */
#ifdef TABLING
  yamop  LOAD_ANSWER[1];
  yamop  TRY_ANSWER[1];
  yamop  ANSWER_RESOLUTION[1];
  yamop  COMPLETION[1];
#ifdef THREADS_CONSUMER_SHARING
  yamop  ANSWER_RESOLUTION_COMPLETION[1];
#endif /* THREADS_CONSUMER_SHARING */
#endif /* TABLING */




  yamop  *P_before_spy;

  yamop  *RETRY_C_RECORDEDP_CODE;
  yamop  *RETRY_C_RECORDED_K_CODE;

  int  PROFILING;
  int  CALL_COUNTING;
  int  optimizer_on;
  int  compile_mode;
  int  profiling;
  int  call_counting;

  int  compile_arrays;

#if defined(YAPOR) || defined(THREADS)
  lockvar  DBTermsListLock;
#endif
  struct dbterm_list  *DBTermsList;

  yamop  *ExpandClausesFirst;
  yamop  *ExpandClausesLast;
  UInt  Yap_ExpandClauses;
#if defined(YAPOR) || defined(THREADS)
  lockvar  ExpandClausesListLock;
  lockvar  OpListLock;
#endif

#ifdef DEBUG
  UInt  Yap_NewCps;
  UInt  Yap_LiveCps;
  UInt  Yap_DirtyCps;
  UInt  Yap_FreedCps;
#endif
  UInt  Yap_expand_clauses_sz;

  struct udi_info  *UdiControlBlocks;


  Int  STATIC_PREDICATES_MARKED;

  Prop  *INT_KEYS;
  Prop  *INT_LU_KEYS;
  Prop  *INT_BB_KEYS;

  UInt  INT_KEYS_SIZE;
  UInt  INT_KEYS_TIMESTAMP;
  UInt  INT_BB_KEYS_SIZE;

  int  UPDATE_MODE;

  struct DB_STRUCT  *DBErasedMarker;
  struct logic_upd_clause  *LogDBErasedMarker;

  struct static_clause  *DeadStaticClauses;
  struct static_mega_clause  *DeadMegaClauses;
  struct static_index  *DeadStaticIndices;
  struct logic_upd_clause  *DBErasedList;
  struct logic_upd_index  *DBErasedIList;
#if defined(YAPOR) || defined(THREADS)
  lockvar  DeadStaticClausesLock;
  lockvar  DeadMegaClausesLock;
  lockvar  DeadStaticIndicesLock;
#endif
#ifdef COROUTINING

  int  NUM_OF_ATTS;

  UInt  Yap_AttsSize;
#endif

  struct operator_entry  *OpList;

  struct ForeignLoadItem  *ForeignCodeLoaded;
  ADDR  ForeignCodeBase;
  ADDR  ForeignCodeTop;
  ADDR  ForeignCodeMax;

  struct record_list  *Yap_Records;

  Atom  *SWI_Atoms;
  Functor  *SWI_Functors;
  swi_rev_hash  SWI_ReverseHash[N_SWI_HASH];

  Int  AtomTranslations;
  Int  MaxAtomTranslations;

  Int  FunctorTranslations;
  Int  MaxFunctorTranslations;
  Atom  EmptyWakeups[MAX_EMPTY_WAKEUPS];
  int  MaxEmptyWakeups;

  struct YAP_blob_t  *BlobTypes;
  struct AtomEntryStruct  *Blobs;
  UInt  NOfBlobs;
  UInt  NOfBlobsMax;
#if defined(YAPOR) || defined(THREADS)
  lockvar  Blobs_Lock;
#endif
} w_local;
