
  /* This file, hstruct.h, was generated automatically by "yap -L misc/buildlocalglobal"
     please do not update, update H/HEAPFIELDS instead */

//
// File defining fields in the Yap_heap_codes global structure
//
// these fields used to spread all over the place, because they must be used in 4 ways:
//  - they must be defined somewhere
//  - they have an #ifdef to get a shorter name
//  - they must be initialised somewhere
//  - they must be restorable and collectable (from the atom gc).
//
//
// The defs include 4+ components:
//   Type
//   name in structured
//   global name
//   init code and restore code (optional)
//
//
//
// MkAT (MkAtomTerm) cvts from a predefined atom to a term
// MkPred constructs a pred_entry
// MkOp gets an opcode
// void does nothing
// =VALUE inits as VALUE
// Init... sets up call to InitFunc
// Restore... sets up call to RestoreFunc
//

/* memory management */
  UInt  Yap_HoleSize_;
#if USE_DL_MALLOC
  struct malloc_state  *Yap_av_;
  struct memory_hole  Yap_MemoryHoles[MAX_DLMALLOC_HOLES]_;
  UInt  Yap_NOfMemoryHoles_;
#if defined(YAPOR) || defined(THREADS)
  lockvar  DLMallocLock_;
#endif
#endif
#if USE_DL_MALLOC || (USE_SYSTEM_MALLOC && HAVE_MALLINFO)
#ifndef  HeapUsed
#define  HeapUsed  Yap_givemallinfo()
#endif
  Int  NotHeapUsed_;
#else
  Int  HeapUsed_;
#endif
  Int  HeapMax_;
  ADDR  HeapTop_;
  ADDR  HeapLim_;
  struct FREEB  *FreeBlocks_;
#if defined(YAPOR) || defined(THREADS)
  lockvar  FreeBlocksLock_;
  lockvar  HeapUsedLock_;
  lockvar  HeapTopLock_;
  int  HeapTopOwner_;
#endif
  UInt  MaxStack_;
  UInt  MaxTrail_;
/* execution info */
/* OPCODE REVERSE TABLE, needed to recover op tables */
#if USE_THREADED_CODE
  op_entry  *OP_RTABLE_;
#endif
/* popular opcodes */
  OPCODE  EXECUTE_CPRED_OP_CODE_;
  OPCODE  EXPAND_OP_CODE_;
  OPCODE  FAIL_OPCODE_;
  OPCODE  INDEX_OPCODE_;
  OPCODE  LOCKPRED_OPCODE_;
  OPCODE  ORLAST_OPCODE_;
  OPCODE  UNDEF_OPCODE_;
  OPCODE  RETRY_USERC_OPCODE_;
  OPCODE  EXECUTE_CPRED_OPCODE_;
/* atom tables */
  UInt  NOfAtoms_;
  UInt  AtomHashTableSize_;
  UInt  WideAtomHashTableSize_;
  UInt  NOfWideAtoms_;
  AtomHashEntry  INVISIBLECHAIN_;
  AtomHashEntry  *WideHashChain_;
  AtomHashEntry  *HashChain_;
/* use atom defs here */
#include "tatoms.h"
#ifdef EUROTRA
  Term  TermDollarU_;
#endif
//modules
  Term  USER_MODULE_;
  Term  IDB_MODULE_;
  Term  ATTRIBUTES_MODULE_;
  Term  CHARSIO_MODULE_;
  Term  CHTYPE_MODULE_;
  Term  TERMS_MODULE_;
  Term  SYSTEM_MODULE_;
  Term  READUTIL_MODULE_;
  Term  HACKS_MODULE_;
  Term  ARG_MODULE_;
  Term  GLOBALS_MODULE_;
  Term  SWI_MODULE_;
  Term  DBLOAD_MODULE_;
  Term  RANGE_MODULE_;
  Term  ERROR_MODULE_;
//
// Module list
//
  struct mod_entry  *CurrentModules_;
// make sure we have the modules set at this point.
// don't actually want to define a field

// hidden predicates
  Prop  HIDDEN_PREDICATES_;
// make sure we have the streams  set at this point.
// don't actually want to define a field

  union flagTerm*  GLOBAL_Flags_;
  UInt  GLOBAL_flagCount_;
/* Anderson's JIT */
  yap_exec_mode  Yap_ExecutionMode_;
/*  The Predicate Hash Table: fast access to predicates. */
  UInt  PredsInHashTable_;
  uint64_t  PredHashTableSize_;
  struct pred_entry  **PredHash_;
#if defined(YAPOR) || defined(THREADS)
  rwlock_t  PredHashRWLock_;
#endif
/* Well-Known Predicates */
  struct pred_entry  *CreepCode_;
  struct pred_entry  *UndefCode_;
  struct pred_entry  *SpyCode_;
  struct pred_entry  *PredFail_;
  struct pred_entry  *PredTrue_;
#ifdef COROUTINING
  struct pred_entry  *WakeUpCode_;
#endif
  struct pred_entry  *PredDollarCatch_;
#ifdef YAPOR
  struct pred_entry  *PredGetwork_;
#endif /* YAPOR */
  struct pred_entry  *PredGoalExpansion_;
  struct pred_entry  *PredHandleThrow_;
  struct pred_entry  *PredIs_;
  struct pred_entry  *PredLogUpdClause_;
  struct pred_entry  *PredLogUpdClauseErase_;
  struct pred_entry  *PredLogUpdClause0_;
  struct pred_entry  *PredCall_;
  struct pred_entry  *PredMetaCall_;
  struct pred_entry  *PredProtectStack_;
  struct pred_entry  *PredRecordedWithKey_;
  struct pred_entry  *PredRestoreRegs_;
  struct pred_entry  *PredRestoreRegs1_;
  struct pred_entry  *PredSafeCallCleanup_;
  struct pred_entry  *PredStaticClause_;
  struct pred_entry  *PredThrow_;
  struct pred_entry  *PredTraceMetaCall_;
  struct pred_entry  *PredCommentHook_;
  struct pred_entry  *PredProcedure_;
  struct pred_entry  *PredUndefinedQuery_;
/* low-level tracer */
#ifdef LOW_LEVEL_TRACER
  int  Yap_do_low_level_trace_;
#if defined(YAPOR) || defined(THREADS)
  lockvar  Yap_low_level_trace_lock_;
#endif
#endif
/* code management info */
  UInt  Yap_ClauseSpace_;
  UInt  Yap_IndexSpace_Tree_;
  UInt  Yap_IndexSpace_EXT_;
  UInt  Yap_IndexSpace_SW_;
  UInt  Yap_LUClauseSpace_;
  UInt  Yap_LUIndexSpace_Tree_;
  UInt  Yap_LUIndexSpace_CP_;
  UInt  Yap_LUIndexSpace_EXT_;
  UInt  Yap_LUIndexSpace_SW_;
/* static code: may be shared by many predicate or may be used for meta-execution */
  yamop  COMMA_CODE_[5];
  yamop  DUMMYCODE_[1];
  yamop  FAILCODE_[1];
  yamop  NOCODE_[1];
  yamop  ENV_FOR_TRUSTFAIL_[2];
  yamop  *TRUSTFAILCODE_;
  yamop  ENV_FOR_YESCODE_[2];
  yamop  *YESCODE_;
  yamop  ENV_FOR_BORDERCODE_[2];
  yamop  *BORDERCODE_;
  yamop  RTRYCODE_[1];
#ifdef BEAM
  yamop  BEAM_RETRY_CODE_[1];
#endif /* BEAM */
#ifdef YAPOR
  yamop  GETWORK_[1];
  yamop  GETWORK_SEQ_[1];
  yamop  GETWORK_FIRST_TIME_[1];
#endif /* YAPOR */
#ifdef TABLING
  yamop  LOAD_ANSWER_[1];
  yamop  TRY_ANSWER_[1];
  yamop  ANSWER_RESOLUTION_[1];
  yamop  COMPLETION_[1];
#ifdef THREADS_CONSUMER_SHARING
  yamop  ANSWER_RESOLUTION_COMPLETION_[1];
#endif /* THREADS_CONSUMER_SHARING */
#endif /* TABLING */
/*  */
/*    PREG just before we enter $spy. We use that to find out the clause which  */
/*    was calling the debugged goal.  */
/*  */
  yamop  *P_before_spy_;
/* support recorded_k  */
  yamop  *RETRY_C_RECORDEDP_CODE_;
  yamop  *RETRY_C_RECORDED_K_CODE_;
/* compiler flags */
  int  PROFILING_;
  int  CALL_COUNTING_;
  int  optimizer_on_;
  int  compile_mode_;
  int  profiling_;
  int  call_counting_;
/********* whether we should try to compile array references ******************/
  int  compile_arrays_;
/* DBTerms: pre-compiled ground terms */
#if defined(YAPOR) || defined(THREADS)
  lockvar  DBTermsListLock_;
#endif
  struct dbterm_list  *DBTermsList_;
/* JITI support */
  yamop  *ExpandClausesFirst_;
  yamop  *ExpandClausesLast_;
  UInt  Yap_ExpandClauses_;
#if defined(YAPOR) || defined(THREADS)
  lockvar  ExpandClausesListLock_;
  lockvar  OpListLock_;
#endif
/* instrumentation */
#ifdef DEBUG
  UInt  Yap_NewCps_;
  UInt  Yap_LiveCps_;
  UInt  Yap_DirtyCps_;
  UInt  Yap_FreedCps_;
#endif
  UInt  Yap_expand_clauses_sz_;
/* UDI support */
  struct udi_info  *UdiControlBlocks_;
/* data-base statistics */
/* system boots in compile mode */
  Int  STATIC_PREDICATES_MARKED_;
/* Internal Database */
  Prop  *INT_KEYS_;
  Prop  *INT_LU_KEYS_;
  Prop  *INT_BB_KEYS_;
/* Internal Database Statistics */
  UInt  INT_KEYS_SIZE_;
  UInt  INT_KEYS_TIMESTAMP_;
  UInt  INT_BB_KEYS_SIZE_;
/* Internal Data-Base Control */
  int  UPDATE_MODE_;
/* nasty IDB stuff */
  struct DB_STRUCT  *DBErasedMarker_;
  struct logic_upd_clause  *LogDBErasedMarker_;
/* Dead clauses and IDB entries */
  struct static_clause  *DeadStaticClauses_;
  struct static_mega_clause  *DeadMegaClauses_;
  struct static_index  *DeadStaticIndices_;
  struct logic_upd_clause  *DBErasedList_;
  struct logic_upd_index  *DBErasedIList_;
#if defined(YAPOR) || defined(THREADS)
  lockvar  DeadStaticClausesLock_;
  lockvar  DeadMegaClausesLock_;
  lockvar  DeadStaticIndicesLock_;
#endif
#ifdef COROUTINING
/* number of attribute modules */
  int  NUM_OF_ATTS_;
/* initialised by memory allocator */
  UInt  Yap_AttsSize_;
#endif
/** opaque terms used to wake up on cut of call catcher meta-goal */
  UInt  setup_call_catcher_cleanup_tag_;
/* Operators */
  struct operator_entry  *OpList_;
/* foreign code loaded */
  struct ForeignLoadItem  *ForeignCodeLoaded_;
  ADDR  ForeignCodeBase_;
  ADDR  ForeignCodeTop_;
  ADDR  ForeignCodeMax_;
/* recorded terms */
  struct record_list  *Yap_Records_;
  Atom  EmptyWakeups_[MAX_EMPTY_WAKEUPS];
  int  MaxEmptyWakeups_;
/* SWI blobs */
  struct _PL_blob_t  *BlobTypes_;
  struct AtomEntryStruct  *Blobs_;
  UInt  NOfBlobs_;
  UInt  NOfBlobsMax_;
#if defined(YAPOR) || defined(THREADS)
  lockvar  Blobs_Lock_;
#endif

