
  /* This file, d0hstruct.h, was generated automatically by "yap -L misc/buildlocalglobal"
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
EXTERNAL  UInt  Yap_HoleSize;
#if USE_DL_MALLOC
EXTERNAL    struct malloc_state  *Yap_av;
EXTERNAL    struct memory_hole  Yap_MemoryHoles[MAX_DLMALLOC_HOLES];
EXTERNAL  UInt  Yap_NOfMemoryHoles;
#if defined(YAPOR) || defined(THREADS)
EXTERNAL  lockvar  DLMallocLock;
#endif
#endif
#if USE_DL_MALLOC || (USE_SYSTEM_MALLOC && HAVE_MALLINFO)
#ifndef  HeapUsed
#define  HeapUsed  Yap_givemallinfo()
#endif
EXTERNAL  Int  NotHeapUsed;
#else
EXTERNAL  Int  HeapUsed;
#endif
EXTERNAL  Int  HeapMax;
EXTERNAL  ADDR  HeapTop;
EXTERNAL  ADDR  HeapLim;
EXTERNAL    struct FREEB  *FreeBlocks;
#if defined(YAPOR) || defined(THREADS)
EXTERNAL  lockvar  FreeBlocksLock;
EXTERNAL  lockvar  HeapUsedLock;
EXTERNAL  lockvar  HeapTopLock;
EXTERNAL  int  HeapTopOwner;
#endif
EXTERNAL  UInt  MaxStack;
EXTERNAL  UInt  MaxTrail;
/* execution info */
/* OPCODE REVERSE TABLE, needed to recover op tables */
#if USE_THREADED_CODE
EXTERNAL  op_entry  *OP_RTABLE;
#endif
/* popular opcodes */
EXTERNAL  OPCODE  EXECUTE_CPRED_OP_CODE;
EXTERNAL  OPCODE  EXPAND_OP_CODE;
EXTERNAL  OPCODE  FAIL_OPCODE;
EXTERNAL  OPCODE  INDEX_OPCODE;
EXTERNAL  OPCODE  LOCKPRED_OPCODE;
EXTERNAL  OPCODE  ORLAST_OPCODE;
EXTERNAL  OPCODE  UNDEF_OPCODE;
EXTERNAL  OPCODE  RETRY_USERC_OPCODE;
EXTERNAL  OPCODE  EXECUTE_CPRED_OPCODE;
/* atom tables */
EXTERNAL  UInt  NOfAtoms;
EXTERNAL  UInt  AtomHashTableSize;
EXTERNAL  UInt  WideAtomHashTableSize;
EXTERNAL  UInt  NOfWideAtoms;
EXTERNAL  AtomHashEntry  INVISIBLECHAIN;
EXTERNAL  AtomHashEntry  *WideHashChain;
EXTERNAL  AtomHashEntry  *HashChain;
/* use atom defs here */
#include "tatoms.h"
#ifdef EUROTRA
EXTERNAL  Term  TermDollarU;
#endif
//modules
EXTERNAL  Term  USER_MODULE;
EXTERNAL  Term  IDB_MODULE;
EXTERNAL  Term  ATTRIBUTES_MODULE;
EXTERNAL  Term  CHARSIO_MODULE;
EXTERNAL  Term  CHTYPE_MODULE;
EXTERNAL  Term  TERMS_MODULE;
EXTERNAL  Term  SYSTEM_MODULE;
EXTERNAL  Term  READUTIL_MODULE;
EXTERNAL  Term  HACKS_MODULE;
EXTERNAL  Term  ARG_MODULE;
EXTERNAL  Term  GLOBALS_MODULE;
EXTERNAL  Term  SWI_MODULE;
EXTERNAL  Term  DBLOAD_MODULE;
EXTERNAL  Term  RANGE_MODULE;
EXTERNAL  Term  ERROR_MODULE;
//
// Module list
//
EXTERNAL    struct mod_entry  *CurrentModules;
// make sure we have the modules set at this point.
// don't actually want to define a field

// hidden predicates
EXTERNAL  Prop  HIDDEN_PREDICATES;
// make sure we have the streams  set at this point.
// don't actually want to define a field

EXTERNAL    union flagTerm*  GLOBAL_Flags;
EXTERNAL  UInt  GLOBAL_flagCount;
/* Anderson's JIT */
EXTERNAL  yap_exec_mode  Yap_ExecutionMode;
/*  The Predicate Hash Table: fast access to predicates. */
EXTERNAL  UInt  PredsInHashTable;
EXTERNAL  uint64_t  PredHashTableSize;
EXTERNAL    struct pred_entry  **PredHash;
#if defined(YAPOR) || defined(THREADS)
EXTERNAL  rwlock_t  PredHashRWLock;
#endif
/* Well-Known Predicates */
EXTERNAL    struct pred_entry  *CreepCode;
EXTERNAL    struct pred_entry  *UndefCode;
EXTERNAL    struct pred_entry  *SpyCode;
EXTERNAL    struct pred_entry  *PredFail;
EXTERNAL    struct pred_entry  *PredTrue;
#ifdef COROUTINING
EXTERNAL    struct pred_entry  *WakeUpCode;
#endif
EXTERNAL    struct pred_entry  *PredDollarCatch;
#ifdef YAPOR
EXTERNAL    struct pred_entry  *PredGetwork;
#endif /* YAPOR */
EXTERNAL    struct pred_entry  *PredGoalExpansion;
EXTERNAL    struct pred_entry  *PredHandleThrow;
EXTERNAL    struct pred_entry  *PredIs;
EXTERNAL    struct pred_entry  *PredLogUpdClause;
EXTERNAL    struct pred_entry  *PredLogUpdClauseErase;
EXTERNAL    struct pred_entry  *PredLogUpdClause0;
EXTERNAL    struct pred_entry  *PredCall;
EXTERNAL    struct pred_entry  *PredMetaCall;
EXTERNAL    struct pred_entry  *PredProtectStack;
EXTERNAL    struct pred_entry  *PredRecordedWithKey;
EXTERNAL    struct pred_entry  *PredRestoreRegs;
EXTERNAL    struct pred_entry  *PredRestoreRegs1;
EXTERNAL    struct pred_entry  *PredSafeCallCleanup;
EXTERNAL    struct pred_entry  *PredStaticClause;
EXTERNAL    struct pred_entry  *PredThrow;
EXTERNAL    struct pred_entry  *PredTraceMetaCall;
EXTERNAL    struct pred_entry  *PredCommentHook;
EXTERNAL    struct pred_entry  *PredProcedure;
EXTERNAL    struct pred_entry  *PredUndefinedQuery;
/* low-level tracer */
#ifdef LOW_LEVEL_TRACER
EXTERNAL  int  Yap_do_low_level_trace;
#if defined(YAPOR) || defined(THREADS)
EXTERNAL  lockvar  Yap_low_level_trace_lock;
#endif
#endif
/* code management info */
EXTERNAL  UInt  Yap_ClauseSpace;
EXTERNAL  UInt  Yap_IndexSpace_Tree;
EXTERNAL  UInt  Yap_IndexSpace_EXT;
EXTERNAL  UInt  Yap_IndexSpace_SW;
EXTERNAL  UInt  Yap_LUClauseSpace;
EXTERNAL  UInt  Yap_LUIndexSpace_Tree;
EXTERNAL  UInt  Yap_LUIndexSpace_CP;
EXTERNAL  UInt  Yap_LUIndexSpace_EXT;
EXTERNAL  UInt  Yap_LUIndexSpace_SW;
/* static code: may be shared by many predicate or may be used for meta-execution */
EXTERNAL  yamop  COMMA_CODE[5];
EXTERNAL  yamop  DUMMYCODE[1];
EXTERNAL  yamop  FAILCODE[1];
EXTERNAL  yamop  NOCODE[1];
EXTERNAL  yamop  ENV_FOR_TRUSTFAIL[2];
EXTERNAL  yamop  *TRUSTFAILCODE;
EXTERNAL  yamop  ENV_FOR_YESCODE[2];
EXTERNAL  yamop  *YESCODE;
EXTERNAL  yamop  ENV_FOR_BORDERCODE[2];
EXTERNAL  yamop  *BORDERCODE;
EXTERNAL  yamop  RTRYCODE[1];
#ifdef BEAM
EXTERNAL  yamop  BEAM_RETRY_CODE[1];
#endif /* BEAM */
#ifdef YAPOR
EXTERNAL  yamop  GETWORK[1];
EXTERNAL  yamop  GETWORK_SEQ[1];
EXTERNAL  yamop  GETWORK_FIRST_TIME[1];
#endif /* YAPOR */
#ifdef TABLING
EXTERNAL  yamop  LOAD_ANSWER[1];
EXTERNAL  yamop  TRY_ANSWER[1];
EXTERNAL  yamop  ANSWER_RESOLUTION[1];
EXTERNAL  yamop  COMPLETION[1];
#ifdef THREADS_CONSUMER_SHARING
EXTERNAL  yamop  ANSWER_RESOLUTION_COMPLETION[1];
#endif /* THREADS_CONSUMER_SHARING */
#endif /* TABLING */
/*  */
/*    PREG just before we enter $spy. We use that to find out the clause which  */
/*    was calling the debugged goal.  */
/*  */
EXTERNAL  yamop  *P_before_spy;
/* support recorded_k  */
EXTERNAL  yamop  *RETRY_C_RECORDEDP_CODE;
EXTERNAL  yamop  *RETRY_C_RECORDED_K_CODE;
/* compiler flags */
EXTERNAL  int  PROFILING;
EXTERNAL  int  CALL_COUNTING;
EXTERNAL  int  optimizer_on;
EXTERNAL  int  compile_mode;
EXTERNAL  int  profiling;
EXTERNAL  int  call_counting;
/********* whether we should try to compile array references ******************/
EXTERNAL  int  compile_arrays;
/* DBTerms: pre-compiled ground terms */
#if defined(YAPOR) || defined(THREADS)
EXTERNAL  lockvar  DBTermsListLock;
#endif
EXTERNAL    struct dbterm_list  *DBTermsList;
/* JITI support */
EXTERNAL  yamop  *ExpandClausesFirst;
EXTERNAL  yamop  *ExpandClausesLast;
EXTERNAL  UInt  Yap_ExpandClauses;
#if defined(YAPOR) || defined(THREADS)
EXTERNAL  lockvar  ExpandClausesListLock;
EXTERNAL  lockvar  OpListLock;
#endif
/* instrumentation */
#ifdef DEBUG
EXTERNAL  UInt  Yap_NewCps;
EXTERNAL  UInt  Yap_LiveCps;
EXTERNAL  UInt  Yap_DirtyCps;
EXTERNAL  UInt  Yap_FreedCps;
#endif
EXTERNAL  UInt  Yap_expand_clauses_sz;
/* UDI support */
EXTERNAL    struct udi_info  *UdiControlBlocks;
/* data-base statistics */
/* system boots in compile mode */
EXTERNAL  Int  STATIC_PREDICATES_MARKED;
/* Internal Database */
EXTERNAL  Prop  *INT_KEYS;
EXTERNAL  Prop  *INT_LU_KEYS;
EXTERNAL  Prop  *INT_BB_KEYS;
/* Internal Database Statistics */
EXTERNAL  UInt  INT_KEYS_SIZE;
EXTERNAL  UInt  INT_KEYS_TIMESTAMP;
EXTERNAL  UInt  INT_BB_KEYS_SIZE;
/* Internal Data-Base Control */
EXTERNAL  int  UPDATE_MODE;
/* nasty IDB stuff */
EXTERNAL    struct DB_STRUCT  *DBErasedMarker;
EXTERNAL    struct logic_upd_clause  *LogDBErasedMarker;
/* Dead clauses and IDB entries */
EXTERNAL    struct static_clause  *DeadStaticClauses;
EXTERNAL    struct static_mega_clause  *DeadMegaClauses;
EXTERNAL    struct static_index  *DeadStaticIndices;
EXTERNAL    struct logic_upd_clause  *DBErasedList;
EXTERNAL    struct logic_upd_index  *DBErasedIList;
#if defined(YAPOR) || defined(THREADS)
EXTERNAL  lockvar  DeadStaticClausesLock;
EXTERNAL  lockvar  DeadMegaClausesLock;
EXTERNAL  lockvar  DeadStaticIndicesLock;
#endif
#ifdef COROUTINING
/* number of attribute modules */
EXTERNAL  int  NUM_OF_ATTS;
/* initialised by memory allocator */
EXTERNAL  UInt  Yap_AttsSize;
#endif
/** opaque terms used to wake up on cut of call catcher meta-goal */
EXTERNAL  UInt  setup_call_catcher_cleanup_tag;
/* Operators */
EXTERNAL    struct operator_entry  *OpList;
/* foreign code loaded */
EXTERNAL    struct ForeignLoadItem  *ForeignCodeLoaded;
EXTERNAL  ADDR  ForeignCodeBase;
EXTERNAL  ADDR  ForeignCodeTop;
EXTERNAL  ADDR  ForeignCodeMax;
/* recorded terms */
EXTERNAL    struct record_list  *Yap_Records;
EXTERNAL  Atom  EmptyWakeups[MAX_EMPTY_WAKEUPS];
EXTERNAL  int  MaxEmptyWakeups;
/* SWI blobs */
EXTERNAL    struct _PL_blob_t  *BlobTypes;
EXTERNAL    struct AtomEntryStruct  *Blobs;
EXTERNAL  UInt  NOfBlobs;
EXTERNAL  UInt  NOfBlobsMax;
#if defined(YAPOR) || defined(THREADS)
EXTERNAL  lockvar  Blobs_Lock;
#endif

