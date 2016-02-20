//
// File defining fields in the Yap_heap_codes global structure
//
// these fields used to spread all over the place, because they must be used in
// 4 ways:
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
// MkAT\(MkAtomTerm) cvts from a predefined atom to a term
// MkPred constructs a pr%ed_entry
// MkOp gets an opcode
// void does nothing
// =VALUE inits as VALUE
// Init... sets up call to InitFunc
// Restore... sets up call to RestoreFunc

// HM refers to standard field
// HI refers to field that does not need restore
// H_R refers to field that does not need init, but needs restore
// HSPACE refers to a field without init not recovery
// HMLOCK refers to a lock
// HRWLOCK refers to a rwlock
// HOPCODE refers to a opcode
// HOPCODE refers to a field initialized/restored with a proceeding

/* memory management */
@DEF @ @TYPE @ struct malloc_state * \ @NAME @Yap_av \\ 

#if USE_DL_MALLOC HSPACE(struct memory_hole,
                         Yap_MemoryHoles) HSPACE(UInt, Yap_NOfMemoryHoles)
#if defined(YAPOR) || defined(THREADS)
    HMLOCK(lockvar, DLMallocLock)
#endif
#endif
#if USE_DL_MALLOC || (USE_SYSTEM_MALLOC && HAVE_MALLINFO)
#ifndef HeapUsed
#define HeapUsed Yap_givemallinfo()
#endif
        HSPACE(Int, NotHeapUsed)
#else
        HSPACE(Int, HeapUsed)
#endif
            HSPACE(Int, HeapMax) HSPACE(ADDR, HeapTop) HSPACE(ADDR, HeapLim)
                HSPACE(struct FREEB *, FreeBlocks)
#if defined(YAPOR) || defined(THREADS)
                    HMLOCK(lockvar, FreeBlocksLock) HMLOCK(lockvar,
                                                           HeapUsedLock)
                        HMLOCK(lockvar, HeapTopLock) HI(int, HeapTopOwner, -1)
#endif
                            HI(UInt, MaxStack, 0) HI(UInt, MaxTrail, 0)

/* execution info */
/* OPCODE REVERSE TABLE, needed to recover op tables */
#if USE_THREADED_CODE
                                HM(op_entry, OP_RTABLE, NULL, OpRTableAdjust)
#endif

    /* popular opcodes */
    HMOPCODE(EXECUTE_CPRED_OP_CODE, _execute_cpred)
        HMOPCODE(EXPAND_OP_CODE, _expand_index) HMOPCODE(FAIL_OPCODE, _op_fail)
            HMOPCODE(INDEX_OPCODE, _index_pred)
                HMOPCODE(LOCKPRED_OPCODE, _lock_pred)
                    HMOPCODE(ORLAST_OPCODE, _or_last)
                        HMOPCODE(UNDEF_OPCODE, _undef_p)
                            HMOPCODE(RETRY_USERC_OPCODE, _retry_userc)
                                HMOPCODE(EXECUTE_CPRED_OPCODE, _execute_cpred)

    /* atom tables */
    HSPACE(UInt, NOfAtoms) HSPACE(UInt, AtomHashTableSize)
        HSPACE(UInt, WideAtomHashTableSize) HSPACE(UInt, NOfWideAtoms)
            HPROC(AtomHashEntry, INVISIBLECHAIN, InitInvisibleAtoms(),
                  RestoreInvisibleAtoms()) HPROC(AtomHashEntry *, WideHashChain,
                                                 InitWideAtoms(),
                                                 RestoreWideAtoms())
                HPROC(AtomHashEntry *, HashChain, InitAtoms(), RestoreAtoms())

#if __INIT_C__
/* use atom defs here */
#include "iatoms.h"
#endif

#ifdef EUROTRA
                    HATOMT(TermDollarU, AtomDollarU)
#endif

    // modules
    HATOMT(USER_MODULE, AtomUser) HATOMT(IDB_MODULE, AtomIDB)
        HATOMT(ATTRIBUTES_MODULE, AtomAttributes) HATOMT(CHARSIO_MODULE,
                                                         AtomCharsio)
            HATOMT(CHTYPE_MODULE, AtomChType) HATOMT(TERMS_MODULE, AtomTerms)
                HATOMT(SYSTEM_MODULE, AtomSystem)
                    HATOMT(READUTIL_MODULE, AtomReadutil)
                        HATOMT(HACKS_MODULE, AtomYapHacks)
                            HATOMT(ARG_MODULE, AtomArg)
                                HATOMT(GLOBALS_MODULE, AtomNb)
                                    HATOMT(SWI_MODULE, AtomSwi)
                                        HATOMT(DBLOAD_MODULE, AtomDBLoad)
                                            HATOMT(RANGE_MODULE, AtomRange)
                                                HATOMT(ERROR_MODULE, AtomError)

    //
    // Module list
    //
    HM(struct mod_entry *, CurrentModules, NULL, ModEntryPtrAdjust)

// make sure we have the modules set at this point.
// don't actually want to define a field
#if __INIT_C__
        Yap_InitModules();
#endif

// hidden predicates
HM(Prop, HIDDEN_PREDICATES, NULL, RestoreHiddenPredicates())

// make sure we have the streams  set at this point.
// don't actually want to define a field
#if __INIT_C__
Yap_InitPlIO();
#endif

HSPACE(union flagTerm *, GFlags)
HM(UInt, GLOBAL_flagCount, Yap_InitFlags(true), RestoreFlags(GLOBAL_flagCount))

/* Anderson's JIT */
HM(yap_exec_mode, Yap_ExecutionMode, INTERPRETED, rv_void)
/*  The Predicate Hash Table: fast access to predicates. */
HPROC(struct pred_entry **, PredHash, InitPredHash(), RestorePredHash())
#if defined(YAPOR) || defined(THREADS)
HRWLOCK(rwlock_t, PredHashRWLock)
#endif
HSPACE(UInt, PredsInHashTable)
HSPACE(UInt, PredHashTableSize)

/* Well-Known Predicates */
HAROP(CreepCode, AtomCreep, 1, PROLOG_MODULE)
HAROP(UndefCode, AtomUndefp, 2, PROLOG_MODULE)
HAROP(SpyCode, AtomSpy, 1, PROLOG_MODULE)
HAROP(PredFail, AtomFail, 0, PROLOG_MODULE)
HAROP(PredTrue, AtomTrue, 0, PROLOG_MODULE)
#ifdef COROUTINING
HAROP(WakeUpCode, AtomWakeUpGoal, 2, PROLOG_MODULE)
#endif
HFOP(PredGoalExpansion, FunctorGoalExpansion, USER_MODULE)
HFOP(PredMetaCall, FunctorMetaCall, PROLOG_MODULE)
HFOP(PredTraceMetaCall, FunctorTraceMetaCall, PROLOG_MODULE)
HFOP(PredDollarCatch, FunctorCatch, PROLOG_MODULE)
HFOP(PredRecordedWithKey, FunctorRecordedWithKey, PROLOG_MODULE)
HFOP(PredLogUpdClause, FunctorDoLogUpdClause, PROLOG_MODULE)
HFOP(PredLogUpdClauseErase, FunctorDoLogUpdClauseErase, PROLOG_MODULE)
HFOP(PredLogUpdClause0, FunctorDoLogUpdClause, PROLOG_MODULE)
HFOP(PredStaticClause, FunctorDoStaticClause, PROLOG_MODULE)
HFOP(PredThrow, FunctorThrow, PROLOG_MODULE)
HFOP(PredHandleThrow, FunctorHandleThrow, PROLOG_MODULE)
HFOP(PredIs, FunctorIs, PROLOG_MODULE)
HFOP(PredSafeCallCleanup, FunctorSafeCallCleanup, PROLOG_MODULE)
HFOP(PredRestoreRegs, FunctorRestoreRegs, PROLOG_MODULE)
HFOP(PredCommentHook, FunctorCommentHook, PROLOG_MODULE)
#ifdef YAPOR
HAROP(PredGetwork, AtomGetwork, 0, PROLOG_MODULE)
HFOP(PredProcedure, MkLogPred, FunctorProcedure, PROLOG_MODULE)
#endif /* YAPOR */

/* low-level tracer */
#ifdef LOW_LEVEL_TRACER
HSPACE(bool, Yap_do_low_level_trace)
#if defined(YAPOR) || defined(THREADS)
HMLOCK(Yap_low_level_trace_lock)
#endif
#endif

/* code management info */
HI(UInt, Yap_ClauseSpace, 0)
HI(UInt, Yap_IndexSpace_Tree, 0)
HI(UInt, Yap_IndexSpace_EXT, 0)
HI(UInt, Yap_IndexSpace_SW, 0)
HI(UInt, Yap_LUClauseSpace, 0)
HI(UInt, Yap_LUIndexSpace_Tree, 0)
HI(UInt, Yap_LUIndexSpace_CP, 0)
HI(UInt, Yap_LUIndexSpace_EXT, 0)
HI(UInt, Yap_LUIndexSpace_SW, 0)

/* static code: may be shared by many predicate or may be used for
 * meta-execution */
HYOP(5, COMMA_CODE, _op_fail)
HYOP(1, DUMMYCODE, _op_fail)
HYOP(1, FAILCODE, _op_fail)
HYOP(1, NOCODE, _Nstop)
#ifdef BEAM
HYOP(beam_retry_code, 1, BEAM_RETRY_CODE, _beam_retry_code)
#endif /* BEAM */

HENVYOP(2, ENV_FOR_TRUSTFAIL, _trust_fail, PredFail, TRUSTFAILCODE)
HSPACE(yamop *, TRUSTFAILCODE)

HENVYOP(2, ENV_FOR_YESCODE, _Ystop, PredFail, YESCODE)
HSPACE(yamop *, YESCODE)

HCPYOP(1, RTRYCODE, _retry_and_mark, PredFail)
#ifdef BEAM
HCPYOP(1, BEAM_RETRY_CODE, PredFail)
#endif
#ifdef YAPOR
HCPYOP(1, GETWORK, _getwork, PredGetwork)
HCPYOP(1, GETWORK_SEQ, _getwork_seq, PredGetworkSeq)
HCPYOP(1, GETWORK_FIRST_TIME, _getwork_first_time, PredGetworkFirstTime)
#endif /* YAPOR */
#ifdef TABLING
HCPYOP(1, LOAD_ANSWER, _table_load_answer, PredFail)
HCPYOP(1, TRY_ANSWER, _table_try_answer, PredFail)
HCPYOP(1, ANSWER_RESOLUTION, _table_load_answer, PredFail)
HCPYOP(1, COMPLETION, _table_completion, PredFail)
#ifdef THREADS_CONSUMER_SHARING
HCPYOP(1, ANSWER_RESOLUTION_COMPLETION, _table_answer_resolution_completion,
       PredFail)
#endif /* THREADS_CONSUMER_SHARING */
#endif /* TABLING */

/*  */
/*    PREG just before we enter $spy. We use that to find out the clause which
 */
/*    was calling the debugged goal.  */
/*  */
HM(struct yami *, P_before_spy, NULL, PtoOpAdjust(P_before_spy))

/* support recorded_k  */
HM(struct yami *, RETRY_C_RECORDEDP_CODE, NULL,
   PtoOpAdjust(RETRY_C_RECORDEDP_CODE))

HM(struct yami *, RETRY_C_RECORDED_K_CODE, NULL,
   PtoOpAdjust(RETRY_C_RECORDED__CODE))

/* compiler flags */
HI(bool, PROFILING, false)
HI(bool, CALL_COUNTING, false)
HI(bool, optimizer_on, true)
HI(bool, compile_mode, false)
HI(bool, profiling, false)
HI(bool, call_counting, false)
/********* whether we should try to compile array references ******************/
HI(bool, compile_arrays, false)

/* DBTerms: pre-compiled ground terms */
#if defined(YAPOR) || defined(THREADS)
HMLOCK(lockvar, DBTermsListLock)
#endif
HM(struct dbterm_list *, DBTermsList, NULL, RestoreDBTermsList())

/* JITI support */
HI(yamop, ExpandClausesFirst, NULL)
HM(yamop, ExpandClausesLast, NULL, RestoreExpandList())
HI(UInt, Yap_ExpandClauses, 0)
#if defined(YAPOR) || defined(THREADS)
HMLOCK(lockvar, ExpandClausesListLock)
HMLOCK(lockvar, OpListLock)
#endif
/* instrumentation */
#ifdef DEBUG
HI(UInt, Yap_NewCps, 0L)
HI(UInt, Yap_LiveCps, 0L)
HI(UInt, Yap_DirtyCps, 0L)
HI(UInt, Yap_FreedCps, 0L)
#endif
HI(UInt, Yap_expand_clauses_sz, 0L)

/* UDI support */
H_R(struct udi_info *, UdiControlBlocks, RestoreUdiControlBlocks())

/* data-base statistics */
/* system boots in compile mode */
HI(Int, STATIC_PREDICATES_MARKED, false)

/* Internal Database */
HM(Prop, INT_KEYS, NULL, RestoreIntKeys())
HM(Prop, INT_LU_KEYS, NULL, RestoreIntLUKeys())
HM(Prop, INT_BB_KEYS, NULL, RestoreIntBBKeys())

/* Internal Database Statistics */
HI(UInt, INT_KEYS_SIZE, INT_KEYS_DEFAULT_SIZE)
HI(UInt, INT_KEYS_TIMESTAMP, 0L)
HI(UInt, INT_BB_KEYS_SIZE, INT_KEYS_DEFAULT_SIZE)

/* Internal Data-Base Control */
HI(int, UPDATE_MODE, UPDATE_MODE_LOGICAL)

/* nasty IDB stuff */
HPROC(struct DB_STRUCT *, DBErasedMarker, InitDBErasedMarker(),
      RestoreDBErasedMarker())
HPROC(struct logic_upd_clause *, LogDBErasedMarker, InitLogDBErasedMarker(),
      RestoreLogDBErasedMarker())

/* Dead clauses and IDB entries */
H_R(struct static_clause *, DeadStaticClauses, RestoreDeadStaticClauses())
H_R(struct static_mega_clause *, DeadMegaClauses, RestoreDeadMegaClauses())
H_R(struct static_index *, DeadStaticIndices, RestoreDeadStaticIndices())
H_R(struct logic_upd_clause *, DBErasedList, RestoreDBErasedList())
H_R(struct logic_upd_index *, DBErasedIList, RestoreDBErasedIList())
#if defined(YAPOR) || defined(THREADS)
HMLOCK(lockvar, DeadStaticClausesLock)
HMLOCK(lockvar, DeadMegaClausesLock)
HMLOCK(lockvar, DeadStaticIndicesLock)
#endif

#ifdef COROUTINING
/* number of attribute modules */
HI(int, NUM_OF_ATTS, 1)
/* initialised by memory allocator */
HI(UInt, Yap_AttsSize, 0)
#endif

/* Operators */
HM(struct operator_entry *, OpList, NULL, OpListAdjust)

/* foreign code loaded */
HM(struct ForeignLoadItem *, ForeignCodeLoaded, NULL, RestoreForeignCode())
HI(ADDR, ForeignCodeBase, NULL)
HI(ADDR, ForeignCodeTop, NULL)
HI(ADDR, ForeignCodeMax, NULL)

/* recorded terms */
HM(struct record_list *, Yap_Records, NULL, RestoreYapRecords())

/* SWI atoms and functors */
HPROC(struct atom_entry *, SWI_Atoms, InitSWIAtoms(), RestoreSWIAtoms())
HSPACE(struct functor_entry *, SWI_Functors)

HSPACEN(struct swi_reverse_hash, N_SWI_HASH, SWI_ReverseHash)

/* integer access to atoms */
HSPACE(Int, AtomTranslations)
HSPACE(Int, MaxAtomTranslations)

// initialization: tell whether the system has been initialised and by whom.
HI(int, Initialised, false)
HI(int, InitialisedFromPL, false)
HI(int, PL_Argc, 0)
HI(char **, PL_Argv, NULL)
HI(bool, FAST_BOOT_FLAG, false)

// halt hooks
HI(struct halt_hook *, HaltHooks, NULL)
HI(fptr_t, JIT_finalizer, NULL)

// stack overflow expansion/gc control
HI(int, AllowLocalExpansion, true)
HI(int, AllowGlobalExpansion, true)
HI(int, AllowTrailExpansion, true)
HI(UInt, SizeOfOverflow, 0)

// amount of space recovered in all garbage collections
HI(UInt, AGcThreshold, 10000)
HI(Agc_hook, AGCHook, NULL)

/* integer access to functors */
HSPACE(Int, FunctorTranslations)
HSPACE(Int, MaxFunctorTranslations)

HPROCN(Atom, MAX_EMPTY_WAKEUPS, EmptyWakeups, InitEmptyWakeups(),
       RestoreEmptyWakeups())
HSPACE(int, MaxEmptyWakeups)

/* SWI blobs */
HM(struct YAP_blob_t *, BlobTypes, NULL, RestoreBlobTypes())
HM(struct AtomEntryStruct *, Blobs, NULL, RestoreBlobs())
HI(UInt, NOfBlobs, 0)

HI(UInt, NOfBlobsMax, 256)
#if defined(YAPOR) || defined(THREADS)
HMLOCK(lockvar blobs_lock, Blobs_Lock)
#endif

#if __ANDROID__
// no need to perform initialization, it is done before we start the Prolog
// engine.
HI(struct AAssetManager *, assetManager, NULL)
HI(char *, AssetsWD, NULL)
#endif

/* multi-thread support */
#if THREADS
/* number of threads and processes in system */
HI(UInt, NOfThreads, 1)
/* number of threads created since start */
HI(UInt, NOfThreadsCreated, 1)
/* total run time for dead threads */
HI(UInt, ThreadsTotalTime, 0L)
// Threads Array
HI(lockvar, ThreadHandlesLock, MkLock)
#endif

#if defined(YAPOR) || defined(THREADS)
// protect long critical regions
HI(lockvar, BGL, MkLock)
#endif

#if defined(YAPOR) || defined(TABLING)
HSPACE(struct global_optyap_data, optyap_data)
#endif /* YAPOR || TABLING */

// whether Yap is responsible for signal handling

HSPACE(int, PrologShouldHandleInterrupts)

/* This is the guy who actually started the system, and who has the correct
 * registers */
#if defined(THREADS)
HSPACE(pthread_t, master_thread)
HI(struct thread_mbox *, named_mboxes, NULL)
HI(lockvar, mboxq_lock, MkLock)
HI(UInt, mbox_count, 0)
HSPACE(struct swi_mutex *, WithMutex)
#endif /* THREADS */

// streams
HSPACE(struct stream_desc *, Stream)
#if defined(THREADS) || defined(YAPOR)
HI(lockvar, StreamDescLock MkLock)
#endif

// access to yap initial arguments
HSPACE(char **, argv)
HSPACE(int, argc)

// extensions to Terms
#ifdef COROUTINING
/* array with the ops for your favourite extensions */
HSPACEN(ext_op, attvars_ext + 1, attas)
#endif

// agc.c
HSPACE(int, agc_calls)
HSPACE(YAP_ULONG_LONG, agc_collected)
/* total time spent in GC */
HI(Int, tot_agc_time, 0)
/* number of heap objects in all garbage collections */
HI(Int, tot_agc_recovered, 0)

// arrays.c
#if HAVE_MMAP
HI(struct MMAP_ARRAY_BLOCK *, mmap_arrays, NULL)
#endif

#ifdef DEBUG
// computils.c
HSPACEN(char, 20, Option)
HSPACE(YP_FILE *, logfile)
// init.c
// int  , output_msg , false
#endif

#if defined(COFF) || defined(A_OUT)
// loada_coff.c && load_aout.c
HSPACEN(char, Executable, YAP_FILENAME_MAX)
#endif

HI(int, OpaqueHandlersCount, 0)
HI(struct opaque_handler_struct *, OpaqueHandlers, NULL)

#if __simplescalar__
HSPACEN(char, pwd, YAP_FILENAME_MAX)
#endif

// udi.c
// struct udi_control_block , RtreeCmd, void,

HSPACE(char *, RestoreFile)

// gprof.c
HSPACE(Int, ProfCalls)
HSPACE(Int, ProfGCs)
HSPACE(Int, ProfHGrows)
HSPACE(Int, ProfSGrows)
HSPACE(Int, ProfMallocs)
HSPACE(Int, ProfIndexing)
HSPACE(Int, ProfOn)
HSPACE(Int, ProfOns)
HSPACE(struct RB_red_blk_node *, ProfilerRoot)
HSPACE(struct RB_red_blk_node *, ProfilerNil)
HI(char *, DIRNAME, NULL)
#if LOW_PROF
HI(int, ProfilerOn, false)
HI(FILE *, FProf, NULL)
HI(FILE *, FPreds, NULL)
#endif /* LOW_PROF */

// Mutexes
#if THREADS
HI(struct swi_mutex *, FreeMutexes, NULL)
HI(struct swi_mutex *, mutex_backbone, NULL)
HSPACEN(struct swi_reverse_hash, N_SWI_HASH. SWI_ReverseHash
HI(lockvar, MUT_ACCESS, MkLock)
#endif

HI(char *, Home, NULL)

/* ISO char conversion: I will make no comments */
HI(char *, CharConversionTable, NULL)
HI(char *, CharConversionTable2, NULL)

/* time */
HI(void *, LastWTimePtr, NULL)

/* max priority */
HI(int, MaxPriority, 1200)
