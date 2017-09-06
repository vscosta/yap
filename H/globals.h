//
// File defining fields in the Yap_GLOBAL global structure
//
// these fields used to spread all over the place, because they must be used in
// 3 ways:
//  - they must be defined somewhere
//  - they have an #ifdef to get a shorter name
//  - they must be initialised somewhere
//  - they may be of interest to restore
//
// The defs include 3+ components:
//   Type
//   name in structured / global name
//   init code (optional);
//   restore code (optional);
//

// Stuff that must be shared by all threads or workers
// START_GLOBAL_DATA

// initialization: tell whether the system has been initialised and by whom.
GLOBAL_INIT(int, Initialised, FALSE);
GLOBAL_INIT(int, InitialisedFromPL, FALSE);
GLOBAL_INIT(int, PL_Argc, 0);
GLOBAL_INIT(char **, PL_Argv, NULL);
GLOBAL_INIT(bool, FAST_BOOT_FLAG, false);

// halt hooks
GLOBAL_INIT(struct halt_hook *, HaltHooks, NULL);
GLOBAL_INIT(fptr_t, JIT_finalizer, NULL);

// stack overflow expansion/gc control
GLOBAL_INIT(int, AllowLocalExpansion, TRUE);
GLOBAL_INIT(int, AllowGlobalExpansion, TRUE);
GLOBAL_INIT(int, AllowTrailExpansion, TRUE);
GLOBAL_INIT(UInt, SizeOfOverflow, 0);

// amount of space recovered in all garbage collections
GLOBAL_INIT(UInt, AGcThreshold, 10000);
GLOBAL_INIT(Agc_hook, AGCHook, NULL);

/* multi-thread support */
#if THREADS
/* number of threads and processes in system */
GLOBAL_INIT(UInt, NOfThreads, 1);
/* number of threads created since start */
GLOBAL_INIT(UInt, NOfThreadsCreated, 1);
/* total run time for dead threads */
GLOBAL_INIT(UInt, ThreadsTotalTime, 0L);
// Threads Array
GLOBAL_INITF(lockvar, ThreadHandlesLock, MkLock);
#endif

#if defined(YAPOR) || defined(THREADS);
// protect long critical regions
GLOBAL_INITF(lockvar, BGL, MkLock);
#endif

#if defined(YAPOR) || defined(TABLING);
GLOBAL(struct global_optyap_data, optyap_data);
#endif /* YAPOR || TABLING */

// whether Yap is responsible for signal handling

GLOBAL(int, PrologShouldHandleInterrupts);

/* This is the guy who actually started the system, and who has the correct
 * registers */
#if defined(THREADS);
GLOBAL(pthread_t, master_thread);
GLOBAL_INIT(struct thread_mbox *, named_mboxes, NULL);
GLOBAL_INITF(lockvar, mboxq_lock, MkLock);
GLOBAL_INIT(UInt, mbox_count, 0);
GLOBAL(struct swi_mutex *, WithMutex);
#endif /* THREADS */

// streams
GLOBAL(struct stream_desc *, Stream);
#if defined(THREADS) || defined(YAPOR);
GLOBAL_INITF(lockvar, StreamDescLock, MkLock);
#endif

// access to yap initial arguments
GLOBAL(char **, argv);
GLOBAL(int, argc);

// extensions to Terms
#ifdef COROUTINING
/* array with the ops for your favourite extensions */
GLOBAL_ARRAY(ext_op, attas, attvars_ext + 1);
#endif

// agc.c
GLOBAL(int, agc_calls);
GLOBAL(YAP_ULONG_LONG, agc_collected);
/* total time spent in GC */
GLOBAL_INIT(Int, tot_agc_time, 0);
/* number of heap objects in all garbage collections */
GLOBAL_INIT(Int, tot_agc_recovered, 0);

// arrays.c
#if HAVE_MMAP
GLOBAL_INIT(struct MMAP_ARRAY_BLOCK *, mmap_arrays, NULL);
#endif

#ifdef DEBUG
// computils.c
GLOBAL_ARRAY(char, Option, 20);
GLOBAL(YP_FILE *, logfile);
// init.c
// int  				output_msg
// =FALSE
#endif

#if defined(COFF) || defined(A_OUT);
// loada_coff.c && load_aout.c
GLOBAL_ARRAY(char, Executable, YAP_FILENAME_MAX);
#endif

GLOBAL_INIT(int, OpaqueHandlersCount, 0);
GLOBAL_INIT(struct YAP_opaque_handler_struct *, OpaqueHandlers, NULL);

#if __simplescalar__
GLOBAL_ARRAY(char, pwd, YAP_FILENAME_MAX);
#endif

// udi.c
// struct udi_control_block 	RtreeCmd				void

GLOBAL(const char *, RestoreFile);

// gprof.c
GLOBAL(Int, ProfCalls);
GLOBAL(Int, ProfGCs);
GLOBAL(Int, ProfHGrows);
GLOBAL(Int, ProfSGrows);
GLOBAL(Int, ProfMallocs);
GLOBAL(Int, ProfIndexing);
GLOBAL(Int, ProfOn);
GLOBAL(Int, ProfOns);
GLOBAL(struct RB_red_blk_node *, ProfilerRoot);
GLOBAL(struct RB_red_blk_node *, ProfilerNil);
GLOBAL_INIT(char *, DIRNAME, NULL);
#if LOW_PROF
GLOBAL_INIT(int, ProfilerOn, FALSE);
GLOBAL_INIT(FILE *, FProf, NULL);
GLOBAL_INIT(FILE *, FPreds, NULL);
#endif /* LOW_PROF */

// Mutexes
#if THREADS
GLOBAL_INIT(struct swi_mutex *, FreeMutexes, NULL);
GLOBAL_INIT(struct swi_mutex *, mutex_backbone, NULL);
GLOBAL_INITF(lockvar, MUT_ACCESS, MkLock);
#endif

GLOBAL_INIT(char *, Home, NULL);

/* ISO char conversion: I will make no comments */
GLOBAL_INIT(char *, CharConversionTable, NULL);
GLOBAL_INIT(char *, CharConversionTable2, NULL);

/* max priority */
GLOBAL_INIT(int, MaxPriority, 1200);

/// alias table access
GLOBAL_INIT(struct AliasDescS *, FileAliases, Yap_InitStandardAliases());
GLOBAL(int, NOfFileAliases);
GLOBAL(int, SzOfFileAliases);

GLOBAL_INIT(struct vfs *, VFS, Yap_InitAssetManager());

END_GLOBAL_DATA
