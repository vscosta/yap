
  /* This file, hglobals.h, was generated automatically by "yap -L misc/buildlocalglobal"
     please do not update, update H/GLOBALS instead */

//
// File defining fields in the Yap_GLOBAL global structure
//
// these fields used to spread all over the place, because they must be used in 3 ways:
//  - they must be defined somewhere
//  - they have an #ifdef to get a shorter name
//  - they must be initialised somewhere
//  - they may be of interest to restore
//
// The defs include 3+ components:
//   Type
//   name in structured / global name
//   init code (optional)
//   restore code (optional)
//
// Stuff that must be shared by all threads or workers

// initialization: tell whether the system has been initialised and by whom.
EXTERNAL  int  GLOBAL_Initialised;
EXTERNAL  int  GLOBAL_InitialisedFromPL;
EXTERNAL  int  GLOBAL_PL_Argc;
EXTERNAL  char**  GLOBAL_PL_Argv;
EXTERNAL  bool  GLOBAL_FAST_BOOT_FLAG;
// halt hooks
EXTERNAL    struct halt_hook*  GLOBAL_HaltHooks;
EXTERNAL  fptr_t  GLOBAL_JIT_finalizer;
// stack overflow expansion/gc control
EXTERNAL  int  GLOBAL_AllowLocalExpansion;
EXTERNAL  int  GLOBAL_AllowGlobalExpansion;
EXTERNAL  int  GLOBAL_AllowTrailExpansion;
EXTERNAL  UInt  GLOBAL_SizeOfOverflow;
// amount of space recovered in all garbage collections
EXTERNAL  UInt  GLOBAL_AGcThreshold;
EXTERNAL  Agc_hook  GLOBAL_AGCHook;
/* multi-thread support */
#if THREADS
/* number of threads and processes in system */
EXTERNAL  UInt  GLOBAL_NOfThreads;
/* number of threads created since start */
EXTERNAL  UInt  GLOBAL_NOfThreadsCreated;
/* total run time for dead threads */
EXTERNAL  UInt  GLOBAL_ThreadsTotalTime;
// Threads Array
EXTERNAL  lockvar  GLOBAL_ThreadHandlesLock;
#endif
#if defined(YAPOR) || defined(THREADS)
// protect long critical regions
EXTERNAL  lockvar  GLOBAL_BGL;
#endif
#if defined(YAPOR) || defined(TABLING)
EXTERNAL    struct global_optyap_data  GLOBAL_optyap_data;
#endif /* YAPOR || TABLING */
// whether Yap is responsible for signal handling
EXTERNAL  int  GLOBAL_PrologShouldHandleInterrupts;
/* This is the guy who actually started the system, and who has the correct registers */
#if defined(THREADS)
EXTERNAL  pthread_t  GLOBAL_master_thread;
EXTERNAL    struct thread_mbox*  GLOBAL_named_mboxes;
EXTERNAL  lockvar  GLOBAL_mboxq_lock;
EXTERNAL  UInt  GLOBAL_mbox_count;
EXTERNAL    struct swi_mutex*  GLOBAL_WithMutex;
#endif /* THREADS */
// streams
EXTERNAL    struct stream_desc*  GLOBAL_Stream;
#if defined(THREADS)||defined(YAPOR)
EXTERNAL  lockvar  GLOBAL_StreamDescLock;
#endif
// access to yap initial arguments
EXTERNAL  char**  GLOBAL_argv;
EXTERNAL  int  GLOBAL_argc;
// extensions to Terms
#ifdef COROUTINING
/* array with the ops for your favourite extensions */
EXTERNAL  ext_op  GLOBAL_attas[attvars_ext+1];
#endif
// agc.c
EXTERNAL  int  GLOBAL_agc_calls;
EXTERNAL  YAP_ULONG_LONG  GLOBAL_agc_collected;
/* total time spent in GC */
EXTERNAL  Int  GLOBAL_tot_agc_time;
/* number of heap objects in all garbage collections */
EXTERNAL  Int  GLOBAL_tot_agc_recovered;
//arrays.c
#if HAVE_MMAP
EXTERNAL    struct MMAP_ARRAY_BLOCK*  GLOBAL_mmap_arrays;
#endif
#ifdef DEBUG
//computils.c
EXTERNAL  char  GLOBAL_Option[20];
EXTERNAL  YP_FILE*  GLOBAL_logfile;
//init.c
//int  				output_msg 				=FALSE
#endif
#if defined(COFF)  || defined(A_OUT)
// loada_coff.c && load_aout.c
EXTERNAL  char  GLOBAL_Executable[YAP_FILENAME_MAX];
#endif
EXTERNAL  int  GLOBAL_OpaqueHandlersCount;
EXTERNAL    struct YAP_opaque_handler_struct*  GLOBAL_OpaqueHandlers;
#if  __simplescalar__
EXTERNAL  char  GLOBAL_pwd[YAP_FILENAME_MAX];
#endif
//udi.c
//struct udi_control_block 	RtreeCmd				void
EXTERNAL    const char*  GLOBAL_RestoreFile;
//gprof.c
EXTERNAL  Int  GLOBAL_ProfCalls;
EXTERNAL  Int  GLOBAL_ProfGCs;
EXTERNAL  Int  GLOBAL_ProfHGrows;
EXTERNAL  Int  GLOBAL_ProfSGrows;
EXTERNAL  Int  GLOBAL_ProfMallocs;
EXTERNAL  Int  GLOBAL_ProfIndexing;
EXTERNAL  Int  GLOBAL_ProfOn;
EXTERNAL  Int  GLOBAL_ProfOns;
EXTERNAL    struct RB_red_blk_node*  GLOBAL_ProfilerRoot;
EXTERNAL    struct RB_red_blk_node*  GLOBAL_ProfilerNil;
EXTERNAL  char*  GLOBAL_DIRNAME;
#if LOW_PROF
EXTERNAL  int  GLOBAL_ProfilerOn;
EXTERNAL  FILE*  GLOBAL_FProf;
EXTERNAL  FILE*  GLOBAL_FPreds;
#endif /* LOW_PROF */
// Mutexes
#if THREADS
EXTERNAL    struct swi_mutex*  GLOBAL_FreeMutexes;
EXTERNAL    struct swi_mutex*  GLOBAL_mutex_backbone;
EXTERNAL  lockvar  GLOBAL_MUT_ACCESS;
#endif
EXTERNAL  char*  GLOBAL_Home;
/* ISO char conversion: I will make no comments */
EXTERNAL  char*  GLOBAL_CharConversionTable;
EXTERNAL  char*  GLOBAL_CharConversionTable2;
/* max priority */
EXTERNAL  int  GLOBAL_MaxPriority;
/// alias table access
EXTERNAL    struct AliasDescS*  GLOBAL_FileAliases;
EXTERNAL  int  GLOBAL_NOfFileAliases;
EXTERNAL  int  GLOBAL_SzOfFileAliases;
EXTERNAL    struct vfs*  GLOBAL_VFS;
EXTERNAL  char*  GLOBAL_cwd;

