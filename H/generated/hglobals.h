
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
typedef struct global_data {
// initialization: tell whether the system has been initialised and by whom.
  int  Initialised_;
  int  InitialisedFromPL_;
  int  PL_Argc_;
  char**  PL_Argv_;
  bool  FAST_BOOT_FLAG_;
// halt hooks
  struct halt_hook*  HaltHooks_;
  fptr_t  JIT_finalizer_;
// stack overflow expansion/gc control
  int  AllowLocalExpansion_;
  int  AllowGlobalExpansion_;
  int  AllowTrailExpansion_;
  UInt  SizeOfOverflow_;
// amount of space recovered in all garbage collections
  UInt  AGcThreshold_;
  Agc_hook  AGCHook_;
/* multi-thread support */
#if THREADS
/* number of threads and processes in system */
  UInt  NOfThreads_;
/* number of threads created since start */
  UInt  NOfThreadsCreated_;
/* total run time for dead threads */
  UInt  ThreadsTotalTime_;
// Threads Array
  lockvar  ThreadHandlesLock_;
#endif
#if defined(YAPOR) || defined(THREADS)
// protect long critical regions
  lockvar  BGL_;
#endif
#if defined(YAPOR) || defined(TABLING)
  struct global_optyap_data  optyap_data_;
#endif /* YAPOR || TABLING */
// whether Yap is responsible for signal handling
  int  PrologShouldHandleInterrupts_;
/* This is the guy who actually started the system, and who has the correct registers */
#if defined(THREADS)
  pthread_t  master_thread_;
  struct thread_mbox*  named_mboxes_;
  lockvar  mboxq_lock_;
  UInt  mbox_count_;
  struct swi_mutex*  WithMutex_;
#endif /* THREADS */
// streams
  struct stream_desc*  Stream_;
#if defined(THREADS)||defined(YAPOR)
  lockvar  StreamDescLock_;
#endif
// access to yap initial arguments
  char**  argv_;
  int  argc_;
// extensions to Terms
#ifdef COROUTINING
/* array with the ops for your favourite extensions */
  ext_op  attas_[attvars_ext+1];
#endif
// agc.c
  int  agc_calls_;
  YAP_ULONG_LONG  agc_collected_;
/* total time spent in GC */
  Int  tot_agc_time_;
/* number of heap objects in all garbage collections */
  Int  tot_agc_recovered_;
//arrays.c
#if HAVE_MMAP
  struct MMAP_ARRAY_BLOCK*  mmap_arrays_;
#endif
#ifdef DEBUG
//computils.c
  char  Option_[20];
  YP_FILE*  logfile_;
//init.c
//int  				output_msg 				=FALSE
#endif
#if defined(COFF)  || defined(A_OUT)
// loada_coff.c && load_aout.c
  char  Executable_[YAP_FILENAME_MAX];
#endif
  int  OpaqueHandlersCount_;
  struct YAP_opaque_handler_struct*  OpaqueHandlers_;
#if  __simplescalar__
  char  pwd_[YAP_FILENAME_MAX];
#endif
//udi.c
//struct udi_control_block 	RtreeCmd				void
const char*  RestoreFile_;
//gprof.c
  Int  ProfCalls_;
  Int  ProfGCs_;
  Int  ProfHGrows_;
  Int  ProfSGrows_;
  Int  ProfMallocs_;
  Int  ProfIndexing_;
  Int  ProfOn_;
  Int  ProfOns_;
  struct RB_red_blk_node*  ProfilerRoot_;
  struct RB_red_blk_node*  ProfilerNil_;
  char*  DIRNAME_;
#if LOW_PROF
  int  ProfilerOn_;
  FILE*  FProf_;
  FILE*  FPreds_;
#endif /* LOW_PROF */
// Mutexes
#if THREADS
  struct swi_mutex*  FreeMutexes_;
  struct swi_mutex*  mutex_backbone_;
  lockvar  MUT_ACCESS_;
#endif
  char*  Home_;
/* ISO char conversion: I will make no comments */
  char*  CharConversionTable_;
  char*  CharConversionTable2_;
/* max priority */
  int  MaxPriority_;
/// alias table access
  struct AliasDescS*  FileAliases_;
  int  NOfFileAliases_;
  int  SzOfFileAliases_;
  struct vfs*  VFS_;
  char*  cwd_;
} w_shared;
