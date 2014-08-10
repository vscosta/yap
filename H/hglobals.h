
  /* This file, hglobals.h, was generated automatically by "yap -L misc/buildlocalglobal"
     please do not update, update misc/GLOBALS instead */

















typedef struct global_data {

  int  Initialised_;
  int  InitialisedFromPL_;
  int  PL_Argc_;
  char**  PL_Argv_;

  struct halt_hook*  HaltHooks_;

  int  AllowLocalExpansion_;
  int  AllowGlobalExpansion_;
  int  AllowTrailExpansion_;
  UInt  SizeOfOverflow_;

  UInt  AGcThreshold_;
  Agc_hook  AGCHook_;
#if __ANDROID__

  struct AAssetManager*  assetManager_;
#endif

#if THREADS

  UInt  NOfThreads_;

  UInt  NOfThreadsCreated_;

  UInt  ThreadsTotalTime_;

  lockvar  ThreadHandlesLock_;
#endif	
#if defined(YAPOR) || defined(THREADS)

  lockvar  BGL_;
#endif
#if defined(YAPOR) || defined(TABLING)
  struct global_optyap_data  optyap_data_;
#endif /* YAPOR || TABLING */

  int  PrologShouldHandleInterrupts_;

#if defined(THREADS)
  pthread_t  master_thread_;
#endif /* THREADS */

  struct io_stream*  stdout_;
  struct io_stream*  stderr_;

  char**  argv_;
  int  argc_;

#ifdef COROUTINING

  ext_op  attas_[attvars_ext+1];
#endif

  int  agc_calls_;
  YAP_ULONG_LONG  agc_collected_;

  Int  tot_agc_time_;

  Int  tot_agc_recovered_;

#if HAVE_MMAP
  struct MMAP_ARRAY_BLOCK*  mmap_arrays_;
#endif
#ifdef DEBUG

  char  Option_[20];
  YP_FILE*  logfile_;


#endif
#if defined(COFF)  || defined(A_OUT)

  char  Executable_[YAP_FILENAME_MAX];
#endif
  int  OpaqueHandlersCount_;
  struct opaque_handler_struct*  OpaqueHandlers_;
#if  __simplescalar__
  char  pwd_[YAP_FILENAME_MAX];
#endif


  char*  RestoreFile_;

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
} w_shared;
