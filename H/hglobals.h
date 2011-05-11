
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
} w_shared;
