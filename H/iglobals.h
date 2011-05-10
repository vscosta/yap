
  /* This file, iglobals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/GLOBALS instead */


















static void InitGlobal(void) {

  GLOBAL_Initialised = FALSE;
  GLOBAL_InitialisedFromPL = FALSE;
  GLOBAL_PL_Argc = 0;
  GLOBAL_PL_Argv = NULL;

  GLOBAL_HaltHooks = NULL;

  GLOBAL_AllowLocalExpansion = TRUE;
  GLOBAL_AllowGlobalExpansion = TRUE;
  GLOBAL_AllowTrailExpansion = TRUE;
  GLOBAL_SizeOfOverflow = 0;

  GLOBAL_AGcThreshold = 10000;
  GLOBAL_AGCHook = NULL;

#if THREADS

  GLOBAL_NOfThreads = 1;

  GLOBAL_NOfThreadsCreated = 1;

  GLOBAL_ThreadsTotalTime = 0L;
#endif

#if defined(YAPOR) || defined(THREADS)

  INIT_LOCK(GLOBAL_BGL);
#endif

#ifdef THREADS
  INIT_LOCK(GLOBAL_ThreadHandlesLock);
#endif 
#if defined(YAPOR) || defined(TABLING)

#endif /* YAPOR || TABLING */
}
