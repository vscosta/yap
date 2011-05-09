
  /* This file, iglobals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/GLOBALS instead */


















static void InitGlobal(void) {

  Yap_Initialised = FALSE;
  Yap_InitialisedFromPL = FALSE;
  Yap_PL_Argc = 0;
  Yap_PL_Argv = NULL;

  Yap_HaltHooks = NULL;

  Yap_AllowLocalExpansion = TRUE;
  Yap_AllowGlobalExpansion = TRUE;
  Yap_AllowTrailExpansion = TRUE;
  Yap_SizeOfOverflow = 0;

  Yap_AGcLastCall = 0;

  Yap_AGcThreshold = 10000;
  Yap_AGCHook = NULL;

#if THREADS

  Yap_NOfThreads = 1;

  Yap_NOfThreadsCreated = 1;

  Yap_ThreadsTotalTime = 0L;
#endif

#if defined(YAPOR) || defined(THREADS)

  INIT_LOCK(Yap_BGL);
#endif

#ifdef THREADS
  INIT_LOCK(Yap_ThreadHandlesLock);
#endif 
#if defined(YAPOR) || defined(TABLING)

#endif /* YAPOR || TABLING */
}
