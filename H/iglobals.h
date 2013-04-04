
  /* This file, iglobals.h, was generated automatically by "yap -L misc/buildlocalglobal"
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

  INIT_LOCK(GLOBAL_ThreadHandlesLock);
#endif	
#if defined(YAPOR) || defined(THREADS)

  INIT_LOCK(GLOBAL_BGL);
#endif
#if defined(YAPOR) || defined(TABLING)

#endif /* YAPOR || TABLING */



#if defined(THREADS)

#endif /* THREADS */

  GLOBAL_stdout = stdout;
  GLOBAL_stderr = stderr;




#ifdef COROUTINING


#endif




  GLOBAL_tot_agc_time = 0;

  GLOBAL_tot_agc_recovered = 0;

#if HAVE_MMAP
  GLOBAL_mmap_arrays = NULL;
#endif
#ifdef DEBUG





#endif
#if defined(COFF)  || defined(A_OUT)


#endif
  GLOBAL_OpaqueHandlersCount = 0;
  GLOBAL_OpaqueHandlers = NULL;
#if  __simplescalar__

#endif














  GLOBAL_DIRNAME = NULL;
#if LOW_PROF
  GLOBAL_ProfilerOn = FALSE;
  GLOBAL_FProf = NULL;
  GLOBAL_FPreds = NULL;
#endif /* LOW_PROF */
}
