
  /* This file, iglobals.h, was generated automatically by "yap -L misc/buildlocalglobal"
     please do not update, update H/GLOBALS instead */

















static void InitGlobal(void) {

  GLOBAL_Initialised = FALSE;
  GLOBAL_InitialisedFromPL = FALSE;
  GLOBAL_PL_Argc = 0;
  GLOBAL_PL_Argv = NULL;
  GLOBAL_FAST_BOOT_FLAG = false;

  GLOBAL_HaltHooks = NULL;
  GLOBAL_JIT_finalizer = NULL;

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

  GLOBAL_named_mboxes = NULL;
  INIT_LOCK(GLOBAL_mboxq_lock);
  GLOBAL_mbox_count = 0;

#endif /* THREADS */


#if defined(THREADS)||defined(YAPOR)
  INIT_LOCK(GLOBAL_StreamDescLock);
#endif




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

#if THREADS
  GLOBAL_FreeMutexes = NULL;
  GLOBAL_mutex_backbone = NULL;
  INIT_LOCK(GLOBAL_MUT_ACCESS);
#endif
  GLOBAL_Home = NULL;

  GLOBAL_CharConversionTable = NULL;
  GLOBAL_CharConversionTable2 = NULL;

  GLOBAL_MaxPriority = 1200;

  GLOBAL_FileAliases = Yap_InitStandardAliases();


  GLOBAL_VFS = Yap_InitAssetManager();
  GLOBAL_cwd = NULL;
}
