
  /* This file, dglobals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/GLOBALS instead */




















#define GLOBAL_Initialised Yap_global->initialised
#define GLOBAL_InitialisedFromPL Yap_global->initialised_from_pl
#define GLOBAL_PL_Argc Yap_global->pl_argc
#define GLOBAL_PL_Argv Yap_global->pl_argv

#define GLOBAL_HaltHooks Yap_global->yap_halt_hook

#define GLOBAL_AllowLocalExpansion Yap_global->allow_local_expansion
#define GLOBAL_AllowGlobalExpansion Yap_global->allow_global_expansion
#define GLOBAL_AllowTrailExpansion Yap_global->allow_trail_expansion
#define GLOBAL_SizeOfOverflow Yap_global->size_of_overflow

#define GLOBAL_AGcThreshold Yap_global->agc_threshold
#define GLOBAL_AGCHook Yap_global->agc_hook

#if THREADS

#define GLOBAL_NOfThreads Yap_global->n_of_threads

#define GLOBAL_NOfThreadsCreated Yap_global->n_of_threads_created

#define GLOBAL_ThreadsTotalTime Yap_global->threads_total_time
#endif

#if defined(YAPOR) || defined(THREADS)

#define GLOBAL_BGL Yap_global->bgl
#endif

#ifdef THREADS
#define GLOBAL_ThreadHandlesLock Yap_global->thread_handles_lock
#endif 
#if defined(YAPOR) || defined(TABLING)
#define Yap_optyap_data Yap_global->optyap_data
#endif /* YAPOR || TABLING */

