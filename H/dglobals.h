
  /* This file, dglobals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/GLOBALS instead */





















#if THREADS

#define Yap_NOfThreads Yap_global->n_of_threads

#define Yap_NOfThreadsCreated Yap_global->n_of_threads_created

#define Yap_ThreadsTotalTime Yap_global->threads_total_time
#endif

#if defined(YAPOR) || defined(THREADS)

#define Yap_BGL Yap_global->bgl
#endif

#define Yap_AllowLocalExpansion Yap_global->allow_local_expansion
#define Yap_AllowGlobalExpansion Yap_global->allow_global_expansion
#define Yap_AllowTrailExpansion Yap_global->allow_trail_expansion
#define Yap_SizeOfOverflow Yap_global->size_of_overflow

#define Yap_AGcLastCall Yap_global->agc_last_call

#define Yap_AGcThreshold Yap_global->agc_threshold
#define Yap_AGCHook Yap_global->agc_hook

#ifdef THREADS
#define Yap_ThreadHandlesLock Yap_global->thread_handles_lock
#endif 

#if defined(YAPOR) || defined(TABLING)
#define Yap_optyap_data Yap_global->optyap_data
#define REMOTE Yap_global->remote
#endif

#define Yap_Initialised Yap_global->initialised
#define Yap_InitialisedFromPL Yap_global->initialised_from_pl
#define Yap_PL_Argc Yap_global->pl_argc
#define Yap_PL_Argv Yap_global->pl_argv

#define Yap_HaltHooks Yap_global->yap_halt_hook

