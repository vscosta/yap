
  /* This file, dglobals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/GLOBALS instead */





















#if THREADS

#define NOfThreads Yap_global->n_of_threads

#define NOfThreadsCreated Yap_global->n_of_threads_created

#define ThreadsTotalTime Yap_global->threads_total_time
#endif

#if defined(YAPOR) || defined(THREADS)

#define BGL Yap_global->bgl
#endif

#define Yap_AllowLocalExpansion Yap_global->allow_local_expansion
#define Yap_AllowGlobalExpansion Yap_global->allow_global_expansion
#define Yap_AllowTrailExpansion Yap_global->allow_trail_expansion
#define SizeOfOverflow Yap_global->size_of_overflow

#define AGcLastCall Yap_global->agc_last_call

#define AGcThreshold Yap_global->agc_threshold
#define AGCHook Yap_global->agc_hook

#if HAVE_LIBREADLINE
#define ReadlineBuf Yap_global->readline_buf
#define ReadlinePos Yap_global->readline_pos
#endif

#ifdef THREADS
#define ThreadHandlesLock Yap_global->thread_handles_lock
#endif 

#if defined(YAPOR) || defined(TABLING)
#define GLOBAL Yap_global->global
#define REMOTE Yap_global->remote
#endif

#define Yap_Initialised Yap_global->initialised
#define Yap_InitialisedFromPL Yap_global->initialised_from_pl
#define Yap_PL_Argc Yap_global->pl_argc
#define Yap_PL_Argv Yap_global->pl_argv

#define Yap_HaltHooks Yap_global->yap_halt_hook

