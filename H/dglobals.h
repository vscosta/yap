
  /* This file, dglobals.h, was generated automatically by "yap -L misc/buildlocalglobal"
     please do not update, update misc/GLOBALS instead */



















#define GLOBAL_Initialised Yap_global->Initialised_
#define GLOBAL_InitialisedFromPL Yap_global->InitialisedFromPL_
#define GLOBAL_PL_Argc Yap_global->PL_Argc_
#define GLOBAL_PL_Argv Yap_global->PL_Argv_

#define GLOBAL_HaltHooks Yap_global->HaltHooks_

#define GLOBAL_AllowLocalExpansion Yap_global->AllowLocalExpansion_
#define GLOBAL_AllowGlobalExpansion Yap_global->AllowGlobalExpansion_
#define GLOBAL_AllowTrailExpansion Yap_global->AllowTrailExpansion_
#define GLOBAL_SizeOfOverflow Yap_global->SizeOfOverflow_

#define GLOBAL_AGcThreshold Yap_global->AGcThreshold_
#define GLOBAL_AGCHook Yap_global->AGCHook_

#if THREADS

#define GLOBAL_NOfThreads Yap_global->NOfThreads_

#define GLOBAL_NOfThreadsCreated Yap_global->NOfThreadsCreated_

#define GLOBAL_ThreadsTotalTime Yap_global->ThreadsTotalTime_

#define GLOBAL_ThreadHandlesLock Yap_global->ThreadHandlesLock_
#endif
#if defined(YAPOR) || defined(THREADS)

#define GLOBAL_BGL Yap_global->BGL_
#endif
#if defined(YAPOR) || defined(TABLING)
#define GLOBAL_optyap_data Yap_global->optyap_data_
#endif /* YAPOR || TABLING */

