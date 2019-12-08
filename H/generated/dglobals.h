
  /* This file, dglobals.h, was generated automatically by "yap -L misc/buildlocalglobal"
     please do not update, update H/GLOBALS instead */



















#define GLOBAL_Initialised Yap_global->Initialised_
#define GLOBAL_InitialisedFromPL Yap_global->InitialisedFromPL_
#define GLOBAL_PL_Argc Yap_global->PL_Argc_
#define GLOBAL_PL_Argv Yap_global->PL_Argv_
#define GLOBAL_FAST_BOOT_FLAG Yap_global->FAST_BOOT_FLAG_

#define GLOBAL_HaltHooks Yap_global->HaltHooks_
#define GLOBAL_JIT_finalizer Yap_global->JIT_finalizer_

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

#define GLOBAL_PrologShouldHandleInterrupts Yap_global->PrologShouldHandleInterrupts_

#if defined(THREADS)
#define GLOBAL_master_thread Yap_global->master_thread_
#define GLOBAL_named_mboxes Yap_global->named_mboxes_
#define GLOBAL_mboxq_lock Yap_global->mboxq_lock_
#define GLOBAL_mbox_count Yap_global->mbox_count_
#define GLOBAL_WithMutex Yap_global->WithMutex_
#endif /* THREADS */

#define GLOBAL_Stream Yap_global->Stream_
#if defined(THREADS)||defined(YAPOR)
#define GLOBAL_StreamDescLock Yap_global->StreamDescLock_
#endif

#define GLOBAL_argv Yap_global->argv_
#define GLOBAL_argc Yap_global->argc_

#ifdef COROUTINING

#define GLOBAL_attas Yap_global->attas_
#endif

#define GLOBAL_agc_calls Yap_global->agc_calls_
#define GLOBAL_agc_collected Yap_global->agc_collected_

#define GLOBAL_tot_agc_time Yap_global->tot_agc_time_

#define GLOBAL_tot_agc_recovered Yap_global->tot_agc_recovered_

#if HAVE_MMAP
#define GLOBAL_mmap_arrays Yap_global->mmap_arrays_
#endif
#ifdef DEBUG

#define GLOBAL_Option Yap_global->Option_
#define GLOBAL_logfile Yap_global->logfile_


#endif
#if defined(COFF)  || defined(A_OUT)

#define GLOBAL_Executable Yap_global->Executable_
#endif
#define GLOBAL_OpaqueHandlersCount Yap_global->OpaqueHandlersCount_
#define GLOBAL_OpaqueHandlers Yap_global->OpaqueHandlers_
#if  __simplescalar__
#define GLOBAL_pwd Yap_global->pwd_
#endif


#define GLOBAL_RestoreFile Yap_global->RestoreFile_

#define GLOBAL_ProfCalls Yap_global->ProfCalls_
#define GLOBAL_ProfGCs Yap_global->ProfGCs_
#define GLOBAL_ProfHGrows Yap_global->ProfHGrows_
#define GLOBAL_ProfSGrows Yap_global->ProfSGrows_
#define GLOBAL_ProfMallocs Yap_global->ProfMallocs_
#define GLOBAL_ProfIndexing Yap_global->ProfIndexing_
#define GLOBAL_ProfOn Yap_global->ProfOn_
#define GLOBAL_ProfOns Yap_global->ProfOns_
#define GLOBAL_ProfilerRoot Yap_global->ProfilerRoot_
#define GLOBAL_ProfilerNil Yap_global->ProfilerNil_
#define GLOBAL_DIRNAME Yap_global->DIRNAME_
#if LOW_PROF
#define GLOBAL_ProfilerOn Yap_global->ProfilerOn_
#define GLOBAL_FProf Yap_global->FProf_
#define GLOBAL_FPreds Yap_global->FPreds_
#endif /* LOW_PROF */

#if THREADS
#define GLOBAL_FreeMutexes Yap_global->FreeMutexes_
#define GLOBAL_mutex_backbone Yap_global->mutex_backbone_
#define GLOBAL_MUT_ACCESS Yap_global->MUT_ACCESS_
#endif
#define GLOBAL_Home Yap_global->Home_

#define GLOBAL_CharConversionTable Yap_global->CharConversionTable_
#define GLOBAL_CharConversionTable2 Yap_global->CharConversionTable2_

#define GLOBAL_MaxPriority Yap_global->MaxPriority_

#define GLOBAL_FileAliases Yap_global->FileAliases_
#define GLOBAL_NOfFileAliases Yap_global->NOfFileAliases_
#define GLOBAL_SzOfFileAliases Yap_global->SzOfFileAliases_
#define GLOBAL_VFS Yap_global->VFS_
#define GLOBAL_cwd Yap_global->cwd_

