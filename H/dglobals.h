
  /* This file, dglobals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/GLOBALS instead */





















#define Yap_c_input_stream WL->c_input_stream
#define Yap_c_output_stream WL->c_output_stream
#define Yap_c_error_stream WL->c_error_stream

#define OldASP WL->rinfo.old_ASP
#define OldLCL0 WL->rinfo.old_LCL0
#define OldTR WL->rinfo.old_TR
#define OldGlobalBase WL->rinfo.old_GlobalBase
#define OldH WL->rinfo.old_H
#define OldH0 WL->rinfo.old_H0
#define OldTrailBase WL->rinfo.old_TrailBase
#define OldTrailTop WL->rinfo.old_TrailTop
#define OldHeapBase WL->rinfo.old_HeapBase
#define OldHeapTop WL->rinfo.old_HeapTop
#define ClDiff WL->rinfo.cl_diff
#define GDiff WL->rinfo.g_diff
#define HDiff WL->rinfo.h_diff
#define GDiff0 WL->rinfo.g_diff0
#define GSplit WL->rinfo.g_split
#define LDiff WL->rinfo.l_diff
#define TrDiff WL->rinfo.tr_diff
#define XDiff WL->rinfo.x_diff
#define DelayDiff WL->rinfo.delay_diff
#define BaseDiff WL->rinfo.base_diff
#define ReductionsCounter WL->call_counters.reductions
#define PredEntriesCounter WL->call_counters.reductions_retries
#define RetriesCounter WL->call_counters.retries
#define ReductionsCounterOn WL->call_counters.reductions_on
#define PredEntriesCounterOn WL->call_counters.reductions_retries_on
#define RetriesCounterOn WL->call_counters.retries_on
#define Yap_InterruptsDisabled WL->interrupts_disabled


#define ConsultSp WL->consultsp

#define ConsultCapacity WL->consultcapacity

#define ConsultBase WL->consultbase

#define ConsultLow WL->consultlow

#define GlobalArena WL->global_arena
#define GlobalArenaOverflows WL->global_arena_overflows
#define DelayArenaOverflows WL->delay_arena_overflows
#define ArenaOverflows WL->arena_overflows
#define DepthArenas WL->depth_arenas
#define ArithError WL->arith_error
#define LastAssertedPred WL->last_asserted_pred
#define DebugOn WL->debug_on
#define FormatInfo WL->f_info
#define ScannerStack WL->scanner_stack
#define ScannerExtraBlocks WL->scanner_extra_blocks
#define BallTerm WL->ball_term
#define ActiveSignals WL->active_signals
#define IPredArity WL->i_pred_arity
#define ProfEnd WL->prof_end
#define UncaughtThrow WL->uncaught_throw
#define DoingUndefp WL->doing_undefp
#define StartLine WL->start_line
#define ScratchPad WL->scratchpad
#ifdef  COROUTINING
#define WokenGoals WL->woken_goals
#define AttsMutableList WL->atts_mutable_list
#endif

#define GcGeneration WL->gc_generation
#define GcPhase WL->gc_phase
#define GcCurrentPhase WL->gc_current_phase
#define GcCalls WL->gc_calls
#define TotGcTime WL->tot_gc_time
#define TotGcRecovered WL->tot_gc_recovered
#define LastGcTime WL->last_gc_time
#define LastSSTime WL->last_ss_time
#if LOW_LEVEL_TRACER
#define Yap_total_choicepoints WL->total_cps
#endif

#if defined(YAPOR) || defined(THREADS)
#define SignalLock WL->signal_lock
#define WPP WL->wpp

#define total_marked WL->tot_marked
#define total_oldies WL->tot_oldies
#if DEBUG && COROUTINING
#define total_smarked WL->tot_smarked
#endif
#define current_B WL->wl_current_B
#define prev_HB WL->wl_prev_HB
#define HGEN WL->hgen
#define iptop WL->ip_top
#if GC_NO_TAGS
#define Yap_bp WL->b_p
#endif
#if defined(TABLING) || defined(SBA)
#define sTR WL->wl_sTR
#define sTR0 WL->wl_sTR0
#define new_TR WL->new_tr
#else
#define sTR WL->wl_sTR
#define sTR0 WL->wl_sTR0
#define new_TR WL->new_tr
#endif
#define cont_top0 WL->conttop0
#define cont_top WL->conttop
#define discard_trail_entries WL->disc_trail_entries
#define gc_ma_hash_table WL->Gc_ma_hash_table
#define gc_ma_h_top WL->Gc_ma_h_top
#define gc_ma_h_list WL->Gc_ma_h_list
#define gc_timestamp WL->Gc_timestamp
#define db_vec WL->DB_vec
#define db_vec0 WL->DB_vec0
#define db_root WL->DB_root
#define db_nil WL->DB_nil
#endif /* defined(YAPOR) || defined(THREADS) */
#define Yap_gc_restore WL->gc_restore
#define DynamicArrays WL->dynamic_arrays
#define StaticArrays WL->static_arrays
#define GlobalVariables WL->global_variables
#define Yap_AllowRestart WL->allow_restart

#define Yap_CMemFirstBlock WL->cmem_first_block
#define Yap_CMemFirstBlockSz WL->cmem_first_block_sz

#define Yap_LabelFirstArray WL->label_first_array
#define Yap_LabelFirstArraySz WL->label_first_array_sz

#define putc_curp WL->putc_curp_
#define putc_cur_buf WL->putc_cur_buf_
#define putc_cur_lim WL->putc_cur_lim_
#define putc_cur_flags WL->putc_cur_flags_
#define SWI_buffers WL->SWI_buffers_
#define SWI_buffers_sz WL->SWI_buffers_sz_
#define SWI_buf_index WL->SWI_buf_index_
#define execution WL->_execution

#if (defined(YAPOR) || defined(TABLING)) && defined(THREADS)
#define WORKER WL->worker
#endif
#ifdef THREADS
#define ThreadHandle WL->thread_handle
#define FOREIGN_ThreadHandle(wid)  (Yap_WLocal[(wid)].thread_handle)		       						
#define MY_ThreadHandle	       (Yap_WLocal[worker_id].thread_handle)
#endif





#if THREADS

#define NOfThreads Yap_global->n_of_threads

#define NOfThreadsCreated Yap_global->n_of_threads_created

#define ThreadsTotalTime Yap_global->threads_total_time
#endif

#if defined(YAPOR) || defined(THREADS)

#define BGL Yap_global->bgl
#endif
#define FSWIStream Yap_global->f_swi_stream
#define SWIGetc Yap_global->swi_getc
#define SWIPutc Yap_global->swi_putc
#define SWIWideGetc Yap_global->swi_wgetc
#define SWIWidePutc Yap_global->swi_wputc
#define SWIClose Yap_global->swi_close
#define SWIFlush Yap_global->swi_flush
#define SWIGetStream Yap_global->swi_get_stream_f
#define SWIGetStreamPosition Yap_global->swi_get_stream_position_f

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

