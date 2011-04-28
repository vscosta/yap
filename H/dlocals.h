
  /* This file, dlocals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/LOCALS instead */




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
#if defined(TABLING) || defined(YAPOR_SBA)
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

#define PL_local_data_p WL->Yap_ld_
#define execution WL->_execution
#ifdef THREADS
#define ThreadHandle WL->thread_handle
#define FOREIGN_ThreadHandle(wid)  (Yap_WLocal[(wid)]->thread_handle)		       						
#define MY_ThreadHandle	       (Yap_WLocal[worker_id]->thread_handle)
#endif


