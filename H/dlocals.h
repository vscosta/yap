
  /* This file, dlocals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/LOCALS instead */




#define LOCAL_c_input_stream WL->c_input_stream
#define LOCAL_c_output_stream WL->c_output_stream
#define LOCAL_c_error_stream WL->c_error_stream

#define LOCAL_OldASP WL->rinfo.old_ASP
#define LOCAL_OldLCL0 WL->rinfo.old_LCL0
#define LOCAL_OldTR WL->rinfo.old_TR
#define LOCAL_OldGlobalBase WL->rinfo.old_GlobalBase
#define LOCAL_OldH WL->rinfo.old_H
#define LOCAL_OldH0 WL->rinfo.old_H0
#define LOCAL_OldTrailBase WL->rinfo.old_TrailBase
#define LOCAL_OldTrailTop WL->rinfo.old_TrailTop
#define LOCAL_OldHeapBase WL->rinfo.old_HeapBase
#define LOCAL_OldHeapTop WL->rinfo.old_HeapTop
#define LOCAL_ClDiff WL->rinfo.cl_diff
#define LOCAL_GDiff WL->rinfo.g_diff
#define LOCAL_HDiff WL->rinfo.h_diff
#define LOCAL_GDiff0 WL->rinfo.g_diff0
#define LOCAL_GSplit WL->rinfo.g_split
#define LOCAL_LDiff WL->rinfo.l_diff
#define LOCAL_TrDiff WL->rinfo.tr_diff
#define LOCAL_XDiff WL->rinfo.x_diff
#define LOCAL_DelayDiff WL->rinfo.delay_diff
#define LOCAL_BaseDiff WL->rinfo.base_diff
#define LOCAL_ReductionsCounter WL->call_counters.reductions
#define LOCAL_PredEntriesCounter WL->call_counters.reductions_retries
#define LOCAL_RetriesCounter WL->call_counters.retries
#define LOCAL_ReductionsCounterOn WL->call_counters.reductions_on
#define LOCAL_PredEntriesCounterOn WL->call_counters.reductions_retries_on
#define LOCAL_RetriesCounterOn WL->call_counters.retries_on
#define LOCAL_InterruptsDisabled WL->interrupts_disabled


#define LOCAL_ConsultSp WL->consultsp

#define LOCAL_ConsultCapacity WL->consultcapacity

#define LOCAL_ConsultBase WL->consultbase

#define LOCAL_ConsultLow WL->consultlow

#define LOCAL_GlobalArena WL->global_arena
#define LOCAL_GlobalArenaOverflows WL->global_arena_overflows
#define LOCAL_DelayArenaOverflows WL->delay_arena_overflows
#define LOCAL_ArenaOverflows WL->arena_overflows
#define LOCAL_DepthArenas WL->depth_arenas
#define LOCAL_ArithError WL->arith_error
#define LOCAL_LastAssertedPred WL->last_asserted_pred
#define LOCAL_DebugOn WL->debug_on
#define LOCAL_FormatInfo WL->f_info
#define LOCAL_ScannerStack WL->scanner_stack
#define LOCAL_ScannerExtraBlocks WL->scanner_extra_blocks
#define LOCAL_BallTerm WL->ball_term
#define LOCAL_ActiveSignals WL->active_signals
#define LOCAL_IPredArity WL->i_pred_arity
#define LOCAL_ProfEnd WL->prof_end
#define LOCAL_UncaughtThrow WL->uncaught_throw
#define LOCAL_DoingUndefp WL->doing_undefp
#define LOCAL_StartLine WL->start_line
#define LOCAL_ScratchPad WL->scratchpad
#ifdef  COROUTINING
#define LOCAL_WokenGoals WL->woken_goals
#define LOCAL_AttsMutableList WL->atts_mutable_list
#endif

#define LOCAL_GcGeneration WL->gc_generation
#define LOCAL_GcPhase WL->gc_phase
#define LOCAL_GcCurrentPhase WL->gc_current_phase
#define LOCAL_GcCalls WL->gc_calls
#define LOCAL_TotGcTime WL->tot_gc_time
#define LOCAL_TotGcRecovered WL->tot_gc_recovered
#define LOCAL_LastGcTime WL->last_gc_time
#define LOCAL_LastSSTime WL->last_ss_time
#if LOW_LEVEL_TRACER
#define LOCAL_total_choicepoints WL->total_cps
#endif
#define LOCAL_consult_level WL->consult_level_

#if defined(YAPOR) || defined(THREADS)
#define LOCAL_SignalLock WL->signal_lock
#define LOCAL_WPP WL->wpp

#define LOCAL_total_marked WL->tot_marked
#define LOCAL_total_oldies WL->tot_oldies
#if DEBUG && COROUTINING
#define LOCAL_total_smarked WL->tot_smarked
#endif
#define LOCAL_current_B WL->wl_current_B
#define LOCAL_prev_HB WL->wl_prev_HB
#define LOCAL_HGEN WL->hgen
#define LOCAL_iptop WL->ip_top
#if GC_NO_TAGS
#define LOCAL_bp WL->b_p
#endif
#if defined(TABLING) || defined(YAPOR_SBA)
#define LOCAL_sTR WL->wl_sTR
#define LOCAL_sTR0 WL->wl_sTR0
#define LOCAL_new_TR WL->new_tr
#else
#define LOCAL_sTR WL->wl_sTR
#define LOCAL_sTR0 WL->wl_sTR0
#define LOCAL_new_TR WL->new_tr
#endif
#define LOCAL_cont_top0 WL->conttop0
#define LOCAL_cont_top WL->conttop
#define LOCAL_discard_trail_entries WL->disc_trail_entries
#define LOCAL_gc_ma_hash_table WL->Gc_ma_hash_table
#define LOCAL_gc_ma_h_top WL->Gc_ma_h_top
#define LOCAL_gc_ma_h_list WL->Gc_ma_h_list
#define LOCAL_gc_timestamp WL->Gc_timestamp
#define LOCAL_db_vec WL->DB_vec
#define LOCAL_db_vec0 WL->DB_vec0
#define LOCAL_db_root WL->DB_root
#define LOCAL_db_nil WL->DB_nil
#endif /* defined(YAPOR) || defined(THREADS) */
#define LOCAL_gc_restore WL->gc_restore
#define LOCAL_DynamicArrays WL->dynamic_arrays
#define LOCAL_StaticArrays WL->static_arrays
#define LOCAL_GlobalVariables WL->global_variables
#define LOCAL_AllowRestart WL->allow_restart

#define LOCAL_CMemFirstBlock WL->cmem_first_block
#define LOCAL_CMemFirstBlockSz WL->cmem_first_block_sz

#define LOCAL_LabelFirstArray WL->label_first_array
#define LOCAL_LabelFirstArraySz WL->label_first_array_sz

#define LOCAL_PL_local_data_p WL->Yap_ld_
#define LOCAL_execution WL->_execution
#ifdef THREADS
#define LOCAL_ThreadHandle WL->thread_handle
#define FOREIGN_ThreadHandle(wid)  (Yap_WLocal[(wid)]->thread_handle)		       						
#define MY_ThreadHandle	       (Yap_WLocal[worker_id]->thread_handle)
#endif


