
  /* This file, dlocals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/LOCALS instead */




#define LOCAL_c_input_stream LOCAL->c_input_stream
#define LOCAL_c_output_stream LOCAL->c_output_stream
#define LOCAL_c_error_stream LOCAL->c_error_stream

#define LOCAL_OldASP LOCAL->rinfo.old_ASP
#define LOCAL_OldLCL0 LOCAL->rinfo.old_LCL0
#define LOCAL_OldTR LOCAL->rinfo.old_TR
#define LOCAL_OldGlobalBase LOCAL->rinfo.old_GlobalBase
#define LOCAL_OldH LOCAL->rinfo.old_H
#define LOCAL_OldH0 LOCAL->rinfo.old_H0
#define LOCAL_OldTrailBase LOCAL->rinfo.old_TrailBase
#define LOCAL_OldTrailTop LOCAL->rinfo.old_TrailTop
#define LOCAL_OldHeapBase LOCAL->rinfo.old_HeapBase
#define LOCAL_OldHeapTop LOCAL->rinfo.old_HeapTop
#define LOCAL_ClDiff LOCAL->rinfo.cl_diff
#define LOCAL_GDiff LOCAL->rinfo.g_diff
#define LOCAL_HDiff LOCAL->rinfo.h_diff
#define LOCAL_GDiff0 LOCAL->rinfo.g_diff0
#define LOCAL_GSplit LOCAL->rinfo.g_split
#define LOCAL_LDiff LOCAL->rinfo.l_diff
#define LOCAL_TrDiff LOCAL->rinfo.tr_diff
#define LOCAL_XDiff LOCAL->rinfo.x_diff
#define LOCAL_DelayDiff LOCAL->rinfo.delay_diff
#define LOCAL_BaseDiff LOCAL->rinfo.base_diff
#define LOCAL_ReductionsCounter LOCAL->call_counters.reductions
#define LOCAL_PredEntriesCounter LOCAL->call_counters.reductions_retries
#define LOCAL_RetriesCounter LOCAL->call_counters.retries
#define LOCAL_ReductionsCounterOn LOCAL->call_counters.reductions_on
#define LOCAL_PredEntriesCounterOn LOCAL->call_counters.reductions_retries_on
#define LOCAL_RetriesCounterOn LOCAL->call_counters.retries_on
#define LOCAL_InterruptsDisabled LOCAL->interrupts_disabled


#define LOCAL_ConsultSp LOCAL->consultsp

#define LOCAL_ConsultCapacity LOCAL->consultcapacity

#define LOCAL_ConsultBase LOCAL->consultbase

#define LOCAL_ConsultLow LOCAL->consultlow

#define LOCAL_GlobalArena LOCAL->global_arena
#define LOCAL_GlobalArenaOverflows LOCAL->global_arena_overflows
#define LOCAL_ArenaOverflows LOCAL->arena_overflows
#define LOCAL_DepthArenas LOCAL->depth_arenas
#define LOCAL_ArithError LOCAL->arith_error
#define LOCAL_LastAssertedPred LOCAL->last_asserted_pred
#define LOCAL_DebugOn LOCAL->debug_on
#define LOCAL_ScannerStack LOCAL->scanner_stack
#define LOCAL_ScannerExtraBlocks LOCAL->scanner_extra_blocks
#define LOCAL_BallTerm LOCAL->ball_term
#define LOCAL_ActiveSignals LOCAL->active_signals
#define LOCAL_IPredArity LOCAL->i_pred_arity
#define LOCAL_ProfEnd LOCAL->prof_end
#define LOCAL_UncaughtThrow LOCAL->uncaught_throw
#define LOCAL_DoingUndefp LOCAL->doing_undefp
#define LOCAL_StartLine LOCAL->start_line
#define LOCAL_ScratchPad LOCAL->scratchpad
#ifdef  COROUTINING
#define LOCAL_WokenGoals LOCAL->woken_goals
#define LOCAL_AttsMutableList LOCAL->atts_mutable_list
#endif

#define LOCAL_GcGeneration LOCAL->gc_generation
#define LOCAL_GcPhase LOCAL->gc_phase
#define LOCAL_GcCurrentPhase LOCAL->gc_current_phase
#define LOCAL_GcCalls LOCAL->gc_calls
#define LOCAL_TotGcTime LOCAL->tot_gc_time
#define LOCAL_TotGcRecovered LOCAL->tot_gc_recovered
#define LOCAL_LastGcTime LOCAL->last_gc_time
#define LOCAL_LastSSTime LOCAL->last_ss_time
#if LOW_LEVEL_TRACER
#define LOCAL_total_choicepoints LOCAL->total_cps
#endif
#define LOCAL_consult_level LOCAL->consult_level_
#if defined(YAPOR) || defined(THREADS)
#define LOCAL_SignalLock LOCAL->signal_lock
#endif

#define LOCAL_total_marked LOCAL->tot_marked
#define LOCAL_total_oldies LOCAL->tot_oldies
#define LOCAL_current_B LOCAL->wl_current_B
#define LOCAL_prev_HB LOCAL->wl_prev_HB
#define LOCAL_HGEN LOCAL->hgen
#define LOCAL_iptop LOCAL->ip_top

#if defined(GC_NO_TAGS)
#define LOCAL_bp LOCAL->b_p
#endif
#define LOCAL_sTR LOCAL->wl_sTR
#define LOCAL_sTR0 LOCAL->wl_sTR0
#define LOCAL_new_TR LOCAL->new_tr
#define LOCAL_cont_top0 LOCAL->conttop0
#define LOCAL_cont_top LOCAL->conttop
#define LOCAL_discard_trail_entries LOCAL->disc_trail_entries
#define LOCAL_gc_ma_hash_table LOCAL->Gc_ma_hash_table
#define LOCAL_gc_ma_h_top LOCAL->Gc_ma_h_top
#define LOCAL_gc_ma_h_list LOCAL->Gc_ma_h_list
#define LOCAL_gc_timestamp LOCAL->Gc_timestamp
#define LOCAL_db_vec LOCAL->DB_vec
#define LOCAL_db_vec0 LOCAL->DB_vec0
#define LOCAL_db_root LOCAL->DB_root
#define LOCAL_db_nil LOCAL->DB_nil
#define LOCAL_gc_restore LOCAL->gc_restore
#define LOCAL_DynamicArrays LOCAL->dynamic_arrays
#define LOCAL_StaticArrays LOCAL->static_arrays
#define LOCAL_GlobalVariables LOCAL->global_variables
#define LOCAL_AllowRestart LOCAL->allow_restart

#define LOCAL_CMemFirstBlock LOCAL->cmem_first_block
#define LOCAL_CMemFirstBlockSz LOCAL->cmem_first_block_sz

#define LOCAL_LabelFirstArray LOCAL->label_first_array
#define LOCAL_LabelFirstArraySz LOCAL->label_first_array_sz

#define LOCAL_PL_local_data_p LOCAL->Yap_ld_
#define LOCAL_execution LOCAL->_execution
#ifdef THREADS
#define LOCAL_ThreadHandle LOCAL->thread_handle
#define FOREIGN_ThreadHandle(wid)  			(Yap_local[(wid)]->thread_handle)
#define MY_ThreadHandle	       				(Yap_local[worker_id]->thread_handle)
#endif
#if defined(YAPOR) || defined(TABLING)
#define LOCAL_optyap_data LOCAL->optyap_data
#endif /* YAPOR || TABLING */


