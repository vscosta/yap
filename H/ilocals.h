
  /* This file, ilocals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/LOCALS instead */


static void InitWorker(int wid) {

  FOREIGN(wid)->c_input_stream = 0;
  FOREIGN(wid)->c_output_stream = 1;
  FOREIGN(wid)->c_error_stream = 2;

  FOREIGN(wid)->rinfo.old_ASP = NULL;
  FOREIGN(wid)->rinfo.old_LCL0 = NULL;
  FOREIGN(wid)->rinfo.old_TR = NULL;
  FOREIGN(wid)->rinfo.old_GlobalBase = NULL;
  FOREIGN(wid)->rinfo.old_H = NULL;
  FOREIGN(wid)->rinfo.old_H0 = NULL;
  FOREIGN(wid)->rinfo.old_TrailBase = NULL;
  FOREIGN(wid)->rinfo.old_TrailTop = NULL;
  FOREIGN(wid)->rinfo.old_HeapBase = NULL;
  FOREIGN(wid)->rinfo.old_HeapTop = NULL;
  FOREIGN(wid)->rinfo.cl_diff = 0L;
  FOREIGN(wid)->rinfo.g_diff = 0L;
  FOREIGN(wid)->rinfo.h_diff = 0L;
  FOREIGN(wid)->rinfo.g_diff0 = 0L;
  FOREIGN(wid)->rinfo.g_split = NULL;
  FOREIGN(wid)->rinfo.l_diff = 0L;
  FOREIGN(wid)->rinfo.tr_diff = 0L;
  FOREIGN(wid)->rinfo.x_diff = 0L;
  FOREIGN(wid)->rinfo.delay_diff = 0L;
  FOREIGN(wid)->rinfo.base_diff = 0L;
  FOREIGN(wid)->call_counters.reductions = 0L;
  FOREIGN(wid)->call_counters.reductions_retries = 0L;
  FOREIGN(wid)->call_counters.retries = 0L;
  FOREIGN(wid)->call_counters.reductions_on = 0L;
  FOREIGN(wid)->call_counters.reductions_retries_on = 0L;
  FOREIGN(wid)->call_counters.retries_on = 0L;
  FOREIGN(wid)->interrupts_disabled = FALSE;


  FOREIGN(wid)->consultsp = NULL;



  FOREIGN(wid)->consultbase = NULL;

  FOREIGN(wid)->consultlow = NULL;

  FOREIGN(wid)->global_arena = 0L;
  FOREIGN(wid)->global_arena_overflows = 0L;
  FOREIGN(wid)->arena_overflows = 0L;
  FOREIGN(wid)->depth_arenas = 0;
  FOREIGN(wid)->arith_error = FALSE;
  FOREIGN(wid)->last_asserted_pred = NULL;
  FOREIGN(wid)->debug_on = FALSE;
  FOREIGN(wid)->scanner_stack = NULL;
  FOREIGN(wid)->scanner_extra_blocks = NULL;
  FOREIGN(wid)->ball_term = NULL;
  FOREIGN(wid)->active_signals = 0L;
  FOREIGN(wid)->i_pred_arity = 0L;
  FOREIGN(wid)->prof_end = NULL;
  FOREIGN(wid)->uncaught_throw = FALSE;
  FOREIGN(wid)->doing_undefp = FALSE;
  FOREIGN(wid)->start_line = 0L;
  InitScratchPad(wid);
#ifdef  COROUTINING
  FOREIGN(wid)->woken_goals = 0L;
  FOREIGN(wid)->atts_mutable_list = 0L;
#endif

  FOREIGN(wid)->gc_generation = 0L;
  FOREIGN(wid)->gc_phase = 0L;
  FOREIGN(wid)->gc_current_phase = 0L;
  FOREIGN(wid)->gc_calls = 0L;
  FOREIGN(wid)->tot_gc_time = 0L;
  FOREIGN(wid)->tot_gc_recovered = 0L;
  FOREIGN(wid)->last_gc_time = 0L;
  FOREIGN(wid)->last_ss_time = 0L;
#if LOW_LEVEL_TRACER
  FOREIGN(wid)->total_cps = 0;
#endif
  FOREIGN(wid)->consult_level_ = 0;

#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(FOREIGN(wid)->signal_lock);

  FOREIGN(wid)->tot_marked = 0L;
  FOREIGN(wid)->tot_oldies = 0L;
#if DEBUG && COROUTINING
  FOREIGN(wid)->tot_smarked = 0L;
#endif
  FOREIGN(wid)->wl_current_B = NULL;
  FOREIGN(wid)->wl_prev_HB = NULL;
  FOREIGN(wid)->hgen = NULL;
  FOREIGN(wid)->ip_top = NULL;
#if GC_NO_TAGS
  FOREIGN(wid)->b_p = NULL;
#endif
#if defined(TABLING) || defined(YAPOR_SBA)
  FOREIGN(wid)->wl_sTR = NULL;
  FOREIGN(wid)->wl_sTR0 = NULL;
  FOREIGN(wid)->new_tr = NULL;
#else
  FOREIGN(wid)->wl_sTR = NULL;
  FOREIGN(wid)->wl_sTR0 = NULL;
  FOREIGN(wid)->new_tr = NULL;
#endif
  FOREIGN(wid)->conttop0 = NULL;
  FOREIGN(wid)->conttop = NULL;
  FOREIGN(wid)->disc_trail_entries = 0;

  FOREIGN(wid)->Gc_ma_h_top = NULL;
  FOREIGN(wid)->Gc_ma_h_list = NULL;
  FOREIGN(wid)->Gc_timestamp = 0L;
  FOREIGN(wid)->DB_vec = NULL;
  FOREIGN(wid)->DB_vec0 = NULL;
  FOREIGN(wid)->DB_root = NULL;
  FOREIGN(wid)->DB_nil = NULL;
#endif /* defined(YAPOR) || defined(THREADS) */

  FOREIGN(wid)->dynamic_arrays = NULL;
  FOREIGN(wid)->static_arrays = NULL;
  FOREIGN(wid)->global_variables = NULL;
  FOREIGN(wid)->allow_restart = FALSE;

  FOREIGN(wid)->cmem_first_block = NULL;
  FOREIGN(wid)->cmem_first_block_sz = 0L;

  FOREIGN(wid)->label_first_array = NULL;
  FOREIGN(wid)->label_first_array_sz = 0L;

  FOREIGN(wid)->Yap_ld_ = Yap_InitThreadIO(wid);
  FOREIGN(wid)->_execution = NULL;
#ifdef THREADS
  InitThreadHandle(wid);
#define FOREIGN_ThreadHandle(wid)  (Yap_WLocal[(wid)]->thread_handle)		       						
#define MY_ThreadHandle	       (Yap_WLocal[worker_id]->thread_handle)
#endif

}
