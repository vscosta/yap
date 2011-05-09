
  /* This file, ilocals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/LOCALS instead */


static void InitWorker(int wid) {

  REMOTE(wid)->c_input_stream = 0;
  REMOTE(wid)->c_output_stream = 1;
  REMOTE(wid)->c_error_stream = 2;

  REMOTE(wid)->rinfo.old_ASP = NULL;
  REMOTE(wid)->rinfo.old_LCL0 = NULL;
  REMOTE(wid)->rinfo.old_TR = NULL;
  REMOTE(wid)->rinfo.old_GlobalBase = NULL;
  REMOTE(wid)->rinfo.old_H = NULL;
  REMOTE(wid)->rinfo.old_H0 = NULL;
  REMOTE(wid)->rinfo.old_TrailBase = NULL;
  REMOTE(wid)->rinfo.old_TrailTop = NULL;
  REMOTE(wid)->rinfo.old_HeapBase = NULL;
  REMOTE(wid)->rinfo.old_HeapTop = NULL;
  REMOTE(wid)->rinfo.cl_diff = 0L;
  REMOTE(wid)->rinfo.g_diff = 0L;
  REMOTE(wid)->rinfo.h_diff = 0L;
  REMOTE(wid)->rinfo.g_diff0 = 0L;
  REMOTE(wid)->rinfo.g_split = NULL;
  REMOTE(wid)->rinfo.l_diff = 0L;
  REMOTE(wid)->rinfo.tr_diff = 0L;
  REMOTE(wid)->rinfo.x_diff = 0L;
  REMOTE(wid)->rinfo.delay_diff = 0L;
  REMOTE(wid)->rinfo.base_diff = 0L;
  REMOTE(wid)->call_counters.reductions = 0L;
  REMOTE(wid)->call_counters.reductions_retries = 0L;
  REMOTE(wid)->call_counters.retries = 0L;
  REMOTE(wid)->call_counters.reductions_on = 0L;
  REMOTE(wid)->call_counters.reductions_retries_on = 0L;
  REMOTE(wid)->call_counters.retries_on = 0L;
  REMOTE(wid)->interrupts_disabled = FALSE;


  REMOTE(wid)->consultsp = NULL;



  REMOTE(wid)->consultbase = NULL;

  REMOTE(wid)->consultlow = NULL;

  REMOTE(wid)->global_arena = 0L;
  REMOTE(wid)->global_arena_overflows = 0L;
  REMOTE(wid)->arena_overflows = 0L;
  REMOTE(wid)->depth_arenas = 0;
  REMOTE(wid)->arith_error = FALSE;
  REMOTE(wid)->last_asserted_pred = NULL;
  REMOTE(wid)->debug_on = FALSE;
  REMOTE(wid)->scanner_stack = NULL;
  REMOTE(wid)->scanner_extra_blocks = NULL;
  REMOTE(wid)->ball_term = NULL;
  REMOTE(wid)->active_signals = 0L;
  REMOTE(wid)->i_pred_arity = 0L;
  REMOTE(wid)->prof_end = NULL;
  REMOTE(wid)->uncaught_throw = FALSE;
  REMOTE(wid)->doing_undefp = FALSE;
  REMOTE(wid)->start_line = 0L;
  InitScratchPad(wid);
#ifdef  COROUTINING
  REMOTE(wid)->woken_goals = 0L;
  REMOTE(wid)->atts_mutable_list = 0L;
#endif

  REMOTE(wid)->gc_generation = 0L;
  REMOTE(wid)->gc_phase = 0L;
  REMOTE(wid)->gc_current_phase = 0L;
  REMOTE(wid)->gc_calls = 0L;
  REMOTE(wid)->tot_gc_time = 0L;
  REMOTE(wid)->tot_gc_recovered = 0L;
  REMOTE(wid)->last_gc_time = 0L;
  REMOTE(wid)->last_ss_time = 0L;
#if LOW_LEVEL_TRACER
  REMOTE(wid)->total_cps = 0;
#endif
  REMOTE(wid)->consult_level_ = 0;
#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(REMOTE(wid)->signal_lock);
#endif

  REMOTE(wid)->tot_marked = 0L;
  REMOTE(wid)->tot_oldies = 0L;
  REMOTE(wid)->wl_current_B = NULL;
  REMOTE(wid)->wl_prev_HB = NULL;
  REMOTE(wid)->hgen = NULL;
  REMOTE(wid)->ip_top = NULL;

#if defined(GC_NO_TAGS)
  REMOTE(wid)->b_p = NULL;
#endif
  REMOTE(wid)->wl_sTR = NULL;
  REMOTE(wid)->wl_sTR0 = NULL;
  REMOTE(wid)->new_tr = NULL;
  REMOTE(wid)->conttop0 = NULL;
  REMOTE(wid)->conttop = NULL;
  REMOTE(wid)->disc_trail_entries = 0;

  REMOTE(wid)->Gc_ma_h_top = NULL;
  REMOTE(wid)->Gc_ma_h_list = NULL;
  REMOTE(wid)->Gc_timestamp = 0L;
  REMOTE(wid)->DB_vec = NULL;
  REMOTE(wid)->DB_vec0 = NULL;
  REMOTE(wid)->DB_root = NULL;
  REMOTE(wid)->DB_nil = NULL;

  REMOTE(wid)->dynamic_arrays = NULL;
  REMOTE(wid)->static_arrays = NULL;
  REMOTE(wid)->global_variables = NULL;
  REMOTE(wid)->allow_restart = FALSE;

  REMOTE(wid)->cmem_first_block = NULL;
  REMOTE(wid)->cmem_first_block_sz = 0L;

  REMOTE(wid)->label_first_array = NULL;
  REMOTE(wid)->label_first_array_sz = 0L;

  REMOTE(wid)->Yap_ld_ = Yap_InitThreadIO(wid);
  REMOTE(wid)->_execution = NULL;
#ifdef THREADS
  InitThreadHandle(wid);
#endif /* THREADS */
#if defined(YAPOR) || defined(TABLING)
  Yap_init_local_optyap_data(wid);
#endif /* YAPOR || TABLING */

#define REMOTE_ThreadHandle(wid)     (REMOTE(wid)->thread_handle)
#define REMOTE_c_input_stream(wid)   (REMOTE(wid)->c_input_stream)
#define REMOTE_c_output_stream(wid)  (REMOTE(wid)->c_output_stream)
#define REMOTE_c_error_stream(wid)   (REMOTE(wid)->c_error_stream)
#define REMOTE_ActiveSignals(wid)    (REMOTE(wid)->active_signals)
#define REMOTE_SignalLock(wid)       (REMOTE(wid)->signal_lock)
#define REMOTE_ScratchPad(wid)       (REMOTE(wid)->scratchpad)

}
