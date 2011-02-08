
  /* This file, iglobals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/GLOBALS instead */



















static void InitWorker(int wid) {

  FOREIGN_WL(wid)->c_input_stream = 0;
  FOREIGN_WL(wid)->c_output_stream = 1;
  FOREIGN_WL(wid)->c_error_stream = 2;

  FOREIGN_WL(wid)->rinfo.old_ASP = NULL;
  FOREIGN_WL(wid)->rinfo.old_LCL0 = NULL;
  FOREIGN_WL(wid)->rinfo.old_TR = NULL;
  FOREIGN_WL(wid)->rinfo.old_GlobalBase = NULL;
  FOREIGN_WL(wid)->rinfo.old_H = NULL;
  FOREIGN_WL(wid)->rinfo.old_H0 = NULL;
  FOREIGN_WL(wid)->rinfo.old_TrailBase = NULL;
  FOREIGN_WL(wid)->rinfo.old_TrailTop = NULL;
  FOREIGN_WL(wid)->rinfo.old_HeapBase = NULL;
  FOREIGN_WL(wid)->rinfo.old_HeapTop = NULL;
  FOREIGN_WL(wid)->rinfo.cl_diff = 0L;
  FOREIGN_WL(wid)->rinfo.g_diff = 0L;
  FOREIGN_WL(wid)->rinfo.h_diff = 0L;
  FOREIGN_WL(wid)->rinfo.g_diff0 = 0L;
  FOREIGN_WL(wid)->rinfo.g_split = NULL;
  FOREIGN_WL(wid)->rinfo.l_diff = 0L;
  FOREIGN_WL(wid)->rinfo.tr_diff = 0L;
  FOREIGN_WL(wid)->rinfo.x_diff = 0L;
  FOREIGN_WL(wid)->rinfo.delay_diff = 0L;
  FOREIGN_WL(wid)->rinfo.base_diff = 0L;
  FOREIGN_WL(wid)->call_counters.reductions = 0L;
  FOREIGN_WL(wid)->call_counters.reductions_retries = 0L;
  FOREIGN_WL(wid)->call_counters.retries = 0L;
  FOREIGN_WL(wid)->call_counters.reductions_on = 0L;
  FOREIGN_WL(wid)->call_counters.reductions_retries_on = 0L;
  FOREIGN_WL(wid)->call_counters.retries_on = 0L;
  FOREIGN_WL(wid)->interrupts_disabled = FALSE;


  FOREIGN_WL(wid)->consultsp = NULL;



  FOREIGN_WL(wid)->consultbase = NULL;

  FOREIGN_WL(wid)->consultlow = NULL;

  FOREIGN_WL(wid)->global_arena = 0L;
  FOREIGN_WL(wid)->global_arena_overflows = 0L;
  FOREIGN_WL(wid)->delay_arena_overflows = 0L;
  FOREIGN_WL(wid)->arena_overflows = 0L;
  FOREIGN_WL(wid)->depth_arenas = 0;
  FOREIGN_WL(wid)->arith_error = FALSE;
  FOREIGN_WL(wid)->last_asserted_pred = NULL;
  FOREIGN_WL(wid)->debug_on = FALSE;
  FOREIGN_WL(wid)->f_info = NULL;
  FOREIGN_WL(wid)->scanner_stack = NULL;
  FOREIGN_WL(wid)->scanner_extra_blocks = NULL;
  FOREIGN_WL(wid)->ball_term = NULL;
  FOREIGN_WL(wid)->active_signals = 0L;
  FOREIGN_WL(wid)->i_pred_arity = 0L;
  FOREIGN_WL(wid)->prof_end = NULL;
  FOREIGN_WL(wid)->uncaught_throw = FALSE;
  FOREIGN_WL(wid)->doing_undefp = FALSE;
  FOREIGN_WL(wid)->start_line = 0L;
  InitScratchPad(wid);
#ifdef  COROUTINING
  FOREIGN_WL(wid)->woken_goals = 0L;
  FOREIGN_WL(wid)->atts_mutable_list = 0L;
#endif

  FOREIGN_WL(wid)->gc_generation = 0L;
  FOREIGN_WL(wid)->gc_phase = 0L;
  FOREIGN_WL(wid)->gc_current_phase = 0L;
  FOREIGN_WL(wid)->gc_calls = 0L;
  FOREIGN_WL(wid)->tot_gc_time = 0L;
  FOREIGN_WL(wid)->tot_gc_recovered = 0L;
  FOREIGN_WL(wid)->last_gc_time = 0L;
  FOREIGN_WL(wid)->last_ss_time = 0L;
#if LOW_LEVEL_TRACER
  FOREIGN_WL(wid)->total_cps = 0;
#endif

#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(FOREIGN_WL(wid)->signal_lock);
  FOREIGN_WL(wid)->wpp = NULL;

  FOREIGN_WL(wid)->tot_marked = 0L;
  FOREIGN_WL(wid)->tot_oldies = 0L;
#if DEBUG && COROUTINING
  FOREIGN_WL(wid)->tot_smarked = 0L;
#endif
  FOREIGN_WL(wid)->wl_current_B = NULL;
  FOREIGN_WL(wid)->wl_prev_HB = NULL;
  FOREIGN_WL(wid)->hgen = NULL;
  FOREIGN_WL(wid)->ip_top = NULL;
#if GC_NO_TAGS
  FOREIGN_WL(wid)->b_p = NULL;
#endif
#if defined(TABLING) || defined(SBA)
  FOREIGN_WL(wid)->wl_sTR = NULL;
  FOREIGN_WL(wid)->wl_sTR0 = NULL;
  FOREIGN_WL(wid)->new_tr = NULL;
#else
  FOREIGN_WL(wid)->wl_sTR = NULL;
  FOREIGN_WL(wid)->wl_sTR0 = NULL;
  FOREIGN_WL(wid)->new_tr = NULL;
#endif
  FOREIGN_WL(wid)->conttop0 = NULL;
  FOREIGN_WL(wid)->conttop = NULL;
  FOREIGN_WL(wid)->disc_trail_entries = 0;

  FOREIGN_WL(wid)->Gc_ma_h_top = NULL;
  FOREIGN_WL(wid)->Gc_ma_h_list = NULL;
  FOREIGN_WL(wid)->Gc_timestamp = 0L;
  FOREIGN_WL(wid)->DB_vec = NULL;
  FOREIGN_WL(wid)->DB_vec0 = NULL;
  FOREIGN_WL(wid)->DB_root = NULL;
  FOREIGN_WL(wid)->DB_nil = NULL;
#endif /* defined(YAPOR) || defined(THREADS) */

  FOREIGN_WL(wid)->dynamic_arrays = NULL;
  FOREIGN_WL(wid)->static_arrays = NULL;
  FOREIGN_WL(wid)->global_variables = NULL;
  FOREIGN_WL(wid)->allow_restart = FALSE;

  FOREIGN_WL(wid)->cmem_first_block = NULL;
  FOREIGN_WL(wid)->cmem_first_block_sz = 0L;

  FOREIGN_WL(wid)->label_first_array = NULL;
  FOREIGN_WL(wid)->label_first_array_sz = 0L;

  FOREIGN_WL(wid)->putc_curp_ = NULL;
  FOREIGN_WL(wid)->putc_cur_buf_ = NULL;
  FOREIGN_WL(wid)->putc_cur_lim_ = NULL;
  FOREIGN_WL(wid)->putc_cur_flags_ = 0L;
  InitSWIBuffers(wid);

  FOREIGN_WL(wid)->SWI_buf_index_ = 0;
  FOREIGN_WL(wid)->_execution = NULL;

#if (defined(YAPOR) || defined(TABLING)) && defined(THREADS)

#endif
#ifdef THREADS
  InitThreadHandle(wid);
#define FOREIGN_ThreadHandle(wid)  (Yap_WLocal[(wid)].thread_handle)		       						
#define MY_ThreadHandle	       (Yap_WLocal[worker_id].thread_handle)
#endif

}

static void InitGlobal(void) {

#if THREADS

  Yap_global->n_of_threads = 1;

  Yap_global->n_of_threads_created = 1;

  Yap_global->threads_total_time = 0L;
#endif

#if defined(YAPOR) || defined(THREADS)

  INIT_LOCK(Yap_global->bgl);
#endif
  Yap_global->f_swi_stream = NULL;
  Yap_global->swi_getc = NULL;
  Yap_global->swi_putc = NULL;
  Yap_global->swi_wgetc = NULL;
  Yap_global->swi_wputc = NULL;
  Yap_global->swi_close = NULL;
  Yap_global->swi_flush = NULL;
  Yap_global->swi_get_stream_f = NULL;
  Yap_global->swi_get_stream_position_f = NULL;

  Yap_global->allow_local_expansion = TRUE;
  Yap_global->allow_global_expansion = TRUE;
  Yap_global->allow_trail_expansion = TRUE;
  Yap_global->size_of_overflow = 0;

  Yap_global->agc_last_call = 0;

  Yap_global->agc_threshold = 10000;
  Yap_global->agc_hook = NULL;

#if HAVE_LIBREADLINE
  Yap_global->readline_buf = NULL;
  Yap_global->readline_pos = 0L;
#endif

#ifdef THREADS
  INIT_LOCK(Yap_global->thread_handles_lock);
#endif 

#if defined(YAPOR) || defined(TABLING)


#endif

  Yap_global->initialised = FALSE;
  Yap_global->initialised_from_pl = FALSE;
  Yap_global->pl_argc = 0;
  Yap_global->pl_argv = NULL;

  Yap_global->yap_halt_hook = NULL;
}
