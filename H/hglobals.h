
  /* This file, hglobals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/GLOBALS instead */



















typedef struct worker_local {

  int  c_input_stream;
  int  c_output_stream;
  int  c_error_stream;

  restoreinfo  rinfo;




















  struct reduction_counters  call_counters;






  int  interrupts_disabled;


  union CONSULT_OBJ*  consultsp;

  UInt  consultcapacity;

  union CONSULT_OBJ*  consultbase;

  union CONSULT_OBJ*  consultlow;

  Term  global_arena;
  UInt  global_arena_overflows;
  Int  delay_arena_overflows;
  Int  arena_overflows;
  Int  depth_arenas;
  int  arith_error;
  struct pred_entry*  last_asserted_pred;
  int  debug_on;
  struct format_status*  f_info;
  char*  scanner_stack;
  struct scanner_extra_alloc*  scanner_extra_blocks;
  struct DB_TERM  *ball_term;
  UInt  active_signals;
  UInt  i_pred_arity;
  yamop*  prof_end;
  int  uncaught_throw;
  int  doing_undefp;
  Int  start_line;
  scratch_block  scratchpad;
#ifdef  COROUTINING
  Term  woken_goals;
  Term  atts_mutable_list;
#endif

  Term  gc_generation;
  Term  gc_phase;
  UInt  gc_current_phase;
  UInt  gc_calls;
  Int  tot_gc_time;
  YAP_ULONG_LONG  tot_gc_recovered;
  Int  last_gc_time;
  Int  last_ss_time;
#if LOW_LEVEL_TRACER
  Int  total_cps;
#endif

#if defined(YAPOR) || defined(THREADS)
  lockvar  signal_lock;
  struct pred_entry*  wpp;

  Int  tot_marked;
  Int  tot_oldies;
#if DEBUG && COROUTINING
  UInt  tot_smarked;
#endif
  struct choicept  *wl_current_B;
  CELL*  wl_prev_HB;
  CELL*  hgen;
  CELL**  ip_top;
#if GC_NO_TAGS
  char*  b_p;
#endif
#if defined(TABLING) || defined(SBA)
  struct trail_frame*  wl_sTR;
  struct trail_frame*  wl_sTR0;
  struct trail_frame*  new_tr;
#else
  Term  *wl_sTR;
  Term  *wl_sTR0;
  Term  *new_tr;
#endif
  struct gc_mark_continuation*  conttop0;
  struct gc_mark_continuation*  conttop;
  int  disc_trail_entries;
  gc_ma_hash_entry  Gc_ma_hash_table[GC_MAVARS_HASH_SIZE];
  gc_ma_hash_entry*  Gc_ma_h_top;
  gc_ma_hash_entry*  Gc_ma_h_list;
  UInt  Gc_timestamp;
  ADDR  DB_vec;
  ADDR  DB_vec0;
  struct RB_red_blk_node*  DB_root;
  struct RB_red_blk_node*  DB_nil;
#endif /* defined(YAPOR) || defined(THREADS) */
  sigjmp_buf  gc_restore;
  struct array_entry*  dynamic_arrays;
  struct static_array_entry*  static_arrays;
  struct global_entry*  global_variables;
  int  allow_restart;

  struct mem_blk*  cmem_first_block;
  UInt  cmem_first_block_sz;

  Int*  label_first_array;
  UInt  label_first_array_sz;

  char*  putc_curp_;
  char*  putc_cur_buf_;
  char*  putc_cur_lim_;
  UInt  putc_cur_flags_;
  char*  SWI_buffers_[1+SWI_BUF_RINGS];
  size_t  SWI_buffers_sz_[1+SWI_BUF_RINGS];
  int  SWI_buf_index_;
  struct open_query_struct*  _execution;

#if (defined(YAPOR) || defined(TABLING)) && defined(THREADS)
  struct worker  worker;
#endif
#ifdef THREADS
  struct thandle  thread_handle;
#define FOREIGN_ThreadHandle(wid)  (Yap_WLocal[(wid)].thread_handle)		       						
#define MY_ThreadHandle	       (Yap_WLocal[worker_id].thread_handle)
#endif

} w_local;

typedef struct worker_shared {

#if THREADS

  UInt  n_of_threads;

  UInt  n_of_threads_created;

  UInt  threads_total_time;
#endif

#if defined(YAPOR) || defined(THREADS)

  lockvar  bgl;
#endif
  Functor  f_swi_stream;
  SWI_GetFunction  swi_getc;
  SWI_PutFunction  swi_putc;
  SWI_GetWideFunction  swi_wgetc;
  SWI_PutWideFunction  swi_wputc;
  SWI_CloseFunction  swi_close;
  SWI_FlushFunction  swi_flush;
  SWI_PLGetStreamFunction  swi_get_stream_f;
  SWI_PLGetStreamPositionFunction  swi_get_stream_position_f;

  int  allow_local_expansion;
  int  allow_global_expansion;
  int  allow_trail_expansion;
  UInt  size_of_overflow;

  UInt  agc_last_call;

  UInt  agc_threshold;
  Agc_hook  agc_hook;

#if HAVE_LIBREADLINE
  char  *readline_buf;
  char  *readline_pos;
#endif

#ifdef THREADS
  lockvar  thread_handles_lock;
#endif 

#if defined(YAPOR) || defined(TABLING)
  struct global_data  global;
  struct local_data  remote[MAX_WORKERS];
#endif

  int  initialised;
  int  initialised_from_pl;
  int  pl_argc;
  char  **pl_argv;

  struct halt_hook  *yap_halt_hook;
} w_shared;
