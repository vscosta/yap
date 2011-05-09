
  /* This file, hlocals.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/LOCALS instead */


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
  Int  arena_overflows;
  Int  depth_arenas;
  int  arith_error;
  struct pred_entry*  last_asserted_pred;
  int  debug_on;
  char*  scanner_stack;
  struct scanner_extra_alloc*  scanner_extra_blocks;
  struct DB_TERM*  ball_term;
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
  int  consult_level_;
#if defined(YAPOR) || defined(THREADS)
  lockvar  signal_lock;
#endif

  Int  tot_marked;
  Int  tot_oldies;
  struct choicept*  wl_current_B;
  CELL*  wl_prev_HB;
  CELL*  hgen;
  CELL**  ip_top;

#if defined(GC_NO_TAGS)
  char*  b_p;
#endif
  tr_fr_ptr  wl_sTR;
  tr_fr_ptr  wl_sTR0;
  tr_fr_ptr  new_tr;
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
  sigjmp_buf  gc_restore;
  struct array_entry*  dynamic_arrays;
  struct static_array_entry*  static_arrays;
  struct global_entry*  global_variables;
  int  allow_restart;

  struct mem_blk*  cmem_first_block;
  UInt  cmem_first_block_sz;

  Int*  label_first_array;
  UInt  label_first_array_sz;

  struct PL_local_data*  Yap_ld_;
  struct open_query_struct*  _execution;
#ifdef THREADS
  struct thandle  thread_handle;
#define FOREIGN_ThreadHandle(wid)  			(Yap_local[(wid)]->thread_handle)
#define MY_ThreadHandle	       				(Yap_local[worker_id]->thread_handle)
#endif
#if defined(YAPOR) || defined(TABLING)
  struct local_optyap_data  optyap_data;
#endif /* YAPOR || TABLING */

} w_local;
