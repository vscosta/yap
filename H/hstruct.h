
  /* This file, hstruct.h, was generated automatically by "yap -L misc/buildheap"
     please do not update, update misc/HEAPFIELDS instead */




























  UInt  hole_size;
  struct malloc_state  *av_;
#if USE_DL_MALLOC
  struct memory_hole  memory_holes[MAX_DLMALLOC_HOLES];
  UInt  nof_memory_holes;
#if defined(YAPOR) || defined(THREADS)
  lockvar  dlmalloc_lock;
#endif
#endif
#if USE_DL_MALLOC || (USE_SYSTEM_MALLOC && HAVE_MALLINFO)
#ifndef  HeapUsed
#define  HeapUsed  Yap_givemallinfo()		
#endif
  Int  heap_used;
#else
  Int  heap_used;
#endif
  Int  heap_max;
  ADDR  heap_top;
  ADDR  heap_lim;
  struct FREEB  *free_blocks;
#if defined(YAPOR) || defined(THREADS)
  lockvar  free_blocks_lock;
  lockvar  heap_used_lock;
  lockvar  heap_top_lock;
  int  heap_top_owner;
#endif


#if USE_THREADED_CODE
  opentry  *op_rtable;
#endif

  OPCODE  execute_cpred_op_code;
  OPCODE  expand_op_code;
  OPCODE  fail_op;
  OPCODE  index_op;
  OPCODE  lockpred_op;
  OPCODE  orlast_op;
  OPCODE  undef_op;

  UInt  n_of_atoms;
  UInt  atom_hash_table_size;
  UInt  wide_atom_hash_table_size;
  UInt  n_of_wide_atoms;
  AtomHashEntry  invisiblechain;
  AtomHashEntry  *wide_hash_chain;
  AtomHashEntry  *hash_chain;

#include "tatoms.h"
#ifdef EUROTRA
  Term  term_dollar_u;
#endif
  Term  term_prolog;
  Term  term_refound_var;
  Term  user_module;
  Term  idb_module;
  Term  attributes_module;
  Term  charsio_module;
  Term  terms_module;
  Term  system_module;
  Term  operating_system_module;
  Term  readutil_module;
  Term  hacks_module;
  Term  arg_module;
  Term  globals_module;
  Term  swi_module;



  struct mod_entry  *current_modules;




  yap_exec_mode  execution_mode;

  struct pred_entry  **pred_hash;
#if defined(YAPOR) || defined(THREADS)
  rwlock_t  pred_hash_rw_lock;
#endif
  UInt  preds_in_hash_table;
  UInt  pred_hash_table_size;

  struct pred_entry  *creep_code;
  struct pred_entry  *undef_code;
  struct pred_entry  *spy_code;
  struct pred_entry  *pred_fail;
  struct pred_entry  *pred_true;
#ifdef COROUTINING
  struct pred_entry  *wake_up_code;
#endif
  struct pred_entry  *pred_goal_expansion;
  struct pred_entry  *pred_meta_call;
  struct pred_entry  *pred_dollar_catch;
  struct pred_entry  *pred_recorded_with_key;
  struct pred_entry  *pred_log_upd_clause;
  struct pred_entry  *pred_log_upd_clause_erase;
  struct pred_entry  *pred_log_upd_clause0;
  struct pred_entry  *pred_static_clause;
  struct pred_entry  *pred_throw;
  struct pred_entry  *pred_handle_throw;
  struct pred_entry  *pred_is;
  struct pred_entry  *pred_safe_call_cleanup;
  struct pred_entry  *pred_restore_regs;
#ifdef YAPOR
  struct pred_entry  *pred_getwork;
  struct pred_entry  *pred_getwork_seq;
#endif /* YAPOR */

#ifdef LOW_LEVEL_TRACER
  int  yap_do_low_level_trace;
#if defined(YAPOR) || defined(THREADS)
  lockvar  low_level_trace_lock;
#endif
#endif

  UInt  clause_space;
  UInt  index_space_Tree;
  UInt  index_space_EXT;
  UInt  index_space_SW;
  UInt  lu_clause_space;
  UInt  lu_index_space_Tree;
  UInt  lu_index_space_CP;
  UInt  lu_index_space_EXT;
  UInt  lu_index_space_SW;

  yamop  comma_code[5];
  yamop  dummycode[1];
  yamop  failcode[1];
  yamop  nocode[1];
  yamop  env_for_trustfail[2];
  yamop  *trustfailcode;
  yamop  env_for_yescode[2];
  yamop  *yescode;
  yamop  rtrycode[1];
#ifdef BEAM
  yamop  beam_retry_code[1];
#endif /* BEAM */
#ifdef YAPOR
  int  seq_def;
  yamop  getwork_code[1];
  yamop  getwork_seq_code[1];
  yamop  getwork_first_time[1];
#endif /* YAPOR */
#ifdef TABLING
  yamop  table_load_answer_code[1];
  yamop  table_try_answer_code[1];
  yamop  table_answer_resolution_code[1];
  yamop  table_completion_code[1];
#endif /* TABLING */




  yamop  *debugger_p_before_spy;

  yamop  *retry_recordedp_code;
  yamop  *retry_recorded_k_code;

  int  system_profiling;
  int  system_call_counting;
  int  system_pred_goal_expansion_all;
  int  system_pred_goal_expansion_func;
  int  system_pred_goal_expansion_on;
  int  compiler_optimizer_on;
  int  compiler_compile_mode;
  int  compiler_profiling;
  int  compiler_call_counting;

  int  compiler_compile_arrays;

#if defined(YAPOR) || defined(THREADS)
  lockvar  dbterms_list_lock;
#endif
  struct dbterm_list  *dbterms_list;

  yamop  *expand_clauses_first;
  yamop  *expand_clauses_last;
  UInt  expand_clauses;
#if defined(YAPOR) || defined(THREADS)
  lockvar  expand_clauses_list_lock;
  lockvar  op_list_lock;
#endif

#ifdef DEBUG
  UInt  new_cps;
  UInt  live_cps;
  UInt  dirty_cps;
  UInt  freed_cps;
#endif
  UInt  expand_clauses_sz;

  struct udi_info  *udi_control_blocks;


  Int  static_predicates_marked;

  Prop  *IntKeys;
  Prop  *IntLUKeys;
  Prop  *IntBBKeys;

  UInt  int_keys_size;
  UInt  int_keys_timestamp;
  UInt  int_bb_keys_size;

  int  update_mode;

  struct DB_STRUCT  *db_erased_marker;
  struct logic_upd_clause  *logdb_erased_marker;

  struct static_clause  *dead_static_clauses;
  struct static_mega_clause  *dead_mega_clauses;
  struct static_index  *dead_static_indices;
  struct logic_upd_clause  *db_erased_list;
  struct logic_upd_index  *db_erased_ilist;
#if defined(YAPOR) || defined(THREADS)
  lockvar  dead_static_clauses_lock;
  lockvar  dead_mega_clauses_lock;
  lockvar  dead_static_indices_lock;
#endif
#ifdef COROUTINING

  int  num_of_atts;

  UInt  atts_size;
#endif

  Int  yap_flags_field[NUMBER_OF_YAP_FLAGS];

  struct operator_entry  *op_list;


  struct stream_desc  *yap_streams;

  UInt  n_of_file_aliases;
  UInt  sz_of_file_aliases;
  struct AliasDescS  *file_aliases;

  Atom  atprompt;
  char  prompt[MAX_PROMPT];

  char  *char_conversion_table;
  char  *char_conversion_table2;

  UInt  maxdepth;
  UInt  axlist;
  UInt  maxwriteargs;

  int  parser_error_style;

  char  *yap_lib_dir;

  void  *last_wtime;

  int  debugger_output_msg;
#if LOW_PROF
  int  profiler_on;
  int  offline_profiler;
  FILE  *f_prof;
  FILE  *f_preds;
  UInt  prof_preds;
#endif /* LOW_PROF */

  struct ForeignLoadItem  *foreign_code_loaded;
  ADDR  foreign_code_base;
  ADDR  foreign_code_top;
  ADDR  foreign_code_max;

  struct record_list  *yap_records;

  Atom  swi_atoms[N_SWI_ATOMS];
  Functor  swi_functors[N_SWI_FUNCTORS];
  struct swi_reverse_hash  swi_reverse_hash[N_SWI_HASH];

  struct PL_blob_t  *swi_blobs;
