

























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
  Term  readutil_module;
  Term  hacks_module;
  Term  arg_module;
  Term  globals_module;
  Term  swi_module;



  struct mod_entry  *current_modules;





#if USE_THREADED_CODE
  opentry  *op_rtable;
#endif

  yap_exec_mode  execution_mode;

  OPCODE  execute_cpred_op_code;
  OPCODE  expand_op_code;
  OPCODE  fail_op;
  OPCODE  index_op;
  OPCODE  lockpred_op;
  OPCODE  undef_op;

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
  int  num_of_atts;
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
  UInt  expand_clauses_sz;
#endif

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
