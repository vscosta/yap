/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		Heap.h         						 *
* mods:									 *
* comments:	Heap Init Structure					 *
* version:      $Id: Heap.h,v 1.69 2004-10-27 15:56:34 vsc Exp $	 *
*************************************************************************/

/* information that can be stored in Code Space */

#ifndef HEAP_H
#define HEAP_H 1

#ifndef INT_KEYS_DEFAULT_SIZE
#define INT_KEYS_DEFAULT_SIZE 256
#endif


typedef struct atom_hash_entry {
#if defined(YAPOR) || defined(THREADS)
  rwlock_t AERWLock;
#endif
  Atom Entry;
} AtomHashEntry;

typedef struct reduction_counters {
  YAP_ULONG_LONG reductions;
  YAP_ULONG_LONG reductions_retries;
  YAP_ULONG_LONG retries;
  int reductions_on;
  int reductions_retries_on;
  int retries_on;
} red_counters;

typedef struct scratch_block_struct {
  char *ptr;
  UInt sz, msz;
} scratch_block;

typedef struct worker_local_struct {
#if defined(YAPOR) || defined(THREADS)
  lockvar  signal_lock;        /* protect signal handlers from IPIs */
  struct pred_entry *wpp;
#endif
  UInt   active_signals;
  UInt   delayed_trace;
  UInt   i_pred_arity;
  yamop *prof_end;
  Int    start_line;
  scratch_block scratchpad;
#ifdef MULTI_ASSIGNMENT_VARIABLES
  Term   woken_goals;
  Term   mutable_list;
  Term   atts_mutable_list;
#endif
  /* gc_stuff */
  unsigned int      gc_calls;	/* number of times GC has been called */ 
  Int      tot_gc_time; /* total time spent in GC */
  Int      tot_gc_recovered; /* number of heap objects in all garbage collections */
  jmp_buf  gc_restore; /* where to jump if garbage collection crashes */
  struct trail_frame *old_TR;
} worker_local;

#ifdef THREADS
typedef struct thandle {
  int in_use;
  UInt ssize;
  UInt tsize;
  Term tdetach;
  Term  cmod;
  struct DB_TERM *tgoal;
  int id;
  int ret;
  REGSTORE *default_yaam_regs;
  REGSTORE *current_yaam_regs;
  struct pred_entry *local_preds;
  pthread_t handle;
  int ref_count;
  pthread_mutex_t tlock;
#if HAVE_GETRUSAGE
  struct timeval *start_of_timesp;
  struct timeval *last_timep;
#endif
} yap_thandle;
#endif

typedef int   (*Agc_hook)(Atom);

typedef struct various_codes {
  special_functors funcs;
  Int heap_used;
  Int heap_max;
  ADDR heap_top;
  ADDR heap_lim;
  struct FREEB  *free_blocks;
#if defined(YAPOR) || defined(THREADS)
  lockvar  bgl;		 /* protect long critical regions   */
  lockvar  free_blocks_lock;     /* protect the list of free blocks */
  worker_local wl[MAX_WORKERS];
#else
  worker_local wl;
#endif
#ifdef YAPOR
  int   seq_def;
  yamop getworkcode;
  yamop getworkcode_seq;
  yamop getworkfirsttimecode;
#endif /* YAPOR */
#ifdef TABLING
  yamop tablecompletioncode;
  yamop tableanswerresolutioncode;
#endif /* TABLING */
  OPCODE expand_op_code;
  yamop *expand_clauses_first, *expand_clauses_last;
#if defined(YAPOR) || defined(THREADS)
  lockvar expand_clauses_list_lock;
#endif
  yamop comma_code[5];
  yamop failcode[1];
  OPCODE failcode_1;
  OPCODE failcode_2;
  OPCODE failcode_3;
  OPCODE failcode_4;
  OPCODE failcode_5;
  OPCODE failcode_6;
  struct {
    OPCODE op;
#ifdef YAPOR
    COUNT             ltt;
    COUNT             cut;
    COUNT             seq;
#endif /* YAPOR */
    COUNT s;
    CELL *l2;
    struct pred_entry *p;
    struct pred_entry *p0;
  } env_for_trustfail_code; /* sla */
  yamop trustfailcode[1];
  struct {
    OPCODE op;
#ifdef YAPOR
    COUNT             ltt;
    COUNT             cut;
    COUNT             seq;
#endif /* YAPOR */
    COUNT s;
    CELL *l2;
    struct pred_entry *p;
    struct pred_entry *p0;
  } env_for_yes_code; /* sla */
  yamop yescode[1];
  yamop nocode[1];
  yamop rtrycode[1];
  struct {
    OPREG arity;
    struct yami *clause;
    Functor func;
  } clausecode[1];
  union CONSULT_OBJ *consultsp;
  union CONSULT_OBJ *consultbase;
  union CONSULT_OBJ *consultlow;
  UInt   consultcapacity;
#if HAVE_LIBREADLINE
  char *readline_buf, *readline_pos;
#endif
#if USE_THREADED_CODE
  opentry *op_rtable;
#endif
#ifdef COROUTINING
  int  num_of_atts;            /* max. number of attributes we have for a variable */
  struct pred_entry  *wake_up_code;
#endif
  struct pred_entry  *creep_code;
  struct pred_entry  *undef_code;
  struct pred_entry  *spy_code;
  int   system_profiling;
  int   system_call_counting;
  int   system_pred_goal_expansion_on;
  int   compiler_optimizer_on;
  int   compiler_compile_mode;
  AtomHashEntry invisiblechain;
  OPCODE dummycode[1];
  UInt maxdepth, maxlist;
  int update_mode;
  Atom atprompt;
  char prompt[MAX_PROMPT];
  OPCODE undef_op; 
  OPCODE index_op; 
  OPCODE fail_op; 
  yamop *retry_recorded_k_code,
    *retry_c_recordedp_code;
  Int static_predicates_marked;
  UInt int_keys_size;
  UInt int_keys_timestamp;
  Prop *IntKeys;
  UInt int_lu_keys_size;
  UInt int_lu_keys_timestamp;
  Prop *IntLUKeys;
  UInt int_bb_keys_size;
  Prop *IntBBKeys;
  Int   yap_flags_field[NUMBER_OF_YAP_FLAGS];
  char *char_conversion_table;
  char *char_conversion_table2;
#if THREADS
  unsigned int n_of_threads;      /* number of threads and processes in system */
  unsigned int n_of_threads_created;      /* number of threads created since start */
  UInt  threads_total_time;      /* total run time for dead threads */
#endif
#if defined(YAPOR) || defined(THREADS)
  lockvar  heap_used_lock;        /* protect HeapUsed */
  lockvar  heap_top_lock;        /* protect HeapTop */
  lockvar  dead_clauses_lock;        /* protect DeadClauses */
  int      heap_top_owner;
#ifdef LOW_LEVEL_TRACER
  lockvar  low_level_trace_lock;
#endif
#endif
  unsigned int size_of_overflow;
  Term  module_name[MaxModules];
  struct pred_entry *module_pred[MaxModules];
  SMALLUNSGN   no_of_modules;
  struct dead_clause *dead_clauses;
  Atom
    atom_abol,
    atom_alarm,
    atom_append,
    atom_array,
    atom_assert,
    atom_b,
    atom_break,
    atom_call,
    atom_catch,
    atom_comma,
    atom_cpu_time,
    atom_csult,
    atom_cut,
    atom_cut_by,
#ifdef EUROTRA
#ifdef SFUNC
    atom_dollar_undef,
#endif
#endif
    atom_dbref,
    atom_e,
    atom_e_q,
    atom_eof,
#ifdef EUROTRA
    atom_f_b,
#endif
    atom_fail,
    atom_false,
    atom_fast,
    atom_g_t,
    atom_gc,
    atom_gc_margin,
    atom_gc_trace,
    atom_gc_verbose,
    atom_gc_very_verbose,
    atom_global,
    atom_heap_used,
    atom_inf,
    atom_l_t,
    atom_local,
    atom_meta_call,
    atom_minus,
    atom_multi_file,
    atom_nan,
    atom_otherwise,
    atom_pi,
    atom_plus,
    atom_portray,
    atom_profile,
    atom_random,
    atom_read,
    atom_repeat,
    atom_restore_regs,
#if HAVE_SIGACTION
    atom_sig_pending,
#endif
    atom_stack_free,
    atom_true,
    atom_unwritable,
    atom_user,
    atom_usr_in,
    atom_usr_out,
    atom_usr_err,
    atom_version_number,
    atom_write;
  Functor
#ifdef   USE_SOCKET
    functor_af_inet,
    functor_af_local,
    functor_af_unix,
#endif
    functor_alt_not,
#ifdef COROUTINING
    functor_array_entry,
#endif
    functor_arrow,
    functor_assert,
#ifdef COROUTINING
    functor_att_goal,   /* goal that activates attributed variables */
#endif
    functor_braces,
    functor_call,
    functor_clist,
    functor_comma,
    functor_creep,
    functor_csult,
    functor_cut_by,
    functor_eq,
    functor_execute_in_mod,
    functor_execute_within,
    functor_g_atom,
    functor_g_atomic,
    functor_g_compound,
    functor_g_integer,
    functor_g_float,
    functor_g_number,
    functor_g_primitive,
    functor_g_var,
    functor_last_execute_within,
    functor_list,
    functor_mega_clause,
    functor_module,
    functor_multi_file_clause,
#ifdef MULTI_ASSIGNMENT_VARIABLES
    functor_mutable,
#endif
    functor_not,
    functor_or,
    functor_portray,
    functor_query,
    functor_static_clause,
    functor_stream,
    functor_stream_pos,
    functor_stream_eOS,
    functor_thread_run,
    functor_change_module,
    functor_current_module,
    functor_u_minus,
    functor_u_plus,
    functor_v_bar,
    functor_var;
  Term
#ifdef EUROTRA
    term_dollar_u,
#endif
    term_prolog,
    term_refound_var,
    user_module,
    idb_module,
    attributes_module,
    charsio_module,
    terms_module;
  void *last_wtime;
  struct pred_entry *pred_goal_expansion;
  struct pred_entry *pred_meta_call;
  struct pred_entry *pred_dollar_catch;
  struct pred_entry *pred_recorded_with_key;
  struct pred_entry *pred_log_upd_clause;
  struct pred_entry *pred_log_upd_clause0;
  struct pred_entry *pred_static_clause;
  struct pred_entry *pred_throw;
  struct pred_entry *pred_handle_throw;
  struct array_entry *dyn_array_list;
  struct DB_STRUCT *db_erased_marker;
#ifdef DEBUG
  struct logic_upd_clause *db_erased_list;
  struct logic_upd_index *db_erased_ilist;
  UInt expand_clauses_sz;
#endif /* DEBUG */
  struct stream_desc *yap_streams;
#ifdef DEBUG
  int    debugger_output_msg;
#endif
  UInt n_of_file_aliases;
  UInt sz_of_file_aliases;
  struct AliasDescS * file_aliases;
#if LOW_PROF
  int   profiler_on;
  void *f_prof, *f_preds;
  UInt  prof_preds;
#endif /* LOW_PROF */
  struct reduction_counters call_counters;
  char *yap_lib_dir;
  Agc_hook  agc_hook;
  void *foreign_code_loaded;
  ADDR  foreign_code_base;
  ADDR  foreign_code_top;
  ADDR  foreign_code_max;
  int parser_error_style;
  int   compiler_profiling;
  int   compiler_call_counting;
  /********* whether we should try to compile array references ******************/
  int   compiler_compile_arrays;
  /*
    PREG just before we enter $spy. We use that to find out the clause which
    was calling the debugged goal.
  */
  yamop   *debugger_p_before_spy;
#if defined(YAPOR) || defined(TABLING)
  struct global_data global;
  struct local_data remote[MAX_WORKERS];
#endif /* YAPOR || TABLING */
#ifdef THREADS
  lockvar  thread_handles_lock;        /* protect ThreadManipulation */
  struct thandle thread_handle[MAX_WORKERS];
#endif
  UInt n_of_atoms;
  UInt atom_hash_table_size;
  AtomHashEntry *hash_chain;
} all_heap_codes;

#ifdef USE_SYSTEM_MALLOC
struct various_codes *heap_regs;
#else
#define heap_regs  ((all_heap_codes *)HEAP_INIT_BASE)
#endif

#define  HeapUsed                heap_regs->heap_used
#define  HeapMax                 heap_regs->heap_max
#define  HeapTop                 heap_regs->heap_top
#define  HeapLim                 heap_regs->heap_lim
#ifdef YAPOR
#define  SEQUENTIAL_IS_DEFAULT   heap_regs->seq_def
#define  GETWORK		 (&(heap_regs->getworkcode               ))
#define  GETWORK_SEQ             (&(heap_regs->getworkcode_seq           ))
#define  GETWORK_FIRST_TIME      (&(heap_regs->getworkfirsttimecode      ))
#endif /* YAPOR */
#ifdef TABLING
#define  COMPLETION               ((yamop *)&(heap_regs->tablecompletioncode   ))
#define  ANSWER_RESOLUTION        ((yamop *)&(heap_regs->tableanswerresolutioncode ))
#endif /* TABLING */
#define  EXPAND_OP_CODE           heap_regs->expand_op_code
#define  ExpandClausesFirst       heap_regs->expand_clauses_first
#define  ExpandClausesLast        heap_regs->expand_clauses_last
#define  ExpandClausesListLock    heap_regs->expand_clauses_list_lock
#define  COMMA_CODE               heap_regs->comma_code
#define  FAILCODE                 heap_regs->failcode
#define  TRUSTFAILCODE            heap_regs->trustfailcode
#define  YESCODE                  heap_regs->yescode
#define  NOCODE                   heap_regs->nocode
#define  RTRYCODE                 heap_regs->rtrycode
#define  DUMMYCODE                heap_regs->dummycode
#define  CLAUSECODE               heap_regs->clausecode
#define  INVISIBLECHAIN           heap_regs->invisiblechain
#define  max_depth                heap_regs->maxdepth
#define  max_list                 heap_regs->maxlist
#define  AtPrompt                 (&(heap_regs->atprompt    	         ))
#define  Prompt                   heap_regs->prompt
#if USE_THREADED_CODE
#define  OP_RTABLE                heap_regs->op_rtable
#endif
#define  PROFILING                heap_regs->system_profiling
#define  CALL_COUNTING            heap_regs->system_call_counting
#define  PRED_GOAL_EXPANSION_ON   heap_regs->system_pred_goal_expansion_on
#define  UPDATE_MODE              heap_regs->update_mode
#define  RETRY_C_RECORDED_CODE    heap_regs->retry_recorded_code
#define  RETRY_C_RECORDED_K_CODE  heap_regs->retry_recorded_k_code
#define  RETRY_C_RECORDEDP_CODE   heap_regs->retry_c_recordedp_code
#define  STATIC_PREDICATES_MARKED heap_regs->static_predicates_marked
#define  yap_flags                heap_regs->yap_flags_field
#define  UNDEF_OPCODE             heap_regs->undef_op
#define  INDEX_OPCODE             heap_regs->index_op
#define  FAIL_OPCODE              heap_regs->fail_op
#ifdef THREADS
#define  ThreadHandlesLock	  heap_regs->thread_handles_lock
#define  ThreadHandle		  heap_regs->thread_handle
#endif
#define  NOfAtoms                 heap_regs->n_of_atoms
#define  AtomHashTableSize        heap_regs->atom_hash_table_size
#define  HashChain                heap_regs->hash_chain
#define  INT_KEYS_SIZE            heap_regs->int_keys_size
#define  INT_KEYS_TIMESTAMP       heap_regs->int_keys_timestamp
#define  INT_KEYS                 heap_regs->IntKeys
#define  INT_LU_KEYS_SIZE         heap_regs->int_lu_keys_size
#define  INT_LU_KEYS_TIMESTAMP    heap_regs->int_lu_keys_timestamp
#define  INT_LU_KEYS              heap_regs->IntLUKeys
#define  INT_BB_KEYS_SIZE         heap_regs->int_bb_keys_size
#define  INT_BB_KEYS              heap_regs->IntBBKeys
#define  CharConversionTable      heap_regs->char_conversion_table
#define  CharConversionTable2     heap_regs->char_conversion_table2
#define  ModuleName               heap_regs->module_name
#define  ModulePred               heap_regs->module_pred
#define  NoOfModules              heap_regs->no_of_modules
#define  AtomAbol                 heap_regs->atom_abol
#define  AtomAlarm                heap_regs->atom_alarm
#define  AtomAppend               heap_regs->atom_append
#define  AtomArray                heap_regs->atom_array
#define  AtomAssert               heap_regs->atom_assert
#define  AtomB                    heap_regs->atom_b
#define  AtomBreak                heap_regs->atom_break
#define  AtomCall                 heap_regs->atom_call
#define  AtomCatch                heap_regs->atom_catch
#define  AtomComma                heap_regs->atom_comma
#define  AtomCpuTime              heap_regs->atom_cpu_time
#define  AtomCsult                heap_regs->atom_csult
#define  AtomCut                  heap_regs->atom_cut
#define  AtomCutBy                heap_regs->atom_cut_by
#if defined(EUROTRA) && defined(SFUNC)
#define  AtomDollarUndef          heap_regs->atom_dollar_undef
#endif
#define  AtomDBRef                heap_regs->atom_dbref
#define  AtomE                    heap_regs->atom_e
#define  AtomEQ                   heap_regs->atom_e_q
#define  AtomEof                  heap_regs->atom_eof
#ifdef EUROTRA
#define  AtomFB                   heap_regs->atom_f_b
#endif
#define  AtomFail                 heap_regs->atom_fail
#define  AtomFalse                heap_regs->atom_false
#define  AtomFast                 heap_regs->atom_fast
#define  AtomGT                   heap_regs->atom_g_t
#define  AtomGc                   heap_regs->atom_gc
#define  AtomGcMargin             heap_regs->atom_gc_margin
#define  AtomGcTrace              heap_regs->atom_gc_trace
#define  AtomGcVerbose            heap_regs->atom_gc_verbose
#define  AtomGcVeryVerbose        heap_regs->atom_gc_very_verbose
#define  AtomGlobal               heap_regs->atom_global
#define  AtomHeapUsed             heap_regs->atom_heap_used
#define  AtomInf                  heap_regs->atom_inf
#define  AtomLocal                heap_regs->atom_local
#define  AtomLT                   heap_regs->atom_l_t
#define  AtomMetaCall             heap_regs->atom_meta_call
#define  AtomMinus                heap_regs->atom_minus
#define  AtomMultiFile            heap_regs->atom_multi_file
#define  AtomNan                  heap_regs->atom_nan
#define  AtomOtherwise            heap_regs->atom_otherwise
#define  AtomPi                   heap_regs->atom_pi
#define  AtomPlus                 heap_regs->atom_plus
#define  AtomPortray              heap_regs->atom_portray
#define  AtomProfile              heap_regs->atom_profile
#define  AtomRandom               heap_regs->atom_random
#define  AtomRead                 heap_regs->atom_read
#define  AtomRepeat               heap_regs->atom_repeat
#define  AtomRestoreRegs          heap_regs->atom_restore_regs
#if HAVE_SIGACTION
#define  AtomSigPending           heap_regs->atom_sig_pending
#endif
#define  AtomStackFree            heap_regs->atom_stack_free
#define  AtomTrue                 heap_regs->atom_true
#define  AtomUser                 heap_regs->atom_user
#define  AtomUsrErr               heap_regs->atom_usr_err
#define  AtomUsrIn                heap_regs->atom_usr_in
#define  AtomUsrOut               heap_regs->atom_usr_out
#define  AtomVersionNumber        heap_regs->atom_version_number
#define  AtomWrite                heap_regs->atom_write
#ifdef   USE_SOCKET
#define  FunctorAfInet            heap_regs->functor_af_inet
#define  FunctorAfLocal           heap_regs->functor_af_local
#define  FunctorAfUnix            heap_regs->functor_af_unix
#endif
#define  FunctorAltNot            heap_regs->functor_alt_not
#ifdef COROUTINING
#define  FunctorArrayEntry        heap_regs->functor_array_entry
#endif
#define  FunctorArrow             heap_regs->functor_arrow
#define  FunctorAssert            heap_regs->functor_assert
#ifdef COROUTINING
#define  FunctorAttGoal           heap_regs->functor_att_goal
#endif
#define  FunctorBraces            heap_regs->functor_braces
#define  FunctorCall              heap_regs->functor_call
#define  FunctorClist             heap_regs->functor_clist
#define  FunctorComma             heap_regs->functor_comma
#define  FunctorCreep             heap_regs->functor_creep
#define  FunctorCsult             heap_regs->functor_csult
#define  FunctorCutBy             heap_regs->functor_cut_by
#define  FunctorEq                heap_regs->functor_eq
#define  FunctorExecuteInMod      heap_regs->functor_execute_in_mod
#define  FunctorExecuteWithin     heap_regs->functor_execute_within
#define  FunctorGAtom             heap_regs->functor_g_atom
#define  FunctorGAtomic           heap_regs->functor_g_atomic
#define  FunctorGCompound         heap_regs->functor_g_compound
#define  FunctorGFloat            heap_regs->functor_g_float
#define  FunctorGInteger          heap_regs->functor_g_integer
#define  FunctorGNumber           heap_regs->functor_g_number
#define  FunctorGPrimitive        heap_regs->functor_g_primitive
#define  FunctorGVar              heap_regs->functor_g_var
#define  FunctorLastExecuteWithin     heap_regs->functor_last_execute_within
#define  FunctorList              heap_regs->functor_list
#define  FunctorMegaClause        heap_regs->functor_mega_clause
#define  FunctorModule            heap_regs->functor_module
#define  FunctorMultiFileClause   heap_regs->functor_multi_file_clause
#ifdef MULTI_ASSIGNMENT_VARIABLES
#define  FunctorMutable           heap_regs->functor_mutable
#endif
#define  FunctorNot               heap_regs->functor_not
#define  FunctorOr                heap_regs->functor_or
#define  FunctorPortray           heap_regs->functor_portray
#define  FunctorQuery             heap_regs->functor_query
#define  FunctorStaticClause      heap_regs->functor_static_clause
#define  FunctorStream            heap_regs->functor_stream
#define  FunctorStreamPos         heap_regs->functor_stream_pos
#define  FunctorStreamEOS         heap_regs->functor_stream_eOS
#define  FunctorThreadRun         heap_regs->functor_thread_run
#define  FunctorChangeModule      heap_regs->functor_change_module
#define  FunctorCurrentModule     heap_regs->functor_current_module
#define  FunctorModSwitch         heap_regs->functor_mod_switch
#define  FunctorUMinus            heap_regs->functor_u_minus
#define  FunctorUPlus             heap_regs->functor_u_plus
#define  FunctorVBar              heap_regs->functor_v_bar
#define  FunctorVar               heap_regs->functor_var
#define  TermDollarU              heap_regs->term_dollar_u
#define  TermProlog               heap_regs->term_prolog
#define  TermReFoundVar           heap_regs->term_refound_var
#define  PROLOG_MODULE            0
#define  USER_MODULE              heap_regs->user_module
#define  IDB_MODULE               heap_regs->idb_module
#define  ATTRIBUTES_MODULE        heap_regs->attributes_module
#define  CHARSIO_MODULE           heap_regs->charsio_module
#define  TERMS_MODULE             heap_regs->terms_module
#define  PredGoalExpansion        heap_regs->pred_goal_expansion
#define  PredMetaCall             heap_regs->pred_meta_call
#define  PredDollarCatch          heap_regs->pred_dollar_catch
#define  PredRecordedWithKey      heap_regs->pred_recorded_with_key
#define  PredLogUpdClause         heap_regs->pred_log_upd_clause
#define  PredLogUpdClause0        heap_regs->pred_log_upd_clause0
#define  PredStaticClause         heap_regs->pred_static_clause
#define  PredThrow                heap_regs->pred_throw
#define  PredHandleThrow          heap_regs->pred_handle_throw
#define  DynArrayList             heap_regs->dyn_array_list
#define  DBErasedMarker           heap_regs->db_erased_marker
#ifdef DEBUG
#define  DBErasedList             heap_regs->db_erased_list
#define  DBErasedIList            heap_regs->db_erased_ilist
#define  Yap_expand_clauses_sz    heap_regs->expand_clauses_sz
#endif /* DEBUG */
#define  Stream		          heap_regs->yap_streams
#define  output_msg	          heap_regs->debugger_output_msg
#define  NOfFileAliases           heap_regs->n_of_file_aliases
#define  SzOfFileAliases          heap_regs->sz_of_file_aliases
#define  FileAliases              heap_regs->file_aliases
#if LOW_PROF
#define  ProfilerOn		  heap_regs->profiler_on
#define  FProf     		  ((FILE *)heap_regs->f_prof)
#define  FPreds     		  ((FILE *)heap_regs->f_preds)
#define  ProfPreds		  heap_regs->prof_preds
#endif /* LOW_PROF */
#define  ReductionsCounter        heap_regs->call_counters.reductions
#define  PredEntriesCounter       heap_regs->call_counters.reductions_retries
#define  RetriesCounter           heap_regs->call_counters.retries
#define  ReductionsCounterOn      heap_regs->call_counters.reductions_on
#define  PredEntriesCounterOn     heap_regs->call_counters.reductions_retries_on
#define  RetriesCounterOn         heap_regs->call_counters.retries_on
#define  Yap_LibDir               heap_regs->yap_lib_dir
#define  AGCHook                  heap_regs->agc_hook
#define  ParserErrorStyle         heap_regs->parser_error_style
#ifdef  COROUTINING
#define  WakeUpCode               heap_regs->wake_up_code
#endif
#if defined(YAPOR) || defined(THREADS)
#define  SignalLock               heap_regs->wl[worker_id].signal_lock
#define  WPP                      heap_regs->wl[worker_id].wpp
#define  ActiveSignals            heap_regs->wl[worker_id].active_signals
#define  DelayedTrace	          heap_regs->wl[worker_id].delayed_trace
#define  IPredArity               heap_regs->wl[worker_id].i_pred_arity
#define  ProfEnd                  heap_regs->wl[worker_id].prof_end
#define  StartLine                heap_regs->wl[worker_id].start_line
#define  ScratchPad               heap_regs->wl[worker_id].scratchpad
#ifdef  COROUTINING
#define  WokenGoals               heap_regs->wl[worker_id].woken_goals
#define  MutableList              heap_regs->wl[worker_id].mutable_list
#define  AttsMutableList          heap_regs->wl[worker_id].atts_mutable_list
#endif
#define  GcCalls                  heap_regs->wl[worker_id].gc_calls
#define  TotGcTime                heap_regs->wl[worker_id].tot_gc_time
#define  TotGcRecovered           heap_regs->wl[worker_id].tot_gc_recovered
#define  Yap_gc_restore           heap_regs->wl[worker_id].gc_restore
#define  Yap_old_TR               heap_regs->wl[worker_id].old_TR
#else
#define  ActiveSignals            heap_regs->wl.active_signals
#define  DelayedTrace	          heap_regs->wl.delayed_trace
#define  IPredArity               heap_regs->wl.i_pred_arity
#define  ProfEnd                  heap_regs->wl.prof_end
#define  StartLine                heap_regs->wl.start_line
#define  ScratchPad               heap_regs->wl.scratchpad
#ifdef  COROUTINING
#define  WokenGoals               heap_regs->wl.woken_goals
#define  MutableList              heap_regs->wl.mutable_list
#define  AttsMutableList          heap_regs->wl.atts_mutable_list
#endif
#define  GcCalls                  heap_regs->wl.gc_calls
#define  TotGcTime                heap_regs->wl.tot_gc_time
#define  TotGcRecovered           heap_regs->wl.tot_gc_recovered
#define  Yap_gc_restore           heap_regs->wl.gc_restore
#define  Yap_old_TR               heap_regs->wl.old_TR
#endif
#define  profiling                heap_regs->compiler_profiling
#define  call_counting            heap_regs->compiler_call_counting
#define  compile_arrays           heap_regs->compiler_compile_arrays
#define  optimizer_on             heap_regs->compiler_optimizer_on
#define  compile_mode             heap_regs->compiler_compile_mode
#define  P_before_spy             heap_regs->debugger_p_before_spy
#define  ForeignCodeBase          heap_regs->foreign_code_base;
#define  ForeignCodeTop           heap_regs->foreign_code_top;
#define  ForeignCodeMax           heap_regs->foreign_code_max;
#define  ForeignCodeLoaded        heap_regs->foreign_code_loaded
#define  ParserErrorStyle         heap_regs->parser_error_style
#define  DeadClauses              heap_regs->dead_clauses
#define  SizeOfOverflow           heap_regs->size_of_overflow
#define  LastWtimePtr             heap_regs->last_wtime
#define  BGL			  heap_regs->bgl
#define  FreeBlocks		  heap_regs->free_blocks
#if defined(YAPOR) || defined(THREADS)
#define  FreeBlocksLock           heap_regs->free_blocks_lock
#define  HeapTopLock              heap_regs->heap_top_lock
#define  HeapTopOwner             heap_regs->heap_top_owner
#define  NOfThreads               heap_regs->n_of_threads
#define  NOfThreadsCreated        heap_regs->n_of_threads_created
#define  ThreadsTotalTime         heap_regs->threads_total_time
#define  HeapUsedLock             heap_regs->heap_used_lock
#define  DeadClausesLock          heap_regs->dead_clauses_lock
#endif
#define  CreepCode                heap_regs->creep_code
#define  UndefCode                heap_regs->undef_code
#define  SpyCode                  heap_regs->spy_code
#if defined(YAPOR) || defined(TABLING)
#define  GLOBAL		          heap_regs->global
#define  REMOTE                   heap_regs->remote
#endif /* YAPOR || TABLING */

#define UPDATE_MODE_IMMEDIATE          0
#define UPDATE_MODE_LOGICAL            1
#define UPDATE_MODE_LOGICAL_ASSERT     2


#ifdef COROUTINING
#define  NUM_OF_ATTS              heap_regs->num_of_atts
#endif

/* initially allow for files with up to 1024 predicates. This number
   is extended whenever needed */
#define  InitialConsultCapacity    1024
/* current consult stack */
#define  ConsultSp                (heap_regs->consultsp                   )
/* top of consult stack  */
#define  ConsultBase              (heap_regs->consultbase                 )
/* low-water mark for consult  */
#define  ConsultLow               (heap_regs->consultlow	          )
/* current maximum number of cells in consult stack */
#define  ConsultCapacity          (heap_regs->consultcapacity             )
#if HAVE_LIBREADLINE
#define  ReadlineBuf              heap_regs->readline_buf
#define  ReadlinePos              heap_regs->readline_pos
#endif


ADDR    STD_PROTO(Yap_ExpandPreAllocCodeSpace, (UInt));
#define Yap_ReleasePreAllocCodeSpace(x)
#if USE_SYSTEM_MALLOC
ADDR    STD_PROTO(Yap_InitPreAllocCodeSpace, (void));
EXTERN inline ADDR
Yap_PreAllocCodeSpace(void)
{
  ADDR ptr = ScratchPad.ptr;
  if (ptr) return ptr;
  return Yap_InitPreAllocCodeSpace();
}
#else
EXTERN inline ADDR
Yap_PreAllocCodeSpace(void)
{
  return Addr(HeapTop) + sizeof(CELL);
}
#endif

#if THREADS
Prop STD_PROTO(Yap_NewThreadPred, (PredEntry *));
Prop STD_PROTO(Yap_NewPredPropByFunctor, (Functor, Term));

EXTERN inline PredEntry *
Yap_GetThreadPred(PredEntry *ap)
{
  Functor f = ap->FunctorOfPred;
  Term  mod = ap->ModuleOfPred;
  Prop p0 = AbsPredProp(heap_regs->thread_handle[worker_id].local_preds);

  while(p0) {
    PredEntry *ap = RepPredProp(p0);
    if (ap->FunctorOfPred == f &&
	ap->ModuleOfPred == mod) return ap;
    p0 = ap->NextOfPE;
  }
  return RepPredProp(Yap_NewThreadPred(ap));
}
#endif

#endif /* HEAP_H */
