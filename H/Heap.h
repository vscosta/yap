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
* version:      $Id: Heap.h,v 1.43 2003-06-06 11:54:01 vsc Exp $	 *
*************************************************************************/

/* information that can be stored in Code Space */

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

typedef int   (*Agc_hook)(Atom);

typedef struct various_codes {
  special_functors funcs;
  Int heap_used;
  Int heap_max;
  ADDR heap_top;
  struct FREEB  *free_blocks;
#if defined(YAPOR) || defined(THREADS)
  lockvar  free_blocks_lock;     /* protect the list of free blocks */
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
#ifdef MULTI_ASSIGNMENT_VARIABLES
  Term   woken_goals;
  Term   mutable_list;
  Term   atts_mutable_list;
#endif
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
  struct pred_entry   *compiler_current_pred;
  AtomHashEntry invisiblechain;
  OPCODE dummycode[1];
  UInt maxdepth, maxlist;
  int update_mode;
  Atom atprompt;
  char prompt[MAX_PROMPT];
  OPCODE undef_op; 
  OPCODE index_op; 
  OPCODE fail_op; 
  yamop *retry_recorded_code,
    *retry_recorded_k_code,
    *retry_drecorded_code,
    *retry_c_recordedp_code;
  Int static_predicates_marked;
  UInt int_keys_size;
  UInt int_keys_timestamp;
  Prop *IntKeys;
  UInt int_bb_keys_size;
  Prop *IntBBKeys;
  Int   yap_flags_field[NUMBER_OF_YAP_FLAGS];
  char *char_conversion_table;
  char *char_conversion_table2;
#if defined(YAPOR) || defined(THREADS)
  lockvar  heap_used_lock;        /* protect HeapUsed */
  lockvar  heap_top_lock;        /* protect HeapTop */
  lockvar  dead_clauses_lock;        /* protect DeadClauses */
  int      heap_top_owner;
  unsigned int n_of_threads;      /* number of threads and processes in system */
#endif
  unsigned int size_of_overflow;
  Term  module_name[MaxModules];
  struct pred_entry *module_pred[MaxModules];
  SMALLUNSGN   no_of_modules;
  struct dead_clause *dead_clauses;
  int   primitives_module;
  int   user_module;
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
    functor_module,
#ifdef MULTI_ASSIGNMENT_VARIABLES
    functor_mutable,
#endif
    functor_not,
    functor_or,
    functor_portray,
    functor_query,
    functor_spy,
    functor_stream,
    functor_stream_pos,
    functor_stream_eOS,
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
    term_refound_var;
  void *last_wtime;
  struct pred_entry *pred_goal_expansion;
  struct pred_entry *pred_meta_call;
  struct pred_entry *pred_dollar_catch;
  struct pred_entry *pred_throw;
  struct pred_entry *pred_handle_throw;
  struct array_entry *dyn_array_list;
  struct DB_STRUCT *db_erased_marker;
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
  int   profiler_pred_count;
  UInt  prof_calls;
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
  char *compiler_freep;
  char *compiler_freep0;
  struct PSEUDO *compiler_cpc;
  struct PSEUDO *compiler_CodeStart;
  struct PSEUDO *compiler_icpc;
  struct PSEUDO *compiler_BlobsStart;
  int  *compiler_label_offset;
  UInt   i_pred_arity;
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
#endif
  AtomHashEntry hash_chain[MaxHash];
} all_heap_codes;

#define heap_regs  ((all_heap_codes *)HEAP_INIT_BASE)

#define  HeapUsed                heap_regs->heap_used
#define  HeapMax                 heap_regs->heap_max
#define  HeapTop            heap_regs->heap_top
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
#define  RETRY_C_DRECORDED_CODE   heap_regs->retry_drecorded_code
#define  RETRY_C_RECORDEDP_CODE   heap_regs->retry_c_recordedp_code
#define  STATIC_PREDICATES_MARKED heap_regs->static_predicates_marked
#define  yap_flags                heap_regs->yap_flags_field
#define  UNDEF_OPCODE             heap_regs->undef_op
#define  INDEX_OPCODE             heap_regs->index_op
#define  FAIL_OPCODE              heap_regs->fail_op
#define  HashChain                heap_regs->hash_chain
#define  INT_KEYS_SIZE            heap_regs->int_keys_size
#define  INT_KEYS_TIMESTAMP       heap_regs->int_keys_timestamp
#define  INT_KEYS                 heap_regs->IntKeys
#define  INT_BB_KEYS_SIZE         heap_regs->int_bb_keys_size
#define  INT_BB_KEYS              heap_regs->IntBBKeys
#define  CharConversionTable      heap_regs->char_conversion_table
#define  CharConversionTable2     heap_regs->char_conversion_table2
#define  ModuleName               heap_regs->module_name
#define  ModulePred               heap_regs->module_pred
#define  PrimitivesModule         heap_regs->primitives_module
#define  UserModule               heap_regs->user_module
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
#define  FunctorModule            heap_regs->functor_module
#ifdef MULTI_ASSIGNMENT_VARIABLES
#define  FunctorMutable           heap_regs->functor_mutable
#endif
#define  FunctorNot               heap_regs->functor_not
#define  FunctorOr                heap_regs->functor_or
#define  FunctorPortray           heap_regs->functor_portray
#define  FunctorQuery             heap_regs->functor_query
#define  FunctorSpy               heap_regs->functor_spy
#define  FunctorStream            heap_regs->functor_stream
#define  FunctorStreamPos         heap_regs->functor_stream_pos
#define  FunctorStreamEOS         heap_regs->functor_stream_eOS
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
#define  PredGoalExpansion        heap_regs->pred_goal_expansion
#define  PredMetaCall             heap_regs->pred_meta_call
#define  PredDollarCatch          heap_regs->pred_dollar_catch
#define  PredThrow                heap_regs->pred_throw
#define  PredHandleThrow          heap_regs->pred_handle_throw
#define  DynArrayList             heap_regs->dyn_array_list
#define  DBErasedMarker           heap_regs->db_erased_marker
#define  Stream		          heap_regs->yap_streams
#define  output_msg	          heap_regs->debugger_output_msg
#define  NOfFileAliases           heap_regs->n_of_file_aliases
#define  SzOfFileAliases          heap_regs->sz_of_file_aliases
#define  FileAliases              heap_regs->file_aliases
#if LOW_PROF
#define  ProfilerOn		  heap_regs->profiler_on
#define  FProf     		  ((FILE *)heap_regs->f_prof)
#define  FPreds     		  ((FILE *)heap_regs->f_preds)
#define  ProfilerPredCount	  heap_regs->profiler_pred_count
#define  ProfCalls		  heap_regs->prof_calls
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
#define  freep                    heap_regs->compiler_freep
#define  freep0                   heap_regs->compiler_freep0
#define  cpc                      heap_regs->compiler_cpc
#define  CodeStart                heap_regs->compiler_CodeStart
#define  icpc                     heap_regs->compiler_icpc
#define  BlobsStart               heap_regs->compiler_BlobsStart
#define  label_offset             heap_regs->compiler_label_offset
#define  IPredArity               heap_regs->i_pred_arity
#define  profiling                heap_regs->compiler_profiling
#define  call_counting            heap_regs->compiler_call_counting
#define  compile_arrays           heap_regs->compiler_compile_arrays
#define  optimizer_on             heap_regs->compiler_optimizer_on
#define  compile_mode             heap_regs->compiler_compile_mode
#define  P_before_spy             heap_regs->debugger_p_before_spy
#define  CurrentPred              heap_regs->compiler_current_pred
#define  ForeignCodeBase          heap_regs->foreign_code_base;
#define  ForeignCodeTop           heap_regs->foreign_code_top;
#define  ForeignCodeMax           heap_regs->foreign_code_max;
#define  ForeignCodeLoaded        heap_regs->foreign_code_loaded
#define  ParserErrorStyle         heap_regs->parser_error_style
#define  DeadClauses              heap_regs->dead_clauses
#define  SizeOfOverflow           heap_regs->size_of_overflow
#define  LastWtimePtr             heap_regs->last_wtime
#ifdef  COROUTINING
#define  WakeUpCode               heap_regs->wake_up_code
#define  WokenGoals               heap_regs->woken_goals
#define  MutableList              heap_regs->mutable_list
#define  AttsMutableList          heap_regs->atts_mutable_list
#endif
#if defined(YAPOR) || defined(THREADS)
#define  FreeBlocksLock           heap_regs->free_blocks_lock
#define  HeapTopLock              heap_regs->heap_top_lock
#define  HeapTopOwner             heap_regs->heap_top_owner
#define  NOfThreads               heap_regs->n_of_threads
#define  HeapUsedLock             heap_regs->heap_used_lock
#define  DeadClausesLock          heap_regs->dead_clauses_lock
#endif
#define  CreepCode                heap_regs->creep_code
#define  UndefCode                heap_regs->undef_code
#define  SpyCode                  heap_regs->spy_code
#if defined(YAPOR) || defined(TABLING)
#define  REMOTE                   heap_regs->remote
#define  GLOBAL		          heap_regs->global
#endif

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


