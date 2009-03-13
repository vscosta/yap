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
* version:      $Id: Heap.h,v 1.136 2008-08-08 14:05:34 vsc Exp $	 *
*************************************************************************/

/* information that can be stored in Code Space */
#include <stdio.h>

#ifndef HEAP_H
#define HEAP_H 1

#if defined(YAPOR) || defined(THREADS)
#define WL wl[worker_id]
#else
#define WL wl
#endif

#if defined(THREADS)
#define RINFO rinfo[worker_id]
#else
#define RINFO rinfo
#endif

#ifndef INT_KEYS_DEFAULT_SIZE
#define INT_KEYS_DEFAULT_SIZE 256
#endif

#if USE_DL_MALLOC

#define MAX_DLMALLOC_HOLES 32

typedef struct memory_hole {
  ADDR start;
  ADDR end;
} memory_hole_type;
#endif


#define GC_MAVARS_HASH_SIZE 512

typedef struct gc_ma_hash_entry_struct {
  UInt timestmp;
#ifdef TABLING
  tr_fr_ptr loc;
  struct gc_ma_hash_entry_struct *more;
#endif
  CELL* addr;
  struct gc_ma_hash_entry_struct *next;
} gc_ma_hash_entry;

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

typedef struct restore_info {
  Int cl_diff,
    g_diff,
    g_diff0,
    h_diff,
    l_diff,
    tr_diff,
    x_diff,
    delay_diff;
  CELL    *old_ASP, *old_LCL0;
  CELL    *g_split;
  tr_fr_ptr old_TR;  
  CELL    *old_GlobalBase, *old_H, *old_H0;
  CELL    *old_DelayTop, *current_DelayTop;
  ADDR     old_TrailBase, old_TrailTop;
  ADDR     old_HeapBase, old_HeapTop;
} restoreinfo;

#if defined(THREADS)
extern struct restore_info rinfo[MAX_THREADS];
#else
extern struct restore_info rinfo;
#endif

typedef struct worker_local_struct {
  struct format_status *f_info;
  Int delay_arena_overflows;
  Int arena_overflows;
  Int depth_arenas;
  char *scanner_stack;
  struct scanner_extra_alloc *scanner_extra_blocks;
#if defined(YAPOR) || defined(THREADS)
  lockvar  signal_lock;        /* protect signal handlers from IPIs */
  struct pred_entry *wpp;
#endif
#ifdef USE_GMP
  mpz_t  big_tmp;
#endif
  union CONSULT_OBJ *consultsp;
  union CONSULT_OBJ *consultbase;
  union CONSULT_OBJ *consultlow;
  struct pred_entry *last_asserted_pred;
  int    debug_on;
  UInt   consultcapacity;
  UInt   active_signals;
  UInt   i_pred_arity;
  yamop *prof_end;
  Int    start_line;
  int    uncaught_throw;
  int    doing_undefp;
  scratch_block scratchpad;
#ifdef MULTI_ASSIGNMENT_VARIABLES
  Term   woken_goals;
  Term   atts_mutable_list;
#endif
  /* gc_stuff */
  Term     gc_generation;	/* global stack limit at last generation */ 
  Term     gc_phase;	        /* gc phase to be sure we are on a valid compression */ 
  UInt     gc_current_phase;    /* gc currrent phase */ 
  unsigned int      gc_calls;	/* number of times GC has been called */ 
  Int      tot_gc_time; /* total time spent in GC */
  YAP_ULONG_LONG      tot_gc_recovered; /* number of heap objects in all garbage collections */
  Int      last_gc_time, last_ss_time; /* number of heap objects in all garbage collections */
/* in a single gc */
#if defined(YAPOR) || defined(THREADS)
  /* otherwise, use global variables for speed */
  unsigned long int   tot_marked, tot_oldies;	/* number of heap objects marked */
#ifdef COROUTINING
  unsigned long int   tot_smarked;
#endif
  struct choicept *wl_current_B;
#if defined(TABLING) || defined(SBA)
  struct trail_frame *wl_sTR, *wl_sTR0;
  struct trail_frame *new_tr;
#else
  Term *wl_sTR, *wl_sTR0;
  Term *new_tr;
#endif
  CELL *wl_prev_HB;
  CELL *hgen;
  CELL **ip_top;
#if GC_NO_TAGS
  char *b_p;
#endif
  struct gc_mark_continuation *conttop0;
  struct gc_mark_continuation *conttop;
  int disc_trail_entries;
  gc_ma_hash_entry Gc_ma_hash_table[GC_MAVARS_HASH_SIZE];
  gc_ma_hash_entry *Gc_ma_h_top;
  gc_ma_hash_entry *Gc_ma_h_list;
  UInt Gc_timestamp;    /* an unsigned int */
  ADDR  DB_vec, DB_vec0;
  struct RB_red_blk_node *DB_root, *DB_nil;
#endif /* defined(YAPOR) || defined(THREADS) */
  jmp_buf  gc_restore; /* where to jump if garbage collection crashes */
  struct array_entry *dynamic_arrays;
  struct static_array_entry *static_arrays;
  struct global_entry *global_variables;
  int allow_restart;
  Term global_arena;
  UInt global_arena_overflows; 
  Term global_delay_arena;
  yamop trust_lu_code[3];
} worker_local;

#ifdef THREADS
typedef struct thandle {
  int in_use;
  int zombie;
  UInt ssize;
  UInt tsize;
  UInt sysize;
  void *stack_address;
  Term tdetach;
  Term  cmod, texit_mod;
  struct DB_TERM *tgoal, *texit;
  int id;
  int ret;
  REGSTORE *default_yaam_regs;
  REGSTORE *current_yaam_regs;
  struct pred_entry *local_preds;
  pthread_t handle;
  int ref_count;
#ifdef LOW_LEVEL_TRACER
  long long int thread_inst_count;
  int been_here1;
  int been_here2;
#endif
  pthread_mutex_t tlock;
  pthread_mutex_t tlock_status;
#if HAVE_GETRUSAGE
  struct timeval *start_of_timesp;
  struct timeval *last_timep;
#endif
} yap_thandle;
#endif

typedef int   (*Agc_hook)(Atom);

typedef struct various_codes {
  special_functors funcs;
  UInt hole_size;
  struct malloc_state *av_;

  UInt clause_space, index_space_Tree, index_space_EXT, index_space_SW;
  UInt lu_clause_space, lu_index_space_Tree, lu_index_space_CP, lu_index_space_EXT, lu_index_space_SW;
#if USE_DL_MALLOC
  struct memory_hole memory_holes[MAX_DLMALLOC_HOLES];
  UInt nof_memory_holes;
#endif
  Int heap_used;
  Int heap_max;
  ADDR heap_top;
  ADDR heap_lim;
  struct FREEB  *free_blocks;
#if defined(YAPOR) || defined(THREADS)
  lockvar  bgl;		 /* protect long critical regions   */
  lockvar  free_blocks_lock;     /* protect the list of free blocks */
  worker_local wl[MAX_AGENTS];
#else
  worker_local wl;
#endif
#ifdef BEAM
  yamop beam_retry_code;
#endif /* BEAM */
#ifdef YAPOR
  int seq_def;
  yamop getwork_code;
  yamop getwork_seq_code;
  yamop getwork_first_time_code;
#endif /* YAPOR */
#ifdef TABLING
  yamop table_load_answer_code;
  yamop table_try_answer_code;
  yamop table_answer_resolution_code;
  yamop table_completion_code;
#endif /* TABLING */
  OPCODE execute_cpred_op_code, expand_op_code;
  yamop *expand_clauses_first, *expand_clauses_last;
  UInt expand_clauses;
#if defined(YAPOR) || defined(THREADS)
  lockvar expand_clauses_list_lock;
  lockvar op_list_lock;
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
  struct yami yescode;
  struct yami nocode;
  struct yami rtrycode;
  struct {
    OPREG arity;
    struct yami *clause;
    Functor func;
  } clausecode[1];
#if HAVE_LIBREADLINE
  char *readline_buf, *readline_pos;
#endif
#if USE_THREADED_CODE
  opentry *op_rtable;
#endif
  struct udi_info *udi_control_blocks;
#ifdef COROUTINING
  int  num_of_atts;            /* max. number of attributes we have for a variable */
  struct pred_entry  *wake_up_code;
#endif
  struct pred_entry  *creep_code;
  struct pred_entry  *undef_code;
  struct pred_entry  *spy_code;
  UInt new_cps, live_cps, dirty_cps, freed_cps;
  int   system_profiling;
  int   system_call_counting;
  int   system_pred_goal_expansion_all;
  int   system_pred_goal_expansion_func;
  int   system_pred_goal_expansion_on;
  int   compiler_optimizer_on;
  int   compiler_compile_mode;
  AtomHashEntry invisiblechain;
  OPCODE dummycode[1];
  UInt maxdepth, maxlist, maxwriteargs;
  int update_mode;
  Atom atprompt;
  char prompt[MAX_PROMPT];
  OPCODE undef_op; 
  OPCODE index_op; 
  OPCODE lockpred_op; 
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
#if defined(THREADS) || defined(YAPOR)
  unsigned int n_of_threads;      /* number of threads and processes in system */
  unsigned int n_of_threads_created;      /* number of threads created since start */
  UInt  threads_total_time;      /* total run time for dead threads */
#endif
#if defined(YAPOR) || defined(THREADS)
  lockvar  heap_used_lock;        /* protect HeapUsed */
  lockvar  heap_top_lock;        /* protect HeapTop */
  lockvar  dead_static_clauses_lock;        /* protect DeadStaticClauses */
  lockvar  dead_mega_clauses_lock;        /* protect DeadMegaClauses */
  lockvar  dead_static_indices_lock;        /* protect DeadStaticIndices */
  lockvar  dbterms_list_lock;        /* protect DBTermList */
  int      heap_top_owner;
#ifdef LOW_LEVEL_TRACER
  lockvar  low_level_trace_lock;
#endif
#endif
  unsigned int size_of_overflow;
  struct mod_entry *current_modules;
  struct operator_entry *op_list;
  struct hold_entry *global_hold_entry;
  struct static_clause *dead_static_clauses;
  struct static_mega_clause *dead_mega_clauses;
  struct static_index *dead_static_indices;
  struct dbterm_list *dbterms_list;
#include "tatoms.h"
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
    terms_module,
    system_module,
    readutil_module,
    hacks_module,
    arg_module,
    globals_module,
    swi_module;
  void *last_wtime;
  struct pred_entry *pred_goal_expansion;
  struct pred_entry *pred_meta_call;
  struct pred_entry *pred_dollar_catch;
  struct pred_entry *pred_recorded_with_key;
  struct pred_entry *pred_log_upd_clause;
  struct pred_entry *pred_log_upd_clause_erase;
  struct pred_entry *pred_log_upd_clause0;
  struct pred_entry *pred_static_clause;
  struct pred_entry *pred_throw;
  struct pred_entry *pred_handle_throw;
  struct pred_entry *pred_is;
  struct DB_STRUCT *db_erased_marker;
  struct logic_upd_clause *logdb_erased_marker;
  struct logic_upd_clause *db_erased_list;
  struct logic_upd_index *db_erased_ilist;
  UInt expand_clauses_sz;
  struct stream_desc *yap_streams;
  int    debugger_output_msg;
  UInt n_of_file_aliases;
  UInt sz_of_file_aliases;
  struct AliasDescS * file_aliases;
#if LOW_PROF
  int   profiler_on;
  int   offline_profiler;
  FILE *f_prof, *f_preds;
  UInt  prof_preds;
#endif /* LOW_PROF */
  struct reduction_counters call_counters;
  char *yap_lib_dir;
  UInt      agc_last_call; /* amount of space recovered in all garbage collections */
  UInt      agc_threshold; /* amount of space recovered in all garbage collections */
  Agc_hook  agc_hook;
  void *foreign_code_loaded;
  ADDR  foreign_code_base;
  ADDR  foreign_code_top;
  ADDR  foreign_code_max;
  struct pred_entry **pred_hash;
#if defined(YAPOR) || defined(THREADS)
  rwlock_t pred_hash_rw_lock;
#endif
  UInt  preds_in_hash_table, pred_hash_table_size;
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
  struct thandle thread_handle[MAX_THREADS];
#endif
  UInt n_of_atoms;
  UInt atom_hash_table_size;
  UInt wide_atom_hash_table_size;
  UInt n_of_wide_atoms;
  AtomHashEntry *wide_hash_chain;
  AtomHashEntry *hash_chain;
} all_heap_codes;

#ifdef USE_SYSTEM_MALLOC
extern struct various_codes *Yap_heap_regs;
#else
#define Yap_heap_regs  ((all_heap_codes *)HEAP_INIT_BASE)
#endif

#define  Yap_HoleSize            Yap_heap_regs->hole_size
#define  Yap_av                  Yap_heap_regs->av_
#define  Yap_ClauseSpace         Yap_heap_regs->clause_space
#define  Yap_IndexSpace_Tree     Yap_heap_regs->index_space_Tree
#define  Yap_IndexSpace_EXT      Yap_heap_regs->index_space_EXT
#define  Yap_IndexSpace_SW       Yap_heap_regs->index_space_SW
#define  Yap_LUClauseSpace       Yap_heap_regs->lu_clause_space
#define  Yap_LUIndexSpace_Tree   Yap_heap_regs->lu_index_space_Tree
#define  Yap_LUIndexSpace_CP     Yap_heap_regs->lu_index_space_CP
#define  Yap_LUIndexSpace_EXT    Yap_heap_regs->lu_index_space_EXT
#define  Yap_LUIndexSpace_SW     Yap_heap_regs->lu_index_space_SW
#define  Yap_MemoryHoles         Yap_heap_regs->memory_holes
#define  Yap_NOfMemoryHoles      Yap_heap_regs->nof_memory_holes
#if USE_DL_MALLOC || (USE_SYSTEM_MALLOC && HAVE_MALLINFO)
#define  HeapUsed                Yap_givemallinfo()
#ifdef YAPOR
#define  HeapUsedLock            Yap_heap_regs->heap_used_lock
#endif
#else
#define  HeapUsed                Yap_heap_regs->heap_used
#define  HeapUsedLock            Yap_heap_regs->heap_used_lock
#endif
#define  HeapMax                 Yap_heap_regs->heap_max
#define  HeapTop                 Yap_heap_regs->heap_top
#define  HeapLim                 Yap_heap_regs->heap_lim
#ifdef YAPOR
#define  SEQUENTIAL_IS_DEFAULT   Yap_heap_regs->seq_def
#define  GETWORK		 (&(Yap_heap_regs->getwork_code))
#define  GETWORK_SEQ             (&(Yap_heap_regs->getwork_seq_code))
#define  GETWORK_FIRST_TIME      (&(Yap_heap_regs->getwork_first_time_code))
#endif /* YAPOR */
#ifdef BEAM
#define  BEAM_RETRY_CODE         ((yamop *)&(Yap_heap_regs->beam_retry_code)) 
#endif /* BEAM */
#ifdef TABLING
#define  LOAD_ANSWER              ((yamop *)&(Yap_heap_regs->table_load_answer_code))
#define  TRY_ANSWER               ((yamop *)&(Yap_heap_regs->table_try_answer_code))
#define  ANSWER_RESOLUTION        ((yamop *)&(Yap_heap_regs->table_answer_resolution_code))
#define  COMPLETION               ((yamop *)&(Yap_heap_regs->table_completion_code))
#endif /* TABLING */
#define  EXECUTE_CPRED_OP_CODE    Yap_heap_regs->execute_cpred_op_code
#define  EXPAND_OP_CODE           Yap_heap_regs->expand_op_code
#define  ExpandClausesFirst       Yap_heap_regs->expand_clauses_first
#define  ExpandClausesLast        Yap_heap_regs->expand_clauses_last
#define  ExpandClausesListLock    Yap_heap_regs->expand_clauses_list_lock
#define  Yap_ExpandClauses        Yap_heap_regs->expand_clauses
#define  OpListLock               Yap_heap_regs->op_list_lock
#define  COMMA_CODE               Yap_heap_regs->comma_code
#define  FAILCODE                 Yap_heap_regs->failcode
#define  TRUSTFAILCODE            Yap_heap_regs->trustfailcode
#define  YESCODE                  (&Yap_heap_regs->yescode)
#define  NOCODE                   (&Yap_heap_regs->nocode)
#define  RTRYCODE                 (&Yap_heap_regs->rtrycode)
#define  DUMMYCODE                Yap_heap_regs->dummycode
#define  CLAUSECODE               Yap_heap_regs->clausecode
#define  INVISIBLECHAIN           Yap_heap_regs->invisiblechain
#define  max_depth                Yap_heap_regs->maxdepth
#define  max_list                 Yap_heap_regs->maxlist
#define  max_write_args           Yap_heap_regs->maxwriteargs
#define  AtPrompt                 (&(Yap_heap_regs->atprompt    	         ))
#define  Prompt                   Yap_heap_regs->prompt
#if USE_THREADED_CODE
#define  OP_RTABLE                Yap_heap_regs->op_rtable
#endif
#define  UdiControlBlocks	  Yap_heap_regs->udi_control_blocks
#define  PROFILING                Yap_heap_regs->system_profiling
#define  CALL_COUNTING            Yap_heap_regs->system_call_counting
#define  PRED_GOAL_EXPANSION_ALL  Yap_heap_regs->system_pred_goal_expansion_all
#define  PRED_GOAL_EXPANSION_FUNC Yap_heap_regs->system_pred_goal_expansion_func
#define  PRED_GOAL_EXPANSION_ON   Yap_heap_regs->system_pred_goal_expansion_on
#define  UPDATE_MODE              Yap_heap_regs->update_mode
#define  RETRY_C_RECORDED_CODE    Yap_heap_regs->retry_recorded_code
#define  RETRY_C_RECORDED_K_CODE  Yap_heap_regs->retry_recorded_k_code
#define  RETRY_C_RECORDEDP_CODE   Yap_heap_regs->retry_c_recordedp_code
#define  STATIC_PREDICATES_MARKED Yap_heap_regs->static_predicates_marked
#define  yap_flags                Yap_heap_regs->yap_flags_field
#define  UNDEF_OPCODE             Yap_heap_regs->undef_op
#define  INDEX_OPCODE             Yap_heap_regs->index_op
#define  LOCKPRED_OPCODE          Yap_heap_regs->lockpred_op
#define  FAIL_OPCODE              Yap_heap_regs->fail_op
#ifdef THREADS
#define  ThreadHandlesLock	  Yap_heap_regs->thread_handles_lock
#define  ThreadHandle		  Yap_heap_regs->thread_handle
#endif
#define  NOfAtoms                 Yap_heap_regs->n_of_atoms
#define  AtomHashTableSize        Yap_heap_regs->atom_hash_table_size
#define  HashChain                Yap_heap_regs->hash_chain
#define  NOfWideAtoms             Yap_heap_regs->n_of_wide_atoms
#define  WideAtomHashTableSize    Yap_heap_regs->wide_atom_hash_table_size
#define  WideHashChain            Yap_heap_regs->wide_hash_chain
#define  INT_KEYS_SIZE            Yap_heap_regs->int_keys_size
#define  INT_KEYS_TIMESTAMP       Yap_heap_regs->int_keys_timestamp
#define  INT_KEYS                 Yap_heap_regs->IntKeys
#define  INT_LU_KEYS_SIZE         Yap_heap_regs->int_lu_keys_size
#define  INT_LU_KEYS_TIMESTAMP    Yap_heap_regs->int_lu_keys_timestamp
#define  INT_LU_KEYS              Yap_heap_regs->IntLUKeys
#define  INT_BB_KEYS_SIZE         Yap_heap_regs->int_bb_keys_size
#define  INT_BB_KEYS              Yap_heap_regs->IntBBKeys
#define  CharConversionTable      Yap_heap_regs->char_conversion_table
#define  CharConversionTable2     Yap_heap_regs->char_conversion_table2
#define  CurrentModules		  Yap_heap_regs->current_modules
#define  OpList                   Yap_heap_regs->op_list
#define  FloatFormat              Yap_heap_regs->float_format
#define  TermDollarU              Yap_heap_regs->term_dollar_u
#define  TermProlog               Yap_heap_regs->term_prolog
#define  TermReFoundVar           Yap_heap_regs->term_refound_var
#define  PROLOG_MODULE            0
#define  USER_MODULE              Yap_heap_regs->user_module
#define  IDB_MODULE               Yap_heap_regs->idb_module
#define  ATTRIBUTES_MODULE        Yap_heap_regs->attributes_module
#define  CHARSIO_MODULE           Yap_heap_regs->charsio_module
#define  TERMS_MODULE             Yap_heap_regs->terms_module
#define  SYSTEM_MODULE            Yap_heap_regs->system_module
#define  READUTIL_MODULE          Yap_heap_regs->readutil_module
#define  HACKS_MODULE             Yap_heap_regs->hacks_module
#define  GLOBALS_MODULE           Yap_heap_regs->globals_module
#define  ARG_MODULE	          Yap_heap_regs->arg_module
#define  SWI_MODULE               Yap_heap_regs->swi_module
#define  PredGoalExpansion        Yap_heap_regs->pred_goal_expansion
#define  PredMetaCall             Yap_heap_regs->pred_meta_call
#define  PredDollarCatch          Yap_heap_regs->pred_dollar_catch
#define  PredRecordedWithKey      Yap_heap_regs->pred_recorded_with_key
#define  PredLogUpdClause         Yap_heap_regs->pred_log_upd_clause
#define  PredLogUpdClauseErase    Yap_heap_regs->pred_log_upd_clause_erase
#define  PredLogUpdClause0        Yap_heap_regs->pred_log_upd_clause0
#define  PredStaticClause         Yap_heap_regs->pred_static_clause
#define  PredThrow                Yap_heap_regs->pred_throw
#define  PredHandleThrow          Yap_heap_regs->pred_handle_throw
#define  PredIs		          Yap_heap_regs->pred_is
#define  DBErasedMarker           Yap_heap_regs->db_erased_marker
#define  LogDBErasedMarker        Yap_heap_regs->logdb_erased_marker
#define  DBErasedList             Yap_heap_regs->db_erased_list
#define  DBErasedIList            Yap_heap_regs->db_erased_ilist
#define  Yap_expand_clauses_sz    Yap_heap_regs->expand_clauses_sz
#define  Stream		          Yap_heap_regs->yap_streams
#define  output_msg	          Yap_heap_regs->debugger_output_msg
#define  NOfFileAliases           Yap_heap_regs->n_of_file_aliases
#define  SzOfFileAliases          Yap_heap_regs->sz_of_file_aliases
#define  FileAliases              Yap_heap_regs->file_aliases
#if LOW_PROF
#define  ProfilerOn		  Yap_heap_regs->profiler_on
#define  Yap_OffLineProfiler	  Yap_heap_regs->offline_profiler
#define  FProf     		  Yap_heap_regs->f_prof
#define  FPreds     		  Yap_heap_regs->f_preds
#define  ProfPreds		  Yap_heap_regs->prof_preds
#endif /* LOW_PROF */
#define  ReductionsCounter        Yap_heap_regs->call_counters.reductions
#define  PredEntriesCounter       Yap_heap_regs->call_counters.reductions_retries
#define  RetriesCounter           Yap_heap_regs->call_counters.retries
#define  ReductionsCounterOn      Yap_heap_regs->call_counters.reductions_on
#define  PredEntriesCounterOn     Yap_heap_regs->call_counters.reductions_retries_on
#define  RetriesCounterOn         Yap_heap_regs->call_counters.retries_on
#define  Yap_LibDir               Yap_heap_regs->yap_lib_dir
#define  AGcLastCall              Yap_heap_regs->agc_last_call
#define  AGcThreshold             Yap_heap_regs->agc_threshold
#define  AGCHook                  Yap_heap_regs->agc_hook
#define  ParserErrorStyle         Yap_heap_regs->parser_error_style
#ifdef  COROUTINING
#define  WakeUpCode               Yap_heap_regs->wake_up_code
#endif
#if defined(YAPOR) || defined(THREADS)
#define  SignalLock               Yap_heap_regs->wl[worker_id].signal_lock
#define  WPP                      Yap_heap_regs->wl[worker_id].wpp
#define  total_marked             Yap_heap_regs->wl[worker_id].tot_marked
#define  total_oldies             Yap_heap_regs->wl[worker_id].tot_oldies
#if DEBUG
#ifdef COROUTINING
#define  total_smarked            Yap_heap_regs->wl[worker_id].tot_smarked
#endif
#endif /* DEBUG */
#define  current_B                Yap_heap_regs->wl[worker_id].wl_current_B
#define  sTR                      Yap_heap_regs->wl[worker_id].wl_sTR
#define  sTR0                     Yap_heap_regs->wl[worker_id].wl_sTR0
#define  prev_HB                  Yap_heap_regs->wl[worker_id].wl_prev_HB
#define  new_TR                   Yap_heap_regs->wl[worker_id].new_tr
#define  HGEN                     Yap_heap_regs->wl[worker_id].hgen
#define  iptop                    Yap_heap_regs->wl[worker_id].ip_top
#define  discard_trail_entries    Yap_heap_regs->wl[worker_id].disc_trail_entries
#if GC_NO_TAGS
#define  Yap_bp                   Yap_heap_regs->wl[worker_id].b_p
#endif /* GC_NO_TAGS */
#define  gc_ma_hash_table         Yap_heap_regs->wl[worker_id].Gc_ma_hash_table
#define  gc_ma_h_top              Yap_heap_regs->wl[worker_id].Gc_ma_h_top
#define  gc_ma_h_list             Yap_heap_regs->wl[worker_id].Gc_ma_h_list
#define  gc_timestamp             Yap_heap_regs->wl[worker_id].Gc_timestamp
#define  cont_top0                Yap_heap_regs->wl[worker_id].conttop0
#define  db_vec                   Yap_heap_regs->wl[worker_id].DB_vec
#define  db_vec0                  Yap_heap_regs->wl[worker_id].DB_vec0
#define  db_root                  Yap_heap_regs->wl[worker_id].DB_root
#define  db_nil                   Yap_heap_regs->wl[worker_id].DB_nil
#define  cont_top                 Yap_heap_regs->wl[worker_id].conttop
#endif
#define  OldASP                   RINFO.old_ASP
#define  OldLCL0                  RINFO.old_LCL0
#define  OldTR                    RINFO.old_TR
#define  OldGlobalBase            RINFO.old_GlobalBase
#define  OldH                     RINFO.old_H
#define  OldH0                    RINFO.old_H0
#define  OldTrailBase             RINFO.old_TrailBase
#define  OldTrailTop              RINFO.old_TrailTop
#define  OldHeapBase              RINFO.old_HeapBase
#define  OldHeapTop               RINFO.old_HeapTop
#define  OldDelayTop              RINFO.old_DelayTop
#define  CurrentDelayTop          RINFO.current_DelayTop
#define  ClDiff                   RINFO.cl_diff
#define  GDiff                    RINFO.g_diff
#define  GDiff0                   RINFO.g_diff0
#define  GSplit			  RINFO.g_split
#define  HDiff                    RINFO.h_diff
#define  LDiff                    RINFO.l_diff
#define  TrDiff                   RINFO.tr_diff
#define  XDiff                    RINFO.x_diff
#define  DelayDiff                RINFO.delay_diff
/* current consult stack */
#define  ConsultSp                Yap_heap_regs->WL.consultsp
/* top of consult stack  */
#define  ConsultBase              Yap_heap_regs->WL.consultbase
/* low-water mark for consult  */
#define  ConsultLow               Yap_heap_regs->WL.consultlow
#define  LastAssertedPred         Yap_heap_regs->WL.last_asserted_pred
/* current maximum number of cells in consult stack */
#define  ConsultCapacity          Yap_heap_regs->WL.consultcapacity
#define  DebugOn                  Yap_heap_regs->WL.debug_on
#define  FormatInfo               Yap_heap_regs->WL.f_info
#define  DelayArenaOverflows      Yap_heap_regs->WL.delay_arena_overflows
#define  ArenaOverflows		  Yap_heap_regs->WL.arena_overflows
#define  DepthArenas		  Yap_heap_regs->WL.depth_arenas
#define  ScannerStack             Yap_heap_regs->WL.scanner_stack
#define  ScannerExtraBlocks       Yap_heap_regs->WL.scanner_extra_blocks
#define  Yap_BigTmp               Yap_heap_regs->WL.big_tmp
#define  ActiveSignals            Yap_heap_regs->WL.active_signals
#define  IPredArity               Yap_heap_regs->WL.i_pred_arity
#define  ProfEnd                  Yap_heap_regs->WL.prof_end
#define  UncaughtThrow            Yap_heap_regs->WL.uncaught_throw
#define  DoingUndefp              Yap_heap_regs->WL.doing_undefp
#define  StartLine                Yap_heap_regs->WL.start_line
#define  ScratchPad               Yap_heap_regs->WL.scratchpad
#ifdef  COROUTINING
#define  WokenGoals               Yap_heap_regs->WL.woken_goals
#define  AttsMutableList          Yap_heap_regs->WL.atts_mutable_list
#endif
#define  GcGeneration             Yap_heap_regs->WL.gc_generation
#define  GcPhase                  Yap_heap_regs->WL.gc_phase
#define  GcCurrentPhase           Yap_heap_regs->WL.gc_current_phase
#define  GcCalls                  Yap_heap_regs->WL.gc_calls
#define  TotGcTime                Yap_heap_regs->WL.tot_gc_time
#define  TotGcRecovered           Yap_heap_regs->WL.tot_gc_recovered
#define  LastGcTime               Yap_heap_regs->WL.last_gc_time
#define  LastSSTime               Yap_heap_regs->WL.last_ss_time
#define  Yap_gc_restore           Yap_heap_regs->WL.gc_restore
#define  TrustLUCode              Yap_heap_regs->WL.trust_lu_code
#define  DynamicArrays            Yap_heap_regs->WL.dynamic_arrays
#define  StaticArrays             Yap_heap_regs->WL.static_arrays
#define  GlobalVariables          Yap_heap_regs->WL.global_variables
#define  GlobalArena              Yap_heap_regs->WL.global_arena
#define  GlobalArenaOverflows     Yap_heap_regs->WL.global_arena_overflows
#define  Yap_AllowRestart         Yap_heap_regs->WL.allow_restart
#define  GlobalDelayArena         Yap_heap_regs->WL.global_delay_arena
#define  profiling                Yap_heap_regs->compiler_profiling
#define  call_counting            Yap_heap_regs->compiler_call_counting
#define  compile_arrays           Yap_heap_regs->compiler_compile_arrays
#define  optimizer_on             Yap_heap_regs->compiler_optimizer_on
#define  compile_mode             Yap_heap_regs->compiler_compile_mode
#define  P_before_spy             Yap_heap_regs->debugger_p_before_spy
#define  ForeignCodeBase          Yap_heap_regs->foreign_code_base;
#define  ForeignCodeTop           Yap_heap_regs->foreign_code_top;
#define  ForeignCodeMax           Yap_heap_regs->foreign_code_max;
#define  ForeignCodeLoaded        Yap_heap_regs->foreign_code_loaded
#define  PredHash                 Yap_heap_regs->pred_hash
#define  PredHashRWLock           Yap_heap_regs->pred_hash_rw_lock
#define  PredsInHashTable         Yap_heap_regs->preds_in_hash_table
#define  PredHashTableSize        Yap_heap_regs->pred_hash_table_size
#define  PredHashInitialSize      1039L
#define  PredHashIncrement        7919L
#define  ParserErrorStyle         Yap_heap_regs->parser_error_style
#define  GlobalHoldEntry          Yap_heap_regs->global_hold_entry
#define  DeadStaticClauses        Yap_heap_regs->dead_static_clauses
#define  DeadMegaClauses          Yap_heap_regs->dead_mega_clauses
#define  DBTermsList              Yap_heap_regs->dbterms_list
#define  DeadStaticIndices        Yap_heap_regs->dead_static_indices
#define  SizeOfOverflow           Yap_heap_regs->size_of_overflow
#define  LastWtimePtr             Yap_heap_regs->last_wtime
#define  BGL			  Yap_heap_regs->bgl
#define  FreeBlocks		  Yap_heap_regs->free_blocks
#if defined(YAPOR) || defined(THREADS)
#define  FreeBlocksLock           Yap_heap_regs->free_blocks_lock
#define  HeapTopLock              Yap_heap_regs->heap_top_lock
#define  HeapTopOwner             Yap_heap_regs->heap_top_owner
#define  NOfThreads               Yap_heap_regs->n_of_threads
#define  NOfThreadsCreated        Yap_heap_regs->n_of_threads_created
#define  ThreadsTotalTime         Yap_heap_regs->threads_total_time
#define  DeadStaticClausesLock    Yap_heap_regs->dead_static_clauses_lock
#define  DeadMegaClausesLock      Yap_heap_regs->dead_mega_clauses_lock
#define  DBTermsListLock          Yap_heap_regs->dbterms_list_lock
#define  DeadStaticIndicesLock    Yap_heap_regs->dead_static_indices_lock
#define  ModulesLock		  Yap_heap_regs->modules_lock
#endif
#define  CreepCode                Yap_heap_regs->creep_code
#define  UndefCode                Yap_heap_regs->undef_code
#define  SpyCode                  Yap_heap_regs->spy_code
#ifdef DEBUG
#define  Yap_NewCps		  Yap_heap_regs->new_cps
#define  Yap_LiveCps		  Yap_heap_regs->live_cps
#define  Yap_DirtyCps		  Yap_heap_regs->dirty_cps
#define  Yap_FreedCps		  Yap_heap_regs->freed_cps
#endif
#if defined(YAPOR) || defined(TABLING)
#define  GLOBAL		          Yap_heap_regs->global
#define  REMOTE                   Yap_heap_regs->remote
#endif /* YAPOR || TABLING */

#define UPDATE_MODE_IMMEDIATE          0
#define UPDATE_MODE_LOGICAL            1
#define UPDATE_MODE_LOGICAL_ASSERT     2


#ifdef COROUTINING
#define  NUM_OF_ATTS              Yap_heap_regs->num_of_atts
#endif

/* initially allow for files with up to 1024 predicates. This number
   is extended whenever needed */
#define  InitialConsultCapacity    1024
#if HAVE_LIBREADLINE
#define  ReadlineBuf              Yap_heap_regs->readline_buf
#define  ReadlinePos              Yap_heap_regs->readline_pos
#endif


#if (defined(USE_SYSTEM_MALLOC) && HAVE_MALLINFO)||USE_DL_MALLOC
UInt STD_PROTO(Yap_givemallinfo, (void));
#endif

ADDR    STD_PROTO(Yap_ExpandPreAllocCodeSpace, (UInt, void *));
#define Yap_ReleasePreAllocCodeSpace(x)
ADDR    STD_PROTO(Yap_InitPreAllocCodeSpace, (void));
EXTERN inline ADDR
Yap_PreAllocCodeSpace(void)
{
  ADDR ptr = ScratchPad.ptr;
  if (ptr) return ptr;
  return Yap_InitPreAllocCodeSpace();
}

#ifdef ATOMS_H
#ifdef THREADS
Prop STD_PROTO(Yap_NewThreadPred, (struct pred_entry *));
Prop STD_PROTO(Yap_NewPredPropByFunctor, (Functor, Term));

EXTERN inline PredEntry *
Yap_GetThreadPred(struct pred_entry *ap)
{
  Functor f = ap->FunctorOfPred;
  Term  mod = ap->ModuleOfPred;
  Prop p0 = AbsPredProp(Yap_heap_regs->thread_handle[worker_id].local_preds);

  while(p0) {
    PredEntry *ap = RepPredProp(p0);
    if (ap->FunctorOfPred == f &&
	ap->ModuleOfPred == mod) return ap;
    p0 = ap->NextOfPE;
  }
  return RepPredProp(Yap_NewThreadPred(ap));
}
#endif
#endif

#endif /* HEAP_H */
