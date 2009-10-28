/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		YapHeap.h      						 *
* mods:									 *
* comments:	Heap Init Structure					 *
* version:      $Id: Heap.h,v 1.136 2008-08-08 14:05:34 vsc Exp $	 *
*************************************************************************/

/* information that can be stored in Code Space */

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
  Int
  base_diff,
    cl_diff,
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
  int    interrupts_disabled;
  UInt   consultcapacity;
  UInt   active_signals;
  UInt   i_pred_arity;
  yamop *prof_end;
  Int    start_line;
  int    uncaught_throw;
  int    doing_undefp;
  int    arith_error;
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
  /* memory allocation and management */
  special_functors funcs;
  UInt hole_size;
  struct malloc_state *av_;
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
  lockvar  heap_used_lock;        /* protect HeapUsed */
  lockvar  heap_top_lock;        /* protect HeapTop */
  int      heap_top_owner;
#endif

  /* multi-thread/ORP support */
#if defined(YAPOR) || defined(THREADS)
  unsigned int n_of_threads;      /* number of threads and processes in system */
  unsigned int n_of_threads_created;      /* number of threads created since start */
  UInt  threads_total_time;      /* total run time for dead threads */
  lockvar  bgl;		 /* protect long critical regions   */
  lockvar  free_blocks_lock;     /* protect the list of free blocks */
  worker_local wl[MAX_AGENTS];
#else
  worker_local wl;
#endif
#ifdef THREADS
  lockvar  thread_handles_lock;        /* protect ThreadManipulation */
  struct thandle thread_handle[MAX_THREADS];
#endif

  /* atoms and functors */
  UInt n_of_atoms;
  UInt atom_hash_table_size;
  UInt wide_atom_hash_table_size;
  UInt n_of_wide_atoms;
  AtomHashEntry invisiblechain;
  AtomHashEntry *wide_hash_chain;
  AtomHashEntry *hash_chain;
#include "tatoms.h"

  /* Terms multiply used */
#include "hstruct.h"

  /* UDI support */
  struct udi_info *udi_control_blocks;

  /* data-base statistics */
  Int static_predicates_marked;

  /* Internal Database */
  Prop *IntKeys;
  Prop *IntLUKeys;
  Prop *IntBBKeys;

  /* Internal Data-Base Control */
  int update_mode;

  /* Internal Database Statistics */
  UInt int_keys_size;
  UInt int_keys_timestamp;
  UInt int_lu_keys_size;
  UInt int_lu_keys_timestamp;
  UInt int_bb_keys_size;

  /* nasty IDB stuff */
  struct DB_STRUCT *db_erased_marker;
  struct logic_upd_clause *logdb_erased_marker;

  /* Dead clauses and IDB entries */
  struct static_clause *dead_static_clauses;
  struct static_mega_clause *dead_mega_clauses;
  struct static_index *dead_static_indices;
  struct logic_upd_clause *db_erased_list;
  struct logic_upd_index *db_erased_ilist;
#if defined(YAPOR) || defined(THREADS)
  lockvar  dead_static_clauses_lock;        /* protect DeadStaticClauses */
  lockvar  dead_mega_clauses_lock;        /* protect DeadMegaClauses */
  lockvar  dead_static_indices_lock;        /* protect DeadStaticIndices */
#endif

  /* execution counters */
  struct reduction_counters call_counters;

  /* number of attribute modules */
  UInt atts_size;

  /* stack overflow expansion/gc control */
  int    allow_local_expansion;
  int    allow_global_expansion;
  int    allow_trail_expansion;
  unsigned int size_of_overflow;
  struct hold_entry *global_hold_entry;
  UInt      agc_last_call; /* amount of space recovered in all garbage collections */
  UInt      agc_threshold; /* amount of space recovered in all garbage collections */
  Agc_hook  agc_hook;

  /* YAP control flags */
  Int   yap_flags_field[NUMBER_OF_YAP_FLAGS];

  /* Operators */
  struct operator_entry *op_list;

  /* Input/Output */

  /* stream array */
  struct stream_desc *yap_streams;

  /* stream aliases */
  UInt n_of_file_aliases;
  UInt sz_of_file_aliases;
  struct AliasDescS * file_aliases;

  /* prompting */
  Atom atprompt;
  char prompt[MAX_PROMPT];

  /* readline */
#if HAVE_LIBREADLINE
  char *readline_buf, *readline_pos;
#endif

  /* ISO char conversion: I will make no comments */
  char *char_conversion_table;
  char *char_conversion_table2;

  /* write depth */
  UInt maxdepth, maxlist, maxwriteargs;

  /* what to do when the parser gives an error: should be thread bound?  */
  int parser_error_style;

  /* library location.  */
  char *yap_lib_dir;

  /* time */
  void *last_wtime;

  /* profiling */
  int    debugger_output_msg;
#if LOW_PROF
  int   profiler_on;
  int   offline_profiler;
  FILE *f_prof, *f_preds;
  UInt  prof_preds;
#endif /* LOW_PROF */

  /* foreign code loaded */
  void *foreign_code_loaded;
  ADDR  foreign_code_base;
  ADDR  foreign_code_top;
  ADDR  foreign_code_max;

#if defined(YAPOR) || defined(TABLING)
  struct global_data global;
  struct local_data remote[MAX_WORKERS];
#endif /* YAPOR || TABLING */
} all_heap_codes;

#ifdef USE_SYSTEM_MALLOC
extern struct various_codes *Yap_heap_regs;
#else
#define Yap_heap_regs  ((all_heap_codes *)HEAP_INIT_BASE)
#endif

#define  Yap_HoleSize            Yap_heap_regs->hole_size
#define  Yap_av                  Yap_heap_regs->av_
#define  Yap_AttsSize            Yap_heap_regs->atts_size
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
#define  INVISIBLECHAIN           Yap_heap_regs->invisiblechain
#define  max_depth                Yap_heap_regs->maxdepth
#define  max_list                 Yap_heap_regs->maxlist
#define  max_write_args           Yap_heap_regs->maxwriteargs
#define  AtPrompt                 (&(Yap_heap_regs->atprompt    	         ))
#define  Prompt                   Yap_heap_regs->prompt
#define  UdiControlBlocks	  Yap_heap_regs->udi_control_blocks
#define  UPDATE_MODE              Yap_heap_regs->update_mode
#define  STATIC_PREDICATES_MARKED Yap_heap_regs->static_predicates_marked
#define  yap_flags                Yap_heap_regs->yap_flags_field
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
#define  OpList                   Yap_heap_regs->op_list
#define  FloatFormat              Yap_heap_regs->float_format
#define  PROLOG_MODULE            0
#include "dhstruct.h"
#define  DBErasedMarker           Yap_heap_regs->db_erased_marker
#define  LogDBErasedMarker        Yap_heap_regs->logdb_erased_marker
#define  DBErasedList             Yap_heap_regs->db_erased_list
#define  DBErasedIList            Yap_heap_regs->db_erased_ilist
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
#define  BaseDiff	          RINFO.base_diff
#define  Yap_InterruptsDisabled    Yap_heap_regs->WL.interrupts_disabled
/* current consult stack */
#define  ConsultSp                Yap_heap_regs->WL.consultsp
/* top of consult stack  */
#define  ConsultBase              Yap_heap_regs->WL.consultbase
/* low-water mark for consult  */
#define  ConsultLow               Yap_heap_regs->WL.consultlow
#define  ArithError               Yap_heap_regs->WL.arith_error
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
#define  ForeignCodeBase          Yap_heap_regs->foreign_code_base;
#define  ForeignCodeTop           Yap_heap_regs->foreign_code_top;
#define  ForeignCodeMax           Yap_heap_regs->foreign_code_max;
#define  ForeignCodeLoaded        Yap_heap_regs->foreign_code_loaded

#define  PredHashInitialSize      1039L
#define  PredHashIncrement        7919L

#define  ParserErrorStyle         Yap_heap_regs->parser_error_style
#define  GlobalHoldEntry          Yap_heap_regs->global_hold_entry
#define  DeadStaticClauses        Yap_heap_regs->dead_static_clauses
#define  DeadMegaClauses          Yap_heap_regs->dead_mega_clauses
#define  DeadStaticIndices        Yap_heap_regs->dead_static_indices
#define  Yap_AllowLocalExpansion  Yap_heap_regs->allow_local_expansion
#define  Yap_AllowGlobalExpansion Yap_heap_regs->allow_global_expansion
#define  Yap_AllowTrailExpansion  Yap_heap_regs->allow_trail_expansion
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
#define  DeadStaticIndicesLock    Yap_heap_regs->dead_static_indices_lock
#define  ModulesLock		  Yap_heap_regs->modules_lock
#endif
#if defined(YAPOR) || defined(TABLING)
#define  GLOBAL		          Yap_heap_regs->global
#define  REMOTE                   Yap_heap_regs->remote
#endif /* YAPOR || TABLING */

#define UPDATE_MODE_IMMEDIATE          0
#define UPDATE_MODE_LOGICAL            1
#define UPDATE_MODE_LOGICAL_ASSERT     2


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

ADDR    STD_PROTO(Yap_ExpandPreAllocCodeSpace, (UInt, void *, int));
#define Yap_ReleasePreAllocCodeSpace(x)
ADDR    STD_PROTO(Yap_InitPreAllocCodeSpace, (void));
EXTERN inline ADDR
Yap_PreAllocCodeSpace(void) 
{
  return AuxBase;
}

#endif /* HEAP_H */
