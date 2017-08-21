/************************************************************************
**                                                                     **
**                   The YapTab/YapOr/OPTYap systems                   **
**                                                                     **
** YapTab extends the Yap Prolog engine to support sequential tabling  **
** YapOr extends the Yap Prolog engine to support or-parallelism       **
** OPTYap extends the Yap Prolog engine to support or-parallel tabling **
**                                                                     **
**                                                                     **
**      Yap Prolog was developed at University of Porto, Portugal      **
**                                                                     **
************************************************************************/

/**********************
**      typedefs     **
**********************/

typedef double realtime;
typedef unsigned long bitmap;

#ifdef YAPOR_THREADS
/* Threads may not assume addresses are the same at different workers */
static inline choiceptr offset_to_cptr(Int node) {
  CACHE_REGS
  return (choiceptr)(LCL0+node);
}

static inline Int cptr_to_offset(choiceptr node) {
  CACHE_REGS
  return (Int)((CELL *)node-LCL0);
}

static inline choiceptr offset_to_cptr_with_null(Int node) {
  CACHE_REGS
  if (node == 0L) return NULL;
  return (choiceptr)(LCL0+node);
}

static inline Int cptr_to_offset_with_null(choiceptr node) {
  CACHE_REGS
  if (node == NULL) return 0L;
  return (Int)((CELL *)node-LCL0);
}
#endif /* YAPOR_THREADS */



/**************************************************
**      ma_h_inner_struct and ma_hash_entry      **
**************************************************/

#if (defined(TABLING) || !defined(YAPOR_COW)) && defined(MULTI_ASSIGNMENT_VARIABLES)
#define MAVARS_HASH_SIZE 512

typedef struct ma_h_entry {
  CELL* addr;
  struct ma_h_entry *next;
} ma_h_inner_struct;

typedef struct {
  UInt timestmp;
  struct ma_h_entry val;
} ma_hash_entry;
#endif /* (TABLING || !YAPOR_COW) && MULTI_ASSIGNMENT_VARIABLES */



/***************************************
**      threads_dependency_frame      **
***************************************/

#ifdef THREADS_CONSUMER_SHARING
struct threads_dependency_frame {
  lockvar lock;
  enum {
    working,
    idle,
    completing
  } state;
  int terminator;
  int next;
};
#endif /* THREADS_CONSUMER_SHARING */

#define ThDepFr_lock(X)        ((X).lock)
#define ThDepFr_state(X)       ((X).state)
#define ThDepFr_terminator(X)  ((X).terminator)
#define ThDepFr_next(X)        ((X).next)



/**************************
**      page_header      **
**************************/

#ifdef USE_PAGES_MALLOC
typedef struct page_header {
  volatile size_t structs_in_use;
  void *allocated_area;
  void *first_free_struct;
  struct page_header *previous;
  struct page_header *next;
} *pg_hd_ptr;
#endif /* USE_PAGES_MALLOC */

#define PgHd_strs_in_use(X)  ((X)->structs_in_use)
#define PgHd_alloc_area(X)   ((X)->allocated_area)
#define PgHd_first_str(X)    ((X)->first_free_struct)
#define PgHd_previous(X)     ((X)->previous)
#define PgHd_next(X)         ((X)->next)



/*****************************************
**      global_page and local_page      **
*****************************************/

struct global_page_entry {
#if defined(YAPOR) || defined(THREADS)
  lockvar lock;
#endif /* YAPOR || THREADS */
#ifdef USE_PAGES_MALLOC
  struct page_header *first_page;
  struct page_header *last_page;
  int structs_per_page;
  volatile long pages_in_use;
#endif /* USE_PAGES_MALLOC */
  volatile size_t structs_in_use;
};

struct local_page_entry {
#ifdef USE_PAGES_MALLOC
  struct page_header *first_page;
  struct page_header *last_page;
  int structs_per_page;

  size_t pages_in_use;
#endif /* USE_PAGES_MALLOC */
  size_t structs_in_use;
};

#define PgEnt_lock(X)           ((X).lock)
#define PgEnt_first(X)          ((X).first_page)
#define PgEnt_last(X)           ((X).last_page)
#define PgEnt_strs_per_page(X)  ((X).structs_per_page)
#define PgEnt_pages_in_use(X)   ((X).pages_in_use)
#define PgEnt_strs_in_use(X)    ((X).structs_in_use)
#define PgEnt_strs_free(X)      (PgEnt_pg_in_use(X) * PgEnt_str_per_pg(X) - PgEnt_str_in_use(X))



/***************************
**      global_pages      **
***************************/

struct global_pages {
#ifdef USE_PAGES_MALLOC
  struct global_page_entry alloc_pages;
  struct global_page_entry void_pages;
#endif /* USE_PAGES_MALLOC */

#ifdef TABLING
  struct global_page_entry table_entry_pages;
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
  struct global_page_entry subgoal_entry_pages;
#endif
  struct global_page_entry subgoal_frame_pages;
  struct global_page_entry dependency_frame_pages;
  struct global_page_entry subgoal_trie_node_pages;
  struct global_page_entry subgoal_trie_hash_pages;
  struct global_page_entry answer_trie_node_pages;
  struct global_page_entry answer_trie_hash_pages;
#if defined(THREADS_FULL_SHARING)
  struct global_page_entry answer_ref_node_pages;
#endif
  struct global_page_entry global_trie_node_pages;
  struct global_page_entry global_trie_hash_pages;
#endif /* TABLING */

#ifdef YAPOR
  struct global_page_entry or_frame_pages;
  struct global_page_entry query_goal_solution_frame_pages;
  struct global_page_entry query_goal_answer_frame_pages;
#ifdef TABLING
  struct global_page_entry suspension_frame_pages;
#endif
#ifdef TABLING_INNER_CUTS
  struct global_page_entry table_subgoal_solution_frame_pages;
  struct global_page_entry table_subgoal_answer_frame_pages;
#endif
#endif /* YAPOR */
};


 
/**************************
**      local_pages      **
**************************/

#if defined(TABLING) && (defined(YAPOR) || defined(THREADS))
struct local_pages {
#ifdef YAPOR
  struct answer_trie_node *next_free_answer_trie_node;
#elif THREADS
#ifdef USE_PAGES_MALLOC
  struct local_page_entry void_pages;
#endif
  struct local_page_entry table_entry_pages;
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
  struct local_page_entry subgoal_entry_pages;
#endif
  struct local_page_entry subgoal_frame_pages;
  struct local_page_entry dependency_frame_pages;
  struct local_page_entry subgoal_trie_node_pages;
  struct local_page_entry subgoal_trie_hash_pages;
  struct local_page_entry answer_trie_node_pages;
  struct local_page_entry answer_trie_hash_pages;
#if defined(THREADS_FULL_SHARING)
  struct local_page_entry answer_ref_node_pages;
#endif
  struct local_page_entry global_trie_node_pages;
  struct local_page_entry global_trie_hash_pages;
#endif
};
#endif /* TABLING && (YAPOR || THREADS) */



/**********************************
**      global_optyap_locks      **
**********************************/

#ifdef YAPOR
struct global_optyap_locks {
  lockvar bitmap_idle_workers;
  lockvar bitmap_root_cp_workers;
  lockvar bitmap_invisible_workers;
  lockvar bitmap_requestable_workers;
  lockvar bitmap_finished_workers;
#ifdef TABLING_INNER_CUTS
  lockvar bitmap_pruning_workers;
#endif /* TABLING_INNER_CUTS */

  int who_locked_heap;
  lockvar heap_access;
  lockvar alloc_block;
};
#endif /* YAPOR */



/***********************************
**      local_optyap_signals      **
***********************************/

#ifdef YAPOR
struct local_optyap_signals {
#if defined(YAPOR_COPY) || defined(YAPOR_THREADS)
  lockvar lock;
  volatile enum {
    Q_idle = 0,
    trail  = 1,
    global = 2,
    local  = 3,
    P_idle = 4
  } P_fase, Q_fase;
#endif /* YAPOR_COPY || YAPOR_THREADS */
  volatile enum {
    no_sharing   = 0, 
    sharing      = 1,
    nodes_shared = 2,
    copy_done    = 3,
    worker_ready = 4
  } reply_signal;
};
#endif /* YAPOR */



/*********************************
**      global_optyap_data      **
*********************************/

struct global_optyap_data {
  /* global data related to memory management */
  struct global_pages pages;

#ifdef YAPOR
  /* global static data */
  int scheduler_loop;
  int delayed_release_load;
  int number_workers;
  int worker_pid[MAX_WORKERS];
  
#ifdef YAPOR_COW
  int master_worker;
#endif /* YAPOR_COW */

  /* global data related to or-parallelism */
  realtime execution_time;
#ifdef YAPOR_THREADS
  Int  root_choice_point_offset;
#else
  choiceptr root_choice_point;
#endif
  struct or_frame *root_or_frame;
  bitmap present_workers;
  volatile bitmap idle_workers;
  volatile bitmap root_cp_workers;
  volatile bitmap invisible_workers;
  volatile bitmap requestable_workers;
  volatile bitmap finished_workers;
#ifdef TABLING_INNER_CUTS
  volatile bitmap pruning_workers;
#endif /* TABLING_INNER_CUTS */
  struct global_optyap_locks locks;
  volatile unsigned int branch[MAX_WORKERS][MAX_BRANCH_DEPTH];
  volatile char parallel_mode;  /* PARALLEL_MODE_OFF / PARALLEL_MODE_ON / PARALLEL_MODE_RUNNING */
#endif /* YAPOR */

#ifdef TABLING
  /* global data related to tabling */
  struct global_trie_node *root_global_trie;
  struct table_entry *root_table_entry;
#ifdef LIMIT_TABLING
  int max_pages;
  struct subgoal_frame *first_subgoal_frame;
  struct subgoal_frame *last_subgoal_frame;
  struct subgoal_frame *check_subgoal_frame;
#endif /* LIMIT_TABLING */
#ifdef YAPOR
  struct dependency_frame *root_dependency_frame;
#endif /* YAPOR */
#ifdef THREADS_CONSUMER_SHARING
  struct threads_dependency_frame threads_dependency_frame[MAX_THREADS];
#endif /*THREADS_CONSUMER_SHARING*/
  CELL table_var_enumerator[MAX_TABLE_VARS];
#ifdef TRIE_LOCK_USING_GLOBAL_ARRAY
  lockvar trie_locks[TRIE_LOCK_BUCKETS];
#endif /* TRIE_LOCK_USING_GLOBAL_ARRAY */
#ifdef TIMESTAMP_CHECK
  long timestamp;
#endif /* TIMESTAMP_CHECK */
#endif /* TABLING */
};

#define GLOBAL_pages_alloc                      (GLOBAL_optyap_data.pages.alloc_pages)
#define GLOBAL_pages_void                       (GLOBAL_optyap_data.pages.void_pages)
#define GLOBAL_pages_tab_ent                    (GLOBAL_optyap_data.pages.table_entry_pages)
#define GLOBAL_pages_sg_ent                     (GLOBAL_optyap_data.pages.subgoal_entry_pages)
#define GLOBAL_pages_sg_fr                      (GLOBAL_optyap_data.pages.subgoal_frame_pages)
#define GLOBAL_pages_dep_fr                     (GLOBAL_optyap_data.pages.dependency_frame_pages)
#define GLOBAL_pages_sg_node                    (GLOBAL_optyap_data.pages.subgoal_trie_node_pages)
#define GLOBAL_pages_sg_hash                    (GLOBAL_optyap_data.pages.subgoal_trie_hash_pages)
#define GLOBAL_pages_ans_node                   (GLOBAL_optyap_data.pages.answer_trie_node_pages)
#define GLOBAL_pages_ans_hash                   (GLOBAL_optyap_data.pages.answer_trie_hash_pages)
#define GLOBAL_pages_ans_ref_node               (GLOBAL_optyap_data.pages.answer_ref_node_pages)
#define GLOBAL_pages_gt_node                    (GLOBAL_optyap_data.pages.global_trie_node_pages)
#define GLOBAL_pages_gt_hash                    (GLOBAL_optyap_data.pages.global_trie_hash_pages)
#define GLOBAL_pages_or_fr                      (GLOBAL_optyap_data.pages.or_frame_pages)
#define GLOBAL_pages_qg_sol_fr                  (GLOBAL_optyap_data.pages.query_goal_solution_frame_pages)
#define GLOBAL_pages_qg_ans_fr                  (GLOBAL_optyap_data.pages.query_goal_answer_frame_pages)
#define GLOBAL_pages_susp_fr                    (GLOBAL_optyap_data.pages.suspension_frame_pages)
#define GLOBAL_pages_tg_sol_fr                  (GLOBAL_optyap_data.pages.table_subgoal_solution_frame_pages)
#define GLOBAL_pages_tg_ans_fr                  (GLOBAL_optyap_data.pages.table_subgoal_answer_frame_pages)
#define GLOBAL_scheduler_loop                   (GLOBAL_optyap_data.scheduler_loop)
#define GLOBAL_delayed_release_load             (GLOBAL_optyap_data.delayed_release_load)
#define GLOBAL_number_workers                   (GLOBAL_optyap_data.number_workers)
#define GLOBAL_worker_pid(worker)               (GLOBAL_optyap_data.worker_pid[worker])
#define GLOBAL_master_worker                    (GLOBAL_optyap_data.master_worker)
#define GLOBAL_execution_time                   (GLOBAL_optyap_data.execution_time)
#ifdef YAPOR_THREADS
#define Get_GLOBAL_root_cp()	                offset_to_cptr(GLOBAL_optyap_data.root_choice_point_offset)
#define Set_GLOBAL_root_cp(bptr)                (GLOBAL_optyap_data.root_choice_point_offset = cptr_to_offset(bptr))
#else
#define GLOBAL_root_cp                          (GLOBAL_optyap_data.root_choice_point)
#define Get_GLOBAL_root_cp()                    (GLOBAL_optyap_data.root_choice_point)
#define Set_GLOBAL_root_cp(bptr)                (GLOBAL_optyap_data.root_choice_point = (bptr))
#endif
#define GLOBAL_root_or_fr                       (GLOBAL_optyap_data.root_or_frame)
#define GLOBAL_bm_present_workers               (GLOBAL_optyap_data.present_workers)
#define GLOBAL_bm_idle_workers                  (GLOBAL_optyap_data.idle_workers)
#define GLOBAL_bm_root_cp_workers               (GLOBAL_optyap_data.root_cp_workers)
#define GLOBAL_bm_invisible_workers             (GLOBAL_optyap_data.invisible_workers)
#define GLOBAL_bm_requestable_workers           (GLOBAL_optyap_data.requestable_workers)
#define GLOBAL_bm_finished_workers              (GLOBAL_optyap_data.finished_workers)
#define GLOBAL_bm_pruning_workers               (GLOBAL_optyap_data.pruning_workers)
#define GLOBAL_locks_bm_idle_workers            (GLOBAL_optyap_data.locks.bitmap_idle_workers)
#define GLOBAL_locks_bm_root_cp_workers         (GLOBAL_optyap_data.locks.bitmap_root_cp_workers)
#define GLOBAL_locks_bm_invisible_workers       (GLOBAL_optyap_data.locks.bitmap_invisible_workers)
#define GLOBAL_locks_bm_requestable_workers     (GLOBAL_optyap_data.locks.bitmap_requestable_workers)
#define GLOBAL_locks_bm_finished_workers        (GLOBAL_optyap_data.locks.bitmap_finished_workers)
#define GLOBAL_locks_bm_pruning_workers         (GLOBAL_optyap_data.locks.bitmap_pruning_workers)
#define GLOBAL_locks_who_locked_heap            (GLOBAL_optyap_data.locks.who_locked_heap)
#define GLOBAL_locks_heap_access                (GLOBAL_optyap_data.locks.heap_access)
#define GLOBAL_locks_alloc_block                (GLOBAL_optyap_data.locks.alloc_block)
#define GLOBAL_branch(worker, depth)            (GLOBAL_optyap_data.branch[worker][depth])
#define GLOBAL_parallel_mode                    (GLOBAL_optyap_data.parallel_mode)
#define GLOBAL_root_gt                          (GLOBAL_optyap_data.root_global_trie)
#define GLOBAL_root_tab_ent                     (GLOBAL_optyap_data.root_table_entry)
#define GLOBAL_max_pages                        (GLOBAL_optyap_data.max_pages)
#define GLOBAL_first_sg_fr                      (GLOBAL_optyap_data.first_subgoal_frame)
#define GLOBAL_last_sg_fr                       (GLOBAL_optyap_data.last_subgoal_frame)
#define GLOBAL_check_sg_fr                      (GLOBAL_optyap_data.check_subgoal_frame)
#define GLOBAL_root_dep_fr                      (GLOBAL_optyap_data.root_dependency_frame)
#define GLOBAL_th_dep_fr(wid)                   (GLOBAL_optyap_data.threads_dependency_frame[wid])
#define GLOBAL_table_var_enumerator(index)      (GLOBAL_optyap_data.table_var_enumerator[index])
#define GLOBAL_table_var_enumerator_addr(index) (GLOBAL_optyap_data.table_var_enumerator + (index))
#define GLOBAL_trie_locks(index)                (GLOBAL_optyap_data.trie_locks[index])
#define GLOBAL_timestamp                        (GLOBAL_optyap_data.timestamp)



/********************************
**      local_optyap_data      **
********************************/

struct local_optyap_data {
#if defined(TABLING) && (defined(YAPOR) || defined(THREADS))
  /* local data related to memory management */
  struct local_pages pages;
#endif /* TABLING && (YAPOR || THREADS) */

#ifdef YAPOR
  lockvar lock;
  /* local data related to or-parallelism */
  volatile int load;
#ifdef YAPOR_THREADS
  Int top_choice_point_offset;
#else
  choiceptr top_choice_point;
#endif /* YAPOR_THREADS */
  struct or_frame *top_or_frame;
#ifdef YAPOR_THREADS
  Int prune_request_offset;
#else
  choiceptr prune_request;
#endif /* YAPOR_THREADS */
  volatile int share_request;
  struct local_optyap_signals share_signals;
  volatile struct {
    CELL start;
    CELL end;
  } global_copy, local_copy, trail_copy;
#endif /* YAPOR */

#ifdef TABLING
  /* local data related to tabling */
  struct subgoal_frame *top_subgoal_frame;
  struct dependency_frame *top_dependency_frame;
#ifdef TABLING_INNER_CUTS
  choiceptr bottom_pruning_scope;
#endif /* TABLING_INNER_CUTS */
#ifdef YAPOR
#ifdef YAPOR_THREADS
  Int top_choice_point_on_stack_offset;
#else
  choiceptr top_choice_point_on_stack;
#endif /* YAPOR_THREADS */
  struct or_frame *top_or_frame_with_suspensions;
#endif /* YAPOR */
#ifdef OUTPUT_THREADS_TABLING
  FILE *thread_output;
#endif /* OUTPUT_THREADS_TABLING */
#endif /* TABLING */

#if (defined(TABLING) || !defined(YAPOR_COW)) && defined(MULTI_ASSIGNMENT_VARIABLES)
  UInt ma_timestamp;
  ma_h_inner_struct *ma_h_top;
  ma_hash_entry ma_hash_table[MAVARS_HASH_SIZE];
#endif /* (TABLING || !YAPOR_COW) && MULTI_ASSIGNMENT_VARIABLES */
};

#define LOCAL_pages_void                   (LOCAL_optyap_data.pages.void_pages)
#define LOCAL_pages_tab_ent                (LOCAL_optyap_data.pages.table_entry_pages)
#define LOCAL_pages_sg_ent                 (LOCAL_optyap_data.pages.subgoal_entry_pages)
#define LOCAL_pages_sg_fr                  (LOCAL_optyap_data.pages.subgoal_frame_pages)
#define LOCAL_pages_dep_fr                 (LOCAL_optyap_data.pages.dependency_frame_pages)
#define LOCAL_pages_sg_node                (LOCAL_optyap_data.pages.subgoal_trie_node_pages)
#define LOCAL_pages_sg_hash                (LOCAL_optyap_data.pages.subgoal_trie_hash_pages)
#define LOCAL_pages_ans_node               (LOCAL_optyap_data.pages.answer_trie_node_pages)
#define LOCAL_pages_ans_hash               (LOCAL_optyap_data.pages.answer_trie_hash_pages)
#define LOCAL_pages_ans_ref_node           (LOCAL_optyap_data.pages.answer_ref_node_pages)
#define LOCAL_pages_gt_node                (LOCAL_optyap_data.pages.global_trie_node_pages)
#define LOCAL_pages_gt_hash                (LOCAL_optyap_data.pages.global_trie_hash_pages)
#define LOCAL_next_free_ans_node           (LOCAL_optyap_data.pages.next_free_answer_trie_node)
#define LOCAL_lock                         (LOCAL_optyap_data.lock)
#define LOCAL_load                         (LOCAL_optyap_data.load)
#ifdef YAPOR_THREADS
#define Get_LOCAL_top_cp()                 offset_to_cptr(LOCAL_optyap_data.top_choice_point_offset)
#define Set_LOCAL_top_cp(cpt)              (LOCAL_optyap_data.top_choice_point_offset =  cptr_to_offset(cpt))
#else
#define LOCAL_top_cp                       (LOCAL_optyap_data.top_choice_point)
#define Get_LOCAL_top_cp()		   (LOCAL_optyap_data.top_choice_point)
#define Set_LOCAL_top_cp(cpt)	           (LOCAL_optyap_data.top_choice_point =  cpt)
#endif /* YAPOR_THREADS */
#define LOCAL_top_or_fr                    (LOCAL_optyap_data.top_or_frame)
#ifdef YAPOR_THREADS
#define Get_LOCAL_prune_request()	   offset_to_cptr_with_null(LOCAL_optyap_data.prune_request_offset)
#define Set_LOCAL_prune_request(cpt)       (LOCAL_optyap_data.prune_request_offset =  cptr_to_offset_with_null(cpt))
#else
#define LOCAL_prune_request                (LOCAL_optyap_data.prune_request)
#define Get_LOCAL_prune_request()          (LOCAL_optyap_data.prune_request)
#define Set_LOCAL_prune_request(cpt)       (LOCAL_optyap_data.prune_request = cpt)
#endif /* YAPOR_THREADS */
#define LOCAL_share_request                (LOCAL_optyap_data.share_request)
#define LOCAL_reply_signal                 (LOCAL_optyap_data.share_signals.reply_signal)
#define LOCAL_p_fase_signal                (LOCAL_optyap_data.share_signals.P_fase)
#define LOCAL_q_fase_signal                (LOCAL_optyap_data.share_signals.Q_fase)
#define LOCAL_lock_signals                 (LOCAL_optyap_data.share_signals.lock)
#define LOCAL_start_global_copy            (LOCAL_optyap_data.global_copy.start)
#define LOCAL_end_global_copy              (LOCAL_optyap_data.global_copy.end)
#define LOCAL_start_local_copy             (LOCAL_optyap_data.local_copy.start)
#define LOCAL_end_local_copy               (LOCAL_optyap_data.local_copy.end)
#define LOCAL_start_trail_copy             (LOCAL_optyap_data.trail_copy.start)
#define LOCAL_end_trail_copy               (LOCAL_optyap_data.trail_copy.end)
#define LOCAL_top_sg_fr                    (LOCAL_optyap_data.top_subgoal_frame)
#define LOCAL_top_dep_fr                   (LOCAL_optyap_data.top_dependency_frame)
#define LOCAL_pruning_scope                (LOCAL_optyap_data.bottom_pruning_scope)
#ifdef YAPOR_THREADS
#define Get_LOCAL_top_cp_on_stack()        offset_to_cptr(LOCAL_optyap_data.top_choice_point_on_stack_offset)
#define Set_LOCAL_top_cp_on_stack(cpt)     (LOCAL_optyap_data.top_choice_point_on_stack_offset =  cptr_to_offset(cpt))
#else
#define LOCAL_top_cp_on_stack              (LOCAL_optyap_data.top_choice_point_on_stack)
#define Get_LOCAL_top_cp_on_stack()	   (LOCAL_optyap_data.top_choice_point_on_stack)
#define Set_LOCAL_top_cp_on_stack(cpt)	   (LOCAL_optyap_data.top_choice_point_on_stack =  cpt)
#endif /* YAPOR_THREADS */
#define LOCAL_top_susp_or_fr               (LOCAL_optyap_data.top_or_frame_with_suspensions)
#define LOCAL_thread_output                (LOCAL_optyap_data.thread_output)
#define LOCAL_ma_timestamp                 (LOCAL_optyap_data.ma_timestamp)
#define LOCAL_ma_h_top                     (LOCAL_optyap_data.ma_h_top)
#define LOCAL_ma_hash_table                (LOCAL_optyap_data.ma_hash_table)
 
#define REMOTE_pages_void(wid)                 (REMOTE(wid)->optyap_data.pages.void_pages)
#define REMOTE_pages_tab_ent(wid)              (REMOTE(wid)->optyap_data.pages.table_entry_pages)
#define REMOTE_pages_sg_ent(wid)               (REMOTE(wid)->optyap_data.pages.subgoal_entry_pages)
#define REMOTE_pages_sg_fr(wid)                (REMOTE(wid)->optyap_data.pages.subgoal_frame_pages)
#define REMOTE_pages_dep_fr(wid)               (REMOTE(wid)->optyap_data.pages.dependency_frame_pages)
#define REMOTE_pages_sg_node(wid)              (REMOTE(wid)->optyap_data.pages.subgoal_trie_node_pages)
#define REMOTE_pages_sg_hash(wid)              (REMOTE(wid)->optyap_data.pages.subgoal_trie_hash_pages)
#define REMOTE_pages_ans_node(wid)             (REMOTE(wid)->optyap_data.pages.answer_trie_node_pages)
#define REMOTE_pages_ans_hash(wid)             (REMOTE(wid)->optyap_data.pages.answer_trie_hash_pages)
#define REMOTE_pages_ans_ref_node(wid)         (REMOTE(wid)->optyap_data.pages.answer_ref_node_pages)
#define REMOTE_pages_gt_node(wid)              (REMOTE(wid)->optyap_data.pages.global_trie_node_pages)
#define REMOTE_pages_gt_hash(wid)              (REMOTE(wid)->optyap_data.pages.global_trie_hash_pages)
#define REMOTE_next_free_ans_node(wid)         (REMOTE(wid)->optyap_data.pages.next_free_answer_trie_node)
#define REMOTE_lock(wid)                       (REMOTE(wid)->optyap_data.lock)
#define REMOTE_load(wid)                       (REMOTE(wid)->optyap_data.load)
#ifdef YAPOR_THREADS
#define REMOTE_top_cp(wid)                     offset_to_cptr(REMOTE(wid)->optyap_data.top_choice_point_offset)
#define Set_REMOTE_top_cp(wid, bptr)           (REMOTE(wid)->optyap_data.top_choice_point_offset = cptr_to_offset(bptr))
#else
#define REMOTE_top_cp(wid)                     (REMOTE(wid)->optyap_data.top_choice_point)
#define Set_REMOTE_top_cp(wid, bptr)           (REMOTE(wid)->optyap_data.top_choice_point = (bptr))
#endif /* YAPOR_THREADS */
#define REMOTE_top_or_fr(wid)                  (REMOTE(wid)->optyap_data.top_or_frame)
#ifdef YAPOR_THREADS
#define Get_REMOTE_prune_request(wid)          offset_to_cptr_with_null(REMOTE(wid)->optyap_data.prune_request_offset)
#define Set_REMOTE_prune_request(wid,cp)       (REMOTE(wid)->optyap_data.prune_request_offset = cptr_to_offset_with_null(cp))
#else
#define REMOTE_prune_request(wid)              (REMOTE(wid)->optyap_data.prune_request)
#define Get_REMOTE_prune_request(wid)          (REMOTE(wid)->optyap_data.prune_request)
#define Set_REMOTE_prune_request(wid,cp)       (REMOTE(wid)->optyap_data.prune_request = cp)
#endif /* YAPOR_THREADS */
#define REMOTE_share_request(wid)              (REMOTE(wid)->optyap_data.share_request)
#define REMOTE_reply_signal(wid)               (REMOTE(wid)->optyap_data.share_signals.reply_signal)
#define REMOTE_p_fase_signal(wid)              (REMOTE(wid)->optyap_data.share_signals.P_fase)
#define REMOTE_q_fase_signal(wid)              (REMOTE(wid)->optyap_data.share_signals.Q_fase)
#define REMOTE_lock_signals(wid)               (REMOTE(wid)->optyap_data.share_signals.lock)
#define REMOTE_start_global_copy(wid)          (REMOTE(wid)->optyap_data.global_copy.start)
#define REMOTE_end_global_copy(wid)            (REMOTE(wid)->optyap_data.global_copy.end)
#define REMOTE_start_local_copy(wid)           (REMOTE(wid)->optyap_data.local_copy.start)
#define REMOTE_end_local_copy(wid)             (REMOTE(wid)->optyap_data.local_copy.end)
#define REMOTE_start_trail_copy(wid)           (REMOTE(wid)->optyap_data.trail_copy.start)
#define REMOTE_end_trail_copy(wid)             (REMOTE(wid)->optyap_data.trail_copy.end)
#define REMOTE_top_sg_fr(wid)                  (REMOTE(wid)->optyap_data.top_subgoal_frame)
#define REMOTE_top_dep_fr(wid)                 (REMOTE(wid)->optyap_data.top_dependency_frame)
#define REMOTE_pruning_scope(wid)              (REMOTE(wid)->optyap_data.bottom_pruning_scope)
#ifdef YAPOR_THREADS
#define REMOTE_top_cp_on_stack(wid)            offset_to_cptr(REMOTE(wid)->optyap_data.top_choice_point_on_stack_offset)
#define Set_REMOTE_top_cp_on_stack(wid, bptr)  (REMOTE(wid)->optyap_data.top_choice_point_on_stack_offset = cptr_to_offset(bptr))
#else
#define REMOTE_top_cp_on_stack(wid)            (REMOTE(wid)->optyap_data.top_choice_point_on_stack)
#define Set_REMOTE_top_cp_on_stack(wid, bptr)  (REMOTE(wid)->optyap_data.top_choice_point_on_stack = (bptr))
#endif /* YAPOR_THREADS */
#define REMOTE_top_susp_or_fr(wid)             (REMOTE(wid)->optyap_data.top_or_frame_with_suspensions)
#define REMOTE_thread_output(wid)              (REMOTE(wid)->optyap_data.thread_output)
#define REMOTE_ma_timestamp(wid)               (REMOTE(wid)->optyap_data.ma_timestamp)
#define REMOTE_ma_h_top(wid)                   (REMOTE(wid)->optyap_data.ma_h_top)
#define REMOTE_ma_hash_table(wid)              (REMOTE(wid)->optyap_data.ma_hash_table)

#ifdef YAPOR
#include "or.structs.h"
#endif /* YAPOR */


#ifdef TABLING
#include "tab.structs.h"
#endif  /* TABLING */
