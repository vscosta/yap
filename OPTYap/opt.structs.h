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
**      Typedefs     **
**********************/

typedef double realtime;
typedef unsigned long bitmap;

#ifdef YAPOR_THREADS
/* Threads may not assume addresses are the same at different workers */
static inline choiceptr
offset_to_cptr(Int node)
{
  CACHE_REGS
  return (choiceptr)(LCL0+node);
}

static inline Int
cptr_to_offset(choiceptr node)
{
  CACHE_REGS
  return (Int)((CELL *)node-LCL0);
}

static inline choiceptr
offset_to_cptr_with_null(Int node)
{
  CACHE_REGS
  if (node == 0L) return NULL;
  return (choiceptr)(LCL0+node);
}

static inline Int
cptr_to_offset_with_null(choiceptr node)
{
  CACHE_REGS
  if (node == NULL) return 0L;
  return (Int)((CELL *)node-LCL0);
}
#endif /* YAPOR_THREADS */



/*********************************
**      Struct page_header      **
*********************************/

typedef struct page_header {
  volatile int structs_in_use;
  void *first_free_struct;
  struct page_header *previous;
  struct page_header *next;
} *pg_hd_ptr;

#define PgHd_str_in_use(X)  ((X)->structs_in_use)
#define PgHd_free_str(X)    ((X)->first_free_struct)
#define PgHd_previous(X)    ((X)->previous)
#define PgHd_next(X)        ((X)->next)



/***************************
**      Struct pages      **
***************************/

struct pages {
#ifdef USE_PAGES_MALLOC
#ifdef YAPOR
  lockvar lock;
#endif /* YAPOR */
  int structs_per_page;
  struct page_header *first_free_page;
  volatile long pages_allocated;
#endif /* USE_PAGES_MALLOC */
  volatile long structs_in_use;
};

#define Pg_lock(X)        ((X).lock)
#define Pg_str_per_pg(X)  ((X).structs_per_page)
#define Pg_free_pg(X)     ((X).first_free_page)
#define Pg_pg_alloc(X)    ((X).pages_allocated)
#define Pg_str_in_use(X)  ((X).structs_in_use)
#define Pg_str_free(X)    (Pg_pg_alloc(X) * Pg_str_per_pg(X) - Pg_str_in_use(X))



/**********************************
**      Struct global_pages      **
**********************************/

struct global_pages {
#ifdef LIMIT_TABLING
  int max_pages;
#endif /* LIMIT_TABLING */
  struct pages void_pages;
#ifdef YAPOR
  struct pages or_frame_pages;
  struct pages query_goal_solution_frame_pages;
  struct pages query_goal_answer_frame_pages;
#endif /* YAPOR */
#ifdef TABLING_INNER_CUTS
  struct pages table_subgoal_solution_frame_pages;
  struct pages table_subgoal_answer_frame_pages;
#endif /* TABLING_INNER_CUTS */
#ifdef TABLING
  struct pages table_entry_pages;
  struct pages subgoal_frame_pages;
  struct pages dependency_frame_pages;
  struct pages subgoal_trie_node_pages;
  struct pages answer_trie_node_pages;
  struct pages global_trie_node_pages;
  struct pages subgoal_trie_hash_pages;
  struct pages answer_trie_hash_pages;
  struct pages global_trie_hash_pages;
#endif /* TABLING */
#if defined(YAPOR) && defined(TABLING)
  struct pages suspension_frame_pages;
#endif /* YAPOR && TABLING */
};



/*****************************************
**      Struct global_optyap_locks      **
*****************************************/

#ifdef YAPOR
struct global_optyap_locks {
  lockvar bitmap_idle_workers;
  lockvar bitmap_root_cp_workers;
  lockvar bitmap_invisible_workers;
  lockvar bitmap_requestable_workers;
  lockvar bitmap_executing_workers;
  lockvar bitmap_finished_workers;
#ifdef TABLING_INNER_CUTS
  lockvar bitmap_pruning_workers;
#endif /* TABLING_INNER_CUTS */

  int who_locked_heap;
  lockvar heap_access;
  lockvar alloc_block;
};
#endif /* YAPOR */



/*********************************
*   Struct global_optyap_data   **
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

  /* global data related to or-performance */
  realtime execution_time;
  realtime best_execution_times[MAX_BEST_TIMES];
  int number_of_executed_goals;
  char performance_mode;  /* PERFORMANCE_OFF / PERFORMANCE_ON / PERFORMANCE_IN_EXECUTION */

  /* global data related to or-parallelism */
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
  volatile bitmap executing_workers;
  volatile bitmap finished_workers;
#ifdef TABLING_INNER_CUTS
  volatile bitmap pruning_workers;
#endif /* TABLING_INNER_CUTS */
  struct global_optyap_locks locks;
  volatile unsigned int branch[MAX_WORKERS][MAX_BRANCH_DEPTH];
  volatile char parallel_execution_mode;  /* TRUE / FALSE */
  volatile int answers;
#endif /* YAPOR */

#ifdef TABLING
  /* global data related to tabling */
  struct global_trie_node *root_global_trie;
  struct table_entry *root_table_entry;
#ifdef LIMIT_TABLING
  struct subgoal_frame *first_subgoal_frame;
  struct subgoal_frame *last_subgoal_frame;
  struct subgoal_frame *check_subgoal_frame;
#endif /* LIMIT_TABLING */
  struct dependency_frame *root_dependency_frame;
  CELL table_var_enumerator[MAX_TABLE_VARS];
#ifdef TABLE_LOCK_AT_WRITE_LEVEL
  lockvar table_lock[TABLE_LOCK_BUCKETS];
#endif /* TABLE_LOCK_AT_WRITE_LEVEL */
#ifdef TIMESTAMP_CHECK
  long timestamp;
#endif /* TIMESTAMP_CHECK */
#endif /* TABLING */
};

#define Yap_max_pages                        (Yap_optyap_data.pages.max_pages)
#define Yap_pages_void                       (Yap_optyap_data.pages.void_pages)
#define Yap_pages_or_fr                      (Yap_optyap_data.pages.or_frame_pages)
#define Yap_pages_qg_sol_fr                  (Yap_optyap_data.pages.query_goal_solution_frame_pages)
#define Yap_pages_qg_ans_fr                  (Yap_optyap_data.pages.query_goal_answer_frame_pages)
#define Yap_pages_tg_sol_fr                  (Yap_optyap_data.pages.table_subgoal_solution_frame_pages)
#define Yap_pages_tg_ans_fr                  (Yap_optyap_data.pages.table_subgoal_answer_frame_pages)
#define Yap_pages_tab_ent                    (Yap_optyap_data.pages.table_entry_pages)
#define Yap_pages_sg_fr                      (Yap_optyap_data.pages.subgoal_frame_pages)
#define Yap_pages_dep_fr                     (Yap_optyap_data.pages.dependency_frame_pages)
#define Yap_pages_sg_node                    (Yap_optyap_data.pages.subgoal_trie_node_pages)
#define Yap_pages_ans_node                   (Yap_optyap_data.pages.answer_trie_node_pages)
#define Yap_pages_gt_node                    (Yap_optyap_data.pages.global_trie_node_pages)
#define Yap_pages_sg_hash                    (Yap_optyap_data.pages.subgoal_trie_hash_pages)
#define Yap_pages_ans_hash                   (Yap_optyap_data.pages.answer_trie_hash_pages)
#define Yap_pages_gt_hash                    (Yap_optyap_data.pages.global_trie_hash_pages)
#define Yap_pages_susp_fr                    (Yap_optyap_data.pages.suspension_frame_pages)
#define Yap_scheduler_loop                   (Yap_optyap_data.scheduler_loop)
#define Yap_delayed_release_load             (Yap_optyap_data.delayed_release_load)
#define Yap_number_workers                   (Yap_optyap_data.number_workers)
#define Yap_worker_pid(worker)               (Yap_optyap_data.worker_pid[worker])
#define Yap_master_worker                    (Yap_optyap_data.master_worker)
#define Yap_execution_time                   (Yap_optyap_data.execution_time)
#define Yap_best_times(time)                 (Yap_optyap_data.best_execution_times[time])
#define Yap_number_goals                     (Yap_optyap_data.number_of_executed_goals)
#define Yap_performance_mode                 (Yap_optyap_data.performance_mode)
#ifdef YAPOR_THREADS
#define Get_Yap_root_cp()	             offset_to_cptr(Yap_optyap_data.root_choice_point_offset)
#define Set_Yap_root_cp(bptr)                (Yap_optyap_data.root_choice_point_offset = cptr_to_offset(bptr))
#else
#define Yap_root_cp                          (Yap_optyap_data.root_choice_point)
#define Get_Yap_root_cp()                    (Yap_optyap_data.root_choice_point)
#define Set_Yap_root_cp(bptr)                (Yap_optyap_data.root_choice_point = (bptr))
#endif
#define Yap_root_or_fr                       (Yap_optyap_data.root_or_frame)
#define Yap_bm_present_workers               (Yap_optyap_data.present_workers)
#define Yap_bm_idle_workers                  (Yap_optyap_data.idle_workers)
#define Yap_bm_root_cp_workers               (Yap_optyap_data.root_cp_workers)
#define Yap_bm_invisible_workers             (Yap_optyap_data.invisible_workers)
#define Yap_bm_requestable_workers           (Yap_optyap_data.requestable_workers)
#define Yap_bm_executing_workers             (Yap_optyap_data.executing_workers)
#define Yap_bm_finished_workers              (Yap_optyap_data.finished_workers)
#define Yap_bm_pruning_workers               (Yap_optyap_data.pruning_workers)
#define Yap_locks_bm_idle_workers            (Yap_optyap_data.locks.bitmap_idle_workers)
#define Yap_locks_bm_root_cp_workers         (Yap_optyap_data.locks.bitmap_root_cp_workers)
#define Yap_locks_bm_invisible_workers       (Yap_optyap_data.locks.bitmap_invisible_workers)
#define Yap_locks_bm_requestable_workers     (Yap_optyap_data.locks.bitmap_requestable_workers)
#define Yap_locks_bm_executing_workers       (Yap_optyap_data.locks.bitmap_executing_workers)
#define Yap_locks_bm_finished_workers        (Yap_optyap_data.locks.bitmap_finished_workers)
#define Yap_locks_bm_pruning_workers         (Yap_optyap_data.locks.bitmap_pruning_workers)
#define Yap_locks_who_locked_heap            (Yap_optyap_data.locks.who_locked_heap)
#define Yap_locks_heap_access                (Yap_optyap_data.locks.heap_access)
#define Yap_locks_alloc_block                (Yap_optyap_data.locks.alloc_block)
#define Yap_branch(worker, depth)            (Yap_optyap_data.branch[worker][depth])
#define Yap_parallel_execution_mode          (Yap_optyap_data.parallel_execution_mode)
#define Yap_answers                          (Yap_optyap_data.answers)
#define Yap_root_gt                          (Yap_optyap_data.root_global_trie)
#define Yap_root_tab_ent                     (Yap_optyap_data.root_table_entry)
#define Yap_first_sg_fr                      (Yap_optyap_data.first_subgoal_frame)
#define Yap_last_sg_fr                       (Yap_optyap_data.last_subgoal_frame)
#define Yap_check_sg_fr                      (Yap_optyap_data.check_subgoal_frame)
#define Yap_root_dep_fr                      (Yap_optyap_data.root_dependency_frame)
#define Yap_table_var_enumerator(index)      (Yap_optyap_data.table_var_enumerator[index])
#define Yap_table_var_enumerator_addr(index) (Yap_optyap_data.table_var_enumerator + (index))
#define Yap_table_lock(index)                (Yap_optyap_data.table_lock[index])
#define Yap_timestamp                        (Yap_optyap_data.timestamp)



/******************************************
**      Struct local_optyap_signals      **
******************************************/

#ifdef YAPOR
struct local_optyap_signals{
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



/**********************************************************
**      Structs ma_h_inner_struct and ma_hash_entry      **
**********************************************************/

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
**      Struct local_optyap_data      **
***************************************/

struct local_optyap_data {
#ifdef YAPOR
  lockvar lock;
  /* local data related to or-parallelism */
  volatile int load;
#ifdef YAPOR_THREADS
  Int top_choice_point_offset;
#else
  choiceptr top_choice_point;
#endif
  struct or_frame *top_or_frame;
#ifdef YAPOR_THREADS
  Int prune_request_offset;
#else
  choiceptr prune_request;
#endif
  volatile int share_request;
  struct local_optyap_signals share_signals;
  volatile struct {
    CELL start;
    CELL end;
  } global_copy, local_copy, trail_copy;
#endif /* YAPOR */

#ifdef TABLING
  /* local data related to tabling */
  struct answer_trie_node *next_free_answer_trie_node;
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
#endif
  struct or_frame *top_or_frame_with_suspensions;
#endif /* YAPOR */
#endif /* TABLING */

#if (defined(TABLING) || !defined(YAPOR_COW)) && defined(MULTI_ASSIGNMENT_VARIABLES)
  UInt ma_timestamp;
  ma_h_inner_struct *ma_h_top;
  ma_hash_entry ma_hash_table[MAVARS_HASH_SIZE];
#endif /* (TABLING || !YAPOR_COW) && MULTI_ASSIGNMENT_VARIABLES */
};

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
#define LOCAL_next_free_ans_node           (LOCAL_optyap_data.next_free_answer_trie_node)
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
#define LOCAL_ma_timestamp                 (LOCAL_optyap_data.ma_timestamp)
#define LOCAL_ma_h_top                     (LOCAL_optyap_data.ma_h_top)
#define LOCAL_ma_hash_table                (LOCAL_optyap_data.ma_hash_table)


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
#define REMOTE_next_free_ans_node(wid)         (REMOTE(wid)->optyap_data.next_free_answer_trie_node)
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


#ifdef YAPOR
#include "or.structs.h"
#endif /* YAPOR */


#ifdef TABLING
#include "tab.structs.h"
#endif  /* TABLING */
