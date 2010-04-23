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

#ifdef THREADS
/* Threads may not assume addresses are the same at different workers */
static inline choiceptr
offset_to_cptr(Int node)
{
  return (choiceptr)(LCL0+node);
}

static inline Int
cptr_to_offset(choiceptr node)
{
  return (Int)((CELL *)node-LCL0);
}

static inline choiceptr
offset_to_cptr_with_null(Int node)
{
  if (node == 0L) return NULL;
  return (choiceptr)(LCL0+node);
}

static inline Int
cptr_to_offset_with_null(choiceptr node)
{
  if (node == NULL) return 0L;
  return (Int)((CELL *)node-LCL0);
}
#endif /* THREADS */



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
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef YAPOR
  lockvar lock;
#endif /* YAPOR */
  int structs_per_page;
  struct page_header *first_free_page;
  volatile long pages_allocated;
#endif /* SHM_MEMORY_ALLOC_SCHEME */
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



/**********************************
**      Struct global_locks      **
**********************************/

#ifdef YAPOR
struct global_locks {
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
**      Struct global_data      **
*********************************/

struct global_data{
  /* global data related to memory management */
  struct global_pages pages;

#ifdef YAPOR
  /* global static data */
  int scheduler_loop;
  int delayed_release_load;
  int number_workers;
  int worker_pid[MAX_WORKERS];
#ifdef ACOW
  int master_worker;
#endif /* ACOW */

  /* global data related to or-performance */
  realtime execution_time;
  realtime best_execution_times[MAX_BEST_TIMES];
  int number_of_executed_goals;
  char performance_mode;  /* PERFORMANCE_OFF / PERFORMANCE_ON / PERFORMANCE_IN_EXECUTION */

  /* global data related to or-parallelism */
#if THREADS
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
  struct global_locks locks;
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

#define GLOBAL_MAX_PAGES                        (GLOBAL.pages.max_pages)
#define GLOBAL_PAGES_void                       (GLOBAL.pages.void_pages)
#define GLOBAL_PAGES_or_fr                      (GLOBAL.pages.or_frame_pages)
#define GLOBAL_PAGES_qg_sol_fr                  (GLOBAL.pages.query_goal_solution_frame_pages)
#define GLOBAL_PAGES_qg_ans_fr                  (GLOBAL.pages.query_goal_answer_frame_pages)
#define GLOBAL_PAGES_tg_sol_fr                  (GLOBAL.pages.table_subgoal_solution_frame_pages)
#define GLOBAL_PAGES_tg_ans_fr                  (GLOBAL.pages.table_subgoal_answer_frame_pages)
#define GLOBAL_PAGES_tab_ent                    (GLOBAL.pages.table_entry_pages)
#define GLOBAL_PAGES_sg_fr                      (GLOBAL.pages.subgoal_frame_pages)
#define GLOBAL_PAGES_dep_fr                     (GLOBAL.pages.dependency_frame_pages)
#define GLOBAL_PAGES_sg_node                    (GLOBAL.pages.subgoal_trie_node_pages)
#define GLOBAL_PAGES_ans_node                   (GLOBAL.pages.answer_trie_node_pages)
#define GLOBAL_PAGES_gt_node                    (GLOBAL.pages.global_trie_node_pages)
#define GLOBAL_PAGES_sg_hash                    (GLOBAL.pages.subgoal_trie_hash_pages)
#define GLOBAL_PAGES_ans_hash                   (GLOBAL.pages.answer_trie_hash_pages)
#define GLOBAL_PAGES_gt_hash                    (GLOBAL.pages.global_trie_hash_pages)
#define GLOBAL_PAGES_susp_fr                    (GLOBAL.pages.suspension_frame_pages)
#define SCHEDULER_LOOP                          (GLOBAL.scheduler_loop)
#define DELAYED_RELEASE_LOAD                    (GLOBAL.delayed_release_load)
#define number_workers                          (GLOBAL.number_workers)
#define worker_pid(worker)                      (GLOBAL.worker_pid[worker])
#define GLOBAL_master_worker                    (GLOBAL.master_worker)
#define GLOBAL_execution_time                   (GLOBAL.execution_time)
#define GLOBAL_best_times(time)                 (GLOBAL.best_execution_times[time])
#define GLOBAL_number_goals                     (GLOBAL.number_of_executed_goals)
#define GLOBAL_performance_mode                 (GLOBAL.performance_mode)
#if THREADS
#define Get_GLOBAL_root_cp()	                offset_to_cptr(GLOBAL.root_choice_point_offset)
#define Set_GLOBAL_root_cp(bptr)                (GLOBAL.root_choice_point_offset = cptr_to_offset(bptr))
#else
#define GLOBAL_root_cp                          (GLOBAL.root_choice_point)
#define Get_GLOBAL_root_cp()                    (GLOBAL.root_choice_point)
#define Set_GLOBAL_root_cp(bptr)                (GLOBAL.root_choice_point = (bptr))
#endif
#define GLOBAL_root_or_fr                       (GLOBAL.root_or_frame)
#define GLOBAL_bm_present_workers               (GLOBAL.present_workers)
#define GLOBAL_bm_idle_workers                  (GLOBAL.idle_workers)
#define GLOBAL_bm_root_cp_workers               (GLOBAL.root_cp_workers)
#define GLOBAL_bm_invisible_workers             (GLOBAL.invisible_workers)
#define GLOBAL_bm_requestable_workers           (GLOBAL.requestable_workers)
#define GLOBAL_bm_executing_workers             (GLOBAL.executing_workers)
#define GLOBAL_bm_finished_workers              (GLOBAL.finished_workers)
#define GLOBAL_bm_pruning_workers               (GLOBAL.pruning_workers)
#define GLOBAL_LOCKS_bm_idle_workers            (GLOBAL.locks.bitmap_idle_workers)
#define GLOBAL_LOCKS_bm_root_cp_workers         (GLOBAL.locks.bitmap_root_cp_workers)
#define GLOBAL_LOCKS_bm_invisible_workers       (GLOBAL.locks.bitmap_invisible_workers)
#define GLOBAL_LOCKS_bm_requestable_workers     (GLOBAL.locks.bitmap_requestable_workers)
#define GLOBAL_LOCKS_bm_executing_workers       (GLOBAL.locks.bitmap_executing_workers)
#define GLOBAL_LOCKS_bm_finished_workers        (GLOBAL.locks.bitmap_finished_workers)
#define GLOBAL_LOCKS_bm_pruning_workers         (GLOBAL.locks.bitmap_pruning_workers)
#define GLOBAL_LOCKS_who_locked_heap            (GLOBAL.locks.who_locked_heap)
#define GLOBAL_LOCKS_heap_access                (GLOBAL.locks.heap_access)
#define GLOBAL_LOCKS_alloc_block                (GLOBAL.locks.alloc_block)
#define GLOBAL_branch(worker, depth)            (GLOBAL.branch[worker][depth])
#define PARALLEL_EXECUTION_MODE                 (GLOBAL.parallel_execution_mode)
#define GLOBAL_answers                          (GLOBAL.answers)
#define GLOBAL_root_gt                          (GLOBAL.root_global_trie)
#define GLOBAL_root_tab_ent                     (GLOBAL.root_table_entry)
#define GLOBAL_first_sg_fr                      (GLOBAL.first_subgoal_frame)
#define GLOBAL_last_sg_fr                       (GLOBAL.last_subgoal_frame)
#define GLOBAL_check_sg_fr                      (GLOBAL.check_subgoal_frame)
#define GLOBAL_root_dep_fr                      (GLOBAL.root_dependency_frame)
#define GLOBAL_table_var_enumerator(index)      (GLOBAL.table_var_enumerator[index])
#define GLOBAL_table_var_enumerator_addr(index) (GLOBAL.table_var_enumerator + (index))
#define GLOBAL_table_lock(index)                (GLOBAL.table_lock[index])
#define GLOBAL_timestamp                        (GLOBAL.timestamp)



/***********************************
**      Struct local_signals      **
***********************************/

#ifdef YAPOR
struct local_signals{
#if defined(ENV_COPY) || defined(THREADS)
  lockvar lock;
  volatile enum {
    Q_idle = 0,
    trail  = 1,
    global = 2,
    local  = 3,
    P_idle = 4
  } P_fase, Q_fase;
#endif /* ENV_COPY || THREADS */
  volatile enum {
    no_sharing   = 0, 
    sharing      = 1,
    nodes_shared = 2,
    copy_done    = 3,
    worker_ready = 4
  } reply;
};
#endif /* YAPOR */



/********************************
**      Struct local_data      **
********************************/

struct local_data{
#if defined(YAPOR) || defined(THREADS)
  lockvar lock;
#endif
#ifdef YAPOR
  /* local data related to or-parallelism */
  volatile int load;
#if THREADS
  Int top_choice_point_offset;
#else
  choiceptr top_choice_point;
#endif
  struct or_frame *top_or_frame;
#if THREADS
  Int prune_request_offset;
#else
  choiceptr prune_request;
#endif
  volatile int share_request;
  struct local_signals share_signals;
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
#ifdef THREADS
  Int top_choice_point_on_stack_offset;
#else
  choiceptr top_choice_point_on_stack;
#endif
  struct or_frame *top_or_frame_with_suspensions;
#endif /* YAPOR */
#endif /* TABLING */
};

#define LOCAL_lock                         (LOCAL->lock)
#define LOCAL_load                         (LOCAL->load)
#if THREADS
#define Get_LOCAL_top_cp() offset_to_cptr(LOCAL->top_choice_point_offset)
#define Set_LOCAL_top_cp(cpt) (LOCAL->top_choice_point_offset =  cptr_to_offset(cpt))
#else
#define LOCAL_top_cp                       (LOCAL->top_choice_point)
#define Get_LOCAL_top_cp()		   (LOCAL->top_choice_point)
#define Set_LOCAL_top_cp(cpt)	           (LOCAL->top_choice_point =  cpt)
#endif
#define LOCAL_top_or_fr                    (LOCAL->top_or_frame)
#if THREADS
#define Get_LOCAL_prune_request()	   offset_to_cptr_with_null(LOCAL->prune_request_offset)
#define Set_LOCAL_prune_request(cpt)       (LOCAL->prune_request_offset =  cptr_to_offset_with_null(cpt))
#else
#define LOCAL_prune_request                (LOCAL->prune_request)
#define Get_LOCAL_prune_request()          (LOCAL->prune_request)
#define Set_LOCAL_prune_request(cpt)       (LOCAL->prune_request = cpt)
#endif
#define LOCAL_share_request                (LOCAL->share_request)
#define LOCAL_reply_signal                 (LOCAL->share_signals.reply)
#define LOCAL_p_fase_signal                (LOCAL->share_signals.P_fase)
#define LOCAL_q_fase_signal                (LOCAL->share_signals.Q_fase)
#define LOCAL_lock_signals                 (LOCAL->share_signals.lock)
#define LOCAL_start_global_copy            (LOCAL->global_copy.start)
#define LOCAL_end_global_copy              (LOCAL->global_copy.end)
#define LOCAL_start_local_copy             (LOCAL->local_copy.start)
#define LOCAL_end_local_copy               (LOCAL->local_copy.end)
#define LOCAL_start_trail_copy             (LOCAL->trail_copy.start)
#define LOCAL_end_trail_copy               (LOCAL->trail_copy.end)
#define LOCAL_next_free_ans_node           (LOCAL->next_free_answer_trie_node)
#define LOCAL_top_sg_fr                    (LOCAL->top_subgoal_frame)
#define LOCAL_top_dep_fr                   (LOCAL->top_dependency_frame)
#define LOCAL_pruning_scope                (LOCAL->bottom_pruning_scope)
#if THREADS
#define Get_LOCAL_top_cp_on_stack() offset_to_cptr(LOCAL->top_choice_point_on_stack_offset)
#define Set_LOCAL_top_cp_on_stack(cpt) (LOCAL->top_choice_point_on_stack_offset =  cptr_to_offset(cpt))
#else
#define LOCAL_top_cp_on_stack              (LOCAL->top_choice_point_on_stack)
#define Get_LOCAL_top_cp_on_stack()	   (LOCAL->top_choice_point_on_stack)
#define Set_LOCAL_top_cp_on_stack(cpt)	   (LOCAL->top_choice_point_on_stack =  cpt)
#endif
#define LOCAL_top_susp_or_fr               (LOCAL->top_or_frame_with_suspensions)

#define REMOTE_lock(worker)                (REMOTE[worker].lock)
#define REMOTE_load(worker)                (REMOTE[worker].load)
#if THREADS
#define REMOTE_top_cp(worker)              offset_to_cptr(REMOTE[worker].top_choice_point_offset)
#define Set_REMOTE_top_cp(worker, bptr)    (REMOTE[worker].top_choice_point_offset = cptr_to_offset(bptr))
#else
#define REMOTE_top_cp(worker)              (REMOTE[worker].top_choice_point)
#define Set_REMOTE_top_cp(worker, bptr)    (REMOTE[worker].top_choice_point = (bptr))
#endif
#define REMOTE_top_or_fr(worker)           (REMOTE[worker].top_or_frame)
#if THREADS
#define Get_REMOTE_prune_request(worker)   offset_to_cptr_with_null(REMOTE[worker].prune_request_offset)
#define Set_REMOTE_prune_request(worker,cp)   (REMOTE[worker].prune_request_offset = cptr_to_offset_with_null(cp))
#else
#define REMOTE_prune_request(worker)       (REMOTE[worker].prune_request)
#define Get_REMOTE_prune_request(worker)   (REMOTE[worker].prune_request)
#define Set_REMOTE_prune_request(worker,cp)   (REMOTE[worker].prune_request = cp)
#endif
#define REMOTE_share_request(worker)       (REMOTE[worker].share_request)
#define REMOTE_reply_signal(worker)        (REMOTE[worker].share_signals.reply)
#define REMOTE_p_fase_signal(worker)       (REMOTE[worker].share_signals.P_fase)
#define REMOTE_q_fase_signal(worker)       (REMOTE[worker].share_signals.Q_fase)
#define REMOTE_lock_signals(worker)        (REMOTE[worker].share_signals.lock)
#define REMOTE_start_global_copy(worker)   (REMOTE[worker].global_copy.start)
#define REMOTE_end_global_copy(worker)     (REMOTE[worker].global_copy.end)
#define REMOTE_start_local_copy(worker)    (REMOTE[worker].local_copy.start)
#define REMOTE_end_local_copy(worker)      (REMOTE[worker].local_copy.end)
#define REMOTE_start_trail_copy(worker)    (REMOTE[worker].trail_copy.start)
#define REMOTE_end_trail_copy(worker)      (REMOTE[worker].trail_copy.end)
#define REMOTE_next_free_ans_node(worker)  (REMOTE[worker].next_free_answer_trie_node)
#define REMOTE_top_sg_fr(worker)           (REMOTE[worker].top_subgoal_frame)
#define REMOTE_top_dep_fr(worker)          (REMOTE[worker].top_dependency_frame)
#define REMOTE_pruning_scope(worker)       (REMOTE[worker].bottom_pruning_scope)
#if THREADS
#define REMOTE_top_cp_on_stack(worker)              offset_to_cptr(REMOTE[worker].top_choice_point_on_stack_offset)
#define Set_REMOTE_top_cp_on_stack(worker, bptr)    (REMOTE[worker].top_choice_point_on_stack_offset = cptr_to_offset(bptr))
#else
#define REMOTE_top_cp_on_stack(worker)              (REMOTE[worker].top_choice_point_on_stack)
#define Set_REMOTE_top_cp_on_stack(worker, bptr)    (REMOTE[worker].top_choice_point_on_stack = (bptr))
#endif
#define REMOTE_top_susp_or_fr(worker)      (REMOTE[worker].top_or_frame_with_suspensions)


#ifdef YAPOR
#include "or.structs.h"
#endif /* YAPOR */


#ifdef TABLING
#include "tab.structs.h"
#endif  /* TABLING */
