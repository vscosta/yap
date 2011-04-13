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

/***********************
**      Includes      **
***********************/

#include "Yap.h"
#if defined(YAPOR) || defined(TABLING)
#define OPT_MAVAR_STATIC
#include "Yatom.h"
#include "YapHeap.h"
#include <unistd.h>
#include <signal.h>
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#if defined(TABLING) || !defined(YAPOR_COW)
#ifndef TABLING
#include "opt.mavar.h"
#endif /* !TABLING */
#ifdef MULTI_ASSIGNMENT_VARIABLES
ma_hash_entry Yap_ma_hash_table[MAVARS_HASH_SIZE];
UInt Yap_ma_timestamp;    /* an unsigned int */
ma_h_inner_struct *Yap_ma_h_top;
#endif /* MULTI_ASSIGNMENT_VARIABLES */
#endif /* TABLING || !YAPOR_COW */
#ifdef YAPOR_COW
#include "sys/wait.h"
#endif /* YAPOR_COW */




/************************************************
**      Global variables are defined here      **
************************************************/

#if defined(YAPOR) && ! defined(THREADS)
struct worker WORKER;
#endif /* YAPOR && ! THREADS */



/*********************
**      Macros      **
*********************/

#ifdef SHM_MEMORY_ALLOC_SCHEME
#define STRUCTS_PER_PAGE(STR_TYPE)  ((Yap_page_size - STRUCT_SIZE(struct page_header)) / STRUCT_SIZE(STR_TYPE))

#define INIT_PAGES(PG, STR_TYPE)                         \
        INIT_LOCK(Pg_lock(PG));                          \
        Pg_pg_alloc(PG) = 0;                             \
        Pg_str_in_use(PG) = 0;                           \
        Pg_str_per_pg(PG) = STRUCTS_PER_PAGE(STR_TYPE);  \
        Pg_free_pg(PG) = NULL
#else
#define INIT_PAGES(PG, STR_TYPE)  Pg_str_in_use(PG) = 0
#endif /* SHM_MEMORY_ALLOC_SCHEME */



/*******************************
**      Global functions      **
*******************************/

void Yap_init_optyap_global(int max_table_size, int n_workers, int sch_loop, int delay_load) {
  int i;

  /* global data related to memory management */
#ifdef LIMIT_TABLING
  if (max_table_size)
    Yap_max_pages = ((max_table_size - 1) * 1024 * 1024 / SHMMAX + 1) * SHMMAX / Yap_page_size;
  else
    Yap_max_pages = -1;
#endif /* LIMIT_TABLING */
  INIT_PAGES(Yap_pages_void, void *);      
#ifdef YAPOR
  INIT_PAGES(Yap_pages_or_fr , struct or_frame);
  INIT_PAGES(Yap_pages_qg_sol_fr , struct query_goal_solution_frame);
  INIT_PAGES(Yap_pages_qg_ans_fr, struct query_goal_answer_frame);
#endif /* YAPOR */
#ifdef TABLING_INNER_CUTS
  INIT_PAGES(Yap_pages_tg_sol_fr, struct table_subgoal_solution_frame);
  INIT_PAGES(Yap_pages_tg_ans_fr, struct table_subgoal_answer_frame);
#endif /* TABLING_INNER_CUTS */
#ifdef TABLING
  INIT_PAGES(Yap_pages_tab_ent, struct table_entry);
  INIT_PAGES(Yap_pages_sg_fr, struct subgoal_frame);
  INIT_PAGES(Yap_pages_dep_fr, struct dependency_frame);
  INIT_PAGES(Yap_pages_sg_node, struct subgoal_trie_node);
  INIT_PAGES(Yap_pages_ans_node, struct answer_trie_node);
  INIT_PAGES(Yap_pages_gt_node, struct global_trie_node);
  INIT_PAGES(Yap_pages_sg_hash, struct subgoal_trie_hash);
  INIT_PAGES(Yap_pages_ans_hash, struct answer_trie_hash);
  INIT_PAGES(Yap_pages_gt_hash, struct global_trie_hash);
#endif /* TABLING */
#if defined(YAPOR) && defined(TABLING)
  INIT_PAGES(Yap_pages_susp_fr, struct suspension_frame);
#endif /* YAPOR && TABLING */

#ifdef YAPOR
  /* global static data */
  Yap_number_workers= n_workers;
  Yap_worker_pid(0) = getpid();
  for (i = 1; i < Yap_number_workers; i++) Yap_worker_pid(i) = 0;
  Yap_scheduler_loop = sch_loop;
  Yap_delayed_release_load = delay_load;

  /* global data related to or-performance */
  Yap_number_goals = 0;
  Yap_best_times(0) = 0;
  Yap_performance_mode = PERFORMANCE_OFF;

  /* global data related to or-parallelism */
  BITMAP_clear(Yap_bm_present_workers);
  for (i = 0; i < Yap_number_workers; i++) 
    BITMAP_insert(Yap_bm_present_workers, i);
  BITMAP_copy(Yap_bm_idle_workers, Yap_bm_present_workers);
  BITMAP_clear(Yap_bm_root_cp_workers);
  BITMAP_clear(Yap_bm_invisible_workers);
  BITMAP_clear(Yap_bm_requestable_workers);
  BITMAP_clear(Yap_bm_executing_workers);
  BITMAP_copy(Yap_bm_finished_workers, Yap_bm_present_workers);
  INIT_LOCK(Yap_locks_bm_idle_workers);
  INIT_LOCK(Yap_locks_bm_root_cp_workers);
  INIT_LOCK(Yap_locks_bm_invisible_workers);
  INIT_LOCK(Yap_locks_bm_requestable_workers);
  INIT_LOCK(Yap_locks_bm_executing_workers);
  INIT_LOCK(Yap_locks_bm_finished_workers);
#ifdef TABLING_INNER_CUTS
  INIT_LOCK(Yap_locks_bm_pruning_workers);
#endif /* TABLING_INNER_CUTS */
  Yap_locks_who_locked_heap = MAX_WORKERS;
  INIT_LOCK(Yap_locks_heap_access);
  INIT_LOCK(Yap_locks_alloc_block);
  if (Yap_number_workers== 1)
    Yap_parallel_execution_mode = FALSE;
  else
    Yap_parallel_execution_mode = TRUE;
#endif /* YAPOR */

#ifdef TABLING
  /* global data related to tabling */
  new_global_trie_node(Yap_root_gt, 0, NULL, NULL, NULL);
  Yap_root_tab_ent = NULL;
#ifdef LIMIT_TABLING
  Yap_first_sg_fr = NULL;
  Yap_last_sg_fr = NULL;
  Yap_check_sg_fr = NULL;
#endif /* LIMIT_TABLING */
  Yap_root_dep_fr = NULL;
  for (i = 0; i < MAX_TABLE_VARS; i++) {
    CELL *pt = Yap_table_var_enumerator_addr(i);
    RESET_VARIABLE(pt);
  }
#ifdef TABLE_LOCK_AT_WRITE_LEVEL
  for (i = 0; i < TABLE_LOCK_BUCKETS; i++)
    INIT_LOCK(Yap_table_lock(i));
#endif /* TABLE_LOCK_AT_WRITE_LEVEL */
#endif /* TABLING */

  return;
}


void Yap_init_local(void) {
#ifdef YAPOR
  CACHE_REGS
  /* local data related to or-parallelism */
  LOCAL = REMOTE + worker_id;
  Set_LOCAL_top_cp((choiceptr) Yap_LocalBase);
  LOCAL_top_or_fr = Yap_root_or_fr;
  LOCAL_load = 0;
  LOCAL_share_request = MAX_WORKERS;
  LOCAL_reply_signal = worker_ready;
#ifdef YAPOR_COPY
  INIT_LOCK(LOCAL_lock_signals);
#endif /* YAPOR_COPY */
  Set_LOCAL_prune_request(NULL);
#endif /* YAPOR */
  INIT_LOCK(LOCAL_lock);
#ifdef TABLING
  /* local data related to tabling */
  LOCAL_next_free_ans_node = NULL;
  LOCAL_top_sg_fr = NULL; 
  LOCAL_top_dep_fr = Yap_root_dep_fr; 
#ifdef YAPOR
  Set_LOCAL_top_cp_on_stack((choiceptr) Yap_LocalBase); /* ??? */
  LOCAL_top_susp_or_fr = Yap_root_or_fr;
#endif /* YAPOR */
#endif /* TABLING */
  return;
}


void make_root_frames(void) {
#ifdef YAPOR
  CACHE_REGS
  /* root or frame */
  or_fr_ptr or_fr;

  ALLOC_OR_FRAME(or_fr);   
  INIT_LOCK(OrFr_lock(or_fr));
  OrFr_alternative(or_fr) = NULL;
  BITMAP_copy(OrFr_members(or_fr), Yap_bm_present_workers);
  SetOrFr_node(or_fr, (choiceptr) Yap_LocalBase);
  OrFr_nearest_livenode(or_fr) = NULL;
  OrFr_depth(or_fr) = 0;
  Set_OrFr_pend_prune_cp(or_fr, NULL);
  OrFr_nearest_leftnode(or_fr) = or_fr;
  OrFr_qg_solutions(or_fr) = NULL;
#ifdef TABLING_INNER_CUTS
  OrFr_tg_solutions(or_fr) = NULL;
#endif /* TABLING_INNER_CUTS */
#ifdef TABLING
  OrFr_owners(or_fr) = Yap_number_workers;
  OrFr_next_on_stack(or_fr) = NULL;
  OrFr_suspensions(or_fr) = NULL;
  OrFr_nearest_suspnode(or_fr) = or_fr;
#endif /* TABLING */
  OrFr_next(or_fr) = NULL;
  Yap_root_or_fr = or_fr;
#endif /* YAPOR */

#ifdef TABLING
  /* root dependency frame */
  if (!Yap_root_dep_fr) {
    new_dependency_frame(Yap_root_dep_fr, FALSE, NULL, NULL, NULL, NULL, NULL);
#ifdef TABLING
    DepFr_cons_cp(Yap_root_dep_fr) = B;
#endif /* TABLING */
  }
#endif /* TABLING */
}

#ifdef YAPOR
void init_workers(void) {
  CACHE_REGS
  int proc;
#ifdef THREADS
  return;
#endif
#ifdef YAPOR_COW
  if (Yap_number_workers> 1) {
    int son;
    son = fork();
    if (son == -1)
      Yap_Error(FATAL_ERROR, TermNil, "fork error (init_workers)");
    if (son > 0) {
      /* I am the father, I must stay here and wait for my children to all die */
      struct sigaction sigact;

      Yap_master_worker = getpid();
      sigact.sa_handler = SIG_DFL;
      sigemptyset(&sigact.sa_mask);
      sigact.sa_flags = SA_RESTART;
      sigaction(SIGINT, &sigact, NULL);
      pause();
      exit(0);
    } else Yap_worker_pid(0) = getpid();
  }
#endif /* YAPOR_COW */
  for (proc = 1; proc < Yap_number_workers; proc++) {
    int son;
    son = fork();
    if (son == -1)
      Yap_Error(FATAL_ERROR, TermNil, "fork error (init_workers)");
    if (son == 0) { 
      /* new worker */
      worker_id = proc;
      remap_memory();
      break;
    }
    else Yap_worker_pid(proc) = son;
  }
}
#endif /* YAPOR */


void itos(int i, char *s) {
  int n,r,j;
  n = 10;
  while (n <= i) n *= 10;
  j = 0;
  while (n > 1) {
    n = n / 10;   
    r = i / n;
    i = i - r * n;
    s[j++] = r + '0';
  }
  s[j] = 0;
  return;
}
#endif /* YAPOR || TABLING */
