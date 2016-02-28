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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef YAPOR
#include "or.macros.h"
#endif /* YAPOR */
#ifdef TABLING
#include "tab.macros.h"
#elif !defined(YAPOR_COW)
#include "opt.mavar.h"
#endif /* TABLING */
#ifdef YAPOR_COW
#include "sys/wait.h"
#endif /* YAPOR_COW */

/*********************
**      Macros      **
*********************/

#ifdef USE_PAGES_MALLOC
#define STRUCTS_PER_PAGE(STR_TYPE)                                             \
  ((Yap_page_size - ADJUST_SIZE(sizeof(struct page_header))) /                 \
   ADJUST_SIZE(sizeof(STR_TYPE)))

#define INIT_GLOBAL_PAGE_ENTRY(PG, STR_TYPE)                                   \
  INIT_LOCK(PgEnt_lock(PG));                                                   \
  PgEnt_pages_in_use(PG) = 0;                                                  \
  PgEnt_strs_in_use(PG) = 0;                                                   \
  PgEnt_strs_per_page(PG) = STRUCTS_PER_PAGE(STR_TYPE);                        \
  PgEnt_first(PG) = NULL;                                                      \
  PgEnt_last(PG) = NULL;
#define INIT_LOCAL_PAGE_ENTRY(PG, STR_TYPE)                                    \
  PgEnt_pages_in_use(PG) = 0;                                                  \
  PgEnt_strs_in_use(PG) = 0;                                                   \
  PgEnt_strs_per_page(PG) = STRUCTS_PER_PAGE(STR_TYPE);                        \
  PgEnt_first(PG) = NULL;                                                      \
  PgEnt_last(PG) = NULL;
#else
#define INIT_GLOBAL_PAGE_ENTRY(PG, STR_TYPE) PgEnt_strs_in_use(PG) = 0
#define INIT_LOCAL_PAGE_ENTRY(PG, STR_TYPE) PgEnt_strs_in_use(PG) = 0
#endif /* USE_PAGES_MALLOC */

/*******************************
**      Global functions      **
*******************************/

void Yap_init_global_optyap_data(int max_table_size, int n_workers,
                                 int sch_loop, int delay_load) {
  int i;

/* global data related to memory management */
#ifdef USE_PAGES_MALLOC
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_alloc, void *);
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_void, void *);
#endif /* USE_PAGES_MALLOC */
#ifdef TABLING
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_tab_ent, struct table_entry);
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_sg_ent, struct subgoal_entry);
#endif
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_sg_fr, struct subgoal_frame);
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_dep_fr, struct dependency_frame);
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_sg_node, struct subgoal_trie_node);
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_sg_hash, struct subgoal_trie_hash);
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_ans_node, struct answer_trie_node);
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_ans_hash, struct answer_trie_hash);
#if defined(THREADS_FULL_SHARING)
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_ans_ref_node, struct answer_ref_node);
#endif
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_gt_node, struct global_trie_node);
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_gt_hash, struct global_trie_hash);
#endif /* TABLING */
#ifdef YAPOR
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_or_fr, struct or_frame);
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_qg_sol_fr,
                         struct query_goal_solution_frame);
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_qg_ans_fr,
                         struct query_goal_answer_frame);
#ifdef TABLING
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_susp_fr, struct suspension_frame);
#endif
#ifdef TABLING_INNER_CUTS
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_tg_sol_fr,
                         struct table_subgoal_solution_frame);
  INIT_GLOBAL_PAGE_ENTRY(GLOBAL_pages_tg_ans_fr,
                         struct table_subgoal_answer_frame);
#endif
#endif /* YAPOR */

#ifdef YAPOR
  /* global static data */
  GLOBAL_number_workers = n_workers;
  GLOBAL_worker_pid(0) = getpid();
  for (i = 1; i < GLOBAL_number_workers; i++)
    GLOBAL_worker_pid(i) = 0;
  GLOBAL_scheduler_loop = sch_loop;
  GLOBAL_delayed_release_load = delay_load;

  /* global data related to or-parallelism */
  ALLOC_OR_FRAME(GLOBAL_root_or_fr);
  BITMAP_clear(GLOBAL_bm_present_workers);
  for (i = 0; i < GLOBAL_number_workers; i++)
    BITMAP_insert(GLOBAL_bm_present_workers, i);
  BITMAP_copy(GLOBAL_bm_idle_workers, GLOBAL_bm_present_workers);
  BITMAP_clear(GLOBAL_bm_root_cp_workers);
  BITMAP_clear(GLOBAL_bm_invisible_workers);
  BITMAP_clear(GLOBAL_bm_requestable_workers);
  BITMAP_copy(GLOBAL_bm_finished_workers, GLOBAL_bm_present_workers);
  INIT_LOCK(GLOBAL_locks_bm_idle_workers);
  INIT_LOCK(GLOBAL_locks_bm_root_cp_workers);
  INIT_LOCK(GLOBAL_locks_bm_invisible_workers);
  INIT_LOCK(GLOBAL_locks_bm_requestable_workers);
  INIT_LOCK(GLOBAL_locks_bm_finished_workers);
#ifdef TABLING_INNER_CUTS
  INIT_LOCK(GLOBAL_locks_bm_pruning_workers);
#endif /* TABLING_INNER_CUTS */
  GLOBAL_locks_who_locked_heap = MAX_WORKERS;
  INIT_LOCK(GLOBAL_locks_heap_access);
  INIT_LOCK(GLOBAL_locks_alloc_block);
  if (GLOBAL_number_workers == 1)
    GLOBAL_parallel_mode = PARALLEL_MODE_OFF;
  else
    GLOBAL_parallel_mode = PARALLEL_MODE_ON;
#endif /* YAPOR */

#ifdef TABLING
  /* global data related to tabling */
  GLOBAL_root_gt = NULL;
  GLOBAL_root_tab_ent = NULL;
#ifdef LIMIT_TABLING
  if (max_table_size)
    GLOBAL_max_pages = ((max_table_size - 1) * 1024 * 1024 / SHMMAX + 1) *
                       SHMMAX / Yap_page_size;
  else
    GLOBAL_max_pages = -1;
  GLOBAL_first_sg_fr = NULL;
  GLOBAL_last_sg_fr = NULL;
  GLOBAL_check_sg_fr = NULL;
#endif /* LIMIT_TABLING */
#ifdef YAPOR
  new_dependency_frame(GLOBAL_root_dep_fr, FALSE, NULL, NULL, NULL, NULL, FALSE,
                       NULL);
#endif /* YAPOR */
  for (i = 0; i < MAX_TABLE_VARS; i++) {
    CELL *pt = GLOBAL_table_var_enumerator_addr(i);
    RESET_VARIABLE(pt);
  }
#ifdef TRIE_LOCK_USING_GLOBAL_ARRAY
  for (i = 0; i < TRIE_LOCK_BUCKETS; i++)
    INIT_LOCK(GLOBAL_trie_locks(i));
#endif /* TRIE_LOCK_USING_GLOBAL_ARRAY */
#endif /* TABLING */

  return;
}

void Yap_init_local_optyap_data(int wid) {
#if defined(YAPOR_THREADS) || defined(THREADS_CONSUMER_SHARING)
  CACHE_REGS
#endif /* YAPOR_THREADS || THREADS_CONSUMER_SHARING */

#if defined(TABLING) && (defined(YAPOR) || defined(THREADS))
/* local data related to memory management */
#ifdef YAPOR
  REMOTE_next_free_ans_node(wid) = NULL;
#elif THREADS
#ifdef USE_PAGES_MALLOC
  INIT_LOCAL_PAGE_ENTRY(REMOTE_pages_void(wid), void *);
#endif
  INIT_LOCAL_PAGE_ENTRY(REMOTE_pages_tab_ent(wid), struct table_entry);
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
  INIT_LOCAL_PAGE_ENTRY(REMOTE_pages_sg_ent(wid), struct subgoal_entry);
#endif
  INIT_LOCAL_PAGE_ENTRY(REMOTE_pages_sg_fr(wid), struct subgoal_frame);
  INIT_LOCAL_PAGE_ENTRY(REMOTE_pages_dep_fr(wid), struct dependency_frame);
  INIT_LOCAL_PAGE_ENTRY(REMOTE_pages_sg_node(wid), struct subgoal_trie_node);
  INIT_LOCAL_PAGE_ENTRY(REMOTE_pages_sg_hash(wid), struct subgoal_trie_hash);
  INIT_LOCAL_PAGE_ENTRY(REMOTE_pages_ans_node(wid), struct answer_trie_node);
  INIT_LOCAL_PAGE_ENTRY(REMOTE_pages_ans_hash(wid), struct answer_trie_hash);
#if defined(THREADS_FULL_SHARING)
  INIT_LOCAL_PAGE_ENTRY(REMOTE_pages_ans_ref_node(wid), struct answer_ref_node);
#endif
  INIT_LOCAL_PAGE_ENTRY(REMOTE_pages_gt_node(wid), struct global_trie_node);
  INIT_LOCAL_PAGE_ENTRY(REMOTE_pages_gt_hash(wid), struct global_trie_hash);
#endif
#endif /* TABLING && (YAPOR || THREADS) */

#ifdef YAPOR
  /* local data related to or-parallelism */
  Set_REMOTE_top_cp(wid, (choiceptr)LOCAL_LocalBase);
  REMOTE_top_or_fr(wid) = GLOBAL_root_or_fr;
  REMOTE_load(wid) = 0;
  REMOTE_share_request(wid) = MAX_WORKERS;
  REMOTE_reply_signal(wid) = worker_ready;
#ifdef YAPOR_COPY
  INIT_LOCK(REMOTE_lock_signals(wid));
#endif /* YAPOR_COPY */
  Set_REMOTE_prune_request(wid, NULL);
  INIT_LOCK(REMOTE_lock(wid));
#endif /* YAPOR */

#ifdef TABLING
  /* local data related to tabling */
  REMOTE_top_sg_fr(wid) = NULL;
  REMOTE_top_dep_fr(wid) = NULL;
#ifdef YAPOR
  REMOTE_top_dep_fr(wid) = GLOBAL_root_dep_fr;
  Set_REMOTE_top_cp_on_stack(wid, (choiceptr)LOCAL_LocalBase); /* ??? */
  REMOTE_top_susp_or_fr(wid) = GLOBAL_root_or_fr;
#endif /* YAPOR */
#ifdef THREADS_CONSUMER_SHARING
  ThDepFr_terminator(GLOBAL_th_dep_fr(wid)) = 0;
  ThDepFr_next(GLOBAL_th_dep_fr(wid)) = wid;
  ThDepFr_state(GLOBAL_th_dep_fr(wid)) = working;
  INIT_LOCK(ThDepFr_lock(GLOBAL_th_dep_fr(wid)));
#endif /* THREADS_CONSUMER_SHARING */
#endif /* TABLING */
  return;
}

void Yap_init_root_frames(void) {
  CACHE_REGS

#ifdef YAPOR
  /* root or frame */
  or_fr_ptr or_fr = GLOBAL_root_or_fr;
  INIT_LOCK(OrFr_lock(or_fr));
  OrFr_alternative(or_fr) = NULL;
  BITMAP_copy(OrFr_members(or_fr), GLOBAL_bm_present_workers);
  SetOrFr_node(or_fr, (choiceptr)LOCAL_LocalBase);
  OrFr_nearest_livenode(or_fr) = NULL;
  OrFr_depth(or_fr) = 0;
  Set_OrFr_pend_prune_cp(or_fr, NULL);
  OrFr_nearest_leftnode(or_fr) = or_fr;
  OrFr_qg_solutions(or_fr) = NULL;
#ifdef TABLING_INNER_CUTS
  OrFr_tg_solutions(or_fr) = NULL;
#endif /* TABLING_INNER_CUTS */
#ifdef TABLING
  OrFr_owners(or_fr) = GLOBAL_number_workers;
  OrFr_next_on_stack(or_fr) = NULL;
  OrFr_suspensions(or_fr) = NULL;
  OrFr_nearest_suspnode(or_fr) = or_fr;
#endif /* TABLING */
  OrFr_next(or_fr) = NULL;
#endif /* YAPOR */

#ifdef TABLING
  /* root global trie node */
  new_global_trie_node(GLOBAL_root_gt, 0, NULL, NULL, NULL);
/* root dependency frame */
#ifdef YAPOR
  DepFr_cons_cp(GLOBAL_root_dep_fr) = B; /* with YAPOR, at that point,
                                            LOCAL_top_dep_fr shouldn't be the
                                            same as GLOBAL_root_dep_fr ? */
#else
  new_dependency_frame(LOCAL_top_dep_fr, FALSE, NULL, NULL, B, NULL, FALSE,
                       NULL);
#endif /* YAPOR */
#endif /* TABLING */
}

void itos(int i, char *s) {
  int n, r, j;
  n = 10;
  while (n <= i)
    n *= 10;
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
