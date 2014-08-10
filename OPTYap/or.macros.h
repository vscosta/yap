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

#ifndef OR_MACROS_H
#define OR_MACROS_H

/* -------------------- **
**      Prototypes      **
** -------------------- */

/* get a def for NULL */
#include <stdlib.h>

static inline void PUT_IN_ROOT_NODE(int);
static inline void PUT_OUT_ROOT_NODE(int);
static inline void PUT_IN_FINISHED(int);
#ifdef TABLING_INNER_CUTS
static inline void PUT_IN_PRUNING(int);
static inline void PUT_OUT_PRUNING(int);
#endif /* TABLING_INNER_CUTS */

static inline void PUT_IN_REQUESTABLE(int);
static inline void PUT_OUT_REQUESTABLE(int);
static inline void SCH_update_local_or_tops(void);
static inline void SCH_refuse_share_request_if_any(void);
static inline void SCH_set_load(choiceptr);
static inline void SCH_new_alternative(yamop *,yamop *);

static inline void CUT_send_prune_request(int, choiceptr);
static inline void CUT_reset_prune_request(void);

static inline int CUT_last_worker_left_pending_prune(or_fr_ptr);
static inline or_fr_ptr CUT_leftmost_or_frame(void);
#ifdef TABLING_INNER_CUTS
static inline or_fr_ptr CUT_leftmost_until(or_fr_ptr, int);
#endif /* TABLING_INNER_CUTS */

static inline void CUT_store_answer(or_fr_ptr, qg_ans_fr_ptr);
static inline void CUT_store_answers(or_fr_ptr, qg_sol_fr_ptr);
static inline void CUT_join_answers_in_an_unique_frame(qg_sol_fr_ptr);
static inline void CUT_free_solution_frame(qg_sol_fr_ptr);
static inline void CUT_free_solution_frames(qg_sol_fr_ptr);
static inline qg_sol_fr_ptr CUT_prune_solution_frames(qg_sol_fr_ptr, int);



/* ---------------------------- **
**      Instruction Macros      **
** ---------------------------- */

#if SIZEOF_INT == 2
#define YAMOP_CUT_FLAG    0x8000
#define YAMOP_SEQ_FLAG    0x4000
#define YAMOP_FLAGS_BITS  0xc000
#define YAMOP_LTT_BITS    0x3fff
#elif SIZEOF_INT == 4
#define YAMOP_CUT_FLAG    0x80000000
#define YAMOP_SEQ_FLAG    0x40000000
#define YAMOP_FLAGS_BITS  0xc0000000
#define YAMOP_LTT_BITS    0x3fffffff
#elif SIZEOF_INT == 8
#define YAMOP_CUT_FLAG    0x8000000000000000
#define YAMOP_SEQ_FLAG    0x4000000000000000
#define YAMOP_FLAGS_BITS  0xc000000000000000
#define YAMOP_LTT_BITS    0x3fffffffffffffff
#else
#define YAMOP_CUT_FLAG    OOOOPPS!!! Unknown Integer Sizeof
#define YAMOP_SEQ_FLAG    OOOOPPS!!! Unknown Integer Sizeof
#define YAMOP_FLAGS_BITS  OOOOPPS!!! Unknown Integer Sizeof
#define YAMOP_LTT_BITS    OOOOPPS!!! Unknown Integer Sizeof
#endif /* SIZEOF_INT */

#define YAMOP_OR_ARG(INST)         ((INST)->y_u.Otapl.or_arg)
#define YAMOP_LTT(INST)            (((INST)->y_u.Otapl.or_arg) & YAMOP_LTT_BITS)
#define YAMOP_SEQ(INST)            (((INST)->y_u.Otapl.or_arg) & YAMOP_SEQ_FLAG)
#define YAMOP_CUT(INST)            (((INST)->y_u.Otapl.or_arg) & YAMOP_CUT_FLAG)
#define YAMOP_FLAGS(INST)          (((INST)->y_u.Otapl.or_arg) & YAMOP_FLAGS_BITS)

#define INIT_YAMOP_LTT(INST, LTT)  ((INST)->y_u.Otapl.or_arg = LTT+1)
#define PUT_YAMOP_LTT(INST, LTT)   (INST)->y_u.Otapl.or_arg = YAMOP_FLAGS(INST) | (LTT+1)
#define PUT_YAMOP_SEQ(INST)        (INST)->y_u.Otapl.or_arg |= YAMOP_SEQ_FLAG
#define PUT_YAMOP_CUT(INST)        (INST)->y_u.Otapl.or_arg |= YAMOP_CUT_FLAG

#define BRANCH(WORKER, DEPTH)      GLOBAL_branch(WORKER, DEPTH)
#define BRANCH_LTT(WORKER, DEPTH)  (BRANCH(WORKER, DEPTH) & YAMOP_LTT_BITS)
#define BRANCH_CUT(WORKER, DEPTH)  (BRANCH(WORKER, DEPTH) & YAMOP_CUT_FLAG)



/* ------------------------------ **
**      Parallel Mode Macros      **
** ------------------------------ */

#define PARALLEL_MODE_OFF     0
#define PARALLEL_MODE_ON      1
#define PARALLEL_MODE_RUNNING 2



/* ----------------------- **
**      Engine Macros      **
** ----------------------- */

#define worker_offset(X)	  ((GLOBAL_number_workers + X - worker_id) % GLOBAL_number_workers * Yap_worker_area_size)

#define LOCK_OR_FRAME(fr)      LOCK(OrFr_lock(fr))
#define UNLOCK_OR_FRAME(fr)  UNLOCK(OrFr_lock(fr))

#define LOCK_WORKER(w)        LOCK(REMOTE_lock(w))
#define UNLOCK_WORKER(w)    UNLOCK(REMOTE_lock(w))



/* -------------------------- **
**      Scheduler Macros      **
** -------------------------- */

#define SCH_top_shared_cp(CP)  (Get_LOCAL_top_cp() == CP)

#define SCH_any_share_request  (LOCAL_share_request != MAX_WORKERS)

#define SCHEDULER_GET_WORK()          \
        if (get_work())               \
          goto shared_fail;           \
        else                          \
          goto shared_end

#define SCH_check_prune_request()     \
  if (Get_LOCAL_prune_request()) {    \
          SCHEDULER_GET_WORK();       \
        }

#if defined(YAPOR_COPY) || defined(YAPOR_SBA) || defined(YAPOR_THREADS)
#define SCH_check_share_request()     \
        if (SCH_any_share_request) {  \
	  ASP = YENV;                 \
	  saveregs();                 \
          p_share_work();             \
	  setregs();                  \
        }
#else /* YAPOR_COW */
#define SCH_check_share_request()     \
        if (SCH_any_share_request) {  \
          if (! p_share_work())       \
            goto shared_fail;         \
        }
#endif /* YAPOR_COPY || YAPOR_SBA || YAPOR_COW || YAPOR_THREADS */

#define SCH_check_requests()          \
        SCH_check_prune_request();    \
        SCH_check_share_request()

#define SCH_last_alternative(curpc, CP_PTR)    \
        HR = HBREG = PROTECT_FROZEN_H(CP_PTR); \
        CPREG = CP_PTR->cp_cp;		       \
        ENV = CP_PTR->cp_env;                  \
        SCH_new_alternative(curpc, NULL)



/* -------------------- **
**      Cut Macros      **
** -------------------- */

#define CUT_prune_to(PRUNE_CP)                           \
        if (YOUNGER_CP(Get_LOCAL_top_cp(), PRUNE_CP)) {  \
          if (! Get_LOCAL_prune_request())               \
	    prune_shared_branch(PRUNE_CP, NULL);         \
	  PRUNE_CP = Get_LOCAL_top_cp();	         \
        }

#define CUT_wait_leftmost()                                           \
        if (GLOBAL_parallel_mode == PARALLEL_MODE_RUNNING) {          \
           /* parallel execution mode --> wait until leftmost */      \
           int i, loop, depth, ltt;                                   \
           bitmap members;                                            \
           or_fr_ptr leftmost_or_fr;                                  \
           leftmost_or_fr = LOCAL_top_or_fr;                          \
           do {                                                       \
             depth = OrFr_depth(leftmost_or_fr);                      \
             ltt = BRANCH_LTT(worker_id, depth);                      \
             do {                                                     \
               loop = FALSE;                                          \
               SCH_check_requests();                                  \
               BITMAP_copy(members, OrFr_members(leftmost_or_fr));    \
               BITMAP_delete(members, worker_id);                     \
               for (i = 0; i < GLOBAL_number_workers; i++) {                 \
                 /*  not leftmost in current frame if there is a  */  \
	         /* worker in a left branch and it is not idle or */  \
                 /*     if it is idle it is in a younger node     */  \
                 if (BITMAP_member(members, i) &&                     \
                     BRANCH_LTT(i, depth) > ltt &&                    \
                     (! BITMAP_member(GLOBAL_bm_idle_workers, i) ||   \
                      leftmost_or_fr != REMOTE_top_or_fr(i))) {       \
                   loop = TRUE;                                       \
                   break;                                             \
                 }                                                    \
               }                                                      \
             } while (loop);                                          \
             leftmost_or_fr = OrFr_nearest_leftnode(leftmost_or_fr);  \
           } while (leftmost_or_fr != GLOBAL_root_or_fr);             \
         }



/* ---------------------- **
**      Engine Stuff      **
** ---------------------- */
static inline
void PUT_IN_ROOT_NODE(int worker_num) {
  LOCK(GLOBAL_locks_bm_root_cp_workers);
  BITMAP_insert(GLOBAL_bm_root_cp_workers, worker_num);
  UNLOCK(GLOBAL_locks_bm_root_cp_workers);
  return;
}


static inline
void PUT_OUT_ROOT_NODE(int worker_num) {
  LOCK(GLOBAL_locks_bm_root_cp_workers);
  BITMAP_delete(GLOBAL_bm_root_cp_workers, worker_num);
  UNLOCK(GLOBAL_locks_bm_root_cp_workers);
  return;
}

static inline 
void PUT_IN_FINISHED(int w) {
  LOCK(GLOBAL_locks_bm_finished_workers);
  BITMAP_insert(GLOBAL_bm_finished_workers, w);
  UNLOCK(GLOBAL_locks_bm_finished_workers);
  return;
}


#ifdef TABLING_INNER_CUTS
static inline 
void PUT_IN_PRUNING(int w) {
  LOCK(GLOBAL_locks_bm_pruning_workers);
  BITMAP_insert(GLOBAL_bm_pruning_workers, w);
  UNLOCK(GLOBAL_locks_bm_pruning_workers);
  return;
}


static inline 
void PUT_OUT_PRUNING(int w) {
  LOCK(GLOBAL_locks_bm_pruning_workers);
  BITMAP_delete(GLOBAL_bm_pruning_workers, w);
  UNLOCK(GLOBAL_locks_bm_pruning_workers);
  return;
}
#endif /* TABLING_INNER_CUTS */



/* ------------------------- **
**      Scheduler Stuff      **
** ------------------------- */

static inline 
void PUT_IN_REQUESTABLE(int p) {
  LOCK(GLOBAL_locks_bm_requestable_workers);
  BITMAP_insert(GLOBAL_bm_requestable_workers, p);
  UNLOCK(GLOBAL_locks_bm_requestable_workers);
  return;
}


static inline 
void PUT_OUT_REQUESTABLE(int p) {
  LOCK(GLOBAL_locks_bm_requestable_workers);
  BITMAP_delete(GLOBAL_bm_requestable_workers, p);
  UNLOCK(GLOBAL_locks_bm_requestable_workers);
  return;
}


static inline 
void SCH_update_local_or_tops(void) {
  CACHE_REGS
  Set_LOCAL_top_cp(Get_LOCAL_top_cp()->cp_b);
  LOCAL_top_or_fr = Get_LOCAL_top_cp()->cp_or_fr;
  return;
}


static inline
void SCH_refuse_share_request_if_any(void)  {
  CACHE_REGS
  if (SCH_any_share_request) {
    REMOTE_reply_signal(LOCAL_share_request) = no_sharing;
    LOCAL_share_request = MAX_WORKERS;
    PUT_OUT_REQUESTABLE(worker_id);
  }
  return;
}


static inline 
void SCH_set_load(choiceptr current_cp) {
  CACHE_REGS
  Int lub;  /* local untried branches */
  choiceptr previous_cp = current_cp->cp_b;

#define INIT_CP_LUB(CP, LUB)  CP->cp_or_fr = (struct or_frame *)(LUB)
#define CP_LUB(CP)            (Int)(CP->cp_or_fr)

  if (SCH_top_shared_cp(previous_cp))
    lub = 0;  
  else if (YAMOP_SEQ(previous_cp->cp_ap))
    lub = CP_LUB(previous_cp);
  else
    lub = CP_LUB(previous_cp) + YAMOP_LTT(previous_cp->cp_ap);
  INIT_CP_LUB(current_cp, lub);

  if (YAMOP_SEQ(current_cp->cp_ap))
    LOCAL_load = lub;
  else
    LOCAL_load = lub + YAMOP_LTT(current_cp->cp_ap);
  return;
}

static inline
void SCH_new_alternative(yamop *curpc, yamop *new) {
  CACHE_REGS
  OrFr_alternative(LOCAL_top_or_fr) = new;
  BRANCH(worker_id, OrFr_depth(LOCAL_top_or_fr)) = YAMOP_OR_ARG(curpc);
  UNLOCK_OR_FRAME(LOCAL_top_or_fr);
  return;
}



/* ---------------------------- **
**      Cut Stuff: Pruning      **
** ---------------------------- */

static inline
void CUT_send_prune_request(int worker, choiceptr prune_cp) {
  LOCK_WORKER(worker);
  if (YOUNGER_CP(REMOTE_top_cp(worker), prune_cp) &&
     (! Get_REMOTE_prune_request(worker) || YOUNGER_CP(Get_REMOTE_prune_request(worker), prune_cp)))
    Set_REMOTE_prune_request(worker, prune_cp);
  UNLOCK_WORKER(worker);
  return;
}


static inline
void CUT_reset_prune_request(void) {
  CACHE_REGS
  LOCK_WORKER(worker_id); 
  if (Get_LOCAL_prune_request() && EQUAL_OR_YOUNGER_CP(Get_LOCAL_prune_request(), Get_LOCAL_top_cp()))
    Set_LOCAL_prune_request(NULL);
  UNLOCK_WORKER(worker_id);
  return;
}



/* ----------------------------- **
**      Cut Stuff: Leftmost      **
** ----------------------------- */

static inline
int CUT_last_worker_left_pending_prune(or_fr_ptr or_frame) {
  CACHE_REGS
  int i, depth, ltt;
  bitmap members;

  depth = OrFr_depth(or_frame);
  ltt = OrFr_pend_prune_ltt(or_frame);
  members = OrFr_members(or_frame);
  BITMAP_delete(members, worker_id);
  for (i = 0; i < GLOBAL_number_workers; i++) {
    if (BITMAP_member(members, i) && BRANCH_LTT(i, depth) > ltt)
      return FALSE;
  }
  return TRUE;
}


static inline
or_fr_ptr CUT_leftmost_or_frame(void) {
  CACHE_REGS
  int i, depth, ltt;
  bitmap members;
  or_fr_ptr leftmost_or_fr, or_fr, nearest_or_fr;

  BITMAP_clear(members);
  BITMAP_insert(members, worker_id);
  leftmost_or_fr = LOCAL_top_or_fr;
  depth = OrFr_depth(leftmost_or_fr);
  do {
    ltt = BRANCH_LTT(worker_id, depth);
    BITMAP_difference(members, OrFr_members(leftmost_or_fr), members);
    if (members)
      for (i = 0; i < GLOBAL_number_workers; i++)
        if (BITMAP_member(members, i) && BRANCH_LTT(i, depth) > ltt)
          goto update_nearest_leftnode_data;
    BITMAP_copy(members, OrFr_members(leftmost_or_fr));
    leftmost_or_fr = OrFr_nearest_leftnode(leftmost_or_fr);
    depth = OrFr_depth(leftmost_or_fr);
  } while (depth);

update_nearest_leftnode_data:
  or_fr = LOCAL_top_or_fr;
  nearest_or_fr = OrFr_nearest_leftnode(or_fr);
  while (OrFr_depth(nearest_or_fr) > depth) {
    LOCK_OR_FRAME(or_fr);
    OrFr_nearest_leftnode(or_fr) = leftmost_or_fr;
    UNLOCK_OR_FRAME(or_fr);
    or_fr = nearest_or_fr;
    nearest_or_fr = OrFr_nearest_leftnode(or_fr);
  }

  return leftmost_or_fr;
}


#ifdef TABLING_INNER_CUTS
static inline
or_fr_ptr CUT_leftmost_until(or_fr_ptr start_or_fr, int until_depth) {
  int i, ltt, depth;
  bitmap prune_members, members;
  or_fr_ptr leftmost_or_fr, nearest_or_fr;

  /* we assume that the start_or_fr frame is locked and empty (without members) */
  leftmost_or_fr = OrFr_nearest_leftnode(start_or_fr);
  depth = OrFr_depth(leftmost_or_fr);
  if (depth > until_depth) {
    BITMAP_copy(prune_members, GLOBAL_bm_pruning_workers);
    BITMAP_delete(prune_members, worker_id);
    ltt = BRANCH_LTT(worker_id, depth);
    BITMAP_intersection(members, prune_members, OrFr_members(leftmost_or_fr));
    if (members) {
      for (i = 0; i < GLOBAL_number_workers; i++) {
        if (BITMAP_member(members, i) && 
            BRANCH_LTT(i, depth) > ltt &&
            EQUAL_OR_YOUNGER_CP(GetOrFr_node(leftmost_or_fr), REMOTE_pruning_scope(i)))
          return leftmost_or_fr;
      }
      BITMAP_minus(prune_members, members);
    }
    /* reaching that point we should update the nearest leftnode data before return */
    leftmost_or_fr = OrFr_nearest_leftnode(leftmost_or_fr);
    depth = OrFr_depth(leftmost_or_fr);
    while (depth > until_depth) {
      ltt = BRANCH_LTT(worker_id, depth);
      BITMAP_intersection(members, prune_members, OrFr_members(leftmost_or_fr));
      if (members) {
        for (i = 0; i < GLOBAL_number_workers; i++) {
          if (BITMAP_member(members, i) &&
              BRANCH_LTT(i, depth) > ltt &&
              EQUAL_OR_YOUNGER_CP(GetOrFr_node(leftmost_or_fr), REMOTE_pruning_scope(i))) {
            /* update nearest leftnode data */
            OrFr_nearest_leftnode(start_or_fr) = leftmost_or_fr;
            start_or_fr = OrFr_nearest_leftnode(start_or_fr);
            nearest_or_fr = OrFr_nearest_leftnode(start_or_fr);
            while (OrFr_depth(nearest_or_fr) > depth) {
              LOCK_OR_FRAME(start_or_fr);
              OrFr_nearest_leftnode(start_or_fr) = leftmost_or_fr;
              UNLOCK_OR_FRAME(start_or_fr);
              start_or_fr = nearest_or_fr;
              nearest_or_fr = OrFr_nearest_leftnode(start_or_fr);
            }
            return leftmost_or_fr;
	  }
        }
        BITMAP_minus(prune_members, members);
      }
      leftmost_or_fr = OrFr_nearest_leftnode(leftmost_or_fr);
      depth = OrFr_depth(leftmost_or_fr);
    }
    /* update nearest leftnode data */
    OrFr_nearest_leftnode(start_or_fr) = leftmost_or_fr;
    start_or_fr = OrFr_nearest_leftnode(start_or_fr);
    nearest_or_fr = OrFr_nearest_leftnode(start_or_fr);
    while (OrFr_depth(nearest_or_fr) > depth) {
      LOCK_OR_FRAME(start_or_fr);
      OrFr_nearest_leftnode(start_or_fr) = leftmost_or_fr;
      UNLOCK_OR_FRAME(start_or_fr);
      start_or_fr = nearest_or_fr;
      nearest_or_fr = OrFr_nearest_leftnode(start_or_fr);
    }
  }
  return NULL;
}
#endif /* TABLING_INNER_CUTS */



/* ------------------------------------------------ **
**      Cut Stuff: Managing query goal answers      **
** ------------------------------------------------ */

static inline 
void CUT_store_answer(or_fr_ptr or_frame, qg_ans_fr_ptr new_answer) {
  CACHE_REGS
  int ltt;
  qg_sol_fr_ptr *solution_ptr;

  ltt = BRANCH_LTT(worker_id, OrFr_depth(or_frame));
  solution_ptr = & OrFr_qg_solutions(or_frame);
  while (*solution_ptr && ltt > SolFr_ltt(*solution_ptr)) {
    solution_ptr = & SolFr_next(*solution_ptr);
  }
  if (*solution_ptr && ltt == SolFr_ltt(*solution_ptr)) {
    AnsFr_next(SolFr_last(*solution_ptr)) = new_answer;
    SolFr_last(*solution_ptr) = new_answer;
  } else {
    qg_sol_fr_ptr new_solution;
    ALLOC_QG_SOLUTION_FRAME(new_solution);
    SolFr_next(new_solution) = *solution_ptr;
    SolFr_ltt(new_solution) = ltt;
    SolFr_first(new_solution) = new_answer;
    SolFr_last(new_solution) = new_answer;
    *solution_ptr = new_solution;
  }
  return;
}


static inline 
void CUT_store_answers(or_fr_ptr or_frame, qg_sol_fr_ptr new_solution) {
  CACHE_REGS
  int ltt;
  qg_sol_fr_ptr *solution_ptr;

  ltt = BRANCH_LTT(worker_id, OrFr_depth(or_frame));
  solution_ptr = & OrFr_qg_solutions(or_frame);
  while (*solution_ptr && ltt > SolFr_ltt(*solution_ptr)) {
    solution_ptr = & SolFr_next(*solution_ptr);
  }
  if (*solution_ptr && ltt == SolFr_ltt(*solution_ptr)) {
    AnsFr_next(SolFr_last(*solution_ptr)) = SolFr_first(new_solution);
    SolFr_last(*solution_ptr) = SolFr_last(new_solution);
    FREE_QG_SOLUTION_FRAME(new_solution);
  } else {
    SolFr_next(new_solution) = *solution_ptr;
    SolFr_ltt(new_solution) = ltt;
    *solution_ptr = new_solution;
  }
  return;
}


static inline 
void CUT_join_answers_in_an_unique_frame(qg_sol_fr_ptr join_solution) {
  qg_sol_fr_ptr next_solution;

  while ((next_solution = SolFr_next(join_solution))) {
    AnsFr_next(SolFr_last(join_solution)) = SolFr_first(next_solution);
    SolFr_last(join_solution) = SolFr_last(next_solution);
    SolFr_next(join_solution) = SolFr_next(next_solution);
    FREE_QG_SOLUTION_FRAME(next_solution);
  }
  return;
}


static inline 
void CUT_free_solution_frame(qg_sol_fr_ptr solution) {
  qg_ans_fr_ptr current_answer, next_answer;

  current_answer = SolFr_first(solution);
  do {
    next_answer = AnsFr_next(current_answer);
    FREE_QG_ANSWER_FRAME(current_answer);
    current_answer = next_answer;
  } while (current_answer);
  FREE_QG_SOLUTION_FRAME(solution);
  return;
}


static inline 
void CUT_free_solution_frames(qg_sol_fr_ptr current_solution) {
  qg_sol_fr_ptr next_solution;

  while (current_solution) {
    next_solution = SolFr_next(current_solution);
    CUT_free_solution_frame(current_solution);
    current_solution = next_solution;
  }
  return;
}


static inline 
qg_sol_fr_ptr CUT_prune_solution_frames(qg_sol_fr_ptr solutions, int ltt) {
  qg_sol_fr_ptr next_solution;

  while (solutions && ltt > SolFr_ltt(solutions)) {
    next_solution = SolFr_next(solutions);
    CUT_free_solution_frame(solutions);
    solutions = next_solution;
  }
  return solutions;
}

#endif
