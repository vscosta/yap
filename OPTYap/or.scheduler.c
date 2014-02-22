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

/* ------------------ **
**      Includes      **
** ------------------ */

#include "Yap.h"
#ifdef YAPOR
#include "Yatom.h"
#include "YapHeap.h"
#include "or.macros.h"
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */



/* ------------------------------------- **
**      Local functions declaration      **
** ------------------------------------- */

static int move_up_one_node(or_fr_ptr nearest_livenode);
static int get_work_below(void);
static int get_work_above(void);
static int find_a_better_position(void);
static int search_for_hidden_shared_work(bitmap stable_busy);



/* ----------------------- **
**      Local inlines      **
** ----------------------- */

static inline void PUT_NO_WORK_IN_UPPER_NODES(void);
static inline void PUT_IDLE(int);
static inline void PUT_BUSY(int);
static inline void move_up_to_prune_request(void);


static inline
void PUT_NO_WORK_IN_UPPER_NODES(void) {
  CACHE_REGS
  or_fr_ptr current_node, nearest_livenode;
  current_node = LOCAL_top_or_fr;
  while ((nearest_livenode = OrFr_nearest_livenode(current_node))) {
    OrFr_nearest_livenode(current_node) = NULL;
    current_node = nearest_livenode;
  }
  return;
}


static inline
void PUT_IDLE(int worker_num) {
  LOCK(GLOBAL_locks_bm_idle_workers);
  BITMAP_insert(GLOBAL_bm_idle_workers, worker_num);
  UNLOCK(GLOBAL_locks_bm_idle_workers);
  return;
}


static inline
void PUT_BUSY(int worker_num) {
  LOCK(GLOBAL_locks_bm_idle_workers);
  BITMAP_delete(GLOBAL_bm_idle_workers, worker_num);
  UNLOCK(GLOBAL_locks_bm_idle_workers);
  return;
}


static inline
void move_up_to_prune_request(void) {
  CACHE_REGS
  YAPOR_ERROR_CHECKING(move_up_to_prune_request, EQUAL_OR_YOUNGER_CP(Get_LOCAL_prune_request(), Get_LOCAL_top_cp()));

  do {
    LOCK_OR_FRAME(LOCAL_top_or_fr);
    if (BITMAP_alone(OrFr_members(LOCAL_top_or_fr), worker_id)) {
#ifdef TABLING
      if (OrFr_suspensions(LOCAL_top_or_fr) || OrFr_owners(LOCAL_top_or_fr) != 1)
        pruning_over_tabling_data_structures();
#endif /* TABLING */
      CUT_free_solution_frames(OrFr_qg_solutions(LOCAL_top_or_fr));
#ifdef TABLING_INNER_CUTS
      CUT_free_tg_solution_frames(OrFr_tg_solutions(LOCAL_top_or_fr));
#endif /* TABLING_INNER_CUTS */
      FREE_OR_FRAME(LOCAL_top_or_fr);
    } else {
      BITMAP_delete(OrFr_members(LOCAL_top_or_fr), worker_id);
#ifdef TABLING
      OrFr_owners(LOCAL_top_or_fr)--;
#endif /* TABLING */
      UNLOCK_OR_FRAME(LOCAL_top_or_fr);
    }
    SCH_update_local_or_tops();
  } while (Get_LOCAL_top_cp() != Get_LOCAL_prune_request());

  CUT_reset_prune_request();
#ifdef TABLING
  Set_LOCAL_top_cp_on_stack( Get_LOCAL_top_cp());
  abolish_incomplete_subgoals(Get_LOCAL_top_cp() - 1);  /* do not include LOCAL_top_cp */
#endif /* TABLIG */

  return;
}



/* -------------------------- **
**      Global functions      **
** -------------------------- */

int get_work(void) {
  CACHE_REGS
  int counter;
  bitmap stable_busy;
  yamop *alt_with_work;
  or_fr_ptr or_fr_with_work, or_fr_to_move_to;
#ifdef TABLING
  choiceptr leader_node = DepFr_leader_cp(LOCAL_top_dep_fr);
#endif /* TABLING */

  /* reset local load */
  LOCAL_load = 0;

  /* check for prune request */
  if (Get_LOCAL_prune_request())
    move_up_to_prune_request();

  /* find nearest node with available work */
  or_fr_with_work = LOCAL_top_or_fr;
  do {
    or_fr_with_work = OrFr_nearest_livenode(or_fr_with_work);
    if (or_fr_with_work == NULL)
      break;
    alt_with_work = OrFr_alternative(or_fr_with_work);
  } while (alt_with_work == NULL || YAMOP_SEQ(alt_with_work));

#ifndef TABLING
  /* wait for incomplete installations */
  while (LOCAL_reply_signal != worker_ready);
#endif /* TABLING */

  if (or_fr_with_work) {
    /* move up to the nearest node with available work */
#ifdef TABLING
    if (leader_node && YOUNGER_CP(leader_node, GetOrFr_node(or_fr_with_work)))
      /* there is a leader node before the nearest node with work */
      or_fr_to_move_to = leader_node->cp_or_fr;
    else
#endif /* TABLING */ 
      or_fr_to_move_to = or_fr_with_work;
    do {
      if (! move_up_one_node(or_fr_with_work))
        break;
    } while (LOCAL_top_or_fr != or_fr_to_move_to);
    return TRUE;
  }

  /* no nodes with available work */
  PUT_NO_WORK_IN_UPPER_NODES();
#ifdef TABLING
  if (leader_node) {
    /* there is a leader node */
    or_fr_to_move_to = leader_node->cp_or_fr;;
    do {
      if (! move_up_one_node(NULL))
        break;
    } while (LOCAL_top_or_fr != or_fr_to_move_to);
    return TRUE;
  }
#endif /* TABLING */ 

#ifdef TABLING_INNER_CUTS
  if (LOCAL_pruning_scope) {
    PUT_OUT_PRUNING(worker_id);
    LOCAL_pruning_scope = NULL;
  }
#endif /* TABLING_INNER_CUTS */
  PUT_OUT_ROOT_NODE(worker_id);
  LOCK_WORKER(worker_id);
  PUT_IDLE(worker_id);
  UNLOCK_WORKER(worker_id);
  SCH_refuse_share_request_if_any();
  
  counter = 0;
  BITMAP_difference(stable_busy, OrFr_members(LOCAL_top_or_fr), GLOBAL_bm_idle_workers);
   while (1) {
    while (BITMAP_subset(GLOBAL_bm_idle_workers, OrFr_members(LOCAL_top_or_fr)) &&
           Get_LOCAL_top_cp() != Get_GLOBAL_root_cp()) {
      /* no busy workers here and below */
      if (! move_up_one_node(NULL)) {
        PUT_BUSY(worker_id);
        return TRUE;
      }
    }
    if (Get_LOCAL_top_cp() == Get_GLOBAL_root_cp()) {
      if (! BITMAP_member(GLOBAL_bm_root_cp_workers, worker_id))
        /* We need this extra bitmap because the GLOBAL_bm_idle_workers bitmap 
           is not enough to deal with sequential predicates. The condition of
           all workers being idle is not sufficient to ensure that there is no 
           available computation since a sequential predicate can still be resumed.
           Only when all workers are idle and in the root choicepoint it is safe to
           finish execution. */
        PUT_IN_ROOT_NODE(worker_id);
      if (BITMAP_same(GLOBAL_bm_root_cp_workers, GLOBAL_bm_present_workers))
        /* All workers are idle in the root choicepoint. Execution 
           must finish as there is no available computation. */
        return FALSE;
    }
    if (get_work_below()) {
      PUT_BUSY(worker_id);
      return TRUE;
    }
    if (get_work_above()) {
      PUT_BUSY(worker_id);
      return TRUE;
    }
    if (find_a_better_position()) {
      PUT_BUSY(worker_id);
      return TRUE;
    }
    if (++counter == GLOBAL_scheduler_loop) {
      if (search_for_hidden_shared_work(stable_busy)) {
        PUT_BUSY(worker_id);
        return TRUE;
      }
      counter = 0;
      BITMAP_difference(stable_busy, OrFr_members(LOCAL_top_or_fr), GLOBAL_bm_idle_workers);
    } else {
      BITMAP_minus(stable_busy, GLOBAL_bm_idle_workers);
    }
  }
}



/* ------------------------- **
**      Local functions      **
** ------------------------- */

static
int move_up_one_node(or_fr_ptr nearest_livenode) {
  CACHE_REGS
  YAPOR_ERROR_CHECKING(move_up_one_node, Get_LOCAL_prune_request() && EQUAL_OR_YOUNGER_CP(Get_LOCAL_prune_request(), Get_LOCAL_top_cp()));

  LOCK_OR_FRAME(LOCAL_top_or_fr);
  /* last worker in a sequential choicepoint ? */
  if (OrFr_alternative(LOCAL_top_or_fr) 
      && YAMOP_SEQ(OrFr_alternative(LOCAL_top_or_fr)) 
      && BITMAP_alone(OrFr_members(LOCAL_top_or_fr), worker_id)) {
    UNLOCK_OR_FRAME(LOCAL_top_or_fr);
    return FALSE;
  }

  /* pending prune ? */
  if (Get_OrFr_pend_prune_cp(LOCAL_top_or_fr) 
      && ! Get_LOCAL_prune_request()
      && CUT_last_worker_left_pending_prune(LOCAL_top_or_fr)) {
#ifdef TABLING
    choiceptr aux_cp = Get_LOCAL_top_cp();
#endif /* TABLIG */
    choiceptr prune_cp = Get_OrFr_pend_prune_cp(LOCAL_top_or_fr);
    Set_OrFr_pend_prune_cp(LOCAL_top_or_fr, NULL);
    BRANCH(worker_id, OrFr_depth(LOCAL_top_or_fr)) = OrFr_pend_prune_ltt(LOCAL_top_or_fr);
    UNLOCK_OR_FRAME(LOCAL_top_or_fr);
    prune_shared_branch(prune_cp, &OrFr_pend_prune_ltt(LOCAL_top_or_fr));
#ifdef TABLING
    while (YOUNGER_CP(aux_cp->cp_b, Get_LOCAL_top_cp()))
      aux_cp = aux_cp->cp_b;
    abolish_incomplete_subgoals(aux_cp);
#endif /* TABLIG */
    return FALSE;
  }

  OPTYAP_ERROR_CHECKING(move_up_one_node, B_FZ != DepFr_cons_cp(LOCAL_top_dep_fr));
  OPTYAP_ERROR_CHECKING(move_up_one_node, LOCAL_top_susp_or_fr && EQUAL_OR_YOUNGER_CP(Get_LOCAL_top_cp(), B_FZ) && YOUNGER_CP(GetOrFr_node(LOCAL_top_susp_or_fr), Get_LOCAL_top_cp()));
  OPTYAP_ERROR_CHECKING(move_up_one_node, LOCAL_top_susp_or_fr && YOUNGER_CP(B_FZ, Get_LOCAL_top_cp()) && YOUNGER_CP(GetOrFr_node(LOCAL_top_susp_or_fr), B_FZ));

#ifdef TABLING
  /* frozen stacks on branch ? */
  if (YOUNGER_CP(B_FZ, Get_LOCAL_top_cp())) {
    if (nearest_livenode)
      OrFr_nearest_livenode(LOCAL_top_or_fr) = nearest_livenode;
    BITMAP_delete(OrFr_members(LOCAL_top_or_fr), worker_id);
    if (BITMAP_empty(OrFr_members(LOCAL_top_or_fr))) {
      if (frame_with_suspensions_not_collected(LOCAL_top_or_fr)) {
        collect_suspension_frames(LOCAL_top_or_fr);
      }
#ifdef TABLING_INNER_CUTS
      if (OrFr_tg_solutions(LOCAL_top_or_fr)) {
        tg_sol_fr_ptr tg_solutions;
        or_fr_ptr leftmost_until;
        tg_solutions = OrFr_tg_solutions(LOCAL_top_or_fr);
        leftmost_until = CUT_leftmost_until(LOCAL_top_or_fr, OrFr_depth(TgSolFr_gen_cp(tg_solutions)->cp_or_fr));
        OrFr_tg_solutions(LOCAL_top_or_fr) = NULL;
        UNLOCK_OR_FRAME(LOCAL_top_or_fr);
        if (leftmost_until) {
          LOCK_OR_FRAME(leftmost_until);
          tg_solutions = CUT_store_tg_answers(leftmost_until, tg_solutions,
                                              BRANCH_LTT(worker_id, OrFr_depth(leftmost_until)));
          UNLOCK_OR_FRAME(leftmost_until);
        }
        CUT_validate_tg_answers(tg_solutions);
        goto update_local_tops1;
      }
#endif /* TABLING_INNER_CUTS */
    }
    UNLOCK_OR_FRAME(LOCAL_top_or_fr);
#ifdef TABLING_INNER_CUTS
  update_local_tops1:
#endif /* TABLING_INNER_CUTS */
    SCH_update_local_or_tops();
    if (Get_LOCAL_prune_request())
      pruning_over_tabling_data_structures();
    return TRUE;
  }

  /* suspension frames to resume ? */
  if (OrFr_suspensions(LOCAL_top_or_fr)) {
    susp_fr_ptr resume_fr;
#ifdef TIMESTAMP_CHECK
    resume_fr = suspension_frame_to_resume(LOCAL_top_or_fr, ++GLOBAL_timestamp);
#else
    resume_fr = suspension_frame_to_resume(LOCAL_top_or_fr);
#endif /* TIMESTAMP_CHECK */
    if (resume_fr) {
      if (LOCAL_top_susp_or_fr == LOCAL_top_or_fr && OrFr_suspensions(LOCAL_top_or_fr) == NULL) {
        LOCAL_top_susp_or_fr = OrFr_nearest_suspnode(LOCAL_top_or_fr);
        OrFr_nearest_suspnode(LOCAL_top_or_fr) = LOCAL_top_or_fr;
      }
      UNLOCK_OR_FRAME(LOCAL_top_or_fr);
      unbind_variables(TR, Get_LOCAL_top_cp()->cp_tr);
      resume_suspension_frame(resume_fr, LOCAL_top_or_fr);
      return FALSE;
    }
    if (LOCAL_top_susp_or_fr == LOCAL_top_or_fr) {
      LOCAL_top_susp_or_fr = OrFr_nearest_suspnode(LOCAL_top_or_fr);
      OrFr_nearest_suspnode(LOCAL_top_or_fr) = NULL;
    }
  } else if (LOCAL_top_susp_or_fr == LOCAL_top_or_fr) {
    LOCAL_top_susp_or_fr = OrFr_nearest_suspnode(LOCAL_top_or_fr);
    OrFr_nearest_suspnode(LOCAL_top_or_fr) = LOCAL_top_or_fr;
  }
  
  /* top node frozen ? */
  if (B_FZ == Get_LOCAL_top_cp()) {
    if (nearest_livenode)
      OrFr_nearest_livenode(LOCAL_top_or_fr) = nearest_livenode;
    BITMAP_delete(OrFr_members(LOCAL_top_or_fr), worker_id);
#ifdef TABLING_INNER_CUTS
    if (BITMAP_empty(OrFr_members(LOCAL_top_or_fr))) {
#endif /* TABLING_INNER_CUTS */
      if (OrFr_suspensions(LOCAL_top_or_fr) && OrFr_owners(LOCAL_top_or_fr) == 1) {
        complete_suspension_frames(LOCAL_top_or_fr);
      }
#ifdef TABLING_INNER_CUTS
      if (OrFr_tg_solutions(LOCAL_top_or_fr)) {
        tg_sol_fr_ptr tg_solutions;
        or_fr_ptr leftmost_until;
        tg_solutions = OrFr_tg_solutions(LOCAL_top_or_fr);
        leftmost_until = CUT_leftmost_until(LOCAL_top_or_fr, OrFr_depth(TgSolFr_gen_cp(tg_solutions)->cp_or_fr));
        OrFr_tg_solutions(LOCAL_top_or_fr) = NULL;
        UNLOCK_OR_FRAME(LOCAL_top_or_fr);
        if (leftmost_until) {
          LOCK_OR_FRAME(leftmost_until);
          tg_solutions = CUT_store_tg_answers(leftmost_until, tg_solutions,
                                              BRANCH_LTT(worker_id, OrFr_depth(leftmost_until)));
          UNLOCK_OR_FRAME(leftmost_until);
        }
        CUT_validate_tg_answers(tg_solutions);
        goto update_local_tops2;
      }
    }
#endif /* TABLING_INNER_CUTS */
    UNLOCK_OR_FRAME(LOCAL_top_or_fr);
#ifdef TABLING_INNER_CUTS
  update_local_tops2:
#endif /* TABLING_INNER_CUTS */
    SCH_update_local_or_tops();
    if (Get_LOCAL_prune_request())
      pruning_over_tabling_data_structures();
    return TRUE;
  }

  OPTYAP_ERROR_CHECKING(move_up_one_node, OrFr_alternative(LOCAL_top_or_fr) && ! YAMOP_SEQ(OrFr_alternative(LOCAL_top_or_fr)));
  OPTYAP_ERROR_CHECKING(move_up_one_node, Get_LOCAL_top_cp() == DepFr_cons_cp(LOCAL_top_dep_fr));
  OPTYAP_ERROR_CHECKING(move_up_one_node, Get_LOCAL_top_cp() != Get_LOCAL_top_cp_on_stack());

  /* no frozen nodes */
  Set_LOCAL_top_cp_on_stack(GetOrFr_node(OrFr_next_on_stack(LOCAL_top_or_fr)));

  /* no more owners ? */
  if (OrFr_owners(LOCAL_top_or_fr) == 1) {
    if (OrFr_suspensions(LOCAL_top_or_fr)) {
      complete_suspension_frames(LOCAL_top_or_fr);
    }
    if (LOCAL_top_sg_fr && Get_LOCAL_top_cp() == SgFr_gen_cp(LOCAL_top_sg_fr)) {
      mark_as_completed(LOCAL_top_sg_fr);
      LOCAL_top_sg_fr = SgFr_next(LOCAL_top_sg_fr);
    }
#else
  /* last member worker in node ? */
  if (BITMAP_alone(OrFr_members(LOCAL_top_or_fr), worker_id)) {
#endif /* TABLING */
    if (Get_LOCAL_prune_request()) {
      CUT_free_solution_frames(OrFr_qg_solutions(LOCAL_top_or_fr));
#ifdef TABLING_INNER_CUTS
      CUT_free_tg_solution_frames(OrFr_tg_solutions(LOCAL_top_or_fr));
#endif /* TABLING_INNER_CUTS */
      FREE_OR_FRAME(LOCAL_top_or_fr);
      SCH_update_local_or_tops();
      CUT_reset_prune_request();
    } else {
      qg_sol_fr_ptr qg_solutions = OrFr_qg_solutions(LOCAL_top_or_fr);
#ifdef TABLING_INNER_CUTS
      tg_sol_fr_ptr tg_solutions = OrFr_tg_solutions(LOCAL_top_or_fr);
#endif /* TABLING_INNER_CUTS */
      FREE_OR_FRAME(LOCAL_top_or_fr);
      SCH_update_local_or_tops();
      CUT_reset_prune_request();
#ifdef TABLING_INNER_CUTS
      if (qg_solutions || tg_solutions) {
        or_fr_ptr leftmost_or_fr;
        if (qg_solutions)
          CUT_join_answers_in_an_unique_frame(qg_solutions);
        leftmost_or_fr = CUT_leftmost_or_frame();
        LOCK_OR_FRAME(leftmost_or_fr);
        if (qg_solutions)
          CUT_store_answers(leftmost_or_fr, qg_solutions);
        if (tg_solutions)
          tg_solutions = CUT_store_tg_answers(leftmost_or_fr, tg_solutions, 
                                              BRANCH_LTT(worker_id, OrFr_depth(leftmost_or_fr)));
        UNLOCK_OR_FRAME(leftmost_or_fr);
        CUT_validate_tg_answers(tg_solutions);
      }
#else
      if (qg_solutions) {
        or_fr_ptr leftmost_or_fr;
        CUT_join_answers_in_an_unique_frame(qg_solutions);
        leftmost_or_fr = CUT_leftmost_or_frame();
        LOCK_OR_FRAME(leftmost_or_fr);
        CUT_store_answers(leftmost_or_fr, qg_solutions);
        UNLOCK_OR_FRAME(leftmost_or_fr);
      }
#endif /* TABLING_INNER_CUTS */
    }
    return TRUE;
  }

  /* more owners */
  if (nearest_livenode)
    OrFr_nearest_livenode(LOCAL_top_or_fr) = nearest_livenode;
  BITMAP_delete(OrFr_members(LOCAL_top_or_fr), worker_id);
#ifdef TABLING
  OrFr_owners(LOCAL_top_or_fr)--;
  if (BITMAP_empty(OrFr_members(LOCAL_top_or_fr))) {
#ifdef TABLING_INNER_CUTS
    if (OrFr_tg_solutions(LOCAL_top_or_fr)) {
      tg_sol_fr_ptr tg_solutions;
      or_fr_ptr leftmost_until;
      tg_solutions = OrFr_tg_solutions(LOCAL_top_or_fr);
      leftmost_until = CUT_leftmost_until(LOCAL_top_or_fr, OrFr_depth(TgSolFr_gen_cp(tg_solutions)->cp_or_fr));
      if (Get_LOCAL_prune_request())
        pruning_over_tabling_data_structures();
      OrFr_tg_solutions(LOCAL_top_or_fr) = NULL;
      UNLOCK_OR_FRAME(LOCAL_top_or_fr);
      if (leftmost_until) {
        LOCK_OR_FRAME(leftmost_until);
        tg_solutions = CUT_store_tg_answers(leftmost_until, tg_solutions,
                                            BRANCH_LTT(worker_id, OrFr_depth(leftmost_until)));
        UNLOCK_OR_FRAME(leftmost_until);
      }
      CUT_validate_tg_answers(tg_solutions);
      goto update_local_tops3;
    }
#endif /* TABLING_INNER_CUTS */
    if (Get_LOCAL_prune_request())
      pruning_over_tabling_data_structures();
  }
#endif /* TABLING */
  UNLOCK_OR_FRAME(LOCAL_top_or_fr);
#ifdef TABLING
#ifdef TABLING_INNER_CUTS
  update_local_tops3:
#endif /* TABLING_INNER_CUTS */
  if (LOCAL_top_sg_fr && Get_LOCAL_top_cp() == SgFr_gen_cp(LOCAL_top_sg_fr)) {
    LOCAL_top_sg_fr = SgFr_next(LOCAL_top_sg_fr);
  }
#endif /* TABLING */
  SCH_update_local_or_tops();
  CUT_reset_prune_request();
  return TRUE;
}


static
int get_work_below(void){
  CACHE_REGS
  int i, worker_p, big_load;
  bitmap busy_below, idle_below;

  worker_p = -1;
  big_load = GLOBAL_delayed_release_load ;
  BITMAP_difference(busy_below, OrFr_members(LOCAL_top_or_fr), GLOBAL_bm_idle_workers);
  BITMAP_difference(idle_below, OrFr_members(LOCAL_top_or_fr), busy_below);
  BITMAP_delete(idle_below, worker_id);
  for (i = 0; i < GLOBAL_number_workers; i++) {
    if (BITMAP_member(idle_below ,i) && YOUNGER_CP(REMOTE_top_cp(i), Get_LOCAL_top_cp()))
      BITMAP_minus(busy_below, OrFr_members(REMOTE_top_or_fr(i)));
  }
  if (BITMAP_empty(busy_below))
    return FALSE;
  /* choose the worker with highest load */
  for (i = 0 ; i < GLOBAL_number_workers; i++) {
    if (BITMAP_member(busy_below ,i) && REMOTE_load(i) > big_load) {
      worker_p = i;
      big_load = REMOTE_load(i);
    }
  }
  if (worker_p == -1) 
    return FALSE;
  return (q_share_work(worker_p));
}


static
int get_work_above(void){
  CACHE_REGS
  int i, worker_p, big_load;
  bitmap visible_busy_above, visible_idle_above;

  worker_p = -1; 
  big_load = GLOBAL_delayed_release_load ;
  BITMAP_difference(visible_busy_above, GLOBAL_bm_present_workers, OrFr_members(LOCAL_top_or_fr));
  BITMAP_minus(visible_busy_above, GLOBAL_bm_invisible_workers);
  BITMAP_copy(visible_idle_above, visible_busy_above); 
  BITMAP_minus(visible_busy_above, GLOBAL_bm_idle_workers);
  BITMAP_and(visible_idle_above, GLOBAL_bm_idle_workers);
  BITMAP_insert(visible_busy_above, worker_id);
  for (i = 0 ; i < GLOBAL_number_workers; i++) {
    if (BITMAP_member(visible_idle_above, i))
      BITMAP_minus(visible_busy_above, OrFr_members(REMOTE_top_or_fr(i)));
  }
  if (!BITMAP_member(visible_busy_above, worker_id) || BITMAP_alone(visible_busy_above, worker_id))
    return FALSE;
  BITMAP_delete(visible_busy_above, worker_id);
  /* choose the worker with higher load */
  for (i = 0; i < GLOBAL_number_workers; i++) {
    if (BITMAP_member(visible_busy_above ,i) && REMOTE_load(i) > big_load) {
      worker_p = i;
      big_load = REMOTE_load(i);
    }
  }
  if (worker_p == -1)
    return FALSE;
  /* put workers invisibles */
  LOCK(GLOBAL_locks_bm_invisible_workers);
  if (BITMAP_member(GLOBAL_bm_invisible_workers, worker_p)) {
    UNLOCK(GLOBAL_locks_bm_invisible_workers);
    return FALSE;
  }
  BITMAP_insert(GLOBAL_bm_invisible_workers, worker_id);
  BITMAP_insert(GLOBAL_bm_invisible_workers, worker_p);
  UNLOCK(GLOBAL_locks_bm_invisible_workers);
  /* move up to cp with worker_p */
  do {
    if (! move_up_one_node(NULL)) {
      return TRUE;
    }
  } while (! BITMAP_member(OrFr_members(LOCAL_top_or_fr), worker_p));
  /* put workers visibles */
  LOCK(GLOBAL_locks_bm_invisible_workers);
  BITMAP_delete(GLOBAL_bm_invisible_workers, worker_id);
  BITMAP_delete(GLOBAL_bm_invisible_workers, worker_p);
  UNLOCK(GLOBAL_locks_bm_invisible_workers);
  return (q_share_work(worker_p));
}


static
int find_a_better_position(void){
  CACHE_REGS
  int i;
  bitmap busy_above, idle_above;
  BITMAP_difference(busy_above, GLOBAL_bm_present_workers, OrFr_members(LOCAL_top_or_fr));
  BITMAP_copy(idle_above, busy_above); 
  BITMAP_minus(busy_above, GLOBAL_bm_idle_workers);
  BITMAP_and(idle_above, GLOBAL_bm_idle_workers);
  for (i = 0; i < GLOBAL_number_workers; i++) {
    if (BITMAP_member(idle_above, i)) {
      if (BITMAP_empty(busy_above)) 
        break;
      if (BITMAP_member(OrFr_members(REMOTE_top_or_fr(i)), worker_id))
        BITMAP_clear(busy_above);
      BITMAP_minus(busy_above, OrFr_members(REMOTE_top_or_fr(i)));
    }
  }
  if (BITMAP_empty(busy_above))
    return FALSE;
  /* move up to cp with all workers of bitmap busy_above */
  do {
    if (! move_up_one_node(NULL)) {
      return TRUE;
    }
  } while (! BITMAP_subset(OrFr_members(LOCAL_top_or_fr), busy_above));
  return FALSE;
}


static
int search_for_hidden_shared_work(bitmap stable_busy){
  CACHE_REGS
  int i;
  bitmap invisible_work, idle_below;
  BITMAP_intersection(invisible_work, stable_busy, GLOBAL_bm_requestable_workers);
  BITMAP_intersection(idle_below, OrFr_members(LOCAL_top_or_fr), GLOBAL_bm_idle_workers);
  BITMAP_delete(idle_below, worker_id);
  for (i = 0; i < GLOBAL_number_workers; i++) {
    if (BITMAP_member(idle_below ,i) && YOUNGER_CP(REMOTE_top_cp(i), Get_LOCAL_top_cp()))
      BITMAP_minus(invisible_work, OrFr_members(REMOTE_top_or_fr(i)));
  }
  if (BITMAP_empty(invisible_work))
    return FALSE;
  /* choose the first available worker */
  for (i = 0; i < GLOBAL_number_workers; i++ ) {
    if (BITMAP_member(invisible_work ,i))
      break;
  }
  return (q_share_work(i));
}
#endif /* YAPOR */
