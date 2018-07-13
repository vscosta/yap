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
#ifdef TABLING
#include "Yatom.h"
#include "YapHeap.h"
#include "tab.macros.h"
#ifdef YAPOR
#include "or.macros.h"
#endif /* YAPOR */



/******************************
**      Local functions      **
******************************/

#ifdef YAPOR
static void complete_suspension_branch(susp_fr_ptr susp_fr, choiceptr top_cp, or_fr_ptr *chain_or_fr, dep_fr_ptr *chain_dep_fr) {
  or_fr_ptr aux_or_fr;
  sg_fr_ptr aux_sg_fr;
  dep_fr_ptr aux_dep_fr;

  /* complete all subgoals */
  aux_dep_fr = SuspFr_top_dep_fr(susp_fr);
  aux_sg_fr = SuspFr_top_sg_fr(susp_fr);
  if (DepFr_leader_dep_is_on_stack(aux_dep_fr)) {
    while (aux_sg_fr && 
	   /* continue if the subgoal was early completed */ 
           /* SgFr_state(aux_sg_fr) == evaluating && */
           (SgFr_state(aux_sg_fr) == evaluating || SgFr_first_answer(aux_sg_fr) == SgFr_answer_trie(aux_sg_fr)) &&
           EQUAL_OR_YOUNGER_CP(SgFr_gen_cp(aux_sg_fr), top_cp)) {
      mark_as_completed(aux_sg_fr);
      aux_sg_fr = SgFr_next(aux_sg_fr);
    }
  } else {
    while (aux_sg_fr && 
	   /* continue if the subgoal was early completed */ 
           /* SgFr_state(aux_sg_fr) == evaluating && */
           (SgFr_state(aux_sg_fr) == evaluating || SgFr_first_answer(aux_sg_fr) == SgFr_answer_trie(aux_sg_fr)) &&
           YOUNGER_CP(SgFr_gen_cp(aux_sg_fr), top_cp)) {
      mark_as_completed(aux_sg_fr);
      aux_sg_fr = SgFr_next(aux_sg_fr);
    }
  }

  /* chain dependency frames to release (using DepFr_next) */
  while (IS_UNLOCKED_DEP_FR(aux_dep_fr) && 
         YOUNGER_CP(DepFr_cons_cp(aux_dep_fr), top_cp)) {
    dep_fr_ptr next_dep_fr;
    LOCK_DEP_FR(aux_dep_fr);
    next_dep_fr = DepFr_next(aux_dep_fr);
    DepFr_next(aux_dep_fr) = *chain_dep_fr;
    *chain_dep_fr = aux_dep_fr;
    aux_dep_fr = next_dep_fr;
  }

  /* chain or-frames to release (using OrFr_next_on_stack)    **
  ** we use the OrFr_next_on_stack field instead of OrFr_next **
  ** to avoid conflicts with the 'find_dependency_node' macro */
  aux_or_fr = SuspFr_top_or_fr_on_stack(susp_fr);
  while (IS_UNLOCKED(OrFr_lock(aux_or_fr))) {
    susp_fr_ptr aux_susp_fr;
    or_fr_ptr next_or_fr_on_stack;
    OPTYAP_ERROR_CHECKING(complete_suspension_branch, YOUNGER_CP(top_cp, GetOrFr_node(aux_or_fr)));
    LOCK_OR_FRAME(aux_or_fr);
    aux_susp_fr = OrFr_suspensions(aux_or_fr);
    while (aux_susp_fr) {
      susp_fr_ptr next_susp_fr;
      complete_suspension_branch(aux_susp_fr, GetOrFr_node(aux_or_fr), chain_or_fr, chain_dep_fr);
      next_susp_fr = SuspFr_next(aux_susp_fr);
      FREE_SUSPENSION_FRAME(aux_susp_fr);
      aux_susp_fr = next_susp_fr;
    }
    next_or_fr_on_stack = OrFr_next_on_stack(aux_or_fr);
    OrFr_next_on_stack(aux_or_fr) = *chain_or_fr;
    *chain_or_fr = aux_or_fr;
    aux_or_fr = next_or_fr_on_stack;
  }

  return;
}
#endif /* YAPOR */



/*******************************
**      Global functions      **
*******************************/

void private_completion(sg_fr_ptr sg_fr) {
  CACHE_REGS

  /* complete subgoals */
#ifdef LIMIT_TABLING
  sg_fr_ptr aux_sg_fr;
  while (LOCAL_top_sg_fr != sg_fr) {
    aux_sg_fr = LOCAL_top_sg_fr;
    LOCAL_top_sg_fr = SgFr_next(aux_sg_fr);
    mark_as_completed(aux_sg_fr);
    insert_into_global_sg_fr_list(aux_sg_fr);
  }
  aux_sg_fr = LOCAL_top_sg_fr;
  LOCAL_top_sg_fr = SgFr_next(aux_sg_fr);
  mark_as_completed(aux_sg_fr);
  insert_into_global_sg_fr_list(aux_sg_fr);
#else
  while (LOCAL_top_sg_fr != sg_fr) {
    mark_as_completed(LOCAL_top_sg_fr);
    LOCAL_top_sg_fr = SgFr_next(LOCAL_top_sg_fr);
  }
  mark_as_completed(LOCAL_top_sg_fr);
  LOCAL_top_sg_fr = SgFr_next(LOCAL_top_sg_fr);
#endif /* LIMIT_TABLING */

  /* release dependency frames */
  while (EQUAL_OR_YOUNGER_CP(DepFr_cons_cp(LOCAL_top_dep_fr), B)) {  /* never equal if batched scheduling */
    dep_fr_ptr dep_fr = DepFr_next(LOCAL_top_dep_fr);
    FREE_DEPENDENCY_FRAME(LOCAL_top_dep_fr);
    LOCAL_top_dep_fr = dep_fr;
  }

  /* adjust freeze registers */
  adjust_freeze_registers();

  /* adjust thread dependency */
#ifdef THREADS_CONSUMER_SHARING
  ThDepFr_state(GLOBAL_th_dep_fr(worker_id)) = working;
  ThDepFr_next(GLOBAL_th_dep_fr(worker_id)) = worker_id;
#endif /* THREADS_CONSUMER_SHARING */
  return;
}


#ifdef YAPOR
void public_completion(void) {
  CACHE_REGS
  dep_fr_ptr chain_dep_fr, next_dep_fr;
  or_fr_ptr chain_or_fr, top_or_fr, next_or_fr;
  susp_fr_ptr susp_fr, next_susp_fr;
  qg_sol_fr_ptr solutions, aux_solutions;

  if (YOUNGER_CP(Get_LOCAL_top_cp(), B_FZ)) {
    /* the current node is a generator node without younger consumer **
    ** nodes --> we only have the current node to complete           */
    sg_fr_ptr top_sg_fr;

    /* complete subgoals */
#ifdef DETERMINISTIC_TABLING
    if (IS_DET_GEN_CP(Get_LOCAL_top_cp()))
      top_sg_fr = SgFr_next(DET_GEN_CP(Get_LOCAL_top_cp())->cp_sg_fr);
    else
#endif /* DETERMINISTIC_TABLING */
      top_sg_fr = SgFr_next(GEN_CP(Get_LOCAL_top_cp())->cp_sg_fr);
    do {
      mark_as_completed(LOCAL_top_sg_fr);
      LOCAL_top_sg_fr = SgFr_next(LOCAL_top_sg_fr);
    } while (LOCAL_top_sg_fr != top_sg_fr);

    /* no dependency frames to release */
    chain_dep_fr = NULL;

    /* no need to adjust freeze registers */
  } else {
    /* the current node is a leader node with younger consumer **
    ** nodes ---> we need to complete all dependent subgoals   */

    /* complete subgoals */
    if (DepFr_leader_dep_is_on_stack(LOCAL_top_dep_fr)) {
      while (LOCAL_top_sg_fr && 
             EQUAL_OR_YOUNGER_CP(SgFr_gen_cp(LOCAL_top_sg_fr), Get_LOCAL_top_cp())) {
        mark_as_completed(LOCAL_top_sg_fr);
        LOCAL_top_sg_fr = SgFr_next(LOCAL_top_sg_fr);
      }
    } else {
      while (LOCAL_top_sg_fr && 
             YOUNGER_CP(SgFr_gen_cp(LOCAL_top_sg_fr), Get_LOCAL_top_cp())) {
        mark_as_completed(LOCAL_top_sg_fr);
        LOCAL_top_sg_fr = SgFr_next(LOCAL_top_sg_fr);
      }
    }

    /* chain dependency frames to release */
    chain_dep_fr = NULL;
    while (YOUNGER_CP(DepFr_cons_cp(LOCAL_top_dep_fr), Get_LOCAL_top_cp())) {
      LOCK_DEP_FR(LOCAL_top_dep_fr);
      next_dep_fr = DepFr_next(LOCAL_top_dep_fr);
      DepFr_next(LOCAL_top_dep_fr) = chain_dep_fr;
      chain_dep_fr = LOCAL_top_dep_fr;
      LOCAL_top_dep_fr = next_dep_fr;
    }

    /* adjust freeze registers */
    adjust_freeze_registers();
  }

  /* chain or-frames to release */
  chain_or_fr = NULL;
  top_or_fr = Get_LOCAL_top_cp_on_stack()->cp_or_fr;
  while (top_or_fr != LOCAL_top_or_fr) {
    or_fr_ptr next_or_fr_on_stack;
    LOCK_OR_FRAME(top_or_fr);
    susp_fr = OrFr_suspensions(top_or_fr);
    while (susp_fr) {
      complete_suspension_branch(susp_fr, GetOrFr_node(top_or_fr), &chain_or_fr, &chain_dep_fr);
      next_susp_fr = SuspFr_next(susp_fr);
      FREE_SUSPENSION_FRAME(susp_fr);
      susp_fr = next_susp_fr;
    }
    next_or_fr_on_stack = OrFr_next_on_stack(top_or_fr);
    OrFr_next_on_stack(top_or_fr) = chain_or_fr;
    chain_or_fr = top_or_fr;
    top_or_fr = next_or_fr_on_stack;
  }
  LOCK_OR_FRAME(top_or_fr);
  susp_fr = OrFr_suspensions(top_or_fr);
  while (susp_fr) {
    complete_suspension_branch(susp_fr, GetOrFr_node(top_or_fr), &chain_or_fr, &chain_dep_fr);
    next_susp_fr = SuspFr_next(susp_fr);
    FREE_SUSPENSION_FRAME(susp_fr);
    susp_fr = next_susp_fr;
  }
  OrFr_suspensions(top_or_fr) = NULL;
  OrFr_nearest_suspnode(top_or_fr) = top_or_fr;
  UNLOCK_OR_FRAME(top_or_fr);

  /* release dependency frames */
  while (chain_dep_fr) {
    next_dep_fr = DepFr_next(chain_dep_fr);
    FREE_DEPENDENCY_FRAME(chain_dep_fr);
    chain_dep_fr = next_dep_fr;
  }

  /* release or frames */
  solutions = NULL;
  while (chain_or_fr) {
    aux_solutions = OrFr_qg_solutions(chain_or_fr);
    if (aux_solutions) {
      CUT_join_answers_in_an_unique_frame(aux_solutions);
      SolFr_next(aux_solutions) = solutions;
      solutions = aux_solutions;
    }
    next_or_fr = OrFr_next_on_stack(chain_or_fr);
    FREE_OR_FRAME(chain_or_fr);
    chain_or_fr = next_or_fr;
  }
  if (solutions) {
    CUT_join_answers_in_an_unique_frame(solutions);
    SolFr_next(solutions) = OrFr_qg_solutions(LOCAL_top_or_fr);
    OrFr_qg_solutions(LOCAL_top_or_fr) = solutions;
  }

  /* adjust top register */
  Set_LOCAL_top_cp_on_stack( Get_LOCAL_top_cp() );

  return;
}


void complete_suspension_frames(or_fr_ptr or_fr) {
  CACHE_REGS
  dep_fr_ptr chain_dep_fr;
  or_fr_ptr chain_or_fr;
  susp_fr_ptr susp_fr;
  qg_sol_fr_ptr solutions;

  /* complete suspension frames */
  chain_dep_fr = NULL;
  chain_or_fr = NULL;
  susp_fr = OrFr_suspensions(or_fr);
  do {
    susp_fr_ptr next_susp_fr;
    complete_suspension_branch(susp_fr, GetOrFr_node(or_fr), &chain_or_fr, &chain_dep_fr);
    next_susp_fr = SuspFr_next(susp_fr);
    FREE_SUSPENSION_FRAME(susp_fr);
    susp_fr = next_susp_fr;
  } while (susp_fr);
  OrFr_suspensions(or_fr) = NULL;
  OrFr_nearest_suspnode(or_fr) = or_fr;

  /* release dependency frames */
  while (chain_dep_fr) {
    dep_fr_ptr next_dep_fr;
    next_dep_fr = DepFr_next(chain_dep_fr);
    FREE_DEPENDENCY_FRAME(chain_dep_fr);
    chain_dep_fr = next_dep_fr;
  }

  /* release or frames */
  solutions = NULL;
  while (chain_or_fr) {
    or_fr_ptr next_or_fr;
    qg_sol_fr_ptr aux_solutions;
    aux_solutions = OrFr_qg_solutions(chain_or_fr);
    if (aux_solutions) {
      CUT_join_answers_in_an_unique_frame(aux_solutions);
      SolFr_next(aux_solutions) = solutions;
      solutions = aux_solutions;
    }
    next_or_fr = OrFr_next_on_stack(chain_or_fr);
    FREE_OR_FRAME(chain_or_fr);
    chain_or_fr = next_or_fr;
  }
  if (solutions) {
    CUT_join_answers_in_an_unique_frame(solutions);
    SolFr_next(solutions) = OrFr_qg_solutions(or_fr);
    OrFr_qg_solutions(LOCAL_top_or_fr) = solutions;
  }

  return;
}


void suspend_branch(void) {
  CACHE_REGS
  or_fr_ptr or_frame;

  /* suspension only occurs in shared nodes that **
  **   are leaders with younger consumer nodes   */
#ifdef DEBUG_OPTYAP
  OPTYAP_ERROR_CHECKING(suspend_branch, Get_LOCAL_top_cp()->cp_or_fr != LOCAL_top_or_fr);
  OPTYAP_ERROR_CHECKING(suspend_branch, B_FZ == Get_LOCAL_top_cp());
  OPTYAP_ERROR_CHECKING(suspend_branch, YOUNGER_CP(Get_LOCAL_top_cp(), Get_LOCAL_top_cp_on_stack()));
  OPTYAP_ERROR_CHECKING(suspend_branch, Get_LOCAL_top_cp()->cp_or_fr != LOCAL_top_or_fr);
  or_frame = Get_LOCAL_top_cp_on_stack()->cp_or_fr;
  while (or_frame != LOCAL_top_or_fr) {
    OPTYAP_ERROR_CHECKING(suspend_branch, YOUNGER_CP(Get_LOCAL_top_cp(), GetOrFr_node(or_frame)));
    or_frame = OrFr_next_on_stack(or_frame);
  }
#endif /* DEBUG_OPTYAP */

  or_frame = Get_LOCAL_top_cp_on_stack()->cp_or_fr;
  LOCK_OR_FRAME(or_frame);
  if (B_FZ == Get_LOCAL_top_cp_on_stack() && OrFr_owners(or_frame) > 1) {
    /* there are other workers sharing the whole branch **
    **         --> we can avoid suspension <--          */

    /* update shared nodes */
    OrFr_owners(or_frame)--;
    UNLOCK_OR_FRAME(or_frame);
    or_frame = OrFr_next_on_stack(or_frame);
    while (or_frame != LOCAL_top_or_fr) {
      LOCK_OR_FRAME(or_frame);
      OrFr_owners(or_frame)--;
      UNLOCK_OR_FRAME(or_frame);
      or_frame = OrFr_next_on_stack(or_frame);
    }
  } else {
    /* the branch has private parts **
    **    --> suspend branch <--    */
    susp_fr_ptr new_susp_fr;
    long h_size, b_size, tr_size;
    UNLOCK_OR_FRAME(or_frame);

    /* alloc suspension frame */
    h_size = (unsigned long) H_FZ - (unsigned long) Get_LOCAL_top_cp()->cp_h;
    b_size = (unsigned long) Get_LOCAL_top_cp() - (unsigned long) B_FZ;
    tr_size = (unsigned long) TR_FZ - (unsigned long) Get_LOCAL_top_cp()->cp_tr;
    new_suspension_frame(new_susp_fr, Get_LOCAL_top_cp_on_stack()->cp_or_fr, LOCAL_top_dep_fr, LOCAL_top_sg_fr,
                         Get_LOCAL_top_cp()->cp_h, B_FZ, Get_LOCAL_top_cp()->cp_tr, h_size, b_size, tr_size);

    /* store suspension frame in current top or-frame */
    LOCK_OR_FRAME(LOCAL_top_or_fr);
    if (OrFr_nearest_suspnode(LOCAL_top_or_fr) == LOCAL_top_or_fr)
      OrFr_nearest_suspnode(LOCAL_top_or_fr) = NULL;
    SuspFr_next(new_susp_fr) = OrFr_suspensions(LOCAL_top_or_fr);
    OrFr_suspensions(LOCAL_top_or_fr) = new_susp_fr;
    UNLOCK_OR_FRAME(LOCAL_top_or_fr);
  }

  /* adjust top pointers */
  while (LOCAL_top_sg_fr && YOUNGER_CP(SgFr_gen_cp(LOCAL_top_sg_fr), Get_LOCAL_top_cp_on_stack())) {
    SgFr_gen_worker(LOCAL_top_sg_fr) = MAX_WORKERS;
    LOCAL_top_sg_fr = SgFr_next(LOCAL_top_sg_fr);
  }
  while (LOCAL_top_sg_fr && YOUNGER_CP(SgFr_gen_cp(LOCAL_top_sg_fr), Get_LOCAL_top_cp())) {
    LOCAL_top_sg_fr = SgFr_next(LOCAL_top_sg_fr);
  }
  while (YOUNGER_CP(DepFr_cons_cp(LOCAL_top_dep_fr), Get_LOCAL_top_cp())) {
    LOCAL_top_dep_fr = DepFr_next(LOCAL_top_dep_fr);
  }
  Set_LOCAL_top_cp_on_stack( Get_LOCAL_top_cp() );

  /* adjust freeze registers */
  adjust_freeze_registers();

  return;
}


void resume_suspension_frame(susp_fr_ptr resume_fr, or_fr_ptr top_or_fr) {
  CACHE_REGS
  or_fr_ptr or_frame;
  sg_fr_ptr sg_frame;

  /* copy suspended stacks */
  memmove(SuspFr_global_reg(resume_fr),
         SuspFr_global_start(resume_fr),
         SuspFr_global_size(resume_fr));
  memmove(SuspFr_local_reg(resume_fr),
         SuspFr_local_start(resume_fr),
         SuspFr_local_size(resume_fr));
  memmove(SuspFr_trail_reg(resume_fr),
         SuspFr_trail_start(resume_fr),
         SuspFr_trail_size(resume_fr));

  OPTYAP_ERROR_CHECKING(resume_suspension_frame, DepFr_cons_cp(SuspFr_top_dep_fr(resume_fr))->cp_h != SuspFr_global_reg(resume_fr) + SuspFr_global_size(resume_fr));
  OPTYAP_ERROR_CHECKING(resume_suspension_frame, DepFr_cons_cp(SuspFr_top_dep_fr(resume_fr))->cp_tr != SuspFr_trail_reg(resume_fr) + SuspFr_trail_size(resume_fr));
  OPTYAP_ERROR_CHECKING(resume_suspension_frame, DepFr_cons_cp(SuspFr_top_dep_fr(resume_fr)) != SuspFr_local_reg(resume_fr));
  OPTYAP_ERROR_CHECKING(resume_suspension_frame, (void *)Get_LOCAL_top_cp() < SuspFr_local_reg(resume_fr) + SuspFr_local_size(resume_fr));

  /* update shared nodes */
  or_frame = top_or_fr;
  while (or_frame != LOCAL_top_or_fr) {
    LOCK_OR_FRAME(or_frame);
    OrFr_owners(or_frame)++;
    UNLOCK_OR_FRAME(or_frame);
    or_frame = OrFr_next_on_stack(or_frame);
  }  
  or_frame = top_or_fr;
  while (or_frame != LOCAL_top_or_fr) {
    LOCK_OR_FRAME(or_frame);
    BITMAP_insert(OrFr_members(or_frame), worker_id);
    BRANCH(worker_id, OrFr_depth(or_frame)) = 1;
    UNLOCK_OR_FRAME(or_frame);
    or_frame = OrFr_next(or_frame);
  }

  /* adjust top pointers */
  LOCAL_top_or_fr = top_or_fr;
  SetOrFr_node(top_or_fr, Get_LOCAL_top_cp());
  LOCAL_top_sg_fr = SuspFr_top_sg_fr(resume_fr);
  LOCAL_top_dep_fr = SuspFr_top_dep_fr(resume_fr);
  Set_LOCAL_top_cp_on_stack( GetOrFr_node(SuspFr_top_or_fr_on_stack(resume_fr)) );
  sg_frame = LOCAL_top_sg_fr;
  while (sg_frame && YOUNGER_CP(SgFr_gen_cp(sg_frame), Get_LOCAL_top_cp_on_stack())) {
    SgFr_gen_worker(sg_frame) = worker_id;
    sg_frame = SgFr_next(sg_frame);
  }

  /* adjust freeze registers */
  adjust_freeze_registers();

  /* free suspension frame */
  FREE_SUSPENSION_FRAME(resume_fr);

  return;
}
#endif /* YAPOR */
#endif /* TABLING */
