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



/* -------------------------- **
**      Global functions      **
** -------------------------- */

void prune_shared_branch(choiceptr prune_cp, int* pend_ltt) {
  CACHE_REGS
  int i, ltt, depth;
  bitmap members;
  choiceptr leftmost_cp;
  or_fr_ptr leftmost_or_fr;
  qg_sol_fr_ptr qg_solutions, aux_qg_solutions;
#ifdef TABLING_INNER_CUTS
  tg_sol_fr_ptr tg_solutions, aux_tg_solutions;
#endif /* TABLING_INNER_CUTS */
  leftmost_or_fr = CUT_leftmost_or_frame();
  if (Get_LOCAL_prune_request())
    return;
  leftmost_cp = GetOrFr_node(leftmost_or_fr);
  qg_solutions = NULL;
#ifdef TABLING_INNER_CUTS
  tg_solutions = NULL;
#endif /* TABLING_INNER_CUTS */
  if (EQUAL_OR_YOUNGER_CP(prune_cp, leftmost_cp)) {
    /* pruning being leftmost */
    or_fr_ptr prune_or_fr;


    bitmap members2;
    or_fr_ptr old_LOCAL_top_or_fr =  LOCAL_top_or_fr;
    
    if(pend_ltt){
    LOCK_OR_FRAME(LOCAL_top_or_fr);
    int depth2 = OrFr_depth(LOCAL_top_or_fr);
    members2 = OrFr_members(LOCAL_top_or_fr);
      for (i = 0; i < GLOBAL_number_workers; i++) {
	if (BITMAP_member(members2, i) && *pend_ltt != BRANCH_LTT(i,depth2)){
	  BITMAP_delete(members2,i);
	}
      }
    }

    /* send prune requests */
    prune_or_fr = prune_cp->cp_or_fr;
    depth = OrFr_depth(prune_or_fr);
    ltt = BRANCH_LTT(worker_id, depth);
    LOCK_OR_FRAME(prune_or_fr);
    members = OrFr_members(prune_or_fr);

    if(pend_ltt){
      BITMAP_minus(members,members2);
    }


    BITMAP_delete(members, worker_id);
    
    for (i = 0; i < GLOBAL_number_workers; i++) {
      if (BITMAP_member(members, i) && ltt == BRANCH_LTT(i, depth)) {
        CUT_send_prune_request(i, prune_cp);
      }
    }
    UNLOCK_OR_FRAME(prune_or_fr);

    /* move up to prune_cp */
    do {
      ltt = BRANCH_LTT(worker_id, OrFr_depth(LOCAL_top_or_fr));
      if(!pend_ltt || LOCAL_top_or_fr != old_LOCAL_top_or_fr)
         LOCK_OR_FRAME(LOCAL_top_or_fr);
      if (Get_OrFr_pend_prune_cp(LOCAL_top_or_fr)){
        Set_OrFr_pend_prune_cp(LOCAL_top_or_fr, NULL);
      }
      aux_qg_solutions = OrFr_qg_solutions(LOCAL_top_or_fr);
#ifdef TABLING_INNER_CUTS
      aux_tg_solutions = OrFr_tg_solutions(LOCAL_top_or_fr);
#endif /* TABLING_INNER_CUTS */
      if (BITMAP_alone(OrFr_members(LOCAL_top_or_fr), worker_id)) {
#ifdef TABLING
        if (OrFr_suspensions(LOCAL_top_or_fr) || OrFr_owners(LOCAL_top_or_fr) != 1)
          pruning_over_tabling_data_structures();
#endif /* TABLING */
        if(!pend_ltt || LOCAL_top_or_fr != old_LOCAL_top_or_fr){
          FREE_OR_FRAME(LOCAL_top_or_fr);
	}
      } else {
        OrFr_qg_solutions(LOCAL_top_or_fr) = NULL;
#ifdef TABLING_INNER_CUTS
        OrFr_tg_solutions(LOCAL_top_or_fr) = NULL;
#endif /* TABLING_INNER_CUTS */
        OrFr_alternative(LOCAL_top_or_fr) = NULL;
        BITMAP_delete(OrFr_members(LOCAL_top_or_fr), worker_id);
#ifdef TABLING
        OrFr_owners(LOCAL_top_or_fr)--;
#endif /* TABLING */
       if(!pend_ltt || LOCAL_top_or_fr != old_LOCAL_top_or_fr)
        UNLOCK_OR_FRAME(LOCAL_top_or_fr);
      }
      if ((aux_qg_solutions = CUT_prune_solution_frames(aux_qg_solutions, ltt))) {
        CUT_join_answers_in_an_unique_frame(aux_qg_solutions);
        SolFr_next(aux_qg_solutions) = qg_solutions;
        qg_solutions = aux_qg_solutions;
      }
#ifdef TABLING_INNER_CUTS
      if ((aux_tg_solutions = CUT_prune_tg_solution_frames(aux_tg_solutions, ltt))) {
        CUT_join_tg_solutions(& tg_solutions, aux_tg_solutions);
      }
#endif /* TABLING_INNER_CUTS */
      SCH_update_local_or_tops();
    } while (Get_LOCAL_top_cp() != prune_cp);

    
    if(pend_ltt){
      UNLOCK_OR_FRAME(old_LOCAL_top_or_fr);
      if (BITMAP_alone(OrFr_members(old_LOCAL_top_or_fr), worker_id)){
	FREE_OR_FRAME(old_LOCAL_top_or_fr);
      }
    }


    YAPOR_ERROR_CHECKING(prune_shared_branch, Get_LOCAL_prune_request() && EQUAL_OR_YOUNGER_CP(Get_LOCAL_prune_request(), Get_LOCAL_top_cp()));
    /* store answers not pruned */
    if (qg_solutions)
      CUT_join_answers_in_an_unique_frame(qg_solutions);
    LOCK_OR_FRAME(leftmost_or_fr);
    if (Get_LOCAL_prune_request()) {
      UNLOCK_OR_FRAME(leftmost_or_fr);
      if (qg_solutions)
        CUT_free_solution_frame(qg_solutions);
#ifdef TABLING_INNER_CUTS
      CUT_free_tg_solution_frames(tg_solutions);
#endif /* TABLING_INNER_CUTS */
    } else {
      if (qg_solutions)
        CUT_store_answers(leftmost_or_fr, qg_solutions);
#ifdef TABLING_INNER_CUTS
      if (tg_solutions)
        tg_solutions = CUT_store_tg_answers(leftmost_or_fr, tg_solutions, BRANCH_LTT(worker_id, OrFr_depth(leftmost_or_fr)));
#endif /* TABLING_INNER_CUTS */
      UNLOCK_OR_FRAME(leftmost_or_fr);
#ifdef TABLING_INNER_CUTS
      CUT_validate_tg_answers(tg_solutions);
#endif /* TABLING_INNER_CUTS */
    }
  } else {
    /* pruning not being leftmost */
    int prune_more;
    prune_more = 1;

    bitmap members2;
    or_fr_ptr old_LOCAL_top_or_fr =  LOCAL_top_or_fr;
    
    if(pend_ltt){
    LOCK_OR_FRAME(LOCAL_top_or_fr);
    int depth2 = OrFr_depth(LOCAL_top_or_fr);
    members2 = OrFr_members(LOCAL_top_or_fr);
      for (i = 0; i < GLOBAL_number_workers; i++) {
	if (BITMAP_member(members2, i) && *pend_ltt != BRANCH_LTT(i,depth2)){
	  BITMAP_delete(members2,i);
	}
      }
    }


    /* send prune requests */
    depth = OrFr_depth(leftmost_or_fr);
    ltt = BRANCH_LTT(worker_id, depth);
    if(!pend_ltt || LOCAL_top_or_fr != leftmost_or_fr)
    	LOCK_OR_FRAME(leftmost_or_fr);
    members = OrFr_members(leftmost_or_fr);

    if(pend_ltt){
      BITMAP_minus(members,members2);
    }


    BITMAP_delete(members, worker_id);
    for (i = 0; i < GLOBAL_number_workers; i++) {
      if (BITMAP_member(members, i)) {
        if (ltt >= BRANCH_LTT(i, depth)) {
          CUT_send_prune_request(i, leftmost_cp->cp_b);
        } else if (BRANCH_CUT(i, depth)) {
          prune_more = 0; 
        }
      }
    }
    if(!pend_ltt || LOCAL_top_or_fr != leftmost_or_fr)
    UNLOCK_OR_FRAME(leftmost_or_fr);

    /* move up to leftmost_cp */
    while (Get_LOCAL_top_cp() != leftmost_cp) {
      ltt = BRANCH_LTT(worker_id, OrFr_depth(LOCAL_top_or_fr));
      if(!pend_ltt || LOCAL_top_or_fr != old_LOCAL_top_or_fr)
      	LOCK_OR_FRAME(LOCAL_top_or_fr);
      if (Get_OrFr_pend_prune_cp(LOCAL_top_or_fr))
        prune_more = 0;
      if (Get_OrFr_pend_prune_cp(LOCAL_top_or_fr)){
        Set_OrFr_pend_prune_cp(LOCAL_top_or_fr, NULL);
      }
      aux_qg_solutions = OrFr_qg_solutions(LOCAL_top_or_fr);
#ifdef TABLING_INNER_CUTS
      aux_tg_solutions = OrFr_tg_solutions(LOCAL_top_or_fr);
#endif /* TABLING_INNER_CUTS */
      if (BITMAP_alone(OrFr_members(LOCAL_top_or_fr), worker_id)) {
#ifdef TABLING
        if (OrFr_suspensions(LOCAL_top_or_fr) || OrFr_owners(LOCAL_top_or_fr) != 1)
          pruning_over_tabling_data_structures();
#endif /* TABLING */
       if(!pend_ltt || LOCAL_top_or_fr != old_LOCAL_top_or_fr){
        FREE_OR_FRAME(LOCAL_top_or_fr);
       }
      } else {
        OrFr_qg_solutions(LOCAL_top_or_fr) = NULL;
#ifdef TABLING_INNER_CUTS
        OrFr_tg_solutions(LOCAL_top_or_fr) = NULL;
#endif /* TABLING_INNER_CUTS */
        OrFr_alternative(LOCAL_top_or_fr) = NULL;
        BITMAP_delete(OrFr_members(LOCAL_top_or_fr), worker_id);
#ifdef TABLING
        OrFr_owners(LOCAL_top_or_fr)--;
#endif /* TABLING */
        if(!pend_ltt || LOCAL_top_or_fr != old_LOCAL_top_or_fr)
          UNLOCK_OR_FRAME(LOCAL_top_or_fr);
      }
      if ((aux_qg_solutions = CUT_prune_solution_frames(aux_qg_solutions, ltt))) {
        CUT_join_answers_in_an_unique_frame(aux_qg_solutions);
        SolFr_next(aux_qg_solutions) = qg_solutions;
        qg_solutions = aux_qg_solutions;
      }
#ifdef TABLING_INNER_CUTS
      if ((aux_tg_solutions = CUT_prune_tg_solution_frames(aux_tg_solutions, ltt))) {
        CUT_join_tg_solutions(& tg_solutions, aux_tg_solutions);
      }
#endif /* TABLING_INNER_CUTS */
      SCH_update_local_or_tops();
    }

    if(pend_ltt){
      UNLOCK_OR_FRAME(old_LOCAL_top_or_fr);    
      if (BITMAP_alone(OrFr_members(old_LOCAL_top_or_fr), worker_id)){
	FREE_OR_FRAME(old_LOCAL_top_or_fr);
      }
    }

    YAPOR_ERROR_CHECKING(prune_shared_branch, Get_LOCAL_prune_request() && EQUAL_OR_YOUNGER_CP(Get_LOCAL_prune_request(), Get_LOCAL_top_cp()));
    /* store answers not pruned */
    if (qg_solutions)
      CUT_join_answers_in_an_unique_frame(qg_solutions);
    LOCK_OR_FRAME(leftmost_or_fr);
    if (Get_LOCAL_prune_request()) {
      UNLOCK_OR_FRAME(leftmost_or_fr);
      if (qg_solutions)
        CUT_free_solution_frame(qg_solutions);
#ifdef TABLING_INNER_CUTS
      CUT_free_tg_solution_frames(tg_solutions);
#endif /* TABLING_INNER_CUTS */
    } else {
      ltt = BRANCH_LTT(worker_id, depth);
      if (qg_solutions)
        CUT_store_answers(leftmost_or_fr, qg_solutions);
#ifdef TABLING_INNER_CUTS
      if (tg_solutions)
        tg_solutions = CUT_store_tg_answers(leftmost_or_fr, tg_solutions, ltt);
#endif /* TABLING_INNER_CUTS */
      if (Get_OrFr_pend_prune_cp(leftmost_or_fr))
        prune_more = 0;
      OrFr_alternative(leftmost_or_fr) = NULL;
      if (!Get_LOCAL_prune_request()){
        Set_OrFr_pend_prune_cp(leftmost_or_fr, prune_cp);
        OrFr_pend_prune_ltt(leftmost_or_fr) = ltt;
      }
      UNLOCK_OR_FRAME(leftmost_or_fr);
#ifdef TABLING_INNER_CUTS
      CUT_validate_tg_answers(tg_solutions);
#endif /* TABLING_INNER_CUTS */

      /* continue pruning to prune_cp */
      if (prune_more) {
        BITMAP_copy(members, OrFr_members(leftmost_or_fr));
        leftmost_cp = leftmost_cp->cp_b;
        while (leftmost_cp != prune_cp) {
          leftmost_or_fr = leftmost_cp->cp_or_fr;
          depth = OrFr_depth(leftmost_or_fr);
          ltt = BRANCH_LTT(worker_id, depth);
          LOCK_OR_FRAME(leftmost_or_fr);
          BITMAP_difference(members, OrFr_members(leftmost_or_fr), members);
          for (i = 0; i < GLOBAL_number_workers; i++) {
            if (BITMAP_member(members, i)) {
              if (ltt > BRANCH_LTT(i, depth)) {
                CUT_send_prune_request(i, leftmost_cp->cp_b); 
              } else if (BRANCH_CUT(i, depth)) {
                UNLOCK_OR_FRAME(leftmost_or_fr);
                goto end_prune_more;
              }
	    }
	  }
          OrFr_alternative(leftmost_or_fr) = NULL;
          UNLOCK_OR_FRAME(leftmost_or_fr);
          BITMAP_copy(members, OrFr_members(leftmost_or_fr));
          leftmost_cp = leftmost_cp->cp_b;
        }
      }
    }
  }

end_prune_more:
  CUT_reset_prune_request();
#ifdef TABLING
  Set_LOCAL_top_cp_on_stack(Get_LOCAL_top_cp());
#endif /* TABLING */
  return;
}
#endif /* YAPOR */
