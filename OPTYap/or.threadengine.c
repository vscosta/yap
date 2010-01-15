/**********************************************************************
                                                               
                       The OPTYap Prolog system                
  OPTYap extends the Yap Prolog system to support or-parallel tabling
                                                               
  Copyright:   R. Rocha and NCC - University of Porto, Portugal
  File:        or.threadengine.c
  version:     $Id: or.engine.c,v 1.11 2008-03-25 16:45:53 vsc Exp $   
                                                                     
**********************************************************************/

/* ------------------ **
**      Includes      **
** ------------------ */

#include "Yap.h"
#ifdef THREADS
#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */
#include "Yatom.h"
#include "YapHeap.h"
#include "or.macros.h"
#ifdef TABLING
#include "tab.macros.h"
#else
#include "opt.mavar.h"
#endif /* TABLING */



/* ------------------------------------- **
**      Local functions declaration      **
** ------------------------------------- */

static void share_private_nodes(int worker_q);



/* ---------------------- **
**      Local macros      **
** ---------------------- */

/* -------------------------- **
**      Global functions      **
** -------------------------- */

void make_root_choice_point(void) {
  if (worker_id == 0) {
    SetOrFr_node(GLOBAL_root_or_fr, B);
    Set_LOCAL_top_cp(B);
    Set_GLOBAL_root_cp(B);
  } else {
    Set_LOCAL_top_cp(Get_GLOBAL_root_cp());
    B = Get_GLOBAL_root_cp();
    B->cp_tr = TR = ((choiceptr) (worker_offset(0) + (CELL)(B)))->cp_tr;
  }
  B->cp_h = H0;
  B->cp_ap = GETWORK;
  B->cp_or_fr = GLOBAL_root_or_fr;
  LOCAL_top_or_fr = GLOBAL_root_or_fr;
  LOCAL_load = 0;
  LOCAL_prune_request = NULL;
  BRANCH(worker_id, 0) = 0;
#ifdef TABLING_INNER_CUTS
  LOCAL_pruning_scope = NULL;
#endif /* TABLING_INNER_CUTS */
#ifdef TABLING
  Set_LOCAL_top_cp_on_stack(Get_LOCAL_top_cp());
  adjust_freeze_registers();
#endif /* TABLING */
  return;
}


void free_root_choice_point(void) {
  B = Get_LOCAL_top_cp()->cp_b;
#ifdef TABLING
  Set_LOCAL_top_cp_on_stack(B_BASE);
#endif /* TABLING */
  Set_GLOBAL_root_cp( B_BASE );
  Set_LOCAL_top_cp( B_BASE );
  SetOrFr_node(GLOBAL_root_or_fr, B_BASE);
  return;
}


int p_share_work(void) {
  int worker_q = LOCAL_share_request;

  if (! BITMAP_member(OrFr_members(REMOTE_top_or_fr(worker_q)), worker_id) ||
      B == REMOTE_top_cp(worker_q) ||
      (LOCAL_load <= DELAYED_RELEASE_LOAD && OrFr_nearest_livenode(LOCAL_top_or_fr) == NULL)) {
    /* refuse sharing request */
    REMOTE_reply_signal(LOCAL_share_request) = no_sharing;
    LOCAL_share_request = MAX_WORKERS;
    PUT_OUT_REQUESTABLE(worker_id);
    return TRUE;
  }
  /* sharing request accepted */
  REMOTE_q_fase_signal(worker_q) = Q_idle;
  REMOTE_p_fase_signal(worker_q) = P_idle;
#ifndef TABLING
  /* wait for incomplete installations */
  while (LOCAL_reply_signal != worker_ready);
#endif /* TABLING */
  LOCAL_reply_signal = sharing;
  REMOTE_reply_signal(worker_q) = sharing;
  share_private_nodes(worker_q);
  REMOTE_reply_signal(worker_q) = nodes_shared;
  while (LOCAL_reply_signal == sharing);
  LOCAL_share_request = MAX_WORKERS;
  PUT_IN_REQUESTABLE(worker_id);

  return TRUE;
}


int q_share_work(int worker_p) {
  LOCK_OR_FRAME(LOCAL_top_or_fr);
  if (REMOTE_prune_request(worker_p)) {
    /* worker p with prune request */
    UNLOCK_OR_FRAME(LOCAL_top_or_fr);
    return FALSE;
  }
#ifdef YAPOR_ERRORS
  if (OrFr_pend_prune_cp(LOCAL_top_or_fr) &&
      BRANCH_LTT(worker_p, OrFr_depth(LOCAL_top_or_fr)) < OrFr_pend_prune_ltt(LOCAL_top_or_fr))
    YAPOR_ERROR_MESSAGE("prune ltt > worker_p branch ltt (q_share_work)");
#endif /* YAPOR_ERRORS */
  /* there is no pending prune with worker p at right --> safe move to worker p branch */
  BRANCH(worker_id, OrFr_depth(LOCAL_top_or_fr)) = BRANCH(worker_p, OrFr_depth(LOCAL_top_or_fr));
  LOCAL_prune_request = NULL;
  UNLOCK_OR_FRAME(LOCAL_top_or_fr);

#ifdef OPTYAP_ERRORS
  if (Get_LOCAL_top_cp() != Get_LOCAL_top_cp_on_stack())
    OPTYAP_ERROR_MESSAGE("LOCAL_top_cp != LOCAL_top_cp_on_stack (q_share_work)");
  if (YOUNGER_CP(B_FZ, Get_LOCAL_top_cp()))
    OPTYAP_ERROR_MESSAGE("YOUNGER_CP(B_FZ, LOCAL_top_cp) (q_share_work)");
#endif /* OPTYAP_ERRORS */
#ifdef YAPOR_ERRORS
  if (LOCAL_reply_signal != worker_ready)
    YAPOR_ERROR_MESSAGE("LOCAL_reply_signal != worker_ready (q_share_work)");
#endif /* YAPOR_ERRORS */

  /* make sharing request */
  LOCK_WORKER(worker_p);
  if (BITMAP_member(GLOBAL_bm_idle_workers, worker_p) || 
      REMOTE_share_request(worker_p) != MAX_WORKERS) {
    /* worker p is idle or has another request */
    UNLOCK_WORKER(worker_p);
    return FALSE;
  }
  REMOTE_share_request(worker_p) = worker_id;
  UNLOCK_WORKER(worker_p);

  /* wait for an answer */
  while (LOCAL_reply_signal == worker_ready);
  if (LOCAL_reply_signal == no_sharing) {
    /* sharing request refused */
    LOCAL_reply_signal = worker_ready;
    return FALSE;
  }
  while (LOCAL_reply_signal == sharing);

  Yap_CopyThreadStacks(worker_id, worker_p);

  /* update registers and return */
#ifndef TABLING
  REMOTE_reply_signal(worker_p) = worker_ready;
#endif /* TABLING */
  LOCAL_reply_signal = worker_ready;
  PUT_IN_REQUESTABLE(worker_id);
  return TRUE;
}



/* ------------------------- **
**      Local functions      **
** ------------------------- */

static
void share_private_nodes(int worker_q) {
  choiceptr sharing_node = B;

#ifdef OPTYAP_ERRORS
  if (YOUNGER_CP(Get_LOCAL_top_cp(), Get_LOCAL_top_cp_on_stack())) {
    OPTYAP_ERROR_MESSAGE("YOUNGER_CP(LOCAL_top_cp, LOCAL_top_cp_on_stack) (share_private_nodes)");
  } else {
    choiceptr aux_cp = B;
    while (aux_cp != Get_LOCAL_top_cp()) {
      if (YOUNGER_CP(Get_LOCAL_top_cp(), aux_cp)) {
        OPTYAP_ERROR_MESSAGE("LOCAL_top_cp not in branch (share_private_nodes)");
        break;
      }
      if (EQUAL_OR_YOUNGER_CP(Get_LOCAL_top_cp_on_stack(), aux_cp)) {
        OPTYAP_ERROR_MESSAGE("shared frozen segments in branch (share_private_nodes)");
        break;
      }
      aux_cp = aux_cp->cp_b;
    }
  }
#endif /* OPTYAP_ERRORS */

#ifdef TABLING
  /* check if the branch is already shared */
  if (EQUAL_OR_YOUNGER_CP(Get_LOCAL_top_cp_on_stack(), sharing_node)) {
    or_fr_ptr or_frame;
    sg_fr_ptr sg_frame;
    dep_fr_ptr dep_frame;

#ifdef OPTYAP_ERRORS
    { or_fr_ptr aux_or_fr;
      aux_or_fr = LOCAL_top_or_fr;
      while (aux_or_fr != REMOTE_top_or_fr(worker_q)) {
        if (YOUNGER_CP(GetOrFr_node(REMOTE_top_or_fr(worker_q)), GetOrFr_node(aux_or_fr))) {
          OPTYAP_ERROR_MESSAGE("YOUNGER_CP(GetOrFr_node(REMOTE_top_or_fr(worker_q)), OrFr_node(aux_or_fr)) (share_private_nodes)");
          break; 
        }
        aux_or_fr = OrFr_next_on_stack(aux_or_fr);
      }
    }
#endif /* OPTYAP_ERRORS */

    /* update old shared nodes */
    or_frame = LOCAL_top_or_fr;
    while (or_frame != REMOTE_top_or_fr(worker_q)) {
      LOCK_OR_FRAME(or_frame);
      BRANCH(worker_q, OrFr_depth(or_frame)) = BRANCH(worker_id, OrFr_depth(or_frame));
      OrFr_owners(or_frame)++;
      if (BITMAP_member(OrFr_members(or_frame), worker_id))
        BITMAP_insert(OrFr_members(or_frame), worker_q);
      UNLOCK_OR_FRAME(or_frame);
      or_frame = OrFr_next_on_stack(or_frame);
    }

    /* update worker Q top subgoal frame */
    sg_frame = LOCAL_top_sg_fr;
    while (sg_frame && YOUNGER_CP(SgFr_gen_cp(sg_frame), sharing_node)) {
      sg_frame = SgFr_next(sg_frame);
    }
    REMOTE_top_sg_fr(worker_q) = sg_frame;

    /* update worker Q top dependency frame */
    dep_frame = LOCAL_top_dep_fr;
    while (YOUNGER_CP(DepFr_cons_cp(dep_frame), sharing_node)) {
      dep_frame = DepFr_next(dep_frame);
    }
    REMOTE_top_dep_fr(worker_q) = dep_frame;

    /* update worker Q top shared nodes */
    Set_REMOTE_top_cp_on_stack(worker_q, Get_LOCAL_top_cp());
    Set_REMOTE_top_cp(worker_q, Get_LOCAL_top_cp());
    REMOTE_top_or_fr(worker_q) = LOCAL_top_or_fr;
  } else
#endif /* TABLING */
  {
    int depth;
    bitmap bm_workers;
    or_fr_ptr or_frame, previous_or_frame;
#ifdef TABLING
    choiceptr consumer_cp, next_node_on_branch;
    dep_fr_ptr dep_frame;
    sg_fr_ptr sg_frame;
    CELL *stack, *stack_base, *stack_limit;

    /* find top dependency frame above current choice point */
    dep_frame = LOCAL_top_dep_fr;
    while (EQUAL_OR_YOUNGER_CP(DepFr_cons_cp(dep_frame), sharing_node)) {
      dep_frame = DepFr_next(dep_frame);
    }
    /* initialize tabling auxiliary variables */ 
    consumer_cp = DepFr_cons_cp(dep_frame);
    next_node_on_branch = NULL;
    stack_limit = (CELL *)TR;
    stack_base = stack = (CELL *)Yap_TrailTop;
#endif /* TABLING */

    /* initialize auxiliary variables */
    BITMAP_clear(bm_workers);
    BITMAP_insert(bm_workers, worker_id);
    BITMAP_insert(bm_workers, worker_q);
    previous_or_frame = NULL;
    depth = OrFr_depth(LOCAL_top_or_fr);

    /* sharing loop */
#ifdef TABLING
    while (YOUNGER_CP(sharing_node, Get_LOCAL_top_cp_on_stack())) {
#else
      while (sharing_node != Get_LOCAL_top_cp()) {
#endif /* TABLING */

#ifdef OPTYAP_ERRORS
      if (next_node_on_branch) {
        choiceptr aux_cp = B;
        while (aux_cp != next_node_on_branch) {
          if (sharing_node == aux_cp)
            OPTYAP_ERROR_MESSAGE("sharing_node on branch (share_private_nodes)");
          if (YOUNGER_CP(next_node_on_branch, aux_cp)) {
            OPTYAP_ERROR_MESSAGE("next_node_on_branch not in branch (share_private_nodes)");
            break;
  	  }
          aux_cp = aux_cp->cp_b;
        }
      } else {
        choiceptr aux_cp = B;
        while (aux_cp != sharing_node) {
          if (YOUNGER_CP(sharing_node, aux_cp)) {
            OPTYAP_ERROR_MESSAGE("sharing_node not in branch (share_private_nodes)");
            break;
	  }
          aux_cp = aux_cp->cp_b;
        }
      }
#endif /* OPTYAP_ERRORS */

      ALLOC_OR_FRAME(or_frame);
      if (previous_or_frame) {
#ifdef TABLING
        OrFr_next_on_stack(previous_or_frame) =
#endif /* TABLING */
        OrFr_nearest_livenode(previous_or_frame) = OrFr_next(previous_or_frame) = or_frame;
      }
      previous_or_frame = or_frame;
      depth++;
      INIT_LOCK(OrFr_lock(or_frame));
      SetOrFr_node(or_frame, sharing_node);
      OrFr_alternative(or_frame) = sharing_node->cp_ap;
      OrFr_pend_prune_cp(or_frame) = NULL;
      OrFr_nearest_leftnode(or_frame) = LOCAL_top_or_fr;
      OrFr_qg_solutions(or_frame) = NULL;
#ifdef TABLING_INNER_CUTS
      OrFr_tg_solutions(or_frame) = NULL;
#endif /* TABLING_INNER_CUTS */
#ifdef TABLING
      OrFr_suspensions(or_frame) = NULL;
      OrFr_nearest_suspnode(or_frame) = or_frame;
      OrFr_owners(or_frame) = 2;
      if (next_node_on_branch)
        BITMAP_clear(OrFr_members(or_frame));
      else
#endif /* TABLING */
        OrFr_members(or_frame) = bm_workers;

#ifdef YAPOR_ERRORS
      if (sharing_node->cp_ap == GETWORK || sharing_node->cp_ap == GETWORK_SEQ)
        YAPOR_ERROR_MESSAGE("choicepoint already shared (share_private_nodes)");
#endif /* YAPOR_ERRORS */
      if (sharing_node->cp_ap && YAMOP_SEQ(sharing_node->cp_ap)) {
        sharing_node->cp_ap = GETWORK_SEQ;
      } else {
        sharing_node->cp_ap = GETWORK;
      }
      sharing_node->cp_or_fr = or_frame;
      sharing_node = sharing_node->cp_b;

#ifdef TABLING
      /* when next_node_on_branch is not NULL the **
      ** sharing_node belongs to a frozen branch. */   
      if (YOUNGER_CP(consumer_cp, sharing_node)) {
        /* frozen stack segment */
        if (! next_node_on_branch)
          next_node_on_branch = sharing_node;
        STACK_PUSH_UP(or_frame, stack);
        STACK_CHECK_EXPAND1(stack, stack_limit, stack_base);
        STACK_PUSH_UP(sharing_node, stack);  /* vsc: STACK_PUSH -> STACK_PUSH_UP? */
        STACK_CHECK_EXPAND1(stack, stack_limit, stack_base);
        sharing_node = consumer_cp;
        dep_frame = DepFr_next(dep_frame);
        consumer_cp = DepFr_cons_cp(dep_frame);
      } else if (consumer_cp == sharing_node) {
        dep_frame = DepFr_next(dep_frame);
        consumer_cp = DepFr_cons_cp(dep_frame);
      }
      if (next_node_on_branch == sharing_node)
        next_node_on_branch = NULL;
#endif /* TABLING */
#ifdef OPTYAP_ERRORS
      if (next_node_on_branch && YOUNGER_CP(next_node_on_branch, sharing_node))
        OPTYAP_ERROR_MESSAGE("frozen node greater than next_node_on_branch (share_private_nodes)");
#endif /* OPTYAP_ERRORS */
    }

    /* initialize last or-frame pointer */
    or_frame = sharing_node->cp_or_fr;
    if (previous_or_frame) {
#ifdef TABLING
      OrFr_next_on_stack(previous_or_frame) =
#endif /* TABLING */
      OrFr_nearest_livenode(previous_or_frame) = OrFr_next(previous_or_frame) = or_frame;
    }

#ifdef TABLING
    /* update or-frames stored in auxiliary stack */
    while (STACK_NOT_EMPTY(stack, stack_base)) {
      next_node_on_branch = (choiceptr) STACK_POP_DOWN(stack);
      or_frame = (or_fr_ptr) STACK_POP_DOWN(stack);
      OrFr_nearest_livenode(or_frame) = OrFr_next(or_frame) = next_node_on_branch->cp_or_fr;
    }
#endif /* TABLING */

    /* update depth */
    if (depth >= MAX_BRANCH_DEPTH)
      Yap_Error(INTERNAL_ERROR, TermNil, "maximum depth exceded (share_private_nodes)");
    or_frame = B->cp_or_fr;
#ifdef TABLING
    previous_or_frame = Get_LOCAL_top_cp_on_stack()->cp_or_fr;
    while (or_frame != previous_or_frame) {
#else
    while (or_frame != LOCAL_top_or_fr) {
#endif /* TABLING */
      unsigned int branch;
      if (OrFr_alternative(or_frame)) {
        branch = YAMOP_OR_ARG(OrFr_alternative(or_frame)) + 1;
      } else {
        branch = 1;
      }
      branch |= YAMOP_CUT_FLAG;  /* in doubt, assume cut */
      BRANCH(worker_id, depth) = BRANCH(worker_q, depth) = branch;
      OrFr_depth(or_frame) = depth--;
      or_frame = OrFr_next_on_stack(or_frame);
    }

#ifdef YAPOR_ERRORS
    if (depth != OrFr_depth(LOCAL_top_or_fr))
      YAPOR_ERROR_MESSAGE("incorrect depth value (share_private_nodes)");
#endif /* YAPOR_ERRORS */

#ifdef OPTYAP_ERRORS
    { or_fr_ptr aux_or_fr = B->cp_or_fr;
      choiceptr aux_cp;
      while (aux_or_fr != Get_LOCAL_top_cp_on_stack()->cp_or_fr) {
        aux_cp = GetOrFr_node(aux_or_fr);
        if (OrFr_next(aux_or_fr) != aux_cp->cp_b->cp_or_fr)
          OPTYAP_ERROR_MESSAGE("OrFr_next not in branch (share_private_nodes)");
        if (OrFr_nearest_livenode(aux_or_fr) != aux_cp->cp_b->cp_or_fr)
          OPTYAP_ERROR_MESSAGE("OrFr_nearest_livenode not in branch (share_private_nodes)");
        aux_or_fr = OrFr_next_on_stack(aux_or_fr);
      }
      aux_or_fr = B->cp_or_fr;
      while (aux_or_fr != Get_LOCAL_top_cp_on_stack()->cp_or_fr) {
        or_fr_ptr nearest_leftnode = OrFr_nearest_leftnode(aux_or_fr);
        aux_cp = GetOrFr_node(aux_or_fr);
        while (GetOrFr_node(nearest_leftnode) != aux_cp) {
          if (YOUNGER_CP(GetOrFr_node(nearest_leftnode), aux_cp)) {
            OPTYAP_ERROR_MESSAGE("OrFr_nearest_leftnode not in branch (share_private_nodes)");
            break;
          }
          aux_cp = aux_cp->cp_b;
        }
        aux_or_fr = OrFr_next_on_stack(aux_or_fr);
      }
    }
#endif /* OPTYAP_ERRORS */

    /* update old shared nodes */
    while (or_frame != REMOTE_top_or_fr(worker_q)) {
      LOCK_OR_FRAME(or_frame);
      BRANCH(worker_q, OrFr_depth(or_frame)) = BRANCH(worker_id, OrFr_depth(or_frame));
#ifdef TABLING
      OrFr_owners(or_frame)++;
      if (BITMAP_member(OrFr_members(or_frame), worker_id))
#endif /* TABLING */
        BITMAP_insert(OrFr_members(or_frame), worker_q);
      UNLOCK_OR_FRAME(or_frame);
      or_frame = OrFr_next_on_stack(or_frame);
    }

#ifdef TABLING
    /* update subgoal frames in the maintained private branches */
    sg_frame = LOCAL_top_sg_fr;
    while (sg_frame && YOUNGER_CP(SgFr_gen_cp(sg_frame), B)) {
      choiceptr top_cp_on_branch;
      top_cp_on_branch = SgFr_gen_cp(sg_frame);
      while (YOUNGER_CP(top_cp_on_branch, B)) {
        top_cp_on_branch = top_cp_on_branch->cp_b;
      }
      SgFr_gen_top_or_fr(sg_frame) = top_cp_on_branch->cp_or_fr;
      sg_frame = SgFr_next(sg_frame);
    }
    /* update worker Q top subgoal frame */
    REMOTE_top_sg_fr(worker_q) = sg_frame;
    /* update subgoal frames in the recently shared branches */
    while (sg_frame && YOUNGER_CP(SgFr_gen_cp(sg_frame), Get_LOCAL_top_cp_on_stack())) {
      SgFr_gen_worker(sg_frame) = MAX_WORKERS;
      SgFr_gen_top_or_fr(sg_frame) = SgFr_gen_cp(sg_frame)->cp_or_fr;
      sg_frame = SgFr_next(sg_frame);
    }

    /* update dependency frames in the maintained private branches */
    dep_frame = LOCAL_top_dep_fr;
    while (YOUNGER_CP(DepFr_cons_cp(dep_frame), B)) {
      choiceptr top_cp_on_branch;
      top_cp_on_branch = DepFr_cons_cp(dep_frame);
      while (YOUNGER_CP(top_cp_on_branch, B)) {
        top_cp_on_branch = top_cp_on_branch->cp_b;
      }
      DepFr_top_or_fr(dep_frame) = top_cp_on_branch->cp_or_fr;
      dep_frame = DepFr_next(dep_frame);
    }
    /* update worker Q top dependency frame */
    REMOTE_top_dep_fr(worker_q) = dep_frame;
    /* update dependency frames in the recently shared branches */
    while (YOUNGER_CP(DepFr_cons_cp(dep_frame), Get_LOCAL_top_cp_on_stack())) {
      DepFr_top_or_fr(dep_frame) = DepFr_cons_cp(dep_frame)->cp_or_fr;
      dep_frame = DepFr_next(dep_frame);
    }
#endif /* TABLING */

#ifdef OPTYAP_ERRORS
    { dep_fr_ptr aux_dep_fr = LOCAL_top_dep_fr;
      while(aux_dep_fr != GLOBAL_root_dep_fr) {
        choiceptr top_cp_on_branch;
        top_cp_on_branch = DepFr_cons_cp(aux_dep_fr);
        while (YOUNGER_CP(top_cp_on_branch, B)) {
          top_cp_on_branch = top_cp_on_branch->cp_b;
        }
        if (top_cp_on_branch->cp_or_fr != DepFr_top_or_fr(aux_dep_fr))
          OPTYAP_ERROR_MESSAGE("Error on DepFr_top_or_fr (share_private_nodes)");
        aux_dep_fr = DepFr_next(aux_dep_fr);
      }
    }
#endif /* OPTYAP_ERRORS */

    /* update top shared nodes */
#ifdef TABLING
    Set_REMOTE_top_cp_on_stack(worker_q,B);
    Set_LOCAL_top_cp_on_stack( B );
#endif /* TABLING */
    Set_REMOTE_top_cp(worker_q,B);
    Set_LOCAL_top_cp(B);
    REMOTE_top_or_fr(worker_q) = LOCAL_top_or_fr = Get_LOCAL_top_cp()->cp_or_fr;
  }

#ifdef TABLING_INNER_CUTS
  /* update worker Q pruning scope */
  if (LOCAL_pruning_scope && EQUAL_OR_YOUNGER_CP(Get_LOCAL_top_cp(), LOCAL_pruning_scope)) {
    REMOTE_pruning_scope(worker_q) = LOCAL_pruning_scope;
    PUT_IN_PRUNING(worker_q);
  } else {
    PUT_OUT_PRUNING(worker_q);
    REMOTE_pruning_scope(worker_q) = NULL;
  }
#endif /* TABLING_INNER_CUTS */

  /* update worker Q prune request */
  if (LOCAL_prune_request) {
    CUT_send_prune_request(worker_q, LOCAL_prune_request);
  }

  /* update load and return */
  REMOTE_load(worker_q) = LOCAL_load = 0;
  return;
}
#endif /* ENV_COPY */
