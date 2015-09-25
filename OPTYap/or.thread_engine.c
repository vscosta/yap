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
#ifdef YAPOR_THREADS
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


#define INCREMENTAL_COPYING 1
#define COMPUTE_SEGMENTS_TO_COPY_TO(Q)                                   \
        REMOTE_start_global_copy(Q) = (CELL) (REMOTE_top_cp(Q)->cp_h);   \
        REMOTE_end_global_copy(Q)   = (CELL) (B->cp_h);                  \
        REMOTE_start_local_copy(Q)  = (CELL) (B);                        \
        REMOTE_end_local_copy(Q)    = (CELL) (REMOTE_top_cp(Q));         \
        REMOTE_start_trail_copy(Q)  = (CELL) (REMOTE_top_cp(Q)->cp_tr);  \
        REMOTE_end_trail_copy(Q)    = (CELL) (TR)

/* ------------------------------------- **
**      Local functions declaration      **
** ------------------------------------- */

static void share_private_nodes(int worker_q);


/* -------------------------- **
**      Global functions      **
** -------------------------- */

void make_root_choice_point(void) {
  CACHE_REGS
  if (worker_id == 0) {
    SetOrFr_node(GLOBAL_root_or_fr, B);
    Set_LOCAL_top_cp(B);
    Set_GLOBAL_root_cp(B);
  } else {
    choiceptr imageB;

    Set_LOCAL_top_cp(Get_GLOBAL_root_cp());
    B = Get_GLOBAL_root_cp();
    /*
      this is tricky, we need to get the B from some other stack
      and convert back to our own stack;
     */
    LOCAL_OldLCL0 = LCL0;
    LCL0 = REMOTE_ThreadHandle(0).current_yaam_regs->LCL0_;
    imageB = Get_GLOBAL_root_cp();
    /* we know B */
    B->cp_tr = TR = 
      (tr_fr_ptr)((CELL)(imageB->cp_tr)+((CELL)LOCAL_OldLCL0-(CELL)LCL0));
    LCL0 = LOCAL_OldLCL0;
  }
  B->cp_h = H0;
  B->cp_ap = GETWORK;
  B->cp_or_fr = GLOBAL_root_or_fr;
  LOCAL_top_or_fr = GLOBAL_root_or_fr;
  LOCAL_load = 0;
  Set_LOCAL_prune_request(NULL);
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
  CACHE_REGS
  B = Get_LOCAL_top_cp()->cp_b;
#ifdef TABLING
  Set_LOCAL_top_cp_on_stack((choiceptr) LOCAL_LocalBase);
#endif /* TABLING */
  Set_GLOBAL_root_cp((choiceptr) LOCAL_LocalBase);
  Set_LOCAL_top_cp((choiceptr) LOCAL_LocalBase);
  SetOrFr_node(GLOBAL_root_or_fr, (choiceptr) LOCAL_LocalBase);
  return;
}


int p_share_work() {
  CACHE_REGS
  int worker_q = LOCAL_share_request;

  if (! BITMAP_member(OrFr_members(REMOTE_top_or_fr(worker_q)), worker_id) ||
      B == REMOTE_top_cp(worker_q) ||
      (LOCAL_load <= GLOBAL_delayed_release_load && OrFr_nearest_livenode(LOCAL_top_or_fr) == NULL)) {
    /* refuse sharing request */
    REMOTE_reply_signal(LOCAL_share_request) = no_sharing;
    LOCAL_share_request = MAX_WORKERS;
    PUT_OUT_REQUESTABLE(worker_id);
    return TRUE;
  }
  /* sharing request accepted */
  COMPUTE_SEGMENTS_TO_COPY_TO(worker_q);
  REMOTE_q_fase_signal(worker_q) = Q_idle;
  REMOTE_p_fase_signal(worker_q) = P_idle;
#ifndef TABLING
  /* wait for incomplete installations */
  while (LOCAL_reply_signal != worker_ready);
#endif /* TABLING */
  LOCAL_reply_signal = sharing;
  REMOTE_reply_signal(worker_q) = sharing;
  share_private_nodes(worker_q);
  if(Get_LOCAL_prune_request())
    CUT_send_prune_request(worker_q, Get_LOCAL_prune_request()); 
  REMOTE_reply_signal(worker_q) = nodes_shared;
  while (LOCAL_reply_signal == sharing);
  while (REMOTE_reply_signal(worker_q) != worker_ready);
  LOCAL_share_request = MAX_WORKERS;
  PUT_IN_REQUESTABLE(worker_id);

  return TRUE;
}

int q_share_work(int worker_p) {
  CACHE_REGS
  register tr_fr_ptr aux_tr;
  register CELL aux_cell;

  LOCK_OR_FRAME(LOCAL_top_or_fr);
  if (Get_REMOTE_prune_request(worker_p)) {
    /* worker p with prune request */
    UNLOCK_OR_FRAME(LOCAL_top_or_fr);
    return FALSE;
  }
  YAPOR_ERROR_CHECKING(q_share_work, Get_OrFr_pend_prune_cp(LOCAL_top_or_fr) && BRANCH_LTT(worker_p, OrFr_depth(LOCAL_top_or_fr)) < OrFr_pend_prune_ltt(LOCAL_top_or_fr));
  /* there is no pending prune with worker p at right --> safe move to worker p branch */
  CUT_reset_prune_request();
  if(Get_LOCAL_prune_request()){
   UNLOCK_OR_FRAME(LOCAL_top_or_fr);
   return FALSE;
  }
  BRANCH(worker_id, OrFr_depth(LOCAL_top_or_fr)) = BRANCH(worker_p, OrFr_depth(LOCAL_top_or_fr));
  UNLOCK_OR_FRAME(LOCAL_top_or_fr);

 /* unbind variables */
  aux_tr = Get_LOCAL_top_cp()->cp_tr;
  TABLING_ERROR_CHECKING(q_share_work, TR < aux_tr);
  while (aux_tr != TR) {
    aux_cell = TrailTerm(--TR);
    /* check for global or local variables */
    if (IsVarTerm(aux_cell)) {
      RESET_VARIABLE(aux_cell);
#ifdef TABLING
    } else if (IsPairTerm(aux_cell)) {
      aux_cell = (CELL) RepPair(aux_cell);
      if (IN_BETWEEN(LOCAL_TrailBase, aux_cell, LOCAL_TrailTop)) {
        /* avoid frozen segments */
        TR = (tr_fr_ptr) aux_cell;
        TABLING_ERROR_CHECKING(q_share_work, TR > (tr_fr_ptr) LOCAL_TrailTop);
        TABLING_ERROR_CHECKING(q_share_work, TR < aux_tr);
      }
#endif /* TABLING */
#ifdef MULTI_ASSIGNMENT_VARIABLES
    } else if (IsApplTerm(aux_cell)) {
      CELL *aux_ptr = RepAppl(aux_cell);
      Term aux_val = TrailTerm(--aux_tr);
      *aux_ptr = aux_val;
#endif /* MULTI_ASSIGNMENT_VARIABLES */
    }
  }
  OPTYAP_ERROR_CHECKING(q_share_work, Get_LOCAL_top_cp() != Get_LOCAL_top_cp_on_stack());
  OPTYAP_ERROR_CHECKING(q_share_work, YOUNGER_CP(B_FZ, Get_LOCAL_top_cp()));
  YAPOR_ERROR_CHECKING(q_share_work, LOCAL_reply_signal != worker_ready);

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

#if INCREMENTAL_COPYING
  Yap_CopyThreadStacks(worker_id, worker_p, TRUE);
#else
  Yap_CopyThreadStacks(worker_id, worker_p, FALSE);
#endif

  PUT_OUT_ROOT_NODE(worker_id);
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
  CACHE_REGS
  choiceptr sharing_node = B;


#ifdef DEBUG_OPTYAP
  OPTYAP_ERROR_CHECKING(share_private_nodes, YOUNGER_CP(Get_LOCAL_top_cp(), Get_LOCAL_top_cp_on_stack()));
  { choiceptr aux_cp = B;
    while (aux_cp != Get_LOCAL_top_cp()) {
      OPTYAP_ERROR_CHECKING(share_private_nodes, YOUNGER_CP(Get_LOCAL_top_cp(), aux_cp));
      OPTYAP_ERROR_CHECKING(share_private_nodes, EQUAL_OR_YOUNGER_CP(Get_LOCAL_top_cp_on_stack(), aux_cp));
      aux_cp = aux_cp->cp_b;
    }
  }
#endif /* DEBUG_OPTYAP */

#ifdef TABLING
  /* check if the branch is already shared */
  if (EQUAL_OR_YOUNGER_CP(Get_LOCAL_top_cp_on_stack(), sharing_node)) {
    or_fr_ptr or_frame;
    sg_fr_ptr sg_frame;
    dep_fr_ptr dep_frame;

#ifdef DEBUG_OPTYAP
    { or_fr_ptr aux_or_fr;
      aux_or_fr = LOCAL_top_or_fr;
      while (aux_or_fr != REMOTE_top_or_fr(worker_q)) {
	OPTYAP_ERROR_CHECKING(share_private_nodes, YOUNGER_CP(GetOrFr_node(REMOTE_top_or_fr(worker_q)), GetOrFr_node(aux_or_fr)));
        aux_or_fr = OrFr_next_on_stack(aux_or_fr);
      }
    }
#endif /* DEBUG_OPTYAP */

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
    CELL *stack, *stack_limit;

    /* find top dependency frame above current choice point */
    dep_frame = LOCAL_top_dep_fr;
    while (EQUAL_OR_YOUNGER_CP(DepFr_cons_cp(dep_frame), sharing_node)) {
      dep_frame = DepFr_next(dep_frame);
    }
    /* initialize tabling auxiliary variables */ 
    consumer_cp = DepFr_cons_cp(dep_frame);
    next_node_on_branch = NULL;
    stack_limit = (CELL *)TR;
    stack = (CELL *)LOCAL_TrailTop;
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

#ifdef DEBUG_OPTYAP
      if (next_node_on_branch) {
        choiceptr aux_cp = B;
        while (aux_cp != next_node_on_branch) {
	  OPTYAP_ERROR_CHECKING(share_private_nodes, sharing_node == aux_cp);
	  OPTYAP_ERROR_CHECKING(share_private_nodes, YOUNGER_CP(next_node_on_branch, aux_cp));
          aux_cp = aux_cp->cp_b;
        }
      } else {
        choiceptr aux_cp = B;
        while (aux_cp != sharing_node) {
	  OPTYAP_ERROR_CHECKING(share_private_nodes, YOUNGER_CP(sharing_node, aux_cp));
          aux_cp = aux_cp->cp_b;
        }
      }
#endif /* DEBUG_OPTYAP */

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
      Set_OrFr_pend_prune_cp(or_frame, NULL);
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

      YAPOR_ERROR_CHECKING(share_private_nodes, sharing_node->cp_ap == GETWORK || sharing_node->cp_ap == GETWORK_SEQ);
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
        STACK_CHECK_EXPAND(stack, stack_limit);
        STACK_PUSH_UP(sharing_node, stack);  /* vsc: STACK_PUSH -> STACK_PUSH_UP? */
        STACK_CHECK_EXPAND(stack, stack_limit);
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
      OPTYAP_ERROR_CHECKING(share_private_nodes, next_node_on_branch && YOUNGER_CP(next_node_on_branch, sharing_node));
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
    while (STACK_NOT_EMPTY(stack, (CELL *)LOCAL_TrailTop)) {
      next_node_on_branch = (choiceptr) STACK_POP_DOWN(stack);
      or_frame = (or_fr_ptr) STACK_POP_DOWN(stack);
      OrFr_nearest_livenode(or_frame) = OrFr_next(or_frame) = next_node_on_branch->cp_or_fr;
    }
#endif /* TABLING */

    /* update depth */
    if (depth >= MAX_BRANCH_DEPTH)
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "maximum depth exceded (share_private_nodes)");
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

    YAPOR_ERROR_CHECKING(share_private_nodes, depth != OrFr_depth(LOCAL_top_or_fr));

#ifdef DEBUG_OPTYAP
    { or_fr_ptr aux_or_fr = B->cp_or_fr;
      choiceptr aux_cp;
      while (aux_or_fr != Get_LOCAL_top_cp_on_stack()->cp_or_fr) {
        aux_cp = GetOrFr_node(aux_or_fr);
	OPTYAP_ERROR_CHECKING(share_private_nodes, OrFr_next(aux_or_fr) != aux_cp->cp_b->cp_or_fr);
	OPTYAP_ERROR_CHECKING(share_private_nodes, OrFr_nearest_livenode(aux_or_fr) != aux_cp->cp_b->cp_or_fr);
        aux_or_fr = OrFr_next_on_stack(aux_or_fr);
      }
      aux_or_fr = B->cp_or_fr;
      while (aux_or_fr != Get_LOCAL_top_cp_on_stack()->cp_or_fr) {
        or_fr_ptr nearest_leftnode = OrFr_nearest_leftnode(aux_or_fr);
        aux_cp = GetOrFr_node(aux_or_fr);
        while (GetOrFr_node(nearest_leftnode) != aux_cp) {
	  OPTYAP_ERROR_CHECKING(share_private_nodes, YOUNGER_CP(GetOrFr_node(nearest_leftnode), aux_cp));
          aux_cp = aux_cp->cp_b;
        }
        aux_or_fr = OrFr_next_on_stack(aux_or_fr);
      }
    }
#endif /* DEBUG_OPTYAP */

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
    
    LOCK_OR_FRAME(REMOTE_top_or_fr(worker_q));
    or_fr_ptr old_top = REMOTE_top_or_fr(worker_q);
    Set_REMOTE_top_cp(worker_q,B);
    Set_LOCAL_top_cp(B);
    REMOTE_top_or_fr(worker_q) = LOCAL_top_or_fr = Get_LOCAL_top_cp()->cp_or_fr;
    UNLOCK_OR_FRAME(old_top);    

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

#ifdef DEBUG_OPTYAP
    { dep_fr_ptr aux_dep_fr = LOCAL_top_dep_fr;
      while(aux_dep_fr != GLOBAL_root_dep_fr) {
        choiceptr top_cp_on_branch;
        top_cp_on_branch = DepFr_cons_cp(aux_dep_fr);
        while (YOUNGER_CP(top_cp_on_branch, B)) {
          top_cp_on_branch = top_cp_on_branch->cp_b;
        }
	OPTYAP_ERROR_CHECKING(share_private_nodes, top_cp_on_branch->cp_or_fr != DepFr_top_or_fr(aux_dep_fr));
        aux_dep_fr = DepFr_next(aux_dep_fr);
      }
    }
#endif /* DEBUG_OPTYAP */

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
  if (Get_LOCAL_prune_request()) {
    CUT_send_prune_request(worker_q, Get_LOCAL_prune_request());
  }

  /* update load and return */
  REMOTE_load(worker_q) = LOCAL_load = 0;
  return;
}
#endif /* YAPOR_THREADS */
