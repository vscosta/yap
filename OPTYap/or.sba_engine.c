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
#ifdef YAPOR_SBA
#include <stdio.h>
#include "Yatom.h"
#include "YapHeap.h"
#include "or.macros.h"
#include "opt.mavar.h"



/* ------------------------------------- **
**      Local functions declaration      **
** ------------------------------------- */

static void share_private_nodes(int worker_q);

static void
reset_trail(tr_fr_ptr tr_top, tr_fr_ptr trp)
{
  register CELL aux_cell;

  /* unbinding variables */
  while (tr_top != trp) {
    aux_cell = TrailTerm(--trp);
    /* check for global or local variables */
    if (IsVarTerm(aux_cell)) {
      /* clean up the trail when we backtrack */
      /* shouldn't this test always succeed? */
      if (Unsigned((Int)(aux_cell)-(Int)(H_FZ)) >
	  Unsigned((Int)(B_FZ)-(Int)(H_FZ))) {
	RESET_VARIABLE(STACK_TO_SBA(aux_cell));
      } else {
	RESET_VARIABLE(aux_cell);
      }
    }
    else if (IsPairTerm(aux_cell)) {
      /* avoid frozen segments */
      if ((ADDR) RepPair(aux_cell) > HeapTop) {
	trp = (tr_fr_ptr) RepPair(aux_cell);
      }
#ifdef MULTI_ASSIGNMENT_VARIABLES
    } else {
      CELL *aux_ptr = RepAppl(aux_cell);
      trp--;
      if (Unsigned((Int)(aux_ptr)-(Int)(H_FZ)) >
	  Unsigned((Int)(B_FZ)-(Int)(H_FZ))) {
	*STACK_TO_SBA(aux_ptr) = TrailTerm(trp);
      } else {
	*aux_ptr = TrailTerm(trp);
      }
#endif /* MULTI_ASSIGNMENT_VARIABLES */
    }
  }
}


/* ---------------------- **
**      Local macros      **
** ---------------------- */

#define COMPUTE_SEGMENTS_TO_COPY_TO(Q)                              \
        REMOTE_end_local_copy(Q)      = (CELL) (REMOTE_top_cp(Q));  \
        REMOTE_start_local_copy(Q)    = (CELL) (B)



/* -------------------------- **
**      Global functions      **
** -------------------------- */

void make_root_choice_point(void) {
  if (worker_id == 0) {
    LOCAL_top_cp = GLOBAL_root_cp = OrFr_node(GLOBAL_root_or_fr) = B;
    B->cp_h = H0;
    B->cp_ap = GETWORK;
    B->cp_or_fr = GLOBAL_root_or_fr;
  } else {
    B = LOCAL_top_cp = GLOBAL_root_cp;
    TR = B->cp_tr;
  }
  LOCAL_top_or_fr = GLOBAL_root_or_fr;
  LOCAL_load = 0;
  LOCAL_prune_request = NULL;
  BRANCH(worker_id, 0) = 0;
  H_FZ = (CELL *) LOCAL_GlobalBase;
  B_FZ = (choiceptr) LOCAL_LocalBase;
  TR_FZ = (tr_fr_ptr) LOCAL_TrailBase;
}


void free_root_choice_point(void) {
  reset_trail(LOCAL_top_cp->cp_tr, TR);
  TR = LOCAL_top_cp->cp_tr;
  B = LOCAL_top_cp->cp_b;
  LOCAL_top_cp = (choiceptr) LOCAL_LocalBase;
  H_FZ = (CELL *) LOCAL_GlobalBase;
  B_FZ = (choiceptr) LOCAL_LocalBase;
  TR_FZ = (tr_fr_ptr) LOCAL_TrailBase;
}


void p_share_work(void) {
  int worker_q = LOCAL_share_request;

  if (! BITMAP_member(OrFr_members(REMOTE_top_or_fr(worker_q)), worker_id) ||
      B == REMOTE_top_cp(worker_q) ||
      (LOCAL_load <= GLOBAL_delayed_release_load && OrFr_nearest_livenode(LOCAL_top_or_fr) == NULL)) {
    /* refuse sharing request */
    REMOTE_reply_signal(LOCAL_share_request) = no_sharing;
    LOCAL_share_request = MAX_WORKERS;
    PUT_OUT_REQUESTABLE(worker_id);
    return;
  }
  /* sharing request accepted */
  /* LOCAL_reply_signal = sharing; */
  COMPUTE_SEGMENTS_TO_COPY_TO(worker_q);
  share_private_nodes(worker_q);
  REMOTE_reply_signal(worker_q) = sharing;
  /* REMOTE_reply_signal(worker_q) = nodes_shared; */
  /* while (LOCAL_reply_signal == sharing); */
  LOCAL_share_request = MAX_WORKERS;
  PUT_IN_REQUESTABLE(worker_id);

  return;
}


int q_share_work(int worker_p) {
  register tr_fr_ptr aux_tr;
  register CELL aux_cell;

  LOCK_OR_FRAME(LOCAL_top_or_fr);
  if (Get_REMOTE_prune_request(worker_p)) {
    /* worker p with prune request */
    UNLOCK_OR_FRAME(LOCAL_top_or_fr);
    return FALSE;
  }
  YAPOR_ERROR_CHECKING(q_share_work, OrFr_pend_prune_cp(LOCAL_top_or_fr) && BRANCH_LTT(worker_p, OrFr_depth(LOCAL_top_or_fr)) < OrFr_pend_prune_ltt(LOCAL_top_or_fr));
  /* there is no pending prune with worker p at right --> safe move to worker p branch */
  BRANCH(worker_id, OrFr_depth(LOCAL_top_or_fr)) = BRANCH(worker_p, OrFr_depth(LOCAL_top_or_fr));
  LOCAL_prune_request = NULL;
  UNLOCK_OR_FRAME(LOCAL_top_or_fr);

  reset_trail(LOCAL_top_cp->cp_tr, TR);
  TR = LOCAL_top_cp->cp_tr;
 
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

  /* install fase --> TR and LOCAL_top_cp->cp_tr are equal */
  TR = ((choiceptr)LOCAL_end_local_copy)->cp_tr;
  aux_tr = ((choiceptr) LOCAL_start_local_copy)->cp_tr;
  NEW_MAHASH((ma_h_inner_struct *)HR);
  while (TR != aux_tr) {
    aux_cell = TrailTerm(--aux_tr);
    if (IsVarTerm(aux_cell)) {
      CELL *ptr = STACK_TO_SBA(aux_cell);
      *ptr = TrailVal(aux_tr);
    } else if ((ADDR) RepPair(aux_cell) >= HeapTop) {
      /* avoid frozen segments */
      aux_tr = (tr_fr_ptr) RepPair(aux_cell);
#ifdef MULTI_ASSIGNMENT_VARIABLES
    } else if (IsApplTerm(aux_cell)) {
      CELL *cell_ptr = RepAppl(aux_cell);
      if (!lookup_ma_var(cell_ptr)) {
	/* first time we found the variable, let's put the new value */
	CELL *ptr = STACK_TO_SBA(cell_ptr);
	*ptr = TrailVal(aux_tr);
      }
      /* skip the old value */
      aux_tr--;
    }
#endif /* MULTI_ASSIGNMENT_VARIABLES */
  }

  /* update registers and return */
  /* REMOTE_reply_signal(worker_p) = worker_ready; */
  LOCAL_reply_signal = worker_ready;
  PUT_IN_REQUESTABLE(worker_id);
  TR = LOCAL_top_cp->cp_tr;
  return TRUE;
}



/* ------------------------- **
**      Local functions      **
** ------------------------- */

static
void share_private_nodes(int worker_q) {
  int depth;
  choiceptr AuxB;
  or_fr_ptr or_frame, previous_or_frame;

  /* initialize auxiliary variables */
  AuxB = B;
  previous_or_frame = NULL;
  depth = OrFr_depth(LOCAL_top_or_fr);
  /* sharing loop */
  while (AuxB != LOCAL_top_cp) {
    depth++;
    ALLOC_OR_FRAME(or_frame);
    INIT_LOCK(OrFr_lock(or_frame));
    OrFr_node(or_frame) = AuxB;
    OrFr_alternative(or_frame) = AuxB->cp_ap;
    OrFr_pend_prune_cp(or_frame) = NULL;
    OrFr_nearest_leftnode(or_frame) = LOCAL_top_or_fr;
    OrFr_qg_solutions(or_frame) = NULL;
    BITMAP_clear(OrFr_members(or_frame));
    BITMAP_insert(OrFr_members(or_frame), worker_id);
    BITMAP_insert(OrFr_members(or_frame), worker_q);
    if (AuxB->cp_ap && YAMOP_SEQ(AuxB->cp_ap)) {
      AuxB->cp_ap = GETWORK_SEQ;
    } else {
      AuxB->cp_ap = GETWORK;
    }
    AuxB->cp_or_fr = or_frame;
    AuxB = AuxB->cp_b;
    if (previous_or_frame) {
      OrFr_nearest_livenode(previous_or_frame) = OrFr_next(previous_or_frame) = or_frame;
    }
    previous_or_frame = or_frame;
  }
  /* initialize last or-frame pointer */
  or_frame = AuxB->cp_or_fr;
  if (previous_or_frame) {
    OrFr_nearest_livenode(previous_or_frame) = OrFr_next(previous_or_frame) = or_frame;
  }
  /* update depth */
  if (depth >= MAX_BRANCH_DEPTH)
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "maximum depth exceded (share_private_nodes)");
  or_frame = B->cp_or_fr;

  while (or_frame != LOCAL_top_or_fr) {
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
  /* update old shared nodes */
  while (or_frame != REMOTE_top_or_fr(worker_q)) {
    LOCK_OR_FRAME(or_frame);
    BRANCH(worker_q, OrFr_depth(or_frame)) = BRANCH(worker_id, OrFr_depth(or_frame));
    BITMAP_insert(OrFr_members(or_frame), worker_q);
    UNLOCK_OR_FRAME(or_frame);
    or_frame = OrFr_next_on_stack(or_frame);
  }
  /* move conditional bindings to BA */
  {
    tr_fr_ptr top, tr_ptr;
    top = LOCAL_top_cp->cp_tr;
    tr_ptr = TR;
    while (tr_ptr != top) {
      CELL aux_cell = TrailTerm(--tr_ptr);
      if (IsVarTerm(aux_cell) && 
          ((CELL *)aux_cell < B->cp_h || (choiceptr)aux_cell > B) &&
          !((CELL *)aux_cell < H_FZ || (choiceptr)aux_cell > B_FZ)) {
	CELL *ptr = STACK_TO_SBA(aux_cell);
	*ptr = TrailVal(tr_ptr);
        *(CELL *)aux_cell = (CELL)ptr;
      } else if (IsPairTerm(aux_cell) && (ADDR) RepPair(aux_cell) > HeapTop) {
        /* avoid frozen segments */
        aux_cell = (CELL) RepPair(aux_cell);
        tr_ptr = (tr_fr_ptr) aux_cell;
#ifdef MULTI_ASSIGNMENT_VARIABLES
     } else {
       CELL *cell_ptr = RepAppl(aux_cell);
       /* first do as a for a standard cell */
       if ((cell_ptr < B->cp_h || cell_ptr > (CELL *)B) && !(cell_ptr < H_FZ || (choiceptr)cell_ptr > B_FZ)) {
	 CELL *ptr = STACK_TO_SBA(cell_ptr);
	 /* we may have several bindings in the trail */
	 if ((CELL)ptr != *cell_ptr) {
	   *ptr = TrailVal(tr_ptr);
	   *cell_ptr = (CELL)ptr;
	 }
       }
       /* but we also need to skip the old value */
       tr_ptr--;
#endif /* MULTI_ASSIGNMENT_VARIABLES */
     }
    }
  }
  /* update frozen registers */
  B_FZ  = B;
  H_FZ  = B->cp_h;
  TR_FZ = B->cp_tr;
  /* update top shared nodes */
  REMOTE_top_cp(worker_q) = LOCAL_top_cp = B;
  REMOTE_top_or_fr(worker_q) = LOCAL_top_or_fr = LOCAL_top_cp->cp_or_fr;
  /* update prune request */
  if (LOCAL_prune_request) {
    CUT_send_prune_request(worker_q, LOCAL_prune_request);
  }
  /* update load and return */
  REMOTE_load(worker_q) = LOCAL_load = 0;
}
#endif /* SBA */
