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
#ifdef YAPOR_COW
#include <sys/types.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
#include "Yatom.h"
#include "YapHeap.h"
#include "or.macros.h"




/* ------------------------------------- **
**      Local functions declaration      **
** ------------------------------------- */

static void share_private_nodes(int worker_q);



/* ----------------------- **
**      Local inlines      **
** ----------------------- */

static inline void PUT_BUSY(int);

static inline
void PUT_BUSY(int worker_num) {
  LOCK(GLOBAL_locks_bm_idle_workers);
  BITMAP_delete(GLOBAL_bm_idle_workers, worker_num);
  UNLOCK(GLOBAL_locks_bm_idle_workers);
  return;
}



/* -------------------------- **
**      Global functions      **
** -------------------------- */

void make_root_choice_point(void) {
  if (worker_id == 0) {
    LOCAL_top_cp = GLOBAL_root_cp = OrFr_node(GLOBAL_root_or_fr) = B;
  } else {
    B = LOCAL_top_cp = GLOBAL_root_cp;
    B->cp_tr = TR = ((choiceptr) (worker_offset(0) + (CELL)(B)))->cp_tr;
  }
  B->cp_h = H0;
  B->cp_ap = GETWORK;
  B->cp_or_fr = GLOBAL_root_or_fr;
  LOCAL_top_or_fr = GLOBAL_root_or_fr;
  LOCAL_load = 0;
  LOCAL_prune_request = NULL;
  BRANCH(worker_id, 0) = 0;
  return;
}


void free_root_choice_point(void) {
  B = LOCAL_top_cp->cp_b;
  LOCAL_top_cp = (choiceptr) LOCAL_LocalBase;
  return;
}


int p_share_work(void) {
  int worker_q = LOCAL_share_request;
  int son;

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
  REMOTE_reply_signal(worker_q) = sharing;
  share_private_nodes(worker_q);
  if ((son = fork()) == 0) {
    worker_id = worker_q;  /* child becomes requesting worker */
    LOCAL = REMOTE(worker_id);
    LOCAL_reply_signal = worker_ready;
    PUT_IN_REQUESTABLE(worker_id);
    PUT_BUSY(worker_id);

    return FALSE;
  } else {
    GLOBAL_worker_pid(worker_q) = son;
    LOCAL_share_request = MAX_WORKERS;
    PUT_IN_REQUESTABLE(worker_id);

    return TRUE;
  }
}


int q_share_work(int worker_p) {
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
  /* exit this process */
  exit(0);
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
    SetOrFr_node(or_frame, AuxB);
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
  /* update top shared nodes */
  REMOTE_top_cp(worker_q) = LOCAL_top_cp = B;
  REMOTE_top_or_fr(worker_q) = LOCAL_top_or_fr = LOCAL_top_cp->cp_or_fr;
  /* update prune request */
  if (LOCAL_prune_request) {
    CUT_send_prune_request(worker_q, LOCAL_prune_request);
  }
  /* update load and return */
  REMOTE_load(worker_q) = LOCAL_load = 0;
  return;
}
#endif /* YAPOR_COW */
