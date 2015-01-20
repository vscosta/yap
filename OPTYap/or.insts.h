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

/* -------------------------------- **
**      Scheduler instructions      **
** -------------------------------- */

  PBOp(getwork_first_time,e)
    /* wait for a new parallel goal */
    while (BITMAP_same(GLOBAL_bm_present_workers,GLOBAL_bm_finished_workers));
    make_root_choice_point();  
    SCHEDULER_GET_WORK();
  shared_end:
    PUT_IN_FINISHED(worker_id);
    /* wait until everyone else is finished! */
    while (! BITMAP_same(GLOBAL_bm_present_workers,GLOBAL_bm_finished_workers));
    PUT_OUT_ROOT_NODE(worker_id);
    if (worker_id == 0) {
      finish_yapor();
      free_root_choice_point();
      /* wait until no one is executing */
      while (! BITMAP_empty(GLOBAL_bm_root_cp_workers));
      goto fail;
    } else {
      PREG = GETWORK_FIRST_TIME;
      PREFETCH_OP(PREG);
      GONext();
    }
  ENDPBOp();




  PBOp(getwork,Otapl)
#ifdef TABLING
    if (DepFr_leader_cp(LOCAL_top_dep_fr) == Get_LOCAL_top_cp()) {
      /* the current top node is a leader node with consumer nodes below */
      if (DepFr_leader_dep_is_on_stack(LOCAL_top_dep_fr)) {
        /*    the frozen branch depends on the current top node     **
	** this means that the current top node is a generator node */
        LOCK_OR_FRAME(LOCAL_top_or_fr);
        if (OrFr_alternative(LOCAL_top_or_fr) == NULL ||
           (OrFr_alternative(LOCAL_top_or_fr) == ANSWER_RESOLUTION && B_FZ != Get_LOCAL_top_cp())) {
          /*                 there are no unexploited alternatives                 **
          ** (NULL if batched scheduling OR ANSWER_RESOLUTION if local scheduling) */
          UNLOCK_OR_FRAME(LOCAL_top_or_fr);
	  goto completion;
        } else {
          /*                     there are unexploited alternatives                     **
	  ** we should exploit all the available alternatives before execute completion */
          PREG = OrFr_alternative(LOCAL_top_or_fr);
          PREFETCH_OP(PREG);
          GONext();
        }
/* ricroc - obsolete
#ifdef  batched scheduling
        if (OrFr_alternative(LOCAL_top_or_fr) != NULL) {
#else   local scheduling
        if (OrFr_alternative(LOCAL_top_or_fr) != ANSWER_RESOLUTION || B_FZ == Get_LOCAL_top_cp()) {
#endif
          PREG = OrFr_alternative(LOCAL_top_or_fr);
          PREFETCH_OP(PREG);
          GONext();
        }
        UNLOCK_OR_FRAME(LOCAL_top_or_fr);
*/
      }
      goto completion;
    }
#endif /* TABLING */
    LOCK_OR_FRAME(LOCAL_top_or_fr);
    if (OrFr_alternative(LOCAL_top_or_fr)) {
      PREG = OrFr_alternative(LOCAL_top_or_fr);
      PREFETCH_OP(PREG);
      GONext();
    } else {
      UNLOCK_OR_FRAME(LOCAL_top_or_fr);
      SCHEDULER_GET_WORK();
    }
  ENDPBOp();



  /* The idea is to check whether we are the last worker in the node.
     If we are, we can go ahead, otherwise we should call the scheduler. */
  PBOp(getwork_seq,Otapl)
    LOCK_OR_FRAME(LOCAL_top_or_fr);
    if (OrFr_alternative(LOCAL_top_or_fr) &&
        BITMAP_alone(OrFr_members(LOCAL_top_or_fr), worker_id)) {
      PREG = OrFr_alternative(LOCAL_top_or_fr);
      PREFETCH_OP(PREG);
      GONext();
    } else {
      UNLOCK_OR_FRAME(LOCAL_top_or_fr);
      SCHEDULER_GET_WORK();
    }
  ENDPBOp();



  PBOp(sync,Otapl)
    CUT_wait_leftmost();
    PREG = NEXTOP(PREG,Otapl);
    PREFETCH_OP(PREG);
    GONext();
  ENDPBOp();
