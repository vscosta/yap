/* ------------------------------------------------ **
**      Tabling instructions: auxiliary macros      **
** ------------------------------------------------ */

#ifdef TABLING_BATCHED_SCHEDULING
#define GCP_SCHEDULING_STRATEGY_INIT(GCP, SG_FR)                    \
        GCP->gcp_sg_fr = SG_FR
#else /* TABLING_LOCAL_SCHEDULING */
#define GCP_SCHEDULING_STRATEGY_INIT(GCP, SG_FR)                    \
        { register dep_fr_ptr new_dep_fr;                           \
          /* adjust freeze registers */                             \
          H_FZ = H;                                                 \
          B_FZ = NORM_CP(GCP);                                      \
          TR_FZ = TR;                                               \
          /* store dependency frame */                              \
          new_dependency_frame(new_dep_fr, TRUE, LOCAL_top_or_fr,   \
                               GCP, GCP, SG_FR, LOCAL_top_dep_fr);  \
          LOCAL_top_dep_fr = new_dep_fr;                            \
        }                                                           \
        GCP->gcp_dep_fr = LOCAL_top_dep_fr
#endif /* TABLING_SCHEDULING */


#define store_generator_node(PTR, ARITY, AP, SG_FR)            \
        { register CELL *pt_args;                              \
          register gen_cp_ptr gcp;                             \
          /* store args */                                     \
          pt_args = XREGS + (ARITY);                           \
	  while (pt_args > XREGS) {                            \
            register CELL aux_arg = pt_args[0];                \
            --PTR;                                             \
            --pt_args;                                         \
            *PTR = aux_arg;                                    \
	  }                                                    \
          /* initialize gcp and adjust subgoal frame field */  \
          gcp = --GEN_CP(PTR);                                 \
          SgFr_gen_cp(SG_FR) = NORM_CP(gcp);                   \
          /* store generator choice point */                   \
          HBREG = H;                                           \
          gcp->gcp_tr = TR;            	                       \
          gcp->gcp_ap = (yamop *)(AP);                         \
          gcp->gcp_h  = H;                                     \
          gcp->gcp_b  = B;                                     \
          gcp->gcp_env = ENV;                                  \
          gcp->gcp_cp = CPREG;                                 \
          GCP_SCHEDULING_STRATEGY_INIT(gcp, SG_FR);            \
          set_cut((CELL *)gcp, B);                             \
          B = NORM_CP(gcp);                                    \
          YAPOR_SET_LOAD(B);                                   \
          SET_BB(B);                                           \
        }


#define restore_generator_node(PTR, ARITY, AP)          \
        { register CELL *pt_args, *x_args;              \
          register gen_cp_ptr gcp = GEN_CP(PTR);        \
          /* restore generator choice point */          \
          H = HBREG = PROTECT_FROZEN_H(NORM_CP(gcp));   \
          CPREG = gcp->gcp_cp;                          \
          ENV = gcp->gcp_env;                           \
          YAPOR_update_alternative(PREG, (yamop *) AP)  \
          gcp->gcp_ap = (yamop *) AP;                   \
          /* restore args */                            \
          pt_args = (CELL *)(gcp + 1) + ARITY;          \
          x_args = XREGS + 1 + ARITY;                   \
          while (x_args > XREGS + 1) {                  \
            register CELL x = pt_args[-1];              \
            --x_args;                                   \
            --pt_args;                                  \
            *x_args = x;                                \
	  }                                             \
        }


#define pop_generator_node(PTR, ARITY)            \
        { register CELL *pt_args, *x_args;        \
          register gen_cp_ptr gcp = GEN_CP(PTR);  \
          /* pop generator choice point */        \
          H = PROTECT_FROZEN_H(NORM_CP(gcp));     \
          CPREG = gcp->gcp_cp;                    \
          ENV = gcp->gcp_env;                     \
          TR = B->cp_tr;                          \
          B = gcp->gcp_b;                         \
          HBREG = PROTECT_FROZEN_H(B);		  \
          /* pop args */                          \
          x_args = XREGS + 1 ;                    \
          pt_args = (CELL *)(gcp + 1);            \
	  while (x_args < XREGS + 1 + ARITY) {    \
            register CELL x = pt_args[0];         \
            pt_args++;                            \
            x_args++;                             \
            x_args[-1] = x;                       \
          }                                       \
          YENV = pt_args;		    	          \
          SET_BB(PROTECT_FROZEN_B(B));            \
        }


#define store_consumer_node(PTR, SG_FR, LEADER_CP, DEP_ON_STACK)           \
        { register cons_cp_ptr ccp;                                        \
          register dep_fr_ptr new_dep_fr;                                  \
	  /* initialize ccp */                                             \
          ccp = --CONS_CP(PTR);                                            \
          /* adjust freeze registers */                                    \
          H_FZ = H;                                                        \
          B_FZ = NORM_CP(ccp);                 	                           \
          TR_FZ = TR;                                                      \
          /* store dependency frame */                                     \
          new_dependency_frame(new_dep_fr, DEP_ON_STACK, LOCAL_top_or_fr,  \
                               LEADER_CP, ccp, SG_FR, LOCAL_top_dep_fr);   \
          LOCAL_top_dep_fr = new_dep_fr;                                   \
          /* store consumer choice point */                                \
          HBREG = H;                                                       \
          ccp->ccp_tr = TR;         	                                   \
          ccp->ccp_ap = ANSWER_RESOLUTION;                                 \
          ccp->ccp_h  = H;                                                 \
          ccp->ccp_b  = B;                                                 \
          ccp->ccp_env= ENV;                                               \
          ccp->ccp_cp = CPREG;                                             \
          ccp->ccp_dep_fr = LOCAL_top_dep_fr;                              \
          set_cut((CELL *)ccp, B);                                         \
          B = NORM_CP(ccp);                                                \
          YAPOR_SET_LOAD(B);                                               \
          SET_BB(B);                                                       \
        }


#ifdef TABLING_BATCHED_SCHEDULING
#define init_substitution_pointer(SUBS_PTR, DEP_FR)  \
        SUBS_PTR = (CELL *) (CONS_CP(B) + 1)
#else /* TABLING_LOCAL_SCHEDULING */
#define init_substitution_pointer(SUBS_PTR, DEP_FR)  \
        SUBS_PTR = (CELL *) (CONS_CP(B) + 1);        \
        if (DepFr_leader_cp(DEP_FR) == B)            \
          SUBS_PTR += SgFr_arity(GEN_CP_SG_FR(B))
#endif /* TABLING_SCHEDULING */


#define consume_answer_and_procceed(DEP_FR, ANSWER)     \
        { CELL *subs_ptr;                               \
          /* restore consumer choice point */           \
          H = HBREG = PROTECT_FROZEN_H(B);              \
          CPREG = B->cp_cp;                             \
          ENV = B->cp_env;                              \
          /* set_cut(YENV, B->cp_b); has no effect */      \
          PREG = (yamop *) CPREG;                       \
          PREFETCH_OP(PREG);                            \
          /* load answer from table to global stack */  \
          init_substitution_pointer(subs_ptr, DEP_FR);  \
          load_answer_trie(ANSWER, subs_ptr);           \
          /* procceed */                                \
          YENV = ENV;                                      \
          GONext();                                     \
        }

#ifdef DEPTH_LIMIT
#define allocate_environment(PTR)  \
        PTR[E_CP] = (CELL) CPREG;  \
        PTR[E_E] = (CELL) ENV;     \
        PTR[E_DEPTH] = (CELL)DEPTH;\
        PTR[E_B] = (CELL) B;       \
        ENV = PTR
#else
#define allocate_environment(PTR)  \
        PTR[E_CP] = (CELL) CPREG;  \
        PTR[E_E] = (CELL) ENV;     \
        PTR[E_B] = (CELL) B;       \
        ENV = PTR
#endif


/* ------------------------------ **
**      Tabling instructions      **
** ------------------------------ */  

#ifdef TABLING_INNER_CUTS
  Op(clause_with_cut, e)
    if (LOCAL_pruning_scope) {
      if (YOUNGER_CP(LOCAL_pruning_scope, B))
        LOCAL_pruning_scope = B;
    } else {
      LOCAL_pruning_scope = B;
      PUT_IN_PRUNING(worker_id);
    }
    PREG = NEXTOP(PREG, e);
    GONext();
  ENDOp();
#endif /* TABLING_INNER_CUTS */



  PBOp(table_try_me_single, ld)
    tab_ent_ptr tab_ent;
    sg_node_ptr sg_node;
    sg_fr_ptr sg_fr;
    CELL *Yaddr;

    Yaddr = YENV;
    check_trail();
    tab_ent = PREG->u.ld.te;
#ifdef TABLE_LOCK_AT_ENTRY_LEVEL
    LOCK(TabEnt_lock(tab_ent));
#endif /* TABLE_LOCK_LEVEL */
    sg_node = subgoal_search(tab_ent, PREG->u.ld.s, &Yaddr);
    YENV = Yaddr;
#if defined(TABLE_LOCK_AT_NODE_LEVEL)
    LOCK(TrNode_lock(sg_node));
#elif defined(TABLE_LOCK_AT_WRITE_LEVEL)
    LOCK_TABLE(sg_node);
#endif /* TABLE_LOCK_LEVEL */
    if (TrNode_sg_fr(sg_node) == NULL) {
      /* new tabled subgoal */
      new_subgoal_frame(sg_fr, sg_node, PREG->u.ld.s, LOCAL_top_sg_fr);
      TrNode_sg_fr(sg_node) = (sg_node_ptr) sg_fr;
#if defined(TABLE_LOCK_AT_ENTRY_LEVEL)
      UNLOCK(TabEnt_lock(tab_ent));
#elif defined(TABLE_LOCK_AT_NODE_LEVEL)
      UNLOCK(TrNode_lock(sg_node));
#elif defined(TABLE_LOCK_AT_WRITE_LEVEL)
      UNLOCK_TABLE(sg_node);
#endif /* TABLE_LOCK_LEVEL */
      LOCAL_top_sg_fr = sg_fr;
      store_generator_node(YENV, PREG->u.ld.s, COMPLETION, sg_fr);
      PREG = NEXTOP(PREG, ld);
      PREFETCH_OP(PREG);
      allocate_environment(YENV);
      GONext();
    } else {
      /* tabled subgoal not new */
#if defined(TABLE_LOCK_AT_ENTRY_LEVEL)
      UNLOCK(TabEnt_lock(tab_ent));
#elif defined(TABLE_LOCK_AT_NODE_LEVEL)
      UNLOCK(TrNode_lock(sg_node));
#elif defined(TABLE_LOCK_AT_WRITE_LEVEL)
      UNLOCK_TABLE(sg_node);
#endif /* TABLE_LOCK_LEVEL */
      sg_fr = (sg_fr_ptr) TrNode_sg_fr(sg_node);
      LOCK(SgFr_lock(sg_fr));
      if (SgFr_state(sg_fr)) {
        /* subgoal completed */
        if (SgFr_state(sg_fr) == complete)
          update_answer_trie(sg_fr);
        UNLOCK(SgFr_lock(sg_fr));
        if (SgFr_first_answer(sg_fr) == NULL) {
          /* no answers --> fail */
          goto fail;
        } else if (SgFr_first_answer(sg_fr) == SgFr_answer_trie(sg_fr)) {
          /* yes answer --> procceed */
          PREG = (yamop *) CPREG;
          PREFETCH_OP(PREG);
          YENV = ENV;
          GONext();
        } else {
          /* answers -> load first answer */
          PREG = (yamop *) TrNode_child(SgFr_answer_trie(sg_fr));
          PREFETCH_OP(PREG);
          *--YENV = 0;  /* vars_arity */
          *--YENV = 0;  /* heap_arity */
          GONext();
        }
      } else {
        /* subgoal not completed */
        choiceptr leader_cp;
        int leader_dep_on_stack;
        find_dependency_node(sg_fr, leader_cp, leader_dep_on_stack);
        UNLOCK(SgFr_lock(sg_fr));
        find_leader_node(leader_cp, leader_dep_on_stack);
        store_consumer_node(YENV, sg_fr, leader_cp, leader_dep_on_stack);
#ifdef OPTYAP_ERRORS
        if (PARALLEL_EXECUTION_MODE) {
          choiceptr aux_cp;
          aux_cp = B;
          while (YOUNGER_CP(aux_cp, LOCAL_top_cp_on_stack))
            aux_cp = aux_cp->cp_b;
          if (aux_cp->cp_or_fr != DepFr_top_or_fr(LOCAL_top_dep_fr))
            OPTYAP_ERROR_MESSAGE("Error on DepFr_top_or_fr (table_try_me_single)");
          aux_cp = B;
          while (YOUNGER_CP(aux_cp, DepFr_leader_cp(LOCAL_top_dep_fr)))
            aux_cp = aux_cp->cp_b;
          if (aux_cp != DepFr_leader_cp(LOCAL_top_dep_fr))
            OPTYAP_ERROR_MESSAGE("Error on DepFr_leader_cp (table_try_me_single)");
        }
#endif /* OPTYAP_ERRORS */
        goto answer_resolution;
      }
    }
  ENDPBOp();



  PBOp(table_try_me, ld)
    tab_ent_ptr tab_ent;
    sg_node_ptr sg_node;
    sg_fr_ptr sg_fr;
    CELL *Yaddr;

    Yaddr = YENV;
    check_trail();
    tab_ent = PREG->u.ld.te;
#ifdef TABLE_LOCK_AT_ENTRY_LEVEL
    LOCK(TabEnt_lock(tab_ent));
#endif /* TABLE_LOCK_LEVEL */
    sg_node = subgoal_search(tab_ent, PREG->u.ld.s, &Yaddr);
    YENV = Yaddr;
#if defined(TABLE_LOCK_AT_NODE_LEVEL)
    LOCK(TrNode_lock(sg_node));
#elif defined(TABLE_LOCK_AT_WRITE_LEVEL)
    LOCK_TABLE(sg_node);
#endif /* TABLE_LOCK_LEVEL */
    if (TrNode_sg_fr(sg_node) == NULL) {
      /* new tabled subgoal */
      new_subgoal_frame(sg_fr, sg_node, PREG->u.ld.s, LOCAL_top_sg_fr);
      TrNode_sg_fr(sg_node) = (sg_node_ptr) sg_fr;
#if defined(TABLE_LOCK_AT_ENTRY_LEVEL)
      UNLOCK(TabEnt_lock(tab_ent));
#elif defined(TABLE_LOCK_AT_NODE_LEVEL)
      UNLOCK(TrNode_lock(sg_node));
#elif defined(TABLE_LOCK_AT_WRITE_LEVEL)
      UNLOCK_TABLE(sg_node);
#endif /* TABLE_LOCK_LEVEL */
      LOCAL_top_sg_fr = sg_fr;
      store_generator_node(YENV, PREG->u.ld.s, PREG->u.ld.d, sg_fr);
      PREG = NEXTOP(PREG, ld);
      PREFETCH_OP(PREG);
      allocate_environment(YENV);
      GONext();
    } else {
      /* tabled subgoal not new */
#if defined(TABLE_LOCK_AT_ENTRY_LEVEL)
      UNLOCK(TabEnt_lock(tab_ent));
#elif defined(TABLE_LOCK_AT_NODE_LEVEL)
      UNLOCK(TrNode_lock(sg_node));
#elif defined(TABLE_LOCK_AT_WRITE_LEVEL)
      UNLOCK_TABLE(sg_node);
#endif /* TABLE_LOCK_LEVEL */
      sg_fr = (sg_fr_ptr) TrNode_sg_fr(sg_node);
      LOCK(SgFr_lock(sg_fr));
      if (SgFr_state(sg_fr)) {
        /* subgoal completed */
        if (SgFr_state(sg_fr) == complete)
          update_answer_trie(sg_fr);
        UNLOCK(SgFr_lock(sg_fr));
        if (SgFr_first_answer(sg_fr) == NULL) {
          /* no answers --> fail */
          goto fail;
        } else if (SgFr_first_answer(sg_fr) == SgFr_answer_trie(sg_fr)) {
          /* yes answer --> procceed */
          PREG = (yamop *) CPREG;
          PREFETCH_OP(PREG);
          YENV = ENV;
          GONext();
        } else {
          /* answers -> load first answer */
          PREG = (yamop *) TrNode_child(SgFr_answer_trie(sg_fr));
          PREFETCH_OP(PREG);
          *--YENV = 0;  /* vars_arity */
          *--YENV = 0;  /* heap_arity */
          GONext();
        }
      } else {
        /* subgoal not completed */
        choiceptr leader_cp;
        int leader_dep_on_stack;
        find_dependency_node(sg_fr, leader_cp, leader_dep_on_stack);
        UNLOCK(SgFr_lock(sg_fr));
        find_leader_node(leader_cp, leader_dep_on_stack);
        store_consumer_node(YENV, sg_fr, leader_cp, leader_dep_on_stack);
#ifdef OPTYAP_ERRORS
        if (PARALLEL_EXECUTION_MODE) {
          choiceptr aux_cp;
          aux_cp = B;
          while (YOUNGER_CP(aux_cp, LOCAL_top_cp_on_stack))
            aux_cp = aux_cp->cp_b;
          if (aux_cp->cp_or_fr != DepFr_top_or_fr(LOCAL_top_dep_fr))
            OPTYAP_ERROR_MESSAGE("Error on DepFr_top_or_fr (table_try_me)");
          aux_cp = B;
          while (YOUNGER_CP(aux_cp, DepFr_leader_cp(LOCAL_top_dep_fr)))
            aux_cp = aux_cp->cp_b;
          if (aux_cp != DepFr_leader_cp(LOCAL_top_dep_fr))
            OPTYAP_ERROR_MESSAGE("Error on DepFr_leader_cp (table_try_me)");
        }
#endif /* OPTYAP_ERRORS */
        goto answer_resolution;
      }
    }
  ENDPBOp();



  Op(table_retry_me, ld)
    restore_generator_node(B, PREG->u.ld.s, PREG->u.ld.d);
    YENV = (CELL *) PROTECT_FROZEN_B(B);
    set_cut(YENV, B->cp_b);
    SET_BB(NORM_CP(YENV));
    allocate_environment(YENV);
    PREG = NEXTOP(PREG,ld);
    GONext();
  ENDOp();



  Op(table_trust_me, ld)
    restore_generator_node(B, PREG->u.ld.s, COMPLETION);
    YENV = (CELL *) PROTECT_FROZEN_B(B);
    set_cut(YENV, B->cp_b);
    SET_BB(NORM_CP(YENV));
    allocate_environment(YENV);
    PREG = NEXTOP(PREG,ld);
    GONext();
  ENDOp();



  PBOp(table_new_answer, s)
    CELL *subs_ptr;
    gen_cp_ptr gcp;
    sg_fr_ptr sg_fr;
    ans_node_ptr ans_node;

    /* possible optimization: when the number of substitution variables **
    ** is zero, an answer is sufficient to perform an early completion  */
    gcp = GEN_CP(YENV[E_B]);
    sg_fr = GEN_CP_SG_FR(gcp);
    subs_ptr = (CELL *)(gcp + 1) + PREG->u.s.s;
#ifdef TABLING_ERRORS
    {
      sg_fr_ptr aux_sg_fr;
      int i, j, arity_args, arity_subs;
      CELL *aux_args;
      CELL *aux_subs;

      aux_sg_fr = LOCAL_top_sg_fr;
      while (aux_sg_fr && aux_sg_fr != sg_fr)
        aux_sg_fr = SgFr_next(aux_sg_fr);
      if (aux_sg_fr == NULL)
        TABLING_ERROR_MESSAGE("aux_sg_fr == NULL (table_new_answer)");

      arity_args = PREG->u.s.s;
      arity_subs = *subs_ptr;
      aux_args = (CELL *)(gcp + 1);
      aux_subs = subs_ptr;
      for (i = 1; i <= arity_subs; i++) {
        Term term_subs = Deref(*(aux_subs + i));
        for (j = 0; j < arity_args; j++) {
          Term term_arg = Deref(*(aux_args + j));
          if (term_subs == term_arg) break;
	}
        if (j == arity_args)
          TABLING_ERROR_MESSAGE("j == arity_args (table_new_answer)");
      }
    }
#endif /* TABLING_ERRORS */
#ifdef TABLE_LOCK_AT_ENTRY_LEVEL
    LOCK(SgFr_lock(sg_fr));
#endif /* TABLE_LOCK_LEVEL */
    ans_node = answer_search(sg_fr, subs_ptr);
#if defined(TABLE_LOCK_AT_NODE_LEVEL)
    LOCK(TrNode_lock(ans_node));
#elif defined(TABLE_LOCK_AT_WRITE_LEVEL)
    LOCK_TABLE(ans_node);
#endif /* TABLE_LOCK_LEVEL */
    if (! IS_ANSWER_LEAF_NODE(ans_node)) {
      /* new answer */
#ifdef TABLING_INNER_CUTS
      /* check for potencial prunings */
      if (! BITMAP_empty(GLOBAL_bm_pruning_workers)) {
        int until_depth, depth;

        until_depth = OrFr_depth(SgFr_gen_top_or_fr(sg_fr));
        depth = OrFr_depth(LOCAL_top_or_fr);
        if (depth > until_depth) {
          int i, ltt;
          bitmap prune_members, members;
          or_fr_ptr leftmost_or_fr, or_fr, nearest_or_fr;

          BITMAP_copy(prune_members, GLOBAL_bm_pruning_workers);
          BITMAP_delete(prune_members, worker_id);
          ltt = BRANCH_LTT(worker_id, depth);
          BITMAP_intersection(members, prune_members, OrFr_members(LOCAL_top_or_fr));
          if (members) {
            for (i = 0; i < number_workers; i++) {
              if (BITMAP_member(members, i) && 
                  BRANCH_LTT(i, depth) > ltt && 
                  EQUAL_OR_YOUNGER_CP(LOCAL_top_cp, REMOTE_pruning_scope(i))) {
                leftmost_or_fr = LOCAL_top_or_fr;
  pending_table_new_answer:
#if defined(TABLE_LOCK_AT_ENTRY_LEVEL)
                UNLOCK(SgFr_lock(sg_fr));
#elif defined(TABLE_LOCK_AT_NODE_LEVEL)
                UNLOCK(TrNode_lock(ans_node));
#elif defined(TABLE_LOCK_AT_WRITE_LEVEL)
                UNLOCK_TABLE(ans_node);
#endif /* TABLE_LOCK_LEVEL */
                LOCK_OR_FRAME(leftmost_or_fr);
                if (LOCAL_prune_request) {
                  UNLOCK_OR_FRAME(leftmost_or_fr);
                  SCHEDULER_GET_WORK();
                } else {
                  CUT_store_tg_answer(leftmost_or_fr, ans_node, NORM_CP(gcp), ltt);
                  UNLOCK_OR_FRAME(leftmost_or_fr);
                }
#ifdef TABLING_BATCHED_SCHEDULING
                /* deallocate and procceed */
                PREG = (yamop *) YENV[E_CP];
                PREFETCH_OP(PREG);
                CPREG = PREG;
                SREG = YENV;
                ENV = YENV = (CELL *) YENV[E_E];
                GONext();
#else /* TABLING_LOCAL_SCHEDULING */
                /* fail */
                goto fail;
#endif /* TABLING_SCHEDULING */
              }
	    }
            BITMAP_minus(prune_members, members);
	  }
          leftmost_or_fr = OrFr_nearest_leftnode(LOCAL_top_or_fr);
          depth = OrFr_depth(leftmost_or_fr);
          if (depth > until_depth) {
            ltt = BRANCH_LTT(worker_id, depth);
            BITMAP_intersection(members, prune_members, OrFr_members(leftmost_or_fr));
            if (members) {
              for (i = 0; i < number_workers; i++) {
                if (BITMAP_member(members, i) &&
                    BRANCH_LTT(i, depth) > ltt &&
                    EQUAL_OR_YOUNGER_CP(OrFr_node(leftmost_or_fr), REMOTE_pruning_scope(i)))
                  goto pending_table_new_answer;
	      }
              BITMAP_minus(prune_members, members);
            }
            /* reaching that point we should update the nearest leftnode data */
            leftmost_or_fr = OrFr_nearest_leftnode(leftmost_or_fr);
            depth = OrFr_depth(leftmost_or_fr);
            while (depth > until_depth) {
              ltt = BRANCH_LTT(worker_id, depth);
              BITMAP_intersection(members, prune_members, OrFr_members(leftmost_or_fr));
              if (members) {
                for (i = 0; i < number_workers; i++) {
                  if (BITMAP_member(members, i) &&
                      BRANCH_LTT(i, depth) > ltt &&
                      EQUAL_OR_YOUNGER_CP(OrFr_node(leftmost_or_fr), REMOTE_pruning_scope(i))) {
                    /* update nearest leftnode data */
                    or_fr = LOCAL_top_or_fr;
                    nearest_or_fr = OrFr_nearest_leftnode(or_fr);
                    while (OrFr_depth(nearest_or_fr) > depth) {
                      LOCK_OR_FRAME(or_fr);
                      OrFr_nearest_leftnode(or_fr) = leftmost_or_fr;
                      UNLOCK_OR_FRAME(or_fr);
                      or_fr = nearest_or_fr;
                      nearest_or_fr = OrFr_nearest_leftnode(or_fr);
                    }
                    goto pending_table_new_answer;
  	       	  }
		}
		BITMAP_minus(prune_members, members);
              }
              leftmost_or_fr = OrFr_nearest_leftnode(leftmost_or_fr);
              depth = OrFr_depth(leftmost_or_fr);
            }
            /* update nearest leftnode data */
            or_fr = LOCAL_top_or_fr;
            nearest_or_fr = OrFr_nearest_leftnode(or_fr);
            while (OrFr_depth(nearest_or_fr) > depth) {
              LOCK_OR_FRAME(or_fr);
              OrFr_nearest_leftnode(or_fr) = leftmost_or_fr;
              UNLOCK_OR_FRAME(or_fr);
              or_fr = nearest_or_fr;
              nearest_or_fr = OrFr_nearest_leftnode(or_fr);
            }
          }
        }
      }

      /* check for prune requests */
      if (LOCAL_prune_request) {
#if defined(TABLE_LOCK_AT_ENTRY_LEVEL)
        UNLOCK(SgFr_lock(sg_fr));
#elif defined(TABLE_LOCK_AT_NODE_LEVEL)
        UNLOCK(TrNode_lock(ans_node));
#elif defined(TABLE_LOCK_AT_WRITE_LEVEL)
        UNLOCK_TABLE(ans_node);
#endif /* TABLE_LOCK_LEVEL */
        SCHEDULER_GET_WORK();
      }
#endif /* TABLING_INNER_CUTS */
      TAG_AS_ANSWER_LEAF_NODE(ans_node);
#if defined(TABLE_LOCK_AT_NODE_LEVEL)
      UNLOCK(TrNode_lock(ans_node));
      LOCK(SgFr_lock(sg_fr));
#elif defined(TABLE_LOCK_AT_WRITE_LEVEL)
      UNLOCK_TABLE(ans_node);
      LOCK(SgFr_lock(sg_fr));
#endif /* TABLE_LOCK_LEVEL */
      if (SgFr_first_answer(sg_fr) == NULL) {
        SgFr_first_answer(sg_fr) = ans_node;
      } else {
        TrNode_child(SgFr_last_answer(sg_fr)) = ans_node;
      }
      SgFr_last_answer(sg_fr) = ans_node;
#ifdef TABLING_ERRORS
      { 
        ans_node_ptr aux_ans_node = SgFr_first_answer(sg_fr);
        while (aux_ans_node != SgFr_last_answer(sg_fr)) {
          if (! IS_ANSWER_LEAF_NODE(aux_ans_node))
            TABLING_ERROR_MESSAGE("! IS_ANSWER_LEAF_NODE(aux_ans_node) (table_new_answer)");
          aux_ans_node = TrNode_child(aux_ans_node);
        }
      }
#endif /* TABLING_ERRORS */
      UNLOCK(SgFr_lock(sg_fr));
#ifdef TABLING_BATCHED_SCHEDULING
      /* deallocate and procceed */
      PREG = (yamop *) YENV[E_CP];
      PREFETCH_OP(PREG);
      CPREG = PREG;
      SREG = YENV;
      ENV = YENV = (CELL *) YENV[E_E];
      GONext();
#else /* TABLING_LOCAL_SCHEDULING */
      /* fail */
      goto fail;
#endif /* TABLING_SCHEDULING */
    } else {
      /* repeated answer */
#if defined(TABLE_LOCK_AT_ENTRY_LEVEL)
      UNLOCK(SgFr_lock(sg_fr));
#elif defined(TABLE_LOCK_AT_NODE_LEVEL)
      UNLOCK(TrNode_lock(ans_node));
#elif defined(TABLE_LOCK_AT_WRITE_LEVEL)
      UNLOCK_TABLE(ans_node);
#endif /* TABLE_LOCK_LEVEL */
      goto fail;
    }
  ENDPBOp();



  BOp(table_answer_resolution, ld)
#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
      UNLOCK_OR_FRAME(LOCAL_top_or_fr);
    }
#endif /* YAPOR */


  answer_resolution:


    INIT_PREFETCH()
    dep_fr_ptr dep_fr;
    ans_node_ptr ans_node;

#ifdef OPTYAP_ERRORS
    if (SCH_top_shared_cp(B)) {
      if (B->cp_or_fr->alternative != ANSWER_RESOLUTION)
        OPTYAP_ERROR_MESSAGE("B->cp_or_fr->alternative != ANSWER_RESOLUTION (answer_resolution)");
    } else {
      if (B->cp_ap != ANSWER_RESOLUTION)
        OPTYAP_ERROR_MESSAGE("B->cp_ap != ANSWER_RESOLUTION (answer_resolution)");
    }
#endif /* OPTYAP_ERRORS */
    dep_fr = CONS_CP(B)->ccp_dep_fr;
    LOCK(DepFr_lock(dep_fr));
    ans_node = DepFr_last_ans(dep_fr);
    if (ans_node != SgFr_last_answer(DepFr_sg_fr(dep_fr))) {
      /* unconsumed answer */
      if (ans_node == NULL) {
        ans_node = DepFr_last_ans(dep_fr) = SgFr_first_answer(DepFr_sg_fr(dep_fr));
      } else {
        ans_node = DepFr_last_ans(dep_fr) = TrNode_child(ans_node);
      }
      UNLOCK(DepFr_lock(dep_fr));
      consume_answer_and_procceed(dep_fr, ans_node);
    }
    UNLOCK(DepFr_lock(dep_fr));

#if defined(YAPOR) && defined(TABLING_LOCAL_SCHEDULING)
    if (DepFr_leader_cp(LOCAL_top_dep_fr) == B) {
      /* the current top node is a generator-consumer node */
      goto completion;
    }
#endif /* YAPOR && TABLING_LOCAL_SCHEDULING */

    /* no unconsumed answers */
    if (DepFr_backchain_cp(dep_fr) == NULL) {
      /* normal backtrack */
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
        SCHEDULER_GET_WORK();
      }
#endif /* YAPOR */
      B = B->cp_b;
      goto fail;
    } else {
      /* chain backtrack */
      choiceptr top_chain_cp, chain_cp;
#ifdef YAPOR
      or_fr_ptr start_or_fr, end_or_fr;
#endif /* YAPOR */

      /* find chain choice point to backtrack */
      top_chain_cp = DepFr_backchain_cp(dep_fr);
      chain_cp = DepFr_leader_cp(LOCAL_top_dep_fr);
      if (YOUNGER_CP(top_chain_cp, chain_cp))
        chain_cp = top_chain_cp;
#ifdef TABLING_ERRORS
      if (EQUAL_OR_YOUNGER_CP(top_chain_cp, B))
        TABLING_ERROR_MESSAGE("EQUAL_OR_YOUNGER_CP(top_chain_cp, B) (answer_resolution)");
      else if (EQUAL_OR_YOUNGER_CP(chain_cp, B))
        TABLING_ERROR_MESSAGE("EQUAL_OR_YOUNGER_CP(chain_cp, B) (answer_resolution)");
#endif /* TABLING_ERRORS */

      /* check for dependency frames with unconsumed answers */
      dep_fr = DepFr_next(dep_fr);
      while (YOUNGER_CP(DepFr_cons_cp(dep_fr), chain_cp)) {
        LOCK(DepFr_lock(dep_fr));
        ans_node = DepFr_last_ans(dep_fr);
        if (ans_node != SgFr_last_answer(DepFr_sg_fr(dep_fr))) {
          /* dependency frame with unconsumed answers */
          if (ans_node == NULL) {
            ans_node = DepFr_last_ans(dep_fr) = SgFr_first_answer(DepFr_sg_fr(dep_fr));
          } else {
            ans_node = DepFr_last_ans(dep_fr) = TrNode_child(ans_node);
          }
#ifdef YAPOR
          if (YOUNGER_CP(DepFr_backchain_cp(dep_fr), top_chain_cp))
#endif /* YAPOR */
            DepFr_backchain_cp(dep_fr) = top_chain_cp;
          UNLOCK(DepFr_lock(dep_fr));

          chain_cp = DepFr_cons_cp(dep_fr);
#ifdef YAPOR
          /* update shared nodes */
          start_or_fr = LOCAL_top_or_fr;
          end_or_fr = DepFr_top_or_fr(dep_fr);
          if (start_or_fr != end_or_fr) {
            LOCAL_top_or_fr = end_or_fr;
            LOCAL_top_cp = OrFr_node(end_or_fr);
            do {
              while (YOUNGER_CP(OrFr_node(start_or_fr), OrFr_node(end_or_fr))) {
                LOCK_OR_FRAME(start_or_fr);
                BITMAP_delete(OrFr_members(start_or_fr), worker_id);
                if (BITMAP_empty(OrFr_members(start_or_fr))) {
                  if (frame_with_suspensions_not_collected(start_or_fr)) {
                    collect_suspension_frames(start_or_fr);
                  }
#ifdef TABLING_INNER_CUTS
                  if (OrFr_tg_solutions(start_or_fr)) {
                    tg_sol_fr_ptr tg_solutions;
                    or_fr_ptr leftmost_until;
                    tg_solutions = OrFr_tg_solutions(start_or_fr);
                    leftmost_until = CUT_leftmost_until(start_or_fr, OrFr_depth(TgSolFr_gen_cp(tg_solutions)->cp_or_fr));
                    OrFr_tg_solutions(start_or_fr) = NULL;
                    UNLOCK_OR_FRAME(start_or_fr);
                    if (leftmost_until) {
                      LOCK_OR_FRAME(leftmost_until);
                      tg_solutions = CUT_store_tg_answers(leftmost_until, tg_solutions,
                                                          BRANCH_LTT(worker_id, OrFr_depth(leftmost_until)));
                      UNLOCK_OR_FRAME(leftmost_until);
                    }
                    CUT_validate_tg_answers(tg_solutions);
                    goto continue_update_loop1;
                  }
#endif /* TABLING_INNER_CUTS */
                }
                UNLOCK_OR_FRAME(start_or_fr);
#ifdef TABLING_INNER_CUTS
  continue_update_loop1:
#endif /* TABLING_INNER_CUTS */
                start_or_fr = OrFr_next(start_or_fr);
  	      }
              while (YOUNGER_CP(OrFr_node(end_or_fr), OrFr_node(start_or_fr))) {
                LOCK_OR_FRAME(end_or_fr);
                BITMAP_insert(OrFr_members(end_or_fr), worker_id);
                BRANCH(worker_id, OrFr_depth(end_or_fr)) = 1;
                UNLOCK_OR_FRAME(end_or_fr);
                end_or_fr = OrFr_next(end_or_fr);
	      }
    	    } while (start_or_fr != end_or_fr);
            if (LOCAL_prune_request)
              pruning_over_tabling_data_structures(); 	
          }
#endif /* YAPOR */
#ifdef OPTYAP_ERRORS
          if (PARALLEL_EXECUTION_MODE) {
            if (YOUNGER_CP(LOCAL_top_cp, LOCAL_top_cp_on_stack)) {
              OPTYAP_ERROR_MESSAGE("YOUNGER_CP(LOCAL_top_cp, LOCAL_top_cp_on_stack) (answer_resolution)");
    	    } else {
              choiceptr aux_cp;
              aux_cp = chain_cp;
              while (aux_cp != LOCAL_top_cp) {
                if (YOUNGER_CP(LOCAL_top_cp, aux_cp)) {
                  OPTYAP_ERROR_MESSAGE("LOCAL_top_cp not in branch (answer_resolution)");
                  break;
                }
                if (EQUAL_OR_YOUNGER_CP(LOCAL_top_cp_on_stack, aux_cp)) {
                  OPTYAP_ERROR_MESSAGE("shared frozen segments in branch (answer_resolution)");
                  break;
                }
                aux_cp = aux_cp->cp_b;
              }
    	    }
	  }
#endif /* OPTYAP_ERRORS */
          /* restore bindings, update registers, consume answer and procceed */
          restore_bindings(B->cp_tr, chain_cp->cp_tr);
#ifdef TABLING_ERRORS
          if (TR != B->cp_tr) {
            if(! IsPairTerm((CELL)TrailTerm(TR - 1)))
              TABLING_ERROR_MESSAGE("! IsPairTerm((CELL)TrailTerm(TR - 1)) (answer_resolution)");
            if ((tr_fr_ptr) RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr)
              TABLING_ERROR_MESSAGE("RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr (answer_resolution)");
	  }
#endif /* TABLING_ERRORS */
          B = chain_cp;
          TR = TR_FZ;
          TRAIL_LINK(B->cp_tr);
          consume_answer_and_procceed(dep_fr, ans_node);
	}
        UNLOCK(DepFr_lock(dep_fr));
        dep_fr = DepFr_next(dep_fr);
      }

      /* no dependency frames with unconsumed answers found */
#ifdef YAPOR
      /* update shared nodes */
      if (EQUAL_OR_YOUNGER_CP(LOCAL_top_cp_on_stack, chain_cp)) {
        end_or_fr = chain_cp->cp_or_fr;
        start_or_fr = LOCAL_top_or_fr;
        if (start_or_fr != end_or_fr) {
          LOCAL_top_or_fr = end_or_fr;
          LOCAL_top_cp = OrFr_node(end_or_fr);
          while (start_or_fr != end_or_fr) {
            LOCK_OR_FRAME(start_or_fr);
            BITMAP_delete(OrFr_members(start_or_fr), worker_id);
            if (BITMAP_empty(OrFr_members(start_or_fr))) {
              if (frame_with_suspensions_not_collected(start_or_fr)) {
                collect_suspension_frames(start_or_fr);
              }
#ifdef TABLING_INNER_CUTS
              if (OrFr_tg_solutions(start_or_fr)) {
                tg_sol_fr_ptr tg_solutions;
                or_fr_ptr leftmost_until;
                tg_solutions = OrFr_tg_solutions(start_or_fr);
                leftmost_until = CUT_leftmost_until(start_or_fr, OrFr_depth(TgSolFr_gen_cp(tg_solutions)->cp_or_fr));
                OrFr_tg_solutions(start_or_fr) = NULL;
                UNLOCK_OR_FRAME(start_or_fr);
                if (leftmost_until) {
                  LOCK_OR_FRAME(leftmost_until);
                  tg_solutions = CUT_store_tg_answers(leftmost_until, tg_solutions,
                                                      BRANCH_LTT(worker_id, OrFr_depth(leftmost_until)));
                  UNLOCK_OR_FRAME(leftmost_until);
                }
                CUT_validate_tg_answers(tg_solutions);
                goto continue_update_loop2;
              }
#endif /* TABLING_INNER_CUTS */
            }
            UNLOCK_OR_FRAME(start_or_fr);
#ifdef TABLING_INNER_CUTS
  continue_update_loop2:
#endif /* TABLING_INNER_CUTS */
            start_or_fr = OrFr_next(start_or_fr);
  	  }
          if (LOCAL_prune_request)
            pruning_over_tabling_data_structures(); 
        }
      }
#endif /* YAPOR */
#ifdef OPTYAP_ERRORS
      if (PARALLEL_EXECUTION_MODE) {
        if (YOUNGER_CP(LOCAL_top_cp, LOCAL_top_cp_on_stack)) {
          OPTYAP_ERROR_MESSAGE("YOUNGER_CP(LOCAL_top_cp, LOCAL_top_cp_on_stack) (answer_resolution)");
	} else {
          choiceptr aux_cp;
          aux_cp = chain_cp;
          while (aux_cp != LOCAL_top_cp) {
            if (YOUNGER_CP(LOCAL_top_cp, aux_cp)) {
              OPTYAP_ERROR_MESSAGE("LOCAL_top_cp not in branch (answer_resolution)");
              break;
            }
            if (EQUAL_OR_YOUNGER_CP(LOCAL_top_cp_on_stack, aux_cp)) {
              OPTYAP_ERROR_MESSAGE("shared frozen segments in branch (answer_resolution)");
              break;
            }
            aux_cp = aux_cp->cp_b;
          }
	}
      }
#endif /* OPTYAP_ERRORS */
      /* unbind variables */
      unbind_variables(B->cp_tr, chain_cp->cp_tr);
#ifdef TABLING_ERRORS
      if (TR != B->cp_tr) {
        if(! IsPairTerm((CELL)TrailTerm(TR - 1)))
          TABLING_ERROR_MESSAGE("! IsPairTerm((CELL)TrailTerm(TR - 1)) (answer_resolution)");
        if ((tr_fr_ptr) RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr)
          TABLING_ERROR_MESSAGE("RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr (answer_resolution)");
      }
#endif /* TABLING_ERRORS */
      if (DepFr_leader_cp(LOCAL_top_dep_fr) == chain_cp) {
        /* the chain node is a leader node */
#ifdef YAPOR
        if (chain_cp->cp_ap == GEN_CP_NULL_ALT || DepFr_leader_dep_is_on_stack(LOCAL_top_dep_fr) == FALSE) {
          /* there are no unexploited alternatives OR the leader dependency is not on stack */
#else
        if (chain_cp->cp_ap == GEN_CP_NULL_ALT) {
          /* there are no unexploited alternatives */
#endif /* YAPOR */
          B = chain_cp;
          TR = TR_FZ;
          TRAIL_LINK(B->cp_tr);
          goto completion;
	}
      }
      /* backtrack to chain choice point */
      PREG = chain_cp->cp_ap;
      PREFETCH_OP(PREG);
      B = chain_cp;
      TR = TR_FZ;
      TRAIL_LINK(B->cp_tr);
      GONext();
    }
    END_PREFETCH()
  ENDBOp();



  BOp(table_completion, ld);
#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
      SCH_new_alternative(PREG, GEN_CP_NULL_ALT);
    } else
#endif /* YAPOR */
      B->cp_ap = GEN_CP_NULL_ALT;
#ifdef TABLING_BATCHED_SCHEDULING
    if (EQUAL_OR_YOUNGER_CP(B_FZ, B) && B != DepFr_leader_cp(LOCAL_top_dep_fr)) {
#else /* TABLING_LOCAL_SCHEDULING */
    if (B != DepFr_leader_cp(LOCAL_top_dep_fr)) {
#endif /* TABLING_SCHEDULING */
      /* not leader on that node */
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
        SCHEDULER_GET_WORK();
      }
#endif /* YAPOR */
      B = B->cp_b;
      goto fail;
    }
    /* leader on that node */


  completion:


    INIT_PREFETCH()
    dep_fr_ptr dep_fr;
    ans_node_ptr ans_node;
#ifdef YAPOR
#ifdef TIMESTAMP_CHECK
    long timestamp = 0;
#endif /* TIMESTAMP_CHECK */
    int entry_owners = 0;

    if (SCH_top_shared_cp(B)) {
#ifdef TIMESTAMP_CHECK
      timestamp = ++GLOBAL_timestamp;
#endif /* TIMESTAMP_CHECK */
      entry_owners = OrFr_owners(LOCAL_top_or_fr);
    }
#endif /* YAPOR */

    /* check for dependency frames with unconsumed answers */
    dep_fr = LOCAL_top_dep_fr;
    while (YOUNGER_CP(DepFr_cons_cp(dep_fr), B)) {
      LOCK(DepFr_lock(dep_fr));
      ans_node = DepFr_last_ans(dep_fr);
      if (ans_node != SgFr_last_answer(DepFr_sg_fr(dep_fr))) {    
        /* dependency frame with unconsumed answers */
        if (ans_node == NULL) {
          ans_node = DepFr_last_ans(dep_fr) = SgFr_first_answer(DepFr_sg_fr(dep_fr));
        } else {
          ans_node = DepFr_last_ans(dep_fr) = TrNode_child(ans_node);
        }
        if (B->cp_ap) {
#ifdef YAPOR
          if (YOUNGER_CP(DepFr_backchain_cp(dep_fr), B))
#endif /* YAPOR */
            DepFr_backchain_cp(dep_fr) = B;
	} else {
#ifdef YAPOR
          if (YOUNGER_CP(DepFr_backchain_cp(dep_fr), B->cp_b))
#endif /* YAPOR */
            DepFr_backchain_cp(dep_fr) = B->cp_b;
	}
        UNLOCK(DepFr_lock(dep_fr));

#ifdef OPTYAP_ERRORS
        if (PARALLEL_EXECUTION_MODE) {
          if (YOUNGER_CP(LOCAL_top_cp, LOCAL_top_cp_on_stack)) {
            OPTYAP_ERROR_MESSAGE("YOUNGER_CP(LOCAL_top_cp, LOCAL_top_cp_on_stack) (completion)");
          } else {
            choiceptr aux_cp;
            aux_cp = DepFr_cons_cp(dep_fr);
            while (YOUNGER_CP(aux_cp, LOCAL_top_cp_on_stack))
              aux_cp = aux_cp->cp_b;
            if (aux_cp->cp_or_fr != DepFr_top_or_fr(dep_fr))
              OPTYAP_ERROR_MESSAGE("Error on DepFr_top_or_fr (completion)");
	  }
	}
#endif /* OPTYAP_ERRORS */
#ifdef YAPOR
        /* update shared nodes */
        if (YOUNGER_CP(LOCAL_top_cp_on_stack, LOCAL_top_cp)) {
          or_fr_ptr or_frame = DepFr_top_or_fr(dep_fr);
          while (or_frame != LOCAL_top_or_fr) {
            LOCK_OR_FRAME(or_frame);
            BITMAP_insert(OrFr_members(or_frame), worker_id);
            BRANCH(worker_id, OrFr_depth(or_frame)) = 1;
            UNLOCK_OR_FRAME(or_frame);
            or_frame = OrFr_next(or_frame);
          }
          LOCAL_top_or_fr = DepFr_top_or_fr(dep_fr);
          LOCAL_top_cp = OrFr_node(LOCAL_top_or_fr);
        }
#endif /* YAPOR */
#ifdef OPTYAP_ERRORS
        if (PARALLEL_EXECUTION_MODE) {
          if (YOUNGER_CP(LOCAL_top_cp, LOCAL_top_cp_on_stack)) {
            OPTYAP_ERROR_MESSAGE("YOUNGER_CP(LOCAL_top_cp, LOCAL_top_cp_on_stack) (completion)");
          } else {
            choiceptr aux_cp;
            aux_cp = DepFr_cons_cp(dep_fr);
            while (aux_cp != LOCAL_top_cp) {
              if (YOUNGER_CP(LOCAL_top_cp, aux_cp)) {
                OPTYAP_ERROR_MESSAGE("LOCAL_top_cp not in branch (completion)");
                break;
              }
              if (EQUAL_OR_YOUNGER_CP(LOCAL_top_cp_on_stack, aux_cp)) {
                OPTYAP_ERROR_MESSAGE("shared frozen segments in branch (completion)");
                break;
              }
              aux_cp = aux_cp->cp_b;
            }
          }
	}
#endif /* OPTYAP_ERRORS */
        /* rebind variables, update registers, consume answer and procceed */
#ifdef TABLING_ERRORS
        if (EQUAL_OR_YOUNGER_CP(B, DepFr_cons_cp(dep_fr)))
          TABLING_ERROR_MESSAGE("EQUAL_OR_YOUNGER_CP(B, DepFr_cons_cp(dep_fr)) (completion)");
        if (B->cp_tr > DepFr_cons_cp(dep_fr)->cp_tr)
          TABLING_ERROR_MESSAGE("B->cp_tr > DepFr_cons_cp(dep_fr)->cp_tr (completion)");
#endif /* TABLING_ERRORS */
        rebind_variables(DepFr_cons_cp(dep_fr)->cp_tr, B->cp_tr);
#ifdef TABLING_ERRORS
        if (TR != B->cp_tr) {
          if(! IsPairTerm((CELL)TrailTerm(TR - 1)))
            TABLING_ERROR_MESSAGE("! IsPairTerm((CELL)TrailTerm(TR - 1)) (completion)");
          if ((tr_fr_ptr) RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr)
            TABLING_ERROR_MESSAGE("RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr (completion)");
	}
#endif /* TABLING_ERRORS */
        B = DepFr_cons_cp(dep_fr);
        TR = TR_FZ;
        if (TR != B->cp_tr)
          TRAIL_LINK(B->cp_tr);
        consume_answer_and_procceed(dep_fr, ans_node);
      }
      UNLOCK(DepFr_lock(dep_fr));
#ifdef TIMESTAMP_CHECK
      DepFr_timestamp(dep_fr) = timestamp;
#endif /* TIMESTAMP_CHECK */
      dep_fr = DepFr_next(dep_fr);
    }

    /* no dependency frames with unconsumed answers found */
#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
      if (entry_owners > 1) {
        /* more owners when we start looking for dependency frames with unconsumed answers */
        if (YOUNGER_CP(B_FZ, B)) {
          suspend_branch();
          /* check for suspension frames to be resumed */
          while (YOUNGER_CP(OrFr_node(LOCAL_top_susp_or_fr), LOCAL_top_cp)) {
            or_fr_ptr susp_or_fr;
            susp_fr_ptr resume_fr;
            susp_or_fr = LOCAL_top_susp_or_fr;
            LOCK_OR_FRAME(susp_or_fr);
#ifdef TIMESTAMP_CHECK
            resume_fr = suspension_frame_to_resume(susp_or_fr, timestamp);
#else
            resume_fr = suspension_frame_to_resume(susp_or_fr);
#endif /* TIMESTAMP_CHECK */
	    if (resume_fr) {
              if (OrFr_suspensions(susp_or_fr) == NULL) {
                LOCAL_top_susp_or_fr = OrFr_nearest_suspnode(susp_or_fr);
                OrFr_nearest_suspnode(susp_or_fr) = susp_or_fr;
              }
              UNLOCK_OR_FRAME(susp_or_fr);
              rebind_variables(OrFr_node(susp_or_fr)->cp_tr, B->cp_tr);
              resume_suspension_frame(resume_fr, susp_or_fr);
              B = LOCAL_top_cp;
              SET_BB(B_FZ);
              TR = TR_FZ;
              TRAIL_LINK(B->cp_tr);
              goto completion;
            }
            LOCAL_top_susp_or_fr = OrFr_nearest_suspnode(susp_or_fr);
            OrFr_nearest_suspnode(susp_or_fr) = NULL;
            UNLOCK_OR_FRAME(susp_or_fr);
          }
        }
      } else {
        /* unique owner */
        if (frame_with_suspensions_not_collected(LOCAL_top_or_fr))
          collect_suspension_frames(LOCAL_top_or_fr);
        /* check for suspension frames to be resumed */
        while (EQUAL_OR_YOUNGER_CP(OrFr_node(LOCAL_top_susp_or_fr), LOCAL_top_cp)) {
          or_fr_ptr susp_or_fr;
          susp_fr_ptr resume_fr;
          susp_or_fr = LOCAL_top_susp_or_fr;
#ifdef TIMESTAMP_CHECK
          resume_fr = suspension_frame_to_resume(susp_or_fr, timestamp);
#else
          resume_fr = suspension_frame_to_resume(susp_or_fr);
#endif /* TIMESTAMP_CHECK */
          if (resume_fr) {
            if (OrFr_suspensions(susp_or_fr) == NULL) {
              LOCAL_top_susp_or_fr = OrFr_nearest_suspnode(susp_or_fr);
              OrFr_nearest_suspnode(susp_or_fr) = susp_or_fr;
            }
            if (YOUNGER_CP(B_FZ, B)) {
              suspend_branch();
            }
            rebind_variables(OrFr_node(susp_or_fr)->cp_tr, B->cp_tr);
            resume_suspension_frame(resume_fr, susp_or_fr);
            B = LOCAL_top_cp;
            SET_BB(B_FZ);
            TR = TR_FZ;
            TRAIL_LINK(B->cp_tr);
            goto completion;
          }
          LOCAL_top_susp_or_fr = OrFr_nearest_suspnode(susp_or_fr);
          OrFr_nearest_suspnode(susp_or_fr) = NULL;
        }
        /* complete all */
        public_completion();
      }
#ifdef TABLING_ERRORS
      if (TR != B->cp_tr) {
        if(! IsPairTerm((CELL)TrailTerm(TR - 1)))
          TABLING_ERROR_MESSAGE("! IsPairTerm((CELL)TrailTerm(TR - 1)) (completion)");
        if ((tr_fr_ptr) RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr)
          TABLING_ERROR_MESSAGE("RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr (completion)");
      }
#endif /* TABLING_ERRORS */
#ifdef TABLING_LOCAL_SCHEDULING
      if (DepFr_leader_cp(LOCAL_top_dep_fr) == B) {
        /* the current top node is a generator-consumer node */
        ans_node_ptr ans_node;
        TR = B->cp_tr;
        SET_BB(B);
        LOCK_OR_FRAME(LOCAL_top_or_fr);
        LOCK(DepFr_lock(LOCAL_top_dep_fr));
        ans_node = DepFr_last_ans(LOCAL_top_dep_fr);
        if (ans_node != SgFr_last_answer(DepFr_sg_fr(LOCAL_top_dep_fr))) {
          /* unconsumed answer */
          UNLOCK_OR_FRAME(LOCAL_top_or_fr);
          if (ans_node == NULL) {
            ans_node = DepFr_last_ans(LOCAL_top_dep_fr) = SgFr_first_answer(DepFr_sg_fr(LOCAL_top_dep_fr));
          } else {
            ans_node = DepFr_last_ans(LOCAL_top_dep_fr) = TrNode_child(ans_node);
          }
          UNLOCK(DepFr_lock(LOCAL_top_dep_fr));
          consume_answer_and_procceed(LOCAL_top_dep_fr, ans_node);
        }
        /* no unconsumed answers */
        UNLOCK(DepFr_lock(LOCAL_top_dep_fr));
        if (OrFr_owners(LOCAL_top_or_fr) > 1) {
          /* more owners -> move up one node */
          LOCAL_top_cp_on_stack = OrFr_node(OrFr_next_on_stack(LOCAL_top_or_fr));
          BITMAP_delete(OrFr_members(LOCAL_top_or_fr), worker_id);
          OrFr_owners(LOCAL_top_or_fr)--;
          LOCAL_top_dep_fr = DepFr_next(LOCAL_top_dep_fr);
          UNLOCK_OR_FRAME(LOCAL_top_or_fr);
          if (LOCAL_top_sg_fr && LOCAL_top_cp == SgFr_gen_cp(LOCAL_top_sg_fr)) {
            LOCAL_top_sg_fr = SgFr_next(LOCAL_top_sg_fr);
          }
          SCH_update_local_or_tops();
          CUT_reset_prune_request();
          adjust_freeze_registers();
          goto shared_fail;
        } else {
          /* free top dependency frame --> get work */
          OrFr_alternative(LOCAL_top_or_fr) = NULL;
          UNLOCK_OR_FRAME(LOCAL_top_or_fr);
          dep_fr = DepFr_next(LOCAL_top_dep_fr);
          FREE_DEPENDENCY_FRAME(LOCAL_top_dep_fr);
          LOCAL_top_dep_fr = dep_fr;
          adjust_freeze_registers();
          SCHEDULER_GET_WORK();
        }
      }
#endif /* TABLING_LOCAL_SCHEDULING */
      /* goto getwork */
      PREG = B->cp_ap;
      PREFETCH_OP(PREG);
      TR = B->cp_tr;
      SET_BB(B);
      GONext();
    } else
#endif /* YAPOR */
    {
      /* complete all */
      sg_fr_ptr sg_fr;

      sg_fr = GEN_CP_SG_FR(B);
      private_completion(sg_fr);
#ifdef TABLING_BATCHED_SCHEDULING
      /* backtrack */
      B = B->cp_b;
      SET_BB(PROTECT_FROZEN_B(B));
      goto fail;
#else /* TABLING_LOCAL_SCHEDULING */
      /* subgoal completed */
      LOCK(SgFr_lock(sg_fr));
      if (SgFr_state(sg_fr) == complete)
        update_answer_trie(sg_fr);
      UNLOCK(SgFr_lock(sg_fr));
      if (SgFr_first_answer(sg_fr) == NULL) {
        /* no answers --> fail */
        B = B->cp_b;
        SET_BB(PROTECT_FROZEN_B(B));
        goto fail;
      }
#ifdef TABLING_ERRORS
      if (TR != B->cp_tr) {
        if(! IsPairTerm((CELL)TrailTerm(TR - 1)))
          TABLING_ERROR_MESSAGE("! IsPairTerm((CELL)TrailTerm(TR - 1)) (completion)");
        if ((tr_fr_ptr) RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr)
          TABLING_ERROR_MESSAGE("RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr (completion)");
      }
#endif /* TABLING_ERRORS */
      pop_generator_node(B, SgFr_arity(sg_fr));
      if (SgFr_first_answer(sg_fr) == SgFr_answer_trie(sg_fr)) {
        /* yes answer --> procceed */
        PREG = (yamop *) CPREG;
        PREFETCH_OP(PREG);
        YENV = ENV;
        GONext();
      } else  {
        /* answers -> load first answer */
        PREG = (yamop *) TrNode_child(SgFr_answer_trie(sg_fr));
        PREFETCH_OP(PREG);
        *--YENV = 0;  /* vars_arity */
        *--YENV = 0;  /* heap_arity */
        GONext();
      }
#endif /* TABLING_SCHEDULING */
    }
    END_PREFETCH()
  ENDBOp();
