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

/************************************************************************
**               Tabling instructions: auxiliary macros                **
************************************************************************/

#ifdef LOW_LEVEL_TRACER
#define store_low_level_trace_info(CP, TAB_ENT)  \
        CP->cp_pred_entry = TabEnt_pe(TAB_ENT)
#else
#define store_low_level_trace_info(CP, TAB_ENT)
#endif /* LOW_LEVEL_TRACER */

#define TABLING_ERROR_CHECKING_STACK                                             \
        TABLING_ERROR_CHECKING(store_node, Unsigned(H) + 1024 > Unsigned(B));    \
	TABLING_ERROR_CHECKING(store_node, Unsigned(H_FZ) + 1024 > Unsigned(B))


#define store_generator_node(TAB_ENT, SG_FR, ARITY, AP)                   \
        { register CELL *pt_args;                                         \
          register choiceptr gcp;                                         \
          /* store args */                                                \
          pt_args = XREGS + (ARITY);                                      \
	  while (pt_args > XREGS) {                                       \
            register CELL aux_arg = pt_args[0];                           \
            --YENV;                                                       \
            --pt_args;                                                    \
            *YENV = aux_arg;                                              \
	  }                                                               \
          /* initialize gcp and adjust subgoal frame field */             \
          YENV = (CELL *) (GEN_CP(YENV) - 1);                             \
          gcp = NORM_CP(YENV);                                            \
          SgFr_gen_cp(SG_FR) = gcp;                                       \
          /* store generator choice point */                              \
          HBREG = H;                                                      \
          store_yaam_reg_cpdepth(gcp);                                    \
          gcp->cp_tr = TR;                                                \
          gcp->cp_ap = (yamop *)(AP);                                     \
          gcp->cp_h  = H;                                                 \
          gcp->cp_b  = B;                                                 \
          gcp->cp_env = ENV;                                              \
          gcp->cp_cp = CPREG;                                             \
	  if (IsMode_Local(TabEnt_mode(TAB_ENT))) {                       \
            /* go local */                                                \
            register dep_fr_ptr new_dep_fr;                               \
            /* adjust freeze registers */                                 \
            H_FZ = H;                                                     \
            B_FZ = gcp;                                                   \
            TR_FZ = TR;                                                   \
            /* store dependency frame */                                  \
            new_dependency_frame(new_dep_fr, TRUE, LOCAL_top_or_fr, gcp,  \
                                 gcp, SG_FR, FALSE, LOCAL_top_dep_fr);    \
            LOCAL_top_dep_fr = new_dep_fr;                                \
            GEN_CP(gcp)->cp_dep_fr = LOCAL_top_dep_fr;                    \
          } else {                                                        \
            /* go batched */                                              \
            GEN_CP(gcp)->cp_dep_fr = NULL;                                \
          }                                                               \
          GEN_CP(gcp)->cp_sg_fr = SG_FR;                                  \
          store_low_level_trace_info(GEN_CP(gcp), TAB_ENT);               \
          set_cut((CELL *)gcp, B);                                        \
          B = gcp;                                                        \
          YAPOR_SET_LOAD(B);                                              \
          SET_BB(B);                                                      \
          TABLING_ERROR_CHECKING_STACK;                                   \
        }


#ifdef DETERMINISTIC_TABLING
#define store_deterministic_generator_node(TAB_ENT, SG_FR)            \
        { register choiceptr gcp;                                     \
          /* initialize gcp and adjust subgoal frame field */         \
          YENV = (CELL *) (DET_GEN_CP(YENV) - 1);                     \
	  gcp = NORM_CP(YENV);                                        \
          SgFr_gen_cp(SG_FR) = gcp;                                   \
          /* store deterministic generator choice point */            \
          HBREG = H;                                                  \
          store_yaam_reg_cpdepth(gcp);                                \
          gcp->cp_ap = COMPLETION;                                    \
          gcp->cp_b  = B;                                             \
          gcp->cp_tr = TR;           	  	                      \
          gcp->cp_h = H;                                              \
	  DET_GEN_CP(gcp)->cp_sg_fr = SG_FR;                          \
          store_low_level_trace_info(DET_GEN_CP(gcp), TAB_ENT);       \
          set_cut((CELL *)gcp, B);                                    \
          B = gcp;                                                    \
          YAPOR_SET_LOAD(B);                                          \
          SET_BB(B);                                                  \
          TABLING_ERROR_CHECKING_STACK;                               \
	}
#endif /* DETERMINISTIC_TABLING */


#ifdef THREADS_CONSUMER_SHARING
#define store_generator_consumer_node(TAB_ENT, SG_FR, DEP_ON_STACK,ARITY)   \
       {  register CELL *pt_args;				            \
          register choiceptr gccp;				            \
          register dep_fr_ptr new_dep_fr;                                   \
          /* store args */						    \
          pt_args = XREGS + (ARITY);                                        \
	  while (pt_args > XREGS) {                                         \
            register CELL aux_arg = pt_args[0];                             \
            --YENV;                                                         \
            --pt_args;                                                      \
            *YENV = aux_arg;                                                \
	  }                                                                 \
          /* initialize gcp and adjust subgoal frame field */               \
          YENV = (CELL *) (GEN_CP(YENV) - 1);                               \
          gccp = NORM_CP(YENV);                                             \
          SgFr_gen_cp(SG_FR) = gccp;					    \
          /* store generator choice point */		                    \
          HBREG = H;                                                        \
          store_yaam_reg_cpdepth(gccp);                                     \
          gccp->cp_tr = TR;                                                 \
          gccp->cp_ap = ANSWER_RESOLUTION_COMPLETION;                       \
          gccp->cp_h  = H;                                                  \
          gccp->cp_b  = B;                                                  \
          gccp->cp_env = ENV;                                               \
          gccp->cp_cp = CPREG;                                              \
          /* store dependency frame */                                      \
          new_dependency_frame(new_dep_fr, DEP_ON_STACK, LOCAL_top_or_fr,   \
			       gccp, gccp, SG_FR, TRUE, LOCAL_top_dep_fr);  \
          LOCAL_top_dep_fr = new_dep_fr;                                    \
          GEN_CP(gccp)->cp_dep_fr = LOCAL_top_dep_fr;                       \
          GEN_CP(gccp)->cp_sg_fr = SG_FR;                                   \
	  /* adjust freeze registers */					    \
	  H_FZ = H;							    \
	  B_FZ = gccp;							    \
	  TR_FZ = TR;							    \
	  store_low_level_trace_info(GEN_CP(gccp), TAB_ENT);		    \
          set_cut((CELL *)gccp, B);                                         \
          B = gccp;                                                         \
          YAPOR_SET_LOAD(B);                                                \
          SET_BB(B);                                                        \
          TABLING_ERROR_CHECKING_STACK;                                     \
       }
#endif /* THREADS_CONSUMER_SHARING */


#define restore_generator_node(ARITY, AP)               \
        { register CELL *pt_args, *x_args;              \
          register choiceptr gcp = B;                   \
          /* restore generator choice point */          \
          H = HBREG = PROTECT_FROZEN_H(gcp);            \
          restore_yaam_reg_cpdepth(gcp);                \
          CPREG = gcp->cp_cp;                           \
          ENV = gcp->cp_env;                            \
          YAPOR_update_alternative(PREG, (yamop *) AP)  \
          gcp->cp_ap = (yamop *) AP;                    \
          /* restore args */                            \
          pt_args = (CELL *)(GEN_CP(gcp) + 1) + ARITY;  \
          x_args = XREGS + 1 + ARITY;                   \
          while (x_args > XREGS + 1) {                  \
            register CELL x = pt_args[-1];              \
            --x_args;                                   \
            --pt_args;                                  \
            *x_args = x;                                \
	  }                                             \
        }


#define pop_generator_node(ARITY)               \
        { register CELL *pt_args, *x_args;      \
          register choiceptr gcp = B;           \
          /* pop generator choice point */      \
          H = PROTECT_FROZEN_H(gcp);            \
          pop_yaam_reg_cpdepth(gcp);            \
          CPREG = gcp->cp_cp;                   \
          ENV = gcp->cp_env;                    \
          TR = gcp->cp_tr;                      \
          B = gcp->cp_b;                        \
          HBREG = PROTECT_FROZEN_H(B);		\
          /* pop args */                        \
          x_args = XREGS + 1 ;                  \
          pt_args = (CELL *)(GEN_CP(gcp) + 1);  \
	  while (x_args < XREGS + 1 + ARITY) {  \
            register CELL x = pt_args[0];       \
            pt_args++;                          \
            x_args++;                           \
            x_args[-1] = x;                     \
          }                                     \
          YENV = pt_args;		    	\
          SET_BB(PROTECT_FROZEN_B(B));          \
        }


#define store_consumer_node(TAB_ENT, SG_FR, LEADER_CP, DEP_ON_STACK)             \
        { register choiceptr ccp;                                                \
          register dep_fr_ptr new_dep_fr;                                        \
	  /* initialize ccp */                                                   \
          YENV = (CELL *) (CONS_CP(YENV) - 1);                                   \
          ccp = NORM_CP(YENV);                                                   \
          /* adjust freeze registers */                                          \
          H_FZ = H;                                                              \
          B_FZ = ccp;                    	                                 \
          TR_FZ = TR;                                                            \
          /* store dependency frame */                                           \
          new_dependency_frame(new_dep_fr, DEP_ON_STACK, LOCAL_top_or_fr,        \
                               LEADER_CP, ccp, SG_FR, FALSE, LOCAL_top_dep_fr);  \
          LOCAL_top_dep_fr = new_dep_fr;                                         \
          /* store consumer choice point */                                      \
          HBREG = H;                                                             \
          store_yaam_reg_cpdepth(ccp);                                           \
          ccp->cp_tr = TR;         	                                         \
          ccp->cp_ap = ANSWER_RESOLUTION;                                        \
          ccp->cp_h  = H;                                                        \
          ccp->cp_b  = B;                                                        \
          ccp->cp_env= ENV;                                                      \
          ccp->cp_cp = CPREG;                                                    \
          CONS_CP(ccp)->cp_dep_fr = LOCAL_top_dep_fr;                            \
          store_low_level_trace_info(CONS_CP(ccp), TAB_ENT);                     \
          /* set_cut((CELL *)ccp, B); --> no effect */                           \
          B = ccp;                                                               \
          YAPOR_SET_LOAD(B);                                                     \
          SET_BB(B);                                                             \
          TABLING_ERROR_CHECKING_STACK;                                          \
        }


#ifdef THREADS_CONSUMER_SHARING
#define consume_answer_and_procceed(DEP_FR, ANSWER)                            \
        { CELL *subs_ptr;                                                      \
          /* restore consumer choice point */                                  \
          H = HBREG = PROTECT_FROZEN_H(B);                                     \
          restore_yaam_reg_cpdepth(B);                                         \
          CPREG = B->cp_cp;                                                    \
          ENV = B->cp_env;                                                     \
          /* set_cut(YENV, B->cp_b); --> no effect */                          \
          PREG = (yamop *) CPREG;                                              \
          PREFETCH_OP(PREG);                                                   \
          /* load answer from table to global stack */                         \
          if (B == DepFr_leader_cp(DEP_FR) || DepFr_external(DEP_FR)) {        \
            /*  B is a generator-consumer node  */                             \
            TABLING_ERROR_CHECKING(generator_consumer, IS_BATCHED_GEN_CP(B));  \
            subs_ptr = (CELL *) (GEN_CP(B) + 1);                               \
            subs_ptr += SgFr_arity(GEN_CP(B)->cp_sg_fr);                       \
	  } else {                                                             \
            subs_ptr = (CELL *) (CONS_CP(B) + 1);                              \
	  }                                                                    \
          load_answer(ANSWER, subs_ptr);                                       \
          /* procceed */                                                       \
          YENV = ENV;                                                          \
          GONext();                                                            \
        }
#else
#define consume_answer_and_procceed(DEP_FR, ANSWER)                            \
        { CELL *subs_ptr;                                                      \
          /* restore consumer choice point */                                  \
          H = HBREG = PROTECT_FROZEN_H(B);                                     \
          restore_yaam_reg_cpdepth(B);                                         \
          CPREG = B->cp_cp;                                                    \
          ENV = B->cp_env;                                                     \
          /* set_cut(YENV, B->cp_b); --> no effect */                          \
          PREG = (yamop *) CPREG;                                              \
          PREFETCH_OP(PREG);                                                   \
          /* load answer from table to global stack */                         \
          if (B == DepFr_leader_cp(DEP_FR)) {                                  \
            /*  B is a generator-consumer node  */                             \
            /* never here if batched scheduling */                             \
            TABLING_ERROR_CHECKING(generator_consumer, IS_BATCHED_GEN_CP(B));  \
            subs_ptr = (CELL *) (GEN_CP(B) + 1);                               \
            subs_ptr += SgFr_arity(GEN_CP(B)->cp_sg_fr);                       \
	  } else {                                                             \
            subs_ptr = (CELL *) (CONS_CP(B) + 1);                              \
	  }                                                                    \
          load_answer(ANSWER, subs_ptr);                                       \
          /* procceed */                                                       \
          YENV = ENV;                                                          \
          GONext();                                                            \
        }
#endif /* THREADS_CONSUMER_SHARING */


#define store_loader_node(TAB_ENT, ANSWER)                    \
        { register choiceptr lcp;                             \
	  /* initialize lcp */                                \
          lcp = NORM_CP(LOAD_CP(YENV) - 1);                   \
          /* store loader choice point */                     \
          HBREG = H;                                          \
          store_yaam_reg_cpdepth(lcp);                        \
          lcp->cp_tr = TR;         	                      \
          lcp->cp_ap = LOAD_ANSWER;                           \
          lcp->cp_h  = H;                                     \
          lcp->cp_b  = B;                                     \
          lcp->cp_env= ENV;                                   \
          lcp->cp_cp = CPREG;                                 \
          LOAD_CP(lcp)->cp_last_answer = ANSWER;              \
          store_low_level_trace_info(LOAD_CP(lcp), TAB_ENT);  \
          /* set_cut((CELL *)lcp, B); --> no effect */        \
          B = lcp;                                            \
          YAPOR_SET_LOAD(B);                                  \
          SET_BB(B);                                          \
          TABLING_ERROR_CHECKING_STACK;                       \
        }


#define restore_loader_node(ANSWER)           \
        H = HBREG = PROTECT_FROZEN_H(B);      \
        restore_yaam_reg_cpdepth(B);          \
        CPREG = B->cp_cp;                     \
        ENV = B->cp_env;                      \
        LOAD_CP(B)->cp_last_answer = ANSWER;  \
        SET_BB(PROTECT_FROZEN_B(B))


#define pop_loader_node()             \
        H = PROTECT_FROZEN_H(B);      \
        pop_yaam_reg_cpdepth(B);      \
	CPREG = B->cp_cp;             \
        TABLING_close_alt(B);	      \
        ENV = B->cp_env;              \
	B = B->cp_b;	              \
        HBREG = PROTECT_FROZEN_H(B);  \
        SET_BB(PROTECT_FROZEN_B(B))


#ifdef DEPTH_LIMIT
#define allocate_environment()        \
        YENV[E_CP] = (CELL) CPREG;    \
        YENV[E_E] = (CELL) ENV;       \
        YENV[E_B] = (CELL) B;         \
        YENV[E_DEPTH] = (CELL)DEPTH;  \
        ENV = YENV
#else
#define allocate_environment()        \
        YENV[E_CP] = (CELL) CPREG;    \
        YENV[E_E] = (CELL) ENV;       \
        YENV[E_B] = (CELL) B;         \
        ENV = YENV
#endif /* DEPTH_LIMIT */



/************************************************************************
**                           clause_with_cut                           **
************************************************************************/

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



/************************************************************************
**                          table_load_answer                          **
************************************************************************/

  PBOp(table_load_answer, Otapl)
    CELL *subs_ptr;
    ans_node_ptr ans_node;

#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
#if 0
      PROBLEM: cp_last_answer field is local to the cp!
               -> we need a shared data structure to avoid redundant computations!
      UNLOCK_OR_FRAME(LOCAL_top_or_fr);
#else
      Sfprintf(Serror, "PROBLEM: cp_last_answer field is local to the cp!\n");
      exit(1);
#endif
    }
#endif /* YAPOR */
    subs_ptr = (CELL *) (LOAD_CP(B) + 1);
    ans_node = TrNode_child(LOAD_CP(B)->cp_last_answer);
    if(TrNode_child(ans_node) != NULL) {
      restore_loader_node(ans_node);
    } else {
      pop_loader_node();
    }
    PREG = (yamop *) CPREG;
    PREFETCH_OP(PREG);
    load_answer(ans_node, subs_ptr);
    YENV = ENV;
    GONext();
  ENDPBOp();



/************************************************************************
**                          table_try_answer                           **
************************************************************************/

  PBOp(table_try_answer, Otapl)
#ifdef INCOMPLETE_TABLING
    sg_fr_ptr sg_fr;
    ans_node_ptr ans_node;

    sg_fr = GEN_CP(B)->cp_sg_fr;
    ans_node = TrNode_child(SgFr_try_answer(sg_fr));
    if(ans_node) {
      CELL *subs_ptr = (CELL *) (GEN_CP(B) + 1) + SgFr_arity(sg_fr);

      H = HBREG = PROTECT_FROZEN_H(B);
      restore_yaam_reg_cpdepth(B);
      CPREG = B->cp_cp;
      ENV = B->cp_env;
      SgFr_try_answer(sg_fr) = ans_node;
#ifdef YAPOR
      if (SCH_top_shared_cp(B))
	UNLOCK_OR_FRAME(LOCAL_top_or_fr);
#endif /* YAPOR */
      SET_BB(PROTECT_FROZEN_B(B));

      PREG = (yamop *) CPREG;
      PREFETCH_OP(PREG);
      load_answer(ans_node, subs_ptr);
      YENV = ENV;
      GONext();
    } else {
      yamop *code_ap;
      PREG = SgFr_code(sg_fr);
      if (PREG->opc == Yap_opcode(_table_try)) {
	/* table_try */
	code_ap = NEXTOP(PREG,Otapl);
	PREG = PREG->u.Otapl.d;
      } else if (PREG->opc == Yap_opcode(_table_try_single)) {
	/* table_try_single */
	code_ap = COMPLETION;
	PREG = PREG->u.Otapl.d;
      } else {
	/* table_try_me */
	code_ap = PREG->u.Otapl.d;
	PREG = NEXTOP(PREG,Otapl);
      }
      PREFETCH_OP(PREG);
      restore_generator_node(SgFr_arity(sg_fr), code_ap);
      YENV = (CELL *) PROTECT_FROZEN_B(B);
      set_cut(YENV, B->cp_b);
      SET_BB(NORM_CP(YENV));
      allocate_environment();
      GONext();
    }
#else
    PREG = PREG->u.Otapl.d;
    PREFETCH_OP(PREG);
    GONext();    
#endif /* INCOMPLETE_TABLING */
  ENDPBOp();



/************************************************************************
**                          table_try_single                           **
************************************************************************/

  PBOp(table_try_single, Otapl)
    tab_ent_ptr tab_ent;
    sg_fr_ptr sg_fr;

    check_trail(TR);
    tab_ent = PREG->u.Otapl.te;
    YENV2MEM;
    sg_fr = subgoal_search(PREG, YENV_ADDRESS);
    MEM2YENV;
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
    if (SgFr_state(sg_fr) <= ready) {
      LOCK_SG_FR(sg_fr);
      if (SgFr_sg_ent_state(sg_fr) >= complete)
	SgFr_state(sg_fr) = SgFr_sg_ent_state(sg_fr);
      else
	SgFr_active_workers(sg_fr)++;
      UNLOCK_SG_FR(sg_fr);
    }
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
    LOCK_SG_FR(sg_fr);
#ifdef THREADS_CONSUMER_SHARING
    if (SgFr_state(sg_fr) == ready_external) {
      init_subgoal_frame(sg_fr);
      store_generator_consumer_node(tab_ent, sg_fr, TRUE, PREG->u.Otapl.s);
      PREFETCH_OP(PREG);
      allocate_environment();
      check_for_deadlock(sg_fr);
      goto answer_resolution_completion;
    } else
#endif /* THREADS_CONSUMER_SHARING */
    if (SgFr_state(sg_fr) == ready) {
      /* subgoal new */
      init_subgoal_frame(sg_fr);
      UNLOCK_SG_FR(sg_fr);
#ifdef DETERMINISTIC_TABLING
      if (IsMode_Batched(TabEnt_mode(tab_ent))) {
	store_deterministic_generator_node(tab_ent, sg_fr);
      } else
#endif /* DETERMINISTIC_TABLING */
      {
	store_generator_node(tab_ent, sg_fr, PREG->u.Otapl.s, COMPLETION);
      }
      PREG = PREG->u.Otapl.d;  /* should work also with PREG = NEXTOP(PREG,Otapl); */
      PREFETCH_OP(PREG);
      allocate_environment();
      GONext();
#ifdef INCOMPLETE_TABLING
    } else if (SgFr_state(sg_fr) == incomplete) {
      /* subgoal incomplete --> start by loading the answers already found */
      ans_node_ptr ans_node = SgFr_first_answer(sg_fr);
      CELL *subs_ptr = YENV;
      init_subgoal_frame(sg_fr);
      UNLOCK_SG_FR(sg_fr);
      SgFr_try_answer(sg_fr) = ans_node;
      store_generator_node(tab_ent, sg_fr, PREG->u.Otapl.s, TRY_ANSWER);
      PREG = (yamop *) CPREG;
      PREFETCH_OP(PREG);
      load_answer(ans_node, subs_ptr);
      YENV = ENV;
      GONext();
#endif /* INCOMPLETE_TABLING */
    } else if (SgFr_state(sg_fr) == evaluating) {
      /* subgoal in evaluation */
      choiceptr leader_cp;
      int leader_dep_on_stack;
      find_dependency_node(sg_fr, leader_cp, leader_dep_on_stack);
      UNLOCK_SG_FR(sg_fr);
      find_leader_node(leader_cp, leader_dep_on_stack);
      store_consumer_node(tab_ent, sg_fr, leader_cp, leader_dep_on_stack);
#ifdef DEBUG_OPTYAP
      if (GLOBAL_parallel_mode == PARALLEL_MODE_RUNNING) {
	choiceptr aux_cp;
	aux_cp = B;
	while (YOUNGER_CP(aux_cp, Get_LOCAL_top_cp_on_stack()))
	  aux_cp = aux_cp->cp_b;
	OPTYAP_ERROR_CHECKING(table_try_single, aux_cp->cp_or_fr != DepFr_top_or_fr(LOCAL_top_dep_fr));
	aux_cp = B;
	while (YOUNGER_CP(aux_cp, DepFr_leader_cp(LOCAL_top_dep_fr)))
	  aux_cp = aux_cp->cp_b;
	OPTYAP_ERROR_CHECKING(table_try_single, aux_cp != DepFr_leader_cp(LOCAL_top_dep_fr));
      }
#endif /* DEBUG_OPTYAP */
      goto answer_resolution;
    } else {
      /* subgoal completed */
      ans_node_ptr ans_node = SgFr_first_answer(sg_fr);
      if (ans_node == NULL) {
	/* no answers --> fail */
	UNLOCK_SG_FR(sg_fr);
	goto fail;
      } else if (ans_node == SgFr_answer_trie(sg_fr)) {
	/* yes answer --> procceed */
	UNLOCK_SG_FR(sg_fr);
	PREG = (yamop *) CPREG;
	PREFETCH_OP(PREG);
	YENV = ENV;
	GONext();
      } else {
	/* answers -> get first answer */
#ifdef LIMIT_TABLING
	if (SgFr_state(sg_fr) == complete || SgFr_state(sg_fr) == compiled) {
	  SgFr_state(sg_fr)++;  /* complete --> complete_in_use : compiled --> compiled_in_use */
	  remove_from_global_sg_fr_list(sg_fr);
	  TRAIL_FRAME(sg_fr);
	}
#endif /* LIMIT_TABLING */
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
	if (IsMode_LoadAnswers(TabEnt_mode(tab_ent)) || SgFr_active_workers(sg_fr) > 0) {
#else
        if (IsMode_LoadAnswers(TabEnt_mode(tab_ent))) {
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
          /* load answers from the trie */
	  UNLOCK_SG_FR(sg_fr);
	  if(TrNode_child(ans_node) != NULL) {
	    store_loader_node(tab_ent, ans_node);
	  }
          PREG = (yamop *) CPREG;
          PREFETCH_OP(PREG);
          load_answer(ans_node, YENV);
	  YENV = ENV;
          GONext();
	} else {
	  /* execute compiled code from the trie */
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
	  if (SgFr_sg_ent_state(sg_fr) < compiled)
#else
	  if (SgFr_state(sg_fr) < compiled)
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
	    update_answer_trie(sg_fr);
	  UNLOCK_SG_FR(sg_fr);
	  PREG = (yamop *) TrNode_child(SgFr_answer_trie(sg_fr));
	  PREFETCH_OP(PREG);
	  *--YENV = 0;  /* vars_arity */
	  *--YENV = 0;  /* heap_arity */
	  GONext();
	}
      }
    }
  ENDPBOp();



/************************************************************************
**                            table_try_me                             **
************************************************************************/

  PBOp(table_try_me, Otapl)
    tab_ent_ptr tab_ent;
    sg_fr_ptr sg_fr;

    check_trail(TR);
    tab_ent = PREG->u.Otapl.te;
    YENV2MEM;
    sg_fr = subgoal_search(PREG, YENV_ADDRESS);
    MEM2YENV;
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
    if (SgFr_state(sg_fr) <= ready) {
      LOCK_SG_FR(sg_fr);
      if (SgFr_sg_ent_state(sg_fr) >= complete)
	SgFr_state(sg_fr) = SgFr_sg_ent_state(sg_fr);
      else
	SgFr_active_workers(sg_fr)++;
      UNLOCK_SG_FR(sg_fr);
    }
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
    LOCK_SG_FR(sg_fr);
#ifdef THREADS_CONSUMER_SHARING
    if (SgFr_state(sg_fr) == ready_external) {
      init_subgoal_frame(sg_fr);
      UNLOCK_SG_FR(sg_fr);
      store_generator_consumer_node(tab_ent, sg_fr, TRUE, PREG->u.Otapl.s);
      PREFETCH_OP(PREG);
      allocate_environment();
      check_for_deadlock(sg_fr);
      goto answer_resolution_completion;
    } else
#endif /* THREADS_CONSUMER_SHARING */
    if (SgFr_state(sg_fr) == ready) {
      /* subgoal new */
      init_subgoal_frame(sg_fr);
      UNLOCK_SG_FR(sg_fr);
      store_generator_node(tab_ent, sg_fr, PREG->u.Otapl.s, PREG->u.Otapl.d);
      PREG = NEXTOP(PREG, Otapl);
      PREFETCH_OP(PREG);
      allocate_environment();
      GONext();
#ifdef INCOMPLETE_TABLING
    } else if (SgFr_state(sg_fr) == incomplete) {
      /* subgoal incomplete --> start by loading the answers already found */
      ans_node_ptr ans_node = SgFr_first_answer(sg_fr);
      CELL *subs_ptr = YENV;
      init_subgoal_frame(sg_fr);
      UNLOCK_SG_FR(sg_fr);
      SgFr_try_answer(sg_fr) = ans_node;
      store_generator_node(tab_ent, sg_fr, PREG->u.Otapl.s, TRY_ANSWER);
      PREG = (yamop *) CPREG;
      PREFETCH_OP(PREG);
      load_answer(ans_node, subs_ptr);
      YENV = ENV;
      GONext();
#endif /* INCOMPLETE_TABLING */
    } else if (SgFr_state(sg_fr) == evaluating) {
      /* subgoal in evaluation */
      choiceptr leader_cp;
      int leader_dep_on_stack;
      find_dependency_node(sg_fr, leader_cp, leader_dep_on_stack);
      UNLOCK_SG_FR(sg_fr);
      find_leader_node(leader_cp, leader_dep_on_stack);
      store_consumer_node(tab_ent, sg_fr, leader_cp, leader_dep_on_stack);
#ifdef DEBUG_OPTYAP
      if (GLOBAL_parallel_mode == PARALLEL_MODE_RUNNING) {
	choiceptr aux_cp;
	aux_cp = B;
	while (YOUNGER_CP(aux_cp, Get_LOCAL_top_cp_on_stack()))
	  aux_cp = aux_cp->cp_b;
	OPTYAP_ERROR_CHECKING(table_try_me, aux_cp->cp_or_fr != DepFr_top_or_fr(LOCAL_top_dep_fr));
	aux_cp = B;
	while (YOUNGER_CP(aux_cp, DepFr_leader_cp(LOCAL_top_dep_fr)))
	  aux_cp = aux_cp->cp_b;
	OPTYAP_ERROR_CHECKING(table_try_me, aux_cp != DepFr_leader_cp(LOCAL_top_dep_fr));
      }
#endif /* DEBUG_OPTYAP */
      goto answer_resolution;
    } else {
      /* subgoal completed */
      ans_node_ptr ans_node = SgFr_first_answer(sg_fr);
      if (ans_node == NULL) {
	/* no answers --> fail */
	UNLOCK_SG_FR(sg_fr);
	goto fail;
      } else if (ans_node == SgFr_answer_trie(sg_fr)) {
	/* yes answer --> procceed */
	UNLOCK_SG_FR(sg_fr);
	PREG = (yamop *) CPREG;
	PREFETCH_OP(PREG);
	YENV = ENV;
	GONext();
      } else {
	/* answers -> get first answer */
#ifdef LIMIT_TABLING
	if (SgFr_state(sg_fr) == complete || SgFr_state(sg_fr) == compiled) {
	  SgFr_state(sg_fr)++;  /* complete --> complete_in_use : compiled --> compiled_in_use */
	  remove_from_global_sg_fr_list(sg_fr);
	  TRAIL_FRAME(sg_fr);
	}
#endif /* LIMIT_TABLING */
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
	if (IsMode_LoadAnswers(TabEnt_mode(tab_ent)) || SgFr_active_workers(sg_fr) > 0) {
#else
        if (IsMode_LoadAnswers(TabEnt_mode(tab_ent))) {
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
          /* load answers from the trie */
	  UNLOCK_SG_FR(sg_fr);
	  if(TrNode_child(ans_node) != NULL) {
	    store_loader_node(tab_ent, ans_node);
	  }
          PREG = (yamop *) CPREG;
          PREFETCH_OP(PREG);
          load_answer(ans_node, YENV);
	  YENV = ENV;
          GONext();
	} else {
	  /* execute compiled code from the trie */
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
	  if (SgFr_sg_ent_state(sg_fr) < compiled)
#else
	  if (SgFr_state(sg_fr) < compiled)
#endif /*THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING*/
	    update_answer_trie(sg_fr);
	  UNLOCK_SG_FR(sg_fr);
	  PREG = (yamop *) TrNode_child(SgFr_answer_trie(sg_fr));
	  PREFETCH_OP(PREG);
	  *--YENV = 0;  /* vars_arity */
	  *--YENV = 0;  /* heap_arity */
	  GONext();
	}
      }
    }
  ENDPBOp();



/************************************************************************
**                             table_try                               **
************************************************************************/

  PBOp(table_try, Otapl)
    tab_ent_ptr tab_ent;
    sg_fr_ptr sg_fr;

    check_trail(TR);
    tab_ent = PREG->u.Otapl.te;
    YENV2MEM;
    sg_fr = subgoal_search(PREG, YENV_ADDRESS);
    MEM2YENV;
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
    if (SgFr_state(sg_fr) <= ready) {
      LOCK_SG_FR(sg_fr);
      if (SgFr_sg_ent_state(sg_fr) >= complete)
	SgFr_state(sg_fr) = SgFr_sg_ent_state(sg_fr);
      else
	SgFr_active_workers(sg_fr)++;
      UNLOCK_SG_FR(sg_fr);
    }
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
    LOCK_SG_FR(sg_fr);
#ifdef THREADS_CONSUMER_SHARING
    if (SgFr_state(sg_fr) == ready_external) {
      init_subgoal_frame(sg_fr);
      UNLOCK_SG_FR(sg_fr);
      store_generator_consumer_node(tab_ent, sg_fr, TRUE , PREG->u.Otapl.s);
      PREFETCH_OP(PREG);
      allocate_environment();
      check_for_deadlock(sg_fr);
      goto answer_resolution_completion;
    } else
#endif /* THREADS_CONSUMER_SHARING */
    if (SgFr_state(sg_fr) == ready) {
      /* subgoal new */
      init_subgoal_frame(sg_fr);
      UNLOCK_SG_FR(sg_fr);
      store_generator_node(tab_ent, sg_fr, PREG->u.Otapl.s, NEXTOP(PREG,Otapl));
      PREG = PREG->u.Otapl.d;
      PREFETCH_OP(PREG);
      allocate_environment();
      GONext();
#ifdef INCOMPLETE_TABLING
    } else if (SgFr_state(sg_fr) == incomplete) {
      /* subgoal incomplete --> start by loading the answers already found */
      ans_node_ptr ans_node = SgFr_first_answer(sg_fr);
      CELL *subs_ptr = YENV;
      init_subgoal_frame(sg_fr);
      UNLOCK_SG_FR(sg_fr);
      SgFr_try_answer(sg_fr) = ans_node;
      store_generator_node(tab_ent, sg_fr, PREG->u.Otapl.s, TRY_ANSWER);
      PREG = (yamop *) CPREG;
      PREFETCH_OP(PREG);
      load_answer(ans_node, subs_ptr);
      YENV = ENV;
      GONext();
#endif /* INCOMPLETE_TABLING */
    } else if (SgFr_state(sg_fr) == evaluating) {
      /* subgoal in evaluation */
      choiceptr leader_cp;
      int leader_dep_on_stack;
      find_dependency_node(sg_fr, leader_cp, leader_dep_on_stack);
      UNLOCK_SG_FR(sg_fr);
      find_leader_node(leader_cp, leader_dep_on_stack);
      store_consumer_node(tab_ent, sg_fr, leader_cp, leader_dep_on_stack);
#ifdef DEBUG_OPTYAP
      if (GLOBAL_parallel_mode == PARALLEL_MODE_RUNNING) {
	choiceptr aux_cp;
	aux_cp = B;
	while (YOUNGER_CP(aux_cp, Get_LOCAL_top_cp_on_stack()))
	  aux_cp = aux_cp->cp_b;
	OPTYAP_ERROR_CHECKING(table_try, aux_cp->cp_or_fr != DepFr_top_or_fr(LOCAL_top_dep_fr));
	aux_cp = B;
	while (YOUNGER_CP(aux_cp, DepFr_leader_cp(LOCAL_top_dep_fr)))
	  aux_cp = aux_cp->cp_b;
	OPTYAP_ERROR_CHECKING(table_try, aux_cp != DepFr_leader_cp(LOCAL_top_dep_fr));
      }
#endif /* DEBUG_OPTYAP */
      goto answer_resolution;
    } else {
      /* subgoal completed */
      ans_node_ptr ans_node = SgFr_first_answer(sg_fr);
      if (ans_node == NULL) {
	/* no answers --> fail */
	UNLOCK_SG_FR(sg_fr);
	goto fail;
      } else if (ans_node == SgFr_answer_trie(sg_fr)) {
	/* yes answer --> procceed */
	UNLOCK_SG_FR(sg_fr);
	PREG = (yamop *) CPREG;
	PREFETCH_OP(PREG);
	YENV = ENV;
	GONext();
      } else {
        /* answers -> get first answer */
#ifdef LIMIT_TABLING
	if (SgFr_state(sg_fr) == complete || SgFr_state(sg_fr) == compiled) {
	  SgFr_state(sg_fr)++;  /* complete --> complete_in_use : compiled --> compiled_in_use */
	  remove_from_global_sg_fr_list(sg_fr);
	  TRAIL_FRAME(sg_fr);
	}
#endif /* LIMIT_TABLING */
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
	if (IsMode_LoadAnswers(TabEnt_mode(tab_ent)) || SgFr_active_workers(sg_fr) > 0) {
#else
        if (IsMode_LoadAnswers(TabEnt_mode(tab_ent))) {
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
          /* load answers from the trie */
	  UNLOCK_SG_FR(sg_fr);
	  if(TrNode_child(ans_node) != NULL) {
	    store_loader_node(tab_ent, ans_node);
	  }
          PREG = (yamop *) CPREG;
          PREFETCH_OP(PREG);
          load_answer(ans_node, YENV);
	  YENV = ENV;
          GONext();
	} else {
	  /* execute compiled code from the trie */
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
	  if (SgFr_sg_ent_state(sg_fr) < compiled)
#else
	  if (SgFr_state(sg_fr) < compiled)
#endif /*THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
	    update_answer_trie(sg_fr);
	  UNLOCK_SG_FR(sg_fr);
	  PREG = (yamop *) TrNode_child(SgFr_answer_trie(sg_fr));
	  PREFETCH_OP(PREG);
	  *--YENV = 0;  /* vars_arity */
	  *--YENV = 0;  /* heap_arity */
	  GONext();
	}
      }
    }
  ENDPBOp();



/************************************************************************
**                           table_retry_me                            **
************************************************************************/

  Op(table_retry_me, Otapl)
    restore_generator_node(PREG->u.Otapl.s, PREG->u.Otapl.d);
    YENV = (CELL *) PROTECT_FROZEN_B(B);
    set_cut(YENV, B->cp_b);
    SET_BB(NORM_CP(YENV));
    allocate_environment();
    PREG = NEXTOP(PREG,Otapl);
    GONext();
  ENDOp();



/************************************************************************
**                            table_retry                              **
************************************************************************/

  Op(table_retry, Otapl)
    restore_generator_node(PREG->u.Otapl.s, NEXTOP(PREG,Otapl));
    YENV = (CELL *) PROTECT_FROZEN_B(B);
    set_cut(YENV, B->cp_b);
    SET_BB(NORM_CP(YENV));
    allocate_environment();
    PREG = PREG->u.Otapl.d;
    GONext();
  ENDOp();



/************************************************************************
**                           table_trust_me                            **
************************************************************************/

  Op(table_trust_me, Otapl)
    restore_generator_node(PREG->u.Otapl.s, COMPLETION);
#ifdef DETERMINISTIC_TABLING
    if (B_FZ > B && IS_BATCHED_NORM_GEN_CP(B)) {   
      CELL *subs_ptr = (CELL *)(GEN_CP(B) + 1) + PREG->u.Otapl.s;
      choiceptr gcp = NORM_CP(DET_GEN_CP(subs_ptr) - 1);
      sg_fr_ptr sg_fr = GEN_CP(B)->cp_sg_fr; 
      DET_GEN_CP(gcp)->cp_sg_fr = sg_fr;         
      gcp->cp_h     = B->cp_h;
#ifdef DEPTH_LIMIT
      gcp->cp_depth = B->cp_depth;
#endif /* DEPTH_LIMIT */
      gcp->cp_tr    = B->cp_tr;
      gcp->cp_b     = B->cp_b;
      gcp->cp_ap    = B->cp_ap;
      SgFr_gen_cp(sg_fr) = B = gcp;       
    }
#endif /* DETERMINISTIC_TABLING */
    YENV = (CELL *) PROTECT_FROZEN_B(B);
    set_cut(YENV, B->cp_b);
    SET_BB(NORM_CP(YENV));
    allocate_environment();
    PREG = NEXTOP(PREG,Otapl);
    GONext();
  ENDOp();



/************************************************************************
**                            table_trust                              **
************************************************************************/

  Op(table_trust, Otapl)
    restore_generator_node(PREG->u.Otapl.s, COMPLETION);
#ifdef DETERMINISTIC_TABLING
  if (B_FZ > B && IS_BATCHED_NORM_GEN_CP(B)) {    
      CELL *subs_ptr = (CELL *)(GEN_CP(B) + 1) + PREG->u.Otapl.s;
      choiceptr gcp = NORM_CP(DET_GEN_CP(subs_ptr) - 1);
      sg_fr_ptr sg_fr = GEN_CP(B)->cp_sg_fr; 
      DET_GEN_CP(gcp)->cp_sg_fr = sg_fr;         
      gcp->cp_h     = B->cp_h;
#ifdef DEPTH_LIMIT
      gcp->cp_depth = B->cp_depth;
#endif /* DEPTH_LIMIT */
      gcp->cp_tr    = B->cp_tr;
      gcp->cp_b     = B->cp_b;
      gcp->cp_ap    = B->cp_ap;
      SgFr_gen_cp(sg_fr) = B = gcp;
    }
#endif /* DETERMINISTIC_TABLING */
    YENV = (CELL *) PROTECT_FROZEN_B(B);
    set_cut(YENV, B->cp_b);
    SET_BB(NORM_CP(YENV));
    allocate_environment();
    PREG = PREG->u.Otapl.d;
    GONext();
  ENDOp();



/************************************************************************
**                          table_new_answer                           **
************************************************************************/

  PBOp(table_new_answer, s)
    CELL *subs_ptr;
    choiceptr gcp;
    sg_fr_ptr sg_fr;
    ans_node_ptr ans_node;

    gcp = NORM_CP(YENV[E_B]);
#ifdef DETERMINISTIC_TABLING
    if (IS_DET_GEN_CP(gcp)){  
      sg_fr = DET_GEN_CP(gcp)->cp_sg_fr;
      subs_ptr = (CELL *)(DET_GEN_CP(gcp) + 1) ; 
    } else
#endif /* DETERMINISTIC_TABLING */
    {
      sg_fr = GEN_CP(gcp)->cp_sg_fr;
      subs_ptr = (CELL *)(GEN_CP(gcp) + 1) + PREG->u.s.s;
    }
#if defined(DEBUG_TABLING) && !defined(DETERMINISTIC_TABLING)
    {
      int i, j, arity_args, arity_subs;
      CELL *aux_args;
      CELL *aux_subs;

      arity_args = PREG->u.s.s;
      arity_subs = *subs_ptr;
      aux_args = (CELL *)(GEN_CP(gcp) + 1);
      aux_subs = subs_ptr;
      for (i = 1; i <= arity_subs; i++) {
        Term term_subs = Deref(*(aux_subs + i));
        for (j = 0; j < arity_args; j++) {
          Term term_arg = Deref(*(aux_args + j));
          if (term_subs == term_arg) break;
	}
	TABLING_ERROR_CHECKING(table_new_answer, j == arity_args);
      }
    }
#endif /* DEBUG_TABLING && !DETERMINISTIC_TABLING */
    LOCK_ANSWER_TRIE(sg_fr);
#ifdef MODE_DIRECTED_TABLING
    if (SgFr_mode_directed(sg_fr)) {
      ans_node = mode_directed_answer_search(sg_fr, subs_ptr);
      if (ans_node == NULL) {
	/* no answer inserted */
	UNLOCK_ANSWER_TRIE(sg_fr);
	goto fail;
      }
    } else
#endif /* MODE_DIRECTED_TABLING */
      ans_node = answer_search(sg_fr, subs_ptr);
    LOCK_ANSWER_NODE(ans_node);
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
            for (i = 0; i < GLOBAL_number_workers; i++) {
              if (BITMAP_member(members, i) && 
                  BRANCH_LTT(i, depth) > ltt && 
                  EQUAL_OR_YOUNGER_CP(Get_LOCAL_top_cp(), REMOTE_pruning_scope(i))) {
                leftmost_or_fr = LOCAL_top_or_fr;
  pending_table_new_answer:
		UNLOCK_ANSWER_NODE(ans_node);
                UNLOCK_ANSWER_TRIE(sg_fr);
                LOCK_OR_FRAME(leftmost_or_fr);
                if (Get_LOCAL_prune_request()) {
                  UNLOCK_OR_FRAME(leftmost_or_fr);
                  SCHEDULER_GET_WORK();
                } else {
                  CUT_store_tg_answer(leftmost_or_fr, ans_node, gcp, ltt);
                  UNLOCK_OR_FRAME(leftmost_or_fr);
                }
		if (IS_BATCHED_GEN_CP(gcp)) {
                  /* deallocate and procceed */
                  PREG = (yamop *) YENV[E_CP];
                  PREFETCH_OP(PREG);
                  CPREG = PREG;
                  SREG = YENV;
                  ENV = YENV = (CELL *) YENV[E_E];
#ifdef DEPTH_LIMIT
		  DEPTH = YENV[E_DEPTH];
#endif /* DEPTH_LIMIT */
                  GONext();
		} else {
                  /* fail */
                  goto fail;
		}
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
              for (i = 0; i < GLOBAL_number_workers; i++) {
                if (BITMAP_member(members, i) &&
                    BRANCH_LTT(i, depth) > ltt &&
                    EQUAL_OR_YOUNGER_CP(GetOrFr_node(leftmost_or_fr), REMOTE_pruning_scope(i)))
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
                for (i = 0; i < GLOBAL_number_workers; i++) {
                  if (BITMAP_member(members, i) &&
                      BRANCH_LTT(i, depth) > ltt &&
                      EQUAL_OR_YOUNGER_CP(GetOrFr_node(leftmost_or_fr), REMOTE_pruning_scope(i))) {
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
      if (Get_LOCAL_prune_request()) {
	UNLOCK_ANSWER_NODE(ans_node);
	UNLOCK_ANSWER_TRIE(sg_fr);
        SCHEDULER_GET_WORK();
      }
#endif /* TABLING_INNER_CUTS */
      TAG_AS_ANSWER_LEAF_NODE(ans_node);
#ifdef THREADS_FULL_SHARING
      INFO_THREADS("new answer  (1)  sgfr=%p ans_node=%p",SgFr_sg_ent(sg_fr),ans_node);
      if (IsMode_Batched(TabEnt_mode(SgFr_tab_ent(sg_fr)))) {
	ANSWER_LEAF_NODE_INSTR_RELATIVE(ans_node);
	if (worker_id < ANSWER_LEAF_NODE_MAX_THREADS)
	  ANSWER_LEAF_NODE_SET_WID(ans_node,worker_id);
      }
#endif /* THREADS_FULL_SHARING */
      UNLOCK_ANSWER_NODE(ans_node);
#ifndef ANSWER_TRIE_LOCK_AT_ENTRY_LEVEL
      LOCK_SG_FR(sg_fr);
#endif /* ! ANSWER_TRIE_LOCK_AT_ENTRY_LEVEL */
      if (SgFr_first_answer(sg_fr) == NULL)
	SgFr_first_answer(sg_fr) = ans_node;
      else
        TrNode_child(SgFr_last_answer(sg_fr)) = ans_node;
      SgFr_last_answer(sg_fr) = ans_node;
#ifdef DEBUG_TABLING
      { 
        ans_node_ptr aux_ans_node = SgFr_first_answer(sg_fr);
        while (aux_ans_node != SgFr_last_answer(sg_fr)) {
	  TABLING_ERROR_CHECKING(table_new_answer, !IS_ANSWER_LEAF_NODE(aux_ans_node));
          aux_ans_node = TrNode_child(aux_ans_node);
        }
      }
#endif /* DEBUG_TABLING */
      UNLOCK_SG_FR(sg_fr);
      if (IS_BATCHED_GEN_CP(gcp)) {
#ifdef THREADS_FULL_SHARING
        if (worker_id >= ANSWER_LEAF_NODE_MAX_THREADS)
          SgFr_batched_cached_answers_check_insert(sg_fr,ans_node); //add to buffer all answers except the ans_node
#endif /* THREADS_FULL_SHARING */
#ifdef TABLING_EARLY_COMPLETION
	if (gcp == PROTECT_FROZEN_B(B) && (*subs_ptr == 0 || gcp->cp_ap == COMPLETION)) {
	  /* if the current generator choice point is the topmost choice point and the current */
	  /* call is deterministic (i.e., the number of substitution variables is zero or      */
	  /* there are no more alternatives) then the current answer is deterministic and we   */
	  /* can perform an early completion and remove the current generator choice point     */
	  private_completion(sg_fr);
	  B = B->cp_b;
	  SET_BB(PROTECT_FROZEN_B(B));
	} else if (*subs_ptr == 0) {
	  /* if the number of substitution variables is zero, an answer is sufficient to perform */
          /* an early completion, but the current generator choice point cannot be removed       */
	  mark_as_completed(sg_fr);
	  if (gcp->cp_ap != NULL)
	    gcp->cp_ap = COMPLETION;
	}
#endif /* TABLING_EARLY_COMPLETION */
        /* deallocate and procceed */
        PREG = (yamop *) YENV[E_CP];
        PREFETCH_OP(PREG);
        CPREG = PREG;
        SREG = YENV;
        ENV = YENV = (CELL *) YENV[E_E];
#ifdef DEPTH_LIMIT
	DEPTH = YENV[E_DEPTH];
#endif /* DEPTH_LIMIT */
        GONext();
      } else {
#ifdef TABLING_EARLY_COMPLETION
	if (*subs_ptr == 0) {
	  /* if the number of substitution variables is zero, an answer is sufficient to perform */
          /* an early completion, but the current generator choice point cannot be removed       */
	  mark_as_completed(sg_fr);
	  if (gcp->cp_ap != ANSWER_RESOLUTION)
	    gcp->cp_ap = COMPLETION;
	}
#endif /* TABLING_EARLY_COMPLETION */
        /* fail */
        goto fail;
      }
    } else {
      /* repeated answer */
#ifdef THREADS_FULL_SHARING
      if (IsMode_Batched(TabEnt_mode(SgFr_tab_ent(sg_fr)))){
	if (worker_id >= ANSWER_LEAF_NODE_MAX_THREADS) {
	  UNLOCK_ANSWER_NODE(ans_node);
	  UNLOCK_ANSWER_TRIE(sg_fr);
	  SgFr_batched_cached_answers_check_insert(sg_fr,NULL); 
	  INFO_THREADS("new      answer  (2)   sgfr=%p ans_node=%p",SgFr_sg_ent(sg_fr),ans_node);
	  if (SgFr_batched_cached_answers_check_remove(sg_fr , ans_node) == 1){
	    INFO_THREADS("ans_node=%p not found", ans_node);
	    goto fail;
	  }
	  /* deallocate and procceed */
	  PREG = (yamop *) YENV[E_CP];
	  PREFETCH_OP(PREG);
	  CPREG = PREG;
	  SREG = YENV;
	  ENV = YENV = (CELL *) YENV[E_E];
#ifdef DEPTH_LIMIT
	  DEPTH = YENV[E_DEPTH];
#endif  /*DEPTH_LIMIT */
	  GONext();
	} else {
	  if (!ANSWER_LEAF_NODE_CHECK_WID(ans_node,worker_id)){
	    ANSWER_LEAF_NODE_SET_WID(ans_node,worker_id);
	    UNLOCK_ANSWER_NODE(ans_node);
	    UNLOCK_ANSWER_TRIE(sg_fr);
	    /* deallocate and procceed */
	    INFO_THREADS("new      answer  (2)  sgfr=%p ans_node=%p",SgFr_sg_ent(sg_fr),ans_node);
	    PREG = (yamop *) YENV[E_CP];
	    PREFETCH_OP(PREG);
	    CPREG = PREG;
	    SREG = YENV;
	    ENV = YENV = (CELL *) YENV[E_E];
#ifdef DEPTH_LIMIT
	    DEPTH = YENV[E_DEPTH];
#endif  /*DEPTH_LIMIT */
	    GONext();
	  }
	}
      }
#else
      UNLOCK_ANSWER_NODE(ans_node);
      UNLOCK_ANSWER_TRIE(sg_fr);
#endif /* THREADS_FULL_SHARING */
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
      INFO_THREADS("new      answer(rep)  sgfr=%p ans_node=%p",SgFr_sg_ent(sg_fr),ans_node);
#else
      INFO_THREADS("new      answer(rep)  sgfr=%p ans_node=%p",sg_fr,ans_node);
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
      goto fail;
    }
  ENDPBOp();



/************************************************************************
**                      table_answer_resolution                        **
************************************************************************/

  BOp(table_answer_resolution, Otapl)
#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
      UNLOCK_OR_FRAME(LOCAL_top_or_fr);
    }
#endif /* YAPOR */


  answer_resolution:
    INIT_PREFETCH()
    dep_fr_ptr dep_fr;
    ans_node_ptr ans_node;

    OPTYAP_ERROR_CHECKING(answer_resolution, SCH_top_shared_cp(B) && B->cp_or_fr->alternative != ANSWER_RESOLUTION);
    OPTYAP_ERROR_CHECKING(answer_resolution, !SCH_top_shared_cp(B) && B->cp_ap != ANSWER_RESOLUTION);
    dep_fr = CONS_CP(B)->cp_dep_fr;
    LOCK_DEP_FR(dep_fr);
    ans_node = DepFr_last_answer(dep_fr);
    if (TrNode_child(ans_node)) {
      /* unconsumed answers */
#ifdef MODE_DIRECTED_TABLING
      if (IS_ANSWER_INVALID_NODE(TrNode_child(ans_node))) {
	ans_node_ptr old_ans_node;
	old_ans_node = ans_node;
	ans_node = TrNode_child(ans_node);
	do {
	  ans_node = TrNode_child(ans_node);
	} while (IS_ANSWER_INVALID_NODE(ans_node));
	TrNode_child(old_ans_node) = ans_node;
      } else
#endif /* MODE_DIRECTED_TABLING */
	ans_node = TrNode_child(ans_node);
      DepFr_last_answer(dep_fr) = ans_node;
      UNLOCK_DEP_FR(dep_fr);
      consume_answer_and_procceed(dep_fr, ans_node);
    }
    UNLOCK_DEP_FR(dep_fr);

#ifdef YAPOR
    if (B == DepFr_leader_cp(LOCAL_top_dep_fr)) {
      /*  B is a generator-consumer node  **
      ** never here if batched scheduling */
      TABLING_ERROR_CHECKING(answer_resolution, IS_BATCHED_GEN_CP(B));
      goto completion;
    }
#endif /* YAPOR */

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
      TABLING_ERROR_CHECKING(answer_resolution, EQUAL_OR_YOUNGER_CP(top_chain_cp, B));
      TABLING_ERROR_CHECKING(answer_resolution, EQUAL_OR_YOUNGER_CP(chain_cp, B));

      /* check for dependency frames with unconsumed answers */
      dep_fr = DepFr_next(dep_fr);
      while (YOUNGER_CP(DepFr_cons_cp(dep_fr), chain_cp)) {
        LOCK_DEP_FR(dep_fr);
        ans_node = DepFr_last_answer(dep_fr);
	if (TrNode_child(ans_node)) {
          /* dependency frame with unconsumed answers */
#ifdef MODE_DIRECTED_TABLING
	  if (IS_ANSWER_INVALID_NODE(TrNode_child(ans_node))) {
	    ans_node_ptr old_ans_node;
	    old_ans_node = ans_node;
	    ans_node = TrNode_child(ans_node);
	    do {
	      ans_node = TrNode_child(ans_node);
	    } while (IS_ANSWER_INVALID_NODE(ans_node));
	    TrNode_child(old_ans_node) = ans_node;
	  } else
#endif /* MODE_DIRECTED_TABLING */
	    ans_node = TrNode_child(ans_node);
          DepFr_last_answer(dep_fr) = ans_node;
#ifdef YAPOR
          if (YOUNGER_CP(DepFr_backchain_cp(dep_fr), top_chain_cp))
#endif /* YAPOR */
            DepFr_backchain_cp(dep_fr) = top_chain_cp;
          UNLOCK_DEP_FR(dep_fr);

          chain_cp = DepFr_cons_cp(dep_fr);
#ifdef YAPOR
          /* update shared nodes */
          start_or_fr = LOCAL_top_or_fr;
          end_or_fr = DepFr_top_or_fr(dep_fr);
          if (start_or_fr != end_or_fr) {
            LOCAL_top_or_fr = end_or_fr;
            Set_LOCAL_top_cp(GetOrFr_node(end_or_fr));
            do {
              while (YOUNGER_CP(GetOrFr_node(start_or_fr), GetOrFr_node(end_or_fr))) {
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
              while (YOUNGER_CP(GetOrFr_node(end_or_fr), GetOrFr_node(start_or_fr))) {
                LOCK_OR_FRAME(end_or_fr);
                BITMAP_insert(OrFr_members(end_or_fr), worker_id);
                BRANCH(worker_id, OrFr_depth(end_or_fr)) = 1;
                UNLOCK_OR_FRAME(end_or_fr);
                end_or_fr = OrFr_next(end_or_fr);
	      }
    	    } while (start_or_fr != end_or_fr);
            if (Get_LOCAL_prune_request())
              pruning_over_tabling_data_structures(); 	
          }
#endif /* YAPOR */
#ifdef DEBUG_OPTYAP
	  if (GLOBAL_parallel_mode == PARALLEL_MODE_RUNNING) {
	    choiceptr aux_cp;
	    OPTYAP_ERROR_CHECKING(completion, YOUNGER_CP(Get_LOCAL_top_cp(), Get_LOCAL_top_cp_on_stack()));
	    aux_cp = chain_cp;
	    while (aux_cp != Get_LOCAL_top_cp()) {
	      OPTYAP_ERROR_CHECKING(completion, YOUNGER_CP(Get_LOCAL_top_cp(), aux_cp));
	      OPTYAP_ERROR_CHECKING(completion, EQUAL_OR_YOUNGER_CP(Get_LOCAL_top_cp_on_stack(), aux_cp));
	      aux_cp = aux_cp->cp_b;
	    }
	  }
#endif /* DEBUG_OPTYAP */
          /* restore bindings, update registers, consume answer and procceed */
          restore_bindings(B->cp_tr, chain_cp->cp_tr);
	  TABLING_ERROR_CHECKING(answer_resolution, TR != B->cp_tr && !IsPairTerm((CELL)TrailTerm(TR - 1)));
	  TABLING_ERROR_CHECKING(answer_resolution, TR != B->cp_tr && (tr_fr_ptr) RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr);
          B = chain_cp;
          TR = TR_FZ;
          TRAIL_LINK(B->cp_tr);
          consume_answer_and_procceed(dep_fr, ans_node);
	}
        UNLOCK_DEP_FR(dep_fr);
        dep_fr = DepFr_next(dep_fr);
      }

      /* no dependency frames with unconsumed answers found */
#ifdef YAPOR
      /* update shared nodes */
      if (EQUAL_OR_YOUNGER_CP(Get_LOCAL_top_cp_on_stack(), chain_cp)) {
        end_or_fr = chain_cp->cp_or_fr;
        start_or_fr = LOCAL_top_or_fr;
        if (start_or_fr != end_or_fr) {
          LOCAL_top_or_fr = end_or_fr;
          Set_LOCAL_top_cp(GetOrFr_node(end_or_fr));
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
          if (Get_LOCAL_prune_request())
            pruning_over_tabling_data_structures(); 
        }
      }
#endif /* YAPOR */
#ifdef DEBUG_OPTYAP
      if (GLOBAL_parallel_mode == PARALLEL_MODE_RUNNING) {
	choiceptr aux_cp;
	OPTYAP_ERROR_CHECKING(completion, YOUNGER_CP(Get_LOCAL_top_cp(), Get_LOCAL_top_cp_on_stack()));
	aux_cp = chain_cp;
	while (aux_cp != Get_LOCAL_top_cp()) {
	  OPTYAP_ERROR_CHECKING(completion, YOUNGER_CP(Get_LOCAL_top_cp(), aux_cp));
	  OPTYAP_ERROR_CHECKING(completion, EQUAL_OR_YOUNGER_CP(Get_LOCAL_top_cp_on_stack(), aux_cp));
	  aux_cp = aux_cp->cp_b;
	}
      }
#endif /* DEBUG_OPTYAP */
      /* unbind variables */
      unbind_variables(B->cp_tr, chain_cp->cp_tr);
      TABLING_ERROR_CHECKING(answer_resolution, TR != B->cp_tr && !IsPairTerm((CELL)TrailTerm(TR - 1)));
      TABLING_ERROR_CHECKING(answer_resolution, TR != B->cp_tr && (tr_fr_ptr) RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr);
      if (DepFr_leader_cp(LOCAL_top_dep_fr) == chain_cp && (
        /* chain_cp is a leader node AND ... */
#ifdef YAPOR
        /* the leader dependency is not on stack OR ... */
        DepFr_leader_dep_is_on_stack(LOCAL_top_dep_fr) == FALSE ||
        /* the leader dependency is on stack (this means that chain_cp is a generator node) and */
#endif /* YAPOR */
        /*                 there are no unexploited alternatives                 **
        ** (NULL if batched scheduling OR ANSWER_RESOLUTION if local scheduling) */
        chain_cp->cp_ap == NULL || chain_cp->cp_ap == ANSWER_RESOLUTION)) {
        B = chain_cp;
        TR = TR_FZ;
        TRAIL_LINK(B->cp_tr);
        goto completion;
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



/************************************************************************
**                          table_completion                           **
************************************************************************/

  BOp(table_completion, Otapl)
#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
      if (IS_BATCHED_GEN_CP(B)) {
        SCH_new_alternative(PREG, NULL);
        if (B != DepFr_leader_cp(LOCAL_top_dep_fr) && EQUAL_OR_YOUNGER_CP(B_FZ, B)) {
          /* not leader on that node */
          SCHEDULER_GET_WORK();
        }
      } else {
        SCH_new_alternative(PREG, ANSWER_RESOLUTION);
        if (B != DepFr_leader_cp(LOCAL_top_dep_fr)) {
          /* not leader on that node */
          SCHEDULER_GET_WORK();
        }
      }
    } else
#endif /* YAPOR */
    {

#ifdef THREADS_CONSUMER_SHARING
    goto answer_resolution_completion;
#endif /* THREADS_CONSUMER_SHARING */

      if (IS_BATCHED_GEN_CP(B)) {
        B->cp_ap = NULL;
        if (EQUAL_OR_YOUNGER_CP(B_FZ, B) && B != DepFr_leader_cp(LOCAL_top_dep_fr)) {
          /* not leader on that node */
          B = B->cp_b;
          goto fail;
        }
      } else {
        B->cp_ap = ANSWER_RESOLUTION;
        if (B != DepFr_leader_cp(LOCAL_top_dep_fr)) {
          /* not leader on that node */
          B = B->cp_b;
          goto fail;
        }
      }
    }
    /* leader on that node */


  completion:
#ifdef THREADS_CONSUMER_SHARING
  goto answer_resolution_completion;
#endif /* THREADS_CONSUMER_SHARING */
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
      LOCK_DEP_FR(dep_fr);
      ans_node = DepFr_last_answer(dep_fr);
      if (TrNode_child(ans_node)) {
        /* dependency frame with unconsumed answers */
#ifdef MODE_DIRECTED_TABLING
	if (IS_ANSWER_INVALID_NODE(TrNode_child(ans_node))) {
	  ans_node_ptr old_ans_node;
	  old_ans_node = ans_node;
	  ans_node = TrNode_child(ans_node);
	  do {
	    ans_node = TrNode_child(ans_node);
	  } while (IS_ANSWER_INVALID_NODE(ans_node));
	  TrNode_child(old_ans_node) = ans_node;
	} else
#endif /* MODE_DIRECTED_TABLING */
	  ans_node = TrNode_child(ans_node);
        DepFr_last_answer(dep_fr) = ans_node;
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
        UNLOCK_DEP_FR(dep_fr);

#ifdef DEBUG_OPTYAP
        if (GLOBAL_parallel_mode == PARALLEL_MODE_RUNNING) {
	  choiceptr aux_cp;
	  OPTYAP_ERROR_CHECKING(completion, Get_LOCAL_top_cp(), Get_LOCAL_top_cp_on_stack());
	  aux_cp = DepFr_cons_cp(dep_fr);
	  while (YOUNGER_CP(aux_cp, Get_LOCAL_top_cp_on_stack()))
	    aux_cp = aux_cp->cp_b;
	  OPTYAP_ERROR_CHECKING(completion, aux_cp->cp_or_fr != DepFr_top_or_fr(dep_fr));
	}
#endif /* DEBUG_OPTYAP */
#ifdef YAPOR
        /* update shared nodes */
        if (YOUNGER_CP(Get_LOCAL_top_cp_on_stack(), Get_LOCAL_top_cp())) {
          or_fr_ptr or_frame = DepFr_top_or_fr(dep_fr);
          while (or_frame != LOCAL_top_or_fr) {
            LOCK_OR_FRAME(or_frame);
            BITMAP_insert(OrFr_members(or_frame), worker_id);
            BRANCH(worker_id, OrFr_depth(or_frame)) = 1;
            UNLOCK_OR_FRAME(or_frame);
            or_frame = OrFr_next(or_frame);
          }
          LOCAL_top_or_fr = DepFr_top_or_fr(dep_fr);
          Set_LOCAL_top_cp(GetOrFr_node(LOCAL_top_or_fr));
        }
#endif /* YAPOR */
#ifdef DEBUG_OPTYAP
        if (GLOBAL_parallel_mode == PARALLEL_MODE_RUNNING) {
	  choiceptr aux_cp;
	  OPTYAP_ERROR_CHECKING(completion, YOUNGER_CP(Get_LOCAL_top_cp(), Get_LOCAL_top_cp_on_stack()));
	  aux_cp = DepFr_cons_cp(dep_fr);
	  while (aux_cp != Get_LOCAL_top_cp()) {
	    OPTYAP_ERROR_CHECKING(completion, YOUNGER_CP(Get_LOCAL_top_cp(), aux_cp));
	    OPTYAP_ERROR_CHECKING(completion, EQUAL_OR_YOUNGER_CP(Get_LOCAL_top_cp_on_stack(), aux_cp));
	    aux_cp = aux_cp->cp_b;
	  }
	}
#endif /* DEBUG_OPTYAP */
        /* rebind variables, update registers, consume answer and procceed */
	TABLING_ERROR_CHECKING(completion, EQUAL_OR_YOUNGER_CP(B, DepFr_cons_cp(dep_fr)));
	TABLING_ERROR_CHECKING(completion, B->cp_tr > DepFr_cons_cp(dep_fr)->cp_tr);
        rebind_variables(DepFr_cons_cp(dep_fr)->cp_tr, B->cp_tr);
	TABLING_ERROR_CHECKING(completion, TR != B->cp_tr && !IsPairTerm((CELL)TrailTerm(TR - 1)));
	TABLING_ERROR_CHECKING(completion, TR != B->cp_tr && (tr_fr_ptr) RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr);
        B = DepFr_cons_cp(dep_fr);
        TR = TR_FZ;
        if (TR != B->cp_tr)
          TRAIL_LINK(B->cp_tr);
        consume_answer_and_procceed(dep_fr, ans_node);
      }
      UNLOCK_DEP_FR(dep_fr);
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
          while (YOUNGER_CP(GetOrFr_node(LOCAL_top_susp_or_fr), Get_LOCAL_top_cp())) {
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
              rebind_variables(GetOrFr_node(susp_or_fr)->cp_tr, B->cp_tr);
              resume_suspension_frame(resume_fr, susp_or_fr);
              B = Get_LOCAL_top_cp();
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
        while (EQUAL_OR_YOUNGER_CP(GetOrFr_node(LOCAL_top_susp_or_fr), Get_LOCAL_top_cp())) {
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
            rebind_variables(GetOrFr_node(susp_or_fr)->cp_tr, B->cp_tr);
            resume_suspension_frame(resume_fr, susp_or_fr);
            B = Get_LOCAL_top_cp();
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
      TABLING_ERROR_CHECKING(completion, TR != B->cp_tr && !IsPairTerm((CELL)TrailTerm(TR - 1)));
      TABLING_ERROR_CHECKING(completion, TR != B->cp_tr && (tr_fr_ptr) RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr);
      if (B == DepFr_leader_cp(LOCAL_top_dep_fr)) {
        /*  B is a generator-consumer node  */
        /* never here if batched scheduling */
        ans_node_ptr ans_node;
	TABLING_ERROR_CHECKING(completion, IS_BATCHED_GEN_CP(B));
        TR = B->cp_tr;
        SET_BB(B);
        LOCK_OR_FRAME(LOCAL_top_or_fr);
        LOCK_DEP_FR(LOCAL_top_dep_fr);
        ans_node = DepFr_last_answer(LOCAL_top_dep_fr);
        if (TrNode_child(ans_node)) {
          /* unconsumed answers */
          UNLOCK_OR_FRAME(LOCAL_top_or_fr);
#ifdef MODE_DIRECTED_TABLING
	  if (IS_ANSWER_INVALID_NODE(TrNode_child(ans_node))) {
	    ans_node_ptr old_ans_node;
	    old_ans_node = ans_node;
	    ans_node = TrNode_child(ans_node);
	    do {
	      ans_node = TrNode_child(ans_node);
	    } while (IS_ANSWER_INVALID_NODE(ans_node));
	    TrNode_child(old_ans_node) = ans_node;
	  } else
#endif /* MODE_DIRECTED_TABLING */
	    ans_node = TrNode_child(ans_node);
          DepFr_last_answer(LOCAL_top_dep_fr) = ans_node;
          UNLOCK_DEP_FR(LOCAL_top_dep_fr);
          consume_answer_and_procceed(LOCAL_top_dep_fr, ans_node);
        }
        /* no unconsumed answers */
        UNLOCK_DEP_FR(LOCAL_top_dep_fr);
        if (OrFr_owners(LOCAL_top_or_fr) > 1) {
          /* more owners -> move up one node */
          Set_LOCAL_top_cp_on_stack(GetOrFr_node(OrFr_next_on_stack(LOCAL_top_or_fr)));
          BITMAP_delete(OrFr_members(LOCAL_top_or_fr), worker_id);
          OrFr_owners(LOCAL_top_or_fr)--;
          LOCAL_top_dep_fr = DepFr_next(LOCAL_top_dep_fr);
          UNLOCK_OR_FRAME(LOCAL_top_or_fr);
          if (LOCAL_top_sg_fr && Get_LOCAL_top_cp() == SgFr_gen_cp(LOCAL_top_sg_fr)) {
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
      /* goto getwork */
      PREG = B->cp_ap;
      PREFETCH_OP(PREG);
      TR = B->cp_tr;
      SET_BB(B);
      GONext();
    } else
#endif /* YAPOR */

#ifdef THREADS_CONSUMER_SHARING
complete_all:
#endif /* THREADS_CONSUMER_SHARING */
    {
      /* complete all */
      sg_fr_ptr sg_fr;

#ifdef DETERMINISTIC_TABLING
      if (IS_DET_GEN_CP(B))
	sg_fr = DET_GEN_CP(B)->cp_sg_fr;
      else	 
#endif /* DETERMINISTIC_TABLING */
	sg_fr = GEN_CP(B)->cp_sg_fr;
      private_completion(sg_fr);
#ifdef THREADS_CONSUMER_SHARING
      if (IS_BATCHED_GEN_CP(B) || SgFr_gen_worker(sg_fr) != worker_id) {  /* if it is an gen_cons node then all the answers were already consumed */
#else
      if (IS_BATCHED_GEN_CP(B)) {
#endif /*THREADS_CONSUMER_SHARING */
        /* backtrack */
        B = B->cp_b;
        SET_BB(PROTECT_FROZEN_B(B));
        goto fail;
      } else {
        /* subgoal completed */
	ans_node = SgFr_first_answer(sg_fr);
        if (ans_node == NULL) {
          /* no answers --> fail */
          B = B->cp_b;
          SET_BB(PROTECT_FROZEN_B(B));
          goto fail;
        }
	TABLING_ERROR_CHECKING(completion, TR != B->cp_tr && !IsPairTerm((CELL)TrailTerm(TR - 1)));
	TABLING_ERROR_CHECKING(completion, TR != B->cp_tr && (tr_fr_ptr) RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr);
        pop_generator_node(SgFr_arity(sg_fr));
        if (ans_node == SgFr_answer_trie(sg_fr)) {
          /* yes answer --> procceed */
          PREG = (yamop *) CPREG;
          PREFETCH_OP(PREG);
          YENV = ENV;
          GONext();
        } else  {
          /* answers -> get first answer */
	  tab_ent_ptr tab_ent = SgFr_tab_ent(sg_fr);
#ifdef LIMIT_TABLING
	  SgFr_state(sg_fr)++;  /* complete --> complete_in_use */
	  remove_from_global_sg_fr_list(sg_fr);
	  TRAIL_FRAME(sg_fr);
#endif /* LIMIT_TABLING */
	  if (IsMode_LoadAnswers(TabEnt_mode(tab_ent))) {
            /* load answers from the trie */
	    if(TrNode_child(ans_node) != NULL) {
	      store_loader_node(tab_ent, ans_node);
	    }
            PREG = (yamop *) CPREG;
            PREFETCH_OP(PREG);
            load_answer(ans_node, YENV);
	    YENV = ENV;
            GONext();
	  } else {
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
	    LOCK_SG_FR(sg_fr);
	    if (SgFr_active_workers(sg_fr) > 0) {
	      /* load answers from the trie */
	      UNLOCK_SG_FR(sg_fr);
	      if(TrNode_child(ans_node) != NULL) {
		store_loader_node(tab_ent, ans_node);
	      }
	      PREG = (yamop *) CPREG;
	      PREFETCH_OP(PREG);
	      load_answer(ans_node, YENV);
	      YENV = ENV;
	      GONext();
	    }
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
	    /* execute compiled code from the trie */
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
	    if (SgFr_sg_ent_state(sg_fr) < compiled)
#else
	    LOCK_SG_FR(sg_fr);
	    if (SgFr_state(sg_fr) < compiled)
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
	      update_answer_trie(sg_fr);
	    UNLOCK_SG_FR(sg_fr);
	    PREG = (yamop *) TrNode_child(SgFr_answer_trie(sg_fr));
	    PREFETCH_OP(PREG);
	    *--YENV = 0;  /* vars_arity */
            *--YENV = 0;  /* heap_arity */
	    GONext();
	  }
	}
      }
    }
    END_PREFETCH()
  ENDBOp();



/************************************************************************
**                 table_answer_resolution_completion                  **
************************************************************************/

#ifdef THREADS_CONSUMER_SHARING
  BOp(table_answer_resolution_completion, Otapl)
answer_resolution_completion:
{
   INIT_PREFETCH()
     int do_not_complete_tables;
     int wid = worker_id ;
     do {
       dep_fr_ptr dep_fr;
       ans_node_ptr ans_node;
     

       do_not_complete_tables = 0; /* 0 - complete all the tables 1 - do not complete all the tables */	
       if (B->cp_ap == ANSWER_RESOLUTION_COMPLETION ){
	 /* generator consumer node (external node) */
	 if ((IS_BATCHED_GEN_CP(B) && (EQUAL_OR_YOUNGER_CP(B_FZ, B) && B != DepFr_leader_cp(LOCAL_top_dep_fr))) ||
	     (B != DepFr_leader_cp(LOCAL_top_dep_fr))) {
	   /* not leader on that node */
	   INFO_THREADS("ans_reso_com (1) : not leader on that node dep_fr = %p leader_node =%p", LOCAL_top_dep_fr, GEN_CP(DepFr_leader_cp(LOCAL_top_dep_fr))->cp_dep_fr);
	   ThDepFr_state(GLOBAL_th_dep_fr(wid)) = working;  
	   B->cp_ap = ANSWER_RESOLUTION;
	   goto answer_resolution;
	 }
	 /* leader on that node */
	 dep_fr = GEN_CP(B)->cp_dep_fr;
	 ans_node = DepFr_last_answer(dep_fr);
	 if (TrNode_child(ans_node)) {
	   /* unconsumed answer */
	   ThDepFr_state(GLOBAL_th_dep_fr(wid)) = working;
	   ans_node = DepFr_last_answer(dep_fr) = TrNode_child(ans_node);
	   INFO_THREADS("ans_reso_com (2) : consume_answer =%p dep_fr = %p leader_node =%p", ans_node, dep_fr, GEN_CP(DepFr_leader_cp(dep_fr))->cp_dep_fr);
	   consume_answer_and_procceed(dep_fr, ans_node);      
	 }

	 sg_fr_ptr sg_fr = GEN_CP(B)->cp_sg_fr;
	 if (SgFr_sg_ent_state(sg_fr) < complete || (SgFr_sg_ent_state(sg_fr) >= complete && TrNode_child(ans_node)!= NULL))
	   do_not_complete_tables = 1; 

       } else {   /* using the B->cp_ap == ANSWER_RESOLUTION_COMPLETION to distinguish gen_cons nodes from gen */
	 /* generator node */
	 if (IS_BATCHED_GEN_CP(B)) { 
	   if (EQUAL_OR_YOUNGER_CP(B_FZ, B) && B != DepFr_leader_cp(LOCAL_top_dep_fr)) {
	     /* not leader on that node */
	     ThDepFr_state(GLOBAL_th_dep_fr(wid)) = working;  
	     B->cp_ap = NULL;
	     B = B->cp_b;
	     goto fail;
	   }
	 } else {
	   if (B != DepFr_leader_cp(LOCAL_top_dep_fr)) {
	     /* not leader on that node */
       	     ThDepFr_state(GLOBAL_th_dep_fr(wid)) = working;
       	     B->cp_ap = ANSWER_RESOLUTION;
       	     B = B->cp_b;
	     INFO_THREADS("ans_reso_com (3) : not leader on that node dep_fr = %p leader_node =%p", LOCAL_top_dep_fr, GEN_CP(DepFr_leader_cp(LOCAL_top_dep_fr))->cp_dep_fr);
       	     goto fail;
       	   }
       	 }
       }
       
       /* leader on that node */

       /* no unconsumed answers */

       dep_fr = LOCAL_top_dep_fr;
       
       /* check for dependency frames with unconsumed answers */
       while (YOUNGER_CP(DepFr_cons_cp(dep_fr), B)) {
       	 ans_node = DepFr_last_answer(dep_fr);
       	 if (TrNode_child(ans_node)) {
       	   ThDepFr_state(GLOBAL_th_dep_fr(wid)) = working;
	   /*dependency frame with unconsumed answers */
       	   ans_node = DepFr_last_answer(dep_fr) = TrNode_child(ans_node);
       	  if (IS_BATCHED_GEN_CP(B))
	    DepFr_backchain_cp(dep_fr) = B->cp_b;
	  else
	    DepFr_backchain_cp(dep_fr) = B; 
	   


	   /*rebind variables, update registers, consume answer and procceed */

       	   TABLING_ERROR_CHECKING(answer_resolution_completion, EQUAL_OR_YOUNGER_CP(B, DepFr_cons_cp(dep_fr)));
       	   TABLING_ERROR_CHECKING(answer_resolution_completion, B->cp_tr > DepFr_cons_cp(dep_fr)->cp_tr);	   
       	   rebind_variables(DepFr_cons_cp(dep_fr)->cp_tr,B->cp_tr);   //don't know if it is the same unbind_variables(DepFr_cons_cp(dep_fr)->cp_tr, B->cp_tr);
	   
       	   TABLING_ERROR_CHECKING(answer_resolution_completion, TR != B->cp_tr && !IsPairTerm((CELL)TrailTerm(TR - 1)));
       	   TABLING_ERROR_CHECKING(answer_resolution_completion, TR != B->cp_tr && (tr_fr_ptr) RepPair((CELL)TrailTerm(TR - 1)) != B->cp_tr);
       	   B = DepFr_cons_cp(dep_fr);
       	   TR = TR_FZ;
       	   if (TR != B->cp_tr)
       	     TRAIL_LINK(B->cp_tr);
	   INFO_THREADS("ans_reso_com (4) : consume_answer =%p dep_fr = %p leader_node =%p", ans_node, dep_fr, GEN_CP(DepFr_leader_cp(dep_fr))->cp_dep_fr);
       	   consume_answer_and_procceed(dep_fr, ans_node);
	 }


	 if (DepFr_external(dep_fr) == TRUE){
	   sg_fr_ptr sg_fr = GEN_CP(DepFr_cons_cp(dep_fr))->cp_sg_fr;
	   if (SgFr_sg_ent_state(sg_fr) < complete || (SgFr_sg_ent_state(sg_fr) >= complete && TrNode_child(ans_node)!= NULL))
	     do_not_complete_tables = 1;
	   
	 }
	 dep_fr = DepFr_next(dep_fr);
       }       

       /******************************** a terminaçao das threads *************************************/ 

       if (do_not_complete_tables == 1){
	 /*all the dependency frames have consumed all answers and we have external tables */
	 if (ThDepFr_next(GLOBAL_th_dep_fr(wid)) == wid)
	   /* worker_id is not inside an SCC */
	   continue;
	 
	 if (ThDepFr_state(GLOBAL_th_dep_fr(wid)) == working) {
	   int c_wid = ThDepFr_next(GLOBAL_th_dep_fr(wid));
	   do {
	     ThDepFr_terminator(GLOBAL_th_dep_fr(c_wid)) = 1;
	     c_wid = ThDepFr_next(GLOBAL_th_dep_fr(c_wid));
	   }while(c_wid != wid);
	   ThDepFr_terminator(GLOBAL_th_dep_fr(wid)) = 1;
	   ThDepFr_state(GLOBAL_th_dep_fr(wid)) = idle;	 

	 }else if (ThDepFr_state(GLOBAL_th_dep_fr(wid)) == idle){ 
	   int l_wid = wid;  /* leader wid */
	   int c_wid = ThDepFr_next(GLOBAL_th_dep_fr(wid));
	   int jump_state = TRUE;
	   do{
	     if (ThDepFr_state(GLOBAL_th_dep_fr(c_wid)) != idle){
	       jump_state = FALSE;
	       break;
	     } else 
	       if (l_wid > c_wid)
		 l_wid = c_wid;
	     c_wid = ThDepFr_next(GLOBAL_th_dep_fr(c_wid));
	   } while(c_wid != wid);
	   
	   if (jump_state && l_wid == wid){
	     /* wid is the current leader thread */
	     ThDepFr_terminator(GLOBAL_th_dep_fr(wid)) = 0;
	     c_wid = ThDepFr_next(GLOBAL_th_dep_fr(wid));
	     do {
	       dep_fr_ptr remote_dep_fr = REMOTE_top_dep_fr(c_wid);
	       while(YOUNGER_CP(DepFr_cons_cp(remote_dep_fr),DepFr_leader_cp(REMOTE_top_dep_fr(c_wid)))){
		 if (TrNode_child(DepFr_last_answer(remote_dep_fr))){
		   /* dependency frame with unconsumed answers */
		   jump_state = FALSE;
		   break;
		 }
		 remote_dep_fr = DepFr_next(remote_dep_fr);
	       }      
	       if (ThDepFr_state(GLOBAL_th_dep_fr(c_wid)) != idle){
		 jump_state = FALSE;
		 break;
	       }
	       c_wid = ThDepFr_next(GLOBAL_th_dep_fr(c_wid));
	     } while(c_wid != wid);
	   }
	   
	   if (jump_state && ThDepFr_terminator(GLOBAL_th_dep_fr(wid)) == 0){
	     c_wid = ThDepFr_next(GLOBAL_th_dep_fr(wid));
	     do {
	       ThDepFr_state(GLOBAL_th_dep_fr(c_wid)) = completing;
	       c_wid = ThDepFr_next(GLOBAL_th_dep_fr(c_wid));
	     }while(c_wid != wid);
	     ThDepFr_state(GLOBAL_th_dep_fr(wid)) = completing;
	   }
	 }else if (ThDepFr_state(GLOBAL_th_dep_fr(wid)) == completing){
	   INFO_THREADS("ans_reso_com (5) : completing thread_state =%d",ThDepFr_state(GLOBAL_th_dep_fr(wid)));
	   break;  	     /*do_not_complete_tables = 0;   -- same as "break;" */
	 }
       }       
      } while(do_not_complete_tables);

     END_PREFETCH()
     INFO_THREADS("ans_reso_com (6) : completing thread_state =%d",ThDepFr_state(GLOBAL_th_dep_fr(worker_id)));  

    goto complete_all;
}

  ENDBOp();
#endif /* THREADS_CONSUMER_SHARING */
