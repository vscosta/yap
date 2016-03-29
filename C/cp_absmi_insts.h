#ifdef INDENT_CODE
{
  {
    {
#endif /* INDENT_CODE */

      /*****************************************************************
       *        Plain try - retry - trust instructions                  *
       *****************************************************************/
      /* try_me    Label,NArgs */
      Op(try_me, Otapl);
      /* check if enough space between trail and codespace */
      check_trail(TR);
      /* I use YREG to go through the choicepoint. Usually YREG is in a
       * register, but sometimes (X86) not. In this case, have a
       * new register to point at YREG =*/
      CACHE_Y(YREG);
      /* store arguments for procedure */
      store_at_least_one_arg(PREG->y_u.Otapl.s);
      /* store abstract machine registers */
      store_yaam_regs(PREG->y_u.Otapl.d, 0);
      /* On a try_me, set cut to point at previous choicepoint,
       * that is, to the B before the cut.
       */
      set_cut(S_YREG, B);
      /* now, install the new YREG =*/
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif /* YAPOR */
      PREG = NEXTOP(PREG, Otapl);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      GONext();
      ENDOp();

      /* retry_me    Label,NArgs */
      Op(retry_me, Otapl);
      CACHE_Y(B);
      /* After retry, cut should be pointing at the parent
       * choicepoint for the current B */
      restore_yaam_regs(PREG->y_u.Otapl.d);
      restore_at_least_one_arg(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
      S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
      SET_BB(B_YREG);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, Otapl);
      GONext();
      ENDOp();

      /* trust_me    UnusedLabel,NArgs */
      Op(trust_me, Otapl);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
        SCH_last_alternative(PREG, B_YREG);
        restore_at_least_one_arg(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
        S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
        set_cut(S_YREG, B->cp_b);
      } else
#endif /* YAPOR */
      {
        pop_yaam_regs();
        pop_at_least_one_arg(PREG->y_u.Otapl.s);
/* After trust, cut should be pointing at the new top
 * choicepoint */
#ifdef FROZEN_STACKS
        S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
        set_cut(S_YREG, B);
      }
      PREG = NEXTOP(PREG, Otapl);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      GONext();
      ENDOp();

      /*****************************************************************
       *        EXO try - retry instructions                  *
       *****************************************************************/
      /* enter_exo    Pred,Label */
      BOp(enter_exo, e);
      {
        yamop *pt;
        saveregs();
        pt = Yap_ExoLookup(PredFromDefCode(PREG) PASS_REGS);
        setregs();
#ifdef SHADOW_S
        SREG = S;
#endif
        PREG = pt;
      }
      JMPNext();
      ENDBOp();

      /* check if enough space between trail and codespace */
      /* try_exo    Pred,Label */
      Op(try_exo, lp);
      /* check if enough space between trail and codespace */
      check_trail(TR);
      /* I use YREG to go through the choicepoint. Usually YREG is in a
       * register, but sometimes (X86) not. In this case, have a
       * new register to point at YREG =*/
      CACHE_Y(YREG);
      {
        struct index_t *i = (struct index_t *)(PREG->y_u.lp.l);
        S_YREG[-1] =
            (CELL)LINK_TO_ADDRESS(i, i->links[EXO_ADDRESS_TO_OFFSET(i, SREG)]);
      }
      S_YREG--;
      /* store arguments for procedure */
      store_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
      /* store abstract machine registers */
      store_yaam_regs(NEXTOP(PREG, lp), 0);
      /* On a try_me, set cut to point at previous choicepoint,
       * that is, to the B before the cut.
       */
      set_cut(S_YREG, B);
      /* now, install the new YREG =*/
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif /* YAPOR */
      PREG = NEXTOP(NEXTOP(PREG, lp), lp);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      GONext();
      ENDOp();

      /* check if enough space between trail and codespace */
      /* try_exo_udi    Pred,Label */
      Op(try_exo_udi, lp);
      /* check if enough space between trail and codespace */
      check_trail(TR);
      /* I use YREG =to go through the choicepoint. Usually YREG =is in a
       * register, but sometimes (X86) not. In this case, have a
       * new register to point at YREG =*/
      CACHE_Y(YREG);
      S_YREG--;
      /* store arguments for procedure */
      store_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
      /* store abstract machine registers */
      store_yaam_regs(NEXTOP(PREG, lp), 0);
      /* On a try_me, set cut to point at previous choicepoint,
       * that is, to the B before the cut.
       */
      set_cut(S_YREG, B);
      /* now, install the new YREG =*/
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif /* YAPOR */
      PREG = NEXTOP(NEXTOP(PREG, lp), lp);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      GONext();
      ENDOp();

      /* try_udi    Pred,Label */
      Op(try_udi, p);
      /* check if enough space between trail and codespace */
      check_trail(TR);
      /* I use YREG =to go through the choicepoint. Usually YREG =is in a
       * register, but sometimes (X86) not. In this case, have a
       * new register to point at YREG =*/
      CACHE_Y(YREG);
      { S_YREG[-1] = (CELL)SREG; /* the udi code did S = (CELL*)judyp; */ }
      S_YREG--;
      /* store arguments for procedure */
      store_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
      /* store abstract machine registers */
      store_yaam_regs(NEXTOP(PREG, lp), 0);
      /* On a try_me, set cut to point at previous choicepoint,
       * that is, to the B before the cut.
       */
      set_cut(S_YREG, B);
      /* now, install the new YREG =*/
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif /* YAPOR */
      PREG = NEXTOP(NEXTOP(PREG, lp), lp);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      GONext();
      ENDOp();

      /* check if enough space between trail and codespace */
      /* try_exo    Pred,Label */
      Op(try_all_exo, lp);
      /* check if enough space between trail and codespace */
      check_trail(TR);
      /* I use YREG =to go through the choicepoint. Usually YREG =is in a
       * register, but sometimes (X86) not. In this case, have a
       * new register to point at YREG =*/
      CACHE_Y(YREG);
      {
        struct index_t *i = (struct index_t *)(PREG->y_u.lp.l);
        SREG = i->cls;
        S_YREG[-2] = (CELL)(SREG + i->arity);
        S_YREG[-1] = (CELL)(SREG + i->arity * i->nels);
      }
      S_YREG -= 2;
      /* store arguments for procedure */
      store_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
      /* store abstract machine registers */
      store_yaam_regs(NEXTOP(PREG, lp), 0);
      /* On a try_me, set cut to point at previous choicepoint,
       * that is, to the B before the cut.
       */
      set_cut(S_YREG, B);
      /* now, install the new YREG =*/
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif /* YAPOR */
      PREG = NEXTOP(NEXTOP(PREG, lp), lp);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      GONext();
      ENDOp();

      /* retry_exo    Pred */
      Op(retry_exo, lp);
      BEGD(d0);
      CACHE_Y(B);
      {
        struct index_t *it = (struct index_t *)(PREG->y_u.lp.l);
        BITS32 offset =
            ADDRESS_TO_LINK(it, (BITS32 *)((CELL *)(B + 1))[it->arity]);
        d0 = it->links[offset];
        ((CELL *)(B + 1))[it->arity] = (CELL)LINK_TO_ADDRESS(it, d0);
        SREG = EXO_OFFSET_TO_ADDRESS(it, offset);
      }
      if (d0) {
        /* After retry, cut should be pointing at the parent
         * choicepoint for the current B */
        restore_yaam_regs(PREG);
        restore_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
#ifdef FROZEN_STACKS
        S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
        set_cut(S_YREG, B->cp_b);
#else
        set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
        SET_BB(B_YREG);
      } else {
#ifdef YAPOR
        if (SCH_top_shared_cp(B)) {
          SCH_last_alternative(PREG, B_YREG);
          restore_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
#ifdef FROZEN_STACKS
          S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
          set_cut(S_YREG, B->cp_b);
        } else
#endif /* YAPOR */
        {
          pop_yaam_regs();
          pop_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
/* After trust, cut should be pointing at the new top
 * choicepoint */
#ifdef FROZEN_STACKS
          S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
          set_cut(S_YREG, B);
        }
      }
      PREG = NEXTOP(PREG, lp);
      ENDCACHE_Y();
      ENDD(D0);
      GONext();
      ENDOp();

      /* retry_exo_udi    Pred */
      Op(retry_exo_udi, lp);
      BEGD(d0);
      CACHE_Y(B);
      {
        struct index_t *it = (struct index_t *)(PREG->y_u.lp.l);
        saveregs();
        d0 = ((CRetryExoIndex)it->udi_next)(it PASS_REGS);
        setregs();
#ifdef SHADOW_S
        SREG = S;
#endif
      }
      if (d0) {
        /* After retry, cut should be pointing at the parent
         * choicepoint for the current B */
        restore_yaam_regs(PREG);
        restore_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
#ifdef FROZEN_STACKS
        S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
        set_cut(S_YREG, B->cp_b);
#else
        set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
        SET_BB(B_YREG);
      } else {
#ifdef YAPOR
        if (SCH_top_shared_cp(B)) {
          SCH_last_alternative(PREG, B_YREG);
          restore_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
#ifdef FROZEN_STACKS
          S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
          set_cut(S_YREG, B->cp_b);
        } else
#endif /* YAPOR */
        {
          pop_yaam_regs();
          pop_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
/* After trust, cut should be pointing at the new top
 * choicepoint */
#ifdef FROZEN_STACKS
          S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
          set_cut(S_YREG, B);
        }
      }
      PREG = NEXTOP(PREG, lp);
      ENDCACHE_Y();
      ENDD(D0);
      GONext();
      ENDOp();

      /* retry_exo    Pred */
      Op(retry_udi, p);
      BEGD(d0);
      CACHE_Y(B);
      {
        // struct udi_index_t *jp = (struct udi_index_t *)((CELL
        // *)(B+1))[it->arity];
        /* operation has a side-effect: S = (CELL*)NextClause */
        saveregs();
        d0 = 0L; // Yap_UDI_NextAlt(jp);
        setregs();
#ifdef SHADOW_S
        SREG = S;
#endif
        /* d0 says if we're last */
      }
      if (d0) {
        /* After retry, cut should be pointing at the parent
         * choicepoint for the current B */
        restore_yaam_regs(PREG);
        restore_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
#ifdef FROZEN_STACKS
        S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
        set_cut(S_YREG, B->cp_b);
#else
        set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
        SET_BB(B_YREG);
      } else {
#ifdef YAPOR
        if (SCH_top_shared_cp(B)) {
          SCH_last_alternative(PREG, B_YREG);
          restore_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
#ifdef FROZEN_STACKS
          S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
          set_cut(S_YREG, B->cp_b);
        } else
#endif /* YAPOR */
        {
          pop_yaam_regs();
          pop_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
/* After trust, cut should be pointing at the new top
 * choicepoint */
#ifdef FROZEN_STACKS
          S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
          set_cut(S_YREG, B);
        }
      }
      PREG = (yamop *)SREG;
      ENDCACHE_Y();
      ENDD(D0);
      GONext();
      ENDOp();

      /* retry_exo    Pred */
      Op(retry_all_exo, lp);
      BEGD(d0);
      CACHE_Y(B);
      {
        UInt arity = ((struct index_t *)PREG->y_u.lp.l)->arity;
        CELL *extras = (CELL *)(B + 1);
        SREG = (CELL *)extras[arity];
        d0 = (SREG + arity != (CELL *)extras[arity + 1]);
        if (d0) {
          extras[arity] = (CELL)(SREG + arity);
          /* After retry, cut should be pointing at the parent
           * choicepoint for the current B */
          restore_yaam_regs(PREG);
          restore_at_least_one_arg(arity);
#ifdef FROZEN_STACKS
          S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
          set_cut(S_YREG, B->cp_b);
#else
          set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
          SET_BB(B_YREG);
        } else {
#ifdef YAPOR
          if (SCH_top_shared_cp(B)) {
            SCH_last_alternative(PREG, B_YREG);
            restore_at_least_one_arg(arity);
#ifdef FROZEN_STACKS
            S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
            set_cut(S_YREG, B->cp_b);
          } else
#endif /* YAPOR */
          {
            pop_yaam_regs();
            pop_at_least_one_arg(arity);
/* After trust, cut should be pointing at the new top
 * choicepoint */
#ifdef FROZEN_STACKS
            S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
            set_cut(S_YREG, B);
          }
        }
      }
      PREG = NEXTOP(PREG, lp);
      ENDCACHE_Y();
      ENDD(D0);
      GONext();
      ENDOp();

      /*****************************************************************
       *        Profiled try - retry - trust instructions               *
       *****************************************************************/

      /* profiled_enter_me    Pred */
      Op(enter_profiling, p);
      LOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      PREG->y_u.p.p->StatisticsForPred->NOfEntries++;
      UNLOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      PREG = NEXTOP(PREG, p);
      GONext();
      ENDOp();

      /* profiled_retry    Label,NArgs */
      Op(retry_profiled, p);
      LOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      PREG->y_u.p.p->StatisticsForPred->NOfRetries++;
      UNLOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      PREG = NEXTOP(PREG, p);
      GONext();
      ENDOp();

      /* profiled_retry_me    Label,NArgs */
      Op(profiled_retry_me, Otapl);
      CACHE_Y(B);
      /* After retry, cut should be pointing at the parent
       * choicepoint for the current B */
      LOCK(PREG->y_u.Otapl.p->StatisticsForPred->lock);
      PREG->y_u.Otapl.p->StatisticsForPred->NOfRetries++;
      UNLOCK(PREG->y_u.Otapl.p->StatisticsForPred->lock);
      restore_yaam_regs(PREG->y_u.Otapl.d);
      restore_args(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
      S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
      SET_BB(B_YREG);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, Otapl);
      GONext();
      ENDOp();

      /* profiled_trust_me    UnusedLabel,NArgs */
      Op(profiled_trust_me, Otapl);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
        SCH_last_alternative(PREG, B_YREG);
        restore_args(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
        S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
        set_cut(S_YREG, B->cp_b);
      } else
#endif /* YAPOR */
      {
        pop_yaam_regs();
        pop_args(PREG->y_u.Otapl.s);
/* After trust, cut should be pointing at the new top
 * choicepoint */
#ifdef FROZEN_STACKS
        S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
        set_cut(S_YREG, B);
      }
      SET_BB(B_YREG);
      ENDCACHE_Y();
      LOCK(PREG->y_u.Otapl.p->StatisticsForPred->lock);
      PREG->y_u.Otapl.p->StatisticsForPred->NOfRetries++;
      UNLOCK(PREG->y_u.Otapl.p->StatisticsForPred->lock);
      PREG = NEXTOP(PREG, Otapl);
      GONext();
      ENDOp();

      /*****************************************************************
      *        Call count instructions                                 *
      *****************************************************************/

      /* count_enter_me    Label,NArgs */
      Op(count_call, p);
      LOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      PREG->y_u.p.p->StatisticsForPred->NOfEntries++;
      UNLOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      LOCAL_ReductionsCounter--;
      if (LOCAL_ReductionsCounter == 0 && LOCAL_ReductionsCounterOn) {
        saveregs();
        Yap_NilError(CALL_COUNTER_UNDERFLOW_EVENT, "");
        setregs();
        JMPNext();
      }
      LOCAL_PredEntriesCounter--;
      if (LOCAL_PredEntriesCounter == 0 && LOCAL_PredEntriesCounterOn) {
        saveregs();
        Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT, "");
        setregs();
        JMPNext();
      }
      PREG = NEXTOP(PREG, p);
      GONext();
      ENDOp();

      /* count_retry    Label,NArgs */
      Op(count_retry, p);
      LOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      PREG->y_u.p.p->StatisticsForPred->NOfRetries++;
      UNLOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      LOCAL_RetriesCounter--;
      if (LOCAL_RetriesCounter == 0 && LOCAL_RetriesCounterOn) {
        /* act as if we had backtracked */
        ENV = B->cp_env;
        saveregs();
        Yap_NilError(RETRY_COUNTER_UNDERFLOW_EVENT, "");
        setregs();
        JMPNext();
      }
      LOCAL_PredEntriesCounter--;
      if (LOCAL_PredEntriesCounter == 0 && LOCAL_PredEntriesCounterOn) {
        ENV = B->cp_env;
        saveregs();
        Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT, "");
        setregs();
        JMPNext();
      }
      PREG = NEXTOP(PREG, p);
      GONext();
      ENDOp();

      /* count_retry_me    Label,NArgs */
      Op(count_retry_me, Otapl);
      CACHE_Y(B);
      restore_yaam_regs(PREG->y_u.Otapl.d);
      restore_args(PREG->y_u.Otapl.s);
/* After retry, cut should be pointing at the parent
 * choicepoint for the current B */
#ifdef FROZEN_STACKS
      S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
      SET_BB(B_YREG);
      ENDCACHE_Y();
      LOCK(((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->lock);
      ((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->NOfRetries++;
      UNLOCK(((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->lock);
      LOCAL_RetriesCounter--;
      if (LOCAL_RetriesCounter == 0 && LOCAL_RetriesCounterOn) {
        saveregs();
        Yap_NilError(RETRY_COUNTER_UNDERFLOW_EVENT, "");
        setregs();
        JMPNext();
      }
      LOCAL_PredEntriesCounter--;
      if (LOCAL_PredEntriesCounter == 0 && LOCAL_PredEntriesCounterOn) {
        saveregs();
        Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT, "");
        setregs();
        JMPNext();
      }
      PREG = NEXTOP(PREG, Otapl);
      GONext();
      ENDOp();

      /* count_trust_me    UnusedLabel,NArgs */
      Op(count_trust_me, Otapl);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
        SCH_last_alternative(PREG, B_YREG);
        restore_args(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
        S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
        set_cut(S_YREG, B->cp_b);
      } else
#endif /* YAPOR */
      {
        pop_yaam_regs();
        pop_args(PREG->y_u.Otapl.s);
/* After trust, cut should be pointing at the new top
 * choicepoint */
#ifdef FROZEN_STACKS
        S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
        set_cut(S_YREG, B);
      }
      SET_BB(B_YREG);
      ENDCACHE_Y();
      LOCAL_RetriesCounter--;
      if (LOCAL_RetriesCounter == 0) {
        saveregs();
        Yap_NilError(RETRY_COUNTER_UNDERFLOW_EVENT, "");
        setregs();
        JMPNext();
      }
      LOCAL_PredEntriesCounter--;
      if (LOCAL_PredEntriesCounter == 0) {
        saveregs();
        Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT, "");
        setregs();
        JMPNext();
      }
      LOCK(((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->lock);
      ((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->NOfRetries++;
      UNLOCK(((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->lock);
      PREG = NEXTOP(PREG, Otapl);
      GONext();
      ENDOp();

      /*****************************************************************
       *        check for enough room                                  *
       *****************************************************************/

      /* ensure_space                   */
      BOp(ensure_space, Osbpa);
      {
        Int sz = PREG->y_u.Osbpa.i;
        UInt arity = PREG->y_u.Osbpa.p->ArityOfPE;

        if (Unsigned(HR) + sz > Unsigned(YREG) - StackGap(PASS_REGS1)) {
          YENV[E_CP] = (CELL)CPREG;
          YENV[E_E] = (CELL)ENV;
#ifdef DEPTH_LIMIT
          YENV[E_DEPTH] = DEPTH;
#endif /* DEPTH_LIMIT */
          SET_ASP(YREG, PREG->y_u.Osbpa.s);
          PREG = NEXTOP(PREG, Osbpa);
          saveregs();
          if (!Yap_gcl(sz, arity, YENV, PREG)) {
            Yap_NilError(RESOURCE_ERROR_STACK, LOCAL_ErrorMessage);
            setregs();
            FAIL();
          } else {
            setregs();
          }
        } else {
          PREG = NEXTOP(PREG, Osbpa);
        }
      }
      JMPNext();
      ENDBOp();

      /*****************************************************************
       *        try and retry of dynamic predicates                     *
       *****************************************************************/

      /* spy_or_trymark                   */
      BOp(spy_or_trymark, Otapl);
      PELOCK(5, ((PredEntry *)(PREG->y_u.Otapl.p)));
      PREG = (yamop *)(&(((PredEntry *)(PREG->y_u.Otapl.p))->OpcodeOfPred));
      UNLOCKPE(11, (PredEntry *)(PREG->y_u.Otapl.p));
      saveregs();
      spy_goal(PASS_REGS1);
      setregs();
      ENDBOp();

      /* try_and_mark   Label,NArgs       */
      BOp(try_and_mark, Otapl);
      check_trail(TR);
#if defined(YAPOR) || defined(THREADS)
#ifdef YAPOR
      /* The flags I check here should never change during execution */
      CUT_wait_leftmost();
#endif /* YAPOR */
      if (PREG->y_u.Otapl.p->PredFlags & LogUpdatePredFlag) {
        PELOCK(6, PREG->y_u.Otapl.p);
        PP = PREG->y_u.Otapl.p;
      }
      if (PREG->y_u.Otapl.p->CodeOfPred != PREG) {
        /* oops, someone changed the procedure under our feet,
           fortunately this is no big deal because we haven't done
           anything yet */
        PP = NULL;
        PREG = PREG->y_u.Otapl.p->CodeOfPred;
        UNLOCKPE(12, PREG->y_u.Otapl.p);
        /* for profiler */
        save_pc();
        JMPNext();
      }
#endif
      CACHE_Y(YREG);
      PREG = PREG->y_u.Otapl.d;
      /*
        I've got a read lock on the DB, so I don't need to care...
        niaaahh.... niahhhh...
      */
      LOCK(DynamicLock(PREG));
      /* one can now mess around with the predicate */
      UNLOCKPE(13, ((PredEntry *)(PREG->y_u.Otapl.p)));
      BEGD(d1);
      d1 = PREG->y_u.Otapl.s;
      store_args(d1);
      store_yaam_regs(PREG, 0);
      ENDD(d1);
      set_cut(S_YREG, B);
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif /* YAPOR */
      SET_BB(B_YREG);
      ENDCACHE_Y();
#if MULTIPLE_STACKS
      INC_CLREF_COUNT(ClauseCodeToDynamicClause(PREG));
      UNLOCK(DynamicLock(PREG));
      TRAIL_CLREF(ClauseCodeToDynamicClause(PREG));
#else
      if (FlagOff(InUseMask, DynamicFlags(PREG))) {

        SetFlag(InUseMask, DynamicFlags(PREG));
        TRAIL_CLREF(ClauseCodeToDynamicClause(PREG));
      }
#endif
      PREG = NEXTOP(PREG, Otapl);
      JMPNext();

      ENDBOp();

      BOp(count_retry_and_mark, Otapl);
      LOCAL_RetriesCounter--;
      if (LOCAL_RetriesCounter == 0) {
        saveregs();
        Yap_NilError(RETRY_COUNTER_UNDERFLOW_EVENT, "");
        setregs();
        JMPNext();
      }
      LOCAL_PredEntriesCounter--;
      if (LOCAL_PredEntriesCounter == 0) {
        saveregs();
        Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT, "");
        setregs();
        JMPNext();
      }
      /* enter a retry dynamic */
      ENDBOp();

      BOp(profiled_retry_and_mark, Otapl);
      LOCK(((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->lock);
      ((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->NOfRetries++;
      UNLOCK(((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->lock);
      /* enter a retry dynamic */
      ENDBOp();

      /* retry_and_mark   Label,NArgs     */
      BOp(retry_and_mark, Otapl);
#ifdef YAPOR
      CUT_wait_leftmost();
#endif /* YAPOR */
      /* need to make the DB stable until I get the new clause */
      PELOCK(7, PREG->y_u.Otapl.p);
      CACHE_Y(B);
      PREG = PREG->y_u.Otapl.d;
      LOCK(DynamicLock(PREG));
      UNLOCK(PREG->y_u.Otapl.p->PELock);
      restore_yaam_regs(PREG);
      restore_args(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
      S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
      SET_BB(B_YREG);
      ENDCACHE_Y();
#if MULTIPLE_STACKS
      INC_CLREF_COUNT(ClauseCodeToDynamicClause(PREG));
      TRAIL_CLREF(ClauseCodeToDynamicClause(PREG));
      UNLOCK(DynamicLock(PREG));
#else
      if (FlagOff(InUseMask, DynamicFlags(PREG))) {

        SetFlag(InUseMask, DynamicFlags(PREG));
        TRAIL_CLREF(ClauseCodeToDynamicClause(PREG));
      }
#endif
      PREG = NEXTOP(PREG, Otapl);
      JMPNext();

      ENDBOp();

      /************************************************************************\
       *    Try / Retry / Trust for main indexing blocks                    *
\************************************************************************/

      BOp(try_clause, Otapl);
      check_trail(TR);
      CACHE_Y(YREG);
      /* Point AP to the code that follows this instruction */
      store_at_least_one_arg(PREG->y_u.Otapl.s);
      store_yaam_regs(NEXTOP(PREG, Otapl), 0);
      PREG = PREG->y_u.Otapl.d;
      set_cut(S_YREG, B);
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif /* YAPOR */
      SET_BB(B_YREG);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      BOp(try_clause2, l);
      check_trail(TR);
      CACHE_Y(YREG);
      /* Point AP to the code that follows this instruction */
      {
        register CELL x2 = ARG2;
        register CELL x1 = ARG1;

        store_yaam_regs(NEXTOP(PREG, l), 2);
        B_YREG->cp_a1 = x1;
        B_YREG->cp_a2 = x2;
      }
      PREG = PREG->y_u.l.l;
      set_cut(S_YREG, B);
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif /* YAPOR */
      SET_BB(B_YREG);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      BOp(try_clause3, l);
      check_trail(TR);
      CACHE_Y(YREG);
      /* Point AP to the code that follows this instruction */
      {
        store_yaam_regs(NEXTOP(PREG, l), 3);
        B_YREG->cp_a1 = ARG1;
        B_YREG->cp_a2 = ARG2;
        B_YREG->cp_a3 = ARG3;
      }
      PREG = PREG->y_u.l.l;
      set_cut(S_YREG, B);
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif /* YAPOR */
      SET_BB(B_YREG);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      BOp(try_clause4, l);
      check_trail(TR);
      CACHE_Y(YREG);
      /* Point AP to the code that follows this instruction */
      {
        store_yaam_regs(NEXTOP(PREG, l), 4);
        B_YREG->cp_a1 = ARG1;
        B_YREG->cp_a2 = ARG2;
        B_YREG->cp_a3 = ARG3;
        B_YREG->cp_a4 = ARG4;
      }
      PREG = PREG->y_u.l.l;
      set_cut(S_YREG, B);
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif /* YAPOR */
      SET_BB(B_YREG);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      BOp(retry, Otapl);
      CACHE_Y(B);
      restore_yaam_regs(NEXTOP(PREG, Otapl));
      restore_at_least_one_arg(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
      S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
      SET_BB(B_YREG);
      ENDCACHE_Y();
      PREG = PREG->y_u.Otapl.d;
      JMPNext();
      ENDBOp();

      BOp(retry2, l);
      CACHE_Y(B);
      restore_yaam_regs(NEXTOP(PREG, l));
      PREG = PREG->y_u.l.l;
      ARG1 = B_YREG->cp_a1;
      ARG2 = B_YREG->cp_a2;
#ifdef FROZEN_STACKS
      S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
      SET_BB(B_YREG);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      BOp(retry3, l);
      CACHE_Y(B);
      restore_yaam_regs(NEXTOP(PREG, l));
      PREG = PREG->y_u.l.l;
      ARG1 = B_YREG->cp_a1;
      ARG2 = B_YREG->cp_a2;
      ARG3 = B_YREG->cp_a3;
#ifdef FROZEN_STACKS
      S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
      SET_BB(B_YREG);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      BOp(retry4, l);
      CACHE_Y(B);
      restore_yaam_regs(NEXTOP(PREG, l));
      PREG = PREG->y_u.l.l;
      ARG1 = B_YREG->cp_a1;
      ARG2 = B_YREG->cp_a2;
      ARG3 = B_YREG->cp_a3;
      ARG4 = B_YREG->cp_a4;
#ifdef FROZEN_STACKS
      S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
      SET_BB(B_YREG);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      BOp(trust, Otapl);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
        SCH_last_alternative(PREG, B_YREG);
        restore_at_least_one_arg(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
        S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
        set_cut(S_YREG, B->cp_b);
      } else
#endif /* YAPOR */
      {
        pop_yaam_regs();
        pop_at_least_one_arg(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
        S_YREG = (CELL *)PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
        set_cut(S_YREG, B);
      }
      SET_BB(B_YREG);
      ENDCACHE_Y();
      PREG = PREG->y_u.Otapl.d;
      JMPNext();
      ENDBOp();

      BOp(try_in, l);
      B->cp_ap = NEXTOP(PREG, l);
      PREG = PREG->y_u.l.l;
      JMPNext();
      ENDBOp();
