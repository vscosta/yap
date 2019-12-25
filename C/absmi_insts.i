/*****************************************************************
 *                         INSTRUCTIONS                           *
 *****************************************************************/

#ifdef INDENT_CODE
{
  {
    {
#endif /* INDENT_CODE */

      BOp(Ystop, l);
      SET_ASP(YREG, E_CB*sizeof(CELL));
      /* make sure ASP is initialized */
      saveregs();

#if PUSH_REGS
      restore_absmi_regs(old_regs);
#endif
#if BP_FREE
      P1REG = PCBACKUP;
#endif
      return 1;
      ENDBOp();

      BOp(Nstop, e);
      SET_ASP(YREG, E_CB*sizeof(CELL));
      saveregs();
#if PUSH_REGS
      restore_absmi_regs(old_regs);
#endif
#if BP_FREE
      P1REG = PCBACKUP;
#endif
      return 0;
      ENDBOp();

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
#endif  /* YAPOR */
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
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
	set_cut(S_YREG, B->cp_b);
      }
      else
#endif  /* YAPOR */
	{
	  pop_yaam_regs();
	  pop_at_least_one_arg(PREG->y_u.Otapl.s);
	  /* After trust, cut should be pointing at the new top
	   * choicepoint */
#ifdef FROZEN_STACKS
	  S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	S_YREG[-1] = (CELL)LINK_TO_ADDRESS(i,i->links[EXO_ADDRESS_TO_OFFSET(i, SREG)]);
      }
      S_YREG--;
      /* store arguments for procedure */
      store_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
      /* store abstract machine registers */
      store_yaam_regs(NEXTOP(PREG,lp), 0);
      /* On a try_me, set cut to point at previous choicepoint,
       * that is, to the B before the cut.
       */
      set_cut(S_YREG, B);
      /* now, install the new YREG =*/
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif  /* YAPOR */
      PREG = NEXTOP(NEXTOP(PREG, lp),lp);
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
      store_yaam_regs(NEXTOP(PREG,lp), 0);
      /* On a try_me, set cut to point at previous choicepoint,
       * that is, to the B before the cut.
       */
      set_cut(S_YREG, B);
      /* now, install the new YREG =*/
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif  /* YAPOR */
      PREG = NEXTOP(NEXTOP(PREG, lp),lp);
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
      {
	S_YREG[-1] = (CELL)SREG; /* the udi code did S = (CELL*)judyp; */
      }
      S_YREG--;
      /* store arguments for procedure */
      store_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
      /* store abstract machine registers */
      store_yaam_regs(NEXTOP(PREG,lp), 0);
      /* On a try_me, set cut to point at previous choicepoint,
       * that is, to the B before the cut.
       */
      set_cut(S_YREG, B);
      /* now, install the new YREG =*/
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif  /* YAPOR */
      PREG = NEXTOP(NEXTOP(PREG, lp),lp);
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
	S_YREG[-2] = (CELL)(SREG+i->arity);
	S_YREG[-1] = (CELL)(SREG+i->arity*i->nels);
      }
      S_YREG-=2;
      /* store arguments for procedure */
      store_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
      /* store abstract machine registers */
      store_yaam_regs(NEXTOP(PREG,lp), 0);
      /* On a try_me, set cut to point at previous choicepoint,
       * that is, to the B before the cut.
       */
      set_cut(S_YREG, B);
      /* now, install the new YREG =*/
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif  /* YAPOR */
      PREG = NEXTOP(NEXTOP(PREG, lp),lp);
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
	BITS32 offset = ADDRESS_TO_LINK(it,(BITS32 *)((CELL *)(B+1))[it->arity]);
	d0 = it->links[offset];
	((CELL *)(B+1))[it->arity] = (CELL)LINK_TO_ADDRESS(it, d0);
	SREG = EXO_OFFSET_TO_ADDRESS(it, offset);
      }
      if (d0) {
	/* After retry, cut should be pointing at the parent
	 * choicepoint for the current B */
	restore_yaam_regs(PREG);
	restore_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
#ifdef FROZEN_STACKS
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	  S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
	  set_cut(S_YREG, B->cp_b);
	} else
#endif  /* YAPOR */
	  {
	    pop_yaam_regs();
	    pop_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
	    /* After trust, cut should be pointing at the new top
	     * choicepoint */
#ifdef FROZEN_STACKS
	    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	  S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
	  set_cut(S_YREG, B->cp_b);
	} else
#endif  /* YAPOR */
	  {
	    pop_yaam_regs();
	    pop_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
	    /* After trust, cut should be pointing at the new top
	     * choicepoint */
#ifdef FROZEN_STACKS
	    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	// struct udi_index_t *jp = (struct udi_index_t *)((CELL *)(B+1))[it->arity];
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
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	  S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
	  set_cut(S_YREG, B->cp_b);
	} else
#endif  /* YAPOR */
	  {
	    pop_yaam_regs();
	    pop_at_least_one_arg(PREG->y_u.lp.p->ArityOfPE);
	    /* After trust, cut should be pointing at the new top
	     * choicepoint */
#ifdef FROZEN_STACKS
	    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	CELL *extras = (CELL *)(B+1);
	SREG = (CELL *)extras[arity];
	d0 = (SREG+arity != (CELL *)extras[arity+1]);
	if (d0) {
	  extras[arity] = (CELL)(SREG+arity);
	  /* After retry, cut should be pointing at the parent
	   * choicepoint for the current B */
	  restore_yaam_regs(PREG);
	  restore_at_least_one_arg(arity);
#ifdef FROZEN_STACKS
	  S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
	    set_cut(S_YREG, B->cp_b);
	  } else
#endif  /* YAPOR */
	    {
	      pop_yaam_regs();
	      pop_at_least_one_arg(arity);
	      /* After trust, cut should be pointing at the new top
	       * choicepoint */
#ifdef FROZEN_STACKS
	      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
	set_cut(S_YREG, B->cp_b);
      }
      else
#endif  /* YAPOR */
	{
	  pop_yaam_regs();
	  pop_args(PREG->y_u.Otapl.s);
	  /* After trust, cut should be pointing at the new top
	   * choicepoint */
#ifdef FROZEN_STACKS
	  S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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

      BOp(profiled_retry_logical, OtaLl);
      check_trail(TR);
      {
	UInt timestamp;
	CACHE_Y(B);

#if defined(YAPOR) || defined(THREADS)
	if (PP != PREG->y_u.OtaLl.d->ClPred) {
	  if (PP) UNLOCKPE(15,PP);
	  PP = PREG->y_u.OtaLl.d->ClPred;
	  PELOCK(15,PP);
	}
#endif
	timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[PREG->y_u.OtaLl.s]);
	if (!VALID_TIMESTAMP(timestamp, PREG->y_u.OtaLl.d)) {
	  /* jump to next instruction */
	  PREG=PREG->y_u.OtaLl.n;
	  JMPNext();
	}
	restore_yaam_regs(PREG->y_u.OtaLl.n);
	restore_args(PREG->y_u.OtaLl.s);
	LOCK(PREG->y_u.OtaLl.d->ClPred->StatisticsForPred->lock);
	PREG->y_u.OtaLl.d->ClPred->StatisticsForPred->NOfRetries++;
	UNLOCK(PREG->y_u.OtaLl.d->ClPred->StatisticsForPred->lock);
#ifdef THREADS
	PP = PREG->y_u.OtaLl.d->ClPred;
#endif
	PREG = PREG->y_u.OtaLl.d->ClCode;
#ifdef FROZEN_STACKS
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
	set_cut(S_YREG, B->cp_b);
#else
	set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
	SET_BB(B_YREG);
	ENDCACHE_Y();
      }
      JMPNext();
      ENDBOp();

      BOp(profiled_trust_logical, OtILl);
      CACHE_Y(B);
      {
	LogUpdIndex *cl = PREG->y_u.OtILl.block;
	PredEntry *ap = cl->ClPred;
	LogUpdClause *lcl = PREG->y_u.OtILl.d;
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]);

#if defined(YAPOR) || defined(THREADS)
	if (PP != ap) {
	  if (PP) UNLOCKPE(16,PP);
	  PP = ap;
	  PELOCK(16,PP);
	}
#endif
	if (!VALID_TIMESTAMP(timestamp, lcl)) {
	  /* jump to next alternative */
	  PREG = FAILCODE;
	} else {
	  LOCK(ap->StatisticsForPred->lock);
	  ap->StatisticsForPred->NOfRetries++;
	  UNLOCK(ap->StatisticsForPred->lock);
	  PREG = lcl->ClCode;
	}
	/* HEY, leave indexing block alone!! */
	/* check if we are the ones using this code */
#if MULTIPLE_STACKS
	DEC_CLREF_COUNT(cl);
	/* clear the entry from the trail */
	B->cp_tr--;
	TR = B->cp_tr;
	/* actually get rid of the code */
	if (cl->ClRefCount == 0 && (cl->ClFlags & (ErasedMask|DirtyMask))) {
	  if (PREG != FAILCODE) {
	    /* I am the last one using this clause, hence I don't need a lock
	       to dispose of it
	    */
	    if (lcl->ClRefCount == 1) {
	      /* make sure the clause isn't destroyed */
	      /* always add an extra reference */
	      INC_CLREF_COUNT(lcl);
	      TRAIL_CLREF(lcl);
	    }
	  }
	  if (cl->ClFlags & ErasedMask) {
	    saveregs();
	    Yap_ErLogUpdIndex(cl);
	    setregs();
	  } else {
	    saveregs();
	    Yap_CleanUpIndex(cl);
	    setregs();
	  }
	  save_pc();
	}
#else
	if (TrailTerm(B->cp_tr-1) == CLREF_TO_TRENTRY(cl) &&
	    B->cp_tr != B->cp_b->cp_tr) {
	  cl->ClFlags &= ~InUseMask;
	  --B->cp_tr;
#if FROZEN_STACKS
	  if (B->cp_tr > TR_FZ)
#endif
	    {
	      TR = B->cp_tr;
	    }
	  /* next, recover space for the indexing code if it was erased */
	  if (cl->ClFlags & (ErasedMask|DirtyMask)) {
	    if (PREG != FAILCODE) {
	      /* make sure we don't erase the clause we are jumping to,
		 notice that we can erase a number of refs in one go. */
	      if (!(lcl->ClFlags & InUseMask)) {
		lcl->ClFlags |= InUseMask;
		TRAIL_CLREF(lcl);
	      }
	    }
	    if (cl->ClFlags & ErasedMask) {
	      saveregs();
	      Yap_ErLogUpdIndex(cl);
	      setregs();
	    } else {
	      saveregs();
	      Yap_CleanUpIndex(cl);
	      setregs();
	    }
	    save_pc();
	  }
	}
#endif
#ifdef YAPOR
	if (SCH_top_shared_cp(B)) {
	  SCH_last_alternative(PREG, B_YREG);
	  restore_args(ap->ArityOfPE);
#ifdef FROZEN_STACKS
	  S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#else
	  S_YREG++;
#endif /* FROZEN_STACKS */
	  set_cut(S_YREG, B->cp_b);
	} else
#endif  /* YAPOR */
	  {
	    pop_yaam_regs();
	    pop_args(ap->ArityOfPE);
	    S_YREG--;
#ifdef FROZEN_STACKS
	    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
	    set_cut(S_YREG, B);
	  }
	SET_BB(B_YREG);
	ENDCACHE_Y();
	JMPNext();
      }
      ENDBOp();

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
	Yap_NilError(CALL_COUNTER_UNDERFLOW,"");
	setregs();
	JMPNext();
      }
      LOCAL_PredEntriesCounter--;
      if (LOCAL_PredEntriesCounter == 0 && LOCAL_PredEntriesCounterOn) {
	saveregs();
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,"");
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
	Yap_NilError(RETRY_COUNTER_UNDERFLOW,"");
	setregs();
	JMPNext();
      }
      LOCAL_PredEntriesCounter--;
      if (LOCAL_PredEntriesCounter == 0 && LOCAL_PredEntriesCounterOn) {
	ENV = B->cp_env;
	saveregs();
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,"");
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
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	Yap_NilError(RETRY_COUNTER_UNDERFLOW,"");
	setregs();
	JMPNext();
      }
      LOCAL_PredEntriesCounter--;
      if (LOCAL_PredEntriesCounter == 0 && LOCAL_PredEntriesCounterOn) {
	saveregs();
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,"");
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
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
	set_cut(S_YREG, B->cp_b);
      }
      else
#endif  /* YAPOR */
	{
	  pop_yaam_regs();
	  pop_args(PREG->y_u.Otapl.s);
	  /* After trust, cut should be pointing at the new top
	   * choicepoint */
#ifdef FROZEN_STACKS
	  S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
	  set_cut(S_YREG, B);
	}
      SET_BB(B_YREG);
      ENDCACHE_Y();
      LOCAL_RetriesCounter--;
      if (LOCAL_RetriesCounter == 0) {
	saveregs();
	Yap_NilError(RETRY_COUNTER_UNDERFLOW,"");
	setregs();
	JMPNext();
      }
      LOCAL_PredEntriesCounter--;
      if (LOCAL_PredEntriesCounter == 0) {
	saveregs();
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,"");
	setregs();
	JMPNext();
      }
      LOCK(((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->lock);
      ((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->NOfRetries++;
      UNLOCK(((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->lock);
      PREG = NEXTOP(PREG, Otapl);
      GONext();
      ENDOp();

      BOp(count_retry_logical, OtaLl);
      check_trail(TR);
      {
	UInt timestamp;
	CACHE_Y(B);

#if defined(YAPOR) || defined(THREADS)
	if (PP != PREG->y_u.OtaLl.d->ClPred) {
	  if (PP) UNLOCKPE(15,PP);
	  PP = PREG->y_u.OtaLl.d->ClPred;
	  PELOCK(15,PP);
	}
#endif
	timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[PREG->y_u.OtaLl.s]);
	if (!VALID_TIMESTAMP(timestamp, PREG->y_u.OtaLl.d)) {
	  /* jump to next instruction */
	  PREG=PREG->y_u.OtaLl.n;
	  JMPNext();
	}
	restore_yaam_regs(PREG->y_u.OtaLl.n);
	restore_args(PREG->y_u.OtaLl.s);
	LOCAL_RetriesCounter--;
	if (LOCAL_RetriesCounter == 0) {
	  saveregs();
	  Yap_NilError(RETRY_COUNTER_UNDERFLOW,"");
	  setregs();
	  JMPNext();
	}
	LOCAL_PredEntriesCounter--;
	if (LOCAL_PredEntriesCounter == 0) {
	  saveregs();
	  Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,"");
	  setregs();
	  JMPNext();
	}
	LOCK(PREG->y_u.OtaLl.d->ClPred->StatisticsForPred->lock);
	PREG->y_u.OtaLl.d->ClPred->StatisticsForPred->NOfRetries++;
	UNLOCK(PREG->y_u.OtaLl.d->ClPred->StatisticsForPred->lock);
#ifdef THREADS
	PP = PREG->y_u.OtaLl.d->ClPred;
#endif
	PREG = PREG->y_u.OtaLl.d->ClCode;
#ifdef FROZEN_STACKS
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
	set_cut(S_YREG, B->cp_b);
#else
	set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
	SET_BB(B_YREG);
	ENDCACHE_Y();
      }
      JMPNext();
      ENDBOp();

      BOp(count_trust_logical, OtILl);
      CACHE_Y(B);
      {
	LogUpdIndex *cl = PREG->y_u.OtILl.block;
	PredEntry *ap = cl->ClPred;
	LogUpdClause *lcl = PREG->y_u.OtILl.d;
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]);

#if defined(YAPOR) || defined(THREADS)
	if (PP != ap) {
	  if (PP) UNLOCKPE(16,PP);
	  PP = ap;
	  PELOCK(16,PP);
	}
#endif
	if (!VALID_TIMESTAMP(timestamp, lcl)) {
	  /* jump to next alternative */
	  PREG = FAILCODE;
	} else {
	  LOCAL_RetriesCounter--;
	  if (LOCAL_RetriesCounter == 0) {
	    saveregs();
	    Yap_NilError(RETRY_COUNTER_UNDERFLOW,"");
	    setregs();
	    JMPNext();
	  }
	  LOCAL_PredEntriesCounter--;
	  if (LOCAL_PredEntriesCounter == 0) {
	    saveregs();
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,"");
	    setregs();
	    JMPNext();
	  }
	  LOCK(ap->StatisticsForPred->lock);
	  ap->StatisticsForPred->NOfRetries++;
	  UNLOCK(ap->StatisticsForPred->lock);
	  PREG = lcl->ClCode;
	}
	/* HEY, leave indexing block alone!! */
	/* check if we are the ones using this code */
#if MULTIPLE_STACKS
	PELOCK(2, ap);
	PP = ap;
	DEC_CLREF_COUNT(cl);
	/* clear the entry from the trail */
	--B->cp_tr;
	TR = B->cp_tr;
	/* actually get rid of the code */
	if (cl->ClRefCount == 0 && (cl->ClFlags & (ErasedMask|DirtyMask))) {
	  if (PREG != FAILCODE) {
	    /* I am the last one using this clause, hence I don't need a lock
	       to dispose of it
	    */
	    if (lcl->ClRefCount == 1) {
	      /* make sure the clause isn't destroyed */
	      /* always add an extra reference */
	      INC_CLREF_COUNT(lcl);
	      TRAIL_CLREF(lcl);
	    }
	  }
	  if (cl->ClFlags & ErasedMask) {
	    saveregs();
	    Yap_ErLogUpdIndex(cl);
	    setregs();
	  } else {
	    saveregs();
	    Yap_CleanUpIndex(cl);
	    setregs();
	  }
	  save_pc();
	}
#else
	if (TrailTerm(B->cp_tr-1) == CLREF_TO_TRENTRY(cl) &&
	    B->cp_tr != B->cp_b->cp_tr) {
	  cl->ClFlags &= ~InUseMask;
	  --B->cp_tr;
#if FROZEN_STACKS
	  if (B->cp_tr > TR_FZ)
#endif
	    {
	      TR = B->cp_tr;
	    }
	  /* next, recover space for the indexing code if it was erased */
	  if (cl->ClFlags & (ErasedMask|DirtyMask)) {
	    if (PREG != FAILCODE) {
	      /* make sure we don't erase the clause we are jumping too */
	      if (!(lcl->ClFlags & InUseMask)) {
		lcl->ClFlags |= InUseMask;
		TRAIL_CLREF(lcl);
	      }
	    }
	    if (cl->ClFlags & ErasedMask) {
	      saveregs();
	      Yap_ErLogUpdIndex(cl);
	      setregs();
	    } else {
	      saveregs();
	      Yap_CleanUpIndex(cl);
	      setregs();
	    }
	    save_pc();
	  }
	}
#endif
#ifdef YAPOR
	if (SCH_top_shared_cp(B)) {
	  SCH_last_alternative(PREG, B_YREG);
	  restore_args(ap->ArityOfPE);
#ifdef FROZEN_STACKS
	  S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#else
	  S_YREG++;
#endif /* FROZEN_STACKS */
	  set_cut(S_YREG, B->cp_b);
	} else
#endif  /* YAPOR */
	  {
	    pop_yaam_regs();
	    pop_args(ap->ArityOfPE);
	    S_YREG--;
#ifdef FROZEN_STACKS
	    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
	    set_cut(S_YREG, B);
	  }
	SET_BB(B_YREG);
	ENDCACHE_Y();
	JMPNext();
      }
      ENDBOp();



      /*****************************************************************
       *        enter a logical semantics dynamic predicate             *
       *****************************************************************/

      /* only meaningful with THREADS on! */
      /* lock logical updates predicate.  */
      Op(lock_lu, p);
#if PARALLEL_YAP
      if (PP) {
	GONext();
      }
      PP = PREG->y_u.p.p;
      PELOCK(3, PP);
#endif
      PREG = NEXTOP(PREG, p);
      GONext();
      ENDOp();

      /* only meaningful with THREADS on! */
      /* lock logical updates predicate.  */
      Op(unlock_lu, e);
#if defined(YAPOR) || defined(THREADS)
      if (PP) {
	UNLOCKPE(1,PP);
	PP = NULL;
      }
#endif
      PREG = NEXTOP(PREG, e);
      GONext();
      ENDOp();


      /* enter logical pred               */
      BOp(alloc_for_logical_pred, L);
      check_trail(TR);
      /* say that an environment is using this clause */
      /* we have our own copy for the clause */
#if MULTIPLE_STACKS
      {
	LogUpdClause *cl = PREG->y_u.L.ClBase;
#if PARALLEL_YAP
	PredEntry *ap = cl->ClPred;
#endif

	/* always add an extra reference */
	INC_CLREF_COUNT(cl);
	TRAIL_CLREF(cl);
	UNLOCKPE(2,ap);
	PP = NULL;
      }
#else
      {
        LogUpdClause *cl = (LogUpdClause *)PREG->y_u.L.ClBase;
        if (!(cl->ClFlags & InUseMask)) {
          cl->ClFlags |= InUseMask;
          TRAIL_CLREF(cl);
        }
      }
#endif
      PREG = NEXTOP(PREG, L);
      JMPNext();
      ENDBOp();

      /* copy database term               */
      BOp(copy_idb_term, e);
      {
	LogUpdClause *cl = ClauseCodeToLogUpdClause(PREG);
	Term t;

	SET_ASP(YREG, E_CB*sizeof(CELL));
	saveregs();
	while ((t = Yap_FetchTermFromDB(cl->lusl.ClSource)) == 0L) {
	  if (PP) UNLOCKPE(3,PP);
#if defined(YAPOR) || defined(THREADS)
	  PP = NULL;
#endif
	    if (!Yap_gc(3, ENV, CP)) {
	      Yap_NilError(RESOURCE_ERROR_STACK, "stack overflow: gc failed");
	      FAIL();
	    }
#if defined(YAPOR) || defined(THREADS)
	  PELOCK(5,ClauseCodeToLogUpdClause(PREG)->ClPred);
	  PP = ClauseCodeToLogUpdClause(PREG)->ClPred;
#endif
	}
	if (!Yap_IUnify(ARG2, t)) {
	  setregs();
#if defined(YAPOR) || defined(THREADS)
	  if (PP) UNLOCKPE(6,PP);
	  PP = NULL;
#endif
	  FAIL();
	}
	if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) {
	  setregs();
#if defined(YAPOR) || defined(THREADS)
	  if (PP) UNLOCKPE(5,PP);
	  PP = NULL;
#endif
	  FAIL();
	}
	setregs();

#if MULTIPLE_STACKS
	/* always add an extra reference */
	INC_CLREF_COUNT(cl);
	TRAIL_CLREF(cl);
	if (PP) UNLOCKPE(7,PP);
	PP = NULL;
#else
	if (!(cl->ClFlags & InUseMask)) {
	  /* Clause *cl = (Clause *)PREG->y_u.EC.ClBase;

	     PREG->y_u.EC.ClTrail = TR-(tr_fr_ptr)LOCAL_TrailBase;
	     PREG->y_u.EC.ClENV = LCL0-YREG;*/
	  cl->ClFlags |= InUseMask;
	  TRAIL_CLREF(cl);
	}
#endif
      }
      PREG = CPREG;
      YREG = ENV;
#ifdef DEPTH_LIMIT
      DEPTH = YREG[E_DEPTH];
#endif
      JMPNext();
      ENDBOp();


      /* unify with database term               */
      BOp(unify_idb_term, e);
      {
	LogUpdClause *cl = ClauseCodeToLogUpdClause(PREG);

	saveregs();
	if (!Yap_IUnify(ARG2, cl->lusl.ClSource->Entry)) {
	  setregs();
	  UNLOCKPE(8,PP);
#if defined(YAPOR) || defined(THREADS)
	  PP = NULL;
#endif
	  FAIL();
	}
	if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) {
	  setregs();
	  UNLOCKPE(9,PP);
#if defined(YAPOR) || defined(THREADS)
	  PP = NULL;
#endif
	  FAIL();
	}
	setregs();

	/* say that an environment is using this clause */
	/* we have our own copy for the clause */
#if MULTIPLE_STACKS
	/* always add an extra reference */
	INC_CLREF_COUNT(cl);
	TRAIL_CLREF(cl);
	UNLOCKPE(10,PP);
	PP = NULL;
#else
	if (!(cl->ClFlags & InUseMask)) {
	  /* Clause *cl = (Clause *)PREG->y_u.EC.ClBase;

	     PREG->y_u.EC.ClTrail = TR-(tr_fr_ptr)LOCAL_TrailBase;
	     PREG->y_u.EC.ClENV = LCL0-YREG;*/
	  cl->ClFlags |= InUseMask;
	  TRAIL_CLREF(cl);
	}
#endif
      }
      PREG = CPREG;
      YREG = ENV;
#ifdef DEPTH_LIMIT
      DEPTH = YREG[E_DEPTH];
#endif
      JMPNext();
      ENDBOp();


      /*****************************************************************
       *        check for enough room                                  *
       *****************************************************************/

      /* ensure_space                   */
      BOp(ensure_space, Osbpa);
      {
	Int sz =  PREG->y_u.Osbpa.i;
	UInt arity = PREG->y_u.Osbpa.p->ArityOfPE;

	if (Unsigned(HR) + sz > Unsigned(YREG)-StackGap( PASS_REGS1 )) {
	  YENV[E_CP] = (CELL) CPREG;
	  YENV[E_E] = (CELL) ENV;
#ifdef DEPTH_LIMIT
	  YENV[E_DEPTH] = DEPTH;
#endif  /* DEPTH_LIMIT */
	  SET_ASP(YREG, PREG->y_u.Osbpa.s);
	  PREG = NEXTOP(PREG,Osbpa);
	  saveregs();
	  if (!Yap_gcl(sz, arity, YENV, PREG)) {
	    Yap_NilError(RESOURCE_ERROR_STACK,"stack overflow: gc failed");
	    setregs();
	    FAIL();
	  } else {
	    setregs();
	  }
	} else {
	  PREG = NEXTOP(PREG,Osbpa);
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
      UNLOCKPE(11,(PredEntry *)(PREG->y_u.Otapl.p));
      saveregs();
      spy_goal( PASS_REGS1 );
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
	PELOCK(6,PREG->y_u.Otapl.p);
	PP = PREG->y_u.Otapl.p;
      }
      if (PREG->y_u.Otapl.p->CodeOfPred != PREG) {
	/* oops, someone changed the procedure under our feet,
	   fortunately this is no big deal because we haven't done
	   anything yet */
	PP = NULL;
	PREG = PREG->y_u.Otapl.p->CodeOfPred;
	UNLOCKPE(12,PREG->y_u.Otapl.p);
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
      UNLOCKPE(13,((PredEntry *)(PREG->y_u.Otapl.p)));
      BEGD(d1);
      d1 = PREG->y_u.Otapl.s;
      store_args(d1);
      store_yaam_regs(PREG, 0);
      ENDD(d1);
      set_cut(S_YREG, B);
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif  /* YAPOR */
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
      PREG = NEXTOP(PREG,Otapl);
      JMPNext();

      ENDBOp();

      BOp(count_retry_and_mark, Otapl);
      LOCAL_RetriesCounter--;
      if (LOCAL_RetriesCounter == 0) {
	saveregs();
	Yap_NilError(RETRY_COUNTER_UNDERFLOW,"");
	setregs();
	JMPNext();
      }
      LOCAL_PredEntriesCounter--;
      if (LOCAL_PredEntriesCounter == 0) {
	saveregs();
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,"");
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
      PELOCK(7,PREG->y_u.Otapl.p);
      CACHE_Y(B);
      PREG = PREG->y_u.Otapl.d;
      LOCK(DynamicLock(PREG));
      UNLOCK(PREG->y_u.Otapl.p->PELock);
      restore_yaam_regs(PREG);
      restore_args(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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

      /*****************************************************************
       *        Failure                                                 *
       *****************************************************************/

      /* trust_fail                       */
      BOp(trust_fail, e);
      {
	while (POP_CHOICE_POINT(B->cp_b))
	  {
	    POP_EXECUTE();
	  }
      }
#ifdef YAPOR
      {
        choiceptr cut_pt;
        cut_pt = B->cp_b;
        CUT_prune_to(cut_pt);
        B = cut_pt;
      }
#else
      B = B->cp_b;
#endif  /* YAPOR */
      goto fail;
      ENDBOp();

#ifdef YAPOR
    shared_fail:
      B = Get_LOCAL_top_cp();
      SET_BB(PROTECT_FROZEN_B(B));
      goto fail;
#endif  /* YAPOR */

      /* fail                             */
      PBOp(op_fail, e);

      if (PP) {
	UNLOCK(PP->PELock);
	PP = NULL;
      }
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
      check_stack(NoStackFail, HR);
      ENDCACHE_Y_AS_ENV();
#endif

    fail:
      {
	register tr_fr_ptr pt0 = TR;
#if defined(YAPOR) || defined(THREADS)
	if (PP) {
	  UNLOCK(PP->PELock);
	  PP = NULL;
	}
#endif
	PREG = B->cp_ap;
	save_pc();
	CACHE_TR(B->cp_tr);
	PREFETCH_OP(PREG);
      failloop:
	if (pt0 == S_TR) {
	  SP = SP0;
#ifdef LOW_LEVEL_TRACER
	  if (Yap_do_low_level_trace) {
	    int go_on = true;
	    yamop *ipc = PREG;

	    while (go_on) {
	      op_numbers opnum = Yap_op_from_opcode(ipc->opc);

	      go_on = false;
	      switch (opnum) {
#ifdef TABLING
	      case _table_load_answer:
		low_level_trace(retry_table_loader, LOAD_CP(B)->cp_pred_entry, NULL);
		break;
	      case _table_try_answer:
	      case _table_retry_me:
	      case _table_trust_me:
	      case _table_retry:
	      case _table_trust:
	      case _table_completion:
#ifdef THREADS_CONSUMER_SHARING
	      case _table_answer_resolution_completion:
#endif /* THREADS_CONSUMER_SHARING */
#ifdef DETERMINISTIC_TABLING
		if (IS_DET_GEN_CP(B))
		  low_level_trace(retry_table_generator, DET_GEN_CP(B)->cp_pred_entry, NULL);
		else
#endif /* DETERMINISTIC_TABLING */
		  low_level_trace(retry_table_generator, GEN_CP(B)->cp_pred_entry, (CELL *)(GEN_CP(B) + 1));
		break;
	      case _table_answer_resolution:
		low_level_trace(retry_table_consumer, CONS_CP(B)->cp_pred_entry, NULL);
		break;
	      case _trie_trust_var:
	      case _trie_retry_var:
	      case _trie_trust_var_in_pair:
	      case _trie_retry_var_in_pair:
	      case _trie_trust_val:
	      case _trie_retry_val:
	      case _trie_trust_val_in_pair:
	      case _trie_retry_val_in_pair:
	      case _trie_trust_atom:
	      case _trie_retry_atom:
	      case _trie_trust_atom_in_pair:
	      case _trie_retry_atom_in_pair:
	      case _trie_trust_null:
	      case _trie_retry_null:
	      case _trie_trust_null_in_pair:
	      case _trie_retry_null_in_pair:
	      case _trie_trust_pair:
	      case _trie_retry_pair:
	      case _trie_trust_appl:
	      case _trie_retry_appl:
	      case _trie_trust_appl_in_pair:
	      case _trie_retry_appl_in_pair:
	      case _trie_trust_extension:
	      case _trie_retry_extension:
	      case _trie_trust_double:
	      case _trie_retry_double:
	      case _trie_trust_longint:
	      case _trie_retry_longint:
	      case _trie_trust_gterm:
	      case _trie_retry_gterm:
		low_level_trace(retry_table_loader, UndefCode, NULL);
		break;
#endif /* TABLING */
	      case _or_else:
	      case _or_last:
		low_level_trace(retry_or, (PredEntry *)ipc, &(B->cp_a1));
		break;
	      case _retry2:
	      case _retry3:
	      case _retry4:
		ipc = NEXTOP(ipc,l);
		go_on = true;
		break;
	      case _jump:
		ipc = ipc->y_u.l.l;
		go_on = true;
		break;
	      case _retry_c:
	      case _retry_userc:
		low_level_trace(retry_pred, ipc->y_u.OtapFs.p, B->cp_args);
		break;
	      case _retry_profiled:
	      case _count_retry:
		ipc = NEXTOP(ipc,p);
		go_on = true;
		break;
	      case _retry_me:
	      case _trust_me:
	      case _count_retry_me:
	      case _count_trust_me:
	      case _profiled_retry_me:
	      case _profiled_trust_me:
	      case _retry_and_mark:
	      case _profiled_retry_and_mark:
	      case _retry:
	      case _trust:
		low_level_trace(retry_pred, ipc->y_u.Otapl.p, B->cp_args);
		break;
	      case _try_logical:
	      case _retry_logical:
	      case _profiled_retry_logical:
	      case _count_retry_logical:
	      case _trust_logical:
	      case _profiled_trust_logical:
	      case _count_trust_logical:
		low_level_trace(retry_pred, ipc->y_u.OtILl.d->ClPred, B->cp_args);
		break;
	      case _Nstop:
	      case _Ystop:
		low_level_trace(retry_pred, NULL, B->cp_args);
		break;
	      default:
		break;
	      }
	    }
	  }
#endif  /* LOW_LEVEL_TRACER */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
	  if (pt0 < TR_FZ || pt0 > (ADDR)CurrentTrailTop+MinTrailGap)
#else
	    if (pt0 < TR_FZ)
#endif /* YAPOR_SBA */
	      {
		TR = TR_FZ;
		TRAIL_LINK(pt0);
	      } else
#endif /* FROZEN_STACKS */
	      RESTORE_TR();
	  GONext();
	}
	BEGD(d1);
	d1 = TrailTerm(pt0-1);
	pt0--;
	if (IsVarTerm(d1)) {
#if defined(YAPOR_SBA) && defined(YAPOR)
	  /* clean up the trail when we backtrack */
	  if (Unsigned((Int)(d1)-(Int)(H_FZ)) >
	      Unsigned((Int)(B_FZ)-(Int)(H_FZ))) {
	    RESET_VARIABLE(STACK_TO_SBA(d1));
	  } else
#endif
	    /* normal variable */
	    RESET_VARIABLE(d1);
	  goto failloop;
	}
	/* pointer to code space */
	/* or updatable variable */
#if defined(TERM_EXTENSIONS) || defined(FROZEN_STACKS) || defined(MULTI_ASSIGNMENT_VARIABLES)
	if (IsPairTerm(d1))
#endif /* TERM_EXTENSIONS || FROZEN_STACKS || MULTI_ASSIGNMENT_VARIABLES */
	  {
	    register CELL flags;
	    CELL *pt1 = RepPair(d1);
#ifdef LIMIT_TABLING
	    if ((ADDR) pt1 == LOCAL_TrailBase) {
	      sg_fr_ptr sg_fr = (sg_fr_ptr) TrailVal(pt0);
	      TrailTerm(pt0) = AbsPair((CELL *)(pt0 - 1));
	      SgFr_state(sg_fr)--;  /* complete_in_use --> complete : compiled_in_use --> compiled */
	      insert_into_global_sg_fr_list(sg_fr);
	      goto failloop;
	    }
#endif /* LIMIT_TABLING */
#ifdef FROZEN_STACKS  /* TRAIL */
	    /* avoid frozen segments */
	    if (
#ifdef YAPOR_SBA
		(ADDR) pt1 >= HeapTop
#else
		IN_BETWEEN(LOCAL_TrailBase, pt1, (ADDR)CurrentTrailTop+MinTrailGap)
#endif /* YAPOR_SBA */
		)
	      {
		pt0 = (tr_fr_ptr) pt1;
		goto failloop;
	      } else
#endif /* FROZEN_STACKS */
	      if (IN_BETWEEN(H0,pt1,HR)) {
		if (IsAttVar(pt1)) {
		  goto failloop;
		} else if (*pt1 == (CELL)FunctorBigInt) {
		  Yap_CleanOpaqueVariable(pt1);
		  goto failloop;
		}
	      }
#ifdef FROZEN_STACKS  /* TRAIL */
	    /* don't reset frozen variables */
	    if (pt0 < TR_FZ)
	      goto failloop;
#endif
	    flags = *pt1;
#if MULTIPLE_STACKS
	    if (FlagOn(DBClMask, flags)) {
	      DBRef dbr = DBStructFlagsToDBStruct(pt1);
	      int erase;

	      LOCK(dbr->lock);
	      DEC_DBREF_COUNT(dbr);
	      erase = (dbr->Flags & ErasedMask) && (dbr->ref_count == 0);
	      UNLOCK(dbr->lock);
	      if (erase) {
		saveregs();
		Yap_ErDBE(dbr);
		setregs();
	      }
	    } else {
	      if (flags & LogUpdMask) {
		if (flags & IndexMask) {
		  LogUpdIndex *cl = ClauseFlagsToLogUpdIndex(pt1);
		  int erase;
#if PARALLEL_YAP
		  PredEntry *ap = cl->ClPred;
#endif

		  PELOCK(8,ap);
		  DEC_CLREF_COUNT(cl);
		  erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount);
		  if (erase) {
		    saveregs();
		    /* at this point,
		       we are the only ones accessing the clause,
		       hence we don't need to have a lock it */
		    Yap_ErLogUpdIndex(cl);
		    setregs();
		  } else if (cl->ClFlags & DirtyMask) {
		    saveregs();
		    /* at this point,
		       we are the only ones accessing the clause,
		       hence we don't need to have a lock it */
		    Yap_CleanUpIndex(cl);
		    setregs();
		  }
		  UNLOCK(ap->PELock);
		} else {
		  LogUpdClause *cl = ClauseFlagsToLogUpdClause(pt1);
		  int erase;
#if PARALLEL_YAP
		  PredEntry *ap = cl->ClPred;
#endif
		  /* BB support */
		  if (ap) {

		    PELOCK(9,ap);
		    DEC_CLREF_COUNT(cl);
		    erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount);
		    if (erase) {
		      saveregs();
		      /* at this point,
			 we are the only ones accessing the clause,
			 hence we don't need to have a lock it */
		      Yap_ErLogUpdCl(cl);
		      setregs();
		    }
		    UNLOCK(ap->PELock);
		  }
		}
	      } else {
		DynamicClause *cl = ClauseFlagsToDynamicClause(pt1);
		int erase;

		LOCK(cl->ClLock);
		DEC_CLREF_COUNT(cl);
		erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount);
		UNLOCK(cl->ClLock);
		if (erase) {
		  saveregs();
		  /* at this point,
		     we are the only ones accessing the clause,
		     hence we don't need to have a lock it */
		  Yap_ErCl(cl);
		  setregs();
		}
	      }
	    }
#else
	    ResetFlag(InUseMask, flags);
	    *pt1 = flags;
	    if (FlagOn((ErasedMask|DirtyMask), flags)) {
	      if (FlagOn(DBClMask, flags)) {
		saveregs();
		Yap_ErDBE(DBStructFlagsToDBStruct(pt1));
		setregs();
	      } else {
		saveregs();
		if (flags & LogUpdMask) {
		  if (flags & IndexMask) {
		    if (FlagOn(ErasedMask, flags)) {
		      Yap_ErLogUpdIndex(ClauseFlagsToLogUpdIndex(pt1));
		    } else {
		      Yap_CleanUpIndex(ClauseFlagsToLogUpdIndex(pt1));
		    }
		  } else {
		    Yap_ErLogUpdCl(ClauseFlagsToLogUpdClause(pt1));
		  }
		} else {
		  Yap_ErCl(ClauseFlagsToDynamicClause(pt1));
		}
		setregs();
	      }
	    }
#endif
	    goto failloop;
	  }
#ifdef MULTI_ASSIGNMENT_VARIABLES
	else /* if (IsApplTerm(d1)) */ {
	  CELL *pt = RepAppl(d1);
	  /* AbsAppl means */
	  /* multi-assignment variable */
	  /* so the next cell is the old value */
#ifdef FROZEN_STACKS
	  --pt0;
	  pt[0] = TrailVal(pt0);
#else
	  pt[0] = TrailTerm(pt0-1);
	  pt0 -= 2;
#endif /* FROZEN_STACKS */
	  goto failloop;
	}
#endif
	ENDD(d1);
	ENDCACHE_TR();
      }

#ifdef COROUTINING
    NoStackFail:
      BEGD(d0);
#ifdef SHADOW_S
      Yap_REGS.S_ = SREG;
#endif
      saveregs();
      d0 = interrupt_fail( PASS_REGS1 );
      setregs();
#ifdef SHADOW_S
      SREG = Yap_REGS.S_;
#endif
      if (!d0) FAIL();
      JMPNext();
      ENDD(d0);

#endif /* COROUTINING */
      ENDPBOp();



      /************************************************************************\
       *      Cut & Commit Instructions                                       *
\************************************************************************/

      /* cut                              */
      Op(cut, s);
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
      check_stack(NoStackCut, HR);
      ENDCACHE_Y_AS_ENV();
    do_cut:
#endif
      SET_ASP(YREG, PREG->y_u.s.s);
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, s),Osbpp),l);
      /* assume cut is always in stack */
      saveregs();
      prune((choiceptr)YREG[E_CB] PASS_REGS);
      setregs();
      GONext();

#ifdef COROUTINING
    NoStackCut:
      PROCESS_INT(interrupt_cut, do_cut);
#endif

      ENDOp();

      /* cut_t                            */
      /* cut_t does the same as cut */
      Op(cut_t, s);
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
      check_stack(NoStackCutT, HR);
      ENDCACHE_Y_AS_ENV();
    do_cut_t:
#endif
      SET_ASP(YREG, PREG->y_u.s.s);
      /* assume cut is always in stack */
      saveregs();
      prune((choiceptr)YREG[E_CB] PASS_REGS);
      setregs();
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, s),Osbpp),l);
      GONext();

#ifdef COROUTINING
    NoStackCutT:
      PROCESS_INT(interrupt_cut_t, do_cut_t);
#endif

      ENDOp();

      /* cut_e                            */
      Op(cut_e, s);
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
      check_stack(NoStackCutE, HR);
      ENDCACHE_Y_AS_ENV();
    do_cut_e:
#endif
      SET_ASP(YREG, PREG->y_u.s.s);
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, s),Osbpp),l);
      saveregs();
      prune((choiceptr)SREG[E_CB] PASS_REGS);
      setregs();
      GONext();

#ifdef COROUTINING
    NoStackCutE:
      PROCESS_INT(interrupt_cut_e, do_cut_e);
#endif

      ENDOp();

      /* save_b_x      Xi                 */
      Op(save_b_x, x);
      BEGD(d0);
      d0 = PREG->y_u.x.x;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
      XREG(d0) = MkIntegerTerm((Int)B);
#else
      XREG(d0) = MkIntegerTerm(LCL0-(CELL *) (B));
#endif /* YAPOR_SBA && FROZEN_STACKS */
      PREG = NEXTOP(PREG, x);
      ENDD(d0);
      GONext();
      ENDOp();

      /* save_b_y      Yi                 */
      Op(save_b_y, y);
#if defined(YAPOR_SBA)
      INITIALIZE_PERMVAR(YREG+PREG->y_u.y.y,MkIntegerTerm((Int)B));
#else
      INITIALIZE_PERMVAR(YREG+PREG->y_u.y.y,MkIntegerTerm(LCL0-(CELL *)(B)));
#endif /* YAPOR_SBA*/
      PREG = NEXTOP(PREG, y);
      GONext();
      ENDOp();

      /* commit_b_x    Xi                 */
      Op(commit_b_x, xps);
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
      check_stack(NoStackCommitX, HR);
      ENDCACHE_Y_AS_ENV();
    do_commit_b_x:
#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xps.x);
      deref_head(d0, commit_b_x_unk);
    commit_b_x_nvar:
      /* skip a void call and a label */
      SET_ASP(YREG, PREG->y_u.xps.s);
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xps),Osbpp),l);
      {
	choiceptr pt0;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
	pt0 = (choiceptr)IntegerOfTerm(d0);
#else
	pt0 = (choiceptr)(LCL0-IntegerOfTerm(d0));
#endif /* YAPOR_SBA && FROZEN_STACKS */
	saveregs();
	prune(pt0 PASS_REGS);
	setregs();
      }
      GONext();

      BEGP(pt1);
      deref_body(d0, pt1, commit_b_x_unk, commit_b_x_nvar);
      ENDP(pt1);
      /* never cut to a variable */
      /* Abort */
      FAIL();
      ENDD(d0);

#ifdef COROUTINING
      /* Problem: have I got an environment or not? */
    NoStackCommitX:
      PROCESS_INT(interrupt_commit_x, do_commit_b_x);
#endif
      ENDOp();

      /* commit_b_y    Yi                 */
      Op(commit_b_y, yps);
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
      check_stack(NoStackCommitY, HR);
      ENDCACHE_Y_AS_ENV();
    do_commit_b_y:
#endif
      BEGD(d0);
      d0 = YREG[PREG->y_u.yps.y];
      deref_head(d0, commit_b_y_unk);
    commit_b_y_nvar:
      SET_ASP(YREG, PREG->y_u.yps.s);
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yps),Osbpp),l);
      {
	choiceptr pt0;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
	pt0 = (choiceptr)IntegerOfTerm(d0);
#else
	pt0 = (choiceptr)(LCL0-IntegerOfTerm(d0));
#endif
	saveregs();
	prune(pt0 PASS_REGS);
	setregs();
      }
      GONext();

      BEGP(pt1);
      deref_body(d0, pt1, commit_b_y_unk, commit_b_y_nvar);
      ENDP(pt1);
      /* never cut to a variable */
      /* Abort */
      FAIL();
      ENDD(d0);

#ifdef COROUTINING
      /* This is easier: I know there is an environment so I cannot do allocate */
    NoStackCommitY:
      PROCESS_INT(interrupt_commit_y, do_commit_b_y);
#endif
      ENDOp();

      /*************************************************************************
       *      Call / Proceed instructions                                      *
       *************************************************************************/

      /* Macros for stack trimming                                            */

      /* execute     Label               */
      BOp(execute, pp);
      {
	PredEntry *pt0;
	CACHE_Y_AS_ENV(YREG);
	pt0 = PREG->y_u.pp.p;
#ifndef NO_CHECKING
	check_stack(NoStackExecute, HR);
	goto skip_do_execute;
#endif
      do_execute:
	FETCH_Y_FROM_ENV(YREG);
	pt0 = PREG->y_u.pp.p;
      skip_do_execute:
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  low_level_trace(enter_pred,pt0,XREGS+1);
	}
#endif  /* LOW_LEVEL_TRACE */
	CACHE_A1();
	ALWAYS_LOOKAHEAD(pt0->OpcodeOfPred);
	BEGD(d0);
	d0 = (CELL)B;
	PREG = pt0->CodeOfPred;
	/* for profiler */
	save_pc();
	ENV_YREG[E_CB] = d0;
	ENDD(d0);
#ifdef DEPTH_LIMIT
	if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is prolog */
	  if (pt0->ModuleOfPred) {
	    if (DEPTH == MkIntTerm(0))
	      FAIL();
	    else DEPTH = RESET_DEPTH();
	  }
	} else if (pt0->ModuleOfPred)
	  DEPTH -= MkIntConstant(2);
#endif  /* DEPTH_LIMIT */
	/* this is the equivalent to setting up the stack */
	ALWAYS_GONext();
	ALWAYS_END_PREFETCH();
	ENDCACHE_Y_AS_ENV();
      }

    NoStackExecute:
      PROCESS_INT(interrupt_execute, do_execute);

      ENDBOp();

      /* dexecute    Label               */
      /* joint deallocate and execute */
      BOp(dexecute, pp);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace)
	low_level_trace(enter_pred,PREG->y_u.pp.p,XREGS+1);
#endif  /* LOW_LEVEL_TRACER */
      CACHE_Y_AS_ENV(YREG);
      {
	PredEntry *pt0;

	CACHE_A1();
	pt0 = PREG->y_u.pp.p;
#ifndef NO_CHECKING
	/* check stacks */
	check_stack(NoStackDExecute, HR);
	goto skip_dexecute;
#endif
      continue_dexecute:
	FETCH_Y_FROM_ENV(YREG);
	pt0 = PREG->y_u.pp.p;
      skip_dexecute:
#ifdef DEPTH_LIMIT
	if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is primitives */
	  if (pt0->ModuleOfPred) {
	    if (DEPTH == MkIntTerm(0))
	      FAIL();
	    else DEPTH = RESET_DEPTH();
	  }
	} else if (pt0->ModuleOfPred)
	  DEPTH -= MkIntConstant(2);
#endif  /* DEPTH_LIMIT */
	PREG = pt0->CodeOfPred;
	/* for profiler */
	save_pc();
	ALWAYS_LOOKAHEAD(pt0->OpcodeOfPred);
	/* do deallocate */
	CPREG = (yamop *) ENV_YREG[E_CP];
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E];
#ifdef FROZEN_STACKS
	{
	  choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) ENV_YREG = (CELL *) top_b;
#else
	  if (ENV_YREG > (CELL *) top_b) ENV_YREG = (CELL *) top_b;
#endif /* YAPOR_SBA */
	  else ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size(CPREG));
	}
#else
	if (ENV_YREG > (CELL *)B) {
	  ENV_YREG = (CELL *)B;
	}
	else {
	  ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size(CPREG));
	}
#endif /* FROZEN_STACKS */
	WRITEBACK_Y_AS_ENV();
	/* setup GB */
	ENV_YREG[E_CB] = (CELL) B;
	ALWAYS_GONext();
	ALWAYS_END_PREFETCH();
      }
      ENDCACHE_Y_AS_ENV();

    NoStackDExecute:
      PROCESS_INT(interrupt_dexecute, continue_dexecute);

      ENDBOp();

      BOp(fcall, Osbpp);
      CACHE_Y_AS_ENV(YREG);
      ENV_YREG[E_CP] = (CELL) CPREG;
      ENV_YREG[E_E] = (CELL) ENV;
#ifdef DEPTH_LIMIT
      ENV_YREG[E_DEPTH] = DEPTH;
#endif  /* DEPTH_LIMIT */
      ENDCACHE_Y_AS_ENV();
      ENDBOp();

      BOp(call, Osbpp);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
	low_level_trace(enter_pred,PREG->y_u.Osbpp.p,XREGS+1);
      }
#endif  /* LOW_LEVEL_TRACER */
      CACHE_Y_AS_ENV(YREG);
      {
	PredEntry *pt;
	CACHE_A1();
	pt = PREG->y_u.Osbpp.p;
#ifndef NO_CHECKING
	check_stack(NoStackCall, HR);
	goto skip_call;
#endif
      call_body:
	/* external jump if we don;t want to creep */
	FETCH_Y_FROM_ENV(YREG);
	pt = PREG->y_u.Osbpp.p;
      skip_call:
	ENV = ENV_YREG;
	/* Try to preserve the environment */
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + PREG->y_u.Osbpp.s);
	CPREG = NEXTOP(PREG, Osbpp);
	ALWAYS_LOOKAHEAD(pt->OpcodeOfPred);
	PREG = pt->CodeOfPred;
	/* for profiler */
	save_pc();
#ifdef DEPTH_LIMIT
	if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is primitives */
	  if (pt->ModuleOfPred) {
	    if (DEPTH == MkIntTerm(0))
	      FAIL();
	    else DEPTH = RESET_DEPTH();
	  }
	} else if (pt->ModuleOfPred)
	  DEPTH -= MkIntConstant(2);
#endif  /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
	{
	  choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) ENV_YREG = (CELL *) top_b;
#else
	  if (ENV_YREG > (CELL *) top_b) ENV_YREG = (CELL *) top_b;
#endif /* YAPOR_SBA */
	}
#else
	if (ENV_YREG > (CELL *) B) {
	  ENV_YREG = (CELL *) B;
	}
#endif /* FROZEN_STACKS */
	WRITEBACK_Y_AS_ENV();
	/* setup GB */
	ENV_YREG[E_CB] = (CELL) B;
#ifdef YAPOR
	SCH_check_requests();
#endif  /* YAPOR */
	ALWAYS_GONext();
	ALWAYS_END_PREFETCH();
      }
      ENDCACHE_Y_AS_ENV();
      ENDBOp();

      BOp(procceed, p);
      CACHE_Y_AS_ENV(YREG);
      ALWAYS_LOOKAHEAD(CPREG->opc);
      PREG = CPREG;
      /* for profiler */
      save_pc();
      ENV_YREG = ENV;
#ifdef DEPTH_LIMIT
      DEPTH = ENV_YREG[E_DEPTH];
#endif
      WRITEBACK_Y_AS_ENV();
      ALWAYS_GONext();
      ALWAYS_END_PREFETCH();
      ENDCACHE_Y_AS_ENV();

    NoStackCall:
      PROCESS_INT(interrupt_call, call_body);

      ENDBOp();

      Op(allocate, e);
      CACHE_Y_AS_ENV(YREG);
      PREG = NEXTOP(PREG, e);
      ENV_YREG[E_CP] = (CELL) CPREG;
      ENV_YREG[E_E] = (CELL) ENV;
#ifdef DEPTH_LIMIT
      ENV_YREG[E_DEPTH] = DEPTH;
#endif  /* DEPTH_LIMIT */
      ENV = ENV_YREG;
      ENDCACHE_Y_AS_ENV();
      GONext();
      ENDOp();

      Op(deallocate, p);
      CACHE_Y_AS_ENV(YREG);
      check_trail(TR);
      PREG = NEXTOP(PREG, p);
      /* other instructions do depend on S being set by deallocate
	 :-( */
      SREG = YREG;
      CPREG = (yamop *) ENV_YREG[E_CP];
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E];
#ifdef DEPTH_LIMIT
      DEPTH = ENV_YREG[E_DEPTH];
#endif  /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
      {
	choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
	if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) ENV_YREG = (CELL *) top_b;
#else
	if (ENV_YREG > (CELL *) top_b) ENV_YREG = (CELL *) top_b;
#endif /* YAPOR_SBA */
	else ENV_YREG = (CELL *)((CELL) ENV_YREG + ENV_Size(CPREG));
      }
#else
      if (ENV_YREG > (CELL *) B)
	ENV_YREG = (CELL *) B;
      else
	ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size(CPREG));
#endif /* FROZEN_STACKS */
      WRITEBACK_Y_AS_ENV();
#ifndef NO_CHECKING
      /* check stacks */
      check_stack(NoStackDeallocate, HR);
#endif
      ENDCACHE_Y_AS_ENV();
      GONext();

    NoStackDeallocate:
      BEGD(d0);
#ifdef SHADOW_S
      Yap_REGS.S_ = SREG;
#endif
      saveregs();
      d0 = interrupt_deallocate( _deallocate PASS_REGS );
      setregs();
#ifdef SHADOW_S
      SREG = Yap_REGS.S_;
#endif
      if (!d0) FAIL();
      JMPNext();
      ENDD(d0);
      ENDOp();

      /**********************************************
       *        OPTYap instructions                  *
       **********************************************/

#ifdef YAPOR
#include "or.insts.i"
#endif /* YAPOR */
#ifdef TABLING
#include "tab.insts.i"
#include "tab.tries.insts.i"
#endif /* TABLING */



#ifdef BEAM
      extern int eam_am(PredEntry *);

      Op(retry_eam, e);
      printf("Aqui estou eu..................\n");
      if (!eam_am(2)) {
	abort_eam("Falhei\n");
	FAIL();
      }

      goto procceed;
      PREG = NEXTOP(PREG, e);
      GONext();
      ENDOp();

      Op(run_eam, os);
      if (inp==-9000) { /* use indexing to find out valid alternatives */
	extern CELL *beam_ALTERNATIVES;
	*beam_ALTERNATIVES= (CELL *) PREG->y_u.os.opcw;
	beam_ALTERNATIVES++;
	if (OLD_B!=B) goto fail;
#if PUSH_REGS
	Yap_regp=old_regs;
#endif
	return(0);
      }

      saveregs();
      if (!eam_am((PredEntry *) PREG->y_u.os.s)) FAIL();
      setregs();

      /* cut */
      BACKUP_B();
      while (POP_CHOICE_POINT(B->cp_b)) {
	POP_EXECUTE();
      }
      B = B->cp_b;  /* cut_fail */
      HB = B->cp_h; /* cut_fail */
      RECOVER_B();

      if (0) { register choiceptr ccp;
	/* initialize ccp */
#define NORM_CP(CP)            ((choiceptr)(CP))

	YREG = (CELL *) (NORM_CP(YREG) - 1);
	ccp = NORM_CP(YREG);
	store_yaam_reg_cpdepth(ccp);
	ccp->cp_tr = TR;
	ccp->cp_ap = BEAM_RETRY_CODE;
	ccp->cp_h  = HR;
	ccp->cp_b  = B;
	ccp->cp_env= ENV;
	ccp->cp_cp = CPREG;
	B = ccp;
	SET_BB(B);
      }
      goto procceed;
      PREG = NEXTOP(PREG, os);
      GONext();
      ENDOp();
#endif




      /************************************************************************\
       *    Get Instructions                                                  *
\************************************************************************/

      Op(get_x_var, xx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xr);
      XREG(PREG->y_u.xx.xl) = d0;
      PREG = NEXTOP(PREG, xx);
      ENDD(d0);
      GONext();
      ENDOp();

      Op(get_y_var, yx);
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yx.y;
      d0 = XREG(PREG->y_u.yx.x);
      PREG = NEXTOP(PREG, yx);
      INITIALIZE_PERMVAR(pt0,d0);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(get_yy_var, yyxx);
      CACHE_Y(YREG);
      BEGD(d0);
      BEGP(pt0);
      pt0 = S_YREG + PREG->y_u.yyxx.y1;
      d0 = XREG(PREG->y_u.yyxx.x1);
      BEGD(d1);
      BEGP(pt1);
      pt1 = S_YREG + PREG->y_u.yyx.y2;
      d1 = XREG(PREG->y_u.yyxx.x2);
      PREG = NEXTOP(PREG, yyxx);
      INITIALIZE_PERMVAR(pt0,d0);
      INITIALIZE_PERMVAR(pt1,d1);
      ENDP(pt1);
      ENDD(d1);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDCACHE_Y();
      ENDOp();

      /* The code for get_x_val is hard to follow because I use a
       * lot of jumps. The convention is that in the label
       * gval_X_YREG X refers to the state of the first argument, and
       * YREG to the state of the second argument */
      Op(get_x_val, xx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      deref_head(d0, gvalx_unk);

      /* d0 will keep the first argument */
    gvalx_nonvar:
      /* first argument is bound */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      deref_head(d1, gvalx_nonvar_unk);

    gvalx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, xx);
      UnifyBound(d0, d1);

      BEGP(pt0);
      /* deref second argument */
      deref_body(d1, pt0, gvalx_nonvar_unk, gvalx_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, xx);
      YapBind(pt0, d0);
      GONext();

      ENDP(pt0);
      ENDD(d1);

      BEGP(pt0);
      /* first argument may be unbound */
      deref_body(d0, pt0, gvalx_unk, gvalx_nonvar);
      /* first argument is unbound and in pt0 and in d0 */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      deref_head(d1, gvalx_var_unk);

    gvalx_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, xx);
      YapBind(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, gvalx_var_unk, gvalx_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, xx);
      UnifyCells(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      /* The code for get_y_val mostly uses the code for get_x_val
       */

      Op(get_y_val, yx);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yx.y;
      d0 = *pt0;

      /* From now on, it's just a copy of the code for get_x_val */

      deref_head(d0, gvaly_unk);
    gvaly_nonvar:

      /* first argument is bound */
      d1 = XREG(PREG->y_u.yx.x);
      deref_head(d1, gvaly_nonvar_unk);
    gvaly_nonvar_nonvar:

      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, yx);
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      deref_body(d1, pt1, gvaly_nonvar_unk, gvaly_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, yx);
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);


      /* first argument may be unbound */
      derefa_body(d0, pt0, gvaly_unk, gvaly_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->y_u.yx.x);
      deref_head(d1, gvaly_var_unk);

    gvaly_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, yx);
      YapBind(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, gvaly_var_unk, gvaly_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, yx);
      UnifyCells(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_atom, xc);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = XREG(PREG->y_u.xc.x);
      d1 = PREG->y_u.xc.c;

      BEGP(pt0);
      deref_head(d0, gatom_unk);
      /* argument is nonvar */
    gatom_nonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, xc);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_unk, gatom_nonvar);
      /* argument is a variable */
      PREG = NEXTOP(PREG, xc);
      YapBind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_atom_exo, x);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = XREG(PREG->y_u.x.x);
      d1 = *SREG;
      SREG++;

      BEGP(pt0);
      deref_head(d0, gatom_exo_unk);
      /* argument is nonvar */
    gatom_exo_nonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, x);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_exo_unk, gatom_exo_nonvar);
      /* argument is a variable */
      pt0 = (CELL *)d0;
      PREG = NEXTOP(PREG, x);
      YapBind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_2atoms, cc);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
      deref_head(d0, gatom_2unk);
      /* argument is nonvar */
    gatom_2nonvar:
      if (d0 == PREG->y_u.cc.c1) {
	goto gatom_2b;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_2unk, gatom_2nonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cc.c1);
      ENDP(pt0);
    gatom_2b:
      /* fetch arguments */
      d0 = ARG2;
      d1 = PREG->y_u.cc.c2;

      BEGP(pt0);
      deref_head(d0, gatom_2bunk);
      /* argument is nonvar */
    gatom_2bnonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, cc);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_2bunk, gatom_2bnonvar);
      /* argument is a variable */
      PREG = NEXTOP(PREG, cc);
      YapBind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_3atoms, ccc);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
      deref_head(d0, gatom_3unk);
      /* argument is nonvar */
    gatom_3nonvar:
      if (d0 == PREG->y_u.ccc.c1) {
	goto gatom_3b;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_3unk, gatom_3nonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.ccc.c1);
      ENDP(pt0);
    gatom_3b:
      /* fetch arguments */
      d0 = ARG2;

      BEGP(pt0);
      deref_head(d0, gatom_3bunk);
      /* argument is nonvar */
    gatom_3bnonvar:
      if (d0 == PREG->y_u.ccc.c2) {
	goto gatom_3c;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_3bunk, gatom_3bnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.ccc.c2);
      ENDP(pt0);
    gatom_3c:
      /* fetch arguments */
      d0 = ARG3;
      d1 = PREG->y_u.ccc.c3;

      BEGP(pt0);
      deref_head(d0, gatom_3cunk);
      /* argument is nonvar */
    gatom_3cnonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, ccc);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_3cunk, gatom_3cnonvar);
      /* argument is a variable */
      PREG = NEXTOP(PREG, ccc);
      YapBind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_4atoms, cccc);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
      deref_head(d0, gatom_4unk);
      /* argument is nonvar */
    gatom_4nonvar:
      if (d0 == PREG->y_u.cccc.c1) {
	goto gatom_4b;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_4unk, gatom_4nonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccc.c1);
      ENDP(pt0);
    gatom_4b:
      /* fetch arguments */
      d0 = ARG2;

      BEGP(pt0);
      deref_head(d0, gatom_4bunk);
      /* argument is nonvar */
    gatom_4bnonvar:
      if (d0 == PREG->y_u.cccc.c2) {
	goto gatom_4c;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_4bunk, gatom_4bnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccc.c2);
      ENDP(pt0);
    gatom_4c:
      /* fetch arguments */
      d0 = ARG3;

      BEGP(pt0);
      deref_head(d0, gatom_4cunk);
      /* argument is nonvar */
    gatom_4cnonvar:
      if (d0 == PREG->y_u.cccc.c3) {
	goto gatom_4d;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_4cunk, gatom_4cnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccc.c3);
      ENDP(pt0);
    gatom_4d:
      /* fetch arguments */
      d0 = ARG4;
      d1 = PREG->y_u.cccc.c4;

      BEGP(pt0);
      deref_head(d0, gatom_4dunk);
      /* argument is nonvar */
    gatom_4dnonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, cccc);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_4dunk, gatom_4dnonvar);
      /* argument is a variable */
      PREG = NEXTOP(PREG, cccc);
      YapBind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_5atoms, ccccc);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
      deref_head(d0, gatom_5unk);
      /* argument is nonvar */
    gatom_5nonvar:
      if (d0 == PREG->y_u.ccccc.c1) {
	goto gatom_5b;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_5unk, gatom_5nonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.ccccc.c1);
      ENDP(pt0);
    gatom_5b:
      /* fetch arguments */
      d0 = ARG2;

      BEGP(pt0);
      deref_head(d0, gatom_5bunk);
      /* argument is nonvar */
    gatom_5bnonvar:
      if (d0 == PREG->y_u.ccccc.c2) {
	goto gatom_5c;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_5bunk, gatom_5bnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.ccccc.c2);
      ENDP(pt0);
    gatom_5c:
      /* fetch arguments */
      d0 = ARG3;

      BEGP(pt0);
      deref_head(d0, gatom_5cunk);
      /* argument is nonvar */
    gatom_5cnonvar:
      if (d0 == PREG->y_u.ccccc.c3) {
	goto gatom_5d;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_5cunk, gatom_5cnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.ccccc.c3);
      ENDP(pt0);
    gatom_5d:
      /* fetch arguments */
      d0 = ARG4;

      BEGP(pt0);
      deref_head(d0, gatom_5dunk);
      /* argument is nonvar */
    gatom_5dnonvar:
      if (d0 == PREG->y_u.ccccc.c4) {
	goto gatom_5e;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_5dunk, gatom_5dnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.ccccc.c4);
      ENDP(pt0);
    gatom_5e:
      /* fetch arguments */
      d0 = ARG5;
      d1 = PREG->y_u.ccccc.c5;

      BEGP(pt0);
      deref_head(d0, gatom_5eunk);
      /* argument is nonvar */
    gatom_5enonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, ccccc);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_5eunk, gatom_5enonvar);
      /* argument is a variable */
      PREG = NEXTOP(PREG, ccccc);
      YapBind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_6atoms, cccccc);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
      deref_head(d0, gatom_6unk);
      /* argument is nonvar */
    gatom_6nonvar:
      if (d0 == PREG->y_u.cccccc.c1) {
	goto gatom_6b;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_6unk, gatom_6nonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccccc.c1);
      ENDP(pt0);
    gatom_6b:
      /* fetch arguments */
      d0 = ARG2;

      BEGP(pt0);
      deref_head(d0, gatom_6bunk);
      /* argument is nonvar */
    gatom_6bnonvar:
      if (d0 == PREG->y_u.cccccc.c2) {
	goto gatom_6c;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_6bunk, gatom_6bnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccccc.c2);
      ENDP(pt0);
    gatom_6c:
      /* fetch arguments */
      d0 = ARG3;

      BEGP(pt0);
      deref_head(d0, gatom_6cunk);
      /* argument is nonvar */
    gatom_6cnonvar:
      if (d0 == PREG->y_u.cccccc.c3) {
	goto gatom_6d;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_6cunk, gatom_6cnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccccc.c3);
      ENDP(pt0);
    gatom_6d:
      /* fetch arguments */
      d0 = ARG4;

      BEGP(pt0);
      deref_head(d0, gatom_6dunk);
      /* argument is nonvar */
    gatom_6dnonvar:
      if (d0 == PREG->y_u.cccccc.c4) {
	goto gatom_6e;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_6dunk, gatom_6dnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccccc.c4);
      ENDP(pt0);
    gatom_6e:
      /* fetch arguments */
      d0 = ARG5;

      BEGP(pt0);
      deref_head(d0, gatom_6eunk);
      /* argument is nonvar */
    gatom_6enonvar:
      if (d0 == PREG->y_u.cccccc.c5) {
	goto gatom_6f;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_6eunk, gatom_6enonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccccc.c5);
      ENDP(pt0);
    gatom_6f:
      /* fetch arguments */
      d0 = ARG6;
      d1 = PREG->y_u.cccccc.c6;

      BEGP(pt0);
      deref_head(d0, gatom_6funk);
      /* argument is nonvar */
    gatom_6fnonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, cccccc);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_6funk, gatom_6fnonvar);
      /* argument is a variable */
      PREG = NEXTOP(PREG, cccccc);
      YapBind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      /* The next instructions can lead to either the READ stream
       * or the write stream */

      OpRW(get_list, x);
      BEGD(d0);
      d0 = XREG(PREG->y_u.x.x);
      deref_head(d0, glist_unk);

    glist_nonvar:
      /* did we find a list? */
      if (!IsPairTerm(d0)) {
	FAIL();
      }
      START_PREFETCH(x);
      PREG = NEXTOP(PREG, x);
      /* enter read mode */
      SREG = RepPair(d0);
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, glist_unk, glist_nonvar);
      /* glist var */
      /* enter write mode */
      CACHE_S();
      S_SREG = HR;
      START_PREFETCH_W(x);
      PREG = NEXTOP(PREG, x);
      BEGD(d0);
      d0 = AbsPair(S_SREG);
      YapBind(pt0, d0);
      S_SREG = HR;
      /* don't put an ENDD just after a label */
      HR = S_SREG + 2;
      ENDD(d0);
      WRITEBACK_S(S_SREG);
      GONextW();


      END_PREFETCH_W();
      ENDCACHE_S();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpRW(get_struct, xfa);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xfa.x);
      deref_head(d0, gstruct_unk);

    gstruct_nonvar:
      if (!IsApplTerm(d0))
	FAIL();
      /* we have met a compound term */
      START_PREFETCH(xfa);
      CACHE_S();
      S_SREG = RepAppl(d0);
      /* check functor */
      d0 = (CELL) (PREG->y_u.xfa.f);
      if (*S_SREG != d0) {
	FAIL();
      }
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      PREG = NEXTOP(PREG, xfa);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, gstruct_unk, gstruct_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH_W(xfa);
      BEGD(d1);
      d1 = AbsAppl(HR);
      YapBind(pt0, d1);
      /* now, set pt0 to point to the heap where we are going to
       * build our term */
      pt0 = HR;
      ENDD(d1);
      /* first, put the functor */
      d0 = (CELL) (PREG->y_u.xfa.f);
      *pt0++ = d0;
      HR = pt0 + PREG->y_u.xfa.a;
      PREG = NEXTOP(PREG, xfa);
      /* set SREG */
      SREG = pt0;
      /* update HR */
      GONextW();
      END_PREFETCH_W();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      Op(get_float, xd);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xd.x);
      deref_head(d0, gfloat_unk);

    gfloat_nonvar:
      if (!IsApplTerm(d0))
	FAIL();
      /* we have met a preexisting float */
      START_PREFETCH(xd);
      BEGP(pt0);
      pt0 = RepAppl(d0);
      /* check functor */
      if (*pt0 != (CELL)FunctorDouble) {
	FAIL();
      }
      BEGP(pt1);
      pt1 = PREG->y_u.xd.d;
      PREG = NEXTOP(PREG, xd);
      if (
	  pt1[1] != pt0[1]
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
	  || pt1[2] != pt0[2]
#endif
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, gfloat_unk, gfloat_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH(xc);
      BEGD(d1);
      d1 = AbsAppl(PREG->y_u.xd.d);
      PREG = NEXTOP(PREG, xd);
      YapBind(pt0, d1);
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(get_string, xu);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xu.x);
      deref_head(d0, gstring_unk);

    gstring_nonvar:
      if (!IsApplTerm(d0))
	FAIL();
      /* we have met a preexisting string */
      START_PREFETCH(xu);
      BEGP(pt0);
      pt0 = RepAppl(d0);
      /* check functor */
      if (*pt0 != (CELL)FunctorString) {
	FAIL();
      }
      BEGP(pt1);
      pt1 = RepAppl(PREG->y_u.xu.ut);
      PREG = NEXTOP(PREG, xu);
      if (
	  pt1[1] != pt0[1] ||
	  strcmp((const char *)(pt1+2), (const char *)(pt0+2))
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, gstring_unk, gstring_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH(xc);
      BEGD(d1);
      d1 = PREG->y_u.xu.ut;
      PREG = NEXTOP(PREG, xu);
      YapBind(pt0, d1);
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(get_longint, xi);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xi.x);
      deref_head(d0, glongint_unk);

    glongint_nonvar:
      if (!IsApplTerm(d0))
	FAIL();
      /* we have met a preexisting longint */
      START_PREFETCH(xi);
      BEGP(pt0);
      pt0 = RepAppl(d0);
      /* check functor */
      if (*pt0 != (CELL)FunctorLongInt) {
	FAIL();
      }
      if (PREG->y_u.xi.i[1] != (CELL)pt0[1]) FAIL();
      ENDP(pt0);
      PREG = NEXTOP(PREG, xi);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, glongint_unk, glongint_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH(xi);
      BEGD(d1);
      d1 = AbsAppl(PREG->y_u.xi.i);
      PREG = NEXTOP(PREG, xi);
      YapBind(pt0, d1);
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(get_bigint, xN);
#ifdef USE_GMP
      BEGD(d0);
      d0 = XREG(PREG->y_u.xN.x);
      deref_head(d0, gbigint_unk);

    gbigint_nonvar:
      if (!IsApplTerm(d0))
	FAIL();
      /* we have met a preexisting bigint */
      START_PREFETCH(xN);
      BEGP(pt0);
      pt0 = RepAppl(d0);
      /* check functor */
      if (*pt0 != (CELL)FunctorBigInt)
	{
	  FAIL();
	}
      if (Yap_gmp_tcmp_big_big(d0,PREG->y_u.xN.b))
	FAIL();
      PREG = NEXTOP(PREG, xN);
      ENDP(pt0);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, gbigint_unk, gbigint_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH(xN);
      BEGD(d1);
      d1 = PREG->y_u.xN.b;
      PREG = NEXTOP(PREG, xN);
      YapBind(pt0, d1);
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
#else
      FAIL();
#endif
      ENDOp();


      Op(get_dbterm, xD);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xD.x);
      deref_head(d0, gdbterm_unk);

    gdbterm_nonvar:
      BEGD(d1);
      /* we have met a preexisting dbterm */
      d1 = PREG->y_u.xD.D;
      PREG = NEXTOP(PREG, xD);
      UnifyBound(d0,d1);
      ENDD(d1);

      BEGP(pt0);
      deref_body(d0, pt0, gdbterm_unk, gdbterm_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH(xD);
      BEGD(d1);
      d1 = PREG->y_u.xD.D;
      PREG = NEXTOP(PREG, xD);
      YapBind(pt0, d1);
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      /************************************************************************\
       *    Optimised Get List Instructions                                   *
\************************************************************************/
      OpRW(glist_valx, xx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      deref_head(d0, glist_valx_write);
    glist_valx_read:
      BEGP(pt0);
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      /* enter read mode */
      START_PREFETCH(xx);
      pt0 = RepPair(d0);
      SREG = pt0 + 1;
      /* start unification with first argument */
      d0 = *pt0;
      deref_head(d0, glist_valx_unk);

      /* first argument is in d0 */
    glist_valx_nonvar:
      /* first argument is bound */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      deref_head(d1, glist_valx_nonvar_unk);

    glist_valx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, xx);
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      deref_body(d1, pt1, glist_valx_nonvar_unk, glist_valx_nonvar_nonvar);
      /* head bound, argument unbound */
      PREG = NEXTOP(PREG, xx);
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);


      ENDD(d1);

      /* head may be unbound */
      derefa_body(d0, pt0, glist_valx_unk, glist_valx_nonvar);
      /* head is unbound, pt0 has the value */
      d0 = XREG(PREG->y_u.xx.xr);
      deref_head(d0, glist_valx_var_unk);

    glist_valx_var_nonvar:
      /* head is unbound, second arg bound */
      PREG = NEXTOP(PREG, xx);
      Bind_Global(pt0, d0);
      GONext();

      BEGP(pt1);
      deref_body(d0, pt1, glist_valx_var_unk, glist_valx_var_nonvar);
      /* head and second argument are unbound */
      PREG = NEXTOP(PREG, xx);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, glist_valx_write, glist_valx_read);
      CACHE_S();
      /* enter write mode */
      S_SREG = HR;
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      d0 = AbsPair(S_SREG);
      S_SREG[0] = d1;
      ENDD(d1);
      ALWAYS_START_PREFETCH_W(xx);
      PREG = NEXTOP(PREG, xx);
      HR = S_SREG + 2;
      WRITEBACK_S(S_SREG+1);
      YapBind(pt0, d0);
      ALWAYS_GONextW();
      ALWAYS_END_PREFETCH_W();
      ENDCACHE_S();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpRW(glist_valy, yx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yx.x);
      deref_head(d0, glist_valy_write);
    glist_valy_read:
      BEGP(pt0);
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      START_PREFETCH(yx);
      /* enter read mode */
      pt0 = RepPair(d0);
      SREG = pt0 + 1;
      /* start unification with first argument */
      d0 = *pt0;
      deref_head(d0, glist_valy_unk);

    glist_valy_nonvar:
      /* first argument is bound */
      BEGD(d1);
      BEGP(pt1);
      pt1 = YREG + PREG->y_u.yx.y;
      d1 = *pt1;
      PREG = NEXTOP(PREG, yx);
      deref_head(d1, glist_valy_nonvar_unk);

    glist_valy_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      SREG = pt0 + 1;
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, glist_valy_nonvar_unk, glist_valy_nonvar_nonvar);
      /* first argument bound, second unbound */
      YapBind(pt1, d0);
      GONext();


      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, glist_valy_unk, glist_valy_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.yx.y;
      d1 = *pt1;
      deref_head(d1, glist_valy_var_unk);
    glist_valy_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, yx);
      Bind_Global(pt0, d1);
      GONext();

      derefa_body(d1, pt1, glist_valy_var_unk, glist_valy_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, yx);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDD(d1);

      END_PREFETCH();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d0, pt0, glist_valy_write, glist_valy_read);
      /* enter write mode */
      START_PREFETCH_W(yx);
      BEGP(pt1);
      pt1 = HR;
      d0 = AbsPair(pt1);
      YapBind(pt0, d0);
      BEGD(d0);
      /* include XREG on it */
      d0 = YREG[PREG->y_u.yx.y];
      pt1[0] = d0;
      ENDD(d0);
      HR = pt1 + 2;
      SREG = pt1 + 1;
      ENDP(pt1);
      PREG = NEXTOP(PREG, yx);
      GONextW();
      END_PREFETCH_W();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      Op(gl_void_varx, xx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      deref_head(d0, glist_void_varx_write);
    glist_void_varx_read:
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      ALWAYS_START_PREFETCH(xx);
      /* enter read mode */
      BEGP(pt0);
      pt0 = RepPair(d0);
      d0 = pt0[1];
      XREG(PREG->y_u.xx.xr) = d0;
      PREG = NEXTOP(PREG, xx);
      ALWAYS_GONext();
      ENDP(pt0);
      ALWAYS_END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, glist_void_varx_write, glist_void_varx_read);
      /* enter write mode */
      BEGP(pt1);
      pt1 = HR;
      /* include XREG on it */
      XREG(PREG->y_u.xx.xr) =
	Unsigned(pt1 + 1);
      RESET_VARIABLE(pt1);
      RESET_VARIABLE(pt1+1);
      HR = pt1 + 2;
      BEGD(d0);
      d0 = AbsPair(pt1);
      YapBind(pt0, d0);
      PREG = NEXTOP(PREG, xx);
      ENDD(d0);
      ENDP(pt1);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(gl_void_vary, yx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yx.x);
      deref_head(d0, glist_void_vary_write);
    glist_void_vary_read:
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      /* enter read mode */
      BEGP(pt0);
      pt0 = RepPair(d0);
      d0 = pt0[1];
      ENDP(pt0);
      INITIALIZE_PERMVAR(YREG+PREG->y_u.yx.y,d0);
      PREG = NEXTOP(PREG, yx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, glist_void_vary_write, glist_void_vary_read);
      /* enter write mode */
      BEGP(pt1);
      pt1 = HR;
      /* include XREG on it */
      INITIALIZE_PERMVAR(YREG+PREG->y_u.yx.y,Unsigned(pt1 + 1));
      PREG = NEXTOP(PREG, yx);
      RESET_VARIABLE(pt1);
      RESET_VARIABLE(pt1+1);
      d0 = AbsPair(pt1);
      HR = pt1 + 2;
      YapBind(pt0, d0);
      GONext();
      ENDP(pt1);
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(gl_void_valx, xx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      deref_head(d0, glist_void_valx_write);
    glist_void_valx_read:
      BEGP(pt0);
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      /* enter read mode */
      pt0 = RepPair(d0)+1;
      /* start unification with first argument */
      d0 = *pt0;
      deref_head(d0, glist_void_valx_unk);

    glist_void_valx_nonvar:
      /* first argument is bound */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      deref_head(d1, glist_void_valx_nonvar_unk);

    glist_void_valx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, xx);
      UnifyBound(d0, d1);

      /* deref second argument */
      BEGP(pt1);
      deref_body(d1, pt1, glist_void_valx_nonvar_unk, glist_void_valx_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, xx);
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);
      ENDD(d1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, glist_void_valx_unk, glist_void_valx_nonvar);
      /* first argument is unbound */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      deref_head(d1, glist_void_valx_var_unk);

    glist_void_valx_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, xx);
      Bind_Global(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, glist_void_valx_var_unk, glist_void_valx_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, xx);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDD(d1);
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d0, pt0, glist_void_valx_write, glist_void_valx_read);
      /* enter write mode */
      BEGP(pt1);
      pt1 = HR;
      d0 = AbsPair(pt1);
      YapBind(pt0, d0);
      pt1 = HR;
      BEGD(d0);
      /* include XREG on it */
      d0 = XREG(PREG->y_u.xx.xr);
      RESET_VARIABLE(pt1);
      pt1[1] = d0;
      HR = pt1 + 2;
      ENDD(d0);
      ENDP(pt1);
      PREG = NEXTOP(PREG, xx);
      GONext();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(gl_void_valy, yx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yx.x);
      deref_head(d0, glist_void_valy_write);
    glist_void_valy_read:
      BEGP(pt0);
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      /* enter read mode */
      pt0 = RepPair(d0)+1;
      /* start unification with first argument */
      d0 = *pt0;
      deref_head(d0, glist_void_valy_unk);

    glist_void_valy_nonvar:
      /* first argument is bound */
      BEGD(d1);
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.yx.y;
      d1 = *pt1;
      deref_head(d1, glist_void_valy_nonvar_unk);

    glist_void_valy_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, yx);
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, glist_void_valy_nonvar_unk, glist_void_valy_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, yx);
      YapBind(pt1, d0);
      GONext();

      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, glist_void_valy_unk, glist_void_valy_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.yx.y;
      d1 = *pt1;
      deref_head(d1, glist_void_valy_var_unk);

    glist_void_valy_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, yx);
      Bind_Global(pt0, d1);
      GONext();

      deref_body(d1, pt1, glist_void_valy_var_unk, glist_void_valy_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, yx);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDD(d1);
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d0, pt0, glist_void_valy_write, glist_void_valy_read);
      /* enter write mode */
      CACHE_S();
      S_SREG = HR;
      d0 = AbsPair(S_SREG);
      YapBind(pt0, d0);
      S_SREG = HR;
      /* include XREG on it */
      BEGD(d1);
      d1 = YREG[PREG->y_u.yx.y];
      RESET_VARIABLE(S_SREG);
      S_SREG[1] = d1;
      ENDD(d1);
      PREG = NEXTOP(PREG, yx);
      HR = S_SREG + 2;
      ENDCACHE_S();
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();



      /************************************************************************\
       *      Unify instructions                                              *
\************************************************************************/

      Op(unify_x_var, ox);
      CACHE_S();
      READ_IN_S();
      BEGD(d0);
      d0 = *S_SREG;
#ifdef YAPOR_SBA
      if (d0 == 0)
	d0 = (CELL)S_SREG;
#endif
      WRITEBACK_S(S_SREG+1);
      ALWAYS_START_PREFETCH(ox);
      XREG(PREG->y_u.ox.x) = d0;
      PREG = NEXTOP(PREG, ox);
      ALWAYS_GONext();
      ALWAYS_END_PREFETCH();
      ENDD(d0);
      ENDCACHE_S();
      ENDOp();

      OpW(unify_x_var_write, ox);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->y_u.ox.x);
      PREG = NEXTOP(PREG, ox);
      RESET_VARIABLE(S_SREG);
      *pt0 = (CELL) S_SREG;
      WRITEBACK_S(S_SREG+1);
      ENDP(pt0);
      ENDCACHE_S();
      GONextW();
      ENDOpW();

      BOp(unify_l_x_var, ox);
      ALWAYS_START_PREFETCH(ox);
      BEGP(pt0);
      BEGD(d0);
      d0 = SREG[0];
      pt0 = &XREG(PREG->y_u.ox.x);
      PREG = NEXTOP(PREG, ox);
#ifdef YAPOR_SBA
      if (d0 == 0)
	d0 = (CELL)SREG;
#endif
      *pt0 = d0;
      ALWAYS_GONext();
      ENDD(d0);
      ENDP(pt0);
      ALWAYS_END_PREFETCH();
      ENDBOp();

      BOp(unify_l_x_var_write, ox);
      ALWAYS_START_PREFETCH(ox);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->y_u.ox.x);
      PREG = NEXTOP(PREG, ox);
      RESET_VARIABLE(S_SREG);
      *pt0 = (CELL)S_SREG;
      ENDP(pt0);
      ENDCACHE_S();
      ALWAYS_GONext();
      ENDBOp();
      ALWAYS_END_PREFETCH();

      BOp(unify_x_var2, oxx);
      CACHE_S();
      ALWAYS_START_PREFETCH(oxx);
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->y_u.oxx.xr);
      BEGD(d0);
      d0 = S_SREG[0];
      BEGD(d1);
      d1 = S_SREG[1];
#ifdef YAPOR_SBA
      if (d0 == 0)
	d0 = (CELL)S_SREG;
      if (d1 == 0)
	d1 = (CELL)(S_SREG+1);
#endif
      WRITEBACK_S(S_SREG+2);
      XREG(PREG->y_u.oxx.xl) = d0;
      PREG = NEXTOP(PREG, oxx);
      *pt0 = d1;
      ENDD(d0);
      ENDD(d1);
      ENDP(pt0);
      ALWAYS_GONext();
      ENDBOp();
      ALWAYS_END_PREFETCH();
      ENDCACHE_S();

      OpW(unify_x_var2_write, oxx);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->y_u.oxx.xr);
      RESET_VARIABLE(S_SREG);
      XREG(PREG->y_u.oxx.xl) = (CELL) S_SREG;
      S_SREG++;
      PREG = NEXTOP(PREG, oxx);
      RESET_VARIABLE(S_SREG);
      *pt0 = (CELL) S_SREG;
      ENDP(pt0);
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      GONextW();
      ENDOpW();

      BOp(unify_l_x_var2, oxx);
      ALWAYS_START_PREFETCH(oxx);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->y_u.oxx.xr);
      BEGD(d0);
      d0 = S_SREG[0];
      BEGD(d1);
      d1 = S_SREG[1];
#ifdef YAPOR_SBA
      if (d0 == 0)
	XREG(PREG->y_u.oxx.xl) = (CELL)S_SREG;
      else
#endif
	XREG(PREG->y_u.oxx.xl) = d0;
      PREG = NEXTOP(PREG, oxx);
#ifdef YAPOR_SBA
      if (d1 == 0)
	*pt0 = (CELL)(S_SREG+1);
      else
#endif
	*pt0 = d1;
      ENDD(d0);
      ENDD(d1);
      ENDP(pt0);
      ENDCACHE_S();
      ALWAYS_GONext();
      ENDBOp();
      ALWAYS_END_PREFETCH();

      Op(unify_l_x_var2_write, oxx);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->y_u.oxx.xr);
      XREG(PREG->y_u.oxx.xl) = (CELL) S_SREG;
      RESET_VARIABLE(S_SREG);
      S_SREG++;
      *pt0 = (CELL) S_SREG;
      PREG = NEXTOP(PREG, oxx);
      RESET_VARIABLE(S_SREG);
      ENDP(pt0);
      ENDCACHE_S();
      GONext();
      ENDOp();

      Op(unify_y_var, oy);
      BEGD(d0);
      d0 = *SREG++;
#ifdef YAPOR_SBA
      if (d0 == 0) {
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,(CELL)(SREG-1));
      } else
#else
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,d0);
#endif /* YAPOR_SBA */
      PREG = NEXTOP(PREG, oy);
      GONext();
      ENDD(d0);
      ENDOp();

      OpW(unify_y_var_write, oy);
      CACHE_S();
      READ_IN_S();
      INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,(CELL) S_SREG);
      PREG = NEXTOP(PREG, oy);
      RESET_VARIABLE(S_SREG);
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      GONextW();
      ENDOpW();

      Op(unify_l_y_var, oy);
      BEGD(d0);
      d0 = SREG[0];
#ifdef YAPOR_SBA
      if (d0 == 0) {
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,(CELL)SREG);
      } else
#else
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,d0);
#endif /* YAPOR_SBA */
      PREG = NEXTOP(PREG, oy);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(unify_l_y_var_write, oy);
      CACHE_S();
      READ_IN_S();
      INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,(CELL) S_SREG);
      PREG = NEXTOP(PREG, oy);
      RESET_VARIABLE(S_SREG);
      ENDCACHE_S();
      GONext();
      ENDOp();

      /* We assume the value in X is pointing to an object in the
       * global stack */
      Op(unify_x_val, ox);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvalx_unk);

    uvalx_nonvar:
      /* first argument is bound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, uvalx_nonvar_unk);

    uvalx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      UnifyBound(d0, d1);

      /* deref second argument */
      /* pt0 is in the structure and pt1 the register */
      BEGP(pt1);
      deref_body(d1, pt1, uvalx_nonvar_unk, uvalx_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, uvalx_unk, uvalx_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, uvalx_var_unk);

    uvalx_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      Bind_Global(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, uvalx_var_unk, uvalx_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      OpW(unify_x_val_write, ox);
      /* we are in write mode */
      *SREG++ = XREG(PREG->y_u.ox.x);
      PREG = NEXTOP(PREG, ox);
      GONextW();
      ENDOpW();

      /* We assume the value in X is pointing to an object in the
       * global stack */
      Op(unify_l_x_val, ox);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvalx_unk);

    ulvalx_nonvar:
      /* first argument is bound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, ulvalx_nonvar_unk);

    ulvalx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, ox);
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      deref_body(d1, pt1, ulvalx_nonvar_unk, ulvalx_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, ox);
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvalx_unk, ulvalx_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, ulvalx_var_unk);

    ulvalx_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, ox);
      Bind_Global(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, ulvalx_var_unk, ulvalx_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, ox);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(unify_l_x_val_write, ox);
      /* we are in write mode */
      SREG[0] = XREG(PREG->y_u.ox.x);
      PREG = NEXTOP(PREG, ox);
      GONext();
      ENDOp();

      /* We assume the value in X is pointing to an object in the
       * global stack */
      Op(unify_y_val, oy);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvaly_unk);

    uvaly_nonvar:
      /* first argument is bound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, uvaly_nonvar_unk);

    uvaly_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, uvaly_nonvar_unk, uvaly_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, uvaly_unk, uvaly_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, uvaly_var_unk);

    uvaly_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      Bind_Global(pt0, d1);
      GONext();

      derefa_body(d1, pt1, uvaly_var_unk, uvaly_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      OpW(unify_y_val_write, oy);
      /* we are in write mode */
      BEGD(d0);
      d0 = YREG[PREG->y_u.oy.y];
#ifdef YAPOR_SBA
      if (d0 == 0) /* free variable */
	*SREG++ = (CELL)(YREG+PREG->y_u.oy.y);
      else
#endif
	*SREG++ = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, oy);
      GONextW();
      ENDOpW();

      /* We assume the value in X is pointing to an object in the
       * global stack */
      Op(unify_l_y_val, oy);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvaly_unk);

    ulvaly_nonvar:
      /* first argument is bound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_nonvar_unk);

    ulvaly_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, oy);
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, ulvaly_nonvar_unk, ulvaly_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, oy);
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvaly_unk, ulvaly_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_var_unk);

    ulvaly_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, oy);
      Bind_Global(pt0, d1);
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      derefa_body(d1, pt1, ulvaly_var_unk, ulvaly_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, oy);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(unify_l_y_val_write, oy);
      /* we are in write mode */
      BEGD(d0);
      d0 = YREG[PREG->y_u.oy.y];
#ifdef YAPOR_SBA
      if (d0 == 0) /* new variable */
	SREG[0] = (CELL)(YREG+PREG->y_u.oy.y);
      else
#endif
	SREG[0] = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, oy);
      GONext();
      ENDOp();

      /* In the next instructions, we do not know anything about
       * what is in X */
      Op(unify_x_loc, ox);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvalx_loc_unk);

    uvalx_loc_nonvar:
      /* first argument is bound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, uvalx_loc_nonvar_unk);

    uvalx_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      deref_body(d1, pt1, uvalx_loc_nonvar_unk, uvalx_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);


      /* first argument may be unbound */
      derefa_body(d0, pt0, uvalx_loc_unk, uvalx_loc_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, uvalx_loc_var_unk);

    uvalx_loc_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      Bind_Global(pt0, d1);
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      BEGP(pt1);
      deref_body(d1, pt1, uvalx_loc_var_unk, uvalx_loc_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      UnifyCells(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      OpW(unify_x_loc_write, ox);
      /* we are in write mode */
      BEGD(d0);
      d0 = XREG(PREG->y_u.ox.x);
      deref_head(d0, unify_x_loc_unk);
    unify_x_loc_nonvar:
      *SREG++ = d0;
      PREG = NEXTOP(PREG, ox);
      GONextW();

      BEGP(pt0);
      deref_body(d0, pt0, unify_x_loc_unk, unify_x_loc_nonvar);
      /* move ahead in the instructions */
      PREG = NEXTOP(PREG, ox);
      /* d0 is a variable, check whether we need to globalise it */
      if (pt0 < HR) {
	/* variable is global */
	*SREG++ = Unsigned(pt0);
	GONextW();
      }
      else {
	/* bind our variable to the structure */
	CACHE_S();
	READ_IN_S();
	Bind_Local(pt0, Unsigned(S_SREG));
	RESET_VARIABLE(S_SREG);
	WRITEBACK_S(S_SREG+1);
	ENDCACHE_S();
	GONextW();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOpW();

      /* In the next instructions, we do not know anything about
       * what is in X */
      Op(unify_l_x_loc, ox);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvalx_loc_unk);

    ulvalx_loc_nonvar:
      /* first argument is bound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, ulvalx_loc_nonvar_unk);

    ulvalx_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, ox);
      UnifyBound(d0, d1);

      /* deref second argument */
      deref_body(d1, pt0, ulvalx_loc_nonvar_unk, ulvalx_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, ox);
      YapBind(pt0, d0);
      GONext();

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvalx_loc_unk, ulvalx_loc_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, ulvalx_loc_var_unk);

    ulvalx_loc_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, ox);
      Bind_Global(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, ulvalx_loc_var_unk, ulvalx_loc_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, ox);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(unify_l_x_loc_write, ox);
      /* we are in write mode */
      BEGD(d0);
      d0 = XREG(PREG->y_u.ox.x);
      deref_head(d0, ulnify_x_loc_unk);
    ulnify_x_loc_nonvar:
      SREG[0] = d0;
      PREG = NEXTOP(PREG, ox);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, ulnify_x_loc_unk, ulnify_x_loc_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      PREG = NEXTOP(PREG, ox);
      if (pt0 < HR) {
	/* variable is global */
	SREG[0] = Unsigned(pt0);
	GONext();
      }
      else {
	/* create a new Heap variable and bind our variable to it */
	Bind_Local(pt0, Unsigned(SREG));
	RESET_VARIABLE(SREG);
	GONext();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOpW();

      Op(unify_y_loc, oy);
      /* we are in read mode */
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvaly_loc_unk);

    uvaly_loc_nonvar:
      /* structure is bound */
      BEGP(pt1);
      pt1 =  YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, uvaly_loc_nonvar_unk);

    uvaly_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, uvaly_loc_nonvar_unk, uvaly_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, uvaly_loc_unk, uvaly_loc_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, uvaly_loc_var_unk);

    uvaly_loc_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      Bind_Global(pt0, d1);
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      derefa_body(d1, pt1, uvaly_loc_var_unk, uvaly_loc_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      UnifyCells(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      OpW(unify_y_loc_write, oy);
      /* we are in write mode */
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG+PREG->y_u.oy.y;
      d0 = *pt0;
      deref_head(d0, unify_y_loc_unk);
    unify_y_loc_nonvar:
      *SREG++ = d0;
      PREG = NEXTOP(PREG, oy);
      GONextW();

      derefa_body(d0, pt0, unify_y_loc_unk, unify_y_loc_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      PREG = NEXTOP(PREG, oy);
      if (pt0 < HR) {
	/* variable is global */
	*SREG++ = Unsigned(pt0);
	GONextW();
      }
      else {
	/* create a new Heap variable and bind our variable to it */
	CACHE_S();
	READ_IN_S();
	Bind_Local(pt0, Unsigned(S_SREG));
	RESET_VARIABLE(S_SREG);
	WRITEBACK_S(S_SREG+1);
	ENDCACHE_S();
	GONextW();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOpW();

      Op(unify_l_y_loc, oy);
      /* else we are in read mode */
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvaly_loc_unk);

    ulvaly_loc_nonvar:
      /* structure is bound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_loc_nonvar_unk);

    ulvaly_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, oy);
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, ulvaly_loc_nonvar_unk, ulvaly_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, oy);
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvaly_loc_unk, ulvaly_loc_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_loc_var_unk);

    ulvaly_loc_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, oy);
      Bind_Global(pt0, d1);
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      derefa_body(d1, pt1, ulvaly_loc_var_unk, ulvaly_loc_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, oy);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(unify_l_y_loc_write, oy);
      /* we are in write mode */
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG+PREG->y_u.oy.y;
      d0 = *pt0;
      deref_head(d0, ulunify_y_loc_unk);
    ulunify_y_loc_nonvar:
      SREG[0] = d0;
      PREG = NEXTOP(PREG, oy);
      GONext();

      derefa_body(d0, pt0, ulunify_y_loc_unk, ulunify_y_loc_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      PREG = NEXTOP(PREG, oy);
      if (pt0 < HR) {
	/* variable is global */
	SREG[0] = Unsigned(pt0);
	GONext();
      }
      else {
	/* create a new Heap variable and bind our variable to it */
	CACHE_S();
	READ_IN_S();
	Bind_Local(pt0, Unsigned(S_SREG));
	RESET_VARIABLE(S_SREG);
	ENDCACHE_S();
	GONext();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(unify_void, o);
      START_PREFETCH(o);
      PREG = NEXTOP(PREG, o);
      SREG++;
      GONext();
      END_PREFETCH();
      ENDOp();

      OpW(unify_void_write, o);
      CACHE_S();
      READ_IN_S();
      PREG = NEXTOP(PREG, o);
      RESET_VARIABLE(S_SREG);
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      GONextW();
      ENDOpW();

      Op(unify_l_void, o);
      PREG = NEXTOP(PREG, o);
      GONext();
      ENDOp();

      Op(unify_l_void_write, o);
      PREG = NEXTOP(PREG, o);
      RESET_VARIABLE(SREG);
      GONext();
      ENDOp();

      Op(unify_n_voids, os);
      SREG += PREG->y_u.os.s;
      PREG = NEXTOP(PREG, os);
      GONext();
      ENDOp();

      OpW(unify_n_voids_write, os);
      BEGD(d0);
      CACHE_S();
      d0 = PREG->y_u.os.s;
      READ_IN_S();
      PREG = NEXTOP(PREG, os);
      for (; d0 > 0; d0--) {
	RESET_VARIABLE(S_SREG);
	S_SREG++;
      }
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      ENDD(d0);
      GONextW();
      ENDOpW();

      Op(unify_l_n_voids, os);
      PREG = NEXTOP(PREG, os);
      GONext();
      ENDOp();

      Op(unify_l_n_voids_write, os);
      BEGD(d0);
      d0 = PREG->y_u.os.s;
      PREG = NEXTOP(PREG, os);
      CACHE_S();
      READ_IN_S();
      for (; d0 > 0; d0--) {
	RESET_VARIABLE(S_SREG);
	S_SREG++;
      }
      ENDCACHE_S();
      ENDD(d0);
      GONext();
      ENDOp();

      Op(unify_atom, oc);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, uatom_unk);
    uatom_nonvar:
      if (d0 != PREG->y_u.oc.c) {
	FAIL();
      }
      PREG = NEXTOP(PREG, oc);
      GONext();

      derefa_body(d0, pt0, uatom_unk, uatom_nonvar);
      d0 = PREG->y_u.oc.c;
      PREG = NEXTOP(PREG, oc);
      Bind_Global(pt0, d0);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      OpW(unify_atom_write, oc);
      * SREG++ = PREG->y_u.oc.c;
      PREG = NEXTOP(PREG, oc);
      GONextW();
      ENDOpW();

      Op(unify_l_atom, oc);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *SREG;
      deref_head(d0, ulatom_unk);
    ulatom_nonvar:
      if (d0 != PREG->y_u.oc.c) {
	FAIL();
      }
      PREG = NEXTOP(PREG, oc);
      GONext();

      derefa_body(d0, pt0, ulatom_unk, ulatom_nonvar);
      d0 = PREG->y_u.oc.c;
      PREG = NEXTOP(PREG, oc);
      Bind_Global(pt0, d0);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(unify_l_atom_write, oc);
      SREG[0] = PREG->y_u.oc.c;
      PREG = NEXTOP(PREG, oc);
      GONext();
      ENDOp();

      Op(unify_n_atoms, osc);
      {
	register Int i = PREG->y_u.osc.s;             /* not enough registers */

	BEGD(d1);
	d1 = PREG->y_u.osc.c;
	for (; i > 0; i--) {
	  BEGD(d0);
	  BEGP(pt0);
	  pt0 = SREG++;
	  d0 = *pt0;
	  deref_head(d0, uatom_n_var);
	uatom_n_nonvar:
	  if (d0 != d1) {
	    FAIL();
	  }
	  continue;

	  derefa_body(d0, pt0, uatom_n_var, uatom_n_nonvar);
	  Bind_Global(pt0, d1);
	  continue;
	  ENDP(pt0);
	  ENDD(d0);
	}
	ENDD(d1);
      }
      PREG = NEXTOP(PREG, osc);
      GONext();
      ENDOp();

      OpW(unify_n_atoms_write, osc);
      BEGD(d0);
      BEGD(d1);
      d0 = PREG->y_u.osc.s;
      d1 = PREG->y_u.osc.c;
      /* write N atoms */
      CACHE_S();
      READ_IN_S();
      PREG = NEXTOP(PREG, osc);
      for (; d0 > 0; d0--)
	*S_SREG++ = d1;
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      ENDD(d1);
      ENDD(d0);
      GONextW();
      ENDOpW();

      Op(unify_float, od);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, ufloat_unk);
    ufloat_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      /* look inside term */
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorDouble) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = PREG->y_u.od.d;
      PREG = NEXTOP(PREG, od);
      if (
	  pt1[1] != pt0[1]
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
	  || pt1[2] != pt0[2]
#endif
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, pt0, ufloat_unk, ufloat_nonvar);
      BEGD(d1);
      d1 = AbsAppl(PREG->y_u.od.d);
      PREG = NEXTOP(PREG, od);
      Bind_Global(pt0, d1);
      GONext();
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      OpW(unify_float_write, od);
      * SREG++ = AbsAppl(PREG->y_u.od.d);
      PREG = NEXTOP(PREG, od);
      GONextW();
      ENDOpW();

      Op(unify_l_float, od);
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, ulfloat_unk);
    ulfloat_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorDouble) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = PREG->y_u.od.d;
      PREG = NEXTOP(PREG, od);
      if (
	  pt1[1] != pt0[1]
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
	  || pt1[2] != pt0[2]
#endif
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, S_SREG, ulfloat_unk, ulfloat_nonvar);
      BEGD(d1);
      d1 = AbsAppl(PREG->y_u.od.d);
      PREG = NEXTOP(PREG, od);
      Bind_Global(S_SREG, d1);
      GONext();
      ENDD(d1);
      ENDCACHE_S();
      ENDD(d0);
      ENDOp();

      Op(unify_l_float_write, od);
      SREG[0] = AbsAppl(PREG->y_u.od.d);
      PREG = NEXTOP(PREG, od);
      GONext();
      ENDOp();

      Op(unify_string, ou);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, ustring_unk);
    ustring_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      /* look inside term */
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorString) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = RepAppl(PREG->y_u.ou.ut);
      PREG = NEXTOP(PREG, ou);
      if (
	  pt1[1] != pt0[1]
	  || strcmp( (const char *)(pt1 + 2), (const char *)(pt0+2) )
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, pt0, ustring_unk, ustring_nonvar);
      BEGD(d1);
      d1 = PREG->y_u.ou.ut;
      PREG = NEXTOP(PREG, ou);
      Bind_Global(pt0, d1);
      GONext();
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(unify_l_string, ou);
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, ulstring_unk);
    ulstring_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorString) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = RepAppl(PREG->y_u.ou.ut);
      PREG = NEXTOP(PREG, ou);
      if (
	  pt1[1] != pt0[1]
	  || strcmp( (const char *)(pt1 + 2), (const char *)(pt0+2) )
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, S_SREG, ulstring_unk, ulstring_nonvar);
      BEGD(d1);
      d1 = PREG->y_u.ou.ut;
      PREG = NEXTOP(PREG, ou);
      Bind_Global(S_SREG, d1);
      GONext();
      ENDD(d1);
      ENDCACHE_S();
      ENDD(d0);
      ENDOp();

      Op(unify_longint, oi);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, ulongint_unk);
    ulongint_nonvar:
      /* look inside term */
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorLongInt) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = PREG->y_u.oi.i;
      PREG = NEXTOP(PREG, oi);
      if (pt1[1] != pt0[1]) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, pt0, ulongint_unk, ulongint_nonvar);
      BEGD(d1);
      d1 = AbsAppl(PREG->y_u.oi.i);
      PREG = NEXTOP(PREG, oi);
      Bind_Global(pt0, d1);
      GONext();
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      OpW(unify_longint_write, oi);
      * SREG++ = AbsAppl(PREG->y_u.oi.i);
      PREG = NEXTOP(PREG, oi);
      GONextW();
      ENDOpW();

      Op(unify_l_longint, oi);
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, ullongint_unk);
    ullongint_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorLongInt) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = PREG->y_u.oi.i;
      PREG = NEXTOP(PREG, oi);
      if (pt1[1] != pt0[1]) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, S_SREG, ullongint_unk, ullongint_nonvar);
      BEGD(d1);
      d1 = AbsAppl(PREG->y_u.oi.i);
      PREG = NEXTOP(PREG, oi);
      Bind_Global(S_SREG, d1);
      GONext();
      ENDD(d1);
      ENDCACHE_S();
      ENDD(d0);
      ENDOp();

      Op(unify_l_longint_write, oi);
      SREG[0] = AbsAppl(PREG->y_u.oi.i);
      PREG = NEXTOP(PREG, oi);
      GONext();
      ENDOp();

      Op(unify_bigint, oN);
#ifdef USE_GMP
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, ubigint_unk);
    ubigint_nonvar:
      /* look inside term */
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d1);
      d1 = *pt0;
      if (d1 != (CELL)FunctorBigInt)
	{
	  FAIL();
	}
      ENDD(d1);
      if (Yap_gmp_tcmp_big_big(d0,PREG->y_u.oN.b))
	FAIL();
      PREG = NEXTOP(PREG, oN);
      ENDP(pt0);
      GONext();

      derefa_body(d0, pt0, ubigint_unk, ubigint_nonvar);
      BEGD(d1);
      d1 = PREG->y_u.oN.b;
      PREG = NEXTOP(PREG, oN);
      Bind_Global(pt0, d1);
      GONext();
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
#else
      FAIL();
#endif
      ENDOp();

      Op(unify_l_bigint, oN);
#ifdef USE_GMP
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, ulbigint_unk);
    ulbigint_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorBigInt)
	{
	  FAIL();
	}
      ENDD(d0);
      if (Yap_gmp_tcmp_big_big(d0,PREG->y_u.oN.b))
	FAIL();
      PREG = NEXTOP(PREG, oN);
      ENDP(pt0);
      GONext();

      derefa_body(d0, S_SREG, ulbigint_unk, ulbigint_nonvar);
      BEGD(d1);
      d1 = PREG->y_u.oN.b;
      PREG = NEXTOP(PREG, oN);
      Bind_Global(S_SREG, d1);
      GONext();
      ENDD(d1);
      ENDCACHE_S();
      ENDD(d0);
#else
      FAIL();
#endif
      ENDOp();

      Op(unify_dbterm, oD);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, udbterm_unk);
    udbterm_nonvar:
      BEGD(d1);
      /* we have met a preexisting dbterm */
      d1 = PREG->y_u.oD.D;
      PREG = NEXTOP(PREG, oD);
      UnifyBound(d0,d1);
      ENDD(d1);

      derefa_body(d0, pt0, udbterm_unk, udbterm_nonvar);
      BEGD(d1);
      d1 = PREG->y_u.oD.D;
      PREG = NEXTOP(PREG, oD);
      Bind_Global(pt0, d1);
      GONext();
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(unify_l_dbterm, oD);
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, uldbterm_unk);
    uldbterm_nonvar:
      BEGD(d1);
      /* we have met a preexisting dbterm */
      d1 = PREG->y_u.oD.D;
      PREG = NEXTOP(PREG, oD);
      UnifyBound(d0,d1);
      ENDD(d1);

      derefa_body(d0, S_SREG, uldbterm_unk, uldbterm_nonvar);
      BEGD(d1);
      d1 = PREG->y_u.oD.D;
      PREG = NEXTOP(PREG, oD);
      Bind_Global(S_SREG, d1);
      GONext();
      ENDD(d1);
      ENDCACHE_S();
      ENDD(d0);
      ENDOp();

      OpRW(unify_list, o);
      *--SP = Unsigned(SREG + 1);
      *--SP = READ_MODE;
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulist_unk);
    ulist_nonvar:
      if (!IsPairTerm(d0)) {
	FAIL();
      }
      /* we continue in read mode */
      START_PREFETCH(o);
      SREG = RepPair(d0);
      PREG = NEXTOP(PREG, o);
      GONext();
      END_PREFETCH();

      derefa_body(d0, pt0, ulist_unk, ulist_nonvar);
      /* we enter write mode */
      START_PREFETCH_W(o);
      CACHE_S();
      READ_IN_S();
      S_SREG = HR;
      PREG = NEXTOP(PREG, o);
      HR = S_SREG + 2;
      d0 = AbsPair(S_SREG);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      Bind_Global(pt0, d0);
      GONextW();
      END_PREFETCH_W();

      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpW(unify_list_write, o);
      PREG = NEXTOP(PREG, o);
      BEGD(d0);
      d0 = AbsPair(HR);
      CACHE_S();
      READ_IN_S();
      SP -= 2;
      SP[0] = WRITE_MODE;
      SP[1] = Unsigned(S_SREG + 1);
      S_SREG[0] = d0;
      S_SREG = HR;
      HR = S_SREG + 2;
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      GONextW();
      ENDD(d0);
      ENDOpW();

      OpRW(unify_l_list, o);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ullist_unk);
    ullist_nonvar:
      START_PREFETCH(o);
      if (!IsPairTerm(d0)) {
	FAIL();
      }
      /* we continue in read mode */
      PREG = NEXTOP(PREG, o);
      SREG = RepPair(d0);
      GONext();
      END_PREFETCH();

      derefa_body(d0, pt0, ullist_unk, ullist_nonvar);
      /* we enter write mode */
      START_PREFETCH_W(o);
      PREG = NEXTOP(PREG, o);
      CACHE_S();
      READ_IN_S();
      S_SREG = HR;
      HR = S_SREG + 2;
      d0 = AbsPair(S_SREG);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      Bind_Global(pt0, d0);
      GONextW();
      END_PREFETCH_W();

      ENDP(pt0);
      ENDD(d0);
      ENDOpRW();

      OpW(unify_l_list_write, o);
      /* we continue in write mode */
      BEGD(d0);
      d0 = AbsPair(HR);
      PREG = NEXTOP(PREG, o);
      CACHE_S();
      READ_IN_S();
      S_SREG[0] = d0;
      S_SREG = HR;
      HR = S_SREG + 2;
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      GONextW();
      ENDD(d0);
      ENDOpW();

      OpRW(unify_struct, ofa);
      *--SP = Unsigned(SREG + 1);
      *--SP = READ_MODE;
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      START_PREFETCH(ofa);
      deref_head(d0, ustruct_unk);
    ustruct_nonvar:
      /* we are in read mode */
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      CACHE_S();
      READ_IN_S();
      /* we continue in read mode */
      S_SREG = RepAppl(d0);
      /* just check functor */
      d0 = (CELL) (PREG->y_u.ofa.f);
      if (*S_SREG != d0) {
	FAIL();
      }
      PREG = NEXTOP(PREG, ofa);
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      GONext();
      END_PREFETCH();

      derefa_body(d0, pt0, ustruct_unk, ustruct_nonvar);
      /* Enter Write mode */
      START_PREFETCH_W(ofa);
      /* set d1 to be the new structure we are going to create */
      BEGD(d1);
      d1 = AbsAppl(HR);
      /* we know the variable must be in the heap */
      Bind_Global(pt0, d1);
      /* now, set pt0 to point to the heap where we are going to
       * build our term */
      pt0 = HR;
      ENDD(d1);
      /* first, put the functor */
      d0 = (CELL) (PREG->y_u.ofa.f);
      *pt0++ = d0;
      HR = pt0 + PREG->y_u.ofa.a;
      PREG = NEXTOP(PREG, ofa);
      /* set SREG */
      SREG = pt0;
      /* update H */
      GONextW();
      END_PREFETCH_W();

      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpW(unify_struct_write, ofa);
      CACHE_S();
      READ_IN_S();
      *--SP = Unsigned(S_SREG + 1);
      *--SP = WRITE_MODE;
      /* we continue in write mode */
      BEGD(d0);
      d0 = AbsAppl(HR);
      S_SREG[0] = d0;
      S_SREG = HR;
      d0 = (CELL) (PREG->y_u.ofa.f);
      *S_SREG++ = d0;
      HR = S_SREG + PREG->y_u.ofa.a;
      PREG = NEXTOP(PREG, ofa);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      ENDD(d0);
      GONextW();
      ENDOpW();

      OpRW(unify_l_struc, ofa);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulstruct_unk);
    ulstruct_nonvar:
      /* we are in read mode */
      START_PREFETCH(ofa);
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      /* we continue in read mode */
      SREG = RepAppl(d0);
      /* just check functor */
      d0 = (CELL) (PREG->y_u.ofa.f);
      if (*SREG++ != d0) {
	FAIL();
      }
      PREG = NEXTOP(PREG, ofa);
      GONext();
      END_PREFETCH();

      derefa_body(d0, pt0, ulstruct_unk, ulstruct_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH_W(ofa);
      BEGD(d1);
      d1 = AbsAppl(HR);
      /* we know the variable must be in the heap */
      Bind_Global(pt0, d1);
      /* now, set pt0 to point to the heap where we are going to
       * build our term */
      pt0 = HR;
      ENDD(d1);
      /* first, put the functor */
      d0 = (CELL) (PREG->y_u.ofa.f);
      *pt0++ = d0;
      HR = pt0 + PREG->y_u.ofa.a;
      PREG = NEXTOP(PREG, ofa);
      /* set SREG */
      SREG = pt0;
      /* update H */
      GONextW();
      END_PREFETCH_W();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpW(unify_l_struc_write, ofa);
      BEGD(d0);
      d0 = AbsAppl(HR);
      CACHE_S();
      READ_IN_S();
      S_SREG[0] = d0;
      S_SREG = HR;
      d0 = (CELL) (PREG->y_u.ofa.f);
      *S_SREG++ = d0;
      HR = S_SREG + PREG->y_u.ofa.a;
      PREG = NEXTOP(PREG, ofa);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      ENDD(d0);
      GONextW();
      ENDOpW();


      /************************************************************************\
       * Put Instructions                                                      *
\************************************************************************/

      Op(put_x_var, xx);
      BEGP(pt0);
      pt0 = HR;
      XREG(PREG->y_u.xx.xl) = Unsigned(pt0);
      HR = pt0 + 1;
      XREG(PREG->y_u.xx.xr) = Unsigned(pt0);
      PREG = NEXTOP(PREG, xx);
      RESET_VARIABLE(pt0);
      ENDP(pt0);
      GONext();
      ENDOp();

      Op(put_y_var, yx);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yx.y;
      XREG(PREG->y_u.yx.x) = (CELL) pt0;
      PREG = NEXTOP(PREG, yx);
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
      /* We must initialize a shared variable to point to the SBA */
      if (Unsigned((Int)(pt0)-(Int)(H_FZ)) >
	  Unsigned((Int)(B_FZ)-(Int)(H_FZ))) {
	*pt0 =  (CELL)STACK_TO_SBA(pt0);
      } else
#endif /* YAPOR_SBA && FROZEN_STACKS */
	INITIALIZE_PERMVAR(pt0, (CELL)pt0);
      ENDP(pt0);
      GONext();
      ENDOp();

      Op(put_x_val, xx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      XREG(PREG->y_u.xx.xr) = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, xx);
      GONext();
      ENDOp();

      Op(put_xx_val, xxxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxxx.xl1);
      d1 = XREG(PREG->y_u.xxxx.xl2);
      XREG(PREG->y_u.xxxx.xr1) = d0;
      XREG(PREG->y_u.xxxx.xr2) = d1;
      ENDD(d1);
      ENDD(d0);
      PREG = NEXTOP(PREG, xxxx);
      GONext();
      ENDOp();

      Op(put_y_val, yx);
      BEGD(d0);
      d0 = YREG[PREG->y_u.yx.y];
#ifdef YAPOR_SBA
      if (d0 == 0) /* new variable */
	XREG(PREG->y_u.yx.x) = (CELL)(YREG+PREG->y_u.yx.y);
      else
#endif
	XREG(PREG->y_u.yx.x) = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, yx);
      GONext();
      ENDOp();

      Op(put_y_vals, yyxx);
      ALWAYS_START_PREFETCH(yyxx);
      BEGD(d0);
      d0 = YREG[PREG->y_u.yyxx.y1];
#ifdef YAPOR_SBA
      if (d0 == 0) /* new variable */
	XREG(PREG->y_u.yyxx.x1) = (CELL)(YREG+PREG->y_u.yyxx.y1);
      else
#endif
	XREG(PREG->y_u.yyxx.x1) = d0;
      ENDD(d0);
      /* allow for some prefetching */
      PREG = NEXTOP(PREG, yyxx);
      BEGD(d1);
      d1 = YREG[PREVOP(PREG,yyxx)->y_u.yyxx.y2];
#ifdef YAPOR_SBA
      if (d1 == 0) /* new variable */
	XREG(PREVOP(PREG->y_u.yyxx,yyxx).x2) = (CELL)(YREG+PREG->y_u.yyxx.y2);
      else
#endif
	XREG(PREVOP(PREG,yyxx)->y_u.yyxx.x2) = d1;
      ENDD(d1);
      ALWAYS_GONext();
      ALWAYS_END_PREFETCH();
      ENDOp();

      Op(put_unsafe, yx);
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG+PREG->y_u.yx.y;
      d0 = *pt0;
      deref_head(d0, punsafe_unk);
    punsafe_nonvar:
      XREG(PREG->y_u.yx.x) = d0;
      PREG = NEXTOP(PREG, yx);
      GONext();

      derefa_body(d0, pt0, punsafe_unk, punsafe_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      if (pt0 <= HR || pt0 >= YREG) {
	/* variable is safe */
	XREG(PREG->y_u.yx.x) = Unsigned(pt0);
	PREG = NEXTOP(PREG, yx);
	GONext();
      }
      else {
	/* create a new Heap variable and bind our variable to it */
	Bind_Local(pt0, Unsigned(HR));
	XREG(PREG->y_u.yx.x) = (CELL) HR;
	RESET_VARIABLE(HR);
	HR++;
	PREG = NEXTOP(PREG, yx);
	GONext();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(put_atom, xc);
      BEGD(d0);
      d0 = PREG->y_u.xc.c;
      XREG(PREG->y_u.xc.x) = d0;
      PREG = NEXTOP(PREG, xc);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_dbterm, xD);
      BEGD(d0);
      d0 = PREG->y_u.xD.D;
      XREG(PREG->y_u.xD.x) = d0;
      PREG = NEXTOP(PREG, xD);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_bigint, xN);
      BEGD(d0);
      d0 = PREG->y_u.xN.b;
      XREG(PREG->y_u.xN.x) = d0;
      PREG = NEXTOP(PREG, xN);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_float, xd);
      BEGD(d0);
      d0 = AbsAppl(PREG->y_u.xd.d);
      XREG(PREG->y_u.xd.x) = d0;
      PREG = NEXTOP(PREG, xd);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_longint, xi);
      BEGD(d0);
      d0 = AbsAppl(PREG->y_u.xi.i);
      XREG(PREG->y_u.xi.x) = d0;
      PREG = NEXTOP(PREG, xi);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_list, x);
      CACHE_S();
      READ_IN_S();
      S_SREG = HR;
      HR += 2;
      BEGD(d0);
      d0 = AbsPair(S_SREG);
      XREG(PREG->y_u.x.x) = d0;
      PREG = NEXTOP(PREG, x);
      ENDD(d0);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      GONext();
      ENDOp();

      Op(put_struct, xfa);
      BEGD(d0);
      d0 = AbsAppl(HR);
      XREG(PREG->y_u.xfa.x) = d0;
      d0 = (CELL) (PREG->y_u.xfa.f);
      *HR++ = d0;
      SREG = HR;
      HR += PREG->y_u.xfa.a;
      ENDD(d0);
      PREG = NEXTOP(PREG, xfa);
      GONext();
      ENDOp();

      /************************************************************************\
       *      Write Instructions                                              *
\************************************************************************/

      Op(write_x_var, x);
      XREG(PREG->y_u.x.x) = Unsigned(SREG);
      PREG = NEXTOP(PREG, x);
      RESET_VARIABLE(SREG);
      SREG++;
      GONext();
      ENDOp();

      Op(write_void, e);
      PREG = NEXTOP(PREG, e);
      RESET_VARIABLE(SREG);
      SREG++;
      GONext();
      ENDOp();

      Op(write_n_voids, s);
      BEGD(d0);
      d0 = PREG->y_u.s.s;
      PREG = NEXTOP(PREG, s);
      for (; d0 > 0; d0--) {
	RESET_VARIABLE(SREG);
	SREG++;
      }
      ENDD(d0);
      GONext();
      ENDOp();

      Op(write_y_var, y);
      INITIALIZE_PERMVAR(YREG+PREG->y_u.y.y,Unsigned(SREG));
      PREG = NEXTOP(PREG, y);
      RESET_VARIABLE(SREG);
      SREG++;
      GONext();
      ENDOp();

      Op(write_x_val, x);
      BEGD(d0);
      d0 = XREG(PREG->y_u.x.x);
      *SREG++ = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, x);
      GONext();
      ENDOp();

      Op(write_x_loc, x);
      BEGD(d0);
      d0 = XREG(PREG->y_u.x.x);
      PREG = NEXTOP(PREG, x);
      deref_head(d0, w_x_unk);
    w_x_bound:
      *SREG++ = d0;
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, w_x_unk, w_x_bound);
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
      if (pt0 > HR && pt0<(CELL *)B_FZ) {
#else
	if (pt0 > HR) {
#endif /* YAPOR_SBA && FROZEN_STACKS */
	  /* local variable: let us bind it to the list */
#ifdef FROZEN_STACKS  /* TRAIL */
	  Bind_Local(pt0, Unsigned(SREG));
#else
	  TRAIL_LOCAL(pt0, Unsigned(SREG));
	  *pt0 = Unsigned(SREG);
#endif /* FROZEN_STACKS */
	  RESET_VARIABLE(SREG);
	  SREG++;
	  GONext();
	}
	else {
	  *SREG++ = Unsigned(pt0);
	  GONext();
	}
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(write_y_val, y);
	BEGD(d0);
	d0 = YREG[PREG->y_u.y.y];
#ifdef YAPOR_SBA
	if (d0 == 0) /* new variable */
	  *SREG++ = (CELL)(YREG+PREG->y_u.y.y);
	else
#endif
	  *SREG++ = d0;
	ENDD(d0);
	PREG = NEXTOP(PREG, y);
	GONext();
	ENDOp();

	Op(write_y_loc, y);
	BEGD(d0);
	BEGP(pt0);
	pt0 = YREG+PREG->y_u.y.y;
	d0 = *pt0;
	deref_head(d0, w_y_unk);
      w_y_bound:
	PREG = NEXTOP(PREG, y);
	*SREG++ = d0;
	GONext();

	derefa_body(d0, pt0, w_y_unk, w_y_bound);
	if (pt0 > HR
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
	    && pt0<(CELL *)B_FZ
#endif /* YAPOR_SBA && FROZEN_STACKS */
	    ) {
	  PREG = NEXTOP(PREG, y);
	  /* local variable: let us bind it to the list */
#ifdef FROZEN_STACKS
	  Bind_Local(pt0, Unsigned(SREG));
#else
	  *pt0 = Unsigned(SREG);
	  TRAIL_LOCAL(pt0, Unsigned(SREG));
#endif /* FROZEN_STACKS */
	  RESET_VARIABLE(SREG);
	  SREG++;
	  GONext();
	} else {
	  PREG = NEXTOP(PREG, y);
	  *SREG++ = Unsigned(pt0);
	  GONext();
	}
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(write_atom, c);
	BEGD(d0);
	d0 = PREG->y_u.c.c;
	*SREG++ = d0;
	ENDD(d0);
	PREG = NEXTOP(PREG, c);
	GONext();
	ENDOp();

	Op(write_bigint, N);
	BEGD(d0);
	d0 = PREG->y_u.N.b;
	*SREG++ = d0;
	ENDD(d0);
	PREG = NEXTOP(PREG, N);
	GONext();
	ENDOp();

	Op(write_dbterm, D);
	BEGD(d0);
	d0 = PREG->y_u.D.D;
	*SREG++ = d0;
	ENDD(d0);
	PREG = NEXTOP(PREG, D);
	GONext();
	ENDOp();

	Op(write_float, d);
	BEGD(d0);
	d0 = AbsAppl(PREG->y_u.d.d);
	*SREG++ = d0;
	ENDD(d0);
	PREG = NEXTOP(PREG, d);
	GONext();
	ENDOp();

	Op(write_longint, i);
	BEGD(d0);
	d0 = AbsAppl(PREG->y_u.i.i);
	*SREG++ = d0;
	ENDD(d0);
	PREG = NEXTOP(PREG, i);
	GONext();
	ENDOp();

	Op(write_n_atoms, sc);
	BEGD(d0);
	BEGD(d1);
	d0 = PREG->y_u.sc.s;
	d1 = PREG->y_u.sc.c;
	for (; d0 > 0; d0--)
	  *SREG++ = d1;
	ENDD(d1);
	ENDD(d0);
	PREG = NEXTOP(PREG, sc);
	GONext();
	ENDOp();

	Op(write_list, e);
	BEGD(d0);
	d0 = AbsPair(HR);
	*SREG++ = d0;
	/* I will not actually store the mode in the stack */
	SP[-1] = Unsigned(SREG);
	SP[-2] = 1;           /* Put instructions follow the main stream */
	SP -= 2;
	SREG = HR;
	HR += 2;
	ENDD(d0);
	PREG = NEXTOP(PREG, e);
	GONext();
	ENDOp();

	Op(write_l_list, e);
	ALWAYS_START_PREFETCH(e);
	PREG = NEXTOP(PREG, e);
	BEGD(d0);
	CACHE_S();
	READ_IN_S();
	d0 = AbsPair(HR);
	*S_SREG = d0;
	WRITEBACK_S(HR);
	HR += 2;
	ENDCACHE_S();
	ENDD(d0);
	ALWAYS_GONext();
	ALWAYS_END_PREFETCH();
	ENDOp();

	Op(write_struct, fa);
	BEGD(d0);
	d0 = AbsAppl(HR);
	*SREG++ = d0;
	SP[-1] = Unsigned(SREG);
	SP[-2] = 1;           /* Put instructions follow the main stream */
	SP -= 2;
	d0 = (CELL) (PREG->y_u.fa.f);
	*HR++ = d0;
	ENDD(d0);
	BEGD(d0);
	d0 = PREG->y_u.fa.a;
	PREG = NEXTOP(PREG, fa);
	SREG = HR;
	HR += d0;
	ENDD(d0);
	GONext();
	ENDOp();

	Op(write_l_struc, fa);
	BEGD(d0);
	d0 = AbsAppl(HR);
	*SREG = d0;
	d0 = (CELL) (PREG->y_u.fa.f);
	*HR++ = d0;
	SREG = HR;
	ENDD(d0);
	BEGD(d0);
	d0 = PREG->y_u.fa.a;
	PREG = NEXTOP(PREG, fa);
	HR += d0;
	ENDD(d0);
	GONext();
	ENDOp();

	/************************************************************************\
	 *   Save last unified struct or list                                 *
\************************************************************************/

	/* vitor: I think I should kill these two instructions, by expanding the
	 * othe instructions.
	 */

	Op(save_pair_x, ox);
	XREG(PREG->y_u.ox.x) = AbsPair(SREG);
	PREG = NEXTOP(PREG, ox);
	GONext();
	ENDOp();

	OpW(save_pair_x_write, ox);
	XREG(PREG->y_u.ox.x) = AbsPair(SREG);
	PREG = NEXTOP(PREG, ox);
	GONextW();
	ENDOpW();

	Op(save_pair_y, oy);
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,AbsPair(SREG));
	PREG = NEXTOP(PREG, oy);
	GONext();
	ENDOp();

	OpW(save_pair_y_write, oy);
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,AbsPair(SREG));
	PREG = NEXTOP(PREG, oy);
	GONextW();
	ENDOpW();

	Op(save_appl_x, ox);
	XREG(PREG->y_u.ox.x) = AbsAppl(SREG - 1);
	PREG = NEXTOP(PREG, ox);
	GONext();
	ENDOp();

	OpW(save_appl_x_write, ox);
	XREG(PREG->y_u.ox.x) = AbsAppl(SREG - 1);
	PREG = NEXTOP(PREG, ox);
	GONextW();
	ENDOpW();

	Op(save_appl_y, oy);
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,AbsAppl(SREG-1));
	PREG = NEXTOP(PREG, oy);
	GONext();
	ENDOp();

	OpW(save_appl_y_write, oy);
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,AbsAppl(SREG-1));
	PREG = NEXTOP(PREG, oy);
	GONextW();
	ENDOpW();


	/************************************************************************\
	 *   Instructions for implemeting 'or;'                                        *
\************************************************************************/

	BOp(jump, l);
	PREG = PREG->y_u.l.l;
	JMPNext();
	ENDBOp();

	/* This instruction is called when the previous goal
	   was interrupted when waking up goals
	*/
	BOp(move_back, l);
	PREG = (yamop *)(((char *)PREG)-(Int)(NEXTOP((yamop *)NULL,Osbpp)));
	JMPNext();
	ENDBOp();

	/* This instruction is called when the previous goal
	   was interrupted when waking up goals
	*/
	BOp(skip, l);
	PREG = NEXTOP(PREG,l);
	JMPNext();
	ENDBOp();

	Op(either, Osblp);
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  low_level_trace(try_or, (PredEntry *)PREG, NULL);
	}
#endif
#ifdef COROUTINING
	CACHE_Y_AS_ENV(YREG);
	check_stack(NoStackEither, HR);
	ENDCACHE_Y_AS_ENV();
      either_notest:
#endif
	BEGD(d0);
	/* Try to preserve the environment */
	d0 = PREG->y_u.Osblp.s;
	BEGCHO(pt1);
	pt1 = (choiceptr) ((char *) YREG + (yslot) d0);
#ifdef FROZEN_STACKS
	{
	  choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
	  if (pt1 > top_b || pt1 < (choiceptr)HR) pt1 = top_b;
#else
	  if (pt1 > top_b) pt1 = top_b;
#endif /* YAPOR_SBA */
	}
#else
	if (pt1 > B) {
	  pt1 = B;
	}
#endif /* FROZEN_STACKS */
	pt1 = (choiceptr)(((CELL *) pt1)-1);
	*(CELL **) pt1 = YREG;
	store_yaam_regs_for_either(PREG->y_u.Osblp.l, PREG);
	SREG = (CELL *) (B = pt1);
#ifdef YAPOR
	SCH_set_load(pt1);
#endif  /* YAPOR */
	SET_BB(pt1);
	ENDCHO(pt1);
	/* skip the current instruction plus the next one */
	PREG = NEXTOP(NEXTOP(PREG, Osblp),l);
	GONext();
	ENDD(d0);

#ifdef COROUTINING
      NoStackEither:
	PROCESS_INT(interrupt_either, either_notest);
#endif

	ENDOp();

	Op(or_else, Osblp);
	HR = HBREG = PROTECT_FROZEN_H(B);
	ENV = B->cp_env;
	B->cp_cp = PREG;
#ifdef DEPTH_LIMIT
	DEPTH = B->cp_depth;
#endif  /* DEPTH_LIMIT */
	SET_BB(PROTECT_FROZEN_B(B));
#ifdef YAPOR
	if (SCH_top_shared_cp(B)) {
	  SCH_new_alternative(PREG, PREG->y_u.Osblp.l);
	} else
#endif  /* YAPOR */
	  B->cp_ap = PREG->y_u.Osblp.l;
	PREG = NEXTOP(PREG, Osblp);
	YREG = (CELL *) B->cp_a1;
	GONext();
	ENDOp();

#ifdef YAPOR
	Op(or_last, Osblp);
#else
	Op(or_last, p);
#endif  /* YAPOR */
	BEGCHO(pt0);
	pt0 = B;
#ifdef YAPOR
	if (SCH_top_shared_cp(B)) {
	  HR = HBREG = PROTECT_FROZEN_H(pt0);
	  YREG = (CELL *) pt0->cp_a1;
	  ENV = pt0->cp_env;
#ifdef DEPTH_LIMIT
	  DEPTH = pt0->cp_depth;
#endif  /* DEPTH_LIMIT */
	  SCH_new_alternative(PREG, NULL);
	}
	else
#endif  /* YAPOR */
	  {
	    B = pt0->cp_b;
	    HR = PROTECT_FROZEN_H(pt0);
	    YREG = (CELL *) pt0->cp_a1;
	    ENV = pt0->cp_env;
#ifdef DEPTH_LIMIT
	    DEPTH = pt0->cp_depth;
#endif  /* DEPTH_LIMIT */
	    HBREG = PROTECT_FROZEN_H(B);
	  }
#ifdef YAPOR
	PREG = NEXTOP(PREG, Osblp);
#else
	PREG = NEXTOP(PREG, p);
#endif  /* YAPOR */
	SET_BB(PROTECT_FROZEN_B(B));
	GONext();
	ENDCHO(pt0);
	ENDOp();

	/************************************************************************\
	 *    Pop operations                                                   *
\************************************************************************/

	OpRW(pop_n, s);
	/* write mode might have been called from read mode */
	BEGD(d0);
	d0 = PREG->y_u.os.s;
	SP = (CELL *) (((char *) SP) + d0);
	ENDD(d0);
	BEGD(d0);
	d0 = SP[0];
	if (d0) {
	  START_PREFETCH(s);
	  SREG = (CELL *) (SP[1]);
	  SP += 2;
	  PREG = NEXTOP(PREG, s);
	  GONext();
	  END_PREFETCH();
	}
	else {
	  START_PREFETCH_W(s);
	  SREG = (CELL *) (SP[1]);
	  SP += 2;
	  PREG = NEXTOP(PREG, s);
	  GONextW();
	  END_PREFETCH_W();
	}
	ENDD(d0);
	ENDOpRW();

	OpRW(pop, e);
	BEGD(d0);
	d0 = SP[0];
	SREG = (CELL *) (SP[1]);
	SP += 2;
	if (d0) {
	  START_PREFETCH(e);
	  PREG = NEXTOP(PREG, e);
	  GONext();
	  END_PREFETCH();
	}
	else {
	  START_PREFETCH_W(e);
	  PREG = NEXTOP(PREG, e);
	  GONextW();
	  END_PREFETCH_W();
	}
	ENDD(d0);
	ENDOpRW();

	/************************************************************************\
	 *    Call C predicates instructions                                   *
\************************************************************************/

	BOp(call_cpred, Osbpp);
	check_trail(TR);
	if (!(PREG->y_u.Osbpp.p->PredFlags & (SafePredFlag|NoTracePredFlag|HiddenPredFlag))) {
	  CACHE_Y_AS_ENV(YREG);
	  check_stack(NoStackCCall, HR);
	  ENDCACHE_Y_AS_ENV();
	}
      do_c_call:
#ifdef FROZEN_STACKS
	{
	  choiceptr top_b = PROTECT_FROZEN_B(B);

#ifdef YAPOR_SBA
	  if (YREG > (CELL *) top_b || YREG < HR) ASP = (CELL *)top_b;
#else
	  if (YREG > (CELL *) top_b) ASP = (CELL *)top_b;
#endif /* YAPOR_SBA */
	  else ASP = (CELL *)(((char *)YREG) +  PREG->y_u.Osbpp.s);
	}
#else
	SET_ASP(YREG, PREG->y_u.Osbpp.s);
	/* for slots to work */
#endif /* FROZEN_STACKS */
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace)
	  low_level_trace(enter_pred,PREG->y_u.Osbpp.p,XREGS+1);
#endif  /* LOW_LEVEL_TRACE */
	BEGD(d0);
	PP = PREG->Osbpp.p0;
	PREG = NEXTOP(PREG, Osbpp);
	saveregs();
	d0 = (f)(PASS_REGS1);
	setregs();
	CPredicate f = PREG->y_u.Osbpp.p->cs.f_code;
#ifdef SHADOW_S
	SREG = Yap_REGS.S_;
#endif
	if (!d0) {
	  FAIL();
	}
	CACHE_A1();
	ENDD(d0);
	JMPNext();

      NoStackCCall:
	PROCESS_INT(interrupt_call, do_c_call);

	ENDBOp();

	/* execute     Label               */
	BOp(execute_cpred, pp);
	check_trail(TR);
	{
	  PredEntry *pt0;

	  BEGD(d0);
	  CACHE_Y_AS_ENV(YREG);
#ifndef NO_CHECKING
	  check_stack(NoStackExecuteC, HR);
	do_executec:
#endif
#ifdef FROZEN_STACKS
	  {
	    choiceptr top_b = PROTECT_FROZEN_B(B);

#ifdef YAPOR_SBA
	    if (YREG > (CELL *) top_b || YREG < HR) ASP = (CELL *)top_b;
#else
	    if (YREG > (CELL *) top_b) ASP = (CELL *)top_b;
#endif /* YAPOR_SBA */
	    else ASP = YREG+E_CB;
	  }
#else
	  SET_ASP(YREG, E_CB*sizeof(CELL));
	  /* for slots to work */
#endif /* FROZEN_STACKS */
	  pt0 = PREG->y_u.pp.p;
#ifdef LOW_LEVEL_TRACER
	  if (Yap_do_low_level_trace) {
	    low_level_trace(enter_pred,pt0,XREGS+1);
	  }
#endif  /* LOW_LEVEL_TRACE */
	  CACHE_A1();
	  BEGD(d0);
	  d0 = (CELL)B;
	  /* for profiler */
	  save_pc();
	  ENV_YREG[E_CB] = d0;
	  ENDD(d0);
#ifdef DEPTH_LIMIT
	  if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is prolog */
	    if (pt0->ModuleOfPred) {
	      if (DEPTH == MkIntTerm(0))
		FAIL();
	      else DEPTH = RESET_DEPTH();
	    }
	  } else if (pt0->ModuleOfPred) {
	    DEPTH -= MkIntConstant(2);
	  }
#endif  /* DEPTH_LIMIT */
        /* now call C-Code */
	  {
	    CPredicate f = PREG->y_u.pp.p->cs.f_code;
	    PP = PREG->y_u.pp.p0;
	    yamop *oldPREG = PREG;
	    saveregs();
	    d0 = (f)(PASS_REGS1);
	    setregs();
#ifdef SHADOW_S
	    SREG = Yap_REGS.S_;
#endif
	    if (!d0) {
	      FAIL();
	    }
	    if (oldPREG == PREG) {
	      /* we did not update PREG */
	      /* we can proceed */
	      PREG = CPREG;
	      ENV_YREG = ENV;
#ifdef DEPTH_LIMIT
	      DEPTH = ENV_YREG[E_DEPTH];
#endif
	      WRITEBACK_Y_AS_ENV();
	    } else {
	      /* call the new code  */
	      CACHE_A1();
	    }
	  }
	  JMPNext();
	  ENDCACHE_Y_AS_ENV();
	  ENDD(d0);
	}

      NoStackExecuteC:
	PROCESS_INT(interrupt_execute, do_executec);
	ENDBOp();

	/* Like previous, the only difference is that we do not */
	/* trust the C-function we are calling and hence we must */
	/* guarantee that *all* machine registers are saved and */
	/* restored */
	BOp(call_usercpred, Osbpp);
	CACHE_Y_AS_ENV(YREG);
	check_stack(NoStackUserCall, HR);
	ENDCACHE_Y_AS_ENV();
      do_user_call:
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  low_level_trace(enter_pred,PREG->y_u.Osbpp.p,XREGS+1);
	}
#endif  /* LOW_LEVEL_TRACE */
#ifdef FROZEN_STACKS
	{
	  choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
	  if (YREG > (CELL *) top_b || YREG < HR) ASP = (CELL *) top_b;
#else
	  if (YREG > (CELL *) top_b) ASP = (CELL *) top_b;
#endif /* YAPOR_SBA */
	  else ASP = (CELL *)(((char *)YREG) +  PREG->y_u.Osbpp.s);
	}
#else
	SET_ASP(YREG, PREG->y_u.Osbpp.s);
	/* for slots to work */
#endif /* FROZEN_STACKS */
	{
	  /* make sure that we can still have access to our old PREG after calling user defined goals and backtracking or failing */
	  yamop *savedP;

	  LOCAL_PrologMode |= UserCCallMode;
	  {
	    PredEntry *p = PREG->y_u.Osbpp.p;

	    PREG = NEXTOP(PREG, Osbpp);
	    savedP = PREG;
	    saveregs();
	    save_machine_regs();

	    SREG = (CELL *) YAP_Execute(p, p->cs.f_code);
	  }
	  setregs();
	  LOCAL_PrologMode &= ~UserCCallMode;
	  restore_machine_regs();
	  PREG = savedP;
	}
	if (EX) {
	  struct DB_TERM *exp = EX;
	  EX = NULL;
	  Yap_JumpToEnv(Yap_PopTermFromDB(exp));
	  SREG = NULL;
	}
	if (!SREG) {
	  FAIL();
	}
	/* in case we call Execute */
	YENV = ENV;
	YREG = ENV;
	JMPNext();

      NoStackUserCall:
	PROCESS_INT(interrupt_call, do_user_call);

	ENDBOp();

	BOp(call_c_wfail, slpp);
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  low_level_trace(enter_pred,PREG->y_u.slpp.p,XREGS+1);
	}
#endif  /* LOW_LEVEL_TRACE */
#ifdef FROZEN_STACKS
	{
	  choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
	  if (YREG > (CELL *) top_b || YREG < HR) ASP = (CELL *) top_b;
#else
	  if (YREG > (CELL *) top_b) ASP = (CELL *) top_b;
#endif /* YAPOR_SBA */
	  else {
	    BEGD(d0);
	    d0 = PREG->y_u.slpp.s;
	    ASP = ((CELL *)YREG) + d0;
	    ENDD(d0);
	  }
	}
#else
	if (YREG > (CELL *) B)
	  ASP = (CELL *) B;
	else {
	  BEGD(d0);
	  d0 = PREG->y_u.slpp.s;
	  ASP = ((CELL *) YREG) + d0;
	  ENDD(d0);
	}
#endif /* FROZEN_STACKS */
	{
	  CPredicate f = PREG->y_u.slpp.p->cs.f_code;
	  PP = PREG->y_u.slpp.p0;
	  saveregs();
	  SREG = (CELL *)((f)(PASS_REGS1));
	  setregs();
	}
	if (!SREG) {
	  /* be careful about error handling */
	  if (PREG != FAILCODE)
	    PREG = PREG->y_u.slpp.l;
	} else {
	  PREG = NEXTOP(PREG, slpp);
	}
	CACHE_A1();
	JMPNext();
	ENDBOp();

	BOp(try_c, OtapFs);
#ifdef YAPOR
	CUT_wait_leftmost();
#endif /* YAPOR */
	CACHE_Y(YREG);
	/* Alocate space for the cut_c structure*/
	CUT_C_PUSH(NEXTOP(NEXTOP(PREG,OtapFs),OtapFs),S_YREG);
	S_YREG = S_YREG - PREG->y_u.OtapFs.extra;
	store_args(PREG->y_u.OtapFs.s);
	store_yaam_regs(NEXTOP(PREG, OtapFs), 0);
	B = B_YREG;
#ifdef YAPOR
	SCH_set_load(B_YREG);
#endif  /* YAPOR */
	SET_BB(B_YREG);
	ENDCACHE_Y();

      TRYCC:
	1ASP = (CELL *)B;
	{
	  CPredicate f = (CPredicate)(PREG->y_u.OtapFs.f);
	  saveregs();
	  SREG = (CELL *) ((f) (PASS_REGS1));
	  /* This last instruction changes B B*/
	  while (POP_CHOICE_POINT(B)){
	    cut_c_pop();
	  }
	  setregs();
	}
	if (!SREG) {
	  /* Removes the cut functions from the stack
	     without executing them because we have fail
	     and not cuted the predicate*/
	  while(POP_CHOICE_POINT(B))
	    cut_c_pop();
	  FAIL();
	}
	if ((CELL *) B == YREG && ASP != (CELL *) B) {
	  /* as Luis says, the predicate that did the try C might
	   * have left some data on the stack. We should preserve
	   * it, unless the builtin also did cut */
	  YREG = ASP;
	  HBREG = PROTECT_FROZEN_H(B);
	  SET_BB(B);
	}
	PREG = CPREG;
	YREG = ENV;
	JMPNext();
	ENDBOp();

	BOp(retry_c, OtapFs);
#ifdef YAPOR
	CUT_wait_leftmost();
#endif /* YAPOR */
	CACHE_Y(B);
	CPREG = B_YREG->cp_cp;
	ENV = B_YREG->cp_env;
	HR = PROTECT_FROZEN_H(B);
#ifdef DEPTH_LIMIT
	DEPTH =B->cp_depth;
#endif
	HBREG = HR;
	restore_args(PREG->y_u.OtapFs.s);
	ENDCACHE_Y();
	goto TRYCC;
	ENDBOp();

	BOp(cut_c, OtapFs);
	/*This is a phantom instruction. This is not executed by the WAM*/
#ifdef DEBUG
	/*If WAM executes this instruction, probably there's an error
	  when we put this instruction, cut_c, after retry_c*/
	printf ("ERROR: Should not print this message FILE: absmi.c %d\n",__LINE__);
#endif /*DEBUG*/
	ENDBOp();

	BOp(try_userc, OtapFs);
#ifdef YAPOR
	CUT_wait_leftmost();
#endif /* YAPOR */
	CACHE_Y(YREG);
	/* Alocate space for the cut_c structure*/

  (NEXTOP(NEXTOP(PREG,OtapFs),OtapFs),S_YREG);
	S_YREG = S_YREG - PREG->y_u.OtapFs.extra;
	store_args(PREG->y_u.OtapFs.s);
	store_yaam_regs(NEXTOP(PREG, OtapFs), 0);
	B = B_YREG;
#ifdef YAPOR
	SCH_set_load(B_YREG);
#endif
	SET_BB(B_YREG);
	ENDCACHE_Y();
	LOCAL_PrologMode |= UserCCallMode;
	ASP = YREG;
	saveregs();
	save_machine_regs();
	SREG = (CELL *) YAP_ExecuteFirst(PREG->y_u.OtapFs.p, (CPredicate)(PREG->y_u.OtapFs.f));
	EX = NULL;
	restore_machine_regs();
	setregs();
	LOCAL_PrologMode &= ~UserCCallMode;
	if (!SREG) {
	  FAIL();
	}
	if ((CELL *) B == YREG && ASP != (CELL *) B) {
	  /* as Luis says, the predicate that did the try C might
	   * have left some data on the stack. We should preserve
	   * it, unless the builtin also did cut */
	  YREG = ASP;
	  HBREG = PROTECT_FROZEN_H(B);
	}
	PREG = CPREG;
	YREG = ENV;
	CACHE_A1();
	JMPNext();
	ENDBOp();

	BOp(retry_userc, OtapFs);
#ifdef YAPOR
	CUT_wait_leftmost();
#endif /* YAPOR */
	CACHE_Y(B);
	CPREG = B_YREG->cp_cp;
	ENV = B_YREG->cp_env;
	HR = PROTECT_FROZEN_H(B);
#ifdef DEPTH_LIMIT
	DEPTH =B->cp_depth;
#endif
	HBREG = HR;
	restore_args(PREG->y_u.OtapFs.s);
	ENDCACHE_Y();

	LOCAL_PrologMode |= UserCCallMode;
	SET_ASP(YREG, E_CB*sizeof(CELL));
	saveregs();
	save_machine_regs();
	SREG = (CELL *) YAP_ExecuteNext(PREG->y_u.OtapFs.p, (CPredicate)(PREG->y_u.OtapFs.f));
	EX = NULL;
	restore_machine_regs();
	setregs();
	LOCAL_PrologMode &= ~UserCCallMode;
	if (!SREG) {
	  /* Removes the cut functions from the stack
	     without executing them because we have fail
	     and not cuted the predicate*/
	  while(POP_CHOICE_POINT(B))
	    cut_c_pop();
	  FAIL();
	}
	if ((CELL *) B == YREG && ASP != (CELL *) B) {
	  /* as Luis says, the predicate that did the try C might
	   * have left some data on the stack. We should preserve
	   * it, unless the builtin also did cut */
	  YREG = ASP;
	  HBREG = PROTECT_FROZEN_H(B);
	}
	PREG = CPREG;
	YREG = ENV;
	CACHE_A1();
	JMPNext();
	ENDBOp();

	BOp(cut_userc, OtapFs);
	/*This is a phantom instruction. This is not executed by the WAM*/
#ifdef DEBUG
	/*If WAM executes this instruction, probably there's an error
	  when we put this instruction, cut_userc, after retry_userc*/
	printf ("ERROR: Should not print this message FILE: absmi.c %d\n",__LINE__);
#endif /*DEBUG*/
	CACHE_A1();
	JMPNext();
	ENDBOp();


	/************************************************************************\
	 *    support instructions                                             *
\************************************************************************/

	BOp(lock_pred, e);
	{
	  PredEntry *ap = PredFromDefCode(PREG);
	  PELOCK(10,ap);
	  PP = ap;
	  if (!ap->NOfClauses) {
	    UNLOCKPE(11,ap);
	    FAIL();
	  }
	  /*
	    we do not lock access to the predicate,
	    we must take extra care here
	  */
	  if (ap->NOfClauses > 1 &&
	      !(ap->PredFlags & IndexedPredFlag)) {
	    /* update ASP before calling IPred */
	    SET_ASP(YREG, E_CB*sizeof(CELL));
	    saveregs();
	    Yap_IPred(ap, 0, CP);
	    /* IPred can generate errors, it thus must get rid of the lock itself */
	    setregs();
	    CACHE_A1();
	    /* for profiler */
	    save_pc();
	  }
	  PREG = ap->TrueCodeOfPred;
	}
	JMPNext();
	ENDBOp();

	BOp(index_pred, e);
	{
	  PredEntry *ap = PredFromDefCode(PREG);
#if defined(YAPOR) || defined(THREADS)
	  /*
	    we do not lock access to the predicate,
	    we must take extra care here
	  */
	  if (!PP) {
	    PELOCK(11,ap);
	  }
	  if (ap->OpcodeOfPred != INDEX_OPCODE) {
	    /* someone was here before we were */
	    if (!PP) {
	      UNLOCKPE(11,ap);
	    }
	    PREG = ap->CodeOfPred;
	    /* for profiler */
	    save_pc();
	    JMPNext();
	  }
#endif
	  /* update ASP before calling IPred */
	  SET_ASP(YREG, E_CB*sizeof(CELL));
	  saveregs();
	  Yap_IPred(ap, 0, CP);
	  /* IPred can generate errors, it thus must get rid of the lock itself */
	  setregs();
	  CACHE_A1();
	  PREG = ap->CodeOfPred;
	  /* for profiler */
	  save_pc();
#if defined(YAPOR) || defined(THREADS)
	  if (!PP)
#endif
	    UNLOCKPE(14,ap);

	}
	JMPNext();
	ENDBOp();

#if THREADS
	BOp(thread_local, e);
	{
	  PredEntry *ap = PredFromDefCode(PREG);
	  ap = Yap_GetThreadPred(ap PASS_REGS);
	  PREG = ap->CodeOfPred;
	  /* for profiler */
	  save_pc();
	}
	JMPNext();
	ENDBOp();
#endif

	BOp(expand_index, e);
	{
	  PredEntry *pe = PredFromExpandCode(PREG);
	  yamop *pt0;

	  /* update ASP before calling IPred */
	  SET_ASP(YREG, E_CB*sizeof(CELL));
#if defined(YAPOR) || defined(THREADS)
	  if (!PP) {
	    PELOCK(12,pe);
	  }
	  if (!same_lu_block(PREG_ADDR, PREG)) {
	    PREG = *PREG_ADDR;
	    if (!PP) {
	      UNLOCKPE(15,pe);
	    }
	    JMPNext();
	  }
#endif
#ifdef SHADOW_S
	  S = SREG;
#endif /* SHADOW_S */
	  saveregs();
	  pt0 = Yap_ExpandIndex(pe, 0);
	  /* restart index */
	  setregs();
#ifdef SHADOW_S
	  SREG = S;
#endif /* SHADOW_S */
	  PREG = pt0;
#if defined(YAPOR) || defined(THREADS)
	  if (!PP) {
	    UNLOCKPE(12,pe);
	  }
#endif
	  JMPNext();
	}
	ENDBOp();

	BOp(expand_clauses, sssllp);
	{
	  PredEntry *pe = PREG->y_u.sssllp.p;
	  yamop *pt0;

	  /* update ASP before calling IPred */
	  SET_ASP(YREG, E_CB*sizeof(CELL));
#if defined(YAPOR) || defined(THREADS)
	  if (PP == NULL) {
	    PELOCK(13,pe);
	  }
	  if (!same_lu_block(PREG_ADDR, PREG)) {
	    PREG = *PREG_ADDR;
	    if (!PP) {
	      UNLOCKPE(16,pe);
	    }
	    JMPNext();
	  }
#endif
	  saveregs();
	  pt0 = Yap_ExpandIndex(pe, 0);
	  /* restart index */
	  setregs();
	  PREG = pt0;
#if defined(YAPOR) || defined(THREADS)
	  if (!PP) {
	    UNLOCKPE(18,pe);
	  }
#endif
	  JMPNext();
	}
	ENDBOp();

	BOp(undef_p, e);
	/* save S for module name */
	saveregs();
	undef_goal( PASS_REGS1 );
	setregs();
	/* for profiler */
	CACHE_A1();
	JMPNext();
	ENDBOp();

	BOp(spy_pred, e);
	saveregs();
	spy_goal( PASS_REGS1 );
	setregs();
	CACHE_A1();
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
#endif  /* YAPOR */
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
#endif  /* YAPOR */
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
#endif  /* YAPOR */
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
#endif  /* YAPOR */
	SET_BB(B_YREG);
	ENDCACHE_Y();
	JMPNext();
	ENDBOp();

	BOp(retry, Otapl);
	CACHE_Y(B);
	restore_yaam_regs(NEXTOP(PREG, Otapl));
	restore_at_least_one_arg(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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
	  S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
	  set_cut(S_YREG, B->cp_b);
	}
	else
#endif  /* YAPOR */
	  {
	    pop_yaam_regs();
	    pop_at_least_one_arg(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
	    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
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



	/************************************************************************\
	 *    Logical Updates                                                 *
\************************************************************************/

	/* enter logical pred               */
	BOp(enter_lu_pred, Illss);
	check_trail(TR);
	/* mark the indexing code */
	{
	  LogUpdIndex *cl = PREG->y_u.Illss.I;
	  PredEntry *ap = cl->ClPred;

	  if (!cl) { FAIL(); } /* in case the index is empty */
	  if (ap->LastCallOfPred != LUCALL_EXEC) {
	    /*
	      only increment time stamp if we are working on current time
	      stamp
	    */
	    if (ap->TimeStampOfPred >= TIMESTAMP_RESET)
	      Yap_UpdateTimestamps(ap);
	    ap->TimeStampOfPred++;
	    ap->LastCallOfPred = LUCALL_EXEC;
	    /*          fprintf(stderr,"R %x--%d--%ul\n",ap,ap->TimeStampOfPred,ap->ArityOfPE);*/
	  }
	  *--YREG = MkIntegerTerm(ap->TimeStampOfPred);
	  /* fprintf(stderr,"> %p/%p %d %d\n",cl,ap,ap->TimeStampOfPred,PREG->y_u.Illss.s);*/
	  PREG = PREG->y_u.Illss.l1;
	  /* indicate the indexing code is being used */
#if MULTIPLE_STACKS
	  /* just store a reference */
	  INC_CLREF_COUNT(cl);
	  TRAIL_CLREF(cl);
#else
	  if (!(cl->ClFlags & InUseMask)) {
	    cl->ClFlags |= InUseMask;
	    TRAIL_CLREF(cl);
	  }
#endif
	}
	JMPNext();
	ENDBOp();

	BOp(try_logical, OtaLl);
	check_trail(TR);
	{
	  UInt timestamp;

	  CACHE_Y(YREG);
	  timestamp = IntegerOfTerm(S_YREG[0]);
	  /* fprintf(stderr,"+ %p/%p %d %d %d--%u\n",PREG,PREG->y_u.OtaLl.d->ClPred,timestamp,PREG->y_u.OtaLl.d->ClPred->TimeStampOfPred,PREG->y_u.OtaLl.d->ClTimeStart,PREG->y_u.OtaLl.d->ClTimeEnd);*/
	  /* Point AP to the code that follows this instruction */
	  /* always do this, even if we are not going to use it */
	  store_args(PREG->y_u.OtaLl.s);
	  store_yaam_regs(PREG->y_u.OtaLl.n, 0);
	  set_cut(S_YREG, B);
	  B = B_YREG;
#ifdef YAPOR
	  SCH_set_load(B_YREG);
#endif  /* YAPOR */
#ifdef YAPOR
	  PP = PREG->y_u.OtaLl.d->ClPred;
#endif  /* YAPOR */
	  if (!VALID_TIMESTAMP(timestamp, PREG->y_u.OtaLl.d)) {
	    /* jump to next alternative */
	    PREG=PREG->y_u.OtaLl.n;
	  } else {
	    PREG = PREG->y_u.OtaLl.d->ClCode;
	  }
	  SET_BB(B_YREG);
	  ENDCACHE_Y();
	}
	JMPNext();
	ENDBOp();

	BOp(retry_logical, OtaLl);
	check_trail(TR);
	{
	  UInt timestamp;
	  CACHE_Y(B);

#if defined(YAPOR) || defined(THREADS)
	  if (PP != PREG->y_u.OtaLl.d->ClPred) {
	    if (PP) UNLOCKPE(15,PP);
	    PP = PREG->y_u.OtaLl.d->ClPred;
	    PELOCK(15,PP);
	  }
#endif
	  timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[PREG->y_u.OtaLl.s]);
	  /* fprintf(stderr,"^ %p/%p %d %d %d--%u\n",PREG,PREG->y_u.OtaLl.d->ClPred,timestamp,PREG->y_u.OtaLl.d->ClPred->TimeStampOfPred,PREG->y_u.OtaLl.d->ClTimeStart,PREG->y_u.OtaLl.d->ClTimeEnd);*/
	  if (!VALID_TIMESTAMP(timestamp, PREG->y_u.OtaLl.d)) {
	    /* jump to next instruction */
	    PREG=PREG->y_u.OtaLl.n;
	    JMPNext();
	  }
	  restore_yaam_regs(PREG->y_u.OtaLl.n);
	  restore_at_least_one_arg(PREG->y_u.OtaLl.s);
#ifdef THREADS
	  PP = PREG->y_u.OtaLl.d->ClPred;
#endif
	  PREG = PREG->y_u.OtaLl.d->ClCode;
#ifdef FROZEN_STACKS
	  S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
	  set_cut(S_YREG, B->cp_b);
#else
	  set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
	  SET_BB(B_YREG);
	  ENDCACHE_Y();
	}
	JMPNext();
	ENDBOp();

	BOp(trust_logical, OtILl);
	CACHE_Y(B);
	{
	  LogUpdIndex *cl = PREG->y_u.OtILl.block;
	  PredEntry *ap = cl->ClPred;
	  LogUpdClause *lcl = PREG->y_u.OtILl.d;
	  UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]);

	  /* fprintf(stderr,"- %p/%p %d %d %p\n",PREG,ap,timestamp,ap->TimeStampOfPred,PREG->y_u.OtILl.d->ClCode);*/
#if defined(YAPOR) || defined(THREADS)
	  if (PP != ap) {
	    if (PP) UNLOCKPE(16,PP);
	    PP = ap;
	    PELOCK(16,PP);
	  }
#endif
	  if (!VALID_TIMESTAMP(timestamp, lcl)) {
	    /* jump to next alternative */
	    PREG = FAILCODE;
	  } else {
	    PREG = lcl->ClCode;
	  }
	  /* HEY, leave indexing block alone!! */
	  /* check if we are the ones using this code */
#if MULTIPLE_STACKS
	  DEC_CLREF_COUNT(cl);
	  /* clear the entry from the trail */
	  B->cp_tr--;
	  TR = B->cp_tr;
	  /* actually get rid of the code */
	  if (cl->ClRefCount == 0 && (cl->ClFlags & (ErasedMask|DirtyMask))) {
	    if (PREG != FAILCODE) {
	      if (lcl->ClRefCount == 1) {
		/* make sure the clause isn't destroyed */
		/* always add an extra reference */
		INC_CLREF_COUNT(lcl);
		TRAIL_CLREF(lcl);
		B->cp_tr = TR;
	      }
	    }
	    if (cl->ClFlags & ErasedMask) {
	      saveregs();
	      Yap_ErLogUpdIndex(cl);
	      setregs();
	    } else {
	      saveregs();
	      Yap_CleanUpIndex(cl);
	      setregs();
	    }
	    save_pc();
	  }
#else
	  if (TrailTerm(B->cp_tr-1) == CLREF_TO_TRENTRY(cl) &&
	      B->cp_tr != B->cp_b->cp_tr) {
	    cl->ClFlags &= ~InUseMask;
	    B->cp_tr--;
#if FROZEN_STACKS
	    if (B->cp_tr > TR_FZ)
#endif
	      {
		TR = B->cp_tr;
	      }
	    /* next, recover space for the indexing code if it was erased */
	    if (cl->ClFlags & (ErasedMask|DirtyMask)) {
	      if (PREG != FAILCODE) {
		/* make sure we don't erase the clause we are jumping too */
		if (!(lcl->ClFlags & InUseMask)) {
		  lcl->ClFlags |= InUseMask;
		  TRAIL_CLREF(lcl);
		  B->cp_tr = TR;
		}
	      }
	      if (cl->ClFlags & ErasedMask) {
		saveregs();
		Yap_ErLogUpdIndex(cl);
		setregs();
	      } else {
		saveregs();
		Yap_CleanUpIndex(cl);
		setregs();
	      }
	    }
	  }
#endif
#ifdef YAPOR
	  if (SCH_top_shared_cp(B)) {
	    SCH_last_alternative(PREG, B_YREG);
	    restore_args(ap->ArityOfPE);
#ifdef FROZEN_STACKS
	    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#else
	    S_YREG++;
#endif /* FROZEN_STACKS */
	    set_cut(S_YREG, B->cp_b);
	  } else
#endif  /* YAPOR */
	    {
	      pop_yaam_regs();
	      pop_args(ap->ArityOfPE);
	      S_YREG--;
#ifdef FROZEN_STACKS
	      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
	      set_cut(S_YREG, B);
	    }
	  SET_BB(B_YREG);
	  ENDCACHE_Y();
#if defined(YAPOR) || defined(THREADS)
	  if (PREG == FAILCODE) {
	    UNLOCKPE(26,PP);
	    PP = NULL;
	  }
#endif
	  JMPNext();
	}
	ENDBOp();


	/************************************************************************\
	 *    Indexing in ARG1                                                *
\************************************************************************/

	BOp(user_switch, lp);
	{
	  yamop *new = Yap_udi_search(PREG->y_u.lp.p);
	  if (!new) {
	    PREG = PREG->y_u.lp.l;
	    JMPNext();
	  }
	  PREG = new;
	  JMPNext();
	}
	ENDBOp();

	BOp(switch_on_type, llll);
	BEGD(d0);
	d0 = CACHED_A1();
	deref_head(d0, swt_unk);
	/* nonvar */
      swt_nvar:
	if (IsPairTerm(d0)) {
	  /* pair */
	  SREG = RepPair(d0);
	  copy_jmp_address(PREG->y_u.llll.l1);
	  PREG = PREG->y_u.llll.l1;
	  JMPNext();
	}
	else if (!IsApplTerm(d0)) {
	  /* constant */
	  copy_jmp_address(PREG->y_u.llll.l2);
	  PREG = PREG->y_u.llll.l2;
	  I_R = d0;
	  JMPNext();
	}
	else {
	  /* appl */
	  copy_jmp_address(PREG->y_u.llll.l3);
	  PREG = PREG->y_u.llll.l3;
	  SREG = RepAppl(d0);
	  JMPNext();
	}

	BEGP(pt0);
	deref_body(d0, pt0, swt_unk, swt_nvar);
	/* variable */
	copy_jmp_address(PREG->y_u.llll.l4);
	PREG = PREG->y_u.llll.l4;
	JMPNext();
	ENDP(pt0);
	ENDD(d0);
	ENDBOp();

	/* specialised case where the arguments may be:
	 * a list;
	 * the empty list;
	 * some other atom;
	 * a variable;
	 *
	 */
	BOp(switch_list_nl, ollll);
	ALWAYS_LOOKAHEAD(PREG->y_u.ollll.pop);
	BEGD(d0);
	d0 = CACHED_A1();
#if UNIQUE_TAG_FOR_PAIRS
	deref_list_head(d0, swlnl_unk_p);
      swlnl_list_p:
	{
#else
	  deref_head(d0, swlnl_unk_p);
	  /* non variable */
	swlnl_nvar_p:
	  if (__builtin_expect(IsPairTerm(d0),1)) {
	    /* pair */
#endif
	    copy_jmp_address(PREG->y_u.ollll.l1);
	    PREG = PREG->y_u.ollll.l1;
	    SREG = RepPair(d0);
	    ALWAYS_GONext();
	  }
#if UNIQUE_TAG_FOR_PAIRS
	swlnl_nlist_p:
#endif
	  if (d0 == TermNil) {
	    /* empty list */
	    PREG = PREG->y_u.ollll.l2;
	    JMPNext();
	  }
	  else {
	    /* appl or constant */
	    if (IsApplTerm(d0)) {
	      copy_jmp_address(PREG->y_u.ollll.l3);
	      PREG = PREG->y_u.ollll.l3;
	      SREG = RepAppl(d0);
	      JMPNext();
	    } else {
	      copy_jmp_address(PREG->y_u.ollll.l3);
	      PREG = PREG->y_u.ollll.l3;
	      I_R = d0;
	      JMPNext();
	    }
	  }

	  BEGP(pt0);
#if UNIQUE_TAG_FOR_PAIRS
	swlnl_unk_p:
	  deref_list_body(d0, pt0, swlnl_list_p, swlnl_nlist_p);
#else
	  deref_body(d0, pt0, swlnl_unk_p, swlnl_nvar_p);
#endif
	  ENDP(pt0);
	  /* variable */
	  copy_jmp_address(PREG->y_u.ollll.l4);
	  PREG = PREG->y_u.ollll.l4;
	  JMPNext();
	  ENDD(d0);
	}
	ENDBOp();

	BOp(switch_on_arg_type, xllll);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xllll.x);
	deref_head(d0, arg_swt_unk);
	/* nonvar */
      arg_swt_nvar:
	if (IsPairTerm(d0)) {
	  /* pair */
	  copy_jmp_address(PREG->y_u.xllll.l1);
	  PREG = PREG->y_u.xllll.l1;
	  SREG = RepPair(d0);
	  JMPNext();
	}
	else if (!IsApplTerm(d0)) {
	  /* constant */
	  copy_jmp_address(PREG->y_u.xllll.l2);
	  PREG = PREG->y_u.xllll.l2;
	  I_R = d0;
	  JMPNext();
	}
	else {
	  /* appl */
	  copy_jmp_address(PREG->y_u.xllll.l3);
	  PREG = PREG->y_u.xllll.l3;
	  SREG = RepAppl(d0);
	  JMPNext();
	}

	BEGP(pt0);
	deref_body(d0, pt0, arg_swt_unk, arg_swt_nvar);
	/* variable */
	copy_jmp_address(PREG->y_u.xllll.l4);
	PREG = PREG->y_u.xllll.l4;
	JMPNext();
	ENDP(pt0);
	ENDD(d0);
	ENDBOp();

	BOp(switch_on_sub_arg_type, sllll);
	BEGD(d0);
	d0 = SREG[PREG->y_u.sllll.s];
	deref_head(d0, sub_arg_swt_unk);
	/* nonvar */
      sub_arg_swt_nvar:
	if (IsPairTerm(d0)) {
	  /* pair */
	  copy_jmp_address(PREG->y_u.sllll.l1);
	  PREG = PREG->y_u.sllll.l1;
	  SREG = RepPair(d0);
	  JMPNext();
	}
	else if (!IsApplTerm(d0)) {
	  /* constant */
	  copy_jmp_address(PREG->y_u.sllll.l2);
	  PREG = PREG->y_u.sllll.l2;
	  I_R = d0;
	  JMPNext();
	}
	else {
	  /* appl */
	  copy_jmp_address(PREG->y_u.sllll.l3);
	  PREG = PREG->y_u.sllll.l3;
	  SREG = RepAppl(d0);
	  JMPNext();
	}

	BEGP(pt0);
	deref_body(d0, pt0, sub_arg_swt_unk, sub_arg_swt_nvar);
	/* variable */
	copy_jmp_address(PREG->y_u.sllll.l4);
	PREG = PREG->y_u.sllll.l4;
	JMPNext();
	ENDP(pt0);
	ENDD(d0);
	ENDBOp();

	BOp(jump_if_var, l);
	BEGD(d0);
	d0 = CACHED_A1();
	deref_head(d0, jump_if_unk);
	/* non var */
      jump0_if_nonvar:
	PREG = NEXTOP(PREG, l);
	JMPNext();

	BEGP(pt0);
	deref_body(d0, pt0, jump_if_unk, jump0_if_nonvar);
	/* variable */
	copy_jmp_address(PREG->y_u.l.l);
	PREG = PREG->y_u.l.l;
	ENDP(pt0);
	JMPNext();
	ENDD(d0);
	ENDBOp();

	BOp(jump_if_nonvar, xll);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xll.x);
	deref_head(d0, jump2_if_unk);
	/* non var */
      jump2_if_nonvar:
	copy_jmp_address(PREG->y_u.xll.l1);
	PREG = PREG->y_u.xll.l1;
	JMPNext();

	BEGP(pt0);
	deref_body(d0, pt0, jump2_if_unk, jump2_if_nonvar);
	/* variable */
	PREG = NEXTOP(PREG, xll);
	ENDP(pt0);
	JMPNext();
	ENDD(d0);
	ENDBOp();

	BOp(if_not_then, clll);
	BEGD(d0);
	d0 = CACHED_A1();
	deref_head(d0, if_n_unk);
      if_n_nvar:
	/* not variable */
	if (d0 == PREG->y_u.clll.c) {
	  /* equal to test value */
	  copy_jmp_address(PREG->y_u.clll.l2);
	  PREG = PREG->y_u.clll.l2;
	  JMPNext();
	}
	else {
	  /* different from test value */
	  /* the case to optimise */
	  copy_jmp_address(PREG->y_u.clll.l1);
	  PREG = PREG->y_u.clll.l1;
	  JMPNext();
	}

	BEGP(pt0);
	deref_body(d0, pt0, if_n_unk, if_n_nvar);
	ENDP(pt0);
	/* variable */
	copy_jmp_address(PREG->y_u.clll.l3);
	PREG = PREG->y_u.clll.l3;
	JMPNext();
	ENDD(d0);
	ENDBOp();

	/************************************************************************\
	 *    Indexing on ARG1                                                        *
\************************************************************************/

#define HASH_SHIFT 6

	BOp(switch_on_func, sssl);
	BEGD(d1);
	d1 = *SREG++;
	/* we use a very simple hash function to find elements in a
	 * switch table */
	{
	  CELL
	    /* first, calculate the mask */
	    Mask = (PREG->y_u.sssl.s - 1) << 1,       /* next, calculate the hash function */
	    hash = d1 >> (HASH_SHIFT - 1) & Mask;
	  CELL *base;

	  base = (CELL *)PREG->y_u.sssl.l;
	  /* PREG now points at the beginning of the hash table */
	  BEGP(pt0);
	  /* pt0 will always point at the item */
	  pt0 = base + hash;
	  BEGD(d0);
	  d0 = pt0[0];
	  /* a match happens either if we found the value, or if we
	   * found an empty slot */
	  if (d0 == d1 || d0 == 0) {
	    copy_jmp_addressa(pt0+1);
	    PREG = (yamop *) (pt0[1]);
	    JMPNext();
	  }
	  else {
	    /* ooops, collision, look for other items   */
	    register CELL d = ((d1 | 1) << 1) & Mask;

	    while (1) {
	      hash = (hash + d) & Mask;
	      pt0 = base + hash;
	      d0 = pt0[0];
	      if (d0 == d1 || d0 == 0) {
		copy_jmp_addressa(pt0+1);
		PREG = (yamop *) pt0[1];
		JMPNext();
	      }
	    }
	  }
	  ENDD(d0);
	  ENDP(pt0);
	}
	ENDD(d1);
	ENDBOp();

	BOp(switch_on_cons, sssl);
	BEGD(d1);
	d1 = I_R;
	/* we use a very simple hash function to find elements in a
	 * switch table */
	{
	  CELL
	    /* first, calculate the mask */
	    Mask = (PREG->y_u.sssl.s - 1) << 1,       /* next, calculate the hash function */
	    hash = d1 >> (HASH_SHIFT - 1) & Mask;
	  CELL *base;

	  base = (CELL *)PREG->y_u.sssl.l;
	  /* PREG now points at the beginning of the hash table */
	  BEGP(pt0);
	  /* pt0 will always point at the item */
	  pt0 = base + hash;
	  BEGD(d0);
	  d0 = pt0[0];
	  /* a match happens either if we found the value, or if we
	   * found an empty slot */
	  if (d0 == d1 || d0 == 0) {
	    copy_jmp_addressa(pt0+1);
	    PREG = (yamop *) (pt0[1]);
	    JMPNext();
	  }
	  else {
	    /* ooops, collision, look for other items   */
	    register CELL d = ((d1 | 1) << 1) & Mask;

	    while (1) {
	      hash = (hash + d) & Mask;
	      pt0 = base + hash;
	      d0 = pt0[0];
	      if (d0 == d1 || d0 == 0) {
		copy_jmp_addressa(pt0+1);
		PREG = (yamop *) pt0[1];
		JMPNext();
	      }
	    }
	  }
	  ENDD(d0);
	  ENDP(pt0);
	}
	ENDD(d1);
	ENDBOp();

	BOp(go_on_func, sssl);
	BEGD(d0);
	{
	  CELL *pt = (CELL *)(PREG->y_u.sssl.l);

	  d0 = *SREG++;
	  if (d0 == pt[0]) {
	    copy_jmp_addressa(pt+1);
	    PREG = (yamop *) pt[1];
	    JMPNext();
	  } else {
	    copy_jmp_addressa(pt+3);
	    PREG = (yamop *) pt[3];
	    JMPNext();
	  }
	}
	ENDD(d0);
	ENDBOp();

	BOp(go_on_cons, sssl);
	BEGD(d0);
	{
	  CELL *pt = (CELL *)(PREG->y_u.sssl.l);

	  d0 = I_R;
	  if (d0 == pt[0]) {
	    copy_jmp_addressa(pt+1);
	    PREG = (yamop *) pt[1];
	    JMPNext();
	  } else {
	    copy_jmp_addressa(pt+3);
	    PREG = (yamop *) pt[3];
	    JMPNext();
	  }
	}
	ENDD(d0);
	ENDBOp();

	BOp(if_func, sssl);
	BEGD(d1);
	BEGP(pt0);
	pt0 = (CELL *) PREG->y_u.sssl.l;
	d1 = *SREG++;
	while (pt0[0] != d1 && pt0[0] != (CELL)NULL ) {
	  pt0 += 2;
	}
	copy_jmp_addressa(pt0+1);
	PREG = (yamop *) (pt0[1]);
	JMPNext();
	ENDP(pt0);
	ENDD(d1);
	ENDBOp();

	BOp(if_cons, sssl);
	BEGD(d1);
	BEGP(pt0);
	pt0 = (CELL *) PREG->y_u.sssl.l;
	d1 = I_R;
	while (pt0[0] != d1 && pt0[0] != 0L ) {
	  pt0 += 2;
	}
	copy_jmp_addressa(pt0+1);
	PREG = (yamop *) (pt0[1]);
	JMPNext();
	ENDP(pt0);
	ENDD(d1);
	ENDBOp();

	Op(index_dbref, e);
	PREG = NEXTOP(PREG, e);
	I_R = AbsAppl(SREG-1);
	GONext();
	ENDOp();

	Op(index_blob, e);
	PREG = NEXTOP(PREG, e);
	I_R = Yap_DoubleP_key(SREG);
	GONext();
	ENDOp();

	Op(index_long, e);
	PREG = NEXTOP(PREG, e);
	I_R = Yap_IntP_key(SREG);
	GONext();
	ENDOp();



	/************************************************************************\
	 *    Native Code Execution                                            *
\************************************************************************/

	/* native_me  */
	BOp(native_me, aFlp);

	if (PREG->y_u.aFlp.n)
	  EXEC_NATIVE(PREG->y_u.aFlp.n);
	else {
	  PREG->y_u.aFlp.n++;
	  if (PREG->y_u.aFlp.n == MAX_INVOCATION)
	    PREG->y_u.aFlp.n = Yapc_Compile(PREG->y_u.aFlp.p);
	}

	PREG = NEXTOP(PREG, aFlp);
	JMPNext();

	ENDBOp();



	/************************************************************************\
	 *    Basic Primitive Predicates                                       *
\************************************************************************/

	Op(p_atom_x, xl);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xl.x);
	deref_head(d0, atom_x_unk);
      atom_x_nvar:
	if (IsAtomTerm(d0) && !IsBlob(AtomOfTerm(d0))) {
	  PREG = NEXTOP(PREG, xl);
	  GONext();
	}
	else {
	  PREG = PREG->y_u.xl.F;
	  GONext();
	}

	BEGP(pt0);
	deref_body(d0, pt0, atom_x_unk, atom_x_nvar);
	PREG = PREG->y_u.xl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_atom_y, yl);
	BEGD(d0);
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yl.y;
	d0 = *pt0;
	deref_head(d0, atom_y_unk);
      atom_y_nvar:
	if (IsAtomTerm(d0) && !IsBlob(AtomOfTerm(d0))) {
	  PREG = NEXTOP(PREG, yl);
	  GONext();
	}
	else {
	  PREG = PREG->y_u.yl.F;
	  GONext();
	}

	derefa_body(d0, pt0, atom_y_unk, atom_y_nvar);
	PREG = PREG->y_u.yl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_atomic_x, xl);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xl.x);
	deref_head(d0, atomic_x_unk);
      atomic_x_nvar:
	/* non variable */
	if (IsAtomicTerm(d0)) {
	  PREG = NEXTOP(PREG, xl);
	  GONext();
	}
	else {
	  PREG = PREG->y_u.xl.F;
	  GONext();
	}

	BEGP(pt0);
	deref_body(d0, pt0, atomic_x_unk, atomic_x_nvar);
	PREG = PREG->y_u.xl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_atomic_y, yl);
	BEGD(d0);
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yl.y;
	d0 = *pt0;
	deref_head(d0, atomic_y_unk);
      atomic_y_nvar:
	/* non variable */
	if (IsAtomicTerm(d0)) {
	  PREG = NEXTOP(PREG, yl);
	  GONext();
	}
	else {
	  PREG = PREG->y_u.yl.F;
	  GONext();
	}

	derefa_body(d0, pt0, atomic_y_unk, atomic_y_nvar);
	PREG = PREG->y_u.yl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_integer_x, xl);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xl.x);
	deref_head(d0, integer_x_unk);
      integer_x_nvar:
	/* non variable */
	if (IsIntTerm(d0)) {
	  PREG = NEXTOP(PREG, xl);
	  GONext();
	}
	if (IsApplTerm(d0)) {
	  Functor f0 = FunctorOfTerm(d0);
	  if (IsExtensionFunctor(f0)) {
	    switch ((CELL)f0) {
	    case (CELL)FunctorBigInt:
	      { CELL *pt = RepAppl(d0);
		if (  pt[1] != BIG_RATIONAL || pt[1] != BIG_INT ) {
		  PREG = PREG->y_u.xl.F;
		  GONext();
		}
	      }
	      break;
	    case (CELL)FunctorLongInt:
	      PREG = NEXTOP(PREG, xl);
	      GONext();
	      break;
	    default:
	      PREG = PREG->y_u.xl.F;
	      GONext();
	    }
	  }
	}
	PREG = PREG->y_u.xl.F;
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, integer_x_unk, integer_x_nvar);
	PREG = PREG->y_u.xl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_integer_y, yl);
	BEGD(d0);
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yl.y;
	d0 = *pt0;
	deref_head(d0, integer_y_unk);
      integer_y_nvar:
	/* non variable */
	if (IsIntTerm(d0)) {
	  PREG = NEXTOP(PREG, yl);
	  GONext();
	}
	if (IsApplTerm(d0)) {
	  Functor f0 = FunctorOfTerm(d0);
	  if (IsExtensionFunctor(f0)) {
	    switch ((CELL)f0) {
	    case (CELL)FunctorBigInt:
	      { CELL *pt = RepAppl(d0);
		if (  pt[1] != BIG_RATIONAL || pt[1] != BIG_INT ) {
		  PREG = PREG->y_u.yl.F;
		  GONext();
		}
	      }
	    case (CELL)FunctorLongInt:
	      PREG = NEXTOP(PREG, yl);
	      GONext();
	    default:
	      PREG = PREG->y_u.yl.F;
	      GONext();
	    }
	  }
	}
	PREG = PREG->y_u.yl.F;
	GONext();

	derefa_body(d0, pt0, integer_y_unk, integer_y_nvar);
	PREG = PREG->y_u.yl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_nonvar_x, xl);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xl.x);
	deref_head(d0, nonvar_x_unk);
      nonvar_x_nvar:
	PREG = NEXTOP(PREG, xl);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, nonvar_x_unk, nonvar_x_nvar);
	PREG = PREG->y_u.xl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_nonvar_y, yl);
	BEGD(d0);
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yl.y;
	d0 = *pt0;
	deref_head(d0, nonvar_y_unk);
      nonvar_y_nvar:
	PREG = NEXTOP(PREG, yl);
	GONext();

	derefa_body(d0, pt0, nonvar_y_unk, nonvar_y_nvar);
	PREG = PREG->y_u.yl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_number_x, xl);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xl.x);
	deref_head(d0, number_x_unk);
      number_x_nvar:
	/* non variable */
	if (IsIntTerm(d0)) {
	  PREG = NEXTOP(PREG, xl);
	  GONext();
	}
	if (IsApplTerm(d0)) {
	  Functor f0 = FunctorOfTerm(d0);
	  if (IsExtensionFunctor(f0)) {
	    switch ((CELL)f0) {
	    case (CELL)FunctorBigInt:
	      { CELL *pt = RepAppl(d0);
		if (  pt[1] != BIG_RATIONAL || pt[1] != BIG_INT ) {
		  PREG = PREG->y_u.xl.F;
		  GONext();
		}
	      }
	    case (CELL)FunctorLongInt:
	    case (CELL)FunctorDouble:
	      PREG = NEXTOP(PREG, xl);
	      GONext();
	      break;
	    default:
	      PREG = PREG->y_u.xl.F;
	      GONext();
	    }
	  }
	}
	PREG = PREG->y_u.xl.F;
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, number_x_unk, number_x_nvar);
	PREG = PREG->y_u.xl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_number_y, yl);
	BEGD(d0);
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yl.y;
	d0 = *pt0;
	deref_head(d0, number_y_unk);
      number_y_nvar:
	/* non variable */
	/* non variable */
	if (IsIntTerm(d0)) {
	  PREG = NEXTOP(PREG, xl);
	  GONext();
	}
	if (IsApplTerm(d0)) {
	  Functor f0 = FunctorOfTerm(d0);
	  if (IsExtensionFunctor(f0)) {
	    switch ((CELL)f0) {
	    case (CELL)FunctorBigInt:
	      { CELL *pt = RepAppl(d0);
		if (  pt[1] != BIG_RATIONAL || pt[1] != BIG_INT ) {
		  PREG = PREG->y_u.yl.F;
		  GONext();
		}
	      }
	      break;
	    case (CELL)FunctorLongInt:
	    case (CELL)FunctorDouble:
	      PREG = NEXTOP(PREG, yl);
	      GONext();
	      break;
	    default:
	      PREG = PREG->y_u.yl.F;
	      GONext();
	    }
	  }
	}
	PREG = PREG->y_u.yl.F;
	GONext();

	derefa_body(d0, pt0, number_y_unk, number_y_nvar);
	PREG = PREG->y_u.yl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_var_x, xl);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xl.x);
	deref_head(d0, var_x_unk);
      var_x_nvar:
	/* non variable */
	PREG = PREG->y_u.xl.F;
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, var_x_unk, var_x_nvar);
	PREG = NEXTOP(PREG, xl);
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_var_y, yl);
	BEGD(d0);
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yl.y;
	d0 = *pt0;
	deref_head(d0, var_y_unk);
      var_y_nvar:
	/* non variable */
	PREG = PREG->y_u.yl.F;
	GONext();

	derefa_body(d0, pt0, var_y_unk, var_y_nvar);
	PREG = NEXTOP(PREG, yl);
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_db_ref_x, xl);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xl.x);
	deref_head(d0, dbref_x_unk);
      dbref_x_nvar:
	/* non variable */
	if (IsDBRefTerm(d0)) {
	  /* only allow references to the database, not general references
	   * to go through. */
	  PREG = NEXTOP(PREG, xl);
	  GONext();
	}
	else {
	  PREG = PREG->y_u.xl.F;
	  GONext();
	}

	BEGP(pt0);
	deref_body(d0, pt0, dbref_x_unk, dbref_x_nvar);
	PREG = PREG->y_u.xl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_db_ref_y, yl);
	BEGD(d0);
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yl.y;
	d0 = *pt0;
	deref_head(d0, dbref_y_unk);
      dbref_y_nvar:
	/* non variable */
	if (IsDBRefTerm(d0)) {
	  /* only allow references to the database, not general references
	   * to go through. */
	  PREG = NEXTOP(PREG, yl);
	  GONext();
	}
	else {
	  PREG = PREG->y_u.yl.F;
	  GONext();
	}

	derefa_body(d0, pt0, dbref_y_unk, dbref_y_nvar);
	PREG = PREG->y_u.yl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_primitive_x, xl);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xl.x);
	deref_head(d0, primi_x_unk);
      primi_x_nvar:
	/* non variable */
	if (IsPrimitiveTerm(d0)) {
	  PREG = NEXTOP(PREG, xl);
	  GONext();
	}
	else {
	  PREG = PREG->y_u.xl.F;
	  GONext();
	}

	BEGP(pt0);
	deref_body(d0, pt0, primi_x_unk, primi_x_nvar);
	PREG = PREG->y_u.xl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_primitive_y, yl);
	BEGD(d0);
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yl.y;
	d0 = *pt0;
	deref_head(d0, primi_y_unk);
      primi_y_nvar:
	/* non variable */
	if (IsPrimitiveTerm(d0)) {
	  PREG = NEXTOP(PREG, yl);
	  GONext();
	}
	else {
	  PREG = PREG->y_u.yl.F;
	  GONext();
	}

	derefa_body(d0, pt0, primi_y_unk, primi_y_nvar);
	PREG = PREG->y_u.yl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_compound_x, xl);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xl.x);
	deref_head(d0, compound_x_unk);
      compound_x_nvar:
	/* non variable */
	if (IsPairTerm(d0)) {
	  PREG = NEXTOP(PREG, xl);
	  GONext();
	}
	else if (IsApplTerm(d0)) {
	  if (IsExtensionFunctor(FunctorOfTerm(d0))) {
	    PREG = PREG->y_u.xl.F;
	    GONext();
	  }
	  PREG = NEXTOP(PREG, xl);
	  GONext();
	}
	else {
	  PREG = PREG->y_u.xl.F;
	  GONext();
	}

	BEGP(pt0);
	deref_body(d0, pt0, compound_x_unk, compound_x_nvar);
	PREG = PREG->y_u.xl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_compound_y, yl);
	BEGD(d0);
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yl.y;
	d0 = *pt0;
	deref_head(d0, compound_y_unk);
      compound_y_nvar:
	/* non variable */
	if (IsPairTerm(d0)) {
	  PREG = NEXTOP(PREG, yl);
	  GONext();
	}
	else if (IsApplTerm(d0)) {
	  if (IsExtensionFunctor(FunctorOfTerm(d0))) {
	    PREG = PREG->y_u.yl.F;
	    GONext();
	  }
	  PREG = NEXTOP(PREG, yl);
	  GONext();
	}
	else {
	  PREG = PREG->y_u.yl.F;
	  GONext();
	}

	derefa_body(d0, pt0, compound_y_unk, compound_y_nvar);
	PREG = PREG->y_u.yl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_float_x, xl);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xl.x);
	deref_head(d0, float_x_unk);
      float_x_nvar:
	/* non variable */
	if (IsFloatTerm(d0)) {
	  PREG = NEXTOP(PREG, xl);
	  GONext();
	}
	PREG = PREG->y_u.xl.F;
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, float_x_unk, float_x_nvar);
	PREG = PREG->y_u.xl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_float_y, yl);
	BEGD(d0);
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yl.y;
	d0 = *pt0;
	deref_head(d0, float_y_unk);
      float_y_nvar:
	/* non variable */
	if (IsFloatTerm(d0)) {
	  PREG = NEXTOP(PREG, yl);
	  GONext();
	}
	PREG = PREG->y_u.yl.F;
	GONext();

	derefa_body(d0, pt0, float_y_unk, float_y_nvar);
	PREG = PREG->y_u.yl.F;
	GONext();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_plus_vv, xxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.xxx.x1);
	/* first check pt1 */
	deref_head(d0, plus_vv_unk);
      plus_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, plus_vv_nvar_unk);
      plus_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) + IntOfTerm(d1));
	}
	else {
	  saveregs();
	  d0 = p_plus(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();

	}
	XREG(PREG->y_u.xxx.x) = d0;
	PREG = NEXTOP(PREG, xxx);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, plus_vv_unk, plus_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is _+B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, plus_vv_nvar_unk, plus_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A+B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_plus_vc, xxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxn.xi);
	/* first check pt1 */
	deref_head(d0, plus_vc_unk);
      plus_vc_nvar:
	{
	  Int d1 = PREG->y_u.xxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = MkIntegerTerm(IntOfTerm(d0) + d1);
	  }
	  else {
	    saveregs();
	    d0 = p_plus(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	    setregs();
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	XREG(PREG->y_u.xxn.x) = d0;
	PREG = NEXTOP(PREG, xxn);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, plus_vc_unk, plus_vc_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A + " Int_FORMAT, PREG->y_u.xxn.c);
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_plus_y_vv, yxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.yxx.x1);
	/* first check pt1 */
	deref_head(d0, plus_y_vv_unk);
      plus_y_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, plus_y_vv_nvar_unk);
      plus_y_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) + IntOfTerm(d1));
	}
	else {
	  saveregs();
	  d0 = p_plus(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_PreprocessedError()
	    setregs();
	    FAIL();
	  }
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxx.y;
	PREG = NEXTOP(PREG, yxx);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, plus_y_vv_unk, plus_y_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A+B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, plus_y_vv_nvar_unk, plus_y_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A+B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_plus_y_vc, yxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxn.xi);
	/* first check pt1 */
	deref_head(d0, plus_y_vc_unk);
      plus_y_vc_nvar:
	{
	  Int d1 = PREG->y_u.yxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = MkIntegerTerm(IntOfTerm(d0) + d1);
	  }
	  else {
	    saveregs();
	    d0 = p_plus(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	    setregs();
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(PREG, yxn);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, plus_y_vc_unk, plus_y_vc_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A + " Int_FORMAT, PREG->y_u.yxn.c);
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_minus_vv, xxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.xxx.x1);
	/* first check pt1 */
	deref_head(d0, minus_vv_unk);
      minus_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, minus_vv_nvar_unk);
      minus_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) - IntOfTerm(d1));
	}
	else {
	  saveregs();
	  d0 = p_minus(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_PreprocessedError()
	    setregs();
	    FAIL();
	  }
	}
	XREG(PREG->y_u.xxx.x) = d0;
	PREG = NEXTOP(PREG, xxx);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, minus_vv_unk, minus_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A-B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, minus_vv_nvar_unk, minus_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A-B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_minus_cv, xxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxn.xi);
	/* first check pt1 */
	deref_head(d0, minus_cv_unk);
      minus_cv_nvar:
	{
	  Int d1 = PREG->y_u.xxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = MkIntegerTerm(d1 - IntOfTerm(d0));
	  }
	  else {
	    saveregs();
	    d0 = p_minus(MkIntegerTerm(d1),Yap_Eval(d0) PASS_REGS);
	    setregs();
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	XREG(PREG->y_u.xxn.x) = d0;
	PREG = NEXTOP(PREG, xxn);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, minus_cv_unk, minus_cv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is " Int_FORMAT "-A", PREG->y_u.xxn.c);
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_minus_y_vv, yxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.yxx.x1);
	/* first check pt1 */
	deref_head(d0, minus_y_vv_unk);
      minus_y_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, minus_y_vv_nvar_unk);
      minus_y_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) - IntOfTerm(d1));
	}
	else {
	  saveregs();
	  d0 = p_minus(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_PreprocessedError()
	    setregs();
	    FAIL();
	  }
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxx.y;
	PREG = NEXTOP(PREG, yxx);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, minus_y_vv_unk, minus_y_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A-B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, minus_y_vv_nvar_unk, minus_y_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A-B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_minus_y_cv, yxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxn.xi);
	/* first check pt1 */
	deref_head(d0, minus_y_cv_unk);
      minus_y_cv_nvar:
	{
	  Int d1 = PREG->y_u.yxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = MkIntegerTerm(d1 - IntOfTerm(d0));
	  }
	  else {
	    saveregs();
	    d0 = p_minus(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
	    setregs();
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(PREG, yxn);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, minus_y_cv_unk, minus_y_cv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is " Int_FORMAT "-A", PREG->y_u.yxn.c);
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_times_vv, xxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.xxx.x1);
	/* first check pt1 */
	deref_head(d0, times_vv_unk);
      times_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, times_vv_nvar_unk);
      times_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  d0 = times_int(IntOfTerm(d0), IntOfTerm(d1) PASS_REGS);
	}
	else {
	  saveregs();
	  d0 = p_times(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_PreprocessedError()
	    setregs();
	    FAIL();
	  }
	}
	XREG(PREG->y_u.xxx.x) = d0;
	PREG = NEXTOP(PREG, xxx);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, times_vv_unk, times_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A*B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, times_vv_nvar_unk, times_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A*B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_times_vc, xxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxn.xi);
	/* first check pt1 */
	deref_head(d0, times_vc_unk);
      times_vc_nvar:
	{
	  Int d1 = PREG->y_u.xxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = times_int(IntOfTerm(d0), d1 PASS_REGS);
	  }
	  else {
	    saveregs();
	    d0 = p_times(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	    setregs();
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	XREG(PREG->y_u.xxn.x) = d0;
	PREG = NEXTOP(PREG, xxn);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, times_vc_unk, times_vc_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A* " Int_FORMAT, PREG->y_u.xxn.c);
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_times_y_vv, yxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.yxx.x1);
	/* first check pt1 */
	deref_head(d0, times_y_vv_unk);
      times_y_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, times_y_vv_nvar_unk);
      times_y_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  d0 = times_int(IntOfTerm(d0), IntOfTerm(d1) PASS_REGS);
	}
	else {
	  saveregs();
	  d0 = p_times(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_PreprocessedError()
	    setregs();
	    FAIL();
	  }
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxx.y;
	PREG = NEXTOP(PREG, yxx);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, times_y_vv_unk, times_y_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A*B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, times_y_vv_nvar_unk, times_y_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A*B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_times_y_vc, yxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxn.xi);
	/* first check pt1 */
	deref_head(d0, times_y_vc_unk);
      times_y_vc_nvar:
	{
	  Int d1 = PREG->y_u.yxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = times_int(IntOfTerm(d0), d1 PASS_REGS);
	  }
	  else {
	    saveregs();
	    d0 = p_times(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	    setregs();
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(PREG, yxn);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, times_y_vc_unk, times_y_vc_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A* " Int_FORMAT, PREG->y_u.yxn.c);
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_div_vv, xxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.xxx.x1);
	/* first check pt1 */
	deref_head(d0, div_vv_unk);
      div_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, div_vv_nvar_unk);
      div_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  Int div = IntOfTerm(d1);
	  if (div == 0) {
	    saveregs();
	    Yap_NilError(EVALUATION_ERROR_ZERO_DIVISOR,"// /2");
	    setregs();
	    FAIL();
	  }
	  d0 = MkIntTerm(IntOfTerm(d0) / div);
	}
	else {
	  saveregs();
	  d0 = p_div(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_PreprocessedError()
	    setregs();
	    FAIL();
	  }
	}
	XREG(PREG->y_u.xxx.x) = d0;
	PREG = NEXTOP(PREG, xxx);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, div_vv_unk, div_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A//B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, div_vv_nvar_unk, div_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A//B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_div_vc, xxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxn.xi);
	/* first check pt1 */
	deref_head(d0, div_vc_unk);
      div_vc_nvar:
	{
	  Int d1 = PREG->y_u.xxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = MkIntTerm(IntOfTerm(d0) / d1);
	  }
	  else {
	    saveregs();
	    d0 = p_div(Yap_Eval(d0),MkIntegerTerm(d1) PASS_REGS);
	    setregs();
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	XREG(PREG->y_u.xxn.x) = d0;
	PREG = NEXTOP(PREG, xxn);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, div_vc_unk, div_vc_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A//B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_div_cv, xxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxn.xi);
	/* first check pt1 */
	deref_head(d0, div_cv_unk);
      div_cv_nvar:
	{
	  Int d1 = PREG->y_u.xxn.c;
	  if (IsIntTerm(d0)) {
	    Int div = IntOfTerm(d0);
	    if (div == 0){
	      saveregs();
	      Yap_NilError(EVALUATION_ERROR_ZERO_DIVISOR,"// /2");
	      setregs();
	      FAIL();
	    }
	    d0 = MkIntegerTerm(d1 / div);
	  }
	  else {
	    saveregs();
	    d0 = p_div(MkIntegerTerm(d1),Yap_Eval(d0) PASS_REGS);
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	XREG(PREG->y_u.xxn.x) = d0;
	PREG = NEXTOP(PREG, xxn);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, div_cv_unk, div_cv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is " Int_FORMAT "// A", PREG->y_u.xxn.c);
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_div_y_vv, yxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.yxx.x1);
	/* first check pt1 */
	deref_head(d0, div_y_vv_unk);
      div_y_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, div_y_vv_nvar_unk);
      div_y_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  Int div = IntOfTerm(d1);
	  if (div == 0) {
	    saveregs();
	    Yap_NilError(EVALUATION_ERROR_ZERO_DIVISOR,"// /2");
	    setregs();
	    FAIL();
	  }
	  d0 = MkIntTerm(IntOfTerm(d0) / div);
	}
	else {
	  saveregs();
	  d0 = p_div(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_PreprocessedError()
	    setregs();
	    FAIL();
	  }
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxx.y;
	PREG = NEXTOP(PREG, yxx);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, div_y_vv_unk, div_y_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A//B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, div_y_vv_nvar_unk, div_y_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A//B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_div_y_vc, yxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxn.xi);
	/* first check pt1 */
	deref_head(d0, div_y_vc_unk);
      div_y_vc_nvar:
	{
	  Int d1 = PREG->y_u.yxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = MkIntTerm(IntOfTerm(d0)/d1);
	  }
	  else {
	    saveregs();
	    d0 = p_div(Yap_Eval(d0),MkIntegerTerm(d1) PASS_REGS);
	    setregs();
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(PREG, yxn);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, div_y_vc_unk, div_y_vc_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A//B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_div_y_cv, yxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxn.xi);
	/* first check pt1 */
	deref_head(d0, div_y_cv_unk);
      div_y_cv_nvar:
	{
	  Int d1 = PREG->y_u.yxn.c;
	  if (IsIntTerm(d0)) {
	    Int div = IntOfTerm(d0);
	    if (div == 0) {
	      saveregs();
	      Yap_NilError(EVALUATION_ERROR_ZERO_DIVISOR,"// /2");
	      setregs();
	      FAIL();
	    }
	    d0 = MkIntegerTerm(d1 / div);
	  }
	  else {
	    saveregs();
	    d0 = p_div(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
	    setregs();
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(PREG, yxn);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, div_y_cv_unk, div_y_cv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is " Int_FORMAT "// A", PREG->y_u.yxn.c);
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();


	Op(p_and_vv, xxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.xxx.x1);
	/* first check pt1 */
	deref_head(d0, and_vv_unk);
      and_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, and_vv_nvar_unk);
      and_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) & IntOfTerm(d1));
	}
	else {
	  saveregs();
	  d0 = p_and(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_PreprocessedError()
	    setregs();
	    FAIL();
	  }
	}
	XREG(PREG->y_u.xxx.x) = d0;
	PREG = NEXTOP(PREG, xxx);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, and_vv_unk, and_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A/\\B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, and_vv_nvar_unk, and_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A/\\B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_and_vc, xxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxn.xi);
	/* first check pt1 */
	deref_head(d0, and_vc_unk);
      and_vc_nvar:
	{
	  Int d1 = PREG->y_u.xxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = MkIntegerTerm(IntOfTerm(d0) & d1);
	  }
	  else {
	    saveregs();
	    d0 = p_and(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	    setregs();
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	XREG(PREG->y_u.xxn.x) = d0;
	PREG = NEXTOP(PREG, xxn);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, and_vc_unk, and_vc_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A /\\ " Int_FORMAT , PREG->y_u.xxn.c);
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_and_y_vv, yxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.yxx.x1);
	/* first check pt1 */
	deref_head(d0, and_y_vv_unk);
      and_y_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, and_y_vv_nvar_unk);
      and_y_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) & IntOfTerm(d1));
	}
	else {
	  saveregs();
	  d0 = p_and(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_PreprocessedError()
	    setregs();
	    FAIL();
	  }
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxx.y;
	PREG = NEXTOP(PREG, yxx);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, and_y_vv_unk, and_y_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A/\\B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, and_y_vv_nvar_unk, and_y_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A/\\B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_and_y_vc, yxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxn.xi);
	/* first check pt1 */
	deref_head(d0, and_y_vc_unk);
      and_y_vc_nvar:
	{
	  Int d1 = PREG->y_u.yxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = MkIntegerTerm(IntOfTerm(d0) & d1);
	  }
	  else {
	    saveregs();
	    d0 = p_and(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	    setregs();
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(PREG, yxn);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, and_y_vc_unk, and_y_vc_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A /\\ " Int_FORMAT , PREG->y_u.yxn.c);
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();


	Op(p_or_vv, xxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.xxx.x1);
	/* first check pt1 */
	deref_head(d0, or_vv_unk);
      or_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, or_vv_nvar_unk);
      or_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) | IntOfTerm(d1));
	}
	else {
	  saveregs();
	  d0 = p_or(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_PreprocessedError()
	    setregs();
	    FAIL();
	  }
	}
	XREG(PREG->y_u.xxx.x) = d0;
	PREG = NEXTOP(PREG, xxx);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, or_vv_unk, or_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A\\/B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, or_vv_nvar_unk, or_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A\\/B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_or_vc, xxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxn.xi);
	/* first check pt1 */
	deref_head(d0, or_vc_unk);
      or_vc_nvar:
	{
	  Int d1 = PREG->y_u.xxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = MkIntegerTerm(IntOfTerm(d0) | d1);
	  }
	  else {
	    saveregs();
	    d0 = p_or(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	XREG(PREG->y_u.xxn.x) = d0;
	PREG = NEXTOP(PREG, xxn);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, or_vc_unk, or_vc_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A \\/ " Int_FORMAT , PREG->y_u.xxn.c);
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_or_y_vv, yxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.yxx.x1);
	/* first check pt1 */
	deref_head(d0, or_y_vv_unk);
      or_y_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, or_y_vv_nvar_unk);
      or_y_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) | IntOfTerm(d1));
	}
	else {
	  saveregs();
	  d0 = p_or(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_PreprocessedError()
	    setregs();
	    FAIL();
	  }
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxx.y;
	PREG = NEXTOP(PREG, yxx);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, or_y_vv_unk, or_y_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A\\/B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, or_y_vv_nvar_unk, or_y_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A\\/B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_or_y_vc, yxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxn.xi);
	/* first check pt1 */
	deref_head(d0, or_y_vc_unk);
      or_y_vc_nvar:
	{
	  Int d1 = PREG->y_u.yxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = MkIntegerTerm(IntOfTerm(d0) | d1);
	  }
	  else {
	    saveregs();
	    d0 = p_or(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	    setregs();
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(PREG, yxn);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, or_y_vc_unk, or_y_vc_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A \\/ " Int_FORMAT , PREG->y_u.yxn.c);
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_sll_vv, xxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.xxx.x1);
	/* first check pt1 */
	deref_head(d0, sll_vv_unk);
      sll_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, sll_vv_nvar_unk);
      sll_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  Int i2 = IntOfTerm(d1);
	  if (i2 < 0)
	    d0 = MkIntegerTerm(SLR(IntOfTerm(d0), -i2));
	  else
	    d0 = do_sll(IntOfTerm(d0),i2 PASS_REGS);
	}
	else {
	  saveregs();
	  d0 = p_sll(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	}
	if (d0 == 0L) {
	  saveregs();
	  Yap_PreprocessedError()
	  setregs();
	  FAIL();
	}
	XREG(PREG->y_u.xxx.x) = d0;
	PREG = NEXTOP(PREG, xxx);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, sll_vv_unk, sll_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, sll_vv_nvar_unk, sll_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_sll_vc, xxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxn.xi);
	/* first check pt1 */
	deref_head(d0, sll_vc_unk);
      sll_vc_nvar:
	{
	  Int d1 = PREG->y_u.xxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = do_sll(IntOfTerm(d0), (Int)d1 PASS_REGS);
	  }
	  else {
	    saveregs();
	    d0 = p_sll(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	    setregs();
	  }
	}
	if (d0 == 0L) {
	  saveregs();
	  Yap_PreprocessedError()
	  setregs();
	  FAIL();
	}
	XREG(PREG->y_u.xxn.x) = d0;
	PREG = NEXTOP(PREG, xxn);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, sll_vc_unk, sll_vc_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_sll_cv, xxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxn.xi);
	/* first check pt1 */
	deref_head(d0, sll_cv_unk);
      sll_cv_nvar:
	{
	  Int d1 = PREG->y_u.xxn.c;
	  if (IsIntTerm(d0)) {
	    Int i2 = IntOfTerm(d0);
	    if (i2 < 0)
	      d0 = MkIntegerTerm(SLR(d1, -i2));
	    else
	      d0 = do_sll(d1,i2 PASS_REGS);
	  }
	  else {
	    saveregs();
	    d0 = p_sll(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
	    setregs();
	  }
	}
	if (d0 == 0L) {
	  saveregs();
	  Yap_PreprocessedError()
	  setregs();
	  FAIL();
	}
	XREG(PREG->y_u.xxn.x) = d0;
	PREG = NEXTOP(PREG, xxn);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, sll_cv_unk, sll_cv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_sll_y_vv, yxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.yxx.x1);
	/* first check pt1 */
	deref_head(d0, sll_y_vv_unk);
      sll_y_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, sll_y_vv_nvar_unk);
      sll_y_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  Int i2 = IntOfTerm(d1);
	  if (i2 < 0)
	    d0 = MkIntegerTerm(SLR(IntOfTerm(d0), -i2));
	  else
	    d0 = do_sll(IntOfTerm(d0),i2 PASS_REGS);
	}
	else {
	  saveregs();
	  d0 = p_sll(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	}
	if (d0 == 0L) {
	  saveregs();
	  Yap_PreprocessedError()
	  setregs();
	  FAIL();
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxx.y;
	PREG = NEXTOP(PREG, yxx);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, sll_y_vv_unk, sll_y_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, sll_y_vv_nvar_unk, sll_y_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_sll_y_vc, yxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxn.xi);
	/* first check pt1 */
	deref_head(d0, sll_y_vc_unk);
      sll_y_vc_nvar:
	{
	  Int d1 = PREG->y_u.yxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = do_sll(IntOfTerm(d0), Yap_Eval(d1) PASS_REGS);
	  }
	  else {
	    saveregs();
	    d0 = p_sll(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	    setregs();
	  }
	}
	if (d0 == 0L) {
	  saveregs();
	  Yap_PreprocessedError()
	  setregs();
	  FAIL();
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(PREG, yxn);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, sll_y_vc_unk, sll_y_vc_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();


	Op(p_sll_y_cv, yxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxn.xi);
	/* first check pt1 */
	deref_head(d0, sll_y_cv_unk);
      sll_y_cv_nvar:
	{
	  Int d1 = PREG->y_u.yxn.c;
	  if (IsIntTerm(d0)) {
	    Int i2 = IntOfTerm(d0);
	    if (i2 < 0)
	      d0 = MkIntegerTerm(SLR(d1, -i2));
	    else
	      d0 = do_sll(d1,i2 PASS_REGS);
	  }
	  else {
	    saveregs();
	    d0 = p_sll(MkIntegerTerm(d1), Yap_Eval(0) PASS_REGS);
	    setregs();
	  }
	}
	if (d0 == 0L) {
	  saveregs();
	  Yap_PreprocessedError()
	  setregs();
	  FAIL();
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(PREG, yxn);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, sll_y_cv_unk, sll_y_cv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_slr_vv, xxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.xxx.x1);
	/* first check pt1 */
	deref_head(d0, slr_vv_unk);
      slr_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, slr_vv_nvar_unk);
      slr_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  Int i2 = IntOfTerm(d1);
	  if (i2 < 0)
	    d0 = do_sll(IntOfTerm(d0), -i2 PASS_REGS);
	  else
	    d0 = MkIntTerm(SLR(IntOfTerm(d0), i2));
	}
	else {
	  saveregs();
	  d0 = p_slr(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	}
	if (d0 == 0L) {
	  saveregs();
	  Yap_PreprocessedError()
	  setregs();
	  FAIL();
	}
	XREG(PREG->y_u.xxx.x) = d0;
	PREG = NEXTOP(PREG, xxx);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, slr_vv_unk, slr_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, slr_vv_nvar_unk, slr_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_slr_vc, xxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxn.xi);
	/* first check pt1 */
	deref_head(d0, slr_vc_unk);
      slr_vc_nvar:
	{
	  Int d1 = PREG->y_u.xxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = MkIntTerm(SLR(IntOfTerm(d0), d1));
	  }
	  else {
	    saveregs();
	    d0 = p_slr(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	    setregs();
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	XREG(PREG->y_u.xxn.x) = d0;
	PREG = NEXTOP(PREG, xxn);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, slr_vc_unk, slr_vc_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_slr_cv, xxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxn.xi);
	/* first check pt1 */
	deref_head(d0, slr_cv_unk);
      slr_cv_nvar:
	{
	  Int d1 = PREG->y_u.xxn.c;
	  if (IsIntTerm(d0)) {
	    Int i2 = IntOfTerm(d0);
	    if (i2 < 0)
	      d0 = do_sll(d1, -i2 PASS_REGS);
	    else
	      d0 = MkIntegerTerm(SLR(d1, i2));
	  }
	  else {
	    saveregs();
	    d0 = p_slr(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
	    setregs();
	  }
	}
	if (d0 == 0L) {
	  saveregs();
	  Yap_PreprocessedError()
	  setregs();
	  FAIL();
	}
	XREG(PREG->y_u.xxn.x) = d0;
	PREG = NEXTOP(PREG, xxn);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, slr_cv_unk, slr_cv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_slr_y_vv, yxx);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.yxx.x1);
	/* first check pt1 */
	deref_head(d0, slr_y_vv_unk);
      slr_y_vv_nvar:
	d1 = XREG(PREG->y_u.xxx.x2);
	/* next check A2 */
	deref_head(d1, slr_y_vv_nvar_unk);
      slr_y_vv_nvar_nvar:
	/* d0 and d1 are where I want them */
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  Int i2 = IntOfTerm(d1);
	  if (i2 < 0)
	    d0 = do_sll(IntOfTerm(d0), -i2 PASS_REGS);
	  else
	    d0 = MkIntTerm(SLR(IntOfTerm(d0), i2));
	}
	else {
	  saveregs();
	  d0 = p_slr(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	  setregs();
	}
	BEGP(pt0);
	if (d0 == 0L) {
	  saveregs();
	  Yap_PreprocessedError()
	  setregs();
	  FAIL();
	}
	pt0 = YREG + PREG->y_u.yxx.y;
	PREG = NEXTOP(PREG, yxx);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, slr_y_vv_unk, slr_y_vv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
	setregs();
	FAIL();
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, slr_y_vv_nvar_unk, slr_y_vv_nvar_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_slr_y_vc, yxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxn.xi);
	/* first check pt1 */
	deref_head(d0, slr_y_vc_unk);
      slr_y_vc_nvar:
	{
	  Int d1 = PREG->y_u.yxn.c;
	  if (IsIntTerm(d0)) {
	    d0 = MkIntTerm(SLR(IntOfTerm(d0), d1));
	  }
	  else {
	    saveregs();
	    d0 = p_slr(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	    setregs();
	    if (d0 == 0L) {
	      saveregs();
	      Yap_PreprocessedError()
	      setregs();
	      FAIL();
	    }
	  }
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(PREG, yxn);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, slr_y_vc_unk, slr_y_vc_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
	setregs();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(p_slr_y_cv, yxn);
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxn.xi);
	/* first check pt1 */
	deref_head(d0, slr_y_cv_unk);
      slr_y_cv_nvar:
	{
	  Int d1 = PREG->y_u.yxn.c;
	  if (IsIntTerm(d0)) {
	    Int i2 = IntOfTerm(d0);
	    if (i2 < 0)
	      d0 = do_sll(d1, -i2 PASS_REGS);
	    else
	      d0 = MkIntegerTerm(SLR(d1, i2));
	  }
	  else {
	    saveregs();
	    d0 = p_slr(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
	    setregs();
	  }
	}
	if (d0 == 0L) {
	  saveregs();
	  Yap_PreprocessedError()
	  setregs();
	  FAIL();
	}
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(PREG, yxn);
	INITIALIZE_PERMVAR(pt0,d0);
	ENDP(pt0);
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, slr_y_cv_unk, slr_y_cv_nvar);
	saveregs();
	Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
	setregs();
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	BOp(call_bfunc_xx, plxxs);
	BEGD(d0);
	BEGD(d1);
	d0 = XREG(PREG->y_u.plxxs.x1);
      call_bfunc_xx_nvar:
	d1 = XREG(PREG->y_u.plxxs.x2);
      call_bfunc_xx2_nvar:
	deref_head(d0, call_bfunc_xx_unk);
	deref_head(d1, call_bfunc_xx2_unk);
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  COUNT flags;

	  Int v = IntOfTerm(d0) - IntOfTerm(d1);
	  flags = PREG->y_u.plxxs.flags;
	  if (v > 0) {
	    if (flags & GT_OK_IN_CMP) {
	      yamop *nextp = NEXTOP(PREG, plxxs);
	      ALWAYS_LOOKAHEAD(nextp->opc);
	      PREG = nextp;
	      ALWAYS_GONext();
	      ALWAYS_END_PREFETCH();
	    } else {
	      yamop *nextp = PREG->y_u.plxxs.f;
	      ALWAYS_LOOKAHEAD(nextp->opc);
	      PREG = nextp;
	      ALWAYS_GONext();
	      ALWAYS_END_PREFETCH();
	    }
	  } else if (v < 0) {
	    if (flags & LT_OK_IN_CMP) {
	      yamop *nextp = NEXTOP(PREG, plxxs);
	      ALWAYS_LOOKAHEAD(nextp->opc);
	      PREG = nextp;
	      ALWAYS_GONext();
	      ALWAYS_END_PREFETCH();
	    } else {
	      yamop *nextp = PREG->y_u.plxxs.f;
	      ALWAYS_LOOKAHEAD(nextp->opc);
	      PREG = nextp;
	      ALWAYS_GONext();
	      ALWAYS_END_PREFETCH();
	    }
	  } else /* if (v == 0) */ {
	    if (flags & EQ_OK_IN_CMP) {
	      yamop *nextp = NEXTOP(PREG, plxxs);
	      ALWAYS_LOOKAHEAD(nextp->opc);
	      PREG = nextp;
	      ALWAYS_GONext();
	      ALWAYS_END_PREFETCH();
	    } else {
	      yamop *nextp = PREG->y_u.plxxs.f;
	      ALWAYS_LOOKAHEAD(nextp->opc);
	      PREG = nextp;
	      ALWAYS_GONext();
	      ALWAYS_END_PREFETCH();
	    }
	  }
	}
      exec_bin_cmp_xx:
	{
	  CmpPredicate f = PREG->y_u.plxxs.p->cs.d_code;
	  saveregs();
	  d0 = (CELL) (f) (d0,d1);
	  setregs();
	}
	if (PREG == FAILCODE) {
	  JMPNext();
	}
	if (!d0) {
	  PREG = PREG->y_u.plxxs.f;
	  JMPNext();
	}
	PREG = NEXTOP(PREG, plxxs);
	JMPNext();

	BEGP(pt0);
	deref_body(d0, pt0, call_bfunc_xx_unk, call_bfunc_xx_nvar);
	d1 = Deref(d1);
	goto exec_bin_cmp_xx;
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, call_bfunc_xx2_unk, call_bfunc_xx2_nvar);
	goto exec_bin_cmp_xx;
	ENDP(pt0);

	ENDD(d1);
	ENDD(d0);
	ENDBOp();

	BOp(call_bfunc_yx, plxys);
	BEGD(d0);
	BEGD(d1);
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.plxys.y;
	d1 = XREG(PREG->y_u.plxys.x);
	d0 = *pt0;
	ENDP(pt0);
	deref_head(d0, call_bfunc_yx_unk);
      call_bfunc_yx_nvar:
	deref_head(d1, call_bfunc_yx2_unk);
      call_bfunc_yx2_nvar:
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  int flags;

	  Int v = IntOfTerm(d0) - IntOfTerm(d1);
	  flags = PREG->y_u.plxys.flags;
	  if (v > 0) {
	    if (flags & GT_OK_IN_CMP) {
	      PREG = NEXTOP(PREG, plxys);
	      JMPNext();
	    } else {
	      PREG = PREG->y_u.plxys.f;
	      JMPNext();
	    }
	  } else if (v < 0) {
	    if (flags & LT_OK_IN_CMP) {
	      PREG = NEXTOP(PREG, plxys);
	      JMPNext();
	    } else {
	      PREG = PREG->y_u.plxys.f;
	      JMPNext();
	    }
	  } else /* if (v == 0) */ {
	    if (flags & EQ_OK_IN_CMP) {
	      PREG = NEXTOP(PREG, plxys);
	      JMPNext();
	    } else {
	      PREG = PREG->y_u.plxys.f;
	      JMPNext();
	    }
	  }
	}
      exec_bin_cmp_yx:
	{
	  CmpPredicate f = PREG->y_u.plxys.p->cs.d_code;
	  saveregs();
	  d0 = (CELL) (f) (d0,d1);
	  setregs();
	}
	if (!d0 || PREG == FAILCODE) {
	  if (PREG != FAILCODE)
	    PREG = PREG->y_u.plxys.f;
	  JMPNext();
	}
	PREG = NEXTOP(PREG, plxys);
	JMPNext();

	BEGP(pt0);
	deref_body(d0, pt0, call_bfunc_yx_unk, call_bfunc_yx_nvar);
	d1 = Deref(d1);
	goto exec_bin_cmp_yx;
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, call_bfunc_yx2_unk, call_bfunc_yx2_nvar);
	goto exec_bin_cmp_yx;
	ENDP(pt0);

	ENDD(d1);
	ENDD(d0);
	ENDBOp();

	BOp(call_bfunc_xy, plxys);
	BEGD(d0);
	BEGD(d1);
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.plxys.y;
	d0 = XREG(PREG->y_u.plxys.x);
	d1 = *pt0;
	ENDP(pt0);
	deref_head(d0, call_bfunc_xy_unk);
      call_bfunc_xy_nvar:
	deref_head(d1, call_bfunc_xy2_unk);
      call_bfunc_xy2_nvar:
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  int flags;

	  Int v = IntOfTerm(d0) - IntOfTerm(d1);
	  flags = PREG->y_u.plxys.flags;
	  if (v > 0) {
	    if (flags & GT_OK_IN_CMP) {
	      PREG = NEXTOP(PREG, plxys);
	      JMPNext();
	    } else {
	      PREG = PREG->y_u.plxys.f;
	      JMPNext();
	    }
	  } else if (v < 0) {
	    if (flags & LT_OK_IN_CMP) {
	      PREG = NEXTOP(PREG, plxys);
	      JMPNext();
	    } else {
	      PREG = PREG->y_u.plxys.f;
	      JMPNext();
	    }
	  } else /* if (v == 0) */ {
	    if (flags & EQ_OK_IN_CMP) {
	      PREG = NEXTOP(PREG, plxys);
	      JMPNext();
	    } else {
	      PREG = PREG->y_u.plxys.f;
	      JMPNext();
	    }
	  }
	}
      exec_bin_cmp_xy:
	{
	  CmpPredicate f = PREG->y_u.plxys.p->cs.d_code;
	  saveregs();
	  d0 = (CELL) (f) (d0,d1);
	  setregs();
	}
	if (!d0 || PREG == FAILCODE) {
	  if (PREG != FAILCODE)
	    PREG = PREG->y_u.plxys.f;
	  JMPNext();
	}
	PREG = NEXTOP(PREG, plxys);
	JMPNext();

	BEGP(pt0);
	deref_body(d0, pt0, call_bfunc_xy_unk, call_bfunc_xy_nvar);
	d1 = Deref(d1);
	goto exec_bin_cmp_xy;
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, call_bfunc_xy2_unk, call_bfunc_xy2_nvar);
	goto exec_bin_cmp_xy;
	ENDP(pt0);

	ENDD(d1);
	ENDD(d0);
	ENDBOp();

	BOp(call_bfunc_yy, plyys);
	BEGD(d0);
	BEGD(d1);
	BEGP(pt0);
	pt0 = YREG + PREG->y_u.plyys.y1;
	BEGP(pt1);
	pt1 = YREG + PREG->y_u.plyys.y2;
	d0 = *pt0;
	d1 = *pt1;
	ENDP(pt1);
	ENDP(pt0);
	deref_head(d0, call_bfunc_yy_unk);
      call_bfunc_yy_nvar:
	deref_head(d1, call_bfunc_yy2_unk);
      call_bfunc_yy2_nvar:
	if (IsIntTerm(d0) && IsIntTerm(d1)) {
	  int flags;

	  Int v = IntOfTerm(d0) - IntOfTerm(d1);
	  flags = PREG->y_u.plyys.flags;
	  if (v > 0) {
	    if (flags & GT_OK_IN_CMP) {
	      PREG = NEXTOP(PREG, plyys);
	      JMPNext();
	    } else {
	      PREG = PREG->y_u.plyys.f;
	      JMPNext();
	    }
	  } else if (v < 0) {
	    if (flags & LT_OK_IN_CMP) {
	      PREG = NEXTOP(PREG, plyys);
	      JMPNext();
	    } else {
	      PREG = PREG->y_u.plyys.f;
	      JMPNext();
	    }
	  } else /* if (v == 0) */ {
	    if (flags & EQ_OK_IN_CMP) {
	      PREG = NEXTOP(PREG, plyys);
	      JMPNext();
	    } else {
	      PREG = PREG->y_u.plyys.f;
	      JMPNext();
	    }
	  }
	}
      exec_bin_cmp_yy:
	{
	  CmpPredicate f = PREG->y_u.plyys.p->cs.d_code;
	  saveregs();
	  d0 = (CELL) (f) (d0,d1);
	  setregs();
	}
	if (!d0 || PREG == FAILCODE) {
	  if (PREG != FAILCODE)
	    PREG = PREG->y_u.plyys.f;
	  JMPNext();
	}
	PREG = NEXTOP(PREG, plyys);
	JMPNext();

	BEGP(pt0);
	deref_body(d0, pt0, call_bfunc_yy_unk, call_bfunc_yy_nvar);
	d1 = Deref(d1);
	goto exec_bin_cmp_yy;
	ENDP(pt0);

	BEGP(pt0);
	deref_body(d1, pt0, call_bfunc_yy2_unk, call_bfunc_yy2_nvar);
	goto exec_bin_cmp_yy;
	ENDP(pt0);

	ENDD(d1);
	ENDD(d0);
	ENDBOp();

	Op(p_equal, e);
	save_hb();
	if (Yap_IUnify(ARG1, ARG2) == false) {
	  FAIL();
	}
	PREG = NEXTOP(PREG, e);
	GONext();
	ENDOp();

	Op(p_dif, l);
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace)
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1);
#endif  /* LOW_LEVEL_TRACE */
	BEGD(d0);
	BEGD(d1);
	d0 = ARG1;
	deref_head(d0, dif_unk1);
      dif_nvar1:
	/* first argument is bound */
	d1 = ARG2;
	deref_head(d1, dif_nvar1_unk2);
      dif_nvar1_nvar2:
	/* both arguments are bound */
	if (d0 == d1) {
	  PREG = PREG->y_u.l.l;
	  GONext();
	}
	if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) {
	  PREG = NEXTOP(PREG, l);
	  GONext();
	}
	{
	  Int opresult;
#ifdef COROUTINING
	  /*
	   * We may wake up goals during our attempt to unify the
	   * two terms. If we are adding to the tail of a list of
	   * woken goals that should be ok, but otherwise we need
	   * to restore LOCAL_WokenGoals to its previous value.
	   */
	  CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals);

#endif
	  /* We will have to look inside compound terms */
	  register tr_fr_ptr pt0;
	  /* store the old value of TR for clearing bindings */
	  pt0 = TR;
	  BEGCHO(pt1);
	  pt1 = B;
	  /* make B and HB point to H to guarantee all bindings will
	   * be trailed
	   */
	  HBREG = HR;
	  B = (choiceptr) HR;
	  B->cp_h = HR;
	  SET_BB(B);
	  save_hb();
	  opresult = Yap_IUnify(d0, d1);
#ifdef COROUTINING
	  /* now restore Woken Goals to its old value */
	  Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals);
	  if (OldWokenGoals == TermNil) {
	    Yap_get_signal(YAP_WAKEUP_SIGNAL);
	  }
#endif
	  /* restore B */
	  B = pt1;
	  SET_BB(PROTECT_FROZEN_B(pt1));
#ifdef COROUTINING
	  HR = HBREG;
#endif
	  HBREG = B->cp_h;
	  /* untrail all bindings made by Yap_IUnify */
	  while (TR != pt0) {
	    BEGD(d1);
	    d1 = TrailTerm(--TR);
	    if (IsVarTerm(d1)) {
#if defined(YAPOR_SBA) && defined(YAPOR)
	      /* clean up the trail when we backtrack */
	      if (Unsigned((Int)(d1)-(Int)(H_FZ)) >
		  Unsigned((Int)(B_FZ)-(Int)(H_FZ))) {
		RESET_VARIABLE(STACK_TO_SBA(d1));
	      } else
#endif
		/* normal variable */
		RESET_VARIABLE(d1);
#ifdef MULTI_ASSIGNMENT_VARIABLES
	    } else /* if (IsApplTerm(d1)) */ {
	      CELL *pt = RepAppl(d1);
	      /* AbsAppl means */
	      /* multi-assignment variable */
	      /* so the next cell is the old value */
#ifdef FROZEN_STACKS
	      pt[0] = TrailVal(--TR);
#else
	      pt[0] = TrailTerm(--TR);
	      TR--;
#endif /* FROZEN_STACKS */
#endif /* MULTI_ASSIGNMENT_VARIABLES */
	    }
	    ENDD(d1);
	  }
	  if (opresult) {
	    /* restore B, no need to restore HB */
	    PREG = PREG->y_u.l.l;
	    GONext();
	  }
	  /* restore B, and later HB */
	  PREG = NEXTOP(PREG, l);
	  ENDCHO(pt1);
	}
	GONext();

	BEGP(pt0);
	deref_body(d0, pt0, dif_unk1, dif_nvar1);
	ENDP(pt0);
	/* first argument is unbound */
	PREG = PREG->y_u.l.l;
	GONext();

	BEGP(pt0);
	deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2);
	ENDP(pt0);
	/* second argument is unbound */
	PREG = PREG->y_u.l.l;
	GONext();
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_eq, l);
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace)
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorSame,0)),XREGS+1);
#endif  /* LOW_LEVEL_TRACE */
	BEGD(d0);
	BEGD(d1);
	d0 = ARG1;
	deref_head(d0, p_eq_unk1);
      p_eq_nvar1:
	/* first argument is bound */
	d1 = ARG2;
	deref_head(d1, p_eq_nvar1_unk2);
      p_eq_nvar1_nvar2:
	/* both arguments are bound */
	if (d0 == d1) {
	  PREG = NEXTOP(PREG, l);
	  GONext();
	}
	if (IsPairTerm(d0)) {
	  if (!IsPairTerm(d1)) {
	    PREG = PREG->y_u.l.l;
	    GONext();
	  }
	  BEGD(d2);
	  always_save_pc();
	  d2 = iequ_complex(RepPair(d0)-1, RepPair(d0)+1,RepPair(d1)-1);
	  if (d2 == false) {
	    PREG = PREG->y_u.l.l;
	    GONext();
	  }
	  ENDD(d2);
	  always_set_pc();
	  PREG = NEXTOP(PREG, l);
	  GONext();
	}
	if (IsApplTerm(d0)) {
	  Functor f0 = FunctorOfTerm(d0);
	  Functor f1;

	  /* f1 must be a compound term, even if it is a suspension */
	  if (!IsApplTerm(d1)) {
	    PREG = PREG->y_u.l.l;
	    GONext();
	  }
	  f1 = FunctorOfTerm(d1);

	  /* we now know f1 is true */
	  /* deref if a compound term */
	  if (IsExtensionFunctor(f0)) {
	    switch ((CELL)f0) {
	    case (CELL)FunctorDBRef:
	      if (d0 == d1) {
		PREG = NEXTOP(PREG, l);
		GONext();
	      }
	      PREG = PREG->y_u.l.l;
	      GONext();
	    case (CELL)FunctorLongInt:
	      if (f1 != FunctorLongInt) {
		PREG = PREG->y_u.l.l;
		GONext();
	      }
	      if (LongIntOfTerm(d0) == LongIntOfTerm(d1)) {
		PREG = NEXTOP(PREG, l);
		GONext();
	      }
	      PREG = PREG->y_u.l.l;
	      GONext();
	    case (CELL)FunctorString:
	      if (f1 != FunctorString) {
		PREG = PREG->y_u.l.l;
		GONext();
	      }
	      if (strcmp((char *)(RepAppl(d0)+2),(char *)(RepAppl(d1)+2)) == 0) {
		PREG = NEXTOP(PREG, l);
		GONext();
	      }
	      PREG = PREG->y_u.l.l;
	      GONext();
	      break;
#ifdef USE_GMP
	    case (CELL)FunctorBigInt:
	      if (f1 != FunctorBigInt) {
		PREG = PREG->y_u.l.l;
		GONext();
	      }
	      if (Yap_gmp_tcmp_big_big(d0,d1) == 0) {
		PREG = NEXTOP(PREG, l);
		GONext();
	      }
	      PREG = PREG->y_u.l.l;
	      GONext();
#endif
	    case (CELL)FunctorDouble:
	      if (f1 != FunctorDouble) {
		PREG = PREG->y_u.l.l;
		GONext();
	      }
	      if (FloatOfTerm(d0) == FloatOfTerm(d1)) {
		PREG = NEXTOP(PREG, l);
		GONext();
	      }
	      PREG = PREG->y_u.l.l;
	      GONext();
	      break;
	    default:
	      PREG = PREG->y_u.l.l;
	      GONext();
	    }
	  }
	  if (f0 != f1) {
	    PREG = PREG->y_u.l.l;
	    GONext();
	  }
	  always_save_pc();
	  BEGD(d2);
	  d2 = iequ_complex(RepAppl(d0), RepAppl(d0)+ArityOfFunctor(f0), RepAppl(d1));
	  if (d2 == false) {
	    PREG = PREG->y_u.l.l;
	    GONext();
	  }
	  ENDD(d2);
	  always_set_pc();
	  PREG = NEXTOP(PREG, l);
	  GONext();
	}
	PREG = PREG->y_u.l.l;
	GONext();

	BEGP(pt0);
	deref_body(d1, pt0, p_eq_nvar1_unk2, p_eq_nvar1_nvar2);
	ENDP(pt0);
	/* first argument is bound */
	/* second argument is unbound */
	/* I don't need to worry about co-routining because an
	   unbound variable may never be == to a constrained variable!! */
	PREG = PREG->y_u.l.l;
	GONext();
	ENDD(d1);

	BEGP(pt0);
	deref_body(d0, pt0, p_eq_unk1, p_eq_nvar1);
	BEGD(d1);
	d1 = ARG2;
	deref_head(d1, p_eq_var1_unk2);
      p_eq_var1_nvar2:
	/* I don't need to worry about co-routining because an
	   unbound variable may never be == to a constrained variable!! */
	PREG = PREG->y_u.l.l;
	GONext();

	BEGP(pt1);
	deref_body(d1, pt1, p_eq_var1_unk2, p_eq_var1_nvar2);
	/* first argument is unbound */
	/* second argument is unbound */
	if (pt1 != pt0) {
	  PREG = PREG->y_u.l.l;
	  GONext();
	}
	PREG = NEXTOP(PREG, l);
	GONext();
	ENDP(pt1);
	ENDD(d1);
	ENDP(pt0);

	ENDD(d0);
	ENDOp();

	Op(p_arg_vv, xxx);
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  HR[0] = XREG(PREG->y_u.xxx.x1);
	  HR[1] = XREG(PREG->y_u.xxx.x2);
	  RESET_VARIABLE(HR+2);
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),HR);
	}
#endif  /* LOW_LEVEL_TRACE */
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxx.x1);
	deref_head(d0, arg_arg1_unk);
      arg_arg1_nvar:
	/* ARG1 is ok! */
	if (IsIntTerm(d0))
	  d0 = IntOfTerm(d0);
	else if (IsLongIntTerm(d0)) {
	  d0 = LongIntOfTerm(d0);
	} else {
	  if (IsBigIntTerm( d0 ))
	    FAIL();
	  saveregs();
	  Yap_Error(TYPE_ERROR_INTEGER,d0,"arg 1 of arg/3");
	  setregs();
	  FAIL();
	}

	/* d0 now got the argument we want */
	BEGD(d1);
	d1 = XREG(PREG->y_u.xxx.x2);
	deref_head(d1, arg_arg2_unk);
      arg_arg2_nvar:
	/* d1 now got the structure we want to fetch the argument
	 * from */
	if (IsApplTerm(d1)) {
	  BEGP(pt0);
	  pt0 = RepAppl(d1);
	  d1 = *pt0;
	  if (IsExtensionFunctor((Functor) d1)) {
	    FAIL();
	  }
	  if ((Int)d0 <= 0 ||
	      (Int)d0 > ArityOfFunctor((Functor) d1)) {
	    /* don't complain here for Prolog compatibility
	       if ((Int)d0 <= 0) {
	       saveregs();
	       Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
	       MkIntegerTerm(d0),"arg 1 of arg/3");
	       setregs();
	       }
	    */
	    FAIL();
	  }
	  XREG(PREG->y_u.xxx.x) = pt0[d0];
	  PREG = NEXTOP(PREG, xxx);
	  GONext();
	  ENDP(pt0);
	}
	else if (IsPairTerm(d1)) {
	  BEGP(pt0);
	  pt0 = RepPair(d1);
	  if (d0 != 1 && d0 != 2) {
	    if ((Int)d0 < 0) {
	      saveregs();
	      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
			MkIntegerTerm(d0),"arg 1 of arg/3");
	      setregs();
	    }
	    FAIL();
	  }
	  XREG(PREG->y_u.xxx.x) = pt0[d0-1];
	  PREG = NEXTOP(PREG, xxx);
	  GONext();
	  ENDP(pt0);
	}
	else {
	  /*
	    don't complain here for SWI Prolog compatibility
	    saveregs();
	    Yap_Error(TYPE_ERROR_COMPOUND, d1, "arg 2 of arg/3");
	    setregs();
	  */
	  FAIL();
	}

	BEGP(pt0);
	deref_body(d1, pt0, arg_arg2_unk, arg_arg2_nvar);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3");;
	setregs();
	ENDP(pt0);
	FAIL();
	ENDD(d1);

	BEGP(pt0);
	deref_body(d0, pt0, arg_arg1_unk, arg_arg1_nvar);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d0, "arg 1 of arg/3");;
	setregs();
	ENDP(pt0);
	FAIL();
	ENDD(d0);
	ENDOp();

	Op(p_arg_cv, xxn);
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  CELL *Ho = HR;
	  Term t = MkIntegerTerm(PREG->y_u.xxn.c);
	  HR[0] =  t;
	  HR[1] = XREG(PREG->y_u.xxn.xi);
	  RESET_VARIABLE(HR+2);
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),HR);
	  HR = Ho;
	}
#endif  /* LOW_LEVEL_TRACE */
	BEGD(d0);
	d0 = PREG->y_u.xxn.c;
	/* d0 now got the argument we want */
	BEGD(d1);
	d1 = XREG(PREG->y_u.xxn.xi);
	deref_head(d1, arg_arg2_vc_unk);
      arg_arg2_vc_nvar:
	/* d1 now got the structure we want to fetch the argument
	 * from */
	if (IsApplTerm(d1)) {
	  BEGP(pt0);
	  pt0 = RepAppl(d1);
	  d1 = *pt0;
	  if (IsExtensionFunctor((Functor) d1)) {
	    FAIL();
	  }
	  if ((Int)d0 <= 0 ||
	      (Int)d0 > ArityOfFunctor((Functor) d1)) {
	    /* don't complain here for Prolog compatibility
	       if ((Int)d0 <= 0) {
	       saveregs();
	       Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
	       MkIntegerTerm(d0),"arg 1 of arg/3");
	       setregs();
	       }
	    */
	    FAIL();
	  }
	  XREG(PREG->y_u.xxn.x) = pt0[d0];
	  PREG = NEXTOP(PREG, xxn);
	  GONext();
	  ENDP(pt0);
	}
	else if (IsPairTerm(d1)) {
	  BEGP(pt0);
	  pt0 = RepPair(d1);
	  if (d0 != 1 && d0 != 2) {
	    if ((Int)d0 < 0) {
	      saveregs();
	      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
			MkIntegerTerm(d0),"arg 1 of arg/3");
	      setregs();
	    }
	    FAIL();
	  }
	  XREG(PREG->y_u.xxn.x) = pt0[d0-1];
	  PREG = NEXTOP(PREG, xxn);
	  GONext();
	  ENDP(pt0);
	}
	else {
	  /*
	    keep SWI Prolog compatibility, just fail on trying to obtain an argument of a compound term.
	    saveregs();
	    Yap_Error(TYPE_ERROR_COMPOUND, d1, "arg 2 of arg/3");
	    setregs();
	  */
	  FAIL();
	}

	BEGP(pt0);
	deref_body(d1, pt0, arg_arg2_vc_unk, arg_arg2_vc_nvar);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3");;
	setregs();
	ENDP(pt0);
	FAIL();
	ENDD(d1);

	ENDD(d0);
	ENDOp();

	Op(p_arg_y_vv, yxx);
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  HR[0] = XREG(PREG->y_u.yxx.x1);
	  HR[1] = XREG(PREG->y_u.yxx.x2);
	  HR[2] = YREG[PREG->y_u.yxx.y];
	  RESET_VARIABLE(HR+2);
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),HR);
	}
#endif  /* LOW_LEVEL_TRACE */
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxx.x1);
	deref_head(d0, arg_y_arg1_unk);
      arg_y_arg1_nvar:
	/* ARG1 is ok! */
	if (IsIntTerm(d0))
	  d0 = IntOfTerm(d0);
	else if (IsLongIntTerm(d0)) {
	  d0 = LongIntOfTerm(d0);
	} else {
	  if (IsBigIntTerm( d0 ))
	    FAIL();
	  saveregs();
	  Yap_Error(TYPE_ERROR_INTEGER,d0,"arg 1 of arg/3");
	  setregs();
	  FAIL();
	}

	/* d0 now got the argument we want */
	BEGD(d1);
	d1 = XREG(PREG->y_u.yxx.x2);
	deref_head(d1, arg_y_arg2_unk);
      arg_y_arg2_nvar:
	/* d1 now got the structure we want to fetch the argument
	 * from */
	if (IsApplTerm(d1)) {
	  BEGP(pt0);
	  pt0 = RepAppl(d1);
	  d1 = *pt0;
	  if (IsExtensionFunctor((Functor) d1)) {
	    FAIL();
	  }
	  if ((Int)d0 <= 0 ||
	      (Int)d0 > ArityOfFunctor((Functor) d1)) {
	    /* don't complain here for Prolog compatibility
	       if ((Int)d0 <= 0) {
	       saveregs();
	       Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
	       MkIntegerTerm(d0),"arg 1 of arg/3");
	       saveregs();
	       }
	    */
	    FAIL();
	  }
	  BEGP(pt1);
	  pt1 = YREG + PREG->y_u.yxx.y;
	  PREG = NEXTOP(PREG, yxx);
	  INITIALIZE_PERMVAR(pt1,pt0[d0]);
	  ENDP(pt1);
	  GONext();
	  ENDP(pt0);
	}
	else if (IsPairTerm(d1)) {
	  BEGP(pt0);
	  pt0 = RepPair(d1);
	  if (d0 != 1 && d0 != 2) {
	    if ((Int)d0 < 0) {
	      saveregs();
	      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
			MkIntegerTerm(d0),"arg 1 of arg/3");
	      setregs();
	    }
	    FAIL();
	  }
	  BEGP(pt1);
	  pt1 = YREG + PREG->y_u.yxx.y;
	  PREG = NEXTOP(PREG, yxx);
	  INITIALIZE_PERMVAR(pt1,pt0[d0-1]);
	  GONext();
	  ENDP(pt1);
	  ENDP(pt0);
	}
	else {
	  /*
	    don't complain here for SWI Prolog compatibility
	    saveregs();
	    Yap_Error(TYPE_ERROR_COMPOUND, d1, "arg 2 of arg/3");
	    setregs();
	  */
	  FAIL();
	}

	BEGP(pt0);
	deref_body(d1, pt0, arg_y_arg2_unk, arg_y_arg2_nvar);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3");;
	setregs();
	ENDP(pt0);
	FAIL();
	ENDD(d1);

	BEGP(pt0);
	deref_body(d0, pt0, arg_y_arg1_unk, arg_y_arg1_nvar);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d0, "arg 1 of arg/3");;
	setregs();
	ENDP(pt0);
	FAIL();
	ENDD(d0);
	ENDOp();

	Op(p_arg_y_cv, yxn);
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  CELL *Ho = HR;
	  Term t = MkIntegerTerm(PREG->y_u.yxn.c);
	  HR[0] =  t;
	  HR[1] = XREG(PREG->y_u.yxn.xi);
	  HR[2] = YREG[PREG->y_u.yxn.y];
	  RESET_VARIABLE(HR+2);
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),HR);
	  HR = Ho;
	}
#endif  /* LOW_LEVEL_TRACE */
	BEGD(d0);
	d0 = PREG->y_u.yxn.c;
	/* d0 now got the argument we want */
	BEGD(d1);
	d1 = XREG(PREG->y_u.yxn.xi);
	deref_head(d1, arg_y_arg2_vc_unk);
      arg_y_arg2_vc_nvar:
	/* d1 now got the structure we want to fetch the argument
	 * from */
	if (IsApplTerm(d1)) {
	  BEGP(pt0);
	  pt0 = RepAppl(d1);
	  d1 = *pt0;
	  if (IsExtensionFunctor((Functor) d1)) {
	    FAIL();
	  }
	  if ((Int)d0 <= 0 ||
	      (Int)d0 > ArityOfFunctor((Functor) d1)) {
	    /* don't complain here for Prolog compatibility
	       if ((Int)d0 <= 0) {
	       saveregs();
	       Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
	       MkIntegerTerm(d0),"arg 1 of arg/3");
	       setregs();
	       }
	    */
	    FAIL();
	  }
	  BEGP(pt1);
	  pt1 = YREG + PREG->y_u.yxn.y;
	  PREG = NEXTOP(PREG, yxn);
	  INITIALIZE_PERMVAR(pt1,pt0[d0]);
	  ENDP(pt1);
	  GONext();
	  ENDP(pt0);
	}
	else if (IsPairTerm(d1)) {
	  BEGP(pt0);
	  pt0 = RepPair(d1);
	  if (d0 != 1 && d0 != 2) {
	    if ((Int)d0 < 0) {
	      saveregs();
	      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
			MkIntegerTerm(d0),"arg 1 of arg/3");
	      setregs();
	    }
	    FAIL();
	  }
	  BEGP(pt1);
	  pt1 = YREG + PREG->y_u.yxn.y;
	  PREG = NEXTOP(PREG, yxn);
	  INITIALIZE_PERMVAR(pt1,pt0[d0-1]);
	  ENDP(pt1);
	  GONext();
	  ENDP(pt0);
	}
	else {
	  /*
	    don't complain here for SWI Prolog compatibility
	    saveregs();
	    Yap_Error(TYPE_ERROR_COMPOUND, d1, "arg 2 of arg/3");
	    setregs();
	  */
	  FAIL();
	}

	BEGP(pt0);
	deref_body(d1, pt0, arg_y_arg2_vc_unk, arg_y_arg2_vc_nvar);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3");;
	setregs();
	ENDP(pt0);
	FAIL();
	ENDD(d1);

	ENDD(d0);
	ENDOp();

	Op(p_func2s_vv, xxx);
	/* A1 is a variable */
      restart_func2s:
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  RESET_VARIABLE(HR);
	  HR[1] = XREG(PREG->y_u.xxx.x1);
	  HR[2] = XREG(PREG->y_u.xxx.x2);
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
	}
#endif  /* LOW_LEVEL_TRACE */
	/* We have to build the structure */
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxx.x1);
	deref_head(d0, func2s_unk);
      func2s_nvar:
	/* we do, let's get the third argument */
	BEGD(d1);
	d1 = XREG(PREG->y_u.xxx.x2);
	deref_head(d1, func2s_unk2);
      func2s_nvar2:
	/* Uuuff, the second and third argument are bound */
	if (IsIntegerTerm(d1))
	  d1 = IntegerOfTerm(d1);
	else {
	  saveregs();
	  if (IsBigIntTerm(d1)) {
	    Yap_Error(RESOURCE_ERROR_STACK, d1, "functor/3");
	  } else {
	    Yap_Error(TYPE_ERROR_INTEGER, d1, "functor/3");
	  }
	  setregs();
	  FAIL();
	}
	if (!IsAtomicTerm(d0)) {
	  saveregs();
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  setregs();
	  FAIL();
	}
	/* We made it!!!!! we got in d0 the name, in d1 the arity and
	 * in pt0 the variable to bind it to. */
	if (d0 == TermDot && d1 == 2) {
	  RESET_VARIABLE(HR);
	  RESET_VARIABLE(HR+1);
	  d0 = AbsPair(HR);
	  HR += 2;
	  /* else if arity is 0 just pass d0 through */
	  /* Ding, ding, we made it */
	  XREG(PREG->y_u.xxx.x) = d0;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxx),Osbpp),l);
	  GONext();
	}
	else if ((Int)d1 > 0) {
	  /* now let's build a compound term */
	  if (!IsAtomTerm(d0)) {
	    saveregs();
	    Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	    setregs();
	    FAIL();
	  }
	  BEGP(pt1);
	  if (!IsAtomTerm(d0)) {
	    FAIL();
	  }
	  else
	    d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);
	  pt1 = HR;
	  *pt1++ = d0;
	  d0 = AbsAppl(HR);
	  if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	    /* make sure we have something to show for our trouble */
	    saveregs();
	    if (!Yap_gcl((1+d1)*sizeof(CELL), 0, YREG, NEXTOP(NEXTOP(PREG,xxx),Osbpp))) {
	      Yap_NilError(RESOURCE_ERROR_STACK,"stack overflow: gc failed");
	      setregs();
	      JMPNext();
	    } else {
	      setregs();
	    }
	    goto restart_func2s;
	  }
	  while ((Int)d1--) {
	    RESET_VARIABLE(pt1);
	    pt1++;
	  }
	  HR = pt1;
	  /* done building the term */
	  ENDP(pt1);
	  /* else if arity is 0 just pass d0 through */
	  /* Ding, ding, we made it */
	  XREG(PREG->y_u.xxx.x) = d0;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxx),Osbpp),l);
	  GONext();
	}     else if ((Int)d1  == 0) {
	  XREG(PREG->y_u.xxx.x) = d0;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxx),Osbpp),l);
	  GONext();
	}     else {
	  saveregs();
	  Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	  setregs();
	  FAIL();
	}

	BEGP(pt1);
	deref_body(d1, pt1, func2s_unk2, func2s_nvar2);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d1, "functor/3");
	setregs();
	ENDP(pt1);
	/* Oops, third argument was unbound */
	FAIL();
	ENDD(d1);

	BEGP(pt1);
	deref_body(d0, pt1, func2s_unk, func2s_nvar);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
	setregs();
	ENDP(pt1);
	/* Oops, second argument was unbound too */
	FAIL();
	ENDD(d0);
	ENDOp();

	Op(p_func2s_cv, xxc);
	/* A1 is a variable */
      restart_func2s_cv:
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  RESET_VARIABLE(HR);
	  HR[1] = PREG->y_u.xxc.c;
	  HR[2] = XREG(PREG->y_u.xxc.xi);
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
	}
#endif  /* LOW_LEVEL_TRACE */
	BEGD(d0);
	/* We have to build the structure */
	d0 = PREG->y_u.xxc.c;
	/* we do, let's get the third argument */
	BEGD(d1);
	d1 = XREG(PREG->y_u.xxc.xi);
	deref_head(d1, func2s_unk2_cv);
      func2s_nvar2_cv:
	/* Uuuff, the second and third argument are bound */
	if (IsIntegerTerm(d1))
	  d1 = IntegerOfTerm(d1);
	else {
	  saveregs();
	  if (IsBigIntTerm(d1)) {
	    Yap_Error(RESOURCE_ERROR_STACK, d1, "functor/3");
	  } else {
	    Yap_Error(TYPE_ERROR_INTEGER,d1,"functor/3");
	  }
	  setregs();
	  FAIL();
	}
	/* We made it!!!!! we got in d0 the name, in d1 the arity and
	 * in pt0 the variable to bind it to. */
	if (d0 == TermDot && d1 == 2) {
	  RESET_VARIABLE(HR);
	  RESET_VARIABLE(HR+1);
	  d0 = AbsPair(HR);
	  HR += 2;
	  /* else if arity is 0 just pass d0 through */
	  /* Ding, ding, we made it */
	  XREG(PREG->y_u.xxc.x) = d0;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxc),Osbpp),l);
	  GONext();
	} else if ((Int)d1 > 0) {
	  /* now let's build a compound term */
	  if (!IsAtomTerm(d0)) {
	    saveregs();
	    Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	    setregs();
	    FAIL();
	  }
	  BEGP(pt1);
	  if (!IsAtomTerm(d0)) {
	    FAIL();
	  }
	  else
	    d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);
	  pt1 = HR;
	  *pt1++ = d0;
	  d0 = AbsAppl(HR);
	  if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	    /* make sure we have something to show for our trouble */
	    saveregs();
	    if (!Yap_gcl((1+d1)*sizeof(CELL), 0, YREG, NEXTOP(NEXTOP(PREG,xxc),Osbpp))) {
	      Yap_NilError(RESOURCE_ERROR_STACK,"stack overflow: gc failed");
	      setregs();
	      JMPNext();
	    } else {
	      setregs();
	    }
	    goto restart_func2s_cv;
	  }
	  while ((Int)d1--) {
	    RESET_VARIABLE(pt1);
	    pt1++;
	  }
	  /* done building the term */
	  HR = pt1;
	  ENDP(pt1);
	  /* else if arity is 0 just pass d0 through */
	  /* Ding, ding, we made it */
	  XREG(PREG->y_u.xxc.x) = d0;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxc),Osbpp),l);
	  GONext();
	}     else if (d1  == 0) {
	  XREG(PREG->y_u.xxc.x) = d0;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxc),Osbpp),l);
	  GONext();
	}     else {
	  saveregs();
	  Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	  setregs();
	  FAIL();
	}

	BEGP(pt1);
	deref_body(d1, pt1, func2s_unk2_cv, func2s_nvar2_cv);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d1, "functor/3");
	setregs();
	ENDP(pt1);
	/* Oops, third argument was unbound */
	FAIL();
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_func2s_vc, xxn);
	/* A1 is a variable */
      restart_func2s_vc:
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  Term ti;
	  CELL *hi = HR;

	  ti = MkIntegerTerm(PREG->y_u.xxn.c);
	  RESET_VARIABLE(HR);
	  HR[1] = XREG(PREG->y_u.xxn.xi);
	  HR[2] = ti;
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
	  HR = hi;
	}
#endif  /* LOW_LEVEL_TRACE */
	/* We have to build the structure */
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxn.xi);
	deref_head(d0, func2s_unk_vc);
      func2s_nvar_vc:
	BEGD(d1);
	d1 = PREG->y_u.xxn.c;
	if (!IsAtomicTerm(d0)) {
	  saveregs();
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  setregs();
	  FAIL();
	}
	/* We made it!!!!! we got in d0 the name, in d1 the arity and
	 * in pt0 the variable to bind it to. */
	if (d0 == TermDot && d1 == 2) {
	  RESET_VARIABLE(HR);
	  RESET_VARIABLE(HR+1);
	  d0 = AbsPair(HR);
	  HR += 2;
	  /* else if arity is 0 just pass d0 through */
	  /* Ding, ding, we made it */
	  XREG(PREG->y_u.xxn.x) = d0;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxn),Osbpp),l);
	  GONext();
	}
	/* now let's build a compound term */
	if (d1 == 0) {
	  XREG(PREG->y_u.xxn.x) = d0;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxn),Osbpp),l);
	  GONext();
	}
	if (!IsAtomTerm(d0)) {
	  saveregs();
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  setregs();
	  FAIL();
	}
	BEGP(pt1);
	if (!IsAtomTerm(d0)) {
	  FAIL();
	}
	else
	  d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);
	pt1 = HR;
	*pt1++ = d0;
	d0 = AbsAppl(HR);
	if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	  /* make sure we have something to show for our trouble */
	  saveregs();
	  if (!Yap_gc(0, YREG, NEXTOP(NEXTOP(PREG,xxn),Osbpp))) {
	    Yap_NilError(RESOURCE_ERROR_STACK,"stack overflow: gc failed");
	    setregs();
	    JMPNext();
	  } else {
	    setregs();
	  }
	  goto restart_func2s_vc;
	}
	while ((Int)d1--) {
	  RESET_VARIABLE(pt1);
	  pt1++;
	}
	/* done building the term */
	HR = pt1;
	ENDP(pt1);
	ENDD(d1);
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	XREG(PREG->y_u.xxn.x) = d0;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxn),Osbpp),l);
	GONext();

	BEGP(pt1);
	deref_body(d0, pt1, func2s_unk_vc, func2s_nvar_vc);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
	setregs();
	ENDP(pt1);
	/* Oops, second argument was unbound too */
	FAIL();
	ENDD(d0);
	ENDOp();

	Op(p_func2s_y_vv, yxx);
	/* A1 is a variable */
      restart_func2s_y:
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  RESET_VARIABLE(HR);
	  HR[1] = XREG(PREG->y_u.yxx.x1);
	  HR[2] = XREG(PREG->y_u.yxx.x2);
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
	}
#endif  /* LOW_LEVEL_TRACE */
	/* We have to build the structure */
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxx.x1);
	deref_head(d0, func2s_y_unk);
      func2s_y_nvar:
	/* we do, let's get the third argument */
	BEGD(d1);
	d1 = XREG(PREG->y_u.yxx.x2);
	deref_head(d1, func2s_y_unk2);
      func2s_y_nvar2:
	/* Uuuff, the second and third argument are bound */
	if (IsIntegerTerm(d1))
	  d1 = IntegerOfTerm(d1);
	else {
	  saveregs();
	  if (IsBigIntTerm(d1)) {
	    Yap_Error(RESOURCE_ERROR_STACK, d1, "functor/3");
	  } else {
	    Yap_Error(TYPE_ERROR_INTEGER,d1,"functor/3");
	  }
	  setregs();
	  FAIL();
	}
	if (!IsAtomicTerm(d0)) {
	  saveregs();
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  setregs();
	  FAIL();
	}
	/* We made it!!!!! we got in d0 the name, in d1 the arity and
	 * in pt0 the variable to bind it to. */
	if (d0 == TermDot && d1 == 2) {
	  RESET_VARIABLE(HR);
	  RESET_VARIABLE(HR+1);
	  d0 = AbsPair(HR);
	  HR += 2;
	  BEGP(pt1);
	  pt1 = YREG + PREG->y_u.yxx.y;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxx),Osbpp),l);
	  INITIALIZE_PERMVAR(pt1,d0);
	  ENDP(pt1);
	  GONext();
	} else if ((Int)d1 > 0) {
	  /* now let's build a compound term */
	  if (!IsAtomTerm(d0)) {
	    saveregs();
	    Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	    setregs();
	    FAIL();
	  }
	  BEGP(pt1);
	  if (!IsAtomTerm(d0)) {
	    FAIL();
	  }
	  else
	    d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);
	  pt1 = HR;
	  *pt1++ = d0;
	  d0 = AbsAppl(HR);
	  if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	    /* make sure we have something to show for our trouble */
	    saveregs();
	    if (!Yap_gcl((1+d1)*sizeof(CELL), 0, YREG, NEXTOP(NEXTOP(PREG,yxx),Osbpp))) {
	      Yap_NilError(RESOURCE_ERROR_STACK,"stack overflow: gc failed");
	      setregs();
	      JMPNext();
	    } else {
	      setregs();
	    }
	    goto restart_func2s_y;
	  }
	  while ((Int)d1--) {
	    RESET_VARIABLE(pt1);
	    pt1++;
	  }
	  /* done building the term */
	  HR = pt1;
	  ENDP(pt1);
	  /* else if arity is 0 just pass d0 through */
	  /* Ding, ding, we made it */
	  BEGP(pt1);
	  pt1 = YREG + PREG->y_u.yxx.y;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxx),Osbpp),l);
	  INITIALIZE_PERMVAR(pt1,d0);
	  ENDP(pt1);
	  GONext();
	}     else if (d1  == 0) {
	  BEGP(pt1);
	  pt1 = YREG + PREG->y_u.yxx.y;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxx),Osbpp),l);
	  INITIALIZE_PERMVAR(pt1,d0);
	  ENDP(pt1);
	  GONext();
	}     else {
	  saveregs();
	  Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	  setregs();
	  FAIL();
	}

	BEGP(pt1);
	deref_body(d1, pt1, func2s_y_unk2, func2s_y_nvar2);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d1, "functor/3");
	setregs();
	ENDP(pt1);
	/* Oops, third argument was unbound */
	FAIL();
	ENDD(d1);

	BEGP(pt1);
	deref_body(d0, pt1, func2s_y_unk, func2s_y_nvar);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
	setregs();
	ENDP(pt1);
	/* Oops, second argument was unbound too */
	FAIL();
	ENDD(d0);
	ENDOp();

	Op(p_func2s_y_cv, yxc);
	/* A1 is a variable */
      restart_func2s_y_cv:
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  RESET_VARIABLE(HR);
	  HR[1] = PREG->y_u.yxc.c;
	  HR[2] = XREG(PREG->y_u.yxc.xi);
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
	}
#endif  /* LOW_LEVEL_TRACE */
	/* We have to build the structure */
	BEGD(d0);
	d0 = PREG->y_u.yxc.c;
	/* we do, let's get the third argument */
	BEGD(d1);
	d1 = XREG(PREG->y_u.yxc.xi);
	deref_head(d1, func2s_y_unk_cv);
      func2s_y_nvar_cv:
	/* Uuuff, the second and third argument are bound */
	if (IsIntegerTerm(d1)) {
	  d1 = IntegerOfTerm(d1);
	} else {
	  saveregs();
	  if (IsBigIntTerm(d1)) {
	    Yap_Error(RESOURCE_ERROR_STACK, d1, "functor/3");
	  } else {
	    Yap_Error(TYPE_ERROR_INTEGER,d1,"functor/3");
	  }
	  setregs();
	  FAIL();
	}
	/* We made it!!!!! we got in d0 the name, in d1 the arity and
	 * in pt0 the variable to bind it to. */
	if (d0 == TermDot && d1 == 2) {
	  RESET_VARIABLE(HR);
	  RESET_VARIABLE(HR+1);
	  d0 = AbsPair(HR);
	  HR += 2;
	  /* else if arity is 0 just pass d0 through */
	  /* Ding, ding, we made it */
	  BEGP(pt1);
	  pt1 = YREG + PREG->y_u.yxc.y;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxc),Osbpp),l);
	  INITIALIZE_PERMVAR(pt1,d0);
	  ENDP(pt1);
	  GONext();
	}
	else if ((Int)d1 > 0) {
	  /* now let's build a compound term */
	  if (!IsAtomTerm(d0)) {
	    saveregs();
	    Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	    setregs();
	    FAIL();
	  }
	  if (!IsAtomTerm(d0)) {
	    FAIL();
	  }
	  else
	    d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);
	  BEGP(pt1);
	  pt1 = HR;
	  *pt1++ = d0;
	  d0 = AbsAppl(HR);
	  if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	    /* make sure we have something to show for our trouble */
	    saveregs();
	    if (!Yap_gcl((1+d1)*sizeof(CELL), 0, YREG, NEXTOP(NEXTOP(PREG,yxc),Osbpp))) {
	      Yap_NilError(RESOURCE_ERROR_STACK,"stack overflow: gc failed");
	      setregs();
	      JMPNext();
	    } else {
	      setregs();
	    }
	    goto restart_func2s_y_cv;
	  }
	  while ((Int)d1--) {
	    RESET_VARIABLE(pt1);
	    pt1++;
	  }
	  /* done building the term */
	  HR = pt1;
	  ENDP(pt1);
	  /* else if arity is 0 just pass d0 through */
	  /* Ding, ding, we made it */
	  BEGP(pt1);
	  pt1 = YREG + PREG->y_u.yxc.y;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxc),Osbpp),l);
	  INITIALIZE_PERMVAR(pt1,d0);
	  ENDP(pt1);
	  GONext();
	}     else if (d1  == 0) {
	  BEGP(pt1);
	  pt1 = YREG + PREG->y_u.yxc.y;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxc),Osbpp),l);
	  INITIALIZE_PERMVAR(pt1,d0);
	  ENDP(pt1);
	  GONext();
	}     else {
	  saveregs();
	  Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	  setregs();
	  FAIL();
	}

	BEGP(pt1);
	deref_body(d1, pt1, func2s_y_unk_cv, func2s_y_nvar_cv);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d1, "functor/3");
	setregs();
	ENDP(pt1);
	/* Oops, third argument was unbound */
	FAIL();
	ENDD(d1);
	ENDD(d0);
	ENDOp();

	Op(p_func2s_y_vc, yxn);
	/* A1 is a variable */
      restart_func2s_y_vc:
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  Term ti;
	  CELL *hi = HR;

	  ti = MkIntegerTerm((Int)(PREG->y_u.yxn.c));
	  RESET_VARIABLE(HR);
	  HR[1] = XREG(PREG->y_u.yxn.xi);
	  HR[2] = ti;
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
	  HR = hi;
	}
#endif  /* LOW_LEVEL_TRACE */
	/* We have to build the structure */
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxn.xi);
	deref_head(d0, func2s_y_unk_vc);
      func2s_y_nvar_vc:
	BEGD(d1);
	d1 = PREG->y_u.yxn.c;
	if (!IsAtomicTerm(d0)) {
	  saveregs();
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  setregs();
	  FAIL();
	}
	/* We made it!!!!! we got in d0 the name, in d1 the arity and
	 * in pt0 the variable to bind it to. */
	if (d0 == TermDot && d1 == 2) {
	  RESET_VARIABLE(HR);
	  RESET_VARIABLE(HR+1);
	  d0 = AbsPair(HR);
	  HR += 2;
	  /* else if arity is 0 just pass d0 through */
	  /* Ding, ding, we made it */
	  BEGP(pt1);
	  pt1 = YREG + PREG->y_u.yxn.y;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxn),Osbpp),l);
	  INITIALIZE_PERMVAR(pt1,d0);
	  ENDP(pt1);
	  GONext();
	}
	if (d1 == 0) {
	  BEGP(pt1);
	  pt1 = YREG + PREG->y_u.yxn.y;
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxn),Osbpp),l);
	  INITIALIZE_PERMVAR(pt1,d0);
	  ENDP(pt1);
	  GONext();
	}
	if (!IsAtomTerm(d0)) {
	  saveregs();
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  setregs();
	  FAIL();
	}
	/* now let's build a compound term */
	if (!IsAtomTerm(d0)) {
	  saveregs();
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  setregs();
	  FAIL();
	}
	BEGP(pt1);
	if (!IsAtomTerm(d0)) {
	  FAIL();
	}
	else
	  d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);
	pt1 = HR;
	*pt1++ = d0;
	d0 = AbsAppl(HR);
	if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	  /* make sure we have something to show for our trouble */
	  saveregs();
	  if (!Yap_gcl((1+d1)*sizeof(CELL), 0, YREG, NEXTOP(NEXTOP(PREG,yxn),Osbpp))) {
	    Yap_NilError(RESOURCE_ERROR_STACK,"stack overflow: gc failed");
	    setregs();
	    JMPNext();
	  } else {
	    setregs();
	  }
	  goto restart_func2s_y_vc;
	}
	while ((Int)d1--) {
	  RESET_VARIABLE(pt1);
	  pt1++;
	}
	/* done building the term */
	HR = pt1;
	ENDP(pt1);
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	BEGP(pt1);
	pt1 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxn),Osbpp),l);
	INITIALIZE_PERMVAR(pt1,d0);
	ENDP(pt1);
	ENDD(d1);
	GONext();

	BEGP(pt1);
	deref_body(d0, pt1, func2s_y_unk_vc, func2s_y_nvar_vc);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
	setregs();
	ENDP(pt1);
	/* Oops, second argument was unbound too */
	FAIL();
	ENDD(d0);
	ENDOp();

	Op(p_func2f_xx, xxx);
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  HR[0] = XREG(PREG->y_u.xxx.x);
	  RESET_VARIABLE(HR+1);
	  RESET_VARIABLE(HR+2);
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
	}
#endif  /* LOW_LEVEL_TRACE */
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxx.x);
	deref_head(d0, func2f_xx_unk);
      func2f_xx_nvar:
	if (IsApplTerm(d0)) {
	  Functor d1 = FunctorOfTerm(d0);
	  if (IsExtensionFunctor(d1)) {
	    XREG(PREG->y_u.xxx.x1) = d0;
	    XREG(PREG->y_u.xxx.x2) = MkIntTerm(0);
	    PREG = NEXTOP(PREG, xxx);
	    GONext();
	  }
	  XREG(PREG->y_u.xxx.x1) = MkAtomTerm(NameOfFunctor(d1));
	  XREG(PREG->y_u.xxx.x2) = MkIntegerTerm(ArityOfFunctor(d1));
	  PREG = NEXTOP(PREG, xxx);
	  GONext();
	} else if (IsPairTerm(d0)) {
	  XREG(PREG->y_u.xxx.x1) = TermDot;
	  XREG(PREG->y_u.xxx.x2) = MkIntTerm(2);
	  PREG = NEXTOP(PREG, xxx);
	  GONext();
	} else {
	  XREG(PREG->y_u.xxx.x1) = d0;
	  XREG(PREG->y_u.xxx.x2) = MkIntTerm(0);
	  PREG = NEXTOP(PREG, xxx);
	  GONext();
	}

	BEGP(pt1);
	deref_body(d0, pt1, func2f_xx_unk, func2f_xx_nvar);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
	setregs();
	ENDP(pt1);
	/* Oops, second argument was unbound too */
	FAIL();
	ENDD(d0);
	ENDOp();

	Op(p_func2f_xy, xxy);
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  HR[0] = XREG(PREG->y_u.xxy.x);
	  RESET_VARIABLE(HR+1);
	  RESET_VARIABLE(HR+2);
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
	}
#endif  /* LOW_LEVEL_TRACE */
	BEGD(d0);
	d0 = XREG(PREG->y_u.xxy.x);
	deref_head(d0, func2f_xy_unk);
      func2f_xy_nvar:
	if (IsApplTerm(d0)) {
	  Functor d1 = FunctorOfTerm(d0);
	  CELL *pt0 = YREG+PREG->y_u.xxy.y2;
	  if (IsExtensionFunctor(d1)) {
	    XREG(PREG->y_u.xxy.x1) = d0;
	    PREG = NEXTOP(PREG, xxy);
	    INITIALIZE_PERMVAR(pt0, MkIntTerm(0));
	    GONext();
	  }
	  XREG(PREG->y_u.xxy.x1) = MkAtomTerm(NameOfFunctor(d1));
	  PREG = NEXTOP(PREG, xxy);
	  INITIALIZE_PERMVAR(pt0, MkIntegerTerm(ArityOfFunctor(d1)));
	  GONext();
	} else if (IsPairTerm(d0)) {
	  CELL *pt0 = YREG+PREG->y_u.xxy.y2;
	  XREG(PREG->y_u.xxy.x1) = TermDot;
	  PREG = NEXTOP(PREG, xxy);
	  INITIALIZE_PERMVAR(pt0, MkIntTerm(2));
	  GONext();
	} else {
	  CELL *pt0 = YREG+PREG->y_u.xxy.y2;
	  XREG(PREG->y_u.xxy.x1) = d0;
	  PREG = NEXTOP(PREG, xxy);
	  INITIALIZE_PERMVAR(pt0, MkIntTerm(0));
	  GONext();
	}

	BEGP(pt1);
	deref_body(d0, pt1, func2f_xy_unk, func2f_xy_nvar);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
	setregs();
	ENDP(pt1);
	/* Oops, second argument was unbound too */
	FAIL();
	ENDD(d0);
	ENDOp();

	Op(p_func2f_yx, yxx);
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  HR[0] = XREG(PREG->y_u.yxx.x2);
	  RESET_VARIABLE(HR+1);
	  RESET_VARIABLE(HR+2);
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
	}
#endif  /* LOW_LEVEL_TRACE */
	BEGD(d0);
	d0 = XREG(PREG->y_u.yxx.x2);
	deref_head(d0, func2f_yx_unk);
      func2f_yx_nvar:
	if (IsApplTerm(d0)) {
	  Functor d1 = FunctorOfTerm(d0);
	  CELL *pt0 = YREG+PREG->y_u.yxx.y;
	  if (IsExtensionFunctor(d1)) {
	    XREG(PREG->y_u.yxx.x1) = MkIntTerm(0);
	    PREG = NEXTOP(PREG, yxx);
	    INITIALIZE_PERMVAR(pt0, d0);
	    GONext();
	  }
	  XREG(PREG->y_u.yxx.x1) = MkIntegerTerm(ArityOfFunctor(d1));
	  PREG = NEXTOP(PREG, yxx);
	  INITIALIZE_PERMVAR(pt0,  MkAtomTerm(NameOfFunctor(d1)));
	  GONext();
	} else if (IsPairTerm(d0)) {
	  CELL *pt0 = YREG+PREG->y_u.yxx.y;
	  XREG(PREG->y_u.yxx.x1) = MkIntTerm(2);
	  PREG = NEXTOP(PREG, yxx);
	  INITIALIZE_PERMVAR(pt0 ,TermDot);
	  GONext();
	} else {
	  CELL *pt0 = YREG+PREG->y_u.yxx.y;
	  XREG(PREG->y_u.yxx.x1) = MkIntTerm(0);
	  PREG = NEXTOP(PREG, yxx);
	  INITIALIZE_PERMVAR(pt0, d0);
	  GONext();
	}

	BEGP(pt1);
	deref_body(d0, pt1, func2f_yx_unk, func2f_yx_nvar);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
	setregs();
	ENDP(pt1);
	/* Oops, second argument was unbound too */
	FAIL();
	ENDD(d0);
	ENDOp();

	Op(p_func2f_yy, yyx);
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  HR[0] = XREG(PREG->y_u.yyx.x);
	  RESET_VARIABLE(HR+1);
	  RESET_VARIABLE(HR+2);
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
	}
#endif  /* LOW_LEVEL_TRACE */
	BEGD(d0);
	d0 = XREG(PREG->y_u.yyx.x);
	deref_head(d0, func2f_yy_unk);
      func2f_yy_nvar:
	if (IsApplTerm(d0)) {
	  Functor d1 = FunctorOfTerm(d0);
	  CELL *pt0 = YREG+PREG->y_u.yyx.y1;
	  CELL *pt1 = YREG+PREG->y_u.yyx.y2;
	  if (IsExtensionFunctor(d1)) {
	    PREG = NEXTOP(PREG, yyx);
	    INITIALIZE_PERMVAR(pt0,  d0);
	    INITIALIZE_PERMVAR(pt1, MkIntTerm(0));
	    GONext();
	  }
	  PREG = NEXTOP(PREG, yyx);
	  INITIALIZE_PERMVAR(pt0, MkAtomTerm(NameOfFunctor(d1)));
	  INITIALIZE_PERMVAR(pt1, MkIntegerTerm(ArityOfFunctor(d1)));
	  GONext();
	} else if (IsPairTerm(d0)) {
	  CELL *pt0 = YREG+PREG->y_u.yyx.y1;
	  CELL *pt1 = YREG+PREG->y_u.yyx.y2;
	  PREG = NEXTOP(PREG, yyx);
	  INITIALIZE_PERMVAR(pt0, TermDot);
	  INITIALIZE_PERMVAR(pt1, MkIntTerm(2));
	  GONext();
	} else {
	  CELL *pt0 = YREG+PREG->y_u.yyx.y1;
	  CELL *pt1 = YREG+PREG->y_u.yyx.y2;
	  PREG = NEXTOP(PREG, yyx);
	  INITIALIZE_PERMVAR(pt0, d0);
	  INITIALIZE_PERMVAR(pt1, MkIntTerm(0));
	  GONext();
	}

	BEGP(pt1);
	deref_body(d0, pt1, func2f_yy_unk, func2f_yy_nvar);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
	setregs();
	ENDP(pt1);
	/* Oops, second argument was unbound too */
	FAIL();
	ENDD(d0);
	ENDOp();

	Op(p_functor, e);
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace)
	  low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),XREGS+1);
#endif  /* LOW_LEVEL_TRACE */
      restart_functor:
	BEGD(d0);
	d0 = ARG1;
	deref_head(d0, func_unk);
      func_nvar:
	/* A1 is bound */
	BEGD(d1);
	if (IsApplTerm(d0)) {
	  d1 = *RepAppl(d0);
	  if (IsExtensionFunctor((Functor) d1)) {
	    if (d1 <= (CELL)FunctorDouble && d1 >= (CELL)FunctorLongInt ) {
	      d1 = MkIntTerm(0);
	    } else
	      FAIL();
	  } else {
	    d0 = MkAtomTerm(NameOfFunctor((Functor) d1));
	    d1 = MkIntTerm(ArityOfFunctor((Functor) d1));
	  }
	}
	else if (IsPairTerm(d0)) {
	  d0 = TermDot;
	  d1 = MkIntTerm(2);
	}
	else {
	  d1 = MkIntTerm(0);
	}
	/* d1 and d0 now have the two arguments */
	/* let's go and bind them */
	{
	  register CELL arity = d1;

	  d1 = ARG2;
	  deref_head(d1, func_nvar_unk);
	func_nvar_nvar:
	  /* A2 was bound */
	  if (d0 != d1) {
	    FAIL();
	  }
	  /* I have to this here so that I don't have a jump to a closing bracket */
	  d0 = arity;
	  goto func_bind_x3;

	  BEGP(pt0);
	  deref_body(d1, pt0, func_nvar_unk, func_nvar_nvar);
	  /* A2 is a variable, go and bind it */
	  YapBind(pt0, d0);
	  /* I have to this here so that I don't have a jump to a closing bracket */
	  d0 = arity;
	  ENDP(pt0);
	func_bind_x3:
	  /* now let's process A3 */
	  d1 = ARG3;
	  deref_head(d1, func_nvar3_unk);
	func_nvar3_nvar:
	  /* A3 was bound */
	  if (d0 != d1) {
	    FAIL();
	  }
	  /* Done */
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, e),Osbmp),l);
	  GONext();

	  BEGP(pt0);
	  deref_body(d1, pt0, func_nvar3_unk, func_nvar3_nvar);
	  /* A3 is a variable, go and bind it */
	  PREG = NEXTOP(NEXTOP(NEXTOP(PREG, e),Osbmp),l);
	  YapBind(pt0, d0);
	  /* Done */
	  GONext();


	  ENDP(pt0);

	}
	ENDD(d1);

	BEGP(pt0);
	deref_body(d0, pt0, func_unk, func_nvar);
	/* A1 is a variable */
	/* We have to build the structure */
	d0 = ARG2;
	deref_head(d0, func_var_2unk);
      func_var_2nvar:
	/* we do, let's get the third argument */
	BEGD(d1);
	d1 = ARG3;
	deref_head(d1, func_var_3unk);
      func_var_3nvar:
	/* Uuuff, the second and third argument are bound */
	if (IsIntTerm(d1))
	  d1 = IntOfTerm(d1);
	else {
	  saveregs();
	  Yap_Error(TYPE_ERROR_INTEGER,ARG3,"functor/3");
	  setregs();
	  FAIL();
	}
	if (!IsAtomicTerm(d0)) {
	  saveregs();
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  setregs();
	  FAIL();
	}      /* We made it!!!!! we got in d0 the name, in d1 the arity and
		* in pt0 the variable to bind it to. */
	if (d0 == TermDot && d1 == 2) {
	  RESET_VARIABLE(HR);
	  RESET_VARIABLE(HR+1);
	  d0 = AbsPair(HR);
	  HR += 2;
	}
	else if ((Int)d1 > 0) {
	  /* now let's build a compound term */
	  if (!IsAtomTerm(d0)) {
	    saveregs();
	    Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	    setregs();
	    FAIL();
	  }
	  BEGP(pt1);
	  if (!IsAtomTerm(d0)) {
	    FAIL();
	  }
	  else
	    d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);
	  pt1 = HR;
	  *pt1++ = d0;
	  d0 = AbsAppl(HR);
	  if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	    /* make sure we have something to show for our trouble */
	    saveregs();
	    if (!Yap_gcl((1+d1)*sizeof(CELL), 3, YREG, NEXTOP(NEXTOP(PREG,e),Osbmp))) {
	      Yap_NilError(RESOURCE_ERROR_STACK,"stack overflow: gc failed" );
	      setregs();
	      JMPNext();
	    } else {
	      setregs();
	    }
	    goto restart_functor;     /*  */
	  }
	  while ((Int)d1--) {
	    RESET_VARIABLE(pt1);
	    pt1++;
	  }
	  /* done building the term */
	  HR = pt1;
	  ENDP(pt1);
	}     else if ((Int)d1  < 0) {
	  saveregs();
	  Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	  setregs();
	  FAIL();
	}
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, e),Osbpp),l);
	YapBind(pt0, d0);
	GONext();


	BEGP(pt1);
	deref_body(d1, pt1, func_var_3unk, func_var_3nvar);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d1, "functor/3");
	setregs();
	ENDP(pt1);
	/* Oops, third argument was unbound */
	FAIL();
	ENDD(d1);

	BEGP(pt1);
	deref_body(d0, pt1, func_var_2unk, func_var_2nvar);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
	setregs();
	ENDP(pt1);
	/* Oops, second argument was unbound too */
	FAIL();
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	/* join all the meta-call code into a single procedure with three entry points */
	{
	  CACHE_Y_AS_ENV(YREG);
	  BEGD(d0);       /* term to be meta-called */
	  Term mod;       /* module to be used */
	  PredEntry *pen;  /* predicate */
	  choiceptr b_ptr;        /* cut point */
	  Functor f;

	  /* we are doing the rhs of a , */
	  BOp(p_execute_tail, Osbmp);

	  FETCH_Y_FROM_ENV(YREG);
	  /* place to cut to */
	  b_ptr = (choiceptr)ENV_YREG[E_CB];
	  /* original goal */
	  d0 = ENV_YREG[-EnvSizeInCells-1];
	  /* predicate we had used */
	  pen = RepPredProp((Prop)IntegerOfTerm(ENV_YREG[-EnvSizeInCells-2]));
	  /* current module at the time */
	  mod = ENV_YREG[-EnvSizeInCells-3];
	  /* set YREG */
	  /* Try to preserve the environment */
	  ENV_YREG = (CELL *) (((char *) YREG) + PREG->y_u.Osbmp.s);
#ifdef FROZEN_STACKS
	  {
	    choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
	    if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) ENV_YREG = (CELL *) top_b;
#else
	    if (ENV_YREG > (CELL *) top_b) ENV_YREG = (CELL *) top_b;
#endif /* YAPOR_SBA */
	  }
#else
	  if (ENV_YREG > (CELL *) B) {
	    ENV_YREG = (CELL *) B;
	  }
#endif /* FROZEN_STACKS */
	  /* now, jump to actual execution */
	  if (pen->ArityOfPE) {
	    f = pen->FunctorOfPred;
	    /* reuse environment if we are continuining a comma, ie, (g1,g2,g3) */
	    /* can only do it deterministically */
	    /* broken
	       if (f == FunctorComma && (CELL *)B >= ENV) {
	       ENV_YREG = ENV;
	       ENV = (CELL *)ENV[E_E];
	       }
	    */
	    goto execute_pred_f;
	  } else
	    goto execute_pred_a;
	  ENDBOp();

	  /* fetch the module from ARG2 */
	  BOp(p_execute2, Osbpp);

	  mod = ARG2;
	  deref_head(mod, execute2_unk0);
	execute2_nvar0:
	  if (!IsAtomTerm(mod)) {
	    saveregs();
	    Yap_Error(TYPE_ERROR_ATOM, mod, "call/2");
	    setregs();
	  }
	  goto start_execute;

	  BEGP(pt1);
	  deref_body(mod, pt1, execute2_unk0, execute2_nvar0);
	  saveregs();
	  Yap_Error(INSTANTIATION_ERROR, mod, "call/2");
	  setregs();
	  ENDP(pt1);
	  /* Oops, second argument was unbound too */
	  FAIL();
	  ENDBOp();

	  BOp(p_execute, Osbmp);
	  /* fetch the module from PREG */
	  mod = PREG->y_u.Osbmp.mod;
	start_execute:
	  /* place to cut to */
	  b_ptr = B;
	  /* we have mod, and ARG1 has the goal, let us roll */
	  /* Try to preserve the environment */
	  ENV_YREG = (CELL *) (((char *) YREG) + PREG->y_u.Osbmp.s);
#ifdef FROZEN_STACKS
	  {
	    choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
	    if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) ENV_YREG = (CELL *) top_b;
#else
	    if (ENV_YREG > (CELL *) top_b) ENV_YREG = (CELL *) top_b;
#endif /* YAPOR_SBA */
	  }
#else
	  if (ENV_YREG > (CELL *) B) {
	    ENV_YREG = (CELL *) B;
	  }
#endif /* FROZEN_STACKS */
	  d0 = ARG1;
	  if (PRED_GOAL_EXPANSION_ALL) {
	    goto execute_metacall;
	  }
	restart_execute:
	  deref_head(d0, execute_unk);
	execute_nvar:
	  if (IsApplTerm(d0)) {
	    f = FunctorOfTerm(d0);
	    if (IsExtensionFunctor(f)) {
	      goto execute_metacall;
	    }
	    pen = RepPredProp(PredPropByFunc(f, mod));
	  execute_pred_f:
	    if (pen->PredFlags & (MetaPredFlag|GoalExPredFlag)) {
	      /* just strip all of M:G */
	      if (f == FunctorModule) {
		Term tmod = ArgOfTerm(1,d0);
		/* loop on modules */
		if (!IsVarTerm(tmod) && IsAtomTerm(tmod)) {
		  d0 = ArgOfTerm(2,d0);
		  mod = tmod;
		  goto execute_nvar;
		}
		goto execute_metacall;
	      }
	      if (f == FunctorComma) {
		Term nmod = mod;

		/* optimise conj */
		SREG = RepAppl(d0);
		BEGD(d1);
		d1 = SREG[2];
		/* create an environment to execute the call */
		deref_head(d1, execute_comma_unk);
	      execute_comma_nvar:
		if (IsAtomTerm(d1)) {
		  /* atomic goal is simpler */
		  ENV_YREG[-EnvSizeInCells-2]  = MkIntegerTerm((Int)PredPropByAtom(AtomOfTerm(d1),nmod));
		} else if (IsApplTerm(d1)) {
		  Functor f1 = FunctorOfTerm(d1);
		  if (IsExtensionFunctor(f1)) {
		    goto execute_metacall;
		  } else {
		    /* check for modules when looking up */
		    if (f1 == FunctorModule) {
		      Term tmod = ArgOfTerm(1,d1);
		      /* loop on modules */
		      if (!IsVarTerm(tmod) && IsAtomTerm(tmod)) {
			d1 = ArgOfTerm(2,d1);
			nmod = tmod;
			goto execute_comma_nvar;
		      }
		      goto execute_metacall;
		    }
		    ENV_YREG[-EnvSizeInCells-2]  = MkIntegerTerm((Int)PredPropByFunc(f1,nmod));
		  }
		} else {
		  goto execute_metacall;
		}
		ENV_YREG[-EnvSizeInCells-3]  = mod;
		/* now, we can create the new environment for the meta-call */
		/* notice that we are at a call, so we should ignore CP */
		ENV_YREG[E_CP] = (CELL)NEXTOP(PREG,Osbmp);
		ENV_YREG[E_CB] = (CELL)b_ptr;
		ENV_YREG[E_E]  = (CELL)ENV;
#ifdef DEPTH_LIMIT
		ENV_YREG[E_DEPTH] = DEPTH;
#endif  /* DEPTH_LIMIT */
		ENV_YREG[-EnvSizeInCells-1]  = d1;
		ENV = ENV_YREG;
		ENV_YREG -= EnvSizeInCells+3;
		CPREG = NEXTOP(PREG, Osbmp);
		PREG = COMMA_CODE;
		/* for profiler */
		save_pc();
		d0 = SREG[1];
		goto restart_execute;

		BEGP(pt1);
		deref_body(d1, pt1, execute_comma_unk, execute_comma_nvar);
		goto execute_metacall;
		ENDP(pt1);
		ENDD(d1);
	      } else if (mod != CurrentModule) {
		goto execute_metacall;
	      }
	    }

	    /* copy arguments of meta-call to XREGS */
	    BEGP(pt1);
	    pt1 = RepAppl(d0);
	    BEGD(d2);
	    for (d2 = ArityOfFunctor(f); d2; d2--) {
#ifdef YAPOR_SBA
	      BEGD(d1);
	      d1 = pt1[d2];
	      if (d1 == 0) {
		XREGS[d2] = (CELL)(pt1+d2);
	      } else {
		XREGS[d2] = d1;
	      }
#else
	      XREGS[d2] = pt1[d2];
#endif
	    }
	    ENDD(d2);
	    ENDP(pt1);
	    CACHE_A1();
	  } else if (IsAtomTerm(d0)) {
	    pen = RepPredProp(PredPropByAtom(AtomOfTerm(d0), mod));
	  execute_pred_a:
	    /* handle extra pruning */
	    if (pen->FunctorOfPred == (Functor)AtomCut) {
	      if (b_ptr != B) {
		saveregs();
		prune(b_ptr PASS_REGS);
		setregs();
	      }
	    }
	  } else {
	    goto execute_metacall;
	  }

	  /* execute, but test first for interrupts */
	execute_end:
	  /* code copied from call */
#ifndef NO_CHECKING
	  check_stack(NoStackPExecute, HR);
#endif
	execute_stack_checked:
	  CPREG = NEXTOP(PREG, Osbmp);
	  ALWAYS_LOOKAHEAD(pen->OpcodeOfPred);
	  PREG = pen->CodeOfPred;
	  /* for profiler */
	  save_pc();
#ifdef DEPTH_LIMIT
	  if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is primitives */
	    if (pen->ModuleOfPred) {
	      if (DEPTH == MkIntTerm(0))
		FAIL();
	      else DEPTH = RESET_DEPTH();
	    }
	  } else if (pen->ModuleOfPred)
	    DEPTH -= MkIntConstant(2);
#endif  /* DEPTH_LIMIT */
#ifdef LOW_LEVEL_TRACER
	  if (Yap_do_low_level_trace)
	    low_level_trace(enter_pred,pen,XREGS+1);
#endif  /* LOW_LEVEL_TRACER */
	  WRITEBACK_Y_AS_ENV();
	  /* setup GB */
	  ENV_YREG[E_CB] = (CELL) B;
#ifdef YAPOR
	  SCH_check_requests();
#endif  /* YAPOR */
	  CACHE_A1();
	  ALWAYS_GONext();
	  ALWAYS_END_PREFETCH();

	  /* meta-call: Prolog to the rescue */
	  BEGP(pt1);
	  deref_body(d0, pt1, execute_unk, execute_nvar);
	execute_metacall:
	  ARG1 = ARG3 = d0;
	  pen = PredMetaCall;
	  ARG2 = Yap_cp_as_integer(b_ptr);
	  if (mod)
	    ARG4 = mod;
	  else
	    ARG4 = TermProlog;
	  goto execute_end;
	  ENDP(pt1);

	  /* at this point, we have the arguments all set in the argument registers, pen says who is the current predicate. don't remove. */
	NoStackPExecute:
	  WRITEBACK_Y_AS_ENV();
#ifdef SHADOW_S
	  Yap_REGS.S_ = SREG;
#endif
	  saveregs_and_ycache();
	  d0 = interrupt_pexecute( pen PASS_REGS );
	  setregs_and_ycache();
#ifdef SHADOW_S
	  SREG = Yap_REGS.S_;
#endif
	  if (!d0) FAIL();
	  if (d0 == 2) goto execute_stack_checked;
	  goto execute_end;
	  ENDBOp();

	  ENDD(d0);
	  ENDCACHE_Y_AS_ENV();
	}
