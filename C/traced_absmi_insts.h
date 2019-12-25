#ifndef THREADS
#define PASS_REGS
#else
#define PASS_REGS , regcache
#endif

#define undef_goal( x )

if (P == NULL) goto fail;
NoStackCut:
if (P == NULL) goto NoStackCut;
NoStackCommitY:
if (P == NULL) goto NoStackCommitY;
NoStackCutT:
if (P== NULL) goto NoStackCutT;
NoStackCutE:
if (P== NULL) goto NoStackCutE;
NoStackEither:
if (P== NULL) goto NoStackEither;
NoStackExecute:
if (P== NULL) goto NoStackExecute;
NoStackCall:
if (P== NULL) goto NoStackCall;
NoStackDExecute:
if (P== NULL) goto NoStackDExecute;
NoStackDeallocate:
if (P== NULL) goto NoStackDeallocate;
NoStackFail:
if (P== NULL) goto NoStackFail;
NoStackCommitX:
if (P== NULL) goto NoStackCommitX;

/*****************************************************************
*                     TRACED INSTRUCTIONS                        *
*****************************************************************/

      BOp(Ystop, l);
      goto _Ystop;
      ENDBOp();

      BOp(Nstop, e);
      goto _Nstop;
      ENDBOp();

/*****************************************************************
*        Plain try - retry - trust instructions                  *
*****************************************************************/
      /* try_me    Label,NArgs */
      Op(try_me, Otapl);
      /* check if enough space between trail and codespace */
      EMIT_ENTRY_BLOCK(PREG,TRY_ME_INSTINIT);
      check_trail(TR);
      /* I use YREG =to go through the choicepoint. Usually YREG =is in a
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
      EMIT_SIMPLE_BLOCK_TEST(TRY_ME_YAPOR);
      SCH_set_load(B_YREG);
#endif	/* YAPOR */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(TRY_ME_END);
      PREG = NEXTOP(PREG, Otapl);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      GONext();
      ENDOp();

      /* retry_me    Label,NArgs */
      Op(retry_me, OBtapl);
      EMIT_ENTRY_BLOCK(PREG,RETRY_ME_INSTINIT);
      CACHE_Y(B);
      /* After retry, cut should be pointing at the parent
       * choicepoint for the current B */
      restore_yaam_regs(PREG->y_u.Otapl.d);
      restore_at_least_one_arg(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
      EMIT_SIMPLE_BLOCK_TEST(RETRY_ME_FROZEN);
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
      EMIT_SIMPLE_BLOCK_TEST(RETRY_ME_NOFROZEN);
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
      EMIT_SIMPLE_BLOCK_TEST(RETRY_ME_END);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, Otapl);
      GONext();
      ENDOp();

      /* trust_me    UnusedLabel,NArgs */
      Op(trust_me, Otapl);
      EMIT_ENTRY_BLOCK(PREG,TRUST_ME_INSTINIT);
      CACHE_Y(B);
      EMIT_SIMPLE_BLOCK_TEST(TRUST_ME_IF);
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
#endif	/* YAPOR */
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
      EMIT_SIMPLE_BLOCK_TEST(TRUST_ME_END);
      PREG = NEXTOP(PREG, Otapl);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      GONext();
      ENDOp();

/*****************************************************************
*        Profiled try - retry - trust instructions               *
*****************************************************************/

      /* profiled_enter_me    Pred */
      Op(enter_profiling, p);
      EMIT_ENTRY_BLOCK(PREG,ENTER_PROFILING_INSTINIT);
      LOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      PREG->y_u.p.p->StatisticsForPred->NOfEntries++;
      UNLOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      PREG = NEXTOP(PREG, p);
      GONext();
      ENDOp();

     /* profiled_retry    Label,NArgs */
      Op(retry_profiled, p);
      EMIT_ENTRY_BLOCK(PREG,RETRY_PROFILED_INSTINIT);
      LOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      PREG->y_u.p.p->StatisticsForPred->NOfRetries++;
      UNLOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      PREG = NEXTOP(PREG, p);
      GONext();
      ENDOp();

      /* profiled_retry_me    Label,NArgs */
      Op(profiled_retry_me, Otapl);
      EMIT_ENTRY_BLOCK(PREG,PROFILED_RETRY_ME_INSTINIT);
      CACHE_Y(B);
      /* After retry, cut should be pointing at the parent
       * choicepoint for the current B */
      LOCK(PREG->y_u.Otapl.p->StatisticsForPred->lock);
      PREG->y_u.Otapl.p->StatisticsForPred->NOfRetries++;
      UNLOCK(PREG->y_u.Otapl.p->StatisticsForPred->lock);
      restore_yaam_regs(PREG->y_u.Otapl.d);
      restore_args(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
      EMIT_SIMPLE_BLOCK(PROFILED_RETRY_ME_FROZEN);
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
      EMIT_SIMPLE_BLOCK(PROFILED_RETRY_ME_NOFROZEN);
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
      EMIT_SIMPLE_BLOCK(PROFILED_RETRY_ME_END);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, Otapl);
      GONext();
      ENDOp();

      /* profiled_trust_me    UnusedLabel,NArgs */
      Op(profiled_trust_me, Otapl);
      EMIT_ENTRY_BLOCK(PREG,PROFILED_TRUST_ME_INSTINIT);
      CACHE_Y(B);
      EMIT_SIMPLE_BLOCK(PROFILED_TRUST_ME_IF);
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
#endif	/* YAPOR */
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
      EMIT_SIMPLE_BLOCK(PROFILED_TRUST_ME_END);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      LOCK(PREG->y_u.Otapl.p->StatisticsForPred->lock);
      PREG->y_u.Otapl.p->StatisticsForPred->NOfRetries++;
      UNLOCK(PREG->y_u.Otapl.p->StatisticsForPred->lock);
      PREG = NEXTOP(PREG, Otapl);
      GONext();
      ENDOp();

      BOp(profiled_retry_logical, OtaLl);
      EMIT_ENTRY_BLOCK(PREG,PROFILED_RETRY_LOGICAL_INSTINIT);
      check_trail(TR);
      {
	UInt timestamp;
	CACHE_Y(B);

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
      EMIT_SIMPLE_BLOCK(PROFILED_RETRY_LOGICAL_THREADS);
	PP = PREG->y_u.OtaLl.d->ClPred;
#endif
      EMIT_SIMPLE_BLOCK(PROFILED_RETRY_LOGICAL_POST_THREADS);
	PREG = PREG->y_u.OtaLl.d->ClCode;
#ifdef FROZEN_STACKS
      EMIT_SIMPLE_BLOCK(PROFILED_RETRY_LOGICAL_FROZEN);
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
	set_cut(S_YREG, B->cp_b);
#else
      EMIT_SIMPLE_BLOCK(PROFILED_RETRY_LOGICAL_NOFROZEN);
	set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
      EMIT_SIMPLE_BLOCK(PROFILED_RETRY_LOGICAL_END);
	SET_BB(B_YREG);
	ENDCACHE_Y();
      }
      JMPNext();
      ENDBOp();

      BOp(profiled_trust_logical, OtILl);
      EMIT_ENTRY_BLOCK(PREG,PROFILED_TRUST_LOGICAL_INSTINIT);
	  EMIT_SIMPLE_BLOCK(PROFILED_TRUST_LOGICAL_END);
      CACHE_Y(B);
      {
	LogUpdIndex *cl = PREG->y_u.OtILl.block;
	PredEntry *ap = cl->ClPred;
	LogUpdClause *lcl = PREG->y_u.OtILl.d;
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]);

	if (!VALID_TIMESTAMP(timestamp, lcl)) {
	  /* jump to next alternative */
	  PREG = FAILCODE;
	} else {
	  LOCK(ap->StatisticsForPred->lock);
	  ap->StatisticsForPred->NOfRetries++;
	  UNLOCK(ap->StatisticsForPred->lock);
	  PREG = lcl->ClCode;
	}
	/* HREY, leave indexing block alone!! */
	/* check if we are the ones using this code */
#if MULTIPLE_STACKS
	PELOCK(1, ap);
	PP = ap;
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
	      /* make sure we don't erase the clause we are jumping too */
	      if (lcl->ClRefCount == 1 && !(lcl->ClFlags & InUseMask)) {
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
#endif	/* YAPOR */
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
      EMIT_ENTRY_BLOCK(PREG,COUNT_CALL_INSTINIT);
      LOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      PREG->y_u.p.p->StatisticsForPred->NOfEntries++;
      UNLOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      LOCAL_ReductionsCounter--;
      EMIT_SIMPLE_BLOCK(COUNT_CALL_MIDDLE);
      if (LOCAL_ReductionsCounter == 0 && LOCAL_ReductionsCounterOn) {
	saveregs();
	Yap_NilError(CALL_COUNTER_UNDERFLOW,"");
	setregs();
	JMPNext();
      }
      LOCAL_PredEntriesCounter--;
      if (LOCAL_PredEntriesCounter == 0 && LOCAL_PredEntriesCounterOn) {
	saveregs();
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW,"");
	setregs();
	JMPNext();
      }
      EMIT_SIMPLE_BLOCK(COUNT_CALL_END);
      PREG = NEXTOP(PREG, p);
      GONext();
      ENDOp();

      /* count_retry    Label,NArgs */
      Op(count_retry, p);
      EMIT_ENTRY_BLOCK(PREG,COUNT_RETRY_INSTINIT);
      LOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      PREG->y_u.p.p->StatisticsForPred->NOfRetries++;
      UNLOCK(PREG->y_u.p.p->StatisticsForPred->lock);
      LOCAL_RetriesCounter--;
      EMIT_SIMPLE_BLOCK(COUNT_RETRY_MIDDLE);
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
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW,"");
	setregs();
	JMPNext();
      }
      EMIT_SIMPLE_BLOCK(COUNT_RETRY_END);
      PREG = NEXTOP(PREG, p);
      GONext();
      ENDOp();

      /* count_retry_me    Label,NArgs */
      Op(count_retry_me, Otapl);
      EMIT_ENTRY_BLOCK(PREG,COUNT_RETRY_ME_INSTINIT);
      CACHE_Y(B);
      restore_yaam_regs(PREG->y_u.Otapl.d);
      restore_args(PREG->y_u.Otapl.s);
      /* After retry, cut should be pointing at the parent
       * choicepoint for the current B */
      EMIT_SIMPLE_BLOCK(COUNT_RETRY_ME_MIDDLE);
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
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW,"");
	setregs();
	JMPNext();
      }
      EMIT_SIMPLE_BLOCK(COUNT_RETRY_ME_END);
      PREG = NEXTOP(PREG, Otapl);
      GONext();
      ENDOp();

      /* count_trust_me    UnusedLabel,NArgs */
      Op(count_trust_me, Otapl);
      EMIT_ENTRY_BLOCK(PREG,COUNT_TRUST_ME_INSTINIT);
      CACHE_Y(B);
      EMIT_SIMPLE_BLOCK(COUNT_TRUST_ME_MIDDLE);
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
#endif	/* YAPOR */
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
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW,"");
	setregs();
	JMPNext();
      }
      EMIT_SIMPLE_BLOCK(COUNT_TRUST_ME_END);
      LOCK(((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->lock);
      ((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->NOfRetries++;
      UNLOCK(((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->lock);
      PREG = NEXTOP(PREG, Otapl);
      GONext();
      ENDOp();

      BOp(count_retry_logical, OtaLl);
      EMIT_ENTRY_BLOCK(PREG,COUNT_RETRY_LOGICAL_INSTINIT);
	  EMIT_SIMPLE_BLOCK(COUNT_RETRY_LOGICAL_END);
      check_trail(TR);
      {
	UInt timestamp;
	CACHE_Y(B);

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
	  Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW,"");
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
      EMIT_ENTRY_BLOCK(PREG,COUNT_TRUST_LOGICAL_INSTINIT);
	  EMIT_SIMPLE_BLOCK(COUNT_TRUST_LOGICAL_END);
      CACHE_Y(B);
      {
	LogUpdIndex *cl = PREG->y_u.OtILl.block;
	PredEntry *ap = cl->ClPred;
	LogUpdClause *lcl = PREG->y_u.OtILl.d;
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]);

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
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW,"");
	    setregs();
	    JMPNext();
	  }
	  LOCK(ap->StatisticsForPred->lock);
	  ap->StatisticsForPred->NOfRetries++;
	  UNLOCK(ap->StatisticsForPred->lock);
	  PREG = lcl->ClCode;
	}
	/* HREY, leave indexing block alone!! */
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
	      if (lcl->ClRefCount == 1 && !(lcl->ClFlags & InUseMask)) {
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
#endif	/* YAPOR */
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
      EMIT_SIMPLE_BLOCK(LOCK_LU_INSTINIT);
	  EMIT_SIMPLE_BLOCK(LOCK_LU_END);
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
      EMIT_SIMPLE_BLOCK(UNLOCK_LU_INSTINIT);
#if defined(YAPOR) || defined(THREADS)
      EMIT_SIMPLE_BLOCK(UNLOCK_LU_YAPOR_THREADS);
      UNLOCKPE(1,PP);
      PP = NULL;
#endif
      EMIT_SIMPLE_BLOCK(UNLOCK_LU_END);
      PREG = NEXTOP(PREG, e);
      GONext();
      ENDOp();


      /* enter logical pred               */
      BOp(alloc_for_logical_pred, L);
      EMIT_ENTRY_BLOCK(PREG,ALLOC_FOR_LOGICAL_PRED_INSTINIT);
      EMIT_SIMPLE_BLOCK(YAAM_CHECK_TRAIL_TR);
      check_trail(TR);
      /* say that an environment is using this clause */
      /* we have our own copy for the clause */
#if MULTIPLE_STACKS
      {
      EMIT_SIMPLE_BLOCK(ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS);
	LogUpdClause *cl = PREG->y_u.L.ClBase;
#if PARALLEL_YAP
      EMIT_SIMPLE_BLOCK(ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS_PARALLEL);
	PredEntry *ap = cl->ClPred;
#endif
      EMIT_SIMPLE_BLOCK(ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS_END);
	/* always add an extra reference */
	INC_CLREF_COUNT(cl);
	TRAIL_CLREF(cl);
	UNLOCKPE(2,ap);
	PP = NULL;
      }
#else
      {
      EMIT_SIMPLE_BLOCK(ALLOC_FOR_LOGICAL_PRED_NOMULTIPLE_STACKS_INIT);
	LogUpdClause *cl = (LogUpdClause *)PREG->y_u.L.ClBase;
      EMIT_SIMPLE_BLOCK(ALLOC_FOR_LOGICAL_PRED_NOMULTIPLE_STACKS_IF);
	if (!(cl->ClFlags & InUseMask)) {
	  cl->ClFlags |= InUseMask;
	  TRAIL_CLREF(cl);
	}
      }
#endif
      EMIT_SIMPLE_BLOCK(ALLOC_FOR_LOGICAL_PRED_END);
      PREG = NEXTOP(PREG, L);
      JMPNext();
      ENDBOp();

      /* copy database term               */
      BOp(copy_idb_term, e);
      EMIT_ENTRY_BLOCK(PREG,COPY_IDB_TERM_INSTINIT);
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(COPY_IDB_TERM_END);
      {
	LogUpdClause *cl = ClauseCodeToLogUpdClause(PREG);
	Term t;

	SET_ASP(YREG, E_CB*sizeof(CELL));
	saveregs();
	while ((t = Yap_FetchTermFromDB(cl->lusl.ClSource)) == 0L) {
	  if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
	    LOCAL_Error_TYPE = YAP_NO_ERROR;
	    if (!Yap_growglobal(NULL)) {
	      UNLOCKPE(3,PP);
#if defined(YAPOR) || defined(THREADS)
	      PP = NULL;
#endif
	      Yap_NilError(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, LOCAL_ErrorMessage);
	      FAIL();
	    }
	  } else {
	    LOCAL_Error_TYPE = YAP_NO_ERROR;
	    if (!Yap_gc(3, ENV, CP)) {
	      UNLOCKPE(4,PP);
#if defined(YAPOR) || defined(THREADS)
	      PP = NULL;
#endif
	      Yap_NilError(RESOURCE_ERROR_STACK, LOCAL_ErrorMessage);
	      FAIL();
	    }
	  }
	}
	if (!Yap_IUnify(ARG2, t)) {
	  setregs();
	  UNLOCKPE(5,PP);
#if defined(YAPOR) || defined(THREADS)
	  PP = NULL;
#endif
	  FAIL();
	}
	if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) {
	  setregs();
	  UNLOCKPE(6,PP);
#if defined(YAPOR) || defined(THREADS)
	  PP = NULL;
#endif
	  FAIL();
	}
	setregs();

#if MULTIPLE_STACKS
	/* always add an extra reference */
	INC_CLREF_COUNT(cl);
	TRAIL_CLREF(cl);
	UNLOCKPE(7,PP);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_IDB_TERM_INSTINIT);
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_IDB_TERM_END);
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
*        check for enough room			                 *
*****************************************************************/

      /* ensure_space                   */
      BOp(ensure_space, Osbpa);
      EMIT_ENTRY_BLOCK(PREG,ENSURE_SPACE_INSTINIT);
	  EMIT_SIMPLE_BLOCK(ENSURE_SPACE_END);
      {
	Int sz =  PREG->y_u.Osbpa.i;
	UInt arity = PREG->y_u.Osbpa.p->ArityOfPE;
	if (Unsigned(HR) + sz > Unsigned(YREG)-CreepFlag) {
	  YENV[E_CP] = (CELL) CPREG;
	  YENV[E_E] = (CELL) ENV;
#ifdef DEPTH_LIMIT
	  YENV[E_DEPTH] = DEPTH;
#endif	/* DEPTH_LIMIT */
	  SET_ASP(YREG, PREG->y_u.Osbpa.s);
	  PREG = NEXTOP(PREG,Osbpa);
	  saveregs();
	  if (!Yap_gcl(sz, arity, YENV, PREG)) {
	    Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage);
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
      EMIT_ENTRY_BLOCK(PREG,SPY_OR_TRYMARK_INSTINIT);
      PELOCK(5, ((PredEntry *)(PREG->y_u.Otapl.p)));
      PREG = (yamop *)(&(((PredEntry *)(PREG->y_u.Otapl.p))->OpcodeOfPred));
      UNLOCKPE(11,(PredEntry *)(PREG->y_u.Otapl.p));
      goto dospy;
      ENDBOp();

      /* try_and_mark   Label,NArgs       */
      BOp(try_and_mark, Otapl);
      EMIT_ENTRY_BLOCK(PREG,TRY_AND_MARK_INSTINIT);
      check_trail(TR);
#if defined(YAPOR) || defined(THREADS)
#ifdef YAPOR
      /* The flags I check here should never change during execution */
      EMIT_SIMPLE_BLOCK(TRY_AND_MARK_YAPOR_THREADS_YAPOR);
      CUT_wait_leftmost();
#endif /* YAPOR */
      EMIT_SIMPLE_BLOCK(TRY_AND_MARK_YAPOR_THREADS_NOYAPOR_IF);
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
      EMIT_SIMPLE_BLOCK(TRY_AND_MARK_NOYAPOR_NOTHREADS);
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
      EMIT_SIMPLE_BLOCK(TRY_AND_MARK_SET_LOAD);
      SCH_set_load(B_YREG);
#endif	/* YAPOR */
      EMIT_SIMPLE_BLOCK(TRY_AND_MARK_POST_SET_LOAD);
      SET_BB(B_YREG);
      ENDCACHE_Y();
#if MULTIPLE_STACKS
      EMIT_SIMPLE_BLOCK(TRY_AND_MARK_MULTIPLE_STACKS);
      INC_CLREF_COUNT(ClauseCodeToDynamicClause(PREG));
      UNLOCK(DynamicLock(PREG));
      TRAIL_CLREF(ClauseCodeToDynamicClause(PREG));
#else
      EMIT_SIMPLE_BLOCK(TRY_AND_MARK_NOMULTIPLE_STACKS_IF);
      if (FlagOff(InUseMask, DynamicFlags(PREG))) {
	SetFlag(InUseMask, DynamicFlags(PREG));
	TRAIL_CLREF(ClauseCodeToDynamicClause(PREG));
      }
#endif
      EMIT_SIMPLE_BLOCK(TRY_AND_MARK_END);
      PREG = NEXTOP(PREG,Otapl);
      JMPNext();

      ENDBOp();

      BOp(count_retry_and_mark, Otapl);
      EMIT_ENTRY_BLOCK(PREG,COUNT_RETRY_AND_MARK_INSTINIT);
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
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW,"");
	setregs();
	JMPNext();
      }
      /* enter a retry dynamic */
      ENDBOp();

      BOp(profiled_retry_and_mark, Otapl);
      EMIT_ENTRY_BLOCK(PREG,PROFILED_RETRY_AND_MARK_INSTINIT);
      LOCK(((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->lock);
      ((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->NOfRetries++;
      UNLOCK(((PredEntry *)(PREG->y_u.Otapl.p))->StatisticsForPred->lock);
      /* enter a retry dynamic */
      ENDBOp();

      /* retry_and_mark   Label,NArgs     */
      BOp(retry_and_mark, Otapl);
      EMIT_ENTRY_BLOCK(PREG,RETRY_AND_MARK_INSTINIT);
#ifdef YAPOR
      EMIT_SIMPLE_BLOCK(RETRY_AND_MARK_YAPOR);
      CUT_wait_leftmost();
#endif /* YAPOR */
      /* need to make the DB stable until I get the new clause */
      EMIT_SIMPLE_BLOCK(RETRY_AND_MARK_POST_YAPOR);
      PELOCK(7,PREG->y_u.Otapl.p);
      CACHE_Y(B);
      PREG = PREG->y_u.Otapl.d;
      LOCK(DynamicLock(PREG));
      UNLOCK(PREG->y_u.Otapl.p->PELock);
      restore_yaam_regs(PREG);
      restore_args(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
      EMIT_SIMPLE_BLOCK(RETRY_AND_MARK_FROZEN);
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
      EMIT_SIMPLE_BLOCK(RETRY_AND_MARK_NOFROZEN);
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
      EMIT_SIMPLE_BLOCK(RETRY_AND_MARK_POST_FROZEN);
      SET_BB(B_YREG);
      ENDCACHE_Y();
#if MULTIPLE_STACKS
      EMIT_SIMPLE_BLOCK(RETRY_AND_MARK_MULTIPLE_STACKS);
      INC_CLREF_COUNT(ClauseCodeToDynamicClause(PREG));
      TRAIL_CLREF(ClauseCodeToDynamicClause(PREG));
      UNLOCK(DynamicLock(PREG));
#else
      EMIT_SIMPLE_BLOCK(RETRY_AND_MARK_NOMULTIPLE_STACKS_IF);
      if (FlagOff(InUseMask, DynamicFlags(PREG))) {
	SetFlag(InUseMask, DynamicFlags(PREG));
	TRAIL_CLREF(ClauseCodeToDynamicClause(PREG));
      }
#endif
      EMIT_SIMPLE_BLOCK(RETRY_AND_MARK_END);
      PREG = NEXTOP(PREG, Otapl);
      JMPNext();

      ENDBOp();

/*****************************************************************
*        Failure                                                 *
*****************************************************************/

      /* trust_fail                       */
      BOp(trust_fail, e);
      EMIT_ENTRY_BLOCK(PREG,TRUST_FAIL_INSTINIT);
#ifdef CUT_C
      {
      EMIT_SIMPLE_BLOCK(TRUST_FAIL_CUT_C);
	while (POP_CHOICE_POINT(B->cp_b))
	  {
	    POP_EXECUTE();
	  }
      }
#endif /* CUT_C */
#ifdef YAPOR
      {
      EMIT_SIMPLE_BLOCK(TRUST_FAIL_YAPOR);
	choiceptr cut_pt;
	cut_pt = B->cp_b;
	CUT_prune_to(cut_pt);
	B = cut_pt;
      }
#else
      EMIT_SIMPLE_BLOCK(TRUST_FAIL_NOYAPOR);
      B = B->cp_b;
#endif	/* YAPOR */
      goto fail;
      ENDBOp();

#ifdef YAPOR
    shared_fail:
      EMIT_SIMPLE_BLOCK(LBL_SHARED_FAIL);
      B = Get_LOCAL_top_cp();
      SET_BB(PROTECT_FROZEN_B(B));
      goto fail;
#endif	/* YAPOR */

      /* fail                             */
      PBOp(op_fail, e);
      EMIT_ENTRY_BLOCK(PREG,OP_FAIL_INSTINIT);
      if (PP) {
	UNLOCK(PP->PELock);
	PP = NULL;
      }
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
	  EMIT_SIMPLE_BLOCK_TEST(NoStackFail_Exception);
      check_stack(NoStackFail, HR);
      ENDCACHE_Y_AS_ENV();
#endif

    fail:
      {
      EMIT_ENTRY_BLOCK(PREG,LBL_FAIL_INSTINIT);
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
	    int go_on = TRUE;
	    yamop *ipc = PREG;

	    while (go_on) {
	      op_numbers opnum = Yap_op_from_opcode(ipc->opc);

	      go_on = FALSE;
	      switch (opnum) {
#ifdef TABLING
	      case _table_load_answer:
		  {
		low_level_trace(retry_table_loader, LOAD_CP(B)->cp_pred_entry, NULL);
		break;
		  }
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
		if (IS_DET_GEN_CP(B)) {
		  low_level_trace(retry_table_generator, DET_GEN_CP(B)->cp_pred_entry, NULL);
		} else
#endif /* DETERMINISTIC_TABLING */
        {
		  low_level_trace(retry_table_generator, GEN_CP(B)->cp_pred_entry, (CELL *)(GEN_CP(B) + 1));
		}
		break;
	      case _table_answer_resolution:
		{
		low_level_trace(retry_table_consumer, CONS_CP(B)->cp_pred_entry, NULL);
		break;
		}
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
	    {
		low_level_trace(retry_table_loader, UndefCode, NULL);
		break;
		}
#endif /* TABLING */
	      case _or_else:
	      case _or_last:
		{
		low_level_trace(retry_or, (PredEntry *)ipc, &(B->cp_a1));
		break;
		}
	      case _retry2:
	      case _retry3:
	      case _retry4:
		{
		ipc = NEXTOP(ipc,l);
		go_on = TRUE;
		break;
		}
	      case _jump:
		{
		ipc = ipc->y_u.l.l;
		go_on = TRUE;
		break;
		}
	      case _retry_c:
	      case _retry_userc:
		{
		low_level_trace(retry_pred, ipc->y_u.OtapFs.p, B->cp_args);
		break;
		}
	      case _retry_profiled:
	      case _count_retry:
		{
		ipc = NEXTOP(ipc,p);
		go_on = TRUE;
		break;
		}
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
		{
		low_level_trace(retry_pred, ipc->y_u.Otapl.p, B->cp_args);
		break;
		}
	      case _try_logical:
	      case _retry_logical:
	      case _profiled_retry_logical:
	      case _count_retry_logical:
	      case _trust_logical:
	      case _profiled_trust_logical:
	      case _count_trust_logical:
		{
		low_level_trace(retry_pred, ipc->y_u.OtILl.d->ClPred, B->cp_args);
		break;
		}
	      case _Nstop:
	      case _Ystop:
		{
		low_level_trace(retry_pred, NULL, B->cp_args);
		break;
		}
	      default:
		break;
	      }
	    }
	  }
#endif	/* LOW_LEVEL_TRACER */
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
        {
      RESTORE_TR();
        }
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(LBL_FAIL_END);
      GONext();
	}
	BEGD(d1);
	d1 = TrailTerm(pt0-1);
	pt0--;
	if (IsVarTerm(d1)) {
#if defined(YAPOR_SBA) && defined(YAPOR)
	  if (Unsigned((Int)(d1)-(Int)(H_FZ)) >
	      Unsigned((Int)(B_FZ)-(Int)(H_FZ))) {
	    RESET_VARIABLE(STACK_TO_SBA(d1));
	  } else
#endif
      {
         /* normal variable */
	    RESET_VARIABLE(d1);
      }
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
		}
	      }
#ifdef FROZEN_STACKS  /* TRAIL */
	    if (pt0 < TR_FZ) {
		  goto failloop;
	    }
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
		    Yap_ErLogUpdIndex(cl);
		    setregs();
		  } else if (cl->ClFlags & DirtyMask) {
		    saveregs();
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
		  PELOCK(9,ap);
		  DEC_CLREF_COUNT(cl);
		  erase = (cl->ClFlags & ErasedMask) && !(cl->ClRefCount);
		  if (erase) {
		    saveregs();
		    Yap_ErLogUpdCl(cl);
		    setregs();
		  }
		  UNLOCK(ap->PELock);
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
      ENDPBOp();



/************************************************************************\
*	Cut & Commit Instructions					*
\************************************************************************/

      /* cut                              */
Op(cut, s);
      EMIT_ENTRY_BLOCK(PREG,CUT_INSTINIT);
#ifdef COROUTINING
      EMIT_SIMPLE_BLOCK_TEST(CUT_COROUTINING);
      if (FALSE) {
	CACHE_Y_AS_ENV(YREG);
	EMIT_SIMPLE_BLOCK_TEST(NoStackCut_Exception);
	check_stack(NoStackCut, HR);
	ENDCACHE_Y_AS_ENV();
      }
#endif
      EMIT_SIMPLE_BLOCK_TEST(CUT_NOCOROUTINING);
      SET_ASP(YREG, PREG->y_u.s.s);
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, s),Osbpp),l);
      /* assume cut is always in stack */
      saveregs();
      prune((choiceptr)YREG[E_CB] PASS_REGS);
      setregs();
      GONext();
      ENDOp();

      /* cut_t                            */
      /* cut_t does the same as cut */
      Op(cut_t, s);
      EMIT_ENTRY_BLOCK(PREG,CUT_T_INSTINIT);
#ifdef COROUTINING
      EMIT_SIMPLE_BLOCK_TEST(CUT_T_COROUTINING);
      if (FALSE) {
	CACHE_Y_AS_ENV(YREG);
	EMIT_SIMPLE_BLOCK_TEST(NoStackCutT_Exception);
	check_stack(NoStackCutT, HR);
	ENDCACHE_Y_AS_ENV();
      }
#endif
      EMIT_SIMPLE_BLOCK_TEST(CUT_T_NOCOROUTINING);
      SET_ASP(YREG, PREG->y_u.s.s);
      /* assume cut is always in stack */
      saveregs();
    prune((choiceptr)YREG[E_CB] PASS_REGS);
      setregs();
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, s),Osbpp),l);
      GONext();
      ENDOp();

      /* cut_e                            */
      Op(cut_e, s);
      EMIT_ENTRY_BLOCK(PREG,CUT_E_INSTINIT);
#ifdef COROUTINING
      EMIT_SIMPLE_BLOCK_TEST(CUT_E_COROUTINING);
      if (FALSE) {
	CACHE_Y_AS_ENV(YREG);
	EMIT_SIMPLE_BLOCK_TEST(NoStackCutE_Exception);
	check_stack(NoStackCutE, HR);
	ENDCACHE_Y_AS_ENV();
      }
#endif
      EMIT_SIMPLE_BLOCK_TEST(CUT_E_NOCOROUTINING);
      SET_ASP(YREG, PREG->y_u.s.s);
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, s),Osbpp),l);
      saveregs();
      prune((choiceptr)SREG[E_CB] PASS_REGS);
      setregs();
      GONext();
      ENDOp();

      /* save_b_x      Xi                 */
      Op(save_b_x, x);
      EMIT_ENTRY_BLOCK(PREG,SAVE_B_X_INSTINIT);
      BEGD(d0);
      d0 = PREG->y_u.x.x;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
      EMIT_SIMPLE_BLOCK_TEST(SAVE_B_X_YSBA_FROZEN);
      XREG(d0) = MkIntegerTerm((Int)B);
#else
      EMIT_SIMPLE_BLOCK_TEST(SAVE_B_X_NOYSBA_NOFROZEN);
      XREG(d0) = MkIntegerTerm(LCL0-(CELL *) (B));
#endif /* YAPOR_SBA && FROZEN_STACKS */
      EMIT_SIMPLE_BLOCK_TEST(SAVE_B_X_END);
      PREG = NEXTOP(PREG, x);
      ENDD(d0);
      GONext();
      ENDOp();

      /* save_b_y      Yi                 */
      Op(save_b_y, y);
      EMIT_ENTRY_BLOCK(PREG,SAVE_B_Y_INSTINIT);
#if defined(YAPOR_SBA)
      EMIT_SIMPLE_BLOCK_TEST(SAVE_B_Y_YSBA);
      INITIALIZE_PERMVAR(YREG+PREG->y_u.y.y,MkIntegerTerm((Int)B));
#else
      EMIT_SIMPLE_BLOCK_TEST(SAVE_B_Y_NOYSBA);
      INITIALIZE_PERMVAR(YREG+PREG->y_u.y.y,MkIntegerTerm(LCL0-(CELL *)(B)));
#endif /* YAPOR_SBA*/
      EMIT_SIMPLE_BLOCK_TEST(SAVE_B_Y_END);
      PREG = NEXTOP(PREG, y);
      GONext();
      ENDOp();

      /* commit_b_x    Xi                 */
      Op(commit_b_x, xps);
      EMIT_ENTRY_BLOCK(PREG,COMMIT_B_X_INSTINIT);
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
	  EMIT_SIMPLE_BLOCK_TEST(NoStackCommitX_Exception);
      check_stack(NoStackCommitX, HR);
      ENDCACHE_Y_AS_ENV();
#endif
      EMIT_SIMPLE_BLOCK_TEST(COMMIT_B_X_DO_COMMIT_B_X);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xps.x);
      profiled_deref_head_TEST(d0, commit_b_x_unk);
    commit_b_x_nvar:
      /* skip a void call and a label */
      EMIT_SIMPLE_BLOCK_TEST(COMMIT_B_X_COMMIT_B_X_NVAR);
      SET_ASP(YREG, PREG->y_u.xps.s);
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xps),Osbpp),l);
      {
	choiceptr pt0;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
      EMIT_SIMPLE_BLOCK_TEST(COMMIT_B_X_YSBA_FROZEN);
	pt0 = (choiceptr)IntegerOfTerm(d0);
#else
      EMIT_SIMPLE_BLOCK_TEST(COMMIT_B_X_NOYSBA_NOFROZEN);
	pt0 = (choiceptr)(LCL0-IntegerOfTerm(d0));
#endif /* YAPOR_SBA && FROZEN_STACKS */
      EMIT_SIMPLE_BLOCK_TEST(COMMIT_B_X_POST_YSBA_FROZEN);
	saveregs();
	prune(pt0 PASS_REGS);
	setregs();
      }
      GONext();

      BEGP(pt1);
      profiled_deref_body(d0, pt1, commit_b_x_unk, commit_b_x_nvar);
      EMIT_SIMPLE_BLOCK_TEST(COMMIT_B_X_END);
      ENDP(pt1);
      /* never cut to a variable */
      /* Abort */
      FAIL();
      ENDD(d0);
      ENDOp();

      /* commit_b_y    Yi                 */
      Op(commit_b_y, yps);
      EMIT_ENTRY_BLOCK(PREG,COMMIT_B_Y_INSTINIT);
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
	  EMIT_SIMPLE_BLOCK_TEST(NoStackCommitY_Exception);
      check_stack(NoStackCommitY, HR);
      ENDCACHE_Y_AS_ENV();
#endif
      EMIT_SIMPLE_BLOCK_TEST(COMMIT_B_Y_DO_COMMIT_B_Y);
      BEGD(d0);
      d0 = YREG[PREG->y_u.yps.y];
      profiled_deref_head_TEST(d0, commit_b_y_unk);
    commit_b_y_nvar:
      EMIT_SIMPLE_BLOCK_TEST(COMMIT_B_Y_COMMIT_B_Y_NVAR);
      SET_ASP(YREG, PREG->y_u.yps.s);
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yps),Osbpp),l);
      {
	choiceptr pt0;
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
      EMIT_SIMPLE_BLOCK_TEST(COMMIT_B_Y_YSBA_FROZEN);
	pt0 = (choiceptr)IntegerOfTerm(d0);
#else
      EMIT_SIMPLE_BLOCK_TEST(COMMIT_B_Y_NOYSBA_NOFROZEN);
	pt0 = (choiceptr)(LCL0-IntegerOfTerm(d0));
#endif
      EMIT_SIMPLE_BLOCK_TEST(COMMIT_B_Y_POST_YSBA_FROZEN);
	saveregs();
	prune(pt0 PASS_REGS);
	setregs();
      }
      GONext();

      BEGP(pt1);
      profiled_deref_body(d0, pt1, commit_b_y_unk, commit_b_y_nvar);
      EMIT_SIMPLE_BLOCK_TEST(COMMIT_B_Y_END);
      ENDP(pt1);
      /* never cut to a variable */
      /* Abort */
      FAIL();
      ENDD(d0);
      ENDOp();

/*************************************************************************
* 	Call / Proceed instructions                                      *
*************************************************************************/

/* Macros for stack trimming                                            */

      /* execute     Label               */
      BOp(execute, pp);
      {
      EMIT_ENTRY_BLOCK(PREG,EXECUTE_INSTINIT);
	PredEntry *pt0;
	CACHE_Y_AS_ENV(YREG);
	pt0 = PREG->y_u.pp.p;
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
      EMIT_SIMPLE_BLOCK_TEST(EXECUTE_LOW_LEVEL_TRACER);
	  low_level_trace(enter_pred,pt0,XREGS+1);
	}
#endif	/* LOW_LEVEL_TRACE */
      EMIT_SIMPLE_BLOCK_TEST(EXECUTE_POST_LOW_LEVEL_TRACER);
	CACHE_A1();
	ALWAYS_LOOKAHEAD(pt0->OpcodeOfPred);
	BEGD(d0);
	d0 = (CELL)B;
#ifndef NO_CHECKING
    EMIT_SIMPLE_BLOCK_TEST(NoStackExecute_Exception);
	check_stack(NoStackExecute, HR);
#endif
      EMIT_SIMPLE_BLOCK_TEST(EXECUTE_POST_NOCHECKING);
	PREG = pt0->CodeOfPred;
	/* for profiler */
	save_pc();
	ENV_YREG[E_CB] = d0;
	ENDD(d0);
#ifdef DEPTH_LIMIT
	if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is prolog */
      EMIT_CONDITIONAL_SUCCESS("DEPTH <= MkIntTerm(1)");
      EMIT_SIMPLE_BLOCK_TEST(EXECUTE_DEPTH_MINOR);
	  if (pt0->ModuleOfPred) {
	    if (DEPTH == MkIntTerm(0)) {
          FAIL();
        }
	    else {
          DEPTH = RESET_DEPTH();
        }
	  }
	} else if (pt0->ModuleOfPred) {
      EMIT_CONDITIONAL_FAIL("DEPTH <= MkIntTerm(1)");
	  EMIT_CONDITIONAL_SUCCESS("pt0->ModuleOfPred");
      EMIT_SIMPLE_BLOCK_TEST(EXECUTE_DEPTH_MOFPRED);
	  DEPTH -= MkIntConstant(2);
    }
    else {
      EMIT_CONDITIONAL_FAIL("DEPTH <= MkIntTerm(1)");
	  EMIT_CONDITIONAL_FAIL("pt0->ModuleOfPred");
      EMIT_SIMPLE_BLOCK_TEST(EXECUTE_DEPTH_END);
    }
#endif	/* DEPTH_LIMIT */
      /* this is the equivalent to setting up the stack */
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(EXECUTE_END_END);
	ALWAYS_GONext();
	ALWAYS_END_PREFETCH();
	ENDCACHE_Y_AS_ENV();
      }
      ENDBOp();

    /*NoStackExecute:
      CHECK_ALARM(JMPNext());
      SREG = (CELL *) PREG->y_u.pp.p;
      PP = PREG->y_u.pp.p0;
      if (LOCAL_ActiveSignals & YAP_CDOVF_SIGNAL) {
	SET_ASP(YREG, E_CB*sizeof(CELL));
	SREG = YENV;
	goto noheapleft;
      }
      if (LOCAL_ActiveSignals)
	goto creep;
      else
	goto NoStackExec;*/

      /* dexecute    Label               */
      /* joint deallocate and execute */
      BOp(dexecute, pp);
      EMIT_ENTRY_BLOCK(PREG,DEXECUTE_INSTINIT);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
      EMIT_SIMPLE_BLOCK_TEST(DEXECUTE_LOW_LEVEL_TRACER);
	low_level_trace(enter_pred,PREG->y_u.pp.p,XREGS+1);
	  }
#endif	/* LOW_LEVEL_TRACER */
      EMIT_SIMPLE_BLOCK_TEST(DEXECUTE_POST_LOW_LEVEL_TRACER);
     CACHE_Y_AS_ENV(YREG);
      {
	PredEntry *pt0;

	CACHE_A1();
	pt0 = PREG->y_u.pp.p;
#ifndef NO_CHECKING
	/* check stacks */
	EMIT_SIMPLE_BLOCK_TEST(NoStackDExecute_Exception);
	check_stack(NoStackDExecute, HR);
#endif
#ifdef DEPTH_LIMIT
	if (DEPTH <= MkIntTerm(1)) { /* I assume Module==0 is primitives */
      EMIT_CONDITIONAL_SUCCESS("DEPTH <= MkIntTerm(1)");
      EMIT_SIMPLE_BLOCK_TEST(DEXECUTE_DEPTH_MINOR);
	  if (pt0->ModuleOfPred) {
	    if (DEPTH == MkIntTerm(0)) {
	      FAIL();
		}
	    else {
		  DEPTH = RESET_DEPTH();
		}
	  }
	} else if (pt0->ModuleOfPred) {
      EMIT_CONDITIONAL_FAIL("DEPTH <= MkIntTerm(1)");
	  EMIT_CONDITIONAL_SUCCESS("pt0->ModuleOfPred");
      EMIT_SIMPLE_BLOCK_TEST(DEXECUTE_DEPTH_MOFPRED);
	  DEPTH -= MkIntConstant(2);
	 }
	 else {
      EMIT_CONDITIONAL_FAIL("DEPTH <= MkIntTerm(1)");
	  EMIT_CONDITIONAL_FAIL("pt0->ModuleOfPred");
      EMIT_SIMPLE_BLOCK_TEST(DEXECUTE_DEPTH_END);
    }
#endif	/* DEPTH_LIMIT */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(DEXECUTE_END_END);
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
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) {
	    ENV_YREG = (CELL *) top_b;
      }
#else
	  if (ENV_YREG > (CELL *) top_b) {
	    ENV_YREG = (CELL *) top_b;
	  }
#endif /* YAPOR_SBA */
	  else {
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size(CPREG));
	  }
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
      ENDBOp();

      BOp(fcall, Osbpp);
      CACHE_Y_AS_ENV(YREG);
      ENV_YREG[E_CP] = (CELL) CPREG;
      ENV_YREG[E_E] = (CELL) ENV;
#ifdef DEPTH_LIMIT
      ENV_YREG[E_DEPTH] = DEPTH;
#endif	/* DEPTH_LIMIT */
      ENDCACHE_Y_AS_ENV();
      ENDBOp();

      BOp(call, Osbpp);
      EMIT_ENTRY_BLOCK(PREG,CALL_INSTINIT);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
      EMIT_SIMPLE_BLOCK_TEST(CALL_LOW_LEVEL_TRACER);
	low_level_trace(enter_pred,PREG->y_u.Osbpp.p,XREGS+1);
      }
#endif	/* LOW_LEVEL_TRACER */
      EMIT_SIMPLE_BLOCK_TEST(CALL_POST_LOW_LEVEL_TRACER);
      CACHE_Y_AS_ENV(YREG);
      {
	PredEntry *pt;
	pt = PREG->y_u.Osbpp.p;
	CACHE_A1();
#ifndef NO_CHECKING
    EMIT_SIMPLE_BLOCK_TEST(NoStackCall_Exception);
	check_stack(NoStackCall, HR);
#endif
      EMIT_SIMPLE_BLOCK_TEST(CALL_POST_NO_CHECKING);
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
      EMIT_CONDITIONAL_SUCCESS("DEPTH <= MkIntTerm(1)");
      EMIT_SIMPLE_BLOCK_TEST(CALL_DEPTH_MINOR);
	  if (pt->ModuleOfPred) {
	    if (DEPTH == MkIntTerm(0)){
	        FAIL();
	    } else {
	        DEPTH = RESET_DEPTH();
	    }
	  }
	} else if (pt->ModuleOfPred) {
      EMIT_CONDITIONAL_FAIL("DEPTH <= MkIntTerm(1)");
	  EMIT_CONDITIONAL_SUCCESS("pt->ModuleOfPred");
      EMIT_SIMPLE_BLOCK_TEST(CALL_DEPTH_MOFPRED);
	  DEPTH -= MkIntConstant(2);
	}
	else {
      EMIT_CONDITIONAL_FAIL("DEPTH <= MkIntTerm(1)");
	  EMIT_CONDITIONAL_FAIL("pt->ModuleOfPred");
      EMIT_SIMPLE_BLOCK_TEST(CALL_DEPTH_END);
    }
#endif	/* DEPTH_LIMIT */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(CALL_END_END);
#ifdef FROZEN_STACKS
	{
	  choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) {
	      ENV_YREG = (CELL *) top_b;
	  }
#else
	  if (ENV_YREG > (CELL *) top_b) {
	      ENV_YREG = (CELL *) top_b;
	  }
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
#endif	/* YAPOR */
	ALWAYS_GONext();
	ALWAYS_END_PREFETCH();
      }
      ENDCACHE_Y_AS_ENV();
      ENDBOp();

      BOp(procceed, p);
      EMIT_ENTRY_BLOCK(PREG,PROCCEED_INSTINIT);
      CACHE_Y_AS_ENV(YREG);
      ALWAYS_LOOKAHEAD(CPREG->opc);
      PREG = CPREG;
      /* for profiler */
      save_pc();
      ENV_YREG = ENV;
#ifdef DEPTH_LIMIT
      EMIT_SIMPLE_BLOCK_TEST(PROCCEED_DEPTH);
      DEPTH = ENV_YREG[E_DEPTH];
#endif
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(PROCCEED_END);
      WRITEBACK_Y_AS_ENV();
      ALWAYS_GONext();
      ALWAYS_END_PREFETCH();
      ENDCACHE_Y_AS_ENV();
      ENDBOp();

      Op(allocate, e);
      EMIT_ENTRY_BLOCK(PREG,ALLOCATE_INSTINIT);
      CACHE_Y_AS_ENV(YREG);
      PREG = NEXTOP(PREG, e);
      ENV_YREG[E_CP] = (CELL) CPREG;
      ENV_YREG[E_E] = (CELL) ENV;
#ifdef DEPTH_LIMIT
      EMIT_SIMPLE_BLOCK_TEST(ALLOCATE_DEPTH);
      ENV_YREG[E_DEPTH] = DEPTH;
#endif	/* DEPTH_LIMIT */
      EMIT_SIMPLE_BLOCK_TEST(ALLOCATE_END);
      ENV = ENV_YREG;
      ENDCACHE_Y_AS_ENV();
      GONext();
      ENDOp();

      Op(deallocate, p);
      EMIT_ENTRY_BLOCK(PREG,DEALLOCATE_INSTINIT);
      EMIT_SIMPLE_BLOCK_TEST(YAAM_CHECK_TRAIL_TR);
      check_trail(TR);
      EMIT_SIMPLE_BLOCK_TEST(DEALLOCATE_POST_CHECK);
      CACHE_Y_AS_ENV(YREG);
      PREG = NEXTOP(PREG, p);
      /* other instructions do depend on S being set by deallocate
	 :-( */
      SREG = YREG;
      CPREG = (yamop *) ENV_YREG[E_CP];
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E];
#ifdef DEPTH_LIMIT
      EMIT_SIMPLE_BLOCK_TEST(DEALLOCATE_DEPTH);
      DEPTH = ENV_YREG[E_DEPTH];
#endif	/* DEPTH_LIMIT */
      EMIT_SIMPLE_BLOCK_TEST(DEALLOCATE_FROZEN);
#ifdef FROZEN_STACKS
      {
	choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
	if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR)
	    ENV_YREG = (CELL *) top_b;
#else
	if (ENV_YREG > (CELL *) top_b)
	    ENV_YREG = (CELL *) top_b;
#endif /* YAPOR_SBA */
	else
	  ENV_YREG = (CELL *)((CELL) ENV_YREG + ENV_Size(CPREG));
      }
#else
      if (ENV_YREG > (CELL *) B)
	ENV_YREG = (CELL *) B;
      else
	ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size(CPREG));
#endif /* FROZEN_STACKS */
      EMIT_SIMPLE_BLOCK_TEST(DEALLOCATE_POST_FROZEN);
      WRITEBACK_Y_AS_ENV();
#ifndef NO_CHECKING
      /* check stacks */
	  EMIT_SIMPLE_BLOCK_TEST(NoStackDeallocate_Exception);
      check_stack(NoStackDeallocate, HR);
#endif
      EMIT_SIMPLE_BLOCK_TEST(DEALLOCATE_END);
      ENDCACHE_Y_AS_ENV();
      GONext();
      ENDOp();


#if EAM
/**********************************************
*        EAM instructions                     *
**********************************************/

     Op(retry_eam, e);
     //goto retry_eam;
{ printf("retry_eam not supported by JIT!!\n"); exit(1); }
     ENDOp();

     Op(run_eam, os);
{ printf("run_eam not supported by JIT!!\n"); exit(1); }
     ENDOp();

#endif


/************************************************************************\
*    Get Instructions							*
\************************************************************************/

      Op(get_x_var, xx);
      EMIT_ENTRY_BLOCK(PREG,GET_X_VAR_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xr);
      XREG(PREG->y_u.xx.xl) = d0;
      PREG = NEXTOP(PREG, xx);
      ENDD(d0);
      GONext();
      ENDOp();

      Op(get_y_var, yx);
      EMIT_ENTRY_BLOCK(PREG,GET_Y_VAR_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,GET_YY_VAR_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,GET_X_VAL_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      profiled_deref_head_TEST(d0, gvalx_unk);

      /* d0 will keep the first argument */
    gvalx_nonvar:
      /* first argument is bound */
	  EMIT_SIMPLE_BLOCK_TEST(GET_X_VAL_GVALX_NONVAR);
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      profiled_deref_head_TEST(d1, gvalx_nonvar_unk);

    gvalx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GET_X_VAL_GVALX_NONVAR_NONVAR);
      PREG = NEXTOP(PREG, xx);
      UnifyBound(d0, d1);

      BEGP(pt0);
      /* deref second argument */
	  profiled_deref_body(d1, pt0, gvalx_nonvar_unk, gvalx_nonvar_nonvar);
      /* first argument bound, second unbound */
	  EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GET_X_VAL_GVALX_NONVAR_UNK);
      PREG = NEXTOP(PREG, xx);
Bind(pt0, d0);
      GONext();

      ENDP(pt0);
      ENDD(d1);

      BEGP(pt0);
      /* first argument may be unbound */
	  profiled_deref_body(d0, pt0, gvalx_unk, gvalx_nonvar);
      /* first argument is unbound and in pt0 and in d0 */
	  EMIT_SIMPLE_BLOCK_TEST(GET_X_VAL_GVALX_UNK);
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      profiled_deref_head_TEST(d1, gvalx_var_unk);

    gvalx_var_nonvar:
      /* first unbound, second bound */
	  EMIT_SIMPLE_BLOCK_TEST(GET_X_VAL_GVALX_VAR_NONVAR);
      PREG = NEXTOP(PREG, xx);
      Bind(pt0, d1);
      GONext();

      BEGP(pt1);
	  profiled_deref_body(d1, pt1, gvalx_var_unk, gvalx_var_nonvar);
      /* both arguments are unbound */
	  EMIT_SIMPLE_BLOCK_TEST(GET_X_VAL_GVALX_VAR_UNK);
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
      EMIT_ENTRY_BLOCK(PREG,GET_Y_VAL_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yx.y;
      d0 = *pt0;

      /* From now on, it's just a copy of the code for get_x_val */
      profiled_deref_head_TEST(d0, gvaly_unk);
    gvaly_nonvar:

      /* first argument is bound */
      EMIT_SIMPLE_BLOCK_TEST(GET_Y_VAL_GVALY_NONVAR);
      d1 = XREG(PREG->y_u.yx.x);
      profiled_deref_head_TEST(d1, gvaly_nonvar_unk);

    gvaly_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GET_Y_VAL_GVALY_NONVAR_NONVAR);
      PREG = NEXTOP(PREG, yx);
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
	  profiled_deref_body(d1, pt1, gvaly_nonvar_unk, gvaly_nonvar_nonvar);
      /* first argument bound, second unbound */
	  EMIT_SIMPLE_BLOCK_TEST(GET_Y_VAL_GVALY_NONVAR_UNK);
      PREG = NEXTOP(PREG, yx);
      Bind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
	  profiled_derefa_body(d0, pt0, gvaly_unk, gvaly_nonvar);
      /* first argument is unbound */
	  EMIT_SIMPLE_BLOCK_TEST(GET_Y_VAL_GVALY_UNK);
      d1 = XREG(PREG->y_u.yx.x);
      profiled_deref_head_TEST(d1, gvaly_var_unk);
    gvaly_var_nonvar:
      /* first unbound, second bound */
      EMIT_SIMPLE_BLOCK_TEST(GET_Y_VAL_GVALY_VAR_NONVAR);
      PREG = NEXTOP(PREG, yx);
      Bind(pt0, d1);
      GONext();

      BEGP(pt1);
	  profiled_deref_body(d1, pt1, gvaly_var_unk, gvaly_var_nonvar);
      /* both arguments are unbound */
	  EMIT_SIMPLE_BLOCK_TEST(GET_Y_VAL_GVALY_VAR_UNK);
      PREG = NEXTOP(PREG, yx);
      UnifyCells(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_atom, xc);
      EMIT_ENTRY_BLOCK(PREG,GET_ATOM_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = XREG(PREG->y_u.xc.x);
      d1 = PREG->y_u.xc.c;

      BEGP(pt0);
      profiled_deref_head_TEST(d0, gatom_unk);
      /* argument is nonvar */
    gatom_nonvar:
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GET_ATOM_GATOM_NONVAR);
      if (d0 == d1) {
	PREG = NEXTOP(PREG, xc);
	GONext();
      }
      else {
	FAIL();
      }

	  profiled_deref_body(d0, pt0, gatom_unk, gatom_nonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_ATOM_GATOM_UNK);
      PREG = NEXTOP(PREG, xc);
      Bind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_2atoms, cc);
      EMIT_ENTRY_BLOCK(PREG,GET_2ATOMS_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
      profiled_deref_head_TEST(d0, gatom_2unk);
      /* argument is nonvar */
    gatom_2nonvar:
      if (d0 == PREG->y_u.cc.c1) {
	goto gatom_2b;
      }
      else {
	FAIL();
      }

	  profiled_deref_body(d0, pt0, gatom_2unk, gatom_2nonvar);
	  EMIT_SIMPLE_BLOCK_TEST(GET_2ATOMS_GATOM_2UNK);
      Bind(pt0, PREG->y_u.cc.c1);
      ENDP(pt0);
    gatom_2b:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_2ATOMS_GATOM_2B);
      d0 = ARG2;
      d1 = PREG->y_u.cc.c2;

      BEGP(pt0);
      profiled_deref_head_TEST(d0, gatom_2bunk);
      /* argument is nonvar */
    gatom_2bnonvar:
	  EMIT_SIMPLE_BLOCK_TEST(GET_2ATOMS_GATOM_2BNONVAR);
      if (d0 == d1) {
	PREG = NEXTOP(PREG, cc);
	GONext();
      }
      else {
	FAIL();
      }

	  profiled_deref_body(d0, pt0, gatom_2bunk, gatom_2bnonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_2ATOMS_GATOM_2BUNK);
      PREG = NEXTOP(PREG, cc);
      Bind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_3atoms, ccc);
      EMIT_ENTRY_BLOCK(PREG,GET_3ATOMS_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
      profiled_deref_head_TEST(d0, gatom_3unk);
      /* argument is nonvar */
    gatom_3nonvar:
      if (d0 == PREG->y_u.ccc.c1) {
	goto gatom_3b;
      }
      else {
	FAIL();
      }

	  profiled_deref_body(d0, pt0, gatom_3unk, gatom_3nonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_3ATOMS_GATOM_3UNK);
      Bind(pt0, PREG->y_u.ccc.c1);
      ENDP(pt0);
    gatom_3b:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_3ATOMS_GATOM_3B);
      d0 = ARG2;

      BEGP(pt0);
      profiled_deref_head_TEST(d0, gatom_3bunk);
      /* argument is nonvar */
    gatom_3bnonvar:
      if (d0 == PREG->y_u.ccc.c2) {
	goto gatom_3c;
      }
      else {
	FAIL();
      }

	  profiled_deref_body(d0, pt0, gatom_3bunk, gatom_3bnonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_3ATOMS_GATOM_3BUNK);
      Bind(pt0, PREG->y_u.ccc.c2);
      ENDP(pt0);
    gatom_3c:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_3ATOMS_GATOM_3C);
      d0 = ARG3;
      d1 = PREG->y_u.ccc.c3;

      BEGP(pt0);
      profiled_deref_head_TEST(d0, gatom_3cunk);
      /* argument is nonvar */
    gatom_3cnonvar:
	  EMIT_SIMPLE_BLOCK_TEST(GET_3ATOMS_GATOM_3CNONVAR);
      if (d0 == d1) {
	PREG = NEXTOP(PREG, ccc);
	GONext();
      }
      else {
	FAIL();
      }

	  profiled_deref_body(d0, pt0, gatom_3cunk, gatom_3cnonvar);
      /* argument is a variable	  */
	  EMIT_SIMPLE_BLOCK_TEST(GET_3ATOMS_GATOM_3CUNK);
      PREG = NEXTOP(PREG, ccc);
      Bind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_4atoms, cccc);
      EMIT_ENTRY_BLOCK(PREG,GET_4ATOMS_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
      profiled_deref_head_TEST(d0, gatom_4unk);
      /* argument is nonvar */
    gatom_4nonvar:
      if (d0 == PREG->y_u.cccc.c1) {
	goto gatom_4b;
      }
      else {
	FAIL();
      }

	  profiled_deref_body(d0, pt0, gatom_4unk, gatom_4nonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_4ATOMS_GATOM_4UNK);
      Bind(pt0, PREG->y_u.cccc.c1);
      ENDP(pt0);
    gatom_4b:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_4ATOMS_GATOM_4B);
      d0 = ARG2;

      BEGP(pt0);
	  profiled_deref_head_TEST(d0, gatom_4bunk);
      /* argument is nonvar */
    gatom_4bnonvar:
      if (d0 == PREG->y_u.cccc.c2) {
	goto gatom_4c;
      }
      else {
	FAIL();
      }

      profiled_deref_body(d0, pt0, gatom_4bunk, gatom_4bnonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_4ATOMS_GATOM_4BUNK);
      Bind(pt0, PREG->y_u.cccc.c2);
      ENDP(pt0);
    gatom_4c:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_4ATOMS_GATOM_4C);
      d0 = ARG3;

      BEGP(pt0);
	  profiled_deref_head_TEST(d0, gatom_4cunk);
      /* argument is nonvar */
    gatom_4cnonvar:
      if (d0 == PREG->y_u.cccc.c3) {
	goto gatom_4d;
      }
      else {
	FAIL();
      }

      profiled_deref_body(d0, pt0, gatom_4cunk, gatom_4cnonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_4ATOMS_GATOM_4CUNK);
      Bind(pt0, PREG->y_u.cccc.c3);
      ENDP(pt0);
   gatom_4d:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_4ATOMS_GATOM_4D);
      d0 = ARG4;
      d1 = PREG->y_u.cccc.c4;

      BEGP(pt0);
	  profiled_deref_head_TEST(d0, gatom_4dunk);
      /* argument is nonvar */
    gatom_4dnonvar:
	  EMIT_SIMPLE_BLOCK_TEST(GET_4ATOMS_GATOM_4DNONVAR);
      if (d0 == d1) {
	PREG = NEXTOP(PREG, cccc);
	GONext();
      }
      else {
	FAIL();
      }

      profiled_deref_body(d0, pt0, gatom_4dunk, gatom_4dnonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_4ATOMS_GATOM_4DUNK);
      PREG = NEXTOP(PREG, cccc);
      Bind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_5atoms, ccccc);
      EMIT_ENTRY_BLOCK(PREG,GET_5ATOMS_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
	  profiled_deref_head_TEST(d0, gatom_5unk);
      /* argument is nonvar */
    gatom_5nonvar:
      if (d0 == PREG->y_u.ccccc.c1) {
	goto gatom_5b;
      }
      else {
	FAIL();
      }

      profiled_deref_body(d0, pt0, gatom_5unk, gatom_5nonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_5ATOMS_GATOM_5UNK);
      Bind(pt0, PREG->y_u.ccccc.c1);
      ENDP(pt0);
    gatom_5b:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_5ATOMS_GATOM_5B);
      d0 = ARG2;

      BEGP(pt0);
	  profiled_deref_head_TEST(d0, gatom_5bunk);
      /* argument is nonvar */
    gatom_5bnonvar:
      if (d0 == PREG->y_u.ccccc.c2) {
	goto gatom_5c;
      }
      else {
	FAIL();
      }

      profiled_deref_body(d0, pt0, gatom_5bunk, gatom_5bnonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_5ATOMS_GATOM_5BUNK);
      Bind(pt0, PREG->y_u.ccccc.c2);
      ENDP(pt0);
    gatom_5c:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_5ATOMS_GATOM_5C);
      d0 = ARG3;

      BEGP(pt0);
	  profiled_deref_head_TEST(d0, gatom_5cunk);
      /* argument is nonvar */
    gatom_5cnonvar:
      if (d0 == PREG->y_u.ccccc.c3) {
	goto gatom_5d;
      }
      else {
	FAIL();
      }

      profiled_deref_body(d0, pt0, gatom_5cunk, gatom_5cnonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_5ATOMS_GATOM_5CUNK);
      Bind(pt0, PREG->y_u.ccccc.c3);
      ENDP(pt0);
   gatom_5d:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_5ATOMS_GATOM_5D);
      d0 = ARG4;

      BEGP(pt0);
	  profiled_deref_head_TEST(d0, gatom_5dunk);
      /* argument is nonvar */
    gatom_5dnonvar:
      if (d0 == PREG->y_u.ccccc.c4) {
	goto gatom_5e;
      }
      else {
	FAIL();
      }

      profiled_deref_body(d0, pt0, gatom_5dunk, gatom_5dnonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_5ATOMS_GATOM_5DUNK);
      Bind(pt0, PREG->y_u.ccccc.c4);
      ENDP(pt0);
   gatom_5e:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_5ATOMS_GATOM_5E);
      d0 = ARG5;
      d1 = PREG->y_u.ccccc.c5;

      BEGP(pt0);
	  profiled_deref_head_TEST(d0, gatom_5eunk);
      /* argument is nonvar */
    gatom_5enonvar:
	  EMIT_SIMPLE_BLOCK_TEST(GET_5ATOMS_GATOM_5ENONVAR);
      if (d0 == d1) {
	PREG = NEXTOP(PREG, ccccc);
	GONext();
      }
      else {
	FAIL();
      }

      profiled_deref_body(d0, pt0, gatom_5eunk, gatom_5enonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_5ATOMS_GATOM_5EUNK);
      PREG = NEXTOP(PREG, ccccc);
      Bind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_6atoms, cccccc);
      EMIT_ENTRY_BLOCK(PREG,GET_6ATOMS_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
	  profiled_deref_head_TEST(d0, gatom_6unk);
      /* argument is nonvar */
    gatom_6nonvar:
      if (d0 == PREG->y_u.cccccc.c1) {
	goto gatom_6b;
      }
      else {
	FAIL();
      }

      profiled_deref_body(d0, pt0, gatom_6unk, gatom_6nonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_6ATOMS_GATOM_6UNK);
      Bind(pt0, PREG->y_u.cccccc.c1);
      ENDP(pt0);
    gatom_6b:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_6ATOMS_GATOM_6B);
      d0 = ARG2;

      BEGP(pt0);
	  profiled_deref_head_TEST(d0, gatom_6bunk);
      /* argument is nonvar */
    gatom_6bnonvar:
      if (d0 == PREG->y_u.cccccc.c2) {
	goto gatom_6c;
      }
      else {
	FAIL();
      }

      profiled_deref_body(d0, pt0, gatom_6bunk, gatom_6bnonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_6ATOMS_GATOM_6BUNK);
      Bind(pt0, PREG->y_u.cccccc.c2);
      ENDP(pt0);
    gatom_6c:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_6ATOMS_GATOM_6C);
      d0 = ARG3;

      BEGP(pt0);
	  profiled_deref_head_TEST(d0, gatom_6cunk);
      /* argument is nonvar */
    gatom_6cnonvar:
      if (d0 == PREG->y_u.cccccc.c3) {
	goto gatom_6d;
      }
      else {
	FAIL();
      }

      profiled_deref_body(d0, pt0, gatom_6cunk, gatom_6cnonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_6ATOMS_GATOM_6CUNK);
      Bind(pt0, PREG->y_u.cccccc.c3);
      ENDP(pt0);
   gatom_6d:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_6ATOMS_GATOM_6D);
      d0 = ARG4;

      BEGP(pt0);
	  profiled_deref_head_TEST(d0, gatom_6dunk);
      /* argument is nonvar */
    gatom_6dnonvar:
      if (d0 == PREG->y_u.cccccc.c4) {
	goto gatom_6e;
      }
      else {
	FAIL();
      }

      profiled_deref_body(d0, pt0, gatom_6dunk, gatom_6dnonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_6ATOMS_GATOM_6DUNK);
      Bind(pt0, PREG->y_u.cccccc.c4);
      ENDP(pt0);
   gatom_6e:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_6ATOMS_GATOM_6E);
      d0 = ARG5;

      BEGP(pt0);
	  profiled_deref_head_TEST(d0, gatom_6eunk);
      /* argument is nonvar */
    gatom_6enonvar:
      if (d0 == PREG->y_u.cccccc.c5) {
	goto gatom_6f;
      }
      else {
	FAIL();
      }

      profiled_deref_body(d0, pt0, gatom_6eunk, gatom_6enonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_6ATOMS_GATOM_6EUNK);
      Bind(pt0, PREG->y_u.cccccc.c5);
      ENDP(pt0);
    gatom_6f:
      /* fetch arguments */
	  EMIT_SIMPLE_BLOCK_TEST(GET_6ATOMS_GATOM_6F);
      d0 = ARG6;
      d1 = PREG->y_u.cccccc.c6;

      BEGP(pt0);
	  profiled_deref_head_TEST(d0, gatom_6funk);
      /* argument is nonvar */
    gatom_6fnonvar:
	  EMIT_SIMPLE_BLOCK_TEST(GET_6ATOMS_GATOM_6FNONVAR);
      if (d0 == d1) {
	PREG = NEXTOP(PREG, cccccc);
	GONext();
      }
      else {
	FAIL();
      }

      profiled_deref_body(d0, pt0, gatom_6funk, gatom_6fnonvar);
      /* argument is a variable */
	  EMIT_SIMPLE_BLOCK_TEST(GET_6ATOMS_GATOM_6FUNK);
      PREG = NEXTOP(PREG, cccccc);
      Bind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      /* The next instructions can lead to either the READ stream
       * or the write stream */

      OpRW(get_list, x);
      EMIT_ENTRY_BLOCK(PREG,GET_LIST_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.x.x);
      profiled_deref_head_TEST(d0, glist_unk);

    glist_nonvar:
      /* did we find a list? */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GET_LIST_GLIST_NONVAR);
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
      profiled_deref_body(d0, pt0, glist_unk, glist_nonvar);
      /* glist var */
      /* enter write mode */
      EMIT_SIMPLE_BLOCK_TEST(GET_LIST_GLIST_UNK);
      CACHE_S();
      S_SREG = HR;
      START_PREFETCH_W(x);
      PREG = NEXTOP(PREG, x);
      BEGD(d0);
      d0 = AbsPair(S_SREG);
      Bind(pt0, d0);
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
      EMIT_ENTRY_BLOCK(PREG,GET_STRUCT_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xfa.x);
      profiled_deref_head_TEST(d0, gstruct_unk);

    gstruct_nonvar:
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GET_STRUCT_GSTRUCT_NONVAR);
      if (!IsApplTerm(d0)) {
	FAIL();
      }
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
      profiled_deref_body(d0, pt0, gstruct_unk, gstruct_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
	  EMIT_SIMPLE_BLOCK_TEST(GET_STRUCT_GSTRUCT_UNK);
      START_PREFETCH_W(xfa);
      BEGD(d1);
      d1 = AbsAppl(HR);
      Bind(pt0, d1);
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
      EMIT_ENTRY_BLOCK(PREG,GET_FLOAT_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xd.x);
	  profiled_deref_head_TEST(d0, gfloat_unk);

    gfloat_nonvar:
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GET_FLOAT_GFLOAT_NONVAR);
      if (!IsApplTerm(d0)) {
	FAIL();
	  }
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
	  ) {
	    FAIL();
	    }
      ENDP(pt1);
      ENDP(pt0);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, gfloat_unk, gfloat_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
	  EMIT_SIMPLE_BLOCK_TEST(GET_FLOAT_GFLOAT_UNK);
      START_PREFETCH(xc);
      BEGD(d1);
      d1 = AbsAppl(PREG->y_u.xd.d);
      PREG = NEXTOP(PREG, xd);
      Bind(pt0, d1);
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(get_longint, xi);
      EMIT_ENTRY_BLOCK(PREG,GET_LONGINT_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xi.x);
	  profiled_deref_head_TEST(d0, glongint_unk);

    glongint_nonvar:
      EMIT_SIMPLE_BLOCK_TEST(GET_LONGINT_GLONGINT_NONVAR);
      if (!IsApplTerm(d0)) {
	FAIL();
	  }
      /* we have met a preexisting longint */
      START_PREFETCH(xi);
      BEGP(pt0);
      pt0 = RepAppl(d0);
      /* check functor */
      if (*pt0 != (CELL)FunctorLongInt) {
	FAIL();
      }
      if (PREG->y_u.xi.i[1] != (CELL)pt0[1]) {
	    FAIL();
	   }
      ENDP(pt0);
      PREG = NEXTOP(PREG, xi);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, glongint_unk, glongint_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
	  EMIT_SIMPLE_BLOCK_TEST(GET_LONGINT_GLONGINT_UNK);
      START_PREFETCH(xi);
      BEGD(d1);
      d1 = AbsAppl(PREG->y_u.xi.i);
      PREG = NEXTOP(PREG, xi);
      Bind(pt0, d1);
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(get_bigint, xN);
#ifdef USE_GMP
      EMIT_ENTRY_BLOCK(PREG,GET_BIGINT_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xN.x);
	  profiled_deref_head_TEST(d0, gbigint_unk);

    gbigint_nonvar:
      EMIT_SIMPLE_BLOCK_TEST(GET_BIGINT_GBIGINT_NONVAR);
      if (!IsApplTerm(d0)) {
	FAIL();
	  }
      /* we have met a preexisting bigint */
      START_PREFETCH(xN);
      BEGP(pt0);
      pt0 = RepAppl(d0);
      /* check functor */
      if (*pt0 != (CELL)FunctorBigInt)
	{
	  FAIL();
	}
      if (Yap_gmp_tcmp_big_big(d0,PREG->y_u.xN.b)) {
	FAIL();
	  }
      PREG = NEXTOP(PREG, xN);
      ENDP(pt0);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, gbigint_unk, gbigint_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      EMIT_SIMPLE_BLOCK_TEST(GET_BIGINT_GBIGINT_UNK);
      START_PREFETCH(xN);
      BEGD(d1);
      d1 = PREG->y_u.xN.b;
      PREG = NEXTOP(PREG, xN);
      Bind(pt0, d1);
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
      EMIT_ENTRY_BLOCK(PREG,GET_DBTERM_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xD.x);
	  profiled_deref_head_TEST(d0, gdbterm_unk);

    gdbterm_nonvar:
      BEGD(d1);
      /* we have met a preexisting dbterm */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GET_DBTERM_GDBTERM_NONVAR);
      d1 = PREG->y_u.xD.D;
      PREG = NEXTOP(PREG, xD);
      UnifyBound(d0,d1);
      ENDD(d1);

      BEGP(pt0);
      profiled_deref_body(d0, pt0, gdbterm_unk, gdbterm_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
	  EMIT_SIMPLE_BLOCK_TEST(GET_DBTERM_GDBTERM_UNK);
      START_PREFETCH(xD);
      BEGD(d1);
      d1 = PREG->y_u.xD.D;
      PREG = NEXTOP(PREG, xD);
      Bind(pt0, d1);
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

/************************************************************************\
*    Optimised Get List Instructions					*
\************************************************************************/
      OpRW(glist_valx, xx);
      EMIT_ENTRY_BLOCK(PREG,GLIST_VALX_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      profiled_deref_head_TEST(d0, glist_valx_write);
    glist_valx_read:
      BEGP(pt0);
      /* did we find a list? */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GLIST_VALX_GLIST_VALX_READ);
      if (!IsPairTerm(d0)) {
      FAIL();
      }
      /* enter read mode */
      START_PREFETCH(xx);
      pt0 = RepPair(d0);
      SREG = pt0 + 1;
      /* start unification with first argument */
      d0 = *pt0;
      profiled_deref_head_TEST(d0, glist_valx_unk);

      /* first argument is in d0 */
    glist_valx_nonvar:
      /* first argument is bound */
      EMIT_SIMPLE_BLOCK_TEST(GLIST_VALX_GLIST_VALX_NONVAR);
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      profiled_deref_head_TEST(d1, glist_valx_nonvar_unk);

    glist_valx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GLIST_VALX_GLIST_VALX_NONVAR_NONVAR);
      PREG = NEXTOP(PREG, xx);
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      profiled_deref_body(d1, pt1, glist_valx_nonvar_unk, glist_valx_nonvar_nonvar);
      /* head bound, argument unbound */
	  EMIT_SIMPLE_BLOCK_TEST(GLIST_VALX_GLIST_VALX_NONVAR_UNK);
      PREG = NEXTOP(PREG, xx);
      Bind(pt1, d0);
      GONext();
      ENDP(pt1);


      ENDD(d1);

      /* head may be unbound */
      profiled_derefa_body(d0, pt0, glist_valx_unk, glist_valx_nonvar);
      /* head is unbound, pt0 has the value */
      EMIT_SIMPLE_BLOCK_TEST(GLIST_VALX_GLIST_VALX_UNK);
      d0 = XREG(PREG->y_u.xx.xr);
      profiled_deref_head_TEST(d0, glist_valx_var_unk);

    glist_valx_var_nonvar:
      /* head is unbound, second arg bound */
      EMIT_SIMPLE_BLOCK_TEST(GLIST_VALX_GLIST_VALX_VAR_NONVAR);
      PREG = NEXTOP(PREG, xx);
      Bind_Global(pt0, d0);
      GONext();

      BEGP(pt1);
      profiled_deref_body(d0, pt1, glist_valx_var_unk, glist_valx_var_nonvar);
      /* head and second argument are unbound */
	  EMIT_SIMPLE_BLOCK_TEST(GLIST_VALX_GLIST_VALX_VAR_UNK);
      PREG = NEXTOP(PREG, xx);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      END_PREFETCH();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, glist_valx_write, glist_valx_read);
      EMIT_SIMPLE_BLOCK_TEST(GLIST_VALX_GLIST_VALX_WRITE);
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
      Bind(pt0, d0);
      ALWAYS_GONextW();
      ALWAYS_END_PREFETCH_W();
      ENDCACHE_S();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpRW(glist_valy, yx);
      EMIT_ENTRY_BLOCK(PREG,GLIST_VALY_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yx.x);
      profiled_deref_head_TEST(d0, glist_valy_write);
    glist_valy_read:
      BEGP(pt0);
      /* did we find a list? */
      EMIT_SIMPLE_BLOCK_TEST(GLIST_VALY_GLIST_VALY_READ);
      if (!IsPairTerm(d0)) {
	FAIL();
	  }
      START_PREFETCH(yx);
      /* enter read mode */
      pt0 = RepPair(d0);
      SREG = pt0 + 1;
      /* start unification with first argument */
      d0 = *pt0;
      profiled_deref_head_TEST(d0, glist_valy_unk);

    glist_valy_nonvar:
      /* first argument is bound */
      EMIT_SIMPLE_BLOCK_TEST(GLIST_VALY_GLIST_VALY_NONVAR);
      BEGD(d1);
      BEGP(pt1);
      pt1 = YREG + PREG->y_u.yx.y;
      d1 = *pt1;
      PREG = NEXTOP(PREG, yx);
      profiled_deref_head_TEST(d1, glist_valy_nonvar_unk);

    glist_valy_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GLIST_VALY_GLIST_VALY_NONVAR_NONVAR);
      SREG = pt0 + 1;
      UnifyBound(d0, d1);

      /* deref second argument */
      profiled_derefa_body(d1, pt1, glist_valy_nonvar_unk, glist_valy_nonvar_nonvar);
      /* first argument bound, second unbound */
      EMIT_SIMPLE_BLOCK_TEST(GLIST_VALY_GLIST_VALY_NONVAR_UNK);
      Bind(pt1, d0);
      GONext();

      ENDP(pt1);
      /* first argument may be unbound */
      profiled_derefa_body(d0, pt0, glist_valy_unk, glist_valy_nonvar);
      /* first argument is unbound */
	  EMIT_SIMPLE_BLOCK_TEST(GLIST_VALY_GLIST_VALY_UNK);
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.yx.y;
      d1 = *pt1;
      profiled_deref_head_TEST(d1, glist_valy_var_unk);
    glist_valy_var_nonvar:
      /* first unbound, second bound */
      EMIT_SIMPLE_BLOCK_TEST(GLIST_VALY_GLIST_VALY_VAR_NONVAR);
      PREG = NEXTOP(PREG, yx);
      Bind_Global(pt0, d1);
      GONext();

      profiled_derefa_body(d1, pt1, glist_valy_var_unk, glist_valy_var_nonvar);
      /* both arguments are unbound */
      EMIT_SIMPLE_BLOCK_TEST(GLIST_VALY_GLIST_VALY_VAR_UNK);
      PREG = NEXTOP(PREG, yx);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDD(d1);

      END_PREFETCH();
      ENDP(pt0);

      BEGP(pt0);
      profiled_deref_body(d0, pt0, glist_valy_write, glist_valy_read);
      /* enter write mode */
	  EMIT_SIMPLE_BLOCK_TEST(GLIST_VALY_GLIST_VALY_WRITE);
      START_PREFETCH_W(yx);
      BEGP(pt1);
      pt1 = HR;
      d0 = AbsPair(pt1);
      Bind(pt0, d0);
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
      EMIT_ENTRY_BLOCK(PREG,GL_VOID_VARX_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      profiled_deref_head_TEST(d0, glist_void_varx_write);
    glist_void_varx_read:
      /* did we find a list? */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GL_VOID_VARX_GLIST_VOID_VARX_READ);
      if (!IsPairTerm(d0)) {
	FAIL();
	  }
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
      profiled_deref_body(d0, pt0, glist_void_varx_write, glist_void_varx_read);
      EMIT_SIMPLE_BLOCK_TEST(GL_VOID_VARX_GLIST_VOID_VAR_WRITE);
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
      Bind(pt0, d0);
      PREG = NEXTOP(PREG, xx);
      ENDD(d0);
      ENDP(pt1);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(gl_void_vary, yx);
      EMIT_ENTRY_BLOCK(PREG,GL_VOID_VARY_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yx.x);
      profiled_deref_head_TEST(d0, glist_void_vary_write);
    glist_void_vary_read:
      /* did we find a list? */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GL_VOID_VARY_GLIST_VOID_VARY_READ);
      if (!IsPairTerm(d0)) {
	FAIL();
	  }
      /* enter read mode */
      BEGP(pt0);
      pt0 = RepPair(d0);
      d0 = pt0[1];
      ENDP(pt0);
      INITIALIZE_PERMVAR(YREG+PREG->y_u.yx.y,d0);
      PREG = NEXTOP(PREG, yx);
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, glist_void_vary_write, glist_void_vary_read);
      /* enter write mode */
      EMIT_SIMPLE_BLOCK_TEST(GL_VOID_VARY_GLIST_VOID_VARY_WRITE);
      BEGP(pt1);
      pt1 = HR;
      /* include XREG on it */
      INITIALIZE_PERMVAR(YREG+PREG->y_u.yx.y,Unsigned(pt1 + 1));
      PREG = NEXTOP(PREG, yx);
      RESET_VARIABLE(pt1);
      RESET_VARIABLE(pt1+1);
      d0 = AbsPair(pt1);
      HR = pt1 + 2;
      Bind(pt0, d0);
      GONext();
      ENDP(pt1);
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(gl_void_valx, xx);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,GL_VOID_VALX_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      deref_head(d0, glist_void_valx_write);
    glist_void_valx_read:
      BEGP(pt0);
      /* did we find a list? */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALX_GLIST_VOID_VALX_READ);
///#endif
      if (!IsPairTerm(d0)) {
	FAIL();
	  }
      /* enter read mode */
      pt0 = RepPair(d0)+1;
      /* start unification with first argument */
      d0 = *pt0;
      deref_head(d0, glist_void_valx_unk);

    glist_void_valx_nonvar:
      /* first argument is bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALX_GLIST_VOID_VALX_NONVAR);
//#endif
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      deref_head(d1, glist_void_valx_nonvar_unk);

    glist_void_valx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALX_GLIST_VOID_VALX_NONVAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, xx);
      UnifyBound(d0, d1);

      /* deref second argument */
      BEGP(pt1);
      deref_body(d1, pt1, glist_void_valx_nonvar_unk, glist_void_valx_nonvar_nonvar);
      /* first argument bound, second unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALX_GLIST_VOID_VALX_NONVAR_UNK);
///#endif
      PREG = NEXTOP(PREG, xx);
      Bind(pt1, d0);
      GONext();
      ENDP(pt1);
      ENDD(d1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, glist_void_valx_unk, glist_void_valx_nonvar);
      /* first argument is unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALX_GLIST_VOID_VALX_UNK);
///#endif
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      deref_head(d1, glist_void_valx_var_unk);

    glist_void_valx_var_nonvar:
      /* first unbound, second bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALX_GLIST_VOID_VALX_VAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, xx);
      Bind_Global(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, glist_void_valx_var_unk, glist_void_valx_var_nonvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALX_GLIST_VOID_VALX_VAR_UNK);
///#endif
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
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALX_GLIST_VOID_VALX_WRITE);
///#endif
      BEGP(pt1);
      pt1 = HR;
      d0 = AbsPair(pt1);
      Bind(pt0, d0);
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
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,GL_VOID_VALY_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.yx.x);
      deref_head(d0, glist_void_valy_write);
   glist_void_valy_read:
///#ifdef PROFILED_ABSMI
///#endif
      BEGP(pt0);
      /* did we find a list? */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALY_GLIST_VOID_VALY_READ);
///#endif
      if (!IsPairTerm(d0)) {
	FAIL();
	  }
      /* enter read mode */
      pt0 = RepPair(d0)+1;
      /* start unification with first argument */
      d0 = *pt0;
      deref_head(d0, glist_void_valy_unk);

    glist_void_valy_nonvar:
      /* first argument is bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALY_GLIST_VOID_VALY_NONVAR);
///#endif
      BEGD(d1);
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.yx.y;
      d1 = *pt1;
      deref_head(d1, glist_void_valy_nonvar_unk);

    glist_void_valy_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALY_GLIST_VOID_VALY_NONVAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, yx);
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, glist_void_valy_nonvar_unk, glist_void_valy_nonvar_nonvar);
      /* first argument bound, second unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALY_GLIST_VOID_VALY_NONVAR_UNK);
///#endif
      PREG = NEXTOP(PREG, yx);
      Bind(pt1, d0);
      GONext();

      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, glist_void_valy_unk, glist_void_valy_nonvar);
      /* first argument is unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALY_GLIST_VOID_VALY_UNK);
///#endif
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.yx.y;
      d1 = *pt1;
      deref_head(d1, glist_void_valy_var_unk);

    glist_void_valy_var_nonvar:
      /* first unbound, second bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALY_GLIST_VOID_VALY_VAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, yx);
      Bind_Global(pt0, d1);
      GONext();

      deref_body(d1, pt1, glist_void_valy_var_unk, glist_void_valy_var_nonvar);
      /* both arguments are unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALY_GLIST_VOID_VALY_VAR_UNK);
///#endif
      PREG = NEXTOP(PREG, yx);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDD(d1);
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d0, pt0, glist_void_valy_write, glist_void_valy_read);
      /* enter write mode */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(GL_VOID_VALY_GLIST_VOID_VALY_WRITE);
///#endif
      CACHE_S();
      S_SREG = HR;
      d0 = AbsPair(S_SREG);
      Bind(pt0, d0);
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
* 	Unify instructions						*
\************************************************************************/

      Op(unify_x_var, ox);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_X_VAR_INSTINIT);
      CACHE_S();
      READ_IN_S();
      BEGD(d0);
      d0 = *S_SREG;
#ifdef YAPOR_SBA
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_X_VAR_YAPOR_SBA);
      if (d0 == 0) {
	d0 = (CELL)S_SREG;
	}
#endif
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_X_VAR_END);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_X_VAR_WRITE_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_X_VAR_INSTINIT);
      ALWAYS_START_PREFETCH(ox);
      BEGP(pt0);
      BEGD(d0);
      d0 = SREG[0];
      pt0 = &XREG(PREG->y_u.ox.x);
      PREG = NEXTOP(PREG, ox);
#ifdef YAPOR_SBA
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_X_VAR_YAPOR_SBA);
      if (d0 == 0) {
	d0 = (CELL)SREG;
      }
#endif
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_X_VAR_END);
      *pt0 = d0;
      ALWAYS_GONext();
      ENDD(d0);
      ENDP(pt0);
      ALWAYS_END_PREFETCH();
      ENDBOp();

      BOp(unify_l_x_var_write, ox);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_X_VAR_WRITE_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_X_VAR2_INSTINIT);
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
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_X_VAR2_YAPOR_SBA);
      if (d0 == 0) {
	d0 = (CELL)S_SREG;
	}
      if (d1 == 0) {
	d1 = (CELL)(S_SREG+1);
	}
#endif
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_X_VAR2_END);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_X_VAR2_WRITE_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_X_VAR2_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_X_VAR2_WRITE_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_Y_VAR_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_Y_VAR_WRITE_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_Y_VAR_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_Y_VAR_WRITE_INSTINIT);
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
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_X_VAL_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvalx_unk);

    uvalx_nonvar:
      /* first argument is bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(UNIFY_X_VAL_UVALX_NONVAR);
///#endif
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, uvalx_nonvar_unk);

    uvalx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(UNIFY_X_VAL_UVALX_NONVAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, ox);
      SREG++;
      UnifyBound(d0, d1);

      /* deref second argument */
      /* pt0 is in the structure and pt1 the register */
      BEGP(pt1);
      deref_body(d1, pt1, uvalx_nonvar_unk, uvalx_nonvar_nonvar);
      /* first argument bound, second unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(UNIFY_X_VAL_UVALX_NONVAR_UNK);
///#endif
      PREG = NEXTOP(PREG, ox);
      SREG++;
      Bind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, uvalx_unk, uvalx_nonvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(UNIFY_X_VAL_UVALX_UNK);
///#endif
      /* first argument is unbound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, uvalx_var_unk);

    uvalx_var_nonvar:
      /* first unbound, second bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(UNIFY_X_VAL_UVALX_VAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, ox);
      SREG++;
      Bind_Global(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, uvalx_var_unk, uvalx_var_nonvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(UNIFY_X_VAL_UVALX_VAR_UNK);
///#endif
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_X_VAL_WRITE_INSTINIT);
      *SREG++ = XREG(PREG->y_u.ox.x);
      PREG = NEXTOP(PREG, ox);
      GONextW();
      ENDOpW();

      /* We assume the value in X is pointing to an object in the
       * global stack */
      Op(unify_l_x_val, ox);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_X_VAL_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvalx_unk);

    ulvalx_nonvar:
      /* first argument is bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(UNIFY_L_X_VAL_ULVALX_NONVAR);
///#endif
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, ulvalx_nonvar_unk);

    ulvalx_nonvar_nonvar:
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(UNIFY_L_X_VAL_ULVALX_NONVAR_NONVAR);
///#endif
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, ox);
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      deref_body(d1, pt1, ulvalx_nonvar_unk, ulvalx_nonvar_nonvar);
      /* first argument bound, second unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(UNIFY_L_X_VAL_ULVALX_NONVAR_UNK);
///#endif
      PREG = NEXTOP(PREG, ox);
      Bind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvalx_unk, ulvalx_nonvar);
      /* first argument is unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(UNIFY_L_X_VAL_ULVALX_UNK);
///#endif
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, ulvalx_var_unk);

    ulvalx_var_nonvar:
      /* first unbound, second bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(UNIFY_L_X_VAL_ULVALX_VAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, ox);
      Bind_Global(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, ulvalx_var_unk, ulvalx_var_nonvar);
      /* both arguments are unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(UNIFY_L_X_VAL_ULVALX_VAR_UNK);
///#endif
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_X_VAL_WRITE_INSTINIT);
      SREG[0] = XREG(PREG->y_u.ox.x);
      PREG = NEXTOP(PREG, ox);
      GONext();
      ENDOp();

      /* We assume the value in X is pointing to an object in the
       * global stack */
      Op(unify_y_val, oy);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_Y_VAL_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvaly_unk);

    uvaly_nonvar:
      /* first argument is bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_Y_VAL_UVALY_NONVAR);
///#endif
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, uvaly_nonvar_unk);

    uvaly_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
///#ifdef PROFILED_ABSMI
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_Y_VAL_UVALY_NONVAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, oy);
      SREG++;
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, uvaly_nonvar_unk, uvaly_nonvar_nonvar);
      /* first argument bound, second unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_Y_VAL_UVALY_NONVAR_UNK);
///#endif
      PREG = NEXTOP(PREG, oy);
      SREG++;
      Bind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, uvaly_unk, uvaly_nonvar);
      /* first argument is unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_Y_VAL_UVALY_UNK);
///#endif
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, uvaly_var_unk);

    uvaly_var_nonvar:
      /* first unbound, second bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_Y_VAL_UVALY_VAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, oy);
      SREG++;
      Bind_Global(pt0, d1);
      GONext();

      derefa_body(d1, pt1, uvaly_var_unk, uvaly_var_nonvar);
      /* both arguments are unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_Y_VAL_UVALY_VAR_UNK);
///#endif
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_Y_VAL_WRITE_INSTINIT);
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
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_Y_VAL_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvaly_unk);

    ulvaly_nonvar:
      /* first argument is bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_Y_VAL_ULVALY_NONVAR);
///#endif
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_nonvar_unk);

    ulvaly_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
///#ifdef PROFILED_ABSMI
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_L_Y_VAL_ULVALY_NONVAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, oy);
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, ulvaly_nonvar_unk, ulvaly_nonvar_nonvar);
      /* first argument bound, second unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_Y_VAL_ULVALY_NONVAR_UNK);
///#endif
      PREG = NEXTOP(PREG, oy);
      Bind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvaly_unk, ulvaly_nonvar);
      /* first argument is unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_Y_VAL_ULVALY_UNK);
///#endif
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_var_unk);

    ulvaly_var_nonvar:
      /* first unbound, second bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_Y_VAL_ULVALY_VAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, oy);
      Bind_Global(pt0, d1);
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      derefa_body(d1, pt1, ulvaly_var_unk, ulvaly_var_nonvar);
      /* both arguments are unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_Y_VAL_ULVALY_VAR_UNK);
///#endif
      PREG = NEXTOP(PREG, oy);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(unify_l_y_val_write, oy);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_Y_VAL_WRITE_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_X_LOC_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;

      profiled_deref_head_TEST(d0, uvalx_loc_unk);
    uvalx_loc_nonvar:
      /* first argument is bound */
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_X_LOC_UVALX_LOC_NONVAR);
      d1 = XREG(PREG->y_u.ox.x);
      profiled_deref_head_TEST(d1, uvalx_loc_nonvar_unk);

    uvalx_loc_nonvar_nonvar:
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_X_LOC_UVALX_LOC_NONVAR_NONVAR);
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      profiled_deref_body(d1, pt1, uvalx_loc_nonvar_unk, uvalx_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_X_LOC_UVALX_LOC_NONVAR_UNK);
      PREG = NEXTOP(PREG, ox);
      SREG++;
      Bind(pt1, d0);
      GONext();
      ENDP(pt1);


      /* first argument may be unbound */
      profiled_derefa_body(d0, pt0, uvalx_loc_unk, uvalx_loc_nonvar);
      /* first argument is unbound */
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_X_LOC_UVALX_LOC_UNK);
      d1 = XREG(PREG->y_u.ox.x);
      profiled_deref_head_TEST(d1, uvalx_loc_var_unk);
    uvalx_loc_var_nonvar:
      /* first unbound, second bound */
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_X_LOC_UVALX_LOC_VAR_NONVAR);
      PREG = NEXTOP(PREG, ox);
      SREG++;
      Bind_Global(pt0, d1);
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      BEGP(pt1);
      profiled_deref_body(d1, pt1, uvalx_loc_var_unk, uvalx_loc_var_nonvar);
      /* both arguments are unbound */
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_X_LOC_UVALX_LOC_VAR_UNK);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_X_LOC_WRITE_INSTINIT);
      /* we are in write mode */
      BEGD(d0);
      d0 = XREG(PREG->y_u.ox.x);
      profiled_deref_head_TEST(d0, unify_x_loc_unk);
    unify_x_loc_nonvar:
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_X_LOC_WRITE_UNIFY_X_LOC_NONVAR);
      *SREG++ = d0;
      PREG = NEXTOP(PREG, ox);
      GONextW();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, unify_x_loc_unk, unify_x_loc_nonvar);
      /* move ahead in the instructions */
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_X_LOC_WRITE_UNIFY_X_LOC_UNK);
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
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_X_LOC_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvalx_loc_unk);

    ulvalx_loc_nonvar:
      /* first argument is bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_X_LOC_ULVALX_LOC_NONVAR);
///#endif
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, ulvalx_loc_nonvar_unk);

    ulvalx_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
///#ifdef PROFILED_ABSMI
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_L_X_LOC_ULVALX_LOC_NONVAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, ox);
      UnifyBound(d0, d1);

      /* deref second argument */
      deref_body(d1, pt0, ulvalx_loc_nonvar_unk, ulvalx_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_X_LOC_ULVALX_LOC_NONVAR_UNK);
///#endif
      PREG = NEXTOP(PREG, ox);
      Bind(pt0, d0);
      GONext();

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvalx_loc_unk, ulvalx_loc_nonvar);
      /* first argument is unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_X_LOC_ULVALX_LOC_UNK);
///#endif
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, ulvalx_loc_var_unk);

    ulvalx_loc_var_nonvar:
      /* first unbound, second bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_X_LOC_ULVALX_LOC_VAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, ox);
      Bind_Global(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, ulvalx_loc_var_unk, ulvalx_loc_var_nonvar);
      /* both arguments are unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_X_LOC_ULVALX_LOC_VAR_UNK);
///#endif
      PREG = NEXTOP(PREG, ox);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(unify_l_x_loc_write, ox);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_X_LOC_WRITE_INSTINIT);
      /* we are in write mode */
      BEGD(d0);
      d0 = XREG(PREG->y_u.ox.x);
      profiled_deref_head_TEST(d0, ulnify_x_loc_unk);
    ulnify_x_loc_nonvar:
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_X_LOC_WRITE_ULNIFY_X_LOC_NONVAR);
      SREG[0] = d0;
      PREG = NEXTOP(PREG, ox);
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, ulnify_x_loc_unk, ulnify_x_loc_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_X_LOC_WRITE_ULNIFY_X_LOC_UNK);
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
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_Y_LOC_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvaly_loc_unk);

    uvaly_loc_nonvar:
      /* structure is bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_Y_LOC_UVALY_LOC_NONVAR);
///#endif
      BEGP(pt1);
      pt1 =  YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, uvaly_loc_nonvar_unk);

    uvaly_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
///#ifdef PROFILED_ABSMI
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_Y_LOC_UVALY_LOC_NONVAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, oy);
      SREG++;
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, uvaly_loc_nonvar_unk, uvaly_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_Y_LOC_UVALY_LOC_NONVAR_UNK);
///#endif
      PREG = NEXTOP(PREG, oy);
      SREG++;
      Bind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, uvaly_loc_unk, uvaly_loc_nonvar);
      /* first argument is unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_Y_LOC_UVALY_LOC_UNK);
///#endif
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, uvaly_loc_var_unk);

    uvaly_loc_var_nonvar:
      /* first unbound, second bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_Y_LOC_UVALY_LOC_VAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, oy);
      SREG++;
      Bind_Global(pt0, d1);
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      derefa_body(d1, pt1, uvaly_loc_var_unk, uvaly_loc_var_nonvar);
      /* both arguments are unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_Y_LOC_UVALY_LOC_VAR_UNK);
///#endif
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_Y_LOC_WRITE_INSTINIT);
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG+PREG->y_u.oy.y;
      d0 = *pt0;
      profiled_deref_head_TEST(d0, unify_y_loc_unk);
    unify_y_loc_nonvar:
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_Y_LOC_WRITE_UNIFY_Y_LOC_NONVAR);
      *SREG++ = d0;
      PREG = NEXTOP(PREG, oy);
      GONextW();

      profiled_derefa_body(d0, pt0, unify_y_loc_unk, unify_y_loc_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_Y_LOC_WRITE_UNIFY_Y_LOC_UNK);
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
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_Y_LOC_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvaly_loc_unk);

    ulvaly_loc_nonvar:
      /* structure is bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR);
///#endif
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_loc_nonvar_unk);

    ulvaly_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
///#ifdef PROFILED_ABSMI
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, oy);
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, ulvaly_loc_nonvar_unk, ulvaly_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_Y_LOC_ULVALY_LOC_NONVAR_UNK);
///#endif
      PREG = NEXTOP(PREG, oy);
      Bind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvaly_loc_unk, ulvaly_loc_nonvar);
      /* first argument is unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_Y_LOC_ULVALY_LOC_UNK);
///#endif
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_loc_var_unk);

    ulvaly_loc_var_nonvar:
      /* first unbound, second bound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_Y_LOC_ULVALY_LOC_VAR_NONVAR);
///#endif
      PREG = NEXTOP(PREG, oy);
      Bind_Global(pt0, d1);
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      derefa_body(d1, pt1, ulvaly_loc_var_unk, ulvaly_loc_var_nonvar);
      /* both arguments are unbound */
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_Y_LOC_ULVALY_LOC_VAR_UNK);
///#endif
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_Y_LOC_WRITE_INSTINIT);
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG+PREG->y_u.oy.y;
      d0 = *pt0;
      profiled_deref_head_TEST(d0, ulunify_y_loc_unk);
    ulunify_y_loc_nonvar:
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_Y_LOC_WRITE_ULUNIFY_Y_LOC_NONVAR);
      SREG[0] = d0;
      PREG = NEXTOP(PREG, oy);
      GONext();

      profiled_derefa_body(d0, pt0, ulunify_y_loc_unk, ulunify_y_loc_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_Y_LOC_WRITE_ULUNIFY_Y_LOC_UNK);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_VOID_INSTINIT);
      START_PREFETCH(o);
      PREG = NEXTOP(PREG, o);
      SREG++;
      GONext();
      END_PREFETCH();
      ENDOp();

      OpW(unify_void_write, o);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_VOID_WRITE_INSTINIT);
      CACHE_S();
      READ_IN_S();
      PREG = NEXTOP(PREG, o);
      RESET_VARIABLE(S_SREG);
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      GONextW();
      ENDOpW();

      Op(unify_l_void, o);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_VOID_INSTINIT);
      PREG = NEXTOP(PREG, o);
      GONext();
      ENDOp();

      Op(unify_l_void_write, o);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_VOID_WRITE_INSTINIT);
      PREG = NEXTOP(PREG, o);
      RESET_VARIABLE(SREG);
      GONext();
      ENDOp();

      Op(unify_n_voids, os);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_N_VOIDS_INSTINIT);
      SREG += PREG->y_u.os.s;
      PREG = NEXTOP(PREG, os);
      GONext();
      ENDOp();

      OpW(unify_n_voids_write, os);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_N_VOIDS_WRITE_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_N_VOIDS_INSTINIT);
      PREG = NEXTOP(PREG, os);
      GONext();
      ENDOp();

      Op(unify_l_n_voids_write, os);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_N_VOIDS_WRITE_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_ATOM_INSTINIT);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      profiled_deref_head_TEST(d0, uatom_unk);
    uatom_nonvar:
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_ATOM_UATOM_NONVAR);
      if (d0 != PREG->y_u.oc.c) {
	FAIL();
      }
      PREG = NEXTOP(PREG, oc);
      GONext();

      profiled_derefa_body(d0, pt0, uatom_unk, uatom_nonvar);
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_ATOM_UATOM_UNK);
      d0 = PREG->y_u.oc.c;
      PREG = NEXTOP(PREG, oc);
      Bind_Global(pt0, d0);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      OpW(unify_atom_write, oc);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_ATOM_WRITE_INSTINIT);
      * SREG++ = PREG->y_u.oc.c;
      PREG = NEXTOP(PREG, oc);
      GONextW();
      ENDOpW();

      Op(unify_l_atom, oc);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_ATOM_INSTINIT);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *SREG;
      profiled_deref_head_TEST(d0, ulatom_unk);
    ulatom_nonvar:
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_L_ATOM_ULATOM_NONVAR);
      if (d0 != PREG->y_u.oc.c) {
	FAIL();
      }
      PREG = NEXTOP(PREG, oc);
      GONext();

      profiled_derefa_body(d0, pt0, ulatom_unk, ulatom_nonvar);
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_ATOM_ULATOM_UNK);
      d0 = PREG->y_u.oc.c;
      PREG = NEXTOP(PREG, oc);
      Bind_Global(pt0, d0);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(unify_l_atom_write, oc);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_ATOM_WRITE_INSTINIT);
      SREG[0] = PREG->y_u.oc.c;
      PREG = NEXTOP(PREG, oc);
      GONext();
      ENDOp();

      Op(unify_n_atoms, osc);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_N_ATOMS_INSTINIT);
      {
	register Int i = PREG->y_u.osc.s;		/* not enough registers */

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

      do {
        (pt0) = (CELL *)(d0);
        (d0) = *(CELL *)(d0);
        if(!IsVarTerm(d0)) goto uatom_n_nonvar;
      uatom_n_var:;
      } while (Unsigned(pt0) != (d0));
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_N_ATOMS_WRITE_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      d0 = PREG->y_u.osc.s;
      d1 = PREG->y_u.osc.c;
      /* write N atoms */
      CACHE_S();
      READ_IN_S();
      PREG = NEXTOP(PREG, osc);
      for (; d0 > 0; d0--) {
	*S_SREG++ = d1;
	  }
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      ENDD(d1);
      ENDD(d0);
      GONextW();
      ENDOpW();

      Op(unify_float, od);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_FLOAT_INSTINIT);
///#endif
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
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_FLOAT_UFLOAT_NONVAR_INIT);
///#endif
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorDouble) {
	FAIL();
      }
      ENDD(d0);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_FLOAT_UFLOAT_NONVAR_D0ISFUNCTOR);
///#endif
      BEGP(pt1);
      pt1 = PREG->y_u.od.d;
      PREG = NEXTOP(PREG, od);
      if (
	  pt1[1] != pt0[1]
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
	  || pt1[2] != pt0[2]
#endif
	  ) {
	    FAIL();
	  }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_FLOAT_UFLOAT_NONVAR_END);
///#endif
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, pt0, ufloat_unk, ufloat_nonvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_FLOAT_UFLOAT_UNK);
///#endif
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_FLOAT_WRITE_INSTINIT);
      * SREG++ = AbsAppl(PREG->y_u.od.d);
      PREG = NEXTOP(PREG, od);
      GONextW();
      ENDOpW();

      Op(unify_l_float, od);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_FLOAT_INSTINIT);
///#endif
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, ulfloat_unk);
    ulfloat_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_FLOAT_D0ISAPPL);
///#endif
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorDouble) {
	FAIL();
      }
      ENDD(d0);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_FLOAT_D0ISFUNC);
//#endif
      BEGP(pt1);
      pt1 = PREG->y_u.od.d;
      PREG = NEXTOP(PREG, od);
      if (
	  pt1[1] != pt0[1]
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
	  || pt1[2] != pt0[2]
#endif
	  ) {
	   FAIL();
	  }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_FLOAT_EQUALS);
///#endif
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, S_SREG, ulfloat_unk, ulfloat_nonvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_FLOAT_ULFLOAT_UNK);
///#endif
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_FLOAT_WRITE_INSTINIT);
      SREG[0] = AbsAppl(PREG->y_u.od.d);
      PREG = NEXTOP(PREG, od);
      GONext();
      ENDOp();

      Op(unify_longint, oi);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_LONGINT_INSTINIT);
///#endif
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
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_LONGINT_D0ISAPPL);
///#endif
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorLongInt) {
	FAIL();
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_LONGINT_D0ISFUNC);
///#endif
      ENDD(d0);
      BEGP(pt1);
      pt1 = PREG->y_u.oi.i;
      PREG = NEXTOP(PREG, oi);
      if (pt1[1] != pt0[1]) {
		FAIL();
	  }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_LONGINT_EQUALS);
///#endif
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, pt0, ulongint_unk, ulongint_nonvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_LONGINT_ULONGINT_UNK);
///#endif
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_LONGINT_WRITE_INSTINIT);
      * SREG++ = AbsAppl(PREG->y_u.oi.i);
      PREG = NEXTOP(PREG, oi);
      GONextW();
      ENDOpW();

      Op(unify_l_longint, oi);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_LONGINT_INSTINIT);
///#endif
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, ullongint_unk);
    ullongint_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_LONGINT_D0ISAPPL);
///#endif
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorLongInt) {
	FAIL();
      }
      ENDD(d0);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_LONGINT_D0ISFUNC);
///#endif
      BEGP(pt1);
      pt1 = PREG->y_u.oi.i;
      PREG = NEXTOP(PREG, oi);
      if (pt1[1] != pt0[1]) {
      FAIL();
	  }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_LONGINT_EQUALS);
///#endif
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, S_SREG, ullongint_unk, ullongint_nonvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_LONGINT_ULLONGINT_UNK);
///#endif
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_LONGINT_WRITE_INSTINIT);
      SREG[0] = AbsAppl(PREG->y_u.oi.i);
      PREG = NEXTOP(PREG, oi);
      GONext();
      ENDOp();

      Op(unify_bigint, oN);
#ifdef USE_GMP
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_BIGINT_INSTINIT);
///#endif
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
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_BIGINT_D0ISAPPL);
///#endif
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d1);
      d1 = *pt0;
      if (d1 != (CELL)FunctorBigInt)
      {
	FAIL();
      }
      ENDD(d1);
      if (Yap_gmp_tcmp_big_big(d0,PREG->y_u.oN.b)) {
	FAIL();
	  }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_BIGINT_D1ISFUNC_GMP);
///#endif
      PREG = NEXTOP(PREG, oN);
      ENDP(pt0);
      GONext();

      derefa_body(d0, pt0, ubigint_unk, ubigint_nonvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_BIGINT_UBIGINT_UNK);
///#endif
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
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_BIGINT_INSTINIT);
///#endif
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, ulbigint_unk);
    ulbigint_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_BIGINT_D0ISAPPL);
///#endif
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorBigInt)
      {
	FAIL();
      }
      ENDD(d0);
      if (Yap_gmp_tcmp_big_big(d0,PREG->y_u.oN.b)) {
	FAIL();
	  }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_BIGINT_D0ISFUNC_GMP);
///#endif
      PREG = NEXTOP(PREG, oN);
      ENDP(pt0);
      GONext();

      derefa_body(d0, S_SREG, ulbigint_unk, ulbigint_nonvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_BIGINT_ULBIGINT_UNK);
///#endif
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
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_DBTERM_INSTINIT);
///#endif
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, udbterm_unk);
    udbterm_nonvar:
///#ifdef PROFILED_ABSMI
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_DBTERM_UDBTERM_NONVAR);
///#endif
      BEGD(d1);
      /* we have met a preexisting dbterm */
      d1 = PREG->y_u.oD.D;
      PREG = NEXTOP(PREG, oD);
      UnifyBound(d0,d1);
      ENDD(d1);

      derefa_body(d0, pt0, udbterm_unk, udbterm_nonvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_DBTERM_UDBTERM_UNK);
///#endif
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
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_DBTERM_INSTINIT);
///#endif
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, uldbterm_unk);
    uldbterm_nonvar:
///#ifdef PROFILED_ABSMI
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_L_DBTERM_ULDBTERM_NONVAR);
///#endif
      BEGD(d1);
      /* we have met a preexisting dbterm */
      d1 = PREG->y_u.oD.D;
      PREG = NEXTOP(PREG, oD);
      UnifyBound(d0,d1);
      ENDD(d1);

      derefa_body(d0, S_SREG, uldbterm_unk, uldbterm_nonvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_DBTERM_ULDBTERM_UNK);
///#endif
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_LIST_INSTINIT);
      *--SP = Unsigned(SREG + 1);
      *--SP = READ_MODE;
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      profiled_deref_head_TEST(d0, ulist_unk);
    ulist_nonvar:
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_LIST_READMODE);
      if (!IsPairTerm(d0)) {
	FAIL();
      }
      /* we continue in read mode */
      START_PREFETCH(o);
      SREG = RepPair(d0);
      PREG = NEXTOP(PREG, o);
      GONext();
      END_PREFETCH();

      profiled_derefa_body(d0, pt0, ulist_unk, ulist_nonvar);
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_LIST_WRITEMODE);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_LIST_WRITE_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_LIST_INSTINIT);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      profiled_deref_head_TEST(d0, ullist_unk);
    ullist_nonvar:
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_L_LIST_READMODE);
      START_PREFETCH(o);
      if (!IsPairTerm(d0)) {
	FAIL();
      }
      /* we continue in read mode */
      PREG = NEXTOP(PREG, o);
      SREG = RepPair(d0);
      GONext();
      END_PREFETCH();

      profiled_derefa_body(d0, pt0, ullist_unk, ullist_nonvar);
      /* we enter write mode */
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_LIST_WRITEMODE);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_LIST_WRITE_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_STRUCT_INSTINIT);
      *--SP = Unsigned(SREG + 1);
      *--SP = READ_MODE;
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      START_PREFETCH(ofa);
      profiled_deref_head_TEST(d0, ustruct_unk);
    ustruct_nonvar:
      /* we are in read mode */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_STRUCT_READMODE);
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

      profiled_derefa_body(d0, pt0, ustruct_unk, ustruct_nonvar);
      /* Enter Write mode */
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_STRUCT_WRITEMODE);
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
      /* update HR */
      GONextW();
      END_PREFETCH_W();

      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpW(unify_struct_write, ofa);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_STRUCT_WRITE_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_STRUC_INSTINIT);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      profiled_deref_head_TEST(d0, ulstruct_unk);
    ulstruct_nonvar:
      /* we are in read mode */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNIFY_L_STRUC_READMODE);
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

      profiled_derefa_body(d0, pt0, ulstruct_unk, ulstruct_nonvar);
      /* Enter Write mode */
      EMIT_SIMPLE_BLOCK_TEST(UNIFY_L_STRUC_WRITEMODE);
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
      /* update HR */
      GONextW();
      END_PREFETCH_W();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpW(unify_l_struc_write, ofa);
      EMIT_ENTRY_BLOCK(PREG,UNIFY_L_STRUC_WRITE_INSTINIT);
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
* Put Instructions							 *
\************************************************************************/

      Op(put_x_var, xx);
      EMIT_ENTRY_BLOCK(PREG,PUT_X_VAR_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,PUT_Y_VAR_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,PUT_X_VAL_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      XREG(PREG->y_u.xx.xr) = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, xx);
      GONext();
      ENDOp();

      Op(put_xx_val, xxxx);
      EMIT_ENTRY_BLOCK(PREG,PUT_XX_VAL_INSTINIT);
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
      EMIT_ENTRY_BLOCK(PREG,PUT_Y_VAL_INSTINIT);
      BEGD(d0);
      d0 = YREG[PREG->y_u.yx.y];
#ifdef YAPOR_SBA
      if (d0 == 0) { /* new variable */
	XREG(PREG->y_u.yx.x) = (CELL)(YREG+PREG->y_u.yx.y);
      } else
#endif
	XREG(PREG->y_u.yx.x) = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, yx);
      GONext();
      ENDOp();

      Op(put_y_vals, yyxx);
      EMIT_ENTRY_BLOCK(PREG,PUT_Y_VALS_INSTINIT);
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
    EMIT_ENTRY_BLOCK(PREG,PUT_UNSAFE_INSTINIT);
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG+PREG->y_u.yx.y;
      d0 = *pt0;
      profiled_deref_head_TEST(d0, punsafe_unk);
    punsafe_nonvar:
    EMIT_SIMPLE_BLOCK_TEST(PUT_UNSAFE_PUNSAFE_NONVAR);
      XREG(PREG->y_u.yx.x) = d0;
      PREG = NEXTOP(PREG, yx);
      GONext();

      profiled_derefa_body(d0, pt0, punsafe_unk, punsafe_nonvar);
    EMIT_SIMPLE_BLOCK_TEST(PUT_UNSAFE_PUNSAFE_UNK);
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
      EMIT_ENTRY_BLOCK(PREG,PUT_ATOM_INSTINIT);
      BEGD(d0);
      d0 = PREG->y_u.xc.c;
      XREG(PREG->y_u.xc.x) = d0;
      PREG = NEXTOP(PREG, xc);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_dbterm, xD);
    EMIT_ENTRY_BLOCK(PREG,PUT_DBTERM_INSTINIT);
      BEGD(d0);
      d0 = PREG->y_u.xD.D;
      XREG(PREG->y_u.xD.x) = d0;
      PREG = NEXTOP(PREG, xD);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_bigint, xN);
    EMIT_ENTRY_BLOCK(PREG,PUT_BIGINT_INSTINIT);
      BEGD(d0);
      d0 = PREG->y_u.xN.b;
      XREG(PREG->y_u.xN.x) = d0;
      PREG = NEXTOP(PREG, xN);
      GONext();
      ENDD(d0);
      ENDOp();

     Op(put_float, xd);
    EMIT_ENTRY_BLOCK(PREG,PUT_FLOAT_INSTINIT);
      BEGD(d0);
      d0 = AbsAppl(PREG->y_u.xd.d);
      XREG(PREG->y_u.xd.x) = d0;
      PREG = NEXTOP(PREG, xd);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_longint, xi);
    EMIT_ENTRY_BLOCK(PREG,PUT_LONGINT_INSTINIT);
      BEGD(d0);
      d0 = AbsAppl(PREG->y_u.xi.i);
      XREG(PREG->y_u.xi.x) = d0;
      PREG = NEXTOP(PREG, xi);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_list, x);
    EMIT_ENTRY_BLOCK(PREG,PUT_LIST_INSTINIT);
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
    EMIT_ENTRY_BLOCK(PREG,PUT_STRUCT_INSTINIT);
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
* 	Write Instructions						*
\************************************************************************/

      Op(write_x_var, x);
    EMIT_ENTRY_BLOCK(PREG,WRITE_X_VAR_INSTINIT);
      XREG(PREG->y_u.x.x) = Unsigned(SREG);
      PREG = NEXTOP(PREG, x);
      RESET_VARIABLE(SREG);
      SREG++;
      GONext();
      ENDOp();

      Op(write_void, e);
    EMIT_ENTRY_BLOCK(PREG,WRITE_VOID_INSTINIT);
      PREG = NEXTOP(PREG, e);
      RESET_VARIABLE(SREG);
      SREG++;
      GONext();
      ENDOp();

      Op(write_n_voids, s);
    EMIT_ENTRY_BLOCK(PREG,WRITE_N_VOIDS_INSTINIT);
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
    EMIT_ENTRY_BLOCK(PREG,WRITE_Y_VAR_INSTINIT);
      INITIALIZE_PERMVAR(YREG+PREG->y_u.y.y,Unsigned(SREG));
      PREG = NEXTOP(PREG, y);
      RESET_VARIABLE(SREG);
      SREG++;
      GONext();
      ENDOp();

      Op(write_x_val, x);
    EMIT_ENTRY_BLOCK(PREG,WRITE_X_VAL_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.x.x);
      *SREG++ = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, x);
      GONext();
      ENDOp();

      Op(write_x_loc, x);
    EMIT_ENTRY_BLOCK(PREG,WRITE_X_LOC_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.x.x);
      PREG = NEXTOP(PREG, x);
      profiled_deref_head_TEST(d0, w_x_unk);
    w_x_bound:
    EMIT_SIMPLE_BLOCK_TEST(WRITE_X_LOC_W_X_BOUND);
      *SREG++ = d0;
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, w_x_unk, w_x_bound);
    EMIT_SIMPLE_BLOCK_TEST(WRITE_X_LOC_W_X_UNK);
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
    EMIT_ENTRY_BLOCK(PREG,WRITE_Y_VAL_INSTINIT);
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
    EMIT_ENTRY_BLOCK(PREG,WRITE_Y_LOC_INSTINIT);
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG+PREG->y_u.y.y;
      d0 = *pt0;
      profiled_deref_head_TEST(d0, w_y_unk);
    w_y_bound:
    EMIT_SIMPLE_BLOCK_TEST(WRITE_Y_LOC_W_Y_BOUND);
      PREG = NEXTOP(PREG, y);
      *SREG++ = d0;
      GONext();

      profiled_derefa_body(d0, pt0, w_y_unk, w_y_bound);
    EMIT_SIMPLE_BLOCK_TEST(WRITE_Y_LOC_W_Y_UNK);
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
    EMIT_ENTRY_BLOCK(PREG,WRITE_ATOM_INSTINIT);
      BEGD(d0);
      d0 = PREG->y_u.c.c;
      *SREG++ = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, c);
      GONext();
      ENDOp();

      Op(write_bigint, N);
    EMIT_ENTRY_BLOCK(PREG,WRITE_BIGINT_INSTINIT);
      BEGD(d0);
      d0 = PREG->y_u.N.b;
      *SREG++ = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, N);
      GONext();
      ENDOp();

      Op(write_dbterm, D);
    EMIT_ENTRY_BLOCK(PREG,WRITE_DBTERM_INSTINIT);
      BEGD(d0);
      d0 = PREG->y_u.D.D;
      *SREG++ = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, D);
      GONext();
      ENDOp();

      Op(write_float, d);
    EMIT_ENTRY_BLOCK(PREG,WRITE_FLOAT_INSTINIT);
      BEGD(d0);
      d0 = AbsAppl(PREG->y_u.d.d);
      *SREG++ = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, d);
      GONext();
      ENDOp();

      Op(write_longint, i);
    EMIT_ENTRY_BLOCK(PREG,WRITE_LONGIT_INSTINIT);
      BEGD(d0);
      d0 = AbsAppl(PREG->y_u.i.i);
      *SREG++ = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, i);
      GONext();
      ENDOp();

      Op(write_n_atoms, sc);
    EMIT_ENTRY_BLOCK(PREG,WRITE_N_ATOMS_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      d0 = PREG->y_u.sc.s;
      d1 = PREG->y_u.sc.c;
      for (; d0 > 0; d0--) {
	*SREG++ = d1;
	  }
      ENDD(d1);
      ENDD(d0);
      PREG = NEXTOP(PREG, sc);
      GONext();
      ENDOp();

      Op(write_list, e);
    EMIT_ENTRY_BLOCK(PREG,WRITE_LIST_INSTINIT);
      BEGD(d0);
      d0 = AbsPair(HR);
      *SREG++ = d0;
      /* I will not actually store the mode in the stack */
      SP[-1] = Unsigned(SREG);
      SP[-2] = 1;		/* Put instructions follow the main stream */
      SP -= 2;
      SREG = HR;
      HR += 2;
      ENDD(d0);
      PREG = NEXTOP(PREG, e);
      GONext();
      ENDOp();

      Op(write_l_list, e);
    EMIT_ENTRY_BLOCK(PREG,WRITE_L_LIST_INSTINIT);
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
    EMIT_ENTRY_BLOCK(PREG,WRITE_STRUCT_INSTINIT);
      BEGD(d0);
      d0 = AbsAppl(HR);
      *SREG++ = d0;
      SP[-1] = Unsigned(SREG);
      SP[-2] = 1;		/* Put instructions follow the main stream */
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
    EMIT_ENTRY_BLOCK(PREG,WRITE_L_STRUC_INSTINIT);
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
*   Save last unified struct or list					*
\************************************************************************/

/* vitor: I think I should kill these two instructions, by expanding the
 * othe instructions.
 */

      Op(save_pair_x, ox);
    EMIT_ENTRY_BLOCK(PREG,SAVE_PAIR_X_INSTINIT);
      XREG(PREG->y_u.ox.x) = AbsPair(SREG);
      PREG = NEXTOP(PREG, ox);
      GONext();
      ENDOp();

      OpW(save_pair_x_write, ox);
    EMIT_ENTRY_BLOCK(PREG,SAVE_PAIR_X_WRITE_INSTINIT);
      XREG(PREG->y_u.ox.x) = AbsPair(SREG);
      PREG = NEXTOP(PREG, ox);
      GONextW();
      ENDOpW();

      Op(save_pair_y, oy);
    EMIT_ENTRY_BLOCK(PREG,SAVE_PAIR_Y_INSTINIT);
      INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,AbsPair(SREG));
      PREG = NEXTOP(PREG, oy);
      GONext();
      ENDOp();

      OpW(save_pair_y_write, oy);
    EMIT_ENTRY_BLOCK(PREG,SAVE_PAIR_Y_WRITE_INSTINIT);
      INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,AbsPair(SREG));
      PREG = NEXTOP(PREG, oy);
      GONextW();
      ENDOpW();

      Op(save_appl_x, ox);
    EMIT_ENTRY_BLOCK(PREG,SAVE_APPL_X_INSTINIT);
      XREG(PREG->y_u.ox.x) = AbsAppl(SREG - 1);
      PREG = NEXTOP(PREG, ox);
      GONext();
      ENDOp();

      OpW(save_appl_x_write, ox);
    EMIT_ENTRY_BLOCK(PREG,SAVE_APPL_X_WRITE_INSTINIT);
      XREG(PREG->y_u.ox.x) = AbsAppl(SREG - 1);
      PREG = NEXTOP(PREG, ox);
      GONextW();
      ENDOpW();

      Op(save_appl_y, oy);
    EMIT_ENTRY_BLOCK(PREG,SAVE_APPL_Y_INSTINIT);
      INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,AbsAppl(SREG-1));
      PREG = NEXTOP(PREG, oy);
      GONext();
      ENDOp();

      OpW(save_appl_y_write, oy);
    EMIT_ENTRY_BLOCK(PREG,SAVE_APPL_Y_WRITE_INSTINIT);
      INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,AbsAppl(SREG-1));
      PREG = NEXTOP(PREG, oy);
      GONextW();
      ENDOpW();


/************************************************************************\
*   Instructions for implemeting 'or;'					 *
\************************************************************************/

      BOp(jump, l);
    EMIT_ENTRY_BLOCK(PREG,JUMP_INSTINIT);
      PREG = PREG->y_u.l.l;
      JMPNext();
      ENDBOp();

      /* This instruction is called when the previous goal
	 was interrupted when waking up goals
      */
      BOp(move_back, l);
    EMIT_ENTRY_BLOCK(PREG,MOVE_BACK_INSTINIT);
      PREG = (yamop *)(((char *)PREG)-(Int)(NEXTOP((yamop *)NULL,Osbpp)));
      JMPNext();
      ENDBOp();

      /* This instruction is called when the previous goal
	 was interrupted when waking up goals
      */
      BOp(skip, l);
    EMIT_ENTRY_BLOCK(PREG,SKIP_INSTINIT);
      PREG = NEXTOP(PREG,l);
      JMPNext();
      ENDBOp();

      Op(either, Osblp);
    EMIT_ENTRY_BLOCK(PREG,EITHER_INSTINIT);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
    EMIT_SIMPLE_BLOCK_TEST(EITHER_LOW_LEVEL_TRACER);
	low_level_trace(try_or, (PredEntry *)PREG, NULL);
      }
#endif
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
	  EMIT_SIMPLE_BLOCK_TEST(NoStackEither_Exception);
      check_stack(NoStackEither, HR);
      ENDCACHE_Y_AS_ENV();
#endif
    EMIT_SIMPLE_BLOCK_TEST(EITHER_POST_COROUTINING);
      BEGD(d0);
      /* Try to preserve the environment */
      d0 = PREG->y_u.Osblp.s;
      BEGCHO(pt1);
      pt1 = (choiceptr) ((char *) YREG + (yslot) d0);
    EMIT_SIMPLE_BLOCK_TEST(EITHER_FROZEN_YSBA);
#ifdef FROZEN_STACKS
	choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
	if (pt1 > top_b || pt1 < (choiceptr)H)
	  pt1 = top_b;
#else
	if (pt1 > top_b)
	  pt1 = top_b;
#endif /* YAPOR_SBA */
#else
      if (pt1 > B)
	pt1 = B;
#endif /* FROZEN_STACKS */
    EMIT_SIMPLE_BLOCK_TEST(EITHER_POST_FROZEN_YSBA);
      pt1 = (choiceptr)(((CELL *) pt1)-1);
      *(CELL **) pt1 = YREG;
      store_yaam_regs_for_either(PREG->y_u.Osblp.l, PREG);
      SREG = (CELL *) (B = pt1);
#ifdef YAPOR
    EMIT_SIMPLE_BLOCK_TEST(EITHER_YAPOR);
      SCH_set_load(pt1);
#endif	/* YAPOR */
    EMIT_SIMPLE_BLOCK_TEST(EITHER_END);
      SET_BB(pt1);
      ENDCHO(pt1);
      /* skip the current instruction plus the next one */
      PREG = NEXTOP(NEXTOP(PREG, Osblp),l);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(or_else, Osblp);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,OR_ELSE_INSTINIT);
///#endif
      HR = HBREG = PROTECT_FROZEN_H(B);
      ENV = B->cp_env;
      B->cp_cp = PREG;
#ifdef DEPTH_LIMIT
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(OR_ELSE_DEPTH);
///#endif
      DEPTH = B->cp_depth;
#endif	/* DEPTH_LIMIT */
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(OR_ELSE_POST_DEPTH);
///#endif
      SET_BB(PROTECT_FROZEN_B(B));
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(OR_ELSE_YAPOR);
///#endif
	SCH_new_alternative(PREG, PREG->y_u.Osblp.l);
      } else
#endif	/* YAPOR */
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(OR_ELSE_END);
///#endif
      B->cp_ap = PREG->y_u.Osblp.l;
      PREG = NEXTOP(PREG, Osblp);
      YREG = (CELL *) B->cp_a1;
      GONext();
      ENDOp();

#ifdef YAPOR
      Op(or_last, Osblp);
#else
      Op(or_last, p);
#endif	/* YAPOR */
    EMIT_ENTRY_BLOCK(PREG,OR_LAST_INSTINIT);
      BEGCHO(pt0);
      pt0 = B;
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
    EMIT_SIMPLE_BLOCK_TEST(OR_LAST_IFOK_INIT);
	H = HBREG = PROTECT_FROZEN_H(pt0);
	YREG = (CELL *) pt0->cp_a1;
	ENV = pt0->cp_env;
#ifdef DEPTH_LIMIT
    EMIT_SIMPLE_BLOCK_TEST(OR_LAST_IFOK_DEPTH);
	DEPTH = pt0->cp_depth;
#endif	/* DEPTH_LIMIT */
    EMIT_SIMPLE_BLOCK_TEST(OR_LAST_IFOK_END);
        SCH_new_alternative(PREG, NULL);
      }
      else
#endif	/* YAPOR */
      {
    EMIT_SIMPLE_BLOCK_TEST(OR_LAST_NOIF_INIT);
	B = pt0->cp_b;
	HR = PROTECT_FROZEN_H(pt0);
	YREG = (CELL *) pt0->cp_a1;
	ENV = pt0->cp_env;
#ifdef DEPTH_LIMIT
    EMIT_SIMPLE_BLOCK_TEST(OR_LAST_NOIF_DEPTH);
	DEPTH = pt0->cp_depth;
#endif	/* DEPTH_LIMIT */
    EMIT_SIMPLE_BLOCK_TEST(OR_LAST_NOIF_END);
	HBREG = PROTECT_FROZEN_H(B);
      }
#ifdef YAPOR
    EMIT_SIMPLE_BLOCK_TEST(OR_LAST_YAPOR);
      PREG = NEXTOP(PREG, Osblp);
#else
    EMIT_SIMPLE_BLOCK_TEST(OR_LAST_NOYAPOR);
      PREG = NEXTOP(PREG, p);
#endif	/* YAPOR */
    EMIT_SIMPLE_BLOCK_TEST(OR_LAST_END);
      SET_BB(PROTECT_FROZEN_B(B));
      GONext();
      ENDCHO(pt0);
      ENDOp();

/************************************************************************\
*	Pop operations							 *
\************************************************************************/

      OpRW(pop_n, s);
      /* write mode might have been called from read mode */
      EMIT_ENTRY_BLOCK(PREG,POP_N_INSTINIT);
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(POP_N_END);
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
      EMIT_ENTRY_BLOCK(PREG,POP_INSTINIT);
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(POP_END);
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
*	Call C predicates instructions					 *
\************************************************************************/

      BOp(call_cpred, Osbpp);
    EMIT_ENTRY_BLOCK(PREG,CALL_CPRED_INSTINIT);
    EMIT_SIMPLE_BLOCK_TEST(YAAM_CHECK_TRAIL_TR);
      check_trail(TR);
    EMIT_SIMPLE_BLOCK_TEST(CALL_CPRED_TEST_STACK);
      if (!(PREG->y_u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) {
	CACHE_Y_AS_ENV(YREG);
	EMIT_SIMPLE_BLOCK_TEST(NoStackCall_Exception);
	check_stack(NoStackCall, HR);
	ENDCACHE_Y_AS_ENV();
      }
#ifdef FROZEN_STACKS
      {
    EMIT_SIMPLE_BLOCK_TEST(CALL_CPRED_FROZEN_INIT);
	choiceptr top_b = PROTECT_FROZEN_B(B);

    EMIT_SIMPLE_BLOCK_TEST(CALL_CPRED_TOPB);
#ifdef YAPOR_SBA
	if (YREG > (CELL *) top_b || YREG < HR)
	    ASP = (CELL *)top_b;
#else
	if (YREG > (CELL *) top_b)
	    ASP = (CELL *)top_b;
#endif /* YAPOR_SBA */
	else
	    ASP = (CELL *)(((char *)YREG) +  PREG->y_u.Osbpp.s);
      }
#else
    EMIT_SIMPLE_BLOCK_TEST(CALL_CPRED_NOFROZEN);
      SET_ASP(YREG, PREG->y_u.Osbpp.s);
      /* for slots to work */
#endif /* FROZEN_STACKS */
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
    EMIT_SIMPLE_BLOCK_TEST(CALL_CPRED_LOW_LEVEL_TRACER);
	low_level_trace(enter_pred,PREG->y_u.Osbpp.p,XREGS+1);
      }
#endif	/* LOW_LEVEL_TRACE */
    EMIT_SIMPLE_BLOCK_TEST(CALL_CPRED_POST_LOW_LEVEL_TRACER);
      BEGD(d0);
      CPredicate f = PREG->y_u.Osbpp.p->cs.f_code;
      PREG = NEXTOP(PREG, Osbpp);
      saveregs();
      d0 = (f)(PASS_REGS1);
      setregs();
#ifdef SHADOW_S
    EMIT_SIMPLE_BLOCK_TEST(CALL_CPRED_SETSREG);
      SREG = Yap_REGS.S_;
#endif
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(CALL_CPRED_END);
      if (!d0) {
        FAIL();
      }
      CACHE_A1();
      ENDD(d0);
      JMPNext();
      ENDBOp();

      /* execute     Label               */
      BOp(execute_cpred, pp);
    EMIT_ENTRY_BLOCK(PREG,EXECUTE_CPRED_INSTINIT);
    EMIT_SIMPLE_BLOCK_TEST(YAAM_CHECK_TRAIL_TR);
      check_trail(TR);
      {
    EMIT_SIMPLE_BLOCK_TEST(EXECUTE_CPRED_POST_CHECK_TRAIL);
	PredEntry *pt0;

	BEGD(d0);
	CACHE_Y_AS_ENV(YREG);
#ifdef FROZEN_STACKS
	{
    EMIT_SIMPLE_BLOCK_TEST(EXECUTE_CPRED_FROZEN);
	  choiceptr top_b = PROTECT_FROZEN_B(B);

    EMIT_SIMPLE_BLOCK_TEST(EXECUTE_CPRED_TOPB);
#ifdef YAPOR_SBA
	  if (YREG > (CELL *) top_b || YREG < HR) {
	      ASP = (CELL *)top_b;
	  }
#else
	  if (YREG > (CELL *) top_b) {
	      ASP = (CELL *)top_b;
	  }
#endif /* YAPOR_SBA */
	  else {
        ASP = YREG+E_CB;
	  }
	}
#else
    EMIT_SIMPLE_BLOCK_TEST(EXECUTE_CPRED_NOFROZEN);
	SET_ASP(YREG, E_CB*sizeof(CELL));
	/* for slots to work */
#endif /* FROZEN_STACKS */
    EMIT_SIMPLE_BLOCK_TEST(EXECUTE_CPRED_POST_FROZEN);
	pt0 = PREG->y_u.pp.p;
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
    EMIT_SIMPLE_BLOCK_TEST(EXECUTE_CPRED_LOW_LEVEL_TRACER);
	  low_level_trace(enter_pred,pt0,XREGS+1);
	}
#endif	/* LOW_LEVEL_TRACE */
    EMIT_SIMPLE_BLOCK_TEST(EXECUTE_CPRED_POST_LOW_LEVEL_TRACER);
	CACHE_A1();
	BEGD(d0);
	d0 = (CELL)B;
#ifndef NO_CHECKING
    EMIT_SIMPLE_BLOCK_TEST(NoStackExecute_Exception);
	check_stack(NoStackExecute, HR);
#endif
	/* for profiler */
    EMIT_SIMPLE_BLOCK_TEST(EXECUTE_CPRED_SAVE_PC);
	save_pc();
	ENV_YREG[E_CB] = d0;
	ENDD(d0);
#ifdef DEPTH_LIMIT
	if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is prolog */
    EMIT_CONDITIONAL_SUCCESS("DEPTH <= MkIntTerm(1)");
    EMIT_SIMPLE_BLOCK_TEST(EXECUTE_CPRED_DEPTH_MINOR);
	  if (pt0->ModuleOfPred) {
	    if (DEPTH == MkIntTerm(0)) {
	      FAIL();
		}
	    else {
          DEPTH = RESET_DEPTH();
	    }
	  }
	} else if (pt0->ModuleOfPred) {
    EMIT_CONDITIONAL_FAIL("DEPTH <= MkIntTerm(1)");
	EMIT_CONDITIONAL_SUCCESS("pt0->ModuleOfPred");
    EMIT_SIMPLE_BLOCK_TEST(EXECUTE_CPRED_DEPTH_MOFPRED);
	  DEPTH -= MkIntConstant(2);
	}
	else {
    EMIT_CONDITIONAL_FAIL("DEPTH <= MkIntTerm(1)");
	EMIT_CONDITIONAL_FAIL("pt0->ModuleOfPred");
    EMIT_SIMPLE_BLOCK_TEST(EXECUTE_CPRED_DEPTH_END);
}
#endif	/* DEPTH_LIMIT */
	/* now call C-Code */
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(EXECUTE_CPRED_END);
	{
	  CPredicate f = PREG->y_u.pp.p->cs.f_code;
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
      ENDBOp();

      /* Like previous, the only difference is that we do not */
      /* trust the C-function we are calling and hence we must */
      /* guarantee that *all* machine registers are saved and */
      /* restored */
      BOp(call_usercpred, Osbpp);
    EMIT_ENTRY_BLOCK(PREG,CALL_USERCPRED_INSTINIT);
      CACHE_Y_AS_ENV(YREG);
	  EMIT_SIMPLE_BLOCK_TEST(NoStackCall_Exception);
      check_stack(NoStackCall, HR);
      ENDCACHE_Y_AS_ENV();
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
    EMIT_SIMPLE_BLOCK_TEST(CALL_USERCPRED_LOW_LEVEL_TRACER);
	  low_level_trace(enter_pred,PREG->y_u.Osbpp.p,XREGS+1);
	}
#endif	/* LOW_LEVEL_TRACE */
    EMIT_SIMPLE_BLOCK_TEST(CALL_USERCPRED_FROZEN);
#ifdef FROZEN_STACKS
      {
	choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
	if (YREG > (CELL *) top_b || YREG < HR)
	  ASP = (CELL *) top_b;
#else
	if (YREG > (CELL *) top_b)
      ASP = (CELL *) top_b;
#endif /* YAPOR_SBA */
	else
      ASP = (CELL *)(((char *)YREG) +  PREG->y_u.Osbpp.s);
      }
#else
      SET_ASP(YREG, PREG->y_u.Osbpp.s);
      /* for slots to work */
#endif /* FROZEN_STACKS */
      {
    EMIT_SIMPLE_BLOCK_TEST(CALL_USERCPRED_POST_FROZEN);
	/* make sure that we can still have access to our old PREG after calling user defined goals and backtracking or failing */
	yamop *savedP;

	Yap_StartSlots( );
	LOCAL_PrologMode = UserCCallMode;
	{
	  PredEntry *p = PREG->y_u.Osbpp.p;

	  PREG = NEXTOP(PREG, Osbpp);
	  savedP = PREG;
	  saveregs();
	  save_machine_regs();

	  SREG = (CELL *) YAP_Execute(p, p->cs.f_code);
	}
	setregs();
	LOCAL_PrologMode = UserMode;
	restore_machine_regs();
	PREG = savedP;
      }
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(CALL_USERCPRED_END);
      if (EX) {
	struct DB_TERM *exp = EX;
	EX = NULL;
	Yap_JumpToEnv(Yap_PopTermFromDB(exp));
      }
      if (!SREG) {
	FAIL();
      }
      /* in case we call Execute */
      YENV = ENV;
      YREG = ENV;
      JMPNext();
      ENDBOp();

      BOp(call_c_wfail, slp);
      //printf("call_c_wfail!!\n\n");
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
	  low_level_trace(enter_pred,PREG->y_u.slp.p,XREGS+1);
	}
#endif	/* LOW_LEVEL_TRACE */
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
	  d0 = PREG->y_u.slp.s;
	  ASP = ((CELL *)YREG) + d0;
	  ENDD(d0);
	}
      }
#else
      if (YREG > (CELL *) B)
	ASP = (CELL *) B;
      else {
	BEGD(d0);
	d0 = PREG->y_u.slp.s;
	ASP = ((CELL *) YREG) + d0;
	ENDD(d0);
      }
#endif /* FROZEN_STACKS */
      {
	CPredicate f = PREG->y_u.slp.p->cs.f_code;
	saveregs();
	SREG = (CELL *)((f)(PASS_REGS1));
	setregs();
      }
      if (!SREG) {
	/* be careful about error handling */
	if (PREG != FAILCODE)
	  PREG = PREG->y_u.slp.l;
      } else {
	PREG = NEXTOP(PREG, slp);
      }
      CACHE_A1();
      JMPNext();
      ENDBOp();

      BOp(try_c, OtapFs);
      //printf("try_c!!\n\n");
#ifdef YAPOR
      CUT_wait_leftmost();
#endif /* YAPOR */
      CACHE_Y(YREG);
#ifdef CUT_C
      /* Alocate space for the cut_c structure*/
      CUT_C_PUSH(NEXTOP(NEXTOP(PREG,OtapFs),OtapFs),S_YREG);
#endif
      S_YREG = S_YREG - PREG->y_u.OtapFs.extra;
      store_args(PREG->y_u.OtapFs.s);
      store_yaam_regs(NEXTOP(PREG, OtapFs), 0);
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif	/* YAPOR */
      SET_BB(B_YREG);
      ENDCACHE_Y();

    TRYCC:
      ASP = (CELL *)B;
      {
	CPredicate f = (CPredicate)(PREG->y_u.OtapFs.f);
	saveregs();
	SREG = (CELL *) ((f) (PASS_REGS1));
      	/* This last instruction changes B B*/
#ifdef CUT_C
	while (POP_CHOICE_POINT(B)){
	  cut_c_pop();
	}
#endif
	setregs();
      }
      if (!SREG) {
#ifdef CUT_C
	/* Removes the cut functions from the stack
	 without executing them because we have fail
	 and not cuted the predicate*/
	while(POP_CHOICE_POINT(B))
	  cut_c_pop();
#endif
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
      //printf("retry_c!!\n\n");
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
      //printf("cut_c!!\n");
      /*This is a phantom instruction. This is not executed by the WAM*/
#ifdef DEBUG
      /*If WAM executes this instruction, probably there's an error
       when we put this instruction, cut_c, after retry_c*/
#endif /*DEBUG*/
      printf ("ERROR: Should not print this message FILE: absmi.c %d\n",__LINE__);
      ENDBOp();

      BOp(try_userc, OtapFs);
      //printf("try_userc!!\n\n");
#ifdef YAPOR
      CUT_wait_leftmost();
#endif /* YAPOR */
      CACHE_Y(YREG);
#ifdef CUT_C
      /* Alocate space for the cut_c structure*/
      CUT_C_PUSH(NEXTOP(NEXTOP(PREG,OtapFs),OtapFs),S_YREG);
#endif
      S_YREG = S_YREG - PREG->y_u.OtapFs.extra;
      store_args(PREG->y_u.OtapFs.s);
      store_yaam_regs(NEXTOP(PREG, OtapFs), 0);
      B = B_YREG;
#ifdef YAPOR
      SCH_set_load(B_YREG);
#endif
      SET_BB(B_YREG);
      ENDCACHE_Y();
      LOCAL_PrologMode = UserCCallMode;
      ASP = YREG;
      /* for slots to work */
      Yap_StartSlots( );
      saveregs();
      save_machine_regs();
      SREG = (CELL *) YAP_ExecuteFirst(PREG->y_u.OtapFs.p, (CPredicate)(PREG->y_u.OtapFs.f));
      EX = NULL;
      restore_machine_regs();
      setregs();
      LOCAL_PrologMode = UserMode;
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
      //printf("retry_userc!!\n\n");
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
      LOCAL_PrologMode = UserMode;
      if (!SREG) {
#ifdef CUT_C
	/* Removes the cut functions from the stack
	 without executing them because we have fail
	 and not cuted the predicate*/
	while(POP_CHOICE_POINT(B))
	  cut_c_pop();
#endif
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

#ifdef CUT_C
      BOp(cut_userc, OtapFs);
      //printf("cut_userc!!\n");
      /*This is a phantom instruction. This is not executed by the WAM*/
#ifdef DEBUG
      /*If WAM executes this instruction, probably there's an error
       when we put this instruction, cut_userc, after retry_userc*/
      printf ("ERROR: Should not print this message FILE: absmi.c %d\n",__LINE__);
#endif /*DEBUG*/
      CACHE_A1();
      JMPNext();
      ENDBOp();
#endif


/************************************************************************\
*	support instructions             				 *
\************************************************************************/

      BOp(lock_pred, e);
      {
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,LOCK_PRED_INSTINIT);
///#endif
	PredEntry *ap = PredFromDefCode(PREG);
 	PELOCK(10,ap);
	PP = ap;
	if (!ap->NOfClauses) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(LOCK_PRED_FIRSTIFOK);
///#endif
	  UNLOCKPE(11,ap);
	  FAIL();
	}
	/*
	  we do not lock access to the predicate,
	  we must take extra care here
	*/
	if (ap->NOfClauses > 1 &&
	    !(ap->PredFlags & IndexedPredFlag)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(LOCK_PRED_SECONDTIFOK);
///#endif
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
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(LOCK_PRED_END);
///#endif
	PREG = ap->TrueCodeOfPred;
      }
      JMPNext();
      ENDBOp();

      BOp(index_pred, e);
      {
        EMIT_ENTRY_BLOCK(PREG,INDEX_PRED_INSTINIT);
	EMIT_MULTIPLE_DESTINY_BLOCK_TEST(INDEX_PRED_END);
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
    {
	  UNLOCKPE(14,ap);
    }

      }
      JMPNext();
      ENDBOp();

#if THREADS
      BOp(thread_local, e);
      {
	EMIT_ENTRY_BLOCK(PREG,THREAD_LOCAL_INSTINIT);
	PredEntry *ap = PredFromDefCode(PREG);
	ap = Yap_GetThreadPred(ap PASS_REGS);
	PREG = ap->CodeOfPred;
	CACHE_A1();
      }
      JMPNext();
      ENDBOp();
#endif

      BOp(expand_index, e);
      {
	  printf("expand index ainda não perfilado!\n");
	  exit(1);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,EXPAND_INDEX_INSTINIT);
///#endif
	PredEntry *pe = PredFromExpandCode(PREG);
	yamop *pt0;

	/* update ASP before calling IPred */
	SET_ASP(YREG, E_CB*sizeof(CELL));
#if defined(YAPOR) || defined(THREADS)
	if (!PP) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_INDEX_YAPOR_THREADS_NOPP);
///#endif
	  PELOCK(12,pe);
	}
	if (!same_lu_block(PREG_ADDR, PREG)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_INDEX_YAPOR_THREADS_IFOK_INIT);
///#endif
	  PREG = *PREG_ADDR;
	  if (!PP) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_INDEX_YAPOR_THREADS_IFOK_IFOK);
///#endif
	    UNLOCKPE(15,pe);
	  }
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_INDEX_YAPOR_THREADS_IFOK_END);
///#endif
	  JMPNext();
	}
#endif
#ifdef SHADOW_S
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_INDEX_NOYAPOR_NOTHREADS_SETS);
///#endif
	S = SREG;
#endif /* SHADOW_S */
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_INDEX_NOYAPOR_NOTHREADS_POST_SETS);
///#endif
 	saveregs();
	pt0 = Yap_ExpandIndex(pe, 0);
	/* restart index */
	setregs();
#ifdef SHADOW_S
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_INDEX_NOYAPOR_NOTHREADS_SETSREG);
///#endif
	SREG = S;
#endif /* SHADOW_S */
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_INDEX_NOYAPOR_NOTHREADS_POST_SETSREG);
///#endif
 	PREG = pt0;
#if defined(YAPOR) || defined(THREADS)
	if (!PP) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_INDEX_UNLOCK);
///#endif
	  UNLOCKPE(12,pe);
	}
#endif
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_INDEX_END);
///#endif
	JMPNext();
      }
      ENDBOp();

      BOp(expand_clauses, sssllp);
      {
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,EXPAND_CLAUSES_INSTINIT);
///#endif
	PredEntry *pe = PREG->y_u.sssllp.p;
	yamop *pt0;

	/* update ASP before calling IPred */
	SET_ASP(YREG, E_CB*sizeof(CELL));
#if defined(YAPOR) || defined(THREADS)
	if (PP == NULL) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_CLAUSES_YAPOR_THREADS_NOPP);
///#endif
	  PELOCK(13,pe);
	}
	if (!same_lu_block(PREG_ADDR, PREG)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_CLAUSES_YAPOR_THREADS_IFOK_INIT);
///#endif
	  PREG = *PREG_ADDR;
	  if (!PP) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_CLAUSES_YAPOR_THREADS_IFOK_IFOK);
///#endif
	    UNLOCKPE(16,pe);
	  }
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_CLAUSES_YAPOR_THREADS_IFOK_END);
///#endif
	  JMPNext();
	}
#endif
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_CLAUSES_NOYAPOR_NOTHREADS);
///#endif
 	saveregs();
	pt0 = Yap_ExpandIndex(pe, 0);
	/* restart index */
	setregs();
	UNLOCKPE(17,pe);
 	PREG = pt0;
#if defined(YAPOR) || defined(THREADS)
	if (!PP) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_CLAUSES_UNLOCK);
///#endif
	  UNLOCKPE(18,pe);
	}
#endif
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(EXPAND_CLAUSES_END);
///#endif
	JMPNext();
      }
      ENDBOp();

      BOp(undef_p, e);
      /* save S for module name */
      EMIT_ENTRY_BLOCK(PREG,UNDEF_P_INSTINIT);
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(UNDEF_P_END);
      {
	PredEntry *pe = PredFromDefCode(PREG);
	BEGD(d0);
	/* avoid trouble with undefined dynamic procedures */
	if ((pe->PredFlags & (DynamicPredFlag|LogUpdatePredFlag|MultiFileFlag)) ||
	    (UndefCode->OpcodeOfPred == UNDEF_OPCODE)) {
#if defined(YAPOR) || defined(THREADS)
	  PP = NULL;
#endif
	  UNLOCKPE(19,pe);
	  FAIL();
	}
	d0 = pe->ArityOfPE;
	UNLOCKPE(19,pe);
	if (d0 == 0) {
	  HR[1] = MkAtomTerm((Atom)(pe->FunctorOfPred));
	}
	else {
	  HR[d0 + 2] = AbsAppl(HR);
	  *HR = (CELL) pe->FunctorOfPred;
	  HR++;
	  BEGP(pt1);
	  pt1 = XREGS + 1;
	  for (; d0 > 0; --d0) {
	    BEGD(d1);
	    BEGP(pt0);
	    pt0 = pt1++;
	    d1 = *pt0;
	    deref_head(d1, undef_unk);
	  undef_nonvar:
	    /* just copy it to the heap */
	    *HR++ = d1;
	    continue;

        do {
          (pt0) = (CELL *)(d1);
          (d1) = *(CELL *)(d1);
          if(!IsVarTerm(d1)) goto undef_nonvar;
        undef_unk:;
        } while (Unsigned(pt0) != (d1));
		
	    if (pt0 <= HR) {
	      /* variable is safe */
	      *HR++ = (CELL)pt0;
	    } else {
	      /* bind it, in case it is a local variable */
	      d1 = Unsigned(HR);
	      RESET_VARIABLE(HR);
	      HR += 1;
	      Bind_Local(pt0, d1);
	    }
	    ENDP(pt0);
	    ENDD(d1);
	  }
	  ENDP(pt1);
	}
	ENDD(d0);
	HR[0] = Yap_Module_Name(pe);
	ARG1 = (Term) AbsPair(HR);
	HR += 2;
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
	low_level_trace(enter_pred,UndefCode,XREGS+1);
	}
#endif	/* LOW_LEVEL_TRACE */
      }

      PREG = UndefCode->CodeOfPred;
      /* for profiler */
      save_pc();
      CACHE_A1();
      JMPNext();
      ENDBOp();

      BOp(spy_pred, e);
    dospy:
      {
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,SPY_PRED_INSTINIT);
///#endif
	PredEntry *pe = PredFromDefCode(PREG);
	BEGD(d0);
 	PELOCK(14,pe);
	if (!(pe->PredFlags & IndexedPredFlag) &&
	      pe->NOfClauses > 1) {
	  /* update ASP before calling IPred */
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_FIRSTIFOK);
///#endif
	  SET_ASP(YREG, E_CB*sizeof(CELL));
	  saveregs();
	  Yap_IPred(pe, 0, CP);
	  /* IPred can generate errors, it thus must get rid of the lock itself */
	  setregs();
	}
	/* first check if we need to increase the counter */
	if ((pe->PredFlags & CountPredFlag)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_SECONDIFOK_INIT);
///#endif
	  LOCK(pe->StatisticsForPred->lock);
	  pe->StatisticsForPred->NOfEntries++;
	  UNLOCK(pe->StatisticsForPred->lock);
	  LOCAL_ReductionsCounter--;
	  if (LOCAL_ReductionsCounter == 0 && LOCAL_ReductionsCounterOn) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_SECONDIFOK_FIRSTIFOK);
///#endif
	    UNLOCKPE(20,pe);
	    saveregs();
	    Yap_NilError(CALL_COUNTER_UNDERFLOW,"");
	    setregs();
	    JMPNext();
	  }
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_SECONDIFOK_POST_FIRSTIF);
///#endif
	  LOCAL_PredEntriesCounter--;
	  if (LOCAL_PredEntriesCounter == 0 && LOCAL_PredEntriesCounterOn) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_SECONDIFOK_SECONDIFOK);
///#endif
	    UNLOCKPE(21,pe);
	    saveregs();
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW,"");
	    setregs();
	    JMPNext();
	  }
	  if ((pe->PredFlags & (CountPredFlag|ProfiledPredFlag|SpiedPredFlag)) ==
	    CountPredFlag) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_SECONDIFOK_THIRDIFOK);
///#endif
	    PREG = pe->TrueCodeOfPred;
	    UNLOCKPE(22,pe);
	    JMPNext();
	  }
	}
	/* standard profiler */
	if ((pe->PredFlags & ProfiledPredFlag)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_THIRDIFOK_INIT);
///#endif
	  LOCK(pe->StatisticsForPred->lock);
	  pe->StatisticsForPred->NOfEntries++;
	  UNLOCK(pe->StatisticsForPred->lock);
	  if (!(pe->PredFlags & SpiedPredFlag)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_THIRDIFOK_FIRSTIFOK);
///#endif
	    PREG = pe->TrueCodeOfPred;
	    UNLOCKPE(23,pe);
	    JMPNext();
	  }
	}
	if (1 /* !LOCAL_DebugOn */) {
///#ifdef PROFILED_ABSMI

    EMIT_SIMPLE_BLOCK(SPY_PRED_FOURTHIFOK);
///#endif
	  PREG = pe->TrueCodeOfPred;
	  UNLOCKPE(24,pe);
	  JMPNext();
	}
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_POST_FOURTHIF);
///#endif
	UNLOCKPE(25,pe);

	d0 = pe->ArityOfPE;
	/* save S for ModuleName */
	if (d0 == 0) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_D0ISZERO);
///#endif
	  HR[1] = MkAtomTerm((Atom)(pe->FunctorOfPred));
	} else {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_D0ISNOZERO_INIT);
///#endif
	  *HR = (CELL) pe->FunctorOfPred;
	  HR[d0 + 2] = AbsAppl(HR);
	  HR++;
	  BEGP(pt1);
	  pt1 = XREGS + 1;
	  for (; d0 > 0; --d0) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_D0ISNOZERO_INSIDEFOR_INIT);
///#endif
	    BEGD(d1);
	    BEGP(pt0);
	    pt0 = pt1++;
	    d1 = *pt0;
	    deref_head(d1, dospy_unk);
	  dospy_nonvar:
	    /* just copy it to the heap */
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_D0ISNOZERO_INSIDEFOR_DOSPY_NONVAR);
///#endif
	    *HR++ = d1;
	    continue;

	    derefa_body(d1, pt0, dospy_unk, dospy_nonvar);
	    if (pt0 <= HR) {
	      /* variable is safe */
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_D0ISNOZERO_INSIDEFOR_SAFEVAR);
///#endif
	      *HR++ = (CELL)pt0;
	    } else {
	      /* bind it, in case it is a local variable */
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_D0ISNOZERO_INSIDEFOR_UNSAFEVAR);
///#endif
	      d1 = Unsigned(HR);
	      RESET_VARIABLE(HR);
	      HR += 1;
	      Bind_Local(pt0, d1);
	    }
	    ENDP(pt0);
	    ENDD(d1);
	  }
	  ENDP(pt1);
	}
	ENDD(d0);
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_POST_IFS);
///#endif
	HR[0] = Yap_Module_Name(pe);
      }
      ARG1 = (Term) AbsPair(HR);
      HR += 2;
      {
	PredEntry *pt0;
#ifdef THREADS
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_THREADS_LOCK);
///#endif
	LOCK(GLOBAL_ThreadHandlesLock);
#endif
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_POST_LOCK);
///#endif
	pt0 = SpyCode;
	P_before_spy = PREG;
	PREG = pt0->CodeOfPred;
	/* for profiler */
#ifdef THREADS
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_THREADS_UNLOCK);
///#endif
	UNLOCK(GLOBAL_ThreadHandlesLock);
#endif
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_POST_UNLOCK);
///#endif
	save_pc();
	CACHE_A1();
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_LOW_LEVEL_TRACER);
///#endif
	  low_level_trace(enter_pred,pt0,XREGS+1);
	}
#endif	/* LOW_LEVEL_TRACE */
      }
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(SPY_PRED_END);
///#endif
      JMPNext();
      ENDBOp();


/************************************************************************\
* 	Try / Retry / Trust for main indexing blocks			*
\************************************************************************/

      BOp(try_clause, Otapl);
    EMIT_ENTRY_BLOCK(PREG,TRY_CLAUSE_INSTINIT);
      check_trail(TR);
      CACHE_Y(YREG);
      /* Point AP to the code that follows this instruction */
      store_at_least_one_arg(PREG->y_u.Otapl.s);
      store_yaam_regs(NEXTOP(PREG, Otapl), 0);
      PREG = PREG->y_u.Otapl.d;
      set_cut(S_YREG, B);
      B = B_YREG;
#ifdef YAPOR
    EMIT_SIMPLE_BLOCK_TEST(TRY_CLAUSE_YAPOR);
      SCH_set_load(B_YREG);
#endif	/* YAPOR */
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(TRY_CLAUSE_END);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      BOp(try_clause2, l);
    EMIT_ENTRY_BLOCK(PREG,TRY_CLAUSE2_INSTINIT);
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
    EMIT_SIMPLE_BLOCK_TEST(TRY_CLAUSE2_YAPOR);
      SCH_set_load(B_YREG);
#endif	/* YAPOR */
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(TRY_CLAUSE2_END);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      BOp(try_clause3, l);
    EMIT_ENTRY_BLOCK(PREG,TRY_CLAUSE3_INSTINIT);
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
    EMIT_SIMPLE_BLOCK_TEST(TRY_CLAUSE3_YAPOR);
      SCH_set_load(B_YREG);
#endif	/* YAPOR */
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(TRY_CLAUSE3_END);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      BOp(try_clause4, l);
    EMIT_ENTRY_BLOCK(PREG,TRY_CLAUSE4_INSTINIT);
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
      EMIT_SIMPLE_BLOCK_TEST(TRY_CLAUSE4_YAPOR);
      SCH_set_load(B_YREG);
#endif	/* YAPOR */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(TRY_CLAUSE4_END);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      BOp(retry, Otapl);
    EMIT_ENTRY_BLOCK(PREG,RETRY_INSTINIT);
      CACHE_Y(B);
      restore_yaam_regs(NEXTOP(PREG, Otapl));
      restore_at_least_one_arg(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
    EMIT_SIMPLE_BLOCK_TEST(RETRY_FROZEN);
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
    EMIT_SIMPLE_BLOCK_TEST(RETRY_NOFROZEN);
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(RETRY_END);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      PREG = PREG->y_u.Otapl.d;
      JMPNext();
      ENDBOp();

      BOp(retry2, l);
    EMIT_ENTRY_BLOCK(PREG,RETRY2_INSTINIT);
      CACHE_Y(B);
      restore_yaam_regs(NEXTOP(PREG, l));
      PREG = PREG->y_u.l.l;
      ARG1 = B_YREG->cp_a1;
      ARG2 = B_YREG->cp_a2;
#ifdef FROZEN_STACKS
    EMIT_SIMPLE_BLOCK_TEST(RETRY2_FROZEN);
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
    EMIT_SIMPLE_BLOCK_TEST(RETRY2_NOFROZEN);
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(RETRY2_END);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      BOp(retry3, l);
    EMIT_ENTRY_BLOCK(PREG,RETRY3_INSTINIT);
      CACHE_Y(B);
      restore_yaam_regs(NEXTOP(PREG, l));
      PREG = PREG->y_u.l.l;
      ARG1 = B_YREG->cp_a1;
      ARG2 = B_YREG->cp_a2;
      ARG3 = B_YREG->cp_a3;
#ifdef FROZEN_STACKS
    EMIT_SIMPLE_BLOCK_TEST(RETRY3_FROZEN);
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
    EMIT_SIMPLE_BLOCK_TEST(RETRY3_NOFROZEN);
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(RETRY3_END);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      BOp(retry4, l);
    EMIT_ENTRY_BLOCK(PREG,RETRY4_INSTINIT);
      CACHE_Y(B);
      restore_yaam_regs(NEXTOP(PREG, l));
      PREG = PREG->y_u.l.l;
      ARG1 = B_YREG->cp_a1;
      ARG2 = B_YREG->cp_a2;
      ARG3 = B_YREG->cp_a3;
      ARG4 = B_YREG->cp_a4;
#ifdef FROZEN_STACKS
    EMIT_SIMPLE_BLOCK_TEST(RETRY4_FROZEN);
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
      set_cut(S_YREG, B->cp_b);
#else
    EMIT_SIMPLE_BLOCK_TEST(RETRY4_NOFROZEN);
      set_cut(S_YREG, B_YREG->cp_b);
#endif /* FROZEN_STACKS */
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(RETRY4_END);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      BOp(trust, Otapl);
    EMIT_ENTRY_BLOCK(PREG,TRUST_INSTINIT);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
    EMIT_SIMPLE_BLOCK_TEST(TRUST_IFOK_INIT);
	SCH_last_alternative(PREG, B_YREG);
	restore_at_least_one_arg(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
    EMIT_SIMPLE_BLOCK_TEST(TRUST_IFOK_FROZEN);
        S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
    EMIT_SIMPLE_BLOCK_TEST(TRUST_IFOK_END);
	set_cut(S_YREG, B->cp_b);
      }
      else
#endif	/* YAPOR */
      {
    EMIT_SIMPLE_BLOCK_TEST(TRUST_NOIF_INIT);
	pop_yaam_regs();
	pop_at_least_one_arg(PREG->y_u.Otapl.s);
#ifdef FROZEN_STACKS
    EMIT_SIMPLE_BLOCK_TEST(TRUST_NOIF_FROZEN);
        S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif /* FROZEN_STACKS */
	set_cut(S_YREG, B);
      }
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(TRUST_END);
      SET_BB(B_YREG);
      ENDCACHE_Y();
      PREG = PREG->y_u.Otapl.d;
      JMPNext();
      ENDBOp();

      BOp(try_in, l);
    EMIT_ENTRY_BLOCK(PREG,TRY_IN_INSTINIT);
	EMIT_MULTIPLE_DESTINY_BLOCK_TEST(TRY_IN_END);
      B->cp_ap = NEXTOP(PREG, l);
      PREG = PREG->y_u.l.l;
      JMPNext();
      ENDBOp();



/************************************************************************\
* 	Logical Updates							*
\************************************************************************/

      /* enter logical pred               */
      BOp(enter_lu_pred, Illss);
      check_trail(TR);
      /* mark the indexing code */
      {
	LogUpdIndex *cl = PREG->y_u.Illss.I;
	PredEntry *ap = cl->ClPred;

	if (ap->LastCallOfPred != LUCALL_EXEC) {
	  /*
	    only increment time stamp if we are working on current time
	    stamp
	  */
	  if (ap->TimeStampOfPred >= TIMESTAMP_RESET)
	    Yap_UpdateTimestamps(ap);
	  ap->TimeStampOfPred++;
	  ap->LastCallOfPred = LUCALL_EXEC;
	  /*	  fprintf(stderr,"R %x--%d--%ul\n",ap,ap->TimeStampOfPred,ap->ArityOfPE);*/
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
#endif	/* YAPOR */
#ifdef YAPOR
	PP = PREG->y_u.OtaLl.d->ClPred;
#endif	/* YAPOR */
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
	if (!PP) {
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
	if (!PP) {
	  PELOCK(16,ap);
	  PP = ap;
	}
#endif
	if (!VALID_TIMESTAMP(timestamp, lcl)) {
	  /* jump to next alternative */
	  PREG = FAILCODE;
	} else {
	  PREG = lcl->ClCode;
	}
	/* HREY, leave indexing block alone!! */
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
	      if (lcl->ClRefCount == 1 && !(lcl->ClFlags & InUseMask)) {
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
#endif	/* YAPOR */
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
* 	Indexing in ARG1						*
\************************************************************************/

      BOp(user_switch, lp);
      {
    EMIT_ENTRY_BLOCK(PREG,USER_SWITCH_INSTINIT);
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(USER_SWITCH_END);
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
    EMIT_ENTRY_BLOCK(PREG,SWITCH_ON_TYPE_INSTINIT);
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(SWITCH_ON_TYPE_END);
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
      do {
        if(!IsVarTerm(d0)) goto swt_nvar;
      swt_unk:
        (pt0) = (CELL *)(d0);
        (d0) = *(CELL *)(d0);
      } while (Unsigned(pt0) != (d0));

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
    EMIT_ENTRY_BLOCK(PREG,SWITCH_LIST_NL_INSTINIT);
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(SWITCH_LIST_NL_END);
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
    EMIT_ENTRY_BLOCK(PREG,SWITCH_ON_ARG_TYPE_INSTINIT);
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(SWITCH_ON_ARG_TYPE_END);
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
    EMIT_ENTRY_BLOCK(PREG,SWITCH_ON_SUB_ARG_TYPE_INSTINIT);
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(SWITCH_ON_SUB_ARG_TYPE_END);
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
      do {
        if(!IsVarTerm(d0)) goto sub_arg_swt_nvar;
      sub_arg_swt_unk:
        (pt0) = (CELL *)(d0);
        (d0) = *(CELL *)(d0);
      } while (Unsigned(pt0) != (d0));

      /* variable */
      copy_jmp_address(PREG->y_u.sllll.l4);
      PREG = PREG->y_u.sllll.l4;
      JMPNext();
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();

      BOp(jump_if_var, l);
    EMIT_ENTRY_BLOCK(PREG,JUMP_IF_VAR_INSTINIT);
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(JUMP_IF_VAR_END);
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
    EMIT_ENTRY_BLOCK(PREG,JUMP_IF_NONVAR_INSTINIT);
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(JUMP_IF_NONVAR_END);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xll.x);
      deref_head(d0, jump2_if_unk);
      /* non var */
    jump2_if_nonvar:
      copy_jmp_address(PREG->y_u.xll.l1);
      PREG = PREG->y_u.xll.l1;
      JMPNext();

      BEGP(pt0);
      do {
        if(!IsVarTerm(d0)) goto jump2_if_nonvar;
      jump2_if_unk:
        (pt0) = (CELL *)(d0);
        (d0) = *(CELL *)(d0);
      } while (Unsigned(pt0) != (d0));

      /* variable */
      PREG = NEXTOP(PREG, xll);
      ENDP(pt0);
      JMPNext();
      ENDD(d0);
      ENDBOp();

      BOp(if_not_then, clll);
    EMIT_ENTRY_BLOCK(PREG,IF_NOT_THEN_INSTINIT);
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(IF_NOT_THEN_END);
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
      do {
        if(!IsVarTerm(d0)) goto if_n_nvar;
      if_n_unk:
        (pt0) = (CELL *)(d0);
        (d0) = *(CELL *)(d0);
      } while (Unsigned(pt0) != (d0));

      ENDP(pt0);
      /* variable */
      copy_jmp_address(PREG->y_u.clll.l3);
      PREG = PREG->y_u.clll.l3;
      JMPNext();
      ENDD(d0);
      ENDBOp();

/************************************************************************\
* 	Indexing on ARG1							*
\************************************************************************/

#define HRASH_SHIFT 6

      BOp(switch_on_func, sssl);
      EMIT_ENTRY_BLOCK(PREG,SWITCH_ON_FUNC_INSTINIT);
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(SWITCH_ON_FUNC_END);
      BEGD(d1);
      d1 = *SREG++;
      /* we use a very simple hash function to find elements in a
       * switch table */
      {
	CELL
	/* first, calculate the mask */
	  Mask = (PREG->y_u.sssl.s - 1) << 1,	/* next, calculate the hash function */
	  hash = d1 >> (HRASH_SHIFT - 1) & Mask;
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
      EMIT_ENTRY_BLOCK(PREG,SWITCH_ON_CONS_INSTINIT);
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(SWITCH_ON_CONS_END);
      BEGD(d1);
      d1 = I_R;
      /* we use a very simple hash function to find elements in a
       * switch table */
      {
	CELL
	/* first, calculate the mask */
	  Mask = (PREG->y_u.sssl.s - 1) << 1,	/* next, calculate the hash function */
	  hash = d1 >> (HRASH_SHIFT - 1) & Mask;
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
    EMIT_ENTRY_BLOCK(PREG,GO_ON_FUNC_INSTINIT);
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GO_ON_FUNC_END);
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
    EMIT_ENTRY_BLOCK(PREG,GO_ON_CONS_INSTINIT);
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(GO_ON_CONS_END);
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
    EMIT_ENTRY_BLOCK(PREG,IF_FUNC_INSTINIT);
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(IF_FUNC_END);
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
    EMIT_ENTRY_BLOCK(PREG,IF_CONS_INSTINIT);
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(IF_CONS_END);
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
      EMIT_ENTRY_BLOCK(PREG,INDEX_DBREF_INSTINIT);
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(INDEX_DBREF_END);
      PREG = NEXTOP(PREG, e);
      I_R = AbsAppl(SREG-1);
      GONext();
      ENDOp();

      Op(index_blob, e);
      EMIT_ENTRY_BLOCK(PREG,INDEX_BLOB_INSTINIT);
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(INDEX_BLOB_END);
      PREG = NEXTOP(PREG, e);
      I_R = Yap_DoubleP_key(SREG);
      GONext();
      ENDOp();

      Op(index_long, e);
      EMIT_ENTRY_BLOCK(PREG,INDEX_LONG_INSTINIT);
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(INDEX_LONG_END);
      PREG = NEXTOP(PREG, e);
      I_R = Yap_IntP_key(SREG);
      GONext();
      ENDOp();



/************************************************************************\
*	Native Code Execution						 *
\************************************************************************/

      /* native_me  */
      BOp(jit_handler, J);
	  
	  /* Needs to recompile */
      if (PREG->y_u.J.jh->jitman.torecomp) {
	    /* First: recompile on Smart JIT */
	    if (ExpEnv.config_struc.execution_mode == SMART_JIT) {
		  PREG->y_u.J.jh->jitman.torecomp = 0;
          recompile((void*)PREG);
		  if (NativeArea->area.p[PREG->y_u.J.jh->caa.naddress])
            goto native_lbl;
		  else {
		    PREG->y_u.J.jh->caa.taddress = -1;
			PREG->y_u.J.jh->caa.naddress = -1;
#if YAP_DBG_PREDS
            print_main_when_head(PREG, ON_PROFILED_INTERPRETER);
#endif
		    PREG = NEXTOP(PREG, J);
            JMPNext();
		  }
	    }
		/* Recompile on Continuous compilation */
	    else if (ExpEnv.config_struc.execution_mode == CONTINUOUS_COMPILATION) {
		  CELL i = 0;
		  while (i < ExpEnv.config_struc.compilation_threads && ExpEnv.config_struc.posthreads[i]) i++;
		  if (!ExpEnv.config_struc.posthreads[i]) {
            yamop* pt = PREG;
            ExpEnv.config_struc.posthreads[i] = 1;
            PREG->y_u.J.jh->jitman.used_thread = i;
            if(pthread_create(&ExpEnv.config_struc.threaded_compiler_threads[i], NULL, recompile, (void*)pt)) {
              fprintf(stderr, "Error creating thread\n");
              exit(1);
            }
            PREG->y_u.J.jh->jitman.torecomp = 0;
#if YAP_DBG_PREDS
            print_main_when_head(PREG, ON_PROFILED_INTERPRETER);
#endif
            PREG = NEXTOP(PREG, J);
            JMPNext();
          }
          else if (i == ExpEnv.config_struc.compilation_threads) {
#if YAP_DBG_PREDS
            print_main_when_head(PREG, ON_PROFILED_INTERPRETER);
#endif
	    PREG = NEXTOP(PREG, J);
            JMPNext();
		  }
	    }
	  }
	  /* Does not need to recompile */
	  else {
	    /* Do I have native function to execute? */
        if (PREG->y_u.J.jh->caa.naddress != -1 && NativeArea->area.p && NativeArea->area.p[PREG->y_u.J.jh->caa.naddress] && NativeArea->area.ok[PREG->y_u.J.jh->caa.naddress]) {
		  /* Yes, I do */
          native_lbl:;
	      void* (*callee)(yamop**,yamop**,CELL**,void*[],void*[]);
	      void* go;
          callee = (void*(*)(yamop**,yamop**,CELL**,void*[],void*[]))NativeArea->area.p[PREG->y_u.J.jh->caa.naddress];

	  HEADPREG = PREG;
#if YAP_STAT_PREDS
          getrusage(RUSAGE_SELF, &rustart);
#endif
#if YAP_DBG_PREDS
		  if (ExpEnv.debug_struc.pprint_me.nativerun_init != 0 && ExpEnv.debug_struc.pprint_me.nativerun_init != 0x1) {
	          fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);
		  fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.nativerun_init);
	        }
#endif
		  saveregs();
		  go = (*callee)(&P, &CPREG, &S, control_labels, OpAddress);
		  setregs();
#if YAP_STAT_PREDS
		  getrusage(RUSAGE_SELF, &ruend);
          timstart = rustart.ru_utime;
          timend = ruend.ru_utime;
		  NativeArea->t_runs[HEADPREG->y_u.J.jh->caa.naddress] += (((double)timend.tv_sec - (double)timstart.tv_sec) + ((double)timend.tv_usec - (double)timstart.tv_usec) / 1000000.0);
	      //fprintf(stdout, "Executou nativo!!\n\n");
#endif
		  
		  /* Do I need to recompile? */
	      if (HEADPREG->y_u.J.jh->jitman.torecomp) {
		    /* Yes. Manage intermediate code so that emitted BBs append to right last block */
		    IntermediatecodeArea->area.isactive[HEADPREG->y_u.J.jh->caa.taddress] = 1;
		    BlocksContext* b = IntermediatecodeArea->area.t[HEADPREG->y_u.J.jh->caa.taddress]->bc;
		    set_last_deeply(b, &(IntermediatecodeArea->area.lastblock[HEADPREG->y_u.J.jh->caa.taddress]));
		  }
        
		  goto *go;
        }
		/* No, I don't. So, Did PREG reach threshold value to become hot? */
        else if (PREG->y_u.J.jh->fi.bcst.c == ExpEnv.config_struc.frequency_bound && PREG->y_u.J.jh->caa.taddress != -1 && IntermediatecodeArea->area.t && IntermediatecodeArea->area.t[PREG->y_u.J.jh->caa.taddress] && IntermediatecodeArea->area.ok[PREG->y_u.J.jh->caa.taddress]) {
	      /* Yes, it did */
	      if (ExpEnv.config_struc.useonlypi) {
                /* Don't compile. Use only profiled insts.
		This will be executed only if "only_profiled_interpreter" is used */
         	IntermediatecodeArea->area.isactive[PREG->y_u.J.jh->caa.taddress] = 0;
		PREG->y_u.J.jh->fi.bcst.c = 2*ExpEnv.config_struc.frequency_bound;
#if YAP_DBG_PREDS
                print_main_when_head(PREG, ON_PROFILED_INTERPRETER);
                PREG = NEXTOP(PREG, J);
#endif
	        JMPNext();
	      }
		  
		  /* Compile on Smart JIT */
	      if (ExpEnv.config_struc.execution_mode == SMART_JIT) {
		    compile((void*)PREG);
		    PREG->y_u.J.jh->fi.bcst.c = 2*ExpEnv.config_struc.frequency_bound;
		    if (PREG->y_u.J.jh->caa.naddress != -1 && NativeArea->area.p[PREG->y_u.J.jh->caa.naddress]) // success
              goto native_lbl;
		    else { // fail
#if YAP_DBG_PREDS
              print_main_when_head(PREG, ON_PROFILED_INTERPRETER);
#endif
		      PREG = NEXTOP(PREG, J);
                      JMPNext();
		    }
		  }
		  /* Compile on Continuous compilation */
          else if (ExpEnv.config_struc.execution_mode == CONTINUOUS_COMPILATION) {
		    CELL i = 0;
			/* verifying thread availability */
		    while (i < ExpEnv.config_struc.compilation_threads && ExpEnv.config_struc.posthreads[i]) i++;
		    if (!ExpEnv.config_struc.posthreads[i]) {
			  /* I have available thread */
              yamop* pt = PREG;
              ExpEnv.config_struc.posthreads[i] = 1;
              PREG->y_u.J.jh->jitman.used_thread = i;
              if(pthread_create(&ExpEnv.config_struc.threaded_compiler_threads[i], NULL, compile, (void*)pt)) {
                fprintf(stderr, "Error creating thread\n");
                exit(1);
              }
              PREG->y_u.J.jh->fi.bcst.c = 2*ExpEnv.config_struc.frequency_bound;
#if YAP_DBG_PREDS
              print_main_when_head(PREG, ON_PROFILED_INTERPRETER);
#endif
              PREG = NEXTOP(PREG, J);
              JMPNext();
            }
            else if (i == ExpEnv.config_struc.compilation_threads) {
			  /* I don't have */
#if YAP_DBG_PREDS
              print_main_when_head(PREG, ON_PROFILED_INTERPRETER);
	          PREG = NEXTOP(PREG, J);
#endif
              JMPNext();
		    }
	      }
        }
		/* No, I don't have native pointer to execute and PREG did not become hot */
        else {
	      if (!PREG->y_u.J.jh->fi.bcst.c) PREG->y_u.J.jh->mf.isground = IsGround(PREG);
          PREG->y_u.J.jh->fi.bcst.c++;

		  /* Did PREG reach threshold value to become critical? */
          if (PREG->y_u.J.jh->fi.bcst.c == (COUNT)(ExpEnv.config_struc.frequency_bound*(ExpEnv.config_struc.profiling_startp)) && !PREG->y_u.J.jh->mf.isground) {
		    /* Yes, it did
               So... */
			critical_lbl:;
		    {
			  /* Set displacement */
			  ExpEnv.config_struc.current_displacement = ExpEnv.config_struc.TOTAL_OF_OPCODES;
			  /* Init new trace with NATIVE_ME_INSTINIT as first BB and ...*/
			  EMIT_ENTRY_BLOCK(PREG,JIT_HANDLER_INSTINIT);
              IntermediatecodeArea->area.t = (TraceContext**)realloc(IntermediatecodeArea->area.t, (IntermediatecodeArea->n+1)*sizeof(TraceContext*));
              IntermediatecodeArea->area.ok = (COUNT*)realloc(IntermediatecodeArea->area.ok, (IntermediatecodeArea->n+1)*sizeof(COUNT));
              IntermediatecodeArea->area.isactive = (COUNT*)realloc(IntermediatecodeArea->area.isactive, (IntermediatecodeArea->n+1)*sizeof(COUNT));
              IntermediatecodeArea->area.lastblock = (BlocksContext**)realloc(IntermediatecodeArea->area.lastblock, (IntermediatecodeArea->n+1)*sizeof(BlocksContext*));
#if YAP_STAT_PREDS
              IntermediatecodeArea->area.profiling_time = (double*)realloc(IntermediatecodeArea->area.profiling_time, (IntermediatecodeArea->n+1)*sizeof(double));
#endif
              PREG->y_u.J.jh->caa.taddress = IntermediatecodeArea->n;
              IntermediatecodeArea->n += 1;
              IntermediatecodeArea->area.isactive[PREG->y_u.J.jh->caa.taddress] = 1;
              IntermediatecodeArea->area.ok[PREG->y_u.J.jh->caa.taddress] = 1;
              IntermediatecodeArea->area.t[PREG->y_u.J.jh->caa.taddress] = (TraceContext*)malloc(sizeof(TraceContext));
              IntermediatecodeArea->area.t[PREG->y_u.J.jh->caa.taddress]->bc = NULL;
              IntermediatecodeArea->area.t[PREG->y_u.J.jh->caa.taddress]->tracesize = 0;
              IntermediatecodeArea->area.t[PREG->y_u.J.jh->caa.taddress]->n = 0;
              curtrace = &IntermediatecodeArea->area.t[PREG->y_u.J.jh->caa.taddress];
              headoftrace = PREG;
              ineedredefinedest = 0;
			  /* Define NATIVE_ME_INSTINIT as simple BB on traces initialized before the latter */
              EMIT_HEAD_BLOCK(PREG);
              IntermediatecodeArea->area.lastblock[PREG->y_u.J.jh->caa.taddress] = (*curtrace)->bc;
	        }
          }
          else if (PREG->y_u.J.jh->fi.bcst.c >= (COUNT)(ExpEnv.config_struc.frequency_bound*(ExpEnv.config_struc.profiling_startp)) && PREG->y_u.J.jh->fi.bcst.c < ExpEnv.config_struc.frequency_bound && !PREG->y_u.J.jh->mf.isground) {
		    /* No, PREG is critical but still not hot */
		    EMIT_ENTRY_BLOCK(PREG,JIT_HANDLER_INSTINIT);
	        {
	          curtrace = &IntermediatecodeArea->area.t[PREG->y_u.J.jh->caa.taddress];
              headoftrace = PREG;
              ineedredefinedest = 0;
              EMIT_HEAD_BLOCK(PREG);
              IntermediatecodeArea->area.lastblock[PREG->y_u.J.jh->caa.taddress] = (*curtrace)->bc;
	        }
	      }
	      else {
		    /* PREG is not critical */
	        EMIT_ENTRY_BLOCK(PREG,JIT_HANDLER_INSTINIT);
	      }
#if YAP_DBG_PREDS
          print_main_when_head(PREG, ON_PROFILED_INTERPRETER);
#endif
	      PREG = NEXTOP(PREG, J);
          JMPNext();
        }
      }
      ENDBOp();


 
/************************************************************************\
*	Basic Primitive Predicates					 *
\************************************************************************/

      Op(p_atom_x, xl);
    EMIT_ENTRY_BLOCK(PREG,P_ATOM_X_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xl.x);
      profiled_deref_head_TEST(d0, atom_x_unk);
    atom_x_nvar:
      if (IsAtomTerm(d0) && !IsBlob(AtomOfTerm(d0))) {
        EMIT_SIMPLE_BLOCK_TEST(P_ATOM_X_ATOM);
	PREG = NEXTOP(PREG, xl);
	GONext();
      }
      else {
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_ATOM_X_NOATOM);
	PREG = PREG->y_u.xl.F;
	GONext();
      }

      BEGP(pt0);
      profiled_deref_body(d0, pt0, atom_x_unk, atom_x_nvar);
    EMIT_SIMPLE_BLOCK_TEST(P_ATOM_X_NOATOM);
      PREG = PREG->y_u.xl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_atom_y, yl);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,P_ATOM_Y_INSTINIT);
///#endif
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yl.y;
      d0 = *pt0;
      deref_head(d0, atom_y_unk);
    atom_y_nvar:
      if (IsAtomTerm(d0) && !IsBlob(AtomOfTerm(d0))) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_ATOM_Y_IFOK);
///#endif
	PREG = NEXTOP(PREG, yl);
	GONext();
      }
      else {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_ATOM_Y_NOIF);
///#endif
	PREG = PREG->y_u.yl.F;
	GONext();
      }

      derefa_body(d0, pt0, atom_y_unk, atom_y_nvar);
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_ATOM_Y_END);
//#endif
      PREG = PREG->y_u.yl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_atomic_x, xl);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,P_ATOMIC_X_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xl.x);
      deref_head(d0, atomic_x_unk);
    atomic_x_nvar:
      /* non variable */
      if (IsAtomicTerm(d0)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_ATOMIC_X_NONVAR);
///#endif
	PREG = NEXTOP(PREG, xl);
	GONext();
      }
      else {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_ATOMIC_X_VAR);
///#endif
	PREG = PREG->y_u.xl.F;
	GONext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, atomic_x_unk, atomic_x_nvar);
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_ATOMIC_X_END);
//#endif
      PREG = PREG->y_u.xl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_atomic_y, yl);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,P_ATOMIC_Y_INSTINIT);
///#endif
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yl.y;
      d0 = *pt0;
      deref_head(d0, atomic_y_unk);
    atomic_y_nvar:
      /* non variable */
      if (IsAtomicTerm(d0)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_ATOMIC_Y_NONVAR);
///#endif
	PREG = NEXTOP(PREG, yl);
	GONext();
      }
      else {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_ATOMIC_Y_VAR);
//#endif
	PREG = PREG->y_u.yl.F;
	GONext();
      }

      derefa_body(d0, pt0, atomic_y_unk, atomic_y_nvar);
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_ATOMIC_Y_END);
///#endif
      PREG = PREG->y_u.yl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_integer_x, xl);
    EMIT_ENTRY_BLOCK(PREG,P_INTEGER_X_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xl.x);
      profiled_deref_head_TEST(d0, integer_x_unk);
    integer_x_nvar:
      /* non variable */
      if (IsIntTerm(d0)) {
    EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0)");
    EMIT_SIMPLE_BLOCK_TEST(P_INTEGER_X_INTEGER_X_NVAR_OK);
	PREG = NEXTOP(PREG, xl);
	GONext();
      }
      if (IsApplTerm(d0)) {
	Functor f0 = FunctorOfTerm(d0);
	if (IsExtensionFunctor(f0)) {
	  switch ((CELL)f0) {
	  case (CELL)FunctorLongInt:
	  case (CELL)FunctorBigInt:
    EMIT_CONDITIONAL_FAIL("IsIntTerm(d0)");
	EMIT_CONDITIONAL_SUCCESS("IsApplTerm(d0)");
	EMIT_CONDITIONAL_SUCCESS("IsExtensionFunctor(f0)");
	EMIT_CONDITIONAL_SUCCESS("(CELL)f0 == (CELL)FunctorLongInt || (CELL)f0 == (CELL)FunctorBigInt");
    EMIT_SIMPLE_BLOCK_TEST(P_INTEGER_X_INTEGER_X_NVAR_OK);
	    PREG = NEXTOP(PREG, xl);
	    GONext();
	  default:
    EMIT_CONDITIONAL_FAIL("IsIntTerm(d0)");
	EMIT_CONDITIONAL_SUCCESS("IsApplTerm(d0)");
	EMIT_CONDITIONAL_SUCCESS("IsExtensionFunctor(f0)");
	EMIT_CONDITIONAL_FAIL("(CELL)f0 == (CELL)FunctorLongInt || (CELL)f0 == (CELL)FunctorBigInt");
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_INTEGER_X_INTEGER_X_NVAR_NOOK);
	    PREG = PREG->y_u.xl.F;
	    GONext();
	  }
	}
      }
    EMIT_CONDITIONAL_FAIL("IsIntTerm(d0)");
	EMIT_CONDITIONAL_FAIL("IsApplTerm(d0)");
    EMIT_SIMPLE_BLOCK_TEST(P_INTEGER_X_INTEGER_X_NVAR_NOOK);
      PREG = PREG->y_u.xl.F;
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, integer_x_unk, integer_x_nvar);
    EMIT_SIMPLE_BLOCK_TEST(P_INTEGER_X_INTEGER_X_UNK);
      PREG = PREG->y_u.xl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_integer_y, yl);
    EMIT_ENTRY_BLOCK(PREG,P_INTEGER_Y_INSTINIT);
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yl.y;
      d0 = *pt0;
      profiled_deref_head_TEST(d0, integer_y_unk);
    integer_y_nvar:
      /* non variable */
      if (IsIntTerm(d0)) {
    EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0)");
    EMIT_SIMPLE_BLOCK_TEST(P_INTEGER_Y_INTEGER_Y_NVAR_OK);
	PREG = NEXTOP(PREG, yl);
	GONext();
      }
      if (IsApplTerm(d0)) {
	Functor f0 = FunctorOfTerm(d0);
	if (IsExtensionFunctor(f0)) {
	  switch ((CELL)f0) {
	  case (CELL)FunctorLongInt:
#ifdef USE_GMP
	  case (CELL)FunctorBigInt:
#endif
    EMIT_CONDITIONAL_FAIL("IsIntTerm(d0)");
	EMIT_CONDITIONAL_SUCCESS("IsApplTerm(d0)");
	EMIT_CONDITIONAL_SUCCESS("IsExtensionFunctor(f0)");
	EMIT_CONDITIONAL_SUCCESS(FUNCTOR_LARGE_INT);
    EMIT_SIMPLE_BLOCK_TEST(P_INTEGER_Y_INTEGER_Y_NVAR_OK);
	    PREG = NEXTOP(PREG, yl);
	    GONext();
	  default:
    EMIT_CONDITIONAL_FAIL("IsIntTerm(d0)");
	EMIT_CONDITIONAL_SUCCESS("IsApplTerm(d0)");
	EMIT_CONDITIONAL_SUCCESS("IsExtensionFunctor(f0)");
	EMIT_CONDITIONAL_FAIL(FUNCTOR_LARGE_INT);
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_INTEGER_Y_INTEGER_Y_NVAR_NOOK);
	    PREG = PREG->y_u.yl.F;
	    GONext();
	  }
	}
      }
    EMIT_CONDITIONAL_FAIL("IsIntTerm(d0)");
	EMIT_CONDITIONAL_FAIL("IsApplTerm(d0)");
    EMIT_SIMPLE_BLOCK_TEST(P_INTEGER_Y_INTEGER_Y_NVAR_NOOK);
      PREG = PREG->y_u.yl.F;
      GONext();

      profiled_derefa_body(d0, pt0, integer_y_unk, integer_y_nvar);
    EMIT_SIMPLE_BLOCK(P_INTEGER_Y_INTEGER_Y_UNK);
      PREG = PREG->y_u.yl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_nonvar_x, xl);
    EMIT_ENTRY_BLOCK(PREG,P_NONVAR_X_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xl.x);
      profiled_deref_head_TEST(d0, nonvar_x_unk);
    nonvar_x_nvar:
    EMIT_SIMPLE_BLOCK_TEST(P_NONVAR_X_NONVAR);
      PREG = NEXTOP(PREG, xl);
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, nonvar_x_unk, nonvar_x_nvar);
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_NONVAR_X_NONONVAR);
      PREG = PREG->y_u.xl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_nonvar_y, yl);
    EMIT_ENTRY_BLOCK(PREG,P_NONVAR_Y_INSTINIT);
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yl.y;
      d0 = *pt0;
      profiled_deref_head_TEST(d0, nonvar_y_unk);
    nonvar_y_nvar:
    EMIT_SIMPLE_BLOCK_TEST(P_NONVAR_Y_NONVAR);
      PREG = NEXTOP(PREG, yl);
      GONext();

      profiled_derefa_body(d0, pt0, nonvar_y_unk, nonvar_y_nvar);
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_NONVAR_Y_NONONVAR);
      PREG = PREG->y_u.yl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_number_x, xl);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,P_NUMBER_X_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xl.x);
      deref_head(d0, number_x_unk);
    number_x_nvar:
      /* non variable */

      if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_NUMBER_X_INT);
///#endif
	PREG = NEXTOP(PREG, xl);
	GONext();
      }
      if (IsApplTerm(d0)) {
	Functor f0 = FunctorOfTerm(d0);
	if (IsExtensionFunctor(f0)) {
	  switch ((CELL)f0) {
	  case (CELL)FunctorLongInt:
	  case (CELL)FunctorDouble:
#ifdef USE_GMP
	  case (CELL)FunctorBigInt:
#endif
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_NUMBER_X_FUNCTORINT);
///#endif
	    PREG = NEXTOP(PREG, xl);
	    GONext();
	  default:
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_NUMBER_X_FUNCTORDEFAULT);
///#endif
	    PREG = PREG->y_u.xl.F;
	    GONext();
	  }
	}
      }
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_NUMBER_X_POST_IF);
///#endif
      PREG = PREG->y_u.xl.F;
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, number_x_unk, number_x_nvar);
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_NUMBER_X_NUMBER_X_UNK);
///#endif
      PREG = PREG->y_u.xl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_number_y, yl);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,P_NUMBER_Y_INSTINIT);
///#endif
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yl.y;
      d0 = *pt0;
      deref_head(d0, number_y_unk);
    number_y_nvar:
      /* non variable */
      /* non variable */
      if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_NUMBER_Y_INT);
///#endif
	PREG = NEXTOP(PREG, xl);
	GONext();
      }
      if (IsApplTerm(d0)) {
	Functor f0 = FunctorOfTerm(d0);
	if (IsExtensionFunctor(f0)) {
	  switch ((CELL)f0) {
	  case (CELL)FunctorLongInt:
	  case (CELL)FunctorDouble:
#ifdef USE_GMP
	  case (CELL)FunctorBigInt:
#endif
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_NUMBER_Y_FUNCTORINT);
///#endif
	    PREG = NEXTOP(PREG, yl);
	    GONext();
	  default:
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_NUMBER_Y_FUNCTORDEFAULT);
///#endif
	    PREG = PREG->y_u.yl.F;
	    GONext();
	  }
	}
      }
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_NUMBER_Y_POST_IF);
///#endif
      PREG = PREG->y_u.xl.F;
      GONext();

      derefa_body(d0, pt0, number_y_unk, number_y_nvar);
///#ifdef PROFILED_ABSMI
	EMIT_SIMPLE_BLOCK(P_NUMBER_Y_NUMBER_Y_UNK);
///#endif
      PREG = PREG->y_u.yl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_var_x, xl);
    EMIT_ENTRY_BLOCK(PREG,P_VAR_X_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xl.x);
      profiled_deref_head_TEST(d0, var_x_unk);
    var_x_nvar:
      /* non variable */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_VAR_X_NONVAR);
      PREG = PREG->y_u.xl.F;
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, var_x_unk, var_x_nvar);
    EMIT_SIMPLE_BLOCK_TEST(P_VAR_X_VAR);
      PREG = NEXTOP(PREG, xl);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_var_y, yl);
    EMIT_ENTRY_BLOCK(PREG,P_VAR_Y_INSTINIT);
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yl.y;
      d0 = *pt0;
      profiled_deref_head_TEST(d0, var_y_unk);
    var_y_nvar:
      /* non variable */
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_VAR_Y_NONVAR);
      PREG = PREG->y_u.yl.F;
      GONext();

      profiled_derefa_body(d0, pt0, var_y_unk, var_y_nvar);
    EMIT_SIMPLE_BLOCK_TEST(P_VAR_Y_VAR);
      PREG = NEXTOP(PREG, yl);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_db_ref_x, xl);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,P_DB_REF_X_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xl.x);
      deref_head(d0, dbref_x_unk);
    dbref_x_nvar:
      /* non variable */
      if (IsDBRefTerm(d0)) {
	/* only allow references to the database, not general references
	 * to go through. */
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_DB_REF_X_DBREF);
///#endif
	PREG = NEXTOP(PREG, xl);
	GONext();
      }
      else {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_DB_REF_X_NODBREF);
///#endif
	PREG = PREG->y_u.xl.F;
	GONext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, dbref_x_unk, dbref_x_nvar);
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_DB_REF_X_DBREF_X_UNK);
///#endif
      PREG = PREG->y_u.xl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_db_ref_y, yl);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,P_DB_REF_Y_INSTINIT);
///#endif
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
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_DB_REF_Y_DBREF);
///#endif
	PREG = NEXTOP(PREG, yl);
	GONext();
      }
      else {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_DB_REF_Y_NODBREF);
///#endif
	PREG = PREG->y_u.yl.F;
	GONext();
      }

      derefa_body(d0, pt0, dbref_y_unk, dbref_y_nvar);
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_DB_REF_Y_DBREF_Y_UNK);
///#endif
      PREG = PREG->y_u.yl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_primitive_x, xl);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,P_PRIMITIVE_X_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xl.x);
      deref_head(d0, primi_x_unk);
    primi_x_nvar:
      /* non variable */
      if (IsPrimitiveTerm(d0)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_PRIMITIVE_X_PRIMITIVE);
///#endif
	PREG = NEXTOP(PREG, xl);
	GONext();
      }
      else {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_PRIMITIVE_X_NOPRIMITIVE);
///#endif
	PREG = PREG->y_u.xl.F;
	GONext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, primi_x_unk, primi_x_nvar);
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_PRIMITIVE_X_PRIMI_X_UNK);
///#endif
      PREG = PREG->y_u.xl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_primitive_y, yl);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,P_PRIMITIVE_Y_INSTINIT);
///#endif
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yl.y;
      d0 = *pt0;
      deref_head(d0, primi_y_unk);
    primi_y_nvar:
      /* non variable */
      if (IsPrimitiveTerm(d0)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_PRIMITIVE_Y_PRIMITIVE);
///#endif
	PREG = NEXTOP(PREG, yl);
	GONext();
      }
      else {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_PRIMITIVE_Y_NOPRIMITIVE);
///#endif
	PREG = PREG->y_u.yl.F;
	GONext();
      }

      derefa_body(d0, pt0, primi_y_unk, primi_y_nvar);
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_PRIMITIVE_Y_PRIMI_Y_UNK);
///#endif
      PREG = PREG->y_u.yl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_compound_x, xl);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,P_COMPOUND_X_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xl.x);
      deref_head(d0, compound_x_unk);
    compound_x_nvar:
      /* non variable */
      if (IsPairTerm(d0)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_COMPOUND_X_PAIR);
///#endif
	PREG = NEXTOP(PREG, xl);
	GONext();
      }
      else if (IsApplTerm(d0)) {
	if (IsExtensionFunctor(FunctorOfTerm(d0))) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_COMPOUND_X_APPL_IFOK);
///#endif
	  PREG = PREG->y_u.xl.F;
	  GONext();
	}
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_COMPOUND_X_APPL);
///#endif
	PREG = NEXTOP(PREG, xl);
	GONext();
      }
      else {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_COMPOUND_X_NOAPPL);
///#endif
	PREG = PREG->y_u.xl.F;
	GONext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, compound_x_unk, compound_x_nvar);
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_COMPOUND_X_COMPOUND_X_UNK);
///#endif
      PREG = PREG->y_u.xl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_compound_y, yl);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,P_COMPOUND_Y_INSTINIT);
///#endif
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yl.y;
      d0 = *pt0;
      deref_head(d0, compound_y_unk);
    compound_y_nvar:
      /* non variable */
      if (IsPairTerm(d0)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_COMPOUND_Y_PAIR);
///#endif
	PREG = NEXTOP(PREG, yl);
	GONext();
      }
      else if (IsApplTerm(d0)) {
	if (IsExtensionFunctor(FunctorOfTerm(d0))) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_COMPOUND_Y_APPL_IFOK);
///#endif
	  PREG = PREG->y_u.yl.F;
	  GONext();
	}
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_COMPOUND_Y_APPL);
///#endif
	PREG = NEXTOP(PREG, yl);
	GONext();
      }
      else {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_COMPOUND_Y_NOAPPL);
///#endif
	PREG = PREG->y_u.yl.F;
	GONext();
      }

      derefa_body(d0, pt0, compound_y_unk, compound_y_nvar);
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_COMPOUND_Y_COMPOUND_Y_UNK);
///#endif
      PREG = PREG->y_u.yl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_float_x, xl);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,P_FLOAT_X_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xl.x);
      deref_head(d0, float_x_unk);
    float_x_nvar:
      /* non variable */
      if (IsFloatTerm(d0)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_FLOAT_X_FLOAT);
///#endif
	PREG = NEXTOP(PREG, xl);
	GONext();
      }
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_FLOAT_X_POST_IF);
///#endif
      PREG = PREG->y_u.xl.F;
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, float_x_unk, float_x_nvar);
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_FLOAT_X_FLOAT_X_UNK);
///#endif
      PREG = PREG->y_u.xl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_float_y, yl);
///#ifdef PROFILED_ABSMI
    EMIT_ENTRY_BLOCK(PREG,P_FLOAT_Y_INSTINIT);
///#endif
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yl.y;
      d0 = *pt0;
      deref_head(d0, float_y_unk);
    float_y_nvar:
      /* non variable */
      if (IsFloatTerm(d0)) {
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_FLOAT_Y_FLOAT);
///#endif
	PREG = NEXTOP(PREG, yl);
	GONext();
      }
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_FLOAT_Y_POST_IF);
///#endif
      PREG = PREG->y_u.yl.F;
      GONext();

      derefa_body(d0, pt0, float_y_unk, float_y_nvar);
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_FLOAT_Y_FLOAT_Y_UNK);
///#endif
      PREG = PREG->y_u.yl.F;
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_plus_vv, xxx);
    EMIT_ENTRY_BLOCK(PREG,P_PLUS_VV_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      profiled_deref_head_TEST(d0, plus_vv_unk);
    plus_vv_nvar:
    EMIT_SIMPLE_BLOCK_TEST(P_PLUS_VV_PLUS_VV_NVAR);
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      profiled_deref_head_TEST(d1, plus_vv_nvar_unk);
    plus_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
    EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0) && IsIntTerm(d1)");
    EMIT_SIMPLE_BLOCK_TEST(P_PLUS_VV_PLUS_VV_NVAR_NVAR_INT);
	d0 = MkIntegerTerm(IntOfTerm(d0) + IntOfTerm(d1));
      }
      else {
    EMIT_CONDITIONAL_FAIL("IsIntTerm(d0) && IsIntTerm(d1)");
    EMIT_SIMPLE_BLOCK_TEST(P_PLUS_VV_PLUS_VV_NVAR_NVAR_NOINT);
	saveregs();
	d0 = p_plus(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
	if (d0 == 0L) {
	  saveregs();
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	  setregs();
	  FAIL();
	}
      }
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, plus_vv_unk, plus_vv_nvar);
    EMIT_SIMPLE_BLOCK_TEST(P_PLUS_VV_PLUS_VV_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is _+B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      profiled_deref_body(d1, pt0, plus_vv_nvar_unk, plus_vv_nvar_nvar);
    EMIT_SIMPLE_BLOCK_TEST(P_PLUS_VV_PLUS_VV_NVAR_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A+B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_plus_vc, xxn);
    EMIT_ENTRY_BLOCK(PREG,P_PLUS_VC_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      profiled_deref_head_TEST(d0, plus_vc_unk);
    plus_vc_nvar:
      {
	Int d1 = PREG->y_u.xxn.c;
	if (IsIntTerm(d0)) {
    EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0)");
    EMIT_SIMPLE_BLOCK_TEST(P_PLUS_VC_PLUS_VC_NVAR_INT);
	  d0 = MkIntegerTerm(IntOfTerm(d0) + d1);
	}
	else {
    EMIT_CONDITIONAL_FAIL("IsIntTerm(d0)");
    EMIT_SIMPLE_BLOCK_TEST(P_PLUS_VC_PLUS_VC_NVAR_NOINT);
	  saveregs();
	  d0 = p_plus(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	    setregs();
	    FAIL();
	  }
	}
      }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, plus_vc_unk, plus_vc_nvar);
    EMIT_SIMPLE_BLOCK_TEST(P_PLUS_VC_PLUS_VC_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A + " Int_FORMAT, PREG->y_u.xxn.c);
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_plus_y_vv, yxx);
    EMIT_ENTRY_BLOCK(PREG,P_PLUS_Y_VV_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      profiled_deref_head_TEST(d0, plus_y_vv_unk);
    plus_y_vv_nvar:
    EMIT_SIMPLE_BLOCK_TEST(P_PLUS_Y_VV_PLUS_Y_VV_NVAR);
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      profiled_deref_head_TEST(d1, plus_y_vv_nvar_unk);
    plus_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
    EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0) && IsIntTerm(d1)");
    EMIT_SIMPLE_BLOCK_TEST(P_PLUS_Y_VV_PLUS_Y_VV_NVAR_NVAR_INT);
	d0 = MkIntegerTerm(IntOfTerm(d0) + IntOfTerm(d1));
      }
      else {
    EMIT_CONDITIONAL_FAIL("IsIntTerm(d0) && IsIntTerm(d1)");
    EMIT_SIMPLE_BLOCK_TEST(P_PLUS_Y_VV_PLUS_Y_VV_NVAR_NVAR_NOINT);
	saveregs();
	d0 = p_plus(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
	if (d0 == 0L) {
	  saveregs();
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
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
      profiled_deref_body(d0, pt0, plus_y_vv_unk, plus_y_vv_nvar);
    EMIT_SIMPLE_BLOCK_TEST(P_PLUS_Y_VV_PLUS_Y_VV_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A+B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      profiled_deref_body(d1, pt0, plus_y_vv_nvar_unk, plus_y_vv_nvar_nvar);
    EMIT_SIMPLE_BLOCK_TEST(P_PLUS_Y_VV_PLUS_Y_VV_NVAR_UNK);
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
      EMIT_ENTRY_BLOCK(PREG,P_PLUS_Y_VC_INSTINIT);
      /* first check pt1 */
      profiled_deref_head_TEST(d0, plus_y_vc_unk);

    plus_y_vc_nvar:
      {
	Int d1 = PREG->y_u.yxn.c;
	if (IsIntTerm(d0)) {
      EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0)");
      EMIT_SIMPLE_BLOCK_TEST(P_PLUS_Y_VC_PLUS_Y_VC_NVAR_INT);
	  d0 = MkIntegerTerm(IntOfTerm(d0) + d1);
	}
	else {
      EMIT_CONDITIONAL_FAIL("IsIntTerm(d0)");
      EMIT_SIMPLE_BLOCK_TEST(P_PLUS_Y_VC_PLUS_Y_VC_NVAR_NOINT);
	  saveregs();
	  d0 = p_plus(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
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
      profiled_deref_body(d0, pt0, plus_y_vc_unk, plus_y_vc_nvar);
      EMIT_SIMPLE_BLOCK_TEST(P_PLUS_Y_VC_PLUS_Y_VC_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A + " Int_FORMAT, PREG->y_u.yxn.c);
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_minus_vv, xxx);
      EMIT_ENTRY_BLOCK(PREG,P_MINUS_VV_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      profiled_deref_head_TEST(d0, minus_vv_unk);
    minus_vv_nvar:
      EMIT_SIMPLE_BLOCK_TEST(P_MINUS_VV_MINUS_VV_NVAR);
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      profiled_deref_head_TEST(d1, minus_vv_nvar_unk);
    minus_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
      EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0) && IsIntTerm(d1)");
      EMIT_SIMPLE_BLOCK_TEST(P_MINUS_VV_MINUS_VV_NVAR_NVAR_INT);
	d0 = MkIntegerTerm(IntOfTerm(d0) - IntOfTerm(d1));
      }
      else {
      EMIT_CONDITIONAL_FAIL("IsIntTerm(d0) && IsIntTerm(d1)");
      EMIT_SIMPLE_BLOCK_TEST(P_MINUS_VV_MINUS_VV_NVAR_NVAR_NOINT);
	saveregs();
	d0 = p_minus(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
	if (d0 == 0L) {
	  saveregs();
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	  setregs();
	  FAIL();
	}
      }
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, minus_vv_unk, minus_vv_nvar);
      EMIT_SIMPLE_BLOCK_TEST(P_MINUS_VV_MINUS_VV_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A-B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      profiled_deref_body(d1, pt0, minus_vv_nvar_unk, minus_vv_nvar_nvar);
      EMIT_SIMPLE_BLOCK_TEST(P_MINUS_VV_MINUS_VV_NVAR_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A-B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_minus_cv, xxn);
      EMIT_ENTRY_BLOCK(PREG,P_MINUS_CV_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      profiled_deref_head_TEST(d0, minus_cv_unk);
    minus_cv_nvar:
      {
	Int d1 = PREG->y_u.xxn.c;
	if (IsIntTerm(d0)) {
      EMIT_SIMPLE_BLOCK_TEST(P_MINUS_CV_MINUS_CV_NVAR_INT);
	  d0 = MkIntegerTerm(d1 - IntOfTerm(d0));
	}
	else {
      EMIT_SIMPLE_BLOCK_TEST(P_MINUS_CV_MINUS_CV_NVAR_NOINT);
	  saveregs();
	  d0 = p_minus(MkIntegerTerm(d1),Yap_Eval(d0) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	    setregs();
	    FAIL();
	  }
	}
      }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, minus_cv_unk, minus_cv_nvar);
      EMIT_SIMPLE_BLOCK_TEST(P_MINUS_CV_MINUS_CV_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is " Int_FORMAT "-A", PREG->y_u.xxn.c);
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_minus_y_vv, yxx);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_MINUS_Y_VV_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, minus_y_vv_unk);
    minus_y_vv_nvar:
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_MINUS_Y_VV_MINUS_Y_VV_NVAR);
///#endif
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, minus_y_vv_nvar_unk);
    minus_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_MINUS_Y_VV_INTTERM);
///#endif
	d0 = MkIntegerTerm(IntOfTerm(d0) - IntOfTerm(d1));
      }
      else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_MINUS_Y_VV_NOINTTERM);
///#endif
	saveregs();
	d0 = p_minus(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
	if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_MINUS_Y_VV_D0EQUALS0L);
///#endif
	  saveregs();
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	  setregs();
	  FAIL();
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_MINUS_Y_VV_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, minus_y_vv_unk, minus_y_vv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_MINUS_Y_VV_MINUS_Y_VV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A-B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, minus_y_vv_nvar_unk, minus_y_vv_nvar_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_MINUS_Y_VV_MINUS_Y_VV_NVAR_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A-B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_minus_y_cv, yxn);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_MINUS_Y_CV_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, minus_y_cv_unk);
    minus_y_cv_nvar:
      {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_MINUS_Y_CV_MINUS_Y_CV_NVAR);
///#endif
	Int d1 = PREG->y_u.yxn.c;
	if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_MINUS_Y_CV_INTTERM);
///#endif
	  d0 = MkIntegerTerm(d1 - IntOfTerm(d0));
	}
	else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_MINUS_Y_CV_NOINTTERM);
///#endif
	  saveregs();
	  d0 = p_minus(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_MINUS_Y_CV_D0EQUALS0L);
///#endif
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	    setregs();
	    FAIL();
	  }
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_MINUS_Y_CV_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, minus_y_cv_unk, minus_y_cv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_MINUS_Y_CV_MINUS_Y_CV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is " Int_FORMAT "-A", PREG->y_u.yxn.c);
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_times_vv, xxx);
      EMIT_ENTRY_BLOCK(PREG,P_TIMES_VV_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      profiled_deref_head_TEST(d0, times_vv_unk);
    times_vv_nvar:
      EMIT_SIMPLE_BLOCK_TEST(P_TIMES_VV_TIMES_VV_NVAR);
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      profiled_deref_head_TEST(d1, times_vv_nvar_unk);
    times_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
      EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0) && IsIntTerm(d1)");
      EMIT_SIMPLE_BLOCK_TEST(P_TIMES_VV_TIMES_VV_NVAR_NVAR_INT);
	d0 = times_int(IntOfTerm(d0), IntOfTerm(d1) PASS_REGS);
      }
      else {
      EMIT_CONDITIONAL_FAIL("IsIntTerm(d0) && IsIntTerm(d1)");
      EMIT_SIMPLE_BLOCK_TEST(P_TIMES_VV_TIMES_VV_NVAR_NVAR_NOINT);
	saveregs();
	d0 = p_times(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
	if (d0 == 0L) {
	  saveregs();
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	  setregs();
	  FAIL();
	}
      }
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, times_vv_unk, times_vv_nvar);
      EMIT_SIMPLE_BLOCK_TEST(P_TIMES_VV_TIMES_VV_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A*B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      profiled_deref_body(d1, pt0, times_vv_nvar_unk, times_vv_nvar_nvar);
      EMIT_SIMPLE_BLOCK_TEST(P_TIMES_VV_TIMES_VV_NVAR_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A*B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_times_vc, xxn);
      EMIT_ENTRY_BLOCK(PREG,P_TIMES_VC_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      profiled_deref_head_TEST(d0, times_vc_unk);
    times_vc_nvar:
      {
	Int d1 = PREG->y_u.xxn.c;
	if (IsIntTerm(d0)) {
      EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0)");
      EMIT_SIMPLE_BLOCK_TEST(P_TIMES_VC_TIMES_VC_NVAR_INT);
	  d0 = times_int(IntOfTerm(d0), d1 PASS_REGS);
	}
	else {
      EMIT_CONDITIONAL_FAIL("IsIntTerm(d0)");
      EMIT_SIMPLE_BLOCK_TEST(P_TIMES_VC_TIMES_VC_NVAR_NOINT);
	  saveregs();
	  d0 = p_times(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	    setregs();
	    FAIL();
	  }
	}
      }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, times_vc_unk, times_vc_nvar);
      EMIT_SIMPLE_BLOCK_TEST(P_TIMES_VC_TIMES_VC_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A* " Int_FORMAT, PREG->y_u.xxn.c);
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_times_y_vv, yxx);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_TIMES_Y_VV_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, times_y_vv_unk);
    times_y_vv_nvar:
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_TIMES_Y_VV_TIMES_Y_VV_NVAR);
///#endif
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, times_y_vv_nvar_unk);
    times_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_TIMES_Y_VV_INTTERM);
///#endif
	d0 = times_int(IntOfTerm(d0), IntOfTerm(d1) PASS_REGS);
      }
      else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_TIMES_Y_VV_NOINTTERM);
///#endif
	saveregs();
	d0 = p_times(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
	if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_TIMES_Y_VV_D0EQUALS0L);
///#endif
	  saveregs();
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	  setregs();
	  FAIL();
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_TIMES_Y_VV_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, times_y_vv_unk, times_y_vv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_TIMES_Y_VV_TIMES_Y_VV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A*B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, times_y_vv_nvar_unk, times_y_vv_nvar_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_TIMES_Y_VV_TIMES_Y_VV_NVAR_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A*B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_times_y_vc, yxn);
      EMIT_ENTRY_BLOCK(PREG,P_TIMES_Y_VC_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      profiled_deref_head_TEST(d0, times_y_vc_unk);
    times_y_vc_nvar:
      {
	Int d1 = PREG->y_u.yxn.c;
	if (IsIntTerm(d0)) {
      EMIT_SIMPLE_BLOCK_TEST(P_TIMES_Y_VC_TIMES_Y_VC_NVAR_INT);
	  d0 = times_int(IntOfTerm(d0), d1 PASS_REGS);
	}
	else {
      EMIT_SIMPLE_BLOCK_TEST(P_TIMES_Y_VC_TIMES_Y_VC_NVAR_NOINT);
	  saveregs();
	  d0 = p_times(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
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
      profiled_deref_body(d0, pt0, times_y_vc_unk, times_y_vc_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK_TEST(P_TIMES_Y_VC_TIMES_Y_VC_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A* " Int_FORMAT, PREG->y_u.yxn.c);
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_div_vv, xxx);
      EMIT_ENTRY_BLOCK(PREG,P_DIV_VV_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      profiled_deref_head_TEST(d0, div_vv_unk);
    div_vv_nvar:
      EMIT_SIMPLE_BLOCK_TEST(P_DIV_VV_DIV_VV_NVAR);
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      profiled_deref_head_TEST(d1, div_vv_nvar_unk);
    div_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
      EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0) && IsIntTerm(d1)");
      EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_DIV_VV_DIV_VV_NVAR_NVAR_INT);
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
      EMIT_CONDITIONAL_FAIL("IsIntTerm(d0) && IsIntTerm(d1)");
      EMIT_SIMPLE_BLOCK_TEST(P_DIV_VV_DIV_VV_NVAR_NVAR_NOINT);
          saveregs();
	d0 = p_div(Yap_Eval(d0), Yap_Eval(d1)  PASS_REGS);
	setregs();
	if (d0 == 0L) {
	  saveregs();
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	  setregs();
	  FAIL();
	}
      }
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, div_vv_unk, div_vv_nvar);
      EMIT_SIMPLE_BLOCK_TEST(P_DIV_VV_DIV_VV_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A//B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      profiled_deref_body(d1, pt0, div_vv_nvar_unk, div_vv_nvar_nvar);
      EMIT_SIMPLE_BLOCK_TEST(P_DIV_VV_DIV_VV_NVAR_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A//B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_div_vc, xxn);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_DIV_VC_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, div_vc_unk);
    div_vc_nvar:
      {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_VC_DIV_VC_NVAR);
///#endif
	Int d1 = PREG->y_u.xxn.c;
	if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_VC_INTTERM);
///#endif
	  d0 = MkIntTerm(IntOfTerm(d0) / d1);
	}
	else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_VC_NOINTTERM);
///#endif
	  saveregs();
	  d0 = p_div(Yap_Eval(d0),MkIntegerTerm(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_VC_D0EQUALS0L);
///#endif
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	    setregs();
	    FAIL();
	  }
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_VC_NVAR_END);
///#endif
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_vc_unk, div_vc_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_VC_DIV_VC_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A//B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_div_cv, xxn);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_DIV_CV_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, div_cv_unk);
    div_cv_nvar:
      {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_CV_DIV_CV_NVAR);
///#endif
	Int d1 = PREG->y_u.xxn.c;
	if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_CV_INTTERM_INIT);
///#endif
	  Int div = IntOfTerm(d0);
	  if (div == 0){
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_CV_INTTERM_DIVEQUALS0);
///#endif
	    saveregs();
	    Yap_NilError(EVALUATION_ERROR_ZERO_DIVISOR,"// /2");
	    setregs();
	    FAIL();
	  }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_CV_INTTERM_END);
///#endif
	  d0 = MkIntegerTerm(d1 / div);
	}
	else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_CV_NOINTTERM);
///#endif
	  saveregs();
	  d0 = p_div(MkIntegerTerm(d1),Yap_Eval(d0) PASS_REGS);
	  if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_CV_D0EQUALS0L);
///#endif
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	    setregs();
	    FAIL();
	  }
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_CV_NVAR_END);
///#endif
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_cv_unk, div_cv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_CV_DIV_CV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is " Int_FORMAT "// A", PREG->y_u.xxn.c);
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_div_y_vv, yxx);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_DIV_Y_VV_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, div_y_vv_unk);
    div_y_vv_nvar:
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VV_DIV_Y_VV_NVAR);
///#endif
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, div_y_vv_nvar_unk);
    div_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VV_INTTERM_INIT);
///#endif
	Int div = IntOfTerm(d1);
	if (div == 0) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VV_INTTERM_DIVEQUALS0);
///#endif
	  saveregs();
	  Yap_NilError(EVALUATION_ERROR_ZERO_DIVISOR,"// /2");
	  setregs();
	  FAIL();
	}
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VV_INTTERM_END);
///#endif
	d0 = MkIntTerm(IntOfTerm(d0) / div);
      }
      else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VV_NOINTTERM);
///#endif
	saveregs();
	d0 = p_div(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
	if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VV_D0EQUALS0L);
///#endif
	  saveregs();
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	  setregs();
	  FAIL();
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VV_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_y_vv_unk, div_y_vv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VV_DIV_Y_VV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A//B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, div_y_vv_nvar_unk, div_y_vv_nvar_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VV_DIV_Y_VV_NVAR_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A//B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_div_y_vc, yxn);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_DIV_Y_VC_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, div_y_vc_unk);
    div_y_vc_nvar:
      {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VC_DIV_Y_VC_NVAR);
///#endif
	Int d1 = PREG->y_u.yxn.c;
	if (IsIntTerm(d0)) {
//#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VC_INTTERM);
///#endif
	  d0 = MkIntTerm(IntOfTerm(d0)/d1);
	}
	else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VC_NOINTTERM);
///#endif
	  saveregs();
	  d0 = p_div(Yap_Eval(d0),MkIntegerTerm(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VC_D0EQUALS0L);
///#endif
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	    setregs();
	    FAIL();
	  }
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VC_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_y_vc_unk, div_y_vc_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_VC_DIV_Y_VC_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A//B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_div_y_cv, yxn);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_DIV_Y_CV_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, div_y_cv_unk);
    div_y_cv_nvar:
      {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_CV_DIV_Y_CV_NVAR);
///#endif
	Int d1 = PREG->y_u.yxn.c;
	if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_CV_INTTERM_INIT);
///#endif
	  Int div = IntOfTerm(d0);
	  if (div == 0) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_CV_INTTERM_DIVEQUALS0);
///#endif
	    saveregs();
	    Yap_NilError(EVALUATION_ERROR_ZERO_DIVISOR,"// /2");
	    setregs();
	    FAIL();
	  }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_CV_INTTERM_END);
///#endif
	  d0 = MkIntegerTerm(d1 / div);
	}
	else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_CV_NOINTTERM);
///#endif
	  saveregs();
	  d0 = p_div(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_CV_D0EQUALS0L);
///#endif
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	    setregs();
	    FAIL();
	  }
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_CV_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_y_cv_unk, div_y_cv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_DIV_Y_CV_DIV_Y_CV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is " Int_FORMAT "// A", PREG->y_u.yxn.c);
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();


      Op(p_and_vv, xxx);
      EMIT_ENTRY_BLOCK(PREG,P_AND_VV_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      profiled_deref_head_TEST(d0, and_vv_unk);
    and_vv_nvar:
      EMIT_SIMPLE_BLOCK_TEST(P_AND_VV_AND_VV_NVAR);
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      profiled_deref_head_TEST(d1, and_vv_nvar_unk);
    and_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
      EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0) && IsIntTerm(d1)");
      EMIT_SIMPLE_BLOCK_TEST(P_AND_VV_AND_VV_NVAR_NVAR_INT);
	d0 = MkIntegerTerm(IntOfTerm(d0) & IntOfTerm(d1));
      }
      else {
      EMIT_CONDITIONAL_FAIL("IsIntTerm(d0) && IsIntTerm(d1)");
      EMIT_SIMPLE_BLOCK_TEST(P_AND_VV_AND_VV_NVAR_NVAR_NOINT);
	saveregs();
	d0 = p_and(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
	if (d0 == 0L) {
	  saveregs();
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	  setregs();
	  FAIL();
	}
      }
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, and_vv_unk, and_vv_nvar);
      EMIT_SIMPLE_BLOCK_TEST(P_AND_VV_AND_VV_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A/\\B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      profiled_deref_body(d1, pt0, and_vv_nvar_unk, and_vv_nvar_nvar);
      EMIT_SIMPLE_BLOCK_TEST(P_AND_VV_AND_VV_NVAR_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A/\\B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_and_vc, xxn);
      EMIT_ENTRY_BLOCK(PREG,P_AND_VC_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      profiled_deref_head_TEST(d0, and_vc_unk);
    and_vc_nvar:
      {
	Int d1 = PREG->y_u.xxn.c;
	if (IsIntTerm(d0)) {
      EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0)");
      EMIT_SIMPLE_BLOCK_TEST(P_AND_VC_AND_VC_NVAR_INT);
	  d0 = MkIntegerTerm(IntOfTerm(d0) & d1);
	}
	else {
      EMIT_CONDITIONAL_FAIL("IsIntTerm(d0)");
      EMIT_SIMPLE_BLOCK_TEST(P_AND_VC_AND_VC_NVAR_NOINT);
	  saveregs();
	  d0 = p_and(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	    setregs();
	    FAIL();
	  }
	}
      }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, and_vc_unk, and_vc_nvar);
      EMIT_SIMPLE_BLOCK_TEST(P_AND_VC_AND_VC_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A /\\ " Int_FORMAT , PREG->y_u.xxn.c);
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_and_y_vv, yxx);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_AND_Y_VV_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, and_y_vv_unk);
    and_y_vv_nvar:
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_AND_Y_VV_AND_Y_VV_NVAR);
///#endif
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, and_y_vv_nvar_unk);
    and_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_AND_Y_VV_INTTERM);
///#endif
	d0 = MkIntegerTerm(IntOfTerm(d0) & IntOfTerm(d1));
      }
      else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_AND_Y_VV_NOINTTERM);
///#endif
	saveregs();
	d0 = p_and(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
	if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_AND_Y_VV_D0EQUALS0L);
///#endif
	  saveregs();
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	  setregs();
	  FAIL();
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_AND_Y_VV_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, and_y_vv_unk, and_y_vv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_AND_Y_VV_AND_Y_VV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A/\\B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, and_y_vv_nvar_unk, and_y_vv_nvar_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_AND_Y_VV_AND_Y_VV_NVAR_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A/\\B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_and_y_vc, yxn);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_AND_Y_VC_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, and_y_vc_unk);
    and_y_vc_nvar:
      {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_AND_Y_VC_AND_Y_VC_NVAR);
///#endif
	Int d1 = PREG->y_u.yxn.c;
	if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_AND_Y_VC_INTTERM);
///#endif
	  d0 = MkIntegerTerm(IntOfTerm(d0) & d1);
	}
	else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_AND_Y_VC_NOINTTERM);
///#endif
	  saveregs();
	  d0 = p_and(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_AND_Y_VC_D0EQUALS0L);
///#endif
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	    setregs();
	    FAIL();
	  }
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_AND_Y_VC_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, and_y_vc_unk, and_y_vc_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_AND_Y_VC_AND_Y_VC_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A /\\ " Int_FORMAT , PREG->y_u.yxn.c);
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();


      Op(p_or_vv, xxx);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_OR_VV_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, or_vv_unk);
    or_vv_nvar:
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_VV_OR_VV_NVAR);
///#endif
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, or_vv_nvar_unk);
    or_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_VV_INTTERM);
///#endif
	d0 = MkIntegerTerm(IntOfTerm(d0) | IntOfTerm(d1));
      }
      else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_VV_NOINTTERM);
///#endif
	saveregs();
	d0 = p_or(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
	if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_VV_D0EQUALS0L);
///#endif
	  saveregs();
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	  setregs();
	  FAIL();
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_VV_NVAR_END);
///#endif
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, or_vv_unk, or_vv_nvar);
//#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_VV_OR_VV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A\\/B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, or_vv_nvar_unk, or_vv_nvar_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_VV_OR_VV_NVAR_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A\\/B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_or_vc, xxn);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_OR_VC_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, or_vc_unk);
    or_vc_nvar:
      {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_VC_OR_VC_NVAR);
///#endif
	Int d1 = PREG->y_u.xxn.c;
	if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_VC_INTTERM);
///#endif
	  d0 = MkIntegerTerm(IntOfTerm(d0) | d1);
	}
	else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_VC_NOINTTERM);
///#endif
	  saveregs();
	  d0 = p_or(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	  if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_VC_D0EQUALS0L);
///#endif
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	    setregs();
	    FAIL();
	  }
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_VC_NVAR_END);
///#endif
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, or_vc_unk, or_vc_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_VC_OR_VC_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A \\/ " Int_FORMAT , PREG->y_u.xxn.c);
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_or_y_vv, yxx);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_OR_Y_VV_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, or_y_vv_unk);
    or_y_vv_nvar:
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_Y_VV_OR_Y_VV_NVAR);
///#endif
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, or_y_vv_nvar_unk);
    or_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_Y_VV_INTTERM);
///#endif
	d0 = MkIntegerTerm(IntOfTerm(d0) | IntOfTerm(d1));
      }
      else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_Y_VV_NOINTTERM);
///#endif
	saveregs();
	d0 = p_or(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
	if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_Y_VV_D0EQUALS0L);
///#endif
	  saveregs();
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	  setregs();
	  FAIL();
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_Y_VV_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, or_y_vv_unk, or_y_vv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_Y_VV_OR_Y_VV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A\\/B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, or_y_vv_nvar_unk, or_y_vv_nvar_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_Y_VV_OR_Y_VV_NVAR_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A\\/B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_or_y_vc, yxn);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_OR_Y_VC_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, or_y_vc_unk);
    or_y_vc_nvar:
      {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_Y_VC_OR_Y_VC_NVAR);
///#endif
	Int d1 = PREG->y_u.yxn.c;
	if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_Y_VC_INTTERM);
///#endif
	  d0 = MkIntegerTerm(IntOfTerm(d0) | d1);
	}
	else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_Y_VC_NOINTTERM);
///#endif
	  saveregs();
	  d0 = p_or(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_Y_VC_D0EQUALS0L);
///#endif
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	    setregs();
	    FAIL();
	  }
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_Y_VC_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, or_y_vc_unk, or_y_vc_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_OR_Y_VC_OR_Y_VC_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A \\/ " Int_FORMAT , PREG->y_u.yxn.c);
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_sll_vv, xxx);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_SLL_VV_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, sll_vv_unk);
    sll_vv_nvar:
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VV_SLL_VV_NVAR);
///#endif
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, sll_vv_nvar_unk);
    sll_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VV_INTTERM_INIT);
///#endif
	Int i2 = IntOfTerm(d1);
	if (i2 < 0) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VV_INTTERM_LESS);
///#endif
	  d0 = MkIntegerTerm(SLR(IntOfTerm(d0), -i2));
	} else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VV_INTTERM_GREATER);
///#endif
	  d0 = do_sll(IntOfTerm(d0),i2 PASS_REGS);
      }
	}
      else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VV_NOINTTERM);
///#endif
	saveregs();
	d0 = p_sll(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
      }
      if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VV_D0EQUALS0L);
///#endif
	saveregs();
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	setregs();
	FAIL();
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VV_NVAR_END);
///#endif
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_vv_unk, sll_vv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VV_SLL_VV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, sll_vv_nvar_unk, sll_vv_nvar_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VV_SLL_VV_NVAR_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_sll_vc, xxn);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_SLL_VC_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, sll_vc_unk);
    sll_vc_nvar:
      {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VC_SLL_VC_NVAR);
///#endif
	Int d1 = PREG->y_u.xxn.c;
	if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VC_INTTERM);
///#endif
	  d0 = do_sll(IntOfTerm(d0), (Int)d1 PASS_REGS);
	}
	else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VC_NOINTTERM);
///#endif
	  saveregs();
	  d0 = p_sll(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	  setregs();
	}
      }
      if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VC_D0EQUALS0L);
///#endif
	saveregs();
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	setregs();
	FAIL();
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VC_NVAR_END);
///#endif
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_vc_unk, sll_vc_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_VC_SLL_VC_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_sll_cv, xxn);
      EMIT_ENTRY_BLOCK(PREG,P_SLL_CV_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      profiled_deref_head_TEST(d0, sll_cv_unk);
    sll_cv_nvar:
      {
	Int d1 = PREG->y_u.xxn.c;
	if (IsIntTerm(d0)) {
      EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0)");
      EMIT_SIMPLE_BLOCK_TEST(P_SLL_CV_SLL_CV_NVAR_INT);
	  Int i2 = IntOfTerm(d0);
	  if (i2 < 0) {
	    d0 = MkIntegerTerm(SLR(d1, -i2));
	  } else {
	    d0 = do_sll(d1,i2 PASS_REGS);
	}
	} else {
      EMIT_CONDITIONAL_FAIL("IsIntTerm(d0)");
      EMIT_SIMPLE_BLOCK_TEST(P_SLL_CV_SLL_CV_NVAR_NOINT);
	  saveregs();
	  d0 = p_sll(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
	  setregs();
	}
      }
      if (d0 == 0L) {
	saveregs();
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	setregs();
	FAIL();
      }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, sll_cv_unk, sll_cv_nvar);
      EMIT_SIMPLE_BLOCK_TEST(P_SLL_CV_SLL_CV_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_sll_y_vv, yxx);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_SLL_Y_VV_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, sll_y_vv_unk);
    sll_y_vv_nvar:
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VV_SLL_Y_VV_NVAR);
///#endif
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, sll_y_vv_nvar_unk);
    sll_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VV_INTTERM_INIT);
///#endif
	Int i2 = IntOfTerm(d1);
	if (i2 < 0) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VV_INTERM_LESS);
///#endif
	  d0 = MkIntegerTerm(SLR(IntOfTerm(d0), -i2));
	} else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VV_INTTERM_GREATER);
///#endif
	  d0 = do_sll(IntOfTerm(d0),i2 PASS_REGS);
      }
      } else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VV_NOINTTERM);
///#endif
	saveregs();
	d0 = p_sll(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
      }
      if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VV_D0EQUALS0L);
///#endif
	saveregs();
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	setregs();
	FAIL();
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VV_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_y_vv_unk, sll_y_vv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VV_SLL_Y_VV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, sll_y_vv_nvar_unk, sll_y_vv_nvar_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VV_SLL_Y_VV_NVAR_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_sll_y_vc, yxn);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_SLL_Y_VC_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, sll_y_vc_unk);
    sll_y_vc_nvar:
      {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VC_SLL_Y_VC_NVAR);
///#endif
	Int d1 = PREG->y_u.yxn.c;
	if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VC_INTTERM);
///#endif
	  d0 = do_sll(IntOfTerm(d0), Yap_Eval(d1)  PASS_REGS);
	}
	else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VC_NOINTTERM);
///#endif
	  saveregs();
	  d0 = p_sll(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	  setregs();
	}
      }
      if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VC_D0EQUALS0L);
///#endif
	saveregs();
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	setregs();
	FAIL();
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VC_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_y_vc_unk, sll_y_vc_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_VC_SLL_Y_VC_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();


      Op(p_sll_y_cv, yxn);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_SLL_Y_CV_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, sll_y_cv_unk);
    sll_y_cv_nvar:
      {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_CV_SLL_Y_CV_NVAR);
///#endif
	Int d1 = PREG->y_u.yxn.c;
	if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_CV_INTTERM_INIT);
///#endif
	  Int i2 = IntOfTerm(d0);
	  if (i2 < 0) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_CV_INTTERM_LESS);
///#endif
	    d0 = MkIntegerTerm(SLR(d1, -i2));
	  } else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_CV_INTTERM_GREATER);
///#endif
	    d0 = do_sll(d1,i2 PASS_REGS);
	}
	} else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_CV_NOINTTERM);
///#endif
	  saveregs();
	  d0 = p_sll(MkIntegerTerm(d1), Yap_Eval(0) PASS_REGS);
	  setregs();
	}
      }
      if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_CV_D0EQUALS0L);
///#endif
	saveregs();
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	setregs();
	FAIL();
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_CV_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_y_cv_unk, sll_y_cv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLL_Y_CV_SLL_Y_CV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_slr_vv, xxx);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_SLR_VV_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, slr_vv_unk);
    slr_vv_nvar:
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_VV_SLR_VV_NVAR);
///#endif
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, slr_vv_nvar_unk);
    slr_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_VV_INTTERM_INIT);
///#endif
	Int i2 = IntOfTerm(d1);
	if (i2 < 0) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_VV_INTTERM_LESS);
///#endif
	  d0 = do_sll(IntOfTerm(d0), -i2 PASS_REGS);
	} else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_VV_INTTERM_GREATER);
///#endif
	  d0 = MkIntTerm(SLR(IntOfTerm(d0), i2));
      }
      } else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_VV_NOINTTERM);
///#endif
	saveregs();
	d0 = p_slr(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
      }
      if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_VV_D0EQUALS0L);
///#endif
	saveregs();
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	setregs();
	FAIL();
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_VV_NVAR_END);
///#endif
      XREG(PREG->y_u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_vv_unk, slr_vv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_VV_SRL_VV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, slr_vv_nvar_unk, slr_vv_nvar_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_VV_SRL_VV_NVAR_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_slr_vc, xxn);
      EMIT_ENTRY_BLOCK(PREG,P_SLR_VC_INSTINIT);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      profiled_deref_head_TEST(d0, slr_vc_unk);
    slr_vc_nvar:
      {
	Int d1 = PREG->y_u.xxn.c;
	if (IsIntTerm(d0)) {
      EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0)");
      EMIT_SIMPLE_BLOCK_TEST(P_SLR_VC_SLR_VC_NVAR_INT);
	  d0 = MkIntTerm(SLR(IntOfTerm(d0), d1));
	}
	else {
      EMIT_CONDITIONAL_FAIL("IsIntTerm(d0)");
      EMIT_SIMPLE_BLOCK_TEST(P_SLR_VC_SLR_VC_NVAR_NOINT);
	  saveregs();
	  d0 = p_slr(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	    setregs();
	    FAIL();
	  }
	}
      }
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, slr_vc_unk, slr_vc_nvar);
      EMIT_SIMPLE_BLOCK_TEST(P_SLR_VC_SRL_VC_UNK);
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_slr_cv, xxn);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_SLR_CV_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      /* first check pt1 */
      deref_head(d0, slr_cv_unk);
    slr_cv_nvar:
      {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_CV_SLR_CV_NVAR);
///#endif
	Int d1 = PREG->y_u.xxn.c;
	if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_CV_INTTERM_INIT);
///#endif
	 Int i2 = IntOfTerm(d0);
	 if (i2 < 0) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_CV_INTTERM_LESS);
///#endif
	   d0 = do_sll(d1, -i2 PASS_REGS);
	 } else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_CV_INTTERM_GREATER);
///#endif
	   d0 = MkIntegerTerm(SLR(d1, i2));
	}
	} else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_CV_NOINTTERM);
///#endif
	  saveregs();
	  d0 = p_slr(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
	  setregs();
	}
      }
      if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_CV_D0EQUALS0L);
///#endif
	saveregs();
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	setregs();
	FAIL();
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_CV_NVAR_END);
///#endif
      XREG(PREG->y_u.xxn.x) = d0;
      PREG = NEXTOP(PREG, xxn);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_cv_unk, slr_cv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_CV_SLR_CV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_slr_y_vv, yxx);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_SLR_Y_VV_INSTINIT);
///#endif
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, slr_y_vv_unk);
    slr_y_vv_nvar:
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VV_SLR_Y_VV_NVAR);
///#endif
      d1 = XREG(PREG->y_u.xxx.x2);
      /* next check A2 */
      deref_head(d1, slr_y_vv_nvar_unk);
    slr_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VV_INTTERM_INIT);
///#endif
	 Int i2 = IntOfTerm(d1);
	 if (i2 < 0) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VV_INTTERM_LESS);
///#endif
	   d0 = do_sll(IntOfTerm(d0), -i2 PASS_REGS);
	 } else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VV_INTTERM_GREATER);
///#endif
	   d0 = MkIntTerm(SLR(IntOfTerm(d0), i2));
      }
      } else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VV_NOINTTERM);
///#endif
	saveregs();
	d0 = p_slr(Yap_Eval(d0), Yap_Eval(d1) PASS_REGS);
	setregs();
      }
      BEGP(pt0);
      if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VV_D0EQUALS0L);
///#endif
	saveregs();
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	setregs();
	FAIL();
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VV_NVAR_END);
///#endif
      pt0 = YREG + PREG->y_u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_y_vv_unk, slr_y_vv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VV_SLR_Y_VV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
      setregs();
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, slr_y_vv_nvar_unk, slr_y_vv_nvar_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VV_SLR_Y_VV_NVAR_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_slr_y_vc, yxn);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_SLR_Y_VC_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, slr_y_vc_unk);
    slr_y_vc_nvar:
      {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VC_SLR_Y_VC_NVAR);
///#endif
	Int d1 = PREG->y_u.yxn.c;
	if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VC_INTTERM);
///#endif
	  d0 = MkIntTerm(SLR(IntOfTerm(d0), d1));
	}
	else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VC_NOINTTERM);
///#endif
	  saveregs();
	  d0 = p_slr(Yap_Eval(d0), MkIntegerTerm(d1) PASS_REGS);
	  setregs();
	  if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VC_D0EQUALS0L);
///#endif
	    saveregs();
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	    setregs();
	    FAIL();
	  }
	}
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VC_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_y_vc_unk, slr_y_vc_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_VC_SLR_Y_VC_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
      setregs();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_slr_y_cv, yxn);
///#ifdef PROFILED_ABSMI
      EMIT_ENTRY_BLOCK(PREG,P_SLR_Y_CV_INSTINIT);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      /* first check pt1 */
      deref_head(d0, slr_y_cv_unk);
    slr_y_cv_nvar:
      {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_CV_SLR_Y_CV_NVAR);
///#endif
	Int d1 = PREG->y_u.yxn.c;
	if (IsIntTerm(d0)) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_CV_INTTERM_INIT);
///#endif
	 Int i2 = IntOfTerm(d0);
	 if (i2 < 0) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_CV_INTTERM_LESS);
///#endif
	   d0 = do_sll(d1, -i2 PASS_REGS);
	 } else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_CV_INTTERM_GREATER);
///#endif
	   d0 = MkIntegerTerm(SLR(d1, i2));
	}
	} else {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_CV_NOINTTERM);
///#endif
	  saveregs();
	  d0 = p_slr(MkIntegerTerm(d1), Yap_Eval(d0) PASS_REGS);
	  setregs();
	}
      }
      if (d0 == 0L) {
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_CV_D0EQUALS0L);
///#endif
	saveregs();
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
	setregs();
	FAIL();
      }
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_CV_NVAR_END);
///#endif
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yxn.y;
      PREG = NEXTOP(PREG, yxn);
      INITIALIZE_PERMVAR(pt0,d0);
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_y_cv_unk, slr_y_cv_nvar);
///#ifdef PROFILED_ABSMI
      EMIT_SIMPLE_BLOCK(P_SLR_Y_CV_SLR_Y_CV_UNK);
///#endif
      saveregs();
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B");
      setregs();
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      BOp(call_bfunc_xx, plxxs);
      EMIT_ENTRY_BLOCK(PREG,CALL_BFUNC_XX_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.plxxs.x1);
    call_bfunc_xx_nvar:
        EMIT_SIMPLE_BLOCK_TEST(CALL_BFUNC_XX_CALL_BFUNC_XX_NVAR);
      d1 = XREG(PREG->y_u.plxxs.x2);
    call_bfunc_xx2_nvar:
      profiled_deref_head_TEST(d0, call_bfunc_xx_unk);
      profiled_deref_head_TEST(d1, call_bfunc_xx2_unk);
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
    EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0) && IsIntTerm(d1)");
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_INT);
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
      }
	  else {
	    yamop *nextp = PREG->y_u.plxxs.f;
	    ALWAYS_LOOKAHEAD(nextp->opc);
	    PREG = nextp;
	    ALWAYS_GONext();
	    ALWAYS_END_PREFETCH();
      }
    }
	else if (v < 0) {
	  if (flags & LT_OK_IN_CMP) {
	    yamop *nextp = NEXTOP(PREG, plxxs);
	    ALWAYS_LOOKAHEAD(nextp->opc);
	    PREG = nextp;
	    ALWAYS_GONext();
	    ALWAYS_END_PREFETCH();
      }
	  else {
	    yamop *nextp = PREG->y_u.plxxs.f;
	    ALWAYS_LOOKAHEAD(nextp->opc);
	    PREG = nextp;
	    ALWAYS_GONext();
	    ALWAYS_END_PREFETCH();
      }
	}
	else /* if (v == 0) */ {
	  if (flags & EQ_OK_IN_CMP) {
	    yamop *nextp = NEXTOP(PREG, plxxs);
	    ALWAYS_LOOKAHEAD(nextp->opc);
	    PREG = nextp;
	    ALWAYS_GONext();
	    ALWAYS_END_PREFETCH();
	  }
	  else {
	    yamop *nextp = PREG->y_u.plxxs.f;
	    ALWAYS_LOOKAHEAD(nextp->opc);
	    PREG = nextp;
	    ALWAYS_GONext();
	    ALWAYS_END_PREFETCH();
	  }
	}
      }
    exec_bin_cmp_xx:
    EMIT_CONDITIONAL_FAIL("IsIntTerm(d0) && IsIntTerm(d1)");
    EMIT_MULTIPLE_DESTINY_BLOCK_TEST(CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_NOINT);
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
      profiled_deref_body(d0, pt0, call_bfunc_xx_unk, call_bfunc_xx_nvar);
        EMIT_SIMPLE_BLOCK_TEST(CALL_BFUNC_XX_CALL_BFUNC_XX_UNK);
      d1 = Deref(d1);
      goto exec_bin_cmp_xx;
      ENDP(pt0);

      BEGP(pt0);
      profiled_deref_body(d1, pt0, call_bfunc_xx2_unk, call_bfunc_xx2_nvar);
      goto exec_bin_cmp_xx;
      ENDP(pt0);

      ENDD(d1);
      ENDD(d0);
      ENDBOp();

      BOp(call_bfunc_yx, plxys);
        EMIT_ENTRY_BLOCK(PREG,CALL_BFUNC_YX_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.plxys.y;
      d1 = XREG(PREG->y_u.plxys.x);
      d0 = *pt0;
      ENDP(pt0);
      profiled_deref_head_TEST(d0, call_bfunc_yx_unk);
    call_bfunc_yx_nvar:
      profiled_deref_head_TEST(d1, call_bfunc_yx2_unk);
    call_bfunc_yx2_nvar:
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0) && IsIntTerm(d1)");
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_INT);
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
        EMIT_CONDITIONAL_FAIL("IsIntTerm(d0) && IsIntTerm(d1)");
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_NOINT);
      {
	CmpPredicate f = PREG->y_u.plxys.p->cs.d_code;
	saveregs();
	d0 = (CELL) (f) (d0,d1);
	setregs();
      }
      if (!d0 || PREG == FAILCODE) {
	if (PREG != FAILCODE) {
	  PREG = PREG->y_u.plxys.f;
	}
	JMPNext();
      }
      PREG = NEXTOP(PREG, plxys);
      JMPNext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, call_bfunc_yx_unk, call_bfunc_yx_nvar);
        EMIT_SIMPLE_BLOCK_TEST(CALL_BFUNC_YX_CALL_BFUNC_YX_UNK);
      d1 = Deref(d1);
      goto exec_bin_cmp_yx;
      ENDP(pt0);

      BEGP(pt0);
      profiled_deref_body(d1, pt0, call_bfunc_yx2_unk, call_bfunc_yx2_nvar);
      goto exec_bin_cmp_yx;
      ENDP(pt0);

      ENDD(d1);
      ENDD(d0);
      ENDBOp();

      BOp(call_bfunc_xy, plxys);
        EMIT_ENTRY_BLOCK(PREG,CALL_BFUNC_XY_INSTINIT);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.plxys.y;
      d0 = XREG(PREG->y_u.plxys.x);
      d1 = *pt0;
      ENDP(pt0);
      profiled_deref_head_TEST(d0, call_bfunc_xy_unk);
    call_bfunc_xy_nvar:
      profiled_deref_head_TEST(d1, call_bfunc_xy2_unk);
    call_bfunc_xy2_nvar:
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0) && IsIntTerm(d1)");
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_INT);
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
        EMIT_CONDITIONAL_FAIL("IsIntTerm(d0) && IsIntTerm(d1)");
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_NOINT);
      {
	CmpPredicate f = PREG->y_u.plxys.p->cs.d_code;
	saveregs();
	d0 = (CELL) (f) (d0,d1);
	setregs();
      }
      if (!d0 || PREG == FAILCODE) {
	if (PREG != FAILCODE) {
	  PREG = PREG->y_u.plxys.f;
	}
	JMPNext();
      }
      PREG = NEXTOP(PREG, plxys);
      JMPNext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, call_bfunc_xy_unk, call_bfunc_xy_nvar);
        EMIT_SIMPLE_BLOCK_TEST(CALL_BFUNC_XY_CALL_BFUNC_XY_UNK);
      d1 = Deref(d1);
      goto exec_bin_cmp_xy;
      ENDP(pt0);

      BEGP(pt0);
      profiled_deref_body(d1, pt0, call_bfunc_xy2_unk, call_bfunc_xy2_nvar);
      goto exec_bin_cmp_xy;
      ENDP(pt0);

      ENDD(d1);
      ENDD(d0);
      ENDBOp();

      BOp(call_bfunc_yy, plyys);
        EMIT_ENTRY_BLOCK(PREG,CALL_BFUNC_YY_INSTINIT);
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
      profiled_deref_head_TEST(d0, call_bfunc_yy_unk);
    call_bfunc_yy_nvar:
      profiled_deref_head_TEST(d1, call_bfunc_yy2_unk);
    call_bfunc_yy2_nvar:
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
        EMIT_CONDITIONAL_SUCCESS("IsIntTerm(d0) && IsIntTerm(d1)");
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_INT);
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
        EMIT_CONDITIONAL_FAIL("IsIntTerm(d0) && IsIntTerm(d1)");
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_NOINT);
      {
	CmpPredicate f = PREG->y_u.plyys.p->cs.d_code;
	saveregs();
	d0 = (CELL) (f) (d0,d1);
	setregs();
      }
      if (!d0 || PREG == FAILCODE) {
	if (PREG != FAILCODE) {
	  PREG = PREG->y_u.plyys.f;
	}
	JMPNext();
      }
      PREG = NEXTOP(PREG, plyys);
      JMPNext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, call_bfunc_yy_unk, call_bfunc_yy_nvar);
        EMIT_SIMPLE_BLOCK_TEST(CALL_BFUNC_YY_CALL_BFUNC_YY_UNK);
      d1 = Deref(d1);
      goto exec_bin_cmp_yy;
      ENDP(pt0);

      BEGP(pt0);
      profiled_deref_body(d1, pt0, call_bfunc_yy2_unk, call_bfunc_yy2_nvar);
      goto exec_bin_cmp_yy;
      ENDP(pt0);

      ENDD(d1);
      ENDD(d0);
      ENDBOp();

      Op(p_equal, e);
        EMIT_ENTRY_BLOCK(PREG,P_EQUAL_INSTINIT);
      save_hb();
      if (Yap_IUnify(ARG1, ARG2) == FALSE) {
	FAIL();
      }
        EMIT_SIMPLE_BLOCK(P_EQUAL_END);
      PREG = NEXTOP(PREG, e);
      GONext();
      ENDOp();

      Op(p_dif, l);
        EMIT_ENTRY_BLOCK(PREG,P_DIF_INSTINIT);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
        EMIT_SIMPLE_BLOCK_TEST(P_DIF_LOW_LEVEL_TRACER);
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1);
	}
#endif	/* LOW_LEVEL_TRACE */
        EMIT_SIMPLE_BLOCK_TEST(P_DIF_POST_LOW_LEVEL_TRACER);
      BEGD(d0);
      BEGD(d1);
      d0 = ARG1;
      profiled_deref_head_TEST(d0, dif_unk1);
    dif_nvar1:
      /* first argument is bound */
        EMIT_SIMPLE_BLOCK_TEST(P_DIF_DIF_NVAR1);
      d1 = ARG2;
      profiled_deref_head_TEST(d1, dif_nvar1_unk2);
    dif_nvar1_nvar2:
      /* both arguments are bound */
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_DIF_DIF_NVAR1_NVAR2);
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
	/* make B and HRB point to HR to guarantee all bindings will
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
        {
	      /* normal variable */
	      RESET_VARIABLE(d1);
		}
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
	  /* restore B, no need to restore HRB */
	  PREG = PREG->y_u.l.l;
	  GONext();
	}
	/* restore B, and later HRB */
	PREG = NEXTOP(PREG, l);
	ENDCHO(pt1);
      }
      GONext();

      BEGP(pt0);
      profiled_deref_body(d0, pt0, dif_unk1, dif_nvar1);
        EMIT_SIMPLE_BLOCK_TEST(P_DIF_DIF_UNK1);
      ENDP(pt0);
      /* first argument is unbound */
      PREG = PREG->y_u.l.l;
      GONext();

      BEGP(pt0);
      profiled_deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2);
        EMIT_SIMPLE_BLOCK_TEST(P_DIF_DIF_NVAR1_UNK2);
      ENDP(pt0);
      /* second argument is unbound */
      PREG = PREG->y_u.l.l;
      GONext();
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_eq, l);
        EMIT_ENTRY_BLOCK(PREG,P_EQ_INSTINIT);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
        EMIT_SIMPLE_BLOCK_TEST(P_EQ_LOW_LEVEL_TRACER);
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorSame,0)),XREGS+1);
    }
#endif	/* LOW_LEVEL_TRACE */
        EMIT_SIMPLE_BLOCK_TEST(P_EQ_POST_LOW_LEVEL_TRACER);
      BEGD(d0);
      BEGD(d1);
      d0 = ARG1;
      profiled_deref_head_TEST(d0, p_eq_unk1);
    p_eq_nvar1:
      /* first argument is bound */
        EMIT_SIMPLE_BLOCK_TEST(P_EQ_P_EQ_NVAR1);
      d1 = ARG2;
      profiled_deref_head_TEST(d1, p_eq_nvar1_unk2);
    p_eq_nvar1_nvar2:
      /* both arguments are bound */
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_EQ_P_EQ_NVAR1_NVAR2);
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
	if (d2 == FALSE) {
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
	if (d2 == FALSE) {
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
      profiled_deref_body(d1, pt0, p_eq_nvar1_unk2, p_eq_nvar1_nvar2);
        EMIT_SIMPLE_BLOCK_TEST(P_EQ_P_EQ_NVAR1_UNK2);
      ENDP(pt0);
      /* first argument is bound */
      /* second argument is unbound */
      /* I don't need to worry about co-routining because an
	 unbound variable may never be == to a constrained variable!! */
      PREG = PREG->y_u.l.l;
      GONext();
      ENDD(d1);

      BEGP(pt0);
      profiled_deref_body(d0, pt0, p_eq_unk1, p_eq_nvar1);
        EMIT_SIMPLE_BLOCK_TEST(P_EQ_P_EQ_UNK1);
      BEGD(d1);
      d1 = ARG2;
      profiled_deref_head_TEST(d1, p_eq_var1_unk2);
    p_eq_var1_nvar2:
      /* I don't need to worry about co-routining because an
	 unbound variable may never be == to a constrained variable!! */
        EMIT_SIMPLE_BLOCK_TEST(P_EQ_P_EQ_VAR1_NVAR2);
      PREG = PREG->y_u.l.l;
      GONext();

      BEGP(pt1);
      profiled_deref_body(d1, pt1, p_eq_var1_unk2, p_eq_var1_nvar2);
      /* first argument is unbound */
      /* second argument is unbound */
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_EQ_P_EQ_VAR1_UNK2_END);
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
        EMIT_ENTRY_BLOCK(PREG,P_ARG_VV_INSTINIT);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
        EMIT_SIMPLE_BLOCK(P_ARG_VV_LOW_LEVEL_TRACER);
	HR[0] = XREG(PREG->y_u.xxx.x1);
	HR[1] = XREG(PREG->y_u.xxx.x2);
	RESET_VARIABLE(HR+2);
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),HR);
      }
#endif	/* LOW_LEVEL_TRACE */
        EMIT_SIMPLE_BLOCK_TEST(P_ARG_VV_TEST_D0);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxx.x1);
      profiled_deref_head_TEST(d0, arg_arg1_unk);
    arg_arg1_nvar:
      /* ARG1 is ok! */
        EMIT_SIMPLE_BLOCK_TEST(P_ARG_VV_ARG_ARG1_NVAR);
      if (IsIntTerm(d0)) {
	d0 = IntOfTerm(d0);
      } else if (IsLongIntTerm(d0)) {
	d0 = LongIntOfTerm(d0);
      } else {
	saveregs();
	Yap_Error(TYPE_ERROR_INTEGER,d0,"arg 1 of arg/3");
	setregs();
	FAIL();
      }

      /* d0 now got the argument we want */
        EMIT_SIMPLE_BLOCK_TEST(P_ARG_VV_TEST_D1);
      BEGD(d1);
      d1 = XREG(PREG->y_u.xxx.x2);
      profiled_deref_head_TEST(d1, arg_arg2_unk);
    arg_arg2_nvar:
      /* d1 now got the structure we want to fetch the argument
       * from */
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_ARG_VV_ARG_ARG2_NVAR);
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
      profiled_deref_body(d1, pt0, arg_arg2_unk, arg_arg2_nvar);
        EMIT_SIMPLE_BLOCK_TEST(P_ARG_VV_ARG_ARG2_UNK);
      saveregs();
      Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3");;
      setregs();
      ENDP(pt0);
      FAIL();
      ENDD(d1);

      BEGP(pt0);
      profiled_deref_body(d0, pt0, arg_arg1_unk, arg_arg1_nvar);
        EMIT_SIMPLE_BLOCK_TEST(P_ARG_VV_ARG_ARG1_UNK);
      saveregs();
      Yap_Error(INSTANTIATION_ERROR, d0, "arg 1 of arg/3");;
      setregs();
      ENDP(pt0);
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_arg_cv, xxn);
        EMIT_ENTRY_BLOCK(PREG,P_ARG_CV_INSTINIT);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
        EMIT_SIMPLE_BLOCK_TEST(P_ARG_CV_LOW_LEVEL_TRACER);
	CELL *Ho = HR;
	Term t = MkIntegerTerm(PREG->y_u.xxn.c);
	HR[0] =  t;
	HR[1] = XREG(PREG->y_u.xxn.xi);
	RESET_VARIABLE(HR+2);
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),HR);
	HR = Ho;
      }
#endif	/* LOW_LEVEL_TRACE */
        EMIT_SIMPLE_BLOCK_TEST(P_ARG_CV_TEST_D1);
      BEGD(d0);
      d0 = PREG->y_u.xxn.c;
      /* d0 now got the argument we want */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xxn.xi);
      profiled_deref_head_TEST(d1, arg_arg2_vc_unk);
    arg_arg2_vc_nvar:
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_ARG_CV_ARG_ARG2_VC_NVAR);
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
      profiled_deref_body(d1, pt0, arg_arg2_vc_unk, arg_arg2_vc_nvar);
        EMIT_SIMPLE_BLOCK_TEST(P_ARG_CV_ARG_ARG2_VC_UNK);
      saveregs();
      Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3");;
      setregs();
      ENDP(pt0);
      FAIL();
      ENDD(d1);

      ENDD(d0);
      ENDOp();

      Op(p_arg_y_vv, yxx);
        EMIT_ENTRY_BLOCK(PREG,P_ARG_Y_VV_INSTINIT);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
        EMIT_SIMPLE_BLOCK_TEST(P_ARG_Y_VV_LOW_LEVEL_TRACER);
	HR[0] = XREG(PREG->y_u.yxx.x1);
	HR[1] = XREG(PREG->y_u.yxx.x2);
	HR[2] = YREG[PREG->y_u.yxx.y];
	RESET_VARIABLE(HR+2);
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),HR);
      }
#endif	/* LOW_LEVEL_TRACE */
        EMIT_SIMPLE_BLOCK_TEST(P_ARG_Y_VV_TEST_D0);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxx.x1);
      profiled_deref_head_TEST(d0, arg_y_arg1_unk);
    arg_y_arg1_nvar:
        EMIT_SIMPLE_BLOCK_TEST(P_ARG_Y_VV_ARG_Y_ARG1_NVAR);
      /* ARG1 is ok! */
      if (IsIntTerm(d0)) {
	d0 = IntOfTerm(d0);
      } else if (IsLongIntTerm(d0)) {
	d0 = LongIntOfTerm(d0);
      } else {
	saveregs();
	Yap_Error(TYPE_ERROR_INTEGER,d0,"arg 1 of arg/3");
	setregs();
	FAIL();
      }

      /* d0 now got the argument we want */
        EMIT_SIMPLE_BLOCK_TEST(P_ARG_Y_VV_TEST_D1);
      BEGD(d1);
      d1 = XREG(PREG->y_u.yxx.x2);
      profiled_deref_head_TEST(d1, arg_y_arg2_unk);
    arg_y_arg2_nvar:
      /* d1 now got the structure we want to fetch the argument
       * from */
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_ARG_Y_VV_ARG_Y_ARG2_NVAR);
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
      profiled_deref_body(d1, pt0, arg_y_arg2_unk, arg_y_arg2_nvar);
        EMIT_SIMPLE_BLOCK_TEST(P_ARG_Y_VV_ARG_Y_ARG2_UNK);
      saveregs();
      Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3");;
      setregs();
      ENDP(pt0);
      FAIL();
      ENDD(d1);

      BEGP(pt0);
      profiled_deref_body(d0, pt0, arg_y_arg1_unk, arg_y_arg1_nvar);
        EMIT_SIMPLE_BLOCK_TEST(P_ARG_Y_VV_ARG_Y_ARG1_UNK);
      saveregs();
      Yap_Error(INSTANTIATION_ERROR, d0, "arg 1 of arg/3");
      setregs();
      ENDP(pt0);
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_arg_y_cv, yxn);
///#ifdef PROFILED_ABSMI
        EMIT_ENTRY_BLOCK(PREG,P_ARG_Y_CV_INSTINIT);
///#endif
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_ARG_Y_CV_LOW_LEVEL_TRACER);
///#endif
	CELL *Ho = HR;
	Term t = MkIntegerTerm(PREG->y_u.yxn.c);
	HR[0] =  t;
	HR[1] = XREG(PREG->y_u.yxn.xi);
	HR[2] = YREG[PREG->y_u.yxn.y];
	RESET_VARIABLE(HR+2);
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),HR);
	HR = Ho;
      }
#endif	/* LOW_LEVEL_TRACE */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_ARG_Y_CV_TEST_D1);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_ARG_Y_CV_D1APPL_INIT);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_ARG_Y_CV_D1APPL_END);
///#endif
	BEGP(pt1);
	pt1 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(PREG, yxn);
	INITIALIZE_PERMVAR(pt1,pt0[d0]);
	ENDP(pt1);
	GONext();
	ENDP(pt0);
      }
      else if (IsPairTerm(d1)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_ARG_Y_CV_D1PAIR_INIT);
///#endif
	BEGP(pt0);
	pt0 = RepPair(d1);
	if (d0 != 1 && d0 != 2) {
	  if ((Int)d0 < 0) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_ARG_Y_CV_D1PAIR_LESS0);
///#endif
	    saveregs();
	    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
		  MkIntegerTerm(d0),"arg 1 of arg/3");
	    setregs();
	  }
	  FAIL();
	}
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_ARG_Y_CV_D1PAIR_END);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_ARG_Y_CV_ARG_Y_ARG2_VC_UNK);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_ENTRY_BLOCK(PREG,P_FUNC2S_VV_INSTINIT);
///#endif
    restart_func2s:
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_LOW_LEVEL_TRACER);
///#endif
	RESET_VARIABLE(HR);
	HR[1] = XREG(PREG->y_u.xxx.x1);
	HR[2] = XREG(PREG->y_u.xxx.x2);
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
      }
#endif	/* LOW_LEVEL_TRACE */
      /* We have to build the structure */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_TEST_D0);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxx.x1);
      deref_head(d0, func2s_unk);
    func2s_nvar:
      /* we do, let's get the third argument */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_TEST_D1);
///#endif
      BEGD(d1);
      d1 = XREG(PREG->y_u.xxx.x2);
      deref_head(d1, func2s_unk2);
    func2s_nvar2:
      /* Uuuff, the second and third argument are bound */
      if (IsIntegerTerm(d1)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_D1INT);
///#endif
	d1 = IntegerOfTerm(d1);
      } else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_D1NOTINT);
///#endif
	saveregs();
	if (IsBigIntTerm(d1)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_D1BIGINT);
///#endif
	  Yap_Error(RESOURCE_ERROR_STACK, d1, "functor/3");
	} else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_D1NOTBIGINT);
///#endif
	  Yap_Error(TYPE_ERROR_INTEGER, d1, "functor/3");
	}
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_D1NOTINT_END);
///#endif
	setregs();
	FAIL();
      }
      if (!IsAtomicTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_D0NOTATOMIC);
///#endif
	saveregs();
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	setregs();
	FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_FIRSTIFOK);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_SECONDIFOK_D0NOTATOM);
///#endif
	  saveregs();
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  setregs();
	  FAIL();
	}
	BEGP(pt1);
	if (!IsAtomTerm(d0)) {
	  FAIL();
	}
	else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_SECONDIFOK_D0ATOM);
///#endif
	  d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);
	}
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_SECONDIFOK_POST_D0ATOM);
///#endif
	pt1 = HR;
	*pt1++ = d0;
	d0 = AbsAppl(HR);
	if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	  /* make sure we have something to show for our trouble */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_INIT);
///#endif
	  saveregs();
	  if (!Yap_gcl((1+d1)*sizeof(CELL), 0, YREG, NEXTOP(NEXTOP(PREG,xxx),Osbpp))) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_IFOK);
///#endif
	    Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage);
	    setregs();
	    JMPNext();
	  } else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_NOIF);
///#endif
	    setregs();
	  }
	  goto restart_func2s;
	}
	while ((Int)d1--) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_SECONDIFOK_INSIDEWHILE);
///#endif
	  RESET_VARIABLE(pt1);
	  pt1++;
	}
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_SECONDIFOK_END);
///#endif
	HR = pt1;
	/* done building the term */
	ENDP(pt1);
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	XREG(PREG->y_u.xxx.x) = d0;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxx),Osbpp),l);
	GONext();
      }	else if ((Int)d1  == 0) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_THIRDIFOK);
///#endif
	XREG(PREG->y_u.xxx.x) = d0;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxx),Osbpp),l);
	GONext();
      }	else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_ELSE);
///#endif
	saveregs();
	Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	setregs();
	FAIL();
      }

      BEGP(pt1);
      deref_body(d1, pt1, func2s_unk2, func2s_nvar2);
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_FUNC2S_UNK2);
///#endif
      saveregs();
      Yap_Error(INSTANTIATION_ERROR, d1, "functor/3");
      setregs();
      ENDP(pt1);
      /* Oops, third argument was unbound */
      FAIL();
      ENDD(d1);

      BEGP(pt1);
      deref_body(d0, pt1, func2s_unk, func2s_nvar);
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VV_FUNC2S_UNK);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_ENTRY_BLOCK(PREG,P_FUNC2S_CV_INSTINIT);
///#endif
    restart_func2s_cv:
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_LOW_LEVEL_TRACER);
///#endif
	RESET_VARIABLE(HR);
	HR[1] = PREG->y_u.xxc.c;
	HR[2] = XREG(PREG->y_u.xxc.xi);
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
      }
#endif	/* LOW_LEVEL_TRACE */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_TEST_D1);
///#endif
      BEGD(d0);
      /* We have to build the structure */
      d0 = PREG->y_u.xxc.c;
      /* we do, let's get the third argument */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xxc.xi);
      deref_head(d1, func2s_unk2_cv);
    func2s_nvar2_cv:
      /* Uuuff, the second and third argument are bound */
      if (IsIntegerTerm(d1)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_D1INT);
///#endif
	d1 = IntegerOfTerm(d1);
      } else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_D1NOTINT);
///#endif
	saveregs();
	if (IsBigIntTerm(d1)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_D1NOINT_D1BIGINT);
///#endif
	  Yap_Error(RESOURCE_ERROR_STACK, d1, "functor/3");
	} else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_D1NOTBIGINT);
///#endif
	  Yap_Error(TYPE_ERROR_INTEGER,d1,"functor/3");
	}
///#ifdef PROFILED_ABSMI
    EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_POST_IF);
///#endif
	setregs();
	FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_FIRSTIFOK);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_D1GREATER_D0NOTATOM);
///#endif
	  saveregs();
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  setregs();
	  FAIL();
	}
	BEGP(pt1);
	if (!IsAtomTerm(d0)) {
	  FAIL();
	}
	else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_D1GREATER_D0ATOM);
///#endif
	  d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);
	}
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_D1GREATER_POST_IF);
///#endif
	pt1 = HR;
	*pt1++ = d0;
	d0 = AbsAppl(HR);
	if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	  /* make sure we have something to show for our trouble */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_D1GREATER_IFOK_INIT);
///#endif
	  saveregs();
	  if (!Yap_gcl((1+d1)*sizeof(CELL), 0, YREG, NEXTOP(NEXTOP(PREG,xxc),Osbpp))) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_D1GREATER_IFOK_IFOK);
///#endif
	    Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage);
	    setregs();
	    JMPNext();
	  } else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_D1GREATER_IFOK_NOIF);
///#endif
	    setregs();
	  }
	  goto restart_func2s_cv;
	}
	while ((Int)d1--) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_D1GREATER_INSIDEWHILE);
///#endif
	  RESET_VARIABLE(pt1);
	  pt1++;
	}
	/* done building the term */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_D1GREATER_END);
///#endif
	HR = pt1;
	ENDP(pt1);
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	XREG(PREG->y_u.xxc.x) = d0;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxc),Osbpp),l);
	GONext();
      }	else if (d1  == 0) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_D1ISZERO);
///#endif
	XREG(PREG->y_u.xxc.x) = d0;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxc),Osbpp),l);
	GONext();
      }	else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_ELSE);
///#endif
	saveregs();
	Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	setregs();
	FAIL();
      }

      BEGP(pt1);
      deref_body(d1, pt1, func2s_unk2_cv, func2s_nvar2_cv);
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_CV_END);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_ENTRY_BLOCK(PREG,P_FUNC2S_VC_INSTINIT);
///#endif
    restart_func2s_vc:
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_LOW_LEVEL_TRACER);
///#endif
	Term ti;
	CELL *hi = HR;

	ti = MkIntegerTerm(PREG->y_u.xxn.c);
	RESET_VARIABLE(HR);
	HR[1] = XREG(PREG->y_u.xxn.xi);
	HR[2] = ti;
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
	HR = hi;
      }
#endif	/* LOW_LEVEL_TRACE */
      /* We have to build the structure */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_TEST_D0);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxn.xi);
      deref_head(d0, func2s_unk_vc);
    func2s_nvar_vc:
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_FUNC2S_NVAR_VC);
///#endif
      BEGD(d1);
      d1 = PREG->y_u.xxn.c;
      if (!IsAtomicTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_D0NOATOMIC);
///#endif
	saveregs();
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	setregs();
	FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_EQUALS);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_D1ISZERO);
///#endif
	XREG(PREG->y_u.xxn.x) = d0;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxn),Osbpp),l);
	GONext();
      }
      if (!IsAtomTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_D0NOATOM);
///#endif
	saveregs();
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	setregs();
	FAIL();
      }
      BEGP(pt1);
      if (!IsAtomTerm(d0)) {
	FAIL();
      }
      else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_D0ATOM);
///#endif
	d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);
	  }
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_POST_ELSE);
///#endif
	pt1 = HR;
      *pt1++ = d0;
      d0 = AbsAppl(HR);
      if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	/* make sure we have something to show for our trouble */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_IFOK_INIT);
///#endif
	saveregs();
	if (!Yap_gc(0, YREG, NEXTOP(NEXTOP(PREG,xxn),Osbpp))) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_IFOK_IFOK);
///#endif
	  Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage);
	  setregs();
	  JMPNext();
	} else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_IFOK_NOIF);
///#endif
	  setregs();
	}
	goto restart_func2s_vc;
      }
      while ((Int)d1--) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_INSIDEWHILE);
///#endif
	RESET_VARIABLE(pt1);
	pt1++;
      }
      /* done building the term */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_END1);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_VC_END2);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_ENTRY_BLOCK(PREG,P_FUNC2S_Y_VV_INSTINIT);
///#endif
    restart_func2s_y:
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_LOW_LEVEL_TRACER);
///#endif
	RESET_VARIABLE(HR);
	HR[1] = XREG(PREG->y_u.yxx.x1);
	HR[2] = XREG(PREG->y_u.yxx.x2);
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)), HR );
      }
#endif	/* LOW_LEVEL_TRACE */
      /* We have to build the structure */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_TEST_D0);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxx.x1);
      deref_head(d0, func2s_y_unk);
    func2s_y_nvar:
      /* we do, let's get the third argument */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_TEST_D1);
///#endif
      BEGD(d1);
      d1 = XREG(PREG->y_u.yxx.x2);
      deref_head(d1, func2s_y_unk2);
    func2s_y_nvar2:
      /* Uuuff, the second and third argument are bound */
      if (IsIntegerTerm(d1)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_D1INT);
///#endif
	d1 = IntegerOfTerm(d1);
      } else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_D1NOTINT);
///#endif
	saveregs();
	if (IsBigIntTerm(d1)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_D1BIGINT);
///#endif
	  Yap_Error(RESOURCE_ERROR_STACK, d1, "functor/3");
	} else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_D1NOTBIGINT);
///#endif
	  Yap_Error(TYPE_ERROR_INTEGER,d1,"functor/3");
	}
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_POST_IF);
///#endif
	setregs();
	FAIL();
      }
      if (!IsAtomicTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_D0NOATOMIC);
///#endif
	saveregs();
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	setregs();
	FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_EQUALS);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_D1GREATER_D0NOATOM);
///#endif
	  saveregs();
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  setregs();
	  FAIL();
	}
	BEGP(pt1);
	if (!IsAtomTerm(d0)) {
	  FAIL();
	}
	else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_D1GREATER_D0ATOM);
///#endif
	  d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);
	}
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_D1GREATER_POST_ELSE);
///#endif
	pt1 = HR;
	*pt1++ = d0;
	d0 = AbsAppl(HR);
	if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	  /* make sure we have something to show for our trouble */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_D1GREATER_IFOK_INIT);
///#endif
	  saveregs();
	  if (!Yap_gcl((1+d1)*sizeof(CELL), 0, YREG, NEXTOP(NEXTOP(PREG,yxx),Osbpp))) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_D1GREATER_IFOK_IFOK);
///#endif
	    Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage);
	    setregs();
	    JMPNext();
	  } else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_D1GREATER_IFOK_NOIF);
///#endif
	    setregs();
	  }
	  goto restart_func2s_y;
	}
	while ((Int)d1--) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_D1GREATER_INSIDEWHILE);
///#endif
	  RESET_VARIABLE(pt1);
	  pt1++;
	}
	/* done building the term */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_D1GREATER_END);
///#endif
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
      }	else if (d1  == 0) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_D1ISZERO);
///#endif
	BEGP(pt1);
	pt1 = YREG + PREG->y_u.yxx.y;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxx),Osbpp),l);
	INITIALIZE_PERMVAR(pt1,d0);
	ENDP(pt1);
	GONext();
      }	else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_ELSE);
///#endif
	saveregs();
	Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	setregs();
	FAIL();
      }

      BEGP(pt1);
      deref_body(d1, pt1, func2s_y_unk2, func2s_y_nvar2);
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_END1);
///#endif
      saveregs();
      Yap_Error(INSTANTIATION_ERROR, d1, "functor/3");
      setregs();
      ENDP(pt1);
      /* Oops, third argument was unbound */
      FAIL();
      ENDD(d1);

      BEGP(pt1);
      deref_body(d0, pt1, func2s_y_unk, func2s_y_nvar);
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VV_END2);
///#endif
      saveregs();
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
      setregs();
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2s_y_cv, yxn);
      /* A1 is a variable */
///#ifdef PROFILED_ABSMI
        EMIT_ENTRY_BLOCK(PREG,P_FUNC2S_Y_CV_INSTINIT);
///#endif
    restart_func2s_y_cv:
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_LOW_LEVEL_TRACER);
///#endif
	RESET_VARIABLE(HR);
	HR[1] = PREG->y_u.yxn.c;
	HR[2] = XREG(PREG->y_u.yxn.xi);
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
      }
#endif	/* LOW_LEVEL_TRACE */
      /* We have to build the structure */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_TEST_D1);
///#endif
      BEGD(d0);
      d0 = PREG->y_u.yxn.c;
      /* we do, let's get the third argument */
      BEGD(d1);
      d1 = XREG(PREG->y_u.yxn.xi);
      deref_head(d1, func2s_y_unk_cv);
    func2s_y_nvar_cv:
      /* Uuuff, the second and third argument are bound */
      if (IsIntegerTerm(d1)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_D1INT);
///#endif
	d1 = IntegerOfTerm(d1);
      } else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_D1NOTINT);
///#endif
	saveregs();
	if (IsBigIntTerm(d1)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_D1BIGINT);
///#endif
	  Yap_Error(RESOURCE_ERROR_STACK, d1, "functor/3");
	} else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_D1NOTBIGINT);
///#endif
	  Yap_Error(TYPE_ERROR_INTEGER,d1,"functor/3");
	}
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_POST_IF);
///#endif
	setregs();
	FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_EQUALS);
///#endif
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
      else if ((Int)d1 > 0) {
	/* now let's build a compound term */
	if (!IsAtomTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_D1GREATER_D0NOATOM);
///#endif
	  saveregs();
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  setregs();
	  FAIL();
	}
	if (!IsAtomTerm(d0)) {
	  FAIL();
	}
	else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_D1GREATER_D0ATOM);
///#endif
	  d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);
	}
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_D1GREATER_POST_ELSE);
///#endif
	BEGP(pt1);
	pt1 = HR;
	*pt1++ = d0;
	d0 = AbsAppl(HR);
	if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	  /* make sure we have something to show for our trouble */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_D1GREATER_IFOK_INIT);
///#endif
	  saveregs();
	  if (!Yap_gcl((1+d1)*sizeof(CELL), 0, YREG, NEXTOP(NEXTOP(PREG,yxn),Osbpp))) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_D1GREATER_IFOK_IFOK);
///#endif
	    Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage);
	    setregs();
	    JMPNext();
	  } else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_D1GREATER_IFOK_NOIF);
///#endif
	    setregs();
	  }
	  goto restart_func2s_y_cv;
	}
	while ((Int)d1--) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_D1GREATER_INSIDEWHILE);
///#endif
	  RESET_VARIABLE(pt1);
	  pt1++;
	}
	/* done building the term */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_D1GREATER_END);
///#endif
	HR = pt1;
	ENDP(pt1);
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	BEGP(pt1);
	pt1 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxn),Osbpp),l);
	INITIALIZE_PERMVAR(pt1,d0);
	ENDP(pt1);
	GONext();
      }	else if (d1  == 0) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_D1ISZERO);
///#endif
	BEGP(pt1);
	pt1 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxn),Osbpp),l);
	INITIALIZE_PERMVAR(pt1,d0);
	ENDP(pt1);
	GONext();
      }	else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_ELSE);
///#endif
	saveregs();
	Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	setregs();
	FAIL();
      }

      BEGP(pt1);
      deref_body(d1, pt1, func2s_y_unk_cv, func2s_y_nvar_cv);
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_CV_END);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_ENTRY_BLOCK(PREG,P_FUNC2S_Y_VC_INSTINIT);
///#endif
    restart_func2s_y_vc:
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_LOW_LEVEL_TRACER);
///#endif
	Term ti;
	CELL *hi = HR;

	ti = MkIntegerTerm((Int)(PREG->y_u.yxn.c));
	RESET_VARIABLE(HR);
	HR[1] = XREG(PREG->y_u.yxn.xi);
	HR[2] = ti;
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
	HR = hi;
      }
#endif	/* LOW_LEVEL_TRACE */
      /* We have to build the structure */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_TEST_D0);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxn.xi);
      deref_head(d0, func2s_y_unk_vc);
    func2s_y_nvar_vc:
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_FUNC2S_Y_NVAR_VC);
///#endif
      BEGD(d1);
      d1 = PREG->y_u.yxn.c;
      if (!IsAtomicTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_D0NOATOMIC);
///#endif
	saveregs();
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	setregs();
	FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_EQUALS);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_D1ISZERO);
///#endif
	BEGP(pt1);
	pt1 = YREG + PREG->y_u.yxn.y;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxn),Osbpp),l);
	INITIALIZE_PERMVAR(pt1,d0);
	ENDP(pt1);
	GONext();
      }
      if (!IsAtomTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_D0NOATOM1);
///#endif
	saveregs();
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	setregs();
	FAIL();
      }
      /* now let's build a compound term */
      if (!IsAtomTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_D0NOATOM2);
///#endif
	saveregs();
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3");
	setregs();
	FAIL();
      }
      BEGP(pt1);
      if (!IsAtomTerm(d0)) {
	FAIL();
      }
      else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_D0ATOM);
///#endif
	d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);
	  }
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_POST_ELSE);
///#endif
      pt1 = HR;
      *pt1++ = d0;
      d0 = AbsAppl(HR);
      if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	/* make sure we have something to show for our trouble */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_IFOK_INIT);
///#endif
	saveregs();
	if (!Yap_gcl((1+d1)*sizeof(CELL), 0, YREG, NEXTOP(NEXTOP(PREG,yxn),Osbpp))) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_IFOK_IFOK);
///#endif
	  Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage);
	  setregs();
	  JMPNext();
	} else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_IFOK_NOIF);
///#endif
	  setregs();
	}
	goto restart_func2s_y_vc;
      }
      while ((Int)d1--) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_INSIDEWHILE);
///#endif
	RESET_VARIABLE(pt1);
	pt1++;
      }
      /* done building the term */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_END1);
///#endif
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
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2S_Y_VC_END2);
///#endif
      saveregs();
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
      setregs();
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2f_xx, xxx);
///#ifdef PROFILED_ABSMI
        EMIT_ENTRY_BLOCK(PREG,P_FUNC2F_XX_INSTINIT);
///#endif
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XX_LOW_LEVEL_TRACER);
///#endif
	HR[0] = XREG(PREG->y_u.xxx.x);
	RESET_VARIABLE(HR+1);
	RESET_VARIABLE(HR+2);
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
      }
#endif	/* LOW_LEVEL_TRACE */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XX_TEST_D0);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxx.x);
      deref_head(d0, func2f_xx_unk);
    func2f_xx_nvar:
      if (IsApplTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XX_D0APPL);
///#endif
	Functor d1 = FunctorOfTerm(d0);
	if (IsExtensionFunctor(d1)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XX_D0APPL_D1EXTFUNC);
///#endif
	  XREG(PREG->y_u.xxx.x1) = d0;
	  XREG(PREG->y_u.xxx.x2) = MkIntTerm(0);
	  PREG = NEXTOP(PREG, xxx);
	  GONext();
	}
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XX_D0APPL_END);
///#endif
	XREG(PREG->y_u.xxx.x1) = MkAtomTerm(NameOfFunctor(d1));
	XREG(PREG->y_u.xxx.x2) = MkIntegerTerm(ArityOfFunctor(d1));
	PREG = NEXTOP(PREG, xxx);
	GONext();
      } else if (IsPairTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XX_D0PAIR);
///#endif
	XREG(PREG->y_u.xxx.x1) = TermDot;
	XREG(PREG->y_u.xxx.x2) = MkIntTerm(2);
	PREG = NEXTOP(PREG, xxx);
	GONext();
      } else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XX_D0NOCOMPOUND);
///#endif
	XREG(PREG->y_u.xxx.x1) = d0;
	XREG(PREG->y_u.xxx.x2) = MkIntTerm(0);
	PREG = NEXTOP(PREG, xxx);
	GONext();
      }

      BEGP(pt1);
      deref_body(d0, pt1, func2f_xx_unk, func2f_xx_nvar);
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XX_END);
///#endif
      saveregs();
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
      setregs();
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2f_xy, xxy);
///#ifdef PROFILED_ABSMI
        EMIT_ENTRY_BLOCK(PREG,P_FUNC2F_XY_INSTINIT);
///#endif
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XY_LOW_LEVEL_TRACER);
///#endif
	HR[0] = XREG(PREG->y_u.xxy.x);
	RESET_VARIABLE(HR+1);
	RESET_VARIABLE(HR+2);
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
      }
#endif	/* LOW_LEVEL_TRACE */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XY_TEST_D0);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.xxy.x);
      deref_head(d0, func2f_xy_unk);
    func2f_xy_nvar:
      if (IsApplTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XY_D0APPL);
///#endif
	Functor d1 = FunctorOfTerm(d0);
	CELL *pt0 = YREG+PREG->y_u.xxy.y2;
	if (IsExtensionFunctor(d1)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XY_D0APPL_D1EXTFUNC);
///#endif
	  XREG(PREG->y_u.xxy.x1) = d0;
	  PREG = NEXTOP(PREG, xxy);
	  INITIALIZE_PERMVAR(pt0, MkIntTerm(0));
	  GONext();
	}
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XY_D0APPL_END);
///#endif
	XREG(PREG->y_u.xxy.x1) = MkAtomTerm(NameOfFunctor(d1));
	PREG = NEXTOP(PREG, xxy);
	INITIALIZE_PERMVAR(pt0, MkIntegerTerm(ArityOfFunctor(d1)));
	GONext();
      } else if (IsPairTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XY_D0PAIR);
///#endif
	CELL *pt0 = YREG+PREG->y_u.xxy.y2;
	XREG(PREG->y_u.xxy.x1) = TermDot;
	PREG = NEXTOP(PREG, xxy);
	INITIALIZE_PERMVAR(pt0, MkIntTerm(2));
	GONext();
      } else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XY_D0NOCOMPOUND);
///#endif
	CELL *pt0 = YREG+PREG->y_u.xxy.y2;
	XREG(PREG->y_u.xxy.x1) = d0;
	PREG = NEXTOP(PREG, xxy);
	INITIALIZE_PERMVAR(pt0, MkIntTerm(0));
	GONext();
      }

      BEGP(pt1);
      deref_body(d0, pt1, func2f_xy_unk, func2f_xy_nvar);
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_XY_END);
///#endif
      saveregs();
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
      setregs();
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2f_yx, yxx);
///#ifdef PROFILED_ABSMI
        EMIT_ENTRY_BLOCK(PREG,P_FUNC2F_YX_INSTINIT);
///#endif
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YX_LOW_LEVEL_TRACER);
///#endif
	HR[0] = XREG(PREG->y_u.yxx.x2);
	RESET_VARIABLE(HR+1);
	RESET_VARIABLE(HR+2);
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),HR);
      }
#endif	/* LOW_LEVEL_TRACE */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YX_TEST_D0);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.yxx.x2);
      deref_head(d0, func2f_yx_unk);
    func2f_yx_nvar:
      if (IsApplTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YX_D0APPL);
///#endif
	Functor d1 = FunctorOfTerm(d0);
	CELL *pt0 = YREG+PREG->y_u.yxx.y;
	if (IsExtensionFunctor(d1)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YX_D0APPL_D1EXTFUNC);
///#endif
	  XREG(PREG->y_u.yxx.x1) = MkIntTerm(0);
	  PREG = NEXTOP(PREG, yxx);
	  INITIALIZE_PERMVAR(pt0, d0);
	  GONext();
	}
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YX_D0APPL_END);
///#endif
	XREG(PREG->y_u.yxx.x1) = MkIntegerTerm(ArityOfFunctor(d1));
	PREG = NEXTOP(PREG, yxx);
	INITIALIZE_PERMVAR(pt0,  MkAtomTerm(NameOfFunctor(d1)));
	GONext();
      } else if (IsPairTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YX_D0PAIR);
///#endif
	CELL *pt0 = YREG+PREG->y_u.yxx.y;
	XREG(PREG->y_u.yxx.x1) = MkIntTerm(2);
	PREG = NEXTOP(PREG, yxx);
	INITIALIZE_PERMVAR(pt0 ,TermDot);
	GONext();
      } else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YX_D0NOCOMPOUND);
///#endif
	CELL *pt0 = YREG+PREG->y_u.yxx.y;
	XREG(PREG->y_u.yxx.x1) = MkIntTerm(0);
	PREG = NEXTOP(PREG, yxx);
	INITIALIZE_PERMVAR(pt0, d0);
	GONext();
      }

      BEGP(pt1);
      deref_body(d0, pt1, func2f_yx_unk, func2f_yx_nvar);
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YX_END);
///#endif
      saveregs();
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
      setregs();
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2f_yy, yyx);
///#ifdef PROFILED_ABSMI
        EMIT_ENTRY_BLOCK(PREG,P_FUNC2F_YY_INSTINIT);
///#endif
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YY_LOW_LEVEL_TRACER);
///#endif
	HR[0] = XREG(PREG->y_u.yyx.x);
	RESET_VARIABLE(HR+1);
	RESET_VARIABLE(HR+2);
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,2)),HR);
      }
#endif	/* LOW_LEVEL_TRACE */
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YY_TEST_D0);
///#endif
      BEGD(d0);
      d0 = XREG(PREG->y_u.yyx.x);
      deref_head(d0, func2f_yy_unk);
    func2f_yy_nvar:
      if (IsApplTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YY_D0APPL);
///#endif
	Functor d1 = FunctorOfTerm(d0);
	CELL *pt0 = YREG+PREG->y_u.yyx.y1;
	CELL *pt1 = YREG+PREG->y_u.yyx.y2;
	if (IsExtensionFunctor(d1)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YY_D0APPL_D1EXTFUNC);
///#endif
	  PREG = NEXTOP(PREG, yyx);
	  INITIALIZE_PERMVAR(pt0,  d0);
	  INITIALIZE_PERMVAR(pt1, MkIntTerm(0));
	  GONext();
	}
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YY_D0APPL_END);
///#endif
	PREG = NEXTOP(PREG, yyx);
	INITIALIZE_PERMVAR(pt0, MkAtomTerm(NameOfFunctor(d1)));
	INITIALIZE_PERMVAR(pt1, MkIntegerTerm(ArityOfFunctor(d1)));
	GONext();
      } else if (IsPairTerm(d0)) {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YY_D0PAIR);
///#endif
	CELL *pt0 = YREG+PREG->y_u.yyx.y1;
	CELL *pt1 = YREG+PREG->y_u.yyx.y2;
	PREG = NEXTOP(PREG, yyx);
	INITIALIZE_PERMVAR(pt0, TermDot);
	INITIALIZE_PERMVAR(pt1, MkIntTerm(2));
	GONext();
      } else {
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YY_D0NOCOMPOUND);
///#endif
	CELL *pt0 = YREG+PREG->y_u.yyx.y1;
	CELL *pt1 = YREG+PREG->y_u.yyx.y2;
	PREG = NEXTOP(PREG, yyx);
	INITIALIZE_PERMVAR(pt0, d0);
	INITIALIZE_PERMVAR(pt1, MkIntTerm(0));
	GONext();
      }

      BEGP(pt1);
      deref_body(d0, pt1, func2f_yy_unk, func2f_yy_nvar);
///#ifdef PROFILED_ABSMI
        EMIT_SIMPLE_BLOCK(P_FUNC2F_YY_END);
///#endif
      saveregs();
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3");
      setregs();
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_functor, e);
        EMIT_ENTRY_BLOCK(PREG,P_FUNCTOR_INSTINIT);
        EMIT_MULTIPLE_DESTINY_BLOCK_TEST(P_FUNCTOR_END);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace)
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
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
	Bind(pt0, d0);
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
	Bind(pt0, d0);
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
	    Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage);
	    setregs();
	    JMPNext();
	  } else {
	    setregs();
	  }
	  goto restart_functor;	/*  */
	}
	while ((Int)d1--) {
	  RESET_VARIABLE(pt1);
	  pt1++;
	}
	/* done building the term */
	HR = pt1;
	ENDP(pt1);
      }	else if ((Int)d1  < 0) {
	saveregs();
	Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	setregs();
	FAIL();
      }
      /* else if arity is 0 just pass d0 through */
      /* Ding, ding, we made it */
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, e),Osbpp),l);
      Bind(pt0, d0);
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
              check_stack(TracedNoStackPExecute, HR);
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
                  if (DEPTH == MkIntTerm(0)) {
                    FAIL(); 
                  } else {
                    DEPTH = RESET_DEPTH();
                  }
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
	    TracedNoStackPExecute:
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

	    BOp(unify_l_string             ,ou);
            goto _Ystop;
            ENDBOp();
            BOp(unify_string             ,ou);
            goto _Ystop;
            ENDBOp();
            BOp(get_string                 ,xu);
            goto _Ystop;
            ENDBOp();
            BOp(get_atom_exo               ,x);
            goto _Ystop;
            ENDBOp();
            BOp(retry_exo_udi              ,lp);
            goto _Ystop;
            ENDBOp();
            BOp(retry_exo                  ,lp);
            goto _Ystop;
            ENDBOp();
            BOp(retry_udi                  ,p);
            goto _Ystop;
            ENDBOp();
            BOp(retry_all_exo              ,lp);
            goto _Ystop;
            ENDBOp();
            BOp(enter_exo                  ,e);
            goto _Ystop;
            ENDBOp();
            BOp(try_exo                    ,lp);
            goto _Ystop;
            ENDBOp();
            BOp(try_exo_udi                ,lp);
            goto _Ystop;
            ENDBOp();
            BOp(try_all_exo                ,lp);
            goto _Ystop;
            ENDBOp();
            BOp(try_udi                    ,p);
            goto _Ystop;
            ENDBOp();

	    
