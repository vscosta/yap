#define COUNT_CALL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      LOCK((*_PREG)->u.p.p->StatisticsForPred->lock); \
      (*_PREG)->u.p.p->StatisticsForPred->NOfEntries++; \
      UNLOCK((*_PREG)->u.p.p->StatisticsForPred->lock); \
      LOCAL_ReductionsCounter--;

#define COUNT_CALL_MIDDLE \
      if (LOCAL_ReductionsCounter == 0 && LOCAL_ReductionsCounterOn) { \
	    saveregs(); \
	    Yap_NilError(CALL_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext(); \
      } \
	  else { \
        LOCAL_PredEntriesCounter--; \
        if (LOCAL_PredEntriesCounter == 0 && LOCAL_PredEntriesCounterOn) { \
	      saveregs(); \
	      Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	      setregs(); \
	      JMPNext(); \
        } \
	  }

#define COUNT_CALL_END \
      (*_PREG) = NEXTOP((*_PREG), p); \
      GONext();

#define COUNT_RETRY_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      LOCK((*_PREG)->u.p.p->StatisticsForPred->lock); \
      (*_PREG)->u.p.p->StatisticsForPred->NOfRetries++; \
      UNLOCK((*_PREG)->u.p.p->StatisticsForPred->lock); \
      LOCAL_RetriesCounter--;

#define COUNT_RETRY_MIDDLE \
      if (LOCAL_RetriesCounter == 0 && LOCAL_RetriesCounterOn) { \
	    ENV = B->cp_env; \
	    saveregs(); \
	    Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext(); \
      } \
	  else { \
        LOCAL_PredEntriesCounter--; \
        if (LOCAL_PredEntriesCounter == 0 && LOCAL_PredEntriesCounterOn) { \
	      ENV = B->cp_env; \
	      saveregs(); \
	      Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	      setregs(); \
	      JMPNext(); \
        } \
	  }

#define COUNT_RETRY_END \
      (*_PREG) = NEXTOP((*_PREG), p); \
      GONext();

#define COUNT_RETRY_ME_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      CACHE_Y(B); \
      restore_yaam_regs((*_PREG)->u.Otapl.d); \
      restore_args((*_PREG)->u.Otapl.s);
	  
#ifdef FROZEN_STACKS
#define COUNT_RETRY_ME_MIDDLE \
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
      set_cut(S_YREG, B->cp_b); \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      LOCK(((PredEntry *)((*_PREG)->u.Otapl.p))->StatisticsForPred->lock); \
      ((PredEntry *)((*_PREG)->u.Otapl.p))->StatisticsForPred->NOfRetries++; \
      UNLOCK(((PredEntry *)((*_PREG)->u.Otapl.p))->StatisticsForPred->lock); \
      LOCAL_RetriesCounter--; \
      if (LOCAL_RetriesCounter == 0 && LOCAL_RetriesCounterOn) { \
	saveregs(); \
	Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	setregs(); \
	JMPNext(); \
      } \
      LOCAL_PredEntriesCounter--; \
      if (LOCAL_PredEntriesCounter == 0 && LOCAL_PredEntriesCounterOn) { \
	saveregs(); \
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	setregs(); \
	JMPNext(); \
      }
#else /* FROZEN_STACKS */
#define COUNT_RETRY_ME_MIDDLE \
      set_cut(S_YREG, B_YREG->cp_b); \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      LOCK(((PredEntry *)((*_PREG)->u.Otapl.p))->StatisticsForPred->lock); \
      ((PredEntry *)((*_PREG)->u.Otapl.p))->StatisticsForPred->NOfRetries++; \
      UNLOCK(((PredEntry *)((*_PREG)->u.Otapl.p))->StatisticsForPred->lock); \
      LOCAL_RetriesCounter--; \
      if (LOCAL_RetriesCounter == 0 && LOCAL_RetriesCounterOn) { \
	saveregs(); \
	Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	setregs(); \
	JMPNext(); \
      } \
      LOCAL_PredEntriesCounter--; \
      if (LOCAL_PredEntriesCounter == 0 && LOCAL_PredEntriesCounterOn) { \
	saveregs(); \
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	setregs(); \
	JMPNext(); \
      }
#endif /* FROZEN_STACKS */

#define COUNT_RETRY_ME_END \
      (*_PREG) = NEXTOP((*_PREG), Otapl); \
      GONext();

#define COUNT_TRUST_ME_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      CACHE_Y(B);
	  
#ifdef YAPOR
#ifdef FROZEN_STACKS
#define COUNT_TRUST_ME_MIDDLE \
      if (SCH_top_shared_cp(B)) { \
	SCH_last_alternative((*_PREG), B_YREG); \
	restore_args((*_PREG)->u.Otapl.s); \
        S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	set_cut(S_YREG, B->cp_b); \
      } \
      else \
      { \
	pop_yaam_regs(); \
	pop_args((*_PREG)->u.Otapl.s); \
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	set_cut(S_YREG, B); \
      } \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      LOCAL_RetriesCounter--; \
      if (LOCAL_RetriesCounter == 0) { \
	saveregs(); \
	Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	setregs(); \
	JMPNext(); \
      } \
      LOCAL_PredEntriesCounter--; \
      if (LOCAL_PredEntriesCounter == 0) { \
	saveregs(); \
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	setregs(); \
	JMPNext(); \
      }
#else /* FROZEN_STACKS */
#define COUNT_TRUST_ME_MIDDLE \
      if (SCH_top_shared_cp(B)) { \
	SCH_last_alternative((*_PREG), B_YREG); \
	restore_args((*_PREG)->u.Otapl.s); \
	set_cut(S_YREG, B->cp_b); \
      } \
      else \
      { \
	pop_yaam_regs(); \
	pop_args((*_PREG)->u.Otapl.s); \
	set_cut(S_YREG, B); \
      } \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      LOCAL_RetriesCounter--; \
      if (LOCAL_RetriesCounter == 0) { \
	saveregs(); \
	Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	setregs(); \
	JMPNext(); \
      } \
      LOCAL_PredEntriesCounter--; \
      if (LOCAL_PredEntriesCounter == 0) { \
	saveregs(); \
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	setregs(); \
	JMPNext(); \
      }
#endif /* FROZEN_STACKS */
#else /* YAPOR */
#ifdef FROZEN_STACKS
#define COUNT_TRUST_ME_MIDDLE \
      { \
	pop_yaam_regs(); \
	pop_args((*_PREG)->u.Otapl.s); \
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	set_cut(S_YREG, B); \
      } \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      LOCAL_RetriesCounter--; \
      if (LOCAL_RetriesCounter == 0) { \
	saveregs(); \
	Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	setregs(); \
	JMPNext(); \
      } \
      LOCAL_PredEntriesCounter--; \
      if (LOCAL_PredEntriesCounter == 0) { \
	saveregs(); \
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	setregs(); \
	JMPNext(); \
      }
#else /* FROZEN_STACKS */
#define COUNT_TRUST_ME_MIDDLE \
      { \
	pop_yaam_regs(); \
	pop_args((*_PREG)->u.Otapl.s); \
	set_cut(S_YREG, B); \
      } \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      LOCAL_RetriesCounter--; \
      if (LOCAL_RetriesCounter == 0) { \
	saveregs(); \
	Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	setregs(); \
	JMPNext(); \
      } \
      LOCAL_PredEntriesCounter--; \
      if (LOCAL_PredEntriesCounter == 0) { \
	saveregs(); \
	Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	setregs(); \
	JMPNext(); \
      }
#endif /* FROZEN_STACKS */
#endif /* YAPOR */

#define COUNT_TRUST_ME_END \
      LOCK(((PredEntry *)((*_PREG)->u.Otapl.p))->StatisticsForPred->lock); \
      ((PredEntry *)((*_PREG)->u.Otapl.p))->StatisticsForPred->NOfRetries++; \
      UNLOCK(((PredEntry *)((*_PREG)->u.Otapl.p))->StatisticsForPred->lock); \
      (*_PREG) = NEXTOP((*_PREG), Otapl); \
      GONext();

#ifdef THREADS  
#ifdef FROZEN_STACKS
#define COUNT_RETRY_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
    YAAM_CHECK_TRAIL_TR; \
	  { \
	UInt timestamp; \
	CACHE_Y(B); \
	timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[(*_PREG)->u.OtaLl.s]); \
	if (!VALID_TIMESTAMP(timestamp, (*_PREG)->u.OtaLl.d)) { \
	  (*_PREG)=(*_PREG)->u.OtaLl.n; \
	  JMPNext(); \
	} \
	restore_yaam_regs((*_PREG)->u.OtaLl.n); \
	restore_args((*_PREG)->u.OtaLl.s); \
	LOCAL_RetriesCounter--; \
	if (LOCAL_RetriesCounter == 0) { \
	  saveregs(); \
	  Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	  setregs(); \
	  JMPNext(); \
	} \
	LOCAL_PredEntriesCounter--; \
	if (LOCAL_PredEntriesCounter == 0) { \
	  saveregs(); \
	  Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	  setregs(); \
	  JMPNext(); \
	} \
	LOCK((*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->lock); \
	(*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->NOfRetries++; \
	UNLOCK((*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->lock); \
	PP = (*_PREG)->u.OtaLl.d->ClPred; \
	(*_PREG) = (*_PREG)->u.OtaLl.d->ClCode; \
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	set_cut(S_YREG, B->cp_b); \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
      } \
      JMPNext();
#else /* FROZEN_STACKS */
#define COUNT_RETRY_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
    YAAM_CHECK_TRAIL_TR; \
	  { \
	UInt timestamp; \
	CACHE_Y(B); \
	timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[(*_PREG)->u.OtaLl.s]); \
	if (!VALID_TIMESTAMP(timestamp, (*_PREG)->u.OtaLl.d)) { \
	  (*_PREG)=(*_PREG)->u.OtaLl.n; \
	  JMPNext(); \
	} \
	restore_yaam_regs((*_PREG)->u.OtaLl.n); \
	restore_args((*_PREG)->u.OtaLl.s); \
	LOCAL_RetriesCounter--; \
	if (LOCAL_RetriesCounter == 0) { \
	  saveregs(); \
	  Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	  setregs(); \
	  JMPNext(); \
	} \
	LOCAL_PredEntriesCounter--; \
	if (LOCAL_PredEntriesCounter == 0) { \
	  saveregs(); \
	  Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	  setregs(); \
	  JMPNext(); \
	} \
	LOCK((*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->lock); \
	(*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->NOfRetries++; \
	UNLOCK((*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->lock); \
	PP = (*_PREG)->u.OtaLl.d->ClPred; \
	(*_PREG) = (*_PREG)->u.OtaLl.d->ClCode; \
	set_cut(S_YREG, B_YREG->cp_b); \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
      } \
      JMPNext();
#endif /* FROZEN_STACKS */
#else /* THREADS   */
#ifdef FROZEN_STACKS
#define COUNT_RETRY_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
    YAAM_CHECK_TRAIL_TR; \
	  { \
	UInt timestamp; \
	CACHE_Y(B); \
	timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[(*_PREG)->u.OtaLl.s]); \
	if (!VALID_TIMESTAMP(timestamp, (*_PREG)->u.OtaLl.d)) { \
	  (*_PREG)=(*_PREG)->u.OtaLl.n; \
	  JMPNext(); \
	} \
	restore_yaam_regs((*_PREG)->u.OtaLl.n); \
	restore_args((*_PREG)->u.OtaLl.s); \
	LOCAL_RetriesCounter--; \
	if (LOCAL_RetriesCounter == 0) { \
	  saveregs(); \
	  Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	  setregs(); \
	  JMPNext(); \
	} \
	LOCAL_PredEntriesCounter--; \
	if (LOCAL_PredEntriesCounter == 0) { \
	  saveregs(); \
	  Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	  setregs(); \
	  JMPNext(); \
	} \
	LOCK((*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->lock); \
	(*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->NOfRetries++; \
	UNLOCK((*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->lock); \
	(*_PREG) = (*_PREG)->u.OtaLl.d->ClCode; \
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	set_cut(S_YREG, B->cp_b); \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
      } \
      JMPNext();
#else /* FROZEN_STACKS */
#define COUNT_RETRY_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
    YAAM_CHECK_TRAIL_TR; \
	  { \
	UInt timestamp; \
	CACHE_Y(B); \
	timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[(*_PREG)->u.OtaLl.s]); \
	if (!VALID_TIMESTAMP(timestamp, (*_PREG)->u.OtaLl.d)) { \
	  (*_PREG)=(*_PREG)->u.OtaLl.n; \
	  JMPNext(); \
	} \
	restore_yaam_regs((*_PREG)->u.OtaLl.n); \
	restore_args((*_PREG)->u.OtaLl.s); \
	LOCAL_RetriesCounter--; \
	if (LOCAL_RetriesCounter == 0) { \
	  saveregs(); \
	  Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	  setregs(); \
	  JMPNext(); \
	} \
	LOCAL_PredEntriesCounter--; \
	if (LOCAL_PredEntriesCounter == 0) { \
	  saveregs(); \
	  Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	  setregs(); \
	  JMPNext(); \
	} \
	LOCK((*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->lock); \
	(*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->NOfRetries++; \
	UNLOCK((*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->lock); \
	(*_PREG) = (*_PREG)->u.OtaLl.d->ClCode; \
	set_cut(S_YREG, B_YREG->cp_b); \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
      } \
      JMPNext();
#endif /* FROZEN_STACKS */
#endif /* THREADS   */

#define COUNT_RETRY_LOGICAL_END \
      BLOCK = (CELL)COUNT_RETRY_LOGICAL_END;

#if MULTIPLE_STACKS
#ifdef YAPOR
#ifdef FROZEN_STACKS
#define COUNT_TRUST_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  CACHE_Y(B); \
      { \
	LogUpdIndex *cl = (*_PREG)->u.OtILl.block; \
	PredEntry *ap = cl->ClPred; \
	LogUpdClause *lcl = (*_PREG)->u.OtILl.d; \
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]); \
	if (!VALID_TIMESTAMP(timestamp, lcl)) { \
	  (*_PREG) = FAILCODE; \
	} else { \
	  LOCAL_RetriesCounter--; \
	  if (LOCAL_RetriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCAL_PredEntriesCounter--; \
	  if (LOCAL_PredEntriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	PELOCK(2, ap); \
	PP = ap; \
	DEC_CLREF_COUNT(cl); \
	--B->cp_tr; \
	TR = B->cp_tr; \
	if (cl->ClRefCount == 0 && (cl->ClFlags & (ErasedMask|DirtyMask))) { \
	  if ((*_PREG) != FAILCODE) { \
	    if (lcl->ClRefCount == 1) { \
	      INC_CLREF_COUNT(lcl); \
	      TRAIL_CLREF(lcl); \
	    } \
	  } \
	  if (cl->ClFlags & ErasedMask) { \
	    saveregs(); \
	    Yap_ErLogUpdIndex(cl); \
	    setregs(); \
	  } else { \
	    saveregs(); \
	    Yap_CleanUpIndex(cl); \
	    setregs(); \
	  } \
	  save_pc(); \
	} \
	if (SCH_top_shared_cp(B)) { \
	  SCH_last_alternative((*_PREG), B_YREG); \
	  restore_args(ap->ArityOfPE); \
	  S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	  set_cut(S_YREG, B->cp_b); \
	} else \
	  { \
	    pop_yaam_regs(); \
	    pop_args(ap->ArityOfPE); \
	    S_YREG--; \
	    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	    set_cut(S_YREG, B); \
	  } \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
	JMPNext(); \
      }
#else /* FROZEN_STACKS */
#define COUNT_TRUST_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  CACHE_Y(B); \
      { \
	LogUpdIndex *cl = (*_PREG)->u.OtILl.block; \
	PredEntry *ap = cl->ClPred; \
	LogUpdClause *lcl = (*_PREG)->u.OtILl.d; \
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]); \
	if (!VALID_TIMESTAMP(timestamp, lcl)) { \
	  (*_PREG) = FAILCODE; \
	} else { \
	  LOCAL_RetriesCounter--; \
	  if (LOCAL_RetriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCAL_PredEntriesCounter--; \
	  if (LOCAL_PredEntriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	PELOCK(2, ap); \
	PP = ap; \
	DEC_CLREF_COUNT(cl); \
	--B->cp_tr; \
	TR = B->cp_tr; \
	if (cl->ClRefCount == 0 && (cl->ClFlags & (ErasedMask|DirtyMask))) { \
	  if ((*_PREG) != FAILCODE) { \
	    if (lcl->ClRefCount == 1) { \
	      INC_CLREF_COUNT(lcl); \
	      TRAIL_CLREF(lcl); \
	    } \
	  } \
	  if (cl->ClFlags & ErasedMask) { \
	    saveregs(); \
	    Yap_ErLogUpdIndex(cl); \
	    setregs(); \
	  } else { \
	    saveregs(); \
	    Yap_CleanUpIndex(cl); \
	    setregs(); \
	  } \
	  save_pc(); \
	} \
	if (SCH_top_shared_cp(B)) { \
	  SCH_last_alternative((*_PREG), B_YREG); \
	  restore_args(ap->ArityOfPE); \
	  S_YREG++; \
	  set_cut(S_YREG, B->cp_b); \
	} else \
	  { \
	    pop_yaam_regs(); \
	    pop_args(ap->ArityOfPE); \
	    S_YREG--; \
	    set_cut(S_YREG, B); \
	  } \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
	JMPNext(); \
      }
#endif /* FROZEN_STACKS */
#else /* YAPOR */
#ifdef FROZEN_STACKS
#define COUNT_TRUST_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  CACHE_Y(B); \
      { \
	LogUpdIndex *cl = (*_PREG)->u.OtILl.block; \
	PredEntry *ap = cl->ClPred; \
	LogUpdClause *lcl = (*_PREG)->u.OtILl.d; \
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]); \
	if (!VALID_TIMESTAMP(timestamp, lcl)) { \
	  (*_PREG) = FAILCODE; \
	} else { \
	  LOCAL_RetriesCounter--; \
	  if (LOCAL_RetriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCAL_PredEntriesCounter--; \
	  if (LOCAL_PredEntriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	PELOCK(2, ap); \
	PP = ap; \
	DEC_CLREF_COUNT(cl); \
	--B->cp_tr; \
	TR = B->cp_tr; \
	if (cl->ClRefCount == 0 && (cl->ClFlags & (ErasedMask|DirtyMask))) { \
	  if ((*_PREG) != FAILCODE) { \
	    if (lcl->ClRefCount == 1) { \
	      INC_CLREF_COUNT(lcl); \
	      TRAIL_CLREF(lcl); \
	    } \
	  } \
	  if (cl->ClFlags & ErasedMask) { \
	    saveregs(); \
	    Yap_ErLogUpdIndex(cl); \
	    setregs(); \
	  } else { \
	    saveregs(); \
	    Yap_CleanUpIndex(cl); \
	    setregs(); \
	  } \
	  save_pc(); \
	} \
	  { \
	    pop_yaam_regs(); \
	    pop_args(ap->ArityOfPE); \
	    S_YREG--; \
	    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	    set_cut(S_YREG, B); \
	  } \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
	JMPNext(); \
      }
#else /* FROZEN_STACKS */
#define COUNT_TRUST_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  CACHE_Y(B); \
      { \
	LogUpdIndex *cl = (*_PREG)->u.OtILl.block; \
	PredEntry *ap = cl->ClPred; \
	LogUpdClause *lcl = (*_PREG)->u.OtILl.d; \
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]); \
	if (!VALID_TIMESTAMP(timestamp, lcl)) { \
	  (*_PREG) = FAILCODE; \
	} else { \
	  LOCAL_RetriesCounter--; \
	  if (LOCAL_RetriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCAL_PredEntriesCounter--; \
	  if (LOCAL_PredEntriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	PELOCK(2, ap); \
	PP = ap; \
	DEC_CLREF_COUNT(cl); \
	--B->cp_tr; \
	TR = B->cp_tr; \
	if (cl->ClRefCount == 0 && (cl->ClFlags & (ErasedMask|DirtyMask))) { \
	  if ((*_PREG) != FAILCODE) { \
	    if (lcl->ClRefCount == 1) { \
	      INC_CLREF_COUNT(lcl); \
	      TRAIL_CLREF(lcl); \
	    } \
	  } \
	  if (cl->ClFlags & ErasedMask) { \
	    saveregs(); \
	    Yap_ErLogUpdIndex(cl); \
	    setregs(); \
	  } else { \
	    saveregs(); \
	    Yap_CleanUpIndex(cl); \
	    setregs(); \
	  } \
	  save_pc(); \
	} \
	  { \
	    pop_yaam_regs(); \
	    pop_args(ap->ArityOfPE); \
	    S_YREG--; \
	    set_cut(S_YREG, B); \
	  } \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
	JMPNext(); \
      }
#endif /* FROZEN_STACKS */
#endif /* YAPOR */
#else /* MULTIPLE_STACKS */
#if FROZEN_STACKS
#ifdef YAPOR
#ifdef FROZEN_STACKS
#define COUNT_TRUST_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  CACHE_Y(B); \
      { \
	LogUpdIndex *cl = (*_PREG)->u.OtILl.block; \
	PredEntry *ap = cl->ClPred; \
	LogUpdClause *lcl = (*_PREG)->u.OtILl.d; \
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]); \
	if (!VALID_TIMESTAMP(timestamp, lcl)) { \
	  (*_PREG) = FAILCODE; \
	} else { \
	  LOCAL_RetriesCounter--; \
	  if (LOCAL_RetriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCAL_PredEntriesCounter--; \
	  if (LOCAL_PredEntriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	if (TrailTerm(B->cp_tr-1) == CLREF_TO_TRENTRY(cl) && \
	    B->cp_tr != B->cp_b->cp_tr) { \
	  cl->ClFlags &= ~InUseMask; \
	  --B->cp_tr; \
	  if (B->cp_tr > TR_FZ) \
	    { \
	      TR = B->cp_tr; \
	    }	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
	    if ((*_PREG) != FAILCODE) { \
	      if (lcl->ClRefCount == 1 && !(lcl->ClFlags & InUseMask)) { \
		lcl->ClFlags |= InUseMask; \
		TRAIL_CLREF(lcl); \
	      } \
	    } \
	    if (cl->ClFlags & ErasedMask) { \
	      saveregs(); \
	      Yap_ErLogUpdIndex(cl); \
	      setregs(); \
	    } else { \
	      saveregs(); \
	      Yap_CleanUpIndex(cl); \
	      setregs(); \
	    } \
	    save_pc(); \
	  } \
	} \
	if (SCH_top_shared_cp(B)) { \
	  SCH_last_alternative((*_PREG), B_YREG); \
	  restore_args(ap->ArityOfPE); \
	  S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	  set_cut(S_YREG, B->cp_b); \
	} else \
	  { \
	    pop_yaam_regs(); \
	    pop_args(ap->ArityOfPE); \
	    S_YREG--; \
	    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	    set_cut(S_YREG, B); \
	  } \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
	JMPNext(); \
      }
#else /* FROZEN_STACKS */
#define COUNT_TRUST_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  CACHE_Y(B); \
      { \
	LogUpdIndex *cl = (*_PREG)->u.OtILl.block; \
	PredEntry *ap = cl->ClPred; \
	LogUpdClause *lcl = (*_PREG)->u.OtILl.d; \
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]); \
	if (!VALID_TIMESTAMP(timestamp, lcl)) { \
	  (*_PREG) = FAILCODE; \
	} else { \
	  LOCAL_RetriesCounter--; \
	  if (LOCAL_RetriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCAL_PredEntriesCounter--; \
	  if (LOCAL_PredEntriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	if (TrailTerm(B->cp_tr-1) == CLREF_TO_TRENTRY(cl) && \
	    B->cp_tr != B->cp_b->cp_tr) { \
	  cl->ClFlags &= ~InUseMask; \
	  --B->cp_tr; \
	  if (B->cp_tr > TR_FZ) \
	    { \
	      TR = B->cp_tr; \
	    }	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
	    if ((*_PREG) != FAILCODE) { \
	      if (lcl->ClRefCount == 1 && !(lcl->ClFlags & InUseMask)) { \
		lcl->ClFlags |= InUseMask; \
		TRAIL_CLREF(lcl); \
	      } \
	    } \
	    if (cl->ClFlags & ErasedMask) { \
	      saveregs(); \
	      Yap_ErLogUpdIndex(cl); \
	      setregs(); \
	    } else { \
	      saveregs(); \
	      Yap_CleanUpIndex(cl); \
	      setregs(); \
	    } \
	    save_pc(); \
	  } \
	} \
	if (SCH_top_shared_cp(B)) { \
	  SCH_last_alternative((*_PREG), B_YREG); \
	  restore_args(ap->ArityOfPE); \
	  S_YREG++; \
	  set_cut(S_YREG, B->cp_b); \
	} else \
	  { \
	    pop_yaam_regs(); \
	    pop_args(ap->ArityOfPE); \
	    S_YREG--; \
	    set_cut(S_YREG, B); \
	  } \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
	JMPNext(); \
      }
#endif /* FROZEN_STACKS */
#else /* YAPOR */
#ifdef FROZEN_STACKS
#define COUNT_TRUST_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  CACHE_Y(B); \
      { \
	LogUpdIndex *cl = (*_PREG)->u.OtILl.block; \
	PredEntry *ap = cl->ClPred; \
	LogUpdClause *lcl = (*_PREG)->u.OtILl.d; \
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]); \
	if (!VALID_TIMESTAMP(timestamp, lcl)) { \
	  (*_PREG) = FAILCODE; \
	} else { \
	  LOCAL_RetriesCounter--; \
	  if (LOCAL_RetriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCAL_PredEntriesCounter--; \
	  if (LOCAL_PredEntriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	if (TrailTerm(B->cp_tr-1) == CLREF_TO_TRENTRY(cl) && \
	    B->cp_tr != B->cp_b->cp_tr) { \
	  cl->ClFlags &= ~InUseMask; \
	  --B->cp_tr; \
	  if (B->cp_tr > TR_FZ) \
	    { \
	      TR = B->cp_tr; \
	    }	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
	    if ((*_PREG) != FAILCODE) { \
	      if (lcl->ClRefCount == 1 && !(lcl->ClFlags & InUseMask)) { \
		lcl->ClFlags |= InUseMask; \
		TRAIL_CLREF(lcl); \
	      } \
	    } \
	    if (cl->ClFlags & ErasedMask) { \
	      saveregs(); \
	      Yap_ErLogUpdIndex(cl); \
	      setregs(); \
	    } else { \
	      saveregs(); \
	      Yap_CleanUpIndex(cl); \
	      setregs(); \
	    } \
	    save_pc(); \
	  } \
	} \
	  { \
	    pop_yaam_regs(); \
	    pop_args(ap->ArityOfPE); \
	    S_YREG--; \
	    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	    set_cut(S_YREG, B); \
	  } \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
	JMPNext(); \
      }
#else /* FROZEN_STACKS */
#define COUNT_TRUST_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  CACHE_Y(B); \
      { \
	LogUpdIndex *cl = (*_PREG)->u.OtILl.block; \
	PredEntry *ap = cl->ClPred; \
	LogUpdClause *lcl = (*_PREG)->u.OtILl.d; \
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]); \
	if (!VALID_TIMESTAMP(timestamp, lcl)) { \
	  (*_PREG) = FAILCODE; \
	} else { \
	  LOCAL_RetriesCounter--; \
	  if (LOCAL_RetriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCAL_PredEntriesCounter--; \
	  if (LOCAL_PredEntriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	if (TrailTerm(B->cp_tr-1) == CLREF_TO_TRENTRY(cl) && \
	    B->cp_tr != B->cp_b->cp_tr) { \
	  cl->ClFlags &= ~InUseMask; \
	  --B->cp_tr; \
	  if (B->cp_tr > TR_FZ) \
	    { \
	      TR = B->cp_tr; \
	    }	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
	    if ((*_PREG) != FAILCODE) { \
	      if (lcl->ClRefCount == 1 && !(lcl->ClFlags & InUseMask)) { \
		lcl->ClFlags |= InUseMask; \
		TRAIL_CLREF(lcl); \
	      } \
	    } \
	    if (cl->ClFlags & ErasedMask) { \
	      saveregs(); \
	      Yap_ErLogUpdIndex(cl); \
	      setregs(); \
	    } else { \
	      saveregs(); \
	      Yap_CleanUpIndex(cl); \
	      setregs(); \
	    } \
	    save_pc(); \
	  } \
	} \
	  { \
	    pop_yaam_regs(); \
	    pop_args(ap->ArityOfPE); \
	    S_YREG--; \
	    set_cut(S_YREG, B); \
	  } \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
	JMPNext(); \
      }
#endif /* FROZEN_STACKS */
#endif /* YAPOR */
#else /* FROZEN_STACKS */
#ifdef YAPOR
#ifdef FROZEN_STACKS
#define COUNT_TRUST_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  CACHE_Y(B); \
      { \
	LogUpdIndex *cl = (*_PREG)->u.OtILl.block; \
	PredEntry *ap = cl->ClPred; \
	LogUpdClause *lcl = (*_PREG)->u.OtILl.d; \
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]); \
	if (!VALID_TIMESTAMP(timestamp, lcl)) { \
	  (*_PREG) = FAILCODE; \
	} else { \
	  LOCAL_RetriesCounter--; \
	  if (LOCAL_RetriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCAL_PredEntriesCounter--; \
	  if (LOCAL_PredEntriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	if (TrailTerm(B->cp_tr-1) == CLREF_TO_TRENTRY(cl) && \
	    B->cp_tr != B->cp_b->cp_tr) { \
	  cl->ClFlags &= ~InUseMask; \
	  --B->cp_tr; \
	    { \
	      TR = B->cp_tr; \
	    }	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
	    if ((*_PREG) != FAILCODE) { \
	      if (lcl->ClRefCount == 1 && !(lcl->ClFlags & InUseMask)) { \
		lcl->ClFlags |= InUseMask; \
		TRAIL_CLREF(lcl); \
	      } \
	    } \
	    if (cl->ClFlags & ErasedMask) { \
	      saveregs(); \
	      Yap_ErLogUpdIndex(cl); \
	      setregs(); \
	    } else { \
	      saveregs(); \
	      Yap_CleanUpIndex(cl); \
	      setregs(); \
	    } \
	    save_pc(); \
	  } \
	} \
	if (SCH_top_shared_cp(B)) { \
	  SCH_last_alternative((*_PREG), B_YREG); \
	  restore_args(ap->ArityOfPE); \
	  S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	  set_cut(S_YREG, B->cp_b); \
	} else \
	  { \
	    pop_yaam_regs(); \
	    pop_args(ap->ArityOfPE); \
	    S_YREG--; \
	    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	    set_cut(S_YREG, B); \
	  } \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
	JMPNext(); \
      }
#else /* FROZEN_STACKS */
#define COUNT_TRUST_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  CACHE_Y(B); \
      { \
	LogUpdIndex *cl = (*_PREG)->u.OtILl.block; \
	PredEntry *ap = cl->ClPred; \
	LogUpdClause *lcl = (*_PREG)->u.OtILl.d; \
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]); \
	if (!VALID_TIMESTAMP(timestamp, lcl)) { \
	  (*_PREG) = FAILCODE; \
	} else { \
	  LOCAL_RetriesCounter--; \
	  if (LOCAL_RetriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCAL_PredEntriesCounter--; \
	  if (LOCAL_PredEntriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	if (TrailTerm(B->cp_tr-1) == CLREF_TO_TRENTRY(cl) && \
	    B->cp_tr != B->cp_b->cp_tr) { \
	  cl->ClFlags &= ~InUseMask; \
	  --B->cp_tr; \
	    { \
	      TR = B->cp_tr; \
	    }	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
	    if ((*_PREG) != FAILCODE) { \
	      if (lcl->ClRefCount == 1 && !(lcl->ClFlags & InUseMask)) { \
		lcl->ClFlags |= InUseMask; \
		TRAIL_CLREF(lcl); \
	      } \
	    } \
	    if (cl->ClFlags & ErasedMask) { \
	      saveregs(); \
	      Yap_ErLogUpdIndex(cl); \
	      setregs(); \
	    } else { \
	      saveregs(); \
	      Yap_CleanUpIndex(cl); \
	      setregs(); \
	    } \
	    save_pc(); \
	  } \
	} \
	if (SCH_top_shared_cp(B)) { \
	  SCH_last_alternative((*_PREG), B_YREG); \
	  restore_args(ap->ArityOfPE); \
	  S_YREG++; \
	  set_cut(S_YREG, B->cp_b); \
	} else \
	  { \
	    pop_yaam_regs(); \
	    pop_args(ap->ArityOfPE); \
	    S_YREG--; \
	    set_cut(S_YREG, B); \
	  } \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
	JMPNext(); \
      }
#endif /* FROZEN_STACKS */
#else /* YAPOR */
#ifdef FROZEN_STACKS
#define COUNT_TRUST_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  CACHE_Y(B); \
      { \
	LogUpdIndex *cl = (*_PREG)->u.OtILl.block; \
	PredEntry *ap = cl->ClPred; \
	LogUpdClause *lcl = (*_PREG)->u.OtILl.d; \
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]); \
	if (!VALID_TIMESTAMP(timestamp, lcl)) { \
	  (*_PREG) = FAILCODE; \
	} else { \
	  LOCAL_RetriesCounter--; \
	  if (LOCAL_RetriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCAL_PredEntriesCounter--; \
	  if (LOCAL_PredEntriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	if (TrailTerm(B->cp_tr-1) == CLREF_TO_TRENTRY(cl) && \
	    B->cp_tr != B->cp_b->cp_tr) { \
	  cl->ClFlags &= ~InUseMask; \
	  --B->cp_tr; \
	    { \
	      TR = B->cp_tr; \
	    }	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
	    if ((*_PREG) != FAILCODE) { \
	      if (lcl->ClRefCount == 1 && !(lcl->ClFlags & InUseMask)) { \
		lcl->ClFlags |= InUseMask; \
		TRAIL_CLREF(lcl); \
	      } \
	    } \
	    if (cl->ClFlags & ErasedMask) { \
	      saveregs(); \
	      Yap_ErLogUpdIndex(cl); \
	      setregs(); \
	    } else { \
	      saveregs(); \
	      Yap_CleanUpIndex(cl); \
	      setregs(); \
	    } \
	    save_pc(); \
	  } \
	} \
	  { \
	    pop_yaam_regs(); \
	    pop_args(ap->ArityOfPE); \
	    S_YREG--; \
	    S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	    set_cut(S_YREG, B); \
	  } \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
	JMPNext(); \
      }
#else /* FROZEN_STACKS */
#define COUNT_TRUST_LOGICAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  CACHE_Y(B); \
      { \
	LogUpdIndex *cl = (*_PREG)->u.OtILl.block; \
	PredEntry *ap = cl->ClPred; \
	LogUpdClause *lcl = (*_PREG)->u.OtILl.d; \
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]); \
	if (!VALID_TIMESTAMP(timestamp, lcl)) { \
	  (*_PREG) = FAILCODE; \
	} else { \
	  LOCAL_RetriesCounter--; \
	  if (LOCAL_RetriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCAL_PredEntriesCounter--; \
	  if (LOCAL_PredEntriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	    setregs(); \
	    JMPNext(); \
	  } \
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	if (TrailTerm(B->cp_tr-1) == CLREF_TO_TRENTRY(cl) && \
	    B->cp_tr != B->cp_b->cp_tr) { \
	  cl->ClFlags &= ~InUseMask; \
	  --B->cp_tr; \
	    { \
	      TR = B->cp_tr; \
	    }	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
	    if ((*_PREG) != FAILCODE) { \
	      if (lcl->ClRefCount == 1 && !(lcl->ClFlags & InUseMask)) { \
		lcl->ClFlags |= InUseMask; \
		TRAIL_CLREF(lcl); \
	      } \
	    } \
	    if (cl->ClFlags & ErasedMask) { \
	      saveregs(); \
	      Yap_ErLogUpdIndex(cl); \
	      setregs(); \
	    } else { \
	      saveregs(); \
	      Yap_CleanUpIndex(cl); \
	      setregs(); \
	    } \
	    save_pc(); \
	  } \
	} \
	  { \
	    pop_yaam_regs(); \
	    pop_args(ap->ArityOfPE); \
	    S_YREG--; \
	    set_cut(S_YREG, B); \
	  } \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
	JMPNext(); \
      }
#endif /* FROZEN_STACKS */
#endif /* YAPOR */
#endif /* FROZEN_STACKS */
#endif /* MULTIPLE_STACKS */

#define COUNT_TRUST_LOGICAL_END \
      BLOCK = (CELL)COUNT_TRUST_LOGICAL_END;

