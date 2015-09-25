#define TRY_ME_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      check_trail(TR); \
      CACHE_Y(YREG); \
      store_at_least_one_arg((*_PREG)->u.Otapl.s); \
      store_yaam_regs((*_PREG)->u.Otapl.d, 0); \
      set_cut(S_YREG, B); \
      B = B_YREG;
	  
#ifdef YAPOR
#define TRY_ME_YAPOR \
      SCH_set_load(B_YREG);
#endif

#define TRY_ME_END \
      BLOCK = (CELL)TRY_ME_END; \
      (*_PREG) = NEXTOP((*_PREG), Otapl); \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      GONext();

#define RETRY_ME_INSTINIT \
      CACHE_Y(B); \
      restore_yaam_regs((*_PREG)->u.Otapl.d); \
      restore_at_least_one_arg((*_PREG)->u.Otapl.s);
	  
#ifdef FROZEN_STACKS
#define RETRY_ME_FROZEN \
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
      set_cut(S_YREG, B->cp_b);
#else
#define RETRY_ME_NOFROZEN \
      set_cut(S_YREG, B_YREG->cp_b);
#endif

#define RETRY_ME_END \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      (*_PREG) = NEXTOP((*_PREG), Otapl); \
      GONext();

#define TRUST_ME_INSTINIT \
      CACHE_Y(B);
	  
#ifdef YAPOR
#ifdef FROZEN_STACKS
#define TRUST_ME_YAPOR_IF \
      if (SCH_top_shared_cp(B)) { \
	SCH_last_alternative((*_PREG), B_YREG); \
	restore_at_least_one_arg((*_PREG)->u.Otapl.s); \
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	set_cut(S_YREG, B->cp_b); \
      } \
      else \
      { \
	pop_yaam_regs(); \
	pop_at_least_one_arg((*_PREG)->u.Otapl.s); \
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	set_cut(S_YREG, B); \
      }
#else /* FROZEN_STACKS */
#define TRUST_ME_YAPOR_IF \
      if (SCH_top_shared_cp(B)) { \
	SCH_last_alternative((*_PREG), B_YREG); \
	restore_at_least_one_arg((*_PREG)->u.Otapl.s); \
	set_cut(S_YREG, B->cp_b); \
      } \
      else \
      { \
	pop_yaam_regs(); \
	pop_at_least_one_arg((*_PREG)->u.Otapl.s); \
	set_cut(S_YREG, B); \
      }
#endif /* FROZEN STACKS */
#else /* YAPOR */
#ifdef FROZEN_STACKS
#define TRUST_ME_YAPOR_IF \
      { \
	pop_yaam_regs(); \
	pop_at_least_one_arg((*_PREG)->u.Otapl.s); \
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	set_cut(S_YREG, B); \
      }
#else /* FROZEN_STACKS */
#define TRUST_ME_IF \
      { \
	pop_yaam_regs(); \
	pop_at_least_one_arg((*_PREG)->u.Otapl.s); \
	set_cut(S_YREG, B); \
      }
#endif /* FROZEN STACKS */
#endif /* YAPOR */

#define TRUST_ME_END \
      (*_PREG) = NEXTOP((*_PREG), Otapl); \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      GONext();

#define ENTER_PROFILING_INSTINIT \
      LOCK((*_PREG)->u.p.p->StatisticsForPred->lock); \
      (*_PREG)->u.p.p->StatisticsForPred->NOfEntries++; \
      UNLOCK((*_PREG)->u.p.p->StatisticsForPred->lock); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      GONext();

#define RETRY_PROFILED_INSTINIT \
      LOCK((*_PREG)->u.p.p->StatisticsForPred->lock); \
      (*_PREG)->u.p.p->StatisticsForPred->NOfRetries++; \
      UNLOCK((*_PREG)->u.p.p->StatisticsForPred->lock); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      GONext();

#define PROFILED_RETRY_ME_INSTINIT \
      CACHE_Y(B); \
      LOCK((*_PREG)->u.Otapl.p->StatisticsForPred->lock); \
      (*_PREG)->u.Otapl.p->StatisticsForPred->NOfRetries++; \
      UNLOCK((*_PREG)->u.Otapl.p->StatisticsForPred->lock); \
      restore_yaam_regs((*_PREG)->u.Otapl.d); \
      restore_args((*_PREG)->u.Otapl.s);
	  
#ifdef FROZEN_STACKS
#define PROFILED_RETRY_ME_FROZEN \
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
      set_cut(S_YREG, B->cp_b);
#else
#define PROFILED_RETRY_ME_NOFROZEN \
      set_cut(S_YREG, B_YREG->cp_b);
#endif

#define PROFILED_RETRY_ME_END \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      (*_PREG) = NEXTOP((*_PREG), Otapl); \
      GONext();

#define PROFILED_TRUST_ME_INSTINIT \
      CACHE_Y(B);
	  
#ifdef YAPOR
#ifdef FROZEN_STACKS
#define PROFILED_TRUST_ME_IF \
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
      }
#else /* FROZEN_STACKS */
#define PROFILED_TRUST_ME_IF \
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
      }
#endif /* FROZEN_STACKS */
#else /* YAPOR */
#ifdef FROZEN_STACKS
#define PROFILED_TRUST_ME_IF \
      { \
	pop_yaam_regs(); \
	pop_args((*_PREG)->u.Otapl.s); \
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	set_cut(S_YREG, B); \
      }
#else /* FROZEN_STACKS */
#define PROFILED_TRUST_ME_IF \
      { \
	pop_yaam_regs(); \
	pop_args((*_PREG)->u.Otapl.s); \
	set_cut(S_YREG, B); \
      }
#endif /* FROZEN_STACKS */
#endif /* YAPOR */

#define PROFILED_TRUST_ME_END \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      LOCK((*_PREG)->u.Otapl.p->StatisticsForPred->lock); \
      (*_PREG)->u.Otapl.p->StatisticsForPred->NOfRetries++; \
      UNLOCK((*_PREG)->u.Otapl.p->StatisticsForPred->lock); \
      (*_PREG) = NEXTOP((*_PREG), Otapl); \
      GONext();

#define PROFILED_RETRY_LOGICAL_INSTINIT \
      check_trail(TR); \
      { \
	UInt timestamp; \
	CACHE_Y(B); \
	timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[(*_PREG)->u.OtaLl.s]); \
	if (!VALID_TIMESTAMP(timestamp, (*_PREG)->u.OtaLl.d)) { \
	  (*_PREG)=(*_PREG)->u.OtaLl.n; \
	  JMPNext(); \
	} \
	else { \
	  restore_yaam_regs((*_PREG)->u.OtaLl.n); \
	  restore_args((*_PREG)->u.OtaLl.s); \
	  LOCK((*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->lock); \
	  (*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->NOfRetries++; \
	  UNLOCK((*_PREG)->u.OtaLl.d->ClPred->StatisticsForPred->lock); \
	}
	
#ifdef THREADS
#define PROFILED_RETRY_LOGICAL_THREADS \
	PP = (*_PREG)->u.OtaLl.d->ClPred;
#endif

#define PROFILED_RETRY_LOGICAL_POST_THREADS \
	(*_PREG) = (*_PREG)->u.OtaLl.d->ClCode;
	
#ifdef FROZEN_STACKS
#define PROFILED_RETRY_LOGICAL_FROZEN \
	S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
	set_cut(S_YREG, B->cp_b);
#else
#define PROFILED_RETRY_LOGICAL_NOFROZEN \
	set_cut(S_YREG, B_YREG->cp_b);
#endif

#define PROFILED_RETRY_LOGICAL_END \
      BLOCKADDRESS = (CELL)(*_PREG); \
      BLOCK = (CELL)PROFILED_RETRY_LOGICAL_END; \
	SET_BB(B_YREG); \
	ENDCACHE_Y(); \
      } \
      JMPNext();

#if MULTIPLE_STACKS
#ifdef YAPOR
#ifdef FROZEN_STACKS
#define PROFILED_TRUST_LOGICAL_INSTINIT \
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
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	PELOCK(1, ap); \
	PP = ap; \
	DEC_CLREF_COUNT(cl); \
	B->cp_tr--; \
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
#define PROFILED_TRUST_LOGICAL_INSTINIT \
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
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	PELOCK(1, ap); \
	PP = ap; \
	DEC_CLREF_COUNT(cl); \
	B->cp_tr--; \
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
#define PROFILED_TRUST_LOGICAL_INSTINIT \
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
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	PELOCK(1, ap); \
	PP = ap; \
	DEC_CLREF_COUNT(cl); \
	B->cp_tr--; \
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
#define PROFILED_TRUST_LOGICAL_INSTINIT \
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
	  LOCK(ap->StatisticsForPred->lock); \
	  ap->StatisticsForPred->NOfRetries++; \
	  UNLOCK(ap->StatisticsForPred->lock); \
	  (*_PREG) = lcl->ClCode; \
	} \
	PELOCK(1, ap); \
	PP = ap; \
	DEC_CLREF_COUNT(cl); \
	B->cp_tr--; \
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
#define PROFILED_TRUST_LOGICAL_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      CACHE_Y(B); \
      { \
	LogUpdIndex *cl = (*_PREG)->u.OtILl.block; \
	PredEntry *ap = cl->ClPred; \
	LogUpdClause *lcl = (*_PREG)->u.OtILl.d; \
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]); \
 \
	if (!VALID_TIMESTAMP(timestamp, lcl)) { \
	  (*_PREG) = FAILCODE; \
	} else { \
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
	    } \
	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
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
#define PROFILED_TRUST_LOGICAL_INSTINIT \
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
	    } \
	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
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
#define PROFILED_TRUST_LOGICAL_INSTINIT \
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
	    } \
	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
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
#define PROFILED_TRUST_LOGICAL_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      CACHE_Y(B); \
      { \
	LogUpdIndex *cl = (*_PREG)->u.OtILl.block; \
	PredEntry *ap = cl->ClPred; \
	LogUpdClause *lcl = (*_PREG)->u.OtILl.d; \
	UInt timestamp = IntegerOfTerm(((CELL *)(B_YREG+1))[ap->ArityOfPE]); \
 \
	if (!VALID_TIMESTAMP(timestamp, lcl)) { \
	  (*_PREG) = FAILCODE; \
	} else { \
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
	    } \
	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
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
#define PROFILED_TRUST_LOGICAL_INSTINIT \
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
	    } \
	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
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
#define PROFILED_TRUST_LOGICAL_INSTINIT \
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
	    } \
	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
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
#define PROFILED_TRUST_LOGICAL_INSTINIT \
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
	    } \
	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
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
#define PROFILED_TRUST_LOGICAL_INSTINIT \
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
	    } \
	  if (cl->ClFlags & (ErasedMask|DirtyMask)) { \
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
	
#define PROFILED_TRUST_LOGICAL_END \
      BLOCK = (CELL)PROFILED_TRUST_LOGICAL_END;

#define TRY_CLAUSE_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      check_trail(TR); \
      CACHE_Y(YREG); \
      store_at_least_one_arg((*_PREG)->u.Otapl.s); \
      store_yaam_regs(NEXTOP((*_PREG), Otapl), 0); \
      (*_PREG) = (*_PREG)->u.Otapl.d; \
      set_cut(S_YREG, B); \
      B = B_YREG;
	  
#ifdef YAPOR
#define TRY_CLAUSE_YAPOR \
      SCH_set_load(B_YREG);
#endif

#define TRY_CLAUSE_END \
      BLOCK = (CELL)TRY_CLAUSE_END; \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      JMPNext();

#define TRY_CLAUSE2_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      check_trail(TR); \
      CACHE_Y(YREG); \
      { \
	register CELL x2 = ARG2; \
	register CELL x1 = ARG1; \
	store_yaam_regs(NEXTOP((*_PREG), l), 2); \
	B_YREG->cp_a1 = x1; \
	B_YREG->cp_a2 = x2; \
      } \
      (*_PREG) = (*_PREG)->u.l.l; \
      set_cut(S_YREG, B); \
      B = B_YREG;

	  
#ifdef YAPOR
#define TRY_CLAUSE2_YAPOR \
      SCH_set_load(B_YREG);
#endif

#define TRY_CLAUSE2_END \
      BLOCK = (CELL)TRY_CLAUSE2_END; \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      JMPNext();

#define TRY_CLAUSE3_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      check_trail(TR); \
      CACHE_Y(YREG); \
      { \
	store_yaam_regs(NEXTOP((*_PREG), l), 3); \
	B_YREG->cp_a1 = ARG1; \
	B_YREG->cp_a2 = ARG2; \
	B_YREG->cp_a3 = ARG3; \
      } \
      (*_PREG) = (*_PREG)->u.l.l; \
      set_cut(S_YREG, B); \
      B = B_YREG;

#ifdef YAPOR
#define TRY_CLAUSE3_YAPOR \
      SCH_set_load(B_YREG);
#endif

#define TRY_CLAUSE3_END \
      BLOCK = (CELL)TRY_CLAUSE3_END; \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      JMPNext();

#define TRY_CLAUSE4_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      check_trail(TR); \
      CACHE_Y(YREG); \
      { \
	store_yaam_regs(NEXTOP((*_PREG), l), 4); \
	B_YREG->cp_a1 = ARG1; \
	B_YREG->cp_a2 = ARG2; \
	B_YREG->cp_a3 = ARG3; \
	B_YREG->cp_a4 = ARG4; \
      } \
      (*_PREG) = (*_PREG)->u.l.l; \
      set_cut(S_YREG, B); \
      B = B_YREG;
	  
#ifdef YAPOR
#define TRY_CLAUSE4_YAPOR \
      SCH_set_load(B_YREG);
#endif

#define TRY_CLAUSE4_END \
      BLOCK = (CELL)TRY_CLAUSE4_END; \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      JMPNext();

#define RETRY_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      CACHE_Y(B); \
      restore_yaam_regs(NEXTOP((*_PREG), Otapl)); \
      restore_at_least_one_arg((*_PREG)->u.Otapl.s);
	  
#ifdef FROZEN_STACKS
#define RETRY_FROZEN \
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
      set_cut(S_YREG, B->cp_b);
#else
#define RETRY_NOFROZEN \
      set_cut(S_YREG, B_YREG->cp_b);
#endif

#define RETRY_END \
      BLOCK = (CELL)RETRY_END; \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      (*_PREG) = (*_PREG)->u.Otapl.d; \
      JMPNext();

#define RETRY2_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      CACHE_Y(B); \
      restore_yaam_regs(NEXTOP((*_PREG), l)); \
      (*_PREG) = (*_PREG)->u.l.l; \
      ARG1 = B_YREG->cp_a1; \
      ARG2 = B_YREG->cp_a2;
	  
#ifdef FROZEN_STACKS
#define RETRY2_FROZEN \
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
      set_cut(S_YREG, B->cp_b);
#else
#define RETRY2_NOFROZEN \
      set_cut(S_YREG, B_YREG->cp_b);
#endif

#define RETRY2_END \
      BLOCK = (CELL)RETRY2_END; \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      JMPNext();

#define RETRY3_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      CACHE_Y(B); \
      restore_yaam_regs(NEXTOP((*_PREG), l)); \
      (*_PREG) = (*_PREG)->u.l.l; \
      ARG1 = B_YREG->cp_a1; \
      ARG2 = B_YREG->cp_a2; \
      ARG3 = B_YREG->cp_a3;
	  
#ifdef FROZEN_STACKS
#define RETRY3_FROZEN \
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
      set_cut(S_YREG, B->cp_b);
#else
#define RETRY3_NOFROZEN \
      set_cut(S_YREG, B_YREG->cp_b);
#endif

#define RETRY3_END \
      BLOCK = (CELL)RETRY3_END; \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      JMPNext();

#define RETRY4_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      CACHE_Y(B); \
      restore_yaam_regs(NEXTOP((*_PREG), l)); \
      (*_PREG) = (*_PREG)->u.l.l; \
      ARG1 = B_YREG->cp_a1; \
      ARG2 = B_YREG->cp_a2; \
      ARG3 = B_YREG->cp_a3; \
      ARG4 = B_YREG->cp_a4;
	  
#ifdef FROZEN_STACKS
#define RETRY4_FROZEN \
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
      set_cut(S_YREG, B->cp_b);
#else
#define RETRY4_NOFROZEN \
      set_cut(S_YREG, B_YREG->cp_b);
#endif

#define RETRY4_END \
      BLOCK = (CELL)RETRY4_END; \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      JMPNext();

#define TRUST_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      CACHE_Y(B);
	  
#ifdef YAPOR
#define TRUST_IFOK_INIT \
      if (SCH_top_shared_cp(B)) { \
	SCH_last_alternative((*_PREG), B_YREG); \
	restore_at_least_one_arg((*_PREG)->u.Otapl.s);
#ifdef FROZEN_STACKS
#define TRUST_IFOK_FROZEN \
        S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif
#define TRUST_IFOK_END \
	set_cut(S_YREG, B->cp_b); \
      } \
      else
#endif

#define TRUST_NOIF_INIT \
      { \
	pop_yaam_regs(); \
	pop_at_least_one_arg((*_PREG)->u.Otapl.s);
	
#ifdef FROZEN_STACKS
#define TRUST_NOIF_FROZEN \
        S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG);
#endif

#define TRUST_END \
      BLOCK = (CELL)TRUST_END; \
	set_cut(S_YREG, B); \
      } \
      SET_BB(B_YREG); \
      ENDCACHE_Y(); \
      (*_PREG) = (*_PREG)->u.Otapl.d; \
      JMPNext();

#define TRY_IN_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      B->cp_ap = NEXTOP((*_PREG), l); \
      (*_PREG) = (*_PREG)->u.l.l; \
      JMPNext();
	  
#define TRY_IN_END \
      BLOCK = (CELL)TRY_IN_END;

#define SPY_OR_TRYMARK_INSTINIT \
      PELOCK(5, ((PredEntry *)((*_PREG)->u.Otapl.p))); \
      (*_PREG) = (yamop *)(&(((PredEntry *)((*_PREG)->u.Otapl.p))->OpcodeOfPred)); \
      UNLOCKPE(11,(PredEntry *)((*_PREG)->u.Otapl.p)); \
	  return external_labels[12];

#define TRY_AND_MARK_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      check_trail(TR);

#if defined(YAPOR) || defined(THREADS)
#ifdef YAPOR
#define TRY_AND_MARK_YAPOR_THREADS_YAPOR \
      CUT_wait_leftmost();
#endif

#define TRY_AND_MARK_YAPOR_THREADS_NOYAPOR_IF \
    BLOCK = (CELL)TRY_AND_MARK_YAPOR_THREADS_NOYAPOR_IF; \
      if ((*_PREG)->u.Otapl.p->PredFlags & LogUpdatePredFlag) { \
	PELOCK(6,(*_PREG)->u.Otapl.p); \
	PP = (*_PREG)->u.Otapl.p; \
      } \
      if ((*_PREG)->u.Otapl.p->CodeOfPred != (*_PREG)) { \
	PP = NULL; \
	(*_PREG) = (*_PREG)->u.Otapl.p->CodeOfPred; \
	UNLOCKPE(12,(*_PREG)->u.Otapl.p); \
	save_pc(); \
	JMPNext(); \
      }
#endif

#define TRY_AND_MARK_NOYAPOR_NOTHREADS \
      CACHE_Y(YREG); \
      (*_PREG) = (*_PREG)->u.Otapl.d; \
      LOCK(DynamicLock((*_PREG))); \
      UNLOCKPE(13,((PredEntry *)((*_PREG)->u.Otapl.p))); \
      BEGD(d1); \
      d1 = (*_PREG)->u.Otapl.s; \
      store_args(d1); \
      store_yaam_regs((*_PREG), 0); \
      ENDD(d1); \
      set_cut(S_YREG, B); \
      B = B_YREG;
	  
#ifdef YAPOR
#define TRY_AND_MARK_SET_LOAD \
      SCH_set_load(B_YREG);
#endif

#define TRY_AND_MARK_POST_SET_LOAD \
      SET_BB(B_YREG); \
      ENDCACHE_Y();
	  
#if MULTIPLE_STACKS
#define TRY_AND_MARK_MULTIPLE_STACKS \
      INC_CLREF_COUNT(ClauseCodeToDynamicClause((*_PREG))); \
      UNLOCK(DynamicLock((*_PREG))); \
      TRAIL_CLREF(ClauseCodeToDynamicClause((*_PREG)));
#else
#define TRY_AND_MARK_NOMULTIPLE_STACKS_IF \
      if (FlagOff(InUseMask, DynamicFlags((*_PREG)))) { \
	SetFlag(InUseMask, DynamicFlags((*_PREG))); \
	TRAIL_CLREF(ClauseCodeToDynamicClause((*_PREG))); \
      }
#endif

#define TRY_AND_MARK_END \
      (*_PREG) = NEXTOP((*_PREG),Otapl); \
      JMPNext();

#define COUNT_RETRY_AND_MARK_INSTINIT \
    LOCAL_RetriesCounter--; \
    if (LOCAL_RetriesCounter == 0) { \
	  saveregs(); \
	  Yap_NilError(RETRY_COUNTER_UNDERFLOW,""); \
	  setregs(); \
	  JMPNext(); \
    } \
	else { \
      LOCAL_PredEntriesCounter--; \
      if (LOCAL_PredEntriesCounter == 0) { \
	    saveregs(); \
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	    setregs(); \
	    JMPNext(); \
      } \
	}

#define PROFILED_RETRY_AND_MARK_INSTINIT \
      LOCK(((PredEntry *)((*_PREG)->u.Otapl.p))->StatisticsForPred->lock); \
      ((PredEntry *)((*_PREG)->u.Otapl.p))->StatisticsForPred->NOfRetries++; \
      UNLOCK(((PredEntry *)((*_PREG)->u.Otapl.p))->StatisticsForPred->lock); \

#define RETRY_AND_MARK_INSTINIT
	  
#ifdef YAPOR
#define RETRY_AND_MARK_YAPOR \
      CUT_wait_leftmost();
#endif

#define RETRY_AND_MARK_POST_YAPOR \
      PELOCK(7,(*_PREG)->u.Otapl.p); \
      CACHE_Y(B); \
      (*_PREG) = (*_PREG)->u.Otapl.d; \
      LOCK(DynamicLock((*_PREG))); \
      UNLOCK((*_PREG)->u.Otapl.p->PELock); \
      restore_yaam_regs((*_PREG)); \
      restore_args((*_PREG)->u.Otapl.s);
	  
#ifdef FROZEN_STACKS
#define RETRY_AND_MARK_FROZEN \
      S_YREG = (CELL *) PROTECT_FROZEN_B(B_YREG); \
      set_cut(S_YREG, B->cp_b);
#else
#define RETRY_AND_MARK_NOFROZEN \
      set_cut(S_YREG, B_YREG->cp_b);
#endif

#define RETRY_AND_MARK_POST_FROZEN \
      SET_BB(B_YREG); \
      ENDCACHE_Y();
	  
#if MULTIPLE_STACKS
#define RETRY_AND_MARK_MULTIPLE_STACKS \
      INC_CLREF_COUNT(ClauseCodeToDynamicClause((*_PREG))); \
      TRAIL_CLREF(ClauseCodeToDynamicClause((*_PREG))); \
      UNLOCK(DynamicLock((*_PREG)));
#else
#define RETRY_AND_MARK_NOMULTIPLE_STACKS_IF \
      if (FlagOff(InUseMask, DynamicFlags((*_PREG)))) { \
	SetFlag(InUseMask, DynamicFlags((*_PREG))); \
	TRAIL_CLREF(ClauseCodeToDynamicClause((*_PREG))); \
      }
#endif

#define RETRY_AND_MARK_END \
      (*_PREG) = NEXTOP((*_PREG), Otapl); \
      JMPNext();

