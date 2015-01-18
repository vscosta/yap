#ifdef SHADOW_S
#ifdef LOW_LEVEL_TRACER
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _call_cpred_instinit \
      check_trail(TR); \
      if (!((*_PREG)->u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) { \
	CACHE_Y_AS_ENV(YREG); \
	check_stack_on_call; \
	ENDCACHE_Y_AS_ENV(); \
      } \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
 \
	if (YREG > (CELL *) top_b || YREG < HR) \
	    ASP = (CELL *)top_b; \
	else \
	    ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s); \
      } \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      BEGD(d0); \
      CPredicate f = (*_PREG)->u.Osbpp.p->cs.f_code; \
      (*_PREG) = NEXTOP((*_PREG), Osbpp); \
      saveregs(); \
      d0 = (f)(PASS_REGS1); \
      setregs(); \
      (*_SREG) = Yap_REGS.S_; \
      if (!d0) { \
        FAIL(); \
      } \
      CACHE_A1(); \
      ENDD(d0); \
      GONEXT();
#else /* YAPOR_SBA */
#define _call_cpred_instinit \
      check_trail(TR); \
      if (!((*_PREG)->u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) { \
	CACHE_Y_AS_ENV(YREG); \
	check_stack_on_call; \
	ENDCACHE_Y_AS_ENV(); \
      } \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
 \
	if (YREG > (CELL *) top_b) \
	    ASP = (CELL *)top_b; \
	else \
	    ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s); \
      } \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      BEGD(d0); \
      CPredicate f = (*_PREG)->u.Osbpp.p->cs.f_code; \
      (*_PREG) = NEXTOP((*_PREG), Osbpp); \
      saveregs(); \
      d0 = (f)(PASS_REGS1); \
      setregs(); \
      (*_SREG) = Yap_REGS.S_; \
      if (!d0) { \
        FAIL(); \
      } \
      CACHE_A1(); \
      ENDD(d0); \
      GONEXT();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _call_cpred_instinit \
      check_trail(TR); \
      if (!((*_PREG)->u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) { \
	CACHE_Y_AS_ENV(YREG); \
	check_stack_on_call; \
	ENDCACHE_Y_AS_ENV(); \
      } \
      SET_ASP(YREG, (*_PREG)->u.Osbpp.s); \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      BEGD(d0); \
      CPredicate f = (*_PREG)->u.Osbpp.p->cs.f_code; \
      (*_PREG) = NEXTOP((*_PREG), Osbpp); \
      saveregs(); \
      d0 = (f)(PASS_REGS1); \
      setregs(); \
      (*_SREG) = Yap_REGS.S_; \
      if (!d0) { \
        FAIL(); \
      } \
      CACHE_A1(); \
      ENDD(d0); \
      GONEXT();
#endif /* FROZEN_STACKS */
#else /* LOW_LEVEL_TRACER */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _call_cpred_instinit \
      check_trail(TR); \
      if (!((*_PREG)->u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) { \
	CACHE_Y_AS_ENV(YREG); \
	check_stack_on_call; \
	ENDCACHE_Y_AS_ENV(); \
      } \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
 \
	if (YREG > (CELL *) top_b || YREG < HR) \
	    ASP = (CELL *)top_b; \
	else \
	    ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s); \
      } \
      BEGD(d0); \
      CPredicate f = (*_PREG)->u.Osbpp.p->cs.f_code; \
      (*_PREG) = NEXTOP((*_PREG), Osbpp); \
      saveregs(); \
      d0 = (f)(PASS_REGS1); \
      setregs(); \
      (*_SREG) = Yap_REGS.S_; \
      if (!d0) { \
        FAIL(); \
      } \
      CACHE_A1(); \
      ENDD(d0); \
      GONEXT();
#else /* YAPOR_SBA */
#define _call_cpred_instinit \
      check_trail(TR); \
      if (!((*_PREG)->u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) { \
	CACHE_Y_AS_ENV(YREG); \
	check_stack_on_call; \
	ENDCACHE_Y_AS_ENV(); \
      } \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
 \
	if (YREG > (CELL *) top_b) \
	    ASP = (CELL *)top_b; \
	else \
	    ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s); \
      } \
      BEGD(d0); \
      CPredicate f = (*_PREG)->u.Osbpp.p->cs.f_code; \
      (*_PREG) = NEXTOP((*_PREG), Osbpp); \
      saveregs(); \
      d0 = (f)(PASS_REGS1); \
      setregs(); \
      (*_SREG) = Yap_REGS.S_; \
      if (!d0) { \
        FAIL(); \
      } \
      CACHE_A1(); \
      ENDD(d0); \
      GONEXT();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _call_cpred_instinit \
      check_trail(TR); \
      if (!((*_PREG)->u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) { \
	CACHE_Y_AS_ENV(YREG); \
	check_stack_on_call; \
	ENDCACHE_Y_AS_ENV(); \
      } \
      SET_ASP(YREG, (*_PREG)->u.Osbpp.s); \
      BEGD(d0); \
      CPredicate f = (*_PREG)->u.Osbpp.p->cs.f_code; \
      (*_PREG) = NEXTOP((*_PREG), Osbpp); \
      saveregs(); \
      d0 = (f)(PASS_REGS1); \
      setregs(); \
      (*_SREG) = Yap_REGS.S_; \
      if (!d0) { \
        FAIL(); \
      } \
      CACHE_A1(); \
      ENDD(d0); \
      GONEXT();
#endif /* FROZEN_STACKS */
#endif /* LOW_LEVEL_TRACER */
#else /* SHADOW_S */
#ifdef LOW_LEVEL_TRACER
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _call_cpred_instinit \
      check_trail(TR); \
      if (!((*_PREG)->u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) { \
	CACHE_Y_AS_ENV(YREG); \
	check_stack_on_call; \
	ENDCACHE_Y_AS_ENV(); \
      } \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
 \
	if (YREG > (CELL *) top_b || YREG < HR) \
	    ASP = (CELL *)top_b; \
	else \
	    ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s); \
      } \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      BEGD(d0); \
      CPredicate f = (*_PREG)->u.Osbpp.p->cs.f_code; \
      (*_PREG) = NEXTOP((*_PREG), Osbpp); \
      saveregs(); \
      d0 = (f)(PASS_REGS1); \
      setregs(); \
      if (!d0) { \
        FAIL(); \
      } \
      CACHE_A1(); \
      ENDD(d0); \
      GONEXT();
#else /* YAPOR_SBA */
#define _call_cpred_instinit \
      check_trail(TR); \
      if (!((*_PREG)->u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) { \
	CACHE_Y_AS_ENV(YREG); \
	check_stack_on_call; \
	ENDCACHE_Y_AS_ENV(); \
      } \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
 \
	if (YREG > (CELL *) top_b) \
	    ASP = (CELL *)top_b; \
	else \
	    ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s); \
      } \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      BEGD(d0); \
      CPredicate f = (*_PREG)->u.Osbpp.p->cs.f_code; \
      (*_PREG) = NEXTOP((*_PREG), Osbpp); \
      saveregs(); \
      d0 = (f)(PASS_REGS1); \
      setregs(); \
      if (!d0) { \
        FAIL(); \
      } \
      CACHE_A1(); \
      ENDD(d0); \
      GONEXT();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _call_cpred_instinit \
      check_trail(TR); \
      if (!((*_PREG)->u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) { \
	CACHE_Y_AS_ENV(YREG); \
	check_stack_on_call; \
	ENDCACHE_Y_AS_ENV(); \
      } \
      SET_ASP(YREG, (*_PREG)->u.Osbpp.s); \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      BEGD(d0); \
      CPredicate f = (*_PREG)->u.Osbpp.p->cs.f_code; \
      (*_PREG) = NEXTOP((*_PREG), Osbpp); \
      saveregs(); \
      d0 = (f)(PASS_REGS1); \
      setregs(); \
      if (!d0) { \
        FAIL(); \
      } \
      CACHE_A1(); \
      ENDD(d0); \
      GONEXT();
#endif /* FROZEN_STACKS */
#else /* LOW_LEVEL_TRACER */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _call_cpred_instinit \
      check_trail(TR); \
      if (!((*_PREG)->u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) { \
	CACHE_Y_AS_ENV(YREG); \
	check_stack_on_call; \
	ENDCACHE_Y_AS_ENV(); \
      } \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
 \
	if (YREG > (CELL *) top_b || YREG < HR) \
	    ASP = (CELL *)top_b; \
	else \
	    ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s); \
      } \
      BEGD(d0); \
      CPredicate f = (*_PREG)->u.Osbpp.p->cs.f_code; \
      (*_PREG) = NEXTOP((*_PREG), Osbpp); \
      saveregs(); \
      d0 = (f)(PASS_REGS1); \
      setregs(); \
      if (!d0) { \
        FAIL(); \
      } \
      CACHE_A1(); \
      ENDD(d0); \
      GONEXT();
#else /* YAPOR_SBA */
#define _call_cpred_instinit \
      check_trail(TR); \
      if (!((*_PREG)->u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) { \
	CACHE_Y_AS_ENV(YREG); \
	check_stack_on_call; \
	ENDCACHE_Y_AS_ENV(); \
      } \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
 \
	if (YREG > (CELL *) top_b) \
	    ASP = (CELL *)top_b; \
	else \
	    ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s); \
      } \
      BEGD(d0); \
      CPredicate f = (*_PREG)->u.Osbpp.p->cs.f_code; \
      (*_PREG) = NEXTOP((*_PREG), Osbpp); \
      saveregs(); \
      d0 = (f)(PASS_REGS1); \
      setregs(); \
      if (!d0) { \
        FAIL(); \
      } \
      CACHE_A1(); \
      ENDD(d0); \
      GONEXT();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _call_cpred_instinit \
      check_trail(TR); \
      if (!((*_PREG)->u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) { \
	CACHE_Y_AS_ENV(YREG); \
	check_stack_on_call; \
	ENDCACHE_Y_AS_ENV(); \
      } \
      SET_ASP(YREG, (*_PREG)->u.Osbpp.s); \
      BEGD(d0); \
      CPredicate f = (*_PREG)->u.Osbpp.p->cs.f_code; \
      (*_PREG) = NEXTOP((*_PREG), Osbpp); \
      saveregs(); \
      d0 = (f)(PASS_REGS1); \
      setregs(); \
      if (!d0) { \
        FAIL(); \
      } \
      CACHE_A1(); \
      ENDD(d0); \
      GONEXT();
#endif /* FROZEN_STACKS */
#endif /* LOW_LEVEL_TRACER */
#endif /* SHADOW_S */

#ifdef LOW_LEVEL_TRACER
#ifdef SHADOW_S
#ifdef NO_CHECKING
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#else /* NO_CHECKING */
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#endif /* NO_CHECKING */
#else /* SHADOW_S */
#ifdef NO_CHECKING
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#else /* NO_CHECKING */
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#endif /* NO_CHECKING */
#endif /* SHADOW_S */
#else /* LOW_LEVEL_TRACER */
#ifdef SHADOW_S
#ifdef NO_CHECKING
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#else /* NO_CHECKING */
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#endif /* NO_CHECKING */
#else /* SHADOW_S */
#ifdef NO_CHECKING
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#else /* NO_CHECKING */
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      FAIL(); \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt0->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    DEPTH = ENV_YREG[E_DEPTH]; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#else /* YAPOR_SBA */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  } \
	} \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _execute_cpred_instinit \
      check_trail(TR); \
      { \
	PredEntry *pt0; \
	BEGD(d0); \
	CACHE_Y_AS_ENV(YREG); \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	{ \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    FAIL(); \
	  } \
	  if (oldPREG == (*_PREG)) { \
	    (*_PREG) = (*_CPREG); \
	    ENV_YREG = ENV; \
	    WRITEBACK_Y_AS_ENV(); \
	  } else { \
	    CACHE_A1(); \
	  } \
	} \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
	ENDD(d0); \
      }
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#endif /* NO_CHECKING */
#endif /* SHADOW_S */
#endif /* LOW_LEVEL_TRACER */

#ifdef LOW_LEVEL_TRACER
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _call_usercpred_instinit \
      CACHE_Y_AS_ENV(YREG); \
      check_stack_on_call; \
      ENDCACHE_Y_AS_ENV(); \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
	} \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (YREG > (CELL *) top_b || YREG < HR) \
	  ASP = (CELL *) top_b; \
	else \
      ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s); \
      } \
      { \
	yamop *savedP; \
	Yap_StartSlots( PASS_REGS1 ); \
	LOCAL_PrologMode = UserCCallMode; \
	{ \
	  PredEntry *p = (*_PREG)->u.Osbpp.p; \
	  (*_PREG) = NEXTOP((*_PREG), Osbpp); \
	  savedP = (*_PREG); \
	  saveregs(); \
	  save_machine_regs(); \
	  (*_SREG) = (CELL *) YAP_Execute(p, p->cs.f_code); \
	} \
	Yap_CloseSlots( PASS_REGS1 ); \
	setregs(); \
	LOCAL_PrologMode = UserMode; \
	restore_machine_regs(); \
	(*_PREG) = savedP; \
      } \
      if (EX) { \
	struct DB_TERM *exp = EX; \
	EX = NULL; \
	Yap_JumpToEnv(Yap_PopTermFromDB(exp)); \
      } \
      if (!(*_SREG)) { \
	FAIL(); \
      } \
      YENV = ENV; \
      YREG = ENV; \
      GONEXT();
#else /* YAPOR_SBA */
#define _call_usercpred_instinit \
      CACHE_Y_AS_ENV(YREG); \
      check_stack_on_call; \
      ENDCACHE_Y_AS_ENV(); \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
	} \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (YREG > (CELL *) top_b) \
      ASP = (CELL *) top_b; \
	else \
      ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s); \
      } \
      { \
	yamop *savedP; \
	Yap_StartSlots( PASS_REGS1 ); \
	LOCAL_PrologMode = UserCCallMode; \
	{ \
	  PredEntry *p = (*_PREG)->u.Osbpp.p; \
	  (*_PREG) = NEXTOP((*_PREG), Osbpp); \
	  savedP = (*_PREG); \
	  saveregs(); \
	  save_machine_regs(); \
	  (*_SREG) = (CELL *) YAP_Execute(p, p->cs.f_code); \
	} \
	Yap_CloseSlots( PASS_REGS1 ); \
	setregs(); \
	LOCAL_PrologMode = UserMode; \
	restore_machine_regs(); \
	(*_PREG) = savedP; \
      } \
      if (EX) { \
	struct DB_TERM *exp = EX; \
	EX = NULL; \
	Yap_JumpToEnv(Yap_PopTermFromDB(exp)); \
      } \
      if (!(*_SREG)) { \
	FAIL(); \
      } \
      YENV = ENV; \
      YREG = ENV; \
      GONEXT();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _call_usercpred_instinit \
      CACHE_Y_AS_ENV(YREG); \
      check_stack_on_call; \
      ENDCACHE_Y_AS_ENV(); \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
	} \
      SET_ASP(YREG, (*_PREG)->u.Osbpp.s); \
      { \
	yamop *savedP; \
	Yap_StartSlots( PASS_REGS1 ); \
	LOCAL_PrologMode = UserCCallMode; \
	{ \
	  PredEntry *p = (*_PREG)->u.Osbpp.p; \
	  (*_PREG) = NEXTOP((*_PREG), Osbpp); \
	  savedP = (*_PREG); \
	  saveregs(); \
	  save_machine_regs(); \
	  (*_SREG) = (CELL *) YAP_Execute(p, p->cs.f_code); \
	} \
	Yap_CloseSlots( PASS_REGS1 ); \
	setregs(); \
	LOCAL_PrologMode = UserMode; \
	restore_machine_regs(); \
	(*_PREG) = savedP; \
      } \
      if (EX) { \
	struct DB_TERM *exp = EX; \
	EX = NULL; \
	Yap_JumpToEnv(Yap_PopTermFromDB(exp)); \
      } \
      if (!(*_SREG)) { \
	FAIL(); \
      } \
      YENV = ENV; \
      YREG = ENV; \
      GONEXT();
#endif /* FROZEN_STACKS */
#else /* LOW_LEVEL_TRACER */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _call_usercpred_instinit \
      CACHE_Y_AS_ENV(YREG); \
      check_stack_on_call; \
      ENDCACHE_Y_AS_ENV(); \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (YREG > (CELL *) top_b || YREG < HR) \
	  ASP = (CELL *) top_b; \
	else \
      ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s); \
      } \
      { \
	yamop *savedP; \
	Yap_StartSlots( PASS_REGS1 ); \
	LOCAL_PrologMode = UserCCallMode; \
	{ \
	  PredEntry *p = (*_PREG)->u.Osbpp.p; \
	  (*_PREG) = NEXTOP((*_PREG), Osbpp); \
	  savedP = (*_PREG); \
	  saveregs(); \
	  save_machine_regs(); \
	  (*_SREG) = (CELL *) YAP_Execute(p, p->cs.f_code); \
	} \
	Yap_CloseSlots( PASS_REGS1 ); \
	setregs(); \
	LOCAL_PrologMode = UserMode; \
	restore_machine_regs(); \
	(*_PREG) = savedP; \
      } \
      if (EX) { \
	struct DB_TERM *exp = EX; \
	EX = NULL; \
	Yap_JumpToEnv(Yap_PopTermFromDB(exp)); \
      } \
      if (!(*_SREG)) { \
	FAIL(); \
      } \
      YENV = ENV; \
      YREG = ENV; \
      GONEXT();
#else /* YAPOR_SBA */
#define _call_usercpred_instinit \
      CACHE_Y_AS_ENV(YREG); \
      check_stack_on_call; \
      ENDCACHE_Y_AS_ENV(); \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (YREG > (CELL *) top_b) \
      ASP = (CELL *) top_b; \
	else \
      ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s); \
      } \
      { \
	yamop *savedP; \
	Yap_StartSlots( PASS_REGS1 ); \
	LOCAL_PrologMode = UserCCallMode; \
	{ \
	  PredEntry *p = (*_PREG)->u.Osbpp.p; \
	  (*_PREG) = NEXTOP((*_PREG), Osbpp); \
	  savedP = (*_PREG); \
	  saveregs(); \
	  save_machine_regs(); \
	  (*_SREG) = (CELL *) YAP_Execute(p, p->cs.f_code); \
	} \
	Yap_CloseSlots( PASS_REGS1 ); \
	setregs(); \
	LOCAL_PrologMode = UserMode; \
	restore_machine_regs(); \
	(*_PREG) = savedP; \
      } \
      if (EX) { \
	struct DB_TERM *exp = EX; \
	EX = NULL; \
	Yap_JumpToEnv(Yap_PopTermFromDB(exp)); \
      } \
      if (!(*_SREG)) { \
	FAIL(); \
      } \
      YENV = ENV; \
      YREG = ENV; \
      GONEXT();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _call_usercpred_instinit \
      CACHE_Y_AS_ENV(YREG); \
      check_stack_on_call; \
      ENDCACHE_Y_AS_ENV(); \
      SET_ASP(YREG, (*_PREG)->u.Osbpp.s); \
      { \
	yamop *savedP; \
	Yap_StartSlots( PASS_REGS1 ); \
	LOCAL_PrologMode = UserCCallMode; \
	{ \
	  PredEntry *p = (*_PREG)->u.Osbpp.p; \
	  (*_PREG) = NEXTOP((*_PREG), Osbpp); \
	  savedP = (*_PREG); \
	  saveregs(); \
	  save_machine_regs(); \
	  (*_SREG) = (CELL *) YAP_Execute(p, p->cs.f_code); \
	} \
	Yap_CloseSlots( PASS_REGS1 ); \
	setregs(); \
	LOCAL_PrologMode = UserMode; \
	restore_machine_regs(); \
	(*_PREG) = savedP; \
      } \
      if (EX) { \
	struct DB_TERM *exp = EX; \
	EX = NULL; \
	Yap_JumpToEnv(Yap_PopTermFromDB(exp)); \
      } \
      if (!(*_SREG)) { \
	FAIL(); \
      } \
      YENV = ENV; \
      YREG = ENV; \
      GONEXT();
#endif /* FROZEN_STACKS */
#endif /* LOW_LEVEL_TRACER */
