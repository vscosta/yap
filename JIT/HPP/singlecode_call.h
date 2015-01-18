#if (defined(YAPOR_SBA) && defined(YAPOR)) || defined(TABLING)
#define check_stack_on_call \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) return external_labels[6];
#else
#define check_stack_on_call \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) return external_labels[6];
#endif /* YAPOR_SBA && YAPOR */

#if (defined(YAPOR_SBA) && defined(YAPOR)) || defined(TABLING)
#define check_stack_on_execute \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) return external_labels[5];
#else
#define check_stack_on_execute \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) return external_labels[5];
#endif /* YAPOR_SBA && YAPOR */

#if (defined(YAPOR_SBA) && defined(YAPOR)) || defined(TABLING)
#define check_stack_on_dexecute \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) return external_labels[7];
#else
#define check_stack_on_dexecute \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) return external_labels[7];
#endif /* YAPOR_SBA && YAPOR */

#if (defined(YAPOR_SBA) && defined(YAPOR)) || defined(TABLING)
#define check_stack_on_deallocate \
  if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) return external_labels[8];
#else
#define check_stack_on_deallocate \
  if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) return external_labels[8];
#endif /* YAPOR_SBA && YAPOR */

#ifdef DEPTH_LIMIT
#define _procceed_instinit \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = (*_CPREG); \
      save_pc(); \
      ENV_YREG = ENV; \
      DEPTH = ENV_YREG[E_DEPTH]; \
      WRITEBACK_Y_AS_ENV(); \
      GONEXT(); \
      ENDCACHE_Y_AS_ENV();
#else /* DEPTH_LIMIT */
#define _procceed_instinit \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = (*_CPREG); \
      save_pc(); \
      ENV_YREG = ENV; \
      WRITEBACK_Y_AS_ENV(); \
      GONEXT(); \
      ENDCACHE_Y_AS_ENV();
#endif /* DEPTH_LIMIT */

#ifdef DEPTH_LIMIT
#define _fcall_instinit \
      CACHE_Y_AS_ENV(YREG); \
      ENV_YREG[E_CP] = (CELL) (*_CPREG); \
      ENV_YREG[E_E] = (CELL) ENV; \
      ENV_YREG[E_DEPTH] = DEPTH; \
      ENDCACHE_Y_AS_ENV(); \
      _call_instinit;
#else /* DEPTH_LIMIT */
#define _fcall_instinit \
      CACHE_Y_AS_ENV(YREG); \
      ENV_YREG[E_CP] = (CELL) (*_CPREG); \
      ENV_YREG[E_E] = (CELL) ENV; \
      ENDCACHE_Y_AS_ENV(); \
      _call_instinit;
#endif /* DEPTH_LIMIT */

#ifdef LOW_LEVEL_TRACER
#ifdef NO_CHECKING
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#ifdef YAPOR
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#else /* YAPOR_SBA */
#ifdef YAPOR
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#ifdef YAPOR
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#ifdef YAPOR
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#else /* YAPOR_SBA */
#ifdef YAPOR
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#ifdef YAPOR
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#else /* NO_CHECKING */
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#ifdef YAPOR
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#else /* YAPOR_SBA */
#ifdef YAPOR
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#ifdef YAPOR
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#ifdef YAPOR
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#else /* YAPOR_SBA */
#ifdef YAPOR
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#ifdef YAPOR
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1); \
      } \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#endif /* NO_CHECKING */
#else /* LOW_LEVEL_TRACER */
#ifdef NO_CHECKING
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#ifdef YAPOR
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#else /* YAPOR_SBA */
#ifdef YAPOR
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#ifdef YAPOR
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#ifdef YAPOR
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#else /* YAPOR_SBA */
#ifdef YAPOR
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#ifdef YAPOR
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#else /* NO_CHECKING */
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#ifdef YAPOR
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#else /* YAPOR_SBA */
#ifdef YAPOR
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#ifdef YAPOR
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (DEPTH <= MkIntTerm(1)) { \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        FAIL(); \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  } \
	} else if (pt->ModuleOfPred) { \
	  DEPTH -= MkIntConstant(2); \
	} \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#ifdef YAPOR
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#else /* YAPOR_SBA */
#ifdef YAPOR
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#ifdef YAPOR
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR */
#define _call_instinit \
      CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1(); \
	check_stack_on_call; \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR */
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#endif /* NO_CHECKING */
#endif /* LOW_LEVEL_TRACER */

#ifdef LOW_LEVEL_TRACER
#ifdef NO_CHECKING
#ifdef DEPTH_LIMIT
#define _execute_instinit \
      { \
	PredEntry *pt0; \
	CACHE_Y_AS_ENV(YREG); \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	(*_PREG) = pt0->CodeOfPred; \
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
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
      }
#else /* DEPTH_LIMIT */
#define _execute_instinit \
      { \
	PredEntry *pt0; \
	CACHE_Y_AS_ENV(YREG); \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
      }
#endif /* DEPTH_LIMIT */
#else /* NO_CHECKING */
#ifdef DEPTH_LIMIT
#define _execute_instinit \
      { \
	PredEntry *pt0; \
	CACHE_Y_AS_ENV(YREG); \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	(*_PREG) = pt0->CodeOfPred; \
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
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
      }
#else /* DEPTH_LIMIT */
#define _execute_instinit \
      { \
	PredEntry *pt0; \
	CACHE_Y_AS_ENV(YREG); \
	pt0 = (*_PREG)->u.pp.p; \
	if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,pt0,XREGS+1); \
	} \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
      }
#endif /* DEPTH_LIMIT */
#endif /* NO_CHECKING */
#else /* LOW_LEVEL_TRACER */
#ifdef NO_CHECKING
#ifdef DEPTH_LIMIT
#define _execute_instinit \
      { \
	PredEntry *pt0; \
	CACHE_Y_AS_ENV(YREG); \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	(*_PREG) = pt0->CodeOfPred; \
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
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
      }
#else /* DEPTH_LIMIT */
#define _execute_instinit \
      { \
	PredEntry *pt0; \
	CACHE_Y_AS_ENV(YREG); \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
      }
#endif /* DEPTH_LIMIT */
#else /* NO_CHECKING */
#ifdef DEPTH_LIMIT
#define _execute_instinit \
      { \
	PredEntry *pt0; \
	CACHE_Y_AS_ENV(YREG); \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	(*_PREG) = pt0->CodeOfPred; \
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
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
      }
#else /* DEPTH_LIMIT */
#define _execute_instinit \
      { \
	PredEntry *pt0; \
	CACHE_Y_AS_ENV(YREG); \
	pt0 = (*_PREG)->u.pp.p; \
	CACHE_A1(); \
	BEGD(d0); \
	d0 = (CELL)B; \
	check_stack_on_execute; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	ENV_YREG[E_CB] = d0; \
	ENDD(d0); \
	GONEXT(); \
	ENDCACHE_Y_AS_ENV(); \
      }
#endif /* DEPTH_LIMIT */
#endif /* NO_CHECKING */
#endif /* LOW_LEVEL_TRACER */

#ifdef LOW_LEVEL_TRACER
#ifdef NO_CHECKING
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _dexecute_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.pp.p,XREGS+1); \
	  } \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
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
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	    ENV_YREG = (CELL *) top_b; \
      } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR_SBA */
#define _dexecute_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.pp.p,XREGS+1); \
	  } \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
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
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	    ENV_YREG = (CELL *) top_b; \
	  } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _dexecute_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.pp.p,XREGS+1); \
	  } \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
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
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	if (ENV_YREG > (CELL *)B) { \
	  ENV_YREG = (CELL *)B; \
	} \
	else { \
	  ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _dexecute_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.pp.p,XREGS+1); \
	  } \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	    ENV_YREG = (CELL *) top_b; \
      } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR_SBA */
#define _dexecute_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.pp.p,XREGS+1); \
	  } \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	    ENV_YREG = (CELL *) top_b; \
	  } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _dexecute_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.pp.p,XREGS+1); \
	  } \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	if (ENV_YREG > (CELL *)B) { \
	  ENV_YREG = (CELL *)B; \
	} \
	else { \
	  ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#else /* NO_CHECKING */
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _dexecute_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.pp.p,XREGS+1); \
	  } \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	check_stack_on_dexecute; \
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
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	    ENV_YREG = (CELL *) top_b; \
      } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR_SBA */
#define _dexecute_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.pp.p,XREGS+1); \
	  } \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	check_stack_on_dexecute; \
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
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	    ENV_YREG = (CELL *) top_b; \
	  } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _dexecute_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.pp.p,XREGS+1); \
	  } \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	check_stack_on_dexecute; \
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
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	if (ENV_YREG > (CELL *)B) { \
	  ENV_YREG = (CELL *)B; \
	} \
	else { \
	  ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _dexecute_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.pp.p,XREGS+1); \
	  } \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	check_stack_on_dexecute; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	    ENV_YREG = (CELL *) top_b; \
      } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR_SBA */
#define _dexecute_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.pp.p,XREGS+1); \
	  } \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	check_stack_on_dexecute; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	    ENV_YREG = (CELL *) top_b; \
	  } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _dexecute_instinit \
      if (Yap_do_low_level_trace) { \
	low_level_trace(enter_pred,(*_PREG)->u.pp.p,XREGS+1); \
	  } \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	check_stack_on_dexecute; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	if (ENV_YREG > (CELL *)B) { \
	  ENV_YREG = (CELL *)B; \
	} \
	else { \
	  ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#endif /* NO_CHECKING */
#else /* LOW_LEVEL_TRACER */
#ifdef NO_CHECKING
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _dexecute_instinit \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
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
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	    ENV_YREG = (CELL *) top_b; \
      } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR_SBA */
#define _dexecute_instinit \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
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
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	    ENV_YREG = (CELL *) top_b; \
	  } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _dexecute_instinit \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
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
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	if (ENV_YREG > (CELL *)B) { \
	  ENV_YREG = (CELL *)B; \
	} \
	else { \
	  ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _dexecute_instinit \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	    ENV_YREG = (CELL *) top_b; \
      } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR_SBA */
#define _dexecute_instinit \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	    ENV_YREG = (CELL *) top_b; \
	  } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _dexecute_instinit \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	if (ENV_YREG > (CELL *)B) { \
	  ENV_YREG = (CELL *)B; \
	} \
	else { \
	  ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#else /* NO_CHECKING */
#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _dexecute_instinit \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	check_stack_on_dexecute; \
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
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	    ENV_YREG = (CELL *) top_b; \
      } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR_SBA */
#define _dexecute_instinit \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	check_stack_on_dexecute; \
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
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	    ENV_YREG = (CELL *) top_b; \
	  } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _dexecute_instinit \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	check_stack_on_dexecute; \
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
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	if (ENV_YREG > (CELL *)B) { \
	  ENV_YREG = (CELL *)B; \
	} \
	else { \
	  ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define _dexecute_instinit \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	check_stack_on_dexecute; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	    ENV_YREG = (CELL *) top_b; \
      } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#else /* YAPOR_SBA */
#define _dexecute_instinit \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	check_stack_on_dexecute; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	    ENV_YREG = (CELL *) top_b; \
	  } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define _dexecute_instinit \
     CACHE_Y_AS_ENV(YREG); \
      { \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p; \
	check_stack_on_dexecute; \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	(*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	if (ENV_YREG > (CELL *)B) { \
	  ENV_YREG = (CELL *)B; \
	} \
	else { \
	  ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	GONEXT(); \
      } \
      ENDCACHE_Y_AS_ENV();
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
#endif /* NO_CHECKING */
#endif /* LOW_LEVEL_TRACER */

#ifdef DEPTH_LIMIT
#define _allocate_instinit \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), e); \
      ENV_YREG[E_CP] = (CELL) (*_CPREG); \
      ENV_YREG[E_E] = (CELL) ENV; \
      ENV_YREG[E_DEPTH] = DEPTH; \
      ENV = ENV_YREG; \
      ENDCACHE_Y_AS_ENV(); \
      GONEXT();
#else /* DEPTH_LIMIT */
#define _allocate_instinit \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), e); \
      ENV_YREG[E_CP] = (CELL) (*_CPREG); \
      ENV_YREG[E_E] = (CELL) ENV; \
      ENV = ENV_YREG; \
      ENDCACHE_Y_AS_ENV(); \
      GONEXT();
#endif /* DEPTH_LIMIT */

#ifdef DEPTH_LIMIT
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#ifdef NO_CHECKING
#define _deallocate_instinit \
      check_trail(TR); \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      (*_SREG) = YREG; \
      (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E]; \
      DEPTH = ENV_YREG[E_DEPTH]; \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) \
	    ENV_YREG = (CELL *) top_b; \
	else \
	  ENV_YREG = (CELL *)((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
      } \
      WRITEBACK_Y_AS_ENV(); \
      ENDCACHE_Y_AS_ENV(); \
      GONEXT();
#else /* NO_CHECKING */
#define _deallocate_instinit \
      check_trail(TR); \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      (*_SREG) = YREG; \
      (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E]; \
      DEPTH = ENV_YREG[E_DEPTH]; \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) \
	    ENV_YREG = (CELL *) top_b; \
	else \
	  ENV_YREG = (CELL *)((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
      } \
      WRITEBACK_Y_AS_ENV(); \
      check_stack_on_deallocate; \
      ENDCACHE_Y_AS_ENV(); \
      GONEXT();
#endif /* NO_CHECKING */
#else /* YAPOR_SBA */
#ifdef NO_CHECKING
#define _deallocate_instinit \
      check_trail(TR); \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      (*_SREG) = YREG; \
      (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E]; \
      DEPTH = ENV_YREG[E_DEPTH]; \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (ENV_YREG > (CELL *) top_b) \
	    ENV_YREG = (CELL *) top_b; \
	else \
	  ENV_YREG = (CELL *)((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
      } \
      WRITEBACK_Y_AS_ENV(); \
      ENDCACHE_Y_AS_ENV(); \
      GONEXT();
#else /* NO_CHECKING */
#define _deallocate_instinit \
      check_trail(TR); \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      (*_SREG) = YREG; \
      (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E]; \
      DEPTH = ENV_YREG[E_DEPTH]; \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (ENV_YREG > (CELL *) top_b) \
	    ENV_YREG = (CELL *) top_b; \
	else \
	  ENV_YREG = (CELL *)((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
      } \
      WRITEBACK_Y_AS_ENV(); \
      check_stack_on_deallocate; \
      ENDCACHE_Y_AS_ENV(); \
      GONEXT();
#endif /* NO_CHECKING */
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#ifdef NO_CHECKING
#define _deallocate_instinit \
      check_trail(TR); \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      (*_SREG) = YREG; \
      (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E]; \
      DEPTH = ENV_YREG[E_DEPTH]; \
      if (ENV_YREG > (CELL *) B) \
	ENV_YREG = (CELL *) B; \
      else \
	ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
      WRITEBACK_Y_AS_ENV(); \
      ENDCACHE_Y_AS_ENV(); \
      GONEXT();
#else /* NO_CHECKING */
#define _deallocate_instinit \
      check_trail(TR); \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      (*_SREG) = YREG; \
      (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E]; \
      DEPTH = ENV_YREG[E_DEPTH]; \
      if (ENV_YREG > (CELL *) B) \
	ENV_YREG = (CELL *) B; \
      else \
	ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
      WRITEBACK_Y_AS_ENV(); \
      check_stack_on_deallocate; \
      ENDCACHE_Y_AS_ENV(); \
      GONEXT();
#endif /* NO_CHECKING */
#endif /* FROZEN_STACKS */
#else /* DEPTH_LIMIT */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#ifdef NO_CHECKING
#define _deallocate_instinit \
      check_trail(TR); \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      (*_SREG) = YREG; \
      (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E]; \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) \
	    ENV_YREG = (CELL *) top_b; \
	else \
	  ENV_YREG = (CELL *)((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
      } \
      WRITEBACK_Y_AS_ENV(); \
      ENDCACHE_Y_AS_ENV(); \
      GONEXT();
#else /* NO_CHECKING */
#define _deallocate_instinit \
      check_trail(TR); \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      (*_SREG) = YREG; \
      (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E]; \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) \
	    ENV_YREG = (CELL *) top_b; \
	else \
	  ENV_YREG = (CELL *)((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
      } \
      WRITEBACK_Y_AS_ENV(); \
      check_stack_on_deallocate; \
      ENDCACHE_Y_AS_ENV(); \
      GONEXT();
#endif /* NO_CHECKING */
#else /* YAPOR_SBA */
#ifdef NO_CHECKING
#define _deallocate_instinit \
      check_trail(TR); \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      (*_SREG) = YREG; \
      (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E]; \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (ENV_YREG > (CELL *) top_b) \
	    ENV_YREG = (CELL *) top_b; \
	else \
	  ENV_YREG = (CELL *)((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
      } \
      WRITEBACK_Y_AS_ENV(); \
      ENDCACHE_Y_AS_ENV(); \
      GONEXT();
#else /* NO_CHECKING */
#define _deallocate_instinit \
      check_trail(TR); \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      (*_SREG) = YREG; \
      (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E]; \
      { \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (ENV_YREG > (CELL *) top_b) \
	    ENV_YREG = (CELL *) top_b; \
	else \
	  ENV_YREG = (CELL *)((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
      } \
      WRITEBACK_Y_AS_ENV(); \
      check_stack_on_deallocate; \
      ENDCACHE_Y_AS_ENV(); \
      GONEXT();
#endif /* NO_CHECKING */
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#ifdef NO_CHECKING
#define _deallocate_instinit \
      check_trail(TR); \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      (*_SREG) = YREG; \
      (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E]; \
      if (ENV_YREG > (CELL *) B) \
	ENV_YREG = (CELL *) B; \
      else \
	ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
      WRITEBACK_Y_AS_ENV(); \
      ENDCACHE_Y_AS_ENV(); \
      GONEXT();
#else /* NO_CHECKING */
#define _deallocate_instinit \
      check_trail(TR); \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      (*_SREG) = YREG; \
      (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E]; \
      if (ENV_YREG > (CELL *) B) \
	ENV_YREG = (CELL *) B; \
      else \
	ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size((*_CPREG))); \
      WRITEBACK_Y_AS_ENV(); \
      check_stack_on_deallocate; \
      ENDCACHE_Y_AS_ENV(); \
      GONEXT();
#endif /* NO_CHECKING */
#endif /* FROZEN_STACKS */
#endif /* DEPTH_LIMIT */
