#define CALL_CPRED_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
        register CELL d0;
		
#if (defined(YAPOR_SBA) && defined(YAPOR)) || defined(TABLING)
#define CALL_CPRED_TEST_STACK \
      if (!((*_PREG)->u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) { \
	CACHE_Y_AS_ENV(YREG); \
	if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) {  return external_labels[6]; } \
	ENDCACHE_Y_AS_ENV(); \
      }
#else
#define CALL_CPRED_TEST_STACK \
      if (!((*_PREG)->u.Osbpp.p->PredFlags & (SafePredFlag|HiddenPredFlag))) { \
	CACHE_Y_AS_ENV(YREG); \
	if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) { return external_labels[6]; } \
	ENDCACHE_Y_AS_ENV(); \
      }
#endif

#ifdef FROZEN_STACKS
#define CALL_CPRED_FROZEN_INIT \
	choiceptr top_b = PROTECT_FROZEN_B(B);

#ifdef YAPOR_SBA
#define CALL_CPRED_TOPB \
	if (YREG > (CELL *) top_b || YREG < HR) \
	    ASP = (CELL *)top_b; \
	else \
	    ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s);
#else /* YAPOR_SBA */
#define CALL_CPRED_TOPB \
	if (YREG > (CELL *) top_b) \
	    ASP = (CELL *)top_b; \
	else \
	    ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s);
#endif /* YAPOR_SBA */

#else
#define CALL_CPRED_NOFROZEN \
      SET_ASP(YREG, (*_PREG)->u.Osbpp.s);
#endif

#ifdef LOW_LEVEL_TRACER
#define CALL_CPRED_LOW_LEVEL_TRACER \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1);
#endif

#define CALL_CPRED_POST_LOW_LEVEL_TRACER \
      CPredicate f = (*_PREG)->u.Osbpp.p->cs.f_code; \
      (*_PREG) = NEXTOP((*_PREG), Osbpp); \
      saveregs(); \
      d0 = (f)(PASS_REGS1); \
      setregs();

#ifdef SHADOW_S
#define CALL_CPRED_SETSREG \
      (*_SREG) = Yap_REGS.S_;
#endif

#define CALL_CPRED_END \
      BLOCK = (CELL)CALL_CPRED_END; \
      FAILED = 0; \
      if (!d0) { \
        YAAM_FAIL; \
	  } \
	  else { \
	    CACHE_A1(); \
	    JMPNext(); \
	  }

#define EXECUTE_CPRED_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \

#define EXECUTE_CPRED_POST_CHECK_TRAIL \
	PredEntry *pt0; \
        CELL d0; \
	CACHE_Y_AS_ENV(YREG);

#ifdef FROZEN_STACKS
#define EXECUTE_CPRED_FROZEN \
	  choiceptr top_b = PROTECT_FROZEN_B(B);

#ifdef YAPOR_SBA
#define EXECUTE_CPRED_TOPB \
	  if (YREG > (CELL *) top_b || YREG < HR) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  }
#else
#define EXECUTE_CPRED_TOPB \
	  if (YREG > (CELL *) top_b) { \
	      ASP = (CELL *)top_b; \
	  } \
	  else { \
        ASP = YREG+E_CB; \
	  }
#endif

#else
#define EXECUTE_CPRED_NOFROZEN \
	SET_ASP(YREG, E_CB*sizeof(CELL));
#endif

#define EXECUTE_CPRED_POST_FROZEN \
	pt0 = (*_PREG)->u.pp.p;

#ifdef LOW_LEVEL_TRACER
#define EXECUTE_CPRED_LOW_LEVEL_TRACER \
	  low_level_trace(enter_pred,pt0,XREGS+1);
#endif

#define EXECUTE_CPRED_POST_LOW_LEVEL_TRACER \
	CACHE_A1(); \
	register CELL d0; \
	d0 = (CELL)B;

#define EXECUTE_CPRED_SAVE_PC \
	save_pc(); \
	ENV_YREG[E_CB] = d0;
	
#ifdef DEPTH_LIMIT
#define EXECUTE_CPRED_DEPTH_MINOR \
      FAILED = 0; \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      YAAM_FAIL; \
		} \
	    else { \
          DEPTH = RESET_DEPTH(); \
	    } \
	  }
	  
#define EXECUTE_CPRED_DEPTH_MOFPRED \
      FAILED = 0; \
	  DEPTH -= MkIntConstant(2);

#define EXECUTE_CPRED_DEPTH_END \
      FAILED = 0;
#endif

#ifdef SHADOW_S
#ifdef DEPTH_LIMIT
#define EXECUTE_CPRED_END \
      BLOCK = (CELL)EXECUTE_CPRED_END; \
	if (!FAILED) { \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    YAAM_FAIL; \
	  } \
	  else { \
	    if (oldPREG == (*_PREG)) { \
	      (*_PREG) = (*_CPREG); \
	      ENV_YREG = ENV; \
	      DEPTH = ENV_YREG[E_DEPTH]; \
	      WRITEBACK_Y_AS_ENV(); \
	    } else { \
	      CACHE_A1(); \
	    } \
	    JMPNext(); \
	  } \
	} \
	ENDCACHE_Y_AS_ENV();
#else /* DEPTH_LIMIT */
#define EXECUTE_CPRED_END \
      BLOCK = (CELL)EXECUTE_CPRED_END; \
	if (!FAILED) { \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  (*_SREG) = Yap_REGS.S_; \
	  if (!d0) { \
	    YAAM_FAIL; \
	  } \
	  else { \
	    if (oldPREG == (*_PREG)) { \
	      (*_PREG) = (*_CPREG); \
	      ENV_YREG = ENV; \
	      WRITEBACK_Y_AS_ENV(); \
	    } else { \
	      CACHE_A1(); \
	    } \
	    JMPNext(); \
	  } \
	} \
	ENDCACHE_Y_AS_ENV();
#endif /* DEPTH_LIMIT */
#else /* SHADOW_S */
#ifdef DEPTH_LIMIT
#define EXECUTE_CPRED_END \
      BLOCK = (CELL)EXECUTE_CPRED_END; \
	if (!FAILED) { \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    YAAM_FAIL; \
	  } \
	  else { \
	    if (oldPREG == (*_PREG)) { \
	      (*_PREG) = (*_CPREG); \
	      ENV_YREG = ENV; \
	      DEPTH = ENV_YREG[E_DEPTH]; \
	      WRITEBACK_Y_AS_ENV(); \
	    } else { \
	      CACHE_A1(); \
	    } \
	    JMPNext(); \
	  } \
	} \
	ENDCACHE_Y_AS_ENV();
#else /* DEPTH_LIMIT */
#define EXECUTE_CPRED_END \
      BLOCK = (CELL)EXECUTE_CPRED_END; \
	if (!FAILED) { \
	  CPredicate f = (*_PREG)->u.pp.p->cs.f_code; \
	  yamop *oldPREG = (*_PREG); \
	  saveregs(); \
	  d0 = (f)(PASS_REGS1); \
	  setregs(); \
	  if (!d0) { \
	    YAAM_FAIL; \
	  } \
	  else { \
	    if (oldPREG == (*_PREG)) { \
	      (*_PREG) = (*_CPREG); \
	      ENV_YREG = ENV; \
	      WRITEBACK_Y_AS_ENV(); \
	    } else { \
	      CACHE_A1(); \
	    } \
	    JMPNext(); \
	  } \
	} \
	ENDCACHE_Y_AS_ENV();
#endif /* DEPTH_LIMIT */
#endif /* SHADOW_S */

#if (defined(YAPOR_SBA) && defined(YAPOR)) || defined(TABLING)
#define CALL_USERCPRED_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
	CACHE_Y_AS_ENV(YREG); \
	if (__builtin_expect( ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG,B_FZ)) - Unsigned(YOUNGEST_H(H_FZ,H))) < CreepFlag), 0)  ) {  return external_labels[6]; } \
	ENDCACHE_Y_AS_ENV();
#else
#define CALL_USERCPRED_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
	CACHE_Y_AS_ENV(YREG); \
	if  (__builtin_expect(((Int)(Unsigned(ENV_YREG) - Unsigned(HR)) < CreepFlag ), 0) ) { return external_labels[6]; } \
	ENDCACHE_Y_AS_ENV();
#endif

#ifdef LOW_LEVEL_TRACER
#define CALL_USERCPRED_LOW_LEVEL_TRACER \
	  low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1);
#endif

#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define CALL_USERCPRED_FROZEN \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (YREG > (CELL *) top_b || YREG < HR) \
	  ASP = (CELL *) top_b; \
	else \
      ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s);
#else /* YAPOR_SBA */
#define CALL_USERCPRED_FROZEN \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (YREG > (CELL *) top_b) \
      ASP = (CELL *) top_b; \
	else \
      ASP = (CELL *)(((char *)YREG) +  (*_PREG)->u.Osbpp.s);
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define CALL_USERCPRED_FROZEN \
      SET_ASP(YREG, (*_PREG)->u.Osbpp.s);
#endif /* FROZEN_STACKS */

#define CALL_USERCPRED_POST_FROZEN \
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
	(*_PREG) = savedP;

#define CALL_USERCPRED_END \
      BLOCK = (CELL)CALL_USERCPRED_END; \
      FAILED = 0; \
      if (EX) { \
	struct DB_TERM *exp = EX; \
	EX = NULL; \
	Yap_JumpToEnv(Yap_PopTermFromDB(exp)); \
      } \
      if (!(*_SREG)) { \
	YAAM_FAIL; \
      } \
      else { \
    YENV = ENV; \
    YREG = ENV; \
    JMPNext(); \
      }
