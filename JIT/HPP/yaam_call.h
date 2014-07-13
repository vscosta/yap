#define EXECUTE_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
        register CELL d0; \
	PredEntry *pt0; \
	register CELL *ENV_YREG = (YREG); \
	pt0 = (*_PREG)->u.pp.p;
	
#ifdef LOW_LEVEL_TRACER
#define EXECUTE_LOW_LEVEL_TRACER \
	  low_level_trace(enter_pred,pt0,XREGS+1);
#endif

#define EXECUTE_POST_LOW_LEVEL_TRACER \
	CACHE_A1(); \
	ALWAYS_LOOKAHEAD(pt0->OpcodeOfPred); \
	d0 = (CELL)B;
	
#define EXECUTE_POST_NOCHECKING \
	(*_PREG) = pt0->CodeOfPred; \
	save_pc(); \
	ENV_YREG[E_CB] = d0;
	
#ifdef DEPTH_LIMIT
#define EXECUTE_DEPTH_MINOR \
      FAILED = 0; \
	  if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
          YAAM_FAIL; \
        } \
	    else { \
          DEPTH = RESET_DEPTH(); \
        } \
	  }
	  
#define EXECUTE_DEPTH_MOFPRED \
      FAILED = 0; \
      DEPTH -= MkIntConstant(2);

#define EXECUTE_DEPTH_END \
      FAILED = 0;
#endif

#define EXECUTE_END_END \
      BLOCK = (CELL)EXECUTE_END_END; \
    if (!FAILED) { \
	  ALWAYS_GONext(); \
    } \

#define DEXECUTE_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  
#ifdef LOW_LEVEL_TRACER
#define DEXECUTE_LOW_LEVEL_TRACER \
	low_level_trace(enter_pred,(*_PREG)->u.pp.p,XREGS+1);
#endif

#define DEXECUTE_POST_LOW_LEVEL_TRACER \
     CACHE_Y_AS_ENV(YREG); \
	PredEntry *pt0; \
	CACHE_A1(); \
	pt0 = (*_PREG)->u.pp.p;
	
#ifdef DEPTH_LIMIT
#define DEXECUTE_DEPTH_MINOR \
      FAILED = 0; \
      if (pt0->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)) { \
	      YAAM_FAIL; \
		} \
	    else { \
		  DEPTH = RESET_DEPTH(); \
		} \
	  }
      
#define DEXECUTE_DEPTH_MOFPRED \
      FAILED = 0; \
      DEPTH -= MkIntConstant(2);
      
#define DEXECUTE_DEPTH_END \
      FAILED = 0;
#endif

#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define DEXECUTE_END_END \
      BLOCK = (CELL)DEXECUTE_END_END; \
	 if (!FAILED) { \
	  (*_PREG) = pt0->CodeOfPred; \
	  save_pc(); \
	  (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	  ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	    ENV_YREG = (CELL *) top_b; \
      } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	  WRITEBACK_Y_AS_ENV(); \
	  ENV_YREG[E_CB] = (CELL) B; \
	  ALWAYS_GONext(); \
    } \
    ENDCACHE_Y_AS_ENV();
#else /* YAPOR_SBA */
#define DEXECUTE_END_END \
      BLOCK = (CELL)DEXECUTE_END_END; \
	 if (!FAILED) { \
	  (*_PREG) = pt0->CodeOfPred; \
	  save_pc(); \
	  (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
	  ENV_YREG = ENV = (CELL *) ENV_YREG[E_E]; \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	    ENV_YREG = (CELL *) top_b; \
	  } \
	  else { \
	    ENV_YREG = (CELL *)((CELL)ENV_YREG + ENV_Size((*_CPREG))); \
	  } \
	  WRITEBACK_Y_AS_ENV(); \
	  ENV_YREG[E_CB] = (CELL) B; \
	  ALWAYS_GONext(); \
    } \
    ENDCACHE_Y_AS_ENV();
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define DEXECUTE_END_END \
      BLOCK = (CELL)DEXECUTE_END_END; \
	 if (!FAILED) { \
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
	  ALWAYS_GONext(); \
    } \
    ENDCACHE_Y_AS_ENV();
#endif /* FROZEN_STACKS */

#ifdef DEPTH_LIMIT
#define FCALL_INST \
      CACHE_Y_AS_ENV(YREG); \
      ENV_YREG[E_CP] = (CELL) (*_CPREG); \
      ENV_YREG[E_E] = (CELL) ENV; \
      ENV_YREG[E_DEPTH] = DEPTH; \
      ENDCACHE_Y_AS_ENV();
#else /* DEPTH_LIMIT */
#define FCALL_INST \
      CACHE_Y_AS_ENV(YREG); \
      ENV_YREG[E_CP] = (CELL) (*_CPREG); \
      ENV_YREG[E_E] = (CELL) ENV; \
      ENDCACHE_Y_AS_ENV();
#endif /* DEPTH_LIMIT */
	  
#define CALL_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      if (Yap_op_from_opcode((*_PREG)->opc) == _fcall) { \
	    FCALL_INST; \
	  }
	  
#ifdef LOW_LEVEL_TRACER
#define CALL_LOW_LEVEL_TRACER \
	low_level_trace(enter_pred,(*_PREG)->u.Osbpp.p,XREGS+1);
#endif

#define CALL_POST_LOW_LEVEL_TRACER \
    register CELL *ENV_YREG = (YREG); \
	PredEntry *pt; \
	pt = (*_PREG)->u.Osbpp.p; \
	CACHE_A1();
	
#define CALL_POST_NO_CHECKING \
	ENV = ENV_YREG; \
	ENV_YREG = (CELL *) (((char *) ENV_YREG) + (*_PREG)->u.Osbpp.s); \
	(*_CPREG) = NEXTOP((*_PREG), Osbpp); \
	(*_PREG) = pt->CodeOfPred; \
	save_pc(); \
	
#ifdef DEPTH_LIMIT
#define CALL_DEPTH_MINOR \
      FAILED = 0; \
	  if (pt->ModuleOfPred) { \
	    if (DEPTH == MkIntTerm(0)){ \
	        YAAM_FAIL; \
	    } else { \
	        DEPTH = RESET_DEPTH(); \
	    } \
	  }
	  
#define CALL_DEPTH_MOFPRED \
      FAILED = 0; \
	  DEPTH -= MkIntConstant(2);
      
#define CALL_DEPTH_END \
      FAILED = 0;
#endif

#ifdef YAPOR
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define CALL_END_END \
  BLOCK = (CELL)CALL_END_END; \
  if (!FAILED) { \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	ALWAYS_GONext(); \
  }
#else /* YAPOR_SBA */
#define CALL_END_END \
  BLOCK = (CELL)CALL_END_END; \
  if (!FAILED) { \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	ALWAYS_GONext(); \
  }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define CALL_END_END \
  BLOCK = (CELL)CALL_END_END; \
  if (!FAILED) { \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	SCH_check_requests(); \
	ALWAYS_GONext(); \
  }
#endif /* FROZEN_STACKS */
#else /* YAPOR */
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define CALL_END_END \
  BLOCK = (CELL)CALL_END_END; \
  if (!FAILED) { \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	ALWAYS_GONext(); \
  }
#else /* YAPOR_SBA */
#define CALL_END_END \
  BLOCK = (CELL)CALL_END_END; \
  if (!FAILED) { \
	{ \
	  choiceptr top_b = PROTECT_FROZEN_B(B); \
	  if (ENV_YREG > (CELL *) top_b) { \
	      ENV_YREG = (CELL *) top_b; \
	  } \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	ALWAYS_GONext(); \
  }
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define CALL_END_END \
  BLOCK = (CELL)CALL_END_END; \
  if (!FAILED) { \
	if (ENV_YREG > (CELL *) B) { \
	  ENV_YREG = (CELL *) B; \
	} \
	WRITEBACK_Y_AS_ENV(); \
	ENV_YREG[E_CB] = (CELL) B; \
	ALWAYS_GONext(); \
  }
#endif /* FROZEN_STACKS */
#endif /* YAPOR */

#define PROCCEED_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = (*_CPREG); \
      save_pc(); \
      ENV_YREG = ENV;
	  
#ifdef DEPTH_LIMIT
#define PROCCEED_DEPTH \
      DEPTH = ENV_YREG[E_DEPTH];
#endif

#define PROCCEED_END \
      BLOCK = (CELL)PROCCEED_END; \
      WRITEBACK_Y_AS_ENV(); \
      ENDCACHE_Y_AS_ENV(); \
      ALWAYS_GONext();

#define ALLOCATE_INSTINIT \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), e); \
      ENV_YREG[E_CP] = (CELL) (*_CPREG); \
      ENV_YREG[E_E] = (CELL) ENV;
	  
#ifdef DEPTH_LIMIT
#define ALLOCATE_DEPTH \
      ENV_YREG[E_DEPTH] = DEPTH;
#endif

#define ALLOCATE_END \
      ENV = ENV_YREG; \
      ENDCACHE_Y_AS_ENV(); \
      GONext();

#define DEALLOCATE_INSTINIT

#define DEALLOCATE_POST_CHECK \
      CACHE_Y_AS_ENV(YREG); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      (*_SREG) = YREG; \
      (*_CPREG) = (yamop *) ENV_YREG[E_CP]; \
      ENV = ENV_YREG = (CELL *) ENV_YREG[E_E];
	  
#ifdef DEPTH_LIMIT
#define DEALLOCATE_DEPTH \
      DEPTH = ENV_YREG[E_DEPTH];
#endif

#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define DEALLOCATE_FROZEN \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) \
	    ENV_YREG = (CELL *) top_b; \
	else \
	  ENV_YREG = (CELL *)((CELL) ENV_YREG + ENV_Size(CPREG));
#else /* YAPOR_SBA */
#define DEALLOCATE_FROZEN \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (ENV_YREG > (CELL *) top_b) \
	    ENV_YREG = (CELL *) top_b; \
	else \
	  ENV_YREG = (CELL *)((CELL) ENV_YREG + ENV_Size(CPREG));
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define DEALLOCATE_FROZEN \
      if (ENV_YREG > (CELL *) B) \
	ENV_YREG = (CELL *) B; \
      else \
	ENV_YREG = (CELL *) ((CELL) ENV_YREG + ENV_Size(CPREG));
#endif /* FROZEN_STACKS */
	
#define DEALLOCATE_POST_FROZEN \
      WRITEBACK_Y_AS_ENV();

#define DEALLOCATE_END \
      ENDCACHE_Y_AS_ENV(); \
      GONext();

