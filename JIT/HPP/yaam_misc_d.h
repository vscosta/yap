#if PARALLEL_YAP
#define LUCK_LU_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
    if (PP) { \
	  GONext(); \
    } \
	else { \
      PP = (*_PREG)->u.p.p; \
      PELOCK(3, PP); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      GONext(); \
	}
#else /* PARALLEL_YAP */
#define LUCK_LU_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      (*_PREG) = NEXTOP((*_PREG), p); \
      GONext();
#endif /* PARALLEL_YAP */

#define LOCK_LU_END \
      BLOCK = (CELL)LOCK_LU_END;

#define UNLOCK_LU_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#if defined(YAPOR) || defined(THREADS)
#define UNLOCK_LU_YAPOR_THREADS \
      UNLOCKPE(1,PP); \
      PP = NULL;
#endif

#define UNLOCK_LU_END \
      (*_PREG) = NEXTOP((*_PREG), e); \
      GONext();

#define ALLOC_FOR_LOGICAL_PRED_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#if MULTIPLE_STACKS
#define ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS \
	LogUpdClause *cl = (*_PREG)->u.L.ClBase;
#if PARALLEL_YAP
#define ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS_PARALLEL \
	PredEntry *ap = cl->ClPred;
#endif
#define ALLOC_FOR_LOGICAL_PRED_MULTIPLE_STACKS_END \
	INC_CLREF_COUNT(cl); \
	TRAIL_CLREF(cl); \
	UNLOCKPE(2,ap); \
	PP = NULL;
#else
#define ALLOC_FOR_LOGICAL_PRED_NOMULTIPLE_STACKS_INIT \
	LogUpdClause *cl = (LogUpdClause *)(*_PREG)->u.L.ClBase;

#define ALLOC_FOR_LOGICAL_PRED_NOMULTIPLE_STACKS_IF \
	if (!(cl->ClFlags & InUseMask)) { \
	  cl->ClFlags |= InUseMask; \
	  TRAIL_CLREF(cl); \
	}
#endif

#define ALLOC_FOR_LOGICAL_PRED_END \
      (*_PREG) = NEXTOP((*_PREG), L); \
      JMPNext();

/** COPY_IDB_TERM_INSTINIT was changed: no error messages **/
#if defined(YAPOR) || defined(THREADS)
#if MULTIPLE_STACKS
#ifdef DEPTH_LIMIT
#define COPY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	Term t; \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	saveregs(); \
	while ((t = Yap_FetchTermFromDB(cl->ClSource)) == 0L) { \
	  LOCAL_Error_TYPE = YAP_NO_ERROR; \
	} \
	  if (!Yap_IUnify(ARG2, t)) { \
	    setregs(); \
	    UNLOCKPE(5,PP); \
	    PP = NULL; \
	    YAAM_FAIL; \
	  } \
	  else if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	    setregs(); \
	    UNLOCKPE(6,PP); \
	    PP = NULL; \
	    YAAM_FAIL; \
	  } \
	  else { \
	    setregs(); \
	    INC_CLREF_COUNT(cl); \
	    TRAIL_CLREF(cl); \
	    UNLOCKPE(7,PP); \
	    PP = NULL; \
        (*_PREG) = (*_CPREG); \
        YREG = ENV; \
        DEPTH = YREG[E_DEPTH]; \
        JMPNext(); \
	  }
#else /* DEPTH_LIMIT */
#define COPY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	Term t; \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	saveregs(); \
	while ((t = Yap_FetchTermFromDB(cl->ClSource)) == 0L) { \
	  LOCAL_Error_TYPE = YAP_NO_ERROR; \
	} \
	  if (!Yap_IUnify(ARG2, t)) { \
	    setregs(); \
	    UNLOCKPE(5,PP); \
	    PP = NULL; \
	    YAAM_FAIL; \
	  } \
	  else if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	    setregs(); \
	    UNLOCKPE(6,PP); \
	    PP = NULL; \
	    YAAM_FAIL; \
	  } \
	  else { \
	    setregs(); \
	    INC_CLREF_COUNT(cl); \
	    TRAIL_CLREF(cl); \
	    UNLOCKPE(7,PP); \
	    PP = NULL; \
        (*_PREG) = (*_CPREG); \
        YREG = ENV; \
        JMPNext(); \
	  }
#endif /* DEPTH_LIMIT */
#else /* MULTIPLE_STACKS */
#ifdef DEPTH_LIMIT
#define COPY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	Term t; \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	saveregs(); \
	while ((t = Yap_FetchTermFromDB(cl->ClSource)) == 0L) { \
	  LOCAL_Error_TYPE = YAP_NO_ERROR; \
	} \
	  if (!Yap_IUnify(ARG2, t)) { \
	    setregs(); \
	    UNLOCKPE(5,PP); \
	    PP = NULL; \
	    YAAM_FAIL; \
	  } \
	  else if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	    setregs(); \
	    UNLOCKPE(6,PP); \
	    PP = NULL; \
	    YAAM_FAIL; \
	  } \
	  else { \
	    setregs(); \
	    if (!(cl->ClFlags & InUseMask)) { \
	      cl->ClFlags |= InUseMask; \
	      TRAIL_CLREF(cl); \
	    } \
        (*_PREG) = (*_CPREG); \
        YREG = ENV; \
        DEPTH = YREG[E_DEPTH]; \
        JMPNext(); \
	  }
#else /* DEPTH_LIMIT */
#define COPY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	Term t; \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	saveregs(); \
	while ((t = Yap_FetchTermFromDB(cl->ClSource)) == 0L) { \
	  LOCAL_Error_TYPE = YAP_NO_ERROR; \
	} \
	  if (!Yap_IUnify(ARG2, t)) { \
	    setregs(); \
	    UNLOCKPE(5,PP); \
	    PP = NULL; \
	    YAAM_FAIL; \
	  } \
	  else if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	    setregs(); \
	    UNLOCKPE(6,PP); \
	    PP = NULL; \
	    YAAM_FAIL; \
	  } \
	  else { \
	    setregs(); \
	    if (!(cl->ClFlags & InUseMask)) { \
	      cl->ClFlags |= InUseMask; \
	      TRAIL_CLREF(cl); \
	    } \
        (*_PREG) = (*_CPREG); \
        YREG = ENV; \
        JMPNext(); \
	  }
#endif /* DEPTH_LIMIT */
#endif /* MULTIPLE_STACKS */
#else /* defined(YAPOR) || defined(THREADS) */
#if MULTIPLE_STACKS
#ifdef DEPTH_LIMIT
#define COPY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	Term t; \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	saveregs(); \
	while ((t = Yap_FetchTermFromDB(cl->ClSource)) == 0L) { \
	  LOCAL_Error_TYPE = YAP_NO_ERROR; \
	} \
	  if (!Yap_IUnify(ARG2, t)) { \
	    setregs(); \
	    UNLOCKPE(5,PP); \
	    YAAM_FAIL; \
	  } \
	  else if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	    setregs(); \
	    UNLOCKPE(6,PP); \
	    YAAM_FAIL; \
	  } \
	  else { \
	    setregs(); \
	    INC_CLREF_COUNT(cl); \
	    TRAIL_CLREF(cl); \
	    UNLOCKPE(7,PP); \
	    PP = NULL; \
        (*_PREG) = (*_CPREG); \
        YREG = ENV; \
        DEPTH = YREG[E_DEPTH]; \
        JMPNext(); \
	  }
#else /* DEPTH_LIMIT */
#define COPY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	Term t; \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	saveregs(); \
	while ((t = Yap_FetchTermFromDB(cl->ClSource)) == 0L) { \
	  LOCAL_Error_TYPE = YAP_NO_ERROR; \
	} \
	  if (!Yap_IUnify(ARG2, t)) { \
	    setregs(); \
	    UNLOCKPE(5,PP); \
	    YAAM_FAIL; \
	  } \
	  else if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	    setregs(); \
	    UNLOCKPE(6,PP); \
	    YAAM_FAIL; \
	  } \
	  else { \
	    setregs(); \
	    INC_CLREF_COUNT(cl); \
	    TRAIL_CLREF(cl); \
	    UNLOCKPE(7,PP); \
	    PP = NULL; \
        (*_PREG) = (*_CPREG); \
        YREG = ENV; \
        JMPNext(); \
	  }
#endif /* DEPTH_LIMIT */
#else /* MULTIPLE_STACKS */
#ifdef DEPTH_LIMIT
#define COPY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	Term t; \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	saveregs(); \
	while ((t = Yap_FetchTermFromDB(cl->ClSource)) == 0L) { \
	  LOCAL_Error_TYPE = YAP_NO_ERROR; \
	} \
	  if (!Yap_IUnify(ARG2, t)) { \
	    setregs(); \
	    UNLOCKPE(5,PP); \
	    YAAM_FAIL; \
	  } \
	  else if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	    setregs(); \
	    UNLOCKPE(6,PP); \
	    YAAM_FAIL; \
	  } \
	  else { \
	    setregs(); \
	    if (!(cl->ClFlags & InUseMask)) { \
	      cl->ClFlags |= InUseMask; \
	      TRAIL_CLREF(cl); \
	    } \
        (*_PREG) = (*_CPREG); \
        YREG = ENV; \
        DEPTH = YREG[E_DEPTH]; \
        JMPNext(); \
	  }
#else /* DEPTH_LIMIT */
#define COPY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	Term t; \
	SET_ASP(YREG, E_CB*sizeof(CELL)); \
	saveregs(); \
	while ((t = Yap_FetchTermFromDB(cl->ClSource)) == 0L) { \
	  LOCAL_Error_TYPE = YAP_NO_ERROR; \
	} \
	  if (!Yap_IUnify(ARG2, t)) { \
	    setregs(); \
	    UNLOCKPE(5,PP); \
	    YAAM_FAIL; \
	  } \
	  else if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	    setregs(); \
	    UNLOCKPE(6,PP); \
	    YAAM_FAIL; \
	  } \
	  else { \
	    setregs(); \
	    if (!(cl->ClFlags & InUseMask)) { \
	      cl->ClFlags |= InUseMask; \
	      TRAIL_CLREF(cl); \
	    } \
        (*_PREG) = (*_CPREG); \
        YREG = ENV; \
        JMPNext(); \
	  }
#endif /* DEPTH_LIMIT */
#endif /* MULTIPLE_STACKS */
#endif /* defined(YAPOR) || defined(THREADS) */

#define COPY_IDB_TERM_END \
      BLOCK = (CELL)COPY_IDB_TERM_END;

#if defined(YAPOR) || defined(THREADS)    
#if MULTIPLE_STACKS
#ifdef DEPTH_LIMIT 
#define UNIFY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  { \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	saveregs(); \
	if (!Yap_IUnify(ARG2, cl->ClSource->Entry)) { \
	  setregs(); \
	  UNLOCKPE(8,PP); \
	  PP = NULL; \
	  FAIL(); \
	} \
	if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	  setregs(); \
	  UNLOCKPE(9,PP); \
	  PP = NULL; \
	  FAIL(); \
	} \
	setregs(); \
	INC_CLREF_COUNT(cl); \
	TRAIL_CLREF(cl); \
	UNLOCKPE(10,PP); \
	PP = NULL; \
      } \
      (*_PREG) = (*_C(*_PREG)); \
      YREG = ENV; \
      DEPTH = YREG[E_DEPTH]; \
      JMPNext();
#else /* DEPTH_LIMIT  */
#define UNIFY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  { \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	saveregs(); \
	if (!Yap_IUnify(ARG2, cl->ClSource->Entry)) { \
	  setregs(); \
	  UNLOCKPE(8,PP); \
	  PP = NULL; \
	  FAIL(); \
	} \
	if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	  setregs(); \
	  UNLOCKPE(9,PP); \
	  PP = NULL; \
	  FAIL(); \
	} \
	setregs(); \
	if (!(cl->ClFlags & InUseMask)) { \
	  cl->ClFlags |= InUseMask; \
	  TRAIL_CLREF(cl); \
	} \
      } \
      (*_PREG) = (*_C(*_PREG)); \
      YREG = ENV; \
      JMPNext();
#endif /* DEPTH_LIMIT  */
#else /* MULTIPLE_STACKS */
#ifdef DEPTH_LIMIT 
#define UNIFY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  { \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	saveregs(); \
	if (!Yap_IUnify(ARG2, cl->ClSource->Entry)) { \
	  setregs(); \
	  UNLOCKPE(8,PP); \
	  PP = NULL; \
	  FAIL(); \
	} \
	if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	  setregs(); \
	  UNLOCKPE(9,PP); \
	  PP = NULL; \
	  FAIL(); \
	} \
	setregs(); \
	if (!(cl->ClFlags & InUseMask)) { \
	  cl->ClFlags |= InUseMask; \
	  TRAIL_CLREF(cl); \
	} \
      } \
      (*_PREG) = (*_C(*_PREG)); \
      YREG = ENV; \
      DEPTH = YREG[E_DEPTH]; \
      JMPNext();
#else /* DEPTH_LIMIT  */
#define UNIFY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  { \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	saveregs(); \
	if (!Yap_IUnify(ARG2, cl->ClSource->Entry)) { \
	  setregs(); \
	  UNLOCKPE(8,PP); \
	  PP = NULL; \
	  FAIL(); \
	} \
	if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	  setregs(); \
	  UNLOCKPE(9,PP); \
	  PP = NULL; \
	  FAIL(); \
	} \
	setregs(); \
	if (!(cl->ClFlags & InUseMask)) { \
	  cl->ClFlags |= InUseMask; \
	  TRAIL_CLREF(cl); \
	} \
      } \
      (*_PREG) = (*_C(*_PREG)); \
      YREG = ENV; \
      JMPNext();
#endif /* DEPTH_LIMIT  */
#endif /* MULTIPLE_STACKS */
#else /* defined(YAPOR) || defined(THREADS) */
#if MULTIPLE_STACKS
#ifdef DEPTH_LIMIT 
#define UNIFY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  { \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	saveregs(); \
	if (!Yap_IUnify(ARG2, cl->ClSource->Entry)) { \
	  setregs(); \
	  UNLOCKPE(8,PP); \
	  FAIL(); \
	} \
	if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	  setregs(); \
	  UNLOCKPE(9,PP); \
	  FAIL(); \
	} \
	setregs(); \
	INC_CLREF_COUNT(cl); \
	TRAIL_CLREF(cl); \
	UNLOCKPE(10,PP); \
	PP = NULL; \
      } \
      (*_PREG) = (*_C(*_PREG)); \
      YREG = ENV; \
      DEPTH = YREG[E_DEPTH]; \
      JMPNext();
#else /* DEPTH_LIMIT  */
#define UNIFY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  { \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	saveregs(); \
	if (!Yap_IUnify(ARG2, cl->ClSource->Entry)) { \
	  setregs(); \
	  UNLOCKPE(8,PP); \
	  FAIL(); \
	} \
	if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	  setregs(); \
	  UNLOCKPE(9,PP); \
	  FAIL(); \
	} \
	setregs(); \
	INC_CLREF_COUNT(cl); \
	TRAIL_CLREF(cl); \
	UNLOCKPE(10,PP); \
	PP = NULL; \
      } \
      (*_PREG) = (*_C(*_PREG)); \
      YREG = ENV; \
      JMPNext();
#endif /* DEPTH_LIMIT  */
#else /* MULTIPLE_STACKS */
#ifdef DEPTH_LIMIT 
#define UNIFY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  { \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	saveregs(); \
	if (!Yap_IUnify(ARG2, cl->ClSource->Entry)) { \
	  setregs(); \
	  UNLOCKPE(8,PP); \
	  FAIL(); \
	} \
	if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	  setregs(); \
	  UNLOCKPE(9,PP); \
	  FAIL(); \
	} \
	setregs(); \
	if (!(cl->ClFlags & InUseMask)) { \
	  cl->ClFlags |= InUseMask; \
	  TRAIL_CLREF(cl); \
	} \
      } \
      (*_PREG) = (*_C(*_PREG)); \
      YREG = ENV; \
      DEPTH = YREG[E_DEPTH]; \
      JMPNext();
#else /* DEPTH_LIMIT  */
#define UNIFY_IDB_TERM_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  { \
	LogUpdClause *cl = ClauseCodeToLogUpdClause((*_PREG)); \
	saveregs(); \
	if (!Yap_IUnify(ARG2, cl->ClSource->Entry)) { \
	  setregs(); \
	  UNLOCKPE(8,PP); \
	  FAIL(); \
	} \
	if (!Yap_IUnify(ARG3, MkDBRefTerm((DBRef)cl))) { \
	  setregs(); \
	  UNLOCKPE(9,PP); \
	  FAIL(); \
	} \
	setregs(); \
	if (!(cl->ClFlags & InUseMask)) { \
	  cl->ClFlags |= InUseMask; \
	  TRAIL_CLREF(cl); \
	} \
      } \
      (*_PREG) = (*_C(*_PREG)); \
      YREG = ENV; \
      JMPNext();
#endif /* DEPTH_LIMIT  */
#endif /* MULTIPLE_STACKS */
#endif /* defined(YAPOR) || defined(THREADS) */

#define UNIFY_IDB_TERM_END \
      BLOCK = (CELL)UNIFY_IDB_TERM_END;

#ifdef DEPTH_LIMIT
#define ENSURE_SPACE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      { \
	Int sz =  (*_PREG)->u.Osbpa.i; \
	UInt arity = (*_PREG)->u.Osbpa.p->ArityOfPE; \
	if (Unsigned(HR) + sz > Unsigned(YREG)-CreepFlag) { \
	  YENV[E_CP] = (CELL) (*_C(*_PREG)); \
	  YENV[E_E] = (CELL) ENV; \
	  YENV[E_DEPTH] = DEPTH; \
	  SET_ASP(YREG, (*_PREG)->u.Osbpa.s); \
	  (*_PREG) = NEXTOP((*_PREG),Osbpa); \
	  saveregs(); \
	  setregs(); \
	} else { \
	  (*_PREG) = NEXTOP((*_PREG),Osbpa); \
	} \
      } \
      JMPNext();
#else /* DEPTH_LIMIT */
#define ENSURE_SPACE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      { \
	Int sz =  (*_PREG)->u.Osbpa.i; \
	UInt arity = (*_PREG)->u.Osbpa.p->ArityOfPE; \
	if (Unsigned(HR) + sz > Unsigned(YREG)-CreepFlag) { \
	  YENV[E_CP] = (CELL) (*_C(*_PREG)); \
	  YENV[E_E] = (CELL) ENV; \
	  SET_ASP(YREG, (*_PREG)->u.Osbpa.s); \
	  (*_PREG) = NEXTOP((*_PREG),Osbpa); \
	  saveregs(); \
	  setregs(); \
	} else { \
	  (*_PREG) = NEXTOP((*_PREG),Osbpa); \
	} \
      } \
      JMPNext();
#endif /* DEPTH_LIMIT */

#define ENSURE_SPACE_END

#define JUMP_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      (*_PREG) = (*_PREG)->u.l.l; \
      JMPNext();

#define MOVE_BACK_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      (*_PREG) = (yamop *)(((char *)(*_PREG))-(Int)(NEXTOP((yamop *)NULL,Osbpp))); \
      JMPNext();

#define SKIP_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      (*_PREG) = NEXTOP((*_PREG),l); \
      JMPNext();

#define EITHER_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#ifdef LOW_LEVEL_TRACER
#define EITHER_LOW_LEVEL_TRACER \
	low_level_trace(try_or, (PredEntry *)(*_PREG), NULL);
#endif

#define EITHER_POST_COROUTINING \
      register CELL d0; \
	  register choiceptr pt1; \
      d0 = (*_PREG)->u.Osblp.s; \
      pt1 = (choiceptr) ((char *) YREG + (yslot) d0);
     
#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define EITHER_FROZEN_YSBA \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (pt1 > top_b || pt1 < (choiceptr)H) \
	  pt1 = top_b;
#else /* YAPOR_SBA */
#define EITHER_FROZEN_YSBA \
	choiceptr top_b = PROTECT_FROZEN_B(B); \
	if (pt1 > top_b) \
	  pt1 = top_b;
#endif /* YAPOR_SBA */
#else /* FROZEN_STACKS */
#define EITHER_FROZEN_YSBA \
      if (pt1 > B) \
	pt1 = B;
#endif /* FROZEN_STACKS */

#define EITHER_POST_FROZEN_YSBA \
      pt1 = (choiceptr)(((CELL *) pt1)-1); \
      *(CELL **) pt1 = YREG; \
      store_yaam_regs_for_either((*_PREG)->u.Osblp.l, (*_PREG)); \
      (*_SREG) = (CELL *) (B = pt1);
	  
#ifdef YAPOR
#define EITHER_YAPOR \
      SCH_set_load(pt1);
#endif

#define EITHER_END \
      SET_BB(pt1); \
      (*_PREG) = NEXTOP(NEXTOP((*_PREG), Osblp),l); \
      GONext();

#define OR_ELSE_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      HR = HRBREG = PROTECT_FROZEN_H(B); \
      ENV = B->cp_env; \
      B->cp_cp = (*_PREG);
	  
#ifdef DEPTH_LIMIT
#define OR_ELSE_DEPTH \
      DEPTH = B->cp_depth;
#endif

#define OR_ELSE_POST_DEPTH \
      SET_BB(PROTECT_FROZEN_B(B));
	  
#ifdef YAPOR
#define OR_ELSE_YAPOR \
	SCH_new_alternative((*_PREG), (*_PREG)->u.Osblp.l);
#endif

#define OR_ELSE_END \
      B->cp_ap = (*_PREG)->u.Osblp.l; \
      (*_PREG) = NEXTOP((*_PREG), Osblp); \
      YREG = (CELL *) B->cp_a1; \
      GONext();

#define OR_LAST_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register choiceptr pt0; \
      pt0 = B;
	  
#ifdef YAPOR
#define OR_LAST_IFOK_INIT \
	H = HRBREG = PROTECT_FROZEN_H(pt0); \
	YREG = (CELL *) pt0->cp_a1; \
	ENV = pt0->cp_env;
	
#ifdef DEPTH_LIMIT
#define OR_LAST_IFOK_DEPTH \
	DEPTH = pt0->cp_depth;
#endif

#define OR_LAST_IFOK_END \
        SCH_new_alternative((*_PREG), NULL);
#endif

#define OR_LAST_NOIF_INIT \
	B = pt0->cp_b; \
	H = PROTECT_FROZEN_H(pt0); \
	YREG = (CELL *) pt0->cp_a1; \
	ENV = pt0->cp_env;
	
#ifdef DEPTH_LIMIT
#define OR_LAST_NOIF_DEPTH \
	DEPTH = pt0->cp_depth;
#endif

#define OR_LAST_NOIF_END \
	HBREG = PROTECT_FROZEN_H(B);
#ifdef YAPOR

#define OR_LAST_YAPOR \
      (*_PREG) = NEXTOP((*_PREG), Osblp);
#else

#define OR_LAST_NOYAPOR \
      (*_PREG) = NEXTOP((*_PREG), p);
#endif

#define OR_LAST_END \
      SET_BB(PROTECT_FROZEN_B(B)); \
      GONext();

#define LOCK_PRED_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	PredEntry *ap = PredFromDefCode((*_PREG)); \
 	PELOCK(10,ap); \
	PP = ap;

#define LOCK_PRED_FIRSTIFOK \
	  UNLOCKPE(11,ap); \
	  FAIL();

#define LOCK_PRED_SECONDTIFOK \
	  SET_ASP(YREG, E_CB*sizeof(CELL)); \
	  saveregs(); \
	  Yap_IPred(ap, 0, CP); \
	  setregs(); \
	  CACHE_A1(); \
	  save_pc();

#define LOCK_PRED_END \
	(*_PREG) = ap->TrueCodeOfPred; \
      JMPNext();

#if defined(YAPOR) || defined(THREADS)
#define INDEX_PRED_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	PredEntry *ap = PredFromDefCode((*_PREG)); \
	if (!PP) { \
	  PELOCK(11,ap); \
	} \
	if (ap->OpcodeOfPred != INDEX_OPCODE) { \
	  if (!PP) { \
	    UNLOCKPE(11,ap); \
	  } \
	  (*_PREG) = ap->CodeOfPred; \
	  save_pc(); \
	  JMPNext(); \
	} \
	else \
    { \
	  SET_ASP(YREG, E_CB*sizeof(CELL)); \
	  saveregs(); \
	  Yap_IPred(ap, 0, CP); \
	  setregs(); \
	  CACHE_A1(); \
	  (*_PREG) = ap->CodeOfPred; \
	  save_pc(); \
	  if (!PP) \
      { \
	    UNLOCKPE(14,ap); \
      } \
      JMPNext(); \
	}
#else /* defined(YAPOR) || defined(THREADS) */
#define INDEX_PRED_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	PredEntry *ap = PredFromDefCode((*_PREG)); \
    { \
	  SET_ASP(YREG, E_CB*sizeof(CELL)); \
	  saveregs(); \
	  Yap_IPred(ap, 0, CP); \
	  setregs(); \
	  CACHE_A1(); \
	  (*_PREG) = ap->CodeOfPred; \
	  save_pc(); \
      { \
	    UNLOCKPE(14,ap); \
      } \
      JMPNext(); \
	}
#endif /* defined(YAPOR) || defined(THREADS) */

#define INDEX_PRED_END \
      BLOCK = (CELL)INDEX_PRED_END;

#if THREADS
#define THREAD_LOCAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	PredEntry *ap = PredFromDefCode((*_PREG)); \
	ap = Yap_GetThreadPred(ap PASS_REGS); \
	(*_PREG) = ap->CodeOfPred; \
	save_pc(); \
      JMPNext();
#endif

#define EXPAND_INDEX_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	PredEntry *pe = PredFromExpandCode((*_PREG)); \
	yamop *pt0; \
	SET_ASP(YREG, E_CB*sizeof(CELL));
	
#if defined(YAPOR) || defined(THREADS)
#define EXPAND_INDEX_YAPOR_THREADS_NOPP \
	  PELOCK(12,pe);

#define EXPAND_INDEX_YAPOR_THREADS_IFOK_INIT \
	  (*_PREG) = *PREG_ADDR;

#define EXPAND_INDEX_YAPOR_THREADS_IFOK_IFOK \
	    UNLOCKPE(15,pe);

#define EXPAND_INDEX_YAPOR_THREADS_IFOK_END \
	  JMPNext();
#endif

#ifdef SHADOW_S
#define EXPAND_INDEX_NOYAPOR_NOTHREADS_SETS \
	S = (*_SREG);
#endif

#define EXPAND_INDEX_NOYAPOR_NOTHREADS_POST_SETS \
 	saveregs(); \
	pt0 = Yap_ExpandIndex(pe, 0); \
	setregs();
	
#ifdef SHADOW_S
#define EXPAND_INDEX_NOYAPOR_NOTHREADS_SETSREG \
	(*_SREG) = S;
#endif

#define EXPAND_INDEX_NOYAPOR_NOTHREADS_POST_SETSREG \
 	(*_PREG) = pt0;
	
#if defined(YAPOR) || defined(THREADS)
#define EXPAND_INDEX_UNLOCK \
	  UNLOCKPE(12,pe);
#endif

#define EXPAND_INDEX_END \
	JMPNext();

#define EXPAND_CLAUSES_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	PredEntry *pe = (*_PREG)->u.sssllp.p; \
	yamop *pt0; \
	SET_ASP(YREG, E_CB*sizeof(CELL));
	
#if defined(YAPOR) || defined(THREADS)
#define EXPAND_CLAUSES_YAPOR_THREADS_NOPP \
	  PELOCK(13,pe);

#define EXPAND_CLAUSES_YAPOR_THREADS_IFOK_INIT \
	  (*_PREG) = *PREG_ADDR;

#define EXPAND_CLAUSES_YAPOR_THREADS_IFOK_IFOK \
	    UNLOCKPE(16,pe);

#define EXPAND_CLAUSES_YAPOR_THREADS_IFOK_END \
	  JMPNext();
#endif

#define EXPAND_CLAUSES_NOYAPOR_NOTHREADS \
 	saveregs(); \
	pt0 = Yap_ExpandIndex(pe, 0); \
	setregs(); \
	UNLOCKPE(17,pe); \
 	(*_PREG) = pt0;
	
#if defined(YAPOR) || defined(THREADS)
#define EXPAND_CLAUSES_UNLOCK \
	  UNLOCKPE(18,pe);
#endif

#define EXPAND_CLAUSES_END \
	JMPNext();

#if defined(YAPOR) || defined(THREADS)
#ifdef LOW_LEVEL_TRACER
#define UNDEF_P_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	PredEntry *pe = PredFromDefCode((*_PREG)); \
	register CELL d0, d1; \
	register CELL *pt0, *pt1; \
	if ((pe->PredFlags & (DynamicPredFlag|LogUpdatePredFlag|MultiFileFlag)) || \
	    (UndefCode->OpcodeOfPred == UNDEF_OPCODE)) { \
	  PP = NULL; \
	  UNLOCKPE(19,pe); \
	  YAAM_FAIL; \
	} \
	else { \
	  d0 = pe->ArityOfPE; \
	  UNLOCKPE(19,pe); \
	  if (d0 == 0) { \
	    HR[1] = MkAtomTerm((Atom)(pe->FunctorOfPred)); \
	  } \
	  else { \
	    HR[d0 + 2] = AbsAppl(HR); \
	    *H = (CELL) pe->FunctorOfPred; \
	    HR++; \
	    pt1 = XREGS + 1; \
	    for (; d0 > 0; --d0) { \
	      pt0 = pt1++; \
	      d1 = *pt0; \
		  if (IsVarTerm(d1)) { \
		    short setHwithd1 = 0; \
		    while (Unsigned(pt0) != (d1)) { \
		      (pt0) = (CELL *)(d1); \
              (d1) = *(CELL *)(d1); \
			  if(!IsVarTerm(d1)) { \
			    *H++ = d1; \
			    setHwithd1 = 1; \
			    break; \
			  } \
		    } \
		    if (setHwithd1) { continue; } \
		    if (pt0 <= HR) { \
	          *H++ = (CELL)pt0; \
	        } else { \
	          d1 = Unsigned(HR); \
	          RESET_VARIABLE(HR); \
	          HR += 1; \
	          Bind_Local(pt0, d1); \
	        } \
		  } \
		  else { \
	        *H++ = d1; \
		  } \
	    } \
	  } \
	  HR[0] = Yap_Module_Name(pe); \
	  ARG1 = (Term) AbsPair(HR); \
	  HR += 2; \
        if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,UndefCode,XREGS+1); \
	  } \
      (*_PREG) = UndefCode->CodeOfPred; \
      save_pc(); \
      CACHE_A1(); \
      JMPNext(); \
	}
#else /* LOW_LEVEL_TRACER */
#define UNDEF_P_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	PredEntry *pe = PredFromDefCode((*_PREG)); \
	register CELL d0, d1; \
	register CELL *pt0, *pt1; \
	if ((pe->PredFlags & (DynamicPredFlag|LogUpdatePredFlag|MultiFileFlag)) || \
	    (UndefCode->OpcodeOfPred == UNDEF_OPCODE)) { \
	  PP = NULL; \
	  UNLOCKPE(19,pe); \
	  YAAM_FAIL; \
	} \
	else { \
	  d0 = pe->ArityOfPE; \
	  UNLOCKPE(19,pe); \
	  if (d0 == 0) { \
	    HR[1] = MkAtomTerm((Atom)(pe->FunctorOfPred)); \
	  } \
	  else { \
	    HR[d0 + 2] = AbsAppl(HR); \
	    *H = (CELL) pe->FunctorOfPred; \
	    HR++; \
	    pt1 = XREGS + 1; \
	    for (; d0 > 0; --d0) { \
	      pt0 = pt1++; \
	      d1 = *pt0; \
		  if (IsVarTerm(d1)) { \
		    short setHwithd1 = 0; \
		    while (Unsigned(pt0) != (d1)) { \
		      (pt0) = (CELL *)(d1); \
              (d1) = *(CELL *)(d1); \
			  if(!IsVarTerm(d1)) { \
			    *H++ = d1; \
			    setHwithd1 = 1; \
			    break; \
			  } \
		    } \
		    if (setHwithd1) { continue; } \
		    if (pt0 <= HR) { \
	          *H++ = (CELL)pt0; \
	        } else { \
	          d1 = Unsigned(HR); \
	          RESET_VARIABLE(HR); \
	          HR += 1; \
	          Bind_Local(pt0, d1); \
	        } \
		  } \
		  else { \
	        *H++ = d1; \
		  } \
	    } \
	  } \
	  HR[0] = Yap_Module_Name(pe); \
	  ARG1 = (Term) AbsPair(HR); \
	  HR += 2; \
      (*_PREG) = UndefCode->CodeOfPred; \
      save_pc(); \
      CACHE_A1(); \
      JMPNext(); \
	}
#endif /* LOW_LEVEL_TRACER */
#else /* defined(YAPOR) || defined(THREADS) */
#ifdef LOW_LEVEL_TRACER
#define UNDEF_P_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
      FAILED = 0; \
	PredEntry *pe = PredFromDefCode((*_PREG)); \
	register CELL d0, d1; \
	register CELL *pt0, *pt1; \
	if ((pe->PredFlags & (DynamicPredFlag|LogUpdatePredFlag|MultiFileFlag)) || \
	    (UndefCode->OpcodeOfPred == UNDEF_OPCODE)) { \
	  UNLOCKPE(19,pe); \
	  YAAM_FAIL; \
	} \
	else { \
	  d0 = pe->ArityOfPE; \
	  UNLOCKPE(19,pe); \
	  if (d0 == 0) { \
	    HR[1] = MkAtomTerm((Atom)(pe->FunctorOfPred)); \
	  } \
	  else { \
	    HR[d0 + 2] = AbsAppl(HR); \
	    *H = (CELL) pe->FunctorOfPred; \
	    HR++; \
	    pt1 = XREGS + 1; \
	    for (; d0 > 0; --d0) { \
	      pt0 = pt1++; \
	      d1 = *pt0; \
		  if (IsVarTerm(d1)) { \
		    short setHwithd1 = 0; \
		    while (Unsigned(pt0) != (d1)) { \
		      (pt0) = (CELL *)(d1); \
              (d1) = *(CELL *)(d1); \
			  if(!IsVarTerm(d1)) { \
			    *H++ = d1; \
			    setHwithd1 = 1; \
			    break; \
			  } \
		    } \
		    if (setHwithd1) { continue; } \
		    if (pt0 <= HR) { \
	          *H++ = (CELL)pt0; \
	        } else { \
	          d1 = Unsigned(HR); \
	          RESET_VARIABLE(HR); \
	          HR += 1; \
	          Bind_Local(pt0, d1); \
	        } \
		  } \
		  else { \
	        *H++ = d1; \
		  } \
	    } \
	  } \
	  HR[0] = Yap_Module_Name(pe); \
	  ARG1 = (Term) AbsPair(HR); \
	  HR += 2; \
        if (Yap_do_low_level_trace) { \
	  low_level_trace(enter_pred,UndefCode,XREGS+1); \
	  } \
      (*_PREG) = UndefCode->CodeOfPred; \
      save_pc(); \
      CACHE_A1(); \
      JMPNext(); \
	}
#else /* LOW_LEVEL_TRACER */
#define UNDEF_P_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
      FAILED = 0; \
	PredEntry *pe = PredFromDefCode((*_PREG)); \
	register CELL d0, d1; \
	register CELL *pt0, *pt1; \
	if ((pe->PredFlags & (DynamicPredFlag|LogUpdatePredFlag|MultiFileFlag)) || \
	    (UndefCode->OpcodeOfPred == UNDEF_OPCODE)) { \
	  UNLOCKPE(19,pe); \
	  YAAM_FAIL; \
	} \
	else { \
	  d0 = pe->ArityOfPE; \
	  UNLOCKPE(19,pe); \
	  if (d0 == 0) { \
	    HR[1] = MkAtomTerm((Atom)(pe->FunctorOfPred)); \
	  } \
	  else { \
	    HR[d0 + 2] = AbsAppl(HR); \
	    *H = (CELL) pe->FunctorOfPred; \
	    HR++; \
	    pt1 = XREGS + 1; \
	    for (; d0 > 0; --d0) { \
	      pt0 = pt1++; \
	      d1 = *pt0; \
		  if (IsVarTerm(d1)) { \
		    short setHwithd1 = 0; \
		    while (Unsigned(pt0) != (d1)) { \
		      (pt0) = (CELL *)(d1); \
              (d1) = *(CELL *)(d1); \
			  if(!IsVarTerm(d1)) { \
			    *H++ = d1; \
			    setHwithd1 = 1; \
			    break; \
			  } \
		    } \
		    if (setHwithd1) { continue; } \
		    if (pt0 <= HR) { \
	          *H++ = (CELL)pt0; \
	        } else { \
	          d1 = Unsigned(HR); \
	          RESET_VARIABLE(HR); \
	          HR += 1; \
	          Bind_Local(pt0, d1); \
	        } \
		  } \
		  else { \
	        *H++ = d1; \
		  } \
	    } \
	  } \
	  HR[0] = Yap_Module_Name(pe); \
	  ARG1 = (Term) AbsPair(HR); \
	  HR += 2; \
      (*_PREG) = UndefCode->CodeOfPred; \
      save_pc(); \
      CACHE_A1(); \
      JMPNext(); \
	}
#endif /* LOW_LEVEL_TRACER */
#endif /* defined(YAPOR) || defined(THREADS) */

#define UNDEF_P_END \
      BLOCK = (CELL)UNDEF_P_END;

#define SPY_PRED_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	register CELL d0, d1; \
	register CELL *pt0, *pt1; \
	PredEntry *pe = PredFromDefCode((*_PREG)); \
 	PELOCK(14,pe);

#define SPY_PRED_FIRSTIFOK \
	  SET_ASP(YREG, E_CB*sizeof(CELL)); \
	  saveregs(); \
	  Yap_IPred(pe, 0, CP); \
	  setregs();

#define SPY_PRED_SECONDIFOK_INIT \
	  LOCK(pe->StatisticsForPred->lock); \
	  pe->StatisticsForPred->NOfEntries++; \
	  UNLOCK(pe->StatisticsForPred->lock); \
	  LOCAL_ReductionsCounter--;

#define SPY_PRED_SECONDIFOK_FIRSTIFOK \
	    UNLOCKPE(20,pe); \
	    saveregs(); \
	    Yap_NilError(CALL_COUNTER_UNDERFLOW,""); \
	    setregs(); \
	    JMPNext();

#define SPY_PRED_SECONDIFOK_POST_FIRSTIF \
	  LOCAL_PredEntriesCounter--;

#define SPY_PRED_SECONDIFOK_SECONDIFOK \
	    UNLOCKPE(21,pe); \
	    saveregs(); \
	    Yap_NilError(PRED_ENTRY_COUNTER_UNDERFLOW_EVENT,""); \
	    setregs(); \
	    JMPNext();

#define SPY_PRED_SECONDIFOK_THIRDIFOK \
	    (*_PREG) = pe->TrueCodeOfPred; \
	    UNLOCKPE(22,pe); \
	    JMPNext();

#define SPY_PRED_THIRDIFOK_INIT \
	  LOCK(pe->StatisticsForPred->lock); \
	  pe->StatisticsForPred->NOfEntries++; \
	  UNLOCK(pe->StatisticsForPred->lock);
	  
#define SPY_PRED_THIRDIFOK_FIRSTIFOK \
	    (*_PREG) = pe->TrueCodeOfPred; \
	    UNLOCKPE(23,pe); \
	    JMPNext();

#define SPY_PRED_FOURTHIFOK \
	  (*_PREG) = pe->TrueCodeOfPred; \
	  UNLOCKPE(24,pe); \
	  JMPNext();

#define SPY_PRED_POST_FOURTHIF \
	UNLOCKPE(25,pe); \
	d0 = pe->ArityOfPE;

#define SPY_PRED_D0ISZERO \
	  HR[1] = MkAtomTerm((Atom)(pe->FunctorOfPred));

#define SPY_PRED_D0ISNOZERO_INIT \
	  *H = (CELL) pe->FunctorOfPred; \
	  HR[d0 + 2] = AbsAppl(HR); \
	  HR++; \
	  pt1 = XREGS + 1;

#define SPY_PRED_D0ISNOZERO_INSIDEFOR_INIT \
	    pt0 = pt1++; \
	    d1 = *pt0;

#define SPY_PRED_D0ISNOZERO_INSIDEFOR_DOSPY_NONVAR \
	    *H++ = d1;

#define SPY_PRED_D0ISNOZERO_INSIDEFOR_SAFEVAR \
	      *H++ = (CELL)pt0;

#define SPY_PRED_D0ISNOZERO_INSIDEFOR_UNSAFEVAR \
	      d1 = Unsigned(HR); \
	      RESET_VARIABLE(HR); \
	      HR += 1; \
	      Bind_Local(pt0, d1);

#define SPY_PRED_POST_IFS \
	H[0] = Yap_Module_Name(pe); \
      ARG1 = (Term) AbsPair(HR); \
      HR += 2; \
	PredEntry *pt0;
	
#ifdef THREADS
#define SPY_PRED_THREADS_LOCK \
	LOCK(GLOBAL_ThreadHandlesLock);
#endif

#define SPY_PRED_POST_LOCK \
	pt0 = SpyCode; \
	P_before_spy = (*_PREG); \
	(*_PREG) = pt0->CodeOfPred;
	
#ifdef THREADS
#define SPY_PRED_THREADS_UNLOCK \
	UNLOCK(GLOBAL_ThreadHandlesLock);
#endif

#define SPY_PRED_POST_UNLOCK \
	save_pc(); \
	CACHE_A1();
	
#ifdef LOW_LEVEL_TRACER
#define SPY_PRED_LOW_LEVEL_TRACER \
	  low_level_trace(enter_pred,pt0,XREGS+1);
#endif

#define SPY_PRED_END \
      JMPNext();

