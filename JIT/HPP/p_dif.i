#define P_DIF_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \

#ifdef LOW_LEVEL_TRACER
#define P_DIF_LOW_LEVEL_TRACER \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorDiff,0)),XREGS+1);
#endif

#define P_DIF_POST_LOW_LEVEL_TRACER \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = ARG1;

#define P_DIF_DIF_NVAR1 \
      d1 = ARG2;

#ifdef COROUTINING
#if defined(YAPOR_SBA) && defined(YAPOR)
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define P_DIF_DIF_NVAR1_NVAR2 \
    BLOCK = (CELL)P_DIF_DIF_NVAR1_NVAR2; \
    if (d0 == d1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  GONext(); \
    } \
    else { \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	    (*_PREG) = NEXTOP((*_PREG), l); \
	    GONext(); \
      } \
      else { \
	    Int opresult; \
	    CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	    register tr_fr_ptr pt0; \
	    pt0 = TR; \
	    BEGCHO(pt1); \
	    pt1 = B; \
	    HRBREG = HR; \
	    B = (choiceptr) HR; \
	    B->cp_h = HR; \
	    SET_BB(B); \
	    save_hb(); \
	    opresult = Yap_IUnify(d0, d1); \
	    Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	    if (OldWokenGoals == TermNil) { \
	      Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	    }\
	    B = pt1; \
	    SET_BB(PROTECT_FROZEN_B(pt1)); \
	    HR = HRBREG; \
	    HRBREG = B->cp_h; \
	    while (TR != pt0) { \
	      d1 = TrailTerm(--TR); \
	      if (IsVarTerm(d1)) { \
	        if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		    Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	          RESET_VARIABLE(STACK_TO_SBA(d1)); \
	        } else \
	          RESET_VARIABLE(d1); \
	      } else { \
	        CELL *pt = RepAppl(d1); \
	        pt[0] = TrailVal(--TR); \
	      } \
	    } \
	    if (opresult) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
          GONext(); \
        } \
        ENDCHO(pt1); \
      } \
    }
#else /* FROZEN_STACKS */
#define P_DIF_DIF_NVAR1_NVAR2 \
    BLOCK = (CELL)P_DIF_DIF_NVAR1_NVAR2; \
    if (d0 == d1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  GONext(); \
    } \
    else { \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	    (*_PREG) = NEXTOP((*_PREG), l); \
	    GONext(); \
      } \
      else { \
	    Int opresult; \
	    CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	    register tr_fr_ptr pt0; \
	    pt0 = TR; \
	    BEGCHO(pt1); \
	    pt1 = B; \
	    HRBREG = HR; \
	    B = (choiceptr) HR; \
	    B->cp_h = HR; \
	    SET_BB(B); \
	    save_hb(); \
	    opresult = Yap_IUnify(d0, d1); \
	    Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	    if (OldWokenGoals == TermNil) { \
	      Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	    }\
	    B = pt1; \
	    SET_BB(PROTECT_FROZEN_B(pt1)); \
	    HR = HRBREG; \
	    HRBREG = B->cp_h; \
	    while (TR != pt0) { \
	      d1 = TrailTerm(--TR); \
	      if (IsVarTerm(d1)) { \
	        if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		    Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	          RESET_VARIABLE(STACK_TO_SBA(d1)); \
	        } else \
	          RESET_VARIABLE(d1); \
	      } else { \
	        CELL *pt = RepAppl(d1); \
	        pt[0] = TrailTerm(--TR); \
	        TR--; \
	      } \
	    } \
	    if (opresult) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
          GONext(); \
        } \
        ENDCHO(pt1); \
      } \
    }
#endif /* FROZEN_STACKS */
#else /*MULTI_ASSIGNMENT_VARIABLES */
#define P_DIF_DIF_NVAR1_NVAR2 \
    BLOCK = (CELL)P_DIF_DIF_NVAR1_NVAR2; \
    if (d0 == d1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  GONext(); \
    } \
    else { \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	    (*_PREG) = NEXTOP((*_PREG), l); \
	    GONext(); \
      } \
      else { \
	    Int opresult; \
	    CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	    register tr_fr_ptr pt0; \
	    pt0 = TR; \
	    BEGCHO(pt1); \
	    pt1 = B; \
	    HRBREG = HR; \
	    B = (choiceptr) HR; \
	    B->cp_h = HR; \
	    SET_BB(B); \
	    save_hb(); \
	    opresult = Yap_IUnify(d0, d1); \
	    Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	    if (OldWokenGoals == TermNil) { \
	      Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	    }\
	    B = pt1; \
	    SET_BB(PROTECT_FROZEN_B(pt1)); \
	    HR = HRBREG; \
	    HRBREG = B->cp_h; \
	    while (TR != pt0) { \
	      d1 = TrailTerm(--TR); \
	      if (IsVarTerm(d1)) { \
	        if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		    Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	          RESET_VARIABLE(STACK_TO_SBA(d1)); \
	        } else \
	          RESET_VARIABLE(d1); \
	      } \
	    } \
	    if (opresult) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
          GONext(); \
        } \
        ENDCHO(pt1); \
      } \
    }
#endif /*MULTI_ASSIGNMENT_VARIABLES */
#else /* defined(YAPOR_SBA) && defined(YAPOR) */
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define P_DIF_DIF_NVAR1_NVAR2 \
    BLOCK = (CELL)P_DIF_DIF_NVAR1_NVAR2; \
    if (d0 == d1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  GONext(); \
    } \
    else { \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	    (*_PREG) = NEXTOP((*_PREG), l); \
	    GONext(); \
      } \
      else { \
	    Int opresult; \
	    CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	    register tr_fr_ptr pt0; \
	    pt0 = TR; \
	    BEGCHO(pt1); \
	    pt1 = B; \
	    HRBREG = HR; \
	    B = (choiceptr) HR; \
	    B->cp_h = HR; \
	    SET_BB(B); \
	    save_hb(); \
	    opresult = Yap_IUnify(d0, d1); \
	    Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	    if (OldWokenGoals == TermNil) { \
	      Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	    } \
	    B = pt1; \
	    SET_BB(PROTECT_FROZEN_B(pt1)); \
	    HR = HRBREG; \
	    HRBREG = B->cp_h; \
	    while (TR != pt0) { \
	      d1 = TrailTerm(--TR); \
	      if (IsVarTerm(d1)) { \
	          RESET_VARIABLE(d1); \
	      } else { \
	        CELL *pt = RepAppl(d1); \
	        pt[0] = TrailVal(--TR); \
	      } \
	    } \
	    if (opresult) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
          GONext(); \
        } \
        ENDCHO(pt1); \
      } \
    }
#else /* FROZEN_STACKS */
#define P_DIF_DIF_NVAR1_NVAR2 \
    BLOCK = (CELL)P_DIF_DIF_NVAR1_NVAR2; \
    if (d0 == d1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  GONext(); \
    } \
    else { \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	    (*_PREG) = NEXTOP((*_PREG), l); \
	    GONext(); \
      } \
      else { \
	    Int opresult; \
	    CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	    register tr_fr_ptr pt0; \
	    pt0 = TR; \
	    BEGCHO(pt1); \
	    pt1 = B; \
	    HRBREG = HR; \
	    B = (choiceptr) HR; \
	    B->cp_h = HR; \
	    SET_BB(B); \
	    save_hb(); \
	    opresult = Yap_IUnify(d0, d1); \
	    Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	    if (OldWokenGoals == TermNil) { \
	      Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	    }\
	    B = pt1; \
	    SET_BB(PROTECT_FROZEN_B(pt1)); \
	    HR = HRBREG; \
	    HRBREG = B->cp_h; \
	    while (TR != pt0) { \
	      d1 = TrailTerm(--TR); \
	      if (IsVarTerm(d1)) { \
	          RESET_VARIABLE(d1); \
	      } else { \
	        CELL *pt = RepAppl(d1); \
	        pt[0] = TrailTerm(--TR); \
	        TR--; \
	      } \
	    } \
	    if (opresult) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
          GONext(); \
        } \
        ENDCHO(pt1); \
      } \
    }
#endif /* FROZEN_STACKS */
#else /*MULTI_ASSIGNMENT_VARIABLES */
#define P_DIF_DIF_NVAR1_NVAR2 \
    BLOCK = (CELL)P_DIF_DIF_NVAR1_NVAR2; \
    if (d0 == d1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  GONext(); \
    } \
    else { \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	    (*_PREG) = NEXTOP((*_PREG), l); \
	    GONext(); \
      } \
      else { \
	    Int opresult; \
	    CELL OldWokenGoals = Yap_ReadTimedVar(LOCAL_WokenGoals); \
	    register tr_fr_ptr pt0; \
	    pt0 = TR; \
	    BEGCHO(pt1); \
	    pt1 = B; \
	    HRBREG = HR; \
	    B = (choiceptr) HR; \
	    B->cp_h = HR; \
	    SET_BB(B); \
	    save_hb(); \
	    opresult = Yap_IUnify(d0, d1); \
	    Yap_UpdateTimedVar(LOCAL_WokenGoals, OldWokenGoals); \
	    if (OldWokenGoals == TermNil) { \
	      Yap_undo_signal(YAP_WAKEUP_SIGNAL); \
	    }\
	    B = pt1; \
	    SET_BB(PROTECT_FROZEN_B(pt1)); \
	    HR = HRBREG; \
	    HRBREG = B->cp_h; \
	    while (TR != pt0) { \
	      d1 = TrailTerm(--TR); \
	      if (IsVarTerm(d1)) { \
	          RESET_VARIABLE(d1); \
	      } \
	    } \
	    if (opresult) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
          GONext(); \
        } \
        ENDCHO(pt1); \
      } \
    }
#endif /*MULTI_ASSIGNMENT_VARIABLES */
#endif /* defined(YAPOR_SBA) && defined(YAPOR) */
#else /* COROUTINING */
#if defined(YAPOR_SBA) && defined(YAPOR)
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define P_DIF_DIF_NVAR1_NVAR2 \
    BLOCK = (CELL)P_DIF_DIF_NVAR1_NVAR2; \
    if (d0 == d1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  GONext(); \
    } \
    else { \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	    (*_PREG) = NEXTOP((*_PREG), l); \
	    GONext(); \
      } \
      else { \
	    Int opresult; \
	    register tr_fr_ptr pt0; \
	    pt0 = TR; \
	    BEGCHO(pt1); \
	    pt1 = B; \
	    HRBREG = HR; \
	    B = (choiceptr) HR; \
	    B->cp_h = HR; \
	    SET_BB(B); \
	    save_hb(); \
	    opresult = Yap_IUnify(d0, d1); \
	    B = pt1; \
	    SET_BB(PROTECT_FROZEN_B(pt1)); \
	    HRBREG = B->cp_h; \
	    while (TR != pt0) { \
	      d1 = TrailTerm(--TR); \
	      if (IsVarTerm(d1)) { \
	        if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		    Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	          RESET_VARIABLE(STACK_TO_SBA(d1)); \
	        } else \
	          RESET_VARIABLE(d1); \
	      } else { \
	        CELL *pt = RepAppl(d1); \
	        pt[0] = TrailVal(--TR); \
	      } \
	    } \
	    if (opresult) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
          GONext(); \
        } \
        ENDCHO(pt1); \
      } \
    }
#else /* FROZEN_STACKS */
#define P_DIF_DIF_NVAR1_NVAR2 \
    BLOCK = (CELL)P_DIF_DIF_NVAR1_NVAR2; \
    if (d0 == d1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  GONext(); \
    } \
    else { \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	    (*_PREG) = NEXTOP((*_PREG), l); \
	    GONext(); \
      } \
      else { \
	    Int opresult; \
	    register tr_fr_ptr pt0; \
	    pt0 = TR; \
	    BEGCHO(pt1); \
	    pt1 = B; \
	    HRBREG = HR; \
	    B = (choiceptr) HR; \
	    B->cp_h = HR; \
	    SET_BB(B); \
	    save_hb(); \
	    opresult = Yap_IUnify(d0, d1); \
	    B = pt1; \
	    SET_BB(PROTECT_FROZEN_B(pt1)); \
	    HRBREG = B->cp_h; \
	    while (TR != pt0) { \
	      d1 = TrailTerm(--TR); \
	      if (IsVarTerm(d1)) { \
	        if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		    Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	          RESET_VARIABLE(STACK_TO_SBA(d1)); \
	        } else \
	          RESET_VARIABLE(d1); \
	      } else { \
	        CELL *pt = RepAppl(d1); \
	        pt[0] = TrailTerm(--TR); \
	        TR--; \
	      } \
	    } \
	    if (opresult) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
          GONext(); \
        } \
        ENDCHO(pt1); \
      } \
    }
#endif /* FROZEN_STACKS */
#else /*MULTI_ASSIGNMENT_VARIABLES */
#define P_DIF_DIF_NVAR1_NVAR2 \
    BLOCK = (CELL)P_DIF_DIF_NVAR1_NVAR2; \
    if (d0 == d1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  GONext(); \
    } \
    else { \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	    (*_PREG) = NEXTOP((*_PREG), l); \
	    GONext(); \
      } \
      else { \
	    Int opresult; \
	    register tr_fr_ptr pt0; \
	    pt0 = TR; \
	    BEGCHO(pt1); \
	    pt1 = B; \
	    HRBREG = HR; \
	    B = (choiceptr) HR; \
	    B->cp_h = HR; \
	    SET_BB(B); \
	    save_hb(); \
	    opresult = Yap_IUnify(d0, d1); \
	    B = pt1; \
	    SET_BB(PROTECT_FROZEN_B(pt1)); \
	    HRBREG = B->cp_h; \
	    while (TR != pt0) { \
	      d1 = TrailTerm(--TR); \
	      if (IsVarTerm(d1)) { \
	        if (Unsigned((Int)(d1)-(Int)(H_FZ)) > \
		    Unsigned((Int)(B_FZ)-(Int)(H_FZ))) { \
	          RESET_VARIABLE(STACK_TO_SBA(d1)); \
	        } else \
	          RESET_VARIABLE(d1); \
	      } \
	    } \
	    if (opresult) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
          GONext(); \
        } \
        ENDCHO(pt1); \
      } \
    }
#endif /*MULTI_ASSIGNMENT_VARIABLES */
#else /* defined(YAPOR_SBA) && defined(YAPOR) */
#ifdef MULTI_ASSIGNMENT_VARIABLES
#ifdef FROZEN_STACKS
#define P_DIF_DIF_NVAR1_NVAR2 \
    BLOCK = (CELL)P_DIF_DIF_NVAR1_NVAR2; \
    if (d0 == d1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  GONext(); \
    } \
    else { \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	    (*_PREG) = NEXTOP((*_PREG), l); \
	    GONext(); \
      } \
      else { \
	    Int opresult; \
	    register tr_fr_ptr pt0; \
	    pt0 = TR; \
	    BEGCHO(pt1); \
	    pt1 = B; \
	    HRBREG = HR; \
	    B = (choiceptr) HR; \
	    B->cp_h = HR; \
	    SET_BB(B); \
	    save_hb(); \
	    opresult = Yap_IUnify(d0, d1); \
	    B = pt1; \
	    SET_BB(PROTECT_FROZEN_B(pt1)); \
	    HRBREG = B->cp_h; \
	    while (TR != pt0) { \
	      d1 = TrailTerm(--TR); \
	      if (IsVarTerm(d1)) { \
	          RESET_VARIABLE(d1); \
	      } else { \
	        CELL *pt = RepAppl(d1); \
	        pt[0] = TrailVal(--TR); \
	      } \
	    } \
	    if (opresult) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
          GONext(); \
        } \
        ENDCHO(pt1); \
      } \
    }
#else /* FROZEN_STACKS */
#define P_DIF_DIF_NVAR1_NVAR2 \
    BLOCK = (CELL)P_DIF_DIF_NVAR1_NVAR2; \
    if (d0 == d1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  GONext(); \
    } \
    else { \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	    (*_PREG) = NEXTOP((*_PREG), l); \
	    GONext(); \
      } \
      else { \
	    Int opresult; \
	    register tr_fr_ptr pt0; \
	    pt0 = TR; \
	    BEGCHO(pt1); \
	    pt1 = B; \
	    HRBREG = HR; \
	    B = (choiceptr) HR; \
	    B->cp_h = HR; \
	    SET_BB(B); \
	    save_hb(); \
	    opresult = Yap_IUnify(d0, d1); \
	    B = pt1; \
	    SET_BB(PROTECT_FROZEN_B(pt1)); \
	    HRBREG = B->cp_h; \
	    while (TR != pt0) { \
	      d1 = TrailTerm(--TR); \
	      if (IsVarTerm(d1)) { \
	          RESET_VARIABLE(d1); \
	      } else { \
	        CELL *pt = RepAppl(d1); \
	        pt[0] = TrailTerm(--TR); \
	        TR--; \
	      } \
	    } \
	    if (opresult) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
          GONext(); \
        } \
        ENDCHO(pt1); \
      } \
    }
#endif /* FROZEN_STACKS */
#else /*MULTI_ASSIGNMENT_VARIABLES */
#define P_DIF_DIF_NVAR1_NVAR2 \
    BLOCK = (CELL)P_DIF_DIF_NVAR1_NVAR2; \
    if (d0 == d1) { \
	  (*_PREG) = (*_PREG)->u.l.l; \
	  GONext(); \
    } \
    else { \
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) { \
	    (*_PREG) = NEXTOP((*_PREG), l); \
	    GONext(); \
      } \
      else { \
	    Int opresult; \
	    register tr_fr_ptr pt0; \
	    pt0 = TR; \
	    BEGCHO(pt1); \
	    pt1 = B; \
	    HRBREG = HR; \
	    B = (choiceptr) HR; \
	    B->cp_h = HR; \
	    SET_BB(B); \
	    save_hb(); \
	    opresult = Yap_IUnify(d0, d1); \
	    B = pt1; \
	    SET_BB(PROTECT_FROZEN_B(pt1)); \
	    HRBREG = B->cp_h; \
	    while (TR != pt0) { \
	      d1 = TrailTerm(--TR); \
	      if (IsVarTerm(d1)) { \
	          RESET_VARIABLE(d1); \
	      } \
	    } \
	    if (opresult) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      (*_PREG) = NEXTOP((*_PREG), l); \
          GONext(); \
        } \
        ENDCHO(pt1); \
      } \
    }
#endif /*MULTI_ASSIGNMENT_VARIABLES */
#endif /* defined(YAPOR_SBA) && defined(YAPOR) */
#endif /* COROUTINING */

#define P_DIF_DIF_UNK1 \
      (*_PREG) = (*_PREG)->u.l.l; \
      GONext();

#define P_DIF_DIF_NVAR1_UNK2 \
      (*_PREG) = (*_PREG)->u.l.l; \
      GONext();
