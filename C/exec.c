/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		exec.c							 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	Execute Prolog code					 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "@(#)cdmgr.c	1.1 05/02/98";
#endif

#include "absmi.h"
#include "yapio.h"


STATIC_PROTO(Int  CallProlog, (PredEntry *, unsigned int, Int));
STATIC_PROTO(Int  p_save_cp, (void));
STATIC_PROTO(Int  p_execute, (void));
STATIC_PROTO(Int  p_execute0, (void));
STATIC_PROTO(Int  p_at_execute, (void));

static Int 
FastCallProlog(PredEntry *pen) {
#ifdef DEPTH_LIMIT
  if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is prolog */
    if (pen->ModuleOfPred) {
      if (DEPTH == MkIntTerm(0))
	return(FALSE);
      else DEPTH = RESET_DEPTH();
    }
  } else if (pen->ModuleOfPred)
    DEPTH -= MkIntConstant(2);
#endif	/* DEPTH_LIMIT */
#ifdef LOW_LEVEL_TRACER
  if (do_low_level_trace)
    low_level_trace(enter_pred,pen,XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
  if (pen->PredFlags & ProfiledPredFlag)
    pen->StatisticsForPred.NOfEntries++;
  CP = P;
  P = (yamop *)(pen->CodeOfPred);
  WRITE_UNLOCK(pen->PRWLock);
  ENV = YENV;
  YENV = ASP;
  YENV[E_CB] = (CELL) B;
  return (Unsigned(&(pen->StateOfPred)));
}

static Int 
CallProlog(PredEntry *pen, unsigned int arity, Int position)
{
  CELL            flags;


  WRITE_LOCK(pen->PRWLock);
  flags = pen->PredFlags;
  if ((flags & (CompiledPredFlag | DynamicPredFlag)) ||
      pen->OpcodeOfPred == UNDEF_OPCODE) {
    if (position == -1 ||
	pen->OpcodeOfPred == UNDEF_OPCODE) {
      return(FastCallProlog(pen));
    } else {
      CODEADDR        q;
#ifdef DEPTH_LIMIT
      if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is prolog */
	if (pen->ModuleOfPred) {
	  if (DEPTH == MkIntTerm(0))
	    return(FALSE);
	  else DEPTH = RESET_DEPTH();
	}
      } else if (pen->ModuleOfPred)
	DEPTH -= MkIntConstant(2);
#endif	/* DEPTH_LIMIT */
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace)
	low_level_trace(enter_pred,pen,XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
      ENV = YENV;
      YENV = ASP;
      YENV[E_CB] = (CELL)(B->cp_b);
      CP = P;
      q = pen->FirstClause;
      if (pen->PredFlags & ProfiledPredFlag) {
	LOCK(pen->StatisticsForPred.lock);
	if (position == 1)
	  pen->StatisticsForPred.NOfEntries++;
	else
	  pen->StatisticsForPred.NOfRetries++;
	UNLOCK(pen->StatisticsForPred.lock);
      }
      if (flags & DynamicPredFlag) {
	CLAUSECODE->arity = pen->ArityOfPE;
	CLAUSECODE->func = pen->FunctorOfPred;
	while (position > 1) {
	  while (ClauseCodeToClause(q)->ClFlags & ErasedMask)
	    q = NextClause(q);
	  position--;
	  q = NextClause(q);
	}
	while (ClauseCodeToClause(q)->ClFlags & ErasedMask)
	  q = NextClause(q);
#if defined(YAPOR) || defined(THREADS)
	{
	  Clause *cl = ClauseCodeToClause(q);

	  LOCK(cl->ClLock);
	  TRAIL_CLREF(cl);
	  INC_DBREF_COUNT(cl);
	  UNLOCK(cl->ClLock);
	}
#else 
	if (!(ClauseCodeToClause(q)->ClFlags & InUseMask)) {
	  OPREG     *opp = &(ClauseCodeToClause(q)->ClFlags);
	  TRAIL_CLREF(ClauseCodeToClause(q));
	  *opp |= InUseMask;
	}
#endif
	CLAUSECODE->clause = (CODEADDR)NEXTOP((yamop *)(q),ld);
	P = (yamop *)CLAUSECODE->clause;
	WRITE_UNLOCK(pen->PRWLock);
	return((CELL)(&(CLAUSECODE->clause)));
      } else {
	for (; position > 1; position--)
	  q = NextClause(q);
	P = NEXTOP((yamop *)(q),ld);
	WRITE_UNLOCK(pen->PRWLock);
	return (Unsigned(&(pen->StateOfPred)));
      }
    }
  }
  if (flags & UserCPredFlag) {
    Int(*p) (void) = (Int(*) (void)) pen->CodeOfPred;
    Int out;
    
    WRITE_UNLOCK(pen->PRWLock);
    save_machine_regs();
    out = YapExecute(p);
    restore_machine_regs();
    return(out);
  }
  if (flags & CPredFlag) {
    Int(*p) (void) = (Int(*) (void)) pen->CodeOfPred;
    WRITE_UNLOCK(pen->PRWLock);
    return ((*p) ());
  } else if (flags & BasicPredFlag) {
    if (pen->OpcodeOfPred != UNDEF_OPCODE) {
      Int(*p) (void) = (Int(*) (void)) pen->CodeOfPred;
      WRITE_UNLOCK(pen->PRWLock);
      return (((*p) ()) != FALSE);
    }
  }
  return (FALSE);
}

static Term
current_cp_as_integer(void)
{
  return(MkIntTerm(LCL0-(CELL *)B));
}

static Int
p_save_cp(void)
{
  Term t = Deref(ARG1);
  Term td;
#if SHADOW_HB
  register CELL *HBREG = HB;
#endif
  if (!IsVarTerm(t)) return(FALSE);
  td = current_cp_as_integer();
  BIND((CELL *)t,td,bind_save_cp);
#ifdef COROUTINING
  DO_TRAIL(CellPtr(t), td);
  if (CellPtr(t) < H0) WakeUp((CELL *)t);
 bind_save_cp:
#endif
  return(TRUE);
}

inline static int
SpecialCallFunctor(Functor f) {
  return(f == FunctorComma || f == FunctorOr || f == FunctorArrow ||
	 f == FunctorVBar || f == FunctorNot || f == FunctorAltNot);
}

inline static Int
CallMetaCall(void) {
  ARG2 = current_cp_as_integer(); /* p_save_cp */
  ARG3 = TermNil;
  WRITE_LOCK(PredMetaCall->PRWLock);
  return (FastCallProlog(PredMetaCall));
}

static Int
p_execute(void)
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  unsigned int    arity;
  Prop            pe;
  Atom            a;

 restart_exec:
  if (yap_flags[SPY_CREEP_FLAG]) {
    a = NameOfFunctor(FunctorSpiedMetaCall);
    return (CallProlog(RepPredProp(PredProp(a,1)), 1, (Int) (-1)));
  }
  if (PredGoalExpansion->OpcodeOfPred != UNDEF_OPCODE) {
    return(CallMetaCall());
  } else  if (IsVarTerm(t)) {
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) {
      return(CallMetaCall());
    } else {
      Error(INSTANTIATION_ERROR,t,"call/1");    
      return(FALSE);
    }
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    register unsigned int    i;
    register CELL *pt;

    if (IsExtensionFunctor(f)) {
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) {
	return(CallMetaCall());
      } else {
	Error(TYPE_ERROR_CALLABLE,t,"call/1");    
	return(FALSE);
      }
    }
    
    if (SpecialCallFunctor(f)) {
      return(CallMetaCall());
    } else if (f == FunctorModule) {
      Term mod = ArgOfTerm(1, t);
      if (mod == ModuleName[CurrentModule]) {
	/* we can skip this operation */
	/* should catch most cases */
	t = ArgOfTerm(2, t);
	goto restart_exec;
      } else {
	/* I can't do better because I don't have a way of restoring the module */
	return(CallMetaCall());
      }
    } else {
      PredEntry *pen;
      arity = ArityOfFunctor(f);
      a = NameOfFunctor(f);

      if (CurrentModule)
	pe = PredProp(a, arity);
      else {
	pe = GetPredProp(a, arity);
	if (pe == NIL) {
	  return(CallMetaCall());
	}
      }
      pen = RepPredProp(pe);
      /* You thought we would be over by now */
      /* but no meta calls require special preprocessing */
      if (pen->PredFlags & MetaPredFlag) {
	return(CallMetaCall());
      }
      /* now let us do what we wanted to do from the beginning !! */
      /* I cannot use the standard macro here because
	 otherwise I would dereference the argument and
	 might skip a svar */
      pt = RepAppl(t)+1;
      for (i = 1; i <= arity; ++i) {
#if SBA
	Term d0 = *pt++;
	if (d0 == 0)
	  XREGS[i] = (CELL)(pt-1);
	else
	  XREGS[i] = d0;
#else
	XREGS[i] = *pt++;
#endif
      }
      return (CallProlog(pen, arity, (Int) (-1)));
    }
  } else if (IsAtomOrIntTerm(t)) {
    if (IsIntTerm(t)) {
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) {
	return (CallMetaCall());
      } else {
	Error(TYPE_ERROR_CALLABLE,t,"call/1");    
	return(FALSE);
      }
    }    
    a = AtomOfTerm(t);
    if (a == AtomTrue || a == AtomOtherwise || a == AtomCut)
      return(TRUE);
    else if (a == AtomFail || a == AtomFalse)
      return(FALSE);
    else
      arity = 0;
    /* call may not define new system predicates!! */
    if (CurrentModule)
      pe = PredProp(a, arity);
    else {
      pe = GetPredProp(a, arity);
      if (pe == NIL) {
	ARG1 = t;
	return(CallMetaCall());
      }
    }
    return (CallProlog(RepPredProp(pe), arity, (Int) (-1)));
  } else {
    /* Is Pair Term */
    return(CallMetaCall());
  }
}

inline static Int
CallMetaCallWithin(void)
{
  WRITE_LOCK(PredMetaCall->PRWLock);
  return (FastCallProlog(PredMetaCall));
}

/* '$execute_within'(Goal,CutPt,OrigGoal)	 */
static Int
p_execute_within(void)
{ 
  Term            t = Deref(ARG1);
  unsigned int    arity;
  Prop            pe;
  Atom            a;

 restart_exec:
  if (yap_flags[SPY_CREEP_FLAG]) {
    a = NameOfFunctor(FunctorSpiedMetaCall);
    return (CallProlog(RepPredProp(PredProp(a,1)), 1, (Int) (-1)));
  }
  if (PredGoalExpansion->OpcodeOfPred != UNDEF_OPCODE) {
    return(CallMetaCallWithin());
  } else  if (IsVarTerm(t)) {
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) {
      return(CallMetaCallWithin());
    } else {
      Error(INSTANTIATION_ERROR,t,"call/1");    
      return(FALSE);
    }
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    register unsigned int    i;
    register CELL *pt;

    if (IsExtensionFunctor(f)) {
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) {
	return(CallMetaCallWithin());
      } else {
	Error(TYPE_ERROR_CALLABLE,t,"call/1");    
	return(FALSE);
      }
    }
    
    if (SpecialCallFunctor(f)) {
      return(CallMetaCallWithin());
    } else if (f == FunctorModule) {
      Term mod = ArgOfTerm(1, t);
      if (mod == ModuleName[CurrentModule]) {
	/* we can skip this operation */
	/* should catch most cases */
	t = ArgOfTerm(2, t);
	goto restart_exec;
      } else {
	/* I can't do better because I don't have a way of restoring the module */
	return(CallMetaCallWithin());
      }
    } else {
      PredEntry *pen;
      arity = ArityOfFunctor(f);
      a = NameOfFunctor(f);

      if (CurrentModule)
	pe = PredProp(a, arity);
      else {
	pe = GetPredProp(a, arity);
	if (pe == NIL) {
	  return(CallMetaCallWithin());
	}
      }
      pen = RepPredProp(pe);
      /* You thought we would be over by now */
      /* but no meta calls require special preprocessing */
      if (pen->PredFlags & MetaPredFlag) {
	return(CallMetaCallWithin());
      }
      /* now let us do what we wanted to do from the beginning !! */
      /* I cannot use the standard macro here because
	 otherwise I would dereference the argument and
	 might skip a svar */
      pt = RepAppl(t)+1;
      for (i = 1; i <= arity; ++i) {
#if SBA
	Term d0 = *pt++;
	if (d0 == 0)
	  XREGS[i] = (CELL)(pt-1);
	else
	  XREGS[i] = d0;
#else
	XREGS[i] = *pt++;
#endif
      }
      return (CallProlog(pen, arity, (Int) (-1)));
    }
  } else if (IsAtomOrIntTerm(t)) {
    if (IsIntTerm(t)) {
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) {
	return (CallMetaCallWithin());
      } else {
	Error(TYPE_ERROR_CALLABLE,t,"call/1");    
	return(FALSE);
      }
    }    
    a = AtomOfTerm(t);
    if (a == AtomTrue || a == AtomOtherwise)
      return(TRUE);
    else if (a == AtomCut) {
#ifdef SBA
      choiceptr pt0 = (choiceptr)IntegerOfTerm(Deref(ARG2));
#else
      choiceptr pt0 = (choiceptr)(LCL0-IntegerOfTerm(Deref(ARG2)));
#endif
      if (TopB != NULL && YOUNGER_CP(TopB,pt0)) {  
	pt0 = TopB;
	if (DelayedB == NULL || YOUNGER_CP(pt0,DelayedB))
	  DelayedB = pt0;
      }
      /* find where to cut to */
#ifdef YAPOR
      if (SHOULD_CUT_UP_TO(B,pt0)) {
	/* Wow, we're gonna cut!!! */
	CUT_prune_to(pt0);
#else
      if (SHOULD_CUT_UP_TO(B,pt0)) {
	/* Wow, we're gonna cut!!! */
	B = pt0;
#endif /* YAPOR */
	HB = PROTECT_FROZEN_H(B);
      }
      return(TRUE);
    } else if (a == AtomFail || a == AtomFalse)
      return(FALSE);
    else
      arity = 0;
    /* call may not define new system predicates!! */
    if (CurrentModule)
      pe = PredProp(a, arity);
    else {
      pe = GetPredProp(a, arity);
      if (pe == NIL) {
	ARG1 = t;
	return(CallMetaCallWithin());
      }
    }
    return (CallProlog(RepPredProp(pe), arity, (Int) (-1)));
  } else {
    /* Is Pair Term */
    return(CallMetaCallWithin());
  }
}


static Int
p_execute0(void)
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  unsigned int    arity;
  Prop            pe;
  Atom            a;

  if (IsAtomTerm(t)) {
    arity = 0;
    a = AtomOfTerm(t);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    register unsigned int    i;
    register CELL *pt;

    if (IsExtensionFunctor(f))
      return(FALSE);
    arity = ArityOfFunctor(f);
    a = NameOfFunctor(f);
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t)+1;
    for (i = 1; i <= arity; ++i)
#if SBA
      {
	Term d0 = *pt++;
	if (d0 == 0)
	  XREGS[i] = (CELL)(pt-1);
	else
	  XREGS[i] = d0;
      }
#else
      XREGS[i] = *pt++;
#endif
  } else
    return (FALSE);	/* for the moment */
  /*	N = arity; */
  /* call may not define new system predicates!! */
  if (CurrentModule)
    pe = PredProp(a, arity);
  else {
    pe = GetPredProp(a, arity);
    if (pe == NIL)
      return(FALSE);
  }
  return (CallProlog(RepPredProp(pe), arity, (Int) (-1)));
}

static Int
p_execute_0(void)
{				/* '$execute_0'(Goal)	 */
  Term            t = Deref(ARG1);
  Prop            pe;
  Atom            a;

  a = AtomOfTerm(t);
  if (CurrentModule)
    pe = PredProp(a, 0);
  else {
    pe = GetPredProp(a, 0);
    if (pe == NIL)
      return(FALSE);
  }
  return (CallProlog(RepPredProp(pe), 0, (Int) (-1)));

}

static Int
p_execute_1(void)
{				/* '$execute_0'(Goal)	 */
  Term            t = Deref(ARG1);
  Prop            pe;
  Atom            a;

  a = AtomOfTerm(t);
  ARG1 = ARG2;
  if (CurrentModule)
    pe = PredProp(a, 1);
  else {
    pe = GetPredProp(a, 1);
    if (pe == NIL)
      return(FALSE);
  }
  return (CallProlog(RepPredProp(pe), 1, (Int) (-1)));
}

static Int
p_execute_2(void)
{				/* '$execute_2'(Goal)	 */
  Term            t = Deref(ARG1);
  Prop            pe;
  Atom            a;

  a = AtomOfTerm(t);
  ARG1 = ARG2;
  ARG2 = ARG3;
  if (CurrentModule)
    pe = PredProp(a, 2);
  else {
    pe = GetPredProp(a, 2);
    if (pe == NIL)
      return(FALSE);
  }
  return (CallProlog(RepPredProp(pe), 2, (Int) (-1)));
}

static Int
p_execute_3(void)
{				/* '$execute_3'(Goal)	 */
  Term            t = Deref(ARG1);
  Prop            pe;
  Atom            a;

  a = AtomOfTerm(t);
  ARG1 = ARG2;
  ARG2 = ARG3;
  ARG3 = ARG4;
  if (CurrentModule)
    pe = PredProp(a, 3);
  else {
    pe = GetPredProp(a, 3);
    if (pe == NIL)
      return(FALSE);
  }
  return (CallProlog(RepPredProp(pe), 3, (Int) (-1)));
}

static Int
p_execute_4(void)
{				/* '$execute_4'(Goal)	 */
  Term            t = Deref(ARG1);
  Prop            pe;
  Atom            a;

  a = AtomOfTerm(t);
  ARG1 = ARG2;
  ARG2 = ARG3;
  ARG3 = ARG4;
  ARG4 = ARG5;
  if (CurrentModule)
    pe = PredProp(a, 4);
  else {
    pe = GetPredProp(a, 4);
    if (pe == NIL)
      return(FALSE);
  }
  return (CallProlog(RepPredProp(pe), 4, (Int) (-1)));
}

static Int
p_execute_5(void)
{				/* '$execute_5'(Goal)	 */
  Term            t = Deref(ARG1);
  Prop            pe;
  Atom            a;

  a = AtomOfTerm(t);
  ARG1 = ARG2;
  ARG2 = ARG3;
  ARG3 = ARG4;
  ARG4 = ARG5;
  ARG5 = ARG6;
  if (CurrentModule)
    pe = PredProp(a, 5);
  else {
    pe = GetPredProp(a, 5);
    if (pe == NIL)
      return(FALSE);
  }
  return (CallProlog(RepPredProp(pe), 5, (Int) (-1)));
}

static Int
p_execute_6(void)
{				/* '$execute_6'(Goal)	 */
  Term            t = Deref(ARG1);
  Prop            pe;
  Atom            a;

  a = AtomOfTerm(t);
  ARG1 = ARG2;
  ARG2 = ARG3;
  ARG3 = ARG4;
  ARG4 = ARG5;
  ARG5 = ARG6;
  ARG6 = ARG7;
  if (CurrentModule)
    pe = PredProp(a, 6);
  else {
    pe = GetPredProp(a, 6);
    if (pe == NIL)
      return(FALSE);
  }
  return (CallProlog(RepPredProp(pe), 6, (Int) (-1)));
}

static Int
p_execute_7(void)
{				/* '$execute_7'(Goal)	 */
  Term            t = Deref(ARG1);
  Prop            pe;
  Atom            a;

  a = AtomOfTerm(t);
  ARG1 = ARG2;
  ARG2 = ARG3;
  ARG3 = ARG4;
  ARG4 = ARG5;
  ARG5 = ARG6;
  ARG6 = ARG7;
  ARG7 = ARG8;
  if (CurrentModule)
    pe = PredProp(a, 7);
  else {
    pe = GetPredProp(a, 7);
    if (pe == NIL)
      return(FALSE);
  }
  return (CallProlog(RepPredProp(pe), 7, (Int) (-1)));
}

static Int
p_execute_8(void)
{				/* '$execute_8'(Goal)	 */
  Term            t = Deref(ARG1);
  Prop            pe;
  Atom            a;

  a = AtomOfTerm(t);
  ARG1 = ARG2;
  ARG2 = ARG3;
  ARG3 = ARG4;
  ARG4 = ARG5;
  ARG5 = ARG6;
  ARG6 = ARG7;
  ARG7 = ARG8;
  ARG8 = ARG9;
  if (CurrentModule)
    pe = PredProp(a, 8);
  else {
    pe = GetPredProp(a, 8);
    if (pe == NIL)
      return(FALSE);
  }
  return (CallProlog(RepPredProp(pe), 8, (Int) (-1)));
}

static Int
p_execute_9(void)
{				/* '$execute_9'(Goal)	 */
  Term            t = Deref(ARG1);
  Prop            pe;
  Atom            a;

  a = AtomOfTerm(t);
  ARG1 = ARG2;
  ARG2 = ARG3;
  ARG3 = ARG4;
  ARG4 = ARG5;
  ARG5 = ARG6;
  ARG6 = ARG7;
  ARG7 = ARG8;
  ARG8 = ARG9;
  ARG9 = ARG10;
  if (CurrentModule)
    pe = PredProp(a, 9);
  else {
    pe = GetPredProp(a, 9);
    if (pe == NIL)
      return(FALSE);
  }
  return (CallProlog(RepPredProp(pe), 9, (Int) (-1)));
}

static Int
p_execute_10(void)
{				/* '$execute_10'(Goal)	 */
  Term            t = Deref(ARG1);
  Prop            pe;
  Atom            a;

  a = AtomOfTerm(t);
  ARG1 = ARG2;
  ARG2 = ARG3;
  ARG3 = ARG4;
  ARG4 = ARG5;
  ARG5 = ARG6;
  ARG6 = ARG7;
  ARG7 = ARG8;
  ARG8 = ARG9;
  ARG9 = ARG10;
  ARG10 = ARG11;
  if (CurrentModule)
    pe = PredProp(a, 10);
  else {
    pe = GetPredProp(a, 10);
    if (pe == NIL)
      return(FALSE);
  }
  return (CallProlog(RepPredProp(pe), 10, (Int) (-1)));
}

#ifdef DEPTH_LIMIT
static Int
p_execute_depth_limit(void) {
  Term d = Deref(ARG2);
  if (IsVarTerm(d)) {
    Error(INSTANTIATION_ERROR,d,"depth_bound_call/2");    
  } else if (!IsIntTerm(d)) {
    Error(TYPE_ERROR_INTEGER, d, "depth_bound_call/2");
    return(FALSE);
  }
  DEPTH = MkIntTerm(IntOfTerm(d)*2);
  return(p_execute());
}
#endif

static Int
p_pred_goal_expansion_on(void) {
  /* a goal needs expansion if we have goal_expansion defined or
     if the goal is a meta-call */
  return (PredGoalExpansion->OpcodeOfPred != UNDEF_OPCODE);
}

static Int 
p_at_execute(void)
{				/* '$execute'(Goal,ClauseNumber) */
  Term            t = Deref(ARG1), t2 = Deref(ARG2);
  unsigned int arity;
  Prop            pe;
  Atom            a;

  if (IsAtomTerm(t))
    arity = 0, a = AtomOfTerm(t);
  else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    register unsigned int    i;
    register CELL *pt;

    if (IsBlobFunctor(f))
      return(FALSE);
    arity = ArityOfFunctor(f);
    a = NameOfFunctor(f);
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t)+1;
    for (i = 1; i <= arity; ++i)
#if SBA
      {
	Term d0 = *pt++;
	if (d0 == 0)
	  XREGS[i] = (CELL)(pt-1);
	else
	  XREGS[i] = d0;
      }
#else
      XREGS[i] = *pt++;
#endif
  } else
    return (FALSE);	/* for the moment */
  if (IsVarTerm(t2) || !IsIntTerm(t2))
    return (FALSE);
  /* N = arity; */
  /* call may not define new system predicates!! */
  if (CurrentModule) {
    pe = PredProp(a, arity);
  } else {
    pe = GetPredProp(a, arity);
    if (pe == NIL)
      return(FALSE);
  }
  return (CallProlog(RepPredProp(pe), arity, IntOfTerm(t2)));
}

int
exec_absmi(int top)
{
  int lval;
  if (top && (lval = sigsetjmp (RestartEnv, 1)) != 0) {
    if (lval == 1) { /* restart */
      int depth;
      /* otherwise, SetDBForThrow will fail entering critical mode */
      PrologMode = UserMode;
      /* find out where to cut to */
#if defined(__GNUC__)
#if defined(hppa) || defined(__alpha)
     /* siglongjmp resets the TR hardware register */
      restore_TR();
#endif
#if defined(__alpha)
     /* siglongjmp resets the H hardware register */
      restore_H();
#endif
#endif
      depth = SetDBForThrow(MkAtomTerm(LookupAtom("abort")));
      if (depth == 0) {
	Error(SYSTEM_ERROR, TermNil, "database entry for throw corrupted");
      }
      /* make the abstract machine jump where we want them to jump to */

#ifdef SBA
      B = (choiceptr)depth;
#else
      B = (choiceptr)(LCL0-depth);
#endif
      yap_flags[SPY_CREEP_FLAG] = 0;
      CreepFlag = CalculateStackGap();
      P = (yamop *)FAILCODE;
    }
    if (lval == 2) { /* arithmetic exception */
      /* must be done here, otherwise siglongjmp will clobber all the registers */
      Error(YAP_matherror,TermNil,NULL);
      /* reset the registers so that we don't have trash in abstract machine */
      set_fpu_exceptions(yap_flags[LANGUAGE_MODE_FLAG] == 1);
      P = (yamop *)FAILCODE;
    }
    if (lval == 3) { /* saved state */
      return(FALSE);
    }
  }
  PrologMode = UserMode;
  return(absmi(0));
}

static int
do_goal(CODEADDR CodeAdr, int arity, CELL *pt, int args_to_save, int top)
{
  choiceptr saved_b = B;

  /* create an initial pseudo environment so that when garbage
     collection is going up in the environment chain it doesn't get
     confused */
  YENV = ASP;
  YENV[E_CP] = (CELL)P;
  YENV[E_CB] = (CELL)B;
  YENV[E_E]  = (CELL)ENV;
#ifdef  DEPTH_LIMIT
  YENV[E_DEPTH] = DEPTH;
#endif
  ENV = YENV;
  ASP -= EnvSizeInCells;
  /* and now create a pseudo choicepoint for much the same reasons */
  CP = (yamop *) YESCODE;
  /* this is an hack to save the arguments */
  {
    CELL *pt1 = XREGS+1+args_to_save;

    while (pt1 > XREGS+1)
      *--ASP = *--pt1;
  }
  *--ASP = MkIntTerm(args_to_save+1);
  { 
    int i;
    for (i = 0; i < arity; i++) {
      XREGS[i+1] = *pt++;
    }
  }
  B = (choiceptr)ASP;
  B--;
#ifdef TABLING
  if (top) {
    DepFr_cons_cp(GLOBAL_root_dep_fr) = B;
  }
#endif /* TABLING */
  B->cp_h     = H;
  B->cp_tr    = TR;
  B->cp_cp    = CP;
  B->cp_ap    = (yamop *) NOCODE;
  B->cp_env   = ENV;
  B->cp_b     = saved_b;
#ifdef DEPTH_LIMIT
  B->cp_depth = DEPTH;
#endif /* DEPTH_LIMIT */
  if (top) {
    Term t;
#if COROUTINING
    RESET_VARIABLE((CELL *)GlobalBase);
    DelayedVars = NewTimedVar((CELL)GlobalBase);
    WokenGoals = NewTimedVar(TermNil);
    MutableList = NewTimedVar(TermNil);
    AttsMutableList = NewTimedVar(TermNil);
#endif
    t = NewTimedVar(MkIntTerm(0));
    CurrentModulePtr = RepAppl(t)+1;
  }
  YENV = ASP = (CELL *)B;
  HB = H;
  YENV[E_CB] = Unsigned (B);
  P = (yamop *) CodeAdr;
  S = CellPtr (&(RepPredProp (PredProp (AtomCall, 1))->StateOfPred));	/* A1 mishaps */
  TopB = B;

  return(exec_absmi(top));
}


Int
execute_goal(Term t, int nargs)
{
  Int             out;
  CODEADDR        CodeAdr;
  yamop *saved_p, *saved_cp;
  int arity;
  Atom a;
  Prop pe;
  PredEntry *ppe;
  CELL *pt;
  /* preserve the current restart environment */
  /* visualc*/
  /* just keep the difference because of possible garbage collections */
  Int MyOldTopB = LCL0-(CELL *)TopB;
  Int MyOldDelayedB = LCL0-(CELL *)DelayedB;
  choiceptr OldTopB, OldDelayedB;


  DelayedB = NULL;
  /* forget we have a DelayedB to do, this is not a problem for the embedded computation to solve */

  saved_p = P;
  saved_cp = CP;
  
  if (IsAtomTerm(t)) {
    arity = 0;
    a = AtomOfTerm(t);
    pt = NULL;
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);

    if (IsBlobFunctor(f)) {
      Error(TYPE_ERROR_CALLABLE,t,"call/1");
      return(FALSE);
    }
    arity = ArityOfFunctor(f);
    a = NameOfFunctor(f);
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t)+1;
  } else {
    Error(TYPE_ERROR_CALLABLE,t,"call/1");
    return(FALSE);
  }
  pe = GetPredProp(a, arity);
  ppe = RepPredProp(pe);
  if (pe != NIL)
    READ_LOCK(ppe->PRWLock);
  if (pe == NIL ||
      ppe->OpcodeOfPred == UNDEF_OPCODE ||
      ppe->PredFlags & (UserCPredFlag|CPredFlag|BasicPredFlag) )
    {
      if (pe != NIL)
	READ_UNLOCK(ppe->PRWLock);
      return(CallMetaCall());
    }
  CodeAdr = RepPredProp (PredProp (a, arity))->CodeOfPred;
  READ_UNLOCK(ppe->PRWLock);
  out = do_goal(CodeAdr, arity, pt, nargs, FALSE);

  if (out == 1) {
    choiceptr old_B;
    /* we succeeded, let's prune */
    /* restore the old environment */
    /* get to previous environment */
#ifdef YAPOR
    CUT_prune_to((choiceptr)(ENV[E_CB]));
#else
    B = (choiceptr)(ENV[E_CB]);
#endif /* YAPOR */
    /* find out where we have the old arguments */
    old_B = ((choiceptr)(ENV-(EnvSizeInCells+nargs+1)))-1;
    {
      int i;
      for (i=1; i<= nargs; i++) {
#if MIN_ARRAY == 0
	XREGS[i] = old_B->cp_args[i];
#else
	XREGS[i] = old_B->cp_last.cp_args[i];
#endif
      }
    }
    CP   = saved_cp;
    P    = saved_p;
    ASP  = ENV;
    ENV  = (CELL *)(ENV[E_E]);
#ifdef DEPTH_LIMIT
    DEPTH= ENV[E_DEPTH];
#endif
    /* we have failed, and usually we would backtrack to this B,
       trouble is, we may also have a delayed cut to do */
    OldTopB = (choiceptr)(LCL0-MyOldTopB);
    OldDelayedB = (choiceptr)(LCL0-MyOldDelayedB);
    if (DelayedB != NULL) {
      if (YOUNGER_CP(DelayedB,B)) {
	/* we have a delayed cut to do */
	if (YOUNGER_CP(OldTopB,DelayedB)) {
	  /* and this delayed cut is to before the c-code that actually called us */
	  B = OldTopB;
	  /* did we have a cut which was cutting more than our current cut? */
	  if (OldDelayedB != NULL && YOUNGER_CP(DelayedB,OldDelayedB))
	    DelayedB = OldDelayedB;
	} else {
	  /* just cut back to where we should cut */
	  B = DelayedB;
	  DelayedB = OldDelayedB;
	}
      }
    } else {
      DelayedB = OldDelayedB;
    }
    TopB = OldTopB;
    HB   = B->cp_h;
    YENV = ENV;
    return(TRUE);
  } else if (out == 0) {
    ASP  = B->cp_env;
    {
      int i;
      for (i=1; i<= nargs; i++) {
#if MIN_ARRAY==0
	XREGS[i] = B->cp_args[i];
#else
	XREGS[i] = B->cp_last.cp_args[i];
#endif
      }
    }
    P    = saved_p;
    CP   = saved_cp;
    H    = B->cp_h;
#ifdef DEPTH_LIMIT
    DEPTH= B->cp_depth;
#endif
    YENV = ENV  = (CELL *)((B->cp_env)[E_E]);
    B    = B->cp_b;
    SET_BB(B);
    /* we have failed, and usually we would backtrack to this B,
       trouble is, we may also have a delayed cut to do */
    OldTopB = (choiceptr)(LCL0-MyOldTopB);
    OldDelayedB = (choiceptr)(LCL0-MyOldDelayedB);
    if (DelayedB != NULL) {
      if (YOUNGER_CP(DelayedB,B)) {
	/* we have a delayed cut to do */
	if (YOUNGER_CP(OldTopB,DelayedB)) {
	  /* and this delayed cut is to before the c-code that actually called us */
	  B = OldTopB;
	  /* did we have a cut which was cutting more than our current cut? */
	  if (OldDelayedB != NULL && YOUNGER_CP(DelayedB,OldDelayedB))
	    DelayedB = OldDelayedB;
	} else {
	  /* just cut back to where we should cut */
	  B = DelayedB;
	  DelayedB = OldDelayedB;
	}
      }
    } else {
      DelayedB = OldDelayedB;
    }
    TopB = OldTopB;
    HB = PROTECT_FROZEN_H(B);
    return(FALSE);
  } else {
    Error(SYSTEM_ERROR,TermNil,"emulator crashed");
    return(FALSE);
  }
}

int
RunTopGoal(Term t)
{
  CODEADDR        CodeAdr;
  int arity;
  Atom a;
  Prop pe;
  PredEntry *ppe;
  CELL *pt;

  if (IsAtomTerm(t)) {
    arity = 0;
    a = AtomOfTerm(t);
    pt = NULL;
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);

    if (IsBlobFunctor(f)) {
      Error(TYPE_ERROR_CALLABLE,t,"call/1");
      return(FALSE);
    }
    arity = ArityOfFunctor(f);
    a = NameOfFunctor(f);
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t)+1;
  } else {
    Error(TYPE_ERROR_CALLABLE,t,"call/1");
    return(FALSE);
  }
  pe = GetPredProp(a, arity);
  ppe = RepPredProp(pe);
  if (pe != NIL)
    READ_LOCK(ppe->PRWLock);
  if (pe == NIL ||
      ppe->OpcodeOfPred == UNDEF_OPCODE ||
      ppe->PredFlags & (UserCPredFlag|CPredFlag|BasicPredFlag) )
    {
      if (pe != NIL)
	READ_UNLOCK(ppe->PRWLock);
      /* we must always start the emulator with Prolog code */
      return(FALSE);
    }
  CodeAdr = ppe->CodeOfPred;
  READ_UNLOCK(ppe->PRWLock);
  if (TrailTop - HeapTop < 2048) {
    PrologMode = BootMode;
    Error(SYSTEM_ERROR,TermNil,
	  "unable to boot because of too little heap space");
  }
  return(do_goal(CodeAdr, arity, pt, 0, TRUE));
}

static void
restore_regs(Term t)
{
  if (IsApplTerm(t)) {
    Int i;
    Int max = ArityOfFunctor(FunctorOfTerm(t));
    CELL *ptr = RepAppl(t)+1;

    for (i = 0; i < max; i += 2) {
      Int j = IntOfTerm(ptr[0]);
      XREGS[j] = ptr[1];
      ptr+=2;
    }
  }
}

/* low level voodoo to restore temporary registers after a call */
static Int
p_restore_regs(void)
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Error(INSTANTIATION_ERROR,t,"support for coroutining");    
    return(FALSE);
  }
  if (IsAtomTerm(t)) return(TRUE);
  restore_regs(t);
  return(TRUE);
}

/* low level voodoo to cut and then restore temporary registers after a call */
static Int
p_restore_regs2(void)
{
  Term t = Deref(ARG1), d0;
  choiceptr pt0;
  if (IsVarTerm(t)) {
    Error(INSTANTIATION_ERROR,t,"support for coroutining");    
    return(FALSE);
  }
  if (!IsAtomTerm(t)) {
    restore_regs(t);
  }
  d0 = Deref(ARG2);
  if (IsVarTerm(d0)) {
    Error(INSTANTIATION_ERROR,d0,"support for coroutining");    
    return(FALSE);
  }
#if SBA
  if (!IsIntegerTerm(d0)) {
#else
  if (!IsIntTerm(d0)) {
#endif
    return(FALSE);
  }
#if SBA
  pt0 = (choiceptr)IntegerOfTerm(d0);
#else
  pt0 = (choiceptr)(LCL0-IntOfTerm(d0));
#endif
  if (TopB != NULL && YOUNGER_CP(TopB,pt0)) {  
    if (DelayedB == NULL || YOUNGER_CP(DelayedB,pt0))
      DelayedB = pt0;
    pt0 = TopB;
  }
  /* find where to cut to */
  if (pt0 > B) {
    /* Wow, we're gonna cut!!! */
#ifdef YAPOR
    CUT_prune_to((choiceptr) d0);
#else
    B = pt0;
#endif /* YAPOR */
    HB = B->cp_h;
    /*    trim_trail();*/
  }
  return(TRUE);
}

static Int
p_clean_ifcp(void) {
#if SBA
  choiceptr pt0 = (choiceptr)IntegerOfTerm(Deref(ARG1));
#else
  choiceptr pt0 = (choiceptr)(LCL0-IntOfTerm(Deref(ARG1)));
#endif
  if (pt0 == B) {
    B = B->cp_b;
    HB = B->cp_h;
  } else {
    pt0->cp_ap = (yamop *)TRUSTFAILCODE;
  }
  return(TRUE);
}


void 
InitExecFs(void)
{
  InitCPred("$execute", 1, p_execute, 0);
  InitCPred("$execute_within", 3, p_execute_within, 0);
  InitCPred("$execute", 2, p_at_execute, 0);
  InitCPred("$call_with_args", 1, p_execute_0, 0);
  InitCPred("$call_with_args", 2, p_execute_1, 0);
  InitCPred("$call_with_args", 3, p_execute_2, 0);
  InitCPred("$call_with_args", 4, p_execute_3, 0);
  InitCPred("$call_with_args", 5, p_execute_4, 0);
  InitCPred("$call_with_args", 6, p_execute_5, 0);
  InitCPred("$call_with_args", 7, p_execute_6, 0);
  InitCPred("$call_with_args", 8, p_execute_7, 0);
  InitCPred("$call_with_args", 9, p_execute_8, 0);
  InitCPred("$call_with_args", 10, p_execute_9, 0);
  InitCPred("$call_with_args", 11, p_execute_10, 0);
#ifdef DEPTH_LIMIT
  InitCPred("$execute_under_depth_limit", 2, p_execute_depth_limit, 0);
#endif
  InitCPred("$execute0", 1, p_execute0, 0);
  InitCPred("$save_current_choice_point", 1, p_save_cp, 0);
  InitCPred("$pred_goal_expansion_on", 0, p_pred_goal_expansion_on, SafePredFlag);
  InitCPred("$restore_regs", 1, p_restore_regs, SafePredFlag);
  InitCPred("$restore_regs", 2, p_restore_regs2, SafePredFlag);
  InitCPred("$clean_ifcp", 1, p_clean_ifcp, SafePredFlag);
}

