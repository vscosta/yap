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

STATIC_PROTO(Int  CallPredicate, (PredEntry *, choiceptr, yamop *));
STATIC_PROTO(Int  EnterCreepMode, (Term, Term));
STATIC_PROTO(Int  p_save_cp, (void));
STATIC_PROTO(Int  p_execute, (void));
STATIC_PROTO(Int  p_execute0, (void));

static Term
cp_as_integer(choiceptr cp)
{
  return(MkIntTerm(LCL0-(CELL *)cp));
}

Term
Yap_cp_as_integer(choiceptr cp)
{
  return cp_as_integer(cp);
}

static inline Int
CallPredicate(PredEntry *pen, choiceptr cut_pt, yamop *code) {
#ifdef LOW_LEVEL_TRACER
  if (Yap_do_low_level_trace)
    low_level_trace(enter_pred,pen,XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
  READ_LOCK(pen->PRWLock);
#ifdef DEPTH_LIMIT
  if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is prolog */
    if (pen->ModuleOfPred) {
      if (DEPTH == MkIntTerm(0)) {
	READ_UNLOCK(pen->PRWLock);
	return FALSE;
      }
      else DEPTH = RESET_DEPTH();
    }
  } else if (pen->ModuleOfPred)
    DEPTH -= MkIntConstant(2);
#endif	/* DEPTH_LIMIT */
  CP = P;
  P = code;
  /* vsc: increment reduction counter at meta-call entry */
  READ_UNLOCK(pen->PRWLock);
  if (pen->PredFlags & ProfiledPredFlag) {
    LOCK(pen->StatisticsForPred.lock);
    pen->StatisticsForPred.NOfEntries++;
    UNLOCK(pen->StatisticsForPred.lock);
  }
  ENV = YENV;
  YENV = ASP;
  YENV[E_CB] = (CELL) cut_pt;
  return (TRUE);
}

inline static Int
CallMetaCall(Term mod) {
  ARG2 = cp_as_integer(B); /* p_save_cp */
  ARG3 = ARG1;
  ARG4 = mod;
  return (CallPredicate(PredMetaCall, B, PredMetaCall->CodeOfPred));
}

Term
Yap_ExecuteCallMetaCall(Term mod) {
  Term ts[4];
  ts[0] = ARG1;
  ts[1] = cp_as_integer(B); /* p_save_cp */
  ts[2] = ARG1;
  ts[3] = mod;
  return(Yap_MkApplTerm(PredMetaCall->FunctorOfPred,4,ts));
}

static Int
CallError(yap_error_number err, Term mod)
{
  if (yap_flags[LANGUAGE_MODE_FLAG] == 1) {
    return(CallMetaCall(mod));
  } else {
    Yap_Error(err, ARG1, "call/1");
    return(FALSE);
  }
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
  td = cp_as_integer(B);
  BIND((CELL *)t,td,bind_save_cp);
#ifdef COROUTINING
  DO_TRAIL(CellPtr(t), td);
  if (CellPtr(t) < H0) Yap_WakeUp((CELL *)t);
 bind_save_cp:
#endif
  return(TRUE);
}

inline static Int
do_execute(Term t, Term mod)
{
  /* first do predicate expansion, even before you process signals.
     This way you don't get to spy goal_expansion(). */
  if (PRED_GOAL_EXPANSION_ON) {
    return CallMetaCall(mod);
  } else if (ActiveSignals) {
    return EnterCreepMode(t, mod);
  }
 restart_exec:
  if (IsVarTerm(t)) {
    return CallError(INSTANTIATION_ERROR, mod);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    register CELL *pt;
    PredEntry *pen;
    unsigned int i, arity;

    f = FunctorOfTerm(t);
    if (IsExtensionFunctor(f)) {
      return CallError(TYPE_ERROR_CALLABLE, mod);
    }
    arity = ArityOfFunctor(f);
    
    pen = RepPredProp(PredPropByFunc(f, mod));
    /* You thought we would be over by now */
    /* but no meta calls require special preprocessing */
    if (pen->PredFlags & MetaPredFlag) {
      if (f == FunctorModule) {
	Term tmod = ArgOfTerm(1,t);
	if (!IsVarTerm(tmod) && IsAtomTerm(tmod)) {
	  mod = tmod;
	  t = ArgOfTerm(2,t);
	  goto restart_exec;
	}
      } else {
	return(CallMetaCall(mod));
      }
    }
    /* now let us do what we wanted to do from the beginning !! */
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t)+1;
    for (i = 1; i <= arity; i++) {
#if SBA
      Term d0 = *pt++;
      if (d0 == 0)
`	XREGS[i] = (CELL)(pt-1);
      else
	XREGS[i] = d0;
#else
      XREGS[i] = *pt++;
#endif
    }
    return (CallPredicate(pen, B, pen->CodeOfPred));
  } else if (IsAtomTerm(t)) { 
    PredEntry            *pe;
    Atom a = AtomOfTerm(t);

    if (a == AtomTrue || a == AtomOtherwise || a == AtomCut)
      return(TRUE);
    else if (a == AtomFail || a == AtomFalse)
      return(FALSE);
    /* call may not define new system predicates!! */
    pe = RepPredProp(PredPropByAtom(a, mod));
    return (CallPredicate(pe, B, pe->CodeOfPred));
  } else if (IsIntTerm(t)) {
    return CallError(TYPE_ERROR_CALLABLE, mod);
  } else {
    /* Is Pair Term */
    return(CallMetaCall(mod));
  }
}

static Int
EnterCreepMode(Term t, Term mod) {
  PredEntry *PredCreep;

  if (ActiveSignals & YAP_CDOVF_SIGNAL) {
    ARG1 = t;
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_Error(FATAL_ERROR, TermNil, "YAP failed to grow heap at meta-call");
    }
    if (!ActiveSignals) {
      return do_execute(ARG1, mod);
    }
  }
  PredCreep = RepPredProp(PredPropByFunc(FunctorCreep,1));
  if (mod) {
    ARG1 = MkPairTerm(mod,ARG1);
  } else {
    ARG1 = MkPairTerm(TermProlog,ARG1);
  }
  LOCK(SignalLock);
  CreepFlag = CalculateStackGap();
  UNLOCK(SignalLock);
  P_before_spy = P;
  return (CallPredicate(PredCreep, B, PredCreep->CodeOfPred));
}

static Int
p_execute(void)
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  return(do_execute(t, CurrentModule));
}

static Int
p_execute_in_mod(void)
{				/* '$execute'(Goal)	 */
  return(do_execute(Deref(ARG1), IntOfTerm(Deref(ARG2))));
}

static Int
p_execute0(void)
{				/* '$execute0'(Goal,Mod)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG2);
  unsigned int    arity;
  Prop            pe;

  if (ActiveSignals) {
    return EnterCreepMode(t, mod);
  }
 restart_exec:
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,ARG3,"call/1");    
    return FALSE;
  } else if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    pe = PredPropByAtom(a, mod);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    register unsigned int    i;
    register CELL *pt;

    if (IsExtensionFunctor(f))
      return(FALSE);
    if (f == FunctorModule) {
      Term tmod = ArgOfTerm(1,t);
      if (!IsVarTerm(tmod) && IsAtomTerm(tmod)) {
	mod = tmod;
	t = ArgOfTerm(2,t);
	goto restart_exec;
      }
    }
    pe = PredPropByFunc(f, mod);
    arity = ArityOfFunctor(f);
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
  } else {
    Yap_Error(TYPE_ERROR_CALLABLE,ARG3,"call/1");    
    return FALSE;
  }
  /*	N = arity; */
  /* call may not define new system predicates!! */
  return CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred);
}

static Int
p_execute_nonstop(void)
{				/* '$execute_nonstop'(Goal,Mod)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG2);
  unsigned int    arity;
  Prop            pe;

 restart_exec:
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,ARG3,"call/1");    
    return FALSE;
  } else if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    pe = PredPropByAtom(a, mod);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    register unsigned int    i;
    register CELL *pt;

    if (IsExtensionFunctor(f))
      return(FALSE);
    if (f == FunctorModule) {
      Term tmod = ArgOfTerm(1,t);
      if (!IsVarTerm(tmod) && IsAtomTerm(tmod)) {
	mod = tmod;
	t = ArgOfTerm(2,t);
	goto restart_exec;
      }
    }
    pe = PredPropByFunc(f, mod);
    arity = ArityOfFunctor(f);
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
  } else {
    Yap_Error(TYPE_ERROR_CALLABLE,ARG3,"call/1");    
    return FALSE;
  }
  /*	N = arity; */
  /* call may not define new system predicates!! */
  if (RepPredProp(pe)->PredFlags & SpiedPredFlag) {
    return CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->cs.p_code.TrueCodeOfPred);
  } else {
    return CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred);
  }
}

static Int
p_execute_0(void)
{				/* '$execute_0'(Goal)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG2);
  Prop            pe;

  if (IsAtomTerm(t)) {
    Atom            a;
    a = AtomOfTerm(t);
    pe = PredPropByAtom(a, mod);
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    Int Arity, i;
    CELL *ptr;

    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call_with_args/1");
      return(FALSE);
    }
    pe = PredPropByFunc(f, mod);
    Arity = ArityOfFunctor(f);
    ptr = RepAppl(t)+1;
    for (i=1;i<=Arity;i++) {
      XREGS[i] = *ptr++;
    }
  } else {
    CELL *ptr;

    pe = PredPropByFunc(Yap_MkFunctor(AtomDot,2), mod);
    ptr = RepPair(t);
    XREGS[1] = ptr[0];
    XREGS[2] = ptr[1];
  }
  return (CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred));
}

static Int
p_execute_1(void)
{				/* '$execute_0'(Goal)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG3);
  Prop            pe;

  if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM,ARG1,"call_with_args/2");
    return(FALSE);
  }
  if (IsAtomTerm(t)) {
    Atom            a;
    a = AtomOfTerm(t);
    ARG1 = ARG2;
    pe = PredPropByFunc(Yap_MkFunctor(a,1),mod);
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    Int Arity, i;
    Atom a;
    CELL *ptr;

    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call_with_args/2");
      return(FALSE);
    }
    Arity = ArityOfFunctor(f);
    a = NameOfFunctor(f);
    pe = PredPropByFunc(Yap_MkFunctor(a,Arity+1), mod);
    XREGS[Arity+1] = ARG2;
    ptr = RepAppl(t)+1;
    for (i=1;i<=Arity;i++) {
      XREGS[i] = *ptr++;
    }
  } else {
    CELL *ptr;

    pe = PredPropByFunc(Yap_MkFunctor(AtomDot,3), mod);
    ptr = RepPair(t);
    XREGS[3] = ARG2;
    XREGS[1] = ptr[0];
    XREGS[2] = ptr[1];
  }
  return (CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred));
}

static Int
p_execute_2(void)
{				/* '$execute_2'(Goal)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG4);
  Prop            pe;

  if (IsAtomTerm(t)) {
    Atom            a;
    a = AtomOfTerm(t);
    ARG1 = ARG2;
    ARG2 = ARG3;
    pe = PredPropByFunc(Yap_MkFunctor(a,2),mod);
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    Int Arity, i;
    Atom a;
    CELL *ptr;

    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call_with_args/3");
      return(FALSE);
    }
    Arity = ArityOfFunctor(f);
    a = NameOfFunctor(f);
    pe = PredPropByFunc(Yap_MkFunctor(a,Arity+2), mod);
    XREGS[Arity+2] = ARG3;
    XREGS[Arity+1] = ARG2;
    ptr = RepAppl(t)+1;
    for (i=1;i<=Arity;i++) {
      XREGS[i] = *ptr++;
    }
  } else {
    CELL *ptr;

    pe = PredPropByFunc(Yap_MkFunctor(AtomDot,4), mod);
    ptr = RepPair(t);
    XREGS[4] = ARG3;
    XREGS[3] = ARG2;
    XREGS[1] = ptr[0];
    XREGS[2] = ptr[1];
  }
  return (CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred));
}

static Int
p_execute_3(void)
{				/* '$execute_3'(Goal)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG5);
  Prop            pe;

  if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM,ARG1,"call_with_args/4");
    return(FALSE);
  }
  if (IsAtomTerm(t)) {
    Atom            a;
    a = AtomOfTerm(t);
    ARG1 = ARG2;
    ARG2 = ARG3;
    ARG3 = ARG4;
    pe = PredPropByFunc(Yap_MkFunctor(a,3),mod);
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    Int Arity, i;
    Atom a;
    CELL *ptr;

    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call_with_args/2");
      return(FALSE);
    }
    Arity = ArityOfFunctor(f);
    a = NameOfFunctor(f);
    pe = PredPropByFunc(Yap_MkFunctor(a,Arity+3), mod);
    XREGS[Arity+3] = ARG4;
    XREGS[Arity+2] = ARG3;
    XREGS[Arity+1] = ARG2;
    ptr = RepAppl(t)+1;
    for (i=1;i<=Arity;i++) {
      XREGS[i] = *ptr++;
    }
  } else {
    CELL *ptr;

    pe = PredPropByFunc(Yap_MkFunctor(AtomDot,5), mod);
    ptr = RepPair(t);
    XREGS[5] = ARG4;
    XREGS[4] = ARG3;
    XREGS[3] = ARG2;
    XREGS[1] = ptr[0];
    XREGS[2] = ptr[1];
  }
  return (CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred));
}

static Int
p_execute_4(void)
{				/* '$execute_4'(Goal)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG6);
  Prop            pe;

  if (IsAtomTerm(t)) {
    Atom            a;
    a = AtomOfTerm(t);
    ARG1 = ARG2;
    ARG2 = ARG3;
    ARG3 = ARG4;
    ARG4 = ARG5;
    pe = PredPropByFunc(Yap_MkFunctor(a,4),mod);
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    Int Arity, i;
    Atom a;
    CELL *ptr;

    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call_with_args/5");
      return(FALSE);
    }
    Arity = ArityOfFunctor(f);
    a = NameOfFunctor(f);
    pe = PredPropByFunc(Yap_MkFunctor(a,Arity+4), mod);
    XREGS[Arity+4] = ARG5;
    XREGS[Arity+3] = ARG4;
    XREGS[Arity+2] = ARG3;
    XREGS[Arity+1] = ARG2;
    ptr = RepAppl(t)+1;
    for (i=1;i<=Arity;i++) {
      XREGS[i] = *ptr++;
    }
  } else {
    CELL *ptr;

    pe = PredPropByFunc(Yap_MkFunctor(AtomDot,6), mod);
    ptr = RepPair(t);
    XREGS[6] = ARG5;
    XREGS[5] = ARG4;
    XREGS[4] = ARG3;
    XREGS[3] = ARG2;
    XREGS[1] = ptr[0];
    XREGS[2] = ptr[1];
  }
  return (CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred));
}

static Int
p_execute_5(void)
{				/* '$execute_5'(Goal)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG7);
  Prop            pe;

  if (IsAtomTerm(t)) {
    Atom            a;
    a = AtomOfTerm(t);
    ARG1 = ARG2;
    ARG2 = ARG3;
    ARG3 = ARG4;
    ARG4 = ARG5;
    ARG5 = ARG6;
    pe = PredPropByFunc(Yap_MkFunctor(a,5),mod);
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    Int Arity, i;
    Atom a;
    CELL *ptr;

    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call_with_args/6");
      return(FALSE);
    }
    Arity = ArityOfFunctor(f);
    a = NameOfFunctor(f);
    pe = PredPropByFunc(Yap_MkFunctor(a,Arity+5), mod);
    XREGS[Arity+5] = ARG6;
    XREGS[Arity+4] = ARG5;
    XREGS[Arity+3] = ARG4;
    XREGS[Arity+2] = ARG3;
    XREGS[Arity+1] = ARG2;
    ptr = RepAppl(t)+1;
    for (i=1;i<=Arity;i++) {
      XREGS[i] = *ptr++;
    }
  } else {
    CELL *ptr;

    pe = PredPropByFunc(Yap_MkFunctor(AtomDot,7), mod);
    ptr = RepPair(t);
    XREGS[7] = ARG6;
    XREGS[6] = ARG5;
    XREGS[5] = ARG4;
    XREGS[4] = ARG3;
    XREGS[3] = ARG2;
    XREGS[1] = ptr[0];
    XREGS[2] = ptr[1];
  }
  return (CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred));
}

static Int
p_execute_6(void)
{				/* '$execute_6'(Goal)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG8);
  Prop            pe;

  if (IsAtomTerm(t)) {
    Atom            a;
    a = AtomOfTerm(t);
    ARG1 = ARG2;
    ARG2 = ARG3;
    ARG3 = ARG4;
    ARG4 = ARG5;
    ARG5 = ARG6;
    ARG6 = ARG7;
    pe = PredPropByFunc(Yap_MkFunctor(a,6),mod);
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    Int Arity, i;
    Atom a;
    CELL *ptr;

    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call_with_args/7");
      return(FALSE);
    }
    Arity = ArityOfFunctor(f);
    a = NameOfFunctor(f);
    pe = PredPropByFunc(Yap_MkFunctor(a,Arity+6), mod);
    XREGS[Arity+6] = ARG7;
    XREGS[Arity+5] = ARG6;
    XREGS[Arity+4] = ARG5;
    XREGS[Arity+3] = ARG4;
    XREGS[Arity+2] = ARG3;
    XREGS[Arity+1] = ARG2;
    ptr = RepAppl(t)+1;
    for (i=1;i<=Arity;i++) {
      XREGS[i] = *ptr++;
    }
  } else {
    CELL *ptr;

    pe = PredPropByFunc(Yap_MkFunctor(AtomDot,8), mod);
    ptr = RepPair(t);
    XREGS[8] = ARG7;
    XREGS[7] = ARG6;
    XREGS[6] = ARG5;
    XREGS[5] = ARG4;
    XREGS[4] = ARG3;
    XREGS[3] = ARG2;
    XREGS[1] = ptr[0];
    XREGS[2] = ptr[1];
  }
  return (CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred));
}

static Int
p_execute_7(void)
{				/* '$execute_7'(Goal)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG9);
  Prop            pe;

  if (IsAtomTerm(t)) {
    Atom            a;
    a = AtomOfTerm(t);
    ARG1 = ARG2;
    ARG2 = ARG3;
    ARG3 = ARG4;
    ARG4 = ARG5;
    ARG5 = ARG6;
    ARG6 = ARG7;
    ARG7 = ARG8;
    pe = PredPropByFunc(Yap_MkFunctor(a,7),mod);
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    Int Arity, i;
    Atom a;
    CELL *ptr;

    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call_with_args/8");
      return(FALSE);
    }
    Arity = ArityOfFunctor(f);
    a = NameOfFunctor(f);
    pe = PredPropByFunc(Yap_MkFunctor(a,Arity+7), mod);
    XREGS[Arity+7] = ARG8;
    XREGS[Arity+6] = ARG7;
    XREGS[Arity+5] = ARG6;
    XREGS[Arity+4] = ARG5;
    XREGS[Arity+3] = ARG4;
    XREGS[Arity+2] = ARG3;
    XREGS[Arity+1] = ARG2;
    ptr = RepAppl(t)+1;
    for (i=1;i<=Arity;i++) {
      XREGS[i] = *ptr++;
    }
  } else {
    CELL *ptr;

    pe = PredPropByFunc(Yap_MkFunctor(AtomDot,9), mod);
    ptr = RepPair(t);
    XREGS[9] = ARG8;
    XREGS[8] = ARG7;
    XREGS[7] = ARG6;
    XREGS[6] = ARG5;
    XREGS[5] = ARG4;
    XREGS[4] = ARG3;
    XREGS[3] = ARG2;
    XREGS[1] = ptr[0];
    XREGS[2] = ptr[1];
  }
  return (CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred));
}

static Int
p_execute_8(void)
{				/* '$execute_8'(Goal)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG10);
  Prop            pe;

  if (IsAtomTerm(t)) {
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
    pe = PredPropByFunc(Yap_MkFunctor(a,8),mod);
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    Int Arity, i;
    Atom a;
    CELL *ptr;

    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call_with_args/9");
      return(FALSE);
    }
    Arity = ArityOfFunctor(f);
    a = NameOfFunctor(f);
    pe = PredPropByFunc(Yap_MkFunctor(a,Arity+8), mod);
    XREGS[Arity+8] = ARG9;
    XREGS[Arity+7] = ARG8;
    XREGS[Arity+6] = ARG7;
    XREGS[Arity+5] = ARG6;
    XREGS[Arity+4] = ARG5;
    XREGS[Arity+3] = ARG4;
    XREGS[Arity+2] = ARG3;
    XREGS[Arity+1] = ARG2;
    ptr = RepAppl(t)+1;
    for (i=1;i<=Arity;i++) {
      XREGS[i] = *ptr++;
    }
  } else {
    CELL *ptr;

    pe = PredPropByFunc(Yap_MkFunctor(AtomDot,10), mod);
    ptr = RepPair(t);
    XREGS[10] = ARG9;
    XREGS[9] = ARG8;
    XREGS[8] = ARG7;
    XREGS[7] = ARG6;
    XREGS[6] = ARG5;
    XREGS[5] = ARG4;
    XREGS[4] = ARG3;
    XREGS[3] = ARG2;
    XREGS[1] = ptr[0];
    XREGS[2] = ptr[1];
  }
  return (CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred));
}

static Int
p_execute_9(void)
{				/* '$execute_9'(Goal)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG11);
  Prop            pe;

  if (IsAtomTerm(t)) {
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
    pe = PredPropByFunc(Yap_MkFunctor(a,9),mod);
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    Int Arity, i;
    Atom a;
    CELL *ptr;

    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call_with_args/10");
      return(FALSE);
    }
    Arity = ArityOfFunctor(f);
    a = NameOfFunctor(f);
    pe = PredPropByFunc(Yap_MkFunctor(a,Arity+9), mod);
    XREGS[Arity+9] = ARG10;
    XREGS[Arity+8] = ARG9;
    XREGS[Arity+7] = ARG8;
    XREGS[Arity+6] = ARG7;
    XREGS[Arity+5] = ARG6;
    XREGS[Arity+4] = ARG5;
    XREGS[Arity+3] = ARG4;
    XREGS[Arity+2] = ARG3;
    XREGS[Arity+1] = ARG2;
    ptr = RepAppl(t)+1;
    for (i=1;i<=Arity;i++) {
      XREGS[i] = *ptr++;
    }
  } else {
    CELL *ptr;

    pe = PredPropByFunc(Yap_MkFunctor(AtomDot,11), mod);
    ptr = RepPair(t);
    XREGS[11] = ARG10;
    XREGS[10] = ARG9;
    XREGS[9] = ARG8;
    XREGS[8] = ARG7;
    XREGS[7] = ARG6;
    XREGS[6] = ARG5;
    XREGS[5] = ARG4;
    XREGS[4] = ARG3;
    XREGS[3] = ARG2;
    XREGS[1] = ptr[0];
    XREGS[2] = ptr[1];
  }
  return (CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred));
}

static Int
p_execute_10(void)
{				/* '$execute_10'(Goal)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG12);
  Prop            pe;

  if (IsAtomTerm(t)) {
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
    pe = PredPropByFunc(Yap_MkFunctor(a,10),mod);
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    Int Arity, i;
    Atom a;
    CELL *ptr;

    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call_with_args/11");
      return(FALSE);
    }
    Arity = ArityOfFunctor(f);
    a = NameOfFunctor(f);
    pe = PredPropByFunc(Yap_MkFunctor(a,Arity+10), mod);
    XREGS[Arity+10] = ARG11;
    XREGS[Arity+9] = ARG10;
    XREGS[Arity+8] = ARG9;
    XREGS[Arity+7] = ARG8;
    XREGS[Arity+6] = ARG7;
    XREGS[Arity+5] = ARG6;
    XREGS[Arity+4] = ARG5;
    XREGS[Arity+3] = ARG4;
    XREGS[Arity+2] = ARG3;
    XREGS[Arity+1] = ARG2;
    ptr = RepAppl(t)+1;
    for (i=1;i<=Arity;i++) {
      XREGS[i] = *ptr++;
    }
  } else {
    CELL *ptr;

    pe = PredPropByFunc(Yap_MkFunctor(AtomDot,12), mod);
    ptr = RepPair(t);
    XREGS[12] = ARG11;
    XREGS[11] = ARG10;
    XREGS[10] = ARG9;
    XREGS[9] = ARG8;
    XREGS[8] = ARG7;
    XREGS[7] = ARG6;
    XREGS[6] = ARG5;
    XREGS[5] = ARG4;
    XREGS[4] = ARG3;
    XREGS[3] = ARG2;
    XREGS[1] = ptr[0];
    XREGS[2] = ptr[1];
  }
  return (CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred));
}

#ifdef DEPTH_LIMIT
static Int
p_execute_depth_limit(void) {
  Term d = Deref(ARG2);
  if (IsVarTerm(d)) {
    Yap_Error(INSTANTIATION_ERROR,d,"depth_bound_call/2");    
  } else if (!IsIntTerm(d)) {
    Yap_Error(TYPE_ERROR_INTEGER, d, "depth_bound_call/2");
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
  return PRED_GOAL_EXPANSION_ON;
}

static int
exec_absmi(int top)
{
  int lval;
  if (top && (lval = sigsetjmp (Yap_RestartEnv, 1)) != 0) {
    switch(lval) {
    case 1:
      { /* restart */
	/* otherwise, SetDBForThrow will fail entering critical mode */
	Yap_PrologMode = UserMode;
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
	yap_flags[SPY_CREEP_FLAG] = 0;
	LOCK(SignalLock);
	CreepFlag = CalculateStackGap();
	Yap_PrologMode = UserMode;
	UNLOCK(SignalLock);
	P = (yamop *)FAILCODE;
      }
      break;
    case 2:
      {
	/* arithmetic exception */
	/* must be done here, otherwise siglongjmp will clobber all the registers */
	Yap_Error(Yap_matherror,TermNil,NULL);
	/* reset the registers so that we don't have trash in abstract machine */
	Yap_set_fpu_exceptions(yap_flags[LANGUAGE_MODE_FLAG] == 1);
	P = (yamop *)FAILCODE;
	Yap_PrologMode = UserMode;
      }
      break;
    case 3:
      { /* saved state */
	return(FALSE);
      }
    default:
      /* do nothing */
      Yap_PrologMode = UserMode;
    }
  } else {
    Yap_PrologMode = UserMode;
  }
  return(Yap_absmi(0));
}

static Term
do_goal(Term t, yamop *CodeAdr, int arity, CELL *pt, int top)
{
  choiceptr saved_b = B;
  Term out = 0L;

  /* create an initial pseudo environment so that when garbage
     collection is going up in the environment chain it doesn't get
     confused */
  EX = 0L;
  //  sl = Yap_InitSlot(t);
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
  /* CP = YESCODE; */
  /* keep a place where you can inform you had an exception */
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
  B->cp_ap    = NOCODE;
  B->cp_env   = ENV;
  B->cp_b     = saved_b;
#ifdef DEPTH_LIMIT
  B->cp_depth = DEPTH;
#endif /* DEPTH_LIMIT */
  YENV = ASP = (CELL *)B;
  HB = H;
#if defined(YAPOR) || defined(THREADS)
  WPP = NULL;
#endif
  YENV[E_CB] = Unsigned (B);
  P = (yamop *) CodeAdr;
  CP = YESCODE;
  S = CellPtr (RepPredProp (PredPropByFunc (Yap_MkFunctor(AtomCall, 1),0)));	/* A1 mishaps */

  out = exec_absmi(top);
  //  if (out) {
  //    out = Yap_GetFromSlot(sl);
  //  }
  //  Yap_RecoverSlots(1);
  return out;
}

int
Yap_exec_absmi(int top)
{
  return exec_absmi(top);
}


Int
Yap_execute_goal(Term t, int nargs, Term mod)
{
  Int             out;
  yamop        *CodeAdr;
  yamop *saved_p, *saved_cp;
  Prop pe;
  PredEntry *ppe;
  CELL *pt;
  /* preserve the current restart environment */
  /* visualc*/
  /* just keep the difference because of possible garbage collections */


  saved_p = P;
  saved_cp = CP;
  
  if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    pt = NULL;
    pe = PredPropByAtom(a, mod);
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);

    if (IsBlobFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE,t,"call/1");
      return(FALSE);
    }
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t)+1;
    pe = PredPropByFunc(f, mod);
  } else {
    Yap_Error(TYPE_ERROR_CALLABLE,t,"call/1");
    return(FALSE);
  }
  ppe = RepPredProp(pe);
  if (pe == NIL) {
    return(CallMetaCall(mod));
  }
  READ_LOCK(ppe->PRWLock);
  if (IsAtomTerm(t)) {
    CodeAdr = RepPredProp (pe)->CodeOfPred;
    READ_UNLOCK(ppe->PRWLock);
    out = do_goal(t, CodeAdr, 0, pt, FALSE);
  } else {
    Functor f = FunctorOfTerm(t);
    CodeAdr = RepPredProp (pe)->CodeOfPred;
    READ_UNLOCK(ppe->PRWLock);
    out = do_goal(t, CodeAdr, ArityOfFunctor(f), pt, FALSE);
  }

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
#ifdef TABLING
    abolish_incomplete_subgoals(B);
#endif /* TABLING */
    /* find out where we have the old arguments */
    old_B = ((choiceptr)(ENV-(EnvSizeInCells+nargs+1)))-1;
    CP   = saved_cp;
    P    = saved_p;
    ASP  = ENV;
    *--ASP = MkIntTerm(0);
#ifdef DEPTH_LIMIT
    DEPTH= ENV[E_DEPTH];
#endif
    ENV  = (CELL *)(ENV[E_E]);
    /* we have failed, and usually we would backtrack to this B,
       trouble is, we may also have a delayed cut to do */
    if (B != NULL)
      HB   = B->cp_h;
    YENV = ENV;
    return(TRUE);
  } else if (out == 0) {
    ASP  = B->cp_env;
    P    = saved_p;
    CP   = saved_cp;
    H    = B->cp_h;
#ifdef DEPTH_LIMIT
    DEPTH= B->cp_depth;
#endif
    /* ASP should be set to the top of the local stack when we
       did the call */
    ASP = B->cp_env;
    /* YENV should be set to the current environment */
    YENV = ENV  = (CELL *)((B->cp_env)[E_E]);
    B    = B->cp_b;
    SET_BB(B);
    HB = PROTECT_FROZEN_H(B);
    return(FALSE);
  } else {
    Yap_Error(SYSTEM_ERROR,TermNil,"emulator crashed");
    return(FALSE);
  }
}

void
Yap_trust_last(void)
{
  ASP  = B->cp_env;
  P    = (yamop *)(B->cp_env[E_CP]);
  CP   = B->cp_cp;
  H    = B->cp_h;
#ifdef DEPTH_LIMIT
  DEPTH= B->cp_depth;
#endif
  YENV= ASP = B->cp_env;
  ENV  = (CELL *)((B->cp_env)[E_E]);
  B    = B->cp_b;
  if (B) {
    SET_BB(B);
    HB = PROTECT_FROZEN_H(B);
  }
}

Term
Yap_RunTopGoal(Term t)
{
  yamop        *CodeAdr;
  Prop pe;
  PredEntry *ppe;
  CELL *pt;
  UInt arity;
  Term mod = CurrentModule;
  Term goal_out = 0;

 restart_runtopgoal:
  if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    pt = NULL;
    pe = PredPropByAtom(a, CurrentModule);
    arity = 0;
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);

    if (IsBlobFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE,t,"call/1");
      return(FALSE);
    }
    if (f == FunctorModule) {
      Term tmod = ArgOfTerm(1,t);
      if (!IsVarTerm(tmod) && IsAtomTerm(tmod)) {
	mod = tmod;
	t = ArgOfTerm(2,t);
	goto restart_runtopgoal;
      }
    }
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pe = Yap_GetPredPropByFunc(f, CurrentModule);
    pt = RepAppl(t)+1;
    arity = ArityOfFunctor(f); 
  } else {
    Yap_Error(TYPE_ERROR_CALLABLE,t,"call/1");
    return(FALSE);
  }
  ppe = RepPredProp(pe);
  if (pe == NIL) {
    /* we must always start the emulator with Prolog code */
    return FALSE;
  }
  READ_LOCK(ppe->PRWLock);
  CodeAdr = ppe->CodeOfPred;
  READ_UNLOCK(ppe->PRWLock);
  if (Yap_TrailTop - HeapTop < 2048) {
    Yap_PrologMode = BootMode;
    Yap_Error(SYSTEM_ERROR,TermNil,
	  "unable to boot because of too little heap space");
  }
  goal_out = do_goal(t, CodeAdr, arity, pt, TRUE);
  return(goal_out);
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
    Yap_Error(INSTANTIATION_ERROR,t,"support for coroutining");    
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
    Yap_Error(INSTANTIATION_ERROR,t,"support for coroutining");    
    return(FALSE);
  }
  d0 = Deref(ARG2);
  if (!IsAtomTerm(t)) {
    restore_regs(t);
  }
  if (IsVarTerm(d0)) {
    Yap_Error(INSTANTIATION_ERROR,d0,"support for coroutining");    
    return(FALSE);
  }
  if (!IsIntegerTerm(d0)) {
    return(FALSE);
  }
#if SBA
  pt0 = (choiceptr)IntegerOfTerm(d0);
#else
  pt0 = (choiceptr)(LCL0-IntOfTerm(d0));
#endif
  /* find where to cut to */
  if (pt0 > B) {
    /* Wow, we're gonna cut!!! */
#ifdef YAPOR
    CUT_prune_to(pt0);
#else
    B = pt0;
#endif /* YAPOR */
#ifdef TABLING
    abolish_incomplete_subgoals(B);
#endif /* TABLING */
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


static Int
JumpToEnv(Term t) {
  yamop *pos = NEXTOP(PredDollarCatch->cs.p_code.TrueCodeOfPred,ld),
    *catchpos = NEXTOP(PredHandleThrow->cs.p_code.TrueCodeOfPred,ld);
  CELL *env;
  choiceptr first_func = NULL, B0 = B;

  do {
    /* find the first choicepoint that may be a catch */
    while (B != NULL && B->cp_ap != pos) {
      /* we are already doing a catch */
      if (B->cp_ap == catchpos) {
	P = (yamop *)FAILCODE;
	if (first_func != NULL) {
	  B = first_func;
	}
	return(FALSE);
      }
      if (B->cp_ap == NOCODE) {
	/* up to the C-code to deal with this! */
	B->cp_h = H;
	EX = t;
	return(FALSE);
      }
      B = B->cp_b;
    }
    /* uncaught throw */
    if (B == NULL) {
      B = B0;
#if PUSH_REGS
      restore_absmi_regs(&Yap_standard_regs);
#endif
      siglongjmp(Yap_RestartEnv,1);
    }
    /* is it a continuation? */
    env = B->cp_env;
    while (env > ENV)
      ENV = (CELL *)ENV[E_E];
    /* yes, we found it ! */
    if (env == ENV) break;
    /* oops, try next */
    B = B->cp_b;
  } while (TRUE);
  /* step one environment above */
  B->cp_cp = (yamop *)env[E_CP];
  B->cp_ap = NEXTOP(PredHandleThrow->CodeOfPred,ld);
  B->cp_env = (CELL *)env[E_E];
  /* cannot recover Heap because of copy term :-( */
  B->cp_h = H;
  /* I could backtrack here, but it is easier to leave the unwinding
     to the emulator */
  B->cp_a3 = t;
  P = (yamop *)FAILCODE;
  if (first_func != NULL) {
    B = first_func;
  }
#ifdef TABLING
  abolish_incomplete_subgoals(B);
#endif /* TABLING */
  return(FALSE);
}

Int
Yap_JumpToEnv(Term t) {
  return JumpToEnv(t);
}


/* This does very nasty stuff!!!!! */
static Int
p_jump_env(void) {
  return(JumpToEnv(Deref(ARG1)));
}

/* set up a meta-call based on . context info */
static Int
p_generate_pred_info(void) {
  ARG1 = ARG3 = ENV[-EnvSizeInCells-1];
  ARG4 = ENV[-EnvSizeInCells-3];
  ARG2 = cp_as_integer((choiceptr)ENV[E_CB]);
  return TRUE;
}

void
Yap_InitYaamRegs(void)
{

#if PUSH_REGS
  /* Guarantee that after a longjmp we go back to the original abstract
     machine registers */
#ifdef THREADS
  int myworker_id = worker_id;
  pthread_setspecific(Yap_yaamregs_key, (const void *)ThreadHandle[myworker_id].default_yaam_regs);
  ThreadHandle[myworker_id].current_yaam_regs = ThreadHandle[myworker_id].default_yaam_regs;
  worker_id = myworker_id;
#else
  Yap_regp = &Yap_standard_regs;
#endif
#endif /* PUSH_REGS */
  Yap_PutValue (AtomBreak, MkIntTerm (0));
  TR = (tr_fr_ptr)Yap_TrailBase;
#ifdef COROUTINING
  H = H0 = ((CELL *) Yap_GlobalBase)+ 2048;
#else
  H = H0 = (CELL *) Yap_GlobalBase;
#endif
  LCL0 = ASP = (CELL *) Yap_LocalBase;
  /* notice that an initial choice-point and environment
   *must* be created since for the garbage collector to work */
  B = NULL;
  ENV = NULL;
  P = CP = YESCODE;
#ifdef DEPTH_LIMIT
  DEPTH = RESET_DEPTH();
#endif
  STATIC_PREDICATES_MARKED = FALSE;
#ifdef FROZEN_STACKS
  H = HB = H0 = H_FZ = H_BASE;
#ifdef SBA
  BSEG =
#endif /* SBA */
  BBREG = B_FZ = B_BASE;
  TR = TR_FZ = TR_BASE;
#endif /* FROZEN_STACKS */
  LOCK(SignalLock);
  CreepFlag = CalculateStackGap();
  UNLOCK(SignalLock);
  EX = 0L;
  /* for slots to work */
  *--ASP = MkIntTerm(0);
#if COROUTINING
  RESET_VARIABLE((CELL *)Yap_GlobalBase);
  DelayedVars = Yap_NewTimedVar((CELL)Yap_GlobalBase);
  WokenGoals = Yap_NewTimedVar(TermNil);
  MutableList = Yap_NewTimedVar(TermNil);
  AttsMutableList = Yap_NewTimedVar(TermNil);
#endif
#if defined(YAPOR) || defined(THREADS)
  PP = NULL;
  WPP = NULL;
  PREG_ADDR = NULL;
#endif
}


void 
Yap_InitExecFs(void)
{
  Yap_InitComma();
  Yap_InitCPred("$execute", 1, p_execute, 0);
  Yap_InitCPred("$execute_in_mod", 2, p_execute_in_mod, 0);
  Yap_InitCPred("$call_with_args", 2, p_execute_0, 0);
  Yap_InitCPred("$call_with_args", 3, p_execute_1, 0);
  Yap_InitCPred("$call_with_args", 4, p_execute_2, 0);
  Yap_InitCPred("$call_with_args", 5, p_execute_3, 0);
  Yap_InitCPred("$call_with_args", 6, p_execute_4, 0);
  Yap_InitCPred("$call_with_args", 7, p_execute_5, 0);
  Yap_InitCPred("$call_with_args", 8, p_execute_6, 0);
  Yap_InitCPred("$call_with_args", 9, p_execute_7, 0);
  Yap_InitCPred("$call_with_args", 10, p_execute_8, 0);
  Yap_InitCPred("$call_with_args", 11, p_execute_9, 0);
  Yap_InitCPred("$call_with_args", 12, p_execute_10, 0);
#ifdef DEPTH_LIMIT
  Yap_InitCPred("$execute_under_depth_limit", 2, p_execute_depth_limit, 0);
#endif
  Yap_InitCPred("$execute0", 2, p_execute0, 0);
  Yap_InitCPred("$execute_nonstop", 2, p_execute_nonstop, 0);
  Yap_InitCPred("$save_current_choice_point", 1, p_save_cp, 0);
  Yap_InitCPred("$pred_goal_expansion_on", 0, p_pred_goal_expansion_on, SafePredFlag);
  Yap_InitCPred("$restore_regs", 1, p_restore_regs, SafePredFlag);
  Yap_InitCPred("$restore_regs", 2, p_restore_regs2, SafePredFlag);
  Yap_InitCPred("$clean_ifcp", 1, p_clean_ifcp, SafePredFlag);
  Yap_InitCPred("$jump_env_and_store_ball", 1, p_jump_env, 0);
  Yap_InitCPred("$generate_pred_info", 4, p_generate_pred_info, 0);
}

