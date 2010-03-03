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
#include "attvar.h"
#ifdef CUT_C
#include "cut_c.h"
#endif
#if defined MYDDAS_ODBC || defined MYDDAS_MYSQL
#include "myddas.h"
#endif

STATIC_PROTO(Int  CallPredicate, (PredEntry *, choiceptr, yamop *));
STATIC_PROTO(Int  EnterCreepMode, (Term, Term));
STATIC_PROTO(Int  p_save_cp, (void));
STATIC_PROTO(Int  p_execute, (void));
STATIC_PROTO(Int  p_execute0, (void));

static Term
cp_as_integer(choiceptr cp)
{
  return(MkIntegerTerm(LCL0-(CELL *)cp));
}

static choiceptr
cp_from_integer(Term cpt)
{
  return (choiceptr)(LCL0-IntegerOfTerm(cpt));
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
#ifdef DEPTH_LIMIT
  if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is prolog */
    if (pen->ModuleOfPred) {
      if (DEPTH == MkIntTerm(0)) {
	UNLOCK(pen->PELock);
	return FALSE;
      }
      else DEPTH = RESET_DEPTH();
    }
  } else if (pen->ModuleOfPred)
    DEPTH -= MkIntConstant(2);
#endif	/* DEPTH_LIMIT */
  if (P->opc != EXECUTE_CPRED_OP_CODE) {
    CP = P;
    ENV = YENV;
    YENV = ASP;
  }
  /* make sure we have access to the user given cut */
  YENV[E_CB] = (CELL) cut_pt;
  P = code;
  return TRUE;
}

inline static Int
CallMetaCall(Term mod) {
  ARG2 = cp_as_integer(B); /* p_save_cp */
  ARG3 = ARG1;
  if (mod) {
    ARG4 = mod;
  } else {
    ARG4 = TermProlog;
  }
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

static Int
p_save_env_b(void)
{
  Term t = Deref(ARG1);
  Term td;
#if SHADOW_HB
  register CELL *HBREG = HB;
#endif
  if (!IsVarTerm(t)) return(FALSE);
  td = cp_as_integer((choiceptr)YENV[E_CB]);
  BIND((CELL *)t,td,bind_save_cp);
#ifdef COROUTINING
  DO_TRAIL(CellPtr(t), td);
  if (CellPtr(t) < H0) Yap_WakeUp((CELL *)t);
 bind_save_cp:
#endif
  return(TRUE);
}

static Int
p_trail_suspension_marker(void)
{
  Term t = Deref(ARG1);
  
  TrailTerm(TR) = AbsPair((CELL*)t);
  TR++;
  return TRUE;
}

inline static Int
do_execute(Term t, Term mod)
{
  /* first do predicate expansion, even before you process signals.
     This way you don't get to spy goal_expansion(). */
  if (PRED_GOAL_EXPANSION_ALL) {
    LOCK(SignalLock);
    /* disable creeping when we do goal expansion */
    if (ActiveSignals & YAP_CREEP_SIGNAL && !Yap_InterruptsDisabled) {
      ActiveSignals &= ~YAP_CREEP_SIGNAL;
      CreepFlag = CalculateStackGap();
    }
    UNLOCK(SignalLock);
    return CallMetaCall(mod);
  } else if (ActiveSignals  && !Yap_InterruptsDisabled) {
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
      return CallError(TYPE_ERROR_CALLABLE, t);
    }
    arity = ArityOfFunctor(f);
    if (arity > MaxTemps) {
      return CallError(TYPE_ERROR_CALLABLE, t);
    }    
    pen = RepPredProp(PredPropByFunc(f, mod));
    /* You thought we would be over by now */
    /* but no meta calls require special preprocessing */
    if (pen->PredFlags & (GoalExPredFlag|MetaPredFlag)) {
      if (f == FunctorModule) {
	Term tmod = ArgOfTerm(1,t);
	if (!IsVarTerm(tmod) && IsAtomTerm(tmod)) {
	  mod = tmod;
	  t = ArgOfTerm(2,t);
	  goto restart_exec;
	} else {
	  if (IsVarTerm(tmod)) {
	    return CallError(INSTANTIATION_ERROR,t);
	  } else {
	    return CallError(TYPE_ERROR_ATOM,t);
	  }
	}
      } else {
	return CallMetaCall(mod);
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

static Term
copy_execn_to_heap(Functor f, CELL *pt, unsigned int n, unsigned int arity, Term mod)
{
  CELL *h0 = H;
  Term tf;
  unsigned int i;

  if (arity == 2 &&
      NameOfFunctor(f) == AtomDot) {
    for (i = 0; i<arity-n;i++) {
      *H++ = pt[i];
    }
    for (i=0; i< n; i++) {
      *H++ = h0[(int)(i-n)];
    }
    tf = AbsPair(h0);
  } else {
    *H++ = (CELL)f;
    for (i = 0; i<arity-n;i++) {
      *H++ = pt[i];
    }
    for (i=0; i< n; i++) {
      *H++ = h0[(int)(i-n)];
    }
    tf = AbsAppl(h0);
  }
  if (mod != CurrentModule) {
    CELL *h0 = H;
    *H++ = (CELL)FunctorModule;
    *H++ = mod;
    *H++ = tf;
    tf = AbsAppl(h0);
  }
  return tf;
}

inline static Int
do_execute_n(Term t, Term mod, unsigned int n)
{
  Functor f;
  Atom Name;
  register CELL *pt;
  PredEntry *pen;
  unsigned int i, arity;
  int j = -n;

 restart_exec:
  if (IsVarTerm(t)) {
    return CallError(INSTANTIATION_ERROR, mod);
  } else if (IsAtomTerm(t)) {
    arity = n;
    Name = AtomOfTerm(t);
    pt = NULL;
  } else if (IsIntTerm(t)) {
    return CallError(TYPE_ERROR_CALLABLE, mod);
  } else if (IsPairTerm(t)) {
    arity = n+2;
    pt = RepPair(t);
    Name = AtomOfTerm(TermNil);
  } else /* if (IsApplTerm(t)) */ {
    f = FunctorOfTerm(t);
    while (f == FunctorModule) {
      Term tmod = ArgOfTerm(1,t);
      if (!IsVarTerm(tmod) && IsAtomTerm(tmod)) {
	mod = tmod;
	t = ArgOfTerm(2,t);
	goto restart_exec;
      } else {
	if (IsVarTerm(tmod)) {
	  return CallError(INSTANTIATION_ERROR,t);
	} else {
	  return CallError(TYPE_ERROR_ATOM,t);
	}
      }
    }
    arity = ArityOfFunctor(f)+n;
    Name = NameOfFunctor(f);
    pt = RepAppl(t)+1;
  }
  f = Yap_MkFunctor(Name,arity);
  if (IsExtensionFunctor(f)) {
    return CallError(TYPE_ERROR_CALLABLE, mod);
  }
  if (PRED_GOAL_EXPANSION_ALL) {
    LOCK(SignalLock);
    /* disable creeping when we do goal expansion */
    if (ActiveSignals & YAP_CREEP_SIGNAL && !Yap_InterruptsDisabled) {
      ActiveSignals &= ~YAP_CREEP_SIGNAL;
      CreepFlag = CalculateStackGap();
    }
    UNLOCK(SignalLock);
    ARG1 = copy_execn_to_heap(f, pt, n, arity, mod);
    return CallMetaCall(mod);
  } else if (ActiveSignals  && !Yap_InterruptsDisabled) {
    return EnterCreepMode(copy_execn_to_heap(f, pt, n, arity, CurrentModule), mod);
  }
  if (arity > MaxTemps) {
    return CallError(TYPE_ERROR_CALLABLE, t);
  }
  pen = RepPredProp(PredPropByFunc(f, mod));
  /* You thought we would be over by now */
  /* but no meta calls require special preprocessing */
  if (pen->PredFlags & (GoalExPredFlag|MetaPredFlag)) {
    ARG1 = copy_execn_to_heap(f, pt, n, arity, mod);
    return(CallMetaCall(mod));
  }
  /* now let us do what we wanted to do from the beginning !! */
  /* I cannot use the standard macro here because
     otherwise I would dereference the argument and
     might skip a svar */
  for (i = 1; i <= arity-n; i++) {
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
  for (i = arity-n+1; i <= arity; i++,j++) {
    XREGS[i] = H[j];
  }
  return CallPredicate(pen, B, pen->CodeOfPred);
}

static Int
EnterCreepMode(Term t, Term mod) {
  PredEntry *PredCreep;

  if (ActiveSignals & YAP_CDOVF_SIGNAL) {
    ARG1 = t;
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, "YAP failed to grow heap at meta-call");
    }
    if (!ActiveSignals) {
      return do_execute(ARG1, mod);
    }
  }
  PP = PredMetaCall;
  PredCreep = RepPredProp(PredPropByFunc(FunctorCreep,1));
  if (mod) {
    ARG1 = MkPairTerm(mod,t);
  } else {
    ARG1 = MkPairTerm(TermProlog,t);
  }
  LOCK(SignalLock);
  CreepFlag = CalculateStackGap();
  UNLOCK(SignalLock);
  P_before_spy = P;
  return CallPredicate(PredCreep, B, PredCreep->CodeOfPred);
}

static Int
p_execute(void)
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  return do_execute(t, CurrentModule);
}

static void
heap_store(Term t)
{
  if (IsVarTerm(t)) {
    if (VarOfTerm(t) < H) {
      *H++ = t;
    } else {
      RESET_VARIABLE(H);
      Bind_Local(VarOfTerm(t), (CELL)H);
      H++;
    }
  } else {
    *H++ = t;
  }
}

static Int
p_execute2(void)
{				/* '$execute'(Goal)	 */
  Term       t = Deref(ARG1);
  heap_store(Deref(ARG2));
  return do_execute_n(t, CurrentModule, 1);
}

static Int
p_execute3(void)
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2));
  heap_store(Deref(ARG3));
  return do_execute_n(t, CurrentModule, 2);
}

static Int
p_execute4(void)
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2));
  heap_store(Deref(ARG3));
  heap_store(Deref(ARG4));
  return do_execute_n(t, CurrentModule, 3);
}

static Int
p_execute5(void)
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2));
  heap_store(Deref(ARG3));
  heap_store(Deref(ARG4));
  heap_store(Deref(ARG5));
  return do_execute_n(t, CurrentModule, 4);
}

static Int
p_execute6(void)
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2));
  heap_store(Deref(ARG3));
  heap_store(Deref(ARG4));
  heap_store(Deref(ARG5));
  heap_store(Deref(ARG6));
  return do_execute_n(t, CurrentModule, 5);
}

static Int
p_execute7(void)
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2));
  heap_store(Deref(ARG3));
  heap_store(Deref(ARG4));
  heap_store(Deref(ARG5));
  heap_store(Deref(ARG6));
  heap_store(Deref(ARG7));
  return do_execute_n(t, CurrentModule, 6);
}

static Int
p_execute8(void)
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2));
  heap_store(Deref(ARG3));
  heap_store(Deref(ARG4));
  heap_store(Deref(ARG5));
  heap_store(Deref(ARG6));
  heap_store(Deref(ARG7));
  heap_store(Deref(ARG8));
  return do_execute_n(t, CurrentModule, 7);
}

static Int
p_execute9(void)
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2));
  heap_store(Deref(ARG3));
  heap_store(Deref(ARG4));
  heap_store(Deref(ARG5));
  heap_store(Deref(ARG6));
  heap_store(Deref(ARG7));
  heap_store(Deref(ARG8));
  heap_store(Deref(ARG9));
  return do_execute_n(t, CurrentModule, 8);
}

static Int
p_execute10(void)
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2));
  heap_store(Deref(ARG3));
  heap_store(Deref(ARG4));
  heap_store(Deref(ARG5));
  heap_store(Deref(ARG6));
  heap_store(Deref(ARG7));
  heap_store(Deref(ARG8));
  heap_store(Deref(ARG9));
  heap_store(Deref(ARG10));
  return(do_execute_n(t, CurrentModule, 9));
}

static Int
p_execute11(void)
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2));
  heap_store(Deref(ARG3));
  heap_store(Deref(ARG4));
  heap_store(Deref(ARG5));
  heap_store(Deref(ARG6));
  heap_store(Deref(ARG7));
  heap_store(Deref(ARG8));
  heap_store(Deref(ARG9));
  heap_store(Deref(ARG10));
  heap_store(Deref(ARG11));
  return(do_execute_n(t, CurrentModule, 10));
}

static Int
p_execute12(void)
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2));
  heap_store(Deref(ARG3));
  heap_store(Deref(ARG4));
  heap_store(Deref(ARG5));
  heap_store(Deref(ARG6));
  heap_store(Deref(ARG7));
  heap_store(Deref(ARG8));
  heap_store(Deref(ARG9));
  heap_store(Deref(ARG10));
  heap_store(Deref(ARG11));
  heap_store(Deref(ARG12));
  return(do_execute_n(t, CurrentModule, 11));
}

static Int
p_execute_clause(void)
{				/* '$execute_clause'(Goal)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG2);
  choiceptr       cut_cp = cp_from_integer(Deref(ARG4));
  unsigned int    arity;
  Prop            pe;
  yamop *code;
  Term            clt = Deref(ARG3);

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
    if (arity > MaxTemps) {
      return CallError(TYPE_ERROR_CALLABLE, t);
    }
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
  if (RepPredProp(pe)->PredFlags & MegaClausePredFlag) {
    code = Yap_MegaClauseFromTerm(clt);    
  } else {
    code = Yap_ClauseFromTerm(clt)->ClCode;
  }
  if (ActiveSignals & YAP_CREEP_SIGNAL) {
    Yap_signal(YAP_CREEP_SIGNAL);
  }
  return CallPredicate(RepPredProp(pe), cut_cp, code);
}

static Int
p_execute_in_mod(void)
{				/* '$execute'(Goal)	 */
  return(do_execute(Deref(ARG1), Deref(ARG2)));
}

static Int
p_execute0(void)
{				/* '$execute0'(Goal,Mod)	 */
  Term            t = Deref(ARG1);
  Term            mod = Deref(ARG2);
  unsigned int    arity;
  Prop            pe;

  if (ActiveSignals  && !Yap_InterruptsDisabled) {
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
      return FALSE;
    if (f == FunctorModule) {
      Term tmod = ArgOfTerm(1,t);
      if (!IsVarTerm(tmod) && IsAtomTerm(tmod)) {
	mod = tmod;
	t = ArgOfTerm(2,t);
	goto restart_exec;
      } else {
	if (IsVarTerm(tmod)) {
	  return CallError(INSTANTIATION_ERROR,t);
	} else {
	  return CallError(TYPE_ERROR_ATOM,t);
	}
      }
    }
    pe = PredPropByFunc(f, mod);
    //    Yap_DebugPlWrite(mod);fprintf(stderr,"\n");
    arity = ArityOfFunctor(f);
    if (arity > MaxTemps) {
      return CallError(TYPE_ERROR_CALLABLE, t);
    }
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
    Yap_Error(TYPE_ERROR_CALLABLE,ARG1,"call/1");    
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
  if (IsVarTerm(mod)) {
    mod = CurrentModule;
  } else if (!IsAtomTerm(mod)) {
    Yap_Error(TYPE_ERROR_ATOM, ARG2, "call/1");
    return FALSE;
  }
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,ARG1,"call/1");    
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
      } else {
	if (IsVarTerm(tmod)) {
	  return CallError(INSTANTIATION_ERROR,t);
	} else {
	  return CallError(TYPE_ERROR_ATOM,t);
	}
      }
    }
    pe = PredPropByFunc(f, mod);
    arity = ArityOfFunctor(f);
    if (arity > MaxTemps) {
      return CallError(TYPE_ERROR_CALLABLE, t);
    }
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
    if (ActiveSignals & YAP_CREEP_SIGNAL  && !Yap_InterruptsDisabled) {
      Yap_signal(YAP_CREEP_SIGNAL);
    }
#if defined(YAPOR) || defined(THREADS)
    if (RepPredProp(pe)->PredFlags & LogUpdatePredFlag) {
      PP = RepPredProp(pe);
      LOCK(PP->PELock);
    }
#endif
    return CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->cs.p_code.TrueCodeOfPred);
  }  else { if (ActiveSignals & YAP_CREEP_SIGNAL  && 
		!Yap_InterruptsDisabled &&
		(!(RepPredProp(pe)->PredFlags & (AsmPredFlag|CPredFlag)) ||
		  RepPredProp(pe)->OpcodeOfPred == Yap_opcode(_call_bfunc_xx))) {
      Yap_signal(YAP_CREEP_SIGNAL);
    }
    return CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred);
  }
}

static Term
slice_module_for_call_with_args(Term tin, Term *modp, int arity)
{
  if (IsVarTerm(tin)) {
    Yap_Error(INSTANTIATION_ERROR,tin,"call_with_args/%d", arity);    
    return 0L;
  }
  while (IsApplTerm(tin)) {
    Functor f = FunctorOfTerm(tin);
    Term newmod;
    if (f != FunctorModule) {
      Yap_Error(TYPE_ERROR_ATOM,tin,"call_with_args/%d", arity);    
      return 0L;
    }
    newmod = ArgOfTerm(1,tin);
    if (IsVarTerm(newmod)) {
      Yap_Error(INSTANTIATION_ERROR,tin,"call_with_args/%d",arity);    
      return 0L;
    } else if (!IsAtomTerm(newmod)) {
      Yap_Error(TYPE_ERROR_ATOM,newmod,"call_with_args/%d",arity);    
      return 0L;
    }
    *modp = newmod;
    tin = ArgOfTerm(2,tin);
  }
  if (!IsAtomTerm(tin)) {
    Yap_Error(TYPE_ERROR_ATOM,tin,"call_with_args/%d",arity);  
    return 0L;
  }
  return tin;
}

static Int
p_execute_0(void)
{				/* '$execute_0'(Goal)	 */
  Term mod = CurrentModule;
  Term t = slice_module_for_call_with_args(Deref(ARG1),&mod,0);
  if (!t)
    return FALSE;
  return do_execute(t, mod);
}

static Int
call_with_args(int i)
{
  Term mod = CurrentModule, t;
  int j;

  t = slice_module_for_call_with_args(Deref(ARG1),&mod,i);
  if (!t)
    return FALSE;
  for (j=0;j<i;j++)
    heap_store(Deref(XREGS[j+2]));
  return(do_execute_n(t, mod, i));
}


static Int
p_execute_1(void)
{				/* '$execute_0'(Goal)	 */
  return call_with_args(1);
}

static Int
p_execute_2(void)
{				/* '$execute_2'(Goal)	 */
  return call_with_args(2);
}

static Int
p_execute_3(void)
{				/* '$execute_3'(Goal)	 */
  return call_with_args(3);
}

static Int
p_execute_4(void)
{				/* '$execute_4'(Goal)	 */
  return call_with_args(4);
}

static Int
p_execute_5(void)
{				/* '$execute_5'(Goal)	 */
  return call_with_args(5);
}

static Int
p_execute_6(void)
{				/* '$execute_6'(Goal)	 */
  return call_with_args(6);
}

static Int
p_execute_7(void)
{				/* '$execute_7'(Goal)	 */
  return call_with_args(7);
}

static Int
p_execute_8(void)
{				/* '$execute_8'(Goal)	 */
  return call_with_args(8);
}

static Int
p_execute_9(void)
{				/* '$execute_9'(Goal)	 */
  return call_with_args(9);
}

static Int
p_execute_10(void)
{				/* '$execute_10'(Goal)	 */
  return call_with_args(10);
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
  int lval, out;
  if (top && (lval = sigsetjmp (Yap_RestartEnv, 1)) != 0) {
    switch(lval) {
    case 1:
      { /* restart */
	/* otherwise, SetDBForThrow will fail entering critical mode */
	Yap_PrologMode = UserMode;
	/* find out where to cut to */
	/* siglongjmp resets the TR hardware register */
	/* TR and B are crucial, they might have been changed, or not */
	restore_TR();
	restore_B();
	/* H is not so important, because we're gonna backtrack */
	restore_H();
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
  out = Yap_absmi(0);
  Yap_StartSlots();
  return out;
}


static  void
init_stack(int arity, CELL *pt, int top, choiceptr saved_b)
{
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
  if (top && GLOBAL_root_dep_fr) {
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
  CP = YESCODE;
}

static Term
do_goal(Term t, yamop *CodeAdr, int arity, CELL *pt, int top)
{
  choiceptr saved_b = B;
  Term out = 0L;

  init_stack(arity, pt, top, saved_b);
  P = (yamop *) CodeAdr;
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
  LOCK(ppe->PELock);
  if (IsAtomTerm(t)) {
    CodeAdr = RepPredProp (pe)->CodeOfPred;
    UNLOCK(ppe->PELock);
    out = do_goal(t, CodeAdr, 0, pt, FALSE);
  } else {
    Functor f = FunctorOfTerm(t);
    CodeAdr = RepPredProp (pe)->CodeOfPred;
    UNLOCK(ppe->PELock);
    out = do_goal(t, CodeAdr, ArityOfFunctor(f), pt, FALSE);
  }

  if (out == 1) {
    choiceptr cut_B, old_B;
    /* we succeeded, let's prune */
    /* restore the old environment */
    /* get to previous environment */
    cut_B = (choiceptr)ENV[E_CB];
#ifdef CUT_C
    {
      /* Note that 
	 cut_B == (choiceptr)ENV[E_CB] */
      while (POP_CHOICE_POINT(ENV[E_CB]))
	{
	  POP_EXECUTE();
	}
    }
#endif /* CUT_C */
#ifdef YAPOR
    CUT_prune_to(cut_B);
#endif /* YAPOR */
#ifdef TABLING
    if (B != cut_B) {
      while (B->cp_b < cut_B) {
	B = B->cp_b;
      }
      abolish_incomplete_subgoals(B);
    }
#endif /* TABLING */
    B = cut_B;
    /* find out where we have the old arguments */
    old_B = ((choiceptr)(ENV-(EnvSizeInCells+nargs+1)))-1;
    CP   = saved_cp;
    P    = saved_p;
    ASP  = ENV;
    Yap_StartSlots();
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
      } else {
	if (IsVarTerm(tmod)) {
	  Yap_Error(INSTANTIATION_ERROR,t,"call/1");
	} else {
	  Yap_Error(TYPE_ERROR_ATOM,t,"call/1");
	}
	return FALSE;
      }
    }
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pe = PredPropByFunc(f, CurrentModule);
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
  LOCK(ppe->PELock);
  CodeAdr = ppe->CodeOfPred;
  UNLOCK(ppe->PELock);
#if !USE_SYSTEM_MALLOC
  if (Yap_TrailTop - HeapTop < 2048) {
    Yap_PrologMode = BootMode;
    Yap_Error(OUT_OF_TRAIL_ERROR,TermNil,
	  "unable to boot because of too little Trail space");
  }
#endif
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
#ifdef CUT_C
  {
    while (POP_CHOICE_POINT(pt0))
      {
	POP_EXECUTE();
      }
  }
#endif /* CUT_C */
#ifdef YAPOR
  CUT_prune_to(pt0);
#endif /* YAPOR */
  /* find where to cut to */
  if (pt0 > B) {
    /* Wow, we're gonna cut!!! */
#ifdef TABLING
    while (B->cp_b < pt0) {
      B = B->cp_b;
    }
    abolish_incomplete_subgoals(B);
#endif /* TABLING */
    B = pt0;
    HB = B->cp_h;
    Yap_TrimTrail();
  }
  return(TRUE);
}

static Int
p_clean_ifcp(void) {
  Term t = Deref(ARG1);
  choiceptr pt0;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "cut_at/1");
    return FALSE;    
  }
  if (!IsIntegerTerm(t)) {
    Yap_Error(TYPE_ERROR_INTEGER, t, "cut_at/1");
    return FALSE;    
  }
#if SBA
  pt0 = (choiceptr)IntegerOfTerm(t);
#else
  pt0 = cp_from_integer(t);
#endif
  if (pt0 < B) {
    /* this should never happen */ 
    return TRUE;
  } else if (pt0 == B) {
    B = B->cp_b;
    HB = B->cp_h;
  } else {
    pt0->cp_ap = (yamop *)TRUSTFAILCODE;
  }
  return TRUE;
}



static int disj_marker(yamop *apc) {
  op_numbers opnum = Yap_op_from_opcode(apc->opc);

  return opnum == _or_else || opnum == _or_last;
}


static Int
p_cut_up_to_next_disjunction(void) {
  choiceptr pt0 = B;
  CELL *qenv = (CELL *)ENV[E_E];
  
  while (pt0 &&
	 !( qenv == pt0->cp_env && disj_marker(pt0->cp_ap))) {
    pt0 = pt0->cp_b;
  }
  if (!pt0)
    return TRUE;
#ifdef YAPOR
  CUT_prune_to(pt0);
#endif /* YAPOR */
  /* find where to cut to */
  if (SHOULD_CUT_UP_TO(B,pt0)) {
    B = pt0;
#ifdef TABLING
    abolish_incomplete_subgoals(B);
#endif /* TABLING */
  }
  HB = B->cp_h;
  Yap_TrimTrail();
  return TRUE;
}

static int is_cleanup_cp(choiceptr cp_b)
{
  PredEntry *pe;

  if (cp_b->cp_ap->opc != ORLAST_OPCODE)
    return FALSE;
#ifdef YAPOR
  pe = cp_b->cp_ap->u.Osblp.p0;
#else
  pe = cp_b->cp_ap->u.p.p;
#endif	/* YAPOR */
  /* 
     it has to be a cleanup and it has to be a completed goal,
     otherwise the throw will be caught anyway.
   */
  return pe == PredSafeCallCleanup;
}

static Int
JumpToEnv(Term t) {
  yamop *pos = NEXTOP(PredDollarCatch->cs.p_code.TrueCodeOfPred,l),
    *catchpos = NEXTOP(PredHandleThrow->cs.p_code.TrueCodeOfPred,l);
  CELL *env, *env1;
  choiceptr handler, previous = NULL;

  /* throws cannot interrupt throws */
  if (EX)
    return FALSE;
  /* just keep the throwed object away, we don't need to care about it */
  if (!(BallTerm = Yap_StoreTermInDB(t, 0))) {
    /* fat chance */
    siglongjmp(Yap_RestartEnv,1);
  }
  /* careful, previous step may have caused a stack shift, 
     so get pointers here */
  handler = B;
  env1 = ENV;
  do {
    /* find the first choicepoint that may be a catch */
    while (handler != NULL && handler->cp_ap != pos) {
      /* we are already doing a catch */
      if (handler->cp_ap == catchpos) {
	P = (yamop *)FAILCODE;
	return TRUE;
      }
      /* we have a cleanup handler in the middle */
      if (is_cleanup_cp(handler)) {
	/* keep it around */
	if (previous == NULL)
	  B = handler;
	else 
	  previous->cp_b = handler;
	previous = handler;
#ifdef TABLING
      } else {
	if (handler->cp_ap != NOCODE) {
	  abolish_incomplete_subgoals(handler);
	}
#endif /* TABLING */
      }
      /* we reached C-Code */
      if (handler->cp_ap == NOCODE) {
	/* up to the C-code to deal with this! */
	UncaughtThrow = TRUE;
	if (previous == NULL)
	  B = handler;
	else 
	  previous->cp_b = handler;
	EX = t;
	P = (yamop *)FAILCODE;
	HB = B->cp_h;
	return TRUE;
      }
      handler = handler->cp_b;
    }
    /* uncaught throw */
    if (handler == NULL) {
      UncaughtThrow = TRUE;
#if PUSH_REGS
      restore_absmi_regs(&Yap_standard_regs);
#endif
      siglongjmp(Yap_RestartEnv,1);
    }
    /* is it a continuation? */
    env = handler->cp_env;
    while (env > env1) {
      env1 = ENV_Parent(env1);
    }
    /* yes, we found it ! */
    //    while (env < ENV)
    //      env = ENV_Parent(env);
    if (env == env1) {
      break;
    }
    /* oops, try next */
    handler = handler->cp_b;
  } while (TRUE);
  /* step one environment above, otherwise we'll redo the original goal */
  if (previous == NULL) {
    B = handler;
  } else {
    //    EX = t;
    previous->cp_b = handler;
  }
  handler->cp_cp = (yamop *)env[E_CP];
  handler->cp_env = (CELL *)env[E_E];
  handler->cp_ap = NEXTOP(PredHandleThrow->CodeOfPred,l);
  /* can recover Heap thanks to copy term :-( */
  /* B->cp_h = H; */
  /* I could backtrack here, but it is easier to leave the unwinding
     to the emulator */
  P = (yamop *)FAILCODE;
  HB = B->cp_h;
  /* try to recover space */
  /* can only do that when we recover space */
  /* first, simulate backtracking */
  /* so that I will execute op_fail */
  return TRUE;
}

Int
Yap_JumpToEnv(Term t) {
  if (Yap_PrologMode & BootMode) {
    return FALSE;
  } 
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
  Term h0var;

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
  Yap_ResetExceptionTerm ();
  Yap_PutValue (AtomBreak, MkIntTerm (0));
  TR = (tr_fr_ptr)Yap_TrailBase;
  if (Yap_AttsSize > (Yap_LocalBase-Yap_GlobalBase)/8)
    Yap_AttsSize = (Yap_LocalBase-Yap_GlobalBase)/8;
  H = H0 = ((CELL *) Yap_GlobalBase)+ Yap_AttsSize/sizeof(CELL);
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
  H_FZ = H;
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
  Yap_StartSlots();
  init_stack(0, NULL, TRUE, NULL);
  /* the first real choice-point will also have AP=FAIL */ 
  Yap_StartSlots();
  GlobalArena = TermNil;
  h0var = MkVarTerm();
#if COROUTINING
  DelayedVars = Yap_NewTimedVar(h0var);
  WokenGoals = Yap_NewTimedVar(TermNil);
  AttsMutableList = Yap_NewTimedVar(h0var);
  GlobalDelayArena = TermNil;
#endif
  GcGeneration = Yap_NewTimedVar(h0var);
  GcCurrentPhase = 0L;
  GcPhase = Yap_NewTimedVar(MkIntTerm(GcCurrentPhase));
#if defined(YAPOR) || defined(THREADS)
  PP = NULL;
  WPP = NULL;
  PREG_ADDR = NULL;
#endif
  Yap_AllocateDefaultArena(128*1024, 2);
  Yap_InitPreAllocCodeSpace();
#ifdef CUT_C
  cut_c_initialize();
#endif
#if defined MYDDAS_MYSQL || defined MYDDAS_ODBC
  Yap_REGS.MYDDAS_GLOBAL_POINTER = NULL;
#endif
}

static Int
p_uncaught_throw(void)
{
  Int out = UncaughtThrow;
  UncaughtThrow = FALSE; /* just caught it */
  return out;
}

static Int
p_creep_allowed(void)
{
  if (PP != NULL) {
    LOCK(SignalLock);
    if (ActiveSignals & YAP_CREEP_SIGNAL  && !Yap_InterruptsDisabled) {
      ActiveSignals &= ~YAP_CREEP_SIGNAL;    
      if (!ActiveSignals)
	CreepFlag = CalculateStackGap();
      UNLOCK(SignalLock);
    } else {
      UNLOCK(SignalLock);
    }
    return TRUE;
  }
  return FALSE;
}

static Int
p_debug_on(void)
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {
    if (DebugOn)
      return Yap_unify(MkAtomTerm(AtomTrue),ARG1);
    else
      return Yap_unify(MkAtomTerm(AtomFalse),ARG1);
  }
  if (t == MkAtomTerm(AtomTrue))
    DebugOn = TRUE;
  else
    DebugOn = FALSE;
  return TRUE;
}

static Term 
GetException(void)
{
  Term t = 0L;
  if (BallTerm) {
    do {
      t = Yap_PopTermFromDB(BallTerm);
      if (t == 0) {
	if (Yap_Error_TYPE == OUT_OF_ATTVARS_ERROR) {
	  Yap_Error_TYPE = YAP_NO_ERROR;
	  if (!Yap_growglobal(NULL)) {
	    Yap_Error(OUT_OF_ATTVARS_ERROR, TermNil, Yap_ErrorMessage);
	    return FALSE;
	  }
	} else {
	  Yap_Error_TYPE = YAP_NO_ERROR;
	  if (!Yap_growstack(BallTerm->NOfCells*CellSize)) {
	    Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	    return FALSE;
	  }
	}
      }
    } while (t == 0);
    BallTerm = NULL;
  }
  return t;
}

static Int
p_reset_exception(void)
{
  Term t;
  EX = 0L;
  t = GetException();
  if (!t) 
    return FALSE;
  return Yap_unify(t, ARG1);
}

void
Yap_ResetExceptionTerm(void)
{
  Yap_ReleaseTermFromDB(BallTerm);
  BallTerm = NULL;
}

static Int
p_get_exception(void)
{
  Term t = GetException();
  if (!t) 
    return FALSE;
  return Yap_unify(t, ARG1);
}

void 
Yap_InitExecFs(void)
{
  Term cm = CurrentModule;
  Yap_InitComma();
  Yap_InitCPred("$execute", 1, p_execute, HiddenPredFlag);
  Yap_InitCPred("$execute", 2, p_execute2, HiddenPredFlag);
  Yap_InitCPred("$execute", 3, p_execute3, HiddenPredFlag);
  Yap_InitCPred("$execute", 4, p_execute4, HiddenPredFlag);
  Yap_InitCPred("$execute", 5, p_execute5, HiddenPredFlag);
  Yap_InitCPred("$execute", 6, p_execute6, HiddenPredFlag);
  Yap_InitCPred("$execute", 7, p_execute7, HiddenPredFlag);
  Yap_InitCPred("$execute", 8, p_execute8, HiddenPredFlag);
  Yap_InitCPred("$execute", 9, p_execute9, HiddenPredFlag);
  Yap_InitCPred("$execute", 10, p_execute10, HiddenPredFlag);
  Yap_InitCPred("$execute", 11, p_execute11, HiddenPredFlag);
  Yap_InitCPred("$execute", 12, p_execute12, HiddenPredFlag);
  Yap_InitCPred("$execute_in_mod", 2, p_execute_in_mod, HiddenPredFlag);
  Yap_InitCPred("$execute_wo_mod", 2, p_execute_in_mod, HiddenPredFlag);
  Yap_InitCPred("call_with_args", 1, p_execute_0, HiddenPredFlag);
  Yap_InitCPred("call_with_args", 2, p_execute_1, HiddenPredFlag);
  Yap_InitCPred("call_with_args", 3, p_execute_2, HiddenPredFlag);
  Yap_InitCPred("call_with_args", 4, p_execute_3, HiddenPredFlag);
  Yap_InitCPred("call_with_args", 5, p_execute_4, HiddenPredFlag);
  Yap_InitCPred("call_with_args", 6, p_execute_5, HiddenPredFlag);
  Yap_InitCPred("call_with_args", 7, p_execute_6, HiddenPredFlag);
  Yap_InitCPred("call_with_args", 8, p_execute_7, HiddenPredFlag);
  Yap_InitCPred("call_with_args", 9, p_execute_8, HiddenPredFlag);
  Yap_InitCPred("call_with_args", 10, p_execute_9, HiddenPredFlag);
  Yap_InitCPred("call_with_args", 11, p_execute_10, HiddenPredFlag);
  Yap_InitCPred("$debug_on", 1, p_debug_on, HiddenPredFlag);
#ifdef DEPTH_LIMIT
  Yap_InitCPred("$execute_under_depth_limit", 2, p_execute_depth_limit, HiddenPredFlag);
#endif
  Yap_InitCPred("$execute0", 2, p_execute0, HiddenPredFlag);
  Yap_InitCPred("$execute_nonstop", 2, p_execute_nonstop, HiddenPredFlag);
  Yap_InitCPred("$execute_clause", 4, p_execute_clause, HiddenPredFlag);
  CurrentModule = HACKS_MODULE;
  Yap_InitCPred("current_choice_point", 1, p_save_cp, HiddenPredFlag);
  Yap_InitCPred("current_choicepoint", 1, p_save_cp, HiddenPredFlag);
  Yap_InitCPred("env_choice_point", 1, p_save_env_b, HiddenPredFlag);
  Yap_InitCPred("trail_suspension_marker", 1, p_trail_suspension_marker, HiddenPredFlag);
  Yap_InitCPred("cut_at", 1, p_clean_ifcp, SafePredFlag);
  CurrentModule = cm;
  Yap_InitCPred("$pred_goal_expansion_on", 0, p_pred_goal_expansion_on, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$restore_regs", 1, p_restore_regs, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$restore_regs", 2, p_restore_regs2, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$clean_ifcp", 1, p_clean_ifcp, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("qpack_clean_up_to_disjunction", 0, p_cut_up_to_next_disjunction, SafePredFlag);
  Yap_InitCPred("$jump_env_and_store_ball", 1, p_jump_env, HiddenPredFlag);
  Yap_InitCPred("$creep_allowed", 0, p_creep_allowed, HiddenPredFlag);
  Yap_InitCPred("$generate_pred_info", 4, p_generate_pred_info, HiddenPredFlag);
  Yap_InitCPred("$uncaught_throw", 0, p_uncaught_throw, HiddenPredFlag);
  Yap_InitCPred("$reset_exception", 1, p_reset_exception, HiddenPredFlag);
  Yap_InitCPred("$get_exception", 1, p_get_exception, HiddenPredFlag);
}


