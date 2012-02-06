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

STATIC_PROTO(Int  CallPredicate, (PredEntry *, choiceptr, yamop * CACHE_TYPE));
STATIC_PROTO(Int  EnterCreepMode, (Term, Term CACHE_TYPE));
STATIC_PROTO(Int  p_save_cp, ( USES_REGS1 ));
STATIC_PROTO(Int  p_execute, ( USES_REGS1 ));
STATIC_PROTO(Int  p_execute0, ( USES_REGS1 ));

static Term
cp_as_integer(choiceptr cp USES_REGS)
{
  return(MkIntegerTerm(LCL0-(CELL *)cp));
}

static choiceptr
cp_from_integer(Term cpt USES_REGS)
{
  return (choiceptr)(LCL0-IntegerOfTerm(cpt));
}

Term
Yap_cp_as_integer(choiceptr cp)
{
  CACHE_REGS
  return cp_as_integer(cp PASS_REGS);
}

static inline Int
CallPredicate(PredEntry *pen, choiceptr cut_pt, yamop *code USES_REGS) {
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
CallMetaCall(Term t, Term mod USES_REGS) {
  ARG1 = t;
  ARG2 = cp_as_integer(B PASS_REGS); /* p_save_cp */
  ARG3 = t;
  if (mod) {
    ARG4 = mod;
  } else {
    ARG4 = TermProlog;
  }
  return (CallPredicate(PredMetaCall, B, PredMetaCall->CodeOfPred PASS_REGS));
}

Term
Yap_ExecuteCallMetaCall(Term mod) {
  CACHE_REGS
  Term ts[4];
  ts[0] = ARG1;
  ts[1] = cp_as_integer(B PASS_REGS); /* p_save_cp */
  ts[2] = ARG1;
  ts[3] = mod;
  return(Yap_MkApplTerm(PredMetaCall->FunctorOfPred,4,ts));
}

static Int
CallError(yap_error_number err, Term t, Term mod USES_REGS)
{
  if (yap_flags[LANGUAGE_MODE_FLAG] == 1) {
    return(CallMetaCall(t, mod PASS_REGS));
  } else {
    Yap_Error(err, t, "call/1");
    return(FALSE);
  }
}

static Int
p_save_cp( USES_REGS1 )
{
  Term t = Deref(ARG1);
  Term td;
#if SHADOW_HB
  register CELL *HBREG = HB;
#endif
  if (!IsVarTerm(t)) return(FALSE);
  td = cp_as_integer(B PASS_REGS);
  Bind((CELL *)t,td);
  return(TRUE);
}

static Int
p_save_env_b( USES_REGS1 )
{
  Term t = Deref(ARG1);
  Term td;
#if SHADOW_HB
  register CELL *HBREG = HB;
#endif
  if (!IsVarTerm(t)) return(FALSE);
  td = cp_as_integer((choiceptr)YENV[E_CB] PASS_REGS);
  Bind((CELL *)t,td);
  return(TRUE);
}

static Int
p_trail_suspension_marker( USES_REGS1 )
{
  Term t = Deref(ARG1);
  
  TrailTerm(TR) = AbsPair((CELL*)t);
  TR++;
  return TRUE;
}

inline static Int
do_execute(Term t, Term mod USES_REGS)
{
  Term t0 = t;
  /* first do predicate expansion, even before you process signals.
     This way you don't get to spy goal_expansion(). */
  if (PRED_GOAL_EXPANSION_ALL) {
    LOCK(LOCAL_SignalLock);
    /* disable creeping when we do goal expansion */
    if (LOCAL_ActiveSignals & YAP_CREEP_SIGNAL && !LOCAL_InterruptsDisabled) {
      LOCAL_ActiveSignals &= ~YAP_CREEP_SIGNAL;
      CreepFlag = CalculateStackGap();
    }
    UNLOCK(LOCAL_SignalLock);
    return CallMetaCall(ARG1, mod PASS_REGS);
  } else if (LOCAL_ActiveSignals  && !LOCAL_InterruptsDisabled) {
    return EnterCreepMode(t, mod PASS_REGS);
  }
 restart_exec:
  if (IsVarTerm(t)) {
    return CallError(INSTANTIATION_ERROR, t0, mod PASS_REGS);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    register CELL *pt;
    PredEntry *pen;
    unsigned int i, arity;

    f = FunctorOfTerm(t);
    if (IsExtensionFunctor(f)) {
      return CallError(TYPE_ERROR_CALLABLE, t0, mod PASS_REGS);
    }
    arity = ArityOfFunctor(f);
    if (arity > MaxTemps) {
      return CallError(TYPE_ERROR_CALLABLE, t0, mod PASS_REGS);
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
	    return CallError(INSTANTIATION_ERROR, t0, mod PASS_REGS);
	  } else {
	    return CallError(TYPE_ERROR_ATOM, t0, mod PASS_REGS);
	  }
	}
      } else {
	return CallMetaCall(t, mod PASS_REGS);
      }
    }
    /* now let us do what we wanted to do from the beginning !! */
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t)+1;
    for (i = 1; i <= arity; i++) {
#if YAPOR_SBA
      Term d0 = *pt++;
      if (d0 == 0)
`	XREGS[i] = (CELL)(pt-1);
      else
	XREGS[i] = d0;
#else


      XREGS[i] = *pt++;
#endif
    }
    return (CallPredicate(pen, B, pen->CodeOfPred PASS_REGS));
  } else if (IsAtomTerm(t)) { 
    PredEntry            *pe;
    Atom a = AtomOfTerm(t);

    if (a == AtomTrue || a == AtomOtherwise || a == AtomCut)
      return(TRUE);
    else if (a == AtomFail || a == AtomFalse)
      return(FALSE);
    /* call may not define new system predicates!! */
    pe = RepPredProp(PredPropByAtom(a, mod));
    return (CallPredicate(pe, B, pe->CodeOfPred PASS_REGS));
  } else if (IsIntTerm(t)) {
    return CallError(TYPE_ERROR_CALLABLE, t0, mod PASS_REGS);
  } else {
    /* Is Pair Term */
    return(CallMetaCall(t, mod PASS_REGS));
  }
}

static Term
copy_execn_to_heap(Functor f, CELL *pt, unsigned int n, unsigned int arity, Term mod USES_REGS)
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
do_execute_n(Term t, Term mod, unsigned int n USES_REGS)
{
  Functor f;
  Atom Name;
  register CELL *pt;
  PredEntry *pen;
  unsigned int i, arity;
  int j = -n;
  Term t0 = t;

 restart_exec:
  if (IsVarTerm(t)) {
    return CallError(INSTANTIATION_ERROR, t0, mod PASS_REGS);
  } else if (IsAtomTerm(t)) {
    arity = n;
    Name = AtomOfTerm(t);
    pt = NULL;
  } else if (IsIntTerm(t)) {
    return CallError(TYPE_ERROR_CALLABLE, t0, mod PASS_REGS);
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
	  return CallError(INSTANTIATION_ERROR, t0, tmod PASS_REGS);
	} else {
	  return CallError(TYPE_ERROR_ATOM, t0, tmod PASS_REGS);
	}
      }
    }
    arity = ArityOfFunctor(f)+n;
    Name = NameOfFunctor(f);
    pt = RepAppl(t)+1;
  }
  f = Yap_MkFunctor(Name,arity);
  if (IsExtensionFunctor(f)) {
    return CallError(TYPE_ERROR_CALLABLE, t0, mod PASS_REGS);
  }
  if (PRED_GOAL_EXPANSION_ALL) {
    LOCK(LOCAL_SignalLock);
    /* disable creeping when we do goal expansion */
    if (LOCAL_ActiveSignals & YAP_CREEP_SIGNAL && !LOCAL_InterruptsDisabled) {
      LOCAL_ActiveSignals &= ~YAP_CREEP_SIGNAL;
      CreepFlag = CalculateStackGap();
    }
    UNLOCK(LOCAL_SignalLock);
    t = copy_execn_to_heap(f, pt, n, arity, mod PASS_REGS);
    return CallMetaCall(t, mod PASS_REGS);
  } else if (LOCAL_ActiveSignals  && !LOCAL_InterruptsDisabled) {
    return EnterCreepMode(copy_execn_to_heap(f, pt, n, arity, CurrentModule PASS_REGS), mod PASS_REGS);
  }
  if (arity > MaxTemps) {
    return CallError(TYPE_ERROR_CALLABLE, t0, mod PASS_REGS);
  }
  pen = RepPredProp(PredPropByFunc(f, mod));
  /* You thought we would be over by now */
  /* but no meta calls require special preprocessing */
  if (pen->PredFlags & (GoalExPredFlag|MetaPredFlag)) {
    Term t = copy_execn_to_heap(f, pt, n, arity, mod PASS_REGS);
    return(CallMetaCall(t, mod PASS_REGS));
  }
  /* now let us do what we wanted to do from the beginning !! */
  /* I cannot use the standard macro here because
     otherwise I would dereference the argument and
     might skip a svar */
  for (i = 1; i <= arity-n; i++) {
#if YAPOR_SBA
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
  return CallPredicate(pen, B, pen->CodeOfPred PASS_REGS);
}

static Int
EnterCreepMode(Term t, Term mod USES_REGS) {
  PredEntry *PredCreep;

  if (LOCAL_ActiveSignals & YAP_CDOVF_SIGNAL) {
    ARG1 = t;
    if (!Yap_growheap(FALSE, 0, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, "YAP failed to grow heap at meta-call");
    }
    if (!LOCAL_ActiveSignals) {
      return do_execute(ARG1, mod PASS_REGS);
    }
  }
  PP = PredMetaCall;
  PredCreep = RepPredProp(PredPropByFunc(FunctorCreep,1));
  if (mod) {
    ARG1 = MkPairTerm(mod,t);
  } else {
    ARG1 = MkPairTerm(TermProlog,t);
  }
  LOCK(LOCAL_SignalLock);
  CreepFlag = CalculateStackGap();
  UNLOCK(LOCAL_SignalLock);
  P_before_spy = P;
  return CallPredicate(PredCreep, B, PredCreep->CodeOfPred PASS_REGS);
}

static Int
p_execute( USES_REGS1 )
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  return do_execute(t, CurrentModule PASS_REGS);
}

static void
heap_store(Term t USES_REGS)
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
p_execute2( USES_REGS1 )
{				/* '$execute'(Goal)	 */
  Term       t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  return do_execute_n(t, CurrentModule, 1 PASS_REGS);
}

static Int
p_execute3( USES_REGS1 )
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  return do_execute_n(t, CurrentModule, 2 PASS_REGS);
}

static Int
p_execute4( USES_REGS1 )
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  return do_execute_n(t, CurrentModule, 3  PASS_REGS);
}

static Int
p_execute5( USES_REGS1 )
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  heap_store(Deref(ARG5) PASS_REGS);
  return do_execute_n(t, CurrentModule, 4 PASS_REGS);
}

static Int
p_execute6( USES_REGS1 )
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  heap_store(Deref(ARG5) PASS_REGS);
  heap_store(Deref(ARG6) PASS_REGS);
  return do_execute_n(t, CurrentModule, 5 PASS_REGS);
}

static Int
p_execute7( USES_REGS1 )
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  heap_store(Deref(ARG5) PASS_REGS);
  heap_store(Deref(ARG6) PASS_REGS);
  heap_store(Deref(ARG7) PASS_REGS);
  return do_execute_n(t, CurrentModule, 6 PASS_REGS);
}

static Int
p_execute8( USES_REGS1 )
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  heap_store(Deref(ARG5) PASS_REGS);
  heap_store(Deref(ARG6) PASS_REGS);
  heap_store(Deref(ARG7) PASS_REGS);
  heap_store(Deref(ARG8) PASS_REGS);
  return do_execute_n(t, CurrentModule, 7 PASS_REGS);
}

static Int
p_execute9( USES_REGS1 )
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  heap_store(Deref(ARG5) PASS_REGS);
  heap_store(Deref(ARG6) PASS_REGS);
  heap_store(Deref(ARG7) PASS_REGS);
  heap_store(Deref(ARG8) PASS_REGS);
  heap_store(Deref(ARG9) PASS_REGS);
  return do_execute_n(t, CurrentModule, 8 PASS_REGS);
}

static Int
p_execute10( USES_REGS1 )
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  heap_store(Deref(ARG5) PASS_REGS);
  heap_store(Deref(ARG6) PASS_REGS);
  heap_store(Deref(ARG7) PASS_REGS);
  heap_store(Deref(ARG8) PASS_REGS);
  heap_store(Deref(ARG9) PASS_REGS);
  heap_store(Deref(ARG10) PASS_REGS);
  return(do_execute_n(t, CurrentModule, 9 PASS_REGS));
}

static Int
p_execute11( USES_REGS1 )
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  heap_store(Deref(ARG5) PASS_REGS);
  heap_store(Deref(ARG6) PASS_REGS);
  heap_store(Deref(ARG7) PASS_REGS);
  heap_store(Deref(ARG8) PASS_REGS);
  heap_store(Deref(ARG9) PASS_REGS);
  heap_store(Deref(ARG10) PASS_REGS);
  heap_store(Deref(ARG11) PASS_REGS);
  return(do_execute_n(t, CurrentModule, 10 PASS_REGS));
}

static Int
p_execute12( USES_REGS1 )
{				/* '$execute'(Goal)	 */
  Term            t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  heap_store(Deref(ARG5) PASS_REGS);
  heap_store(Deref(ARG6) PASS_REGS);
  heap_store(Deref(ARG7) PASS_REGS);
  heap_store(Deref(ARG8) PASS_REGS);
  heap_store(Deref(ARG9) PASS_REGS);
  heap_store(Deref(ARG10) PASS_REGS);
  heap_store(Deref(ARG11) PASS_REGS);
  heap_store(Deref(ARG12) PASS_REGS);
  return(do_execute_n(t, CurrentModule, 11 PASS_REGS));
}

static Int
p_execute_clause( USES_REGS1 )
{				/* '$execute_clause'(Goal)	 */
  Term            t = Deref(ARG1), t0 = t;
  Term            mod = Deref(ARG2);
  choiceptr       cut_cp = cp_from_integer(Deref(ARG4) PASS_REGS);
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
      return CallError(TYPE_ERROR_CALLABLE, t0, mod PASS_REGS);
    }
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t)+1;
    for (i = 1; i <= arity; ++i) {
#if YAPOR_SBA
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
  if (LOCAL_ActiveSignals & YAP_CREEP_SIGNAL) {
    Yap_signal(YAP_CREEP_SIGNAL);
  }
  return CallPredicate(RepPredProp(pe), cut_cp, code PASS_REGS);
}

static Int
p_execute_in_mod( USES_REGS1 )
{				/* '$execute'(Goal)	 */
  return(do_execute(Deref(ARG1), Deref(ARG2) PASS_REGS));
}

static Int
p_execute0( USES_REGS1 )
{				/* '$execute0'(Goal,Mod)	 */
  Term            t = Deref(ARG1), t0 = t;
  Term            mod = Deref(ARG2);
  unsigned int    arity;
  Prop            pe;

  if (LOCAL_ActiveSignals  && !LOCAL_InterruptsDisabled) {
    return EnterCreepMode(t, mod PASS_REGS);
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
	  return CallError(INSTANTIATION_ERROR, t0, mod PASS_REGS);
	} else {
	  return CallError(TYPE_ERROR_ATOM, t0, mod PASS_REGS);
	}
      }
    }
    pe = PredPropByFunc(f, mod);
    //    Yap_DebugPlWrite(mod);fprintf(stderr,"\n");
    arity = ArityOfFunctor(f);
    if (arity > MaxTemps) {
      return CallError(TYPE_ERROR_CALLABLE, t0, mod PASS_REGS);
    }
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t)+1;
    for (i = 1; i <= arity; ++i) {
#if YAPOR_SBA
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
  return CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred PASS_REGS);
}

static Int
p_execute_nonstop( USES_REGS1 )
{				/* '$execute_nonstop'(Goal,Mod)	 */
  Term            t = Deref(ARG1), t0 = t;
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
	  return CallError(INSTANTIATION_ERROR, t0, mod PASS_REGS);
	} else {
	  return CallError(TYPE_ERROR_ATOM, t0, mod PASS_REGS);
	}
      }
    }
    pe = PredPropByFunc(f, mod);
    arity = ArityOfFunctor(f);
    if (arity > MaxTemps) {
      return CallError(TYPE_ERROR_CALLABLE, t0, mod PASS_REGS);
    }
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t)+1;
    for (i = 1; i <= arity; ++i) {
#if YAPOR_SBA
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
    if (LOCAL_ActiveSignals & YAP_CREEP_SIGNAL  && !LOCAL_InterruptsDisabled) {
      Yap_signal(YAP_CREEP_SIGNAL);
    }
#if defined(YAPOR) || defined(THREADS)
    if (RepPredProp(pe)->PredFlags & LogUpdatePredFlag) {
      PP = RepPredProp(pe);
      PELOCK(80,PP);
    }
#endif
    return CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->cs.p_code.TrueCodeOfPred PASS_REGS);
  }  else { if (LOCAL_ActiveSignals & YAP_CREEP_SIGNAL  && 
		!LOCAL_InterruptsDisabled &&
		(!(RepPredProp(pe)->PredFlags & (AsmPredFlag|CPredFlag)) ||
		  RepPredProp(pe)->OpcodeOfPred == Yap_opcode(_call_bfunc_xx))) {
      Yap_signal(YAP_CREEP_SIGNAL);
    }
    return CallPredicate(RepPredProp(pe), B, RepPredProp(pe)->CodeOfPred PASS_REGS);
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
p_execute_0( USES_REGS1 )
{				/* '$execute_0'(Goal)	 */
  Term mod = CurrentModule;
  Term t = slice_module_for_call_with_args(Deref(ARG1),&mod,0);
  if (!t)
    return FALSE;
  return do_execute(t, mod PASS_REGS);
}

static Int
call_with_args(int i USES_REGS)
{
  Term mod = CurrentModule, t;
  int j;

  t = slice_module_for_call_with_args(Deref(ARG1),&mod,i);
  if (!t)
    return FALSE;
  for (j=0;j<i;j++)
    heap_store(Deref(XREGS[j+2]) PASS_REGS);
  return(do_execute_n(t, mod, i PASS_REGS));
}


static Int
p_execute_1( USES_REGS1 )
{				/* '$execute_0'(Goal)	 */
  return call_with_args(1 PASS_REGS);
}

static Int
p_execute_2( USES_REGS1 )
{				/* '$execute_2'(Goal)	 */
  return call_with_args(2 PASS_REGS);
}

static Int
p_execute_3( USES_REGS1 )
{				/* '$execute_3'(Goal)	 */
  return call_with_args(3 PASS_REGS);
}

static Int
p_execute_4( USES_REGS1 )
{				/* '$execute_4'(Goal)	 */
  return call_with_args(4 PASS_REGS);
}

static Int
p_execute_5( USES_REGS1 )
{				/* '$execute_5'(Goal)	 */
  return call_with_args(5 PASS_REGS);
}

static Int
p_execute_6( USES_REGS1 )
{				/* '$execute_6'(Goal)	 */
  return call_with_args(6 PASS_REGS);
}

static Int
p_execute_7( USES_REGS1 )
{				/* '$execute_7'(Goal)	 */
  return call_with_args(7 PASS_REGS);
}

static Int
p_execute_8( USES_REGS1 )
{				/* '$execute_8'(Goal)	 */
  return call_with_args(8 PASS_REGS);
}

static Int
p_execute_9( USES_REGS1 )
{				/* '$execute_9'(Goal)	 */
  return call_with_args(9 PASS_REGS);
}

static Int
p_execute_10( USES_REGS1 )
{				/* '$execute_10'(Goal)	 */
  return call_with_args(10 PASS_REGS);
}

#ifdef DEPTH_LIMIT
static Int
p_execute_depth_limit( USES_REGS1 ) {
  Term d = Deref(ARG2);
  if (IsVarTerm(d)) {
    Yap_Error(INSTANTIATION_ERROR,d,"depth_bound_call/2");    
  } else if (!IsIntTerm(d)) {
    Yap_Error(TYPE_ERROR_INTEGER, d, "depth_bound_call/2");
    return(FALSE);
  }
  DEPTH = MkIntTerm(IntOfTerm(d)*2);
  return(p_execute( PASS_REGS1 ));
}
#endif

static Int
p_pred_goal_expansion_on( USES_REGS1 ) {
  /* a goal needs expansion if we have goal_expansion defined or
     if the goal is a meta-call */
  return PRED_GOAL_EXPANSION_ON;
}


static Int
exec_absmi(int top USES_REGS)
{
  int lval, out;

  if (top && (lval = sigsetjmp (LOCAL_RestartEnv, 1)) != 0) {
    switch(lval) {
    case 1:
      { /* restart */
	/* otherwise, SetDBForThrow will fail entering critical mode */
	LOCAL_PrologMode = UserMode;
	/* find out where to cut to */
	/* siglongjmp resets the TR hardware register */
	/* TR and B are crucial, they might have been changed, or not */
	restore_TR();
	restore_B();
	/* H is not so important, because we're gonna backtrack */
	restore_H();
	/* set stack */
	ASP = (CELL *)PROTECT_FROZEN_B(B);
	Yap_PopSlots( PASS_REGS1 );
	LOCK(LOCAL_SignalLock);
	/* forget any signals active, we're reborne */
	LOCAL_ActiveSignals = 0;
	CreepFlag = CalculateStackGap();
	LOCAL_PrologMode = UserMode;
	UNLOCK(LOCAL_SignalLock);
	P = (yamop *)FAILCODE;
      }
      break;
    case 2:
      {
	/* arithmetic exception */
	/* must be done here, otherwise siglongjmp will clobber all the registers */
	Yap_Error(LOCAL_matherror ,TermNil,NULL);
	/* reset the registers so that we don't have trash in abstract machine */
	Yap_set_fpu_exceptions(yap_flags[LANGUAGE_MODE_FLAG] == 1);
	P = (yamop *)FAILCODE;
	LOCAL_PrologMode = UserMode;
      }
      break;
    case 3:
      { /* saved state */
	return FALSE;
      }
    default:
      /* do nothing */
      LOCAL_PrologMode = UserMode;
    }
  } else {
    Yap_CloseSlots( PASS_REGS1 );
    LOCAL_PrologMode = UserMode;
  }
  YENV = ASP;
  YENV[E_CB] = Unsigned (B);
  out = Yap_absmi(0);
  /* make sure we don't leave a FAIL signal hanging around */ 
  LOCAL_ActiveSignals &= ~YAP_FAIL_SIGNAL;
  if (!LOCAL_ActiveSignals)
    CreepFlag = CalculateStackGap();
  return out;
}


static  void
init_stack(int arity, CELL *pt, int top, choiceptr saved_b USES_REGS)
{
  /* create an initial pseudo environment so that when garbage
     collection is going up in the environment chain it doesn't get
     confused */
  EX = NULL;
  //  sl = Yap_InitSlot(t);
  YENV = ASP;
  YENV[E_CP] = (CELL)P;
  YENV[E_CB] = (CELL)B;
  YENV[E_E]  = (CELL)ENV;
#ifdef  TABLING
  YENV[E_B] = (CELL)B;
#endif
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
  /* start with some slots so that we can use them */
  Yap_StartSlots( PASS_REGS1 );
  CP = YESCODE;
}

static Int
do_goal(Term t, yamop *CodeAdr, int arity, CELL *pt, int top USES_REGS)
{
  choiceptr saved_b = B;
  Int out;

  init_stack(arity, pt, top, saved_b PASS_REGS);
  P = (yamop *) CodeAdr;
  S = CellPtr (RepPredProp (PredPropByFunc (Yap_MkFunctor(AtomCall, 1),0)));	/* A1 mishaps */

  out = exec_absmi(top PASS_REGS);
  //  if (out) {
  //    out = Yap_GetFromSlot(sl);
  //  }
  //  Yap_RecoverSlots(1);
  return out;
}

Int
Yap_exec_absmi(int top)
{
  CACHE_REGS
  return exec_absmi(top PASS_REGS);
}


Int
Yap_execute_goal(Term t, int nargs, Term mod)
{
  CACHE_REGS
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
    return CallMetaCall(t, mod PASS_REGS);
  }
  PELOCK(81,RepPredProp(pe));
  if (IsAtomTerm(t)) {
    CodeAdr = ppe->CodeOfPred;
    UNLOCK(ppe->PELock);
    out = do_goal(t, CodeAdr, 0, pt, FALSE PASS_REGS);
  } else {
    Functor f = FunctorOfTerm(t);
    CodeAdr = ppe->CodeOfPred;
    UNLOCK(ppe->PELock);
    out = do_goal(t, CodeAdr, ArityOfFunctor(f), pt, FALSE PASS_REGS);
  }

  if (out == 1) {
    choiceptr cut_B;
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
#ifdef TABLING
      abolish_incomplete_subgoals(B);
#endif
    }
#endif /* TABLING */
    B = cut_B;
    CP   = saved_cp;
    P    = saved_p;
    ASP  = ENV;
#ifdef DEPTH_LIMIT
    DEPTH= ENV[E_DEPTH];
#endif
    ENV  = (CELL *)(ENV[E_E]);
    Yap_StartSlots( PASS_REGS1 );
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
  CACHE_REGS
  ASP  = B->cp_env;
  CP   = B->cp_cp;
  H    = B->cp_h;
#ifdef DEPTH_LIMIT
  DEPTH= B->cp_depth;
#endif
  YENV= ASP = B->cp_env;
  ENV  = (CELL *)((B->cp_env)[E_E]);
  B    = B->cp_b;
  P    = (yamop *)(ENV[E_CP]);
  if (B) {
    SET_BB(B);
    HB = PROTECT_FROZEN_H(B);
  }
}

Term
Yap_RunTopGoal(Term t)
{
  CACHE_REGS
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
    pe = PredPropByFunc(f, mod);
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
  PELOCK(82,ppe);
  CodeAdr = ppe->CodeOfPred;
  UNLOCK(ppe->PELock);
#if !USE_SYSTEM_MALLOC
  if (LOCAL_TrailTop - HeapTop < 2048) {
    LOCAL_PrologMode = BootMode;
    Yap_Error(OUT_OF_TRAIL_ERROR,TermNil,
	  "unable to boot because of too little Trail space");
  }
#endif
  goal_out = do_goal(t, CodeAdr, arity, pt, TRUE PASS_REGS);
  return goal_out;
}

static void
restore_regs(Term t USES_REGS)
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
p_restore_regs( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"support for coroutining");    
    return(FALSE);
  }
  if (IsAtomTerm(t)) return(TRUE);
  restore_regs(t PASS_REGS);
  return(TRUE);
}

/* low level voodoo to cut and then restore temporary registers after a call */
static Int
p_restore_regs2( USES_REGS1 )
{

  Term t = Deref(ARG1), d0;
  choiceptr pt0;
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"support for coroutining");    
    return(FALSE);
  }
  d0 = Deref(ARG2);
  if (!IsAtomTerm(t)) {
    restore_regs(t PASS_REGS);
  }
  if (IsVarTerm(d0)) {
    Yap_Error(INSTANTIATION_ERROR,d0,"support for coroutining");    
    return(FALSE);
  }
  if (!IsIntegerTerm(d0)) {
    return(FALSE);
  }
#if YAPOR_SBA
  pt0 = (choiceptr)IntegerOfTerm(d0);
#else
  pt0 = (choiceptr)(LCL0-IntOfTerm(d0));
#endif
  /* find where to cut to */
  if (pt0 > B) {
    /* Wow, we're gonna cut!!! */
    while (B->cp_b < pt0) {
      while (POP_CHOICE_POINT(B->cp_b))
      {
	POP_EXECUTE();
      }
      HB = B->cp_h;
      Yap_TrimTrail();
      B = B->cp_b;
    }
#ifdef TABLING
    abolish_incomplete_subgoals(B);
#endif
#ifdef YAPOR
    CUT_prune_to(pt0);
#endif /* YAPOR */
    B = pt0;
  }
  return(TRUE);
}

static Int
p_clean_ifcp( USES_REGS1 ) {
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
#if YAPOR_SBA
  pt0 = (choiceptr)IntegerOfTerm(t);
#else
  pt0 = cp_from_integer(t PASS_REGS);
#endif
  if (pt0 < B) {
    /* this should never happen */ 
    return TRUE;
  } else if (pt0 == B) {
    while (POP_CHOICE_POINT(B->cp_b))
      {
	POP_EXECUTE();
      }
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
p_cut_up_to_next_disjunction( USES_REGS1 ) {
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
JumpToEnv(Term t USES_REGS) {
#ifndef YAPOR
  yamop *pos = NEXTOP(PredDollarCatch->cs.p_code.TrueCodeOfPred,l),
    *catchpos = NEXTOP(PredHandleThrow->cs.p_code.TrueCodeOfPred,l);
#else
  yamop *pos = NEXTOP(PredDollarCatch->cs.p_code.TrueCodeOfPred,Otapl),
    *catchpos = NEXTOP(PredHandleThrow->cs.p_code.TrueCodeOfPred,Otapl);
#endif
  CELL *env, *env1;
  choiceptr handler, previous = NULL;

  /* throws cannot interrupt throws */
  if (EX)
    return FALSE;
  /* just keep the throwed object away, we don't need to care about it */
  if (!(LOCAL_BallTerm = Yap_StoreTermInDB(t, 0))) {
    Yap_RestartYap( 1 );
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
	if (previous == NULL) {
	  B = handler;
	} else {
	  previous->cp_b = handler;
	}
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
	LOCAL_UncaughtThrow = TRUE;
	if (previous == NULL)
	  B = handler;
	else 
	  previous->cp_b = handler;
	EX = LOCAL_BallTerm;
	LOCAL_BallTerm = NULL;
	P = (yamop *)FAILCODE;
	/* make sure failure will be seen at next port */
	if (LOCAL_PrologMode & AsyncIntMode) {
	  Yap_signal(YAP_FAIL_SIGNAL);
	}
	HB = B->cp_h;
	return TRUE;
      }
      /* make sure we prune C-choicepoints */
      while (POP_CHOICE_POINT(handler->cp_b))
	{
	  POP_EXECUTE();
	}
      handler = handler->cp_b;
    }
    /* uncaught throw */
    if (handler == NULL) {
      LOCAL_UncaughtThrow = TRUE;
      Yap_RestartYap( 1 );
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
  /* make sure we get rid of trash in the trail */
  handler->cp_cp = (yamop *)env[E_CP];
  handler->cp_env = (CELL *)env[E_E];
  handler->cp_ap = catchpos;

  /* can recover Heap thanks to copy term :-( */
  /* B->cp_h = H; */
  /* I could backtrack here, but it is easier to leave the unwinding
     to the emulator */
  if (LOCAL_PrologMode & AsyncIntMode) {
    Yap_signal(YAP_FAIL_SIGNAL);
  }
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
  CACHE_REGS
  if (LOCAL_PrologMode & BootMode) {
    return FALSE;
  } 
  return JumpToEnv(t PASS_REGS);
}


/* This does very nasty stuff!!!!! */
static Int
p_jump_env( USES_REGS1 ) {
  return(JumpToEnv(Deref(ARG1) PASS_REGS));
}

/* set up a meta-call based on . context info */
static Int
p_generate_pred_info( USES_REGS1 ) {
  ARG1 = ARG3 = ENV[-EnvSizeInCells-1];
  ARG4 = ENV[-EnvSizeInCells-3];
  ARG2 = cp_as_integer((choiceptr)ENV[E_CB] PASS_REGS);
  return TRUE;
}

void
Yap_InitYaamRegs(void)
{
  CACHE_REGS
  Term h0var;
#if PUSH_REGS
  /* Guarantee that after a longjmp we go back to the original abstract
     machine registers */
#ifdef THREADS
  int myworker_id = worker_id;
  pthread_setspecific(Yap_yaamregs_key, (const void *)REMOTE_ThreadHandle(myworker_id).default_yaam_regs);
  REMOTE_ThreadHandle(myworker_id).current_yaam_regs = REMOTE_ThreadHandle(myworker_id).default_yaam_regs;
  worker_id = myworker_id;  /* ricroc: for what I understand, this shouldn't be necessary */
#else
  Yap_regp = &Yap_standard_regs;
#endif
#endif /* PUSH_REGS */
  Yap_ResetExceptionTerm ();
  Yap_PutValue (AtomBreak, MkIntTerm (0));
  TR = (tr_fr_ptr)LOCAL_TrailBase;
  if (Yap_AttsSize > (LOCAL_LocalBase-LOCAL_GlobalBase)/8)
    Yap_AttsSize = (LOCAL_LocalBase-LOCAL_GlobalBase)/8;
  H = H0 = ((CELL *) LOCAL_GlobalBase)+ Yap_AttsSize/sizeof(CELL);
  RESET_VARIABLE(H0-1);
  LCL0 = ASP = (CELL *) LOCAL_LocalBase;
  CurrentTrailTop = (tr_fr_ptr)(LOCAL_TrailTop-MinTrailGap);
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
#ifdef YAPOR_SBA
  BSEG =
#endif /* YAPOR_SBA */
  BBREG = B_FZ = (choiceptr) LOCAL_LocalBase;
  TR = TR_FZ = (tr_fr_ptr) LOCAL_TrailBase;
#endif /* FROZEN_STACKS */
  LOCK(LOCAL_SignalLock);
  CreepFlag = CalculateStackGap();
  UNLOCK(LOCAL_SignalLock);
  EX = NULL;
  init_stack(0, NULL, TRUE, NULL PASS_REGS);
  /* the first real choice-point will also have AP=FAIL */ 
  /* always have an empty slots for people to use */
  CurSlot = 0;
  Yap_StartSlots( PASS_REGS1 );
  LOCAL_GlobalArena = TermNil;
  h0var = MkVarTerm();
#if COROUTINING
  LOCAL_WokenGoals = Yap_NewTimedVar(TermNil);
  LOCAL_AttsMutableList = Yap_NewTimedVar(h0var);
#endif
  LOCAL_GcGeneration = Yap_NewTimedVar(h0var);
  LOCAL_GcCurrentPhase = 0L;
  LOCAL_GcPhase = Yap_NewTimedVar(MkIntTerm(LOCAL_GcCurrentPhase));
#if defined(YAPOR) || defined(THREADS)
  PP = NULL;
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
p_uncaught_throw( USES_REGS1 )
{
  Int out = LOCAL_UncaughtThrow;
  LOCAL_UncaughtThrow = FALSE; /* just caught it */
  return out;
}

static Int
p_creep_allowed( USES_REGS1 )
{
  if (PP != NULL) {
    LOCK(LOCAL_SignalLock);
    if (LOCAL_ActiveSignals & YAP_CREEP_SIGNAL  && !LOCAL_InterruptsDisabled) {
      LOCAL_ActiveSignals &= ~YAP_CREEP_SIGNAL;    
      if (!LOCAL_ActiveSignals)
	CreepFlag = CalculateStackGap();
      UNLOCK(LOCAL_SignalLock);
    } else {
      UNLOCK(LOCAL_SignalLock);
    }
    return TRUE;
  }
  return FALSE;
}

static Int
p_debug_on( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {
    if (LOCAL_DebugOn)
      return Yap_unify(MkAtomTerm(AtomTrue),ARG1);
    else
      return Yap_unify(MkAtomTerm(AtomFalse),ARG1);
  }
  if (t == MkAtomTerm(AtomTrue))
    LOCAL_DebugOn = TRUE;
  else
    LOCAL_DebugOn = FALSE;
  return TRUE;
}

Term 
Yap_GetException(void)
{
  CACHE_REGS
  Term t = 0L;
  if (LOCAL_BallTerm) {
    do {
      t = Yap_PopTermFromDB(LOCAL_BallTerm);
      if (t == 0) {
	if (LOCAL_Error_TYPE == OUT_OF_ATTVARS_ERROR) {
	  LOCAL_Error_TYPE = YAP_NO_ERROR;
	  if (!Yap_growglobal(NULL)) {
	    Yap_Error(OUT_OF_ATTVARS_ERROR, TermNil, LOCAL_ErrorMessage);
	    return FALSE;
	  }
	} else {
	  LOCAL_Error_TYPE = YAP_NO_ERROR;
	  if (!Yap_growstack(LOCAL_BallTerm->NOfCells*CellSize)) {
	    Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	    return FALSE;
	  }
	}
      }
    } while (t == 0);
    LOCAL_BallTerm = NULL;
  }
  return t;
}

static Int
p_reset_exception( USES_REGS1 )
{
  Term t;
  EX = NULL;
  t = Yap_GetException();
  if (!t) 
    return FALSE;
  return Yap_unify(t, ARG1);
}

void
Yap_ResetExceptionTerm(void)
{
  CACHE_REGS
  Yap_ReleaseTermFromDB(LOCAL_BallTerm);
  LOCAL_BallTerm = NULL;
}

static Int
p_get_exception( USES_REGS1 )
{
  Term t = Yap_GetException();
  if (!t) 
    return FALSE;
  return Yap_unify(t, ARG1);
}

void 
Yap_InitExecFs(void)
{
  CACHE_REGS
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


