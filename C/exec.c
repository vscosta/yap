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
 * File:		exec.c *
 * Last rev:	8/2/88							 *
 * mods: *
 * comments:	Execute Prolog code					 *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "@(#)cdmgr.c	1.1 05/02/98";
#endif

#include "absmi.h"
#include "attvar.h"
#include "cut_c.h"
#include "yapio.h"
#include "yapio.h"

static bool CallPredicate(PredEntry *, choiceptr, yamop *CACHE_TYPE);
// must hold thread worker comm lock at call.
static bool EnterCreepMode(Term, Term CACHE_TYPE);
static Int current_choice_point(USES_REGS1);
static Int execute(USES_REGS1);
static Int execute0(USES_REGS1);

static Term cp_as_integer(choiceptr cp USES_REGS) {
  return (MkIntegerTerm(LCL0 - (CELL *)cp));
}

static choiceptr cp_from_integer(Term cpt USES_REGS) {
  return (choiceptr)(LCL0 - IntegerOfTerm(cpt));
}

/**
 * Represents a choice-point as an offset to the top of local stack. This should
 * *be stable acroos gc or stack shifts.
 * @method Yap_cp_as_integer
 * @param  cp                pointer to choice-point
 * @return                   Term with offset
 */
Term Yap_cp_as_integer(choiceptr cp) {
  CACHE_REGS
  return cp_as_integer(cp PASS_REGS);
}

/**
 * Sets up the engine to run a different predicate.
 * @method CallPredicate
 * @param  pen           the new code
 * @param  cut_pt        cut boundary
 * @param  USES_REGS     thread support
 * @return               success
 */
static inline bool CallPredicate(PredEntry *pen, choiceptr cut_pt,
                                 yamop *code USES_REGS) {
#ifdef LOW_LEVEL_TRACER
  if (Yap_do_low_level_trace)
    low_level_trace(enter_pred, pen, XREGS + 1);
#endif /* LOW_LEVEL_TRACE */
#ifdef DEPTH_LIMIT
  if (DEPTH <= MkIntTerm(1)) { /* I assume Module==0 is prolog */
    if (pen->ModuleOfPred) {
      if (DEPTH == MkIntTerm(0)) {
        UNLOCK(pen->PELock);
        return false;
      } else
        DEPTH = RESET_DEPTH();
    }
  } else if (pen->ModuleOfPred)
    DEPTH -= MkIntConstant(2);
#endif /* DEPTH_LIMIT */
  if (P->opc != EXECUTE_CPRED_OP_CODE) {
    //	YENV[E_CP] = CP;
    //      YENV[E_E] = ENV;
    //#ifdef DEPTH_LIMIT
    //	YENV[E_DEPTH] = DEPTH;
    //#endif
    //        ENV = YENV;
    ENV = YENV;
    YENV = ASP;
    CP = P;
  }
  /* make sure we have access to the user given cut */
  YENV[E_CB] = (CELL)cut_pt;
  P = code;
  return true;
}

/**
 * calls a meta-predicate or anything weird
 * @method CallMetaCall
 * @param  t            the called goal
 * @param  USES_REGS    MT
 * @return              did we fiid it?
 */
inline static bool CallMetaCall(Term t, Term mod USES_REGS) {
  // we have a creep requesr waiting

  ARG1 = t;
  ARG2 = cp_as_integer(B PASS_REGS); /* p_current_choice_point */
  ARG3 = t;
  if (mod) {
    ARG4 = mod;
  } else {
    ARG4 = TermProlog;
  }
  if (Yap_GetGlobal(AtomDebugMeta) == TermOn) {
    return CallPredicate(PredTraceMetaCall, B,
                         PredTraceMetaCall->CodeOfPred PASS_REGS);
  } else {
    return CallPredicate(PredMetaCall, B, PredMetaCall->CodeOfPred PASS_REGS);
  }
}

/**
 * Transfer control to a meta-call in ARG1, cut up to B.
 * @method Yap_ExecuteCallMetaCall
 * @param  mod                     current module
 * @return                         su
 */
Term Yap_ExecuteCallMetaCall(Term mod) {
  CACHE_REGS
  Term ts[4];
  ts[0] = ARG1;
  ts[1] = cp_as_integer(B PASS_REGS); /* p_current_choice_point */
  ts[2] = ARG1;
  ts[3] = mod;
  if (Yap_GetGlobal(AtomDebugMeta) == TermOn) {
    return Yap_MkApplTerm(PredTraceMetaCall->FunctorOfPred, 3, ts);
  }
  return Yap_MkApplTerm(PredMetaCall->FunctorOfPred, 4, ts);
}

Term Yap_PredicateIndicator(Term t, Term mod) {
  CACHE_REGS
  // generate predicate indicator in this case
  Term ti[2];
  t = Yap_YapStripModule(t, &mod);
  if (IsApplTerm(t) && !IsExtensionFunctor(FunctorOfTerm(t))) {
    ti[0] = MkAtomTerm(NameOfFunctor(FunctorOfTerm(t)));
    ti[1] = MkIntegerTerm(ArityOfFunctor(FunctorOfTerm(t)));
  } else if (IsPairTerm(t)) {
    ti[0] = MkAtomTerm(AtomDot);
    ti[1] = MkIntTerm(2);
  } else {
    ti[0] = t;
    ti[1] = MkIntTerm(0);
  }
  t = Yap_MkApplTerm(FunctorSlash, 2, ti);
  if (mod != CurrentModule) {
    ti[0] = mod;
    ti[1] = t;
    return Yap_MkApplTerm(FunctorModule, 2, ti);
  }
  return t;
}

static bool CallError(yap_error_number err, Term t, Term mod USES_REGS) {
  if (isoLanguageFlag()) {
    return (CallMetaCall(t, mod PASS_REGS));
  } else {
    if (err == TYPE_ERROR_CALLABLE) {
      t = Yap_YapStripModule(t, &mod);
    }
    Yap_Error(err, t, "call/1");
    return false;
  }
}

/** @pred current_choice_point( -CP )
 *
 * unify the logic variable _CP_ with a number that gives the offset of the
 * current choice-point. This number is only valid as long as we do not
 *backtrack by or cut
 * _CP_, and is safe in the presence of stack shifting and/or garbage
 *collection.
 */
static Int current_choice_point(USES_REGS1) {
  Term t = Deref(ARG1);
  Term td;
#if SHADOW_HB
  register CELL *HBREG = HB;
#endif
  if (!IsVarTerm(t))
    return (FALSE);
  td = cp_as_integer(B PASS_REGS);
  YapBind((CELL *)t, td);
  return TRUE;
}

static Int save_env_b(USES_REGS1) {
  Term t = Deref(ARG1);
  Term td;
#if SHADOW_HB
  register CELL *HBREG = HB;
#endif
  if (!IsVarTerm(t))
    return (FALSE);
  td = cp_as_integer((choiceptr)YENV[E_CB] PASS_REGS);
  YapBind((CELL *)t, td);
  return true;
}

inline static bool do_execute(Term t, Term mod USES_REGS) {
  Term t0 = t;
  t = Yap_YapStripModule(t, &mod);
  /* first do predicate expansion, even before you process signals.
    This way you don't get to spy goal_expansion(). */
  if (Yap_has_a_signal() && !LOCAL_InterruptsDisabled &&
      !(LOCAL_PrologMode & (AbortMode | InterruptMode | SystemMode))) {
    return EnterCreepMode(t, mod PASS_REGS);
  }
restart_exec:
  if (IsVarTerm(t) || IsVarTerm(mod)) {
    return CallError(INSTANTIATION_ERROR, t0, mod PASS_REGS);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    register CELL *pt;
    PredEntry *pen;
    unsigned int i, arity;

    f = FunctorOfTerm(t);
    if (IsExtensionFunctor(f)) {
      return CallError(TYPE_ERROR_CALLABLE, t, mod PASS_REGS);
    }
    arity = ArityOfFunctor(f);
    if (arity > MaxTemps) {
      return CallError(TYPE_ERROR_CALLABLE, t, mod PASS_REGS);
    }
    pen = RepPredProp(PredPropByFunc(f, mod));
    /* You thought we would be over by now */
    /* but no meta calls require special preprocessing */
    if (pen->PredFlags & MetaPredFlag) {
      return CallMetaCall(t, mod PASS_REGS);
    }
    /* now let us do what we wanted to do from the beginning !! */
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t) + 1;
    for (i = 1; i <= arity; i++) {
#if YAPOR_SBA
      Term d0 = *pt++;
      if (d0 == 0)
        ` XREGS[i] = (CELL)(pt - 1);
      else
        XREGS[i] = d0;
#else

      XREGS[i] = *pt++;
#endif
    }
    return (CallPredicate(pen, B, pen->CodeOfPred PASS_REGS));
  } else if (IsAtomTerm(t)) {
    PredEntry *pe;
    Atom a = AtomOfTerm(t);

    if (a == AtomTrue || a == AtomOtherwise || a == AtomCut)
      return true;
    else if (a == AtomFail ||
             (a == AtomFalse &&
              !RepPredProp(PredPropByAtom(a, mod))->ModuleOfPred))
      return false;
    /* call may not define new system predicates!! */
    pe = RepPredProp(PredPropByAtom(a, mod));
    return (CallPredicate(pe, B, pe->CodeOfPred PASS_REGS));
  } else if (IsIntTerm(t)) {
    return CallError(TYPE_ERROR_CALLABLE, t, mod PASS_REGS);
  } else {
    /* Is Pair Term */
    return (CallMetaCall(t, mod PASS_REGS));
  }
}

static Term copy_execn_to_heap(Functor f, CELL *pt, unsigned int n,
                               unsigned int arity, Term mod USES_REGS) {
  CELL *h0 = HR;
  Term tf;
  unsigned int i;

  if (arity == 2 && NameOfFunctor(f) == AtomDot) {
    for (i = 0; i < arity - n; i++) {
      *HR++ = pt[i];
    }
    for (i = 0; i < n; i++) {
      *HR++ = h0[(int)(i - n)];
    }
    tf = AbsPair(h0);
  } else {
    *HR++ = (CELL)f;
    for (i = 0; i < arity - n; i++) {
      *HR++ = pt[i];
    }
    for (i = 0; i < n; i++) {
      *HR++ = h0[(int)(i - n)];
    }
    tf = AbsAppl(h0);
  }
  if (mod != CurrentModule) {
    CELL *h0 = HR;
    *HR++ = (CELL)FunctorModule;
    *HR++ = mod;
    *HR++ = tf;
    tf = AbsAppl(h0);
  }
  return tf;
}

inline static bool do_execute_n(Term t, Term mod, unsigned int n USES_REGS) {
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
    return CallError(TYPE_ERROR_CALLABLE, t, mod PASS_REGS);
  } else if (IsPairTerm(t)) {
    arity = n + 2;
    pt = RepPair(t);
    Name = AtomOfTerm(TermNil);
  } else /* if (IsApplTerm(t)) */ {
    f = FunctorOfTerm(t);
    while (f == FunctorModule) {
      Term tmod = ArgOfTerm(1, t);
      if (!IsVarTerm(tmod) && IsAtomTerm(tmod)) {
        mod = tmod;
        t = ArgOfTerm(2, t);
        goto restart_exec;
      } else {
        if (IsVarTerm(tmod)) {
          return CallError(INSTANTIATION_ERROR, t0, tmod PASS_REGS);
        } else {
          return CallError(TYPE_ERROR_ATOM, t0, tmod PASS_REGS);
        }
      }
    }
    arity = ArityOfFunctor(f) + n;
    Name = NameOfFunctor(f);
    pt = RepAppl(t) + 1;
  }
  f = Yap_MkFunctor(Name, arity);
  if (IsExtensionFunctor(f)) {
    return CallError(TYPE_ERROR_CALLABLE, t, mod PASS_REGS);
  }
  if (Yap_has_a_signal() && !LOCAL_InterruptsDisabled) {
    return EnterCreepMode(
        copy_execn_to_heap(f, pt, n, arity, CurrentModule PASS_REGS),
        mod PASS_REGS);
  }
  if (arity > MaxTemps) {
    return CallError(TYPE_ERROR_CALLABLE, t, mod PASS_REGS);
  }
  pen = RepPredProp(PredPropByFunc(f, mod));
  /* You thought we would be over by now */
  /* but no meta calls require special preprocessing */
  if (pen->PredFlags & MetaPredFlag) {
    Term t = copy_execn_to_heap(f, pt, n, arity, mod PASS_REGS);
    return (CallMetaCall(t, mod PASS_REGS));
  }
  /* now let us do what we wanted to do from the beginning !! */
  /* I cannot use the standard macro here because
     otherwise I would dereference the argument and
     might skip a svar */
  for (i = 1; i <= arity - n; i++) {
#if YAPOR_SBA
    Term d0 = *pt++;
    if (d0 == 0)
      XREGS[i] = (CELL)(pt - 1);
    else
      XREGS[i] = d0;
#else
    XREGS[i] = *pt++;
#endif
  }
  for (i = arity - n + 1; i <= arity; i++, j++) {
    XREGS[i] = HR[j];
  }
  return CallPredicate(pen, B, pen->CodeOfPred PASS_REGS);
}

// enter locked
static bool EnterCreepMode(Term t, Term mod USES_REGS) {
  PredEntry *PredCreep;

  if (Yap_get_signal(YAP_CDOVF_SIGNAL)) {
    ARG1 = t;
    if (!Yap_locked_growheap(FALSE, 0, NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil,
                "YAP failed to grow heap at meta-call");
    }
    if (!Yap_has_a_signal()) {
      return do_execute(ARG1, mod PASS_REGS);
    }
  }
  PredCreep = RepPredProp(PredPropByFunc(FunctorCreep, 1));
  PP = PredCreep;
  if (!IsVarTerm(t) && IsApplTerm(t) && FunctorOfTerm(t) == FunctorModule) {
    ARG1 = MkPairTerm(ArgOfTerm(1, t), ArgOfTerm(2, t));
  } else {
    if (mod) {
      ARG1 = MkPairTerm(mod, t);
    } else {
      ARG1 = MkPairTerm(TermProlog, t);
    }
  }
  CalculateStackGap(PASS_REGS1);
  P_before_spy = P;
  return CallPredicate(PredCreep, B, PredCreep->CodeOfPred PASS_REGS);
}

static Int execute(USES_REGS1) { /* '$execute'(Goal)	 */
  Term t = Deref(ARG1);
  return do_execute(t, CurrentModule PASS_REGS);
}

bool Yap_Execute(Term t USES_REGS) { /* '$execute'(Goal)	 */
  return do_execute(t, CurrentModule PASS_REGS);
}

static void heap_store(Term t USES_REGS) {
  if (IsVarTerm(t)) {
    if (VarOfTerm(t) < HR) {
      *HR++ = t;
    } else {
      RESET_VARIABLE(HR);
      Bind_Local(VarOfTerm(t), (CELL)HR);
      HR++;
    }
  } else {
    *HR++ = t;
  }
}

static Int execute2(USES_REGS1) { /* '$execute'(Goal)	 */
  Term t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  return do_execute_n(t, CurrentModule, 1 PASS_REGS);
}

static Int execute3(USES_REGS1) { /* '$execute'(Goal)	 */
  Term t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  return do_execute_n(t, CurrentModule, 2 PASS_REGS);
}

static Int execute4(USES_REGS1) { /* '$execute'(Goal)	 */
  Term t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  return do_execute_n(t, CurrentModule, 3 PASS_REGS);
}

static Int execute5(USES_REGS1) { /* '$execute'(Goal)	 */
  Term t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  heap_store(Deref(ARG5) PASS_REGS);
  return do_execute_n(t, CurrentModule, 4 PASS_REGS);
}

static Int execute6(USES_REGS1) { /* '$execute'(Goal)	 */
  Term t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  heap_store(Deref(ARG5) PASS_REGS);
  heap_store(Deref(ARG6) PASS_REGS);
  return do_execute_n(t, CurrentModule, 5 PASS_REGS);
}

static Int execute7(USES_REGS1) { /* '$execute'(Goal)	 */
  Term t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  heap_store(Deref(ARG5) PASS_REGS);
  heap_store(Deref(ARG6) PASS_REGS);
  heap_store(Deref(ARG7) PASS_REGS);
  return do_execute_n(t, CurrentModule, 6 PASS_REGS);
}

static Int execute8(USES_REGS1) { /* '$execute'(Goal)	 */
  Term t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  heap_store(Deref(ARG5) PASS_REGS);
  heap_store(Deref(ARG6) PASS_REGS);
  heap_store(Deref(ARG7) PASS_REGS);
  heap_store(Deref(ARG8) PASS_REGS);
  return do_execute_n(t, CurrentModule, 7 PASS_REGS);
}

static Int execute9(USES_REGS1) { /* '$execute'(Goal)	 */
  Term t = Deref(ARG1);
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

static Int execute10(USES_REGS1) { /* '$execute'(Goal)	 */
  Term t = Deref(ARG1);
  heap_store(Deref(ARG2) PASS_REGS);
  heap_store(Deref(ARG3) PASS_REGS);
  heap_store(Deref(ARG4) PASS_REGS);
  heap_store(Deref(ARG5) PASS_REGS);
  heap_store(Deref(ARG6) PASS_REGS);
  heap_store(Deref(ARG7) PASS_REGS);
  heap_store(Deref(ARG8) PASS_REGS);
  heap_store(Deref(ARG9) PASS_REGS);
  heap_store(Deref(ARG10) PASS_REGS);
  return (do_execute_n(t, CurrentModule, 9 PASS_REGS));
}

static Int execute11(USES_REGS1) { /* '$execute'(Goal)	 */
  Term t = Deref(ARG1);
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
  return (do_execute_n(t, CurrentModule, 10 PASS_REGS));
}

static Int execute12(USES_REGS1) { /* '$execute'(Goal)	 */
  Term t = Deref(ARG1);

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
  return (do_execute_n(t, CurrentModule, 11 PASS_REGS));
}

static Int execute_clause(USES_REGS1) { /* '$execute_clause'(Goal)	 */
  Term t = Deref(ARG1);
  Term mod = Deref(ARG2);
  choiceptr cut_cp = cp_from_integer(Deref(ARG4) PASS_REGS);
  unsigned int arity;
  Prop pe;
  yamop *code;
  Term clt = Deref(ARG3);

restart_exec:
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, ARG3, "call/1");
    return FALSE;
  } else if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    pe = PredPropByAtom(a, mod);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    register unsigned int i;
    register CELL *pt;

    if (IsExtensionFunctor(f))
      return (FALSE);
    if (f == FunctorModule) {
      Term tmod = ArgOfTerm(1, t);
      if (!IsVarTerm(tmod) && IsAtomTerm(tmod)) {
        mod = tmod;
        t = ArgOfTerm(2, t);
        goto restart_exec;
      }
    }
    pe = PredPropByFunc(f, mod);
    arity = ArityOfFunctor(f);
    if (arity > MaxTemps) {
      return CallError(TYPE_ERROR_CALLABLE, t, mod PASS_REGS);
    }
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t) + 1;
    for (i = 1; i <= arity; ++i) {
#if YAPOR_SBA
      Term d0 = *pt++;
      if (d0 == 0)
        XREGS[i] = (CELL)(pt - 1);
      else
        XREGS[i] = d0;
#else
      XREGS[i] = *pt++;
#endif
    }
  } else {
    return CallError(TYPE_ERROR_CALLABLE, t, mod PASS_REGS);
  }
  /*	N = arity; */
  /* call may not define new system predicates!! */
  if (RepPredProp(pe)->PredFlags & MegaClausePredFlag) {
    code = Yap_MegaClauseFromTerm(clt);
  } else {
    code = Yap_ClauseFromTerm(clt)->ClCode;
  }
  if (Yap_get_signal(YAP_CREEP_SIGNAL)) {
    Yap_signal(YAP_CREEP_SIGNAL);
  }
  return CallPredicate(RepPredProp(pe), cut_cp, code PASS_REGS);
}

static Int execute_in_mod(USES_REGS1) { /* '$execute'(Goal)	 */
  return do_execute(Deref(ARG1), Deref(ARG2) PASS_REGS);
}

typedef enum {
  CALLED_FROM_CALL = 0x1,
  CALLED_FROM_ANSWER = 0x2,
  CALLED_FROM_EXIT = 0x4,
  CALLED_FROM_RETRY = 0x8,
  CALLED_FROM_FAIL = 0x18,
  CALLED_FROM_CUT = 0x20,
  CALLED_FROM_EXCEPTION = 0x40,
  CALLED_FROM_THROW = 0x80
} execution_port;

INLINE_ONLY inline bool called_from_forward(execution_port port) {
  return port & (CALLED_FROM_EXIT | CALLED_FROM_CALL | CALLED_FROM_ANSWER |
                 CALLED_FROM_CUT | CALLED_FROM_THROW);
}

INLINE_ONLY inline bool called_from_backward(execution_port port) {
  return port & (CALLED_FROM_RETRY | CALLED_FROM_FAIL | CALLED_FROM_EXCEPTION);
}

/**
 * remove choice points created since a call to top-goal.
 *
 * @method prune_inner_computation
 */
static void prune_inner_computation(choiceptr parent) {
  /* code */
  choiceptr cut_pt;
  yamop *oP = P, *oCP = CP;
  Int oENV = LCL0 - ENV;

  cut_pt = B;
  while (cut_pt < parent) {
    /* make sure we
    e C-choicepoints */
    if (POP_CHOICE_POINT(cut_pt->cp_b)) {
      POP_EXECUTE();
    }
    cut_pt = cut_pt->cp_b;
  }
#ifdef YAPOR
  CUT_prune_to(cut_pt);
#endif
  B = parent;
  Yap_TrimTrail();
  LOCAL_AllowRestart = FALSE;
  P = oP;
  CP = oCP;
  ENV = LCL0 - oENV;
}
/**
 * restore abstract machine state
 * after completing a computation.
 * @method complete_inner_computation
 */
static void complete_inner_computation(choiceptr old_B) {
  choiceptr myB = B;
  if (myB == NULL) {
    return;
  } else if (myB->cp_b == old_B) {
    B = old_B;
#ifdef DEPTH_LIMIT
    DEPTH = myB->cp_depth;
#endif
  } else if (myB->cp_b && myB->cp_b < old_B) {
    while (myB->cp_b < old_B) {
      // we're recovering from a non-deterministic computation...
      myB = myB->cp_b;
    }
  } else {
    return;
  }
  // restore environment at call...
  CP = myB->cp_cp;
  ENV = myB->cp_env;
}

static inline Term *GetTermAddress(CELL a) {
  Term *b = NULL;
restart:
  if (!IsVarTerm(a)) {
    return (b);
  } else if (a == (CELL)b) {
    return (b);
  } else {
    b = (CELL *)a;
    a = *b;
    goto restart;
  }
}

/**
 * call a cleanup routine taking care with the status variable.
 */
static bool call_cleanup(Term t3, Term t4, Term cleanup,
                         choiceptr B0 USES_REGS) {
  CELL *pt = GetTermAddress(t3);
  DBTerm *ball = Yap_RefToException();
  if (pt == NULL)
    return false;
  *pt = cleanup;
  bool out = Yap_RunTopGoal(t4, true);
  if (out) {
    prune_inner_computation(B0);
  } else {
    complete_inner_computation(B0);
  }
  pt = GetTermAddress(t3);
  if (ball)
    Yap_CopyException(ball);
  if (pt == NULL) {
    return false;
  }
  RESET_VARIABLE(pt);
  return true;
}

/**
 * What to do when we exit a protected call
 * @method exit_set_call
 * @param  exec_result   result of call (0 or 1)
 * @param  b0            original choicepointer (pointed to by root)
 * @param  t3            state
 * @param  b0            user goal to call on port.
 *
 * @param  USES_REGS     [description]
 * @return               [description]
 */
static bool exit_set_call(execution_port exec_result, choiceptr B0, yamop *oCP,
                          Term t3, Term t4 USES_REGS) {
  Term rc;

  switch (exec_result) {
  // we failed
  // Exception: we'll pass it through
  case CALLED_FROM_EXCEPTION:
    // internal exception
    {
      Term ball = Yap_PeekException();
      Term signal = Yap_MkApplTerm(FunctorException, 1, &ball);
      rc = signal;
      B = B0;
    }
    break;
  case CALLED_FROM_THROW:
    // internal exception
    {
      Term ball = Yap_PeekException();
      Term signal = Yap_MkApplTerm(FunctorException, 1, &ball);
      rc = signal;
      B = B0;
    }
    break;
  case CALLED_FROM_RETRY:
    // external exception
    rc = TermRetry;
    // internal failure
    return true;
    break;
  case CALLED_FROM_FAIL:
    B = B0;
    rc = TermFail;
    break;
  case CALLED_FROM_EXIT:
    // deterministic exit
    rc = TermExit;
    if (B->cp_b == B0) {
      CP = B->cp_cp;
      ENV = B->cp_env;
      ASP = (CELL *)B;
      B = B0;
    }
    break;
  case CALLED_FROM_CUT:
    if (B->cp_b == B0) {
      CP = B->cp_cp;
      ENV = B->cp_env;
      ASP = (CELL *)B;
      B = B0;
    }
    rc = TermCut;
    break;
  case CALLED_FROM_CALL:
    // cut exit
    rc = TermCall;
    break;
  case CALLED_FROM_ANSWER:
    //   cut exit
    rc = TermAnswer;
    // non deterministic
    choiceptr saved_b = B;
    CELL *pt = ASP;
    CUT_C_PUSH(
        NEXTOP(NEXTOP(PredProtectStack->cs.p_code.FirstClause, OtapFs), OtapFs),
        pt); // this is where things get complicated, we need to
    // protect the stack and be able to backtrack
    pt -= 4;
    pt[3] = t4;
    pt[2] = t3;
    pt[1] = MkAddressTerm(oCP);
    pt[0] = MkIntegerTerm(LCL0 - (CELL *)B0);
    B = (choiceptr)pt;
    B--;
    B->cp_h = HR;
    B->cp_tr = TR;
    B->cp_cp = oCP;
    B->cp_ap = NEXTOP(PredProtectStack->cs.p_code.FirstClause, OtapFs);
    B->cp_env = ENV;
    B->cp_b = saved_b;
#ifdef DEPTH_LIMIT
    B->cp_depth = saved_b->cp_depth;
#endif /* DEPTH_LIMIT */
    YENV = ASP = (CELL *)B;
    YENV[E_CB] = (CELL)B;
    HB = HR;

    return true;
  }
  call_cleanup(t3, t4, rc, B PASS_REGS);

  return true;
}

static Int protect_stack_from_cut(USES_REGS1) {
  // called after backtracking..
  /* reinitialize the engine */
  /* the first real choice-point will also have AP=FAIL */
  /* always have an empty slots for people to use */
  YENV = ASP = (CELL *)B;
  call_cleanup(B->cp_a3, B->cp_a4, (P == FAILCODE ? TermException : TermCut),
               B PASS_REGS);
  return true;
}

/**
 * external backtrack to current stack frame: call method
 * and control backtracking.
 *
 * @`
 * method protect_stack_from_restore
 * @param  USES_REGS1                 [env for threaded execution]
 * @return                       c
 [next answer]
 */
static Int protect_stack_from_retry(USES_REGS1) {
  // called after backtracking..
  //
  yamop *oP = P;
  Int oENV = LCL0 - ENV;
  yamop *oCP = (yamop *)AddressOfTerm(B->cp_a2);
  Term t3 = B->cp_a3;
  Term t4 = B->cp_a4;
  Int b0 = IntegerOfTerm(ARG1);
  choiceptr B0 = (choiceptr)(LCL0 - b0);

  cut_c_pop();

  // call_cleanup(t3, t4, TermRetry, B0 USES_REGS);
  // binding to t3 should be undone
  // by next backtrack.
  /* first, destroy the current choice-point,
   */
  B = B->cp_b;
  // B should lead to CP with _ystop,,
  P = FAILCODE;
  bool res = Yap_exec_absmi(false, CurrentModule);
  /* reinitialize the engine */
  /* the first real choice-point will also have AP=FAIL */
  /* always have an empty slots for people to use */
  // ensure that we have slots where we need the
  execution_port p;
  if (res) {
    if (Yap_HasException()) {
      p = CALLED_FROM_THROW;
    } else if (B->cp_b >= B0) {
      p = CALLED_FROM_EXIT;
    } else
      p = CALLED_FROM_ANSWER;
  } else {
    if (Yap_HasException())
      p = CALLED_FROM_EXCEPTION;
    else
      p = CALLED_FROM_FAIL;
  }
  Int rc = exit_set_call(p, B0, oCP, t3, t4 PASS_REGS);
  if (rc) {
    CP = oCP;
    P = oP;
    ENV = LCL0 - oENV;
  }
  if (Yap_RaiseException())
    return false;
  return res;
}

/**
 * First call to non deterministic predicate. Just leaves a choice-point
 * hanging about for the future.
 *
 * @method protect_stack
 * @param  USES_REGS1    [env for threaded execution]
 * @return               [always succeed]
 */
static Int protect_stack(USES_REGS1) {

  // just create the choice-point;
  return true;
}

static Int setup_call_catcher_cleanup(USES_REGS1) {
  Term Setup = Deref(ARG1);
  Term cmod = CurrentModule;
  Int oENV = LCL0 - ENV;
  choiceptr B0 = B;
  Term t3, t4;
  yhandle_t hl = Yap_StartSlots();
  yhandle_t h2 = Yap_InitHandle(ARG2);
  yhandle_t h3 = Yap_InitHandle(t3 = Deref(ARG3));
  yhandle_t h4 = Yap_InitHandle(ARG4);
  yamop *oCP = CP, *oP = P;
  bool rc;
  execution_port port;

  Yap_DisableInterrupts(worker_id);
  rc = Yap_RunTopGoal(Setup, false);
  Yap_EnableInterrupts(worker_id);

  if (Yap_RaiseException()) {
    return false;
  }
  if (!rc) {
    complete_inner_computation(B0);
    // We'll pass it through

    return false;
  } else {
    prune_inner_computation(B0);
  }
  // at this point starts actual goal execution....
  cmod = CurrentModule;

  rc = Yap_RunTopGoal(Yap_GetFromSlot(h2), false);
  complete_inner_computation(B);
  t4 = Yap_GetFromSlot(h4);
  t3 = Yap_GetFromSlot(h3);
  // make sure that t3 point to our nice cell.
  Yap_CloseSlots(hl);

  if (rc) {
    // ignore empty choice
    while (B->cp_ap->opc == FAIL_OPCODE)
      B = B->cp_b;
    if (Yap_HasException()) {
      port = CALLED_FROM_THROW;
    } else if (B->cp_b < B0) {
      port = CALLED_FROM_ANSWER;
    } else {
      port = CALLED_FROM_EXIT;
    }
  } else {
    if (Yap_HasException())
      port = CALLED_FROM_EXCEPTION;
    else
      port = CALLED_FROM_FAIL;
  }
  // store the correct CP, ENV can be recovered from last env.
  bool e = exit_set_call(port, B0, oCP, t3, t4 PASS_REGS);
  // ensure we have same P
  // also, we cannot trust recovered ENV and CP
  if (e) {
    P = oP;
    CP = oCP;
    ENV = LCL0 - oENV;
  }
  if (Yap_RaiseException()) {
    return false;
  }
  return rc;
}

static bool complete_ge(bool out, Term omod, yhandle_t sl, bool creeping) {
  CACHE_REGS
  if (creeping) {
    Yap_signal(YAP_CREEP_SIGNAL);
  }
  CurrentModule = omod;
  Yap_CloseSlots(sl);
  if (out) {
  }
  return out;
}

static Int _user_expand_goal(USES_REGS1) {
  yhandle_t sl = Yap_StartSlots();
  Int creeping = Yap_get_signal(YAP_CREEP_SIGNAL);
  PredEntry *pe;
  Term cmod = CurrentModule, omod = cmod;
  Term mg_args[2];
  Term g = Yap_YapStripModule(ARG1, &cmod);
  yhandle_t h1 = Yap_InitSlot(g), h2 = Yap_InitSlot(ARG2);

  /* CurMod:goal_expansion(A,B) */
  ARG1 = g;
  if ((pe = RepPredProp(Yap_GetPredPropByFunc(FunctorGoalExpansion2, cmod))) &&
      pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred != UNDEF_OPCODE &&
      Yap_execute_pred(pe, NULL, false PASS_REGS)) {
    return complete_ge(true, omod, sl, creeping);
  }
  /* system:goal_expansion(A,B) */
  mg_args[0] = cmod;
  mg_args[1] = Yap_GetFromSlot(h1);
  ARG1 = Yap_MkApplTerm(FunctorModule, 2, mg_args);
  ARG2 = Yap_GetFromSlot(h2);
  if ((pe = RepPredProp(
           Yap_GetPredPropByFunc(FunctorGoalExpansion2, SYSTEM_MODULE))) &&
      pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred != UNDEF_OPCODE &&
      Yap_execute_pred(pe, NULL, false PASS_REGS)) {
    return complete_ge(true, omod, sl, creeping);
  }
  ARG1 = Yap_GetFromSlot(h1);
  ARG2 = cmod;
  ARG3 = Yap_GetFromSlot(h2);
  /* user:goal_expansion(A,CurMod,B) */
  if ((pe = RepPredProp(
           Yap_GetPredPropByFunc(FunctorGoalExpansion, USER_MODULE))) &&
      pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred != UNDEF_OPCODE &&
      Yap_execute_pred(pe, NULL PASS_REGS, false)) {
    return complete_ge(true, omod, sl, creeping);
  }
  mg_args[0] = cmod;
  mg_args[1] = Yap_GetFromSlot(h1);
  ARG1 = Yap_MkApplTerm(FunctorModule, 2, mg_args);
  ARG2 = Yap_GetFromSlot(h2);
  /* user:goal_expansion(A,B) */
  if (cmod != USER_MODULE && /* we have tried this before */
      (pe = RepPredProp(
           Yap_GetPredPropByFunc(FunctorGoalExpansion2, USER_MODULE))) &&
      pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred != UNDEF_OPCODE &&
      Yap_execute_pred(pe, NULL PASS_REGS, false)) {
    return complete_ge(true, omod, sl, creeping);
  }
  return complete_ge(false, omod, sl, creeping);
}

static Int do_term_expansion(USES_REGS1) {
  yhandle_t sl = Yap_StartSlots();
  Int creeping = Yap_get_signal(YAP_CREEP_SIGNAL);
  PredEntry *pe;
  Term cmod = CurrentModule, omod = cmod;
  Term mg_args[2];
  Term g = Yap_YapStripModule(ARG1, &cmod);
  yhandle_t h1 = Yap_InitSlot(g), h2 = Yap_InitSlot(ARG2);
  /* user:term_expansion(A,B) */

  ARG1 = g;
  if ((pe = RepPredProp(
           Yap_GetPredPropByFunc(FunctorTermExpansion, USER_MODULE))) &&
      pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred != UNDEF_OPCODE &&
      Yap_execute_pred(pe, NULL, false PASS_REGS)) {
    return complete_ge(true, omod, sl, creeping);
  }
  /* CurMod:term_expansion(A,B) */
  ARG1 = g;
  if (cmod != USER_MODULE &&
      (pe = RepPredProp(Yap_GetPredPropByFunc(FunctorTermExpansion, cmod))) &&
      pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred != UNDEF_OPCODE &&
      Yap_execute_pred(pe, NULL, false PASS_REGS)) {
    return complete_ge(true, omod, sl, creeping);
  }
  /* system:term_expansion(A,B) */
  mg_args[0] = cmod;
  mg_args[1] = Yap_GetFromSlot(h1);
  ARG1 = Yap_MkApplTerm(FunctorModule, 2, mg_args);
  ARG2 = Yap_GetFromSlot(h2);
  if ((pe = RepPredProp(
           Yap_GetPredPropByFunc(FunctorTermExpansion, SYSTEM_MODULE))) &&
      pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred != UNDEF_OPCODE &&
      Yap_execute_pred(pe, NULL, false PASS_REGS)) {
    return complete_ge(true, omod, sl, creeping);
  }
  return complete_ge(false, omod, sl, creeping);
}

static Int execute0(USES_REGS1) { /* '$execute0'(Goal,Mod)	 */
  Term t = Deref(ARG1), t0 = t;
  Term mod = Deref(ARG2);
  unsigned int arity;
  Prop pe;

  if (Yap_has_a_signal() && !LOCAL_InterruptsDisabled) {
    return EnterCreepMode(t, mod PASS_REGS);
  }
restart_exec:
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, ARG3, "call/1");
    return false;
  } else if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    pe = PredPropByAtom(a, mod);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    register unsigned int i;
    register CELL *pt;

    if (IsExtensionFunctor(f))
      return FALSE;
    if (f == FunctorModule) {
      Term tmod = ArgOfTerm(1, t);
      if (!IsVarTerm(tmod) && IsAtomTerm(tmod)) {
        mod = tmod;
        t = ArgOfTerm(2, t);
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
    //    Yap_DebugPlWrite(mod);ffprintf(stderr, stderr,"\n");
    arity = ArityOfFunctor(f);
    if (arity > MaxTemps) {
      return CallError(TYPE_ERROR_CALLABLE, t, mod PASS_REGS);
    }
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t) + 1;
    for (i = 1; i <= arity; ++i) {
#if YAPOR_SBA
      Term d0 = *pt++;
      if (d0 == 0)
        XREGS[i] = (CELL)(pt - 1);
      else
        XREGS[i] = d0;
#else
      XREGS[i] = *pt++;
#endif
    }
  } else {
    Yap_Error(TYPE_ERROR_CALLABLE, t, "call/1");
    return false;
  }
  /*	N = arity; */
  /* call may not define new system predicates!! */
  return CallPredicate(RepPredProp(pe), B,
                       RepPredProp(pe)->CodeOfPred PASS_REGS);
}

static Int execute_nonstop(USES_REGS1) { /* '$execute_nonstop'(Goal,Mod)
                                          */
  Term t = Deref(ARG1), t0 = t;
  Term mod = Deref(ARG2);
  unsigned int arity;
  Prop pe;

  t = Yap_YapStripModule(t, &mod);
restart_exec:
  if (IsVarTerm(mod)) {
    mod = CurrentModule;
  } else if (!IsAtomTerm(mod)) {
    Yap_Error(TYPE_ERROR_ATOM, ARG2, "call/1");
    return FALSE;
  }
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, ARG1, "call/1");
    return FALSE;
  } else if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    pe = PredPropByAtom(a, mod);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    register unsigned int i;
    register CELL *pt;

    if (IsExtensionFunctor(f))
      return (FALSE);
    pe = PredPropByFunc(f, mod);
    arity = ArityOfFunctor(f);
    if (arity > MaxTemps) {
      return CallError(TYPE_ERROR_CALLABLE, t, mod PASS_REGS);
    }
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t) + 1;
    for (i = 1; i <= arity; ++i) {
#if YAPOR_SBA
      Term d0 = *pt++;
      if (d0 == 0)
        XREGS[i] = (CELL)(pt - 1);
      else
        XREGS[i] = d0;
#else
      XREGS[i] = *pt++;
#endif
    }
  } else {
    Yap_Error(TYPE_ERROR_CALLABLE, t, "call/1");
    return FALSE;
  }
  /*	N = arity; */
  /* call may not define new system predicates!! */
  if (RepPredProp(pe)->PredFlags & SpiedPredFlag) {
    if (!LOCAL_InterruptsDisabled && Yap_get_signal(YAP_CREEP_SIGNAL)) {
      Yap_signal(YAP_CREEP_SIGNAL);
    }
#if defined(YAPOR) || defined(THREADS)
    if (RepPredProp(pe)->PredFlags & LogUpdatePredFlag) {
      PP = RepPredProp(pe);
      PELOCK(80, PP);
    }
#endif
    return CallPredicate(RepPredProp(pe), B,
                         RepPredProp(pe)->cs.p_code.TrueCodeOfPred PASS_REGS);
  } else {
    if (Yap_get_signal(YAP_CREEP_SIGNAL) && !LOCAL_InterruptsDisabled &&
        (!(RepPredProp(pe)->PredFlags & (AsmPredFlag | CPredFlag)) ||
         RepPredProp(pe)->OpcodeOfPred == Yap_opcode(_call_bfunc_xx))) {
      Yap_signal(YAP_CREEP_SIGNAL);
    }
    return CallPredicate(RepPredProp(pe), B,
                         RepPredProp(pe)->CodeOfPred PASS_REGS);
  }
}

static Int execute_0(USES_REGS1) { /* '$execute_0'(Goal)	 */
  Term mod = CurrentModule;
  Term t = Yap_YapStripModule(Deref(ARG1), &mod);
  if (t == 0)
    return false;
  return do_execute(t, mod PASS_REGS);
}

static bool call_with_args(int i USES_REGS) {
  Term mod = CurrentModule, t;
  int j;

  t = Yap_YapStripModule(Deref(ARG1), &mod);
  if (t == 0)
    return false;
  for (j = 0; j < i; j++)
    heap_store(Deref(XREGS[j + 2]) PASS_REGS);
  return (do_execute_n(t, mod, i PASS_REGS));
}

static Int execute_1(USES_REGS1) { /* '$execute_0'(Goal)	 */
  return call_with_args(1 PASS_REGS);
}

static Int execute_2(USES_REGS1) { /* '$execute_2'(Goal)	 */
  return call_with_args(2 PASS_REGS);
}

static Int execute_3(USES_REGS1) { /* '$execute_3'(Goal)	 */
  return call_with_args(3 PASS_REGS);
}

static Int execute_4(USES_REGS1) { /* '$execute_4'(Goal)	 */
  return call_with_args(4 PASS_REGS);
}

static Int execute_5(USES_REGS1) { /* '$execute_5'(Goal)	 */
  return call_with_args(5 PASS_REGS);
}

static Int execute_6(USES_REGS1) { /* '$execute_6'(Goal)	 */
  return call_with_args(6 PASS_REGS);
}

static Int execute_7(USES_REGS1) { /* '$execute_7'(Goal)	 */
  return call_with_args(7 PASS_REGS);
}

static Int execute_8(USES_REGS1) { /* '$execute_8'(Goal)	 */
  return call_with_args(8 PASS_REGS);
}

static Int execute_9(USES_REGS1) { /* '$execute_9'(Goal)	 */
  return call_with_args(9 PASS_REGS);
}

static Int execute_10(USES_REGS1) { /* '$execute_10'(Goal)	 */
  return call_with_args(10 PASS_REGS);
}

#ifdef DEPTH_LIMIT
static Int execute_depth_limit(USES_REGS1) {
  Term d = Deref(ARG2);
  if (IsVarTerm(d)) {
    Yap_Error(INSTANTIATION_ERROR, d, "depth_bound_call/2");
    return false;
  } else if (!IsIntegerTerm(d)) {
    if (IsFloatTerm(d) && isinf(FloatOfTerm(d))) {
      DEPTH = RESET_DEPTH();
    } else {
      Yap_Error(TYPE_ERROR_INTEGER, d, "depth_bound_call/2");
      return false;
    }
  } else {
    DEPTH = MkIntTerm(IntegerOfTerm(d) * 2);
  }
  return execute(PASS_REGS1);
}
#endif

static bool exec_absmi(bool top, yap_reset_t reset_mode USES_REGS) {
  int lval, out;
  Int OldBorder = LOCAL_CBorder;
  LOCAL_CBorder = LCL0 - (CELL *)B;
  if (top && (lval = sigsetjmp(LOCAL_RestartEnv, 1)) != 0) {
    switch (lval) {
    case 1: { /* restart */
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
      /* forget any signals active, we're reborne */
      LOCAL_Signals = 0;
      CalculateStackGap(PASS_REGS1);
      LOCAL_PrologMode = UserMode;
      P = (yamop *)FAILCODE;
    } break;
    case 2: {
      /* arithmetic exception */
      /* must be done here, otherwise siglongjmp will clobber all the
       * registers
       */
      Yap_Error(LOCAL_matherror, TermNil, NULL);
      /* reset the registers so that we don't have trash in abstract
       * machine */
      Yap_set_fpu_exceptions(
          getAtomicGlobalPrologFlag(ARITHMETIC_EXCEPTIONS_FLAG));
      P = (yamop *)FAILCODE;
      LOCAL_PrologMode = UserMode;
    } break;
    case 3: { /* saved state */
      LOCAL_CBorder = OldBorder;
      return false;
    }
    default:
      /* do nothing */
      LOCAL_PrologMode = UserMode;
    }
  } else {
    LOCAL_PrologMode = UserMode;
  }
  YENV = ASP;
  YENV[E_CB] = Unsigned(B);
  out = Yap_absmi(0);
  /* make sure we don't leave a FAIL signal hanging around */
  Yap_get_signal(YAP_FAIL_SIGNAL);
  if (!Yap_has_a_signal())
    CalculateStackGap(PASS_REGS1);
  LOCAL_CBorder = OldBorder;
  return out;
}

void Yap_PrepGoal(UInt arity, CELL *pt, choiceptr saved_b USES_REGS) {
  /* create an initial pseudo environment so that when garbage
     collection is going up in the environment chain it doesn't get
     confused */
  Yap_ResetException(worker_id);
  //  sl = Yap_InitSlot(t);
  YENV = ASP;
  YENV[E_CP] = (CELL)YESCODE;
  YENV[E_CB] = (CELL)B;
  YENV[E_E] = (CELL)ENV;
#ifdef TABLING
  YENV[E_B] = (CELL)B;
#endif
#ifdef DEPTH_LIMIT
  YENV[E_DEPTH] = DEPTH;
#endif
  ENV = YENV;
  ASP -= EnvSizeInCells;
  /* and now create a pseudo choicepoint for much the same reasons */
  /* CP = YESCODE; */
  /* keep a place where you can inform you had an exception */
  if (pt) {
    int i;
    for (i = 0; i < arity; i++) {
      XREGS[i + 1] = *pt++;
    }
  }
  B = (choiceptr)ASP;
  B--;
  B->cp_h = HR;
  B->cp_tr = TR;
  B->cp_cp = CP;
  B->cp_ap = NOCODE;
  B->cp_env = ENV;
  B->cp_b = saved_b;
#ifdef DEPTH_LIMIT
  B->cp_depth = DEPTH;
#endif /* DEPTH_LIMIT */
  YENV = ASP = (CELL *)B;
  YENV[E_CB] = (CELL)B;
  HB = HR;
  CP = YESCODE;
}

static bool do_goal(yamop *CodeAdr, int arity, CELL *pt, bool top USES_REGS) {
  choiceptr saved_b = B;
  bool out;

  Yap_PrepGoal(arity, pt, saved_b PASS_REGS);
  P = (yamop *)CodeAdr;
  S = CellPtr(RepPredProp(
      PredPropByFunc(Yap_MkFunctor(AtomCall, 1), 0))); /* A1 mishaps */

  out = exec_absmi(top, YAP_EXEC_ABSMI PASS_REGS);
  if (top)
    Yap_flush();
  //  if (out) {
  //    out = Yap_GetFromSlot(sl);
  //  }
  //  Yap_RecoverSlots(1);
  LOCAL_PrologMode &= ~TopGoalMode;
  return out;
}

bool Yap_exec_absmi(bool top, yap_reset_t has_reset) {
  CACHE_REGS
  return exec_absmi(top, has_reset PASS_REGS);
}

/**
 * Fails computation up to choice-point bb
 * @method Yap_fail_all
 * @param  USES_REGS    [description]
 */
void Yap_fail_all(choiceptr bb USES_REGS) {
  yamop *saved_p, *saved_cp;

  saved_p = P;
  saved_cp = CP;
  /* prune away choicepoints */
  while (B->cp_b && B->cp_b != bb && B->cp_ap != NOCODE) {
    B = B->cp_b;
#ifdef YAPOR
    CUT_prune_to(B);
#endif
  }
  P = FAILCODE;
  exec_absmi(true, YAP_EXEC_ABSMI PASS_REGS);
  /* recover stack space */
  HR = B->cp_h;
  TR = B->cp_tr;
#ifdef DEPTH_LIMIT
  DEPTH = B->cp_depth;
#endif /* DEPTH_LIMIT */
  YENV = ENV = B->cp_env;
/* recover local stack */
#ifdef DEPTH_LIMIT
  DEPTH = ENV[E_DEPTH];
#endif
  /* make sure we prune C-choicepoints */
  if (POP_CHOICE_POINT(B->cp_b)) {
    POP_EXECUTE();
  }
  ENV = (CELL *)(ENV[E_E]);
  /* ASP should be set to the top of the local stack when we
     did the call */
  ASP = B->cp_env;
  /* YENV should be set to the current environment */
  YENV = ENV = (CELL *)((B->cp_env)[E_E]);
  if (B->cp_b) {
    B = B->cp_b;
  }
  // SET_BB(B);
  HB = PROTECT_FROZEN_H(B);
  CP = saved_cp;
  P = saved_p;
}

bool Yap_execute_pred( PredEntry *ppe, CELL *pt, bool pass_ex USES_REGS) {
  yamop *saved_p, *saved_cp;
  yamop *CodeAdr;
  bool out;

  saved_p = P;
  saved_cp = CP;
  LOCAL_PrologMode |= TopGoalMode;

  PELOCK(81, ppe);
  CodeAdr = ppe->CodeOfPred;
  UNLOCK(ppe->PELock);
  out = do_goal(CodeAdr, ppe->ArityOfPE, pt, false PASS_REGS);

  if (out) {
    choiceptr cut_B;
    /* we succeeded, let's prune */
    /* restore the old environment */
    /* get to previous environment */
    cut_B = (choiceptr)ENV[E_CB];
    {
      /* Note that
         cut_B == (choiceptr)ENV[E_CB] */
      while (POP_CHOICE_POINT(ENV[E_CB])) {
        POP_EXECUTE();
      }
    }
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
    CP = saved_cp;
    P = saved_p;
    ASP = ENV;
#ifdef DEPTH_LIMIT
    DEPTH = ENV[E_DEPTH];
#endif
    ENV = (CELL *)(ENV[E_E]);
    /* we have failed, and usually we would backtrack to this B,
       trouble is, we may also have a delayed cut to do */
    if (B != NULL)
      HB = B->cp_h;
    YENV = ENV;
    // should we catch the exception or pass it through?
    // We'll pass it through
    if (pass_ex && Yap_HasException()) {
      Yap_RaiseException();
      return false;
    }
    return true;
  } else if (out == 0) {
    P = saved_p;
    CP = saved_cp;
    HR = B->cp_h;
#ifdef DEPTH_LIMIT
    DEPTH = B->cp_depth;
#endif
    /* ASP should be set to the top of the local stack when we
       did the call */
    ASP = B->cp_env;
    /* YENV should be set to the current environment */
    YENV = ENV = (CELL *)((B->cp_env)[E_E]);
    B = B->cp_b;
    SET_BB(B);
    HB = PROTECT_FROZEN_H(B);
    // should we catch the exception or pass it through?
    // We'll pass it through
    if (pass_ex) {
      Yap_RaiseException();
    }
    return false;
  } else {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "emulator crashed");
    return false;
  }
}

bool Yap_execute_goal(Term t, int nargs, Term mod, bool pass_ex) {
  CACHE_REGS
  Prop pe;
  PredEntry *ppe;
  CELL *pt;
  /* preserve the current restart environment */
  /* visualc*/
  /* just keep the difference because of possible garbage collections
   */

  if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    pt = NULL;
    pe = PredPropByAtom(a, mod);
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);

    if (IsBlobFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call/1");
      return false;
    }
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t) + 1;
    pe = PredPropByFunc(f, mod);
  } else {
    Yap_Error(TYPE_ERROR_CALLABLE, t, "call/1");
    return false;
  }
  ppe = RepPredProp(pe);
  if (pe == NIL) {
    return CallMetaCall(t, mod PASS_REGS);
  }
  return Yap_execute_pred(ppe, pt, pass_ex PASS_REGS);
}

void Yap_trust_last(void) {
  CACHE_REGS
  ASP = B->cp_env;
  CP = B->cp_cp;
  HR = B->cp_h;
#ifdef DEPTH_LIMIT
  DEPTH = B->cp_depth;
#endif
  YENV = ASP = B->cp_env;
  ENV = (CELL *)((B->cp_env)[E_E]);
  B = B->cp_b;
  P = (yamop *)(ENV[E_CP]);
  if (B) {
    SET_BB(B);
    HB = PROTECT_FROZEN_H(B);
  }
}

Term Yap_RunTopGoal(Term t, bool handle_errors) {
  CACHE_REGS
  yamop *CodeAdr;
  Prop pe;
  PredEntry *ppe;
  CELL *pt;
  UInt arity;
  Term tmod = CurrentModule;
  Term goal_out = 0;
  LOCAL_PrologMode |= TopGoalMode;

  t = Yap_YapStripModule(t, &tmod);
restart_runtopgoal:
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "call/1");
    LOCAL_PrologMode &= ~TopGoalMode;
    return (FALSE);
  } else if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    pt = NULL;
    pe = Yap_GetPredPropByAtom(a, tmod);
    arity = 0;
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);

    if (IsBlobFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call/1");
      LOCAL_PrologMode &= ~TopGoalMode;
      return (FALSE);
    }
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pe = Yap_GetPredPropByFunc(f, tmod);
    pt = RepAppl(t) + 1;
    arity = ArityOfFunctor(f);
  } else {
    Yap_Error(TYPE_ERROR_CALLABLE, Yap_PredicateIndicator(t, tmod), "call/1");
    LOCAL_PrologMode &= ~TopGoalMode;
    return (FALSE);
  }
  ppe = RepPredProp(pe);
  if (pe == NIL || ppe->cs.p_code.TrueCodeOfPred->opc == UNDEF_OPCODE) {
    pe = AbsPredProp(ppe = UndefCode);
    pt = HR;
    HR[0] = MkPairTerm(tmod, t);
    HR[1] = MkAtomTerm(Yap_LookupAtom("top"));
    arity = 2;
    HR += 2;
  } else if (ppe->PredFlags & MetaPredFlag) {
    // we're in a meta-call, rake care about modules
    //
    Term ts[2];
    ts[0] = tmod;
    ts[1] = t;
    Functor f = Yap_MkFunctor(Yap_LookupAtom("call"), 1);

    pt = &t;
    t = Yap_MkApplTerm(FunctorModule, 2, ts);
    pe = Yap_GetPredPropByFunc(f, tmod);
    arity = 1;
  }
  PELOCK(82, ppe);
  CodeAdr = ppe->CodeOfPred;
  UNLOCK(ppe->PELock);

#if !USE_SYSTEM_MALLOC
  if (LOCAL_TrailTop - HeapTop < 2048) {
    Yap_Error(RESOURCE_ERROR_TRAIL, TermNil,
              "unable to boot because of too little Trail space");
  }
#endif
  goal_out = do_goal(CodeAdr, arity, pt, handle_errors PASS_REGS);
  return goal_out;
}

static void do_restore_regs(Term t, int restore_all USES_REGS) {
  if (IsApplTerm(t)) {
    Int i;
    Int max = ArityOfFunctor(FunctorOfTerm(t)) - 4;
    CELL *ptr = RepAppl(t) + 5;

    P = (yamop *)IntegerOfTerm(ptr[-4]);
    CP = (yamop *)IntegerOfTerm(ptr[-3]);
    ENV = (CELL *)(LCL0 - IntegerOfTerm(ptr[-2]));
    YENV = (CELL *)(LCL0 - IntegerOfTerm(ptr[-1]));
    for (i = 0; i < max; i += 2) {
      Int j = IntOfTerm(ptr[0]);
      XREGS[j] = ptr[1];
      ptr += 2;
    }
  }
}

/* low level voodoo to restore temporary registers after a call */
static Int restore_regs(USES_REGS1) {
  Term t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "support for coroutining");
    return (FALSE);
  }
  if (IsAtomTerm(t))
    return (TRUE);
  do_restore_regs(t, FALSE PASS_REGS);
  return (TRUE);
}

/* low level voodoo to cut and then restore temporary registers after
 * a
 * call */
static Int restore_regs2(USES_REGS1) {

  Term t = Deref(ARG1), d0;
  choiceptr pt0;
  Int d;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "support for coroutining");
    return (FALSE);
  }
  d0 = Deref(ARG2);
  if (!IsAtomTerm(t)) {
    do_restore_regs(t, TRUE PASS_REGS);
  }
  if (IsVarTerm(d0)) {
    Yap_Error(INSTANTIATION_ERROR, d0, "support for coroutining");
    return (FALSE);
  }
  if (!IsIntegerTerm(d0)) {
    return (FALSE);
  }
  d = IntegerOfTerm(d0);
  if (!d)
    return TRUE;
#if YAPOR_SBA
  pt0 = (choiceptr)d;
#else
  pt0 = (choiceptr)(LCL0 - d);
#endif
  /* find where to cut to */
  if ((CELL *)pt0 != LCL0 && pt0 > B) {
    /* Wow, we're gonna cut!!! */
    while (B->cp_b < pt0) {
      while (POP_CHOICE_POINT(B->cp_b)) {
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
  return (TRUE);
}

static Int clean_ifcp(USES_REGS1) {
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
    while (POP_CHOICE_POINT(B->cp_b)) {
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

static Int cut_up_to_next_disjunction(USES_REGS1) {
  choiceptr pt0 = B;
  CELL *qenv = (CELL *)ENV[E_E];

  while (pt0 && !(qenv == pt0->cp_env && disj_marker(pt0->cp_ap))) {
    pt0 = pt0->cp_b;
  }
  if (!pt0)
    return TRUE;
#ifdef YAPOR
  CUT_prune_to(pt0);
#endif /* YAPOR */
  /* find where to cut to */
  if (SHOULD_CUT_UP_TO(B, pt0)) {
    B = pt0;
#ifdef TABLING
    abolish_incomplete_subgoals(B);
#endif /* TABLING */
  }
  HB = B->cp_h;
  Yap_TrimTrail();
  return TRUE;
}

bool Yap_Reset(yap_reset_t mode) {
  CACHE_REGS
  int res = TRUE;

  Yap_ResetException(worker_id);
  /* first, backtrack to the root */
  while (B->cp_b) {
    B = B->cp_b;
  }
  // B shoul lead to CP with _ystop0,
  P = FAILCODE;
  res = Yap_exec_absmi(true, mode);
  /* reinitialize the engine */
  //  Yap_InitYaamRegs( worker_id );
  GLOBAL_Initialised = true;
  ENV = LCL0;
  ASP = (CELL *)B;
  /* the first real choice-point will also have AP=FAIL */
  /* always have an empty slots for people to use */
  P = CP = YESCODE;
  // ensure that we have slots where we need them
  Yap_RebootSlots(worker_id);
  return res;
}

bool is_cleanup_cp(choiceptr cp_b) {
  PredEntry *pe;

  if (cp_b->cp_ap->opc != ORLAST_OPCODE)
    return FALSE;
#ifdef YAPOR
  pe = cp_b->cp_ap->y_u.Osblp.p0;
#else
  pe = cp_b->cp_ap->y_u.p.p;
#endif /* YAPOR */
       /*
         it has to be a cleanup and it has to be a completed goal,
         otherwise the throw will be caught anyway.
       */
  return pe == PredSafeCallCleanup;
}

static Int JumpToEnv() {
  choiceptr handler = B, oh = NULL;
  /* just keep the throwm object away, we don't need to care about it
   */
  /* careful, previous step may have caused a stack shift,
     so get pointers here     */
  /* find the first choicepoint that may be a catch */
  // DBTerm *dbt = Yap_RefToException();
  while (handler && Yap_PredForChoicePt(handler, NULL) != PredDollarCatch) {
    while (POP_CHOICE_POINT(handler)) {
      POP_FAIL_EXECUTE(handler);
    }
    /* we are already doing a catch */
    /* make sure we prune C-choicepoints */
    if (handler->cp_ap == NOCODE &&
        (handler >= (choiceptr)(LCL0 - LOCAL_CBorder) ||
         handler->cp_b == NULL)) {
      break;
    }
    oh = handler;
    handler = handler->cp_b;
  }
  if (LOCAL_PrologMode & AsyncIntMode) {
    Yap_signal(YAP_FAIL_SIGNAL);
  }
  POP_FAIL(handler);
  B = handler;
  // Yap_CopyException(ref);
  if (Yap_PredForChoicePt(B, NULL) == PredDollarCatch) {
    /* can recover Heap thanks to copy term :-( */
    /* B->cp_h = H; */
    /* I could backtrack here, but it is easier to leave the unwinding
       to the emulator */
    // handler->cp_h = HR;
    /* try to recover space */
    /* can only do that when we recover space */
    /* first, backtrack */
    /* so that I recover memory execute op_fail */
    // now put the ball in place
    // Yap_CopyException(dbt);
    Term t = Yap_GetException();
    if (t == 0) {
      return false;
    } else if (IsVarTerm(t)) {
      t = Yap_MkApplTerm(FunctorGVar, 1, &t);
    }
    B->cp_h = HR;
    HB = HR;
    Yap_unify(t, B->cp_a2);
    B->cp_tr = TR;
  }
  P = FAILCODE;
  return true;
}

bool Yap_JumpToEnv(Term t) {
  CACHE_REGS
  LOCAL_BallTerm = Yap_StoreTermInDB(t, 0);
  if (!LOCAL_BallTerm)
    return false;
  if (LOCAL_PrologMode & TopGoalMode)
    return true;
  return JumpToEnv(PASS_REGS);
}

/* This does very nasty stuff!!!!! */
static Int jump_env(USES_REGS1) {
  Term t = Deref(ARG1);
  Yap_PutException(t);
  bool out = JumpToEnv(PASS_REGS1);
  if (B != NULL && P == FAILCODE && B->cp_ap == NOCODE &&
      LCL0 - (CELL *)B > LOCAL_CBorder) {
    // we're failing up to the top layer
    LOCAL_Error_TYPE = THROW_EVENT;
  }
  return out;
}

/* set up a meta-call based on . context info */
static Int generate_pred_info(USES_REGS1) {
  ARG1 = ARG3 = ENV[-EnvSizeInCells - 1];
  ARG4 = ENV[-EnvSizeInCells - 3];
  ARG2 = cp_as_integer((choiceptr)ENV[E_CB] PASS_REGS);
  return TRUE;
}

void Yap_InitYaamRegs(int myworker_id) {
  Term h0var;
//  getchar();
#if PUSH_REGS
/* Guarantee that after a longjmp we go back to the original abstract
   machine registers */
#ifdef THREADS
  if (myworker_id) {
    REGSTORE *rs = REMOTE_ThreadHandle(myworker_id).default_yaam_regs;
    pthread_setspecific(Yap_yaamregs_key, (const void *)rs);
    REMOTE_ThreadHandle(myworker_id).current_yaam_regs = rs;
  }
/* may be run by worker_id on behalf on myworker_id */
#else
  Yap_regp = &Yap_standard_regs;
#endif
#endif /* PUSH_REGS */
  CACHE_REGS
  Yap_ResetException(worker_id);
  Yap_PutValue(AtomBreak, MkIntTerm(0));
  TR = (tr_fr_ptr)REMOTE_TrailBase(myworker_id);
  HR = H0 = ((CELL *)REMOTE_GlobalBase(myworker_id)) +
            1; // +1: hack to ensure the gc does not try to mark mistakenly
  LCL0 = ASP = (CELL *)REMOTE_LocalBase(myworker_id);
  CurrentTrailTop = (tr_fr_ptr)(REMOTE_TrailTop(myworker_id) - MinTrailGap);
  /* notice that an initial choice-point and environment
   *must* be created  for the garbage collector to work */
  B = NULL;
  ENV = NULL;
  P = CP = YESCODE;
#ifdef DEPTH_LIMIT
  DEPTH = RESET_DEPTH();
#endif
  STATIC_PREDICATES_MARKED = FALSE;
  if (REMOTE_GlobalArena(myworker_id) == 0L ||
      REMOTE_GlobalArena(myworker_id) == TermNil) {
  } else {
    HR = RepAppl(REMOTE_GlobalArena(myworker_id));
  }
  REMOTE_GlobalArena(myworker_id) = TermNil;
  Yap_InitPreAllocCodeSpace(myworker_id);
#ifdef FROZEN_STACKS
  H_FZ = HR;
#ifdef YAPOR_SBA
  BSEG =
#endif /* YAPOR_SBA */
      BBREG = B_FZ = (choiceptr)REMOTE_LocalBase(myworker_id);
  TR = TR_FZ = (tr_fr_ptr)REMOTE_TrailBase(myworker_id);
#endif /* FROZEN_STACKS */
  CalculateStackGap(PASS_REGS1);
/* the first real choice-point will also have AP=FAIL */
/* always have an empty slots for people to use */
#if defined(YAPOR) || defined(THREADS)
  LOCAL = REMOTE(myworker_id);
  worker_id = myworker_id;
#endif /* THREADS */
#if COROUTINING
  REMOTE_WokenGoals(myworker_id) = Yap_NewTimedVar(TermNil);
  h0var = MkVarTerm();
  REMOTE_AttsMutableList(myworker_id) = Yap_NewTimedVar(h0var);
#endif
  Yap_RebootSlots(myworker_id);
  h0var = MkVarTerm();
  REMOTE_GcGeneration(myworker_id) = Yap_NewTimedVar(h0var);
  REMOTE_GcCurrentPhase(myworker_id) = 0L;
  REMOTE_GcPhase(myworker_id) =
      Yap_NewTimedVar(MkIntTerm(REMOTE_GcCurrentPhase(myworker_id)));
#if defined(YAPOR) || defined(THREADS)
  PP = NULL;
  PREG_ADDR = NULL;
#endif
  Yap_AllocateDefaultArena(128 * 1024, 2, myworker_id);
  cut_c_initialize(myworker_id);
  Yap_PrepGoal(0, NULL, NULL PASS_REGS);
#ifdef FROZEN_STACKS
  H_FZ = HR;
#ifdef YAPOR_SBA
  BSEG =
#endif /* YAPOR_SBA */
      BBREG = B_FZ = (choiceptr)REMOTE_LocalBase(myworker_id);
  TR = TR_FZ = (tr_fr_ptr)REMOTE_TrailBase(myworker_id);
#endif /* FROZEN_STACKS */
  CalculateStackGap(PASS_REGS1);
#ifdef TABLING
  /* ensure that LOCAL_top_dep_fr is always valid */
  if (REMOTE_top_dep_fr(myworker_id))
    DepFr_cons_cp(REMOTE_top_dep_fr(myworker_id)) = NORM_CP(B);
#endif
}

Term Yap_GetException(void) {
  CACHE_REGS
  Term t = 0;

  if (LOCAL_BallTerm) {
    t = Yap_PopTermFromDB(LOCAL_BallTerm);
  }
  LOCAL_BallTerm = NULL;
  return t;
}

Term Yap_PeekException(void) { return Yap_FetchTermFromDB(LOCAL_BallTerm); }

bool Yap_RaiseException(void) {
  if (LOCAL_BallTerm == NULL)
    return false;
  return JumpToEnv();
}

bool Yap_PutException(Term t) {
  CACHE_REGS
  if ((LOCAL_BallTerm = Yap_StoreTermInDB(t, 0)) != NULL)
    return true;

  return false;
}

bool Yap_ResetException(int wid) {
  if (REMOTE_BallTerm(wid)) {
    Yap_PopTermFromDB(REMOTE_BallTerm(wid));
  }
  REMOTE_BallTerm(wid) = NULL;
  return true;
}

static Int reset_exception(USES_REGS1) { return Yap_ResetException(worker_id); }

static Int get_exception(USES_REGS1) {
  Term t = Yap_GetException();
  if (t == 0)
    return false;
  return Yap_unify(t, ARG1);
}

int Yap_dogc(int extra_args, Term *tp USES_REGS) {
  UInt arity;
  yamop *nextpc;
  int i;

  if (P && PREVOP(P, Osbpp)->opc == Yap_opcode(_call_usercpred)) {
    arity = PREVOP(P, Osbpp)->y_u.Osbpp.p->ArityOfPE;
    nextpc = P;
  } else {
    arity = 0;
    nextpc = CP;
  }
  for (i = 0; i < extra_args; i++) {
    XREGS[arity + i + 1] = tp[i];
  }
  if (!Yap_gc(arity + extra_args, ENV, nextpc)) {
    return FALSE;
  }
  for (i = 0; i < extra_args; i++) {
    tp[i] = XREGS[arity + i + 1];
  }
  return TRUE;
}

void Yap_InitExecFs(void) {
  CACHE_REGS
  Term cm = CurrentModule;
  Yap_InitComma();
  Yap_InitCPred("$execute", 1, execute, 0);
  Yap_InitCPred("$execute", 2, execute2, 0);
  Yap_InitCPred("$execute", 3, execute3, 0);
  Yap_InitCPred("$execute", 4, execute4, 0);
  Yap_InitCPred("$execute", 5, execute5, 0);
  Yap_InitCPred("$execute", 6, execute6, 0);
  Yap_InitCPred("$execute", 7, execute7, 0);
  Yap_InitCPred("$execute", 8, execute8, 0);
  Yap_InitCPred("$execute", 9, execute9, 0);
  Yap_InitCPred("$execute", 10, execute10, 0);
  Yap_InitCPred("$execute", 11, execute11, 0);
  Yap_InitCPred("$execute", 12, execute12, 0);
  Yap_InitCPred("$execute_in_mod", 2, execute_in_mod, 0);
  Yap_InitCPred("$execute_wo_mod", 2, execute_in_mod, 0);
  Yap_InitCPred("call_with_args", 1, execute_0, 0);
  Yap_InitCPred("call_with_args", 2, execute_1, 0);
  Yap_InitCPred("call_with_args", 3, execute_2, 0);
  Yap_InitCPred("call_with_args", 4, execute_3, 0);
  Yap_InitCPred("call_with_args", 5, execute_4, 0);
  Yap_InitCPred("call_with_args", 6, execute_5, 0);
  Yap_InitCPred("call_with_args", 7, execute_6, 0);
  Yap_InitCPred("call_with_args", 8, execute_7, 0);
  Yap_InitCPred("call_with_args", 9, execute_8, 0);
  Yap_InitCPred("call_with_args", 10, execute_9, 0);
  Yap_InitCPred("call_with_args", 11, execute_10, 0);
#ifdef DEPTH_LIMIT
  Yap_InitCPred("$execute_under_depth_limit", 2, execute_depth_limit, 0);
#endif
  Yap_InitCPred("$execute0", 2, execute0, NoTracePredFlag);
  Yap_InitCPred("$execute_nonstop", 2, execute_nonstop, NoTracePredFlag);
  Yap_InitCPred("$execute_clause", 4, execute_clause, NoTracePredFlag);
  Yap_InitCPred("$current_choice_point", 1, current_choice_point, 0);
  Yap_InitCPred("$current_choicepoint", 1, current_choice_point, 0);
  CurrentModule = HACKS_MODULE;
  Yap_InitCPred("current_choice_point", 1, current_choice_point, 0);
  Yap_InitCPred("current_choicepoint", 1, current_choice_point, 0);
  Yap_InitCPred("env_choice_point", 1, save_env_b, 0);
  Yap_InitCPred("cut_at", 1, clean_ifcp, SafePredFlag);
  CurrentModule = cm;
  Yap_InitCPred("$restore_regs", 1, restore_regs,
                NoTracePredFlag | SafePredFlag);
  Yap_InitCPred("$restore_regs", 2, restore_regs2,
                NoTracePredFlag | SafePredFlag);
  Yap_InitCPred("$clean_ifcp", 1, clean_ifcp, SafePredFlag);
  Yap_InitCPred("qpack_clean_up_to_disjunction", 0, cut_up_to_next_disjunction,
                SafePredFlag);
  Yap_InitCPred("$jump_env_and_store_ball", 1, jump_env, 0);
  Yap_InitCPred("$generate_pred_info", 4, generate_pred_info, 0);
  Yap_InitCPred("$reset_exception", 1, reset_exception, 0);
  Yap_InitCPred("_user_expand_goal", 2, _user_expand_goal, 0);
  Yap_InitCPred("$do_term_expansion", 2, do_term_expansion, 0);
  Yap_InitCPred("$get_exception", 1, get_exception, 0);
  Yap_InitCPred("setup_call_catcher_cleanup", 4, setup_call_catcher_cleanup, 0);
  Yap_InitCPredBackCut("$protect_stack", 4, 0, protect_stack,
                       protect_stack_from_retry, protect_stack_from_cut, 0);
}
