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

/**
 * @file   exec.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
 * @date   Mon Apr 30 13:48:35 2018
 *
 * @brief  meta-call and related
 *
 * @namespace prolog
 *
 *
 *
 */

#include "absmi.h"
#include "attvar.h"
#include "cut_c.h"
#include "yapio.h"
#include "heapgc.h"

static bool CallPredicate(PredEntry *, choiceptr, yamop *CACHE_TYPE);
static Int execute_nonstop(PASS_REGS1);
static Int creep_clause(PASS_REGS1);

// must hold thread worker comm lock at call.
static bool EnterCreepMode(Term, Term CACHE_TYPE);

static Int current_choice_point(USES_REGS1);

static Int execute(USES_REGS1);

static Int execute0(USES_REGS1);

static bool should_creep() {
    return
    !(LOCAL_PrologMode & (AbortMode | InterruptMode | SystemMode|BootMode))
    &&
    LOCAL_debugger_state[DEBUG_DEBUG] == TermTrue
    &&
      trueLocalPrologFlag(DEBUG_FLAG)
    &&
    (
            (Yap_has_a_signal() && !LOCAL_InterruptsDisabled) ||
            LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP] == TermUserCreep ||
            LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP] == TermLeap);

}

static Term cp_as_integer(choiceptr cp USES_REGS) {
    return (MkIntegerTerm(LCL0 - (CELL *) cp));
}

static choiceptr cp_from_integer(Term cpt USES_REGS) {
    return (choiceptr) (LCL0 - IntegerOfTerm(cpt));
}

/**
 * Represents a choice-point as an offset to the top of local stack. This should
 * *be stable acroos gc or stack shifts.
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
    YENV[E_CB] = (CELL) cut_pt;
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

    if (IsVarTerm(t))
        Yap_ThrowError(INSTANTIATION_ERROR, t, "meta-call");
    if (IsIntTerm(t) || (IsApplTerm(t) && IsExtensionFunctor(FunctorOfTerm(t))))
        Yap_ThrowError(TYPE_ERROR_CALLABLE, Yap_TermToIndicator(t, mod), "meta-call");
    Term tmod;
    if (mod) {
        tmod = mod;
    } else {
        tmod = TermProlog;
    }
    Functor f = FunctorOfTerm(t);
     if (f == FunctorExecuteNonStop) {
        tmod = ArgOfTerm(2,t);
        t = ArgOfTerm(1,t);
        PredEntry *pe = Yap_get_pred(t, tmod, "$execute_nonstop");
        CELL *pt = RepAppl(t) + 1;
        arity_t i;
        for (i = 1; i <= pe->ArityOfPE; i++) {
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
        return CallPredicate(pe,B, pe->CodeOfPred PASS_REGS);
     } else if (f==FunctorCreepClause) {
         return creep_clause(PASS_REGS1);
     } else {
        ARG1 = t;
        ARG2 = cp_as_integer(B PASS_REGS); /* p_current_choice_point */
        ARG3 = t;
        ARG4 = tmod;
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
 *
 * @param  g                     goal
 * @param  mod                     curre1nt module
 * @return                         su
 */
Term Yap_ExecuteCallMetaCall(Term g, Term mod) {
    CACHE_REGS
    Term ts[4];
    if (IsVarTerm(g))
        Yap_ThrowError(INSTANTIATION_ERROR, g, "meta-call");
    if (IsIntTerm(g) || (IsApplTerm(g) && IsExtensionFunctor(FunctorOfTerm(g))))
        Yap_ThrowError(TYPE_ERROR_CALLABLE, Yap_TermToIndicator(g, mod), "meta-call");
    ts[0] = g;
    ts[1] = cp_as_integer(B PASS_REGS); /* p_current_choice_point */
    ts[2] = g;
    ts[3] = mod;
    if (Yap_GetGlobal(AtomDebugMeta) == TermOn) {
        return Yap_MkApplTerm(PredTraceMetaCall->FunctorOfPred, 3, ts);
    }
    return Yap_MkApplTerm(PredMetaCall->FunctorOfPred, 4, ts);
}

Term Yap_TermToIndicator(Term t, Term mod) {
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
        return t;
    }
    t = Yap_MkApplTerm(FunctorSlash, 2, ti);
    if (mod != PROLOG_MODULE && mod != USER_MODULE && mod != TermProlog) {
        ti[0] = mod;
        ti[1] = t;
        return Yap_MkApplTerm(FunctorModule, 2, ti);
    }
    return t;
}

Term Yap_PredicateToIndicator(PredEntry *pe) {
    CACHE_REGS
    // generate predicate indicator in this case
    Term ti[2];
    if (pe->ArityOfPE) {
        ti[0] = MkAtomTerm(NameOfFunctor(pe->FunctorOfPred));
        ti[1] = MkIntegerTerm(ArityOfFunctor(pe->FunctorOfPred));
    } else {
        ti[0] = MkAtomTerm((Atom) (pe->FunctorOfPred));
        ti[1] = MkIntTerm(0);
    }
    Term t = Yap_MkApplTerm(FunctorSlash, 2, ti);
    Term mod = pe->ModuleOfPred;
    if (mod != PROLOG_MODULE && mod != USER_MODULE && mod != TermProlog) {
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
        Yap_ThrowError(err, t, "call/1");
        return false;
    }
}

/** @pred current_choice_point( -CP )
 *
 * unify the logic variable _CP_ with a number that identifies the
 * last alternative taken, or current choice-point. This number is
 * only valid as long as we do not backtrack by or cut _CP_, and is
 * safe in the presence of stack shifting and/or garbage collection.
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
    YapBind((CELL *) t, td);
    return TRUE;
}

/** @pred parent_choice_point( +CP, -PCP )
 *
 * given that _CP_  identifies an
 *  alternative taken, or  choice-point, _PCP_ identifies its parent.
 *
 * The call will fail if _CP_ is topmost in the search tree. 
 */
static Int parent_choice_point(USES_REGS1) {
    Term t = Deref(ARG1);
    Term td;
#if SHADOW_HB
    register CELL *HBREG = HB;
#endif
    if (!IsVarTerm(t)) {
        Yap_ThrowError(INSTANTIATION_ERROR, t, "child choicr-point missing");
    }
    choiceptr cp = cp_from_integer(t);
    if (cp == NULL || cp->cp_b == NULL)
        return false;
    td = cp_as_integer(cp->cp_b PASS_REGS);
    YapBind((CELL *) t, td);
    return TRUE;
}

/** @pred parent_choice_point( -PB )
 *
 *  PB is a number identifying the parent of the current choice-point.
 *  It storing the offset of the current ch
	      arity = 0;
 4314 	      nextpc = CP;
 4315  *
 * The call will fail if _CP_ is topmost in the search tree. 
 */
static Int parent_choice_point1(USES_REGS1) {
    Term t = Deref(ARG1);
    Term td;
#if SHADOW_HB
    register CELL *HBREG = HB;
#endif
    if (B == NULL || B->cp_b == NULL)
        return false;
    td = cp_as_integer(B->cp_b PASS_REGS);
    YapBind((CELL *) t, td);
    return true;
}


static Int save_env_b(USES_REGS1) {
    Term t = Deref(ARG1);
    Term td;
#if SHADOW_HB
    register CELL *HBREG = HB;
#endif
    if (!IsVarTerm(t))
        return (FALSE);
    td = cp_as_integer((choiceptr) YENV[E_CB] PASS_REGS);
    YapBind((CELL *) t, td);
    return true;
}

/** Look for a predicate with same functor as t,
    create a new one of it cannot find it.
*/
static PredEntry *new_pred(Term t, Term tmod, char *pname) {
    Term t0 = t;

    restart:
    if (IsVarTerm(t)) {
        Yap_ThrowError(INSTANTIATION_ERROR, t0, pname);
        return NULL;
    } else if (IsAtomTerm(t)) {
        return RepPredProp(PredPropByAtom(AtomOfTerm(t), tmod));
    } else if (IsIntegerTerm(t) && tmod == IDB_MODULE) {
        return Yap_FindLUIntKey(IntegerOfTerm(t));
    } else if (IsApplTerm(t)) {
        Functor fun = FunctorOfTerm(t);
        if (IsExtensionFunctor(fun)) {
            Yap_ThrowError(TYPE_ERROR_CALLABLE, Yap_TermToIndicator(t, tmod), pname);
            return NULL;
        }
        if (fun == FunctorModule) {
            Term tmod = ArgOfTerm(1, t);
            if (IsVarTerm(tmod)) {
                Yap_ThrowError(INSTANTIATION_ERROR, t0, pname);
                return NULL;
            }
            if (!IsAtomTerm(tmod)) {
                Yap_ThrowError(TYPE_ERROR_ATOM, t0, pname);
                return NULL;
            }
            t = ArgOfTerm(2, t);
            goto restart;
        }
        return RepPredProp(PredPropByFunc(fun, tmod));
    } else
        return NULL;
}

static bool CommaCall(Term t, Term mod) {
    PredEntry *pen;
    arity_t i;
    if (IsVarTerm(t) || (pen = new_pred(t, mod, "_,_")))
        return false;
    for (i = 0; i < pen->ArityOfPE; i++) {
        YENV[-EnvSizeInCells - i] = XREGS[i + 1];
    }
    YENV[E_CB] = (CELL) B;
    YENV[E_CP] = (CELL) P;
    YENV[E_E] = (CELL) ENV;
    YENV[E_DEPTH] = DEPTH;

    ASP = YENV - (EnvSizeInCells + i);
    ENV = YENV;
    YENV = ASP;
    if ((P = pen->MetaEntryOfPred) == NULL) {
        P = Yap_InitCommaContinuation(pen);
    }
    return P == NULL;
}

inline static bool do_execute(Term t, Term mod USES_REGS) {
    Term t0 = t, mod0 = mod;
    t = Yap_YapStripModule(t, &mod);
    /* first do predicate expansion, even before you process signals.
       This way you don't get to spy goal_expansion(). */
    if (   should_creep() ) {
        return EnterCreepMode(t, mod PASS_REGS);
    }
    if (IsVarTerm(t) || IsVarTerm(mod)) {
        return CallError(INSTANTIATION_ERROR, t0, mod0 PASS_REGS);
    }
    if (IsApplTerm(t)) {
        register Functor f = FunctorOfTerm(t);
        register CELL *pt;
        PredEntry *pen;
        unsigned int i, arity;

        f = FunctorOfTerm(t);
        if (f == FunctorComma && false) {
            Term t2 = ArgOfTerm(2, t);
            if (IsVarTerm(t2))
                return CallMetaCall(t, mod PASS_REGS);
            if (1 || !CommaCall(t2, mod))
                return CallMetaCall(t, mod PASS_REGS);
            Term t1 = ArgOfTerm(1, t);

            t = t1;
            pen = new_pred(t, mod, "_,_");
            if (pen == NULL || (arity = pen->ArityOfPE) == 0) {
                return do_execute(t, mod);
            }
        } else if (IsExtensionFunctor(f)) {
            return CallError(TYPE_ERROR_CALLABLE, t0, mod0 PASS_REGS);
        }
        arity = ArityOfFunctor(f);
        if (arity > MaxTemps) {
            return CallError(TYPE_ERROR_CALLABLE, t0, mod0 PASS_REGS);
        }
        pen = RepPredProp(PredPropByFunc(f, mod));
        /* You thought we would be over by now */
        /* but no meta calls require special preprocessing */
        /* now let us do what we wanted to do from the beginning !! */
        /* I cannot use the standard macro here because
           otherwise I would dereference the argument and
           might skip a svar */
        if (pen->PredFlags & (MetaPredFlag | UndefPredFlag | SpiedPredFlag)) {
            return CallMetaCall(t, mod PASS_REGS);
        }
        pt = RepAppl(t) + 1;
        for (i = 1; i <= arity; i++) {
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
        return CallPredicate(pen, B, pen->CodeOfPred PASS_REGS);
    }
    if (IsAtomTerm(t)) {
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
    }
    return CallMetaCall(t, mod PASS_REGS);
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
            *HR++ = h0[(int) (i - n)];
        }
        tf = AbsPair(h0);
    } else {
        *HR++ = (CELL) f;
        for (i = 0; i < arity - n; i++) {
            *HR++ = pt[i];
        }
        for (i = 0; i < n; i++) {
            *HR++ = h0[(int) (i - n)];
        }
        tf = AbsAppl(h0);
    }
    if (mod != CurrentModule) {
        CELL *h0 = HR;
        *HR++ = (CELL) FunctorModule;
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
    Term t0 = t, mod0 = mod;
t = Yap_YapStripModule(t, &mod);
    restart_exec:
    if (IsVarTerm(t)) {
        return CallError(INSTANTIATION_ERROR, t0, mod0 PASS_REGS);
    } else if (IsAtomTerm(t)) {
        arity = n;
        Name = AtomOfTerm(t);
        pt = NULL;
    } else if (IsIntTerm(t)) {
        return CallError(TYPE_ERROR_CALLABLE, t, mod0 PASS_REGS);
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
                    return CallError(INSTANTIATION_ERROR, t0, mod0 PASS_REGS);
                } else {
                    return CallError(TYPE_ERROR_ATOM, t0, mod0 PASS_REGS);
                }
            }
        }
        arity = ArityOfFunctor(f) + n;
        Name = NameOfFunctor(f);
        pt = RepAppl(t) + 1;
    }
    f = Yap_MkFunctor(Name, arity);
    if (IsExtensionFunctor(f)) {
        return CallError(TYPE_ERROR_CALLABLE, t0, mod0 PASS_REGS);
    }
    if (Yap_has_a_signal() && !LOCAL_InterruptsDisabled) {
        return EnterCreepMode(
                copy_execn_to_heap(f, pt, n, arity, CurrentModule PASS_REGS),
                mod PASS_REGS);
    }
    if (arity > MaxTemps) {
        return CallError(TYPE_ERROR_CALLABLE, t0, mod0 PASS_REGS);
    }
    pen = RepPredProp(PredPropByFunc(f, mod));
    /* You thought we would be over by now */
    /* but no meta calls require special preprocessing */
    //  if (pen->PredFlags & (MetaPredFlag | UndefPredFlag)) {
    // Term t = copy_execn_to_heap(f, pt, n, arity, mod PASS_REGS);
    // return (CallMetaCall(t0, mod0 PASS_REGS));
    //}
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
        if (!Yap_locked_growheap(false, 0, NULL)) {
            Yap_ThrowError(RESOURCE_ERROR_HEAP, TermNil,
                           "YAP failed to grow heap at meta-call");
        }
        if (!Yap_has_a_signal()) {
            return do_execute(ARG1, mod PASS_REGS);
        }
    }
    PredCreep = Yap_get_pred(Yap_MkNewApplTerm(FunctorCreep, 1), CurrentModule, "creep" );
    PP = PredCreep;
    if (IsVarTerm(t)) { return false; }
        ARG2 = mod;
        ARG1 = t;
ARG3 = MkVarTerm();
ARG4 = MkVarTerm();
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
            Bind_Local(VarOfTerm(t), (CELL) HR);
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
        Yap_ThrowError(INSTANTIATION_ERROR, ARG3, "call/1");
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

static Int creep_clause(USES_REGS1) { /* '$execute_clause'(Goal)	 */
  LOCAL_debugger_state[DEBUG_DEBUG] = TermTrue;
  if (!LOCAL_InterruptsDisabled) {
        Yap_signal(YAP_CREEP_SIGNAL);
    }
  Int rc = execute_clause(PASS_REGS1);
    return rc;
}

static Int execute_in_mod(USES_REGS1) { /* '$execute'(Goal)	 */
    return do_execute(Deref(ARG1), Deref(ARG2) PASS_REGS);
}

/**
 * remove choice points created since a call to top-goal.
 *
 * @method prune_inner_computation
 */
static void prune_inner_computation(choiceptr parent) {
    /* code */
    choiceptr cut_pt;

    cut_pt = B;
    while (B && B->cp_b <= parent) {
        B = B->cp_b;
    }
    if (!B)
        return;
#ifdef YAPOR
        CUT_prune_to(B);
#endif
    Yap_TrimTrail();
    LOCAL_AllowRestart = FALSE;
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
    } else if (myB->cp_b && myB->cp_b <= old_B) {
        while (myB->cp_b <= old_B) {
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

static Int Yap_ignore(Term t, bool fail USES_REGS) {
    yamop *oP = P, *oCP = CP;
    Int oENV = LCL0 - ENV;
    Int oYENV = LCL0 - YENV;
    Int oB = LCL0 - (CELL *) B;
    yap_error_descriptor_t *ctx = calloc(1,sizeof(yap_error_descriptor_t));
    bool newxp = Yap_pushErrorContext(true, ctx);
    ARG1 = t;
    PredEntry *pe = Yap_get_pred(Yap_MkApplTerm(FunctorCall,1,&t),TermProlog,"ïgnore");
    bool rc = Yap_execute_pred(pe, NULL, false);
    if (!rc) {
        complete_inner_computation((choiceptr) (LCL0 - oB));
        // We'll pass it through
    } else {
        prune_inner_computation((choiceptr) (LCL0 - oB));
    }
    Yap_popErrorContext(newxp, true);
    P = oP;
    CP = oCP;
    ENV = LCL0 - oENV;
    YENV = LCL0 - oYENV;
    B = (choiceptr) (LCL0 - oB);
    free(ctx);
    return true;
}

extern void *Yap_blob_info(Term t);

static bool set_watch(Int Bv, Term task) {
    CELL *pt;
    Term t = Yap_AllocExternalDataInStack((CELL) setup_call_catcher_cleanup_tag,
                                          sizeof(Int), &pt);
    if (t == TermNil)
        return false;
    *pt = Bv;
    *HR++ = t;
    *HR++ = task;
    TrailTerm(TR) = AbsPair(HR - 2);
    TR++;
    return true;
}

static bool watch_cut(Term ext USES_REGS) {
    // called after backtracking..
    //
    Term task = TailOfTerm(ext);
    Term cleanup = ArgOfTerm(3, task);
    Term e = 0;
    bool complete = IsNonVarTerm(Deref(ArgOfTerm(4, task)));
    bool active = ArgOfTerm(5, task) == TermTrue;
    bool ex_mode = false;

    if (complete) {
        return true;
    }
    // try to execute signals on the main loop.
    LOCAL_Signals = 0;
    CalculateStackGap(PASS_REGS1);
    LOCAL_PrologMode = UserMode;

    CELL *port_pt = pDerefa(RepAppl(task) + 2);
    CELL *completion_pt = pDerefa(RepAppl(task) + 4);

    if (LOCAL_ActiveError && LOCAL_ActiveError->errorNo != YAP_NO_ERROR) {
        e = MkErrorTerm(LOCAL_ActiveError);
        Term t;
        if (active) {
            t = Yap_MkApplTerm(FunctorException, 1, &e);
        } else {
            t = Yap_MkApplTerm(FunctorExternalException, 1, &e);
        }
        port_pt[0] = t;
        completion_pt[0] = TermException;
    } else {
        completion_pt[0] = port_pt[0] = TermCut;
    }
    Yap_ignore(cleanup, false);
    CELL *complete_pt = deref_ptr(RepAppl(task) + 4);
    complete_pt[0] = TermTrue;
    if (ex_mode) {
      // Yap_PutException(e);
      return true;
    }
    bool rc = !Yap_RaiseException();
    return rc;
}

/**
 * external backtrack to current stack frame: call method
 * and control backtracking.
 *
 * @method protect_stack_from_restore
 * @param  USES_REGS1                 [env for threaded execution]
 * @return                       c
 */
static bool watch_retry(Term d0 USES_REGS) {
    // called after backtracking..
    //
    Term task = TailOfTerm(d0);
    bool box = ArgOfTerm(1, task) == TermTrue;
    Term cleanup = ArgOfTerm(3, task);
    bool complete = !IsVarTerm(ArgOfTerm(4, task));
    bool active = ArgOfTerm(5, task) == TermTrue;
    choiceptr B0 = (choiceptr) (LCL0 - IntegerOfTerm(ArgOfTerm(6, task)));

    if (complete)
        return true;
    LOCAL_Signals = 0;
    CalculateStackGap(PASS_REGS1);
    LOCAL_PrologMode = UserMode;

    CELL *port_pt = pDerefa(RepAppl(Deref(task)) + 2);
    CELL *complete_pt = pDerefa(RepAppl(Deref(task)) + 4);
    Term t, e = 0;
    bool ex_mode = false;

    while (B && B->cp_ap &&                                         
	   (B->cp_ap->opc == FAIL_OPCODE ||
	   B->cp_ap == TRUSTFAILCODE ||
	    B->cp_ap == NOCODE)
	   )
        B = B->cp_b;
    if (!B || B->cp_ap == NULL || B->cp_ap->opc == 0) {
      B = (choiceptr)(LCL0-(LOCAL_CBorder));
      B--;
    }
    ASP = (CELL *) PROTECT_FROZEN_B(B);
    // just do the frrpest
    if (B >= B0 && !ex_mode && !active) {
      port_pt[0] = TermFail;
        return true;
    }
    if (LOCAL_ActiveError && LOCAL_ActiveError->errorNo != YAP_NO_ERROR) {
        e = MkErrorTerm(LOCAL_ActiveError);
        if (active) {
            t = Yap_MkApplTerm(FunctorException, 1, &e);
        } else {
            t = Yap_MkApplTerm(FunctorExternalException, 1, &e);
        }
        complete_pt[0] = TermException;
    } else if (B >= B0) {
        t = TermFail;
         complete_pt[0] = t;
    } else if (box) {
        t = TermRedo;
    } else {
        return true;
    }
    port_pt[0] = t;
    Yap_ignore(cleanup, true);
    if (ex_mode) {
        // Yap_PutException(e);
        return true;
    }
    bool rc = !Yap_RaiseException();
    return rc;
}

/**
 * First call to non deterministic predicate. Just leaves a choice-point
 * hanging about for the future.
 *
 * @method protect_stack
 * @param  USES_REGS1    [env for threaded execution]
 * @return               [always succeed]
 */

static Int setup_call_catcher_cleanup(USES_REGS1) {
    yhandle_t sl = Yap_StartSlots();
    Term Setup = Deref(ARG1);
    Int  B0 = LCL0-(CELL*)B;
    yamop *oP = P, *oCP = CP;
    Int oENV = LCL0 - ENV;
    Int oYENV = LCL0 - YENV;
    bool rc;
    LOCAL_Signals = 0;
    CalculateStackGap(PASS_REGS1);
    LOCAL_PrologMode = UserMode;
    Yap_DisableInterrupts(worker_id);
    rc = Yap_RunTopGoal(Setup, false);
    Yap_EnableInterrupts(worker_id);

    if (Yap_RaiseException()) {
        return false;
    }
    if (!rc) {
      complete_inner_computation((choiceptr)(LCL0-B0));
        // We'll pass it throughs

    Yap_CloseSlots(sl);
	rc = false;
    } else {
      B = (choiceptr)(LCL0-B0);
      prune_inner_computation(B);
    }
    P = oP;
    CP = oCP;
    ENV = LCL0 - oENV;
    YENV = LCL0 - oYENV;
    Yap_CloseSlots(sl);
    return rc;
}

static Int tag_cleanup(USES_REGS1) {
    Int iB = LCL0 - (CELL *) B;
    set_watch(iB, Deref(ARG2));
    return Yap_unify(ARG1, MkIntegerTerm(iB));
}

static Int cleanup_on_exit(USES_REGS1) {

    yhandle_t sl = Yap_StartSlots();
    choiceptr B0 = (choiceptr) (LCL0 - IntegerOfTerm(Deref(ARG1)));
    Term task = Deref(ARG2);
    bool box = ArgOfTerm(1, task) == TermTrue;
    Term cleanup = ArgOfTerm(3, task);
    Term complete = IsNonVarTerm(ArgOfTerm(4, task));
    LOCAL_Signals = 0;
    CalculateStackGap(PASS_REGS1);
    LOCAL_PrologMode = UserMode;

    while (B->cp_ap->opc == FAIL_OPCODE)
        B = B->cp_b;

    if (complete) {
        return true;
    }
    CELL *catcher_pt = deref_ptr(RepAppl(Deref(task)) + 2);
    CELL *complete_pt = deref_ptr(RepAppl(Deref(task)) + 4);
    if (B < B0) {
        // non-deterministic
        set_watch(LCL0 - (CELL *) B, task);
        if (!box) {
	  Yap_CloseSlots(sl);
            return true;
        }
        catcher_pt[0] = TermAnswer;
    } else {
        catcher_pt[0] = TermExit;
        complete_pt[0] = TermExit;
    }
    Yap_ignore(cleanup, false);
    Yap_CloseSlots(sl);
    if (Yap_RaiseException()) {
        return false;
    }
    return true;
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
    Yap_ResetException(NULL);
    ARG1 = Yap_GetFromSlot(h1);
    ARG2 = cmod;
    ARG3 = Yap_GetFromSlot(h2);
    /* user:goal_expansion(A,CurMod,B) */
    if ((pe = RepPredProp(
            Yap_GetPredPropByFunc(FunctorGoalExpansion, USER_MODULE))) &&
        pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred != UNDEF_OPCODE &&
        Yap_execute_pred(pe, NULL, false  PASS_REGS)) {
        return complete_ge(true, omod, sl, creeping);
    }
    Yap_ResetException(NULL);

    mg_args[0] = cmod;
    mg_args[1] = Yap_GetFromSlot(h1);
    ARG1 = Yap_MkApplTerm(FunctorModule, 2, mg_args);
    ARG2 = Yap_GetFromSlot(h2);
    /* user:goal_expansion(A,B) */
    if (cmod != USER_MODULE && /* we have tried this before */
        (pe = RepPredProp(
                Yap_GetPredPropByFunc(FunctorGoalExpansion2, USER_MODULE))) &&
        pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred != UNDEF_OPCODE &&
        Yap_execute_pred(pe, NULL, false PASS_REGS)) {
        return complete_ge(true, omod, sl, creeping);
    }
    Yap_ResetException(NULL);
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

    if (should_creep()) {
        return EnterCreepMode(t, mod PASS_REGS);
    }
    t = Yap_YapStripModule(t, &mod);
    restart_exec:
    if (IsVarTerm(t)) {
        Yap_ThrowError(INSTANTIATION_ERROR, ARG3, "call/1");
        return false;
    } else if (IsAtomTerm(t)) {
        Atom a = AtomOfTerm(t);
        pe = PredPropByAtom(a, mod);
    } else if (IsPairTerm(t)) {
        Term ts[2];
        ts[0] = t;
        ts[1] = (CurrentModule == 0 ? TermProlog : CurrentModule);
        t = Yap_MkApplTerm(FunctorCsult, 2, ts);
        goto restart_exec;
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
        //Yap_ThrowError(TYPE_ERROR_CALLABLE, t, "call/1");
        //return false;
        return CallMetaCall(t, mod);
    }
    /*	N = arity; */
    /* call may not define new system predicates!! */
    return CallPredicate(RepPredProp(pe), B,
                         RepPredProp(pe)->CodeOfPred PASS_REGS);
}

static Int creep_step(USES_REGS1) { /* '$execute_nonstop'(Goal,Mod)
                                     */
    Term t = Deref(ARG1);
    Term mod = Deref(ARG2);
    unsigned int arity;
    Prop pe;
    bool rc;
    t = Yap_YapStripModule(t, &mod);
    if (IsVarTerm(mod)) {
        mod = CurrentModule;
    } else if (!IsAtomTerm(mod)) {
        Yap_ThrowError(TYPE_ERROR_ATOM, ARG2, "call/1");
        return FALSE;
    }
    if (should_creep()) {
        return EnterCreepMode(t, mod PASS_REGS);
    }
    if (IsVarTerm(t)) {
        Yap_ThrowError(INSTANTIATION_ERROR, ARG1, "call/1");
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
        return CallMetaCall(t, mod);
    }
    /*	N = arity; */
    /* call may not define new system predicates!! */
    if (RepPredProp(pe)->PredFlags & SpiedPredFlag) {
        if (should_creep()) {
            Yap_signal(YAP_CREEP_SIGNAL);
        }
#if defined(YAPOR) || defined(THREADS)
        if (RepPredProp(pe)->PredFlags & LogUpdatePredFlag) {
          PP = RepPredProp(pe);
          PELOCK(80, PP);
        }
#endif
        rc = CallPredicate(RepPredProp(pe), B,
                           RepPredProp(pe)->TrueCodeOfPred PASS_REGS);
    } else {
        rc = CallPredicate(RepPredProp(pe), B,
                           RepPredProp(pe)->CodeOfPred PASS_REGS);
    }
            if (should_creep()) {
        Yap_signal(YAP_CREEP_SIGNAL);
    }
    return rc;
}

/**
 * @brief Two argument version of non-interruptible execution: this will
 * ignore signals including debugging requests.
 *
 * @return Int succeeds if it can transfer control.
 */

static Int execute_nonstop(USES_REGS1) {
    Term t = Deref(ARG1);
    Term mod = Deref(ARG2);
    unsigned int arity;
    Prop pe;

    t = Yap_YapStripModule(t, &mod);
    if (IsVarTerm(mod)) {
        mod = CurrentModule;
    } else if (!IsAtomTerm(mod)) {
        Yap_ThrowError(TYPE_ERROR_ATOM, ARG2, "call/1");
        return FALSE;
    }
    if (IsVarTerm(t)) {
        Yap_ThrowError(INSTANTIATION_ERROR, ARG1, "call/1");
        return FALSE;
    } else if (IsAtomTerm(t)) {
        Atom a = AtomOfTerm(t);
        pe = PredPropByAtom(a, mod);
    } else if (IsPairTerm(t)) {
        ARG1 = t;
        ARG2 = (CurrentModule == 0 ? TermProlog : CurrentModule);
        pe = PredPropByFunc(FunctorCsult, TermProlog);
        arity = 2;
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
        Yap_ThrowError(TYPE_ERROR_CALLABLE, t, "call/1");
        return FALSE;
    }
    /*	N = arity; */
    /* call may not define new system predicates!! */
    if (RepPredProp(pe)->PredFlags & SpiedPredFlag) {
        if (should_creep()) {
            Yap_signal(YAP_CREEP_SIGNAL);
        }
#if defined(YAPOR) || defined(THREADS)
        if (RepPredProp(pe)->PredFlags & LogUpdatePredFlag) {
          PP = RepPredProp(pe);
          PELOCK(80, PP);
        }
#endif
        return CallPredicate(RepPredProp(pe), B,
                             RepPredProp(pe)->TrueCodeOfPred PASS_REGS);
    } else {
        if (should_creep()) {
            Yap_signal(YAP_CREEP_SIGNAL);
        }
        return CallPredicate(RepPredProp(pe), B,
                             RepPredProp(pe)->CodeOfPred PASS_REGS);
    }
}

/**
 * @brief One argument version of non-interruptible execution: this will
 * ignore signals including debugging requests.
 *
 * @return Int succeeds if it can transfer control.
 */
static Int execute_nonstop1(USES_REGS1) {
    ARG2 = CurrentModule;
    return execute_nonstop(PASS_REGS1);
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
        Yap_ThrowError(INSTANTIATION_ERROR, d, "depth_bound_call/2");
        return false;
    } else if (!IsIntegerTerm(d)) {
        if (IsFloatTerm(d) && isinf(FloatOfTerm(d))) {
            DEPTH = RESET_DEPTH();
        } else {
            Yap_ThrowError(TYPE_ERROR_INTEGER, d, "depth_bound_call/2");
            return false;
        }
    } else {
        DEPTH = MkIntTerm(IntegerOfTerm(d) * 2);
    }
    return execute(PASS_REGS1);
}

#endif

static bool exec_absmi(bool top, yap_reset_t reset_mode USES_REGS) {
    int lval = 0, out;
    Int OldBorder = LOCAL_CBorder;
    yhandle_t OldHandleBorder = LOCAL_HandleBorder;
    //   yap_error_descriptor_t *err_info= LOCAL_ActiveError;
    LOCAL_CBorder = LCL0 - ENV;
    LOCAL_MallocDepth = AllocLevel();
    LOCAL_HandleBorder = Yap_CurrentSlot();

    sigjmp_buf signew, *sighold = LOCAL_RestartEnv;
    LOCAL_RestartEnv = &signew;
    volatile int i = AllocLevel();
    if /* top &&*/ ((lval = sigsetjmp(signew, 1)) != 0) {
        switch (lval) {
            case 1: { /* restart */
                /* otherwise, SetDBForThrow will fail entering critical mode */
                // LOCAL_ActiveError = err_info;
                LOCAL_PrologMode = UserMode;
                LOCAL_DoingUndefp = false;
                /* find out where to cut to */
                /* siglongjmp resets the TR hardware register */
                /* TR and B are crucial, they might have been changed, or not */
                restore_TR();
                restore_B();
                /* H is not so important, because we're gonna backtrack */
                restore_H();
                /* set stack */
                ASP = (CELL *) PROTECT_FROZEN_B(B);
                /* forget any signals active, we're reborne */
                LOCAL_Signals = 0;
                CalculateStackGap(PASS_REGS1);
                LOCAL_PrologMode = UserMode;
                Yap_CloseSlots(OldHandleBorder);
                P = (yamop *) FAILCODE;
            }
                break;
            case 2: {
                // LOCAL_ActiveError = err_info;
                /* arithmetic exception */
                /* must be done here, otherwise siglongjmp will clobber all the
                 * registers
                 */
                /* reset the registers so that we don't have trash in abstract
                 * machine */
                pop_text_stack(i + 1);
                Yap_set_fpu_exceptions(
                        getAtomicGlobalPrologFlag(ARITHMETIC_EXCEPTIONS_FLAG));
                P = (yamop *) FAILCODE;
                LOCAL_PrologMode = UserMode;
                LOCAL_DoingUndefp = false;
                Yap_CloseSlots(OldHandleBorder);
            }
                break;
            case 3: { /* saved state */
                // LOCAL_ActiveError = err_info;
                pop_text_stack(i + 1);
                LOCAL_CBorder = OldBorder;
		LOCAL_HandleBorder = OldHandleBorder;
		LOCAL_RestartEnv = sighold;
                LOCAL_PrologMode = UserMode;
                LOCAL_DoingUndefp = false;
                Yap_CloseSlots(OldHandleBorder);
                return false;
            }
            case 4:
                /* abort */
                /* can be called from anywhere, must reset registers,
                 */
                // LOCAL_ActiveError = err_info;
                while (B) {
                    LOCAL_ActiveError->errorNo = ABORT_EVENT;
                    pop_text_stack(i + 1);
                    Yap_CloseSlots(LOCAL_HandleBorder);
                    Yap_JumpToEnv();
                }
                LOCAL_PrologMode = UserMode;
                LOCAL_DoingUndefp = false;
                P = (yamop *) FAILCODE;
		LOCAL_HandleBorder = OldHandleBorder;
		LOCAL_RestartEnv = sighold;
                Yap_CloseSlots(OldHandleBorder);
                pop_text_stack(i + 1);
                return false;
                break;
            case 5:
                // going up, unless there is no up to go to. or someone
                // but we should inform the caller on what happened.

                //           Yap_regp = old_rs;
                // LOCAL_ActiveError = err_info;
                restore_TR();
                restore_B();
                /* H is not so important, because we're gonna backtrack */
                restore_H();
                /* set stack */
                Yap_JumpToEnv();
                Yap_CloseTemporaryStreams();
                Yap_CloseSlots(LOCAL_HandleBorder );
                ASP = (CELL *) PROTECT_FROZEN_B(B);

                if (B == NULL || B->cp_b == NULL ||
                    (CELL *) (B->cp_b) > LCL0 - LOCAL_CBorder) {
		  LOCAL_HandleBorder = OldHandleBorder;
		  LOCAL_RestartEnv = sighold;
                    LOCAL_CBorder = OldBorder;
                    pop_text_stack(i + 1);
                    return false;
                }
                P = FAILCODE;
        }
    }
    YENV = ASP;
    YENV[E_CB] = Unsigned(B);
    pop_text_stack(i + 1);
    out = Yap_absmi(0);
    /* make sure we don't leave a FAIL signal hanging around */
    Yap_get_signal(YAP_FAIL_SIGNAL);
    if (!Yap_has_a_signal())
        CalculateStackGap(PASS_REGS1);
    LOCAL_CBorder = OldBorder;
    LOCAL_RestartEnv = sighold;
    Yap_CloseSlots(OldHandleBorder);
    LOCAL_HandleBorder = OldHandleBorder;
    return out;
}

/**
   @brief prepare a (sub-)computation.

 create an initial pseudo environment so that when garbage
       collection is going up in the environment chain it doesn't get
       confused */
void Yap_PrepGoal(arity_t arity, CELL *pt, choiceptr saved_b USES_REGS) {
  Yap_ResetException(worker_id);
    //  sl = Yap_InitSlot(t);
    // recover CP when doing gc.
    YENV = ASP;
    YENV[E_CP] = (CELL) CP;
    YENV[E_CB] = (CELL) B;
    YENV[E_E] = (CELL) ENV;
#ifdef TABLING
    YENV[E_B] = (CELL) B;
#endif
#ifdef DEPTH_LIMIT
    YENV[E_DEPTH] = DEPTH;
#endif
    ENV = YENV;
    ASP -= EnvSizeInCells;
    /* and now create a pseudo choicepoint for much the same reasons */
    CP = YESCODE;
    /* keep a place where you can inform you had an exception */
    if (pt) {
        int i;
        for (i = 0; i < arity; i++) {
            XREGS[i + 1] = *pt++;
        }
    }
    B = (choiceptr) ASP;
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
    YENV = ASP = (CELL *) B;
    YENV[E_CB] = (CELL) B;
    HB = HR;
    CP = YESCODE;
}

static bool do_goal(yamop *CodeAdr, int arity, CELL *pt, bool top USES_REGS) {
    choiceptr saved_b = B;
    bool out;
    Yap_PrepGoal(arity, pt, saved_b PASS_REGS);
    //  CACHE_A1();
    P = (yamop *) CodeAdr;
    //  S = CellPtr(RepPredProp(
    //    PredPropByFunc(Yap_MkFunctor(AtomCall, 1), 0))); /* A1 mishaps */

    out = exec_absmi(top, YAP_EXEC_ABSMI PASS_REGS);
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
 *
 * @param  USES_REGS    thread support
 */
void Yap_fail_all(choiceptr bb USES_REGS) {
    yamop *saved_p, *saved_cp;

    saved_p = P;
    saved_cp = CP;
    /* prune away choicepoints */
    if (B == bb)
        return;
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
    ENV = (CELL *) (ENV[E_E]);
    /* ASP should be set to the top of the local stack when we
       did the call */
    ASP = B->cp_env;
    /* YENV should be set to the current environment */
    YENV = ENV = (CELL *) ((B->cp_env)[E_E]);
    if (B->cp_b) {
        B = B->cp_b;
    }
    // SET_BB(B);
    HB = PROTECT_FROZEN_H(B);
    CP = saved_cp;
    P = saved_p;
}

bool Yap_execute_pred(PredEntry *ppe, CELL *pt, bool pass_ex USES_REGS) {
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
        cut_B = (choiceptr) ENV[E_CB];
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
        ENV = (CELL *) (ENV[E_E]);
        /* we have failed, and usually we would backtrack to this B,
           trouble is, we may also have a delayed cut to do */
        if (B != NULL)
            HB = B->cp_h;
        YENV = ENV;
        // should we catch the exception or pass it through?
        // We'll pass it through
        if (Yap_HasException()) {
            if (pass_ex &&
                ((LOCAL_PrologMode & BootMode) || !CurrentModule)) {
                Yap_ResetException(LOCAL_ActiveError);
            } else {
                Yap_RaiseException();
            }
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
        YENV = ENV = (CELL *) ((B->cp_env)[E_E]);
        B = B->cp_b;
        SET_BB(B);
        HB = PROTECT_FROZEN_H(B);
        // should we catch the exception or pass it through?
        // We'll pass it through
        if (Yap_HasException()) {
            if (pass_ex &&
                ((LOCAL_PrologMode & BootMode) || !CurrentModule)) {
                Yap_ResetException(LOCAL_ActiveError);
            } else {
                Yap_RaiseException();
            }
        }
        return false;
    } else {
        Yap_ThrowError(SYSTEM_ERROR_INTERNAL, TermNil, "emulator crashed");
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
            Yap_ThrowError(TYPE_ERROR_CALLABLE, t, "call/1");
            return false;
        }
        /* I cannot use the standard macro here because
           otherwise I would dereference the argument and
           might skip a svar */
        pt = RepAppl(t) + 1;
        pe = PredPropByFunc(f, mod);
    } else {
        Yap_ThrowError(TYPE_ERROR_CALLABLE, t, "call/1");
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
    ENV = (CELL *) ((B->cp_env)[E_E]);
    B = B->cp_b;
    P = (yamop *) (ENV[E_CP]);
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
    if (IsVarTerm(t)) {
        Yap_ThrowError(INSTANTIATION_ERROR, t, "call/1");
        LOCAL_PrologMode &= ~TopGoalMode;
        return (FALSE);
    }
    if (IsPairTerm(t)) {
        Term ts[2];
        ts[0] = t;
        ts[1] = (CurrentModule == 0 ? TermProlog : CurrentModule);
        t = Yap_MkApplTerm(FunctorCsult, 2, ts);
    }
    if (IsAtomTerm(t)) {
        Atom a = AtomOfTerm(t);
        pt = NULL;
        pe = Yap_GetPredPropByAtom(a, tmod);
        arity = 0;
    } else if (IsApplTerm(t)) {
        Functor f = FunctorOfTerm(t);

        if (IsBlobFunctor(f)) {
            Yap_ThrowError(TYPE_ERROR_CALLABLE, t, "call/1");
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
        Yap_ThrowError(TYPE_ERROR_CALLABLE, Yap_TermToIndicator(t, tmod), "call/1");
        LOCAL_PrologMode &= ~TopGoalMode;
        return (FALSE);
    }
    ppe = RepPredProp(pe);
    if (pe == NIL || ppe->TrueCodeOfPred->opc == UNDEF_OPCODE
	||ppe->PredFlags & (MetaPredFlag | UndefPredFlag)) {
        // we're in a meta-call, take care about modules
        //
       ARG1 = t;
        ARG2 = cp_as_integer(B PASS_REGS); /* p_current_choice_point */
        ARG3 = t;
        ARG4 = tmod;
         return CallPredicate(PredMetaCall, B, PredMetaCall->CodeOfPred PASS_REGS);
    }
    PELOCK(82, ppe);
    CodeAdr = ppe->CodeOfPred;
    UNLOCK(ppe->PELock);

#if !USE_SYSTEM_MALLOC
    if (LOCAL_TrailTop - HeapTop < 2048) {
      Yap_ThrowError(RESOURCE_ERROR_TRAIL, TermNil,
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
        Yap_ThrowError(INSTANTIATION_ERROR, t, "support for coroutining");
        return (FALSE);
    }
    if (IsAtomicTerm(t))
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
        Yap_ThrowError(INSTANTIATION_ERROR, t, "support for coroutining");
        return (FALSE);
    }
     if (!IsAtomicTerm(t)) {
      do_restore_regs(t, TRUE PASS_REGS);
    }
   d0 = Deref(ARG2);
    if (IsVarTerm(d0)) {
        Yap_ThrowError(INSTANTIATION_ERROR, d0, "support for coroutining");
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
    pt0 = (choiceptr) (LCL0 - d);
#endif
    /* find where to cut to */
    if ((CELL *) pt0 != LCL0 && pt0 > B) {
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
        pt0->cp_ap = (yamop *) TRUSTFAILCODE;
    }
    return TRUE;
}

static int disj_marker(yamop *apc) {
    op_numbers opnum = Yap_op_from_opcode(apc->opc);

    return opnum == _or_else || opnum == _or_last;
}

static Int cut_up_to_next_disjunction(USES_REGS1) {
    choiceptr pt0 = B;
    CELL *qenv = (CELL *) ENV[E_E];

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

/**
 * Reset the Prolog engine . If _Hard_ resèt the global stack_el. If
 * p_no_use_'soft_float keei
 *
 * @param mode
 * @param hard
 *
 * @return
 */
bool Yap_Reset(yap_reset_t mode, bool hard) {
    CACHE_REGS
    int res = TRUE;

    Yap_ResetException(worker_id);
    /* first, backtrack to the root */
    while (B) {
        P = FAILCODE;
        Yap_exec_absmi(true, mode);
        B = B->cp_b;
    }
    /* reinitialize the engine */
    Yap_InitYaamRegs(worker_id, false);
    GLOBAL_Initialised = true;
    ENV = LCL0;
    ASP = (CELL *) B;
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

static Int JumpToEnv(USES_REGS1) {
    choiceptr handler = B;
    /* just keep the throwm object away, we don't need to care about it
     */
    /* careful, previous step may have caused a stack shift,
       so get pointers here     */
    /* find the first choicepoint that may be a catch */
    // DBTerm *dbt = Yap_RefToException();
    while (handler && handler->cp_ap &&
	   Yap_PredForChoicePt(handler, NULL) != PredDollarCatch &&
           LOCAL_CBorder < LCL0 - (CELL *) handler && handler->cp_ap != NOCODE &&
           handler->cp_b != NULL) {
        handler->cp_ap = TRUSTFAILCODE;
        handler = handler->cp_b;
    }
    if (LOCAL_PrologMode & AsyncIntMode) {
        Yap_signal(YAP_FAIL_SIGNAL);
    }

    B = handler;
    P = FAILCODE;
    LOCAL_DoingUndefp = false;
    return true;
}

bool Yap_JumpToEnv(void) {
    CACHE_REGS
    if (LOCAL_PrologMode & TopGoalMode)
        return true;
    return JumpToEnv(PASS_REGS1);
}

/* This does very nasty stuff!!!!! */
static Int jump_env(USES_REGS1) {
    Term t = Deref(ARG1), t0 = t;
    if (IsVarTerm(t)) {
        Yap_ThrowError(INSTANTIATION_ERROR, t,
                       "throw/1 must be called instantiated");
    }
    // Yap_DebugPlWriteln(t);
    // char *buf = Yap_TermToBuffer(t, ENC_ISO_UTF8,
    //                             Quote_illegal_f | Ignore_ops_f |
    //                             Unfold_cyclics_f);
    //  __android_log_print(ANDROID_LOG_INFO, "YAPDroid ", " throw(%s)", buf);
    LOCAL_ActiveError = Yap_UserError(t0, LOCAL_ActiveError);
    bool out = JumpToEnv(PASS_REGS1);
    if (LOCAL_ActiveError->errorNo == ABORT_EVENT) {
    if (B != NULL && P == FAILCODE && B->cp_ap == NOCODE &&
        LCL0 - (CELL *) B > LOCAL_CBorder) {
        // we're failing up to the top layer
    }
    } else {
      if (B != NULL &&
	  LCL0 - (CELL *) B > LOCAL_CBorder) {
        // we're failing up to the top layer
      }
    }
    if (B != NULL && P == FAILCODE && B->cp_ap == NOCODE &&
        LCL0 - (CELL *) B > LOCAL_CBorder) {
        // we're failing up to the top layer
    }
    pop_text_stack(LOCAL_MallocDepth + 1);
    return out;
}

/* set up a meta-call based on . context info */
static Int generate_pred_info(USES_REGS1) {
    ARG1 = ARG3 = ENV[-EnvSizeInCells - 1];
    ARG4 = ENV[-EnvSizeInCells - 3];
    ARG2 = cp_as_integer((choiceptr) ENV[E_CB] PASS_REGS);
    return TRUE;
}


void Yap_InitYaamRegs(int myworker_id, bool full_reset) {
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
    Yap_ResetException(LOCAL_ActiveError);
    Yap_PutValue(AtomBreak, MkIntTerm(0));
    TR = (tr_fr_ptr) REMOTE_TrailBase(myworker_id);
    HR = H0 = ((CELL *) REMOTE_GlobalBase(myworker_id)) +
              1; // +1: hack to ensure the gc does not try to mark mistakenly
    LCL0 = ASP = (CELL *) REMOTE_LocalBase(myworker_id);
    CurrentTrailTop = (tr_fr_ptr) (REMOTE_TrailTop(myworker_id) - MinTrailGap);
    /* notice that an initial choice-point and environment
     *must* be created  for the garbage collector to work */
    B = NULL;
    ENV = NULL;
    CP = YESCODE;
    P = NULL;
#ifdef DEPTH_LIMIT
    DEPTH = RESET_DEPTH();
#endif
    STATIC_PREDICATES_MARKED = FALSE;
    if (full_reset) {
      HB = HR = H0;
        h0var = MkVarTerm();
        REMOTE_GcGeneration(myworker_id) = Yap_NewTimedVar(h0var);
        REMOTE_GcCurrentPhase(myworker_id) = 0;
        REMOTE_GcPhase(myworker_id) =
                Yap_NewTimedVar(MkIntTerm(0));
#if COROUTINING
        REMOTE_WokenGoals(myworker_id) = Yap_NewTimedVar(TermNil);
        h0var = MkVarTerm();
        REMOTE_AttsMutableList(myworker_id) = Yap_NewTimedVar(h0var);
#endif
        size_t defsz = 128 * 1024;
        Yap_AllocateDefaultArena(defsz, myworker_id, NULL);
    } else {
        HR = Yap_ArenaLimit(REMOTE_GlobalArena(myworker_id));
    }
    Yap_InitPreAllocCodeSpace(myworker_id);
#ifdef FROZEN_STACKS
    H_FZ = HR;
#ifdef YAPOR_SBA
    BSEG =
#endif /* YAPOR_SBA */
    BBREG = B_FZ = (choiceptr) REMOTE_LocalBase(myworker_id);
    TR = TR_FZ = (tr_fr_ptr) REMOTE_TrailBase(myworker_id);
#endif /* FROZEN_STACKS */
    CalculateStackGap(PASS_REGS1);
    /* the first real choice-point will also have AP=FAIL */
    /* always have an empty slots for people to use */
#if defined(YAPOR) || defined(THREADS)
    LOCAL = REMOTE(myworker_id);
    worker_id = myworker_id;
#endif /* THREADS */
    Yap_RebootSlots(myworker_id);
#if defined(YAPOR) || defined(THREADS)
    PP = NULL;
    PREG_ADDR = NULL;
#endif
    cut_c_initialize(myworker_id);
    Yap_PrepGoal(0, NULL, NULL PASS_REGS);
#ifdef FROZEN_STACKS
    H_FZ = HR;
#ifdef YAPOR_SBA
    BSEG =
#endif /* YAPOR_SBA */
    BBREG = B_FZ = (choiceptr) REMOTE_LocalBase(myworker_id);
    TR = TR_FZ = (tr_fr_ptr) REMOTE_TrailBase(myworker_id);
#endif /* FROZEN_STACKS */
    CalculateStackGap(PASS_REGS1);
#ifdef TABLING
    /* ensure that LOCAL_top_dep_fr is always valid */
    if (REMOTE_top_dep_fr(myworker_id))
        DepFr_cons_cp(REMOTE_top_dep_fr(myworker_id)) = NORM_CP(B);
#endif
}

void Yap_track_cpred(void *v)
{
  gc_entry_info_t*i = v;

  if (!P) {
    i->env = ENV;
    i->p = NULL;
    i->p_env = CP;
    i->a = 0;
    i->op = 0;
    return;
  }
  if ((i->op = (i->p = PREVOP(P, Osbpp))->opc) == Yap_opcode(_call_usercpred)||
      i->op == Yap_opcode(_call_cpred)||i->op == Yap_opcode(_call)) {
    i->env = ENV;// YENV should be tracking ENV
    i->p_env = P;
    i->a = i->p->y_u.Osbpp.p->ArityOfPE;
  } else if ((i->op = (i->p=P)->opc) == Yap_opcode(_execute_cpred)){
      i->a = i->p->y_u.Osbpp.p->ArityOfPE;
      i->p_env = CP;
      i->env = ENV;
    } else if ((i->op = (i->p=P)->opc) == Yap_opcode(_try_c) ||
	       i->op == Yap_opcode(_retry_c)){
      i->a = P->y_u.OtapFs.s;
      i->p_env = CP;
      i->env = ENV;
  } else {

    i->env = ENV;
    i->p = P;
    i->p_env = CP;
    i->a = 0;
    i->op = 0;
  }
}
 
int Yap_dogc(arity_t args, Term *tp USES_REGS) {
  gc_entry_info_t info;
  arity_t arity = args;
    
    Yap_track_cpred( &info );
    info.a = arity;
    if (!Yap_gc(&info)) {
        return false;
    }
    return true;
}

static Int get_debugger_state(USES_REGS1) {
    const char *s = RepAtom(AtomOfTerm(Deref(ARG1)))->StrOfAE;
    if (!strcmp(s, "creep")) {
        return Yap_unify(ARG2, LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP]);
    }
    if (!strcmp(s, "goal_number")) {
        return Yap_unify(ARG2, LOCAL_debugger_state[DEBUG_GOAL_NUMBER]);
    }
    if (!strcmp(s, "spy")) {
        return Yap_unify(ARG2, LOCAL_debugger_state[DEBUG_SPY]);
    }
    if (!strcmp(s, "trace")) {
        return Yap_unify(ARG2, LOCAL_debugger_state[DEBUG_TRACE]);
    }
    if (!strcmp(s, "debug")) {
        return Yap_unify(ARG2, LOCAL_debugger_state[DEBUG_DEBUG]);
    }
    return false;
}

static Int set_debugger_state(USES_REGS1) {
    const char *s = RepAtom(AtomOfTerm(Deref(ARG1)))->StrOfAE;
    if (!strcmp(s, "creep")) {
        LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP] = Deref(ARG2);
        return true;
    }
    if (!strcmp(s, "goal_number")) {
        LOCAL_debugger_state[DEBUG_GOAL_NUMBER] = Deref(ARG2);
        return true;
    }
    if (!strcmp(s, "spy")) {
        LOCAL_debugger_state[DEBUG_SPY] = Deref(ARG2);
        return true;
    }
    if (!strcmp(s, "trace")) {
        LOCAL_debugger_state[DEBUG_TRACE] = Deref(ARG2);
        return true;
    }
    if (!strcmp(s, "debug")) {
        LOCAL_debugger_state[DEBUG_DEBUG] = Deref(ARG2);
        return true;
    }
    return false;
}


static void init_debugger_state(void)
    {

        LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP] = TermCreep;
        LOCAL_debugger_state[DEBUG_GOAL_NUMBER] = MkIntTerm(0);
        LOCAL_debugger_state[DEBUG_SPY] = TermOff;
        LOCAL_debugger_state[DEBUG_TRACE] = TermFalse;
        LOCAL_debugger_state[DEBUG_DEBUG] = TermFalse;
    }


static Int set_debugger_state5(USES_REGS1) {
    Term t1 = Deref(ARG1);
    if (!IsVarTerm(t1)) {

        LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP] = t1;
    }
    t1 = Deref(ARG2);
    if (!IsVarTerm(t1)) {
        LOCAL_debugger_state[DEBUG_GOAL_NUMBER] = t1;
    }
    t1 = Deref(ARG3);
    if (!IsVarTerm(t1)) {
        LOCAL_debugger_state[DEBUG_SPY] = t1;
    }
    t1 = Deref(ARG4);
    if (!IsVarTerm(t1)) {
        LOCAL_debugger_state[DEBUG_TRACE] = t1;
    }
    t1 = Deref(ARG5);
    if (!IsVarTerm(t1)) {
        LOCAL_debugger_state[DEBUG_DEBUG] = t1;
    }
    return true;
}


static Int get_debugger_state5(USES_REGS1) {
    Term t1 = Deref(ARG1);
        if (!Yap_unify(LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP], t1))
            return false;
    t1 = Deref(ARG2);
        if (!Yap_unify(LOCAL_debugger_state[DEBUG_GOAL_NUMBER], t1))
            return false;
    t1 = Deref(ARG3);
        if (!Yap_unify(LOCAL_debugger_state[DEBUG_SPY], t1))
            return false;
    t1 = Deref(ARG4);
        if (!Yap_unify(LOCAL_debugger_state[DEBUG_TRACE], t1))
            return false;
    t1 = Deref(ARG5);
        if (!Yap_unify(LOCAL_debugger_state[DEBUG_DEBUG], t1))
            return false;
    return true;
}

void Yap_InitExecFs(void) {
    CACHE_REGS
    YAP_opaque_handler_t catcher_ops;
    memset(&catcher_ops, 0, sizeof(catcher_ops));
    catcher_ops.cut_handler = watch_cut;
    catcher_ops.fail_handler = watch_retry;
    setup_call_catcher_cleanup_tag = YAP_NewOpaqueType(&catcher_ops);
init_debugger_state();

    Term cm = CurrentModule;
    Yap_InitComma();
    Yap_InitCPred("restore_regs", 1, restore_regs,
                  NoTracePredFlag );
    Yap_InitCPred("restore_regs", 2, restore_regs2,
                  NoTracePredFlag );
    Yap_InitCPred("$execute", 1, execute,  NoTracePredFlag);
    Yap_InitCPred("$execute", 2, execute2,  NoTracePredFlag);
    Yap_InitCPred("$execute", 3, execute3,  NoTracePredFlag);
    Yap_InitCPred("$execute", 4, execute4,  NoTracePredFlag);
    Yap_InitCPred("$execute", 5, execute5,  NoTracePredFlag);
    Yap_InitCPred("$execute", 6, execute6,  NoTracePredFlag);
    Yap_InitCPred("$execute", 7, execute7,  NoTracePredFlag);
    Yap_InitCPred("$execute", 8, execute8,  NoTracePredFlag);
    Yap_InitCPred("$execute", 9, execute9,  NoTracePredFlag);
    Yap_InitCPred("$execute", 10, execute10,  NoTracePredFlag);
    Yap_InitCPred("$execute", 11, execute11,  NoTracePredFlag);
    Yap_InitCPred("$execute", 12, execute12,  NoTracePredFlag);
    Yap_InitCPred("$execute_in_mod", 2, execute_in_mod,  NoTracePredFlag);
    Yap_InitCPred("$execute_wo_mod", 2, execute_in_mod,  NoTracePredFlag);
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
    Yap_InitCPred("$execute_under_depth_limit", 2, execute_depth_limit, NoTracePredFlag);
#endif

    Yap_InitCPred("$execute0", 2, execute0, NoTracePredFlag);
    Yap_InitCPred("$execute_nonstop", 2, execute_nonstop, NoTracePredFlag);
    Yap_InitCPred("$execute_nonstop", 1, execute_nonstop1, NoTracePredFlag);
    Yap_InitCPred("$creep_step", 2, creep_step, NoTracePredFlag);
    Yap_InitCPred("$execute_clause", 4, execute_clause, NoTracePredFlag);
    Yap_InitCPred("$creep_clause", 4, creep_clause,  NoTracePredFlag);
    Yap_InitCPred("$current_choice_point", 1, current_choice_point,  NoTracePredFlag);
    Yap_InitCPred("$current_choicepoint", 1, current_choice_point,  NoTracePredFlag);
    CurrentModule = HACKS_MODULE;
    Yap_InitCPred("current_choice_point", 1, current_choice_point, 0);
    Yap_InitCPred("current_choicepoint", 1, current_choice_point, 0);
    Yap_InitCPred("env_choice_point", 1, save_env_b, 0);
    Yap_InitCPred("parent_choice_point", 1, parent_choice_point1, 0);
    Yap_InitCPred("parent_choice_point", 2, parent_choice_point, 0);
    Yap_InitCPred("cut_at", 1, clean_ifcp, SafePredFlag);
    CurrentModule = cm;
    Yap_InitCPred("qpack_clean_up_to_disjunction", 0, cut_up_to_next_disjunction,
                  SafePredFlag);
    Yap_InitCPred("$clean_ifcp", 1, clean_ifcp, NoTracePredFlag | SafePredFlag);
    Yap_InitCPred("throw", 1, jump_env, 0);
    Yap_InitCPred("$generate_pred_info", 4, generate_pred_info, NoTracePredFlag);
    Yap_InitCPred("_user_expand_goal", 2, _user_expand_goal, NoTracePredFlag);
    Yap_InitCPred("$do_term_expansion", 2, do_term_expansion, NoTracePredFlag);
    Yap_InitCPred("$setup_call_catcher_cleanup", 1, setup_call_catcher_cleanup,
                  NoTracePredFlag);
    Yap_InitCPred("$cleanup_on_exit", 2, cleanup_on_exit, NoTracePredFlag);
    Yap_InitCPred("$tag_cleanup", 2, tag_cleanup, NoTracePredFlag);
    Yap_InitCPred("$get_debugger_state", 2, get_debugger_state, NoTracePredFlag);
    Yap_InitCPred("$get_debugger_state", 5, get_debugger_state5, NoTracePredFlag);
    Yap_InitCPred("$set_debugger_state", 2, set_debugger_state, NoTracePredFlag);
    Yap_InitCPred("$set_debugger_state", 5, set_debugger_state5, NoTracePredFlag);
}

