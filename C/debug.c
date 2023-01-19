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

#include "Yap.h"
#include "absmi.h"
#include "attvar.h"
#include "cut_c.h"
#include "yapio.h"

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

static void init_debugger_state(void) {
  CACHE_REGS
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

/******************************************************************

                MANAGING SPY-POINTS

******************************************************************/

static Int p_is_no_trace(USES_REGS1) { /* '$undefined'(P,Mod)	 */
  PredEntry *pe;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "undefined/1");
  if (EndOfPAEntr(pe))
    return true;
  PELOCK(36, pe);
  if (pe->PredFlags & (NoTracePredFlag | HiddenPredFlag)) {
    UNLOCKPE(57, pe);
    return true;
  }
  UNLOCKPE(59, pe);
  return false;
}

static Int p_set_no_trace(USES_REGS1) { /* '$set_no_trace'(+Fun,+M)	 */
  PredEntry *pe;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "undefined/1");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(36, pe);
  pe->PredFlags |= NoTracePredFlag;
  UNLOCKPE(57, pe);
  return TRUE;
}

int Yap_SetNoTrace(char *name, arity_t arity, Term tmod) {
  PredEntry *pe;

  if (arity == 0) {
    pe = Yap_get_pred(MkAtomTerm(Yap_LookupAtom(name)), tmod, "no_trace");
  } else {
    pe = RepPredProp(
        PredPropByFunc(Yap_MkFunctor(Yap_LookupAtom(name), arity), tmod));
  }
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(36, pe);
  pe->PredFlags |= NoTracePredFlag;
  UNLOCKPE(57, pe);
  return TRUE;
}

static Int p_setspy(USES_REGS1) { /* '$set_spy'(+Fun,+M)	 */
  Atom at;
  PredEntry *pred;
  pred_flags_t fg;
  Term t, mod;

  at = AtomSpy;
  pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1), 0));
  SpyCode = pred;
  t = Deref(ARG1);
  mod = Deref(ARG2);
  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return (FALSE);
  if (IsVarTerm(t))
    return (FALSE);
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pred = RepPredProp(Yap_PredPropByAtomNonThreadLocal(at, mod));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pred = RepPredProp(Yap_PredPropByFunctorNonThreadLocal(fun, mod));
  } else {
    return (FALSE);
  }
  PELOCK(22, pred);
restart_spy:
  if (pred->PredFlags & (CPredFlag | SafePredFlag)) {
    UNLOCKPE(35, pred);
    return FALSE;
  }
  if (pred->OpcodeOfPred == UNDEF_OPCODE || pred->OpcodeOfPred == FAIL_OPCODE) {
    UNLOCKPE(36, pred);
    return FALSE;
  }
  if (pred->OpcodeOfPred == INDEX_OPCODE) {
    int i = 0;
    for (i = 0; i < pred->ArityOfPE; i++) {
      XREGS[i + 1] = MkVarTerm();
    }
    Yap_IPred(pred, 0, CP);
    goto restart_spy;
  }
  fg = pred->PredFlags;
  if (fg & DynamicPredFlag) {
    pred->OpcodeOfPred = ((yamop *)(pred->CodeOfPred))->opc =
        Yap_opcode(_spy_or_trymark);
  } else {
    pred->OpcodeOfPred = Yap_opcode(_spy_pred);
    pred->CodeOfPred = (yamop *)(&(pred->OpcodeOfPred));
  }
  pred->PredFlags |= SpiedPredFlag;
  UNLOCKPE(37, pred);
  return TRUE;
}

static Int p_rmspy(USES_REGS1) { /* '$rm_spy'(+T,+Mod)	 */
  Atom at;
  PredEntry *pred;
  Term t;
  Term mod;

  t = Deref(ARG1);
  mod = Deref(ARG2);
  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return (FALSE);
  if (IsVarTerm(t))
    return (FALSE);
  if (IsAtomTerm(t)) {
    at = AtomOfTerm(t);
    pred = RepPredProp(Yap_PredPropByAtomNonThreadLocal(at, mod));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pred = RepPredProp(Yap_PredPropByFunctorNonThreadLocal(fun, mod));
  } else
    return FALSE;
  PELOCK(23, pred);
  if (!(pred->PredFlags & SpiedPredFlag)) {
    UNLOCKPE(38, pred);
    return FALSE;
  }
#if THREADS
  if (pred->PredFlags & ThreadLocalPredFlag) {
    pred->OpcodeOfPred = Yap_opcode(_thread_local);
    pred->PredFlags ^= SpiedPredFlag;
    UNLOCKPE(39, pred);
    return TRUE;
  }
#endif
  if (!(pred->PredFlags & (CountPredFlag | ProfiledPredFlag))) {
    if (!(pred->PredFlags & DynamicPredFlag)) {
#if defined(YAPOR) || defined(THREADS)
      if (pred->PredFlags & LogUpdatePredFlag &&
          !(pred->PredFlags & ThreadLocalPredFlag) &&
          pred->ModuleOfPred != IDB_MODULE) {
        pred->OpcodeOfPred = LOCKPRED_OPCODE;
        pred->CodeOfPred = (yamop *)(&(pred->OpcodeOfPred));
      } else {
#endif
        pred->CodeOfPred = pred->cs.p_code.TrueCodeOfPred;
        pred->OpcodeOfPred = pred->CodeOfPred->opc;
#if defined(YAPOR) || defined(THREADS)
      }
#endif
    } else if (pred->OpcodeOfPred == Yap_opcode(_spy_or_trymark)) {
      pred->OpcodeOfPred = Yap_opcode(_try_and_mark);
    } else {
      UNLOCKPE(39, pred);
      return FALSE;
    }
  }
  pred->PredFlags ^= SpiedPredFlag;
  UNLOCKPE(40, pred);
  return (TRUE);
}

static Int p_creep(USES_REGS1) {
  Atom at;
  PredEntry *pred;

  if (LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP] == TermZip ||
      LOCAL_debugger_state[DEBUG_DEBUG] == TermFalse)
    return true;
  at = AtomCreep;
  pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1), 0));
  CreepCode = pred;
  Yap_signal( YAP_CREEP_SIGNAL);
  return TRUE;
}

static Int p_creep_fail(USES_REGS1) {
  Atom at;
  PredEntry *pred;
  if (LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP] == TermZip ||
      LOCAL_debugger_state[DEBUG_DEBUG] == TermFalse)
    return true;
  at = AtomCreep;
  pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1), 0));
  CreepCode = pred;
  Yap_signal(YAP_CREEP_SIGNAL);
  return FALSE;
}

static Int stop_creeping(USES_REGS1) {
  LOCAL_debugger_state[DEBUG_DEBUG] = TermFalse;
  Yap_get_signal(YAP_CREEP_SIGNAL);
    return Yap_unify(ARG1, TermTrue);
}

static Int disable_debugging(USES_REGS1) {
  Yap_get_signal(YAP_CREEP_SIGNAL);
  return true;
}

static Int creep_allowed(USES_REGS1) {
  if (PP != NULL) {
    Yap_get_signal(YAP_CREEP_SIGNAL);
    return true;
  }
  return false;
}

void Yap_InitDebugFs(void) {
    init_debugger_state();
    Yap_InitCPred("creep", 0, p_creep, SafePredFlag);
   Yap_InitCPred("$creep", 0, p_creep, SafePredFlag);
  Yap_InitCPred("$creep_fail", 0, p_creep_fail, SafePredFlag);
  Yap_InitCPred("$stop_creeping", 1, stop_creeping,
                NoTracePredFlag | HiddenPredFlag | SafePredFlag);
  Yap_InitCPred("$set_spy", 2, p_setspy, SyncPredFlag);
  Yap_InitCPred("$rm_spy", 2, p_rmspy, SafePredFlag | SyncPredFlag);
     Yap_InitCPred("$get_debugger_state", 2, get_debugger_state, NoTracePredFlag);
  Yap_InitCPred("$get_debugger_state", 5, get_debugger_state5, NoTracePredFlag);
  Yap_InitCPred("$set_debugger_state", 2, set_debugger_state, NoTracePredFlag);
  Yap_InitCPred("$set_debugger_state", 5, set_debugger_state5, NoTracePredFlag);
  Yap_InitCPred("$is_no_trace", 2, p_is_no_trace, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$set_no_trace", 2, p_set_no_trace,
                TestPredFlag | SafePredFlag);
  Yap_InitCPred("creep_allowed", 0, creep_allowed, 0);
  Yap_InitCPred("$disable_debugging", 0, disable_debugging,
                NoTracePredFlag | HiddenPredFlag | SafePredFlag);
}
