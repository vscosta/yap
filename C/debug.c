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

/** '$may_set_spy_point(P)
 * 
 * may be set as a debugging point,
*/
static Int may_set_spy_point(USES_REGS1) { 
  PredEntry *pe;
  bool out;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "$set_spy_point/2");
  if (EndOfPAEntr(pe))
    return false;
  out =! (pe->PredFlags & AsmPredFlag );
  return out;
}
static Int p_setspy(USES_REGS1) { /* '$set_spy'(+Fun,+M)	 */
  PredEntry *pe, *pred;
  pred_flags_t fg;
  Term t;

  t = Deref(ARG1);
  pe =
    Yap_get_pred(t, Deref(ARG2), "$set_spy_point/2");
  if (EndOfPAEntr(pe))
    return false;
  Atom at = AtomSpy;
  pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1), 0));
  SpyCode = pred;
  if (pe->PredFlags & (CPredFlag | SafePredFlag)) {
    UNLOCKPE(35, pe);
    return false;
  }
  if (pe->OpcodeOfPred == UNDEF_OPCODE || pe->OpcodeOfPred == FAIL_OPCODE) {
    UNLOCKPE(36, pe);
    return false;
  }
  while (pe->OpcodeOfPred == INDEX_OPCODE) {
    int i = 0;
    for (i = 0; i < pe->ArityOfPE; i++) {
      XREGS[i + 1] = MkVarTerm();
    }
    Yap_IPred(pe, 0, CP);
  }
  fg = pe->PredFlags;
  if (fg & DynamicPredFlag) {
    pe->OpcodeOfPred = ((yamop *)(pe->CodeOfPred))->opc =
        Yap_opcode(_spy_or_trymark);
  } else {
    pe->OpcodeOfPred = Yap_opcode(_spy_pred);
    pe->CodeOfPred = (yamop *)(&(pe->OpcodeOfPred));
  }
  pe->PredFlags |= SpiedPredFlag;
  UNLOCKPE(37, pe);
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

bool Yap_may_creep(bool creep_on_forward)
{
CACHE_REGS
  Atom at;
  PredEntry *pred;
  if (  LOCAL_DebEvent) {
    LOCAL_DebEvent = false;
    if (creep_on_forward) {
      at = AtomCreep;
    } else {
      at = AtomCreep;
    }
    pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1), 0));
    CreepCode = pred;
    Yap_signal( YAP_CREEP_SIGNAL);
    return true;
  }
  Yap_SetGlobalVal(AtomCreep, TermZip);
  return false;
}

static Int p_creep(USES_REGS1) {
  LOCAL_DebEvent = true;
  return true;
}

static Int creepfail(USES_REGS1) {
  return true;
}

static Int stop_creeping(USES_REGS1) {
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
    Yap_InitCPred("creep", 0, p_creep, SafePredFlag);
   Yap_InitCPred("$creep", 0, p_creep, SafePredFlag);
  Yap_InitCPred("$creep_fail", 0, creepfail, SafePredFlag);
  Yap_InitCPred("$stop_creeping", 1, stop_creeping,
                NoTracePredFlag | HiddenPredFlag | SafePredFlag);
  Yap_InitCPred("$may_set_spy_point",2, may_set_spy_point, SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("$set_spy", 2, p_setspy, SyncPredFlag);
  Yap_InitCPred("$rm_spy", 2, p_rmspy, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$is_no_trace", 2, p_is_no_trace, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$set_no_trace", 2, p_set_no_trace,
                TestPredFlag | SafePredFlag);
  Yap_InitCPred("creep_allowed", 0, creep_allowed, 0);
  Yap_InitCPred("$disable_debugging", 0, disable_debugging,
                NoTracePredFlag | HiddenPredFlag | SafePredFlag);
}
