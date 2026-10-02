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

/**
 * @file debug.c
   debugger low level support: set and remove spy points, creeping,..
 */

/**
   @defgroup SpySupport Low-Level Machinery for Debugging

Support Spying, Creeping, and other tracing
   functionality.
 */

   
#ifdef SCCS
static char SccsId[] = "@(#)cdmgr.c	1.1 05/02/98";
#endif

#include "Yap.h"
#include "attvar.h"

/******************************************************************

                MANAGING SPY-POINTS

******************************************************************/

/// @pred $is_no_trace( Goal, Module)
///
/// Check whether we can debug goals for this predicate.
///
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

bool Yap_SetNoTrace(PredEntry *pe) {

  if (!pe)
    return false;
  PELOCK(36, pe);
  pe->PredFlags |= NoTracePredFlag;
  UNLOCKPE(57, pe);
  return TRUE;
}
/// @pred $set_no_ce( Goal, Module)
///
/// debugger will ignore this goal
///
static Int p_set_no_trace(USES_REGS1) { /* '$set_no_trace'(+Fun,+M)	 */
  PredEntry *pe;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "set as untraceable");
  PELOCK(36, pe);
  bool rc =  Yap_SetNoTrace(pe);
  UNLOCKPE(57, pe);
  return rc;
}

static Int set_spy_point(PredEntry *pe)
{    
  if (EndOfPAEntr(pe)) {
    return false;
  }
  pred_flags_t fg = pe->PredFlags;
  if( pe->PredFlags & UndefPredFlag) {
    return false;
  }
  if (fg & DynamicPredFlag) {
    pe->OpcodeOfPred = ((yamop *)(pe->CodeOfPred))->opc =
        Yap_opcode(_spy_or_trymark);
  } else {
    pe->OpcodeOfPred = Yap_opcode(_spy_pred);
    pe->CodeOfPred = (yamop *)(&(pe->OpcodeOfPred));
  }
  pe->PredFlags |= SpiedPredFlag;
  return true;
}


/** '$set_spy_point(G,M)
 *
 * may be set as a debugging point,
 */
static Int p_setspy(USES_REGS1) { /* '$set_spy'(+Fun,+M)	 */
  PredEntry *pe;
  Term t;

  t = Deref(ARG1);
  pe =
    Yap_get_pred(t, Deref(ARG2), "$set_spy_point/2");
  PELOCK(37, pe);
  bool rc = set_spy_point(pe);
  UNLOCKPE(37, pe);
  return rc;
}

/** '$rm_spy_point(G,M)
 *
 * may be set as a debugging point,
 */
static Int p_rmspy(USES_REGS1) { /* '$rm_spy'(+T,+Mod)	 */
  PredEntry *pred;
  Term t;

  t = Deref(ARG1);
  pred =
    Yap_get_pred(t, Deref(ARG2), "$rm_spy_point/2");
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



static bool allow_creep(USES_REGS1) {
  return !LOCAL_DebEvent
	 && !LOCAL_InterruptsDisabled &&
         !(LOCAL_PrologMode & (AbortMode | InterruptMode | SystemMode)) &&
         PP != NULL &&
         (Yap_GetValue(AtomCreep) != TermZip);
}

/**
@pred $crep

creep may happen:
  - when calling a top-level goal;
  - after stopping at a spy point
  - on the continuation
  - on backtracking.
    
*/
bool Yap_creep(bool creep_on_forward USES_REGS)
{
  if (allow_creep(PASS_REGS1) ) {
    Yap_signal( YAP_CREEP_SIGNAL);
  }
  // LOCAL_IsDebugging =false;
  return true;
}

static Int p_creep(USES_REGS1) {

  LOCAL_DebEvent = false;
  return Yap_creep(true);
}

static Int stop_creeping(USES_REGS1) {
  Yap_get_signal(YAP_CREEP_SIGNAL);
  LOCAL_DebEvent = false;
  Yap_PutValue(AtomCreep, TermZip);
  return Yap_unify(ARG1, TermTrue);
}



static Int start_debugger(USES_REGS1) {
  if (LOCAL_DebEvent) {
    fprintf(stderr,"Debugger already on\n");	     
  }
  if (trueLocalPrologFlag(DEBUG_FLAG)) {
    LOCAL_DebEvent = true;
  }
  setBooleanLocalPrologFlag(DEBUG_FLAG,true);
  return true;
}

static Int stop_debugger(USES_REGS1) {
  #if DEBUG
  if (!LOCAL_DebEvent) {
    fprintf(stderr,"Debugger already off\n");	     
  }
  #endif
  setBooleanLocalPrologFlag(DEBUG_FLAG,false);
  LOCAL_DebEvent = false;
  return true;
}


void Yap_InitDebugFs(void) {
    Yap_InitCPred("creep", 0, p_creep, SafePredFlag);
   Yap_InitCPred("$creep", 0, p_creep, SafePredFlag);
  Yap_InitCPred("$start_debugger", 0, start_debugger, SafePredFlag);
  Yap_InitCPred("$stop_debugger", 0, stop_debugger, SafePredFlag);
  Yap_InitCPred("$stop_creeping", 1, stop_creeping,
                NoTracePredFlag | HiddenPredFlag | SafePredFlag);
  Yap_InitCPred("$set_spy", 2, p_setspy, SyncPredFlag);
  Yap_InitCPred("$rm_spy", 2, p_rmspy, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$is_no_trace", 2, p_is_no_trace, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$set_no_trace", 2, p_set_no_trace,
                TestPredFlag | SafePredFlag);
}
