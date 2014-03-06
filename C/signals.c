/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V. Santos Costa and Universidade do Porto 1985--	 *
*									 *
**************************************************************************
*									 *
* File:		signal.c						 *
* comments:	Signal Handling & Debugger Support			 *
*									 *
*									 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

#define HAS_CACHE_REGS 1

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "eval.h"
#include "yapio.h"
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#include <stdio.h>
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <wchar.h>

#ifndef THREADS
#define worker_id 0
#endif

inline static void
do_signal(int wid, yap_signals sig USES_REGS)
{
#if THREADS
  LOCK(REMOTE_SignalLock(wid));
  if (!REMOTE_InterruptsDisabled(wid)) {
    REMOTE_ThreadHandle(wid).current_yaam_regs->CreepFlag_ = 
      Unsigned(REMOTE_ThreadHandle(wid).current_yaam_regs->LCL0_);
    if (sig != YAP_CREEP_SIGNAL)
      REMOTE_ThreadHandle(wid).current_yaam_regs->EventFlag_ = 
	Unsigned(REMOTE_ThreadHandle(wid).current_yaam_regs->LCL0_);
  }
  UInt i = REMOTE_FirstActiveSignal(wid);
  if (REMOTE_FirstActiveSignal(wid) != REMOTE_LastActiveSignal(wid)) {
    do {
      if (sig == REMOTE_ActiveSignals(wid)[i]) {
	UNLOCK(REMOTE_SignalLock(wid));
	return;
      }
      i++;
      if (i == REMOTE_MaxActiveSignals(wid))
	i = 0;
    } while (i != REMOTE_LastActiveSignal(wid));
  }
  REMOTE_ActiveSignals(wid)[i] = sig;
  REMOTE_LastActiveSignal(wid)++;
  if (REMOTE_LastActiveSignal(wid) == REMOTE_MaxActiveSignals(wid))
      REMOTE_LastActiveSignal(wid) = 0;
  UNLOCK(REMOTE_SignalLock(wid));
#else
  LOCK(LOCAL_SignalLock);
  if (!LOCAL_InterruptsDisabled) {
    Yap_regp->CreepFlag_ = 
      Unsigned(Yap_regp->LCL0_);
    if (sig != YAP_CREEP_SIGNAL)
      Yap_regp->EventFlag_ = 
	Unsigned(Yap_regp->LCL0_);
  }
  UInt i = LOCAL_FirstActiveSignal;
  if (LOCAL_FirstActiveSignal != LOCAL_LastActiveSignal) {
    do {
      if (sig == LOCAL_ActiveSignals[i]) {
	UNLOCK(LOCAL_SignalLock);
	return;
      }
      i++;
      if (i == LOCAL_MaxActiveSignals)
	i = 0;
    } while (i != LOCAL_LastActiveSignal);
  }
  LOCAL_ActiveSignals[i] = sig;
  LOCAL_LastActiveSignal++;
  if (LOCAL_LastActiveSignal == LOCAL_MaxActiveSignals)
      LOCAL_LastActiveSignal = 0;
  UNLOCK(LOCAL_SignalLock);
#endif
}

inline static int
undo_signal(yap_signals sig USES_REGS)
{
  LOCK(LOCAL_SignalLock);
  UInt i = LOCAL_FirstActiveSignal;
  if (LOCAL_FirstActiveSignal != LOCAL_LastActiveSignal) {
    do {
      if (sig == LOCAL_ActiveSignals[i])
	break;
      i++;
      if (i == LOCAL_MaxActiveSignals)
	i = 0;
    } while (i != LOCAL_LastActiveSignal);
  }
  if (i == LOCAL_LastActiveSignal) {
    UNLOCK(LOCAL_SignalLock);
    return FALSE;
  }
  while ((i+1) % LOCAL_MaxActiveSignals != LOCAL_LastActiveSignal) {
    LOCAL_ActiveSignals[i] = LOCAL_ActiveSignals[(i+1) % LOCAL_MaxActiveSignals];
    i++;
  }
  if (LOCAL_LastActiveSignal == 0)
    LOCAL_LastActiveSignal = LOCAL_MaxActiveSignals-1;
  else
    LOCAL_LastActiveSignal--;
  if (LOCAL_FirstActiveSignal != LOCAL_LastActiveSignal) {
    CalculateStackGap( PASS_REGS1 );
  }
  UNLOCK(LOCAL_SignalLock);
  return TRUE;
}

static Int 
p_creep( USES_REGS1 )
{
  Atom            at;
  PredEntry      *pred;

  at = AtomCreep;
  pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1),0));
  CreepCode = pred;
  do_signal(worker_id, YAP_CREEP_SIGNAL PASS_REGS);
  return TRUE;
}

static Int 
p_creep_fail( USES_REGS1 )
{
  Atom            at;
  PredEntry      *pred;

  at = AtomCreep;
  pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1),0));
  CreepCode = pred;
  do_signal(worker_id, YAP_CREEP_SIGNAL PASS_REGS);
  return FALSE;
}

static Int 
p_stop_creeping( USES_REGS1 )
{
  undo_signal( YAP_CREEP_SIGNAL PASS_REGS );
  return TRUE;
}

static Int
p_creep_allowed( USES_REGS1 )
{
  if (PP != NULL) {
    undo_signal(YAP_CREEP_SIGNAL PASS_REGS);
    LOCK(LOCAL_SignalLock);
    if (!LOCAL_InterruptsDisabled) {
      if (LOCAL_FirstActiveSignal == LOCAL_LastActiveSignal)
	CalculateStackGap( PASS_REGS1 );
      UNLOCK(LOCAL_SignalLock);
    } else {
      UNLOCK(LOCAL_SignalLock);
    }
    return TRUE;
  }
  UNLOCK(LOCAL_SignalLock);
  return FALSE;
}

void 
Yap_signal(yap_signals sig)
{
  CACHE_REGS
    do_signal(worker_id, sig PASS_REGS);
}

void 
Yap_external_signal(int wid, yap_signals sig)
{
#if THREADS
  REGSTORE *regcache = REMOTE_ThreadHandle(wid).current_yaam_regs;
#endif
  do_signal(wid, sig PASS_REGS);
}

int
Yap_undo_signal__(yap_signals sig USES_REGS)
{
  return undo_signal(sig PASS_REGS);
}

int 
Yap_has_signal__(yap_signals sig USES_REGS)
{
  LOCK(LOCAL_SignalLock);
  UInt i = LOCAL_FirstActiveSignal;
  if (LOCAL_FirstActiveSignal != LOCAL_LastActiveSignal) {
    do {
      if (sig == LOCAL_ActiveSignals[i]) {
	UNLOCK(LOCAL_SignalLock);
	return TRUE;
      }
      i++;
      if (i == LOCAL_MaxActiveSignals)
	i = 0;
    } while (i != LOCAL_LastActiveSignal);
  }
  UNLOCK(LOCAL_SignalLock);
  return FALSE;
}

int 
Yap_has_signals__(yap_signals sig1, yap_signals sig2 USES_REGS)
{
  LOCK(LOCAL_SignalLock);
  UInt i = LOCAL_FirstActiveSignal;
  if (LOCAL_FirstActiveSignal != LOCAL_LastActiveSignal) {
    do {
      if (sig1 == LOCAL_ActiveSignals[i] ||
	  sig2 == LOCAL_ActiveSignals[i]) {
	UNLOCK(LOCAL_SignalLock);
	return TRUE;
      }
      i++;
      if (i == LOCAL_MaxActiveSignals)
	i = 0;
    } while (i != LOCAL_LastActiveSignal);
  }
  UNLOCK(LOCAL_SignalLock);
  return FALSE;
}


int 
Yap_only_has_signal__(yap_signals sig  USES_REGS)
{
  LOCK(LOCAL_SignalLock);
  UInt i = LOCAL_FirstActiveSignal;
  if (LOCAL_FirstActiveSignal != LOCAL_LastActiveSignal) {
    do {
      if (sig != LOCAL_ActiveSignals[i]) {
	UNLOCK(LOCAL_SignalLock);
	return FALSE;
      }
      i++;
      if (i == LOCAL_MaxActiveSignals)
	i = 0;
    } while (i != LOCAL_LastActiveSignal);
  } else {
    UNLOCK(LOCAL_SignalLock);
    return FALSE;
  }
  UNLOCK(LOCAL_SignalLock);
  return TRUE;
}

int 
Yap_only_has_signals__(yap_signals sig1, yap_signals sig2 USES_REGS)
{
  LOCK(LOCAL_SignalLock);
  UInt i = LOCAL_FirstActiveSignal;
  if (LOCAL_FirstActiveSignal != LOCAL_LastActiveSignal) {
    do {
      if (sig1 != LOCAL_ActiveSignals[i] &&
	  sig2 != LOCAL_ActiveSignals[i]) {
	UNLOCK(LOCAL_SignalLock);
	return FALSE;
      }
      i++;
      if (i == LOCAL_MaxActiveSignals)
	i = 0;
    } while (i != LOCAL_LastActiveSignal);
  } else {
    UNLOCK(LOCAL_SignalLock);
    return FALSE;
  }
  UNLOCK(LOCAL_SignalLock);
  return TRUE;
}

#ifdef DEBUG
static Int 
p_debug( USES_REGS1 )
{				/* $debug(+Flag) */
  int             i = IntOfTerm(Deref(ARG1));

  if (i >= 'a' && i <= 'z')
    GLOBAL_Option[i - 96] = !GLOBAL_Option[i - 96];
  return (1);
}
#endif

static Int
p_first_signal( USES_REGS1 )
{
  Atom at;
  yap_signals sig;

  LOCK(LOCAL_SignalLock);
  MUTEX_LOCK(&(LOCAL_ThreadHandle.tlock));
  /* always do wakeups first, because you don't want to keep the
     non-backtrackable variable bad */
  if (LOCAL_FirstActiveSignal != LOCAL_LastActiveSignal) {
    sig = LOCAL_ActiveSignals[LOCAL_FirstActiveSignal];
    LOCAL_FirstActiveSignal++;
    if (LOCAL_FirstActiveSignal == LOCAL_MaxActiveSignals)
      LOCAL_FirstActiveSignal = 0;
  } else {
    sig = YAP_NO_SIGNAL;
  }
  switch (sig) {
  case YAP_INT_SIGNAL:
    at = AtomSigInt;
    break;
  case YAP_CREEP_SIGNAL:
    at = AtomSigCreep;
    break;
  case YAP_TRACE_SIGNAL:
    at = AtomSigTrace;
    break;
  case YAP_DEBUG_SIGNAL:
    at = AtomSigDebug;
    break;
  case YAP_BREAK_SIGNAL:
    at = AtomSigBreak;
    break;
  case YAP_FAIL_SIGNAL:
    at = AtomFail;
    break;
  case YAP_STACK_DUMP_SIGNAL:
    at = AtomSigStackDump;
    break;
  case YAP_STATISTICS_SIGNAL:
    at = AtomSigStatistics;
    break;
#ifdef SIGALRM
  case YAP_ALARM_SIGNAL:
#endif
  case YAP_WINTIMER_SIGNAL:
    at = AtomSigAlarm;
    break;
#ifdef SIGVTALRM
  case YAP_VTALARM_SIGNAL:
    at = AtomSigVTAlarm;
    break;
#endif
  case YAP_WAKEUP_SIGNAL:
    at = AtomSigWakeUp;
    break;
  case YAP_ITI_SIGNAL:
    at = AtomSigIti;
    break;
#ifdef SIGPIPE
  case YAP_PIPE_SIGNAL:
    at = AtomSigPipe;
    break;
#endif
#ifdef SIGHUP
  case YAP_HUP_SIGNAL:
    at = AtomSigHup;
    break;
#endif
#ifdef SIGUSR1
  case YAP_USR1_SIGNAL:
    at = AtomSigUsr1;
    break;
#endif
#ifdef SIGUSR2
  case YAP_USR2_SIGNAL:
    at = AtomSigUsr2;
    break;
#endif
  default:
    MUTEX_UNLOCK(&(LOCAL_ThreadHandle.tlock));
    UNLOCK(LOCAL_SignalLock);
    return FALSE;
  }
  UNLOCK(LOCAL_SignalLock);
  return Yap_unify(ARG1, MkAtomTerm(at));
}

static Int
p_continue_signals( USES_REGS1 )
{
  yap_signals sig;
  /* hack to force the signal anew */
  if (LOCAL_FirstActiveSignal != LOCAL_LastActiveSignal) {
    sig = LOCAL_ActiveSignals[LOCAL_FirstActiveSignal];
    Yap_signal(sig);
  }
  MUTEX_UNLOCK(&(LOCAL_ThreadHandle.tlock));
  return TRUE;
}

void
Yap_InitSignalCPreds(void)
{
  /* Basic predicates for the debugger */
  Yap_InitCPred("$creep", 0, p_creep, SafePredFlag);
  Yap_InitCPred("$creep_fail", 0, p_creep_fail, SafePredFlag);
  Yap_InitCPred("$stop_creeping", 0, p_stop_creeping, SafePredFlag);
  Yap_InitCPred ("$first_signal", 1, p_first_signal, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$continue_signals", 0, p_continue_signals, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$creep_allowed", 0, p_creep_allowed, 0);
#ifdef DEBUG
  Yap_InitCPred("$debug", 1, p_debug, SafePredFlag|SyncPredFlag);
#endif
}

void Yap_InitSignals(int wid)
{
  REMOTE_ActiveSignals(wid) = (UInt *)malloc(sizeof(UInt)*REMOTE_MaxActiveSignals(wid));
}
