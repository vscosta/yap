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

inline static void
do_signal(yap_signals sig USES_REGS)
{
  LOCK(LOCAL_SignalLock);
  if (!LOCAL_InterruptsDisabled)
    CreepFlag = Unsigned(LCL0);
  LOCAL_ActiveSignals |= sig;
  UNLOCK(LOCAL_SignalLock);
}

inline static void
undo_signal(yap_signals sig USES_REGS)
{
  LOCK(LOCAL_SignalLock);
  if ((LOCAL_ActiveSignals & ~(YAP_CREEP_SIGNAL|YAP_DELAY_CREEP_SIGNAL)) == sig) {
    CreepFlag = CalculateStackGap();
  }
  LOCAL_ActiveSignals &= ~sig;
  UNLOCK(LOCAL_SignalLock);
}

static Int 
p_creep( USES_REGS1 )
{
  Atom            at;
  PredEntry      *pred;

  at = AtomCreep;
  pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1),0));
  CreepCode = pred;
  do_signal(YAP_CREEP_SIGNAL PASS_REGS);
  return TRUE;
}

static Int 
p_stop_creeping( USES_REGS1 )
{
  Atom            at;
  PredEntry      *pred;

  LOCK(LOCAL_SignalLock);
  LOCAL_ActiveSignals &= ~(YAP_CREEP_SIGNAL|YAP_DELAY_CREEP_SIGNAL);
  if (!LOCAL_ActiveSignals) {
    CreepFlag = CalculateStackGap();
  }
  UNLOCK(LOCAL_SignalLock);
  return TRUE;
}

static Int 
p_meta_creep( USES_REGS1 )
{
  Atom            at;
  PredEntry      *pred;

  at = AtomCreep;
  pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1),0));
  CreepCode = pred;
  LOCK(LOCAL_SignalLock);
  LOCAL_ActiveSignals |= YAP_DELAY_CREEP_SIGNAL;
  UNLOCK(LOCAL_SignalLock);
  return TRUE;
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


void 
Yap_signal(yap_signals sig)
{
  CACHE_REGS
  do_signal(sig PASS_REGS);
}

void 
Yap_undo_signal(yap_signals sig)
{
  CACHE_REGS
  undo_signal(sig PASS_REGS);
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
  LOCK(LOCAL_SignalLock);
#ifdef THREADS
  pthread_mutex_lock(&(LOCAL_ThreadHandle.tlock));
#endif  
  /* always do wakeups first, because you don't want to keep the
     non-backtrackable variable bad */
  if (LOCAL_ActiveSignals & YAP_WAKEUP_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_WAKEUP_SIGNAL;
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigWakeUp));
  }
  if (LOCAL_ActiveSignals & YAP_ITI_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_ITI_SIGNAL;
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigIti));
  }
  if (LOCAL_ActiveSignals & YAP_INT_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_INT_SIGNAL;
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigInt));
  }
  if (LOCAL_ActiveSignals & YAP_USR2_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_USR2_SIGNAL;
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigUsr2));
  }
  if (LOCAL_ActiveSignals & YAP_USR1_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_USR1_SIGNAL;
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigUsr1));
  }
  if (LOCAL_ActiveSignals & YAP_PIPE_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_PIPE_SIGNAL;
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigPipe));
  }
  if (LOCAL_ActiveSignals & YAP_HUP_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_HUP_SIGNAL;
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigHup));
  }
  if (LOCAL_ActiveSignals & YAP_ALARM_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_ALARM_SIGNAL;
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigAlarm));
  }
  if (LOCAL_ActiveSignals & YAP_VTALARM_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_VTALARM_SIGNAL;
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigVTAlarm));
  }
  if (LOCAL_ActiveSignals & YAP_DELAY_CREEP_SIGNAL) {
    LOCAL_ActiveSignals &= ~(YAP_CREEP_SIGNAL|YAP_DELAY_CREEP_SIGNAL);
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigDelayCreep));
  }
  if (LOCAL_ActiveSignals & YAP_CREEP_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_CREEP_SIGNAL;
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigCreep));
  }
  if (LOCAL_ActiveSignals & YAP_TRACE_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_TRACE_SIGNAL;
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigTrace));
  }
  if (LOCAL_ActiveSignals & YAP_DEBUG_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_DEBUG_SIGNAL;
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigDebug));
  }
  if (LOCAL_ActiveSignals & YAP_BREAK_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_BREAK_SIGNAL;
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigBreak));
  }
  if (LOCAL_ActiveSignals & YAP_STACK_DUMP_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_STACK_DUMP_SIGNAL;
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigStackDump));
  }
  if (LOCAL_ActiveSignals & YAP_STATISTICS_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_STATISTICS_SIGNAL;
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomSigStatistics));
  }
  if (LOCAL_ActiveSignals & YAP_FAIL_SIGNAL) {
    LOCAL_ActiveSignals &= ~YAP_FAIL_SIGNAL;
#ifdef THREADS
    pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
    UNLOCK(LOCAL_SignalLock);
    return Yap_unify(ARG1, MkAtomTerm(AtomFail));
  }
#ifdef THREADS
  pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
  UNLOCK(LOCAL_SignalLock);
  return FALSE;
}

static Int
p_continue_signals( USES_REGS1 )
{
  /* hack to force the signal anew */
  if (LOCAL_ActiveSignals & YAP_ITI_SIGNAL) {
    Yap_signal(YAP_ITI_SIGNAL);
  }
  if (LOCAL_ActiveSignals & YAP_INT_SIGNAL) {
    Yap_signal(YAP_INT_SIGNAL);
  }
  if (LOCAL_ActiveSignals & YAP_USR2_SIGNAL) {
    Yap_signal(YAP_USR2_SIGNAL);
  }
  if (LOCAL_ActiveSignals & YAP_USR1_SIGNAL) {
    Yap_signal(YAP_USR1_SIGNAL);
  }
  if (LOCAL_ActiveSignals & YAP_HUP_SIGNAL) {
    Yap_signal(YAP_HUP_SIGNAL);
  }
  if (LOCAL_ActiveSignals & YAP_ALARM_SIGNAL) {
    Yap_signal(YAP_ALARM_SIGNAL);
  }
  if (LOCAL_ActiveSignals & YAP_VTALARM_SIGNAL) {
    Yap_signal(YAP_VTALARM_SIGNAL);
  }
  if (LOCAL_ActiveSignals & YAP_CREEP_SIGNAL) {
    Yap_signal(YAP_CREEP_SIGNAL);
  }
  if (LOCAL_ActiveSignals & YAP_DELAY_CREEP_SIGNAL) {
    Yap_signal(YAP_DELAY_CREEP_SIGNAL|YAP_CREEP_SIGNAL);
  }
  if (LOCAL_ActiveSignals & YAP_TRACE_SIGNAL) {
    Yap_signal(YAP_TRACE_SIGNAL);
  }
  if (LOCAL_ActiveSignals & YAP_DEBUG_SIGNAL) {
    Yap_signal(YAP_DEBUG_SIGNAL);
  }
  if (LOCAL_ActiveSignals & YAP_BREAK_SIGNAL) {
    Yap_signal(YAP_BREAK_SIGNAL);
  }
  if (LOCAL_ActiveSignals & YAP_STACK_DUMP_SIGNAL) {
    Yap_signal(YAP_STACK_DUMP_SIGNAL);
  }
  if (LOCAL_ActiveSignals & YAP_STATISTICS_SIGNAL) {
    Yap_signal(YAP_STATISTICS_SIGNAL);
  }
  if (LOCAL_ActiveSignals & YAP_FAIL_SIGNAL) {
    Yap_signal(YAP_FAIL_SIGNAL);
  }
#ifdef THREADS
  pthread_mutex_unlock(&(LOCAL_ThreadHandle.tlock));
#endif  
  return TRUE;
}

void
Yap_InitSignalCPreds(void)
{
  /* Basic predicates for the debugger */
  Yap_InitCPred("$creep", 0, p_creep, SafePredFlag);
  Yap_InitCPred("$meta_creep", 0, p_meta_creep, SafePredFlag);
  Yap_InitCPred("$stop_creeping", 0, p_stop_creeping, SafePredFlag);
  Yap_InitCPred ("$first_signal", 1, p_first_signal, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$continue_signals", 0, p_continue_signals, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$debug_on", 1, p_debug_on, 0);
  Yap_InitCPred("$creep_allowed", 0, p_creep_allowed, 0);
#ifdef DEBUG
  Yap_InitCPred("$debug", 1, p_debug, SafePredFlag|SyncPredFlag);
#endif
}
