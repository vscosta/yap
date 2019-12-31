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
 * File:		signal.c *
 * comments:	Signal Handling & Debugger Support			 *
 *									 *
 *									 *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

#define HAS_CACHE_REGS 1

#include "Yap.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if _WIN32
#include <io.h>
#include <stdio.h>
#endif
#include "YapEval.h"
#include "YapHeap.h"
#include "Yatom.h"
#include "yapio.h"
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#include <stdio.h>
#include <stdlib.h>
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_STRINGS_H
#include <strings.h>
#endif
#if HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <wchar.h>
#ifdef LOW_LEVEL_TRACER
#include <tracer.h>
#endif

/*
 * The InteractSIGINT function is called after a normal interrupt had been
 * caught.
 * It allows 6 possibilities: abort, continue, trace, debug, help, exit.
 */
static yap_signals InteractSIGINT(int ch) {
#ifdef HAVE_SETBUF
  /* make sure we are not waiting for the end of line */
  setbuf(stdin, NULL);
#endif
  switch (ch) {
  case 'a':
/* abort computation */
#if PUSH_REGS
  // restore_absmi_regs(&Yap_standard_regs);
#endif
    return YAP_ABORT_SIGNAL;
  case 'b':
    /* continue */
    return YAP_BREAK_SIGNAL;
  case 'c':
    /* continue */
    return YAP_NO_SIGNAL;
  case 'd':
    /* enter debug mode */
    return YAP_DEBUG_SIGNAL;
  case 'e':
    /* exit */
    Yap_exit(1);
    return YAP_EXIT_SIGNAL;
  case 'g':
    /* stack dump */
    return YAP_STACK_DUMP_SIGNAL;
  case 't':
    /* start tracing */
    return YAP_TRACE_SIGNAL;
#ifdef LOW_LEVEL_TRACER
  case 'T':
    toggle_low_level_trace();
    return YAP_NO_SIGNAL;
#endif
  case 's':
    /* show some statistics */
    return YAP_STATISTICS_SIGNAL;
  case EOF:
    return YAP_NO_SIGNAL;
  case 'h':
  case '?':
  default:
    /* show an helpful message */
    fprintf(stderr, "Please press one of:\n");
    fprintf(stderr, "  a for abort\n  c for continue\n  d for debug\n");
    fprintf(stderr, "  e for exit\n  g for stack dump\n  s for statistics\n  t "
                    "for trace\n");
    fprintf(stderr, "  b for break\n");
    return YAP_NO_SIGNAL;
  }
}

/**
  This function interacts with the user about a signal. We assume we are in
  the context of the main Prolog thread (trivial in Unix, but hard in WIN32).


 */
static yap_signals ProcessSIGINT(void) {
  CACHE_REGS
  int ch, out;
#if _WIN32
  if (!_isatty(0)) {
    return YAP_INT_SIGNAL;
  }
#elif HAVE_ISATTY
  if (!isatty(0)) {
    return YAP_INT_SIGNAL;
  }
#endif
  LOCAL_PrologMode |= AsyncIntMode;
  do {
    ch = Yap_GetCharForSIGINT();
  } while (!(out = InteractSIGINT(ch)));
  LOCAL_PrologMode &= ~AsyncIntMode;
  return (out);
}

inline static void do_signal(int wid, yap_signals sig USES_REGS) {
#if THREADS
  __sync_fetch_and_or(&REMOTE(wid)->Signals_, SIGNAL_TO_BIT(sig));
  if (!REMOTE_InterruptsDisabled(wid)) {
    REMOTE_ThreadHandle(wid).current_yaam_regs->CreepFlag_ =
        Unsigned(REMOTE_ThreadHandle(wid).current_yaam_regs->LCL0_);
  }
#else
  LOCAL_Signals |= SIGNAL_TO_BIT(sig);
  if (!LOCAL_InterruptsDisabled) {
    CreepFlag = Unsigned(LCL0);
  }
#endif
}

inline static bool get_signal(yap_signals sig USES_REGS) {
#if THREADS
  uint64_t old;

  // first, clear the Creep Flag, now if someone sets it it is their problem
  CalculateStackGap(PASS_REGS1);
  // reset the flag
  if ((old = __sync_fetch_and_and(&LOCAL_Signals, ~SIGNAL_TO_BIT(sig))) !=
      SIGNAL_TO_BIT(sig)) {
    if (!LOCAL_InterruptsDisabled && LOCAL_Signals != 0) {
      CreepFlag = (CELL)LCL0;
    }
    if (!(old & SIGNAL_TO_BIT(sig))) {
      // not there?
      return FALSE;
    }
    // more likely case, we have other interrupts.
    return TRUE;
  }
  // success, we are good
  return TRUE;
// should we set the flag?
#else
  if (LOCAL_Signals & SIGNAL_TO_BIT(sig)) {
    LOCAL_Signals &= ~SIGNAL_TO_BIT(sig);
    if (!LOCAL_InterruptsDisabled && LOCAL_Signals != 0) {
      CreepFlag = (CELL)LCL0;
    } else {
      CalculateStackGap(PASS_REGS1);
    }
    return TRUE;
  } else {
    return FALSE;
  }
#endif
}

bool Yap_DisableInterrupts(int wid) {
  LOCAL_InterruptsDisabled = true;
  YAPEnterCriticalSection();
  return true;
}

bool Yap_EnableInterrupts(int wid) {
  LOCAL_InterruptsDisabled = false;
  YAPLeaveCriticalSection();
  return true;
}

/**
  Function called to handle delayed interrupts.
 */
bool Yap_HandleSIGINT(void) {
  CACHE_REGS
  yap_signals sig;

  do {
    if ((sig = ProcessSIGINT()) != YAP_NO_SIGNAL)
      do_signal(worker_id, sig PASS_REGS);
    LOCAL_PrologMode &= ~InterruptMode;
    return true;
  } while (get_signal(YAP_INT_SIGNAL PASS_REGS));
  return false;
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
  do_signal(worker_id, YAP_CREEP_SIGNAL PASS_REGS);
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
  do_signal(worker_id, YAP_CREEP_SIGNAL PASS_REGS);
  return FALSE;
}

static Int stop_creeping(USES_REGS1) {
  LOCAL_debugger_state[DEBUG_DEBUG] = TermFalse;
  if (get_signal(YAP_CREEP_SIGNAL PASS_REGS)) {
    return Yap_unify(ARG1, TermTrue);
  }
  return Yap_unify(ARG1, TermFalse);
}

static Int disable_debugging(USES_REGS1) {
  get_signal(YAP_CREEP_SIGNAL PASS_REGS);
  return true;
}

static Int creep_allowed(USES_REGS1) {
  if (PP != NULL) {
    get_signal(YAP_CREEP_SIGNAL PASS_REGS);
    return true;
  }
  return false;
}

void Yap_signal(yap_signals sig) {
  CACHE_REGS
  do_signal(worker_id, sig PASS_REGS);
}


#ifdef DEBUG
static Int p_debug(USES_REGS1);
#endif

void Yap_external_signal(int wid, yap_signals sig) {
#if THREADS
  REGSTORE *regcache = REMOTE_ThreadHandle(wid).current_yaam_regs;
#endif
  do_signal(wid, sig PASS_REGS);
  LOCAL_PrologMode &= ~InterruptMode;
}

int Yap_get_signal__(yap_signals sig USES_REGS) {
  return get_signal(sig PASS_REGS);
}

// the caller holds the lock.
int Yap_has_signals__(yap_signals sig1, yap_signals sig2 USES_REGS) {
  return LOCAL_Signals & (SIGNAL_TO_BIT(sig1) | SIGNAL_TO_BIT(sig2));
}

int Yap_only_has_signals__(yap_signals sig1, yap_signals sig2 USES_REGS) {
  uint64_t sigs = LOCAL_Signals;
  return sigs & (SIGNAL_TO_BIT(sig1) | SIGNAL_TO_BIT(sig2)) &&
         !(sigs & ~(SIGNAL_TO_BIT(sig1) | SIGNAL_TO_BIT(sig2)));
}

#ifdef DEBUG

volatile int volat = 0;

static Int p_debug(USES_REGS1) { /* $debug(+Flag) */
  int i = IntOfTerm(Deref(ARG1));
  while (volat == 0) {
  }
  if (i >= 'a' && i <= 'z')
    GLOBAL_Option[i - 96] = !GLOBAL_Option[i - 96];
  return 1;
}
void Yap_loop(void);
void Yap_debug_end_loop(void);

void Yap_loop(void) {
  while (volat == 0)
    ;
}

void Yap_debug_end_loop(void) { volat = 1; }
#endif

static Int first_signal(USES_REGS1) {
  Atom at;
  yap_signals sig;

  while (TRUE) {
    uint64_t mask = LOCAL_Signals;
    if (mask == 0)
      return FALSE;
#if HAVE___BUILTIN_FFSLL
    sig = __builtin_ffsll(mask);
#elif HAVE_FFSLL
    sig = ffsll(mask);
#else
    sig = Yap_msb(mask PASS_REGS) + 1;
#endif
    if (get_signal(sig PASS_REGS)) {
      break;
    }
  }
loop:
  switch (sig) {
  case YAP_INT_SIGNAL:
    sig = ProcessSIGINT();
    if (sig == YAP_INT_SIGNAL) {
      at = AtomSigInt;
      break;
    }
    if (sig != YAP_NO_SIGNAL)
      goto loop;
    return FALSE;
  case YAP_ABORT_SIGNAL:
    /* abort computation */
    LOCAL_PrologMode &= ~AsyncIntMode;
    if (LOCAL_PrologMode & (GCMode | ConsoleGetcMode | CritMode)) {
      LOCAL_PrologMode |= AbortMode;
      return -1;
    } else {
      Yap_Error(ABORT_EVENT, TermNil, "abort from console");
    }
    Yap_RestartYap(1);
    return FALSE;
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
  case YAP_EXIT_SIGNAL:
    Yap_exit(1);
    return FALSE;
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
#ifdef SIGFPE
  case YAP_FPE_SIGNAL:
    at = AtomSigFPE;
    break;
#endif
  default:
    return FALSE;
  }
  return Yap_unify(ARG1, MkAtomTerm(at));
}

static Int continue_signals(USES_REGS1) { return first_signal(PASS_REGS1); }

void Yap_InitSignalCPreds(void) {
  /* Basic predicates for the debugger */
  Yap_InitCPred("$creep", 0, p_creep, SafePredFlag);
  Yap_InitCPred("$creep_fail", 0, p_creep_fail, SafePredFlag);
  Yap_InitCPred("$stop_creeping", 1, stop_creeping,
                NoTracePredFlag | HiddenPredFlag | SafePredFlag);
  Yap_InitCPred("$disable_debugging", 0, disable_debugging,
                NoTracePredFlag | HiddenPredFlag | SafePredFlag);
  Yap_InitCPred("$first_signal", 1, first_signal, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$continue_signals", 0, continue_signals,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("creep_allowed", 0, creep_allowed, 0);
#ifdef DEBUG
  Yap_InitCPred("sys_debug", 1, p_debug, SafePredFlag | SyncPredFlag);
#endif
}

void * Yap_InitSignals(int wid) {
  void *ptr = (void *)malloc(sizeof(UInt) * REMOTE_MaxActiveSignals(wid));
  return ptr;
}
