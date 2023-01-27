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

#include "YapFlags.h"

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


static yap_signals ProcessSIGINT(void);


bool Yap_DisableInterrupts(int wid) {
    CACHE_REGS
  LOCAL_InterruptsDisabled = true;
  LOCAL_debugger_state[DEBUG_DEBUG] =     TermFalse;
    LOCAL_InterruptsDisabled = true;
  YAPEnterCriticalSection();
  return true;
}

bool Yap_EnableInterrupts(int wid ) {
    CACHE_REGS
  LOCAL_debugger_state[DEBUG_DEBUG]= getAtomicLocalPrologFlag(DEBUG_FLAG);
  LOCAL_InterruptsDisabled = false;
  if (LOCAL_Signals) {
    if (LOCAL_Signals == SIGINT) {
      yap_signals sig = ProcessSIGINT();
      if (sig)
	Yap_signal(sig PASS_REGS);
      else {
	YAPLeaveCriticalSection();
	return true;
      }
    }
    CreepFlag = Unsigned(LCL0);
  }
  YAPLeaveCriticalSection();
  return true;
}



/*
 * The InteractSIGINT function is called after a normal interrupt had been
 * caught.
 * It allows 6 possibilities: abort, continue, trace, debug, help, exit.
 */
static bool InteractSIGINT(int ch USES_REGS) {
  bool rc = true;
  
#ifdef HAVE_SETBUF
  /* make sure we are not waiting for the end of line */
  setbuf(stdin, NULL);
#endif
  switch (ch) {
  case 'a':
/* abort computation */
    Yap_ThrowExistingError(); 
    return true;
    break;
  case 'b':
    /* continue */
    Yap_signal(YAP_BREAK_SIGNAL);
    return true;
  case 'c':
    /* continue */
    return true;
  case 'd':                                             
    /* enter debug 1mode */
     Yap_signal(YAP_DEBUG_SIGNAL);
    return true;
  case 'e':
    /* exit */
    Yap_exit(1);
    break;
  case 'g':
     Yap_signal(YAP_STACK_DUMP_SIGNAL);
    return true;
 case 't':
   LOCAL_Flags[DEBUG_FLAG].at = TermTrue;
   LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP] = TermCreep;
   LOCAL_debugger_state[DEBUG_SPY] = TermTrue;
   LOCAL_debugger_state[DEBUG_TRACE] = TermTrue;
   LOCAL_debugger_state[DEBUG_DEBUG] = TermTrue;
   /* start tracing */
    Yap_signal(YAP_CREEP_SIGNAL);
    break;
#ifdef LOW_LEVEL_TRACER
  case 'T':
    toggle_low_level_trace();
          return true;

#endif
  case 's':
     Yap_signal( YAP_STATISTICS_SIGNAL);
    return true;
  case EOF:
    clearerr(stdin);
    return false;
    break;
  case 'h':
  case '?':
  default:
    /* show an helpful message */
    fprintf(stderr, "Please press one of:\n");
    fprintf(stderr, "  a for abort\n  c for continue\n  d for debug\n");
    fprintf(stderr, "  e for exit\n  g for stack dump\n  s for statistics\n  t "
                    "for trace\n");
    fprintf(stderr, "  b for break\n");
    rc = false;
  }
    return rc;
}

int Yap_GetCharForSIGINT(void) {
    if (trueGlobalPrologFlag(READLINE_FLAG)) {
      extern int Yap_ReadlineForSIGINT(void);
    return  Yap_ReadlineForSIGINT();
    } else 
     {
       const char *s;
    char line[1025];
    do {
      // for a new line
      fprintf(stderr, "\n");
      fputs("Please press key (h help): ", stderr);
      fflush(NULL);
      s = fgets( line, 1024, stdin);
    } while (s==NULL || s[0] == '\0');
    return line[0];
     }
      
}

/**
  This function interacts with the user about a signal×. We assume we are in
  the context of the main Prolog thread (trivial in Unix, but hard in WIN32).


 */
static yap_signals ProcessSIGINT(void) {
  CACHE_REGS
    int ch;
  yap_signals out;
    Yap_EnableInterrupts(worker_id);
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
  //Yap_do_low_level_trace=1;
  ch = Yap_GetCharForSIGINT();
#if 0
      fprintf(stderr,"ch=%c %d %lx\n",ch,LOCAL_InterruptsDisabled,LOCAL_Signals);
#endif
      out = false;
      while (!out)
	{
	  out = InteractSIGINT(ch PASS_REGS);
	}
  LOCAL_PrologMode &= ~AsyncIntMode;
  if (  LOCAL_PrologMode & ConsoleGetcMode) {
    LOCAL_PrologMode &= ~ConsoleGetcMode;
      Yap_ThrowError(INTERRUPT_EVENT,MkIntegerTerm(ch       ), NULL);
  }
  return out;
}


inline static void do_signal(int wid, yap_signals sig USES_REGS) {
#if THREADS
    __sync_fetch_and_or(&REMOTE(wid)->Signals, SIGNAL_TO_BIT(sig));
  if (!REMOTE_InterruptsDisabled(wid)) {
    REMOTE_ThreadHandle(wid).current_yaam_regs->CreepFlag_ =
        Unsigned(REMOTE_ThreadHandle(wid).current_yaam_regs->LCL0_);
  }
#else
  if (LOCAL_InterruptsDisabled) {
      if (sig) {
        LOCAL_Signals |= SIGNAL_TO_BIT(sig);
      }
  } else {
      if (sig == SIGINT) {
	sig = ProcessSIGINT();
      }
      if (sig) {
        LOCAL_Signals |= SIGNAL_TO_BIT(sig);
        CreepFlag = Unsigned(LCL0);
      }
    }
#endif
}


inline static bool get_signal(yap_signals sig USES_REGS) {
#if THREADS
  uint64_t old;
  // reset the flag
  if ((old = __sync_fetch_and_and(&LOCAL_Signals, ~SIGNAL_TO_BIT(sig))) !=
      SIGNAL_TO_BIT(sig)) {
    if ( LOCAL_Signals != 0 && !LOCAL_DisableInterrupts) {
      CreepFlag = (CELL)LCL0;
  // first, clear the Creep Flag, now if someone sets it it is their problem
  CalculateStackGap(PASS_REGS1);
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
  if ( !LOCAL_InterruptsDisabled &&
       LOCAL_Signals & SIGNAL_TO_BIT(sig)) {
    LOCAL_Signals &= ~SIGNAL_TO_BIT(sig);
    if (LOCAL_Signals != 0) {
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

/**
  Function called to handle delayed interrupts.
 */
bool Yap_HandleSIGINT(void) {
  CACHE_REGS
  yap_signals sig;
  Yap_EnableInterrupts(worker_id);
    if ((sig = ProcessSIGINT()) != YAP_NO_SIGNAL) {
      printf("sig=%d\n", sig);
	     
      LOCAL_PrologMode &= ~InterruptMode;
    }
      return true;
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

static Term sig_to_term(yap_signals sig)
{
  Atom at;
  switch (sig) {
  case YAP_INT_SIGNAL:
    return TermSigInt;
      case YAP_ABORT_SIGNAL:
    return TermDAbort;
  case YAP_CREEP_SIGNAL:
    return TermSigCreep;
      case YAP_TRACE_SIGNAL:
    return TermSigTrace;
      case YAP_DEBUG_SIGNAL:
    return TermSigDebug;
  case YAP_BREAK_SIGNAL:
	return TermSigBreak;
  case YAP_FAIL_SIGNAL:
    return TermFail;
  case YAP_STACK_DUMP_SIGNAL:
    return TermSigStackDump;
      case YAP_STATISTICS_SIGNAL:
    return TermSigStatistics;
    #ifdef SIGALRM
  case YAP_ALARM_SIGNAL:
#endif
  case YAP_WINTIMER_SIGNAL:
    return TermSigAlarm;
    #ifdef SIGVTALRM
  case YAP_VTALARM_SIGNAL:
    return TermSigVTAlarm;
    #endif
  case YAP_EXIT_SIGNAL:
    Yap_exit(1);
    return FALSE;
  case YAP_WAKEUP_SIGNAL:
    return TermSigWakeUp;
      case YAP_ITI_SIGNAL:
    return TermSigIti;
    #ifdef SIGPIPE
  case YAP_PIPE_SIGNAL:
    return TermSigPipe;
    #endif
#ifdef SIGHUP
  case YAP_HUP_SIGNAL:
    return TermSigHup;
    #endif
#ifdef SIGUSR1
  case YAP_USR1_SIGNAL:
    return TermSigUsr1;
#endif
#ifdef SIGUSR2
  case YAP_USR2_SIGNAL:
    return TermSigUsr2;
#endif
#ifdef SIGFPE
  case YAP_FPE_SIGNAL:
    return TermSigFPE;
#endif
  default:
    return 0;
  }
  return MkAtomTerm(at);
}


Term Yap_next_signal( USES_REGS1 )
{
  yap_signals sig;
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
  if (get_signal(sig PASS_REGS))
    return sig_to_term(sig);
  return 0;
}




static Int first_signal(USES_REGS1) {
  Term t;

  while((t = Yap_next_signal(PASS_REGS1))) {
    if (t == TermSigInt) {
      yap_signals sig = ProcessSIGINT();
      if (sig == YAP_INT_SIGNAL) {
	break;
      }
      if (sig != YAP_NO_SIGNAL)
	continue;
    return FALSE;
    } else if (t == TermDAbort) {
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
    } else {
      break;
    }
  }
  return Yap_unify(ARG1, t);
}

static Int continue_signals(USES_REGS1) { return first_signal(PASS_REGS1); }

void Yap_InitSignalCPreds(void) {
  /* Basic predicates for the debugger */
  Yap_InitCPred("$first_signal", 1, first_signal, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$continue_signals", 0, continue_signals,
                SafePredFlag | SyncPredFlag);
#ifdef DEBUG
  Yap_InitCPred("sys_debug", 1, p_debug, SafePredFlag | SyncPredFlag);
#endif
}

void * Yap_InitSignals(int wid) {
  void *ptr = (void *)malloc(sizeof(UInt) * REMOTE_MaxActiveSignals(wid));
  return ptr;
}
