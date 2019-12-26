#include "sysbits.h"

#if HAVE_SIGINFO_H
#include <siginfo.h>
#endif
#if HAVE_SYS_UCONTEXT_H
#include <sys/ucontext.h>
#endif

#if HAVE_FPU_CONTROL_H
#include <fpu_control.h>
#endif

#define SIG_PROLOG_OFFSET 32 /* Start of Prolog signals */

#define SIG_EXCEPTION (SIG_PROLOG_OFFSET + 0)
#ifdef O_ATOMGC
#define SIG_ATOM_GC (SIG_PROLOG_OFFSET + 1)
#endif
#define SIG_GC (SIG_PROLOG_OFFSET + 2)
#ifdef O_PLMT
#define SIG_THREAD_SIGNAL (SIG_PROLOG_OFFSET + 3)
#endif
#define SIG_FREECLAUSES (SIG_PROLOG_OFFSET + 4)
#define SIG_PLABORT (SIG_PROLOG_OFFSET + 5)

static struct signame {
  int sig;
  const char *name;
  int flags;
} signames[] = {
#ifdef SIGHUP
    {SIGHUP, "hup", 0},
#endif
    {SIGINT, "int", 0},
#ifdef SIGQUIT
    {SIGQUIT, "quit", 0},
#endif
    {SIGILL, "ill", 0},
    //    {SIGABRT, "abrt", 0},
    {SIGFPE, "fpe", 0},
#ifdef SIGKILL
    {SIGKILL, "kill", 0},
#endif
    {SIGSEGV, "segv", 0},
#ifdef SIGPIPE
    {SIGPIPE, "pipe", 0},
#endif
#ifdef SIGALRM
    {SIGALRM, "alrm", 0},
#endif
    {SIGTERM, "term", 0},
#ifdef SIGUSR1
    {SIGUSR1, "usr1", 0},
#endif
#ifdef SIGUSR2
    {SIGUSR2, "usr2", 0},
#endif
#ifdef SIGCHLD
    {SIGCHLD, "chld", 0},
#endif
#ifdef SIGCONT
    {SIGCONT, "cont", 0},
#endif
#ifdef SIGSTOP
    {SIGSTOP, "stop", 0},
#endif
#ifdef SIGTSTP
    {SIGTSTP, "tstp", 0},
#endif
#ifdef SIGTTIN
    {SIGTTIN, "ttin", 0},
#endif
#ifdef SIGTTOU
    {SIGTTOU, "ttou", 0},
#endif
#ifdef SIGTRAP
    {SIGTRAP, "trap", 0},
#endif
#ifdef SIGBUS
    {SIGBUS, "bus", 0},
#endif
#ifdef SIGSTKFLT
    {SIGSTKFLT, "stkflt", 0},
#endif
#ifdef SIGURG
    {SIGURG, "urg", 0},
#endif
#ifdef SIGIO
    {SIGIO, "io", 0},
#endif
#ifdef SIGPOLL
    {SIGPOLL, "poll", 0},
#endif
#ifdef SIGXCPU
    {SIGXCPU, "xcpu", 0},
#endif
#ifdef SIGXFSZ
    {SIGXFSZ, "xfsz", 0},
#endif
#ifdef SIGVTALRM
    {SIGVTALRM, "vtalrm", 0},
#endif
#ifdef SIGPROF
    {SIGPROF, "prof", 0},
#endif
#ifdef SIGPWR
    {SIGPWR, "pwr", 0},
#endif
    {SIG_EXCEPTION, "prolog:exception", 0},
#ifdef SIG_ATOM_GC
    {SIG_ATOM_GC, "prolog:atom_gc", 0},
#endif
    {SIG_GC, "prolog:gc", 0},
#ifdef SIG_THREAD_SIGNAL
    {SIG_THREAD_SIGNAL, "prolog:thread_signal", 0},
#endif

    {-1, NULL, 0}};

#if HAVE_SIGACTION
static void my_signal_info(int sig, void *handler) {
  struct sigaction sigact;

  sigact.sa_handler = handler;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = SA_SIGINFO;

  sigaction(sig, &sigact, NULL);
}

static void my_signal(int sig, void *handler) {
  struct sigaction sigact;

  sigact.sa_handler = (void *)handler;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = 0;
  sigaction(sig, &sigact, NULL);
}

#else

static void my_signal(int sig, void *handler) {
#if HAVE_SIGNAL
  signal(sig, handler);
#endif
}

static void my_signal_info(int sig, void *handler) {
#if HAVE_SIGNAL
  if (signal(sig, (void *)handler) == SIG_ERR)
    exit(1);
#endif
}

#endif

static void HandleMatherr(int sig, void *sipv, void *uapv) {
  CACHE_REGS
  LOCAL_Error_TYPE = Yap_MathException();
  /* reset the registers so that we don't have trash in abstract machine */
  Yap_external_signal(worker_id, YAP_FPE_SIGNAL);
}

/* SWI emulation */
int Yap_signal_index(const char *name) {
  struct signame *sn = signames;
  char tmp[12];

  if (strncmp(name, "SIG", 3) == 0 && strlen(name) < 12) {
    char *p = (char *)name + 3, *q = tmp;
    while ((*q++ = tolower(*p++))) {
    };
    name = tmp;
  }

  for (; sn->name; sn++) {
    if (!strcmp(sn->name, name))
      return sn->sig;
  }

  return -1;
}

#if HAVE_SIGSEGV
static void SearchForTrailFault(void *ptr, int sure) {

/* If the TRAIL is very close to the top of mmaped allocked space,
   then we can try increasing the TR space and restarting the
   instruction. In the worst case, the system will
   crash again
*/
#if OS_HANDLES_TR_OVERFLOW && !USE_SYSTEM_MALLOC
  if ((ptr > (void *)LOCAL_TrailTop - 1024 &&
       TR < (tr_fr_ptr)LOCAL_TrailTop + (64 * 1024))) {
    if (!Yap_growtrail(64 * 1024, TRUE)) {
      Yap_Error(RESOURCE_ERROR_TRAIL, TermNil,
                "YAP failed to reserve %ld bytes in growtrail", K64);
    }
    /* just in case, make sure the OS keeps the signal handler. */
    /*    my_signal_info(SIGSEGV, HandleSIGSEGV); */
  } else
#endif /* OS_HANDLES_TR_OVERFLOW */
      if (sure)
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil,
              "tried to access illegal address %p!!!!", ptr);
  else
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil,
              "likely bug in YAP, segmentation violation");
}

/* This routine believes there is a continuous space starting from the
   HeapBase and ending on TrailTop */
static void
HandleSIGSEGV(int sig, void *sipv, void *uap) {
  CACHE_REGS

  void *ptr = TR;
  int sure = FALSE;
  if (LOCAL_PrologMode & ExtendStackMode) {
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil,
              "OS memory allocation crashed at address %p, bailing out\n",
              LOCAL_TrailTop);
  }
#if (defined(__svr4__) || defined(__SVR4))
  siginfo_t *sip = sipv;
  if (sip->si_code != SI_NOINFO && sip->si_code == SEGV_MAPERR) {
    ptr = sip->si_addr;
    sure = TRUE;
  }
#elif __linux__
  siginfo_t *sip = sipv;
  ptr = sip->si_addr;
  sure = TRUE;
#endif
  SearchForTrailFault(ptr, sure);
}
#endif /* SIGSEGV */

/* by default Linux with glibc is IEEE compliant anyway..., but we will pretend
 * it is not. */
static bool set_fpu_exceptions(Term flag) {
  if (flag == TermTrue) {
#if HAVE_FESETEXCEPTFLAG
    fexcept_t excepts;
    return fesetexceptflag(&excepts,
                           FE_DIVBYZERO | FE_UNDERFLOW | FE_OVERFLOW) == 0;
#elif HAVE_FEENABLEEXCEPT
    /* I shall ignore de-normalization and precision errors */
    feenableexcept(FE_DIVBYZERO | FE_INVALID | FE_OVERFLOW);
#elif _WIN32
    // Enable zero-divide, overflow and underflow exception
    _controlfp_s(0, ~(_EM_ZERODIVIDE | _EM_UNDERFLOW | _EM_OVERFLOW),
                 _MCW_EM); // Line B
#elif defined(__hpux)
#if HAVE_FESETTRAPENABLE
    /* From HP-UX 11.0 onwards: */
    fesettrapenable(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW);
#else
    /*
      Up until HP-UX 10.20:
      FP_X_INV   invalid operation exceptions
      FP_X_DZ    divide-by-zero exception
      FP_X_OFL   overflow exception
      FP_X_UFL   underflow exception
      FP_X_IMP   imprecise (inexact result)
      FP_X_CLEAR simply zero to clear all flags
    */
    fpsetmask(FP_X_INV | FP_X_DZ | FP_X_OFL | FP_X_UFL);
#endif
#endif /* __hpux */
#if HAVE_FPU_CONTROL_H && i386 && defined(__GNUC__)
    /* I shall ignore denormalization and precision errors */
    int v = _FPU_IEEE &
            ~(_FPU_MASK_IM | _FPU_MASK_ZM | _FPU_MASK_OM | _FPU_MASK_UM);
    _FPU_SETCW(v);
#endif
#if HAVE_FETESTEXCEPT
    feclearexcept(FE_ALL_EXCEPT);
#endif
#ifdef HAVE_SIGFPE
    my_signal(SIGFPE, HandleMatherr);
#endif
  } else {
/* do IEEE arithmetic in the way the big boys do */
#if HAVE_FESETEXCEPTFLAG
    fexcept_t excepts;
    return fesetexceptflag(&excepts, 0) == 0;
#elif HAVE_FEENABLEEXCEPT
    /* I shall ignore de-normalization and precision errors */
    feenableexcept(0);
#elif _WIN32
    // Enable zero-divide, overflow and underflow exception
    _controlfp_s(0, (_EM_ZERODIVIDE | _EM_UNDERFLOW | _EM_OVERFLOW),
                 _MCW_EM); // Line B
#elif defined(__hpux)
#if HAVE_FESETTRAPENABLE
    fesettrapenable(FE_ALL_EXCEPT);
#else
    fpsetmask(FP_X_CLEAR);
#endif
#endif /* __hpux */
#if HAVE_FPU_CONTROL_H && i386 && defined(__GNUC__)
    /* this will probably not work in older releases of Linux */
    int v = _FPU_IEEE;
    _FPU_SETCW(v);
#endif
#ifdef HAVE_SIGFPE
    my_signal(SIGFPE, SIG_IGN);
#endif
  }
  return true;
}

static void ReceiveSignal(int s, void *x, void *y) {
  CACHE_REGS
  LOCAL_PrologMode |= InterruptMode;
#if !defined(LIGHT) && !_MSC_VER && !defined(__MINGW32__)

  if (s == SIGINT && (LOCAL_PrologMode & ConsoleGetcMode)) {
    return;
  }
    my_signal(s, ReceiveSignal);
  switch (s) {
  case SIGINT:
    // always direct SIGINT to console
    Yap_HandleSIGINT();
    break;
  case SIGALRM:
    Yap_external_signal(worker_id, YAP_ALARM_SIGNAL);
    break;
  case SIGVTALRM:
    Yap_external_signal(worker_id, YAP_VTALARM_SIGNAL);
    break;
#ifndef MPW
#ifdef HAVE_SIGFPE
  case SIGFPE:
    Yap_external_signal(worker_id, YAP_FPE_SIGNAL);
    break;
#endif
#endif
#if !defined(LIGHT) && !defined(_WIN32)
  /* These signals are not handled by WIN32 and not the Macintosh */
  case SIGQUIT:
  case SIGKILL:
    LOCAL_PrologMode &= ~InterruptMode;
    Yap_Error(INTERRUPT_EVENT, MkIntTerm(s), NULL);
    break;
#endif
#ifdef SIGUSR1
  case SIGUSR1:
    /* force the system to creep */
    Yap_external_signal(worker_id, YAP_USR1_SIGNAL);
    break;
#endif /* defined(SIGUSR1) */
#ifdef SIGUSR2
  case SIGUSR2:
    /* force the system to creep */
    Yap_external_signal(worker_id, YAP_USR2_SIGNAL);
    break;
#endif /* defined(SIGUSR2) */
#ifdef SIGPIPE
  case SIGPIPE:
    /* force the system to creep */
    Yap_external_signal(worker_id, YAP_PIPE_SIGNAL);
    break;
#endif /* defined(SIGPIPE) */
#ifdef SIGHUP
  case SIGHUP:
    /* force the system to creep */
    /* Just ignore SUGHUP Yap_signal (YAP_HUP_SIGNAL); */
    break;
#endif /* defined(SIGHUP) */
  default:
    fprintf(stderr, "\n[ Unexpected signal ]\n");
    exit(s);
  }
#endif
  LOCAL_PrologMode &= ~InterruptMode;
}

#if (_MSC_VER || defined(__MINGW32__))
static BOOL WINAPI MSCHandleSignal(DWORD dwCtrlType) {
  if (
#if THREADS
      REMOTE_InterruptsDisabled(0)
#else
      LOCAL_InterruptsDisabled
#endif
          ) {
    return FALSE;
  }
  switch (dwCtrlType) {
  case CTRL_C_EVENT:
  case CTRL_BREAK_EVENT:
#if THREADS
    Yap_external_signal(0, YAP_WINTIMER_SIGNAL);
    REMOTE_PrologMode(0) |= InterruptMode;
#else
    Yap_signal(YAP_WINTIMER_SIGNAL);
    LOCAL_PrologMode |= InterruptMode;
#endif
    return (TRUE);
  default:
    return (FALSE);
  }
}
#endif

/* wrapper for alarm system call */
#if _MSC_VER || defined(__MINGW32__)

static DWORD WINAPI DoTimerThread(LPVOID targ) {
  Int *time = (Int *)targ;
  HANDLE htimer;
  LARGE_INTEGER liDueTime;

  htimer = CreateWaitableTimer(NULL, FALSE, NULL);
  liDueTime.QuadPart = -10000000;
  liDueTime.QuadPart *= time[0];
  /* add time in usecs */
  liDueTime.QuadPart -= time[1] * 10;
  /* Copy the relative time into a LARGE_INTEGER. */
  if (SetWaitableTimer(htimer, &liDueTime, 0, NULL, NULL, 0) == 0) {
    return (FALSE);
  }
  if (WaitForSingleObject(htimer, INFINITE) != WAIT_OBJECT_0)
    fprintf(stderr, "WaitForSingleObject failed (%ld)\n", GetLastError());
  Yap_signal(YAP_WINTIMER_SIGNAL);
  /* now, say what is going on */
  Yap_PutValue(AtomAlarm, MkAtomTerm(AtomTrue));
  ExitThread(1);
#if _MSC_VER
  return (0L);
#endif
}

#endif

static Int enable_interrupts(USES_REGS1) {
  LOCAL_InterruptsDisabled--;
  if (LOCAL_Signals && !LOCAL_InterruptsDisabled) {
    CreepFlag = Unsigned(LCL0);
    if (!Yap_only_has_signal(YAP_CREEP_SIGNAL))
      EventFlag = Unsigned(LCL0);
  }
  return TRUE;
}

static Int disable_interrupts(USES_REGS1) {
  LOCAL_InterruptsDisabled++;
  CalculateStackGap(PASS_REGS1);
  return TRUE;
}



static Int alarm4(USES_REGS1) {
  Term t = Deref(ARG1);
  Term t2 = Deref(ARG2);
  Int i1, i2;
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "alarm/2");
    return (FALSE);
  }
  if (!IsIntegerTerm(t)) {
    Yap_Error(TYPE_ERROR_INTEGER, t, "alarm/2");
    return (FALSE);
  }
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "alarm/2");
    return (FALSE);
  }
  if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "alarm/2");
    return (FALSE);
  }
  i1 = IntegerOfTerm(t);
  i2 = IntegerOfTerm(t2);
  if (i1 == 0 && i2 == 0) {  
#if _WIN32
    Yap_get_signal(YAP_WINTIMER_SIGNAL);
#else
    Yap_get_signal(YAP_ALARM_SIGNAL);
#endif
  }
#if _MSC_VER || defined(__MINGW32__)
  {
    Term tout;
    Int time[2];

    time[0] = i1;
    time[1] = i2;

    if (time[0] != 0 && time[1] != 0) {
      DWORD dwThreadId;
      HANDLE hThread;

      hThread = CreateThread(NULL,          /* no security attributes */
                             0,             /* use default stack size */
                             DoTimerThread, /* thread function */
                             (LPVOID)time,  /* argument to thread function */
                             0,             /* use default creation flags  */
                             &dwThreadId);  /* returns the thread identifier */

      /* Check the return value for success. */
      if (hThread == NULL) {
        Yap_WinError("trying to use alarm");
      }
    }
    tout = MkIntTerm(0);
    return Yap_unify(ARG3, tout) && Yap_unify(ARG4, MkIntTerm(0));
  }
#elif HAVE_SETITIMER && !SUPPORT_CONDOR
  {
    struct itimerval new, old;

    new.it_interval.tv_sec = 0;
    new.it_interval.tv_usec = 0;
    new.it_value.tv_sec = i1;
    new.it_value.tv_usec = i2;
    //    Yap_do_low_level_trace=1;

    if (setitimer(ITIMER_REAL, &new, &old) < 0) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, "setitimer: %s",
                strerror(errno));
#else
      Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, "setitimer %d", errno);
#endif
      return FALSE;
    }
    return Yap_unify(ARG3, MkIntegerTerm(old.it_value.tv_sec)) &&
           Yap_unify(ARG4, MkIntegerTerm(old.it_value.tv_usec));
  }
#elif HAVE_ALARM && !SUPPORT_CONDOR
  {
    Int left;
    Term tout;

    left = alarm(i1);
    tout = MkIntegerTerm(left);
    return Yap_unify(ARG3, tout) && Yap_unify(ARG4, MkIntTerm(0));
  }
#else
  /* not actually trying to set the alarm */
  if (IntegerOfTerm(t) == 0)
    return TRUE;
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "alarm not available in this configuration");
  return FALSE;
#endif
}

static Int virtual_alarm(USES_REGS1) {
  Term t = Deref(ARG1);
  Term t2 = Deref(ARG2);
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "alarm/2");
    return (FALSE);
  }
  if (!IsIntegerTerm(t)) {
    Yap_Error(TYPE_ERROR_INTEGER, t, "alarm/2");
    return (FALSE);
  }
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "alarm/2");
    return (FALSE);
  }
  if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "alarm/2");
    return (FALSE);
  }
#if _MSC_VER || defined(__MINGW32__)
  {
    Term tout;
    Int time[2];

    time[0] = IntegerOfTerm(t);
    time[1] = IntegerOfTerm(t2);

    if (time[0] != 0 && time[1] != 0) {
      DWORD dwThreadId;
      HANDLE hThread;

      hThread = CreateThread(NULL,          /* no security attributes */
                             0,             /* use default stack size */
                             DoTimerThread, /* thread function */
                             (LPVOID)time,  /* argument to thread function */
                             0,             /* use default creation flags  */
                             &dwThreadId);  /* returns the thread identifier */

      /* Check the return value for success. */
      if (hThread == NULL) {
        Yap_WinError("trying to use alarm");
      }
    }
    tout = MkIntTerm(0);
    return Yap_unify(ARG3, tout) && Yap_unify(ARG4, MkIntTerm(0));
  }
#elif HAVE_SETITIMER && !SUPPORT_CONDOR
  {
    struct itimerval new, old;

    new.it_interval.tv_sec = 0;
    new.it_interval.tv_usec = 0;
    new.it_value.tv_sec = IntegerOfTerm(t);
    new.it_value.tv_usec = IntegerOfTerm(t2);
    if (setitimer(ITIMER_VIRTUAL, &new, &old) < 0) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, "setitimer: %s",
                strerror(errno));
#else
      Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, "setitimer %d", errno);
#endif
      return FALSE;
    }
    return Yap_unify(ARG3, MkIntegerTerm(old.it_value.tv_sec)) &&
           Yap_unify(ARG4, MkIntegerTerm(old.it_value.tv_usec));
  }
#else
  /* not actually trying to set the alarm */
  if (IntegerOfTerm(t) == 0)
    return TRUE;
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "alarm not available in this configuration");
  return FALSE;
#endif
}

#ifdef VAX

/* avoid longjmp botch */

int vax_absmi_fp;

typedef struct {
  int eh;
  int flgs;
  int ap;
  int fp;
  int pc;
  int dummy1;
  int dummy2;
  int dummy3;
  int oldfp;
  int dummy4;
  int dummy5;
  int dummy6;
  int oldpc;
}

    * VaxFramePtr;

VaxFixFrame(dummy) {
  int maxframes = 100;
  VaxFramePtr fp = (VaxFramePtr)(((int *)&dummy) - 6);
  while (--maxframes) {
    fp = (VaxFramePtr)fp->fp;
    if (fp->flgs == 0) {
      if (fp->oldfp >= &REGS[6] && fp->oldfp < &REGS[REG_SIZE])
        fp->oldfp = vax_absmi_fp;
      return;
    }
  }
}

#endif

#if defined(_WIN32)

int WINAPI win_yap(HANDLE, DWORD, LPVOID);

int WINAPI win_yap(HANDLE hinst, DWORD reason, LPVOID reserved) {
  switch (reason) {
  case DLL_PROCESS_ATTACH:
    break;
  case DLL_PROCESS_DETACH:
    break;
  case DLL_THREAD_ATTACH:
    break;
  case DLL_THREAD_DETACH:
    break;
  }
  return 1;
}
#endif

#if (defined(YAPOR) || defined(THREADS)) && !defined(USE_PTHREAD_LOCKING)
#ifdef sparc
void rw_lock_voodoo(void);

void rw_lock_voodoo(void) {
  /* code taken from the Linux kernel, it handles shifting between locks */
  /* Read/writer locks, as usual this is overly clever to make it as fast as
   * possible. */
  /* caches... */
  __asm__ __volatile__("___rw_read_enter_spin_on_wlock:\n"
                       "	orcc	%g2, 0x0, %g0\n"
                       "	be,a	___rw_read_enter\n"
                       "	 ldstub	[%g1 + 3], %g2\n"
                       "	b	___rw_read_enter_spin_on_wlock\n"
                       "	 ldub	[%g1 + 3], %g2\n"
                       "___rw_read_exit_spin_on_wlock:\n"
                       "	orcc	%g2, 0x0, %g0\n"
                       "	be,a	___rw_read_exit\n"
                       "	 ldstub	[%g1 + 3], %g2\n"
                       "	b	___rw_read_exit_spin_on_wlock\n"
                       "	 ldub	[%g1 + 3], %g2\n"
                       "___rw_write_enter_spin_on_wlock:\n"
                       "	orcc	%g2, 0x0, %g0\n"
                       "	be,a	___rw_write_enter\n"
                       "	 ldstub	[%g1 + 3], %g2\n"
                       "	b	___rw_write_enter_spin_on_wlock\n"
                       "	 ld	[%g1], %g2\n"
                       "\n"
                       "	.globl	___rw_read_enter\n"
                       "___rw_read_enter:\n"
                       "	orcc	%g2, 0x0, %g0\n"
                       "	bne,a	___rw_read_enter_spin_on_wlock\n"
                       "	 ldub	[%g1 + 3], %g2\n"
                       "	ld	[%g1], %g2\n"
                       "	add	%g2, 1, %g2\n"
                       "	st	%g2, [%g1]\n"
                       "	retl\n"
                       "	 mov	%g4, %o7\n"
                       "	.globl	___rw_read_exit\n"
                       "___rw_read_exit:\n"
                       "	orcc	%g2, 0x0, %g0\n"
                       "	bne,a	___rw_read_exit_spin_on_wlock\n"
                       "	 ldub	[%g1 + 3], %g2\n"
                       "	ld	[%g1], %g2\n"
                       "	sub	%g2, 0x1ff, %g2\n"
                       "	st	%g2, [%g1]\n"
                       "	retl\n"
                       "	 mov	%g4, %o7\n"
                       "	.globl	___rw_write_enter\n"
                       "___rw_write_enter:\n"
                       "	orcc	%g2, 0x0, %g0\n"
                       "	bne	___rw_write_enter_spin_on_wlock\n"
                       "	 ld	[%g1], %g2\n"
                       "	andncc	%g2, 0xff, %g0\n"
                       "	bne,a	___rw_write_enter_spin_on_wlock\n"
                       "	 stb	%g0, [%g1 + 3]\n"
                       "	retl\n"
                       "	 mov	%g4, %o7\n");
}
#endif /* sparc */

#endif /* YAPOR || THREADS */

yap_error_number Yap_MathException__(USES_REGS1) {
#if HAVE_FETESTEXCEPT
  int raised;

  // #pragma STDC FENV_ACCESS ON
  if ((raised = fetestexcept(FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW))) {

    feclearexcept(FE_ALL_EXCEPT);
    if (raised & FE_OVERFLOW) {
      return EVALUATION_ERROR_FLOAT_OVERFLOW;
    } else if (raised & FE_DIVBYZERO) {
      return EVALUATION_ERROR_ZERO_DIVISOR;
    } else if (raised & FE_UNDERFLOW) {
      return EVALUATION_ERROR_FLOAT_UNDERFLOW;
      //} else if (raised & (FE_INVALID|FE_INEXACT)) {
      //    return  EVALUATION_ERROR_UNDEFINED;
    } else {
      return EVALUATION_ERROR_UNDEFINED;
    }
  }
#elif _WIN32
  unsigned int raised;
  int err;

  // Show original FP control word and do calculation.
  err = _controlfp_s(&raised, 0, 0);
  if (err) {
    return EVALUATION_ERROR_UNDEFINED;
  }
  if (raised) {
    feclearexcept(FE_ALL_EXCEPT);
    if (raised & FE_OVERFLOW) {
      return EVALUATION_ERROR_FLOAT_OVERFLOW;
    } else if (raised & FE_DIVBYZERO) {
      return EVALUATION_ERROR_ZERO_DIVISOR;
    } else if (raised & FE_UNDERFLOW) {
      return EVALUATION_ERROR_FLOAT_UNDERFLOW;
      //} else if (raised & (FE_INVALID|FE_INEXACT)) {
      //    return  EVALUATION_ERROR_UNDEFINED;
    } else {
      return EVALUATION_ERROR_UNDEFINED;
    }
  }
#elif (defined(__svr4__) || defined(__SVR4))
  switch (sip->si_code) {
  case FPE_INTDIV:
    return EVALUATION_ERROR_ZERO_DIVISOR;
    break;
  case FPE_INTOVF:
    return EVALUATION_ERROR_INT_OVERFLOW;
    break;
  case FPE_FLTDIV:
    return EVALUATION_ERROR_ZERO_DIVISOR;
    break;
  case FPE_FLTOVF:
    return EVALUATION_ERROR_FLOAT_OVERFLOW;
    break;
  case FPE_FLTUND:
    return EVALUATION_ERROR_FLOAT_UNDERFLOW;
    break;
  case FPE_FLTRES:
  case FPE_FLTINV:
  case FPE_FLTSUB:
  default:
    return EVALUATION_ERROR_UNDEFINED;
  }
  set_fpu_exceptions(0);
#endif

  return LOCAL_Error_TYPE;
}

/**
 *
 * This function implements the sigsegv prolog flag. It should be called when we want other languages to take over
 * handling this signal.
 *
 * @param enable: on off
 * @return should always succeed
 */
bool Yap_InitSIGSEGV(Term enable) {
#if HAVE_SIGSEGV
    if (GLOBAL_PrologShouldHandleInterrupts || enable == TermFalse || enable == TermOff) {
        my_signal(SIGSEGV, SIG_DFL);
    } else {
        my_signal_info(SIGSEGV, HandleSIGSEGV);
    }
    return true;
#else
return false;
#endif
}


/**
 *
 * This routine sets up the signal handlers. It depends on the flag GLOBAL_PrologShouldHandleInterrupts.
 * Notice that it should only be called if we want to set up interrupt handlers, or if we want to disable ones set up
 * by Prolog. It should not be called if Prolog is in embedded mode.
 *
 * SIGINT can cause problems, if caught before full initialization
 *
 * */
void Yap_InitOSSignals(int wid) {
    void * hdl;
    if (GLOBAL_PrologShouldHandleInterrupts) {
        hdl = ReceiveSignal;

#if !defined(LIGHT) && !_MSC_VER && !defined(__MINGW32__) && !defined(LIGHT)
        my_signal(SIGQUIT, hdl);
        my_signal(SIGKILL, hdl);
        my_signal(SIGUSR1, hdl);
        my_signal(SIGUSR2, hdl);
        my_signal(SIGHUP, hdl);
        my_signal(SIGALRM, hdl);
        my_signal(SIGVTALRM, hdl);
#endif
#ifdef SIGPIPE
        my_signal(SIGPIPE, hdl);
#endif
#if _MSC_VER || defined(__MINGW32__)
        signal(SIGINT, SIG_IGN);
        SetConsoleCtrlHandler(MSCHandleSignal, TRUE);
#else
        my_signal(SIGINT, hdl);
#endif
    }
#ifdef HAVE_SIGFPE
  if (GLOBAL_PrologShouldHandleInterrupts) {
      my_signal(SIGFPE, HandleMatherr);
  } else {
      my_signal(SIGFPE, hdl);
  }
#endif
#if HAVE_SIGSEGV
  if (GLOBAL_PrologShouldHandleInterrupts) {
    my_signal_info(SIGSEGV, HandleSIGSEGV);
  } else {
      my_signal(SIGFPE, hdl);
  }
#endif
#ifdef YAPOR_COW
    signal(SIGCHLD, SIG_IGN); /* avoid ghosts */
#endif
}

bool Yap_set_fpu_exceptions(Term flag) { return set_fpu_exceptions(flag); }

/**
 * @brief Initialize internal interface predicates
 */
void Yap_InitSignalPreds(void) {
  CACHE_REGS
  Term cm = CurrentModule;
  Yap_InitCPred("$alarm", 4, alarm4, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$virtual_alarm", 4, virtual_alarm, SafePredFlag | SyncPredFlag);
  CurrentModule = HACKS_MODULE;
  Yap_InitCPred("enable_interrupts", 0, enable_interrupts, SafePredFlag);
  Yap_InitCPred("disable_interrupts", 0, disable_interrupts, SafePredFlag);
  my_signal_info(SIGSEGV, HandleSIGSEGV);
  CurrentModule = cm;
}
