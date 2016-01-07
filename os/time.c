
#include "sysbits.h" 

#ifdef SIMICS
#ifdef HAVE_GETRUSAGE
#undef HAVE_GETRUSAGE
#endif
#ifdef HAVE_TIMES
#undef HAVE_TIMES
#endif
#endif /* SIMICS */

#ifdef _WIN32
#if HAVE_GETRUSAGE
#undef HAVE_GETRUSAGE
#endif
#endif

#if HAVE_GETRUSAGE

#if HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#if THREADS
#define StartOfTimes (*(LOCAL_ThreadHandle.start_of_timesp))
#define last_time    (*(LOCAL_ThreadHandle.last_timep))

#define StartOfTimes_sys (*(LOCAL_ThreadHandle.start_of_times_sysp))
#define last_time_sys    (*(LOCAL_ThreadHandle.last_time_sysp))

#else
/* since the point YAP was started */
static struct timeval StartOfTimes;

/* since last call to runtime */
static struct timeval last_time;

/* same for system time */
static struct timeval last_time_sys;
static struct timeval StartOfTimes_sys;
#endif

/* store user time in this variable */
 void
Yap_InitTime (int wid)
{
  struct rusage   rusage;

#if THREADS
  REMOTE_ThreadHandle(wid).start_of_timesp = (struct timeval *)malloc(sizeof(struct timeval));
  REMOTE_ThreadHandle(wid).last_timep = (struct timeval *)malloc(sizeof(struct timeval));
  REMOTE_ThreadHandle(wid).start_of_times_sysp = (struct timeval *)malloc(sizeof(struct timeval));
  REMOTE_ThreadHandle(wid).last_time_sysp = (struct timeval *)malloc(sizeof(struct timeval));
  getrusage(RUSAGE_SELF, &rusage);
  (*REMOTE_ThreadHandle(wid).last_timep).tv_sec =
    (*REMOTE_ThreadHandle(wid).start_of_timesp).tv_sec =
    rusage.ru_utime.tv_sec;
  (*REMOTE_ThreadHandle(wid).last_timep).tv_usec =
    (*REMOTE_ThreadHandle(wid).start_of_timesp).tv_usec =
    rusage.ru_utime.tv_usec;
  (*REMOTE_ThreadHandle(wid).last_time_sysp).tv_sec =
    (*REMOTE_ThreadHandle(wid).start_of_times_sysp).tv_sec =
    rusage.ru_stime.tv_sec;
  (*REMOTE_ThreadHandle(wid).last_time_sysp).tv_usec =
    (*REMOTE_ThreadHandle(wid).start_of_times_sysp).tv_usec =
    rusage.ru_stime.tv_usec;
#else
  getrusage(RUSAGE_SELF, &rusage);
  last_time.tv_sec =
    StartOfTimes.tv_sec =
    rusage.ru_utime.tv_sec;
  last_time.tv_usec =
    StartOfTimes.tv_usec =
    rusage.ru_utime.tv_usec;
  last_time_sys.tv_sec =
    StartOfTimes_sys.tv_sec =
    rusage.ru_stime.tv_sec;
  last_time_sys.tv_usec =
    StartOfTimes_sys.tv_usec =
    rusage.ru_stime.tv_usec;
#endif
}


UInt
Yap_cputime ( void )
{
  CACHE_REGS
    struct rusage   rusage;

  getrusage(RUSAGE_SELF, &rusage);
  return((rusage.ru_utime.tv_sec - StartOfTimes.tv_sec)) * 1000 +
    ((rusage.ru_utime.tv_usec - StartOfTimes.tv_usec) / 1000);
}

void Yap_cputime_interval(Int *now,Int *interval)
{
  CACHE_REGS
    struct rusage   rusage;

  getrusage(RUSAGE_SELF, &rusage);
  *now = (rusage.ru_utime.tv_sec - StartOfTimes.tv_sec) * 1000 +
    (rusage.ru_utime.tv_usec - StartOfTimes.tv_usec) / 1000;
  *interval = (rusage.ru_utime.tv_sec - last_time.tv_sec) * 1000 +
    (rusage.ru_utime.tv_usec - last_time.tv_usec) / 1000;
  last_time.tv_usec = rusage.ru_utime.tv_usec;
  last_time.tv_sec = rusage.ru_utime.tv_sec;
}

void Yap_systime_interval(Int *now,Int *interval)
{
  CACHE_REGS
    struct rusage   rusage;

  getrusage(RUSAGE_SELF, &rusage);
  *now = (rusage.ru_stime.tv_sec - StartOfTimes_sys.tv_sec) * 1000 +
    (rusage.ru_stime.tv_usec - StartOfTimes_sys.tv_usec) / 1000;
  *interval = (rusage.ru_stime.tv_sec - last_time_sys.tv_sec) * 1000 +
    (rusage.ru_stime.tv_usec - last_time_sys.tv_usec) / 1000;
  last_time_sys.tv_usec = rusage.ru_stime.tv_usec;
  last_time_sys.tv_sec = rusage.ru_stime.tv_sec;
}

#elif defined(_WIN32)

#ifdef __GNUC__

/* This is stolen from the Linux kernel.
   The problem is that mingw32 does not seem to have acces to div */
#ifndef do_div
#define do_div(n,base) ({						\
      unsigned long __upper, __low, __high, __mod;			\
      asm("":"=a" (__low), "=d" (__high):"A" (n));			\
      __upper = __high;							\
      if (__high) {							\
	__upper = __high % (base);					\
	__high = __high / (base);					\
      }									\
      asm("divl %2":"=a" (__low), "=d" (__mod):"rm" (base), "0" (__low), "1" (__upper)); \
      asm("":"=A" (n):"a" (__low),"d" (__high));			\
      __mod;								\
    })
#endif

#endif



#include <time.h>

  static FILETIME StartOfTimes, last_time;

static FILETIME StartOfTimes_sys, last_time_sys;

static clock_t TimesStartOfTimes, Times_last_time;

/* store user time in this variable */
static void
InitTime (int wid)
{
  HANDLE hProcess = GetCurrentProcess();
  FILETIME CreationTime, ExitTime, KernelTime, UserTime;
  if (!GetProcessTimes(hProcess, &CreationTime, &ExitTime, &KernelTime, &UserTime)) {
    /* WIN98 */
    clock_t t;
    t = clock ();
    Times_last_time = TimesStartOfTimes = t;
  } else {
#if THREADS
    REMOTE_ThreadHandle(wid).start_of_timesp = (struct _FILETIME *)malloc(sizeof(FILETIME));
    REMOTE_ThreadHandle(wid).last_timep = (struct _FILETIME *)malloc(sizeof(FILETIME));
    REMOTE_ThreadHandle(wid).start_of_times_sysp = (struct _FILETIME *)malloc(sizeof(FILETIME));
    REMOTE_ThreadHandle(wid).last_time_sysp = (struct _FILETIME *)malloc(sizeof(FILETIME));
    (*REMOTE_ThreadHandle(wid).last_timep).dwLowDateTime =
      UserTime.dwLowDateTime;
    (*REMOTE_ThreadHandle(wid).last_timep).dwHighDateTime =
      UserTime.dwHighDateTime;
    (*REMOTE_ThreadHandle(wid).start_of_timesp).dwLowDateTime =
      UserTime.dwLowDateTime;
    (*REMOTE_ThreadHandle(wid).start_of_timesp).dwHighDateTime =
      UserTime.dwHighDateTime;
    (*REMOTE_ThreadHandle(wid).last_time_sysp).dwLowDateTime =
      KernelTime.dwLowDateTime;
    (*REMOTE_ThreadHandle(wid).last_time_sysp).dwHighDateTime =
      KernelTime.dwHighDateTime;
    (*REMOTE_ThreadHandle(wid).start_of_times_sysp).dwLowDateTime =
      KernelTime.dwLowDateTime;
    (*REMOTE_ThreadHandle(wid).start_of_times_sysp).dwHighDateTime =
      KernelTime.dwHighDateTime;
#else
    last_time.dwLowDateTime =
      UserTime.dwLowDateTime;
    last_time.dwHighDateTime =
      UserTime.dwHighDateTime;
    StartOfTimes.dwLowDateTime =
      UserTime.dwLowDateTime;
    StartOfTimes.dwHighDateTime =
      UserTime.dwHighDateTime;
    last_time_sys.dwLowDateTime =
      KernelTime.dwLowDateTime;
    last_time_sys.dwHighDateTime =
      KernelTime.dwHighDateTime;
    StartOfTimes_sys.dwLowDateTime =
      KernelTime.dwLowDateTime;
    StartOfTimes_sys.dwHighDateTime =
      KernelTime.dwHighDateTime;
#endif
  }
}

#ifdef __GNUC__
static unsigned long long int
sub_utime(FILETIME t1, FILETIME t2)
{
  ULARGE_INTEGER u[2];
  memcpy((void *)u,(void *)&t1,sizeof(FILETIME));
  memcpy((void *)(u+1),(void *)&t2,sizeof(FILETIME));
  return
    u[0].QuadPart - u[1].QuadPart;
}
#endif

UInt
Yap_cputime ( void )
{
  HANDLE hProcess = GetCurrentProcess();
  FILETIME CreationTime, ExitTime, KernelTime, UserTime;
  if (!GetProcessTimes(hProcess, &CreationTime, &ExitTime, &KernelTime, &UserTime)) {
    clock_t t;
    t = clock ();
    return(((t - TimesStartOfTimes)*1000) / CLOCKS_PER_SEC);
  } else {
#ifdef __GNUC__
    unsigned long long int t =
      sub_utime(UserTime,StartOfTimes);
    do_div(t,10000);
    return((Int)t);
#endif
#ifdef _MSC_VER
    __int64 t = *(__int64 *)&UserTime - *(__int64 *)&StartOfTimes;
    return((Int)(t/10000));
#endif
  }
}

void Yap_cputime_interval(Int *now,Int *interval)
{
  HANDLE hProcess = GetCurrentProcess();
  FILETIME CreationTime, ExitTime, KernelTime, UserTime;
  if (!GetProcessTimes(hProcess, &CreationTime, &ExitTime, &KernelTime, &UserTime)) {
    clock_t t;
    t = clock ();
    *now = ((t - TimesStartOfTimes)*1000) / CLOCKS_PER_SEC;
    *interval = (t - Times_last_time) * 1000 / CLOCKS_PER_SEC;
    Times_last_time = t;
  } else {
#ifdef __GNUC__
    unsigned long long int t1 =
      sub_utime(UserTime, StartOfTimes);
    unsigned long long int t2 =
      sub_utime(UserTime, last_time);
    do_div(t1,10000);
    *now = (Int)t1;
    do_div(t2,10000);
    *interval = (Int)t2;
#endif
#ifdef _MSC_VER
    __int64 t1 = *(__int64 *)&UserTime - *(__int64 *)&StartOfTimes;
    __int64 t2 = *(__int64 *)&UserTime - *(__int64 *)&last_time;
    *now = (Int)(t1/10000);
    *interval = (Int)(t2/10000);
#endif
    last_time.dwLowDateTime = UserTime.dwLowDateTime;
    last_time.dwHighDateTime = UserTime.dwHighDateTime;
  }
}

void Yap_systime_interval(Int *now,Int *interval)
{
  HANDLE hProcess = GetCurrentProcess();
  FILETIME CreationTime, ExitTime, KernelTime, UserTime;
  if (!GetProcessTimes(hProcess, &CreationTime, &ExitTime, &KernelTime, &UserTime)) {
    *now = *interval = 0; /* not available */
  } else {
#ifdef __GNUC__
    unsigned long long int t1 =
      sub_utime(KernelTime, StartOfTimes_sys);
    unsigned long long int t2 =
      sub_utime(KernelTime, last_time_sys);
    do_div(t1,10000);
    *now = (Int)t1;
    do_div(t2,10000);
    *interval = (Int)t2;
#endif
#ifdef _MSC_VER
    __int64 t1 = *(__int64 *)&KernelTime - *(__int64 *)&StartOfTimes_sys;
    __int64 t2 = *(__int64 *)&KernelTime - *(__int64 *)&last_time_sys;
    *now = (Int)(t1/10000);
    *interval = (Int)(t2/10000);
#endif
    last_time_sys.dwLowDateTime = KernelTime.dwLowDateTime;
    last_time_sys.dwHighDateTime = KernelTime.dwHighDateTime;
  }
}

#elif HAVE_TIMES

#if defined(_WIN32)

#include <time.h>

#define TicksPerSec     CLOCKS_PER_SEC

#else

#if HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif

#endif

#if defined(__sun__) && (defined(__svr4__) || defined(__SVR4))

#if HAVE_LIMITS_H
#include <limits.h>
#endif

#define TicksPerSec	CLK_TCK
#endif

#if defined(__alpha) || defined(__FreeBSD__) || defined(__linux__) || defined(__DragonFly__)

#if HAVE_TIME_H
#include <time.h>
#endif

#define TicksPerSec	sysconf(_SC_CLK_TCK)

#endif

#if !TMS_IN_SYS_TIME
#if HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
#endif

static clock_t StartOfTimes, last_time;

static clock_t StartOfTimes_sys, last_time_sys;

/* store user time in this variable */
static void
InitTime (void)
{
  struct tms t;
  times (&t);
  (*REMOTE_ThreadHandle(wid).last_timep) = StartOfTimes = t.tms_utime;
  last_time_sys = StartOfTimes_sys = t.tms_stime;
}

UInt
Yap_cputime (void)
{
  struct tms t;
  times(&t);
  return((t.tms_utime - StartOfTimes)*1000 / TicksPerSec);
}

void Yap_cputime_interval(Int *now,Int *interval)
{
  struct tms t;
  times (&t);
  *now = ((t.tms_utime - StartOfTimes)*1000) / TicksPerSec;
  *interval = (t.tms_utime - last_time) * 1000 / TicksPerSec;
  last_time = t.tms_utime;
}

void Yap_systime_interval(Int *now,Int *interval)
{
  struct tms t;
  times (&t);
  *now = ((t.tms_stime - StartOfTimes_sys)*1000) / TicksPerSec;
  *interval = (t.tms_stime - last_time_sys) * 1000 / TicksPerSec;
  last_time_sys = t.tms_stime;
}

#else /* HAVE_TIMES */

#ifdef SIMICS

#include <sys/time.h>

/* since the point YAP was started */
static struct timeval StartOfTimes;

/* since last call to runtime */
static struct timeval last_time;

/* store user time in this variable */
static void
InitTime (int wid)
{
  struct timeval   tp;

  gettimeofday(&tp,NULL);
  (*REMOTE_ThreadHandle(wid).last_timep).tv_sec = (*REMOTE_ThreadHandle.start_of_timesp(wid)).tv_sec = tp.tv_sec;
  (*REMOTE_ThreadHandle(wid).last_timep).tv_usec = (*REMOTE_ThreadHandle.start_of_timesp(wid)).tv_usec = tp.tv_usec;
}


UInt
Yap_cputime (void)
{
  struct timeval   tp;

  gettimeofday(&tp,NULL);
  if (StartOfTimes.tv_usec > tp.tv_usec)
    return((tp.tv_sec - StartOfTimes.tv_sec - 1) * 1000 +
	   (StartOfTimes.tv_usec - tp.tv_usec) /1000);
  else
    return((tp.tv_sec - StartOfTimes.tv_sec)) * 1000 +
      ((tp.tv_usec - StartOfTimes.tv_usec) / 1000);
}

void Yap_cputime_interval(Int *now,Int *interval)
{
  struct timeval   tp;

  gettimeofday(&tp,NULL);
  *now = (tp.tv_sec - StartOfTimes.tv_sec) * 1000 +
    (tp.tv_usec - StartOfTimes.tv_usec) / 1000;
  *interval = (tp.tv_sec - last_time.tv_sec) * 1000 +
    (tp.tv_usec - last_time.tv_usec) / 1000;
  last_time.tv_usec = tp.tv_usec;
  last_time.tv_sec = tp.tv_sec;
}

void Yap_systime_interval(Int *now,Int *interval)
{
  *now =  *interval = 0; /* not available */
}

#endif /* SIMICS */

#ifdef COMMENTED_OUT
/* This code is not working properly. I left it here to help future ports */
#ifdef MPW

#include <files.h>
#include <Events.h>

#define TicksPerSec 60.0

static double
real_cputime ()
{
  return (((double) TickCount ()) / TicksPerSec);
}

#endif /* MPW */

#ifdef LATTICE

#include "osbind.h"

static long *ptime;

gettime ()
{
  *ptime = *(long *) 0x462;
}

static double
real_cputime ()
{
  long thetime;
  ptime = &thetime;
  xbios (38, gettime);
  return (((double) thetime) / (Getrez () == 2 ? 70 : 60));
}

#endif /* LATTICE */

#ifdef M_WILLIAMS

#include <osbind.h>
#include <xbios.h>

static long *ptime;

static long
readtime ()
{
  return (*((long *) 0x4ba));
}

static double
real_cputime ()
{
  long time;

  time = Supexec (readtime);
  return (time / 200.0);
}

#endif /* M_WILLIAMS */

#ifdef LIGHT

#undef FALSE
#undef TRUE

#include <FileMgr.h>

#define TicksPerSec 60.0

static double
real_cputime ()
{
  return (((double) TickCount ()) / TicksPerSec);
}

#endif /* LIGHT */

#endif /* COMMENTED_OUT */

#endif /* HAVE_GETRUSAGE */

#if HAVE_GETHRTIME

#if HAVE_TIME_H
#include <time.h>
#endif

/* since the point YAP was started */
static hrtime_t StartOfWTimes;

/* since last call to walltime */
#define  LastWTime (*(hrtime_t *)ALIGN_BY_TYPE(GLOBAL_LastWTimePtr,hrtime_t))

static void
Yap_InitWTime (void)
{
  StartOfWTimes = gethrtime();
}

static void
Yap_InitLastWTime(void) {
  /* ask for twice the space in order to guarantee alignment */
  GLOBAL_LastWTimePtr = (void *)Yap_AllocCodeSpace(2*sizeof(hrtime_t));
  LastWTime = StartOfWTimes;
}

Int
Yap_walltime (void)
{
  hrtime_t tp = gethrtime();
  /* return time in milliseconds */
  return((Int)((tp-StartOfWTimes)/((hrtime_t)1000000)));

}

void Yap_walltime_interval(Int *now,Int *interval)
{
  hrtime_t tp = gethrtime();
  /* return time in milliseconds */
  *now = (Int)((tp-StartOfWTimes)/((hrtime_t)1000000));
  *interval = (Int)((tp-LastWTime)/((hrtime_t)1000000));
  LastWTime = tp;
}


#elif HAVE_GETTIMEOFDAY

/* since the point YAP was started */
static struct timeval StartOfWTimes;

/* since last call to walltime */
#define LastWTime (*(struct timeval *)GLOBAL_LastWTimePtr)

/* store user time in this variable */
 void
Yap_InitWTime (void)
{
  gettimeofday(&StartOfWTimes,NULL);
}

 void
Yap_InitLastWTime(void) {
  GLOBAL_LastWTimePtr = (void *)Yap_AllocCodeSpace(sizeof(struct timeval));
  LastWTime.tv_usec = StartOfWTimes.tv_usec;
  LastWTime.tv_sec = StartOfWTimes.tv_sec;
}


Int
Yap_walltime (void)
{
  struct timeval   tp;

  gettimeofday(&tp,NULL);
  if (StartOfWTimes.tv_usec > tp.tv_usec)
    return((tp.tv_sec - StartOfWTimes.tv_sec - 1) * 1000 +
	   (StartOfWTimes.tv_usec - tp.tv_usec) /1000);
  else
    return((tp.tv_sec - StartOfWTimes.tv_sec)) * 1000 +
      ((tp.tv_usec - LastWTime.tv_usec) / 1000);
}

void Yap_walltime_interval(Int *now,Int *interval)
{
  struct timeval   tp;

  gettimeofday(&tp,NULL);
  *now = (tp.tv_sec - StartOfWTimes.tv_sec) * 1000 +
    (tp.tv_usec - StartOfWTimes.tv_usec) / 1000;
  *interval = (tp.tv_sec - LastWTime.tv_sec) * 1000 +
    (tp.tv_usec - LastWTime.tv_usec) / 1000;
  LastWTime.tv_usec = tp.tv_usec;
  LastWTime.tv_sec = tp.tv_sec;
}

#elif defined(_WIN32)

#include <sys/timeb.h>
#include <time.h>

/* since the point YAP was started */
static struct _timeb StartOfWTimes;

/* since last call to walltime */
#define LastWTime (*(struct timeb *)GLOBAL_LastWTimePtr)

/* store user time in this variable */
static void
InitWTime (void)
{
  _ftime(&StartOfWTimes);
}

static void
InitLastWTime(void) {
  GLOBAL_LastWTimePtr = (void *)Yap_AllocCodeSpace(sizeof(struct timeb));
  LastWTime.time = StartOfWTimes.time;
  LastWTime.millitm = StartOfWTimes.millitm;
}


Int
Yap_walltime (void)
{
  struct _timeb   tp;

  _ftime(&tp);
  if (StartOfWTimes.millitm > tp.millitm)
    return((tp.time - StartOfWTimes.time - 1) * 1000 +
	   (StartOfWTimes.millitm - tp.millitm));
  else
    return((tp.time - StartOfWTimes.time)) * 1000 +
      ((tp.millitm - LastWTime.millitm) / 1000);
}

void Yap_walltime_interval(Int *now,Int *interval)
{
  struct _timeb   tp;

  _ftime(&tp);
  *now = (tp.time - StartOfWTimes.time) * 1000 +
    (tp.millitm - StartOfWTimes.millitm);
  *interval = (tp.time - LastWTime.time) * 1000 +
    (tp.millitm - LastWTime.millitm) ;
  LastWTime.millitm = tp.millitm;
  LastWTime.time = tp.time;
}

#elif HAVE_TIMES

static clock_t StartOfWTimes;

#define LastWTime (*(clock_t *)GLOBAL_LastWTimePtr)

/* store user time in this variable */
static void
InitWTime (void)
{
  StartOfWTimes = times(NULL);
}

static void
InitLastWTime(void) {
  GLOBAL_LastWTimePtr = (void *)Yap_AllocCodeSpace(sizeof(clock_t));
  LastWTime = StartOfWTimes;
}

Int
Yap_walltime (void)
{
  clock_t t;
  t = times(NULL);
  return ((t - StartOfWTimes)*1000 / TicksPerSec));
}

void Yap_walltime_interval(Int *now,Int *interval)
{
  clock_t t;
  t = times(NULL);
  *now = ((t - StartOfWTimes)*1000) / TicksPerSec;
  *interval = (t - GLOBAL_LastWTime) * 1000 / TicksPerSec;
}

 
#endif /* HAVE_TIMES */
 void
  Yap_ReInitWTime (void)
  {
    Yap_InitWTime();
    if (GLOBAL_LastWTimePtr != NULL)
      Yap_FreeCodeSpace(GLOBAL_LastWTimePtr);
    Yap_InitLastWTime();
  }


void
Yap_InitTimePreds(void)
{
    /* can only do after heap is initialized */
    Yap_InitLastWTime();
}
