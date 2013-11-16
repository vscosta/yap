// SWI stuff that is needed everywhere

#ifndef PL_BASIC_H

#define PL_BASIC_H

/* we are in YAP */
#ifndef __YAP_PROLOG__
#define __YAP_PROLOG__ 1
#endif

#if USE_GMP
#define O_GMP 1
#endif

#define O_LOCALE 1

#ifndef PL_CONSOLE
#define PL_KERNEL 1
#endif

#include <SWI-Prolog.h>

#ifdef __MINGW32__
#ifndef O_XOS
#define O_XOS 1
#endif
#ifndef __WINDOWS__
#define __WINDOWS__ 1
#endif
#endif

#ifdef __WINDOWS__
#include <windows.h>
#include <windows/uxnt.h>
#endif

#ifndef __unix__
#if defined(_AIX) || defined(__APPLE__) || defined(__unix) || defined(__BEOS__) || defined(__NetBSD__)
#define __unix__ 1
#endif
#endif

#ifdef THREADS
#define O_PLMT 1
#else
#ifdef _REENTRANT
#undef _REENTRANT
#endif
#endif

#if defined(__GNUC__) && !defined(MAY_ALIAS)
#define MAY_ALIAS __attribute__ ((__may_alias__))
#else
#define MAY_ALIAS
#endif

#ifndef PL_HAVE_TERM_T
#define PL_HAVE_TERM_T
typedef	uintptr_t    term_t;
#endif

#if _WIN32
#ifndef THREADS
typedef int pthread_t;
#endif
#endif

typedef uintptr_t		word;		/* Anonymous 4 byte object */

typedef int bool;


#define GLOBAL_LD (LOCAL_PL_local_data_p)

#if !defined(O_PLMT) && !defined(YAPOR)
#define LOCAL_LD (GLOBAL_LD)
#define LD (GLOBAL_LD)
#define ARG1_LD   void
#define ARG_LD
#define GET_LD
#define PRED_LD
#define PASS_LD
#define PASS_LD1 
#define IGNORE_LD

#define REGS_FROM_LD
#define LD_FROM_REGS

#else

#define LOCAL_LD (__PL_ld)
#define LD	  LOCAL_LD

#define GET_LD	  CACHE_REGS struct PL_local_data *__PL_ld = GLOBAL_LD;
#define ARG1_LD   struct PL_local_data *__PL_ld

#define ARG_LD    , ARG1_LD
#define PASS_LD1  LD
#define PASS_LD   , LD
#define PRED_LD   GET_LD
#define IGNORE_LD (void)__PL_ld;

#define REGS_FROM_LD  struct regstore_t *regcache = __PL_ld->reg_cache;
#define LD_FROM_REGS struct PL_local_data *__PL_ld = LOCAL_PL_local_data_p;

#endif

static inline Term
OpenList(int n USES_REGS)
{
  Term t;
  BACKUP_H();

  while (H+2*n > ASP-1024) {
    if (!Yap_dogc( 0, NULL PASS_REGS )) {
      RECOVER_H();
      return FALSE;
    }
  }
  t = AbsPair(H);
  H += 2*n;

  RECOVER_H();
  return t;
}

static inline Term
ExtendList(Term t0, Term inp)
{
  Term t;
  CELL *ptr = RepPair(t0);
  BACKUP_H();

  ptr[0] = inp;
  ptr[1] = AbsPair(ptr+2);
  t = AbsPair(ptr+2);

  RECOVER_H();
  return t;
}

static inline int
CloseList(Term t0, Term tail)
{
  CELL *ptr = RepPair(t0);

  RESET_VARIABLE(ptr-1);
  if (!Yap_unify((Term)(ptr-1), tail))
    return FALSE;
  return TRUE;
}


#endif


