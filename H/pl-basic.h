// SWI stuff that is needed everywhere

#ifndef PL_BASIC_H

#define PL_BASIC_H

#if USE_GMP
#define O_GMP 1
#endif

#define O_LOCALE 1

#ifndef PL_CONSOLE
#define PL_KERNEL 1
#endif

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


#if  !defined(_FLI_H_INCLUDED)

typedef int bool;

/* this must be called if we're not including SWI-Prolog first */
/* should be a copy of what we can find in SWI-Prolog.h */

#ifndef PL_HAVE_TERM_T
#define PL_HAVE_TERM_T
typedef	uintptr_t    term_t;
#endif
typedef	struct mod_entry *module_t;
typedef struct DB_STRUCT *record_t;
typedef uintptr_t	atom_t;
typedef	struct pred_entry    *predicate_t;
typedef struct  open_query_struct *qid_t;
typedef uintptr_t    functor_t;
typedef int     (*PL_agc_hook_t)(atom_t);
typedef unsigned long	foreign_t;	/* return type of foreign functions */
typedef wchar_t pl_wchar_t;             /* wide character support */
#include <inttypes.h>			/* more portable than stdint.h */
#if  !defined(_MSC_VER) 
typedef uintptr_t	PL_fid_t;	/* opaque foreign context handle */
#endif
typedef int  (*PL_dispatch_hook_t)(int fd);


#define O_STRING 1

typedef void *pl_function_t;

#define fid_t PL_fid_t			/* avoid AIX name-clash */

#endif

#ifndef COMMON
#define COMMON(X) X
#endif


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
#define LD_FROM_CACHE 

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

#endif


