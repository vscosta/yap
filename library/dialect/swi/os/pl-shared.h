
#ifndef PL_SHARED_H

#define PL_SHARED_H

#include "pl-types.h"

#ifndef __WINDOWS__
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(__MSYS__)
#define __WINDOWS__ 1
#endif
#endif

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
#define LD    LOCAL_LD

#define GET_LD    CACHE_REGS struct PL_local_data *__PL_ld = GLOBAL_LD;
#define ARG1_LD   struct PL_local_data *__PL_ld

#define ARG_LD    , ARG1_LD
#define PASS_LD1  LD
#define PASS_LD   , LD
#define PRED_LD   GET_LD
#define IGNORE_LD (void)__PL_ld;

#define REGS_FROM_LD  struct regstore_t *regcache = __PL_ld->reg_cache;
#define LD_FROM_REGS struct PL_local_data *__PL_ld = LOCAL_PL_local_data_p;

#endif

Atom                  YAP_AtomFromSWIAtom(atom_t at);
atom_t                YAP_SWIAtomFromAtom(Atom at);


static inline Term
OpenList(int n USES_REGS)
{
  Term t;
  BACKUP_H();

  while (HR+2*n > ASP-1024) {
    if (!Yap_dogc( 0, (Term *)NULL PASS_REGS )) {
      RECOVER_H();
      return FALSE;
    }
  }
  t = AbsPair(HR);
  HR += 2*n;

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

#ifndef __YAP_PROLOG__

#include <assert.h>

// SWI stuff that is needed everywhere

#ifndef __unix__
#if defined(_AIX) || defined(__APPLE__) || defined(__unix) || defined(__BEOS__) || defined(__NetBSD__)
#define __unix__ 1
#endif
#endif

#ifndef PL_CONSOLE
#define PL_KERNEL 1
#endif


// SWI Options
#define O_STRING		1
#define O_QUASIQUOTATIONS	1
#if HAVE_LOCALE_H && HAVE_SETLOCALE
//#define O_LOCALE		1
#endif
//#define O_ATOMGC		1
//#define O_CLAUSEGC		1
#ifdef HAVE_GMP_H
#define O_GMP			1
#endif
#ifdef __WINDOWS__
#define NOTTYCONTROL           TRUE
#define O_DDE 1
#define O_DLL 1
#define O_HASDRIVES 1
#define O_HASSHARES 1
#define O_XOS 1
#define O_RLC 1
#define EMULATE_DLOPEN  1
#endif

#ifdef THREADS
#define O_PLMT 1
#else
#ifdef _REENTRANT
#undef _REENTRANT
#endif
#endif

//#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#define COMMON(X) extern X

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
#ifndef WIN_PTHREADS_H
typedef int pthread_t;
#endif
#endif
#endif


typedef uintptr_t		word;		/* Anonymous 4 byte object */


#define GLOBAL_LD (LOCAL_PL_local_data_p)



		 /*******************************
		 *	    STREAM I/O		*
		 *******************************/

#define REDIR_MAGIC 0x23a9bef3

typedef struct redir_context
{ int		magic;			/* REDIR_MAGIC */
  struct io_stream     *stream;			/* temporary output */
  int		is_stream;		/* redirect to stream */
  int		redirected;		/* output is redirected */
  term_t	term;			/* redirect target */
  int		out_format;		/* output type */
  int		out_arity;		/* 2 for difference-list versions */
  size_t	size;			/* size of I/O buffer */
  char	       *data;			/* data written */
  char		buffer[1024];		/* fast temporary buffer */
} redir_context;

#include "pl-file.h"

#define EOS '\0'

		/********************************
		*       HASH TABLES             *
		*********************************/

#include "pl-table.h"

		/********************************
		*       BUFFERS                 *
		*********************************/

#define BUFFER_RING_SIZE 	16	/* foreign buffer ring (pl-fli.c) */

#include "pl-buffer.h"

typedef struct canonical_dir *	CanonicalDir;	/* pl-os.c */

typedef struct
{ char *state;				/* system's boot file */
  char *startup;			/* default user startup file */
  int  local;				/* default local stack size (K) */
  int  global;				/* default global stack size (K) */
  int  trail;				/* default trail stack size (K) */
  char *goal;				/* default initialisation goal */
  char *toplevel;			/* default top level goal */
  bool notty;				/* use tty? */
  char *arch;				/* machine/OS we are using */
  char *home;				/* systems home directory */
} pl_defaults_t;

typedef struct on_halt *	OnHalt;		/* pl-os.c */

typedef struct extension_cell *	ExtensionCell;  /* pl-ext.c */

typedef struct tempfile *	TempFile; 	/* pl-os.c */

typedef struct initialise_handle * InitialiseHandle;

		/********************************
		*       Operating System         *
		*********************************/

#include "pl-os.h"

		/********************************
		*       Table		         *
		*********************************/

#include "pl-table.h"

		/********************************
		*       LOCALE		         *
		*********************************/

#include "pl-locale.h"		/* Locale objects */

		/********************************
		*       GLOBALS		         *
		*********************************/

/* vsc: global variables */
#include "pl-global.h"

// THIS HAS TO BE ABSTRACTED

#ifndef YAP_CPP_INTERFACE
#define True(s, a)		((s)->flags & (a))
#define False(s, a)		(!True((s), (a)))
#define set(s, a)		((s)->flags |= (a))
#define clear(s, a)		((s)->flags &= ~(a))
#endif

#define P_QUASI_QUOTATION_SYNTAX	(0x00000004) /* <![Type[Quasi Quote]]> */
#define PLFLAG_CHARESCAPE           0x000001 /* handle \ in atoms */
#define PLFLAG_GC                   0x000002 /* do GC */
#define PLFLAG_TRACE_GC             0x000004 /* verbose gc */
#define PLFLAG_TTY_CONTROL          0x000008 /* allow for tty control */
#define PLFLAG_READLINE             0x000010 /* readline is loaded */
#define PLFLAG_DEBUG_ON_ERROR       0x000020 /* start tracer on error */
#define PLFLAG_REPORT_ERROR         0x000040 /* print error message */
#define PLFLAG_FILE_CASE            0x000080 /* file names are case sensitive */
#define PLFLAG_FILE_CASE_PRESERVING 0x000100 /* case preserving file names */
#define PLFLAG_DOS_FILE_NAMES       0x000200 /* dos (8+3) file names */
#define ALLOW_VARNAME_FUNCTOR       0x000400 /* Read Foo(x) as 'Foo'(x) */
#define PLFLAG_ISO                  0x000800 /* Strict ISO compliance */
#define PLFLAG_OPTIMISE             0x001000 /* -O: optimised compilation */
#define PLFLAG_FILEVARS             0x002000 /* Expand $var and ~ in filename */
#define PLFLAG_AUTOLOAD             0x004000 /* do autoloading */
#define PLFLAG_CHARCONVERSION       0x008000 /* do character-conversion */
#define PLFLAG_LASTCALL             0x010000 /* Last call optimization enabled?  */
#define PLFLAG_EX_ABORT             0x020000 /* abort with exception */
#define PLFLAG_BACKQUOTED_STRING    0x040000 /* `a string` */
#define PLFLAG_SIGNALS              0x080000 /* Handle signals */
#define PLFLAG_DEBUGINFO            0x100000 /* generate debug info */
#define PLFLAG_FILEERRORS           0x200000 /* Edinburgh file errors */
#define PLFLAG_WARN_OVERRIDE_IMPLICIT_IMPORT 0x200000 /* Warn overriding weak symbols */
#define PLFLAG_QUASI_QUOTES	    0x400000 /* Support quasi quotes */

/* Flags on module.  Most of these flags are copied to the read context
   in pl-read.c.
*/
#ifndef M_SYSTEM
#define M_SYSTEM		(0x0001) /* system module */
#define M_CHARESCAPE		(0x0002) /* module */
#define DBLQ_CHARS		(0x0004) /* "ab" --> ['a', 'b'] */
#define DBLQ_ATOM		(0x0008) /* "ab" --> 'ab' */
#define DBLQ_STRING		(0x0010) /* "ab" --> "ab" */
#define DBLQ_MASK		(DBLQ_CHARS|DBLQ_ATOM|DBLQ_STRING)
#define UNKNOWN_FAIL		(0x0020) /* module */
#define UNKNOWN_WARNING		(0x0040) /* module */
#define UNKNOWN_ERROR		(0x0080) /* module */
#define UNKNOWN_MASK		(UNKNOWN_ERROR|UNKNOWN_WARNING|UNKNOWN_FAIL)
#endif
extern unsigned int
getUnknownModule(module_t m);

/* keep in sync with style_name/1 in boot/prims.pl */

#define LONGATOM_CHECK      0x0001      /* read/1: error on intptr_t atoms */
#define SINGLETON_CHECK     0x0002      /* read/1: check singleton vars */
#define MULTITON_CHECK      0x0004      /* read/1: check multiton vars */
#define DISCONTIGUOUS_STYLE 0x0008      /* warn on discontiguous predicates */
#define DYNAMIC_STYLE       0x0010      /* warn on assert/retract active */
#define CHARSET_CHECK       0x0020      /* warn on unquoted characters */
#define SEMSINGLETON_CHECK  0x0040      /* Semantic singleton checking */
#define NOEFFECT_CHECK      0x0080      /* Check for meaningless statements */
#define VARBRANCH_CHECK     0x0100      /* warn on unbalanced variables */
#define MULTIPLE_CHECK      0x0200      /* warn on multiple file definitions for a predicate */
#define MAXNEWLINES         5           /* maximum # of newlines in atom */

#define debugstatus            (LD->_debugstatus)

#define truePrologFlag(flag)      True(&LD->prolog_flag.mask, flag)
#define setPrologFlagMask(flag)   set(&LD->prolog_flag.mask, flag)
#define clearPrologFlagMask(flag) clear(&LD->prolog_flag.mask, flag)

#ifndef YAP_CPP_INTERFACE
COMMON(int)		debugmode(debug_type newp, debug_type *old);
COMMON(int)		tracemode(debug_type newp, debug_type *old);
#endif
COMMON(void)		Yap_setCurrentSourceLocation( void *rd );

#define SIG_PROLOG_OFFSET	32	/* Start of Prolog signals */

#define SIG_EXCEPTION	  (SIG_PROLOG_OFFSET+0)
#ifdef O_ATOMGC
#define SIG_ATOM_GC	  (SIG_PROLOG_OFFSET+1)
#endif
#define SIG_GC		  (SIG_PROLOG_OFFSET+2)
#ifdef O_PLMT
#define SIG_THREAD_SIGNAL (SIG_PROLOG_OFFSET+3)
#endif
#define SIG_FREECLAUSES	  (SIG_PROLOG_OFFSET+4)
#define SIG_PLABORT	  (SIG_PROLOG_OFFSET+5)

extern int raiseSignal(PL_local_data_t *ld, int sig);


#endif /* YAP codee */

#endif /* PL_SHARED_INCLUDE */
