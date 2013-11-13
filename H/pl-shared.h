
#ifndef PL_SHARED_INCLUDE

#define PL_SHARED_INCLUDE

#if defined(__GNUC__) && !defined(MAY_ALIAS)
#define MAY_ALIAS __attribute__ ((__may_alias__))
#else
#define MAY_ALIAS
#endif

#define COMMON(X) X

#ifndef PL_HAVE_TERM_T
#define PL_HAVE_TERM_T
typedef	uintptr_t    term_t;
#endif

#ifndef _FLI_H_INCLUDED

#ifdef __WINDOWS__
#include <windows.h>
#include <windows/uxnt.h>
#endif

typedef	void *record_t;
typedef struct mod_entry *module_t;
typedef uintptr_t	atom_t;

typedef int  (*PL_dispatch_hook_t)(int fd);

typedef	struct pred_entry    *predicate_t;

typedef uintptr_t	PL_fid_t;	/* opaque foreign context handle */
#define fid_t PL_fid_t			/* avoid AIX name-clash */

typedef uintptr_t		word;		/* Anonymous 4 byte object */
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

#endif

#if _WIN32
#ifndef THREADS
typedef int pthread_t;
#endif
#endif

#include "SWI-Stream.h"

		 /*******************************
		 *	    STREAM I/O		*
		 *******************************/

#define REDIR_MAGIC 0x23a9bef3

typedef struct redir_context
{ int		magic;			/* REDIR_MAGIC */
  IOSTREAM     *stream;			/* temporary output */
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


/* vsc: global variables */
#include "pl-global.h"

// THIS HAS TO BE ABSTRACTED

#define true(s, a)		((s)->flags & (a))
#define false(s, a)		(!true((s), (a)))
#define set(s, a)		((s)->flags |= (a))
#define clear(s, a)		((s)->flags &= ~(a))
#ifdef  DEBUG
/* should have messages here */
#undef DEBUG
#define DEBUG(LEVEL, COMMAND)
#else
#define DEBUG(LEVEL, COMMAND)
#endif

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

/* Flags on module.  Most of these flags are copied to the read context
   in pl-read.c.
*/

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

extern unsigned int
getUnknownModule(module_t m);

#define debugstatus LOCAL_debugstatus        /* status of the debugger */

#define truePrologFlag(flag)      true(&LD->prolog_flag.mask, flag)
#define setPrologFlagMask(flag)   set(&LD->prolog_flag.mask, flag)
#define clearPrologFlagMask(flag) clear(&LD->prolog_flag.mask, flag)

#endif /* PL_SHARED_INCLUDE */
