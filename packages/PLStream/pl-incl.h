
#include "config.h"

#if USE_GMP
#define O_GMP 1
#endif

#define PL_KERNEL 1

#ifdef __MINGW32__
#define O_XOS 1
#endif

#ifdef THREADS
#define O_PLMT 1
#endif

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
typedef int bool;
typedef int			Char;		/* char that can pass EOF */
typedef uintptr_t		word;		/* Anonymous 4 byte object */

#if SIZE_DOUBLE==SIZEOF_INT_P
#define WORDS_PER_DOUBLE 1
#else
#define WORDS_PER_DOUBLE 2
#endif

// numbers

typedef enum
{ V_INTEGER,				/* integer (64-bit) value */
#ifdef O_GMP    
  V_MPZ,				/* mpz_t */
  V_MPQ,				/* mpq_t */
#endif
  V_FLOAT				/* Floating point number (double) */
} numtype;

typedef struct
{ numtype type;				/* type of number */
  union { double f;			/* value as real */
	  int64_t i;			/* value as integer */
	  word  w[WORDS_PER_DOUBLE];	/* for packing/unpacking the double */
#ifdef O_GMP
	  mpz_t mpz;			/* GMP integer */
	  mpq_t mpq;			/* GMP rational */
#endif
	} value;
} number, *Number;

#define TOINT_CONVERT_FLOAT     0x1     /* toIntegerNumber() */
#define TOINT_TRUNCATE          0x2

#ifdef O_GMP
#define intNumber(n)    ((n)->type <=  V_MPZ)
#else
#define intNumber(n)    ((n)->type <  V_FLOAT)
#endif
#define floatNumber(n)  ((n)->type >= V_FLOAT)

typedef enum
{ NUM_ERROR = FALSE,                    /* Syntax error */
  NUM_OK    = TRUE,                     /* Ok */
  NUM_FUNDERFLOW = -1,                  /* Float underflow */
  NUM_FOVERFLOW = -2,                   /* Float overflow */
  NUM_IOVERFLOW = -3                    /* Integer overflow */
} strnumstat;


#define Arg(N)  (PL__t0+((n)-1))
#define A1      (PL__t0)
#define A2      (PL__t0+1)
#define A3      (PL__t0+2)
#define A3      (PL__t0+2)
#define A4      (PL__t0+3)
#define A5      (PL__t0+4)
#define A6      (PL__t0+5)
#define A7      (PL__t0+6)
#define A8      (PL__t0+7)
#define A9      (PL__t0+8)
#define A10     (PL__t0+9)


/* atom_t macro layer */
#define NULL_ATOM ((atom_t)0)
#if __YAP_PROLOG__
#include "dswiatoms.h"
#else
#include "atoms.h"
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#define COMMON(X) X

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif
#include <setjmp.h>
#include <assert.h>
#if HAVE_SYS_PARAM_H
#include <sys/param.h> //MAXPATHLEN
#endif
#if __YAP_PROLOG__
#include "pl-yap.h"
#if _WIN32
typedef int pthread_t;
#define __WINDOWS__ 1
#else
#include <pthread.h>
#endif
#endif
typedef uintptr_t	PL_atomic_t;	/* same a word */

#define MAXSIGNAL	64

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


		/********************************
		*       UTILITIES               *
		*********************************/
#define ROUND(p, n) ((((p) + (n) - 1) & ~((n) - 1)))

		/********************************
		*       HASH TABLES             *
		*********************************/

#include "pl-table.h"

		/********************************
		*       OS		         *
		*********************************/

#include "pl-os.h"

		/********************************
		*       Error		         *
		*********************************/

#include "pl-error.h"

		/********************************
		*       Files		         *
		*********************************/

#include "pl-files.h"

		/********************************
		*       BUFFERS                 *
		*********************************/

#define BUFFER_RING_SIZE 	16	/* foreign buffer ring (pl-fli.c) */

#include "pl-buffer.h"

		 /*******************************
		 *	   OPTION LISTS		*
		 *******************************/

#include "pl-option.h"

		 /*******************************
		 *	  TEXT PROCESSING	*
		 *******************************/

typedef enum
{ CVT_ok = 0,				/* Conversion ok */
  CVT_wide,				/* Conversion needs wide characters */
  CVT_partial,				/* Input list is partial */
  CVT_nolist,				/* Input list is not a list */
  CVT_nocode,				/* List contains a non-code */
  CVT_nochar				/* List contains a non-char */
} CVT_status;

typedef struct
{ CVT_status status;
  word culprit;				/* for CVT_nocode/CVT_nochar */
} CVT_result;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operator types.  NOTE: if you change OP_*, check operatorTypeToAtom()!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define OP_MAXPRIORITY		1200	/* maximum operator priority */

#define OP_PREFIX  0
#define OP_INFIX   1
#define OP_POSTFIX 2
#define OP_MASK    0xf

#define	OP_FX	(0x10|OP_PREFIX)
#define OP_FY	(0x20|OP_PREFIX)
#define OP_XF	(0x30|OP_POSTFIX)
#define OP_YF	(0x40|OP_POSTFIX)
#define OP_XFX	(0x50|OP_INFIX)
#define OP_XFY	(0x60|OP_INFIX)
#define OP_YFX	(0x70|OP_INFIX)

#define CHARESCAPE		(0x0004) /* module */

		 /*******************************
		 *	       COMPARE		*
		 *******************************/

/* Results from comparison operations.  Mostly used by compareStandard() */

#define CMP_ERROR  -2			/* Error (out of memory) */
#define CMP_LESS   -1			/* < */
#define CMP_EQUAL   0			/* == */
#define CMP_GREATER 1			/* > */
#define CMP_NOTEQ   2			/* \== */

		 /*******************************
		 *	     NUMBERVARS		*
		 *******************************/

typedef enum
{ AV_BIND,
  AV_SKIP,
  AV_ERROR
} av_action;

typedef struct
{ functor_t functor;			/* Functor to use ($VAR/1) */
  av_action on_attvar;			/* How to handle attvars */
  int	    singletons;			/* Write singletons as $VAR('_') */
} nv_options;


		 /*******************************
		 *	   GET-PROCEDURE	*
		 *******************************/

#define GP_FIND		0		/* find anywhere */
#define GP_FINDHERE	1		/* find in this module */
#define GP_CREATE	2		/* create (in this module) */
#define GP_DEFINE	4		/* define a procedure */
#define GP_RESOLVE	5		/* find defenition */

#define GP_HOW_MASK	0x0ff
#define GP_NAMEARITY	0x100		/* or'ed mask */
#define GP_HIDESYSTEM	0x200		/* hide system module */
#define GP_TYPE_QUIET	0x400		/* don't throw errors on wrong types */
#define GP_EXISTENCE_ERROR 0x800	/* throw error if proc is not found */
#define GP_QUALIFY	0x1000		/* Always module-qualify */

					/* get_functor() */
#define GF_EXISTING	1
#define GF_PROCEDURE	2		/* check for max arity */


		 /*******************************
		 *	   LIST BUILDING	*
		 *******************************/

#include "pl-privitf.h"

typedef int simpleMutex;

typedef struct counting_mutex
{ simpleMutex mutex;			/* mutex itself */
  const char *name;			/* name of the mutex */
  long count;				/* # times locked */
  long unlocked;			/* # times unlocked */
#ifdef O_CONTENTION_STATISTICS
  long collisions;			/* # contentions */
#endif
  struct counting_mutex *next;		/* next of allocated chain */
} counting_mutex;

typedef enum
{ CLN_NORMAL = 0,			/* Normal mode */
  CLN_ACTIVE,				/* Started cleanup */
  CLN_FOREIGN,				/* Foreign hooks */
  CLN_PROLOG,				/* Prolog hooks */
  CLN_SHARED,				/* Unload shared objects */
  CLN_DATA				/* Remaining data */
} cleanup_status;

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

typedef enum
{ LDATA_IDLE = 0,
  LDATA_SIGNALLED,
  LDATA_ANSWERING,
  LDATA_ANSWERED
} ldata_status_t;

typedef struct _PL_thread_info_t
{ int		    pl_tid;		/* Prolog thread id */
  size_t	    local_size;		/* Stack sizes */
  size_t	    global_size;
  size_t	    trail_size;
  size_t	    stack_size;		/* system (C-) stack */
  int		    (*cancel)(int id);	/* cancel function */
  int		    open_count;		/* for PL_thread_detach_engine() */
  bool		    detached;		/* detached thread */
  int		    status;		/* PL_THREAD_* */
  pthread_t	    tid;		/* Thread identifier */
  int		    has_tid;		/* TRUE: tid = valid */
#ifdef __linux__
  pid_t		    pid;		/* for identifying */
#endif
#ifdef __WINDOWS__
  unsigned long	    w32id;		/* Win32 thread HANDLE */
#endif
  struct PL_local_data  *thread_data;	/* The thread-local data  */
  module_t	    module;		/* Module for starting goal */
  record_t	    goal;		/* Goal to start thread */
  record_t	    return_value;	/* Value (term) returned */
  atom_t	    name;		/* Name of the thread */
  ldata_status_t    ldata_status;	/* status of forThreadLocalData() */
} PL_thread_info_t;



typedef struct tempfile *	TempFile; 	/* pl-os.c */
typedef struct canonical_dir *	CanonicalDir;	/* pl-os.c */
typedef struct on_halt *	OnHalt;		/* pl-os.c */
typedef struct extension_cell *	ExtensionCell;  /* pl-ext.c */
typedef struct initialise_handle * InitialiseHandle;

/* The GD global variable */
typedef struct {
  int io_initialised;
  cleanup_status cleaning;		/* Inside PL_cleanup() */

  pl_defaults_t	defaults;		/* system default settings */

 struct
  { Table       table;                  /* global (read-only) features */
  } prolog_flag;

#if THREADS
  struct
  { int		    	enabled;	/* threads are enabled */
  } thread;
#endif

  struct
  { Table		tmp_files;	/* Known temporary files */
    CanonicalDir	_canonical_dirlist;
    char *		myhome;		/* expansion of ~ */
    char *		fred;		/* last expanded ~user */
    char *		fredshome;	/* home of fred */
    OnHalt		on_halt_list;	/* list of onhalt hooks */
    int			halting;	/* process is shutting down */
    int			gui_app;	/* Win32: Application is a gui app */
    IOFUNCTIONS		iofunctions;	/* initial IO functions */
    IOFUNCTIONS 	org_terminal;	/* IO+Prolog terminal functions */
    IOFUNCTIONS		rl_functions;	/* IO+Terminal+Readline functions */
  } os;

  struct
  { size_t	heap;			/* heap in use */
    size_t	atoms;			/* No. of atoms defined */
    size_t	atomspace;		/* # bytes used to store atoms */
    size_t	stack_space;		/* # bytes on stacks */
#ifdef O_ATOMGC
    size_t	atomspacefreed;		/* Freed atom-space */
#endif
    int		functors;		/* No. of functors defined */
    int		predicates;		/* No. of predicates defined */
    int		modules;		/* No. of modules in the system */
    intptr_t	codes;			/* No. of byte codes generated */
#ifdef O_PLMT
    int		threads_created;	/* # threads created */
    int		threads_finished;	/* # finished threads */
    double	thread_cputime;		/* Total CPU time of threads */
#endif
    double	start_time;		/* When Prolog was started */
  } statistics;

  struct
  { atom_t *	array;			/* index --> atom */
    size_t	count;			/* elements in array */
    atom_t     *for_code[256];		/* code --> one-char-atom */
  } atoms;

  struct
  { ExtensionCell _ext_head;		/* head of registered extensions */
    ExtensionCell _ext_tail;		/* tail of this chain */

    InitialiseHandle initialise_head;	/* PL_initialise_hook() */
    InitialiseHandle initialise_tail;
    PL_dispatch_hook_t dispatch_events; /* PL_dispatch_hook() */

    int		  _loaded;		/* system extensions are loaded */
  } foreign;

#ifdef O_PLMT
  FreeChunk	    left_over_pool;	/* Left-over from threads */

  struct
  { struct _at_exit_goal *exit_goals;	/* Global thread_at_exit/1 goals */
    int		    	enabled;	/* threads are enabled */
    Table		mutexTable;	/* Name --> mutex table */
    int			mutex_next_id;	/* next id for anonymous mutexes */
    struct pl_mutex*	MUTEX_load;	/* The $load mutex */
#ifdef __WINDOWS__
    HINSTANCE	    	instance;	/* Win32 process instance */
#endif
    counting_mutex     *mutexes;	/* Registered mutexes */
    int			thread_max;	/* Maximum # threads */
    PL_thread_info_t  **threads;	/* Pointers to thread-info */
  } thread;
#endif /*O_PLMT*/

  struct				/* pl-format.c */
  { Table	predicates;
  } format;

  struct
  {/*  Procedure	dgarbage_collect1; */
/*     Procedure	catch3; */
/*     Procedure	true0; */
/*     Procedure	fail0; */
/*     Procedure	equals2;		/\* =/2 *\/ */
/*     Procedure	is2;			/\* is/2 *\/ */
/*     Procedure	strict_equal2;		/\* ==/2 *\/ */
/*     Procedure	event_hook1; */
/*     Procedure	exception_hook4; */
/*     Procedure	print_message2; */
/*     Procedure	foreign_registered2;	/\* $foreign_registered/2 *\/ */
/*     Procedure	prolog_trace_interception4; */
    predicate_t	portray;		/* portray/1 */
/*     Procedure   dcall1;			/\* $call/1 *\/ */
/*     Procedure	setup_call_catcher_cleanup4; /\* setup_call_catcher_cleanup/4 *\/ */
/*     Procedure	undefinterc4;		/\* $undefined_procedure/4 *\/ */
/*     Procedure   dthread_init0;		/\* $thread_init/0 *\/ */
/*     Procedure   dc_call_prolog0;	/\* $c_call_prolog/0 *\/ */
/* #ifdef O_ATTVAR */
/*     Procedure	dwakeup1;		/\* system:$wakeup/1 *\/ */
    predicate_t	portray_attvar1;	/* $attvar:portray_attvar/1 */ 
/* #endif */
/* #ifdef O_CALL_RESIDUE */
/*     Procedure	call_residue_vars2;	/\* $attvar:call_residue_vars/2 *\/ */
/* #endif */

/*     SourceFile  reloading;		/\* source file we are re-loading *\/ */
/*     int		active_marked;		/\* #prodedures marked active *\/ */
/*     int		static_dirty;		/\* #static dirty procedures *\/ */

/* #ifdef O_CLAUSEGC */
/*     DefinitionChain dirty;		/\* List of dirty static procedures *\/ */
/* #endif */
  } procedures;

} gds_t;

extern gds_t gds;

#define GD (&gds)
#define GLOBAL_LD (&gds)



typedef struct
{ unsigned long flags;                  /* Fast access to some boolean Prolog flags */
} pl_features_t;

#define truePrologFlag(flag)      true(&LD->prolog_flag.mask, flag)
#define setPrologFlagMask(flag)   set(&LD->prolog_flag.mask, flag)
#define clearPrologFlagMask(flag) clear(&LD->prolog_flag.mask, flag)


// LOCAL variables (heap will get this form LOCAL

#define FT_ATOM		0		/* atom feature */
#define FT_BOOL		1		/* boolean feature (true, false) */
#define FT_INTEGER	2		/* integer feature */
#define FT_TERM		3		/* term feature */
#define FT_INT64	4		/* passed as int64_t */
#define FT_MASK		0x0f		/* mask to get type */

#define FF_READONLY	0x10		/* feature is read-only */
#define FF_KEEP		0x20		/* keep value it already set */

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

typedef enum
{ OCCURS_CHECK_FALSE = 0,
  OCCURS_CHECK_TRUE,
  OCCURS_CHECK_ERROR
} occurs_check_t;

typedef struct
{ atom_t	file;			/* current source file */
  int	  	line;			/* current line */
  int		linepos;		/* position in the line */
  int64_t	character;		/* current character location */
} source_location;


typedef struct exception_frame		/* PL_throw exception environments */
{ struct exception_frame *parent;	/* parent frame */
  jmp_buf	exception_jmp_env;	/* longjmp environment */
} exception_frame;

#define EXCEPTION_GUARDED(code, cleanup) \
	{ exception_frame __throw_env; \
	  __throw_env.parent = LD->exception.throw_environment; \
	  if ( setjmp(__throw_env.exception_jmp_env) != 0 ) \
	  { LD->exception.throw_environment = __throw_env.parent; \
	    cleanup; \
	  } else \
	  { LD->exception.throw_environment = &__throw_env; \
	    code; \
	    LD->exception.throw_environment = __throw_env.parent; \
	  } \
	}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
At times an abort is not allowed because the heap  is  inconsistent  the
programmer  should  call  startCritical  to start such a code region and
endCritical to end it.

MT/TBD: how to handle this gracefully in the multi-threading case.  Does
it mean anything?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* vsc: needs defining */
#define startCritical  TRUE
#define endCritical    TRUE

/* The LD macro layer */
typedef struct PL_local_data {

  struct				/* Local IO stuff */
  { IOSTREAM *streams[6];		/* handles for standard streams */
    struct input_context *input_stack;	/* maintain input stream info */
    struct output_context *output_stack; /* maintain output stream info */
  } IO;

  struct
  { Table	  table;		/* Feature table */
    pl_features_t mask;			/* Masked access to booleans */
    int		  write_attributes;	/* how to write attvars? */
    occurs_check_t occurs_check;	/* Unify and occurs check */
  } feature;

  source_location read_source;		/* file, line, char of last term */

  struct
  { int		active;			/* doing pipe I/O */
    jmp_buf	context;		/* context of longjmp() */
  } pipe;

  struct
  { atom_t	current;		/* current global prompt */
    atom_t	first;			/* how to prompt first line */
    int		first_used;		/* did we do the first line? */
    int		next;			/* prompt on next read operation */
  } prompt;

  struct
  { Table         table;                /* Feature table */
    pl_features_t mask;                 /* Masked access to booleans */
    int           write_attributes;     /* how to write attvars? */
    occurs_check_t occurs_check;        /* Unify and occurs check */
  } prolog_flag;

  void *        glob_info;              /* pl-glob.c */
  IOENC		encoding;		/* default I/O encoding */

  struct
  { char *	_CWDdir;
    size_t	_CWDlen;
#ifdef __BEOS__
    status_t	dl_error;		/* dlopen() emulation in pl-beos.c */
#endif
    int		rand_initialised;	/* have we initialised random? */
  } os;

 struct
  { int64_t     pending;                /* PL_raise() pending signals */
    int         current;                /* currently processing signal */
    int         is_sync;                /* current signal is synchronous */
    record_t    exception;              /* Pending exception from signal */
#ifdef O_PLMT
    simpleMutex sig_lock;               /* lock delivery and processing */
#endif
  } signal;

  int		critical;		/* heap is being modified */

  struct
  { term_t	term;			/* exception term */
    term_t	bin;			/* temporary handle for exception */
    term_t	printed;		/* already printed exception */
    term_t	tmp;			/* tmp for errors */
    term_t	pending;		/* used by the debugger */
    int		in_hook;		/* inside exception_hook() */
    int		processing;		/* processing an exception */
    exception_frame *throw_environment;	/* PL_throw() environments */
  } exception;
  const char   *float_format;		/* floating point format */

  struct {
    buffer	_discardable_buffer;	/* PL_*() character buffers */
    buffer	_buffer_ring[BUFFER_RING_SIZE];
    int		_current_buffer_id;
  } fli;

#ifdef O_GMP
  struct
  { 
    int		persistent;		/* do persistent operations */
  } gmp;
#endif

}  PL_local_data_t;

#define usedStack(D) 0

#define features		(LD->feature.mask)

extern PL_local_data_t lds;

#define exception_term		(LD->exception.term)

// THIS HAS TO BE ABSTRACTED

#define LD (&lds)
#define LOCAL_LD (&lds)

#define ARG_LD
#define GET_LD
#define PRED_LD
#define PASS_LD

#define Suser_input             (LD->IO.streams[0])
#define Suser_output            (LD->IO.streams[1])
#define Suser_error             (LD->IO.streams[2])
#define Scurin                  (LD->IO.streams[3])
#define Scurout                 (LD->IO.streams[4])
#define Sprotocol               (LD->IO.streams[5])
#define Sdin                    Suser_input             /* not used for now */
#define Sdout                   Suser_output

#define source_line_no		(LD->read_source.line)
#define source_file_name	(LD->read_source.file)


/* Support PL_LOCK in the interface */
#if THREADS

typedef pthread_mutex_t simpleMutex;

#define simpleMutexInit(p)	pthread_mutex_init(p, NULL)
#define simpleMutexDelete(p)	pthread_mutex_destroy(p)
#define simpleMutexLock(p)	pthread_mutex_lock(p)
#define simpleMutexUnlock(p)	pthread_mutex_unlock(p)

extern counting_mutex _PL_mutexes[];	/* Prolog mutexes */

#define L_MISC		0
#define L_ALLOC		1
#define L_ATOM		2
#define L_FLAG	        3
#define L_FUNCTOR	4
#define L_RECORD	5
#define L_THREAD	6
#define L_PREDICATE	7
#define L_MODULE	8
#define L_TABLE		9
#define L_BREAK	       10
#define L_FILE	       11
#define L_PLFLAG      12
#define L_OP	       13
#define L_INIT	       14
#define L_TERM	       15
#define L_GC	       16
#define L_AGC	       17
#define L_FOREIGN      18
#define L_OS	       19

#define IF_MT(id, g) if ( id == L_THREAD || GD->thread.enabled ) g

#ifdef O_CONTENTION_STATISTICS
#define countingMutexLock(cm) \
	do \
	{ if ( pthread_mutex_trylock(&(cm)->mutex) == EBUSY ) \
	  { (cm)->collisions++; \
	    pthread_mutex_lock(&(cm)->mutex); \
	  } \
	  (cm)->count++; \
	} while(0)
#else
#define countingMutexLock(cm) \
	do \
	{ simpleMutexLock(&(cm)->mutex); \
	  (cm)->count++; \
	} while(0)
#endif
#define countingMutexUnlock(cm) \
	do \
	{ (cm)->unlocked++; \
	  assert((cm)->unlocked <= (cm)->count); \
	  simpleMutexUnlock(&(cm)->mutex); \
	} while(0)

#define PL_LOCK(id)   IF_MT(id, countingMutexLock(&_PL_mutexes[id]))
#define PL_UNLOCK(id) IF_MT(id, countingMutexUnlock(&_PL_mutexes[id]))

#else
#define PL_LOCK(X)		
#define PL_UNLOCK(X)		
#endif


#ifndef TRUE
#define TRUE			1
#define FALSE			0
#endif
#define succeed			return TRUE
#define fail			return FALSE
#define TRY(goal)		if ((goal) == FALSE) fail


extern int fileerrors;

extern int ttymode;

#define CHARESCAPE_FEATURE	  0x00001 /* handle \ in atoms */
#define GC_FEATURE		  0x00002 /* do GC */
#define TRACE_GC_FEATURE	  0x00004 /* verbose gc */
#define TTY_CONTROL_FEATURE	  0x00008 /* allow for tty control */
#define READLINE_FEATURE	  0x00010 /* readline is loaded */
#define DEBUG_ON_ERROR_FEATURE	  0x00020 /* start tracer on error */
#define REPORT_ERROR_FEATURE	  0x00040 /* print error message */
#define FILE_CASE_FEATURE	  0x00080 /* file names are case sensitive */
#define FILE_CASE_PRESERVING_FEATURE 0x0100 /* case preserving file names */
#define DOS_FILE_NAMES_FEATURE    0x00200 /* dos (8+3) file names */
#define ISO_FEATURE		  0x00800 /* Strict ISO compliance */
#define OPTIMISE_FEATURE	  0x01000 /* -O: optimised compilation */
#define FILEVARS_FEATURE	  0x02000 /* Expand $var and ~ in filename */
#define AUTOLOAD_FEATURE	  0x04000 /* do autoloading */
#define CHARCONVERSION_FEATURE	  0x08000 /* do character-conversion */
#define LASTCALL_FEATURE	  0x10000 /* Last call optimization enabled? */
#define EX_ABORT_FEATURE	  0x20000 /* abort with exception */
#define BACKQUOTED_STRING_FEATURE 0x40000 /* `a string` */
#define SIGNALS_FEATURE		  0x80000 /* Handle signals */
#define DEBUGINFO_FEATURE	  0x100000 /* generate debug info */

int    defFeature(const char *c, int f, ...);

int    trueFeature(int f);

		 /*******************************
		 *	      WAKEUP		*
		 *******************************/

#define WAKEUP_STATE_WAKEUP    0x1
#define WAKEUP_STATE_EXCEPTION 0x2
#define WAKEUP_STATE_SKIP_EXCEPTION 0x4

typedef struct wakeup_state
{ fid_t		fid;			/* foreign frame reference */
  int		flags;
} wakeup_state;


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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Defining built-in predicates using the new interface 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define EOS '\0'
#define ESC			((char) 27)
#define streq(s, q)		((strcmp((s), (q)) == 0))

#define CHAR_MODE 0		/* See PL_unify_char() */
#define CODE_MODE 1
#define BYTE_MODE 2


/* string stuff */
		 /*******************************
		 *	  STRING SUPPORT	*
		 *******************************/
char *		store_string(const char *s);
void 		remove_string(char *s);


/* from foreign interface */
		 /*******************************
		 *	  FILENAME SUPPORT	*
		 *******************************/

#define PL_FILE_ABSOLUTE	0x01	/* return absolute path */
#define PL_FILE_OSPATH		0x02	/* return path in OS notation */
#define PL_FILE_SEARCH		0x04	/* use file_search_path */
#define PL_FILE_EXIST		0x08	/* demand file to exist */
#define PL_FILE_READ		0x10	/* demand read-access */
#define PL_FILE_WRITE		0x20	/* demand write-access */
#define PL_FILE_EXECUTE		0x40	/* demand execute-access */
#define PL_FILE_NOERRORS	0x80	/* do not raise exceptions */


#define PL_FA_ISO		(0x20)	/* Internal: ISO core predicate */

		/********************************
		*       READ WARNINGS           *
		*********************************/

#define ReadingSource (source_line_no > 0 && \
		       source_file_name != NULL_ATOM)


#include <pl-text.h>

typedef double			real;

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

#define forwards static		/* forwards function declarations */

/* uxnt package interface */
#if defined(__YAP_PROLOG__) && defined(__MINGW32__)
#define O_XOS 1

#define _XOS_ISFILE	0x01
#define _XOS_ISDIR	0x02

#define _XOS_FILE	0x0001		/* is a file */
#define _XOS_DIR	0x0002		/* is a directory */

#define XOS_DOWNCASE	0x01		/* _xos_canonical_filename() */

#ifndef __WINDOWS__
#define __WINDOWS__ 1
#endif

#endif

extern int PL_unify_char(term_t chr, int c, int how);
extern int PL_get_char(term_t chr, int *c, int eof);
extern int PL_get_text(term_t l, PL_chars_t *text, int flags);
extern void PL_cleanup_fork(void);
extern int PL_rethrow(void);
extern void PL_get_number(term_t l, number *n);
extern int PL_unify_atomic(term_t t, PL_atomic_t a);

#define _PL_get_arg(X,Y,Z) PL_get_arg(X,Y,Z)
#define _PL_unify_atomic PL_unify_atomic
extern IOSTREAM **			/* provide access to Suser_input, */
  _PL_streams(void);			/* Suser_output and Suser_error */

#define PL_get_text__LD PL_get_text
#define getInputStream__LD getInputStream
extern int get_atom_text(atom_t atom, PL_chars_t *text);
extern int get_string_text(word w, PL_chars_t *text);
extern char *format_float(double f, char *buf);

/**** stuff from pl-ctype.c ****/
extern IOENC initEncoding(void);

/**** stuff from pl-error.c ****/
extern int PL_get_bool_ex(term_t t, int *i);
extern int PL_get_nchars_ex(term_t t, size_t *len, char **s, unsigned int flags);
extern int PL_get_chars_ex(term_t t, char **s, unsigned int flags);
extern int PL_get_atom_ex(term_t t, atom_t *a);
extern int PL_get_integer_ex(term_t t, int *i);
extern int PL_get_long_ex(term_t t, long *i);
extern int PL_get_int64_ex(term_t t, int64_t *i);
extern int PL_get_intptr_ex(term_t t, intptr_t *i);
extern int PL_get_bool_ex(term_t t, int *i);
extern int PL_get_float_ex(term_t t, double *f);
extern int PL_get_char_ex(term_t t, int *p, int eof);
extern int PL_unify_list_ex(term_t l, term_t h, term_t t);
extern int PL_unify_nil_ex(term_t l);
extern int PL_get_list_ex(term_t l, term_t h, term_t t);
extern int PL_get_nil_ex(term_t l);
extern int PL_unify_bool_ex(term_t t, bool val);
extern int PL_unify_bool_ex(term_t t, bool val);
extern int PL_get_bool_ex(term_t t, int *i);
extern int PL_get_integer_ex(term_t t, int *i);
extern int PL_get_module_ex(term_t t, module_t *m);

/**** stuff from pl-file.c ****/
extern void initIO(void);

extern void dieIO(void);
extern void protocol(const char *str, size_t n);
extern bool readLine(IOSTREAM *in, IOSTREAM *out, char *buffer);
extern bool tellString(char **s, size_t *size, IOENC enc);
extern bool tellString(char **s, size_t *size, IOENC enc);
extern bool toldString(void);

extern int setupOutputRedirect(term_t to, redir_context *ctx, int redir);
extern void discardOutputRedirect(redir_context *ctx);
extern int closeOutputRedirect(redir_context *ctx);

extern IOENC atom_to_encoding(atom_t);

void closeFiles(int);
atom_t PrologPrompt(void);
word pl_current_input(term_t);
word pl_current_output(term_t);
word pl_exists_file(term_t name);
char *DirName(const char *f, char *dir);
void			outOfCore(void);

word pl_noprotocol(void);

IOSTREAM *PL_current_input(void);
IOSTREAM *PL_current_output(void);

int reportStreamError(IOSTREAM *s);

PL_EXPORT(int)  	PL_unify_stream(term_t t, IOSTREAM *s);
PL_EXPORT(int)  	PL_unify_stream_or_alias(term_t t, IOSTREAM *s);
PL_EXPORT(int)  	PL_get_stream_handle(term_t t, IOSTREAM **s);
PL_EXPORT(void)  	PL_write_prompt(int);
PL_EXPORT(int) 		PL_release_stream(IOSTREAM *s);

COMMON(atom_t) 		fileNameStream(IOSTREAM *s);
COMMON(int) 		streamStatus(IOSTREAM *s);

COMMON(int) 		getOutputStream(term_t t, IOSTREAM **s);
COMMON(int) 		getInputStream__LD(term_t t, IOSTREAM **s ARG_LD);
#define getInputStream(t, s)	getInputStream__LD(t, s PASS_LD)
COMMON(void) 		pushOutputContext(void);
COMMON(void) 		popOutputContext(void);
COMMON(int) 		getSingleChar(IOSTREAM *s, int signals);

COMMON(void) 		prompt1(atom_t prompt);
COMMON(atom_t)		encoding_to_atom(IOENC enc);
COMMON(int) 		pl_see(term_t f);
COMMON(int) 		pl_seen(void);

/**** stuff from pl-error.c ****/
extern void		outOfCore(void);
extern void		fatalError(const char *fm, ...);
extern int		callProlog(void * module, term_t goal, int flags, term_t *ex);
extern word notImplemented(char *name, int arity);

/**** stuff from pl-ctype.c ****/
extern void  initCharTypes(void);

/**** stuff from pl-fmt.c ****/
COMMON(word) 		pl_current_format_predicate(term_t chr, term_t descr,
					    control_t h);
COMMON(intptr_t) 	lengthList(term_t list, int errors);
COMMON(word) 		pl_format_predicate(term_t chr, term_t descr);
COMMON(word) 		pl_format(term_t fmt, term_t args);
COMMON(word) 		pl_format3(term_t s, term_t fmt, term_t args);

/**** stuff from pl-glob.c ****/
extern void  initGlob(void);

/**** stuff from pl-os.c ****/
extern void cleanupOs(void);
extern void PL_clock_wait_ticks(long waited);
extern void setOSFeatures(void);
extern uintptr_t FreeMemory(void);
extern uint64_t _PL_Random(void);
extern void RemoveTemporaryFiles(void);
extern int Pause(real t);
char *findExecutable(const char *av0, char *buffer);

void setOSPrologFlags(void);
void setRandom(unsigned int *seedp);
char *canoniseFileName(char *path);
char *canonisePath(char *path);
void PL_changed_cwd(void);
struct tm *LocalTime(long *t, struct tm *r);
size_t getenv3(const char *name, char *buf, size_t len);
int Setenv(char *name, char *value);
int Unsetenv(char *name);
int System(char *cmd);
bool expandVars(const char *pattern, char *expanded, int maxlen);

/**** SWI stuff (emulated in pl-yap.c) ****/
extern int writeAtomToStream(IOSTREAM *so, atom_t at);
extern int valueExpression(term_t t, Number r ARG_LD);
extern word lookupAtom(const char *s, size_t len);
extern atom_t	lookupUCSAtom(const pl_wchar_t *s, size_t len);
extern int toIntegerNumber(Number n, int flags);
extern int get_atom_ptr_text(Atom a, PL_chars_t *text);
extern int warning(const char *fm, ...);

/**** stuff from pl-files.c ****/
void initFiles(void);
int RemoveFile(const char *path);
int PL_get_file_name(term_t n, char **namep, int flags);
PL_EXPORT(int)		PL_get_file_nameW(term_t n, wchar_t **name, int flags);

COMMON(int) 		unifyTime(term_t t, time_t time);

COMMON(char)		digitName(int n, int small);

/**** stuff from pl-utf8.c ****/
size_t utf8_strlen(const char *s, size_t len);

/**** stuff from pl-write.c ****/
COMMON(char *) 		varName(term_t var, char *buf);
COMMON(int)		writeUCSAtom(IOSTREAM *fd, atom_t atom, int flags);
COMMON(word) 		pl_nl1(term_t stream);
COMMON(word) 		pl_nl(void);
COMMON(int) 		writeAttributeMask(atom_t name);
COMMON(word) 		pl_write_term(term_t term, term_t options);
COMMON(word) 		pl_write_term3(term_t stream,
			       term_t term, term_t options);
COMMON(word) 		pl_print(term_t term);
COMMON(word) 		pl_write2(term_t stream, term_t term);
COMMON(word) 		pl_writeq2(term_t stream, term_t term);
COMMON(word) 		pl_print2(term_t stream, term_t term);
COMMON(word) 		pl_writeln(term_t term);
COMMON(word) 		pl_write_canonical2(term_t stream, term_t term);


/* empty stub */
void setPrologFlag(const char *name, int flags, ...);
void PL_set_prolog_flag(const char *name, int flags, ...);

COMMON(int)		saveWakeup(wakeup_state *state, int forceframe ARG_LD);
COMMON(void)		restoreWakeup(wakeup_state *state ARG_LD);

COMMON(int)		skip_list(Word l, Word *tailp ARG_LD);
COMMON(int) 		priorityOperator(Module m, atom_t atom);
COMMON(int) 		currentOperator(Module m, atom_t name, int kind,
				int *type, int *priority);
COMMON(int) 		numberVars(term_t t, nv_options *opts, int n ARG_LD);

COMMON(Buffer)		codes_or_chars_to_buffer(term_t l, unsigned int flags,
						 int wide, CVT_result *status);

COMMON(int)		uflagsW(int code);

static inline word
setBoolean(int *flag, term_t old, term_t new)
{ if ( !PL_unify_bool_ex(old, *flag) ||
       !PL_get_bool_ex(new, flag) )
    fail;

  succeed;
}

static inline word
setInteger(int *flag, term_t old, term_t new)
{ if ( !PL_unify_integer(old, *flag) ||
       !PL_get_integer_ex(new, flag) )
    fail;

  succeed;
}

extern const PL_extension PL_predicates_from_ctype[];
extern const PL_extension PL_predicates_from_file[];
extern const PL_extension PL_predicates_from_files[];
extern const PL_extension PL_predicates_from_glob[];
extern const PL_extension PL_predicates_from_write[];
extern const PL_extension PL_predicates_from_read[];



