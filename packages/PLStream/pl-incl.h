
#include "config.h"

#define PL_KERNEL 1

#ifdef __MINGW32__
#define O_XOS 1
#endif

#include <SWI-Prolog.h>
typedef int bool;

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
#endif


		/********************************
		*       UTILITIES               *
		*********************************/
#define ROUND(p, n) ((((p) + (n) - 1) & ~((n) - 1)))

		/********************************
		*       HASH TABLES             *
		*********************************/

#include "pl-table.h"
#include "SWI-Stream.h"
#include "pl-os.h"
#include "pl-error.h"

		/********************************
		*       BUFFERS                 *
		*********************************/

#include "pl-buffer.h"

		 /*******************************
		 *	   OPTION LISTS		*
		 *******************************/

#include "pl-opts.h"

		 /*******************************
		 *	   LIST BUILDING	*
		 *******************************/

#include "pl-privitf.h"

// numbers

typedef enum
{ V_INTEGER,				/* integer (64-bit) value */
#ifdef O_GMP    
  V_MPZ,				/* mpz_t */
  V_MPQ,				/* mpq_t */
#endif
  V_REAL				/* Floating point number (double) */
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

typedef enum
{ CLN_NORMAL = 0,			/* Normal mode */
  CLN_ACTIVE,				/* Started cleanup */
  CLN_FOREIGN,				/* Foreign hooks */
  CLN_PROLOG,				/* Prolog hooks */
  CLN_SHARED,				/* Unload shared objects */
  CLN_DATA				/* Remaining data */
} cleanup_status;

typedef struct tempfile *	TempFile; 	/* pl-os.c */
typedef struct canonical_dir *	CanonicalDir;	/* pl-os.c */
typedef struct on_halt *	OnHalt;		/* pl-os.c */
typedef struct extension_cell *	ExtensionCell;  /* pl-ext.c */
typedef struct initialise_handle * InitialiseHandle;

/* The GD global variable */
typedef struct {
  int io_initialised;
  cleanup_status cleaning;		/* Inside PL_cleanup() */

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
#define startCritical 
#define endCritical   

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

  buffer	discardable_buffer;	/* PL_*() character buffers */
  buffer	buffer_ring[BUFFER_RING_SIZE];
  int		current_buffer_id;

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
#define DEBUG(LEVEL, COMMAND)

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
extern char *format_float(double f, char *buf, const char *format);

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
extern int PL_get_module_ex(term_t name, module_t *m);
extern int PL_unify_bool_ex(term_t t, bool val);
extern int PL_unify_bool_ex(term_t t, bool val);
extern int PL_get_bool_ex(term_t t, int *i);
extern int PL_get_integer_ex(term_t t, int *i);

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

/**** stuff from pl-error.c ****/
extern void		outOfCore(void);
extern void		fatalError(const char *fm, ...);
extern void		printMessage(int type, ...);
extern int		callProlog(void * module, term_t goal, int flags, term_t *ex);
extern word notImplemented(char *name, int arity);

/**** stuff from pl-ctype.c ****/
extern void  initCharTypes(void);

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

/**** stuff from pl-utils.c ****/
bool stripostfix(char *s, char *e);

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

/**** stuff from pl-utf8.c ****/
size_t utf8_strlen(const char *s, size_t len);

/* empty stub */
void setPrologFlag(const char *name, int flags, ...);
void PL_set_prolog_flag(const char *name, int flags, ...);

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

#if defined(__SWI_PROLOG__)

static inline word
INIT_SEQ_CODES(size_t n)
{
  return allocGlobal(1+(n)*3);  /* TBD: shift */
}

static inline word
EXTEND_SEQ_CODES(word gstore, int c) {
  *gstore = consPtr(&gstore[1], TAG_COMPOUND|STG_GLOBAL);
  gstore++;
  *gstore++ = FUNCTOR_dot2;
  *gstore++ = consInt(c);
  return gstore;
}

static inline int 
CLOSE_SEQ_OF_CODES(word gstore, word lp, word A2, word A3)) {
    setVar(*gstore);
    gTop = gstore+1;

    a = valTermRef(A2);
    deRef(a);
    if ( !unify_ptrs(a, lp PASS_LD) )
      return FALSE;
    a = valTermRef(A3);
    deRef(a);
    if ( !unify_ptrs(a, gstore PASS_LD) )
      return FALSE;
    return TRUE;
}

#else

#endif
