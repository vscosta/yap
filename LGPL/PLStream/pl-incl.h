
#include "config.h"
#include <SWI-Prolog.h>
/* atom_t macro layer */
#define NULL_ATOM ((atom_t)0)
#include "atoms.h"
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
#include "pl-stream.h"
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

/* The GD global variable */
struct {
  int io_initialised;
  cleanup_status cleaning;		/* Inside PL_cleanup() */

  struct
  { TempFile		_tmpfile_head;
    TempFile		_tmpfile_tail;
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
  { atom_t *	array;			/* index --> atom */
    size_t	count;			/* elements in array */
    atom_t     *for_code[256];		/* code --> one-char-atom */
  } atoms;

} gds;


#define GD (&gds)
#define GLOBAL_LD (&gds)

// LOCAL variables (heap will get this form LOCAL

#define FT_ATOM		0		/* atom feature */
#define FT_BOOL		1		/* boolean feature (true, false) */
#define FT_INTEGER	2		/* integer feature */
#define FT_TERM		3		/* term feature */
#define FT_INT64	4		/* passed as int64_t */
#define FT_MASK		0x0f		/* mask to get type */

#define FF_READONLY	0x10		/* feature is read-only */
#define FF_KEEP		0x20		/* keep value it already set */

typedef struct
{ unsigned long flags;			/* the feature flags */
} pl_features_t;

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
  { term_t	term;			/* exception term */
    term_t	bin;			/* temporary handle for exception */
    term_t	printed;		/* already printed exception */
    term_t	tmp;			/* tmp for errors */
    term_t	pending;		/* used by the debugger */
    int		in_hook;		/* inside exception_hook() */
    exception_frame *throw_environment;	/* PL_throw() environments */
  } exception;
  const char   *float_format;		/* floating point format */

  buffer	discardable_buffer;	/* PL_*() character buffers */
  buffer	buffer_ring[BUFFER_RING_SIZE];
  int		current_buffer_id;

}  PL_local_data_t;

#define features		(LD->feature.mask)

PL_local_data_t lds;

#define exception_term		(LD->exception.term)

// THIS HAS TO BE ABSTRACTED

#define LD (&lds)
#define LOCAL_LD (&lds)

#define ARG_LD
#define GET_LD
#define PRED_LD
#define PASS_LD

/* Support PL_LOCK in the interface */
#define PL_LOCK(X)
#define PL_UNLOCK(X)


#ifndef TRUE
#define TRUE			1
#define FALSE			0
#endif
#define succeed			return TRUE
#define fail			return FALSE
#define TRY(goal)		if ((goal) == FALSE) fail


atom_t source_file_name; /** source name of the current file that we are
			     consulting */
int source_line_no; /** guess.... */

IOSTREAM *  Suser_input;
IOSTREAM *    Suser_output;
IOSTREAM *    Suser_error;
IOSTREAM *    Scurin;		/* see/tell */
IOSTREAM *    Scurout;
IOSTREAM *    Sprotocol;			/* protocolling */

int fileerrors;

int ttymode;

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
#define ALLOW_VARNAME_FUNCTOR	  0x00400 /* Read Foo(x) as 'Foo'(x) */
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

#define PL_dispatch(FD, COM)
extern int PL_unify_char(term_t chr, int c, int how);
extern int PL_get_char(term_t chr, int *c, int eof);
extern int PL_get_text(term_t l, PL_chars_t *text, int flags);
extern void PL_cleanup_fork(void);
extern int PL_rethrow(void);
extern void PL_get_number(term_t l, number *n);
extern int PL_write_term(IOSTREAM *s, term_t term, int precedence, int flags);
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
extern int PL_release_stream(IOSTREAM *s);
extern int PL_get_stream_handle(term_t t, IOSTREAM **s);
extern int PL_unify_stream_or_alias(term_t t, IOSTREAM *s);
extern int PL_unify_stream(term_t t, IOSTREAM *s);
extern bool PL_open_stream(term_t handle, IOSTREAM *s);
extern void PL_write_prompt(int dowrite);

/**** stuff from pl-error.c ****/
extern void		outOfCore(void);
extern void		fatalError(const char *fm, ...);
extern void		printMessage(int type, ...);
extern int		callProlog(void * module, term_t goal, int flags, term_t *ex);
extern word notImplemented(char *name, int arity);

/**** stuff from pl-ctype.c ****/
extern void  initCharTypes(void);

/**** stuff from pl-os.c ****/
extern void cleanupOs(void);
extern void PL_clock_wait_ticks(intptr_t waited);
extern void setOSFeatures(void);
extern uintptr_t FreeMemory(void);
extern uint64_t _PL_Random(void);
extern void RemoveTemporaryFiles(void);
extern int Pause(real t);
char *findExecutable(const char *av0, char *buffer);

/**** SWI stuff (emulated in pl-yap.c) ****/
extern int writeAtomToStream(IOSTREAM *so, atom_t at);
extern int valueExpression(term_t t, Number r ARG_LD);
extern word lookupAtom(const char *s, size_t len);
extern atom_t	lookupUCSAtom(const pl_wchar_t *s, size_t len);
extern int toIntegerNumber(Number n, int flags);
extern int get_atom_ptr_text(Atom a, PL_chars_t *text);

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
