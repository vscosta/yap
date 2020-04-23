

#ifndef PL_INCL_H

#define PL_INCL_H 1

#ifndef __WINDOWS__
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(__MSYS__)
#define __WINDOWS__ 1
#endif
#endif

#ifdef __WINDOWS__

#ifndef __MSYS__
#include <winsock2.h>
#endif
#include <windows.h>


#if HAVE_XOS_H
#include <xos.h>			/* Windows POSIX enhancements */
#endif

#include "windows/uxnt.h"		/* More Windows POSIX enhancements */

#endif

#include "Yap.h"

#include "YapHeap.h"

/* define that we are in the pl-* code */
#define _PL_EMULATION_LAYER 1

/* include all stuff that is exported to yap */
#include "pl-shared.h"

#define PLVERSION YAP_NUMERIC_VERSION
#define PLNAME "yap"

#define SWIP "swi_"

/* PL internal magic */
typedef word *			Word;

/* SWI internal name for a predicate */
typedef struct pred_entry *      Procedure;      /* predicate */

#ifndef SWI_H

/* try not to pollute the SWI space */
#ifdef P
#undef P
#endif
#ifdef B
#undef B
#endif
#ifdef S
#undef S
#endif
#ifdef H
#undef H
#endif

#endif

/* swi code called from pl-incl.h */
/* should have messages here */
#ifdef  DEBUG
#undef DEBUG
#endif
#define DEBUG(LEVEL, COMMAND)

/* vsc: needs defining before getting rid of YAP locks */
static inline int
do_startCritical(void) {
  CACHE_REGS
  YAPEnterCriticalSection();
  return 1;
}
static inline int
do_endCritical(void) {
  CACHE_REGS
  YAPLeaveCriticalSection();
  return 1;
}
#define startCritical  do_startCritical()
#define endCritical do_endCritical()

#ifdef LOCK
#undef LOCK
#endif
#ifdef UNLOCK
#undef UNLOCK
#endif

#include <SWI-Stream.h>
#ifdef HAVE_ERRNO_H
#include <errno.h>
#else
extern int errno;
#endif

typedef int			Char;		/* char that can pass EOF */

#define usedStack(D) 0

#define exception_term         (LD->exception.term)

#ifdef Suser_input
#undef Suser_input
#endif
#ifdef Suser_output
#undef Suser_output
#endif
#ifdef Suser_error
#undef Suser_error
#endif

#define Suser_input             (LD->IO.streams[0])
#define Suser_output            (LD->IO.streams[1])
#define Suser_error             (LD->IO.streams[2])
#define Scurin                  (LD->IO.streams[3])
#define Scurout                 (LD->IO.streams[4])
#define Sprotocol               (LD->IO.streams[5])
#define Sdin                    Suser_input             /* not used for now */
#define Sdout                   Suser_output

#define source_file_name	(LD->read_source.file)
#define source_line_no          (LD->read_source.position.lineno)
#define source_line_pos         (LD->read_source.position.linepos)
#define source_char_no          (LD->read_source.position.charno)
#define source_byte_no          (LD->read_source.position.byteno)

#if SIZE_DOUBLE==SIZEOF_INT_P
#define WORDS_PER_DOUBLE 1
#else
#define WORDS_PER_DOUBLE 2
#endif

#define allocForeignState(size)                 ((void *)Yap_AllocCodeSpace(size))
#define freeForeignState(ptr, size)             Yap_FreeCodeSpace((void*)(ptr))

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

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif
#ifdef HAVE_LIMITS_H			/* get MAXPATHLEN */
#include <limits.h>
#endif
#include <setjmp.h>
#include <assert.h>
#if HAVE_SYS_PARAM_H
#include <sys/param.h> //MAXPATHLEN
#endif
#if __YAP_PROLOG__
#include "pl-yap.h"

#if _WIN32
#define __WINDOWS__ 1
#else
#include <pthread.h>
#endif
#endif
  typedef Term	PL_atomic_t;	/* same size as a word */

typedef struct record *		Record;

#define MAXSIGNAL	64

#define LOCAL_OVERFLOW    (-1)
#define GLOBAL_OVERFLOW   (-2)
#define TRAIL_OVERFLOW    (-3)
#define ARGUMENT_OVERFLOW (-4)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Foreign language interface definitions.  Note that these macros MUST  be
consistent  with  the  definitions  in  pl-itf.h, which is included with
users foreign language code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define NOTRACE PL_FA_NOTRACE
#define METAP    PL_FA_TRANSPARENT
#define NDET	PL_FA_NONDETERMINISTIC
#define VA	PL_FA_VARARGS
#define CREF	PL_FA_CREF
#define ISO	PL_FA_ISO

/********************************
		*       THREADS	             *
		*********************************/

#include "pl-thread.h"

#if O_PLMT
                 /*******************************
                 *             WINDOWS          *
                 *******************************/

#define WM_SIGNALLED (WM_USER+4201)     /* how to select a good number!? */
#endif


		/********************************
		*       UTILITIES               *
		*********************************/
#define ROUND(p, n) ((((p) + (n) - 1) & ~((n) - 1)))

		/********************************
		*       Error		         *
v		*********************************/

#define isDefinedProcedure(pred) TRUE // TBD
#include "pl-error.h"

		/********************************
		*       Files		         *
		*********************************/

#include "pl-files.h"

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
  int       numbered_check;             /* Check for already numbered */
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



// LOCAL variables (heap will get this form LOCAL

#define FT_ATOM         0               /* atom feature */
#define FT_BOOL         1               /* boolean feature (true, false) */
#define FT_INTEGER      2               /* integer feature */
#define FT_FLOAT        3               /* float feature */
#define FT_TERM         4               /* term feature */
#define FT_INT64        5               /* passed as int64_t */
#define FT_FROM_VALUE   0x0f            /* Determine type from value */
#define FT_MASK         0x0f            /* mask to get type */

#define SYSTEM_MODE         (LD->prolog_flag.access_level == ACCESS_LEVEL_SYSTEM)

#define PL_malloc_atomic malloc

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

#ifndef TRUE
#define TRUE			1
#define FALSE			0
#endif
#define succeed			return TRUE
#define fail			return FALSE
#define TRY(goal)		if ((goal) == FALSE) fail

/* Flags on module.  Most of these flags are copied to the read context
   in pl-read.c.
*/

#define M_SYSTEM                (0x0001) /* system module */
#define M_CHARESCAPE            (0x0002) /* module */
#define DBLQ_CHARS              (0x0004) /* "ab" --> ['a', 'b'] */
#define DBLQ_ATOM               (0x0008) /* "ab" --> 'ab' */
#define DBLQ_STRING             (0x0010) /* "ab" --> "ab" */
#ifdef DBLQ_MASK
#undef DBLQ_MASK
#endif
#define DBLQ_MASK               (DBLQ_CHARS|DBLQ_ATOM|DBLQ_STRING)
#define UNKNOWN_FAIL            (0x0020) /* module */
#define UNKNOWN_WARNING         (0x0040) /* module */
#define UNKNOWN_ERROR           (0x0080) /* module */
#define UNKNOWN_MASK            (UNKNOWN_ERROR|UNKNOWN_WARNING|UNKNOWN_FAIL)


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



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Defining built-in predicates using the new interface
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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

#define forwards static		/* forwards function declarations */

/* uxnt package interface */
#if defined(__YAP_PROLOG__) && defined(__MINGW32__)

#ifndef __WINDOWS__
#define __WINDOWS__ 1
#endif

#endif

extern int PL_unify_char(term_t chr, int c, int how);
extern int PL_get_char(term_t chr, int *c, int eof);
extern void PL_cleanup_fork(void);
extern int PL_rethrow(void);
extern void PL_get_number(term_t l, number *n);
extern int PL_unify_atomic(term_t t, PL_atomic_t a);
extern int PL_unify_termv(term_t l, va_list args);
extern int _PL_unify_atomic(term_t t, PL_atomic_t a);
extern int _PL_unify_string(term_t t, word w);

extern IOSTREAM **			/* provide access to Suser_input, */
  _PL_streams(void);			/* Suser_output and Suser_error */

extern int get_atom_text(atom_t atom, PL_chars_t *text);
COMMON(int)		get_string_text(atom_t atom, PL_chars_t *text ARG_LD);
extern char *format_float(double f, char *buf);

/**** stuff from pl-ctype.c ****/
extern IOENC initEncoding(void);

/**** stuff from pl-error.c ****/
extern int PL_get_bool_ex(term_t t, int *i);
extern int PL_get_chars_ex(term_t t, char **s, unsigned int flags);
extern int PL_get_integer_ex(term_t t, int *i);
extern int PL_get_module_ex(term_t t, module_t *m);
extern int PL_get_nchars_ex(term_t t, size_t *len, char **s, unsigned int flags);
extern int PL_get_long_ex(term_t t, long *i);
extern int PL_get_int64_ex(term_t t, int64_t *i);
extern int PL_get_intptr_ex(term_t t, intptr_t *i);
extern int PL_get_float_ex(term_t t, double *f);
extern int PL_get_char_ex(term_t t, int *p, int eof);
extern int PL_unify_list_ex(term_t l, term_t h, term_t t);
extern int PL_unify_nil_ex(term_t l);
extern int PL_get_list_ex(term_t l, term_t h, term_t t);
extern int PL_get_nil_ex(term_t l);
extern int PL_unify_bool_ex(term_t t, int val);

/**** stuff from pl-file.c ****/
extern void initIO(void);

extern void dieIO(void);
extern void protocol(const char *str, size_t n);
extern bool readLine(IOSTREAM *in, IOSTREAM *out, char *buffer);
extern bool tellString(char **s, size_t *size, IOENC enc);
extern bool tellString(char **s, size_t *size, IOENC enc);
extern bool toldString(void);


void closeFiles(int);
atom_t PrologPrompt(void);
word pl_exists_file(term_t name);
char *DirName(const char *f);
void			outOfCore(void);

word pl_noprotocol(void);

IOSTREAM *PL_current_input(void);
IOSTREAM *PL_current_output(void);

COMMON(int) stricmp(const char *s1, const char *s2);

COMMON(word) textToString(PL_chars_t *text);

COMMON(int) reportStreamError(IOSTREAM *s);

extern int digitValue(int b, int c);

PL_EXPORT(int)  	PL_unify_stream(term_t t, IOSTREAM *s);
PL_EXPORT(int)  	PL_unify_stream_or_alias(term_t t, IOSTREAM *s);
PL_EXPORT(int)  	PL_get_stream_handle(term_t t, IOSTREAM **s);
PL_EXPORT(void)  	PL_write_prompt(int);
PL_EXPORT(int) 		PL_release_stream(IOSTREAM *s);


COMMON(atom_t) 		fileNameStream(IOSTREAM *s);
COMMON(int) 		streamStatus(IOSTREAM *s);

#define getOutputStream(t, k, s)	getOutputStream__LD(t, k, s PASS_LD)
#define getTextOutputStream(t, s)       getTextOutputStream__LD(t, s PASS_LD)
#define getBinaryOutputStream(t, s)     getBinaryOutputStream__LD(t, s PASS_LD)

#define getInputStream(t, k, s)	       getInputStream__LD(t, k, s PASS_LD)
#define getTextInputStream(t, s)       getTextInputStream__LD(t, s PASS_LD)
#define getBinaryInputStream(t, s)     getBinaryInputStream__LD(t, s PASS_LD)

COMMON(int) 		getTextOutputStream__LD(term_t t, IOSTREAM **s ARG_LD);
COMMON(int) 		getBinaryOutputStream__LD(term_t t, IOSTREAM **s ARG_LD);
COMMON(int) 		getTextInputStream__LD(term_t t, IOSTREAM **s ARG_LD);
COMMON(int) 		getBinaryInputStream__LD(term_t t, IOSTREAM **s ARG_LD);

COMMON(void) 		pushOutputContext(void);
COMMON(void) 		popOutputContext(void);
COMMON(int) 		getSingleChar(IOSTREAM *s, int signals);

COMMON(void) 		prompt1(atom_t prompt);
COMMON(atom_t)		encoding_to_atom(IOENC enc);
COMMON(int) 		pl_see(term_t f);
COMMON(int) 		pl_seen(void);

COMMON(int)		unicode_separator(pl_wchar_t c);
COMMON(word) 		pl_raw_read(term_t term);
COMMON(word) 		pl_raw_read2(term_t stream, term_t term);

COMMON(access_level_t)	setAccessLevel(access_level_t new_level);

/**** stuff from pl-error.c ****/
extern void		outOfCore(void);
extern void		fatalError(const char *fm, ...);
extern int		callProlog(module_t  module, term_t goal, int flags, term_t *ex);
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

extern void setOSPrologFlags(void);
extern void setRandom(unsigned int *seedp);
extern char *canoniseFileName(char *path);
extern char *canonisePath(char *path);
extern void PL_changed_cwd(void);
extern struct tm *LocalTime(long *t, struct tm *r);
extern size_t getenv3(const char *name, char *buf, size_t len);
extern int Setenv(char *name, char *value);
extern int Unsetenv(char *name);
extern int System(char *cmd);
extern char *expandVars(const char *pattern, char *expanded, int maxlen);

PL_EXPORT(bool) ChDir(const char *X);

#if _WIN32 || defined(__MINGW32__)
PL_EXPORT(char *) BaseName(const char *X);
PL_EXPORT(char *) DirName(const char *f);
#else
#define BaseName basename
#define DirName dirname
#endif

PL_EXPORT(char *) OsPath(const char *X, char *Y);



#define    DeleteTemporaryFile(F) RemoveFile(stringAtom(F))

PL_EXPORT(intptr_t)      lengthList(term_t list, int errors);
PL_EXPORT(int)           promoteToFloatNumber(Number n);
PL_EXPORT(char *)        PrologPath(const char *ospath, char *plpath, size_t len);
PL_EXPORT(char *)                ExpandOneFile(const char *spec, char *file);
PL_EXPORT(char *)                AbsoluteFile(const char *spec, char *path);
PL_EXPORT(int)           IsAbsolutePath(const char *p);
 PL_EXPORT(char *)                OsPath(const char *plpath, char *ospath);
 PL_EXPORT(int)           IsAbsolutePath(const char *spec);
PL_EXPORT(bool)          sysError(const char *fm, ...);
PL_EXPORT(int)           setDoubleQuotes(atom_t a, unsigned int *flagp);
 PL_EXPORT(int)           getAccessLevelMask(atom_t a, access_level_t *val);

/**** SWI stuff (emulated in pl-yap.c) ****/
extern int writeAtomToStream(IOSTREAM *so, atom_t at);
extern int valueExpression(term_t t, Number r ARG_LD);
extern Atom lookupAtom(const char *s, size_t len);
extern Atom lookupUCSAtom(const pl_wchar_t *s, size_t len);
extern int toIntegerNumber(Number n, int flags);
extern int get_atom_ptr_text(Atom a, PL_chars_t *text);
extern int warning(const char *fm, ...);

/**** stuff from pl-files.c ****/
void initFiles(void);
int RemoveFile(const char *path);
int PL_get_file_name(term_t n, char **namep, int flags);
PL_EXPORT(int)		PL_get_file_nameW(term_t n, wchar_t **name, int flags);

COMMON(int) 		unifyTime(term_t t, time_t time);

COMMON(char)		digitName(int n, int sm);

/**** stuff from pl-utf8.c ****/
size_t utf8_strlen(const char *s, size_t len);

/**** stuff from pl-version.c ****/
COMMON(void) 		setGITVersion(void);


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
extern void setPrologFlag(const char *name, int flags, ...);
extern int  PL_set_prolog_flag(const char *name, int flags, ...);

extern install_t PL_install_readline(void);

COMMON(int)		saveWakeup(wakeup_state *state, int forceframe ARG_LD);
COMMON(void)		restoreWakeup(wakeup_state *state ARG_LD);

COMMON(int) 		priorityOperator(Module m, atom_t atom);
COMMON(int) 		currentOperator(Module m, atom_t name, int kind,
				int *type, int *priority);
COMMON(int) 		numberVars(term_t t, nv_options *opts, int n ARG_LD);

COMMON(Buffer)		codes_or_chars_to_buffer(term_t l, unsigned int flags,
						 int wide, CVT_result *status);

COMMON(bool)		systemMode(bool accept);


COMMON(void)		cleanupPrologFlags(void);
COMMON(void)		initPrologFlags(void);
COMMON(int)		raiseStackOverflow(int overflow);

COMMON(int)		PL_qualify(term_t raw, term_t qualified);

static inline word
setBoolean(bool *flag, term_t old, term_t new)
{ int fl = *flag; if ( !PL_unify_bool_ex(old, fl) ||
       !PL_get_bool_ex(new, &fl) )
    fail;
  *flag = fl;
  succeed;
}

#define BEGIN_NUMBERVARS(save) \
	{ fid_t _savedf; \
	  if ( save ) \
	  { _savedf = LD->var_names.numbervars_frame; \
	    LD->var_names.numbervars_frame = PL_open_foreign_frame(); \
	  }
#define END_NUMBERVARS(save) \
          if ( save ) \
	  { PL_discard_foreign_frame(LD->var_names.numbervars_frame); \
	    LD->var_names.numbervars_frame = _savedf; \
	  } \
	}


COMMON(int)             f_is_prolog_var_start(wint_t c);
COMMON(int)             f_is_prolog_atom_start(wint_t c);
COMMON(int)             f_is_prolog_identifier_continue(wint_t c);
COMMON(int)             f_is_prolog_symbol(wint_t c);

COMMON(int)		_PL_get_arg__LD(int index, term_t t, term_t a ARG_LD);

COMMON(int) 		PL_get_atom__LD(term_t t1, atom_t *a ARG_LD);
COMMON(int) 		PL_get_atom_ex__LD(term_t t, atom_t *a ARG_LD);
COMMON(int)		PL_get_text__LD(term_t l, PL_chars_t *text, int flags ARG_LD);
COMMON(int) 		PL_is_atom__LD(term_t t ARG_LD);
COMMON(int) 		PL_is_variable__LD(term_t t ARG_LD);
COMMON(term_t) 		PL_new_term_ref__LD(ARG1_LD);
COMMON(int) 		PL_put_atom__LD(term_t t, atom_t a ARG_LD);
COMMON(int) 		PL_put_term__LD(term_t t1, term_t t2 ARG_LD);
COMMON(int) 		PL_unify__LD(term_t t1, term_t t2 ARG_LD);
COMMON(int) 		PL_unify_atom__LD(term_t t, atom_t a ARG_LD);
COMMON(int)		PL_unify_int64__LD(term_t t1, int64_t ARG_LD);
COMMON(int)		PL_unify_int64_ex__LD(term_t t1, int64_t ARG_LD);
COMMON(int) 		PL_unify_integer__LD(term_t t1, intptr_t i ARG_LD);

COMMON(word)		pl_get_prolog_flag(term_t key, term_t value);
COMMON(word)		pl_prolog_flag5(term_t key, term_t value, word scope, word access, word type, control_t h);
COMMON(foreign_t)	pl_prolog_flag(term_t name, term_t value, control_t h);

COMMON(struct tm *)	PL_localtime_r(const time_t *t, struct tm *r);
COMMON(char *)		PL_asctime_r(const struct tm *tm, char *buf);


#define PL_unify(t1, t2)	PL_unify__LD(t1, t2 PASS_LD)
#define PL_unify_int64(t, i)	PL_unify_int64__LD(t, i PASS_LD)
#define PL_unify_int64_ex(t, i)	PL_unify_int64_ex__LD(t, i PASS_LD)

static inline int
PL_unify_time(term_t t, time_t time) {
  GET_LD
  return PL_unify_int64(t, (int64_t)time);
}

/* inlines that need ARG_LD */
static inline intptr_t
skip_list(Word l, Word *tailp ARG_LD) {
  return (intptr_t)YAP_SkipList(l, tailp);
}

static inline word
valHandle__LD(term_t r ARG_LD)
{
  return (word)YAP_GetFromSlot((Int)r);
}

static inline void *allocHeap__LD(size_t n ARG_LD)
{
  return YAP_AllocSpaceFromYap(n);
}

static inline void *allocHeapOrHalt(size_t n)
{
  void *ptr = YAP_AllocSpaceFromYap(n);
  if (!ptr) Yap_exit(1);
  return ptr;
}

static inline void freeHeap(void *mem, size_t n)
{
  YAP_FreeSpaceFromYap(mem);
}

extern void unallocStream(IOSTREAM *s);

extern atom_t accessLevel(void);
int currentBreakLevel(void);

#ifdef __WINDOWS__
int hasConsole(void);
int PL_wait_for_console_input(void *handle);
void PlMessage(const char *fm, ...);
const char *WinError(void);
word pl_win_exec(term_t cmd, term_t how);
foreign_t pl_win_module_file(term_t module, term_t file);
int PL_w32_wrap_ansi_console(void);

#ifdef EMULATE_DLOPEN
	/* file is in UTF-8, POSIX path */
void *dlopen(const char *file, int flags);
const char *dlerror(void);
void *dlsym(void *handle, char *symbol);
int dlclose(void *handle);
#endif

int ms_snprintf(char *buffer, size_t count, const char *fmt, ...);
void getDefaultsFromRegistry(void);

DWORD RunSilent(const char* strCommand);
FILE *pt_popen(const char *cmd, const char *mode);
int pt_pclose(FILE *fd);

int PL_w32thread_raise(DWORD id, int sig);
#endif

extern const PL_extension PL_predicates_from_ctype[];
extern const PL_extension PL_predicates_from_file[];
extern const PL_extension PL_predicates_from_files[];
extern const PL_extension PL_predicates_from_glob[];
extern const PL_extension PL_predicates_from_read[];
extern const PL_extension PL_predicates_from_tai[];
extern const PL_extension PL_predicates_from_write[];
extern const PL_extension PL_predicates_from_prologflag[];
extern const PL_extension PL_predicates_from_win[];
extern const PL_extension PL_predicates_from_locale[];
extern const PL_extension PL_predicates_from_system[];

#define enableThreads(val) FALSE

#endif
