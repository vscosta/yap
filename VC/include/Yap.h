






/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		Yap.h.m4						 *
* mods:									 *
* comments:	main header file for YAP				 *
* version:      $Id: Yap.h,v 1.9 2002-06-01 04:29:01 vsc Exp $	 *
*************************************************************************/

#include "config.h"

/*

#define RATIONAL_TREES 1

#define DEPTH_LIMIT 1

#define COROUTINING 1

#define YAPOR      1

#define ANALYST 1

*/

#define MULTI_ASSIGNMENT_VARIABLES 1

#if defined(TABLING)
#error Do not explicitly define TABLING
#endif /* YAPOR */

#if defined(TABLING_BATCHED_SCHEDULING) && defined(TABLING_LOCAL_SCHEDULING)
#error Do not define multiple tabling scheduling strategies
#endif /* TABLING_BATCHED_SCHEDULING || TABLING_LOCAL_SCHEDULING */

#if defined(TABLING_BATCHED_SCHEDULING) || defined(TABLING_LOCAL_SCHEDULING)
#define TABLING 1
#endif /* TABLING_BATCHED_SCHEDULING || TABLING_LOCAL_SCHEDULING */

#if defined(YAPOR)
#error Do not explicitly define YAPOR
#endif /* YAPOR */

#if (defined(ENV_COPY) && (defined(ACOW) || defined(SBA))) || (defined(ACOW) && defined(SBA))
#error Do not define multiple or-parallel models
#endif /* (ENV_COPY && (ACOW || SBA)) || (ACOW && SBA) */

#if defined(ENV_COPY) || defined(ACOW) || defined(SBA)
#define YAPOR 1
#endif /* ENV_COPY || ACOW || SBA */

#if defined(TABLING) && (defined(ACOW) || defined(SBA))
#error Currently TABLING only works with ENV_COPY
#endif /* TABLING && (ACOW || SBA) */

#ifdef YAPOR
#define FIXED_STACKS 1
#endif /* YAPOR */

#if defined(YAPOR) || defined(TABLING)
#undef TRAILING_REQUIRES_BRANCH 
#endif /* YAPOR || TABLING */

#if ANALYST
#ifdef USE_THREADED_CODE
#undef USE_THREADED_CODE
#endif
#endif

#ifdef  COROUTINING
#ifndef TERM_EXTENSIONS
#define TERM_EXTENSIONS 1
#endif
#endif

#ifdef SBA
#ifdef YAPOR
#ifndef FROZEN_STACKS
#define FROZEN_STACKS 1
#endif
#endif
#endif

#ifdef TABLING
#ifndef FROZEN_STACKS
#define FROZEN_STACKS 1
#endif
#endif

#ifdef _MSC_VER /* Microsoft's Visual C++ Compiler */
/* adjust a config.h from mingw32 to work with vc++ */
#ifdef HAVE_GCC
#undef  HAVE_GCC
#endif
#ifdef  USE_THREADED_CODE
#undef  USE_THREADED_CODE
#endif
#define inline __inline
#define YAP_VERSION "Yap-4.3.21"
#define BIN_DIR "c:\\Program Files\\Yap\\bin"
#define LIB_DIR "c:\\Program Files\\Yap\\lib\\Yap"
#define SHARE_DIR "c:\\Program Files\\Yap\\share\\Yap"
#ifdef  HOST_ALIAS
#undef  HOST_ALIAS
#endif
#define HOST_ALIAS "i386-pc-win32"
#ifdef  HAVE_IEEEFP_H
#undef  HAVE_IEEEFP_H
#endif
#ifdef  HAVE_UNISTD_H
#undef  HAVE_UNISTD_H
#endif
#ifdef  HAVE_SYS_TIME_H
#undef  HAVE_SYS_TIME_H
#endif
#endif

#ifdef __MINGW32__
#ifndef _WIN32
#define _WIN32 1
#endif
#endif

#if HAVE_GCC
#define MIN_ARRAY 0
#define DUMMY_FILLER_FOR_ABS_TYPE 
#else
#define MIN_ARRAY 1
#define DUMMY_FILLER_FOR_ABS_TYPE int dummy;
#endif

#ifndef ADTDEFS_C
#define EXTERN  static
#else
#define EXTERN
#endif

/*  truth-values							*/
#define	 TRUE	1
#define	 FALSE	0

/*  null pointer							*/
#define	 NIL	0

/* Basic types */

/* defines integer  types Int and UInt (unsigned) with the same size as a ptr
** and integer types Short and UShort with half the size of a ptr 
*/

#if SIZEOF_INT_P==4

#if SIZEOF_INT==4
/*   */ typedef int Int;
/*   */ typedef unsigned int UInt;

#elif SIZEOF_LONG_INT==4
/*   */ typedef long int Int;
/*   */ typedef unsigned long int UInt;

#else
#	error Yap require integer types of the same size as a pointer
#endif

#if SIZEOF_SHORT_INT==2
/*   */ typedef short int Short;
/*   */ typedef unsigned short int UShort;

#else
#	error Yap requires integer types half the size of a pointer
#endif

#elif SIZEOF_INT_P==8

#   if SIZEOF_INT==8
/*   */ typedef int Int;
/*   */ typedef unsigned int UInt;

#elif SIZEOF_LONG_INT==8
/*   */ typedef long int Int;
/*   */ typedef unsigned long int UInt;

#   elif SIZEOF_LONG_LONG_INT==8
/*   */ typedef long long int Int;
/*   */ typedef unsigned long long int UInt;

#   else
#	error Yap requires integer types of the same size as a pointer
#   endif

#   if SIZEOF_SHORT_INT==4
/*   */ typedef short int Short;
/*   */ typedef unsigned short int UShort;

#   elif SIZEOF_INT==4
/*   */ typedef int Short;
/*   */ typedef short int UShort;

#   else
#	error Yap requires integer types half the size of a pointer
#   endif

#else

#  error Yap requires pointers of size 4 or 8

#endif

/*   */ typedef double  Float;

#if SIZEOF_INT<SIZEOF_INT_P
#define SHORT_INTS 1
#else
#define SHORT_INTS 0
#endif

#if DEBUG
extern char     Option[20];
#endif

/* #define FORCE_SECOND_QUADRANT 1 */

#if defined(FORCE_SECOND_QUADRANT)
#define IN_SECOND_QUADRANT 1
#define MMAP_ADDR 0x42000000
#endif

#if !IN_SECOND_QUADRANT
#if __linux__ || __FreeBSD__ || __NetBSD__ || mips || __APPLE__
#if defined(YAPOR) && defined(__alpha)
#define MMAP_ADDR 0x40000000
#elif mips
#define MMAP_ADDR 0x02000000
#elif __APPLE__
#define MMAP_ADDR 0x01000000
#else
#define MMAP_ADDR 0x09000000
#endif
#elif __svr4__ || defined(__SVR4)
#define MMAP_ADDR 0x02000000
#elif defined(_WIN32) || defined(__CYGWIN__)
#define MMAP_ADDR 0x30000000L
#endif
#endif /* !IN_SECOND_QUADRANT */

#if defined(MMAP_ADDR) && (USE_MMAP || USE_SHMAT || _WIN32) && !__simplescalar__
#define HEAP_INIT_BASE  (MMAP_ADDR)
#define AtomBase        ((char *)MMAP_ADDR)
#else
#define HEAP_INIT_BASE  ((CELL)HeapBase)
#define AtomBase        (HeapBase)
#endif



#ifndef SHORT_ADDRESSES
#	define LONG_ADDRESSES	1
#else
#	define LONG_ADDRESSES	0
#endif

#ifndef ALIGN_LONGS
#define ALIGN_LONGS 1
#endif

/*  basic data types  */

typedef UInt CELL;
typedef UShort BITS16;
typedef Short SBITS16;
typedef UInt BITS32;

#if ALIGN_LONGS
typedef CELL SFLAGS;
#else
typedef BITS16 SFLAGS;
#endif

typedef char   *ADDR;
typedef CELL OFFSET;
typedef unsigned char *CODEADDR;

#define WordSize     sizeof(BITS16)
#define CellSize     sizeof(CELL)
#define SmallSize    sizeof(SMALLUNSGN)

/* type	casting	macros		*/

#define	Addr(V)		((ADDR) (V))
#define	Unsigned(V)	((CELL) (V))
#define	Signed(V)	((Int) (V))

#define CodePtr(V)	((CODEADDR)(V))
#define	CellPtr(V) 	((CELL *)(V))
#define OpCodePtr(V)	((OPCODE *)(V))
#define OpRegPtr(V)	((OPREG *)(V))
#define SmallPtr(V)	((SMALLUNSGN *)(V))
#define	WordPtr(V)	((BITS16 *)(V))
#define DisplPtr(V)	((DISPREG *)(V))
#define TermPtr(V)	((Term *) (V))

/*	Abstract Type Definitions for YAPProlog			       */

typedef CELL Term;

#if !defined(YAPOR) && !defined(THREADS)
#include <nolocks.h>
#else
#ifdef i386
#include <locks_x86.h>
#endif
#if defined(sparc) || defined(__sparc)
#include <locks_sparc.h>
#endif
#ifdef mips
#include <locks_mips.h>
#endif
#ifdef __alpha
#include <locks_alpha.h>
#endif
#endif

/********************** use an auxiliary function for ranges ************/

#ifdef __GNUC__
#define IN_BETWEEN(MIN,X,MAX) (Unsigned((Int)(X)-(Int)(MIN)) <=  \
 			    Unsigned((Int)(MAX)-(Int)(MIN)) )

#define OUTSIDE(MIN,X,MAX) (Unsigned((Int)(X)-(Int)(MIN)) >  \
 			    Unsigned((Int)(MAX)-(Int)(MIN)) )
#else
#define IN_BETWEEN(MIN,X,MAX) ((void *)(X) >= (void *)(MIN) && (void *)(X) <= (void *)(MAX))

#define OUTSIDE(MIN,X,MAX) ((void *)(X) < (void *)(MIN) || (void *)(X) > (void *)(MAX))
#endif

/* ************************* Atoms  *************************************/

#include "Atoms.h"

/* ************************* Coroutining  **********************************/

#ifdef COROUTINING
/* Support for co-routining */
#include "corout.h"
#endif

/*********  abstract machine registers **********************************/


#include "amidefs.h"

#include "Regs.h"

#if defined(YAPOR) ||defined(THREADS)
#ifdef mips
#include <locks_mips_funcs.h>
#endif
#ifdef __alpha
#include <locks_alpha_funcs.h>
#endif
#endif

/************ variables	concerned with Error Handling *************/

#include <setjmp.h>

#if defined(SIMICS) || !HAVE_SIGSETJMP
#define sigjmp_buf jmp_buf
#define sigsetjmp(Env, Arg) setjmp(Env)
#define siglongjmp(Env, Arg) longjmp(Env, Arg)
#endif

extern sigjmp_buf    RestartEnv;   /* used to restart after an abort */

/* Support for arrays */
#include "arrays.h"

/************ variables	concerned with Error Handling	*************/

/* Types of Errors */
typedef enum {
  YAP_NO_ERROR,
  FATAL_ERROR,
  INTERNAL_ERROR,
  PURE_ABORT,
  /* ISO_ERRORS */
  DOMAIN_ERROR_ARRAY_OVERFLOW,
  DOMAIN_ERROR_ARRAY_TYPE,
  DOMAIN_ERROR_IO_MODE,
  DOMAIN_ERROR_MUTABLE,
  DOMAIN_ERROR_NON_EMPTY_LIST,
  DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
  DOMAIN_ERROR_NOT_NL,
  DOMAIN_ERROR_NOT_ZERO,
  DOMAIN_ERROR_OUT_OF_RANGE,
  DOMAIN_ERROR_OPERATOR_PRIORITY,
  DOMAIN_ERROR_OPERATOR_SPECIFIER,
  DOMAIN_ERROR_RADIX,
  DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW,
  DOMAIN_ERROR_SOURCE_SINK,
  DOMAIN_ERROR_STREAM,
  DOMAIN_ERROR_STREAM_OR_ALIAS,
  DOMAIN_ERROR_STREAM_POSITION,
  DOMAIN_ERROR_TIMEOUT_SPEC,
  DOMAIN_ERROR_SYNTAX_ERROR_HANDLER,
  EVALUATION_ERROR_FLOAT_OVERFLOW,
  EVALUATION_ERROR_FLOAT_UNDERFLOW,
  EVALUATION_ERROR_INT_OVERFLOW,
  EVALUATION_ERROR_UNDEFINED,
  EVALUATION_ERROR_UNDERFLOW,
  EVALUATION_ERROR_ZERO_DIVISOR,
  EXISTENCE_ERROR_ARRAY,
  EXISTENCE_ERROR_SOURCE_SINK,
  EXISTENCE_ERROR_STREAM,
  INSTANTIATION_ERROR,
  PERMISSION_ERROR_ACCESS_PRIVATE_PROCEDURE,
  PERMISSION_ERROR_NEW_ALIAS_FOR_STREAM,
  PERMISSION_ERROR_CREATE_ARRAY,
  PERMISSION_ERROR_CREATE_OPERATOR,
  PERMISSION_ERROR_INPUT_BINARY_STREAM,
  PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM,
  PERMISSION_ERROR_INPUT_STREAM,
  PERMISSION_ERROR_INPUT_TEXT_STREAM,
  PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE,
  PERMISSION_ERROR_OPEN_SOURCE_SINK,
  PERMISSION_ERROR_OUTPUT_BINARY_STREAM,
  PERMISSION_ERROR_OUTPUT_STREAM,
  PERMISSION_ERROR_OUTPUT_TEXT_STREAM,
  PERMISSION_ERROR_RESIZE_ARRAY,
  PERMISSION_ERROR_REPOSITION_STREAM,
  REPRESENTATION_ERROR_CHARACTER,
  REPRESENTATION_ERROR_CHARACTER_CODE,
  REPRESENTATION_ERROR_MAX_ARITY,
  SYNTAX_ERROR,
  SYSTEM_ERROR,
  TYPE_ERROR_ARRAY,
  TYPE_ERROR_ATOM,
  TYPE_ERROR_ATOMIC,
  TYPE_ERROR_BYTE,
  TYPE_ERROR_CALLABLE,
  TYPE_ERROR_CHARACTER,
  TYPE_ERROR_COMPOUND,
  TYPE_ERROR_DBREF,
  TYPE_ERROR_DBTERM,
  TYPE_ERROR_EVALUABLE,
  TYPE_ERROR_FLOAT,
  TYPE_ERROR_INTEGER,
  TYPE_ERROR_KEY,
  TYPE_ERROR_LIST,
  TYPE_ERROR_NUMBER,
  TYPE_ERROR_PREDICATE_INDICATOR,
  TYPE_ERROR_PTR,
  TYPE_ERROR_UBYTE,
  TYPE_ERROR_VARIABLE,
  UNKNOWN_ERROR
} yap_error_number;

extern char    *ErrorMessage;	/* used to pass error messages		*/
extern Term     Error_Term;	/* used to pass error terms */
extern yap_error_number  Error_TYPE;	/* used to pass the error */

typedef enum {
  YAP_INT_BOUNDED_FLAG = 0,
  MAX_ARITY_FLAG = 1,
  INTEGER_ROUNDING_FLAG = 2,
  YAP_MAX_INTEGER_FLAG = 3,
  YAP_MIN_INTEGER_FLAG = 4,
  CHAR_CONVERSION_FLAG = 5,
  YAP_DOUBLE_QUOTES_FLAG = 6,
  YAP_TO_CHARS_FLAG = 7,
  LANGUAGE_MODE_FLAG = 8,
  STRICT_ISO_FLAG = 9,
  SPY_CREEP_FLAG = 10,
  SOURCE_MODE_FLAG = 11,
  CHARACTER_ESCAPE_FLAG = 12,
  WRITE_QUOTED_STRING_FLAG = 13,
  ALLOW_ASSERTING_STATIC_FLAG = 14,
  HALT_AFTER_CONSULT_FLAG = 15,
  FAST_BOOT_FLAG = 16
} yap_flags;

#define STRING_AS_CHARS		0
#define STRING_AS_ATOM		2

#define QUINTUS_TO_CHARS	0
#define ISO_TO_CHARS		1

#define CPROLOG_CHARACTER_ESCAPES		0
#define ISO_CHARACTER_ESCAPES			1
#define SICSTUS_CHARACTER_ESCAPES		2

#define NUMBER_OF_YAP_FLAGS     FAST_BOOT_FLAG+1

/************************  prototypes **********************************/

#include "Yapproto.h"

/************************  OPTYap configuration ************************/

/* These must be included before unification handlers */
#if defined(YAPOR) || defined(TABLING)
#include "opt.config.h"
#endif

/***********************************************************************/

     /*
absrectype Term	= Int + Float +	Atom + Pair + Appl + Ref + Var

with AbsAppl(t)	: *CELL	-> Term
and  RepAppl(t)	: Term -> *CELL

and  AbsPair(t)	: *CELL	-> Term
and  RepPair(t)	: Term -> *CELL

and  IsIntTerm(t) = ...
and  IsAtomTerm(t) = ...
and  IsVarTerm(t) = ...
and  IsPairTerm(t) = ...
and  IsApplTerm(t) = ...
and  IsFloatTerm(t) = ...
and  IsRefTerm(t) = ...
and  IsNonVarTerm(t) = ! IsVar(t)
and  IsNumterm(t) = IsIntTerm(t) || IsFloatTerm(t)
and  IsAtomicTerm(t) = IsNumTerm(t) || IsAtomTerm(t)
and  IsPrimitiveTerm(t) = IsAtomicTerm(t) || IsRefTerm(t)

and  MkIntTerm(n) = ...
and  MkFloatTerm(f) = ...
and  MkAtomTerm(a) = ...
and  MkVarTerm(r) = ...
and  MkApplTerm(f,n,args) = ...
and  MkPairTerm(hd,tl) = ...
and  MkRefTerm(R) = ...

and  PtrOfTerm(t) : Term -> CELL * = ...
and  IntOfTerm(t) : Term -> int	= ...
and  FloatOfTerm(t) : Term -> flt = ...
and  AtomOfTerm(t) : Term -> Atom = ...
and  VarOfTerm(t) : Term -> *Term = ....
and  HeadOfTerm(t) : Term -> Term = ...
and  TailOfTerm(t) : Term -> Term = ...
and  FunctorOfTerm(t) :	Term ->	Functor	= ...
and  ArgOfTerm(i,t)  : Term -> Term= ...
and  RefOfTerm(t) : Term -> DBRef = ...

*/

/* 
   YAP can use several different tag schemes, according to the kind of
   machine we are experimenting with.
*/

#if LONG_ADDRESSES && defined(OLD_TAG_SCHEME)

#include "Tags_32bits.h"

#endif /* LONG_ADDRESSES && defined(OLD_TAG_SCHEME) */

/* AIX will by default place mmaped segments at 0x30000000. This is
	incompatible with the high tag scheme. Linux-ELF also does not like
	if you place things in the lower addresses (power to the libc people).
*/
#if (defined(_AIX) || defined(_WIN32) || defined(sparc) || defined(__sparc) || defined(mips) || defined(__FreeBSD__) || defined(_POWER) || defined(__linux__) || defined(IN_SECOND_QUADRANT)) && !defined(TABLING)
#define USE_LOW32_TAGS 1
#endif

#if LONG_ADDRESSES && SIZEOF_INT_P==4 && !defined(OLD_TAG_SCHEME) && !defined(USE_LOW32_TAGS)

#include "Tags_32Ops.h"

#endif /* LONG_ADDRESSES && !defined(OLD_TAG_SCHEME) && !defined(USE_LOW32_TAGS) */

#if LONG_ADDRESSES && SIZEOF_INT_P==4 && !defined(OLD_TAG_SCHEME) && defined(USE_LOW32_TAGS)

#include "Tags_32LowTag.h"

#endif /* LONG_ADDRESSES && !defined(OLD_TAG_SCHEME) */

#if LONG_ADDRESSES && SIZEOF_INT_P==8 && !defined(OLD_TAG_SCHEME)

#include "Tags_64bits.h"

#endif /* LONG_ADDRESSES && SIZEOF_INT_P==8 && !defined(OLD_TAG_SCHEME) */

#if !LONG_ADDRESSES

#include "Tags_24bits.h"

#endif /* !LONG_ADDRESSES */

#ifdef TAG_LOW_BITS_32
#define MBIT     0x80000000
#define RBIT     0x40000000

#if IN_SECOND_QUADRANT
#define INVERT_RBIT 1 /* RBIT is 1 by default */
#endif

#else

#if defined(SBA) && defined(__linux__)
#define MBIT     /* 0x20000000 */ MKTAG(0x1,0)	/* mark bit */
#else
#define RBIT     /* 0x20000000 */ MKTAG(0x1,0)  /* relocation chain bit */
#define MBIT     /* 0x40000000 */ MKTAG(0x2,0)	/* mark bit */
#endif
#endif

#define	TermSize    sizeof(Term)

/************* variables related to memory allocation *******************/
/* must be before TermExt.h */
extern ADDR     HeapBase,
		     LocalBase,
		     GlobalBase,
		     TrailBase, TrailTop,
		     ForeignCodeBase, ForeignCodeTop, ForeignCodeMax;


/* applies to unbound variables */

inline EXTERN Term * VarOfTerm(Term t);

inline EXTERN Term * VarOfTerm(Term t)
{
	return (Term *) (t);
}


#if SBA

inline EXTERN Term MkVarTerm(void);

inline EXTERN Term MkVarTerm()
{
	return (Term) ((*H = 0, H++));
}



inline EXTERN int IsUnboundVar(Term);

inline EXTERN int IsUnboundVar(Term t)
{
	return (int) (t == 0);
}


#else

inline EXTERN Term MkVarTerm(void);

inline EXTERN Term MkVarTerm()
{
	return (Term) ((*H = (CELL) H, H++));
}



inline EXTERN int IsUnboundVar(Term);

inline EXTERN int IsUnboundVar(Term t)
{
	return (int) (*VarOfTerm(t) == (t));
}


#endif

inline EXTERN CELL * PtrOfTerm(Term);

inline EXTERN CELL * PtrOfTerm(Term t)
{
	return (CELL *) (*(CELL *)(t));
}




inline EXTERN Functor FunctorOfTerm(Term);

inline EXTERN Functor FunctorOfTerm(Term t)
{
	return (Functor) (*RepAppl(t));
}



inline EXTERN Term MkAtomTerm(Atom);

inline EXTERN Term MkAtomTerm(Atom a)
{
	return (Term) (TAGGEDA(AtomTag, (CELL)(a)-HEAP_INIT_BASE));
}



inline EXTERN Atom AtomOfTerm(Term t);

inline EXTERN Atom AtomOfTerm(Term t)
{
	return (Atom) (HEAP_INIT_BASE+NonTagPart(t));
}



inline EXTERN int IsAtomTerm(Term);

inline EXTERN int IsAtomTerm(Term t)
{
	return (int) (CHKTAG((t), AtomTag));
}




inline EXTERN Term MkIntTerm(Int);

inline EXTERN Term MkIntTerm(Int n)
{
	return (Term) (TAGGED(NumberTag, (n)));
}


/*
  A constant to subtract or add to a well-known term, we assume no
  overflow problems are possible
*/

inline EXTERN Term MkIntConstant(Int);

inline EXTERN Term MkIntConstant(Int n)
{
	return (Term) (NONTAGGED(NumberTag, (n)));
}



inline EXTERN int IsIntTerm(Term);

inline EXTERN int IsIntTerm(Term t)
{
	return (int) (CHKTAG((t), NumberTag));
}



/* Needed to handle numbers:
   	these two macros are fundamental in the integer/float conversions */

#ifdef M_WILLIAMS
#define IntInBnd(X)	(TRUE)
#else
#ifdef TAGS_FAST_OPS
#define IntInBnd(X)	(Unsigned( ( (Int)(X) >> (32-7) ) + 1) <= 1)
#else
#define IntInBnd(X)	( (X) < MAX_ABS_INT && \
                          (X) > -MAX_ABS_INT-1L )
#endif
#endif
#ifdef C_PROLOG
#define FlIsInt(X)	( (X) == (Int)(X) && IntInBnd((X)) )
#else
#define FlIsInt(X)	( FALSE )
#endif


/*
  There are two types of functors:

  o Special functors mark special terms
  on the heap that should be seen as constants.

  o Standard functors mark normal applications.

*/

#include "TermExt.h"

#define IsAccessFunc(func)		((func) == FunctorAccess)


inline EXTERN Term MkIntegerTerm(Int);

inline EXTERN Term MkIntegerTerm(Int n)
{
	return (Term) (IntInBnd(n) ? MkIntTerm(n) : MkLongIntTerm(n));
}



inline EXTERN int IsIntegerTerm(Term);

inline EXTERN int IsIntegerTerm(Term t)
{
	return (int) (IsIntTerm(t) || IsLongIntTerm(t));
}



inline EXTERN Int IntegerOfTerm(Term);

inline EXTERN Int IntegerOfTerm(Term t)
{
	return (Int) (IsIntTerm(t) ? IntOfTerm(t) : LongIntOfTerm(t));
}




/*************** unification routines ***********************************/

#if SBA
#include "or.sbaamiops.h"
#else
#include "amiops.h"
#endif

/*************** High level macros to access arguments ******************/


inline EXTERN Term ArgOfTerm(int i, Term t);

inline EXTERN Term ArgOfTerm(int i, Term t)
{
	return (Term) (Derefa(RepAppl(t) + (i)));
}



inline EXTERN Term HeadOfTerm(Term);

inline EXTERN Term HeadOfTerm(Term t)
{
	return (Term) (Derefa(RepPair(t)));
}



inline EXTERN Term TailOfTerm(Term);

inline EXTERN Term TailOfTerm(Term t)
{
	return (Term) (Derefa(RepPair(t) + 1));
}




inline EXTERN Term ArgOfTermCell(int i, Term t);

inline EXTERN Term ArgOfTermCell(int i, Term t)
{
	return (Term) ((CELL)(RepAppl(t) + (i)));
}



inline EXTERN Term HeadOfTermCell(Term);

inline EXTERN Term HeadOfTermCell(Term t)
{
	return (Term) ((CELL)(RepPair(t)));
}



inline EXTERN Term TailOfTermCell(Term);

inline EXTERN Term TailOfTermCell(Term t)
{
	return (Term) ((CELL)(RepPair(t) + 1));
}



/*************** variables concerned with atoms	table *******************/
#define	MaxHash	    1001

/************ variables	concerned with save and restore	*************/
extern int      splfild;

#define FAIL_RESTORE  0
#define DO_EVERYTHING 1
#define DO_ONLY_CODE  2


#ifdef EMACS

/******************** using Emacs mode ********************************/

extern int      emacs_mode;

#endif


/************ variable concerned with version number *****************/
extern char    version_number[];

/********* common instructions codes*************************/

#define MAX_PROMPT  256

#if USE_THREADED_CODE

/************ reverse lookup of instructions *****************/
typedef struct opcode_tab_entry {
  OPCODE opc;
  op_numbers opnum;
} opentry;

#endif

/******************* controlling the compiler ****************************/
extern int      optimizer_on;

/******************* the line for the current parse **********************/
extern int      StartLine;
extern int      StartCh;
extern int      CurFileNo;

/********************* how to write a Prolog term ***********************/

/********* Prolog may be in several modes *******************************/

typedef enum {
  BootMode  =   1,		/* if booting or restoring */
  UserMode  =   2,		/* Normal mode */
  CritMode  =   4,		/* If we are meddling with the heap */
  AbortMode =   8,		/* expecting to abort */
  InterruptMode = 16,		/* under an interrupt */
  InErrorMode = 32,		/* under an interrupt */
  ConsoleGetcMode = 64		/* blocked reading from console */
} prolog_exec_mode;

extern prolog_exec_mode      PrologMode;
extern int      CritLocks;

/************** Access to yap initial arguments ***************************/

extern char   **yap_args;
extern int      yap_argc;

/******************* controlling debugging ****************************/
extern int      creep_on;

#ifdef YAPOR
#define YAPEnterCriticalSection()                           \
	{                                                   \
          if (worker_id != GLOBAL_LOCKS_who_locked_heap) {  \
	    LOCK(GLOBAL_LOCKS_heap_access);                 \
	    GLOBAL_LOCKS_who_locked_heap = worker_id;       \
	  }                                                 \
          PrologMode |= CritMode;                           \
          CritLocks++;                                      \
        }
#define YAPLeaveCriticalSection()                                        \
	{                                                                \
          CritLocks--;                                                   \
          if (!CritLocks) {                                              \
            PrologMode &= ~CritMode;                                     \
            if (PrologMode & InterruptMode) {                            \
	      PrologMode &= ~InterruptMode;                              \
	      ProcessSIGINT();                                           \
            }                                                            \
            if (PrologMode & AbortMode) {                                \
	      PrologMode &= ~AbortMode;                                  \
	      Error(PURE_ABORT, 0, "");                            \
            }                                                            \
	    GLOBAL_LOCKS_who_locked_heap = MAX_WORKERS;                  \
            UNLOCK(GLOBAL_LOCKS_heap_access);                            \
          }                                                              \
        }
#else
#define YAPEnterCriticalSection()                           \
	{                                                   \
          PrologMode |= CritMode;                           \
          CritLocks++;                                      \
        }
#define YAPLeaveCriticalSection()                                        \
	{                                                                \
          CritLocks--;                                                   \
          if (!CritLocks) {                                              \
            PrologMode &= ~CritMode;                                     \
            if (PrologMode & InterruptMode) {                            \
	      PrologMode &= ~InterruptMode;                              \
	      ProcessSIGINT();                                           \
            }                                                            \
            if (PrologMode & AbortMode) {                                \
	      PrologMode &= ~AbortMode;                                  \
	      Error(PURE_ABORT, 0, "");                            \
            }                                                            \
          }                                                              \
        }
#endif /* YAPOR */

/* when we are calling the InitStaff procedures */
#define AT_BOOT      0
#define AT_RESTORE   1

/********* whether we should try to compile array references ******************/

extern int  compile_arrays;

/********* mutable variables ******************/

/* I assume that the size of this structure is a multiple of the size
   of CELL!!! */
typedef struct TIMED_MAVAR{
  CELL value;
  CELL clock;
} timed_var;

/********* while debugging you may need some info ***********************/

#if DEBUG
extern int      output_msg;
#endif

#if EMACS
extern char     emacs_tmp[], emacs_tmp2[];
#endif

#if HAVE_SIGNAL
extern int      snoozing;
#endif

#if defined(YAPOR) || defined(TABLING)
#include "opt.structs.h"
#include "opt.macros.h"
#include "opt.proto.h"
#endif /* YAPOR || TABLING */

#if SBA
#include "or.sbaunify.h"
#endif

