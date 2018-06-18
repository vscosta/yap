#ifndef BPX_H
#define BPX_H

#include "bprolog.h"
#include "stuff.h"

#ifdef __YAP_PROLOG__

#include <stdio.h>
#include <stdlib.h>
#include <YapTerm.h>
#include <YapRegs.h>
#include <YapTags.h>

typedef void *SYM_REC_PTR;

#define heap_top HR
#define local_top ASP
#define trail_top TR
#define trail_up_addr ((tr_fr_ptr)LCL0)

#define UNDO_TRAILING while (TR > (tr_fr_ptr)trail_top0) { RESET_VARIABLE(VarOfTerm(TrailTerm(TR))); TR--; }

#define NEW_HEAP_NODE(x)          (*heap_top++ = (x))

#define STACK_OVERFLOW 1

/*====================================================================*/

#define       ARG(X,Y)  XREGS[X]
#define       XDEREF(T) while (IsVarTerm(T)) { CELL *next = VarOfTerm(T); if (IsUnboundVar(next)) break; (T) = *next; }
#define       MAKEINT(I)  bp_build_integer(I)
#define       INTVAL(T)  bp_get_integer(T)

#define       MAX_ARITY 256

#define       BP_MALLOC(X,Y,Z)  ( X = malloc((Y)*sizeof(BPLONG)) )

#define       NULL_TERM   ((TERM)(0))

#define REF0 0x0L
#define REF1 0x1L
#define SUSP 0x2L
#define LST  0x4L
#define ATM  0x8L
#define INT  0x10L
#define STR  0x20L
#define NVAR (LST|ATM|INT|STR)

#define GET_STR_SYM_REC(p) ((SYM_REC_PTR)*RepAppl(p))
#define GET_ATM_SYM_REC(p) ((SYM_REC_PTR)AtomOfTerm(p))

#define GET_ARITY_STR(s)    YAP_ArityOfFunctor((YAP_Functor)(s))
#define GET_ARITY_ATOM(s)    0

#define GET_NAME_STR(f)     YAP_AtomName(YAP_NameOfFunctor((YAP_Functor)(f)))
#define GET_NAME_ATOM(a)    YAP_AtomName((YAP_Atom)(a))

static inline
long int XTAG(TERM t)
{
  switch(YAP_TagOfTerm(t)) {
  case YAP_TAG_UNBOUND:
    return REF0;
  case YAP_TAG_ATT:
    return SUSP;
  case YAP_TAG_REF:
    return REF1;
  case YAP_TAG_PAIR:
    return LST;
  case YAP_TAG_ATOM:
    return ATM;
  case YAP_TAG_INT:
    return INT;
  case YAP_TAG_LONG_INT:
    return INT;
  case YAP_TAG_APPL:
  default:
    return STR;
  }
}

#include "inline-only.h"

INLINE_ONLY TERM ADDTAG(void * t,int tag);
INLINE_ONLY int is_UNIFIABLE(TERM t1, TERM t2);
INLINE_ONLY int is_IDENTICAL(TERM t1, TERM t2);
INLINE_ONLY char *bp_term_2_string(TERM t);
INLINE_ONLY int bp_string_2_term(const char *s, TERM to, TERM tv);
INLINE_ONLY SYM_REC_PTR insert(const char *name, int size, int arity);
INLINE_ONLY int compare(TERM t1, TERM t2);
INLINE_ONLY void write_term(TERM t);
INLINE_ONLY void numberVarTermOpt(TERM t);
INLINE_ONLY TERM unnumberVarTerm(TERM t, BPLONG_PTR pt1, BPLONG_PTR pt2);
INLINE_ONLY int unifyNumberedTerms(TERM t1, TERM t2);

int bpx_call_term(TERM t);
int bpx_call_string(const char *s);
int bpx_call_string(const char *s);
int bpx_mount_query_string(const char *s);
int bpx_next_solution(void);
void bpx_write(TERM t);
int bpx_printf(const char *fmt, ...);


INLINE_ONLY TERM ADDTAG(void * t,int tag) {
  if (tag == ATM) 
    return MkAtomTerm((Atom)t);
  if (tag == LST) 
    return AbsPair((CELL *)t);
  return AbsAppl((CELL *)t);
}

#define ISREF(t) IsVarTerm(t)
#define ISATOM(t) IsAtomTerm(t)
#define ISINT(t) IsIntegerTerm(t)
#define ISNUM(t) YAP_IsNumberTerm(t)
#define ISNIL(t) YAP_IsTermNil(t)
#define ISLIST(t) IsPairTerm(t)
#define ISSTRUCT(t) IsApplTerm(t)
#define ISFLOAT(t) IsFloatTerm(t)
#define ISCOMPOUND(t)  YAP_IsCompoundTerm(t)

static inline
double floatval(TERM t)
{
  return (Float)FloatOfTerm(t);
}

static inline
TERM encodefloat1(double f USES_REGS)
{
  return MkFloatTerm((Float)f);
}

INLINE_ONLY int is_UNIFIABLE(TERM t1, TERM t2)
{
  return YAP_Unifiable(t1, t2);
}

INLINE_ONLY int is_IDENTICAL(TERM t1, TERM t2)
{
  return YAP_ExactlyEqual(t1, t2);
}


#define SWITCH_OP(T,NDEREF,VCODE,ACODE,LCODE,SCODE,SUCODE) \
  switch (XTAG((T))) { 			        \
    case REF0:                                  \
      VCODE                                     \
    case LST:                                   \
      LCODE                                     \
    case SUSP:                                  \
      SUCODE                                    \
    case STR:                                   \
      SCODE                                     \
    default:                                    \
      ACODE                                     \
  }

#define XNDEREF(X,LAB)

#define GET_ARG(A,I)  YAP_ArgOfTerm((I),(A))
#define GET_CAR(A)  YAP_HeadOfTerm(A)
#define GET_CDR(A)  YAP_TailOfTerm(A)

#define MAKE_NVAR(id) ( (YAP_Term)(id) )

#define float_psc ((YAP_Functor)FunctorDouble)

#define NEW_HEAP_FREE   (*HR = (CELL)HR); HR++

#define nil_sym   YAP_TermNil()

extern BPLONG illegal_arguments;
extern BPLONG failure_atom;
extern BPLONG number_var_exception;

extern BPLONG toam_signal_vec;

#define unify YAP_UnifyINT

extern inline INLINE_ONLY int YAP_UnifyINT(YAP_Term t1, YAP_Term t2);
extern inline INLINE_ONLY int YAP_UnifyINT(YAP_Term t1, YAP_Term t2) { return YAP_Unify(t1,t2); }

INLINE_ONLY char *
bp_term_2_string(TERM t)
{
  char *buf = malloc(256);
  if (!buf) return NULL;
  YAP_WriteBuffer(t, buf, 256, 0);
  return buf;
}

// char *bp_get_name(TERM t)
INLINE_ONLY int
bp_string_2_term(const char *s, TERM to, TERM tv)
{
  TERM t0 = YAP_ReadBuffer(s, NULL);
  TERM t1 = YAP_TermNil(); // for now
  return unify(t0, to) && unify(t1,tv);
}

INLINE_ONLY SYM_REC_PTR
insert(const char *name, int size, int arity)
{
  if (!arity) {
    return (SYM_REC_PTR)YAP_LookupAtom(name);
  }
  return (SYM_REC_PTR)YAP_MkFunctor(YAP_LookupAtom(name), arity);
}

INLINE_ONLY int
compare(TERM t1, TERM t2)
{
  // compare terms??
  return YAP_CompareTerms(t1,t2);
}

INLINE_ONLY void
write_term(TERM t)
{
  YAP_Write(t,NULL,0);
}

INLINE_ONLY NORET quit(const char *s);
INLINE_ONLY NORET myquit(int i, const char *s);

INLINE_ONLY NORET quit(const char *s)
{
  fprintf(stderr,"PRISM QUIT: %s\n",s);
  exit(0);
}


INLINE_ONLY NORET myquit(int i, const char *s)
{
  fprintf(stderr,"PRISM QUIT: %s\n",s);
  exit(i);
}

// vsc: why two arguments?
static inline int 
list_length(BPLONG t1, BPLONG t2)
{
  return YAP_ListLength((TERM)t1);
}

#define PRE_NUMBER_VAR(X) 

INLINE_ONLY void
numberVarTermOpt(TERM t)
{
  YAP_NumberVars(t, 0);
}

INLINE_ONLY TERM
unnumberVarTerm(TERM t, BPLONG_PTR pt1, BPLONG_PTR pt2)
{
  return YAP_UnNumberVars(t);
}

INLINE_ONLY int
unifyNumberedTerms(TERM t1, TERM t2)
{
  if (YAP_Unify(t1,t2))
    return TRUE;
  return FALSE;
}

#define IsNumberedVar YAP_IsNumberedVariable

#else

#define GET_ARITY_ATOM GET_ARITY
#define GET_ARITY_STR GET_ARITY

#define GET_NAME_STR     GET_NAME
#define GET_NAME_ATOM    GET_NAME

/*====================================================================*/

#define NULL_TERM ((TERM)(0))

/*--------------------------------*/

/* These are the safer versions of DEREF and NDEREF macros.           */

#define XDEREF(op) \
	do { if(TAG(op) || (op) == FOLLOW(op)) { break; } (op) = FOLLOW(op); } while(1)
#define XNDEREF(op, label) \
	do { if(TAG(op) || (op) == FOLLOW(op)) { break; } (op) = FOLLOW(op); goto label; } while(1)

/*--------------------------------*/

/* This low-level macro provides more detailed information about the  */
/* type of a given term than TAG(op).                                 */

#define XTAG(op) ((op) & TAG_MASK)

#define REF0 0x0L
#define REF1 TOP_BIT
#define INT  INT_TAG
#define NVAR TAG_MASK

/*--------------------------------*/

/* The following macros are the same as IsNumberedVar and NumberVar  */
/* respectively, provided just for more consistent naming.           */

#define IS_NVAR(op) ( ((op) & TAG_MASK) == NVAR )
#define MAKE_NVAR(id) ( (((BPLONG)(id)) << 2) | NVAR )

/*--------------------------------*/

/* This macro is redefined to reduce warnings on GCC 4.x.            */

#if defined LINUX && ! defined M64BITS
#undef  UNTAGGED_ADDR
#define UNTAGGED_ADDR(op) ( (((BPLONG)(op)) & VAL_MASK0) | addr_top_bit )
#endif

/*====================================================================*/

#endif /* YAP */

bool        bpx_is_var(TERM);
bool        bpx_is_atom(TERM);
bool        bpx_is_integer(TERM);
bool        bpx_is_float(TERM);
bool        bpx_is_nil(TERM);
bool        bpx_is_list(TERM);
bool        bpx_is_structure(TERM);
bool        bpx_is_compound(TERM);
bool        bpx_is_unifiable(TERM, TERM);
bool        bpx_is_identical(TERM, TERM);

TERM        bpx_get_call_arg(BPLONG, BPLONG);

BPLONG      bpx_get_integer(TERM);
double      bpx_get_float(TERM);
const char* bpx_get_name(TERM);
int         bpx_get_arity(TERM);
TERM        bpx_get_arg(BPLONG, TERM);
TERM        bpx_get_car(TERM);
TERM        bpx_get_cdr(TERM);

TERM        bpx_build_var(void);
TERM        bpx_build_integer(BPLONG);
TERM        bpx_build_float(double);
TERM        bpx_build_atom(const char *);
TERM        bpx_build_list(void);
TERM        bpx_build_nil(void);
TERM        bpx_build_structure(const char *, BPLONG);

bool        bpx_unify(TERM, TERM);

TERM        bpx_string_2_term(const char *);
const char* bpx_term_2_string(TERM);

#endif /* BPX_H */
