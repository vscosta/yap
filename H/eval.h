/*************************************************************************
*									 *
*	 YAP Prolog 	@(#)eval.h	1.2
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		eval.h							 *
* Last rev:								 *
* mods:									 *
* comments:	arithmetical functions info				 *
*									 *
*************************************************************************/

#include <stdlib.h>

/* C library used to implement floating point functions */
#if HAVE_MATH_H
#include <math.h>
#endif
#if HAVE_IEEEFP_H
#include <ieeefp.h>
#endif
#if HAVE_LIMITS_H
#include <limits.h>
#endif

#ifdef LONG_MAX
#define Int_MAX  LONG_MAX
#else
#define Int_MAX  ((Int)((~((CELL)0))>>1))
#endif
#ifdef LONG_MIN
#define Int_MIN  LONG_MIN
#else
#define Int_MIN  (-Int_MAX-(CELL)1)
#endif

typedef union arith_ret {
  Int Int;
  Float dbl;
#ifdef USE_GMP
  MP_INT *big;
#endif
} *arith_retptr;

/*
#define RINT(v)     return(MkIntegerTerm(v))
#define RFLOAT(v)   return(MkFloatTerm(v))
#define RBIG(v)     return(MkBigIntTerm(v))
#define RBIG_FL(v)  return(MkBigIntTerm((MP_INT *)(Int)v))
#define RERROR()    return(MkIntTerm(0))
*/

Functor     STD_PROTO(EvalArg,(Term,arith_retptr));

/* Needed to handle numbers:
   	these two macros are fundamental in the integer/float conversions */

#ifdef C_PROLOG
#define FlIsInt(X)	( (X) == (Int)(X) && IntInBnd((X)) )
#else
#define FlIsInt(X)	( FALSE )
#endif



#ifdef M_WILLIAMS
#define MkEvalFl(X)	MkFloatTerm(X)
#else
#define MkEvalFl(X)	( FlIsInt(X) ? MkIntTerm((Int)(X)) : MkFloatTerm(X) )
#endif


/* Macros used by some of the eval functions */
#define REvalInt(I)	{ eval_int = (I); return(FInt); }
#define REvalFl(F)	{ eval_flt = (F); return(FFloat); }
#define REvalError()	{ return(FError); }

/* this macro, dependent on the particular implementation
	is used to interface the arguments into the C libraries */
#if	MPW
#define FL(X)		((extended)(X))
#else
#define FL(X)		((double)(X))
#endif

extern yap_error_number YAP_matherror;

void	STD_PROTO(InitConstExps,(void));
void	STD_PROTO(InitUnaryExps,(void));
void	STD_PROTO(InitBinaryExps,(void));

int	STD_PROTO(ReInitConstExps,(void));
int	STD_PROTO(ReInitUnaryExps,(void));
int	STD_PROTO(ReInitBinaryExps,(void));

blob_type	STD_PROTO(Eval,(Term, union arith_ret *));
