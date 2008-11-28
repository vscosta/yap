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
#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif
#ifdef HAVE_LIMITS_H
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
  mpz_t big;
#endif
} *arith_retptr;

/*
#define RINT(v)     return(MkIntegerTerm(v))
#define RFLOAT(v)   return(MkFloatTerm(v))
#define RBIG(v)     return(Yap_MkBigIntTerm(v))
#define RBIG_FL(v)  return(Yap_MkBigIntTerm((MP_INT *)(Int)v))
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
#ifdef	MPW
#define FL(X)		((extended)(X))
#else
#define FL(X)		((double)(X))
#endif

extern yap_error_number Yap_matherror;

void	STD_PROTO(Yap_InitConstExps,(void));
void	STD_PROTO(Yap_InitUnaryExps,(void));
void	STD_PROTO(Yap_InitBinaryExps,(void));

int	STD_PROTO(Yap_ReInitConstExps,(void));
int	STD_PROTO(Yap_ReInitUnaryExps,(void));
int	STD_PROTO(Yap_ReInitBinaryExps,(void));

blob_type	STD_PROTO(Yap_Eval,(Term, union arith_ret *));

#if USE_GMP
MP_INT *STD_PROTO(Yap_gmp_add_ints,(Int, Int, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_sub_ints,(Int, Int, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_mul_ints,(Int, Int, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_sll_ints,(Int, Int, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_add_int_big,(Int, MP_INT *, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_sub_int_big,(Int, MP_INT *, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_sub_big_int,(MP_INT *, Int, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_mul_int_big,(Int, MP_INT *, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_div_big_int,(MP_INT *, Int, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_and_int_big,(Int, MP_INT *, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_ior_int_big,(Int, MP_INT *, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_sll_big_int,(MP_INT *, Int, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_add_big_big,(MP_INT *, MP_INT *, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_sub_big_big,(MP_INT *, MP_INT *, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_mul_big_big,(MP_INT *, MP_INT *, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_div_big_big,(MP_INT *, MP_INT *, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_and_big_big,(MP_INT *, MP_INT *, MP_INT *));
MP_INT *STD_PROTO(Yap_gmp_ior_big_big,(MP_INT *, MP_INT *, MP_INT *));



Float  STD_PROTO(Yap_gmp_add_float_big,(Float, MP_INT *));
Float  STD_PROTO(Yap_gmp_sub_float_big,(Float, MP_INT *));
Float  STD_PROTO(Yap_gmp_sub_big_float,(MP_INT *, Float));
Float  STD_PROTO(Yap_gmp_mul_float_big,(Float, MP_INT *));
#endif
