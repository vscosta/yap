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
#ifdef HAVE_FLOAT_H
#include <float.h>
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

typedef enum {
  op_pi,
  op_e,
  op_epsilon,
  op_inf,
  op_nan,
  op_random,
  op_cputime,
  op_heapused,
  op_localsp,
  op_globalsp,
  op_b,
  op_env,
  op_tr,
  op_stackfree
} arith0_op;

typedef enum {
  op_uplus,
  op_uminus,
  op_unot,
  op_exp,
  op_log,
  op_log10,
  op_sqrt,
  op_sin,
  op_cos,
  op_tan,
  op_sinh,
  op_cosh,
  op_tanh,
  op_asin,
  op_acos,
  op_atan,
  op_asinh,
  op_acosh,
  op_atanh,
  op_floor,
  op_ceiling,
  op_round,
  op_truncate,
  op_integer,
  op_float,
  op_abs,
  op_lsb,
  op_msb,
  op_popcount,
  op_ffracp,
  op_fintp,
  op_sign,
  op_lgamma,
  op_erf,
  op_erfc,
  op_rational,
  op_rationalize,
  op_random1
} arith1_op;

typedef enum {
  op_plus,
  op_minus,
  op_times,
  op_fdiv,
  op_mod,
  op_rem,
  op_div,
  op_idiv,
  op_sll,
  op_slr,
  op_and,
  op_or,
  op_xor,
  op_atan2,
  /* C-Prolog exponentiation */
  op_power,
  /* ISO-Prolog exponentiation */
  /*  op_power, */
  op_power2,
  /* Quintus exponentiation */
  /* op_power, */
  op_gcd,
  op_min,
  op_max,
  op_rdiv
} arith2_op;

Functor     EvalArg(Term);

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

void	Yap_InitConstExps(void);
void	Yap_InitUnaryExps(void);
void	Yap_InitBinaryExps(void);

int	Yap_ReInitConstExps(void);
int	Yap_ReInitUnaryExps(void);
int	Yap_ReInitBinaryExps(void);

Term	Yap_eval_atom(Int);
Term	Yap_eval_unary(Int,Term);
Term	Yap_eval_binary(Int,Term,Term);

Term	Yap_InnerEval(Term);
Int     Yap_ArithError(yap_error_number,Term,char *msg, ...);

#include "inline-only.h"
INLINE_ONLY inline EXTERN Term
Yap_Eval(Term t);

INLINE_ONLY inline EXTERN Term
Yap_Eval(Term t)
{
  if (t == 0L || ( !IsVarTerm(t) && IsNumTerm(t) ))
    return t;
  return Yap_InnerEval(t);
}

#ifdef P
inline static Term
Yap_FoundArithError(Term t, Term inp)
{ 
  CACHE_REGS
  if (LOCAL_Error_TYPE) {
    Yap_Error(LOCAL_Error_TYPE, (inp ? inp : LOCAL_Error_Term), LOCAL_ErrorMessage);
    P = FAILCODE;
    return 0L;
  }
  return t;
}
#endif


#define RINT(v)       return(MkIntegerTerm(v))
#define RFLOAT(v)     return(MkFloatTerm(v))
#define RBIG(v)       return(Yap_MkBigIntTerm(v))
#define RERROR()      return(0L)

static inline blob_type
ETypeOfTerm(Term t)
{
  if (IsIntTerm(t)) 
    return long_int_e;
  if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    if (f == FunctorDouble)
      return double_e;
    if (f == FunctorLongInt)
      return long_int_e;
    if (f == FunctorBigInt) {
      return big_int_e;
    }
  }
  return db_ref_e;
}

#if USE_GMP
Term  Yap_gmq_rdiv_int_int(Int, Int);
Term  Yap_gmq_rdiv_int_big(Int, Term);
Term  Yap_gmq_rdiv_big_int(Term, Int);
Term  Yap_gmq_rdiv_big_big(Term, Term);

Term  Yap_gmp_add_ints(Int, Int);
Term  Yap_gmp_sub_ints(Int, Int);
Term  Yap_gmp_mul_ints(Int, Int);
Term  Yap_gmp_sll_ints(Int, Int);
Term  Yap_gmp_add_int_big(Int, Term);
Term  Yap_gmp_sub_int_big(Int, Term);
Term  Yap_gmp_sub_big_int(Term, Int);
Term  Yap_gmp_mul_int_big(Int, Term);
Term  Yap_gmp_div_int_big(Int, Term);
Term  Yap_gmp_div_big_int(Term, Int);
Term  Yap_gmp_div2_big_int(Term, Int);
Term  Yap_gmp_fdiv_int_big(Int, Term);
Term  Yap_gmp_fdiv_big_int(Term, Int);
Term  Yap_gmp_and_int_big(Int, Term);
Term  Yap_gmp_ior_int_big(Int, Term);
Term  Yap_gmp_xor_int_big(Int, Term);
Term  Yap_gmp_sll_big_int(Term, Int);
Term  Yap_gmp_add_big_big(Term, Term);
Term  Yap_gmp_sub_big_big(Term, Term);
Term  Yap_gmp_mul_big_big(Term, Term);
Term  Yap_gmp_div_big_big(Term, Term);
Term  Yap_gmp_div2_big_big(Term, Term);
Term  Yap_gmp_fdiv_big_big(Term, Term);
Term  Yap_gmp_and_big_big(Term, Term);
Term  Yap_gmp_ior_big_big(Term, Term);
Term  Yap_gmp_xor_big_big(Term, Term);
Term  Yap_gmp_mod_big_big(Term, Term);
Term  Yap_gmp_mod_big_int(Term, Int);
Term  Yap_gmp_mod_int_big(Int, Term);
Term  Yap_gmp_rem_big_big(Term, Term);
Term  Yap_gmp_rem_big_int(Term, Int);
Term  Yap_gmp_rem_int_big(Int, Term);
Term  Yap_gmp_exp_int_int(Int,Int);
Term  Yap_gmp_exp_int_big(Int,Term);
Term  Yap_gmp_exp_big_int(Term,Int);
Term  Yap_gmp_exp_big_big(Term,Term);
Term  Yap_gmp_gcd_int_big(Int,Term);
Term  Yap_gmp_gcd_big_big(Term,Term);

Term  Yap_gmp_big_from_64bits(YAP_LONG_LONG);

Term  Yap_gmp_float_to_big(Float);
Term  Yap_gmp_float_to_rational(Float);
Term  Yap_gmp_float_rationalize(Float);
Float Yap_gmp_to_float(Term);
Term  Yap_gmp_add_float_big(Float, Term);
Term  Yap_gmp_sub_float_big(Float, Term);
Term  Yap_gmp_sub_big_float(Term, Float);
Term  Yap_gmp_mul_float_big(Float, Term);
Term  Yap_gmp_fdiv_float_big(Float, Term);
Term  Yap_gmp_fdiv_big_float(Term, Float);

int   Yap_gmp_cmp_big_int(Term, Int);
#define Yap_gmp_cmp_int_big(I, T) (-Yap_gmp_cmp_big_int(T, I))
int   Yap_gmp_cmp_big_float(Term, Float);
#define Yap_gmp_cmp_float_big(D, T) (-Yap_gmp_cmp_big_float(T, D))
int   Yap_gmp_cmp_big_big(Term, Term);

int   Yap_gmp_tcmp_big_int(Term, Int);
#define Yap_gmp_tcmp_int_big(I, T) (-Yap_gmp_tcmp_big_int(T, I))
int   Yap_gmp_tcmp_big_float(Term, Float);
#define Yap_gmp_tcmp_float_big(D, T) (-Yap_gmp_tcmp_big_float(T, D))
int   Yap_gmp_tcmp_big_big(Term, Term);

Term  Yap_gmp_neg_int(Int);
Term  Yap_gmp_abs_big(Term);
Term  Yap_gmp_neg_big(Term);
Term  Yap_gmp_unot_big(Term);
Term  Yap_gmp_floor(Term);
Term  Yap_gmp_ceiling(Term);
Term  Yap_gmp_round(Term);
Term  Yap_gmp_trunc(Term);
Term  Yap_gmp_float_fractional_part(Term);
Term  Yap_gmp_float_integer_part(Term);
Term  Yap_gmp_sign(Term);
Term  Yap_gmp_lsb(Term);
Term  Yap_gmp_msb(Term);
Term  Yap_gmp_popcount(Term);

char *  Yap_gmp_to_string(Term, char *, size_t, int);
size_t  Yap_gmp_to_size(Term, int);

int   Yap_term_to_existing_big(Term, MP_INT *);
int   Yap_term_to_existing_rat(Term, MP_RAT *); 

void  Yap_gmp_set_bit(Int i, Term t);
#endif

#define Yap_Mk64IntegerTerm(i) __Yap_Mk64IntegerTerm((i) PASS_REGS)

INLINE_ONLY inline EXTERN Term __Yap_Mk64IntegerTerm(YAP_LONG_LONG USES_REGS);

INLINE_ONLY inline EXTERN Term
__Yap_Mk64IntegerTerm(YAP_LONG_LONG i USES_REGS)
{
  if (i <= Int_MAX && i >= Int_MIN) {
    return MkIntegerTerm((Int)i);
  } else {
#if USE_GMP
    return Yap_gmp_big_from_64bits(i);
#else
    return MkIntTerm(-1);
#endif
  }
}



inline static int
add_overflow(Int x, Int i, Int j)
{
  return ((i & j & ~x) | (~i & ~j & x)) < 0;
}

inline static Term
add_int(Int i, Int j USES_REGS)
{
  Int x = i+j;
#if USE_GMP
  /* Integer overflow, we need to use big integers */
  Int overflow = (i & j & ~x) | (~i & ~j & x);
  if (overflow < 0) {
    return(Yap_gmp_add_ints(i, j));
  }
#endif
#ifdef BEAM
  RINT(x);
  return( MkIntegerTerm (x));
#else
  RINT(x);
#endif
}
