/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		arith1.c						 *
* Last rev:								 *
* mods:									 *
* comments:	arithmetical expression evaluation			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

/*
 * This file implements unary arithmetic operations in YAP
 *
 */

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "eval.h"

#define E_FUNC   blob_type
#define E_ARGS   , arith_retptr o

#define RINT(v)     (o)->Int = v; return(long_int_e)
#define RFLOAT(v)   (o)->dbl = v; return(double_e)
#define RBIG(v)     (o)->big = v; return(big_int_e)
#if  USE_GMP
static blob_type
float_to_int(Float v, union arith_ret *o)
{
  Int i = (Int)v;
  if (i-v == 0.0) {
    o->Int = i;
    return(long_int_e);
  } else {
    MP_INT *new = PreAllocBigNum();

    mpz_set_d(new, v);
    o->big = new;
    return(big_int_e);
  }
}
#define RBIG_FL(v)  return(float_to_int(v,o))
#else
#define RBIG_FL(v)  (o)->Int = (Int)v; return(long_int_e)
#endif
#define RERROR()    return(db_ref_e)

inline static Functor
AritFunctorOfTerm(Term t) {
  if (IsVarTerm(t)) {
    return(FunctorDBRef);
  }
  if (IsApplTerm(t)) {
    return(FunctorOfTerm(t));
  } else {
    if (IsIntTerm(t))
      return(FunctorLongInt);
    else
      return(FunctorDBRef);
  }
}

static Term
EvalToTerm(blob_type f, union arith_ret *res)
{
  switch (f) {
  case long_int_e:
    return(MkIntegerTerm(res->Int));
  case double_e:
    return(MkFloatTerm(res->dbl));
#ifdef USE_GMP
  case big_int_e:
    return(MkBigIntTerm(res->big));
#endif
  default:
    return(TermNil);
  }
}

typedef blob_type (*f_unexp)(Term, arith_retptr);

typedef struct init_un_eval {
  char          *OpName;
  f_unexp        f;
} InitUnEntry;

/* Some compilers just don't get it */

#ifdef __MINGW32__
#undef HAVE_ASINH
#undef HAVE_ACOSH
#undef HAVE_ATANH
#undef HAVE_ARINT
#undef HAVE_FINITE
#endif

#if !HAVE_ASINH
#define asinh(F)  (log((F)+sqrt((F)*(F)+1)))
#endif
#if !HAVE_ACOSH
#define acosh(F)  (log((F)+sqrt((F)*(F)-1)))
#endif
#if !HAVE_ATANH
#define atanh(F)  (log((1+(F))/(1-(F)))/2)
#endif
#if !HAVE_RINT
inline static double
rint(double x) {
  double d = x - floor(x);
  if (d > 0.5) {
    return(ceil(x));
  } else if (d > 0.5) {
    return(floor(x));
  } else {
    double z = floor(x);
    if ((z/2)*2 == z) {
      return(z);
    } else {
      return(ceil(x));
    }
  }
}
#endif

/*
  do nothing...
*/
static E_FUNC
p_uplus(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  blob_type bt;
  union arith_ret v;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    RINT(IntegerOfTerm(t));
  case double_e:
    RFLOAT(FloatOfTerm(t));
#ifdef USE_GMP
  case big_int_e:
    {
      RBIG(BigIntOfTerm(t));
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT(v.Int);
    case double_e:
      RFLOAT(v.dbl);
#ifdef USE_GMP
    case big_int_e:
      {
	RBIG(v.big);
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  }
}

/*
  unary minus: -
*/
static E_FUNC
p_uminus(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    RINT(-IntegerOfTerm(t));
  case double_e:
    RFLOAT(-FloatOfTerm(t));
#ifdef USE_GMP
  case big_int_e:
    {
      MP_INT *new = PreAllocBigNum();

      mpz_neg(new, BigIntOfTerm(t));
      RBIG(new);
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT(-v.Int);
    case double_e:
      RFLOAT(-v.dbl);
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = PreAllocBigNum();

	mpz_neg(new, v.big);
	RBIG(new);
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  }
}

/*
  unary negation is \
*/
static E_FUNC
p_unot(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    RINT(~IntegerOfTerm(t));
  case double_e:
    Error(TYPE_ERROR_INTEGER, t, "\\(f)", FloatOfTerm(t));
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    {
      MP_INT *new = PreAllocBigNum();

      mpz_com(new, BigIntOfTerm(t));
      RBIG(new);
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT(~v.Int);
    case double_e:
      Error(TYPE_ERROR_INTEGER, t, "\\(%f)", v.dbl);
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = PreAllocBigNum();
	
	mpz_com(new, v.big);
	RBIG(new);
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  }
}

/*
  exponentiation exp(x)
*/
static E_FUNC
p_exp(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    RFLOAT(exp(IntegerOfTerm(t)));
  case double_e:
    RFLOAT(exp(FloatOfTerm(t)));
#ifdef USE_GMP
  case big_int_e:
    RFLOAT(mpz_get_d(BigIntOfTerm(t)));
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RFLOAT(exp(v.Int));
    case double_e:
      RFLOAT(exp(v.dbl));
#ifdef USE_GMP
    case big_int_e:
      RFLOAT(mpz_get_d(v.big));
#endif
    default:
      /* Error */
      RERROR();
    }
  }
}

/*
  natural logarithm log(x)
*/
static E_FUNC
p_log(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  if (dbl > 0) {
    RFLOAT(log(dbl));
  } else if (dbl == 0) {
    Error(DOMAIN_ERROR_NOT_ZERO, t, "log(%f)", dbl);
    P = (yamop *)FAILCODE;
    RERROR();
  } else {
    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t, "log(%f)", dbl);
    P = (yamop *)FAILCODE;
    RERROR();
  }
}

/*
  base 10 logarithm log10(x)
*/
static E_FUNC
p_log10(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  if (dbl > 0) {
    RFLOAT(log10(dbl));
  } else if (dbl == 0) {
    Error(DOMAIN_ERROR_NOT_ZERO, t, "log10(%f)", dbl);
    P = (yamop *)FAILCODE;
    RERROR();
  } else {
    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t, "log10(%f)", dbl);
    P = (yamop *)FAILCODE;
    RERROR();
  }
}

/*
  square root sqrt(x)
*/
static E_FUNC
p_sqrt(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl, out;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }
  out = sqrt(dbl);
#if HAVE_ISNAN
  if (isnan(out)) {
    Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "acos(%f)", dbl);
    P = (yamop *)FAILCODE;
    RERROR();
  }
#endif
  RFLOAT(out);
}

/*
  sine sin(x) ? why did they take the e
*/
static E_FUNC
p_sin(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  RFLOAT(sin(dbl));
}

/*
  cosine cos(x)
*/
static E_FUNC
p_cos(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  RFLOAT(cos(dbl));
}

/*
  tangent tan(x)
*/
static E_FUNC
p_tan(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  RFLOAT(tan(dbl));
}

/*
  hyperbolic sine  sinh(x) = (exp(x) - exp(-x)) / 2.
*/
static E_FUNC
p_sinh(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  RFLOAT(sinh(dbl));
}

/*
  hyperbolic cosine  cosh(x) = (exp(x) + exp(-x)) / 2.
*/
static E_FUNC
p_cosh(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  RFLOAT(cosh(dbl));
}

/*
  hyperbolic tangent  tanh(x)
*/
static E_FUNC
p_tanh(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  RFLOAT(tanh(dbl));
}

/*
  asin(x) arc sine function
*/
static E_FUNC
p_asin(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl, out;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  out = asin(dbl);
#if HAVE_ISNAN
  if (isnan(out)) {
    Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "acos(%f)", dbl);
    P = (yamop *)FAILCODE;
    RERROR();
  }
#endif
  RFLOAT(out);
}

/*
  acos(x) arc cosine function
*/
static E_FUNC
p_acos(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl, out;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  out = acos(dbl);
#if HAVE_ISNAN
  if (isnan(out)) {
    Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "acos(%f)", dbl);
    P = (yamop *)FAILCODE;
    RERROR();
  }
#endif
  RFLOAT(out);
}

/*
  atan(x) arc tangent function
*/
static E_FUNC
p_atan(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  RFLOAT(atan(dbl));
}

/*
  asinh(x) arc hyperbolic sine
*/
static E_FUNC
p_asinh(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  RFLOAT(asinh(dbl));
}

/*
  acosh(x) arc hyperbolic cosine
*/
static E_FUNC
p_acosh(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl, out;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  out = acosh(dbl);
#if HAVE_ISNAN
  if (isnan(out)) {
    Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "acosh(%f)", dbl);
    P = (yamop *)FAILCODE;
    RERROR();
  }
#endif
  RFLOAT(out);
}

/*
  atanh(x) arc hyperbolic tangent
*/
static E_FUNC
p_atanh(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl, out;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    dbl = IntegerOfTerm(t);
    break;
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    dbl = mpz_get_d(BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      dbl = v.Int;
      break;
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      dbl = mpz_get_d(v.big);
      break;
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  out = atanh(dbl);
#if HAVE_ISNAN
  if (isnan(out)) {
    Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "acosh(%f)", dbl);
    P = (yamop *)FAILCODE;
    RERROR();
  }
#endif
  RFLOAT(out);
}

/*
  floor(x) maximum integer greatest or equal to X

  There are really two built-ins:
  SICStus converts from int/big/float -> float
  ISO only converts from float -> int/big

*/
static E_FUNC
p_floor(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      Error(TYPE_ERROR_FLOAT, t, "X is floor(%f)", IntegerOfTerm(t));
      P = (yamop *)FAILCODE;
      RERROR();
    } else {
      RFLOAT(IntegerOfTerm(t));
    }
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      MP_INT *big = BigIntOfTerm(t);
      Int sz = 2+mpz_sizeinbase(big,10);
      char *s = malloc(sz);

      if (s != NULL) {
	mpz_get_str(s, 10, BigIntOfTerm(t));
	Error(TYPE_ERROR_FLOAT, t, "X is floor(%s)", IntegerOfTerm(t));
	P = (yamop *)FAILCODE;
	free(s);
	RERROR();
      } else {
	Error(TYPE_ERROR_FLOAT, t, "X is floor(t)");
	P = (yamop *)FAILCODE;
	RERROR();
      }
    } else {
      RFLOAT(mpz_get_d(BigIntOfTerm(t)));
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	Error(TYPE_ERROR_FLOAT, t, "X is floor(%f)", v.Int);
	P = (yamop *)FAILCODE;
	RERROR();
      } else {
	RFLOAT(v.Int);
      }
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	MP_INT *big = v.big;
	Int sz = 2+mpz_sizeinbase(big,10);
	char *s = malloc(sz);

	if (s != NULL) {
	  mpz_get_str(s, 10, BigIntOfTerm(t));
	  Error(TYPE_ERROR_FLOAT, t, "X is floor(%s)", IntegerOfTerm(t));
	  free(s);
	  P = (yamop *)FAILCODE;
	  RERROR();
	} else {
	  Error(TYPE_ERROR_FLOAT, t, "X is floor(t)");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
      } else {
	RFLOAT(mpz_get_d(v.big));
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
    RBIG_FL(floor(dbl));
  } else {
    RFLOAT(floor(dbl));
  }
}

/*
  ceiling(x) minimum integer smallest or equal to X
*/
static E_FUNC
p_ceiling(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      Error(TYPE_ERROR_FLOAT, t, "X is ceiling(%f)", IntegerOfTerm(t));
      P = (yamop *)FAILCODE;
      RERROR();
    } else {
      RFLOAT(IntegerOfTerm(t));
    }
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      MP_INT *big = BigIntOfTerm(t);
      Int sz = 2+mpz_sizeinbase(big,10);
      char *s = malloc(sz);

      if (s != NULL) {
	mpz_get_str(s, 10, BigIntOfTerm(t));
	Error(TYPE_ERROR_FLOAT, t, "X is ceiling(%s)", IntegerOfTerm(t));
	free(s);
	P = (yamop *)FAILCODE;
	RERROR();
      } else {
	Error(TYPE_ERROR_FLOAT, t, "X is ceiling(t)");
	P = (yamop *)FAILCODE;
	RERROR();
      }
    } else {
      RFLOAT(mpz_get_d(BigIntOfTerm(t)));
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	Error(TYPE_ERROR_FLOAT, t, "X is ceiling(%f)", v.Int);
	P = (yamop *)FAILCODE;
	RERROR();
      } else {
	RFLOAT(v.Int);
      }
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	MP_INT *big = v.big;
	Int sz = 2+mpz_sizeinbase(big,10);
	char *s = malloc(sz);

	if (s != NULL) {
	  mpz_get_str(s, 10, BigIntOfTerm(t));
	  Error(TYPE_ERROR_FLOAT, t, "X is ceiling(%s)", IntegerOfTerm(t));
	  free(s);
	  P = (yamop *)FAILCODE;
	  RERROR();
	} else {
	  Error(TYPE_ERROR_FLOAT, t, "X is ceiling(t)");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
      } else {
	RFLOAT(mpz_get_d(v.big));
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
    RBIG_FL(ceil(dbl));
  } else {
    RFLOAT(floor(dbl));
  }
}

/*
  round(x) integer closest to 0
*/
static E_FUNC
p_round(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      Error(TYPE_ERROR_FLOAT, t, "X is round(%f)", IntegerOfTerm(t));
      P = (yamop *)FAILCODE;
      RERROR();
    } else {
      RFLOAT(IntegerOfTerm(t));
    }
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      MP_INT *big = BigIntOfTerm(t);
      Int sz = 2+mpz_sizeinbase(big,10);
      char *s = malloc(sz);

      if (s != NULL) {
	mpz_get_str(s, 10, BigIntOfTerm(t));
	Error(TYPE_ERROR_FLOAT, t, "X is round(%s)", IntegerOfTerm(t));
	free(s);
	P = (yamop *)FAILCODE;
	RERROR();
      } else {
	Error(TYPE_ERROR_FLOAT, t, "X is round(t)");
	P = (yamop *)FAILCODE;
	RERROR();
      }
    } else {
      RFLOAT(mpz_get_d(BigIntOfTerm(t)));
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	Error(TYPE_ERROR_FLOAT, t, "X is round(%f)", v.Int);
	P = (yamop *)FAILCODE;
	RERROR();
      } else {
	RFLOAT(v.Int);
      }
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	MP_INT *big = v.big;
	Int sz = 2+mpz_sizeinbase(big,10);
	char *s = malloc(sz);

	if (s == NULL) {
	  mpz_get_str(s, 10, BigIntOfTerm(t));
	  Error(TYPE_ERROR_FLOAT, t, "X is round(%s)", IntegerOfTerm(t));
	  free(s);
	  P = (yamop *)FAILCODE;
	  RERROR();
	} else {
	  Error(TYPE_ERROR_FLOAT, t, "X is round(t)");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
      } else {
	RFLOAT(mpz_get_d(v.big));
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
    RBIG_FL(rint(dbl));
  } else {
    RFLOAT(rint(dbl));
  }
}

/*
  truncate(x) integer closest to 0
*/
static E_FUNC
p_truncate(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      Error(TYPE_ERROR_FLOAT, t, "X is truncate(%f)", IntegerOfTerm(t));
      P = (yamop *)FAILCODE;
      RERROR();
    } else {
      RFLOAT(IntegerOfTerm(t));
    }
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      MP_INT *big = BigIntOfTerm(t);
      Int sz = 2+mpz_sizeinbase(big,10);
      char *s = malloc(sz);

      if (s != NULL) {
	mpz_get_str(s, 10, BigIntOfTerm(t));
	Error(TYPE_ERROR_FLOAT, t, "X is truncate(%s)", IntegerOfTerm(t));
	free(s);
	P = (yamop *)FAILCODE;
	RERROR();
      } else {
	Error(TYPE_ERROR_FLOAT, t, "X is truncate(t)");
	P = (yamop *)FAILCODE;
	RERROR();
      }
    } else {
      RFLOAT(mpz_get_d(BigIntOfTerm(t)));
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	Error(TYPE_ERROR_FLOAT, t, "X is truncate(%f)", v.Int);
	P = (yamop *)FAILCODE;
	RERROR();
      } else {
	RFLOAT(v.Int);
      }
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	MP_INT *big = v.big;
	Int sz = 2+mpz_sizeinbase(big,10);
	char *s = malloc(sz);

	if (s == NULL) {
	  mpz_get_str(s, 10, BigIntOfTerm(t));
	  Error(TYPE_ERROR_FLOAT, t, "X is truncate(%s)", IntegerOfTerm(t));
	  free(s);
	  P = (yamop *)FAILCODE;
	  RERROR();
	} else {
	  Error(TYPE_ERROR_FLOAT, t, "X is truncate(t)");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
      } else {
	RFLOAT(mpz_get_d(v.big));
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
    RBIG_FL(rint(dbl));
  } else {
    RFLOAT(rint(dbl));
  }
}

/*
  integer(x) SICStus integer closest to 0, similar to truncate
*/
static E_FUNC
p_integer(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    RINT(IntegerOfTerm(t));
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    RBIG(BigIntOfTerm(t));
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT(v.Int);
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      RBIG(v.big);
#endif
    default:
      /* Error */
      RERROR();
    }
  }
  if (dbl <= (Float)Int_MAX && dbl >= (Float)Int_MIN) {
    RINT((Int) dbl);
  } else {
#ifdef USE_GMP
    MP_INT *new = PreAllocBigNum();

    mpz_set_d(new, dbl);
    RBIG(new);
#else
    Error(EVALUATION_ERROR_INT_OVERFLOW, MkFloatTerm(dbl), "integer/1");
    P = (yamop *)FAILCODE;
    RERROR();	    
#endif
  }
}

/*
  float(x) SICStus float
*/
static E_FUNC
p_float(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    RFLOAT(IntegerOfTerm(t));
  case double_e:
    RFLOAT(FloatOfTerm(t));
#ifdef USE_GMP
  case big_int_e:
    RFLOAT(mpz_get_d(BigIntOfTerm(t)));
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RFLOAT(v.Int);
    case double_e:
      RFLOAT(v.dbl);
#ifdef USE_GMP
    case big_int_e:
      RFLOAT(mpz_get_d(v.big));
#endif
    default:
      /* Error */
      RERROR();
    }
  }
}

static Int
msb(Int inp)	/* calculate the most significant bit for an integer */
{
  /* the obvious solution: do it by using binary search */
  Int out = 0;
  int off = sizeof(CELL)*4;

  if (inp < 0) {
    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, MkIntegerTerm(inp),
	  "msb/1 received %d", inp);
    P = (yamop *)FAILCODE;
    return(0);
  }

  while (off) {
    int limit = 1 << (off);
    if (inp >= limit) {
      out += off;
      inp >>= off;
    }
    off >>= 1;
  }
  return(out);
}

/*
  abs(x): absolute value of a number
*/
static E_FUNC
p_abs(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    RINT(labs(IntegerOfTerm(t)));
  case double_e:
    RFLOAT(fabs(FloatOfTerm(t)));
#ifdef USE_GMP
  case big_int_e:
    {
      MP_INT *new = PreAllocBigNum();

      mpz_abs(new, BigIntOfTerm(t));
      RBIG(new);
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT(labs(v.Int));
    case double_e:
      RFLOAT(fabs(v.dbl));
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = PreAllocBigNum();

	mpz_abs(new, v.big);
	RBIG(new);
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  }
}

/*
  msb(x) most significant bit
*/
static E_FUNC
p_msb(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    RINT(msb(IntegerOfTerm(t)));
  case double_e:
    Error(TYPE_ERROR_INTEGER, t, "msb(%f)", FloatOfTerm(t));
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    RINT(mpz_sizeinbase(BigIntOfTerm(t),2));
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT(v.Int);
    case double_e:
      Error(TYPE_ERROR_INTEGER, t, "msb(%f)", v.dbl);
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      RINT(mpz_sizeinbase(v.big,2));
#endif
    default:
      /* Error */
      RERROR();
    }
  }
}

/*
  float_fractional_part(x) fraction for a float.
*/
static E_FUNC
p_ffracp(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      Error(TYPE_ERROR_FLOAT, t, "X is float_fractional_part(%f)", IntegerOfTerm(t));
      P = (yamop *)FAILCODE;
      RERROR();
    } else {
      RFLOAT(0.0);
    }
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      MP_INT *big = BigIntOfTerm(t);
      Int sz = 2+mpz_sizeinbase(big,10);
      char *s = malloc(sz);

      if (s != NULL) {
	mpz_get_str(s, 10, BigIntOfTerm(t));
	Error(TYPE_ERROR_FLOAT, t, "X is float_fractional_part(%s)", IntegerOfTerm(t));
	free(s);
	P = (yamop *)FAILCODE;
	RERROR();
      } else {
	Error(TYPE_ERROR_FLOAT, t, "X is float_fractional_part(t)");
	P = (yamop *)FAILCODE;
	RERROR();
      }
    } else {
      RFLOAT(0.0);
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	Error(TYPE_ERROR_FLOAT, t, "X is float_fractional_part(%f)", v.Int);
	P = (yamop *)FAILCODE;
	RERROR();
      } else {
	RFLOAT(0.0);
      }
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	MP_INT *big = v.big;
	Int sz = 2+mpz_sizeinbase(big,10);
	char *s = malloc(sz);

	if (s == NULL) {
	  mpz_get_str(s, 10, BigIntOfTerm(t));
	  Error(TYPE_ERROR_FLOAT, t, "X is float_fractional_part(%s)", IntegerOfTerm(t));
	  free(s);
	  P = (yamop *)FAILCODE;
	  RERROR();
	} else {
	  Error(TYPE_ERROR_FLOAT, t, "X is float_fractional_part(t)");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
      } else {
	RFLOAT(0.0);
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  }

  RFLOAT(dbl-ceil(dbl));
}

/*
  float_integer_part(x) integer for a float.
*/
static E_FUNC
p_fintp(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      Error(TYPE_ERROR_FLOAT, t, "X is float_integer_part(%f)", IntegerOfTerm(t));
      P = (yamop *)FAILCODE;
      RERROR();
    } else {
      RFLOAT(IntegerOfTerm(t));
    }
  case double_e:
    dbl = FloatOfTerm(t);
    break;
#ifdef USE_GMP
  case big_int_e:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      MP_INT *big = BigIntOfTerm(t);
      Int sz = 2+mpz_sizeinbase(big,10);
      char *s = malloc(sz);

      if (s == NULL) {
	mpz_get_str(s, 10, BigIntOfTerm(t));
	Error(TYPE_ERROR_FLOAT, t, "X is float_integer_part(%s)", IntegerOfTerm(t));
	free(s);
	P = (yamop *)FAILCODE;
	RERROR();
      } else {
	Error(TYPE_ERROR_FLOAT, t, "X is float_integer_part(t)");
	P = (yamop *)FAILCODE;
	RERROR();
      }
    } else {
      RFLOAT(mpz_get_d(BigIntOfTerm(t)));
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	Error(TYPE_ERROR_FLOAT, t, "X is float_integer_part(%f)", v.Int);
	P = (yamop *)FAILCODE;
	RERROR();
      } else {
	RFLOAT(v.Int);
      }
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	MP_INT *big = v.big;
	Int sz = 2+mpz_sizeinbase(big,10);
	char *s = malloc(sz);

	if (s == NULL) {
	  mpz_get_str(s, 10, BigIntOfTerm(t));
	  Error(TYPE_ERROR_FLOAT, t, "X is float_integer_part(%s)", IntegerOfTerm(t));
	  free(s);
	  P = (yamop *)FAILCODE;
	  RERROR();
	} else {
	  Error(TYPE_ERROR_FLOAT, t, "X is float_integer_part(t)");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
      } else {
	RFLOAT(mpz_get_d(v.big));
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  }
  RFLOAT(rint(dbl));
}

/*
  sign(x) sign of a number.
*/
static E_FUNC
p_sign(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;
  Float dbl;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    {
      Int x = IntegerOfTerm(t);

      RINT((x > 0 ? 1 : (x < 0 ? -1 : 0)));
    }
  case double_e:
    dbl = FloatOfTerm(t);

    RINT((dbl > 0.0 ? 1 : (dbl < 0.0 ? -1 : 0)));
#ifdef USE_GMP
  case big_int_e:
    RINT(mpz_sgn(BigIntOfTerm(t)));
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT((v.Int > 0 ? 1 : (v.Int < 0 ? -1 : 0)));
    case double_e:
      RINT((v.dbl > 0.0 ? 1 : (v.dbl < 0.0 ? -1 : 0)));
#ifdef USE_GMP
    case big_int_e:
      RINT(mpz_sgn(v.big));
#endif
    default:
      /* Error */
      RERROR();
    }
  }
  RFLOAT(rint(dbl));
}

static InitUnEntry InitUnTab[] = {
  {"+", p_uplus},
  {"-", p_uminus},
  {"\\", p_unot},
  {"exp", p_exp},
  {"log", p_log},
  {"log10", p_log10},
  {"sqrt", p_sqrt},
  {"sin", p_sin},
  {"cos", p_cos},
  {"tan", p_tan},
  {"sinh", p_sinh},
  {"cosh", p_cosh},
  {"tanh", p_tanh},
  {"asin", p_asin},
  {"acos", p_acos},
  {"atan", p_atan},
  {"asinh", p_asinh},
  {"acosh", p_acosh},
  {"atanh", p_atanh},
  {"floor", p_floor},
  {"ceiling", p_ceiling},
  {"round", p_round},
  {"truncate", p_truncate},
  {"integer", p_integer},
  {"float", p_float},
  {"abs", p_abs},
  {"msb", p_msb},
  {"float_fractional_part", p_ffracp},
  {"float_integer_part", p_fintp},
  {"sign", p_sign}
};

static Int 
p_unary_is(void)
{				/* X is Y	 */
  Term t = Deref(ARG2);
  union arith_ret res;

  if (IsVarTerm(t)) {
    Error(INSTANTIATION_ERROR, ARG2, "X is Y");
    return(FALSE);
  }
  if (IsIntTerm(t)) {
    blob_type f = InitUnTab[IntOfTerm(t)].f(Deref(ARG3),&res);
    return (unify_constant(ARG1,EvalToTerm(f,&res)));
  }
  if (IsAtomTerm(t)) {
    Atom name = AtomOfTerm(t);
    ExpEntry *p;
    blob_type f;

    if (EndOfPAEntr(p = RepExpProp(GetExpProp(name, 1)))) {
      Term ti[2];

      /* error */
      ti[0] = t;
      ti[1] = MkIntTerm(1);
      t = MkApplTerm(MkFunctor(LookupAtom("/"),2), 2, ti);
      Error(TYPE_ERROR_EVALUABLE, t,
	    "functor %s/%d for arithmetic expression",
	    RepAtom(name)->StrOfAE,1);
      P = (yamop *)FAILCODE;
      return(FALSE);
    }
    f = p->FOfEE.unary(Deref(ARG3),&res);
    return (unify_constant(ARG1,EvalToTerm(f,&res)));
  }
  return(FALSE);
}

void
InitUnaryExps(void)
{
  unsigned int    i;
  ExpEntry       *p;

  for (i = 0; i < sizeof(InitUnTab)/sizeof(InitUnEntry); ++i) {
    AtomEntry *ae = RepAtom(LookupAtom(InitUnTab[i].OpName));
    WRITE_LOCK(ae->ARWLock);
    if (LockedGetExpProp(ae, 1)) {
      WRITE_UNLOCK(ae->ARWLock);
      break;
    }
    p = (ExpEntry *) AllocAtomSpace(sizeof(ExpEntry));
    p->KindOfPE = ExpProperty;
    p->ArityOfEE = 1;
    p->ENoOfEE = 1;
    p->FOfEE.unary = InitUnTab[i].f;
    p->NextOfPE = ae->PropOfAE;
    ae->PropOfAE = AbsExpProp(p);
    WRITE_UNLOCK(ae->ARWLock);
  }
  InitCPred("is", 3, p_unary_is, TestPredFlag | SafePredFlag);
}

/* This routine is called from Restore to make sure we have the same arithmetic operators */
int
ReInitUnaryExps(void)
{
  unsigned int i;
  Prop p;

  for (i = 0; i < sizeof(InitUnTab)/sizeof(InitUnEntry); ++i) {
    AtomEntry *ae = RepAtom(FullLookupAtom(InitUnTab[i].OpName));

    WRITE_LOCK(ae->ARWLock);
    if ((p = LockedGetExpProp(ae, 1)) == NULL) {
      WRITE_UNLOCK(ae->ARWLock);
      return(FALSE);
    }
    RepExpProp(p)->FOfEE.unary = InitUnTab[i].f;
    WRITE_UNLOCK(ae->ARWLock);
  }
  return(TRUE);
}

