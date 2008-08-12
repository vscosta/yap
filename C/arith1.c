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

#define TMP_BIG()     ((o)->big)
#define RINT(v)       (o)->Int = v; return(long_int_e)
#define RFLOAT(v)     (o)->dbl = v; return(double_e)
#define RBIG(v)       return(big_int_e)
#define RERROR()      return(db_ref_e)

#if  USE_GMP
static blob_type
float_to_int(Float v, union arith_ret *o)
{
  Int i = (Int)v;
  if (i-v == 0.0) {
    o->Int = i;
    return long_int_e;
  } else {
    mpz_init_set_d(o->big, v);
    return big_int_e;
  }
}
#define RBIG_FL(v)  return(float_to_int(v,o))
#else
#define RBIG_FL(v)  (o)->Int = (Int)(v); return long_int_e
#endif

#if USE_GMP
static void
process_iso_error(MP_INT *big, Term t, char *operation)
{ /* iso */
  Int sz = 2+mpz_sizeinbase(big,10);
  char *s = Yap_AllocCodeSpace(sz);

  if (s != NULL) {
    mpz_get_str(s, 10, big);
    Yap_Error(TYPE_ERROR_FLOAT, t, "X is %s(%s)", operation, s);
    Yap_FreeCodeSpace(s);
    P = (yamop *)FAILCODE;
  } else {
    Yap_Error(TYPE_ERROR_FLOAT, t, "X is %s(t)",operation);
    P = (yamop *)FAILCODE;
  }
}
#endif

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
    {
      Term t = Yap_MkBigIntTerm(res->big);
      mpz_clear(res->big);
      return t;
    }
#endif
  default:
    return TermNil;
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
      MP_INT *new = TMP_BIG();
      mpz_init_set(new, Yap_BigIntOfTerm(t));
      RBIG(new);
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT(v.Int);
    case double_e:
      RFLOAT(v.dbl);
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = TMP_BIG();
	MPZ_SET(new, v.big);
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
    {
 #ifdef USE_GMP
      Int i = IntegerOfTerm(t);
      
      if (i == Int_MIN) {
	MP_INT *new = TMP_BIG();

	mpz_init_set_si(new, i);
	mpz_neg(new, new);
	RBIG(new);	
      }
      else
#endif
	RINT(-IntegerOfTerm(t));
    }
  case double_e:
    RFLOAT(-FloatOfTerm(t));
#ifdef USE_GMP
  case big_int_e:
    {
      MP_INT *new = TMP_BIG();

      mpz_init_set(new, Yap_BigIntOfTerm(t));
      mpz_neg(new, new);
      RBIG(new);
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT(-v.Int);
    case double_e:
      RFLOAT(-v.dbl);
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = TMP_BIG();

	mpz_init_set(new, v.big);
	mpz_neg(new, new);
	mpz_clear(v.big);
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
    Yap_Error(TYPE_ERROR_INTEGER, t, "\\(f)", FloatOfTerm(t));
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    {
      MP_INT *new = TMP_BIG();

      mpz_init_set(new, Yap_BigIntOfTerm(t));
      mpz_com(new, new);
      RBIG(new);
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT(~v.Int);
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t, "\\(%f)", v.dbl);
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
    {
      MP_INT *new = TMP_BIG();

      MPZ_SET(new, v.big);
      mpz_com(new, new);
      RBIG(new);
    }
#endif
    default:
      /* Yap_Error */
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
    RFLOAT(exp(mpz_get_d(Yap_BigIntOfTerm(t))));
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RFLOAT(exp(v.Int));
    case double_e:
      RFLOAT(exp(v.dbl));
#ifdef USE_GMP
    case big_int_e:
      {
	double dbl = mpz_get_d(v.big);

	mpz_clear(v.big);
	RFLOAT(exp(dbl));
      }
#endif
    default:
      /* Yap_Error */
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  }

  if (dbl >= 0) {
    RFLOAT(log(dbl));
  } else {
    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t, "log(%f)", dbl);
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  }

  if (dbl >= 0) {
    RFLOAT(log10(dbl));
  } else {
    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t, "log10(%f)", dbl);
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  }
  out = sqrt(dbl);
#if HAVE_ISNAN
  if (isnan(out)) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "acos(%f)", dbl);
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  }

  out = asin(dbl);
#if HAVE_ISNAN
  if (isnan(out)) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "asin(%f)", dbl);
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  }

  out = acos(dbl);
#if HAVE_ISNAN
  if (isnan(out)) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "acos(%f)", dbl);
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  }

  out = acosh(dbl);
#if HAVE_ISNAN
  if (isnan(out)) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "acosh(%f)", dbl);
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  }

  out = atanh(dbl);
#if HAVE_ISNAN
  if (isnan(out)) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "atanh(%f)", dbl);
    P = (yamop *)FAILCODE;
    RERROR();
  }
#endif
  RFLOAT(out);
}

/*
  lgamma(x) is the logarithm of the gamma function.
*/
static E_FUNC
p_lgamma(Term t E_ARGS)
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
    dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    break;
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
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
      mpz_clear(v.big);
      break;
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  }

#if HAVE_LGAMMA
 {
   Float out;
   out = lgamma(dbl);
   RFLOAT(out);
 }
#else
  RERROR();
#endif
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
      Yap_Error(TYPE_ERROR_FLOAT, t, "X is floor(%f)", IntegerOfTerm(t));
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
      MP_INT *big = Yap_BigIntOfTerm(t);
      Int sz = 2+mpz_sizeinbase(big,10);
      char *s = Yap_AllocCodeSpace(sz);

      if (s != NULL) {
	mpz_get_str(s, 10, big);
	Yap_Error(TYPE_ERROR_FLOAT, t, "X is floor(%s)", s);
	P = (yamop *)FAILCODE;
	Yap_FreeCodeSpace(s);
	RERROR();
      } else {
	Yap_Error(TYPE_ERROR_FLOAT, t, "X is floor(t)");
	P = (yamop *)FAILCODE;
	RERROR();
      }
    } else {
      dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	Yap_Error(TYPE_ERROR_FLOAT, t, "X is floor(%f)", v.Int);
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
	Int sz = 2+mpz_sizeinbase(v.big,10);
	char *s = Yap_AllocCodeSpace(sz);

	if (s != NULL) {
	  mpz_get_str(s, 10, v.big);
	  mpz_clear(v.big);
	  Yap_Error(TYPE_ERROR_FLOAT, t, "X is floor(%s)", s);
	  Yap_FreeCodeSpace(s);
	  P = (yamop *)FAILCODE;
	  RERROR();
	} else {
	  mpz_clear(v.big);
	  Yap_Error(TYPE_ERROR_FLOAT, t, "X is floor(t)");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
      } else {
	dbl = mpz_get_d(v.big);
	mpz_clear(v.big);
      }
#endif
    default:
      /* Yap_Error */
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
      Yap_Error(TYPE_ERROR_FLOAT, t, "X is ceiling(%f)", IntegerOfTerm(t));
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
      process_iso_error(Yap_BigIntOfTerm(t), t, "ceiling");
      RERROR();
    } else {
      dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	Yap_Error(TYPE_ERROR_FLOAT, t, "X is ceiling(%f)", v.Int);
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
	process_iso_error(v.big, t, "ceiling");
	mpz_clear(v.big);
	RERROR();
      } else {
	dbl = mpz_get_d(v.big);
	mpz_clear(v.big);
      }
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  }

  if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
    RBIG_FL(ceil(dbl));
  } else {
    RFLOAT(ceil(dbl));
  }
}

/* WIN32 machines do not necessarily have rint. This will do for now */
#if HAVE_RINT
#define my_rint(X) rint(X)
#else
static
double my_rint(double x)
{ 
  double y, z; 
  Int n; 
  
  if (x >= 0) { 
    y = x + 0.5; 
    z = floor(y); 
    n = (int) z; 
    if (y == z && n % 2)
      return(z-1); 
  } else { 
    y = x - 0.5; 
    z = ceil(y); 
    n = (int) z; 
    if (y == z && n % 2)
      return(z+1); 
  }
  return(z); 
}
#endif

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
      Yap_Error(TYPE_ERROR_FLOAT, t, "X is round(%f)", IntegerOfTerm(t));
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
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) {
      process_iso_error(Yap_BigIntOfTerm(t), t, "round");
      RERROR();      
    } else {
      dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	Yap_Error(TYPE_ERROR_FLOAT, t, "X is round(%f)", v.Int);
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
	process_iso_error(v.big, t, "round");
	mpz_clear(v.big);
	RERROR();
      } else {
	dbl = mpz_get_d(v.big);
	mpz_clear(v.big);
      }
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  }
  
  if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
    double vl = my_rint(dbl);
    RBIG_FL(vl);
  } else {
    double vl = my_rint(dbl);
    RFLOAT(vl);
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
      Yap_Error(TYPE_ERROR_FLOAT, t, "X is truncate(%f)", IntegerOfTerm(t));
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
      process_iso_error(Yap_BigIntOfTerm(t), t, "truncate");
      RERROR();
    } else {
      dbl = mpz_get_d(Yap_BigIntOfTerm(t));
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	Yap_Error(TYPE_ERROR_FLOAT, t, "X is truncate(%f)", v.Int);
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
	process_iso_error(v.big, t, "truncate");
	mpz_clear(v.big);
	RERROR();
      } else {
	dbl = mpz_get_d(v.big);
	mpz_clear(v.big);
      }
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  }

  if (dbl >= 0 ) {
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      RBIG_FL(floor(dbl));
    } else {
      RFLOAT(floor(dbl));
    }
  } else {
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      RBIG_FL(ceil(dbl));
    } else {
      RFLOAT(ceil(dbl));
    }
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
    {
      MP_INT *new = TMP_BIG();
      mpz_init_set(new, Yap_BigIntOfTerm(t));
      RBIG(new);
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT(v.Int);
    case double_e:
      dbl = v.dbl;
      break;
#ifdef USE_GMP
    case big_int_e:
    {
      MP_INT *new = TMP_BIG();

      MPZ_SET(new,v.big);
      RBIG(new);
    }
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  }
  if (dbl <= (Float)Int_MAX && dbl >= (Float)Int_MIN) {
    RINT((Int) dbl);
  } else {
#ifdef USE_GMP
    MP_INT *new = TMP_BIG();

    mpz_init_set_d(new, dbl);
    RBIG(new);
#else
    Yap_Error(EVALUATION_ERROR_INT_OVERFLOW, MkFloatTerm(dbl), "integer/1");
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
    RFLOAT(mpz_get_d(Yap_BigIntOfTerm(t)));
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RFLOAT(v.Int);
    case double_e:
      RFLOAT(v.dbl);
#ifdef USE_GMP
    case big_int_e:
      {
	Float dbl = mpz_get_d(v.big);
	mpz_clear(v.big);
	RFLOAT(dbl);
      }
#endif
    default:
      /* Yap_Error */
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
    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, MkIntegerTerm(inp),
	  "msb/1 received %d", inp);
    P = (yamop *)FAILCODE;
    return(0);
  }

  while (off) {
    Int limit = 1L << (off);
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
      MP_INT *new = TMP_BIG();

      mpz_init_set(new, Yap_BigIntOfTerm(t));
      mpz_abs(new, new);
      RBIG(new);
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT(labs(v.Int));
    case double_e:
      RFLOAT(fabs(v.dbl));
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new =  TMP_BIG();

	MPZ_SET(new, v.big);
	mpz_abs(new, new);
	RBIG(new);
      }
#endif
    default:
      /* Yap_Error */
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
    Yap_Error(TYPE_ERROR_INTEGER, t, "msb(%f)", FloatOfTerm(t));
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    RINT(mpz_sizeinbase(Yap_BigIntOfTerm(t),2));
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT(v.Int);
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t, "msb(%f)", v.dbl);
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      {
	int sz = mpz_sizeinbase(v.big,2);

	mpz_clear(v.big);
	RINT(sz);
      }
#endif
    default:
      /* Yap_Error */
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
      Yap_Error(TYPE_ERROR_FLOAT, t, "X is float_fractional_part(%f)", IntegerOfTerm(t));
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
      process_iso_error(Yap_BigIntOfTerm(t), t, "float_fractional_part");
      RERROR();
    } else {
      RFLOAT(0.0);
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	Yap_Error(TYPE_ERROR_FLOAT, t, "X is float_fractional_part(%f)", v.Int);
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
	process_iso_error(v.big, t, "float_fractional_part");
	mpz_clear(v.big);
	RERROR();
      } else {
	mpz_clear(v.big);
	RFLOAT(0.0);
      }
#endif
    default:
      /* Yap_Error */
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
      Yap_Error(TYPE_ERROR_FLOAT, t, "X is float_integer_part(%f)", IntegerOfTerm(t));
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
      process_iso_error(Yap_BigIntOfTerm(t), t, "float_integer_part");
      RERROR();
    } else {
      RFLOAT(mpz_get_d(Yap_BigIntOfTerm(t)));
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	Yap_Error(TYPE_ERROR_FLOAT, t, "X is float_integer_part(%f)", v.Int);
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
	process_iso_error(Yap_BigIntOfTerm(t), t, "float_integer_part");
	RERROR();
      } else {
	Float dbl = mpz_get_d(v.big);

	mpz_clear(v.big);
	RFLOAT(dbl);
      }
#endif
    default:
      /* Yap_Error */
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
    RINT(mpz_sgn(Yap_BigIntOfTerm(t)));
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT((v.Int > 0 ? 1 : (v.Int < 0 ? -1 : 0)));
    case double_e:
      RINT((v.dbl > 0.0 ? 1 : (v.dbl < 0.0 ? -1 : 0)));
#ifdef USE_GMP
    case big_int_e:
      { 
	int sgn = mpz_sgn(v.big);

	mpz_clear(v.big);
	RINT(sgn);
      }
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  }
}

/*
  unary negation is \
*/
static E_FUNC
p_random(Term t E_ARGS)
{
  Functor f = AritFunctorOfTerm(t);
  union arith_ret v;
  blob_type bt;

  switch (BlobOfFunctor(f)) {
  case long_int_e:
    RINT(Yap_random()*IntegerOfTerm(t));
  case double_e:
    Yap_Error(TYPE_ERROR_INTEGER, t, "random(%f)", FloatOfTerm(t));
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
    Yap_Error(TYPE_ERROR_INTEGER, t, "random(%f)", FloatOfTerm(t));
    P = (yamop *)FAILCODE;
    RERROR();
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt = Yap_Eval(t, &v);
    /* second case, no need no evaluation */
    switch (bt) {
    case long_int_e:
      RINT(Yap_random()*v.Int);
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t, "random(%f)", v.dbl);
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      Yap_Error(TYPE_ERROR_INTEGER, t, "random(%f)", FloatOfTerm(t));
      P = (yamop *)FAILCODE;
      RERROR();
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  }
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
  {"sign", p_sign},
  {"lgamma", p_lgamma},
  {"random", p_random},
};

static Int 
p_unary_is(void)
{				/* X is Y	 */
  Term t = Deref(ARG2);
  union arith_ret res;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, ARG2, "X is Y");
    return(FALSE);
  }
  if (IsIntTerm(t)) {
    blob_type f = InitUnTab[IntOfTerm(t)].f(Deref(ARG3),&res);
    return (Yap_unify_constant(ARG1,EvalToTerm(f,&res)));
  }
  if (IsAtomTerm(t)) {
    Atom name = AtomOfTerm(t);
    ExpEntry *p;
    blob_type f;

    if (EndOfPAEntr(p = RepExpProp(Yap_GetExpProp(name, 1)))) {
      Term ti[2];

      /* error */
      ti[0] = t;
      ti[1] = MkIntTerm(1);
      t = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("/"),2), 2, ti);
      Yap_Error(TYPE_ERROR_EVALUABLE, t,
	    "functor %s/%d for arithmetic expression",
	    RepAtom(name)->StrOfAE,1);
      P = (yamop *)FAILCODE;
      return(FALSE);
    }
    f = p->FOfEE.unary(Deref(ARG3),&res);
    return (Yap_unify_constant(ARG1,EvalToTerm(f,&res)));
  }
  return(FALSE);
}

void
Yap_InitUnaryExps(void)
{
  unsigned int    i;
  ExpEntry       *p;

  for (i = 0; i < sizeof(InitUnTab)/sizeof(InitUnEntry); ++i) {
    AtomEntry *ae = RepAtom(Yap_LookupAtom(InitUnTab[i].OpName));
    if (ae == NULL) {
      Yap_Error(OUT_OF_HEAP_ERROR,TermNil,"at InitUnaryExps");
      return;
    }
    WRITE_LOCK(ae->ARWLock);
    if (Yap_GetExpPropHavingLock(ae, 1)) {
      WRITE_UNLOCK(ae->ARWLock);
      break;
    }
    p = (ExpEntry *) Yap_AllocAtomSpace(sizeof(ExpEntry));
    p->KindOfPE = ExpProperty;
    p->ArityOfEE = 1;
    p->ENoOfEE = 1;
    p->FOfEE.unary = InitUnTab[i].f;
    p->NextOfPE = ae->PropsOfAE;
    ae->PropsOfAE = AbsExpProp(p);
    WRITE_UNLOCK(ae->ARWLock);
  }
  Yap_InitCPred("is", 3, p_unary_is, TestPredFlag | SafePredFlag);
}

/* This routine is called from Restore to make sure we have the same arithmetic operators */
int
Yap_ReInitUnaryExps(void)
{
  unsigned int i;
  Prop p;

  for (i = 0; i < sizeof(InitUnTab)/sizeof(InitUnEntry); ++i) {
    AtomEntry *ae = RepAtom(Yap_FullLookupAtom(InitUnTab[i].OpName));

    if (ae == NULL) {
      Yap_Error(OUT_OF_HEAP_ERROR,TermNil,"at ReInitUnaryExps");
      return FALSE;
    }
    WRITE_LOCK(ae->ARWLock);
    if ((p = Yap_GetExpPropHavingLock(ae, 1)) == NULL) {
      WRITE_UNLOCK(ae->ARWLock);
      return FALSE;
    }
    RepExpProp(p)->FOfEE.unary = InitUnTab[i].f;
    WRITE_UNLOCK(ae->ARWLock);
  }
  return TRUE;
}

