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
* File:		arith2.c						 *
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
#define USE_E_ARGS   , o

#define RINT(v)     (o)->Int = v; return(long_int_e)
#define RFLOAT(v)   (o)->dbl = v; return(double_e)
#define RBIG(v)     (o)->big = v; return(big_int_e)
#define RERROR()    return(db_ref_e)

#define ArithIEval(t,v)     Eval(t,v)

inline static Functor
AritFunctorOfTerm(Term t) {
  if (IsVarTerm(t)) {
    return(FunctorPortray);
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

inline static Term
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

typedef blob_type (*f_binexp)(Term, Term, arith_retptr);

typedef struct init_bin_eval {
  char          *OpName;
  f_binexp        f;
} InitBinEntry;

#include "arith2.h"

/*
  module mod
*/
static E_FUNC
p_mod(Term t1, Term t2 E_ARGS)
{
  Functor f1 = AritFunctorOfTerm(t1), f2;
  blob_type bt1, bt2;
  union arith_ret v1, v2;

  switch (BlobOfFunctor(f1)) {
  case (CELL)long_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case (CELL)long_int_e:
      /* two integers */
      {
	Int i2 = IntegerOfTerm(t2);
      
	if (i2 == 0) goto zero_divisor;
	RINT(IntegerOfTerm(t1) % i2);
      }
    case (CELL)double_e:
      Error(TYPE_ERROR_INTEGER, t2, "mod/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case (CELL)big_int_e:
      /* I know the term is much larger, so: */
      RINT(IntegerOfTerm(t1));
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = Eval(t2, &v2);
    }
    break;
  case (CELL)double_e:
    Error(TYPE_ERROR_INTEGER, t1, "mod/2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case (CELL)big_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      /* modulo between bignum and integer */
      {
	Int i2 = IntegerOfTerm(t2);
	MP_INT *l1 = BigIntOfTerm(t1);

	if (i2 > 0) {
	  MP_INT *new = PreAllocBigNum();
	  Int r = mpz_mod_ui(new, l1, i2);

	  CleanBigNum();
	  RINT((mpz_sgn(l1) ? r : -r));
	} else if (i2 == 0) {
	  goto zero_divisor;
	} else {
	  MP_INT *new = PreAllocBigNum();
	  Int r = mpz_mod_ui(new, l1, -i2);

	  CleanBigNum();
	  RINT((mpz_sgn(l1) ? r : -r));
	}
      }
    case (CELL)big_int_e:
      /* two bignums */
      {
	MP_INT *new = PreAllocBigNum();

	mpz_mod(new, BigIntOfTerm(t1), BigIntOfTerm(t2));
	RBIG(new);
      }
    case double_e:
      Error(TYPE_ERROR_INTEGER, t2, "mod/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    default:
      /* we've got a full term, need to evaluate it first */
      v1.big = BigIntOfTerm(t1);
      bt1 = big_int_e;
      bt2 = Eval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = Eval(t1, &v1);
    /* don't know anything about second */
    bt2 = Eval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      /* two integers */
      if (v2.Int == 0) goto zero_divisor;
      RINT(v1.Int % v2.Int);
    case double_e:
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "mod/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case (CELL)big_int_e:
      /* Cool */
      RINT(v1.Int);
#endif
    default:
      /* Error */
      RERROR();
    }
  case double_e:
    Error(TYPE_ERROR_INTEGER, MkFloatTerm(v1.dbl), "mod/2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case (CELL)big_int_e:
    switch (bt2) {
    case long_int_e:
      /* big mod integer */
      if (v2.Int > 0) {
	MP_INT *new = PreAllocBigNum();
	Int r = mpz_mod_ui(new, v1.big, v2.Int);

	CleanBigNum();
	RINT((mpz_sgn(v1.big) ? r : -r));
      } else if (v2.Int == 0) {
	goto zero_divisor;
      } else {
	MP_INT *new = PreAllocBigNum();
	Int r = mpz_mod_ui(new, v1.big, -v2.Int);

	CleanBigNum();
	RINT((mpz_sgn(v1.big) ? r : -r));
      }
    case double_e:
      /* big // float */
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "mod/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    case (CELL)big_int_e:
      /* big * big */
      {
	MP_INT *new = PreAllocBigNum();

	mpz_mod(new, v1.big, v2.big);
	RBIG(new);
      }
    default:
      /* error */
      RERROR();
    }
#endif
    default:
      /* error  */
      RERROR();
    }
 zero_divisor:
  Error(EVALUATION_ERROR_ZERO_DIVISOR, t2, "X is mod 0");
  /* make GCC happy */
  P = (yamop *)FAILCODE;
  RERROR();
}

#ifdef USE_GMP
static Float
fdiv_bigint(MP_INT *b1,MP_INT *b2)
{
  Float f1 = mpz_get_d(b1);
  Float f2 = mpz_get_d(b2);
  if (1) {
    mpf_t f1,f2;
    Float res;

    PreAllocBigNum();
    mpf_init(f1);
    mpf_init(f2);
    mpf_set_z(f1, b1);
    mpf_set_z(f2, b2);
    mpf_div(f1, f1, f2);
    res = mpf_get_d(f1);
    CleanBigNum();
    return(res);
  } else {
    return(f1/f2);
  }
}
#endif

/*
  Floating point division: /
*/
static E_FUNC
p_fdiv(Term t1, Term t2 E_ARGS)
{
  Functor f1 = AritFunctorOfTerm(t1), f2;
  blob_type bt1, bt2;
  union arith_ret v1, v2;

  switch (BlobOfFunctor(f1)) {
  case long_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	Int i2 = IntegerOfTerm(t2);

	/* two integers */
	RFLOAT((((Float)IntegerOfTerm(t1))/(Float)i2));
      }
    case double_e:
      {
	/* integer, double */
	Float fl1 = (Float)IntegerOfTerm(t1);
	Float fl2 = FloatOfTerm(t2);
	RFLOAT(fl1/fl2);
      }
#ifdef USE_GMP
    case (CELL)big_int_e:
      {
	Int i1 = IntegerOfTerm(t1);
	Float f2 = mpz_get_d(BigIntOfTerm(t2));
	RFLOAT(i1/f2);
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = Eval(t2, &v2);
    }
    break;
  case double_e:
    f2 = AritFunctorOfTerm(t2);

    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      /* float / integer */
      {
	Int i2 = IntegerOfTerm(t2);
	RFLOAT(FloatOfTerm(t1)/(Float)i2);
      }
    case double_e:
      {
	Float f2 = FloatOfTerm(t2);
	RFLOAT(FloatOfTerm(t1)/f2);
      }
#ifdef USE_GMP
    case big_int_e:
      {
	RFLOAT(FloatOfTerm(t1)/mpz_get_d(BigIntOfTerm(t2)));
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.dbl = FloatOfTerm(t1);
      bt1 = double_e;
      bt2 = Eval(t2, &v2);
    }
    break;
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	Int i = IntegerOfTerm(t2);
	RFLOAT(mpz_get_d(BigIntOfTerm(t1))/(Float)i);
      }
    case big_int_e:
      /* two bignums*/
      RFLOAT(fdiv_bigint(BigIntOfTerm(t1),BigIntOfTerm(t2)));
      //      RFLOAT(mpz_get_d(BigIntOfTerm(t1))/mpz_get_d(BigIntOfTerm(t2)));
    case double_e:
      {
	Float dbl = FloatOfTerm(t2);
	RFLOAT(mpz_get_d(BigIntOfTerm(t1))/dbl);
      }
    default:
      /* we've got a full term, need to evaluate it first */
      v1.big = BigIntOfTerm(t1);
      bt1 = big_int_e;
      bt2 = Eval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = Eval(t1, &v1);
    /* don't know anything about second */
    bt2 = Eval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      /* two integers */
      RFLOAT((Float)(((float)v1.Int)/(Float)(v2.Int)));
    case double_e:
      /* integer, double */
      RFLOAT(v1.Int/v2.dbl);
#ifdef USE_GMP
    case big_int_e:
      /* integer, double */
      RFLOAT(v1.Int/mpz_get_d(v2.big));
#endif
    default:
      /* Error */
      RERROR();
    }
  case double_e:
    switch (bt2) {
    case long_int_e:
      /* float / integer */
      RFLOAT(v1.dbl/v2.Int);
    case double_e:
      /* float / float */
      RFLOAT(v1.dbl/v2.dbl);
#ifdef USE_GMP
    case big_int_e:
      /* float / float */
      RFLOAT(v1.dbl/mpz_get_d(v2.big));
#endif
    default:
      /* error */
      RERROR();
    }
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      RFLOAT(mpz_get_d(v1.big)/v2.Int);
    case double_e:
      /* big / float */
      RFLOAT(mpz_get_d(v1.big)/v2.dbl);
    case big_int_e:
      /* big / big */
      RFLOAT(fdiv_bigint(v1.big,v2.big));
    default:
      /* error */
      RERROR();
    }
#endif
    default:
      /* error  */
      RERROR();
    }
}

#if USE_GMP
#if !HAVE_MPZ_XOR
static void
mpz_xor(MP_INT *new, MP_INT *r1, MP_INT *r2)
{
  MP_INT *n2 = PreAllocBigNum(), *n3 = PreAllocBigNum();
  
  mpz_ior(new, r1, r2);
  mpz_com(n2,  r1);
  mpz_and(n2, n2, new);
  mpz_com(n3,  r2);
  mpz_and(n3, n3, new);
  mpz_ior(new, n2, n3);
  CleanBigNum();
}
#endif
#endif

/*
  xor #
*/
static E_FUNC
p_xor(Term t1, Term t2 E_ARGS)
{
  Functor f1 = AritFunctorOfTerm(t1), f2;
  blob_type bt1, bt2;
  union arith_ret v1, v2;

  switch (BlobOfFunctor(f1)) {
  case long_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      /* two integers */
      RINT(IntegerOfTerm(t1) ^ IntegerOfTerm(t2));
    case double_e:
      Error(TYPE_ERROR_INTEGER, t2, "#/2");
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = PreAllocBigNum();

	mpz_set_si(new,IntOfTerm(t1));
	mpz_xor(new, new, BigIntOfTerm(t2));
	RBIG(new);
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = Eval(t2, &v2);
    }
    break;
  case double_e:
    Error(TYPE_ERROR_INTEGER, t1, "#/2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	MP_INT *new = PreAllocBigNum();

	mpz_set_si(new,IntOfTerm(t2));
	mpz_xor(new, BigIntOfTerm(t1), new);
	RBIG(new);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = PreAllocBigNum();

	mpz_xor(new, BigIntOfTerm(t1), BigIntOfTerm(t2));
	RBIG(new);
      }
    case double_e:
      Error(TYPE_ERROR_INTEGER, t2, "#/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    default:
      /* we've got a full term, need to evaluate it first */
      v1.big = BigIntOfTerm(t1);
      bt1 = big_int_e;
      bt2 = Eval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = Eval(t1, &v1);
    /* don't know anything about second */
    bt2 = Eval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      RINT(v1.Int ^ v2.Int);
    case double_e:
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "#/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = PreAllocBigNum();

	mpz_set_si(new,v1.Int);
	mpz_xor(new, new, v2.big);
	RBIG(new);
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  case double_e:
    Error(TYPE_ERROR_INTEGER, MkFloatTerm(v1.dbl), "#/2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      /* anding a bignum with an integer is easy */
      {
	MP_INT *new = PreAllocBigNum();

	mpz_set_si(new,v2.Int);
	mpz_xor(new, v1.big, new);
	RBIG(new);
      }
    case double_e:
      /* big // float */
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "\\/ /2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    case big_int_e:
      /* big * big */
      {
	MP_INT *new = PreAllocBigNum();

	mpz_xor(new, v1.big, v2.big);
	RBIG(new);
      }
    default:
      /* error */
      RERROR();
    }
#endif
    default:
      /* error  */
      RERROR();
    }
}

/*
  atan2: arc tangent x/y
*/
static E_FUNC
p_atan2(Term t1, Term t2 E_ARGS)
{
  Functor f1 = AritFunctorOfTerm(t1), f2;
  blob_type bt1, bt2;
  union arith_ret v1, v2;

  switch (BlobOfFunctor(f1)) {
  case long_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	Int i2 = IntegerOfTerm(t2);

	/* two integers */
	RFLOAT(atan2(IntegerOfTerm(t1),i2));
      }
    case double_e:
      {
	/* integer, double */
	Float fl1 = (Float)IntegerOfTerm(t1);
	Float fl2 = FloatOfTerm(t2);
	RFLOAT(atan2(fl1,fl2));
      }
#ifdef USE_GMP
    case big_int_e:
      {
	Int i1 = IntegerOfTerm(t1);
	Float f2 = mpz_get_d(BigIntOfTerm(t2));
	RFLOAT(atan2(i1,f2));
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = Eval(t2, &v2);
    }
    break;
  case double_e:
    f2 = AritFunctorOfTerm(t2);

    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      /* float / integer */
      {
	Int i2 = IntegerOfTerm(t2);
	RFLOAT(atan2(FloatOfTerm(t1),i2));
      }
    case double_e:
      {
	Float f2 = FloatOfTerm(t2);
	RFLOAT(atan2(FloatOfTerm(t1),f2));
      }
#ifdef USE_GMP
    case big_int_e:
      {
	RFLOAT(atan2(FloatOfTerm(t1),mpz_get_d(BigIntOfTerm(t2))));
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.dbl = FloatOfTerm(t1);
      bt1 = double_e;
      bt2 = Eval(t2, &v2);
    }
    break;
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	Int i = IntegerOfTerm(t2);
	RFLOAT(atan2(mpz_get_d(BigIntOfTerm(t1)),i));
      }
    case big_int_e:
      /* two bignums */
      RFLOAT(atan2(mpz_get_d(BigIntOfTerm(t1)),mpz_get_d(BigIntOfTerm(t2))));
    case double_e:
      {
	Float dbl = FloatOfTerm(t2);
	RFLOAT(atan2(mpz_get_d(BigIntOfTerm(t1)),dbl));
      }
    default:
      /* we've got a full term, need to evaluate it first */
      v1.big = BigIntOfTerm(t1);
      bt1 = big_int_e;
      bt2 = Eval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = Eval(t1, &v1);
    /* don't know anything about second */
    bt2 = Eval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      /* two integers */
      RFLOAT(atan2(v1.Int,v2.Int));
    case double_e:
      /* integer, double */
      RFLOAT(atan2(v1.Int,v2.dbl));
#ifdef USE_GMP
    case big_int_e:
      /* integer, double */
      RFLOAT(atan2(v1.Int,mpz_get_d(v2.big)));
#endif
    default:
      /* Error */
      RERROR();
    }
  case double_e:
    switch (bt2) {
    case long_int_e:
      /* float / integer */
      RFLOAT(atan2(v1.dbl,v2.Int));
    case double_e:
      /* float / float */
      RFLOAT(atan2(v1.dbl,v2.dbl));
#ifdef USE_GMP
    case big_int_e:
      /* float / float */
      RFLOAT(atan2(v1.dbl,mpz_get_d(v2.big)));
#endif
    default:
      /* error */
      RERROR();
    }
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      RFLOAT(atan2(mpz_get_d(v1.big),v2.Int));
    case double_e:
      /* big / float */
      RFLOAT(atan2(mpz_get_d(v1.big),v2.dbl));
    case big_int_e:
      /* big / big */
      RFLOAT(atan2(mpz_get_d(v1.big),mpz_get_d(v2.big)));
    default:
      /* error */
      RERROR();
    }
#endif
    default:
      /* error  */
      RERROR();
    }
}

/*
  power: x^y
*/
static E_FUNC
p_power(Term t1, Term t2 E_ARGS)
{
  Functor f1 = AritFunctorOfTerm(t1), f2;
  blob_type bt1, bt2;
  union arith_ret v1, v2;

  switch (BlobOfFunctor(f1)) {
  case long_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	Int i2 = IntegerOfTerm(t2);

	/* two integers */
	RFLOAT(pow(IntegerOfTerm(t1),i2));
      }
    case double_e:
      {
	/* integer, double */
	Float fl1 = (Float)IntegerOfTerm(t1);
	Float fl2 = FloatOfTerm(t2);
	RFLOAT(pow(fl1,fl2));
      }
#ifdef USE_GMP
    case big_int_e:
      {
	Int i1 = IntegerOfTerm(t1);
	Float f2 = mpz_get_d(BigIntOfTerm(t2));
	RFLOAT(pow(i1,f2));
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = Eval(t2, &v2);
    }
    break;
  case double_e:
    f2 = AritFunctorOfTerm(t2);

    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      /* float / integer */
      {
	Int i2 = IntegerOfTerm(t2);
	RFLOAT(pow(FloatOfTerm(t1),i2));
      }
    case double_e:
      {
	Float f2 = FloatOfTerm(t2);
	RFLOAT(pow(FloatOfTerm(t1),f2));
      }
#ifdef USE_GMP
    case big_int_e:
      {
	RFLOAT(pow(FloatOfTerm(t1),mpz_get_d(BigIntOfTerm(t2))));
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.dbl = FloatOfTerm(t1);
      bt1 = double_e;
      bt2 = Eval(t2, &v2);
    }
    break;
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	Int i = IntegerOfTerm(t2);
	RFLOAT(pow(mpz_get_d(BigIntOfTerm(t1)),i));
      }
    case big_int_e:
      /* two bignums */
      RFLOAT(pow(mpz_get_d(BigIntOfTerm(t1)),mpz_get_d(BigIntOfTerm(t2))));
    case double_e:
      {
	Float dbl = FloatOfTerm(t2);
	RFLOAT(pow(mpz_get_d(BigIntOfTerm(t1)),dbl));
      }
    default:
      /* we've got a full term, need to evaluate it first */
      v1.big = BigIntOfTerm(t1);
      bt1 = big_int_e;
      bt2 = Eval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = Eval(t1, &v1);
    /* don't know anything about second */
    bt2 = Eval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      /* two integers */
      RFLOAT(pow(v1.Int,v2.Int));
    case double_e:
      /* integer, double */
      RFLOAT(pow(v1.Int,v2.dbl));
#ifdef USE_GMP
    case big_int_e:
      /* integer, double */
      RFLOAT(pow(v1.Int,mpz_get_d(v2.big)));
#endif
    default:
      /* Error */
      RERROR();
    }
  case double_e:
    switch (bt2) {
    case long_int_e:
      /* float / integer */
      RFLOAT(pow(v1.dbl,v2.Int));
    case double_e:
      /* float / float */
      RFLOAT(pow(v1.dbl,v2.dbl));
#ifdef USE_GMP
    case big_int_e:
      /* float / float */
      RFLOAT(pow(v1.dbl,mpz_get_d(v2.big)));
#endif
    default:
      /* error */
      RERROR();
    }
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      RFLOAT(pow(mpz_get_d(v1.big),v2.Int));
    case double_e:
      /* big / float */
      RFLOAT(pow(mpz_get_d(v1.big),v2.dbl));
    case big_int_e:
      /* big / big */
      RFLOAT(pow(mpz_get_d(v1.big),mpz_get_d(v2.big)));
    default:
      /* error */
      RERROR();
    }
#endif
    default:
      /* error  */
      RERROR();
    }
}

static Int
gcd(Int m11,Int m21)
{
  /* Blankinship algorithm, provided by Miguel Filgueiras */
  Int m12=1, m22=0, k;

  while (m11>0 && m21>0)
    if (m11<m21) {
      k = m21/m11;  m21 -= k*m11;  m22 -= k*m12;
    } else {
      k=m11/m21;  m11 -= k*m21;  m12 -= k*m22;
    }
  if (m11<0 || m21<0) {		/* overflow? */
/*    Oflow = 1; */
    Error(EVALUATION_ERROR_INT_OVERFLOW, MkIntegerTerm(m11),
	  "gcd/2 with %d and %d", m11, m21);
    P = (yamop *)FAILCODE;
    return(1);
  }
  if (m11)  return(m11);
  return(m21);
}

#ifdef GCD_MULT
Int gcdmult(Int m11,Int m21,Int *pm11)	/* *pm11 gets multiplier of m11 */
{
  Int m12=1, m22=0, k;

  while (m11 && m21)
    if (m11<m21) {
      k = m21/m11;  m21 -= k*m11;  m22 -= k*m12;
    } else {
      k=m11/m21;  m11 -= k*m21;  m12 -= k*m22;
    }
  if (m11<0 || m21<0) {		/* overflow? */
/*    Oflow = 1; */
    Error(EVALUATION_ERROR_INT_OVERFLOW, MkIntegerTerm(m11),
	  "gcdmult/2 with %d and %d", m11, m21);
    P = (yamop *)FAILCODE;
    return(1);
  }
  if (m11) {
    *pm11 = m12;  return(m11);
  }
  *pm11 = m22;
  return(m21);
}
#endif

/*
  module gcd
*/
static E_FUNC
p_gcd(Term t1, Term t2 E_ARGS)
{
  Functor f1 = AritFunctorOfTerm(t1), f2;
  blob_type bt1, bt2;
  union arith_ret v1, v2;

  switch (BlobOfFunctor(f1)) {
  case long_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      /* two integers */
      {
	Int i1 = IntegerOfTerm(t1), i2 = IntegerOfTerm(t2);
	i1 = (i1 >= 0 ? i1 : -i1);
	i2 = (i2 >= 0 ? i2 : -i2);
      
	RINT(gcd(i1,i2));
      }
    case double_e:
      Error(TYPE_ERROR_INTEGER, t2, "gcd/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      /* I know the term is much larger, so: */
      {
	Int i = IntegerOfTerm(t1);

	if (i > 0) {
	  RINT(mpz_gcd_ui(NULL,BigIntOfTerm(t2),i));
	} else if (i == 0) {
	  MP_INT *new = PreAllocBigNum();
	
	  mpz_abs(new, BigIntOfTerm(t2));
	  RBIG(new);
	} else {
	  RINT(mpz_gcd_ui(NULL,BigIntOfTerm(t2),-i));
	}
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = Eval(t2, &v2);
    }
    break;
  case double_e:
    Error(TYPE_ERROR_INTEGER, t1, "gcd/2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      /* modulo between bignum and integer */
      {
	Int i = IntegerOfTerm(t2);

	if (i > 0) {
	  RINT(mpz_gcd_ui(NULL,BigIntOfTerm(t1),i));
	} else if (i == 0) {
	  MP_INT *new = PreAllocBigNum();
	
	  mpz_abs(new, BigIntOfTerm(t1));
	  RBIG(new);
	} else {
	  RINT(mpz_gcd_ui(NULL,BigIntOfTerm(t1),-i));
	}
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = PreAllocBigNum();

	mpz_gcd(new, BigIntOfTerm(t1), BigIntOfTerm(t2));
	RBIG(new);
      }
    case double_e:
      Error(TYPE_ERROR_INTEGER, t2, "gcd/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    default:
      /* we've got a full term, need to evaluate it first */
      v1.big = BigIntOfTerm(t1);
      bt1 = big_int_e;
      bt2 = Eval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = Eval(t1, &v1);
    /* don't know anything about second */
    bt2 = Eval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      /* two integers */
      {
	Int i1 = v1.Int, i2 = v2.Int;
	i1 = (i1 >= 0 ? i1 : -i1);
	i2 = (i2 >= 0 ? i2 : -i2);
      
	RINT(gcd(i1,i2));
      }
    case double_e:
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "gcd/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      {
	if (v1.Int > 0) {
	  RINT(mpz_gcd_ui(NULL,v2.big,v1.Int));
	} else if (v1.Int == 0) {
	  MP_INT *new = PreAllocBigNum();
	
	  mpz_abs(new, v2.big);
	  RBIG(new);
	} else {
	  RINT(mpz_gcd_ui(NULL,v2.big,-v1.Int));
	}
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  case double_e:
    Error(TYPE_ERROR_INTEGER, MkFloatTerm(v1.dbl), "gcd/2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      /* big gcd integer */
      {
	if (v2.Int > 0) {
	  RINT(mpz_gcd_ui(NULL,v1.big,v2.Int));
	} else if (v2.Int == 0) {
	  MP_INT *new = PreAllocBigNum();
	
	  mpz_abs(new, v1.big);
	  RBIG(new);
	} else {
	  RINT(mpz_gcd_ui(NULL,v1.big,-v2.Int));
	}
      }
    case double_e:
      /* big // float */
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "gcd/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    case big_int_e:
      if (v2.Int > 0) {
	MP_INT *new = PreAllocBigNum();
	mpz_gcd(new, v1.big, v2.big);
	RBIG(new);
      }
    default:
      /* error  */
      RERROR();
    }
#endif
  default:
    /* error  */
    RERROR();
  }
}

/*
  minimum: min(x,y)
*/
static E_FUNC
p_min(Term t1, Term t2 E_ARGS)
{
  Functor f1 = AritFunctorOfTerm(t1), f2;
  blob_type bt1, bt2;
  union arith_ret v1, v2;

  switch (BlobOfFunctor(f1)) {
  case long_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	Int i1 = IntegerOfTerm(t1);
	Int i2 = IntegerOfTerm(t2);
	RINT((i1 < i2 ? i1 : i2));
      }
    case double_e:
      {
	/* integer, double */
	Int i = IntegerOfTerm(t1);
	Float fl = FloatOfTerm(t2);
	if (i <= fl) {
	  RINT(i);
	}
	RFLOAT(fl);
      }
#ifdef USE_GMP
    case big_int_e:
      {
	Int i = IntegerOfTerm(t1);
	MP_INT *b = BigIntOfTerm(t2);

	if (mpz_cmp_si(b,i) < 0) {
	  RBIG(b);
	}
	RINT(i);
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = Eval(t2, &v2);
    }
    break;
  case double_e:
    f2 = AritFunctorOfTerm(t2);

    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      /* float / integer */
      {
	Int i = IntegerOfTerm(t2);
	Float fl = FloatOfTerm(t1);
	if (i <= fl) {
	  RINT(i);
	}
	RFLOAT(fl);
      }
    case double_e:
      {
	Float fl1 = FloatOfTerm(t1);
	Float fl2 = FloatOfTerm(t2);
	if (fl1 <= fl2) {
	  RFLOAT(fl1);
	}
	RFLOAT(fl2);
      }
#ifdef USE_GMP
    case big_int_e:
      {
	Float fl1 = FloatOfTerm(t1);
	Float fl2 = mpz_get_d(BigIntOfTerm(t2));
	if (fl1 <= fl2) {
	  RFLOAT(fl1);
	}
	RFLOAT(fl2);
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.dbl = FloatOfTerm(t1);
      bt1 = double_e;
      bt2 = Eval(t2, &v2);
    }
    break;
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	Int i = IntegerOfTerm(t2);
	MP_INT *b = BigIntOfTerm(t1);

	if (mpz_cmp_si(b,i) < 0) {
	  RBIG(b);
	}
	RINT(i);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *b1 = BigIntOfTerm(t1);
	MP_INT *b2 = BigIntOfTerm(t2);

	if (mpz_cmp(b1,b2) < 0) {
	  RBIG(b1);
	}
	RBIG(b2);
      }
    case double_e:
      {
	Float fl1 = FloatOfTerm(t2);
	Float fl2 = mpz_get_d(BigIntOfTerm(t1));
	if (fl1 <= fl2) {
	  RFLOAT(fl1);
	}
	RFLOAT(fl2);
      }
    default:
      /* we've got a full term, need to evaluate it first */
      v1.big = BigIntOfTerm(t1);
      bt1 = big_int_e;
      bt2 = Eval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = Eval(t1, &v1);
    /* don't know anything about second */
    bt2 = Eval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      /* two integers */
      RINT((v1.Int < v2.Int ? v1.Int : v2.Int));
    case double_e:
      /* integer, double */
      {
	if (v1.Int <= v2.dbl) {
	  RINT(v1.Int);
	}
	RFLOAT(v2.dbl);
      }
#ifdef USE_GMP
    case big_int_e:
      /* integer, double */
      {
	if (mpz_cmp_si(v2.big,v1.Int) < 0) {
	  RBIG(v2.big);
	}
	RINT(v1.Int);
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  case double_e:
    switch (bt2) {
    case long_int_e:
      /* float / integer */
      {
	if (v2.Int <= v1.dbl) {
	  RINT(v2.Int);
	}
	RFLOAT(v1.dbl);
      }
    case double_e:
      /* float / float */
      {
	if (v2.dbl <= v1.dbl) {
	  RFLOAT(v2.dbl);
	}
	RFLOAT(v1.dbl);
      }
#ifdef USE_GMP
    case big_int_e:
      /* float / big */
      {
	if (mpz_get_d(v2.big) <= v1.dbl) {
	  RBIG(v2.big);
	}
	RFLOAT(v1.dbl);
      }
#endif
    default:
      /* error */
      RERROR();
    }
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      /* integer, double */
      {
	if (mpz_cmp_si(v1.big,v2.Int) < 0) {
	  RBIG(v1.big);
	}
	RINT(v2.Int);
      }
    case double_e:
      /* big / float */
      {
	if (mpz_get_d(v1.big) <= v2.dbl) {
	  RBIG(v1.big);
	}
	RFLOAT(v2.dbl);
      }
    case big_int_e:
      /* big / big */
      {
	if (mpz_cmp(v1.big,v2.big) < 0) {
	  RBIG(v1.big);
	}
	RBIG(v2.big);
      }
    default:
      /* error */
      RERROR();
    }
#endif
    default:
      /* error  */
      RERROR();
    }
}

/*
  maximum: max(x,y)
*/
static E_FUNC
p_max(Term t1, Term t2 E_ARGS)
{
  Functor f1 = AritFunctorOfTerm(t1), f2;
  blob_type bt1, bt2;
  union arith_ret v1, v2;

  switch (BlobOfFunctor(f1)) {
  case long_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	Int i1 = IntegerOfTerm(t1);
	Int i2 = IntegerOfTerm(t2);
	RINT((i1 > i2 ? i1 : i2));
      }
    case double_e:
      {
	/* integer, double */
	Int i = IntegerOfTerm(t1);
	Float fl = FloatOfTerm(t2);
	if (i >= fl) {
	  RINT(i);
	}
	RFLOAT(fl);
      }
#ifdef USE_GMP
    case big_int_e:
      {
	Int i = IntegerOfTerm(t1);
	MP_INT *b = BigIntOfTerm(t2);

	if (mpz_cmp_si(b,i) > 0) {
	  RBIG(b);
	}
	RINT(i);
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = Eval(t2, &v2);
    }
    break;
  case double_e:
    f2 = AritFunctorOfTerm(t2);

    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      /* float / integer */
      {
	Int i = IntegerOfTerm(t2);
	Float fl = FloatOfTerm(t1);
	if (i >= fl) {
	  RINT(i);
	}
	RFLOAT(fl);
      }
    case double_e:
      {
	Float fl1 = FloatOfTerm(t1);
	Float fl2 = FloatOfTerm(t2);
	if (fl1 >= fl2) {
	  RFLOAT(fl1);
	}
	RFLOAT(fl2);
      }
#ifdef USE_GMP
    case big_int_e:
      {
	Float fl1 = FloatOfTerm(t1);
	Float fl2 = mpz_get_d(BigIntOfTerm(t2));
	if (fl1 >= fl2) {
	  RFLOAT(fl1);
	}
	RFLOAT(fl2);
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.dbl = FloatOfTerm(t1);
      bt1 = double_e;
      bt2 = Eval(t2, &v2);
    }
    break;
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	Int i = IntegerOfTerm(t2);
	MP_INT *b = BigIntOfTerm(t1);

	if (mpz_cmp_si(b,i) > 0) {
	  RBIG(b);
	}
	RINT(i);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *b1 = BigIntOfTerm(t1);
	MP_INT *b2 = BigIntOfTerm(t2);

	if (mpz_cmp(b1,b2) > 0) {
	  RBIG(b1);
	}
	RBIG(b2);
      }
    case double_e:
      {
	Float fl1 = FloatOfTerm(t2);
	Float fl2 = mpz_get_d(BigIntOfTerm(t1));
	if (fl1 >= fl2) {
	  RFLOAT(fl1);
	}
	RFLOAT(fl2);
      }
    default:
      /* we've got a full term, need to evaluate it first */
      v1.big = BigIntOfTerm(t1);
      bt1 = big_int_e;
      bt2 = Eval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = Eval(t1, &v1);
    /* don't know anything about second */
    bt2 = Eval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      /* two integers */
      RINT((v1.Int > v2.Int ? v1.Int : v2.Int));
    case double_e:
      /* integer, double */
      {
	if (v1.Int >= v2.dbl) {
	  RINT(v1.Int);
	}
	RFLOAT(v2.dbl);
      }
#ifdef USE_GMP
    case big_int_e:
      /* integer, double */
      {
	if (mpz_cmp_si(v2.big,v1.Int) > 0) {
	  RBIG(v2.big);
	}
	RINT(v1.Int);
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  case double_e:
    switch (bt2) {
    case long_int_e:
      /* float / integer */
      {
	if (v2.Int >= v1.dbl) {
	  RINT(v2.Int);
	}
	RFLOAT(v1.dbl);
      }
    case double_e:
      /* float / float */
      {
	if (v2.dbl >= v1.dbl) {
	  RFLOAT(v2.dbl);
	}
	RFLOAT(v1.dbl);
      }
#ifdef USE_GMP
    case big_int_e:
      /* float / big */
      {
	if (mpz_get_d(v2.big) >= v1.dbl) {
	  RBIG(v2.big);
	}
	RFLOAT(v1.dbl);
      }
#endif
    default:
      /* error */
      RERROR();
    }
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      /* integer, double */
      {
	if (mpz_cmp_si(v1.big,v2.Int) > 0) {
	  RBIG(v1.big);
	}
	RINT(v2.Int);
      }
    case double_e:
      /* big / float */
      {
	if (mpz_get_d(v1.big) >= v2.dbl) {
	  RBIG(v1.big);
	}
	RFLOAT(v2.dbl);
      }
    case big_int_e:
      /* big / big */
      {
	if (mpz_cmp(v1.big,v2.big) > 0) {
	  RBIG(v1.big);
	}
	RBIG(v2.big);
      }
    default:
      /* error */
      RERROR();
    }
#endif
    default:
      /* error  */
      RERROR();
    }
}

static InitBinEntry InitBinTab[] = {
  {"+", p_plus},
  {"-", p_minus},
  {"*", p_times},
  {"/", p_fdiv},
  {"mod", p_mod},
  {"rem", p_mod},
  {"//", p_div},
  {"<<", p_sll},
  {">>", p_slr},
  {"/\\", p_and},
  {"\\/", p_or},
  {"#", p_xor},
  {"atan2", p_atan2},
  /* C-Prolog exponentiation */
  {"^", p_power},
  /* ISO-Prolog exponentiation */
  {"**", p_power},
  /* Quintus exponentiation */
  {"exp", p_power},
  {"gcd", p_gcd},
  {"min", p_min},
  {"max", p_max},
  {"atan2", p_atan2}
};

static Int 
p_binary_is(void)
{				/* X is Y	 */
  Term t = Deref(ARG2);
  union arith_ret res;
  blob_type f;

  if (IsVarTerm(t)) {
    Error(INSTANTIATION_ERROR,t, "X is Y");
    return(FALSE);
  }
  if (IsIntTerm(t)) {
    blob_type f = InitBinTab[IntOfTerm(t)].f(Deref(ARG3),Deref(ARG4),&res);
    return (unify_constant(ARG1,EvalToTerm(f,&res)));
  }
  if (IsAtomTerm(t)) {
    Atom name = AtomOfTerm(t);
    ExpEntry *p;

    if (EndOfPAEntr(p = RepExpProp(GetExpProp(name, 2)))) {
      Term ti[2];

      /* error */
      ti[0] = t;
      ti[1] = MkIntTerm(2);
      t = MkApplTerm(MkFunctor(LookupAtom("/"),2), 2, ti);
      Error(TYPE_ERROR_EVALUABLE, t,
	    "functor %s/%d for arithmetic expression",
	    RepAtom(name)->StrOfAE,2);
      P = (yamop *)FAILCODE;
      return(FALSE);
    }
    f = p->FOfEE.binary(Deref(ARG3),Deref(ARG4),&res);
    return (unify_constant(ARG1,EvalToTerm(f,&res)));
  }
  return(FALSE);
}

void
InitBinaryExps(void)
{
  unsigned int    i;
  ExpEntry       *p;

  for (i = 0; i < sizeof(InitBinTab)/sizeof(InitBinEntry); ++i) {
    AtomEntry *ae = RepAtom(LookupAtom(InitBinTab[i].OpName));
    WRITE_LOCK(ae->ARWLock);
    if (LockedGetExpProp(ae, 2)) {
      WRITE_UNLOCK(ae->ARWLock);
      break;
    }
    p = (ExpEntry *) AllocAtomSpace(sizeof(ExpEntry));
    p->KindOfPE = ExpProperty;
    p->ArityOfEE = 2;
    p->ENoOfEE = 2;
    p->FOfEE.binary = InitBinTab[i].f;
    p->NextOfPE = ae->PropOfAE;
    ae->PropOfAE = AbsExpProp(p);
    WRITE_UNLOCK(ae->ARWLock);
  }
  InitCPred("is", 4, p_binary_is, TestPredFlag | SafePredFlag);
}

/* This routine is called from Restore to make sure we have the same arithmetic operators */
int
ReInitBinaryExps(void)
{
  unsigned int i;
  Prop p;

  for (i = 0; i < sizeof(InitBinTab)/sizeof(InitBinEntry); ++i) {
    AtomEntry *ae = RepAtom(FullLookupAtom(InitBinTab[i].OpName));

    WRITE_LOCK(ae->ARWLock);
    if ((p = LockedGetExpProp(ae, 2)) == NULL) {
      WRITE_UNLOCK(ae->ARWLock);
      return(FALSE);
    }
    RepExpProp(p)->FOfEE.binary = InitBinTab[i].f;
    WRITE_UNLOCK(ae->ARWLock);
  }
  return(TRUE);
}

