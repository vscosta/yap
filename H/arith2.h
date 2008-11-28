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
* File:		arithi2.c						 *
* Last rev:								 *
* mods:									 *
* comments:	arithmetical expression evaluation			 *
*									 *
*************************************************************************/

/* This file implements fast binary math operations for YAP
 *
 */

inline static E_FUNC
add_int(Int i, Int j E_ARGS)
{
  Int x = i+j;
#if USE_GMP
  /* Integer overflow, we need to use big integers */
  Int overflow = (i & j & ~x) | (~i & ~j & x);
  if (overflow) {
    MP_INT *new = TMP_BIG();
    new = Yap_gmp_add_ints(i, j, new);
    RBIG(new);
  }
#endif
#ifdef BEAM
  RINT(x);
  return( MkIntegerTerm (x));
#else
  RINT(x);
#endif
}

inline static E_FUNC
sub_int(Int i, Int j E_ARGS)
{
  Int x = i-j;
#if USE_GMP
  Int overflow = (i & ~j & ~x) | (~i & j & x);
  /* Integer overflow, we need to use big integers */
  if (overflow) {
    MP_INT *new = TMP_BIG();
    new = Yap_gmp_sub_ints(i, j, new);
    RBIG(new);
  }
#endif
#ifdef BEAM
  RINT(x);
  return( MkIntegerTerm (x));
#else
  RINT(x);
#endif
}

/* Extended version with two possibilities:
   - both terms do not need evaluation;
   - a term needs evaluation;
*/
static E_FUNC
p_plus(Term t1, Term t2 E_ARGS)
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
      return add_int(IntegerOfTerm(t1),IntegerOfTerm(t2) USE_E_ARGS);
    case double_e:
      {
	/* integer, double */
	Float fl1 = (Float)IntegerOfTerm(t1);
	Float fl2 = FloatOfTerm(t2);
	RFLOAT(fl1+fl2);
      }
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_add_int_big(IntegerOfTerm(t1), Yap_BigIntOfTerm(t2), new);
	RBIG(new);
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = ArithIEval(t2, &v2);
    }
    break;
  case double_e:
    f2 = AritFunctorOfTerm(t2);

    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      /* float * integer */
      RFLOAT(FloatOfTerm(t1)+IntegerOfTerm(t2));
    case double_e:
      RFLOAT(FloatOfTerm(t1)+FloatOfTerm(t2));
#ifdef USE_GMP
    case big_int_e:
      RFLOAT(Yap_gmp_add_float_big(FloatOfTerm(t1),Yap_BigIntOfTerm(t2)));
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.dbl = FloatOfTerm(t1);
      bt1 = double_e;
      bt2 = ArithIEval(t2, &v2);
    }
    break;
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_add_int_big(IntegerOfTerm(t2), Yap_BigIntOfTerm(t1), new);
	RBIG(new);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_add_big_big(Yap_BigIntOfTerm(t1), Yap_BigIntOfTerm(t2), new);
	RBIG(new);
      }
    case double_e:
      RFLOAT(Yap_gmp_add_float_big(FloatOfTerm(t2),Yap_BigIntOfTerm(t1)));
    default:
      /* we've got a full term, need to evaluate it first */
      mpz_init_set(v1.big, Yap_BigIntOfTerm(t1));
      bt1 = big_int_e;
      bt2 = ArithIEval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = ArithIEval(t1, &v1);
    /* don't know anything about second */
    bt2 = ArithIEval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      /* two integers */
      return add_int(v1.Int,v2.Int USE_E_ARGS);
    case double_e:
      /* integer, double */
      RFLOAT(v1.Int+v2.dbl);
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_add_int_big(v1.Int, v2.big, new);
	RBIG(new);
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  case double_e:
    switch (bt2) {
    case long_int_e:
      /* float * integer */
      RFLOAT(v1.dbl+v2.Int);
    case double_e:
      /* float * float */
      RFLOAT(v1.dbl+v2.dbl);
#ifdef USE_GMP
    case big_int_e:
      /* float * float */
      RFLOAT(Yap_gmp_add_float_big(v1.dbl, v2.big));
      mpz_clear(v2.big);
#endif
    default:
      /* error */
      RERROR();
    }
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_add_int_big(v2.Int, v1.big, new);
	RBIG(new);
      }
    case double_e:
      /* big * float */
      {
	Float dbl = Yap_gmp_add_float_big(v2.dbl, v1.big);
	mpz_clear(v1.big);
	RFLOAT(dbl);
      }
    case big_int_e:
      /* big * big */
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_add_big_big(v1.big, v2.big, new);
	mpz_clear(v2.big);
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

/* Extended version with two possibilities:
   - both terms do not need evaluation;
   - a term needs evaluation;
*/
static E_FUNC
p_minus(Term t1, Term t2 E_ARGS)
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
      return sub_int(IntegerOfTerm(t1), IntegerOfTerm(t2) USE_E_ARGS);
    case double_e:
      {
	/* integer, double */
	Float fl1 = (Float)IntegerOfTerm(t1);
	Float fl2 = FloatOfTerm(t2);
	RFLOAT(fl1-fl2);
      }
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_sub_int_big(IntegerOfTerm(t1), Yap_BigIntOfTerm(t2), new);
	RBIG(new);
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = ArithIEval(t2, &v2);
    }
    break;
  case double_e:
    f2 = AritFunctorOfTerm(t2);

    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      /* float * integer */
      RFLOAT(FloatOfTerm(t1)-IntegerOfTerm(t2));
    case double_e:
      {
	RFLOAT(FloatOfTerm(t1)-FloatOfTerm(t2));
      }
#ifdef USE_GMP
    case big_int_e:
      {
	RFLOAT(Yap_gmp_sub_float_big(FloatOfTerm(t1),Yap_BigIntOfTerm(t2)));
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.dbl = FloatOfTerm(t1);
      bt1 = double_e;
      bt2 = ArithIEval(t2, &v2);
    }
    break;
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_sub_big_int(Yap_BigIntOfTerm(t1), IntegerOfTerm(t2), new);
	RBIG(new);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_sub_big_big(Yap_BigIntOfTerm(t1), Yap_BigIntOfTerm(t2), new);
	RBIG(new);
      }
    case double_e:
      {
	RFLOAT(Yap_gmp_sub_big_float(Yap_BigIntOfTerm(t1),FloatOfTerm(t2)));
      }
    default:
      /* we've got a full term, need to evaluate it first */
      mpz_init_set(v1.big,Yap_BigIntOfTerm(t1));
      bt1 = big_int_e;
      bt2 = ArithIEval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = ArithIEval(t1, &v1);
    /* don't know anything about second */
    bt2 = ArithIEval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      /* two integers */
      return sub_int(v1.Int, v2.Int USE_E_ARGS);
    case double_e:
      {
	/* integer, double */
	RFLOAT(v1.Int-v2.dbl);
      }
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_sub_int_big(v1.Int, v2.big, new);
	RBIG(new);
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  case double_e:
    switch (bt2) {
    case long_int_e:
      /* float * integer */
      RFLOAT(v1.dbl-v2.Int);
    case double_e:
      /* float * float */
      RFLOAT(v1.dbl-v2.dbl);
#ifdef USE_GMP
    case big_int_e:
      /* float * float */
      {
	Float flt = Yap_gmp_sub_float_big(v1.dbl,v2.big);

	mpz_clear(v2.big);
	RFLOAT(flt);
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
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_sub_big_int(v1.big, v2.Int, new);
	RBIG(new);
      }
    case double_e:
      /* big * float */
      {
	Float flt = Yap_gmp_sub_big_float(v1.big,v2.dbl);
	
	mpz_clear(v1.big);
	RFLOAT(flt);
      }
    case big_int_e:
      /* big * big */
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_sub_big_big(v1.big, v2.big, new);
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

#ifdef __GNUC__
#ifdef __i386__
#define DO_MULTI() { Int tmp1; \
  __asm__ ("imull  %3\n\t movl   $0,%1\n\t jno    0f\n\t movl   $1,%1\n\t 0:"    \
	   : "=a" (z),	    \
	     "=d" (tmp1)    \
	   : "a" (i1),      \
             "rm" (i2)      \
           : "cc" );   \
           if (tmp1) goto overflow; \
          }
#define OPTIMIZE_MULTIPLI 1
#endif
#endif

#ifndef OPTIMIZE_MULTIPLI
#define DO_MULTI() z = i1*i2; \
                          if (i2 &&  z/i2 != i1) goto overflow
#endif

inline static E_FUNC
times_int(Int i1, Int i2 E_ARGS) {
#ifdef USE_GMP
  Int z;
  DO_MULTI();
  RINT(z);
 overflow:
  {
    MP_INT *new = TMP_BIG();

    new = Yap_gmp_mul_ints(i1, i2, new);
    RBIG(new);
  }
#else
  RINT(i1*i2);
#endif
}

/* Extended version with two possibilities:
   - both terms do not need evaluation;
   - a term needs evaluation;
*/
static E_FUNC
p_times(Term t1, Term t2 E_ARGS)
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
      return(times_int(IntegerOfTerm(t1),IntegerOfTerm(t2) USE_E_ARGS));
    case double_e:
      {
	/* integer, double */
	Float fl1 = (Float)IntegerOfTerm(t1);
	Float fl2 = FloatOfTerm(t2);
	RFLOAT(fl1*fl2);
      }
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_mul_int_big(IntegerOfTerm(t1), Yap_BigIntOfTerm(t2), new);
	RBIG(new);
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = ArithIEval(t2, &v2);
    }
    break;
  case double_e:
    f2 = AritFunctorOfTerm(t2);

    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      /* float * integer */
      RFLOAT(FloatOfTerm(t1)*IntegerOfTerm(t2));
    case double_e:
      {
	RFLOAT(FloatOfTerm(t1)*FloatOfTerm(t2));
      }
#ifdef USE_GMP
    case big_int_e:
      {
	RFLOAT(Yap_gmp_mul_float_big(FloatOfTerm(t1),Yap_BigIntOfTerm(t2)));
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.dbl = FloatOfTerm(t1);
      bt1 = double_e;
      bt2 = ArithIEval(t2, &v2);
    }
    break;
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_mul_int_big(IntegerOfTerm(t2), Yap_BigIntOfTerm(t1), new);
	RBIG(new);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_mul_big_big(Yap_BigIntOfTerm(t1), Yap_BigIntOfTerm(t2), new);
	RBIG(new);
      }
    case double_e:
      {
	RFLOAT(Yap_gmp_mul_float_big(FloatOfTerm(t2),Yap_BigIntOfTerm(t1)));
      }
    default:
      /* We've got a full term, need to evaluate it first */
      mpz_init_set(v1.big,Yap_BigIntOfTerm(t1));
      bt1 = big_int_e;
      bt2 = ArithIEval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = ArithIEval(t1, &v1);
    /* don't know anything about second */
    bt2 = ArithIEval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      /* two integers */
      return(times_int(v1.Int,v2.Int USE_E_ARGS));
    case double_e:
      {
	/* integer, double */
	RFLOAT(v1.Int*v2.dbl);
      }
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_mul_int_big(v1.Int, v2.big, new);
	RBIG(new);
      }
#endif
    default:
      /* Error */
      RERROR();
    }
  case double_e:
    switch (bt2) {
    case long_int_e:
      /* float * integer */
      RFLOAT(v1.dbl*v2.Int);
    case double_e:
      /* float * float */
      RFLOAT(v1.dbl*v2.dbl);
#ifdef USE_GMP
    case big_int_e:
      /* float * float */
      {
	Float flt = Yap_gmp_mul_float_big(v1.dbl, v2.big);

	mpz_clear(v2.big);
	RFLOAT(flt);
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
      /* big * integer */
      {
	MP_INT *new = TMP_BIG();
	new = Yap_gmp_mul_int_big(v2.Int, v1.big, new);
	RBIG(new);
      }
    case double_e:
      /* big * float */
      {
	Float dbl = Yap_gmp_mul_float_big(v2.dbl, v1.big);
	mpz_clear(v1.big);
	RFLOAT(dbl);
      }
    case big_int_e:
      /* big * big */
      {
	MP_INT *new = TMP_BIG();
	new = Yap_gmp_mul_big_big(v1.big, v2.big, new);
	mpz_clear(v2.big);
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
  Integer division //
*/
static E_FUNC
p_div(Term t1, Term t2 E_ARGS)
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
	Int i2 = IntegerOfTerm(t2);
      
	if (i2 == 0) {
	  Yap_Error(EVALUATION_ERROR_ZERO_DIVISOR, t2, "// /2");
	  /* make GCC happy */
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
	RINT(IntegerOfTerm(t1) / i2);
      }
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t2, "// /2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      /* Cool */
      RINT(0);
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = ArithIEval(t2, &v2);
    }
    break;
  case double_e:
    Yap_Error(TYPE_ERROR_INTEGER, t1, "// /2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      /* dividing a bignum by an integer */
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_div_big_int(Yap_BigIntOfTerm(t1), IntegerOfTerm(t2), new);
	if (!new) {
	  /* make GCC happy */
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
	RBIG(new);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_div_big_big(Yap_BigIntOfTerm(t1), Yap_BigIntOfTerm(t2), new);
	RBIG(new);
      }
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t2, "// /2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    default:
      /* we've got a full term, need to evaluate it first */
      mpz_init_set(v1.big, Yap_BigIntOfTerm(t1));
      bt1 = big_int_e;
      bt2 = ArithIEval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = ArithIEval(t1, &v1);
    /* don't know anything about second */
    bt2 = ArithIEval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      /* two integers */
      if (v2.Int == 0) {
	Yap_Error(EVALUATION_ERROR_ZERO_DIVISOR, t2, "// /2");
	/* make GCC happy */
	P = (yamop *)FAILCODE;
	RERROR();
      }
      RINT(v1.Int / v2.Int);
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "// /2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      /* Cool */
      RINT(0);
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  case double_e:
    Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v1.dbl), "// /2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      /* big // integer */
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_div_big_int(v1.big, v2.Int, new);
	if (!new) {
	  /* make GCC happy */
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
	RBIG(new);
      }
    case double_e:
      /* big // float */
      Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "// /2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    case big_int_e:
      /* big * big */
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_div_big_big(v1.big, v2.big, new);
	mpz_clear(v2.big);
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
  and /\\
*/
static E_FUNC
p_and(Term t1, Term t2 E_ARGS)
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
      RINT(IntegerOfTerm(t1) & IntegerOfTerm(t2));
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t2, "/\\ /2");
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = TMP_BIG();
	new = Yap_gmp_and_int_big(IntegerOfTerm(t1),Yap_BigIntOfTerm(t2), new);
	RBIG(new);
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = ArithIEval(t2, &v2);
    }
    break;
  case double_e:
    Yap_Error(TYPE_ERROR_INTEGER, t1, "/\\ /2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      /* anding a bignum with an integer is easy */
      {
	MP_INT *new = TMP_BIG();
	new = Yap_gmp_and_int_big(IntegerOfTerm(t2),Yap_BigIntOfTerm(t1), new);
	RBIG(new);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_and_big_big(Yap_BigIntOfTerm(t2), Yap_BigIntOfTerm(t1), new);
	RBIG(new);
      }
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t2, "/\\ /2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    default:
      /* we've got a full term, need to evaluate it first */
      mpz_init_set(v1.big,Yap_BigIntOfTerm(t1));
      bt1 = big_int_e;
      bt2 = ArithIEval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = ArithIEval(t1, &v1);
    /* don't know anything about second */
    bt2 = ArithIEval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      RINT(v1.Int & v2.Int);
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "/\\ /2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = TMP_BIG();
	new = Yap_gmp_and_int_big(v1.Int, v2.big, new);
	mpz_clear(v2.big);
	RBIG(new);
      }
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  case double_e:
#ifdef USE_GMP
    if (bt2 == big_int_e) {
      mpz_clear(v2.big);
    }
#endif
    Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v1.dbl), "/\\ /2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      /* anding a bignum with an integer is easy */
      {
	MP_INT *new = TMP_BIG();
	new = Yap_gmp_and_int_big(v2.Int, v1.big, new);
	mpz_clear(v1.big);
	RBIG(new);
      }
    case double_e:
      /* big // float */
      Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "/\\ /2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    case big_int_e:
      /* big * big */
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_and_big_big(v1.big, v2.big, new);
	mpz_clear(v2.big);
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
  or \/
*/
static E_FUNC
p_or(Term t1, Term t2 E_ARGS)
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
      RINT(IntegerOfTerm(t1) | IntegerOfTerm(t2));
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t2, "\\/ /2");
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = TMP_BIG();

	Yap_gmp_ior_int_big(IntegerOfTerm(t1), Yap_BigIntOfTerm(t2), new);
	RBIG(new);
      }
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = ArithIEval(t2, &v2);
    }
    break;
  case double_e:
    Yap_Error(TYPE_ERROR_INTEGER, t1, "\\/ /2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);
    
    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	MP_INT *new = TMP_BIG();

	Yap_gmp_ior_int_big(IntegerOfTerm(t2), Yap_BigIntOfTerm(t1), new);
	RBIG(new);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = TMP_BIG();

	Yap_gmp_ior_big_big(Yap_BigIntOfTerm(t1), Yap_BigIntOfTerm(t2), new);
	RBIG(new);
      }
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t2, "\\/ /2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    default:
      /* we've got a full term, need to evaluate it first */
      mpz_init_set(v1.big,Yap_BigIntOfTerm(t1));
      bt1 = big_int_e;
      bt2 = ArithIEval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = ArithIEval(t1, &v1);
    /* don't know anything about second */
    bt2 = ArithIEval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      RINT(v1.Int | v2.Int);
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "\\/ /2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = TMP_BIG();

	Yap_gmp_ior_int_big(v1.Int, v2.big, new);
	mpz_clear(v2.big);
	RBIG(new);
      }
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  case double_e:
    Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v1.dbl), "\\/ /2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      /* anding a bignum with an integer is easy */
      {
	MP_INT *new = TMP_BIG();

	Yap_gmp_ior_int_big(v2.Int, v1.big, new);
	mpz_clear(v1.big);
	RBIG(new);
      }
    case double_e:
      /* big // float */
      Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "\\/ /2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    case big_int_e:
      /* big * big */
      {
	MP_INT *new = TMP_BIG();

	Yap_gmp_ior_big_big(v1.big, v2.big, new);
	mpz_clear(v2.big);
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

#if USE_GMP
static inline Int
sll_ovflw(Int x,Int i)
{
  CELL t = (1<<x)-1;
  return (t & i) != i;
}
#endif

inline static E_FUNC
do_sll(Int i, Int j E_ARGS)
{
#if USE_GMP
  Int x = (8*sizeof(CELL)-2)-j;
  
  if (x < 0||
      sll_ovflw(x,i)) {
    MP_INT *new = TMP_BIG();

    new = Yap_gmp_sll_ints(i, j, new);
    RBIG(new);
  }
#endif
  RINT(i << j);
}

/*
  sll <<

  First argument may be int or bignum;
  Second argument may only be an int.
*/
static E_FUNC
p_sll(Term t1, Term t2 E_ARGS)
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
      if (IntegerOfTerm(t2) < 0) {
	Int i2 = IntegerOfTerm(t2);
	if (i2 == Int_MIN) {
	  Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, ">>/2");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
	RINT(IntegerOfTerm(t1) >> -i2);
      }
      return do_sll(IntegerOfTerm(t1),IntegerOfTerm(t2) USE_E_ARGS);
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t2, "<</2");
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, "<</2");
      P = (yamop *)FAILCODE;
      RERROR();
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = ArithIEval(t2, &v2);
    }
    break;
  case double_e:
    Yap_Error(TYPE_ERROR_INTEGER, t1, "<< /2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);

    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	MP_INT *new = TMP_BIG();
	new = Yap_gmp_sll_big_int(Yap_BigIntOfTerm(t1),  IntegerOfTerm(t2), new);
	if (!new) {
	  Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, ">>/2");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
	RBIG(new);
      }
    case big_int_e:
      Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, "<</2");
      P = (yamop *)FAILCODE;
      RERROR();
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t2, "<</2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    default:
      /* we've got a full term, need to evaluate it first */
      mpz_init_set(v1.big,Yap_BigIntOfTerm(t1));
      bt1 = big_int_e;
      bt2 = ArithIEval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = ArithIEval(t1, &v1);
    /* don't know anything about second */
    bt2 = ArithIEval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      if (v2.Int < 0) {
	if (v2.Int == Int_MIN) {
	  Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, MkIntegerTerm(v2.Int), ">>/2");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
	RINT(v1.Int >> -v2.Int);
      }
      return do_sll(v1.Int,v2.Int USE_E_ARGS);
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "<</2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      mpz_clear(v2.big);
      Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, "<</2");
      P = (yamop *)FAILCODE;
      RERROR();
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  case double_e:
#ifdef USE_GMP
    if (bt2 == big_int_e)
      mpz_clear(v2.big);
#endif
    Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v1.dbl), "<</2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      /* big << int */
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_sll_big_int(v1.big,  v2.Int, new);
	if (!new) {
	  Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, MkIntegerTerm(v2.Int), ">>/2");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
	RBIG(new);
      }
    case double_e:
      /* big << float */
      mpz_clear(v1.big);
      Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "<</2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    case big_int_e:
      /* big << big */
      mpz_clear(v1.big);
      mpz_clear(v2.big);
      Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, "<</2");
      P = (yamop *)FAILCODE;
      RERROR();
    default:
      /* error */
      mpz_clear(v1.big);
      RERROR();
    }
#endif
    default:
      /* error  */
      RERROR();
    }
}

/*
  slr >>

  First argument may be int or bignum;
  Second argument may only be an int.
*/
static E_FUNC
p_slr(Term t1, Term t2 E_ARGS)
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
      if (IntegerOfTerm(t2) < 0) {
	Int i2 = IntegerOfTerm(t2);
	if (i2 == Int_MIN) {
	  Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, ">>/2");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
	return do_sll(IntegerOfTerm(t1), -i2 USE_E_ARGS);
      }
      RINT(IntegerOfTerm(t1) >> IntegerOfTerm(t2));
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t2, ">>/2");
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, ">>/2");
      P = (yamop *)FAILCODE;
      RERROR();
#endif
    default:
      /* we've got a full term, need to evaluate it first */
      v1.Int = IntegerOfTerm(t1);
      bt1 = long_int_e;
      bt2 = ArithIEval(t2, &v2);
    }
    break;
  case double_e:
    Yap_Error(TYPE_ERROR_INTEGER, t1, ">>/2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    f2 = AritFunctorOfTerm(t2);

    switch (BlobOfFunctor(f2)) {
    case long_int_e:
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_sll_big_int(Yap_BigIntOfTerm(t1),  -IntegerOfTerm(t2), new);
	if (!new) {
	  Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, ">>/2");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
	RBIG(new);
      }
    case big_int_e:
      Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, ">>/2");
      P = (yamop *)FAILCODE;
      RERROR();
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t2, ">>/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    default:
      /* we've got a full term, need to evaluate it first */
      mpz_init_set(v1.big,Yap_BigIntOfTerm(t1));
      bt1 = big_int_e;
      bt2 = ArithIEval(t2, &v2);
      break;
    }
#endif
  default:
    /* we've got a full term, need to evaluate it first */
    bt1 = ArithIEval(t1, &v1);
    /* don't know anything about second */
    bt2 = ArithIEval(t2, &v2);
  }
  /* second case, no need no evaluation */
  switch (bt1) {
  case long_int_e:
    switch (bt2) {
    case long_int_e:
      if (v2.Int < 0) {
	if (v2.Int == Int_MIN) {
	  Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, MkIntegerTerm(v2.Int), ">>/2");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
	return do_sll(v1.Int, -v2.Int USE_E_ARGS);
      }
      RINT(v1.Int >> v2.Int);
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), ">>/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      mpz_clear(v2.big);
      Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, ">>/2");
      RERROR();
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  case double_e:
#if USE_GMP
    if (bt2 == big_int_e)
      mpz_clear(v2.big);
#endif
    Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v1.dbl), ">>/2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      /* big >> int */
      {
	MP_INT *new = TMP_BIG();

	new = Yap_gmp_sll_big_int(v1.big,  -v2.Int, new);
	if (!new) {
	  Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, MkIntegerTerm(v2.Int), ">>/2");
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
	MPZ_SET(new, v1.big);
	if (v2.Int > 0) {
	  mpz_tdiv_q_2exp(new, new, v2.Int);
	} else if (v2.Int < 0) {
	  if (v2.Int == Int_MIN) {
	    Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, MkIntegerTerm(v2.Int), ">>/2");
	    P = (yamop *)FAILCODE;
	    RERROR();
	  }
	  mpz_mul_2exp(new, v1.big, -v2.Int);
	}
	RBIG(new);
      }
    case double_e:
      /* big >> float */
      mpz_clear(v1.big);
      Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), ">>/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    case big_int_e:
      /* big >> big */
      mpz_clear(v1.big);
      mpz_clear(v2.big);
      Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, ">>/2");
      P = (yamop *)FAILCODE;
      RERROR();
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
