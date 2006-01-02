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
  /* Integer overflow, we need to use big integers */
#if USE_GMP
  if ((i^j) >= 0 && (i^x) < 0) {
    MP_INT *new = TMP_BIG();

    mpz_init_set_si(new,i);
    if (j > 0) {
      mpz_add_ui(new, new, j);
      RBIG(new);
    } else {
      mpz_sub_ui(new, new, -j);
      RBIG(new);
    }
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
      return(add_int(IntegerOfTerm(t1),IntegerOfTerm(t2) USE_E_ARGS));
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
	Int i1 = IntegerOfTerm(t1);
	MP_INT *l2 = Yap_BigIntOfTerm(t2);
	MP_INT *new = TMP_BIG();

	mpz_init_set(new, l2);
	if (i1 > 0) {
	  mpz_add_ui(new, new, i1);
	} else if (i1 < 0) {
	  mpz_sub_ui(new, new, -i1);
	}
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
      RFLOAT(FloatOfTerm(t1)+mpz_get_d(Yap_BigIntOfTerm(t2)));
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
	Int i2 = IntegerOfTerm(t2);
	MP_INT *l1 = Yap_BigIntOfTerm(t1);
	MP_INT *new = TMP_BIG();

	mpz_init_set(new,l1);
	if (i2 > 0) {
	  mpz_add_ui(new, new, i2);
	} else if (i2 < 0) {
	  mpz_sub_ui(new, l1, -i2);
	}
	RBIG(new);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = TMP_BIG();

	mpz_init_set(new, Yap_BigIntOfTerm(t1));
	mpz_add(new, new, Yap_BigIntOfTerm(t2));
	RBIG(new);
      }
    case double_e:
      RFLOAT(mpz_get_d(Yap_BigIntOfTerm(t1))+FloatOfTerm(t2));
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

	MPZ_SET(new, v2.big);
	if (v1.Int > 0) {
	  mpz_add_ui(new, new, v1.Int);
	} else if (v1.Int < 0) {
	  mpz_sub_ui(new, new, -v1.Int);
	}
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
      RFLOAT(v1.dbl+mpz_get_d(v2.big));
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

	MPZ_SET(new, v1.big);
	/* big * integer */
	if (v2.Int > 0) {
	  mpz_add_ui(new, new, v2.Int);
	} else if (v2.Int < 0) {
	  mpz_sub_ui(new, new, -v2.Int);
	}
	RBIG(new);
      }
    case double_e:
      /* big * float */
      {
	Float dbl = mpz_get_d(v1.big)+v2.dbl;
	mpz_clear(v1.big);
	RFLOAT(dbl);
      }
    case big_int_e:
      /* big * big */
      {
	MP_INT *new = TMP_BIG();

	MPZ_SET(new, v1.big);
	mpz_add(new, new, v2.big);
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
      return(add_int(IntegerOfTerm(t1),-IntegerOfTerm(t2) USE_E_ARGS));
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
	Int i1 = IntegerOfTerm(t1);
	MP_INT *l2 = Yap_BigIntOfTerm(t2);
	MP_INT *new = TMP_BIG();

	mpz_init_set(new, l2);
	if (i1 > 0) {
	  mpz_ui_sub(new, i1, new);
	} else if (i1 == 0) {
	  mpz_neg(new, new);	  
	} else {
	  mpz_add_ui(new, new, -i1);
	  mpz_neg(new, new);	  
	}
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
	RFLOAT(FloatOfTerm(t1)-mpz_get_d(Yap_BigIntOfTerm(t2)));
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
	Int i2 = IntegerOfTerm(t2);
	MP_INT *l1 = Yap_BigIntOfTerm(t1);
	MP_INT *new = TMP_BIG();

	mpz_init_set(new, l1);
	if (i2 > 0) {
	  mpz_sub_ui(new, new, i2);
	} else if (i2 < 0) {
	  mpz_add_ui(new, new, -i2);
	}
	RBIG(new);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = TMP_BIG();

	mpz_init_set(new, Yap_BigIntOfTerm(t1));
	mpz_sub(new, new, Yap_BigIntOfTerm(t2));
	RBIG(new);
      }
    case double_e:
      {
	RFLOAT(mpz_get_d(Yap_BigIntOfTerm(t1))-FloatOfTerm(t2));
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
      return(add_int(v1.Int,-v2.Int USE_E_ARGS));
    case double_e:
      {
	/* integer, double */
	RFLOAT(v1.Int-v2.dbl);
      }
#ifdef USE_GMP
    case big_int_e:
      {
	MP_INT *new = TMP_BIG();

	MPZ_SET(new, v2.big);
	if (v1.Int > 0) {
	  mpz_ui_sub(new, v1.Int, v2.big);
	} else if (v1.Int == 0) {
	  mpz_neg(new, new);
	} else {
	  mpz_add_ui(new, new, -v1.Int);
	  mpz_neg(new, new);
	}
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
	Float flt = v1.dbl-mpz_get_d(v2.big);

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

	MPZ_SET(new, v1.big);
	/* big - integer */
	if (v2.Int > 0) {
	  mpz_sub_ui(new, new, v2.Int);
	} else if (v2.Int < 0) {
	  mpz_add_ui(new, new, -v2.Int);
	}
	RBIG(new);
      }
    case double_e:
      /* big * float */
      {
	Float flt = mpz_get_d(v1.big)-v2.dbl;
	
	mpz_clear(v1.big);
	RFLOAT(flt);
      }
    case big_int_e:
      /* big * big */
      {
	MP_INT *new = TMP_BIG();

	MPZ_SET(new, v1.big);
	mpz_sub(new, new, v2.big);
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
    mpz_init_set_si(new,i1);
    if (i2 > 0) {
      mpz_mul_ui(new, new, i2);
      RBIG(new);
    } else {
      mpz_mul_ui(new, new, -i2);
      mpz_neg(new, new);
      RBIG(new);
    }
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
	Int i1 = IntegerOfTerm(t1);
	MP_INT *l2 = Yap_BigIntOfTerm(t2);
	MP_INT *new = TMP_BIG();

	mpz_init_set(new, l2);
	mpz_mul_si(new, new, i1);
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
	RFLOAT(FloatOfTerm(t1)*mpz_get_d(Yap_BigIntOfTerm(t2)));
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
	Int i2 = IntegerOfTerm(t2);
	MP_INT *l1 = Yap_BigIntOfTerm(t1);
	MP_INT *new = TMP_BIG();

	mpz_init_set(new, l1);
	mpz_mul_si(new, new, i2);
	RBIG(new);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = TMP_BIG();

	mpz_init_set(new, Yap_BigIntOfTerm(t1));
	mpz_mul(new, new, Yap_BigIntOfTerm(t2));
	RBIG(new);
      }
    case double_e:
      {
	RFLOAT(mpz_get_d(Yap_BigIntOfTerm(t1))*FloatOfTerm(t2));
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

	MPZ_SET(new, v2.big);
	mpz_mul_si(new, new, v1.Int);
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
	Float flt = v1.dbl*mpz_get_d(v2.big);

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

	MPZ_SET(new, v1.big);
	mpz_mul_si(new, new, v2.Int);
	RBIG(new);
      }
    case double_e:
      /* big * float */
      RFLOAT(mpz_get_d(v1.big)*v2.dbl);
    case big_int_e:
      /* big * big */
      {
	MP_INT *new = TMP_BIG();

	MPZ_SET(new, v1.big);
	mpz_mul(new, new, v2.big);
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
	Int i2 = IntegerOfTerm(t2);
	MP_INT *new = TMP_BIG();

	mpz_init_set(new, Yap_BigIntOfTerm(t1));
	if (i2 > 0) {
	  mpz_tdiv_q_ui(new, new, i2);
	  RBIG(new);
	} else if (i2 == 0) {
	  Yap_Error(EVALUATION_ERROR_ZERO_DIVISOR, t2, "// /2");
	  /* make GCC happy */
	  P = (yamop *)FAILCODE;
	  RERROR();
	} else {
	  mpz_tdiv_q_ui(new, new, -i2);
	  mpz_neg(new, new);
	  RBIG(new);
	}
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = TMP_BIG();

	mpz_init(new);
	mpz_tdiv_q(new, Yap_BigIntOfTerm(t1), Yap_BigIntOfTerm(t2));
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

	MPZ_SET(new, v1.big);
	if (v2.Int > 0) {
	  mpz_tdiv_q_ui(new, new, v2.Int);
	} else if (v2.Int == 0) {
	  mpz_clear(new);
	  Yap_Error(EVALUATION_ERROR_ZERO_DIVISOR, t2, "// /2");
	  /* make GCC happy */
	  P = (yamop *)FAILCODE;
	  RERROR();
	} else {
	  mpz_tdiv_q_ui(new, new, -v2.Int);
	  mpz_neg(new, new);
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

	MPZ_SET(new, v1.big);
	mpz_tdiv_q(new, new, v2.big);
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
	mpz_init_set_si(new, IntegerOfTerm(t1));
	mpz_and(new, new, Yap_BigIntOfTerm(t2));
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
	mpz_init_set_si(new, IntegerOfTerm(t2));
	mpz_and(new, new, Yap_BigIntOfTerm(t1));
	RBIG(new);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = TMP_BIG();

	mpz_init_set(new, Yap_BigIntOfTerm(t1));
	mpz_and(new, new, Yap_BigIntOfTerm(t2));
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
	mpz_init_set_si(new, v1.Int);
	mpz_and(new, new, v2.big);
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
	mpz_init_set_si(new, v2.Int);
	mpz_and(new, new, v1.big);
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

	MPZ_SET(new, v1.big);
	mpz_and(new, new, v2.big);
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

	mpz_init_set_si(new, IntegerOfTerm(t1));
	mpz_ior(new, new, Yap_BigIntOfTerm(t2));
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

	mpz_init_set_si(new,IntegerOfTerm(t2));
	mpz_ior(new, Yap_BigIntOfTerm(t1), new);
	RBIG(new);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = TMP_BIG();

	mpz_init_set(new, Yap_BigIntOfTerm(t1));
	mpz_ior(new, new, Yap_BigIntOfTerm(t2));
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

	mpz_init_set_si(new,v1.Int);
	mpz_ior(new, new, v2.big);
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

	mpz_init_set_si(new, v2.Int);
	mpz_ior(new, v1.big, new);
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

	MPZ_SET(new, v1.big);
	mpz_ior(new, new, v2.big);
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

static Int
sll_ovflw(Int x,Int i)
{
  CELL t = (1<<x)-1;
  return (t & i) != i;
}

inline static E_FUNC
do_sll(Int i, Int j E_ARGS)
{
#if USE_GMP
  Int x = (8*sizeof(CELL)-2)-j;
  
  if (x < 0||
      sll_ovflw(x,i)) {
    MP_INT *new = TMP_BIG();

    mpz_init_set_si(new,i);
    mpz_mul_2exp(new, new, j);
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
	RINT(IntegerOfTerm(t1) >> -IntegerOfTerm(t2));
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
	Int i2 = IntegerOfTerm(t2);
	MP_INT *l1 = Yap_BigIntOfTerm(t1);

	if (i2 > 0) {
	  MP_INT *new = TMP_BIG();

	  mpz_init_set(new, l1);
	  mpz_mul_2exp(new, new, i2);
	  RBIG(new);
	} else if (i2 == 0) {
	  MP_INT *new = TMP_BIG();

	  mpz_init_set(new, l1);
	  RBIG(new);
	} else {
	  MP_INT *new = TMP_BIG();

	  mpz_init_set(new, l1);
	  mpz_tdiv_q_2exp(new, new, -i2);
	  RBIG(new);
	}
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

	MPZ_SET(new, v1.big);
	if (v2.Int > 0) {
	  mpz_mul_2exp(new, new, v2.Int);
	} else if (v2.Int < 0) {
	  mpz_tdiv_q_2exp(v1.big, v1.big, -v2.Int);
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
	return do_sll(IntegerOfTerm(t1),-IntegerOfTerm(t2) USE_E_ARGS);
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
	Int i2 = IntegerOfTerm(t2);
	MP_INT *l1 = Yap_BigIntOfTerm(t1);
	MP_INT *new = TMP_BIG();

	mpz_init_set(new, l1);
	if (i2 > 0) {
	  mpz_tdiv_q_2exp(new, new, i2);
	} else if (i2 < 0) {
	  mpz_mul_2exp(new, new, -i2);
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

	MPZ_SET(new, v1.big);
	if (v2.Int > 0) {
	  mpz_tdiv_q_2exp(new, new, v2.Int);
	} else if (v2.Int < 0) {
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
