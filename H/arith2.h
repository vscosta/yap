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
  if ((i^j) >= 0 && (i^x) < 0) {
    MP_INT *new = Yap_InitBigNum(i);
    if (j > 0) {
      mpz_add_ui(new, new, j);
      RBIG(new);
    } else {
      mpz_add_ui(new, new, -j);
      RBIG(new);
    }
  }
#endif
  /* Integer overflow, we need to use big integers */
  RINT(x);
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

	if (i1 > 0) {
	  MP_INT *new = Yap_PreAllocBigNum();
	  mpz_add_ui(new, l2, i1);
	  RBIG(new);
	} else if (i1 == 0) {
	  RBIG(l2);
	} else {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_sub_ui(new, l2, -i1);
	  RBIG(new);
	}
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

	if (i2 > 0) {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_add_ui(new, l1, i2);
	  RBIG(new);
	} else if (i2 == 0) {
	  RBIG(l1);
	} else {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_sub_ui(new, l1, -i2);
	  RBIG(new);
	}
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_add(new, Yap_BigIntOfTerm(t1), Yap_BigIntOfTerm(t2));
	RBIG(new);
      }
    case double_e:
      RFLOAT(mpz_get_d(Yap_BigIntOfTerm(t1))+FloatOfTerm(t2));
    default:
      /* we've got a full term, need to evaluate it first */
      v1.big = Yap_BigIntOfTerm(t1);
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
      return(add_int(v1.Int,v2.Int USE_E_ARGS));
    case double_e:
      /* integer, double */
      RFLOAT(v1.Int+v2.dbl);
#ifdef USE_GMP
    case big_int_e:
      {
	if (v1.Int > 0) {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_add_ui(new, v2.big, v1.Int);
	  RBIG(new);
	} else if (v1.Int == 0) {
	  RBIG(v2.big);
	} else {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_add_ui(new, v2.big, -v1.Int);
	  RBIG(new);
	}
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
      if (v2.Int > 0) {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_add_ui(new, v1.big, v2.Int);
	RBIG(new);
      } else if (v2.Int == 0) {
	RBIG(v1.big);
      } else {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_sub_ui(new, v2.big, -v1.Int);
	RBIG(new);
      }
    case double_e:
      /* big * float */
      RFLOAT(mpz_get_d(v1.big)*v2.dbl);
    case big_int_e:
      /* big * big */
      {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_add(new, v1.big, v2.big);
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

	if (i1 > 0) {
	  MP_INT *new = Yap_PreAllocBigNum();
	  mpz_sub_ui(new, l2, i1);
	  mpz_neg(new, new);
	  RBIG(new);
	} else if (i1 == 0) {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_neg(new, l2);	  
	  RBIG(new);
	} else {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_add_ui(new, l2, -i1);
	  mpz_neg(new,new);
	  RBIG(new);
	}
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

	if (i2 > 0) {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_sub_ui(new, l1, i2);
	  RBIG(new);
	} else if (i2 == 0) {
	  RBIG(l1);
	} else {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_add_ui(new, l1, -i2);
	  RBIG(new);
	}
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_sub(new, Yap_BigIntOfTerm(t1), Yap_BigIntOfTerm(t2));
	RBIG(new);
      }
    case double_e:
      {
	RFLOAT(mpz_get_d(Yap_BigIntOfTerm(t1))-FloatOfTerm(t2));
      }
    default:
      /* we've got a full term, need to evaluate it first */
      v1.big = Yap_BigIntOfTerm(t1);
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
	if (v1.Int > 0) {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_sub_ui(new, v2.big, v1.Int);
	  mpz_neg(new, new);
	  RBIG(new);
	} else if (v1.Int == 0) {
	  MP_INT *new = Yap_PreAllocBigNum();
	  mpz_neg(new, v2.big);
	  RBIG(new);
	} else {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_add_ui(new, v2.big, -v1.Int);
	  mpz_neg(new, new);
	  RBIG(new);
	}
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
      RFLOAT(v1.dbl-mpz_get_d(v2.big));
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
      if (v2.Int > 0) {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_sub_ui(new, v1.big, v2.Int);
	RBIG(new);
      } else if (v2.Int == 0) {
	RBIG(v1.big);
      } else {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_add_ui(new, v2.big, -v1.Int);
	RBIG(new);
      }
    case double_e:
      /* big * float */
      RFLOAT(mpz_get_d(v1.big)-v2.dbl);
    case big_int_e:
      /* big * big */
      {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_sub(new, v1.big, v2.big);
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
    MP_INT *new = Yap_InitBigNum(i1);
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

	if (i1 > 0) {
	  MP_INT *new = Yap_PreAllocBigNum();
	  mpz_mul_ui(new, l2, i1);
	  RBIG(new);
	} else if (i1 == 0) {
	  RINT(0);
	} else {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_mul_ui(new, l2, -i1);
	  mpz_neg(new, new);
	  RBIG(new);
	}
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

	if (i2 > 0) {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_mul_ui(new, l1, i2);
	  RBIG(new);
	} else if (i2 == 0) {
	  RINT(0);
	} else {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_mul_ui(new, l1, -i2);
	  mpz_neg(new, new);
	  RBIG(new);
	}
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_mul(new, Yap_BigIntOfTerm(t1), Yap_BigIntOfTerm(t2));
	RBIG(new);
      }
    case double_e:
      {
	RFLOAT(mpz_get_d(Yap_BigIntOfTerm(t1))*FloatOfTerm(t2));
      }
    default:
      /* we've got a full term, need to evaluate it first */
      v1.big = Yap_BigIntOfTerm(t1);
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
	if (v1.Int > 0) {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_mul_ui(new, v2.big, v1.Int);
	  RBIG(new);
	} else if (v1.Int == 0) {
	  RINT(0);
	} else {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_mul_ui(new, v2.big, -v1.Int);
	  mpz_neg(new, new);
	  RBIG(new);
	}
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
      RFLOAT(v1.dbl*mpz_get_d(v2.big));
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
      if (v2.Int > 0) {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_mul_ui(new, v1.big, v2.Int);
	RBIG(new);
      } else if (v2.Int == 0) {
	RINT(0);
      } else {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_mul_ui(new, v2.big, -v1.Int);
	mpz_neg(new, new);
	RBIG(new);
      }
    case double_e:
      /* big * float */
      RFLOAT(mpz_get_d(v1.big)*v2.dbl);
    case big_int_e:
      /* big * big */
      {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_mul(new, v1.big, v2.big);
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
	MP_INT *l1 = Yap_BigIntOfTerm(t1);

	if (i2 > 0) {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_tdiv_q_ui(new, l1, i2);
	  RBIG(new);
	} else if (i2 == 0) {
	  Yap_Error(EVALUATION_ERROR_ZERO_DIVISOR, t2, "// /2");
	  /* make GCC happy */
	  P = (yamop *)FAILCODE;
	  RERROR();
	} else {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_tdiv_q_ui(new, l1, -i2);
	  mpz_neg(new, new);
	  RBIG(new);
	}
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = Yap_PreAllocBigNum();

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
      v1.big = Yap_BigIntOfTerm(t1);
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
      if (v2.Int > 0) {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_tdiv_q_ui(new, v1.big, v2.Int);
	RBIG(new);
      } else if (v2.Int == 0) {
	Yap_Error(EVALUATION_ERROR_ZERO_DIVISOR, t2, "// /2");
	/* make GCC happy */
	P = (yamop *)FAILCODE;
	RERROR();
      } else {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_tdiv_q_ui(new, v2.big, -v1.Int);
	mpz_neg(new, new);
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
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_tdiv_q(new, v1.big, v2.big);
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
	unsigned long int i2 = mpz_get_ui(Yap_BigIntOfTerm(t2));
	RINT(IntegerOfTerm(t1) & i2);
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
	unsigned long int i1 = mpz_get_ui(Yap_BigIntOfTerm(t1));
	RINT(i1 & IntegerOfTerm(t2));
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_and(new, Yap_BigIntOfTerm(t1), Yap_BigIntOfTerm(t2));
	RBIG(new);
      }
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t2, "/\\ /2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    default:
      /* we've got a full term, need to evaluate it first */
      v1.big = Yap_BigIntOfTerm(t1);
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
	unsigned long int i2 = mpz_get_ui(v2.big);
	RINT(v1.Int & i2);
      }
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  case double_e:
    Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v1.dbl), "/\\ /2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      /* anding a bignum with an integer is easy */
      {
	unsigned long int i1 = mpz_get_ui(v1.big);
	RINT(i1 & v2.Int);
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
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_and(new, v1.big, v2.big);
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
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_set_si(new,IntOfTerm(t1));
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
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_set_si(new,IntOfTerm(t2));
	mpz_ior(new, Yap_BigIntOfTerm(t1), new);
	RBIG(new);
      }
    case big_int_e:
      /* two bignums */
      {
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_ior(new, Yap_BigIntOfTerm(t1), Yap_BigIntOfTerm(t2));
	RBIG(new);
      }
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, t2, "\\/ /2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    default:
      /* we've got a full term, need to evaluate it first */
      v1.big = Yap_BigIntOfTerm(t1);
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
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_set_si(new,v1.Int);

	mpz_ior(new, new, v2.big);
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
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_set_si(new, v2.Int);
	mpz_ior(new, v1.big, new);
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
	MP_INT *new = Yap_PreAllocBigNum();

	mpz_ior(new, v1.big, v2.big);
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
      RINT(IntegerOfTerm(t1) << IntegerOfTerm(t2));
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
	  MP_INT *new = Yap_PreAllocBigNum();
	  mpz_mul_2exp(new, l1, i2);
	  RBIG(new);
	} else if (i2 == 0) {
	  RBIG(l1);
	} else {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_tdiv_q_2exp(new, l1, -i2);
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
      v1.big = Yap_BigIntOfTerm(t1);
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
      RINT(v1.Int << v2.Int);
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "<</2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, "<</2");
      P = (yamop *)FAILCODE;
      RERROR();
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  case double_e:
    Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v1.dbl), "<</2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      /* big << int */
      {
	if (v2.Int > 0) {
	  MP_INT *new = Yap_PreAllocBigNum();
	  mpz_mul_2exp(new, v1.big, v2.Int);
	  RBIG(new);
	} else if (v2.Int == 0) {
	  RBIG(v1.big);
	} else {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_tdiv_q_2exp(new, v1.big, -v2.Int);
	  RBIG(new);
	}
      }
    case double_e:
      /* big << float */
      Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), "<</2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    case big_int_e:
      /* big << big */
      Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, "<</2");
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

	if (i2 > 0) {
	  MP_INT *new = Yap_PreAllocBigNum();
	  mpz_tdiv_q_2exp(new, l1, i2);
	  RBIG(new);
	} else if (i2 == 0) {
	  RBIG(l1);
	} else {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_mul_2exp(new, l1, -i2);
	  RBIG(new);
	}
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
      v1.big = Yap_BigIntOfTerm(t1);
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
      RINT(v1.Int >> v2.Int);
    case double_e:
      Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), ">>/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
#ifdef USE_GMP
    case big_int_e:
      Yap_Error(DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW, t2, ">>/2");
      P = (yamop *)FAILCODE;
      RERROR();
#endif
    default:
      /* Yap_Error */
      RERROR();
    }
  case double_e:
    Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v1.dbl), ">>/2");
    P = (yamop *)FAILCODE;
    RERROR();
#ifdef USE_GMP
  case big_int_e:
    switch (bt2) {
    case long_int_e:
      /* big >> int */
      {
	if (v2.Int > 0) {
	  MP_INT *new = Yap_PreAllocBigNum();
	  mpz_tdiv_q_2exp(new, v1.big, v2.Int);
	  RBIG(new);
	} else if (v2.Int == 0) {
	  RBIG(v1.big);
	} else {
	  MP_INT *new = Yap_PreAllocBigNum();

	  mpz_mul_2exp(new, v1.big, -v2.Int);
	  RBIG(new);
	}
      }
    case double_e:
      /* big >> float */
      Yap_Error(TYPE_ERROR_INTEGER, MkFloatTerm(v2.dbl), ">>/2");
      /* make GCC happy */
      P = (yamop *)FAILCODE;
      RERROR();
    case big_int_e:
      /* big >> big */
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
