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

inline static int
add_overflow(Int x, Int i, Int j)
{
  return ((i & j & ~x) | (~i & ~j & x)) < 0;
}

inline static Term
add_int(Int i, Int j)
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

inline static int
sub_overflow(Int x, Int i, Int j)
{
  return ((i & ~j & ~x) | (~i & j & x)) < 0;
}

inline static Term
sub_int(Int i, Int j)
{
  Int x = i-j;
#if USE_GMP
  Int overflow = ((i & ~j & ~x) | (~i & j & x)) < 0;
  /* Integer overflow, we need to use big integers */
  if (overflow) {
    return(Yap_gmp_sub_ints(i, j));
  }
#endif
#ifdef BEAM
  RINT(x);
  return( MkIntegerTerm (x));
#else
  RINT(x);
#endif
}

#ifdef __GNUC__
#ifdef __i386__
#define DO_MULTI() { Int tmp1;						\
    __asm__ ("imull  %3\n\t movl   $0,%1\n\t jno    0f\n\t movl   $1,%1\n\t 0:"	\
	     : "=a" (z),						\
	       "=d" (tmp1)						\
	     : "a" (i1),						\
	       "rm" (i2)						\
	     : "cc" );							\
    if (tmp1) goto overflow;						\
  }
#define OPTIMIZE_MULTIPLI 1
#endif
#endif


inline static int
mul_overflow(Int z, Int i1, Int i2)
{
  if (i1 == Int_MIN && i2 == -1)
    return TRUE;
  return (i2 &&  z/i2 != i1);
}

#ifndef OPTIMIZE_MULTIPLI
#define DO_MULTI() z = i1*i2;			\
  if (i2 &&  z/i2 != i1) goto overflow
#endif

inline static Term
times_int(Int i1, Int i2) {
#ifdef USE_GMP
  Int z;
  DO_MULTI();
  RINT(z);
 overflow:
  {
    return(Yap_gmp_mul_ints(i1, i2));
  }
#else
  RINT(i1*i2);
#endif
}


#if USE_GMP
static inline int
sl_overflow(Int i,Int j)
{
  Int x = (8*sizeof(CELL)-2)-j;
  CELL t = (1<<x)-1;

  if (x < 0) return TRUE;
  t = (1<<x)-1;
  return (t & i) != i;
}
#endif

inline static Term
do_sll(Int i, Int j)
{
#ifdef USE_GMP
  if (sl_overflow(i,j)) {
    return Yap_gmp_sll_ints(i, j);
  }
#endif
  RINT(i << j);
}


static inline Term
p_plus(Term t1, Term t2) {
  switch (ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      return add_int(IntegerOfTerm(t1),IntegerOfTerm(t2));
    case double_e:
      {
	/* integer, double */
	Float fl1 = (Float)IntegerOfTerm(t1);
	Float fl2 = FloatOfTerm(t2);
	RFLOAT(fl1+fl2);
      }
    case big_int_e:
#ifdef USE_GMP
      return(Yap_gmp_add_int_big(IntegerOfTerm(t1), t2));
#endif
    default:
      RERROR();
    }
  case double_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* float * integer */
      RFLOAT(FloatOfTerm(t1)+IntegerOfTerm(t2));
    case double_e:
      RFLOAT(FloatOfTerm(t1)+FloatOfTerm(t2));
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_add_float_big(FloatOfTerm(t1),t2);
#endif
    default:
      RERROR();
    }
  case big_int_e:
#ifdef USE_GMP
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      return Yap_gmp_add_int_big(IntegerOfTerm(t2), t1);
    case big_int_e:
      /* two bignums */
      return Yap_gmp_add_big_big(t1, t2);
    case double_e:
      return Yap_gmp_add_float_big(FloatOfTerm(t2),t1);
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}

static Term
p_minus(Term t1, Term t2) {
  switch (ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      return sub_int(IntegerOfTerm(t1), IntegerOfTerm(t2));
    case double_e:
      {
	/* integer, double */
	Float fl1 = (Float)IntegerOfTerm(t1);
	Float fl2 = FloatOfTerm(t2);
	RFLOAT(fl1-fl2);
      }
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_sub_int_big(IntegerOfTerm(t1), t2);
#endif
    default:
      RERROR();
    }
    break;
  case double_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* float * integer */
      RFLOAT(FloatOfTerm(t1)-IntegerOfTerm(t2));
    case double_e:
      {
	RFLOAT(FloatOfTerm(t1)-FloatOfTerm(t2));
      }
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_sub_float_big(FloatOfTerm(t1),t2);
#endif
    default:
      RERROR();
    }
    break;
  case big_int_e:
#ifdef USE_GMP
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      return Yap_gmp_sub_big_int(t1, IntegerOfTerm(t2));
    case big_int_e:
      return Yap_gmp_sub_big_big(t1, t2);
    case double_e:
      return Yap_gmp_sub_big_float(t1,FloatOfTerm(t2));
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}


static Term
p_times(Term t1, Term t2) {
  switch (ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      return(times_int(IntegerOfTerm(t1),IntegerOfTerm(t2)));
    case double_e:
      {
	/* integer, double */
	Float fl1 = (Float)IntegerOfTerm(t1);
	Float fl2 = FloatOfTerm(t2);
	RFLOAT(fl1*fl2);
      }
    case big_int_e:
#ifdef USE_GMP
      return(Yap_gmp_mul_int_big(IntegerOfTerm(t1), t2));
#endif
    default:
      RERROR();
    }
    break;
  case double_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* float * integer */
      RFLOAT(FloatOfTerm(t1)*IntegerOfTerm(t2));
    case double_e:
      RFLOAT(FloatOfTerm(t1)*FloatOfTerm(t2));
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_mul_float_big(FloatOfTerm(t1),t2);
#endif
    default:
      RERROR();
    }
    break;
  case big_int_e:
#ifdef USE_GMP
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      return Yap_gmp_mul_int_big(IntegerOfTerm(t2), t1);
    case big_int_e:
      /* two bignums */
      return Yap_gmp_mul_big_big(t1, t2);
    case double_e:
      return Yap_gmp_mul_float_big(FloatOfTerm(t2),t1);
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}

static Term
p_div(Term t1, Term t2) {
  switch (ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      {
	Int i1 = IntegerOfTerm(t1), i2 = IntegerOfTerm(t2);
      
	if (i2 == 0) {
	  return Yap_ArithError(EVALUATION_ERROR_ZERO_DIVISOR, t2, "// /2");
	} else if (i1 == Int_MIN && i2 == -1) {
#ifdef USE_GMP
	  return Yap_gmp_add_ints(Int_MAX, 1);
#else
	  return Yap_ArithError(EVALUATION_ERROR_INT_OVERFLOW, t1,
		    "rem/2 with %d and %d", i1, i2);
#endif
	} else {
	  RINT(IntegerOfTerm(t1) / i2);
	}
      }
    case double_e:
      return Yap_ArithError(TYPE_ERROR_INTEGER, t2, "// /2");
    case big_int_e:
#ifdef USE_GMP
      /* dividing a bignum by an integer */
      return Yap_gmp_div_int_big(IntegerOfTerm(t1), t2);
#endif
    default:
      RERROR();
    }
    break;
  case double_e:
    return Yap_ArithError(TYPE_ERROR_INTEGER, t1, "// /2");
  case big_int_e:
#ifdef USE_GMP
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* dividing a bignum by an integer */
      return Yap_gmp_div_big_int(t1, IntegerOfTerm(t2));
    case big_int_e:
      /* two bignums */
      return Yap_gmp_div_big_big(t1, t2);
    case double_e:
      return Yap_ArithError(TYPE_ERROR_INTEGER, t2, "// /2");
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}

static Term
p_and(Term t1, Term t2) {
  switch (ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      RINT(IntegerOfTerm(t1) & IntegerOfTerm(t2));
    case double_e:
      return Yap_ArithError(TYPE_ERROR_INTEGER, t2, "/\\ /2");
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_and_int_big(IntegerOfTerm(t1),t2);
#endif
    default:
      RERROR();
    }
    break;
  case double_e:
    return Yap_ArithError(TYPE_ERROR_INTEGER, t1, "/\\ /2");
  case big_int_e:
#ifdef USE_GMP
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* anding a bignum with an integer is easy */
      return Yap_gmp_and_int_big(IntegerOfTerm(t2),t1);
    case big_int_e:
      /* two bignums */
      return Yap_gmp_and_big_big(t1, t2);
    case double_e:
      return Yap_ArithError(TYPE_ERROR_INTEGER, t2, "/\\ /2");
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}

static Term
p_or(Term t1, Term t2) {
  switch(ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      RINT(IntegerOfTerm(t1) | IntegerOfTerm(t2));
    case double_e:
      return Yap_ArithError(TYPE_ERROR_INTEGER, t2, "\\/ /2");
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_ior_int_big(IntegerOfTerm(t1),t2);
#endif
    default:
      RERROR();
    }
    break;
  case double_e:
    return Yap_ArithError(TYPE_ERROR_INTEGER, t1, "\\/ /2");
  case big_int_e:
#ifdef USE_GMP
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* anding a bignum with an integer is easy */
      return Yap_gmp_ior_int_big(IntegerOfTerm(t2),t1);
    case big_int_e:
      /* two bignums */
      return Yap_gmp_ior_big_big(t1, t2);
    case double_e:
      return Yap_ArithError(TYPE_ERROR_INTEGER, t2, "\\/ /2");
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}

static Term
p_sll(Term t1, Term t2) {
  switch (ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      if (IntegerOfTerm(t2) < 0) {
	Int i2 = IntegerOfTerm(t2);
	if (i2 == Int_MIN) {
	  return Yap_ArithError(RESOURCE_ERROR_HUGE_INT, t2, ">>/2");
	}
	RINT(IntegerOfTerm(t1) >> -i2);
      }
      return do_sll(IntegerOfTerm(t1),IntegerOfTerm(t2));
    case double_e:
      return Yap_ArithError(TYPE_ERROR_INTEGER, t2, "<</2");
    case big_int_e:
#ifdef USE_GMP
      return Yap_ArithError(RESOURCE_ERROR_HUGE_INT, t2, "<</2");
#endif
    default:
      RERROR();
    }
    break;
  case double_e:
    return Yap_ArithError(TYPE_ERROR_INTEGER, t1, "<< /2");
  case big_int_e:
#ifdef USE_GMP
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      return Yap_gmp_sll_big_int(t1,  IntegerOfTerm(t2));
    case big_int_e:
      return Yap_ArithError(RESOURCE_ERROR_HUGE_INT, t2, ">>/2");
    case double_e:
      return Yap_ArithError(TYPE_ERROR_INTEGER, t2, "<</2");
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}

static Term
p_slr(Term t1, Term t2) {
  switch (ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      if (IntegerOfTerm(t2) < 0) {
	Int i2 = IntegerOfTerm(t2);
	if (i2 == Int_MIN) {
	  return Yap_ArithError(RESOURCE_ERROR_HUGE_INT, t2, ">>/2");
	}
	return do_sll(IntegerOfTerm(t1), -i2);
      }
      RINT(IntegerOfTerm(t1) >> IntegerOfTerm(t2));
    case double_e:
      return Yap_ArithError(TYPE_ERROR_INTEGER, t2, ">>/2");
    case big_int_e:
#ifdef USE_GMP
      return Yap_ArithError(RESOURCE_ERROR_HUGE_INT, t2, ">>/2");
#endif
    default:
      RERROR();
    }
    break;
  case double_e:
    return Yap_ArithError(TYPE_ERROR_INTEGER, t1, ">>/2");
  case big_int_e:
#ifdef USE_GMP
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      return Yap_gmp_sll_big_int(t1,  -IntegerOfTerm(t2));
    case big_int_e:
      return Yap_ArithError(RESOURCE_ERROR_HUGE_INT, t2, ">>/2");
    case double_e:
      return Yap_ArithError(TYPE_ERROR_INTEGER, t2, ">>/2");
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}

