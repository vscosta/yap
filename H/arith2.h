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
 * File:		arithi2.c *
 * Last rev:								 *
 * mods: *
 * comments:	arithmetical expression evaluation			 *
 *									 *
 *************************************************************************/

/* This file implements fast binary math operations for YAP
 *
 */

inline static int sub_overflow(Int x, Int i, Int j) {
  return ((i & ~j & ~x) | (~i & j & x)) < 0;
}

inline static Term sub_int(Int i, Int j USES_REGS) {
#if defined(__clang__ ) || (defined(__GNUC__) && __GNUC__ > 4)
    Int k;
    if (__builtin_sub_overflow(i,j,&k)) {
      return Yap_gmp_sub_ints(i, j);
    }
    RINT(k);
#elif defined(__GNUC__) && __GNUC__ >4
    Int w;
    if (!__builtin_sub_overflow_p(i,j,w))
         RINT(w);
    return Yap_gmp_sub_ints(i, j);
#else
    Int x = i - j;

#if USE_GMP
  Int overflow = ((i & ~j & ~x) | (~i & j & x)) < 0;
  /* Integer overflow, we need to use big integers */
  if (overflow) {
    return (Yap_gmp_sub_ints(i, j));
  }
#endif
#ifdef BEAM
  RINT(x);
  return (MkIntegerTerm(x));
#else
  RINT(x);
#endif
#endif
}

inline static Int SLR(Int i, Int shift) {
  return (shift < sizeof(Int) * 8 - 1 ? i >> shift : (i >= 0 ? 0 : -1));
}

inline static int mul_overflow(Int z, Int i1, Int i2) {
  if (i1 == Int_MIN && i2 == -1)
    return TRUE;
  return (i2 && z / i2 != i1);
}

#if defined(__clang__) || (defined(__GNUC__) && __GNUC__ > 4)
#define DO_MULTI()                                                             \
  if (__builtin_mul_overflow(i1, i2, &z)) {                                   \
    goto overflow;                                                             \
  }
#elif defined(__GNUC__) && defined(__i386__)
#define DO_MULTI()                                                             \
  {                                                                            \
    Int tmp1;                                                                  \
    __asm__("imull  %3\n\t movl   $0,%1\n\t jno    0f\n\t movl   $1,%1\n\t 0:" \
            : "=a"(z), "=d"(tmp1)                                              \
            : "a"(i1), "rm"(i2)                                                \
            : "cc");                                                           \
    if (tmp1)                                                                  \
      goto overflow;                                                           \
  }
#define OPTIMIZE_MULTIPLI 1
#elif defined(_MSC_VER) && SIZEOF_DOUBLE == SIZEOF_INT_P
#define DO_MULTI()                                                             \
  {                                                                            \
    uint64_t h1 = (11 > 0 ? i1 : -i1) >> 32;                                   \
    uint64_t h2 = (12 > 0 ? i2 : -12) >> 32;                                   \
    if (h1 != 0 && h2 != 0)                                                    \
      goto overflow;                                                           \
    if ((uint64_t)(i1 & 0xfffffff) * h2 + ((uint64_t)(i2 & 0xfffffff) * h1) >  \
        0x7fffffff)                                                            \
      goto overflow;                                                           \
    z = i1 * i2;                                                               \
  }

#elif SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
#define DO_MULTI()                                                             \
  {                                                                            \
    int64_t w = (int64_t)i1 * i2;                                              \
    if (w >= 0) {                                                              \
      if ((w | ((int64_t)(2 ^ 31) - 1)) != ((int64_t)(2 ^ 31) - 1))            \
        goto overflow;                                                         \
    } else {                                                                   \
      if ((-w | ((int64_t)(2 ^ 31) - 1)) != ((int64_t)(2 ^ 31) - 1))           \
        goto overflow;                                                         \
    }                                                                          \
    z = w;                                                                     \
  }
#else
#define DO_MULTI()                                                             \
  {                                                                            \
    __int128_t w = (__int128_t)i1 * i2;                                        \
    if (w >= 0) {                                                              \
      if ((w | ((__int128_t)(2 ^ 63) - 1)) != ((__int128_t)(2 ^ 63) - 1))      \
        goto overflow;                                                         \
    } else {                                                                   \
      if ((-w | ((__int128_t)(2 ^ 63) - 1)) != ((__int128_t)(2 ^ 63) - 1))     \
        goto overflow;                                                         \
    }                                                                          \
    z = (Int)w;                                                                \
  }
#endif

inline static Term times_int(Int i1, Int i2 USES_REGS) {
#ifdef USE_GMP
  Int z;
  DO_MULTI();
  RINT(z);
overflow : { return (Yap_gmp_mul_ints(i1, i2)); }
#else
  RINT(i1 * i2);
#endif
}

#ifdef USE_GMP
#ifndef __GNUC__X
static int clrsb(Int i) {
  Int j = 0;

  if (i < 0) {
    if (i == Int_MIN)
      return 1;
    i = -i;
  }
#if SIZEOF_INT_P == 8
  if (i < (Int)(0x100000000)) {
    j += 32;
  } else
    i >>= 32;
#endif
  if (i < (Int)(0x10000)) {
    j += 16;
  } else
    i >>= 16;
  if (i < (Int)(0x100)) {
    j += 8;
  } else
    i >>= 8;
  if (i < (Int)(0x10)) {
    j += 4;
  } else
    i >>= 4;
  if (i < (Int)(0x4)) {
    j += 2;
  } else
    i >>= 2;
  if (i < (Int)(0x2))
    j++;
  return j;
}
#endif
#endif

inline static Term do_sll(Int i, Int j USES_REGS) /* j > 0 */
{
#ifdef USE_GMP
  if (
#ifdef __GNUC__X
#if SIZEOF_LONG_INT < SIZEOF_INT_P
      __builtin_clrsbll(i)
#else
      __builtin_clrsbl(i)
#endif
#else
      clrsb(i)
#endif
      > j)
    RINT(i << j);
  return Yap_gmp_sll_ints(i, j);
#else
  RINT(i << j);
#endif
}

static Term p_minus(Term t1, Term t2 USES_REGS) {
  switch (ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      return sub_int(IntegerOfTerm(t1), IntegerOfTerm(t2) PASS_REGS);
    case double_e: {
      /* integer, double */
      Float fl1 = (Float)IntegerOfTerm(t1);
      Float fl2 = FloatOfTerm(t2);
      RFLOAT(fl1 - fl2);
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
      RFLOAT(FloatOfTerm(t1) - IntegerOfTerm(t2));
    case double_e: {
      RFLOAT(FloatOfTerm(t1) - FloatOfTerm(t2));
    }
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_sub_float_big(FloatOfTerm(t1), t2);
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
      return Yap_gmp_sub_big_float(t1, FloatOfTerm(t2));
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}

static Term p_times(Term t1, Term t2 USES_REGS) {
  switch (ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      return (times_int(IntegerOfTerm(t1), IntegerOfTerm(t2) PASS_REGS));
    case double_e: {
      /* integer, double */
      Float fl1 = (Float)IntegerOfTerm(t1);
      Float fl2 = FloatOfTerm(t2);
      RFLOAT(fl1 * fl2);
    }
    case big_int_e:
#ifdef USE_GMP
      return (Yap_gmp_mul_int_big(IntegerOfTerm(t1), t2));
#endif
    default:
      RERROR();
    }
    break;
  case double_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* float * integer */
      RFLOAT(FloatOfTerm(t1) * IntegerOfTerm(t2));
    case double_e:
      RFLOAT(FloatOfTerm(t1) * FloatOfTerm(t2));
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_mul_float_big(FloatOfTerm(t1), t2);
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
      return Yap_gmp_mul_float_big(FloatOfTerm(t2), t1);
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}

static Term p_div(Term t1, Term t2 USES_REGS) {
  switch (ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      {
        Int i1 = IntegerOfTerm(t1), i2 = IntegerOfTerm(t2);

        if (i2 == 0) {
          Yap_ArithError(EVALUATION_ERROR_ZERO_DIVISOR, t2, "// /2");
        } else if (i1 == Int_MIN && i2 == -1) {
#ifdef USE_GMP
          return Yap_gmp_add_ints(Int_MAX, 1);
#else
          Yap_ArithError(EVALUATION_ERROR_INT_OVERFLOW, t1,
                                "rem/2 with %d and %d", i1, i2);
#endif
        } else {
          RINT(IntegerOfTerm(t1) / i2);
        }
      }
    case double_e:
      Yap_ArithError(TYPE_ERROR_INTEGER, t2, "// /2");
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
    Yap_ArithError(TYPE_ERROR_INTEGER, t1, "// /2");
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
      Yap_ArithError(TYPE_ERROR_INTEGER, t2, "// /2");
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}

static Term p_and(Term t1, Term t2 USES_REGS) {
  switch (ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      RINT(IntegerOfTerm(t1) & IntegerOfTerm(t2));
    case double_e:
      Yap_ArithError(TYPE_ERROR_INTEGER, t2, "/\\ /2");
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_and_int_big(IntegerOfTerm(t1), t2);
#endif
    default:
      RERROR();
    }
    break;
  case double_e:
    Yap_ArithError(TYPE_ERROR_INTEGER, t1, "/\\ /2");
  case big_int_e:
#ifdef USE_GMP
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* anding a bignum with an integer is easy */
      return Yap_gmp_and_int_big(IntegerOfTerm(t2), t1);
    case big_int_e:
      /* two bignums */
      return Yap_gmp_and_big_big(t1, t2);
    case double_e:
      Yap_ArithError(TYPE_ERROR_INTEGER, t2, "/\\ /2");
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}

static Term p_or(Term t1, Term t2 USES_REGS) {
  switch (ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      RINT(IntegerOfTerm(t1) | IntegerOfTerm(t2));
    case double_e:
      Yap_ArithError(TYPE_ERROR_INTEGER, t2, "\\/ /2");
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_ior_int_big(IntegerOfTerm(t1), t2);
#endif
    default:
      RERROR();
    }
    break;
  case double_e:
    Yap_ArithError(TYPE_ERROR_INTEGER, t1, "\\/ /2");
  case big_int_e:
#ifdef USE_GMP
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* anding a bignum with an integer is easy */
      return Yap_gmp_ior_int_big(IntegerOfTerm(t2), t1);
    case big_int_e:
      /* two bignums */
      return Yap_gmp_ior_big_big(t1, t2);
    case double_e:
      Yap_ArithError(TYPE_ERROR_INTEGER, t2, "\\/ /2");
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}

static Term p_sll(Term t1, Term t2 USES_REGS) {
  switch (ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      {
        Int i2 = IntegerOfTerm(t2);

        if (i2 <= 0) {
          if (i2 == Int_MIN) {
            Yap_ArithError(RESOURCE_ERROR_HUGE_INT, t2, ">>/2");
          }
          RINT(SLR(IntegerOfTerm(t1), -i2));
        }
        return do_sll(IntegerOfTerm(t1), i2 PASS_REGS);
      }
    case double_e:
      Yap_ArithError(TYPE_ERROR_INTEGER, t2, "<</2");
    case big_int_e:
#ifdef USE_GMP
      Yap_ArithError(RESOURCE_ERROR_HUGE_INT, t2, "<</2");
#endif
    default:
      RERROR();
    }
    break;
  case double_e:
    Yap_ArithError(TYPE_ERROR_INTEGER, t1, "<< /2");
  case big_int_e:
#ifdef USE_GMP
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      return Yap_gmp_sll_big_int(t1, IntegerOfTerm(t2));
    case big_int_e:
      Yap_ArithError(RESOURCE_ERROR_HUGE_INT, t2, ">>/2");
    case double_e:
      Yap_ArithError(TYPE_ERROR_INTEGER, t2, "<</2");
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}

static Term p_slr(Term t1, Term t2 USES_REGS) {
  switch (ETypeOfTerm(t1)) {
  case long_int_e:
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      /* two integers */
      {
        Int i2 = IntegerOfTerm(t2);

        if (i2 < 0) {
          if (i2 == Int_MIN) {
            Yap_ArithError(RESOURCE_ERROR_HUGE_INT, t2, ">>/2");
          }
          return do_sll(IntegerOfTerm(t1), -i2 PASS_REGS);
        }
        RINT(SLR(IntegerOfTerm(t1), i2));
      }
    case double_e:
      Yap_ArithError(TYPE_ERROR_INTEGER, t2, ">>/2");
    case big_int_e:
#ifdef USE_GMP
      Yap_ArithError(RESOURCE_ERROR_HUGE_INT, t2, ">>/2");
#endif
    default:
      RERROR();
    }
    break;
  case double_e:
    Yap_ArithError(TYPE_ERROR_INTEGER, t1, ">>/2");
  case big_int_e:
#ifdef USE_GMP
    switch (ETypeOfTerm(t2)) {
    case long_int_e:
      return Yap_gmp_sll_big_int(t1, -IntegerOfTerm(t2));
    case big_int_e:
      Yap_ArithError(RESOURCE_ERROR_HUGE_INT, t2, ">>/2");
    case double_e:
      Yap_ArithError(TYPE_ERROR_INTEGER, t2, ">>/2");
    default:
      RERROR();
    }
#endif
  default:
    RERROR();
  }
  RERROR();
}
