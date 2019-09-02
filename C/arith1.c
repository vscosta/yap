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
 * File:		arith1.c * Last rev:
 ** mods: * comments:	arithmetical expression evaluation *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/**
   @file arith1.c

   @addtogroup arithmetic_operators

  - <b>exp( _X_) [ISO]</b><p> @anchor exp_1

    Natural exponential.

  - <b>log( _X_) [ISO]</b><p> @anchor log_1

    Natural logarithm.

  - <b>log10( _X_)</b><p> @anchor log10_1

    Decimal logarithm.

  - <b>sqrt( _X_) [ISO]</b><p> @anchor sqrt_1

    Square root.

  - <b>sin( _X_) [ISO]</b><p> @anchor sin_1

    Sine.

  - <b>cos( _X_) [ISO]</b><p> @anchor cos_1

    Cosine.

  - <b>tan( _X_) [ISO]</b><p> @anchor tan_1

    Tangent.

  - <b>asin( _X_) [ISO]</b><p> @anchor asin_1

    Arc sine.

  - <b>acos( _X_) [ISO]</b><p> @anchor acos_1

    Arc cosine.

  - <b>atan( _X_) [ISO]</b><p> @anchor atan_1

    Arc tangent.

  - <b>sinh( _X_)</b><p> @anchor sinh_1

    Hyperbolic sine.

  - <b>cosh( _X_)</b><p> @anchor cosh_1

    Hyperbolic cosine.

  - <b>tanh( _X_)</b><p> @anchor tanh_1

    Hyperbolic tangent.

  - <b>asinh( _X_)</b><p> @anchor asinh_1

    Hyperbolic arc sine.

  - <b>acosh( _X_)</b><p> @anchor acosh_1

    Hyperbolic arc cosine.

  - <b>atanh( _X_)</b><p> @anchor atanh_1

    Hyperbolic arc tangent.

  - <b>lgamma( _X_)</b><p> @anchor lgamma_1

    Logarithm of gamma function.

  - <b>erf( _X_)</b><p> @anchor erf_1

    Gaussian error function.

  - <b>erfc( _X_)</b><p> @anchor erfc_1

    Complementary gaussian error function.

  - <b>random( _X_) [ISO]</b><p> @anchor random_1_op

    An integer random number between 0 and  _X_.

    In `iso` language mode the argument must be a floating
    point-number, the result is an integer and it the float is equidistant
    it is rounded up, that is, to the least integer greater than  _X_.

  - <b>integer( _X_)</b><p> @anchor integer_1_op

    If  _X_ evaluates to a float, the integer between the value of  _X_ and 0
closest to the value of  _X_, else if  _X_ evaluates to an integer, the value of
_X_.

  - <b>float( _X_) [ISO]</b><p> @anchor float_1_op

    If  _X_ evaluates to an integer, the corresponding float, else the float
itself.

  - <b>float_fractional_part( _X_) [ISO]</b><p> @anchor float_fractional_part_1

    The fractional part of the floating point number  _X_, or `0.0` if  _X_ is
an integer. In the `iso` language mode,  _X_ must be an integer.

  - <b>float_integer_part( _X_) [ISO]</b><p> @anchor float_integer_part_1

    The float giving the integer part of the floating point number  _X_, or  _X_
if  _X_ is an integer. In the `iso` language mode,  _X_ must be an integer.

  - <b>abs( _X_) [ISO]</b><p> @anchor abs_1

    The absolute value of  _X_.

  - <b>ceiling( _X_) [ISO]</b><p> @anchor ceiling_1

    The integer that is the smallest integral value not smaller than  _X_.

    In `iso` language mode the argument must be a floating point-number and the
result is an integer.

  - <b>floor( _X_) [ISO]</b><p> @anchor floor_1

    The integer that is the greatest integral value not greater than  _X_.

    In `iso` language mode the argument must be a floating
    point-number and the result is an integer.

  - <b>round( _X_) [ISO]</b><p> @anchor round_1

    The nearest integral value to  _X_. If  _X_ is equidistant to two integers,
it will be rounded to the closest even integral value.

    In `iso` language mode the argument must be a floating point-number, the
result is an integer and it the float is equidistant it is rounded up, that is,
to the least integer greater than  _X_.

  - <b>sign( _X_) [ISO]</b><p> @anchor sign_1

    Return 1 if the  _X_ evaluates to a positive integer, 0 it if evaluates to
0, and -1 if it evaluates to a negative integer. If  _X_ evaluates to a
floating-point number return 1.0 for a positive  _X_, 0.0 for 0.0, and -1.0
otherwise.

  - <b>truncate( _X_) [ISO]</b><p> @anchor truncate_1

    The integral value between  _X_ and 0 closest to _X_.

  - <b>rational( _X_)</b><p> @anchor rational_1_op

    Convert the expression  _X_ to a rational number or integer. The function
returns the input on integers and rational numbers. For floating point numbers,
the returned rational number exactly represents the float. As floats cannot
exactly represent all decimal numbers the results may be surprising. In the
examples below, doubles can represent `0.25` and the result is as expected, in
contrast to the result of `rational(0.1)`. The function `rationalize/1` gives a
more intuitive result.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prolog
?- A is rational(0.25).

A is 1 rdiv 4
?- A is rational(0.1).
A = 3602879701896397 rdiv 36028797018963968
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  - <b>rationalize( _X_)</b><p> @anchor rationalize_1

    Convert the expression _X_ to a rational number or integer. The function is
    similar to [rational/1](@ref rational_1), but the result is only accurate
within the rounding error of floating point numbers, generally producing a much
    smaller denominator.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prolog
?- A is rationalize(0.25).

A = 1 rdiv 4
?- A is rationalize(0.1).

A = 1 rdiv 10
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  - <b>\\  _X_ [ISO]</b><p>

    Integer bitwise negation.

  - <b>msb( _X_)</b><p> @anchor msb_1

    The most significant bit of the non-negative integer  _X_.

  - <b>lsb( _X_)</b><p> @anchor lsb_1

    The least significant bit of the non-negative integer  _X_.

  - <b>popcount( _X_)</b><p> @anchor popcount_1

    The number of bits set to `1` in the binary representation of the
non-negative integer  _X_.

  - <b>[ _X_]</b><p>

    Evaluates to  _X_ for expression  _X_. Useful because character
strings in Prolog are lists of character codes.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
X is Y*10+C-"0"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is the same as

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
X is Y*10+C-[48].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
which would be evaluated as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
X is Y*10+C-48.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*/

#include "Yap.h"
#include "YapEval.h"
#include "YapHeap.h"
#include "Yatom.h"

static Term float_to_int(Float v USES_REGS) {
#if USE_GMP
  Int i = (Int)v;

  if (i - v == 0.0) {
    return MkIntegerTerm(i);
  } else {
    return Yap_gmp_float_to_big(v);
  }
#else
  return MkIntegerTerm(v);
#endif
}

#define RBIG_FL(v) return (float_to_int(v PASS_REGS))

typedef struct init_un_eval {
  char *OpName;
  arith1_op f;
} InitUnEntry;

/* Some compilers just don't get it */

#ifdef __MINGW32__
#undef HAVE_ASINH
#undef HAVE_ACOSH
#undef HAVE_ATANH
#undef HAVE_FINITE
#endif

#if !HAVE_ASINH
#define asinh(F) (log((F) + sqrt((F) * (F) + 1)))
#endif
#if !HAVE_ACOSH
#define acosh(F) (log((F) + sqrt((F) * (F)-1)))
#endif
#if !HAVE_ATANH
#define atanh(F) (log((1 + (F)) / (1 - (F))) / 2)
#endif

static inline Float get_float(Term t) {
  if (IsFloatTerm(t)) {
    return FloatOfTerm(t);
  }
  if (IsIntTerm(t)) {
    return IntOfTerm(t);
  }
  if (IsLongIntTerm(t)) {
    return LongIntOfTerm(t);
  }
#ifdef USE_GMP
  if (IsBigIntTerm(t)) {
    return Yap_gmp_to_float(t);
  }
#endif
  return 0.0;
}

/* WIN32 machines do not necessarily have rint. This will do for now */
#if HAVE_RINT
#define my_rint(X) rint(X)
#else
static double my_rint(double x) {
  double y, z;
  Int n;

  if (x >= 0) {
    y = x + 0.5;
    z = floor(y);
    n = (Int)z;
    if (y == z && n % 2)
      return (z - 1);
  } else {
    y = x - 0.5;
    z = ceil(y);
    n = (Int)z;
    if (y == z && n % 2)
      return (z + 1);
  }
  return (z);
}
#endif

static Int
msb(Int inp USES_REGS) /* calculate the most significant bit for an integer */
{
  /* the obvious solution: do it by using binary search */
  Int out = 0;

  if (inp < 0) {
    Yap_ArithError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, MkIntegerTerm(inp),
                   "msb/1 received %d", inp);
  }

#if HAVE__BUILTIN_FFSLL
  out = __builtin_ffsll(inp);
#elif HAVE_FFSLL
  out = ffsll(inp);
#else
  if (inp == 0)
    return 0L;
#if SIZEOF_INT_P == 8
  if (inp & ((CELL)0xffffffffLL << 32)) {
    inp >>= 32;
    out += 32;
  }
#endif
  if (inp & ((CELL)0xffffL << 16)) {
    inp >>= 16;
    out += 16;
  }
  if (inp & ((CELL)0xffL << 8)) {
    inp >>= 8;
    out += 8;
  }
  if (inp & ((CELL)0xfL << 4)) {
    inp >>= 4;
    out += 4;
  }
  if (inp & ((CELL)0x3L << 2)) {
    inp >>= 2;
    out += 2;
  }
  if (inp & ((CELL)0x1 << 1))
    out++;
#endif
  return out;
}

Int Yap_msb(
    Int inp USES_REGS) /* calculate the most significant bit for an integer */
{
  return msb(inp PASS_REGS);
}

static Int
lsb(Int inp USES_REGS) /* calculate the least significant bit for an integer */
{
  /* the obvious solution: do it by using binary search */
  Int out = 0;

  if (inp < 0) {
    Yap_ArithError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, MkIntegerTerm(inp),
                   "msb/1 received %d", inp);
  }
  if (inp == 0)
    return 0L;
#if SIZEOF_INT_P == 8
  if (!(inp & (CELL)0xffffffffLL)) {
    inp >>= 32;
    out += 32;
  }
#endif
  if (!(inp & (CELL)0xffffL)) {
    inp >>= 16;
    out += 16;
  }
  if (!(inp & (CELL)0xffL)) {
    inp >>= 8;
    out += 8;
  }
  if (!(inp & (CELL)0xfL)) {
    inp >>= 4;
    out += 4;
  }
  if (!(inp & (CELL)0x3L)) {
    inp >>= 2;
    out += 2;
  }
  if (!(inp & ((CELL)0x1)))
    out++;

  return out;
}

static Int popcount(
    Int inp USES_REGS) /* calculate the least significant bit for an integer */
{
  /* the obvious solution: do it by using binary search */
  Int c = 0, j = 0, m = ((CELL)1);

  if (inp < 0) {
    Yap_ArithError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, MkIntegerTerm(inp),
                   "popcount/1 received %d", inp);
  }
  if (inp == 0)
    return 0L;
  for (j = 0, c = 0; j < sizeof(inp) * 8; j++, m <<= 1) {
    if (inp & m)
      c++;
  }

  return c;
}

static Term eval1(Int fi, Term t USES_REGS) {
  arith1_op f = fi;
  switch (f) {
  case op_uplus:
    return t;
  case op_uminus:
    switch (ETypeOfTerm(t)) {
    case long_int_e: {
#ifdef USE_GMP
      Int i = IntegerOfTerm(t);

      if (i == Int_MIN) {
        return Yap_gmp_neg_int(i);
      } else
#endif
        RINT(-IntegerOfTerm(t));
    }
    case double_e:
      RFLOAT(-FloatOfTerm(t));
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_neg_big(t);
#endif
    default:
      RERROR();
    }
  case op_unot:
    switch (ETypeOfTerm(t)) {
    case long_int_e:
      RINT(~IntegerOfTerm(t));
    case double_e:
      Yap_ArithError(TYPE_ERROR_INTEGER, t, "\\(%f)", FloatOfTerm(t));
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_unot_big(t);
#endif
    default:
      RERROR();
    }
  case op_exp:
    RFLOAT(exp(get_float(t)));
  case op_log: {
    Float dbl = get_float(t);
    if (dbl >= 0) {
      RFLOAT(log(dbl));
    } else {
      Yap_ArithError(EVALUATION_ERROR_UNDEFINED, t, "log(%f)", dbl);
    }
  }
  case op_log10: {
    Float dbl = get_float(t);
    if (dbl >= 0) {
      RFLOAT(log10(dbl));
    } else {
      Yap_ArithError(EVALUATION_ERROR_UNDEFINED, t, "log10(%f)", dbl);
    }
  }
  case op_sqrt: {
    Float dbl = get_float(t), out;
    out = sqrt(dbl);
#if HAVE_ISNAN
    if (isnan(out)) {
      Yap_ArithError(EVALUATION_ERROR_UNDEFINED, t, "sqrt(%f)", dbl);
    }
#endif
    RFLOAT(out);
  }
  case op_sin: {
    Float dbl = get_float(t), out;
    out = sin(dbl);
    RFLOAT(out);
  }
  case op_cos: {
    Float dbl = get_float(t), out;
    out = cos(dbl);
    RFLOAT(out);
  }
  case op_tan: {
    Float dbl = get_float(t), out;
    out = tan(dbl);
    RFLOAT(out);
  }
  case op_sinh: {
    Float dbl = get_float(t), out;
    out = sinh(dbl);
    RFLOAT(out);
  }
  case op_cosh: {
    Float dbl = get_float(t), out;
    out = cosh(dbl);
    RFLOAT(out);
  }
  case op_tanh: {
    Float dbl = get_float(t), out;
    out = tanh(dbl);
    RFLOAT(out);
  }
  case op_asin: {
    Float dbl, out;

    dbl = get_float(t);
    out = asin(dbl);
#if HAVE_ISNAN
    if (isnan(out)) {
      Yap_ArithError(EVALUATION_ERROR_UNDEFINED, t, "asin(%f)", dbl);
    }
#endif
    RFLOAT(out);
  }
  case op_acos: {
    Float dbl, out;

    dbl = get_float(t);
    out = acos(dbl);
#if HAVE_ISNAN
    if (isnan(out)) {
      Yap_ArithError(EVALUATION_ERROR_UNDEFINED, t, "acos(%f)", dbl);
    }
#endif
    RFLOAT(out);
  }
  case op_atan: {
    Float dbl, out;

    dbl = get_float(t);
    out = atan(dbl);
#if HAVE_ISNAN
    if (isnan(out)) {
      Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "atan(%f)", dbl);
    }
#endif
    RFLOAT(out);
  }
  case op_asinh: {
    Float dbl, out;

    dbl = get_float(t);
    out = asinh(dbl);
#if HAVE_ISNAN
    if (isnan(out)) {
      Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "asinh(%f)", dbl);
    }
#endif
    RFLOAT(out);
  }
  case op_acosh: {
    Float dbl, out;

    dbl = get_float(t);
    out = acosh(dbl);
#if HAVE_ISNAN
    if (isnan(out)) {
      Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "acosh(%f)", dbl);
    }
#endif
    RFLOAT(out);
  }
  case op_atanh: {
    Float dbl, out;

    dbl = get_float(t);
    out = atanh(dbl);
#if HAVE_ISNAN
    if (isnan(out)) {
      Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "atanh(%f)", dbl);
    }
#endif
    RFLOAT(out);
  }
  case op_lgamma: {
#if HAVE_LGAMMA
    Float dbl;

    dbl = get_float(t);
    RFLOAT(lgamma(dbl));
#else
    RERROR();
#endif
  }
  case op_erf: {
#if HAVE_ERF
    Float dbl = get_float(t), out;
    out = erf(dbl);
    RFLOAT(out);
#else
    RERROR();
#endif
  }
  case op_erfc: {
#if HAVE_ERF
    Float dbl = get_float(t), out;
    out = erfc(dbl);
    RFLOAT(out);
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
  case op_floor: {
    Float dbl;

    switch (ETypeOfTerm(t)) {
    case long_int_e:
      return t;
    case double_e:
      dbl = FloatOfTerm(t);
      break;
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_floor(t);
#endif
    default:
      RERROR();
    }
#if HAVE_ISNAN
    if (isnan(dbl)) {
      Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "integer(%f)", dbl);
    }
#endif
#if HAVE_ISINF
    if (isinf(dbl)) {
      Yap_ArithError(EVALUATION_ERROR_INT_OVERFLOW, MkFloatTerm(dbl), "integer\
(%f)",
                     dbl);
    }
#endif
    RBIG_FL(floor(dbl));
  }
  case op_ceiling: {
    Float dbl;
    switch (ETypeOfTerm(t)) {
    case long_int_e:
      return t;
    case double_e:
      dbl = FloatOfTerm(t);
      break;
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_ceiling(t);
#endif
    default:
      RERROR();
    }
#if HAVE_ISNAN
    if (isnan(dbl)) {
      Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "integer(%f)", dbl);
    }
#endif
#if HAVE_ISINF
    if (isinf(dbl)) {
      Yap_ArithError(EVALUATION_ERROR_INT_OVERFLOW, MkFloatTerm(dbl), "integer\
(%f)",
                     dbl);
    }
#endif
    RBIG_FL(ceil(dbl));
  }
  case op_round: {
    Float dbl;

    switch (ETypeOfTerm(t)) {
    case long_int_e:
      return t;
    case double_e:
      dbl = FloatOfTerm(t);
      break;
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_round(t);
#endif
    default:
      RERROR();
    }
#if HAVE_ISNAN
    if (isnan(dbl)) {
      Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "integer(%f)", dbl);
    }
#endif
#if HAVE_ISINF
    if (isinf(dbl)) {
      Yap_ArithError(EVALUATION_ERROR_INT_OVERFLOW, MkFloatTerm(dbl), "integer\
(%f)",
                     dbl);
    }
#endif
    RBIG_FL(my_rint(dbl));
  }
  case op_truncate:
  case op_integer: {
    Float dbl;
    switch (ETypeOfTerm(t)) {
    case long_int_e:
      return t;
    case double_e:
      dbl = FloatOfTerm(t);
      break;
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_trunc(t);
#endif
    default:
      RERROR();
    }
#if HAVE_ISNAN
    if (isnan(dbl)) {
      Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "integer(%f)", dbl);
    }
#endif
#if HAVE_ISINF
    if (isinf(dbl)) {
      Yap_ArithError(EVALUATION_ERROR_INT_OVERFLOW, MkFloatTerm(dbl),
                     "integer (%f)", dbl);
    }
#endif
    if (dbl < 0.0)
      RBIG_FL(ceil(dbl));
    else
      RBIG_FL(floor(dbl));
  }
  case op_float:
    switch (ETypeOfTerm(t)) {
    case long_int_e:
      RFLOAT(IntegerOfTerm(t));
    case double_e:
      return t;
    case big_int_e:
#ifdef USE_GMP
      RFLOAT(Yap_gmp_to_float(t));
#endif
    default:
      RERROR();
    }
  case op_rational:
    switch (ETypeOfTerm(t)) {
    case long_int_e:
      return t;
#ifdef USE_GMP
    case double_e:
      return Yap_gmp_float_to_rational(FloatOfTerm(t));
#endif
    case big_int_e:
      return t;
    default:
      RERROR();
    }
  case op_rationalize:
    switch (ETypeOfTerm(t)) {
    case long_int_e:
      return t;
#ifdef USE_GMP
    case double_e:
      return Yap_gmp_float_rationalize(FloatOfTerm(t));
#endif
    case big_int_e:
      return t;
    default:
      RERROR();
    }
  case op_abs:
    switch (ETypeOfTerm(t)) {
    case long_int_e:
      RINT(labs(IntegerOfTerm(t)));
    case double_e:
      RFLOAT(fabs(FloatOfTerm(t)));
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_abs_big(t);
#endif
    default:
      RERROR();
    }
  case op_msb:
    switch (ETypeOfTerm(t)) {
    case long_int_e:
      RINT(msb(IntegerOfTerm(t) PASS_REGS));
    case double_e:
      Yap_ArithError(TYPE_ERROR_INTEGER, t, "msb(%f)", FloatOfTerm(t));
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_msb(t);
#endif
    default:
      RERROR();
    }
  case op_lsb:
    switch (ETypeOfTerm(t)) {
    case long_int_e:
      RINT(lsb(IntegerOfTerm(t) PASS_REGS));
    case double_e:
      Yap_ArithError(TYPE_ERROR_INTEGER, t, "lsb(%f)", FloatOfTerm(t));
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_lsb(t);
#endif
    default:
      RERROR();
    }
  case op_popcount:
    switch (ETypeOfTerm(t)) {
    case long_int_e:
      RINT(popcount(IntegerOfTerm(t) PASS_REGS));
    case double_e:
      Yap_ArithError(TYPE_ERROR_INTEGER, t, "popcount(%f)", FloatOfTerm(t));
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_popcount(t);
#endif
    default:
      RERROR();
    }
  case op_ffracp:
    switch (ETypeOfTerm(t)) {
    case long_int_e:
      if (isoLanguageFlag()) { /* iso */
        Yap_ArithError(TYPE_ERROR_FLOAT, t, "X is float_fractional_part(%f)",
                       IntegerOfTerm(t));
      } else {
        RFLOAT(0.0);
      }
    case double_e: {
      Float dbl;
      dbl = FloatOfTerm(t);
      RFLOAT(dbl - ceil(dbl));
    } break;
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_float_fractional_part(t);
#endif
    default:
      RERROR();
    }
  case op_fintp:
    switch (ETypeOfTerm(t)) {
    case long_int_e:
      Yap_ArithError(TYPE_ERROR_FLOAT, t, "X is float_integer_part(%f)",
                     IntegerOfTerm(t));
    case double_e:
      RFLOAT(rint(FloatOfTerm(t)));
      break;
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_float_integer_part(t);
#endif
    default:
      RERROR();
    }
  case op_sign:
    switch (ETypeOfTerm(t)) {
    case long_int_e: {
      Int x = IntegerOfTerm(t);

      RINT((x > 0 ? 1 : (x < 0 ? -1 : 0)));
    }
    case double_e: {

      Float dbl = FloatOfTerm(t);

      RINT((dbl > 0.0 ? 1 : (dbl < 0.0 ? -1 : 0)));
    }
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_sign(t);
#endif
    default:
      RERROR();
    }
  case op_random1:
    switch (ETypeOfTerm(t)) {
    case long_int_e:
      RINT(Yap_random() * IntegerOfTerm(t));
    case double_e:
      Yap_ArithError(TYPE_ERROR_INTEGER, t, "random(%f)", FloatOfTerm(t));
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_mul_float_big(Yap_random(), t);
#endif
    default:
      RERROR();
    }
  }
  /// end of switch
  RERROR();
}

Term Yap_eval_unary(Int f, Term t) {
  CACHE_REGS
  return eval1(f, t PASS_REGS);
}

static InitUnEntry InitUnTab[] = {{"+", op_uplus},
                                  {"-", op_uminus},
                                  {"\\", op_unot},
                                  {"exp", op_exp},
                                  {"log", op_log},
                                  {"log10", op_log10},
                                  {"sqrt", op_sqrt},
                                  {"sin", op_sin},
                                  {"cos", op_cos},
                                  {"tan", op_tan},
                                  {"sinh", op_sinh},
                                  {"cosh", op_cosh},
                                  {"tanh", op_tanh},
                                  {"asin", op_asin},
                                  {"acos", op_acos},
                                  {"atan", op_atan},
                                  {"asinh", op_asinh},
                                  {"acosh", op_acosh},
                                  {"atanh", op_atanh},
                                  {"floor", op_floor},
                                  {"ceiling", op_ceiling},
                                  {"round", op_round},
                                  {"truncate", op_truncate},
                                  {"integer", op_integer},
                                  {"float", op_float},
                                  {"abs", op_abs},
                                  {"msb", op_msb},
                                  {"lsb", op_lsb},
                                  {"popcount", op_popcount},
                                  {"float_fractional_part", op_ffracp},
                                  {"float_integer_part", op_fintp},
                                  {"sign", op_sign},
                                  {"lgamma", op_lgamma},
                                  {"erf", op_erf},
                                  {"erfc", op_erfc},
                                  {"rational", op_rational},
                                  {"rationalize", op_rationalize},
                                  {"random", op_random1}};

Atom Yap_NameOfUnaryOp(int i) { return Yap_LookupAtom(InitUnTab[i].OpName); }

static Int p_unary_is(USES_REGS1) { /* X is Y	 */
  Term t = Deref(ARG2);
  Term top;
  bool go;

  if (IsVarTerm(t)) {
    Yap_EvalError(INSTANTIATION_ERROR, t, "unbound unary operator");
    return FALSE;
  }
  Yap_ClearExs();
  top = Yap_Eval(Deref(ARG3));
  if (IsIntTerm(t)) {
    Term tout;
    Int i;

    i = IntegerOfTerm(t);
    tout = eval1(i, top PASS_REGS);
    return Yap_unify_constant(ARG1, tout);
  } else if (IsAtomTerm(t)) {
    Atom name = AtomOfTerm(t);
    ExpEntry *p;
    Term out;

    if (EndOfPAEntr(p = RepExpProp(Yap_GetExpProp(name, 1)))) {
      Yap_EvalError(TYPE_ERROR_EVALUABLE, takeIndicator(t),
                    "functor %s/1 for arithmetic expression",
                    RepAtom(name)->StrOfAE);
      return FALSE;
    }
    do {
      out = eval1(p->FOfEE, top PASS_REGS);
      go = Yap_CheckArithError();
    } while (go);
    return Yap_unify_constant(ARG1, out);
  }
  return false;
}

static Int p_unary_op_as_integer(USES_REGS1) { /* X is Y	 */
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    Yap_EvalError(INSTANTIATION_ERROR, t, "X is _Y");
    return (FALSE);
  }
  if (IsIntTerm(t)) {
    return Yap_unify_constant(ARG2, t);
  }
  if (IsAtomTerm(t)) {
    Atom name = AtomOfTerm(t);
    ExpEntry *p;

    if (EndOfPAEntr(p = RepExpProp(Yap_GetExpProp(name, 1)))) {
      return Yap_unify(ARG1, ARG2);
    }
    return Yap_unify_constant(ARG2, MkIntTerm(p->FOfEE));
  }
  return (FALSE);
}

void Yap_InitUnaryExps(void) {
  unsigned int i;
  ExpEntry *p;

  for (i = 0; i < sizeof(InitUnTab) / sizeof(InitUnEntry); ++i) {
    AtomEntry *ae = RepAtom(Yap_LookupAtom(InitUnTab[i].OpName));
    if (ae == NULL) {
      Yap_EvalError(RESOURCE_ERROR_HEAP, TermNil, "at InitUnaryExps");
      return;
    }
    WRITE_LOCK(ae->ARWLock);
    if (Yap_GetExpPropHavingLock(ae, 1)) {
      WRITE_UNLOCK(ae->ARWLock);
      break;
    }
    p = (ExpEntry *)Yap_AllocAtomSpace(sizeof(ExpEntry));
    p->KindOfPE = ExpProperty;
    p->ArityOfEE = 1;
    p->ENoOfEE = 1;
    p->FOfEE = InitUnTab[i].f;
    AddPropToAtom(ae, (PropEntry *)p);
    WRITE_UNLOCK(ae->ARWLock);
  }
  Yap_InitCPred("is", 3, p_unary_is, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$unary_op_as_integer", 2, p_unary_op_as_integer,
                TestPredFlag | SafePredFlag);
}

/* This routine is called from Restore to make sure we have the same arithmetic
 * operators */
int Yap_ReInitUnaryExps(void) { return TRUE; }
