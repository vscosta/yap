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
#include "YapHeap.h"
#include "eval.h"

static Term
float_to_int(Float v)
{
#if  USE_GMP
  Int i = (Int)v;

  if (i-v == 0.0) {
    return MkIntegerTerm(i);
  } else {
    return Yap_gmp_float_to_big(v);
  }
#else
  return MkIntegerTerm(v);
#endif
}

#define RBIG_FL(v)  return(float_to_int(v))

typedef struct init_un_eval {
  char          *OpName;
  arith1_op      f;
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


static inline Float
get_float(Term t) {
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
static
double my_rint(double x)
{ 
  double y, z; 
  Int n; 
  
  if (x >= 0) { 
    y = x + 0.5; 
    z = floor(y); 
    n = (Int) z; 
    if (y == z && n % 2)
      return(z-1); 
  } else { 
    y = x - 0.5; 
    z = ceil(y); 
    n = (Int) z; 
    if (y == z && n % 2)
      return(z+1); 
  }
  return(z); 
}
#endif

static Int
msb(Int inp)	/* calculate the most significant bit for an integer */
{
  /* the obvious solution: do it by using binary search */
  Int out = 0;
  int off = sizeof(CELL)*4;

  if (inp < 0) {
    return Yap_ArithError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, MkIntegerTerm(inp),
	      "msb/1 received %d", inp);
  }

  while (off) {
    Int limit = ((CELL)1) << (off);
    if (inp >= limit) {
      out += off;
      inp >>= off;
    }
    off >>= 1;
  }
  return(out);
}

static Int
lsb(Int inp)	/* calculate the least significant bit for an integer */
{
  /* the obvious solution: do it by using binary search */
  Int out = 0;

  if (inp < 0) {
    return Yap_ArithError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, MkIntegerTerm(inp),
	      "msb/1 received %d", inp);
  }
  if (inp==0)
    return 0L;
#if SIZEOF_LONG_INT == 8
  if (!(inp & 0xffffffffLL)) {inp >>= 32; out += 32;}
#endif
  if (!(inp &     0xffffL)) {inp >>= 16; out += 16;}
  if (!(inp &       0xffL)) {inp >>=  8; out +=  8;}
  if (!(inp &   0xfL)) {inp >>=  4; out +=  4;}
  if (!(inp &        0x3L)) {inp >>=  2; out +=  2;}
  if (!(inp &        ((CELL)0x1))) out++;

  return out;
}

static Int
popcount(Int inp)	/* calculate the least significant bit for an integer */
{
  /* the obvious solution: do it by using binary search */
  Int c = 0, j = 0, m = ((CELL)1);

  if (inp < 0) {
    return Yap_ArithError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, MkIntegerTerm(inp),
	      "popcount/1 received %d", inp);
  }
  if (inp==0)
    return 0L;
  for(j=0,c=0; j<sizeof(inp)*8; j++, m<<=1)
    { if ( inp&m )
	c++;
    }

  return c;
}

static Term
eval1(Int fi, Term t) {
  arith1_op f = fi;
  switch (f) {
  case op_uplus:
    return t;
  case op_uminus:
    switch (ETypeOfTerm(t)) {
    case long_int_e:
      {
#ifdef USE_GMP
	Int i = IntegerOfTerm(t);
      
	if (i == Int_MIN) {
	  return Yap_gmp_neg_int(i);
	}
	else
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
      return Yap_ArithError(TYPE_ERROR_INTEGER, t, "\\(%f)", FloatOfTerm(t));
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_unot_big(t);
#endif
    default:
      RERROR();
    }
  case op_exp:
    RFLOAT(exp(get_float(t)));
  case op_log:
    {
      Float dbl = get_float(t);
      if (dbl >= 0) {
	RFLOAT(log(dbl));
      } else {
	return Yap_ArithError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t, "log(%f)", dbl);
      }
    }
  case op_log10:
    {
      Float dbl = get_float(t);
      if (dbl >= 0) {
	RFLOAT(log10(dbl));
      } else {
	return Yap_ArithError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t, "log(%f)", dbl);
      }
    }
  case op_sqrt:
    {
      Float dbl = get_float(t), out;
      out = sqrt(dbl);
#if HAVE_ISNAN
      if (isnan(out)) {
	return Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "acos(%f)", dbl);
      }
#endif
      RFLOAT(out);
    }
  case op_sin:
    {
      Float dbl = get_float(t), out;
      out = sin(dbl);
      RFLOAT(out);
    }
  case op_cos:
    {
      Float dbl = get_float(t), out;
      out = cos(dbl);
      RFLOAT(out);
    }
  case op_tan:
    {
      Float dbl = get_float(t), out;
      out = tan(dbl);
      RFLOAT(out);
    }
  case op_sinh:
    {
      Float dbl = get_float(t), out;
      out = sinh(dbl);
      RFLOAT(out);
    }
  case op_cosh:
    {
      Float dbl = get_float(t), out;
      out = cosh(dbl);
      RFLOAT(out);
    }
  case op_tanh:
    {
      Float dbl = get_float(t), out;
      out = tanh(dbl);
      RFLOAT(out);
    }
  case op_asin:
    {
      Float dbl, out;

      dbl = get_float(t);
      out = asin(dbl);
#if HAVE_ISNAN
      if (isnan(out)) {
	return Yap_ArithError(EVALUATION_ERROR_UNDEFINED, t, "asin(%f)", dbl);
      }
#endif
      RFLOAT(out);
    }
  case op_acos:
    {
      Float dbl, out;

      dbl = get_float(t);
      out = acos(dbl);
#if HAVE_ISNAN
      if (isnan(out)) {
	return Yap_ArithError(EVALUATION_ERROR_UNDEFINED, t, "acos(%f)", dbl);
      }
#endif
      RFLOAT(out);
    }
  case op_atan:
    {
      Float dbl, out;

      dbl = get_float(t);
      out = atan(dbl);
#if HAVE_ISNAN
      if (isnan(out)) {
	return Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "atanh(%f)", dbl);
      }
#endif
      RFLOAT(out);
    }
  case op_asinh:
    {
      Float dbl, out;

      dbl = get_float(t);
      out = asinh(dbl);
#if HAVE_ISNAN
      if (isnan(out)) {
	return Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "atanh(%f)", dbl);
      }
#endif
      RFLOAT(out);
    }
  case op_acosh:
    {
      Float dbl, out;

      dbl = get_float(t);
      out = acosh(dbl);
#if HAVE_ISNAN
      if (isnan(out)) {
	return Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "atanh(%f)", dbl);
      }
#endif
      RFLOAT(out);
    }
  case op_atanh:
    {
      Float dbl, out;

      dbl = get_float(t);
      out = atanh(dbl);
#if HAVE_ISNAN
      if (isnan(out)) {
	return Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "atanh(%f)", dbl);
      }
#endif
      RFLOAT(out);
    }
  case op_lgamma:
    {
      Float dbl;

      dbl = get_float(t);
#if HAVE_LGAMMA
      RFLOAT(lgamma(dbl));
#else
      RERROR();
#endif
    }
 case op_erf:
   {
     Float dbl = get_float(t), out;
#if HAVE_ERF
     out = erf(dbl);
     RFLOAT(out);
#else
     RERROR();
#endif
   }
 case op_erfc:
   {
     Float dbl = get_float(t), out;
#if HAVE_ERF
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
  case op_floor:
    {
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
	return Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "integer(%f)", dbl);
      }
#endif
#if HAVE_ISINF
      if (isinf(dbl)) {
	return Yap_ArithError(EVALUATION_ERROR_INT_OVERFLOW, MkFloatTerm(dbl), "integer\
(%f)",dbl);
      }
#endif
      RBIG_FL(floor(dbl));
    }
  case op_ceiling:
    {
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
	return Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "integer(%f)", dbl);
      }
#endif
#if HAVE_ISINF
      if (isinf(dbl)) {
	return Yap_ArithError(EVALUATION_ERROR_INT_OVERFLOW, MkFloatTerm(dbl), "integer\
(%f)",dbl);
      }
#endif
      RBIG_FL(ceil(dbl));
    }
  case op_round:
    {
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
	return Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "integer(%f)", dbl);
      }
#endif
#if HAVE_ISINF
      if (isinf(dbl)) {
	return Yap_ArithError(EVALUATION_ERROR_INT_OVERFLOW, MkFloatTerm(dbl), "integer\
(%f)",dbl);
      }
#endif
      RBIG_FL(my_rint(dbl));
    }
  case op_truncate:
  case op_integer:
    {
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
	return Yap_ArithError(DOMAIN_ERROR_OUT_OF_RANGE, t, "integer(%f)", dbl);
      }
#endif
#if HAVE_ISINF
      if (isinf(dbl)) {
	return Yap_ArithError(EVALUATION_ERROR_INT_OVERFLOW, MkFloatTerm(dbl), "integer\
(%f)",dbl);
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
      RINT(msb(IntegerOfTerm(t)));
    case double_e:
      return Yap_ArithError(TYPE_ERROR_INTEGER, t, "msb(%f)", FloatOfTerm(t));
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
      RINT(lsb(IntegerOfTerm(t)));
    case double_e:
      return Yap_ArithError(TYPE_ERROR_INTEGER, t, "lsb(%f)", FloatOfTerm(t));
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
      RINT(popcount(IntegerOfTerm(t)));
    case double_e:
      return Yap_ArithError(TYPE_ERROR_INTEGER, t, "popcount(%f)", FloatOfTerm(t));
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
      if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
	return Yap_ArithError(TYPE_ERROR_FLOAT, t, "X is float_fractional_part(%f)", IntegerOfTerm(t));
      } else {
	RFLOAT(0.0);
      }
    case double_e:
      {
	Float dbl;
	dbl = FloatOfTerm(t);
	RFLOAT(dbl-ceil(dbl));
      }
      break;
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
      return Yap_ArithError(TYPE_ERROR_FLOAT, t, "X is float_integer_part(%f)", IntegerOfTerm(t));
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
    case long_int_e:
      {
	Int x = IntegerOfTerm(t);

	RINT((x > 0 ? 1 : (x < 0 ? -1 : 0)));
      }
    case double_e:
      {

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
      RINT(Yap_random()*IntegerOfTerm(t));
    case double_e:
      return Yap_ArithError(TYPE_ERROR_INTEGER, t, "random(%f)", FloatOfTerm(t));
    case big_int_e:
#ifdef USE_GMP
      return Yap_gmp_mul_float_big(Yap_random(), t);
#endif
    default:
      RERROR();
    }
  }
  RERROR();
}

Term Yap_eval_unary(Int f, Term t)
{
  return eval1(f,t);
}

static InitUnEntry InitUnTab[] = {
  {"+", op_uplus},
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
  {"erf",op_erf},
  {"erfc",op_erfc},
  {"rational",op_rational},
  {"rationalize",op_rationalize},
  {"random", op_random1}
};

static Int 
p_unary_is( USES_REGS1 )
{				/* X is Y	 */
  Term t = Deref(ARG2);
  Term top;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, ARG2, "X is Y");
    return FALSE;
  }
  top = Yap_Eval(Deref(ARG3));
  if (!Yap_FoundArithError(top, ARG3)) {
    return FALSE;
  }
  if (IsIntTerm(t)) {
    Term tout = Yap_FoundArithError(eval1(IntegerOfTerm(t), top), Deref(ARG3));
    if (!tout)
      return FALSE;
    return Yap_unify_constant(ARG1,tout);
  }
  if (IsAtomTerm(t)) {
    Atom name = AtomOfTerm(t);
    ExpEntry *p;
    Term out;

    if (EndOfPAEntr(p = RepExpProp(Yap_GetExpProp(name, 1)))) {
      Term ti[2];

      /* error */
      ti[0] = t;
      ti[1] = MkIntTerm(1);
      t = Yap_MkApplTerm(FunctorSlash, 2, ti);
      Yap_Error(TYPE_ERROR_EVALUABLE, t,
		"functor %s/%d for arithmetic expression",
		RepAtom(name)->StrOfAE,1);
      P = FAILCODE;
      return(FALSE);
    }
    if (!(out=Yap_FoundArithError(eval1(p->FOfEE, top),Deref(ARG3))))
      return FALSE;
    return Yap_unify_constant(ARG1,out);
  }
  return(FALSE);
}

static Int 
p_unary_op_as_integer( USES_REGS1 )
{				/* X is Y	 */
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t, "X is Y");
    return(FALSE);
  }
  if (IsIntTerm(t)) {
    return Yap_unify_constant(ARG2,t);
  }
  if (IsAtomTerm(t)) {
    Atom name = AtomOfTerm(t);
    ExpEntry *p;

    if (EndOfPAEntr(p = RepExpProp(Yap_GetExpProp(name, 1)))) {
      return Yap_unify(ARG1,ARG2);
    }
    return Yap_unify_constant(ARG2,MkIntTerm(p->FOfEE));
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
    p->FOfEE = InitUnTab[i].f;
    AddPropToAtom(ae, (PropEntry *)p);
    WRITE_UNLOCK(ae->ARWLock);
  }
  Yap_InitCPred("is", 3, p_unary_is, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$unary_op_as_integer", 2, p_unary_op_as_integer, TestPredFlag|SafePredFlag);}

/* This routine is called from Restore to make sure we have the same arithmetic operators */
int
Yap_ReInitUnaryExps(void)
{
  return TRUE;
}

