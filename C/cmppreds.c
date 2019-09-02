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
* File:		cmppreds.c						 *
* Last rev:								 *
* mods:									 *
* comments:	comparing two prolog terms				 *
*									 *
*************************************************************************/

/**
 * @file   cmppreds.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
 * @date   Mon Apr 30 09:35:58 2018
 * 
 * @brief  comparison
 *
 * @namespace prolog
 * 
 * 
 * 
 */
///    @file cmppreds.c

/**
    @defgroup Comparing_Terms Comparing Terms
@ingroup builtins

The following predicates are used to compare and order terms, using the
standard ordering:

+
variables come before numbers, numbers come before atoms which in turn
come before compound terms, i.e.: variables @< numbers @< atoms @<
compound terms.
+  Variables are roughly ordered by "age" (the "oldest" variable is put
first);
+
Floating point numbers are sorted in increasing order;
+
Rational numbers are sorted in increasing order;
+
Integers are sorted in increasing order;
+
Atoms are sorted in lexicographic order;
+
Compound terms are ordered first by arity of the main functor, then by
the name of the main functor, and finally by their arguments in
left-to-right order.

@{



*/

#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

#include "Yap.h"
#include "YapHeap.h"
#include "Yatom.h"
#include "YapEval.h"
#if HAVE_STRING_H
#include <string.h>
#endif
#include <wchar.h>

#include "YapError.h"

static Int compare(Term, Term);
static Int p_compare(USES_REGS1);
static Int p_acomp(USES_REGS1);
static Int a_eq(Term, Term);
static Int a_dif(Term, Term);
static Int a_gt(Term, Term);
static Int a_ge(Term, Term);
static Int a_lt(Term, Term);
static Int a_le(Term, Term);
static Int a_noteq(Term, Term);
static Int a_gen_lt(Term, Term);
static Int a_gen_le(Term, Term);
static Int a_gen_gt(Term, Term);
static Int a_gen_ge(Term, Term);

#define rfloat(X) (X > 0.0 ? 1 : (X == 0.0 ? 0 : -1))

static int cmp_atoms(Atom a1, Atom a2) {
  return strcmp(RepAtom(a1)->StrOfAE, RepAtom(a2)->StrOfAE);
}

static Int compare_complex(register CELL *pt0, register CELL *pt0_end,
                           register CELL *pt1) {
  CACHE_REGS
  register CELL **to_visit = (CELL **)HR;
  register Int out = 0;

loop:
  while (pt0 < pt0_end) {
    register CELL d0, d1;
    ++pt0;
    ++pt1;
    d0 = Derefa(pt0);
    d1 = Derefa(pt1);
    if (IsVarTerm(d0)) {
      if (IsVarTerm(d1)) {
        out = Signed(d0) - Signed(d1);
        if (out)
          goto done;
      } else {
        out = -1;
        goto done;
      }
    } else if (IsVarTerm(d1)) {
      out = 1;
      goto done;
    } else {
      if (d0 == d1)
        continue;
      else if (IsAtomTerm(d0)) {
        if (IsAtomTerm(d1))
          out = cmp_atoms(AtomOfTerm(d0), AtomOfTerm(d1));
        else if (IsPrimitiveTerm(d1))
          out = 1;
        else
          out = -1;
        /* I know out must be != 0 */
        goto done;
      } else if (IsIntTerm(d0)) {
        if (IsIntTerm(d1))
          out = IntOfTerm(d0) - IntOfTerm(d1);
        else if (IsFloatTerm(d1)) {
          out = 1;
        } else if (IsLongIntTerm(d1)) {
          out = IntOfTerm(d0) - LongIntOfTerm(d1);
#ifdef USE_GMP
        } else if (IsBigIntTerm(d1)) {
          out = Yap_gmp_tcmp_int_big(IntOfTerm(d0), d1);
#endif
        } else if (IsRefTerm(d1))
          out = 1;
        else
          out = -1;
        if (out != 0)
          goto done;
      } else if (IsFloatTerm(d0)) {
        if (IsFloatTerm(d1)) {
          out = rfloat(FloatOfTerm(d0) - FloatOfTerm(d1));
        } else if (IsRefTerm(d1)) {
          out = 1;
        } else {
          out = -1;
        }
        if (out != 0)
          goto done;
      } else if (IsStringTerm(d0)) {
        if (IsStringTerm(d1)) {
          out = strcmp((char *)StringOfTerm(d0), (char *)StringOfTerm(d1));
        } else if (IsIntTerm(d1))
          out = 1;
        else if (IsFloatTerm(d1)) {
          out = 1;
        } else if (IsLongIntTerm(d1)) {
          out = 1;
#ifdef USE_GMP
        } else if (IsBigIntTerm(d1)) {
          out = 1;
#endif
        } else if (IsRefTerm(d1)) {
          out = 1;
        } else {
          out = -1;
        }
        if (out != 0)
          goto done;
      } else if (IsLongIntTerm(d0)) {
        if (IsIntTerm(d1))
          out = LongIntOfTerm(d0) - IntOfTerm(d1);
        else if (IsFloatTerm(d1)) {
          out = 1;
        } else if (IsLongIntTerm(d1)) {
          out = LongIntOfTerm(d0) - LongIntOfTerm(d1);
#ifdef USE_GMP
        } else if (IsBigIntTerm(d1)) {
          out = Yap_gmp_tcmp_int_big(LongIntOfTerm(d0), d1);
#endif
        } else if (IsRefTerm(d1)) {
          out = 1;
        } else {
          out = -1;
        }
        if (out != 0)
          goto done;
      }
#ifdef USE_GMP
      else if (IsBigIntTerm(d0)) {
        if (IsIntTerm(d1)) {
          out = Yap_gmp_tcmp_big_int(d0, IntOfTerm(d1));
        } else if (IsFloatTerm(d1)) {
          out = 1;
        } else if (IsLongIntTerm(d1)) {
          out = Yap_gmp_tcmp_big_int(d0, LongIntOfTerm(d1));
        } else if (IsBigIntTerm(d1)) {
          out = Yap_gmp_tcmp_big_big(d0, d1);
        } else if (IsRefTerm(d1))
          out = 1;
        else
          out = -1;
        if (out != 0)
          goto done;
      }
#endif
      else if (IsPairTerm(d0)) {
        if (!IsPairTerm(d1)) {
          if (IsApplTerm(d1)) {
            Functor f = FunctorOfTerm(d1);
            if (IsExtensionFunctor(f))
              out = 1;
            else if (!(out = 2 - ArityOfFunctor(f)))
              out = strcmp(".", (char *)RepAtom(NameOfFunctor(f))->StrOfAE);
          } else
            out = 1;
          goto done;
        }
#ifdef RATIONAL_TREES
        to_visit[0] = pt0;
        to_visit[1] = pt0_end;
        to_visit[2] = pt1;
        to_visit[3] = (CELL *)*pt0;
        to_visit += 4;
        *pt0 = d1;
#else
        /* store the terms to visit */
        if (pt0 < pt0_end) {
          to_visit[0] = pt0;
          to_visit[1] = pt0_end;
          to_visit[2] = pt1;
          to_visit += 3;
        }
#endif
        pt0 = RepPair(d0) - 1;
        pt0_end = RepPair(d0) + 1;
        pt1 = RepPair(d1) - 1;
        continue;
      } else if (IsRefTerm(d0)) {
        if (IsRefTerm(d1))
          out = Unsigned(RefOfTerm(d1)) - Unsigned(RefOfTerm(d0));
        else
          out = -1;
        goto done;
      } else if (IsApplTerm(d0)) {
        register Functor f;
        register CELL *ap2, *ap3;
        if (!IsApplTerm(d1)) {
          out = 1;
          goto done;
        } else {
          /* store the terms to visit */
          Functor f2;
          ap2 = RepAppl(d0);
          ap3 = RepAppl(d1);
          f = (Functor)(*ap2);
          if (IsExtensionFunctor(f)) {
            out = 1;
            goto done;
          }
          f2 = (Functor)(*ap3);
          if (IsExtensionFunctor(f2)) {
            out = -1;
            goto done;
          }
          /* compare functors */
          if (f != (Functor)*ap3) {
            if (!(out = ArityOfFunctor(f) - ArityOfFunctor(f2)))
              out = cmp_atoms(NameOfFunctor(f), NameOfFunctor(f2));
            goto done;
          }
#ifdef RATIONAL_TREES
          to_visit[0] = pt0;
          to_visit[1] = pt0_end;
          to_visit[2] = pt1;
          to_visit[3] = (CELL *)*pt0;
          to_visit += 4;
          *pt0 = d1;
#else
          /* store the terms to visit */
          if (pt0 < pt0_end) {
            to_visit[0] = pt0;
            to_visit[1] = pt0_end;
            to_visit[2] = pt1;
            to_visit += 3;
          }
#endif
          d0 = ArityOfFunctor(f);
          pt0 = ap2;
          pt0_end = ap2 + d0;
          pt1 = ap3;
          continue;
        }
      }
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit > (CELL **)HR) {
#ifdef RATIONAL_TREES
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
#else
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
#endif
    goto loop;
  }

done:
/* failure */
#ifdef RATIONAL_TREES
  while (to_visit > (CELL **)HR) {
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
  }
#endif
  return (out);
}

inline static Int compare(Term t1, Term t2) /* compare terms t1 and t2	 */
{

  if (t1 == t2)
    return 0;
  if (IsVarTerm(t1)) {
    if (IsVarTerm(t2))
      return Signed(t1) - Signed(t2);
    return -1;
  } else if (IsVarTerm(t2)) {
    /* get rid of variables */
    return 1;
  }
  if (IsAtomOrIntTerm(t1)) {
    if (IsAtomTerm(t1)) {
      if (IsAtomTerm(t2))
        return cmp_atoms(AtomOfTerm(t1), AtomOfTerm(t2));
      if (IsPrimitiveTerm(t2))
        return 1;
      if (IsStringTerm(t2))
        return 1;
      return -1;
    } else {
      if (IsIntTerm(t2)) {
        return IntOfTerm(t1) - IntOfTerm(t2);
      }
      if (IsApplTerm(t2)) {
        Functor fun2 = FunctorOfTerm(t2);
        switch ((CELL)fun2) {
        case double_e:
          return 1;
        case long_int_e:
          return IntOfTerm(t1) - LongIntOfTerm(t2);
#ifdef USE_GMP
        case big_int_e:
          return Yap_gmp_tcmp_int_big(IntOfTerm(t1), t2);
#endif
        case db_ref_e:
          return 1;
        case string_e:
          return -1;
        }
      }
      return -1;
    }
  } else if (IsPairTerm(t1)) {
    if (IsApplTerm(t2)) {
      Functor f = FunctorOfTerm(t2);
      if (IsExtensionFunctor(f))
        return 1;
      else {
        if (f != FunctorDot)
          return strcmp(".", RepAtom(NameOfFunctor(f))->StrOfAE);
        else {
          return compare_complex(RepPair(t1) - 1, RepPair(t1) + 1, RepAppl(t2));
        }
      }
    }
    if (IsPairTerm(t2)) {
      return (
          compare_complex(RepPair(t1) - 1, RepPair(t1) + 1, RepPair(t2) - 1));
    } else
      return 1;
  } else {
    /* compound term */
    Functor fun1 = FunctorOfTerm(t1);

    if (IsExtensionFunctor(fun1)) {
      /* float, long, big, dbref */
      switch ((CELL)fun1) {
      case double_e: {
        if (IsFloatTerm(t2))
          return (rfloat(FloatOfTerm(t1) - FloatOfTerm(t2)));
        if (IsRefTerm(t2))
          return 1;
        return -1;
      }
      case long_int_e: {
        if (IsIntTerm(t2))
          return LongIntOfTerm(t1) - IntOfTerm(t2);
        if (IsFloatTerm(t2)) {
          return 1;
        }
        if (IsLongIntTerm(t2))
          return LongIntOfTerm(t1) - LongIntOfTerm(t2);
#ifdef USE_GMP
        if (IsBigIntTerm(t2)) {
          return Yap_gmp_tcmp_int_big(LongIntOfTerm(t1), t2);
        }
#endif
        if (IsRefTerm(t2))
          return 1;
        return -1;
      }
#ifdef USE_GMP
      case big_int_e: {
        if (IsIntTerm(t2))
          return Yap_gmp_tcmp_big_int(t1, IntOfTerm(t2));
        if (IsFloatTerm(t2)) {
          return 1;
        }
        if (IsLongIntTerm(t2))
          return Yap_gmp_tcmp_big_int(t1, LongIntOfTerm(t2));
        if (IsBigIntTerm(t2)) {
          return Yap_gmp_tcmp_big_big(t1, t2);
        }
        if (IsRefTerm(t2))
          return 1;
        return -1;
      }
#endif
      case string_e: {
        if (IsApplTerm(t2)) {
          Functor fun2 = FunctorOfTerm(t2);
          switch ((CELL)fun2) {
          case double_e:
            return 1;
          case long_int_e:
            return 1;
#ifdef USE_GMP
          case big_int_e:
            return 1;
#endif
          case db_ref_e:
            return 1;
          case string_e:
            return strcmp((char *)StringOfTerm(t1), (char *)StringOfTerm(t2));
          }
          return -1;
        }
        return -1;
      }
      case db_ref_e:
        if (IsRefTerm(t2))
          return Unsigned(RefOfTerm(t2)) - Unsigned(RefOfTerm(t1));
        return -1;
      }
    }
    if (!IsApplTerm(t2)) {
      if (IsPairTerm(t2)) {
        Int out;
        Functor f = FunctorOfTerm(t1);

        if (!(out = ArityOfFunctor(f)) - 2)
          out = strcmp((char *)RepAtom(NameOfFunctor(f))->StrOfAE, ".");
        return out;
      }
      return 1;
    } else {
      Functor fun2 = FunctorOfTerm(t2);
      Int r;

      if (IsExtensionFunctor(fun2)) {
        return 1;
      }
      r = ArityOfFunctor(fun1) - ArityOfFunctor(fun2);
      if (r)
        return r;
      r = cmp_atoms(NameOfFunctor(fun1), NameOfFunctor(fun2));
      if (r)
        return r;
      else
        return (compare_complex(RepAppl(t1), RepAppl(t1) + ArityOfFunctor(fun1),
                                RepAppl(t2)));
    }
  }
}

Int Yap_compare_terms(Term d0, Term d1) {
  return compare(Deref(d0), Deref(d1));
}

/** @pred  compare( _C_, _X_, _Y_) is iso


As a result of comparing  _X_ and  _Y_,  _C_ may take one of
the following values:

+
`=` if  _X_ and  _Y_ are identical;
+
`<` if  _X_ precedes  _Y_ in the defined order;
+
`>` if  _Y_ precedes  _X_ in the defined order;

*/
Int p_compare(USES_REGS1) { /* compare(?Op,?T1,?T2)	 */
  Int r = compare(Deref(ARG2), Deref(ARG3));
  Atom p;
  Term t = Deref(ARG1);
  if (r < 0)
    p = AtomLT;
  else if (r > 0)
    p = AtomGT;
  else
    p = AtomEQ;
  if (!IsVarTerm(t)) {
    if (IsAtomTerm(t)) {
      Atom a = AtomOfTerm(t);
      if (a == p)
        return true;
      if (a != AtomLT && a != AtomGT && a != AtomEq)
        Yap_Error(DOMAIN_ERROR_ORDER, ARG1, NULL);
    } else {
      Yap_Error(TYPE_ERROR_ATOM, ARG1, NULL);
    }
    return false;
  }

  return Yap_unify_constant(ARG1, MkAtomTerm(p));
}

/** @pred  X \==  Y is iso

Terms  _X_ and  _Y_ are not strictly identical.
*/
static Int a_noteq(Term t1, Term t2) { return (compare(t1, t2) != 0); }

static Int a_gen_lt(Term t1, Term t2) { return (compare(t1, t2) < 0); }

/** @pred  X @=< Y is iso


Term  _X_ does not follow term  _Y_ in the standard order.

*/
static Int a_gen_le(Term t1, Term t2) { return (compare(t1, t2) <= 0); }

/** @pred  X @>  Y is iso


Term  _X_ does not follow term  _Y_ in the standard order
*/
static Int a_gen_gt(Term t1, Term t2) { return compare(t1, t2) > 0; }

/** @pred  X @>= Y is iso

Term  _X_ does not precede term  _Y_ in the standard order.
*/
static Int a_gen_ge(Term t1, Term t2) { return compare(t1, t2) >= 0; }

/**
@}
*/

/**

   @defgroup arithmetic_cmps Arithmetic Comparison Predicates
   @ingroup arithmetic

   Comparison of Numeric Expressions. Both arguments must be valid ground
   expressions at time of call.

   @{
*/
inline static Int int_cmp(Int dif) { return dif; }

inline static Int flt_cmp(Float dif) {
  if (dif < 0.0)
    return -1;
  if (dif > 0.0)
    return 1;
  return dif = 0.0;
}

static Int a_cmp(Term t1, Term t2 USES_REGS) {
  if (IsVarTerm(t1)) {
    Yap_ArithError(INSTANTIATION_ERROR, t1,
                   "while doing arithmetic comparison");
  }
  if (IsVarTerm(t2)) {
    Yap_ArithError(INSTANTIATION_ERROR,  t2,
                   "while doing arithmetic comparison");
  }
  if (IsFloatTerm(t1) && IsFloatTerm(t2)) {
    return flt_cmp(FloatOfTerm(t1) - FloatOfTerm(t2));
  }
  if (IsIntegerTerm(t1) && IsIntegerTerm(t2)) {
    return int_cmp(IntegerOfTerm(t1) - IntegerOfTerm(t2));
  }
  t1 = Yap_Eval(t1);
  if (!t1) {
    return FALSE;
  }
  if (IsIntegerTerm(t1)) {
    Int i1 = IntegerOfTerm(t1);
    t2 = Yap_Eval(t2);

    if (IsIntegerTerm(t2)) {
      Int i2 = IntegerOfTerm(t2);
      return int_cmp(i1 - i2);
    } else if (IsFloatTerm(t2)) {
      Float f2 = FloatOfTerm(t2);
#if HAVE_ISNAN
      if (isnan(f2)) {
        Yap_ArithError(EVALUATION_ERROR_UNDEFINED, t2,
                       "trying to evaluate nan");
      }
#endif
      return flt_cmp(i1 - f2);
#ifdef USE_GMP
    } else if (IsBigIntTerm(t2)) {
      return Yap_gmp_cmp_int_big(i1, t2);
#endif
    } else {
      return FALSE;
    }
  } else if (IsFloatTerm(t1)) {
    Float f1 = FloatOfTerm(t1);
#if HAVE_ISNAN
    if (isnan(f1)) {
      Yap_ArithError(EVALUATION_ERROR_UNDEFINED, t1,
                     "trying to evaluate nan");
    }
#endif
    t2 = Yap_Eval(t2);
#if HAVE_ISNAN
    if (isnan(f1))
      return -1;
#endif

    if (IsIntegerTerm(t2)) {
      Int i2 = IntegerOfTerm(t2);
      return flt_cmp(f1 - i2);
    } else if (IsFloatTerm(t2)) {
      Float f2 = FloatOfTerm(t2);
#if HAVE_ISNAN
      if (isnan(f2)) {
        Yap_ArithError(EVALUATION_ERROR_UNDEFINED, t2,
                       "trying to evaluate nan");
      }
#endif
      return flt_cmp(f1 - f2);
#ifdef USE_GMP
    } else if (IsBigIntTerm(t2)) {
      return Yap_gmp_cmp_float_big(f1, t2);
#endif
    } else {
      return FALSE;
    }
#ifdef USE_GMP
  } else if (IsBigIntTerm(t1)) {
    {
      t2 = Yap_Eval(t2);

      if (IsIntegerTerm(t2)) {
        return Yap_gmp_cmp_big_int(t1, IntegerOfTerm(t2));
      } else if (IsFloatTerm(t2)) {
        Float f2 = FloatOfTerm(t2);
#if HAVE_ISNAN
        if (isnan(f2)) {
          Yap_ArithError(EVALUATION_ERROR_UNDEFINED, t2,
                         "trying to evaluate nan");
        }
#endif
        return Yap_gmp_cmp_big_float(t1, f2);
      } else if (IsBigIntTerm(t2)) {
        return Yap_gmp_cmp_big_big(t1, t2);
      } else {
        return FALSE;
      }
    }
#endif
  } else {
    return FALSE;
  }
}

Int Yap_acmp(Term t1, Term t2 USES_REGS) {
  Int out = a_cmp(t1, t2 PASS_REGS);
  return out;
}

static Int p_acomp(USES_REGS1) { /* $a_compare(?R,+X,+Y) */
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  Int out;

  out = a_cmp(t1, t2 PASS_REGS);
  return out;
}

/**
   @pred +X '=:=' Y is iso
   Equality of arithmetic expressions

   The value of the expression  _X_ is equal to the value of expression _Y_.
 */

static Int a_eq(Term t1, Term t2) {
  CACHE_REGS
  /* A =:= B		 */
  Int out;
  t1 = Deref(t1);
  t2 = Deref(t2);

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "=:=/2");
    return (FALSE);
  }
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "=:=/2");
    return (FALSE);
  }
  if (IsFloatTerm(t1)) {
    if (IsFloatTerm(t2))
      return (FloatOfTerm(t1) == FloatOfTerm(t2));
    else if (IsIntegerTerm(t2)) {
      return (FloatOfTerm(t1) == IntegerOfTerm(t2));
    }
  }
  if (IsIntegerTerm(t1)) {
    if (IsIntegerTerm(t2)) {
      return (IntegerOfTerm(t1) == IntegerOfTerm(t2));
    } else if (IsFloatTerm(t2)) {
      return (FloatOfTerm(t2) == IntegerOfTerm(t1));
    }
  }
  out = a_cmp(t1, t2 PASS_REGS);
  return out == 0;
}

/*
   @pred +_X_ =\\= _Y_ is iso
    Difference of arithmetic expressions

   The value of the expression  _X_ is different from the value of expression
   _Y_.
 */
static Int a_dif(Term t1, Term t2) {
  CACHE_REGS
  Int out = a_cmp(Deref(t1), Deref(t2) PASS_REGS);
  return out != 0;
}

/**
   @pred +_X_ \> +_Y_ is iso
   Greater than arithmetic expressions

  The value of the expression  _X_ is less than or equal to the value
  of expression  _Y_.
*/
static Int a_gt(Term t1, Term t2) { /* A > B		 */
  CACHE_REGS
  Int out = a_cmp(Deref(t1), Deref(t2) PASS_REGS);
  return out > 0;
}

/**
   @pred +X >= +Y is iso
   Greater than or equal to arithmetic expressions

   The value of the expression  _X_ is greater than or equal to the
   value of expression  _Y_.
*/
static Int a_ge(Term t1, Term t2) { /* A >= B		 */
  CACHE_REGS
  Int out = a_cmp(Deref(t1), Deref(t2) PASS_REGS);
  return out >= 0;
}

/**
   @pred +X < +Y is iso
   Lesser than arithmetic expressions

   The value of the expression  _X_ is less than the value of expression
   _Y_.
*/
static Int a_lt(Term t1, Term t2) { /* A < B       */
  CACHE_REGS
  Int out = a_cmp(Deref(t1), Deref(t2) PASS_REGS);
  return out < 0;
}

/**
 *
 @pred +X =< +Y
  Lesser than or equal to arithmetic expressions


 The value of the expression  _X_ is less than or equal to the value
 of expression  _Y_.
*/
static Int a_le(Term t1, Term t2) { /* A <= B */
  CACHE_REGS
  Int out = a_cmp(Deref(t1), Deref(t2) PASS_REGS);
  return out <= 0;
}

void Yap_InitCmpPreds(void) {
  Yap_InitCmpPred("=:=", 2, a_eq, SafePredFlag | BinaryPredFlag);
  Yap_InitCmpPred("=\\=", 2, a_dif, SafePredFlag | BinaryPredFlag);
  Yap_InitCmpPred(">", 2, a_gt, SafePredFlag | BinaryPredFlag);
  Yap_InitCmpPred("=<", 2, a_le, SafePredFlag | BinaryPredFlag);
  Yap_InitCmpPred("<", 2, a_lt, SafePredFlag | BinaryPredFlag);
  Yap_InitCmpPred(">=", 2, a_ge, SafePredFlag | BinaryPredFlag);
  Yap_InitCPred("$a_compare", 3, p_acomp, TestPredFlag | SafePredFlag);
  Yap_InitCmpPred("\\==", 2, a_noteq, BinaryPredFlag | SafePredFlag);
  Yap_InitCmpPred("@<", 2, a_gen_lt, BinaryPredFlag | SafePredFlag);
  Yap_InitCmpPred("@=<", 2, a_gen_le, BinaryPredFlag | SafePredFlag);
  Yap_InitCmpPred("@>", 2, a_gen_gt, BinaryPredFlag | SafePredFlag);
  Yap_InitCmpPred("@>=", 2, a_gen_ge, BinaryPredFlag | SafePredFlag);
  Yap_InitCPred("compare", 3, p_compare, TestPredFlag | SafePredFlag);
}

/**
@}
*/
