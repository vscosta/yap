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
* File:		eval.c							 *
* Last rev:								 *
* mods:									 *
* comments:	arithmetical expression evaluation			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif
/**
 * @file   eval.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
 * @date   Mon Apr 30 09:37:59 2018
 * 
 * @brief  is/2
 *
 *
 * @namespace prolog
 * 
 * 
 * 
 */

//! @{

/**
   @defgroup arithmetic_preds Arithmetic Predicates
   @ingroup arithmetic

*/
#include "Yap.h"

#include "YapHeap.h"
#include "Yatom.h"
#include "YapEval.h"
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#include <stdlib.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_FENV_H
#include <fenv.h>
#endif

static Term Eval(Term t1 USES_REGS);

static Term get_matrix_element(Term t1, Term t2 USES_REGS) {
  if (!IsPairTerm(t2)) {
    if (t2 == MkAtomTerm(AtomLength)) {
      Int sz = 1;
      while (IsApplTerm(t1)) {
        Functor f = FunctorOfTerm(t1);
        if (NameOfFunctor(f) != AtomNil) {
          return MkIntegerTerm(sz);
        }
        sz *= ArityOfFunctor(f);
        t1 = ArgOfTerm(1, t1);
      }
      return MkIntegerTerm(sz);
    }
    Yap_ArithError(TYPE_ERROR_EVALUABLE, t2, "X is Y^[A]");
    return FALSE;
  }
  while (IsPairTerm(t2)) {
    Int indx;
    Term indxt = Eval(HeadOfTerm(t2) PASS_REGS);
    if (!IsIntegerTerm(indxt)) {
      Yap_ArithError(TYPE_ERROR_EVALUABLE, t2, "X is Y^[A]");
      return FALSE;
    }
    indx = IntegerOfTerm(indxt);
    if (!IsApplTerm(t1)) {
      Yap_ArithError(TYPE_ERROR_EVALUABLE, t1, "X is Y^[A]");
      return FALSE;
    } else {
      Functor f = FunctorOfTerm(t1);
      if (ArityOfFunctor(f) < indx) {
        Yap_ArithError(TYPE_ERROR_EVALUABLE, t1, "X is Y^[A]");
        return FALSE;
      }
    }
    t1 = ArgOfTerm(indx, t1);
    t2 = TailOfTerm(t2);
  }
  if (t2 != TermNil) {
    Yap_ArithError(TYPE_ERROR_EVALUABLE, t2, "X is Y^[A]");
    return FALSE;
  }
  return Eval(t1 PASS_REGS);
}

static Term Eval(Term t USES_REGS) {
  eval_context_t ctx;
  ctx.p = LOCAL_ctx;

  if (IsVarTerm(t)) {
    Yap_ArithError(INSTANTIATION_ERROR, t, "in arithmetic");
  } else if (IsNumTerm(t)) {
    return t;
  } else if (IsAtomTerm(t)) {
    ExpEntry *p;
    Atom name = AtomOfTerm(t);

    if (EndOfPAEntr(p = RepExpProp(Yap_GetExpProp(name, 0)))) {
      Yap_ArithError(TYPE_ERROR_EVALUABLE, takeIndicator(t),
                            "atom %s in arithmetic expression",
                            RepAtom(name)->StrOfAE);
    }
    return Yap_eval_atom(p->FOfEE);
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    if (fun == FunctorString) {
      const char *s = (const char *)StringOfTerm(t);
      if (s[1] == '\0')
        return MkIntegerTerm(s[0]);
      Yap_ArithError(TYPE_ERROR_EVALUABLE, t,
                            "string in arithmetic expression");
    } else if ((Atom)fun == AtomFoundVar) {
      Yap_ArithError(TYPE_ERROR_EVALUABLE, TermNil,
                            "cyclic term in arithmetic expression");
    } else {
      Int n = ArityOfFunctor(fun);
      Atom name = NameOfFunctor(fun);
      ExpEntry *p;
      Term t1, t2;

      if (EndOfPAEntr(p = RepExpProp(Yap_GetExpProp(name, n)))) {
        Yap_ArithError(TYPE_ERROR_EVALUABLE, takeIndicator(t),
                              "functor %s/%d for arithmetic expression",
                              RepAtom(name)->StrOfAE, n);
      }
      if (p->FOfEE == op_power && p->ArityOfEE == 2) {
        t2 = ArgOfTerm(2, t);
        if (IsPairTerm(t2)) {
          return get_matrix_element(ArgOfTerm(1, t), t2 PASS_REGS);
        }
      }
      ctx.f = fun;
      ctx.fp = RepAppl(t);
      LOCAL_ctx = &ctx;
      *RepAppl(t) = (CELL)AtomFoundVar;
      t1 = Eval(ArgOfTerm(1, t) PASS_REGS);
      if (n == 1) {
        *RepAppl(t) = (CELL)fun;
        LOCAL_ctx = ctx.p;
        return Yap_eval_unary(p->FOfEE, t1);
      }
      t2 = Eval(ArgOfTerm(2, t) PASS_REGS);
      *RepAppl(t) = (CELL)fun;
      LOCAL_ctx = ctx.p;
      return Yap_eval_binary(p->FOfEE, t1, t2);
    }
  } /* else if (IsPairTerm(t)) */
  {
    if (TailOfTerm(t) != TermNil) {
      Yap_ArithError(TYPE_ERROR_EVALUABLE, t,
                            "string must contain a single character to be "
                            "evaluated as an arithmetic expression");
    }
    return Eval(HeadOfTerm(t) PASS_REGS);
  }
}

Term Yap_InnerEval__(Term t USES_REGS) {
  return Eval(t PASS_REGS);
 }

#ifdef BEAM
Int BEAM_is(void);

Int BEAM_is(void) { /* X is Y	 */
  union arith_ret res;
  blob_type bt;

  bt = Eval(Deref(XREGS[2]), &res);
  if (bt == db_ref_e)
    return (NULL);
  return (EvalToTerm(bt, &res));
}

#endif

/**
   @pred is( X:number, + Y:ground) is det

   This predicate succeeds iff the result of evaluating the expression
   _Y_ unifies with  _X_. This is the predicate normally used to
   perform evaluation of arithmetic expressions:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
X is 2+3*4
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    succeeds with `X = 14`.

    Consult @ref arithmetic_operators for the complete list of
arithmetic_operators

*/
static Int p_is(USES_REGS1) { /* X is Y	 */
  Term out = TermNil;
  bool go;
  Term t = Deref(ARG2);
  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "var(Y) in X is Y");
    return (FALSE);
  }
  do {
    go = false;
    out = Yap_Eval(t PASS_REGS);
    go = Yap_CheckArithError();
  } while (go);
  return Yap_unify_constant(ARG1, out);
}

/**
 @pred isnan(? X:float) is det

   Interface to the IEE754 `isnan` test.
*/

/// @memberof isnan/1
static Int p_isnan(USES_REGS1) { /* X isnan Y	 */
  Term out = 0L;

  while (!(out = Eval(Deref(ARG1) PASS_REGS))) {
    if (LOCAL_Error_TYPE == RESOURCE_ERROR_STACK) {
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      if (!Yap_gcl(LOCAL_Error_Size, 1, ENV, CP)) {
        Yap_EvalError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
        return FALSE;
      }
    } else {
      Yap_EvalError(LOCAL_Error_TYPE, ARG1, LOCAL_ErrorMessage);
      return FALSE;
    }
  }
  if (IsVarTerm(out)) {
    Yap_EvalError(INSTANTIATION_ERROR, out, "isnan/1");
    return FALSE;
  }
  if (!IsFloatTerm(out)) {
    Yap_EvalError(TYPE_ERROR_FLOAT, out, "isnan/1");
    return FALSE;
  }
  return isnan(FloatOfTerm(out));
}

/**
   @pred isinf(? X:float) is det

   Interface to the IEE754 `isinf` test.
*/
static Int p_isinf(USES_REGS1) { /* X is Y        */
  Term out = 0L;

  while (!(out = Eval(Deref(ARG1) PASS_REGS))) {
    if (LOCAL_Error_TYPE == RESOURCE_ERROR_STACK) {
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      if (!Yap_gcl(LOCAL_Error_Size, 1, ENV, CP)) {
        Yap_EvalError(RESOURCE_ERROR_STACK, ARG2, LOCAL_ErrorMessage);
        return FALSE;
      }
    } else {
      Yap_EvalError(LOCAL_Error_TYPE, ARG1, LOCAL_ErrorMessage);
      return FALSE;
    }
  }
  if (IsVarTerm(out)) {
    Yap_EvalError(INSTANTIATION_ERROR, out, "isinf/1");
    return FALSE;
  }
  if (!IsFloatTerm(out)) {
    Yap_EvalError(TYPE_ERROR_FLOAT, out, "isinf/1");
    return FALSE;
  }
  return isinf(FloatOfTerm(out));
}

/**
   @pred logsum(+ Log1:float, + Log2:float, - Out:float ) is det

True if  _Log1_ is the logarithm of the positive number  _A1_,
 _Log2_ is the logarithm of the positive number  _A2_, and
 _Out_ is the logarithm of the sum of the numbers  _A1_ and
 _A2_. Useful in probability computation.
*/
static Int p_logsum(USES_REGS1) { /* X is Y        */
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  int done = FALSE;
  Float f1, f2;

  while (!done) {
    if (IsFloatTerm(t1)) {
      f1 = FloatOfTerm(t1);
      done = TRUE;
    } else if (IsIntegerTerm(t1)) {
      f1 = IntegerOfTerm(t1);
      done = TRUE;
#if USE_GMP
    } else if (IsBigIntTerm(t1)) {
      f1 = Yap_gmp_to_float(t1);
      done = TRUE;
#endif
    } else {
      while (!(t1 = Eval(t1 PASS_REGS))) {
        if (LOCAL_Error_TYPE == RESOURCE_ERROR_STACK) {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_gcl(LOCAL_Error_Size, 1, ENV, CP)) {
            Yap_EvalError(RESOURCE_ERROR_STACK, ARG2, LOCAL_ErrorMessage);
            return FALSE;
          }
        } else {
          Yap_EvalError(LOCAL_Error_TYPE, ARG1, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
    }
  }
  done = FALSE;
  while (!done) {
    if (IsFloatTerm(t2)) {
      f2 = FloatOfTerm(t2);
      done = TRUE;
    } else if (IsIntegerTerm(t2)) {
      f2 = IntegerOfTerm(t2);
      done = TRUE;
#if USE_GMP
    } else if (IsBigIntTerm(t2)) {
      f2 = Yap_gmp_to_float(t2);
      done = TRUE;
#endif
    } else {
      while (!(t2 = Eval(t2 PASS_REGS))) {
        if (LOCAL_Error_TYPE == RESOURCE_ERROR_STACK) {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_gcl(LOCAL_Error_Size, 2, ENV, CP)) {
            Yap_EvalError(RESOURCE_ERROR_STACK, ARG2, LOCAL_ErrorMessage);
            return FALSE;
          }
        } else {
          Yap_EvalError(LOCAL_Error_TYPE, ARG1, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
    }
  }
  if (f1 >= f2) {
    Float fi = exp(f2 - f1);
    return Yap_unify(ARG3, MkFloatTerm(f1 + log(1 + fi)));
  } else {
    Float fi = exp(f1 - f2);
    return Yap_unify(ARG3, MkFloatTerm(f2 + log(1 + fi)));
  }
}

void Yap_EvalError__(const char *file, const char *function, int lineno,
                       yap_error_number type, Term where, ...) {
  CACHE_REGS
  va_list ap;
  char *format, buf[MAX_ERROR_MSG_SIZE];

  va_start(ap, where);
  format = va_arg(ap, char *);
  if (format != NULL) {
#if HAVE_VSNPRINTF
    (void)vsnprintf(buf, MAX_ERROR_MSG_SIZE, format, ap);
#else
    (void)vsprintf(buf, format, ap);
#endif
  } else {
    buf[0] = '\0';
  }
  va_end(ap);
  Yap_Error__(false, file, function, lineno, type, where, buf);
}

/**

   @pred between(+ Low:int, + High:int, ? Value:int) is nondet

   _Low_ and  _High_ are integers,  _High_ \>= _Low_. If
   _Value_ is an integer,  _Low_ =\< _Value_
   =\< _High_. When  _Value_ is a variable it is successively
   bound to all integers between  _Low_ and  _High_. If
   _High_ is inf or infinite between/3 is true iff
   _Value_ \>=  _Low_, a feature that is particularly interesting
   for generating integers from a certain value.

*/
static Int cont_between(USES_REGS1) {
  Term t1 = EXTRA_CBACK_ARG(3, 1);
  Term t2 = EXTRA_CBACK_ARG(3, 2);

  Yap_unify(ARG3, t1);
  if (IsIntegerTerm(t1)) {
    Int i1;
    Term tn;

    if (t1 == t2)
      cut_succeed();
    i1 = IntegerOfTerm(t1);
    tn = add_int(i1, 1 PASS_REGS);
    EXTRA_CBACK_ARG(3, 1) = tn;
    HB = B->cp_h = HR;
    return TRUE;
  } else {
    Term t[2];
    Term tn;
    Int cmp;

    cmp = Yap_acmp(t1, t2 PASS_REGS);
    if (cmp == 0)
      cut_succeed();
    t[0] = t1;
    t[1] = MkIntTerm(1);
    tn = Eval(Yap_MkApplTerm(FunctorPlus, 2, t) PASS_REGS);
    EXTRA_CBACK_ARG(3, 1) = tn;
    HB = B->cp_h = HR;
    return TRUE;
  }
}
static Int init_between(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);

  if (IsVarTerm(t1)) {
    Yap_EvalError(INSTANTIATION_ERROR, t1, "between/3");
    return FALSE;
  }
  if (IsVarTerm(t2)) {
    Yap_EvalError(INSTANTIATION_ERROR, t1, "between/3");
    return FALSE;
  }
  if (!IsIntegerTerm(t1) && !IsBigIntTerm(t1)) {
    Yap_EvalError(TYPE_ERROR_INTEGER, t1, "between/3");
    return FALSE;
  }
  if (!IsIntegerTerm(t2) && !IsBigIntTerm(t2) && t2 != MkAtomTerm(AtomInf) &&
      t2 != MkAtomTerm(AtomInfinity)) {
    Yap_EvalError(TYPE_ERROR_INTEGER, t2, "between/3");
    return FALSE;
  }
  if (IsIntegerTerm(t1) && IsIntegerTerm(t2)) {
    Int i1 = IntegerOfTerm(t1);
    Int i2 = IntegerOfTerm(t2);
    Term t3;

    t3 = Deref(ARG3);
    if (!IsVarTerm(t3)) {
      if (!IsIntegerTerm(t3)) {
        if (!IsBigIntTerm(t3)) {
          Yap_EvalError(TYPE_ERROR_INTEGER, t3, "between/3");
          return FALSE;
        }
        cut_fail();
      } else {
        Int i3 = IntegerOfTerm(t3);
        if (i3 >= i1 && i3 <= i2)
          cut_succeed();
        cut_fail();
      }
    }
    if (i1 > i2)
      cut_fail();
    if (i1 == i2) {
      Yap_unify(ARG3, t1);
      cut_succeed();
    }
  } else if (IsIntegerTerm(t1) && IsAtomTerm(t2)) {
    Int i1 = IntegerOfTerm(t1);
    Term t3;

    t3 = Deref(ARG3);
    if (!IsVarTerm(t3)) {
      if (!IsIntegerTerm(t3)) {
        if (!IsBigIntTerm(t3)) {
          Yap_EvalError(TYPE_ERROR_INTEGER, t3, "between/3");
          return FALSE;
        }
        cut_fail();
      } else {
        Int i3 = IntegerOfTerm(t3);
        if (i3 >= i1)
          cut_succeed();
        cut_fail();
      }
    }
  } else {
    Term t3 = Deref(ARG3);
    Int cmp;

    if (!IsVarTerm(t3)) {
      if (!IsIntegerTerm(t3) && !IsBigIntTerm(t3)) {
        Yap_EvalError(TYPE_ERROR_INTEGER, t3, "between/3");
        return FALSE;
      }
      if (Yap_acmp(t3, t1 PASS_REGS) >= 0 && Yap_acmp(t2, t3 PASS_REGS) >= 0 &&
          P != FAILCODE)
        cut_succeed();
      cut_fail();
    }
    cmp = Yap_acmp(t1, t2 PASS_REGS);
    if (cmp > 0)
      cut_fail();
    if (cmp == 0) {
      Yap_unify(ARG3, t1);
      cut_succeed();
    }
  }
  EXTRA_CBACK_ARG(3, 1) = t1;
  EXTRA_CBACK_ARG(3, 2) = t2;
  return cont_between(PASS_REGS1);
}

void Yap_InitEval(void) {
  /* here are the arithmetical predicates */
  Yap_InitConstExps();
  Yap_InitUnaryExps();
  Yap_InitBinaryExps();
  Yap_InitCPred("is", 2, p_is, 0L);
  Yap_InitCPred("isnan", 1, p_isnan, TestPredFlag);
  Yap_InitCPred("isinf", 1, p_isinf, TestPredFlag);
  Yap_InitCPred("logsum", 3, p_logsum, TestPredFlag);
  Yap_InitCPredBack("between", 3, 2, init_between, cont_between, 0);
}

/**
 *
 * @}
*/
