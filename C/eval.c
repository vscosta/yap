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
static char     SccsId[] = "%W% %G%";
#endif

/*
 * This file implements arithmetic operations 
 *
 */
#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "eval.h"
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#include <stdlib.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

static Term Eval(Term t1 USES_REGS);

static Term
get_matrix_element(Term t1, Term t2 USES_REGS)
{
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

static Term
Eval(Term t USES_REGS)
{
  if (IsVarTerm(t)) {
    LOCAL_ArithError = TRUE;
    return Yap_ArithError(INSTANTIATION_ERROR,t,"in arithmetic");
  } else if (IsNumTerm(t)) {
    return t;
  } else if (IsAtomTerm(t)) {
    ExpEntry *p;
    Atom name  = AtomOfTerm(t);

    if (EndOfPAEntr(p = RepExpProp(Yap_GetExpProp(name, 0)))) {
      /* error */
      return Yap_ArithError(TYPE_ERROR_EVALUABLE, t,
			    "atom %s for arithmetic expression",
			    RepAtom(name)->StrOfAE);
    }
    return Yap_eval_atom(p->FOfEE);
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    if ((Atom)fun == AtomFoundVar) {
      return Yap_ArithError(TYPE_ERROR_EVALUABLE, TermNil,
			    "cyclic term in arithmetic expression");
    } else {
      Int n = ArityOfFunctor(fun);
      Atom name  = NameOfFunctor(fun);
      ExpEntry *p;
      Term t1, t2;
      
      if (EndOfPAEntr(p = RepExpProp(Yap_GetExpProp(name, n)))) {
	Term ti[2];

	/* error */
	ti[0] = t;
	ti[1] = MkIntegerTerm(n);
	t = Yap_MkApplTerm(FunctorSlash, 2, ti);
	return Yap_ArithError(TYPE_ERROR_EVALUABLE, t,
			      "functor %s/%d for arithmetic expression",
			      RepAtom(name)->StrOfAE,n);
      }
      if (p->FOfEE == op_power && p->ArityOfEE == 2) {
	t2 = ArgOfTerm(2, t);
	if (IsPairTerm(t2)) {
	  return get_matrix_element(ArgOfTerm(1, t), t2 PASS_REGS);
	}
      }
      *RepAppl(t) = (CELL)AtomFoundVar;
      t1 = Eval(ArgOfTerm(1,t) PASS_REGS);
      if (t1 == 0L) {
	*RepAppl(t) = (CELL)fun;
	return FALSE;
      }
      if (n == 1) {
	*RepAppl(t) = (CELL)fun;
	return Yap_eval_unary(p->FOfEE, t1);
      }
      t2 = Eval(ArgOfTerm(2,t) PASS_REGS);
      *RepAppl(t) = (CELL)fun;
      if (t2 == 0L)
	return FALSE;
      return Yap_eval_binary(p->FOfEE,t1,t2);
    }
  } /* else if (IsPairTerm(t)) */ {
    if (TailOfTerm(t) != TermNil) {
      return Yap_ArithError(TYPE_ERROR_EVALUABLE, t,
			    "string must contain a single character to be evaluated as an arithmetic expression");
    }
    return Eval(HeadOfTerm(t) PASS_REGS);
  }
}

Term
Yap_InnerEval(Term t)
{
  CACHE_REGS
  return Eval(t PASS_REGS);
}

#ifdef BEAM
Int BEAM_is(void);

Int
BEAM_is(void)
{				/* X is Y	 */
  union arith_ret res;
  blob_type bt;

  bt = Eval(Deref(XREGS[2]), &res);
  if (bt==db_ref_e) return (NULL);
  return (EvalToTerm(bt,&res));
}

#endif

static Int
p_is( USES_REGS1 )
{				/* X is Y	 */
  Term out = 0L;
  
  while (!(out = Eval(Deref(ARG2) PASS_REGS))) {
    if (LOCAL_Error_TYPE == RESOURCE_ERROR_STACK) {
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      if (!Yap_gcl(LOCAL_Error_Size, 2, ENV, CP)) {
	Yap_Error(RESOURCE_ERROR_STACK, ARG2, LOCAL_ErrorMessage);
	return FALSE;
      }
    } else {
      Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage);
      return FALSE;
    }
  }
  return Yap_unify_constant(ARG1,out);
}

Int
Yap_ArithError(yap_error_number type, Term where, char *format,...)
{
  CACHE_REGS
  va_list ap;

  LOCAL_ArithError = TRUE;
  LOCAL_Error_TYPE = type;
  LOCAL_Error_Term = where;
  if (!LOCAL_ErrorMessage)
    LOCAL_ErrorMessage = LOCAL_ErrorSay;
  va_start (ap, format);
  if (format != NULL) {
#if   HAVE_VSNPRINTF
    (void) vsnprintf(LOCAL_ErrorMessage, MAX_ERROR_MSG_SIZE, format, ap);
#else
    (void) vsprintf(LOCAL_ErrorMessage, format, ap);
#endif
  } else {
    LOCAL_ErrorMessage[0] = '\0';
  }
  va_end (ap);
  return 0L;
}

void
Yap_InitEval(void)
{
  /* here are the arithmetical predicates */
  Yap_InitConstExps();
  Yap_InitUnaryExps();
  Yap_InitBinaryExps();
  Yap_InitCPred("is", 2, p_is, 0L);
}

