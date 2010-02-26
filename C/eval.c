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

yap_error_number Yap_matherror = YAP_NO_ERROR;

static Term
Eval(Term t)
{
  if (IsVarTerm(t)) {
    ArithError = TRUE;
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
      *RepAppl(t) = (CELL)AtomFoundVar;
      t1 = Eval(ArgOfTerm(1,t));
      if (t1 == 0L) {
	*RepAppl(t) = (CELL)fun;
	return FALSE;
      }
      if (n == 1) {
	*RepAppl(t) = (CELL)fun;
	return Yap_eval_unary(p->FOfEE, t1);
      }
      t2 = Eval(ArgOfTerm(2,t));
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
    return Eval(HeadOfTerm(t));
  }
}

Term
Yap_InnerEval(Term t)
{
  return Eval(t);
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
p_is(void)
{				/* X is Y	 */
  Term out = 0L;
  
  while (!(out = Eval(Deref(ARG2)))) {
    if (Yap_Error_TYPE == RESOURCE_ERROR_STACK) {
      Yap_Error_TYPE = YAP_NO_ERROR;
      if (!Yap_gcl(Yap_Error_Size, 2, ENV, CP)) {
	Yap_Error(RESOURCE_ERROR_STACK, ARG2, Yap_ErrorMessage);
	return FALSE;
      }
    } else {
      Yap_Error(Yap_Error_TYPE, Yap_Error_Term, Yap_ErrorMessage);
      return FALSE;
    }
  }
  return Yap_unify_constant(ARG1,out);
}

Int
Yap_ArithError(yap_error_number type, Term where, char *format,...)
{
  va_list ap;

  ArithError = TRUE;
  Yap_Error_TYPE = type;
  Yap_Error_Term = where;
  if (!Yap_ErrorMessage)
    Yap_ErrorMessage = Yap_ErrorSay;
  va_start (ap, format);
  if (format != NULL) {
#if   HAVE_VSNPRINTF
    (void) vsnprintf(Yap_ErrorMessage, MAX_ERROR_MSG_SIZE, format, ap);
#else
    (void) vsprintf(Yap_ErrorMessage, format, ap);
#endif
  } else {
    Yap_ErrorMessage[0] = '\0';
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

