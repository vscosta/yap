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
#include "Heap.h"
#include "eval.h"

yap_error_number Yap_matherror = YAP_NO_ERROR;

static Term
Eval(Term t)
{
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,TermNil,"in arithmetic");
    P = (yamop *)FAILCODE;
    return 0L;
  } else if (IsAtomTerm(t)) {
    ExpEntry *p;
    Atom name  = AtomOfTerm(t);

    if (EndOfPAEntr(p = RepExpProp(Yap_GetExpProp(name, 0)))) {
      Term ti[2], terror;
      
      /* error */
      ti[0] = t;
      ti[1] = MkIntegerTerm(0);
      /* error */
      terror = Yap_MkApplTerm(FunctorSlash, 2, ti);
      Yap_Error(TYPE_ERROR_EVALUABLE, terror,
	    "atom %s for arithmetic expression",
	    RepAtom(name)->StrOfAE);
      P = (yamop *)FAILCODE;
      return 0L;
    }
    return Yap_eval_atom(p->FOfEE);
  } else if (IsIntTerm(t)) {
    return t;
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    switch ((CELL)fun) {
    case (CELL)FunctorLongInt:
    case (CELL)FunctorDouble:
#ifdef USE_GMP
    case (CELL)FunctorBigInt:
#endif
      return t;
    default:
      {
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
	  Yap_Error(TYPE_ERROR_EVALUABLE, t,
		"functor %s/%d for arithmetic expression",
		RepAtom(name)->StrOfAE,n);
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
	t1 = Eval(ArgOfTerm(1,t));
	if (t1 == 0L)
	  return FALSE;
	if (n == 1)
	  return Yap_eval_unary(p->FOfEE, t1);
	t2 = Eval(ArgOfTerm(2,t));
	if (t2 == 0L)
	  return FALSE;
	return Yap_eval_binary(p->FOfEE,t1,t2);
      }
    }
  } /* else if (IsPairTerm(t)) */ {
    if (TailOfTerm(t) != TermNil) {
      Yap_Error(TYPE_ERROR_EVALUABLE, t,
		"string must contain a single character to be evaluated as an arithmetic expression");
      P = (yamop *)FAILCODE;
      return 0L;
    }
    return Eval(HeadOfTerm(t));
  }
}

Term
Yap_Eval(Term t)
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
  Term out;

  out = Eval(Deref(ARG2));
  if (out == 0L) {
    Yap_Error(EVALUATION_ERROR_INT_OVERFLOW, ARG2, "is/2");
    return FALSE;
  }
  return Yap_unify_constant(ARG1,out);
}

void
Yap_InitEval(void)
{
  /* here are the arithmetical predicates */
  Yap_InitConstExps();
  Yap_InitUnaryExps();
  Yap_InitBinaryExps();
  Yap_InitCPred("is", 2, p_is, SafePredFlag);
}

