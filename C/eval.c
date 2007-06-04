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

#define E_FUNC   blob_type
#define E_ARGS   arith_retptr o
#define USE_E_ARGS   o

#define TMP_BIG()   ((o)->big)
#define RBIG(v)     return(big_int_e)
#define RINT(v)     (o)->Int = v; return(long_int_e)
#define RFLOAT(v)   (o)->dbl = v; return(double_e)
#define RERROR()    return(db_ref_e)

static Term
EvalToTerm(blob_type bt, union arith_ret *res)
{
  switch (bt) {
  case long_int_e:
    return MkIntegerTerm(res->Int);
  case double_e:
    return MkFloatTerm(res->dbl);
#ifdef USE_GMP
  case big_int_e:
    {
      Term t = Yap_MkBigIntTerm(res->big);
      mpz_clear(res->big);
      return t;
    }
#endif
  default:
    return TermNil;
  }
}

static E_FUNC
Eval(Term t, E_ARGS)
{
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,TermNil,"in arithmetic");
    P = (yamop *)FAILCODE;
    RERROR();
  }
  if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    switch ((CELL)fun) {
    case (CELL)FunctorLongInt:
      RINT(LongIntOfTerm(t));
    case (CELL)FunctorDouble:
      RFLOAT(FloatOfTerm(t));
#ifdef USE_GMP
    case (CELL)FunctorBigInt:
      {
	MP_INT *new = TMP_BIG();
	mpz_init_set(new, Yap_BigIntOfTerm(t));
	RBIG(new);
      }
#endif
    default:
      {
	Int n = ArityOfFunctor(fun);
	Atom name  = NameOfFunctor(fun);
	ExpEntry *p;

	if (EndOfPAEntr(p = RepExpProp(Yap_GetExpProp(name, n)))) {
	  Term ti[2];

	  /* error */
	  ti[0] = t;
	  ti[1] = MkIntegerTerm(n);
	  t = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("/"),2), 2, ti);
	  Yap_Error(TYPE_ERROR_EVALUABLE, t,
		"functor %s/%d for arithmetic expression",
		RepAtom(name)->StrOfAE,n);
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
	if (n == 1)
	  return(p->FOfEE.unary(ArgOfTerm(1,t), USE_E_ARGS));
	return(p->FOfEE.binary(ArgOfTerm(1,t),ArgOfTerm(2,t), USE_E_ARGS));
      }
    }
  } else if (IsPairTerm(t)) {
    return(Eval(HeadOfTerm(t), USE_E_ARGS));
  } else if (IsIntTerm(t)) {
    RINT(IntOfTerm(t));
  } else {
    Atom name = AtomOfTerm(t);
    ExpEntry *p;

    if (EndOfPAEntr(p = RepExpProp(Yap_GetExpProp(name, 0)))) {
      Term ti[2], terror;
      
      /* error */
      ti[0] = t;
      ti[1] = MkIntegerTerm(0);
      /* error */
      terror = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("/"),2), 2, ti);
      Yap_Error(TYPE_ERROR_EVALUABLE, terror,
	    "atom %s for arithmetic expression",
	    RepAtom(name)->StrOfAE);
      P = (yamop *)FAILCODE;
      RERROR();
    }
    return(p->FOfEE.constant(USE_E_ARGS));
  }
}

E_FUNC
Yap_Eval(Term t, E_ARGS)
{
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,TermNil,"in arithmetic");
    P = (yamop *)FAILCODE;
    RERROR();
  }
  if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);

    switch ((CELL)fun) {
    case (CELL)FunctorLongInt:
      RINT(LongIntOfTerm(t));
    case (CELL)FunctorDouble:
      RFLOAT(FloatOfTerm(t));
#ifdef USE_GMP
    case (CELL)FunctorBigInt:
      {
	MP_INT *new = TMP_BIG();
	mpz_init_set(new, Yap_BigIntOfTerm(t));
	RBIG(new);
      }
#endif
    default:
      {
	Int n = ArityOfFunctor(fun);
	Atom name  = NameOfFunctor(fun);
	ExpEntry *p;

	if (EndOfPAEntr(p = RepExpProp(Yap_GetExpProp(name, n)))) {
	  Term ti[2];

	  /* error */
	  ti[0] = t;
	  ti[1] = MkIntegerTerm(n);
	  t = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("/"),2), 2, ti);
	  Yap_Error(TYPE_ERROR_EVALUABLE, t,
		"functor %s/%d for arithmetic expression",
		RepAtom(name)->StrOfAE,n);
	  P = (yamop *)FAILCODE;
	  RERROR();
	}
	if (n == 1)
	  return(p->FOfEE.unary(ArgOfTerm(1,t), USE_E_ARGS));
	return(p->FOfEE.binary(ArgOfTerm(1,t),ArgOfTerm(2,t), USE_E_ARGS));
      }
    }
  } else if (IsPairTerm(t)) {
    return(Eval(HeadOfTerm(t), USE_E_ARGS));
  } else if (IsIntTerm(t)) {
    RINT(IntOfTerm(t));
  } else {
    Atom name = AtomOfTerm(t);
    ExpEntry *p;

    if (EndOfPAEntr(p = RepExpProp(Yap_GetExpProp(name, 0)))) {
      Term ti[2], terror;
      
      /* error */
      ti[0] = t;
      ti[1] = MkIntegerTerm(0);
      /* error */
      terror = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("/"),2), 2, ti);
      Yap_Error(TYPE_ERROR_EVALUABLE, terror,
	      "atom %s for arithmetic expression",
	      RepAtom(name)->StrOfAE);
      P = (yamop *)FAILCODE;
      RERROR();
    }
    return(p->FOfEE.constant(USE_E_ARGS));
  }
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
  union arith_ret res;
  blob_type bt;
  Term out;

  bt = Eval(Deref(ARG2), &res);
  out = EvalToTerm(bt,&res);
  if (out == TermNil) {
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
  Yap_InitCPred("is", 2, p_is, TestPredFlag | SafePredFlag);
}

