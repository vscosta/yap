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
* File:		evaltwo.c						 *
* Last rev:								 *
* mods:									 *
* comments:	is/4 predicate						 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif 

/*
 * This predicates had to be developed here because of a bug in the MPW
 * compiler, which was not able to compile the original eval.c 
 */

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "eval.h"

#define IntRes(X)       return(unify_constant(ARG1,MkIntegerTerm(X)))
#define FloatRes(X)	return(unify_constant(ARG1,MkEvalFl(X)))

int 
BinEvalInt(BITS16 op, Int i1, Int i2)
{
  switch(op) {
  case e_plus:
    REvalInt(i1 + i2);
  case e_dif:
    REvalInt(i1 - i2);
  case e_times:
    REvalInt(i1 * i2);
  case e_div:
#ifdef TRY_TO_CONVERT_FLOATS_TO_INTS
    if (i1 % i2 == 0)
      REvalInt(i1 / i2);
#endif
    REvalFl(FL(i1) / FL(i2));
  case e_and:
    REvalInt(i1 & i2);
  case e_xor:
    REvalInt(i1 ^ i2);
  case e_or:
    REvalInt(i1 | i2);
  case e_lshift:
    REvalInt(i1 << i2);
  case e_rshift:
    REvalInt(i1 >> i2);
  case e_mod:
    REvalInt(i1 % i2);
  case e_idiv:
    REvalInt(i1 / i2);
  case e_gcd:
    REvalInt(gcd(abs(i1),abs(i2)));
  case e_gcdmult:
    {
      Int i;
      REvalInt(gcdmult(abs(i1),abs(i2), &i));
    }
  case e_min:
    REvalInt((i1 < i2 ? i1 : i2));
  case e_max:
    REvalInt((i1 > i2 ? i1 : i2));
  case e_power:
    REvalFl(pow(FL(i1), FL(i2)));
  case e_atan2:
    REvalFl(atan2(FL(i1), FL(i2)));
  default:
    {
      Term t, ti[2];

      ti[0] = MkAtomTerm(NameOfFunctor(FunctorOfTerm(current_eval_term)));
      ti[1] = MkIntegerTerm(2);
      t = MkApplTerm(MkFunctor(LookupAtom("/"),2), 2, ti);
      Error(TYPE_ERROR_EVALUABLE, t,
	    "in arithmetic expression  %s(%d,%d)",
	    RepAtom(NameOfFunctor(FunctorOfTerm(current_eval_term)))->StrOfAE,
	    i1,
	    i2
	    );
      P = (yamop *)FAILCODE;
      REvalError();
    }
  }
}

int 
BinEvalFl(BITS16 op, Float f1, Float f2, int flts)
{
  switch(op) {
  case e_plus:
    REvalFl(f1 + f2);
  case e_dif:
    REvalFl(f1 - f2);
  case e_times:
    REvalFl(f1 * f2);
  case e_div:
    REvalFl(f1 / f2);
  case e_power:
    REvalFl(pow(f1, f2));
  case e_atan2:
    REvalFl(atan2(f1, f2));
  case e_min:
    REvalFl((f1 < f2 ? f1 : f2));
  case e_max:
    REvalFl((f1 > f2 ? f1 : f2));
  case e_lshift:
    if (flts & 1)
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f1), "<</2");
    else 
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f2), "<</2");
    P = (yamop *)FAILCODE;
    REvalError();
  case e_rshift:
    if (flts & 1)
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f1), ">>/2");
    else 
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f2), ">>/2");
    P = (yamop *)FAILCODE;
    REvalError();
  case e_and:
    if (flts & 1)
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f1), "/\\/2");
    else 
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f2), "/\\/2");
    P = (yamop *)FAILCODE;
    REvalError();
  case e_xor:
    if (flts & 1)
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f1), "#/2");
    else 
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f2), "#/2");
    P = (yamop *)FAILCODE;
    REvalError();
  case e_or:
    if (flts & 1)
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f1), "\\/ /2");
    else 
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f2), "\\/ /2");
    P = (yamop *)FAILCODE;
    REvalError();
  case e_mod:
    if (flts & 1)
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f1), "mod/2");
    else 
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f2), "mod/2");
    P = (yamop *)FAILCODE;
    REvalError();
  case e_idiv:
    if (flts & 1)
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f1), "/ /2");
    else 
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f2), "/ /2");
    P = (yamop *)FAILCODE;
    REvalError();
  case e_gcd:
    if (flts & 1)
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f1), "gcd/2");
    else 
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f2), "gcd/2");
    P = (yamop *)FAILCODE;
    REvalError();
  case e_gcdmult:
    if (flts & 1)
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f1), "gcdmult/2");
    else 
      Error(TYPE_ERROR_INTEGER, MkFloatTerm(f2), "gcdmult/2");
    P = (yamop *)FAILCODE;
    REvalError();
  default:
    {
      Term t, ti[2];
      
      ti[0] = MkAtomTerm(NameOfFunctor(FunctorOfTerm(current_eval_term)));
      ti[1] = MkIntegerTerm(2);
      t = MkApplTerm(MkFunctor(LookupAtom("/"),2), 2, ti);
      Error(TYPE_ERROR_EVALUABLE, t,
	    "in arithmetic expression %s(%d,%d)",
	    RepAtom(NameOfFunctor(FunctorOfTerm(current_eval_term)))->StrOfAE,
	    f1,
	    f2
	    );
      P = (yamop *)FAILCODE;
    }
    REvalError();
  }
}

Int
p_binary_is(void)
{
  register BITS16 OpNum;
  Term t2,t3,t4;
  Int i1;
  Float f1;
  int flag;

  current_eval_term = MkIntTerm(2);
  t2 = Deref(ARG2);
  if (IsVarTerm(t2)) {
    Error(INSTANTIATION_ERROR, t2, "operation for is/4");
    P = (yamop *)FAILCODE;
    return(FALSE);
  }
  if (IsIntTerm(t2))
    OpNum = IntOfTerm(t2);
  else if (IsAtomTerm(t2)) {
    Atom            name = AtomOfTerm(t2);
    Prop            p;
    if ((p = GetExpProp(name, 2)) == NIL) {
      Term t, ti[2];

      ti[0] = MkIntegerTerm(2);
      ti[0] = MkAtomTerm(name);
      t = MkApplTerm(MkFunctor(LookupAtom("/"),2), 2, ti);
      Error(TYPE_ERROR_EVALUABLE, t,
	    "arithmetic expression %s/%d",
	    RepAtom(name)->StrOfAE,
	    2
	    );
      P = (yamop *)FAILCODE;
      return(FALSE);
    }
    OpNum = RepExpProp(p)->ENoOfEE;
  } else
    return (FALSE);
  t3 = Deref(ARG3);
  t4 = Deref(ARG4);
  if (IsVarTerm(t3) || IsVarTerm(t4)) {
    int op = 0;
    
    while (InitTab[op].eno != OpNum) op++;
    Error(INSTANTIATION_ERROR, (IsVarTerm(t3) ? t3 : t4),
	  "arithmetic expression %s/2", InitTab[op].OpName);
    return(FALSE);
  } 
  if (IsIntegerTerm(t3)) {
    i1 = IntegerOfTerm(t3);
  t3_int:
    if (IsIntegerTerm(t4)) {
      flag = BinEvalInt(OpNum, i1, IntegerOfTerm(t4));
    } else if (IsFloatTerm(t4)) {
      flag = BinEvalFl(OpNum, FL(i1), FloatOfTerm(t4), 2);
    } else {
      int aflag = Eval(t4);
      if (aflag == FError) {
	return(FALSE);
      } else if (aflag == FInt) {
	flag = BinEvalInt(OpNum, i1, eval_int);
      } else {
	flag = BinEvalFl(OpNum, FL(i1), eval_flt, 2);
      }
    }
  } else if (IsFloatTerm(t3)) {
    f1 = FloatOfTerm(t3);
  t3_flt:
    if (IsIntegerTerm(t4)) {
      flag = BinEvalFl(OpNum, f1, FL(IntegerOfTerm(t4)), 1);
    } else if (IsFloatTerm(t4)) {
      flag = BinEvalFl(OpNum, f1, FloatOfTerm(t4), 3);
    } else {
      int aflag = Eval(t4);
      if (aflag == FError) {
	return(FALSE);
      } else if (aflag == FInt) {
	flag = BinEvalFl(OpNum, f1, eval_int, 1);
      } else {
	flag = BinEvalFl(OpNum, f1, eval_flt, 3);
      }
    }
  } else {
    int aflag = Eval(t3);
    if (aflag == FError) {
      return(FALSE);
    } else if (aflag == FInt) {
      i1 = eval_int;
      goto t3_int;
    } else {
      f1 = eval_flt;
      goto t3_flt;
    }
  }
  if (flag == FError) {
    return(FALSE);
  } else if (flag == FInt) {
    return(unify_constant(ARG1,MkIntegerTerm(eval_int)));
  } else {
    return(unify_constant(ARG1,MkFloatTerm(eval_flt)));    
  }
}
