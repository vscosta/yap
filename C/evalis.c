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
* File:		evalis.c						 *
* Last rev:								 *
* mods:									 *
* comments:	is/3 predicate						 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif /* SCCS */

/*
 * This predicates had to be developed here because of a bug in the MPW
 * compiler, which was not able to compile the original eval.c 
 */

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "eval.h"


int 
UnEvalInt(BITS16 op, Int i1)
{
  switch(op) {
  case e_uminus:
    REvalInt(-i1);
  case e_abs:
#if SHORT_INTS
#if HAVE_LABS
    REvalInt((Int)labs((long int)i1));
#else
    REvalInt((i1 >= 0 ? i1 : -i1));
#endif
#else
    REvalInt(abs(i1));
#endif
  case e_msb:
    REvalInt(msb(i1));
  case e_uplus:
    REvalInt(i1);
  case e_not:
    REvalInt(~i1);
  case e_exp:
    REvalFl(exp(FL(i1)));
  case e_log:
    REvalFl(log(FL(i1)));
  case e_log10:
    REvalFl(log10(FL(i1)));
  case e_sqrt:
    REvalFl(sqrt(FL(i1)));
  case e_sin:
    REvalFl(sin(FL(i1)));
  case e_cos:
    REvalFl(cos(FL(i1)));
  case e_tan:
    REvalFl(tan(FL(i1)));
  case e_sinh:
    REvalFl(sinh(FL(i1)));
  case e_cosh:
    REvalFl(cosh(FL(i1)));
  case e_tanh:
    REvalFl(tanh(FL(i1)));
  case e_asin:
    REvalFl(asin(FL(i1)));
  case e_acos:
    REvalFl(acos(FL(i1)));
  case e_atan:
    REvalFl(atan(FL(i1)));
  case e_asinh:
    REvalFl(asinh(FL(i1)));
  case e_acosh:
    REvalFl(acosh(FL(i1)));
  case e_atanh:
    REvalFl(atanh(FL(i1)));
  case e_floor:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      Error(TYPE_ERROR_FLOAT, MkIntegerTerm(i1), "floor/1");
      P = (yamop *)FAILCODE;
      REvalError();	    
    } else {
      REvalFl(FL(i1));
    }
  case e_round:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      Error(TYPE_ERROR_FLOAT, MkIntegerTerm(i1), "round/1");
      P = (yamop *)FAILCODE;
      REvalError();	    
    } else {
      REvalFl(FL(i1));
    }
  case e_ceiling:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      Error(TYPE_ERROR_FLOAT, MkIntegerTerm(i1), "floor/1");
      P = (yamop *)FAILCODE;
      REvalError();	    
    } else {
      REvalFl(FL(i1));
    }
  case e_truncate:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      Error(TYPE_ERROR_FLOAT, MkIntegerTerm(i1), "truncate/1");
      P = (yamop *)FAILCODE;
      REvalError();	    
    } else {
      REvalFl(FL(i1));
    }
  case e_integer:
    REvalInt(i1);
  case e_float:
    REvalFl(FL(i1));
  case e_fmodf:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      Error(TYPE_ERROR_FLOAT,MkIntegerTerm(i1),"mod/2");
      P = (yamop *)FAILCODE;
      REvalError();	  
    } else {
      REvalFl(FL(0.0));
    }
  case e_imodf:
    if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
      /* iso does not allow integer arguments to this procedure */
      Error(TYPE_ERROR_FLOAT,MkIntegerTerm(i1),"mod/2");
      P = (yamop *)FAILCODE;
      REvalError();	  
    } else {
      REvalFl(FL(i1));
    }
  case e_sign:
    if (i1 < 0) {
      REvalInt(-1);
    } else if (i1 == 0) {
      REvalInt(0);
    } else {
      REvalInt(1);
    }
  default:
    {
      Term t, ti[2];

      ti[0] = MkAtomTerm(NameOfFunctor(FunctorOfTerm(current_eval_term)));
      ti[1] = MkIntegerTerm(1);
      t = MkApplTerm(MkFunctor(LookupAtom("/"),1), 1, ti);
      Error(TYPE_ERROR_EVALUABLE, t,
	    "arithmetic expression  %s/%d",
	    RepAtom(NameOfFunctor(FunctorOfTerm(current_eval_term)))->StrOfAE,
	    2
	    );
      P = (yamop *)FAILCODE;
      REvalError();
    }
  }
}

Int 
p_unary_is(void)
{
  register BITS16 OpNum;
  Term t2, t3;
  int flag;

  current_eval_term = MkIntTerm(1);
  t2 = Deref(ARG2);
  if (IsVarTerm(t2)) {
    Error(INSTANTIATION_ERROR, t2, "operation for is/3");
    P = (yamop *)FAILCODE;
    return(FALSE);
  }
  if (IsAtomTerm(t2)) {
    Atom            name;
    Prop            p;
    name = AtomOfTerm(t2);
    if ((p = GetExpProp(name, 1)) == NIL) {
      Term t, ti[2];

      ti[0] = MkAtomTerm(name);
      ti[1] = MkIntegerTerm(1);
      t = MkApplTerm(MkFunctor(LookupAtom("/"),2), 2, ti);
      Error(TYPE_ERROR_EVALUABLE, t,
	    "arithmetic expression %s/%d",
	    RepAtom(name)->StrOfAE,
	    1
	    );
      P = (yamop *)FAILCODE;
      return(FALSE);
    }
    OpNum = RepExpProp(p)->ENoOfEE;
  } else if (IsIntTerm(t2))
    OpNum = IntOfTerm(t2);
  else
    return (FALSE);
  t3 = Deref(ARG3);
  if (IsVarTerm(t3)) {
    int op = 0;
    
    while (InitTab[op].eno != OpNum) op++;
    Error(INSTANTIATION_ERROR, t3, "arithmetic expression %s/1", InitTab[op].OpName);
    P = (yamop *)FAILCODE;
    return(FALSE);
  }
  if (IsIntegerTerm(t3)) {
    flag = UnEvalInt(OpNum, IntegerOfTerm(t3));
  } else if (IsFloatTerm(t3)) {
    flag = UnEvalFl(OpNum, FloatOfTerm(t3));    
  } else {
    int aflag = Eval(t3);
    if (aflag == FError) {
      return(FALSE);
    } else if (aflag == FInt) {
      flag = UnEvalInt(OpNum, eval_int);
    } else {
      flag = UnEvalFl(OpNum, eval_flt);
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

