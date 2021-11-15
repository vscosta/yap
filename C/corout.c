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
* File:		corout.c						 *
* Last rev:								 *
* mods:									 *
* comments:	Co-routining from within YAP				 *
*									 *
*************************************************************************/


#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "heapgc.h"
#include "attvar.h"
#ifndef NULL
#define NULL (void *)0
#endif


/* check if variable was there */
static Term AddVarIfNotThere(Term var, Term dest USES_REGS) {
  Term test = dest;
  while (test != TermNil) {
    if ((RepPair(test))[0] == var)
      return (dest);
    else
      test = (RepPair(test))[1];
  }
  return (MkPairTerm(var, dest));
}


/* check whether the two terms unify and return what variables should
   be bound before the terms are exactly equal */
static Int unifiable(USES_REGS1) {
  bool err;
  Term o = TermNil;
  do {
    tr_fr_ptr oTR = TR;
    HB = (CELL*)B;
    err = false;
    if (!Yap_unify(ARG1,ARG2)) {
      HB=B->cp_h;
      return false;
    }
      HB=B->cp_h;
      while (HR+(TR-oTR)*5> ASP-1024) {
	err=true;
       Yap_dogc();
      }
    while(TR>oTR) {
	Term ts[2];
	TR--;
	ts[0]=TrailTerm(TR);
	CELL *p = VarOfTerm(ts[0]);
	ts[1] = *p;
	RESET_VARIABLE(p);
	o = MkPairTerm(Yap_MkApplTerm(FunctorEq,2,ts),o);
    }
  }    while (err);
  return Yap_unify(ARG3, o);
}


/* if the term is not ground return a variable in the term */
static Int p_coroutining(USES_REGS1) {
#ifdef COROUTINING
  return (TRUE);
#else
  return (FALSE);
#endif
}

#if COROUTINING
static Term ListOfWokenGoals(USES_REGS1) {
  return Yap_ReadTimedVar(LOCAL_WokenGoals);
}

Term Yap_ListOfWokenGoals(void) {
  CACHE_REGS
  return ListOfWokenGoals(PASS_REGS1);
}
#endif

/* return a list of awoken goals */
static Int p_awoken_goals(USES_REGS1) {
#ifdef COROUTINING
  Term WGs = Yap_ReadTimedVar(LOCAL_WokenGoals);
  if (WGs == TermNil) {
    return (FALSE);
  }
  WGs = ListOfWokenGoals(PASS_REGS1);
  Yap_UpdateTimedVar(LOCAL_WokenGoals, TermTrue);
  return (Yap_unify(ARG1, WGs));
#else
  return (FALSE);
#endif
}

static Int p_yap_has_rational_trees(USES_REGS1) {
#if RATIONAL_TREES
  return TRUE;
#else
  return FALSE;
#endif
}

static Int p_yap_has_coroutining(USES_REGS1) {
#if COROUTINING
  return TRUE;
#else
  return FALSE;
#endif
}

void Yap_InitCoroutPreds(void) {
#ifdef COROUTINING
  Atom at;
  PredEntry *pred;

  at = AtomWakeUpGoal;
  pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 2), 0));
  WakeUpCode = pred;
#endif
  Yap_InitAttVarPreds();
  Yap_InitCPred("$yap_has_rational_trees", 0, p_yap_has_rational_trees,
                SafePredFlag);
  Yap_InitCPred("$yap_has_coroutining", 0, p_yap_has_coroutining, SafePredFlag);
  Yap_InitCPred("$can_unify", 3, unifiable, 0);
  Yap_InitCPred("$coroutining", 0, p_coroutining, SafePredFlag);
  Yap_InitCPred("$awoken_goals", 1, p_awoken_goals, SafePredFlag);
}
