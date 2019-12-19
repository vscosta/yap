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
#ifndef COROUT_C
#define COROUT_C
#endif

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "heapgc.h"
#include "attvar.h"
#ifndef NULL
#define NULL (void *)0
#endif

#ifdef COROUTINING

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

/* This routine verifies whether two complex structures can unify. */
static int can_unify_complex(register CELL *pt0, register CELL *pt0_end,
                             register CELL *pt1, Term *Vars USES_REGS) {

  /* This is really just unification, folks */
  tr_fr_ptr saved_TR;
  CELL *saved_HB;
  choiceptr saved_B;
  Ystack_t stt;
  init_stack( &stt, 1024 );

  /* make sure to trail all bindings */
  saved_TR = TR;
  saved_B = B;
  saved_HB = HB;
  HB = HR;

loop:
  while (pt0 < pt0_end) {
    register CELL d0, d1;
    ++pt0;
    ++pt1;
    d0 = Derefa(pt0);
    d1 = Derefa(pt1);
    if (IsVarTerm(d0)) {
      if (IsVarTerm(d1)) {
        if (d0 != d1) {
          /* we need to suspend on both variables ! */
          *Vars = AddVarIfNotThere(d0, AddVarIfNotThere(d1, *Vars PASS_REGS)
                                           PASS_REGS);
          /* bind the two variables, we would have to do that to unify
             them */
          if (d1 > d0) { /* youngest */
            /* we don't want to wake up goals */
            Bind_Global_NonAtt((CELL *)d1, d0);
          } else {
            Bind_Global_NonAtt((CELL *)d0, d1);
          }
        }
        /* continue the loop */
        continue;
      } else {
        /* oh no, some more variables! */
        *Vars = AddVarIfNotThere(d0, *Vars PASS_REGS);
      }
      /* now bind it */
      Bind_Global_NonAtt((CELL *)d0, d1);
      /* continue the loop */
    } else if (IsVarTerm(d1)) {
      *Vars = AddVarIfNotThere(d1, *Vars PASS_REGS);
      /* and bind it */
      Bind_Global_NonAtt((CELL *)d1, d0);
      /* continue the loop */
    } else {
      if (d0 == d1)
        continue;
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) {
        if (d0 != d1)
          goto comparison_failed;
        /* else continue the loop */
      } else if (IsPairTerm(d0)) {
        if (!IsPairTerm(d1))
          goto comparison_failed;
        to_visit->pt0 = pt0;
        to_visit->pt0_end = pt0_end;
        to_visit->ptf = pt1;
        to_visit->oldv = *pt0;
        to_visit->oldp = pt0;
	to_visit++;
        pt0 = RepPair(d0) - 1;
        pt0_end = RepPair(d0) + 1;
        pt1 = RepPair(d1) - 1;
        continue;
      } else if (IsApplTerm(d0)) {
        register Functor f;
        register CELL *ap2, *ap3;
        if (!IsApplTerm(d1)) {
          goto comparison_failed;
        } else {
          /* store the terms to visit */
          ap2 = RepAppl(d0);
          ap3 = RepAppl(d1);
          f = (Functor)(*ap2);
          /* compare functors */
          if (f != (Functor)*ap3) {
            goto comparison_failed;
          }
          if (IsExtensionFunctor(f)) {
            switch ((CELL)f) {
            case (CELL) FunctorDBRef:
              if (d0 == d1)
                continue;
              goto comparison_failed;
            case (CELL) FunctorLongInt:
              if (ap2[1] == ap3[1])
                continue;
              goto comparison_failed;
            case (CELL) FunctorDouble:
              if (FloatOfTerm(d0) == FloatOfTerm(d1))
                continue;
              goto comparison_failed;
            case (CELL) FunctorString:
              if (strcmp((char *)StringOfTerm(d0), (char *)StringOfTerm(d1)) ==
                  0)
                continue;
              goto comparison_failed;
#ifdef USE_GMP
            case (CELL) FunctorBigInt:
              if (Yap_gmp_tcmp_big_big(d0, d1) == 0)
                continue;
              goto comparison_failed;
#endif /* USE_GMP */
            default:
              goto comparison_failed;
            }
          }
        to_visit->pt0 = pt0;
        to_visit->pt0_end = pt0_end;
        to_visit->ptf = pt1;
        to_visit->oldv = *pt0;
        to_visit->oldp = pt0;
	to_visit++;
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
  if (to_visit > to_visit0) {
    to_visit --;
    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    pt1 = to_visit->ptf;
    *to_visit->oldp = to_visit->oldv;
    goto loop;
  }
  /* success */
  /* restore B, and later HB */
  B = saved_B;
  HB = saved_HB;
  /* untrail all bindings made by IUnify */
  while (TR != saved_TR) {
    pt1 = (CELL *)(TrailTerm(--TR));
    RESET_VARIABLE(pt1);
  }
  close_stack(&stt);
  return (true);

comparison_failed:
  /* failure */
  while (to_visit > to_visit0) {
    to_visit --;
    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    pt1 = to_visit->ptf;
    *to_visit->oldp = to_visit->oldv;
  }
  /* restore B, and later HB */
  B = saved_B;
  HB = saved_HB;
  /* untrail all bindings made by IUnify */
  while (TR != saved_TR) {
    pt1 = (CELL *)(TrailTerm(--TR));
    RESET_VARIABLE(pt1);
  }
  /* the system will take care of TR for me, no need to worry here! */
  close_stack(&stt);
  return (FALSE);
}

static int can_unify(Term t1, Term t2, Term *Vars USES_REGS) {
  t1 = Deref(t1);
  t2 = Deref(t2);
  if (t1 == t2) {
    *Vars = TermNil;
    return TRUE;
  }
  if (IsVarTerm(t1)) {
    /* we know for sure  they can't be different */
    if (IsVarTerm(t2)) {
      /* we need to suspend on both variables because otherwise
         Y = susp(_) would not wakeup susp ! */
      *Vars = MkPairTerm(t1, MkPairTerm(t2, TermNil));
      return TRUE;
    } else {
      *Vars = MkPairTerm(t1, TermNil);
      return TRUE;
    }
  } else if (IsVarTerm(t2)) {
    /* wait until t2 is bound */
    *Vars = MkPairTerm(t2, TermNil);
    return TRUE;
  }
  /* Two standard terms at last! */
  if (IsAtomOrIntTerm(t1) || IsAtomOrIntTerm(t2)) {
    /* Two primitive terms can only be equal if they are
       the same. If they are, $eq succeeds without further ado.
       */
    if (t1 != t2)
      return FALSE;
    else {
      *Vars = TermNil;
      return TRUE;
    }
  } else if (IsPairTerm(t1)) {
    if (IsPairTerm(t2)) {
      return (can_unify_complex(RepPair(t1) - 1, RepPair(t1) + 1,
                                RepPair(t2) - 1, Vars PASS_REGS));
    } else
      return FALSE;
  } else {
    Functor f = FunctorOfTerm(t1);
    if (f != FunctorOfTerm(t2))
      return FALSE;
    if (IsExtensionFunctor(f)) {
      switch ((CELL)f) {
      case (CELL) FunctorDBRef:
        if (t1 == t2)
          return FALSE;
        return FALSE;
      case (CELL) FunctorLongInt:
        if (RepAppl(t1)[1] == RepAppl(t2)[1])
          return (TRUE);
        return FALSE;
      case (CELL) FunctorString:
        if (strcmp((char *)StringOfTerm(t1), (char *)StringOfTerm(t2)) == 0)
          return (TRUE);
        return FALSE;
      case (CELL) FunctorDouble:
        if (FloatOfTerm(t1) == FloatOfTerm(t2))
          return (TRUE);
        return FALSE;
#ifdef USE_GMP
      case (CELL) FunctorBigInt:
        if (Yap_gmp_tcmp_big_big(t1, t2) == 0)
          return (TRUE);
        return (FALSE);
#endif /* USE_GMP */
      default:
        return FALSE;
      }
    }
    /* Two complex terms with the same functor */
    return can_unify_complex(RepAppl(t1), RepAppl(t1) + ArityOfFunctor(f),
                             RepAppl(t2), Vars PASS_REGS);
  }
}

/* This routine verifies whether a complex has variables. */
static int non_ground_complex( CELL *pt0,  CELL *pt0_end,
                              Term *Var USES_REGS) {

  Ystack_t stt;
  init_stack( &stt, 1024 );

loop:
  while (pt0 < pt0_end) {
     CELL d0;
    ++pt0;
    d0 = Derefa(pt0);
    if (IsVarTerm(d0)) {
      *Var = d0;
      goto var_found;
    }
    if (IsPairTerm(d0)) {
      if (to_visit + 1024 >= to_visit_end) {
        goto aux_overflow;
      }
        to_visit->pt0 = pt0;
        to_visit->pt0_end = pt0_end;
        to_visit->oldv = *pt0;
        to_visit->oldp = pt0;
	to_visit++;
	*pt0 = TermNil;
	pt0 = RepPair(d0) - 1;
      pt0_end = RepPair(d0) + 1;
    } else if (IsApplTerm(d0)) {
      register Functor f;
      register CELL *ap2;

      /* store the terms to visit */
      ap2 = RepAppl(d0);
      f = (Functor)(*ap2);

      if (IsExtensionFunctor(f)) {
        continue;
      }
      if (to_visit + 1024 >= to_visit_end) {
        goto aux_overflow;
      }
        to_visit->pt0 = pt0;
        to_visit->pt0_end = pt0_end;
        to_visit->oldv = *pt0;
        to_visit->oldp = pt0;
      *pt0 = TermNil;
      to_visit++;
      d0 = ArityOfFunctor(f);
      pt0 = ap2;
      pt0_end = ap2 + d0;
    }
    /* just continue the loop */
  }

  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    goto loop;
  }

  /* the term is ground */
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit);
  return FALSE;

var_found:
  /* the term is non-ground */
  while (to_visit > to_visit0) {
    to_visit --;
    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    *to_visit->oldp = to_visit->oldv;
  }
  close_stack(&stt);
  /* the system will take care of TR for me, no need to worry here! */
  return TRUE;

aux_overflow:
  /* unwind stack */
  while (to_visit > to_visit0) {
    to_visit --;
    pt0 = to_visit->pt0;
    pt0_end = to_visit->pt0_end;
    *to_visit->oldp = to_visit->oldv;
  }
  close_stack(&stt);
  return -1;
}

static int non_ground(Term t, Term *Var USES_REGS) {
  int out = -1;
  while (out < 0) {
    t = Deref(t);
    if (IsVarTerm(t)) {
      /* we found a variable */
      *Var = t;
      return TRUE;
    }
    if (IsPrimitiveTerm(t)) {
      return FALSE;
    } else if (IsPairTerm(t)) {
      out = non_ground_complex(RepPair(t) - 1, RepPair(t) + 1, Var PASS_REGS);
      if (out >= 0)
        return out;
    } else {
      Functor f = FunctorOfTerm(t);
      if (IsExtensionFunctor(f)) {
        return FALSE;
      }
      out = non_ground_complex(RepAppl(t),
                               RepAppl(t) + ArityOfFunctor(FunctorOfTerm(t)),
                               Var PASS_REGS);
      if (out >= 0)
        return out;
    }
    if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
      Yap_Error(RESOURCE_ERROR_AUXILIARY_STACK, ARG1, "overflow in ground");
      return FALSE;
    }
  }
  return FALSE;
}

#endif

/* check whether the two terms unify and return what variables should
   be bound before the terms are exactly equal */
static Int p_can_unify(USES_REGS1) {
#ifdef COROUTINING
  Term r = TermNil;
  if (!can_unify(ARG1, ARG2, &r PASS_REGS))
    return FALSE;
  return Yap_unify(ARG3, r);
#else
  return FALSE;
#endif
}

/* if the term is not ground return a variable in the term */
static Int p_non_ground(USES_REGS1) {
#ifdef COROUTINING
  Term r = TermNil;
  if (!non_ground(ARG1, &r PASS_REGS))
    return (FALSE);
  return (Yap_unify(ARG2, r));
#else
  return (FALSE);
#endif
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
  Yap_UpdateTimedVar(LOCAL_WokenGoals, TermNil);
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
  Yap_InitCPred("$can_unify", 3, p_can_unify, SafePredFlag);
  Yap_InitCPred("$non_ground", 2, p_non_ground, SafePredFlag);
  Yap_InitCPred("$coroutining", 0, p_coroutining, SafePredFlag);
  Yap_InitCPred("$awoken_goals", 1, p_awoken_goals, SafePredFlag);
}

