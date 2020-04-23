/************************************************************************
**                                                                     **
**                   The YapTab/YapOr/OPTYap systems                   **
**                                                                     **
** YapTab extends the Yap Prolog engine to support sequential tabling  **
** YapOr extends the Yap Prolog engine to support or-parallelism       **
** OPTYap extends the Yap Prolog engine to support or-parallel tabling **
**                                                                     **
**                                                                     **
**      Yap Prolog was developed at University of Porto, Portugal      **
**                                                                     **
************************************************************************/

#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif /* SCCS */

/************************************************************

Unification Routines

*************************************************************/

static inline
Int bind_variable(Term t0, Term t1)
{
  tr_fr_ptr TR0 = TR;
  if (Yap_IUnify(t0,t1)) {
    return(TRUE);
  } else {
    while(TR != TR0) {
      CELL *p = (CELL *)TrailTerm(--TR);
      RESET_VARIABLE(p);
    }
    return(FALSE);
  }
}

EXTERN inline
Int unify(Term t0, Term t1)
{
  tr_fr_ptr TR0 = TR;
  if (Yap_IUnify(t0,t1)) {
    return(TRUE);
  } else {
    while(TR != TR0) {
      CELL *p = (CELL *)TrailTerm(--TR);
      RESET_VARIABLE(p);
    }
    return(FALSE);
  }
}

EXTERN inline Int unify_constant(register Term a, register Term cons)
{
  CELL *pt;
  CELL *pt0, *pt1;

  deref_head(a,unify_cons_unk);
 unify_cons_nonvar:
  {
    if (a == cons) return(TRUE);
    else if (IsApplTerm(a) && IsExtensionFunctor(FunctorOfTerm(a))) {
      Functor fun = FunctorOfTerm(a);
      if (!IsApplTerm(cons) || FunctorOfTerm(cons) != fun)
	return FALSE;
      switch((CELL)fun) {
      case (CELL)FunctorDBRef:
	return(pt0 == pt1);
      case (CELL)FunctorLongInt:
	return(pt0[1] == pt1[1]);
      case (CELL)FunctorString:
	return(strcmp( (const char *)(pt0+2),  (const char *)(pt1+2)) == 0);
      case (CELL)FunctorDouble:
	return(FloatOfTerm(AbsAppl(pt0)) == FloatOfTerm(AbsAppl(pt1)));
#ifdef USE_GMP
      case (CELL)FunctorBigInt:
	return(Yap_gmp_tcmp_big_big(AbsAppl(pt0),AbsAppl(pt0)) == 0);
#endif /* USE_GMP */
      default:
	return(FALSE);
      }
    }
    /* no other factors are accepted as arguments */
    return(FALSE);
  }
    

  deref_body(a,pt,unify_cons_unk,unify_cons_nonvar);
  YapBind(pt,cons);
  return(TRUE);
}


