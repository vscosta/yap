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
* File:		unify.c							 *
* Last rev:								 *
* mods:									 *
* comments:	Unification and other auxiliary routines for absmi       *
*									 *
*************************************************************************/

int 
IUnify_complex(register CELL *pt0, register CELL *pt0_end,
	       register CELL *pt1
)
{
#if SHADOW_REGS
#if defined(B) || defined(TR)
  register REGSTORE *regp = &REGS;

#define REGS (*regp)
#endif /* defined(B) || defined(TR) || defined(HB) */
#endif

#if SHADOW_HB
  register CELL *HBREG = HB;
#endif /* SHADOW_HB */

  CELL  **to_visit  = (CELL **)H;

loop:
  while (pt0 < pt0_end) {
    register CELL *ptd0 = pt0+1; 
    register CELL d0;

    ++pt1;
    pt0 = ptd0;
    d0 = *ptd0;
    deref_head(d0, unify_comp_unk);
  unify_comp_nvar:
    {
      register CELL *ptd1 = pt1;
      register CELL d1 = *ptd1;

      deref_head(d1, unify_comp_nvar_unk);
    unify_comp_nvar_nvar:
      if (d0 == d1)
	continue;
      if (IsPairTerm(d0)) {
	if (!IsPairTerm(d1)) {
	  goto cufail;
	}
#ifdef RATIONAL_TREES
	/* now link the two structures so that no one else will */
	/* come here */
	if (pt0 < pt1) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = pt1;
	  to_visit[3] = (CELL *)d0;
	  to_visit += 4;
	  *pt0 = d1;
	}
	else {
	  to_visit[0] = pt1;
	  to_visit[1] = pt1+(pt0_end-pt0);
	  to_visit[2] = pt0;
	  to_visit[3] = (CELL *)d1;
	  to_visit += 4;
	  *pt1 = d0;
	}
#else
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = pt1;
	  to_visit += 3;
	}

#endif
	pt0_end = (pt0 = RepPair(d0) - 1) + 2;
	pt1 = RepPair(d1) - 1;
	continue;
      }
      if (IsApplTerm(d0)) {
	register Functor f;
	register CELL *ap2, *ap3;

	if (!IsApplTerm(d1)) {
	  goto cufail;
	}
	/* store the terms to visit */
	ap2 = RepAppl(d0);
	ap3 = RepAppl(d1);
	f = (Functor) (*ap2);
	/* compare functors */
	if (f != (Functor) *ap3)
	  goto cufail;
	if (IsExtensionFunctor(f)) {
	  if (unify_extension(f, d0, ap2, d1))
	    continue;
	  goto cufail;
	}
#ifdef RATIONAL_TREES
	/* now link the two structures so that no one else will */
	/* come here */
	if (pt0 < pt1) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = pt1;
	  to_visit[3] = (CELL *)d0;
	  to_visit += 4;
	  *pt0 = d1;
	}
	else {
	  to_visit[0] = pt1;
	  to_visit[1] = pt1+(pt0_end-pt0);
	  to_visit[2] = pt0;
	  to_visit[3] = (CELL *)d1;
	  to_visit += 4;
	  *pt1 = d0;
	}
#else
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = pt1;
	  to_visit += 3;
	}
#endif
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
	pt1 = ap3;
	continue;
      }
      goto cufail;

      derefa_body(d1, ptd1, unify_comp_nvar_unk, unify_comp_nvar_nvar);
	/* d1 and pt2 have the unbound value, whereas d0 is bound */
      Bind_Global(ptd1, d0, bind_unify1);
#ifdef COROUTINING
      DO_TRAIL(ptd1, d0);
      if (ptd1 < H0) WakeUp(ptd1);
    bind_unify1:
#endif
      continue;
    }

    derefa_body(d0, ptd0, unify_comp_unk, unify_comp_nvar);
    {
      register CELL d1;
      register CELL *ptd1;

      d1 = pt1[0];
      /* pt2 is unbound */
      ptd1 = pt1;
      deref_head(d1, unify_comp_var_unk);
    unify_comp_var_nvar:
      /* pt2 is unbound and d1 is bound */
      Bind_Global(ptd0, d1, bind_unify2);
#ifdef COROUTINING
      DO_TRAIL(ptd0, d1);
      if (ptd0 < H0) WakeUp(ptd0);
    bind_unify2:
#endif
      continue;

      {

	derefa_body(d1, ptd1, unify_comp_var_unk, unify_comp_var_nvar);
	/* ptd0 and ptd1 are unbound */
	UnifyGlobalCells(ptd0, ptd1, ugc1, ugc2);
#ifdef COROUTINING
	DO_TRAIL(ptd0, (CELL)ptd1);
	if (ptd0 < H0) WakeUp(ptd0);
      ugc1:
#endif
	continue;
#ifdef COROUTINING
      ugc2:
	DO_TRAIL(ptd1, (CELL)ptd0);
	if (ptd1 < H0) WakeUp(ptd1);
	continue;
#endif
      }
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit > (CELL **) H) {
#ifdef RATIONAL_TREES
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
#else
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
#endif
    goto loop;
  }
  return (TRUE);

cufail:
#ifdef RATIONAL_TREES
  /* failure */
  while (to_visit > (CELL **) H) {
    CELL *pt0;
    to_visit -= 4;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[3];
  }
#endif
  return (FALSE);
#if SHADOW_REGS
#if defined(B) || defined(TR)
#undef REGS
#endif /* defined(B) || defined(TR) */
#endif
}

int 
IUnify(register CELL d0, register CELL d1)
{
#if SHADOW_REGS
#if defined(B) || defined(TR)
  register REGSTORE *regp = &REGS;

#define REGS (*regp)
#endif /* defined(B) || defined(TR) */
#endif

#if SHADOW_HB
  register CELL *HBREG = HB;
#endif

  register CELL *pt0, *pt1;

  deref_head(d0, unify_unk);

unify_nvar:
  /* d0 is bound */
  deref_head(d1, unify_nvar_unk);
unify_nvar_nvar:
  /* both arguments are bound */
  if (d0 == d1)
    return (TRUE);
  if (IsPairTerm(d0)) {
    if (!IsPairTerm(d1)) {
      return (FALSE);
    }
    pt0 = RepPair(d0);
    pt1 = RepPair(d1);
    return (IUnify_complex(pt0 - 1, pt0 + 1, pt1 - 1));
  }
  else if (IsApplTerm(d0)) {
    pt0 = RepAppl(d0);
    d0 = *pt0;
    if (!IsApplTerm(d1))
      return (FALSE);      
    pt1 = RepAppl(d1);
    d1 = *pt1;
    if (d0 != d1) {
      return (FALSE);
    } else {
      if (IsExtensionFunctor((Functor)d0)) {
	switch(d0) {
	case (CELL)FunctorDBRef:
	  return(pt0 == pt1);
	case (CELL)FunctorLongInt:
	  return(pt0[1] == pt1[1]);
	case (CELL)FunctorDouble:
	  return(FloatOfTerm(AbsAppl(pt0)) == FloatOfTerm(AbsAppl(pt1)));
#ifdef USE_GMP
	case (CELL)FunctorBigInt:
	  return(mpz_cmp(BigIntOfTerm(AbsAppl(pt0)),BigIntOfTerm(AbsAppl(pt0))) == 0);
#endif /* USE_GMP */
	default:
	  return(FALSE);
	}
      }
      return (IUnify_complex(pt0, pt0 + ArityOfFunctor((Functor) d0),
			     pt1));
    }
  } else {
    return (FALSE);
  }

  deref_body(d1, pt1, unify_nvar_unk, unify_nvar_nvar);
  /* d0 is bound and d1 is unbound */
  Bind(pt1, d0, bind_unify3);
#ifdef COROUTINING
  DO_TRAIL(pt1, d0);
  if (pt1 < H0) WakeUp(pt1);
 bind_unify3:
#endif
  return (TRUE);

  deref_body(d0, pt0, unify_unk, unify_nvar);
  /* pt0 is unbound */
  deref_head(d1, unify_var_unk);
unify_var_nvar:
  /* pt0 is unbound and d1 is bound */
  Bind(pt0, d1, bind_unify4);
#ifdef COROUTINING
  DO_TRAIL(pt0, d1);
  if (pt0 < H0) WakeUp(pt0);
 bind_unify4:
#endif
  return (TRUE);

#if TRAILING_REQUIRES_BRANCH
unify_var_nvar_trail:
  DO_TRAIL(pt0);
  return (TRUE);
#endif

  deref_body(d1, pt1, unify_var_unk, unify_var_nvar);
  /* d0 and pt1 are unbound */
  UnifyCells(pt0, pt1, uc1, uc2);
#ifdef COROUTINING
  DO_TRAIL(pt0, (CELL)pt1);
  if (pt0 < H0) WakeUp(pt0);
 uc1:
#endif
  return (TRUE);
#ifdef COROUTINING
 uc2:
  DO_TRAIL(pt1, (CELL)pt0);
  if (pt1 < H0) {
    WakeUp(pt1);
  }
  return (TRUE);
#endif
#if SHADOW_REGS
#if defined(B) || defined(TR)
#undef REGS
#endif /* defined(B) || defined(TR) */
#endif
}

