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
* File:		sbaunify.h						 *
* Last rev:								 *
* mods:									 *
* comments:	Basic abstract machine operations, such as	         *
*               dereferencing, binding, trailing, and unification        *
*               in the SBA model.                                        *
*									 *
*************************************************************************/

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
/*
Int unify(Term t0, Term t1)
*/
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
  deref_head(a,unify_cons_unk);
 unify_cons_nonvar:
  {
    if (a == cons) return(TRUE);
    else if (IsApplTerm(a) && IsExtensionFunctor(FunctorOfTerm(a))) {
      Functor fun = FunctorOfTerm(a);
      if (fun == FunctorDouble)
	return(IsFloatTerm(cons) && FloatOfTerm(a) == FloatOfTerm(cons));
      else if (fun == FunctorLongInt) {
	return(IsLongIntTerm(cons) && LongIntOfTerm(a) == LongIntOfTerm(cons));
#ifdef TERM_EXTENSIONS
      } else if (IsAttachFunc(fun)) {
	return(attas[ExtFromFunctor(fun)].bind_op(SBIND,a,cons));
#endif /* TERM_EXTENSIONS */
      } else
	return(FALSE);
      /* no other factors are accepted as arguments */
    } else return(FALSE);
  }
    

  deref_body(a,pt,unify_cons_unk,unify_cons_nonvar);
  Bind(pt,cons);
  return(TRUE);
}


