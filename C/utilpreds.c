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
* File:		utilpreds.c						 *
* Last rev:	4/03/88							 *
* mods:									 *
* comments:	new utility predicates for YAP				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "@(#)utilpreds.c	1.3";
#endif

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "yapio.h"
#include "eval.h"

typedef struct {
	Term            old_var;
	Term            new_var;
}              *vcell;


STATIC_PROTO(int   copy_complex_term, (CELL *, CELL *, CELL *, CELL *));
STATIC_PROTO(CELL  vars_in_complex_term, (CELL *, CELL *, Term));
STATIC_PROTO(Int   p_non_singletons_in_term, (void));
STATIC_PROTO(CELL  non_singletons_in_complex_term, (CELL *, CELL *));
STATIC_PROTO(Int   p_variables_in_term, (void));
STATIC_PROTO(Int   ground_complex_term, (CELL *, CELL *));
STATIC_PROTO(Int   p_ground, (void));
STATIC_PROTO(Int   p_copy_term, (void));
STATIC_PROTO(Int   var_in_complex_term, (CELL *, CELL *, Term));

#ifdef DEBUG
STATIC_PROTO(Int  p_force_trail_expansion, (void));
#endif /* DEBUG */

static inline void
clean_tr(tr_fr_ptr TR0) {
  if (TR != TR0) {
    do {
      Term p = TrailTerm(--TR);
	RESET_VARIABLE(p);
    } while (TR != TR0);
  }
}

static int
copy_complex_term(register CELL *pt0, register CELL *pt0_end, CELL *ptf, CELL *HLow)
{

  CELL **to_visit0, **to_visit = (CELL **)Yap_PreAllocCodeSpace();
  CELL *HB0 = HB;
  tr_fr_ptr TR0 = TR;
#ifdef COROUTINING
  CELL *dvars = NULL;
#endif
  HB = HLow;

  to_visit0 = to_visit;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, copy_term_unk);
  copy_term_nvar:
    {
      if (IsPairTerm(d0)) {
	CELL *ap2 = RepPair(d0);
	if (ap2 >= HB && ap2 < H) {
	  /* If this is newer than the current term, just reuse */
	  *ptf++ = d0;
	  continue;
	} 
	*ptf = AbsPair(H);
	ptf++;
#ifdef RATIONAL_TREES
	if (to_visit + 4 >= (CELL **)Yap_GlobalBase) {
	  goto heap_overflow;
	}
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = ptf;
	to_visit[3] = (CELL *)*pt0;
	/* fool the system into thinking we had a variable there */
	*pt0 = AbsPair(H);
	to_visit += 4;
#else
	if (pt0 < pt0_end) {
	  if (to_visit + 3 >= (CELL **)Yap_GlobalBase) {
	    goto heap_overflow;
	  }
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = ptf;
	  to_visit += 3;
	}
#endif
	pt0 = ap2 - 1;
	pt0_end = ap2 + 1;
	ptf = H;
	H += 2;
	if (H > ASP - 2048) {
	  goto overflow;
	}
      } else if (IsApplTerm(d0)) {
	register Functor f;
	register CELL *ap2;
	/* store the terms to visit */
	ap2 = RepAppl(d0);
	if (ap2 >= HB && ap2 < H) {
	  /* If this is newer than the current term, just reuse */
	  *ptf++ = d0;
	  continue;
	} 
	f = (Functor)(*ap2);

	if (IsExtensionFunctor(f)) {
	    {
	      *ptf++ = d0;  /* you can just copy other extensions. */
	    }
	  continue;
	}
	*ptf = AbsAppl(H);
	ptf++;
	/* store the terms to visit */
#ifdef RATIONAL_TREES
	if (to_visit + 4 >= (CELL **)Yap_GlobalBase) {
	  goto heap_overflow;
	}
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = ptf;
	to_visit[3] = (CELL *)*pt0;
	/* fool the system into thinking we had a variable there */
	*pt0 = AbsAppl(H);
	to_visit += 4;
#else
	if (pt0 < pt0_end) {
	  if (to_visit + 3 >= (CELL **)Yap_GlobalBase) {
	    goto heap_overflow;
	  }
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = ptf;
	  to_visit += 3;
	}
#endif
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
	/* store the functor for the new term */
	H[0] = (CELL)f;
	ptf = H+1;
	H += 1+d0;
	if (H > ASP - 2048) {
	  goto overflow;
	}
      } else {
	/* just copy atoms or integers */
	*ptf++ = d0;
      }
      continue;
    }
    

    derefa_body(d0, ptd0, copy_term_unk, copy_term_nvar);
    if (ptd0 >= HLow && ptd0 < H) { 
      /* we have already found this cell */
      *ptf++ = (CELL) ptd0;
    } else {
#if COROUTINING
      if (IsAttachedTerm((CELL)ptd0)) {
	/* if unbound, call the standard copy term routine */
	CELL **bp[1];
	
	if (dvars == NULL) {
	  dvars = (CELL *)Yap_ReadTimedVar(DelayedVars);
	} 	
	if (ptd0 >= dvars) {
	  *ptf++ = (CELL) ptd0;
	} else {
	  bp[0] = to_visit;
	  HB = HB0;
	  if (!attas[ExtFromCell(ptd0)].copy_term_op(ptd0, bp, ptf)) {
	    goto overflow;
	  }
	  to_visit = bp[0];
	  HB = HLow;
	  ptf++;
	  Bind_Global(ptd0, ptf[-1]);
	}
      } else {
#endif
	/* first time we met this term */
	RESET_VARIABLE(ptf);
	Bind_Global(ptd0, (CELL)ptf);
	ptf++;
#ifdef COROUTINING
      }
#endif
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
#ifdef RATIONAL_TREES
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    ptf = to_visit[2];
    *pt0 = (CELL)to_visit[3];
#else
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    ptf = to_visit[2];
#endif
    goto loop;
  }

  /* restore our nice, friendly, term to its original state */
  HB = HB0;
  clean_tr(TR0);
  return(0);

 overflow:
  /* oops, we're in trouble */
  H = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    ptf = to_visit[2];
    *pt0 = (CELL)to_visit[3];
  }
#endif
  clean_tr(TR0);
  return(-1);

 heap_overflow:
  /* oops, we're in trouble */
  H = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    ptf = to_visit[2];
    *pt0 = (CELL)to_visit[3];
  }
#endif
  clean_tr(TR0);
  return(-2);
}

static Term
CopyTerm(Term inp) {
  Term t = Deref(inp);

  if (IsVarTerm(t)) {
#if COROUTINING
    if (IsAttachedTerm(t)) {
      CELL *Hi;
      int res;
    restart_attached:

      *H = t;
      Hi = H+1;
      H += 2;
      if ((res = copy_complex_term(Hi-2, Hi-1, Hi, Hi)) < 0) {
	ARG1 = t;
	if (res == -1) { /* handle overflow */
	  if (!Yap_gc(2, ENV, P)) {
	    Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	    return(FALSE);
	  }
	  t = Deref(ARG1);
	  goto restart_attached;
	} else { /* handle overflow */
	  if (!Yap_growheap(FALSE, 0, NULL)) {
	    Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
	    return(FALSE);
	  }
	  t = Deref(ARG1);
	  goto restart_attached;
	}
      }
      return(Hi[0]);
    }
#endif
    return(MkVarTerm());
  } else if (IsPrimitiveTerm(t)) {
    return(t);
  } else if (IsPairTerm(t)) {
    Term tf;
    CELL *ap;
    CELL *Hi;

  restart_list:
    ap = RepPair(t);
    Hi = H;
    tf = AbsPair(H);
    H += 2;
    {
      int res;
      if ((res = copy_complex_term(ap-1, ap+1, Hi, Hi)) < 0) {
	ARG1 = t;
	if (res == -1) { /* handle overflow */
	  if (!Yap_gc(2, ENV, P)) {
	    Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	    return(FALSE);
	  }
	  t = Deref(ARG1);
	  goto restart_list;
	} else { /* handle overflow */
	  if (!Yap_growheap(FALSE, 0, NULL)) {
	    Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
	    return(FALSE);
	  }
	  t = Deref(ARG1);
	  goto restart_list;
	}
      }
    }
    return(tf);
  } else {
    Functor f = FunctorOfTerm(t);
    Term tf;
    CELL *HB0;
    CELL *ap;

  restart_appl:
    f = FunctorOfTerm(t);
    HB0 = H;
    ap = RepAppl(t);
    tf = AbsAppl(H);
    H[0] = (CELL)f;
    H += 1+ArityOfFunctor(f);
    {
      int res;
      if ((res = copy_complex_term(ap, ap+ArityOfFunctor(f), HB0+1, HB0)) < 0) {
	ARG1 = t;
	if (res == -1) {
	  if (!Yap_gc(2, ENV, P)) {
	    Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	    return(FALSE);
	  }
	  t = Deref(ARG1);
	  goto restart_appl;
	} else { /* handle overflow */
	  if (!Yap_growheap(FALSE, 0, NULL)) {
	    Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
	    return(FALSE);
	  }
	  t = Deref(ARG1);
	  goto restart_appl;
	}
      }
    }
    return(tf);
  }
}
 
Term
Yap_CopyTerm(Term inp) {
  return CopyTerm(inp);
}

static Int 
p_copy_term(void)		/* copy term t to a new instance  */
{
  return(Yap_unify(ARG2,CopyTerm(ARG1)));
}

static int copy_complex_term_no_delays(register CELL *pt0, register CELL *pt0_end, CELL *ptf, CELL *HLow)
{

  CELL **to_visit0, **to_visit = (CELL **)Yap_PreAllocCodeSpace();
  tr_fr_ptr TR0 = TR;
  CELL *HB0 = HB;
  HB = HLow;

  to_visit0 = to_visit;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, copy_term_unk);
  copy_term_nvar:
    {
      if (IsPairTerm(d0)) {
	CELL *ap2 = RepPair(d0);
	if (ap2 >= HB && ap2 < H) {
	  /* If this is newer than the current term, just reuse */
	  *ptf++ = d0;
	  continue;
	} 
	*ptf = AbsPair(H);
	ptf++;
#ifdef RATIONAL_TREES
	if (to_visit + 4 >= (CELL **)Yap_GlobalBase) {
	  goto heap_overflow;
	}
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = ptf;
	to_visit[3] = (CELL *)*pt0;
	/* fool the system into thinking we had a variable there */
	*pt0 = AbsPair(H);
	to_visit += 4;
#else
	if (pt0 < pt0_end) {
	  if (to_visit + 3 >= (CELL **)Yap_GlobalBase) {
	    goto heap_overflow;
	  }
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = ptf;
	  to_visit += 3;
	}
#endif
	pt0 = ap2 - 1;
	pt0_end = ap2 + 1;
	ptf = H;
	H += 2;
	if (H > ENV - 2048) {
	  goto overflow;
	}
      } else if (IsApplTerm(d0)) {
	register Functor f;
	register CELL *ap2;
	/* store the terms to visit */
	ap2 = RepAppl(d0);
	if (ap2 >= HB && ap2 < H) {
	  /* If this is newer than the current term, just reuse */
	  *ptf++ = d0;
	  continue;
	} 
	f = (Functor)(*ap2);

	if (IsExtensionFunctor(f)) {
	  *ptf++ = d0;  /* you can just copy other extensions. */
	  continue;
	}
	*ptf = AbsAppl(H);
	ptf++;
	/* store the terms to visit */
#ifdef RATIONAL_TREES
	if (to_visit + 4 >= (CELL **)Yap_GlobalBase) {
	  goto heap_overflow;
	}
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = ptf;
	to_visit[3] = (CELL *)*pt0;
	/* fool the system into thinking we had a variable there */
	*pt0 = AbsAppl(H);
	to_visit += 4;
#else
	if (to_visit + 3 >= (CELL **)Yap_GlobalBase) {
	  goto heap_overflow;
	}
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = ptf;
	  to_visit += 3;
	}
#endif
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
	/* store the functor for the new term */
	H[0] = (CELL)f;
	ptf = H+1;
	H += 1+d0;
	if (H > ENV - 2048) {
	  goto overflow;
	}
      } else {
	/* just copy atoms or integers */
	*ptf++ = d0;
      }
      continue;
    }
    

    derefa_body(d0, ptd0, copy_term_unk, copy_term_nvar);
    if (ptd0 >= HLow && ptd0 < H) { 
      /* we have already found this cell */
      *ptf++ = (CELL) ptd0;
    } else {
      /* first time we met this term */
      RESET_VARIABLE(ptf);
      Bind_Global(ptd0, (CELL)ptf);
      ptf++;
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
#ifdef RATIONAL_TREES
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    ptf = to_visit[2];
    *pt0 = (CELL)to_visit[3];
#else
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    ptf = to_visit[2];
#endif
    goto loop;
  }

  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
  clean_tr(TR0);
  return(0); 

 overflow:
  /* oops, we're in trouble */
  H = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    ptf = to_visit[2];
    *pt0 = (CELL)to_visit[3];
  }
#endif
  clean_tr(TR0);
  return(-1);

 heap_overflow:
  /* oops, we're in trouble */
  H = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    ptf = to_visit[2];
    *pt0 = (CELL)to_visit[3];
  }
#endif
  clean_tr(TR0);
  return(-2);
}

static Term
CopyTermNoDelays(Term inp) {
  Term t = Deref(inp);
  int res;

  if (IsVarTerm(t)) {
    return(MkVarTerm());
  } else if (IsPrimitiveTerm(t)) {
    return(t);
  } else if (IsPairTerm(t)) {
    Term tf;
    CELL *ap, *Hi;

  restart_list:
    Hi = H;
    ap = RepPair(t);
    tf = AbsPair(H);
    H += 2;
    res = copy_complex_term_no_delays(ap-1, ap+1, H-2, H-2);
    if (res) {
      if (res == -1) { /* handle overflow */
	if (!Yap_gc(2, ENV, P)) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	  return(FALSE);
	}
	t = Deref(ARG1);
	goto restart_list;
      } else { /* handle overflow */
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
	  return(FALSE);
	}
	t = Deref(ARG1);
	goto restart_list;
      }
    }
    return(tf);
  } else {
    Functor f;
    Term tf;
    CELL *HB0;
    CELL *ap;

  restart_appl:
    f = FunctorOfTerm(t);
    HB0 = H;
    ap = RepAppl(t);
    tf = AbsAppl(H);
    H[0] = (CELL)f;
    H += 1+ArityOfFunctor(f);
    res = copy_complex_term_no_delays(ap, ap+ArityOfFunctor(f), HB0+1, HB0);
    if (res) {
      if (res == -1) {
	if (!Yap_gc(2, ENV, P)) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
	  return(FALSE);
	}
	t = Deref(ARG1);
	goto restart_appl;
      } else { /* handle overflow */
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  Yap_Error(SYSTEM_ERROR, TermNil, Yap_ErrorMessage);
	  return(FALSE);
	}
	t = Deref(ARG1);
	goto restart_appl;
      }
    }
    return(tf);
  }
}
 
static Int 
p_copy_term_no_delays(void)		/* copy term t to a new instance  */
{
  return(Yap_unify(ARG2,CopyTermNoDelays(ARG1)));
}


static Term vars_in_complex_term(register CELL *pt0, register CELL *pt0_end, Term inp)
{

  register CELL **to_visit0, **to_visit = (CELL **)Yap_PreAllocCodeSpace();
  register tr_fr_ptr TR0 = TR;
  CELL *InitialH = H;
  CELL output = AbsPair(H);

  to_visit0 = to_visit;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, vars_in_term_unk);
  vars_in_term_nvar:
    {
      if (IsPairTerm(d0)) {
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	to_visit += 3;
	*pt0 = TermNil;
#else
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit += 2;
	}
#endif
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
	/* store the terms to visit */
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	to_visit += 3;
	*pt0 = TermNil;
#else
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit += 2;
	}
#endif
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
      }
      continue;
    }
    

    derefa_body(d0, ptd0, vars_in_term_unk, vars_in_term_nvar);
    /* do or pt2 are unbound  */
    *ptd0 = TermNil;
    /* leave an empty slot to fill in later */
    H[1] = AbsPair(H+2);
    H += 2;
    H[-2] = (CELL)ptd0;
    /* next make sure noone will see this as a variable again */ 
    TrailTerm(TR++) = (CELL)ptd0;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
#ifdef RATIONAL_TREES
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    *pt0 = (CELL)to_visit[2];
#else
    to_visit -= 2;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
#endif
    goto loop;
  }

  clean_tr(TR0);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  if (H != InitialH) {
    /* close the list */
    Term t2 = Deref(inp);
    if (IsVarTerm(t2)) {
      RESET_VARIABLE(H-1);
      Yap_unify((CELL)(H-1),ARG2);
    } else {
      H[-1] = t2;		/* don't need to trail */
    }
    return(output);
  } else {
    return(inp);
  }
}
 
static Int 
p_variables_in_term(void)	/* variables in term t		 */
{
  Term t = Deref(ARG1);
  Term out;

  if (IsVarTerm(t)) {
    out = AbsPair(H);
    H += 2;
    RESET_VARIABLE(H-2);
    RESET_VARIABLE(H-1);
    Yap_unify((CELL)(H-2),ARG1);
    Yap_unify((CELL)(H-1),ARG2);
  }  else if (IsPrimitiveTerm(t)) 
    out = ARG2;
  else if (IsPairTerm(t)) {
    out = vars_in_complex_term(RepPair(t)-1,
			       RepPair(t)+1, ARG2);
  }
  else {
    Functor f = FunctorOfTerm(t);
    out = vars_in_complex_term(RepAppl(t),
			       RepAppl(t)+
			       ArityOfFunctor(f), ARG2);
  }
  return(Yap_unify(ARG3,out));
}

static Int 
p_term_variables(void)	/* variables in term t		 */
{
  Term t = Deref(ARG1);
  Term out;

  if (IsVarTerm(t)) {
    return Yap_unify(MkPairTerm(t,TermNil), ARG2);
  }  else if (IsPrimitiveTerm(t)) {
    return Yap_unify(TermNil, ARG2);
  } else if (IsPairTerm(t)) {
    out = vars_in_complex_term(RepPair(t)-1,
			       RepPair(t)+1, TermNil);
  }
  else {
    Functor f = FunctorOfTerm(t);
    out = vars_in_complex_term(RepAppl(t),
			       RepAppl(t)+
			       ArityOfFunctor(f), TermNil);
  }
  return Yap_unify(ARG2,out);
}

static Term non_singletons_in_complex_term(register CELL *pt0, register CELL *pt0_end)
{

  register CELL **to_visit0, **to_visit = (CELL **)Yap_PreAllocCodeSpace();
  register tr_fr_ptr TR0 = TR;
  CELL *InitialH = H;
  CELL output = AbsPair(H);

  to_visit0 = to_visit;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, vars_in_term_unk);
  vars_in_term_nvar:
    {
      if (IsPairTerm(d0)) {
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	to_visit += 3;
	*pt0 = TermNil;
#else
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit += 2;
	}
#endif
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
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	to_visit += 3;
	*pt0 = TermNil;
#else
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit += 2;
	}
#endif
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
      } else if (d0 == TermFoundVar) {
	CELL *pt2 = pt0;
	while(IsVarTerm(*pt2))
	  pt2 = (CELL *)(*pt2);
	H[1] = AbsPair(H+2);
	H += 2;
	H[-2] = (CELL)pt2;
	*pt2 = TermReFoundVar;
      }
      continue;
    }
    

    derefa_body(d0, ptd0, vars_in_term_unk, vars_in_term_nvar);
    /* do or pt2 are unbound  */
    *ptd0 = TermFoundVar;
    /* next make sure we can recover the variable again */ 
    TrailTerm(TR++) = (CELL)ptd0;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
#ifdef RATIONAL_TREES
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    *pt0 = (CELL)to_visit[2];
#else
    to_visit -= 2;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
#endif
    goto loop;
  }

  clean_tr(TR0);
  if (H != InitialH) {
    /* close the list */
    RESET_VARIABLE(H-1);
    Yap_unify((CELL)(H-1),ARG2);
    return(output);
  } else {
    return(ARG2);
  }
}
 
static Int 
p_non_singletons_in_term(void)	/* non_singletons in term t		 */
{
        Term t = Deref(ARG1);
	Term out;
	if (IsVarTerm(t)) {
	  out = MkPairTerm(t,ARG2);
	}  else if (IsPrimitiveTerm(t)) 
	  out = ARG2;
	else if (IsPairTerm(t)) {
	  out = non_singletons_in_complex_term(RepPair(t)-1,
				     RepPair(t)+1);
	}
	else out = non_singletons_in_complex_term(RepAppl(t),
					RepAppl(t)+
					ArityOfFunctor(FunctorOfTerm(t)));
	return(Yap_unify(ARG3,out));
}

static Int ground_complex_term(register CELL *pt0, register CELL *pt0_end)
{

  register CELL **to_visit0, **to_visit = (CELL **)Yap_PreAllocCodeSpace();

  to_visit0 = to_visit;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;

    ++pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, vars_in_term_unk);
  vars_in_term_nvar:
    {
      if (IsPairTerm(d0)) {
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	to_visit += 3;
	*pt0 = TermNil;
#else
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit += 2;
	}
#endif
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
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	to_visit += 3;
	*pt0 = TermNil;
#else
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit += 2;
	}
#endif
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
      }
      continue;
    }
    

    derefa_body(d0, ptd0, vars_in_term_unk, vars_in_term_nvar);
#ifdef RATIONAL_TREES
    while (to_visit > to_visit0) {
      to_visit -= 3;
      pt0 = to_visit[0];
      pt0_end = to_visit[1];
      *pt0 = (CELL)to_visit[2];
    }
#endif
    return(FALSE);
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
#ifdef RATIONAL_TREES
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    *pt0 = (CELL)to_visit[2];
#else
    to_visit -= 2;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
#endif
    goto loop;
  }
  return(TRUE);
}
 
static Int 
p_ground(void)			/* ground(+T)		 */
{
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    return(FALSE);
  }  else if (IsPrimitiveTerm(t)) {
    return(TRUE);
  } else if (IsPairTerm(t)) {
    return(ground_complex_term(RepPair(t)-1,
			       RepPair(t)+1));
  } else {
    Functor fun = FunctorOfTerm(t);

    if (IsExtensionFunctor(fun))
      return(TRUE);
    else return(ground_complex_term(RepAppl(t),
				    RepAppl(t)+
				    ArityOfFunctor(fun)));
  }
}

static Int var_in_complex_term(register CELL *pt0,
			       register CELL *pt0_end,
			       Term v)
{

  register CELL **to_visit0, **to_visit = (CELL **)Yap_PreAllocCodeSpace();
  register tr_fr_ptr TR0 = TR;

  to_visit0 = to_visit;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, var_in_term_unk);
  var_in_term_nvar:
    {
      if (IsPairTerm(d0)) {
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	to_visit += 3;
	*pt0 = TermNil;
#else
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit += 2;
	}
#endif
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
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	to_visit += 3;
	*pt0 = TermNil;
#else
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit += 2;
	}
#endif
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
      }
      continue;
    }
    

    deref_body(d0, ptd0, var_in_term_unk, var_in_term_nvar);
    if ((CELL)ptd0 == v) { /* we found it */
      clean_tr(TR0);
      return(TRUE);
    }
    /* do or pt2 are unbound  */
    *ptd0 = TermNil;
    /* next make sure noone will see this as a variable again */ 
    TrailTerm(TR++) = (CELL)ptd0;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
#ifdef RATIONAL_TREES
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    *pt0 = (CELL)to_visit[2];
#else
    to_visit -= 2;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
#endif
    goto loop;
  }
  clean_tr(TR0);
  return(FALSE);
}
 
static Int 
var_in_term(Term v, Term t)		/* variables in term t		 */
{

  if (IsVarTerm(t)) {
    return(v == t);
  } else if (IsPrimitiveTerm(t)) {
    return(FALSE);
  } else if (IsPairTerm(t)) {
    return(var_in_complex_term(RepPair(t)-1,
			       RepPair(t)+1, v));
  }
  else return(var_in_complex_term(RepAppl(t),
				  RepAppl(t)+
				  ArityOfFunctor(FunctorOfTerm(t)),v));
}

static Int
p_var_in_term(void)
{
  return(var_in_term(Deref(ARG2), Deref(ARG1)));
}

/* The code for TermHash was originally contributed by Gertjen Van Noor */

/* This code with max_depth == -1 will loop for infinite trees */

#define GvNht ((UInt *)H)

#define HASHADD(T) (GvNht[k]+=(T), k=(k<2 ? k+1 : 0))

static Int TermHash(Term t1, Int depth_lim, Int k)
{
  Int i;
  if (IsVarTerm(t1)) {
    return(-1);
  } else if (IsAtomTerm(t1)) {
    register char *s = AtomName(AtomOfTerm(t1));
    for (i=0; s[i]; i++)
	HASHADD(s[i]);
    return(k);
  } else if (IsPairTerm(t1)) {
    HASHADD('.');
    depth_lim--;
    if (depth_lim == 0) return(TRUE);
    k = TermHash(HeadOfTerm(t1),depth_lim,k);
    if (k < 0) return(-1);
    k = TermHash(TailOfTerm(t1),depth_lim,k);
    return(k);
  } else if (IsIntTerm(t1)) {
    HASHADD(IntOfTerm(t1));
    return(k);
  } else {
    Functor f = FunctorOfTerm(t1);

    if (IsExtensionFunctor(f)) {
      if (f == FunctorDouble) {
	Int *iptr = (Int *)(RepAppl(t1)+1);
	int i;

	for (i = 0; i < sizeof(Float) / sizeof(CELL); i++) {
	  HASHADD(*iptr++);
	}
	return(k);
      } else if (f == FunctorLongInt) {
	HASHADD(LongIntOfTerm(t1));
	return(k);
      } else if (f == FunctorDBRef) {
	HASHADD((Int)DBRefOfTerm(t1));
	return(k);
	/* should never happen */
      } else {
	return(-1);
      }
    } else {
      int ar = ArityOfFunctor(FunctorOfTerm(t1));
      int res = TRUE;
      register char *s = AtomName(NameOfFunctor(f));

      depth_lim--;
      if (depth_lim == 0) return(TRUE);
      for (i=0; s[i]; i++) 
	HASHADD(s[i]);
      for (i=1; i<=ar && res; i++) {
	k = TermHash(ArgOfTerm(i,t1),depth_lim,k);
	if (k == -1 ) return(-1);
      }
      return(k);
    }
  }
}

static Int
GvNTermHash(void)
{
  unsigned int i1,i2,i3;
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  Term t3 = Deref(ARG3);
  Term result;
  Int size, depth;


  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR,t2,"term_hash/4");
    return(FALSE);
  }
  if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER,t2,"term_hash/4");
    return(FALSE);
  }
  depth = IntegerOfTerm(t2);
  if (depth == 0) {
    if (IsVarTerm(t1)) return(TRUE);
    return(Yap_unify(ARG4,MkIntTerm(0)));
  }
  if (IsVarTerm(t3)) {
    Yap_Error(INSTANTIATION_ERROR,t3,"term_hash/4");
    return(FALSE);
  }
  if (!IsIntegerTerm(t3)) {
    Yap_Error(TYPE_ERROR_INTEGER,t3,"term_hash/4");
    return(FALSE);
  }
  size = IntegerOfTerm(t3);
  GvNht[0] = 0;
  GvNht[1] = 0;
  GvNht[2] = 0;

  if (TermHash(t1,depth,0) == -1) return(TRUE);

  i1 = GvNht[0];
  i2 = GvNht[1];
  i3 = GvNht[2];
  i2 ^= i3; i1 ^= i2; i1 = (((i3 << 7) + i2) << 7) + i1;
  result = MkIntegerTerm(i1 % size);
  return(Yap_unify(ARG4,result));
}

static int variant_complex(register CELL *pt0, register CELL *pt0_end, register
		   CELL *pt1)
{
  tr_fr_ptr OLDTR = TR;
  register CELL **to_visit = (CELL **)ASP;
  /* make sure that unification always forces trailing */
  HBREG = H;

 loop:
  while (pt0 < pt0_end) {
    register CELL d0, d1;
    ++ pt0;
    ++ pt1;
    d0 = Derefa(pt0);
    d1 = Derefa(pt1);
    if (IsVarTerm(d0)) {
      if (IsVarTerm(d1)) {
	/* bind the two variables to a new term */
	Term key = MkDBRefTerm((DBRef)H);
	*H++ = (CELL)FunctorDBRef;
	Bind_Global(VarOfTerm(d0), key);
	if (d0 != d1) {
	  Bind_Global(VarOfTerm(d1), key);
	}
	continue;
      } else {
	goto fail;
      }
    } else if (IsVarTerm(d1)) {
      goto fail;
    } else {
      if (d0 == d1) continue;
      else if (IsAtomOrIntTerm(d0)) {
	goto fail;
      } else if (IsPairTerm(d0)) {
	if (!IsPairTerm(d1)) {
	  goto fail;
	}
#ifdef RATIONAL_TREES
	/* now link the two structures so that no one else will */
	/* come here */
	to_visit -= 4;
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = (CELL *)*pt0;
	*pt0 = d1;
#else
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit -= 3;
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = pt1;
	}
#endif
	pt0 = RepPair(d0) - 1;
	pt0_end = RepPair(d0) + 1;
	pt1 = RepPair(d1) - 1;
	continue;
      } else if (IsApplTerm(d0)) {
	register Functor f;
	register CELL *ap2, *ap3;
	if (!IsApplTerm(d1)) {
	  goto fail;
	} else {
	  /* store the terms to visit */
	  Functor f2;
	  ap2 = RepAppl(d0);
	  ap3 = RepAppl(d1);
	  f = (Functor)(*ap2);
	  f2 = (Functor)(*ap3);
	  if (f != f2)
	    goto fail;
	  if (IsExtensionFunctor(f)) {
	    if (!unify_extension(f, d0, ap2, d1))
	      goto fail;
	    continue;
	  }
#ifdef RATIONAL_TREES
	/* now link the two structures so that no one else will */
	/* come here */
	to_visit -= 4;
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = (CELL *)*pt0;
	*pt0 = d1;
#else
	  /* store the terms to visit */
	  if (pt0 < pt0_end) {
	    to_visit -= 3;
	    to_visit[0] = pt0;
	    to_visit[1] = pt0_end;
	    to_visit[2] = pt1;
	  }
#endif
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
  if (to_visit < (CELL **)ASP) {
#ifdef RATIONAL_TREES
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
    to_visit += 4;
#else
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    to_visit += 3;
#endif
    goto loop;
  }

  H = HBREG;
  /* untrail all bindings made by variant */
  while (TR != (tr_fr_ptr)OLDTR) {
    CELL *pt1 = (CELL *) TrailTerm(--TR);
    RESET_VARIABLE(pt1);
  }
  HBREG = B->cp_h;
  return(TRUE);

 fail:
  /* failure */
  H = HBREG;
#ifdef RATIONAL_TREES
  while (to_visit < (CELL **)ASP) {
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
    to_visit += 4;
  }
#endif
  /* untrail all bindings made by variant */
  while (TR != (tr_fr_ptr)OLDTR) {
    CELL *pt1 = (CELL *) TrailTerm(--TR);
    RESET_VARIABLE(pt1);
  }
  HBREG = B->cp_h;
  return(FALSE);
}

static Int 
p_variant(void) /* variant terms t1 and t2	 */
{
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);

  if (t1 == t2)
    return (TRUE);
  if (IsVarTerm(t1)) {
    if (IsVarTerm(t2))
      return(TRUE);
    return(FALSE);
  } else if (IsVarTerm(t2))
    return(FALSE);
  if (IsAtomOrIntTerm(t1)) {
    return(t1 == t2);
  }
  if (IsPairTerm(t1)) {
    if (IsPairTerm(t2)) {
      return(variant_complex(RepPair(t1)-1,
			     RepPair(t1)+1,
			     RepPair(t2)-1));
    }
    else return (FALSE);
  }
  if (!IsApplTerm(t2)) return(FALSE);
  {
    Functor f1 = FunctorOfTerm(t1);
    if (f1 != FunctorOfTerm(t2)) return(FALSE);
    if (IsExtensionFunctor(f1)) {
      return(unify_extension(f1, t1, RepAppl(t1), t2));
    }
    return(variant_complex(RepAppl(t1),
			   RepAppl(t1)+ArityOfFunctor(f1),
			   RepAppl(t2)));
  }
}

static int subsumes_complex(register CELL *pt0, register CELL *pt0_end, register
		   CELL *pt1)
{
  register CELL **to_visit = (CELL **)ASP;
  CELL *OLDH = H;

 loop:
  while (pt0 < pt0_end) {
    register CELL d0, d1;
    ++ pt0;
    ++ pt1;
    d0 = Derefa(pt0);
    d1 = Derefa(pt1);
    if (IsVarTerm(d0)) {
      if (IsVarTerm(d1)) {
	/* bind the two variables to a new term */
	Term key = MkDBRefTerm((DBRef)H);
	Bind_Global(VarOfTerm(d0), d1);
	H[0] = (CELL)FunctorDBRef;
	H[1] = d1;
	H += 2;
	Bind_Global(VarOfTerm(d1), key);
	continue;
      } else {
	if (IsApplTerm(d1) && RepAppl(d1) >= OLDH && RepAppl(d1) < H) {
	  /* we are binding to a new variable; */
	  Bind_Global(VarOfTerm(d0),(CELL)pt1);
	} else {
	  Bind_Global(VarOfTerm(d0), d1);	
	}
      }
    } else if (IsVarTerm(d1)) {
      goto fail;
    } else {
      if (d0 == d1) continue;
      else if (IsAtomOrIntTerm(d0)) {
	goto fail;
      } else if (IsPairTerm(d0)) {
	if (!IsPairTerm(d1)) {
	  goto fail;
	}
#ifdef RATIONAL_TREES
	/* now link the two structures so that no one else will */
	/* come here */
	to_visit -= 4;
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = (CELL *)*pt0;
	*pt0 = d1;
#else
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit -= 3;
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = pt1;
	}
#endif
	pt0 = RepPair(d0) - 1;
	pt0_end = RepPair(d0) + 1;
	pt1 = RepPair(d1) - 1;
	continue;
      } else if (IsApplTerm(d0)) {
	register Functor f;
	register CELL *ap2, *ap3;
	if (!IsApplTerm(d1)) {
	  goto fail;
	} else {
	  /* store the terms to visit */
	  Functor f2;
	  ap2 = RepAppl(d0);
	  ap3 = RepAppl(d1);
	  f = (Functor)(*ap2);
	  f2 = (Functor)(*ap3);
	  if (f != f2)
	    goto fail;
	  if (IsExtensionFunctor(f)) {
	    if (!unify_extension(f, d0, ap2, d1))
	      goto fail;
	    continue;
	  }
#ifdef RATIONAL_TREES
	  /* now link the two structures so that no one else will */
	  /* come here */
	  to_visit -= 4;
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = pt1;
	  to_visit[3] = (CELL *)*pt0;
	  *pt0 = d1;
#else
	  /* store the terms to visit */
	  if (pt0 < pt0_end) {
	    to_visit -= 3;
	    to_visit[0] = pt0;
	    to_visit[1] = pt0_end;
	    to_visit[2] = pt1;
	  }
#endif
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
  if (to_visit < (CELL **)ASP) {
#ifdef RATIONAL_TREES
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
    to_visit += 4;
#else
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    to_visit += 3;
#endif
    goto loop;
  }

  while (H > OLDH) {
    H -= 2;
    RESET_VARIABLE(VarOfTerm(H[1]));
  }
  return(TRUE);

 fail:
#ifdef RATIONAL_TREES
  while (to_visit < (CELL **)ASP) {
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
    to_visit += 4;
  }
#endif
  return(FALSE);
}

static Int 
p_subsumes(void) /* subsumes terms t1 and t2	 */
{
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);

  if (t1 == t2)
    return (TRUE);
  if (IsVarTerm(t1)) {
    Bind(VarOfTerm(t1), t2);
    return(TRUE);
  } else if (IsVarTerm(t2))
    return(FALSE);
  if (IsAtomOrIntTerm(t1)) {
    return(t1 == t2);
  }
  if (IsPairTerm(t1)) {
    if (IsPairTerm(t2)) {
      return(subsumes_complex(RepPair(t1)-1,
			     RepPair(t1)+1,
			     RepPair(t2)-1));
    }
    else return (FALSE);
  } else {
    Functor f1;

    if (!IsApplTerm(t2)) return(FALSE);
    f1 = FunctorOfTerm(t1);
    if (f1 != FunctorOfTerm(t2))
      return(FALSE);
    if (IsExtensionFunctor(f1)) {
      return(unify_extension(f1, t1, RepAppl(t1), t2));
    }
    return(subsumes_complex(RepAppl(t1),
			   RepAppl(t1)+ArityOfFunctor(f1),
			   RepAppl(t2)));
  }
}

#ifdef DEBUG
static Int
p_force_trail_expansion()
{
  Int i = IntOfTerm(Deref(ARG1))*1024, j = 0;
  tr_fr_ptr OTR = TR;

  for (j = 0; j < i; j++) {
    TrailTerm(TR) = 0;
    TR++;
  }
  TR = OTR;

  return(TRUE);
}

static Int
camacho_dum(void)
{
  Term t1, t2;
  int  max = 3;

  /* build output list */

  t1 = MkAtomTerm(Yap_LookupAtom("[]"));
      t2 = MkPairTerm(MkIntegerTerm(max), t1);

  return(Yap_unify(t2, ARG1));
}



#endif /* DEBUG */

void Yap_InitUtilCPreds(void)
{
  Yap_InitCPred("copy_term", 2, p_copy_term, 0);
  Yap_InitCPred("$copy_term_but_not_constraints", 2, p_copy_term_no_delays, 0);
  Yap_InitCPred("ground", 1, p_ground, SafePredFlag);
  Yap_InitCPred("$variables_in_term", 3, p_variables_in_term, SafePredFlag);
  Yap_InitCPred("$non_singletons_in_term", 3, p_non_singletons_in_term, SafePredFlag);
  CurrentModule = TERMS_MODULE;
  Yap_InitCPred("term_variables", 2, p_term_variables, SafePredFlag);
  Yap_InitCPred("variable_in_term", 2, p_var_in_term, SafePredFlag);
  Yap_InitCPred("term_hash", 4, GvNTermHash, SafePredFlag);
  Yap_InitCPred("variant", 2, p_variant, SafePredFlag);
  Yap_InitCPred("subsumes", 2, p_subsumes, SafePredFlag);
  CurrentModule = PROLOG_MODULE;
#ifdef DEBUG
  Yap_InitCPred("$force_trail_expansion", 1, p_force_trail_expansion, SafePredFlag);
  Yap_InitCPred("dum", 1, camacho_dum, SafePredFlag);
#endif
}

