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
#include "clause.h"
#include "YapHeap.h"
#include "yapio.h"
#include "eval.h"
#include "attvar.h"
#ifdef HAVE_STRING_H
#include "string.h"
#endif

typedef struct {
	Term            old_var;
	Term            new_var;
}              *vcell;


STATIC_PROTO(int   copy_complex_term, (CELL *, CELL *, int, int, CELL *, CELL *));
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

static inline void
clean_dirty_tr(tr_fr_ptr TR0) {
  if (TR != TR0) {
    tr_fr_ptr pt = TR0;

    do {
      Term p = TrailTerm(pt++);
      RESET_VARIABLE(p);
    } while (pt != TR);
    TR = TR0;
  }
}

int show;

static int
copy_complex_term(CELL *pt0, CELL *pt0_end, int share, int newattvs, CELL *ptf, CELL *HLow)
{

  struct cp_frame *to_visit0, *to_visit = (struct cp_frame *)Yap_PreAllocCodeSpace();
  CELL *HB0 = HB;
  tr_fr_ptr TR0 = TR;
  int ground = TRUE;
#ifdef COROUTINING
  CELL *dvarsmin = NULL, *dvarsmax=NULL;
#endif

  HB = HLow;
  to_visit0 = to_visit;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    if (show) fprintf(stderr,"pt0=%p\n",pt0);
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
	if (to_visit+1 >= (struct cp_frame *)AuxSp) {
	  goto heap_overflow;
	}
	to_visit->start_cp = pt0;
	to_visit->end_cp = pt0_end;
	to_visit->to = ptf;
	to_visit->oldv = *pt0;
	to_visit->ground = ground;
	/* fool the system into thinking we had a variable there */
	*pt0 = AbsPair(H);
	to_visit ++;
#else
	if (pt0 < pt0_end) {
	  if (to_visit+1 >= (struct cp_frame *)AuxSp) {
	    goto heap_overflow;
	  }
	  to_visit->start_cp = pt0;
	  to_visit->end_cp = pt0_end;
	  to_visit->to = ptf;
	  to_visit->ground = ground;
	  to_visit ++;
	}
#endif
	ground = TRUE;
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
	if (show) fprintf(stderr,"d0=%lx %p %lx\n",d0,ap2,*ap2);
	if (ap2 >= HB && ap2 <= H) {
	  /* If this is newer than the current term, just reuse */
	  *ptf++ = d0;
	  continue;
	} 
	f = (Functor)(*ap2);

	if (IsExtensionFunctor(f)) {
#if defined(YAPOR) || defined(THREADS)
	  if (f == FunctorDBRef) {
	    DBRef  entryref = DBRefOfTerm(d0);
	    if (entryref->Flags & LogUpdMask) {
	      LogUpdClause *luclause = (LogUpdClause *)entryref;
	      LOCK(luclause->ClPred->PELock);
	      UNLOCK(luclause->ClPred->PELock);
	    } else {
	      LOCK(entryref->lock);
	      TRAIL_REF(entryref);	/* So that fail will erase it */
	      INC_DBREF_COUNT(entryref);
	      UNLOCK(entryref->lock);
	    }
	    *ptf++ = d0;  /* you can just copy other extensions. */
	  } else
#endif
	  if (!share) {
	    UInt sz;

	    *ptf++ = AbsAppl(H);  /* you can just copy other extensions. */
	    /* make sure to copy floats */
	    if (f== FunctorDouble) {
	      sz = sizeof(Float)/sizeof(CELL)+2;
	    } else if (f== FunctorLongInt) {
	      sz = 3;
	    } else {
	      CELL *pt = ap2+1;
	      sz = 2+sizeof(MP_INT)+(((MP_INT *)(pt+1))->_mp_alloc*sizeof(mp_limb_t));
	    }
	    if (H+sz > ASP - 2048) {
	      goto overflow;
	    }
	    memcpy((void *)H, (void *)ap2, sz*sizeof(CELL));
	    H += sz;
	  } else {
	    *ptf++ = d0;  /* you can just copy other extensions. */
	  }
	  continue;
	}
	*ptf = AbsAppl(H);
	ptf++;
	/* store the terms to visit */
#ifdef RATIONAL_TREES
	if (to_visit+1 >= (struct cp_frame *)AuxSp) {
	  goto heap_overflow;
	}
	to_visit->start_cp = pt0;
	to_visit->end_cp = pt0_end;
	to_visit->to = ptf;
	to_visit->oldv = *pt0;
	to_visit->ground = ground;
	/* fool the system into thinking we had a variable there */
	*pt0 = AbsAppl(H);
	to_visit ++;
#else
	if (pt0 < pt0_end) {
	  if (to_visit+1 >= (struct cp_frame *)AuxSp) {
	    goto heap_overflow;
	  }
	  to_visit->start_cp = pt0;
	  to_visit->end_cp = pt0_end;
	  to_visit->to = ptf;
	  to_visit->ground = ground;
	  to_visit ++;
	}
#endif
	ground = (f != FunctorMutable);
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
    ground = FALSE;
    if (ptd0 >= HLow && ptd0 < H) { 
      /* we have already found this cell */
      *ptf++ = (CELL) ptd0;
    } else {
#if COROUTINING
      if (newattvs && IsAttachedTerm((CELL)ptd0)) {
	/* if unbound, call the standard copy term routine */
	struct cp_frame *bp;

	if (IN_BETWEEN(dvarsmin, ptd0, dvarsmax)) {
	  *ptf++ = (CELL) ptd0;
	} else {
	  CELL new;

	  bp = to_visit;
	  if (!attas[ExtFromCell(ptd0)].copy_term_op(ptd0, &bp, ptf)) {
	    goto overflow;
	  }
	  to_visit = bp;
	  new = *ptf;
	  Bind(ptd0, new);
	  if (dvarsmin == NULL) {
	    dvarsmin = CellPtr(new);
	  } else {
	    *dvarsmax = (CELL)(CellPtr(new)+1);
	  }
	  dvarsmax = CellPtr(new)+1;
	  ptf++;
	}
      } else {
#endif
	/* first time we met this term */
	RESET_VARIABLE(ptf);
	if (TR > (tr_fr_ptr)Yap_TrailTop - 256) {
	  /* Trail overflow */
	  if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	    goto trail_overflow;
	  }
	}
	Bind(ptd0, (CELL)ptf);
	ptf++;
#ifdef COROUTINING
      }
#endif
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit --;
    if (ground && share) {
      CELL old = to_visit->oldv;
      CELL *newp = to_visit->to-1;
      CELL new = *newp;

      *newp = old;
      if (IsApplTerm(new))
	H = RepAppl(new);
      else
	H = RepPair(new);
    }
    pt0 = to_visit->start_cp;
    pt0_end = to_visit->end_cp;
    ptf = to_visit->to;
#ifdef RATIONAL_TREES
    *pt0 = to_visit->oldv;
#endif
    ground = (ground && to_visit->ground);
    goto loop;
  }

  /* restore our nice, friendly, term to its original state */
  clean_dirty_tr(TR0);
  close_attvar_chain(dvarsmin, dvarsmax);
  HB = HB0;
  return ground;

 overflow:
  /* oops, we're in trouble */
  H = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit --;
    pt0 = to_visit->start_cp;
    pt0_end = to_visit->end_cp;
    ptf = to_visit->to;
    *pt0 = to_visit->oldv;
  }
#endif
  reset_trail(TR0);
  /* follow chain of multi-assigned variables */
  reset_attvars(dvarsmin, dvarsmax);
  return -1;

trail_overflow:
  /* oops, we're in trouble */
  H = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit --;
    pt0 = to_visit->start_cp;
    pt0_end = to_visit->end_cp;
    ptf = to_visit->to;
    *pt0 = to_visit->oldv;
  }
#endif
  {
    tr_fr_ptr oTR =  TR;
    reset_trail(TR0);
    reset_attvars(dvarsmin, dvarsmax);
    if (!Yap_growtrail((oTR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
      return -4;
    }
    return -2;
  }

 heap_overflow:
  /* oops, we're in trouble */
  H = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit --;
    pt0 = to_visit->start_cp;
    pt0_end = to_visit->end_cp;
    ptf = to_visit->to;
    *pt0 = to_visit->oldv;
  }
#endif
  reset_trail(TR0);
  reset_attvars(dvarsmin, dvarsmax);
  Yap_Error_Size = (ADDR)AuxSp-(ADDR)to_visit0;
  return -3;
}


static Term
handle_cp_overflow(int res, tr_fr_ptr TR0, UInt arity, Term t)
{
  XREGS[arity+1] = t;
  switch(res) {
  case -1:
    if (!Yap_gcl((ASP-H)*sizeof(CELL), arity+1, ENV, gc_P(P,CP))) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, Yap_ErrorMessage);
      return 0L;
    }
    return Deref(XREGS[arity+1]);
  case -2:
    return Deref(XREGS[arity+1]);
  case -3:
    {
      UInt size = Yap_Error_Size;
      Yap_Error_Size = 0L;
      if (size > 4*1024*1024)
	size = 4*1024*1024;
      if (!Yap_ExpandPreAllocCodeSpace(size, NULL, TRUE)) {
	Yap_Error(OUT_OF_AUXSPACE_ERROR, TermNil, Yap_ErrorMessage);
	return 0L;
      }
    }
    return Deref(XREGS[arity+1]);
  case -4:
    if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), FALSE)) {
      Yap_Error(OUT_OF_TRAIL_ERROR, TermNil, Yap_ErrorMessage);
      return 0L;
    }
    return Deref(XREGS[arity+1]);
  default:
    return 0L;
  }
}

static Term
CopyTerm(Term inp, UInt arity, int share, int newattvs) {
  Term t = Deref(inp);
  tr_fr_ptr TR0 = TR;

  if (IsVarTerm(t)) {
#if COROUTINING
    if (newattvs && IsAttachedTerm(t)) {
      CELL *Hi;
      int res;
    restart_attached:

      *H = t;
      Hi = H+1;
      H += 2;
      if ((res = copy_complex_term(Hi-2, Hi-1, share, newattvs, Hi, Hi)) < 0) {
	H = Hi-1;
	if ((t = handle_cp_overflow(res, TR0, arity, t))== 0L)
	  return FALSE;
	goto restart_attached;
      }
      return Hi[0];
    }
#endif
    return MkVarTerm();
  } else if (IsPrimitiveTerm(t)) {
    return t;
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
      if ((res = copy_complex_term(ap-1, ap+1, share, newattvs, Hi, Hi)) < 0) {
	H = Hi;
	if ((t = handle_cp_overflow(res, TR0, arity, t))== 0L)
	  return FALSE;
	goto restart_list;
      } else if (res && share) {
	H = Hi;
	return t;
      }
    }
    return tf;
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
    if (H > ASP-128) {
      H = HB0;
      if ((t = handle_cp_overflow(-1, TR0, arity, t))== 0L)
	return FALSE;
      goto restart_appl;
    } else {
      int res;

      if ((res = copy_complex_term(ap, ap+ArityOfFunctor(f), share, newattvs, HB0+1, HB0)) < 0) {
	H = HB0;
	if ((t = handle_cp_overflow(res, TR0, arity, t))== 0L)
	  return FALSE;
	goto restart_appl;
      } else if (res && share && FunctorOfTerm(t) != FunctorMutable) {
	H = HB0;
	return t;
      }
    }
    return tf;
  }
}
 
Term
Yap_CopyTerm(Term inp) {
  return CopyTerm(inp, 0, TRUE, TRUE);
}

Term
Yap_CopyTermNoShare(Term inp) {
  return CopyTerm(inp, 0, FALSE, FALSE);
}

static Int 
p_copy_term(void)		/* copy term t to a new instance  */
{
  Term t = CopyTerm(ARG1, 2, TRUE, TRUE); 
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  return Yap_unify(ARG2,t);
}

static Int 
p_duplicate_term(void)		/* copy term t to a new instance  */
{
  Term t = CopyTerm(ARG1, 2, FALSE, TRUE); 
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  return Yap_unify(ARG2,t);
}

static Int 
p_copy_term_no_delays(void)		/* copy term t to a new instance  */
{
  Term t = CopyTerm(ARG1, 2, TRUE, FALSE);
  if (t == 0L) {
    return FALSE;
  }
  /* be careful, there may be a stack shift here */
  return(Yap_unify(ARG2,t));
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
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
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
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
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
    if (H+1024 > ASP) {
      goto global_overflow;
    }
    H[1] = AbsPair(H+2);
    H += 2;
    H[-2] = (CELL)ptd0;
    /* next make sure noone will see this as a variable again */ 
    if (TR > (tr_fr_ptr)Yap_TrailTop - 256) {
      /* Trail overflow */
      if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	goto trail_overflow;
      }
    }
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

 trail_overflow:
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  Yap_Error_TYPE = OUT_OF_TRAIL_ERROR;
  Yap_Error_Size = (TR-TR0)*sizeof(tr_fr_ptr *);
  clean_tr(TR0);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  return 0L;
  
 aux_overflow:
  Yap_Error_Size = (to_visit-to_visit0)*sizeof(CELL **);
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  Yap_Error_TYPE = OUT_OF_AUXSPACE_ERROR;
  clean_tr(TR0);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  return 0L;
  
 global_overflow:
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  clean_tr(TR0);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  Yap_Error_TYPE = OUT_OF_STACK_ERROR;
  Yap_Error_Size = (ASP-H)*sizeof(CELL);
  return 0L;
  
}

static int
expand_vts(void)
{
  UInt expand = Yap_Error_Size;
  yap_error_number yap_errno = Yap_Error_TYPE;
  
  Yap_Error_Size = 0;
  Yap_Error_TYPE = YAP_NO_ERROR;
  if (yap_errno == OUT_OF_TRAIL_ERROR) {
    /* Trail overflow */
    if (!Yap_growtrail(expand, FALSE)) {
      return FALSE;
    }
  } else if (yap_errno == OUT_OF_AUXSPACE_ERROR) {
    /* Aux space overflow */
    if (expand > 4*1024*1024)
      expand = 4*1024*1024;
    if (!Yap_ExpandPreAllocCodeSpace(expand, NULL, TRUE)) {
      return FALSE;
    }
  } else {
    if (!Yap_gcl(expand, 3, ENV, gc_P(P,CP))) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, "in term_variables");
      return FALSE;
    }
  }
  return TRUE;
}
 
static Int 
p_variables_in_term(void)	/* variables in term t		 */
{
  Term out, inp;
  int count;
  

 restart:
  count = 0;
  inp = Deref(ARG2);
  while (!IsVarTerm(inp) && IsPairTerm(inp)) {
    Term t = HeadOfTerm(inp);
    if (IsVarTerm(t)) {
      CELL *ptr = VarOfTerm(t);
      *ptr = TermFoundVar;
      TrailTerm(TR++) = t;
      count++;
      if (TR > (tr_fr_ptr)Yap_TrailTop - 256) {
	clean_tr(TR-count);
	if (!Yap_growtrail(count*sizeof(tr_fr_ptr *), FALSE)) {
	  return FALSE;
	}
	goto restart;
      }
    }
    inp = TailOfTerm(inp);
  }
  do {
    Term t = Deref(ARG1);
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
    if (out == 0L) {
      if (!expand_vts())
	return FALSE;
    }
  } while (out == 0L);
  clean_tr(TR-count);
  return Yap_unify(ARG3,out);
}


static Int 
p_term_variables(void)	/* variables in term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG1);
    if (IsVarTerm(t)) {
      Term out = Yap_MkNewPairTerm();
      return
	Yap_unify(t,HeadOfTerm(out)) &&
	Yap_unify(TermNil, TailOfTerm(out)) &&
	Yap_unify(out, ARG2);
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
    if (out == 0L) {
      if (!expand_vts())
	return FALSE;
    }
  } while (out == 0L);
  return Yap_unify(ARG2,out);
}

static Term attvars_in_complex_term(register CELL *pt0, register CELL *pt0_end, Term inp)
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
    deref_head(d0, attvars_in_term_unk);
  attvars_in_term_nvar:
    {
      if (IsPairTerm(d0)) {
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
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
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
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
    

    derefa_body(d0, ptd0, attvars_in_term_unk, attvars_in_term_nvar);
    if (IsAttVar(ptd0)) {
      /* do or pt2 are unbound  */
      *ptd0 = TermNil;
      /* next make sure noone will see this as a variable again */ 
      if (TR > (tr_fr_ptr)Yap_TrailTop - 256) {
	/* Trail overflow */
	if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	  goto trail_overflow;
	}
      }
      TrailTerm(TR++) = (CELL)ptd0;
      /* leave an empty slot to fill in later */
      if (H+1024 > ASP) {
	goto global_overflow;
      }
      H[1] = AbsPair(H+2);
      H += 2;
      H[-2] = (CELL)ptd0;
      /* store the terms to visit */
      if (to_visit + 1024 >= (CELL **)AuxSp) {
	goto aux_overflow;
      }
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
      pt0 = &RepAttVar(ptd0)->Value;
      pt0_end = &RepAttVar(ptd0)->Atts;
    }
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

 trail_overflow:
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  Yap_Error_TYPE = OUT_OF_TRAIL_ERROR;
  Yap_Error_Size = (TR-TR0)*sizeof(tr_fr_ptr *);
  clean_tr(TR0);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  return 0L;
  
 aux_overflow:
  Yap_Error_Size = (to_visit-to_visit0)*sizeof(CELL **);
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  Yap_Error_TYPE = OUT_OF_AUXSPACE_ERROR;
  clean_tr(TR0);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  return 0L;
  
 global_overflow:
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  clean_tr(TR0);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  Yap_Error_TYPE = OUT_OF_STACK_ERROR;
  Yap_Error_Size = (ASP-H)*sizeof(CELL);
  return 0L;
  
}

static Int 
p_term_attvars(void)	/* variables in term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG1);
    if (IsVarTerm(t)) {
      out = attvars_in_complex_term(VarOfTerm(t)-1,
				    VarOfTerm(t)+1, TermNil);
    }  else if (IsPrimitiveTerm(t)) {
      return Yap_unify(TermNil, ARG2);
    } else if (IsPairTerm(t)) {
      out = attvars_in_complex_term(RepPair(t)-1,
				 RepPair(t)+1, TermNil);
    }
    else {
      Functor f = FunctorOfTerm(t);
      out = attvars_in_complex_term(RepAppl(t),
				 RepAppl(t)+
				 ArityOfFunctor(f), TermNil);
    }
    if (out == 0L) {
      if (!expand_vts())
	return FALSE;
    }
  } while (out == 0L);
  return Yap_unify(ARG2,out);
}

static Int 
p_term_variables3(void)	/* variables in term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG1);
    if (IsVarTerm(t)) {
      Term out = Yap_MkNewPairTerm();
      return
	Yap_unify(t,HeadOfTerm(out)) &&
	Yap_unify(ARG3, TailOfTerm(out)) &&
	Yap_unify(out, ARG2);
    }  else if (IsPrimitiveTerm(t)) {
      return Yap_unify(ARG2, ARG3);
    } else if (IsPairTerm(t)) {
      out = vars_in_complex_term(RepPair(t)-1,
				 RepPair(t)+1, ARG3);
    }
    else {
      Functor f = FunctorOfTerm(t);
      out = vars_in_complex_term(RepAppl(t),
				 RepAppl(t)+
				 ArityOfFunctor(f), ARG3);
    }
    if (out == 0L) {
      if (!expand_vts())
	return FALSE;
    }
  } while (out == 0L);

  return Yap_unify(ARG2,out);
}


static Term vars_within_complex_term(register CELL *pt0, register CELL *pt0_end, Term inp)
{

  register CELL **to_visit0, **to_visit = (CELL **)Yap_PreAllocCodeSpace();
  register tr_fr_ptr TR0 = TR;
  CELL *InitialH = H;
  CELL output = AbsPair(H);

  to_visit0 = to_visit;
  while (!IsVarTerm(inp) && IsPairTerm(inp)) {
    Term t = HeadOfTerm(inp);
    if (IsVarTerm(t)) {
      CELL *ptr = VarOfTerm(t);
      *ptr = TermFoundVar;
      TrailTerm(TR++) = t;
      if (TR > (tr_fr_ptr)Yap_TrailTop - 256) {
	if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	  goto trail_overflow;
	}
      }
    }
    inp = TailOfTerm(inp);
  }
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, vars_within_term_unk);
  vars_within_term_nvar:
    {
      if (IsPairTerm(d0)) {
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
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
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
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
      } else if (d0 == TermFoundVar) {
	/* leave an empty slot to fill in later */
	if (H+1024 > ASP) {
	  goto global_overflow;
	}
	H[1] = AbsPair(H+2);
	H += 2;
	H[-2] = (CELL)ptd0;
	*ptd0 = TermNil;
      }
      continue;
    }

    derefa_body(d0, ptd0, vars_within_term_unk, vars_within_term_nvar);
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
    H[-1] = TermNil;
    return output;
  } else {
    return TermNil;
  }

 trail_overflow:
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  Yap_Error_TYPE = OUT_OF_TRAIL_ERROR;
  Yap_Error_Size = (TR-TR0)*sizeof(tr_fr_ptr *);
  clean_tr(TR0);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  return 0L;
  
 aux_overflow:
  Yap_Error_Size = (to_visit-to_visit0)*sizeof(CELL **);
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  Yap_Error_TYPE = OUT_OF_AUXSPACE_ERROR;
  clean_tr(TR0);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  return 0L;
  
 global_overflow:
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  clean_tr(TR0);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  Yap_Error_TYPE = OUT_OF_STACK_ERROR;
  Yap_Error_Size = (ASP-H)*sizeof(CELL);
  return 0L;
  
}

static Int 
p_variables_within_term(void)	/* variables within term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG2);
    if (IsVarTerm(t)) {
      out = vars_within_complex_term(VarOfTerm(t)-1,
				     VarOfTerm(t), Deref(ARG1));

    }  else if (IsPrimitiveTerm(t)) 
      out = TermNil;
    else if (IsPairTerm(t)) {
      out = vars_within_complex_term(RepPair(t)-1,
				     RepPair(t)+1, Deref(ARG1));
    }
    else {
      Functor f = FunctorOfTerm(t);
      out = vars_within_complex_term(RepAppl(t),
				 RepAppl(t)+
				     ArityOfFunctor(f), Deref(ARG1));
    }
    if (out == 0L) {
      if (!expand_vts())
	return FALSE;
    }
  } while (out == 0L);
  return Yap_unify(ARG3,out);
}

static Term new_vars_in_complex_term(register CELL *pt0, register CELL *pt0_end, Term inp)
{
  register CELL **to_visit0, **to_visit = (CELL **)Yap_PreAllocCodeSpace();
  register tr_fr_ptr TR0 = TR;
  CELL *InitialH = H;
  CELL output = AbsPair(H);

  to_visit0 = to_visit;
  while (!IsVarTerm(inp) && IsPairTerm(inp)) {
    Term t = HeadOfTerm(inp);
    if (IsVarTerm(t)) {
      CELL *ptr = VarOfTerm(t);
      *ptr = TermFoundVar;
      TrailTerm(TR++) = t;
      if (TR > (tr_fr_ptr)Yap_TrailTop - 256) {
	if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	  goto trail_overflow;
	}
      }
    }
    inp = TailOfTerm(inp);
  }
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, vars_within_term_unk);
  vars_within_term_nvar:
    {
      if (IsPairTerm(d0)) {
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
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
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
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

    derefa_body(d0, ptd0, vars_within_term_unk, vars_within_term_nvar);
    /* do or pt2 are unbound  */
    *ptd0 = TermNil;
    /* leave an empty slot to fill in later */
    if (H+1024 > ASP) {
      goto global_overflow;
    }
    H[1] = AbsPair(H+2);
    H += 2;
    H[-2] = (CELL)ptd0;
    /* next make sure noone will see this as a variable again */ 
    if (TR > (tr_fr_ptr)Yap_TrailTop - 256) {
      /* Trail overflow */
      if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	goto trail_overflow;
      }
    }
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
    H[-1] = TermNil;
    return output;
  } else {
    return TermNil;
  }

 trail_overflow:
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  Yap_Error_TYPE = OUT_OF_TRAIL_ERROR;
  Yap_Error_Size = (TR-TR0)*sizeof(tr_fr_ptr *);
  clean_tr(TR0);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  return 0L;
  
 aux_overflow:
  Yap_Error_Size = (to_visit-to_visit0)*sizeof(CELL **);
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  Yap_Error_TYPE = OUT_OF_AUXSPACE_ERROR;
  clean_tr(TR0);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  return 0L;
  
 global_overflow:
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  clean_tr(TR0);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  Yap_Error_TYPE = OUT_OF_STACK_ERROR;
  Yap_Error_Size = (ASP-H)*sizeof(CELL);
  return 0L;
  
}

static Int 
p_new_variables_in_term(void)	/* variables within term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG2);
    if (IsVarTerm(t)) {
      out = new_vars_in_complex_term(VarOfTerm(t)-1,
				     VarOfTerm(t), Deref(ARG1));

    }  else if (IsPrimitiveTerm(t)) 
      out = TermNil;
    else if (IsPairTerm(t)) {
      out = new_vars_in_complex_term(RepPair(t)-1,
				     RepPair(t)+1, Deref(ARG1));
    }
    else {
      Functor f = FunctorOfTerm(t);
      out = new_vars_in_complex_term(RepAppl(t),
			     RepAppl(t)+
			     ArityOfFunctor(f), Deref(ARG1));
    }
    if (out == 0L) {
      if (!expand_vts())
	return FALSE;
    }
  } while (out == 0L);
  return Yap_unify(ARG3,out);
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
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
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
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
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
    return output;
  } else {
    return ARG2;
  }

 aux_overflow:
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  clean_tr(TR0);
  if (H != InitialH) {
    /* close the list */
    RESET_VARIABLE(H-1);
  }
  return 0L;
}
 
static Int 
p_non_singletons_in_term(void)	/* non_singletons in term t		 */
{
  Term t;
  Term out;
	
  while (TRUE) {
    t = Deref(ARG1);
    if (IsVarTerm(t)) {
      out = MkPairTerm(t,ARG2);
    }  else if (IsPrimitiveTerm(t)) {
      out = ARG2;
    } else if (IsPairTerm(t)) {
      out = non_singletons_in_complex_term(RepPair(t)-1,
					   RepPair(t)+1);
    } else {
      out = non_singletons_in_complex_term(RepAppl(t),
					   RepAppl(t)+
					   ArityOfFunctor(FunctorOfTerm(t)));
    }
    if (out != 0L) {
      return Yap_unify(ARG3,out);
    } else {
      if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
	Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "overflow in singletons");
	return FALSE;
      }
    }
  }  
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
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
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
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
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
    return FALSE;
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
  return TRUE;

 aux_overflow:
  /* unwind stack */
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  return -1;
}
 
int Yap_IsGroundTerm(Term t)
{
  while (TRUE) {
    Int out;

    if (IsVarTerm(t)) {
      return FALSE;
    }  else if (IsPrimitiveTerm(t)) {
      return TRUE;
    } else if (IsPairTerm(t)) {
      if ((out =ground_complex_term(RepPair(t)-1,
				    RepPair(t)+1)) >= 0) {
	return out;
      }
    } else {
      Functor fun = FunctorOfTerm(t);
      
      if (IsExtensionFunctor(fun))
	return TRUE;
      else if ((out = ground_complex_term(RepAppl(t),
					     RepAppl(t)+
					     ArityOfFunctor(fun))) >= 0) {
	     return out;
      }
    }
    if (out < 0) {
      if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
	Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "overflow in ground");
	return FALSE;
      }      
    }
  }
}

static Int 
p_ground(void)			/* ground(+T)		 */
{
  return Yap_IsGroundTerm(Deref(ARG1));
}

static int
SizeOfExtension(Term t)
{
  Functor f = FunctorOfTerm(t);
  if (f== FunctorDouble) {
    return 2 + sizeof(Float)/sizeof(CELL);
  }
  if (f== FunctorLongInt) {
    return 2 + sizeof(Float)/sizeof(CELL);
  }
  if (f== FunctorDBRef) {
    return 0;
  }
  if (f== FunctorBigInt) {
    CELL *pt = RepAppl(t)+2;
    return 3+sizeof(MP_INT)+(((MP_INT *)(pt))->_mp_alloc*sizeof(mp_limb_t));
  }
  return 0;
}


static Int sz_ground_complex_term(register CELL *pt0, register CELL *pt0_end, int ground)
{

  register CELL **to_visit0, **to_visit = (CELL **)Yap_PreAllocCodeSpace();
  Int sz = 0;

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
	sz += 2;
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
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
	  sz += SizeOfExtension(d0);
	  continue;
	}
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
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
	sz += (1+d0);
	pt0 = ap2;
	pt0_end = ap2 + d0;
      }
      continue;
    }
    

    derefa_body(d0, ptd0, vars_in_term_unk, vars_in_term_nvar);
    if (!ground)
      continue;
#ifdef RATIONAL_TREES
    while (to_visit > to_visit0) {
      to_visit -= 3;
      pt0 = to_visit[0];
      pt0_end = to_visit[1];
      *pt0 = (CELL)to_visit[2];
    }
#endif
    return 0;
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
  return sz;

 aux_overflow:
  /* unwind stack */
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  return -1;
}

int
Yap_SizeGroundTerm(Term t, int ground)
{
  if (IsVarTerm(t)) {
    if (!ground)
      return 1;
    return 0;
  }  else if (IsPrimitiveTerm(t)) {
    return 1;
  } else if (IsPairTerm(t)) {
    int sz = sz_ground_complex_term(RepPair(t)-1, RepPair(t)+1, ground);
    if (sz <= 0)
      return sz;
    return sz+2;
} else {
  int sz = 0;
  Functor fun = FunctorOfTerm(t);
      
  if (IsExtensionFunctor(fun))
    return 1+ SizeOfExtension(t);
  
   sz = sz_ground_complex_term(RepAppl(t),
			    RepAppl(t)+
			    ArityOfFunctor(fun),
			    ground);
   if (sz <= 0)
     return sz;
   return 1+ArityOfFunctor(fun)+sz;
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
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
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
	continue;
      } else if (IsApplTerm(d0)) {
	register Functor f;
	register CELL *ap2;
	/* store the terms to visit */
	ap2 = RepAppl(d0);
	f = (Functor)(*ap2);

	if (IsExtensionFunctor(f)) {

	  continue;
	}
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
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
#ifdef RATIONAL_TREES
      while (to_visit > to_visit0) {
	to_visit -= 3;
	pt0 = to_visit[0];
	*pt0 = (CELL)to_visit[2];
      }
#endif
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
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  clean_tr(TR0);
  return FALSE;


 aux_overflow:
  /* unwind stack */
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  return -1;
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


//-----------------------------------------------------------------------------
// MurmurHash2, by Austin Appleby

// Note - This code makes a few assumptions about how your machine behaves -

// 1. We can read a 4-byte value from any address without crashing
// 2. sizeof(int) == 4

// And it has a few limitations -

// 1. It will not work incrementally.
// 2. It will not produce the same results on little-endian and big-endian
//    machines.

static unsigned int
MurmurHashNeutral2 ( const void * key, int len, unsigned int seed )
{
	const unsigned int m = 0x5bd1e995;
	const int r = 24;

	unsigned int h = seed ^ len;

	const unsigned char * data = (const unsigned char *)key;

	while(len >= 4)
	{
		unsigned int k;

		k  = data[0];
		k |= data[1] << 8;
		k |= data[2] << 16;
		k |= data[3] << 24;

		k *= m; 
		k ^= k >> r; 
		k *= m;

		h *= m;
		h ^= k;

		data += 4;
		len -= 4;
	}
	
	switch(len)
	{
	case 3: h ^= data[2] << 16;
	case 2: h ^= data[1] << 8;
	case 1: h ^= data[0];
	        h *= m;
	};

	h ^= h >> 13;
	h *= m;
	h ^= h >> 15;

	return h;
} 

static CELL *
AddAtomToHash(CELL *st, Atom at)
{
  unsigned int len;
  CELL * start;

  if (IsWideAtom(at)) {
    wchar_t *c = RepAtom(at)->WStrOfAE;
    int ulen = wcslen(c);
    len = ulen*sizeof(wchar_t);
    if (len % CellSize == 0) {
      len /= CellSize;
    } else {
      len /= CellSize;
      len++;
    }
    st[len-1] = 0L;
    wcsncpy((wchar_t *)st, c, ulen);
  } else {
    char *c = RepAtom(at)->StrOfAE;
    int ulen = strlen(c);
    /* fix hashing over empty atom */
    if (!ulen) {
      return st;
    }
    start = (CELL *)c;
    if (ulen % CellSize == 0) {
      len = ulen/CellSize;
    } else {
      len = ulen/CellSize;
      len++;
    }
    st[len-1] = 0L;
    strncpy((char *)st, c, ulen);
  }
  return st+len;
}

static CELL *
hash_complex_term(register CELL *pt0,
		  register CELL *pt0_end,
		  Int depth,
		  CELL *st,
		  int variant)
{

  register CELL **to_visit0, **to_visit = (CELL **)Yap_PreAllocCodeSpace();

  to_visit0 = to_visit;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, hash_complex_unk);
  hash_complex_nvar:
    {
      if (st + 1024 >= ASP) {
	goto global_overflow;
      }
      if (IsPrimitiveTerm(d0)) {
	if (d0 != TermFoundVar) {
	  if (IsAtomTerm(d0)) {
	    st = AddAtomToHash(st, AtomOfTerm(d0));
	  } else {
	    *st++ = IntOfTerm(d0);
	  }
	}
	continue;
      } else if (IsPairTerm(d0)) {
	st = AddAtomToHash(st, AtomDot);
	if (depth == 1)
	  continue;
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	to_visit[3] = (CELL *)(depth--);
	to_visit += 4;
	*pt0 = TermFoundVar;
#else
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = (CELL *)(depth--);
	  to_visit += 3;
	}
#endif
	pt0 = RepPair(d0) - 1;
	pt0_end = RepPair(d0) + 1;
	continue;
      } else if (IsApplTerm(d0)) {
	register Functor f;
	register CELL *ap2;
	/* store the terms to visit */
	ap2 = RepAppl(d0);
	f = (Functor)(*ap2);

	if (IsExtensionFunctor(f)) {
	  CELL fc = (CELL)NameOfFunctor(f);

	  switch(fc) {
	    
	  case (CELL)FunctorDBRef:
	    *st++ = fc;
	    break;
	  case (CELL)FunctorLongInt:
	    *st++ = LongIntOfTerm(d0);
	    break;
#ifdef USE_GMP
	  case (CELL)FunctorBigInt:
	    {
	      CELL *pt = RepAppl(d0);
	      Int sz = 
		sizeof(MP_INT)+1+
		(((MP_INT *)(pt+2))->_mp_alloc*sizeof(mp_limb_t));

	      if (st + (1024 + sz/CellSize) >= ASP) {
		goto global_overflow;
	      }
	      /* then the actual number */
	      memcpy((void *)(st+1), (void *)(pt+1), sz);
	      st = st+sz/CellSize;
	    }
	    break;
#endif
	  case (CELL)FunctorDouble:
	    {
	      CELL *pt = RepAppl(d0);
	      *st++ = pt[1];
#if  SIZEOF_DOUBLE == 2*SIZEOF_LONG_INT
	      *st++ = pt[2];
#endif
	      break;
	    }
	  }
	  continue;
	}
	st = AddAtomToHash(st, NameOfFunctor(f));
	if (depth == 1)
	  continue;
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	to_visit[3] = (CELL *)(depth--);
	to_visit += 4;
	*pt0 = TermFoundVar;
#else
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = depth--;
	  to_visit += 3;
	}
#endif
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
      }
      continue;
    }
    

    deref_body(d0, ptd0, hash_complex_unk, hash_complex_nvar);
    if (!variant)
      return NULL;
    else
      continue;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
#ifdef RATIONAL_TREES
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    *pt0 = (CELL)to_visit[2];
    depth = (CELL)to_visit[3];
#else
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    depth = (CELL)to_visit[2];
#endif
    goto loop;
  }
  return st;

 aux_overflow:
  /* unwind stack */
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 4;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  return (CELL *)-1;

 global_overflow:
  /* unwind stack */
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 4;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  return (CELL *) -2;
}
 
static Int
p_term_hash(void)
{
  unsigned int i1;
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
  while (TRUE) {
    CELL *ar = hash_complex_term(&t1-1, &t1, depth, H, FALSE);
    if (ar == (CELL *)-1) {
      if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
	Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "overflow in term_hash");
	return FALSE;
      } 
      t1 = Deref(ARG1);
    } else if(ar == (CELL *)-2) {
      if (!Yap_gcl((ASP-H)*sizeof(CELL), 4, ENV, gc_P(P,CP))) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, "in term_hash");
	return FALSE;
      }
      t1 = Deref(ARG1);
    } else if (ar == NULL) {
      return FALSE;
    } else {
      i1 = MurmurHashNeutral2((const void *)H, CellSize*(ar-H),0x1a3be34a);
      break;
    }
  }
  /* got the seed and hash from SWI-Prolog */
  result = MkIntegerTerm(i1 % size);
  return Yap_unify(ARG4,result);
}

static Int
p_instantiated_term_hash(void)
{
  unsigned int i1;
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
  while (TRUE) {
    CELL *ar = hash_complex_term(&t1-1, &t1, depth, H, TRUE);
    if (ar == (CELL *)-1) {
      if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
	Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "overflow in term_hash");
	return FALSE;
      } 
      t1 = Deref(ARG1);
    } else if(ar == (CELL *)-2) {
      if (!Yap_gcl((ASP-H)*sizeof(CELL), 4, ENV, gc_P(P,CP))) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, "in term_hash");
	return FALSE;
      }
      t1 = Deref(ARG1);
    } else if (ar == NULL) {
      return FALSE;
    } else {
      i1 = MurmurHashNeutral2((const void *)H, CellSize*(ar-H),0x1a3be34a);
      break;
    }
  }
  /* got the seed and hash from SWI-Prolog */
  result = MkIntegerTerm(i1 % size);
  return Yap_unify(ARG4,result);
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
	CELL *pt0 = VarOfTerm(d0);
	CELL *pt1 = VarOfTerm(d1);
	if (pt0 >= HBREG || pt1 >= HBREG) {
	  /* one of the variables has been found before */
	  if (VarOfTerm(d0)+1 == VarOfTerm(d1)) continue;
	  goto fail;
	} else {
	  /* two new occurrences of the same variable */
	  Term n0 = MkVarTerm(), n1 = MkVarTerm();
	  Bind_Global(VarOfTerm(d0), n0);
	  Bind_Global(VarOfTerm(d1), n1);
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
	if ((CELL *)to_visit < H+1024)
	  goto out_of_stack;
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = (CELL *)*pt0;
	*pt0 = d1;
#else
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit -= 3;
	  if ((CELL *)to_visit < H+1024)
	    goto out_of_stack;
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
	if ((CELL *)to_visit < H+1024)
	  goto out_of_stack;
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = (CELL *)*pt0;
	*pt0 = d1;
#else
	  /* store the terms to visit */
	  if (pt0 < pt0_end) {
	    to_visit -= 3;
	    if ((CELL *)to_visit < H+1024)
	      goto out_of_stack;
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
  return TRUE;

 out_of_stack:
  H = HBREG;
  /* untrail all bindings made by variant */
#ifdef RATIONAL_TREES
  while (to_visit < (CELL **)ASP) {
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
    to_visit += 4;
  }
#endif
  while (TR != (tr_fr_ptr)OLDTR) {
    CELL *pt1 = (CELL *) TrailTerm(--TR);
    RESET_VARIABLE(pt1);
  }
  HBREG = B->cp_h;
  return -1;


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
  return FALSE;
}

static Int 
p_variant(void) /* variant terms t1 and t2	 */
{
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  int out;

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
      out = variant_complex(RepPair(t1)-1,
			    RepPair(t1)+1,
			    RepPair(t2)-1);
      if (out < 0) goto error;
      return out;
    }
    else return (FALSE);
  }
  if (!IsApplTerm(t2)) {
    return FALSE;
  } else {
    Functor f1 = FunctorOfTerm(t1);

    if (f1 != FunctorOfTerm(t2)) return(FALSE);
    if (IsExtensionFunctor(f1)) {
      return(unify_extension(f1, t1, RepAppl(t1), t2));
    }
    out = variant_complex(RepAppl(t1),
			  RepAppl(t1)+ArityOfFunctor(f1),
			  RepAppl(t2));
    if (out < 0) goto error;
    return out;
  }
 error:
  if (out == -1) {
    if (!Yap_gcl((ASP-H)*sizeof(CELL), 2, ENV, gc_P(P,CP))) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, "in variant");
      return FALSE;
    }
    return p_variant();
  }
  return FALSE;
}


static int subsumes_complex(register CELL *pt0, register CELL *pt0_end, register
		   CELL *pt1)
{
  register CELL **to_visit = (CELL **)ASP;
  tr_fr_ptr OLDTR = TR, new_tr;
  UInt write_mode = TRUE;


  HBREG = H;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0, d1;
    Int our_write_mode = write_mode;

    ++ pt0;
    ++ pt1;
    /* this is a version of Derefa that checks whether we are trying to
       do something evil */
    {
      CELL *npt0 = pt0;

    restart_d0:
      if (npt0 >= HBREG) {
	our_write_mode = FALSE;
      }
      d0 = *npt0;
      if (IsVarTerm(d0) &&
	  d0 != (CELL)npt0
	  ) {
	npt0 = (CELL *)d0;
	goto restart_d0;
      }
    }
    {
      CELL *npt1 = pt1;

    restart_d1:
      d1 = *npt1;
      if (IsVarTerm(d1)
	  && d1 != (CELL)npt1
	  ) {
	/* never dereference through a variable from the left-side */
	if (npt1 >= HBREG) {
	  goto fail;
	} else {
	  npt1 = (CELL *)d1;
	  goto restart_d1;
	}
      }
    }
    if (IsVarTerm(d0)) {
      if (our_write_mode) {
	/* generate a new binding */
	CELL *pt0 = VarOfTerm(d0);
	Term new = MkVarTerm();
	  
	Bind_Global(pt0, new);
	if (d0 != d1) { /* avoid loops */
	  Bind_Global(VarOfTerm(new), d1);
	  if (Yap_rational_tree_loop(VarOfTerm(new)-1,VarOfTerm(new),(CELL **)AuxSp,(CELL **)AuxBase))
	    goto fail;
	}
      } else {
	if (d0 == d1) continue;
	goto fail;
      }
      continue;
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
	to_visit -= 5;
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = (CELL *)*pt0;
	to_visit[4] = (CELL *)write_mode;
	*pt0 = d1;
#else
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit -= 4;
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = pt1;
	  to_visit[3] = (CELL *)write_mode;
	}
#endif
	write_mode = our_write_mode;
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
	  to_visit -= 5;
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = pt1;
	  to_visit[3] = (CELL *)*pt0;
	  to_visit[4] = (CELL *)write_mode;
	  *pt0 = d1;
#else
	  /* store the terms to visit */
	  if (pt0 < pt0_end) {
	    to_visit -= 4;
	    to_visit[0] = pt0;
	    to_visit[1] = pt0_end;
	    to_visit[2] = pt1;
	    to_visit[3] = (CELL *)write_mode;
	  }
#endif
	  write_mode = our_write_mode;
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
    write_mode = (Int)to_visit[ 4];
    to_visit += 5;
#else
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    write_mode = (UInt)to_visit[3];
    to_visit += 4;
#endif
    goto loop;
  }

  H = HBREG;
  /* get rid of intermediate variables  */
  new_tr = TR;
  while (TR != OLDTR) {
    /* cell we bound */
    CELL *pt1 = (CELL *) TrailTerm(--TR);
    /* cell we created */
    CELL *npt1 = (CELL *)*pt1;
    /* shorten the chain */
    if (IsVarTerm(*pt1) && IsUnboundVar(pt1)) {
      RESET_VARIABLE(pt1);
    } else {
      *pt1 = *npt1;
    }
  }
  TR = new_tr;
  HBREG = B->cp_h;
  return TRUE;

 fail:
  H = HBREG;
#ifdef RATIONAL_TREES
  while (to_visit < (CELL **)ASP) {
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
    to_visit += 5;
  }
#endif
  /* untrail all bindings made by variant */
  while (TR != (tr_fr_ptr)OLDTR) {
    CELL *pt1 = (CELL *) TrailTerm(--TR);
    RESET_VARIABLE(pt1);
  }
  HBREG = B->cp_h;
  return FALSE;
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
    if (Yap_rational_tree_loop(VarOfTerm(t1)-1,VarOfTerm(t1),(CELL **)AuxSp,(CELL **)AuxBase))
      return FALSE;
    return TRUE;
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

  t1 = TermNil;
  t2 = MkPairTerm(MkIntegerTerm(max), t1);

  return(Yap_unify(t2, ARG1));
}



#endif /* DEBUG */

int
Yap_IsListTerm(Term t)
{
  while (!IsVarTerm(t) && IsPairTerm(t)) {
    t = TailOfTerm(t);
  }
  return t == TermNil;
}

static Int
p_is_list(void)
{
  return Yap_IsListTerm(Deref(ARG1));
}



void Yap_InitUtilCPreds(void)
{
  Term cm = CurrentModule;
  Yap_InitCPred("copy_term", 2, p_copy_term, 0);
  Yap_InitCPred("duplicate_term", 2, p_duplicate_term, 0);
  Yap_InitCPred("copy_term_nat", 2, p_copy_term_no_delays, 0);
  Yap_InitCPred("ground", 1, p_ground, SafePredFlag);
  Yap_InitCPred("$variables_in_term", 3, p_variables_in_term, HiddenPredFlag);
  Yap_InitCPred("$non_singletons_in_term", 3, p_non_singletons_in_term, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("term_variables", 2, p_term_variables, 0);
  Yap_InitCPred("term_variables", 3, p_term_variables3, 0);
  Yap_InitCPred("term_attvars", 2, p_term_attvars, 0);
  Yap_InitCPred("is_list", 1, p_is_list, SafePredFlag);
  Yap_InitCPred("=@=", 2, p_variant, 0);
  CurrentModule = TERMS_MODULE;
  Yap_InitCPred("variable_in_term", 2, p_var_in_term, SafePredFlag);
  Yap_InitCPred("term_hash", 4, p_term_hash, SafePredFlag);
  Yap_InitCPred("instantiated_term_hash", 4, p_instantiated_term_hash, SafePredFlag);
  Yap_InitCPred("variant", 2, p_variant, 0);
  Yap_InitCPred("subsumes", 2, p_subsumes, SafePredFlag);
  Yap_InitCPred("variables_within_term", 3, p_variables_within_term, 3);
  Yap_InitCPred("new_variables_in_term", 3, p_new_variables_in_term, 3);
  CurrentModule = cm;
#ifdef DEBUG
  Yap_InitCPred("$force_trail_expansion", 1, p_force_trail_expansion, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("dum", 1, camacho_dum, SafePredFlag);
#endif
}

