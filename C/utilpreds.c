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

#include "absmi.h"
#include "YapHeap.h"
#include "yapio.h"
#include "attvar.h"
#ifdef HAVE_STRING_H
#include "string.h"
#endif

typedef struct {
	Term            old_var;
	Term            new_var;
}              *vcell;


STATIC_PROTO(int   copy_complex_term, (CELL *, CELL *, int, int, CELL *, CELL * CACHE_TYPE));
STATIC_PROTO(CELL  vars_in_complex_term, (CELL *, CELL *, Term CACHE_TYPE));
STATIC_PROTO(Int   p_non_singletons_in_term, ( USES_REGS1));
STATIC_PROTO(CELL  non_singletons_in_complex_term, (CELL *, CELL * CACHE_TYPE));
STATIC_PROTO(Int   p_variables_in_term, ( USES_REGS1 ));
STATIC_PROTO(Int   ground_complex_term, (CELL *, CELL * CACHE_TYPE));
STATIC_PROTO(Int   p_ground, ( USES_REGS1 ));
STATIC_PROTO(Int   p_copy_term, ( USES_REGS1 ));
STATIC_PROTO(Int   var_in_complex_term, (CELL *, CELL *, Term CACHE_TYPE));

#ifdef DEBUG
STATIC_PROTO(Int  p_force_trail_expansion, ( USES_REGS1 ));
#endif /* DEBUG */

static inline void
clean_tr(tr_fr_ptr TR0 USES_REGS) {
  if (TR != TR0) {
    do {
      Term p = TrailTerm(--TR);
      RESET_VARIABLE(p);
    } while (TR != TR0);
  }
}

static inline void
clean_dirty_tr(tr_fr_ptr TR0 USES_REGS) {
  if (TR != TR0) {
    tr_fr_ptr pt = TR0;

    do {
      Term p = TrailTerm(pt++);
      RESET_VARIABLE(p);
    } while (pt != TR);
    TR = TR0;
  }
}

static int
copy_complex_term(CELL *pt0, CELL *pt0_end, int share, int newattvs, CELL *ptf, CELL *HLow USES_REGS)
{

  struct cp_frame *to_visit0, *to_visit = (struct cp_frame *)Yap_PreAllocCodeSpace();
  CELL *HB0 = HB;
  tr_fr_ptr TR0 = TR;
  int ground = TRUE;

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
	if (ap2 >= HB && ap2 <= H) {
	  /* If this is newer than the current term, just reuse */
	  *ptf++ = d0;
	  continue;
	} 
	f = (Functor)(*ap2);

	if (IsExtensionFunctor(f)) {
#if MULTIPLE_STACKS
	  if (f == FunctorDBRef) {
	    DBRef  entryref = DBRefOfTerm(d0);
	    if (entryref->Flags & LogUpdMask) {
	      LogUpdClause *luclause = (LogUpdClause *)entryref;
	      PELOCK(100,luclause->ClPred);
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
    } else 
#if COROUTINING
      if (newattvs && IsAttachedTerm((CELL)ptd0)) {
	/* if unbound, call the standard copy term routine */
	struct cp_frame *bp;
	
	CELL new;

	bp = to_visit;
	if (!GLOBAL_attas[ExtFromCell(ptd0)].copy_term_op(ptd0, &bp, ptf PASS_REGS)) {
	  goto overflow;
	}
	to_visit = bp;
	new = *ptf;
	Bind_NonAtt(ptd0, new);
	ptf++;
      } else {
#endif
	/* first time we met this term */
	RESET_VARIABLE(ptf);
	if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
	  /* Trail overflow */
	  if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	    goto trail_overflow;
	  }
	}
	Bind_NonAtt(ptd0, (CELL)ptf);
	ptf++;
#ifdef COROUTINING
    }
#endif
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
  clean_dirty_tr(TR0 PASS_REGS);
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
  LOCAL_Error_Size = (ADDR)AuxSp-(ADDR)to_visit0;
  return -3;
}


static Term
handle_cp_overflow(int res, tr_fr_ptr TR0, UInt arity, Term t)
{
  CACHE_REGS
  XREGS[arity+1] = t;
  switch(res) {
  case -1:
    if (!Yap_gcl((ASP-H)*sizeof(CELL), arity+1, ENV, gc_P(P,CP))) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
      return 0L;
    }
    return Deref(XREGS[arity+1]);
  case -2:
    return Deref(XREGS[arity+1]);
  case -3:
    {
      UInt size = LOCAL_Error_Size;
      LOCAL_Error_Size = 0L;
      if (size > 4*1024*1024)
	size = 4*1024*1024;
      if (!Yap_ExpandPreAllocCodeSpace(size, NULL, TRUE)) {
	Yap_Error(OUT_OF_AUXSPACE_ERROR, TermNil, LOCAL_ErrorMessage);
	return 0L;
      }
    }
    return Deref(XREGS[arity+1]);
  case -4:
    if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), FALSE)) {
      Yap_Error(OUT_OF_TRAIL_ERROR, TermNil, LOCAL_ErrorMessage);
      return 0L;
    }
    return Deref(XREGS[arity+1]);
  default:
    return 0L;
  }
}

static Term
CopyTerm(Term inp, UInt arity, int share, int newattvs USES_REGS) {
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
      if ((res = copy_complex_term(Hi-2, Hi-1, share, newattvs, Hi, Hi PASS_REGS)) < 0) {
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
      if ((res = copy_complex_term(ap-1, ap+1, share, newattvs, Hi, Hi PASS_REGS)) < 0) {
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

      if ((res = copy_complex_term(ap, ap+ArityOfFunctor(f), share, newattvs, HB0+1, HB0 PASS_REGS)) < 0) {
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
  CACHE_REGS
  return CopyTerm(inp, 0, TRUE, TRUE PASS_REGS);
}

Term
Yap_CopyTermNoShare(Term inp) {
  CACHE_REGS
  return CopyTerm(inp, 0, FALSE, FALSE PASS_REGS);
}

static Int 
p_copy_term( USES_REGS1 )		/* copy term t to a new instance  */
{
  Term t = CopyTerm(ARG1, 2, TRUE, TRUE PASS_REGS); 
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  return Yap_unify(ARG2,t);
}

static Int 
p_duplicate_term( USES_REGS1 )		/* copy term t to a new instance  */
{
  Term t = CopyTerm(ARG1, 2, FALSE, TRUE PASS_REGS); 
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  return Yap_unify(ARG2,t);
}

static Int 
p_copy_term_no_delays( USES_REGS1 )		/* copy term t to a new instance  */
{
  Term t = CopyTerm(ARG1, 2, TRUE, FALSE PASS_REGS);
  if (t == 0L) {
    return FALSE;
  }
  /* be careful, there may be a stack shift here */
  return(Yap_unify(ARG2,t));
}

typedef struct copy_frame {
  CELL *start_cp;
  CELL *end_cp;
  CELL *to;
#ifdef RATIONAL_TREES
  CELL oldv;
  CELL *parent;
  int ground;
#endif
} copy_frame_t;

static int
break_rationals_complex_term(CELL *pt0, CELL *pt0_end, CELL *ptf, CELL *HLow USES_REGS)
{

  struct copy_frame *to_visit0, *to_visit = (struct copy_frame *)Yap_PreAllocCodeSpace();
  CELL *HB0 = HB;
  tr_fr_ptr TR0 = TR;
  int ground = TRUE;
  CELL *parent = ptf;

  HB = HLow;
  to_visit0 = to_visit;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, break_rationals_unk);
  break_rationals_nvar:
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
	if (to_visit+1 >= (struct copy_frame *)AuxSp) {
	  goto heap_overflow;
	}
	to_visit->start_cp = pt0;
	to_visit->end_cp = pt0_end;
	to_visit->to = ptf;
	to_visit->oldv = *pt0;
	to_visit->ground = ground;
	to_visit->parent = parent;
	parent = ptf-1;
	/* fool the system into thinking we had a variable there */
	*pt0 = TermFoundVar;
	to_visit ++;
#else
	if (pt0 < pt0_end) {
	  if (to_visit+1 >= (struct copy_frame *)AuxSp) {
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
	if (ap2 >= HB && ap2 <= H) {
	  /* If this is newer than the current term, just reuse */
	  *ptf++ = d0;
	  continue;
	} 
	f = (Functor)(*ap2);

	if (IsExtensionFunctor(f)) {
	  *ptf++ = d0;  /* you can just copy extensions, what about DB?*/
	  continue;
	}
	*ptf = AbsAppl(H);
	ptf++;
	/* store the terms to visit */
#ifdef RATIONAL_TREES
	if (to_visit+1 >= (struct copy_frame *)AuxSp) {
	  goto heap_overflow;
	}
	to_visit->start_cp = pt0;
	to_visit->end_cp = pt0_end;
	to_visit->to = ptf;
	to_visit->oldv = *pt0;
	to_visit->ground = ground;
	to_visit->parent = parent;
	parent = ptf-1;
	/* fool the system into thinking we had a variable there */
	*pt0 = TermFoundVar;
	to_visit ++;
#else
	if (pt0 < pt0_end) {
	  if (to_visit+1 >= (struct copy_frame *)AuxSp) {
	    goto heap_overflow;
	  }
	  to_visit->start_cp = pt0;
	  to_visit->end_cp = pt0_end;
	  to_visit->to = ptf;
	  to_visit->ground = ground;
	  to_visit ++;
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
	if (d0 == TermFoundVar) {
	  struct copy_frame *visited = to_visit-1;
	  CELL *end = pt0_end;
	  RESET_VARIABLE(ptf);
	  while (visited >= to_visit0) {
	    if (visited->end_cp == end) {
	      Term t[1];
	      t[0] = MkIntegerTerm(to_visit-visited);
	      *parent = Yap_MkApplTerm(FunctorLOOP,1,t);
	      break;
	    }
	    visited--;
	  }
	  ptf++;
	  ground = FALSE;
	} else {
	  *ptf++ = d0;
	}
      }
      continue;
    }

    derefa_body(d0, ptd0, break_rationals_unk, break_rationals_nvar);
    /* we have already found this cell */
    *ptf++ = (CELL) ptd0;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit --;
    if (ground) {
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
    parent = to_visit->parent;
#ifdef RATIONAL_TREES
    *pt0 = to_visit->oldv;
#endif
    ground = (ground && to_visit->ground);
    goto loop;
  }

  /* restore our nice, friendly, term to its original state */
  clean_dirty_tr(TR0 PASS_REGS);
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
    parent = to_visit->parent;
    *pt0 = to_visit->oldv;
  }
#endif
  reset_trail(TR0);
  /* follow chain of multi-assigned variables */
  return -1;

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
    parent = to_visit->parent;
    *pt0 = to_visit->oldv;
  }
#endif
  reset_trail(TR0);
  LOCAL_Error_Size = (ADDR)AuxSp-(ADDR)to_visit0;
  return -3;
}


static Term
BreakRational(Term inp, UInt arity USES_REGS) {
  Term t = Deref(inp);
  tr_fr_ptr TR0 = TR;

  if (IsVarTerm(t)) {
    return t;
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
      if ((res = break_rationals_complex_term(ap-1, ap+1, Hi, Hi PASS_REGS)) < 0) {
	H = Hi;
	if ((t = handle_cp_overflow(res, TR0, arity, t))== 0L)
	  return FALSE;
	goto restart_list;
      } else if (res) {
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

      if ((res = break_rationals_complex_term(ap, ap+ArityOfFunctor(f), HB0+1, HB0 PASS_REGS)) < 0) {
	H = HB0;
	if ((t = handle_cp_overflow(res, TR0, arity, t))== 0L)
	  return FALSE;
	goto restart_appl;
      } else if (res && FunctorOfTerm(t) != FunctorMutable) {
	H = HB0;
	return t;
      }
    }
    return tf;
  }
}
 
static Int
p_break_rational( USES_REGS1 ) 
{
  return Yap_unify(ARG2, BreakRational(ARG1, 2 PASS_REGS));
}


typedef struct restore_frame {
  CELL *start_cp;
  CELL *end_cp;
  CELL *to;
#ifdef RATIONAL_TREES
  CELL oldv;
  CELL *parent;
  int ground;
  int term_type;
#endif
} restore_frame_t;

static int
restore_rationals_complex_term(CELL *pt0, CELL *pt0_end, CELL *ptf, CELL *HLow, int pair USES_REGS)
{

  struct restore_frame *to_visit0, *to_visit = (struct restore_frame *)Yap_PreAllocCodeSpace();
  CELL *HB0 = HB;
  tr_fr_ptr TR0 = TR;
  int ground = TRUE;
  CELL *parent = ptf;

  HB = HLow;
  to_visit0 = to_visit;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, restore_rationals_unk);
  restore_rationals_nvar:
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
	if (to_visit+1 >= (struct restore_frame *)AuxSp) {
	  goto heap_overflow;
	}
	to_visit->start_cp = pt0;
	to_visit->end_cp = pt0_end;
	to_visit->to = ptf;
	to_visit->oldv = *pt0;
	to_visit->ground = ground;
	to_visit->parent = parent;
	to_visit->term_type = pair;
	parent = ptf;
	/* fool the system into thinking we had a variable there */
	*pt0 = TermFoundVar;
	to_visit ++;
#else
	if (pt0 < pt0_end) {
	  if (to_visit+1 >= (struct restore_frame *)AuxSp) {
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
	pair = TRUE;
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
	if (ap2 >= HB && ap2 <= H) {
	  /* If this is newer than the current term, just reuse */
	  *ptf++ = d0;
	  continue;
	} 
	f = (Functor)(*ap2);

	if (IsExtensionFunctor(f)) {
	  *ptf++ = d0;  /* you can just copy extensions, what about DB?*/
	  continue;
	} else if (f == FunctorLOOP) {
	  Int nlevels = IntegerOfTerm(ap2[1])-1;
	  struct restore_frame *visited = to_visit-nlevels;
	  CELL *p;
	  int type_pair;

	  if (nlevels) {
	    p = visited->parent;
	    type_pair = visited->term_type;
	  } else {
	    p = parent;
	    type_pair = pair;
	  }
	  if (type_pair) {
	    *ptf++ = AbsPair(p);
	  } else {
	    *ptf++ = AbsAppl(p-1);
	  }
	  ground = FALSE;
	  continue;
	} 
	*ptf = AbsAppl(H);
	ptf++;
	/* store the terms to visit */
#ifdef RATIONAL_TREES
	if (to_visit+1 >= (struct restore_frame *)AuxSp) {
	  goto heap_overflow;
	}
	to_visit->start_cp = pt0;
	to_visit->end_cp = pt0_end;
	to_visit->to = ptf;
	to_visit->oldv = *pt0;
	to_visit->ground = ground;
	to_visit->parent = parent;
	to_visit->term_type = pair;
	parent = ptf;
	/* fool the system into thinking we had a variable there */
	*pt0 = TermFoundVar;
	to_visit ++;
#else
	if (pt0 < pt0_end) {
	  if (to_visit+1 >= (struct restore_frame *)AuxSp) {
	    goto heap_overflow;
	  }
	  to_visit->start_cp = pt0;
	  to_visit->end_cp = pt0_end;
	  to_visit->to = ptf;
	  to_visit->ground = ground;
	  to_visit ++;
	}
#endif
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
	/* store the functor for the new term */
	H[0] = (CELL)f;
	ptf = H+1;
	H += 1+d0;
	pair = FALSE;
	if (H > ASP - 2048) {
	  goto overflow;
	}
      } else {
	*ptf++ = d0;
      }
      continue;
    }

    derefa_body(d0, ptd0, restore_rationals_unk, restore_rationals_nvar);
    /* we have already found this cell */
    *ptf++ = (CELL) ptd0;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit --;
    if (ground) {
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
    parent = to_visit->parent;
    pair = to_visit->term_type;
    *pt0 = to_visit->oldv;
#endif
    ground = (ground && to_visit->ground);
    goto loop;
  }

  /* restore our nice, friendly, term to its original state */
  clean_dirty_tr(TR0 PASS_REGS);
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
    parent = to_visit->parent;
    pair = to_visit->term_type;
    *pt0 = to_visit->oldv;
  }
#endif
  reset_trail(TR0);
  /* follow chain of multi-assigned variables */
  return -1;

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
    parent = to_visit->parent;
    pair = to_visit->term_type;
    *pt0 = to_visit->oldv;
  }
#endif
  reset_trail(TR0);
  LOCAL_Error_Size = (ADDR)AuxSp-(ADDR)to_visit0;
  return -3;
}


static Term
RestoreRational(Term inp, UInt arity USES_REGS) {
  Term t = Deref(inp);
  tr_fr_ptr TR0 = TR;

  if (IsVarTerm(t)) {
    return t;
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
      if ((res = restore_rationals_complex_term(ap-1, ap+1, Hi, Hi, TRUE PASS_REGS)) < 0) {
	H = Hi;
	if ((t = handle_cp_overflow(res, TR0, arity, t))== 0L)
	  return FALSE;
	goto restart_list;
      } else if (res) {
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

      if ((res = restore_rationals_complex_term(ap, ap+ArityOfFunctor(f), HB0+1, HB0, FALSE PASS_REGS)) < 0) {
	H = HB0;
	if ((t = handle_cp_overflow(res, TR0, arity, t))== 0L)
	  return FALSE;
	goto restart_appl;
      } else if (res && FunctorOfTerm(t) != FunctorMutable) {
	H = HB0;
	return t;
      }
    }
    return tf;
  }
}
 
static Int
p_restore_rational( USES_REGS1 ) 
{
  return Yap_unify(ARG2, RestoreRational(ARG1, 2 PASS_REGS));
}


/* 
   FAST EXPORT ROUTINE. Export a Prolog term to something like:

   CELL 0: offset for start of term
   CELL 1: size of actual term (to be copied to stack)
   CELL 2: the original term (just for reference)

   Atoms and functors:
   - atoms are either:
     0 and a char *string
     -1 and a wchar_t *string
   - functors are a CELL with arity and a string.

   Compiled Term.

 */

static inline
CELL *CellDifH(CELL *hptr, CELL *hlow)
{
  return (CELL *)((char *)hptr-(char *)hlow);
}

#define AdjustSizeAtom(X)	(((CELL)(X)+(8-1)) & ~(8-1))

static inline
CELL *AdjustSize(CELL *x, char *buf)
{
  UInt offset = (char *)x-buf;
  return (CELL*)(buf+AdjustSizeAtom(offset));
}

/* export an atom from the symbol table to a buffer */
static inline 
Atom export_atom(Atom at, char **hpp, char *buf, size_t len)
{				 
  char *ptr, *p0;
  size_t sz;

  ptr = *hpp;
  ptr = (char *)AdjustSize((CELL*)ptr, buf);
  
  p0 = ptr;
  if (IsWideAtom(at)) {
    wchar_t *wptr = (wchar_t *)ptr;
    *wptr++ = -1;
    sz = wcslen(RepAtom(at)->WStrOfAE);
    if (sizeof(wchar_t)*(sz+1) >= len)
      return (Atom)NULL;
    wcsncpy(wptr, RepAtom(at)->WStrOfAE, len);
    *hpp = (char *)(wptr+(sz+1));
  } else {
    *ptr++ = 0;
    sz = strlen(RepAtom(at)->StrOfAE);
    if (sz + 1 + sizeof(wchar_t) >= len)
      return (Atom)NULL;
    strcpy(ptr, RepAtom(at)->StrOfAE);
    *hpp = ptr+(sz+1);
  }
  ptr += sz;
  return (Atom)(p0-buf);
}

/* place a buffer: first arity then the atom */
static inline 
Functor export_functor(Functor f, char **hpp, char *buf, size_t len)
{
  CELL *hptr = AdjustSize((CELL *)*hpp, buf);
  UInt arity = ArityOfFunctor(f);
  if (2*sizeof(CELL) >= len)
    return NULL;
  hptr[0] = arity;
  *hpp = (char *)(hptr+1);
  if (!export_atom(NameOfFunctor(f), hpp, buf, len))
    return NULL;
  /* increment so that it cannot be mistaken with a functor on the stack,
     (increment is used as a tag ........01
  */
  return (Functor)(((char *)hptr-buf)+1);
}

#define export_derefa_body(D,A,LabelUnk,LabelNonVar)                \
		do {                                         \
		  if ((CELL *)(D) < CellDifH(H,HLow)) { (A) = (CELL *)(D); break; } \
                   (A) = (CELL *)(D);                        \
                   (D) = *(CELL *)(D);                       \
                   if(!IsVarTerm(D)) goto LabelNonVar;       \
		LabelUnk:      ;                             \
		} while (Unsigned(A) != (D))


static int
export_term_to_buffer(Term inpt, char *buf, char *bptr, CELL *t0 , CELL *tf, size_t len)
{
  char *td = bptr;
  CELL *bf = (CELL *)buf;
  if (buf + len < (char *)((CELL *)td + (tf-t0))) {
    return FALSE;
  }
  memcpy((void *)td, (void *)t0, (tf-t0)* sizeof(CELL)); 
  bf[0] = (td-buf);
  bf[1] = (tf-t0);
  bf[2] = inpt;
  return bf[0]+sizeof(CELL)*bf[1];
}


static size_t
export_complex_term(Term tf, CELL *pt0, CELL *pt0_end, char * buf, size_t len0, int newattvs, CELL *ptf, CELL *HLow USES_REGS)
{
  struct cp_frame *to_visit0, *to_visit = (struct cp_frame *)Yap_PreAllocCodeSpace();
  CELL *HB0 = HB;
  tr_fr_ptr TR0 = TR;
  int ground = TRUE;
  char *bptr = buf+ 3*sizeof(CELL);
  size_t len = len0;

  HB = HLow;
  to_visit0 = to_visit;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, export_term_unk);
  export_term_nvar:
    {
      if (IsPairTerm(d0)) {
	CELL *ap2 = RepPair(d0);
	if (ap2 < CellDifH(H,HLow)) {
	  /* If this is newer than the current term, just reuse */
	  *ptf++ = d0;
	  continue;
	} 
	*ptf = AbsPair(CellDifH(H,HLow));
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
	*pt0 = AbsPair(CellDifH(H,HLow));
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
	if (ap2 < CellDifH(H,HLow)) {
	  /* If this is newer than the current term, just reuse */
	  *ptf++ = d0;
	  continue;
	} 
	f = (Functor)(*ap2);

	*ptf++ = AbsAppl(CellDifH(H,HLow));
	if (IsExtensionFunctor(f)) {
	  UInt sz;

	  /* make sure to export floats */
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
	  continue;
	}
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
	ptf = H+1;
	H += 1+d0;
	if (H > ASP - 2048) {
	  goto overflow;
	}
	ptf[-1] = (CELL)export_functor(f, &bptr, buf, len);
	len = len0 - (bptr-buf);
	if (H > ASP - 2048) {
	  goto overflow;
	}
      } else {
	if (IsAtomTerm(d0)) {
	  *ptf = MkAtomTerm(export_atom(AtomOfTerm(d0), &bptr, buf, len));
	  ptf++;
	  len = len0 - (bptr-buf);
	} else {
	  *ptf++ = d0;
	}
      }
      continue;
    }

    export_derefa_body(d0, ptd0, export_term_unk, export_term_nvar);
    ground = FALSE;
    if (ptd0 < CellDifH(H,HLow)) { 
      /* we have already found this cell */
      *ptf++ = (CELL) ptd0;
    } else {
#if COROUTINING
      if (newattvs && IsAttachedTerm((CELL)ptd0) && FALSE) {
	/* if unbound, call the standard export term routine */
	struct cp_frame *bp;

	CELL new;

	bp = to_visit;
	if (!GLOBAL_attas[ExtFromCell(ptd0)].copy_term_op(ptd0, &bp, ptf PASS_REGS)) {
	  goto overflow;
	}
	to_visit = bp;
	new = *ptf;
	Bind_NonAtt(ptd0, new);
	ptf++;
      } else {
#endif
	/* first time we met this term */
	*ptf = (CELL)CellDifH(ptf,HLow);
	if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
	  /* Trail overflow */
	  if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	    goto trail_overflow;
	  }
	}
	Bind_NonAtt(ptd0, (CELL)ptf);
	ptf++;
#ifdef COROUTINING
      }
#endif
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit --;
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
  clean_dirty_tr(TR0 PASS_REGS);
  HB = HB0;
  return export_term_to_buffer(tf, buf, bptr, HLow, H, len0);

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
  LOCAL_Error_Size = (ADDR)AuxSp-(ADDR)to_visit0;
  return -3;
}

static size_t
ExportTerm(Term inp, char * buf, size_t len, UInt arity, int newattvs USES_REGS) {
  Term t = Deref(inp);
  tr_fr_ptr TR0 = TR;
  size_t res = 0;
  CELL *Hi;

  do {
    if ((Int)res < 0) {
      H = Hi;
      TR = TR0;
      if ((t = handle_cp_overflow(res, TR0, arity, t))== 0L)
	return res;
    }
    Hi = H;
    TR0 = TR;
    res = export_complex_term(inp, &t-1, &t, buf, len, newattvs, Hi, Hi PASS_REGS);
  } while ((Int)res < 0);
  return res;
}
 
size_t
Yap_ExportTerm(Term inp, char * buf, size_t len, UInt arity) {
  CACHE_REGS
  return ExportTerm(inp, buf, len, arity, TRUE PASS_REGS);
}


static CELL *
ShiftPtr(CELL t, char *base)
{
  return (CELL *)(base+t);
}

static Atom
AddAtom(Atom t, char *buf)
{
  char *s = buf+(UInt)t;

  if (!*s) {
    return Yap_LookupAtom(s+1);
  } else {
    wchar_t *w = (wchar_t *)s;
    return Yap_LookupWideAtom(w+1);
  }
}

static UInt
FetchFunctor(CELL *pt, char *buf)
{
  CELL *ptr = (CELL *)(buf+(*pt-1)); 
  // do arity first
  UInt arity = *ptr++;
  Atom name, at;
  // and then an atom
  ptr = AdjustSize(ptr, buf);
  name = (Atom)((char *)ptr-buf);
  at = AddAtom(name, buf);
  *pt = (CELL)Yap_MkFunctor(at, arity);
  return arity;
}


static CELL *import_compound(CELL *hp, char *abase, char *buf, CELL *amax);
static CELL *import_pair(CELL *hp, char *abase, char *buf, CELL *amax);

static CELL *
import_arg(CELL *hp, char *abase, char *buf, CELL *amax)
{
  Term t = *hp;
  if (IsVarTerm(t)) {
    hp[0] = (CELL)ShiftPtr(t, abase);
  } else if (IsAtomTerm(t)) {
    hp[0] = MkAtomTerm(AddAtom(AtomOfTerm(t), buf));
  } else if (IsPairTerm(t)) {
    CELL *newp = ShiftPtr((CELL)RepPair(t), abase);
    hp[0] = AbsPair(newp);
    if (newp > amax) {
      amax = import_pair(newp, abase, buf, newp);
    }
  } else if (IsApplTerm(t)) {
    CELL *newp = ShiftPtr((CELL)RepAppl(t), abase);
    hp[0] = AbsAppl(newp);
    if (newp > amax) {
      amax = import_compound(newp, abase, buf, newp);
    }
  }
  return amax;
}

static CELL *
import_compound(CELL *hp, char *abase, char *buf, CELL *amax)
{
  Functor f = (Functor)*hp;
  UInt ar, i;

  if (!((CELL)f & 1) && IsExtensionFunctor(f))
    return amax;
  ar = FetchFunctor(hp, buf);
  for (i=1; i<=ar; i++) {
    amax = import_arg(hp+i, abase, buf, amax);
  }    
  return amax;
}

static CELL *
import_pair(CELL *hp, char *abase, char *buf, CELL *amax)
{
  amax = import_arg(hp, abase, buf, amax);
  amax = import_arg(hp+1, abase, buf, amax);
  return amax;
}

Term
Yap_ImportTerm(char * buf) {
  CACHE_REGS
  CELL *bc = (CELL *)buf;
  size_t sz = bc[1];
  Term tinp, tret;

  tinp = bc[2];
  if (IsVarTerm(tinp))
    return MkVarTerm();
  if (IsAtomOrIntTerm(tinp)) {
    if (IsAtomTerm(tinp)) {
      char *pt = (char *)AdjustSize(bc+3, buf);
      return MkAtomTerm(Yap_LookupAtom(pt));
    } else
      return tinp;
  }
  if (H + sz > ASP)
    return (Term)0;
  memcpy(H, buf+bc[0], sizeof(CELL)*sz);
  if (IsApplTerm(tinp)) {
    tret = AbsAppl(H);
    import_compound(H, (char *)H, buf, H);
  } else {
    tret = AbsPair(H);
    import_pair(H, (char *)H, buf, H);
  }
  H += sz;
  return tret;
}

size_t
Yap_SizeOfExportedTerm(char * buf) {
  CELL *bc = (CELL *)buf;

  return bc[0]+bc[1]*sizeof(CELL);
}

static Int
p_export_term( USES_REGS1 )
{
  size_t sz = 4096, osz;
  char *export_buf;
  do {
    export_buf  = malloc(sz);
    if (!export_buf)
      return FALSE;
    if (!(osz = Yap_ExportTerm(ARG1, export_buf, sz, 1))) {
      sz += 4096;
      free(export_buf);
    }
  } while (!osz);
  return Yap_unify(ARG3,MkIntegerTerm(osz)) &&
    Yap_unify(ARG2, MkIntegerTerm((Int)export_buf));
}

static Int
p_import_term( USES_REGS1 )
{
  char *export_buf = (char *)IntegerOfTerm(Deref(ARG1));
  if (!export_buf)
    return FALSE;
  Int out = Yap_unify(ARG2,Yap_ImportTerm(export_buf));
  return out;						 
}

static Int
p_kill_exported_term( USES_REGS1 )
{
  char *export_buf = (char *)IntegerOfTerm(Deref(ARG1));
  if (!export_buf)
    return FALSE;
  free(export_buf);
  return TRUE;						 
}


static Term vars_in_complex_term(register CELL *pt0, register CELL *pt0_end, Term inp USES_REGS)
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
    if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
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

  clean_tr(TR0 PASS_REGS);
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
  LOCAL_Error_TYPE = OUT_OF_TRAIL_ERROR;
  LOCAL_Error_Size = (TR-TR0)*sizeof(tr_fr_ptr *);
  clean_tr(TR0 PASS_REGS);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  return 0L;
  
 aux_overflow:
  LOCAL_Error_Size = (to_visit-to_visit0)*sizeof(CELL **);
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  LOCAL_Error_TYPE = OUT_OF_AUXSPACE_ERROR;
  clean_tr(TR0 PASS_REGS);
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
  clean_tr(TR0 PASS_REGS);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  LOCAL_Error_TYPE = OUT_OF_STACK_ERROR;
  LOCAL_Error_Size = (ASP-H)*sizeof(CELL);
  return 0L;
  
}

static int
expand_vts( int args USES_REGS )
{
  UInt expand = LOCAL_Error_Size;
  yap_error_number yap_errno = LOCAL_Error_TYPE;
  
  LOCAL_Error_Size = 0;
  LOCAL_Error_TYPE = YAP_NO_ERROR;
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
p_variables_in_term( USES_REGS1 )	/* variables in term t		 */
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
      if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
	clean_tr(TR-count PASS_REGS);
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
				 RepPair(t)+1, ARG2 PASS_REGS);
    }
    else {
      Functor f = FunctorOfTerm(t);
      out = vars_in_complex_term(RepAppl(t),
				 RepAppl(t)+
				 ArityOfFunctor(f), ARG2 PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts( 3 PASS_REGS ))
	return FALSE;
    }
  } while (out == 0L);
  clean_tr(TR-count PASS_REGS);
  return Yap_unify(ARG3,out);
}


static Int 
p_term_variables( USES_REGS1 )	/* variables in term t		 */
{
  Term out;

  if (!Yap_IsListOrPartialListTerm(ARG2)) {
    Yap_Error(TYPE_ERROR_LIST,ARG2,"term_variables/2");
    return FALSE;
  }

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
				 RepPair(t)+1, TermNil PASS_REGS);
    }
    else {
      Functor f = FunctorOfTerm(t);
      out = vars_in_complex_term(RepAppl(t),
				 RepAppl(t)+
				 ArityOfFunctor(f), TermNil PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts( 3 PASS_REGS ))
	return FALSE;
    }
  } while (out == 0L);
  return Yap_unify(ARG2,out);
}

static Term attvars_in_complex_term(register CELL *pt0, register CELL *pt0_end, Term inp USES_REGS)
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
      if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
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

  clean_tr(TR0 PASS_REGS);
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
  LOCAL_Error_TYPE = OUT_OF_TRAIL_ERROR;
  LOCAL_Error_Size = (TR-TR0)*sizeof(tr_fr_ptr *);
  clean_tr(TR0 PASS_REGS);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  return 0L;
  
 aux_overflow:
  LOCAL_Error_Size = (to_visit-to_visit0)*sizeof(CELL **);
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  LOCAL_Error_TYPE = OUT_OF_AUXSPACE_ERROR;
  clean_tr(TR0 PASS_REGS);
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
  clean_tr(TR0 PASS_REGS);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  LOCAL_Error_TYPE = OUT_OF_STACK_ERROR;
  LOCAL_Error_Size = (ASP-H)*sizeof(CELL);
  return 0L;
  
}

static Int 
p_term_attvars( USES_REGS1 )	/* variables in term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG1);
    if (IsVarTerm(t)) {
      out = attvars_in_complex_term(VarOfTerm(t)-1,
				    VarOfTerm(t)+1, TermNil PASS_REGS);
    }  else if (IsPrimitiveTerm(t)) {
      return Yap_unify(TermNil, ARG2);
    } else if (IsPairTerm(t)) {
      out = attvars_in_complex_term(RepPair(t)-1,
				 RepPair(t)+1, TermNil PASS_REGS);
    }
    else {
      Functor f = FunctorOfTerm(t);
      out = attvars_in_complex_term(RepAppl(t),
				 RepAppl(t)+
				 ArityOfFunctor(f), TermNil PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts( 3 PASS_REGS ))
	return FALSE;
    }
  } while (out == 0L);
  return Yap_unify(ARG2,out);
}

static Int 
p_term_variables3( USES_REGS1 )	/* variables in term t		 */
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
				 RepPair(t)+1, ARG3 PASS_REGS);
    }
    else {
      Functor f = FunctorOfTerm(t);
      out = vars_in_complex_term(RepAppl(t),
				 RepAppl(t)+
				 ArityOfFunctor(f), ARG3 PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts( 3 PASS_REGS ))
	return FALSE;
    }
  } while (out == 0L);

  return Yap_unify(ARG2,out);
}


static Term vars_within_complex_term(register CELL *pt0, register CELL *pt0_end, Term inp USES_REGS)
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
      if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
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

  clean_tr(TR0 PASS_REGS);
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
  LOCAL_Error_TYPE = OUT_OF_TRAIL_ERROR;
  LOCAL_Error_Size = (TR-TR0)*sizeof(tr_fr_ptr *);
  clean_tr(TR0 PASS_REGS);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  return 0L;
  
 aux_overflow:
  LOCAL_Error_Size = (to_visit-to_visit0)*sizeof(CELL **);
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  LOCAL_Error_TYPE = OUT_OF_AUXSPACE_ERROR;
  clean_tr(TR0 PASS_REGS);
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
  clean_tr(TR0 PASS_REGS);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  LOCAL_Error_TYPE = OUT_OF_STACK_ERROR;
  LOCAL_Error_Size = (ASP-H)*sizeof(CELL);
  return 0L;
  
}

static Int 
p_variables_within_term( USES_REGS1 )	/* variables within term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG2);
    if (IsVarTerm(t)) {
      out = vars_within_complex_term(VarOfTerm(t)-1,
				     VarOfTerm(t), Deref(ARG1) PASS_REGS);

    }  else if (IsPrimitiveTerm(t)) 
      out = TermNil;
    else if (IsPairTerm(t)) {
      out = vars_within_complex_term(RepPair(t)-1,
				     RepPair(t)+1, Deref(ARG1) PASS_REGS);
    }
    else {
      Functor f = FunctorOfTerm(t);
      out = vars_within_complex_term(RepAppl(t),
				 RepAppl(t)+
				     ArityOfFunctor(f), Deref(ARG1) PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts( 3 PASS_REGS ))
	return FALSE;
    }
  } while (out == 0L);
  return Yap_unify(ARG3,out);
}

static Term new_vars_in_complex_term(register CELL *pt0, register CELL *pt0_end, Term inp USES_REGS)
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
      if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
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
    if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
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

  clean_tr(TR0 PASS_REGS);
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
  LOCAL_Error_TYPE = OUT_OF_TRAIL_ERROR;
  LOCAL_Error_Size = (TR-TR0)*sizeof(tr_fr_ptr *);
  clean_tr(TR0 PASS_REGS);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  return 0L;
  
 aux_overflow:
  LOCAL_Error_Size = (to_visit-to_visit0)*sizeof(CELL **);
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  LOCAL_Error_TYPE = OUT_OF_AUXSPACE_ERROR;
  clean_tr(TR0 PASS_REGS);
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
  clean_tr(TR0 PASS_REGS);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  LOCAL_Error_TYPE = OUT_OF_STACK_ERROR;
  LOCAL_Error_Size = (ASP-H)*sizeof(CELL);
  return 0L;
  
}

static Int 
p_new_variables_in_term( USES_REGS1 )	/* variables within term t		 */
{
  Term out;

  do {
    Term t = Deref(ARG2);
    if (IsVarTerm(t)) {
      out = new_vars_in_complex_term(VarOfTerm(t)-1,
				     VarOfTerm(t), Deref(ARG1) PASS_REGS);

    }  else if (IsPrimitiveTerm(t)) 
      out = TermNil;
    else if (IsPairTerm(t)) {
      out = new_vars_in_complex_term(RepPair(t)-1,
				     RepPair(t)+1, Deref(ARG1) PASS_REGS);
    }
    else {
      Functor f = FunctorOfTerm(t);
      out = new_vars_in_complex_term(RepAppl(t),
			     RepAppl(t)+
			     ArityOfFunctor(f), Deref(ARG1) PASS_REGS);
    }
    if (out == 0L) {
      if (!expand_vts( 3 PASS_REGS ))
	return FALSE;
    }
  } while (out == 0L);
  return Yap_unify(ARG3,out);
}

static Term non_singletons_in_complex_term(register CELL *pt0, register CELL *pt0_end USES_REGS)
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

  clean_tr(TR0 PASS_REGS);
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
  clean_tr(TR0 PASS_REGS);
  if (H != InitialH) {
    /* close the list */
    RESET_VARIABLE(H-1);
  }
  return 0L;
}
 
static Int 
p_non_singletons_in_term( USES_REGS1 )	/* non_singletons in term t		 */
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
					   RepPair(t)+1 PASS_REGS);
    } else {
      out = non_singletons_in_complex_term(RepAppl(t),
					   RepAppl(t)+
					   ArityOfFunctor(FunctorOfTerm(t)) PASS_REGS);
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

static Int ground_complex_term(register CELL *pt0, register CELL *pt0_end USES_REGS)
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
 
static Int finite_tree_complex_term(register CELL *pt0, register CELL *pt0_end USES_REGS)
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
      if (d0 == TermFoundVar) {
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
      if (IsPairTerm(d0)) {
	if (to_visit + 1024 >= (CELL **)AuxSp) {
	  goto aux_overflow;
	}
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	to_visit += 3;
	*pt0 = TermFoundVar;
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
	*pt0 = TermFoundVar;
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
    continue;
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
 
int Yap_IsAcyclicTerm(Term t)
{
  CACHE_REGS
  while (TRUE) {
    Int out;

    if (IsVarTerm(t)) {
      return TRUE;
    }  else if (IsPrimitiveTerm(t)) {
      return TRUE;
    } else if (IsPairTerm(t)) {
      if ((out =  finite_tree_complex_term(RepPair(t)-1,
				    RepPair(t)+1 PASS_REGS)) >= 0) {
	return out;
      }
    } else {
      Functor fun = FunctorOfTerm(t);
      
      if (IsExtensionFunctor(fun))
	return TRUE;
      else if ((out = finite_tree_complex_term(RepAppl(t),
					     RepAppl(t)+
					     ArityOfFunctor(fun) PASS_REGS)) >= 0) {
	     return out;
      }
    }
    if (out < 0) {
      *H++ = t;
      if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
	Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "overflow in finite_tree");
	return FALSE;
      }      
      t = *--H;
    }
  }
}

static Int 
p_acyclic_term( USES_REGS1 )			/* finite_tree(+T)		 */
{
  return Yap_IsAcyclicTerm(Deref(ARG1));
}

static Int 
p_cyclic_term( USES_REGS1 )			/* finite_tree(+T)		 */
{
  return !Yap_IsAcyclicTerm(Deref(ARG1));
}

int Yap_IsGroundTerm(Term t)
{
  CACHE_REGS
  while (TRUE) {
    Int out;

    if (IsVarTerm(t)) {
      return FALSE;
    }  else if (IsPrimitiveTerm(t)) {
      return TRUE;
    } else if (IsPairTerm(t)) {
      if ((out =ground_complex_term(RepPair(t)-1,
				    RepPair(t)+1 PASS_REGS)) >= 0) {
	return out;
      }
    } else {
      Functor fun = FunctorOfTerm(t);
      
      if (IsExtensionFunctor(fun))
	return TRUE;
      else if ((out = ground_complex_term(RepAppl(t),
					     RepAppl(t)+
					     ArityOfFunctor(fun) PASS_REGS)) >= 0) {
	     return out;
      }
    }
    if (out < 0) {
      *H++ = t;
      if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
	Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "overflow in ground");
	return FALSE;
      }      
      t = *--H;
    }
  }
}

static Int 
p_ground( USES_REGS1 )			/* ground(+T)		 */
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


static Int sz_ground_complex_term(register CELL *pt0, register CELL *pt0_end, int ground USES_REGS)
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
  CACHE_REGS
  if (IsVarTerm(t)) {
    if (!ground)
      return 1;
    return 0;
  }  else if (IsPrimitiveTerm(t)) {
    return 1;
  } else if (IsPairTerm(t)) {
    int sz = sz_ground_complex_term(RepPair(t)-1, RepPair(t)+1, ground PASS_REGS);
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
			    ground PASS_REGS);
   if (sz <= 0)
     return sz;
   return 1+ArityOfFunctor(fun)+sz;
  }  
}

static Int var_in_complex_term(register CELL *pt0,
			       register CELL *pt0_end,
			       Term v USES_REGS)
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
      clean_tr(TR0 PASS_REGS);
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
  clean_tr(TR0 PASS_REGS);
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
var_in_term(Term v, Term t USES_REGS)	/* variables in term t		 */
{

  if (IsVarTerm(t)) {
    return(v == t);
  } else if (IsPrimitiveTerm(t)) {
    return(FALSE);
  } else if (IsPairTerm(t)) {
    return(var_in_complex_term(RepPair(t)-1,
			       RepPair(t)+1,v PASS_REGS));
  }
  else return(var_in_complex_term(RepAppl(t),
				  RepAppl(t)+
				  ArityOfFunctor(FunctorOfTerm(t)),v PASS_REGS));
}

static Int
p_var_in_term( USES_REGS1 )
{
  return(var_in_term(Deref(ARG2), Deref(ARG1) PASS_REGS));
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

typedef struct visited {
  CELL *start;
  CELL  *end;
  CELL old;
  UInt vdepth;
} visited_t;

static CELL *
hash_complex_term(register CELL *pt0,
		  register CELL *pt0_end,
		  Int depth,
		  CELL *st,
		  int variant USES_REGS)
{
  register visited_t *to_visit0, *to_visit = (visited_t *)Yap_PreAllocCodeSpace();

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
      if (IsAtomOrIntTerm(d0)) {
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
	if (to_visit + 256 >= (visited_t *)AuxSp) {
	  goto aux_overflow;
	}
	to_visit->start = pt0;
	to_visit->end = pt0_end;
	to_visit->old = *pt0;
	to_visit->vdepth = depth;
	to_visit++;
	depth--;
	*pt0 = TermFoundVar;
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
	  CELL fc = (CELL)f;

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
	if (to_visit + 1024 >= (visited_t *)AuxSp) {
	  goto aux_overflow;
	}
	to_visit->start = pt0;
	to_visit->end = pt0_end;
	to_visit->old = *pt0;
	to_visit->vdepth = depth;
	to_visit++;
	depth--;
	*pt0 = TermFoundVar;
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
    to_visit--;
    pt0 = to_visit->start;
    pt0_end = to_visit->end;
    *pt0 = to_visit->old;
    depth = to_visit->vdepth;
    goto loop;
  }
  return st;

 aux_overflow:
  /* unwind stack */
  while (to_visit > to_visit0) {
    to_visit --;
    pt0 = to_visit->start;
    *pt0 = to_visit->old;
  }
  return (CELL *)-1;

 global_overflow:
  /* unwind stack */
  while (to_visit > to_visit0) {
    to_visit --;
    pt0 = to_visit->start;
    *pt0 = to_visit->old;
  }
  return (CELL *) -2;
}
 
Int
Yap_TermHash(Term t, Int size, Int depth, int variant)
{
  CACHE_REGS
  unsigned int i1;
  Term t1 = Deref(t);

  while (TRUE) {
    CELL *ar = hash_complex_term(&t1-1, &t1, depth, H, FALSE PASS_REGS);
    if (ar == (CELL *)-1) {
      if (!Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE)) {
	Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "overflow in term_hash");
	return FALSE;
      } 
      t1 = Deref(ARG1);
    } else if(ar == (CELL *)-2) {
      if (!Yap_gcl((ASP-H)*sizeof(CELL), 0, ENV, gc_P(P,CP))) {
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
  return i1 % size;
}

static Int
p_term_hash( USES_REGS1 )
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
    CELL *ar = hash_complex_term(&t1-1, &t1, depth, H, FALSE PASS_REGS);
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
p_instantiated_term_hash( USES_REGS1 )
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
    CELL *ar = hash_complex_term(&t1-1, &t1, depth, H, TRUE PASS_REGS);
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
		   CELL *pt1 USES_REGS)
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

static int
is_variant(Term t1, Term t2, int parity USES_REGS)
{  
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
			    RepPair(t2)-1 PASS_REGS);
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
			  RepAppl(t2) PASS_REGS);
    if (out < 0) goto error;
    return out;
  }
 error:
  if (out == -1) {
    if (!Yap_gcl((ASP-H)*sizeof(CELL), parity, ENV, gc_P(P,CP))) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, "in variant");
      return FALSE;
    }
    return is_variant(t1, t2, parity PASS_REGS);
  }
  return FALSE;
}

int
Yap_Variant(Term t1, Term t2) 
{
  CACHE_REGS
  return is_variant(t1, t2, 0 PASS_REGS);
}

static Int 
p_variant( USES_REGS1 ) /* variant terms t1 and t2	 */
{
  return is_variant(Deref(ARG1), Deref(ARG2), 2 PASS_REGS);
}


static int subsumes_complex(register CELL *pt0, register CELL *pt0_end, register
		   CELL *pt1 USES_REGS)
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
p_subsumes( USES_REGS1 ) /* subsumes terms t1 and t2	 */
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
			     RepPair(t2)-1 PASS_REGS));
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
			   RepAppl(t2) PASS_REGS));
  }
}


static int term_subsumer_complex(register CELL *pt0, register CELL *pt0_end, register
				 CELL *pt1, CELL *npt USES_REGS)
{
  register CELL **to_visit = (CELL **)ASP;
  tr_fr_ptr OLDTR = TR;
  int out;
  CELL *bindings = NULL, *tbindings = NULL;
  HB = H;

 loop:
  while (pt0 < pt0_end) {
    register CELL d0, d1;

    ++ pt0;
    ++ pt1;
    d0 = Derefa(pt0);
    d1 = Derefa(pt1);
    if (d0 == d1) {
      *npt++ = d0;
      continue;
    } else if (IsVarTerm(d0)) {
      CELL *match, *omatch = NULL;

      match = VarOfTerm(d0); 
      if (match >= HB) {
	while (match >= HB) {
	  /* chained to a sequence */
	  if (Yap_eq(d1, match[1]) ) {
	    *npt++ = match[2];
	    break;
	  }
	  omatch = match;
	  match = (CELL *)match[3];
	}
	/* found a match */
	if (match >= HB)
	  continue;
	/* could not find a match, add to end of chain */
 	RESET_VARIABLE(H); /* key */
 	H[1] = d1; /* comparison value */
	H[2] = (CELL)npt; /* new value */
	H[3] = (CELL)match; /* end of chain points back to first cell */
	omatch[3] = (CELL)H;
	H+=4;
	RESET_VARIABLE(npt);
	npt++;
	continue;
      }
      if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
	goto trail_overflow;
      }
      RESET_VARIABLE(H);
      H[1] = d1;
      H[2] = (CELL)npt;
      H[3] = d0;
      Bind(VarOfTerm(d0), (CELL)H);
      H+=4;
      RESET_VARIABLE(npt);
      npt++;
      continue;
    } else if (IsPairTerm(d0) && IsPairTerm(d1)) {
      CELL *match = bindings;

      while (match) {
	if (match[0] == d0 && match[1] == d1) {
	  *npt++ = match[2];
	  break;
	}
	match = (CELL *)match[3];
      }
      if (match) {
	continue;
      }
      if (bindings) {
	*tbindings = (CELL)H;
      } else {
	bindings = H;
      }
      H[0] = d0;
      H[1] = d1;
      H[2] = AbsPair(H+4);
      H[3] = (CELL)NULL;
      tbindings = H+3;
      H+=4;
      *npt++ = AbsPair(H);
#ifdef RATIONAL_TREES
      /* now link the two structures so that no one else will */
      /* come here */
      to_visit -= 5;
      to_visit[0] = pt0;
      to_visit[1] = pt0_end;
      to_visit[2] = pt1;
      to_visit[3] = tbindings;
      to_visit[4] = npt;
#else
      /* store the terms to visit */
      if (pt0 < pt0_end) {
	to_visit -= 4;
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = npt;
      }
#endif
      pt0 = RepPair(d0) - 1;
      pt0_end = RepPair(d0) + 1;
      pt1 = RepPair(d1) - 1;
      npt = H;
      H += 2;
      if (H > (CELL *)to_visit -1024)
	goto stack_overflow;
      continue;
    } else if (IsApplTerm(d0) && IsApplTerm(d1)) {
      CELL *ap2 = RepAppl(d0);
      CELL *ap3 = RepAppl(d1);
      Functor f = (Functor)(*ap2);
      Functor f2 = (Functor)(*ap3);
      if (f == f2) {
	CELL *match = bindings;

	if (IsExtensionFunctor(f)) {
	  if (unify_extension(f, d0, ap2, d1)) {
	    *npt++ = d0;
	    continue;
	  }
	}
	while (match) {
	  if (match[0] == d0 && match[1] == d1) {
	    *npt++ = match[2];
	    break;
	  }
	  match = (CELL *)match[3];
	}
	if (match) {
	  continue;
	}
	if (bindings) {
	  *tbindings = (CELL)H;
	} else {
	  bindings = H;
	}
	H[0] = d0;
	H[1] = d1;
	H[2] = AbsAppl(H+4);
	H[3] = (CELL)NULL;
	tbindings = H+3;
	H+=4;
	*npt++ = AbsAppl(H);
#ifdef RATIONAL_TREES
	/* now link the two structures so that no one else will */
	/* come here */
	to_visit -= 5;
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = tbindings;
	to_visit[4] = npt;
#else
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit -= 4;
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = pt1;
	  to_visit[3] = npt;
	}
#endif
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
	pt1 = ap3;
	npt = H;
	*npt++ = (CELL)f;
	H += d0;
	if (H > (CELL *)to_visit -1024)
	  goto stack_overflow;
	continue;
      }
    }
    RESET_VARIABLE(npt);
    npt++;
  }
  /* Do we still have compound terms to visit */
  if (to_visit < (CELL **)ASP) {
#ifdef RATIONAL_TREES
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    tbindings = to_visit[3];
    npt = to_visit[ 4];
    if (!tbindings) {
      bindings = NULL;
    }
    to_visit += 5;
#else
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    npt = to_visit[3];
    to_visit += 4;
#endif
    goto loop;
  }
  out = 1;

 complete:
  /* get rid of intermediate variables  */
  while (TR != OLDTR) {
    CELL *pt1 = (CELL *) TrailTerm(--TR);
    RESET_VARIABLE(pt1);
  }
  HBREG = B->cp_h;
  return out;

 stack_overflow:
  out = -1;
  goto complete;

 trail_overflow:
  out = -2;
  goto complete;

}

static Int 
p_term_subsumer( USES_REGS1 ) /* term_subsumer terms t1 and t2	 */
{
  int out = 0; 

  while (out != 1) {
    Term t1 = Deref(ARG1);
    Term t2 = Deref(ARG2);
    CELL *oldH = H;

    if (t1 == t2)
      return Yap_unify(ARG3,t1);
    if (IsPairTerm(t1) && IsPairTerm(t2)) {
      Term tf = AbsAppl(H);
      H += 2;
      HB = H;
      if ((out = term_subsumer_complex(RepPair(t1)-1,
				       RepPair(t1)+1,
				       RepPair(t2)-1, H-2 PASS_REGS)) > 0) {
	HB = B->cp_h;
	return Yap_unify(ARG3,tf);
      }
  } else if (IsApplTerm(t1) && IsApplTerm(t2)) {
      Functor f1;

      if ((f1 = FunctorOfTerm(t1)) == FunctorOfTerm(t2)) {
	if (IsExtensionFunctor(f1)) {
	  if (unify_extension(f1, t1, RepAppl(t1), t2)) {
	    return Yap_unify(ARG3,t1);
	  }
	} else {
	  Term tf = AbsAppl(H);
	  UInt ar = ArityOfFunctor(f1);
	  H[0] = (CELL)f1;
	  H += 1+ar;
	  HB = H;
	  if ((out = term_subsumer_complex(RepAppl(t1),
					   RepAppl(t1)+ArityOfFunctor(f1),
					   RepAppl(t2), H-ar PASS_REGS)) > 0) {
	    HB = B->cp_h;
	    return Yap_unify(ARG3,tf);
	  }
	}
      }
    }
    HB = B->cp_h;
    if (out == 0) {
      return Yap_unify(ARG3, MkVarTerm());
    } else {
      H = oldH;
      if (out == -1) {
	if (!Yap_gcl((ASP-H)*sizeof(CELL), 0, ENV, gc_P(P,CP))) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, "in term_subsumer");
	  return FALSE;
	}
      } else {
	/* Trail overflow */
	if (!Yap_growtrail(0, FALSE)) {
	  Yap_Error(OUT_OF_TRAIL_ERROR, TermNil, "in term_subsumer");
	  return FALSE;
	} 
      }
    }
  }
  return FALSE;
}

#ifdef DEBUG
static Int
p_force_trail_expansion( USES_REGS1 )
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
camacho_dum( USES_REGS1 )
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
  Term *tailp;
  Yap_SkipList(&t, &tailp);
  return *tailp == TermNil;
}

static Int
p_is_list( USES_REGS1 )
{
  return Yap_IsListTerm(Deref(ARG1));
}

int
Yap_IsListOrPartialListTerm(Term t)
{
  Term *tailp, tail;
  Yap_SkipList(&t, &tailp);
  tail = *tailp;
  return tail == TermNil || IsVarTerm(tail);
}

static Int
p_is_list_or_partial_list( USES_REGS1 )
{
  return Yap_IsListOrPartialListTerm(Deref(ARG1));
}

static Term
numbervar(Int id USES_REGS)
{
  Term ts[1];
  ts[0] = MkIntegerTerm(id);
  return Yap_MkApplTerm(LOCAL_FunctorVar, 1, ts);
}

static Term
numbervar_singleton(USES_REGS1)
{
  Term ts[1];
  ts[0] = MkIntegerTerm(-1);
  return Yap_MkApplTerm(LOCAL_FunctorVar, 1, ts);
}

static void
renumbervar(Term t, Int id)
{
  Term *ts = RepAppl(t);
  ts[1] = MkIntegerTerm(id);
}


static Int numbervars_in_complex_term(register CELL *pt0, register CELL *pt0_end, Int numbv, int singles USES_REGS)
{

  register CELL **to_visit0, **to_visit = (CELL **)Yap_PreAllocCodeSpace();
  register tr_fr_ptr TR0 = TR;
  CELL *InitialH = H;

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
	if (singles && ap2 >= InitialH && ap2 < H) {
	  renumbervar(d0, numbv++);
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
    if (singles) 
      *ptd0 = numbervar_singleton( PASS_REGS1 );
    else
      *ptd0 = numbervar(numbv++ PASS_REGS);
    /* leave an empty slot to fill in later */
    if (H+1024 > ASP) {
      goto global_overflow;
    }
    /* next make sure noone will see this as a variable again */ 
    if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
      /* Trail overflow */
      if (!Yap_growtrail((TR-TR0)*sizeof(tr_fr_ptr *), TRUE)) {
	goto trail_overflow;
      }
    }

#if defined(TABLING) || defined(YAPOR_SBA)
    TrailVal(TR) = (CELL)ptd0;
#endif
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

  prune(B);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  return numbv;

 trail_overflow:
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  LOCAL_Error_TYPE = OUT_OF_TRAIL_ERROR;
  LOCAL_Error_Size = (TR-TR0)*sizeof(tr_fr_ptr *);
  clean_tr(TR0 PASS_REGS);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  return -1;
  
 aux_overflow:
  LOCAL_Error_Size = (to_visit-to_visit0)*sizeof(CELL **);
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  LOCAL_Error_TYPE = OUT_OF_AUXSPACE_ERROR;
  clean_tr(TR0 PASS_REGS);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  return -1;
  
 global_overflow:
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  clean_tr(TR0 PASS_REGS);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit0);
  H = InitialH;
  LOCAL_Error_TYPE = OUT_OF_STACK_ERROR;
  LOCAL_Error_Size = (ASP-H)*sizeof(CELL);
  return -1;
  
}

Int 
Yap_NumberVars( Term inp, Int numbv, int handle_singles )	/* numbervariables in term t	 */
{
  CACHE_REGS
  Int out;
  Term t;
  
 restart:
  t = Deref(inp);
  if (IsVarTerm(t)) {
    CELL *ptd0 = VarOfTerm(t);
    TrailTerm(TR++) = (CELL)ptd0;
    if (handle_singles) {
      *ptd0 = numbervar_singleton( PASS_REGS1 );
      return numbv;
    } else {
      *ptd0 = numbervar(numbv PASS_REGS);
      return numbv+1;
    }
  }  else if (IsPrimitiveTerm(t)) {
    return numbv;
  } else if (IsPairTerm(t)) {
    out = numbervars_in_complex_term(RepPair(t)-1,
				     RepPair(t)+1, numbv, handle_singles PASS_REGS);
  } else {
    Functor f = FunctorOfTerm(t);

    out = numbervars_in_complex_term(RepAppl(t),
			       RepAppl(t)+
				     ArityOfFunctor(f), numbv, handle_singles PASS_REGS);
  }
  if (out < 0) {
    if (!expand_vts( 3 PASS_REGS ))
      return FALSE;
    goto restart;
  }
  return out;
}

static Int
p_numbervars( USES_REGS1 )
{
  Term t2 = Deref(ARG2);
  Int out;

  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR,t2,"numbervars/3");
    return FALSE;
  }
  if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER,t2,"term_hash/4");
    return(FALSE);
  }
  if ((out = Yap_NumberVars(ARG1, IntegerOfTerm(t2), FALSE)) < 0)
    return FALSE;
  return Yap_unify(ARG3, MkIntegerTerm(out));
}

static int
unnumber_complex_term(CELL *pt0, CELL *pt0_end, CELL *ptf, CELL *HLow, int share USES_REGS)
{

  struct cp_frame *to_visit0, *to_visit = (struct cp_frame *)Yap_PreAllocCodeSpace();
  CELL *HB0 = HB;
  tr_fr_ptr TR0 = TR;
  int ground = share;
  Int max = -1;

  HB = HLow;
  to_visit0 = to_visit;
 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++ pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, unnumber_term_unk);
  unnumber_term_nvar:
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
	ground = share;
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
	if (ap2 >= HB && ap2 <= H) {
	  /* If this is newer than the current term, just reuse */
	  *ptf++ = d0;
	  continue;
	} 
	f = (Functor)(*ap2);

	if (IsExtensionFunctor(f)) {
	  *ptf++ = d0;  /* you can just unnumber other extensions. */
	  continue;
	}
	if (f == FunctorVar) {
	  Int id = IntegerOfTerm(ap2[1]);
	  ground = FALSE;
	  if (id < -1) {
	    Yap_Error(OUT_OF_STACK_ERROR, TermNil, "unnumber vars cannot cope with VAR(-%d)", id);
	    return 0L;
	  }
	  if (id <= max) {
	    if (ASP-(max+1) <= H) {
	      goto overflow;
	    }
	    /* we found this before? */
	    if (ASP[-id-1])
	      *ptf++ = ASP[-id-1];
	    else {
	      RESET_VARIABLE(ptf);
	      ASP[-id-1] = (CELL)ptf;
	      ptf++;
	    }
	    continue;
	  }
	  /* alloc more space */
	  if (ASP-(id+1) <= H) {
	    goto overflow;
	  }
	  while (id > max) {
	    ASP[-(id+1)] = 0L;
	    max++;
	  }
	  /* new variable */
	  RESET_VARIABLE(ptf);
	  ASP[-(id+1)] = (CELL)ptf;
	  ptf++;
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
	ground = (f != FunctorMutable) && share;
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
	/* just unnumber atoms or integers */
	*ptf++ = d0;
      }
      continue;
    }

    derefa_body(d0, ptd0, unnumber_term_unk, unnumber_term_nvar);
    /* this should never happen ? */
    ground = FALSE;
    *ptf++ = (CELL) ptd0;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit --;
    if (ground) {
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
  clean_dirty_tr(TR0 PASS_REGS);
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
  return -1;

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
  LOCAL_Error_Size = (ADDR)AuxSp-(ADDR)to_visit0;
  return -3;
}


static Term
UnnumberTerm(Term inp, UInt arity, int share USES_REGS) {
  Term t = Deref(inp);
  tr_fr_ptr TR0 = TR;

  if (IsVarTerm(t)) {
    return inp;
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
      if ((res = unnumber_complex_term(ap-1, ap+1, Hi, Hi, share PASS_REGS)) < 0) {
	H = Hi;
	if ((t = handle_cp_overflow(res, TR0, arity, t))== 0L)
	  return FALSE;
	goto restart_list;
      } else if (res) {
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

      if ((res = unnumber_complex_term(ap, ap+ArityOfFunctor(f), HB0+1, HB0, share PASS_REGS)) < 0) {
	H = HB0;
	if ((t = handle_cp_overflow(res, TR0, arity, t))== 0L)
	  return FALSE;
	goto restart_appl;
      } else if (res && FunctorOfTerm(t) != FunctorMutable) {
	H = HB0;
	return t;
      }
    }
    return tf;
  }
}
 
Term
Yap_UnNumberTerm(Term inp, int share) {
  CACHE_REGS
  return UnnumberTerm(inp, 0, share PASS_REGS);
}

static Int
p_unnumbervars( USES_REGS1 ) {
  /* this should be a standard Prolog term, so we allow sharing? */
  return Yap_unify(UnnumberTerm(ARG1, 2, FALSE PASS_REGS), ARG2);
}

Int
Yap_SkipList(Term *l, Term **tailp)
{
  Int length = 0;
  Term *s; /* slow */
  Term v; /* temporary */

  do_derefa(v,l,derefa_unk,derefa_nonvar);
  s = l;

  if ( IsPairTerm(*l) )
  { intptr_t power = 1, lam = 0;
    do
    { if ( power == lam )
      { s = l;
	power *= 2;
	lam = 0;
      }
      lam++;
      length++;
      l = RepPair(*l)+1; 
      do_derefa(v,l,derefa2_unk,derefa2_nonvar);
    } while ( *l != *s && IsPairTerm(*l) );
  }
  *tailp = l;

  return length;
}

static Int 
p_skip_list( USES_REGS1 ) {
  Term *tail;
  Int len = Yap_SkipList(XREGS+2, &tail);

  return Yap_unify(MkIntegerTerm(len), ARG1) &&
    Yap_unify(*tail, ARG3);
}

static Int 
p_skip_list4( USES_REGS1 ) {
  Term *tail;
  Int len = Yap_SkipList(XREGS+1, &tail);
  Term t2 = Deref(ARG2), t = *tail;

  if (!IsVarTerm(t2)) {
    Int len1;
    if (!IsIntegerTerm(t2)) {
      Yap_Error(TYPE_ERROR_INTEGER, t2, "length/2");
      return FALSE;
    }
    if ((len1 = IntegerOfTerm(t2)) < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t2, "length/2");
      return FALSE;
    }
    if (t == TermNil)
      return
	len1 == len &&
	Yap_unify(*tail, ARG4);
  }
  /* don't set M0 if full list, just check M */
  if (t == TermNil) {
    return Yap_unify_constant(ARG4, TermNil) &&
      Yap_unify_constant(ARG2, MkIntegerTerm(len));
  }
  return Yap_unify(MkIntegerTerm(len), ARG3) &&
    Yap_unify(t, ARG4);
}

void Yap_InitUtilCPreds(void)
{
  CACHE_REGS
  Term cm = CurrentModule;
  Yap_InitCPred("copy_term", 2, p_copy_term, 0);
  Yap_InitCPred("duplicate_term", 2, p_duplicate_term, 0);
  Yap_InitCPred("copy_term_nat", 2, p_copy_term_no_delays, 0);
  Yap_InitCPred("ground", 1, p_ground, SafePredFlag);
  Yap_InitCPred("acyclic_term", 1, p_acyclic_term, SafePredFlag);
  Yap_InitCPred("cyclic_term", 1, p_cyclic_term, SafePredFlag);
  Yap_InitCPred("$variables_in_term", 3, p_variables_in_term, HiddenPredFlag);
  Yap_InitCPred("$non_singletons_in_term", 3, p_non_singletons_in_term, HiddenPredFlag);
  Yap_InitCPred("term_variables", 2, p_term_variables, 0);
  Yap_InitCPred("term_variables", 3, p_term_variables3, 0);
  Yap_InitCPred("term_attvars", 2, p_term_attvars, 0);
  Yap_InitCPred("is_list", 1, p_is_list, SafePredFlag|TestPredFlag);
  Yap_InitCPred("$is_list_or_partial_list", 1, p_is_list_or_partial_list, SafePredFlag|TestPredFlag);
  Yap_InitCPred("rational_term_to_tree", 2, p_break_rational, 0);
  Yap_InitCPred("tree_to_rational_term", 2, p_restore_rational, 0);
  Yap_InitCPred("=@=", 2, p_variant, 0);
  Yap_InitCPred("numbervars", 3, p_numbervars, 0);
  Yap_InitCPred("unnumbervars", 2, p_unnumbervars, 0);
  /* use this carefully */
  Yap_InitCPred("$skip_list", 3, p_skip_list, SafePredFlag|TestPredFlag);
  Yap_InitCPred("$skip_list", 4, p_skip_list4, SafePredFlag|TestPredFlag);
  CurrentModule = TERMS_MODULE;
  Yap_InitCPred("variable_in_term", 2, p_var_in_term, 0);
  Yap_InitCPred("term_hash", 4, p_term_hash, 0);
  Yap_InitCPred("instantiated_term_hash", 4, p_instantiated_term_hash, 0);
  Yap_InitCPred("variant", 2, p_variant, 0);
  Yap_InitCPred("subsumes", 2, p_subsumes, 0);
  Yap_InitCPred("term_subsumer", 3, p_term_subsumer, 0);
  Yap_InitCPred("variables_within_term", 3, p_variables_within_term, 0);
  Yap_InitCPred("new_variables_in_term", 3, p_new_variables_in_term, 0);
  Yap_InitCPred("export_term", 3, p_export_term, 0);
  Yap_InitCPred("kill_exported_term", 1, p_kill_exported_term, SafePredFlag);
  Yap_InitCPred("import_term", 2, p_import_term, 0);
  CurrentModule = cm;
#ifdef DEBUG
  Yap_InitCPred("$force_trail_expansion", 1, p_force_trail_expansion, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("dum", 1, camacho_dum, SafePredFlag);
#endif
}

