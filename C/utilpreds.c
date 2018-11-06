/*************************************************************************
 *									 *
 *	 YAP Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *
 **************************************************************************
 *									 *
 * File:		utilpreds.c * Last rev:	4/03/88
 ** mods: * comments:	new utility predicates for YAP *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "@(#)utilpreds.c	1.3";
#endif
/**
 * @addtogroup Terms

 */
#include "absmi.h"

#include "YapHeap.h"
#include "attvar.h"
#include "yapio.h"
#ifdef HAVE_STRING_H
#include "string.h"
#endif

typedef struct {
  Term old_var;
  Term new_var;
} * vcell;

static int copy_complex_term(CELL *, CELL *, int, int, CELL *,
                             CELL *CACHE_TYPE);
static CELL vars_in_complex_term(CELL *, CELL *, Term CACHE_TYPE);
static Int p_non_singletons_in_term(USES_REGS1);
static CELL non_singletons_in_complex_term(CELL *, CELL *CACHE_TYPE);
static Int p_variables_in_term(USES_REGS1);
static Int ground_complex_term(CELL *, CELL *CACHE_TYPE);
static Int p_ground(USES_REGS1);
static Int p_copy_term(USES_REGS1);
static Int var_in_complex_term(CELL *, CELL *, Term CACHE_TYPE);

#ifdef DEBUG
static Int p_force_trail_expansion(USES_REGS1);
#endif /* DEBUG */

static inline void clean_tr(tr_fr_ptr TR0 USES_REGS) {
  if (TR != TR0) {
    do {
      Term p = TrailTerm(--TR);
      RESET_VARIABLE(p);
    } while (TR != TR0);
  }
}

static inline void clean_dirty_tr(tr_fr_ptr TR0 USES_REGS) {
  if (TR != TR0) {
    tr_fr_ptr pt = TR0;

    do {
      Term p = TrailTerm(pt++);
      RESET_VARIABLE(p);
    } while (pt != TR);
    TR = TR0;
  }
}

static int copy_complex_term(CELL *pt0, CELL *pt0_end, int share, int newattvs,
                             CELL *ptf, CELL *HLow USES_REGS) {

  struct cp_frame *to_visit0,
      *to_visit = (struct cp_frame *)Yap_PreAllocCodeSpace();
  CELL *HB0 = HB;
  tr_fr_ptr TR0 = TR;
  int ground = TRUE;

  HB = HR;
  to_visit0 = to_visit;
loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, copy_term_unk);
  copy_term_nvar : {
    if (IsPairTerm(d0)) {
      CELL *ap2 = RepPair(d0);
      if (ap2 >= HB && ap2 < HR) {
        /* If this is newer than the current term, just reuse */
        *ptf++ = d0;
        continue;
      }
      *ptf = AbsPair(HR);
      ptf++;
#ifdef RATIONAL_TREES
      if (to_visit + 1 >= (struct cp_frame *)AuxSp) {
        goto heap_overflow;
      }
      to_visit->start_cp = pt0;
      to_visit->end_cp = pt0_end;
      to_visit->to = ptf;
      to_visit->oldv = *pt0;
      to_visit->ground = ground;
      /* fool the system into thinking we had a variable there */
      *pt0 = AbsPair(HR);
      to_visit++;
#else
      if (pt0 < pt0_end) {
        if (to_visit + 1 >= (struct cp_frame *)AuxSp) {
          goto heap_overflow;
        }
        to_visit->start_cp = pt0;
        to_visit->end_cp = pt0_end;
        to_visit->to = ptf;
        to_visit->ground = ground;
        to_visit++;
      }
#endif
      ground = TRUE;
      pt0 = ap2 - 1;
      pt0_end = ap2 + 1;
      ptf = HR;
      HR += 2;
      if (HR > ASP - 2048) {
        goto overflow;
      }
    } else if (IsApplTerm(d0)) {
      register Functor f;
      register CELL *ap2;
      /* store the terms to visit */
      ap2 = RepAppl(d0);
      if (ap2 >= HB && ap2 <= HR) {
        /* If this is newer than the current term, just reuse */
        *ptf++ = d0;
        continue;
      }
      f = (Functor)(*ap2);

      if (IsExtensionFunctor(f)) {
#if MULTIPLE_STACKS
        if (f == FunctorDBRef) {
          DBRef entryref = DBRefOfTerm(d0);
          if (entryref->Flags & LogUpdMask) {
            LogUpdClause *luclause = (LogUpdClause *)entryref;
            PELOCK(100, luclause->ClPred);
            UNLOCK(luclause->ClPred->PELock);
          } else {
            LOCK(entryref->lock);
            TRAIL_REF(entryref); /* So that fail will erase it */
            INC_DBREF_COUNT(entryref);
            UNLOCK(entryref->lock);
          }
          *ptf++ = d0; /* you can just copy other extensions. */
        } else
#endif
            if (!share) {
          UInt sz;

          *ptf++ = AbsAppl(HR); /* you can just copy other extensions. */
          /* make sure to copy floats */
          if (f == FunctorDouble) {
            sz = sizeof(Float) / sizeof(CELL) + 2;
          } else if (f == FunctorLongInt) {
            sz = 3;
          } else if (f == FunctorString) {
            sz = 3 + ap2[1];
          } else {
            CELL *pt = ap2 + 1;
            sz = 2 + sizeof(MP_INT) +
                 (((MP_INT *)(pt + 1))->_mp_alloc * sizeof(mp_limb_t));
          }
          if (HR + sz > ASP - 2048) {
            goto overflow;
          }
          memmove((void *)HR, (void *)ap2, sz * sizeof(CELL));
          HR += sz;
        } else {
          *ptf++ = d0; /* you can just copy other extensions. */
        }
        continue;
      }
      *ptf = AbsAppl(HR);
      ptf++;
      /* store the terms to visit */
#ifdef RATIONAL_TREES
      if (to_visit + 1 >= (struct cp_frame *)AuxSp) {
        goto heap_overflow;
      }
      to_visit->start_cp = pt0;
      to_visit->end_cp = pt0_end;
      to_visit->to = ptf;
      to_visit->oldv = *pt0;
      to_visit->ground = ground;
      /* fool the system into thinking we had a variable there */
      *pt0 = AbsAppl(HR);
      to_visit++;
#else
      if (pt0 < pt0_end) {
        if (to_visit + 1 >= (struct cp_frame *)AuxSp) {
          goto heap_overflow;
        }
        to_visit->start_cp = pt0;
        to_visit->end_cp = pt0_end;
        to_visit->to = ptf;
        to_visit->ground = ground;
        to_visit++;
      }
#endif
      ground = (f != FunctorMutable);
      d0 = ArityOfFunctor(f);
      pt0 = ap2;
      pt0_end = ap2 + d0;
      /* store the functor for the new term */
      HR[0] = (CELL)f;
      ptf = HR + 1;
      HR += 1 + d0;
      if (HR > ASP - 2048) {
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
    if (ptd0 >= HLow && ptd0 < HR) {
      /* we have already found this cell */
      *ptf++ = (CELL)ptd0;
    } else
#if COROUTINING
        if (newattvs && IsAttachedTerm((CELL)ptd0)) {
      /* if unbound, call the standard copy term routine */
      struct cp_frame *bp;

      CELL new;

      bp = to_visit;
      if (!GLOBAL_attas[ExtFromCell(ptd0)].copy_term_op(ptd0, &bp,
                                                        ptf PASS_REGS)) {
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
        if (!Yap_growtrail((TR - TR0) * sizeof(tr_fr_ptr *), TRUE)) {
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
    to_visit--;
    if (ground && share) {
      CELL old = to_visit->oldv;
      CELL *newp = to_visit->to - 1;
      CELL new = *newp;

      *newp = old;
      if (IsApplTerm(new))
        HR = RepAppl(new);
      else
        HR = RepPair(new);
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
  HR = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit--;
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
  HR = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit--;
    pt0 = to_visit->start_cp;
    pt0_end = to_visit->end_cp;
    ptf = to_visit->to;
    *pt0 = to_visit->oldv;
  }
#endif
  {
    tr_fr_ptr oTR = TR;
    reset_trail(TR0);
    if (!Yap_growtrail((oTR - TR0) * sizeof(tr_fr_ptr *), TRUE)) {
      return -4;
    }
    return -2;
  }

heap_overflow:
  /* oops, we're in trouble */
  HR = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit--;
    pt0 = to_visit->start_cp;
    pt0_end = to_visit->end_cp;
    ptf = to_visit->to;
    *pt0 = to_visit->oldv;
  }
#endif
  reset_trail(TR0);
  LOCAL_Error_Size = (ADDR)AuxSp - (ADDR)to_visit0;
  return -3;
}

static Term handle_cp_overflow(int res, tr_fr_ptr TR0, UInt arity, Term t) {
  CACHE_REGS
  XREGS[arity + 1] = t;
  switch (res) {
  case -1:
    if (!Yap_gcl((ASP - HR) * sizeof(CELL), arity + 1, ENV, gc_P(P, CP))) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
      return 0L;
    }
    return Deref(XREGS[arity + 1]);
  case -2:
    return Deref(XREGS[arity + 1]);
  case -3: {
    UInt size = LOCAL_Error_Size;
    LOCAL_Error_Size = 0L;
    if (size > 4 * 1024 * 1024)
      size = 4 * 1024 * 1024;
    if (!Yap_ExpandPreAllocCodeSpace(size, NULL, TRUE)) {
      Yap_Error(RESOURCE_ERROR_AUXILIARY_STACK, TermNil, LOCAL_ErrorMessage);
      return 0L;
    }
  }
    return Deref(XREGS[arity + 1]);
  case -4:
    if (!Yap_growtrail((TR - TR0) * sizeof(tr_fr_ptr *), FALSE)) {
      Yap_Error(RESOURCE_ERROR_TRAIL, TermNil, LOCAL_ErrorMessage);
      return 0L;
    }
    return Deref(XREGS[arity + 1]);
  default:
    return 0L;
  }
}

static Term CopyTerm(Term inp, UInt arity, int share, int newattvs USES_REGS) {
  Term t = Deref(inp);
  tr_fr_ptr TR0 = TR;

  if (IsVarTerm(t)) {
#if COROUTINING
    if (newattvs && IsAttachedTerm(t)) {
      CELL *Hi;
      int res;
    restart_attached:

      *HR = t;
      Hi = HR + 1;
      HR += 2;
      if ((res = copy_complex_term(Hi - 2, Hi - 1, share, newattvs, Hi,
                                   Hi PASS_REGS)) < 0) {
        HR = Hi - 1;
        if ((t = handle_cp_overflow(res, TR0, arity, t)) == 0L)
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
    Hi = HR;
    tf = AbsPair(HR);
    HR += 2;
    {
      int res;
      if ((res = copy_complex_term(ap - 1, ap + 1, share, newattvs, Hi,
                                   Hi PASS_REGS)) < 0) {
        HR = Hi;
        if ((t = handle_cp_overflow(res, TR0, arity, t)) == 0L)
          return FALSE;
        goto restart_list;
      } else if (res && share) {
        HR = Hi;
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
    HB0 = HR;
    ap = RepAppl(t);
    tf = AbsAppl(HR);
    HR[0] = (CELL)f;
    HR += 1 + ArityOfFunctor(f);
    if (HR > ASP - 128) {
      HR = HB0;
      if ((t = handle_cp_overflow(-1, TR0, arity, t)) == 0L)
        return FALSE;
      goto restart_appl;
    } else {
      int res;

      if ((res = copy_complex_term(ap, ap + ArityOfFunctor(f), share, newattvs,
                                   HB0 + 1, HB0 PASS_REGS)) < 0) {
        HR = HB0;
        if ((t = handle_cp_overflow(res, TR0, arity, t)) == 0L)
          return FALSE;
        goto restart_appl;
      } else if (res && share && FunctorOfTerm(t) != FunctorMutable) {
        HR = HB0;
        return t;
      }
    }
    return tf;
  }
}

Term Yap_CopyTerm(Term inp) {
  CACHE_REGS
  return CopyTerm(inp, 0, TRUE, TRUE PASS_REGS);
}

Term Yap_CopyTermNoShare(Term inp) {
  CACHE_REGS
  return CopyTerm(inp, 0, FALSE, FALSE PASS_REGS);
}

static Int p_copy_term(USES_REGS1) /* copy term t to a new instance  */
{
  Term t = CopyTerm(ARG1, 2, TRUE, TRUE PASS_REGS);
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  return Yap_unify(ARG2, t);
}

static Int p_duplicate_term(USES_REGS1) /* copy term t to a new instance  */
{
  Term t = CopyTerm(ARG1, 2, FALSE, TRUE PASS_REGS);
  if (t == 0L)
    return FALSE;
  /* be careful, there may be a stack shift here */
  return Yap_unify(ARG2, t);
}

static Int
p_copy_term_no_delays(USES_REGS1) /* copy term t to a new instance  */
{
  Term t = CopyTerm(ARG1, 2, TRUE, FALSE PASS_REGS);
  if (t == 0L) {
    return FALSE;
  }
  /* be careful, there may be a stack shift here */
  return (Yap_unify(ARG2, t));
}

typedef struct bp_frame {
  CELL *start_cp;
  CELL *end_cp;
  CELL *to;
  CELL *oldp;
  CELL oldv;
} bp_frame_t;

typedef struct copy_frame {
  CELL *start_cp;
  CELL *end_cp;
  CELL *to;
} copy_frame_t;

static Term add_to_list(Term inp, Term v, Term t PASS_REGS) {
  Term ta[2];

  ta[0] = v;
  ta[1] = t;
  return MkPairTerm(Yap_MkApplTerm(FunctorEq, 2, ta), inp);
}

static int break_rationals_complex_term(CELL *pt0, CELL *pt0_end, CELL *ptf,
                                        Term *vout, Term vin,
                                        CELL *HLow USES_REGS) {

  struct bp_frame *to_visit0,
      *to_visit = (struct bp_frame *)Yap_PreAllocCodeSpace();
  CELL *HB0 = HB;
  tr_fr_ptr TR0 = TR;

  HB = HR;
  to_visit0 = to_visit;
loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    register CELL *ptd0;
    ++pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, copy_term_unk);
  copy_term_nvar : {
    if (IsPairTerm(d0)) {
      CELL *ap2 = RepPair(d0);
      fprintf(stderr, "%ld \n", RepPair(ap2[0]) - ptf);
      if (IsVarTerm(ap2[0]) && IN_BETWEEN(HB, (ap2[0]), HR)) {
        Term v = MkVarTerm();
        *ptf = v;
        vin = add_to_list(vin, (CELL)(ptf), AbsPair(ptf));
        ptf++;
        continue;
      }
      if (to_visit + 1 >= (struct bp_frame *)AuxSp) {
        goto heap_overflow;
      }
      *ptf++ = (CELL)(HR);
      to_visit->start_cp = pt0;
      to_visit->end_cp = pt0_end;
      to_visit->to = ptf;
      to_visit->oldp = ap2;
      d0 = to_visit->oldv = ap2[0];
      /* fool the system into thinking we had a variable there */
      to_visit++;
      pt0 = ap2;
      pt0_end = ap2 + 1;
      ptf = HR;
      *ap2 = AbsPair(HR);
      HR += 2;
      if (HR > ASP - 2048) {
        goto overflow;
      }
      if (IsVarTerm(d0) && d0 == (CELL)ap2) {
        RESET_VARIABLE(ptf);
        ptf++;
        continue;
      }
      d0 = Deref(d0);
      if (!IsVarTerm(d0)) {
        goto copy_term_nvar;
      } else {
        *ptf++ = d0;
      }
      continue;
    } else if (IsApplTerm(d0)) {
      register Functor f;
      register CELL *ap2;
      /* store the terms to visit */
      ap2 = RepAppl(d0) + 1;
      f = (Functor)(ap2[-1]);
      if (IsExtensionFunctor(f)) {
        *ptf++ = d0; /* you can just copy other extensions. */
        continue;
      }
      if (IsApplTerm(ap2[0]) && IN_BETWEEN(HB, RepAppl(ap2[0]), HR)) {
        RESET_VARIABLE(ptf);
        vin = add_to_list(vin, (CELL)ptf, ap2[0]);
        ptf++;
        continue;
      }

      arity_t arity = ArityOfFunctor(f);
      if (to_visit + 1 >= (struct bp_frame *)AuxSp) {
        goto heap_overflow;
      }
      *ptf++ = AbsAppl(HR);
      to_visit->start_cp = pt0;
      to_visit->end_cp = pt0_end;
      to_visit->to = ptf;
      to_visit->oldp = ap2;
      d0 = to_visit->oldv = ap2[0];
      /* fool the system into thinking we had a variable there */
      to_visit++;
      pt0 = ap2;
      pt0_end = ap2 + (arity - 1);
      ptf = HR;
      if (HR > ASP - 2048) {
        goto overflow;
      }
      *ptf++ = (CELL)f;
      *ap2 = AbsAppl(HR);
      HR += (arity + 1);
      if (IsVarTerm(d0) && d0 == (CELL)(ap2)) {
        RESET_VARIABLE(ptf);
        ptf++;
        continue;
      }
      d0 = Deref(d0);
      if (!IsVarTerm(d0)) {
        goto copy_term_nvar;
      } else {
        *ptf++ = d0;
      }
      continue;
    } else {
      /* just copy atoms or integers */
      *ptf++ = d0;
    }
    continue;
  }

    derefa_body(d0, ptd0, copy_term_unk, copy_term_nvar);
    *ptf++ = (CELL)ptd0;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit--;
    *to_visit->oldp = to_visit->oldv;
    ptf = to_visit->to;
    pt0 = to_visit->start_cp;
    pt0_end = to_visit->end_cp;
    goto loop;
  }

  /* restore our nice, friendly, term to its original state */
  HB = HB0;
  *vout = vin;
  return true;

overflow:
  /* oops, we're in trouble */
  HR = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit--;
    pt0 = to_visit->start_cp;
    pt0_end = to_visit->end_cp;
    ptf = to_visit->to;
    *to_visit->oldp = to_visit->oldv;
  }
#endif
  reset_trail(TR0);
  /* follow chain of multi-assigned variables */
  return -1;

heap_overflow:
  /* oops, we're in trouble */
  HR = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit--;
    pt0 = to_visit->start_cp;
    pt0_end = to_visit->end_cp;
    ptf = to_visit->to;
    *to_visit->oldp = to_visit->oldv;
  }
#endif
  reset_trail(TR0);
  LOCAL_Error_Size = (ADDR)AuxSp - (ADDR)to_visit0;
  return -3;
}

Term Yap_BreakRational(Term inp, UInt arity, Term *to, Term ti USES_REGS) {
  Term t = Deref(inp);
  Term tii = ti;
  tr_fr_ptr TR0 = TR;

  if (IsVarTerm(t)) {
    *to = ti;
    return t;
  } else if (IsPrimitiveTerm(t)) {
    *to = ti;
    return t;
  } else if (IsPairTerm(t)) {
    CELL *ap;
    CELL *Hi;

  restart_list:
    ap = RepPair(t);
    Hi = HR;
    HR += 2;
    {
      Int res;
      if ((res = break_rationals_complex_term(ap - 1, ap + 1, Hi, to, ti,
                                              Hi PASS_REGS)) < 0) {
        HR = Hi;
        if ((t = handle_cp_overflow(res, TR0, arity, t)) == 0L)
          return FALSE;
        goto restart_list;
      } else if (*to == tii) {
        HR = Hi;
        return t;
      } else {
        return AbsPair(Hi);
      }
    }
  } else {
    Functor f;
    CELL *HB0;
    CELL *ap;

  restart_appl:
    f = FunctorOfTerm(t);
    if (IsExtensionFunctor(f)) {
      *to = ti;
      return t;
    }
    HB0 = HR;
    ap = RepAppl(t);
    HR[0] = (CELL)f;
    arity = ArityOfFunctor(f);
    HR += 1+arity;
    continue;
  }

    derefa_body(d0, ptd0, vars_in_term_unk, vars_in_term_nvar);
    /* do or pt2 are unbound  */
    if (singles)
      *ptd0 = numbervar_singleton(PASS_REGS1);
    else
      *ptd0 = numbervar(numbv++ PASS_REGS);
    /* leave an empty slot to fill in later */
    if (HR + 1024 > ASP) {
      goto global_overflow;
    }
    /* next make sure noone will see this as a variable again */
    if (TR > (tr_fr_ptr)LOCAL_TrailTop - 256) {
      /* Trail overflow */
      if (!Yap_growtrail((TR - TR0) * sizeof(tr_fr_ptr *), TRUE)) {
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
    to_visit--;
    pt0 = to_visit->beg;
    pt0_end = to_visit->end;
    *pt0 = to_visit->oval;
#else
    to_visit -= 2;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
#endif
    goto loop;
  }

  prune(B PASS_REGS);
  pop_text_stack(lvl);
  return numbv;

trail_overflow:
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit--;
    pt0 = to_visit->beg;
    pt0_end = to_visit->end;
    *pt0 = to_visit->oval;
  }
#endif
  LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;
  LOCAL_Error_Size = (TR - TR0) * sizeof(tr_fr_ptr *);
  clean_tr(TR0 PASS_REGS);
  HR = InitialH;
  pop_text_stack(lvl);
  return numbv - 1;

aux_overflow : {
  size_t d1 = to_visit - to_visit0;
  size_t d2 = to_visit_max - to_visit0;
  to_visit0 = Realloc(to_visit0, d2 * sizeof(CELL *) + 64 * 1024);
  to_visit = to_visit0 + d1;
  to_visit_max = to_visit0 + (d2 + (64 * 1024)) / sizeof(CELL **);
}
  pt0--;
  goto loop;

global_overflow:
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit--;
    pt0 = to_visit->beg;
    pt0_end = to_visit->end;
    *pt0 = to_visit->oval;
  }
#endif
  clean_tr(TR0 PASS_REGS);
  HR = InitialH;
  LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;
  LOCAL_Error_Size = (ASP - HR) * sizeof(CELL);
  pop_text_stack(lvl);
  return numbv - 1;
}

Int Yap_NumberVars(Term inp, Int numbv,
                   bool handle_singles) /*
                                         * numbervariables in term t	 */
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
      *ptd0 = numbervar_singleton(PASS_REGS1);
      return numbv;
    } else {
      *ptd0 = numbervar(numbv PASS_REGS);
      return numbv + 1;
    }
  } else if (IsPrimitiveTerm(t)) {
    return numbv;
  } else if (IsPairTerm(t)) {
    out = numbervars_in_complex_term(RepPair(t) - 1, RepPair(t) + 1, numbv,
                                     handle_singles PASS_REGS);
  } else {
    Functor f = FunctorOfTerm(t);

    out = numbervars_in_complex_term(RepAppl(t), RepAppl(t) + ArityOfFunctor(f),
                                     numbv, handle_singles PASS_REGS);
  }
  if (out < numbv) {
    if (!expand_vts(3 PASS_REGS))
      return FALSE;
    goto restart;
  }
  return out;
}

static Int p_numbervars(USES_REGS1) {
  Term t2 = Deref(ARG2);
  Int out;

  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "numbervars/3");
    return FALSE;
  }
  if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "term_hash/4");
    return (FALSE);
  }
  if ((out = Yap_NumberVars(ARG1, IntegerOfTerm(t2), FALSE)) < 0)
    return FALSE;
  return Yap_unify(ARG3, MkIntegerTerm(out));
}

static int unnumber_complex_term(CELL *pt0, CELL *pt0_end, CELL *ptf,
                                 CELL *HLow, int share USES_REGS) {

  struct cp_frame *to_visit0,
      *to_visit = (struct cp_frame *)Yap_PreAllocCodeSpace();
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
    ++pt0;
    ptd0 = pt0;
    d0 = *ptd0;
    deref_head(d0, unnumber_term_unk);
  unnumber_term_nvar : {
    if (IsPairTerm(d0)) {
      CELL *ap2 = RepPair(d0);
      if (ap2 >= HB && ap2 < HR) {
        /* If this is newer than the current term, just reuse */
        *ptf++ = d0;
        continue;
      }
      *ptf = AbsPair(HR);
      ptf++;
#ifdef RATIONAL_TREES
      if (to_visit + 1 >= (struct cp_frame *)AuxSp) {
        goto heap_overflow;
      }
      to_visit->start_cp = pt0;
      to_visit->end_cp = pt0_end;
      to_visit->to = ptf;
      to_visit->oldv = *pt0;
      to_visit->ground = ground;
      /* fool the system into thinking we had a variable there */
      *pt0 = AbsPair(HR);
      to_visit++;
#else
      if (pt0 < pt0_end) {
        if (to_visit + 1 >= (struct cp_frame *)AuxSp) {
          goto heap_overflow;
        }
        to_visit->start_cp = pt0;
        to_visit->end_cp = pt0_end;
        to_visit->to = ptf;
        to_visit->ground = ground;
        to_visit++;
      }
#endif
      ground = share;
      pt0 = ap2 - 1;
      pt0_end = ap2 + 1;
      ptf = HR;
      HR += 2;
      if (HR > ASP - 2048) {
        goto overflow;
      }
    } else if (IsApplTerm(d0)) {
      register Functor f;
      register CELL *ap2;
      /* store the terms to visit */
      ap2 = RepAppl(d0);
      if (ap2 >= HB && ap2 <= HR) {
        /* If this is newer than the current term, just reuse */
        *ptf++ = d0;
        continue;
      }
      f = (Functor)(*ap2);

      if (IsExtensionFunctor(f)) {
        *ptf++ = d0; /* you can just unnumber other extensions. */
        continue;
      }
      if (f == FunctorDollarVar) {
        Int id = IntegerOfTerm(ap2[1]);
        ground = FALSE;
        if (id < -1) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil,
                    "unnumber vars cannot cope with VAR(-%d)", id);
          return 0L;
        }
        if (id <= max) {
          if (ASP - (max + 1) <= HR) {
            goto overflow;
          }
          /* we found this before? */
          if (ASP[-id - 1])
            *ptf++ = ASP[-id - 1];
          else {
            RESET_VARIABLE(ptf);
            ASP[-id - 1] = (CELL)ptf;
            ptf++;
          }
          continue;
        }
        /* alloc more space */
        if (ASP - (id + 1) <= HR) {
          goto overflow;
        }
        while (id > max) {
          ASP[-(id + 1)] = 0L;
          max++;
        }
        /* new variable */
        RESET_VARIABLE(ptf);
        ASP[-(id + 1)] = (CELL)ptf;
        ptf++;
        continue;
      }
      *ptf = AbsAppl(HR);
      ptf++;
      /* store the terms to visit */
#ifdef RATIONAL_TREES
      if (to_visit + 1 >= (struct cp_frame *)AuxSp) {
        goto heap_overflow;
      }
      to_visit->start_cp = pt0;
      to_visit->end_cp = pt0_end;
      to_visit->to = ptf;
      to_visit->oldv = *pt0;
      to_visit->ground = ground;
      /* fool the system into thinking we had a variable there */
      *pt0 = AbsAppl(HR);
      to_visit++;
#else
      if (pt0 < pt0_end) {
        if (to_visit + 1 >= (struct cp_frame *)AuxSp) {
          goto heap_overflow;
        }
        to_visit->start_cp = pt0;
        to_visit->end_cp = pt0_end;
        to_visit->to = ptf;
        to_visit->ground = ground;
        to_visit++;
      }
#endif
      ground = (f != FunctorMutable) && share;
      d0 = ArityOfFunctor(f);
      pt0 = ap2;
      pt0_end = ap2 + d0;
      /* store the functor for the new term */
      HR[0] = (CELL)f;
      ptf = HR + 1;
      HR += 1 + d0;
      if (HR > ASP - 2048) {
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
    *ptf++ = (CELL)ptd0;
  }
  /* Do we still have compound terms to visit */
  if (to_visit > to_visit0) {
    to_visit--;
    if (ground) {
      CELL old = to_visit->oldv;
      CELL *newp = to_visit->to - 1;
      CELL new = *newp;

      *newp = old;
      if (IsApplTerm(new))
        HR = RepAppl(new);
      else
        HR = RepPair(new);
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
  HR = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit--;
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
  HR = HLow;
  /* we've done it */
  /* restore our nice, friendly, term to its original state */
  HB = HB0;
#ifdef RATIONAL_TREES
  while (to_visit > to_visit0) {
    to_visit--;
    pt0 = to_visit->start_cp;
    pt0_end = to_visit->end_cp;
    ptf = to_visit->to;
    *pt0 = to_visit->oldv;
  }
#endif
  reset_trail(TR0);
  LOCAL_Error_Size = (ADDR)AuxSp - (ADDR)to_visit0;
  return -3;
}

static Term UnnumberTerm(Term inp, UInt arity, int share USES_REGS) {
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
    Hi = HR;
    tf = AbsPair(HR);
    HR += 2;
    {
      int res;
      if ((res = unnumber_complex_term(ap - 1, ap + 1, Hi, Hi,
                                       share PASS_REGS)) < 0) {
        HR = Hi;
        if ((t = handle_cp_overflow(res, TR0, arity, t)) == 0L)
          return FALSE;
        goto restart_list;
      } else if (res) {
        HR = Hi;
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
    HB0 = HR;
    ap = RepAppl(t);
    tf = AbsAppl(HR);
    HR[0] = (CELL)f;
    HR += 1 + ArityOfFunctor(f);
    if (HR > ASP - 128) {
      HR = HB0;
      if ((t = handle_cp_overflow(-1, TR0, arity, t)) == 0L)
        return FALSE;
      goto restart_appl;
    } else {
      int res;

      if ((res = unnumber_complex_term(ap, ap + ArityOfFunctor(f), HB0 + 1, HB0,
                                       share PASS_REGS)) < 0) {
        HR = HB0;
        if ((t = handle_cp_overflow(res, TR0, arity, t)) == 0L)
          return FALSE;
        goto restart_appl;
      } else if (res && FunctorOfTerm(t) != FunctorMutable) {
        HR = HB0;
        return t;
      }
    }
    return tf;
  }
}

Term Yap_UnNumberTerm(Term inp, int share) {
  CACHE_REGS
  return UnnumberTerm(inp, 0, share PASS_REGS);
}

static Int p_unnumbervars(USES_REGS1) {
  /* this should be a standard Prolog term, so we allow sharing? */
  return Yap_unify(UnnumberTerm(ARG1, 2, FALSE PASS_REGS), ARG2);
}

Int Yap_SkipList(Term *l, Term **tailp) {
  Int length = 0;
  Term *s; /* slow */
  Term v;  /* temporary */

  do_derefa(v, l, derefa_unk, derefa_nonvar);
  s = l;

  if (IsPairTerm(*l)) {
    intptr_t power = 1, lam = 0;
    do {
      if (power == lam) {
        s = l;
        power *= 2;
        lam = 0;
      }
      lam++;
      length++;
      l = RepPair(*l) + 1;
      do_derefa(v, l, derefa2_unk, derefa2_nonvar);
    } while (*l != *s && IsPairTerm(*l));
  }
  *tailp = l;

  return length;
}

static Int p_skip_list(USES_REGS1) {
  Term *tail;
  Int len = Yap_SkipList(XREGS + 2, &tail);

  return Yap_unify(MkIntegerTerm(len), ARG1) && Yap_unify(*tail, ARG3);
}

static Int p_skip_list4(USES_REGS1) {
  Term *tail;
  Int len, len1 = -1;
  Term t2 = Deref(ARG2), t;

  if (!IsVarTerm(t2)) {
    if (!IsIntegerTerm(t2)) {
      Yap_Error(TYPE_ERROR_INTEGER, t2, "length/2");
      return FALSE;
    }
    if ((len1 = IntegerOfTerm(t2)) < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t2, "length/2");
      return FALSE;
    }
  }
  /* we need len here */
  len = Yap_SkipList(XREGS + 1, &tail);
  t = *tail;
  /* don't set M0 if full list, just check M */
  if (t == TermNil) {
    if (len1 >= 0) { /* ARG2 was bound */
      return len1 == len && Yap_unify(t, ARG4);
    } else {
      return Yap_unify_constant(ARG4, TermNil) &&
             Yap_unify_constant(ARG2, MkIntegerTerm(len));
    }
  }
  return Yap_unify(MkIntegerTerm(len), ARG3) && Yap_unify(t, ARG4);
}

static Int p_free_arguments(USES_REGS1) {
  Term t = Deref(ARG1);
  if (IsVarTerm(t))
    return FALSE;
  if (IsAtomTerm(t) || IsIntTerm(t))
    return TRUE;
  if (IsPairTerm(t)) {
    Term th = HeadOfTerm(t);
    Term tl = TailOfTerm(t);
    return IsVarTerm(th) && IsVarTerm(tl) && th != tl;
  } else {
    Functor f = FunctorOfTerm(t);
    UInt i, ar;
    Int ret = TRUE;

    if (IsExtensionFunctor(f))
      return TRUE;
    ar = ArityOfFunctor(f);
    for (i = 1; i <= ar; i++) {
      Term ta = ArgOfTerm(i, t);
      Int j;

      ret = IsVarTerm(ta);
      if (!ret)
        break;
      /* stupid quadractic algorithm, but needs no testing for overflows */
      for (j = 1; j < i; j++) {
        ret = ArgOfTerm(j, t) != ta;
        if (!ret)
          break;
      }
      if (!ret)
        break;
    }
    return ret;
  }
}

static Int p_freshen_variables(USES_REGS1) {
  Term t = Deref(ARG1);
  Functor f = FunctorOfTerm(t);
  UInt arity = ArityOfFunctor(f), i;
  Term tn = Yap_MkNewApplTerm(f, arity);
  CELL *src = RepAppl(t) + 1;
  CELL *targ = RepAppl(tn) + 1;
  for (i = 0; i < arity; i++) {
    RESET_VARIABLE(targ);
    *VarOfTerm(*src) = (CELL)targ;
    targ++;
    src++;
  }
  return TRUE;
}

static Int p_reset_variables(USES_REGS1) {
  Term t = Deref(ARG1);
  Functor f = FunctorOfTerm(t);
  UInt arity = ArityOfFunctor(f), i;
  CELL *src = RepAppl(t) + 1;

  for (i = 0; i < arity; i++) {
    RESET_VARIABLE(VarOfTerm(*src));
    src++;
  }
  return TRUE;
}

void Yap_InitUtilCPreds(void) {
  CACHE_REGS
  Term cm = CurrentModule;
  Yap_InitCPred("copy_term", 2, p_copy_term, 0);
  /** @pred  copy_term(? _TI_,- _TF_) is iso


  Term  _TF_ is a variant of the original term  _TI_, such that for
  each variable  _V_ in the term  _TI_ there is a new variable  _V'_
  in term  _TF_. Notice that:

  + suspended goals and attributes for attributed variables in _TI_ are also
  duplicated;
  + ground terms are shared between the new and the old term.

  If you do not want any sharing to occur please use
  duplicate_term/2.


  */
  Yap_InitCPred("duplicate_term", 2, p_duplicate_term, 0);
  /** @pred  duplicate_term(? _TI_,- _TF_)


  Term  _TF_ is a variant of the original term  _TI_, such that
  for each variable  _V_ in the term  _TI_ there is a new variable
   _V'_ in term  _TF_, and the two terms do not share any
  structure. All suspended goals and attributes for attributed variables
  in  _TI_ are also duplicated.

  Also refer to copy_term/2.


  */
  Yap_InitCPred("copy_term_nat", 2, p_copy_term_no_delays, 0);
  /** @pred copy_term_nat(? _TI_,- _TF_)


  As copy_term/2.  Attributes however, are <em>not</em> copied but replaced
  by fresh variables.




   */
  Yap_InitCPred("ground", 1, p_ground, SafePredFlag);
  /** @pred  ground( _T_) is iso


  Succeeds if there are no free variables in the term  _T_.


  */
  Yap_InitCPred("$variables_in_term", 3, p_variables_in_term, 0);
  Yap_InitCPred("$free_variables_in_term", 3, p_free_variables_in_term, 0);
  Yap_InitCPred("$non_singletons_in_term", 3, p_non_singletons_in_term, 0);
  Yap_InitCPred("term_variables", 2, p_term_variables, 0);
  /** @pred  term_variables(? _Term_, - _Variables_) is iso



  Unify  _Variables_ with the list of all variables of term
   _Term_.  The variables occur in the order of their first
  appearance when traversing the term depth-first, left-to-right.


  */
  Yap_InitCPred("term_variables", 3, p_term_variables3, 0);
  Yap_InitCPred("term_attvars", 2, p_term_attvars, 0);
  /** @pred term_attvars(+ _Term_,- _AttVars_)


   _AttVars_ is a list of all attributed variables in  _Term_ and
  its attributes. I.e., term_attvars/2 works recursively through
  attributes.  This predicate is Cycle-safe.


  */
  Yap_InitCPred("is_list", 1, p_is_list, SafePredFlag | TestPredFlag);
  Yap_InitCPred("$is_list_or_partial_list", 1, p_is_list_or_partial_list,
                SafePredFlag | TestPredFlag);
  Yap_InitCPred("rational_term_to_tree", 4, p_break_rational, 0);
  /** @pred  rational_term_to_tree(? _TI_,- _TF_, ?SubTerms, ?MoreSubterms)


  The term _TF_ is a forest representation (without cycles and repeated
  terms) for the Prolog term _TI_. The term _TF_ is the main term.  The
  difference list _SubTerms_-_MoreSubterms_ stores terms of the form
  _V=T_, where _V_ is a new variable occuring in _TF_, and _T_ is a copy
  of a sub-term from _TI_.


  */
  Yap_InitCPred("term_factorized", 3, p_break_rational3, 0);
  /** @pred  term_factorized(? _TI_,- _TF_, ?SubTerms)


  Similar to rational_term_to_tree/4, but _SubTerms_ is a proper list.


  */
  Yap_InitCPred("=@=", 2, p_variant, 0);
  Yap_InitCPred("numbervars", 3, p_numbervars, 0);
  /** @pred  numbervars( _T_,+ _N1_,- _Nn_)


  Instantiates each variable in term  _T_ to a term of the form:
  `$VAR( _I_)`, with  _I_ increasing from  _N1_ to  _Nn_.


  */
  Yap_InitCPred("unnumbervars", 2, p_unnumbervars, 0);
  /** @pred  unnumbervars( _T_,+ _NT_)


  Replace every `$VAR( _I_)` by a free variable.


  */
  /* use this carefully */
  Yap_InitCPred("$skip_list", 3, p_skip_list, SafePredFlag | TestPredFlag);
  Yap_InitCPred("$skip_list", 4, p_skip_list4, SafePredFlag | TestPredFlag);
  Yap_InitCPred("$free_arguments", 1, p_free_arguments, TestPredFlag);
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
  Yap_InitCPred("freshen_variables", 1, p_freshen_variables, 0);
  Yap_InitCPred("reset_variables", 1, p_reset_variables, 0);
  CurrentModule = cm;
#ifdef DEBUG
  Yap_InitCPred("$force_trail_expansion", 1, p_force_trail_expansion,
                SafePredFlag);
  Yap_InitCPred("dum", 1, camacho_dum, SafePredFlag);
#endif
}
