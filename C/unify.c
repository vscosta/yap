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
/** @defgroup Rational_Trees Rational Trees
@ingroup extensions
@{

Prolog unification is not a complete implementation. For efficiency
considerations, Prolog systems do not perform occur checks while
unifying terms. As an example, `X = a(X)` will not fail but instead
will create an infinite term of the form `a(a(a(a(a(...)))))`, or
<em>rational tree</em>.

Rational trees are now supported by default in YAP. In previous
versions, this was not the default and these terms could easily lead
to infinite computation. For example, `X = a(X), X = X` would
enter an infinite loop.

The `RATIONAL_TREES` flag improves support for these
terms. Internal primitives are now aware that these terms can exist, and
will not enter infinite loops. Hence, the previous unification will
succeed. Another example, `X = a(X), ground(X)` will succeed
instead of looping. Other affected built-ins include the term comparison
primitives, numbervars/3, copy_term/2, and the internal
data base routines. The support does not extend to Input/Output routines
or to assert/1 YAP does not allow directly reading
rational trees, and you need to use `write_depth/2` to avoid
entering an infinite cycle when trying to write an infinite term.


 */

#define IN_UNIFY_C 1

#define HAS_CACHE_REGS 1

#include "absmi.h"

int   Yap_rational_tree_loop(CELL *, CELL *, CELL **, CELL **);

static int    OCUnify_complex(CELL *, CELL *, CELL *);
static int    OCUnify(register CELL, register CELL);
static Int   p_ocunify( USES_REGS1 );

/* support for rational trees and unification with occur checking */

#define to_visit_base ((struct v_record *)AuxSp)

int
Yap_rational_tree_loop(CELL *pt0, CELL *pt0_end, CELL **to_visit, CELL **to_visit_max)
{
  CELL ** base = to_visit;
rtree_loop:
  while (pt0 < pt0_end) {
    register CELL *ptd0;
    register CELL d0;

    ptd0 = ++pt0; 
    pt0 = ptd0;
    d0 = *ptd0;
    deref_head(d0, rtree_loop_unk);
  rtree_loop_nvar:
    {
      if (d0 == TermFoundVar)
	goto cufail;
      if (IsPairTerm(d0)) {
	to_visit -= 3;
	if (to_visit < to_visit_max) {
	  to_visit = Yap_shift_visit(to_visit, &to_visit_max, &base);
	}
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	*pt0 = TermFoundVar;
	pt0_end = (pt0 = RepPair(d0) - 1) + 2;
	continue;
      }
      if (IsApplTerm(d0)) {
	register Functor f;
	register CELL *ap2;

	/* store the terms to visit */
	ap2 = RepAppl(d0);
	f = (Functor) (*ap2);
	/* compare functors */
	if (IsExtensionFunctor(f)) {
	  continue;
	}
	to_visit -= 3;
	if (to_visit < to_visit_max) {
	  to_visit = Yap_shift_visit(to_visit, &to_visit_max, &base);
	}
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)*pt0;
	*pt0 = TermFoundVar;
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
	continue;
      }
      continue;
    }

    derefa_body(d0, ptd0, rtree_loop_unk, rtree_loop_nvar);
  }
  /* Do we still have compound terms to visit */
  if (to_visit < base) {
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    *pt0 = (CELL)to_visit[2];
    to_visit += 3;
    goto rtree_loop;
  }
  return FALSE;

cufail:
  /* we found an infinite term */
  while (to_visit < (CELL **)base) {
    CELL *pt0;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
    to_visit += 3;
  }
  return TRUE;
}

static inline int
rational_tree(Term d0) {
CACHE_REGS
  CELL  **to_visit_max = (CELL **)AuxBase, **to_visit  = (CELL **)AuxSp;

  if (IsPairTerm(d0)) {
    CELL *pt0 = RepPair(d0);

    return Yap_rational_tree_loop(pt0-1, pt0+1, to_visit, to_visit_max);
  } else if (IsApplTerm(d0)) {
    CELL *pt0 = RepAppl(d0);
    Functor f = (Functor)(*pt0);

    if (IsExtensionFunctor(f))
      return FALSE;
    return Yap_rational_tree_loop(pt0, pt0+ArityOfFunctor(f), to_visit, to_visit_max);
  } else
    return FALSE;
}

static int 
OCUnify_complex(CELL *pt0, CELL *pt0_end, CELL *pt1)
{
CACHE_REGS
#ifdef THREADS
#undef Yap_REGS
  register REGSTORE *regp = Yap_regp;
#define Yap_REGS (*regp)
#elif defined(SHADOW_REGS)
#if defined(B) || defined(TR)
  register REGSTORE *regp = &Yap_REGS;

#define Yap_REGS (*regp)
#endif /* defined(B) || defined(TR) || defined(HB) */
#endif

#ifdef SHADOW_HB
  register CELL *HBREG = HB;
#endif /* SHADOW_HB */

  struct unif_record  *unif = (struct unif_record *)AuxBase;
  struct v_record *to_visit  = (struct v_record *)AuxSp;
#define unif_base ((struct unif_record *)AuxBase)

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
      if (d0 == d1) {
	if (Yap_rational_tree_loop(pt0-1, pt0, (CELL **)to_visit, (CELL **)unif))
	  goto cufail;
	continue;
      }
      if (IsPairTerm(d0)) {
	if (!IsPairTerm(d1)) {
	  goto cufail;
	}
	/* now link the two structures so that no one else will */
	/* come here */
	/* store the terms to visit */
	if (RATIONAL_TREES || pt0 < pt0_end) {
	  to_visit --;
#ifdef RATIONAL_TREES
	  unif++;
#endif
	  if ((void *)to_visit < (void *)unif) {
	    CELL **urec = (CELL **)unif;
	    to_visit = (struct v_record *)Yap_shift_visit((CELL **)to_visit, &urec, NULL);
	    unif = (struct unif_record *)urec;
	  }
	  to_visit->start0 = pt0;
	  to_visit->end0 = pt0_end;
	  to_visit->start1 = pt1;
#ifdef RATIONAL_TREES
	  unif[-1].old = *pt0;
	  unif[-1].ptr = pt0;
	  *pt0 = d1;
#endif
	}
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
	/* now link the two structures so that no one else will */
	/* come here */
	/* store the terms to visit */
	if (RATIONAL_TREES || pt0 < pt0_end) {
	  to_visit --;
#ifdef RATIONAL_TREES
	  unif++;
#endif
	  if ((void *)to_visit < (void *)unif) {
	    CELL **urec = (CELL **)unif;
	    to_visit = (struct v_record *)Yap_shift_visit((CELL **)to_visit, &urec, NULL);
	    unif = (struct unif_record *)urec;
	  }
	  to_visit->start0 = pt0;
	  to_visit->end0 = pt0_end;
	  to_visit->start1 = pt1;
#ifdef RATIONAL_TREES
	  unif[-1].old = *pt0;
	  unif[-1].ptr = pt0;
	  *pt0 = d1;
#endif
	}
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
	pt1 = ap3;
	continue;
      }
      goto cufail;

      derefa_body(d1, ptd1, unify_comp_nvar_unk, unify_comp_nvar_nvar);
      /* d1 and pt2 have the unbound value, whereas d0 is bound */
      Bind_Global(ptd1, d0);
      if (Yap_rational_tree_loop(ptd1-1, ptd1, (CELL **)to_visit, (CELL **)unif))
	goto cufail;
      continue;
    }

    derefa_body(d0, ptd0, unify_comp_unk, unify_comp_nvar);
    /* first arg var */
    {
      register CELL d1;
      register CELL *ptd1;

      ptd1 = pt1;
      d1 = ptd1[0];
      /* pt2 is unbound */
      deref_head(d1, unify_comp_var_unk);
    unify_comp_var_nvar:
      /* pt2 is unbound and d1 is bound */
      Bind_Global(ptd0, d1);
      if (Yap_rational_tree_loop(ptd0-1, ptd0, (CELL **)to_visit, (CELL **)unif))
	goto cufail;
      continue;

      derefa_body(d1, ptd1, unify_comp_var_unk, unify_comp_var_nvar);
      /* ptd0 and ptd1 are unbound */
      UnifyGlobalCells(ptd0, ptd1);
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit < to_visit_base) {
    pt0 = to_visit->start0;
    pt0_end = to_visit->end0;
    pt1 = to_visit->start1;
    to_visit++;
    goto loop;
  }
#ifdef RATIONAL_TREES
  /* restore bindigs */
  while (unif-- != unif_base) {
    CELL *pt0;

    pt0 = unif->ptr;
    *pt0 = unif->old;
  }
#endif
  return TRUE;

cufail:
#ifdef RATIONAL_TREES
  /* restore bindigs */
  while (unif-- != unif_base) {
    CELL *pt0;

    pt0 = unif->ptr;
    *pt0 = unif->old;
  }
#endif
  return FALSE;
#ifdef THREADS
#undef Yap_REGS
#define Yap_REGS (*Yap_regp)  
#elif defined(SHADOW_REGS)
#if defined(B) || defined(TR)
#undef Yap_REGS
#endif /* defined(B) || defined(TR) */
#endif
#undef unif_base
#undef to_visit_base
}

static int 
OCUnify(register CELL d0, register CELL d1)
{
CACHE_REGS
  register CELL *pt0, *pt1;

#if SHADOW_HB
  register CELL *HBREG = HB;
#endif

  deref_head(d0, oc_unify_unk);

oc_unify_nvar:
  /* d0 is bound */
  deref_head(d1, oc_unify_nvar_unk);
oc_unify_nvar_nvar:

  if (d0 == d1) {
    return (!rational_tree(d0));
  }
  /* both arguments are bound */
  if (IsPairTerm(d0)) {
    if (!IsPairTerm(d1)) {
	return (FALSE);
    }
    pt0 = RepPair(d0);
    pt1 = RepPair(d1);
    return (OCUnify_complex(pt0 - 1, pt0 + 1, pt1 - 1));
  }
  else if (IsApplTerm(d0)) {
    if (!IsApplTerm(d1))
	return (FALSE);
    pt0 = RepAppl(d0);
    d0 = *pt0;
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
	case (CELL)FunctorString:
	  return(strcmp( (const char *)(pt0+2),  (const char *)(pt1+2)) == 0);
#ifdef USE_GMP
	case (CELL)FunctorBigInt:
	  return(Yap_gmp_tcmp_big_big(AbsAppl(pt0),AbsAppl(pt0)) == 0);
#endif /* USE_GMP */
	default:
	  return(FALSE);
	}
      }
      return (OCUnify_complex(pt0, pt0 + ArityOfFunctor((Functor) d0),
			      pt1));
    }
  } else {
    return(FALSE);
  }

  deref_body(d1, pt1, oc_unify_nvar_unk, oc_unify_nvar_nvar);
  /* d0 is bound and d1 is unbound */
  YapBind(pt1, d0);
  /* local variables cannot be in a term */
  if (pt1 > HR && pt1 < LCL0)
    return TRUE;
  if (rational_tree(d0))
    return(FALSE);
  return (TRUE);

  deref_body(d0, pt0, oc_unify_unk, oc_unify_nvar);
  /* pt0 is unbound */
  deref_head(d1, oc_unify_var_unk);
oc_unify_var_nvar:
  /* pt0 is unbound and d1 is bound */
  YapBind(pt0, d1);
  /* local variables cannot be in a term */
  if (pt0 > HR && pt0 < LCL0)
    return TRUE;
  if (rational_tree(d1))
    return(FALSE);
  return (TRUE);

  deref_body(d1, pt1, oc_unify_var_unk, oc_unify_var_nvar);
  /* d0 and pt1 are unbound */
  UnifyCells(pt0, pt1);
  return (TRUE);
  return (TRUE);
}

static Int
p_ocunify( USES_REGS1 )
{
  return(OCUnify(ARG1,ARG2));
}

static Int
p_cyclic( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t))
    return(FALSE);
  return rational_tree(t);
}

bool Yap_IsAcyclicTerm(Term t)
{
  return !rational_tree(t);
}

static Int
p_acyclic( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t))
    return(TRUE);
  return !rational_tree(t);
}

int 
Yap_IUnify(register CELL d0, register CELL d1)
{
CACHE_REGS
#if THREADS
#undef Yap_REGS
  register REGSTORE *regp = Yap_regp;
#define Yap_REGS (*regp)
#elif SHADOW_REGS
#if defined(B) || defined(TR)
  register REGSTORE *regp = &Yap_REGS;

#define Yap_REGS (*regp)
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
    return TRUE;
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
      return (IUnify_complex(pt0, pt0 + ArityOfFunctor((Functor) d0),
			     pt1));
    }
  } else {
    return (FALSE);
  }

  deref_body(d1, pt1, unify_nvar_unk, unify_nvar_nvar);
  /* d0 is bound and d1 is unbound */
  YapBind(pt1, d0);
  return (TRUE);

  deref_body(d0, pt0, unify_unk, unify_nvar);
  /* pt0 is unbound */
  deref_head(d1, unify_var_unk);
unify_var_nvar:
  /* pt0 is unbound and d1 is bound */
  YapBind(pt0, d1);
  return TRUE;

#if TRAILING_REQUIRES_BRANCH
unify_var_nvar_trail:
  DO_TRAIL(pt0);
  return TRUE;
#endif

  deref_body(d1, pt1, unify_var_unk, unify_var_nvar);
  /* d0 and pt1 are unbound */
  UnifyCells(pt0, pt1);
  return (TRUE);

#if THREADS
#undef Yap_REGS
#define Yap_REGS (*Yap_regp)  
#elif SHADOW_REGS
#if defined(B) || defined(TR)
#undef Yap_REGS
#endif /* defined(B) || defined(TR) */
#endif
}

/**********************************************************************
 *                                                                    *
 *                 Conversion from Label to Op                        *
 *                                                                    *
 **********************************************************************/

#if USE_THREADED_CODE

/* mask a hash table that allows for fast reverse translation from
   instruction address to corresponding opcode */
static void
InitReverseLookupOpcode(void)
{
  op_entry *opeptr;
  op_numbers i;
  /* 2 K should be OK */
  int hash_size_mask = OP_HASH_SIZE-1;
  UInt sz = OP_HASH_SIZE*sizeof(struct opcode_tab_entry);

    if ((OP_RTABLE = (op_entry *)Yap_AllocCodeSpace(sz)) == NULL) {
      if (!Yap_growheap(FALSE, sz, NULL)) {
	Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
		  "Couldn't obtain space for the reverse translation opcode table");
    }
  }
  memset(OP_RTABLE, 0, sz);
  opeptr = OP_RTABLE;
  /* clear up table */
  {
    int j;
    for (j=0; j<OP_HASH_SIZE; j++) {
      opeptr[j].opc = 0;
      opeptr[j].opnum = _Ystop;
    }
  }
  opeptr = OP_RTABLE;
  opeptr[rtable_hash_op(Yap_opcode(_Ystop),hash_size_mask)].opc
	    = Yap_opcode(_Ystop);
  /* now place entries */
  for (i = _std_top; i > _Ystop; i--) {
    OPCODE opc = Yap_opcode(i);
    int j = rtable_hash_op(opc,hash_size_mask);
    while (opeptr[j].opc) {
      if (++j > hash_size_mask)
	j = 0;	  
    }
    /* clear entry, no conflict */
    opeptr[j].opnum = i;
    opeptr[j].opc = opc;
  }
}
#endif

#define UnifyAndTrailGlobalCells(a, b)                                \
     if((a) > (b)) {                                                  \
       *(a) = (CELL)(b);                                              \
        DO_TRAIL((a), (CELL)(b));                                     \
     } else if((a) < (b)){                                            \
       *(b) = (CELL)(a);                                              \
        DO_TRAIL((b), (CELL)(a));                                     \
     }

static int 
unifiable_complex(CELL *pt0, CELL *pt0_end, CELL *pt1)
{
CACHE_REGS
#ifdef THREADS
#undef Yap_REGS
  register REGSTORE *regp = Yap_regp;
#define Yap_REGS (*regp)
#elif defined(SHADOW_REGS)
#if defined(B) || defined(TR)
  register REGSTORE *regp = &Yap_REGS;

#define Yap_REGS (*regp)
#endif /* defined(B) || defined(TR) || defined(HB) */
#endif

#ifdef SHADOW_HB
  register CELL *HBREG = HB;
#endif /* SHADOW_HB */

  struct unif_record  *unif = (struct unif_record *)AuxBase;
  struct v_record *to_visit  = (struct v_record *)AuxSp;
#define unif_base ((struct unif_record *)AuxBase)
#define to_visit_base ((struct v_record *)AuxSp)

loop:
  while (pt0 < pt0_end) {
    register CELL *ptd0 = pt0+1; 
    register CELL d0;

    ++pt1;
    pt0 = ptd0;
    d0 = *ptd0;
    deref_head(d0, unifiable_comp_unk);
  unifiable_comp_nvar:
    {
      register CELL *ptd1 = pt1;
      register CELL d1 = *ptd1;

      deref_head(d1, unifiable_comp_nvar_unk);
    unifiable_comp_nvar_nvar:
      if (d0 == d1)
	continue;
      if (IsPairTerm(d0)) {
	if (!IsPairTerm(d1)) {
	  goto cufail;
	}
	/* now link the two structures so that no one else will */
	/* come here */
	/* store the terms to visit */
	if (RATIONAL_TREES || pt0 < pt0_end) {
	  to_visit --;
#ifdef RATIONAL_TREES
	  unif++;
#endif
	  if ((void *)to_visit < (void *)unif) {
	    CELL **urec = (CELL **)unif;
	    to_visit = (struct v_record *)Yap_shift_visit((CELL **)to_visit, &urec, NULL);
	    unif = (struct unif_record *)urec;
	  }
	  to_visit->start0 = pt0;
	  to_visit->end0 = pt0_end;
	  to_visit->start1 = pt1;
#ifdef RATIONAL_TREES
	  unif[-1].old = *pt0;
	  unif[-1].ptr = pt0;
	  *pt0 = d1;
#endif
	}
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
	/* now link the two structures so that no one else will */
	/* come here */
	/* store the terms to visit */
	if (RATIONAL_TREES || pt0 < pt0_end) {
	  to_visit --;
#ifdef RATIONAL_TREES
	  unif++;
#endif
	  if ((void *)to_visit < (void *)unif) {
	    CELL **urec = (CELL **)unif;
	    to_visit = (struct v_record *)Yap_shift_visit((CELL **)to_visit, &urec, NULL);
	    unif = (struct unif_record *)urec;
	  }
	  to_visit->start0 = pt0;
	  to_visit->end0 = pt0_end;
	  to_visit->start1 = pt1;
#ifdef RATIONAL_TREES
	  unif[-1].old = *pt0;
	  unif[-1].ptr = pt0;
	  *pt0 = d1;
#endif
	}
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
	pt1 = ap3;
	continue;
      }
      goto cufail;

      derefa_body(d1, ptd1, unifiable_comp_nvar_unk, unifiable_comp_nvar_nvar);
	/* d1 and pt2 have the unbound value, whereas d0 is bound */
      *(ptd1) = d0;
      DO_TRAIL(ptd1, d0);
      continue;
    }

    derefa_body(d0, ptd0, unifiable_comp_unk, unifiable_comp_nvar);
    /* first arg var */
    {
      register CELL d1;
      register CELL *ptd1;

      ptd1 = pt1;
      d1 = ptd1[0];
      /* pt2 is unbound */
      deref_head(d1, unifiable_comp_var_unk);
    unifiable_comp_var_nvar:
      /* pt2 is unbound and d1 is bound */
      *ptd0 = d1;
       DO_TRAIL(ptd0, d1);
      continue;

      derefa_body(d1, ptd1, unifiable_comp_var_unk, unifiable_comp_var_nvar);
      /* ptd0 and ptd1 are unbound */
      UnifyAndTrailGlobalCells(ptd0, ptd1);
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit < to_visit_base) {
    pt0 = to_visit->start0;
    pt0_end = to_visit->end0;
    pt1 = to_visit->start1;
    to_visit++;
    goto loop;
  }
#ifdef RATIONAL_TREES
  /* restore bindigs */
  while (unif-- != unif_base) {
    CELL *pt0;

    pt0 = unif->ptr;
    *pt0 = unif->old;
  }
#endif
  return TRUE;

cufail:
#ifdef RATIONAL_TREES
  /* restore bindigs */
  while (unif-- != unif_base) {
    CELL *pt0;

    pt0 = unif->ptr;
    *pt0 = unif->old;
  }
#endif
  return FALSE;
#ifdef THREADS
#undef Yap_REGS
#define Yap_REGS (*Yap_regp)  
#elif defined(SHADOW_REGS)
#if defined(B) || defined(TR)
#undef Yap_REGS
#endif /* defined(B) || defined(TR) */
#endif
}

/*  don't pollute name space */
#undef to_visit_base
#undef unif_base


static int 
unifiable(CELL d0, CELL d1)
{
CACHE_REGS
#if THREADS
#undef Yap_REGS
  register REGSTORE *regp = Yap_regp;
#define Yap_REGS (*regp)
#elif SHADOW_REGS
#if defined(B) || defined(TR)
  register REGSTORE *regp = &Yap_REGS;

#define Yap_REGS (*regp)
#endif /* defined(B) || defined(TR) */
#endif

#if SHADOW_HB
  register CELL *HBREG = HB;
#endif

  register CELL *pt0, *pt1;

  deref_head(d0, unifiable_unk);

unifiable_nvar:
  /* d0 is bound */
  deref_head(d1, unifiable_nvar_unk);
unifiable_nvar_nvar:
  /* both arguments are bound */
  if (d0 == d1)
    return TRUE;
  if (IsPairTerm(d0)) {
    if (!IsPairTerm(d1)) {
      return (FALSE);
    }
    pt0 = RepPair(d0);
    pt1 = RepPair(d1);
    return (unifiable_complex(pt0 - 1, pt0 + 1, pt1 - 1));
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
      return (unifiable_complex(pt0, pt0 + ArityOfFunctor((Functor) d0),
			     pt1));
    }
  } else {
    return (FALSE);
  }

  deref_body(d1, pt1, unifiable_nvar_unk, unifiable_nvar_nvar);
  /* d0 is bound and d1 is unbound */
  *(pt1) = d0;
  DO_TRAIL(pt1, d0);
  return (TRUE);

  deref_body(d0, pt0, unifiable_unk, unifiable_nvar);
  /* pt0 is unbound */
  deref_head(d1, unifiable_var_unk);
unifiable_var_nvar:
  /* pt0 is unbound and d1 is bound */
  *pt0 = d1;
   DO_TRAIL(pt0, d1);
  return TRUE;

  deref_body(d1, pt1, unifiable_var_unk, unifiable_var_nvar);
  /* d0 and pt1 are unbound */
  UnifyAndTrailCells(pt0, pt1);
  return (TRUE);
#if THREADS
#undef Yap_REGS
#define Yap_REGS (*Yap_regp)  
#elif SHADOW_REGS
#if defined(B) || defined(TR)
#undef Yap_REGS
#endif /* defined(B) || defined(TR) */
#endif
}


static Int
p_unifiable( USES_REGS1 )
{
  tr_fr_ptr trp, trp0 = TR;
  Term tf = TermNil;
  if (!unifiable(ARG1,ARG2)) {
    return FALSE;
  }
  trp = TR;
  while (trp != trp0) {
    Term t[2];
    --trp;
    t[0] = TrailTerm(trp);
    t[1] = *(CELL *)t[0];
    tf = MkPairTerm(Yap_MkApplTerm(FunctorEq,2,t),tf);
    RESET_VARIABLE(t[0]);
  }
  return Yap_unify(ARG3, tf);
}

int
Yap_Unifiable( Term d0, Term d1 )
{
  CACHE_REGS
  tr_fr_ptr trp, trp0 = TR;

  if (!unifiable(d0,d1)) {
    return FALSE;
  }
  trp = TR;
  while (trp != trp0) {
    Term t;

    --trp;
    t = TrailTerm(trp);
    RESET_VARIABLE(t);
  }
  return TRUE;
}

void 
Yap_InitUnify(void)
{
  CACHE_REGS
  Term cm = CurrentModule;
  Yap_InitCPred("unify_with_occurs_check", 2, p_ocunify, SafePredFlag);
  /** @pred  unify_with_occurs_check(?T1,?T2) is iso 


Obtain the most general unifier of terms  _T1_ and  _T2_, if there
is one.

This predicate implements the full unification algorithm. An example:n

~~~~~
unify_with_occurs_check(a(X,b,Z),a(X,A,f(B)).
~~~~~
will succeed with the bindings `A = b` and `Z = f(B)`. On the
other hand:

~~~~~
unify_with_occurs_check(a(X,b,Z),a(X,A,f(Z)).
~~~~~
would fail, because `Z` is not unifiable with `f(Z)`. Note that
`(=)/2` would succeed for the previous examples, giving the following
bindings `A = b` and `Z = f(Z)`.

 
*/
  Yap_InitCPred("acyclic_term", 1, p_acyclic, SafePredFlag|TestPredFlag);
/** @pred  acyclic_term( _T_) is iso 


Succeeds if there are loops in the term  _T_, that is, it is an infinite term.

 
*/
  CurrentModule = TERMS_MODULE;
  Yap_InitCPred("cyclic_term", 1, p_cyclic, SafePredFlag|TestPredFlag);
  Yap_InitCPred("unifiable", 3, p_unifiable, 0);
  CurrentModule = cm;
}


void 
Yap_InitAbsmi(void)
{
  /* initialize access to abstract machine instructions */
#if USE_THREADED_CODE
  Yap_absmi(1);
  InitReverseLookupOpcode();
#endif
}

void
Yap_TrimTrail(void)
{
  CACHE_REGS
#ifdef saveregs
#undef saveregs
#define saveregs()
#endif
#ifdef setregs
#undef setregs
#define setregs()
#endif
#if SHADOW_HB
  register CELL *HBREG = HB;
#endif

#include "trim_trail.h"
}

//! @}
