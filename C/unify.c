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
#define IN_UNIFY_C 1

#include "absmi.h"

STATIC_PROTO(Int    OCUnify_complex, (register CELL *, register CELL *, register CELL *));
STATIC_PROTO(int    OCUnify, (register CELL, register CELL));
STATIC_PROTO(Int   p_ocunify, (void));
#ifdef THREADED_CODE
STATIC_PROTO(int    rtable_hash_op, (OPCODE));
STATIC_PROTO(void   InitReverseLookupOpcode, (void));
#endif

/* support for rational trees and unification with occur checking */

#if USE_SYSTEM_MALLOC
#define address_to_visit_max (&to_visit_max)
#define to_visit_base ((CELL **)AuxSp)

#define TO_VISIT to_visit, to_visit_max
int STD_PROTO(rational_tree_loop,(CELL *, CELL *, CELL **, CELL **));

#else
#define to_visit_max ((CELL **)TR+16)
#define address_to_visit_max NULL
#define to_visit_base ((CELL **)Yap_TrailTop)

#define TO_VISIT to_visit
int STD_PROTO(rational_tree_loop,(CELL *, CELL *, CELL **));

#endif

int
rational_tree_loop(CELL *pt0, CELL *pt0_end, CELL **to_visit0
#if USE_SYSTEM_MALLOC
		   , CELL **to_visit_max
#endif
		   )
{
  CELL **to_visit = to_visit0;

loop:
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
	  to_visit = Yap_shift_visit(to_visit, address_to_visit_max);
	}
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)d0;
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
	  to_visit = Yap_shift_visit(to_visit, address_to_visit_max);
	}
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)d0;
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
  if (to_visit > to_visit0) {
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    *pt0 = (CELL)to_visit[2];
    to_visit += 3;
    goto loop;
  }
  return FALSE;

cufail:
  /* we found an infinite term */
  while (to_visit > to_visit) {
    CELL *pt0;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
    to_visit += 3;
  }
  return (TRUE);
  
}

static inline int
rational_tree(Term d0) {
#if USE_SYSTEM_MALLOC
  CELL  **to_visit_max = (CELL **)Yap_PreAllocCodeSpace(), **to_visit  = (CELL **)AuxSp;
#else
  CELL  **to_visit  = (CELL **)Yap_TrailTop;
#endif

  if (IsPairTerm(d0)) {
    CELL *pt0 = RepPair(d0);

    return rational_tree_loop(pt0-1, pt0+1, TO_VISIT);
  } else if (IsApplTerm(d0)) {
    CELL *pt0 = RepAppl(d0);
    Functor f = (Functor)(*pt0);

    return rational_tree_loop(pt0, pt0+ArityOfFunctor(f), TO_VISIT);
  } else
    return FALSE;
}

static Int 
OCUnify_complex(register CELL *pt0, register CELL *pt0_end,
	       register CELL *pt1
)
{

#if SHADOW_HB
  register CELL *HBREG;
  HBREG = HB;
#endif

#if USE_SYSTEM_MALLOC
  CELL  **to_visit_max = (CELL **)Yap_PreAllocCodeSpace(), **to_visit  = (CELL **)AuxSp;
#else
  CELL  **to_visit  = (CELL **)Yap_TrailTop;
#endif

 loop:
  while (pt0 < pt0_end) {
    register CELL *ptd0 = ++pt0;
    register CELL d0 = *ptd0;

    ++pt1;
    deref_head(d0, unify_comp_unk);
  unify_comp_nvar:
    {
      register CELL *ptd1 = pt1;
      register CELL d1 = *ptd1;

      deref_head(d1, unify_comp_nvar_unk);
    unify_comp_nvar_nvar:
      if (d0 == d1) {
	if (rational_tree_loop(pt0-1, pt0, TO_VISIT))
	  goto cufail;
	continue;
      } if (IsPairTerm(d0)) {
	if (!IsPairTerm(d1)) {
	  goto cufail;
	}
	/* now link the two structures so that no one else will */
	/* come here */
	to_visit -= 5;
	if (to_visit < to_visit_max) {
	  to_visit = Yap_shift_visit(to_visit, address_to_visit_max);
	}
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	/* we want unification of rational trees to fail */
	to_visit[3] = (CELL *)*pt0;
	to_visit[4] = (CELL *)*pt1;
	*pt0 = TermFoundVar;
	*pt1 = TermFoundVar;
	pt0_end = (pt0 = RepPair(d0) - 1) + 2;
	pt0_end = RepPair(d0) + 1;
	pt1 = RepPair(d1) - 1;
	continue;
      }
      else if (IsApplTerm(d0)) {
	register Functor f;
	register CELL *ap2, *ap3;

	/* store the terms to visit */
	ap2 = RepAppl(d0);
	f = (Functor) (*ap2);
	if (!IsApplTerm(d1)) {
	  goto cufail;
	}
	ap3 = RepAppl(d1);
	/* compare functors */
	if (f != (Functor) *ap3) {
	  goto cufail;
	}
	if (IsExtensionFunctor(f)) {
	  switch((CELL)f) {
	  case (CELL)FunctorDBRef:
	    if (d0 == d1) continue;
	    goto cufail;
	  case (CELL)FunctorLongInt:
	    if (ap2[1] == ap3[1]) continue;
	    goto cufail;
	  case (CELL)FunctorDouble:
	    if (FloatOfTerm(d0) == FloatOfTerm(d1)) continue;
	    goto cufail;
#ifdef USE_GMP
	  case (CELL)FunctorBigInt:
	    if (mpz_cmp(Yap_BigIntOfTerm(d0),Yap_BigIntOfTerm(d1)) == 0) continue;
	    goto cufail;
#endif /* USE_GMP */
	  default:
	    goto cufail;
	  }
	}
	/* now link the two structures so that no one else will */
	/* come here */
	to_visit -= 5;
	if (to_visit < to_visit_max) {
	  to_visit = Yap_shift_visit(to_visit, address_to_visit_max);
	}
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = (CELL *)*pt0;
	to_visit[4] = (CELL *)*pt1;
	*pt0 = TermFoundVar;
	*pt1 = TermFoundVar;
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
	pt1 = ap3;
	continue;
      } else {
	if (d0 == d1)
	  continue;
	else goto cufail;
      }

      derefa_body(d1, ptd1, unify_comp_nvar_unk, unify_comp_nvar_nvar);
      
      /* d1 and pt2 have the unbound value, whereas d0 is bound */
      BIND_GLOBAL(ptd1, d0, bind_ocunify1);
#ifdef COROUTINING
      DO_TRAIL(ptd1, d0);
      if (ptd1 < H0) Yap_WakeUp(ptd1);
    bind_ocunify1:
#endif
      if (rational_tree_loop(ptd1-1, ptd1, TO_VISIT))
	goto cufail;
      continue;

    }

    derefa_body(d0, ptd0, unify_comp_unk, unify_comp_nvar);
    {
      register CELL d1;
      register CELL *ptd1 = NULL;

      d1 = *(ptd1 = pt1);
      /* pt2 is unbound */
      deref_head(d1, unify_comp_var_unk);
    unify_comp_var_nvar:
      /* pt2 is unbound and d1 is bound */
      BIND_GLOBAL(ptd0, d1, bind_ocunify2);
#ifdef COROUTINING
      DO_TRAIL(ptd0, d1);
      if (ptd0 < H0) Yap_WakeUp(ptd0);
    bind_ocunify2:
#endif
      if (rational_tree_loop(ptd0-1, ptd0, TO_VISIT))
	goto cufail;
      continue;

      derefa_body(d1, ptd1, unify_comp_var_unk, unify_comp_var_nvar);
      /* ptd0 and ptd1 are unbound */
      UnifyGlobalCells(ptd0, ptd1);
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit < to_visit_max) {
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
    *pt1 = (CELL)to_visit[4];
    to_visit += 5;
    goto loop;
  }
  /* successful exit */
  return (TRUE);

cufail:
  /* failure */
  while (to_visit < to_visit_base) {
    CELL *pt0;
    pt0 = to_visit[0];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
    *pt1 = (CELL)to_visit[4];
    to_visit += 5;
  }
  /* failure */
  return (FALSE);
#if SHADOW_REGS
#if defined(B) || defined(TR)
#undef Yap_REGS
#if PUSH_REGS
#define Yap_REGS (*Yap_regp)
#endif
#endif /* defined(B) || defined(TR) */
#endif
}

static int 
OCUnify(register CELL d0, register CELL d1)
{

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
#ifdef USE_GMP
	case (CELL)FunctorBigInt:
	  return(mpz_cmp(Yap_BigIntOfTerm(AbsAppl(pt0)),Yap_BigIntOfTerm(AbsAppl(pt0))) == 0);
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
  BIND(pt1, d0, bind_ocunify4);
#ifdef COROUTINING
  DO_TRAIL(pt1, d0);
  if (pt1 < H0) Yap_WakeUp(pt1);
 bind_ocunify4:
#endif
  if (rational_tree(d0))
    return(FALSE);
  return (TRUE);

  deref_body(d0, pt0, oc_unify_unk, oc_unify_nvar);
  /* pt0 is unbound */
  deref_head(d1, oc_unify_var_unk);
oc_unify_var_nvar:
  /* pt0 is unbound and d1 is bound */
  BIND(pt0, d1, bind_ocunify5);
#ifdef COROUTINING
  DO_TRAIL(pt0, d1);
  if (pt0 < H0) Yap_WakeUp(pt0);
 bind_ocunify5:
#endif
  if (rational_tree(d1))
    return(FALSE);
  return (TRUE);

  deref_body(d1, pt1, oc_unify_var_unk, oc_unify_var_nvar);
  /* d0 and pt1 are unbound */
  UnifyCells(pt0, pt1, uc1, uc2);
#ifdef COROUTINING
  DO_TRAIL(pt0, (CELL)pt1);
  if (pt0 < H0) Yap_WakeUp(pt0);
 uc1:
#endif
  return (TRUE);
#ifdef COROUTINING
 uc2:
  DO_TRAIL(pt1, (CELL)pt0);
  if (pt1 < H0) {
    Yap_WakeUp(pt1);
  }
#endif
  return (TRUE);
}

static Int
p_ocunify(void)
{
  return(OCUnify(ARG1,ARG2));
}

static Int
p_cyclic(void)
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t))
    return(FALSE);
  return rational_tree(t);
}

static Int
p_acyclic(void)
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t))
    return(TRUE);
  return !rational_tree(t);
}

int 
Yap_IUnify(register CELL d0, register CELL d1)
{
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
	case (CELL)FunctorDouble:
	  return(FloatOfTerm(AbsAppl(pt0)) == FloatOfTerm(AbsAppl(pt1)));
#ifdef USE_GMP
	case (CELL)FunctorBigInt:
	  return(mpz_cmp(Yap_BigIntOfTerm(AbsAppl(pt0)),Yap_BigIntOfTerm(AbsAppl(pt0))) == 0);
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
  BIND(pt1, d0, bind_unify3);
#ifdef COROUTINING
  DO_TRAIL(pt1, d0);
  if (pt1 < H0) Yap_WakeUp(pt1);
 bind_unify3:
#endif
  return (TRUE);

  deref_body(d0, pt0, unify_unk, unify_nvar);
  /* pt0 is unbound */
  deref_head(d1, unify_var_unk);
unify_var_nvar:
  /* pt0 is unbound and d1 is bound */
  BIND(pt0, d1, bind_unify4);
#ifdef COROUTINING
  DO_TRAIL(pt0, d1);
  if (pt0 < H0) Yap_WakeUp(pt0);
 bind_unify4:
#endif
  return TRUE;

#if TRAILING_REQUIRES_BRANCH
unify_var_nvar_trail:
  DO_TRAIL(pt0);
  return TRUE;
#endif

  deref_body(d1, pt1, unify_var_unk, unify_var_nvar);
  /* d0 and pt1 are unbound */
  UnifyCells(pt0, pt1, uc1, uc2);
#ifdef COROUTINING
  DO_TRAIL(pt0, (CELL)pt1);
  if (pt0 < H0) Yap_WakeUp(pt0);
 uc1:
#endif
  return (TRUE);
#ifdef COROUTINING
 uc2:
  DO_TRAIL(pt1, (CELL)pt0);
  if (pt1 < H0) {
    Yap_WakeUp(pt1);
  }
  return (TRUE);
#endif
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
  opentry *opeptr;
  op_numbers i;
  /* 2 K should be OK */
  int hash_size_mask = OP_HASH_SIZE-1;

  if (OP_RTABLE == NULL)
    OP_RTABLE = (opentry *)Yap_AllocCodeSpace(OP_HASH_SIZE*sizeof(struct opcode_tab_entry));
  if (OP_RTABLE == NULL) {
    Yap_Error(INTERNAL_ERROR, TermNil,
	  "Couldn't obtain space for the reverse translation opcode table");
  }
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

void 
Yap_InitUnify(void)
{
  Term cm = CurrentModule;
  Yap_InitCPred("unify_with_occurs_check", 2, p_ocunify, SafePredFlag);
  CurrentModule = TERMS_MODULE;
  Yap_InitCPred("cyclic_term", 1, p_cyclic, SafePredFlag|TestPredFlag);
  Yap_InitCPred("acyclic_term", 1, p_acyclic, SafePredFlag|TestPredFlag);
  CurrentModule = cm;
}


void 
Yap_InitAbsmi(void)
{
  /* initialise access to abstract machine instructions */
#if USE_THREADED_CODE
  Yap_absmi(1);
  InitReverseLookupOpcode();
#endif
}

