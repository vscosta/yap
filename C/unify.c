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

#include "absmi.h"

STATIC_PROTO(Int    OCUnify_complex, (register CELL *, register CELL *, register CELL *));
STATIC_PROTO(int    OCUnify, (register CELL, register CELL));
STATIC_PROTO(Int   p_ocunify, (void));
#ifdef THREADED_CODE
STATIC_PROTO(int    rtable_hash_op, (OPCODE));
STATIC_PROTO(void   InitReverseLookupOpcode, (void));
#endif
STATIC_PROTO(Int    p_atom, (void));
STATIC_PROTO(Int    p_atomic, (void));
STATIC_PROTO(Int    p_integer, (void));
STATIC_PROTO(Int    p_nonvar, (void));
STATIC_PROTO(Int    p_number, (void));
STATIC_PROTO(Int    p_var, (void));
STATIC_PROTO(Int    p_db_ref, (void));
STATIC_PROTO(Int    p_primitive, (void));
STATIC_PROTO(Int    p_compound, (void));
STATIC_PROTO(Int    p_float, (void));
STATIC_PROTO(Int    p_equal, (void));
STATIC_PROTO(Int    p_dif, (void));
STATIC_PROTO(Int    p_eq, (void));
STATIC_PROTO(Int    p_arg, (void));
STATIC_PROTO(Int    p_functor, (void));

static int
rational_tree_loop(CELL *pt0, CELL *pt0_end, CELL **to_visit0)
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
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)d0;
	to_visit += 3;
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
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = (CELL *)d0;
	to_visit += 3;
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
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    *pt0 = (CELL)to_visit[2];
    goto loop;
  }
  return (FALSE);

cufail:
#ifdef RATIONAL_TREES
  /* we found an infinite term */
  while (to_visit > to_visit) {
    CELL *pt0;
    to_visit -= 3;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  return (TRUE);
  
}

static inline int
rational_tree(Term d0) {
  if (IsPairTerm(d0)) {
    CELL *pt0 = RepPair(d0);
    CELL **to_visit = (CELL **)H;

    return(rational_tree_loop(pt0-1, pt0+1, to_visit));
  } else if (IsApplTerm(d0)) {
    CELL *pt0 = RepAppl(d0);
    Functor f = (Functor)(*pt0);
    CELL **to_visit = (CELL **)H;

    return(rational_tree_loop(pt0, pt0+ArityOfFunctor(f), to_visit));
  } else
    return(FALSE);
}

static Int 
OCUnify_complex(register CELL *pt0, register CELL *pt0_end,
	       register CELL *pt1
)
{

  register CELL **to_visit;
#if SHADOW_HB
  register CELL *HBREG;
  HBREG = HB;
#endif

  to_visit = (CELL **) H;
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
	if (rational_tree_loop(pt0-1, pt0, to_visit))
	  goto cufail;
	continue;
      } if (IsPairTerm(d0)) {
	if (!IsPairTerm(d1)) {
	  goto cufail;
	}
	/* now link the two structures so that no one else will */
	/* come here */
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	/* we want unification of rational trees to fail */
	to_visit[3] = (CELL *)d0;
	to_visit[4] = (CELL *)d1;
	to_visit += 5;
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
	    if (mpz_cmp(BigIntOfTerm(d0),BigIntOfTerm(d1)) == 0) continue;
	    goto cufail;
#endif /* USE_GMP */
	  default:
	    goto cufail;
	  }
	}
	/* now link the two structures so that no one else will */
	/* come here */
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = (CELL *)d0;
	to_visit[4] = (CELL *)d1;
	to_visit += 5;
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
      if (ptd1 < H0) WakeUp(ptd1);
    bind_ocunify1:
#endif
      if (rational_tree_loop(ptd1-1, ptd1, to_visit))
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
      if (ptd0 < H0) WakeUp(ptd0);
    bind_ocunify2:
#endif
      if (rational_tree_loop(ptd0-1, ptd0, to_visit))
	goto cufail;
      continue;

      derefa_body(d1, ptd1, unify_comp_var_unk, unify_comp_var_nvar);
      /* ptd0 and ptd1 are unbound */
      UnifyGlobalCells(ptd0, ptd1);
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit > (CELL **) H) {
    to_visit -= 5;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
    *pt1 = (CELL)to_visit[4];
    goto loop;
  }
  /* successful exit */
  return (TRUE);

cufail:
  /* failure */
  while (to_visit > (CELL **) H) {
    CELL *pt0;
    to_visit -= 5;
    pt0 = to_visit[0];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
    *pt1 = (CELL)to_visit[4];
  }
  /* failure */
  return (FALSE);
#if SHADOW_REGS
#if defined(B) || defined(TR)
#undef REGS
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
    if (rational_tree(d0))
      return(FALSE);
    return(TRUE);
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
	  return(mpz_cmp(BigIntOfTerm(AbsAppl(pt0)),BigIntOfTerm(AbsAppl(pt0))) == 0);
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
  if (pt1 < H0) WakeUp(pt1);
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
  if (pt0 < H0) WakeUp(pt0);
 bind_ocunify5:
#endif
  if (rational_tree(d1))
    return(FALSE);
  return (TRUE);

  deref_body(d1, pt1, oc_unify_var_unk, oc_unify_var_nvar);
  /* d0 and pt1 are unbound */
  UnifyCells(pt0, pt1, uc1, uc2);
#ifdef COROUTINING
 uc1:
  DO_TRAIL(pt0, (CELL)pt1);
  if (pt0 < H0) WakeUp(pt0);
#endif
  return (TRUE);
#ifdef COROUTINING
 uc2:
  DO_TRAIL(pt1, (CELL)pt0);
  if (pt1 < H0) {
    WakeUp(pt1);
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
  return(rational_tree(t));
}

static Int
p_acyclic(void)
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t))
    return(TRUE);
  return(!rational_tree(t));
}

int 
IUnify_complex(CELL *pt0, CELL *pt0_end, CELL *pt1)
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

  CELL  **to_visit  = (CELL **)AuxSp;

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
	to_visit -= 4;
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = (CELL *)d0;
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
	to_visit -= 4;
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = (CELL *)d0;
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
      goto cufail;

      derefa_body(d1, ptd1, unify_comp_nvar_unk, unify_comp_nvar_nvar);
	/* d1 and pt2 have the unbound value, whereas d0 is bound */
      BIND_GLOBALCELL(ptd1, d0);
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
      BIND_GLOBALCELL(ptd0, d1);

      derefa_body(d1, ptd1, unify_comp_var_unk, unify_comp_var_nvar);
      /* ptd0 and ptd1 are unbound */
      UnifyGlobalCells(ptd0, ptd1);
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit < (CELL **) AuxSp) {
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
  return (TRUE);

cufail:
#ifdef RATIONAL_TREES
  /* failure */
  while (to_visit < (CELL **) AuxSp) {
    CELL *pt0;
    pt0 = to_visit[0];
    *pt0 = (CELL)to_visit[3];
    to_visit += 4;
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
  BIND(pt1, d0, bind_unify3);
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
  BIND(pt0, d1, bind_unify4);
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

/**********************************************************************
 *                                                                    *
 *                 Conversion from Label to Op                        *
 *                                                                    *
 **********************************************************************/

#if USE_THREADED_CODE

static inline int
rtable_hash_op(OPCODE opc, int hash_mask) {
  return((((CELL)opc) >> 3) & hash_mask);
}

#define OP_HASH_SIZE 2048

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
    OP_RTABLE = (opentry *)AllocCodeSpace(OP_HASH_SIZE*sizeof(struct opcode_tab_entry));
  if (OP_RTABLE == NULL) {
    Error(FATAL_ERROR, TermNil,
	  "Couldn't obtain space for the reverse translation opcode table");
  }
  opeptr = OP_RTABLE;
  /* clear up table */
  {
    int j;
    for (j=0; j<=OP_HASH_SIZE; j++) {
      opeptr[j].opc = NIL;
      opeptr[j].opnum = _Ystop;
    }
  }
  opeptr = OP_RTABLE;
  opeptr[rtable_hash_op(opcode(_Ystop),hash_size_mask)].opc
	    = opcode(_Ystop);
  /* now place entries */
  for (i = _std_top; i > _Ystop; i--) {
    OPCODE opc = opcode(i);
    int j = rtable_hash_op(opc,hash_size_mask);

    while (opeptr[j].opc != NIL) {
      if (++j > hash_size_mask)
	j = 0;	  
    }
    /* clear entry, no conflict */
    opeptr[j].opnum = i;
    opeptr[j].opc = opc;
  }
}

/* given an opcode find the corresponding opnumber. This should make
   switches on ops a much easier operation */
op_numbers
op_from_opcode(OPCODE opc)
{
  int j = rtable_hash_op(opc,OP_HASH_SIZE-1);

  while (OP_RTABLE[j].opc != opc) {
    if (j == OP_HASH_SIZE-1)
      j = 0;
    else
      j++;
  }
  return(OP_RTABLE[j].opnum);
}
#else
op_numbers
op_from_opcode(OPCODE opc)
{
  return((op_numbers)opc);
}
#endif


/**********************************************************************
 *                                                                    *
 *                 Conversion from Op to Label                        *
 *                                                                    *
 **********************************************************************/

int 
iequ_complex(register CELL *pt0, register CELL *pt0_end,
	       register CELL *pt1
)
{
  register CELL **to_visit = (CELL **) H;

#ifdef RATIONAL_TREES
  register CELL *visited = AuxSp;

#endif

loop:
  while (pt0 < pt0_end) {
    register CELL *ptd0 = ++pt0; 
    register CELL d0 = *ptd0;

    ++pt1;
    deref_head(d0, eq_comp_unk);
  eq_comp_nvar:
    {
      register CELL *ptd1 = pt1;
      register CELL d1 = *ptd1;

      deref_head(d1, eq_comp_nvar_unk);
    eq_comp_nvar_nvar:
      if (d0 == d1)
	continue;
      else if (IsPairTerm(d0)) {
	if (!IsPairTerm(d1)) {
	  UNWIND_CUNIF();
	  return (FALSE);
	}
#ifdef RATIONAL_TREES
	/* now link the two structures so that no one else will */
	/* come here */
	if (d0 > d1) {
	  visited -= 2;
	  visited[0] = (CELL) pt0;
	  visited[1] = *pt0;
	  *pt0 = d1;
	}
	else {
	  visited -= 2;
	  visited[0] = (CELL) pt1;
	  visited[1] = *pt1;
	  *pt1 = d0;
	}
#endif
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = pt1;
	  to_visit += 3;
	}
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
	if (IsExtensionFunctor(f)) {
	  switch ((CELL)f) {
	  case (CELL)FunctorDBRef:
	    if (d0 == d1) continue;
	    UNWIND_CUNIF();
	    return (FALSE);
	  case (CELL)FunctorLongInt:
	    if (IsLongIntTerm(d1) && (Int)(ap2[1]) == LongIntOfTerm(d1)) continue;
	    UNWIND_CUNIF();
	    return (FALSE);
	  case (CELL)FunctorDouble:
	    if (IsFloatTerm(d1) && FloatOfTerm(d0) == FloatOfTerm(d1)) continue;
	    UNWIND_CUNIF();
	    return (FALSE);
#ifdef USE_GMP
	  case (CELL)FunctorBigInt:
	    if (IsBigIntTerm(d1) && mpz_cmp((MP_INT *)(ap2+1),BigIntOfTerm(d1)) == 0) continue;
	    UNWIND_CUNIF();
	    return (FALSE);
#endif /* USE_GMP */
	  default:
	    break;
	  }
	}
	if (!IsApplTerm(d1)) {
	  UNWIND_CUNIF();
	  return (FALSE);
	}
	ap3 = RepAppl(d1);
	/* compare functors */
	if (f != (Functor) *ap3) {
	  UNWIND_CUNIF();
	  return (FALSE);
	}
#ifdef RATIONAL_TREES
	/* now link the two structures so that no one else will */
	/* come here */
	if (d0 > d1) {
	  visited -= 2;
	  visited[0] = (CELL) pt0;
	  visited[1] = *pt0;
	  *pt0 = d1;
	}
	else {
	  visited -= 2;
	  visited[0] = (CELL) pt1;
	  visited[1] = *pt1;
	  *pt1 = d0;
	}
#endif
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = pt1;
	  to_visit += 3;
	}
	d0 = ArityOfFunctor(f);
	pt0 = ap2;
	pt0_end = ap2 + d0;
	pt1 = ap3;
	continue;
      } else {
	UNWIND_CUNIF();
	return (FALSE);
      }

      derefa_body(d1, ptd1, eq_comp_nvar_unk, eq_comp_nvar_nvar);
      /* d1 and pt2 have the unbound value, whereas d0 is bound */
      UNWIND_CUNIF();
      return (FALSE);
    }

    derefa_body(d0, ptd0, eq_comp_unk, eq_comp_nvar);
    {
      register CELL d1;
      register CELL *ptd1;

      d1 = *( ptd1 = pt1);
      /* pt2 is unbound */
      deref_head(d1, eq_comp_var_unk);
    eq_comp_var_nvar:
      /* pt2 is unbound and d1 is bound */
      UNWIND_CUNIF();
      return (FALSE);

      derefa_body(d1, ptd1, eq_comp_var_unk, eq_comp_var_nvar);
      /* pt2 and pt3 are unbound */
      if (ptd0 == ptd1)
	continue;
      UNWIND_CUNIF();
      return (FALSE);
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit > (CELL **) H) {
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    goto loop;
  }
  /* successful exit */
  UNWIND_CUNIF();
  return (TRUE);
}

static Int 
p_atom(void)
{				/* atom(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, atom_unk);
    atom_nvar:
      if (IsAtomTerm(d0)) {
	return(TRUE);
      }
      else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, atom_unk, atom_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

static Int 
p_atomic(void)
{				/* atomic(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, atomic_unk);
    atomic_nvar:
      if (IsAtomicTerm(d0)) {
	return(TRUE);
      }
      else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, atomic_unk, atomic_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

static Int 
p_integer(void)
{				/* integer(?,?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, integer_unk);
    integer_nvar:
      if (IsIntegerTerm(d0)) {
	return(TRUE);
      }
      else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, integer_unk, integer_nvar);
      ENDP(pt0);
      return(FALSE);
      ENDD(d0);
}

static Int 
p_number(void)
{				/* number(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, number_unk);
    number_nvar:
      if (IsNumTerm(d0)) {
	return(TRUE);
      }
      else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, number_unk, number_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

static Int 
p_db_ref(void)
{				/* db_reference(?,?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, db_ref_unk);
    db_ref_nvar:
      if (IsDBRefTerm(d0)) {
	return(TRUE);
      }
      else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, db_ref_unk, db_ref_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

static Int 
p_primitive(void)
{				/* primitive(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, primitive_unk);
    primitive_nvar:
      if (IsPrimitiveTerm(d0)) {
	return(TRUE);
      }
      else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, primitive_unk, primitive_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

static Int 
p_float(void)
{				/* float(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, float_unk);
    float_nvar:
      if (IsFloatTerm(d0)) {
	return(TRUE);
      }
      else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, float_unk, float_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

static Int 
p_compound(void)
{				/* compound(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, compound_unk);
    compound_nvar:
      if (IsPairTerm(d0)) {
	return(TRUE);
      }
      else if (IsApplTerm(d0)) {
	if (IsExtensionFunctor(FunctorOfTerm(d0))) {
	  return(FALSE);
	}
	return(TRUE);
      }
      else {
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d0, pt0, compound_unk, compound_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

static Int 
p_nonvar(void)
{				/* nonvar(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, nonvar_unk);
    nonvar_nvar:
      return(TRUE);
	
      BEGP(pt0);
      deref_body(d0, pt0, nonvar_unk, nonvar_nvar);
      return(FALSE);
      ENDP(pt0);
      ENDD(d0);
}

static Int 
p_var(void)
{				/* var(?)	 */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, var_unk);
    var_nvar:
      return(FALSE);

      BEGP(pt0);
      deref_body(d0, pt0, var_unk, var_nvar);
      return(TRUE);
      ENDP(pt0);
      ENDD(d0);
}

static Int 
p_equal(void)
{				/* ?=? */
  return(IUnify(ARG1, ARG2));
}

static Int 
p_eq(void)
{				/* ? == ? */
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, p_eq_unk1);
    p_eq_nvar1:
      /* first argument is bound */
      BEGD(d1);
      d1 = ARG2;
      deref_head(d1, p_eq_nvar1_unk2);
    p_eq_nvar1_nvar2:
      /* both arguments are bound */
      if (d0 == d1) {
	return(TRUE);
      }
      if (IsPairTerm(d0)) {
	if (!IsPairTerm(d1)) {
	  return(FALSE);
	}
	return(iequ_complex(RepPair(d0)-1, RepPair(d0)+1,RepPair(d1)-1));
      }
      if (IsApplTerm(d0)) {
	Functor f0 = FunctorOfTerm(d0);
	Functor f1;
	if (!IsApplTerm(d1)) {
	  return(FALSE);
	}
	f1 = FunctorOfTerm(d1);
	if (f0 != f1) {
	  return(FALSE);
	}
	if (IsExtensionFunctor(f0)) {
	  switch ((CELL)f0) {
	  case (CELL)FunctorDBRef:
	    return (d0 == d1);
	  case (CELL)FunctorLongInt:
	    return(LongIntOfTerm(d0) == LongIntOfTerm(d1));
#ifdef USE_GMP
	  case (CELL)FunctorBigInt:
	    return (mpz_cmp(BigIntOfTerm(d0), BigIntOfTerm(d1)) == 0);
#endif
	  case (CELL)FunctorDouble:
	    return(FloatOfTerm(d0) == FloatOfTerm(d1));
	  default:
	    return(FALSE);
	  }
	}
	return(iequ_complex(RepAppl(d0), RepAppl(d0)+ArityOfFunctor(f0), RepAppl(d1)));
      }
      return(FALSE);

      BEGP(pt0);
      deref_body(d1, pt0, p_eq_nvar1_unk2, p_eq_nvar1_nvar2);
      ENDP(pt0);
      /* first argument is bound */
      /* second argument is unbound */
      /* I don't need to worry about co-routining because an
	 unbound variable may never be == to a constrained variable!! */
      return(FALSE);
      ENDD(d1);

      BEGP(pt0);
      deref_body(d0, pt0, p_eq_unk1, p_eq_nvar1);
      BEGD(d1);
      d1 = ARG2;
      deref_head(d1, p_eq_var1_unk2);
    p_eq_var1_nvar2:
      /* I don't need to worry about co-routining because an
	 unbound variable may never be == to a constrained variable!! */
      return(FALSE);

      BEGP(pt1);
      deref_body(d1, pt1, p_eq_var1_unk2, p_eq_var1_nvar2);
      /* first argument is unbound */
      /* second argument is unbound */
      return(pt1 == pt0);
      ENDP(pt1);
      ENDD(d1);
      ENDP(pt0);

      ENDD(d0);
}

static Int 
p_dif(void)
{				/* ? \= ?  */
#if SHADOW_HB
      register CELL *HBREG = HB;
#endif
      BEGD(d0);
      BEGD(d1);
      d0 = ARG1;
      deref_head(d0, dif_unk1);
    dif_nvar1:
      /* first argument is bound */
      d1 = ARG2;
      deref_head(d1, dif_nvar1_unk2);
    dif_nvar1_nvar2:
      /* both arguments are bound */
      if (d0 == d1) {
	return(FALSE);
      }
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) {
	return(TRUE);
      }
      {
#ifdef COROUTINING
	/*
	 * We may wake up goals during our attempt to unify the
	 * two terms. If we are adding to the tail of a list of
	 * woken goals that should be ok, but otherwise we need
	 * to restore WokenGoals to its previous value.
	 */
	CELL OldWokenGoals = ReadTimedVar(WokenGoals);

#endif
	/* We will have to look inside compound terms */
	BEGP(pt0);
	/* store the old value of TR for clearing bindings */
	pt0 = (CELL *)TR;
	BEGCHO(pt1);
	pt1 = B;
	/* make B and HB point to H to guarantee all bindings will
	 * be trailed
	 */
	HBREG = H;
	B = (choiceptr) H;
	save_hb();
	if (IUnify(d0, d1) == TRUE) {
	  /* restore B, no need to restore HB */
	  B = pt1;
	  return(FALSE);
	}
	B = pt1;
	/* restore B, and later HB */
	ENDCHO(pt1);
	BEGP(pt1);
	/* untrail all bindings made by IUnify */
	while (TR != (tr_fr_ptr)pt0) {
	  pt1 = (CELL *) TrailTerm(--TR);
	  RESET_VARIABLE(pt1);
	}
	HBREG = B->cp_h;
	ENDP(pt1);
      }
#ifdef COROUTINING
      /* now restore Woken Goals to its old value */
      UpdateTimedVar(WokenGoals, OldWokenGoals);
#endif
      return(TRUE);
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d0, pt0, dif_unk1, dif_nvar1);
      ENDP(pt0);
      /* first argument is unbound */
      return(FALSE);

      BEGP(pt0);
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2);
      ENDP(pt0);
      /* second argument is unbound */
      return(FALSE);
      ENDD(d1);
      ENDD(d0);
}

static Int 
p_arg(void)
{				/* arg(?,?,?)	 */
#if SHADOW_HB
      register CELL *HBREG = HB;
#endif
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, arg_arg1_unk);
    arg_arg1_nvar:
      /* ARG1 is ok! */
      if (IsIntTerm(d0))
	d0 = IntOfTerm(d0);
      else if (IsLongIntTerm(d0)) {
	d0 = LongIntOfTerm(d0);
      } else {
	Error(TYPE_ERROR_INTEGER,d0,"arg 1 of arg/3");
	return(FALSE);
      }

      /* d0 now got the argument we want */
      BEGD(d1);
      d1 = ARG2;
      deref_head(d1, arg_arg2_unk);
    arg_arg2_nvar:
      /* d1 now got the structure we want to fetch the argument
       * from */
      if (IsApplTerm(d1)) {
	BEGP(pt0);
	pt0 = RepAppl(d1);
	d1 = *pt0;
	if (IsExtensionFunctor((Functor) d1)) {
	  return(FALSE);
	}
	save_hb();
	if ((Int)d0 <= 0 ||
	    (Int)d0 > ArityOfFunctor((Functor) d1) ||
	    IUnify(pt0[d0], ARG3) == FALSE) {
	  /* don't complain here for Prolog compatibility 
	  if ((Int)d0 <= 0) {
	    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
		  MkIntegerTerm(d0),"arg 1 of arg/3");	    
	  }
	  */
	  return(FALSE);
	}
	return(TRUE);
	ENDP(pt0);
      }
      else if (IsPairTerm(d1)) {
	BEGP(pt0);
	pt0 = RepPair(d1);
	if (d0 == 1) {
	  save_hb();
	  if (IUnify((CELL)pt0, ARG3) == FALSE) {
	    return(FALSE);
	  }
	  return(TRUE);
	}
	else if (d0 == 2) {
	  save_hb();
	  if (IUnify((CELL)(pt0+1), ARG3) == FALSE) {
	    return(FALSE);
	  }
	  return(TRUE);
	}
	else {
	  if ((Int)d0 < 0)
	    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
		  MkIntegerTerm(d0),"arg 1 of arg/3");	 
	  return(FALSE);
	}
	ENDP(pt0);
      }
      else {
	Error(TYPE_ERROR_COMPOUND, d1, "arg 2 of arg/3");
	return(FALSE);
      }

      BEGP(pt0);
      deref_body(d1, pt0, arg_arg2_unk, arg_arg2_nvar);
      Error(INSTANTIATION_ERROR,(CELL)pt0,"arg 2 of arg/3");;
      ENDP(pt0);
      return(FALSE);
      ENDD(d1);

      BEGP(pt0);
      deref_body(d0, pt0, arg_arg1_unk, arg_arg1_nvar);
      Error(INSTANTIATION_ERROR,(CELL)pt0,"arg 1 of arg/3");;
      ENDP(pt0);
      return(FALSE);
      ENDD(d0);

}

static Int
p_functor(void)			/* functor(?,?,?) */
{
#if SHADOW_HB
  register CELL *HBREG;
#endif

 restart:
#if SHADOW_HB
  HBREG = HB;
#endif
  BEGD(d0);
  d0 = ARG1;
  deref_head(d0, func_unk);
 func_nvar:
  /* A1 is bound */
  BEGD(d1);
  if (IsApplTerm(d0)) {
    d1 = *RepAppl(d0);
    if (IsExtensionFunctor((Functor) d1)) {
      if (d1 == (CELL)FunctorDouble) {
	d1 = MkIntTerm(0);
      } else if (d1 == (CELL)FunctorLongInt) {
	d1 = MkIntTerm(0);
      } else
	  return(FALSE);
    } else {
      d0 = MkAtomTerm(NameOfFunctor((Functor) d1));
      d1 = MkIntTerm(ArityOfFunctor((Functor) d1));
    }
  }
  else if (IsPairTerm(d0)) {
    d0 = TermDot;
    d1 = MkIntTerm(2);
  }
  else {
    d1 = MkIntTerm(0);
  }
  /* d1 and d0 now have the two arguments */
  /* let's go and bind them */
  {
    register CELL arity = d1;
    
    d1 = ARG2;
    deref_head(d1, func_nvar_unk);
  func_nvar_nvar:
    /* A2 was bound */
    if (d0 != d1) {
	return(FALSE);
    }
    /* have to buffer ENDP and label */
    d0 = arity;
    goto func_bind_x3;
    
    BEGP(pt0);
    deref_body(d1, pt0, func_nvar_unk, func_nvar_nvar);
    /* A2 is a variable, go and bind it */
    BIND(pt0, d0, bind_func_nvar_var);
#ifdef COROUTINING
    DO_TRAIL(pt0, d0);
    if (pt0 < H0) WakeUp(pt0);
  bind_func_nvar_var:
#endif
    /* have to buffer ENDP and label */
    d0 = arity;
    ENDP(pt0);
    /* now let's process A3 */

  func_bind_x3:
    d1 = ARG3;
    deref_head(d1, func_nvar3_unk);
  func_nvar3_nvar:
    /* A3 was bound */
    if (d0 != d1) {
	return(FALSE);
    }
    /* Done */
    return(TRUE);


    BEGP(pt0);
    deref_body(d1, pt0, func_nvar3_unk, func_nvar3_nvar);
    /* A3 is a variable, go and bind it */
    BIND(pt0, d0, bind_func_nvar3_var);
    /* Done */
#ifdef COROUTINING
    DO_TRAIL(pt0, d0);
    if (pt0 < H0) WakeUp(pt0);
  bind_func_nvar3_var:
#endif
    return(TRUE);

    ENDP(pt0);

  }
  ENDD(d1);

  BEGP(pt0);
  deref_body(d0, pt0, func_unk, func_nvar);
  /* A1 is a variable */
  /* We have to build the structure */
  d0 = ARG2;
  deref_head(d0, func_var_2unk);
 func_var_2nvar:
  /* we do, let's get the third argument */
  BEGD(d1);
  d1 = ARG3;
  deref_head(d1, func_var_3unk);
 func_var_3nvar:
  /* Uuuff, the second and third argument are bound */
  if (IsIntTerm(d1))
    d1 = IntOfTerm(d1);
  else {
    Error(TYPE_ERROR_INTEGER,ARG3,"functor/3");
    return(FALSE);
  }
  if (!IsAtomicTerm(d0)) {
    Error(TYPE_ERROR_ATOMIC,d0,"functor/3");
    return(FALSE);
  }
  /* We made it!!!!! we got in d0 the name, in d1 the arity and
   * in pt0 the variable to bind it to. */
  if (d0 == TermDot && d1 == 2) {
    RESET_VARIABLE(H);
    RESET_VARIABLE(H+1);
    d0 = AbsPair(H);
    H += 2;
  }
  else if ((Int)d1 > 0) {
    /* now let's build a compound term */
    if (!IsAtomTerm(d0)) {
      Error(TYPE_ERROR_ATOM,d0,"functor/3");
      return(FALSE);
    }
    BEGP(pt1);
    if (!IsAtomTerm(d0)) {
      return(FALSE);
    }
    else
      d0 = (CELL) MkFunctor(AtomOfTerm(d0), (Int) d1);
    pt1 = H;
    *pt1++ = d0;
    d0 = AbsAppl(H);
    if (pt1+d1 > ENV - CreepFlag) {
      gc(3, ENV, P);
      goto restart;
    }
    while (d1-- > 0) {
      RESET_VARIABLE(pt1);
      pt1++;
    }
    /* done building the term */
    H = pt1;
    ENDP(pt1);
  } else if ((Int)d1  < 0) {
    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
    return(FALSE);
  } 
  /* else if arity is 0 just pass d0 through */
  /* Ding, ding, we made it */
  BIND(pt0, d0, bind_func_var_3nvar);
#ifdef COROUTINING
  DO_TRAIL(pt0, d0);
  if (pt0 < H0) WakeUp(pt0);
 bind_func_var_3nvar:
#endif
  return(TRUE);


  BEGP(pt1);
  deref_body(d1, pt1, func_var_3unk, func_var_3nvar);
  Error(INSTANTIATION_ERROR,(CELL)pt1,"functor/3");
  ENDP(pt1);
  /* Oops, third argument was unbound */
  return(FALSE);
  ENDD(d1);

  BEGP(pt1);
  deref_body(d0, pt1, func_var_2unk, func_var_2nvar);
  Error(INSTANTIATION_ERROR,(CELL)pt1,"functor/3");
  ENDP(pt1);
  /* Oops, second argument was unbound too */
  return(FALSE);
  ENDP(pt0);
  ENDD(d0);
}

static Int
p_cut_by( void)
{
  BEGD(d0);
  d0 = ARG1;
  deref_head(d0, cutby_x_unk);
 cutby_x_nvar:
#if SBA
  if (!IsIntegerTerm(d0)) {
#else
  if (!IsIntTerm(d0)) {
#endif
    return(FALSE);
  }
  BEGCHO(pt0);
#if SBA
  pt0 = (choiceptr)IntegerOfTerm(d0);
#else
  pt0 = (choiceptr)(LCL0-IntOfTerm(d0));
#endif
  /* find where to cut to */
  if (pt0 > B) {
    /* Wow, we're gonna cut!!! */
#ifdef YAPOR
    CUT_prune_to(pt0);
#else
    B = pt0;
#endif /* YAPOR */
#ifdef TABLING
    abolish_incomplete_subgoals(B);
#endif /* TABLING */
    HB = B->cp_h;
    /*    trim_trail();*/
  }
  ENDCHO(pt0);
  return(TRUE);

  BEGP(pt0);
  deref_body(d0, pt0, cutby_x_unk, cutby_x_nvar);
  /* never cut to a variable */
  /* Abort */
  return(FALSE);
  ENDP(pt0);
  ENDD(d0);
}

static Int
p_erroneous_call(void)
{
  Error(SYSTEM_ERROR, TermNil, "bad call to internal built-in");
  return(FALSE);
}

void 
InitUnify(void)
{
  InitCPred("unify_with_occurs_check", 2, p_ocunify, SafePredFlag);
  InitCPred("cyclic_term", 1, p_cyclic, SafePredFlag|TestPredFlag);
  InitCPred("acyclic_term", 1, p_acyclic, SafePredFlag|TestPredFlag);
  InitAsmPred("$$cut_by", 1, _cut_by, p_cut_by, SafePredFlag);

  InitAsmPred("atom", 1, _atom, p_atom, SafePredFlag);
  InitAsmPred("atomic", 1, _atomic, p_atomic, SafePredFlag);
  InitAsmPred("integer", 1, _integer, p_integer, SafePredFlag);
  InitAsmPred("nonvar", 1, _nonvar, p_nonvar, SafePredFlag);
  InitAsmPred("number", 1, _number, p_number, SafePredFlag);
  InitAsmPred("var", 1, _var, p_var, SafePredFlag);
  InitAsmPred("db_reference", 1, _db_ref, p_db_ref, SafePredFlag);
  InitAsmPred("primitive", 1, _primitive, p_primitive, SafePredFlag);
  InitAsmPred("compound", 1, _compound, p_compound, SafePredFlag);
  InitAsmPred("float", 1, _float, p_float, SafePredFlag);
  InitAsmPred("=", 2, _equal, p_equal, SafePredFlag);
  InitAsmPred("\\=", 2, _dif, p_dif, SafePredFlag);
  InitAsmPred("==", 2, _eq, p_eq, SafePredFlag);
  InitAsmPred("arg", 3, _arg, p_arg, SafePredFlag);
  InitAsmPred("functor", 3, _functor, p_functor, SafePredFlag);
  InitAsmPred("$plus", 3, _plus, p_erroneous_call, SafePredFlag);
  InitAsmPred("$minus", 3, _minus, p_erroneous_call, SafePredFlag);
  InitAsmPred("$times", 3, _times, p_erroneous_call, SafePredFlag);
  InitAsmPred("$div", 3, _div, p_erroneous_call, SafePredFlag);
  InitAsmPred("$and", 3, _and, p_erroneous_call, SafePredFlag);
  InitAsmPred("$or", 3, _or, p_erroneous_call, SafePredFlag);
  InitAsmPred("$sll", 3, _sll, p_erroneous_call, SafePredFlag);
  InitAsmPred("$slr", 3, _slr, p_erroneous_call, SafePredFlag);
}


void 
InitAbsmi(void)
{
  /* initialise access to abstract machine instructions */
#if USE_THREADED_CODE
  absmi(1);
  InitReverseLookupOpcode();
#endif
}

