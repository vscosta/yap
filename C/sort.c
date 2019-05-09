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
* File:		sort.c							 *
* Last rev:								 *
* mods:									 *
* comments:	sorting in Prolog					 *
*									 *
*************************************************************************/

/* for the moment, follow Prolog's traditional mergesort */

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#ifndef NULL
#define NULL (void *)0
#endif

/* fill in the even or the odd elements */
#define M_EVEN  0
#define M_ODD   1

static Int build_new_list(CELL *, Term CACHE_TYPE);
static void simple_mergesort(CELL *, Int, int);
static Int compact_mergesort(CELL *, Int, int);
static int key_mergesort(CELL *, Int, int, Functor);
static void adjust_vector(CELL *, Int);
static Int p_sort( USES_REGS1 );
static Int p_msort( USES_REGS1 );
static Int p_ksort( USES_REGS1 );

/* copy to a new list of terms */
static Int
build_new_list(CELL *pt, Term t USES_REGS)
{
  Int out = 0;
  if (IsVarTerm(t))
    return(-1);
  if (t == TermNil)
    return(0);
 restart:
  while (IsPairTerm(t)) {
    out++;
    pt[0] = HeadOfTerm(t);
    t = TailOfTerm(t);
    if (IsVarTerm(t))
      return(-1);
    if (t == TermNil) {
      return(out);
    }
    pt += 2;
    if (pt > ASP - 4096) {
      if (!Yap_gcl((ASP-HR)*sizeof(CELL), 2, ENV, gc_P(P,CP))) {
	Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
	return(FALSE);
      }
      t = Deref(ARG1);
      pt = HR;
      out = 0;
      goto restart;
    }
  }
  return(-1);
}

/* copy to a new list of terms */
static
void simple_mergesort(CELL *pt, Int size, int my_p)
{

  if (size > 2) {
    Int half_size = size / 2;
    CELL *pt_left, *pt_right, *end_pt, *end_pt_left;
    int left_p, right_p;

    pt_right = pt + half_size*2;
    left_p = my_p^1;
    right_p = my_p;
    simple_mergesort(pt, half_size, left_p);
    simple_mergesort(pt_right, size-half_size, right_p);
    /* now implement a simple merge routine */
    
    /* pointer to after the end of the list */
    end_pt = pt + 2*size;
    /* pointer to the element after the last element to the left */
    end_pt_left = pt+half_size*2;
    /* where is left list */
    pt_left = pt+left_p;
    /* where is right list */
    pt_right += right_p;
    /* where is new list */
    pt += my_p;
    /* while there are elements in the left or right vector do compares */
    while (pt_left < end_pt_left && pt_right < end_pt) {
      /* if the element to the left is larger than the one to the right */
      if (Yap_compare_terms(pt_left[0], pt_right[0]) <= 0) {
	/* copy the one to the left */
	pt[0] = pt_left[0];
	/* and avance the two pointers */
	pt += 2;
	pt_left += 2;
      } else {
	/* otherwise, copy the one to the right */
	pt[0] = pt_right[0];
	pt += 2;
	pt_right += 2;
      }
    }
    /* if any elements were left in the left vector just copy them */
    while (pt_left < end_pt_left) {
      pt[0] = pt_left[0];
      pt += 2;
      pt_left += 2;
    }
    /* if any elements were left in the right vector
       and they are in the wrong place, just copy them */
    if (my_p != right_p) {
      while(pt_right < end_pt) {
	pt[0] = pt_right[0];
	pt += 2;
	pt_right += 2;
      }
    }
  } else {
    if (size > 1 && (Yap_compare_terms(pt[0],pt[2]) > 0)) {
      CELL t = pt[2];
      pt[2+my_p] = pt[0];
      pt[my_p] = t;
    } else if (my_p) {
      pt[1] = pt[0];
      if (size > 1)
	pt[3] = pt[2];
    }
  }
}

/* copy to a new list of terms */
static
int key_mergesort(CELL *pt, Int size, int my_p, Functor FuncDMinus)
{

  if (size > 2) {
    Int half_size = size / 2;
    CELL *pt_left, *pt_right, *end_pt, *end_pt_left;
    int left_p, right_p;

    pt_right = pt + half_size*2;
    left_p = my_p^1;
    right_p = my_p;
    if (!key_mergesort(pt, half_size, left_p, FuncDMinus))
      return(FALSE);
    if (!key_mergesort(pt_right, size-half_size, right_p, FuncDMinus))
      return(FALSE);
    /* now implement a simple merge routine */
    
    /* pointer to after the end of the list */
    end_pt = pt + 2*size;
    /* pointer to the element after the last element to the left */
    end_pt_left = pt+half_size*2;
    /* where is left list */
    pt_left = pt+left_p;
    /* where is right list */
    pt_right += right_p;
    /* where is new list */
    pt += my_p;
    /* while there are elements in the left or right vector do compares */
    while (pt_left < end_pt_left && pt_right < end_pt) {
      /* if the element to the left is larger than the one to the right */
      Term t0 = pt_left[0] , t1 = pt_right[0];
      if (IsVarTerm(t0) || !IsApplTerm(t0) || FunctorOfTerm(t0) != FuncDMinus)
	return(FALSE);
      t0 = ArgOfTerm(1,t0);
      if (IsVarTerm(t1) || !IsApplTerm(t1) || FunctorOfTerm(t1) != FuncDMinus)
	return(FALSE);
      t1 = ArgOfTerm(1,t1);
      if (Yap_compare_terms(t0, t1) <= 0) {
	/* copy the one to the left */
	pt[0] = pt_left[0];
	/* and avance the two pointers */
	pt += 2;
	pt_left += 2;
      } else {
	/* otherwise, copy the one to the right */
	pt[0] = pt_right[0];
	pt += 2;
	pt_right += 2;
      }
    }
    /* if any elements were left in the left vector just copy them */
    while (pt_left < end_pt_left) {
      pt[0] = pt_left[0];
      pt += 2;
      pt_left += 2;
    }
    /* if any elements were left in the right vector
       and they are in the wrong place, just copy them */
    if (my_p != right_p) {
      while(pt_right < end_pt) {
	pt[0] = pt_right[0];
	pt += 2;
	pt_right += 2;
      }
    }
  } else {
    if (size > 1) {
      Term t0 = pt[0], t1 = pt[2];
      if (IsVarTerm(t0) || !IsApplTerm(t0) || FunctorOfTerm(t0) != FuncDMinus)
	return(FALSE);
      t0 = ArgOfTerm(1,t0);
      if (IsVarTerm(t1) || !IsApplTerm(t1) || FunctorOfTerm(t1) != FuncDMinus)
	return(FALSE);
      t1 = ArgOfTerm(1,t1);
      if (Yap_compare_terms(t0,t1) > 0) {
	CELL t = pt[2];
	pt[2+my_p] = pt[0];
	pt[my_p] = t;
      } else if (my_p) {
	pt[1] = pt[0];
	pt[3] = pt[2];
      }
    } else {
      if (my_p) 
	pt[1] = pt[0];
    }
  }
  return(TRUE);
}

/* copy to a new list of terms and compress duplicates */
static
Int compact_mergesort(CELL *pt, Int size, int my_p)
{

  if (size > 2) {
    Int half_size = size / 2;
    CELL *pt_left, *pt_right, *end_pt_right, *end_pt_left;
    int left_p, right_p;
    Int lsize, rsize;

    pt_right = pt + half_size*2;
    left_p = my_p^1;
    right_p = my_p;
    lsize = compact_mergesort(pt, half_size, left_p);
    rsize = compact_mergesort(pt_right, size-half_size, right_p);
    /* now implement a simple merge routine */
    
    /* where is left list */
    pt_left = pt+left_p;
    /* pointer to the element after the last element to the left */
    end_pt_left = pt+2*lsize;
    /* where is right list */
    pt_right += right_p;
    /* pointer to after the end of the list */
    end_pt_right = pt_right + 2*rsize;
    /* where is new list */
    pt += my_p;
    size = 0;
    /* while there are elements in the left or right vector do compares */
    while (pt_left < end_pt_left && pt_right < end_pt_right) {
      /* if the element to the left is larger than the one to the right */
      Int cmp = Yap_compare_terms(pt_left[0], pt_right[0]);
      if (cmp < (Int)0) {
	/* copy the one to the left */
	pt[0] = pt_left[0];
	/* and avance the two pointers */
	pt += 2;
	size ++;
	pt_left += 2;
      } else if (cmp == (Int)0) {
	/* otherwise, just skip one of them, anyone */
	pt_left += 2;
      } else {
	/* otherwise, copy the one to the right */
	pt[0] = pt_right[0];
	pt += 2;
	pt_right += 2;
	size++;
      }
    }
    /* if any elements were left in the left vector just copy them */
    while (pt_left < end_pt_left) {
      pt[0] = pt_left[0];
      pt += 2;
      pt_left += 2;
      size++;
    }
    /* if any elements were left in the right vector
       and they are in the wrong place, just copy them */
    while(pt_right < end_pt_right) {
      pt[0] = pt_right[0];
      pt += 2;
      pt_right += 2;
      size++;
    }
    return(size);
  } else if (size == 2) {
    Int cmp = Yap_compare_terms(pt[0],pt[2]);
    if (cmp > 0) {
      /* swap */
      CELL t = pt[2];
      pt[2+my_p] = pt[0];
      pt[my_p] = t;
      return(2);
    } else if (cmp == 0) {
      if (my_p)
	pt[1] = pt[0];
      return(1);
    } else {
      if (my_p) {
	pt[1] = pt[0];
	pt[3] = pt[2];
      }
      return(2);
    }
  } else {
    /* size = 1 */
    if (my_p)
      pt[1] = pt[0];
    return(1);
  }
}

static void
adjust_vector(CELL *pt, Int size)
{
  /* the elements are where they should be */
  CELL *ptf = pt + 2*(size-1);
  pt ++;
  while (pt < ptf) {
    pt[0] = AbsPair(pt+1);
    pt += 2;
  }
  /* close the list */
  pt[0] = TermNil;
}

static Int
p_sort( USES_REGS1 )
{
  /* use the heap to build a new list */
  CELL *pt = HR;
  Term out;
  /* list size */
  Int size;
  size = build_new_list(pt, Deref(ARG1) PASS_REGS);
  if (size < 0)
    return(FALSE);
  if (size < 2)
     return(Yap_unify(ARG1, ARG2));
  pt = HR;            /* because of possible garbage collection */
  /* make sure no one writes on our temp data structure */
  HR += size*2;
  /* reserve the necessary space */
  size = compact_mergesort(pt, size, M_EVEN);
  /* reajust space */
  HR = pt+size*2;
  adjust_vector(pt, size);
  out = AbsPair(pt);
  return(Yap_unify(out, ARG2));
}

static Int
p_msort( USES_REGS1 )
{
  /* use the heap to build a new list */
  CELL *pt = HR;
  Term out;
  /* list size */
  Int size;
  size = build_new_list(pt, Deref(ARG1) PASS_REGS);
  if (size < 0)
    return(FALSE);
  if (size < 2)
     return(Yap_unify(ARG1, ARG2));
  pt = HR;            /* because of possible garbage collection */
  /* reserve the necessary space */
  HR += size*2;
  simple_mergesort(pt, size, M_EVEN);
  adjust_vector(pt, size);
  out = AbsPair(pt);
  return(Yap_unify(out, ARG2));
}

static Int
p_ksort( USES_REGS1 )
{
  /* use the heap to build a new list */
  CELL *pt = HR;
  Term out;
  /* list size */
  Int size;
  size = build_new_list(pt, Deref(ARG1) PASS_REGS);
  if (size < 0)
    return(FALSE);
  if (size < 2)
     return(Yap_unify(ARG1, ARG2));
  /* reserve the necessary space */
  pt = HR;            /* because of possible garbage collection */
  HR += size*2;
  if (!key_mergesort(pt, size, M_EVEN, FunctorMinus))
    return(FALSE);
  adjust_vector(pt, size);
  out = AbsPair(pt);
  return(Yap_unify(out, ARG2));
}

void 
Yap_InitSortPreds(void)
{
  Yap_InitCPred("$sort", 2, p_sort, 0);
  Yap_InitCPred("$msort", 2, p_msort, 0);
  Yap_InitCPred("$keysort", 2, p_ksort, 0);
}
