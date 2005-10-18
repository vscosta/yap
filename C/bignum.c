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
* File:		arith1.c						 *
* Last rev:								 *
* mods:									 *
* comments:	bignum support through gmp				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

#include "Yap.h"
#include "Yatom.h"

#ifdef USE_GMP

#include "Heap.h"
#include "eval.h"
#include "alloc.h"
#if HAVE_STRING_H
#include <string.h>
#endif

/* This global variable tells how things are going */

static CELL *pre_alloc_base = NULL, *alloc_ptr;

/* This is a trivial allocator that use the global space:

   Each unit has a:
     size;
     nof elements;
 */
static void *
AllocBigNumSpace(size_t size)
{
  void *ret = (void *)(alloc_ptr+1);

  if (pre_alloc_base == NULL) {
    return((void *)malloc(size));
  }
  size = AdjustSize(size)/CellSize;
  alloc_ptr[0] = size;
  alloc_ptr += size+1;
  return(ret);
}

static void *
ReAllocBigNumSpace(void *optr, size_t osize, size_t size)
{
  void *out;

  if (pre_alloc_base == NULL) {
    return((void *)realloc(optr,size));
  }
  size = AdjustSize(size)/CellSize;
  osize = AdjustSize(osize)/CellSize;
  if (((CELL *)optr)+osize == alloc_ptr) {
    alloc_ptr += (size-osize);
    ((CELL *)optr)[-1] = size;
    return(optr);
  }
  out = AllocBigNumSpace(size);
  memcpy(out, (const void *)optr, size*CellSize);
  return(out);
}

static void
FreeBigNumSpace(void *optr, size_t size)
{
  CELL *bp = (CELL *)optr;

  if (pre_alloc_base == NULL) {
    free(optr);
    return;
  }
  size = AdjustSize(size)/CellSize;
  if (bp+size == alloc_ptr) {
    alloc_ptr = bp-1;
    return;
  }
  /* just say it is free */
  bp[-1] = -bp[-1];
}

MP_INT *
Yap_PreAllocBigNum(void)
{
  MP_INT *ret;

#ifdef USE_GMP
  /* YAP style memory allocation */
  mp_set_memory_functions(
			  AllocBigNumSpace,
			  ReAllocBigNumSpace,
			  FreeBigNumSpace);
#endif
  if (pre_alloc_base != H) {
    /* inform where we are allocating */
    alloc_ptr = pre_alloc_base = H;
  }
  ret = (MP_INT *)(alloc_ptr+1);
  /* first reserve space for the functor */
  alloc_ptr[0] = 0L;
  /* now allocate space for mpz_t */
  alloc_ptr = (CELL *)(ret+1);
  /* initialise the fields */
  mpz_init(ret);
  return(ret);
}

void
Yap_CleanBigNum(void)
{
  H = pre_alloc_base;
  pre_alloc_base = NULL;
}

MP_INT *
Yap_InitBigNum(Int in)
{
  MP_INT *ret;

  if (pre_alloc_base == NULL) {
    /* inform where we are allocating */
    alloc_ptr = pre_alloc_base = H;
  }
  ret = (MP_INT *)(alloc_ptr+1);
  /* first reserve space for the functor */
  /* I use a 0 to indicate this is the first time
     we are building the bignum */
  alloc_ptr[0] = 0L;
  /* now allocate space for mpz_t */
  alloc_ptr = (CELL *)(ret+1);
  /* initialise the fields */
  mpz_init_set_si(ret, in);
  return(ret);
}

/* This can be done in several different situations:
 - we did BigIntOf and want to recover now (check through ret[0]);
 - we have done PreAlloc() and then a lot happened in between:
   o our final computation fits in an Int;
   o our final computation is the first we PreAlloced();
   o our final computation is not the first term we PreAlloced();

  The result may be an Int, the old BigInt, or a BigInt moved to
  pre_alloc_base; 
*/
Term
Yap_MkBigIntTerm(MP_INT *big)
{
  CELL *new = (CELL *)(big+1);
  Int nlimbs = (big->_mp_alloc)*(sizeof(mp_limb_t)/CellSize);
  Int sz;
  CELL *ret = ((CELL *)big)-1;

  sz = mpz_sizeinbase(big, 2);
  /* was already there */
  if (ret[0] == (CELL)FunctorBigInt) {
    /* don't need to do no nothing */
    return(AbsAppl(ret));
  }
  if (sz < SIZEOF_LONG_INT*8-1) {
    Int out;
    
    H = pre_alloc_base;
    pre_alloc_base = NULL;
    out = mpz_get_si(big);
    return(MkIntegerTerm(out));
  } else {
    /* we may have created a lot of bignums since we did the first
       PreAlloc().  We want to recover the space, not leave "holes" on
       the global stack */
    if (pre_alloc_base != ret) {
      /* copy everything to the start of the temp area */
      MP_INT *dst = (MP_INT *)(pre_alloc_base+1);

      dst->_mp_size = big->_mp_size;
      dst->_mp_alloc = big->_mp_alloc;
      new = (CELL *)(dst+1);
      ret = pre_alloc_base;
    }
    ret[0] = (CELL)FunctorBigInt;
    memmove((void *)new, (const void *)(big->_mp_d), nlimbs*CellSize);
    H = (CELL *)(new+nlimbs);
    if ((char *)H-(char *)ret > MAX_SPECIALS_TAG-EndSpecials) {
      /* too large */
      return TermNil;
    }
#if GC_NO_TAGS
    H[0] = (H-ret)*sizeof(CELL)+EndSpecials;
#else
    H[0] = ((H-ret)*sizeof(CELL)+EndSpecials)|MBIT;
#endif
    H++;
    pre_alloc_base = NULL;
    return(AbsAppl(ret));
  }
}

MP_INT *
Yap_BigIntOfTerm(Term t)
{
  MP_INT *new = (MP_INT *)(RepAppl(t)+1);

  new->_mp_d = (mp_limb_t *)(new+1);
  return(new);
}

#endif

Term
Yap_MkULLIntTerm(YAP_ULONG_LONG n)
{
#ifdef __GNUC__
  if (n != (UInt)n) {
#if USE_GMP
    /* try to scan it as a bignum */
    MP_INT *new = Yap_PreAllocBigNum();
    
    mpz_init(new);
    mpz_set_ui(new, n>>32);
    mpz_mul_2exp(new, new, 32);
    mpz_add_ui(new, new, (UInt)n);
    return(Yap_MkBigIntTerm(new));
#else
    return(MkFloatTerm(n));
#endif
  } else {
    return MkIntegerTerm(n);
  }
#else
  return MkIntegerTerm(n);
#endif
}

static Int 
p_is_bignum(void)
{
#ifdef USE_GMP
  Term t = Deref(ARG1);
  return(IsNonVarTerm(t) && IsApplTerm(t) && FunctorOfTerm(t) == FunctorBigInt);
#else
  return(FALSE);
#endif
}

void
Yap_InitBigNums(void)
{
  Yap_InitCPred("$bignum", 1, p_is_bignum, SafePredFlag|HiddenPredFlag);
}
