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

Term
Yap_MkBigIntTerm(MP_INT *big)
{
  Int nlimbs;
  MP_INT *dst = (MP_INT *)(H+1);
  CELL *ret = H;

  if (mpz_fits_slong_p(big)) {
    int out = mpz_get_si(big);
    mpz_clear(big);
    return MkIntegerTerm(out);
  }
  nlimbs = (big->_mp_alloc)*(sizeof(mp_limb_t)/CellSize);
  if (nlimbs > (ASP-ret)-1024) {
    mpz_clear(big);
    return TermNil;
  }
  H[0] = (CELL)FunctorBigInt;

  dst->_mp_size = big->_mp_size;
  dst->_mp_alloc = big->_mp_alloc;
  memmove((void *)(dst+1), (const void *)(big->_mp_d), nlimbs*CellSize);
  H = (CELL *)(dst+1)+nlimbs;
  if ((char *)H-(char *)ret > MAX_SPECIALS_TAG-EndSpecials) {
    /* too large */
    mpz_clear(big);
    return TermNil;
  }
#if GC_NO_TAGS
  H[0] = (H-ret)*sizeof(CELL)+EndSpecials;
#else
  H[0] = ((H-ret)*sizeof(CELL)+EndSpecials)|MBIT;
#endif
  H++;
  mpz_clear(big);
  return AbsAppl(ret);
}

Term
Yap_MkBigIntTermCopy(MP_INT *big)
{
  Int nlimbs;
  MP_INT *dst = (MP_INT *)(H+1);
  CELL *ret = H;

  if (mpz_fits_slong_p(big)) {
    int out = mpz_get_si(big);
    return MkIntegerTerm(out);
  }
  nlimbs = (big->_mp_alloc)*(sizeof(mp_limb_t)/CellSize);
  if (nlimbs > (ASP-ret)-1024) {
    return TermNil;
  }
  H[0] = (CELL)FunctorBigInt;

  dst->_mp_size = big->_mp_size;
  dst->_mp_alloc = big->_mp_alloc;
  memmove((void *)(dst+1), (const void *)(big->_mp_d), nlimbs*CellSize);
  H = (CELL *)(dst+1)+nlimbs;
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
  return AbsAppl(ret);
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
#if __GNUC__ && USE_GMP
    mpz_t new;
    char tmp[256];
    Term t;

#if HAVE_SNPRINTF
    snprintf(tmp,256,"%llu",n);
#else    
    sprintf(tmp,"%llu",n);
#endif
    /* try to scan it as a bignum */
    mpz_init_set_str (new, tmp, 10);
    if (mpz_fits_slong_p(new)) {
      return MkIntegerTerm(mpz_get_si(new));
    }
    t = Yap_MkBigIntTerm(new);
    mpz_clear(new);
    return t;
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
