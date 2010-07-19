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

#if HAVE_STRING_H
#include <string.h>
#endif

#ifdef USE_GMP

#include "YapHeap.h"
#include "eval.h"
#include "alloc.h"

Term
Yap_MkBigIntTerm(MP_INT *big)
{
  Int nlimbs;
  MP_INT *dst = (MP_INT *)(H+2);
  CELL *ret = H;

  if (mpz_fits_slong_p(big)) {
    long int out = mpz_get_si(big);
    return MkIntegerTerm((Int)out);
  }
  nlimbs = (big->_mp_alloc)*(sizeof(mp_limb_t)/CellSize);
  if (nlimbs > (ASP-ret)-1024) {
    return TermNil;
  }
  H[0] = (CELL)FunctorBigInt;
  H[1] = BIG_INT;

  dst->_mp_size = big->_mp_size;
  dst->_mp_alloc = big->_mp_alloc;
  memmove((void *)(dst+1), (const void *)(big->_mp_d), nlimbs*CellSize);
  H = (CELL *)(dst+1)+nlimbs;
  H[0] = EndSpecials;
  H++;
  return AbsAppl(ret);
}

MP_INT *
Yap_BigIntOfTerm(Term t)
{
  MP_INT *new = (MP_INT *)(RepAppl(t)+2);

  new->_mp_d = (mp_limb_t *)(new+1);
  return(new);
}

Term
Yap_MkBigRatTerm(MP_RAT *big)
{
  Int nlimbs;
  MP_INT *dst = (MP_INT *)(H+2);
  MP_INT *num = mpq_numref(big);
  MP_INT *den = mpq_denref(big);
  MP_RAT *rat;
  CELL *ret = H;

  if (mpz_cmp_si(den, 1) == 0)
    return Yap_MkBigIntTerm(num);
  if ((num->_mp_alloc+den->_mp_alloc)*(sizeof(mp_limb_t)/CellSize) > (ASP-ret)-1024) {
    return TermNil;
  }
  H[0] = (CELL)FunctorBigInt;
  H[1] = BIG_RATIONAL;
  dst->_mp_size = 0;
  rat = (MP_RAT *)(dst+1);
  rat->_mp_num._mp_size = num->_mp_size;
  rat->_mp_num._mp_alloc = num->_mp_alloc;
  nlimbs = (num->_mp_alloc)*(sizeof(mp_limb_t)/CellSize);
  memmove((void *)(rat+1), (const void *)(num->_mp_d), nlimbs*CellSize);
  rat->_mp_den._mp_size = den->_mp_size;
  rat->_mp_den._mp_alloc = den->_mp_alloc;
  H = (CELL *)(rat+1)+nlimbs;
  nlimbs = (den->_mp_alloc)*(sizeof(mp_limb_t)/CellSize);
  memmove((void *)(H), (const void *)(den->_mp_d), nlimbs*CellSize);
  H += nlimbs;
  dst->_mp_alloc = (H-(CELL *)(dst+1));
  H[0] = EndSpecials;
  H++;
  return AbsAppl(ret);
}

MP_RAT *
Yap_BigRatOfTerm(Term t)
{
  MP_RAT *new = (MP_RAT *)(RepAppl(t)+2+sizeof(MP_INT)/sizeof(CELL));
  mp_limb_t *nt;

  nt = new->_mp_num._mp_d = (mp_limb_t *)(new+1);
  nt += new->_mp_num._mp_alloc;
  new->_mp_den._mp_d = nt;
  return new;
}

Term 
Yap_RatTermToApplTerm(Term t)
{
  Term ts[2];
  MP_RAT *rat = Yap_BigRatOfTerm(t);

  ts[0] =  Yap_MkBigIntTerm(mpq_numref(rat));
  ts[1] =  Yap_MkBigIntTerm(mpq_denref(rat));
  return Yap_MkApplTerm(FunctorRDiv,2,ts);
}


#endif

Term
Yap_MkULLIntTerm(YAP_ULONG_LONG n)
{
#if __GNUC__ && USE_GMP
    mpz_t new;
    char tmp[256];
    Term t;

#ifdef _WIN32
    snprintf(tmp,256,"%I64u",n);
#elif HAVE_SNPRINTF
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
  return(
	 IsNonVarTerm(t) && 
	 IsApplTerm(t) && 
	 FunctorOfTerm(t) == FunctorBigInt &&
	 RepAppl(t)[1] == BIG_INT
	 );
#else
  return FALSE;
#endif
}

static Int 
p_has_bignums(void)
{
#ifdef USE_GMP
  return TRUE;
#else
  return FALSE;
#endif
}

static Int 
p_is_rational(void)
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t))
    return FALSE;
  if (IsIntTerm(t))
    return TRUE;
  if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    CELL *pt;

    if (f == FunctorLongInt)
      return TRUE;
    if (f != FunctorBigInt)
      return FALSE;
    pt = RepAppl(t);
    return (  pt[1] == BIG_RATIONAL || pt[1] == BIG_INT );
  }
  return FALSE;
}

static Int 
p_rational(void)
{
#ifdef USE_GMP
  Term t = Deref(ARG1);
  Functor f;
  CELL *pt;
  MP_RAT *rat;
  Term t1, t2;

  if (IsVarTerm(t))
    return FALSE;
  if (!IsApplTerm(t))
    return FALSE;
  f = FunctorOfTerm(t);
  if (f != FunctorBigInt)
    return FALSE;
  pt = RepAppl(t);
  if (pt[1] != BIG_RATIONAL)
    return FALSE;
  rat = Yap_BigRatOfTerm(t);
  while ((t1 = Yap_MkBigIntTerm(mpq_numref(rat))) == TermNil ||
	 (t2 = Yap_MkBigIntTerm(mpq_denref(rat))) == TermNil) {
    UInt size =
      (mpq_numref(rat)->_mp_alloc)*(sizeof(mp_limb_t)/CellSize) +
      (mpq_denref(rat)->_mp_alloc)*(sizeof(mp_limb_t)/CellSize);
    if (!Yap_gcl(size, 3, ENV, P)) {
      Yap_Error(OUT_OF_STACK_ERROR, t, Yap_ErrorMessage);
      return FALSE;
    }
  }
  return 
    Yap_unify(ARG2, t1) &&
    Yap_unify(ARG3, t2);
#else
  return FALSE;
#endif
}

Term
Yap_MkBlobStringTerm(const char *s, size_t len)
{
  CELL *ret = H;
  size_t sz;
  MP_INT *dst = (MP_INT *)(H+2);
  blob_string_t *sp;
  size_t siz;

  sz = strlen(s);
  if (len > 0 && sz > len) sz = len;
  if (len/sizeof(CELL) > (ASP-ret)-1024) {
    return TermNil;
  }
  H[0] = (CELL)FunctorBigInt;
  H[1] = BLOB_STRING;

  siz = (sizeof(size_t)+len+sizeof(CELL))/sizeof(CELL);
  dst->_mp_size = siz;
  dst->_mp_alloc = 0L;
  sp = (blob_string_t *)(dst+1);
  sp->len = sz;
  strncpy((char *)(sp+1), s, sz);
  H += siz;
  H[0] = EndSpecials;
  H++;
  return AbsAppl(ret);
}

Term
Yap_MkBlobWideStringTerm(const wchar_t *s, size_t len)
{
  CELL *ret = H;
  size_t sz;
  MP_INT *dst = (MP_INT *)(H+2);
  blob_string_t *sp;
  size_t siz;

  sz = wcslen(s);
  if (len > 0 && sz > len) sz = len;
  if (len/sizeof(CELL) > (ASP-ret)-1024) {
    return TermNil;
  }
  H[0] = (CELL)FunctorBigInt;
  H[1] = BLOB_WIDE_STRING;

  siz = (sizeof(size_t)+(len+2)*sizeof(wchar_t))/sizeof(CELL);
  dst->_mp_size = siz;
  dst->_mp_alloc = 0L;
  sp = (blob_string_t *)(dst+1);
  sp->len = sz;
  wcsncpy((wchar_t *)(sp+1), s, sz);
  H += siz;
  H[0] = EndSpecials;
  H++;
  return AbsAppl(ret);
}

char *
Yap_BlobStringOfTerm(Term t)
{
  blob_string_t *new = (blob_string_t *)(RepAppl(t)+2+sizeof(MP_INT)/sizeof(CELL));
  return (char *)(new+1);
}

wchar_t *
Yap_BlobWideStringOfTerm(Term t)
{
  blob_string_t *new = (blob_string_t *)(RepAppl(t)+2+sizeof(MP_INT)/sizeof(CELL));
  return (wchar_t *)(new+1);
}

char *
Yap_BlobStringOfTermAndLength(Term t, size_t *sp)
{
  blob_string_t *new = (blob_string_t *)(RepAppl(t)+2+sizeof(MP_INT)/sizeof(CELL));
  *sp = new->len;
  return (char *)(new+1);
}

void
Yap_InitBigNums(void)
{
  Yap_InitCPred("$has_bignums", 0, p_has_bignums, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$bignum", 1, p_is_bignum, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("rational", 3, p_rational, 0);
  Yap_InitCPred("rational", 1, p_is_rational, SafePredFlag);
}
