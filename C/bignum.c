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

#include "YapHeap.h"

#ifdef USE_GMP

#include "eval.h"
#include "alloc.h"

Term
Yap_MkBigIntTerm(MP_INT *big)
{
  CACHE_REGS
  Int nlimbs;
  MP_INT *dst = (MP_INT *)(H+2);
  CELL *ret = H;
  Int bytes;

  if (mpz_fits_slong_p(big)) {
    long int out = mpz_get_si(big);
    return MkIntegerTerm((Int)out);
  }
  //  bytes = big->_mp_alloc * sizeof(mp_limb_t);
  //  nlimbs = ALIGN_YAPTYPE(bytes,CELL)/CellSize;
  // this works, but it shouldn't need to do this...
  nlimbs = big->_mp_alloc;
  bytes = nlimbs*sizeof(CELL);
  if (nlimbs > (ASP-ret)-1024) {
    return TermNil;
  }
  H[0] = (CELL)FunctorBigInt;
  H[1] = BIG_INT;

  dst->_mp_size = big->_mp_size;
  dst->_mp_alloc = nlimbs*(CellSize/sizeof(mp_limb_t));
  memmove((void *)(dst+1), (const void *)(big->_mp_d), bytes);
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
  CACHE_REGS
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
Yap_AllocExternalDataInStack(CELL tag, size_t bytes)
{
  CACHE_REGS
  Int nlimbs;
  MP_INT *dst = (MP_INT *)(H+2);
  CELL *ret = H;

  nlimbs = ALIGN_YAPTYPE(bytes,CELL)/CellSize;
  if (nlimbs > (ASP-ret)-1024) {
    return TermNil;
  }
  H[0] = (CELL)FunctorBigInt;
  H[1] = tag;
  dst->_mp_size = 0;
  dst->_mp_alloc = nlimbs;
  H = (CELL *)(dst+1)+nlimbs;
  H[0] = EndSpecials;
  H++;
  if (tag != EXTERNAL_BLOB) {
    TrailTerm(TR) = AbsPair(ret);
    TR++;
  }
  return AbsAppl(ret);
}

int Yap_CleanOpaqueVariable(CELL *pt)
{
  CELL blob_info, blob_tag;
  MP_INT *blobp;
#ifdef DEBUG
  /* sanity checking */
  if (pt[0] != (CELL)FunctorBigInt) {
    Yap_Error(SYSTEM_ERROR, TermNil, "CleanOpaqueVariable bad call");
    return FALSE;
  }
#endif
  blob_tag = pt[1];
  if (blob_tag < USER_BLOB_START ||
      blob_tag >= USER_BLOB_END) {
    Yap_Error(SYSTEM_ERROR, AbsAppl(pt), "clean opaque: bad blob with tag " UInt_FORMAT ,blob_tag);
    return FALSE;
  }
  blob_info = blob_tag - USER_BLOB_START;
  if (!GLOBAL_OpaqueHandlers)
    return FALSE;
  blobp = (MP_INT *)(pt+2);
  if (!GLOBAL_OpaqueHandlers[blob_info].fail_handler)
    return TRUE;
  return (GLOBAL_OpaqueHandlers[blob_info].fail_handler)((void *)(blobp+1));
}

Opaque_CallOnWrite
Yap_blob_write_handler_from_slot(Int slot)
{
  CACHE_REGS
  CELL blob_info, blob_tag;
  Term t = Yap_GetFromSlot(slot PASS_REGS);
  CELL *pt = RepAppl(t);

#ifdef DEBUG
  /* sanity checking */
  if (pt[0] != (CELL)FunctorBigInt) {
    Yap_Error(SYSTEM_ERROR, TermNil, "CleanOpaqueVariable bad call");
    return FALSE;
  }
#endif
  blob_tag = pt[1];
  if (blob_tag < USER_BLOB_START ||
      blob_tag >= USER_BLOB_END) {
    Yap_Error(SYSTEM_ERROR, AbsAppl(pt), "clean opaque: bad blob with tag " UInt_FORMAT ,blob_tag);
    return FALSE;
  }
  blob_info = blob_tag - USER_BLOB_START;
  if (!GLOBAL_OpaqueHandlers) {
    return NULL;
  }
  return GLOBAL_OpaqueHandlers[blob_info].write_handler;
}

Opaque_CallOnGCMark
Yap_blob_gc_mark_handler(Term t)
{
  CELL blob_info, blob_tag;
  CELL *pt = RepAppl(t);

#ifdef DEBUG
  /* sanity checking */
  if (pt[0] != (CELL)FunctorBigInt) {
    Yap_Error(SYSTEM_ERROR, TermNil, "CleanOpaqueVariable bad call");
    return FALSE;
  }
#endif
  blob_tag = pt[1];
  if (blob_tag < USER_BLOB_START ||
      blob_tag >= USER_BLOB_END) {
    return NULL;
  }
  blob_info = blob_tag - USER_BLOB_START;
  if (!GLOBAL_OpaqueHandlers)
    return NULL;
  return GLOBAL_OpaqueHandlers[blob_info].gc_mark_handler;
}

Opaque_CallOnGCRelocate
Yap_blob_gc_relocate_handler(Term t)
{
  CELL blob_info, blob_tag;
  CELL *pt = RepAppl(t);

#ifdef DEBUG
  /* sanity checking */
  if (pt[0] != (CELL)FunctorBigInt) {
    Yap_Error(SYSTEM_ERROR, TermNil, "CleanOpaqueVariable bad call");
    return FALSE;
  }
#endif
  blob_tag = pt[1];
  if (blob_tag < USER_BLOB_START ||
      blob_tag >= USER_BLOB_END) {
    Yap_Error(SYSTEM_ERROR, AbsAppl(pt), "clean opaque: bad blob with tag " UInt_FORMAT ,blob_tag);
    return FALSE;
  }
  blob_info = blob_tag - USER_BLOB_START;
  if (!GLOBAL_OpaqueHandlers)
    return NULL;
  return GLOBAL_OpaqueHandlers[blob_info].gc_relocate_handler;
}

extern Int Yap_blob_tag_from_slot(Int slot)
{
  CACHE_REGS
  Term t = Yap_GetFromSlot(slot PASS_REGS);
  CELL *pt = RepAppl(t);

#ifdef DEBUG
  /* sanity checking */
  if (pt[0] != (CELL)FunctorBigInt) {
    Yap_Error(SYSTEM_ERROR, TermNil, "CleanOpaqueVariable bad call");
    return FALSE;
  }
#endif
  return pt[1];
}

void *
Yap_blob_info_from_slot(Int slot)
{
  CACHE_REGS
  MP_INT *blobp;
  Term t = Yap_GetFromSlot(slot PASS_REGS);
  CELL *pt = RepAppl(t);

#ifdef DEBUG
  /* sanity checking */
  if (pt[0] != (CELL)FunctorBigInt) {
    Yap_Error(SYSTEM_ERROR, TermNil, "CleanOpaqueVariable bad call");
    return FALSE;
  }
#endif
  if (!GLOBAL_OpaqueHandlers)
    return FALSE;
  blobp = (MP_INT *)(pt+2);
  return (void *)(blobp+1);
}

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
      CACHE_REGS
      return MkIntegerTerm(mpz_get_si(new));
    }
    t = Yap_MkBigIntTerm(new);
    mpz_clear(new);
    return t;
#else
    CACHE_REGS
    return MkIntegerTerm(n);
#endif
}

static Int 
p_is_bignum( USES_REGS1 )
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
p_nb_set_bit( USES_REGS1 )
{
#ifdef USE_GMP
  Term t = Deref(ARG1);
  Term ti = Deref(ARG2);
  Int i;

  if (!(
	 IsNonVarTerm(t) && 
	 IsApplTerm(t) && 
	 FunctorOfTerm(t) == FunctorBigInt &&
	 RepAppl(t)[1] == BIG_INT
	))
    return FALSE;
  if (!IsIntegerTerm(ti)) {
    return FALSE;
  }
  if (!IsIntegerTerm(ti)) {
    return FALSE;
  }
  i = IntegerOfTerm(ti);
  if (i < 0) {
    return FALSE;
  }
  Yap_gmp_set_bit(i, t);
  return TRUE;
#else
  return FALSE;
#endif
}

static Int 
p_has_bignums( USES_REGS1 )
{
#ifdef USE_GMP
  return TRUE;
#else
  return FALSE;
#endif
}

static Int 
p_is_opaque( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t))
    return FALSE;
  if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    CELL *pt;

    if (f != FunctorBigInt)
      return FALSE;
    pt = RepAppl(t);
    return (  pt[1] != BIG_RATIONAL || pt[1] != BIG_INT );
  }
  return FALSE;
}

static Int 
p_is_rational( USES_REGS1 )
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
p_rational( USES_REGS1 )
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
      Yap_Error(OUT_OF_STACK_ERROR, t, LOCAL_ErrorMessage);
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

int
Yap_IsStringTerm(Term t)
{
  CELL fl;
  if (IsVarTerm(t))
    return FALSE;
  if (!IsApplTerm(t))
    return FALSE;
  if (FunctorOfTerm(t) != FunctorBigInt)
    return FALSE;

  fl = RepAppl(t)[1];
  return fl == BLOB_STRING || fl == BLOB_WIDE_STRING;
}

int
Yap_IsWideStringTerm(Term t)
{
  CELL fl;
  if (IsVarTerm(t))
    return FALSE;
  if (!IsApplTerm(t))
    return FALSE;
  if (FunctorOfTerm(t) != FunctorBigInt)
    return FALSE;

  fl = RepAppl(t)[1];
  return fl == BLOB_WIDE_STRING;
}

Term
Yap_MkBlobStringTerm(const char *s, size_t len)
{
  CACHE_REGS
  CELL *ret = H;
  size_t sz;
  MP_INT *dst = (MP_INT *)(H+2);
  blob_string_t *sp;
  size_t siz;
  char *dest;

  sz = strlen(s);
  if (len > 0 && sz > len) sz = len;
  if (len/sizeof(CELL) > (ASP-ret)-1024) {
    return TermNil;
  }
  H[0] = (CELL)FunctorBigInt;
  H[1] = BLOB_STRING;
  siz = ALIGN_YAPTYPE((len+1+sizeof(blob_string_t)),CELL);
  dst->_mp_size = 0L;
  dst->_mp_alloc = siz/sizeof(mp_limb_t);
  sp = (blob_string_t *)(dst+1);
  sp->len = sz;
  dest = (char *)(sp+1);
  strncpy(dest, s, sz);
  dest[sz] = '\0';
  H += (siz + 2*sizeof(CELL)+sizeof(MP_INT)+sizeof(Functor))/sizeof(CELL);
  H[-1] = EndSpecials;
  return AbsAppl(ret);
}

Term
Yap_MkBlobWideStringTerm(const wchar_t *s, size_t len)
{
  CACHE_REGS
  CELL *ret = H;
  size_t sz;
  MP_INT *dst = (MP_INT *)(H+2);
  blob_string_t *sp = (blob_string_t *)(dst+1);
  size_t siz, i = 0;

  H[0] = (CELL)FunctorBigInt;
  dst->_mp_size = 0L;
  sz = wcslen(s);
  if (len > 0 && sz > len) {
    sz = len;
  }
  if ((len/sizeof(CELL)) > (ASP-ret)-1024) {
    return TermNil;
  }
  while (i < sz) {
    if (s[i++] >= 255) break;
  }
  if (i == sz) {
    /* we have a standard ascii string */
    char *target;
    size_t i = 0;

    H[1] = BLOB_STRING;
    siz = ALIGN_YAPTYPE((sz+1+sizeof(blob_string_t)),CELL);
    dst->_mp_alloc = siz/sizeof(mp_limb_t);
    sp->len = sz;
    target = (char *)(sp+1);
    for (i = 0 ; i < sz; i++) {
      target[i] = s[i];
    }
    target[sz] = '\0';
    H += (siz+2*sizeof(CELL)+sizeof(MP_INT)+sizeof(Functor))/sizeof(CELL);
  } else {
    wchar_t * target;

    H[1] = BLOB_WIDE_STRING;
    siz = ALIGN_YAPTYPE((sz+1)*sizeof(wchar_t)+sizeof(blob_string_t),CELL);
    dst->_mp_alloc = siz/sizeof(mp_limb_t);
    sp->len = sz;
    target = (wchar_t *)(sp+1); 
    wcsncpy(target, s, sz);
    target[sz] = '\0';
    H += (siz + 2*sizeof(CELL)+sizeof(MP_INT)+sizeof(Functor))/sizeof(CELL);
  }
  H[-1] = EndSpecials;
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
  Yap_InitCPred("$has_bignums", 0, p_has_bignums, SafePredFlag);
  Yap_InitCPred("$bignum", 1, p_is_bignum, SafePredFlag);
  Yap_InitCPred("rational", 3, p_rational, 0);
  Yap_InitCPred("rational", 1, p_is_rational, SafePredFlag);
  Yap_InitCPred("opaque", 1, p_is_opaque, SafePredFlag);
  Yap_InitCPred("nb_set_bit", 2, p_nb_set_bit, SafePredFlag);
}
