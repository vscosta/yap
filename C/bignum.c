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
 * File:		arith1.c *
 * Last rev:								 *
 * mods: *
 * comments:	bignum support through gmp				 *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif
#include "Yap.h"
#include "Yatom.h"

#if HAVE_STRING_H
#include <string.h>

#endif

#include "YapHeap.h"
#include "YapText.h"

#ifdef USE_GMP

#include "YapEval.h"
#include "alloc.h"

// full size, includes functor and end-word.
size_t
SizeOfOpaqueTerm(Term *next, CELL cnext)
{
  size_t sz;
  switch (cnext) {
    {
      case (CELL)FunctorLongInt:
	sz = 3;
	break;
    }
  case (CELL)FunctorDouble:
    {
         sz =2+  SIZEOF_DOUBLE / SIZEOF_INT_P;
	 break;
      }
  case (CELL)FunctorString:
    {
       sz = 3 + next[1];
	 break;
    }
  case (CELL)FunctorBigInt:
    {
      
       sz = 3+(sizeof(MP_INT)+
	       ((MP_INT *)(next + 2))->_mp_alloc * sizeof(mp_limb_t)) /CellSize;
    }
	 break;
	 case (CELL)FunctorBlob:
    {
      CELL *ptr = next+1;
      CELL tag = ptr[0];
      size_t
	sz = 4+ptr[ 1];

      if (tag == setup_call_catcher_cleanup_tag )
	return 4;
      if (tag == MATRIX_INT || tag == MATRIX_FLOAT) {
	return 1+ /* functor*/
	   MAT_DIMS + 1+
	  ptr[MAT_NDIMS] +
	  ptr[MAT_SIZE]
	  +1;

	  }
      return sz;
   
    }
                   break;
  default:
                   return 1;
}
  /* if (!IsAtomTerm(next[sz-1])) */
  /*   return 0; */
  /* CELL *p = 	(CELL*)AtomOfTerm(next[sz-1]); */
  /* if ( p == next ) */
  /*   return sz; */
  return sz;
}



Term Yap_MkBigIntTerm(MP_INT *big) {
  CACHE_REGS
  Int nlimbs;
  MP_INT *dst = (MP_INT *)(HR + 2);
  CELL *ret = HR;
  Int bytes;

  if (mpz_fits_slong_p(big)) {
    long int out = mpz_get_si(big);
    return MkIntegerTerm((Int)out);
  }
  //  bytes = big->_mp_alloc * sizeof(mp_limb_t);
  //  nlimbs = ALIGN_YAPTYPE(bytes,CELL)/CellSize;
  // this works, but it shouldn't need to do this...
  nlimbs = big->_mp_alloc;
  bytes = nlimbs * sizeof(mp_limb_t);
  if (nlimbs > (ASP - ret) - 1024) {
    return TermNil;
  }
  HR[0] = (CELL)FunctorBigInt;
  HR[1] = BIG_INT;

  dst->_mp_size = big->_mp_size;
  dst->_mp_alloc = nlimbs * (CellSize / sizeof(mp_limb_t));
  memmove((void *)(dst + 1), (const void *)(big->_mp_d), bytes);
  HR = (CELL *)(dst + 1) + bytes/CellSize;
  HR[0] = CloseExtension(ret);
  HR++;
  return AbsAppl(ret);
}

MP_INT *Yap_BigIntOfTerm(Term t) {
  MP_INT *new = (MP_INT *)(RepAppl(t) + 2);

  new->_mp_d = (mp_limb_t *)(new + 1);
  return (new);
}

Term Yap_MkBigRatTerm(MP_RAT *big) {
  CACHE_REGS
  Int nlimbs;
  MP_INT *dst = (MP_INT *)(HR + 2);
  MP_INT *num = mpq_numref(big);
  MP_INT *den = mpq_denref(big);
  MP_RAT *rat;
  CELL *ret = HR;

  if (mpz_cmp_si(den, 1) == 0)
    return Yap_MkBigIntTerm(num);
  if ((num->_mp_alloc + den->_mp_alloc) * (sizeof(mp_limb_t) / CellSize) >
      (ASP - ret) - 1024) {
    return TermNil;
  }
  HR[0] = (CELL)FunctorBigInt;
  HR[1] = BIG_RATIONAL;
  dst->_mp_size = 0;
  rat = (MP_RAT *)(dst + 1);
  rat->_mp_num._mp_size = num->_mp_size;
  rat->_mp_num._mp_alloc = num->_mp_alloc;
  nlimbs = (num->_mp_alloc) * (sizeof(mp_limb_t) / CellSize);
  memmove((void *)(rat + 1), (const void *)(num->_mp_d), nlimbs * CellSize);
  rat->_mp_den._mp_size = den->_mp_size;
  rat->_mp_den._mp_alloc = den->_mp_alloc;
  HR = (CELL *)(rat + 1) + nlimbs;
  nlimbs = (den->_mp_alloc) * (sizeof(mp_limb_t) / CellSize);
  memmove((void *)(HR), (const void *)(den->_mp_d), nlimbs * CellSize);
  HR += nlimbs;
  dst->_mp_alloc = (HR - (CELL *)(dst + 1));
  HR[0] = CloseExtension(ret);
  HR++;
  return AbsAppl(ret);
}

MP_RAT *Yap_BigRatOfTerm(Term t) {
  MP_RAT *new = (MP_RAT *)(RepAppl(t) + 2 + sizeof(MP_INT) / sizeof(CELL));
  mp_limb_t *nt;

  nt = new->_mp_num._mp_d = (mp_limb_t *)(new + 1);
  nt += new->_mp_num._mp_alloc;
  new->_mp_den._mp_d = nt;
  return new;
}

Term Yap_RatTermToApplTerm(Term t) {
  Term ts[2];
  MP_RAT *rat = Yap_BigRatOfTerm(t);

  ts[0] = Yap_MkBigIntTerm(mpq_numref(rat));
  ts[1] = Yap_MkBigIntTerm(mpq_denref(rat));
  return Yap_MkApplTerm(FunctorRDiv, 2, ts);
}

#endif

Term Yap_AllocExternalDataaaInStack(size_t ncells) {
  CACHE_REGS
  CELL *ret = HR, *tmp = HR;

 // fprintf(stderr,"EW %% %p %lx\n",ret,bytes);

  if (ncells > (ASP - ret) - 1024) {
    return TermNil;
  }
  
  ret[0] = (CELL)FunctorBlob;
  HR = tmp+ ncells;
  HR[0] = CloseExtension((ret));
  HR++;
  return AbsAppl(ret);
}

Term Yap_AllocExternalDataInStack( size_t ncells)
{
  CACHE_REGS

  CELL *ret = HR, *tmp = HR+1;

 // fprintf(stderr,"EW %% %p %lx\n",ret,bytes);

  if (ncells > (ASP - ret) - 1024) {
    return TermNil;
  }
  
  ret[0] = (CELL)FunctorBlob;
  HR = tmp+ ncells;
  HR[0] = CloseExtension((ret));
  HR++;
  return AbsAppl(ret);
}


int Yap_CleanOpaqueVariable(CELL d) {
  CELL *pt = RepAppl(HeadOfTerm(d));
  CELL blob_tag = pt[1];

  //  fprintf(stderr,"FAIL %% %p %lx %lx %lx\n",pt,pt[0],pt[1],pt[2]);
#ifdef DEBUG
  /* sanity checking */
  if (pt[0] != (CELL)FunctorBlob) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "CleanOpaqueVariable bad call");
    return FALSE;
  }
#endif
  if (blob_tag < USER_BLOB_START || blob_tag >= USER_BLOB_END) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, AbsAppl(pt),
              "clean opaque: bad blob with tag " UInt_FORMAT, blob_tag);
    return FALSE;
  }
Int blob_info = blob_tag;
  if (!GLOBAL_OpaqueHandlers)
    return false;
  if (!GLOBAL_OpaqueHandlers[blob_info].fail_handler)
    return true;
  CACHE_REGS
  return (GLOBAL_OpaqueHandlers[blob_info].fail_handler)(d );
}

YAP_Opaque_CallOnWrite Yap_blob_write_handler(Term t) {
  CELL blob_info, blob_tag;
  CELL *pt = RepAppl(t);

#ifdef DEBUG
  /* sanity checking */
  if (pt[0] != (CELL)FunctorBlob) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "CleanOpaqueVariable bad call");
    return FALSE;
  }
#endif
  blob_tag = pt[1];
  if (blob_tag < USER_BLOB_START || blob_tag >= USER_BLOB_END) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, AbsAppl(pt),
              "clean opaque: bad blob with tag " UInt_FORMAT, blob_tag);
    return FALSE;
  }
  blob_info = blob_tag;
  if (!GLOBAL_OpaqueHandlers) {
    return NULL;
  }
  return GLOBAL_OpaqueHandlers[blob_info].write_handler;
}

YAP_Opaque_CallOnGCMark Yap_blob_gc_mark_handler(Term t) {
  CELL blob_info, blob_tag;
  CELL *pt = RepAppl(t);

#ifdef DEBUG0
  /* sanity checking */
  if (pt[0] != (CELL)FunctorBigInt) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "CleanOpaqueVariable bad call");
    return FALSE;
  }
#endif
  blob_tag = pt[1];
  if (blob_tag < USER_BLOB_START || blob_tag >= USER_BLOB_END) {
    return NULL;
  }
  blob_info = blob_tag;
  if (!GLOBAL_OpaqueHandlers)
    return NULL;
  return GLOBAL_OpaqueHandlers[blob_info].mark_handler;
}

YAP_Opaque_CallOnGCRelocate Yap_blob_gc_relocate_handler(Term t) {
  CELL blob_info, blob_tag;
  CELL *pt = RepAppl(t);

#ifdef DEBUG
  /* sanity checking */
  if (pt[0] != (CELL)FunctorBlob) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "CleanOpaqueVariable bad call");
    return FALSE;
  }
#endif
  blob_tag = pt[1];
  if (blob_tag < USER_BLOB_START || blob_tag >= USER_BLOB_END) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, AbsAppl(pt),
              "clean opaque: bad blob with tag " UInt_FORMAT, blob_tag);
    return FALSE;
  }
  blob_info = blob_tag;
  if (!GLOBAL_OpaqueHandlers)
    return NULL;
  return GLOBAL_OpaqueHandlers[blob_info].relocate_handler;
}

extern Int Yap_blob_tag(Term t) {
  CELL *pt = RepAppl(t);

#ifdef DEBUG
  /* sanity checking */
  if (pt[0] != (CELL)FunctorBlob) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "CleanOpaqueVariable bad call");
    return FALSE;
  }
#endif
  return pt[1];
}

void *Yap_blob_info(Term t) {

  CELL *pt = RepAppl(t);

#ifdef DEBUG
  /* sanity checking */
  if (pt[0] != (CELL)FunctorBlob) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "CleanOpaqueVariable bad call");
    return FALSE;
  }
#endif
  if (!GLOBAL_OpaqueHandlers)
    return FALSE;
  return (void *)( pt+ 1);
}

Term Yap_MkULLIntTerm(YAP_ULONG_LONG n) {
#if __GNUC__
  mpz_t new;
  char tmp[256];
  Term t;

#ifdef _WIN32
  snprintf(tmp, 256, "%I64u", n);
#elif HAVE_SNPRINTF
  snprintf(tmp, 256, "%llu", n);
#else
  sprintf(tmp, "%llu", n);
#endif
  /* try to scan it as a bignum */
  mpz_init_set_str(new, tmp, 10);
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

CELL *Yap_HeapStoreOpaqueTerm(Term t) {
  CELL *ptr = RepAppl(t);
  size_t sz;
  void *new;

  sz = SizeOfOpaqueTerm(ptr,ptr[0]);
  new = Yap_AllocCodeSpace(sz);
  if (!new) {
    Yap_Error(RESOURCE_ERROR_HEAP, TermNil,
              "subgoal_search_loop: no space for %s", StringOfTerm(t));
  } else {
    if (ptr[0] == (CELL)FunctorBlob) {
      CELL *new = ptr + 3;
    memmove(new, ptr, sz);
    }
  }
  return new;
}

size_t Yap_OpaqueTermToString(Term t, char *str, size_t max) {
  CACHE_REGS
  size_t str_index = 0;
  CELL *li = RepAppl(t);
  unsigned char *ptr = (unsigned char *)StringOfTerm(AbsAppl(li));
  if (li[0] == (CELL)FunctorString) {
    str_index += sprintf(&str[str_index], "\"");
    do {
      utf8proc_int32_t chr;
      ptr += get_utf8(ptr, -1, &chr);
      if (chr == '\0')
        break;
      str_index += sprintf(str + str_index, "%C", chr);
    } while (TRUE);
    str_index += sprintf(str + str_index, "\"");
  } else {
    CELL big_tag = li[1];

    if (big_tag == MATRIX_INT || big_tag == MATRIX_FLOAT) {
      str_index += sprintf(&str[str_index], "{...}");
#ifdef USE_GMP
    } else if (big_tag == BIG_INT) {
      MP_INT *big = Yap_BigIntOfTerm(AbsAppl(li));
      char *s = mpz_get_str(&str[str_index], 10, big);
      str_index += strlen(&s[str_index]);
    } else if (big_tag == BIG_RATIONAL) {
      MP_RAT *big = Yap_BigRatOfTerm(AbsAppl(li));
      char *s = mpq_get_str(&str[str_index], 10, big);
      str_index += strlen(&s[str_index]);
#endif
    }
    /*
      else if (big_tag >= USER_BLOB_START && big_tag < USER_BLOB_END) {
      Opaque_CallOnWrite f;
      CELL blob_info;

      blob_info = big_tag - USER_BLOB_START;
      if (GLOBAL_OpaqueHandlers &&
      (f= GLOBAL_OpaqueHandlers[blob_info].write_handler)) {
      (f)(wglb->stream, big_tag, ExternalBlobFromTerm(t), 0);
      return;
      }
      } */
    str_index += sprintf(&str[str_index], "0");
  }
  return str_index;
}

static Int p_is_bignum(USES_REGS1) {
#ifdef USE_GMP
  Term t = Deref(ARG1);
  return (IsNonVarTerm(t) && IsApplTerm(t) &&
          FunctorOfTerm(t) == FunctorBigInt && RepAppl(t)[1] == BIG_INT);
#else
  return FALSE;
#endif
}

static Int p_is_string(USES_REGS1) {
  Term t = Deref(ARG1);
  return (IsNonVarTerm(t) && IsApplTerm(t) &&
          FunctorOfTerm(t) == FunctorString);
}

static Int p_nb_set_bit(USES_REGS1) {
#ifdef USE_GMP
  Term t = Deref(ARG1);
  Term ti = Deref(ARG2);
  Int i;

  if (!(IsNonVarTerm(t) && IsApplTerm(t) && FunctorOfTerm(t) == FunctorBigInt &&
        RepAppl(t)[1] == BIG_INT))
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

static Int p_has_bignums(USES_REGS1) {
#ifdef USE_GMP
  return TRUE;
#else
  return FALSE;
#endif
}

static Int p_is_opaque(USES_REGS1) {
  Term t = Deref(ARG1);
  if (IsVarTerm(t))
    return FALSE;
  if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    CELL *pt;

    if (f != FunctorBigInt && f!= FunctorBlob)
      return FALSE;
    pt = RepAppl(t);
    return (pt[1] != BIG_RATIONAL && pt[1] != BIG_INT);
  }
  return FALSE;
}

static Int p_is_rational(USES_REGS1) {
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
    return (pt[1] == BIG_RATIONAL || pt[1] == BIG_INT);
  }
  return FALSE;
}

void * YAP_FetchArray(Term t1, intptr_t *sz, int *type)
{
  AtomEntry *ae = RepAtom(AtomOfTerm(t1));

  READ_LOCK(ae->ARWLock);
  StaticArrayEntry *p = RepStaticArrayProp(ae->PropsOfAE);
  while (!EndOfPAEntr(p) && p->KindOfPE != ArrayProperty){
      p = RepStaticArrayProp(p->NextOfPE);
}  READ_UNLOCK(ae->ARWLock);

    if (EndOfPAEntr(p)) {
      return NULL;
    }
    if (sz)
*sz = p->ArrayEArity;
if (p->ArrayType == 
     array_of_doubles)
  {
    *type = 'f';
    return p->ValueOfVE.floats;
  }
  if (p->ArrayType == 
     array_of_ints)
  {
    *type = 'i';
    return p->ValueOfVE.ints;

  }
return NULL;
}

#define MAT_DIMS 5
static Int fast_get_for_int_vector(USES_REGS1) //Term inp, size_t off, Term)
{
  Int *v = (Int*)RepAppl(Deref(ARG1))+(MAT_DIMS+1);
  return Yap_unify(MkIntegerTerm(v[IntOfTerm(Deref(ARG2))]), ARG3);
  }

static Int fast_set_for_int_vector(USES_REGS1) //Term inp, size_t off, Term)n
{
  Int *v = (Int*)RepAppl(Deref(ARG1))+(MAT_DIMS+1);
  v[IntOfTerm(Deref(ARG2))] = IntegerOfTerm(Deref(ARG3));
  return true;
  }

bool IS_MATRIX(Term inp) {
  inp = Deref(inp);
  if (IsVarTerm(inp))
    return false;
  if (IsApplTerm(inp)) {
    Functor f = FunctorOfTerm(inp);
    CELL *pt;

    if (f == FunctorBlob) {
      pt = RepAppl(inp);
      CELL big_tag = pt[1];
      return big_tag == MATRIX_INT || big_tag == MATRIX_FLOAT;
    }
      return
	f == FunctorMatrix ||
	f == FunctorFloats;
  } else if (IsAtomTerm(inp)) {
    intptr_t size;    int type; 
    if (YAP_FetchArray(inp, &size, &type)) {
      return true;
    }
    }
  return false;

}

static Int p_rational(USES_REGS1) {
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
    UInt size = (mpq_numref(rat)->_mp_alloc) * (sizeof(mp_limb_t) / CellSize) +
                (mpq_denref(rat)->_mp_alloc) * (sizeof(mp_limb_t) / CellSize);
    if (!Yap_dogcl(size PASS_REGS)) {
      Yap_Error(RESOURCE_ERROR_STACK, t, LOCAL_ErrorMessage);
      return FALSE;
    }
  }
  return Yap_unify(ARG2, t1) && Yap_unify(ARG3, t2);
#else
  return FALSE;
#endif
}

void Yap_InitBigNums(void) {
  Yap_InitCPred("$has_bignums", 0, p_has_bignums, SafePredFlag);
  Yap_InitCPred("$bignum", 1, p_is_bignum, SafePredFlag);
  Yap_InitCPred("rational", 3, p_rational, 0);
  Yap_InitCPred("rational", 1, p_is_rational, SafePredFlag);
  /** @pred  rational( _T_)


  Checks whether `T` is a rational number.


  */
  Yap_InitCPred("string", 1, p_is_string, SafePredFlag);
  Yap_InitCPred("opaque", 1, p_is_opaque, SafePredFlag);
  Yap_InitCPred("nb_set_bit", 2, p_nb_set_bit, SafePredFlag);
  Yap_InitCPred("fast_get_for_int_vector" ,3,fast_get_for_int_vector,SafePredFlag);
  Yap_InitCPred("fast_set_for_int_vector" ,3,fast_set_for_int_vector,SafePredFlag);
}
