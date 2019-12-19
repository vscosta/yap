/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		TermExt.h						 *
* mods:									 *
* comments:	Extensions to standard terms for YAP			 *
* version:      $Id: TermExt.h,v 1.15 2008-03-25 22:03:13 vsc Exp $	 *
*************************************************************************/

/**

@file TermExt.h

 */

#ifdef USE_SYSTEM_MALLOC
#define SF_STORE (&(Yap_heap_regs->funcs))
#else
#define SF_STORE ((special_functors *)HEAP_INIT_BASE)
#endif


#if 1
extern Atom AtomFoundVar, AtomFreeTerm, AtomNil, AtomDot;
#elif defined(USE_OFFSETS)
#define AtomFoundVar ((Atom)(&(((special_functors *)(NULL))->AtFoundVar)))
#define AtomFreeTerm ((Atom)(&(((special_functors *)(NULL))->AtFreeTerm)))
#define AtomNil ((Atom)(&(((special_functors *)(NULL))->AtNil)))
#define AtomDot ((Atom)(&(((special_functors *)(NULL))->AtDot)))
#elif OLD_STYLE_INITIAL_ATOMS
#define AtomFoundVar AbsAtom((AtomEntry *)&(SF_STORE->AtFoundVar))
#define AtomFreeTerm AbsAtom((AtomEntry *)&(SF_STORE->AtFreeTerm))
#define AtomNil AbsAtom((AtomEntry *)&(SF_STORE->AtNil))
#define AtomDot AbsAtom((AtomEntry *)&(SF_STORE->AtDot))
#else
#define AtomFoundVar AbsAtom(SF_STORE->AtFoundVar)
#define AtomFreeTerm AbsAtom(SF_STORE->AtFreeTerm)
#define AtomNil AbsAtom(SF_STORE->AtNil)
#define AtomDot AbsAtom(SF_STORE->AtDot)
#endif

#define TermFoundVar MkAtomTerm(AtomFoundVar)
#define TermFreeTerm MkAtomTerm(AtomFreeTerm)
#define TermNil MkAtomTerm(AtomNil)
#define TermDot MkAtomTerm(AtomDot)

typedef enum {
  db_ref_e = sizeof(Functor *),
  attvar_e = 2 * sizeof(Functor *),
  double_e = 3 * sizeof(Functor *),
  long_int_e = 4 * sizeof(Functor *),
  big_int_e = 5 * sizeof(Functor *),
  string_e = 6 * sizeof(Functor *)
} blob_type;

#define FunctorDBRef ((Functor)(db_ref_e))
#define FunctorAttVar ((Functor)(attvar_e))
#define FunctorDouble ((Functor)(double_e))
#define FunctorLongInt ((Functor)(long_int_e))
#define FunctorBigInt ((Functor)(big_int_e))
#define FunctorString ((Functor)(string_e))
#define EndSpecials (string_e + sizeof(Functor *))

#include "inline-only.h"

#define IsAttVar(pt) __IsAttVar((pt)PASS_REGS)

INLINE_ONLY int __IsAttVar(CELL *pt USES_REGS);

INLINE_ONLY int __IsAttVar(CELL *pt USES_REGS) {
#ifdef YAP_H
  return (pt)[-1] == (CELL)attvar_e && pt < HR;
#else
  return (pt)[-1] == (CELL)attvar_e;
#endif
}

INLINE_ONLY int GlobalIsAttVar(CELL *pt);

INLINE_ONLY int GlobalIsAttVar(CELL *pt) {
  return (pt)[-1] == (CELL)attvar_e;
}

typedef enum {
  BIG_INT = 0x01,
  BIG_RATIONAL = 0x02,
  BIG_FLOAT = 0x04,
  EMPTY_ARENA = 0x10,
  ARRAY_INT = 0x21,
  ARRAY_FLOAT = 0x22,
  CLAUSE_LIST = 0x40,
  EXTERNAL_BLOB = 0x0A0,    /* generic data */
  GOAL_CUT_POINT = 0x0A1,
  USER_BLOB_START = 0x0100, /* user defined blob */
  USER_BLOB_END = 0x0200    /* end of user defined blob */
} big_blob_type;

INLINE_ONLY blob_type BlobOfFunctor(Functor f);

INLINE_ONLY blob_type BlobOfFunctor(Functor f) {
  return (blob_type)((CELL)f);
}

#ifdef COROUTINING

typedef struct {
  /* what to do when someone tries to bind our term to someone else
     in some  predefined context */
  void (*bind_op)(Term *, Term CACHE_TYPE);
  /* what to do if someone wants to copy our constraint */
  int (*copy_term_op)(CELL *, void *i, CELL *CACHE_TYPE);
  /* copy the constraint into a term and back */
  Term (*to_term_op)(CELL *);
  int (*term_to_op)(Term, Term CACHE_TYPE);
  /* op called to do marking in GC */
  void (*mark_op)(CELL *);
} ext_op;

/* known delays */
typedef enum {
  empty_ext = 0 * sizeof(ext_op),  /* default op, this should never be called */
  attvars_ext = 1 * sizeof(ext_op) /* support for attributed variables */
                                   /* add your own extensions here */
                                   /* keep this one */
} exts;

#endif

#if defined(YAP_H)
/* make sure that these data structures are the first thing to be allocated
   in the heap when we start the system */
typedef struct special_functors_struct {

#if 0
  struct ExtraAtomEntryStruct AtFoundVar;
  struct ExtraAtomEntryStruct AtFreeTerm;
  struct ExtraAtomEntryStruct AtNil;
  struct ExtraAtomEntryStruct AtDot;
#else
  struct AtomEntryStruct *AtFoundVar;
  struct AtomEntryStruct *AtFreeTerm;
  struct AtomEntryStruct *AtNil;
  struct AtomEntryStruct *AtDot;
#endif
} special_functors;
#endif /* YAP_H */

INLINE_ONLY Float CpFloatUnaligned(CELL *ptr);

#define MkFloatTerm(fl) __MkFloatTerm((fl)PASS_REGS)

INLINE_ONLY Term __MkFloatTerm(Float USES_REGS);

INLINE_ONLY Float FloatOfTerm(Term t);

#if SIZEOF_DOUBLE == SIZEOF_INT_P

INLINE_ONLY Term __MkFloatTerm(Float dbl USES_REGS) {
  return (Term)((HR[0] = (CELL)FunctorDouble, *(Float *)(HR + 1) = dbl,
                 HR[2] = EndSpecials, HR += 3, AbsAppl(HR - 3)));
}

INLINE_ONLY Float FloatOfTerm(Term t) {
  return (Float)(*(Float *)(RepAppl(t) + 1));
}

#define InitUnalignedFloat()

INLINE_ONLY Float CpFloatUnaligned(CELL *ptr) {
  return *((Float *)ptr);
}

#else

#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P

#define DOUBLE_ALIGNED(ADDR) ((CELL)(ADDR)&0x4)

INLINE_ONLY void AlignGlobalForDouble(USES_REGS1);

INLINE_ONLY void AlignGlobalForDouble(USES_REGS1) {
  /* Force Alignment for floats. Note that garbage collector may
     break the alignment; */
  if (!DOUBLE_ALIGNED(HR)) {
    RESET_VARIABLE(HR);
    HR++;
  }
}

#ifdef i386
INLINE_ONLY Float CpFloatUnaligned(CELL *ptr) {
  return *((Float *)(ptr + 1));
}

#else
/* first, need to address the alignment problem */
INLINE_ONLY Float CpFloatUnaligned(CELL *ptr) {
  union {
    Float f;
    CELL d[2];
  } u;
  u.d[0] = ptr[1];
  u.d[1] = ptr[2];
  return (u.f);
}

#endif

INLINE_ONLY Term __MkFloatTerm(Float dbl USES_REGS) {
  return (Term)((AlignGlobalForDouble(PASS_REGS1), HR[0] = (CELL)FunctorDouble,
                 *(Float *)(HR + 1) = dbl, HR[3] = EndSpecials, HR += 4,
                 AbsAppl(HR - 4)));
}

INLINE_ONLY Float FloatOfTerm(Term t) {
  return (Float)((DOUBLE_ALIGNED(RepAppl(t)) ? *(Float *)(RepAppl(t) + 1)
                                             : CpFloatUnaligned(RepAppl(t))));
}

/* no alignment problems for 64 bit machines */
#else
/* OOPS, YAP only understands Floats that are as large as cells or that
   take two cells!!! */

OOPS

#endif
#endif

#ifndef YAP_H
#include <stddef.h>
#endif

INLINE_ONLY bool IsFloatTerm(Term);

INLINE_ONLY bool IsFloatTerm(Term t) {
  return (int)(IsApplTerm(t) && FunctorOfTerm(t) == FunctorDouble);
}

/* extern Functor FunctorLongInt; */

#define MkLongIntTerm(i) __MkLongIntTerm((i)PASS_REGS)

INLINE_ONLY Term __MkLongIntTerm(Int USES_REGS);

INLINE_ONLY Term __MkLongIntTerm(Int i USES_REGS) {
  HR[0] = (CELL)FunctorLongInt;
  HR[1] = (CELL)(i);
  HR[2] = EndSpecials;
  HR += 3;
  return AbsAppl(HR - 3);
}

INLINE_ONLY Int LongIntOfTerm(Term t);

INLINE_ONLY Int LongIntOfTerm(Term t) {
  return (Int)(RepAppl(t)[1]);
}

INLINE_ONLY bool IsLongIntTerm(Term);

INLINE_ONLY bool IsLongIntTerm(Term t) {
  return IsApplTerm(t) &&
          FunctorOfTerm(t) == FunctorLongInt;
}

/****************************************************/

/*********** strings, coded as UTF-8 ****************/

#include <string.h>

/* extern Functor FunctorString; */



#define MkStringTerm(i) __MkStringTerm((i)PASS_REGS)

INLINE_ONLY Term __MkStringTerm(const char *s USES_REGS);

INLINE_ONLY Term __MkStringTerm(const char *s USES_REGS) {
  Term t = AbsAppl(HR);
  size_t sz;
  if ((s[0] == '\0')) {
      sz = sizeof(CELL);
      HR[0] = (CELL)FunctorString;
      HR[1] = (CELL)sz;
      HR[2] = 0; 
    } else {
      sz =  ALIGN_BY_TYPE(strlen((char *)s) + 1, CELL);
      HR[0] = (CELL)FunctorString;
      HR[1] = (CELL)sz;
      strcpy((char *)(HR + 2), (const char *)s);
    }
      HR[2 + sz] = EndSpecials;
  HR += 3 + sz;
  return t;
}

#define MkUStringTerm(i) __MkUStringTerm((i)PASS_REGS)

INLINE_ONLY Term
__MkUStringTerm(const unsigned char *s USES_REGS);

INLINE_ONLY Term
__MkUStringTerm(const unsigned char *s USES_REGS) {
  Term t = AbsAppl(HR);
  size_t sz;
  if ((s[0] == '\0')) {
      sz = sizeof(CELL);
      HR[0] = (CELL)FunctorString;
      HR[1] = (CELL)sz;
      HR[2] = 0; 
    } else {
      sz =  ALIGN_BY_TYPE(strlen((char *)s) + 1, CELL);
      HR[0] = (CELL)FunctorString;
      HR[1] = (CELL)sz;
      strcpy((char *)(HR + 2), (const char *)s);
    }
      HR[2 + sz] = EndSpecials;
  HR += 3 + sz;
  return t;
}

#define MkCharPTerm(i) __MkCharPTerm((i)PASS_REGS)

INLINE_ONLY Term __MkCharPTerm(char *s USES_REGS);

INLINE_ONLY Term __MkCharPTerm(char *s USES_REGS) {
  Term t = AbsAppl(HR);
  size_t sz;
  if (s[0] == '\0') {
    sz = sizeof(CELL);
    HR[0] = (CELL)FunctorString;
    HR[1] = (CELL)sz;
    HR[2] = 0;
  } else {
    sz =  ALIGN_BY_TYPE(strlen((char *)s) + 1, CELL);
    HR[0] = (CELL)FunctorString;
    HR[1] = (CELL)sz;
    strcpy((char *)(HR + 2), (const char *)s);
  }
  HR[2 + sz] = EndSpecials;
  HR += 3 + sz;
  return t;
}



INLINE_ONLY const unsigned char *UStringOfTerm(Term t);

INLINE_ONLY const unsigned char *UStringOfTerm(Term t) {
  return (const unsigned char *)(RepAppl(t) + 2);
}

INLINE_ONLY const char *StringOfTerm(Term t);

INLINE_ONLY const char *StringOfTerm(Term t) {
  return (const char *)(RepAppl(t) + 2);
}

INLINE_ONLY bool IsStringTerm(Term);

INLINE_ONLY bool IsStringTerm(Term t) {
  return IsApplTerm(t) &&
          FunctorOfTerm(t) == FunctorString;
}

/****************************************************/

#ifdef USE_GMP

#include <stdio.h>

INLINE_ONLY bool IsBigIntTerm(Term);

INLINE_ONLY bool IsBigIntTerm(Term t) {
  return IsApplTerm(t) &&
          FunctorOfTerm(t) == FunctorBigInt;
}


#if !defined(__cplusplus)
#include <gmp.h>
#else
#include <gmpxx.h>
#endif


Term Yap_MkBigIntTerm(MP_INT *);
MP_INT *Yap_BigIntOfTerm(Term);

Term Yap_MkBigRatTerm(MP_RAT *);
MP_RAT *Yap_BigRatOfTerm(Term);

INLINE_ONLY void MPZ_SET(mpz_t, MP_INT *);

INLINE_ONLY void MPZ_SET(mpz_t dest, MP_INT *src) {
  dest->_mp_size = src->_mp_size;
  dest->_mp_alloc = src->_mp_alloc;
  dest->_mp_d = src->_mp_d;
}

INLINE_ONLY bool IsLargeIntTerm(Term);

INLINE_ONLY bool IsLargeIntTerm(Term t) {
  return IsApplTerm(t) &&
          ((FunctorOfTerm(t) <= FunctorBigInt) &&
           (FunctorOfTerm(t) >= FunctorLongInt));
}

INLINE_ONLY UInt Yap_SizeOfBigInt(Term);

INLINE_ONLY UInt Yap_SizeOfBigInt(Term t) {
  CELL *pt = RepAppl(t) + 1;
  return 2 +
         (sizeof(MP_INT) + (((MP_INT *)pt)->_mp_alloc * sizeof(mp_limb_t))) /
             sizeof(CELL);
}

#else

INLINE_ONLY int IsLargeIntTerm(Term);

INLINE_ONLY int IsLargeIntTerm(Term t) {
  return (int)(IsApplTerm(t) && FunctorOfTerm(t) == FunctorLongInt);
}

#endif

/* extern Functor FunctorLongInt; */

INLINE_ONLY bool IsLargeNumTerm(Term);

INLINE_ONLY bool IsLargeNumTerm(Term t) {
  return IsApplTerm(t) &&
          ((FunctorOfTerm(t) <= FunctorBigInt) &&
           (FunctorOfTerm(t) >= FunctorDouble));
}

INLINE_ONLY bool IsExternalBlobTerm(Term, CELL);

INLINE_ONLY bool IsExternalBlobTerm(Term t, CELL tag) {
  return IsApplTerm(t) &&
          FunctorOfTerm(t) == FunctorBigInt &&
          RepAppl(t)[1] == tag;
}

INLINE_ONLY void *ExternalBlobFromTerm(Term);

INLINE_ONLY void *ExternalBlobFromTerm(Term t) {
  MP_INT *base = (MP_INT *)(RepAppl(t) + 2);
  return (void *)(base + 1);
}

INLINE_ONLY bool IsNumTerm(Term);

INLINE_ONLY bool IsNumTerm(Term t) {
  return (IsIntTerm(t) || IsLargeNumTerm(t));
}

INLINE_ONLY bool IsAtomicTerm(Term);

INLINE_ONLY bool IsAtomicTerm(Term t) {
  return IsAtomOrIntTerm(t) ||
          IsLargeNumTerm(t) ||
          IsStringTerm(t);
}

INLINE_ONLY bool IsExtensionFunctor(Functor);

INLINE_ONLY bool IsExtensionFunctor(Functor f) {
  return f <= FunctorString;
}

INLINE_ONLY bool IsBlobFunctor(Functor);

INLINE_ONLY bool IsBlobFunctor(Functor f) {
  return (f <= FunctorString &&
          f >= FunctorDBRef);
}

INLINE_ONLY bool IsPrimitiveTerm(Term);

INLINE_ONLY bool IsPrimitiveTerm(Term t) {
  return (IsAtomOrIntTerm(t) ||
          (IsApplTerm(t) &&
                  IsBlobFunctor(FunctorOfTerm(t))));
}

#ifdef TERM_EXTENSIONS

INLINE_ONLY bool IsAttachFunc(Functor);

INLINE_ONLY bool IsAttachFunc(Functor f) { return (Int)(FALSE); }

#define IsAttachedTerm(t) __IsAttachedTerm(t PASS_REGS)

INLINE_ONLY bool __IsAttachedTerm(Term USES_REGS);

INLINE_ONLY bool __IsAttachedTerm(Term t USES_REGS) {
  return (IsVarTerm(t) &&
          IsAttVar(VarOfTerm(t)));
}

INLINE_ONLY bool GlobalIsAttachedTerm(Term);

INLINE_ONLY bool GlobalIsAttachedTerm(Term t) {
  return (IsVarTerm(t) &&
          GlobalIsAttVar(VarOfTerm(t)));
}

#define SafeIsAttachedTerm(t) __SafeIsAttachedTerm((t)PASS_REGS)

INLINE_ONLY bool __SafeIsAttachedTerm(Term USES_REGS);

INLINE_ONLY bool __SafeIsAttachedTerm(Term t USES_REGS) {
  return IsVarTerm(t) && IsAttVar(VarOfTerm(t));
}

INLINE_ONLY exts ExtFromCell(CELL *);

INLINE_ONLY exts ExtFromCell(CELL *pt) { return attvars_ext; }

#else

INLINE_ONLY Int IsAttachFunc(Functor);

INLINE_ONLY Int IsAttachFunc(Functor f) { return (Int)(FALSE); }

INLINE_ONLY Int IsAttachedTerm(Term);

INLINE_ONLY Int IsAttachedTerm(Term t) { return (Int)(FALSE); }

#endif

INLINE_ONLY Int Yap_BlobTag(Term t);

INLINE_ONLY Int Yap_BlobTag(Term t) {
  CELL *pt = RepAppl(t);

  return pt[1];
}

INLINE_ONLY void *Yap_BlobInfo(Term t);

INLINE_ONLY void *Yap_BlobInfo(Term t) {
  MP_INT *blobp;
  CELL *pt = RepAppl(t);

  blobp = (MP_INT *)(pt + 2);
  return (void *)(blobp + 1);
}

#ifdef YAP_H

INLINE_ONLY bool unify_extension(Functor, CELL, CELL *, CELL);

EXTERN bool unify_extension(Functor, CELL, CELL *, CELL);

int Yap_gmp_tcmp_big_big(Term, Term);

INLINE_ONLY bool unify_extension(Functor f, CELL d0, CELL *pt0,
                                              CELL d1) {
  switch (BlobOfFunctor(f)) {
  case db_ref_e:
    return (d0 == d1);
  case attvar_e:
    return (d0 == d1);
  case long_int_e:
    return (pt0[1] == RepAppl(d1)[1]);
  case string_e:
    return strcmp((char *)(pt0 + 2), (char *)(RepAppl(d1) + 2)) == 0;
  case big_int_e:
#ifdef USE_GMP
    return (Yap_gmp_tcmp_big_big(d0, d1) == 0);
#else
    return d0 == d1;
#endif /* USE_GMP */
  case double_e: {
    CELL *pt1 = RepAppl(d1);
    return (pt0[1] == pt1[1]
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
            && pt0[2] == pt1[2]
#endif
            );
  }
  }
  return false;
}

static inline CELL Yap_IntP_key(CELL *pt) {
#ifdef USE_GMP
  if (((Functor)pt[-1] == FunctorBigInt)) {
    MP_INT *b1 = Yap_BigIntOfTerm(AbsAppl(pt - 1));
    /* first cell in program */
    CELL val = ((CELL *)(b1 + 1))[0];
    return MkIntTerm(val & (MAX_ABS_INT - 1));
  }
#endif
  return MkIntTerm(pt[0] & (MAX_ABS_INT - 1));
}

static inline CELL Yap_Int_key(Term t) { return Yap_IntP_key(RepAppl(t) + 1); }

static inline CELL Yap_DoubleP_key(CELL *pt) {
#if SIZEOF_DOUBLE1 == 2 * SIZEOF_INT_P
  CELL val = pt[0] ^ pt[1];
#else
  CELL val = pt[0];
#endif
  return MkIntTerm(val & (MAX_ABS_INT - 1));
}

static inline CELL Yap_Double_key(Term t) {
  return Yap_DoubleP_key(RepAppl(t) + 1);
}

static inline CELL Yap_StringP_key(CELL *pt) {
  UInt n = pt[1], i;
  CELL val = pt[2];
  for (i = 1; i < n; i++) {
    val ^= pt[i + 1];
  }
  return MkIntTerm(val & (MAX_ABS_INT - 1));
}

static inline CELL Yap_String_key(Term t) {
  return Yap_StringP_key(RepAppl(t) + 1);
}

#endif
