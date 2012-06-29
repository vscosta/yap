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

#ifdef USE_SYSTEM_MALLOC
#define SF_STORE  (&(Yap_heap_regs->funcs))
#else
#define SF_STORE  ((special_functors *)HEAP_INIT_BASE)
#endif

#ifdef USE_OFFSETS
#define   AtomFoundVar ((Atom)(&(((special_functors *)(NULL))->AtFoundVar)))
#define   AtomFreeTerm ((Atom)(&(((special_functors *)(NULL))->AtFreeTerm)))
#define   AtomNil ((Atom)(&(((special_functors *)(NULL))->AtNil)))
#define   AtomDot ((Atom)(&(((special_functors *)(NULL))->AtDot)))
#elif defined(THREADS)
#define   AtomFoundVar AbsAtom(SF_STORE->AtFoundVar)
#define   AtomFreeTerm AbsAtom(SF_STORE->AtFreeTerm)
#define   AtomNil AbsAtom(SF_STORE->AtNil)
#define   AtomDot AbsAtom(SF_STORE->AtDot)
#else
#define   AtomFoundVar AbsAtom(&(SF_STORE->AtFoundVar))
#define   AtomFreeTerm AbsAtom(&(SF_STORE->AtFreeTerm))
#define   AtomNil AbsAtom(&(SF_STORE->AtNil))
#define   AtomDot AbsAtom(&(SF_STORE->AtDot))
#endif

#define   TermFoundVar MkAtomTerm(AtomFoundVar)
#define   TermFreeTerm MkAtomTerm(AtomFreeTerm)
#define   TermNil MkAtomTerm(AtomNil)
#define   TermDot MkAtomTerm(AtomDot)

typedef enum
{
  db_ref_e = sizeof (Functor *),
  attvar_e = 2*sizeof (Functor *),
  long_int_e = 3 * sizeof (Functor *),
  big_int_e = 4 * sizeof (Functor *),
  double_e = 5 * sizeof (Functor *)
}
blob_type;

#define   FunctorDBRef    ((Functor)(db_ref_e))
#define   FunctorAttVar   ((Functor)(attvar_e))
#define   FunctorLongInt  ((Functor)(long_int_e))
#define   FunctorBigInt   ((Functor)(big_int_e))
#define   FunctorDouble   ((Functor)(double_e))
#define   EndSpecials     (double_e+sizeof(Functor *))

#include "inline-only.h"

inline EXTERN int IsAttVar (CELL *pt) INLINE_ONLY;

inline EXTERN int
IsAttVar (CELL *pt)
{
#ifdef YAP_H
  CACHE_REGS
  return (pt)[-1] == (CELL)attvar_e
    && pt < H;
#else
  return (pt)[-1] == (CELL)attvar_e;
#endif
}

inline EXTERN int GlobalIsAttVar (CELL *pt) INLINE_ONLY;

inline EXTERN int
GlobalIsAttVar (CELL *pt)
{
  return (pt)[-1] == (CELL)attvar_e;
}

typedef enum
  {
    BIG_INT =      0x01,
    BIG_RATIONAL = 0x02,
    BIG_FLOAT =    0x04,
    EMPTY_ARENA =  0x10,
    ARRAY_INT =    0x21,
    ARRAY_FLOAT =  0x22,
    CLAUSE_LIST =  0x40,
    BLOB_STRING =  0x80, /* SWI style strings */
    BLOB_WIDE_STRING =  0x81, /* SWI style strings */
    EXTERNAL_BLOB =  0x100, /* generic data */
    USER_BLOB_START =  0x1000, /* user defined blob */
    USER_BLOB_END =  0x1100 /* end of user defined blob */
  } 
big_blob_type;

inline EXTERN blob_type BlobOfFunctor (Functor f) INLINE_ONLY;

inline EXTERN blob_type
BlobOfFunctor (Functor f)
{
  return (blob_type) ((CELL)f);
}

typedef struct cp_frame {
  CELL *start_cp;
  CELL *end_cp;
  CELL *to;
#ifdef RATIONAL_TREES
  CELL oldv;
  int ground;
#endif
} copy_frame;


#ifdef COROUTINING

typedef struct
{
  /* what to do when someone tries to bind our term to someone else
     in some  predefined context */
  void (*bind_op) (Term *, Term CACHE_TYPE);
  /* what to do if someone wants to copy our constraint */
  int (*copy_term_op) (CELL *, struct cp_frame **, CELL * CACHE_TYPE);
  /* copy the constraint into a term and back */
  Term (*to_term_op) (CELL *);
  int (*term_to_op) (Term, Term CACHE_TYPE);
  /* op called to do marking in GC */
  void (*mark_op) (CELL *);
} ext_op;

/* known delays */
typedef enum
{
  empty_ext = 0 * sizeof (ext_op),	/* default op, this should never be called */
  attvars_ext = 1 * sizeof (ext_op)	/* support for attributed variables */
    /* add your own extensions here */
    /* keep this one */
}
exts;




#endif


#ifdef YAP_H
/* make sure that these data structures are the first thing to be allocated
   in the heap when we start the system */
#ifdef THREADS
typedef struct special_functors_struct
{
  AtomEntry *AtFoundVar;
  AtomEntry *AtFreeTerm;
  AtomEntry *AtNil;
  AtomEntry *AtDot;
} special_functors;
#else
typedef struct special_functors_struct
{
  AtomEntry AtFoundVar;
  char AtFoundVarChars[8];
  AtomEntry AtFreeTerm;
  char AtFreeTermChars[8];
  AtomEntry AtNil;
  char AtNilChars[8];
  AtomEntry AtDot;
  char AtDotChars[8];
}
special_functors;
#endif

#endif /* YAP_H */

inline EXTERN Float CpFloatUnaligned(CELL *ptr) INLINE_ONLY;

#if SIZEOF_DOUBLE == SIZEOF_LONG_INT


inline EXTERN Term MkFloatTerm (Float) INLINE_ONLY;

inline EXTERN Term
MkFloatTerm (Float dbl)
{
  CACHE_REGS
  return (Term) ((H[0] = (CELL) FunctorDouble, *(Float *) (H + 1) =
		  dbl, H[2] = EndSpecials, H +=
		  3, AbsAppl (H - 3)));
}


inline EXTERN Float FloatOfTerm (Term t) INLINE_ONLY;

inline EXTERN Float
FloatOfTerm (Term t)
{
  return (Float) (*(Float *) (RepAppl (t) + 1));
}



#define InitUnalignedFloat()

inline EXTERN Float
CpFloatUnaligned(CELL *ptr)
{
  return *((Float *)ptr);
}

#else

#if SIZEOF_DOUBLE == 2*SIZEOF_LONG_INT

inline EXTERN void AlignGlobalForDouble( USES_REGS1 ) INLINE_ONLY;

#define DOUBLE_ALIGNED(ADDR) ((CELL)(ADDR) & 0x4)

#ifdef i386
inline EXTERN Float
CpFloatUnaligned (CELL * ptr)
{
  return *((Float *) (ptr + 1));
}

#else
/* first, need to address the alignment problem */
inline EXTERN Float
CpFloatUnaligned (CELL * ptr)
{
  union
  {
    Float f;
    CELL d[2];
  } u;
  u.d[0] = ptr[1];
  u.d[1] = ptr[2];
  return (u.f);
}

#endif

inline EXTERN Term MkFloatTerm (Float) INLINE_ONLY;

inline EXTERN Term
MkFloatTerm (Float dbl)
{
  CACHE_REGS
  return (Term) ((AlignGlobalForDouble ( PASS_REGS1 ), H[0] =
		  (CELL) FunctorDouble, *(Float *) (H + 1) = dbl, H[3] =
		  EndSpecials, H +=
		  4, AbsAppl (H - 4)));
}


inline EXTERN Float FloatOfTerm (Term t) INLINE_ONLY;

inline EXTERN Float
FloatOfTerm (Term t)
{
  return (Float) ((DOUBLE_ALIGNED (RepAppl (t)) ? *(Float *) (RepAppl (t) + 1)
		   : CpFloatUnaligned (RepAppl (t))));
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

Term Yap_MkBlobStringTerm(const char *, size_t len);
Term Yap_MkBlobWideStringTerm(const wchar_t *, size_t len);
char *Yap_BlobStringOfTerm(Term);
wchar_t *Yap_BlobWideStringOfTerm(Term);
char *Yap_BlobStringOfTermAndLength(Term, size_t *);



inline EXTERN int IsFloatTerm (Term) INLINE_ONLY;

inline EXTERN int
IsFloatTerm (Term t)
{
  return (int) (IsApplTerm (t) && FunctorOfTerm (t) == FunctorDouble);
}




/* extern Functor FunctorLongInt; */

inline EXTERN Term MkLongIntTerm (Int) INLINE_ONLY;

inline EXTERN Term
MkLongIntTerm (Int i)
{
  CACHE_REGS
  H[0] = (CELL) FunctorLongInt;
  H[1] = (CELL) (i);
  H[2] =  EndSpecials;
  H += 3;
  return AbsAppl(H - 3);
}


inline EXTERN Int LongIntOfTerm (Term t) INLINE_ONLY;

inline EXTERN Int
LongIntOfTerm (Term t)
{
  return (Int) (RepAppl (t)[1]);
}



inline EXTERN int IsLongIntTerm (Term) INLINE_ONLY;

inline EXTERN int
IsLongIntTerm (Term t)
{
  return (int) (IsApplTerm (t) && FunctorOfTerm (t) == FunctorLongInt);
}



#ifdef USE_GMP

#include <stdio.h>

#include <gmp.h>

#else

typedef UInt mp_limb_t;

typedef struct {
  Int _mp_size, _mp_alloc;
  mp_limb_t *_mp_d;
} MP_INT;

typedef struct {
  MP_INT _mp_num;
  MP_INT _mp_den;
} MP_RAT;

#endif

inline EXTERN int IsBigIntTerm (Term) INLINE_ONLY;

inline EXTERN int
IsBigIntTerm (Term t)
{
  return (int) (IsApplTerm (t) && FunctorOfTerm (t) == FunctorBigInt);
}

#ifdef USE_GMP

Term STD_PROTO (Yap_MkBigIntTerm, (MP_INT *));
MP_INT *STD_PROTO (Yap_BigIntOfTerm, (Term));

Term STD_PROTO (Yap_MkBigRatTerm, (MP_RAT *));
MP_RAT *STD_PROTO (Yap_BigRatOfTerm, (Term));

inline EXTERN void MPZ_SET (mpz_t, MP_INT *) INLINE_ONLY;

inline EXTERN void
MPZ_SET (mpz_t dest, MP_INT *src)
{
  dest->_mp_size = src->_mp_size;
  dest->_mp_alloc = src->_mp_alloc;
  dest->_mp_d = src->_mp_d;
}

inline EXTERN int IsLargeIntTerm (Term) INLINE_ONLY;

inline EXTERN int
IsLargeIntTerm (Term t)
{
  return (int) (IsApplTerm (t)
		&& ((FunctorOfTerm (t) <= FunctorBigInt)
		    && (FunctorOfTerm (t) >= FunctorLongInt)));
}


inline EXTERN UInt Yap_SizeOfBigInt (Term) INLINE_ONLY;

inline EXTERN UInt
Yap_SizeOfBigInt (Term t)
{
  CELL *pt = RepAppl(t)+1;
  return 2+(sizeof(MP_INT)+
	    (((MP_INT *)pt)->_mp_alloc*sizeof(mp_limb_t)))/sizeof(CELL);
}



#else



inline EXTERN int IsLargeIntTerm (Term) INLINE_ONLY;

inline EXTERN int
IsLargeIntTerm (Term t)
{
  return (int) (IsApplTerm (t) && FunctorOfTerm (t) == FunctorLongInt);
}



#endif

typedef struct string_struct {
  UInt len;
}  blob_string_t;

inline EXTERN int IsBlobStringTerm (Term) INLINE_ONLY;

inline EXTERN int
IsBlobStringTerm (Term t)
{
  return (int) (IsApplTerm (t) &&
		FunctorOfTerm (t) == FunctorBigInt &&
		(RepAppl(t)[1] & BLOB_STRING) == BLOB_STRING);
}

inline EXTERN int IsWideBlobStringTerm (Term) INLINE_ONLY;

inline EXTERN int
IsWideBlobStringTerm (Term t)
{
  return (int) (IsApplTerm (t) &&
		FunctorOfTerm (t) == FunctorBigInt &&
		RepAppl(t)[1] == BLOB_WIDE_STRING);
}

/* extern Functor FunctorLongInt; */

inline EXTERN int IsLargeNumTerm (Term) INLINE_ONLY;

inline EXTERN int
IsLargeNumTerm (Term t)
{
  return (int) (IsApplTerm (t)
		&& ((FunctorOfTerm (t) <= FunctorDouble)
		    && (FunctorOfTerm (t) >= FunctorLongInt)));
}

inline EXTERN int IsExternalBlobTerm (Term, CELL) INLINE_ONLY;

inline EXTERN int
IsExternalBlobTerm (Term t, CELL tag)
{
  return (int) (IsApplTerm (t) &&
		FunctorOfTerm (t) == FunctorBigInt &&
		RepAppl(t)[1] == tag);
}

inline EXTERN void *ExternalBlobFromTerm (Term) INLINE_ONLY;

inline EXTERN void *
ExternalBlobFromTerm (Term t)
{
  MP_INT *base = (MP_INT *)(RepAppl(t)+2);
  return (void *) (base+1);
}




inline EXTERN int IsNumTerm (Term) INLINE_ONLY;

inline EXTERN int
IsNumTerm (Term t)
{
  return (int) ((IsIntTerm (t) || IsLargeNumTerm (t)));
}




inline EXTERN Int IsAtomicTerm (Term) INLINE_ONLY;

inline EXTERN Int
IsAtomicTerm (Term t)
{
  return (Int) (IsAtomOrIntTerm (t) || IsLargeNumTerm (t));
}




inline EXTERN Int IsExtensionFunctor (Functor) INLINE_ONLY;

inline EXTERN Int
IsExtensionFunctor (Functor f)
{
  return (Int) (f <= FunctorDouble);
}



inline EXTERN Int IsBlobFunctor (Functor) INLINE_ONLY;

inline EXTERN Int
IsBlobFunctor (Functor f)
{
  return (Int) ((f <= FunctorDouble && f >= FunctorDBRef));
}



inline EXTERN Int IsPrimitiveTerm (Term) INLINE_ONLY;

inline EXTERN Int
IsPrimitiveTerm (Term t)
{
  return (Int) ((IsAtomOrIntTerm (t)
		 || (IsApplTerm (t) && IsBlobFunctor (FunctorOfTerm (t)))));
}

#ifdef TERM_EXTENSIONS


inline EXTERN Int IsAttachFunc (Functor) INLINE_ONLY;

inline EXTERN Int
IsAttachFunc (Functor f)
{
  return (Int) (FALSE);
}




inline EXTERN Int IsAttachedTerm (Term) INLINE_ONLY;

inline EXTERN Int
IsAttachedTerm (Term t)
{
  return (Int) ((IsVarTerm (t) && IsAttVar(VarOfTerm(t))));
}

inline EXTERN Int GlobalIsAttachedTerm (Term) INLINE_ONLY;

inline EXTERN Int
GlobalIsAttachedTerm (Term t)
{
  return (Int) ((IsVarTerm (t) && GlobalIsAttVar(VarOfTerm(t))));
}

inline EXTERN Int SafeIsAttachedTerm (Term) INLINE_ONLY;

inline EXTERN Int
SafeIsAttachedTerm (Term t)
{
  return (Int) (IsVarTerm (t) && IsAttVar(VarOfTerm(t)));
}




inline EXTERN exts ExtFromCell (CELL *) INLINE_ONLY;

inline EXTERN exts
ExtFromCell (CELL * pt)
{
  return attvars_ext;
}



#else


inline EXTERN Int IsAttachFunc (Functor) INLINE_ONLY;

inline EXTERN Int
IsAttachFunc (Functor f)
{
  return (Int) (FALSE);
}




inline EXTERN Int IsAttachedTerm (Term) INLINE_ONLY;

inline EXTERN Int
IsAttachedTerm (Term t)
{
  return (Int) (FALSE);
}




#endif

inline EXTERN Int Yap_BlobTag(Term t) INLINE_ONLY;

inline EXTERN Int Yap_BlobTag(Term t)
{
  CELL *pt = RepAppl(t);

  return pt[1];
}


inline EXTERN void *Yap_BlobInfo(Term t) INLINE_ONLY;

inline EXTERN void *Yap_BlobInfo(Term t)
{
  MP_INT *blobp;
  CELL *pt = RepAppl(t);

  blobp = (MP_INT *)(pt+2);
  return (void *)(blobp+1);
}

#ifdef YAP_H

inline EXTERN int STD_PROTO (unify_extension, (Functor, CELL, CELL *, CELL)) INLINE_ONLY;

EXTERN int STD_PROTO (unify_extension, (Functor, CELL, CELL *, CELL));

int   STD_PROTO(Yap_gmp_tcmp_big_big,(Term, Term));

inline EXTERN int
unify_extension (Functor f, CELL d0, CELL * pt0, CELL d1)
{
  switch (BlobOfFunctor (f))
    {
    case db_ref_e:
      return (d0 == d1);
    case attvar_e:
      return (d0 == d1);
    case long_int_e:
      return (pt0[1] == RepAppl (d1)[1]);
    case big_int_e:
#ifdef USE_GMP
      return (Yap_gmp_tcmp_big_big(d0,d1) == 0);
#else
      return d0 == d1;
#endif /* USE_GMP */
    case double_e:
      {
	CELL *pt1 = RepAppl (d1);
	return (pt0[1] == pt1[1]
#if SIZEOF_DOUBLE == 2*SIZEOF_LONG_INT
		&& pt0[2] == pt1[2]
#endif
	  );
      }
    }
  return (FALSE);
}

static inline
CELL Yap_IntP_key(CELL *pt)
{
#ifdef USE_GMP
  if (((Functor)pt[-1] == FunctorBigInt)) {
    MP_INT *b1 = Yap_BigIntOfTerm(AbsAppl(pt-1));
    /* first cell in program */
    CELL val = ((CELL *)(b1+1))[0];
    return MkIntTerm(val & (MAX_ABS_INT-1));
  }
#endif
  return MkIntTerm(pt[0] & (MAX_ABS_INT-1));
}

static inline
CELL Yap_Int_key(Term t)
{
  return Yap_IntP_key(RepAppl(t)+1);
}

static inline
CELL Yap_DoubleP_key(CELL *pt)
{
#if SIZEOF_DOUBLE == 2*SIZEOF_LONG_INT
  CELL val = pt[0]^pt[1];
#else
  CELL val = pt[0];
#endif
  return MkIntTerm(val & (MAX_ABS_INT-1));  
}

static inline
CELL Yap_Double_key(Term t)
{
  return Yap_DoubleP_key(RepAppl(t)+1);
}

#endif
