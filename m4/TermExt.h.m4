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
* version:      $Id: TermExt.h.m4,v 1.16 2004-10-04 18:56:20 vsc Exp $	 *
*************************************************************************/

#ifdef USE_SYSTEM_MALLOC
#define SF_STORE  (&(heap_regs->funcs))
#else
#define SF_STORE  ((special_functors *)HEAP_INIT_BASE)
#endif

#if USE_OFFSETS
#define   AtomFoundVar ((Atom)(&(((special_functors *)(NULL))->AtFoundVar)))
#define   AtomNil ((Atom)(&(((special_functors *)(NULL))->AtNil)))
#define   AtomDot ((Atom)(&(((special_functors *)(NULL))->AtDot)))
#else
#define   AtomFoundVar AbsAtom(&(SF_STORE->AtFoundVar))
#define   AtomNil AbsAtom(&(SF_STORE->AtNil))
#define   AtomDot AbsAtom(&(SF_STORE->AtDot))
#endif

#define   TermFoundVar MkAtomTerm(AtomFoundVar)
#define   TermNil MkAtomTerm(AtomNil)
#define   TermDot MkAtomTerm(AtomDot)

#if defined(IN_SECOND_QUADRANT) && !GC_NO_TAGS
typedef enum {
  db_ref_e = sizeof(Functor *)|RBIT,
  long_int_e = 2*sizeof(Functor *)|RBIT,
#ifdef USE_GMP
  big_int_e = 3*sizeof(Functor *)|RBIT,
  double_e = 4*sizeof(Functor *)|RBIT
#else
  double_e = 3*sizeof(Functor *)|RBIT
#endif
} blob_type;
#else
typedef enum {
  db_ref_e = sizeof(Functor *),
  long_int_e = 2*sizeof(Functor *),
#ifdef USE_GMP
  big_int_e = 3*sizeof(Functor *),
  double_e = 4*sizeof(Functor *)
#else
  double_e = 3*sizeof(Functor *)
#endif
} blob_type;
#endif

#define   FunctorDBRef    ((Functor)(db_ref_e))
#define   FunctorLongInt  ((Functor)(long_int_e))
#ifdef USE_GMP
#define   FunctorBigInt   ((Functor)(big_int_e))
#endif
#define   FunctorDouble   ((Functor)(double_e))
#define   EndSpecials     (double_e)

Destructor(Functor, BlobOf, blob_type, f, (CELL)f)

#ifdef COROUTINING

typedef struct {
  /* what to do when someone tries to bind our term to someone else
     in some  predefined context */
  void (*bind_op)(Term *, Term);
  /* what to do if someone wants to copy our constraint */
  int (*copy_term_op)(CELL *, CELL ***, CELL *);
  /* copy the constraint into a term and back */
  Term (*to_term_op)(CELL *);
  int (*term_to_op)(Term, Term);
  /* op called to do marking in GC */
  void (*mark_op)(CELL *);
} ext_op;

/* known delays */
typedef enum {
  empty_ext = 0*sizeof(ext_op),	     /* default op, this should never be called */
  attvars_ext = 1*sizeof(ext_op)     /* support for attributed variables */
  /* add your own extensions here */
  /* keep this one */
} exts;


/* array with the ops for your favourite extensions */
extern ext_op attas[attvars_ext+1];

#endif

/* make sure that these data structures are the first thing to be allocated
   in the heap when we start the system */
typedef struct special_functors_struct
{
  AtomEntry AtFoundVar;
  char AtFoundVarChars[8];
  AtomEntry AtNil;
  char AtNilChars[8];
  AtomEntry AtDot;
  char AtDotChars[8];
}
special_functors;

#if USE_SYSTEM_MALLOC
#define MAX_SPECIALS_TAG (4*4096)
#else
#define MAX_SPECIALS_TAG ((CELL)AtomBase)
#endif

#if SIZEOF_DOUBLE == SIZEOF_LONG_INT
#if GC_NO_TAGS
Inline(MkFloatTerm, Term, Float, dbl, (H[0] = (CELL)FunctorDouble, *(Float *)(H+1) = dbl, H[2]=(2*sizeof(CELL)+EndSpecials),H+=3,AbsAppl(H-3)))
#else
Inline(MkFloatTerm, Term, Float, dbl, (H[0] = (CELL)FunctorDouble, *(Float *)(H+1) = dbl, H[2]=((2*sizeof(CELL)+EndSpecials)|MBIT),H+=3,AbsAppl(H-3)))
#endif

Destructor(Term, FloatOf, Float, t, *(Float *)(RepAppl(t)+1))

#define InitUnalignedFloat()

#else

#if SIZEOF_DOUBLE == 2*SIZEOF_LONG_INT

inline EXTERN Float STD_PROTO(CpFloatUnaligned,(CELL *));

inline EXTERN void STD_PROTO(AlignGlobalForDouble,(void));

#define DOUBLE_ALIGNED(ADDR) ((CELL)(ADDR) & 0x4)

#ifdef i386
inline EXTERN Float
CpFloatUnaligned(CELL *ptr)
{
  return *((Float *)(ptr+1));
}

#else
/* first, need to address the alignment problem */
inline EXTERN Float
CpFloatUnaligned(CELL *ptr)
{
  union { Float f; CELL d[2]; } u;
  u.d[0] = ptr[1];
  u.d[1] = ptr[2];
  return(u.f);
}

#endif

#if GC_NO_TAGS
Inline(MkFloatTerm, Term, Float, dbl, (AlignGlobalForDouble(), H[0] = (CELL)FunctorDouble,  *(Float *)(H+1) = dbl, H[3]=(3*sizeof(CELL)+EndSpecials), H+=4, AbsAppl(H-4)))
#else
Inline(MkFloatTerm, Term, Float, dbl, (AlignGlobalForDouble(), H[0] = (CELL)FunctorDouble,  *(Float *)(H+1) = dbl, H[3]=((3*sizeof(CELL)+EndSpecials)|MBIT), H+=4, AbsAppl(H-4)))
#endif

Destructor(Term, FloatOf, Float, t, (DOUBLE_ALIGNED(RepAppl(t)) ? *(Float *)(RepAppl(t)+1) : CpFloatUnaligned(RepAppl(t))))
/* no alignment problems for 64 bit machines */
#else
     /* OOPS, YAP only understands Floats that are as large as cells or that
	take two cells!!! */
#endif
#endif

Inline(IsFloatTerm, int, Term, t, IsApplTerm(t) && FunctorOfTerm(t) == FunctorDouble)


/* extern Functor FunctorLongInt; */
#if GC_NO_TAGS
Inline(MkLongIntTerm, Term, Int, i, (H[0] = (CELL)FunctorLongInt,H[1] = (CELL)(i),H[2]=(2*sizeof(CELL)+EndSpecials),H+=3,AbsAppl(H-3)))
#else
Inline(MkLongIntTerm, Term, Int, i, (H[0] = (CELL)FunctorLongInt,H[1] = (CELL)(i),H[2]=((2*sizeof(CELL)+EndSpecials)|MBIT),H+=3,AbsAppl(H-3)))
#endif
Destructor(Term, LongIntOf, Int, t, RepAppl(t)[1])
Inline(IsLongIntTerm, int, Term, t, IsApplTerm(t) && FunctorOfTerm(t) == FunctorLongInt)


#ifdef USE_GMP
#include <stdio.h>
#include <gmp.h>


MP_INT *STD_PROTO(Yap_PreAllocBigNum,(void));
MP_INT *STD_PROTO(Yap_InitBigNum,(Int));
Term    STD_PROTO(Yap_MkBigIntTerm, (MP_INT *));
MP_INT *STD_PROTO(Yap_BigIntOfTerm, (Term));
void    STD_PROTO(Yap_CleanBigNum,(void));

Inline(IsBigIntTerm, int, Term, t, IsApplTerm(t) && FunctorOfTerm(t) == FunctorBigInt)

Inline(IsLargeIntTerm, int, Term, t, IsApplTerm(t) && ((FunctorOfTerm(t) <= FunctorBigInt) &&  (FunctorOfTerm(t) >= FunctorLongInt)))

#else

Inline(IsBigIntTerm, int, Term, t, FALSE)

Inline(IsLargeIntTerm, int, Term, t, IsApplTerm(t) && FunctorOfTerm(t) == FunctorLongInt)

#endif

/* extern Functor FunctorLongInt; */
Inline(IsLargeNumTerm, int, Term, t, IsApplTerm(t) && ((FunctorOfTerm(t) <= FunctorDouble) &&  (FunctorOfTerm(t) >= FunctorLongInt)))

Inline(IsNumTerm, int, Term, t, (IsIntTerm(t) || IsLargeNumTerm(t)))

Inline(IsAtomicTerm, Int, Term, t, IsAtomOrIntTerm(t) || IsLargeNumTerm(t))

Inline(IsExtensionFunctor, Int, Functor, f,  f <= FunctorDouble)
Inline(IsBlobFunctor, Int, Functor, f,  (f <= FunctorDouble && f >= FunctorDBRef))
Inline(IsPrimitiveTerm, Int, Term, t, (IsAtomOrIntTerm(t) || (IsApplTerm(t) && IsBlobFunctor(FunctorOfTerm(t)))))

#ifdef TERM_EXTENSIONS

Inline(IsAttachFunc, Int, Functor, f,  FALSE)

Inline(IsAttachedTerm, Int, Term, t, (IsVarTerm(t) && VarOfTerm(t) < H0) )

Inline(SafeIsAttachedTerm, Int, Term, t, (IsVarTerm(t) && VarOfTerm(t) < H0 && VarOfTerm(t) >= (CELL *)Yap_GlobalBase) )

Inline(ExtFromCell, exts, CELL *, pt, pt[1])

#else

Inline(IsAttachFunc, Int, Functor, f, FALSE)

Inline(IsAttachedTerm, Int, Term, t, FALSE)


#endif

inline EXTERN int STD_PROTO(unify_extension,(Functor, CELL, CELL *, CELL));

EXTERN int  STD_PROTO(unify_extension,(Functor, CELL, CELL *, CELL));

inline EXTERN int
unify_extension(Functor f, CELL d0, CELL *pt0, CELL d1)
{
  switch(BlobOfFunctor(f)) {
  case db_ref_e:
    return (d0 == d1);
  case long_int_e:
    return(pt0[1] == RepAppl(d1)[1]);
#ifdef USE_GMP
  case big_int_e:
    return (mpz_cmp(Yap_BigIntOfTerm(d0),Yap_BigIntOfTerm(d1)) == 0);
#endif /* USE_GMP */
  case double_e:
    {
      CELL *pt1 = RepAppl(d1);
      return (pt0[1] == pt1[1]
#if SIZEOF_DOUBLE == 2*SIZEOF_LONG_INT
	      && pt0[2] == pt1[2]
#endif
	      );
    }
  }
  return(FALSE);
}

