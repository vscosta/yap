






/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		Tags_32Ops.h.m4					 *
* Last rev:	December 90						 *
* mods:									 *
* comments:	Original Tag Scheme for machines with 32 bits adresses   *
* version:      $Id: Tags_64bits.h,v 1.3 2008-05-15 13:41:46 vsc Exp $	 *
*************************************************************************/

#define TAG_64BITS 1

/*    Version for 64 bit addresses machines,
   Each	term is	represented internally as an unsigned 64 bit integer as
   follows:
			tag	 value
   ints			0m1....001 numeric value
   atoms		0m0....001 offset of atom entry
   pairs		0mr....011 ptr to pair
   aplied functor	0mr....101 ptr to functor followed by args
   undefined		0mr....000 address of cell pointing to itself

 functors are represented as ptrs to the functor entry in the atom
property list

	We rely on the fact that addresses are always multiple of 8.

*/

#define SHIFT_HIGH_TAG  62

#define MKTAG(HI,LO)   ((((UInt) (HI))<<SHIFT_HIGH_TAG)|(LO))

#define	TagBits	    /* 0x30000007L */ MKTAG(0x1,7)
#define LowTagBits  /* 0x00000007L */ MKTAG(0x0,7)
#define HighTagBits /* 0x70000000L */ MKTAG(0x1,0)
#define	AdrHiBit    /* 0x08000000L */ (((UInt)1) << (SHIFT_HIGH_TAG-1))
#define MaskPrim    /* 0x0ffffff8L */ ((((UInt)1) << (SHIFT_HIGH_TAG))-8)
#define	NumberTag   /* 0x30000001L */ MKTAG(0x1,1)
#define	AtomTag	    /* 0x10000001L */ MKTAG(0x0,1)
#define MAX_ABS_INT /* 0xfe00000LL */ (((Int)1) << (63-(2+4)))

/* bits that should not be used by anyone but us */
#define YAP_PROTECTED_MASK 0xe000000000000000L

#define UNIQUE_TAG_FOR_PAIRS 1

#define	PrimiBit    /* 0x00000001L */ 1
#define	PairBits    /* 0x00000003L */ 3
#define	ApplBits    /* 0x00000005L */ 5
#define PrimiBits   /* 0x70000004L */ MKTAG(0x7,7)
#define NumberMask  /* 0x20000007L */ MKTAG(0x2,7)

#define TagOf(t) 	(Unsigned(t)&TagBits)
#define LowTagOf(t) 	(Unsigned(t)&LowTagBits)
#define	NonTagPart(X)	(Signed(X) & MaskPrim)
#define TAGGEDA(TAG,V)	(Unsigned(TAG) | Unsigned(V))
#define TAGGED(TAG,V)   (Unsigned(TAG) | NonTagPart(Unsigned(V)<<3))	/* SQRT(8) */
#define NONTAGGED(TAG,V)   NonTagPart(Unsigned(V)<<3)	/* SQRT(8) */
#define CHKTAG(t,Tag) 	((Unsigned(t)&TagBits)==Tag)


inline EXTERN int IsVarTerm (Term);

inline EXTERN int
IsVarTerm (Term t)
{
  return (int) ((!((t) & 0x1)));
}



inline EXTERN int IsNonVarTerm (Term);

inline EXTERN int
IsNonVarTerm (Term t)
{
  return (int) (((t) & 0x1));
}



inline EXTERN Term *RepPair (Term);

inline EXTERN Term *
RepPair (Term t)
{
  return (Term *) (((t) - PairBits));
}



inline EXTERN Term AbsPair (Term *);

inline EXTERN Term
AbsPair (Term * p)
{
  return (Term) (((CELL) (p) + PairBits));
}



inline EXTERN Int IsPairTerm (Term);

inline EXTERN Int
IsPairTerm (Term t)
{
  return (Int) (((t) & 0x2));
}



inline EXTERN Term *RepAppl (Term);

inline EXTERN Term *
RepAppl (Term t)
{
  return (Term *) (((t) - ApplBits));
}



inline EXTERN Term AbsAppl (Term *);

inline EXTERN Term
AbsAppl (Term * p)
{
  return (Term) (((CELL) (p) + ApplBits));
}



inline EXTERN Int IsApplTerm (Term);

inline EXTERN Int
IsApplTerm (Term t)
{
  return (Int) ((((t) & 0x4)));
}



inline EXTERN Int IsAtomOrIntTerm (Term);

inline EXTERN Int
IsAtomOrIntTerm (Term t)
{
  return (Int) ((((t) & LowTagBits) == 0x1));
}




inline EXTERN Term AdjustPtr (Term t, Term off);

inline EXTERN Term
AdjustPtr (Term t, Term off)
{
  return (Term) (((t) + off));
}



inline EXTERN Term AdjustIDBPtr (Term t, Term off);

inline EXTERN Term
AdjustIDBPtr (Term t, Term off)
{
  return (Term) ((t) + off);
}




inline EXTERN Int IntOfTerm (Term);

inline EXTERN Int
IntOfTerm (Term t)
{
  return (Int) ((Int) (Unsigned (t) << 3) >> 6);
}
