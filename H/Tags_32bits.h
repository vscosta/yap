






/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		Tags_32bits.h.m4					 *
* Last rev:	December 90						 *
* mods:									 *
* comments:	Original Tag Scheme for machines with 32 bits adresses   *
* version:      $Id: Tags_32bits.h,v 1.1 2005-05-27 22:27:06 rslopes Exp $	 *
*************************************************************************/

/*    Original version for 32 bit addresses machines,
   Each	term is	represented internally as an unsigned 32 bit integer as
   follows:
			tag	 value
   ints			1m1....00 numeric value
   atoms		1m0....00 offset of atom entry
   pairs		1mr....01 ptr to pair
   aplied functor	1mr....10 ptr to functor followed by args
   ref			0mr....00 address of cell
   undefined		0mr....00 address of cell pointing to itself

 functors are represented as ptrs to the functor entry in the atom
property list

*/

#if FALSE

#define SHIFT_HIGH_TAG  29

#define MKTAG(HI,LO)   ((((UInt) (HI))<<SHIFT_HIGH_TAG)|(LO))

#define	TagBits	    /* 0xe0000003L */ MKTAG(0x7,3)
#define LowTagBits  /* 0x00000003L */ MKTAG(0x0,3)
#define HighTagBits /* 0xe0000000L */ MKTAG(0x7,0)
#define	AdrHiBit    /* 0x10000000L */ (((UInt)1) << (SHIFT_HIGH_TAG-1))
#define	MaskAdr	    /* 0x1ffffffcL */ ((((UInt)1) << SHIFT_HIGH_TAG)-4)
#define MaskPrim    /* 0x0ffffffcL */ ((((UInt)1) << (SHIFT_HIGH_TAG))-4)
#define	NumberTag   /* 0xa0000000L */ MKTAG(0x5,0)
#define	AtomTag	    /* 0x80000000L */ MKTAG(0x4,0)
#define	PairTag	    /* 0x80000001L */ MKTAG(0x4,1)
#define	ApplTag	    /* 0x80000002L */ MKTAG(0x4,2)
#define MAX_ABS_INT /* 0x04000000L */ (1 << (SHIFT_HIGH_TAG-3))

/* bits that should not be used by anyone but us */
#define YAP_PROTECTED_MASK 0xe0000000L

#define MaskBits    4

#define	PairBit	    /* 0x00000001L */ 1
#define	ApplBit	    /* 0x00000002L */ 2

#define	NonTagPart(X)	(Signed(X) & MaskPrim)
#define TAGGEDA(TAG,V)	(TAG | Unsigned(V))
#define TAGGED(TAG,V)   (TAG | NonTagPart(Unsigned(V)<<2))
#define NONTAGGED(TAG,V)   NonTagPart(Unsigned(V)<<2)
#define BitOn(Bit,V)    (Bit & Unsigned(V))
#define CHKTAG(t,Tag) 	((Unsigned(t)&TagBits)==Tag)

#include "inline-only.h"
INLINE_ONLY int IsVarTerm (Term);

INLINE_ONLY int
IsVarTerm (Term t)
{
  return (int) (Signed (t) >= 0);
}



INLINE_ONLY int IsNonVarTerm (Term);

INLINE_ONLY int
IsNonVarTerm (Term t)
{
  return (int) (Signed (t) < 0);
}



INLINE_ONLY Term *RepPair (Term);

INLINE_ONLY Term *
RepPair (Term t)
{
  return (Term *) (NonTagPart (t));
}



INLINE_ONLY Term AbsPair (Term *);

INLINE_ONLY Term
AbsPair (Term * p)
{
  return (Term) (TAGGEDA (PairTag, (p)));
}



INLINE_ONLY Int IsPairTerm (Term);

INLINE_ONLY Int
IsPairTerm (Term t)
{
  return (Int) (BitOn (PairBit, (t)));
}



INLINE_ONLY Term *RepAppl (Term);

INLINE_ONLY Term *
RepAppl (Term t)
{
  return (Term *) (NonTagPart (t));
}



INLINE_ONLY Term AbsAppl (Term *);

INLINE_ONLY Term
AbsAppl (Term * p)
{
  return (Term) (TAGGEDA (ApplTag, (p)));
}



INLINE_ONLY Int IsApplTerm (Term);

INLINE_ONLY Int
IsApplTerm (Term t)
{
  return (Int) (BitOn (ApplBit, (t)));
}



INLINE_ONLY int IsAtomOrIntTerm (Term);

INLINE_ONLY int
IsAtomOrIntTerm (Term t)
{
  return (int) (((Unsigned (t) & LowTagBits) == 0));
}




INLINE_ONLY Term AdjustPtr (Term t, Term off);

INLINE_ONLY Term
AdjustPtr (Term t, Term off)
{
  return (Term) ((t) + off);
}



INLINE_ONLY Term AdjustIDBPtr (Term t, Term off);

INLINE_ONLY Term
AdjustIDBPtr (Term t, Term off)
{
  return (Term) ((t) + off);
}




INLINE_ONLY Int IntOfTerm (Term);

INLINE_ONLY Int
IntOfTerm (Term t)
{
  return (Int) (((Int) (t << 3)) >> (3 + 2));
}

#endif /* NOT IN USE */
