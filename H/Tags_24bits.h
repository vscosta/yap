






/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		Tags_24bits.h.m4					 *
* Last rev:	December 90						 *
* mods:									 *
* comments:	Tag Scheme for machines with 24 bits adresses (m68000)   *
* version:      $Id: Tags_24bits.h,v 1.2 2008-01-30 10:35:43 vsc Exp $	 *
*************************************************************************/

     /*   Version for 24 bit    addresses (68000)
        Each    term is represented internally as an unsigned 32 bit integer as
        follows:
        tag     value
        ints                    1m1000  numeric value
        floats          1m1001  floating point value
        pairs           1mr10.  ptr to pair
        aplied functor  1mr01.  ptr to functor followed by args
        ref                     0mr000  address of cell
        undefined               0mr000  pointing to itself

      */

#define AllTagBits  0xfc000000L
#define	TagBits	    0xbc000000L
#define	MaskAdr	    0x03ffffffL
#define	AdrHiBit    0x02000000L
#define	NumberTag   0xa0000000L
#define FloatTag    0xa4000000L
#define	AtomTag	    0x84000000L
#define	PairTag	    0x90000000L
#define	ApplTag	    0x88000000L
#define RefTag	    0x80000000L

#define MaskBits    6

#define PairBit         0x10000000L
#define ApplBit         0x08000000L
#define CompBits	0x18000000L
#define NumberMask	0xb8000000L
#define MAX_ABS_INT /* 0xfe00000LL */ ((((UInt)(1<<7))-1) << SHIFT_HIGH_TAG)

#define	TagOf(X)	(Unsigned(X) & TagBits)
#define	LowTagOf(X)	(Unsigned(X) & TagBits)
#define	NonTagPart(X)	(Signed(X) & MaskAdr)
#define TAGGEDA(TAG,V)	(TAG | Unsigned(V))
#define TAGGED(TAG,V)   (TAG | NonTagPart(Unsigned(V)))
#define NONTAGGED(TAG,V)   NonTagPart(Unsigned(V))
#define BitOn(Bit,V)    (Bit & Unsigned(V))
#define CHKTAG(t,Tag) 	((Unsigned(t)&TagBits)==Tag)

/* bits that should not be used by anyone but us */
#define YAP_PROTECTED_MASK 0x00000000L

#include "inline-only.h"

INLINE_ONLY bool IsVarTerm (Term);

INLINE_ONLY bool
IsVarTerm (Term t)
{
  return Signed (t) >= 0;
}



INLINE_ONLY bool IsNonVarTerm (Term);

INLINE_ONLY bool
IsNonVarTerm (Term t)
{
  return Signed (t) < 0;
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



INLINE_ONLY bool IsPairTerm (Term);

INLINE_ONLY bool
IsPairTerm (Term t)
{
  return BitOn (PairBit, (t));
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



INLINE_ONLY bool IsApplTerm (Term);

INLINE_ONLY bool
IsApplTerm (Term t)
{
  return BitOn (ApplBit, (t));
}



INLINE_ONLY bool IsAtomOrIntTerm (Term);

INLINE_ONLY bool
IsAtomOrIntTerm (Term t)
{
  return !(Unsigned (t) & CompBits);
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



static inline Int
IntOfTerm (Term t)
{
  Int n;
  n = (Unsigned (t) & MaskPrim) >> 2;

  if (Unsigned (t) & AdrHiBit)
    n |= 0xfc000000;
  return (n);
}
