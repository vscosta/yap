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
* version:      $Id: Tags_32bits.h.m4,v 1.1.1.1 2001-04-09 19:53:46 vsc Exp $	 *
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

Inline(IsVarTerm, int, Term, t, Signed(t) >= 0)
Inline(IsNonVarTerm, int, Term, t, Signed(t) < 0)
Inline(RepPair, Term *, Term, t, NonTagPart(t))
Inline(AbsPair, Term, Term *, p, TAGGEDA(PairTag, (p)))
Inline(IsPairTerm, Int, Term, t, BitOn(PairBit, (t)))
Inline(RepAppl, Term *, Term, t, NonTagPart(t))
Inline(AbsAppl, Term, Term *, p, TAGGEDA(ApplTag, (p)))
Inline(IsApplTerm, Int, Term, t, BitOn(ApplBit, (t)))
Inline(IsAtomOrIntTerm, int, Term, t, ((Unsigned(t) & LowTagBits) == 0))

Inline2(AdjustPtr, Term, Term, t, Term, off, (t)+off)
Inline2(AdjustIDBPtr, Term, Term, t, Term, off, (t)+off)

Inline(IntOfTerm, Int, Term, t, ((Int)(t << 3))>>(3+2))


