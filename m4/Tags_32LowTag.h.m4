/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		Tags_32LowTag.h.m4					 *
* Last rev:	December 90						 *
* mods:									 *
* comments:	Original Tag Scheme for machines with 32 bits adresses   *
* version:      $Id: Tags_32LowTag.h.m4,v 1.3 2003-06-06 13:16:40 vsc Exp $	 *
*************************************************************************/

#define TAG_LOW_BITS_32 1

     /*    Version for 32 bit addresses machines,
   Each	term is	represented internally as an unsigned 32 bit integer as
   follows:
			tag	 value
   ints			m.....110 numeric value
   atoms		m.....010 offset of atom entry
   pairs		mr.....11 ptr to pair
   aplied functor	mr.....01 ptr to functor followed by args
   ref			mr.....00 address of cell
   undefined		mr.....00 address of cell pointing to itself

 functors are represented as ptrs to the functor entry in the atom
property list

*/

#define SHIFT_LOW_TAG 2
#define SHIFT_HIGH_TAG 2

#define MKTAG(HI,LO)   ((((UInt) (HI))<<SHIFT_HIGH_TAG)|(LO))

#define	TagBits	    /* 0x00000007L */ MKTAG(0x1,3)
#define LowTagBits  /* 0x00000003L */ MKTAG(0x0,3)
#define	LowBit	    /* 0x00000001L */ MKTAG(0x0,1)
#define HighTagBits /* 0x0000000cL */ MKTAG(0x1,0)
#define	NumberTag   /* 0x0000000dL */ MKTAG(0x1,2)
#define	AtomTag	    /* 0x00000006L */ MKTAG(0x0,2)

/*
	subtract the total for tag bits, plus 1 bit for GC, plus another
	for sign
*/
#define MAX_ABS_INT ((Int)0x04000000L)

/*
	UNIQUE_TAG_FOR_PAIR gives the representation for pair an
        unique tag

	This allows optimisation of switch_list

*/
#define UNIQUE_TAG_FOR_PAIRS 1

#define	PairBits    /* 0x00000003L */ MKTAG(0x0,3)
#define	ApplBit	    /* 0x00000001L */ MKTAG(0x0,1)
#define PrimiBits   /* 0x00000002L */ MKTAG(0x0,2)
#define NumberBits  /* 0x0000000aL */ MKTAG(0x2,2)
#define NumberMask  /* 0x0000000bL */ MKTAG(0x2,3)

#define	TagOf(V)	(Unsigned(V) & LowTagBits)
#define	NonTagPart(V)	((Unsigned(V)>>1) & ~LowTagBits)
#define TAGGED(TAG,V)	(((Unsigned(V)<<(SHIFT_HIGH_TAG+SHIFT_LOW_TAG+1))>>1)|(TAG))
#define NONTAGGED(TAG,V) ((Unsigned(V)<<(SHIFT_HIGH_TAG+SHIFT_LOW_TAG+1))>>1)
#define TAGGEDA(TAG,V)	((Unsigned(V) << 1)|(TAG))
#define CHKTAG(t,Tag) 	((Unsigned(t)&TagBits)==Tag)

/* bits that should not be used by anyone but us */
#define YAP_PROTECTED_MASK 0xc0000000L

Inline(IsVarTerm, int, Term, t, !((t) & LowTagBits))
Inline(IsNonVarTerm, int, Term, t, ((t) & LowTagBits))
Inline(RepPair, Term *, Term, t, (t)-PairBits)
Inline(AbsPair, Term, Term *, p, Unsigned(p)+PairBits)
Inline(IsPairTerm, Int, Term, t, (((t) & LowTagBits) == PairBits))
Inline(RepAppl, Term *, Term, t, ((t)-ApplBit))
Inline(AbsAppl, Term, Term *, p, Unsigned(p)+ApplBit)
Inline(IsApplTerm, Int, Term, t, (((t) & LowTagBits) == ApplBit))
Inline(IsAtomOrIntTerm, Int, Term, t, (((t) & LowTagBits) == 2))

Inline2(AdjustPtr, Term, Term, t, Term, off, (t)+off)
Inline2(AdjustIDBPtr, Term, Term, t, Term, off, (t)+off)

Inline(IntOfTerm, Int, Term, t, ((Int)(t << 1))>>(SHIFT_LOW_TAG+SHIFT_HIGH_TAG+1))

