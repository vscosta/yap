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
* version:      $Id: Tags_64ops.h.m4,v 1.2 2001-09-24 18:07:16 vsc Exp $	 *
*************************************************************************/

/*

    Version for 32 bit addresses machines,
   Each	term is	represented internally as an unsigned 32 bit integer as
   follows:
			tag	 value
   ints			1m1....01 numeric value
   atoms		1m0....01 offset of atom entry
   pairs		1mr....11 ptr to pair
   aplied functor	1mr....00 ptr to functor followed by args
   undefined		0mr....00 address of cell pointing to itself

 functors are represented as ptrs to the functor entry in the atom
property list

	This version speeds up access to lists and to compound
terms by using the XOR and NOT operations to build their tags. This
saves operations on RISC machines.

	As a further optimisation, only pairs or compound terms have
the second lowest bit set. This allows one to recognise lists or
compound terms with a single operation.

 The main problem is that the default value of the M and R bits for GC
are now 1 in compound terms and structures.

*/

#   define SHIFT_HIGH_TAG  61

#define MKTAG(HI,LO)   ((((UInt) (HI))<<SHIFT_HIGH_TAG)|(LO))

#define	TagBits	    /* 0xb0000003L */ MKTAG(0x5,3)
#define LowTagBits  /* 0x00000003L */ MKTAG(0x0,3)
#define	LowBit	    /* 0x00000001L */ MKTAG(0x0,1)
#define HighTagBits /* 0xf0000000L */ MKTAG(0x7,0)
#define	AdrHiBit    /* 0x08000000L */ (((UInt)1) << (SHIFT_HIGH_TAG-1))
#define	MaskAdr	    /* 0x1ffffffcL */ ((((UInt)1) << (SHIFT_HIGH_TAG+1))-4)
#define MaskPrim    /* 0x0ffffffcL */ ((((UInt)1) << (SHIFT_HIGH_TAG))-4)
#define	NumberTag   /* 0xb0000001L */ MKTAG(0x5,2)
#define	AtomTag	    /* 0x90000001L */ MKTAG(0x4,2)
#define MAX_ABS_INT /* 0xfe00000LL */ ((Int)0x0400000000000000L)

/* bits that should not be used by anyone but us */
#define YAP_PROTECTED_MASK 0xe000000000000000L

#define MaskBits    4

/*
	UNIQUE_TAG_FOR_PAIR gives the representation for pair an
        unique tag

	This allows optimisation of switch_list

*/
#if defined(i386) || defined(sparc) || defined(_POWER) || defined(__sparc)
#define UNIQUE_TAG_FOR_PAIRS 1
#endif

#if UNIQUE_TAG_FOR_PAIRS
#define	PairBit	    /* 0x00000001L */ 1
#define	ApplBit	    /* 0x00000000L */ 0
#else
#define	PairBit	    /* 0x00000000L */ 0
#define	ApplBit	    /* 0x00000001L */ 1
#endif

#define	NonTagPart(X)	(Signed(X) & MaskPrim)
#define TAGGEDA(TAG,V)	(TAG | Unsigned(V))
#define TAGGED(TAG,V)   (TAG | NonTagPart(Unsigned(V)<<2))
#define NONTAGGED(TAG,V)   NonTagPart(Unsigned(V)<<2)
#define BitOn(Bit,V)    (Bit & Unsigned(V))
#define CHKTAG(t,Tag) 	((Unsigned(t)&TagBits)==Tag)

/* never forget to surround arguments to a macro by brackets */ 
Inline(IsVarTerm, int, Term, t, Signed(t) >= 0)
Inline(IsNonVarTerm, int, Term, t, Signed(t) < 0)
#if UNIQUE_TAG_FOR_PAIRS
Inline(RepPair, Term *, Term, t, (~(t)))
Inline(AbsPair, Term, Term *, p, (~Unsigned(p)))
Inline(IsPairTerm, Int, Term, t, ((t) & PairBit))
Inline(RepAppl, Term *, Term, t, (-(t)))
Inline(AbsAppl, Term, Term *, p, (-Unsigned(p)))
Inline(IsApplTerm, Int, Term, t, (!((t) & LowTagBits)))
#else
Inline(RepPair, Term *, Term, t, (-(t)))
Inline(AbsPair, Term, Term *, p, (-Unsigned(p)))
Inline(IsPairTerm, Int, Term, t, (!((t) & LowTagBits)))
Inline(RepAppl, Term *, Term, t, (~(t)))
Inline(AbsAppl, Term, Term *, p, (~Unsigned(p)))
Inline(IsApplTerm, Int, Term, t, ((t) & ApplBit))
#endif
Inline(IsAtomOrIntTerm, Int, Term, t, (Unsigned(t) & LowTagBits == 0x2))

Inline(IntOfTerm, Int, Term, t, (((Int)((t) << 3)) >> 5))

Inline2(AdjustPtr, Term, Term, t, Term, off, (IsVarTerm(t) ? (t)+off : (IsPairTerm(t) ?  AbsPair((CELL *)(Unsigned(RepPair(t))+off)) : AbsAppl((CELL *)(Unsigned(RepAppl(t))+off)))))
Inline2(AdjustIDBPtr, Term, Term, t, Term, off, (IsVarTerm(t) ? (t)+off : (IsPairTerm(t) ?  AbsPair((CELL *)(Unsigned(RepPair(t))+off)) : AbsAppl((CELL *)(Unsigned(RepAppl(t))+off)))))

