






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
* version:      $Id: Tags_32Ops.h,v 1.3 2008-01-30 10:35:43 vsc Exp $	 *
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

#if SIZEOF_INT_P==4 && !defined(USE_LOW32_TAGS)

#define TAGS_FAST_OPS 1

#define SHIFT_HIGH_TAG  29

#define MKTAG(HI,LO)   ((((UInt) (HI))<<SHIFT_HIGH_TAG)|(LO))

#define	TagBits	    /* 0xb0000003L */ MKTAG(0x5,3)
#define LowTagBits  /* 0x00000003L */ MKTAG(0x0,3)
#define	LowBit	    /* 0x00000001L */ MKTAG(0x0,1)
#define HighTagBits /* 0xf0000000L */ MKTAG(0x7,0)
#define	AdrHiBit    /* 0x08000000L */ (((UInt)1) << (SHIFT_HIGH_TAG-1))
#define	MaskAdr	    /* 0x1ffffffcL */ ((((UInt)1) << (SHIFT_HIGH_TAG))-4)
#define MaskPrim    /* 0x0ffffffcL */ ((((UInt)1) << (SHIFT_HIGH_TAG))-4)
#define	NumberTag   /* 0xb0000001L */ MKTAG(0x5,2)
#define	AtomTag	    /* 0x90000001L */ MKTAG(0x4,2)
#define MAX_ABS_INT /* 0xfe00000LL */ ((Int)0x04000000L)

/* bits that should not be used by anyone but us */
#define YAP_PROTECTED_MASK 0xe0000000L

#define MaskBits    4

/*
	UNIQUE_TAG_FOR_PAIR gives the representation for pair an
        unique tag

	This allows optimisation of switch_list

*/
#if defined(i386) || defined(sparc) || defined(_POWER) || defined(__POWERPC__) || defined(__sparc)
#define UNIQUE_TAG_FOR_PAIRS 1
#endif

#if UNIQUE_TAG_FOR_PAIRS
#define	PairBit	    /* 0x00000001L */ 1
#define	ApplBit	    /* 0x00000000L */ 0
#else
#define	PairBit	    /* 0x00000000L */ 0
#define	ApplBit	    /* 0x00000001L */ 1
#endif

#define TagOf(t) 	(Unsigned(t)&TagBits)
#define LowTagOf(t) 	(Unsigned(t)&TagBits)
#define	NonTagPart(X)	(Signed(X) & MaskPrim)
#define TAGGEDA(TAG,V)	(TAG | Unsigned(V))
#define TAGGED(TAG,V)   (TAG | NonTagPart(Unsigned(V)<<2))
#define NONTAGGED(TAG,V)   NonTagPart(Unsigned(V)<<2)
#define BitOn(Bit,V)    (Bit & Unsigned(V))
#define CHKTAG(t,Tag) 	((Unsigned(t)&TagBits)==Tag)

/* never forget to surround arguments to a macro by brackets */

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


#if UNIQUE_TAG_FOR_PAIRS

INLINE_ONLY Term *RepPair (Term);

INLINE_ONLY Term *
RepPair (Term t)
{
  return (Term *) ((~(t)));
}



INLINE_ONLY Term AbsPair (Term *);

INLINE_ONLY Term
AbsPair (Term * p)
{
  return (Term) ((~Unsigned (p)));
}



INLINE_ONLY Int IsPairTerm (Term);

INLINE_ONLY Int
IsPairTerm (Term t)
{
  return (Int) (((t) & PairBit));
}



INLINE_ONLY Term *RepAppl (Term);

INLINE_ONLY Term *
RepAppl (Term t)
{
  return (Term *) ((-Signed (t)));
}



INLINE_ONLY Term AbsAppl (Term *);

INLINE_ONLY Term
AbsAppl (Term * p)
{
  return (Term) ((-Signed (p)));
}



INLINE_ONLY Int IsApplTerm (Term);

INLINE_ONLY Int
IsApplTerm (Term t)
{
  return (Int) ((!((t) & LowTagBits)));
}


#else

INLINE_ONLY Term *RepPair (Term);

INLINE_ONLY Term *
RepPair (Term t)
{
  return (Term *) ((-Signed (t)));
}



INLINE_ONLY Term AbsPair (Term *);

INLINE_ONLY Term
AbsPair (Term * p)
{
  return (Term) (((CELL) (-Signed (p))));
}



INLINE_ONLY Int IsPairTerm (Term);

INLINE_ONLY Int
IsPairTerm (Term t)
{
  return (Int) ((!((t) & LowTagBits)));
}



INLINE_ONLY Term *RepAppl (Term);

INLINE_ONLY Term *
RepAppl (Term t)
{
  return (Term *) ((~(t)));
}



INLINE_ONLY Term AbsAppl (Term *);

INLINE_ONLY Term
AbsAppl (Term * p)
{
  return (Term) ((~Unsigned (p)));
}



INLINE_ONLY Int IsApplTerm (Term);

INLINE_ONLY Int
IsApplTerm (Term t)
{
  return (Int) (((t) & ApplBit));
}


#endif

INLINE_ONLY Int IsAtomOrIntTerm (Term);

INLINE_ONLY Int
IsAtomOrIntTerm (Term t)
{
  return (Int) (((Unsigned (t) & LowTagBits) == 0x2));
}




INLINE_ONLY Int IntOfTerm (Term);

INLINE_ONLY Int
IntOfTerm (Term t)
{
  return (Int) ((Int) (Unsigned (t) << 3) >> 5);
}



#if UNIQUE_TAG_FOR_PAIRS

INLINE_ONLY Term AdjustPtr (Term t, Term off);

INLINE_ONLY Term
AdjustPtr (Term t, Term off)
{
  return (Term) (((IsVarTerm (t)
		   || IsAtomOrIntTerm (t)) ? (t) +
		  (off) : (IsPairTerm (t) ? (CELL)
			   AbsPair ((CELL *) ((CELL) RepPair (t) +
					      (off))) : (t) - (off))));
}



INLINE_ONLY Term AdjustIDBPtr (Term t, Term off);

INLINE_ONLY Term
AdjustIDBPtr (Term t, Term off)
{
  return (Term) (IsVarTerm (t) ? (t) + (off) : (t) - (off));
}


#else

INLINE_ONLY Term AdjustPtr (Term t, Term off);

INLINE_ONLY Term
AdjustPtr (Term t, Term off)
{
  return (Term) (((IsVarTerm (t)
		   || IsAtomOrIntTerm (t)) ? (t) +
		  (off) : (IsApplTerm (t) ? (CELL)
			   AbsAppl ((CELL *) ((CELL) RepAppl (t) +
					      (off))) : (t) - (off))));
}



INLINE_ONLY Term AdjustIDBPtr (Term t, Term off);

INLINE_ONLY Term
AdjustIDBPtr (Term t, Term off)
{
  return (Term) (IsVarTerm (t) ? (t) +
		 (off) : (IsApplTerm (t) ? (CELL)
			  AbsAppl ((CELL *) ((CELL) RepAppl (t) +
					     (off))) : (t) - (off)));
}


#endif

#endif /* SIZEOF_INT_P==4 */



