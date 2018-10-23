/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		YapTags.h						 *
* mods:									 *
* comments:	Term Operations for YAP					 *
* version:      $Id: Yap.h,v 1.38 2008-06-18 10:02:27 vsc Exp $	 *
*************************************************************************/

#ifndef YAPTAGS_H

#define YAPTAGS_H 1

#ifndef EXTERN
#define EXTERN extern
#endif

#include "inline-only.h"

#ifndef SHORT_ADDRESSES
#define LONG_ADDRESSES 1
#else
#define LONG_ADDRESSES 0
#endif

/***********************************************************************/

/*
   absrectype Term = Int + Float + Atom + Pair + Appl + Ref + Var

   with AbsAppl(t) : *CELL -> Term
   and  RepAppl(t) : Term -> *CELL

   and  AbsPair(t) : *CELL -> Term
   and  RepPair(t) : Term -> *CELL

   and  IsIntTerm(t) = ...
   and  IsAtomTerm(t) = ...
   and  IsVarTerm(t) = ...
   and  IsPairTerm(t) = ...
   and  IsApplTerm(t) = ...
   and  IsFloatTerm(t) = ...
   and  IsRefTerm(t) = ...
   and  IsNonVarTerm(t) = ! IsVar(t)
   and  IsNumterm(t) = IsIntTerm(t) || IsFloatTerm(t)
   and  IsAtomicTerm(t) = IsNumTerm(t) || IsAtomTerm(t)
   and  IsPrimitiveTerm(t) = IsAtomicTerm(t) || IsRefTerm(t)

   and  MkIntTerm(n) = ...
   and  MkFloatTerm(f) = ...
   and  MkAtomTerm(a) = ...
   and  MkVarTerm(r) = ...
   and  MkApplTerm(f,n,args) = ...
   and  MkPairTerm(hd,tl) = ...
   and  MkRefTerm(R) = ...

   and  PtrOfTerm(t) : Term -> CELL * = ...
   and  IntOfTerm(t) : Term -> int = ...
   and  FloatOfTerm(t) : Term -> flt = ...
   and  AtomOfTerm(t) : Term -> Atom = ...
   and  VarOfTerm(t) : Term -> *Term = ....
   and  HeadOfTerm(t) : Term -> Term = ...
   and  TailOfTerm(t) : Term -> Term = ...
   and  FunctorOfTerm(t) : Term -> Functor = ...
   and  ArgOfTerm(i,t)  : Term -> Term= ...
   and  RefOfTerm(t) : Term -> DBRef = ...

 */

/*
   YAP can use several different tag schemes, according to the kind of
   machine we are experimenting with.
*/

#if LONG_ADDRESSES && defined(OLD_TAG_SCHEME)

#include "Tags_32bits.h"

#endif /* LONG_ADDRESSES && defined(OLD_TAG_SCHEME) */

/* AIX will by default place mmaped segments at 0x30000000. This is
        incompatible with the high tag scheme. Linux-ELF also does not like
        if you place things in the lower addresses (power to the libc people).
*/

#if defined(__APPLE__)
/* mmap on __APPLE__ is not the greatest idea. It overwrites memory allocated by
 * malloc */
#undef USE_DL_MALLOC
#ifndef USE_SYSTEM_MALLOC
#define USE_SYSTEM_MALLOC 1
#endif
#endif

#if SIZEOF_INT_P == 4
#define USE_LOW32_TAGS 1
#endif

#if SIZEOF_INT_P == 4 && !defined(OLD_TAG_SCHEME) && !defined(USE_LOW32_TAGS)

#include "Tags_32Ops.h"

#elif LONG_ADDRESSES && SIZEOF_INT_P == 4 && !defined(OLD_TAG_SCHEME) &&       \
    defined(USE_LOW32_TAGS)

#include "Tags_32LowTag.h"

#elif SIZEOF_INT_P == 8 && !defined(OLD_TAG_SCHEME)

#include "Tags_64bits.h"

// #elif !LONG_ADDRESSES
//
// #include "Tags_24bits.h"

#endif

#ifdef TAG_LOW_BITS_32

#if !GC_NO_TAGS
#define MBIT 0x80000000
#define RBIT 0x40000000

#if IN_SECOND_QUADRANT
#define INVERT_RBIT 1 /* RBIT is 1 by default */
#endif
#endif /* !GC_NO_TAGS  */

#else

#if !GC_NO_TAGS
#if defined(YAPOR_SBA) && defined(__linux__)
#define MBIT /* 0x20000000 */ MKTAG(0x1, 0) /* mark bit */
#else
#define RBIT /* 0x20000000 */ MKTAG(0x1, 0) /* relocation chain bit */
#define MBIT /* 0x40000000 */ MKTAG(0x2, 0) /* mark bit */
#endif
#endif /* !GC_NO_TAGS */

#endif

/*************************************************************************************************
                                              ???
*************************************************************************************************/

#define MkVarTerm() MkVarTerm__(PASS_REGS1)
#define MkPairTerm(A, B) MkPairTerm__(A, B PASS_REGS)

/*************************************************************************************************
                                   applies to unbound variables
*************************************************************************************************/

INLINE_ONLY Term *VarOfTerm(Term t);

INLINE_ONLY Term *VarOfTerm(Term t) { return (Term *)(t); }

#ifdef YAPOR_SBA

#define RESET_VARIABLE(V) (*(CELL *)(V) = 0)

INLINE_ONLY Term MkVarTerm__(USES_REGS1);

INLINE_ONLY Term MkVarTerm__(USES_REGS1) {
  return (Term)((*HR = 0, HR++));
}

INLINE_ONLY bool IsUnboundVar(Term *);

INLINE_ONLY bool IsUnboundVar(Term *t) { return (int)(*(t) ==
0); }

#else

#define RESET_VARIABLE(V) (*(CELL *)(V) = Unsigned(V))

INLINE_ONLY Term MkVarTerm__(USES_REGS1);

INLINE_ONLY Term MkVarTerm__(USES_REGS1) {
  return (Term)((*HR = (CELL)HR, HR++));
}

INLINE_ONLY bool IsUnboundVar(Term *);

INLINE_ONLY bool IsUnboundVar(Term *t) {
  return *(t) == (Term)(t);
}

#endif

INLINE_ONLY CELL *PtrOfTerm(Term);

INLINE_ONLY CELL *PtrOfTerm(Term t) {
  return (CELL *)(*(CELL *)(t));
}

INLINE_ONLY Functor FunctorOfTerm(Term);

INLINE_ONLY Functor FunctorOfTerm(Term t) {
  return (Functor)(*RepAppl(t));
}

#if USE_LOW32_TAGS

INLINE_ONLY Term MkAtomTerm(Atom);

INLINE_ONLY Term MkAtomTerm(Atom a) {
  return (Term)(AtomTag | (CELL)(a));
}

INLINE_ONLY Atom AtomOfTerm(Term t);

INLINE_ONLY Atom AtomOfTerm(Term t) {
  return (Atom)((~AtomTag & (CELL)(t)));
}

#else

INLINE_ONLY Term MkAtomTerm(Atom);

INLINE_ONLY Term MkAtomTerm(Atom at) {
  return (Term)(TAGGEDA((CELL)AtomTag, (CELL)(at)));
}

INLINE_ONLY Atom AtomOfTerm(Term t);

INLINE_ONLY Atom AtomOfTerm(Term t) {
  return (Atom)(NonTagPart(t));
}

#endif

INLINE_ONLY bool IsAtomTerm(Term);

INLINE_ONLY bool IsAtomTerm(Term t) {
  return CHKTAG((t), AtomTag);
}

INLINE_ONLY Term MkIntTerm(Int);

INLINE_ONLY
 Term MkIntTerm(Int n) {
  return (Term)(TAGGED(NumberTag, (n)));
}

/*
  A constant to subtract or add to a well-known term, we assume no
  overflow problems are possible
*/

INLINE_ONLY Term MkIntConstant(Int);

INLINE_ONLY
  Term MkIntConstant(Int n) {
  return (Term)(NONTAGGED(NumberTag, (n)));
}

INLINE_ONLY bool IsIntTerm(Term);

INLINE_ONLY
  bool IsIntTerm(Term t) {
  return CHKTAG((t), NumberTag);
}

INLINE_ONLY Term MkPairTerm__(Term head, Term tail USES_REGS);

INLINE_ONLY
 Term MkPairTerm__(Term head, Term tail USES_REGS) {
  CELL *p = HR;

  HR[0] = head;
  HR[1] = tail;
  HR += 2;
  return (AbsPair(p));
}

/* Needed to handle numbers:
        these two macros are fundamental in the integer/float conversions */

#ifdef M_WILLIAMS
#define IntInBnd(X) (TRUE)
#else
#ifdef TAGS_FAST_OPS
#define IntInBnd(X) (Unsigned(((Int)(X) >> (32 - 7)) + 1) <= 1)
#else
#define IntInBnd(X) ((X) < MAX_ABS_INT && (X) > -MAX_ABS_INT - 1L)
#endif
#endif

/*
  There are two types of functors:

  o Special functors mark special terms
  on the heap that should be seen as constants.

  o Standard functors mark normal applications.

*/

#include "TermExt.h"

#define IsAccessFunc(func) ((func) == FunctorAccess)

#ifdef YAP_H

#define MkIntegerTerm(i) __MkIntegerTerm(i PASS_REGS)

INLINE_ONLY Term __MkIntegerTerm(Int USES_REGS);

INLINE_ONLY Term __MkIntegerTerm(Int n USES_REGS) {
  return (Term)(IntInBnd(n) ? MkIntTerm(n) : MkLongIntTerm(n));
}
#endif

INLINE_ONLY bool IsIntegerTerm(Term);

INLINE_ONLY bool IsIntegerTerm(Term t) {
  return (int)(IsIntTerm(t) || IsLongIntTerm(t));
}

INLINE_ONLY Int IntegerOfTerm(Term);

INLINE_ONLY Int IntegerOfTerm(Term t) {

  return (Int)(IsIntTerm(t) ? IntOfTerm(t) : LongIntOfTerm(t));
}

#ifdef YAP_H

#define MkAddressTerm(i) __MkAddressTerm(i PASS_REGS)

INLINE_ONLY Term __MkAddressTerm(void *USES_REGS);

INLINE_ONLY Term __MkAddressTerm(void *n USES_REGS) {
  return __MkIntegerTerm((Int)n PASS_REGS);
}

#endif

INLINE_ONLY bool IsAddressTerm(Term);

INLINE_ONLY bool IsAddressTerm(Term t) {
  return (bool)IsIntegerTerm(t);
}

INLINE_ONLY void *AddressOfTerm(Term);

INLINE_ONLY void *AddressOfTerm(Term t) {
  return (void *)(IsIntTerm(t) ? IntOfTerm(t) : LongIntOfTerm(t));
}


INLINE_ONLY Int IsPairTermOrNil (Term);

INLINE_ONLY Int
IsPairOrNilTerm (Term t)
{
  return IsPairTerm(t) || t == TermNil;
}


#endif
