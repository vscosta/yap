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
/* mmap on __APPLE__ is not the greatest idea. It overwrites memory allocated by malloc */
#undef USE_DL_MALLOC
#ifndef USE_SYSTEM_MALLOC
#define USE_SYSTEM_MALLOC 1
#endif
#elif (defined(_AIX) || (defined(__APPLE__) && !defined(__LP64__)) || defined(_WIN32) || defined(sparc) || defined(__sparc) || defined(mips) || defined(__FreeBSD__) || defined(_POWER) || defined(__POWERPC__) || defined(__linux__) || defined(IN_SECOND_QUADRANT) || defined(__CYGWIN__)) || defined(__NetBSD__) || defined(__DragonFly__)
#define USE_LOW32_TAGS 1
#endif

#if LONG_ADDRESSES && SIZEOF_INT_P==4 && !defined(OLD_TAG_SCHEME) && !defined(USE_LOW32_TAGS)

#include "Tags_32Ops.h"

#endif /* LONG_ADDRESSES && !defined(OLD_TAG_SCHEME) && !defined(USE_LOW32_TAGS) */

#if LONG_ADDRESSES && SIZEOF_INT_P==4 && !defined(OLD_TAG_SCHEME) && defined(USE_LOW32_TAGS)

#include "Tags_32LowTag.h"

#endif /* LONG_ADDRESSES && !defined(OLD_TAG_SCHEME) */

#if LONG_ADDRESSES && SIZEOF_INT_P==8 && !defined(OLD_TAG_SCHEME)

#include "Tags_64bits.h"

#endif /* LONG_ADDRESSES && SIZEOF_INT_P==8 && !defined(OLD_TAG_SCHEME) */

#if !LONG_ADDRESSES

#include "Tags_24bits.h"

#endif /* !LONG_ADDRESSES */

#ifdef TAG_LOW_BITS_32

#if !GC_NO_TAGS
#define MBIT     0x80000000
#define RBIT     0x40000000

#if IN_SECOND_QUADRANT
#define INVERT_RBIT 1		/* RBIT is 1 by default */
#endif
#endif /* !GC_NO_TAGS  */

#else

#if !GC_NO_TAGS
#if defined(YAPOR_SBA) && defined(__linux__)
#define MBIT     /* 0x20000000 */ MKTAG(0x1,0)	/* mark bit */
#else
#define RBIT     /* 0x20000000 */ MKTAG(0x1,0)	/* relocation chain bit */
#define MBIT     /* 0x40000000 */ MKTAG(0x2,0)	/* mark bit */
#endif
#endif /* !GC_NO_TAGS */

#endif

#define	TermSize    sizeof(Term)
