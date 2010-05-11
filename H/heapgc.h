/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		heapgc.c						 *
* Last rev:								 *
* mods:									 *
* comments:	Header for Global Stack garbage collector                *
*									 *
*************************************************************************/



/* macros used by garbage collection */

#if TAG_64BITS
#define MaskAdr		(~((CELL)0x7))
#endif

/* return pointer from object pointed to by ptr (remove tag & mark) */
#ifdef TAGS_FAST_OPS
#define GET_NEXT(val)  ((CELL *)(IsVarTerm((val)) ?                          \
                                 (val) & MaskAdr :                           \
                                 ( IsPairTerm((val)) ?                       \
			            Unsigned(RepPair((val))) & MaskAdr :     \
                                    ( IsApplTerm((val)) ?                    \
                                      Unsigned(RepAppl((val))) & MaskAdr :   \
                                      (val) & MaskAdr                        \
                                    )                                        \
                                  )                                          \
                                 )                                           \
                        ) 
#else
#ifdef  TAG_LOW_BITS_32
#define GET_NEXT(val)  ((CELL *) ((val) & ~LowTagBits))
#else
#define GET_NEXT(val)  ((CELL *) ((val) & MaskAdr))
#endif
#endif

/* is ptr a pointer to the heap? */
#define ONHEAP(ptr) (CellPtr(ptr) >= H0  && CellPtr(ptr) < H)

/* is ptr a pointer to code space? */
#if USE_SYSTEM_MALLOC
#define ONCODE(ptr) (Addr(ptr) < Yap_GlobalBase || Addr(ptr) > Yap_TrailTop)
#else
#define ONCODE(ptr) (Addr(ptr) < HeapTop && Addr(ptr) >= Yap_HeapBase)
#endif

/* is val pointing to something bound to the heap? */


#define GCIsPrimitiveTerm(X)    (/* not really needed !IsVarTerm(X) && */ IsAtomOrIntTerm(X))

/* Does X point to an object in the heap */
#define HEAP_PTR(val)    (!GCIsPrimitiveTerm(val) && ONHEAP(GET_NEXT(val)))

/* 
   Heap_trail_entry must be very careful. We are looking at a valid
   trail entry if: it was between H0 and HB or between B and LCLO
   (that is, if it was covered by choicepoints at the time), and if it
   was a heap pointer.

   We can join the two conditions: (H0 =< val < HB || ((B < val < LCL0)
					&& H0 <= *DETAG(val) < H))
*/
#define HEAP_TRAIL_ENTRY(val) ((IsVarTerm(val)) &&                  \
				((H0 <= CellPtr(val) && CellPtr(val)\
				< cp_H) ||                          \
			       (CellPtr(B) < CellPtr(val) && CellPtr(val) <= \
				LCL0 && HEAP_PTR(val))))

#if !defined(YAPOR) && !defined(THREADS)
extern char *Yap_bp;
#endif

#define  MARK_BIT ((char)1)
#define RMARK_BIT ((char)2)

#define mcell(X)  Yap_bp[(X)-(CELL *)Yap_GlobalBase]

static inline Int
MARKED_PTR(CELL* ptr)
{
  return mcell(ptr) & MARK_BIT;
}

static inline Int
UNMARKED_MARK(CELL* ptr, char *bp)
{
  Int pos = ptr - (CELL *)Yap_GlobalBase;
  char t = bp[pos];
  if (t & MARK_BIT) {
    return TRUE;
  }
  bp[pos] = t | MARK_BIT;
  return FALSE;
}

static inline void
MARK(CELL* ptr)
{
  mcell(ptr) = mcell(ptr) | MARK_BIT;
}

static inline void
UNMARK(CELL* ptr)
{
  mcell(ptr) = mcell(ptr) & ~MARK_BIT;
}

/* not really that useful */
#define MAY_UNMARK(X)

#define UNMARK_CELL(X) (X)

static inline void
RMARK(CELL* ptr)
{
   mcell(ptr) = mcell(ptr) | RMARK_BIT;
}

static inline void
UNRMARK(CELL* ptr)
{
   mcell(ptr) = mcell(ptr) & ~RMARK_BIT;
}

static inline int
RMARKED(CELL* ptr)
{
  return mcell(ptr) & RMARK_BIT;
}


/* is the object pointed to by ptr marked as in a relocation chain? */

#if LONG_ADDRESSES
#ifdef TAG_LOW_BITS_32
#define TAG(X)         ((X) & LowTagBits)
#else
#ifdef TAG_64BITS
#define TAG(X)         ((X) & MKTAG(0x0,0x7))
#else
#define TAG(X)         ((X) & 0x80000003L)
#endif
#endif
#else
#define TAG(X)         ((X) & 0x98000000L)
#endif

typedef CELL   *CELL_PTR;

#define ENVSIZE(E) 	EnvSize(((CELL *)E)[E_CP])

void  STD_PROTO(Yap_mark_variable, (CELL *));
void  STD_PROTO(Yap_mark_external_reference, (CELL *));
void  STD_PROTO(Yap_inc_mark_variable, (void));



