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
#define ONHEAP(ptr) (CellPtr(ptr) >= H0  && CellPtr(ptr) < HR)

/* is ptr a pointer to code space? */
#if USE_SYSTEM_MALLOC
#define ONCODE(ptr) (Addr(ptr) < LOCAL_GlobalBase || Addr(ptr) > LOCAL_TrailTop)
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

#ifdef TAG_64BITS00

#define  MARK_BIT MKTAG(0x2,0x0)
#define RMARK_BIT MKTAG(0x4,0x0)

#define MARKED_PTR(P) MARKED_PTR__(P PASS_REGS) 
#define UNMARKED_CELL(P) MARKED_PTR__(P PASS_REGS) 
#define UNMARKED_MARK(P, BP) UNMARKED_MARK__(P, BP PASS_REGS) 
#define MARK(P) MARK__(P PASS_REGS) 
#define UNMARK(P) UNMARK__(P PASS_REGS) 
#define RMARK(P) RMARK__(P PASS_REGS) 
#define RMARKED(P) RMARKED__(P PASS_REGS) 
#define UNRMARK(P) UNRMARK__(P PASS_REGS) 

static inline Int
MARKED_PTR__(CELL* ptr USES_REGS)
{
  return (CELL)ptr & MARK_BIT;
}

static inline Int
UNMARKED_MARK__(CELL* ptr, char *bp USES_REGS)
{
  CELL t = *ptr;
  if (t & MARK_BIT) {
    return true;
  }
  *ptr = t | MARK_BIT;
  return false;
}

static inline void
MARK__(CELL* ptr USES_REGS)
{
  CELL t = *ptr;
  *ptr = t | MARK_BIT;
}

static inline void
UNMARK__(CELL* ptr USES_REGS)
{
  *ptr  &= ~MARK_BIT;
}

/* not really that useful */
#define MAY_UNMARK(X)

#define UNMARK_CELL(X) (X)

static inline void
RMARK__(CELL* ptr USES_REGS)
{
   *ptr |= RMARK_BIT;
}

static inline void
UNRMARK__(CELL* ptr USES_REGS)
{
  *ptr  &= ~RMARK_BIT;
}

static inline int
RMARKED__(CELL* ptr USES_REGS)
{
  return *ptr & RMARK_BIT;
}

#else

#define  MARK_BIT ((char)1)
#define RMARK_BIT ((char)2)

#define mcell(X)  LOCAL_bp[(X)-(CELL *)LOCAL_GlobalBase]

#define MARKED_PTR(P) MARKED_PTR__(P PASS_REGS) 
#define UNMARKED_MARK(P, BP) UNMARKED_MARK__(P, BP PASS_REGS) 
#define MARK(P) MARK__(P PASS_REGS) 
#define UNMARK(P) UNMARK__(P PASS_REGS) 
#define RMARK(P) RMARK__(P PASS_REGS) 
#define RMARKED(P) RMARKED__(P PASS_REGS) 
#define UNRMARK(P) UNRMARK__(P PASS_REGS) 

static inline Int
MARKED_PTR__(CELL* ptr USES_REGS)
{
  return mcell(ptr) & MARK_BIT;
}

static inline Int
UNMARKED_MARK__(CELL* ptr, char *bp USES_REGS)
{
  Int pos = ptr - (CELL *)LOCAL_GlobalBase;
  char t = bp[pos];
  if (t & MARK_BIT) {
    return TRUE;
  }
  bp[pos] = t | MARK_BIT;
  return FALSE;
}

static inline void
MARK__(CELL* ptr USES_REGS)
{
  Int pos = ptr - (CELL *)LOCAL_GlobalBase;
  char t = LOCAL_bp[pos];
  LOCAL_bp[pos] = t | MARK_BIT;
}

static inline void
UNMARK__(CELL* ptr USES_REGS)
{
  mcell(ptr) = mcell(ptr) & ~MARK_BIT;
}

/* not really that useful */
#define MAY_UNMARK(X)

#define UNMARK_CELL(X) (X)

static inline void
RMARK__(CELL* ptr USES_REGS)
{
   mcell(ptr) = mcell(ptr) | RMARK_BIT;
}

static inline void
UNRMARK__(CELL* ptr USES_REGS)
{
   mcell(ptr) = mcell(ptr) & ~RMARK_BIT;
}

static inline int
RMARKED__(CELL* ptr USES_REGS)
{
  return mcell(ptr) & RMARK_BIT;
}

#endif

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

void  Yap_mark_variable(CELL *);
void  Yap_mark_external_reference(CELL *);
void  Yap_inc_mark_variable(void);


