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
#define MaskAdr		(~(MBIT|RBIT|0x7L))
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
#if INVERT_RBIT
#define GET_NEXT(val)  ((CELL *) (((val) & ~(LowTagBits|MBIT))|RBIT))
#else
#define GET_NEXT(val)  ((CELL *) ((val) & ~(LowTagBits|MBIT|RBIT)))
#endif
#else
#define GET_NEXT(val)  ((CELL *) ((val) & MaskAdr))
#endif
#endif

/* is ptr a pointer to the heap? */
#define ONHEAP(ptr) (CellPtr(ptr) >= H0  && CellPtr(ptr) < H)

/* is ptr a pointer to code space? */
#define ONCODE(ptr) (Addr(ptr) < HeapTop && Addr(ptr) >= Yap_HeapBase)

/* is val pointing to something bound to the heap? */


#define GCIsPrimitiveTerm(X)    (!IsVarTerm(X) && IsAtomOrIntTerm(X))

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

/* is the object pointed to by ptr marked? */
#ifdef TAGS_FAST_OPS
#define MARKED_VAR(val) ((val) &  MBIT) 

#define MARKED_COMP(val) (!((val) &  MBIT))

#define MARKED(val)    ((Int)(val) < 0 && (((val) & LowTagBits) != 2)\
			? \
			!((val) & MBIT) : ((val) & MBIT))
#else
#define MARKED(val)    ((val) &  MBIT) 
#endif



#define MARK(ptr)      (*(ptr) ^= MBIT) /* mark the object pointed to by ptr */

#define MARK_CELL(val) ((val) ^ MBIT)   /* mark the object pointed to by ptr */

#define UNMARK(ptr)    (*(ptr) ^= MBIT) /* unmark the object pointed to by ptr */

#define UNMARK_CELL(val)    ((val) ^ MBIT) /* unmark the object pointed to by ptr */

#ifdef TAGS_FAST_OPS
#define RMARKED(val)    (!GCIsPrimitiveTerm(val) && (IsVarTerm(val) ?\
				((val) & RBIT) : !((val) & RBIT)))

#define UNMARKED(val)   ((Int)(val) < 0 && (((val) & LowTagBits) != 2)\
			? \
			((val) | MBIT) : ((val) & ~MBIT))
#define ENSURE_MARKED(val)   ((Int)(val) < 0 && (((val) & LowTagBits) != 2)\
			? \
			((val) & ~MBIT) : ((val) | MBIT))
#else
#if INVERT_RBIT
#define RMARKED(val)   (!GCIsPrimitiveTerm(val) && !((val) & RBIT))
#else
#define RMARKED(val)   (!GCIsPrimitiveTerm(val) && ((val) & RBIT))
#endif
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

void  STD_PROTO(Yap_mark_variable, (CELL *));
void  STD_PROTO(Yap_mark_external_reference, (CELL *));
void  STD_PROTO(Yap_inc_mark_variable, (void));



