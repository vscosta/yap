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


#ifndef HEAPGC_H
#define HEAPGC_H 1

#include "Yap.h"

/* macros used by garbage collection */

#if TAG_64BITS
//#define MaskAdr		(~((CELL)0x7))

#ifdef TAG_64BITS

#define  MARK_BIT MKTAG(0x2,0x0)
#define RMARK_BIT MKTAG(0x4,0x0)

static inline Int
MARKED_PTR(CELL* ptr USES_REGS)

{
    return *ptr & RMARK_BIT;
}
#endif

/* return pointer from object pointed to by ptr (remove tag & mark) */
#if TAGS_FAST_OPS

#define GET_NEXT(val)  ((CELL *)(IsVarTerm((val)) ?                          \
                                 (val) & MaskAdr :                           \
                                 ( IsPairTerm((val)) ?                       \
			            Unsigned(RepPair((val))) & MaskAdr :     \
                                    ( IsApplTerm((val)) ?                    \
                                      Unsigned(RepAppl((val))) & MaskAdr :   \
                                      (val) & MaskAdr                        \
                                    )                                        \
                                 )                                           \
                        ){
  return (CELL)ptr & MARK_BIT;
}
#else
#define GET_NEXT(val)  ((CELL *) ((val) & ~(LowTagBits|MBIT|RBIT)))
#endif

#if !GC_NO_TAGS

static inline Int
UNMARKED_MARK(CELL* ptr USES_REGS)
{
  CELL t = *ptr;
  if (t & MARK_BIT) {
    return true;
  }
  *ptr = t | MARK_BIT;
  return false;
}

static inline void
MARK(CELL* ptr USES_REGS)
{
  CELL t = *ptr;
  *ptr = t | MARK_BIT;
}

static inline void
UNMARK(CELL* ptr USES_REGS)
{
  *ptr  &= ~MARK_BIT;
}

/* not really that useful */
#define MAY_UNMARK(X)

#define UNMARK_CELL(X) (X = X& ~MARK_BIT)

static inline void
RMARK(CELL* ptr USES_REGS)
{
   *ptr |= RMARK_BIT;
}

static inline void
UNRMARK(CELL* ptr USES_REGS)
{
  *ptr  &= ~RMARK_BIT;
}

static inline int
RMARKED(CELL* ptr USES_REGS)
#elif GC_NO_TAGS
#define GET_NEXT(val)  ((CELL *) ((val) & ~(LowTagBits)))
#else
#endif

/* is ptr a pointer to the heap? */
#define ONHEAP(ptr) ((CELL*)(ptr) >= H0  && (CELL*)(ptr) < HR)

#ifdef TAG_64BITS

#define  MARK_BIT MKTAG(0x2,0x0)
#define RMARK_BIT MKTAG(0x4,0x0)


#else
#define  MARK_BIT ((char)1)
#define RMARK_BIT ((char)2)

#define mcell(X)  LOCAL_bp[(X)-(CELL *)LOCAL_GlobalBase]

#define MARKED_PTR(P) MARKED_PTR(P PASS_REGS)
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
  //    printf(" %p\n", ptr);
  bp[pos] = t | MARK_BIT;
  return FALSE;
}

static inline void
MARK__(CELL* ptr USES_REGS)
{
  Int pos = ptr - (CELL *)LOCAL_GlobalBase;
  char t = LOCAL_bp[pos];
  LOCAL_bp[pos] = t | MARK_BIT;
  //printf(" %p\n", ptr);
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

typedef struct gc_entry_info {
  CELL *env;
  yamop *p, *p_env;
  OPCODE op;
  arity_t a;
} gc_entry_info_t;

//#if TERMS_C||GLOBALS_C

typedef struct cp_frame {
  CELL *pt0;
  CELL *pt0_end;
  CELL *ptf;
  CELL *curp;
  CELL* oldp;
  CELL oldv;
  bool ground;
  Term t;
} copy_frame;


 typedef struct {
  copy_frame *pt0;
  copy_frame *pt;
  copy_frame *max;
  int lvl, restarts;
  size_t sz;
   CELL *hlow;
  tr_fr_ptr tr0;
  Term *t, *bindp,*arenap;
 } Ystack_t;

INLINE_ONLY bool init_stack(Ystack_t *b, size_t nof) {
    int lvl = push_text_stack();
    if (!nof)
        nof = 1024;
    b->pt0 = Malloc(nof*sizeof(copy_frame));
    b->pt = b->pt0;
    b->max = b->pt0+nof;
    b->lvl = lvl;
    b->restarts=0;
    b->hlow = HR;
       b->tr0 = TR;
      return (b->pt0 != NULL);
  }

INLINE_ONLY bool reinit_stack( Ystack_t *b, size_t nof) {
	 if (!nof)
      nof = 2*(b->max-b->pt0);
    b->pt0 = Realloc(b->pt0,nof*sizeof(copy_frame));
    b->pt = b->pt0;
    b->max = b->pt0+nof;
     b->hlow = HR;
       b->restarts++;
      b->tr0 = TR;
  return (b->pt0 != NULL);
  } 

INLINE_ONLY bool close_stack( Ystack_t *b) {
  b->pt = b->pt0 = b->max = NULL;
  HB = B->cp_h;
 return  pop_text_stack(b->lvl);
}


INLINE_ONLY void reset_stack( Ystack_t *b) {
  b->pt = b->pt0;
     b->hlow = HR;
     b->tr0 = TR;
 
}


#define to_visit    stt->pt
#define to_visit0   stt->pt0 
#define to_visit_end   stt->max

extern bool Yap_visitor_error_handler(Ystack_t *stt, void *c);

#define IS_VISIT_MARKER(d0) (IsPairTerm(d0) && \
  RepPair(d0) >= (CELL*)to_visit0				\
&& RepPair(d0) <= (CELL*)to_visit)

#define  VISIT_MARK() AbsPair((CELL*)to_visit)

#define VISIT_ENTRY(d0) (( copy_frame *)RepPair(d0))

#define VISIT_TARGET(d0) ((( copy_frame *)RepPair(d0))->t)


#define VUNMARK(ptd0, d0)  (*(ptd0) = (d0))


#define VISITED(D0)  IS_VISIT_MARKER(D0)


#define VISIT_UNMARK(d0) (IS_VISIT_MARKER(d0)?((  copy_frame *)RepPair(d0))->oldv:d0)

#define mderef_head(D, DM, Label)		\
  D = VISIT_UNMARK(DM);\
  if (IsVarTerm((D)))\
    goto Label

#define mderef_body(D, DM, A, LabelUnk, LabelNonVar)	\
  do {\
      if (!IsVarTerm(D))					\
      goto LabelNonVar;                                                        \
  LabelUnk:                                                                    \
(A) = (CELL *)(D);					\
(DM) = *(A);\
 D = VISIT_UNMARK(DM);\
  } while (Unsigned(A) != (D) )

#define POP_VISIT(A, DD)\
{ DD=*A;\
if(IS_VISIT_MARKER(DD))\
 {*A = ((  copy_frame *)RepPair(dd))->oldv; }}

#define PUSH_VISIT(A, DD, D)\
{if(IS_VISIT_MARKER(DD)) {\
  ((  copy_frame *)RepPair(DD))->oldv = D; *A=DD; }}

#define mSET(A,D) \
  { Term dd; POP_VISIT(A, dd); *A=D; PUSH_VISIT(A,dd,D); }

#define mBind(A,D) \
  { Term dd; POP_VISIT(A, dd); YapBind(A,D); PUSH_VISIT(A,dd,D); }

#define mBind_And_Trail(A,D) \
  { Term dd; POP_VISIT(A, dd); Bind_and_Trail(A,D); PUSH_VISIT(A,dd,D); }

#define mMaBind(A,D) \
  { Term dd; POP_VISIT(A, dd); MaBind(A,D); PUSH_VISIT(A,dd,D); }
  

#define mTrailedMaBind(A,D) \
  { Term dd; POP_VISIT(A, dd); TrailedMaBind(A,D); PUSH_VISIT(A,dd,D); }


#if 1
#define COPY(t) 
#else
extern 
unsigned long long vsc_count;
#define COPY(t) if (!IsAtomOrIntTerm(t)){ fprintf(stderr,"+ %lld %s: ",vsc_count++,__FUNCTION__); Yap_DebugPlxWriteln(t);}
#define OCOPY(t) { fprintf(stderr,"- %lld %s: ",vsc_count++,__FUNCTION__); Yap_DebugPlWriteln(t);if (vsc_count==12190) Yap_do_low_level_trace=1}
#endif


INLINE_ONLY Term MkGlobal(Term t);

INLINE_ONLY Term MkGlobal(Term t)
{
  if (!IsVarTerm((t = Deref(t)))) return t;
  Term *pt = VarOfTerm(t);
  if (H0<=pt && HR> pt)
    return t;
  Term nt = MkVarTerm();
  YapBind(pt, nt);
  return nt;
}


/* is val pointing to something bound to the heapiiiiiiiii? */


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

#endif // HEAPGC_H_

