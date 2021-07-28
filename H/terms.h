#ifndef TERMS_H

#define TERMS_H #include "amidefs.h"

#include "Yap.h"
  
typedef struct cp_frame {
  CELL *pt0;
  CELL *pt0_end;
  CELL *ptf;
  CELL *curp;
  CELL* oldp;
  CELL oldv;
  tr_fr_ptr tr;
  bool ground;
  Term t;
} copy_frame;


#define to_visit    stt->pt
#define to_visit0   stt->pt0
#define to_visit_end   stt->max


 typedef struct {
  copy_frame *pt0;
  copy_frame *pt;
   copy_frame *max;
  size_t szW , arenaW;
   CELL *hlow;
   yap_error_number err;
   Int tr0;
   Term t, *bindp, *arenap;
   int restarts_g;
 } Ystack_t;


#define IS_VISIT_MARKER(d0) (IsPairTerm(d0) && \
  RepPair(d0) >= (CELL*)to_visit0				\
&& RepPair(d0) <= (CELL*)to_visit)

#define  VISIT_MARK() AbsPair((CELL*)to_visit)

#define VISIT_ENTRY(d0) (( copy_frame *)RepPair(d0))

#define VISIT_TARGET(d0) ((( copy_frame *)RepPair(d0))->t)

#define VISIT_REPLACED(d0) ((( copy_frame *)RepPair(d0))->oldv)


#define VUNMARK(ptd0, d0)  (*(ptd0) = (d0))


#define VISITED(D0)  IS_VISIT_MARKER(D0)


#define VISIT_UNMARK(d0) (IS_VISIT_MARKER(d0)?((  copy_frame *)RepPair(d0))->oldv:d0)

#define mderef_head(D, DM, Label)		\
  D = DM;\
  while (IS_VISIT_MARKER(D)){\
   D = VISIT_REPLACED(D); }	      \
  if (IsVarTerm(D)) goto Label	      \

static inline bool init_stack(Ystack_t *b, size_t nof) 
{
  
  if (!b->pt0) {
      memset(b,0,sizeof(Ystack_t));
      b->pt0 =(copy_frame*)Malloc(nof*sizeof(CELL));
 } else 
      b->pt0 =(copy_frame*)Realloc(b->pt0,nof*sizeof(CELL));
    b->szW = nof;
    b->pt = b->pt0;
    b->max = (copy_frame*)((CELL*)b->pt0+nof);
    b->hlow = HR;

    b->tr0 = TR-B->cp_tr;
    b->err = YAP_NO_ERROR;
    return (b->pt0 != NULL);
  }


static inline bool realloc_stack( Ystack_t *b) {
  size_t delta = (CELL*)b->max-(CELL*)b->pt0;
  size_t nsz = delta > 1024*1024 ? delta+1024+1024 : 2*delta; 
  copy_frame *newp = (copy_frame *)Realloc(b->pt0, nsz*sizeof(CELL));
  //  fprintf(stderr,"IN %p[%ld]-%p[%ld] -> %p-%p (%ld)\n", b->pt0,b->pt-b->pt0,b->max,b->max-b->pt0, newp, (CELL*)newp+nsz, nsz);
  b->max = (copy_frame*)((CELL*)newp +nsz);
  b->szW = nsz;
  if (newp != b->pt0) {
    b->pt = newp+(b->pt-b->pt0);
      b->pt0 = newp;
      copy_frame *c;
      for (c = newp; c < b->pt; c++)
	c->oldp[0] = AbsPair((CELL*)c
			     );
    }
  //   fprintf(stderr,"IN %p[%ld]-%p[%ld]\n", b->pt0,b->pt->pt0,b->max,b->max-b->pt0);
  
  return true;
}


static inline void reset_stack( Ystack_t *b) {
  b->pt = b->pt0;
     b->hlow = HR;
     b->tr0 = TR-B->cp_tr;

}

static inline void reset_stack_but_not_trail( Ystack_t *b) {
  b->pt = b->pt0;
     b->hlow = HR;
}


#define mderef_body(D, DM, A, LabelUnk, LabelNonVar)	\
  do {\
      if (!IsVarTerm(D))					\
      goto LabelNonVar;                                                        \
  LabelUnk:                                                                    \
(A) = (CELL *)(D);				\
(DM) = *(A);\
  while (IS_VISIT_MARKER(DM)){\
   DM = VISIT_REPLACED(DM); }	      \
 if (DM==D) break;\
 D=DM;\
} while (true )

#define POP_VISIT(A, DD)\
{ DD=*A;\
if(IS_VISIT_MARKER(DD))\
 {*A = ((  copy_frame *)RepPair(dd))->oldv; }}

#define PUSH_VISIT(A, DD, D)\
{if(IS_VISIT_MARKER(DD)) {\
    ((  copy_frame *)RepPair(DD))->oldv = (D); *(A)=DD; }}

#define TrailedMaBind(VP, D)						\
  {                                                                            \
    DO_MATRAIL((VP), *(VP), (D));                                                 \
    *(VP) = (D);                                                               \
  }


#define mSET(A,D) \
  { Term dd; POP_VISIT(A, dd); *A=D; PUSH_VISIT(A,dd,D); }

#define mBind(A,D) \
  { Term dd; POP_VISIT(A, dd); YapBind(A,D); PUSH_VISIT(A,dd,D); }

#define mBind_And_Trail(A,D) \
  { Term dd; POP_VISIT(A, dd); Bind_and_Trail(A,D); PUSH_VISIT(A,dd,D); }

#define mMaBind(A,D) \
  { Term dd; POP_VISIT(A, dd); MaBind(A,D); PUSH_VISIT(A,dd,D); }
  

#define mTrailedMaBind(A,D) \
  { Term dd; POP_VISIT((A), dd); TrailedMaBind((A),(D)); PUSH_VISIT(A,dd,D); }


#if 1
#define COPY(t) 
#else
extern 
unsigned long long vsc_cnt;
#define COPY(t) if (!IsAtomOrIntTerm(t)){ fprintf(stderr,"+ %lld %s: ",vsc_count++,__FUNCTION__); Yap_DebugPlxWriteln(t);}
#define OCOPY(t) { fprintf(stderr,"- %lld %s: ",vsc_count++,__FUNCTION__); Yap_DebugPlWriteln(t);if (vsc_count==12190) Yap_do_low_level_trace=1}
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
			x		&& H0 <= *DETAG(val) < H))
*/
#define HEAP_TRAIL_ENTRY(val) ((IsVarTerm(val)) &&                  \
				((H0 <= CellPtr(val) && CellPtr(val)\
				< cp_H) ||                          \
			       (CellPtr(B) < CellPtr(val) && CellPtr(val) <= \
				LCL0 && HEAP_PTR(val))))


#endif
