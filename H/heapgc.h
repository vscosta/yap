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

#if !defined(TABLING)
//#define EASY_SHUNTING 1
#endif /* !TABLING */
#define HYBRID_SCHEME 1

/* macros used by garbage collection */

/* return pointer from object pointed to by ptr (remove tag & mark) */

/* is ptr a pointer to the heap? */
#define ONHEAP(ptr) ((CELL*)(ptr) >= H0  && (CELL*)(ptr) < HR)

#if GC_NO_TAGS
#define GET_NEXT(val)  ((CELL *) ((val) & ~(TagBits)))

#elif TAG_64BITS

#define RMARK_BIT MKTAG(0x2,0x0)
#define  MARK_BIT MKTAG(0x4,0x0)

#define GET_NEXT(val)  ((CELL *) ((val) & ~MKTAG(0x7,0x7)))

#elif TAGS_FAST_OPS

#define GET_NEXT(val)  ((CELL *)(IsVarTerm((val)) ?                          \
                                 (val) & MaskAdr :                           \
                                 ( IsPairTerm((val)) ?                       \
			            Unsigned(RepPair((val))) & MaskAdr :     \
                                    ( IsApplTerm((val)) ?                    \
                                      Unsigned(RepAppl((val))) & MaskAdr :   \
                                      (val) & MaskAdr                        \
                                    )                                    HG    \
                                 )                                           \
                        ){
  return (CELL)ptr & MARK_BIT;
}
#endif

#ifdef HYBRID_SCHEME

inline static void PUSH_POINTER(CELL *v USES_REGS) {
  if (LOCAL_iptop >= (CELL **)ASP)
    return;
  *LOCAL_iptop++ = v;
}

#ifdef EASY_SHUNTING
inline static void POP_POINTER(USES_REGS1) {
  if (LOCAL_iptop >= (CELL* *)ASP)
    return;
  --LOCAL_iptop;
}
#endif

inline static void POPSWAP_POINTER(CELL* *vp, CELL* v USES_REGS) {
  if (LOCAL_iptop >= (CELL* *)ASP || LOCAL_iptop == vp)
    return;
  if (*vp != v)
    return;
  --LOCAL_iptop;
  if (vp != LOCAL_iptop)
    *vp = *LOCAL_iptop;
}
#else

#define PUSH_POINTER(P PASS_REGS)
#define POP_POINTER(PASS_REGS1)
#define POPSWAP          _POINTER(P)

#endif /* HYBRID_SCHEME */

#define   INC_MARKED(t,ptr)		   \
  { if  (ptr >= H0   && ptr < HR) {					\
      LOCAL_total_marked ++; \
      fprintf(stderr," %p\n", ptr);\
      }\
  if (ptr >= H0 &&  ptr < LOCAL_HGEN) {			\
	  LOCAL_total_oldies++;\
	}\
  }

#define INC_MARKED_REGION(ptr, n, l)                                           \
  if (ptr >= H0 && ptr < HR) {                                                 \
    LOCAL_total_marked += n;                                                   \
    fprintf(stderr,"%p--%p\n", ptr, ptr + n);				\
  }\
  if (ptr >= H0 && ptr < LOCAL_HGEN) {		\
	    LOCAL_total_oldies+= n ;\
	  } \
  if (!is_EndExtension(ptr+(n-1) ))  {		   			\
	    fprintf(stderr,"[ Error:at %d could not find EndSpecials at blob %p type " UInt_FORMAT " ]\n", l, ptr, ptr[1]); \
	}

                
#if GC_NO_TAGS
#define  MARK_BIT ((char)1)
#define RMARK_BIT ((char)2)

#define GCTagOf TagOf

#define mcell(X)  LOCAL_bp[(X)-(CELL *)LOCAL_GlobalBase]

#define MARKED_PTR(P) MARKED_PTR__(P PASS_REGS)
#define UNMARKED_MARK(P, BP) UNMARKED_MARK__(P, BP PASS_REGS)
#define MARK(P) MARK__(P PASS_REGS)
#define SET_MARK(P) SET_MARK__(P PASS_REGS)
#define MARK_RANGE(P,SZ) MARK_RANGE__(P,SZ,__LINE__ PASS_REGS)
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
    PUSH_POINTER(ptr PASS_REGS);

    INC_MARKED(t, ptr);
    return FALSE;
}

static inline void
SET_MARK__(CELL* ptr USES_REGS)
{
    Int pos = ptr - (CELL *)LOCAL_GlobalBase;
    char t = LOCAL_bp[pos];
    LOCAL_bp[pos] = t | MARK_BIT;
}

static inline void
MARK__(CELL* ptr USES_REGS)
{
    Int pos = ptr - (CELL *)LOCAL_GlobalBase;
    char t = LOCAL_bp[pos];
    LOCAL_bp[pos] = t | MARK_BIT;
    INC_MARKED(t, ptr);
    //printf(" %p\n", ptr);
}

static inline void
MARK_RANGE__(CELL* ptr, size_t sz,int line USES_REGS)
{
    Int pos = ptr - (CELL *)LOCAL_GlobalBase;
    char t = LOCAL_bp[0];
    LOCAL_bp[pos] = t | MARK_BIT;
    t = LOCAL_bp[pos+sz];
    //printf(" %p\n", ptr);
    INC_MARKED_REGION(t, ptr,sz,line);

}

static inline void
UNMARK__(CELL* ptr USES_REGS)
{
  fprintf(stderr,"%p",ptr);
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


extern INLINE_ONLY bool
RMARKED(CELL* ptr);
 
INLINE_ONLY     
RMARKED__(CELL* ptr USES_REGS)
{
    return mcell(ptr) & RMARK_BIT;
}

#else

#define UNMARKED_MARK(ptr, bp) UNMARKED_MARK__(ptr)

#define SET_MARK(PTR) {*(PTR)  |= MARK_BIT;}
#define RESET_MARK(PTR) {*(PTR)  &= ~MARK_BIT;}
static inline

bool UNMARKED_MARK__(CELL *ptr)
{
  Term t = *ptr;
  if (t & MARK_BIT)
    return true;
  *ptr |= MARK_BIT;
  INC_MARKED(t,ptr);
  PUSH_POINTER(ptr PASS_REGS);
  return false;
}

#define MARK(P) MARK__(P PASS_REGS)
static inline
void MARK__(CELL *ptr PASS_REGS)
{
  Term t = *ptr;
  if (t & MARK_BIT)
    return;
  *ptr |= MARK_BIT;
  INC_MARKED(t,ptr);
  PUSH_POINTER(ptr PASS_REGS);
}


#define MARK_RANGE(P, SZ) MARK_RANGE__(P, SZ, __LINE__)
static inline void
MARK_RANGE__(CELL* ptr,size_t n,int line)
{
  *ptr  |= MARK_BIT;
  ptr[n-1] |= MARK_BIT;
  INC_MARKED_REGION(ptr,n,line);
  PUSH_POINTER(ptr PASS_REGS);          
}


static inline void UNMARK(CELL* ptr)
{
  *ptr  &= ~MARK_BIT;
}

static inline bool
MARKED_PTR(CELL* ptr)
{
   return *(ptr)  & MARK_BIT;
}

#define UNMARK_CELL(X) ((X) & ~MARK_BIT)

#define CLEAR_CELL(X) ((X) & ~(MARK_BIT|RMARK_BIT))

static inline void
RMARK(CELL* ptr)                                                
{
   *ptr |= RMARK_BIT;
}

static inline CELL
UNRMARK(CELL* ptr)
{
  *ptr  &= ~RMARK_BIT;
  return *ptr;
}

static inline bool
RMARKED(CELL* ptr)
{
    return ((*ptr & RMARK_BIT) != 0);
}
#endif

/* is the object pointed to by ptr marked as in a relocation chain? */

#if LONG_ADDRESSES
#ifdef TAG_LOW_BITS_32
#define TAG(X)         ((X) & LowTagBits)

#else
#ifdef TAG_64BITS
#define TAG(X)         ((X) & MKTAG(0x1,0x7))
#else
#define TAG(X)         ((X) & 0x80000003L)
#endif
#endif
#else
#endif

//#define TAG(X)         ((X) & 0x9800000L)

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
unsigned long long vsc_cnt;
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
			x		&& H0 <= *DETAG(val) < H))
*/
#define HEAP_TRAIL_ENTRY(val) ((IsVarTerm(val)) &&                  \
				((H0 <= CellPtr(val) && CellPtr(val)\
				< cp_H) ||                          \
			       (CellPtr(B) < CellPtr(val) && CellPtr(val) <= \
				LCL0 && HEAP_PTR(val))))

#endif // HEAPGC_H_

