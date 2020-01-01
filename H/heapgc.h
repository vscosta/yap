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


/* This routine removes array references from complex terms? */
static void replace_array_references_complex(register CELL *pt0,
                                             register CELL *pt0_end,
                                             register CELL *ptn,
                                             Term Var USES_REGS) {

  register CELL **to_visit = (CELL **)Yap_PreAllocCodeSpace();
  CELL **to_visit_base = to_visit;

loop:
  while (pt0 < pt0_end) {
    register CELL d0;

    ++pt0;
    d0 = Derefa(pt0);
    if (IsVarTerm(d0)) {
      *ptn++ = d0;
    } else if (IsPairTerm(d0)) {
      /* store the terms to visit */
      *ptn++ = AbsPair(HR);
#ifdef RATIONAL_TREES
      to_visit[0] = pt0;
      to_visit[1] = pt0_end;
      to_visit[2] = ptn;
      to_visit[3] = (CELL *)*pt0;
      to_visit += 4;
      *pt0 = TermNil;
#else
      if (pt0 < pt0_end) {
        to_visit[0] = pt0;
        to_visit[1] = pt0_end;
        to_visit[2] = ptn;
        to_visit += 3;
      }
#endif
      pt0 = RepPair(d0) - 1;
      pt0_end = RepPair(d0) + 1;
      /* write the head and tail of the list */
      ptn = HR;
      HR += 2;
    } else if (IsApplTerm(d0)) {
      register Functor f;

      f = FunctorOfTerm(d0);
      /* store the terms to visit */
      if (IsExtensionFunctor(f)) {
        {
          *ptn++ = d0;
          continue;
        }
      }
      *ptn++ = AbsAppl(HR);
/* store the terms to visit */
#ifdef RATIONAL_TREES
      to_visit[0] = pt0;
      to_visit[1] = pt0_end;
      to_visit[2] = ptn;
      to_visit[3] = (CELL *)*pt0;
      to_visit += 4;
      *pt0 = TermNil;
#else
      if (pt0 < pt0_end) {
        to_visit[0] = pt0;
        to_visit[1] = pt0_end;
        to_visit[2] = ptn;
        to_visit += 3;
      }
#endif
      pt0 = RepAppl(d0);
      d0 = ArityOfFunctor(f);
      pt0_end = pt0 + d0;
      /* start writing the compound term */
      ptn = HR;
      *ptn++ = (CELL)f;
      HR += d0 + 1;
    } else { /* A

#define to_visit    stt.pt
#define to_visit0   stt.pt0
#define to_visit_max   stt.pt0_end
tomOrInt */
      *ptn++ = d0;
    }
    /* just continue the loop */
  }

  /* Do we still have compound terms to visit */
  if (to_visit > (CELL **)to_visit_base) {
#ifdef RATIONAL_TREES
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    ptn = to_visit[2];
    *pt0 = (CELL)to_visit[3];
#else
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    ptn = to_visit[2];
#endif
    goto loop;
  }

  Bind_Global(PtrOfTerm(Var), TermNil);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit);
}

/*
 *
 * Given a term t0, build a new term tf of the form ta+tb, where ta is
 * obtained by replacing the array references in t0 by empty
 * variables, and tb is a list of array references and corresponding
 * variables.
 */
static Term replace_array_references(Term t0 USES_REGS) {
  Term t;

  t = Deref(t0);
  if (IsVarTerm(t)) {
    /* we found a variable */
    return (MkPairTerm(t, TermNil));
  } else if (IsAtomOrIntTerm(t)) {
    return (MkPairTerm(t, TermNil));
  } else if (IsPairTerm(t)) {
    Term VList = MkVarTerm();
    CELL *h0 = HR;

    HR += 2;
    replace_array_references_complex(RepPair(t) - 1, RepPair(t) + 1, h0,
                                     VList PASS_REGS);
    return MkPairTerm(AbsPair(h0), VList);
  } else {
    Term VList = MkVarTerm();
    CELL *h0 = HR;
    Functor f = FunctorOfTerm(t);

    *HR++ = (CELL)(f);
    HR += ArityOfFunctor(f);
    replace_array_references_complex(
        RepAppl(t), RepAppl(t) + ArityOfFunctor(FunctorOfTerm(t)), h0 + 1,
        VList PASS_REGS);
    return (MkPairTerm(AbsAppl(h0), VList));
  }
}


/* This routine removes array references from complex terms? */
static void replace_array_references_complex(register CELL *pt0,
                                             register CELL *pt0_end,
                                             register CELL *ptn,
                                             Term Var USES_REGS) {

  register CELL **to_visit = (CELL **)Yap_PreAllocCodeSpace();
  CELL **to_visit_base = to_visit;

loop:
  while (pt0 < pt0_end) {
    register CELL d0;

    ++pt0;
    d0 = Derefa(pt0);
    if (IsVarTerm(d0)) {
      *ptn++ = d0;
    } else if (IsPairTerm(d0)) {
      /* store the terms to visit */
      *ptn++ = AbsPair(HR);
#ifdef RATIONAL_TREES
      to_visit[0] = pt0;
      to_visit[1] = pt0_end;
      to_visit[2] = ptn;
      to_visit[3] = (CELL *)*pt0;
      to_visit += 4;
      *pt0 = TermNil;
#else
      if (pt0 < pt0_end) {
        to_visit[0] = pt0;
        to_visit[1] = pt0_end;
        to_visit[2] = ptn;
        to_visit += 3;
      }
#endif
      pt0 = RepPair(d0) - 1;
      pt0_end = RepPair(d0) + 1;
      /* write the head and tail of the list */
      ptn = HR;
      HR += 2;
    } else if (IsApplTerm(d0)) {
      register Functor f;

      f = FunctorOfTerm(d0);
      /* store the terms to visit */
      if (IsExtensionFunctor(f)) {
        {
          *ptn++ = d0;
          continue;
        }
      }
      *ptn++ = AbsAppl(HR);
/* store the terms to visit */
#ifdef RATIONAL_TREES
      to_visit[0] = pt0;
      to_visit[1] = pt0_end;
      to_visit[2] = ptn;
      to_visit[3] = (CELL *)*pt0;
      to_visit += 4;
      *pt0 = TermNil;
#else
      if (pt0 < pt0_end) {
        to_visit[0] = pt0;
        to_visit[1] = pt0_end;
        to_visit[2] = ptn;
        to_visit += 3;
      }
#endif
      pt0 = RepAppl(d0);
      d0 = ArityOfFunctor(f);
      pt0_end = pt0 + d0;
      /* start writing the compound term */
      ptn = HR;
      *ptn++ = (CELL)f;
      HR += d0 + 1;
    } else { /* A

#define to_visit    stt.pt
#define to_visit0   stt.pt0
#define to_visit_max   stt.pt0_end
tomOrInt */
      *ptn++ = d0;
    }
    /* just continue the loop */
  }

  /* Do we still have compound terms to visit */
  if (to_visit > (CELL **)to_visit_base) {
#ifdef RATIONAL_TREES
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    ptn = to_visit[2];
    *pt0 = (CELL)to_visit[3];
#else
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    ptn = to_visit[2];
#endif
    goto loop;
  }

  Bind_Global(PtrOfTerm(Var), TermNil);
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit);
}

/*
 *
 * Given a term t0, build a new term tf of the form ta+tb, where ta is
 * obtained by replacing the array references in t0 by empty
 * variables, and tb is a list of array references and corresponding
 * variables.
 */
static Term replace_array_references(Term t0 USES_REGS) {
  Term t;

  t = Deref(t0);
  if (IsVarTerm(t)) {
    /* we found a variable */
    return (MkPairTerm(t, TermNil));
  } else if (IsAtomOrIntTerm(t)) {
    return (MkPairTerm(t, TermNil));
  } else if (IsPairTerm(t)) {
    Term VList = MkVarTerm();
    CELL *h0 = HR;

    HR += 2;
    replace_array_references_complex(RepPair(t) - 1, RepPair(t) + 1, h0,
                                     VList PASS_REGS);
    return MkPairTerm(AbsPair(h0), VList);
  } else {
    Term VList = MkVarTerm();
    CELL *h0 = HR;
    Functor f = FunctorOfTerm(t);

    *HR++ = (CELL)(f);
    HR += ArityOfFunctor(f);
    replace_array_references_complex(
        RepAppl(t), RepAppl(t) + ArityOfFunctor(FunctorOfTerm(t)), h0 + 1,
        VList PASS_REGS);
    return (MkPairTerm(AbsAppl(h0), VList));
  }
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
  scratch_struct_t bf;
 } Ystack_t;

INLINE_ONLY bool init_stack(Ystack_t *b, size_t nof) {
  if (nof==0)
    nof = 4096;
  if (Yap_get_scratch_buf(&b->bf,nof, sizeof(copy_frame))) {
    b->pt0 = b->bf.data;
    b->pt = b->pt0;
    b->max = b->pt0 + nof;
    return true;
  }
  return false;
  }

INLINE_ONLY bool reinit_stack( Ystack_t *b, size_t nof) {
    if (!nof)
    nof = 2*(b->max-b->pt0);
  if (Yap_realloc_scratch_buf(&b->bf, nof)) {
    b->pt0 = b->bf.data;
    b->pt = b->pt0;
      b->max = b->pt0 + nof;
    return true;
  }
  return false;
}

INLINE_ONLY bool close_stack( Ystack_t *b) {
  return Yap_release_scratch_buf(&b->bf);
}

#define to_visit    stt.pt
#define to_visit0   stt.pt0
#define to_visit_end   stt.max

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

#define mSET(A,D) \
  { CELL *T =(IS_VISIT_MARKER(*A)?&((  copy_frame *)RepPair(*A))->t:A); *T=D; }

#define mBind(A,D) \
  { A=(IS_VISIT_MARKER(*A)?&((  copy_frame *)RepPair(*A))->t:A); *A=D;\
  TRAIL_GLOBAL(A,D);\
}

#define mBind_And_Trail(A,D) \
  { A=(IS_VISIT_MARKER(*A)?&((  copy_frame *)RepPair(*A))->t:A); *A=D;\
  DO_TRAIL(A,D);\
}

#define mMaBind(A, D)                                                          \
  {  CELL ov =*A;  bool marked = IS_VISIT_MARKER(ov);			\
  if (marked) *A = VISIT_ENTRY(ov)->oldv;\
    MaBind((A), (D));                              \
    VISIT_ENTRY(ov)->oldv = (D);					\
    *(A) = (ov);                                                           \
  }

#define mTrailedMaBind(A, D)            \
  {  CELL ov =*A;  bool marked = IS_VISIT_MARKER(ov);			\
  if (marked) *A = VISIT_ENTRY(ov)->oldv;\
    TrailedMaBind((A), (D));                              \
    VISIT_ENTRY(ov)->oldv = (D);					\
    *(A) = (ov);                                                         \
  }


#endif // HEAPGC_H_

