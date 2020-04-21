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
 * File:		amiops.h *
 * Last rev:								 *
 * mods: *
 * comments:	Basic abstract machine operations, such as	         *
 *               dereferencing, binding, trailing, and unification.       *
 *									 *
 *************************************************************************/

#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif /* SCCS */

#ifndef AMIOPS_H

#define AMIOPS_H 1

#include "inline-only.h"

#define IsArrayReference(a) ((a)->array_access_func == FunctorArrayAccess)

/* dereferencing macros */

/************************************************************

Dereferencing macros

*************************************************************/

/* For DEREFD, D has both the input and the exit argument */
/* A is only used locally */

 INLINE_ONLY Term Deref(Term a) {
    while (IsVarTerm(a)) {
        Term *b = (Term *)a;
        a = *b;
        if (a == ((Term)b))
            return a;
    }
    return (a);
}

 INLINE_ONLY Term Derefa(CELL *b) {
    Term a = *b;
    restart:
    if (!IsVarTerm(a)) {
        return (a);
    } else if (a == (CELL)b) {
        return (a);
    } else {
        b = (CELL *)a;
        a = *b;
        goto restart;
    }
}

#define profiled_deref_head_TEST(D, Label)                                     \
  if (IsVarTerm(D)) {                                                          \
    if (!strcmp(#D, "d0")) {                                                   \
      EMIT_CONDITIONAL_SUCCESS("IsVarTerm(d0)");                               \
    } else if (!strcmp(#D, "d1")) {                                            \
      EMIT_CONDITIONAL_SUCCESS("IsVarTerm(d1)");                               \
    }                                                                          \
    goto Label;                                                                \
  }                                                                            \
  if (!strcmp(#D, "d0")) {                                                     \
    EMIT_CONDITIONAL_FAIL("IsVarTerm(d0)");                                    \
  } else if (!strcmp(#D, "d1")) {                                              \
    EMIT_CONDITIONAL_FAIL("IsVarTerm(d1)");                                    \
  }

#define deref_head(D, Label)                                                   \
  if (IsVarTerm(D))                                                            \
  goto Label

#define profiled_deref_body(D, A, LabelUnk, LabelNonVar)                       \
  do {                                                                         \
    if (!IsVarTerm(D))                                                         \
      goto LabelNonVar;                                                        \
  LabelUnk:                                                                    \
    (A) = (CELL *)(D);                                                         \
    (D) = *(CELL *)(D);                                                        \
    if (!strcmp(#D, "d0") && !strcmp(#A, "pt0")) {                             \
      EMIT_SIMPLE_BLOCK_TEST(YAAM_DEREF_BODY_D0PT0);                           \
    } else if (!strcmp(#D, "d0") && !strcmp(#A, "pt1")) {                      \
      EMIT_SIMPLE_BLOCK_TEST(YAAM_DEREF_BODY_D0PT1);                           \
    } else if (!strcmp(#D, "d0") && !strcmp(#A, "S_SREG")) {                   \
      EMIT_SIMPLE_BLOCK_TEST(YAAM_DEREF_BODY_D0S_SREG);                        \
    } else if (!strcmp(#D, "d1") && !strcmp(#A, "pt0")) {                      \
      EMIT_SIMPLE_BLOCK_TEST(YAAM_DEREF_BODY_D1PT0);                           \
    } else if (!strcmp(#D, "d1") && !strcmp(#A, "pt1")) {                      \
      EMIT_SIMPLE_BLOCK_TEST(YAAM_DEREF_BODY_D1PT1);                           \
    }                                                                          \
  } while (Unsigned(A) != (D));

#define deref_body(D, A, LabelUnk, LabelNonVar)                                \
  do {                                                                         \
    if (!IsVarTerm(D))                                                         \
      goto LabelNonVar;                                                        \
  LabelUnk:                                                                    \
    (A) = (CELL *)(D);                                                         \
    (D) = *(CELL *)(D);                                                        \
  } while (Unsigned(A) != (D))

#define deref_body(D, A, LabelUnk, LabelNonVar)                                \
  do {                                                                         \
    if (!IsVarTerm(D))                                                         \
      goto LabelNonVar;                                                        \
  LabelUnk:                                                                    \
    (A) = (CELL *)(D);                                                         \
    (D) = *(CELL *)(D);                                                        \
  } while (Unsigned(A) != (D))

#define do_derefa(D, A, LabelUnk, LabelDone)                                   \
  (D) = *(CELL *)(A);                                                          \
  if (IsNonVarTerm(D))                                                         \
    goto LabelDone;                                                            \
  goto LabelUnk;                                                               \
  do {                                                                         \
    (A) = (CELL *)(D);                                                         \
    (D) = *(CELL *)(D);                                                        \
    if (!IsVarTerm(D))                                                         \
      goto LabelDone;                                                          \
  LabelUnk:;                                                                   \
  } while (Unsigned(A) != (D));                                                \
  LabelDone:

#define profiled_derefa_body(D, A, LabelUnk, LabelNonVar)                      \
  do {                                                                         \
    (A) = (CELL *)(D);                                                         \
    (D) = *(CELL *)(D);                                                        \
    if (!strcmp(#D, "d0") && !strcmp(#A, "pt0")) {                             \
      EMIT_SIMPLE_BLOCK_TEST(YAAM_DEREF_BODY_D0PT0);                           \
    } else if (!strcmp(#D, "d0") && !strcmp(#A, "pt1")) {                      \
      EMIT_SIMPLE_BLOCK_TEST(YAAM_DEREF_BODY_D0PT1);                           \
    } else if (!strcmp(#D, "d0") && !strcmp(#A, "S_SREG")) {                   \
      EMIT_SIMPLE_BLOCK_TEST(YAAM_DEREF_BODY_D0S_SREG);                        \
    } else if (!strcmp(#D, "d1") && !strcmp(#A, "pt0")) {                      \
      EMIT_SIMPLE_BLOCK_TEST(YAAM_DEREF_BODY_D1PT0);                           \
    } else if (!strcmp(#D, "d1") && !strcmp(#A, "pt1")) {                      \
      EMIT_SIMPLE_BLOCK_TEST(YAAM_DEREF_BODY_D1PT1);                           \
    }                                                                          \
    if (!IsVarTerm(D))                                                         \
      goto LabelNonVar;                                                        \
  LabelUnk:;                                                                   \
  } while (Unsigned(A) != (D));

#define derefa_body(D, A, LabelUnk, LabelNonVar)                               \
  do {                                                                         \
    (A) = (CELL *)(D);                                                         \
    (D) = *(CELL *)(D);                                                        \
    if (!IsVarTerm(D))                                                         \
      goto LabelNonVar;                                                        \
  LabelUnk:;                                                                   \
  } while (Unsigned(A) != (D))

#if UNIQUE_TAG_FOR_PAIRS

/* If you have an unique tag for pairs you can use these macros which will
   speed up detection of dereferenced pairs, but will be slow
   for the other cases.

   The only instruction where this seems useful is
   switch_list_nl
*/

#define deref_list_head(D, Label)                                              \
  if (!IsPairTerm(D))                                                          \
  goto Label

#define deref_list_body(D, A, LabelList, LabelNonVar)                          \
  do {                                                                         \
    if (!IsVarTerm(D))                                                         \
      goto LabelNonVar;                                                        \
    (A) = (CELL *)(D);                                                         \
    (D) = *(A);                                                                \
    if (Unsigned(A) == (D))                                                    \
      break;                                                                   \
    if (IsPairTerm(D))                                                         \
      goto LabelList;                                                          \
  } while (TRUE);



INLINE_ONLY CELL *deref_ptr(CELL *A) {
  Term D = *A;
  do {
    if (!IsVarTerm(D))
      return A;
    (A) = (CELL *)(D);
    (D) = *(A);
    if (Unsigned(A) == (D))
      return A;
  } while (TRUE);
}
#endif /* UNIQUE_TAG_FOR_PAIRS */

/************************************************************

TRAIL VARIABLE

A contains the address of the variable that is to be trailed

*************************************************************/

#define RESET_VARIABLE(V) (*(CELL *)(V) = Unsigned(V))

#ifdef TABLING

#define DO_TRAIL(TERM, VAL)                                                    \
  {                                                                            \
    tr_fr_ptr r;                                                               \
    r = TR;                                                                    \
    TR = r + 1;                                                                \
    TrailTerm(r) = (Term)(TERM);                                               \
    TrailVal(r) = (CELL)(VAL);                                                 \
  }

#ifdef BFZ_TRAIL_SCHEME

#define TRAIL(TERM, VAL)                                                       \
  if (OUTSIDE(HBREG, TERM, B) || ((TERM) > (CELL *)B_FZ))                      \
  DO_TRAIL(TERM, VAL)

#define TRAIL_LOCAL(TERM, VAL)                                                 \
  if ((TERM) > (CELL *)B || (TERM) > (CELL *)B_FZ)                             \
  DO_TRAIL(TERM, VAL)

#else /* BBREG_TRAIL_SCHEME */

#define TRAIL(TERM, VAL)                                                       \
  if (OUTSIDE(HBREG, TERM, BBREG))                                             \
  DO_TRAIL(TERM, VAL)

#define TRAIL_LOCAL(TERM, VAL)                                                 \
  if ((TERM) > (CELL *)BBREG)                                                  \
  DO_TRAIL(TERM, VAL)

#endif /* TRAIL_SCHEME */

/* ------------------------------------------------------ */

#define TRAIL_GLOBAL(TERM, VAL)                                                \
  if ((TERM) < HBREG)                                                          \
  DO_TRAIL(TERM, VAL)

#define DO_MATRAIL(TERM, OLDVAL, NEWVAL)                                       \
  {                                                                            \
    register tr_fr_ptr r = TR;                                                 \
    TR = r + 2;                                                                \
    TrailVal(r) = (OLDVAL);                                                    \
    TrailTerm(r) = TrailTerm(r + 1) = AbsAppl((CELL *)(TERM));                 \
    TrailVal(r + 1) = (NEWVAL);                                                \
  }

#define MATRAIL(TERM, OVAL, VAL)                                               \
  if (OUTSIDE(HBREG, TERM, B))                                                 \
  DO_MATRAIL(TERM, OVAL, VAL)

#else /* TABLING */

#if defined(i386) && !defined(TERM_EXTENSIONS)

#define DO_TRAIL(A, D)                                                         \
  {                                                                            \
    tr_fr_ptr r;                                                               \
    r = TR;                                                                    \
    TR = r + 1;                                                                \
    TrailTerm(r) = (CELL)(A);                                                  \
  }

#define TRAIL(A, D)                                                            \
  if (OUTSIDE(HBREG, A, B))                                                    \
    DO_TRAIL(A, D);

#define TRAIL_GLOBAL(A, D)                                                     \
  if ((A) < HBREG)                                                             \
    DO_TRAIL(A, D);

#define TRAIL_LOCAL(A, D)                                                      \
  if ((A) > (CELL *)B)                                                         \
    DO_TRAIL(A, D);

#elif defined(__alpha) && !defined(TERM_EXTENSIONS)

/* alpha machines have a move conditional instruction, which avoids a
   branch when jumping */
#define TRAIL(A, D)                                                            \
  TrailTerm(TR) = (CELL)(A);                                                   \
  if (OUTSIDE(HBREG, A, B))                                                    \
  TR++

#define TRAIL(A, D)                                                            \
  TrailTerm(TR) = (CELL)(A);                                                   \
  if (!OUTSIDE(HBREG, A, B))                                                   \
    GONext();

#define TRAIL_GLOBAL(A, D)                                                     \
  TR[0] = (CELL)(A);                                                           \
  if ((A) < HBREG)                                                             \
  TR++

#define TRAIL_LOCAL(A, D)                                                      \
  TR[0] = (CELL)(A);                                                           \
  if ((A) > ((CELL *)(B)))                                                     \
  TR++

#elif !defined(TERM_EXTENSIONS)

#define DO_TRAIL(A, D) TrailTerm(TR++) = (CELL)(A)

#define TRAIL(A, D)                                                            \
  if (OUTSIDE(HBREG, A, B))                                                    \
  DO_TRAIL(A, D)

#define TRAIL_AND_JUMP(A, D)                                                   \
  if (IN_BETWEEN(HBREG, A, B))                                                 \
    GONext();                                                                  \
  DO_TRAIL(A, D)

#define TRAIL_GLOBAL(A, D)                                                     \
  if ((A) < HBREG)                                                             \
  DO_TRAIL(A, D)

#define TRAIL_LOCAL(A, D)                                                      \
  if ((A) > ((CELL *)B))                                                       \
  DO_TRAIL(A, D)

#else

#define DO_TRAIL(A, D) TrailTerm(TR++) = (CELL)(A)

#define TRAIL(A, D)                                                            \
  if (OUTSIDE(HBREG, A, B))                                                    \
  DO_TRAIL(A, D)

#define TrailAndJump(A, D)                                                     \
  if (IN_BETWEEN(HBREG, A, B))                                                 \
    GONext();

#define TRAIL_GLOBAL(A, D)                                                     \
  if ((A) < HBREG)                                                             \
  DO_TRAIL(A, D)

#define TRAIL_LOCAL(A, D)                                                      \
  if ((A) > ((CELL *)B))                                                       \
  DO_TRAIL(A, D)

#endif

/************************************************************

Binding Macros for Multiple Assignment Variables.

************************************************************/

#define DO_MATRAIL(VP, OLDV, D)                                                \
  {                                                                            \
    TrailTerm(TR + 1) = OLDV;                                                  \
    TrailTerm(TR) = TrailTerm(TR + 2) = AbsAppl(VP);                           \
    TR += 3;                                                                   \
  }

#define MATRAIL(VP, OLDV, D)                                                   \
  if (OUTSIDE(HBREG, VP, B))                                                   \
  DO_MATRAIL(VP, OLDV, D)

#endif /* TABLING */

#define REF_TO_TRENTRY(REF) AbsPair(((CELL *)&((REF)->Flags)))
#define CLREF_TO_TRENTRY(REF) AbsPair(((CELL *)&((REF)->ClFlags)))

#if FROZEN_STACKS
#define TRAIL_REF(REF)                                                         \
  RESET_VARIABLE(&TrailVal(TR)), TrailTerm(TR++) = REF_TO_TRENTRY(REF)
#define TRAIL_CLREF(REF)                                                       \
  RESET_VARIABLE(&TrailVal(TR)), TrailTerm(TR++) = CLREF_TO_TRENTRY(REF)
#define TRAIL_LINK(REF)                                                        \
  RESET_VARIABLE(&TrailVal(TR)), TrailTerm(TR++) = AbsPair((CELL *)(REF))
#else
#define TRAIL_REF(REF) TrailTerm(TR++) = REF_TO_TRENTRY(REF)
#define TRAIL_CLREF(REF) TrailTerm(TR++) = CLREF_TO_TRENTRY(REF)
#define TRAIL_LINK(REF) TrailTerm(TR++) = AbsPair((CELL *)(REF))
#endif
#define TRAIL_FRAME(FR) DO_TRAIL(AbsPair((CELL *)(LOCAL_TrailBase)), FR)

extern void Yap_WakeUp(CELL *v);

#define Bind_Local(A, D)                                                       \
  {                                                                            \
    TRAIL_LOCAL(A, D);                                                         \
    *(A) = (D);                                                                \
  }
#define Bind_Global(A, D)                                                      \
  {                                                                            \
    *(A) = (D);                                                                \
    if (__builtin_expect(GlobalIsAttVar(A), 0))                                \
      Yap_WakeUp(A);                                                           \
    else                                                                       \
      TRAIL_GLOBAL(A, D);                                                      \
  }
#define YapBind(A, D)                                                          \
  {                                                                            \
    *(A) = (D);                                                                \
    if (A < HR) {                                                              \
      if (__builtin_expect(GlobalIsAttVar(A), 0))                              \
        Yap_WakeUp(A);                                                         \
      else                                                                     \
        TRAIL_GLOBAL(A, D);                                                    \
    } else {                                                                   \
      TRAIL_LOCAL(A, D);                                                       \
    }                                                                          \
  }
#define Bind_NonAtt(A, D)                                                      \
  {                                                                            \
    *(A) = (D);                                                                \
    TRAIL(A, D);                                                               \
  }
#define Bind_Global_NonAtt(A, D)                                               \
  {                                                                            \
    *(A) = (D);                                                                \
    TRAIL_GLOBAL(A, D);                                                        \
  }
#define Bind_and_Trail(A, D)                                                   \
  {                                                                            \
    *(A) = (D);                                                                \
    DO_TRAIL(A, D);                                                            \
  }
// #define Bind(A,D) YapBind(A,D) conflicts with Windows headers

#define MaBind(VP, D)                                                          \
  {                                                                            \
    MATRAIL((VP), *(VP), (D));                                                 \
    *(VP) = (D);                                                               \
  }

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


/************************************************************

Unification Routines

*************************************************************/

#include "YapCompoundTerm.h"



INLINE_ONLY void reset_trail(tr_fr_ptr TR0) {
  CACHE_REGS
  while (TR != TR0) {
    CELL d1;
    --TR;
    d1 = TrailTerm(TR);
#ifdef MULTI_ASSIGNMENT_VARIABLES
    if (IsVarTerm(d1)) {
#endif
      CELL *pt = (CELL *)d1;
      RESET_VARIABLE(pt);
#ifdef MULTI_ASSIGNMENT_VARIABLES
    } else {
      CELL *pt = RepAppl(d1);
/* AbsAppl means */
/* multi-assignment variable */
/* so the next cell is the old value */
#ifdef FROZEN_STACKS
      pt[0] = TrailVal(TR - 1);
      TR -= 1;
#else
      pt[0] = TrailTerm(TR - 1);
      TR -= 2;
#endif /* FROZEN_STACKS */
    }
#endif
  }
}



INLINE_ONLY void reset_attvars(CELL *dvarsmin, CELL *dvarsmax) {
  if (dvarsmin) {
    dvarsmin += 1;
    do {
      CELL *newv;
      newv = CellPtr(*dvarsmin);
      RESET_VARIABLE(dvarsmin + 1);
      if (IsUnboundVar(dvarsmin))
        break;
      RESET_VARIABLE(dvarsmin);
      dvarsmin = newv;
    } while (TRUE);
  }
}

INLINE_ONLY void close_attvar_chain(CELL *dvarsmin,
                                                  CELL *dvarsmax);

INLINE_ONLY void close_attvar_chain(CELL *dvarsmin,
                                                  CELL *dvarsmax) {
  CACHE_REGS
  if (dvarsmin) {
    dvarsmin += 1;
    do {
      CELL *newv;
      YapBind(dvarsmin + 1, dvarsmin[1]);
      if (IsUnboundVar(dvarsmin))
        break;
      newv = CellPtr(*dvarsmin);
      RESET_VARIABLE(dvarsmin);
      dvarsmin = newv;
    } while (TRUE);
  }
}

 

 INLINE_ONLY bool Yap_unify(Term t0, Term t1) {
  CACHE_REGS
  tr_fr_ptr TR0 = TR;

  if (Yap_IUnify(t0, t1)) {
    return true;
  } else {
    reset_trail(TR0);
    return false;
  }
}

 

 INLINE_ONLY Int Yap_unify_constant(Term a, Term cons) {
  CACHE_REGS
  CELL *pt;
  deref_head(a, unify_cons_unk);
unify_cons_nonvar : {
  if (a == cons)
    return (TRUE);
  else if (IsApplTerm(a)) {
    Functor f;
    if (!IsApplTerm(cons))
      return (FALSE);
    f = FunctorOfTerm(a);
    if (f != FunctorOfTerm(cons))
      return (FALSE);
    if (IsExtensionFunctor(f)) {
      switch ((CELL)f) {
      case db_ref_e:
        return (a == cons);
      case long_int_e: {
        CELL d0 = RepAppl(a)[1];
        CELL d1 = RepAppl(cons)[1];
        return d0 == d1;
      }
      case double_e: {
        Float d0 = FloatOfTerm(a);
        Float d1 = FloatOfTerm(cons);
        return d0 == d1;
      }
      case big_int_e:
#ifdef USE_GMP
        return (Yap_gmp_tcmp_big_big(a, cons) == 0);
#endif /* USE_GMP */
      default:
        return FALSE;
      }
    }
  } else
    return FALSE;
}

  deref_body(a, pt, unify_cons_unk, unify_cons_nonvar);
  YapBind(pt, cons);
  return (TRUE);
}

#define EQ_OK_IN_CMP 1
#define LT_OK_IN_CMP 2
#define GT_OK_IN_CMP 4

staticint do_cut(int i) {
  CACHE_REGS
  if (POP_CHOICE_POINT(B->cp_b)) {
    cut_c_pop();
  }
  Yap_TrimTrail();
  B = B->cp_b;
  return i;
}

#define cut_succeed() return do_cut(true)

#define cut_fail() return do_cut(false)

#endif
