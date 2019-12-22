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

INLINE_ONLY CELL *deref_ptr(CELL *A);

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

      TrailTerm(TR) = TrailTerm(TR + 2) = AbsAppl(VP);			\
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
    TRAIL_LOCAL(A, D);                                                        \
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

#define YapBindUnsafe(A, D)						\
  {                                                                            \
    *(A) = (D);                                                                \
    if (A < HR) {                                                              \
      if (__builtin_expect(GlobalIsAttVar(A), 0)) { \
         if (!Yap_WakeUpUnsafe(A)) JMPNext(); \
      } else								\
        TRAIL_GLOBAL(A, D);                                                    \
    } else {                                                                   \
      TRAIL_LOCAL(A, D);                                                       \
    }                                                                          \
  }

#define Bind_NonAtt(A, D)						\
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


#define TrailedMaBind(VP, D)						\
  {                                                                            \
    DO_MATRAIL((VP), *(VP), (D));                                                 \
    *(VP) = (D);                                                               \
  }

/************************************************************

Unification Routines

*************************************************************/

INLINE_ONLY void reset_trail(tr_fr_ptr TR0);

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

INLINE_ONLY void reset_attvars(CELL *dvarsmin, CELL *dvarsmax);

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

INLINE_ONLY bool Yap_unify(Term t0, Term t1);

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

INLINE_ONLY Int Yap_unify_constant(Term a, Term cons);

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

static inline int do_cut(int i) {
  CACHE_REGS
  if (POP_CHOICE_POINT(B->cp_b)) {
    cut_c_pop();
  }
  Yap_TrimTrail();
  B = B->cp_b;
  return i;
}

#define cut_succeed() return do_cut(TRUE)

#define cut_fail() return do_cut(FALSE)


#endif
