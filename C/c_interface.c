/*************************************************************************  *
 *	 YAP Prolog 							 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.Santos Costa and Universidade do Porto 1985--	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		c_interface.c *
 * comments:	c_interface primitives definition 			 *
 *									 *
 * Last rev:	$Date: 2008-08-07 20:51:21 $,$Author: vsc $
 **
 * $Log: not supported by cvs2svn $
 *									 *
 *									 *
 *************************************************************************/

/**
 * @file c_interface.c
 *
 * @addtogroup ChYInterface
 */

#ifndef C_INTERFACE_C

#define C_INTERFACE_C 1
#define _EXPORT_KERNEL 1

#include <stdlib.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <string.h>

#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#if _MSC_VER || defined(__MINGW32__)
#include <windows.h>
#endif
// we cannot consult YapInterface.h, that conflicts with what we declare, though
// it shouldn't

#include "Yap.h"
#include "YapInterface.h"
#include "YapText.h"
#include "attvar.h"
#include "clause.h"
#include "yapio.h"

#ifdef TABLING

#include "tab.macros.h"

#endif /* TABLING */
#ifdef YAPOR
#include "or.macros.h"
#endif /* YAPOR */

#include "cut_c.h"

#if HAVE_MALLOC_H

#include <malloc.h>

#endif

#include "iopreds.h"
#include <libgen.h>

typedef void *atom_t;
typedef void *functor_t;

typedef enum {
  FRG_FIRST_CALL = 0, /* Initial call */
  FRG_CUTTED = 1,     /* Context was cutted */
  FRG_REDO = 2        /* Normal redo */
} frg_code;

struct foreign_context {
  uintptr_t context;            /* context value */
  frg_code control;             /* FRG_* action */
  struct PL_local_data *engine; /* invoking engine */
};

X_API bool python_in_python;

X_API int YAP_Reset(yap_reset_t mode, bool reset_global);

#if !HAVE_STRNCPY
#define strncpy(X, Y, Z) strcpy(X, Y)
#endif
#if !HAVE_STRNCAT
#define strncat(X, Y, Z) strcat(X, Y)
#endif

#if defined(_WIN32) && !defined(X_API)
#define X_API __declspec(dllexport)
#endif

#define SOURCEBOOTPath NULL
#if __ANDROID__
#define BOOT_FROM_SAVED_STATE true
#endif

/**
@defgroup slotInterface Term Handles or Slots
@ingroup ChYInterface
@{

Term handles correspond to SWI-Prolog's term_t datatype: they are a safe
representation
of terms. Slots are safe houses in the stack, the garbage collector and the
stack
shifter know about them and make sure they have correct values. In this
case, we use a slot to preserve  _t_ during the execution of
YAP_RunGoal). When the execution of  _t_ is over we read the
(possibly changed) value of  _t_ back from the slot  _sl_ and tell
YAP that the slot  _sl_ is not needed and can be given back to the
system.

 YAP supports storing and manipulating term_t like slots or handles, but in the
C
the programmer needs to take care as most operations still require unwrapping
the term
inside.

For implementation details and more information, please check term_t_slots in
the implementation section.

*/


	      /// @brief initialize the slot data-structure: all existing slots will be
	      /// discarded. Typically, this would be used at the beginning
	      /// top-level or other outer quqqery.
X_API void YAP_StartSlots(void)
{
  Yap_RebootHandles(worker_id);
}

	      /// @brief discard all existing slots: operates as
/// StartSlots, but should be called when we're done.
X_API void YAP_EndSlots(void)
{
  Yap_RebootHandles(worker_id);
}

/// @brief report the current position of the slots, assuming that they occupy
/// the top of the stack.
///
///
X_API yhandle_t YAP_CurrentSlot(void);

/// @brief allocate n empty new slots
///
/// Return a handle to the system's default slo   t.
/// iX_API yhandle_t YAP_NewSlots(int NumberOfSlots);

/// @brief allocate n empty new slots
///
/// Allocate  _NumberOfSlots_ from the stack and return an handle to the
/// last one. The other handle can be obtained by decrementing the handle.
X_API yhandle_t YAP_InitSlot(YAP_Term t);

/// @brief read from a slot.
///
///
X_API YAP_Term YAP_GetFromSlot(YAP_handle_t slot);

/// @brief get the memory address of a slot
///
/// Return the address of slot  _slot_: please use with care.
X_API YAP_Term *YAP_AddressFromSlot(YAP_handle_t);

/// @brief get the memory address of the term actually stored in a slot
///
///
X_API YAP_Term *YAP_AddressOfTermInSlot(YAP_handle_t);

/// @brief store  term in a slot
///
///
X_API void YAP_PutInSlot(YAP_handle_t slot, YAP_Term t);

/// @brief Succeeds if it recovers the space allocated for $n$ contiguous slots
/// starting at topSlot.
///
/// Set the contents of slot  _slot_ to  _t_.
X_API int YAP_RecoverSlots(int, YAP_handle_t topSlot);

/// @brief copies the first new n YAAM registers to slots
///
/// Store the current first   _HowMany_ arguments in new slots.
X_API YAP_handle_t YAP_ArgsToSlots(int HowMany);

/// @brief copies n slots such that sl is copied to the last abstract ,achine
/// register.
///
/// Set the first  _HowMany_ arguments to the  _HowMany_ slots
// starting at  _slot_.
X_API void YAP_SlotsToArgs(int HowMany, YAP_handle_t slot);

static arity_t current_arity(void) {
  CACHE_REGS
  if (P && PREVOP(P, Osbpp)->opc == Yap_opcode(_call_usercpred)) {
    return PREVOP(P, Osbpp)->y_u.Osbpp.p->ArityOfPE;
  } else {
    return 0;
  }
}

static int doexpand(UInt sz) {
  CACHE_REGS
  UInt arity;

  if (P && PREVOP(P, Osbpp)->opc == Yap_opcode(_call_usercpred)) {
    arity = PREVOP(P, Osbpp)->y_u.Osbpp.p->ArityOfPE;
  } else {
    arity = 0;
  }
  if (!Yap_gcl(sz, arity, ENV, gc_P(P, CP))) {
    return FALSE;
  }
  return TRUE;
}

X_API YAP_Term YAP_A(int i) {
  CACHE_REGS
  return (Deref(XREGS[i]));
}

X_API YAP_Term YAP_SetA(int i, YAP_Term t) {
  CACHE_REGS
  return (Deref(XREGS[i]));
}

X_API YAP_Bool YAP_IsIntTerm(YAP_Term t) { return IsIntegerTerm(t); }

X_API YAP_Bool YAP_IsNumberTerm(YAP_Term t) {
  return IsIntegerTerm(t) || IsIntTerm(t) || IsFloatTerm(t) || IsBigIntTerm(t);
}

X_API YAP_Bool YAP_IsLongIntTerm(YAP_Term t) { return IsLongIntTerm(t); }

X_API YAP_Bool YAP_IsBigNumTerm(YAP_Term t) {
#if USE_GMP
  CELL *pt;
  if (IsVarTerm(t))
    return FALSE;
  if (!IsBigIntTerm(t))
    return FALSE;
  pt = RepAppl(t);
  return pt[1] == BIG_INT;
#else
  return FALSE;
#endif
}

X_API YAP_Bool YAP_IsRationalTerm(YAP_Term t) {
#if USE_GMP
  CELL *pt;
  if (IsVarTerm(t))
    return FALSE;
  if (!IsBigIntTerm(t))
    return FALSE;
  pt = RepAppl(t);
  return pt[1] == BIG_RATIONAL;
#else
  return FALSE;
#endif
}

X_API YAP_Bool YAP_IsStringTerm(YAP_Term t) { return (IsStringTerm(t)); }

X_API YAP_Bool YAP_IsVarTerm(YAP_Term t) { return (IsVarTerm(t)); }

X_API YAP_Bool YAP_IsNonVarTerm(YAP_Term t) { return (IsNonVarTerm(t)); }

X_API YAP_Bool YAP_IsFloatTerm(Term t) { return (IsFloatTerm(t)); }

X_API YAP_Bool YAP_IsDbRefTerm(Term t) { return (IsDBRefTerm(t)); }

X_API YAP_Bool YAP_IsAtomTerm(Term t) { return (IsAtomTerm(t)); }

X_API YAP_Bool YAP_IsPairTerm(Term t) { return (IsPairTerm(t)); }

X_API YAP_Bool YAP_IsApplTerm(Term t) {
  return IsApplTerm(t) && !IsExtensionFunctor(FunctorOfTerm(t));
}

X_API YAP_Bool YAP_IsCompoundTerm(Term t) {
  return (IsApplTerm(t) && !IsExtensionFunctor(FunctorOfTerm(t))) ||
         IsPairTerm(t);
}

X_API Term YAP_MkIntTerm(Int n) {
  CACHE_REGS
  Term I;
  BACKUP_H();

  I = MkIntegerTerm(n);
  RECOVER_H();
  return I;
}

X_API Term YAP_MkStringTerm(const char *n) {
  CACHE_REGS
  Term I;
  BACKUP_H();

  I = MkStringTerm(n);
  RECOVER_H();
  return I;
}

X_API Term YAP_MkCharPTerm(char *n) {
  CACHE_REGS
  Term I;
  BACKUP_H();

  I = MkStringTerm(n);
  RECOVER_H();
  return I;
}

X_API Term YAP_MkUnsignedStringTerm(const unsigned char *n) {
  CACHE_REGS
  Term I;
  BACKUP_H();

  I = MkUStringTerm(n);
  RECOVER_H();
  return I;
}

X_API const char *YAP_StringOfTerm(Term t) { return StringOfTerm(t); }

X_API const unsigned char *YAP_UnsignedStringOfTerm(Term t) {
  return UStringOfTerm(t);
}

X_API Int YAP_IntOfTerm(Term t) {
  if (!IsApplTerm(t))
    return IntOfTerm(t);
  else {
    return LongIntOfTerm(t);
  }
}

X_API Term YAP_MkBigNumTerm(void *big) {
#if USE_GMP
  Term I;
  BACKUP_H();
  I = Yap_MkBigIntTerm(big);
  RECOVER_H();
  return I;
#else
  return TermNil;
#endif /* USE_GMP */
}

X_API YAP_Bool YAP_BigNumOfTerm(Term t, void *b) {
#if USE_GMP
  MP_INT *bz = (MP_INT *)b;
  if (IsVarTerm(t))
    return FALSE;
  if (!IsBigIntTerm(t))
    return FALSE;
  mpz_set(bz, Yap_BigIntOfTerm(t));
  return TRUE;
#else
  return FALSE;
#endif /* USE_GMP */
}

X_API Term YAP_MkRationalTerm(void *big) {
#if USE_GMP
  Term I;
  BACKUP_H();
  I = Yap_MkBigRatTerm((MP_RAT *)big);
  RECOVER_H();
  return I;
#else
  return TermNil;
#endif /* USE_GMP */
}

X_API YAP_Bool YAP_RationalOfTerm(Term t, void *b) {
#if USE_GMP
  MP_RAT *br = (MP_RAT *)b;
  if (IsVarTerm(t))
    return FALSE;
  if (!IsBigIntTerm(t))
    return FALSE;
  mpq_set(br, Yap_BigRatOfTerm(t));
  return TRUE;
#else
  return FALSE;
#endif /* USE_GMP */
}

X_API Term YAP_MkBlobTerm(unsigned int sz) {
  CACHE_REGS
  Term I;
  MP_INT *dst;
  BACKUP_H();

  while (HR + (sz + sizeof(MP_INT) / sizeof(CELL) + 2) > ASP - 1024) {
    if (!doexpand((sz + sizeof(MP_INT) / sizeof(CELL) + 2) * sizeof(CELL))) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil,
                "YAP failed to grow the stack while constructing a blob: %s",
                LOCAL_ErrorMessage);
      return TermNil;
    }
  }
  I = AbsAppl(HR);
  HR[0] = (CELL)FunctorBigInt;
  HR[1] = ARRAY_INT;
  dst = (MP_INT *)(HR + 2);
  dst->_mp_size = 0L;
  dst->_mp_alloc = sz;
  HR += (2 + sizeof(MP_INT) / sizeof(CELL));
  HR[sz] = EndSpecials;
  HR += sz + 1;
  RECOVER_H();

  return I;
}

X_API void *YAP_BlobOfTerm(Term t) {
  MP_INT *src;

  if (IsVarTerm(t))
    return NULL;
  if (!IsBigIntTerm(t))
    return NULL;
  src = (MP_INT *)(RepAppl(t) + 2);
  return (void *)(src + 1);
}

X_API Term YAP_MkFloatTerm(double n) {
  CACHE_REGS
  Term t;
  BACKUP_H();

  t = MkFloatTerm(n);

  RECOVER_H();
  return t;
}

X_API YAP_Float YAP_FloatOfTerm(YAP_Term t) { return (FloatOfTerm(t)); }

X_API Term YAP_MkAtomTerm(YAP_Atom n) {
  Term t;

  t = MkAtomTerm(n);
  return t;
}

X_API YAP_Atom YAP_AtomOfTerm(Term t) { return (AtomOfTerm(t)); }

X_API bool YAP_IsWideAtom(YAP_Atom a) {
  const unsigned char *s = RepAtom(a)->UStrOfAE;
  int32_t v;
  while (*s) {
    size_t n = get_utf8(s, 1, &v);
    if (n > 1)
      return true;
  }
  return false;
}

X_API const char *YAP_AtomName(YAP_Atom a) {
  const char *o;

  o = AtomName(a);
  return (o);
}

X_API const wchar_t *YAP_WideAtomName(YAP_Atom a) {
  int32_t v;
  const unsigned char *s = RepAtom(a)->UStrOfAE;
  size_t n = strlen_utf8(s);
  wchar_t *dest = Malloc((n + 1) * sizeof(wchar_t)), *o = dest;
  while (*s) {
    size_t n = get_utf8(s, 1, &v);
    if (n == 0)
      return NULL;
    *o++ = v;
  }
  o[0] = '\0';
  return dest;
}

X_API YAP_Atom YAP_LookupAtom(const char *c) {
  CACHE_REGS
  Atom a;

  while (TRUE) {
    a = Yap_LookupAtom(c);
    if (a == NIL || Yap_get_signal(YAP_CDOVF_SIGNAL)) {
      if (!Yap_locked_growheap(FALSE, 0, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "YAP failed to grow heap: %s",
                  LOCAL_ErrorMessage);
      }
    } else {
      return a;
    }
  }
  return NULL;
}

X_API YAP_Atom YAP_LookupWideAtom(const wchar_t *c) {
  CACHE_REGS
  Atom a;

  while (TRUE) {
    a = Yap_NWCharsToAtom(c, -1 USES_REGS);
    if (a == NIL || Yap_get_signal(YAP_CDOVF_SIGNAL)) {
      if (!Yap_locked_growheap(FALSE, 0, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "YAP failed to grow heap: %s",
                  LOCAL_ErrorMessage);
      }
    } else {
      return a;
    }
  }
  return NULL;
}

X_API YAP_Atom YAP_FullLookupAtom(const char *c) {
  CACHE_REGS
  Atom at;

  while (TRUE) {
    at = Yap_FullLookupAtom(c);
    if (at == NIL || Yap_get_signal(YAP_CDOVF_SIGNAL)) {
      if (!Yap_locked_growheap(FALSE, 0, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "YAP failed to grow heap: %s",
                  LOCAL_ErrorMessage);
      }
    } else {
      return at;
    }
  }
  return NULL;
}

X_API size_t YAP_AtomNameLength(YAP_Atom at) {
  if (IsBlob(at)) {
    return RepAtom(at)->rep.blob->length;
  }
  unsigned char *c = RepAtom(at)->UStrOfAE;

  return strlen_utf8(c);
}

X_API Term YAP_MkVarTerm(void) {
  CACHE_REGS
  CELL t;
  BACKUP_H();

  t = MkVarTerm();

  RECOVER_H();
  return t;
}

X_API Term YAP_MkPairTerm(Term t1, Term t2) {
  CACHE_REGS
  Term t;
  BACKUP_H();

  while (HR > ASP - 1024) {
    Int sl1 = Yap_InitSlot(t1);
    Int sl2 = Yap_InitSlot(t2);
    RECOVER_H();
    if (!Yap_dogc(0, NULL PASS_REGS)) {
      return TermNil;
    }
    BACKUP_H();
    t1 = Yap_GetFromSlot(sl1);
    t2 = Yap_GetFromSlot(sl2);
    Yap_RecoverSlots(2, sl2);
  }
  t = MkPairTerm(t1, t2);
  RECOVER_H();
  return t;
}

X_API Term YAP_MkListFromTerms(Term *ta, Int sz) {
  CACHE_REGS
  Term t;
  CELL *h;
  if (sz == 0)
    return TermNil;
  BACKUP_H();
  while (HR + sz * 2 > ASP - 1024) {
    Int sl1 = Yap_InitSlot((CELL)ta);
    RECOVER_H();
    if (!Yap_dogc(0, NULL PASS_REGS)) {
      return TermNil;
    }
    BACKUP_H();
    ta = (CELL *)Yap_GetFromSlot(sl1);
    Yap_RecoverSlots(1, sl1);
  }
  h = HR;
  t = AbsPair(h);
  while (sz--) {
    Term ti = *ta++;
    if (IsVarTerm(ti)) {
      RESET_VARIABLE(h);
      Yap_unify(ti, h[0]);
    } else {
      h[0] = ti;
    }
    h[1] = AbsPair(h + 2);
    h += 2;
  }
  h[-1] = TermNil;
  HR = h;
  RECOVER_H();
  return t;
}

X_API Term YAP_MkNewPairTerm() {
  CACHE_REGS
  Term t;
  BACKUP_H();

  if (HR > ASP - 1024)
    t = TermNil;
  else
    t = Yap_MkNewPairTerm();

  RECOVER_H();
  return t;
}

X_API Term YAP_HeadOfTerm(Term t) { return (HeadOfTerm(t)); }

X_API Term YAP_TailOfTerm(Term t) { return (TailOfTerm(t)); }

X_API Int YAP_SkipList(Term *l, Term **tailp) {
  return Yap_SkipList(l, tailp);
  Int length = 0;
  Term *s; /* slow */
  Term v;  /* temporary */

  do_derefa(v, l, derefa_unk, derefa_nonvar);
  s = l;

  if (IsPairTerm(*l)) {
    intptr_t power = 1, lam = 0;
    do {
      if (power == lam) {
        s = l;
        power *= 2;
        lam = 0;
      }
      lam++;
      length++;
      l = RepPair(*l) + 1;
      do_derefa(v, l, derefa2_unk, derefa2_nonvar);
    } while (*l != *s && IsPairTerm(*l));
  }
  *tailp = l;

  return length;
}

X_API Term YAP_MkApplTerm(YAP_Functor f, UInt arity, Term args[]) {
  CACHE_REGS
  Term t;
  BACKUP_H();

  if (HR + arity > ASP - 1024)
    t = TermNil;
  else
    t = Yap_MkApplTerm(f, arity, args);

  RECOVER_H();
  return t;
}

X_API Term YAP_MkNewApplTerm(YAP_Functor f, UInt arity) {
  CACHE_REGS
  Term t;
  BACKUP_H();

  if (HR + arity > ASP - 1024)
    t = TermNil;
  else
    t = Yap_MkNewApplTerm(f, arity);

  RECOVER_H();
  return t;
}

X_API YAP_Functor YAP_FunctorOfTerm(Term t) { return (FunctorOfTerm(t)); }

X_API Term YAP_ArgOfTerm(UInt n, Term t) { return (ArgOfTerm(n, t)); }

X_API Term *YAP_ArgsOfTerm(Term t) {
  if (IsApplTerm(t))
    return RepAppl(t) + 1;
  else if (IsPairTerm(t))
    return RepPair(t);
  return NULL;
}

X_API YAP_Functor YAP_MkFunctor(YAP_Atom a, UInt n) {
  return (Yap_MkFunctor(a, n));
}

X_API YAP_Atom YAP_NameOfFunctor(YAP_Functor f) { return (NameOfFunctor(f)); }

X_API UInt YAP_ArityOfFunctor(YAP_Functor f) { return (ArityOfFunctor(f)); }

X_API void *YAP_ExtraSpaceCut(void) {
  CACHE_REGS
  void *ptr;
  BACKUP_B();

  ptr = (void *)(((CELL *)(Yap_REGS.CUT_C_TOP)) -
                 (((yamop *)Yap_REGS.CUT_C_TOP->try_userc_cut_yamop)
                      ->y_u.OtapFs.extra));

  RECOVER_B();
  return (ptr);
}

X_API void *YAP_ExtraSpace(void) {
  CACHE_REGS
  void *ptr;
  BACKUP_B();
  BACKUP_H();

  /* find a pointer to extra space allocable */
  ptr = (void *)((CELL *)(B + 1) + P->y_u.OtapFs.s);
  B->cp_h = HR;

  RECOVER_H();
  RECOVER_B();
  return (ptr);
}

X_API void YAP_cut_up(void) {
  CACHE_REGS
  BACKUP_B();
  {
    while (POP_CHOICE_POINT(B->cp_b)) {
      POP_EXECUTE();
    }
  }
  /* This is complicated: make sure we can restore the ASP
     pointer back to where cut_up called it. Slots depend on it. */
  if (ENV > B->cp_env) {
    ASP = B->cp_env;
  }
#ifdef YAPOR
  {
    choiceptr cut_pt;

    cut_pt = B->cp_b;
    /* make sure we prune C-choicepoints */
    if (POP_CHOICE_POINT(B->cp_b)) {
      POP_EXECUTE();
    }
    CUT_prune_to(cut_pt);
    Yap_TrimTrail();
    B = cut_pt;
  }
#else
  /* make sure we prune C-choicepoints */
  if (POP_CHOICE_POINT(B->cp_b)) {
    POP_EXECUTE();
  }
  Yap_TrimTrail();
  B = B->cp_b; /* cut_fail */
#endif
  HB = B->cp_h; /* cut_fail */
  RECOVER_B();
}

X_API bool YAP_Unify(Term t1, Term t2) {
  Int out;
  BACKUP_MACHINE_REGS();

  out = Yap_unify(t1, t2);

  RECOVER_MACHINE_REGS();
  return out;
}

X_API int YAP_Unifiable(Term t1, Term t2) {
  int out;
  BACKUP_MACHINE_REGS();

  out = Yap_Unifiable(t1, t2);

  RECOVER_MACHINE_REGS();
  return out;
}

/* == */
X_API int YAP_ExactlyEqual(Term t1, Term t2) {
  int out;
  BACKUP_MACHINE_REGS();

  out = Yap_eq(t1, t2);

  RECOVER_MACHINE_REGS();
  return out;
}

/* =@= */
X_API int YAP_Variant(Term t1, Term t2) {
  int out;
  BACKUP_MACHINE_REGS();

  out = Yap_Variant(Deref(t1), Deref(t2));

  RECOVER_MACHINE_REGS();
  return out;
}

/* =@= */
X_API Int YAP_TermHash(Term t, Int sz, Int depth, int variant) {
  Int out;

  BACKUP_MACHINE_REGS();

  out = Yap_TermHash(t, sz, depth, variant);

  RECOVER_MACHINE_REGS();
  return out;
}

X_API Int YAP_CurrentSlot(void) {
  CACHE_REGS
  return Yap_CurrentSlot();
}

X_API Int YAP_NewSlots(int n) {
  CACHE_REGS
  return Yap_NewSlots(n);
}

X_API Int YAP_InitSlot(Term t) {
  CACHE_REGS
  return Yap_InitSlot(t);
}

X_API int YAP_RecoverSlots(int n, Int top_slot) {
  CACHE_REGS
  return Yap_RecoverSlots(n, top_slot);
}

X_API Term YAP_GetFromSlot(Int slot) {
  CACHE_REGS
  return Yap_GetFromSlot(slot);
}

X_API Term *YAP_AddressFromSlot(Int slot) {
  CACHE_REGS
  return Yap_AddressFromSlot(slot);
}

X_API Term *YAP_AddressOfTermInSlot(Int slot) {
  CACHE_REGS
  Term *b = Yap_AddressFromSlot(slot);
  Term a = *b;
restart:
  if (!IsVarTerm(a)) {
    return (b);
  } else if (a == (CELL)b) {
    return (b);
  } else {
    b = (CELL *)a;
    a = *b;
    goto restart;
  }
}

X_API void YAP_PutInSlot(Int slot, Term t) {
  CACHE_REGS
  Yap_PutInSlot(slot, t);
}

typedef Int (*CPredicate0)(void);

typedef Int (*CPredicate1)(yhandle_t);

typedef Int (*CPredicate2)(yhandle_t, yhandle_t);

typedef Int (*CPredicate3)(yhandle_t, yhandle_t, yhandle_t);

typedef Int (*CPredicate4)(yhandle_t, yhandle_t, yhandle_t, yhandle_t);

typedef Int (*CPredicate5)(yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                           yhandle_t);

typedef Int (*CPredicate6)(yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                           yhandle_t, yhandle_t);

typedef Int (*CPredicate7)(yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                           yhandle_t, yhandle_t, yhandle_t);

typedef Int (*CPredicate8)(yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                           yhandle_t, yhandle_t, yhandle_t, yhandle_t);

typedef Int (*CPredicate9)(yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                           yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                           yhandle_t);

typedef Int (*CPredicate10)(yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                            yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                            yhandle_t, yhandle_t);

typedef Int (*CPredicateV)(yhandle_t, yhandle_t, struct foreign_context *);

static Int execute_cargs(PredEntry *pe, CPredicate exec_code USES_REGS) {
  Int rc;
  yhandle_t a1;
  switch (pe->ArityOfPE) {
  case 0: {
    CPredicate0 code0 = (CPredicate0)exec_code;
    return code0();
  }
  case 1: {
    CPredicate1 code1 = (CPredicate1)exec_code;
    a1 = Yap_InitSlots(1, &ARG1);
    rc = code1(a1);
  } break;
  case 2: {
    CPredicate2 code2 = (CPredicate2)exec_code;
    a1 = Yap_InitSlots(2, &ARG1);
    rc = code2(a1, a1 + 1);
  } break;
  case 3: {
    CPredicate3 code3 = (CPredicate3)exec_code;
    a1 = Yap_InitSlots(3, &ARG1);
    rc = code3(a1, a1 + 1, a1 + 2);
  } break;
  case 4: {
    CPredicate4 code4 = (CPredicate4)exec_code;
    a1 = Yap_InitSlots(4, &ARG1);
    rc = code4(a1, a1 + 1, a1 + 2, a1 + 3);
  } break;
  case 5: {
    CPredicate5 code5 = (CPredicate5)exec_code;
    a1 = Yap_InitSlots(5, &ARG1);
    rc = code5(a1, a1 + 1, a1 + 2, a1 + 3, a1 + 4);
  } break;

  case 6: {
    CPredicate6 code6 = (CPredicate6)exec_code;
    a1 = Yap_InitSlots(6, &ARG1);
    rc = code6(a1, a1 + 1, a1 + 2, a1 + 3, a1 + 4, a1 + 5);
  } break;
  case 7: {
    CPredicate7 code7 = (CPredicate7)exec_code;
    a1 = Yap_InitSlots(7, &ARG1);
    rc = code7(a1, a1 + 1, a1 + 2, a1 + 3, a1 + 4, a1 + 5, a1 + 6);
  } break;
  case 8: {
    CPredicate8 code8 = (CPredicate8)exec_code;
    a1 = Yap_InitSlots(8, &ARG1);
    rc = code8(a1, a1 + 1, a1 + 2, a1 + 3, a1 + 4, a1 + 5, a1 + 6, a1 + 7);
  } break;
  case 9: {
    CPredicate9 code9 = (CPredicate9)exec_code;
    a1 = Yap_InitSlots(9, &ARG1);
    rc = code9(a1, a1 + 1, a1 + 2, a1 + 3, a1 + 4, a1 + 5, a1 + 6, a1 + 7,
               a1 + 8);
  } break;
  case 10: {
    CPredicate10 code10 = (CPredicate10)exec_code;
    a1 = Yap_InitSlots(10, &ARG1);
    rc = code10(a1, a1 + 1, a1 + 2, a1 + 3, a1 + 4, a1 + 5, a1 + 6, a1 + 7,
                a1 + 8, a1 + 9);
  } break;
  default:
    YAP_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "YAP only supports SWI C-call with arity =< 10");
    return false;
  }
  Yap_RecoverSlots(pe->ArityOfPE, a1);
  return rc;
}

typedef uintptr_t (*CBPredicate0)(struct foreign_context *);

typedef uintptr_t (*CBPredicate1)(yhandle_t, struct foreign_context *);

typedef uintptr_t (*CBPredicate2)(yhandle_t, yhandle_t,
                                  struct foreign_context *);

typedef uintptr_t (*CBPredicate3)(yhandle_t, yhandle_t, yhandle_t,
                                  struct foreign_context *);

typedef uintptr_t (*CBPredicate4)(yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                                  struct foreign_context *);

typedef uintptr_t (*CBPredicate5)(yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                                  yhandle_t, struct foreign_context *);

typedef uintptr_t (*CBPredicate6)(yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                                  yhandle_t, yhandle_t,
                                  struct foreign_context *);

typedef uintptr_t (*CBPredicate7)(yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                                  yhandle_t, yhandle_t, yhandle_t,
                                  struct foreign_context *);

typedef uintptr_t (*CBPredicate8)(yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                                  yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                                  struct foreign_context *);

typedef uintptr_t (*CBPredicate9)(yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                                  yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                                  yhandle_t, struct foreign_context *);

typedef uintptr_t (*CBPredicate10)(yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                                   yhandle_t, yhandle_t, yhandle_t, yhandle_t,
                                   yhandle_t, yhandle_t,
                                   struct foreign_context *);

static uintptr_t execute_cargs_back(PredEntry *pe, CPredicate exec_code,
                                    struct foreign_context *ctx USES_REGS) {
  switch (pe->ArityOfPE) {
  case 0: {
    CBPredicate0 code0 = (CBPredicate0)exec_code;
    return code0(ctx);
  }
  case 1: {
    CBPredicate1 code1 = (CBPredicate1)exec_code;
    yhandle_t a1 = Yap_InitSlots(1, &B->cp_a1);
    return code1(a1, ctx);
  }
  case 2: {
    CBPredicate2 code2 = (CBPredicate2)exec_code;
    yhandle_t a1 = Yap_InitSlots(2, &B->cp_a1);
    return code2(a1, a1 + 1, ctx);
  }
  case 3: {
    CBPredicate3 code3 = (CBPredicate3)exec_code;
    yhandle_t a1 = Yap_InitSlots(3, &B->cp_a1);
    return code3(a1, a1 + 1, a1 + 2, ctx);
  }
  case 4: {
    CBPredicate4 code4 = (CBPredicate4)exec_code;
    yhandle_t a1 = Yap_InitSlots(4, &B->cp_a1);
    return code4(a1, a1 + 1, a1 + 2, a1 + 3, ctx);
  }
  case 5: {
    CBPredicate5 code5 = (CBPredicate5)exec_code;
    yhandle_t a1 = Yap_InitSlots(5, &B->cp_a1);
    return code5(a1, a1 + 1, a1 + 2, a1 + 3, a1 + 4, ctx);
  }
  case 6: {
    CBPredicate6 code6 = (CBPredicate6)exec_code;
    yhandle_t a1 = Yap_InitSlots(6, &B->cp_a1);
    return code6(a1, a1 + 1, a1 + 2, a1 + 3, a1 + 4, a1 + 5, ctx);
  }
  case 7: {
    CBPredicate7 code7 = (CBPredicate7)exec_code;
    yhandle_t a1 = Yap_InitSlots(7, &B->cp_a1);
    return code7(a1, a1 + 1, a1 + 2, a1 + 3, a1 + 4, a1 + 5, a1 + 6, ctx);
  }
  case 8: {
    CBPredicate8 code8 = (CBPredicate8)exec_code;
    yhandle_t a1 = Yap_InitSlots(8, &B->cp_a1);
    return code8(a1, a1 + 1, a1 + 2, a1 + 3, a1 + 4, a1 + 5, a1 + 6, a1 + 7,
                 ctx);
  }
  case 9: {
    CBPredicate9 code9 = (CBPredicate9)exec_code;
    yhandle_t a1 = Yap_InitSlots(9, &B->cp_a1);
    return code9(a1, a1 + 1, a1 + 2, a1 + 3, a1 + 4, a1 + 5, a1 + 6, a1 + 7,
                 a1 + 8, ctx);
  }
  case 10: {
    CBPredicate10 code10 = (CBPredicate10)exec_code;
    yhandle_t a1 = Yap_InitSlots(10, &B->cp_a1);
    return code10(a1, a1 + 1, a1 + 2, a1 + 3, a1 + 4, a1 + 5, a1 + 6, a1 + 7,
                  a1 + 8, a1 + 9, ctx);
  }
  default:
    YAP_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "YAP only supports SWI C-call with arity =< 10");
    return (FALSE);
  }
}

static uintptr_t complete_fail(choiceptr ptr, int has_cp USES_REGS) {
  // this case is easy, jut be sure to throw everything
  // after the old B;
  while (B && B->cp_b && B->cp_b <= ptr) {
    B = B->cp_b;
  }
  if (has_cp)
    return do_cut(FALSE);
  return FALSE;
}

static uintptr_t complete_exit(choiceptr ptr, int has_cp,
                               int cut_all USES_REGS) {
  // the user often leaves open frames, especially in forward execution
  while (B && (!ptr || B < ptr)) {
    if (cut_all || B->cp_ap == NOCODE) { /* separator */
      do_cut(TRUE);                      // pushes B up
      continue;
    } else if (B->cp_ap->opc == RETRY_USERC_OPCODE && B->cp_b == ptr) {
      // started the current choicepoint, I hope
      return do_cut(TRUE);
    } else
      break; // oops, there is something else
  }
  if (!ptr || B < ptr) {
    // we're still not there yet
    choiceptr new = B;
    while (new &&new < ptr) {
      if (new->cp_ap == NOCODE) /* separator */
        new->cp_ap = FAILCODE;  // there are choice-points above but at least,
                                // these won't harm innocent code
      else if (new->cp_ap->opc == RETRY_USERC_OPCODE && new->cp_b == ptr) {
        // I can't cut, but I can tag it as done
        new->cp_ap = FAILCODE; // there are choice-points above but at least,
                               // these won't harm innocent code
      }
      new = new->cp_b;
    }
  }
  if (has_cp) {
    if (B == ptr) {
      return do_cut(TRUE);
    } else {
      ptr->cp_ap = FAILCODE;
    }
  }
  return TRUE;
}

X_API Int YAP_Execute(PredEntry *pe, CPredicate exec_code) {
  BACKUP_MACHINE_REGS();
  CACHE_REGS
  Int ret;
  Int OASP = LCL0 - (CELL *)B;
  //  Term omod = CurrentModule;
  // if (pe->PredFlags & CArgsPredFlag) {
  //  CurrentModule = pe->ModuleOfPred;
  //}
  int lvl = push_text_stack();
  yhandle_t hdl = Yap_CurrentHandle();
  if (pe->PredFlags & SWIEnvPredFlag) {
    CPredicateV codev = (CPredicateV)exec_code;
    struct foreign_context ctx;

    ctx.engine = NULL;
    yhandle_t s0 = Yap_InitSlots(pe->ArityOfPE, &ARG1);
    PP = pe;
    ret = codev(s0, 0, &ctx);
  } else if (pe->PredFlags & CArgsPredFlag) {
    PP = pe;
    ret = execute_cargs(pe, exec_code PASS_REGS);
  } else {
    PP = pe;
    ret = (exec_code)(PASS_REGS1);
  }
  PP = NULL;
  // check for junk: open frames, etc */
  if (ret)
    complete_exit(((choiceptr)(LCL0 - OASP)), FALSE, FALSE PASS_REGS);
  else {
    complete_fail(((choiceptr)(LCL0 - OASP)), FALSE PASS_REGS);
  }
    Yap_RecoverHandles(0, hdl);
  pop_text_stack( lvl );
// CurrentModule = omod;
   RECOVER_MACHINE_REGS();
  if (!ret) {
    Yap_RaiseException();
  }
  return ret;
}

#define FRG_REDO_MASK 0x00000003L
#define FRG_REDO_BITS 2
#define REDO_INT 0x02 /* Returned an integer */
#define REDO_PTR 0x03 /* returned a pointer */

X_API Int YAP_ExecuteFirst(PredEntry *pe, CPredicate exec_code) {
  CACHE_REGS
  CELL ocp = LCL0 - (CELL *)B;
  /* for slots to work */
  Int CurSlot = Yap_StartSlots();
  if (pe->PredFlags &
      (SWIEnvPredFlag | CArgsPredFlag | ModuleTransparentPredFlag)) {
    uintptr_t val;
    CPredicateV codev = (CPredicateV)exec_code;
    struct foreign_context *ctx =
        (struct foreign_context *)(&EXTRA_CBACK_ARG(pe->ArityOfPE, 1));

    PP = pe;
    ctx->control = FRG_FIRST_CALL;
    ctx->engine = NULL; //(PL_local_data *)Yap_regp;
    ctx->context = (uintptr_t)NULL;
    if (pe->PredFlags & CArgsPredFlag) {
      val = execute_cargs_back(pe, exec_code, ctx PASS_REGS);
    } else {
      val = codev(Yap_InitSlots(pe->ArityOfPE, &ARG1), 0, ctx);
    }
    Yap_CloseSlots(CurSlot);
    PP = NULL;
    if (val == 0) {
      if (Yap_RaiseException()) {
        return false;
      }
      return complete_fail(((choiceptr)(LCL0 - ocp)), TRUE PASS_REGS);
    } else if (val == 1) { /* TRUE */
      return complete_exit(((choiceptr)(LCL0 - ocp)), TRUE, FALSE PASS_REGS);
    } else {
      if ((val & REDO_PTR) == REDO_PTR)
        ctx->context = (uintptr_t)(val & ~REDO_PTR);
      else
        ctx->context = (uintptr_t)((val & ~REDO_PTR) >> FRG_REDO_BITS);
      /* fix dropped cps */
      return complete_exit(((choiceptr)(LCL0 - ocp)), FALSE, FALSE PASS_REGS);
    }
  } else {
    Int ret = (exec_code)(PASS_REGS1);
    Yap_CloseSlots(CurSlot);
    if (!ret) {
      Yap_RaiseException();
    }
    return ret;
  }
}

X_API Int YAP_ExecuteOnCut(PredEntry *pe, CPredicate exec_code,
                           struct cut_c_str *top) {
  CACHE_REGS
  Int oB = LCL0 - (CELL *)B;
  Int val;
  /* for slots to work */
  yhandle_t CurSlot = Yap_StartSlots();
  /* find out where we belong */
  while (B < (choiceptr)top) {
    oB = LCL0 - (CELL *)B;
    B = B->cp_b;
  }
  PP = pe;
  if (pe->PredFlags & (SWIEnvPredFlag | CArgsPredFlag)) {
    // SWI Emulation
    CPredicateV codev = (CPredicateV)exec_code;
    struct foreign_context *ctx =
        (struct foreign_context *)(&EXTRA_CBACK_ARG(pe->ArityOfPE, 1));
    CELL *args = B->cp_args;

    B = (choiceptr)(LCL0 - oB);
    ctx->control = FRG_CUTTED;
    ctx->engine = NULL; //(PL_local_data *)Yap_regp;
    if (pe->PredFlags & CArgsPredFlag) {
      val = execute_cargs_back(pe, exec_code, ctx PASS_REGS);
    } else {
      val = codev(Yap_InitSlots(pe->ArityOfPE, args), 0, ctx);
    }
  } else {
    Int oYENV = LCL0 - YENV;
    yamop *oP = P, *oCP = CP;
    // YAP Native
    B = (choiceptr)(LCL0 - oB);
    val = exec_code(PASS_REGS1);
    YENV = LCL0 - oYENV;
    P = oP;
    CP = oCP;
  }
  Yap_CloseSlots(CurSlot);
  PP = NULL;
  //    B = LCL0-(CELL*)oB;
  if (!val && Yap_RaiseException()) {
    return false;
  } else { /* TRUE */
    return val;
  }
}

X_API Int YAP_ExecuteNext(PredEntry *pe, CPredicate exec_code) {
  CACHE_REGS
  /* for slots to work */
  Yap_StartSlots();
  UInt ocp = LCL0 - (CELL *)B;
  if (pe->PredFlags & (SWIEnvPredFlag | CArgsPredFlag)) {
    Int val;
    CPredicateV codev = (CPredicateV)exec_code;
    struct foreign_context *ctx =
        (struct foreign_context *)(&EXTRA_CBACK_ARG(pe->ArityOfPE, 1));

    PP = pe;
    ctx->control = FRG_REDO;
    if (pe->PredFlags & CArgsPredFlag) {
      val = execute_cargs_back(pe, exec_code, ctx PASS_REGS);
    } else {
      val = codev(Yap_InitSlots(pe->ArityOfPE, &ARG1), 0, ctx);
    }
    /* we are below the original choice point ?? */
    /* make sure we clean up the frames left by the user */
    PP = NULL;
    if (val == 0) {
      if (Yap_RaiseException()) {
        return FALSE;
      } else {
        return complete_fail(((choiceptr)(LCL0 - ocp)), TRUE PASS_REGS);
      }
    } else if (val == 1) { /* TRUE */
      return complete_exit(((choiceptr)(LCL0 - ocp)), TRUE, FALSE PASS_REGS);
    } else {
      if ((val & REDO_PTR) == REDO_PTR)
        ctx->context = (uintptr_t)(val & ~REDO_PTR);
      else
        ctx->context = (uintptr_t)((val & ~REDO_PTR) >> FRG_REDO_BITS);
    }
    /* fix dropped cps */
    return complete_exit(((choiceptr)(LCL0 - ocp)), FALSE, FALSE PASS_REGS);
  } else {
    Int ret = (exec_code)(PASS_REGS1);
    if (!ret) {
      Yap_RaiseException();
    }
    return ret;
  }
}

X_API void *YAP_ReallocSpaceFromYap(void *ptr, size_t size) {
  CACHE_REGS
  void *new_ptr;
  BACKUP_MACHINE_REGS();
  while ((new_ptr = Yap_ReallocCodeSpace(ptr, size)) == NULL) {
    if (!Yap_growheap(FALSE, size, NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
      return NULL;
    }
  }
  RECOVER_MACHINE_REGS();
  return new_ptr;
}

X_API void *YAP_AllocSpaceFromYap(size_t size) {
  CACHE_REGS
  void *ptr;
  BACKUP_MACHINE_REGS();

  while ((ptr = Yap_AllocCodeSpace(size)) == NULL) {
    if (!Yap_growheap(FALSE, size, NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
      return NULL;
    }
  }
  RECOVER_MACHINE_REGS();
  return ptr;
}

X_API void YAP_FreeSpaceFromYap(void *ptr) { Yap_FreeCodeSpace(ptr); }

/*  */
/**
 * copy a string to a buffer, the buffer must have been malloced
 *
 * @param t the text, or string
 * @param buf the user-provided buffer
 * @param bufsize bu
 *
 * @return
 */
X_API char *YAP_StringToBuffer(Term t, char *buf, unsigned int bufsize) {
  CACHE_REGS
  BACKUP_MACHINE_REGS();
  seq_tv_t inp, out;
  int l = push_text_stack();
  inp.val.t = t;
  inp.type = YAP_STRING_ATOMS_CODES | YAP_STRING_STRING | YAP_STRING_ATOM |
             YAP_STRING_TRUNC | YAP_STRING_MALLOC;
  inp.max = bufsize;
  out.type = YAP_STRING_CHARS;
  out.val.c = buf;
  out.enc = ENC_ISO_UTF8;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS)) {
    pop_text_stack(l);
    RECOVER_MACHINE_REGS();
    return NULL;
  } else {
    RECOVER_MACHINE_REGS();
    if (buf == out.val.c) {
      return buf;
    } else {
      return pop_output_text_stack(l, out.val.c);
    }
  }
}

/* copy a string to a buffer */
X_API Term YAP_BufferToString(const char *s) {
  Term t;
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term YAP_NBufferToString(const char *s, size_t len) {
  Term t;
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_CODES | YAP_STRING_NCHARS | YAP_STRING_TRUNC;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term YAP_WideBufferToString(const wchar_t *s) {
  Term t;
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_CODES;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term YAP_NWideBufferToString(const wchar_t *s, size_t len) {
  Term t;
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_CODES | YAP_STRING_NCHARS | YAP_STRING_TRUNC;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term YAP_ReadBuffer(const char *s, Term *tp) {
  CACHE_REGS
  Term tv, t;
  BACKUP_H();

  if (*tp)
    tv = *tp;
  else
    tv = (Term)0;
  LOCAL_ErrorMessage = NULL;
  while (!(t = Yap_BufferToTermWithPrioBindings(s, TermNil, tv, strlen(s) + 1,
                                                GLOBAL_MaxPriority))) {
    if (LOCAL_ErrorMessage) {
      if (!strcmp(LOCAL_ErrorMessage, "Stack Overflow")) {
        if (!Yap_dogc(0, NULL PASS_REGS)) {
          *tp = MkAtomTerm(Yap_LookupAtom(LOCAL_ErrorMessage));
          LOCAL_ErrorMessage = NULL;
          RECOVER_H();
          return 0L;
        }
      } else if (!strcmp(LOCAL_ErrorMessage, "Heap Overflow")) {
        if (!Yap_growheap(FALSE, 0, NULL)) {
          *tp = MkAtomTerm(Yap_LookupAtom(LOCAL_ErrorMessage));
          LOCAL_ErrorMessage = NULL;
          RECOVER_H();
          return 0L;
        }
      } else if (!strcmp(LOCAL_ErrorMessage, "Trail Overflow")) {
        if (!Yap_growtrail(0, FALSE)) {
          *tp = MkAtomTerm(Yap_LookupAtom(LOCAL_ErrorMessage));
          LOCAL_ErrorMessage = NULL;
          RECOVER_H();
          return 0L;
        }
      } else {
        RECOVER_H();
        return 0L;
      }
      LOCAL_ErrorMessage = NULL;
      RECOVER_H();
      return 0;
    } else {
      break;
    }
  }
  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API YAP_Term YAP_BufferToAtomList(const char *s) {
  Term t;
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_ATOMS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string of size len to a buffer */
X_API Term YAP_NBufferToAtomList(const char *s, size_t len) {
  Term t;
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_ATOMS | YAP_STRING_NCHARS | YAP_STRING_TRUNC;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term YAP_WideBufferToAtomList(const wchar_t *s) {
  Term t;
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_ATOMS;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string of size len to a buffer */
X_API Term YAP_NWideBufferToAtomList(const wchar_t *s, size_t len) {
  Term t;
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_ATOMS | YAP_STRING_NCHARS | YAP_STRING_TRUNC;
  out.max = len;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string of size len to a buffer */
X_API Term YAP_NWideBufferToAtomDiffList(const wchar_t *s, Term t0,
                                         size_t len) {
  Term t;
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS;
  out.type =
      YAP_STRING_ATOMS | YAP_STRING_NCHARS | YAP_STRING_TRUNC | YAP_STRING_DIFF;
  out.max = len;
  out.dif = t0;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term YAP_BufferToDiffList(const char *s, Term t0) {
  Term t;
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_CODES | YAP_STRING_DIFF;
  out.dif = t0;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string of size len to a buffer */
X_API Term YAP_NBufferToDiffList(const char *s, Term t0, size_t len) {
  Term t;
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c0 = s;
  inp.type = YAP_STRING_CHARS;
  out.type =
      YAP_STRING_CODES | YAP_STRING_NCHARS | YAP_STRING_TRUNC | YAP_STRING_DIFF;
  out.max = len;
  out.dif = t0;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string to a buffer */
X_API Term YAP_WideBufferToDiffList(const wchar_t *s, Term t0) {
  Term t;
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_CODES | YAP_STRING_DIFF;
  out.dif = t0;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

/* copy a string of size len to a buffer */
X_API Term YAP_NWideBufferToDiffList(const wchar_t *s, Term t0, size_t len) {
  Term t;
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w0 = s;
  inp.type = YAP_STRING_WCHARS;
  out.type =
      YAP_STRING_CODES | YAP_STRING_NCHARS | YAP_STRING_TRUNC | YAP_STRING_DIFF;
  out.max = len;
  out.dif = t0;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS))
    return 0L;
  t = out.val.t;

  RECOVER_H();
  return t;
}

X_API void YAP_Error__(const char *file, const char *function, int lineno,int myerrno, Term t, const char *buf, ...) {
#define YAP_BUF_SIZE 512
  va_list ap;
  char tmpbuf[YAP_BUF_SIZE];

  if (!myerrno)
    myerrno = SYSTEM_ERROR_INTERNAL;
  if (t == 0L)
    t = TermNil;
  if (buf != NULL) {
    va_start(ap, buf);
#if HAVE_VSNPRINTF
    (void)vsnprintf(tmpbuf, YAP_BUF_SIZE, buf, ap);
#else
    (void)vsprintf(tmpbuf, buf, ap);
#endif
    va_end(ap);
  } else {
    tmpbuf[0] = '\0';
  }
  Yap_ThrowError__(file,function,lineno,myerrno, t, tmpbuf);
}

X_API YAP_PredEntryPtr YAP_FunctorToPred(YAP_Functor func) {
  CACHE_REGS
  return RepPredProp(PredPropByFunc(func, CurrentModule));
}

X_API YAP_PredEntryPtr YAP_AtomToPred(YAP_Atom at) {
  CACHE_REGS
  return RepPredProp(PredPropByAtom(at, CurrentModule));
}

X_API YAP_PredEntryPtr YAP_FunctorToPredInModule(YAP_Functor func, Term mod) {
  return RepPredProp(PredPropByFunc(func, mod));
}

X_API YAP_PredEntryPtr YAP_AtomToPredInModule(YAP_Atom at, Term mod) {
  return RepPredProp(PredPropByAtom(at, mod));
}

/*
static int run_emulator(USES_REGS1) {
  int out;

  out = Yap_absmi(0);
  LOCAL_PrologMode |= UserCCallMode;
  return out;
}
*/

X_API bool YAP_EnterGoal(YAP_PredEntryPtr ape, CELL *ptr, YAP_dogoalinfo *dgi) {
  CACHE_REGS
  PredEntry *pe = ape;
  bool out;
  //   fprintf(stderr,"1EnterGoal: H=%d ENV=%p B=%d TR=%d P=%p CP=%p
  //   Slots=%d\n",HR-H0,LCL0-ENV,LCL0-(CELL*)B,(CELL*)TR-LCL0, P, CP,
  //   LOCAL_CurSlot);

  BACKUP_MACHINE_REGS();
  dgi->lvl = push_text_stack();
  LOCAL_ActiveError->errorNo = YAP_NO_ERROR;
  LOCAL_PrologMode = UserMode;
  dgi->p = P;
  dgi->cp = CP;
  dgi->b0 = LCL0 - (CELL *)B;
  dgi->env0 = LCL0 - ENV;
  // ensure our current ENV receives current P.

  Yap_PrepGoal(pe->ArityOfPE, nullptr, B PASS_REGS);
  P = pe->CodeOfPred;
  // __android_log_print(ANDROID_LOG_INFO, "YAP ", "ap=%p %d %x %x args=%x,%x
  // slot=%d", pe, pe->CodeOfPred->opc, FAILCODE, Deref(ARG1), Deref(ARG2),
  // LOCAL_CurSlot);
  dgi->b_entry = LCL0 - (CELL *)B;
  dgi->h = HR - H0;
  dgi->tr = (CELL *)TR - LCL0;
  // fprintf(stderr,"PrepGoal: H=%d ENV=%p B=%d TR=%d P=%p CP=%p Slots=%d\n",
  //  HR-H0,LCL0-ENV,LCL0-(CELL*)B,(CELL*)TR-LCL0, P, CP, LOCAL_CurSlot);
  out = Yap_exec_absmi(true, false);
  //   fprintf(stderr,"EnterGoal success=%d: H=%d ENV=%p B=%d TR=%d P=%p CP=%p
  //   Slots=%d\n", out,HR-H0,LCL0-ENV,LCL0-(CELL*)B,(CELL*)TR-LCL0, P, CP,
  //   LOCAL_CurSlot);
  dgi->b_exit = LCL0 - (CELL *)B;
  if (out) {
    dgi->EndSlot = LOCAL_CurSlot;
    Yap_StartSlots();
  } else {
    LOCAL_CurSlot =
        dgi->CurSlot; // ignore any slots created within the called goal
  }
  pop_text_stack(dgi->lvl);
  RECOVER_MACHINE_REGS();
  return out;
}

X_API bool YAP_RetryGoal(YAP_dogoalinfo *dgi) {
  CACHE_REGS
  choiceptr myB, myB0;
  bool out;

  BACKUP_MACHINE_REGS();
  dgi->lvl = push_text_stack();
  myB = (choiceptr)(LCL0 - dgi->b_exit);
  myB0 = (choiceptr)(LCL0 - dgi->b_entry);
  CP = myB->cp_cp;
  /* sanity check */
  if (B >= myB0) {
    return false;
  }
  if (B < myB) {
    // get rid of garbage choice-points
    B = myB;
  }
  // fprintf(stderr,"RetryGoal: H=%d ENV=%p B=%d TR=%d P=%p CP=%p Slots=%d\n",
  //  HR-H0,LCL0-ENV,LCL0-(CELL*)B,(CELL*)TR-LCL0, P, CP, LOCAL_CurSlot);
  P = FAILCODE;
  /* make sure we didn't leave live slots when we backtrack */
  ASP = (CELL *)B;
  LOCAL_CurSlot = dgi->EndSlot;
  out = Yap_exec_absmi(true, true   );
  if (out) {
    dgi->EndSlot = LOCAL_CurSlot;
    dgi->b_exit = LCL0 - (CELL *)B;
  } else {
  printf("F %ld\n", dgi->CurSlot);
    LOCAL_CurSlot =
        dgi->CurSlot; // ignore any slots created within the called goal
  }
    pop_text_stack(dgi->lvl);
  RECOVER_MACHINE_REGS();
  return out;
}

X_API bool YAP_LeaveGoal(bool successful, YAP_dogoalinfo *dgi) {
  CACHE_REGS

  //   fprintf(stderr,"LeaveGoal success=%d: H=%d ENV=%p B=%ld myB=%ld TR=%d
  //   P=%p CP=%p Slots=%d\n",
  //   successful,HR-H0,LCL0-ENV,LCL0-(CELL*)B,dgi->b0,(CELL*)TR-LCL0, P, CP,
  //   LOCAL_CurSlot);
  BACKUP_MACHINE_REGS();

  dgi->lvl = push_text_stack();
    if (successful) {
      choiceptr nB = (choiceptr)(LCL0 - dgi->b_entry);
      if (B <= nB) {
        B = nB;
      }
      Yap_TrimTrail();
      B = B->cp_b;
    } else if (LOCAL_PrologMode & AsyncIntMode) {
    Yap_signal(YAP_FAIL_SIGNAL);
  }
  B = (choiceptr)(LCL0 - dgi->b0);
#ifdef DEPTH_LIMIT
  DEPTH = B->cp_depth;
#endif
  P = dgi->p;
  CP = dgi->cp;
  YENV = ENV = LCL0-dgi->env0;
    LOCAL_CurSlot =
        dgi->CurSlot; // ignore any slots created within the called goal
    pop_text_stack(dgi->lvl);
  RECOVER_MACHINE_REGS();
  // fprintf(stderr," LeftGoal success=%d: H=%d ENV=%p B=%d TR=%d P=%p CP=%p
  //  Slots=%d\n",    successful,HR-H0,LCL0-ENV,LCL0-(CELL*)B,(CELL*)TR-LCL0, P,
  //  CP, LOCAL_CurSlot);
  return TRUE;
}

X_API Int YAP_RunGoal(Term t) {
  CACHE_REGS
  Term out;
  yhandle_t cslot = LOCAL_CurSlot;
  BACKUP_MACHINE_REGS();

int lvl = push_text_stack();

  LOCAL_AllowRestart = FALSE;
  LOCAL_PrologMode = UserMode;
  out = Yap_RunTopGoal(t, true);
  LOCAL_PrologMode = UserCCallMode;
  // should we catch the exception or pass it through?
  // We'll pass it through
  RECOVER_MACHINE_REGS();
  LOCAL_CurSlot = cslot;
    pop_text_stack(lvl);
  return out;
}

X_API Term YAP_AllocExternalDataInStack(size_t bytes) {
  CELL *pt;
  Term t = Yap_AllocExternalDataInStack(EXTERNAL_BLOB, bytes, &pt);
  if (t == TermNil)
    return 0L;
  return t;
}

X_API YAP_Bool YAP_IsExternalDataInStackTerm(Term t) {
  return IsExternalBlobTerm(t, EXTERNAL_BLOB);
}

X_API void *YAP_ExternalDataInStackFromTerm(Term t) {
  return ExternalBlobFromTerm(t);
}

X_API YAP_opaque_tag_t YAP_NewOpaqueType(struct YAP_opaque_handler_struct *f) {
  int i;
  if (!GLOBAL_OpaqueHandlersCount) {
    GLOBAL_OpaqueHandlers =
        malloc(sizeof(YAP_opaque_handler_t) * USER_BLOB_END);
    if (!GLOBAL_OpaqueHandlers) {
      /* no room */
      return -1;
    }
    GLOBAL_OpaqueHandlersCount = USER_BLOB_START;
  } else if (GLOBAL_OpaqueHandlersCount == USER_BLOB_END) {
    /* all types used */
    return -1;
  }
  i = GLOBAL_OpaqueHandlersCount++;
  memmove(GLOBAL_OpaqueHandlers + i, f, sizeof(YAP_opaque_handler_t));
  return i;
}

X_API Term YAP_NewOpaqueObject(YAP_opaque_tag_t blob_tag, size_t bytes) {
  CELL *pt;
  Term t = Yap_AllocExternalDataInStack((CELL)blob_tag, bytes, &pt);
  if (t == TermNil)
    return 0L;
  pt = RepAppl(t);
  blob_tag = pt[1];
  if (blob_tag < USER_BLOB_START || blob_tag >= USER_BLOB_END) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, AbsAppl(pt),
              "clean opaque: bad blob with tag " UInt_FORMAT, blob_tag);
    return FALSE;
  }
  YAP_opaque_tag_t blob_info = blob_tag;
  if (GLOBAL_OpaqueHandlers[blob_info].cut_handler ||
      GLOBAL_OpaqueHandlers[blob_info].fail_handler) {
    *HR++ = t;
    *HR++ = TermNil;
    TrailTerm(TR) = AbsPair(HR - 2);
  }
  return t;
}

X_API YAP_Bool YAP_IsOpaqueObjectTerm(Term t, YAP_opaque_tag_t tag) {
  return IsExternalBlobTerm(t, (CELL)tag);
}

X_API void *YAP_OpaqueObjectFromTerm(Term t) { return ExternalBlobFromTerm(t); }

X_API CELL *YAP_HeapStoreOpaqueTerm(Term t) {
  return Yap_HeapStoreOpaqueTerm(t);
}

X_API Int YAP_RunGoalOnce(Term t) {
  CACHE_REGS
  Term out;
  yamop *old_CP = CP, *old_P = P;
  Int oldPrologMode = LOCAL_PrologMode;
  yhandle_t CSlot;

  BACKUP_MACHINE_REGS();
  int lvl = push_text_stack();
  CSlot = Yap_StartSlots();
  LOCAL_PrologMode = UserMode;
  //  Yap_heap_regs->yap_do_low_level_trace=true;
  out = Yap_RunTopGoal(t, true);
  LOCAL_PrologMode = oldPrologMode;
  //  Yap_CloseSlots(CSlot);
  if (!(oldPrologMode & UserCCallMode)) {
    /* called from top-level */
  pop_text_stack( lvl);
    LOCAL_AllowRestart = FALSE;
    RECOVER_MACHINE_REGS();
    return out;
  }
  // should we catch the exception or pass it through?
  // We'll pass it through
  // Yap_RaiseException();
  if (out) {
    choiceptr cut_pt, ob;

    ob = NULL;
    cut_pt = B;
    while (cut_pt->cp_ap != NOCODE) {
      /* make sure we prune C-choicepoints */
      if (POP_CHOICE_POINT(cut_pt->cp_b)) {
        POP_EXECUTE();
      }
      ob = cut_pt;
      cut_pt = cut_pt->cp_b;
    }
#ifdef YAPOR
    CUT_prune_to(cut_pt);
#endif
    if (ob) {
      B = ob;
      Yap_TrimTrail();
    }
    B = cut_pt;
  } else {
    Yap_CloseSlots(CSlot);
  }
  ASP = B->cp_env;
  ENV = (CELL *)ASP[E_E];
  B = (choiceptr)ASP[E_CB];
#ifdef DEPTH_LIMIT
  DEPTH = ASP[E_DEPTH];
#endif
 P = old_P;
  CP = old_CP;
  LOCAL_AllowRestart = FALSE;
  RECOVER_MACHINE_REGS();
  pop_text_stack( lvl);
  return out;
}

X_API bool YAP_RestartGoal(void) {
  CACHE_REGS
  BACKUP_MACHINE_REGS();
  bool out;
  if (LOCAL_AllowRestart) {
    P = (yamop *)FAILCODE;
    LOCAL_PrologMode = UserMode;
    out = Yap_exec_absmi(TRUE, YAP_EXEC_ABSMI);
    LOCAL_PrologMode = UserCCallMode;
    if (out == FALSE) {
      /* cleanup */
      Yap_trust_last();
      LOCAL_AllowRestart = FALSE;
    }
  } else {
    out = FALSE;
  }
  RECOVER_MACHINE_REGS();
  return (out);
}

X_API bool YAP_ShutdownGoal(int backtrack) {
  CACHE_REGS
  BACKUP_MACHINE_REGS();

  if (LOCAL_AllowRestart) {
    choiceptr cut_pt;

    cut_pt = B;
    while (cut_pt->cp_ap != NOCODE) {
      /* make sure we prune C-choicepoints */
      if (POP_CHOICE_POINT(cut_pt->cp_b)) {
        POP_EXECUTE();
      }
      cut_pt = cut_pt->cp_b;
    }
#ifdef YAPOR
    CUT_prune_to(cut_pt);
#endif
    /* just force backtrack */
    B = cut_pt;
    if (backtrack) {
      P = FAILCODE;
      Yap_exec_absmi(TRUE, YAP_EXEC_ABSMI);
      /* recover stack space */
      HR = cut_pt->cp_h;
      TR = cut_pt->cp_tr;
    }
    /* we can always recover the stack */
    ASP = cut_pt->cp_env;
    ENV = (CELL *)ASP[E_E];
    B = (choiceptr)ASP[E_CB];
    Yap_TrimTrail();
#ifdef DEPTH_LIMIT
    DEPTH = ASP[E_DEPTH];
#endif
    LOCAL_AllowRestart = FALSE;
  }
  RECOVER_MACHINE_REGS();
  return TRUE;
}

X_API bool YAP_ContinueGoal(void) {
  CACHE_REGS
  bool out;
  BACKUP_MACHINE_REGS();

  LOCAL_PrologMode = UserMode;
  out = Yap_exec_absmi(TRUE, YAP_EXEC_ABSMI);
  LOCAL_PrologMode = UserCCallMode;

  RECOVER_MACHINE_REGS();
  return (out);
}

X_API void YAP_PruneGoal(YAP_dogoalinfo *gi) {
  CACHE_REGS
  BACKUP_B();

  choiceptr myB = (choiceptr)(LCL0 - gi->b_entry);
  while (B != myB) {
    /* make sure we prune C-choicepoints */
    if (POP_CHOICE_POINT(B->cp_b)) {
      POP_EXECUTE();
    }
    if (!B->cp_b)
      break;
    B = B->cp_b;
  }

  Yap_TrimTrail();

  RECOVER_B();
}

X_API bool YAP_GoalHasException(Term *t) {
  CACHE_REGS
  BACKUP_MACHINE_REGS();
  return LOCAL_ActiveError->errorNo != YAP_NO_ERROR;
}

X_API void YAP_ClearExceptions(void) {
  CACHE_REGS

  Yap_ResetException(worker_id);
}

X_API int YAP_InitConsult(int mode, const char *fname, char **full,
                          int *osnop) {
    CACHE_REGS

int sno;
int   lvl = push_text_stack();
    BACKUP_MACHINE_REGS();
    const char *fl = NULL;
    if (mode == YAP_BOOT_MODE) {
        mode = YAP_CONSULT_MODE;
    }
    if (fname == NULL || fname[0] == '\0') {
        fl = Yap_SOURCEBOOT;
    }
    if (!fname || !(fl = Yap_AbsoluteFile(fname, true)) || !fl[0]) {
            __android_log_print(
                    ANDROID_LOG_INFO, "YAPDroid", "failed ABSOLUTEFN %s ", fl);
            *full = NULL;
          return -1;
  }
    __android_log_print(
            ANDROID_LOG_INFO, "YAPDroid", "done init_ consult %s ",fl);
  char *d = Malloc(strlen(fl) + 1);
  strcpy(d, fl);
  bool consulted = (mode == YAP_CONSULT_MODE);
  Term tat = MkAtomTerm(Yap_LookupAtom(d));
  sno = Yap_OpenStream(tat, "r", MkAtomTerm(Yap_LookupAtom(fname)),
                       LOCAL_encoding);
    __android_log_print(
            ANDROID_LOG_INFO, "YAPDroid", "OpenStream got %d ",sno);
    if (sno < 0 || !Yap_ChDir(dirname((char *)d))) {
    *full = NULL;
    pop_text_stack(lvl);
    return -1;
  }
  LOCAL_PrologMode = UserMode;
*full = pop_output_text_stack__(lvl, fl);
  Yap_init_consult(consulted,*full);
  RECOVER_MACHINE_REGS();
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return sno;
}

/// given a stream descriptor or stream alias (see open/3),
/// return YAP's internal handle.
X_API void *YAP_GetStreamFromId(int no) { return GLOBAL_Stream + no; }

X_API FILE *YAP_TermToStream(Term t) {
  BACKUP_MACHINE_REGS();
  FILE *s;

  if (IsVarTerm(t) || !IsAtomTerm(t))
    return NULL;
  if ((s = Yap_GetStreamHandle(t)->file)) {
    RECOVER_MACHINE_REGS();
    return s;
  }
  RECOVER_MACHINE_REGS();
  return NULL;
}

X_API void YAP_EndConsult(int sno, int *osnop, const char *full) {
  BACKUP_MACHINE_REGS();
  Yap_CloseStream(sno);
  int lvl = push_text_stack();
  char *d = Malloc(strlen(full) + 1);
  strcpy(d, full);
  Yap_ChDir(dirname(d));
  if (osnop >= 0)
    Yap_AddAlias(AtomLoopStream, *osnop);
  Yap_end_consult();
  __android_log_print(ANDROID_LOG_INFO, "YAPDroid ", " closing %s:%s(%d), %d",
                      CurrentModule == 0
                          ? "prolog"
                          : RepAtom(AtomOfTerm(CurrentModule))->StrOfAE,
                      full, *osnop, sno);
  // LOCAL_CurSlot);
  pop_text_stack(lvl);
  RECOVER_MACHINE_REGS();
}

X_API Term YAP_Read(FILE *f) {
  Term o;
  int sno = Yap_FileStream(f, NULL, TermNil, Input_Stream_f, NULL);

  BACKUP_MACHINE_REGS();
  o = Yap_read_term(sno, TermNil, 1);
  Yap_ReleaseStream(sno);
  RECOVER_MACHINE_REGS();
  return o;
}

X_API Term YAP_ReadFromStream(int sno) {
  Term o;

  BACKUP_MACHINE_REGS();
  
  sigjmp_buf signew;
  if (sigsetjmp(signew, 0)) {
    Yap_syntax_error(LOCAL_toktide, sno, "ReadFromStream");
  RECOVER_MACHINE_REGS();
  return 0;
  } else { 
  o = Yap_read_term(sno, TermNil, false);
  }
  RECOVER_MACHINE_REGS();
  return o;
}

X_API Term YAP_ReadClauseFromStream(int sno, Term vs, Term pos) {

  BACKUP_MACHINE_REGS();
  Term t = Yap_read_term(
      sno,
      MkPairTerm(
		 Yap_MkApplTerm(Yap_MkFunctor(AtomVariableNames, 1), 1, &vs),
                 MkPairTerm(
			    Yap_MkApplTerm(Yap_MkFunctor(AtomTermPosition, 1),
                                           1, &pos),
                            TermNil)),
      true);
  RECOVER_MACHINE_REGS();
  return t;
}

X_API void YAP_Write(Term t, FILE *f, int flags) {
  BACKUP_MACHINE_REGS();
  int sno = Yap_FileStream(f, NULL, TermNil, Output_Stream_f, NULL);

  Yap_plwrite(t, GLOBAL_Stream + sno, 0, flags, NULL);
  Yap_ReleaseStream(sno);

  RECOVER_MACHINE_REGS();
}

X_API YAP_Term YAP_CopyTerm(Term t) {
  Term tn;
  BACKUP_MACHINE_REGS();

  tn = Yap_CopyTerm(t);

  RECOVER_MACHINE_REGS();

  return (tn);
}

X_API char *YAP_WriteBuffer(Term t, char *buf, size_t sze, int flags) {
  CACHE_REGS
  seq_tv_t inp, out;

  BACKUP_MACHINE_REGS();
  int l = push_text_stack();
  inp.val.t = t;
  inp.type = YAP_STRING_TERM | YAP_STRING_DATUM;
  out.type = YAP_STRING_CHARS;
  out.val.c = NULL;
  out.max = sze - 1;
  out.enc = LOCAL_encoding;
  if (!Yap_CVT_Text(&inp, &out PASS_REGS)) {
    RECOVER_MACHINE_REGS();
    pop_text_stack(l);
    return NULL;
  } else {
    RECOVER_MACHINE_REGS();
    if (buf == out.val.c) {
    pop_text_stack(l);
      return buf;
    } else {
        if ( strlen(out.val.c ) < sze) {
        strcpy( buf, out.val.c);
        pop_text_stack(l);
        return    buf;
       }
    }
  }
  return out.val.c = pop_output_text_stack(l,buf);
}

/// write a a term to n user-provided buffer: make sure not tp
/// overflow the buffer even if the text is much larger.
X_API int YAP_WriteDynamicBuffer(YAP_Term t, char *buf, size_t sze,
                                 size_t *lengthp, encoding_t enc, int flags) {
  char *b;

  BACKUP_MACHINE_REGS();
  b = Yap_TermToBuffer(t, flags);
  strncpy(buf, b, sze - 1);
  buf[sze] = 0;
  RECOVER_MACHINE_REGS();
  return true;
}

X_API bool YAP_CompileClause(Term t) {
  CACHE_REGS
  yamop *codeaddr;
  Term mod = CurrentModule;
  Term tn = TermNil;
  bool ok = true;

  BACKUP_MACHINE_REGS();

  /* allow expansion during stack initialization */
  LOCAL_ErrorMessage = NULL;
  ARG1 = t;
  YAPEnterCriticalSection();
  codeaddr = Yap_cclause(t, 0, mod, t);
  ok = (codeaddr != NULL);
  if (ok) {
    t = Deref(ARG1); /* just in case there was an heap overflow */
    if (!Yap_addclause(t, codeaddr, TermAssertz, mod, &tn)) {
      ok = false;
    }
  } else {
    ok = false;
  }
  YAPLeaveCriticalSection();

  if (Yap_get_signal(YAP_CDOVF_SIGNAL)) {
    if (!Yap_locked_growheap(FALSE, 0, NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "YAP failed to grow heap: %s",
                LOCAL_ErrorMessage);
      ok = false;
    }
  }
  RECOVER_MACHINE_REGS();
  if (!ok) {
    return NULL;
  }
  return ok;
}

X_API void YAP_PutValue(YAP_Atom at, Term t) { Yap_PutValue(at, t); }

X_API Term YAP_GetValue(YAP_Atom at) { return (Yap_GetValue(at)); }

X_API int YAP_CompareTerms(Term t1, Term t2) {
  return Yap_compare_terms(t1, t2);
}

X_API int YAP_Reset(yap_reset_t mode, bool reset_global) {
  int res = TRUE;
  BACKUP_MACHINE_REGS();
  res = Yap_Reset(mode, reset_global);
  RECOVER_MACHINE_REGS();
  return res;
}

X_API void YAP_Exit(int retval) { Yap_exit(retval); }

X_API int YAP_InitSocks(const char *host, long port) { return 0; }

X_API void YAP_SetOutputMessage(void) {
#if DEBUG
  Yap_output_msg = TRUE;
#endif
}

X_API int YAP_StreamToFileNo(Term t) { return (Yap_StreamToFileNo(t)); }

/**
 * Obtain a pointer to the YAP representation of a stream.
 * @param sno Stream Id
 * @return data structure for stream
 */
X_API void *YAP_RepStreamFromId(int sno) { return GLOBAL_Stream + sno; }

X_API void YAP_CloseAllOpenStreams(void) {
  BACKUP_H();

  Yap_CloseStreams();

  RECOVER_H();
}

X_API void YAP_FlushAllStreams(void) {
  BACKUP_H();

  // VSC??  Yap_FlushStreams();

  RECOVER_H();
}

X_API void YAP_Throw(Term t) {
  BACKUP_MACHINE_REGS();
  LOCAL_ActiveError->errorNo = THROW_EVENT;
  LOCAL_ActiveError->errorGoal = Yap_TermToBuffer(t, 0);
  Yap_JumpToEnv();
  RECOVER_MACHINE_REGS();
}

X_API void YAP_AsyncThrow(Term t) {
  CACHE_REGS
  BACKUP_MACHINE_REGS();
  LOCAL_PrologMode |= AsyncIntMode;
  LOCAL_ActiveError->errorNo = THROW_EVENT;
  LOCAL_ActiveError->errorGoal = Yap_TermToBuffer(t, 0);
  Yap_JumpToEnv();
  LOCAL_PrologMode &= ~AsyncIntMode;
  RECOVER_MACHINE_REGS();
}

X_API void YAP_Halt(int i) { Yap_exit(i); }

X_API CELL *YAP_TopOfLocalStack(void) {
  CACHE_REGS
  return (ASP);
}

X_API void *YAP_Predicate(YAP_Atom a, UInt arity, Term m) {
  if (arity == 0) {
    return ((void *)RepPredProp(PredPropByAtom(a, m)));
  } else {
    Functor f = Yap_MkFunctor(a, arity);
    return ((void *)RepPredProp(PredPropByFunc(f, m)));
  }
}

X_API void YAP_PredicateInfo(void *p, YAP_Atom *a, UInt *arity, Term *m) {
  PredEntry *pd = (PredEntry *)p;
  if (pd->ArityOfPE) {
    *arity = pd->ArityOfPE;
    *a = NameOfFunctor(pd->FunctorOfPred);
  } else {
    *arity = 0;
    *a = (Atom)(pd->FunctorOfPred);
  }
  if (pd->ModuleOfPred)
    *m = pd->ModuleOfPred;
  else
    *m = TermProlog;
}

X_API void YAP_UserCPredicate(const char *name, YAP_UserCPred def,
                              YAP_Arity arity) {
  Yap_InitCPred(name, arity, (CPredicate)def, UserCPredFlag);
}

X_API void YAP_UserBackCPredicate_(const char *name, YAP_UserCPred init,
                                   YAP_UserCPred cont, YAP_Arity arity,
                                   YAP_Arity extra) {
  Yap_InitCPredBackCut(name, arity, extra, (CPredicate)init, (CPredicate)cont,
                       NULL, UserCPredFlag);
}

X_API void YAP_UserBackCutCPredicate(const char *name, YAP_UserCPred init,
                                     YAP_UserCPred cont, YAP_UserCPred cut,
                                     YAP_Arity arity, YAP_Arity extra) {
  Yap_InitCPredBackCut(name, arity, extra, (CPredicate)init, (CPredicate)cont,
                       (CPredicate)cut, UserCPredFlag);
}

X_API void YAP_UserBackCPredicate(const char *name, YAP_UserCPred init,
                                  YAP_UserCPred cont, arity_t arity,
                                  arity_t extra) {
  Yap_InitCPredBackCut(name, arity, extra, (CPredicate)init, (CPredicate)cont,
                       NULL, UserCPredFlag);
}

X_API void YAP_UserCPredicateWithArgs(const char *a, YAP_UserCPred f,
                                      arity_t arity, Term mod) {
  CACHE_REGS
  Term cm = CurrentModule;
  CurrentModule = mod;
  Yap_InitCPred(a, arity, (CPredicate)f, UserCPredFlag | CArgsPredFlag);
  CurrentModule = cm;
}

X_API Term YAP_CurrentModule(void) {
  CACHE_REGS
  return (CurrentModule);
}

X_API Term YAP_SetCurrentModule(Term new) {
  CACHE_REGS
  Term omod = CurrentModule;
  LOCAL_SourceModule = CurrentModule = new;
  return omod;
}

X_API Term YAP_CreateModule(YAP_Atom at) {
  Term t;
  WRITE_LOCK(RepAtom(at)->ARWLock);
  t = Yap_Module(MkAtomTerm(at));
  WRITE_UNLOCK(RepAtom(at)->ARWLock);
  return t;
}

X_API Term YAP_StripModule(Term t, Term *modp) {
  return Yap_StripModule(t, modp);
}

X_API int YAP_ThreadSelf(void) {
#if THREADS
  return Yap_thread_self();
#else
  return -2;
#endif
}

X_API int YAP_ThreadCreateEngine(struct YAP_thread_attr_struct *attr) {
#if THREADS
  return Yap_thread_create_engine(attr);
#else
  return -1;
#endif
}

X_API int YAP_ThreadAttachEngine(int wid) {
#if THREADS
  return Yap_thread_attach_engine(wid);
#else
  return FALSE;
#endif
}

X_API int YAP_ThreadDetachEngine(int wid) {
#if THREADS
  return Yap_thread_detach_engine(wid);
#else
  return FALSE;
#endif
}

X_API int YAP_ThreadDestroyEngine(int wid) {
#if THREADS
  return Yap_thread_destroy_engine(wid);
#else
  return FALSE;
#endif
}

X_API Term YAP_TermNil(void) { return TermNil; }

X_API int YAP_IsTermNil(Term t) { return t == TermNil; }

X_API int YAP_AtomGetHold(YAP_Atom at) { return Yap_AtomIncreaseHold(at); }

X_API int YAP_AtomReleaseHold(YAP_Atom at) { return Yap_AtomDecreaseHold(at); }

X_API YAP_agc_hook YAP_AGCRegisterHook(YAP_agc_hook hook) {
  YAP_agc_hook old = (YAP_agc_hook)GLOBAL_AGCHook;
  GLOBAL_AGCHook = (Agc_hook)hook;
  return old;
}

X_API int YAP_HaltRegisterHook(HaltHookFunc hook, void *closure) {
  return Yap_HaltRegisterHook(hook, closure);
}

X_API char *YAP_cwd(void) {
  CACHE_REGS
  char *buf = Yap_AllocCodeSpace(FILENAME_MAX + 1);
  int len;
  if (!Yap_getcwd(buf, FILENAME_MAX))
    return FALSE;
  len = strlen(buf);
  buf = Yap_ReallocCodeSpace(buf, len + 1);
  return buf;
}

X_API Term YAP_FloatsToList(double *dblp, size_t sz) {
  CACHE_REGS
  Term t;
  CELL *oldH;
  BACKUP_H();

  if (!sz)
    return TermNil;
  while (ASP - 1024 < HR + sz * (2 + 2 + SIZEOF_DOUBLE / SIZEOF_INT_P)) {
    if ((CELL *)dblp > H0 && (CELL *)dblp < HR) {
      /* we are in trouble */
      LOCAL_OpenArray = (CELL *)dblp;
    }
    if (!Yap_dogc(0, NULL PASS_REGS)) {
      RECOVER_H();
      return 0L;
    }
    dblp = (double *)LOCAL_OpenArray;
    LOCAL_OpenArray = NULL;
  }
  t = AbsPair(HR);
  while (sz) {
    oldH = HR;
    HR += 2;
    oldH[0] = MkFloatTerm(*dblp++);
    oldH[1] = AbsPair(HR);
    sz--;
  }
  oldH[1] = TermNil;
  RECOVER_H();
  return t;
}

X_API Int YAP_ListToFloats(Term t, double *dblp, size_t sz) {
  size_t i = 0;

  t = Deref(t);
  do {
    Term hd;
    if (IsVarTerm(t))
      return -1;
    if (t == TermNil)
      return i;
    if (!IsPairTerm(t))
      return -1;
    hd = HeadOfTerm(t);
    if (IsFloatTerm(hd)) {
      dblp[i++] = FloatOfTerm(hd);
    } else {
      extern double Yap_gmp_to_float(Term hd);

      if (IsIntTerm(hd))
        dblp[i++] = IntOfTerm(hd);
      else if (IsLongIntTerm(hd))
        dblp[i++] = LongIntOfTerm(hd);
#if USE_GMP
      else if (IsBigIntTerm(hd))
        dblp[i++] = Yap_gmp_to_float(hd);
#endif
      else
        return -1;
    }
    if (i == sz)
      return sz;
    t = TailOfTerm(t);
  } while (TRUE);
}

X_API Term YAP_IntsToList(Int *dblp, size_t sz) {
  CACHE_REGS
  Term t;
  CELL *oldH;
  BACKUP_H();

  if (!sz)
    return TermNil;
  while (ASP - 1024 < HR + sz * 3) {
    if ((CELL *)dblp > H0 && (CELL *)dblp < HR) {
      /* we are in trouble */
      LOCAL_OpenArray = (CELL *)dblp;
    }
    if (!Yap_dogc(0, NULL PASS_REGS)) {
      RECOVER_H();
      return 0L;
    }
    dblp = (Int *)LOCAL_OpenArray;
    LOCAL_OpenArray = NULL;
  }
  t = AbsPair(HR);
  while (sz) {
    oldH = HR;
    HR += 2;
    oldH[0] = MkIntegerTerm(*dblp++);
    oldH[1] = AbsPair(HR);
    sz--;
  }
  oldH[1] = TermNil;
  RECOVER_H();
  return t;
}

X_API Int YAP_ListToInts(Term t, Int *dblp, size_t sz) {
  size_t i = 0;

  t = Deref(t);
  do {
    Term hd;
    if (IsVarTerm(t))
      return -1;
    if (t == TermNil)
      return i;
    if (!IsPairTerm(t))
      return -1;
    hd = HeadOfTerm(t);
    if (!IsIntTerm(hd))
      return -1;
    dblp[i++] = IntOfTerm(hd);
    if (i == sz)
      return sz;
    t = TailOfTerm(t);
  } while (TRUE);
}

X_API Term YAP_OpenList(int n) {
  CACHE_REGS
  Term t;
  BACKUP_H();

  while (HR + 2 * n > ASP - 1024) {
    if (!Yap_dogc(0, NULL PASS_REGS)) {
      RECOVER_H();
      return FALSE;
    }
  }
  t = AbsPair(HR);
  HR += 2 * n;

  RECOVER_H();
  return t;
}

X_API Term YAP_ExtendList(Term t0, Term inp) {
  Term t;
  CELL *ptr = RepPair(t0);
  BACKUP_H();

  ptr[0] = inp;
  ptr[1] = AbsPair(ptr + 2);
  t = AbsPair(ptr + 2);

  RECOVER_H();
  return t;
}

X_API int YAP_CloseList(Term t0, Term tail) {
  CELL *ptr = RepPair(t0);

  RESET_VARIABLE(ptr - 1);
  if (!Yap_unify((Term)(ptr - 1), tail))
    return FALSE;
  return TRUE;
}

X_API int YAP_IsAttVar(Term t) {
  CACHE_REGS
  t = Deref(t);
  if (!IsVarTerm(t))
    return FALSE;
  return IsAttVar(VarOfTerm(t));
}

X_API Term YAP_AttsOfVar(Term t) {
  CACHE_REGS
  attvar_record *attv;

  t = Deref(t);
  if (!IsVarTerm(t))
    return TermNil;
  if (!IsAttVar(VarOfTerm(t)))
    return TermNil;
  attv = RepAttVar(VarOfTerm(t));
  return attv->Atts;
}

X_API int YAP_FileNoFromStream(Term t) {

  t = Deref(t);
  if (IsVarTerm(t))
    return -1;
  return Yap_StreamToFileNo(t);
}

X_API void *YAP_FileDescriptorFromStream(Term t) {

  t = Deref(t);
  if (IsVarTerm(t))
    return NULL;
  return Yap_FileDescriptorFromStream(t);
}

X_API void *YAP_Record(Term t) {
  DBTerm *dbterm;
  DBRecordList *dbt;

  dbterm = Yap_StoreTermInDB(Deref(t), 0);
  if (dbterm == NULL)
    return NULL;
  dbt = (struct record_list *)Yap_AllocCodeSpace(sizeof(struct record_list));
  while (dbt == NULL) {
    if (!Yap_growheap(FALSE, sizeof(struct record_list), NULL)) {
      /* be a good neighbor */
      Yap_FreeCodeSpace((void *)dbterm);
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "using YAP_Record");
      return NULL;
    }
  }
  if (Yap_Records) {
    Yap_Records->prev_rec = dbt;
  }
  dbt->next_rec = Yap_Records;
  dbt->prev_rec = NULL;
  dbt->dbrecord = dbterm;
  Yap_Records = dbt;
  return dbt;
}

X_API Term YAP_Recorded(void *handle) {
  CACHE_REGS
  Term t;
  DBTerm *dbterm = ((DBRecordList *)handle)->dbrecord;

  BACKUP_MACHINE_REGS();
  do {
    LOCAL_Error_TYPE = YAP_NO_ERROR;
    t = Yap_FetchTermFromDB(dbterm);
    if (LOCAL_Error_TYPE == YAP_NO_ERROR) {
      RECOVER_MACHINE_REGS();
      return t;
    } else if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      if (!Yap_growglobal(NULL)) {
        Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                  LOCAL_ErrorMessage);
        RECOVER_MACHINE_REGS();
        return FALSE;
      }
    } else {
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      if (!Yap_growstack(dbterm->NOfCells * CellSize)) {
        Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
        RECOVER_MACHINE_REGS();
        return FALSE;
      }
    }
  } while (t == (CELL)0);
  RECOVER_MACHINE_REGS();
  return t;
}

X_API int YAP_Erase(void *handle) {
  DBRecordList *dbr = (DBRecordList *)handle;
  if (dbr->next_rec)
    dbr->next_rec->prev_rec = dbr->prev_rec;
  if (dbr->prev_rec)
    dbr->prev_rec->next_rec = dbr->next_rec;
  else if (Yap_Records == dbr) {
    Yap_Records = dbr->next_rec;
  }
  Yap_ReleaseTermFromDB(dbr->dbrecord);
  Yap_FreeCodeSpace(handle);
  return 1;
}

X_API yhandle_t YAP_ArgsToSlots(int n) {
  CACHE_REGS
  return Yap_NewSlots(n);
}

X_API void YAP_SlotsToArgs(int n, yhandle_t slot) {
  CACHE_REGS
  CELL *ptr0 = Yap_AddressFromSlot(slot), *ptr1 = &ARG1;
  while (n--) {
    *ptr1++ = *ptr0++;
  }
}

X_API void YAP_signal(int sig) { Yap_signal(sig); }

X_API int YAP_SetYAPFlag(Term flag, Term val) { return Yap_set_flag(flag, val); }

/*    yhandle_t  YAP_VarSlotToNumber(yhandle_t)  */
X_API yhandle_t YAP_VarSlotToNumber(yhandle_t s) {
  CACHE_REGS
  Term *t = (CELL *)Deref(Yap_GetFromSlot(s));
  if (t < HR)
    return t - H0;
  return t - LCL0;
}

/*    Term  YAP_ModuleUser()  */
X_API Term YAP_ModuleUser(void) { return MkAtomTerm(AtomUser); }

/*    int  YAP_PredicateHasClauses()  */
X_API YAP_handle_t YAP_NumberOfClausesForPredicate(YAP_PredEntryPtr ape) {
  PredEntry *pe = ape;
  return pe->NOfClauses;
}

X_API int YAP_MaxOpPriority(YAP_Atom at, Term module) {
  AtomEntry *ae = RepAtom(at);
  OpEntry *info;
  WRITE_LOCK(ae->ARWLock);
  info = Yap_GetOpPropForAModuleHavingALock(ae, module);
  if (!info) {
    WRITE_UNLOCK(ae->ARWLock);
    return 0;
  }
  int ret = info->Prefix;
  if (info->Infix > ret)
    ret = info->Infix;
  if (info->Posfix > ret)
    ret = info->Posfix;
  WRITE_UNLOCK(ae->ARWLock);
  return ret;
}

X_API int YAP_OpInfo(YAP_Atom at, Term module, int opkind, int *yap_type,
                     int *prio) {
  AtomEntry *ae = RepAtom(at);
  OpEntry *info;
  int n;

  WRITE_LOCK(ae->ARWLock);
  info = Yap_GetOpPropForAModuleHavingALock(ae, module);
  if (!info) {
    /* try system operators */
    info = Yap_GetOpPropForAModuleHavingALock(ae, PROLOG_MODULE);
    if (!info) {
      WRITE_UNLOCK(ae->ARWLock);
      return 0;
    }
  }
  if (opkind == PREFIX_OP) {
    SMALLUNSGN p = info->Prefix;
    if (!p) {
      WRITE_UNLOCK(ae->ARWLock);
      return FALSE;
    }
    if (p & DcrrpFlag) {
      n = 6;
      *prio = (p ^ DcrrpFlag);
    } else {
      n = 7;
      *prio = p;
    }
  } else if (opkind == INFIX_OP) {
    SMALLUNSGN p = info->Infix;
    if (!p) {
      WRITE_UNLOCK(ae->ARWLock);
      return FALSE;
    }
    if ((p & DcrrpFlag) && (p & DcrlpFlag)) {
      n = 1;
      *prio = (p ^ (DcrrpFlag | DcrlpFlag));
    } else if (p & DcrrpFlag) {
      n = 3;
      *prio = (p ^ DcrrpFlag);
    } else if (p & DcrlpFlag) {
      n = 2;
      *prio = (p ^ DcrlpFlag);
    } else {
      n = 4;
      *prio = p;
    }
  } else {
    SMALLUNSGN p = info->Posfix;
    if (p & DcrlpFlag) {
      n = 4;
      *prio = (p ^ DcrlpFlag);
    } else {
      n = 5;
      *prio = p;
    }
  }
  *yap_type = n;
  WRITE_UNLOCK(ae->ARWLock);
  return 1;
}

X_API int YAP_Argv(char ***argvp) {
  if (argvp) {
    *argvp = GLOBAL_argv;
  }
  return GLOBAL_argc;
}

X_API YAP_tag_t YAP_TagOfTerm(Term t) {
  if (IsVarTerm(t)) {
    CELL *pt = VarOfTerm(t);
    if (IsUnboundVar(pt)) {
      CACHE_REGS
      if (IsAttVar(pt))
        return YAP_TAG_ATT;
      return YAP_TAG_UNBOUND;
    }
    return YAP_TAG_REF;
  }
  if (IsPairTerm(t))
    return YAP_TAG_PAIR;
  if (IsAtomOrIntTerm(t)) {
    if (IsAtomTerm(t))
      return YAP_TAG_ATOM;
    return YAP_TAG_INT;
  } else {
    Functor f = FunctorOfTerm(t);

    if (IsExtensionFunctor(f)) {
      if (f == FunctorDBRef) {
        return YAP_TAG_DBREF;
      }
      if (f == FunctorLongInt) {
        return YAP_TAG_LONG_INT;
      }
      if (f == FunctorBigInt) {
        big_blob_type bt = RepAppl(t)[1];
        switch (bt) {
        case BIG_INT:
          return YAP_TAG_BIG_INT;
        case BIG_RATIONAL:
          return YAP_TAG_RATIONAL;
        default:
          return YAP_TAG_OPAQUE;
        }
      }
    }
    return YAP_TAG_APPL;
  }
}

int YAP_BPROLOG_exception;
Term YAP_BPROLOG_curr_toam_status;

/**
 * Output the number of bytes needed to represent a string in UTF-8
 * Note that the terminating zero is not included. No error checking
 * is performed (the programmer should have that done).
 *
 * @param t a list of codes, chars, string or atom.
 *
 * @return a positive number with the size, or 0.
 */
X_API size_t YAP_UTF8_TextLength(Term t) {
  utf8proc_uint8_t dst[8];
  size_t sz = 0;

  if (IsPairTerm(t)) {
    while (t != TermNil) {
      int c;

      Term hd = HeadOfTerm(t);
      if (IsAtomTerm(hd)) {
        Atom at = AtomOfTerm(hd);
        unsigned char *s = RepAtom(at)->UStrOfAE;
        int32_t ch;
        get_utf8(s, 1, &ch);
        c = ch;
      } else if (IsIntegerTerm(hd)) {
        c = IntegerOfTerm(hd);
      } else {
        c = '\0';
      }

      sz += utf8proc_encode_char(c, dst);
      t = TailOfTerm(t);
    }
  } else if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    char *s = RepAtom(at)->StrOfAE;
    sz = strlen(s);
  } else if (IsStringTerm(t)) {
    sz = strlen(StringOfTerm(t));
  }
  return sz;
}

X_API Int YAP_ListLength(Term t) {
  Term *aux;

  Int n = Yap_SkipList(&t, &aux);
  if (IsVarTerm(*aux))
    return -1;
  if (*aux == TermNil)
    return n;
  return -1;
}

X_API Int YAP_NumberVars(Term t, Int nbv) {
  return Yap_NumberVars(t, nbv, FALSE, NULL);
}

X_API Term YAP_UnNumberVars(Term t) {
  /* don't allow sharing of ground terms */
  return Yap_UnNumberTerm(t, FALSE);
}

X_API int YAP_IsNumberedVariable(Term t) {
  return IsApplTerm(t) && FunctorOfTerm(t) == FunctorDollarVar &&
         IsIntegerTerm(ArgOfTerm(1, t));
}

X_API size_t YAP_ExportTerm(Term inp, char *buf, size_t len) {
  if (!len)
    return 0;
  return Yap_ExportTerm(inp, buf, len, current_arity());
}

X_API size_t YAP_SizeOfExportedTerm(char *buf) {
  if (!buf)
    return 0;
  return Yap_SizeOfExportedTerm(buf);
}

X_API Term YAP_ImportTerm(char *buf) { return Yap_ImportTerm(buf); }

X_API int YAP_RequiresExtraStack(size_t sz) {
  CACHE_REGS

  if (sz < 16 * 1024)
    sz = 16 * 1024;
  if (HR <= ASP - sz) {
    return FALSE;
  }
  BACKUP_H();
  while (HR > ASP - sz) {
    CACHE_REGS
    RECOVER_H();
    if (!Yap_dogc(0, NULL PASS_REGS)) {
      return -1;
    }
    BACKUP_H();
  }
  RECOVER_H();
  return TRUE;
}

atom_t *TR_Atoms;
functor_t *TR_Functors;
size_t AtomTranslations, MaxAtomTranslations;
size_t FunctorTranslations, MaxFunctorTranslations;

X_API Int YAP_AtomToInt(YAP_Atom At) {
  TranslationEntry *te = Yap_GetTranslationProp(At, 0);
  if (te != NIL)
    return te->Translation;
  TR_Atoms[AtomTranslations] = At;
  Yap_PutAtomTranslation(At, 0, AtomTranslations);
  AtomTranslations++;
  if (AtomTranslations == MaxAtomTranslations) {
    atom_t *ot = TR_Atoms;
    atom_t *nt = (atom_t *)malloc(sizeof(atom_t) * 2 * MaxAtomTranslations);
    if (nt == NULL) {
      Yap_Error(SYSTEM_ERROR_INTERNAL, MkAtomTerm(At),
                "No more room for translations");
      return -1;
    }
    memmove(nt, ot, sizeof(atom_t) * MaxAtomTranslations);
    TR_Atoms = nt;
    free(ot);
    MaxAtomTranslations *= 2;
  }
  return AtomTranslations - 1;
}

X_API YAP_Atom YAP_IntToAtom(Int i) { return TR_Atoms[i]; }

X_API Int YAP_FunctorToInt(YAP_Functor f) {
  YAP_Atom At = NameOfFunctor(f);
  arity_t arity = ArityOfFunctor(f);
  TranslationEntry *te = Yap_GetTranslationProp(At, arity);
  if (te != NIL)
    return te->Translation;
  TR_Functors[FunctorTranslations] = f;
  Yap_PutAtomTranslation(At, arity, FunctorTranslations);
  FunctorTranslations++;
  if (FunctorTranslations == MaxFunctorTranslations) {
    functor_t *nt = (functor_t *)malloc(sizeof(functor_t) * 2 *
                                        MaxFunctorTranslations),
              *ot = TR_Functors;
    if (nt == NULL) {
      Yap_Error(SYSTEM_ERROR_INTERNAL, MkAtomTerm(At),
                "No more room for translations");
      return -1;
    }
    memmove(nt, ot, sizeof(functor_t) * MaxFunctorTranslations);
    TR_Functors = nt;
    free(ot);
    MaxFunctorTranslations *= 2;
  }
  return FunctorTranslations - 1;
}

X_API void *YAP_foreign_stream(int sno) {
  return GLOBAL_Stream[sno].u.private_data;
}

X_API YAP_Functor YAP_IntToFunctor(Int i) { return TR_Functors[i]; }

X_API void *YAP_shared(void) { scratch_struct_t scratch;
  Yap_get_scratch_buf(&scratch, 4096, 1);
  return scratch.data;
  }

X_API YAP_PredEntryPtr YAP_TopGoal(void) {
  Functor f = Yap_MkFunctor(Yap_LookupAtom("yap_query"), 3);
  Term tmod = MkAtomTerm(Yap_LookupAtom("yapi"));
  PredEntry *p = RepPredProp(Yap_GetPredPropByFunc(f, tmod));
  return p;
}

void yap_init(void) {}

#endif // C_INTERFACE_C

/**
@}
*/
