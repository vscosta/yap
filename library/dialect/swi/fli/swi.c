/* yap2swi.c  */
/*
 * Project: jpl for Yap Prolog
 * Author: Steve Moyle and Vitor Santos Costa
 * Email:  steve.moyle@comlab.ox.ac.uk
 * Date:   21 January 2002

 * Copyright (c) 2002-2014 Vitor Santos Costa from an original version by Steve Moyle.  All rights reserved.

*/

/**
 *
 *   @file swi.c
 *
 *   @addtogroup swi-c-interface
 *
 * @{
 */

#define PL_KERNEL 1

//=== includes ===============================================================
#include	<stdlib.h>
#include	<string.h>
#include	<stdio.h>
#include	<wchar.h>
#include	<assert.h>


#include	<Yap.h>
#include	<Yatom.h>
#include	<YapHeap.h>
#include	<eval.h>

#if HAVE_MATH_H
#include	<math.h>
#endif
#if HAVE_ERRNO_H
#include	<errno.h>
#endif
#if HAVE_SIGNAL_H
#include	<signal.h>
#endif

#if !HAVE_SNPRINTF
#define snprintf(X,Y,Z,A) sprintf(X,Z,A)
#endif

#define PL_KERNEL 1

#include	<pl-shared.h>

#include	<yapio.h>
#include	<YapText.h>

#ifdef USE_GMP
#include <gmp.h>
#endif

#ifdef __WIN32__
/* Windows */
#include <fcntl.h>
#endif

#include "swi.h"

#include "pl-error.h"

extern int		PL_unify_termv(term_t l, va_list args);

extern X_API Atom YAP_AtomFromSWIAtom(atom_t at);
extern X_API atom_t YAP_SWIAtomFromAtom(Atom at);

static int 
do_gc(UInt sz)
{
  /* always called from user_call_cpred */
  CACHE_REGS
  UInt arity;
  yamop *nextpc;

  if (P && PREVOP(P,Osbpp)->opc == Yap_opcode(_call_usercpred)) {
    arity = PREVOP(P,Osbpp)->y_u.Osbpp.p->ArityOfPE;
    nextpc = P;
  } else {
    arity = 0;
    nextpc = CP;
  }
  return Yap_gcl(sz, arity, ENV, nextpc);
}


X_API extern Atom
YAP_AtomFromSWIAtom(atom_t at)
{
  return SWIAtomToAtom(at);
}

X_API extern atom_t
YAP_SWIAtomFromAtom(Atom at)
{
  return AtomToSWIAtom(at);
}

extern X_API Int YAP_PLArityOfSWIFunctor(functor_t at);

/* This is silly, but let's keep it like that for now */
X_API Int
YAP_PLArityOfSWIFunctor(functor_t f) {
  if (((CELL)(f) & 2) && ((CELL)f) < N_SWI_FUNCTORS*4+2)
    return ArityOfFunctor(SWI_Functors[(CELL)f/4]);
  if (IsAtomTerm(f))
    return 0;
  return ArityOfFunctor((Functor)f);
}

void
Yap_InitSWIHash(void)
{
  int i, j;
  for (i=0; i < N_SWI_ATOMS; i++) {
    Yap_PutAtomTranslation( SWI_Atoms[i], i );
  }
  AtomTranslations = N_SWI_ATOMS;
  for (j=0; j < N_SWI_FUNCTORS; j++) {
    add_to_hash(j, (ADDR)SWI_Functors[j]);
  }
}

static void
UserCPredicate(char *a, CPredicate def, unsigned long int arity, Term mod, int flags)
{
  CACHE_REGS

  Term cm = CurrentModule;
  /* fprintf(stderr,"doing %s:%s/%d\n", RepAtom(AtomOfTerm(mod))->StrOfAE, a,arity); */
  CurrentModule = mod;
  Yap_InitCPred(a, arity, def, (UserCPredFlag|CArgsPredFlag|flags));
  CurrentModule = cm;
} 

/** @defgroup swi-ATOMS Atom Construction
 *  @ingroup swi-c-interface
 *  @{
 *  */


/* SWI: void PL_agc_hook(void) */
/** @brief Atom garbage collection hook
 *
 */
X_API PL_agc_hook_t
PL_agc_hook(PL_agc_hook_t entry)
{
  return (PL_agc_hook_t)YAP_AGCRegisterHook((YAP_agc_hook)entry);
}

/** @brief extract the text representation from atom
 *
 */
X_API char* PL_atom_chars(atom_t a)	 /* SAM check type */
{
  Atom at = SWIAtomToAtom(a);
  if (IsWideAtom(at))
    return NULL;
  return RepAtom(at)->StrOfAE;
}

/** @brief extract the text representation from atom, including its length
 *
 */
X_API char* PL_atom_nchars(atom_t a, size_t *len)	 /* SAM check type */
{
  char *s = RepAtom(SWIAtomToAtom(a))->StrOfAE;
  *len = strlen(s);
  return s;
}

/** @}
 *
 * @defgroup swi-term_references Term References
 *  @ingroup swi-c-interface
 *  @{
 *  */

/** @brief duplicate a term reference
 *
 */
X_API term_t PL_copy_term_ref(term_t from)
{
  CACHE_REGS
  return Yap_InitSlot(Yap_GetFromSlot(from PASS_REGS) PASS_REGS);
}

/** @brief create a new term reference
 *
 */
X_API term_t PL_new_term_ref(void)
{
  
  CACHE_REGS
  term_t to = Yap_NewSlots(1 PASS_REGS);
  return to;
}

/** @brief create several new term references
 *
 * @par n is the number of references
 */
X_API term_t PL_new_term_refs(int n)
{
  CACHE_REGS
  term_t to = Yap_NewSlots(n PASS_REGS);
  return to;
}

/** @brief dispose of all term references created since after
 *
 */
X_API void PL_reset_term_refs(term_t after)
{
  CACHE_REGS
  term_t new = Yap_NewSlots(1 PASS_REGS);
  Yap_RecoverSlots(after-new, new PASS_REGS);
}

/** @}
 *  @defgroup swi-term_manipulation Term Manipulation
 *  @ingroup swi-c-interface
 *  */

/**
 *  @defgroup swi-get-operations Reading Terms
 *  @ingroup swi-term_manipulation
 *  @{
 *  */

/** @brief *name is assigned the name and *arity the arity if term ts, or the operaton fails.
 *
 */
X_API int PL_get_name_arity(term_t ts, atom_t *name, int *arity)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if (IsAtomTerm(t)) {
    *name = AtomToSWIAtom(AtomOfTerm(t));
    *arity = 0;
    return 1;
  }
  if (YAP_IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    if (IsExtensionFunctor(f)) {
      return 0;
    }
    *name = AtomToSWIAtom(NameOfFunctor(f));
    *arity = ArityOfFunctor(f);
    return 1;
  }
  if (YAP_IsPairTerm(t)) {
    *name = AtomToSWIAtom(AtomDot);
    *arity = 2;
    return 1;
  }
  return 0;
}


/** @brief a is assigned the argument index from term  ts
 *
 */
X_API int PL_get_arg(int index, term_t ts, term_t a)
{
  CACHE_REGS
    YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if (IsVarTerm( t ))
    return 0;
  if ( !IsApplTerm(t) ) {
    if (IsPairTerm(t)) {
      if (index == 1){
        Yap_PutInSlot(a,HeadOfTerm(t) PASS_REGS);
        return 1;
      } else if (index == 2) {
        Yap_PutInSlot(a,TailOfTerm(t) PASS_REGS);
        return 1;
      }
    }
    return 0;
  } else {
    Functor f = FunctorOfTerm(t);
    if (IsExtensionFunctor(f))
      return 0;
    if (index < 1 || index > ArityOfFunctor(f))
      return 0;
    Yap_PutInSlot(a,ArgOfTerm(index, t) PASS_REGS);
    return 1;
  }
}

/** @brief *ap is assigned the name and *ip the arity from term  ts
 *
 */
X_API int PL_get_compound_name_arity(term_t ts, atom_t *ap, int *ip)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if ( !YAP_IsApplTerm(t) ) {
    if (YAP_IsPairTerm(t)) {
      if (ip)
	*ip = 2;
      if (ap)
	*ap = ATOM_nil;
      return 1;
    }
    return 0;
  } else {
    Functor f = FunctorOfTerm( t );
    if (IsExtensionFunctor(f)) return FALSE;
    if (ip)
      *ip = ArityOfFunctor( f );
    if (ap)
      *ap = AtomToSWIAtom( NameOfFunctor( f ));
    return 1;
  }
}


   
/** @brief *a is assigned the atom in term  ts, or the operation fails
 *
 */
X_API int PL_get_atom(term_t ts, atom_t *a)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if ( !IsAtomTerm(t))
    return 0;
  *a = AtomToSWIAtom(AtomOfTerm(t));
  return 1;
}

/** @brief *i is assigned the int in term  ts, or the operation fails
 *
 */
/* SWI: int PL_get_integer(term_t t, int *i)
   YAP: long int  YAP_IntOfTerm(Term) */
X_API int PL_get_integer(term_t ts, int *i)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if (IsVarTerm(t) || !IsIntegerTerm(t) )
    return 0;
  *i = (int)IntegerOfTerm(t);
  return 1;
}

/** @brief *i is assigned the boolean atom `true` or `false` in term  ts, or the operation fails
 *
 */
X_API int PL_get_long(term_t ts, long *i)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if (!YAP_IsIntTerm(t) ) {
    if (YAP_IsFloatTerm(t)) {
      double dbl = YAP_FloatOfTerm(t);
      if (dbl - (long)dbl == 0.0) {
	*i = (long)dbl;
	return 1;
      }
    }
    return 0;
  }
  *i = YAP_IntOfTerm(t);
  return 1;
}

/* SWI: int PL_get_bool(term_t t, int *i)
   YAP: long int  YAP_AtomOfTerm(Term) */
X_API int PL_get_bool(term_t ts, int *i)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  Atom at;

  if (!IsAtomTerm(t) )
    return 0;
  at = AtomOfTerm(t);
  if (at == AtomTrue) {
    *i = TRUE;
    return 1;
  }
  if (at == AtomFalse) {
    *i = FALSE;
    return 1;
  }
  return 0;
}

/** @brief *a is assigned the int64 in term  ts, or the operation fails
 *
 */
X_API int PL_get_int64(term_t ts, int64_t *i)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if (!YAP_IsIntTerm(t) ) {
    if (YAP_IsFloatTerm(t)) {
      double dbl = YAP_FloatOfTerm(t);
      if (dbl - (int64_t)dbl == 0.0) {
    	  *i = (int64_t)dbl;
    	  return	 1;
      }
#if SIZEOF_INT_P==4 && !USE_GMP
      {
      	union {
      		double d;
      		int64_t i;
      	} udbi_;
      	udbi_.d = YAP_FloatOfTerm(t);
      	*i = udbi_.i;
      	return 1;
      }
#endif
      return 0;
    }
#if USE_GMP
    else if (YAP_IsBigNumTerm(t)) {
      MP_INT g;
      char s[64];
      YAP_BigNumOfTerm(t, (void *)&g);
      if (mpz_sizeinbase(&g,2) > 64) {
	    return 0;
      }
      mpz_get_str (s, 10, &g);
#ifdef _WIN32
      sscanf(s, "%I64d", (long long int *)i);
#else
      sscanf(s, "%lld", (long long int *)i);
#endif
      return 1;
    }
#endif
    return 0;
  }
  *i = YAP_IntOfTerm(t);
  return 1;
}

/** @brief *a is assigned the intptr_t in term  ts, or the operation fails
 *
 */
X_API int PL_get_intptr(term_t ts, intptr_t *a)
{
  CACHE_REGS
  Term t = Yap_GetFromSlot(ts PASS_REGS);
  if ( !IsIntegerTerm(t) )
    return 0;
  *a = (intptr_t)(IntegerOfTerm(t));
  return 1;
}

/** @brief *a is assigned the uintptr_t in term  ts, or the operation fails
 *
 */
X_API int PL_get_uintptr(term_t ts, uintptr_t *a)
{
  CACHE_REGS
  Term t = Yap_GetFromSlot(ts PASS_REGS);
  if ( !IsIntegerTerm(t) )
    return 0;
  *a = (uintptr_t)(IntegerOfTerm(t));
  return 1;
}

/** @brief a is assigned the argument index from term  ts
 *
 */
X_API int _PL_get_arg(int index, term_t ts, term_t a)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if ( !YAP_IsApplTerm(t) ) {
    if (YAP_IsPairTerm(t)) {
      if (index == 1){
	Yap_PutInSlot(a,HeadOfTerm(t) PASS_REGS);
	return 1;
      } else if (index == 2) {
	Yap_PutInSlot(a,TailOfTerm(t) PASS_REGS);
	return 1;
      }
    }
    return 0;
  }
  Yap_PutInSlot(a,ArgOfTerm(index, t) PASS_REGS);
  return 1;
}


/** @brief *a is assigned the string representation of the atom in term  ts, or the operation fails
 *
 */
X_API int PL_get_atom_chars(term_t ts, char **a)  /* SAM check type */
{
  CACHE_REGS
  Term t = Yap_GetFromSlot(ts PASS_REGS);
  if (!IsAtomTerm(t) || IsWideAtom(AtomOfTerm(t)))
    return 0;
  *a = RepAtom(AtomOfTerm(t))->StrOfAE;
  return 1;
}

/** @brief *a is assigned the string representation of the atom in term  ts, and *len its size, or the operation fails
 *
 */
X_API int PL_get_atom_nchars(term_t ts, size_t *len, char **s)  /* SAM check type */
{
  CACHE_REGS
  Term t = Yap_GetFromSlot(ts PASS_REGS);
  if (!IsAtomTerm(t))
    return 0;
  *s = RepAtom(AtomOfTerm(t))->StrOfAE;
  *len = strlen(*s);
  return 1;
}

/** PL_get_chars converts a term t to a string.
 *
 * From the SWI manual:
 *
 * int PL_get_chars(term_t +t, char **s, unsigned flags) Convert the
 * argument term t to a 0-terminated C-string. flags is a bitwise
 * disjunction from two groups of constants. The first specifies which
 * term-types should converted and the second how the argument is
 * stored. Below is a specification of these constants. BUF_RING
 * implies, if the data is not static (as from an atom), the data is
 * copied to the next buffer from a ring of sixteen (16) buffers. This is a
 * convenient way of converting multiple arguments passed to a foreign
 * predicate to C-strings. If BUF_MALLOC is used, the data must be
 * freed using free() when not needed any longer.

   - CVT_ATOM Convert if term is an atom
   - CVT_STRING Convert if term is a string
   - CVT_LIST Convert if term is a list of integers between 1 and 255
   - CVT_INTEGER Convert if term is an integer (using %d)
   - CVT_FLOAT Convert if term is a float (using %f)
   - CVT_NUMBER Convert if term is a integer or float
   - CVT_ATOMIC Convert if term is atomic
   - CVT_VARIABLE Convert variable to print-name
   - CVT_ALL Convert if term is any of the above, except for variables
   - BUF_DISCARDABLE Data must copied immediately
   - BUF_RING Data is stored in a ring of buffers
   - BUF_MALLOC Data is copied to a new buffer returned by malloc(3)
*/

/** @brief *f is assigned the functor of term  ts, or the operation fails
 *
 */
X_API int PL_get_functor(term_t ts, functor_t *f)
{
  CACHE_REGS
  Term t = Yap_GetFromSlot(ts PASS_REGS);
  if ( IsAtomTerm(t)) {
    *f = t;
  } else {
    *f = FunctorToSWIFunctor(FunctorOfTerm(t));
  }
  return 1;
}

/** @brief *f is assigned the floating  point number of term  ts, or the operation fails
 *
 */
X_API int PL_get_float(term_t ts, double *f) /* SAM type check*/
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if ( IsFloatTerm(t)) {
    *f = FloatOfTerm(t);
  } else if ( IsIntegerTerm(t)) {
    *f = IntegerOfTerm(t);
#if USE_GMP
  } else if (IsBigIntTerm(t)) {
    *f = Yap_gmp_to_float( t );
#endif
  } else {
    return 0;
  }
  return 1;
}

/** @brief *s is assigned the string representation of the string in term  ts, and *len its size, or the operation fails
 *
 */
X_API int PL_get_string_chars(term_t t, char **s, size_t *len)
{
  CACHE_REGS
  Term tt = Yap_GetFromSlot(t PASS_REGS);
  if (!IsStringTerm(tt)) {
    return 0;
  }
  *s = (char *)StringOfTerm(tt);
  *len = utf8_strlen(*s, strlen(*s));
  return TRUE;
}

/** @brief h is assigned the head of the pair term  ts, and tl its tail, or the operation fails
 *
 */

X_API int PL_get_list(term_t ts, term_t h, term_t tl)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if (IsVarTerm(t) || !IsPairTerm(t) ) {
    return 0;
  }
  Yap_PutInSlot(h,HeadOfTerm(t) PASS_REGS);
  Yap_PutInSlot(tl,TailOfTerm(t) PASS_REGS);
  return 1;
}


/** @brief h is assigned the head of the pair term  ts, or the operation fails
 *
 */
X_API int PL_get_head(term_t ts, term_t h)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if (!YAP_IsPairTerm(t) ) {
    return 0;
  }
  Yap_PutInSlot(h,YAP_HeadOfTerm(t) PASS_REGS);
  return 1;
}

/** @brief *s is assigned the string representation of the term  ts, and *len its size, or the operation fails
 *
 */
X_API int PL_get_string(term_t t, char **s, size_t *len)
{
  return PL_get_string_chars(t, s, len);
}

/**
 *  @}
 *  */

/**
 *  @defgroup swi-unify-operations Unifying Terms
 *  @ingroup swi-term_manipulation
 *  @{
 *  */


/** @brief t unifies with the true/false value in a.
 *
 */
X_API int PL_unify_bool(term_t t, int a)
{
  CACHE_REGS
  Term iterm = (a ? MkAtomTerm(AtomTrue) : MkAtomTerm(AtomFalse) );
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS),iterm);
}


#if USE_GMP

		 /*******************************
		 *	       GMP		*
		 *******************************/

X_API int PL_get_mpz(term_t t, mpz_t mpz)
{
  CACHE_REGS
  Term t0 = Yap_GetFromSlot(t PASS_REGS);

  return Yap_term_to_existing_big(t0, mpz);
}

X_API int PL_unify_mpz(term_t t, mpz_t mpz)
{
  CACHE_REGS
  Term iterm = Yap_MkBigIntTerm(mpz);
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS),iterm);
}

X_API int PL_get_mpq(term_t t, mpq_t mpz)
{
  CACHE_REGS
  Term t0 = Yap_GetFromSlot(t PASS_REGS);

  return Yap_term_to_existing_rat(t0, mpz);
}

X_API int PL_unify_mpq(term_t t, mpq_t mpq)
{
  CACHE_REGS
  Term iterm = Yap_MkBigRatTerm(mpq);
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS),iterm);
}

#endif

/* SWI: int PL_get_module(term_t t, module_t *m) */
X_API int PL_get_module(term_t ts, module_t *m)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if (!IsAtomTerm(t) )
    return FALSE;
  *m = Yap_GetModuleEntry(t);
  return TRUE;
}

/* SWI: int PL_new_module(term_t t, module_t *m) */
X_API module_t PL_new_module(atom_t swiat)
{
  Atom at = SWIAtomToAtom(swiat);
  Term t;

  WRITE_LOCK(RepAtom(at)->ARWLock);  
  t = Yap_Module(MkAtomTerm(at));
  WRITE_UNLOCK(RepAtom(at)->ARWLock);    
  return Yap_GetModuleEntry(t);
}

/* SWI: int PL_get_atom(term_t t, YAP_Atom *a)
   YAP: YAP_Atom YAP_AtomOfTerm(Term) */
X_API int PL_get_nil(term_t ts)
{
  CACHE_REGS
  Term t = Yap_GetFromSlot(ts PASS_REGS);
  return ( t == TermNil );
}

/* SWI: int PL_get_pointer(term_t t, int *i)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API int PL_get_pointer(term_t ts, void **i)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if (IsVarTerm(t) || !IsIntegerTerm(t) )
    return 0;
  *i = (void *)IntegerOfTerm(t);
  return 1;
}

X_API int PL_get_tail(term_t ts, term_t tl)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if (!YAP_IsPairTerm(t) ) {
    return 0;
  }
  Yap_PutInSlot(tl,YAP_TailOfTerm(t) PASS_REGS);
  return 1;
}

/* end PL_get_* functions  =============================*/

/* begin PL_new_* functions =============================*/

/* SWI: atom_t PL_new_atom(const char *)
   YAP: YAP_Atom LookupAtom(char *) */
/*  SAM should the following be used instead?
      YAP_Atom  FullLookupAtom(char *)
      */
X_API atom_t PL_new_atom(const char *c)
{
  CACHE_REGS
  Atom at;
  atom_t sat;

  while((at = Yap_CharsToAtom(c PASS_REGS)) == 0L) {
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_new_atom" ))
      return FALSE;
  }
  Yap_AtomIncreaseHold(at);
  sat = AtomToSWIAtom(at);
  return sat;
}

X_API atom_t PL_new_atom_nchars(size_t len, const char *c)
{
  CACHE_REGS
  Atom at;
  atom_t sat;

  while((at = Yap_NCharsToAtom(c, len PASS_REGS)) == 0L) {
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_new_atom_nchars" ))
      return FALSE;
  }
  Yap_AtomIncreaseHold(at);
  sat = AtomToSWIAtom(at);
  return sat;
}

X_API atom_t PL_new_atom_wchars(size_t len, const wchar_t *c)
{
  CACHE_REGS
  Atom at;
  atom_t sat;

  while((at = Yap_NWCharsToAtom(c, len PASS_REGS)) == 0L) {
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_new_atom_wchars" ))
      return FALSE;
  }
  Yap_AtomIncreaseHold(at);
  sat = AtomToSWIAtom(at);
  return sat;
}

X_API wchar_t *PL_atom_wchars(atom_t name, size_t *sp)
{
  Atom at = SWIAtomToAtom(name);
  if (!IsWideAtom(at))
    return NULL;
  *sp = wcslen(RepAtom(at)->WStrOfAE);
  return RepAtom(at)->WStrOfAE;
}

X_API functor_t PL_new_functor(atom_t name, int arity)
{
  functor_t f;
  Atom at = SWIAtomToAtom(name);
  if (arity == 0) {
    f = FunctorToSWIFunctor((Functor)MkAtomTerm(at));
  } else {
    f = FunctorToSWIFunctor(Yap_MkFunctor(at,arity));
  }
  return f;
}

X_API atom_t PL_functor_name(functor_t f)
{
  if (IsAtomTerm(f)) {
    return AtomToSWIAtom(AtomOfTerm((Term)SWIFunctorToFunctor(f)));
  } else {
    return AtomToSWIAtom(NameOfFunctor(SWIFunctorToFunctor(f)));
  }
}

X_API int PL_functor_arity(functor_t f)
{
  if (IsAtomTerm(f)) {
    return 0;
  } else {
    return ArityOfFunctor(SWIFunctorToFunctor(f));
  }
}

/* end PL_new_* functions =============================*/

/* begin PL_put_* functions =============================*/

X_API int PL_cons_functor(term_t d, functor_t f,...)
{
  CACHE_REGS
  va_list ap;
  int arity, i;
  Term *tmp, t;
  Functor ff = SWIFunctorToFunctor(f);

  if (IsAtomTerm((Term)ff)) {
    Yap_PutInSlot(d, (YAP_Term)f PASS_REGS);
    return TRUE;
  }
  arity = ArityOfFunctor(ff);
  if (Unsigned(HR)+arity > Unsigned(ASP)-CreepFlag) {
    if (!do_gc(arity*sizeof(CELL))) {
      return FALSE;
    }
  }
  if (arity == 2 && ff == FunctorDot) {
    t = Yap_MkNewPairTerm();
    tmp = RepPair(t);
  } else {
    t = Yap_MkNewApplTerm(ff, arity);
    tmp = RepAppl(t)+1;
  }
  va_start (ap, f);
  for (i = 0; i < arity; i++) {
    Yap_unify(tmp[i],Yap_GetFromSlot(va_arg(ap, term_t) PASS_REGS));
  }
  va_end (ap);
  Yap_PutInSlot(d,t PASS_REGS);
  return TRUE;
}

X_API int PL_cons_functor_v(term_t d, functor_t f, term_t a0)
{
  CACHE_REGS
  int arity, i;
  Term *tmp, t;
  Functor ff = SWIFunctorToFunctor(f);

  if (IsAtomTerm((Term)ff)) {
    Yap_PutInSlot(d, (YAP_Term)f PASS_REGS);
    return TRUE;
  }
  arity = ArityOfFunctor(ff);
  if (Unsigned(HR) > Unsigned(ASP)-CreepFlag) {
    if (!do_gc(0)) {
      return FALSE;
    }
  }
  if (arity == 2 && ff == FunctorDot) {
    t = Yap_MkNewPairTerm();
    tmp = RepPair(t);
  } else {
    t = Yap_MkNewApplTerm(ff, arity);
    tmp = RepAppl(t)+1;
  }
  for (i = 0; i < arity; i++) {
    Yap_unify(tmp[i] , Yap_GetFromSlot(a0 PASS_REGS) );
    a0++;
  }
  Yap_PutInSlot(d,t PASS_REGS);

  return TRUE;
}

X_API int PL_cons_list(term_t d, term_t h, term_t t)
{
  CACHE_REGS
  if (Unsigned(HR) > Unsigned(ASP)-CreepFlag) {
    if (!do_gc(0)) {
      return FALSE;
    }
  }
  Yap_PutInSlot(d,MkPairTerm(Yap_GetFromSlot(h PASS_REGS),Yap_GetFromSlot(t PASS_REGS)) PASS_REGS);
  return TRUE;
}

X_API int PL_put_atom(term_t t, atom_t a)
{
  CACHE_REGS
  Yap_PutInSlot(t,MkAtomTerm(SWIAtomToAtom(a)) PASS_REGS);
  return TRUE;
}

X_API int PL_put_atom_chars(term_t t, const char *s)
{
  CACHE_REGS
  Atom at;
  while((at = Yap_CharsToAtom(s PASS_REGS)) == 0L) {
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_atom_nchars" ))
      return FALSE;
  }
  Yap_AtomIncreaseHold(at);
  Yap_PutInSlot(t,MkAtomTerm(at) PASS_REGS);
  return TRUE;
}

X_API int PL_put_atom_nchars(term_t t, size_t len, const char *s)
{
  CACHE_REGS
  Atom at;
  while((at = Yap_NCharsToAtom(s, len PASS_REGS)) == 0L) {
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_atom_nchars" ))
      return FALSE;
  }
  Yap_AtomIncreaseHold(at);
  Yap_PutInSlot(t,MkAtomTerm(at) PASS_REGS);
  return TRUE;
}

X_API int PL_put_float(term_t t, double fl)
{
  CACHE_REGS
  Yap_PutInSlot(t,YAP_MkFloatTerm(fl) PASS_REGS);
  return TRUE;
}

X_API int PL_put_functor(term_t t, functor_t f)
{
  long int  arity;
  Functor ff = SWIFunctorToFunctor(f);

  CACHE_REGS
  if (IsAtomTerm((Term)ff)) {
    Yap_PutInSlot(t,(Term)ff PASS_REGS);
  } else {
    arity = ArityOfFunctor(ff);
    if (Unsigned(HR)+arity > Unsigned(ASP)-CreepFlag) {
      if (!do_gc(arity*sizeof(CELL))) {
	return FALSE;
      }
    }
    if (arity == 2 && ff == FunctorDot)
      Yap_PutInSlot(t,YAP_MkNewPairTerm() PASS_REGS);    
    else
      Yap_PutInSlot(t,YAP_MkNewApplTerm((YAP_Functor)ff,arity) PASS_REGS);
  }
  return TRUE;
}

X_API int PL_put_integer(term_t t, long n)
{
  CACHE_REGS
  Yap_PutInSlot(t,YAP_MkIntTerm(n) PASS_REGS);
  return TRUE;
}

X_API int PL_put_int64(term_t t, int64_t n)
{
  CACHE_REGS
#if SIZEOF_INT_P==8
  Yap_PutInSlot(t,MkIntegerTerm(n) PASS_REGS);
  return TRUE;
#elif USE_GMP
  char s[64];
  MP_INT rop;

#ifdef _WIN32
  snprintf(s, 64, "%I64d", (long long int)n);
#elif HAVE_SNPRINTF
  snprintf(s, 64, "%lld", (long long int)n);
#else
  sprintf(s, "%lld", (long long int)n);
#endif
  mpz_init_set_str (&rop, s, 10);
  Yap_PutInSlot(t,YAP_MkBigNumTerm((void *)&rop) PASS_REGS);
  return TRUE;
#else
	// use a double, but will mess up writing.
    Int x = n;
    if (x == n)
    	return PL_put_integer(t, x);
    else {
    	union {
    		int64_t i;
    		double d;
    	} udi_;
    	udi_.i = n;
    	return PL_put_float(t, udi_.d);
    }
#endif
}

X_API int PL_put_list(term_t t)
{
  CACHE_REGS
  Yap_PutInSlot(t,YAP_MkNewPairTerm() PASS_REGS);
  if (Unsigned(HR) > Unsigned(ASP)-CreepFlag) {
    if (!do_gc(0)) {
      return FALSE;
    }
  }
  return TRUE;
}

X_API int PL_put_list_chars(term_t t, const char *s)
{
  CACHE_REGS
  Term nt;
  while((nt = Yap_CharsToListOfAtoms(s PASS_REGS)) == 0L) {
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_put_string_nchars" ))
      return FALSE;
  }
  Yap_PutInSlot(t, nt PASS_REGS);
  return TRUE;
}

X_API void PL_put_nil(term_t t)
{
  CACHE_REGS
  Yap_PutInSlot(t,TermNil PASS_REGS);
}

/* SWI: void PL_put_pointer(term_t -t, void *ptr)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API int PL_put_pointer(term_t t, void *ptr)
{
  CACHE_REGS
  YAP_Term tptr = YAP_MkIntTerm((YAP_Int)ptr);
  Yap_PutInSlot(t,tptr PASS_REGS);
  return TRUE;
}

X_API int PL_put_string_nchars(term_t t, size_t len, const char *chars)
{
  CACHE_REGS
  Term nt;
  while((nt = Yap_NCharsToString(chars, len PASS_REGS)) == 0L) {
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_put_string_nchars" ))
      return FALSE;
  }
  Yap_PutInSlot(t, nt PASS_REGS);
  return TRUE;
}

X_API int PL_put_term(term_t d, term_t s)
{
  CACHE_REGS
  Yap_PutInSlot(d,Yap_GetFromSlot(s PASS_REGS) PASS_REGS);
  return TRUE;
}

X_API int PL_put_variable(term_t t)
{
  CACHE_REGS
  Yap_PutInSlot(t,MkVarTerm() PASS_REGS);
  return TRUE;
}

/* end PL_put_* functions =============================*/

/* SWI: int PL_raise_exception(term_t exception)
   YAP: NO EQUIVALENT */
/* SAM TO DO */

X_API int PL_raise_exception(term_t exception)
{
  CACHE_REGS
  EX = Yap_StoreTermInDB(Yap_GetFromSlot(exception PASS_REGS),0);
  return 0;
}

X_API int PL_throw(term_t exception)
{
  CACHE_REGS
  YAP_Throw(Yap_GetFromSlot(exception PASS_REGS));
  if (LOCAL_execution)
    longjmp(LOCAL_execution->q_env, 0);
  return 0;
}

X_API void  PL_fatal_error(const char *msg)
{
  fprintf(stderr,"[ FATAL ERROR: %s ]\n",msg);
  Yap_exit(1);
}

X_API int PL_warning(const char *msg, ...) {
  va_list args;
  va_start(args, msg);
  // just print the warning message and return? 
  fprintf(stderr,"[Warning:");
  fprintf(stderr,msg,args);
  fprintf(stderr,"]\n");
  va_end(args);
  PL_fail;
}

/* begin PL_unify_* functions =============================*/

X_API int PL_unify(term_t t1, term_t t2)
{
  CACHE_REGS
  return Yap_unify(Yap_GetFromSlot(t1 PASS_REGS),Yap_GetFromSlot(t2 PASS_REGS));
}

/* SWI: int PL_unify_atom(term_t ?t, atom  *at)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_atom(term_t t, atom_t at)
{
  CACHE_REGS
  YAP_Term cterm = MkAtomTerm(SWIAtomToAtom(at));
  return YAP_Unify(Yap_GetFromSlot(t PASS_REGS),cterm);
}

/* SWI: int PL_unify_atom_chars(term_t ?t, const char *chars)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_atom_chars(term_t t, const char *s)
{
  CACHE_REGS
  Atom at;
  while((at = Yap_CharsToAtom(s PASS_REGS)) == 0L) {
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_atom_nchars" ))
      return FALSE;
  }
  Yap_AtomIncreaseHold(at);
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS), MkAtomTerm(at));
}

/* SWI: int PL_unify_atom_chars(term_t ?t, const char *chars)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_atom_nchars(term_t t, size_t len, const char *s)
{
  CACHE_REGS
  Atom at;
  while((at = Yap_NCharsToAtom(s, len PASS_REGS)) == 0L) {
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_atom_nchars" ))
      return FALSE;
  }
  Yap_AtomIncreaseHold(at);
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS), MkAtomTerm(at));
}

/* SWI: int PL_unify_float(term_t ?t, double f)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_float(term_t t, double f)
{
  CACHE_REGS
  Term fterm = MkFloatTerm(f);
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS),fterm);
}

/* SWI: int PL_unify_integer(term_t ?t, long n)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_integer(term_t t, long n)
{	
  CACHE_REGS
  Term iterm = MkIntegerTerm(n);
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS),iterm);
}

/* SWI: int PL_unify_integer(term_t ?t, long n)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_functor(term_t t, functor_t f)
{	
  CACHE_REGS
  Term tt = Yap_GetFromSlot(t PASS_REGS);
  Functor ff = SWIFunctorToFunctor(f);
  if (IsVarTerm(tt)) {
    if (Unsigned(HR)+ArityOfFunctor(ff) > Unsigned(ASP)-CreepFlag) {
      if (!do_gc(0)) {
	return FALSE;
      }
    }
    return Yap_unify(tt, Yap_MkNewApplTerm(ff,ArityOfFunctor(ff)));
  }
  if (IsPairTerm(tt))
    return ff == FunctorDot;
  if (!IsApplTerm(tt))
    return FALSE;
  return ff == FunctorOfTerm(tt);
}

/* SWI: int PL_unify_integer(term_t ?t, long n)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_int64(term_t t, int64_t n)
{	
  CACHE_REGS
#if SIZEOF_INT_P==8
  Term iterm = MkIntegerTerm(n);
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS),iterm);
#elif USE_GMP
  YAP_Term iterm;
  char s[64];
  MP_INT rop;

#ifdef _WIN32
  snprintf(s, 64, "%I64d", (long long int)n);
#elif HAVE_SNPRINTF
  snprintf(s, 64, "%lld", (long long int)n);
#else
  sprintf(s, "%lld", (long long int)n);
#endif
  mpz_init_set_str (&rop, s, 10);
  iterm = YAP_MkBigNumTerm((void *)&rop);
  return YAP_Unify(Yap_GetFromSlot(t PASS_REGS),iterm);
#else
  if ((long)n == n)
    return PL_unify_integer(t, n);
	// use a double, but will mess up writing.
  else {
  	union {
  		int64_t i;
  		double d;
  	} udi_;
  	udi_.i = n;
  	return PL_unify_float(t, udi_.d);
  }
#endif

}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_list(term_t tt, term_t h, term_t tail)
{
  CACHE_REGS
  Term t;
  if (Unsigned(HR) > Unsigned(ASP)-CreepFlag) {
    if (!do_gc(0)) {
      return FALSE;
    }
  }
  t = Deref(Yap_GetFromSlot(tt PASS_REGS));
  if (IsVarTerm(t)) {
    Term pairterm = Yap_MkNewPairTerm();
    Yap_unify(t, pairterm);
    /* avoid calling deref */
    t = pairterm;
  } else if (!IsPairTerm(t)) {
    return FALSE;
  }
  Yap_PutInSlot(h,HeadOfTerm(t) PASS_REGS);
  Yap_PutInSlot(tail,TailOfTerm(t) PASS_REGS);
  return TRUE;
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_arg(int index, term_t tt, term_t arg)
{
  CACHE_REGS
  Term t = Deref(Yap_GetFromSlot(tt PASS_REGS)), to;
  if (index < 0)
    return FALSE;
  if (IsVarTerm(t) || IsAtomOrIntTerm(t)) {
    return FALSE;
  } else if (IsPairTerm(t)) {
    if (index == 1)
      to = HeadOfTerm(t);
    else if (index == 2)
      to = TailOfTerm(t);
    else
      return FALSE;
  } else {
    Functor f = FunctorOfTerm(t);
    if (IsExtensionFunctor(f)) 
      return FALSE;
    if (index > ArityOfFunctor(f))
      return FALSE;
    to = ArgOfTerm(index, t);
  }
  return Yap_unify(Yap_GetFromSlot(arg PASS_REGS),to);
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_list_chars(term_t t, const char *chars)
{
  CACHE_REGS
  Term chterm;
  while((chterm = Yap_CharsToListOfAtoms(chars PASS_REGS)) == 0L) {
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_list_chars" ))
      return FALSE;
  }
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS), chterm);
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_list_ncodes(term_t t, size_t len, const char *chars)
{
  CACHE_REGS
  Term chterm;
  while((chterm = Yap_NCharsToListOfCodes(chars, len PASS_REGS)) == 0L) {
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_list_ncodes" ))
      return FALSE;
  }
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS), chterm);
}

X_API int
PL_unify_list_codes(term_t t, const char *chars)
{ 
  CACHE_REGS
  Term chterm;
  while((chterm = Yap_CharsToListOfCodes(chars PASS_REGS)) == 0L) {
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_list_codes" ))
      return FALSE;
  }
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS), chterm);
}

/* SWI: int PL_unify_nil(term_t ?l)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_nil(term_t t)
{
  CACHE_REGS
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS), TermNil);
}

/* SWI: int PL_unify_pointer(term_t ?t, void *ptr)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API int PL_unify_pointer(term_t t, void *ptr)
{
  CACHE_REGS
  YAP_Term ptrterm = YAP_MkIntTerm((YAP_Int)ptr);
  return YAP_Unify(Yap_GetFromSlot(t PASS_REGS), ptrterm);
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_string_chars(term_t t, const char *chars)
{
  CACHE_REGS
  Term chterm;
  while((chterm = Yap_CharsToString(chars PASS_REGS)) == 0L) {
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_list_ncodes" ))
      return FALSE;
  }
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS), chterm);
}

X_API int PL_unify_string_nchars(term_t t, size_t len, const char *chars)
{
  CACHE_REGS
  Term chterm;
  while((chterm = Yap_NCharsToString(chars, len PASS_REGS)) == 0L) {
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_list_ncodes" ))
      return FALSE;
  }
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS), chterm);
}

/* SWI: int PL_unify_wchars(term_t ?t, int type, size_t len,, const pl_wchar_t *s)
 */
X_API int PL_unify_wchars(term_t t, int type, size_t len, const pl_wchar_t *chars)
{
  CACHE_REGS
  YAP_Term chterm;

  while (TRUE) {
    switch (type) {
    case PL_ATOM:
    {
      Atom at;
      at = Yap_NWCharsToAtom(chars, len PASS_REGS);
      if (at) {
	Yap_AtomIncreaseHold(at);
	chterm = MkAtomTerm(at);
	return Yap_unify(Yap_GetFromSlot(t PASS_REGS), chterm);
      }
    }
    break;
    case PL_UTF8_STRING:
    case PL_STRING:
      if ((chterm = Yap_NWCharsToString(chars, len PASS_REGS)) != 0) {
	return YAP_Unify(Yap_GetFromSlot(t PASS_REGS), chterm);
      }
      break;
    case PL_CODE_LIST:
      if ((chterm = Yap_NWCharsToListOfCodes(chars, len PASS_REGS)) != 0) {
	return YAP_Unify(Yap_GetFromSlot(t PASS_REGS), chterm);
      }
      break;
    case PL_CHAR_LIST:
      if ((chterm = Yap_NWCharsToListOfAtoms(chars, len PASS_REGS)) != 0) {
	return YAP_Unify(Yap_GetFromSlot(t PASS_REGS), chterm);
      }
      break;
    default:
      /* should give error?? */
      return FALSE;
    }
    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_wchars" ))
      return FALSE;
  }
  return FALSE;
}

typedef struct {
  int type;
  union {
    functor_t f;
    term_t t;
    atom_t a;
    long l;
    int i;
    double dbl;
    char *s;
    struct {
      size_t n;
      char *s;
    } ns;
    struct {
      size_t n;
      wchar_t *w;
    } nw;
    void *p;
    wchar_t *w;
  } arg;
} arg_types;


static YAP_Term
MkBoolTerm(int b)
{
  if (b)
    return MkAtomTerm(AtomTrue);
  else
    return MkAtomTerm(AtomFalse);
}

#define MAX_DEPTH 64

typedef struct {
  int nels;
  CELL *ptr;
} stack_el;

/* SWI: int PL_unify_term(term_t ?t1, term_t ?t2)
   YAP long int  YAP_Unify(YAP_Term* a, Term* b) */
int PL_unify_termv(term_t l, va_list ap)
{
  CACHE_REGS
  int type, res;
  int nels = 1;
  int depth = 1;
  Term a[1], *pt;
  stack_el stack[MAX_DEPTH];
  
  BACKUP_MACHINE_REGS();
  if (Unsigned(HR) > Unsigned(ASP)-CreepFlag) {
    if (!do_gc(0)) {
      RECOVER_MACHINE_REGS();
      return FALSE;
    }
  }
  pt = a;
  while (depth > 0) {
    while (nels > 0) {
      type = va_arg(ap, int);
      nels--;
      switch(type) {
      case PL_VARIABLE:
	*pt++ = MkVarTerm();
	break;
      case PL_BOOL:
	*pt++ = MkBoolTerm(va_arg(ap, int));
	break;
      case PL_ATOM:
	*pt++ = MkAtomTerm(SWIAtomToAtom(va_arg(ap, atom_t)));
	break;
      case PL_INTEGER:
	*pt++ = MkIntegerTerm(va_arg(ap, long));
	break;
      case PL_SHORT:
	*pt++ = MkIntegerTerm(va_arg(ap, int));
	break;
      case PL_LONG:
	*pt++ = MkIntegerTerm(va_arg(ap, long));
	break;
      case PL_INT:
	*pt++ = MkIntegerTerm(va_arg(ap, int));
	break;
      case PL_FLOAT:
	*pt++ = MkFloatTerm(va_arg(ap, double));
	break;
      case PL_STRING:
	{
	  Term chterm;
	  const char *chars = va_arg(ap, char *);
	  while((chterm = Yap_CharsToString(chars PASS_REGS)) == 0L) {
	    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_term" ))
	      return FALSE;
	  }
	  *pt++ = chterm;
	}
	break;
      case PL_CHARS:
	{
	  Atom at;
	  const char *chars = va_arg(ap, char *);
	  while((at = Yap_CharsToAtom(chars PASS_REGS)) == 0L) {
	    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_term" ))
	      return FALSE;
	  }
	  *pt++ = MkAtomTerm(at);
	  Yap_AtomIncreaseHold(at);
	}
	break;
      case PL_NCHARS:
	{
	  Atom at;
	  size_t sz = va_arg(ap, size_t);
	  const char *chars = va_arg(ap, char *);
	  while((at = Yap_NCharsToAtom(chars, sz PASS_REGS)) == 0L) {
	    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_term" ))
	      return FALSE;
	  }
	  *pt++ = MkAtomTerm(at);
	  Yap_AtomIncreaseHold(at);
	}
	break;
      case PL_NWCHARS:
	{
	  Atom at;
	  size_t sz = va_arg(ap, size_t);
	  const wchar_t *chars = va_arg(ap, wchar_t *);
	  while((at = Yap_NWCharsToAtom(chars, sz PASS_REGS)) == 0L) {
	    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_term" ))
	      return FALSE;
	  }
	  *pt++ = MkAtomTerm(at);
	  Yap_AtomIncreaseHold(at);
	}
	break;
      case PL_TERM:
	{
	  Term t = Yap_GetFromSlot(va_arg(ap, size_t) PASS_REGS);
	  if (IsVarTerm(t) && VarOfTerm(t) >= ASP && VarOfTerm(t) < LCL0) {
	    Yap_unify(*pt++, t);
	  }
	  else {
	    *pt++ = t;
	  }
	}
	break;
      case PL_POINTER:
	*pt++ = MkIntegerTerm((Int)va_arg(ap, void *));
	break;
      case PL_INTPTR:
	*pt++ = MkIntegerTerm((Int)va_arg(ap, intptr_t));
	break;
      case PL_INT64:
#if SIZEOF_INT_P==8
	*pt++ = MkIntegerTerm((Int)va_arg(ap, long int));
#elif USE_GMP
	{
	  char s[64];
	  MP_INT rop;

#ifdef _WIN32
	  snprintf(s, 64, "%I64d", va_arg(ap, long long int));
#elif HAVE_SNPRINTF
	  snprintf(s, 64, "%lld", va_arg(ap, long long int));
#else
	  sprintf(s, "%lld", va_arg(ap, long long int));
#endif	  
	  mpz_init_set_str (&rop, s, 10);
	  *pt++ = YAP_MkBigNumTerm((void *)&rop);
	}
#else
	{
		int64_t i = (Int)va_arg(ap, int64_t);
		intptr_t x = i;
		if (x == i)
			*pt++ = MkIntegerTerm( x );
		else {
			// use a double, but will mess up writing.
			union {
				int64_t i;
				double d;
	  	    } udi_;
	  	    udi_.i = i;
	  	    *pt++ = MkFloatTerm(udi_.d);
		}
	}
#endif
	break;
      case PL_FUNCTOR:
	{
	  functor_t f = va_arg(ap, functor_t);
	  Functor ff = SWIFunctorToFunctor(f);
	  UInt arity = ArityOfFunctor(ff);

	  if (!arity) {
	    *pt++ = MkAtomTerm((Atom)f);
	  } else {
	    Term t = Yap_MkNewApplTerm(ff, arity);
	    if (nels) {
	      if (depth == MAX_DEPTH) {
		fprintf(stderr,"ERROR: very deep term in PL_unify_term, change MAX_DEPTH from %d\n", MAX_DEPTH);
		return FALSE;
	      }
	      stack[depth-1].nels = nels;
	      stack[depth-1].ptr = pt+1;
	      depth++;
	    }
	    *pt = t;
	    if (ff == FunctorDot)
	      pt = RepPair(t);
	    else
	      pt = RepAppl(t)+1;
	    nels = arity;
	  }
	}
	break;
      case PL_FUNCTOR_CHARS:
	{
	  char *fname = va_arg(ap, char *);
	  size_t arity = va_arg(ap, size_t);
	  Atom at;

	  while((at = Yap_CharsToAtom(fname PASS_REGS)) == 0L) {
	    if (LOCAL_Error_TYPE && !Yap_SWIHandleError( "PL_unify_term" ))
	      return FALSE;
	  }
	  Yap_AtomIncreaseHold(at);
	  if (!arity) {
	    *pt++ = MkAtomTerm(at);
	  } else {
	    Functor ff;
	    Term t;

	    ff = Yap_MkFunctor(at,arity);
	    t = Yap_MkNewApplTerm(ff, arity);
	    if (nels) {
	      if (depth == MAX_DEPTH) {
		fprintf(stderr,"very deep term in PL_unify_term\n");
		return FALSE;
	      }
	      stack[depth-1].nels = nels;
	      stack[depth-1].ptr = pt+1;
	      depth++;
	    }
	    *pt = t;
	    if (ff == FunctorDot)
	      pt = RepPair(t);
	    else
	      pt = RepAppl(t)+1;
	    nels = arity;
	  }
	}
	break;
      case PL_LIST:
	{
	  Term t = Yap_MkNewPairTerm();

	  if (nels) {
	    if (depth == MAX_DEPTH) {
	      fprintf(stderr,"very deep term in PL_unify_term\n");
	      return FALSE;
	    }
	    stack[depth-1].nels = nels;
	    stack[depth].ptr = pt+1;
	    depth++;
	  }
	  *pt = t;
	  pt = RepPair(t);
	  nels = 2;
	}
	break;
      default:
	fprintf(stderr, "PL_unify_term: %d not supported\n", type);
	exit(1);
      }
    }
    depth--;
    if (depth) {
      pt = stack[depth-1].ptr;
      nels = stack[depth-1].nels;
    }
  }
  res = Yap_unify(Yap_GetFromSlot(l PASS_REGS),a[0]);
  RECOVER_MACHINE_REGS();
  return res;
}


int
PL_unify_term(term_t t, ...)
{ va_list args;
  int rval;

  va_start(args, t);
  rval = PL_unify_termv(t, args);
  va_end(args);

  return rval;
}


/* end PL_unify_* functions =============================*/

/* SWI: void PL_register_atom(atom_t atom) */
X_API void PL_register_atom(atom_t atom)
{
  Yap_AtomIncreaseHold(SWIAtomToAtom(atom));
}

/* SWI: void PL_unregister_atom(atom_t atom) */
X_API void PL_unregister_atom(atom_t atom)
{
  Yap_AtomDecreaseHold(SWIAtomToAtom(atom));
}

X_API int PL_term_type(term_t t)
{
  CACHE_REGS
  /* YAP_ does not support strings as different objects */
  YAP_Term v = Yap_GetFromSlot(t PASS_REGS);
  if (IsVarTerm(v)) {
    return PL_VARIABLE;
  } else if (IsAtomTerm(v)) {
    return PL_ATOM;
  } else if (IsIntegerTerm(v)) {
    return PL_INTEGER;
  } else if (IsStringTerm(v)) {
    return PL_STRING;
  } else if (IsFloatTerm(v)) {
    return PL_FLOAT;
  } else {
    return PL_TERM;
  }
}

X_API int PL_is_atom(term_t t)
{
  CACHE_REGS
  return IsAtomTerm(Yap_GetFromSlot(t PASS_REGS));
}

X_API int PL_is_ground(term_t t)
{
  CACHE_REGS
  return Yap_IsGroundTerm(Yap_GetFromSlot(t PASS_REGS));
}

X_API int PL_is_acyclic(term_t t)
{
  CACHE_REGS
  return Yap_IsAcyclicTerm(Yap_GetFromSlot(t PASS_REGS));
}

X_API int PL_is_callable(term_t t)
{
  CACHE_REGS
  YAP_Term t1 = Yap_GetFromSlot(t PASS_REGS);
  if (IsVarTerm(t1))
    return FALSE;
  if (IsAtomTerm(t1) || IsPairTerm(t1)) 
    return TRUE;
  if (IsApplTerm(t1) && !IsExtensionFunctor(FunctorOfTerm(t1)))
      return TRUE;
  return FALSE;
}

X_API int PL_is_atomic(term_t ts)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  return !IsVarTerm(t) || !IsApplTerm(t) || !IsPairTerm(t);
}

X_API int PL_is_compound(term_t ts)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  return (IsApplTerm(t) || IsPairTerm(t));
}

X_API int PL_is_functor(term_t ts, functor_t f)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  Functor ff = SWIFunctorToFunctor(f);
  if (YAP_IsApplTerm(t)) {
    return FunctorOfTerm(t) == (Functor)ff;
  } else if (YAP_IsPairTerm(t)) {
    return ff == FunctorDot;
  } else
    return 0;
}

X_API int PL_is_float(term_t ts)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  return !IsVarTerm(t) && IsFloatTerm(t);
}

X_API int PL_is_integer(term_t ts)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  if (IsVarTerm(t)) return FALSE;
  if (IsIntTerm(t)) return TRUE;
  if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    if (f == FunctorLongInt)
      return TRUE;
    if (f == FunctorBigInt) {
      CELL mask = RepAppl(t)[1];
      return ( mask == BIG_INT );
    }
  }
  return FALSE;
}

X_API int PL_is_list(term_t ts)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  return !IsVarTerm(t) && (t == TermNil || IsPairTerm(t));
}

X_API int PL_is_pair(term_t ts)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  return !IsVarTerm(t) && IsPairTerm(t);
}

X_API int
PL_skip_list(term_t list, term_t tail, size_t *len)
{ 
  CACHE_REGS
  Term *l = Yap_AddressFromSlot(list PASS_REGS);
  Term *t;
  intptr_t length;

  length = Yap_SkipList(l, &t);
  if ( len )
    *len = length;
  if ( tail )
  { Term t2 = Yap_GetFromSlot(tail PASS_REGS);

    Yap_unify(t2, *t);
  }

  if ( *t == TermNil )
    return PL_LIST;
  else if ( IsVarTerm(*t) )
    return PL_PARTIAL_LIST;
  else if ( IsPairTerm(*t) )
    return PL_CYCLIC_TERM;
  else
    return PL_NOT_A_LIST;
}


X_API int PL_is_number(term_t ts)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  return IsIntegerTerm(t) || IsBigIntTerm(t) || IsFloatTerm(t);
}

X_API int PL_is_string(term_t ts)
{
  CACHE_REGS
  Term t = Yap_GetFromSlot(ts PASS_REGS);
  return IsStringTerm(t);
}

X_API int PL_is_variable(term_t ts)
{
  CACHE_REGS
  YAP_Term t = Yap_GetFromSlot(ts PASS_REGS);
  return YAP_IsVarTerm(t);
}

X_API int PL_compare(term_t ts1, term_t ts2)
{
  CACHE_REGS
  YAP_Term t1 = Yap_GetFromSlot(ts1 PASS_REGS);
  YAP_Term t2 = Yap_GetFromSlot(ts2 PASS_REGS);
  return YAP_CompareTerms(t1, t2);
}

X_API char *
PL_record_external
(term_t ts, size_t *sz)
{
  CACHE_REGS
  Term t = Yap_GetFromSlot(ts PASS_REGS);
  size_t len = 512, nsz;
  char *s;

  while(TRUE) {
    if (!(s = Yap_AllocCodeSpace(len)))
      return NULL;
    if ((nsz = Yap_ExportTerm(t, s, len, 0))) {
      *sz = nsz;
      return (char *)s;
    } else {
      if (len < 16*1024) 
	len = len *2;
      else 
	len += 16*1024;
    }
  }
  return NULL;
}

/* 
   partial implementation of recorded_external, does not guarantee endianness nor portability, and does not
   support constraints.
 */

X_API int
PL_recorded_external
(const char *tp, term_t ts)
{
  CACHE_REGS
    Term t = Yap_ImportTerm((void *)tp);
  if (t == 0)
    return FALSE;
  Yap_PutInSlot(ts, t PASS_REGS);
  return TRUE;
}

X_API int
PL_erase_external
(char *tp)
{
  Yap_FreeCodeSpace(tp);
  return TRUE;
}

X_API record_t
PL_record(term_t ts)
{
  CACHE_REGS
  Term t = Yap_GetFromSlot(ts PASS_REGS);
  return (record_t)YAP_Record(t);
}

X_API int
PL_recorded(record_t db, term_t ts)
{
  CACHE_REGS
  Term t = YAP_Recorded((void *)db);
  if (t == ((CELL)0))
    return FALSE;
  Yap_PutInSlot(ts,t PASS_REGS);
  return TRUE;
}

X_API record_t
PL_duplicate_record(record_t db)
{
  Term t = YAP_Recorded((void *)db);
  if (t == ((CELL)0))
    return FALSE;
  return (record_t)YAP_Record(t);
}

X_API void
PL_erase(record_t db)
{
  YAP_Erase((void *)db);
}

X_API void PL_halt(int e)
{
   YAP_Halt(e);
}

X_API int PL_action(int action,...)
{
  va_list ap;

  va_start (ap, action);
  switch (action) {
  case PL_ACTION_TRACE:
    fprintf(stderr, "PL_ACTION_TRACE not supported\n");
    break;
  case PL_ACTION_DEBUG:
    fprintf(stderr, "PL_ACTION_DEBUG not supported\n");
    break;
  case PL_ACTION_BACKTRACE:
    fprintf(stderr, "PL_ACTION_BACKTRACE not supported\n");
    break;
  case PL_ACTION_HALT:
    {
      int halt_arg = va_arg(ap, int);
      YAP_Halt(halt_arg);
    }
    break;
  case PL_ACTION_ABORT:
    {
      YAP_Throw(MkAtomTerm(Yap_LookupAtom("abort")));
    }
    break;
  case PL_ACTION_BREAK:
    fprintf(stderr, "PL_ACTION_BREAK not supported\n");
    break;
  case PL_ACTION_GUIAPP:
    fprintf(stderr, "PL_ACTION_GUIAPP not supported\n");
    break;
  case PL_ACTION_WRITE:
    fprintf(stderr, "PL_ACTION_WRITE not supported\n");
    break;
  case PL_ACTION_FLUSH:
    fprintf(stderr, "PL_ACTION_WRITE not supported\n");
    break;
  case PL_ACTION_ATTACH_CONSOLE:
    fprintf(stderr, "PL_ACTION_WRITE not supported\n");
    break;
  }
  va_end (ap);
  return 0;
}

X_API term_t
PL_exception(qid_t q)
{
  YAP_Term t;
  if (YAP_GoalHasException(&t)) {
    CACHE_REGS
    term_t to = Yap_NewSlots(1 PASS_REGS);
    Yap_PutInSlot(to,t PASS_REGS);
    return to;
  } else {
    return 0L;
  }
}

X_API void
PL_clear_exception(void)
{
  CACHE_REGS
  EX = NULL;
}

X_API int
PL_initialise(int myargc, char **myargv)
{
  YAP_init_args init_args;

  memset((void *)&init_args,0,sizeof(init_args));
  init_args.Argv = myargv;
  init_args.Argc = myargc;
#if BOOT_FROM_SAVED_STATE
  init_args.SavedState = "startup.yss";
#else
  init_args.SavedState = NULL;
#endif
  init_args.YapLibDir = NULL;
  init_args.YapPrologBootFile = NULL;
  init_args.HaltAfterConsult = FALSE;
  init_args.FastBoot = FALSE;
  init_args.MaxTableSpaceSize = 0;
  init_args.NumberWorkers = 1;
  init_args.SchedulerLoop = 10;
  init_args.DelayedReleaseLoad = 3;

  YAP_parse_yap_arguments(myargc,myargv,&init_args);

  GLOBAL_PL_Argc = myargc;
  GLOBAL_PL_Argv = myargv;
  GLOBAL_InitialisedFromPL = TRUE;
  int rc = YAP_Init(&init_args) != YAP_BOOT_ERROR;
  return rc;
}

X_API int
PL_is_initialised(int *argcp, char ***argvp)
{
  if (GLOBAL_InitialisedFromPL) {
    if (argcp) 
      *argcp = GLOBAL_PL_Argc;
    if (argvp) 
      *argvp = GLOBAL_PL_Argv;
  }
  return GLOBAL_InitialisedFromPL;
}

X_API module_t
PL_context(void)
{
  CACHE_REGS
  return Yap_GetModuleEntry(LOCAL_SourceModule);
}

X_API int
PL_strip_module(term_t raw, module_t *m, term_t plain)
{
  CACHE_REGS
  Term m0, t;
  if (m) {
    if (*m)
      m0 = MkAtomTerm((*m)->AtomOfME);
    else
      m0 = MkAtomTerm(AtomProlog);
  } else
    m0 = USER_MODULE;
  t =  Yap_StripModule(Yap_GetFromSlot(raw PASS_REGS), &m0);
  if (!t)
    return FALSE;
  *m = Yap_GetModuleEntry(m0);
  Yap_PutInSlot(plain, t PASS_REGS);
  return TRUE;
}

X_API atom_t PL_module_name(module_t m)
{
  Atom at = m->AtomOfME;
  return AtomToSWIAtom(at);
}

X_API predicate_t PL_pred(functor_t f, module_t m)
{
  Functor ff = SWIFunctorToFunctor(f);
  Term mod = SWIModuleToModule(m);

  if (IsAtomTerm((Term)f)) {
    return YAP_Predicate(YAP_AtomOfTerm((Term)f),0,mod);
  } else {
    return YAP_Predicate((YAP_Atom)NameOfFunctor(ff),ArityOfFunctor(ff),mod);
  }
}

X_API predicate_t PL_predicate(const char *name, int arity, const char *m)
{
  CACHE_REGS
  Term mod;
  Atom at;
  if (m == NULL) {
    mod = CurrentModule;
    if (!mod) mod = USER_MODULE;
  } else {
    Atom at;
    while (!(at = Yap_LookupAtom((char *)m))) {
      if (!Yap_growheap(FALSE, 0L, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
	return NULL;
      }
    }
    mod = MkAtomTerm(at);
  }
  while (!(at = Yap_LookupAtom((char *)name))) {
    if (!Yap_growheap(FALSE, 0L, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
      return NULL;
    }
  }
  return YAP_Predicate((YAP_Atom)at, arity, mod);
}

X_API int PL_unify_predicate(term_t head, predicate_t pred, int how)
{
  CACHE_REGS
  PredEntry *pe = (PredEntry *)pred;
  Term ts[2], nt;
  if (!pe->ModuleOfPred) {
    ts[0] = pe->ModuleOfPred;
  } else {
    ts[0] = TermProlog;
  }
  if (how == GP_NAMEARITY) {
    Term nts[2];
    nts[1] = MkIntegerTerm(pe->ArityOfPE);
    if (pe->ArityOfPE) {
      nts[0] = MkAtomTerm(NameOfFunctor(pe->FunctorOfPred));
    } else {
      nts[0] = MkAtomTerm((Atom)pe->FunctorOfPred);
    }
    ts[1] = Yap_MkApplTerm(FunctorSlash, 2, nts);
  } else {
    if (pe->ArityOfPE) {
      ts[1] = Yap_MkNewApplTerm(pe->FunctorOfPred, pe->ArityOfPE);
    } else {
      ts[1] = MkAtomTerm((Atom)pe->FunctorOfPred);
    }
  }
  nt = Yap_MkApplTerm(FunctorModule, 2, ts);
  return Yap_unify(Yap_GetFromSlot(head PASS_REGS),nt);
}

X_API void PL_predicate_info(predicate_t p,atom_t *name, int *arity, module_t *m)
{
  PredEntry *pd = (PredEntry *)p;
  Atom aname;

  if (pd->ArityOfPE) {
    if (arity)
      *arity = pd->ArityOfPE;
    aname = NameOfFunctor(pd->FunctorOfPred);
  } else {
    if (arity)
      *arity = 0;
    aname = (Atom)(pd->FunctorOfPred);
  }
  if (pd->ModuleOfPred && m) 
    *m = Yap_GetModuleEntry(pd->ModuleOfPred);
  else if (m)
    *m = Yap_GetModuleEntry(TermProlog);
  if (name)
    *name = AtomToSWIAtom(aname);
}

#undef S_YREG


X_API fid_t
PL_open_foreign_frame(void)
{
  CACHE_REGS
  /* initialise a new marker choicepoint */
    choiceptr cp_b = ((choiceptr)(ASP-1))-1;
    cp_b->cp_tr = TR;
    cp_b->cp_h = HR;
    cp_b->cp_b = B;
    cp_b->cp_cp = CP;
    cp_b->cp_env = ENV;
    cp_b->cp_ap = NOCODE;
#ifdef DEPTH_LIMIT
    cp_b->cp_depth = DEPTH;
#endif /* DEPTH_LIMIT */
    cp_b->cp_a1 = MkIntTerm(LOCAL_CurSlot);
    HB = HR;
    B = cp_b;
    ASP = (CELL *)B;
    Yap_StartSlots( PASS_REGS1 );
    
    return (fid_t)(LCL0-(CELL*)cp_b);
}

X_API void
PL_close_foreign_frame(fid_t f)
{
  CACHE_REGS
  choiceptr cp_b = (choiceptr)(LCL0-(UInt)f);
  LOCAL_CurSlot = IntOfTerm(cp_b->cp_a1);
  B = cp_b;
  HB = B->cp_h;
  Yap_TrimTrail();
  B = cp_b->cp_b;
  CP = cp_b->cp_cp;
  ENV = cp_b->cp_env;
#ifdef DEPTH_LIMIT
  DEPTH = cp_b->cp_depth;
#endif /* DEPTH_LIMIT */
  HB = B->cp_h;
  if (LOCAL_CurSlot) {
  /* we can assume there was a slot before */
    CELL *old_slot;
    old_slot = LCL0-(LOCAL_CurSlot);
    ASP = old_slot-(2+IntOfTerm(old_slot[-1]));
  } else {
    ASP = ((CELL *)(cp_b+1))+1;
  }
}

static void
backtrack(void)
{
  CACHE_REGS
  P = FAILCODE;
  Yap_absmi(0);
  TR = B->cp_tr;
}

X_API void
PL_rewind_foreign_frame(fid_t f)
{
  CACHE_REGS
  choiceptr cp_b = (choiceptr)(LCL0-(UInt)f);
  if (B != cp_b) {
    while (B->cp_b != cp_b)
      B = B->cp_b;
  }
  backtrack();
  // restore to original location
  ASP = (CELL *)B;
  LOCAL_CurSlot = IntOfTerm(B->cp_a1);
  Yap_StartSlots( PASS_REGS1 );  
}

X_API void
PL_discard_foreign_frame(fid_t f)
{
  CACHE_REGS
  choiceptr cp_b = (choiceptr)(LCL0-(UInt)f);

  if (B != cp_b) {
    while (B->cp_b != cp_b)
      B = B->cp_b;
    backtrack();
  }
  LOCAL_CurSlot = IntOfTerm(cp_b->cp_a1);
  B = cp_b->cp_b;
  CP = cp_b->cp_cp;
  ENV = cp_b->cp_env;
  HB = B->cp_h;
#ifdef DEPTH_LIMIT
  DEPTH = cp_b->cp_depth;
#endif /* DEPTH_LIMIT */
  /* we can assume there was a slot before */
  if (LOCAL_CurSlot) {
    CELL *old_slot;
    old_slot = LCL0-(LOCAL_CurSlot);
    ASP = old_slot-(2+IntOfTerm(old_slot[-1]));
  } else {
    ASP = ((CELL *)(cp_b+1))+1;
  }
}

X_API qid_t PL_open_query(module_t ctx, int flags, predicate_t p, term_t t0)
{
  CACHE_REGS
  YAP_Term *t = NULL;
  if (t0) 
    t = Yap_AddressFromSlot(t0 PASS_REGS);

  /* ignore flags  and module for now */
  qid_t new = (qid_t)Yap_AllocCodeSpace(sizeof(struct open_query_struct));
  new->oq = LOCAL_execution;
  LOCAL_execution = new;
  new->q_open=1;
  new->q_state=0;
  new->q_flags = flags;
  new->q_pe = (PredEntry *)p;
  new->q_g = t;
  return new;
}

X_API int PL_next_solution(qid_t qi)
{
  CACHE_REGS
  int result;
  if (qi->q_open != 1) return 0;
  if (setjmp(LOCAL_execution->q_env))
    return 0;
  // don't forget, on success these guys must create slots
  if (qi->q_state == 0) {
    result = YAP_EnterGoal((YAP_PredEntryPtr)qi->q_pe, qi->q_g, &qi->q_h);
  } else {
    LOCAL_AllowRestart = qi->q_open;
    result = YAP_RetryGoal(&qi->q_h);
  }
  qi->q_state = 1;
  if (result == 0) {
    YAP_LeaveGoal(FALSE, &qi->q_h);
    qi->q_open = 0;
  }
  return result;
}

X_API void PL_cut_query(qid_t qi)
{
  CACHE_REGS

  if (qi->q_open != 1 || qi->q_state == 0) return;
  YAP_LeaveGoal(FALSE, &qi->q_h);
  qi->q_open = 0;
  LOCAL_execution = qi->oq;
  Yap_FreeCodeSpace( (char *)qi );
}

X_API void PL_close_query(qid_t qi)
{
  CACHE_REGS

  if (EX && !(qi->q_flags & (PL_Q_CATCH_EXCEPTION))) {
    EX = NULL;
  }
  /* need to implement backtracking here */
  if (qi->q_open != 1 || qi->q_state == 0) {
    return;
  }
  YAP_LeaveGoal(FALSE, &qi->q_h);
  qi->q_open = 0;
  LOCAL_execution = qi->oq;
  Yap_FreeCodeSpace( (char *)qi );
}

X_API int PL_call_predicate(module_t ctx, int flags, predicate_t p, term_t t0)
{
  fid_t f = PL_open_foreign_frame();
  qid_t qi = PL_open_query(ctx, flags, p, t0);
  int ret = PL_next_solution(qi);
  PL_cut_query(qi);
  PL_close_foreign_frame(f);
  return ret;
}

X_API int PL_toplevel(void)
{
  while (TRUE) {
    if (YAP_RunGoal(MkAtomTerm(Yap_FullLookupAtom("$live")))) {
      return TRUE;
    }
  }
  return TRUE;
}

X_API int PL_call(term_t tp, module_t m)
{
  CACHE_REGS
  int out;

  BACKUP_B();
  BACKUP_H();

  Term t[2], g;
  t[0] = SWIModuleToModule(m);
  t[1] = Yap_GetFromSlot(tp PASS_REGS);
  g = Yap_MkApplTerm(FunctorModule,2,t);
  out =  YAP_RunGoal(g);

  RECOVER_H();
  RECOVER_B();
  return out;
}

X_API void PL_register_foreign_in_module(const char *module, const char *name, int arity, pl_function_t function, int flags)
{
  CACHE_REGS
  Term tmod;
  Int nflags = 0;

#ifdef DEBUG
  if (flags & (PL_FA_CREF)) {
    fprintf(stderr,"PL_register_foreign_in_module called with non-implemented flag %x when creating predicate %s:%s/%d\n", flags, module, name, arity);
  }      
#endif
  if (module == NULL) {
    tmod = CurrentModule;
  } else {
    tmod = MkAtomTerm(Yap_LookupAtom((char *)module));
  }
  if (flags & PL_FA_VARARGS) { 
    nflags = SWIEnvPredFlag;
  }
  if (flags & PL_FA_TRANSPARENT) {
    nflags |= ModuleTransparentPredFlag;
  } else {
    nflags |= CArgsPredFlag;
  }
  if (flags & PL_FA_NONDETERMINISTIC) {
    Yap_InitCPredBackCut((char *)name, arity, sizeof(struct foreign_context)/sizeof(CELL), (CPredicate)function, (CPredicate)function, (CPredicate)function, UserCPredFlag|nflags);
  } else {
    UserCPredicate((char *)name,(CPredicate)function,arity,tmod,nflags);
  }
  if (flags & PL_FA_NOTRACE) {
    Yap_SetNoTrace((char *)name, arity, tmod);
  }
}

X_API void PL_register_extensions(const PL_extension *ptr)
{
  // implemented as register foreign
  // may cause problems during initialization?
  PL_load_extensions(ptr);
}

X_API void
PL_register_extensions_in_module(const char *module, const PL_extension *e)
{ 
  // implemented as register foreign
  /* ignore flags for now */
  while(e->predicate_name != NULL) {
    PL_register_foreign_in_module(module, e->predicate_name, e->arity, e->function, e->flags);
    e++;
  }
}


X_API void PL_register_foreign(const char *name, int arity, pl_function_t function, int flags)
{
  PL_register_foreign_in_module(NULL, name, arity, function, flags);
}

X_API void PL_load_extensions(const PL_extension *ptr)
{
  /* ignore flags for now */
  while(ptr->predicate_name != NULL) {
    PL_register_foreign_in_module(NULL, ptr->predicate_name, ptr->arity, ptr->function, ptr->flags);
    ptr++;
  }
}

X_API  int PL_is_inf(term_t st)
{
  CACHE_REGS
  Term t = Deref(Yap_GetFromSlot(st PASS_REGS));
  if (IsVarTerm(t)) return FALSE;
  if (!IsFloatTerm(t)) return FALSE;
#if HAVE_ISINF
  Float fl;
 fl = FloatOfTerm(t);
  return isinf(fl);
#elif HAVE_FPCLASS
  Float fl;
  fl = FloatOfTerm(t);
  return (fpclass(fl) == FP_NINF || fpclass(fl) == FP_PINF);
#else
  return FALSE;
#endif
}

X_API int PL_thread_self(void)
{
  CACHE_REGS
#if THREADS
  if (pthread_getspecific(Yap_yaamregs_key) == NULL)
    return -1;
  return (worker_id+1)<<3;
#else
  return -2;
#endif
}

#if THREADS

static int
alertThread(int tid)
{
#if _WIN32
  return pthread_kill(REMOTE_ThreadHandle(tid).pthread_handle, YAP_WINTIMER_SIGNAL) == 0;
#else
  return pthread_kill(REMOTE_ThreadHandle(tid).pthread_handle, YAP_ALARM_SIGNAL) == 0;
#endif
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_thread_raise() is used  for  re-routing   interrupts  in  the Windows
version, where the signal handler is running  from a different thread as
Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_thread_raise(int tid, int sig)
{ 
  PL_LOCK(L_THREAD);
  if ( tid < 1  )
    { error:
      PL_UNLOCK(L_THREAD);
      return FALSE;
    }
  if ( !REMOTE_ThreadHandle(tid).in_use )
    goto error;


  if ( !raiseSignal(REMOTE_PL_local_data_p(tid), sig) ||
       !alertThread(tid) )
    goto error;

  PL_UNLOCK(L_THREAD);

  return TRUE;
}
#else

int
PL_thread_raise(int tid, int sig)
{ 
  if ( !raiseSignal(NULL, sig) )
    return FALSE;

  return TRUE;
}

#endif

X_API int PL_unify_thread_id(term_t t, int i)
{
  CACHE_REGS
  Term iterm = MkIntegerTerm(i);
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS),iterm);
}


static int
pl_thread_self(void)
{
  CACHE_REGS
#if THREADS
  if (pthread_getspecific(Yap_yaamregs_key) == NULL)
    return -1;
  return worker_id;
#else
  return -2;
#endif
}

X_API int PL_thread_attach_engine(const PL_thread_attr_t *attr)
{
  int wid = pl_thread_self();
  
  if (wid < 0) {
    /* we do not have an engine */
    if (attr) {
      YAP_thread_attr yapt;

      yapt.ssize = attr->local_size;
      yapt.tsize = attr->global_size;
      yapt.alias = MkAtomTerm(Yap_LookupAtom(attr->alias));
      yapt.cancel =  attr->cancel;
      wid = YAP_ThreadCreateEngine(&yapt);
    } else {
      wid = YAP_ThreadCreateEngine(NULL);
    }
    if (wid < 0)
      return -1;
    if (YAP_ThreadAttachEngine(wid)) {
      return wid;
    }
    return -1;
  } else {
    /* attach myself again */
    return YAP_ThreadAttachEngine(wid);
  }
}

X_API int PL_thread_destroy_engine(void)
{
  int wid = pl_thread_self();

  if (wid < 0) {
    /* we do not have an engine */
    return FALSE;
  }
  YAP_ThreadDetachEngine(wid);
  return YAP_ThreadDestroyEngine(wid);
}

X_API int
PL_thread_at_exit(void (*function)(void *), void *closure, int global)
{
  /* don't do nothing for now */
  fprintf(stderr,"%% YAP ERROR: PL_thread_at_exit not implemented yet\n");
  return TRUE;
}


X_API PL_engine_t
PL_create_engine(const PL_thread_attr_t *attr)
{
#if THREADS
  int eng;
  if (attr) {
    YAP_thread_attr yapt;

    yapt.ssize = attr->local_size;
    yapt.tsize = attr->global_size;
    yapt.alias = MkAtomTerm(Yap_LookupAtom(attr->alias));
    yapt.cancel =  attr->cancel;

    eng = YAP_ThreadCreateEngine(&yapt);
  } else {
    eng = YAP_ThreadCreateEngine(NULL);
  }
  if (eng >= 0)
    return  Yap_local[eng];
#endif
  return NULL;
}


X_API int
PL_destroy_engine(PL_engine_t e)
{
#if THREADS
  return YAP_ThreadDestroyEngine(((struct worker_local *)e)->ThreadHandle_.current_yaam_regs->worker_id_);
#else
  return FALSE;
#endif
}

X_API int
PL_set_engine(PL_engine_t engine, PL_engine_t *old)
{
  CACHE_REGS
#if THREADS
  int cwid = pl_thread_self(), nwid;
  if (cwid >= 0) {
    if (old) *old = (PL_engine_t)(Yap_local[cwid]);
  }
  if (!engine) {
    if (cwid < 0)
      return PL_ENGINE_INVAL;
    if (!YAP_ThreadDetachEngine(worker_id)) {
      return PL_ENGINE_INVAL;
    }
    return PL_ENGINE_SET;
  }
  if (engine == PL_ENGINE_MAIN) {
    nwid = 0;
  } else if (engine == PL_ENGINE_CURRENT) {
    if (cwid < 0) {
      if (old) *old = NULL;
      return PL_ENGINE_INVAL;
    }
    return PL_ENGINE_SET;
  } else {
    nwid = ((struct worker_local *)engine)->ThreadHandle_.id;
  }

  MUTEX_LOCK(&(REMOTE_ThreadHandle(nwid).tlock));
  if (REMOTE_ThreadHandle(nwid).ref_count) {
    MUTEX_UNLOCK(&(REMOTE_ThreadHandle(nwid).tlock));
    if (cwid != nwid) {
      return PL_ENGINE_INUSE;
    }
    return PL_ENGINE_SET;
  }
  if (!YAP_ThreadAttachEngine(nwid)) {
    return PL_ENGINE_INVAL;
  }
  return PL_ENGINE_SET;
#else
  if (old) *old = (PL_engine_t)&Yap_local;
  return FALSE;
#endif
}


X_API void *
PL_malloc(size_t sz)
{
  if ( sz == 0 )
    return NULL;
  return (void *)malloc((long unsigned int)sz);
}

X_API void *
PL_realloc(void *ptr, size_t sz)
{
  if (ptr) {
    if (sz) {
      return realloc((char *)ptr,(long unsigned int)sz);
    } else {
      free(ptr);
      return NULL;
    }
  } else {
    return PL_malloc(sz);
  }
}

X_API void
PL_free(void *obj)
{
  if (obj)
    free(obj);
}

X_API int
PL_eval_expression_to_int64_ex(term_t t, int64_t *val)
{
  CACHE_REGS
  Term res = Yap_Eval(Yap_GetFromSlot(t PASS_REGS));
  if (!res) {
    return FALSE;
  }
  if (IsIntegerTerm(res)) {
    *val = IntegerOfTerm(res);
    return TRUE;
#if  SIZEOF_INT_P==4 && USE_GMP
  } else if (YAP_IsBigNumTerm(res)) {
    MP_INT g;
    char s[64];
    
    YAP_BigNumOfTerm(t, (void *)&g);
    if (mpz_sizeinbase(&g,2) > 64) {
      return PL_error(NULL,0,NULL, ERR_EVALUATION, AtomToSWIAtom(Yap_LookupAtom("int_overflow")));
    }
    mpz_get_str (s, 10, &g);
#ifdef _WIN32
    sscanf(s, "%I64d", (long long int *)val);
#else
    sscanf(s, "%lld", (long long int *)val);
#endif
    return 1;
#endif
  }
  PL_error(NULL,0,NULL, ERR_TYPE, AtomToSWIAtom(Yap_LookupAtom("integer_expression")));
  return FALSE;
}

foreign_t
_PL_retry(intptr_t v)
{
  return (((uintptr_t)(v)<<FRG_REDO_BITS)|REDO_INT);
}

foreign_t
_PL_retry_address(void *addr)
{
  return (((uintptr_t)(addr))|REDO_PTR);
}


X_API int
PL_foreign_control(control_t ctx)
{
  switch (ctx->control) {
  case FRG_REDO:
    return PL_REDO;
  case FRG_FIRST_CALL:
    return PL_FIRST_CALL;
  default:
    return PL_CUTTED;
  }
}

X_API intptr_t
PL_foreign_context(control_t ctx)
{
  switch (ctx->control) {
  case FRG_FIRST_CALL:
    return 0L;
  default:
    return (intptr_t)(ctx->context);
  }
}


X_API void *
PL_foreign_context_address(control_t ctx)
{
  switch (ctx->control) {
  case FRG_FIRST_CALL:
    return NULL;
  default:
    return (void *)(ctx->context);
  }
}

X_API int
PL_get_signum_ex(term_t sig, int *n)
{
  char *s;
  int i = -1;

  if ( PL_get_integer(sig, &i) )
  {
  } else if ( PL_get_chars(sig, &s, CVT_ATOM) )
  { i = Yap_signal_index(s);
  } else
  { return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_signal, sig);
  }

  if ( i > 0 && i < 32 )		/* where to get these? */
  { *n = i;
    return TRUE;
  }

  return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_signal, sig);
}

typedef struct blob {
  Functor f;
  CELL type;
  MP_INT blinfo;  /* total size should go here */
  PL_blob_t *blb;
  size_t size;
  CELL  blob_data[1];
} blob_t;

X_API intptr_t
PL_query(int query)
{
  switch(query) {
  case PL_QUERY_ARGC:
    return (intptr_t)GLOBAL_argc;
  case PL_QUERY_ARGV:
    return (intptr_t)GLOBAL_argv;
  case PL_QUERY_USER_CPU:
    return (intptr_t)Yap_cputime();
  case PL_QUERY_VERSION:
    return (intptr_t)60300;
  default:
    fprintf(stderr,"Unimplemented PL_query %d\n",query);
    return (intptr_t)0;
  }
}  

X_API void
PL_cleanup_fork(void)
{
}  


X_API void (*PL_signal(int sig, void (*func)(int)))(int)
{
  //  return Yap_signal2(sig,func);
  return NULL;
}

X_API void PL_on_halt(int (*f)(int, void *), void *closure)
{
  Yap_HaltRegisterHook((HaltHookFunc)f,closure);
}

#define is_signalled() unlikely(LD && LD->signal.pending != 0)

#ifdef O_PLMT
#include <pthread.h>
static pthread_key_t atomgen_key;
#endif

typedef struct scan_atoms {
  Int pos;
  Atom atom;
} scan_atoms_t;

static inline int
str_prefix(const char *p0, char *s)
{
  char *p = (char *)p0;
  while (*p && *p == *s) { p++; s++; }
  return p[0] == '\0';
}

static int
atom_generator(const char *prefix, char **hit, int state)
{
  struct scan_atoms *index;
  Atom            catom;
  Int            i;

#ifdef O_PLMT
  if ( !atomgen_key ) {
    pthread_key_create(&atomgen_key, NULL);
    state = FALSE;
  }
#endif

  if ( !state )
    { index = (struct scan_atoms *)malloc(sizeof(struct scan_atoms));
      i = 0;
      catom = NIL;
  } else
  {
#ifdef O_PLMT
    index = (struct scan_atoms *)pthread_getspecific(atomgen_key);
#else
    index = LOCAL_search_atoms;
#endif
    catom = index->atom;
    i = index->pos;
  }

  while (catom != NIL || i < AtomHashTableSize) {
    //    if ( is_signalled() )		/* Notably allow windows version */
      //      PL_handle_signals();		/* to break out on ^C */
    AtomEntry *ap;

    if (catom == NIL) {
      /* move away from current hash table line */
      READ_LOCK(HashChain[i].AERWLock);
      catom = HashChain[i].Entry;
      READ_UNLOCK(HashChain[i].AERWLock);
      i++;
    } else {
      ap = RepAtom(catom);
      READ_LOCK(ap->ARWLock);
      if ( str_prefix(prefix, ap->StrOfAE) ) {
	index->pos = i;
	index->atom = ap->NextOfAE;
#ifdef O_PLMT
	pthread_setspecific(atomgen_key,index);
#else
	LOCAL_search_atoms = index;
#endif
	*hit = ap->StrOfAE;
	READ_UNLOCK(ap->ARWLock);
	return TRUE;
      }
      catom = ap->NextOfAE;
      READ_UNLOCK(ap->ARWLock);
    }
  }
#ifdef O_PLMT
  pthread_setspecific(atomgen_key,NULL);
#else
  LOCAL_search_atoms = NULL;
#endif
  free(index);
  return FALSE;
}


char *
PL_atom_generator(const char *prefix, int state)
{
  char * hit = NULL;
  if (atom_generator(prefix, &hit, state)) {
    return hit;
  }
  return NULL;
}


X_API pl_wchar_t *PL_atom_generator_w(const pl_wchar_t *pref, pl_wchar_t *buffer, size_t buflen, int state)
{
  return NULL;
}

const char *Yap_GetCurrentPredName(void);
Int Yap_GetCurrentPredArity(void);

const char *
Yap_GetCurrentPredName(void)
{
  CACHE_REGS
  if (!PP)
    return NULL;
  if (PP->ArityOfPE)
    return NameOfFunctor(PP->FunctorOfPred)->StrOfAE;
  return   RepAtom((Atom)(PP->FunctorOfPred))->StrOfAE;
}

Int
Yap_GetCurrentPredArity(void)
{
  CACHE_REGS
  if (!PP)
    return (Int)0;
  return PP->ArityOfPE;
}

void
Yap_swi_install(void)
{
  Yap_install_blobs();
}


extern atom_t 		fileNameStream(IOSTREAM *s);
extern Atom 		Yap_FileName(IOSTREAM *s);

Atom 
Yap_FileName(IOSTREAM *s)
{
  atom_t a = fileNameStream(s);
  if (!a) {
    return AtomEmptyAtom;
  }
  return SWIAtomToAtom(a);
}

extern void closeFiles(int);

void
Yap_CloseStreams(int loud)
{
  closeFiles(FALSE);
}

Int Yap_StreamToFileNo(Term t) {
  CACHE_REGS
  IOSTREAM *s;
  int rc;

  if ( (rc=PL_get_stream_handle(Yap_InitSlot(t PASS_REGS), &s)) ) {
    return Sfileno(s);
  }
  return -1;
}

FILE *Yap_FileDescriptorFromStream(Term t)
{
  CACHE_REGS
  IOSTREAM *s;
  int rc;

  if ( (rc=PL_get_stream_handle(Yap_InitSlot(t PASS_REGS), &s)) ) {
    fprintf(stderr,"Unimplemented\n");
    //    return Sfileno(s);
  }
  return NULL;
}

X_API int
PL_raise(int sig)
{
  Yap_signal(YAP_INT_SIGNAL);
   return 1;
}

int
raiseSignal(PL_local_data_t *ld, int sig)
{
#if THREADS
  if (sig == SIG_THREAD_SIGNAL) {
    Yap_signal(YAP_ITI_SIGNAL);
    return TRUE;    
  }
#endif
  fprintf(stderr, "Unsupported signal %d\n", sig);
  return FALSE;
}


#if THREADS
void Yap_LockStream(IOSTREAM *s)
{
  if ( s->mutex ) recursiveMutexLock(s->mutex);
}

void Yap_UnLockStream(IOSTREAM *s)
{
  if ( s->mutex ) recursiveMutexUnlock(s->mutex);
}
#endif

extern term_t Yap_CvtTerm(term_t ts);

term_t Yap_CvtTerm(term_t ts)
{
  CACHE_REGS
  Term t = Yap_GetFromSlot(ts PASS_REGS);
  if (IsVarTerm(t)) return ts;
  if (IsPairTerm(t)) return ts;
  if (IsAtomTerm(t)) return ts;
  if (IsIntTerm(t)) return ts;
  if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    if (IsExtensionFunctor(f)) {
      if (f == FunctorBigInt) {
	big_blob_type flag = RepAppl(t)[1];
	switch (flag) {
	case BIG_INT:
	  return ts;
	case BIG_RATIONAL:
#if USE_GMP
	  {
	    MP_RAT *b = Yap_BigRatOfTerm(t);
	    Term ta[2];
	    ta[0] = Yap_MkBigIntTerm(mpq_numref(b));
	    if (ta[0] == TermNil)
	      return ts;
	    ta[1] = Yap_MkBigIntTerm(mpq_denref(b));
	    if (ta[1] == TermNil)
	      return ts;
	    return Yap_InitSlot(Yap_MkApplTerm(FunctorRDiv, 2, ta) PASS_REGS);
	  }
#endif	  
	case EMPTY_ARENA:
	case ARRAY_INT:
	case ARRAY_FLOAT:
	case CLAUSE_LIST:
	case EXTERNAL_BLOB:
	  return Yap_InitSlot(MkIntTerm(0) PASS_REGS);
	default:
	  return ts;
	}
      } else if (f == FunctorDBRef) {
	Term ta[0];
	ta[0] = MkIntegerTerm((Int)DBRefOfTerm(t));
	return Yap_InitSlot(Yap_MkApplTerm(FunctorDBREF, 1, ta) PASS_REGS);	
      }
    }
  }
  return ts;
}

/**
 * @}
 * @}
 */

