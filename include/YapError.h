/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		YapError.h   						 *
* mods:									 *
* comments:	error header file for YAP				 *
* version:      $Id: Yap.h,v 1.38 2008-06-18 10:02:27 vsc Exp $	 *
*************************************************************************/

#ifndef YAP_ERROR_H
#define YAP_ERROR_H 1

#define ECLASS(CL, A, B) CL,

#define E0(A, B) A,
#define E(A, B, C) A,
#define E2(A, B, C, D) A,

#define BEGIN_ERRORS() typedef enum {

#define END_ERRORS()                                                           \
  }                                                                            \
  yap_error_number

#define BEGIN_ERROR_CLASSES() typedef enum {

#define END_ERROR_CLASSES()                                                    \
  }                                                                            \
  yap_error_class_number

#include "YapErrors.h"

struct yami  *Yap_Error__(const char *file, const char *function, int lineno, yap_error_number err,YAP_Term wheret, ...);

#define Yap_NilError( id,  ...) Yap_Error__(__FILE__, __FUNCTION__, __LINE__, id, TermNil,  __VA_ARGS__)

#define Yap_Error( id, inp, ...) Yap_Error__(__FILE__, __FUNCTION__, __LINE__, id, inp,  __VA_ARGS__)

#ifdef YAP_TERM_H
/** 
 * make sure next argument is a bound instance of type 
 * atom.
 */
#define Yap_ensure_atom( T0, TF ) { if ( (TF = Yap_ensure_atom__(__FILE__, __FUNCTION__, __LINE__, T0  )  == 0L ) return false; }

INLINE_ONLY extern inline Term
Yap_ensure_atom__( const char *fu, const char *fi, int line, Term in )
{
  Term t = Deref(in);
  //Term Context = Deref(ARG2);
  if (!IsVarTerm(t) && IsAtomTerm(t))
    return t;
  if (IsVarTerm(t)) {
    Yap_Error__(fu, fi, line, INSTANTIATION_ERROR, t, NULL);
  } else {
  if ( IsAtomTerm(t) ) return t ; 
  Yap_Error__(fu, fi, line, TYPE_ERROR_ATOM, t, NULL);
  return 0L;
}

#endif

#define JMP_LOCAL_ERROR(v, LAB)                                                \
  if (H + 2 * (v) > ASP - 1024) {                                              \
    LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;                                   \
    LOCAL_Error_Term = t;                                                      \
    LOCAL_Error_Size = 2 * (v) * sizeof(CELL);                                 \
    goto LAB;                                                                  \
  }

#define LOCAL_ERROR(t, v)                                                         \
  if (HR + (v) > ASP - 1024) {                                                 \
    LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;                                   \
    LOCAL_Error_Term = t;                                                      \
    LOCAL_Error_Size = 2 * (v) * sizeof(CELL);                                 \
    return NULL;                                                               \
  }

#define LOCAL_TERM_ERROR(t, v)                                                    \
  if (HR + (v) > ASP - 1024) {                                                 \
    LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;                                   \
    LOCAL_Error_Term = t;                                                      \
    LOCAL_Error_Size = 2 * (v) * sizeof(CELL);                                 \
    return 0L;                                                                 \
  }

#define AUX_ERROR(t, n, s, TYPE)                                               \
  if (s + (n + 1) > (TYPE *)AuxSp) {                                           \
    LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;                         \
    LOCAL_Error_Term = t;                                                      \
    LOCAL_Error_Size = n * sizeof(TYPE);                                       \
    return NULL;                                                               \
  }

#define AUX_TERM_ERROR(t, n, s, TYPE)                                          \
  if (s + (n + 1) > (TYPE *)AuxSp) {                                           \
    LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;                         \
    LOCAL_Error_Term = t;                                                      \
    LOCAL_Error_Size = n * sizeof(TYPE);                                       \
    return 0L;                                                                 \
  }

#define JMP_AUX_ERROR(n, s, t, TYPE, LAB)                                      \
  if (s + (n + 1) > (TYPE *)AuxSp) {                                           \
    LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;                         \
    LOCAL_Error_Term = t;                                                      \
    LOCAL_Error_Size = n * sizeof(TYPE);                                       \
    goto LAB;                                                                  \
  }

#define HEAP_ERROR(a, TYPE)                                                    \
  if (a == NIL) {                                                              \
    LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;                                    \
    LOCAL_Error_Term = t;                                                      \
    LOCAL_Error_Size = n * sizeof(TYPE);                                       \
    return NULL;                                                               \
  }

#define HEAP_TERM_ERROR(a, TYPE, n)                                            \
  if (a == NIL) {                                                              \
    LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;                                    \
    LOCAL_Error_Term = t;                                                      \
    LOCAL_Error_Size = n * sizeof(TYPE);                                       \
    return 0L;                                                                 \
  }

#define JMP_HEAP_ERROR(a, n, t, TYPE, LAB)                                     \
  if (a == NIL) {                                                              \
    LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;                                    \
    LOCAL_Error_Term = t;                                                      \
    LOCAL_Error_Size = n * sizeof(TYPE);                                       \
    goto LAB;                                                                  \
  }

#endif
