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

#define MAX_ERROR_MSG_SIZE 10

struct yami *Yap_Error__(const char *file, const char *function, int lineno,
                         yap_error_number err, YAP_Term wheret, ...);

void Yap_ThrowError__(const char *file, const char *function, int lineno,
                 yap_error_number err, YAP_Term wheret, ...)
                 #ifndef MSC_VER
                 __attribute__ ((noreturn))
                 #endif
                 ;


#define Yap_NilError(id, ...)                                                  \
  Yap_Error__(__FILE__, __FUNCTION__, __LINE__, id, TermNil, __VA_ARGS__)

#define Yap_Error(id, inp, ...)                                                \
Yap_Error__(__FILE__, __FUNCTION__, __LINE__, id, inp, __VA_ARGS__)

#define Yap_ThrowError(id, inp,  ...)                                                \
Yap_ThrowError__(__FILE__, __FUNCTION__, __LINE__, id, inp, __VA_ARGS__)

#ifdef YAP_TERM_H
/**
 * make sure next argument is a bound instance of type
 * atom.
 */
#define Yap_ensure_atom(T0, TF)                                                                 \
  { if ( (TF = Yap_ensure_atom__(__FILE__, __FUNCTION__, __LINE__, T0  )  == 0L ) return false; \
  }

INLINE_ONLY extern inline Term Yap_ensure_atom__(const char *fu, const char *fi,
                                                 int line, Term in) {
  Term t = Deref(in);
  // Term Context = Deref(ARG2);
  if (!IsVarTerm(t) && IsAtomTerm(t))
    return t;
  if (IsVarTerm(t)) {
    Yap_Error__(fu, fi, line, INSTANTIATION_ERROR, t, NULL);
  } else {
    if (IsAtomTerm(t))
      return t;
    Yap_Error__(fu, fi, line, TYPE_ERROR_ATOM, t, NULL);
    return 0L;
  }

#endif

#define JMP_LOCAL_ERROR(v, LAB)                                                \
  if (H + 2 * (v) > ASP - 1024) {                                              \
    LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;                                   \
    LOCAL_Error_Size = 2 * (v) * sizeof(CELL);                                 \
    goto LAB;                                                                  \
  }

#define LOCAL_ERROR(t, v)                                                      \
  if (HR + (v) > ASP - 1024) {                                                 \
    LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;                                   \
    LOCAL_Error_Size = 2 * (v) * sizeof(CELL);                                 \
    return NULL;                                                               \
  }

#define LOCAL_TERM_ERROR(t, v)                                                 \
  if (HR + (v) > ASP - 1024) {                                                 \
    LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;                                   \
    LOCAL_Error_Size = 2 * (v) * sizeof(CELL);                                 \
    return 0L;                                                                 \
  }

#define AUX_ERROR(t, n, s, TYPE)                                               \
  if (s + (n + 1) > (TYPE *)AuxSp) {                                           \
    LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;                         \
    LOCAL_Error_Size = n * sizeof(TYPE);                                       \
    return NULL;                                                               \
  }

#define AUX_TERM_ERROR(t, n, s, TYPE)                                          \
  if (s + (n + 1) > (TYPE *)AuxSp) {                                           \
    LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;                         \
    LOCAL_Error_Size = n * sizeof(TYPE);                                       \
    return 0L;                                                                 \
  }

#define JMP_AUX_ERROR(n, s, t, TYPE, LAB)                                      \
  if (s + (n + 1) > (TYPE *)AuxSp) {                                           \
    LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;                         \
    LOCAL_Error_Size = n * sizeof(TYPE);                                       \
    goto LAB;                                                                  \
  }

#define HEAP_ERROR(a, TYPE)                                                    \
  if (a == NIL) {                                                              \
    LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;                                    \
    LOCAL_Error_Size = n * sizeof(TYPE);                                       \
    return NULL;                                                               \
  }

#define HEAP_TERM_ERROR(a, TYPE, n)                                            \
  if (a == NIL) {                                                              \
    LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;                                    \
    LOCAL_Error_Size = n * sizeof(TYPE);                                       \
    return 0L;                                                                 \
  }

#define JMP_HEAP_ERROR(a, n, t, TYPE, LAB)                                     \
  if (a == NIL) {                                                              \
    LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;                                    \
    LOCAL_Error_Size = n * sizeof(TYPE);                                       \
    goto LAB;                                                                  \
  }

  /**
   * Error stages since it was initially processed.
   *
   * Notice that some of the stages may be active simultaneouly.
   */
  typedef enum yap_error_status {
    /// where we like to be
    YAP_NO_ERROR_STATUS = 0x0,
    /// Prolog discovered the problem
    YAP_ERROR_INITIATED_IN_PROLOG = 0x1,
    /// The problem was found before the system can cope
    YAP_ERROR_INITIATED_IN_BOOT = 0x2,
    /// C-helper like must_ found out the problem
    YAP_ERROR_INITIATED_IN_HELPER = 0x4,
    /// C-builtin crashed
    YAP_ERROR_INITIATED_IN_SYSTEM_C = 0x8,
    /// user code crashed
    YAP_ERROR_INITIATED_IN_USER_C = 0x10,
    /// ok, we put a throw to be dispatched
    YAP_THROW_THROWN = 0x20,
    /// someone caught it
    YAP_THROW_CAUGHT = 0x40,
    /// error became an exception (usually SWIG bridge)
    YAP_ERROR_EXPORTED_TO_CXX = 0x80,
    /// handle error in Prolog
    YAP_ERROR_BEING_PROCESSED_IN_PROLOG = 0x100
    /// go back t
  } yap_error_stage_t;

  /// a Prolo goal that caused a bug

  typedef struct error_prolog_source {
    YAP_Int prologPredCl;
    YAP_UInt prologPredLine;
    YAP_UInt prologPredFirstLine;
    YAP_UInt prologPredLastLine;
    YAP_Atom prologPredName;
    YAP_UInt prologPredArity;
    YAP_Term prologPredModule;
    YAP_Atom prologPredFile;
    struct DB_TERM *errorGoal;
    struct error_prolog_source *errorParent;
  } error_prolog_source_t;

  /// all we need to know about an error/throw
  typedef struct yap_error_descriptor {
    enum yap_error_status status;
    yap_error_class_number errorClass;
    YAP_Atom errorAsText;
    YAP_Atom classAsText;
    yap_error_number errorNo;
    YAP_Int errorLine;
    const char *errorFunction;
    const char *errorFile;
    // struct error_prolog_source *errorSource;
    YAP_Int prologPredCl;
    YAP_UInt prologPredLine;
    YAP_UInt prologPredFirstLine;
    YAP_UInt prologPredLastLine;
    YAP_Atom prologPredName;
    YAP_UInt prologPredArity;
    YAP_Term prologPredModule;
    YAP_Atom prologPredFile;
    YAP_UInt prologParserLine;
    YAP_UInt prologParserFirstLine;
    YAP_UInt prologParserLastLine;
    YAP_Atom prologParserName;
    YAP_Atom prologParserFile;
    YAP_Bool prologConsulting;
    struct DB_TERM *errorTerm;
    char *errorMsg;
    size_t errorMsgLen;
    struct yap_error_descriptor *top_error;
  } yap_error_descriptor_t;

/// compatibility with existing code..
#define LOCAL_Error_TYPE LOCAL_ActiveError->errorNo
#define LOCAL_Error_File LOCAL_ActiveError->errorFile
#define LOCAL_Error_Function LOCAL_ActiveError->errorFunction
#define LOCAL_Error_Lineno LOCAL_ActiveError->errorLine
#define LOCAL_Error_Size LOCAL_ActiveError->errorMsgLen
#define LOCAL_BallTerm LOCAL_ActiveError->errorTerm
#define LOCAL_ErrorMessage LOCAL_ActiveError->errorMsg

  extern bool Yap_find_prolog_culprit();
  extern yap_error_class_number Yap_errorClass(yap_error_number e);
  extern const char *Yap_errorName(yap_error_number e);
  extern const char *Yap_errorClassName(yap_error_class_number e);

  extern void Yap_pushErrorContext(yap_error_descriptor_t * new_error);
  extern yap_error_descriptor_t *Yap_popErrorContext(void);

#endif
