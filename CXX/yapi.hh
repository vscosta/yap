

#define YAP_CPP_INTERFACE 1

#include <gmpxx.h>


//! @{
/**
 *
 *   @defgroup yap-cplus-interface An object oriented interface for YAP.
 *
 *   @ingroup ChYInterface
 *    @tableofcontents
 *
 *
 * C++ interface to YAP. Designed to be object oriented and to fit naturally
 * with the swig interface language generator. It uses ideas from the old YAP
 * interface and from the SWI foreign language interface.
 *
 */
#include <stdlib.h>

#include <string>

// Bad export from Python

#include <config.h>

extern "C" {

#include <stddef.h>

#include "Yap.h"

#include "Yatom.h"

#include "YapHeap.h"

#include "clause.h"

#include "yapio.h"

#include "Foreign.h"

#include "attvar.h"

#include "YapText.h"

#if HAVE_STDARG_H
#include <stdarg.h>
#endif

#if HAVE_STDINT_H
#include <stdint.h>
#endif

#if HAVE_STRING_H
#include <string.h>
#endif

#if _MSC_VER || defined(__MINGW32__)
//#include <windows.h>
#endif

// taken from yap_structs.h
#include "iopreds.h"

#ifdef SWIGPYTHON
extern  PyObject *term_to_python(yhandle_t t);

#endif
  
X_API void YAP_UserCPredicate(const char *, YAP_UserCPred, YAP_Arity arity);

/*  void UserCPredicateWithArgs(const char *name, int *fn(), unsigned int arity) */
X_API void YAP_UserCPredicateWithArgs(const char *, YAP_UserCPred, YAP_Arity, YAP_Term);

/*  void UserBackCPredicate(const char *name, int *init(), int *cont(), int
     arity, int extra) */
 X_API void YAP_UserBackCPredicate(const char *, YAP_UserCPred, YAP_UserCPred, YAP_Arity, YAP_Arity);

X_API Term Yap_StringToTerm(const char *s, size_t len, encoding_t *encp, int prio, Term *bindings_p);

  
}

class YAPEngine;
class YAPAtom;
class YAPFunctor;
class YAPApplTerm;
class YAPPairTerm;
class YAPQuery;
class YAPModule;
class YAPError;
class YAPPredicate;

#include "yapa.hh"

#include "yapie.hh"

#include "yapt.hh"

#include "yapdb.hh"

#include "yapq.hh"

/**
 * @}
 *
 */
