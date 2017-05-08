/**
   @file yapi,hh

   @brief entry file for the YAP C++ interface

*/


#define YAP_CPP_INTERFACE 1

#include <gmpxx.h>
#include <vector>
#include <string>
#include <iostream>

/*!
 *
 *   @ingroup fli_c_cx
 *   @defgroup yap-cplus-interface An object oriented interface for YAP.
 *
 *   @{         
 *
 *
 * @brief C++ wrapper to terms, predicates and queries
 *
 * This new interface is designed to be object oriented and to fit
 * naturally with the swig interface language generator. It uses ideas
 * from the old YAP interface and from the SWI foreign language
 * interface.
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

  X_API extern void YAP_UserCPredicate(const char *, YAP_UserCPred, YAP_Arity arity);

  /*  extern void UserCPredicateWithArgs(const char *name, int *fn(), unsigned int arity)
   */
  X_API extern void YAP_UserCPredicateWithArgs(const char *, YAP_UserCPred, YAP_Arity,
					       YAP_Term);

  X_API extern void UserBackCPredicate(const char *name, int *init(), int *cont(), int
				       arity, int extra);
#if YAP_PYTHON

#include <Python.h>
  
  extern bool  python_in_python;
#endif
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
