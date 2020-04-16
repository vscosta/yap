
/**
   @file yapi.hh

   @brief entry file for the YAP C++ interface

*/

#define YAP_CPP_INTERFACE 1

#include <iostream>
#include <string>
#include <vector>

extern "C" {

#include "YapConfig.h"

}

#include <gmpxx.h>




/*!
 *
 *   @ingroup fli_c_cxx
 *   @defgroup yap-cplus-interface An object oriented interface for YAP.
 *
1 *   @{
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

extern "C" {

#include <stdlib.h>

// Bad export from Python

#include <YapConfig.h>

#include <stddef.h>

#if YAP_PYTHON

#include <Python.h>

extern bool python_in_python;
#endif

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

X_API extern void YAP_UserCPredicate(const char *, YAP_UserCPred,
                                     YAP_Arity arity);

/*  extern void UserCPredicateWithArgs(const char *name, int *fn(), unsigned int
 * arity)
 */
X_API extern void YAP_UserCPredicateWithArgs(const char *, YAP_UserCPred,
                                             YAP_Arity, YAP_Term);

X_API extern void YAP_UserBackCPredicate(const char *name, YAP_UserCPred init,
                                         YAP_UserCPred cont, YAP_Arity arity,
                                         YAP_Arity extra);

X_API extern void YAP_UserBackCutCPredicate(const char *name,
                                            YAP_UserCPred init,
                                            YAP_UserCPred cont,
                                            YAP_UserCPred cut, YAP_Arity arity,
                                            YAP_Arity extra);

X_API extern YAP_Term YAP_ReadBuffer(const char *s, YAP_Term *tp);

extern YAP_Term YAP_MkcharPTerm(char *s);
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
