
#include <iostream>
#include <string>
#include <vector>
#include <list>
#include <unordered_map>

#define _YAP_NOT_INSTALLED_ 1
//#define YAP_CPP_INTERFACE 1


extern "C" {


#include "inline-only.h"
#define _EXPORT_KERNEL 1
}


#include "yapi.hh"

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

typedef std::unordered_map<std::string,Term> class_map;

class_map cxx;


