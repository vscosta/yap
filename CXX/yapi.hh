
#define YAP_CPP_INTERFACE 1

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

// Bad export from Python
#ifdef HAVE_STAT
#undef HAVE_STAT
#endif
#include <config.h>

#if USE_GMP
#include <gmpxx.h>  
#endif

extern "C" {

#include <stddef.h>
  
#include "Yap.h"

#include "Yatom.h"

#include "YapHeap.h"

#include "pl-shared.h"

#include "clause.h"

#include "yapio.h"

#include "Foreign.h"

#include "attvar.h"

#include "SWI-Stream.h"

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
#include <windows.h>
#endif


// taken from yap_structs.h
#include "iopreds.h"

  extern Term Yap_StringToTerm(const char *s, size_t len, term_t bindings);

  // we cannot consult YapInterface.h, that conflicts with what we declare, though
  // it shouldn't
}


//#include <vector>


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
