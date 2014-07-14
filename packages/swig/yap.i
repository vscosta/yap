/* example.i */
 %module(directors="1") yap

 %{
 /* Put header files here or function declarations like below */
 
#define YAP_CPP_INTERFACE 1
 
extern "C" {

#include "yapi.hh"

  extern Term Yap_StringToTerm(const char *s, size_t len, term_t bindings);

#if THREADS
#define Yap_regp regcache
#endif

  // we cannot consult YapInterface.h, that conflicts with what we declare, though
  // it shouldn't
}

 %}
 
 
/* turn on director wrapping Callback */
%feature("director") YAPCallback;
 	
class YAPPredicate;

#ifdef SWIGPYTHON
%exception YAPPredicate {
   try {
      $action
   } catch (...) {
      PyErr_SetString(PyExc_SyntaxError, "syntax error");
      return NULL;
   }
}
#endif


%include "yapi.hh"

#ifdef SWIGJAVA
%javaexception("java.text.ParseException") YAPPredicate {
  try {
     $action
  } catch (YAPError::SYNTAX_ERROR &e) {
    jclass clazz = jenv->FindClass("java/text/ParseException");
    jenv->ThrowNew(clazz, "Syntax error");
    return $null;
   }
}
#endif

