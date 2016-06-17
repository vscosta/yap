/* example.i */
 %module(directors="1") yap

// Language independent exception handler
%include exception.i

 class YAPPredicate;
 class YAPEngine;


#ifdef SWIGPYTHON
%typemap(out) YAPTerm {
  return term_to_python( $1.handle() );
}
%typemap(out) YAPListTerm {
  return term_to_python( $1.handle() );
}
%typemap(out) YAPAtomTerm {
  return term_to_python( $1.handle() );
}
%typemap(out) YAPIntegerTerm {
  return term_to_python( $1.handle() );
}

%exception YAPPredicate {
   try {
      $action
   } catch (...) {
      PyErr_SetString(PyExc_SyntaxError, "syntax error");
      return NULL;
   }
}
#endif


%exception query {
    try {
        $action
    }
    catch (YAPError YAP_SYMTAX_ERROR) {
        SWIG_exception(SWIG_SyntaxError,"Syntax Error exception");
    }
    catch (...) {
        SWIG_exception(SWIG_RuntimeError,"Unknown exception");
	    }
	}

%exception next {
    try {
        $action
    }
    catch (...) {
        SWIG_exception(SWIG_RuntimeError,"Unknown exception");
    }
}


 %{
 /* Put header files here or function declarations like below */

#define YAP_CPP_INTERFACE 1


#include "yapi.hh"

  extern "C" {

#if THREADS
#define Yap_regp regcache
#endif

  // we cannot consult YapInterface.h, that conflicts with what we declare, though
  // it shouldn't
}

 %}


/* turn on director wrapping Callback */
%feature("director") YAPCallback;


// %include "yapi.hh"

%include "yapa.hh"

%include "yapie.hh"

%include "yapt.hh"

%include "yapdb.hh"

%include "yapq.hh"

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
