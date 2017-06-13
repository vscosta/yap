

%{

#include <cmath>
#include <gmpxx.h>
  extern "C"{
#ifdef SWIGPYTHON
#include <Python.h>
#endif
#include  "Yap.h"
  }
  %}

/* example.i */
%module(directors = "1") yap

 // Language independent exception handler
%include exception.i
%include stdint.i
%include std_string.i
%include std_vector.i

namespace std {
  %template(vectort) vector<Term>;
};

   %feature("novaluewrapper") std::vector<Term>;    

%ignore *::operator[];

class YAPPredicate;
class YAPEngine;

%{

#include "yapi.hh"

#ifdef SWIGPYTHON

  extern "C" {

    extern X_API YAP_Term pythonToYAP(PyObject *pVal);
    extern X_API PyObject  * yap_to_python(YAP_Term t, bool eval, PyObject *ctx);
    X_API extern bool init_python(void);
    extern X_API PyObject *py_Main;
    extern X_API PyObject *py_Builtin;
    extern X_API PyObject *py_Yapex;
  }

  extern inline PyObject *AtomToPy(const char *s) {
    if (strcmp(s, "true") == 0)
      return Py_True;
    if (strcmp(s, "false") == 0)
      return Py_False;
    if (strcmp(s, "none") == 0)
      return Py_None;
    if (strcmp(s, "[]") == 0)
      return PyList_New(0);
    else if (strcmp(s, "{}") == 0)
      return PyDict_New();
    /* return __main__,s */
    else if (PyObject_HasAttrString(py_Main, s)) {
      return PyObject_GetAttrString(py_Main, s);
    }
    // no way to translate
    return NULL;
  }
#endif
  %}

#ifdef SWIGPYTHON
%typemap(typecheck) Term*  {
  $1 = PySequence_Check($input);
 }

// Map a Python sequence into any sized C double array
%typemap(in) Term*  {
  int i;
  if (!PySequence_Check($input)) {
    PyErr_SetString(PyExc_TypeError,"Expecting a sequence");
    $1 = nullptr;
  } else {
    int sz = PyObject_Length($input);
    std::vector<Term> v(sz);
    for (i =0; i < sz; i++) {
      PyObject *o = PySequence_GetItem($input,i);
      v[i] = Term(pythonToYAP(o));
      //Py_DECREF(o);
    }
    $1 = &v[0];
  }
 }


//%typemap(check) YAPEngine::fun(Term) { $1 = 1; }
//%typemap(check) YAPEngine::fun(YAPTerm) { $1 = 0; }


%typemap(in) Int { if (PyLong_Check($input)) {
    $1 = PyLong_AsLong($input);}   }

//%typemap(in) double { if (PyFloat_Check($input)) {
//    $1 = PyFloat_AsDouble($input); }   }

//%typemap(in) char const * { if (PyUnicode_Check($input)) {
//    $1 = PyUnicode_AsUTF8($input); }   }

//%typemap(in) YAPTerm { $1 = new YAPTerm(pythonToYAP($input));   PyErr_Clear(); }
%typemap(in) YAP_Term { $1 = pythonToYAP($input);   PyErr_Clear(); }
%typemap(in) Term {   $1 = pythonToYAP($input);   PyErr_Clear(); }
%typemap(in) YAPTerm {   YAPTerm(($1 = pythonToYAP($input)));   PyErr_Clear(); }

%typecheck(2) Int { $1 = PyLong_Check($input); }
%typecheck(3) double { $1 = PyFloat_Check($input); }
%typecheck(2) const char * { $1 = PyUnicode_Check($input); }

     %typecheck(1) Term { $1 = !PyUnicode_Check($input); }
     %typecheck(1) YAP_Term { $1 = PyUnicode_Check($input); }

%typecheck(0) YAPTerm { $1 = !PyUnicode_Check($input); }


%typemap(out) YAP_Term {  return $result = yap_to_python($1, false, 0);    }

%typemap(out) Term {  return $result = yap_to_python($1, false, 0);  }

%typemap(out) std::vector<Term> {
  size_t len = $1.size();
$result = PyList_New(len);
for (size_t i = 0; i< len; i++) {
PyObject *o = yap_to_python($1[i],false,0);
PyList_SetItem($result,i,o);

}
return $result;  }



  // Language independent exception handler

  %exception next {
    try {
      $action
	} catch (YAPError &e) {
      yap_error_number en = e.getID();
      PyObject *pyerr = PyExc_RuntimeError;

      LOCAL_Error_TYPE = YAP_NO_ERROR;
      switch (e.getErrorClass()) {
      case YAPC_NO_ERROR:
	break;
	/// bad domain, "first argument often is the predicate.
      case DOMAIN_ERROR: {
	switch (en) {
	case DOMAIN_ERROR_OUT_OF_RANGE:
    pyerr = PyExc_GeneratorExit;
    break;
case DOMAIN_ERROR_NOT_LESS_THAN_ZERO:
	  pyerr = PyExc_IndexError;
	  break;
	case DOMAIN_ERROR_CLOSE_OPTION:
	case DOMAIN_ERROR_ENCODING:
	case DOMAIN_ERROR_PROLOG_FLAG:
	case DOMAIN_ERROR_ABSOLUTE_FILE_NAME_OPTION:
	case DOMAIN_ERROR_READ_OPTION:
	case DOMAIN_ERROR_SET_STREAM_OPTION:
	  pyerr = PyExc_KeyError;
	  break;
	case DOMAIN_ERROR_FILE_ERRORS:
	case DOMAIN_ERROR_FILE_TYPE:
	case DOMAIN_ERROR_IO_MODE:
	case DOMAIN_ERROR_SOURCE_SINK:
	case DOMAIN_ERROR_STREAM_POSITION:
	  pyerr = PyExc_IOError;
	  break;
	default:
	  pyerr = PyExc_ValueError;
	}
      } break;
	/// bad arithmetic
      case EVALUATION_ERROR: {
	switch (en) {
	case EVALUATION_ERROR_FLOAT_OVERFLOW:
	case EVALUATION_ERROR_INT_OVERFLOW:
	  pyerr = PyExc_OverflowError;
	  break;
	case EVALUATION_ERROR_FLOAT_UNDERFLOW:
	case EVALUATION_ERROR_UNDERFLOW:
	case EVALUATION_ERROR_ZERO_DIVISOR:
	  pyerr = PyExc_ArithmeticError;
	  break;
	default:
	  pyerr = PyExc_RuntimeError;
	}
      } break;
	/// missing object (I/O mostly)
      case EXISTENCE_ERROR:
	pyerr = PyExc_NotImplementedError;
	break;
	/// should be bound
      case INSTANTIATION_ERROR_CLASS:
	pyerr = PyExc_RuntimeError;
	break;
	/// bad access, I/O
      case PERMISSION_ERROR: {
	switch (en) {
	case PERMISSION_ERROR_INPUT_BINARY_STREAM:
	case PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM:
	case PERMISSION_ERROR_INPUT_STREAM:
	case PERMISSION_ERROR_INPUT_TEXT_STREAM:
	case PERMISSION_ERROR_OPEN_SOURCE_SINK:
	case PERMISSION_ERROR_OUTPUT_BINARY_STREAM:
	case PERMISSION_ERROR_REPOSITION_STREAM:
	case PERMISSION_ERROR_OUTPUT_STREAM:
	case PERMISSION_ERROR_OUTPUT_TEXT_STREAM:
	  pyerr = PyExc_OverflowError;
	  break;
	default:
	  pyerr = PyExc_RuntimeError;
	}
      } break;
	/// something that could not be represented into a type
      case REPRESENTATION_ERROR:
	pyerr = PyExc_RuntimeError;
	break;
	/// not enough ....
      case RESOURCE_ERROR:
	pyerr = PyExc_RuntimeError;
	break;
	/// bad text
      case SYNTAX_ERROR_CLASS:
	pyerr = PyExc_SyntaxError;
	break;
	/// OS or internal
      case SYSTEM_ERROR_CLASS:
	pyerr = PyExc_RuntimeError;
	break;
	/// bad typing
      case TYPE_ERROR:
	pyerr = PyExc_TypeError;
	break;
	/// should be unbound
      case UNINSTANTIATION_ERROR_CLASS:
	pyerr = PyExc_RuntimeError;
	break;
	/// escape hatch
      default:
	break;
      }
      PyErr_SetString(pyerr, e.text().c_str());
    }
  }

#else

  // Language independent exception handler
  %include exception.i

     %exception {
    try {
      $action
	} catch (YAPError e) {
      yap_error_number en = e.getID();
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      switch (e.getErrorClass()) {
      case YAPC_NO_ERROR:
	break;
	/// bad domain, "first argument often is the predicate.
      case DOMAIN_ERROR: {
	switch (en) {
	case DOMAIN_ERROR_OUT_OF_RANGE:
	case DOMAIN_ERROR_NOT_LESS_THAN_ZERO:
	  SWIG_exception(SWIG_IndexError, e.text());
	  break;
	case DOMAIN_ERROR_CLOSE_OPTION:
	case DOMAIN_ERROR_ENCODING:
	case DOMAIN_ERROR_PROLOG_FLAG:
	case DOMAIN_ERROR_ABSOLUTE_FILE_NAME_OPTION:
	case DOMAIN_ERROR_READ_OPTION:
	case DOMAIN_ERROR_SET_STREAM_OPTION:
	  SWIG_exception(SWIG_AttributeError, e.text());
	  break;
	case DOMAIN_ERROR_FILE_ERRORS:
	case DOMAIN_ERROR_FILE_TYPE:
	case DOMAIN_ERROR_IO_MODE:
	case DOMAIN_ERROR_SOURCE_SINK:
	case DOMAIN_ERROR_STREAM_POSITION:
	  SWIG_exception(SWIG_IOError, e.text());
	  break;
	default:
	  SWIG_exception(SWIG_ValueError, e.text());
	}
      } break;
	/// bad arithmetic
      case EVALUATION_ERROR: {
	switch (en) {
	case EVALUATION_ERROR_FLOAT_OVERFLOW:
	case EVALUATION_ERROR_FLOAT_UNDERFLOW:
	case EVALUATION_ERROR_INT_OVERFLOW:
	case EVALUATION_ERROR_UNDERFLOW:
	  SWIG_exception(SWIG_OverflowError, e.text());
	  break;
	case EVALUATION_ERROR_ZERO_DIVISOR:
	  SWIG_exception(SWIG_DivisionByZero, e.text());
	  break;
	default:
	  SWIG_exception(SWIG_RuntimeError, e.text());
	}
      } break;
	/// missing object (I/O mostly)
      case EXISTENCE_ERROR:
	SWIG_exception(SWIG_RuntimeError, e.text());
	break;
	/// should be bound
      case INSTANTIATION_ERROR_CLASS:
	SWIG_exception(SWIG_RuntimeError, e.text());
	break;
	/// bad access, I/O
      case PERMISSION_ERROR: {
	switch (en) {
	case PERMISSION_ERROR_INPUT_BINARY_STREAM:
	case PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM:
	case PERMISSION_ERROR_INPUT_STREAM:
	case PERMISSION_ERROR_INPUT_TEXT_STREAM:
	case PERMISSION_ERROR_OPEN_SOURCE_SINK:
	case PERMISSION_ERROR_OUTPUT_BINARY_STREAM:
	case PERMISSION_ERROR_REPOSITION_STREAM:
	case PERMISSION_ERROR_OUTPUT_STREAM:
	case PERMISSION_ERROR_OUTPUT_TEXT_STREAM:
	  SWIG_exception(SWIG_OverflowError, e.text());
	  break;
	default:
	  SWIG_exception(SWIG_RuntimeError, e.text());
	}
      } break;
	/// something that could not be represented into a type
      case REPRESENTATION_ERROR:
	SWIG_exception(SWIG_RuntimeError, e.text());
	break;
	/// not enough ....
      case RESOURCE_ERROR:
	SWIG_exception(SWIG_RuntimeError, e.text());
	break;
	/// bad text
      case SYNTAX_ERROR_CLASS:
	SWIG_exception(SWIG_SyntaxError, e.text());
	break;
	/// OS or internal
      case SYSTEM_ERROR_CLASS:
	SWIG_exception(SWIG_RuntimeError, e.text());
	break;
	/// bad typing
      case TYPE_ERROR:
	SWIG_exception(SWIG_TypeError, e.text());
	break;
	/// should be unbound
      case UNINSTANTIATION_ERROR_CLASS:
	SWIG_exception(SWIG_RuntimeError, e.text());
	break;
	/// escape hatch
      default:
	break;
      }
    }
  }

#endif

  %{
    /* Put header files here or function declarations like below */


    extern "C" {

#if THREADS
#define Yap_regp regcache
#endif

      // we cannot consult YapInterface.h, that conflicts with what we
      // declare, though
      // it shouldn't
    }

    %}

  /* turn on director wrapping Callback */
  %feature("director") YAPCallback;

  %include "yapa.hh"

     %include "yapie.hh"

     %include "yapt.hh"

     %include "yapdb.hh"

     %include "yapq.hh"

     %init %{
    %}
