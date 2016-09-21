/* example.i */
%module(directors = "1") yap

// Language independent exception handler
%include exception.i 
%include stdint.i

%ignore *::operator[];

class YAPPredicate;
class YAPEngine;

#define arity_t uintptr_t

#ifdef SWIGPYTHON

%typemap(out) YAPTerm {
  if ($1.handle() == 0) {
    return NULL;
  }
}
%extend(out) YAPTerm{YAPTerm & __getitem__(size_t i){Term t0 = $self->term();

if (IsApplTerm(t0)) {
  Functor f = FunctorOfTerm(t0);
  if (!IsExtensionFunctor(f))
    return *new YAPTerm(ArgOfTerm(i + 1, t0));
} else if (IsPairTerm(t0)) {
  if (i == 0)
    return *new YAPTerm(HeadOfTerm(t0));
  else if (i == 1)
    return *new YAPTerm(TailOfTerm(t0));
}
return *new YAPTerm();
}
}

%typemap(out) YAPIntegerTerm {
  Term t = $1.term();
  Int j = IntegerOfTerm(t);
#if PY_MAJOR_VERSION < 3
  return PyInt_FromLong(j);
#else
  return PyLong_FromLong(j);
#endif
}

%typemap(out) YAPFloatTerm {
  Term t = $1.term();
  Int double j = FloatOfTerm(t);
  return PyFloat_FromDouble(j);
}

// translate well-known names and existing
// Python symbols
// Everthing else let wrapped.
// as a term
%typemap(out) YAPAtomTerm {
  const char *s = RepAtom(AtomOfTerm($1.term()))->StrOfAE;
  PyObject *p;
  if ((p = AtomToPy(s))) {
    return p;
  }
}

// translate lists as Python Lists
// Python symbols
// Everthing else let wrapped.
// as a term
%typemap(out) YAPListTerm {
  Term l = $1.term(), *end;
  PyObject *list;
  Int len = Yap_SkipList(&l, &end);
  $result = list = PyList_New(len);
  for (Int i = 0; i < len; i++) {
    Term a = HeadOfTerm(l);
    YAPTerm *argp1 = new YAPTerm(a);
    PyObject *obj0 =
        SWIG_NewPointerObj(SWIG_as_voidptr(argp1), SWIGTYPE_p_YAPTerm, 0 | 0);
    l = TailOfTerm(l);
    PyList_SetItem(list, i, obj0);
  }
  return list;
}

// Language independent exception handler

%exception {
  try {
    $action
  } catch (YAPError e) {
    switch (e.getErrorClass()) {
    case YAPC_NO_ERROR:
      break;
    /// bad domain, "first argument often is the predicate.
    case DOMAIN_ERROR: {
      yap_error_number en = e.getID();
      switch (en) {
      case DOMAIN_ERROR_OUT_OF_RANGE:
      case DOMAIN_ERROR_NOT_LESS_THAN_ZERO:
        PyErr_SetString(PyExc_IndexError, e.text());
        break;
      case DOMAIN_ERROR_CLOSE_OPTION:
      case DOMAIN_ERROR_ENCODING:
      case DOMAIN_ERROR_PROLOG_FLAG:
      case DOMAIN_ERROR_ABSOLUTE_FILE_NAME_OPTION:
      case DOMAIN_ERROR_READ_OPTION:
      case DOMAIN_ERROR_SET_STREAM_OPTION:
        PyErr_SetString(PyExc_KeyError, e.text());
        break;
      case DOMAIN_ERROR_FILE_ERRORS:
      case DOMAIN_ERROR_FILE_TYPE:
      case DOMAIN_ERROR_IO_MODE:
      case DOMAIN_ERROR_SOURCE_SINK:
      case DOMAIN_ERROR_STREAM_POSITION:
        PyErr_SetString(PyExc_IOError, e.text());
        break;
      default:
        PyErr_SetString(PyExc_ValueError, e.text());
      }
    } break;
    /// bad arithmetic
    case EVALUATION_ERROR: {
      yap_error_number en = e.getID();
      switch (en) {
      case EVALUATION_ERROR_FLOAT_OVERFLOW:
      case EVALUATION_ERROR_INT_OVERFLOW:
        PyErr_SetString(PyExc_OverflowError, e.text());
        break;
      case EVALUATION_ERROR_FLOAT_UNDERFLOW:
      case EVALUATION_ERROR_UNDERFLOW:
      case EVALUATION_ERROR_ZERO_DIVISOR:
        PyErr_SetString(PyExc_ArithmeticError, e.text());
        break;
      default:
        PyErr_SetString(PyExc_RuntimeError, e.text());
      }
    } break;
    /// missing object (I/O mostly)
    case EXISTENCE_ERROR:
      PyErr_SetString(PyExc_NotImplementedError, e.text());
      break;
    /// should be bound
    case INSTANTIATION_ERROR_CLASS:
      PyErr_SetString(PyExc_RuntimeError, e.text());
      break;
    /// bad access, I/O
    case PERMISSION_ERROR: {
      yap_error_number en = e.getID();
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
        PyErr_SetString(PyExc_OverflowError, e.text());
        break;
      default:
        PyErr_SetString(PyExc_RuntimeError, e.text());
      }
    } break;
    /// something that could not be represented into a type
    case REPRESENTATION_ERROR:
      PyErr_SetString(PyExc_RuntimeError, e.text());
      break;
    /// not enough ....
    case RESOURCE_ERROR:
      PyErr_SetString(PyExc_RuntimeError, e.text());
      break;
    /// bad text
    case SYNTAX_ERROR_CLASS:
      PyErr_SetString(PyExc_SyntaxError, e.text());
      break;
    /// OS or internal
    case SYSTEM_ERROR_CLASS:
      PyErr_SetString(PyExc_RuntimeError, e.text());
      break;
    /// bad typing
    case TYPE_ERROR:
      PyErr_SetString(PyExc_TypeError, e.text());
      break;
    /// should be unbound
    case UNINSTANTIATION_ERROR_CLASS:
      PyErr_SetString(PyExc_RuntimeError, e.text());
      break;
    /// escape hatch
    default:
      break;
    }
  }
}

#else

// Language independent exception handler
%include exception.i

    %exception {
  try {
    $action
  } catch (YAPError e) {
    switch (e.getErrorClass()) {
    case YAPC_NO_ERROR:
      break;
    /// bad domain, "first argument often is the predicate.
    case DOMAIN_ERROR: {
      yap_error_number en = e.getID();
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
      yap_error_number en = e.getID();
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
      yap_error_number en = e.getID();
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

#define YAP_CPP_INTERFACE 1

#include "yapi.hh"

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

// %include "yapi.hh"

%include "yapa.hh"

    %include "yapie.hh"

    %include "yapt.hh"

    %include "yapdb.hh"

    %include "yapq.hh"

%init %{
#ifdef SWIGPYTHON
  init_python();
#endif
%}
