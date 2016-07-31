/* example.i */
%module(directors="1") yap

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
%extend(out) YAPTerm {
  YAPTerm & __getitem__ (size_t i) {
    Term t0 = $self->term();

    if (IsApplTerm(t0)) {
      Functor f = FunctorOfTerm(t0);
      if (!IsExtensionFunctor(f))
	return *new YAPTerm(ArgOfTerm(i + 1, t0));
    } else if (IsPairTerm(t0)) {
      if (i==0)
	return * new YAPTerm(HeadOfTerm(t0));
      else if (i==1)
	return * new YAPTerm(TailOfTerm(t0));
    }
    return * new YAPTerm();
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
  Int     double j = FloatOfTerm(t);
  return PyFloat_FromDouble(j);
 }

// translate well-known names and existing
// Python symbols
// Everthing else let wrapped.
// as a term
%typemap(out) YAPAtomTerm {
  const char *s  = RepAtom(AtomOfTerm($1.term()))->StrOfAE;
  PyObject *p;
  if ((p = AtomToPy( s))) {
    return p;
  }
 }


// translate lists as Python Lists
// Python symbols
// Everthing else let wrapped.
// as a term
%typemap(out) YAPListTerm {
  Term l =$1.term(), *end;
  PyObject *list;
  Int len = Yap_SkipList( & l, &end );
  $result = list = PyList_New( len );
  for (Int i = 0 ; i < len ; i++) {
    Term a = HeadOfTerm(l);
    YAPTerm *argp1 = new YAPTerm( a );
    PyObject *obj0 = 
      SWIG_NewPointerObj(SWIG_as_voidptr(argp1), SWIGTYPE_p_YAPTerm, 0 |  0 );
    l = TailOfTerm(l);
    PyList_SetItem( list, i, obj0 );
  }
  return list;
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



%init %{
#ifdef SWIGPYTHON
  init_python();
#endif
  %}
