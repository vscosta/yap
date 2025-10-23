
#ifndef PY4YAP_H
#define PY4YAP_H 1

#include "Yap.h"


/** @defgroup Py4YAP  Prolog interface to python.
    @ingroup Python

    @{

Py4YAP is a package designed to embedd  Python objects into Prolog programs.

The interface should activates by consulting the `python`  library. It
 boots a Python image.
 ```
:- use_module(library(python)).
```
Python is invoked through the operators :=/1 and := 2. Classes, methods and fields can be used through Python's standard Python. Python modules are impprted through a call to import_module/1. A simple example:
```
:- [library(python)].

main :-
    import_module( matplotlib.pyplot ),
    Plt = matplotlib.pyplot,
    := Plt.plot([1,2,3,4]),
    := Plt.ylabel(`some numbers`),
    := Plt.show().
```
*/

size_t get_p_int(PyObject *o, Py_ssize_t def) {
  if (o == NULL)
    return def;
  if (PyLong_Check(o)) {
    return PyLong_AsLong(o);
#if PY_MAJOR_VERSION < 3
  } else if (PyInt_Check(o)) {
    return PyInt_AsLong(o);
#endif
  }
  return def;
}

static inline foreign_t address_to_term(PyObject *pVal, term_t t) {
  term_t to = PL_new_term_ref(), t1 = PL_new_term_ref();
  PL_put_pointer(t1, (void *)pVal);
  PL_cons_functor(to, FUNCTOR_pointer1, t1);
  Py_INCREF(pVal);
  foreign_t rc = PL_unify(t, to);
  PL_reset_term_refs(to);
  return rc;
}



static inline int proper_ascii_string(const char *s) {
  unsigned char c;

  while ((c = *s++)) {
    if (c > 127)
      return FALSE;
  }
  return TRUE;
}

static inline PyObject *atom_to_python_string(term_t t) {
  // Yap_DebugPlWrite(YAP_GetFromSlot(t));        fprintf(stderr, " here I
  // am\n");
  const char *s = NULL;
  Term yapt = YAP_GetFromSlot(t);
  if (IsStringTerm(yapt))
    s = StringOfTerm(yapt);
  else if (IsAtomTerm(yapt))
    s = RepAtom(AtomOfTerm(yapt))->StrOfAE;
  else
    return NULL;
/* return __main__,s */
#if PY_MAJOR_VERSION < 3
  if (proper_ascii_string(s)) {
    return PyString_FromStringAndSize(s, strlen(s));
  } else
#endif
  {
    PyObject *pobj = PyUnicode_DecodeUTF8(s, strlen(s), NULL);
    // fprintf(stderr, "%s\n", s);
    return pobj;
  }
}

#define CHECK_CALL(ys, pArgs, pyDict)                                                \
  PyErr_Clear();\
  rc =  PyObject_Call(ys, pArgs, pyDict);				\
  if (rc == NULL || PyErr_Occurred()) {                                        PyErr_Print();                                                             \
    YEC(ys, pArgs, pyDict, __LINE__, __FILE__, __FUNCTION__);                                   \
    PyErr_Clear();                                                             \
  }

extern PyObject *YED2(PyObject *f, PyObject *a, PyObject *d, int line, const char *file, const char *code);

static inline PyObject *CALL_BIP2(PyObject *ys,PyObject * pArg1,PyObject * pArg2)				
{ PyObject *rc = PyObject_CallFunctionObjArgs(ys, pArg1, pArg2, NULL);			\
  if (rc == NULL || PyErr_Occurred()) {                                        \
    YED2(ys, pArg1, pArg2, __LINE__, __FILE__, __FUNCTION__);                                   \
    PyErr_Print();                                                             \
    PyErr_Clear();                                                             \
  }
  return rc;
}

#define CALL_BIP1(ys, pArg1)                                                \
  rc = PyObject_CallFunctionObjArgs(ys, pArg1, NULL);                                                                   \
  if (rc == NULL || PyErr_Occurred()) {                                        \
    YED1(ys, pArg1, __LINE__, __FILE__, __FUNCTION__);                                   \
    PyErr_Print();                                                             \
    PyErr_Clear();                                                             \
  }

#define CHECKNULL(t, rc)                                                       \
  (rc != NULL ? rc : YE(t, __LINE__, __FILE__, __FUNCTION__))
#define AOK(rc, err)                                                           \
  {                                                                            \
    if (!rc)                                                                   \
      YEM(#rc, __LINE__, __FILE__, __FUNCTION__);                              \
  }


extern PyObject *YED1(PyObject *f, PyObject *a, int line, const char *file, const char *code);
extern PyObject *YE(term_t  , int line, const char *file, const char *code);
extern PyObject *YEC(PyObject *c,PyObject *a ,PyObject *d , int line, const char *file, const char *code);
extern void YEM(const char *ex, int line, const char *file, const char *code);
extern void pyErrorHandler__(int line, const char *file, const char *code);

#define PyStart()   PyErr_Clear()


#define pyErrorHandler()                if (PyErr_Occurred()) {                                        PyErr_Print();       \
      pyErrorHandler__(__LINE__, __FILE__, __FUNCTION__);                      \
    }                                                                          \


#define pyErrorAndReturn(x)                                                    \
  { if (PyErr_Occurred()) {PyErr_Print();				\
      pyErrorHandler__(__LINE__, __FILE__, __FUNCTION__);                      \
    }                                                                          \
    return (x); }                        
                                      
// #define pyErrorAndReturn( x, y ) return x
extern PyObject *compound_to_pyeval(YAP_Term t, PyObject *context, bool cvt_t);
extern foreign_t assign_to_symbol(term_t t, PyObject *e);

extern foreign_t python_builtin(term_t out);

extern install_t install_pypreds(void);
extern install_t install_pl2pl(void);

X_API extern bool loadt_python(void);
X_API extern bool do_init_python(void);

extern PyObject *find_term_obj(PyObject *ob, YAP_Term *yt, bool eval);
//extern PyObject PyInit_yap(void);

extern PyObject *PythonLookup(const char *s, PyObject *o);

extern PyObject *assign_symbol(const char *s, PyObject *, PyObject *);


X_API extern PyObject *yap_to_python(Term t, bool eval, PyObject *o, bool cvt);
X_API extern PyObject *term_to_python(term_t t, bool eval, PyObject *o, bool cvt);

X_API extern bool Yap_create_prolog_flag(const char *name, bool writable,  YAP_Term ttype, Term v);

X_API extern foreign_t python_to_term(PyObject *pVal, term_t t);
  X_API extern bool python_assign(YAP_Term t, PyObject *exp, PyObject *context);
#define YAPPy_ThrowError(id, inp, ...)                                         \
  YAPPy_ThrowError__(__FILE__, __FUNCTION__, __LINE__, id, inp, __VA_ARGS__)

extern void YAPPy_ThrowError__(const char *file, const char *function,
                               int lineno, yap_error_number type, term_t where,
                               ...);
#endif

#endif

.
