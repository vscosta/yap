
#ifndef PY4YAP_H
#define PY4YAP_H 1

#include "Yap.h"

//@{

/** @brief  Prolog to Python library
 *
 *
 * Please look at python.pl for more information, and to real.pl and real.c
 * for related work.
 */

#ifdef _XOPEN_SOURCE
#undef _XOPEN_SOURCE // python does its own thing
#endif

#undef HAVE_LIBREADLINE

#undef _POSIX_C_SOURCE

#include <Python.h>

#include <frameobject.h>

#include <Yap.h>


#include <SWI-Prolog.h>
#ifdef HAVE_STAT
#undef HAVE_STATa
#endif
#include <assert.h>
#include <string.h>
#define EXTRA_MESSSAGES 1

#ifndef PYTHON_H

#define X_API
#define I_API
#define O_API

#define PYTHON_H 1

extern bool assign_obj(PyObject *ctx, PyObject *exp, YAP_Term lhs, bool eval);

extern PyObject *term_to_nametuple(const char *s, arity_t arity, PyObject *tuple);

#if DEBUG_MEMORY || 1
#define DebugPrintf(s, op) fprintf(stderr, "%s:%d: " s, __FILE__, __LINE__, op)
#else
#define DebugPrintf(s, op)
#endif

/**
 *  @brief A module is store as an objet plus a list of paths.
 */
#define PY_MAX_MODLEN 16
typedef struct s_mod_t {
  PyObject *mod;
  int length;
  YAP_Term names[PY_MAX_MODLEN];
} Py_mod;

extern X_API YAP_Term pythonToYAP(PyObject *pVal);

extern X_API PyObject *compound_to_pytree(YAP_Term t, PyObject *context, bool cvt);
  
extern X_API PyObject *string_to_python(const char *s, bool eval, PyObject *p0);
typedef YAP_Arity arity_t;
extern bool init_python_vfs(void);

#define  ATOM_true ((atom_t)AtomTrue)
extern Atom AtomTrue;
#define  ATOM_false ((atom_t)AtomFalse)
extern Atom AtomFalse;
#define  ATOM_colon ((atom_t)AtomColon)
extern Atom AtomColon;
#define  ATOM_dot ((atom_t)AtomDot)
extern Atom AtomDot;
#define  ATOM_none ((atom_t)AtomNone)
extern Atom AtomNone;
#define  ATOM_t ((atom_t)AtomT)
extern Atom AtomT;
#define    ATOM_comma ((atom_t)AtomComma)
extern Atom AtomComma;
#define  ATOM_builtin ((atom_t)AtomBuiltin)
extern Atom AtomBuiltin;
#define  ATOM_V ((atom_t)AtomV)
extern Atom AtomV;
#define  ATOM_A ((atom_t)AtomA)
extern Atom AtomA;
#define  ATOM_self ((atom_t)AtomSelf)
extern Atom AtomSelf;
#define  ATOM_nil AtoNnil
#define  ATOM_brackets ((atom_t)AtomBrackets)
extern Atom AtomBrackets;
#define  ATOM_curly_brackets ((atom_t)AtomCurlyBrackets)
extern Atom AtomCurlyBrackets;

#define  FUNCTOR_dollar1 ((functor_t)FunctorDollar)
extern Functor FunctorDollar;
#define  FUNCTOR_abs1 ((functor_t)FunctorAbs)
extern Functor FunctorAbs;
#define  FUNCTOR_all1 ((functor_t)FunctorAll)
extern Functor FunctorAll;
#define  FUNCTOR_any1 ((functor_t)FunctorAny)
extern Functor FunctorAny;
#define    FUNCTOR_as2  ((functor_t)FunctorFunctorAs)
extern Functor FunctorFunctorAs;
#define  FUNCTOR_bin1 ((functor_t)FunctorBin)
extern Functor FunctorBin;
#define  FUNCTOR_brackets1 ((functor_t)FunctorBrackets)
extern Functor FunctorBrackets;
#define  FUNCTOR_comma2 ((functor_t)FunctorComma)
extern Functor FunctorComma;
#define  FUNCTOR_dir1 ((functor_t)FunctorDir)
extern Functor FunctorDir;
#define  FUNCTOR_float1 ((functor_t)FunctorFloat)
extern Functor FunctorFloat;
#define  FUNCTOR_int1 ((functor_t)FunctorInt)
extern Functor FunctorInt;
#define  FUNCTOR_iter1 ((functor_t)FunctorIter1)
extern Functor FunctorIter1;
#define  FUNCTOR_iter2 ((functor_t)FunctorIter2)
extern Functor FunctorIter2;
#define  FUNCTOR_long1 ((functor_t)FunctorLong)
extern Functor FunctorLong;
#define  FUNCTOR_len1 ((functor_t)FunctorLen)
extern Functor FunctorLen;
#define  FUNCTOR_curly1 ((functor_t)FunctorCurly)
extern Functor FunctorCurly;
#define  FUNCTOR_ord1 ((functor_t)FunctorOrd)
extern Functor FunctorOrd;
#define  FUNCTOR_range1 ((functor_t)FunctorRange1)
extern Functor FunctorRange1;
#define  FUNCTOR_range2 ((functor_t)FunctorRange2)
extern Functor FunctorRange2;
#define  FUNCTOR_range3 ((functor_t)FunctorRange3)
extern Functor FunctorRange3;
#define  FUNCTOR_sum1 ((functor_t)FunctorSum)
extern Functor FunctorSum;
#define  FUNCTOR_pointer1 ((functor_t)FunctorPointer)
extern Functor FunctorPointer;
#define  FUNCTOR_object1 ((functor_t)FunctorObject)
extern Functor FunctorObject;
#define  FUNCTOR_py_object1 ((functor_t)FunctorPythonObject)
extern Functor FunctorPythonObject;
#define  FUNCTOR_complex2 ((functor_t)FunctorComplex)
extern Functor FunctorComplex;
#define  FUNCTOR_plus2 ((functor_t)FunctorPlus)
extern Functor FunctorPlus;
#define  FUNCTOR_sub2 ((functor_t)FunctorSub)
extern Functor FunctorSub;
#define  FUNCTOR_mul2 ((functor_t)FunctorMul)
extern Functor FunctorMul;
#define  FUNCTOR_div2 ((functor_t)FunctorDiv)
extern Functor FunctorDiv;
#define  FUNCTOR_hat2 ((functor_t)FunctorHat)
extern Functor FunctorHat;
#define  FUNCTOR_colon2 ((functor_t)FunctorColon)
extern Functor FunctorColon;
#define  FUNCTOR_comma2 ((functor_t)FunctorComma)
extern Functor FunctorComma;
#define  FUNCTOR_equal2 ((functor_t)FunctorEqual)
extern Functor FunctorEqual;
#define  FUNCTOR_sqbrackets2 ((functor_t)FunctorSqbrackets)
extern Functor FunctorSqbrackets;
#define  FUNCTOR_dot2 ((functor_t)FunctorDot)
extern Functor FunctorDot;
#define  FUNCTOR_var1 ((functor_t)FunctorDollarVar)
extern Functor FunctorDollarVar;

extern X_API PyObject *py_Main;
extern X_API PyObject *py_Yapex;
extern X_API PyObject *py_Atoms;
extern X_API PyObject *py_Context;
extern PyObject *Py_f2p;
extern PyObject *py_Sys, *py_Builtins;
#define py_ModDict PyImport_GetModuleDict()

extern bool set_item(YAP_Term yt, PyObject *o, PyObject *val, bool eval, bool cvt);

inline static bool legal_symbol(const char *s) {
  int ch;
  while (((ch = *s++) != '\0')) {
    if (isalnum(ch) || ch == '_')
      continue;
    return false;
  }
  return true;
}

extern X_API PyObject *py_OpMap;

extern X_API bool python_in_python;
extern bool pyStringToString;

extern bool  python_release_GIL(term_t gstate);
extern term_t python_acquire_GIL(void);

static inline Py_ssize_t get_p_int(PyObject *o, Py_ssize_t def) {
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
  if (rc == NULL || PyErr_Occurred()) {                                        \
    YEC(ys, pArgs, pyDict, __LINE__, __FILE__, __FUNCTION__);                                   \
    PyErr_Print();                                                             \
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
