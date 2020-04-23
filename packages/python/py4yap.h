
#undef PASS_REGS
#undef USES_REGS

#ifndef PY4YAP_H
#define PY4YAP_H 1

#define PASS_REGS
#define USES_REGS

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


#undef _POSIX_C_SOURCE

#include <Python.h>

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

PyObject *find_obj(PyObject *ctx, PyObject *exp, term_t lhs, bool eval);

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


extern X_API PyObject *string_to_python(const char *s, bool eval, PyObject *p0);
typedef YAP_Arity arity_t;
extern bool init_python_vfs(void);

extern atom_t ATOM_true, ATOM_false, ATOM_colon, ATOM_dot, ATOM_none, ATOM_t,
    ATOM_comma, ATOM_builtin, ATOM_V, ATOM_A, ATOM_self, ATOM_nil,
  ATOM_brackets, ATOM_curly_brackets;

extern functor_t FUNCTOR_dollar1, FUNCTOR_abs1, FUNCTOR_all1, FUNCTOR_any1,
    FUNCTOR_as2, FUNCTOR_bin1, FUNCTOR_brackets1, FUNCTOR_comma2, FUNCTOR_dir1,
  FUNCTOR_float1, FUNCTOR_int1, FUNCTOR_iter1, FUNCTOR_var1, FUNCTOR_iter2, FUNCTOR_long1, FUNCTOR_var1,
    FUNCTOR_len1, FUNCTOR_curly1, FUNCTOR_ord1, FUNCTOR_range1, FUNCTOR_range2,
    FUNCTOR_range3, FUNCTOR_sum1, FUNCTOR_pointer1, FUNCTOR_complex2,
    FUNCTOR_plus2, FUNCTOR_sub2, FUNCTOR_mul2, FUNCTOR_div2, FUNCTOR_hat2,
    FUNCTOR_colon2, FUNCTOR_comma2, FUNCTOR_equal2, FUNCTOR_sqbrackets2,
  FUNCTOR_dot2, FUNCTOR_var1;

extern X_API PyObject *py_Main;
extern X_API PyObject *py_Yapex;
extern X_API PyObject *py_Atoms;
extern X_API PyObject *py_Context;
extern PyObject *Py_f2p;
extern PyObject *py_Sys;
#define py_ModDict PyImport_GetModuleDict()
#define py_Local   PyEval_GetLocals()
#define py_Global  PyEval_GetGlobals()
#define py_Builtin PyEval_GetBuiltins()

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
  Term yapt = Yap_GetFromSlot(t);
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
  PyErr_Clear();                                                               \
  rc = PyObject_Call(ys, pArgs, pyDict);                                                                   \
  if (rc == NULL || PyErr_Occurred()) {                                        \
    YEC(ys, pArgs, pyDict, __LINE__, __FILE__, __FUNCTION__);                                   \
    PyErr_Print();                                                             \
    PyErr_Clear();                                                             \
  }

extern PyObject *YED2(PyObject *f, PyObject *a, PyObject *d, int line, const char *file, const char *code);

static inline PyObject *CALL_BIP2(PyObject *ys,PyObject * pArg1,PyObject * pArg2)				
{ PyErr_Clear();							\
  PyObject *rc = PyObject_CallFunctionObjArgs(ys, pArg1, pArg2, NULL);			\
  if (rc == NULL || PyErr_Occurred()) {                                        \
    YED2(ys, pArg1, pArg2, __LINE__, __FILE__, __FUNCTION__);                                   \
    PyErr_Print();                                                             \
    PyErr_Clear();                                                             \
  }
  return rc;
}

#define CALL_BIP1(ys, pArg1)                                                \
  PyErr_Clear();                                                               \
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


#define pyErrorHandler()                if (PyErr_Occurred()) {                                               \
      pyErrorHandler__(__LINE__, __FILE__, __FUNCTION__);                      \
    }                                                                          \


#define pyErrorAndReturn(x)                                                    \
    { if (PyErr_Occurred()) {                                                    \
      pyErrorHandler__(__LINE__, __FILE__, __FUNCTION__);                      \
    }                                                                          \
    return (x); }                        
                                      
// #define pyErrorAndReturn( x, y ) return x
extern PyObject *compound_to_pyeval(term_t t, PyObject *context, bool cvt);
extern PyObject *compound_to_pytree(term_t t, PyObject *context, bool cvt);

extern PyObject *term_to_python(term_t t, bool eval, PyObject *context,
                                bool eval_atom);

extern PyObject *term_to_nametuple(const char *s, arity_t arity, PyObject *);

extern foreign_t python_to_term(PyObject *pVal, term_t t);
extern bool python_assign(term_t t, PyObject *exp, PyObject *context);
extern foreign_t assign_to_symbol(term_t t, PyObject *e);

extern foreign_t python_builtin(term_t out);

extern PyObject *lookupPySymbol(const char *s, PyObject *q, PyObject **d);

extern install_t install_pypreds(void);
extern install_t install_pl2pl(void);

X_API extern bool init_python(void);
X_API extern bool loadt_python(void);
X_API extern bool do_init_python(void);

extern PyObject *find_term_obj(PyObject *ob, YAP_Term *yt, bool eval);
extern PyObject PyInit_yap(void);

extern PyObject *PythonLookup(const char *s, PyObject *o);

extern PyObject *PythonLookupSpecial(const char *s);


X_API extern PyObject *yap_to_python(Term t, bool eval, PyObject *o, bool cvt);

X_API extern bool Yap_create_prolog_flag(const char *name, bool writable,  YAP_Term ttype, Term v);

#define YAPPy_ThrowError(id, inp, ...)                                         \
  YAPPy_ThrowError__(__FILE__, __FUNCTION__, __LINE__, id, inp, __VA_ARGS__)

extern void YAPPy_ThrowError__(const char *file, const char *function,
                               int lineno, yap_error_number type, term_t where,
                               ...);
#endif

#endif
