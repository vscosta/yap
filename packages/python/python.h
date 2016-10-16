#ifdef _XOPEN_SOURCE
#undef _XOPEN_SOURCE // python does its own thing
#endif
#include <Python.h>
#include <SWI-Prolog.h>
#ifdef HAVE_STAT
#undef HAVE_STATa
#endif
#include <assert.h>
#define EXTRA_MESSSAGES 1

//@{

/** @brief  Prolog to Python library
*
*
* Please look at python.pl for more information, and to real.pl and real.c
* for related work.
*/

typedef YAP_Arity arity_t;

extern atom_t ATOM_true, ATOM_false, ATOM_colon, ATOM_dot, ATOM_none, ATOM_t,
  ATOM_comma, ATOM_builtin, ATOM_V, ATOM_A, ATOM_self;

extern functor_t FUNCTOR_dollar1, FUNCTOR_abs1, FUNCTOR_all1, FUNCTOR_any1,
    FUNCTOR_bin1, FUNCTOR_brackets1, FUNCTOR_comma2, FUNCTOR_dir1,
    FUNCTOR_float1, FUNCTOR_int1, FUNCTOR_iter1, FUNCTOR_iter2, FUNCTOR_long1,
    FUNCTOR_len1, FUNCTOR_curly1, FUNCTOR_ord1, FUNCTOR_range1, FUNCTOR_range2,
    FUNCTOR_range3, FUNCTOR_sum1, FUNCTOR_pointer1, FUNCTOR_complex2,
    FUNCTOR_plus2, FUNCTOR_sub2, FUNCTOR_mul2, FUNCTOR_div2, FUNCTOR_hat2,
    FUNCTOR_colon2, FUNCTOR_comma2, FUNCTOR_equal2, FUNCTOR_sqbrackets2,
    FUNCTOR_dot2;

extern PyObject *py_Main;
extern PyObject *py_Builtin;
extern PyObject *py_Yapex;
extern PyObject *py_F2P;

extern bool python_in_python;

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
  char *s;
  if (!PL_get_atom_chars(t, &s))
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

extern PyObject *compound_to_pyeval(term_t t, functor_t fun);
extern PyObject *compound_to_pytree(term_t t, functor_t fun);


extern PyObject *yap_to_python(YAP_Term t, bool eval);
extern PyObject *term_to_python(term_t t, bool eval);
extern foreign_t python_to_ptr(PyObject *pVal, term_t t);

foreign_t python_to_term(PyObject *pVal, term_t t);
foreign_t assign_to_symbol(term_t t, PyObject *e);

int assign_python(PyObject *root, term_t t, PyObject *e);

extern foreign_t python_builtin(term_t out);

extern PyObject *ActiveModules[32];
extern int active_modules;

extern install_t install_pypreds(void);
extern install_t install_pl2pl(void);

extern bool init_python(void);

extern PyObject PyInit_yap(void);
