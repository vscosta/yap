

#include "py4yap.h"

extern PyObject *py_Local, *py_Global;

PyObject *YE(term_t t, int line, const char *file, const char *code) {
    YAPPy_ThrowError__(file, code, line, SYSTEM_ERROR_INTERNAL,t,"");
}

void YEM(const char *exp, int line, const char *file, const char *code) {
  fprintf(stderr, "**** Warning,%s@%s:%d: failed while executing %s\n", code,
          file, line, exp);
}

static PyObject *s_to_python(const char *s, bool eval, PyObject *p0) {
  PyObject *o;
  if (eval) {
    o = PythonLookup(s, p0);
    /*     if (!o)
           return o;
    */
  } else {
    o = PythonLookupSpecial(s);
  }
  if (o) {
    Py_INCREF(o);
    return CHECKNULL(YAP_MkStringTerm(s), o);
  } else {
    PyObject *pobj = PyUnicode_DecodeUTF8(s, strlen(s), NULL);
    return pobj;
  }
}

/**
 * obtain  the object matching a certain string.
 *
 * @param t handle to Prolog term
 * @param t whether should  try to evaluate evaluables.
 *
 * @return a Python object descriptor or NULL if failed
 */
X_API PyObject *string_to_python(const char *s, bool eval, PyObject *p0) {
  char *buf = malloc(strlen(s) + 1), *child;
  PyObject *p1;
  while ((child = strchr(s, '.')) != NULL) {
    size_t len = child - s;
    strncpy(buf, s, len);
    buf[len] = '\0';
    p1 = s_to_python(buf, eval, p0);
    Py_DecRef(p0);
    Py_IncRef(p1)
    s = child + 1;
    p0 = p1;
  }
  p1 = s_to_python(s, eval, p0);
  Py_DecRef(p0);
  return p1;
}

/**
 * term_to_python translates and evaluates from Prolog to Python
 *
 * @param t handle to Prolog term
 * @param t whether should  try to evaluate evaluables.
 *
 * @return a Python object descriptor or NULL if failed
 */
PyObject *term_to_python(term_t t, bool eval, PyObject *o) {
  // o≈
  PyObject *o;\
  YAP_Term yt = YAP_GetFromSlot(t);
  //  Yap_DebugPlWriteln(yt);
  switch (PL_term_type(t)) {
  case PL_VARIABLE: {
    if (t == 0) {
        YAPPy_ThrowErrorYA(SYSTEM_ERROR_INTERNAL,t,"");
t    }
    PyObject *out = PyTuple_New(1);
    PyTuple_SET_ITEM(out, 0, PyLong_FromLong(t));
    Py_IncRef(out);
    rc = term_to_nametuple("v", 1, out);
    Py_IncRef(rc);
  };
  case PL_ATOM: {
    YAP_Atom at = YAP_AtomOfTerm(yt);
    const char *s;

    s = YAP_AtomName(at);
    if (eval) {
      o = PythonLookup(s, o);
      /*     if (!o)
             return o;
      */
    } else {
      o = PythonLookupSpecial(s);
    }
    if (o) {
      Py_INCREF(o);
      return CHECKNULL(t, o);
    }
  }
  case PL_STRING: {
    const char *s = NULL;
    if (YAP_IsAtomTerm(yt)) {
      s = YAP_AtomName(YAP_AtomOfTerm(yt));
    } else if (YAP_IsStringTerm(yt)) {
      s = YAP_StringOfTerm(yt);
    } else {
      return CHECKNULL(t, NULL);
    }
#if PY_MAJOR_VERSION < 3
    if (proper_ascii_string(s)) {
      PyObject *o = PyString_FromStringAndSize(s, strlen(s));
      Py_IncRef(o)
      return CHECKNULL(t, o);
    } else
#endif
    {
      //      char *p = malloc(strlen(s)+1);
      //strcpy(p, s);
      PyObject *pobj = PyUnicode_FromString(s);
      Py_IncRef(pobj);
      return CHECKNULL(t, pobj);
    }
  } break;
  case PL_INTEGER: {
    int64_t j;
    if (!PL_get_int64_ex(t, &j))
      return CHECKNULL(t, NULL);
#if PY_MAJOR_VERSION < 3
    PyObject *o = PyInt_FromLong(j);
    return CHECKNULL(t, o);
#else
    PyObject *o = PyLong_FromLong(j);
    return CHECKNULL(t, o);
#endif
  }

  case PL_FLOAT: {
    PyObject *out;
    double fl;
    if (!PL_get_float(t, &fl))
      return CHECKNULL(t, NULL);
    out = PyFloat_FromDouble(fl);
    return CHECKNULL(t, out);
  }
  default: {
    term_t tail = PL_new_term_ref(), arg;
    size_t len, i;
    if (PL_skip_list(t, tail, &len) && PL_get_nil(tail)) {
      PyObject *out, *a;

      arg = tail;
      out = PyList_New(len);
      if (!out) {
        PL_reset_term_refs(tail);
        return CHECKNULL(t, Py_None);
      }

      for (i = 0; i < len; i++) {
        if (!PL_get_list(t, arg, t)) {
            YAPPy_ThrowErrorYA(SYSTEM_ERROR_INTERNAL,t,"");
        }
        a = term_to_python(arg, eval, o);
        if (a) {
          if (PyList_SetItem(out, i, a) < 0) {
              YAPPy_ThrowErrorYA(SYSTEM_ERROR_INTERNAL,t,"");
          }
        }
      }
      PL_reset_term_refs(tail);
      return CHECKNULL(t, out);
    } else {
      functor_t fun;
      PyObject *rc;

      if (!PL_get_functor(t, &fun)) {
        PL_reset_term_refs(tail);
        return CHECKNULL(t, Py_None);
      }
      if (eval)
        rc = compound_to_pyeval(t, o);
      else
        rc = compound_to_pytree(t, o);
      PL_reset_term_refs(tail);
      return rc;
    }
  }
  }
  return CHECKNULL(t, Py_None);
}

PyObject *yap_to_python(YAP_Term t, bool eval, PyObject *o) {
  if (t == 0)
    return NULL;
  term_t yt = YAP_InitSlot(t);
  o = term_to_python(yt, eval, o);
  PL_reset_term_refs(yt);
  return o;
}

PyObject *deref_term_to_python(term_t t) {
  // Yap_DebugPlWrite(YAP_GetFromSlot(t));        fprintf(stderr, " here I
  // am\n");
  YAP_Term yt = YAP_GetFromSlot(t);
  if (YAP_IsVarTerm(yt)) {
    char s[32];
    char *o = YAP_WriteBuffer(yt, s, 31, 0);
    PyObject *p = PyUnicode_FromString(o);
    return p;
  }
  return term_to_python(t, false, NULL);
}
