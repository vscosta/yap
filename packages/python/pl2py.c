

#include "python.h"

PyObject *yap_to_python(YAP_Term t, bool eval) {
  term_t yt = PL_new_term_ref();
  PyObject *o = term_to_python(yt, eval);
  PL_reset_term_refs(yt);
  return o;
}

/**
* term_to_python translates and evaluates from Prolog to Python
*
* @param t handle to Prolog term
* @param t whether should  try to evaluate evaluables.
*
* @return a Python object descriptor or NULL if failed
*/
PyObject *term_to_python(term_t t, bool eval) {
  // Yap_DebugPlWrite(YAP_GetFromSlot(t));        fprintf(stderr, " here I
  YAP_Term yt = YAP_GetFromSlot(t);
  switch (PL_term_type(t)) {
  case PL_VARIABLE: {
    PyObject *o = NULL, *pArgs;
    if (eval)
      return NULL;
    if (PyObject_HasAttrString(py_Main, "V"))
      o = PyObject_GetAttrString(py_Main, "V");
    if (!o && PyObject_HasAttrString(py_Yapex, "V"))
      o = PyObject_GetAttrString(py_Yapex, "V");
    if (!o || !PyCallable_Check(o)) {
      return NULL;
    }
    pArgs = PyTuple_New(1);
    PyTuple_SET_ITEM(pArgs, 0, PyLong_FromLong(t));
    return PyObject_CallObject(o, pArgs);
  };
  case PL_ATOM: {
    YAP_Atom at = YAP_AtomOfTerm(yt);
    const char *s;
    PyObject *o;
    PyObject *c = NULL;

    s = YAP_AtomName(at);
    if (strcmp(s, "true") == 0)
      return Py_True;
    if (strcmp(s, "false") == 0)
      return Py_False;
    if (strcmp(s, "none") == 0)
      return Py_None;
    if (strcmp(s, "[]") == 0)
      o = PyList_New(0);
    else if (strcmp(s, "{}") == 0)
      o = PyDict_New();
    /* return __main__,s */
    else if ((o = PyRun_String(s, Py_single_input,
			       PyEval_GetGlobals(), PyEval_GetLocals()))) {
      Py_IncRef(o);
      return o;
    } else if (PyObject_HasAttrString(py_Main, s)) {
      o = PyObject_GetAttrString(py_Main, s);
    } else {
      o = PyUnicode_FromString(s);
      if (eval)
        return NULL;
      if (PyObject_HasAttrString(py_Main, "A"))
        c = PyObject_GetAttrString(py_Main, "A");
      if (!c && PyObject_HasAttrString(py_Yapex, "A"))
        c = PyObject_GetAttrString(py_Yapex, "A");
      if (!c || !PyCallable_Check(c)) {
        return o;
      } else {
        PyObject *t = PyTuple_New(1);
        PyTuple_SET_ITEM(t, 0, PyUnicode_FromString(s));
        o = PyObject_CallObject(c, t);
      }
    }
    if (o) {
      Py_IncRef(o);
    }
    return o;
  } break;
  case PL_STRING: {
    char *s = NULL;
    if (!PL_get_chars(t, &s,
                      REP_UTF8 | CVT_ATOM | CVT_STRING | BUF_MALLOC)) {
      return NULL;
    }
#if PY_MAJOR_VERSION < 3
    if (proper_ascii_string(s)) {
      return PyString_FromStringAndSize(s, strlen(s));
    } else
#endif
    {
      PyObject *pobj = PyUnicode_DecodeUTF8(s, strlen(s), NULL);
      // fprintf(stderr, "%s\n", s);
      free(s);
      if (pobj) {
	Py_IncRef(pobj);
      }
      return pobj;
    }
  } break;
  case PL_INTEGER: {
    int64_t j;
    if (!PL_get_int64_ex(t, &j))
      return NULL;
#if PY_MAJOR_VERSION < 3
    return PyInt_FromLong(j);
#else
    return PyLong_FromLong(j);
#endif
  }

  case PL_FLOAT: {
    double fl;
    if (!PL_get_float(t, &fl))
      return NULL;
    return PyFloat_FromDouble(fl);
  }
  default: {
    term_t tail = PL_new_term_ref(), arg;
    size_t len, i;
    if (PL_skip_list(t, tail, &len) && PL_get_nil(tail)) {
      PyObject *out;

      arg = tail;
      out = PyList_New(len);
      if (!out)
        return NULL;

      for (i = 0; i < len; i++) {
        if (!PL_get_list(t, arg, t)) {
          return NULL;
        }
        if (PyList_SetItem(out, i, term_to_python(arg, eval)) < 0) {
          return NULL;
        }
      }
      return out;
    } else {
      functor_t fun;

      if (!PL_get_functor(t, &fun))
        return NULL;
      if (eval)
        return compound_to_pyeval(t, fun);
      return compound_to_pytree(t, fun);
    }
  }
  }
  return NULL;
}

PyObject *deref_term_to_python(term_t t) {
  // Yap_DebugPlWrite(YAP_GetFromSlot(t));        fprintf(stderr, " here I
  // am\n");
  YAP_Term yt = YAP_GetFromSlot(t);
  if (YAP_IsVarTerm(yt)) {
    char s[32];
    char *o = YAP_WriteBuffer(yt, s, 31, 0);
        return PyUnicode_FromString(o);
  }
 return term_to_python(t, false);
}
