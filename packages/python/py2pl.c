
#include "python.h"

static foreign_t repr_term(PyObject *pVal, term_t t) {
  term_t to = PL_new_term_ref(), t1 = PL_new_term_ref();
  PL_put_pointer(t1, pVal);
  PL_cons_functor(to, FUNCTOR_pointer1, t1);
  Py_INCREF(pVal);
  PL_reset_term_refs(to);
  return PL_unify(t, to);
}

foreign_t assign_to_symbol(term_t t, PyObject *e);

foreign_t assign_to_symbol(term_t t, PyObject *e) {
  char *s = NULL;
  PyErr_Clear();
  if (!PL_get_atom_chars(t, &s)) {
    return false;
  }
  PyObject *dic;
  if (!lookupPySymbol(s, NULL, &dic))
    dic = py_Main;
  return PyObject_SetAttrString(dic, s, e) == 0;
}

foreign_t python_to_term(PyObject *pVal, term_t t) {
  if (pVal == Py_None) {
    return PL_unify_atom(t, ATOM_none);
  }
  if (PyBool_Check(pVal)) {
    if (PyObject_IsTrue(pVal)) {
      return PL_unify_atom(t, ATOM_true);
    } else {
      return PL_unify_atom(t, ATOM_false);
    }
  } else if (PyLong_Check(pVal)) {
    return PL_unify_int64(t, PyLong_AsLong(pVal));
#if PY_MAJOR_VERSION < 3
  } else if (PyInt_Check(pVal)) {
    return PL_unify_int64(t, PyInt_AsLong(pVal));
#endif
  } else if (PyFloat_Check(pVal)) {
    return PL_unify_float(t, PyFloat_AsDouble(pVal));
  } else if (PyComplex_Check(pVal)) {
    bool rc;
    term_t to = PL_new_term_ref(), t1 = PL_new_term_ref(),
           t2 = PL_new_term_ref();
    if (!PL_put_float(t1, PyComplex_RealAsDouble(pVal)) ||
        !PL_put_float(t2, PyComplex_ImagAsDouble(pVal)) ||
        !PL_cons_functor(to, FUNCTOR_complex2, t1, t2)) {
      rc = FALSE;
    } else {
      rc = PL_unify(t, to);
    }
    PL_reset_term_refs(to);
    return rc;
  } else if (PyUnicode_Check(pVal)) {
    atom_t tmp_atom;

#if PY_MAJOR_VERSION < 3
    = PyUnicode_GetSize(pVal) + 1;
    wchar_t *ptr = malloc(sizeof(wchar_t) * sz);
    sz = PyUnicode_AsWideChar((PyUnicodeObject *)pVal, ptr, sz - 1);
   tmp_atom = PL_new_atom_wchars(sz, ptr);
    free(ptr);
     return PL_unify_atom(t, tmp_atom);
#else
    const char *s = PyUnicode_AsUTF8(pVal);
   tmp_atom = PL_new_atom( s);
     return PL_unify_atom(t, tmp_atom);
#endif
  } else if (PyByteArray_Check(pVal)) {
    atom_t tmp_atom = PL_new_atom(PyByteArray_AsString(pVal));
    return PL_unify_atom(t, tmp_atom);
#if PY_MAJOR_VERSION < 3
  } else if (PyString_Check(pVal)) {
    atom_t tmp_atom = PL_new_atom(PyString_AsString(pVal));
    return PL_unify_atom(t, tmp_atom);
#endif
  } else if (PyTuple_Check(pVal)) {
    Py_ssize_t i, sz = PyTuple_Size(pVal);
    functor_t f;
    const char *s;
    if ((s = (Py_TYPE(pVal)->tp_name))) {
      if (!strcmp(s, "H")) {
        pVal = PyTuple_GetItem(pVal, 0);
        if (PyLong_Check(pVal)) {
          return PyLong_AsLong(pVal);
#if PY_MAJOR_VERSION < 3
        } else if (PyInt_Check(pVal)) {
          return PyInt_AsLong(pVal);
#endif
        }
      }
      if (s[0] == '$') {
        char *ns = malloc(strlen(s) + 5);
        strcpy(ns, "__");
        strcat(ns, s + 1);
        strcat(ns, "__");
        f = PL_new_functor(PL_new_atom(ns), sz);
      } else {
        f = PL_new_functor(PL_new_atom(s), sz);
      }
    } else
      f = PL_new_functor(ATOM_t, sz);
    if (!PL_unify_functor(t, f))
      return FALSE;
    term_t to = PL_new_term_ref();
    for (i = 0; i < sz; i++) {
      if (!PL_get_arg(i + 1, t, to))
        return FALSE;
      if (!python_to_term(PyTuple_GetItem(pVal, i), to))
        return FALSE;
    }
    return true;
  } else if (PyList_Check(pVal)) {
    term_t to = PL_new_term_ref();
    Py_ssize_t i, sz = PyList_GET_SIZE(pVal);

    for (i = 0; i < sz; i++) {
      if (!PL_unify_list(t, to, t) ||
          !python_to_term(PyList_GetItem(pVal, i), to))
        return FALSE;
    }
    return PL_unify_nil(t);
  } else if (PyDict_Check(pVal)) {
    Py_ssize_t pos = 0;
    term_t to = PL_new_term_ref(), ti = to;
    int left = PyDict_Size(pVal);
    PyObject *key, *value;

    while (PyDict_Next(pVal, &pos, &key, &value)) {
      term_t tkey = PL_new_term_ref(), tval = PL_new_term_ref(), tint,
             tnew = PL_new_term_ref();
      /* do something interesting with the values... */
      if (!python_to_term(key, tkey)) {
        PL_reset_term_refs(tkey);
        return FALSE;
      }
      if (!python_to_term(value, tval)) {
        PL_reset_term_refs(tkey);
        return FALSE;
      }
      /* reuse */
      tint = tkey;
      if (!PL_cons_functor(tint, FUNCTOR_colon2, tkey, tval)) {
        PL_reset_term_refs(tkey);
        return FALSE;
      }
      if (--left) {
        if (!PL_cons_functor(tint, FUNCTOR_comma2, tint, tnew))
          PL_reset_term_refs(tkey);
        return FALSE;
      }
      if (!PL_unify(ti, tint)) {
        PL_reset_term_refs(tkey);
        return FALSE;
      }
      ti = tnew;
      PL_reset_term_refs(tkey);
    }
    return PL_unify(t, to);
  } else {
    return repr_term(pVal, t);
  }
}

X_API YAP_Term pythonToYAP(PyObject *pVal) {
  term_t t = PL_new_term_ref();
  if (!python_to_term(pVal, t))
    return 0;
  YAP_Term tt = YAP_GetFromSlot(t);
  PL_reset_term_refs(t);
  Py_DECREF(pVal);
  return tt;
}

PyObject *py_Local, *py_Global;

/**
 *   assigns the Python RHS to a Prolog term LHS, ie LHS = RHS
 *
 * @param root Python environment
 * @param t left hand side, in Prolog, may be
 *    - a Prolog variable, exports the term to Prolog, A <- RHS
 *    - Python variable A, A <- RHS
 *    - Python variable $A, A <- RHS
 *    - Python string "A", A <- RHS
 *    - Python array range
 * @param e the right-hand side
 *
 * @return -1 on failure.
 *
 * Note that this is an auxiliary routine to the Prolog
 *python_assign.
 */
bool python_assign(term_t t, PyObject *exp, PyObject *context) {
  context = find_obj(context, t, false);
  // Yap_DebugPlWriteln(yt);
  switch (PL_term_type(t)) {
  case PL_VARIABLE: {
    if (context == NULL) // prevent a.V= N*N[N-1]
      return python_to_term(exp, t);
  }

  case PL_ATOM: {
    char *s = NULL;
    PL_get_atom_chars(t, &s);
    if (!context)
      context = py_Main;
    return PyObject_SetAttrString(context, s, exp) == 0;
  }
  case PL_STRING:
  case PL_INTEGER:
  case PL_FLOAT:
    // domain or type erro?
    return false;
  default: {
    term_t tail = PL_new_term_ref(), arg = PL_new_term_ref();
    size_t len, i;
    if (PL_skip_list(t, tail, &len) &&
        PL_get_nil(tail)) { //             true list

      if (PySequence_Check(exp) && PySequence_Length(exp) == len)

        for (i = 0; i < len; i++) {
          if (!PL_get_list(t, arg, t)) {
            PL_reset_term_refs(tail);
            return false;
          }
          if (!python_assign(arg, PySequence_GetItem(exp, i), context)) {
            PL_reset_term_refs(tail);
            return false;
          }
        }
      PL_reset_term_refs(tail);
      return true;
    } else {
      functor_t fun;

      if (!PL_get_functor(t, &fun)) {
        PL_reset_term_refs(tail);
        return false;
      }

      if (fun == FUNCTOR_sqbrackets2) {
        if (!PL_get_arg(2, t, tail)) {
          PL_reset_term_refs(tail);
          return false;
        }

        PyObject *o = term_to_python(tail, true, context);
        if (!PL_get_arg(2, t, tail) && !PL_get_nil(tail)) {
          PL_reset_term_refs(tail);
          return false;
        }
        if (!PL_get_arg(1, t, t)) {
          PL_reset_term_refs(tail);
          return false;
        }
        PL_reset_term_refs(tail);
        PyObject *i = term_to_python(t, true, NULL);
        if (!i) {
          return false;
        }
        if (PyList_Check(i)) {
          i = PyList_GetItem(i, 0);
          long int j;
          if (PyList_Check(o)) {
#if PY_MAJOR_VERSION < 3
            if (PyInt_Check(i))
              j = PyInt_AsLong(i);
            else
#endif
                if (PyLong_Check(i))
              j = PyLong_AsLong(i);
            else
              return NULL;
            return PyList_SetItem(o, j, exp) == 0;
          }
          if (PyDict_Check(i)) {
            return PyDict_SetItem(o, i, exp) == 0;
          }
          return PyObject_SetAttr(o, i, exp) == 0;
        }
      }
    }
  }
  }
  return NULL;
}
