
#include "python.h"

static foreign_t repr_term(const char *pVal, size_t sz, term_t t) {
  term_t to = PL_new_term_ref(), t1 = PL_new_term_ref();
  PL_put_string_chars(t1, pVal);
  PL_cons_functor(to, FUNCTOR_pointer1, t1);
  Py_INCREF(pVal);
  PL_reset_term_refs(to);
  return PL_unify(t, to);
}

foreign_t assign_to_symbol(term_t t, PyObject *e);

/**
 * assign_python  assigns the Python RHS to a Prolog term LHS, ie LHS = RHS
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
int assign_python(PyObject *root, term_t t, PyObject *e) {
  switch (PL_term_type(t)) {
  case PL_VARIABLE:
    if (python_to_ptr(e, t))
      return 1;
    else
      return -1;
  case PL_ATOM:
    return assign_to_symbol(t, e);
  case PL_STRING:
  case PL_INTEGER:
  case PL_FLOAT:
    return -1;
  case PL_TERM:
    if (PL_is_list(t)) {
      return -1;
    } else {
      functor_t fun;

      if (!PL_get_functor(t, &fun))
        return -1;
      if (fun == FUNCTOR_dollar1) {
        if (!PL_get_arg(1, t, t))
          return -1;
        return assign_to_symbol(t, e);
      }
      if (fun == FUNCTOR_pointer1) {
        return -1;
      }
      if (fun == FUNCTOR_sqbrackets2) {
        term_t targ = PL_new_term_ref(), trhs = PL_new_term_ref();
        PyObject *lhs, *rhs;

        if (!PL_get_arg(1, t, targ))
          return -1;
        lhs = term_to_python(targ, true);
        if (!PL_get_arg(2, t, targ) || !PL_is_list(targ) ||
            !PL_get_list(targ, trhs, targ))
          return -1;
        if (PL_is_functor(trhs, FUNCTOR_dot2)) {
          Py_ssize_t left, right;
          if (!PL_get_arg(1, trhs, targ))
            return -1;
          left = get_p_int(term_to_python(targ, true), 0);
          if (!PL_get_arg(2, trhs, targ))
            return -1;
          right = get_p_int(term_to_python(targ, true), PyObject_Size(lhs));
          if (!PySequence_Check(lhs))
            return -1;
        PL_reset_term_refs(targ);
          return PySequence_SetSlice(lhs, left, right, e);
        } else {
          rhs = term_to_python(trhs, true);
        PL_reset_term_refs(targ);
          return PyObject_SetItem(lhs, rhs, e);
        }
      }
    }
  }
  return -1;
}

foreign_t assign_to_symbol(term_t t, PyObject *e) {
  char *s;
  PyErr_Clear();
  if (!PL_get_atom_chars(t, &s)) {
    wchar_t *w;
    atom_t at;
    size_t len;
    PyObject *attr;

    if (!PL_get_atom(t, &at)) {
      return false;
    }
    if (!(w = PL_atom_wchars(at, &len)))
      return false;
    attr = PyUnicode_FromWideChar(w, wcslen(w));
    if (attr) {
      return PyObject_SetAttr(py_Main, attr, e) >= 0;
    } else {
      PyErr_Print();
      return false;
    }
  } else if (proper_ascii_string(s)) {
    return PyObject_SetAttrString(py_Main, s, e) >= 0;
  } else {
    PyObject *attr = PyUnicode_DecodeLatin1(s, strlen(s), NULL);
    if (!attr)
      return -1;
    return PyObject_SetAttr(py_Main, attr, e) >= 0;
  }
}

foreign_t python_to_ptr(PyObject *pVal, term_t t) {
  Py_IncRef(pVal);
  return address_to_term(pVal, t);
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
    Py_ssize_t sz = PyUnicode_GetSize(pVal) + 1;
    wchar_t *ptr = malloc(sizeof(wchar_t) * sz);
    sz = PyUnicode_AsWideChar((PyUnicodeObject *)pVal, ptr, sz - 1);
#else
    Py_ssize_t sz = PyUnicode_GetLength(pVal) + 1;
    wchar_t *ptr = malloc(sizeof(wchar_t) * sz);
    sz = PyUnicode_AsWideChar(pVal, ptr, sz);
#endif
    tmp_atom = PL_new_atom_wchars(sz, ptr);
    free(ptr);
    return PL_unify_atom(t, tmp_atom);
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
    functor_t f = PL_new_functor(ATOM_t, sz);
    if (!PL_unify_functor(t, f))
      return FALSE;
    for (i = 0; i < sz; i++) {
      term_t to = PL_new_term_ref();
      if (!PL_unify_arg(i + 1, t, to))
        return FALSE;
      if (!python_to_term(PyTuple_GetItem(pVal, i), to))
        return FALSE;
    }
    return TRUE;
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
        return FALSE;
      }
      if (!python_to_term(value, tval)) {
        return FALSE;
      }
      /* reuse */
      tint = tkey;
      if (!PL_cons_functor(tint, FUNCTOR_colon2, tkey, tval)) {
        return FALSE;
      }
      if (--left) {
        if (!PL_cons_functor(tint, FUNCTOR_comma2, tint, tnew))
          return FALSE;
      }
      if (!PL_unify(ti, tint))
        return FALSE;
      ti = tnew;
    }
    PL_cons_functor(to, FUNCTOR_curly1, to);
    return PL_unify(t, to);
  } else {
    PyObject *pValR = PyObject_Repr(pVal);
    if (pValR == NULL)
      return address_to_term(pVal, t);
    Py_ssize_t sz = PyUnicode_GetSize(pValR) + 1;
#if PY_MAJOR_VERSION < 3
    char *s = malloc(sizeof(char) * sz);
    PyObject *us = PyUnicode_EncodeUTF8((const Py_UNICODE *)pValR, sz, NULL);
    PyString_AsStringAndSize(us, &s, &sz);
    foreign_t rc = repr_term(s, sz, t);
    free((void *)s);
    return rc;
#else
    // new interface
    char *s = PyUnicode_AsUTF8AndSize(pVal, &sz);
    return repr_term(s, sz, t);
#endif
  }
}
