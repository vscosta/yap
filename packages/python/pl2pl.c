
#include "py4yap.h"

static foreign_t array_to_python_list(term_t addr, term_t type, term_t szt,
                                      term_t py) {
  void *src;
  Py_ssize_t sz, i;
  int is_float;

  if (!PL_get_pointer(addr, &src) || !PL_get_bool(type, &is_float) ||
      !PL_get_intptr(szt, &sz))
    return false;
  PyObject *list = PyList_New(sz);
  if (!list)
    return false;
  if (is_float) {
    double *v = (double *)src;
    for (i = 0; i < sz; i++) {
      PyObject *x = PyFloat_FromDouble(v[i]);
      PyList_SET_ITEM(list, i, x);
    }
  } else {
    YAP_Int *v = (YAP_Int *)src;
    for (i = 0; i < sz; i++) {
      PyObject *x = PyFloat_FromDouble(v[i]);
      PyList_SET_ITEM(list, i, x);
    }
  }
  if (PL_is_variable(py)) {
    return address_to_term(list, py);
  }
  return assign_to_symbol(py, list);
}

static foreign_t array_to_python_tuple(term_t addr, term_t type, term_t szt,
                                       term_t py) {
  void *src;
  Py_ssize_t sz, i;
  int is_float;

  if (!PL_get_pointer(addr, &src) || !PL_get_bool(type, &is_float) ||
      !PL_get_intptr(szt, &sz))
    return false;
  PyObject *list = PyTuple_New(sz);
  if (!list)
    return false;
  if (is_float) {
    double *v = (double *)src;

    for (i = 0; i < sz; i++) {
      PyObject *x;
      x = PyFloat_FromDouble(v[i]);
      if (PyTuple_SetItem(list, i, x)) {
        PyErr_Print();
        return FALSE;
      }
    }
  } else {
    int32_t *v = (int32_t *)src;
    PyObject *x;
    for (i = 0; i < sz; i++) {
#if PY_MAJOR_VERSION < 3
      x = PyInt_FromLong(v[i]);
#else
      x = PyLong_FromLong(v[i]);
#endif
      if (PyTuple_SetItem(list, i, x)) {
        PyErr_Print();
        return FALSE;
      }
    }
  }
  if (PL_is_variable(py)) {
    return address_to_term(list, py);
  }
  return assign_to_symbol(py, list);
}

static foreign_t array_to_python_view(term_t addr, term_t type, term_t szt,
                                      term_t colt, term_t py) {
  void *src;
  Py_ssize_t sz, rows;
  int is_float;
  Py_ssize_t shape[2];

  if (!PL_get_pointer(addr, &src) || !PL_get_bool(type, &is_float) ||
      !PL_get_intptr(szt, &sz) || !PL_get_intptr(colt, &rows))
    return false;
  Py_buffer buf;
  buf.buf = src;
  if (is_float) {
    buf.len = sz * sizeof(double);
    buf.itemsize = sizeof(double);
  } else {
    buf.len = sz * sizeof(YAP_Int);
    buf.itemsize = sizeof(YAP_Int);
  }
  buf.readonly = false;
  buf.format = NULL;
  buf.ndim = 2;
  buf.shape = shape;
  buf.strides = NULL;
  buf.suboffsets = NULL;
  PyObject *o = PyMemoryView_FromBuffer(&buf);
  if (!o) {
    PyErr_Print();
    return false;
  }
  if (PL_is_variable(py)) {
    return address_to_term(o, py);
  }
  return assign_to_symbol(py, o);
}

static foreign_t prolog_list_to_python_list(term_t plist, term_t pyt, term_t tlen) {
  size_t sz, i;
   
PyErr_Clear();
   PyObject *pyl = term_to_python(pyt, true, NULL, true);
  term_t targ = PL_new_term_ref();

  if (PL_skip_list(plist, targ, &sz) <0 || ! PL_get_nil(targ)) {
    pyErrorAndReturn( false);
}
  if (!PyList_Check(pyl))
    {
      pyErrorAndReturn( false);
    }
  if (sz > PyList_GET_SIZE(pyl))
    pyErrorAndReturn( false);
  for  (i=0; i < sz; i++) {
    if (!PL_get_list(plist, targ, plist)) {
      pyErrorAndReturn( false);
    }
    PyObject *t = term_to_python(targ, true, NULL, true);
    PyList_SET_ITEM(pyl, i, t);
  }
  if (PL_is_variable(tlen)) {
    PL_unify_int64(tlen, sz);
  } else {
    python_assign(tlen, PyLong_FromUnsignedLong(sz), NULL);
  }
  pyErrorAndReturn( true);
}

install_t install_pl2pl(void) {
  PL_register_foreign("array_to_python_list", 4, array_to_python_list, 0);
  PL_register_foreign("array_to_python_tuple", 4, array_to_python_tuple, 0);
  PL_register_foreign("array_to_python_view", 5, array_to_python_view, 0);
  PL_register_foreign("prolog_list_to_python_list", 3, prolog_list_to_python_list, 0);
}
