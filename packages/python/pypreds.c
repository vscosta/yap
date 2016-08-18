
#include "python.h"

static int conj_size(term_t source) {
  if (PL_is_functor(source, FUNCTOR_comma2)) {
    term_t a1 = PL_new_term_ref(), a2 = PL_new_term_ref();
    if (PL_get_arg(1, source, a1) <= 0 || PL_get_arg(2, source, a2) <= 0)
      return -1;
    return conj_size(a1) + conj_size(a2);
  }
  return 1;
}

static int conj_copy(term_t target, PyObject *e, int pos) {
  if (PL_is_functor(target, FUNCTOR_comma2)) {
    term_t a1 = PL_new_term_ref(), a2 = PL_new_term_ref();
    if (PL_get_arg(1, target, a1) <= 0 || PL_get_arg(2, target, a2) <= 0)
      return -1;
    int p1 = conj_copy(a1, e, pos);
    return conj_copy(a2, e, p1);
  } else {
    assign_python(py_Main, target, PyTuple_GetItem(e, pos));
    return pos + 1;
  }
}

static foreign_t python_f(term_t tmod, term_t fname, term_t tf) {
  char *s;
  size_t len;
  PyObject *pF, *pModule;

  /* if an atom, fetch again */
  if (PL_is_atom(tmod)) {
    PyObject *pName;

    if (!PL_get_nchars(fname, &len, &s, CVT_ALL | CVT_EXCEPTION)) {
      return FALSE;
    }
#if PY_MAJOR_VERSION < 3
    pName = PyString_FromString(s);
#else
    pName = PyUnicode_FromString(s);
#endif
    if (pName == NULL) {
      return FALSE;
    }
    pModule = PyImport_Import(pName);
  } else if (!(pModule = term_to_python(tmod, true)))
    return FALSE;
  if (!PL_get_nchars(fname, &len, &s, CVT_ALL | CVT_EXCEPTION)) {
    return FALSE;
  }
  pF = PyObject_GetAttrString(pModule, s);
  PyErr_Print();
  Py_DECREF(pModule);
  if (pF == NULL || !PyCallable_Check(pF)) {
    return FALSE;
  }
  printf("Module=%s ok\n", s);
  return python_to_ptr(pF, tf);
}

static foreign_t python_o(term_t tmod, term_t fname, term_t tf) {
  char *s;
  size_t len;
  PyObject *pO, *pModule;

  pModule = term_to_python(tmod, true);
  if (!PL_get_nchars(fname, &len, &s, CVT_ALL | CVT_EXCEPTION)) {
    return FALSE;
  }
  pO = PyObject_GetAttrString(pModule, s);
  if (pO == NULL) {
    return FALSE;
  }
  return python_to_ptr(pO, tf);
}

static foreign_t python_len(term_t tobj, term_t tf) {
  Py_ssize_t len;
  PyObject *o;

  o = term_to_python(tobj, true);
  if (o == NULL)
    return FALSE;
  len = PyObject_Length(o);
  return PL_unify_int64(tf, len);
}

static foreign_t python_dir(term_t tobj, term_t tf) {
  PyObject *dir;
  PyObject *o;

  o = term_to_python(tobj, true);
  if (o == NULL)
    return FALSE;
  dir = PyObject_Dir(o);
  return python_to_ptr(dir, tf);
}

static foreign_t python_index(term_t tobj, term_t tindex, term_t val) {
  PyObject *i;
  PyObject *o;
  PyObject *f;
  o = term_to_python(tobj, true);
  if (o == NULL)
    return false;
  if (!PySequence_Check(o))
    return false;
  i = term_to_python(tindex, true);
  if (i == NULL)
    return false;
#if PY_MAJOR_VERSION < 3
  f = PyObject_CallMethodObjArgs(o, PyString_FromString("getitem"), i);
#else
  f = PyObject_CallMethodObjArgs(o, PyUnicode_FromString("getitem"), i);
#endif
  return python_to_ptr(f, val);
}

static foreign_t python_is(term_t tobj, term_t tf) {
  PyObject *o;

  o = term_to_python(tobj, true);
  if (!o)
    return FALSE;

  return python_to_ptr(o, tf);
}

static foreign_t python_assign_item(term_t parent, term_t indx, term_t tobj) {
  PyObject *pF, *pI;

  PyObject *p;

  // get Scope ...
  pI = term_to_python(indx, true);
  // got Scope.Exp
  // get Scope ...
  p = term_to_python(parent, true);
  // Exp
  // get Scope ...
  pF = term_to_python(parent, true);
  // Exp
  if (!pI || !p) {
    return false;
  } else if (PyObject_SetItem(p, pI, pF)) {
    PyErr_Print();
    return FALSE;
  }
  Py_DecRef(pI);
  Py_DecRef(p);

  return true;
}

/** assign a tuple to something:
*/
static foreign_t python_assign_tuple(term_t t_lhs, term_t t_rhs) {
  PyObject *e = term_to_python(t_rhs, true);
  Py_ssize_t sz;
  functor_t f;
  if (!e || !PyTuple_Check(e)) {
    return -1;
  }
  sz = PyTuple_Size(e);
  switch (PL_term_type(t_lhs)) {
  case PL_VARIABLE:
    return PL_unify(t_lhs, t_rhs);
  case PL_ATOM:
    return assign_python(py_Main, t_rhs, e);
  case PL_TERM:
    if (PL_get_functor(t_lhs, &f)) {
      term_t targ = PL_new_term_ref();
      // assign a tuple to a tuple
      if (PL_functor_name(f) == ATOM_t && ((sz = PL_functor_arity(f)))) {
        Py_ssize_t i;
        for (i = 0; i < sz; i++) {
          PL_get_arg(i + 1, t_lhs, targ);
          assign_python(py_Main, targ, PyTuple_GetItem(e, i));
        }
      } else if (PL_functor_name(f) == ATOM_comma) {
        int n = conj_size(t_lhs);
        if (n != sz)
          return -1;
        return conj_copy(t_lhs, e, 0);
      } else if (PL_functor_name(f) == ATOM_dot) { // vectors
        size_t len;
        term_t tail = PL_new_term_ref();

        PL_skip_list(t_lhs, tail, &len);
        if (!PL_get_nil(tail))
          return -1;
        term_t arg = tail;
        size_t i;

        for (i = 0; i < len; i++) {
          if (!PL_get_list(t_rhs, arg, t_rhs)) {
            return -1;
          }
          if (assign_python(py_Main, arg, PyTuple_GetItem(e, i)) < 0)
            return -1;
        }
      }
    }
  }
  return -1;
}

static foreign_t python_item(term_t parent, term_t indx, term_t tobj) {
  PyObject *pF, *pI;

  PyObject *p;

  // get Scope ...
  pI = term_to_python(indx, true);
  // got Scope.Exp
  // get Scope ...
  p = term_to_python(parent, true);
  // Exp
  if (!pI || !p) {
    return false;
  } else if ((pF = PyObject_GetItem(p, pI)) == NULL) {
    PyErr_Print();
    return FALSE;
  }
  Py_DecRef(pI);
  Py_DecRef(p);

  return address_to_term(pF, tobj);
}

static foreign_t python_slice(term_t parent, term_t indx, term_t tobj) {
  PyObject *pF, *pI;

  PyObject *p;

  // get Scope ...
  pI = term_to_python(indx, true);
  // got Scope.Exp
  // get Scope ...
  p = term_to_python(parent, true);
  // Exp
  if (!pI || !p) {
    return false;
  } else if ((pF = PySequence_GetSlice(p, 0, 0)) == NULL) {
    PyErr_Print();
    return FALSE;
  }
  Py_DecRef(pI);
  Py_DecRef(p);

  return address_to_term(pF, tobj);
}

static foreign_t python_apply(term_t tin, term_t targs, term_t keywds,
                              term_t tf) {
  PyObject *pF;
  PyObject *pArgs, *pKeywords;
  PyObject *pValue;
  int i, arity;
  atom_t aname;
  foreign_t out;
  term_t targ = PL_new_term_ref();

  pF = term_to_python(tin, true);
  if (pF == NULL) {
    return false;
  }
  if (PL_is_atom(targs)) {
    pArgs = NULL;
  } else {

    if (!PL_get_name_arity(targs, &aname, &arity)) {
      return FALSE;
    }
    if (arity == 1 && PL_get_arg(1, targs, targ) && PL_is_variable(targ)) {
      /* ignore (_) */
      pArgs = NULL;
    } else {

      pArgs = PyTuple_New(arity);
      if (!pArgs)
        return FALSE;
      for (i = 0; i < arity; i++) {
        PyObject *pArg;
        if (!PL_get_arg(i + 1, targs, targ))
          return FALSE;
        pArg = term_to_python(targ, true);
        if (pArg == NULL)
          return FALSE;
        /* pArg reference stolen here: */
        PyTuple_SetItem(pArgs, i, pArg);
      }
    }
  }
  if (PL_is_atom(keywds)) {
    pKeywords = NULL;
  } else {
    pKeywords = term_to_python(keywds, true);
  }
  if (PyCallable_Check(pF)) {
    pValue = PyEval_CallObjectWithKeywords(pF, pArgs, pKeywords);
    //   PyObject_Print(pF,stderr,0);fprintf(stderr, "\n");
    // PyObject_Print(pArgs,stderr,0);fprintf(stderr, " ");
    // PyObject_Print(pKeywords,stderr,0);fprintf(stderr, "\n");
    if (!pValue)
      PyErr_Print();
    else
      Py_IncRef(pValue);
  } else if (pArgs == NULL) {
    pValue = pF;

    if (pF) {
      Py_IncRef(pValue);
    }
  } else {
    PyErr_Print();
    return FALSE;
  }
  if (pArgs)
    Py_DECREF(pArgs);
  Py_DECREF(pF);
  if (pValue == NULL)
    return FALSE;
  out = python_to_ptr(pValue, tf);
  return out;
}

static foreign_t python_assign(term_t name, term_t exp) {
  PyObject *e = term_to_python(exp, true);

  if (e == NULL)
    return FALSE;
  return assign_python(py_Main, name, e) >= 0;
}

static foreign_t python_assign_field(term_t source, term_t name, term_t exp) {
  PyObject *e = term_to_python(exp, true), *root = term_to_python(source, true);

  if (e == NULL)
    return FALSE;
  return assign_python(root, name, e) >= 0;
}

static foreign_t python_builtin_eval(term_t caller, term_t dict, term_t out) {
  PyObject *pI, *pArgs, *pOut;
  PyObject *env;
  atom_t name;
  char *s;
  int i, arity;
  term_t targ = PL_new_term_ref();

  if ((env = py_Builtin) == NULL) {
    // no point in  even trying
    return false;
  }
  if (PL_get_name_arity(caller, &name, &arity)) {
    if (!(s = PL_atom_chars(name)))
      return false;
    if ((pI = PyObject_GetAttrString(env, s)) == NULL) {
      PyErr_Print();
      return false;
    }
  } else {
    // Prolog should make sure this never happens.
    return false;
  }
  pArgs = PyTuple_New(arity);
  for (i = 0; i < arity; i++) {
    PyObject *pArg;
    if (!PL_get_arg(i + 1, caller, targ))
      return FALSE;
    /* ignore (_) */
    if (i == 0 && PL_is_variable(targ)) {
      pArg = Py_None;
    } else {
      pArg = term_to_python(targ, true);
      if (pArg == NULL)
        return FALSE;
    }
    /* pArg reference stolen here: */
    if (PyTuple_SetItem(pArgs, i, pArg)) {
      PyErr_Print();
      return false;
    }
  }
  pOut = PyObject_CallObject(pI, pArgs);
  Py_DECREF(pArgs);
  Py_DECREF(pI);
  if (pOut == NULL) {
    PyErr_Print();
    return false;
  }
  return python_to_ptr(pOut, out);
}

static foreign_t python_access(term_t obj, term_t f, term_t out) {
  PyObject *o = term_to_python(obj, true), *pValue, *pArgs, *pF;
  atom_t name;
  char *s;
  int i, arity;
  term_t targ = PL_new_term_ref();

  if (o == NULL)
    return FALSE;
  if (PL_is_atom(f)) {
    if (!PL_get_atom_chars(f, &s))
      return FALSE;
    if ((pValue = PyObject_GetAttrString(o, s)) == NULL) {
      PyErr_Print();
      return FALSE;
    }
    return python_to_term(pValue, out);
  }
  if (!PL_get_name_arity(f, &name, &arity)) {
    return FALSE;
  }
  /* follow chains of the form a.b.c.d.e() */
  while (name == ATOM_dot && arity == 2) {
    term_t tleft = PL_new_term_ref();
    PyObject *lhs;

    if (!PL_get_arg(1, f, tleft))
      return FALSE;
    lhs = term_to_python(tleft, true);
    if ((o = PyObject_GetAttr(o, lhs)) == NULL) {
      PyErr_Print();
      return FALSE;
    }
    if (!PL_get_arg(2, f, f))
      return FALSE;
    if (!PL_get_name_arity(f, &name, &arity)) {
      return FALSE;
    }
  }
  s = PL_atom_chars(name);
  if (!s)
    return false;
  if ((pF = PyObject_GetAttrString(o, s)) == NULL) {
    PyErr_Print();
    return FALSE;
  }
  pArgs = PyTuple_New(arity);
  for (i = 0; i < arity; i++) {
    PyObject *pArg;
    if (!PL_get_arg(i + 1, f, targ))
      return FALSE;
    /* ignore (_) */
    if (i == 0 && PL_is_variable(targ)) {
      pArgs = Py_None;
    }
    pArg = term_to_python(targ, true);
    if (pArg == NULL)
      return FALSE;
    /* pArg reference stolen here: */
    PyTuple_SetItem(pArgs, i, pArg);
  }
  pValue = PyObject_CallObject(pF, pArgs);
  Py_DECREF(pArgs);
  Py_DECREF(pF);
  if (pValue == NULL) {
    return FALSE;
  }
  return python_to_term(pValue, out);
}

static foreign_t python_field(term_t parent, term_t att, term_t tobj) {
  PyObject *pF;
  atom_t name;
  char *s;
  int arity;

  if (!PL_get_name_arity(att, &name, &arity)) {
    return false;
  } else {
    PyObject *p;

    // got Scope.Exp
    // get Scope ...
    p = term_to_python(parent, true);
    // Exp
    if (!PL_get_name_arity(att, &name, &arity)) {
      return false;
    }
    s = PL_atom_chars(name);
    if (arity == 1 && !strcmp(s, "()")) {
      if (!PL_get_arg(1, att, att))
        return false;
      if (!PL_get_name_arity(att, &name, &arity)) {
        return false;
      }
      s = PL_atom_chars(name);
    }
    if (!s || !p) {
      return false;
    } else if ((pF = PyObject_GetAttrString(p, s)) == NULL) {
      PyErr_Clear();
      return FALSE;
    }
  }
  return address_to_term(pF, tobj);
}

static foreign_t python_main_module(term_t mod) {
  return address_to_term(py_Main, mod);
}

static foreign_t python_function(term_t tobj) {
  PyObject *obj = term_to_python(tobj, true);

  return PyFunction_Check(obj);
}

foreign_t python_builtin(term_t out) {
  return address_to_term(py_Builtin, out);
}

static foreign_t python_run_file(term_t file) {
  char *s;
  size_t len;
  char si[256];
  s = si;
  if (PL_get_nchars(file, &len, &s, CVT_ALL | CVT_EXCEPTION)) {
#if PY_MAJOR_VERSION < 3
    PyObject *PyFileObject = PyFile_FromString(si, "r");
    PyRun_SimpleFileEx(PyFile_AsFile(PyFileObject), "test.py", 1);
#else
    FILE *f = fopen(s, "r");
    if (f == NULL)
      return false;
    PyRun_SimpleFileEx(f, s, 1);
#endif
    return TRUE;
  }
  return false;
}

static foreign_t python_run_command(term_t cmd) {
  char *s;
  size_t len;
  char si[256];
  s = si;
  if (PL_get_nchars(cmd, &len, &s, CVT_ALL | CVT_EXCEPTION)) {
    PyRun_SimpleString(s);
  }
  return TRUE;
}

static foreign_t python_run_script(term_t cmd, term_t fun) {
  char si[256], sf[256];
  size_t len = 255, len1 = 255;
  PyObject *pName, *pModule, *pFunc;
  PyObject *pArgs = NULL, *pValue;
  char *s;

  s = si;
  if (PL_get_nchars(cmd, &len, &s, CVT_ALL | CVT_EXCEPTION) &&
      (s = sf) != NULL &&
      PL_get_nchars(fun, &len1, &s, CVT_ALL | CVT_EXCEPTION)) {

#if PY_MAJOR_VERSION < 3
    pName = PyString_FromString("rbm");
#else
    // asssumes UTF-8
    pName = PyUnicode_FromString("rbm");
#endif
    /* Error checking of pName left out */

    pModule = PyImport_Import(pName);
    Py_DECREF(pName);

    if (pModule != NULL) {
      pFunc = PyObject_GetAttrString(pModule, sf);
      /* pFunc is a new reference */

      if (pFunc && PyCallable_Check(pFunc)) {
        pValue = PyObject_CallObject(pFunc, pArgs);
        if (pValue != NULL) {
          Py_DECREF(pValue);
        } else {
          Py_DECREF(pFunc);
          Py_DECREF(pModule);
          PyErr_Print();
          fprintf(stderr, "Call failed\n");
          return false;
        }
      } else {
        if (PyErr_Occurred())
          PyErr_Print();
        fprintf(stderr, "Cannot find function \"%s\"\n", sf);
      }
      Py_XDECREF(pFunc);
      Py_DECREF(pModule);
    } else {
      PyErr_Print();
      return false;
    }
    return true;
  }
  return false;
}

static foreign_t python_export(term_t t, term_t pl) {
  foreign_t rc = false;
  if (PL_is_functor(t, FUNCTOR_pointer1)) {
    void *ptr;
    term_t targ = PL_new_term_ref();

    if (!PL_get_arg(1, t, targ))
      return false;
    if (!PL_get_pointer(targ, &ptr))
      return false;
    Py_INCREF((PyObject *)ptr);
    /* return __main__,s */
    rc = python_to_term((PyObject *)ptr, pl);
  }
  return rc;
}

static int python_import(term_t mname, term_t mod) {
  PyObject *pName, *pModule;
  term_t arg = PL_new_term_ref();
  char s0[MAXPATHLEN], *s = s0;

  while (true) {
    size_t len;

    len = (MAXPATHLEN - 1) - (s - s0);
    if (PL_is_pair(mname)) {
      char *sa;
      if (!PL_get_arg(1, mname, arg) || !PL_get_atom_chars(arg, &sa) ||
          !PL_get_arg(2, mname, mname))
        return false;
      s = stpcpy(s, sa);
      *s++ = '.';
    } else if (!PL_get_nchars(mname, &len, &s,
                              CVT_ALL | CVT_EXCEPTION | ENC_ISO_UTF8)) {
      return false;
    } else {
      break;
    }
  }
#if PY_MAJOR_VERSION < 3
  pName = PyString_FromString(s0);
#else
  pName = PyUnicode_FromString(s0);
#endif
  if (pName == NULL) {
    return false;
  }
  pModule = PyImport_Import(pName);
  Py_DECREF(pName);
  if (pModule == NULL) {
#if EXTRA_MESSSAGES
    if (PyErr_Occurred())
      PyErr_Print();
    PyErr_Clear();
#endif
    return FALSE;
  }
  ActiveModules[active_modules++] = pModule;
  return python_to_ptr(pModule, mod);
}

install_t install_pypreds(void) {
  PL_register_foreign("python_builtin_eval", 3, python_builtin_eval, 0);
  PL_register_foreign("python_builtin", 1, python_builtin, 0);
  PL_register_foreign("python_import", 2, python_import, 0);
  PL_register_foreign("python_f", 3, python_f, 0);
  PL_register_foreign("python_o", 3, python_o, 0);
  PL_register_foreign("python_len", 2, python_len, 0);
  PL_register_foreign("python_is", 2, python_is, 0);
  PL_register_foreign("python_dir", 2, python_dir, 0);
  PL_register_foreign("python_apply", 4, python_apply, 0);
  PL_register_foreign("python_index", 3, python_index, 0);
  PL_register_foreign("python_field", 3, python_field, 0);
  PL_register_foreign("python_assign", 2, python_assign, 0);
  PL_register_foreign("python_assign_field", 3, python_assign_field, 0);
  PL_register_foreign("python_assign_tuple", 2, python_assign_tuple, 0);
  PL_register_foreign("python_export", 2, python_export, 0);
  PL_register_foreign("python_function", 1, python_function, 0);
  PL_register_foreign("python_slice", 4, python_slice, 0);
  PL_register_foreign("python_item", 3, python_item, 0);
  PL_register_foreign("python_assign_item", 3, python_assign_item, 0);
  PL_register_foreign("python_run_file", 1, python_run_file, 0);
  PL_register_foreign("python_run_command", 1, python_run_command, 0);
  PL_register_foreign("python_run_script", 2, python_run_script, 0);
  PL_register_foreign("python_main_module", 1, python_main_module, 0);
  PL_register_foreign("python_import", 2, python_import, 0);
  PL_register_foreign("python_access", 3, python_access, 0);
}
