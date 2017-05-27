
#include "python.h"

PyObject *py_Main;

void pyErrorHandler__(int line, const char *file, const char *code) {
  // this code is called if a Python error is found.
  fprintf(stderr, " Python error detected at %s %s:%d\n\n", code, file, line);
  PyErr_Print();
}
static foreign_t python_len(term_t tobj, term_t tf) {
  Py_ssize_t len;
  PyObject *o;

  o = term_to_python(tobj, true, NULL);
  if (o == NULL) {
    pyErrorAndReturn(false, false);
  }
  len = PyObject_Length(o);
  pyErrorAndReturn(PL_unify_int64(tf, len), false);
}

static foreign_t python_dir(term_t tobj, term_t tf) {
  PyObject *dir;
  PyObject *o;

  o = term_to_python(tobj, true, NULL);
  if (o == NULL) {
    pyErrorAndReturn(false, false);
  }
  dir = PyObject_Dir(o);
  {
    foreign_t rc = address_to_term(dir, tf);
    ;
    pyErrorAndReturn(rc, false);
  }
}

static foreign_t python_index(term_t tobj, term_t tindex, term_t val) {
  PyObject *i;
  PyObject *o;
  PyObject *f;

  o = term_to_python(tobj, true, NULL);
  if (o == NULL) {
    pyErrorAndReturn(false, false);
  }
  if (!PySequence_Check(o)) {
    pyErrorAndReturn(false, false);
  }
  i = term_to_python(tindex, true, NULL);
  if (i == NULL) {
    pyErrorAndReturn(false, false);
  }
#if PY_MAJOR_VERSION < 3
  f = PyObject_CallMethodObjArgs(o, PyString_FromString("getitem"), i);
#else
  f = PyObject_CallMethodObjArgs(o, PyUnicode_FromString("getitem"), i);
#endif
  {
    foreign_t rc = address_to_term(f, val);
    ;
    pyErrorAndReturn(rc, false);
  }
}

static foreign_t python_is(term_t tobj, term_t tf) {
  PyObject *o;

  term_t lim = python_acquire_GIL();

  o = term_to_python(tobj, true, NULL);
  if (!o) {
    python_release_GIL(lim);
    pyErrorAndReturn(false, false);
  }
  foreign_t rc = python_to_term(o, tf);
  if (rc)
    PyErr_Clear();
  python_release_GIL(lim);
  pyErrorAndReturn(rc, false);
}


static foreign_t python_proc(term_t tobj) {
  PyObject *o;

  term_t lim = python_acquire_GIL();

  o = term_to_python(tobj, true, NULL);
   python_release_GIL(lim);
   bool rc = o != NULL;
   pyErrorAndReturn(rc , false);
}


static foreign_t python_slice(term_t parent, term_t indx, term_t tobj) {
  PyObject *pF, *pI;

  PyObject *p;

  // get Scope ...
  pI = term_to_python(indx, true, NULL);
  // got Scope.Exp
  // get Scope ...
  p = term_to_python(parent, true, NULL);
  // Exp
  if (!pI || !p) {
    { pyErrorAndReturn(false, false); }
  } else if ((pF = PySequence_GetSlice(p, 0, 0)) == NULL) {
    PyErr_Print();
    { pyErrorAndReturn(false, false); }
  }
  Py_DecRef(pI);
  Py_DecRef(p);
  Py_INCREF(pF);
  {
    foreign_t rc;
    rc = address_to_term(pF, tobj);
    pyErrorAndReturn(rc, false);
  }
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

  pF = term_to_python(tin, true, NULL);
  PyErr_Clear();
  if (pF == NULL) {
    { pyErrorAndReturn(false, false); }
  }
  if (PL_is_atom(targs)) {
    pArgs = NULL;
  } else {

    if (!PL_get_name_arity(targs, &aname, &arity)) {
      { pyErrorAndReturn(false, false); }
    }
    if (arity == 1 && PL_get_arg(1, targs, targ) && PL_is_variable(targ)) {
      /* ignore (_) */
      pArgs = NULL;
    } else {

      pArgs = PyTuple_New(arity);
      DebugPrintf("Tuple %p\n", pArgs);

      if (!pArgs) {
        pyErrorAndReturn(false, false);
      }
      for (i = 0; i < arity; i++) {
        PyObject *pArg;
        if (!PL_get_arg(i + 1, targs, targ)) {
          pyErrorAndReturn(false, false);
        }
        pArg = term_to_python(targ, true, NULL);
        if (pArg == NULL) {
          pyErrorAndReturn(false, false);
        }
        /* pArg reference stolen here: */
        PyTuple_SetItem(pArgs, i, pArg);
      }
    }
  }
  if (PL_is_atom(keywds)) {
    pKeywords = NULL;
  } else {
    pKeywords = term_to_python(keywds, true, NULL);
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
    { pyErrorAndReturn(false, false); }
  }
  if (pArgs)
    Py_DECREF(pArgs);
  Py_DECREF(pF);
  if (pValue == NULL) {
    pyErrorAndReturn(false, false);
  }
  out = address_to_term(pValue, tf);
  pyErrorAndReturn(out, false);
}

static foreign_t assign_python(term_t exp, term_t name) {
term_t stackp = python_acquire_GIL();
  PyObject *e = term_to_python(exp, true, NULL);

  if (e == NULL) {
    python_release_GIL(stackp);
    pyErrorAndReturn(false, false);
  }
  bool b = python_assign(name, e, NULL);
    python_release_GIL(stackp);
  pyErrorAndReturn(b, false);
}


static foreign_t python_builtin_eval(term_t caller, term_t dict, term_t out) {
  PyErr_Clear();
  PyObject *pI, *pArgs, *pOut;
  PyObject *env;
  atom_t name;
  char *s;
  int i, arity;
  term_t targ = PL_new_term_ref();

  if ((env = py_Builtin) == NULL) {
    // no point in  even trying
    { pyErrorAndReturn(false, false); }
  }
  if (PL_get_name_arity(caller, &name, &arity)) {
    if (!(s = PL_atom_chars(name))) {
      pyErrorAndReturn(false, false);
    }
    if ((pI = PyObject_GetAttrString(env, s)) == NULL) {
      PyErr_Print();
      { pyErrorAndReturn(false, false); }
    }
    Py_INCREF(pI);
  } else {
    // Prolog should make sure this never happens.
    { pyErrorAndReturn(false, false); }
  }
  pArgs = PyTuple_New(arity);
  DebugPrintf("Tuple %p\n", pArgs);
  for (i = 0; i < arity; i++) {
    PyObject *pArg;
    if (!PL_get_arg(i + 1, caller, targ)) {
      pyErrorAndReturn(false, false);
    }
    /* ignore (_) */
    if (i == 0 && PL_is_variable(targ)) {
      pArg = Py_None;
    } else {
      pArg = term_to_python(targ, true, NULL);
      if (pArg == NULL) {
        pyErrorAndReturn(false, false);
      }
    }
    /* pArg reference stolen here: */
    if (PyTuple_SetItem(pArgs, i, pArg)) {
      PyErr_Print();
      { pyErrorAndReturn(false, false); }
    }
  }
  pOut = PyObject_CallObject(pI, pArgs);
  Py_DECREF(pArgs);
  Py_DECREF(pI);
  if (pOut == NULL) {
    PyErr_Print();
    { pyErrorAndReturn(false, false); }
  }
  {
    foreign_t rc = address_to_term(pOut, out);
    ;
    pyErrorAndReturn(rc, false);
  }
}

static foreign_t python_access(term_t obj, term_t f, term_t out) {
  PyErr_Clear();
  PyObject *o = term_to_python(obj, true, NULL), *pValue, *pArgs, *pF;
  atom_t name;
  char *s = NULL;
  int i, arity;
  term_t targ = PL_new_term_ref();

  if (o == NULL) {
    pyErrorAndReturn(false, false);
  }
  if (PL_is_atom(f)) {
    if (!PL_get_atom_chars(f, &s)) {
      pyErrorAndReturn(false, false);
    }
    if ((pValue = PyObject_GetAttrString(o, s)) == NULL) {
      PyErr_Print();
      { pyErrorAndReturn(false, false); }
    }
    Py_INCREF(pValue);
    { pyErrorAndReturn(python_to_term(pValue, out), false); }
  }
  if (!PL_get_name_arity(f, &name, &arity)) {
    { pyErrorAndReturn(false, false); }
  }
  s = PL_atom_chars(name);
  if (!s) {
    pyErrorAndReturn(false, false);
  }
  if ((pF = PyObject_GetAttrString(o, s)) == NULL) {
    DebugPrintf("Function %p\n", pArgs);
    PyErr_Print();
    { pyErrorAndReturn(false, false); }
  }
  pArgs = PyTuple_New(arity);
  DebugPrintf("Tuple %p\n", pArgs);
  for (i = 0; i < arity; i++) {
    PyObject *pArg;
    if (!PL_get_arg(i + 1, f, targ)) {
      pyErrorAndReturn(false, false);
    }
    /* ignore (_) */
    if (i == 0 && PL_is_variable(targ)) {
      pArgs = Py_None;
    }
    pArg = term_to_python(targ, true, NULL);
    if (pArg == NULL) {
      pyErrorAndReturn(false, false);
    }
    /* pArg reference stolen here: */
    PyTuple_SetItem(pArgs, i, pArg);
  }
  pValue = PyObject_CallObject(pF, pArgs);
  Py_DECREF(pArgs);
  Py_DECREF(pF);
  if (pValue == NULL) {
    { pyErrorAndReturn(false, false); }
  }
  { pyErrorAndReturn(python_to_term(pValue, out), false); }
}

static foreign_t python_field(term_t parent, term_t att, term_t tobj) {
  PyObject *pF;
  atom_t name;
  char *s;
  int arity;

  if (!PL_get_name_arity(att, &name, &arity)) {
    { pyErrorAndReturn(false, false); }
  } else {
    PyObject *p;

    // got Scope.Exp
    // get Scope ...
    p = term_to_python(parent, true, NULL);
    // Exp
    if (!PL_get_name_arity(att, &name, &arity)) {
      { pyErrorAndReturn(false, false); }
    }
    s = PL_atom_chars(name);
    if (arity == 1 && !strcmp(s, "()")) {
      if (!PL_get_arg(1, att, att)) {
        pyErrorAndReturn(false, false);
      }
      if (!PL_get_name_arity(att, &name, &arity)) {
        { pyErrorAndReturn(false, false); }
      }
      s = PL_atom_chars(name);
    }
    if (!s || !p) {
      { pyErrorAndReturn(false, false); }
    } else if ((pF = PyObject_GetAttrString(p, s)) == NULL) {
      PyErr_Clear();
      { pyErrorAndReturn(false, false); }
    }
  }
  {
    foreign_t rc;
    rc = address_to_term(pF, tobj);
    pyErrorAndReturn(rc, false);
  }
}

static foreign_t python_main_module(term_t mod) {
  {
    foreign_t rc;
    PyErr_Clear();
    rc = address_to_term(py_Main, mod);
    pyErrorAndReturn(rc, false);
  }
}

static foreign_t python_function(term_t tobj) {
  PyErr_Clear();
  PyObject *obj = term_to_python(tobj, true, NULL);
  foreign_t rc = PyFunction_Check(obj);

  pyErrorAndReturn(rc, false);
}

foreign_t python_builtin(term_t out) {
  {
    foreign_t rc;
    PyErr_Clear();
    rc = address_to_term(py_Builtin, out);
    pyErrorAndReturn(rc, false);
  }
}

static foreign_t python_run_file(term_t file) {
  char *s;
  size_t len;
  char si[256];
  s = si;
  PyErr_Clear();
  if (PL_get_nchars(file, &len, &s, CVT_ALL | CVT_EXCEPTION)) {
#if PY_MAJOR_VERSION < 3
    PyObject *PyFileObject = PyFile_FromString(si, "r");
    PyRun_SimpleFileEx(PyFile_AsFile(PyFileObject), "test.py", 1);
#else
    FILE *f = fopen(s, "r");
    if (f == NULL) {
      pyErrorAndReturn(false, false);
    }
    PyRun_SimpleFileEx(f, s, 1);
#endif
    {
      { pyErrorAndReturn(true, false); }
    }
  }
  { pyErrorAndReturn(false, false); }
}

extern PyThreadState *YAP_save;

static foreign_t python_run_command(term_t cmd) {
  char *s;
  bool rc = false;
  size_t len;
  char si[256];

  PyErr_Clear();
  s = si;
  if (PL_get_nchars(cmd, &len, &s, CVT_ALL | CVT_EXCEPTION)) {
    PyRun_SimpleString(s);
    rc = true;
  }
  pyErrorAndReturn(rc, false);
}

static foreign_t python_run_script(term_t cmd, term_t fun) {
  char si[256], sf[256];
  size_t len = 255, len1 = 255;
  PyObject *pName, *pModule, *pFunc;
  PyObject *pArgs = NULL, *pValue;
  char *s;

  PyErr_Clear();
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
    PyErr_Clear();
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
          { pyErrorAndReturn(false, false); }
        }
      } else {
        pyErrorHandler();
        if (PyErr_Occurred())
          PyErr_Print();
        fprintf(stderr, "Cannot find function \"%s\"\n", sf);
      }
      Py_XDECREF(pFunc);
      Py_DECREF(pModule);
    } else {
      PyErr_Print();
      { pyErrorAndReturn(false, false); }
    }
    { pyErrorAndReturn(true, false); }
  }
  { pyErrorAndReturn(false, false); }
}

static foreign_t python_export(term_t t, term_t pl) {
  foreign_t rc = false;
  PyErr_Clear();
  if (PL_is_functor(t, FUNCTOR_pointer1)) {
    void *ptr;
    term_t targ = PL_new_term_ref();

    if (!PL_get_arg(1, t, targ)) {
      pyErrorAndReturn(false, false);
    }
    if (!PL_get_pointer(targ, &ptr)) {
      pyErrorAndReturn(false, false);
    }
    Py_INCREF((PyObject *)ptr);
    /* pyErrorAndReturn( __main__, false) */
    rc = python_to_term((PyObject *)ptr, pl);
  }
  pyErrorAndReturn(rc, false);
}


/**
 * @pred python_import(MName, Mod)
 *   Import a python module to the YAP environment.
 *
 * @param  mname module name, either an atom or a sequence of atoms,
 *   eg os.sys
 * @param  mod   the pointer to the Python object
 * @return       success?
 */
static int python_import(term_t mname, term_t mod) {
  PyObject *pName;

term_t t0 = python_acquire_GIL();
  term_t arg = PL_new_term_ref();
  char s0[MAXPATHLEN], *s = s0;
  while (true) {
    size_t len;

    len = (MAXPATHLEN - 1) - (s - s0);
    if (PL_is_pair(mname)) {
      char *sa = NULL;
      if (!PL_get_arg(1, mname, arg) || !PL_get_atom_chars(arg, &sa) ||
          !PL_get_arg(2, mname, mname)) {
            python_release_GIL(t0);
        pyErrorAndReturn(false, false);
      }
      PL_get_atom_chars(arg, &sa);
      strcpy(s, sa);
      s += strlen(s);
      *s++ = '.';
      s[0] = '\0';
    } else if (!PL_get_nchars(mname, &len, &s,
                              CVT_ATOM | CVT_EXCEPTION | REP_UTF8)) {
      python_release_GIL(t0);
      pyErrorAndReturn(false, false);
    } else {
      break;
    }
  }
#if PY_MAJOR_VERSION < 3
  pName = PyString_FromString(s0);
#else
  pName = PyUnicode_FromString(s0);
#endif
            python_release_GIL(t0);
if (pName == NULL) {
    pyErrorAndReturn(false, false);
  }

  PyObject *pModule = PyImport_Import(pName);

  Py_DECREF(pName);
  if (pModule == NULL) {
#if EXTRA_MESSSAGES
    if (PyErr_Occurred())
      PyErr_Print();
    PyErr_Clear();
#endif
    { pyErrorAndReturn(false, false); }
  }
  {
    foreign_t rc = address_to_term(pModule, mod);
    pyErrorAndReturn(rc, false);
  }
}

static foreign_t python_to_rhs(term_t inp, term_t t) {
  PyObject *pVal;
  PyErr_Clear();
  pVal = term_to_python(inp, true, NULL);
  if (pVal == NULL)
    pyErrorAndReturn(false, false);
  pyErrorAndReturn(address_to_term(pVal, t), false);
}

// static PyThreadState *_saveP = NULL;
static bool _threaded = false;

/*
static YAP_Int
  p_python_ensure(term_t ptr)
  {
     PyGILState_STATE _tState = PyGILState_Ensure();
     pyErrorAndReturn( PL_unify_int64(ptr, _tState), false);
  }

static YAP_Int
  p_python_release(term_t ptr)
  {

    PyGILState_STATE _tState;
    PL_get_int64( ptr, &_tState);
    PyGILState_Release( _tState );
  pyErrorAndReturn( true, false);
  }
*/

int _locked = 0;
PyThreadState *tstate;

static YAP_Int p_python_threaded(void) {

  PyErr_Clear();
  // PyEval_ReleaseThread(tstate);
  // _threaded = true;
  //    _locked = 0;
  pyErrorAndReturn(true, false);
}

static PyGILState_STATE gstate;

term_t python_acquire_GIL(void) {
  term_t curSlot = 1; //PL_new_term_ref();
  if (!_threaded)
    pyErrorAndReturn(curSlot, false);
  //   extern int Yap_do_low_level_trace;
  // Yap_do_low_level_trace = 1;
  // f[rintf( stderr, "++%d\n", ++_locked);
  //  if (_locked > 0) { _locked++  ; }
  // else
  gstate = PyGILState_Ensure();
  pyErrorAndReturn(curSlot, false);
}

bool python_release_GIL(term_t curBlock) {
  PyErr_Clear();
 // PL_reset_term_refs(curBlock);
  if (_threaded) {
      PyGILState_Release(gstate);
  }
  pyErrorAndReturn(true, false);
}

//: prolog: release python

install_t install_pypreds(void) {
  PL_register_foreign("python_builtin_eval", 3, python_builtin_eval, 0);
  PL_register_foreign("python_builtin", 1, python_builtin, 0);
  PL_register_foreign("python_import", 2, python_import, 0);
  PL_register_foreign("python_to_rhs", 2, python_to_rhs, 0);
  PL_register_foreign("python_len", 2, python_len, 0);
  PL_register_foreign("python_is", 2, python_is, 0);
  PL_register_foreign("python_dir", 2, python_dir, 0);
  PL_register_foreign("python_apply", 4, python_apply, 0);
  PL_register_foreign("python_index", 3, python_index, 0);
  PL_register_foreign("python_field", 3, python_field, 0);
  PL_register_foreign("python_assign", 2, assign_python, 0);
  PL_register_foreign("python_export", 2, python_export, 0);
  PL_register_foreign("python_function", 1, python_function, 0);
  PL_register_foreign("python_slice", 4, python_slice, 0);
  PL_register_foreign("python_run_file", 1, python_run_file, 0);
  PL_register_foreign("python_proc", 1, python_proc, 0);
  PL_register_foreign("python_run_command", 1, python_run_command, 0);
  PL_register_foreign("python_run_script", 2, python_run_script, 0);
  PL_register_foreign("python_main_module", 1, python_main_module, 0);
  PL_register_foreign("python_import", 2, python_import, 0);
  PL_register_foreign("python_access", 3, python_access, 0);
  PL_register_foreign("python_threaded", 0, p_python_threaded, 0);
}
