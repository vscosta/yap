


#include "Yap.h"

#include "py4yap.h"

PyObject *py_Main;

void pyErrorHandler__(int line, const char *file, const char *code) {
  // this code is called if a Python error is found.
  // int lvl = push_text_stack();
  //   PyErr_Fetch(&type, &val, NULL);
  //   PyErr_Print();
  // Yap_ThrowError__(file,code,line,0, SYSTEM_ERROR_RUNTIME_PYTHON ,"Python
  // Error %s: %s",PyUnicode_AsUTF8(PyObject_Str(type)),
  // PyUnicode_AsUTF8(PyObject_Str(val)));
};

static foreign_t python_len(term_t tobj, term_t tf) {
  Py_ssize_t len;
  PyObject *o;
  PyStart();

  o = term_to_python(tobj, true, NULL, true);
  if (o == NULL) {
    pyErrorAndReturn(false);
  }
  len = PyObject_Length(o);
  pyErrorAndReturn(PL_unify_int64(tf, len));
}

static foreign_t python_represent( term_t name, term_t tobj) {
  term_t stackp = python_acquire_GIL();
  PyObject *e;
  PyStart();

  e = term_to_python(tobj, false, NULL, false);
  if (e == NULL) {
    python_release_GIL(stackp);
    pyErrorAndReturn(false);
  }
  foreign_t b = python_assign(name, e, NULL);
  python_release_GIL(stackp);
  pyErrorAndReturn(b);
}



static foreign_t python_clear_errors(void) {
  PyErr_Clear();
  return true;
}

static foreign_t python_dir(term_t tobj, term_t tf) {
  PyObject *dir;
  PyObject *o;
  PyStart();

  o = term_to_python(tobj, true, NULL, true);
  if (o == NULL) {
    pyErrorAndReturn(false);
  }
  dir = PyObject_Dir(o);
  {
    foreign_t rc = address_to_term(dir, tf);
    ;
    pyErrorAndReturn(rc);
  }
}

static foreign_t python_index(term_t tobj, term_t tindex, term_t val) {
  PyObject *i;
  PyObject *o;
  PyObject *f;
  PyStart();

  o = term_to_python(tobj, true, NULL, true);
  if (o == NULL) {
    pyErrorAndReturn(false);
  }
  if (!PySequence_Check(o)) {
    pyErrorAndReturn(false);
  }
  i = term_to_python(tindex, true, NULL, true);
  if (i == NULL) {
    pyErrorAndReturn(false);
  }
#if PY_MAJOR_VERSION < 3
  f = PyObject_CallMethodObjArgs(o, PyString_FromString("getitem"), i);
#else
  f = PyObject_CallMethodObjArgs(o, PyUnicode_FromString("getitem"), i);
#endif
  {
    foreign_t rc = address_to_term(f, val);
    ;
    pyErrorAndReturn(rc);
  }
}

static foreign_t python_is(term_t tobj, term_t tf) {
  PyObject *o;
  PyStart();

  term_t lim = python_acquire_GIL();

  o = term_to_python(tobj, true, NULL, true);
  if (!o) {
    python_release_GIL(lim);
    pyErrorAndReturn(false);
  }
  foreign_t rc = python_to_term(o, tf);
  if (rc)
    PyErr_Clear();
  python_release_GIL(lim);
  pyErrorAndReturn(rc);
}

static foreign_t python_proc(term_t tobj) {
  PyStart();
  PyObject *o;
  term_t lim = python_acquire_GIL();

  o = term_to_python(tobj, true, NULL, true);
  python_release_GIL(lim);
  foreign_t rc = o != NULL;
  pyErrorAndReturn(rc);
}

static foreign_t python_slice(term_t parent, term_t indx, term_t tobj) {
  PyObject *pF, *pI;

  PyObject *p;

  // get Scope ...
  pI = term_to_python(indx, true, NULL, true);
  // got Scope.Exp
  // get Scope ...
  p = term_to_python(parent, true, NULL, true);
  // Exp
  if (!pI || !p) {
       pyErrorAndReturn(false);
  } else if ((pF = PySequence_GetSlice(p, 0, 0)) == NULL) {
    PyErr_Print();
    { pyErrorAndReturn(false); }
  }
  Py_DecRef(pI);
  Py_DecRef(p);
  Py_INCREF(pF);
  {
    foreign_t rc;
    rc = address_to_term(pF, tobj);
    pyErrorAndReturn(rc);
  }
}

static foreign_t python_apply(term_t tin, term_t targs, term_t keywds,
                              term_t tf) {
  PyStart();
  PyObject *pF;
  PyObject *pArgs, *pKeywords;
  PyObject *pValue;
  size_t i, arity;
  atom_t aname;
  foreign_t out;
  term_t targ = PL_new_term_ref();

  pF = term_to_python(tin, true, NULL, true);
  if (pF == NULL) {
    {
      pyErrorAndReturn(false);
    }
  }
  if (PL_is_atom(targs)) {
    pArgs = NULL;
  } else {

    if (!PL_get_name_arity(targs, &aname, &arity)) {
      {
        pyErrorAndReturn(false);
      }
    }
    if (arity == 1 && PL_get_arg(1, targs, targ) && PL_is_variable(targ)) {
      /* ignore (_) */
      pArgs = NULL;
    } else {

      pArgs = PyTuple_New(arity);
      DebugPrintf("Tuple %p\n", pArgs);

      if (!pArgs) {
        pyErrorAndReturn(false);
      }
      for (i = 0; i < arity; i++) {
        PyObject *pArg;
        if (!PL_get_arg(i + 1, targs, targ)) {
          pyErrorAndReturn(false);
        }
        pArg = term_to_python(targ, true, NULL, true);
        if (pArg == NULL) {
          pyErrorAndReturn(false);
        }
        /* pArg reference stolen here: */
        PyTuple_SetItem(pArgs, i, pArg);
      }
    }
  }
  if (PL_is_atom(keywds)) {
    pKeywords = NULL;
  } else {
    pKeywords = term_to_python(keywds, true, NULL, true);
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
    { pyErrorAndReturn(false); }
  }
  if (pArgs)
    Py_DECREF(pArgs);
  Py_DECREF(pF);
  if (pValue == NULL) {
    pyErrorAndReturn(false);
  }
  out = address_to_term(pValue, tf);
  pyErrorAndReturn(out);
}

static foreign_t assign_python(term_t exp, term_t name) {
  PyStart();
  term_t stackp = python_acquire_GIL();
  PyObject *e = term_to_python(exp, true, NULL, true);

  if (e == NULL) {
    python_release_GIL(stackp);
    pyErrorAndReturn(false);
  }
  foreign_t b = python_assign(name, e, NULL);
  python_release_GIL(stackp);
  pyErrorAndReturn(b);
}


static foreign_t python_string_to(term_t f) {
  if (PL_is_atom(f)) {
    char *s = NULL;
    if (!PL_get_chars(f, &s, CVT_ATOM |CVT_STRING | CVT_EXCEPTION | REP_UTF8)) {
      pyErrorAndReturn(false);
    }
    if (!strcmp(s,"atom")) {
      pyStringToString = false;
      return true;
    }
    if (!strcmp(s,"string")) {
      pyStringToString = true;
      return true;
    }
  }
  return false;
}

static foreign_t python_builtin_eval(term_t caller, term_t dict, term_t out) {
  PyStart();
  PyObject *pI, *pArgs, *pOut;
  PyObject *env;
  atom_t name;
  char *s;
  size_t i, arity;
  term_t targ = PL_new_term_ref();

  if ((env = py_Builtin) == NULL) {
    // no point in  even trying
    { pyErrorAndReturn(false); }
  }
  if (PL_get_name_arity(caller, &name, &arity)) {
    if (!(s = PL_atom_chars(name))) {
      pyErrorAndReturn(false);
    }
    if ((pI = PyObject_GetAttrString(env, s)) == NULL) {
      PyErr_Print();
      { pyErrorAndReturn(false); }
    }
    Py_INCREF(pI);
  } else {
    // Prolog should make sure this never happens.
    { pyErrorAndReturn(false); }
  }
  pArgs = PyTuple_New(arity);
  DebugPrintf("Tuple %p\n", pArgs);
  for (i = 0; i < arity; i++) {
    PyObject *pArg;
    if (!PL_get_arg(i + 1, caller, targ)) {
      pyErrorAndReturn(false);
    }
    /* ignore (_) */
    if (i == 0 && PL_is_variable(targ)) {
      pArg = Py_None;
    } else {
      pArg = term_to_python(targ, true, NULL, true);
      if (pArg == NULL) {
        pyErrorAndReturn(false);
      }
    }
    /* pArg reference stolen here: */
    if (PyTuple_SetItem(pArgs, i, pArg)) {
      PyErr_Print();
      { pyErrorAndReturn(false); }
    }
  }
  pOut = PyObject_CallObject(pI, pArgs);
  Py_DECREF(pArgs);
  Py_DECREF(pI);
  if (pOut == NULL) {
    PyErr_Print();
    { pyErrorAndReturn(false); }
  }
  {
    foreign_t rc = address_to_term(pOut, out);
    ;
    pyErrorAndReturn(rc);
  }
}

static foreign_t python_access(term_t obj, term_t f, term_t out) {
  PyStart();
  PyObject *o = term_to_python(obj, true, NULL, true), *pValue, *pArgs, *pF;
  atom_t name;
  char *s = NULL;
  size_t i, arity;
  term_t targ = PL_new_term_ref();

  if (o == NULL) {
    pyErrorAndReturn(false);
  }
  if (PL_is_atom(f)) {
    if (!PL_get_chars(f, &s, CVT_ALL | CVT_EXCEPTION | REP_UTF8)) {
      pyErrorAndReturn(false);
    }
    if ((pValue = PyObject_GetAttrString(o, s)) == NULL) {
      PyErr_Print();
      { pyErrorAndReturn(false); }
    }
    Py_INCREF(pValue);
    { pyErrorAndReturn(python_to_term(pValue, out)); }
  }
  if (!PL_get_name_arity(f, &name, &arity)) {
    {
      pyErrorAndReturn(false);
    }
  }
  s = PL_atom_chars(name);
  if (!s) {
    pyErrorAndReturn(false);
  }
  if ((pF = PyObject_GetAttrString(o, s)) == NULL) {
    DebugPrintf("Function %p\n", o);
    PyErr_Print();
    { pyErrorAndReturn(false); }
  }
  pArgs = PyTuple_New(arity);
  DebugPrintf("Tuple %p\n", pArgs);
  for (i = 0; i < arity; i++) {
    PyObject *pArg;
    if (!PL_get_arg(i + 1, f, targ)) {
      pyErrorAndReturn(false);
    }
    /* ignore (_) */
    if (i == 0 && PL_is_variable(targ)) {
      pArgs = Py_None;
    }
    pArg = term_to_python(targ, true, NULL, true);
    if (pArg == NULL) {
      pyErrorAndReturn(false);
    }
    /* pArg reference stolen here: */
    PyTuple_SetItem(pArgs, i, pArg);
  }
  pValue = PyObject_CallObject(pF, pArgs);
  Py_DECREF(pArgs);
  Py_DECREF(pF);
  if (pValue == NULL) {
    {
      pyErrorAndReturn(false);
    }
  }
  { pyErrorAndReturn(python_to_term(pValue, out)); }
}

static foreign_t python_field(term_t parent, term_t att, term_t tobj) {
  PyObject *pF;
  atom_t name;
  char *s;
  size_t arity;
  PyStart();
  if (!PL_get_name_arity(att, &name, &arity)) {
    {
      pyErrorAndReturn(false);
    }
  } else {
    PyObject *p;

    // got Scope.Exp
    // get Scope ...
    p = term_to_python(parent, true, NULL, true);
    // Exp
    if (!PL_get_name_arity(att, &name, &arity)) {
      {
        pyErrorAndReturn(false);
      }
    }
    s = PL_atom_chars(name);
    if (arity == 1 && !strcmp(s, "()")) {
      if (!PL_get_arg(1, att, att)) {
        pyErrorAndReturn(false);
      }
      if (!PL_get_name_arity(att, &name, &arity)) {
        {
          pyErrorAndReturn(false);
        }
      }
      s = PL_atom_chars(name);
    }
    if (!s || !p) {
      {
        pyErrorAndReturn(false);
      }
    } else if ((pF = PyObject_GetAttrString(p, s)) == NULL) {
      PyErr_Clear();
      { pyErrorAndReturn(false); }
    }
  }
  {
    foreign_t rc;
    rc = address_to_term(pF, tobj);
    pyErrorAndReturn(rc);
  }
}

static foreign_t python_main_module(term_t mod) {
  {
    foreign_t rc;
    PyStart();
    rc = address_to_term(py_Main, mod);
    pyErrorAndReturn(rc);
  }
}

static foreign_t python_function(term_t tobj) {
  PyStart();
  PyObject *obj = term_to_python(tobj, true, NULL, true);
  foreign_t rc = PyFunction_Check(obj);

  pyErrorAndReturn(rc);
}

foreign_t python_builtin(term_t out) {
  {
    foreign_t rc;
    PyStart();
    rc = address_to_term(py_Builtin, out);
    pyErrorAndReturn(rc);
  }
}

static foreign_t python_run_file(term_t file) {
  char *s;
  size_t len;
  char si[256];
  s = si;
  PyStart();
  if (PL_get_nchars(file, &len, &s, CVT_ALL | CVT_EXCEPTION)) {
#if PY_MAJOR_VERSION < 3
    PyObject *PyFileObject = PyFile_FromString(si, "r");
    PyRun_SimpleFileEx(PyFile_AsFile(PyFileObject), "test.py", 1);
#else
    FILE *f = fopen(s, "r");
    if (f == NULL) {
      pyErrorAndReturn(false);
    }
    PyRun_SimpleFileEx(f, s, 1);
#endif
    {
      { pyErrorAndReturn(true); }
    }
  }
  { pyErrorAndReturn(false); }
}

extern PyThreadState *YAP_save;

static foreign_t python_run_command(term_t cmd) {
  char *s;
  foreign_t rc = false;
  size_t len;
  char si[256];

  PyStart();
  s = si;
  if (PL_get_nchars(cmd, &len, &s, CVT_ALL | CVT_EXCEPTION)) {
    PyRun_SimpleString(s);
    rc = true;
  }
  pyErrorAndReturn(rc);
}

static foreign_t python_run_script(term_t cmd, term_t fun) {
  char si[256], sf[256];
  size_t len = 255, len1 = 255;
  PyObject *pName, *pModule, *pFunc;
  PyObject *pArgs = NULL, *pValue;
  char *s;

  PyStart();
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
          { pyErrorAndReturn(false); }
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
      { pyErrorAndReturn(false); }
    }
    { pyErrorAndReturn(true); }
  }
  { pyErrorAndReturn(false); }
}

static foreign_t python_export(term_t t, term_t pl) {
  foreign_t rc = false;
  PyStart();
  if (PL_is_functor(t, FUNCTOR_pointer1)) {
    void *ptr;
    term_t targ = PL_new_term_ref();

    if (!PL_get_arg(1, t, targ)) {
      pyErrorAndReturn(false);
    }
    if (!PL_get_pointer(targ, &ptr)) {
      pyErrorAndReturn(false);
    }
    Py_INCREF((PyObject *)ptr);
    /* pyErrorAndReturn( __main__) */
    rc = python_to_term((PyObject *)ptr, pl);
  }
  pyErrorAndReturn(rc);
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
  foreign_t do_as = false;
  PyStart();

  char s0[MAXPATHLEN], *s = s0;
  s[0] = '\0';
  const char *sn, *as = NULL;
  Term t = Deref(ARG1), sm;
  if (IsApplTerm(t)) {
    Functor f = (Functor)*RepAppl(t);
    if (f != FunctorAs)
      return false;
    do_as = true;
    sm = ArgOfTerm(2, t);
    if (IsAtomTerm(sm))
      as = RepAtom(AtomOfTerm(sm))->StrOfAE;
    else if (IsStringTerm(sm))
      as = StringOfTerm(sm);
    else
      return false;
    t = ArgOfTerm(1, t);
  }
  while (IsPairTerm(t)) {
    Term ti = HeadOfTerm(t);
    t = TailOfTerm(t);
    if (IsAtomTerm(ti))
      sn = RepAtom(AtomOfTerm(ti))->StrOfAE;
    else if (IsStringTerm(ti))
      sn = StringOfTerm(ti);
    else
      return false;
    strcat(s, sn);
    //get_mod(s);
    strcat(s, ".");
  }
  if (IsAtomTerm(t))
    sn = RepAtom(AtomOfTerm(t))->StrOfAE;
  else if (IsStringTerm(t))
    sn = StringOfTerm(t);
 else
    return false;
  strcat(s, sn);
  term_t t0 = python_acquire_GIL();
#if PY_MAJOR_VERSION < 3
  pName = PyString_FromString(s0);
#else
  pName = PyUnicode_FromString(s0);
#endif
  if (pName == NULL) {
    python_release_GIL(t0);
    pyErrorAndReturn(false);
  }

  PyObject *pModule = PyImport_Import(pName);

  Py_XDECREF(pName);
  if (pModule == NULL) {
    python_release_GIL(t0);

    pyErrorAndReturn(false);
  }
  {
    foreign_t rc = address_to_term(pModule, mod);

    if (do_as) {
      PyModule_AddObject(py_Main, as, pModule);
    }
    python_release_GIL(t0);
    pyErrorAndReturn(rc);
  }
}

static foreign_t python_to_rhs(term_t inp, term_t t) {
  PyObject *pVal;
  PyStart();
  pVal = term_to_python(inp, true, NULL, true);
  if (pVal == NULL)
    pyErrorAndReturn(false);
  pyErrorAndReturn(address_to_term(pVal, t));
}

// static PyThreadState *_saveP = NULL;
static foreign_t _threaded = true;

/*
static YAP_Int
  p_python_ensure(term_t ptr)
  {
     PyGILState_STATE _tState = PyGILState_Ensure();
     pyErrorAndReturn( PL_unify_int64(ptr), false);
  }

static YAP_Int
  p_python_release(term_t ptr)
  {

    PyGILState_STATE _tState;
    PL_get_int64( ptr, &_tState);
    PyGILState_Release( _tState );
  pyErrorAndReturn( true);
  }
*/

int _locked = 0;
PyThreadState *tstate;

static YAP_Int p_python_threaded(void) {

  PyStart();
  // PyEval_ReleaseThread(tstate);
  // _threaded = true;
  //    _locked = 0;
  pyErrorAndReturn(true);
}

static PyGILState_STATE gstates[64];
static int gstatei = 0;

term_t python_acquire_GIL(void) {
  term_t curSlot = PL_new_term_ref();
  //   extern int Yap_do_low_level_trace;
  // Yap_do_low_level_trace = 1;
  // fprintf( stderr, "++%d\n", ++_locked);
  //  if (_locked > 0) { _locked++  ; }
  // else
  if (_threaded) {
    gstates[gstatei] = PyGILState_Ensure();
  }
  PL_put_integer(curSlot, gstatei++);
  return curSlot;
}
bool python_release_GIL(term_t curBlock) {
  int gstateix;
  gstatei--;
  PL_get_integer(curBlock, &gstateix);
  PL_reset_term_refs(curBlock);
  if (gstatei != gstateix) {
    if (gstateix > gstatei) {
      fprintf(stderr, "gstateix(%d) > gstatei(%d)\n", gstateix, gstatei);
      return false;
    } else {
      fprintf(stderr, "gstateix(%d) < gstatei(%d)\n", gstateix, gstatei);
      return false;
    }
  }
  if (_threaded) {
    PyGILState_Release(gstates[gstatei]);
  }
  pyErrorAndReturn(true);
}

install_t install_pypreds(void) {

  PL_register_foreign_in_module("python", "python_builtin_eval", 3, python_builtin_eval, 0);
  PL_register_foreign_in_module("python", "python_builtin", 1, python_builtin, 0);
  PL_register_foreign_in_module("python", "python_import", 2, python_import, 0);
  PL_register_foreign_in_module("python", "python_to_rhs", 2, python_to_rhs, 0);
  PL_register_foreign_in_module("python", "python_len", 2, python_len, 0);
  PL_register_foreign_in_module("python", "python_is", 2, python_is, 0);
  PL_register_foreign_in_module("python", "python_dir", 2, python_dir, 0);
  PL_register_foreign_in_module("python", "python_apply", 4, python_apply, 0);
  PL_register_foreign_in_module("python", "python_index", 3, python_index, 0);
  PL_register_foreign_in_module("python", "python_field", 3, python_field, 0);
  PL_register_foreign_in_module("python", "python_assign", 2, assign_python, 0);
  PL_register_foreign_in_module("python", "python_represents", 2, python_represent, 0);
  PL_register_foreign_in_module("python", "python_export", 2, python_export, 0);
  PL_register_foreign_in_module("python", "python_function", 1, python_function, 0);
  PL_register_foreign_in_module("python", "python_slice", 4, python_slice, 0);
  PL_register_foreign_in_module("python", "python_run_file", 1, python_run_file, 0);
  PL_register_foreign_in_module("python", "python_proc", 1, python_proc, 0);
  PL_register_foreign_in_module("python", "python_run_command", 1, python_run_command, 0);
  PL_register_foreign_in_module("python", "python_run_script", 2, python_run_script, 0);
  PL_register_foreign_in_module("python", "python_main_module", 1, python_main_module, 0);
  PL_register_foreign_in_module("python", "python_import", 2, python_import, 0);
  PL_register_foreign_in_module("python", "python_access", 3, python_access, 0);
  PL_register_foreign_in_module("python", "python_threaded", 0, p_python_threaded, 0);
  PL_register_foreign_in_module("python", "python_clear_errors", 0, python_clear_errors, 0);
  PL_register_foreign_in_module("python", "python_string_to", 1, python_string_to, 0);

  
  init_python_vfs();
}
