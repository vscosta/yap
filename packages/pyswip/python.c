#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#ifdef HAVE_STAT
#undef HAVE_STAT
#endif
#include <Python.h>
#include <assert.h>

static atom_t ATOM_true, ATOM_false;

static foreign_t
init_python(void)
{ 
  Py_Initialize();

  return TRUE;
}

static foreign_t
end_python(void)
{ 
  Py_Finalize();

  return TRUE;
}

static int
python_import(term_t mname, term_t mod)
{ 
  char *s;
  size_t len;
  PyObject *pName, *pModule;

  if ( !PL_get_nchars(mname, &len, &s, CVT_ALL|CVT_EXCEPTION) ) {  
    return FALSE;
  }
  pName = PyString_FromString(s);
  if (pName == NULL) {
    return FALSE;
  }

  PyRun_SimpleString("import sys");
  PyRun_SimpleString("sys.path.append(\"/Users/vsc/Yap/bins/osx/packages/pyswip\")");
  //PyRun_SimpleString("import multiply");
  pModule = PyImport_Import(pName);
  PyErr_Print();
  Py_DECREF(pName);
  if (pModule == NULL) {
    return FALSE;
  }
  return PL_unify_pointer(mod, (void *)pModule);
}

static foreign_t
python_f(term_t tmod, term_t fname, term_t tf)
{ 
  char *s;
  size_t len;
  PyObject *pF, *pModule;
  void **mp = (void *)&pModule;

  if ( !PL_get_pointer(tmod, mp) ) {
    PyObject *pName;

    if ( !PL_get_nchars(fname, &len, &s, CVT_ALL|CVT_EXCEPTION) ) {  
      return FALSE;
    }
    pName = PyString_FromString(s);
    if (pName == NULL) {
      return FALSE;
    }
    pModule = PyImport_Import(pName);
  }
  if ( !PL_get_nchars(fname, &len, &s, CVT_ALL|CVT_EXCEPTION) ) {  
    return FALSE;
  }
  pF = PyObject_GetAttrString(pModule, s);
  if (pF == NULL || ! PyCallable_Check(pF)) {
    return FALSE;
  }
  return PL_unify_pointer(tf, (void *)pF);
}

static PyObject *
term_to_python(term_t t)
{
  switch (PL_term_type(t)) {
  case PL_VARIABLE:
    return NULL;
  case PL_ATOM:
    {
      char *s;
      
      if (!PL_get_atom_chars(t, &s))
	return NULL;
      return PyByteArray_FromStringAndSize(s, strlen(s) );
    }
  case PL_INTEGER:
    {
      int64_t j;
      if (!PL_get_int64_ex(t, &j))
	return NULL;
      return PyInt_FromLong(j);
    }
  case PL_STRING:
    {
      char *s;
      size_t len;

      if (!PL_get_string_chars(t, &s, &len))
	return NULL;
      return PyByteArray_FromStringAndSize(s, len );
    }
  case PL_FLOAT:
    {
      double fl;
      if (!PL_get_float(t, &fl))
	return NULL;
      return PyFloat_FromDouble( fl );
    }
  case PL_TERM:
    if (PL_is_list(t)) {
      size_t len, i;
      term_t tail = PL_new_term_ref(), arg;
      PyObject *out;

      PL_skip_list(t, tail, &len);
      if (!PL_get_nil(tail))
	return NULL;
      arg = tail;
      out = PyList_New(len);
      if (!out)
	return NULL;
      
      for (i=0; i< len; i++) {
	if (!PL_get_list(t, arg, t)) {
	  return NULL;
	}
	if (PyList_SetItem(out, i, term_to_python(arg)) < 0)
	  return NULL;
      }
      return out;
    }
    return NULL;
  }
  return NULL;
}

static foreign_t
output_python_term(PyObject *pVal, term_t t)
{
  if (PyLong_Check(pVal)) {
    return PL_unify_int64(t, PyLong_AsLong(pVal));
  } else if (PyInt_Check(pVal)) {
    return PL_unify_int64(t, PyInt_AsLong(pVal));
  } else if (PyBool_Check(pVal)) {
    if (pVal == Py_True) {
      return PL_unify_atom(t, ATOM_true);
    } else {
      return PL_unify_atom(t, ATOM_false);
    }
  } else if (PyFloat_Check(pVal)) {
    return PL_unify_float(t, PyFloat_AsDouble(pVal));
  } else if (PyByteArray_Check(pVal)) {
    atom_t tmp_atom = PL_new_atom(PyByteArray_AsString(pVal));
    return PL_unify_atom(t, tmp_atom);
  } else if (PyList_Check(pVal)) {
    term_t to = PL_new_term_ref();
    Py_ssize_t i, sz = PyList_GET_SIZE(pVal);

    for (i = 0; i < sz; i++) {
      if (!PL_unify_list(t, to, t) ||
	  !output_python_term(PyList_GetItem(pVal, i), to))
	return FALSE;
    }
    return PL_unify_nil(t);
  } else {
    return FALSE;
  }
}

static foreign_t
python_apply(term_t tin, term_t targs, term_t tf)
{ 
  PyObject *pF, *pArgs, *pValue;
  void **mpf = (void *)&pF;
  int i, arity;
  atom_t aname;
  foreign_t out;
  term_t targ = PL_new_term_ref();

  if ( !PL_get_pointer(tin, mpf) ) {
    return FALSE;
  }
  if (! PL_get_name_arity( targs, &aname, &arity) ) {
    return FALSE;
  }
  pArgs = PyTuple_New(arity);
  for (i = 0 ; i < arity; i++) {
    PyObject *pArg;
    if (! PL_get_arg(i+1, targs, targ) )
      return FALSE;
    pArg = term_to_python(targ);
    if (pArg == NULL)
      return FALSE;
    /* pArg reference stolen here: */
    PyTuple_SetItem(pArgs, i,  pArg);
  }
  pValue = PyObject_CallObject(pF, pArgs);
  Py_DECREF(pArgs);
  out =  output_python_term(pValue, tf);
  Py_DECREF(pValue);
  return out;
}

static foreign_t
python_run_command(term_t cmd)
{ 
  char *s;
  size_t len;

  if ( PL_get_nchars(cmd, &len, &s, CVT_ALL|CVT_EXCEPTION) ) {  
    PyRun_SimpleString(s);

    return TRUE;
  }
  return FALSE;
}

install_t install_python(void);

install_t
install_python(void)
{ // FUNCTOR_dot2 = PL_new_functor(PL_new_atom("."), 2);
  // FUNCTOR_equal2 = PL_new_functor(PL_new_atom("="), 2);
  // FUNCTOR_boolop1 = PL_new_functor(PL_new_atom("@"), 1);
  ATOM_true  = PL_new_atom("true");
  ATOM_false = PL_new_atom("false");

  PL_register_foreign("init_python",	  0, init_python,      0);
  PL_register_foreign("end_python",	  0, end_python,       0);
  PL_register_foreign("python_import",	  2, python_import,       0);
  PL_register_foreign("python_f",	  3, python_f,       0);
  PL_register_foreign("python_apply",	  3, python_apply,       0);
  PL_register_foreign("python_run_command",	  1, python_run_command,       0);
}

