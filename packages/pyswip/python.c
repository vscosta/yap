#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#ifdef HAVE_STAT
#undef HAVE_STAT
#endif
#include <Python.h>
#include <assert.h>

static atom_t ATOM_true, ATOM_false;

static functor_t FUNCTOR_dollar1,
  FUNCTOR_pointer1, 
  FUNCTOR_add2, 
  FUNCTOR_sub2,
  FUNCTOR_mul2,
  FUNCTOR_div2,
  FUNCTOR_hat2;

static PyObject *py_Main;

static PyObject *
term_to_python(term_t t)
{
  // Yap_DebugPlWrite(YAP_GetFromSlot(t));        fprintf(stderr, " here I am\n");
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
    } else {
      functor_t fun;

      if (!PL_get_functor(t, &fun))
	return NULL;
      if (fun == FUNCTOR_dollar1) {
	char *s;
	term_t targ = PL_new_term_ref();
	if (! PL_get_arg(1, t, targ) )
	  return NULL;
	if (!PL_get_atom_chars(targ, &s))
	  return NULL;
	/* return __main__,s */
	return PyObject_GetAttrString(py_Main, s);
      } else if (fun == FUNCTOR_pointer1) {
	void *ptr;
	term_t targ = PL_new_term_ref();

	if (! PL_get_arg(1, t, targ) )
	  return NULL;
	if (!PL_get_pointer(targ, &ptr))
	  return NULL;
	/* return __main__,s */
	return (PyObject *)ptr;
      } else if (fun == FUNCTOR_add2) {
	term_t targ = PL_new_term_ref();
	PyObject *lhs, *rhs;

	if (! PL_get_arg(1, t, targ) )
	  return NULL;
	lhs = term_to_python(targ);
	if (!PyNumber_Check(lhs))
	  return NULL;
	if (! PL_get_arg(2, t, targ) )
	  return NULL;
	rhs = term_to_python(targ);
	if (!PyNumber_Check(rhs))
	  return NULL;
	return PyNumber_Add(lhs, rhs);
      } else if (fun == FUNCTOR_sub2) {
	term_t targ = PL_new_term_ref();
	PyObject *lhs, *rhs;

	if (! PL_get_arg(1, t, targ) )
	  return NULL;
	lhs = term_to_python(targ);
	if (!PyNumber_Check(lhs))
	  return NULL;
	if (! PL_get_arg(2, t, targ) )
	  return NULL;
	rhs = term_to_python(targ);
	if (!PyNumber_Check(rhs))
	  return NULL;
	return PyNumber_Subtract(lhs, rhs);
      } else if (fun == FUNCTOR_mul2) {
	term_t targ = PL_new_term_ref();
	PyObject *lhs, *rhs;

	if (! PL_get_arg(1, t, targ) )
	  return NULL;
	lhs = term_to_python(targ);
	if (!PyNumber_Check(lhs))
	  return NULL;
	if (! PL_get_arg(2, t, targ) )
	  return NULL;
	rhs = term_to_python(targ);
	if (!PyNumber_Check(rhs))
	  return NULL;
	return PyNumber_Multiply(lhs, rhs);
      } else if (fun == FUNCTOR_div2) {
	term_t targ = PL_new_term_ref();
	PyObject *lhs, *rhs;

	if (! PL_get_arg(1, t, targ) )
	  return NULL;
	lhs = term_to_python(targ);
	if (!PyNumber_Check(lhs))
	  return NULL;
	if (! PL_get_arg(2, t, targ) )
	  return NULL;
	rhs = term_to_python(targ);
	if (!PyNumber_Check(rhs))
	  return NULL;
	return PyNumber_Divide(lhs, rhs);
      } else if (fun == FUNCTOR_hat2) {
	term_t targ = PL_new_term_ref();
	PyObject *lhs, *rhs;

	if (! PL_get_arg(1, t, targ) )
	  return NULL;
	lhs = term_to_python(targ);
	if (! PL_get_arg(2, t, targ) )
	  return NULL;
	rhs = term_to_python(targ);
	return PyObject_GetItem(lhs, rhs);
      }
    }
    return NULL;
  }
  return NULL;
}

static foreign_t
python_to_term(PyObject *pVal, term_t t)
{
  if (PyLong_Check(pVal)) {
    return PL_unify_int64(t, PyLong_AsLong(pVal));
  } else if (PyInt_Check(pVal)) {
    return PL_unify_int64(t, PyInt_AsLong(pVal));
  } else if (PyBool_Check(pVal)) {
    if (PyObject_IsTrue(pVal)) {
      return PL_unify_atom(t, ATOM_true);
    } else {
      return PL_unify_atom(t, ATOM_false);
    }
  } else if (PyFloat_Check(pVal)) {
    return PL_unify_float(t, PyFloat_AsDouble(pVal));
  } else if (PyByteArray_Check(pVal)) {
    atom_t tmp_atom = PL_new_atom(PyByteArray_AsString(pVal));
    return PL_unify_atom(t, tmp_atom);
  } else if (PyString_Check(pVal)) {
    atom_t tmp_atom = PL_new_atom(PyString_AsString(pVal));
    return PL_unify_atom(t, tmp_atom);
  } else if (PyList_Check(pVal)) {
    term_t to = PL_new_term_ref();
    Py_ssize_t i, sz = PyList_GET_SIZE(pVal);

    for (i = 0; i < sz; i++) {
      if (!PL_unify_list(t, to, t) ||
	  !python_to_term(PyList_GetItem(pVal, i), to))
	return FALSE;
    }
    return PL_unify_nil(t);
  } else {
    term_t to = PL_new_term_ref(), t1 = PL_new_term_ref();
    PL_put_pointer(t1, (void *)pVal);
    PL_cons_functor(to, FUNCTOR_pointer1, t1);
    return PL_unify(t, to);
  }
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
  pModule = PyImport_Import(pName);
  // PyErr_Print();
  Py_DECREF(pName);
  if (pModule == NULL) {
    return FALSE;
  }
  return python_to_term(pModule, mod);
}

static foreign_t
python_f(term_t tmod, term_t fname, term_t tf)
{ 
  char *s;
  size_t len;
  PyObject *pF, *pModule;

  /* if an atom, fetch again */
  if ( PL_is_atom(tmod) ) {
    PyObject *pName;

    if ( !PL_get_nchars(fname, &len, &s, CVT_ALL|CVT_EXCEPTION) ) {  
      return FALSE;
    }
    pName = PyString_FromString(s);
    if (pName == NULL) {
      return FALSE;
    }
    pModule = PyImport_Import(pName);
  } else if (!(pModule = term_to_python(tmod)))
    return FALSE;
  if ( !PL_get_nchars(fname, &len, &s, CVT_ALL|CVT_EXCEPTION) ) {  
    return FALSE;
  }
  pF = PyObject_GetAttrString(pModule, s);
  if (pF == NULL || ! PyCallable_Check(pF)) {
    return FALSE;
  }
  return python_to_term(pF, tf);
}

static foreign_t
python_o(term_t tmod, term_t fname, term_t tf)
{ 
  char *s;
  size_t len;
  PyObject *pO, *pModule;

  pModule = term_to_python(tmod);
  if ( !PL_get_nchars(fname, &len, &s, CVT_ALL|CVT_EXCEPTION) ) {  
    return FALSE;
  }
  pO = PyObject_GetAttrString(pModule, s);
  if (pO == NULL) {
    return FALSE;
  }
  return python_to_term(pO, tf);
}

static foreign_t
python_set_item(term_t tobj, term_t tpos, term_t titem)
{ 
  PyObject *obj, *item, *pos;

  obj = term_to_python(tobj);
  if (obj == NULL)
    return FALSE;
  item = term_to_python(titem);
  if (item == NULL)
    return FALSE;
  pos = term_to_python(tpos);
  if (pos == NULL)
    return FALSE;
  if (PyObject_SetItem(obj, pos,  item) < 0)
    return FALSE;
  return TRUE;
}

static foreign_t
python_len(term_t tobj, term_t tf)
{ 
  Py_ssize_t len;
  PyObject *o;

  o = term_to_python(tobj);
  if (o == NULL)
    return FALSE;
  len = PyObject_Length(o);
  return PL_unify_int64(tf, len);
}

static foreign_t
python_dir(term_t tobj, term_t tf)
{ 
  PyObject *dir;
  PyObject *o;

  o = term_to_python(tobj);
  if (o == NULL)
    return FALSE;
  dir = PyObject_Dir(o);
  return python_to_term(dir, tf);
}

static foreign_t
python_is(term_t tobj, term_t tf)
{ 
  PyObject *o;

  o = term_to_python(tobj);
  if (!o)
    return FALSE;
  return python_to_term(o, tf);
}

static foreign_t
python_apply(term_t tin, term_t targs, term_t tf)
{ 
  PyObject *pF, *pArgs, *pValue;
  int i, arity;
  atom_t aname;
  foreign_t out;
  term_t targ = PL_new_term_ref();

  pF = term_to_python(tin);
  if ( pF == NULL ) {
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
  if (pValue == NULL)
    return FALSE;
  out =  python_to_term(pValue, tf);
  Py_DECREF(pValue);
  return out;
}

static foreign_t
python_assign(term_t name, term_t exp)
{
  PyObject *e = term_to_python(exp);
  char *s;

  if (e == NULL)
    return FALSE;
  if (!PL_get_atom_chars(name, &s))
    return FALSE;
  return PyObject_SetAttrString(py_Main, s, e) >= 0;  
}

static foreign_t
python_access(term_t obj, term_t f, term_t out)
{
  PyObject *o = term_to_python(obj), *pValue, *pArgs, *pF;
  atom_t name;
  char *s;
  int i, arity;
  term_t targ = PL_new_term_ref();

  if (o == NULL)
    return FALSE;
  if ( PL_is_atom(f) ) {
    if (!PL_get_atom_chars(f, &s))
      return FALSE;
    if ((pValue = PyObject_GetAttrString(o, s)) == NULL)
      return FALSE;  
    return python_to_term(pValue, out);
  }
  if (! PL_get_name_arity( f, &name, &arity) ) {
    return FALSE;
  }
  s = PL_atom_chars(name);
  if ((pF = PyObject_GetAttrString(o, s)) < 0)
    return FALSE;  
  pArgs = PyTuple_New(arity);
  for (i = 0 ; i < arity; i++) {
    PyObject *pArg;
    if (! PL_get_arg(i+1, f, targ) )
      return FALSE;
    pArg = term_to_python(targ);
    if (pArg == NULL)
      return FALSE;
    /* pArg reference stolen here: */
    PyTuple_SetItem(pArgs, i,  pArg);
  }
  pValue = PyObject_CallObject(pF, pArgs);
  if (pValue == NULL) {
    Py_DECREF(pArgs);
    Py_DECREF(pF);
    return FALSE;
  }
  Py_DECREF(pArgs);
  Py_DECREF(pF);
  return python_to_term(o, out);
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

static foreign_t
init_python(void)
{ 
  Py_Initialize();
  py_Main =  PyImport_AddModule("__main__");

  return TRUE;
}

static foreign_t
end_python(void)
{ 
  Py_Finalize();

  return TRUE;
}

install_t install_python(void);

install_t
install_python(void)
{ // FUNCTOR_dot2 = PL_new_functor(PL_new_atom("."), 2);
  // FUNCTOR_equal2 = PL_new_functor(PL_new_atom("="), 2);
  // FUNCTOR_boolop1 = PL_new_functor(PL_new_atom("@"), 1);
  ATOM_true  = PL_new_atom("true");
  ATOM_false = PL_new_atom("false");
  FUNCTOR_dollar1 = PL_new_functor(PL_new_atom("$"), 1);
  FUNCTOR_pointer1 = PL_new_functor(PL_new_atom("__obj__"), 1);
  FUNCTOR_add2 = PL_new_functor(PL_new_atom("+"), 2);
  FUNCTOR_sub2 = PL_new_functor(PL_new_atom("-"), 2);
  FUNCTOR_mul2 = PL_new_functor(PL_new_atom("*"), 2);
  FUNCTOR_div2 = PL_new_functor(PL_new_atom("/"), 2);
  FUNCTOR_hat2 = PL_new_functor(PL_new_atom("^"), 2);

  PL_register_foreign("init_python",	  0, init_python,      0);
  PL_register_foreign("end_python",	  0, end_python,       0);
  PL_register_foreign("python_import",	  2, python_import,       0);
  PL_register_foreign("python_f",	  3, python_f,       0);
  PL_register_foreign("python_o",	  3, python_o,       0);
  PL_register_foreign("python_len",	  2, python_len,       0);
  PL_register_foreign("python_is",	  2, python_is,       0);
  PL_register_foreign("python_dir",	  2, python_dir,       0);
  PL_register_foreign("python_apply",	  3, python_apply,       0);
  PL_register_foreign("python_access",	  3, python_access,       0);
  PL_register_foreign("python_assign",	  2, python_assign,       0);
  PL_register_foreign("python_set_item",  3, python_set_item,       0);
  PL_register_foreign("python_run_command",	  1, python_run_command,       0);
}

