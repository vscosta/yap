
#include "py4yap.h"

#include <frameobject.h>

void YAPPy_ThrowError__(const char *file, const char *function, int lineno,
                        yap_error_number type, term_t where, ...) {
  va_list ap;
  char tmpbuf[MAXPATHLEN];
  YAP_Term wheret = YAP_GetFromSlot(where);
  PyObject *ptype;

  if ((ptype = PyErr_Occurred()) == NULL) {
    PyErr_Print();
  }

  va_start(ap, where);
  char *format = va_arg(ap, char *);
  if (format != NULL) {
#if HAVE_VSNPRINTF
    (void)vsnprintf(tmpbuf, MAXPATHLEN - 1, format, ap);
#else
    (void)vsprintf(tnpbuf, format, ap);
#endif
    // fprintf(stderr, "warning: ");
    Yap_ThrowError__(file, function, lineno, type, wheret, tmpbuf);
  } else {
    //    Yap_ThrowError__(file, function, lineno, type, wheret);
  }
}

static Term repr_term(PyObject *pVal) {
  CACHE_REGS
  Term t = MkAddressTerm(pVal);
  return Yap_MkApplTerm(FunctorPythonObject, 1, &t);
}

static Term python_to_term__(PyObject *pVal) {
  CACHE_REGS
  if (pVal == Py_None) {
    // fputs("<<*** ",stderr);Yap_DebugPlWrite(YAP_GetFromSlot(t));   fputs("
    // >>***\n",stderr);
    //return YAP_MkVarTerm();
    // fputs("<<*** ",stderr);Yap_DebugPlWrite(YAP_GetFromSlot(t));   fputs("
    return MkAtomTerm(Yap_LookupAtom("none"));
    // >>***\n",stderr);
  } else if (PyBool_Check(pVal)) {
    if(PyObject_IsTrue(pVal)) return TermTrue;
    return TermFalse;
  } else if (PyLong_Check(pVal)) {
    return MkIntegerTerm(PyLong_AsLong(pVal));
#if PY_MAJOR_VERSION < 3
  } else if (PyInt_Check(pVal)) {
    return MkIntegerTerm(PyInt_AsLong(pVal));
#endif
  } else if (PyFloat_Check(pVal)) {
    return MkFloatTerm(PyFloat_AsDouble(pVal));
  } else if (PyComplex_Check(pVal)) {
    Term t[2];
    t[0] = MkFloatTerm(PyComplex_RealAsDouble(pVal));
    t[1] = MkFloatTerm(PyComplex_ImagAsDouble(pVal));
    return Yap_MkApplTerm(FunctorI, 2, t);

  }
  else if (PyUnicode_Check(pVal)) {
#if PY_MAJOR_VERSION < 3
    size_t sz = PyUnicode_GetSize(pVal) + 1;
    wchar_t *s = malloc(sizeof(wchar_t) * sz);
    sz = PyUnicode_AsWideChar((PyUnicodeObject *)pVal, a, sz - 1);
    free(ptr);
#else
    const char *s = PyUnicode_AsUTF8(pVal);
#endif
    if (s[0]=='\0') {
      return MkAtomTerm(Yap_LookupAtom(""));
    }
    PyObject *o;
    if ((o = PythonLookup(s, NULL))!=NULL && o  != Py_None && o != pVal)
        return pythonToYAP(o);
#if 0
    if (Yap_AtomInUse(s))
      rc = rc && PL_unify_atom_chars(t, s);
    else
#endif
      if  (pyStringToString)
        return MkStringTerm(s);
      else
	return MkAtomTerm(Yap_LookupAtom(s));
  }
  else if (PyByteArray_Check(pVal)) {
    return MkStringTerm(PyByteArray_AsString(pVal));
#if PY_MAJOR_VERSION < 3
  }
  else if (PyString_Check(pVal)) {
    return MkStringTerm(PyString_AsString(pVal));
#endif
  }
  else if (PyTuple_Check(pVal)) {
    Py_ssize_t sz = PyTuple_Size(pVal);
    const char *s;
    s = Py_TYPE(pVal)->tp_name;
    if (s == NULL)
      s = "t";
    if (sz == 0) {
      return MkAtomTerm(YAP_LookupAtom(Py_TYPE(pVal)->tp_name));
    }
    else {
      Functor f = Yap_MkFunctor(Yap_LookupAtom(s), sz);
      Term t = Yap_MkNewApplTerm(f, sz);
      long i;
      CELL *ptr = RepAppl(t) + 1;
      for (i = 0; i < sz; i++) {
        PyObject *p = PyTuple_GetItem(pVal, i);
        if (p == NULL) {
          PyErr_Clear();
          return false;
        }
        *ptr++ = python_to_term__(p);
      }
      return t;
    }
    // PL_reset_term_refs(to);
    // fputs(" ||*** ",stderr); Yap_DebugPlWrite(YAP_GetFromSlot(t)); fputs("
    // ||***\n",stderr);
  }
  else if (PyList_Check(pVal)) {
    Py_ssize_t i, sz = PyList_GET_SIZE(pVal);
    if (sz == 0)
      return repr_term(pVal);
    Term t = TermNil;
    for (i = sz; i > 0; ) {
      -- i;
      PyObject *p = PyList_GetItem(pVal, i);
      if (p == NULL) {
        PyErr_Clear();
        return false;
      }
                            Term th;
      if (!(th=python_to_term__(p))
	  )       return false;

      t = MkPairTerm(th, t);
    }
    return t;
  }
  else if (PyDict_Check(pVal)) {
    Py_ssize_t pos = 0, tot = PyDict_Size(pVal);
    PyObject *key, *value;
    Term f, *opt = &f, t, to;
    if (tot == 0)
      return MkAtomTerm( Yap_LookupAtom("{}"));
    while (PyDict_Next(pVal, &pos, &key, &value)) {
      Term t0[2];
      if (PyUnicode_Check(key)) {
	t0[0] = MkAtomTerm(Yap_LookupAtom(PyUnicode_AsUTF8(key)));
      } else {
	t0[0] =python_to_term__(key);
      }
      t0[1] =  python_to_term__(value);
      to = Yap_MkApplTerm(FunctorModule, 2, t0);
      if (pos < tot) {
        t = Yap_MkNewApplTerm(FunctorComma, 2);
        CELL *pt = RepAppl(t) + 1;
        pt[0] = to;
        *opt = t;
	opt = pt+1;
      } else {
        if (pos == 0) {
          return repr_term(pVal);
        }

        *opt = to;
	break;
      }
    }
    return Yap_MkApplTerm(FunctorBraces, 1, &f);
  }
  return repr_term(pVal);

}

foreign_t python_to_term(PyObject *pVal, term_t t) {
  Term o = python_to_term__(pVal);
  return YAP_Unify(o,YAP_GetFromSlot(t));
}

// extern bool Yap_do_low_level_trace;

X_API YAP_Term pythonToYAP(PyObject *pVal) {
  CACHE_REGS
  // Yap_do_low_level_trace=1;
  /* fputs(" ***    ", stderr); */

  /* PyObject_Print(pVal, stderr, 0); */
  /* fputs("***>>\n", stderr); */
  if (pVal == NULL)
    Yap_ThrowError(SYSTEM_ERROR_INTERNAL, 0, NULL);
  yhandle_t h0 = Yap_CurrentHandle();
  Term t =  python_to_term__(pVal);
  /* fputs("<< ***    ", stderr); */
  /* Yap_DebugPlWrite(t); */
  /* fputs(" ***\n", stderr); */
  // Py_DECREF(pVal);
  Yap_CloseHandles(h0);
  return t;
}

      
/** tries to assign an element of an array/embedded lists */
bool set_item(YAP_Term indx, PyObject *ctx, PyObject *o, bool eval, bool cvt)                              
{
  const char *s;
  if (IsAtomTerm(indx)) {
    s=AtomTermName(indx);
  } else if (IsStringTerm(indx)) {
    s=StringOfTerm(indx);
  } else {
    s = NULL;
  }
  // check numeric
  if (PyDict_Check(ctx)) {
    if (s) {
      return 	PyDict_SetItemString(ctx,s,o) ==   0;
    }
    PyObject *yy=  yap_to_python(indx, eval, ctx, cvt);
    return 	PyDict_SetItem(ctx,yy,o) ==   0;
    
  } else
    if (PySequence_Check(ctx)) {
      // moved = true;
      if (o) {
	if (PySequence_SetItem(ctx, IntegerOfTerm(indx),o) == 0)
	  return true;

      }

    }
  return 0;
}      

foreign_t assign_to_symbol(term_t t, PyObject *e) {
  char *s = NULL;
  if (!PL_get_atom_chars(t, &s)) {
    return false;
  }
  PyObject *dic;
  if (!lookupPySymbol(s, 0, NULL, &dic))
    dic = py_Main;
  Py_INCREF(e);
  return PyObject_SetAttrString(dic, s, e) == 0;
}


/** tries to assign an element of an array/embedded lists */
static bool assign_symbol(const char *s, PyObject *ctx, PyObject *o)
{
  if (ctx && ctx !=Py_None && PyObject_HasAttrString(ctx, s)) {
    if (PyObject_SetAttrString(ctx, s, o)==0)
      return o;
  }
  if (ctx && PyDict_Check(ctx)) {
    if (PyDict_SetItemString(ctx, s, o) == 0)
      return o;
                  PyErr_SetString(PyExc_TypeError,
    		 "obj.s does not exist, 1assignment failed");
    return NULL;
  }
  PyObject *py_Local = PyEval_GetLocals();
  if (py_Local && py_Local !=Py_None && PyObject_HasAttrString(py_Local, s)) {
    if (PyObject_SetAttrString(py_Local, s, o)==0)
      return o;
  }
  PyObject *py_Global = PyEval_GetGlobals();
  if (py_Global && py_Global != Py_None && PyObject_HasAttrString(py_Global, s)) {
    if (PyObject_SetAttrString(py_Global, s, o)==0)
      return o;
  }
  if (py_Main && py_Main != Py_None ) {
    if (PyObject_SetAttrString(py_Main, s, o)==0)
      return o;
  }
  return NULL ;
}
/**
 * This is the core to the Python interface implementing
 * f = ctx(t) / <- exp
 * @param ctx
 * @param exp
 * @param t
 * @param eval
 * @return
 */

bool
assign_obj(PyObject* ctx, PyObject *val, YAP_Term yt, bool eval) {
  YAP_Term hd;
  py_Context = ctx;
  // Yap_DebugPlWriteln(yt);
  if (yt == 0)
    return false;
  // a.b = [a|b]
  if (IsPairTerm(yt)) 
    {
      Term t0 = yt;
      Term *tail;
      ssize_t len;
      if ((len = Yap_SkipList(&t0, &tail)) > 0 && *tail == TermNil) {
	if (!PyList_Check(val) || PyList_GET_SIZE(val)!=len)
	  return false;
	int i = 0;
  while (IsPairTerm(yt)) { // f(X).g(X)
    hd = HeadOfTerm(yt);
    PyObject *p = PyList_GET_ITEM(val,i);
    python_assign(hd, p, ctx);
    yt = TailOfTerm(yt);
    i++;
  }
  return true;
  }
  if (IsApplTerm(yt) && FunctorOfTerm(yt) == FunctorSqbrackets) {
    PyObject *o = yap_to_python(HeadOfTerm(ArgOfTerm(1,yt)), eval, ctx, false);
    Term key = HeadOfTerm(ArgOfTerm(2,yt));
    return set_item( key,  o, val, eval, false);
  }
  if (IsAtomTerm(yt)) {
    const char *s;
    s=AtomTermName(yt);
    return assign_symbol(s,ctx,val);
  }
 }
  ssize_t len, i;
if (IsApplTerm(yt) && PyTuple_Check(val) &&
      ArityOfFunctor(FunctorOfTerm(yt)) ==
    (len = PyTuple_GET_SIZE(val))) {
  for (i=0; i<len;i++) {
    PyObject *p = PyTuple_GET_ITEM(val,i);
    python_assign(ArgOfTerm(i+1,yt), p, ctx);
    }
  return true;
 }
 if (IsAtomTerm(yt)) {
    const char *s;
    s=AtomTermName(yt);
    return assign_symbol(s,ctx,val);
  }
  return false;
}


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
 *python find_assign.
 */
bool python_assign(YAP_Term t, PyObject *exp, PyObject *context) {
  PyErr_Print();
  // Yap_DebugPlWriteln(yt);
  if (IsVarTerm(t)) {
    // if (context == NULL) // prevent a.V= N*N[N-1]
    return Yap_unify(t,pythonToYAP(exp));
  }

 return assign_obj(context,exp, t, true);

}
