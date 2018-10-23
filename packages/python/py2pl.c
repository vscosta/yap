

#include "Yap.h"

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
    Yap_ThrowError__(file, function, lineno, type, wheret);
  }
}

static Term repr_term(PyObject *pVal) {
  Term t = MkAddressTerm(pVal);
  return Yap_MkApplTerm(FunctorObj, 1, &t);
}

foreign_t assign_to_symbol(term_t t, PyObject *e);

foreign_t assign_to_symbol(term_t t, PyObject *e) {
  char *s = NULL;
  if (!PL_get_atom_chars(t, &s)) {
    return false;
  }
  PyObject *dic;
  if (!lookupPySymbol(s, NULL, &dic))
    dic = py_Main;
  Py_INCREF(e);
  return PyObject_SetAttrString(dic, s, e) == 0;
}

static Term python_to_term__(PyObject *pVal) {
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
#if 0
    if (false && Yap_AtomInUse(s))
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
    if (!python_to_term__(p))
      return false;

    t = MkPairTerm(python_to_term__(p), t);
  }
  return t;
}
else if (PyDict_Check(pVal)) {
  Py_ssize_t pos = 0, tot = PyDict_Size(pVal);
  PyObject *key, *value;
  Term f, *opt = &f, t, to;
  while (PyDict_Next(pVal, &pos, &key, &value)) {
      Term t0[2];
      t0[0] = python_to_term__(key);
      t0[1] = python_to_term__(value);
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
  // Yap_do_low_level_trace=1;
  /* fputs(" ***    ", stderr); */
  /* PyObject_Print(pVal, stderr, 0); */
  /* fputs("***>>\n", stderr); */
  if (pVal == NULL)
    Yap_ThrowError(SYSTEM_ERROR_INTERNAL, 0, NULL);
  Term t =  python_to_term__(pVal);
  /* fputs("<< ***    ", stderr); */
  /* Yap_DebugPlWrite(t); */
  /* fputs(" ***\n", stderr); */
  // Py_DECREF(pVal);
  return t;
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
    PyErr_Print();
  context = find_obj(context, t, false);
  // Yap_DebugPlWriteln(yt);
  switch (PL_term_type(t)) {
  case PL_VARIABLE: {
    if (context == NULL) // prevent a.V= N*N[N-1]
      return python_to_term(exp, t);
  }

  case PL_STRING: {             
    char *s = NULL;
    size_t l;
    PL_get_string_chars(t, &s,&l);
    if (!context)
      context = py_Main;
    if (PyObject_SetAttrString(context, s, exp) == 0)
      return true;
    PyErr_Print();
    return false;
  }
  case PL_ATOM: {
    char *s = NULL;
    PL_get_atom_chars(t, &s);
    if (!context)
      context = py_Main;
    if (PyObject_SetAttrString(context, s, exp) == 0)
      return true;
    return false;
  }
  case PL_INTEGER:
  case PL_FLOAT:
    // domain or type erro?
    return false;
  default: {
    term_t tail = PL_new_term_ref(), arg = PL_new_term_ref();
    size_t len, i;

    if (PL_is_pair(t)) {
      if (PL_skip_list(t, tail, &len) &&
          PL_get_nil(tail)) { //             true list

        if (PySequence_Check(exp) && PySequence_Length(exp) == len)

          for (i = 0; i < len; i++) {
            PyObject *p;
            if (!PL_get_list(t, arg, t)) {
              PL_reset_term_refs(tail);
              p = Py_None;
            }
            if ((p = PySequence_GetItem(exp, i)) == NULL)
              p = Py_None;
            if (!python_assign(arg, p, context)) {
              PL_reset_term_refs(tail);
            }
          }
      }
    } else {
      functor_t fun;

      if (!PL_get_functor(t, &fun)) {
        PL_reset_term_refs(tail);
        return false;
      }

      if (fun == FUNCTOR_sqbrackets2) {
        //  tail is the object o
        if (!PL_get_arg(2, t, tail)) {
          PL_reset_term_refs(tail);
          return false;
        }
        PyObject *o = term_to_python(tail, true, context, false);
        // t now refers to the index
        if (!PL_get_arg(1, t, t) || !PL_get_list(t, t, tail) ||
            !PL_get_nil(tail)) {
          PL_reset_term_refs(tail);
          return false;
        }
        PyObject *i = term_to_python(t, true, NULL, false);
        // check numeric
        if (PySequence_Check(o) && PyLong_Check(i)) {
          long int j;
          j = PyLong_AsLong(i);
          return PySequence_SetItem(o, j, exp) == 0;
        }
#if PY_MAJOR_VERSION < 3
        if (PySequence_Check(o) && PyInt_Check(i)) {
          long int j;
          j = PyInt_AsLong(i);
          return PySequence_SetItem(o, i, exp) == 0;
        }
#endif
        if (PyDict_Check(o)) {
          if (PyDict_SetItem(o, i, exp) == 0) {
            return true;
          }
        }
        if (PyObject_SetAttr(o, i, exp) == 0) {
          return true;
        }
      } else {
        atom_t s;
        int n, i;
        PL_get_name_arity(t, &s, &n);
        PyObject *o = term_to_python(t, true, context, true);
        if (PySequence_Check(o) && PySequence_Length(o) == n) {
          for (i = 1; i <= n; i++) {
            PyObject *p;
            if (!PL_get_arg(i, t, arg)) {
              PL_reset_term_refs(tail);
              o = false;
              p = Py_None;
            }
            if ((p = PySequence_GetItem(exp, i - 1)) == NULL)
              p = Py_None;
            else if (!python_assign(arg, p, NULL)) {
              PL_reset_term_refs(tail);
              o = NULL;
            }
          }
          return true;
        }
      }
    }
    PL_reset_term_refs(tail);
  }
    return NULL;
  }
}
