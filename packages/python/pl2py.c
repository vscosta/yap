

#include "Yap.h"

#include "py4yap.h"

extern PyObject *py_Local, *py_Global;

PyObject *YE(term_t t, int line, const char *file, const char *code) {
  char buf[1024];
  YAP_WriteBuffer(YAP_GetFromSlot(t), buf, 1023, 0);
  fprintf(stderr, "**** Warning,%s@%s:%d: failed on expression %s\n", code,
          file, line, buf);

  return NULL;
}



PyObject *YEC(PyObject *f, PyObject *a, PyObject *d, int line, const char *file, const char *code) {
  
  fprintf(stderr, "**** Warning,%s@%s:%d: failed on Python call \n", code,
          file, line);
  if (f)
    PyObject_Print(f, stderr, 0);
  else
    fprintf(stderr,"<null>");
  if (a)
    PyObject_Print(a, stderr, 0);
  if (d)
    PyObject_Print(d, stderr, 0);
      fprintf(stderr,"\n");
  return NULL;
}
 
PyObject *YED2(PyObject *f, PyObject *a, PyObject *d, int line, const char *file, const char *code) {
  
  fprintf(stderr, "**** Warning,%s@%s:%d: failed on Python call \n", code,
          file, line);
  if (f)
    PyObject_Print(f, stderr, 0);
  else
    fprintf(stderr,"<null>");
  fprintf(stderr,"(");
  if (a)
    PyObject_Print(a, stderr, 0);
  fprintf(stderr,",");
  if (d)
    PyObject_Print(d, stderr, 0);
  fprintf(stderr,")\n");
  return NULL;

}

PyObject *YED1(PyObject *f, PyObject *a, int line, const char *file, const char *code) {
  
  fprintf(stderr, "**** Warning,%s@%s:%d: failed on Python call \n", code,
          file, line);
  if (f)
    PyObject_Print(f, stderr, 0);
  else
    fprintf(stderr,"<null>");
  fprintf(stderr,"(");
  if (a)
    PyObject_Print(a, stderr, 0);
  fprintf(stderr,")\n");
  return NULL;

}

void YEM(const char *exp, int line, const char *file, const char *code) {
  fprintf(stderr, "**** Warning,%s@%s:%d: failed while executing %s\n", code,
          file, line, exp);
}

static PyObject *s_to_python(const char *s, bool eval, PyObject *p0) {
  PyObject *o;
  if (eval) {
    o = PythonLookup(s, p0);
    /*     if (!o)
           return o;
    */
  } else {
    o = PythonLookupSpecial(s);
  }
  if (o) {
    Py_INCREF(o);
    return CHECKNULL(YAP_MkStringTerm(s), o);
  } else {
    // char *ns = Py_Malloc(strlen(s)+1);
    /// strcpy(ns,s);
    PyObject *pobj = PyUnicode_FromString(s);
    Py_INCREF(pobj);
    return pobj;
  }
}

/**
 * obtain  the object matching a certain string.
 *
 * @param t handle to Prolog term
 * @param t whether should  try to evaluate evaluables.
 *
 * @return a Python object descriptor or NULL if failed
 */
X_API PyObject *string_to_python(const char *s, bool eval, PyObject *p0) {

  char *buf = malloc(strlen(s) + 1), *child;
  while ((child = strchr(s, '.')) != NULL) {
    size_t len = child - s;
    strncpy(buf, s, len);
    buf[len] = '\0';
    p0 = s_to_python(buf, eval, p0);
    s = child + 1;
  }
  return s_to_python(s, eval, p0);
}

static bool entry_to_dictionary(PyObject *dict, Term targ,
                               bool eval, bool cvt) {
  PyObject *lhs = NULL, *rhs;
  Term t1, t2;
  const char *s;
  t1 = ArgOfTerm(1,targ);
  if (YAP_IsAtomTerm(t1)) {
    s = YAP_AtomName(YAP_AtomOfTerm(t1));
  } else if (IsStringTerm(t1)) {
    s = YAP_StringOfTerm(t1);
  } else if (IsIntegerTerm(t1)) {
    lhs = PyLong_FromLong(IntegerOfTerm(t1));
  } else {
    return false;
  }
  
  if (lhs == NULL) {
   lhs = PyUnicode_FromString(s);
  }

  t2 = ArgOfTerm(2,targ);
  rhs = term_to_python(Yap_InitSlot(t2), eval, NULL, cvt);
  if (rhs == NULL) {
    PyErr_Print();
    return false;
  }

  return PyDict_SetItem(dict, lhs, rhs) == 0;
}

/**
 * term_to_python translates and evaluates from Prolog to Python
 *
 * @param t handle to Prolog term
 * @param t whether should  try to evaluate evaluables.
 *
 * @return a Python object descriptor or NULL if failed
 */
PyObject *term_to_python(term_t t, bool eval, PyObject *o, bool cvt) {
  //
  switch (PL_term_type(t)) {
  case PL_VARIABLE: {
    if (t == 0) {
      Yap_ThrowError(SYSTEM_ERROR_INTERNAL, t, "in term_to_python");
    }
    PyObject *out = PyTuple_New(1);
    PyTuple_SET_ITEM(out, 0, PyLong_FromLong((YAP_Int)YAP_GetFromSlot(t)));
    if (!cvt)
      return out;
    return term_to_nametuple("v", 1, out);
  };
  case PL_ATOM: {
  YAP_Term yt = YAP_GetFromSlot(t);
    YAP_Atom at = YAP_AtomOfTerm(yt);
    const char *s;

    s = YAP_AtomName(at);
      o = PythonLookup(s, o);
       while (o && PyUnicode_Check(o)) {
          const char *s = PyUnicode_AsUTF8(o);
          PyObject *ne = PythonLookup(s,NULL);
          if (ne && ne != Py_None )
              o = ne;
          else
              break;
      }
      if (!o) {
          o = PyUnicode_FromString(s);
      }

      if (o) {
      // PyDict_SetItemString(py_Atoms, s, Py_None);
      Py_INCREF(o);
      return o;
    }
      return Py_None;
  }
  case PL_STRING: {
  YAP_Term yt = YAP_GetFromSlot(t);
    const char *s = NULL;
    if (YAP_IsAtomTerm(yt)) {
      s = YAP_AtomName(YAP_AtomOfTerm(yt));
    } else if (YAP_IsStringTerm(yt)) {
      s = YAP_StringOfTerm(yt);
    } else {
      return CHECKNULL(t, NULL);
    }
    PyObject *pobj = PyUnicode_FromString(s);

#if PY_MAJOR_VERSION < 3
    if (proper_ascii_string(s)) {
      PyObject *o = PyString_FromStringAndSize(s, strlen(s));
      return CHECKNULL(t, o);
    }
#endif
    //      char *p = malloc(strlen(s)+1);
    // strcpy(p, s);
    Py_IncRef(pobj);
    return CHECKNULL(t, pobj);
  } break;
  case PL_INTEGER: {
    int64_t j;
    if (!PL_get_int64_ex(t, &j))
      return CHECKNULL(t, NULL);
#if PY_MAJOR_VERSION < 3
    PyObject *o = PyInt_FromLong(j);
    return CHECKNULL(t, o);
#else
    PyObject *o = PyLong_FromLong(j);
    return CHECKNULL(t, o);
#endif
  }

  case PL_FLOAT: {
    PyObject *out;
    double fl;
    if (!PL_get_float(t, &fl))
      return CHECKNULL(t, NULL);
    out = PyFloat_FromDouble(fl);
    return CHECKNULL(t, out);
  }
  default:
    if (PL_is_pair(t)) {
      Term t0 = Yap_GetFromHandle(t);
      Term *tail;
      size_t len, i;
      if ((len = Yap_SkipList(&t0, &tail)) > 0 && *tail == TermNil) {
        PyObject *out, *a;

        out = PyList_New(len);

        for (i = 0; i < len; i++) {
          Term ai = HeadOfTerm(t0);
          a = yap_to_python(ai, eval, o, cvt);
          if (a) {
            if (PyList_SetItem(out, i, a) < 0) {
              YAPPy_ThrowError(SYSTEM_ERROR_INTERNAL, t, "list->python");
            }
          }
          t0 = TailOfTerm(t0);
        }
        return out;
      } else {
        PyObject *no = find_term_obj(o, &t0, false);
        return yap_to_python(t0, eval, no, cvt);
      }
    } else {
      {
        term_t tail = PL_new_term_ref();
        functor_t fun;
     
        if (!PL_get_functor(t, &fun)) {
          PL_reset_term_refs(tail);
          YAPPy_ThrowError(SYSTEM_ERROR_INTERNAL, t, "list->python");
        }
        if (fun == FUNCTOR_pointer1) {
          void *ptr;

          AOK(PL_get_arg(1, t, t), NULL);
          AOK(PL_get_pointer(t, &ptr), NULL)
          /* return __main__,s */
          return (PyObject *)ptr;
        }
        if (fun == FUNCTOR_sqbrackets2) {
          term_t targ = PL_new_term_ref(), trhs = PL_new_term_ref();
          PyObject *v;
          Py_ssize_t min, max;
          AOK(PL_get_arg(2, t, targ), NULL);
          v = term_to_python(targ, eval, o, false);

          AOK(PL_get_arg(1, t, targ), NULL);
          AOK(PL_get_list(targ, trhs, targ), NULL);
          if (PL_is_functor(trhs, FUNCTOR_colon2)) {
            if (!PySequence_Check(v))
              return NULL;
            min = get_p_int(term_to_python(targ, true, NULL, false), 0);
            AOK(PL_get_arg(1, trhs, targ), NULL);
            if (PL_is_functor(targ, FUNCTOR_colon2)) {
              return NULL;
            }
            max = get_p_int(term_to_python(targ, true, o, false),
                            PyObject_Size(v));
            return PySequence_GetSlice(v, min, max);
          } else {
            PyObject *ip = term_to_python(trhs, eval, o, cvt);
            if (PySequence_Check(v)) {
#if PY_MAJOR_VERSION < 3
              if (PyLong_Check(ip)) {
                min = PyLong_AsLong(ip);
              } else if (PyInt_Check(ip)) {
                min = PyInt_asInt(ip);
              }
#else
              if (PyLong_Check(ip)) {
                PyObject *o = PySequence_GetItem(v, PyLong_AsLong(ip));
                if (o == NULL)
                  o = Py_None;
                if (CHECKNULL(t, o) == NULL)
                  return NULL;
                Py_INCREF(o);
                return o;
              }
#endif
            } else {
              o = PyObject_GetItem(v, ip);
              if (o == NULL)
                o = Py_None;
              Py_INCREF(o);
              return o;
            }
          }
        }
        if (fun == FUNCTOR_dollar1) {
          char *s = NULL;
          term_t targ = PL_new_term_ref();
          AOK(PL_get_arg(1, t, targ), NULL);
          AOK(PL_get_atom_chars(targ, &s), NULL);
          /* return __main__,s */
          PyObject *o = PyObject_GetAttrString(py_Main, s);
          return o;
        }
        if (fun == FUNCTOR_brackets1) {
          AOK(PL_get_arg(1, t, t), NULL);
          PyObject *ys = term_to_python(t, true, o, true), *rc;
	  CHECK_CALL(ys, PyTuple_New(0), NULL);
	  return rc;
        }
        if (fun == FUNCTOR_complex2) {
          term_t targ = PL_new_term_ref();
          PyObject *lhs, *rhs;
          double d1, d2;

          AOK(PL_get_arg(1, t, targ), NULL);
          lhs = term_to_python(targ, eval, NULL, cvt);
          AOK(PyNumber_Check(lhs), NULL);
          if (PyFloat_Check(lhs)) {
            d1 = PyFloat_AsDouble(lhs);
          } else if (PyLong_Check(lhs)) {
            d1 = PyLong_AsLong(lhs);
#if PY_MAJOR_VERSION < 3
          } else if (PyInt_Check(lhs)) {
            d1 = PyInt_AsLong(lhs);
#endif
          } else {
            return NULL;
          }
          AOK(PL_get_arg(2, t, targ), NULL);
          rhs = term_to_python(targ, eval, NULL, cvt);
          AOK(PyNumber_Check(rhs), NULL);
          if (PyFloat_Check(rhs)) {
            d2 = PyFloat_AsDouble(rhs);
          } else if (PyLong_Check(rhs)) {
            d2 = PyLong_AsLong(rhs);
#if PY_MAJOR_VERSION < 3
          } else if (PyInt_Check(rhs)) {
            d2 = PyInt_AsLong(rhs);
#endif
          } else {
            return NULL;
          }

          return PyComplex_FromDoubles(d1, d2);
        }
        if (fun == FUNCTOR_curly1) {
           PyObject *dict;
	   Term yt = ArgOfTerm(1,Yap_GetFromHandle(t));
          if (!(dict = PyDict_New()))
            return NULL;
          Py_INCREF(dict);
	  while (IsApplTerm(yt) && FunctorOfTerm(yt) == FunctorComma) {
	    if (!entry_to_dictionary(dict, ArgOfTerm(1,yt), eval, cvt))
	      return NULL;
	    yt = ArgOfTerm(2,yt);
	  }
	  if (entry_to_dictionary(dict, yt, eval, cvt))
	    return dict;
	  else
	    return Py_None;         
        }
        if (fun == FUNCTOR_var1) {
	  return Py_None;
        }
       atom_t name;
       size_t arity;

        AOK(PL_get_name_arity(t, &name, &arity), NULL);

        if (name == ATOM_t) {
          int i;
          PyObject *rc = PyTuple_New(arity);
          for (i = 0; i < arity; i++) {
            term_t arg = PL_new_term_ref();
            if (!PL_get_arg(i + 1, t, arg)) {
              PL_reset_term_refs(arg);
              YAPPy_ThrowError(SYSTEM_ERROR_INTERNAL, t, "t(...)->python");
            }
            PyObject *a = term_to_python(arg, eval, NULL, cvt);
            if (a) {
              if (PyTuple_SetItem(rc, i, a) < 0) {
                PL_reset_term_refs(arg);
                YAPPy_ThrowError(SYSTEM_ERROR_INTERNAL, t, "t(...)->python");
              }
            }
            PL_reset_term_refs(arg);
          }
        }
        if (eval)
         return compound_to_pyeval(t, o, cvt);
        else
         return compound_to_pytree(t, o, cvt);
      }
    }
  }
  return Py_None;
}


PyObject *deref_term_to_python(term_t t) {
  // Yap_DebugPlWrite(YAP_GetFromSlot(t));        fprintf(stderr, " here I
  // am\n");
  PyObject *rc;
  YAP_Term yt = YAP_GetFromSlot(t);
  term_t t0 = PL_new_term_ref();
  if (YAP_IsVarTerm(yt)) {
    char b[1024];
    char *o = YAP_WriteBuffer(yt, b, 1023, 0);
    PyObject *p = PyUnicode_FromString(o);
    rc = p;
  } else {
  rc = term_to_python(t, true, NULL, false);
  }
  PL_reset_term_refs(t0);
  return rc;
}

  PyObject *yap_to_python(Term t, bool eval, PyObject *o, bool cvt) {
    if (t == 0 || t == TermNone)
        return Py_None;
    //  fprintf(stderr,"RS %ld %s:%d\n", LOCAL_CurHandle, __FILE__, __LINE__);
    yhandle_t swit = Yap_InitSlot(t);
    o = term_to_python((term_t)swit, eval, o, cvt);
    LOCAL_CurHandle = swit;
    return o;
}

void YAPPy_ThrowError__(const char *file, const char *function, int lineno,
                        yap_error_number type, term_t where, ...);
