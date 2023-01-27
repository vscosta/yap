

#include "Yap.h"

#include "YapCompoundTerm.h"
#include "YapDefs.h"
#include "YapTags.h"
#include "YapTerm.h"
#include "Yatom.h"

#include "YapInterface.h"

#include "py4yap.h"
#include "pyerrors.h"

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
    o = PythonLookup(s, NULL);
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


/**
 * yap_to_python translates and evaluates from Prolog to Python
 *
 * @param t handle to Prolog term
 * @param t whether should  try to evaluate evaluables.
 *
 * @return a Python object descriptor or NULL if failed
 */

PyObject *yap_to_python(YAP_Term t, bool eval, PyObject *o, bool cvt) {
  PyErr_Clear();
switch (YAP_TagOfTerm(t)) {
  case YAP_TAG_UNBOUND:
  case YAP_TAG_ATT:  {
    
    PyObject *out = PyTuple_New(1);
    PyTuple_SET_ITEM(out, 0, PyLong_FromLong((YAP_Int)t));
    if (!cvt)
      return out;
    return term_to_nametuple("v", 1, out);
  };
  case YAP_TAG_ATOM: {
    YAP_Atom at = YAP_AtomOfTerm(t);
    const char *s;

    s = YAP_AtomName(at);
    o = PythonLookup(s, o);
    while (o && PyUnicode_Check(o) ) {
      const char *s = PyUnicode_AsUTF8(o);
      if (!legal_symbol(s))
	return  PyUnicode_FromString(s);
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
  case YAP_TAG_STRING: {
    const char *s = NULL;
    if (IsStringTerm(t)) {
      s = StringOfTerm(t);
    } else if (YAP_IsAtomTerm(t)) {
      s = YAP_AtomName(YAP_AtomOfTerm(t));
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
  case YAP_TAG_INT: 
  case YAP_TAG_LONG_INT: {
    int64_t j;
    j = YAP_IntOfTerm(t);
#if PY_MAJOR_VERSION < 3
    PyObject *o = PyInt_FromLong(j);
    return CHECKNULL(t, o);
#else
    PyObject *o = PyLong_FromLong(j);
    return CHECKNULL(t, o);
#endif
  }

  case YAP_TAG_FLOAT: {
    PyObject *out;
    double fl;
    fl = YAP_FloatOfTerm(t);
    out = PyFloat_FromDouble(fl);
    return CHECKNULL(t, out);
  }
  case YAP_TAG_PAIR: {
      Term t0 = t;
      Term *tail;
      size_t len;
      if ((len = Yap_SkipList(&t0, &tail)) > 0 && *tail == TermNil) {
          PyObject *out, *o;
          int i = 0;

          out = PyList_New(len);
          while (IsPairTerm(t)) {
              Term ai = HeadOfTerm(t);
              o = yap_to_python(ai, eval, o, false);
              PyList_SetItem(out, i++, o);
              t = TailOfTerm(t);
          }
          return out;
      } else {
          o = find_term_obj(o, &t, eval);
          return yap_to_python(*tail, eval, o, cvt);
      }
  }
    
  case YAP_TAG_APPL:
    {

      YAP_Functor fun = YAP_FunctorOfTerm(t);

      if (fun == FunctorPythonObject ||
	  fun == FunctorPointer ||
	  fun == FunctorObject
	  ) {
	void *ptr;

	ptr = YAP_PointerOfTerm(YAP_ArgOfTerm(1, t) );
	return ptr;
      }
      if (fun == FunctorCurly) {
	PyObject *dict;
	Term yt = ArgOfTerm(1,t);
	if (!(dict = PyDict_New()))
	  return NULL;
	Py_INCREF(dict);
	while (IsApplTerm(yt) && FunctorOfTerm(yt) == FunctorComma) {
	  Term item = ArgOfTerm(1,yt);
	    
	  PyObject *val = yap_to_python(ArgOfTerm(2,item), eval, o, false);
	  if (!set_item(ArgOfTerm(1,item), dict, val, eval, false))
	    return NULL;
	  yt = ArgOfTerm(2,yt);
	}
	{
	  PyObject *val = yap_to_python(ArgOfTerm(2,yt), eval, o, false);
	  if (!set_item(ArgOfTerm(1,yt), dict,val, eval, cvt))
	    return NULL;
	}
	return dict;
      }
      if (fun == FunctorEmptySquareBrackets) {
	PyObject *rc;
	Py_ssize_t min, max;
	YAP_Term tobj, tindex;
	// ex: a[H,Y] -> []([H.Y],a)
	tobj = YAP_ArgOfTerm(2, t);
	rc = yap_to_python(tobj, eval, o, false);

	  
	tindex = HeadOfTerm(ArgOfTerm(1, t));
	if (PyDict_Check(rc)) {
	  const char *s = NULL;
	  if (IsAtomTerm(tindex)) {
	    s = YAP_AtomName(YAP_AtomOfTerm(tindex));
	  } else if (IsStringTerm(tindex)) {
	    s = StringOfTerm(tindex);
	  }
	  if (s) {
	    return 	PyDict_GetItemString(rc,s);
	  }
	  PyObject *yy=  yap_to_python(tindex, true, o, false);
	  return 	PyDict_GetItem(rc,yy);
	} else if (PySequence_Check(rc)) {
	  if (YAP_IsApplTerm(tindex) &&
	      YAP_FunctorOfTerm(tindex) == FunctorModule) {
	    min = get_p_int(yap_to_python(ArgOfTerm(1,tindex), true, NULL, false), 0);
	    max = get_p_int(yap_to_python(ArgOfTerm(2,tindex), true, NULL, false), 0);
            return PySequence_GetSlice(rc, min, max);
          } else {
	    ssize_t i = IntegerOfTerm(tindex);
	    if (i < 0 || i >PySequence_Size(rc))
	      return NULL;
	    rc = PySequence_GetItem(rc, i);
	    return rc;
          }
        }
      }
      if (fun == FunctorDollar) {
	const char *s = NULL;
	YAP_Term targ = YAP_ArgOfTerm(1,t);
	s = AtomTermName(targ);
	/* return __main__,s */
	return PythonLookup(s,o);
      }
      if (fun == FunctorBrackets) {

	PyObject *rc;
	YAP_Term targ = YAP_ArgOfTerm(1,t);
	PyObject *ys = yap_to_python(targ, true, o, true);
	CHECK_CALL(ys, PyTuple_New(0), NULL);
	return rc;
      }
      if (fun == FunctorComplex) {
	PyObject *lhs, *rhs;
	double d1, d2;
	YAP_Term targ = YAP_ArgOfTerm(1,t);

	lhs = yap_to_python(targ, eval, NULL, cvt);
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
	targ = YAP_ArgOfTerm(2,t);
	rhs = yap_to_python(targ, eval, NULL, cvt);
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
  
      if (fun == FunctorVar) {
	return Py_None;
      }
      YAP_Atom name = YAP_NameOfFunctor(fun);
      size_t arity = YAP_ArityOfFunctor(fun);

      if (name == AtomT){
	int i;
	PyObject *rc = PyTuple_New(arity);
	for (i = 0; i < arity; i++) {
	  YAP_Term arg = YAP_ArgOfTerm(i+1,t);
	  PyObject *a = yap_to_python(arg, eval, NULL, cvt);
	  if (a) {
	    if (PyTuple_SetItem(rc, i, a) < 0) {
	      YAPPy_ThrowError(SYSTEM_ERROR_INTERNAL, t, "t(...)->python");
	    }
	  }
	}
      }
      if (eval)
	return compound_to_pyeval(t, o, cvt);
      else
	return compound_to_pytree(t, o, cvt);
    }
  case YAP_TAG_REF:
  case YAP_TAG_DBREF:
  case YAP_TAG_OPAQUE:
  case YAP_TAG_BIG_INT:
  case YAP_TAG_RATIONAL:
  case YAP_TAG_ARRAY:
    return Py_None;
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

PyObject *term_to_python(term_t t, bool eval, PyObject *o, bool cvt) {
  //
  YAP_Term  yt = YAP_GetFromSlot(t);
  return yap_to_python(yt, eval,o,cvt);
}

void YAPPy_ThrowError__(const char *file, const char *function, int lineno,
                        yap_error_number type, term_t where, ...);
