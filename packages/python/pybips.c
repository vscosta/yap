/*
 *
 * @defgroup Py4YAP-Bips Python Built-Ins
 * @ingroup Py4YAP
 *
 * The Python engine includes a large number of Python built-ins. Some
 * of them are interfaced here.
 */

///@{

#include "py4yap.h"
#include "pyerrors.h"


static PyObject *read_symbol( PyObject *ctx, const char *s) {
  PyObject *out;
  if (!ctx)
      return NULL;
  if (PyObject_HasAttrString(ctx, s)) {
      return PyObject_GetAttrString(ctx,s);
  }
      if( PyModule_Check(ctx)) {
         ctx = PyModule_GetDict(ctx);
     }                                                                                                                                                                                                                                 if ( PyDict_Check(ctx)) {
      if ((out = PyDict_GetItemString(ctx, s))) {
	return out;
      }
    }
     if (PyErr_Occurred())
       PyErr_Clear();
    return NULL;
    }

static PyObject *Lookup(PyObject *ctx, const char *s) {
  PyObject *out;
    if (strcmp(s, "none") == 0)
      return Py_None;
    else if (strcmp(s, "true") == 0) {
      return Py_True;
    }
    else if (strcmp(s, "false") == 0) {
      return Py_False;
    }
    else if (strcmp(s, "[]") == 0) {
      out = PyList_New(0);
    }
    else if (strcmp(s, "{}") == 0) {
      out = PyDict_New();
      return out;
    }
    if (ctx) {
      out = read_symbol(ctx, s);
      return out;
    } else {
      if ((out = read_symbol(py_Context ,s))!=NULL)
      	return out;

      if ((out = read_symbol(PyEval_GetBuiltins(),s))!= NULL)
	return out;
      if (	   (out = read_symbol(PyEval_GetLocals(),s))!=NULL)
	return out;
      if (           (out = read_symbol(PyEval_GetGlobals(),s))!=NULL)
	return out;
      if (       (out = read_symbol(py_Main,s))!=NULL)
          return out;
      if (       (out = read_symbol(py_Context,s))!=NULL)
          return out;
    }
         return NULL;
}


PyObject *PythonLookup(const char *s, PyObject *ctx) {
  PyObject *o;
  o = Lookup(ctx, s);
  return o;
}

PyObject *assign_symbol(const char *s, PyObject *ctx, PyObject *v) {
	PyObject *dict;
	if (!ctx) {
		ctx = py_Context;
    }
    if (PyModule_Check(ctx)) {
        dict = PyModule_GetDict(ctx);
	} else if (PyDict_Check(ctx)) {
		dict = ctx;
	} else {
		dict = NULL;
	}
      if (dict && (PyDict_SetItemString(dict, s, v) >= 0)) {
		  return v;
    }
     if (ctx && (PyObject_SetAttrString(ctx, s, v) >= 0)) {
		 return v;
	}
    return NULL;

}



PyObject *find_term_obj(PyObject *ob, YAP_Term *yt, bool eval) {
  YAP_Term hd;

  // Yap_DebugPlWriteln(yt);
  while (YAP_IsPairTerm(*yt)) {
    hd = YAP_HeadOfTerm(*yt);
    *yt = YAP_TailOfTerm(*yt);
    ob = yap_to_python(hd, true, ob, false);
    if (!ob) {
      return Py_None;
    }
  }
  return ob;
}

#if PY_MAJOR_VERSION >= 3
static PyStructSequence_Field pnull[] = {
  {"A1", NULL},  {"A2", NULL},  {"A3", NULL},  {"A4", NULL},  {"A5", NULL},
  {"A6", NULL},  {"A7", NULL},  {"A8", NULL},  {"A9", NULL},  {"A9", NULL},
  {"A10", NULL}, {"A11", NULL}, {"A12", NULL}, {"A13", NULL}, {"A14", NULL},
  {"A15", NULL}, {"A16", NULL}, {"A17", NULL}, {"A18", NULL}, {"A19", NULL},
  {"A19", NULL}, {"A20", NULL}, {"A21", NULL}, {"A22", NULL}, {"A23", NULL},
  {"A24", NULL}, {"A25", NULL}, {"A26", NULL}, {"A27", NULL}, {"A28", NULL},
  {"A29", NULL}, {"A29", NULL}, {"A30", NULL}, {"A31", NULL}, {"A32", NULL},
  {NULL, NULL}};

#endif


PyObject *term_to_nametuple(const char *s, arity_t arity, PyObject *tuple) {
  PyTypeObject *typp;
  PyObject *key = PyUnicode_FromString(s), *d;
  size_t l = 0;
  if (!legal_symbol(s) || !Py_f2p) {
    PyObject *o1;
    o1 = PyTuple_New(2);
    PyTuple_SET_ITEM(o1, 0, PyUnicode_FromString(s));
    PyTuple_SET_ITEM(o1, 1, tuple);
    return o1;
  }

  if ((l = PyList_Size(Py_f2p)) < arity) {
    for (; l < arity; l++) {
      PyList_Append(Py_f2p, PyDict_New());
    }
  }
  if ((d = PyList_GetItem(Py_f2p, arity - 1)) && PyDict_Contains(d, key)) {
    typp = (PyTypeObject *)PyDict_GetItem(d,key);
  } else {
    {
      PyObject *o1;
      o1 = PyTuple_New(2);
      PyTuple_SET_ITEM(o1, 0, PyUnicode_FromString(s));
      PyTuple_SET_ITEM(o1, 1, tuple);
      return o1;
    } PyStructSequence_Desc *desc = PyMem_Calloc(sizeof(PyStructSequence_Desc), 1);
    char *tnp;
    desc->name = tnp = PyMem_Malloc(strlen(s) + 1);
    strcpy(tnp, s);
    desc->doc = "YAPTerm";
    desc->fields = pnull;
    desc->n_in_sequence = arity;
    typp = PyStructSequence_NewType(desc);
    typp->tp_name = desc->name;

    if (PyStructSequence_InitType2(typp, desc) < 0)
      return NULL;
    typp->tp_traverse = NULL;
    typp->tp_flags |=
      //	Py_TPFLAGS_TUPLE_SUBCLASS|
      Py_TPFLAGS_BASETYPE|
      Py_TPFLAGS_HEAPTYPE;
    // don't do this: we cannot add a type as an atribute.
    // PyModule_AddGObject(py_Main, s, (PyObject *)typp);
    if (d && !PyDict_Contains(d, key)) {
      PyDict_SetItem(d, key, (void*)typp);
      Py_INCREF(key);
      Py_INCREF(typp);
    }
  }
  PyObject *o = PyStructSequence_New(typp);
  Py_INCREF(typp);
  arity_t i;
  for (i = 0; i < arity; i++) {
    PyObject *pArg = PyTuple_GetItem(tuple, i);
    if (pArg) {
      PyStructSequence_SetItem(o, i, pArg);

    }
    // PyObject_Print(pArg,stderr,0);fputc('\n',stderr);
  }
  //((PyStructSequence *)o)->ob_base.ob_size = arity;
  // PyObject_Print(o,stderr,0);fputc('\n',stderr);
  Py_INCREF(o);
  return o;
}

#if YAP4PY_BIPS

/**
 * Python abs
 *
 * @param t Prolog term with a number
 *
 * @return a Python object with the number's absolute value
 */

static PyObject *bip_abs(term_t t) {
  PyObject *pVal, *nVal;

  AOK(PL_get_arg(1, t, t), NULL);
  pVal = term_to_python(t, true, NULL, true);
  pVal = CHECKNULL(t, pVal);
  nVal = PyNumber_Absolute(pVal);
  Py_DecRef(pVal);
  return nVal;
}

/**
 * Python all
 *
 * @param t Prolog term with a previously constructed Python iterator
 > *
 * @return the Python boolean `True` if all elements of the iterator are `True`,
 *    `False`  otherwise.
 */
static PyObject *bip_all(term_t t) {
  PyObject *it, *item, *v;
  PyObject *(*iternext)(PyObject *);
  int cmp;

  AOK(PL_get_arg(1, t, t), NULL);
  (v = term_to_python(t, true, NULL, true));
  v = CHECKNULL(t, v);
  it = PyObject_GetIter(v);
  if (CHECKNULL(t, it) == NULL)
    return Py_None;
  iternext = *Py_TYPE(it)->tp_iternext;

  if (PyErr_Occurred()) {
    if (PyErr_ExceptionMatches(PyExc_StopIteration))
      PyErr_Clear();
    else
      return Py_None;
  }
  //  PyObject_Print(v, stderr, 0);
  for (;;) {
    item = iternext(it);
    if (CHECKNULL(t, item) == NULL)
      break;
    cmp = PyObject_IsTrue(item);
    Py_DECREF(item);
    if (cmp < 0) {
      Py_DECREF(it);
      return Py_None;
    }
    if (cmp == 0) {
      Py_DECREF(it);
      return Py_False;
    }
  }
  Py_DECREF(it);
  return Py_True;
}

/**
 * Python any
 *
 * @param t Prolog term with a previously constructed Python iterator
 *
 * @return the Python boolean `True` if any element of the iterator is `True`,
 *    `False`  if all of them are false.
 */
static PyObject *bip_any(term_t t) {
  PyObject *it, *item, *v;
  PyObject *(*iternext)(PyObject *);
  int cmp;

  AOK(PL_get_arg(1, t, t), NULL);
  v = term_to_python(t, true, NULL, true);
  it = PyObject_GetIter(v);
  if (CHECKNULL(t, it) == NULL)
    return Py_None;
  iternext = *Py_TYPE(it)->tp_iternext;

  for (;;) {
    item = iternext(it);
    if (CHECKNULL(t, item) == NULL)
      break;
    cmp = PyObject_IsTrue(item);
    Py_DECREF(item);
    if (cmp < 0) {
      Py_DECREF(it);
      return Py_None;
    }
    if (cmp == 1) {
      Py_DECREF(it);
      Py_RETURN_TRUE;
    }
  }
  Py_DECREF(it);
  if (PyErr_Occurred()) {
    if (PyErr_ExceptionMatches(PyExc_StopIteration))
      PyErr_Clear();
    else
      return CHECKNULL(t, NULL);
  }
  Py_RETURN_FALSE;
}

static PyObject *bip_bin(term_t t) {
  PyObject *v;

  AOK(PL_get_arg(1, t, t), NULL);
  v = term_to_python(t, true, NULL, true);
  return PyNumber_ToBase(v, 2);
}

static PyObject *bip_float(term_t t, bool eval) {
  PyObject *pVal, *o;

  AOK(PL_get_arg(1, t, t), NULL);
  pVal = term_to_python(t, eval, NULL, eval);
  if (PyLong_Check(pVal)) {
    o = PyFloat_FromDouble(PyLong_AsLong(pVal));
    \
    #if PY_MAJOR_VERSION < 3
  } else if (PyInt_Check(pVal)) {
    o = PyFloat_FromDouble(PyInt_AsLong(pVal));
#endif
  } else if (PyFloat_Check(pVal)) {
    return pVal;
  } else
    return Py_None;
  Py_DECREF(pVal);
  return o;
}

static PyObject *bip_int(term_t t) {
  PyObject *pVal, *o;

  AOK(PL_get_arg(1, t, t), NULL);
  pVal = term_to_python(t, true, NULL, true);
#if PY_MAJOR_VERSION < 3
  if (PyLong_Check(pVal)) {
    o = PyInt_FromLong(PyLong_AsLong(pVal));
  } else if (PyInt_Check(pVal)) {
    return pVal;
#else
    if (PyLong_Check(pVal)) {
      return pVal;
#endif
    } else if (PyFloat_Check(pVal)) {
#if PY_MAJOR_VERSION < 3
      o = PyInt_FromLong(PyFloat_AsDouble(pVal));
#else
      o = PyLong_FromDouble(PyFloat_AsDouble(pVal));
#endif
    } else
      return Py_None;
    Py_DECREF(pVal);
    return o;
  }

  static PyObject *bip_long(term_t t) {
    PyObject *pVal, *o;

    AOK(PL_get_arg(1, t, t), NULL);
    pVal = term_to_python(t, true, NULL, true);
    if (PyLong_Check(pVal)) {
      return pVal;
#if PY_MAJOR_VERSION < 3
    } else if (PyInt_Check(pVal)) {
      o = PyLong_FromLong(PyInt_AsLong(pVal));
#endif
    } else if (PyFloat_Check(pVal)) {
      o = pVal;
    } else
      return Py_None;
    Py_DECREF(pVal);
    return o;
  }

  static PyObject *bip_iter(term_t t) {
    PyObject *v;

    AOK(PL_get_arg(1, t, t), NULL);
    v = term_to_python(t, true, NULL, true);
    return PyObject_GetIter(v);
  }

  static PyObject *bip_ord(term_t t) {
    PyObject *pVal;
    Py_ssize_t size;

    AOK(PL_get_arg(1, t, t), NULL);
    pVal = term_to_python(t, true, NULL, true);
    if (PyUnicode_Check(pVal)) {
#if PY_MAJOR_VERSION < 3
      size = PyUnicode_GET_SIZE(pVal);
#else
      size = PyUnicode_GetLength(pVal);
#endif
      if (size == 1) {
#if PY_MAJOR_VERSION < 3
	long ord = (long)*PyUnicode_AS_UNICODE(pVal);
	return PyInt_FromLong(ord);
#else
	Py_UCS4 ord = PyUnicode_ReadChar(pVal, 0);
	return PyLong_FromLong(ord);
#endif
      }
      return Py_None;
    } else if (PyByteArray_Check(pVal)) {
      char *s = PyByteArray_AsString(pVal);

      if (s[1])
	return Py_None;
#if PY_MAJOR_VERSION < 3
      return PyInt_FromLong(s[0]);
    } else if (PyString_Check(pVal)) {
      char *s = PyString_AsString(pVal);

      if (s[1])
	return Py_None;
      return PyInt_FromLong(s[0]);
#else
      return PyLong_FromLong(s[0]);
#endif
    } else
      return NULL;
  }

  static PyObject *bip_sum(term_t t) {
    PyObject *seq;
    PyObject *result = NULL;
    PyObject *temp, *item, *iter;

    AOK(PL_get_arg(1, t, t), NULL);
    seq = term_to_python(t, true, NULL, true);
    iter = PyObject_GetIter(seq);
    if (iter == NULL)
      return NULL;

    if (result == NULL) {
#if PY_MAJOR_VERSION < 3
      result = PyInt_FromLong(0);
#else
      result = PyLong_FromLong(0);
#endif
      if (result == NULL) {
	Py_DECREF(iter);
	return NULL;
      }
    } else {
#if PY_MAJOR_VERSION < 3
      /* reject string values for 'start' parameter */
      if (PyObject_TypeCheck(result, &PyBaseString_Type)) {
	PyErr_SetString(PyExc_TypeError,
			"sum() can't sum strings [use ''.join(seq) instead]");
	Py_DECREF(iter);
	return NULL;
      }
      Py_INCREF(result);
#endif
    }

#ifndef SLOW_SUM
    /* Fast addition by keeping temporary sums in C instead of new Python objects.
       Assumes all inputs are the same type.  If the assumption fails, default
       to the more general routine.
    */
#if PY_MAJOR_VERSION < 3
    if (PyInt_CheckExact(result)) {
      long i_result = PyInt_AS_LONG(result);
#else
      if (PyLong_CheckExact(result)) {
	long i_result = PyLong_AS_LONG(result);
#endif
	Py_DECREF(result);
	result = NULL;
	while (result == NULL) {
	  item = PyIter_Next(iter);
	  if (item == NULL) {
	    Py_DECREF(iter);
	    if (PyErr_Occurred())
	      return NULL;
#if PY_MAJOR_VERSION < 3
	    return PyInt_FromLong(i_result);
#else
	    return PyLong_FromLong(i_result);
#endif
	  }
#if PY_MAJOR_VERSION < 3
	  if (PyInt_CheckExact(item)) {
	    long b = PyInt_AS_LONG(item);
#else
	    if (PyLong_CheckExact(item)) {
	      long b = PyLong_AS_LONG(item);
#endif
	      long x = i_result + b;
	      if ((x ^ i_result) >= 0 || (x ^ b) >= 0) {
		i_result = x;
		Py_DECREF(item);
		continue;
	      }
	    }
	    /* Either overflowed or is not an int. Restore real objects and process normally
	     */
#if PY_MAJOR_VERSION < 3
	    result = PyInt_FromLong(i_result);
#else
	    result = PyLong_FromLong(i_result);
#endif
	    temp = PyNumber_Add(result, item);
	    Py_DECREF(result);
	    Py_DECREF(item);
	    result = temp;
	    if (result == NULL) {
	      Py_DECREF(iter);
	      return NULL;
	    }
	  }
	}

	if (PyFloat_CheckExact(result)) {
	  double f_result = PyFloat_AS_DOUBLE(result);
	  Py_DECREF(result);
	  result = NULL;
	  while (result == NULL) {
	    item = PyIter_Next(iter);
	    if (item == NULL) {
	      Py_DECREF(iter);
	      if (PyErr_Occurred())
		return NULL;
	      return PyFloat_FromDouble(f_result);
	    }
	    if (PyFloat_CheckExact(item)) {
	      PyFPE_START_PROTECT("add", Py_DECREF(item); Py_DECREF(iter); return 0)
		f_result += PyFloat_AS_DOUBLE(item);
	      PyFPE_END_PROTECT(f_result) Py_DECREF(item);
	      continue;
	    }
#if PY_MAJOR_VERSION < 3
	    if (PyInt_CheckExact(item)) {
	      PyFPE_START_PROTECT("add", Py_DECREF(item); Py_DECREF(iter);
				  return 0)f_result += (double)PyInt_AS_LONG(item);
	      PyFPE_END_PROTECT(f_result) Py_DECREF(item);
	      continue;
	    }
#else
	    if (PyLong_CheckExact(item)) {
	      PyFPE_START_PROTECT("add", Py_DECREF(item); Py_DECREF(iter); return 0)
		f_result += PyLong_AsDouble(item);
	      PyFPE_END_PROTECT(f_result) Py_DECREF(item);
	      continue;
	    }
#endif
	    result = PyFloat_FromDouble(f_result);
	    temp = PyNumber_Add(result, item);
	    Py_DECREF(result);
	    Py_DECREF(item);
	    result = temp;
	    if (result == NULL) {
	      Py_DECREF(iter);
	      return NULL;
	    }
	  }
#endif
	}

	for (;;) {
	  item = PyIter_Next(iter);
	  if (item == NULL) {
	    /* error, or end-of-sequence */
	    if (PyErr_Occurred()) {
	      Py_DECREF(result);
	      result = NULL;
	    }
	    break;

	    /* It's tempting to use PyNumber_InPlaceAdd instead of
	       PyNumber_Add here, to avoid quadratic running time
	       when doing 'sum(list_of_lists, [])'.  However, this
	       would produce a change in behaviour: a snippet like

	       empty = []
	       sum([[x] for x in range(10)], empty)

	       would change the value of empty. */
	    temp = PyNumber_Add(result, item);
	    Py_DECREF(result);
	    Py_DECREF(item);
	    result = temp;
	    if (result == NULL)
	      break;
	  }
	  Py_DECREF(iter);
	  return result;
	}

	//@}

	static long get_int(term_t arg, bool eval) {
	  long low;

	  if (!PL_get_long(arg, &low)) {
	    PyObject *low = term_to_python(arg, eval, NULL, eval);
	    if (PyLong_Check(low)) {
	      return PyLong_AsLong(low);
#if PY_MAJOR_VERSION < 3
	    } else if (PyInt_Check(low)) {
	      return PyInt_AsLong(low);
#endif
	    } else {
	      return 0;
	    }
	  }
	  return low;
	}

	/* Return number of items in range/xrange (lo, hi, step).  step > 0
	 * required.  Return a value < 0 if & only if the true value is too
	 * large to fit in a signed long.
	 */
	static long get_len_of_range(long lo, long hi, long step) {
	  /* -------------------------------------------------------------
	     If lo >= hi, the range is empty.
	     Else if n values are in the range, the last one is
	     lo + (n-1)*step, which must be <= hi-1.  Rearranging,
	     n <= (hi - lo - 1)/step + 1, so taking the floor of the RHS gives
	     the proper value.  Since lo < hi in this case, hi-lo-1 >= 0, so
	     the RHS is non-negative and so truncation is the same as the
	     floor.  Letting M be the largest positive long, the worst case
	     for the RHS numerator is hi=M, lo=-M-1, and then
	     hi-lo-1 = M-(-M-1)-1 = 2*M.  Therefore unsigned long has enough
	     precision to compute the RHS exactly.
	     ---------------------------------------------------------------*/
	  long n = 0;
	  if (lo < hi) {
	    unsigned long uhi = (unsigned long)hi;
	    unsigned long ulo = (unsigned long)lo;
	    unsigned long diff = uhi - ulo - 1;
	    n = (long)(diff / (unsigned long)step + 1);
	  }
	  return n;
	}


	static PyObject *bip_range(term_t t) {
	  long ilow = 0, ihigh = 0, istep = 1;
	  long bign;
	  Py_ssize_t i, n;
	  size_t arity;
	  atom_t name;
	  term_t arg = PL_new_term_ref();

	  PyObject *v;

	  if (!PL_get_name_arity(t, &name, &arity))
	    return NULL;
	  AOK(PL_get_arg(1, t, arg), NULL);
	  ilow = get_int(arg, true);
	  if (arity == 1) {
	    ihigh = ilow;
	    ilow = 0;
	  } else {
	    AOK(PL_get_arg(2, t, arg), NULL);
	    ihigh = get_int(arg, true);
	    if (arity == 3) {
	      AOK(PL_get_arg(3, t, arg), NULL);
	      istep = get_int(arg, true);
	    }
	  }
	  if (istep == 0) {
	    PyErr_SetString(PyExc_ValueError, "range() step argument must not be zero");
	    return NULL;
	  }
	  if (istep > 0)
	    bign = get_len_of_range(ilow, ihigh, istep);
	  else
	    bign = get_len_of_range(ihigh, ilow, -istep);
	  n = (Py_ssize_t)bign;
	  AOK(((bign >= 0 && (long)n == bign) || "range() result has too many items"),
	      NULL);
	  v = PyList_New(n);

	  if (v == NULL)
	    return Py_None;
	  for (i = 0; i < n; i++) {
#if PY_MAJOR_VERSION < 3
	    PyObject *w = PyInt_FromLong(ilow);
#else
	    PyObject *w = PyLong_FromLong(ilow);
#endif
	    if (w == NULL) {
	      Py_DECREF(v);
	      return NULL;
	    }
	    PyList_SET_ITEM(v, i, w);
	    Py_INCREF(w);
	    ilow += istep;
	  }
	  return v;
	}

#endif // BIPS


	PyObject *compound_to_pytree(YAP_Term t, PyObject *context, bool cvt) {
	  PyObject *o = py_Main;
	  YAP_Functor fun;
	  YAP_Atom name;
	  size_t arity;

	  if (YAP_IsVarTerm(t)) {
	    return yap_to_python(t, false, context, cvt);
	  }
	  if (YAP_IsAtomTerm(t) || YAP_IsNumberTerm(t)) {
	    return yap_to_python(t, false, context, cvt);
	  } else if (IsPairTerm(t)) {
	    fun = FunctorDot;
	    arity=2;
	    name = AtomDot;
	  } else {
	    fun = YAP_FunctorOfTerm(t);
	    arity = YAP_ArityOfFunctor(fun);
	    name = YAP_NameOfFunctor(fun);
	  }
	  if (!arity) {
	    // this should never happen
	    return yap_to_python(t, false, o, cvt);
	  } else {
	    const char *s;
	    if (!(s = AtomName(name))) {
	      return NULL;
	    }
	    Term tleft;
	    int i;
	    PyObject *out = PyTuple_New(arity);
	    if (CHECKNULL(t, out) == NULL) {
	      if (PyErr_Occurred())
		PyErr_Print();
	      return NULL;
	    }
	    //DebugPrintf("Tuple %s/%d = %p\n", name, arity, out);
	    for (i = 0; i < arity; i++) {
	      PyObject *pArg;
	      tleft = ArgOfTerm(i + 1, t);
	      pArg = yap_to_python(tleft, false, NULL, cvt);
	      if (pArg) {
		/* pArg reference stolen here: */
		PyTuple_SET_ITEM(out, i, pArg);
		Py_INCREF(pArg);
	      }
	    }
	    PyObject *c = PythonLookup(s, o);

	    if (c && PyCallable_Check(c)) {
	      PyObject *n = PyTuple_New(arity);
	      PyTuple_SET_ITEM(n, 0, c);
	      PyTuple_SET_ITEM(n, 1, out);
	      return n;
	    }
	    if (cvt)
	      return term_to_nametuple(s, arity, out);
	    else {
	      PyObject *rc = PyTuple_New(2);
	      PyTuple_SetItem(rc, 0, PyUnicode_FromString(s));
	      PyTuple_SetItem(rc, 1, out);
	      return rc;
	    }
	  }
	}

#define bad "<0>"
 
	PyObject *compound_to_pyeval(YAP_Term t, PyObject *context, bool cvt) {
	  PyObject *o = py_Main;
	  YAP_Atom name;
	  size_t arity;
	  //Yap_DebugPlWriteln(t);
	  Functor fun;
  
	  //  o = find_obj(context, NULL, t, true);
	  if (YAP_IsAtomTerm(t) || YAP_IsNumberTerm(t)) {
	    return yap_to_python(t, false, o, false);
	  } else if (IsApplTerm(t)||IsPairTerm(t)) {
	    PyObject *rc;
	    if(IsPairTerm(t)) {
	      fun = FunctorDot;
	      arity=2;
	      name = AtomDot;
	    } else {
	      fun = FunctorOfTerm(t);
	    name = NameOfFunctor(fun);
	    arity = ArityOfFunctor(fun);
	    }
	    const char *s = AtomName(name);
	    if (!strcmp(s,"t") || !strcmp(s,"tuple")) {
	      YAP_Term tt = t, tleft;
	      int i;
	      rc = PyTuple_New(arity);
	      PyObject *pArg;
	      for (i=0;i<arity;i++) {
		AOK((tleft = YAP_ArgOfTerm(i+1, tt)), NULL);
		pArg = yap_to_python(tleft, true, NULL, true);
		if (pArg == NULL) {
		  pArg = Py_None;
		}
		/* pArg reference stolen here: */
		Py_INCREF(pArg);
      
		PyTuple_SetItem(rc, i, pArg);
	      }
	      return rc;
	    }
	    int i;
	    bool indict = true;
	    PyObject *pyDict = NULL, *pArgs = NULL;
  
	    for (i = arity; i > 0; i--) {
	      PyObject *pArg;
	      Functor fun;
	      PyErr_Clear();
	      Term tleft = YAP_ArgOfTerm(i, t);
	      /* ignore (_) */
	      if (indict &&
		  !IsVarTerm(tleft) && IsApplTerm(tleft) &&
		  !IsExtensionFunctor((fun = FunctorOfTerm( tleft))) &&
		  fun == FunctorEq) {
		Term tatt = ArgOfTerm(1,tleft);
		if (!pyDict)
		  pyDict = PyDict_New();
		PyObject *val = yap_to_python(ArgOfTerm(2,tleft),true,NULL,cvt);
		set_item(tatt,pyDict,val,true,cvt);
		continue;
	      } else {
		if (indict) {
		  indict = false;
		  pArgs = PyTuple_New(i);
		}
		pArg = yap_to_python(tleft, true, NULL, cvt);
		// PyObject_Print(pArg,fdopen(2,"w"),0);
		if (pArg == NULL) {
		  pArg = Py_None;
		} else
		/* pArg reference stolen here: */
		Py_INCREF(pArg);
		PyTuple_SetItem(pArgs, i - 1, pArg);
	      

	      }
	    }
		
#if 0
	    fprintf(stderr,"\n CALL =  " ); 
	    PyObject_Print(context, stderr, 0);

	    fprintf(stderr,". " ); 
	    PyObject_Print(ys, stderr, 0);
	    fprintf(stderr, "( " ); 
	    //PyObject_Print(pArgs, stderr, 0);
	    if (pyDict)
	      PyObject_Print(pyDict, stderr, 0);
	    fprintf(stderr,")\n o =  " );
	    PyObject_Print(o, stderr, 0);
	    fprintf(stderr,"\n" );
#endif
	    PyObject *
		ys = PythonLookup(s, context);
	    if (!ys)  ys =PyUnicode_FromString(s);
		  if ( ys && PyCallable_Check(ys)) {
		    if (!pArgs)
		      pArgs = PyTuple_New(0);
		    CHECK_CALL(ys, pArgs, pyDict);
		      Py_DECREF(pArgs);
		      Py_DECREF(ys);
		      //		    PyObject_Print(rc, stderr, 0);
		    return rc;
		  } else {
		    PyObject *rc;
		    if (cvt)
		      rc = term_to_nametuple(s, arity, pArgs);
		    else {
		      rc = PyTuple_New(2);
		      PyTuple_SetItem(rc, 0, ys);
		      PyTuple_SetItem(rc, 1, pArgs);
		    }
		return rc;
	     }
	  
	  }
	  return NULL;

	  }
   
	
           
	/** @} */
