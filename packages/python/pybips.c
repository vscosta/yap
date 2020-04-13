/**
 *
 * @defgroup Py4YAP-Bips Python Built-Ins
 * @ingroup Py4YAP
 *
 * The Python engine includes a large number of Python built-ins. Some
 * of them are interfaced here.
 */

///@{

#include "py4yap.h"

static PyObject *finalLookup(PyObject *i, const char *s) {
  PyObject *os = PyUnicode_FromString(s), *rc = NULL;
  if (i == NULL)
    return NULL;
  
  if (strcmp(s, "none") == 0)
    return Py_None;
  if (PyModule_Check(i)) {
   i = PyModule_GetDict(i);
   }  

  if (PyDict_Check(i))
  {
    if (PyDict_Contains(i, os)) {
      rc = PyDict_GetItem(i, os);
  }
  }
 if (!rc && PyObject_HasAttr(i, os)) {
    rc = PyObject_GetAttr(i, os);
  }
  if (rc)
  {
      Py_IncRef(rc);
      return rc;
  }
  return NULL;
}

PyObject *PythonLookupSpecial(const char *s) {
  if (s == NULL)
    return NULL;
  if (strcmp(s, "true") == 0) {
    return Py_True;
  }
  if (strcmp(s, "false") == 0) {
    return Py_False;
  }
  if (strcmp(s, "[]") == 0) {
    return PyList_New(0);
  }
  if (strcmp(s, "{}") == 0) {
    return PyDict_New();
    /* return __main__,s */
  }
  return NULL;
}

static PyObject *builtin(const char *sp) {
  return PyDict_GetItemString(py_Builtin, sp);
}

PyObject *lookupPySymbol(const char *sp, PyObject *pContext, PyObject **duc) {
  PyObject *out = NULL;
  if (!sp)
    return NULL;
  if ((out = finalLookup(pContext, sp))) {
    return out;
  }
  if ((out = finalLookup(py_Context, sp))) {
    return out;
  }
  if ((out = finalLookup(py_Builtin, sp))) {
    return out;
  }
  if ((out = finalLookup(py_Atoms, sp)))
  {
    return out;
  }
  if ((out = finalLookup(py_Local, sp)) && out != Py_None) {
    return out;
  }
    if ((out = finalLookup(py_Global, sp))) {
    return out;
  }
  if ((out = finalLookup(py_ModDict, sp))) {
    return out;
  }
  if ((out = finalLookup(py_Main, sp))) {
    return out;
  }
  return NULL;
}

PyObject *PythonLookup(const char *s, PyObject *oo) {
  PyObject *o;
  if ((o = PythonLookupSpecial(s)))
    return o;
  if ((o = lookupPySymbol(s, oo, NULL)) == NULL)
    return NULL;
  else {
    Py_INCREF(o);
    return o;
  }
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
PyObject *

find_obj(PyObject* ctx, PyObject *exp, term_t t, bool eval) {
    YAP_Term hd, yt;
    py_Context = ctx;
// Yap_DebugPlWriteln(yt);
while  (true) {
    if (t == 0)
        return Py_None;
    yt = Yap_GetFromSlot(t);
    // a.b = [a|b]
    while (IsPairTerm(yt) && !Yap_IsListTerm(yt)) { // f(X).g(X)
        hd = HeadOfTerm(yt);
        yt = TailOfTerm(yt);
        ctx = yap_to_python(hd, eval, ctx, false);
    }
// f[i] = []([I],f)

    while (IsApplTerm(yt) && FunctorOfTerm(yt) == (Functor) FUNCTOR_sqbrackets2) {
        //  tail is the object o
        YAP_Term tail = ArgOfTerm(2, yt);
        // get the context
        ctx = yap_to_python(tail, true, ctx, eval);
        // t now refers to the index
        yt = ArgOfTerm(1, yt);
        Term indx = HeadOfTerm(yt);
        yt = TailOfTerm(yt);
        PyObject *i;
        i = yap_to_python(indx, true, NULL, eval);
        // check numeric
        if (PySequence_Check(ctx)) {
            // moved = true;
            if (exp) {
                if (PySequence_SetItem(ctx, PyLong_AsLong(i), exp) == 0)
                    return exp;
                return Py_None;
            } else {
                return PySequence_GetItem(ctx, PyLong_AsLong(i));
            }

        } else if (PyDict_Check(ctx)) {
            if (exp) {
                if (PyDict_SetItem(ctx, i, exp) == 0)
                    return exp;
                return Py_None;
            } else {
                if (PyDict_Contains(ctx, i))
                    return PyDict_GetItem(ctx, i);
                return Py_None;
            }

        }
    }
    if (IsAtomTerm(yt)) {
        if (!ctx) {
            if (py_Local && py_Local != Py_None) {
                ctx = py_Local;
            }
            if (py_Global && py_Global != Py_None) {
                ctx = py_Global;
            }
            if (py_Main && py_Main != Py_None) {
                ctx = py_Main;
            }
        }
        const char *s = AtomTermName(yt);
        if (exp) {
            if (PyObject_SetAttrString(ctx, s, exp) == 0)
                return exp;
            return Py_None;
        } else {
            if (PyObject_HasAttrString(ctx, s))
                ctx = PyObject_GetAttrString(ctx, s);
            return Py_None;
        }
    }
    return Py_None;
}
}

PyObject *find_term_obj(PyObject *ob, YAP_Term *yt, bool eval) {
  YAP_Term hd;

  py_Context = NULL;
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
    }
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

static bool legal_symbol(const char *s) {
  int ch;
  while (((ch = *s++) != '\0')) {
    if (isalnum(ch) || ch == '_')
      continue;
    return false;
  }
  return true;
}

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
      PyStructSequence_Desc *desc = PyMem_Calloc(sizeof(PyStructSequence_Desc), 1);
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


PyObject *compound_to_pytree(term_t t, PyObject *context, bool cvt) {
  PyObject *o = py_Main;
  functor_t fun;
  atom_t name;
  size_t arity;

  if (PL_is_variable(t)) {
    return term_to_python(t, false, context, cvt);
  }
  o = find_obj(context, NULL, t, false);
  AOK(PL_get_name_arity(t, &name, &arity), NULL);
  if (arity == 0)
    return term_to_python(t, false, o, false);
  AOK(PL_get_functor(t, &fun), NULL);
  if (!arity) {
    char *s = NULL;

    AOK(!PL_get_atom_chars(t, &s), NULL);
    // this should never happen
    return term_to_python(t, false, o, cvt);
  } else {
    const char *s;
    if (!(s = PL_atom_chars(name))) {
      return NULL;
    }
    Term tleft;
    int i;

    PyObject *out = PyTuple_New(arity);
    if (CHECKNULL(t, out) == NULL) {
      PyErr_Print();
      return NULL;
    }
    //DebugPrintf("Tuple %s/%d = %p\n", name, arity, out);
    for (i = 0; i < arity; i++) {
      PyObject *pArg;
      tleft = ArgOfTerm(i + 1, Yap_GetFromSlot(t));
      pArg = yap_to_python(tleft, false, NULL, cvt);
      if (pArg) {
        /* pArg reference stolen here: */
        PyTuple_SET_ITEM(out, i, pArg);
        Py_INCREF(pArg);
      }
    }
    PyObject *c = lookupPySymbol(s, out, NULL);

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

PyObject *compound_to_pyeval(term_t t, PyObject *context, bool cvt) {
  PyObject *o = NULL;
  atom_t name;
size_t arity;
  functor_t fun;

  o = find_obj(context, NULL, t, true);
  AOK(PL_get_name_arity(t, &name, &arity), NULL);
  if (arity == 0) {
    return term_to_python(t, true, o, cvt);
  }
  if (!PL_get_functor(t, &fun))
    return NULL;
  if (fun == FUNCTOR_abs1) {
    return bip_abs(t);
  } else if (fun == FUNCTOR_all1) {
    return bip_all(t);
  } else if (fun == FUNCTOR_any1) {
    return bip_any(t);
  } else if (fun == FUNCTOR_bin1) {
    return bip_bin(t);
  } else if (fun == FUNCTOR_ord1) {
    return bip_ord(t);
  } else if (fun == FUNCTOR_int1) {
    return bip_int(t);
  } else if (fun == FUNCTOR_long1) {
    return bip_long(t);
  } else if (fun == FUNCTOR_float1) {
    return bip_float(t, true);
  } else if (fun == FUNCTOR_iter1) {
    return bip_iter(t);
  } else if (fun == FUNCTOR_range1 || fun == FUNCTOR_range2 ||
             fun == FUNCTOR_range3) {
    return bip_range(t);
  } else if (fun == FUNCTOR_sum1) {
    return bip_sum(t);
  }
  if (fun == FUNCTOR_len1) {
    term_t targ = PL_new_term_ref();
    PyObject *ptr;

    AOK(PL_get_arg(1, t, targ), NULL);
    ptr = term_to_python(targ, true, NULL, true);
    return PyLong_FromLong(PyObject_Length(ptr));
  }

  if (fun == FUNCTOR_div2) {
    term_t targ = PL_new_term_ref();
    PyObject *lhs, *rhs;

    AOK(PL_get_arg(1, t, targ), NULL);
    lhs = term_to_python(targ, true, NULL, false);
    if (!PyNumber_Check(lhs))
      return NULL;
    AOK(PL_get_arg(2, t, targ), NULL);
    rhs = term_to_python(targ, true, NULL, false);
    if (!PyNumber_Check(rhs))
      return NULL;
#if PY_MAJOR_VERSION < 3
    return PyNumber_Divide(lhs, rhs);
#else
    return PyNumber_TrueDivide(lhs, rhs);
#endif
  }
  if (fun == FUNCTOR_dir1) {
    term_t targ = PL_new_term_ref();
    PyObject *ptr;

    AOK(PL_get_arg(1, t, targ), NULL);
    ptr = term_to_python(targ, true, NULL, true);
    if (!ptr) return NULL;
    return PyObject_Dir(ptr);
    {}
  }

  else if (fun == FUNCTOR_plus2) {
    term_t targ = PL_new_term_ref();
    PyObject *lhs, *rhs;

    if (!PL_get_arg(1, t, targ))
      return NULL;
    // Yap_DebugPlWriteln(YAP_GetFromSlot(t));
    lhs = term_to_python(targ, true, NULL, true);
    AOK(PL_get_arg(2, t, targ), NULL);
    rhs = term_to_python(targ, true, NULL, true);
    if (PySequence_Check(lhs) && PySequence_Check(rhs)) {
      return PySequence_Concat(lhs, rhs);
    }
    if (PyNumber_Check(lhs) && PyNumber_Check(rhs))
      return PyNumber_Add(lhs, rhs);
    return CALL_BIP2(builtin("+"), lhs, rhs);
  } else if (fun == FUNCTOR_sub2) {
    term_t targ = PL_new_term_ref();
    PyObject *lhs, *rhs;

    if (!PL_get_arg(1, t, targ))
      return NULL;
    lhs = term_to_python(targ, true, NULL, true);
    if (!PL_get_arg(2, t, targ))
      return NULL;
    rhs = term_to_python(targ, true, NULL, true);
    if (PyNumber_Check(rhs) && PyNumber_Check(lhs))
      return PyNumber_Subtract(lhs, rhs);
    return CALL_BIP2(builtin("-"), lhs, rhs);
  } else if (fun == FUNCTOR_mul2) {
    term_t targ = PL_new_term_ref();
    PyObject *lhs, *rhs;

    AOK(PL_get_arg(1, t, targ), NULL);
    /* YAP_DebugPlWriteln(YAP_GetTermSlot(arg)); */
    (lhs = term_to_python(targ, true, NULL, true));
    CHECKNULL(targ, lhs);
    AOK(PL_get_arg(2, t, targ), NULL);
    (rhs = term_to_python(targ, true, NULL, true));
    CHECKNULL(targ, rhs);
    if (PySequence_Check(lhs) && (
#if PY_MAJOR_VERSION < 3
                                     PyInt_Check(rhs) ||
#endif
                                     PyLong_Check(rhs))) {
      return PySequence_Repeat(lhs, get_p_int(rhs, 0));
    }
    if (PyNumber_Check(lhs) && PyNumber_Check(rhs))
      return PyNumber_Multiply(lhs, rhs);
    return PyObject_CallFunctionObjArgs(builtin("*"), lhs, rhs);
  }
  if (!arity) {
    char *s = NULL;
    PyObject *pValue;
    AOK(PL_get_atom_chars(t, &s), NULL);
    PyObject_Print(o, stderr, 0);
    pValue = PyObject_GetAttrString(o, s);
    if (CHECKNULL(t, pValue) == NULL) {
      PyErr_Print();
      return NULL;
    }
    return pValue;
  } else {
    char *s = PL_atom_chars(name);
    if (!strcmp(s,"t") || !strcmp(s,"tuple")) {
      YAP_Term tt = YAP_GetFromSlot(t), tleft;
      int i;
      PyObject *rc = PyTuple_New(arity);
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
    PyObject *ys = lookupPySymbol(s, o, NULL), *pArgs;
    int i;
    term_t tleft = PL_new_term_ref();
    bool indict = true;
    PyObject *pyDict = PyDict_New();

    pArgs = Py_None;

    for (i = arity; i > 0; i--) {
      PyObject *pArg;
      AOK(PL_get_arg(i, t, tleft), NULL);
      /* ignore (_) */
      if (indict) {
        if (PL_get_functor(tleft, &fun) && fun == FUNCTOR_equal2) {
	  Term tatt = ArgOfTerm(1,Yap_GetFromSlot(tleft));
	  const char *sk;
	  if (IsAtomTerm(tatt))
	    sk = RepAtom(AtomOfTerm(tatt))->StrOfAE;
	  else if (IsStringTerm(tatt))
	    sk = StringOfTerm(tatt);
	  else
	    return NULL;
	  PyObject *key = PyUnicode_FromString(sk);
	  AOK(PL_get_arg(2, tleft, tleft), NULL);
          PyObject *val = term_to_python(tleft, true, o, cvt);
          if (val == NULL)
            return NULL;
          PyDict_SetItem(pyDict, key, val);
        } else {
          indict = false;
          pArgs = PyTuple_New(i);
        }
      }
      // fprintf(stderr, "Tuple %p: %s\n", pyDict,
      //        PyUnicode_AsUTF8(PyObject_Str(pyDict)));
      if (!indict) {
        if (PL_is_variable(tleft)) {
          pArg = Py_None;
        } else {
          pArg = term_to_python(tleft, true, o, cvt);
          // PyObject_Print(pArg,fdopen(2,"w"),0);
          if (pArg == NULL) {
            pArg = Py_None;
          }
          /* pArg reference stolen here: */
          Py_INCREF(pArg);
        }

        PyTuple_SetItem(pArgs, i - 1, pArg);
      }
    }

    if (pArgs == Py_None) {
      pArgs = PyTuple_New(0);
    }

    PyObject *rc;
    if ( PyCallable_Check(ys)) {
      //PyObject_Print(ys, stderr, 0);
      // PyObject_Print(pArgs, stderr, 0);
      // PyObject_Print(pyDict, stderr, 0);

      // PyObject_Print(pArgs, stderr, 0);
      // PyObject_Print(o, stderr, 0);
      CHECK_CALL(ys, pArgs, pyDict);
      Py_DECREF(pArgs);
      Py_DECREF(ys);
      // PyObject_Print(rc, stderr, 0);
      // DebugPrintf("CallObject %p\n", rc);
    } else {
      if (cvt)
      rc = term_to_nametuple(s, arity, pArgs);
      else {
	rc = PyTuple_New(2);
        PyTuple_SetItem(rc, 0, ys);
        PyTuple_SetItem(rc, 1, pArgs);
      }
    }

    return rc;
  }
}

/// @}
