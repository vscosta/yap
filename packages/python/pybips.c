#include "python.h"

/**
 *
 * @section Python Built-Ins
 *
 * The Python engine includes a large number of Python built-ins. Some
 * of them are interfaced here.
 */

//@{

#include "python.h"

static PyObject *finalLookup(PyObject *i, const char *s) {
  PyObject *rc;
  if (i == NULL)
    return NULL;
  if (PyDict_Check(i)) {
    if ((rc = PyDict_GetItemString(i, s)))
      return rc;
  }
  if (PyModule_Check(i)) {
    if ((rc = PyDict_GetItemString(i, s)))
      return rc;
  }
  if (PyObject_HasAttrString(i, s)) {
    return PyObject_GetAttrString(i, s);
  }
  return NULL;
}

PyObject *PythonLookupSpecial(const char *s) {
  if (strcmp(s, "true") == 0) {
    return Py_True;
  }
  if (strcmp(s, "false") == 0) {
    return Py_False;
  }
  if (strcmp(s, "none") == 0)
    return Py_None;
  if (strcmp(s, "[]") == 0) {
    return PyList_New(0);
  } else if (strcmp(s, "{}") == 0) {
    return PyDict_New();
    /* return __main__,s */
  }
  return NULL;
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
  PyObject *py_Builtin = PyEval_GetBuiltins();
  if ((out = finalLookup(py_Builtin, sp))) {
    return out;
  }
  PyObject *py_Local = PyEval_GetLocals();
  if ((out = finalLookup(py_Local, sp))) {
     return out;
  }
  PyObject *py_Global = PyEval_GetGlobals();
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

int lookupPyModule(Py_mod *q) {
  char buf[1024], *S = buf;
  int prefix = 0;
  int j;
  py_ModDict = PyObject_GetAttrString(py_Sys, "modules");
  PyObject *ob;
  S[0] = '\0';
  while (YAP_IsAtomTerm(q->names[prefix])) {

    strcat(S, YAP_AtomName(YAP_AtomOfTerm(q->names[prefix])));
    strcat(S, ".");
    S += strlen(S);
    prefix++;
  }
  for (j = prefix; j > 0; j--) {
    S = strrchr(buf, '.');
    S[0] = '\0';
    if ((ob = PyDict_GetItemString(py_ModDict, buf)) != NULL &&
        PyModule_Check(ob)) {
      Py_INCREF(ob);
      q->mod = ob;
      return j;
    }
  }
  return 0;
}

PyObject *PythonLookup(const char *s, PyObject *oo) {
  PyObject *o;
  if ((o = PythonLookupSpecial(s)))
    return o;
  return lookupPySymbol(s, oo, NULL);
}

PyObject *find_obj(PyObject *ob, term_t l, bool eval) {
  YAP_Term hd, yt;

  py_Context = NULL;
  yt = YAP_GetFromSlot(l);
  while (YAP_IsPairTerm(yt)) {
    hd = YAP_HeadOfTerm(yt);
    ob = yap_to_python(hd, true, ob);
    ob = CHECKNULL(yt, ob);
    if (!ob){
      return NULL;
    }
    yt = YAP_TailOfTerm(yt);

   }
  YAP_PutInSlot(l, yt);
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

  AOK( PL_get_arg(1, t, t), NULL);
  pVal = term_to_python(t, true, NULL);
  pVal = CHECKNULL( t, pVal );
  nVal = PyNumber_Absolute(pVal);
  Py_DecRef(pVal);
  return nVal;
}

/**
 * Python all
 *
 * @param t Prolog term with a previously constructed Python iterator
 *
 * @return the Python boolean `True` if all elements of the iterator are `True`,
 *    `False`  otherwise.
 */
static PyObject *bip_all(term_t t) {
  PyObject *it, *item, *v;
  PyObject *(*iternext)(PyObject *);
  int cmp;

  AOK( PL_get_arg(1, t, t),  NULL );
  (v = term_to_python(t, true, NULL) );
  v = CHECKNULL( t, v);
  it = PyObject_GetIter(v);
  if (CHECKNULL( t, it) == NULL)
    return NULL;
  iternext = *Py_TYPE(it)->tp_iternext;

  if (PyErr_Occurred()) {
    if (PyErr_ExceptionMatches(PyExc_StopIteration))
      PyErr_Clear();
    else
      return NULL;
  }
  //  PyObject_Print(v, stderr, 0);
  for (;;) {
    item = iternext(it);
    if (CHECKNULL(t,item) == NULL)
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

  AOK( PL_get_arg(1, t, t), NULL  );
  v = term_to_python(t, true, NULL);
  it = PyObject_GetIter(v);
  if (CHECKNULL(t,it) == NULL)
    return NULL;
  iternext = *Py_TYPE(it)->tp_iternext;

  for (;;) {
    item = iternext(it);
    if (CHECKNULL(t,item) == NULL)
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
      return CHECKNULL(t,NULL);
  }
  Py_RETURN_FALSE;
}

static PyObject *bip_bin(term_t t) {
  PyObject *v;

  AOK ( PL_get_arg(1, t, t), NULL);
  v = term_to_python(t, true, NULL);
  return PyNumber_ToBase(v, 2);
}

static PyObject *bip_float(term_t t, bool eval) {
  PyObject *pVal, *o;

  AOK( PL_get_arg(1, t, t), NULL );
  pVal = term_to_python(t, eval, NULL);
  if (PyLong_Check(pVal)) {
    o = PyFloat_FromDouble(PyLong_AsLong(pVal));
#if PY_MAJOR_VERSION < 3
  } else if (PyInt_Check(pVal)) {
    o = PyFloat_FromDouble(PyInt_AsLong(pVal));
#endif
  } else if (PyFloat_Check(pVal)) {
    return pVal;
  } else
    return NULL;
  Py_DECREF(pVal);
  return o;
}

static PyObject *bip_int(term_t t) {
  PyObject *pVal, *o;

  AOK( PL_get_arg(1, t, t), NULL );
  pVal = term_to_python(t, true, NULL);
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
    return NULL;
  Py_DECREF(pVal);
  return o;
}

static PyObject *bip_long(term_t t) {
  PyObject *pVal, *o;

  AOK( PL_get_arg(1, t, t), NULL );
  pVal = term_to_python(t, true, NULL);
  if (PyLong_Check(pVal)) {
    return pVal;
#if PY_MAJOR_VERSION < 3
  } else if (PyInt_Check(pVal)) {
    o = PyLong_FromLong(PyInt_AsLong(pVal));
#endif
  } else if (PyFloat_Check(pVal)) {
    o = pVal;
  } else
    return NULL;
  Py_DECREF(pVal);
  return o;
}

static PyObject *bip_iter(term_t t) {
  PyObject *v;

  AOK( PL_get_arg(1, t, t), NULL );
   v = term_to_python(t, true, NULL);
  return PyObject_GetIter(v);
}

static PyObject *bip_ord(term_t t) {
  PyObject *pVal;
  Py_ssize_t size;

  AOK( PL_get_arg(1, t, t), NULL );
 pVal = term_to_python(t, true, NULL);
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
    return NULL;
  } else if (PyByteArray_Check(pVal)) {
    char *s = PyByteArray_AsString(pVal);

    if (s[1])
      return NULL;
#if PY_MAJOR_VERSION < 3
    return PyInt_FromLong(s[0]);
  } else if (PyString_Check(pVal)) {
    char *s = PyString_AsString(pVal);

    if (s[1])
      return NULL;
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

  AOK( PL_get_arg(1, t, t), NULL );
  seq = term_to_python(t, true, NULL);
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
        PyFPE_START_PROTECT("add", Py_DECREF(item); Py_DECREF(iter); return 0)
            f_result += (double)PyInt_AS_LONG(item);
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
    PyObject *low = term_to_python(arg, eval, NULL);
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

#if PY_MAJOR_VERSION >= 3 && defined(USE_NAMEDTUPLES)
static PyStructSequence_Field pnull[] = {
    {"A1", NULL},  {"A2", NULL},  {"A3", NULL},  {"A4", NULL},
    {"A5", NULL},  {"A6", NULL},  {"A7", NULL},  {"A8", NULL},
    {"A9", NULL},  {"A9", NULL},  {"A10", NULL}, {"A11", NULL},
    {"A12", NULL}, {"A13", NULL}, {"A14", NULL}, {"A15", NULL},
    {"A16", NULL}, {"A17", NULL}, {"A18", NULL}, {"A19", NULL},
    {"A19", NULL}, {"A20", NULL}, {"A21", NULL}, {"A22", NULL},
    {"A23", NULL}, {"A24", NULL}, {"A25", NULL}, {"A26", NULL},
    {"A27", NULL}, {"A28", NULL}, {"A29", NULL}, {"A29", NULL},
    {"A30", NULL}, {"A31", NULL}, {"A32", NULL}, {0}};

static PyObject *structseq_str(PyObject *iobj) {

/* buffer and type size were chosen well considered. */
#define REPR_BUFFER_SIZE 512
#define TYPE_MAXSIZE 100

  PyStructSequence *obj = (PyStructSequence *)iobj;
  PyTypeObject *typ = Py_TYPE(obj);
  bool removelast = false;
  Py_ssize_t len, i;
  char buf[REPR_BUFFER_SIZE];
  char *endofbuf, *pbuf = buf;
  /* pointer to end of writeable buffer; safes space for "...)\0" */
  endofbuf = &buf[REPR_BUFFER_SIZE - 5];

  /* "typename(", limited to  TYPE_MAXSIZE */
  len =
      strlen(typ->tp_name) > TYPE_MAXSIZE ? TYPE_MAXSIZE : strlen(typ->tp_name);
  strncpy(pbuf, typ->tp_name, len);
  pbuf += len;
  *pbuf++ = '(';

  for (i = 0; i < ((PyStructSequence *)obj)->ob_base.ob_size; i++) {
    PyObject *val, *repr;
    char *crepr;

    val = PyStructSequence_GET_ITEM(obj, i);
    repr = PyObject_Str(val);
    if (repr == NULL)
      return NULL;
    crepr = PyUnicode_AsUTF8(repr);
    if (crepr == NULL) {
      Py_DECREF(repr);
      return NULL;
    }

    /* + 3: keep space for ", " */
    len = strlen(crepr) + 2;
    if ((pbuf + len) <= endofbuf) {
      strcpy(pbuf, crepr);
      pbuf += strlen(crepr);
      *pbuf++ = ',';
      *pbuf++ = ' ';
      removelast = 1;
      Py_DECREF(repr);
    } else {
      strcpy(pbuf, "...");
      pbuf += 3;
      removelast = 0;
      Py_DECREF(repr);
      break;
    }
  }
  if (removelast) {
    /* overwrite last ", " */
    pbuf -= 2;
  }
  *pbuf++ = ')';
  *pbuf = '\0';

  return PyUnicode_FromString(buf);
}

static PyObject *structseq_repr(PyObject *iobj) {

/* buffer and type size were chosen well considered. */
#define REPR_BUFFER_SIZE 512
#define TYPE_MAXSIZE 100

  PyStructSequence *obj = (PyStructSequence *)iobj;
  PyTypeObject *typ = Py_TYPE(obj);
  bool removelast = false;
  Py_ssize_t len, i;
  char buf[REPR_BUFFER_SIZE];
  char *endofbuf, *pbuf = buf;
  /* pointer to end of writeable buffer; safes space for "...)\0" */
  endofbuf = &buf[REPR_BUFFER_SIZE - 5];

  /* "typename(", limited to  TYPE_MAXSIZE */
  len =
      strlen(typ->tp_name) > TYPE_MAXSIZE ? TYPE_MAXSIZE : strlen(typ->tp_name);
  strncpy(pbuf, typ->tp_name, len);
  pbuf += len;
  *pbuf++ = '(';

  for (i = 0; i < ((PyStructSequence *)obj)->ob_base.ob_size; i++) {
    PyObject *val, *repr;
    char *crepr;

    val = PyStructSequence_GET_ITEM(obj, i);
    repr = PyObject_Repr(val);
    if (repr == NULL)
      return NULL;
    crepr = PyUnicode_AsUTF8(repr);
    if (crepr == NULL) {
      Py_DECREF(repr);
      return NULL;
    }

    /* + 3: keep space for ", " */
    len = strlen(crepr) + 2;
    if ((pbuf + len) <= endofbuf) {
      strcpy(pbuf, crepr);
      pbuf += strlen(crepr);
      *pbuf++ = ',';
      *pbuf++ = ' ';
      removelast = 1;
      Py_DECREF(repr);
    } else {
      strcpy(pbuf, "...");
      pbuf += 3;
      removelast = 0;
      Py_DECREF(repr);
      break;
    }
  }
  if (removelast) {
    /* overwrite last ", " */
    pbuf -= 2;
  }
  *pbuf++ = ')';
  *pbuf = '\0';

  return PyUnicode_FromString(buf);
}
#endif

PyObject *term_to_nametuple(const char *s, int arity, term_t t) {
  PyObject *o;
#if PY_MAJOR_VERSION >= 3 && 0
  PyTypeObject *typp;
  PyObject *key = PyUnicode_FromString(s);
  if (py_F2P && PyDict_Contains(py_F2P, key)) {
    typp = (PyTypeObject *)PyDict_GetItem(py_F2P, key);
    Py_INCREF(typp);
  } else {

    typp = PyMem_Malloc(sizeof(PyTypeObject));
    PyStructSequence_Desc *desc = PyMem_Malloc(sizeof(PyStructSequence_Desc));

    desc->name = PyUnicode_AsUTF8(key);
    desc->doc = "YAPTerm";
    desc->fields = pnull;
    desc->n_in_sequence = 32;
    if (PyStructSequence_InitType2(typp, desc) < 0)
      return NULL;
    typp->tp_str = structseq_str;
    typp->tp_repr = structseq_repr;
    //     typp = PyStructSequence_NewType(desc);
    Py_INCREF(typp);
    typp->tp_flags |= Py_TPFLAGS_HEAPTYPE;
    // don't do this: we cannot add a type as an atribute.
    //PyModule_AddObject(py_Main, s, (PyObject *)typp);
    if (py_F2P)
      PyDict_SetItem(py_F2P, key, (PyObject *)typp);
  }
#endif
  o = PyTuple_New(arity);
  DebugPrintf("Tuple %p\n", o);
  if (o == NULL)
    return o;
  term_t tleft = PL_new_term_ref();
  int i;

  for (i = 0; i < arity; i++) {
    PyObject *pArg;
    if (!PL_get_arg(i + 1, t, tleft))
      return NULL;
    pArg = term_to_python(tleft, false, NULL );
    if (CHECKNULL(tleft,pArg))
      return NULL;
#if PY_MAJOR_VERSION >= 3
    /* pArg reference stolen here: */
    Py_INCREF(pArg);
    PyStructSequence_SET_ITEM(o, i, pArg);
  }
  ((PyStructSequence *)o)->ob_base.ob_size = arity;
  return o;
#else
    /* pArg reference stolen here: */
    PyTuple_SET_ITEM(o, i, pArg);
  }
  PyObject *o1;
  o1 = PyTuple_New(2);
  PyTuple_SET_ITEM(o1, 0, PyUnicode_FromString(s));
  PyTuple_SET_ITEM(o1, 1, o);
  return o1;
#endif
}

static PyObject *bip_range(term_t t) {
  long ilow = 0, ihigh = 0, istep = 1;
  long bign;
  Py_ssize_t i, n;
  int arity;
  atom_t name;
  term_t arg = PL_new_term_ref();

  PyObject *v;

  if (!PL_get_name_arity(t, &name, &arity))
    return NULL;
  AOK( PL_get_arg(1, t, arg), NULL);
  ilow = get_int(arg, true);
  if (arity == 1) {
    ihigh = ilow;
    ilow = 0;
  } else {
    AOK( PL_get_arg(2, t, arg), NULL );
    ihigh = get_int(arg, true);
    if (arity == 3) {
      AOK( PL_get_arg(3, t, arg), NULL );
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
  AOK ( ( (bign >= 0 && (long)n == bign) || "range() result has too many items" ), NULL );
  v = PyList_New(n);

  if (v == NULL)
    return NULL;
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

static bool copy_to_dictionary(PyObject *dict, term_t targ, term_t taux,
                              bool eval) {
  PyObject *lhs, *rhs;
  term_t tleft = PL_new_term_ref(), tright = PL_new_term_ref();

    functor_t fun;

    AOK (PL_get_functor(targ, &fun), false);
    while (fun == FUNCTOR_comma2) {
      AOK( PL_get_arg(1, targ, tleft), false);
      if (! copy_to_dictionary(dict, tleft, taux, eval) )
	return false;
      AOK ( PL_get_arg(2, targ, targ), false);
      return copy_to_dictionary(dict, tright, taux, eval);
    }
    // PyObject_Print(dict, stderr, 0); fprintf(stderr,"\n");
    // Py_DECREF(lhs);
    // Py_DECREF(rhs);

    AOK(PL_get_arg(1, targ, tleft), false);
    lhs = atom_to_python_string(tleft);
    if (lhs == NULL) {
      return FALSE;
    }
    AOK(PL_get_arg(2, targ, tright), false);
    rhs = term_to_python(tright, eval, NULL);
    if (rhs == NULL) {
      PyErr_Print();
      return false;
    }
    return PyDict_SetItem(dict, lhs, rhs) >= 0;
    // PyObject_Print(dict, stderr, 0); fprintf(stderr,"\n");
    // Py_DECREF(lhs);
    // Py_DECREF(rhs);
  }


PyObject *compound_to_data(term_t t, PyObject *o, functor_t fun, bool exec) {
  atom_t name;
  int len;

  AOK (PL_get_name_arity(t, &name, &len), o);

  if (fun == FUNCTOR_pointer1) {
    void *ptr;

    AOK (PL_get_arg(1, t, t), NULL);
    AOK (PL_get_pointer(t, &ptr), NULL)
    /* return __main__,s */
    return (PyObject *)ptr;
  }
  if (name == ATOM_t) {
    term_t targ = PL_new_term_ref();
    PyObject *out;
    int i;

    out = PyTuple_New(len);
    DebugPrintf("Tuple %p\n", out);

    if (!out)
      return NULL;
    for (i = 0; i < len; i++) {
      AOK ( PL_get_arg(i + 1, t, targ), NULL);
      PyErr_Clear();
      PyObject *oa = term_to_python(targ, true, o);
      bool rc = PyTuple_SET_ITEM(out, i, oa) == 0;
      if (rc) {
        PyErr_Print();
      }
      Py_INCREF(oa);
    }
    return out;
  } else if (fun == FUNCTOR_div2) {
    term_t targ = PL_new_term_ref();
    PyObject *lhs, *rhs;

    AOK ( PL_get_arg(1, t, targ),  NULL );
    lhs = term_to_python(targ, true, NULL);
    if (!PyNumber_Check(lhs))
      return NULL;
    AOK ( PL_get_arg(2, t, targ), NULL );
    rhs = term_to_python(targ, true, NULL);
    if (!PyNumber_Check(rhs))
      return NULL;
#if PY_MAJOR_VERSION < 3
    return PyNumber_Divide(lhs, rhs);
#else
    return PyNumber_TrueDivide(lhs, rhs);
#endif
  }
  if (fun == FUNCTOR_sqbrackets2) {
    term_t targ = PL_new_term_ref(), trhs = PL_new_term_ref();
    PyObject *v;
    Py_ssize_t min, max;
    AOK (PL_get_arg(2, t, targ), NULL);
    v = term_to_python(targ, true, o);

    AOK (PL_get_arg(1, t, targ), NULL );
    AOK (PL_get_list(targ, trhs, targ) , NULL);
    if (PL_is_functor(trhs, FUNCTOR_colon2)) {
      if (!PySequence_Check(v))
        return NULL;
      min = get_p_int(term_to_python(targ, true, NULL), 0);
      AOK (PL_get_arg(1, trhs, targ), NULL);
      if (PL_is_functor(targ, FUNCTOR_colon2)) {
        return NULL;
      }
      max = get_p_int(term_to_python(targ, true, o), PyObject_Size(v));
      return PySequence_GetSlice(v, min, max);
    } else {
      PyObject *ip = term_to_python(trhs, true, o);
      if (PySequence_Check(v)) {
#if PY_MAJOR_VERSION < 3
  if (PyLong_Check(ip) {
          min = PyLong_AsLong(ip);
  } else if (PyInt_Check(ip) {
          min = PyInt_asInt(ip);
      }
#else
        if (PyLong_Check(ip)) {
          PyObject *o = PySequence_GetItem(v, PyLong_AsLong(ip));
          if (CHECKNULL(t,o) == NULL)
          return NULL;
          Py_INCREF(o);
          return o;
        }
#endif
      } else {
        o = PyObject_GetItem(v, ip);
        Py_INCREF(o);
        return o;
      }
    }
  }
  if (fun == FUNCTOR_dollar1) {
    char *s = NULL;
    term_t targ = PL_new_term_ref();
    AOK ( PL_get_arg(1, t, targ), NULL );
    AOK ( PL_get_atom_chars(targ, &s), NULL );
    /* return __main__,s */
    PyObject *o = PyObject_GetAttrString(py_Main, s);
    return o;
  }
  if (fun == FUNCTOR_brackets1) {
    AOK ( PL_get_arg(1, t, t),  NULL );
    return term_to_python(t, true, NULL);
  }
  if (fun == FUNCTOR_complex2) {
    term_t targ = PL_new_term_ref();
    PyObject *lhs, *rhs;
    double d1, d2;

    AOK ( PL_get_arg(1, t, targ), NULL );
    lhs = term_to_python(targ, true, NULL);
    AOK ( PyNumber_Check(lhs), NULL );
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
    AOK (PL_get_arg(2, t, targ), NULL );
    rhs = term_to_python(targ, true, NULL);
    AOK (PyNumber_Check(rhs), NULL);
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
    term_t targ = PL_new_term_ref(), taux = PL_new_term_ref();
    PyObject *dict;

    AOK ( PL_get_arg(1, t, t), NULL );
    if (!(dict = PyDict_New()))
      return NULL;
    DebugPrintf("Dict %p\n", dict);

    while (PL_is_functor(t, FUNCTOR_comma2)) {
      AOK ( PL_get_arg(1, t, targ), NULL );
      AOK ( PL_is_functor(targ, FUNCTOR_colon2), NULL);

      AOK( copy_to_dictionary(dict, targ, taux, true), NULL);
      AOK( PL_get_arg(2, t, t) , NULL );
     }

    if (PL_is_functor(t, FUNCTOR_colon2)) {
      AOK ( copy_to_dictionary(dict, t, taux, true), NULL);
    }
    return dict;
  }
  return o;
}

PyObject *compound_to_pytree(term_t t, PyObject *context) {
  PyObject *o = py_Main, *no;
  functor_t fun;
  atom_t name;
  int arity;

  o = find_obj(context, t, false);
  AOK( PL_get_name_arity(t, &name, &arity), NULL );
  if (arity == 0)
    return term_to_python(t, false, o);
  AOK( PL_get_functor(t, &fun), NULL);
  if ((no = compound_to_data(t, o, fun, false)) != o && no) {
    return no;
  }
  if (!arity) {
    char *s = NULL;

    AOK (!PL_get_atom_chars(t, &s), NULL );
    // this should never happen
    return term_to_python(t, false, o);
  } else {
    const char *s;
    if (!(s = PL_atom_chars(name))) {
      return NULL;
    }
    term_t tleft;
    int i;
    o = PyTuple_New(arity);
    DebugPrintf("Tuple %p\n", o);
    tleft = PL_new_term_ref();
    for (i = 0; i < arity; i++) {
      PyObject *pArg;
      AOK (PL_get_arg(i + 1, t, tleft), NULL );
      pArg = term_to_python(tleft, false, NULL);
      if (pArg == NULL)
        return NULL;
      /* pArg reference stolen here: */
      PyTuple_SET_ITEM(o, i, pArg);
      Py_INCREF(pArg);
    }
    return term_to_nametuple(s, arity, t);
  }
}

PyObject *compound_to_pyeval(term_t t, PyObject *context) {
  PyObject *o = NULL, *no;
  atom_t name;
  int arity;
  functor_t fun;

  o = find_obj(context, t, true);
   AOK (PL_get_name_arity(t, &name, &arity), NULL );
  if (arity == 0)
    return term_to_python(t, true, o);
  if (!PL_get_functor(t, &fun))
    return NULL;
  if ((no = compound_to_data(t, o, fun, true)) != o && no) {
    return no;
  }
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

    AOK (PL_get_arg(1, t, targ), NULL );
    ptr = term_to_python(targ, true, NULL);
    return PyLong_FromLong(PyObject_Length(ptr));
  }
  if (fun == FUNCTOR_dir1) {
    term_t targ = PL_new_term_ref();
    PyObject *ptr;

    AOK ( PL_get_arg(1, t, targ), NULL );
    ptr = term_to_python(targ, true, NULL);
    return PyObject_Dir(ptr);

  }

  else if (fun == FUNCTOR_plus2) {
    term_t targ = PL_new_term_ref();
    PyObject *lhs, *rhs;

    if (!PL_get_arg(1, t, targ))
      return NULL;
    lhs = term_to_python(targ, true, NULL);
    AOK( PL_get_arg(2, t, targ), NULL );
    rhs = term_to_python(targ, true, NULL);
    if (PySequence_Check(lhs) && PySequence_Check(rhs)) {
      return PySequence_Concat(lhs, rhs);
    }
    if (!PyNumber_Check(lhs))
      return NULL;
    if (!PyNumber_Check(rhs))
      return NULL;
    return PyNumber_Add(lhs, rhs);
  } else if (fun == FUNCTOR_sub2) {
    term_t targ = PL_new_term_ref();
    PyObject *lhs, *rhs;

    if (!PL_get_arg(1, t, targ))
      return NULL;
    lhs = term_to_python(targ, true, NULL);
    if (!PyNumber_Check(lhs))
      return NULL;
    if (!PL_get_arg(2, t, targ))
      return NULL;
    rhs = term_to_python(targ, true, NULL);
    if (!PyNumber_Check(rhs))
      return NULL;
    return PyNumber_Subtract(lhs, rhs);
  } else if (fun == FUNCTOR_mul2) {
    term_t targ = PL_new_term_ref();
    PyObject *lhs, *rhs;

    AOK (PL_get_arg(1, t, targ), NULL );
    (lhs = term_to_python(targ, true, NULL));
    CHECKNULL( targ, lhs);
    AOK (PL_get_arg(2, t, targ), NULL );
    (rhs = term_to_python(targ, true, NULL));
    CHECKNULL( targ, rhs);
    if (PySequence_Check(lhs) && (
#if PY_MAJOR_VERSION < 3
                                     PyInt_Check(rhs) ||
#endif
                                     PyLong_Check(rhs))) {
      return PySequence_Repeat(lhs, get_p_int(rhs, 0));
    }
    if (!PyNumber_Check(lhs) + !PyNumber_Check(rhs))
      return NULL;
    return PyNumber_Multiply(lhs, rhs);
  }
  if (!arity) {
    char *s = NULL;
    PyObject *pValue;

    AOK (PL_get_atom_chars(t, &s), NULL );
    pValue = PyObject_GetAttrString(o, s);
    if (CHECKNULL(t,pValue) == NULL) {
      PyErr_Print();
      return NULL;
    }
    return pValue;
  } else {
    char *s = PL_atom_chars(name);
    o = lookupPySymbol(s, o, NULL);
    if (CHECKNULL(t,o) == NULL) {
      PyErr_Print();
      return NULL;
    }
    if ( !PyCallable_Check(o)) {
    return term_to_nametuple(s, arity, t);
    }
    PyObject *pArgs = PyTuple_New(arity);
    DebugPrintf("Tuple %p\n", pArgs);
    int i;
    term_t tleft = PL_new_term_ref();
    for (i = 0; i < arity; i++) {
      PyObject *pArg;
      AOK (PL_get_arg(i + 1, t, tleft),  NULL );
      /* ignore (_) */
      if (i == 0 && PL_is_variable(tleft)) {
	Py_DECREF(pArgs);
        pArgs = Py_None;
      }
      pArg = term_to_python(tleft, true, NULL);
      pArg = CHECKNULL( tleft, pArg );
      /* pArg reference stolen here: */
      Py_INCREF(pArg);
      PyTuple_SetItem(pArgs, i, pArg);
    }
    PyObject *rc;

    //PyObject_Print(pArgs, stderr, 0);
    //PyObject_Print(o, stderr, 0);
	CHECK_CALL(rc ,t, PyObject_CallObject(o, pArgs));
    Py_DECREF(pArgs);
    Py_DECREF(o);
    DebugPrintf("CallObject %p\n", rc);

    return rc;
  }
}
