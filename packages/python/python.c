
#ifdef _XOPEN_SOURCE
#undef _XOPEN_SOURCE // python does its own thing
#endif
#include <Python.h>
#include <SWI-Prolog.h>
#ifdef HAVE_STAT
#undef HAVE_STAT
#endif
#include <assert.h>

static atom_t ATOM_true,
  ATOM_colon,
  ATOM_false,
  ATOM_t;

static functor_t FUNCTOR_dollar1,
  FUNCTOR_abs1,
  FUNCTOR_all1,
  FUNCTOR_any1,
  FUNCTOR_bin1,
  FUNCTOR_dir1,
  FUNCTOR_float1,
  FUNCTOR_int1,
  FUNCTOR_iter1,
  FUNCTOR_iter2,
  FUNCTOR_long1,
  FUNCTOR_len1,
  FUNCTOR_curly1,
  FUNCTOR_ord1,
  FUNCTOR_range1,
  FUNCTOR_range2,
  FUNCTOR_range3,
  FUNCTOR_sum1,
  FUNCTOR_pointer1,
  FUNCTOR_complex2,
  FUNCTOR_plus2,
  FUNCTOR_sub2,
  FUNCTOR_mul2,
  FUNCTOR_div2,
  FUNCTOR_hat2,
  FUNCTOR_colon2,
  FUNCTOR_comma2,
  FUNCTOR_equal2;

static PyObject *py_Main;
static PyObject *term_to_python(term_t t);

static PyObject *ActiveModules[32];
static int active_modules = 0;

static inline int
proper_ascii_string(char *s)
{
  unsigned char c;

  while ((c = *s++)) {
    if (c > 127)
      return FALSE;
  }
  return TRUE;
}

static Py_ssize_t
get_p_int(PyObject *o, Py_ssize_t def) {
  if (o == NULL)
    return def;
  if (PyLong_Check(o)) {
    return PyLong_AsLong(o);
#if PY_MAJOR_VERSION < 3
  } else if (PyInt_Check(o)) {
    return PyInt_AsLong(o);
#endif
  }
  return def;
}

static PyObject *
find_obj(PyObject *ob, term_t lhs)
{
  char *s;
  PyObject *out, *pName;
  int arity = 0;

  if (!PL_get_atom_chars(lhs, &s)) {
    atom_t name;
    if (!PL_get_name_arity(lhs, &name, &arity) )
	return NULL;
    s = PL_atom_chars(name);
  }
  if (ob) {
    out = PyObject_GetAttrString(ob, s);
    return out;
  }
  if (!ob && !arity) {
#if PY_MAJOR_VERSION<3
     pName = PyString_FromString(s);
#else
    pName = PyUnicode_FromString(s);
#endif
    if (pName == NULL) {
      return NULL;
    }
    if (( out = PyImport_Import(pName))) {
      return out;
    }
  }
  if (!ob && py_Main && (out = PyObject_GetAttrString(py_Main, s) ) )
    return out;
  return NULL;
}

static int
copy_to_dictionary(PyObject *dict, term_t targ, term_t taux)
{
  PyObject *lhs, *rhs;
  term_t tleft = taux, tright = tleft;

  if (!PL_get_arg(1, targ, tleft)) {
    return FALSE;
  }
  lhs = term_to_python(tleft);
  if (!PL_get_arg(2, targ, tright)) {
    return FALSE;
  }
  rhs = term_to_python(tright);
  if (PyDict_SetItem(dict, lhs, rhs) < 0 ) {
    return FALSE;
  }
  //PyObject_Print(dict, stderr, 0); fprintf(stderr,"\n");
  Py_DECREF(lhs);
  Py_DECREF(rhs);
  return TRUE;
}

static PyObject *
bip_abs(term_t t)
{
  PyObject *pVal;

  if (! PL_get_arg(1, t, t) )
    return NULL;
  pVal = term_to_python(t);
  return PyNumber_Absolute(pVal);
}

static PyObject *
bip_all(term_t t)
{
  PyObject *it, *item, *v;
  PyObject *(*iternext)(PyObject *);
  int cmp;

  if (! PL_get_arg(1, t, t) )
    return NULL;
  v = term_to_python(t);
  it = PyObject_GetIter(v);
  if (it == NULL)
    return NULL;
  iternext = *Py_TYPE(it)->tp_iternext;

  //  PyObject_Print(v, stderr, 0);
  for (;;) {
    item = iternext(it);
    if (item == NULL)
      break;
    cmp = PyObject_IsTrue(item);
    Py_DECREF(item);
    if (cmp < 0) {
      Py_DECREF(it);
      return NULL;
    }
    if (cmp == 0) {
      Py_DECREF(it);
      return Py_False;
    }
  }
  Py_DECREF(it);
  if (PyErr_Occurred()) {
    if (PyErr_ExceptionMatches(PyExc_StopIteration))
      PyErr_Clear();
    else
      return NULL;
  }
  return Py_True;
}

static PyObject *
bip_any(term_t t)
{
  PyObject *it, *item, *v;
  PyObject *(*iternext)(PyObject *);
  int cmp;

  if (! PL_get_arg(1, t, t) )
    return NULL;
  v = term_to_python(t);
  it = PyObject_GetIter(v);
  if (it == NULL)
    return NULL;
  iternext = *Py_TYPE(it)->tp_iternext;

  for (;;) {
    item = iternext(it);
    if (item == NULL)
      break;
    cmp = PyObject_IsTrue(item);
    Py_DECREF(item);
    if (cmp < 0) {
      Py_DECREF(it);
      return NULL;
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
      return NULL;
  }
  Py_RETURN_FALSE;
}

static PyObject *
bip_bin(term_t t)
{
  PyObject *v;

  if (! PL_get_arg(1, t, t) )
    return NULL;
  v = term_to_python(t);
  return PyNumber_ToBase(v, 2);
}


static PyObject *
bip_float(term_t t)
{
  PyObject *pVal, *o;

  if (! PL_get_arg(1, t, t) )
    return NULL;
  pVal = term_to_python(t);
  if (PyLong_Check(pVal)) {
    o = PyFloat_FromDouble( PyLong_AsLong(pVal) );
#if PY_MAJOR_VERSION<3
  } else if (PyInt_Check(pVal)) {
    o =  PyFloat_FromDouble( PyInt_AsLong(pVal) );
#endif
  } else if (PyFloat_Check(pVal)) {
    return pVal;
  } else
    return NULL;
  Py_DECREF(pVal);
  return o;
}

static PyObject *
bip_int(term_t t)
{
  PyObject *pVal, *o;

  if (! PL_get_arg(1, t, t) )
    return NULL;
  pVal = term_to_python(t);
#if PY_MAJOR_VERSION<3
  if (PyLong_Check(pVal)) {
    o = PyInt_FromLong( PyLong_AsLong(pVal) );
  } else if (PyInt_Check(pVal)) {
    return pVal;
#else
  if (PyLong_Check(pVal)) {
    return pVal;
#endif
  } else if (PyFloat_Check(pVal)) {
#if PY_MAJOR_VERSION<3
    o = PyInt_FromLong( PyFloat_AsDouble(pVal) );
#else
    o = PyLong_FromDouble( PyFloat_AsDouble(pVal) );
#endif
  } else
    return NULL;
  Py_DECREF(pVal);
  return o;
}

static PyObject *
bip_long(term_t t)
{
  PyObject *pVal, *o;

  if (! PL_get_arg(1, t, t) )
    return NULL;
  pVal = term_to_python(t);
  if (PyLong_Check(pVal)) {
    return pVal;
#if PY_MAJOR_VERSION<3
  } else if (PyInt_Check(pVal)) {
    o = PyLong_FromLong( PyInt_AsLong(pVal) );
#endif
  } else if (PyFloat_Check(pVal)) {
    o = PyLong_FromLong( PyFloat_AsDouble(pVal) );
  } else
    return NULL;
  Py_DECREF(pVal);
  return o;
}

static PyObject *
bip_iter(term_t t)
{
  PyObject *v;

  if (! PL_get_arg(1, t, t) )
    return NULL;
  v = term_to_python(t);
  return PyObject_GetIter(v);
}


static PyObject *
bip_ord(term_t t)
{
  PyObject *pVal;
  Py_ssize_t size;

  if (! PL_get_arg(1, t, t) )
    return NULL;
  pVal = term_to_python(t);
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

static PyObject*
bip_sum(term_t t)
{
  PyObject *seq;
  PyObject *result = NULL;
  PyObject *temp, *item, *iter;

  if (! PL_get_arg(1, t, t) )
    return NULL;
  seq = term_to_python(t);
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
    while(result == NULL) {
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
	if ((x^i_result) >= 0 || (x^b) >= 0) {
	  i_result = x;
	  Py_DECREF(item);
	  continue;
	}
      }
      /* Either overflowed or is not an int. Restore real objects and process normally */
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
    while(result == NULL) {
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
	PyFPE_END_PROTECT(f_result)
	  Py_DECREF(item);
	continue;
      }
#if PY_MAJOR_VERSION < 3
      if (PyInt_CheckExact(item)) {
	PyFPE_START_PROTECT("add", Py_DECREF(item); Py_DECREF(iter); return 0)
	  f_result += (double)PyInt_AS_LONG(item);
#else
      if (PyLong_CheckExact(item)) {
	PyFPE_START_PROTECT("add", Py_DECREF(item); Py_DECREF(iter); return 0)
	  f_result += PyLong_AsDouble(item);
#endif
	PyFPE_END_PROTECT(f_result)
	  Py_DECREF(item);
	continue;
      }
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
  }
#endif

  for(;;) {
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

static long
get_int(term_t arg)
{
  long ilow;

  if (!PL_get_long(arg, &ilow)) {
    PyObject *low = term_to_python(arg);
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
  return ilow;
}


/* Return number of items in range/xrange (lo, hi, step).  step > 0
 * required.  Return a value < 0 if & only if the true value is too
 * large to fit in a signed long.
 */
static long
get_len_of_range(long lo, long hi, long step)
{
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

static PyObject *
bip_range(term_t t)
{
    long ilow = 0, ihigh = 0, istep = 1;
    long bign;
    Py_ssize_t i, n;
    int arity;
    atom_t name;
    term_t arg = PL_new_term_ref();

    PyObject *v;

    if (!PL_get_name_arity(t, &name, &arity) )
	return NULL;
    if (! PL_get_arg(1, t, arg) )
      return NULL;
    ilow = get_int(arg);
    if (arity == 1) {
      ihigh = ilow;
      ilow = 0;
    } else {
      if (! PL_get_arg(2, t, arg) )
	return NULL;
      ihigh = get_int(arg);
      if (arity == 3) {
	if (! PL_get_arg(3, t, arg) )
	  return NULL;
	istep = get_int(arg);
      }
    }
    if (istep == 0) {
        PyErr_SetString(PyExc_ValueError,
                        "range() step argument must not be zero");
        return NULL;
    }
    if (istep > 0)
        bign = get_len_of_range(ilow, ihigh, istep);
    else
        bign = get_len_of_range(ihigh, ilow, -istep);
    n = (Py_ssize_t)bign;
    if (bign < 0 || (long)n != bign) {
        PyErr_SetString(PyExc_OverflowError,
                        "range() result has too many items");
        return NULL;
    }
    v = PyList_New(n);
    if (v == NULL)
        return NULL;
    for (i = 0; i < n; i++) {
#if PY_MAJOR_VERSION < 3
        PyObject *w =  PyInt_FromLong(ilow);
#else
        PyObject *w =  PyLong_FromLong(ilow);
#endif
        if (w == NULL) {
            Py_DECREF(v);
            return NULL;
        }
        PyList_SET_ITEM(v, i, w);
        ilow += istep;
    }
    return v;
}


static PyObject *
term_to_python(term_t t)
{
  // Yap_DebugPlWrite(YAP_GetFromSlot(t));        fprintf(stderr, " here I am\n");
  switch (PL_term_type(t)) {
  case PL_VARIABLE:
    return NULL;
  case PL_ATOM:
  case PL_STRING:
    {
      char *s;
      atom_t at;

      if (PL_get_atom(t, &at)) {
	if (at == ATOM_true) return Py_True;
	if (at == ATOM_false) return Py_False;
      }
      if (!PL_get_chars(t, &s, REP_UTF8|CVT_ATOM|CVT_STRING|BUF_DISCARDABLE) ) {
	return NULL;
      }
#if PY_MAJOR_VERSION < 3
      if (proper_ascii_string(s)) {
	return PyString_FromStringAndSize(s, strlen(s) );
      } else
#endif
	{
	  PyObject *pobj = PyUnicode_DecodeUTF8(s, strlen(s), NULL);
	  //fprintf(stderr, "%s\n", s);
	  return pobj;
	}
    }
  case PL_INTEGER:
    {
      int64_t j;
      if (!PL_get_int64_ex(t, &j))
	return NULL;
#if PY_MAJOR_VERSION < 3
      return PyInt_FromLong(j);
#else
      return PyLong_FromLong(j);
#endif
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
	Py_INCREF((PyObject *)ptr);
	/* return __main__,s */
	return (PyObject *)ptr;
      } else if (fun == FUNCTOR_abs1) {
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
	return bip_float(t);
      } else if (fun == FUNCTOR_iter1) {
	return bip_iter(t);
      } else if (fun == FUNCTOR_range1 ||
		 fun == FUNCTOR_range2 ||
		 fun == FUNCTOR_range3) {
	return bip_range(t);
      } else if (fun == FUNCTOR_sum1) {
	return bip_sum(t);
      } else if (fun == FUNCTOR_len1) {
	term_t targ = PL_new_term_ref();
	PyObject *ptr;

	if (! PL_get_arg(1, t, targ) )
	  return NULL;
	ptr = term_to_python(targ);
	return PyLong_FromLong(PyObject_Length(ptr));
      } else if (fun == FUNCTOR_dir1) {
	term_t targ = PL_new_term_ref();
	PyObject *ptr;

	if (! PL_get_arg(1, t, targ) )
	  return NULL;
	ptr = term_to_python(targ);
	return PyObject_Dir(ptr);
      } else if (fun == FUNCTOR_complex2) {
	term_t targ = PL_new_term_ref();
	PyObject *lhs, *rhs;
	double d1, d2;

	if (! PL_get_arg(1, t, targ) )
	  return NULL;
	lhs = term_to_python(targ);
	if (!PyNumber_Check(lhs))
	  return NULL;
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
	if (! PL_get_arg(2, t, targ) )
	  return NULL;
	rhs = term_to_python(targ);
	if (!PyNumber_Check(rhs))
	  return NULL;
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
      } else if (fun == FUNCTOR_curly1) {
	term_t targ = PL_new_term_ref(), taux = PL_new_term_ref();
	PyObject *dict;

	if (! PL_get_arg(1, t, t) )
	  return NULL;
	if (! (dict = PyDict_New() ) )
	  return NULL;
	while (PL_is_functor(t, FUNCTOR_comma2)) {
	  if (! PL_get_arg(1, t, targ) )
	    return NULL;
	  if (PL_is_functor(targ, FUNCTOR_colon2)) {
	    if ( !copy_to_dictionary(dict, targ, taux) )
	      return NULL;
	    if (! PL_get_arg(2, t, t) )
	      return NULL;
	  } else {
	    return NULL;
	  }
	}
	if (PL_is_functor(t, FUNCTOR_colon2)) {
	  if ( !copy_to_dictionary(dict, t, taux) )
	    return NULL;
	} else {
	  return NULL;
	}
	return dict;
      } else if (fun == FUNCTOR_plus2) {
	term_t targ = PL_new_term_ref();
	PyObject *lhs, *rhs;

	if (! PL_get_arg(1, t, targ) )
	  return NULL;
	lhs = term_to_python(targ);
	if (! PL_get_arg(2, t, targ) )
	  return NULL;
	rhs = term_to_python(targ);
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
	if (! PL_get_arg(2, t, targ) )
	  return NULL;
	rhs = term_to_python(targ);
	if (PySequence_Check(lhs) && (
#if PY_MAJOR_VERSION < 3
	PyInt_Check(rhs) ||
#endif
	  PyLong_Check(rhs)) ){
	  return PySequence_Repeat(lhs, get_p_int(rhs, 0));
	}
	if (!PyNumber_Check(lhs)+!PyNumber_Check(rhs))
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
#if PY_MAJOR_VERSION < 3
        return
	  PyNumber_Divide(lhs, rhs);
#else
        return
	  PyNumber_TrueDivide(lhs, rhs);
#endif
      } else if (fun == FUNCTOR_hat2) {
	term_t targ = PL_new_term_ref(), trhs = PL_new_term_ref();
	PyObject *lhs, *rhs;

	if (! PL_get_arg(1, t, targ))
	  return NULL;
	lhs = term_to_python(targ);
	if (! PL_get_arg(2, t, targ)  || !PL_is_list(targ) || !PL_get_list(targ, trhs, targ)  )
	  return NULL;
	if (PL_is_functor(trhs, FUNCTOR_colon2) ) {
	  Py_ssize_t left, right;
	  if (!PL_get_arg(1, trhs, targ))
	    return NULL;
	  left = get_p_int(term_to_python(targ), 0);
	  if (!PL_get_arg(2, trhs, targ))
	    return NULL;
	  right = get_p_int(term_to_python(targ), PyObject_Size(lhs) );
	  if (!PySequence_Check(lhs))
	    return NULL;
	  return PySequence_GetSlice(lhs, left, right);
	} else {
	  rhs = term_to_python(trhs);
	  return PyObject_GetItem(lhs, rhs);
	}
      } else if (fun == FUNCTOR_colon2) {
	term_t tleft = PL_new_term_ref();
	PyObject *pArgs, *o;
	long i;
	int arity;
	atom_t name;

	o = NULL;
	while (fun == FUNCTOR_colon2) {
	  if (! PL_get_arg(1, t, tleft) )
	    return FALSE;
	  o = find_obj(o, tleft);
	  if (!o)
	    return FALSE;
	  if (! PL_get_arg(2, t, t) )
	    return FALSE;
	  if (!PL_get_functor(t, &fun))
	    break;
	}
	if (! PL_get_name_arity( t, &name, &arity) ) {
	  return NULL;
	}
	if (!arity) {
	  char *s;
	  PyObject *pValue;

	  if (!PL_get_atom_chars(t, &s))
	    return NULL;
	  if ((pValue = PyObject_GetAttrString(o, s)) == NULL) {
	    PyErr_Print();
	    return NULL;
	  }
	  return pValue;
	}
	o = PyObject_GetAttrString(o, PL_atom_chars(name));
	if (!o || ! PyCallable_Check(o)) {
	  return NULL;
	}
	pArgs = PyTuple_New(arity);
	for (i = 0 ; i < arity; i++) {
	  PyObject *pArg;
	  if (! PL_get_arg(i+1, t, tleft) )
	    return NULL;
	  /* ignore (_) */
	  if (i == 0 && PL_is_variable(tleft)) {
	    Py_DECREF(pArgs);
	    pArgs = NULL;
	  }
	  pArg = term_to_python(tleft);
	  if (pArg == NULL)
	    return NULL;
	  /* pArg reference stolen here: */
	  PyTuple_SetItem(pArgs, i,  pArg);
	}
	return PyObject_CallObject(o, pArgs);
      } else {
	atom_t name;
	int len;

	if (! PL_get_name_arity( t, &name, &len) ) {
	  return NULL;
	}
	if (name == ATOM_t) {
	  term_t targ = PL_new_term_ref();
	  PyObject *out;
	  int i;

	  out = PyTuple_New(len);
	  if (!out)
	    return NULL;
	  for (i=0; i< len; i++) {
	    if (!PL_get_arg(i+1, t, targ)) {
	      return NULL;
	    }
	    if (PyTuple_SetItem(out, i, term_to_python(targ)) < 0)
	      return NULL;
	  }
	  return out;
	}
      }
    }
    return NULL;
  }
  return NULL;
}

static int
assign_python(PyObject *root, term_t t, PyObject *e)
{
  switch (PL_term_type(t)) {
  case PL_VARIABLE:
    return -1;
  case PL_ATOM:
    {
      char *s;

      if (!PL_get_atom_chars(t, &s)) {
	wchar_t *w;
	atom_t at;
	size_t len;
	PyObject *wo;

	if (!PL_get_atom(t, &at))
	  return -1;
	if (!(w = PL_atom_wchars(at, &len)))
	  return -1;
	wo = PyUnicode_FromWideChar(w, wcslen(w) );
        return PyObject_SetAttr(root, wo, e);
      }
      if (proper_ascii_string(s)) {
        return PyObject_SetAttrString(root, s, e);
      } else {
	PyObject *wo=  PyUnicode_DecodeLatin1(s, strlen(s), NULL);
        return PyObject_SetAttr(root, wo, e);
      }
    }
  case PL_INTEGER:
  case PL_STRING:
  case PL_FLOAT:
    return -1;
  case PL_TERM:
    if (PL_is_list(t)) {
      return -1;
    } else {
      functor_t fun;

      if (!PL_get_functor(t, &fun))
	return -1;
      if (fun == FUNCTOR_dollar1) {
	char *s;


	if (! PL_get_arg(1, t, t) )
	  return -1;
	if (!PL_get_atom_chars(t, &s)) {
	  wchar_t *w;
	  atom_t at;
	  size_t len;
	  PyObject *attr;

	  if (!PL_get_atom(t, &at)) {
	    return -1;
	  }
	  if (!(w = PL_atom_wchars(at, &len)))
	    return -1;
	  attr = PyUnicode_FromWideChar(w, wcslen(w) );
	  if (!attr)
	    return -1;
	  return PyObject_SetAttr(py_Main, attr, e);
	}
	if (proper_ascii_string(s)) {
	  return PyObject_SetAttrString(py_Main, s, e);
	} else {
	  PyObject *attr=  PyUnicode_DecodeLatin1(s, strlen(s), NULL);
	  if (!attr)
	    return -1;
	  return PyObject_SetAttr(py_Main, attr, e);
	}
      } else if (fun == FUNCTOR_pointer1) {
	return -1;
      } else if (fun == FUNCTOR_hat2) {
	term_t targ = PL_new_term_ref(), trhs = PL_new_term_ref();
	PyObject *lhs, *rhs;

	if (! PL_get_arg(1, t, targ) )
	  return -1;
	lhs = term_to_python(targ);
	if (! PL_get_arg(2, t, targ) || !PL_is_list(targ) || !PL_get_list(targ, trhs, targ ) )
	  return -1;
	if (PL_is_functor(trhs, FUNCTOR_colon2) ) {
	  Py_ssize_t left, right;
	  if (!PL_get_arg(1, trhs, targ))
	    return -1;
	  left = get_p_int(term_to_python(targ), 0);
	  if (!PL_get_arg(2, trhs, targ))
	    return -1;
	  right = get_p_int(term_to_python(targ), PyObject_Size(lhs) );
	  if (!PySequence_Check(lhs))
	    return -1;
	  return PySequence_SetSlice(lhs, left, right, e);
	} else {
	  rhs = term_to_python(trhs);
	  return PyObject_SetItem(lhs, rhs, e);
	}
      }
    }
    return -1;
  }
  return -1;
}

static foreign_t
address_to_term(PyObject *pVal, term_t t)
{
    term_t to = PL_new_term_ref(), t1 = PL_new_term_ref();
    PL_put_pointer(t1, (void *)pVal);
    PL_cons_functor(to, FUNCTOR_pointer1, t1);
    Py_INCREF(pVal);
    return PL_unify(t, to);
}

static foreign_t
python_to_term(PyObject *pVal, term_t t)
{
  if (PyBool_Check(pVal)) {
    if (PyObject_IsTrue(pVal)) {
      return PL_unify_atom(t, ATOM_true);
    } else {
      return PL_unify_atom(t, ATOM_false);
    }
  } else if (PyLong_Check(pVal)) {
    return PL_unify_int64(t, PyLong_AsLong(pVal));
#if PY_MAJOR_VERSION < 3
  } else if (PyInt_Check(pVal)) {
    return PL_unify_int64(t, PyInt_AsLong(pVal));
#endif
  } else if (PyFloat_Check(pVal)) {
    return PL_unify_float(t, PyFloat_AsDouble(pVal));
  } else if (PyComplex_Check(pVal)) {
    term_t to = PL_new_term_ref(), t1= PL_new_term_ref(), t2 = PL_new_term_ref();
    if (!PL_put_float(t1, PyComplex_RealAsDouble(pVal) ) ||
	!PL_put_float(t2, PyComplex_ImagAsDouble(pVal) ) ||
	!PL_cons_functor(to, FUNCTOR_complex2, t1, t2) )
      return FALSE;
    return PL_unify(t, to);
  } else if (PyUnicode_Check(pVal)) {
    atom_t tmp_atom;

#if PY_MAJOR_VERSION < 3
    Py_ssize_t sz = PyUnicode_GetSize(pVal)+1;
    wchar_t *ptr = malloc(sizeof(wchar_t)*sz);
    sz = PyUnicode_AsWideChar((PyUnicodeObject *)pVal, ptr, sz-1);
#else
    Py_ssize_t sz = PyUnicode_GetLength(pVal)+1;
    wchar_t *ptr = malloc(sizeof(wchar_t)*sz);
    sz = PyUnicode_AsWideChar(pVal, ptr, sz);
#endif
    tmp_atom = PL_new_atom_wchars(sz, ptr);
    free(ptr);
    return PL_unify_atom(t, tmp_atom);
  } else if (PyByteArray_Check(pVal)) {
    atom_t tmp_atom = PL_new_atom(PyByteArray_AsString(pVal));
    return PL_unify_atom(t, tmp_atom);
#if PY_MAJOR_VERSION < 3
  } else if (PyString_Check(pVal)) {
    atom_t tmp_atom = PL_new_atom(PyString_AsString(pVal));
    return PL_unify_atom(t, tmp_atom);
#endif
  } else if (PyTuple_Check(pVal)) {
    Py_ssize_t i, sz = PyTuple_Size(pVal);
    functor_t f = PL_new_functor(ATOM_t, sz);
    if (!PL_unify_functor(t, f))
      return FALSE;
    for (i = 0; i < sz; i++) {
      term_t to = PL_new_term_ref();
      if (!PL_unify_arg(i+1, t, to))
	return FALSE;
      if ( !python_to_term(PyTuple_GetItem(pVal, i), to) )
	return FALSE;
    }
    return TRUE;
  } else if (PyList_Check(pVal)) {
    term_t to = PL_new_term_ref();
    Py_ssize_t i, sz = PyList_GET_SIZE(pVal);

    for (i = 0; i < sz; i++) {
      if (!PL_unify_list(t, to, t) ||
	  !python_to_term(PyList_GetItem(pVal, i), to))
	return FALSE;
    }
    return PL_unify_nil(t);
  } else if (PyDict_Check(pVal)) {
    Py_ssize_t pos = 0;
    term_t to = PL_new_term_ref(), ti = to;
    int left = PyDict_Size(pVal);
    PyObject *key, *value;

    while (PyDict_Next(pVal, &pos, &key, &value)) {
      term_t tkey = PL_new_term_ref(), tval = PL_new_term_ref(), tint, tnew = PL_new_term_ref();
      /* do something interesting with the values... */
      if (!python_to_term(key, tkey)) {
	return FALSE;
      }
      if (!python_to_term(value, tval)) {
	return FALSE;
      }
      /* reuse */
      tint = tkey;
      if (!PL_cons_functor(tint, FUNCTOR_colon2, tkey, tval)) {
	return FALSE;
      }
      if (--left) {
	if (!PL_cons_functor(tint, FUNCTOR_comma2, tint, tnew))
	  return FALSE;
      }
      if (!PL_unify(ti, tint))
	return FALSE;
      ti = tnew;
    }
    PL_cons_functor(to, FUNCTOR_curly1, to);
    return PL_unify(t, to);
  } else {
    return address_to_term(pVal, t);
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
#if PY_MAJOR_VERSION < 3
  pName = PyString_FromString(s);
#else
  pName = PyUnicode_FromString(s);
#endif
  if (pName == NULL) {
    return FALSE;
  }
  pModule = PyImport_Import(pName);
  Py_DECREF(pName);
  if (pModule == NULL) {
    PyErr_Clear();
    return FALSE;
  }
  ActiveModules[active_modules++] = pModule;
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
#if PY_MAJOR_VERSION < 3
    pName = PyString_FromString(s);
#else
    pName = PyUnicode_FromString(s);
#endif
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
  PyErr_Print();
  Py_DECREF(pModule);
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
python_apply(term_t tin, term_t targs, term_t keywds, term_t tf)
{
  PyObject *pF, *pValue;
  PyObject *pArgs, *pKeywords;
  int i, arity;
  atom_t aname;
  foreign_t out;
  term_t targ = PL_new_term_ref();

  pF = term_to_python(tin);
  if ( pF == NULL ) {
    return FALSE;
  }
  if (PL_is_atom(keywds) )
    pKeywords = NULL;
  else
    pKeywords = term_to_python(keywds);
  if (! PL_get_name_arity( targs, &aname, &arity) ) {
    return FALSE;
  }
  if (aname == ATOM_t) {
    if (arity == 0)
      pArgs = NULL;
    else
      pArgs = term_to_python( targs );
  } else {
    pArgs = PyTuple_New(arity);
    if (!pArgs)
      return FALSE;
    for (i = 0 ; i < arity; i++) {
      PyObject *pArg;
      if (! PL_get_arg(i+1, targs, targ) )
	return FALSE;
      /* ignore (_) */
      if (i == 0 && PL_is_variable(targ)) {
	Py_DECREF(pArgs);
	pArgs = NULL;
	break;
      }
      pArg = term_to_python(targ);
      if (pArg == NULL)
	return FALSE;
      /* pArg reference stolen here: */
      PyTuple_SetItem(pArgs, i,  pArg);
    }
  }
  if (PyCallable_Check(pF)) {
    if (!pKeywords)
      pValue = PyObject_CallObject(pF, pArgs);
    else {
      pValue = PyObject_Call(pF, pArgs, pKeywords);
      //   PyObject_Print(pF,stderr,0);fprintf(stderr, "\n");
      //PyObject_Print(pArgs,stderr,0);fprintf(stderr, " ");
      //PyObject_Print(pKeywords,stderr,0);fprintf(stderr, "\n");
    }
    if (!pValue)
      PyErr_Print();
  } else {
    PyErr_Print();
    return FALSE;
  }
  if (pArgs)
    Py_DECREF(pArgs);
  Py_DECREF(pF);
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

  if (e == NULL)
    return FALSE;
  return assign_python(py_Main, name, e) >= 0;
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
    if ((pValue = PyObject_GetAttrString(o, s)) == NULL) {
      PyErr_Print();
      return FALSE;
    }
    return python_to_term(pValue, out);
  }
  if (! PL_get_name_arity( f, &name, &arity) ) {
    return FALSE;
  }
  /* follow chains of the form a.b.c.d.e() */
  while (name == ATOM_colon && arity == 2) {
    term_t tleft = PL_new_term_ref();
    PyObject *lhs;

    if (! PL_get_arg(1, f, tleft) )
      return FALSE;
    lhs = term_to_python(tleft);
    if ((o = PyObject_GetAttr(o, lhs)) == NULL) {
      PyErr_Print();
      return FALSE;
    }
    if (! PL_get_arg(2, f, f) )
      return FALSE;
    if (! PL_get_name_arity( f, &name, &arity) ) {
      return FALSE;
    }
  }
  s = PL_atom_chars(name);
  if ((pF = PyObject_GetAttrString(o, s)) == NULL) {
    PyErr_Print();
    return FALSE;
  }
  pArgs = PyTuple_New(arity);
  for (i = 0 ; i < arity; i++) {
    PyObject *pArg;
    if (! PL_get_arg(i+1, f, targ) )
      return FALSE;
    /* ignore (_) */
    if (i == 0 && PL_is_variable(targ)) {
      Py_DECREF(pArgs);
      pArgs = NULL;
    }
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
  return python_to_term(pValue, out);
}

static foreign_t
python_field(term_t f, term_t tobj, term_t tname, term_t tout)
{
  PyObject *o = NULL, *pF;
  atom_t name;
  char *s;
  int arity;

  if (! PL_get_name_arity( f, &name, &arity) ) {
    return FALSE;
  }
  /* follow chains of the form a.b.c.d.e() */
  while (name == ATOM_colon && arity == 2) {
    term_t tleft = PL_new_term_ref();
    PyObject *lhs;

   if (! PL_get_arg(1, f, tleft) )
      return FALSE;
    lhs = term_to_python(tleft);
    if (o == NULL) {
      o = lhs;
    } else if ((o = PyObject_GetAttr(o, lhs)) == NULL) {
      //      PyErr_Print();
      PyErr_Clear();
      return FALSE;
    }
    if (! PL_get_arg(2, f, f) )
      return FALSE;
    if (! PL_get_name_arity( f, &name, &arity) ) {
      return FALSE;
    }
  }
  s = PL_atom_chars(name);
  if (!s || !o) {
    return FALSE;
  } else if ((pF = PyObject_GetAttrString(o, s)) == NULL) {
    //    PyErr_Print();
    PyErr_Clear();
    return FALSE;
  }
  return
    address_to_term(pF, tobj) &&
    PL_unify_atom(tname, name) &&
    PL_unify(tout, f);
}

static foreign_t
python_main_module(term_t mod)
{
  return address_to_term(py_Main, mod);
}

static foreign_t
python_function(term_t tobj)
{
  PyObject *obj = term_to_python(tobj);

  return PyFunction_Check(obj);
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
  ATOM_colon = PL_new_atom(":");
  ATOM_true  = PL_new_atom("true");
  ATOM_false = PL_new_atom("false");
  ATOM_t = PL_new_atom("t");
  FUNCTOR_abs1 = PL_new_functor(PL_new_atom("abs"), 1);
  FUNCTOR_all1 = PL_new_functor(PL_new_atom("all"), 1);
  FUNCTOR_any1 = PL_new_functor(PL_new_atom("any"), 1);
  FUNCTOR_bin1 = PL_new_functor(PL_new_atom("bin"), 1);
  FUNCTOR_ord1 = PL_new_functor(PL_new_atom("ord"), 1);
  FUNCTOR_int1 = PL_new_functor(PL_new_atom("int"), 1);
  FUNCTOR_long1 = PL_new_functor(PL_new_atom("long"), 1);
  FUNCTOR_float1 = PL_new_functor(PL_new_atom("float"), 1);
  FUNCTOR_curly1 = PL_new_functor(PL_new_atom("{}"), 1);
  FUNCTOR_dollar1 = PL_new_functor(PL_new_atom("$"), 1);
  FUNCTOR_pointer1 = PL_new_functor(PL_new_atom("__obj__"), 1);
  FUNCTOR_dir1 = PL_new_functor(PL_new_atom("dir"), 1);
  FUNCTOR_iter1 = PL_new_functor(PL_new_atom("iter"), 1);
  FUNCTOR_iter2 = PL_new_functor(PL_new_atom("iter"), 2);
  FUNCTOR_len1 = PL_new_functor(PL_new_atom("len"), 1);
  FUNCTOR_range1 = PL_new_functor(PL_new_atom("range"), 1);
  FUNCTOR_range2 = PL_new_functor(PL_new_atom("range"), 2);
  FUNCTOR_range3 = PL_new_functor(PL_new_atom("range"), 3);
  FUNCTOR_sum1 = PL_new_functor(PL_new_atom("sum"), 1);
  FUNCTOR_complex2 = PL_new_functor(PL_new_atom("complex"), 2);
  FUNCTOR_plus2 = PL_new_functor(PL_new_atom("+"), 2);
  FUNCTOR_sub2 = PL_new_functor(PL_new_atom("-"), 2);
  FUNCTOR_mul2 = PL_new_functor(PL_new_atom("*"), 2);
  FUNCTOR_div2 = PL_new_functor(PL_new_atom("/"), 2);
  FUNCTOR_hat2 = PL_new_functor(PL_new_atom("^"), 2);
  FUNCTOR_colon2 = PL_new_functor(PL_new_atom(":"), 2);
  FUNCTOR_comma2 = PL_new_functor(PL_new_atom(","), 2);
  FUNCTOR_equal2 = PL_new_functor(PL_new_atom("="), 2);

  PL_register_foreign("init_python",	  0, init_python,      0);
  PL_register_foreign("end_python",	  0, end_python,       0);
  PL_register_foreign("python_import",	  2, python_import,       0);
  PL_register_foreign("python_f",	  3, python_f,       0);
  PL_register_foreign("python_o",	  3, python_o,       0);
  PL_register_foreign("python_len",	  2, python_len,       0);
  PL_register_foreign("python_is",	  2, python_is,       0);
  PL_register_foreign("python_dir",	  2, python_dir,       0);
  PL_register_foreign("python_apply",	  4, python_apply,       0);
  PL_register_foreign("python_access",	  3, python_access,       0);
  PL_register_foreign("python_field",	  4, python_field,       0);
  PL_register_foreign("python_assign",	  2, python_assign,       0);
  PL_register_foreign("python_function",  1, python_function,       0);
  PL_register_foreign("python_run_command",	  1, python_run_command,       0);
  PL_register_foreign("python_main_module",	  1, python_main_module,       0);
}
