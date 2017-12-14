


#include "py4yap.h"
#include <VFS.h>

#include "YapStreams.h"

atom_t ATOM_true, ATOM_false, ATOM_colon, ATOM_dot, ATOM_none, ATOM_t,
    ATOM_comma, ATOM_builtin, ATOM_A, ATOM_V, ATOM_self, ATOM_nil,
    ATOM_brackets, ATOM_curly_brackets;

functor_t FUNCTOR_dollar1, FUNCTOR_abs1, FUNCTOR_all1, FUNCTOR_any1,
    FUNCTOR_bin1, FUNCTOR_brackets1, FUNCTOR_comma2, FUNCTOR_dir1,
    FUNCTOR_float1, FUNCTOR_int1, FUNCTOR_iter1, FUNCTOR_iter2, FUNCTOR_long1,
    FUNCTOR_len1, FUNCTOR_curly1, FUNCTOR_ord1, FUNCTOR_range1, FUNCTOR_range2,
    FUNCTOR_range3, FUNCTOR_sum1, FUNCTOR_pointer1, FUNCTOR_complex2,
    FUNCTOR_plus2, FUNCTOR_sub2, FUNCTOR_mul2, FUNCTOR_div2, FUNCTOR_hat2,
    FUNCTOR_colon2, FUNCTOR_comma2, FUNCTOR_equal2, FUNCTOR_sqbrackets2,
    FUNCTOR_dot2, FUNCTOR_brackets1;

X_API PyObject *py_Builtin;
X_API PyObject *py_Yapex;
X_API PyObject *py_Sys;
PyObject *py_Context;
PyObject *py_ModDict;

VFS_t pystream;

static void *py_open(VFS_t *me, int sno, const char *name, const char *io_mode) {
#if HAVE_STRCASESTR
  if (strcasestr(name, "//python/") == name)
    name += strlen("//python/");
#else
  if (strstr(name, "//python/") == name)
    name += strlen("//python/");
#endif
StreamDesc *st = YAP_RepStreamFromId(sno);
  // we assume object is already open, so there is no need to open it.
  PyObject *stream = string_to_python(name, true, NULL);
  if (stream == Py_None)
    return NULL;
    Py_INCREF(stream);
    st->u.private_data = stream;
    st->vfs = me;
    st->status = Append_Stream_f | Output_Stream_f;
    Yap_DefaultStreamOps(st);
    return stream;
}

static bool py_close(int sno) {
  return true;
    StreamDesc *s = YAP_GetStreamFromId(sno);
  PyObject *fclose = PyObject_GetAttrString(s->u.private_data, "close");
  PyObject *rc = PyObject_CallObject(fclose, NULL);
  bool v = (rc == Py_True);
  return v;
}


static int py_put(int sno, int ch) {
  // PyObject *pyw; // buffer
    //int pyw_kind;
    //PyObject *pyw_data;

    char s[2];
        StreamDesc *st = YAP_GetStreamFromId(sno);
        //  PyUnicode_WRITE(pyw_kind, pyw_data, 0, ch);
        PyObject *err,*fput = PyObject_GetAttrString(st->u.private_data, "write");
  s[0] = ch;
    s[1] = '\0';
  PyObject_CallFunctionObjArgs(fput, PyUnicode_FromString(s), NULL);
if ((err = PyErr_Occurred())) {
  PyErr_SetString(err, "Error in put\n");// %s:%s:%d!\n", __FILE__, __FUNCTION__, __LINE__);
}
  return ch;
}

static int py_get(int sno) {
    StreamDesc *s = YAP_GetStreamFromId(sno);
  PyObject *fget = PyObject_GetAttrString(s->u.private_data, "read");
  PyObject *pyr = PyObject_CallFunctionObjArgs(fget, PyLong_FromLong(1), NULL);
  return PyUnicode_READ_CHAR(pyr, 0);
}

static int64_t py_seek(int sno, int64_t where, int how) {
    StreamDesc *s = YAP_GetStreamFromId(sno);
  PyObject *fseek = PyObject_GetAttrString(s->u.private_data, "seek");
  PyObject *pyr = PyObject_CallFunctionObjArgs(fseek, PyLong_FromLong(where),
                                               PyLong_FromLong(how), NULL);
  return PyLong_AsLong(pyr);
}

static void py_flush(int sno) {
    StreamDesc *s = YAP_GetStreamFromId(sno);
  PyObject *flush = PyObject_GetAttrString(s->u.private_data, "flush");
  PyObject_CallFunction(flush, NULL);
}

#if 0
static void python_output(void) {
  PyObject *stream = string_to_python("sys.stdout", true, NULL);
  StreamDesc *st = YAP_GetStreamFromId(1);
  st->u.private_data = stream;
  st->vfs = &pystream;
  stream = string_to_python("sys.stderr", true, NULL);
  st = YAP_GetStreamFromIds(2);
  st->u.private_data = stream;
  st->vfs = &pystream;
}
#endif

static bool init_python_stream(void) {
  //pyw = PyUnicode_FromString("x");
  //pyw_kind = PyUnicode_KIND(pyw);
  //pyw_data = PyUnicode_DATA(pyw);

  pystream.name = "python stream";
  pystream.vflags =
      VFS_CAN_WRITE | VFS_CAN_EXEC | VFS_CAN_SEEK | VFS_HAS_PREFIX;
  pystream.prefix = "//python/";
  pystream.suffix = NULL;
  pystream.open = py_open;
  pystream.close = py_close;
  pystream.get_char = py_get;
  pystream.put_char = py_put;
  pystream.flush = py_flush;
  pystream.seek = py_seek;
  pystream.next = GLOBAL_VFS;
  GLOBAL_VFS = &pystream;
  // NULL;
  return true;
}

X_API PyObject *Py_f2p;

extern X_API bool python_in_python;

static void add_modules(void) {
  py_Main = PyImport_AddModule("__main__");
  Py_INCREF(py_Main);
  py_Sys = PyImport_AddModule("sys");
  Py_INCREF(py_Sys);
  py_Builtin = PyImport_AddModule("__builtin__");
  Py_INCREF(py_Builtin);
  py_ModDict = PyObject_GetAttrString(py_Sys, "modules");
  // py_Yapex = PyImport_ImportModule("yap4py.yapi");
  // PyObject *py_Yap =
  py_Yapex = PyImport_AddModule("yap4py.yapi");
  if (py_Yapex)
    Py_INCREF(py_Yapex);
  Py_f2p = PythonLookup("f2p", NULL);
  if (Py_f2p)
    Py_INCREF(Py_f2p);
  init_python_stream();
}

static void install_py_constants(void) {
  FUNCTOR_dot2 = PL_new_functor(PL_new_atom("."), 2);
  // FUNCTOR_equal2 = PL_new_functor(PL_new_atom("="), 2);
  // FUNCTOR_boolop1 = PL_new_functor(PL_new_atom("@"), 1);
  ATOM_A = PL_new_atom("A");
  ATOM_V = PL_new_atom("V");
  ATOM_builtin = PL_new_atom("__builtin__");
  ATOM_comma = PL_new_atom(",");
  ATOM_colon = PL_new_atom(":");
  ATOM_true = PL_new_atom("true");
  ATOM_false = PL_new_atom("false");
  ATOM_dot = PL_new_atom(".");
  ATOM_self = PL_new_atom("self");
  ATOM_nil = PL_new_atom("[]");
  ATOM_brackets = PL_new_atom("()");
  ATOM_curly_brackets = PL_new_atom("{}");
  FUNCTOR_abs1 = PL_new_functor(PL_new_atom("abs"), 1);
  FUNCTOR_all1 = PL_new_functor(PL_new_atom("all"), 1);
  FUNCTOR_any1 = PL_new_functor(PL_new_atom("any"), 1);
  FUNCTOR_bin1 = PL_new_functor(PL_new_atom("bin"), 1);
  FUNCTOR_ord1 = PL_new_functor(PL_new_atom("ord"), 1);
  FUNCTOR_int1 = PL_new_functor(PL_new_atom("int"), 1);
  FUNCTOR_long1 = PL_new_functor(PL_new_atom("long"), 1);
  FUNCTOR_float1 = PL_new_functor(PL_new_atom("float"), 1);
  FUNCTOR_curly1 = PL_new_functor(PL_new_atom("{}"), 1);
  FUNCTOR_brackets1 = PL_new_functor(PL_new_atom("()"), 1);
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
  FUNCTOR_complex2 = PL_new_functor(PL_new_atom("i"), 2);
  FUNCTOR_plus2 = PL_new_functor(PL_new_atom("+"), 2);
  FUNCTOR_sub2 = PL_new_functor(PL_new_atom("-"), 2);
  FUNCTOR_mul2 = PL_new_functor(PL_new_atom("*"), 2);
  FUNCTOR_div2 = PL_new_functor(PL_new_atom("/"), 2);
  FUNCTOR_hat2 = PL_new_functor(PL_new_atom("^"), 2);
  FUNCTOR_colon2 = PL_new_functor(PL_new_atom(":"), 2);
  FUNCTOR_comma2 = PL_new_functor(PL_new_atom(","), 2);
  FUNCTOR_equal2 = PL_new_functor(PL_new_atom("="), 2);
  FUNCTOR_sqbrackets2 = PL_new_functor(PL_new_atom("[]"), 2);
}

foreign_t end_python(void) {
  if (!python_in_python)
    Py_Finalize();

  return true;
}

static bool libpython_initialized = 0;

X_API bool do_init_python(void) {
  //  char **argv;
  if (libpython_initialized)
    return true;
  libpython_initialized = true;

  //  PyGILState_STATE gstate = PyGILState_Ensure();
  term_t t = PL_new_term_ref();
  if (!python_in_python)
    Py_Initialize();
  install_py_constants();
  PL_reset_term_refs(t);
  install_pl2pl();
  // PyGILState_Release(gstate);
  add_modules();
//    python_output();
  return true;
}
