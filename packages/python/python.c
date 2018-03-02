

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

X_API PyObject *py_Atoms;
X_API PyObject *py_Builtin;
X_API PyObject *py_Yapex;
X_API PyObject *py_Sys;
PyObject *py_Context;
PyObject *py_ModDict;

X_API PyObject *Py_f2p;

extern X_API bool python_in_python;

static void add_modules(void) {
  py_Main = PyImport_AddModule("__main__");
  Py_INCREF(py_Main);
  py_Sys = PyImport_AddModule("sys");
  py_Atoms = PyDict_New();
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
  init_python_vfs();
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
