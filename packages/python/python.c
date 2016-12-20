
#include "python.h"

int assign_python(PyObject *root, term_t t, PyObject *e);

atom_t ATOM_true, ATOM_false, ATOM_colon, ATOM_dot, ATOM_none, ATOM_t,
    ATOM_comma, ATOM_builtin, ATOM_A, ATOM_V, ATOM_self;

functor_t FUNCTOR_dollar1, FUNCTOR_abs1, FUNCTOR_all1, FUNCTOR_any1,
    FUNCTOR_bin1, FUNCTOR_brackets1, FUNCTOR_comma2, FUNCTOR_dir1,
    FUNCTOR_float1, FUNCTOR_int1, FUNCTOR_iter1, FUNCTOR_iter2, FUNCTOR_long1,
    FUNCTOR_len1, FUNCTOR_curly1, FUNCTOR_ord1, FUNCTOR_range1, FUNCTOR_range2,
    FUNCTOR_range3, FUNCTOR_sum1, FUNCTOR_pointer1, FUNCTOR_complex2,
    FUNCTOR_plus2, FUNCTOR_sub2, FUNCTOR_mul2, FUNCTOR_div2, FUNCTOR_hat2,
    FUNCTOR_colon2, FUNCTOR_equal2, FUNCTOR_sqbrackets2, FUNCTOR_dot2;

PyObject *py_Main;
PyObject *py_Builtin;
PyObject *py_Yapex;
PyObject *term_to_python(term_t t, bool eval);
foreign_t python_to_ptr(PyObject *pVal, term_t t);

PyObject *py_F2P;
PyObject *term_to_python(term_t t, bool eval);

int assign_python(PyObject *root, term_t t, PyObject *e);

PyObject *ActiveModules[32];
int active_modules = 0;

bool python_in_python;

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
  ATOM_none = PL_new_atom("none");
  ATOM_self = PL_new_atom("self");
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
  py_Main = PyImport_AddModule("__main__");
  py_Builtin = PyImport_AddModule("__builtin__");
  py_Yapex = PyImport_AddModule("yapex");
}

foreign_t end_python(void) {
  Py_Finalize();

  return TRUE;
}

X_API bool init_python(void) {
  char **argv;
  python_in_python = false;
  if (YAP_DelayInit(init_python, "python")) {
    // wait for YAP_Init
    return false;
  }
  //  PyGILState_STATE gstate = PyGILState_Ensure();
  term_t t = PL_new_term_ref();
  if (!Py_IsInitialized()) {
    python_in_python = true;
    YAP_Argv(&argv);
    if (argv) {
#if PY_MAJOR_VERSION < 3
      Py_SetProgramName(argv[0]);
#else
      wchar_t *buf = Py_DecodeLocale(argv[0], NULL);
      Py_SetProgramName(buf);
#endif
    }
    Py_Initialize();
  }
  install_py_constants();
  PL_reset_term_refs(t);
  install_pypreds();
  install_pl2pl();
  // PyGILState_Release(gstate);
  return !python_in_python;
}
