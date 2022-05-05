/**
 * @file python.c
 *
 * @brief data structures and init for Py4YAP library
 *
 */

/**
 * @defgroup PY4YAP
 * @ingroup python
 * @brief make Python talk to YAP
 * @{
 */

#include "py4yap.h"
#include <VFS.h>

#define USES_REGS

#include "YapStreams.h"

X_API PyObject *py_Atoms;
X_API PyObject *Py_f2p;
X_API  PyObject *py_Ops;
X_API PyObject *py_Sys;
X_API PyObject * pYAPError;
//////X_API PyObject * py_Ops;
PyObject *py_OpMap[3];
PyObject *py_Context;


bool pyStringToString;

extern X_API bool python_in_python;

typedef struct {
  const char*op;
  int arity;
  const char *f;
} op2f_t;

static op2f_t ops[] = {
//> Addition: a + b -> add(a, b)
//> Concatenation: seq1 + seq2 -> concat(seq1, seq2)
  { "+", 2, "add" },
//> Containment Test: obj in seq -> contains(seq, obj)
  { "in", 2, "contains" },
  //> Division: a / b -> truediv(a, b)
  { "/", 2, "truediv" },
  ///> Division: a // b -> floordiv(a, b)
  { "//", 2, "floordiv" },
  ////> Bitwise And: a & b -> and_(a, b)
  { "&", 2, "and_" },
  //> Bitwise Exclusive Or: a ^ b -> xor(a, b)
  { "^", 2, "xor" },
  //> Bitwise Inversion: ~ a -> invert(a)
  { "~", 1, "invert" },
  //> Bitwise Or: a | b -> or_(a, b)
  { "|", 2, "or_" },
  //> Exponentiation: a ** b -> pow(a, b)
  { "**", 2, "pow" },
  //> Identity: a is b -> is_(a, b)
  { "is", 2, "is_" },
  //> Identity: a is not b -> is_not(a, b)
  { "is", 2, "is_not" },
  //> Indexed Assignment: obj[k] = v -> setitem(obj, k, v)
  //  { "=", 2, "setitem" },
  //> Indexed Deletion: del obj[k] -> delitem(obj, k)
  //  { "obj[k]", 2, "delitem" },
  //> Indexing: obj[k] -> getitem(obj, k),
  //  { "->", 1, "getitem" },
  //> Left Shift: a << b -> lshift(a, b)
  { "<<", 2, "lshift" },
  //> Modulo: a % b -> mod(a, b)  },
  { "%", 2, "mod" },
  //> Multiplication: a * b -> mul(a, b)
    { "*", 2, "mul" },
    //> Matrix Multiplication: a @ b -> matmul(a, b)
    { "@", 2, "matmul" },
    //> Negation (Arithmetic): - a -> neg(a)
    //{ "~",1, "neg" },
    { "-",1, "neg" },
    //> Negation (Logical): not a -> not_(a)
    { "not", 1, "not_" },
    //> Positive: + a -> pos(a)
    { "+", 1, "pos" },
    //> Right Shift: a >> b -> rshift(a, b)
    { ">>", 2, "rshift" },
    //> Slice Assignment: seq[i:j] = values -> setitem(seq, slice(i, j), values)
//> Slice Deletion: del seq[i:j] -> delitem(seq, slice(i, j))
//> Slicing: seq[i:j] -> getitem(seq, slice(i, j))
//> String Formatting: s % obj -> mod(s, obj)
//> Subtraction: a - b -> sub(a, b)
    { "-", 2, "sub" },
    //> Truth Test: obj -> truth(obj)
//> Ordering: a < b -> lt(a, b)
    { "<", 2, "lt" },
    //> Ordering: a <= b -> le(a, b)
    { "<=", 2, "le" },
    //> Equality: a == b -> eq(a, b)
    { "==", 2, "eq" },
    //> Difference: a != b -> ne(a, b)
    { "!=", 2, "ne" },
    //> Ordering: a >= b -> ge(a, b)
    { ">=", 2, "ge" },
    //>  Ordering: a > b -> gt(a, b)
    { ">", 2, "gt" }
};



static void add_modules(void) {
  Term exp_string = MkAtomTerm(Yap_LookupAtom("python_export_string_as"));
  if (getYapFlag(exp_string) == TermString)
    pyStringToString = true;
  else
    pyStringToString = false;
  py_Atoms= PyDict_New();

   if ( PyDict_Contains(PyImport_GetModuleDict(), PyUnicode_FromString("__main__"))) {
      py_Main = PyDict_GetItemString(PyImport_GetModuleDict(),"__main__");
    } else {
      py_Main = PyImport_ImportModule("__main__");
  }
  Py_INCREF(py_Main);
  
     py_Sys =  PyImport_ImportModule("sys");
     py_Ops = PyModule_GetDict(PyImport_ImportModule("operator"));
   Py_INCREF(py_Sys);
   Py_INCREF(py_Ops);
      py_OpMap[0] = NULL;
      py_OpMap[1] = PyDict_New();
      py_OpMap[2] = PyDict_New();
      int i;
   for(i =0;i<sizeof(ops)/sizeof(op2f_t);i++){
     PyObject *v=PyDict_GetItemString(py_Ops, ops[i].f);
     PyDict_SetItemString(py_OpMap[ops[i].arity],ops[i].op,v);
   }
     //  op = pyDict_GetItemString(py_Main, "__builtins__");
  PyObject *py_Yapex = PyImport_ImportModule("yap4py.yapi");
  if (py_Yapex)
    Py_INCREF(py_Yapex);
  Py_f2p = PythonLookup("f2p", NULL);
  if (!Py_f2p)
    Py_f2p = PyList_New(16);
for (i=0; i < 16; i++)
    PyList_SetItem(Py_f2p,i,PyDict_New());
  Py_INCREF(Py_f2p);
  init_python_vfs();
}

/*static void install_py_constants(void) {
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
  ATOM_term = PL_new_atom("term");
  FUNCTOR_abs1 = PL_new_functor(PL_new_atom("abs"), 1);
  FUNCTOR_all1 = PL_new_functor(PL_new_atom("all"), 1);
  FUNCTOR_any1 = PL_new_functor(PL_new_atom("any"), 1);
  FUNCTOR_as2 = PL_new_functor(PL_new_atom("as"), 2);
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
  FUNCTOR_var1 = PL_new_functor(PL_new_atom("$VAR"), 1);
}
*/

foreign_t end_python(void) {
  if (!python_in_python)
    Py_Finalize();

  return true;
}

static bool libpython_initialized = false;

X_API bool do_init_python(void) {
  //  char **argv;
  if ( libpython_initialized)
    return true;
  libpython_initialized = true;

  //  PyGILState_STATE gstate = PyGILState_Ensure();
  term_t t = PL_new_term_ref();
  if (!Py_IsInitialized())
    Py_Initialize();
  Yap_create_prolog_flag("python_export_string_as", true,  YAP_MkAtomTerm(YAP_LookupAtom ("term")),  YAP_MkAtomTerm(YAP_LookupAtom ("term")));
    Yap_set_flag(MkAtomTerm(Yap_LookupAtom("back_quotes")),MkAtomTerm(Yap_LookupAtom("string")));
    Yap_set_flag(MkAtomTerm(Yap_LookupAtom("single_quotes")),MkAtomTerm(Yap_LookupAtom("string")));
    Yap_set_flag(MkAtomTerm(Yap_LookupAtom("double_quotes")),MkAtomTerm(Yap_LookupAtom("string")));
  PL_reset_term_refs(t);
  install_pl2pl();
  // PyGILState_Release(gstate);
  add_modules();
  //    python_output();
  return true;
}

// @}
