/**
 * @file python.c
 *
 * @brief data structures and init for Py4YAP library
 *
 */

/**
 * @addtogroup Py4YAP
 * @ingroup Python
 * @brief make Python talk to YAP
 * @{
 */

//#include "Atoms.h"
#include "py4yap.h"


#include <VFS.h>

//#define USES_REGS

#include "YapStreams.h"

X_API PyObject *py_Atoms;
X_API PyObject *Py_f2p;
X_API  PyObject *py_Ops;
X_API PyObject *py_Np;
X_API PyObject *py_Sys;
X_API PyObject * pYAPError;
//////X_API PyObject * py_Ops;
PyObject *py_Context;


pyst2yap_t pyStringToYAP = PYSTRING2STRING;

extern X_API bool python_in_python;

typedef struct {
  const char*op;
  int arity;
  const char *f;
} op2f_t;

static void add_modules(USES_REGS1) {

  //Term exp_string = MkAtomTerm(Yap_LookupAtom("python_export_string_as"));
  py_Atoms= PyDict_New();
  if (py_Atoms) {
    Py_INCREF(py_Atoms);
  }
  if ((py_Main = PyImport_ImportModule("__main__"))!=NULL) {
    Py_INCREF(py_Main);
  } else {
    py_Main = PyModule_New("__main__");
  }
  py_User = PyModule_New("user");
  
  py_Sys =  PyImport_ImportModule("sys");
  py_Np = PyImport_ImportModule("numpy");
  if (py_Np) {
    Py_INCREF(py_Np);
  } else {

  }
  py_Ops = PyImport_ImportModule("operator");
  if (py_Ops) {
    Py_INCREF(py_Ops);
  } else {
    Yap_ThrowError(DOMAIN_ERROR_MISSING_LIBRARY,MkStringTerm("operator"), " Missin Python operator library");
  }
  
  //  op = pyDict_GetItemString(py_Main, "__builtins__");
  PyObject *py_Yapex = PyImport_ImportModule("yap4py.yapi");
  if (py_Yapex) {
    Py_INCREF(py_Yapex);
    py_Context = py_Yapex;
  } else {
    py_Context = PyDict_New();
  }
  Py_f2p = PythonLookup("f2p",NULL, NULL);
  if (!Py_f2p)
    Py_f2p = PyList_New(16);
  Py_INCREF(Py_f2p);
 {
    int i;
    for (i=0; i < 16; i++)
      PyList_SetItem(Py_f2p,i,PyDict_New());
  }
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

static bool libpython_initialized = false;

X_API bool do_init_python(void) {
CACHE_REGS
  //  char **argv;
if ( libpython_initialized) {
  return true;
}
   libpython_initialized = true;
   term_t t = PL_new_term_ref();
   if (!Py_IsInitialized()) {
     GLOBAL_Embedded = true;
/*   PyInitConfig *config = PyInitConfig_Create(); */
   /*   if (config == NULL) { */
   /*     fprintf(stderr,"PYTHON INIT ERROR: memory allocation failed\n"); */
   /*     return -1; */
   /*   } */

   /*   // Enable the Python Development Mode */
   /*   if (PyInitConfig_SetInt(config, "dev_mode", 1) < 0) { */
   /*     goto error; */
   /*   } */
   /*   // Enable the Python Development Mode */
   /*   if (PyInitConfig_SetInt(config, "dev_mode", 1) < 0) { */
   /*     goto error; */
   /*   } */
   /*   if (PyInitConfig_SetInt(config, "install_signal_handlers", 0) < 0) { */
   /*     goto error; */
   /*   } */
   /*   // Initialize Python with the configuration */
   /*   if (Py_InitializeFromInitConfig(config) < 0) { */
   /*     goto error; */
   /*   } */
   /*   PyInitConfig_Free(config); */
   /*   return 0; */
   /* error: */
   /*   exit(1); */
   /* } else { */
//      Yap_CloseReadline();
    }
    //  PyGILState_Ensure();
   Py_Initialize();
  /*  const char *s = PyUnicode_AsUTF8(PyDict_GetItemString (PySys_GetObject("modules"),"sys")); */
  /*     char *ns =  */
  /* if (py_Sys) { */
  /*   const char *path = PyUnicode_AsUTF8(PyDict_GetItemString (py_sys,"path")), */
  /*   *cwd = get_current_directory(); */
  /*   char *np = mallo(cstrlen(path))+strlen(':')+strlen(cwd)+1; */
  /*   strcpy(np,cwd); */
  /*   strcat(np,":"); */
  /*   strcat(np,path); */
  /*   yDict_SetItemString (py_sys,"path",PyUnicode_FromString(np); */
  /* } */
  //  py_Main = PyDict_GetItemString (PySys_GetObject("modules"),"__maingre__");
  PyObject* py_Main = PyModule_GetDict(PyImport_AddModule("__main__"));
  PyObject  *builtins = PyEval_GetBuiltins(), *globals =PyDict_New();
  add_modules(PASS_REGS1);
 if (builtins)
   PyDict_SetItemString(globals, "__builtins__", builtins);
    PyDict_SetItemString(globals, "sys",(py_Sys));
#if 0
    PyCodeObject *py_code =PyCode_NewEmpty("x", "f", 1);
    PyThreadState * state =  PyThreadState_Get();
    /* PyFrameObject *frame = 0; */
    /* frame = PyFrame_New(state, */
    /* 			py_code,             /\*PyCodeObject *code,*\/ */
    /* 		     globals, */
    /* 		     /\*PyObject *globals,*\/ */
    /* 			   NULL                    /\*PyObject *locals*\/ */
    /* 		     ); */
 //   PyTraceBack_Here(frame);
   //b state->cframe = frame;
 // PyFunctionObject *func = PyFunction_New(py_code, globals);
 //\	_PyFrame_Push(state,func);
 //(void) _PyInterpreterState_SetEvalFrameFunc(PyThreadState_GetInterpreter(state),frame);
 ///_PyFrame_Push(frame);
 /* _PyEvalFramePushAndInit( state,func,
			  PyTuple_New(0),
			  PyTuple_New(0),
			  0,
			  PyTuple_New(0));
 */
 /* Populate the 'fast locals' in `frame` */
    // Py_XDECREF(frame->f_locals);
    //    frame->f_locals = locals;
    //     Py_XINCREF(frame->f_locals);
    //   PyFrame_LocalsToFast(frame, 0);
	//        state->frame = frame;
	// C_TRACE(result, fn(PyCFunction_GET_SELF(cfunc), args, kws));
        //state->frame = frame->f_back;
#endif
 


  Yap_create_prolog_flag("python_export_string_as", true,  YAP_MkAtomTerm(YAP_LookupAtom ("term")),  YAP_MkAtomTerm(YAP_LookupAtom ("term")));
      Yap_set_flag(MkAtomTerm(Yap_LookupAtom("back_quotes")),MkAtomTerm(Yap_LookupAtom("codes")));
    Yap_set_flag(MkAtomTerm(Yap_LookupAtom("double_quotes")),MkAtomTerm(Yap_LookupAtom("string")));
  PL_reset_term_refs(t);
//  python_output();
  return true;
}

// @}

