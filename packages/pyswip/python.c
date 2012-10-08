#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <Python.h>
#include <assert.h>

static foreign_t
init_python(void)
{ 
  Py_Initialize();
  return TRUE;
}

static foreign_t
end_python(void)
{ 
  Py_Finalize();

  return TRUE;
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

install_t install_python(void);

install_t
install_python(void)
{ // FUNCTOR_dot2 = PL_new_functor(PL_new_atom("."), 2);
  // FUNCTOR_equal2 = PL_new_functor(PL_new_atom("="), 2);
  // FUNCTOR_boolop1 = PL_new_functor(PL_new_atom("@"), 1);
  // ATOM_true  = PL_new_atom("true");
  // ATOM_false = PL_new_atom("false");

  PL_register_foreign("init_python",	  0, init_python,      0);
  PL_register_foreign("end_python",	  0, end_python,       0);
  PL_register_foreign("python_run_command",	  1, python_run_command,       0);
}
