#include "pl-incl.h"

void			outOfCore(void) {}
void			fatalError(const char *fm, ...) {exit(1);}
void			printMessage(int type, ...) {}

		 /*******************************
		 *    ERROR-CHECKING *_get()	*
		 *******************************/

int
PL_get_nchars_ex(term_t t, size_t *len, char **s, unsigned int flags)
{ return PL_get_nchars(t, len, s, flags|CVT_EXCEPTION);
}


int
PL_get_chars_ex(term_t t, char **s, unsigned int flags)
{ return PL_get_nchars(t, NULL, s, flags|CVT_EXCEPTION);
}


int
PL_get_atom_ex(term_t t, atom_t *a)
{ if ( PL_get_atom(t, a) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, t);
}


int
PL_get_integer_ex(term_t t, int *i)
{ if ( PL_get_integer(t, i) )
    succeed;

  if ( PL_is_integer(t) )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_int);

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}


int
PL_get_long_ex(term_t t, long *i)
{ if ( PL_get_long(t, i) )
    succeed;

  if ( PL_is_integer(t) )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_long);

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}


int
PL_get_int64_ex(term_t t, int64_t *i)
{ if ( PL_get_int64(t, i) )
    succeed;

  if ( PL_is_integer(t) )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_int64_t);

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}


int
PL_get_intptr_ex(term_t t, intptr_t *i)
{
#if SIZEOF_LONG != SIZEOF_VOIDP && SIZEOF_VOIDP == 8
   return PL_get_int64_ex(t, i);
#else
   return PL_get_long_ex(t, (long*)i);
#endif
}


int
PL_get_bool_ex(term_t t, int *i)
{ if ( PL_get_bool(t, i) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_bool, t);
}


int
PL_get_float_ex(term_t t, double *f)
{ if ( PL_get_float(t, f) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_float, t);
}


int
PL_get_char_ex(term_t t, int *p, int eof)
{ if ( PL_get_char(t, p, eof) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_character, t);
}


int
PL_unify_list_ex(term_t l, term_t h, term_t t)
{ if ( PL_unify_list(l, h, t) )
    succeed;

  if ( PL_get_nil(l) )
    fail;
  
  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, l);
}


int
PL_unify_nil_ex(term_t l)
{ if ( PL_unify_nil(l) )
    succeed;

  if ( PL_is_list(l) )
    fail;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, l);
}


int
PL_get_list_ex(term_t l, term_t h, term_t t)
{ if ( PL_get_list(l, h, t) )
    succeed;

  if ( PL_get_nil(l) )
    fail;
  
  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, l);
}

int
PL_get_nil_ex(term_t l)
{ if ( PL_get_nil(l) )
    succeed;

  if ( PL_is_list(l) )
    fail;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, l);
}


int
PL_get_module_ex(term_t name, module_t *m)
{ if ( !PL_get_module(name, m) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, name);

  succeed;
}

int
PL_unify_bool_ex(term_t t, bool val)
{ bool v;

  if ( PL_is_variable(t) )
    return PL_unify_atom(t, val ? ATOM_true : ATOM_false);
  if ( PL_get_bool(t, &v) )
  { if ( (!val && !v) || (val && v) )
      succeed;
    fail;
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_bool, t);
}

word
notImplemented(char *name, int arity)
{ return (word)PL_error(NULL, 0, NULL, ERR_NOT_IMPLEMENTED_PROC, name, arity);
}

