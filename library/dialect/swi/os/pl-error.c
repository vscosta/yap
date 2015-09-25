
#include	<stdlib.h>
#include	<stdio.h>
#include "pl-incl.h"
#if HAVE_ERRNO_H
#include	<errno.h>
#endif

void			fatalError(const char *fm, ...) {exit(1);}
int			printMessage(atom_t severity, ...);

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


#undef PL_get_atom_ex

int
PL_get_atom_ex__LD(term_t t, atom_t *a ARG_LD)
{ if ( PL_get_atom(t, a) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, t);
}

int
PL_get_atom_ex(term_t t, atom_t *a)
{ GET_LD
  if ( PL_get_atom(t, a) )
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
   return PL_get_int64_ex(t, (int64_t *)i);
#else
   return PL_get_long_ex(t, (long*)i);
#endif
}

int
PL_get_pointer_ex(term_t t, void **i)
{
#if SIZEOF_LONG != SIZEOF_VOIDP && SIZEOF_VOIDP == 8
  return PL_get_int64_ex(t, (int64_t *)i);
#else
   return PL_get_long_ex(t, (long *)i);
#endif
}


int
PL_get_size_ex(term_t t, size_t *i)
{ int64_t val;

  if ( !PL_get_int64_ex(t, &val) )
    fail;
  if ( val < 0 )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
                    ATOM_not_less_than_zero, t);
#if SIZEOF_VOIDP < 8
#if SIZEOF_LONG == SIZEOF_VOIDP
  if ( val > (int64_t)ULONG_MAX )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_size_t);
#endif
#endif

  *i = (size_t)val;

  return TRUE;
}


int
PL_get_bool_ex(term_t t, int *ip)
{ if ( PL_get_bool(t, ip) )
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
PL_unify_bool_ex(term_t t, int val)
{ GET_LD
  int v;

  if ( PL_is_variable(t) )
    return PL_unify_atom(t, val ? ATOM_true : ATOM_false);
  if ( PL_get_bool(t, &v) )
  { if ( (!val && !v) || (val && v) )
      succeed;
    fail;
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_bool, t);
}

		 /*******************************
		 *	  TYPICAL ERRORS	*
		 *******************************/

int
PL_instantiation_error(term_t actual)
{ return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
}

int
PL_uninstantiation_error(term_t actual)
{ return PL_error(NULL, 0, NULL, ERR_UNINSTANTIATION, 0, actual);
}

int
PL_representation_error(const char *resource)
{ atom_t r = PL_new_atom(resource);
  int rc = PL_error(NULL, 0, NULL, ERR_RESOURCE, r);
  PL_unregister_atom(r);

  return rc;
}


int
PL_type_error(const char *expected, term_t actual)
{ return PL_error(NULL, 0, NULL, ERR_CHARS_TYPE, expected, actual);
}


int
PL_domain_error(const char *expected, term_t actual)
{ atom_t a = PL_new_atom(expected);
  int rc = PL_error(NULL, 0, NULL, ERR_DOMAIN, a, actual);
  PL_unregister_atom(a);

  return rc;
}


int
PL_existence_error(const char *type, term_t actual)
{ atom_t a = PL_new_atom(type);
  int rc = PL_error(NULL, 0, NULL, ERR_EXISTENCE, a, actual);
  PL_unregister_atom(a);

  return rc;
}


int
PL_permission_error(const char *op, const char *type, term_t obj)
{ atom_t t = PL_new_atom(type);
  atom_t o = PL_new_atom(op);
  int rc = PL_error(NULL, 0, NULL, ERR_PERMISSION, o, t, obj);

  PL_unregister_atom(t);
  PL_unregister_atom(o);

  return rc;
}


int
PL_resource_error(const char *resource)
{ atom_t r = PL_new_atom(resource);
  int rc = PL_error(NULL, 0, NULL, ERR_RESOURCE, r);

  PL_unregister_atom(r);

  return rc;
}

int
PL_no_memory(void)
{ return PL_error(NULL, 0, NULL, ERR_RESOURCE, ATOM_memory);
}



word
notImplemented(char *name, int arity)
{ return (word)PL_error(NULL, 0, NULL, ERR_NOT_IMPLEMENTED_PROC, name, arity);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
printMessage(atom_t severity, ...)

Calls print_message(severity, term), where  ...   are  arguments  as for
PL_unify_term(). This predicate saves possible   pending  exceptions and
restores them to make the call from B_THROW possible.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define OK_RECURSIVE 10

int
printMessage(atom_t severity, ...)
{ GET_LD
    //wakeup_state wstate;
  term_t av;
  predicate_t pred = RepPredProp(PredPropByFunc(FunctorPrintMessage,PROLOG_MODULE)); //PROCEDURE_print_message2;
  va_list args;
  int rc;

  if ( ++LD->in_print_message >= OK_RECURSIVE*3 )
    fatalError("printMessage(): recursive call\n");
  /*  if ( !saveWakeup(&wstate, TRUE PASS_LD) )
  { LD->in_print_message--;
    return FALSE;
  }
  */

  av = PL_new_term_refs(2);
  va_start(args, severity);
  PL_put_atom(av+0, severity);
  rc = PL_unify_termv(av+1, args);
  va_end(args);

  if ( rc )
    { if ( isDefinedProcedure(pred)  && LD->in_print_message <= OK_RECURSIVE )
    { rc = PL_call_predicate(NULL, PL_Q_NODEBUG|PL_Q_CATCH_EXCEPTION,
			     pred, av);
    } else if ( LD->in_print_message <= OK_RECURSIVE*2 )
    { Sfprintf(Serror, "Message: ");
      rc = PL_write_term(Serror, av+1, 1200, 0);
      Sfprintf(Serror, "\n");
    } else				/* in_print_message == 2 */
    { Sfprintf(Serror, "printMessage(): recursive call\n");
    }
  }

  /* restoreWakeup(&wstate PASS_LD); */
  LD->in_print_message--;

  return rc;
}


int PL_error(const char *pred, int arity, const char *msg, PL_error_code id, ...)
{
  GET_LD
  char msgbuf[50];
  term_t formal, swi, predterm, msgterm, except;
  va_list args;
  int rc = TRUE;

  formal    = PL_new_term_ref();
  swi    = PL_new_term_ref();
  predterm    = PL_new_term_ref();
  msgterm    = PL_new_term_ref();
  except    = PL_new_term_ref();

  if ( msg == ((char *)(-1)) )
    { if ( errno == EPLEXCEPTION )
	return FALSE;
      msg = OsError();
    }

  /* This would really require having pl-error.c, but we'll make do so as */
  va_start(args, id);
  switch(id) {
  case ERR_INSTANTIATION:
  err_instantiation:
    PL_unify_atom(formal, ATOM_instantiation_error);
    break;
  case ERR_UNINSTANTIATION:
    { int argn = va_arg(args, int);
      term_t bound = va_arg(args, term_t);

      if ( !msg && argn > 0 )
      { Ssprintf(msgbuf, "%d-%s argument",
		 argn, argn == 1 ? "st" : argn == 2 ? "nd" : "th");
	msg = msgbuf;
      }

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_uninstantiation_error1,
			   PL_TERM, bound);
      break;
    }
  case ERR_TYPE:			/* ERR_INSTANTIATION if var(actual) */
    { atom_t expected = va_arg(args, atom_t);
      term_t actual   = va_arg(args, term_t);

      if ( PL_is_variable(actual) && expected != ATOM_variable )
	goto err_instantiation;

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_type_error2,
		      PL_ATOM, expected,
		      PL_TERM, actual);
      break;
    }
  case ERR_DOMAIN:			/*  ERR_INSTANTIATION if var(arg) */
    { atom_t domain = va_arg(args, atom_t);
      term_t arg    = va_arg(args, term_t);

      if ( PL_is_variable(arg) )
	goto err_instantiation;

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_domain_error2,
		      PL_ATOM, domain,
		      PL_TERM, arg);
      break;
    }
  case ERR_REPRESENTATION:
    { atom_t what = va_arg(args, atom_t);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_representation_error1,
		      PL_ATOM, what);
      break;
    }
  case ERR_NOT_IMPLEMENTED_PROC:
    { const char *name = va_arg(args, const char *);
      int arity = va_arg(args, int);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_not_implemented2,
		    PL_ATOM, ATOM_procedure,
		    PL_FUNCTOR, FUNCTOR_divide2,
		    PL_CHARS, name,
		    PL_INT, arity);
      break;
    }
  case ERR_EXISTENCE:
    { atom_t type = va_arg(args, atom_t);
      term_t obj  = va_arg(args, term_t);

      PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_existence_error2,
			  PL_ATOM, type,
			  PL_TERM, obj);

      break;
    }
  case ERR_PERMISSION:
    { atom_t type = va_arg(args, atom_t);
      atom_t op   = va_arg(args, atom_t);
      term_t obj  = va_arg(args, term_t);

      PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_permission_error3,
			  PL_ATOM, type,
			  PL_ATOM, op,
			  PL_TERM, obj);

      break;
    }
  case ERR_SYSCALL:
    { const char *op = va_arg(args, const char *);

      if ( !msg )
	msg = op;

      switch(errno)
      { case ENOMEM:
	  PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_resource_error1,
			  PL_ATOM, ATOM_no_memory);
	  break;
	default:
	  PL_unify_atom(formal, ATOM_SYSTEM_ERROR_INTERNAL);
	  break;
      }

      break;
    }
  case ERR_TIMEOUT:
    { atom_t op   = va_arg(args, atom_t);
      term_t obj  = va_arg(args, term_t);

      PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_timeout_error2,
			  PL_ATOM, op,
			  PL_TERM, obj);

      break;
    }
    case ERR_FILE_OPERATION:
    { atom_t action = va_arg(args, atom_t);
      atom_t type   = va_arg(args, atom_t);
      term_t file   = va_arg(args, term_t);

      switch(errno)
      { case EACCES:
	  PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_permission_error3,
			  PL_ATOM, action,
			  PL_ATOM, type,
			  PL_TERM, file);
	  break;
	case EMFILE:
	case ENFILE:
	  PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_resource_error1,
			  PL_ATOM, ATOM_max_files);
	  break;
#ifdef EPIPE
	case EPIPE:
	  if ( !msg )
	    msg = "Broken pipe";
	  /*FALLTHROUGH*/
#endif
	default:			/* what about the other cases? */
	  PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_existence_error2,
			  PL_ATOM, type,
			  PL_TERM, file);
	  break;
      }

      break;
    }
  case ERR_NOMEM:
    { PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_resource_error1,
		      PL_ATOM, ATOM_no_memory);

      break;
    }
  case ERR_EVALUATION:
    { atom_t what = va_arg(args, atom_t);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_evaluation_error1,
		      PL_ATOM, what);
      break;
    }
    case ERR_STREAM_OP:
    { atom_t action = va_arg(args, atom_t);
      term_t stream = va_arg(args, term_t);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_io_error2,
			   PL_ATOM, action,
			   PL_TERM, stream);
      break;
    }
    case ERR_FORMAT:
    { const char *s = va_arg(args, const char*);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR_CHARS, "format", 1,
			   PL_CHARS, s);
      break;
    }
    case ERR_FORMAT_ARG:
    { const char *s = va_arg(args, const char*);
      term_t arg = va_arg(args, term_t);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR_CHARS, "format_argument_type", 2,
			   PL_CHARS, s,
			   PL_TERM, arg);
      break;
    }

  default:
    fprintf(stderr, "unimplemented SWI error %d\n",id);
    goto err_instantiation;
  }
  va_end(args);
  if (!pred) {
    pred = Yap_GetCurrentPredName();
    arity = Yap_GetCurrentPredArity();
  }
  if ( pred )
    { PL_unify_term(predterm,
		    PL_FUNCTOR, FUNCTOR_divide2,
		    PL_CHARS, pred,
		    PL_INT, arity);
    }
  if (!rc) {
    fatalError("Cannot report error: no memory");
  }
  if ( msg )
    {
      rc = PL_put_atom_chars(msgterm, msg);
    }
  rc = PL_unify_term(swi,
		PL_FUNCTOR, FUNCTOR_context2,
		PL_TERM, predterm,
		PL_TERM, msgterm);
  rc = PL_unify_term(except,
		PL_FUNCTOR, FUNCTOR_error2,
		PL_TERM, formal,
		PL_TERM, swi);
  rc = PL_raise_exception(except);
  return rc;
}

