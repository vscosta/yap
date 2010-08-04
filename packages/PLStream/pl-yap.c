
/* YAP support for some low-level SWI stuff */

#include <stdio.h>
#include "pl-incl.h"

#define	Quote_illegal_f		1
#define	Ignore_ops_f		2
#define	Handle_vars_f		4
#define	Use_portray_f		8
#define	To_heap_f	       16
#define	Unfold_cyclics_f       32

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

int fileerrors;

PL_local_data_t lds;

gds_t gds;

static atom_t
uncachedCodeToAtom(int chrcode)
{ if ( chrcode < 256 )
  { char tmp[1];

    tmp[0] = chrcode;
    return lookupAtom(tmp, 1);
  } else
  { pl_wchar_t tmp[2];

    tmp[0] = chrcode;
    tmp[1] = '\0';

    return (atom_t)YAP_LookupWideAtom(tmp);
  }
}


atom_t
codeToAtom(int chrcode)
{ atom_t a;

  if ( chrcode == EOF )
    return ATOM_end_of_file;

  assert(chrcode >= 0);

  if ( chrcode < (1<<15) )
  { int page  = chrcode / 256;
    int entry = chrcode % 256;
    atom_t *pv;

    if ( !(pv=GD->atoms.for_code[page]) )
    { pv = PL_malloc(256*sizeof(atom_t));
      
      memset(pv, 0, 256*sizeof(atom_t));
      GD->atoms.for_code[page] = pv;
    }

    if ( !(a=pv[entry]) )
    { a = pv[entry] = uncachedCodeToAtom(chrcode);
    }
  } else
  { a = uncachedCodeToAtom(chrcode);
  }
  
  return a;
}

word
globalString(size_t size, char *s)
{
  // return YAP_MkBlobStringTerm(s, size);
  return 0L;
}

word
globalWString(size_t size, wchar_t *s)
{
  // return YAP_MkBlobWideStringTerm(size, s);
  return 0L;
}

int
PL_rethrow(void)
{ GET_LD

  if ( LD->exception.throw_environment )
    longjmp(LD->exception.throw_environment->exception_jmp_env, 1);

  fail;
}


int
callProlog(module_t module, term_t goal, int flags, term_t *ex)
{ term_t g = PL_new_term_ref();
  functor_t fd;
  predicate_t proc;

  if ( ex )
    *ex = 0;

  PL_strip_module(goal, &module, g);
  if ( !PL_get_functor(g, &fd) )
  { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, goal);
    if ( ex )
      *ex = exception_term;

    fail;
  }
  
  proc = PL_pred(fd, module);
  
  { int arity = arityFunctor(fd);
    term_t args = PL_new_term_refs(arity);
    qid_t qid;
    int n, rval;

    for(n=0; n<arity; n++)
      _PL_get_arg(n+1, g, args+n);

    qid  = PL_open_query(module, flags, proc, args);
    rval = PL_next_solution(qid);
    if ( !rval && ex )
      *ex = PL_exception(qid);
    PL_cut_query(qid);

    return rval;
  }
}

extern X_API int PL_write_term(IOSTREAM *s, term_t term, int precedence, int flags);

X_API int 
PL_write_term(IOSTREAM *s, term_t term, int precedence, int flags)
{
  int nflags = 0;
  if (flags & PL_WRT_QUOTED)
    nflags |= Quote_illegal_f;
  if (flags & PL_WRT_IGNOREOPS)
    nflags |= Ignore_ops_f;
  if (flags & PL_WRT_NUMBERVARS)
    nflags |= Handle_vars_f;
  if (flags & PL_WRT_PORTRAY)
    nflags |= Use_portray_f;
  /* ignore other flags for now */
  YAP_Write(YAP_GetFromSlot(term), (void (*)(int))Sputc, flags);
  return TRUE;
}

int
writeAtomToStream(IOSTREAM *so, atom_t at)
{

  YAP_Write(YAP_MkAtomTerm((YAP_Atom)at), (void (*)(int))Sputc, 0);
  return TRUE;
}

int
valueExpression(term_t t, Number r ARG_LD)
{ //return YAP__expression(t, r, 0 PASS_LD);
  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
toIntegerNumber(Number n, int flags)

Convert a number to an integer. Default,   only rationals that happen to
be integer are converted. If   TOINT_CONVERT_FLOAT  is present, floating
point  numbers  are  converted  if  they  represent  integers.  If  also
TOINT_TRUNCATE is provided non-integer floats are truncated to integers.

Note that if a double is  out  of   range  for  int64_t,  it never has a
fractional part.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
toIntegerNumber(Number n, int flags)
{ 
#if SWI_PROLOG
switch(n->type)
  { case V_INTEGER:
      succeed;
#ifdef O_GMP
    case V_MPZ:
      succeed;
    case V_MPQ:				/* never from stacks iff integer */
      if ( mpz_cmp_ui(mpq_denref(n->value.mpq), 1L) == 0 )
      { mpz_clear(mpq_denref(n->value.mpq));
	n->value.mpz[0] = mpq_numref(n->value.mpq)[0];
	n->type = V_MPZ;
	succeed;
      }
      fail;
#endif
    case V_REAL:
      if ( (flags & TOINT_CONVERT_FLOAT) )
      { if ( double_in_int64_range(n->value.f) )
	{ int64_t l = (int64_t)n->value.f;
	  
	  if ( (flags & TOINT_TRUNCATE) ||
	       (double)l == n->value.f )
	  { n->value.i = l;
	    n->type = V_INTEGER;
	    
	    return TRUE;
	  }
	  return FALSE;
#ifdef O_GMP
	} else
	{ mpz_init_set_d(n->value.mpz, n->value.f);
	  n->type = V_MPZ;
	  
	  return TRUE;
#endif
	}
      }
      return FALSE;
  }
#endif
  assert(0);
  fail;
} 


int
_PL_unify_atomic(term_t t, PL_atomic_t a)
{
  return PL_unify_atom(t, a);
}

word lookupAtom(const char *s, size_t len)
{
  if (len >= strlen(s)) {
    return (word)YAP_LookupAtom(s);
  } else {
    char * buf = PL_malloc(len+1);
    word out;

    if (!buf)
      return 0;
    strncpy(buf,s,len);
    out = (word)YAP_LookupAtom(buf);
    PL_free(buf);
    return out;
  }
}

atom_t lookupUCSAtom(const pl_wchar_t *s, size_t len)
{
  if (len >= wcslen(s)) {
    return (atom_t)YAP_LookupWideAtom(s);
  } else {
    pl_wchar_t * buf = PL_malloc((len+1)*sizeof(pl_wchar_t));
    word out;

    if (!buf)
      return 0;
    wcsncpy(buf,s,len);
    out = (word)YAP_LookupWideAtom(buf);
    PL_free(buf);
    return out;
  }
}



		 /*******************************
		 *	       OPTIONS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable argument list:

	atom_t	name
	int	type	OPT_ATOM, OPT_STRING, OPT_BOOL, OPT_INT, OPT_LONG
	pointer	value
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAXOPTIONS 32

typedef union
{ bool *b;				/* boolean value */
  long *l;				/* long value */
  int  *i;				/* integer value */
  char **s;				/* string value */
  word *a;				/* atom value */
  term_t *t;				/* term-reference */
  void *ptr;				/* anonymous pointer */
} optvalue;


bool
scan_options(term_t options, int flags, atom_t optype,
	     const opt_spec *specs, ...)
{ va_list args;
  const opt_spec *s;
  optvalue values[MAXOPTIONS];
  term_t list = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t tmp  = PL_new_term_ref();
  term_t val  = PL_new_term_ref();
  int n;

  if ( truePrologFlag(PLFLAG_ISO) )
    flags |= OPT_ALL;

  va_start(args, specs);
  for( n=0, s = specs; s->name; s++, n++ )
    values[n].ptr = va_arg(args, void *);
  va_end(args);

  while ( PL_get_list(list, head, list) )
  { atom_t name;
    int arity;
    
    if ( PL_get_name_arity(head, &name, &arity) )
    { if ( name == ATOM_equals && arity == 2 )
      { PL_get_arg(1, head, tmp);

	if ( !PL_get_atom(tmp, &name) )
	  goto itemerror;
	PL_get_arg(2, head, val);
      } else if ( arity == 1 )
      { PL_get_arg(1, head, val);
      } else if ( arity == 0 )
	PL_put_atom(val, ATOM_true);
    } else if ( PL_is_variable(head) )
    { return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
    } else
    { itemerror:
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, optype, head);
    }

    for( n=0, s = specs; s->name; n++, s++ )
      { if ( s->name == name )
      { switch((s->type & OPT_TYPE_MASK))
	{ case OPT_BOOL:
	  { atom_t aval;

	    if ( !PL_get_atom(val, &aval) )
	      fail;
	    if ( aval == ATOM_true || aval == ATOM_on )
	      *values[n].b = TRUE;
	    else if ( aval == ATOM_false || aval == ATOM_off )
	      *values[n].b = FALSE;
	    else
	      goto itemerror;
	    break;
	  }
	  case OPT_INT:
	  { if ( !PL_get_integer(val, values[n].i) )
	      goto itemerror;

	    break;
	  }
	  case OPT_LONG:
	  { if ( !PL_get_long(val, values[n].l) )
	    { if ( (s->type & OPT_INF) && PL_is_inf(val) )
		*values[n].l = LONG_MAX;
	      else
		goto itemerror;
	    }

	    break;
	  }
	  case OPT_NATLONG:
	  { if ( !PL_get_long(val, values[n].l) )
	      goto itemerror;
	    if ( *(values[n].l) <= 0 )
	      return PL_error(NULL, 0, NULL, ERR_DOMAIN,
			      ATOM_not_less_than_one, val);

	    break;
	  }
	  case OPT_STRING:
	  { char *str;

	    if ( !PL_get_chars(val, &str, CVT_ALL) ) /* copy? */
	      goto itemerror;
	    *values[n].s = str;
	    break;
	  }
	  case OPT_ATOM:
	  { atom_t a;

	    if ( !PL_get_atom(val, &a) )
	      goto itemerror;
	    *values[n].a = a;
	    break;
	  }
	  case OPT_TERM:
	  { *values[n].t = val;
	    val = PL_new_term_ref();	/* can't reuse anymore */
	    break;
	  }
	  default:
	    assert(0);
	    fail;
	}
	break;
      }
    }
    
    if ( !s->name && (flags & OPT_ALL) )
      goto itemerror;
  }

  if ( !PL_get_nil(list) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, list);
  
  succeed;
}


int
get_atom_ptr_text(Atom a, PL_chars_t *text)
{ if (YAP_IsWideAtom(a))
    { pl_wchar_t *name = (pl_wchar_t *)YAP_WideAtomName(a);
      text->text.w   = name;
      text->length   = wcslen(name);
      text->encoding = ENC_WCHAR;
  } else
    { char *name = (char *)YAP_AtomName(a);
    text->text.t   = name;
    text->length   = strlen(name);
    text->encoding = ENC_ISO_LATIN_1;
  }
  text->storage   = PL_CHARS_HEAP;
  text->canonical = TRUE;

  succeed;
}


int
get_atom_text(atom_t atom, PL_chars_t *text)
{ Atom a = atomValue(atom);

  return get_atom_ptr_text(a, text);
}

int
get_string_text(word w, PL_chars_t *text ARG_LD)
{ fail;
}

void
PL_get_number(term_t l, number *n) {
  YAP_Term t = valHandle(l);
  if (YAP_IsIntTerm(t)) {
    n->type = V_INTEGER;
    n->value.i = YAP_IntOfTerm(t);
#ifdef O_GMP
  } else {
    n->type = V_MPZ;
    n->value.mpz = YAP_BigNumOfTerm(t);
#endif
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Formatting a float. This is very  complicated   as  we must write floats
such that it can be read as a float. This means using the conventions of
the C locale and if the float happens to be integer as <int>.0.

Switching the locale is no option as  locale handling is not thread-safe
and may have unwanted  consequences  for   embedding.  There  is  a intptr_t
discussion on the very same topic on  the Python mailinglist. Many hacks
are proposed, none is very satisfactory.   Richard  O'Keefe suggested to
use ecvt(), fcvt() and gcvt(). These  are   not  thread-safe.  The GNU C
library provides *_r() variations that  can   do  the  trick. An earlier
patch used localeconv() to find the  decimal   point,  but  this is both
complicated and not thread-safe.

Finally, with help of Richard we decided  to replace the first character
that is not a digit nor [eE], as this must be the decimal point. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define isDigit(c)	((c) >= '0' && (c) <= '9')

char *
format_float(double f, char *buf, const char *format)
{ char *q;

  sprintf(buf, format, f);

  q = buf;
  if ( *q == '-' )			/* skip -?[0-9]* */
    q++;
  while(*q && (isDigit(*q) || *q <= ' '))
    q++;

  switch( *q )
  { case '\0':
      *q++ = '.';
      *q++ = '0';
      *q = EOS;
      break;
    case 'e':
    case 'E':
      break;
    default:
      *q = '.';
  }

  return buf;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
codes_or_chars_to_buffer(term_t l, unsigned flags, int wide)

If l represents a list of codes   or characters, return a buffer holding
the characters. If wide == TRUE  the   buffer  contains  objects of type
pl_wchar_t. Otherwise it contains traditional characters.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
charCode(YAP_Term w)
{ if ( YAP_IsAtomTerm(w) )
    { 
      Atom a = atomValue(w);

      if ( YAP_AtomNameLength(a) == 1) {
	if (YAP_IsWideAtom(a)) {
	  return YAP_WideAtomName(a)[0];
	}
	return YAP_AtomName(a)[0];
      }
    }
  return -1;
}



Buffer
codes_or_chars_to_buffer(term_t l, unsigned int flags, int wide)
{ GET_LD
  Buffer b;
  YAP_Term list = YAP_GetFromSlot(l);
  YAP_Term arg;
  enum { CHARS, CODES } type;

  if ( YAP_IsPairTerm(list) )
  { arg = YAP_HeadOfTerm(list);
    if ( YAP_IsIntTerm(arg) )
      { long int i = YAP_IntOfTerm(arg);
	if ( i >= 0 && (wide || i < 256) )
	  { type = CODES;
	    goto ok;
	  }
      } else if ( charCode(arg) >= 0 )
      { type = CHARS;
	goto ok;
      }
  } else if ( list != YAP_TermNil() )
  { return findBuffer(flags);
  }

  fail;

ok:
  b = findBuffer(flags);

  while( YAP_IsPairTerm(list) )
  { intptr_t c = -1;

    arg = YAP_HeadOfTerm(list);

    switch(type)
    { case CODES:
	if ( YAP_IsIntTerm(arg) )
	  { c = YAP_IntOfTerm(arg);
	}
        break;
      case CHARS:
	c = charCode(arg);
        break;
    }

    if ( c < 0 || (!wide && c > 0xff) )
    { unfindBuffer(flags);		/* TBD: check unicode range */
      return NULL;
    }

    if ( wide )
      addBuffer(b, (pl_wchar_t)c, pl_wchar_t);
    else
      addBuffer(b, (unsigned char)c, unsigned char);

    list = YAP_TailOfTerm(list);
  }
  if ( list != YAP_TermNil() )
  { unfindBuffer(flags);
    return NULL;
  }

  return b;
}


void
setPrologFlag(const char *name, int flags, ...)
{
}

void
PL_set_prolog_flag(const char *name, int flags, ...)
{
}

int
PL_unify_chars(term_t t, int flags, size_t len, const char *s)
{ PL_chars_t text;
  term_t tail;
  int rc;

  if ( len == (size_t)-1 )
    len = strlen(s);

  text.text.t    = (char *)s;
  text.encoding  = ((flags&REP_UTF8) ? ENC_UTF8 : \
		    (flags&REP_MB)   ? ENC_ANSI : ENC_ISO_LATIN_1);
  text.storage   = PL_CHARS_HEAP;
  text.length    = len;
  text.canonical = FALSE;

  flags &= ~(REP_UTF8|REP_MB|REP_ISO_LATIN_1);

  if ( (flags & PL_DIFF_LIST) )
  { tail = t+1;
    flags &= (~PL_DIFF_LIST);
  } else
  { tail = 0;
  }

  rc = PL_unify_text(t, tail, &text, flags);
  PL_free_text(&text);

  return rc;
}

X_API int PL_handle_signals(void)
{
  if ( !LD || LD->critical || !LD->signal.pending )
    return 0;
  fprintf(stderr,"PL_handle_signals not implemented\n");
  return 0;
}

/* just a stub for now */
int
warning(const char *fm, ...)
{  va_list args;

  va_start(args, fm);
  fprintf(stderr,"warning: %s\n", fm);
  va_end(args);

  return TRUE;
}

#if defined(HAVE_SELECT) && !defined(__WINDOWS__)

#ifdef __WINDOWS__
#include <winsock2.h>
#endif

static int
input_on_fd(int fd)
{ fd_set rfds;
  struct timeval tv;

  FD_ZERO(&rfds);
  FD_SET(fd, &rfds);
  tv.tv_sec = 0;
  tv.tv_usec = 0;

  return select(fd+1, &rfds, NULL, NULL, &tv) != 0;
}

#else
#define input_on_fd(fd) 1
#endif


X_API int
PL_dispatch(int fd, int wait)
{ if ( wait == PL_DISPATCH_INSTALLED )
    return GD->foreign.dispatch_events ? TRUE : FALSE;

  if ( GD->foreign.dispatch_events && PL_thread_self() == 1 )
  { if ( wait == PL_DISPATCH_WAIT )
    { while( !input_on_fd(fd) )
      { if ( PL_handle_signals() < 0 )
	  return FALSE;
	(*GD->foreign.dispatch_events)(fd);
      }
    } else
    { (*GD->foreign.dispatch_events)(fd);
      if ( PL_handle_signals() < 0 )
	  return FALSE;
    }
  }

  return TRUE;
}

extern size_t PL_utf8_strlen(const char *s, size_t len);

X_API size_t
PL_utf8_strlen(const char *s, size_t len)
{ return utf8_strlen(s, len);
}

#define COUNT_MUTEX_INITIALIZER(name) \
 { PTHREAD_MUTEX_INITIALIZER, \
   name, \
   0L \
 }

#if THREADS
counting_mutex _PL_mutexes[] =
{ COUNT_MUTEX_INITIALIZER("L_MISC"),
  COUNT_MUTEX_INITIALIZER("L_ALLOC"),
  COUNT_MUTEX_INITIALIZER("L_ATOM"),
  COUNT_MUTEX_INITIALIZER("L_FLAG"),
  COUNT_MUTEX_INITIALIZER("L_FUNCTOR"),
  COUNT_MUTEX_INITIALIZER("L_RECORD"),
  COUNT_MUTEX_INITIALIZER("L_THREAD"),
  COUNT_MUTEX_INITIALIZER("L_PREDICATE"),
  COUNT_MUTEX_INITIALIZER("L_MODULE"),
  COUNT_MUTEX_INITIALIZER("L_TABLE"),
  COUNT_MUTEX_INITIALIZER("L_BREAK"),
  COUNT_MUTEX_INITIALIZER("L_FILE"),
  COUNT_MUTEX_INITIALIZER("L_PLFLAG"),
  COUNT_MUTEX_INITIALIZER("L_OP"),
  COUNT_MUTEX_INITIALIZER("L_INIT"),
  COUNT_MUTEX_INITIALIZER("L_TERM"),
  COUNT_MUTEX_INITIALIZER("L_GC"),
  COUNT_MUTEX_INITIALIZER("L_AGC"),
  COUNT_MUTEX_INITIALIZER("L_FOREIGN"),
  COUNT_MUTEX_INITIALIZER("L_OS")
};

#endif
