
/* YAP support for some low-level SWI stuff */

#include <stdio.h>
#include "pl-incl.h"
#if HAVE_MATH_H
#include <math.h>
#endif

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
  { char tmp[2];

    tmp[0] = chrcode;
    tmp[1] = '\0';
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
saveWakeup(wakeup_state *state, int forceframe ARG_LD)
{
  return 0;
}

void
restoreWakeup(wakeup_state *state ARG_LD)
{
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
  /* dirty trick to ensure s is null terminated */
  char *st = (char *)s;
  st[len] = '\0';
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
    YAP_BigNumOfTerm(t, &n->value.mpz);
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

intptr_t
lengthList(term_t list, int errors)
{ GET_LD
  intptr_t length = 0;
  Word l = YAP_AddressFromSlot(list);
  Word tail;

  length = skip_list(l, &tail PASS_LD);

  if ( isNil(*tail) )
    return length;

  if ( errors )
    PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, wordToTermRef(l));

  return isVar(*tail) ? -2 : -1;
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

void
outOfCore()
{ fprintf(stderr,"Could not allocate memory: %s", OsError());
  exit(1);
}

int
priorityOperator(Module m, atom_t atom)
{
  YAP_Term mod = (YAP_Term)m;
  if (!m) 
    mod = YAP_CurrentModule();
  return YAP_MaxOpPriority(YAP_AtomFromSWIAtom(atom), mod);
}

int
currentOperator(Module m, atom_t name, int kind, int *type, int *priority)
{
  YAP_Term mod = (YAP_Term)m;
  YAP_Atom at;
  int opkind, yap_type;

  if (!m) 
    mod = YAP_CurrentModule();
  at = YAP_AtomFromSWIAtom(name);
  switch (kind) {
  case OP_PREFIX:
    opkind = 2;
    break;
  case OP_INFIX:
    opkind = 0;
    break;
  case OP_POSTFIX:
  default:
    opkind = 1;
  }
  if (!YAP_OpInfo(YAP_AtomFromSWIAtom(name), mod, opkind, &yap_type, priority))
    return FALSE;
  switch(yap_type) {
  case 1:
    *type = OP_XFX;
    break;
  case 2:
    *type = OP_XFY;
    break;
  case 3:
    *type = OP_YFX;
    break;
  case 4:
    *type = OP_XFX;
    break;
  case 5:
    *type = OP_XF;
    break;
  case 6:
    *type = OP_YF;
    break;
  case 7:
    *type = OP_FX;
    break;    
  default:
    *type = OP_FY;
    break;
  }
  return 1;
}

int
numberVars(term_t t, nv_options *opts, int n ARG_LD) {
  return 0;
}

		 /*******************************
		 *	     PROMOTION		*
		 *******************************/

static int
check_float(double f)
{
#ifdef HAVE_FPCLASSIFY
  switch(fpclassify(f))
  { case FP_NAN:
      return PL_error(NULL, 0, NULL, ERR_AR_UNDEF);
      break;
    case FP_INFINITE:
      return PL_error(NULL, 0, NULL, ERR_AR_OVERFLOW);
      break;
  }
#else
#ifdef HAVE_FPCLASS
  switch(fpclass(f))
  { case FP_SNAN:
    case FP_QNAN:
      return PL_error(NULL, 0, NULL, ERR_AR_UNDEF);
      break;
    case FP_NINF:
    case FP_PINF:
      return PL_error(NULL, 0, NULL, ERR_AR_OVERFLOW);
      break;
    case FP_NDENORM:			/* pos/neg denormalized non-zero */
    case FP_PDENORM:
    case FP_NNORM:			/* pos/neg normalized non-zero */
    case FP_PNORM:
    case FP_NZERO:			/* pos/neg zero */
    case FP_PZERO:
      break;
  }
#else
#ifdef HAVE__FPCLASS
  switch(_fpclass(f))
  { case _FPCLASS_SNAN:
    case _FPCLASS_QNAN:
      return PL_error(NULL, 0, NULL, ERR_AR_UNDEF);
      break;
    case _FPCLASS_NINF:
    case _FPCLASS_PINF:
      return PL_error(NULL, 0, NULL, ERR_AR_OVERFLOW);
      break;
  }
#else
#ifdef HAVE_ISNAN
  if ( isnan(f) )
    return PL_error(NULL, 0, NULL, ERR_AR_UNDEF);
#endif
#ifdef HAVE_ISINF
  if ( isinf(f) )
    return PL_error(NULL, 0, NULL, ERR_AR_OVERFLOW);
#endif
#endif /*HAVE__FPCLASS*/
#endif /*HAVE_FPCLASS*/
#endif /*HAVE_FPCLASSIFY*/
  return TRUE;
}


int
promoteToFloatNumber(Number n)
{ switch(n->type)
  { case V_INTEGER:
      n->value.f = (double)n->value.i;
      n->type = V_FLOAT;
      break;
#ifdef O_GMP
    case V_MPZ:
    { double val = mpz_get_d(n->value.mpz);

      if ( !check_float(val) )
	return FALSE;

      clearNumber(n);
      n->value.f = val;
      n->type = V_FLOAT;
      break;
    }
    case V_MPQ:
    { double val = mpq_get_d(n->value.mpq);

      if ( !check_float(val) )
	return FALSE;

      clearNumber(n);
      n->value.f = val;
      n->type = V_FLOAT;
      break;
    }
#endif
    case V_FLOAT:
      break;
  }

  return TRUE;
}




X_API int
PL_ttymode(IOSTREAM *s)
{ GET_LD

  if ( s == Suser_input )
  { if ( !truePrologFlag(PLFLAG_TTY_CONTROL) ) /* -tty in effect */
      return PL_NOTTY;
    if ( ttymode == TTY_RAW )		/* get_single_char/1 and friends */
      return PL_RAWTTY;
    return PL_COOKEDTTY;		/* cooked (readline) input */
  } else
    return PL_NOTTY;
}

X_API void
PL_prompt_next(int fd)
{ GET_LD

  if ( fd == 0 )
    LD->prompt.next = TRUE;
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

#ifdef _WIN32

#include <windows.h>

#if O_PLMT
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_w32thread_raise(DWORD id, int sig)
    Sets the signalled mask for a specific Win32 thread. This is a
    partial work-around for the lack of proper asynchronous signal
    handling in the Win32 platform.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int thread_highest_id = 1;

X_API int
PL_w32thread_raise(DWORD id, int sig)
{ int i;

  if ( sig < 0 || sig > MAXSIGNAL )
    return FALSE;			/* illegal signal */

  LOCK();
  for(i = 1; i <= thread_highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];

    if ( info && info->w32id == id && info->thread_data )
    { raiseSignal(info->thread_data, sig);
      if ( info->w32id )
	PostThreadMessage(info->w32id, WM_SIGNALLED, 0, 0L);
      UNLOCK();
      DEBUG(1, Sdprintf("Signalled %d to thread %d\n", sig, i));
      return TRUE;
    }
  }
  UNLOCK();

  return FALSE;				/* can't find thread */
}

#else

int
PL_w32thread_raise(DWORD id, int sig)
{ return PL_raise(sig);
}

#endif
#endif /*__WINDOWS__*/


X_API int
PL_raise(int sig)
{ GET_LD

    if (sig == SIG_PLABORT) {
      YAP_signal(0x40); /* YAP_INT_SIGNAL */
      return 1;
    } else {
      return 0;
    }
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
