/* yap2swi.c  */
/*
 * Project: jpl for Yap Prolog
 * Author: Steve Moyle
 * Email:  steve.moyle@comlab.ox.ac.uk
 * Date:   21 January 2002

 * Copyright (c) 2002 Steve Moyle.  All rights reserved.

*/

//=== includes ===============================================================
#include	<stdlib.h>
#include	<string.h>
#include	<stdio.h>

#include	<yap2swi.h>

#define BUF_SIZE 256
#define TMP_BUF_SIZE 2*BUF_SIZE

char buffers[TMP_BUF_SIZE+BUF_SIZE*4];
static int buf_index = 0;

static char *
alloc_ring_buf(void)
{
  int ret = buf_index;
  buf_index++;
  if (buf_index == 4)
    buf_index = 0;
  return buffers+(TMP_BUF_SIZE+ret*BUF_SIZE);
}

/* SWI: void PL_agc_hook(void)
   YAP: NO EQUIVALENT */

/* dummy function for now (until Vitor comes through!)*/
X_API void PL_agc_hook(void)
{
}

/* SWI: char* PL_atom_chars(atom_t atom)
   YAP: char* AtomName(Atom) */
X_API char* PL_atom_chars(atom_t a)	 /* SAM check type */
{
  return YapAtomName(a);
}


/* SWI: term_t PL_copy_term_ref(term_t from)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API term_t PL_copy_term_ref(term_t from)
{
  return YapInitSlot(YapGetFromSlot(from));
}

X_API term_t PL_new_term_ref(void)
{
  
  term_t to = YapNewSlots(1);
  return to;
}

X_API term_t PL_new_term_refs(int n)
{
  
  term_t to = YapNewSlots(n);
  return to;
}

X_API void PL_reset_term_refs(term_t after)
{
  term_t new = YapNewSlots(1);
  YapRecoverSlots(after-new);
}

/* begin PL_get_* functions =============================*/

/* SWI: int PL_get_arg(int index, term_t t, term_t a)
   YAP: Term ArgOfTerm(int argno, Term t)*/
X_API int PL_get_arg(int index, term_t ts, term_t a)
{
  Term t = YapGetFromSlot(ts);
  if ( !IsApplTerm(t) ) {
    if (IsPairTerm(t)) {
      if (index == 1){
	YapPutInSlot(a,HeadOfTerm(t));
	return 1;
      } else if (index == 2) {
	YapPutInSlot(a,TailOfTerm(t));
	return 1;
      }
    }
    return 0;
  }
  YapPutInSlot(a,ArgOfTerm(index, t));
  return 1;
}
   
/* SWI: int PL_get_atom(term_t t, Atom *a)
   YAP: Atom AtomOfTerm(Term) */
X_API int PL_get_atom(term_t ts, atom_t *a)
{
  Term t = YapGetFromSlot(ts);
  if ( !IsAtomTerm(t))
    return 0;
  *a = YapAtomOfTerm(t);
  return 1;
}

/* SWI: int PL_get_atom_chars(term_t t, char **s)
   YAP: char* AtomName(Atom) */
X_API int PL_get_atom_chars(term_t ts, char **a)  /* SAM check type */
{
  Term t = YapGetFromSlot(ts);
  if (!IsAtomTerm(t))
    return 0;
  *a = YapAtomName(YapAtomOfTerm(t));
  return 1;
}

/*
  int PL_get_chars(term_t +t, char **s, unsigned flags) Convert the
  argument term t to a 0-terminated C-string. flags is a bitwise
  disjunction from two groups of constants. The first specifies which
  term-types should converted and the second how the argument is
  stored. Below is a specification of these constants. BUF_RING
  implies, if the data is not static (as from an atom), the data is
  copied to the next buffer from a ring of four (4) buffers. This is a
  convenient way of converting multiple arguments passed to a foreign
  predicate to C-strings. If BUF_MALLOC is used, the data must be
  freed using free() when not needed any longer.

    CVT_ATOM Convert if term is an atom
    CVT_STRING Convert if term is a string
    CVT_LIST Convert if term is a list of integers between 1 and 255
    CVT_INTEGER Convert if term is an integer (using %d)
    CVT_FLOAT Convert if term is a float (using %f)
    CVT_NUMBER Convert if term is a integer or float
    CVT_ATOMIC Convert if term is atomic
    CVT_VARIABLE Convert variable to print-name
    CVT_ALL Convert if term is any of the above, except for variables
    BUF_DISCARDABLE Data must copied immediately
    BUF_RING Data is stored in a ring of buffers
    BUF_MALLOC Data is copied to a new buffer returned by malloc(3)
*/

static int CvtToStringTerm(Term t, char *buf, char *buf_max)
{
  *buf++ = '\"';
  while (IsPairTerm(t)) {
    Term hd = YapHeadOfTerm(t);
    Int i;
    if (!IsIntTerm(hd))
      return 0;
    i = IntOfTerm(hd);
    if (i <= 0 || i >= 255)
      return 0;
    if (!IsIntTerm(hd))
      return 0;
    *buf++ = i;
    if (buf == buf_max)
      return 0;
    t = TailOfTerm(t);
  }
  if (t != MkAtomTerm(LookupAtom("[]")))
    return 0;
  if (buf+1 == buf_max)
    return 0;
  buf[0] = '\"';
  buf[1] = '\0';
  return 1;
}

char *bf, *bf_lim;

static void
buf_writer(int c)
{
  if (bf == bf_lim) {
    return;
  }
  *bf++ = c;
}

#if !HAVE_SNPRINTF
#define snprintf(X,Y,Z,A) sprintf(X,Z,A)
#endif


X_API int PL_get_chars(term_t l, char **sp, unsigned flags)
{
  Term t = YapGetFromSlot(l);
  char *tmp;

  if (!(flags & BUF_RING)) {
    tmp = alloc_ring_buf();
  } else {
    tmp = buffers;
  }
  *sp = tmp;
  if (YapIsAtomTerm(t)) {
    if (!(flags & (CVT_ATOM|CVT_ATOMIC|CVT_ALL)))
      return 0;
    *sp = YapAtomName(YapAtomOfTerm(t));
    return 1;
  } else if (YapIsIntTerm(t)) {
    if (!(flags & (CVT_INTEGER|CVT_NUMBER|CVT_ATOMIC|CVT_ALL)))
      return 0;
    snprintf(tmp,BUF_SIZE,"%ld",IntOfTerm(t));
  } else if (YapIsFloatTerm(t)) {
    if (!(flags & (CVT_FLOAT|CVT_ATOMIC|CVT_NUMBER|CVT_ALL)))
      return 0;
    snprintf(tmp,BUF_SIZE,"%f",FloatOfTerm(t));
  } else if (flags & CVT_STRING) {
    if (CvtToStringTerm(t,tmp,tmp+BUF_SIZE) == 0)
      return 0;
  } else {
    bf = tmp;
    bf_lim = tmp+(BUF_SIZE-1);
    YapWrite(t,buf_writer,0);
    if (bf == bf_lim)
      return 0;
    *bf = '\0';
  }
  if (flags & BUF_MALLOC) {
    char *nbf = malloc(strlen(tmp));
    if (nbf == NULL)
      return 0;
    strncpy(nbf,tmp,BUF_SIZE);
    *sp = nbf;
  }
  return 1;
}

/* SWI: int PL_get_functor(term_t t, functor_t *f)
   YAP: Functor FunctorOfTerm(Term) */
X_API int PL_get_functor(term_t ts, functor_t *f)
{
  Term t = YapGetFromSlot(ts);
  if ( IsAtomTerm(t)) {
    *f = t;
  } else {
    *f = (functor_t)FunctorOfTerm(t);
  }
  return 1;
}

/* SWI: int PL_get_float(term_t t, double *f)
   YAP: flt FloatOfTerm(Term) */
X_API int PL_get_float(term_t ts, double *f) /* SAM type check*/
{	
  Term t = YapGetFromSlot(ts);
  if ( !IsFloatTerm(t))
    return 0;
  *f = FloatOfTerm(t);
  return 1;
}

X_API int PL_get_head(term_t ts, term_t h)
{
  Term t = YapGetFromSlot(ts);
  if (!IsPairTerm(t) ) {
    return 0;
  }
  YapPutInSlot(h,HeadOfTerm(t));
  return 1;
}

/* SWI: int PL_get_integer(term_t t, int *i)
   YAP: Int IntOfTerm(Term) */
X_API int PL_get_integer(term_t ts, int *i)
{
  Term t = YapGetFromSlot(ts);
  if (!IsIntTerm(t) )
    return 0;
  *i = IntOfTerm(t);
  return 1;
}

X_API int PL_get_long(term_t ts, long *i)
{
  Term t = YapGetFromSlot(ts);
  if (!IsIntTerm(t) ) {
    if (IsFloatTerm(t)) {
      double dbl = YapFloatOfTerm(t);
      if (dbl - (long)dbl == 0.0) {
	*i = (long)dbl;
	return 1;
      }
    }
    return 0;
  }
  *i = IntOfTerm(t);
  return 1;
}

X_API int PL_get_list(term_t ts, term_t h, term_t tl)
{
  Term t = YapGetFromSlot(ts);
  if (!IsPairTerm(t) ) {
    return 0;
  }
  YapPutInSlot(h,HeadOfTerm(t));
  YapPutInSlot(tl,TailOfTerm(t));
  return 1;
}

X_API int PL_get_list_chars(term_t l, char **sp, unsigned flags)
{
  if (flags & (CVT_ATOM|CVT_STRING|CVT_INTEGER|CVT_FLOAT|CVT_NUMBER|CVT_ATOMIC|CVT_VARIABLE|CVT_ALL))
    return 0;
  return PL_get_chars(l, sp, CVT_LIST|flags);
}

/* SWI: int PL_get_module(term_t t, module_t *m) */
X_API int PL_get_module(term_t ts, module_t *m)
{
  Term t = YapGetFromSlot(ts);
  if (!YapIsAtomTerm(t) )
    return 0;
  *m = YapLookupModule(t);
  return 1;
}

/* SWI: int PL_get_atom(term_t t, Atom *a)
   YAP: Atom AtomOfTerm(Term) */
X_API int PL_get_name_arity(term_t ts, atom_t *name, int *arity)
{
  Term t = YapGetFromSlot(ts);
  if (YapIsAtomTerm(t)) {
    *name = YapAtomOfTerm(t);
    *arity = 0;
    return 1;
  }
  if (YapIsApplTerm(t)) {
    Functor f = YapFunctorOfTerm(t);
    *name = YapNameOfFunctor(f);
    *arity = YapArityOfFunctor(f);
    return 1;
  }
  if (YapIsPairTerm(t)) {
    *name = YapLookupAtom(".");
    *arity = 2;
    return 1;
  }
  return 0;
}

/* SWI: int PL_get_atom(term_t t, Atom *a)
   YAP: Atom AtomOfTerm(Term) */
X_API int PL_get_nil(term_t ts)
{
  Term t = YapGetFromSlot(ts);
  return ( t == YapMkAtomTerm(YapLookupAtom("[]")));
}

/* SWI: int PL_get_pointer(term_t t, int *i)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API int PL_get_pointer(term_t ts, void **i)
{
  Term t = YapGetFromSlot(ts);
  if (!IsIntTerm(t) )
    return 0;
  *i = (void *)YapIntOfTerm(t);
  return 1;
}

/* SWI: int PL_get_atom_chars(term_t t, char **s)
   YAP: char* AtomName(Atom) */
X_API int PL_get_string(term_t ts, char **sp, int *lenp)  /* SAM check type */
{
  Term t = YapGetFromSlot(ts);
  char *to;
  int len;
  if (!IsPairTerm(t))
    return 0;
  if (!YapStringToBuffer(t, buffers, TMP_BUF_SIZE))
      return(FALSE);
  len = strlen(buffers);
  to = (char *)YapNewSlots((len/sizeof(Term))+1);
  strncpy(to, buffers, TMP_BUF_SIZE);
  *sp = to;
  return 1;
}

X_API int PL_get_tail(term_t ts, term_t tl)
{
  Term t = YapGetFromSlot(ts);
  if (!IsPairTerm(t) ) {
    return 0;
  }
  YapPutInSlot(tl,TailOfTerm(t));
  return 1;
}

/* end PL_get_* functions  =============================*/

/* begin PL_new_* functions =============================*/

/* SWI: atom_t PL_new_atom(const char *)
   YAP: Atom LookupAtom(char *) */
/*  SAM should the following be used instead?
      Atom  FullLookupAtom(char *)
      */
X_API atom_t PL_new_atom(const char *c)
{
  return YapLookupAtom((char *)c);
}

X_API functor_t PL_new_functor(atom_t name, int arity)
{
  functor_t f;
  if (arity == 0) {
    f = (functor_t)YapMkAtomTerm(name);
  } else {
    f = (functor_t)YapMkFunctor(name,arity);
  }
  return f;
}

X_API atom_t PL_functor_name(functor_t f)
{
  if (IsAtomTerm(f)) {
    return AtomOfTerm(f);
  } else {
    return YapNameOfFunctor((Functor)f);
  }
}

X_API int PL_functor_arity(functor_t f)
{
  if (IsAtomTerm(f)) {
    return 0;
  } else {
    return YapArityOfFunctor((Functor)f);
  }
}

/* end PL_new_* functions =============================*/

/* begin PL_put_* functions =============================*/

X_API void PL_cons_functor(term_t d, functor_t f,...)
{
  va_list ap;
  int arity, i;
  Term *tmp = (CELL *)buffers;

  if (IsAtomTerm((Term)f)) {
    YapPutInSlot(d, (Term)f);
    return;
  }
  arity = ArityOfFunctor((Functor)f);
  if (arity > TMP_BUF_SIZE/sizeof(CELL)) {
    fprintf(stderr,"PL_cons_functor: arity too large (%d)\n", arity); 
    return;
  }
  va_start (ap, f);
  for (i = 0; i < arity; i++) {
    tmp[i] =  YapGetFromSlot(va_arg(ap, term_t));
  }
  va_end (ap);
  if (arity == 2 && (Functor)f == YapMkFunctor(YapLookupAtom("."),2))
    YapPutInSlot(d,YapMkPairTerm(tmp[0],tmp[1]));
  else
    YapPutInSlot(d,YapMkApplTerm((Functor)f,arity,tmp));
}

X_API void PL_cons_functor_v(term_t d, functor_t f,term_t a0)
{
  int arity;

  if (IsAtomTerm(f)) {
    YapPutInSlot(d,(Term)f);
    return;
  }
  arity = ArityOfFunctor((Functor)f);
  if (arity == 2 && (Functor)f == YapMkFunctor(YapLookupAtom("."),2))
    YapPutInSlot(d,YapMkPairTerm(YapGetFromSlot(a0),YapGetFromSlot(a0+1)));    
  else
    YapPutInSlot(d,YapMkApplTerm((Functor)f,arity,YapAddressFromSlot(a0)));
}

X_API void PL_cons_list(term_t d, term_t h, term_t t)
{
  YapPutInSlot(d,YapMkPairTerm(YapGetFromSlot(h),YapGetFromSlot(t)));
}

X_API void PL_put_atom(term_t t, atom_t a)
{
  YapPutInSlot(t,YapMkAtomTerm(a));
}

X_API void PL_put_atom_chars(term_t t, const char *s)
{
  YapPutInSlot(t,YapMkAtomTerm(YapLookupAtom((char *)s)));
}

X_API void PL_put_float(term_t t, double fl)
{
  YapPutInSlot(t,YapMkFloatTerm(fl));
}

X_API void PL_put_functor(term_t t, functor_t f)
{
  Int arity;
  if (IsAtomTerm(f)) {
    YapPutInSlot(t,f);
  } else {
    arity = YapArityOfFunctor((Functor)f);
    if (arity == 2 && (Functor)f == YapMkFunctor(YapLookupAtom("."),2))
      YapPutInSlot(t,YapMkNewPairTerm());    
    else
      YapPutInSlot(t,MkNewApplTerm((Functor)f,arity));
  }
}

X_API void PL_put_integer(term_t t, long n)
{
  YapPutInSlot(t,YapMkIntTerm(n));
}

X_API void PL_put_list(term_t t)
{
  YapPutInSlot(t,YapMkNewPairTerm());
}

X_API void PL_put_nil(term_t t)
{
  YapPutInSlot(t,MkAtomTerm(LookupAtom("[]")));
}

/* SWI: void PL_put_pointer(term_t -t, void *ptr)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API void PL_put_pointer(term_t t, void *ptr)
{
  Term tptr = MkIntTerm((Int)ptr);
  YapPutInSlot(t,tptr);
}

X_API void PL_put_string_chars(term_t t, const char *s)
{
  YapPutInSlot(t,YapBufferToString((char *)s));
}

X_API void PL_put_term(term_t d, term_t s)
{
  YapPutInSlot(d,YapGetFromSlot(s));
}

X_API void PL_put_variable(term_t t)
{
  YapPutInSlot(t,MkVarTerm());
}

/* end PL_put_* functions =============================*/

/* SWI: int PL_raise_exception(term_t exception)
   YAP: NO EQUIVALENT */
/* SAM TO DO */

X_API int PL_raise_exception(term_t exception)
{
  YapThrow(YapGetFromSlot(exception));
  return 0;
}

/* begin PL_unify_* functions =============================*/

X_API int PL_unify(term_t t1, term_t t2)
{
  return unify(YapGetFromSlot(t1),YapGetFromSlot(t2));
}

/* SWI: int PL_unify_atom(term_t ?t, atom  *at)
   YAP Int unify(Term* a, Term* b) */
X_API int PL_unify_atom(term_t t, atom_t at)
{
  Term cterm = MkAtomTerm(at);
  return unify(YapGetFromSlot(t),cterm);
}

/* SWI: int PL_unify_atom_chars(term_t ?t, const char *chars)
   YAP Int unify(Term* a, Term* b) */
X_API int PL_unify_atom_chars(term_t t, const char *s)
{
  Atom catom = YapLookupAtom((char *)s);
  Term cterm = MkAtomTerm(catom);
  return unify(YapGetFromSlot(t),cterm);
}

/* SWI: int PL_unify_float(term_t ?t, double f)
   YAP Int unify(Term* a, Term* b) */
X_API int PL_unify_float(term_t t, double f)
{
  Term fterm = MkFloatTerm(f);
  return unify(YapGetFromSlot(t),fterm);
}

/* SWI: int PL_unify_integer(term_t ?t, long n)
   YAP Int unify(Term* a, Term* b) */
X_API int PL_unify_integer(term_t t, long n)
{	
  Term iterm = MkIntTerm(n);
  return unify(YapGetFromSlot(t),iterm);
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP Int unify(Term* a, Term* b) */
X_API int PL_unify_list(term_t t, term_t h, term_t tail)
{
  Term pairterm = MkPairTerm(YapGetFromSlot(h),YapGetFromSlot(tail));
  return unify(YapGetFromSlot(t), pairterm);
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP Int unify(Term* a, Term* b) */
X_API int PL_unify_list_chars(term_t t, const char *chars)
{
  Term chterm = YapBufferToString((char *)chars);
  return unify(YapGetFromSlot(t), chterm);
}

/* SWI: int PL_unify_nil(term_t ?l)
   YAP Int unify(Term* a, Term* b) */
X_API int PL_unify_nil(term_t l)
{
  Term nilterm = MkAtomTerm(YapLookupAtom("[]"));
  return unify(YapGetFromSlot(l), nilterm);
}

/* SWI: int PL_unify_pointer(term_t ?t, void *ptr)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API int PL_unify_pointer(term_t t, void *ptr)
{
  Term ptrterm = MkIntTerm((Int)ptr);
  return unify(YapGetFromSlot(t), ptrterm);
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP Int unify(Term* a, Term* b) */
X_API int PL_unify_string_chars(term_t t, const char *chars)
{
  Term chterm = YapBufferToString((char *)chars);
  return unify(YapGetFromSlot(t), chterm);
}

typedef struct {
  int type;
  union {
    functor_t f;
    term_t t;
    atom_t a;
    long l;
    double dbl;
    char *s;
    void *p;
  } arg;
} arg_types;

static Term
get_term(arg_types **buf)
{
  arg_types *ptr = *buf;
  int type = ptr->type;
  Term t;

  switch (type) {
  /* now build the error string */
  case PL_VARIABLE:
    t = YapMkVarTerm();
    break;
  case PL_ATOM:
    t = YapMkAtomTerm(ptr->arg.a);
    break;
  case PL_INTEGER:
    t = YapMkIntTerm(ptr->arg.l);
    break;
  case PL_FLOAT:
    t = YapMkFloatTerm(ptr->arg.dbl);
    break;
  case PL_POINTER:
    t = YapMkIntTerm((Int)(ptr->arg.p));
    break;
  case PL_STRING:
    t = YapBufferToString(ptr->arg.s);
    break;
  case PL_TERM:
    t = YapGetFromSlot(ptr->arg.t);
    break;
  case PL_CHARS:
    t = MkAtomTerm(YapLookupAtom(ptr->arg.s));
    break;
  case PL_FUNCTOR:
    {
      functor_t f = ptr->arg.f;
      Int arity, i;
      term_t loc;

      if (IsAtomTerm((Term)f)) {
	t = (Term)f;
	break;
      }
      arity = YapArityOfFunctor((Functor)f);
      loc = YapNewSlots(arity);
      ptr++;
      for (i= 0; i < arity; i++) {
	YapPutInSlot(loc+i,get_term(&ptr));
      }
      t = MkApplTerm((Functor)f,arity,YapAddressFromSlot(loc));
    }
    break;
  case PL_LIST:
    {
      term_t loc;

      loc = YapNewSlots(2);
      YapPutInSlot(loc,get_term(&ptr));
      YapPutInSlot(loc+1,get_term(&ptr));
      t = MkPairTerm(YapGetFromSlot(loc),YapGetFromSlot(loc+1));
    }
    break;
  default:
    fprintf(stderr, "PL_FUNCTOR not implemented yet\n");
    exit(1);
  }
  ptr++;
  return t;
}

/* SWI: int PL_unify_term(term_t ?t1, term_t ?t2)
   YAP Int unify(Term* a, Term* b) */
X_API int PL_unify_term(term_t l,...)
{
  va_list ap;
  int type;
  int nels = 1;
  arg_types *ptr = (arg_types *)buffers;

  va_start (ap, l);
  while (nels > 0) {
    type = va_arg(ap, int);
    nels --;
    
    ptr->type = type;
    switch(type) {
    case PL_VARIABLE:
      break;
    case PL_ATOM:
      ptr->arg.a = va_arg(ap, atom_t);
      break;
    case PL_INTEGER:
      ptr->arg.l = va_arg(ap, long);
      break;
    case PL_FLOAT:
      ptr->arg.dbl = va_arg(ap, double);
      break;
    case PL_STRING:
      ptr->arg.s = va_arg(ap, char *);
      break;
    case PL_TERM:
      ptr->arg.t = va_arg(ap, term_t);
      break;
    case PL_POINTER:
      ptr->arg.p = va_arg(ap, void *);
      break;
    case PL_CHARS:
      ptr->arg.s = va_arg(ap, char *);
      break;
    case PL_FUNCTOR:
      {
	functor_t f = va_arg(ap, functor_t);
	ptr->arg.f = f;
	if (!IsAtomTerm((Term)f)) {
	  nels += YapArityOfFunctor((Functor)f);
	}
      }
      break;
    case PL_LIST:
      nels += 2;
      break;
    default:
      fprintf(stderr, "%d not supported\n", type);
      exit(1);
    }
    ptr++;
  }
  va_end (ap);
  ptr = (arg_types *)buffers;
  return unify(YapGetFromSlot(l),get_term(&ptr));
}

/* end PL_unify_* functions =============================*/

/* SWI: void PL_unregister_atom(atom_t atom)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API void PL_unregister_atom(atom_t atom)
{
}

X_API int PL_term_type(term_t t)
{
  /* Yap does not support strings as different objects */
  Term v = YapGetFromSlot(t);
  if (IsVarTerm(v)) {
    return PL_VARIABLE;
  } else if (IsAtomTerm(v)) {
    return PL_ATOM;
  } else if (IsIntTerm(v)) {
    return PL_INTEGER;
  } else if (IsFloatTerm(v)) {
    return PL_FLOAT;
  } else {
    return PL_TERM;
  }
}

X_API int PL_is_atom(term_t t)
{
  return IsAtomTerm(YapGetFromSlot(t));
}

X_API int PL_is_atomic(term_t ts)
{
  Term t = YapGetFromSlot(ts);
  return !IsVarTerm(t) || !IsApplTerm(t) || !IsPairTerm(t);
}

X_API int PL_is_compound(term_t ts)
{
  Term t = YapGetFromSlot(ts);
  return (IsApplTerm(t) || IsPairTerm(t));
}

X_API int PL_is_functor(term_t ts, functor_t f)
{
  Term t = YapGetFromSlot(ts);
  if (IsApplTerm(t)) {
    return FunctorOfTerm(t) == (Functor)f;
  } else if (IsPairTerm(t)) {
    return FunctorOfTerm(t) == YapMkFunctor(YapLookupAtom("."),2);
  } else
    return 0;
}

X_API int PL_is_float(term_t ts)
{
  Term t = YapGetFromSlot(ts);
  return IsFloatTerm(t);
}

X_API int PL_is_integer(term_t ts)
{
  Term t = YapGetFromSlot(ts);
  return IsIntTerm(t);
}

X_API int PL_is_list(term_t ts)
{
  Term t = YapGetFromSlot(ts);
  if (IsPairTerm(t)) {
    return 1;
  } else if (IsAtomTerm(t)) {
    return t == MkAtomTerm(YapLookupAtom("[]"));
  } else
    return 0;
}

X_API int PL_is_number(term_t ts)
{
  Term t = YapGetFromSlot(ts);
  return IsIntTerm(t) || IsFloatTerm(t);
}

X_API int PL_is_string(term_t ts)
{
  Term t = YapGetFromSlot(ts);
  while (IsPairTerm(t)) {
    Term hd = YapHeadOfTerm(t);
    Int i;
    if (!IsIntTerm(hd))
      return 0;
    i = IntOfTerm(hd);
    if (i <= 0 || i >= 255)
      return 0;
    if (!IsIntTerm(hd))
      return 0;
    t = TailOfTerm(t);
  }
  if (t != MkAtomTerm(LookupAtom("[]")))
    return 0;
  return FALSE;
}

X_API int PL_is_variable(term_t ts)
{
  Term t = YapGetFromSlot(ts);
  return IsVarTerm(t);
}

X_API void PL_halt(int e)
{
   YapHalt(e);
}

X_API fid_t
PL_open_foreign_frame(void)
{
  return 0;
}

X_API void
PL_close_foreign_frame(fid_t f)
{
}

X_API void
PL_discard_foreign_frame(fid_t f)
{
  fprintf(stderr,"WARNING: PL_discard_foreign_frame not fully implemented!!");
  /* Missing: undo Trail!! */
}

X_API term_t
PL_exception(qid_t q)
{
  Term t;
  if (YapGoalHasException(&t)) {
    term_t to = YapNewSlots(1);
    YapPutInSlot(to,t);
    return to;
  } else {
    return 0L;
  }
}

X_API int
PL_initialise(int myargc, char **myargv, char **myenviron)
{
  yap_init_args init_args;

  init_args.Argv = myargv;
  init_args.Argc = myargc;
  init_args.SavedState = "startup";
  init_args.HeapSize = 0;
  init_args.StackSize = 0;
  init_args.TrailSize = 0;
  init_args.YapLibDir = NULL;
  init_args.YapPrologBootFile = NULL;
  init_args.HaltAfterConsult = FALSE;
  init_args.FastBoot = FALSE;
  init_args.NumberWorkers = 1;
  init_args.SchedulerLoop = 10;
  init_args.DelayedReleaseLoad = 3;
  return YapInit(&init_args);
}

X_API predicate_t PL_pred(functor_t f, module_t m)
{
  if (IsAtomTerm(f)) {
    return YapPredicate(AtomOfTerm(f),0,m);
  } else {
    Functor tf = (Functor)f;
    return YapPredicate(YapNameOfFunctor(tf),YapArityOfFunctor(tf),m);
  }
}

X_API predicate_t PL_predicate(const char *name, int arity, const char *m)
{
  int mod;
  if (m == NULL)
    mod = YapCurrentModule();
  else
    mod = YapLookupModule(MkAtomTerm(LookupAtom((char *)m)));
  return YapPredicate(YapLookupAtom((char *)name),
		      arity,
		      mod);
}

X_API void PL_predicate_info(predicate_t p,atom_t *name, int *arity, module_t *m)
{
  YapPredicateInfo(p, name, (Int *)arity, (Int *)m);
}

typedef struct open_query_struct {
  int open;
  int state;
  Term g;
} open_query;

open_query execution;

X_API qid_t PL_open_query(module_t ctx, int flags, predicate_t p, term_t t0)
{
  atom_t name;
  Int arity;
  Int m;
  Term t[2];

  /* ignore flags  and module for now */
  if (execution.open != 0) {
    YapError("only one query at a time allowed\n");
  }
  execution.open=1;
  execution.state=0;
  YapPredicateInfo(p, &name, &arity, &m);
  t[0] = YapModuleName(m);
  if (arity == 0) {
    t[1] = YapMkAtomTerm(name);
  } else {
    Functor f = YapMkFunctor(name, arity);
    t[1] = YapMkApplTerm(f,arity,YapAddressFromSlot(t0));
  }
  execution.g = MkApplTerm(YapMkFunctor(YapLookupAtom(":"),2),2,t);
  return &execution;
}

X_API int PL_next_solution(qid_t qi)
{
  int result;

  if (qi->open != 1) return 0;
  if (qi->state == 0) {
    result = YapRunGoal(qi->g);
  } else {
    result = YapRestartGoal();
  }
  qi->state = 1;
  if (result == 0) {
    qi->open = 0;
  }
  return result;
}

X_API void PL_cut_query(qid_t qi)
{
  YapPruneGoal();
  qi->open = 0;
}

X_API void PL_close_query(qid_t qi)
{
  /* need to implement backtracking here */
  if (qi->open != 1)
    return;
  YapPruneGoal();
  YapRestartGoal();
  qi->open = 0;
}

X_API int PL_call_predicate(module_t ctx, int flags, predicate_t p, term_t t0)
{
  qid_t qi = PL_open_query(ctx, flags, p, t0);
  int ret = PL_next_solution(qi);
  PL_cut_query(qi);
  return ret;
}

X_API int PL_call(term_t tp, module_t m)
{
  Term t[2], g;
  t[0] = YapModuleName(m);
  t[1] = YapGetFromSlot(tp);
  g = MkApplTerm(YapMkFunctor(YapLookupAtom(":"),2),2,t);
  return YapRunGoal(g);
}

X_API void PL_register_extensions(PL_extension *ptr)
{
  /* ignore flags for now */
  while(ptr->predicate_name != NULL) {
    YapUserCPredicateWithArgs(ptr->predicate_name,ptr->function,ptr->arity,YapCurrentModule());
    ptr++;
  }
}

/* note: fprintf may be called from anywhere, so please don't try
   to be smart and allocate stack from somewhere else */
X_API int Sprintf(char *format,...)
{
  va_list ap;
  char buf[512];

  va_start(ap,format);
#ifdef HAVE_VSNPRINTF
  vsnprintf(buf,512,format,ap);
#else
  vsprintf(buf,format,ap);
#endif
  va_end(ap);

  fputs(buf, stderr);
  return 1;
}


#ifdef _WIN32

#include <windows.h>

int WINAPI PROTO(win_yap2swi, (HANDLE, DWORD, LPVOID));

int WINAPI win_yap2swi(HANDLE hinst, DWORD reason, LPVOID reserved)
{
  switch (reason) 
    {
    case DLL_PROCESS_ATTACH:
      break;
    case DLL_PROCESS_DETACH:
      break;
    case DLL_THREAD_ATTACH:
      break;
    case DLL_THREAD_DETACH:
      break;
    }
  return 1;
}
#endif
