/* yap2swi.c  */
/*
 * Project: jpl for Yap Prolog
 * Author: Steve Moyle and Vitor Santos Costa
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
X_API PL_agc_hook_t
PL_agc_hook(PL_agc_hook_t entry)
{
  return entry;
}

/* SWI: char* PL_atom_chars(atom_t atom)
   YAP: char* AtomName(Atom) */
X_API char* PL_atom_chars(atom_t a)	 /* SAM check type */
{
  return YAP_AtomName((YAP_Atom)a);
}


/* SWI: term_t PL_copy_term_ref(term_t from)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API term_t PL_copy_term_ref(term_t from)
{
  return YAP_InitSlot(YAP_GetFromSlot(from));
}

X_API term_t PL_new_term_ref(void)
{
  
  term_t to = YAP_NewSlots(1);
  return to;
}

X_API term_t PL_new_term_refs(int n)
{
  
  term_t to = YAP_NewSlots(n);
  return to;
}

X_API void PL_reset_term_refs(term_t after)
{
  term_t new = YAP_NewSlots(1);
  YAP_RecoverSlots(after-new);
}

/* begin PL_get_* functions =============================*/

/* SWI: int PL_get_arg(int index, term_t t, term_t a)
   YAP: YAP_Term YAP_ArgOfTerm(int argno, YAP_Term t)*/
X_API int PL_get_arg(int index, term_t ts, term_t a)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  if ( !YAP_IsApplTerm(t) ) {
    if (YAP_IsPairTerm(t)) {
      if (index == 1){
	YAP_PutInSlot(a,YAP_HeadOfTerm(t));
	return 1;
      } else if (index == 2) {
	YAP_PutInSlot(a,YAP_TailOfTerm(t));
	return 1;
      }
    }
    return 0;
  }
  YAP_PutInSlot(a,YAP_ArgOfTerm(index, t));
  return 1;
}
   
/* SWI: int PL_get_atom(term_t t, YAP_Atom *a)
   YAP: YAP_Atom YAP_AtomOfTerm(Term) */
X_API int PL_get_atom(term_t ts, atom_t *a)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  if ( !YAP_IsAtomTerm(t))
    return 0;
  *a = (atom_t)YAP_AtomOfTerm(t);
  return 1;
}

/* SWI: int PL_get_atom_chars(term_t t, char **s)
   YAP: char* AtomName(Atom) */
X_API int PL_get_atom_chars(term_t ts, char **a)  /* SAM check type */
{
  YAP_Term t = YAP_GetFromSlot(ts);
  if (!YAP_IsAtomTerm(t))
    return 0;
  *a = YAP_AtomName(YAP_AtomOfTerm(t));
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

static int CvtToStringTerm(YAP_Term t, char *buf, char *buf_max)
{
  *buf++ = '\"';
  while (YAP_IsPairTerm(t)) {
    YAP_Term hd = YAP_HeadOfTerm(t);
    long int  i;
    if (!YAP_IsIntTerm(hd))
      return 0;
    i = YAP_IntOfTerm(hd);
    if (i <= 0 || i >= 255)
      return 0;
    if (!YAP_IsIntTerm(hd))
      return 0;
    *buf++ = i;
    if (buf == buf_max)
      return 0;
    t = YAP_TailOfTerm(t);
  }
  if (t != YAP_MkAtomTerm(YAP_LookupAtom("[]")))
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
  YAP_Term t = YAP_GetFromSlot(l);
  char *tmp;

  if (!(flags & BUF_RING)) {
    tmp = alloc_ring_buf();
  } else {
    tmp = buffers;
  }
  *sp = tmp;
  if (YAP_IsAtomTerm(t)) {
    if (!(flags & (CVT_ATOM|CVT_ATOMIC|CVT_ALL)))
      return 0;
    *sp = YAP_AtomName(YAP_AtomOfTerm(t));
    return 1;
  } else if (YAP_IsIntTerm(t)) {
    if (!(flags & (CVT_INTEGER|CVT_NUMBER|CVT_ATOMIC|CVT_ALL)))
      return 0;
    snprintf(tmp,BUF_SIZE,"%ld",YAP_IntOfTerm(t));
  } else if (YAP_IsFloatTerm(t)) {
    if (!(flags & (CVT_FLOAT|CVT_ATOMIC|CVT_NUMBER|CVT_ALL)))
      return 0;
    snprintf(tmp,BUF_SIZE,"%f",YAP_FloatOfTerm(t));
  } else if (flags & CVT_STRING) {
    if (CvtToStringTerm(t,tmp,tmp+BUF_SIZE) == 0)
      return 0;
  } else {
    bf = tmp;
    bf_lim = tmp+(BUF_SIZE-1);
    YAP_Write(t,buf_writer,0);
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
   YAP: YAP_Functor YAP_FunctorOfTerm(Term) */
X_API int PL_get_functor(term_t ts, functor_t *f)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  if ( YAP_IsAtomTerm(t)) {
    *f = t;
  } else {
    *f = (functor_t)YAP_FunctorOfTerm(t);
  }
  return 1;
}

/* SWI: int PL_get_float(term_t t, double *f)
   YAP: double YAP_FloatOfTerm(Term) */
X_API int PL_get_float(term_t ts, double *f) /* SAM type check*/
{	
  YAP_Term t = YAP_GetFromSlot(ts);
  if ( !YAP_IsFloatTerm(t))
    return 0;
  *f = YAP_FloatOfTerm(t);
  return 1;
}

X_API int PL_get_head(term_t ts, term_t h)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  if (!YAP_IsPairTerm(t) ) {
    return 0;
  }
  YAP_PutInSlot(h,YAP_HeadOfTerm(t));
  return 1;
}

/* SWI: int PL_get_integer(term_t t, int *i)
   YAP: long int  YAP_IntOfTerm(Term) */
X_API int PL_get_integer(term_t ts, int *i)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  if (!YAP_IsIntTerm(t) )
    return 0;
  *i = YAP_IntOfTerm(t);
  return 1;
}

X_API int PL_get_long(term_t ts, long *i)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  if (!YAP_IsIntTerm(t) ) {
    if (YAP_IsFloatTerm(t)) {
      double dbl = YAP_FloatOfTerm(t);
      if (dbl - (long)dbl == 0.0) {
	*i = (long)dbl;
	return 1;
      }
    }
    return 0;
  }
  *i = YAP_IntOfTerm(t);
  return 1;
}

X_API int PL_get_list(term_t ts, term_t h, term_t tl)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  if (!YAP_IsPairTerm(t) ) {
    return 0;
  }
  YAP_PutInSlot(h,YAP_HeadOfTerm(t));
  YAP_PutInSlot(tl,YAP_TailOfTerm(t));
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
  YAP_Term t = YAP_GetFromSlot(ts);
  if (!YAP_IsAtomTerm(t) )
    return 0;
  *m = (module_t)YAP_LookupModule(t);
  return 1;
}

/* SWI: int PL_new_module(term_t t, module_t *m) */
X_API module_t PL_new_module(atom_t at)
{
  return (module_t)YAP_CreateModule((YAP_Atom)at);
}

/* SWI: int PL_get_atom(term_t t, YAP_Atom *a)
   YAP: YAP_Atom YAP_AtomOfTerm(Term) */
X_API int PL_get_name_arity(term_t ts, atom_t *name, int *arity)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  if (YAP_IsAtomTerm(t)) {
    *name = (atom_t)YAP_AtomOfTerm(t);
    *arity = 0;
    return 1;
  }
  if (YAP_IsApplTerm(t)) {
    YAP_Functor f = YAP_FunctorOfTerm(t);
    *name = (atom_t)YAP_NameOfFunctor(f);
    *arity = YAP_ArityOfFunctor(f);
    return 1;
  }
  if (YAP_IsPairTerm(t)) {
    *name = (atom_t)YAP_LookupAtom(".");
    *arity = 2;
    return 1;
  }
  return 0;
}

/* SWI: int PL_get_atom(term_t t, YAP_Atom *a)
   YAP: YAP_Atom YAP_AtomOfTerm(Term) */
X_API int PL_get_nil(term_t ts)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  return ( t == YAP_MkAtomTerm(YAP_LookupAtom("[]")));
}

/* SWI: int PL_get_pointer(term_t t, int *i)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API int PL_get_pointer(term_t ts, void **i)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  if (!YAP_IsIntTerm(t) )
    return 0;
  *i = (void *)YAP_IntOfTerm(t);
  return 1;
}

/* SWI: int PL_get_atom_chars(term_t t, char **s)
   YAP: char* AtomName(Atom) */
X_API int PL_get_string(term_t ts, char **sp, int *lenp)  /* SAM check type */
{
  YAP_Term t = YAP_GetFromSlot(ts);
  char *to;
  int len;
  if (!YAP_IsPairTerm(t))
    return 0;
  if (!YAP_StringToBuffer(t, buffers, TMP_BUF_SIZE))
      return(FALSE);
  len = strlen(buffers);
  to = (char *)YAP_NewSlots((len/sizeof(YAP_Term))+1);
  strncpy(to, buffers, TMP_BUF_SIZE);
  *sp = to;
  return 1;
}

X_API int PL_get_tail(term_t ts, term_t tl)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  if (!YAP_IsPairTerm(t) ) {
    return 0;
  }
  YAP_PutInSlot(tl,YAP_TailOfTerm(t));
  return 1;
}

/* end PL_get_* functions  =============================*/

/* begin PL_new_* functions =============================*/

/* SWI: atom_t PL_new_atom(const char *)
   YAP: YAP_Atom LookupAtom(char *) */
/*  SAM should the following be used instead?
      YAP_Atom  FullLookupAtom(char *)
      */
X_API atom_t PL_new_atom(const char *c)
{
  return (atom_t)YAP_LookupAtom((char *)c);
}

X_API functor_t PL_new_functor(atom_t name, int arity)
{
  functor_t f;
  if (arity == 0) {
    f = (functor_t)YAP_MkAtomTerm((YAP_Atom)name);
  } else {
    f = (functor_t)YAP_MkFunctor((YAP_Atom)name,arity);
  }
  return f;
}

X_API atom_t PL_functor_name(functor_t f)
{
  if (YAP_IsAtomTerm(f)) {
    return (atom_t)YAP_AtomOfTerm(f);
  } else {
    return (atom_t)YAP_NameOfFunctor((YAP_Functor)f);
  }
}

X_API int PL_functor_arity(functor_t f)
{
  if (YAP_IsAtomTerm(f)) {
    return 0;
  } else {
    return YAP_ArityOfFunctor((YAP_Functor)f);
  }
}

/* end PL_new_* functions =============================*/

/* begin PL_put_* functions =============================*/

X_API void PL_cons_functor(term_t d, functor_t f,...)
{
  va_list ap;
  int arity, i;
  YAP_Term *tmp = (YAP_CELL *)buffers;

  if (YAP_IsAtomTerm((YAP_Term)f)) {
    YAP_PutInSlot(d, (YAP_Term)f);
    return;
  }
  arity = YAP_ArityOfFunctor((YAP_Functor)f);
  if (arity > TMP_BUF_SIZE/sizeof(YAP_CELL)) {
    fprintf(stderr,"PL_cons_functor: arity too large (%d)\n", arity); 
    return;
  }
  va_start (ap, f);
  for (i = 0; i < arity; i++) {
    tmp[i] =  YAP_GetFromSlot(va_arg(ap, term_t));
  }
  va_end (ap);
  if (arity == 2 && (YAP_Functor)f == YAP_MkFunctor(YAP_LookupAtom("."),2))
    YAP_PutInSlot(d,YAP_MkPairTerm(tmp[0],tmp[1]));
  else
    YAP_PutInSlot(d,YAP_MkApplTerm((YAP_Functor)f,arity,tmp));
}

X_API void PL_cons_functor_v(term_t d, functor_t f,term_t a0)
{
  int arity;

  if (YAP_IsAtomTerm(f)) {
    YAP_PutInSlot(d,(YAP_Term)f);
    return;
  }
  arity = YAP_ArityOfFunctor((YAP_Functor)f);
  if (arity == 2 && (YAP_Functor)f == YAP_MkFunctor(YAP_LookupAtom("."),2))
    YAP_PutInSlot(d,YAP_MkPairTerm(YAP_GetFromSlot(a0),YAP_GetFromSlot(a0+1)));    
  else
    YAP_PutInSlot(d,YAP_MkApplTerm((YAP_Functor)f,arity,YAP_AddressFromSlot(a0)));
}

X_API void PL_cons_list(term_t d, term_t h, term_t t)
{
  YAP_PutInSlot(d,YAP_MkPairTerm(YAP_GetFromSlot(h),YAP_GetFromSlot(t)));
}

X_API void PL_put_atom(term_t t, atom_t a)
{
  YAP_PutInSlot(t,YAP_MkAtomTerm((YAP_Atom)a));
}

X_API void PL_put_atom_chars(term_t t, const char *s)
{
  YAP_PutInSlot(t,YAP_MkAtomTerm(YAP_LookupAtom((char *)s)));
}

X_API void PL_put_float(term_t t, double fl)
{
  YAP_PutInSlot(t,YAP_MkFloatTerm(fl));
}

X_API void PL_put_functor(term_t t, functor_t f)
{
  long int  arity;
  if (YAP_IsAtomTerm(f)) {
    YAP_PutInSlot(t,f);
  } else {
    arity = YAP_ArityOfFunctor((YAP_Functor)f);
    if (arity == 2 && (YAP_Functor)f == YAP_MkFunctor(YAP_LookupAtom("."),2))
      YAP_PutInSlot(t,YAP_MkNewPairTerm());    
    else
      YAP_PutInSlot(t,YAP_MkNewApplTerm((YAP_Functor)f,arity));
  }
}

X_API void PL_put_integer(term_t t, long n)
{
  YAP_PutInSlot(t,YAP_MkIntTerm(n));
}

X_API void PL_put_list(term_t t)
{
  YAP_PutInSlot(t,YAP_MkNewPairTerm());
}

X_API void PL_put_list_chars(term_t t, const char *s)
{
  YAP_PutInSlot(t,YAP_BufferToString((char *)s));
}

X_API void PL_put_nil(term_t t)
{
  YAP_PutInSlot(t,YAP_MkAtomTerm(YAP_LookupAtom("[]")));
}

/* SWI: void PL_put_pointer(term_t -t, void *ptr)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API void PL_put_pointer(term_t t, void *ptr)
{
  YAP_Term tptr = YAP_MkIntTerm((long int)ptr);
  YAP_PutInSlot(t,tptr);
}

X_API void PL_put_string_chars(term_t t, const char *s)
{
  YAP_PutInSlot(t,YAP_BufferToString((char *)s));
}

X_API void PL_put_term(term_t d, term_t s)
{
  YAP_PutInSlot(d,YAP_GetFromSlot(s));
}

X_API void PL_put_variable(term_t t)
{
  YAP_PutInSlot(t,YAP_MkVarTerm());
}

/* end PL_put_* functions =============================*/

/* SWI: int PL_raise_exception(term_t exception)
   YAP: NO EQUIVALENT */
/* SAM TO DO */

X_API int PL_raise_exception(term_t exception)
{
  YAP_Throw(YAP_GetFromSlot(exception));
  return 0;
}

/* begin PL_unify_* functions =============================*/

X_API int PL_unify(term_t t1, term_t t2)
{
  return YAP_Unify(YAP_GetFromSlot(t1),YAP_GetFromSlot(t2));
}

/* SWI: int PL_unify_atom(term_t ?t, atom  *at)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_atom(term_t t, atom_t at)
{
  YAP_Term cterm = YAP_MkAtomTerm((YAP_Atom)at);
  return YAP_Unify(YAP_GetFromSlot(t),cterm);
}

/* SWI: int PL_unify_atom_chars(term_t ?t, const char *chars)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_atom_chars(term_t t, const char *s)
{
  YAP_Atom catom = YAP_LookupAtom((char *)s);
  YAP_Term cterm = YAP_MkAtomTerm(catom);
  return YAP_Unify(YAP_GetFromSlot(t),cterm);
}

/* SWI: int PL_unify_float(term_t ?t, double f)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_float(term_t t, double f)
{
  YAP_Term fterm = YAP_MkFloatTerm(f);
  return YAP_Unify(YAP_GetFromSlot(t),fterm);
}

/* SWI: int PL_unify_integer(term_t ?t, long n)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_integer(term_t t, long n)
{	
  YAP_Term iterm = YAP_MkIntTerm(n);
  return YAP_Unify(YAP_GetFromSlot(t),iterm);
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_list(term_t t, term_t h, term_t tail)
{
  YAP_Term pairterm = YAP_MkPairTerm(YAP_GetFromSlot(h),YAP_GetFromSlot(tail));
  return YAP_Unify(YAP_GetFromSlot(t), pairterm);
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_list_chars(term_t t, const char *chars)
{
  YAP_Term chterm = YAP_BufferToString((char *)chars);
  return YAP_Unify(YAP_GetFromSlot(t), chterm);
}

/* SWI: int PL_unify_nil(term_t ?l)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_nil(term_t l)
{
  YAP_Term nilterm = YAP_MkAtomTerm(YAP_LookupAtom("[]"));
  return YAP_Unify(YAP_GetFromSlot(l), nilterm);
}

/* SWI: int PL_unify_pointer(term_t ?t, void *ptr)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API int PL_unify_pointer(term_t t, void *ptr)
{
  YAP_Term ptrterm = YAP_MkIntTerm((long int)ptr);
  return YAP_Unify(YAP_GetFromSlot(t), ptrterm);
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_string_chars(term_t t, const char *chars)
{
  YAP_Term chterm = YAP_BufferToString((char *)chars);
  return YAP_Unify(YAP_GetFromSlot(t), chterm);
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

static YAP_Term
get_term(arg_types **buf)
{
  arg_types *ptr = *buf;
  int type = ptr->type;
  YAP_Term t;

  switch (type) {
  /* now build the error string */
  case PL_VARIABLE:
    t = YAP_MkVarTerm();
    break;
  case PL_ATOM:
    t = YAP_MkAtomTerm((YAP_Atom)ptr->arg.a);
    break;
  case PL_INTEGER:
    t = YAP_MkIntTerm(ptr->arg.l);
    break;
  case PL_FLOAT:
    t = YAP_MkFloatTerm(ptr->arg.dbl);
    break;
  case PL_POINTER:
    t = YAP_MkIntTerm((long int)(ptr->arg.p));
    break;
  case PL_STRING:
    t = YAP_BufferToString(ptr->arg.s);
    break;
  case PL_TERM:
    t = YAP_GetFromSlot(ptr->arg.t);
    break;
  case PL_CHARS:
    t = YAP_MkAtomTerm(YAP_LookupAtom(ptr->arg.s));
    break;
  case PL_FUNCTOR:
    {
      functor_t f = ptr->arg.f;
      long int  arity, i;
      term_t loc;

      if (YAP_IsAtomTerm((YAP_Term)f)) {
	t = (YAP_Term)f;
	break;
      }
      arity = YAP_ArityOfFunctor((YAP_Functor)f);
      loc = YAP_NewSlots(arity);
      ptr++;
      for (i= 0; i < arity; i++) {
	YAP_PutInSlot(loc+i,get_term(&ptr));
      }
      t = YAP_MkApplTerm((YAP_Functor)f,arity,YAP_AddressFromSlot(loc));
    }
    break;
  case PL_LIST:
    {
      term_t loc;

      loc = YAP_NewSlots(2);
      YAP_PutInSlot(loc,get_term(&ptr));
      YAP_PutInSlot(loc+1,get_term(&ptr));
      t = YAP_MkPairTerm(YAP_GetFromSlot(loc),YAP_GetFromSlot(loc+1));
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
   YAP long int  YAP_Unify(YAP_Term* a, Term* b) */
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
	if (!YAP_IsAtomTerm((YAP_Term)f)) {
	  nels += YAP_ArityOfFunctor((YAP_Functor)f);
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
  return YAP_Unify(YAP_GetFromSlot(l),get_term(&ptr));
}

/* end PL_unify_* functions =============================*/

/* SWI: void PL_register_atom(atom_t atom)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API void PL_register_atom(atom_t atom)
{
  YAP_Term ti = YAP_GetValue((YAP_Atom)atom);
  if (ti == YAP_MkAtomTerm(YAP_LookupAtom("[]"))) {
    YAP_PutValue((YAP_Atom)atom, YAP_MkIntTerm(1));
  } else if (YAP_IsIntTerm(ti)) {
    long int i = YAP_IntOfTerm(ti);
    YAP_PutValue((YAP_Atom)atom, YAP_MkIntTerm(i++));
  }
}

/* SWI: void PL_unregister_atom(atom_t atom)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API void PL_unregister_atom(atom_t atom)
{
  YAP_Term ti = YAP_GetValue((YAP_Atom)atom);
  if (YAP_IsIntTerm(ti)) {
    long int i = YAP_IntOfTerm(ti);
    if (i == 1)
      YAP_PutValue((YAP_Atom)atom, YAP_MkAtomTerm(YAP_LookupAtom("[]")));
    YAP_PutValue((YAP_Atom)atom, YAP_MkIntTerm(i--));
  }
}

X_API int PL_get_string_chars(term_t t, char **s, int *len)
{
  /* there are no such objects in Prolog */
  return FALSE;
}

X_API int PL_term_type(term_t t)
{
  /* YAP_ does not support strings as different objects */
  YAP_Term v = YAP_GetFromSlot(t);
  if (YAP_IsVarTerm(v)) {
    return PL_VARIABLE;
  } else if (YAP_IsAtomTerm(v)) {
    return PL_ATOM;
  } else if (YAP_IsIntTerm(v)) {
    return PL_INTEGER;
  } else if (YAP_IsFloatTerm(v)) {
    return PL_FLOAT;
  } else {
    return PL_TERM;
  }
}

X_API int PL_is_atom(term_t t)
{
  return YAP_IsAtomTerm(YAP_GetFromSlot(t));
}

X_API int PL_is_atomic(term_t ts)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  return !YAP_IsVarTerm(t) || !YAP_IsApplTerm(t) || !YAP_IsPairTerm(t);
}

X_API int PL_is_compound(term_t ts)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  return (YAP_IsApplTerm(t) || YAP_IsPairTerm(t));
}

X_API int PL_is_functor(term_t ts, functor_t f)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  if (YAP_IsApplTerm(t)) {
    return YAP_FunctorOfTerm(t) == (YAP_Functor)f;
  } else if (YAP_IsPairTerm(t)) {
    return YAP_FunctorOfTerm(t) == YAP_MkFunctor(YAP_LookupAtom("."),2);
  } else
    return 0;
}

X_API int PL_is_float(term_t ts)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  return YAP_IsFloatTerm(t);
}

X_API int PL_is_integer(term_t ts)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  return YAP_IsIntTerm(t);
}

X_API int PL_is_list(term_t ts)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  if (YAP_IsPairTerm(t)) {
    return 1;
  } else if (YAP_IsAtomTerm(t)) {
    return t == YAP_MkAtomTerm(YAP_LookupAtom("[]"));
  } else
    return 0;
}

X_API int PL_is_number(term_t ts)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  return YAP_IsIntTerm(t) || YAP_IsFloatTerm(t);
}

X_API int PL_is_string(term_t ts)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  while (YAP_IsPairTerm(t)) {
    YAP_Term hd = YAP_HeadOfTerm(t);
    long int  i;
    if (!YAP_IsIntTerm(hd))
      return 0;
    i = YAP_IntOfTerm(hd);
    if (i <= 0 || i >= 255)
      return 0;
    if (!YAP_IsIntTerm(hd))
      return 0;
    t = YAP_TailOfTerm(t);
  }
  if (t != YAP_MkAtomTerm(YAP_LookupAtom("[]")))
    return 0;
  return FALSE;
}

X_API int PL_is_variable(term_t ts)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  return YAP_IsVarTerm(t);
}

X_API int PL_compare(term_t ts1, term_t ts2)
{
  YAP_Term t1 = YAP_GetFromSlot(ts1);
  YAP_Term t2 = YAP_GetFromSlot(ts2);
  return YAP_CompareTerms(t1, t2);
}

X_API void PL_halt(int e)
{
   YAP_Halt(e);
}

X_API int PL_action(int action,...)
{
  va_list ap;

  va_start (ap, action);
  switch (action) {
  case PL_ACTION_TRACE:
    fprintf(stderr, "PL_ACTION_TRACE not supported\n");
    break;
  case PL_ACTION_DEBUG:
    fprintf(stderr, "PL_ACTION_DEBUG not supported\n");
    break;
  case PL_ACTION_BACKTRACE:
    fprintf(stderr, "PL_ACTION_BACKTRACE not supported\n");
    break;
  case PL_ACTION_HALT:
    {
      int halt_arg = va_arg(ap, int);
      YAP_Halt(halt_arg);
    }
    break;
  case PL_ACTION_ABORT:
    {
      YAP_Throw(YAP_MkAtomTerm(YAP_LookupAtom("abort")));
    }
    break;
  case PL_ACTION_BREAK:
    fprintf(stderr, "PL_ACTION_BREAK not supported\n");
    break;
  case PL_ACTION_GUIAPP:
    fprintf(stderr, "PL_ACTION_GUIAPP not supported\n");
    break;
  case PL_ACTION_WRITE:
    fprintf(stderr, "PL_ACTION_WRITE not supported\n");
    break;
  case PL_ACTION_FLUSH:
    fprintf(stderr, "PL_ACTION_WRITE not supported\n");
    break;
  case PL_ACTION_ATTACH_CONSOLE:
    fprintf(stderr, "PL_ACTION_WRITE not supported\n");
    break;
  }
  va_end (ap);
  return 0;
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
  YAP_Term t;
  if (YAP_GoalHasException(&t)) {
    term_t to = YAP_NewSlots(1);
    YAP_PutInSlot(to,t);
    return to;
  } else {
    return 0L;
  }
}

X_API int
PL_initialise(int myargc, char **myargv)
{
  YAP_init_args init_args;

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
  return YAP_Init(&init_args);
}

X_API int
PL_is_initialised(int *argc, char ***argv)
{
  return TRUE;
}

X_API predicate_t PL_pred(functor_t f, module_t m)
{
  if (YAP_IsAtomTerm(f)) {
    return YAP_Predicate(YAP_AtomOfTerm(f),0,(YAP_Module)m);
  } else {
    YAP_Functor tf = (YAP_Functor)f;
    return YAP_Predicate(YAP_NameOfFunctor(tf),YAP_ArityOfFunctor(tf),(YAP_Module)m);
  }
}

X_API predicate_t PL_predicate(const char *name, int arity, const char *m)
{
  int mod;
  if (m == NULL)
    mod = YAP_CurrentModule();
  else
    mod = YAP_LookupModule(YAP_MkAtomTerm(YAP_LookupAtom((char *)m)));
  return YAP_Predicate(YAP_LookupAtom((char *)name),
		      arity,
		      mod);
}

X_API void PL_predicate_info(predicate_t p,atom_t *name, int *arity, module_t *m)
{
  YAP_PredicateInfo(p, (YAP_Atom *)name, (unsigned long int  *)arity, (YAP_Module *)m);
}

typedef struct open_query_struct {
  int open;
  int state;
  YAP_Term g;
} open_query;

open_query execution;

X_API qid_t PL_open_query(module_t ctx, int flags, predicate_t p, term_t t0)
{
  atom_t name;
  unsigned long int  arity;
  YAP_Module  m;
  YAP_Term t[2];

  /* ignore flags  and module for now */
  if (execution.open != 0) {
    YAP_Error("only one query at a time allowed\n");
  }
  execution.open=1;
  execution.state=0;
  YAP_PredicateInfo(p, (YAP_Atom *)&name, &arity, &m);
  t[0] = YAP_ModuleName(m);
  if (arity == 0) {
    t[1] = YAP_MkAtomTerm((YAP_Atom)name);
  } else {
    YAP_Functor f = YAP_MkFunctor((YAP_Atom)name, arity);
    t[1] = YAP_MkApplTerm(f,arity,YAP_AddressFromSlot(t0));
  }
  execution.g = YAP_MkApplTerm(YAP_MkFunctor(YAP_LookupAtom(":"),2),2,t);
  return &execution;
}

X_API int PL_next_solution(qid_t qi)
{
  int result;

  if (qi->open != 1) return 0;
  if (qi->state == 0) {
    result = YAP_RunGoal(qi->g);
  } else {
    result = YAP_RestartGoal();
  }
  qi->state = 1;
  if (result == 0) {
    qi->open = 0;
  }
  return result;
}

X_API void PL_cut_query(qid_t qi)
{
  YAP_PruneGoal();
  qi->open = 0;
}

X_API void PL_close_query(qid_t qi)
{
  /* need to implement backtracking here */
  if (qi->open != 1)
    return;
  YAP_PruneGoal();
  YAP_RestartGoal();
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
  YAP_Term t[2], g;
  t[0] = YAP_ModuleName((YAP_Module)m);
  t[1] = YAP_GetFromSlot(tp);
  g = YAP_MkApplTerm(YAP_MkFunctor(YAP_LookupAtom(":"),2),2,t);
  return YAP_RunGoal(g);
}

X_API void PL_register_extensions(PL_extension *ptr)
{
  /* ignore flags for now */
  while(ptr->predicate_name != NULL) {
    YAP_UserCPredicateWithArgs(ptr->predicate_name,(YAP_Bool (*)(void))ptr->function,ptr->arity,YAP_CurrentModule());
    ptr++;
  }
}

X_API void PL_load_extensions(PL_extension *ptr)
{
  /* ignore flags for now */
  while(ptr->predicate_name != NULL) {
    YAP_UserCPredicateWithArgs(ptr->predicate_name,(YAP_Bool (*)(void))ptr->function,ptr->arity,YAP_CurrentModule());
    ptr++;
  }
}

X_API int PL_thread_self(void)
{
  return YAP_ThreadSelf();
}

X_API int PL_thread_attach_engine(const PL_thread_attr_t *attr)
{
  int wid = YAP_ThreadSelf();
  
  if (wid < 0) {
    /* we do not have an engine */
    if (attr) {
      YAP_thread_attr yapt;
      int wid; 

      yapt.ssize = attr->local_size;
      yapt.tsize = attr->global_size;
      yapt.alias = (YAP_Term)attr->alias;
      yapt.cancel =  attr->cancel;
      wid = YAP_ThreadCreateEngine(&yapt);
    } else {
      wid = YAP_ThreadCreateEngine(NULL);
    }
    if (wid < 0)
      return -1;
    if (YAP_ThreadAttachEngine(wid)) {
      return wid;
    }
    return -1;
  } else {
    /* attach myself again */
    YAP_ThreadAttachEngine(wid);
    return wid;
  }
}

X_API int PL_thread_destroy_engine(void)
{
  int wid = YAP_ThreadSelf();

  if (wid < 0) {
    /* we do not have an engine */
    return FALSE;
  }
  YAP_ThreadDetachEngine(wid);
  return YAP_ThreadDestroyEngine(wid);
}

X_API int
PL_thread_at_exit(void (*function)(void *), void *closure, int global)
{
  /* don't do nothing for now */
  fprintf(stderr,"%% YAP ERROR: PL_thread_at_exit not implemented yet\n");
  return TRUE;
}


X_API PL_engine_t
PL_create_engine(const PL_thread_attr_t *attr)
{
  if (attr) {
    YAP_thread_attr yapt;

    yapt.ssize = attr->local_size;
    yapt.tsize = attr->global_size;
    yapt.alias = (YAP_Term)attr->alias;
    yapt.cancel =  attr->cancel;
    return (PL_engine_t)YAP_ThreadCreateEngine(&yapt);
  } else {
    return (PL_engine_t)YAP_ThreadCreateEngine(NULL);
  }
}


X_API int
PL_destroy_engine(PL_engine_t e)
{
  return YAP_ThreadDestroyEngine((int)e);
}

X_API int
PL_set_engine(PL_engine_t engine, PL_engine_t *old)
{
  int cwid = YAP_ThreadSelf();
  if (*old) *old = (PL_engine_t)cwid;
  if (engine == PL_ENGINE_CURRENT)
    return PL_ENGINE_SET;
  if (engine < 0) /* should really check if engine does not exist */
    return PL_ENGINE_INVAL;
  if (!(YAP_ThreadAttachEngine((int)engine))) {
    return PL_ENGINE_INUSE;
  }
  return PL_ENGINE_SET;
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


/* note: fprintf may be called from anywhere, so please don't try
   to be smart and allocate stack from somewhere else */
X_API int Sdprintf(char *format,...)
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

#if DEBUG
  fputs(buf, stderr);
#endif
  return 1;
}

int
swi_install(void)
{
  return TRUE;
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
