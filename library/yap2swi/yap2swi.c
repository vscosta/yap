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
#include	<wchar.h>
#include	<assert.h>


#include	<SWI-Stream.h>
#include	<SWI-Prolog.h>

#ifdef USE_GMP
#include <gmp.h>
#endif

#define BUF_SIZE 256
#define TMP_BUF_SIZE 2*BUF_SIZE
#define BUF_RINGS 16

char buffers[TMP_BUF_SIZE+BUF_SIZE*BUF_RINGS];
static int buf_index = 0;

static char *
alloc_ring_buf(void)
{
  int ret = buf_index;
  buf_index++;
  if (buf_index == BUF_RINGS)
    buf_index = 0;
  return buffers+(TMP_BUF_SIZE+ret*BUF_SIZE);
}

/* SWI: void PL_agc_hook(void) */

X_API PL_agc_hook_t
PL_agc_hook(PL_agc_hook_t entry)
{
  return (PL_agc_hook_t)YAP_AGCRegisterHook((YAP_agc_hook)entry);
}

/* SWI: char* PL_atom_chars(atom_t atom)
   YAP: char* AtomName(Atom) */
X_API char* PL_atom_chars(atom_t a)	 /* SAM check type */
{
  return (char *)YAP_AtomName((YAP_Atom)a);
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
  *a = (char *)YAP_AtomName(YAP_AtomOfTerm(t));
  return 1;
}

/*
  int PL_get_chars(term_t +t, char **s, unsigned flags) Convert the
  argument term t to a 0-terminated C-string. flags is a bitwise
  disjunction from two groups of constants. The first specifies which
  term-types should converted and the second how the argument is
  stored. Below is a specification of these constants. BUF_RING
  implies, if the data is not static (as from an atom), the data is
  copied to the next buffer from a ring of sixteen (16) buffers. This is a
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
    YAP_Atom at = YAP_AtomOfTerm(t);
    if (!(flags & (CVT_ATOM|CVT_ATOMIC|CVT_ALL)))
      return 0;
    if (YAP_IsWideAtom(at))
      /* will this always work? */
      snprintf(*sp,BUF_SIZE,"%ls",YAP_WideAtomName(at));
    else
      *sp = (char *)YAP_AtomName(YAP_AtomOfTerm(t));
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
    char *nbf = YAP_AllocSpaceFromYap(strlen(tmp)+1);
    if (nbf == NULL)
      return 0;
    strncpy(nbf,tmp,BUF_SIZE);
    *sp = nbf;
  }
  return 1;
}

X_API int PL_get_nchars(term_t l, size_t *len, char **sp, unsigned flags)
{
  int out = PL_get_chars(l, sp, flags);
  if (!out) return out;
  *len = strlen(*sp);
  return out;
}


/* same as get_chars, but works on buffers of wide chars */
X_API int PL_get_wchars(term_t l, size_t *len, wchar_t **wsp, unsigned flags)
{
  if (YAP_IsAtomTerm(l)) {
    YAP_Atom at = YAP_AtomOfTerm(l);

    if (!(flags & (CVT_ATOM|CVT_ATOMIC|CVT_ALL)))
      return 0;
    if (YAP_IsWideAtom(at))
      /* will this always work? */
      *wsp = (wchar_t *)YAP_WideAtomName(at);
  } else {
    char *sp;
    int res = PL_get_chars(l, &sp, ((flags & ~(BUF_MALLOC|BUF_DISCARDABLE))|BUF_RING));
    size_t sz;

    if (!res) {
      if (flags & CVT_EXCEPTION)
	YAP_Error(0, 0L, "PL_get_wchars");
      return 0;
    }
    sz = wcstombs(sp,NULL,BUF_SIZE);
    if (flags & BUF_MALLOC) {
      wchar_t *nbf = (wchar_t *)YAP_AllocSpaceFromYap(sz+1);
      if (nbf == NULL) {
	if (flags & CVT_EXCEPTION)
	  YAP_Error(0, 0L, "PL_get_wchars: lack of memory");
	return 0;
      }
      *wsp = nbf;
    } else if (flags & BUF_DISCARDABLE) {
      wchar_t *buf = (wchar_t *)buffers;

      if (wcstombs(sp,buf,BUF_SIZE) == -1) {
	if (flags & CVT_EXCEPTION)
	  YAP_Error(0, 0L, "PL_get_wchars: wcstombs");
	return 0;
      }
      *wsp = buf;
    } else {
      wchar_t *tmp = (wchar_t *)alloc_ring_buf();
      if (wcstombs(sp, tmp, BUF_SIZE) == -1) {
	if (flags & CVT_EXCEPTION)
	  YAP_Error(0, 0L, "PL_get_wchars: wcstombs");
	return 0;
      }
      *wsp = tmp;
    }
    return res;
  }
  if (flags & CVT_EXCEPTION)
    YAP_Error(0, 0L, "PL_get_wchars");
  return 0;
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

/* SWI: int PL_get_bool(term_t t, int *i)
   YAP: long int  YAP_AtomOfTerm(Term) */
X_API int PL_get_bool(term_t ts, int *i)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  char *sp;

  if (!YAP_IsAtomTerm(t) )
    return 0;
  sp = (char *)YAP_AtomName(YAP_AtomOfTerm(t));
  if (!strcmp(sp,"true")) {
    *sp = TRUE;
    return 1;    
  }
  if (!strcmp(sp,"false")) {
    *sp = FALSE;
    return 1;    
  }
  return 0;
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


X_API int PL_get_int64(term_t ts, int64_t *i)
{
  YAP_Term t = YAP_GetFromSlot(ts);
  if (!YAP_IsIntTerm(t) ) {
    if (YAP_IsFloatTerm(t)) {
      double dbl = YAP_FloatOfTerm(t);
      if (dbl - (int64_t)dbl == 0.0) {
	*i = (int64_t)dbl;
	return 1;
      }
#if USE_GMP
    } else if (YAP_IsBigNumTerm(t)) {
      MP_INT g;
      char s[64];
      YAP_BigNumOfTerm(t, (void *)&g);
      if (mpz_sizeinbase(&g,2) > 64) {
	return 0;
      }
      mpz_get_str (s, 10, &g);
      sscanf(s, "%lld", (long long int *)i);
      return 1;
#endif
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

X_API atom_t PL_new_atom_wchars(int len, const wchar_t *c)
{
  atom_t at;
  int i;

  for (i=0;i<len;i++) {
    if (c[i] > 255) break;
  }
  if (i!=len) {
    wchar_t *nbf = (wchar_t *)YAP_AllocSpaceFromYap((len+1)*sizeof(wchar_t));
    for (i=0;i<len;i++)
      nbf[i] = c[i];
    nbf[len]='\0';
    at = (atom_t)YAP_LookupWideAtom(nbf);
    YAP_FreeSpaceFromYap(nbf);
  } else {
    char *nbf = (char *)YAP_AllocSpaceFromYap((len+1)*sizeof(char));
    for (i=0;i<len;i++)
      nbf[i] = c[i];
    nbf[len]='\0';
    at = (atom_t)YAP_LookupAtom(nbf);
    YAP_FreeSpaceFromYap(nbf);
  }
  return at;
}

X_API char *PL_atom_nchars(atom_t name, size_t *sp)
{
  *sp = YAP_AtomNameLength((YAP_Atom)name);
  return (char *)YAP_AtomName((YAP_Atom)name);
}

X_API wchar_t *PL_atom_wchars(atom_t name, size_t *sp)
{
  if (!YAP_IsWideAtom((YAP_Atom)name))
    return NULL;
  *sp = YAP_AtomNameLength((YAP_Atom)name);
  return (wchar_t *)YAP_WideAtomName((YAP_Atom)name);
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

X_API void PL_put_int64(term_t t, int64_t n)
{
#if USE_GMP
  char s[64];
  MP_INT rop;

  sprintf(s, "%lld", (long long int)n);
  mpz_init_set_str (&rop, s, 10);
  YAP_PutInSlot(t,YAP_MkBigNumTerm((void *)&rop));
#endif
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

/* SWI: int PL_unify_atom_chars(term_t ?t, const char *chars)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_atom_nchars(term_t t, size_t len, const char *s)
{
  YAP_Atom catom;
  YAP_Term cterm;
  char *buf = (char *)YAP_AllocSpaceFromYap(len+1);

  if (!buf)
    return FALSE;
  strncpy(buf, s, len);
  buf[len] = '\0';
  catom = YAP_LookupAtom(buf);
  free(buf);
  cterm = YAP_MkAtomTerm(catom);
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

/* SWI: int PL_unify_integer(term_t ?t, long n)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_functor(term_t t, functor_t f)
{	
  YAP_Term tt = YAP_GetFromSlot(t);
  if (YAP_IsVarTerm(tt))
    return YAP_Unify(tt, YAP_MkNewApplTerm((YAP_Functor)f,YAP_ArityOfFunctor((YAP_Functor)f)));
  if (!YAP_IsApplTerm(tt))
    return FALSE;
  return f == (functor_t)YAP_FunctorOfTerm(tt);
}

/* SWI: int PL_unify_integer(term_t ?t, long n)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_int64(term_t t, int64_t n)
{	
#if USE_GMP
  YAP_Term iterm;
  char s[64];
  MP_INT rop;

  sprintf(s, "%lld", (long long int)n);
  mpz_init_set_str (&rop, s, 10);
  iterm = YAP_MkBigNumTerm((void *)&rop);
  return YAP_Unify(YAP_GetFromSlot(t),iterm);
#else
  return FALSE;
#endif
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

/* SWI: int PL_unify_wchars(term_t ?t, int type, size_t len,, const pl_wchar_t *s)
 */
X_API int PL_unify_wchars(term_t t, int type, size_t len, const pl_wchar_t *chars)
{
  YAP_Term chterm;

  if (len == (size_t)-1)
    len = wcslen(chars);

  switch (type) {
  case PL_ATOM:
    chterm = YAP_MkAtomTerm(YAP_LookupWideAtom(chars));
    break;
  case PL_STRING:
  case PL_CODE_LIST:
    chterm = YAP_NWideBufferToString(chars, len);
    break;
  case PL_CHAR_LIST:
    chterm = YAP_NWideBufferToAtomList(chars, len);
    break;
  default:
    /* should give error?? */
    return FALSE;
  }
  return YAP_Unify(YAP_GetFromSlot(t), chterm);
}

typedef struct {
  int type;
  union {
    functor_t f;
    term_t t;
    atom_t a;
    long l;
    int i;
    double dbl;
    char *s;
    struct {
      size_t n;
      char *s;
    } ns;
    struct {
      size_t n;
      wchar_t *w;
    } nw;
    void *p;
    wchar_t *w;
  } arg;
} arg_types;

static YAP_Atom
LookupMaxAtom(size_t n, char *s)
{
  YAP_Atom catom;
  char *buf = (char *)YAP_AllocSpaceFromYap(n+1);
  
  if (!buf)
    return FALSE;
  strncpy(buf, s, n);
  buf[n] = '\0';
  catom = YAP_LookupAtom(buf);
  free(buf);
  return catom;
}

static YAP_Atom
LookupMaxWideAtom(size_t n, wchar_t *s)
{
  YAP_Atom catom;
  wchar_t *buf = (wchar_t *)YAP_AllocSpaceFromYap((n+1)*sizeof(wchar_t));
  
  if (!buf)
    return FALSE;
  wcsncpy(buf, s, n);
  buf[n] = '\0';
  catom = YAP_LookupWideAtom(buf);
  free(buf);
  return catom;
}

static YAP_Term
MkBoolTerm(int b)
{
  if (b)
    return YAP_MkAtomTerm(YAP_LookupAtom("true"));
  else
    return YAP_MkAtomTerm(YAP_LookupAtom("false"));
}

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
    ptr++;
    break;
  case PL_BOOL:
    t = MkBoolTerm(ptr->arg.i);
    ptr++;
    break;
  case PL_ATOM:
    t = YAP_MkAtomTerm((YAP_Atom)ptr->arg.a);
    ptr++;
    break;
  case PL_CHARS:
    t = YAP_MkAtomTerm(YAP_LookupAtom(ptr->arg.s));
    break;
  case PL_NCHARS:
    t = YAP_MkAtomTerm(LookupMaxAtom(ptr->arg.ns.n, ptr->arg.ns.s));
    break;
  case PL_NWCHARS:
    t = YAP_MkAtomTerm(LookupMaxWideAtom(ptr->arg.nw.n, ptr->arg.nw.w));
    break;
  case PL_INTEGER:
    t = YAP_MkIntTerm(ptr->arg.l);
    ptr++;
    break;
  case PL_FLOAT:
    t = YAP_MkFloatTerm(ptr->arg.dbl);
    ptr++;
    break;
  case PL_POINTER:
    t = YAP_MkIntTerm((long int)(ptr->arg.p));
    ptr++;
    break;
  case PL_STRING:
    t = YAP_BufferToString(ptr->arg.s);
    ptr++;
    break;
  case PL_TERM:
    t = YAP_GetFromSlot(ptr->arg.t);
    ptr++;
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
      ptr++;
      YAP_PutInSlot(loc,get_term(&ptr));
      YAP_PutInSlot(loc+1,get_term(&ptr));
      t = YAP_MkPairTerm(YAP_GetFromSlot(loc),YAP_GetFromSlot(loc+1));
    }
    break;
  default:
    fprintf(stderr, "type %d not implemented yet\n", type);
    exit(1);
  }
  *buf = ptr;
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
    case PL_BOOL:
      ptr->arg.i = va_arg(ap, int);
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
    case PL_NCHARS:
      ptr->arg.ns.n = va_arg(ap, size_t);
      ptr->arg.ns.s = va_arg(ap, char *);
      break;
    case PL_NWCHARS:
      ptr->arg.nw.n = va_arg(ap, size_t);
      ptr->arg.nw.w = va_arg(ap, wchar_t *);
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

/* SWI: void PL_register_atom(atom_t atom) */
/* SAM TO DO */
X_API void PL_register_atom(atom_t atom)
{
  extern int Yap_AtomGetHold(atom_t atom);
  Yap_AtomGetHold(atom);
}

/* SWI: void PL_unregister_atom(atom_t atom) */
/* SAM TO DO */
X_API void PL_unregister_atom(atom_t atom)
{
  extern int Yap_AtomReleaseHold(atom_t atom);
  Yap_AtomReleaseHold(atom);
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

X_API module_t
PL_context(void)
{
  return (module_t)YAP_CurrentModule();
}

X_API int
PL_strip_module(term_t raw, module_t *m, term_t plain)
{
  YAP_Term t =  YAP_StripModule(YAP_GetFromSlot(raw),(YAP_Term *)m);
  if (!t)
    return FALSE;
  YAP_PutInSlot(plain, t);
  return TRUE;
}

X_API atom_t PL_module_name(module_t m)
{
  YAP_Atom at = YAP_AtomOfTerm((YAP_Term)m);
  YAP_CreateModule(at);
  return (atom_t)at;
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
  YAP_Atom yname;
  atom_t name;
  unsigned long int  arity;
  YAP_Module  m;
  YAP_Term t[2];

  /* ignore flags  and module for now */
  if (execution.open != 0) {
    YAP_Error(0, 0L, "only one query at a time allowed\n");
  }
  execution.open=1;
  execution.state=0;
  YAP_PredicateInfo(p, &yname, &arity, &m);
  name = (atom_t)yname;
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
  while(ptr->predicate_name != NULL) {
    if (ptr->flags & (PL_FA_NOTRACE|PL_FA_NONDETERMINISTIC|PL_FA_VARARGS|PL_FA_CREF)) {
      YAP_Error(0,YAP_MkIntTerm(ptr->flags),"non-implemented flag %x when creating predicates", ptr->flags);
      return;      
    }      
    if (ptr->flags & PL_FA_TRANSPARENT)
      YAP_UserCPredicateWithArgs(ptr->predicate_name,(YAP_Bool (*)(void))ptr->function,ptr->arity,YAP_MkAtomTerm(YAP_LookupAtom("prolog")));
    else
      YAP_UserCPredicateWithArgs(ptr->predicate_name,(YAP_Bool (*)(void))ptr->function,ptr->arity,YAP_CurrentModule());
    ptr++;
  }
}

X_API void PL_register_foreign_in_module(const char *module, const char *name, int arity, foreign_t (*function)(void), int flags)
{
    if (flags & (PL_FA_NOTRACE|PL_FA_NONDETERMINISTIC|PL_FA_VARARGS|PL_FA_CREF)) {
      YAP_Error(0,YAP_MkIntTerm(flags),"non-implemented flag %x when creating predicates", flags);
      return;      
    }      
  if (flags & PL_FA_TRANSPARENT)
    YAP_UserCPredicateWithArgs(name,(YAP_Bool (*)(void))function,arity,YAP_MkAtomTerm(YAP_LookupAtom("prolog")));
  else if (module == NULL)
    YAP_UserCPredicateWithArgs(name,(YAP_Bool (*)(void))function,arity,YAP_CurrentModule());
  else
    YAP_UserCPredicateWithArgs(name,(YAP_Bool (*)(void))function,arity,YAP_MkAtomTerm(YAP_LookupAtom(module)));
}

X_API void PL_load_extensions(PL_extension *ptr)
{
  /* ignore flags for now */
  while(ptr->predicate_name != NULL) {
    YAP_UserCPredicateWithArgs(ptr->predicate_name,(YAP_Bool (*)(void))ptr->function,ptr->arity,YAP_CurrentModule());
    ptr++;
  }
}

X_API int PL_handle_signals(void)
{
  fprintf(stderr,"not implemented\n");
  return 0;
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
  return YAP_ThreadDestroyEngine((YAP_Int)e);
}

X_API int
PL_set_engine(PL_engine_t engine, PL_engine_t *old)
{
  long int cwid = YAP_ThreadSelf();
  if (*old) *old = (PL_engine_t)cwid;
  if (engine == PL_ENGINE_CURRENT)
    return PL_ENGINE_SET;
  if (engine < 0) /* should really check if engine does not exist */
    return PL_ENGINE_INVAL;
  if (!(YAP_ThreadAttachEngine((long int)engine))) {
    return PL_ENGINE_INUSE;
  }
  return PL_ENGINE_SET;
}


X_API void *
PL_malloc(int sz)
{
  return YAP_AllocSpaceFromYap(sz);
}

X_API void
PL_free(void *obj)
{
  return YAP_FreeSpaceFromYap(obj);
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

static int
SWI_ctime(void)
{
#if HAVE_CTIME
  time_t tim;
#endif
  YAP_Term t1 = YAP_ARG1;

  if (YAP_IsVarTerm(t1)) {
    YAP_Error(0,t1,"bad argumento to ctime");
    return FALSE;
  }
#if HAVE_CTIME
  tim = (time_t)YAP_IntOfTerm(t1);
  return YAP_Unify(YAP_BufferToString(ctime(&tim)), YAP_ARG2);
#else
  YAP_Error(0,0L,"convert_time requires ctime");
  return FALSE;
#endif
}

/***** SWI IO ***************/

#include <errno.h>

#define char_to_int(c)	(0xff & (int)(c))

struct {
  struct {
    IOSTREAM *streams[6];
  } IO;
} lds;

#define LD (&lds)

#define ARG_LD
#define GET_LD
#define PASS_LD

#define LOCK()
#define UNLOCK()
#define FUNCTOR_dstream1 (functor_t)YAP_MkFunctor(YAP_LookupAtom("stream"),1)
#define succeed return 1
#define fail    return 0

static int	S__removebuf(IOSTREAM *s);
static int	S__flushbuf(IOSTREAM *s);

#ifndef MB_LEN_MAX
#define MB_LEN_MAX 6
#endif


#ifndef UTF8_H_INCLUDED
#define UTF8_H_INCLUDED

#define UTF8_MALFORMED_REPLACEMENT 0xfffd

#define ISUTF8_MB(c) ((unsigned)(c) >= 0xc0 && (unsigned)(c) <= 0xfd)

#define ISUTF8_CB(c)  (((c)&0xc0) == 0x80) /* Is continuation byte */
#define ISUTF8_FB2(c) (((c)&0xe0) == 0xc0)
#define ISUTF8_FB3(c) (((c)&0xf0) == 0xe0)
#define ISUTF8_FB4(c) (((c)&0xf8) == 0xf0)
#define ISUTF8_FB5(c) (((c)&0xfc) == 0xf8)
#define ISUTF8_FB6(c) (((c)&0xfe) == 0xfc)

#define UTF8_FBN(c) (!(c&0x80)     ? 0 : \
		     ISUTF8_FB2(c) ? 1 : \
		     ISUTF8_FB3(c) ? 2 : \
		     ISUTF8_FB4(c) ? 3 : \
		     ISUTF8_FB5(c) ? 4 : \
		     ISUTF8_FB6(c) ? 5 : -1)
#define UTF8_FBV(c,n) ( n == 0 ? c : (c & ((0x01<<(6-n))-1)) )

#define utf8_get_char(in, chr) \
	(*(in) & 0x80 ? _PL__utf8_get_char(in, chr) \
		      : (*(chr) = *(in), (char *)(in)+1))
#define utf8_put_char(out, chr) \
	((chr) < 0x80 ? out[0]=(char)(chr), out+1 \
		      : _PL__utf8_put_char(out, (chr)))

extern char *_PL__utf8_get_char(const char *in, int *chr);
extern char *_PL__utf8_put_char(char *out, int chr);

extern unsigned int utf8_strlen(const char *s, unsigned int len);

#endif /*UTF8_H_INCLUDED*/

typedef struct symbol *		Symbol;		/* symbol of hash table */

struct symbol
{ Symbol	next;		/* next in chain */
  void *	name;		/* name entry of symbol */
  void *	value;		/* associated value with name */
};

static Symbol *streamContext;

static Symbol *streamAliases;

#define NULL_ATOM 0L

#define allocHeap(size) YAP_AllocSpaceFromYap(size)

// FIX THIS
#define PL_error(A,B,C,D,E,F) 0

static Symbol lookupHTable(Symbol *htp, void *name)
{
  Symbol ht = *htp;
  while (ht) {
    if (ht->name == name) return ht;
  }
  return NULL;
}

static void addHTable(Symbol *htp, void *name, void *val)
{
  Symbol s = (Symbol)allocHeap(sizeof(Symbol));
  if (!s)
    return;
  s->next = *htp;
  s->name = name;
  s->value = val;
  *htp = s;
}

typedef struct _alias
{ struct _alias *next;
  atom_t name;
} alias;

typedef struct
{ alias *alias_head;
  alias *alias_tail;
  atom_t filename;			/* associated filename */
  unsigned flags;
} stream_context;


static stream_context *
getStreamContext(IOSTREAM *s)
{ Symbol symb;

  if ( !(symb = lookupHTable(streamContext, s)) )
  { GET_LD
    stream_context *ctx = allocHeap(sizeof(*ctx));

    //    DEBUG(1, Sdprintf("Created ctx=%p for stream %p\n", ctx, s));

    ctx->alias_head = ctx->alias_tail = NULL;
    ctx->filename = NULL_ATOM;
    ctx->flags = 0;
    addHTable(streamContext, s, ctx);

    return ctx;
  }

  return symb->value;
}

char *
_PL__utf8_put_char(char *out, int chr)
{ if ( chr < 0x80 )
  { *out++ = chr;
  } else if ( chr < 0x800 )
  { *out++ = 0xc0|((chr>>6)&0x1f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( chr < 0x10000 )
  { *out++ = 0xe0|((chr>>12)&0x0f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( chr < 0x200000 )
  { *out++ = 0xf0|((chr>>18)&0x07);
    *out++ = 0x80|((chr>>12)&0x3f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( chr < 0x4000000 )
  { *out++ = 0xf8|((chr>>24)&0x03);
    *out++ = 0x80|((chr>>18)&0x3f);
    *out++ = 0x80|((chr>>12)&0x3f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( chr < 0x80000000 )
  { *out++ = 0xfc|((chr>>30)&0x01);
    *out++ = 0x80|((chr>>24)&0x3f);
    *out++ = 0x80|((chr>>18)&0x3f);
    *out++ = 0x80|((chr>>12)&0x3f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  }

  return out;
}


static inline void
update_linepos(IOSTREAM *s, int c)
{ IOPOS *p = s->position;

  switch(c)
  { case '\n':
      p->lineno++;
      p->linepos = 0;
      s->flags &= ~SIO_NOLINEPOS;
#ifdef __WIN32__
      if ( s->flags & O_TEXT )
	p->charno++;			/* writes one extra! */
#endif
      break;
    case '\r':
      p->linepos = 0;
      s->flags &= ~SIO_NOLINEPOS;
      break;
    case '\b':
      if ( p->linepos > 0 )
	p->linepos--;
      break;
    case EOF:
      break;
    case '\t':
      p->linepos |= 7;
    default:
      p->linepos++;
  }
}




		 /*******************************
		 *	      BUFFER		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that the  buffer  is  allocated   from  s->unbuffer,  which  starts
MB_LEN_MAX before s->buffer, so we can always push-back a wide character
into a multibyte stream. We do  not   do  this for SIO_USERBUF case, but
this is only used by  the  output   stream  Svfprintf()  where it is not
needed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
S__setbuf(IOSTREAM *s, char *buffer, int size)
{ if ( size == 0 )
    size = SIO_BUFSIZE;

  S__removebuf(s);
  s->bufsize = size;

  if ( buffer )
  { s->unbuffer = s->buffer = buffer;
    s->flags |= SIO_USERBUF;
  } else
  { if ( !(s->unbuffer = malloc(s->bufsize+MB_LEN_MAX)) )
    { errno = ENOMEM;
      return -1;
    }
    s->flags &= ~SIO_USERBUF;
    s->buffer = s->unbuffer + MB_LEN_MAX;
  }

  s->limitp = &s->buffer[s->bufsize];
  s->bufp   = s->buffer;

  return size;
}


static int
S__removebuf(IOSTREAM *s)
{ if ( s->buffer && s->unbuffer )
  { int rval = 0;

    if ( (s->flags & SIO_OUTPUT) && S__flushbuf(s) < 0 )
      rval = -1;

    if ( !(s->flags & SIO_USERBUF) )
      free(s->unbuffer);
    s->bufp = s->limitp = s->buffer = s->unbuffer = NULL;
    s->bufsize = 0;

    return rval;
  }

  return 0;
}


#ifdef DEBUG_IO_LOCKS
static char *
Sname(IOSTREAM *s)
{ if ( s == Serror ) return "error";
  if ( s == Sinput ) return "input";
  if ( s == Soutput ) return "output";
  return "?";
}
#endif

		 /*******************************
		 *	     FLUSH/FILL		*
		 *******************************/

static int
S__flushbuf(IOSTREAM *s)
{ int size;
  char *from = s->buffer;

  while ( (size = s->bufp - from) > 0 )
  { int n = (*s->functions->write)(s->handle, from, size);

    if ( n > 0 )			/* wrote some */
    { from += n;
    } else if ( n < 0 )			/* error */
    { s->flags |= SIO_FERR;
      return -1;
    } else				/* wrote nothing? */
    { break;
    }
  }

  if ( s->bufp - from == 0 )		/* full flush */
  { int rc = s->bufp - s->buffer;

    s->bufp = s->buffer;

    return rc;
  } else				/* partial flush */
  { int rc = from - s->buffer;
    int left = s->bufp - from;

    memmove(s->buffer, from, left);
    s->bufp = s->buffer + left;

    return rc;
  }
}


static inline int
S__updatefilepos(IOSTREAM *s, int c)
{ IOPOS *p = s->position;

  if ( p )
  { update_linepos(s, c);
    p->charno++;
  }

  return c;
}


int
Sfileno(IOSTREAM *s)
{ int n;

  if ( s->flags & SIO_FILE )
  { long h = (long)s->handle;
    n = (int)h;
  } else if ( s->flags & SIO_PIPE )
  { n = fileno((FILE *)s->handle);
  } else if ( s->functions->control &&
	      (*s->functions->control)(s->handle,
				       SIO_GETFILENO,
				       (void *)&n)  == 0 )
  { ;
  } else
  { errno = EINVAL;
    n = -1;				/* no file stream */
  }

  return n;
}


IOSTREAM *
Snew(void *handle, int flags, IOFUNCTIONS *functions)
{ IOSTREAM *s;
  int fd;

  if ( !(s = malloc(sizeof(IOSTREAM))) )
  { errno = ENOMEM;
    return NULL;
  }
  memset((char *)s, 0, sizeof(IOSTREAM));
  s->magic         = SIO_MAGIC;
  s->lastc         = EOF;
  s->flags         = flags;
  s->handle        = handle;
  s->functions     = functions;
  s->timeout       = -1;		/* infinite */
  s->posbuf.lineno = 1;
  s->encoding      = ENC_ISO_LATIN_1;
  if ( flags & SIO_RECORDPOS )
    s->position = &s->posbuf;
#ifdef O_PLMT
  if ( !(flags & SIO_NOMUTEX) )
  { if ( !(s->mutex = malloc(sizeof(recursiveMutex))) )
    { free(s);
      return NULL;
    }
    recursiveMutexInit(s->mutex);
  }
#endif
  if ( (fd = Sfileno(s)) >= 0 && isatty(fd) )
    s->flags |= SIO_ISATTY;

  return s;
}


X_API int
PL_unify_stream(term_t t, IOSTREAM *s)
{ GET_LD
  stream_context *ctx;
  term_t a = PL_new_term_ref();

  LOCK();
  ctx = getStreamContext(s);
  UNLOCK();

  PL_put_pointer(a, s);
  PL_cons_functor(a, FUNCTOR_dstream1, a);

  if ( PL_unify(t, a) )
    succeed;
  if ( PL_is_functor(t, FUNCTOR_dstream1) )
    fail;

  return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_stream, t);
}

int					/* old FLI name (compatibility) */
PL_open_stream(term_t handle, IOSTREAM *s)
{ return PL_unify_stream(handle, s);
}

#define SH_ERRORS   0x01		/* generate errors */
#define SH_ALIAS    0x02		/* allow alias */
#define SH_UNLOCKED 0x04		/* don't lock the stream */
#define SH_SAFE	    0x08		/* Lookup in table */

#define _PL_get_arg PL_get_arg

#define getStream(s)	(s)

static int
get_stream_handle__LD(term_t t, IOSTREAM **s, int flags ARG_LD)
{ atom_t alias;

  if ( PL_is_functor(t, FUNCTOR_dstream1) )
  { void *p;
    term_t a = PL_new_term_ref();

    _PL_get_arg(1, t, a);
    if ( PL_get_pointer(a, &p) )
    { if ( flags & SH_SAFE )
      { Symbol symb;

	LOCK();
	symb = lookupHTable(streamContext, p);
	UNLOCK();
	
	if ( !symb )
	  goto noent;
      }

      if ( flags & SH_UNLOCKED )
      { if ( ((IOSTREAM *)p)->magic == SIO_MAGIC )
	{ *s = p;
	  return TRUE;
	}
	goto noent;
      }

      if ( (*s = getStream(p)) )
	return TRUE;

      goto noent;
    }
  } else if ( PL_get_atom(t, &alias) )
  { Symbol symb;

    if ( !(flags & SH_UNLOCKED) )
      LOCK();
    if ( (symb=lookupHTable(streamAliases, (void *)alias)) )
    { IOSTREAM *stream;
      unsigned long n = (unsigned long)symb->value;

      if ( n < 6 )			/* standard stream! */
      { stream = LD->IO.streams[n];
      } else
	stream = symb->value;
	
      if ( !(flags & SH_UNLOCKED) )
	UNLOCK();
      
      if ( stream )
      { if ( (flags & SH_UNLOCKED) )
	{ if ( stream->magic == SIO_MAGIC )
	  { *s = stream;
	    return TRUE;
	  }
	} else if ( (*s = getStream(stream)) )
	  return TRUE;
	goto noent;
      }
    }
    if ( !(flags & SH_UNLOCKED) )
      UNLOCK();

    goto noent;
  }
      
  if ( flags & SH_ERRORS )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    (flags&SH_ALIAS) ? ATOM_stream_or_alias : ATOM_stream, t);

  fail;

noent:
  if ( flags & SH_ERRORS )
    PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_stream, t);
  fail;
}

#define get_stream_handle(t, sp, flags) \
	get_stream_handle__LD(t, sp, flags PASS_LD)

int
PL_get_stream_handle(term_t t, IOSTREAM **s)
{ GET_LD
  return get_stream_handle(t, s, SH_ERRORS|SH_ALIAS);
}

static inline int
get_byte(IOSTREAM *s)
{ int c = Snpgetc(s);

  if ( s->position )
    s->position->byteno++;

  return c;
}


int
Sgetcode(IOSTREAM *s)
{ int c;

#ifdef CRLF_MAPPING
retry:
#endif

  switch(s->encoding)
  { case ENC_OCTET:
    case ENC_ISO_LATIN_1:
      c = get_byte(s);
      break;
    case ENC_ASCII:
    { c = get_byte(s);
      if ( c > 128 )
	Sseterr(s, SIO_WARN, "non-ASCII character");
      break;
    }
    case ENC_ANSI:
    { char b[1];
      int rc, n = 0;
      wchar_t wc;

      if ( !s->mbstate )
      { if ( !(s->mbstate = malloc(sizeof(*s->mbstate))) )
	  return EOF;			/* out of memory */
	memset(s->mbstate, 0, sizeof(*s->mbstate));
      }

      for(;;)
      { if ( (c = get_byte(s)) == EOF )
	{ if ( n == 0 )
	    return EOF;
	  else
	  { Sseterr(s, SIO_WARN, "EOF in multibyte Sequence");
	    goto mberr;
	  }
	}
	b[0] = c;

	if ( (rc=mbrtowc(&wc, b, 1, s->mbstate)) == 1 )
	{ c = wc;
	  goto out;
	} else if ( rc == -1 )
	{ Sseterr(s, SIO_WARN, "Illegal multibyte Sequence");
	  goto mberr;
	}				/* else -2: incomplete */
      }

    mberr:
      c = UTF8_MALFORMED_REPLACEMENT;
      goto out;
    }
    case ENC_UTF8:
    { c = get_byte(s);
      if ( c == EOF )
	break;

      if ( c & 0x80 )
      { int extra = UTF8_FBN(c);
	int code;

	code = UTF8_FBV(c,extra);
	for( ; extra > 0; extra-- )
	{ int c2 = get_byte(s);
	  
	  if ( !ISUTF8_CB(c2) )
	  { Sseterr(s, SIO_WARN, "Illegal UTF-8 Sequence");
	    c = UTF8_MALFORMED_REPLACEMENT;
	    Sungetc(c2, s);
	    goto out;
	  }
	  code = (code<<6)+(c2&0x3f);
	}
	c = code;
      }
      break;
    }
    case ENC_UNICODE_BE:
    case ENC_UNICODE_LE:
    { int c1, c2;

      c1 = get_byte(s);
      if ( c1 == EOF )
	return EOF;
      c2 = get_byte(s);

      if ( c2 == EOF )
      { Sseterr(s, SIO_WARN, "EOF in unicode character");
	c = UTF8_MALFORMED_REPLACEMENT;
      } else
      { if ( s->encoding == ENC_UNICODE_BE )
	  c = (c1<<8)+c2;
	else
	  c = (c2<<8)+c1;
      }

      break;
    }
    case ENC_WCHAR:
    { pl_wchar_t chr;
      char *p = (char*)&chr;
      int n;

      for(n=0; n<sizeof(pl_wchar_t); n++)
      { int c1 = get_byte(s);

	if ( c1 == EOF )
	{ if ( n == 0 )
	  { return EOF;
	  } else
	  { Sseterr(s, SIO_WARN, "EOF in UCS character");
	    c = UTF8_MALFORMED_REPLACEMENT; 
	    goto out;
	  }
	}

	*p++ = c1;
      }

      c = chr;
      break;
    }
    default:
      assert(0);
      c = -1;
  }

out:
#ifdef CRLF_MAPPING
  if ( c == '\r' && (s->flags&SIO_TEXT) )
    goto retry;
#endif

  if ( s->tee && s->tee->magic == SIO_MAGIC && c != -1 )
    Sputcode(c, s->tee);

  return S__updatefilepos(s, c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S__fillbuf() fills the read-buffer, returning the first character of it.
It also realises the SWI-Prolog timeout facility.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
S__fillbuf(IOSTREAM *s)
{ int c;

  if ( s->flags & (SIO_FEOF|SIO_FERR) )
  { s->flags |= SIO_FEOF2;		/* reading past eof */
    goto error;
  }

#ifdef HAVE_SELECT
  s->flags &= ~SIO_TIMEOUT;

  if ( s->timeout >= 0 )
  { int fd = Sfileno(s);

    if ( fd >= 0 )
    { fd_set wait;
      struct timeval time;
      int rc;
      
      time.tv_sec  = s->timeout / 1000;
      time.tv_usec = (s->timeout % 1000) * 1000;
      FD_ZERO(&wait);
#ifdef WIN32
      FD_SET((SOCKET)fd, &wait);
#else
      FD_SET(fd, &wait);
#endif

      for(;;)
      { rc = select(fd+1, &wait, NULL, NULL, &time);
	
	if ( rc < 0 && errno == EINTR )
	{ if ( PL_handle_signals() < 0 )
	  { errno = EPLEXCEPTION;
	    goto error;
	  }

	  continue;
	}

	break;
      }

      if ( rc == 0 )
      { s->flags |= (SIO_TIMEOUT|SIO_FERR);
	goto error;
      }
    } else
    { errno = EPERM;			/* no permission to select */
      s->flags |= SIO_FERR;
      goto error;
    }
  }
#endif


  if ( s->flags & SIO_NBUF )
  { char chr;
    int n;

    if ( (n=(*s->functions->read)(s->handle, &chr, 1)) == 1 )
    { c = char_to_int(chr);
      goto ok;
    } else if ( n == 0 )
    { if ( !(s->flags & SIO_NOFEOF) )
	s->flags |= SIO_FEOF;
      goto error;
    } else
    { s->flags |= SIO_FERR;
      goto error;			/* error */
    }
  } else
  { int n, len;

    if ( !s->buffer )
    { if ( S__setbuf(s, NULL, 0) < 0 )
	goto error;
      s->bufp = s->limitp = s->buffer;
      len = s->bufsize;
    } else if ( s->bufp < s->limitp )
    { len = s->limitp - s->bufp;
      memmove(s->buffer, s->bufp, s->limitp - s->bufp);
      s->bufp = s->buffer;
      s->limitp = &s->bufp[len];
      len = s->bufsize - len;
    } else
    { s->bufp = s->limitp = s->buffer;
      len = s->bufsize;
    }

    if ( (n=(*s->functions->read)(s->handle, s->limitp, len)) > 0 )
    { s->limitp += n;
      c = char_to_int(*s->bufp++);
      goto ok;
    } else
    { if ( n == 0 )
      { if ( !(s->flags & SIO_NOFEOF) )
	  s->flags |= SIO_FEOF;
	goto error;
#ifdef EWOULDBLOCK
      } else if ( errno == EWOULDBLOCK )
      { s->bufp = s->buffer;
	s->limitp = s->buffer;
	goto error;
#endif
      } else
      { s->flags |= SIO_FERR;
	goto error;
      }
    }
  }

error:
  c = -1;
ok:
  return c;
}

static int
S__flushbufc(int c, IOSTREAM *s)
{ if ( s->buffer )
  { if ( S__flushbuf(s) <= 0 )		/* == 0: no progress!? */
      c = -1;
    else
      *s->bufp++ = (c & 0xff);
  } else
  { if ( s->flags & SIO_NBUF )
    { char chr = (char)c;
    
      if ( (*s->functions->write)(s->handle, &chr, 1) != 1 )
      { s->flags |= SIO_FERR;
	c = -1;
      }
    } else
    { if ( S__setbuf(s, NULL, 0) < 0 )
      { s->flags |= SIO_FERR;
	c = -1;
      } else
	*s->bufp++ = (char)c;
    }
  }

  return c;
}


static inline void
unget_byte(int c, IOSTREAM *s)
{ IOPOS *p = s->position;

  *--s->bufp = c;
  if ( p )
  { p->charno--;
    p->byteno--;
    s->flags |= (SIO_NOLINENO|SIO_NOLINEPOS);
  }
}


int
Sungetc(int c, IOSTREAM *s)
{ if ( s->bufp > s->unbuffer )
  { unget_byte(c, s);

    return c;
  }

  return -1;
}

static int
put_byte(int c, IOSTREAM *s)
{ c &= 0xff;

  if ( s->bufp < s->limitp )
  { *s->bufp++ = c;
  } else
  { if ( S__flushbufc(c, s) < 0 )
    { s->lastc = EOF;
      return -1;
    }
  }

  if ( s->position )
    s->position->byteno++;

  return c;
}


static int
reperror(int c, IOSTREAM *s)
{ if ( c >= 0 && (s->flags & (SIO_REPXML|SIO_REPPL)) )
  { char buf[16];
    const char *q;

    if ( (s->flags & SIO_REPPL) )
    { if ( c <= 0xffff )
	sprintf(buf, "\\u%04X", c);
      else
	sprintf(buf, "\\U%08X", c);
    } else
      sprintf(buf, "&#%d;", c);

    for(q = buf; *q; q++)
    { if ( put_byte(*q, s) < 0 )
	return -1;
    }
	
    return c;
  }

  Sseterr(s, SIO_FERR|SIO_CLEARERR, "Encoding cannot represent character");
  return -1;
}

int
Sputcode(int c, IOSTREAM *s)
{ if ( c < 0 )
    return reperror(c, s);

  if ( s->tee && s->tee->magic == SIO_MAGIC )
    Sputcode(c, s->tee);

#ifdef CRLF_MAPPING
  if ( c == '\n' && (s->flags&SIO_TEXT) )
  { if ( Sputcode('\r', s) < 0 )
      return -1;
  }
#endif

  switch(s->encoding)
  { case ENC_OCTET:
    case ENC_ISO_LATIN_1:
      if ( c >= 256 )
      { if ( reperror(c, s) < 0 )
	  return -1;
	break;
      }
    simple:
      if ( put_byte(c, s) < 0 )
	return -1;
      break;
    case ENC_ASCII:
      if ( c >= 128 )
      { if ( reperror(c, s) < 0 )
	  return -1;
	break;
      }
      goto simple;
    case ENC_ANSI:
    { char b[MB_LEN_MAX];
      int n;

      if ( !s->mbstate )
      { if ( !(s->mbstate = malloc(sizeof(*s->mbstate))) )
	  return EOF;			/* out of memory */
	memset(s->mbstate, 0, sizeof(*s->mbstate));
      }

      if ( (n = wcrtomb(b, (wchar_t)c, s->mbstate)) < 0 )
      { if ( reperror(c, s) < 0 )
	  return -1;
      } else
      { int i;

	for(i=0; i<n; i++)
	{ if ( put_byte(b[i]&0xff, s) < 0 )
	    return -1;
	}
      }

      break;
    }
    case ENC_UTF8:
    { char buf[6];
      char *p, *end;
      
      if ( c < 128 )
	goto simple;

      end = utf8_put_char(buf, c);
      for(p=buf; p<end; p++)
      { if ( put_byte(*p&0xff, s) < 0 )
	  return -1;
      }

      break;
    }
    case ENC_UNICODE_BE:
      if ( put_byte(c>>8, s) < 0 )
	return -1;
      if ( put_byte(c&0xff, s) < 0 )
	return -1;
      break;
    case ENC_UNICODE_LE:
      if ( put_byte(c&0xff, s) < 0 )
	return -1;
      if ( put_byte(c>>8, s) < 0 )
	return -1;
      break;
    case ENC_WCHAR:
    { pl_wchar_t chr = c;
      unsigned char *q = (unsigned char *)&chr;
      unsigned char *e = &q[sizeof(pl_wchar_t)];

      while(q<e)
      { if ( put_byte(*q++, s) < 0 )
	  return -1;
      }
      
      break;
    }
    case ENC_UNKNOWN:
      return -1;
  }


  s->lastc = c;

  if ( c == '\n' && (s->flags & SIO_LBUF) )
  { if ( S__flushbuf(s) < 0 )
      return -1;
  }

  return S__updatefilepos(s, c);
}


		 /*******************************
		 *               FLAGS		*
		 *******************************/

int
Sfeof(IOSTREAM *s)
{ if ( s->flags & SIO_FEOF )
    return TRUE;

  if ( s->bufp < s->limitp )
    return FALSE;

  if ( s->flags & SIO_NBUF )
  { errno = EINVAL;
    return -1;
  }

  if ( S__fillbuf(s) == -1 )
    return TRUE;

  s->bufp--;
  return FALSE;
}
    

int
Sferror(IOSTREAM *s)
{ return (s->flags & SIO_FERR) != 0;
}
    

int
Sfpasteof(IOSTREAM *s)
{ return (s->flags & (SIO_FEOF2ERR|SIO_FEOF2)) == (SIO_FEOF2ERR|SIO_FEOF2);
}


void
Sclearerr(IOSTREAM *s)
{ s->flags &= ~(SIO_FEOF|SIO_WARN|SIO_FERR|SIO_FEOF2|SIO_TIMEOUT|SIO_CLEARERR);
  Sseterr(s, 0, NULL);
}


void
Sseterr(IOSTREAM *s, int flag, const char *message)
{ if ( s->message )
  { free(s->message);
    s->message = NULL;
    s->flags &= ~SIO_CLEARERR;
  }
  if ( message )
  { s->flags |= flag;
    s->message = strdup(message);
  } else
  { s->flags &= ~flag;
  }
}



void Yap_swi_install(void);

void
Yap_swi_install(void)
{
  YAP_UserCPredicate("ctime", SWI_ctime, 2);
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

