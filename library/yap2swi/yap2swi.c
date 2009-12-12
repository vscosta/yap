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


#include	<Yap.h>
#include	<Yatom.h>
#include	<eval.h>

#if HAVE_MATH_H
#include	<math.h>
#endif
#if HAVE_ERRNO_H
#include	<errno.h>
#endif

#define PL_KERNEL 1

#include	<SWI-Stream.h>
#include	<SWI-Prolog.h>

#ifdef USE_GMP
#include <gmp.h>
#endif

#ifdef __WIN32__
/* Windows */
#include <fcntl.h>
#endif

#define BUF_SIZE 256
#define TMP_BUF_SIZE 2*BUF_SIZE
#define BUF_RINGS 16

/* Required by PL_error */
#define ERR_NO_ERROR		0
#define ERR_INSTANTIATION	1	/* void */
#define ERR_TYPE		2	/* atom_t expected, term_t value */
#define ERR_DOMAIN		3	/* atom_t domain, term_t value */
#define ERR_REPRESENTATION	4	/* atom_t what */
#define ERR_MODIFY_STATIC_PROC	5	/* predicate_t proc */
#define ERR_EVALUATION		6	/* atom_t what */
#define ERR_AR_TYPE		7	/* atom_t expected, Number value */
#define ERR_NOT_EVALUABLE	8	/* functor_t func */
#define ERR_DIV_BY_ZERO		9	/* void */
#define ERR_FAILED	       10	/* predicate_t proc */
#define ERR_FILE_OPERATION     11	/* atom_t action, atom_t type, term_t */
#define ERR_PERMISSION	       12	/* atom_t type, atom_t op, term_t obj*/
#define ERR_NOT_IMPLEMENTED 13	/* const char *what */
#define ERR_EXISTENCE	       14	/* atom_t type, term_t obj */
#define ERR_STREAM_OP	       15	/* atom_t action, term_t obj */
#define ERR_RESOURCE	       16	/* atom_t resource */
#define ERR_NOMEM	       17	/* void */
#define ERR_SYSCALL	       18	/* void */
#define ERR_SHELL_FAILED       19	/* term_t command */
#define ERR_SHELL_SIGNALLED    20	/* term_t command, int signal */
#define ERR_AR_UNDEF	       21	/* void */
#define ERR_AR_OVERFLOW	       22	/* void */
#define ERR_AR_UNDERFLOW       23	/* void */
#define ERR_UNDEFINED_PROC     24	/* Definition def */
#define ERR_SIGNALLED	       25	/* int sig, char *name */
#define ERR_CLOSED_STREAM      26	/* IOSTREAM * */
#define ERR_BUSY	       27	/* mutexes */
#define ERR_PERMISSION_PROC    28	/* op, type, Definition */
#define ERR_DDE_OP	       29	/* op, error */
#define ERR_SYNTAX	       30	/* what */
#define ERR_SHARED_OBJECT_OP   31	/* op, error */
#define ERR_TIMEOUT	       32	/* op, object */
#define ERR_NOT_IMPLEMENTED_PROC 33	/* name, arity */
#define ERR_FORMAT	       34	/* message */
#define ERR_FORMAT_ARG	       35	/* seq, term */
#define ERR_OCCURS_CHECK       36	/* Word, Word */
#define ERR_CHARS_TYPE	       37	/* char *, term */
#define ERR_MUST_BE_VAR	       38	/* int argn, term_t term */

static inline atom_t
AtomToSWIAtom(Atom at)
{
  return (atom_t)at;
}

static inline Atom
SWIAtomToAtom(atom_t at)
{
  return (Atom)at;
}

static inline functor_t
FunctorToSWIFunctor(Functor at)
{
  return (functor_t)at;
}

static inline Functor
SWIFunctorToFunctor(functor_t at)
{
  return (Functor)at;
}

static void
PredicateInfo(void *p, Atom* a, unsigned long int* arity, Term* m)
{
  PredEntry *pd = (PredEntry *)p;
  if (pd->ArityOfPE) {
    *arity = pd->ArityOfPE;
    *a = NameOfFunctor(pd->FunctorOfPred);
  } else {
    *arity = 0;
    *a = (Atom)(pd->FunctorOfPred);
  }
  if (pd->ModuleOfPred)
    *m = pd->ModuleOfPred;
  else
    *m = TermProlog;
} 

static void
UserCPredicate(char *a, CPredicate def, unsigned long int arity, Term mod, int flags)
{
  PredEntry *pe;
  Term cm = CurrentModule;
  CurrentModule = mod;
  Yap_InitCPred(a, arity, def, UserCPredFlag);
  if (arity == 0) {
    pe = RepPredProp(PredPropByAtom(Yap_LookupAtom(a),mod));
  } else {
    Functor f = Yap_MkFunctor(Yap_LookupAtom(a), arity);
    pe = RepPredProp(PredPropByFunc(f,mod));
  }
  pe->PredFlags |= (CArgsPredFlag|flags);
  CurrentModule = cm;
} 

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
  return AtomName(SWIAtomToAtom(a));
}

X_API int
PL_chars_to_term(term_t term,const char *s) { 
  YAP_Term t,error;
  if ( (t=YAP_ReadBuffer(s,&error))==0L ) {
    Yap_PutInSlot(term, error); 
    return 0L;
  }
  Yap_PutInSlot(term,t);
  return 1L;
}


/* SWI: term_t PL_copy_term_ref(term_t from)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API term_t PL_copy_term_ref(term_t from)
{
  return YAP_InitSlot(Yap_GetFromSlot(from));
}

X_API term_t PL_new_term_ref(void)
{
  
  term_t to = Yap_NewSlots(1);
  return to;
}

X_API term_t PL_new_term_refs(int n)
{
  
  term_t to = Yap_NewSlots(n);
  return to;
}

X_API void PL_reset_term_refs(term_t after)
{
  term_t new = Yap_NewSlots(1);
  YAP_RecoverSlots(after-new);
}

/* begin PL_get_* functions =============================*/

/* SWI: int PL_get_arg(int index, term_t t, term_t a)
   YAP: YAP_Term YAP_ArgOfTerm(int argno, YAP_Term t)*/
X_API int PL_get_arg(int index, term_t ts, term_t a)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  if ( !YAP_IsApplTerm(t) ) {
    if (YAP_IsPairTerm(t)) {
      if (index == 1){
	Yap_PutInSlot(a,YAP_HeadOfTerm(t));
	return 1;
      } else if (index == 2) {
	Yap_PutInSlot(a,YAP_TailOfTerm(t));
	return 1;
      }
    }
    return 0;
  }
  Yap_PutInSlot(a,YAP_ArgOfTerm(index, t));
  return 1;
}
   
/* SWI: int PL_get_atom(term_t t, YAP_Atom *a)
   YAP: YAP_Atom YAP_AtomOfTerm(Term) */
X_API int PL_get_atom(term_t ts, atom_t *a)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  if ( !IsAtomTerm(t))
    return 0;
  *a = AtomToSWIAtom(AtomOfTerm(t));
  return 1;
}

/* SWI: int PL_get_atom_chars(term_t t, char **s)
   YAP: char* AtomName(Atom) */
X_API int PL_get_atom_chars(term_t ts, char **a)  /* SAM check type */
{
  YAP_Term t = Yap_GetFromSlot(ts);
  if (!IsAtomTerm(t))
    return 0;
  *a = RepAtom(AtomOfTerm(t))->StrOfAE;
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
  if (t != TermNil)
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
  YAP_Term t = Yap_GetFromSlot(l);
  char *tmp;

  if (!(flags & BUF_RING)) {
    tmp = alloc_ring_buf();
  } else {
    tmp = buffers;
  }
  *sp = tmp;
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    if (!(flags & (CVT_ATOM|CVT_ATOMIC|CVT_ALL)))
      return 0;
    if (IsWideAtom(at))
      /* will this always work? */
      snprintf(*sp,BUF_SIZE,"%ls",RepAtom(at)->WStrOfAE);
    else
      *sp = RepAtom(at)->StrOfAE;
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
  Term t = Yap_GetFromSlot(l);

  if (IsVarTerm(t)) {
    if (flags & CVT_EXCEPTION)
      YAP_Error(0, 0L, "PL_get_wchars");
    return 0;
  }
  if (flags & CVT_ATOM) {
    if (IsAtomTerm(t)) {
      Atom at = AtomOfTerm(t);

      if (!(flags & (CVT_ATOM|CVT_ATOMIC|CVT_ALL)))
	return 0;
      if (IsWideAtom(at)) {
	/* will this always work? */
	*wsp = RepAtom(at)->WStrOfAE;
      } else {
	char *sp = RepAtom(at)->StrOfAE;
	size_t sz;

	sz = strlen(sp);
	if (flags & BUF_MALLOC) {
	  int i;
	  wchar_t *nbf = (wchar_t *)YAP_AllocSpaceFromYap((sz+1)*sizeof(wchar_t));
	  if (nbf == NULL) {
	    if (flags & CVT_EXCEPTION)
	      YAP_Error(0, 0L, "PL_get_wchars: lack of memory");
	    return 0;
	  }
	  *wsp = nbf;
	  for (i=0; i<= sz; i++)
	    *nbf++ = *sp++;
	} else if (flags & BUF_DISCARDABLE) {
	  wchar_t *buf = (wchar_t *)buffers;
	  int i;
	
	  if ((sz+1)*sizeof(wchar_t) >= BUF_SIZE) {
	    if (flags & CVT_EXCEPTION)
	      YAP_Error(0, 0L, "PL_get_wchars: wcstombs");
	    return 0;
	  }
	  *wsp = buf;
	  for (i=0; i<= sz; i++)
	    *buf++ = *sp++;
	} else {
	  wchar_t *tmp = (wchar_t *)alloc_ring_buf();
	  int i;

	  if ((sz+1)*sizeof(wchar_t) >= BUF_SIZE) {
	    if (flags & CVT_EXCEPTION)
	      YAP_Error(0, 0L, "PL_get_wchars: wcstombs");
	    return 0;
	  }
	  *wsp = tmp;
	  for (i=0; i<= sz; i++)
	    *tmp++ = *sp++;
	}
	return 1;
      }
    }
  }
  if (flags & CVT_EXCEPTION)
    YAP_Error(0, 0L, "PL_get_wchars");
  return 0;
}


/* SWI: int PL_get_functor(term_t t, functor_t *f)
   YAP: YAP_Functor YAP_FunctorOfTerm(Term) */
X_API int PL_get_functor(term_t ts, functor_t *f)
{
  Term t = Yap_GetFromSlot(ts);
  if ( IsAtomTerm(t)) {
    *f = t;
  } else {
    *f = FunctorToSWIFunctor(FunctorOfTerm(t));
  }
  return 1;
}

/* SWI: int PL_get_float(term_t t, double *f)
   YAP: double YAP_FloatOfTerm(Term) */
X_API int PL_get_float(term_t ts, double *f) /* SAM type check*/
{	
  YAP_Term t = Yap_GetFromSlot(ts);
  if ( !YAP_IsFloatTerm(t))
    return 0;
  *f = YAP_FloatOfTerm(t);
  return 1;
}

X_API int PL_get_head(term_t ts, term_t h)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  if (!YAP_IsPairTerm(t) ) {
    return 0;
  }
  Yap_PutInSlot(h,YAP_HeadOfTerm(t));
  return 1;
}

/* SWI: int PL_get_integer(term_t t, int *i)
   YAP: long int  YAP_IntOfTerm(Term) */
X_API int PL_get_integer(term_t ts, int *i)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  if (!YAP_IsIntTerm(t) )
    return 0;
  *i = YAP_IntOfTerm(t);
  return 1;
}

/* SWI: int PL_get_bool(term_t t, int *i)
   YAP: long int  YAP_AtomOfTerm(Term) */
X_API int PL_get_bool(term_t ts, int *i)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  Atom at;

  if (!IsAtomTerm(t) )
    return 0;
  at = AtomOfTerm(t);
  if (at == AtomTrue) {
    *i = TRUE;
    return 1;    
  }
  if (at == AtomFalse) {
    *i = FALSE;
    return 1;    
  }
  return 0;
}

X_API int PL_get_long(term_t ts, long *i)
{
  YAP_Term t = Yap_GetFromSlot(ts);
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
#if SIZE_OF_LONG_INT==8
  return PL_get_long(ts, (long *)i);
#else
  YAP_Term t = Yap_GetFromSlot(ts);
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
#endif
}


X_API int PL_get_list(term_t ts, term_t h, term_t tl)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  if (!YAP_IsPairTerm(t) ) {
    return 0;
  }
  Yap_PutInSlot(h,YAP_HeadOfTerm(t));
  Yap_PutInSlot(tl,YAP_TailOfTerm(t));
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
  YAP_Term t = Yap_GetFromSlot(ts);
  if (!IsAtomTerm(t) )
    return FALSE;
  *m = (module_t)t;
  return TRUE;
}

/* SWI: int PL_new_module(term_t t, module_t *m) */
X_API module_t PL_new_module(atom_t swiat)
{
  Atom at = SWIAtomToAtom(swiat);
  Term t;

  WRITE_LOCK(RepAtom(at)->ARWLock);  
  t = Yap_Module(MkAtomTerm(at));
  WRITE_UNLOCK(RepAtom(at)->ARWLock);    
  return (module_t)t;
}

/* SWI: int PL_get_atom(term_t t, YAP_Atom *a)
   YAP: YAP_Atom YAP_AtomOfTerm(Term) */
X_API int PL_get_name_arity(term_t ts, atom_t *name, int *arity)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  if (IsAtomTerm(t)) {
    *name = AtomToSWIAtom(AtomOfTerm(t));
    *arity = 0;
    return 1;
  }
  if (YAP_IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    *name = AtomToSWIAtom(NameOfFunctor(f));
    *arity = ArityOfFunctor(f);
    return 1;
  }
  if (YAP_IsPairTerm(t)) {
    *name = AtomToSWIAtom(AtomDot);
    *arity = 2;
    return 1;
  }
  return 0;
}

/* SWI: int PL_get_atom(term_t t, YAP_Atom *a)
   YAP: YAP_Atom YAP_AtomOfTerm(Term) */
X_API int PL_get_nil(term_t ts)
{
  Term t = Yap_GetFromSlot(ts);
  return ( t == TermNil );
}

/* SWI: int PL_get_pointer(term_t t, int *i)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API int PL_get_pointer(term_t ts, void **i)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  if (!YAP_IsIntTerm(t) )
    return 0;
  *i = (void *)YAP_IntOfTerm(t);
  return 1;
}

/* SWI: int PL_get_atom_chars(term_t t, char **s)
   YAP: char* AtomName(Atom) */
X_API int PL_get_string(term_t ts, char **sp, int *lenp)  /* SAM check type */
{
  YAP_Term t = Yap_GetFromSlot(ts);
  char *to;
  int len;
  if (!YAP_IsPairTerm(t))
    return 0;
  if (!YAP_StringToBuffer(t, buffers, TMP_BUF_SIZE))
      return(FALSE);
  len = strlen(buffers);
  to = (char *)Yap_NewSlots((len/sizeof(YAP_Term))+1);
  strncpy(to, buffers, TMP_BUF_SIZE);
  *sp = to;
  return 1;
}

X_API int PL_get_tail(term_t ts, term_t tl)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  if (!YAP_IsPairTerm(t) ) {
    return 0;
  }
  Yap_PutInSlot(tl,YAP_TailOfTerm(t));
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
  return AtomToSWIAtom(Yap_LookupAtom((char *)c));
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
    at = AtomToSWIAtom(Yap_LookupMaybeWideAtom(nbf));
    YAP_FreeSpaceFromYap(nbf);
  } else {
    char *nbf = (char *)YAP_AllocSpaceFromYap((len+1)*sizeof(char));
    for (i=0;i<len;i++)
      nbf[i] = c[i];
    nbf[len]='\0';
    at = AtomToSWIAtom(Yap_LookupAtom(nbf));
    YAP_FreeSpaceFromYap(nbf);
  }
  return at;
}

X_API char *PL_atom_nchars(atom_t name, size_t *sp)
{
  Atom at = SWIAtomToAtom(name);
  if (IsWideAtom(at)) {
    wchar_t *c = RepAtom(at)->WStrOfAE;

    *sp = wcslen(c);
    return (char *)c;
  } else {
    char *c = RepAtom(at)->StrOfAE;

    *sp = strlen(c);
    return c;
  }
}

X_API wchar_t *PL_atom_wchars(atom_t name, size_t *sp)
{
  Atom at = SWIAtomToAtom(name);
  if (!IsWideAtom(at))
    return NULL;
  *sp = wcslen(RepAtom(at)->WStrOfAE);
  return RepAtom(at)->WStrOfAE;
}

X_API functor_t PL_new_functor(atom_t name, int arity)
{
  functor_t f;
  Atom at = SWIAtomToAtom(name);
  if (arity == 0) {
    f = FunctorToSWIFunctor((Functor)MkAtomTerm(at));
  } else {
    f = FunctorToSWIFunctor(Yap_MkFunctor(at,arity));
  }
  return f;
}

X_API atom_t PL_functor_name(functor_t f)
{
  if (IsAtomTerm(f)) {
    return AtomToSWIAtom(AtomOfTerm((Term)SWIFunctorToFunctor(f)));
  } else {
    return AtomToSWIAtom(NameOfFunctor(SWIFunctorToFunctor(f)));
  }
}

X_API int PL_functor_arity(functor_t f)
{
  if (IsAtomTerm(f)) {
    return 0;
  } else {
    return ArityOfFunctor(SWIFunctorToFunctor(f));
  }
}

/* end PL_new_* functions =============================*/

/* begin PL_put_* functions =============================*/

X_API void PL_cons_functor(term_t d, functor_t f,...)
{
  va_list ap;
  int arity, i;
  YAP_Term *tmp = (YAP_CELL *)buffers;
  Functor ff = SWIFunctorToFunctor(f);

  if (IsAtomTerm((Term)ff)) {
    Yap_PutInSlot(d, (YAP_Term)f);
    return;
  }
  arity = ArityOfFunctor(ff);
  if (arity > TMP_BUF_SIZE/sizeof(YAP_CELL)) {
    fprintf(stderr,"PL_cons_functor: arity too large (%d)\n", arity); 
    return;
  }
  va_start (ap, f);
  for (i = 0; i < arity; i++) {
    tmp[i] =  Yap_GetFromSlot(va_arg(ap, term_t));
  }
  va_end (ap);
  if (arity == 2 && ff == FunctorDot)
    Yap_PutInSlot(d,YAP_MkPairTerm(tmp[0],tmp[1]));
  else
    Yap_PutInSlot(d,YAP_MkApplTerm((YAP_Functor)ff,arity,tmp));
}

X_API void PL_cons_functor_v(term_t d, functor_t f,term_t a0)
{
  int arity;
  Functor ff = SWIFunctorToFunctor(f);

  if (IsAtomTerm((Term)ff)) {
    Yap_PutInSlot(d,(Term)ff);
    return;
  }
  arity = ArityOfFunctor(ff);
  if (arity == 2 && ff == FunctorDot)
    Yap_PutInSlot(d,YAP_MkPairTerm(Yap_GetFromSlot(a0),Yap_GetFromSlot(a0+1)));    
  else
    Yap_PutInSlot(d,YAP_MkApplTerm((YAP_Functor)ff,arity,YAP_AddressFromSlot(a0)));
}

X_API void PL_cons_list(term_t d, term_t h, term_t t)
{
  Yap_PutInSlot(d,YAP_MkPairTerm(Yap_GetFromSlot(h),Yap_GetFromSlot(t)));
}

X_API void PL_put_atom(term_t t, atom_t a)
{
  Yap_PutInSlot(t,MkAtomTerm(SWIAtomToAtom(a)));
}

X_API void PL_put_atom_chars(term_t t, const char *s)
{
  Yap_PutInSlot(t,MkAtomTerm(Yap_LookupAtom((char *)s)));
}

X_API void PL_put_float(term_t t, double fl)
{
  Yap_PutInSlot(t,YAP_MkFloatTerm(fl));
}

X_API void PL_put_functor(term_t t, functor_t f)
{
  long int  arity;
  Functor ff = SWIFunctorToFunctor(f);

  if (IsAtomTerm((Term)ff)) {
    Yap_PutInSlot(t,(Term)ff);
  } else {
    arity = ArityOfFunctor(ff);
    if (arity == 2 && ff == FunctorDot)
      Yap_PutInSlot(t,YAP_MkNewPairTerm());    
    else
      Yap_PutInSlot(t,YAP_MkNewApplTerm((YAP_Functor)ff,arity));
  }
}

X_API void PL_put_integer(term_t t, long n)
{
  Yap_PutInSlot(t,YAP_MkIntTerm(n));
}

X_API void PL_put_int64(term_t t, int64_t n)
{
#if USE_GMP
  char s[64];
  MP_INT rop;

  sprintf(s, "%lld", (long long int)n);
  mpz_init_set_str (&rop, s, 10);
  Yap_PutInSlot(t,YAP_MkBigNumTerm((void *)&rop));
#endif
}

X_API void PL_put_list(term_t t)
{
  Yap_PutInSlot(t,YAP_MkNewPairTerm());
}

X_API void PL_put_list_chars(term_t t, const char *s)
{
  Yap_PutInSlot(t,YAP_BufferToString((char *)s));
}

X_API void PL_put_nil(term_t t)
{
  Yap_PutInSlot(t,TermNil);
}

/* SWI: void PL_put_pointer(term_t -t, void *ptr)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API void PL_put_pointer(term_t t, void *ptr)
{
  YAP_Term tptr = YAP_MkIntTerm((long int)ptr);
  Yap_PutInSlot(t,tptr);
}

X_API void PL_put_string_chars(term_t t, const char *s)
{
  Yap_PutInSlot(t,YAP_BufferToString((char *)s));
}

X_API void PL_put_term(term_t d, term_t s)
{
  Yap_PutInSlot(d,Yap_GetFromSlot(s));
}

X_API void PL_put_variable(term_t t)
{
  Yap_PutInSlot(t,YAP_MkVarTerm());
}

/* end PL_put_* functions =============================*/

/* SWI: int PL_raise_exception(term_t exception)
   YAP: NO EQUIVALENT */
/* SAM TO DO */

X_API int PL_raise_exception(term_t exception)
{
  YAP_Throw(Yap_GetFromSlot(exception));
  return 0;
}

#define ATOM_instantiation_error AtomToSWIAtom(AtomInstantiationError)
#define ATOM_max_files AtomToSWIAtom(AtomMaxFiles)
#define ATOM_no_memory AtomToSWIAtom(AtomNoMemory)
#define ATOM_procedure AtomToSWIAtom(AtomProcedure)
#define ATOM_system_error AtomToSWIAtom(AtomSystemError)
#define ATOM_variable AtomToSWIAtom(AtomVariable)
#define FUNCTOR_error2 FunctorToSWIFunctor(FunctorError)
#define FUNCTOR_context2 FunctorToSWIFunctor(FunctorContext2)
#define FUNCTOR_divide2 FunctorToSWIFunctor(FunctorSlash)
#define FUNCTOR_domain_error2 FunctorToSWIFunctor(FunctorDomainError)
#define FUNCTOR_existence_error2 FunctorToSWIFunctor(FunctorExistenceError)
#define FUNCTOR_evaluation_error1 FunctorToSWIFunctor(FunctorEvaluationError)
#define FUNCTOR_not_implemented2 FunctorToSWIFunctor(FunctorNotImplemented)
#define FUNCTOR_permission_error3 FunctorToSWIFunctor(FunctorPermissionError)
#define FUNCTOR_representation_error1 FunctorToSWIFunctor(FunctorRepresentationError)
#define FUNCTOR_resource_error1 FunctorToSWIFunctor(FunctorResourceError)
#define FUNCTOR_timeout_error2 FunctorToSWIFunctor(FunctorTimeoutError)
#define FUNCTOR_type_error2 FunctorToSWIFunctor(FunctorTypeError)


static char *
OsError(void)
{
#ifdef HAVE_STRERROR
#ifdef __WINDOWS__
  return NULL;
#else
  return strerror(errno);
#endif
#else /*HAVE_STRERROR*/
static char errmsg[64];

#ifdef __unix__
  extern int sys_nerr;
#if !EMX
  extern char *sys_errlist[];
#endif
  extern int errno;

  if ( errno < sys_nerr )
    return sys_errlist[errno];
#endif

  Ssprintf(errmsg, "Unknown Error (%d)", errno);
  return errmsg;
#endif /*HAVE_STRERROR*/
}

X_API int PL_warning(const char *msg, ...) {
  va_list args;
  va_start(args, msg);
  // just print the warning message and return? 
  fprintf(stderr,"[Warning:");
  fprintf(stderr,msg,args);
  fprintf(stderr,"]\n");
  va_end(args);
  PL_fail;
}

X_API int PL_error(const char *pred, int arity, const char *msg, int id, ...)
{
  term_t formal, swi, predterm, msgterm, except;
  va_list args;

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
	  PL_unify_atom(formal, ATOM_system_error);
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
      
  default:
    fprintf(stderr, "unimplemented SWI error %d\n",id);
    goto err_instantiation;
  }
  va_end(args);
  if ( pred )
    { PL_unify_term(predterm,
		    PL_FUNCTOR, FUNCTOR_divide2,
		    PL_CHARS, pred,
		    PL_INT, arity);
    }   
  if ( msg )
    {
      PL_put_atom_chars(msgterm, msg);
    }
  PL_unify_term(swi,
		PL_FUNCTOR, FUNCTOR_context2,
		PL_TERM, predterm,
		PL_TERM, msgterm);
  PL_unify_term(except,
		PL_FUNCTOR, FUNCTOR_error2,
		PL_TERM, formal,
		PL_TERM, swi);
  return PL_raise_exception(except);
}

/* begin PL_unify_* functions =============================*/

X_API int PL_unify(term_t t1, term_t t2)
{
  return YAP_Unify(Yap_GetFromSlot(t1),Yap_GetFromSlot(t2));
}

/* SWI: int PL_unify_atom(term_t ?t, atom  *at)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_atom(term_t t, atom_t at)
{
  YAP_Term cterm = MkAtomTerm(SWIAtomToAtom(at));
  return YAP_Unify(Yap_GetFromSlot(t),cterm);
}

/* SWI: int PL_unify_atom_chars(term_t ?t, const char *chars)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_atom_chars(term_t t, const char *s)
{
  Atom catom = Yap_LookupAtom((char *)s);
  Term cterm = MkAtomTerm(catom);
  return Yap_unify(Yap_GetFromSlot(t),cterm);
}

/* SWI: int PL_unify_atom_chars(term_t ?t, const char *chars)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_atom_nchars(term_t t, size_t len, const char *s)
{
  Atom catom;
  YAP_Term cterm;
  char *buf = (char *)YAP_AllocSpaceFromYap(len+1);

  if (!buf)
    return FALSE;
  strncpy(buf, s, len);
  buf[len] = '\0';
  catom = Yap_LookupAtom(buf);
  free(buf);
  cterm = MkAtomTerm(catom);
  return YAP_Unify(Yap_GetFromSlot(t),cterm);
}

/* SWI: int PL_unify_float(term_t ?t, double f)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_float(term_t t, double f)
{
  YAP_Term fterm = YAP_MkFloatTerm(f);
  return YAP_Unify(Yap_GetFromSlot(t),fterm);
}

/* SWI: int PL_unify_integer(term_t ?t, long n)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_integer(term_t t, long n)
{	
  YAP_Term iterm = YAP_MkIntTerm(n);
  return YAP_Unify(Yap_GetFromSlot(t),iterm);
}

/* SWI: int PL_unify_integer(term_t ?t, long n)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_functor(term_t t, functor_t f)
{	
  YAP_Term tt = Yap_GetFromSlot(t);
  Functor ff = SWIFunctorToFunctor(f);
  if (YAP_IsVarTerm(tt))
    return YAP_Unify(tt, YAP_MkNewApplTerm((YAP_Functor)ff,YAP_ArityOfFunctor((YAP_Functor)f)));
  if (YAP_IsPairTerm(tt))
    return ff == FunctorDot;
  if (!YAP_IsApplTerm(tt))
    return FALSE;
  return ff == FunctorOfTerm(tt);
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
  return YAP_Unify(Yap_GetFromSlot(t),iterm);
#else
  return FALSE;
#endif
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_list(term_t tt, term_t h, term_t tail)
{
  Term t = Deref(Yap_GetFromSlot(tt));
  if (IsVarTerm(t)) {
    Term pairterm = Yap_MkNewPairTerm();
    Yap_unify(t, pairterm);
    /* avoid calling deref */
    t = pairterm;
  } else if (!IsPairTerm(t)) {
    return FALSE;
  }
  Yap_PutInSlot(h,HeadOfTerm(t));
  Yap_PutInSlot(tail,TailOfTerm(t));
  return TRUE;
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_list_chars(term_t t, const char *chars)
{
  YAP_Term chterm = YAP_BufferToString((char *)chars);
  return YAP_Unify(Yap_GetFromSlot(t), chterm);
}

/* SWI: int PL_unify_nil(term_t ?l)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_nil(term_t l)
{
  YAP_Term nilterm = TermNil;
  return YAP_Unify(Yap_GetFromSlot(l), nilterm);
}

/* SWI: int PL_unify_pointer(term_t ?t, void *ptr)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API int PL_unify_pointer(term_t t, void *ptr)
{
  YAP_Term ptrterm = YAP_MkIntTerm((long int)ptr);
  return YAP_Unify(Yap_GetFromSlot(t), ptrterm);
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_string_chars(term_t t, const char *chars)
{
  YAP_Term chterm = YAP_BufferToString((char *)chars);
  return YAP_Unify(Yap_GetFromSlot(t), chterm);
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
    chterm = MkAtomTerm(Yap_LookupMaybeWideAtom((wchar_t *)chars));
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
  return YAP_Unify(Yap_GetFromSlot(t), chterm);
}

/* SWI: int PL_unify_wchars(term_t ?t, int type, size_t len,, const pl_wchar_t *s)
 */
X_API int PL_unify_wchars_diff(term_t t, term_t tail, int type, size_t len, const pl_wchar_t *chars)
{
  YAP_Term chterm;

  if (len == (size_t)-1)
    len = wcslen(chars);

  switch (type) {
  case PL_STRING:
  case PL_CODE_LIST:
    chterm = YAP_NWideBufferToDiffList(chars, Yap_GetFromSlot(tail), len);
    break;
  case PL_CHAR_LIST:
    chterm = YAP_NWideBufferToAtomDiffList(chars, Yap_GetFromSlot(tail), len);
    break;
  default:
    /* should give error?? */
    return FALSE;
  }
  return YAP_Unify(Yap_GetFromSlot(t), chterm);
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

static Atom
LookupMaxAtom(size_t n, char *s)
{
  Atom catom;
  char *buf = (char *)Yap_AllocCodeSpace(n+1);
  
  if (!buf)
    return FALSE;
  strncpy(buf, s, n);
  buf[n] = '\0';
  catom = Yap_LookupAtom(buf);
  Yap_FreeCodeSpace(buf);
  return catom;
}

static Atom
LookupMaxWideAtom(size_t n, wchar_t *s)
{
  Atom catom;
  wchar_t *buf = (wchar_t *)Yap_AllocCodeSpace((n+1)*sizeof(wchar_t));
  
  if (!buf)
    return FALSE;
  wcsncpy(buf, s, n);
  buf[n] = '\0';
  catom = Yap_LookupMaybeWideAtom(buf);
  Yap_FreeAtomSpace((ADDR)buf);
  return catom;
}

static YAP_Term
MkBoolTerm(int b)
{
  if (b)
    return MkAtomTerm(AtomTrue);
  else
    return MkAtomTerm(AtomFalse);
}

#define MAX_DEPTH 64

typedef struct {
  int nels;
  CELL *ptr;
} stack_el;

/* SWI: int PL_unify_term(term_t ?t1, term_t ?t2)
   YAP long int  YAP_Unify(YAP_Term* a, Term* b) */
X_API int PL_unify_term(term_t l,...)
{
  va_list ap;
  int type;
  int nels = 1;
  int depth = 1;
  Term a[1], *pt;
  stack_el stack[MAX_DEPTH];
  

  va_start (ap, l);
  pt = a;
  while (depth > 0) {
    while (nels > 0) {
      type = va_arg(ap, int);
      nels--;
      switch(type) {
      case PL_VARIABLE:
	*pt++ = MkVarTerm();
	break;
      case PL_BOOL:
	*pt++ = MkBoolTerm(va_arg(ap, int));
	break;
      case PL_ATOM:
	*pt++ = MkAtomTerm(SWIAtomToAtom(va_arg(ap, atom_t)));
	break;
      case PL_INTEGER:
	*pt++ = MkIntegerTerm(va_arg(ap, long));
	break;
      case PL_SHORT:
	*pt++ = MkIntegerTerm(va_arg(ap, int));
	break;
      case PL_INT:
	*pt++ = MkIntegerTerm(va_arg(ap, int));
	break;
      case PL_FLOAT:
	*pt++ = MkFloatTerm(va_arg(ap, double));
	break;
      case PL_STRING:
	*pt++ = YAP_BufferToString(va_arg(ap, char *));
	break;
      case PL_CHARS:
	*pt++ = MkAtomTerm(Yap_LookupAtom(va_arg(ap, char *)));
	break;
      case PL_NCHARS:
	{
	  size_t sz = va_arg(ap, size_t);
	  *pt++ = MkAtomTerm(LookupMaxAtom(sz,va_arg(ap, char *)));
	}
	break;
      case PL_NWCHARS:
	{
	  size_t sz = va_arg(ap, size_t);
	  *pt++ = MkAtomTerm(LookupMaxWideAtom(sz,va_arg(ap, wchar_t *)));
	}
	break;
      case PL_TERM:
	{
	  Term t = Yap_GetFromSlot(va_arg(ap, size_t));
	  if (IsVarTerm(t) && VarOfTerm(t) >= ASP && VarOfTerm(t) < LCL0) {
	    Yap_unify(*pt++, t);
	  }
	  else {
	    *pt++ = t;
	  }
	}
	break;
      case PL_POINTER:
	*pt++ = MkIntegerTerm((Int)va_arg(ap, void *));
	break;
      case PL_INT64:
#if SIZE_OF_LONG_INT==8
	*pt++ = MkIntegerTerm((Int)va_arg(ap, long int));
#elif USE_GMP
	{
	  char s[64];
	  MP_INT rop;

	  sprintf(s, "%lld", va_arg(ap, long long int));
	  mpz_init_set_str (&rop, s, 10);
	  *pt++ = YAP_MkBigNumTerm((void *)&rop);
	}
#else
	fprintf(stderr, "PL_unify_term: PL_int64 not supported\n");
	exit(1);
#endif
	break;
      case PL_FUNCTOR:
	{
	  functor_t f = va_arg(ap, functor_t);
	  Functor ff = SWIFunctorToFunctor(f);
	  UInt arity = ArityOfFunctor(ff);

	  if (!arity) {
	    *pt++ = MkAtomTerm((Atom)f);
	  } else {
	    Term t = Yap_MkNewApplTerm(ff, arity);
	    if (nels) {
	      if (depth == MAX_DEPTH) {
		fprintf(stderr,"ERROR: very deep term in PL_unify_term, change MAX_DEPTH from %d\n", MAX_DEPTH);
		return FALSE;
	      }
	      stack[depth-1].nels = nels;
	      stack[depth-1].ptr = pt+1;
	      depth++;
	    }
	    *pt = t;
	    if (ff == FunctorDot)
	      pt = RepPair(t);
	    else
	      pt = RepAppl(t)+1;
	    nels = arity;
	  }
	}
	break;
      case PL_FUNCTOR_CHARS:
	{
	  char *fname = va_arg(ap, char *);
	  size_t arity = va_arg(ap, size_t);

	  if (!arity) {
	    *pt++ = MkAtomTerm(Yap_LookupAtom(fname));
	  } else {
	    Functor ff = Yap_MkFunctor(Yap_LookupAtom(fname),arity);
	    Term t = Yap_MkNewApplTerm(ff, arity);

	    if (nels) {
	      if (depth == MAX_DEPTH) {
		fprintf(stderr,"very deep term in PL_unify_term\n");
		return FALSE;
	      }
	      stack[depth-1].nels = nels;
	      stack[depth-1].ptr = pt+1;
	      depth++;
	    }
	    *pt = t;
	    if (ff == FunctorDot)
	      pt = RepPair(t);
	    else
	      pt = RepAppl(t)+1;
	    nels = arity;
	  }
	}
	break;
      case PL_LIST:
	{
	  Term t = Yap_MkNewPairTerm();

	  if (nels) {
	    if (depth == MAX_DEPTH) {
	      fprintf(stderr,"very deep term in PL_unify_term\n");
	      return FALSE;
	    }
	    stack[depth-1].nels = nels;
	    stack[depth].ptr = pt+1;
	    depth++;
	  }
	  *pt = t;
	  pt = RepPair(t);
	  nels = 2;
	}
	break;
      default:
	fprintf(stderr, "PL_unify_term: %d not supported\n", type);
	exit(1);
      }
    }
    depth--;
    if (depth) {
      pt = stack[depth-1].ptr;
      nels = stack[depth-1].nels;
    }
  }
  va_end (ap);
  return YAP_Unify(Yap_GetFromSlot(l),a[0]);
}

/* end PL_unify_* functions =============================*/

/* SWI: void PL_register_atom(atom_t atom) */
/* SAM TO DO */
X_API void PL_register_atom(atom_t atom)
{
  Yap_AtomGetHold(SWIAtomToAtom(atom));
}

/* SWI: void PL_unregister_atom(atom_t atom) */
/* SAM TO DO */
X_API void PL_unregister_atom(atom_t atom)
{
  Yap_AtomReleaseHold(SWIAtomToAtom(atom));
}

X_API int PL_get_string_chars(term_t t, char **s, int *len)
{
  /* there are no such objects in Prolog */
  return FALSE;
}

X_API int PL_term_type(term_t t)
{
  /* YAP_ does not support strings as different objects */
  YAP_Term v = Yap_GetFromSlot(t);
  if (YAP_IsVarTerm(v)) {
    return PL_VARIABLE;
  } else if (IsAtomTerm(v)) {
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
  return IsAtomTerm(Yap_GetFromSlot(t));
}

X_API int PL_is_atomic(term_t ts)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  return !YAP_IsVarTerm(t) || !YAP_IsApplTerm(t) || !YAP_IsPairTerm(t);
}

X_API int PL_is_compound(term_t ts)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  return (YAP_IsApplTerm(t) || YAP_IsPairTerm(t));
}

X_API int PL_is_functor(term_t ts, functor_t f)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  Functor ff = SWIFunctorToFunctor(f);
  if (YAP_IsApplTerm(t)) {
    return FunctorOfTerm(t) == (Functor)ff;
  } else if (YAP_IsPairTerm(t)) {
    return FunctorOfTerm(t) == FunctorDot;
  } else
    return 0;
}

X_API int PL_is_float(term_t ts)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  return YAP_IsFloatTerm(t);
}

X_API int PL_is_integer(term_t ts)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  return YAP_IsIntTerm(t);
}

X_API int PL_is_list(term_t ts)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  return Yap_IsListTerm(t);
}

X_API int PL_is_number(term_t ts)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  return YAP_IsIntTerm(t) || YAP_IsFloatTerm(t);
}

X_API int PL_is_string(term_t ts)
{
  YAP_Term t = Yap_GetFromSlot(ts);
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
  if (t != TermNil)
    return 0;
  return FALSE;
}

X_API int PL_is_variable(term_t ts)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  return YAP_IsVarTerm(t);
}

X_API int PL_compare(term_t ts1, term_t ts2)
{
  YAP_Term t1 = Yap_GetFromSlot(ts1);
  YAP_Term t2 = Yap_GetFromSlot(ts2);
  return YAP_CompareTerms(t1, t2);
}

X_API record_t
PL_record(term_t ts)
{
  Term t = Yap_GetFromSlot(ts);
  return (record_t)Yap_StoreTermInDB(t, 0);
}

X_API void
PL_recorded(record_t db, term_t ts)
{
  Term t = Yap_FetchTermFromDB((DBTerm *)db);
  Yap_PutInSlot(ts,t);
}

X_API void
PL_erase(record_t db)
{
  Yap_ReleaseTermFromDB((DBTerm *)db);
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
      YAP_Throw(MkAtomTerm(Yap_LookupAtom("abort")));
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
PL_rewind_foreign_frame(fid_t f)
{
}

X_API void
PL_discard_foreign_frame(fid_t f)
{
  if (f)
    fprintf(stderr,"WARNING: PL_discard_foreign_frame not fully implemented!!\n");
  /* Missing: undo Trail!! */
}

X_API term_t
PL_exception(qid_t q)
{
  YAP_Term t;
  if (YAP_GoalHasException(&t)) {
    term_t to = Yap_NewSlots(1);
    Yap_PutInSlot(to,t);
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
  init_args.SavedState = "startup.yss";
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
  YAP_Term t =  YAP_StripModule(Yap_GetFromSlot(raw),(YAP_Term *)m);
  if (!t)
    return FALSE;
  Yap_PutInSlot(plain, t);
  return TRUE;
}

X_API atom_t PL_module_name(module_t m)
{
  Term t;
  Atom at = AtomOfTerm((Term)m);
  WRITE_LOCK(RepAtom(at)->ARWLock);  
  t = Yap_Module(MkAtomTerm(at));
  WRITE_UNLOCK(RepAtom(at)->ARWLock);  
  return AtomToSWIAtom(at);
}

X_API predicate_t PL_pred(functor_t f, module_t m)
{
  Functor ff = SWIFunctorToFunctor(f);
  if (IsAtomTerm((Term)f)) {
    return YAP_Predicate(YAP_AtomOfTerm((Term)f),0,(YAP_Module)m);
  } else {
    return YAP_Predicate((YAP_Atom)NameOfFunctor(ff),ArityOfFunctor(ff),(YAP_Module)m);
  }
}

X_API predicate_t PL_predicate(const char *name, int arity, const char *m)
{
  int mod;
  if (m == NULL)
    mod = YAP_CurrentModule();
  else
    mod = MkAtomTerm(Yap_LookupAtom((char *)m));
  return YAP_Predicate(YAP_LookupAtom((char *)name),
		       arity,
		       mod);
}

X_API void PL_predicate_info(predicate_t p,atom_t *name, int *arity, module_t *m)
{
  PredEntry *pd = (PredEntry *)p;
  Atom aname;

  if (pd->ArityOfPE) {
    *arity = pd->ArityOfPE;
    aname = NameOfFunctor(pd->FunctorOfPred);
  } else {
    *arity = 0;
    aname = (Atom)(pd->FunctorOfPred);
  }
  if (pd->ModuleOfPred)
    *m = (module_t)pd->ModuleOfPred;
  else
    *m = (module_t)TermProlog;
  *name = AtomToSWIAtom(aname);
}

typedef struct open_query_struct {
  int open;
  int state;
  YAP_Term g;
} open_query;

open_query execution;

X_API qid_t PL_open_query(module_t ctx, int flags, predicate_t p, term_t t0)
{
  Atom yname;
  unsigned long int  arity;
  Term  m;
  Term t[2];

  /* ignore flags  and module for now */
  if (execution.open != 0) {
    YAP_Error(0, 0L, "only one query at a time allowed\n");
  }
  execution.open=1;
  execution.state=0;
  PredicateInfo((PredEntry *)p, &yname, &arity, &m);
  t[0] = YAP_ModuleName(m);
  if (arity == 0) {
    t[1] = MkAtomTerm(yname);
  } else {
    Functor f = Yap_MkFunctor(yname, arity);
    t[1] = Yap_MkApplTerm(f,arity,Yap_AddressFromSlot(t0));
  }
  execution.g = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom(":"),2),2,t);
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

X_API int PL_toplevel(void)
{
  return YAP_RunGoal(MkAtomTerm(Yap_FullLookupAtom("$live")));
}

X_API int PL_call(term_t tp, module_t m)
{
  YAP_Term t[2], g;
  t[0] = YAP_ModuleName((YAP_Module)m);
  t[1] = Yap_GetFromSlot(tp);
  g = YAP_MkApplTerm(YAP_MkFunctor(YAP_LookupAtom(":"),2),2,t);
  return YAP_RunGoal(g);
}

X_API void PL_register_foreign_in_module(const char *module, const char *name, int arity, foreign_t (*function)(void), int flags)
{
  Term tmod;
  Int nflags = 0;

  if (flags & (PL_FA_NOTRACE|PL_FA_CREF)) {
    fprintf(stderr,"PL_register_foreign_in_module called with non-implemented flag %x when creating predicate %s:%s/%d\n", flags, module, name, arity);
  }      
  if (module == NULL) {
    tmod = CurrentModule;
  } else {
    tmod = MkAtomTerm(Yap_LookupAtom((char *)module));
  }
  if (flags & PL_FA_VARARGS) { 
    nflags = SWIEnvPredFlag;
  }
  if (flags & PL_FA_TRANSPARENT) {
    nflags |= ModuleTransparentPredFlag;
  } else {
    nflags |= CArgsPredFlag;
  }
  if (flags & PL_FA_NONDETERMINISTIC) {
    Yap_InitCPredBack((char *)name, arity, sizeof(struct foreign_context)/sizeof(CELL), (CPredicate)function, (CPredicate)function, UserCPredFlag|nflags);
  } else {
    UserCPredicate((char *)name,(CPredicate)function,arity,tmod,nflags);
  }
}

X_API void PL_register_extensions(const PL_extension *ptr)
{
  PL_load_extensions(ptr);
}

X_API void PL_load_extensions(const PL_extension *ptr)
{
  /* ignore flags for now */
  while(ptr->predicate_name != NULL) {
    PL_register_foreign_in_module(NULL, ptr->predicate_name, ptr->arity, ptr->function, ptr->flags);
    ptr++;
  }
}

X_API int PL_handle_signals(void)
{
  if (EX) {
    return -1;
  }
  fprintf(stderr,"PL_handle_signals not implemented\n");
  return 0;
}

X_API  int PL_is_inf(term_t st)
{
  Term t = Deref(Yap_GetFromSlot(st));
  Float fl;
  if (IsVarTerm(t)) return FALSE;
  if (!IsFloatTerm(t)) return FALSE;
  fl = FloatOfTerm(t);
#if HAVE_ISINF
  return isinf(fl);
#elif HAVE_FPCLASS
  return (fpclass(fl) == FP_NINF || fpclass(fl) == FP_PINF);
#else
  return FALSE;
#endif
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
  return (void *)Yap_AllocCodeSpace((long unsigned int)sz);
}

X_API void *
PL_realloc(void *ptr, int sz)
{
  return Yap_ReallocCodeSpace((char *)ptr,(long unsigned int)sz);
}

X_API void
PL_free(void *obj)
{
  return Yap_FreeCodeSpace((char *)obj);
}

X_API int
PL_eval_expression_to_int64_ex(term_t t, int64_t *val)
{
  Term res = Yap_Eval(Yap_GetFromSlot(t));
  if (!res) {
    return FALSE;
  }
  if (IsIntegerTerm(res)) {
    *val = IntegerOfTerm(res);
    return TRUE;
#if  SIZEOF_LONG_INT==4 && USE_GMP
  } else if (YAP_IsBigNumTerm(res)) {
    MP_INT g;
    char s[64];
    
    YAP_BigNumOfTerm(t, (void *)&g);
    if (mpz_sizeinbase(&g,2) > 64) {
      return PL_error(NULL,0,NULL, ERR_EVALUATION, AtomToSWIAtom(Yap_LookupAtom("int_overflow")));
    }
    mpz_get_str (s, 10, &g);
    sscanf(s, "%lld", (long long int *)val);
    return 1;
#endif
  }
  PL_error(NULL,0,NULL, ERR_TYPE, AtomToSWIAtom(Yap_LookupAtom("integer_expression")));
  return FALSE;
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

