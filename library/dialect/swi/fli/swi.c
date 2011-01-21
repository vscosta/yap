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
#include	<YapHeap.h>
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

#include	<yapio.h>

#ifdef USE_GMP
#include <gmp.h>
#endif

#ifdef __WIN32__
/* Windows */
#include <fcntl.h>
#endif

#include "swi.h"

extern X_API Int YAP_PLArityOfSWIFunctor(functor_t at);

/* This is silly, but let's keep it like that for now */
X_API Int
YAP_PLArityOfSWIFunctor(functor_t f) {
  if ((CELL)(f) & 2 && ((CELL)f) < N_SWI_FUNCTORS*4+2)
    return ArityOfFunctor(SWI_Functors[(CELL)f/4]);
  if (IsAtomTerm(f))
    return 0;
  return ArityOfFunctor((Functor)f);
}

void
Yap_InitSWIHash(void)
{
  int i, j;
  memset(SWI_ReverseHash, 0, N_SWI_HASH*sizeof(swi_rev_hash));
  for (i=0; i < N_SWI_ATOMS; i++) {
    add_to_hash(i, (ADDR)SWI_Atoms[i]);
  }
  for (j=0; j < N_SWI_FUNCTORS; j++) {
    add_to_hash(j, (ADDR)SWI_Functors[j]);
  }
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
  /* fprintf(stderr,"doing %s:%s/%d\n", RepAtom(AtomOfTerm(mod))->StrOfAE, a,arity); */
  CurrentModule = mod;
  Yap_InitCPred(a, arity, def, UserCPredFlag);
  if (arity == 0) {
    Atom at;
    while ((at = Yap_LookupAtom(a)) == NULL) {
      if (!Yap_growheap(FALSE, 0L, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
	return;
      }
    }
    pe = RepPredProp(PredPropByAtom(at,mod));
  } else {
    Atom at;
    Functor f;

    while ((at = Yap_LookupAtom(a)) == NULL) {
      if (!Yap_growheap(FALSE, 0L, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
	return;
      }
    }
    f = Yap_MkFunctor(at, arity);
    pe = RepPredProp(PredPropByFunc(f,mod));
  }
  pe->PredFlags |= (CArgsPredFlag|flags);
  CurrentModule = cm;
} 

static char *
alloc_ring_buf(void)
{
  SWI_buf_index++;
  if (SWI_buf_index == SWI_BUF_RINGS)
    SWI_buf_index = 0;
  if (SWI_buffers_sz[SWI_buf_index+1] == 0) {
    char * new;
    if (!(new = malloc(512))) {
      return NULL;
    }
    SWI_buffers_sz[SWI_buf_index+1] = 512;
    SWI_buffers[SWI_buf_index+1] = new;
  }
  return SWI_buffers[SWI_buf_index+1];
}

static char *
ensure_space(char **sp, size_t room, unsigned flags) {
  size_t min = 512;
  int i = 0;
  char *ptr = *sp;

  if (room < SWI_BUF_SIZE)
    return *sp;
  while (min < room)
    min += 512;

  if (flags & BUF_MALLOC) {
    PL_free(*sp);
    *sp = PL_malloc(room);
    return *sp; 
  } else if (flags & BUF_RING) {
    for (i=1; i<= SWI_BUF_RINGS; i++)
      if (SWI_buffers[i] == ptr)
	break;
  } else {
    i = 0;
  }
  if (SWI_buffers_sz[i] >= room)
    return ptr;
  free(SWI_buffers[i]);
  SWI_buffers[i] = malloc(min);
  if (SWI_buffers[i]) 
    SWI_buffers_sz[i] = min;
  else
    SWI_buffers_sz[i] = 0;
  *sp = SWI_buffers[i];
  return *sp;
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
  return RepAtom(SWIAtomToAtom(a))->StrOfAE;
}

/* SWI: char* PL_atom_chars(atom_t atom)
   YAP: char* AtomName(Atom) */
X_API char* PL_atom_nchars(atom_t a, size_t *len)	 /* SAM check type */
{
  char *s = RepAtom(SWIAtomToAtom(a))->StrOfAE;
  *len = strlen(s);
  return s;
}

X_API int
PL_chars_to_term(const char *s, term_t term) { 
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
   
/* SWI: int PL_get_arg(int index, term_t t, term_t a)
   YAP: YAP_Term YAP_ArgOfTerm(int argno, YAP_Term t)*/
X_API int _PL_get_arg(int index, term_t ts, term_t a)
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

/* SWI: int PL_get_atom(term_t t, YAP_Atom *a)
   YAP: YAP_Atom YAP_AtomOfTerm(Term) */
X_API int PL_get_intptr(term_t ts, intptr_t *a)
{
  Term t = Yap_GetFromSlot(ts);
  if ( !IsIntegerTerm(t) )
    return 0;
  *a = (intptr_t)(IntegerOfTerm(t));
  return 1;
}

/* SWI: int PL_get_atom_chars(term_t t, char **s)
   YAP: char* AtomName(Atom) */
X_API int PL_get_atom_chars(term_t ts, char **a)  /* SAM check type */
{
  Term t = Yap_GetFromSlot(ts);
  if (!IsAtomTerm(t))
    return 0;
  *a = RepAtom(AtomOfTerm(t))->StrOfAE;
  return 1;
}

/* SWI: int PL_get_atom_chars(term_t t, char **s)
   YAP: char* AtomName(Atom) */
X_API int PL_get_atom_nchars(term_t ts, size_t *len, char **s)  /* SAM check type */
{
  Term t = Yap_GetFromSlot(ts);
  if (!IsAtomTerm(t))
    return 0;
  *s = RepAtom(AtomOfTerm(t))->StrOfAE;
  *len = strlen(*s);
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

static int CvtToStringTerm(Term t, char *buf, char *buf_max)
{
  while (IsPairTerm(t)) {
    YAP_Term hd = HeadOfTerm(t);
    long int  i;
    if (IsVarTerm(hd) || !IsIntTerm(hd))
      return 0;
    i = IntOfTerm(hd);
    if (i <= 0 || i >= 255)
      return 0;
    *buf++ = i;
    if (buf == buf_max)
      return 0;
    t = TailOfTerm(t);
    if (IsVarTerm(t))
      return 0;
  }
  if (t != TermNil)
    return 0;
  if (buf+1 == buf_max)
    return 0;
  buf[0] = '\0';
  return 1;
}

#if !HAVE_SNPRINTF
#define snprintf(X,Y,Z,A) sprintf(X,Z,A)
#endif

/* This does not understand UNICODE yet */
static int do_yap_putc(int sno, wchar_t ch) {
  if (putc_curp < putc_cur_lim) {
    *putc_curp++ = ch;
    return TRUE;
  } else if (putc_cur_flags & BUF_MALLOC) {
    /* handle overflow by using realloc(); */
    UInt bufsize = putc_cur_lim-putc_cur_buf;
    UInt bufpos = putc_curp-putc_cur_buf;

    if (!(putc_cur_buf = PL_realloc(putc_cur_buf, bufsize+SWI_BUF_SIZE))) {
      /* we can+t go forever */
      return FALSE;
    }
    putc_curp = putc_cur_buf+bufpos;
    putc_cur_lim = putc_cur_buf+(bufsize+SWI_BUF_SIZE);
    return do_yap_putc(sno, ch);
  }
  return FALSE;
}

static int
CvtToGenericTerm(Term t, char *tmp, unsigned flags, char **sp)
{
  int wflags = 0;

  putc_cur_buf = putc_curp = tmp;
  putc_cur_flags = flags;
  if ((flags & BUF_RING)) {
    putc_cur_lim = tmp+(SWI_TMP_BUF_SIZE-1);
  } else {
    putc_cur_lim = tmp+(SWI_BUF_SIZE-1);
  }
  if (flags & CVT_WRITE_CANONICAL) {
    wflags |= (YAP_WRITE_IGNORE_OPS|YAP_WRITE_QUOTED);
  }
  Yap_plwrite(t, do_yap_putc, wflags, 1200);
  if (putc_cur_buf == putc_cur_lim)
    return 0;
  *putc_curp = '\0';
  /* may have changed due to overflows */
  *sp = putc_cur_buf;
  return 1;
}

static int 
cv_error(unsigned flags)
{
  if (flags & CVT_EXCEPTION) {
    YAP_Error(0, 0L, "PL_get_chars");
  }
  return 0;
}

X_API int PL_get_chars(term_t l, char **sp, unsigned flags)
{
  YAP_Term t = Yap_GetFromSlot(l);
  char *tmp;

  if ((flags & BUF_RING)) {
    tmp = alloc_ring_buf();
  } else if ((flags & BUF_MALLOC)) {
    tmp = PL_malloc(SWI_BUF_SIZE);
  } else {
    tmp = SWI_buffers[0];
  }
  *sp = tmp;
  if (IsVarTerm(t)) {
    if (!(flags & (CVT_VARIABLE|CVT_WRITE|CVT_WRITE_CANONICAL)))
      return cv_error(flags);  
    if (!CvtToGenericTerm(t, tmp, flags, sp))
      return 0;
  } else if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    if (!(flags & (CVT_ATOM|CVT_ATOMIC|CVT_WRITE|CVT_WRITE_CANONICAL|CVT_ALL)))
      return cv_error(flags);
    if (IsWideAtom(at)) {
      wchar_t* s = RepAtom(at)->WStrOfAE;
      size_t sz = wcslen(s)+1;
      if (!(tmp = ensure_space(sp, sz*sizeof(wchar_t), flags)))
	return 0;
      memcpy(*sp,s,sz*sizeof(wchar_t));
    } else {
      char *s = RepAtom(at)->StrOfAE;
      size_t sz = strlen(s)+1;

      if (!(tmp = ensure_space(sp, sz, flags)))
	return 0;
      memcpy(*sp,s,sz);
    }
  } else if (IsNumTerm(t)) {
    if (IsFloatTerm(t)) {
      if (!(flags & (CVT_FLOAT|CVT_NUMBER|CVT_ATOMIC|CVT_WRITE|CVT_WRITE_CANONICAL|CVT_ALL)))
	return cv_error(flags);
      snprintf(tmp,SWI_BUF_SIZE,"%f",FloatOfTerm(t));
#if USE_GMP
    } else if (YAP_IsBigNumTerm(t)) {
      if (!(flags & (CVT_FLOAT|CVT_NUMBER|CVT_ATOMIC|CVT_WRITE|CVT_WRITE_CANONICAL|CVT_ALL)))
	return cv_error(flags);
      MP_INT g;
      YAP_BigNumOfTerm(t, (void *)&g);
      if (mpz_sizeinbase(&g,2) > SWI_BUF_SIZE-1) {
	return 0;
      }
      mpz_get_str (tmp, 10, &g);
#endif
    } else {
      if (!(flags & (CVT_INTEGER|CVT_NUMBER|CVT_ATOMIC|CVT_WRITE|CVT_WRITE_CANONICAL|CVT_ALL)))
	return cv_error(flags);
      snprintf(tmp,SWI_BUF_SIZE,Int_FORMAT,IntegerOfTerm(t));
    }
  } else if (IsPairTerm(t))  {
    if (!(flags & (CVT_LIST|CVT_WRITE|CVT_WRITE_CANONICAL|CVT_ALL))) { 
      return cv_error(flags);
    }
    if (CvtToStringTerm(t,tmp,tmp+SWI_BUF_SIZE) == 0) {
      if (flags & (CVT_WRITE|CVT_WRITE_CANONICAL)) {
	if (!CvtToGenericTerm(t, tmp, flags, sp))
	  return 0;
      } else {
	return cv_error(flags);
      }
    }
  } else {
#if USE_GMP
    if (IsBigIntTerm(t)) {
      if (!(flags & (CVT_INTEGER|CVT_NUMBER|CVT_ATOMIC|CVT_WRITE|CVT_WRITE_CANONICAL|CVT_ALL)))
	return cv_error(flags);
      Yap_gmp_to_string(t, tmp, SWI_BUF_SIZE-1, 10);
    } else 
#endif
      if (IsBlobStringTerm(t)) {
      if (!(flags & (CVT_STRING|CVT_WRITE|CVT_WRITE_CANONICAL|CVT_ALL))) {
	return cv_error(flags);
      } else {
	char *s = Yap_BlobStringOfTerm(t);
	strncat(tmp, s, SWI_BUF_SIZE-1);
      } 
    } else {
      if (!(flags & (CVT_WRITE|CVT_WRITE_CANONICAL))) {
	return cv_error(flags);
      }
      if (!CvtToGenericTerm(t, tmp, flags, sp))
	return 0;
    }
  }
  if (flags & BUF_MALLOC) {
    size_t sz = strlen(tmp);
    char *nbf = PL_malloc(sz+1);
    if (!nbf)
      return 0;
    memcpy(nbf,tmp,sz+1);
    free(tmp);
    *sp = nbf;
  }
  return 1;
}

X_API int PL_get_nchars(term_t l, size_t *len, char **sp, unsigned flags)
{
  int out = PL_get_chars(l, sp, flags);
  if (!out) return out;
  if (len)
    *len = strlen(*sp);
  return out;
}


/* same as get_chars, but works on buffers of wide chars */
X_API int PL_get_wchars(term_t l, size_t *len, wchar_t **wsp, unsigned flags)
{
  unsigned nflags = ((CVT_MASK & flags) | (CVT_EXCEPTION & flags) | BUF_MALLOC);
  size_t room;
  wchar_t *buf, *wbuf = NULL;
  size_t sz, i;
  char *sp, *sp0;
  Term t = Yap_GetFromSlot(l);

  if (!IsVarTerm(t) && IsAtomTerm(t) && IsWideAtom(AtomOfTerm(t))) {
    if (!(flags & (CVT_ATOM|CVT_ATOMIC|CVT_WRITE|CVT_WRITE_CANONICAL|CVT_ALL)))
      return cv_error(flags);
    wbuf = RepAtom(AtomOfTerm(t))->WStrOfAE;
    sz = wcslen(wbuf);
    *len = sz;
  } else {
    if (!PL_get_nchars(l, &sz, &sp, nflags))
      return 0;
    if (len)
      *len = sz;
  }
  room = (sz+1)*sizeof(wchar_t);
  if (flags & BUF_MALLOC) {
    *wsp = buf = (wchar_t *)PL_malloc(room);
  } else if (flags & BUF_RING) {
    *wsp = (wchar_t *)alloc_ring_buf();
    buf = (wchar_t *)ensure_space((char **)wsp, room, flags);
  } else {
    *wsp = (wchar_t *)SWI_buffers[0];
    buf = (wchar_t *)ensure_space((char **)wsp, room, flags);
  }
  if (!buf) {
    if (flags & CVT_EXCEPTION)
      YAP_Error(0, 0L, "PL_get_wchars: buf_discardable too small for %ld wchars", sz);
    return 0;
  }
  if (wbuf) {
    wcsncpy(buf, wbuf, sz);
  } else {
    sp0 = sp;
    buf[sz] = '\0';
    for (i=0; i< sz; i++)
      *buf++ = *sp++;
    free(sp0);
  }
  return 1;
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
  if ( YAP_IsFloatTerm(t)) {
    *f = YAP_FloatOfTerm(t);
  } else if ( YAP_IsIntTerm(t)) {
    *f = YAP_IntOfTerm(t);
  } else {
    return 0;
  }
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

X_API int PL_get_string(term_t t, char **s, size_t *len)
{
  return PL_get_string_chars(t, s, len);
}

X_API int PL_get_string_chars(term_t t, char **s, size_t *len)
{
  Term tt = Yap_GetFromSlot(t);
  if (!IsBlobStringTerm(tt)) {
    return 0;
  }
  *s = Yap_BlobStringOfTermAndLength(tt, len);
  return TRUE;
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
#ifdef _WIN32
      sscanf(s, "%I64d", (long long int *)i);
#else
      sscanf(s, "%lld", (long long int *)i);
#endif
      return 1;
#endif
    }
    return 0;
  }
  *i = YAP_IntOfTerm(t);
  return 1;
#endif
}


#if USE_GMP

		 /*******************************
		 *	       GMP		*
		 *******************************/

X_API int PL_get_mpz(term_t t, mpz_t mpz)
{
  Term t0 = Yap_GetFromSlot(t);

  return Yap_term_to_existing_big(t0, mpz);
}

X_API int PL_unify_mpz(term_t t, mpz_t mpz)
{
  Term iterm = Yap_MkBigIntTerm(mpz);
  return Yap_unify(Yap_GetFromSlot(t),iterm);
}

X_API int PL_get_mpq(term_t t, mpq_t mpz)
{
  Term t0 = Yap_GetFromSlot(t);

  return Yap_term_to_existing_rat(t0, mpz);
}

X_API int PL_unify_mpq(term_t t, mpq_t mpq)
{
  Term iterm = Yap_MkBigRatTerm(mpq);
  return Yap_unify(Yap_GetFromSlot(t),iterm);
}

#endif

X_API int PL_get_list(term_t ts, term_t h, term_t tl)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  if (IsVarTerm(t) || !IsPairTerm(t) ) {
    return 0;
  }
  Yap_PutInSlot(h,HeadOfTerm(t));
  Yap_PutInSlot(tl,TailOfTerm(t));
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
  Atom at;
  while ((at = Yap_LookupAtom((char *)c)) == NULL) {
    if (!Yap_growheap(FALSE, 0L, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
      return 0L;
    }
  }
  Yap_AtomIncreaseHold(at);
  return AtomToSWIAtom(at);
}

X_API atom_t PL_new_atom_nchars(size_t len, const char *c)
{
  Atom at;
  char *pt;
  if (strlen(c) > len) {
    while ((pt = (char *)Yap_AllocCodeSpace(len+1)) == NULL) {
      if (!Yap_growheap(FALSE, 0L, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
	return 0L;
      }
    }
    memcpy(pt, c, len);
    pt[len] = '\0';
  } else {
    pt = (char *)c;
  }
  while ((at = Yap_LookupAtom(pt)) == NULL) {
    if (!Yap_growheap(FALSE, 0L, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
      return 0L;
    }
  }
  Yap_AtomIncreaseHold(at);
  return AtomToSWIAtom(at);
}

X_API atom_t PL_new_atom_wchars(size_t len, const wchar_t *c)
{
  atom_t at;
  int i;

  for (i=0;i<len;i++) {
    if (c[i] > 255) break;
  }
  if (i!=len) {
    Atom at0;
    wchar_t *nbf;
    while (!(nbf = (wchar_t *)YAP_AllocSpaceFromYap((len+1)*sizeof(wchar_t)))) {
      if (!Yap_growheap(FALSE, 0L, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
	return 0;
      }
    }
    for (i=0;i<len;i++)
      nbf[i] = c[i];
    nbf[len]='\0';
    while ((at0 = Yap_LookupWideAtom(nbf)) == NULL) {
      if (!Yap_growheap(FALSE, 0L, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
	return 0L;
      }
    }
    at = AtomToSWIAtom(at0);
    YAP_FreeSpaceFromYap(nbf);
  } else {
    char *nbf;
    Atom at0;

    while (!(nbf = (char *)YAP_AllocSpaceFromYap((len+1)*sizeof(char)))) {
      if (!Yap_growheap(FALSE, 0L, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
	return 0;
      }
    }
    for (i=0;i<len;i++)
      nbf[i] = c[i];
    nbf[len]='\0';
    while (!(at0 = Yap_LookupAtom(nbf))) {
      if (!Yap_growheap(FALSE, 0L, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
	return 0;
      }
    }
    at = AtomToSWIAtom(at0);
    YAP_FreeSpaceFromYap(nbf);
  }
  return at;
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

X_API int PL_cons_functor(term_t d, functor_t f,...)
{
  va_list ap;
  int arity, i;
  Term *tmp, t;
  Functor ff = SWIFunctorToFunctor(f);

  if (IsAtomTerm((Term)ff)) {
    Yap_PutInSlot(d, (YAP_Term)f);
    return TRUE;
  }
  arity = ArityOfFunctor(ff);
  while (Unsigned(H+arity) > Unsigned(ASP)-CreepFlag) {
    if (!Yap_gc(0, ENV, CP)) {
      return FALSE;
    }
  }
  if (arity == 2 && ff == FunctorDot) {
    t = Yap_MkNewPairTerm();
    tmp = RepPair(t);
  } else {
    t = Yap_MkNewApplTerm(ff, arity);
    tmp = RepAppl(t)+1;
  }
  va_start (ap, f);
  for (i = 0; i < arity; i++) {
    Yap_unify(tmp[i],Yap_GetFromSlot(va_arg(ap, term_t)));
  }
  va_end (ap);
  Yap_PutInSlot(d,t);
  return TRUE;
}

X_API int PL_cons_functor_v(term_t d, functor_t f, term_t a0)
{
  int arity, i;
  Term *tmp, t;
  Functor ff = SWIFunctorToFunctor(f);

  if (IsAtomTerm((Term)ff)) {
    Yap_PutInSlot(d, (YAP_Term)f);
    return TRUE;
  }
  arity = ArityOfFunctor(ff);
  while (Unsigned(H+arity) > Unsigned(ASP)-CreepFlag) {
    if (!Yap_gc(0, ENV, CP)) {
      return FALSE;
    }
  }
  if (arity == 2 && ff == FunctorDot) {
    t = Yap_MkNewPairTerm();
    tmp = RepPair(t);
  } else {
    t = Yap_MkNewApplTerm(ff, arity);
    tmp = RepAppl(t)+1;
  }
  for (i = 0; i < arity; i++) {
    Yap_unify(tmp[i],Yap_GetFromSlot(a0));
    a0++;
  }
  Yap_PutInSlot(d,t);
  return TRUE;
}

X_API int PL_cons_list(term_t d, term_t h, term_t t)
{
  Yap_PutInSlot(d,YAP_MkPairTerm(Yap_GetFromSlot(h),Yap_GetFromSlot(t)));
  return TRUE;
}

X_API int PL_put_atom(term_t t, atom_t a)
{
  Yap_PutInSlot(t,MkAtomTerm(SWIAtomToAtom(a)));
  return TRUE;
}

X_API int PL_put_atom_chars(term_t t, const char *s)
{
  Atom at;
  while (!(at = Yap_LookupAtom((char *)s))) {
    if (!Yap_growheap(FALSE, 0L, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
      return FALSE;
    }
  }
  Yap_PutInSlot(t,MkAtomTerm(at));
  return TRUE;
}

X_API int PL_put_atom_nchars(term_t t, size_t len, const char *s)
{
  Atom at;
  char *buf;

  if (strlen(s) > len) {
    while (!(buf = (char *)Yap_AllocCodeSpace(len+1))) {
      if (!Yap_growheap(FALSE, 0L, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
	return FALSE;
      }
    }
    memcpy(buf, s, len);
    buf[len] = 0;
  } else {
    buf = (char *)s;
  }
  while (!(at = Yap_LookupAtom(buf))) {
    if (!Yap_growheap(FALSE, 0L, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
      return FALSE;
    }
  }
  Yap_PutInSlot(t,MkAtomTerm(at));
  return TRUE;
}

X_API int PL_put_float(term_t t, double fl)
{
  Yap_PutInSlot(t,YAP_MkFloatTerm(fl));
  return TRUE;
}

X_API int PL_put_functor(term_t t, functor_t f)
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
    if (Unsigned(H) > Unsigned(ASP)-CreepFlag) {
      if (!Yap_gc(0, ENV, CP)) {
	return FALSE;
      }
    }
  }
  return TRUE;
}

X_API int PL_put_integer(term_t t, long n)
{
  Yap_PutInSlot(t,YAP_MkIntTerm(n));
  return TRUE;
}

X_API int PL_put_int64(term_t t, int64_t n)
{
#if USE_GMP
  char s[64];
  MP_INT rop;

#ifdef _WIN32
  snprintf(s, 64, "%I64d", (long long int)n);
#elif HAVE_SNPRINTF
  snprintf(s, 64, "%lld", (long long int)n);
#else
  sprintf(s, "%lld", (long long int)n);
#endif
  mpz_init_set_str (&rop, s, 10);
  Yap_PutInSlot(t,YAP_MkBigNumTerm((void *)&rop));
  return TRUE;
#else
  return FALSE;
#endif
}

X_API int PL_put_list(term_t t)
{
  Yap_PutInSlot(t,YAP_MkNewPairTerm());
  if (Unsigned(H) > Unsigned(ASP)-CreepFlag) {
    if (!Yap_gc(0, ENV, CP)) {
      return FALSE;
    }
  }
  return TRUE;
}

X_API int PL_put_list_chars(term_t t, const char *s)
{
  Yap_PutInSlot(t,YAP_BufferToString((char *)s));
  if (Unsigned(H) > Unsigned(ASP)-CreepFlag) {
    if (!Yap_gc(0, ENV, CP)) {
      return FALSE;
    }
  }
  return TRUE;
}

X_API void PL_put_nil(term_t t)
{
  Yap_PutInSlot(t,TermNil);
}

/* SWI: void PL_put_pointer(term_t -t, void *ptr)
   YAP: NO EQUIVALENT */
/* SAM TO DO */
X_API int PL_put_pointer(term_t t, void *ptr)
{
  YAP_Term tptr = YAP_MkIntTerm((YAP_Int)ptr);
  Yap_PutInSlot(t,tptr);
  return TRUE;
}

X_API int PL_put_string_nchars(term_t t, size_t len, const char *chars)
{
  Term tt;

  if ((tt = Yap_MkBlobStringTerm(chars, len)) == TermNil)
    return FALSE;
  Yap_PutInSlot(t,tt);
  return TRUE;
}

X_API int PL_put_term(term_t d, term_t s)
{
  Yap_PutInSlot(d,Yap_GetFromSlot(s));
  return TRUE;
}

X_API int PL_put_variable(term_t t)
{
  Yap_PutInSlot(t,MkVarTerm());
  return TRUE;
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

X_API int PL_throw(term_t exception)
{
  YAP_Throw(Yap_GetFromSlot(exception));
  longjmp(execution->env, 0);
  return 0;
}

X_API void  PL_fatal_error(const char *msg)
{
  fprintf(stderr,"[ FATAL ERROR: %s ]\n",msg);
  Yap_exit(1);
}

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
    case ERR_STREAM_OP:
    { atom_t action = va_arg(args, atom_t);
      term_t stream = va_arg(args, term_t);
      int rc;

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_io_error2,
			   PL_ATOM, action,
			   PL_TERM, stream);
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
  Atom catom;
  Term cterm;
  while (!(catom = Yap_LookupAtom((char *)s))) {
    if (!Yap_growheap(FALSE, 0L, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
      return FALSE;
    }
  }
  cterm = MkAtomTerm(catom);
  return Yap_unify(Yap_GetFromSlot(t),cterm);
}

/* SWI: int PL_unify_atom_chars(term_t ?t, const char *chars)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_atom_nchars(term_t t, size_t len, const char *s)
{
  Atom catom;
  YAP_Term cterm;
  char *buf = (char *)malloc(len+1);

  if (!buf)
    return FALSE;
  memcpy(buf, s, len);
  buf[len] = '\0';
  while (!(catom = Yap_LookupAtom(buf))) {
    if (!Yap_growheap(FALSE, 0L, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
      return FALSE;
    }
  }
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
  Term iterm = MkIntegerTerm(n);
  return Yap_unify(Yap_GetFromSlot(t),iterm);
}

/* SWI: int PL_unify_integer(term_t ?t, long n)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_functor(term_t t, functor_t f)
{	
  YAP_Term tt = Yap_GetFromSlot(t);
  Functor ff = SWIFunctorToFunctor(f);
  if (Unsigned(H) > Unsigned(ASP)-CreepFlag) {
    if (!Yap_gc(0, ENV, CP)) {
      return FALSE;
    }
  }
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
#if SIZEOF_INT_P==8
  Term iterm = MkIntegerTerm(n);
  return Yap_unify(Yap_GetFromSlot(t),iterm);
#elif USE_GMP
  YAP_Term iterm;
  char s[64];
  MP_INT rop;

#ifdef _WIN32
  snprintf(s, 64, "%I64d", (long long int)n);
#elif HAVE_SNPRINTF
  snprintf(s, 64, "%lld", (long long int)n);
#else
  sprintf(s, "%lld", (long long int)n);
#endif
  mpz_init_set_str (&rop, s, 10);
  iterm = YAP_MkBigNumTerm((void *)&rop);
  return YAP_Unify(Yap_GetFromSlot(t),iterm);
#else
  fprintf(stderr,"Error: please install GM\n");
  return FALSE;
#endif

}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_list(term_t tt, term_t h, term_t tail)
{
  Term t;
  if (Unsigned(H) > Unsigned(ASP)-CreepFlag) {
    if (!Yap_gc(0, ENV, CP)) {
      return FALSE;
    }
  }
  t = Deref(Yap_GetFromSlot(tt));
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
X_API int PL_unify_arg(int index, term_t tt, term_t arg)
{
  Term t = Deref(Yap_GetFromSlot(tt)), to;
  if (index < 0)
    return FALSE;
  if (IsVarTerm(t) || IsAtomOrIntTerm(t)) {
    return FALSE;
  } else if (IsPairTerm(t)) {
    if (index == 1)
      to = HeadOfTerm(t);
    else if (index == 2)
      to = TailOfTerm(t);
    else
      return FALSE;
  } else {
    Functor f = FunctorOfTerm(t);
    if (IsExtensionFunctor(f)) 
      return FALSE;
    if (index > ArityOfFunctor(f))
      return FALSE;
    to = ArgOfTerm(index, t);
  }
  return Yap_unify(Yap_GetFromSlot(t),to);
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_list_chars(term_t t, const char *chars)
{
  YAP_Term chterm;
  if (Unsigned(H) > Unsigned(ASP)-CreepFlag) {
    if (!Yap_gc(0, ENV, CP)) {
      return FALSE;
    }
  }
  chterm = YAP_BufferToString((char *)chars);
  return YAP_Unify(Yap_GetFromSlot(t), chterm);
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_list_ncodes(term_t t, size_t len, const char *chars)
{
  Term chterm;
  if (Unsigned(H) > Unsigned(ASP+len*2)-CreepFlag) {
    if (!Yap_gc(len*2*sizeof(CELL), ENV, CP)) {
      return FALSE;
    }
  }
  chterm = Yap_NStringToList((char *)chars, len);
  return Yap_unify(Yap_GetFromSlot(t), chterm);
}

X_API int
PL_unify_list_codes(term_t l, const char *chars)
{ return PL_unify_list_ncodes(l, strlen(chars), chars);
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
  YAP_Term ptrterm = YAP_MkIntTerm((YAP_Int)ptr);
  return YAP_Unify(Yap_GetFromSlot(t), ptrterm);
}

/* SWI: int PL_unify_list(term_t ?t, term_t +h, term_t -t)
   YAP long int  unify(YAP_Term* a, Term* b) */
X_API int PL_unify_string_chars(term_t t, const char *chars)
{
  YAP_Term chterm;
  if (Unsigned(H) > Unsigned(ASP)-CreepFlag) {
    if (!Yap_gc(0, ENV, CP)) {
      return FALSE;
    }
  }
  chterm = YAP_BufferToString((char *)chars);
  return YAP_Unify(Yap_GetFromSlot(t), chterm);
}

X_API int PL_unify_string_nchars(term_t t, size_t len, const char *chars)
{
  YAP_Term chterm;
  if (Unsigned(H) > Unsigned(ASP)-CreepFlag) {
    if (!Yap_gc(0, ENV, CP)) {
      return FALSE;
    }
  }
  chterm = YAP_NBufferToString((char *)chars, len);
  return YAP_Unify(Yap_GetFromSlot(t), chterm);
}

/* SWI: int PL_unify_wchars(term_t ?t, int type, size_t len,, const pl_wchar_t *s)
 */
X_API int PL_unify_wchars(term_t t, int type, size_t len, const pl_wchar_t *chars)
{
  YAP_Term chterm;

  if (len == (size_t)-1)
    len = wcslen(chars);

  if (Unsigned(H) > Unsigned(ASP)-CreepFlag) {
    if (!Yap_gc(0, ENV, CP)) {
      return FALSE;
    }
  }
  switch (type) {
  case PL_ATOM:
    {
      Atom at;
      while ((at = Yap_LookupMaybeWideAtomWithLength((wchar_t *)chars, len)) == NULL) {
	if (!Yap_growheap(FALSE, 0L, NULL)) {
	  Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
	  return FALSE;
	}
      }
      chterm = MkAtomTerm(at);
    }
    break;
  case PL_STRING:
    chterm = Yap_MkBlobWideStringTerm(chars, len);
    break;
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

  if (tail == 0)
    return PL_unify_wchars(t, type, len, chars);
  if (Unsigned(H) > Unsigned(ASP)-CreepFlag) {
    if (!Yap_gc(0, ENV, CP)) {
      return FALSE;
    }
  }
  if (len == (size_t)-1)
    len = wcslen(chars);

  switch (type) {
  case PL_CODE_LIST:
    chterm = YAP_NWideBufferToDiffList(chars, Yap_GetFromSlot(tail), len);
    break;
  case PL_CHAR_LIST:
    chterm = YAP_NWideBufferToAtomDiffList(chars, Yap_GetFromSlot(tail), len);
    break;
  default:
    fprintf(stderr,"NOT GOOD option %d PL_unify_chars_wdiff\n",type);
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
  memcpy(buf, s, n);
  buf[n] = '\0';
  while (!(catom = Yap_LookupAtom(buf))) {
    if (!Yap_growheap(FALSE, 0L, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
      return NULL;
    }
  }
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
  while (!(catom = Yap_LookupMaybeWideAtom(buf))) {
    if (!Yap_growheap(FALSE, 0L, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
      return NULL;
    }
  }
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
  int type, res;
  int nels = 1;
  int depth = 1;
  Term a[1], *pt;
  stack_el stack[MAX_DEPTH];
  
  BACKUP_MACHINE_REGS();
  if (Unsigned(H) > Unsigned(ASP)-CreepFlag) {
    if (!Yap_gc(0, ENV, CP)) {
      RECOVER_MACHINE_REGS();
      return FALSE;
    }
  }
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
      case PL_LONG:
	*pt++ = MkIntegerTerm(va_arg(ap, long));
	break;
      case PL_INT:
	*pt++ = MkIntegerTerm(va_arg(ap, int));
	break;
      case PL_FLOAT:
	*pt++ = MkFloatTerm(va_arg(ap, double));
	break;
      case PL_STRING:
	*pt++ = Yap_MkBlobStringTerm(va_arg(ap, char *), -1);
	break;
      case PL_CHARS:
	{
	  Atom at;
	  char *s = va_arg(ap, char *);
	  while (!(at = Yap_LookupAtom(s))) {
	    if (!Yap_growheap(FALSE, 0L, NULL)) {
	      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
	      return FALSE;
	    }
	  }
	  *pt++ = MkAtomTerm(at);
	}
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
	  wchar_t * arg = va_arg(ap, wchar_t *);
	  *pt++ = MkAtomTerm(LookupMaxWideAtom(sz,arg));
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

#ifdef _WIN32
	  snprintf(s, 64, "%I64d", va_arg(ap, long long int));
#elif HAVE_SNPRINTF
	  snprintf(s, 64, "%lld", va_arg(ap, long long int));
#else
	  sprintf(s, "%lld", va_arg(ap, long long int));
#endif	  
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
	    Atom at;

	    while (!(at = Yap_LookupAtom(fname))) {
	      if (!Yap_growheap(FALSE, 0L, NULL)) {
		Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
		return FALSE;
	      }
	    }
	    *pt++ = MkAtomTerm(at);
	  } else {
	    Atom at;
	    Functor ff;
	    Term t;

	    while (!(at = Yap_LookupAtom(fname))) {
	      if (!Yap_growheap(FALSE, 0L, NULL)) {
		Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
		return FALSE;
	      }
	    }
	    ff = Yap_MkFunctor(at,arity);
	    t = Yap_MkNewApplTerm(ff, arity);
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
  res = Yap_unify(Yap_GetFromSlot(l),a[0]);
  RECOVER_MACHINE_REGS();
  return res;
}

/* end PL_unify_* functions =============================*/

/* SWI: void PL_register_atom(atom_t atom) */
X_API void PL_register_atom(atom_t atom)
{
  Yap_AtomIncreaseHold(SWIAtomToAtom(atom));
}

/* SWI: void PL_unregister_atom(atom_t atom) */
X_API void PL_unregister_atom(atom_t atom)
{
  Yap_AtomDecreaseHold(SWIAtomToAtom(atom));
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

X_API int PL_is_ground(term_t t)
{
  return Yap_IsGroundTerm(Yap_GetFromSlot(t));
}

X_API int PL_is_callable(term_t t)
{
  YAP_Term t1 = Yap_GetFromSlot(t);
  if (IsVarTerm(t1))
    return FALSE;
  if (IsAtomTerm(t1) || IsPairTerm(t1)) 
    return TRUE;
  if (IsApplTerm(t1) && !IsExtensionFunctor(FunctorOfTerm(t1)))
      return TRUE;
  return FALSE;
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
  return YAP_IsIntTerm(t) || YAP_IsBigNumTerm(t);
}

X_API int PL_is_list(term_t ts)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  return Yap_IsListTerm(t);
}

X_API int PL_is_number(term_t ts)
{
  YAP_Term t = Yap_GetFromSlot(ts);
  return YAP_IsIntTerm(t) || YAP_IsBigNumTerm(t) || YAP_IsFloatTerm(t);
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

X_API char *
PL_record_external
(term_t ts, size_t *sz)
{
  Term t = Yap_GetFromSlot(ts);
  size_t len = 512, nsz;
  char *s;

  while(TRUE) {
    if (!(s = Yap_AllocCodeSpace(len)))
      return NULL;
    if ((nsz = Yap_ExportTerm(t, s, len))) {
      *sz = nsz;
      return s;
    } else {
      if (len < 16*1024) 
	len = len *2;
      else 
	len += 16*1024;
    }
  }
  return NULL;
}

/* 
   partial implementation of recorded_external, does not guarantee endianness nor portability, and does not
   support constraints.
 */

X_API int
PL_recorded_external
(char *tp, term_t ts)
{
  Term t = Yap_ImportTerm(tp);
  if (t == 0)
    return FALSE;
  Yap_PutInSlot(ts,t);
  return TRUE;
}

X_API int
PL_erase_external
(char *tp)
{
  Yap_FreeCodeSpace(tp);
  return TRUE;
}

X_API record_t
PL_record(term_t ts)
{
  Term t = Yap_GetFromSlot(ts);
  return (record_t)YAP_Record(t);
}

X_API int
PL_recorded(record_t db, term_t ts)
{
  Term t = YAP_Recorded((void *)db);
  fprintf(stderr,"PL_recorded %ld\n", t);
  if (t == ((CELL)0))
    return FALSE;
  Yap_PutInSlot(ts,t);
  fprintf(stderr,"PL_recorded\n");
  return TRUE;
}

X_API void
PL_erase(record_t db)
{
  YAP_Erase((void *)db);
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

X_API void
PL_clear_exception(void)
{
  EX = NULL;
}

X_API int
PL_initialise(int myargc, char **myargv)
{
  YAP_init_args init_args;

  memset((void *)&init_args,0,sizeof(init_args));
  init_args.Argv = myargv;
  init_args.Argc = myargc;
#if BOOT_FROM_SAVED_STATE
  init_args.SavedState = "startup.yss";
#else
  init_args.SavedState = NULL;
#endif
  init_args.YapLibDir = NULL;
  init_args.YapPrologBootFile = NULL;
  init_args.HaltAfterConsult = FALSE;
  init_args.FastBoot = FALSE;
  init_args.MaxTableSpaceSize = 0;
  init_args.NumberWorkers = 1;
  init_args.SchedulerLoop = 10;
  init_args.DelayedReleaseLoad = 3;

  Yap_PL_Argc = myargc;
  Yap_PL_Argv = myargv;
  Yap_InitialisedFromPL = TRUE;
  return YAP_Init(&init_args) != YAP_BOOT_ERROR;
}

X_API int
PL_is_initialised(int *argcp, char ***argvp)
{
  if (Yap_InitialisedFromPL) {
    if (argcp) 
      *argcp = Yap_PL_Argc;
    if (argvp) 
      *argvp = Yap_PL_Argv;
  }
  return Yap_InitialisedFromPL;
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
  Term mod = SWIModuleToModule(m);

  if (IsAtomTerm((Term)f)) {
    return YAP_Predicate(YAP_AtomOfTerm((Term)f),0,mod);
  } else {
    return YAP_Predicate((YAP_Atom)NameOfFunctor(ff),ArityOfFunctor(ff),mod);
  }
}

X_API predicate_t PL_predicate(const char *name, int arity, const char *m)
{
  Term mod;
  Atom at;
  if (m == NULL) {
    mod = CurrentModule;
    if (!mod) mod = USER_MODULE;
  } else {
    Atom at;
    while (!(at = Yap_LookupAtom((char *)m))) {
      if (!Yap_growheap(FALSE, 0L, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
	return NULL;
      }
    }
    mod = MkAtomTerm(at);
  }
  while (!(at = Yap_LookupAtom((char *)name))) {
    if (!Yap_growheap(FALSE, 0L, NULL)) {
      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, Yap_ErrorMessage);
      return NULL;
    }
  }
  return YAP_Predicate((YAP_Atom)at, arity, mod);
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

X_API fid_t
PL_open_foreign_frame(void)
{
  open_query *new = (open_query *)malloc(sizeof(open_query));
  if (!new) return 0;
  new->old = execution;
  new->g = TermNil;
  new->open = FALSE;
  new->cp = CP;
  new->p = P;
  new->slots = CurSlot;
  Yap_StartSlots();
  execution = new;
  return (fid_t)new;
}

X_API void
PL_close_foreign_frame(fid_t f)
{
  execution = (open_query *)f;
  CP = execution->cp;
  P = execution->p;
  CurSlot = execution->slots;
  execution = execution->old;
}

X_API void
PL_rewind_foreign_frame(fid_t f)
{
  execution = (open_query *)f;
  CurSlot = execution->slots;
}

X_API void
PL_discard_foreign_frame(fid_t f)
{
  execution = (open_query *)f;
  CP = execution->cp;
  P = execution->p;
  CurSlot = execution->slots;
  execution = execution->old;
}

X_API qid_t PL_open_query(module_t ctx, int flags, predicate_t p, term_t t0)
{
  Atom yname;
  unsigned long int  arity;
  Term t[2], m;

  /* ignore flags  and module for now */
  PL_open_foreign_frame();
  execution->open=1;
  execution->state=0;
  PredicateInfo((PredEntry *)p, &yname, &arity, &m);
  t[0] = SWIModuleToModule(ctx);
  if (arity == 0) {
    t[1] = MkAtomTerm(yname);
  } else {
    Functor f = Yap_MkFunctor(yname, arity);
    t[1] = Yap_MkApplTerm(f,arity,Yap_AddressFromSlot(t0));
  }
  if (ctx) {
    Term ti;
    t[0] = MkAtomTerm((Atom)ctx);
    ti = Yap_MkApplTerm(FunctorModule,2,t);
    t[0] = ti;
    execution->g = Yap_MkApplTerm(FunctorCall,1,t);
  } else {
    if (m && m != CurrentModule) {
      Term ti;
      t[0] = m;
      ti = Yap_MkApplTerm(FunctorModule,2,t);
      t[0] = ti;
      execution->g = Yap_MkApplTerm(FunctorCall,1,t);
    } else {
      execution->g = t[1];
    }
  }
  return execution;
}

X_API int PL_next_solution(qid_t qi)
{
  int result;
  if (qi->open != 1) return 0;
  if (setjmp(execution->env))
    return 0;
  if (qi->state == 0) {
    result = YAP_RunGoal(qi->g);
  } else {
    Yap_AllowRestart = qi->open;
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
  if (qi->open != 1) return;
  YAP_PruneGoal();
  YAP_cut_up();
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
  int out;

  BACKUP_B();
  BACKUP_H();

  Term t[2], g;
  t[0] = SWIModuleToModule(m);
  t[1] = Yap_GetFromSlot(tp);
  g = Yap_MkApplTerm(FunctorModule,2,t);
  out =  YAP_RunGoal(g);

  RECOVER_H();
  RECOVER_B();
  return out;
}

X_API void PL_register_foreign_in_module(const char *module, const char *name, int arity, pl_function_t function, int flags)
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
    Yap_InitCPredBackCut((char *)name, arity, sizeof(struct foreign_context)/sizeof(CELL), (CPredicate)function, (CPredicate)function, (CPredicate)function, UserCPredFlag|nflags);
  } else {
    UserCPredicate((char *)name,(CPredicate)function,arity,tmod,nflags);
  }
}

X_API void PL_register_extensions(const PL_extension *ptr)
{
  PL_load_extensions(ptr);
}

X_API void PL_register_foreign(const char *name, int arity, pl_function_t function, int flags)
{
  PL_register_foreign_in_module(NULL, name, arity, function, flags);
}

X_API void PL_load_extensions(const PL_extension *ptr)
{
  /* ignore flags for now */
  while(ptr->predicate_name != NULL) {
    PL_register_foreign_in_module(NULL, ptr->predicate_name, ptr->arity, ptr->function, ptr->flags);
    ptr++;
  }
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
#if THREADS
  if (pthread_getspecific(Yap_yaamregs_key) == NULL)
    return -1;
  return worker_id;
#else
  return -2;
#endif
}

X_API int PL_unify_thread_id(term_t t, int i)
{
  Term iterm = MkIntegerTerm(i);
  return Yap_unify(Yap_GetFromSlot(t),iterm);
}


X_API int PL_thread_attach_engine(const PL_thread_attr_t *attr)
{
  int wid = PL_thread_self();
  
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
    return YAP_ThreadAttachEngine(wid);
  }
}

X_API int PL_thread_destroy_engine(void)
{
  int wid = PL_thread_self();

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
#if THREADS
  if (attr) {
    YAP_thread_attr yapt;

    yapt.ssize = attr->local_size;
    yapt.tsize = attr->global_size;
    yapt.alias = (YAP_Term)attr->alias;
    yapt.cancel =  attr->cancel;
    return  Yap_WLocal+YAP_ThreadCreateEngine(&yapt);
  } else {
    return Yap_WLocal+YAP_ThreadCreateEngine(NULL);
  }
#else
  return NULL;
#endif
}


X_API int
PL_destroy_engine(PL_engine_t e)
{
#if THREADS
  return YAP_ThreadDestroyEngine((struct worker_local *)e-Yap_WLocal);
#else
  return FALSE;
#endif
}

X_API int
PL_set_engine(PL_engine_t engine, PL_engine_t *old)
{
#if THREADS
  int cwid = PL_thread_self(), nwid;

  if (cwid >= 0) {
    if (old) *old = (PL_engine_t)(Yap_WLocal+cwid);
  }
  if (!engine) {
    if (cwid < 0)
      return PL_ENGINE_INVAL;
    if (!YAP_ThreadDetachEngine(worker_id)) {
      return PL_ENGINE_INVAL;
    }
    return PL_ENGINE_SET;
  }
  if (engine == PL_ENGINE_MAIN) {
    nwid = 0;
  } else if (engine == PL_ENGINE_CURRENT) {
    if (cwid < 0) {
      if (old) *old = NULL;
      return PL_ENGINE_INVAL;
    }
    return PL_ENGINE_SET;
  } else {
    nwid = (struct worker_local *)engine-Yap_WLocal;
  }

  pthread_mutex_lock(&(FOREIGN_ThreadHandle(nwid).tlock));
  if (FOREIGN_ThreadHandle(nwid).pthread_handle) {
    pthread_mutex_unlock(&(FOREIGN_ThreadHandle(nwid).tlock));
    if (cwid != nwid) {
      return PL_ENGINE_INUSE;
    }
    return PL_ENGINE_SET;
  }
  if (cwid >= 0) {
    if (!YAP_ThreadDetachEngine(cwid)) {
      *old = NULL;
      pthread_mutex_unlock(&(FOREIGN_ThreadHandle(nwid).tlock));
      return PL_ENGINE_INVAL;
    }
  }
  if (!YAP_ThreadAttachEngine(nwid)) {
    return PL_ENGINE_INVAL;
  }
  return PL_ENGINE_SET;
#else
  if (old) *old = (PL_engine_t)&Yap_WLocal;
  return FALSE;
#endif
}


X_API void *
PL_malloc(size_t sz)
{
  if ( sz == 0 )
    return NULL;
  return (void *)malloc((long unsigned int)sz);
}

X_API void *
PL_realloc(void *ptr, size_t sz)
{
  if (ptr) {
    if (sz) {
      return realloc((char *)ptr,(long unsigned int)sz);
    } else {
      free(ptr);
      return NULL;
    }
  } else {
    return PL_malloc(sz);
  }
}

X_API void
PL_free(void *obj)
{
  if (obj)
    free(obj);
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
#ifdef _WIN32
    sscanf(s, "%I64d", (long long int *)val);
#else
    sscanf(s, "%lld", (long long int *)val);
#endif
    return 1;
#endif
  }
  PL_error(NULL,0,NULL, ERR_TYPE, AtomToSWIAtom(Yap_LookupAtom("integer_expression")));
  return FALSE;
}

foreign_t
_PL_retry(intptr_t n)
{
  /* first we need to get the pointer to the predicate */
  PredEntry *pe = B->cp_ap->u.OtapFs.p;
  struct foreign_context *ctx = (struct foreign_context *)(&EXTRA_CBACK_ARG(pe->ArityOfPE,1));  
  ctx->context = n;
  return LCL0-(CELL *)ctx;
}

foreign_t
_PL_retry_address(void *addr)
{
  /* first we need to get the pointer to the predicate */
  PredEntry *pe = B->cp_ap->u.OtapFs.p;
  struct foreign_context *ctx = (struct foreign_context *)(&EXTRA_CBACK_ARG(pe->ArityOfPE,1));  
  ctx->context = (intptr_t)addr;
  return LCL0-(CELL *)ctx;
}


X_API int
PL_foreign_control(control_t ctx)
{
  switch (ctx->control) {
  case FRG_REDO:
    return PL_REDO;
  case FRG_FIRST_CALL:
    return PL_FIRST_CALL;
  default:
    return PL_CUTTED;
  }
}

X_API intptr_t
PL_foreign_context(control_t ctx)
{
  switch (ctx->control) {
  case FRG_FIRST_CALL:
    return 0L;
  default:
    return (intptr_t)(ctx->context);
  }
}


X_API void *
PL_foreign_context_address(control_t ctx)
{
  switch (ctx->control) {
  case FRG_FIRST_CALL:
    return NULL;
  default:
    return (void *)(ctx->context);
  }
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
  if (YAP_IsIntTerm(t1))
    tim = (time_t)YAP_IntOfTerm(t1);
  else if (YAP_IsFloatTerm(t1))
    tim = (time_t)YAP_FloatOfTerm(t1);
  else
    return FALSE;
  return YAP_Unify(YAP_BufferToString(ctime(&tim)), YAP_ARG2);
#else
  YAP_Error(0,0L,"convert_time requires ctime");
  return FALSE;
#endif
}

X_API int
PL_get_signum_ex(term_t sig, int *n)
{
  char *s;
  int i = -1;

  if ( PL_get_integer(sig, &i) )
  {
  } else if ( PL_get_chars(sig, &s, CVT_ATOM) )
  { i = Yap_signal_index(s);
  } else
  { return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_signal, sig);
  }

  if ( i > 0 && i < 32 )		/* where to get these? */
  { *n = i;
    return TRUE;
  }

  return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_signal, sig);
}

typedef struct blob {
  Functor f;
  CELL type;
  MP_INT blinfo;  /* total size should go here */
  PL_blob_t *blb;
  size_t size;
  CELL  blob_data[1];
} blob_t;

X_API intptr_t
PL_query(int query)
{
  switch(query) {
  case PL_QUERY_ARGC:
    return (intptr_t)Yap_argc;
  case PL_QUERY_ARGV:
    return (intptr_t)Yap_argv;
  case PL_QUERY_USER_CPU:
    return (intptr_t)Yap_cputime();
  default:
    fprintf(stderr,"Unimplemented PL_query %d\n",query);
    return (intptr_t)0;
  }
}  


/* glue function to connect back PLStream to YAP IO */
X_API void
PL_YAP_InitSWIIO(struct SWI_IO *swio)
{
  FSWIStream = SWIFunctorToFunctor(swio->f);
  SWIGetc = swio->get_c;
  SWIPutc = swio->put_c;
  SWIWideGetc = swio->get_w;
  SWIWidePutc = swio->put_w;
  SWIFlush = swio->flush_s;
  SWIClose = swio->close_s;
  SWIGetStream = swio->get_stream_handle;
  SWIGetStreamPosition = swio->get_stream_position;
}

typedef int     (*GetStreamF)(term_t, int, int, IOSTREAM **s);

int 
Yap_get_stream_handle(Term t0, int read_mode, int write_mode, void *s){
  term_t t;
  GetStreamF f = (GetStreamF)SWIGetStream;
  if (t0 == MkAtomTerm(AtomUserOut) && write_mode && !read_mode) {
    t = 0;
  } else if (t0 == MkAtomTerm(AtomUserIn) && !write_mode && read_mode) {
    t = 0;
  } else {
    t = (term_t)YAP_InitSlot(t0);
  }
  return (*f)(t, read_mode, write_mode, s);
}


typedef int     (*GetStreamPosF)(IOSTREAM *s, term_t);

Term 
Yap_get_stream_position(void *s){
  term_t t;
  Term t0;
  GetStreamPosF f = (GetStreamPosF)SWIGetStreamPosition;

  t = (term_t)Yap_NewSlots(1);
  if (!(*f)(s, t))
    return 0L;
  t0 = Yap_GetFromSlot((Int)t);
  Yap_RecoverSlots(1);
  return t0;
}


X_API void (*PL_signal(int sig, void (*func)(int)))(int)
{
  //  return Yap_signal2(sig,func);
  return NULL;
}

X_API void PL_on_halt(void (*f)(int, void *), void *closure)
{
  Yap_HaltRegisterHook((HaltHookFunc)f,closure);
}

void
Yap_swi_install(void)
{
  Yap_install_blobs();
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

