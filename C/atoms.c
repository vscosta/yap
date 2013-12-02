/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V. Santos Costa and Universidade do Porto 1985--	 *
*									 *
**************************************************************************
*									 *
* File:		atoms.c							 *
* comments:	General-purpose C implemented system predicates		 *
*									 *
* Last rev:     $Date: 2008-07-24 16:02:00 $,$Author: vsc $	     	 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

#define HAS_CACHE_REGS 1
/*
 * This file includes the definition of a miscellania of standard operations
 * for yap refering to sequences of characters conversions.
 *
 */

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "eval.h"
#include "yapio.h"
#include "pl-shared.h"
#include "YapMirror.h"
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#include <stdio.h>
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <wchar.h>

static Int p_name( USES_REGS1 );
static Int p_atom_chars( USES_REGS1 );
static Int p_atom_codes( USES_REGS1 );
static Int p_atom_length( USES_REGS1 );
static Int p_string_length( USES_REGS1 );
static Int p_atom_split( USES_REGS1 );
static Int p_number_chars( USES_REGS1 );
static Int p_number_codes( USES_REGS1 );
static Int init_current_atom( USES_REGS1 );
static Int cont_current_atom( USES_REGS1 );

static Int 
p_char_code( USES_REGS1 )
{
  Int t0 = Deref(ARG1);
  if (IsVarTerm(t0)) {
    Term t1 = Deref(ARG2);
    if (IsVarTerm(t1)) {
      Yap_Error(INSTANTIATION_ERROR,t0,"char_code/2");
      return(FALSE);
    } else if (!IsIntegerTerm(t1)) {
      Yap_Error(TYPE_ERROR_INTEGER,t1,"char_code/2");
      return(FALSE);
    } else {
      Int code = IntegerOfTerm(t1);
      Term tout;

      if (code < 0) {
	Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE,t1,"char_code/2");
	return(FALSE);
      }
      if (code > MAX_ISO_LATIN1) {
	wchar_t wcodes[2];

	wcodes[0] = code;
	wcodes[1] = '\0';
	tout = MkAtomTerm(Yap_LookupWideAtom(wcodes));
      } else {
	char codes[2];

	codes[0] = code;
	codes[1] = '\0';
	tout = MkAtomTerm(Yap_LookupAtom(codes));
      }
      return Yap_unify(ARG1,tout);
    }
  } else if (!IsAtomTerm(t0)) {
    Yap_Error(TYPE_ERROR_CHARACTER,t0,"char_code/2");
    return(FALSE);
  } else {
    Atom at = AtomOfTerm(t0);
    Term tf;

    if (IsWideAtom(at)) {
      wchar_t *c = RepAtom(at)->WStrOfAE;
      
      if (c[1] != '\0') {
	Yap_Error(TYPE_ERROR_CHARACTER,t0,"char_code/2");
	return FALSE;
      }
      tf = MkIntegerTerm(c[0]);
    } else {
      char *c = RepAtom(at)->StrOfAE;
      
      if (c[1] != '\0') {
	Yap_Error(TYPE_ERROR_CHARACTER,t0,"char_code/2");
	return FALSE;
      }
      tf = MkIntTerm((unsigned char)(c[0]));
    }
    return Yap_unify(ARG2,tf);
  }
}

static Int 
p_name( USES_REGS1 )
{				/* name(?Atomic,?String)		 */
  Term            t = Deref(ARG2), NewT, AtomNameT = Deref(ARG1);
  LOCAL_MAX_SIZE = 1024;

 restart_aux:
  if (!IsVarTerm(AtomNameT)) {
    if (!IsVarTerm(t) && !IsPairTerm(t) && t != TermNil) {
      Yap_Error(TYPE_ERROR_LIST,ARG2,
		"name/2");
      return FALSE;
    }
    // verify if an atom, int, float or bi§gnnum
    NewT = Yap_AtomicToListOfCodes( AtomNameT PASS_REGS );
    if (NewT)
      return Yap_unify(NewT, ARG2);
    // else
  } else {
    Term at = Yap_ListToAtomic( t PASS_REGS );
    if (at) return Yap_unify(at, ARG1);
  }
  if (LOCAL_Error_TYPE && Yap_HandleError( "atom/2" )) {
    AtomNameT = Deref(ARG1);
    t = Deref(ARG2);
    goto restart_aux;
  }
  return FALSE;
}

static Int 
p_string_to_atom( USES_REGS1 )
{				/* string_to_atom(?String,?Atom)		 */
  Term            t2 = Deref(ARG2), t1 = Deref(ARG1);
  LOCAL_MAX_SIZE = 1024;

 restart_aux:
  if (!IsVarTerm(t1)) {
    Atom at;
    // verify if an atom, int, float or bignnum
    at = Yap_ListToAtom( t1 PASS_REGS );
    if (at)
      return Yap_unify(MkAtomTerm(at), t2);
    // else
  } else {
    Term t0 = Yap_AtomicToString( t2 PASS_REGS );
    if (t0) return Yap_unify(t0, t1);
  }
  if (LOCAL_Error_TYPE && Yap_HandleError( "string_to_atom/2" )) {
    t1 = Deref(ARG1);
    t2 = Deref(ARG2);
    goto restart_aux;
  }
  return FALSE;
}

static Int 
p_string_to_list( USES_REGS1 )
{				
  Term            list = Deref(ARG2), string = Deref(ARG1);
  LOCAL_MAX_SIZE = 1024;

 restart_aux:
  if (!IsVarTerm(list)) {
    Term t1 = Yap_ListToString( list PASS_REGS);
    if (t1)
      return Yap_unify( ARG1, t1 );
  } else {
    Term tf = Yap_AtomicToListOfCodes(string PASS_REGS);
    return Yap_unify( ARG2, tf );
  }
  if (LOCAL_Error_TYPE && Yap_HandleError( "string_to_list/2" )) {
    string = Deref(ARG1);
    list = Deref(ARG2);
    goto restart_aux;
  }
  return FALSE;
}

static Int 
p_atom_chars( USES_REGS1 )
{
  Term t1;
  LOCAL_MAX_SIZE = 1024;

 restart_aux:
  t1  = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    Term tf = Yap_AtomicToListOfAtoms(t1 PASS_REGS);
    if (tf)
      return Yap_unify( ARG2, tf );
  } else {
    /* ARG1 unbound */
    Term   t = Deref(ARG2);
    Atom af = Yap_ListToAtom(t PASS_REGS);
    if (af)
      return Yap_unify( ARG1, MkAtomTerm(af) );
  /* error handling */
  }
  if (LOCAL_Error_TYPE && Yap_HandleError( "atom_chars/2" )) {
    goto restart_aux;
  }
  return FALSE;
}

static Int 
p_atom_codes( USES_REGS1 )
{
  Term t1;
 restart_aux:
  t1  = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    Term tf = Yap_AtomicToListOfCodes(t1 PASS_REGS);
    if (tf)
      return Yap_unify( ARG2, tf );
  } else {
    /* ARG1 unbound */
    Term   t = Deref(ARG2);
    Atom af = Yap_ListToAtom(t PASS_REGS);
    if (af)
      return Yap_unify( ARG1, MkAtomTerm(af) );
  }
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "atom_codes/2" )) {
    t1 = Deref(ARG1);
    goto restart_aux;
  }
  return FALSE;
}


static Int 
p_number_chars( USES_REGS1 )
{
  Term t1;
 restart_aux:
  t1  = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    Term tf;
    tf = Yap_NumberToListOfAtoms(t1 PASS_REGS);
    if (tf)
      return Yap_unify( ARG2, tf );
  } else {
    /* ARG1 unbound */
    Term   t = Deref(ARG2);
    Term tf = Yap_ListToNumber(t PASS_REGS);
    if (tf)
      return Yap_unify( ARG1, tf );
  }
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "atom_chars/2" )) {
    goto restart_aux;
  }
  return FALSE;
}

static Int 
p_number_atom( USES_REGS1 )
{
  Term t1;
 restart_aux:
  t1  = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    Atom af;
    af = Yap_NumberToAtom(t1 PASS_REGS);
    if (af)
      return Yap_unify( ARG2, MkAtomTerm(af) );
  } else {
    /* ARG1 unbound */
    Term   t = Deref(ARG2);
    Term tf = Yap_ListToNumber(t PASS_REGS);
    if (tf)
      return Yap_unify( ARG1, tf );
  }
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "number_atom/2")) {
    goto restart_aux;
  }
  return FALSE;
}

static Int 
p_number_codes( USES_REGS1 )
{
  Term t1;
 restart_aux:
  t1  = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    Term tf;
    tf = Yap_NumberToListOfCodes(t1 PASS_REGS);
    if (tf)
      return Yap_unify( ARG2, tf );
  } else {
    /* ARG1 unbound */
    Term   t = Deref(ARG2);
    Term tf = Yap_ListToNumber(t PASS_REGS);
    if (tf)
      return Yap_unify( ARG2, tf );
  }
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "number_codes/2" )) {
    goto restart_aux;
  }
  return FALSE;
}

static Int 
p_atom_concat( USES_REGS1 )
{
  Term t1;
  int wide_mode = FALSE;

 restart:
  t1 = Deref(ARG1);
  /* we need to have a list */
  if (IsVarTerm(t1)) {
    Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
    Yap_Error(INSTANTIATION_ERROR, ARG1, "atom_concat/2");
    return FALSE;
  }
  if (wide_mode) {
    wchar_t *cptr = (wchar_t *)(((AtomEntry *)Yap_PreAllocCodeSpace())->StrOfAE), *cpt0;
    wchar_t *top = (wchar_t *)AuxSp;
    unsigned char *atom_str = NULL;
    Atom ahead;
    UInt sz;

    cpt0 = cptr;
    while (IsPairTerm(t1)) {
      Term thead = HeadOfTerm(t1);
      if (IsVarTerm(thead)) {
	Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	Yap_Error(INSTANTIATION_ERROR, ARG1, "atom_concat/2");
	return(FALSE);
      }
      if (!IsAtomTerm(thead)) {
	Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	Yap_Error(TYPE_ERROR_ATOM, ARG1, "atom_concat/2");
	return(FALSE);
      }
      ahead = AtomOfTerm(thead);
      if (IsWideAtom(ahead)) {
	/* check for overflows */
	sz = wcslen(RepAtom(ahead)->WStrOfAE);
      } else {
	atom_str = (unsigned char *)RepAtom(ahead)->StrOfAE;
	sz = strlen((char *)atom_str);
      }
      if (cptr+sz > top+1024) {
	cptr = (wchar_t *)Yap_ExpandPreAllocCodeSpace(sz+1024,NULL, TRUE);
	if (cptr+sz > (wchar_t *)AuxSp+1024) {
	  /* crash in flames */
	  Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "allocating temp space in atom_concat/2");
	  return FALSE;
	}
	top = (wchar_t *)AuxSp;
	goto restart;
      }
      if (IsWideAtom(ahead)) {
	memcpy((void *)cptr, RepAtom(ahead)->WStrOfAE, sz*sizeof(wchar_t));
	cptr += sz;
      } else {
	UInt i;

	for (i=0; i < sz; i++) {
	  *cptr++ = *atom_str++;
	}
      }
      t1 = TailOfTerm(t1);
      if (IsVarTerm(t1)) {
	Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	Yap_Error(INSTANTIATION_ERROR, ARG1, "atom_concat/2");
	return FALSE;
      }
    }
    if (t1 == TermNil) {
      Atom at;
      
      cptr[0] = '\0';
      while ((at = Yap_LookupWideAtom(cpt0)) == NIL) {
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}
      }
      Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
      return Yap_unify(ARG2, MkAtomTerm(at));
    }
  } else {
    char *cptr = ((AtomEntry *)Yap_PreAllocCodeSpace())->StrOfAE, *cpt0;
    char *top = (char *)AuxSp;
    unsigned char *atom_str;
    UInt sz;

    cpt0 = cptr;
    while (IsPairTerm(t1)) {
      Term thead = HeadOfTerm(t1);
      if (IsVarTerm(thead)) {
	Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	Yap_Error(INSTANTIATION_ERROR, ARG1, "atom_concat/2");
	return(FALSE);
      }
      if (!IsAtomTerm(thead)) {
	Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	Yap_Error(TYPE_ERROR_ATOM, ARG1, "atom_concat/2");
	return(FALSE);
      }
      if (IsWideAtom(AtomOfTerm(thead)) && !wide_mode) {
	wide_mode = TRUE;
	Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	goto restart;
      }
      atom_str = (unsigned char *)RepAtom(AtomOfTerm(thead))->StrOfAE;
      /* check for overflows */
      sz = strlen((char *)atom_str);
      if (cptr+sz >= top-1024) {
	Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	if (!Yap_growheap(FALSE, sz+1024, NULL)) {
	  Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}
	goto restart;
      }
      memcpy((void *)cptr, (void *)atom_str, sz);
      cptr += sz;
      t1 = TailOfTerm(t1);
      if (IsVarTerm(t1)) {
	Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	Yap_Error(INSTANTIATION_ERROR, ARG1, "atom_concat/2");
	return FALSE;
      }
    }
    if (t1 == TermNil) {
      Atom at;
      
      cptr[0] = '\0';
      while ((at = Yap_LookupAtom(cpt0)) == NIL) {
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}
      }
      Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
      return Yap_unify(ARG2, MkAtomTerm(at));
    }
  }
  Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
  Yap_Error(TYPE_ERROR_LIST, ARG1, "atom_concat/2");
  return FALSE;
}

static Int 
p_atomic_concat( USES_REGS1 )
{
  Term t1;
  int wide_mode = FALSE;
  char *base;

 restart:
  base = ((AtomEntry *)Yap_PreAllocCodeSpace())->StrOfAE;
  while (base+1024 > (char *)AuxSp) {
    base = Yap_ExpandPreAllocCodeSpace(0,NULL, TRUE);
    if (base + 1024 > (char *)AuxSp) {
      /* crash in flames */
      Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "allocating temp space in atomic_concat/2");
      return FALSE;
    }
  }
  t1 = Deref(ARG1);
  /* we need to have a list */
  if (IsVarTerm(t1)) {
    Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
    Yap_Error(INSTANTIATION_ERROR, ARG1, "atom_concat/2");
    return FALSE;
  }
  if (wide_mode) {
    wchar_t *wcptr = (wchar_t *)base, *wcpt0;
    wchar_t *wtop = (wchar_t *)AuxSp;
    
    wcpt0 = wcptr;
    while (IsPairTerm(t1)) {
      Term thead = HeadOfTerm(t1);
      if (IsVarTerm(thead)) {
	Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	Yap_Error(INSTANTIATION_ERROR, ARG1, "atom_concat/2");
	return FALSE;
      }
      if (!IsAtomicTerm(thead)) {
	Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	Yap_Error(TYPE_ERROR_ATOMIC, ARG1, "atom_concat/2");
	return FALSE;
      }
      if (IsAtomTerm(thead)) {
	Atom at = AtomOfTerm(thead);

	if (IsWideAtom(at)) {
	  wchar_t *watom_str = (wchar_t *)RepAtom(AtomOfTerm(thead))->StrOfAE;
	  UInt sz = wcslen(watom_str);

	  if (wcptr+sz >= wtop-1024) {
	    Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	    if (!Yap_growheap(FALSE, sz+1024, NULL)) {
	      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
	      return FALSE;
	    }
	    goto restart;
	  }
	  memcpy((void *)wcptr, (void *)watom_str, sz*sizeof(wchar_t));
	  wcptr += sz;
	} else {
	  unsigned char *atom_str = (unsigned char *)RepAtom(AtomOfTerm(thead))->StrOfAE;
	  /* check for overflows */
	  UInt sz = strlen((char *)atom_str);
	  if (wcptr+sz >= wtop-1024) {
	    Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	    if (!Yap_growheap(FALSE, sz+1024, NULL)) {
	      Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
	      return FALSE;
	    }
	    goto restart;
	  }
	  while ((*wcptr++ = *atom_str++));
	  wcptr--;
	} 
      } else if (IsIntegerTerm(thead)) {
	UInt sz, i;
	char *cptr = (char *)wcptr;

#if HAVE_SNPRINTF
	sz = snprintf(cptr, (wtop-wcptr)-1024,Int_FORMAT, IntegerOfTerm(thead));
#else
	sz = sprintf(cptr,Int_FORMAT, IntegerOfTerm(thead));
#endif
	for (i=sz; i>0; i--) {
	  wcptr[i-1] = cptr[i-1];
	}
	wcptr += sz;
      } else if (IsFloatTerm(thead)) {
	char *cptr = (char *)wcptr;
	UInt i, sz;

#if HAVE_SNPRINTF
	sz = snprintf(cptr,(wtop-wcptr)-1024,"%g", FloatOfTerm(thead));
#else
	sz = sprintf(cptr,"%g", FloatOfTerm(thead));
#endif
	for (i=sz; i>0; i--) {
	  wcptr[i-1] = cptr[i-1];
	}
	wcptr += sz;
#if USE_GMP
      } else if (IsBigIntTerm(thead)) {
	size_t sz, i;
	char *tmp = (char *)wcptr;

	sz = Yap_gmp_to_size(thead, 10);
	if (!Yap_gmp_to_string(thead, tmp, (wtop-wcptr)-1024, 10 )) {
	  Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	  if (!Yap_growheap(FALSE, sz+1024, NULL)) {
	    Yap_Error(OUT_OF_AUXSPACE_ERROR, TermNil, LOCAL_ErrorMessage);
	    return(FALSE);
	  }
	  goto restart;
	}
	for (i=sz; i>0; i--) {
	  wcptr[i-1] = tmp[i-1];
	}
	wcptr += sz;
#endif
      }
      t1 = TailOfTerm(t1);
      if (IsVarTerm(t1)) {
	Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	Yap_Error(INSTANTIATION_ERROR, ARG1, "atom_concat/2");
	return(FALSE);
      }
    }
    if (t1 == TermNil) {
      Atom at;

      wcptr[0] = '\0';
      while ((at = Yap_LookupWideAtom(wcpt0)) == NIL) {
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}
      }
      Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
      return Yap_unify(ARG2, MkAtomTerm(at));
    }
  } else {
    char *top = (char *)AuxSp;
    char *cpt0 = base;
    char *cptr = base;

    while (IsPairTerm(t1)) {
      Term thead = HeadOfTerm(t1);
      if (IsVarTerm(thead)) {
	Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	Yap_Error(INSTANTIATION_ERROR, ARG1, "atom_concat/2");
	return(FALSE);
      }
      if (!IsAtomicTerm(thead)) {
	Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	Yap_Error(TYPE_ERROR_ATOMIC, ARG1, "atom_concat/2");
	return(FALSE);
      }
      if (IsAtomTerm(thead)) {
	unsigned char *atom_str;
	UInt sz;

	if (IsWideAtom(AtomOfTerm(thead))) {
	  Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	  wide_mode = TRUE;
	  goto restart;
	}
	atom_str = (unsigned char *)RepAtom(AtomOfTerm(thead))->StrOfAE;
	/* check for overflows */
	sz = strlen((char *)atom_str);
	if (cptr+sz >= top-1024) {
	  Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	  if (!Yap_growheap(FALSE, sz+1024, NULL)) {
	    Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
	    return(FALSE);
	  }
	  goto restart;
	} 
	memcpy((void *)cptr, (void *)atom_str, sz);
	cptr += sz;
      } else if (IsIntegerTerm(thead)) {
#if HAVE_SNPRINTF
	snprintf(cptr, (top-cptr)-1024,Int_FORMAT, IntegerOfTerm(thead));
#else
	sprintf(cptr, Int_FORMAT, IntegerOfTerm(thead));
#endif
	while (*cptr && cptr < top-1024) cptr++;
      } else if (IsFloatTerm(thead)) {
#if HAVE_SNPRINTF
	snprintf(cptr,(top-cptr)-1024,"%g", FloatOfTerm(thead));
#else
	sprintf(cptr,"%g", FloatOfTerm(thead));
#endif
	while (*cptr && cptr < top-1024) cptr++;
#if USE_GMP
      } else if (IsBigIntTerm(thead)) {
	if (!Yap_gmp_to_string(thead, cptr, (top-cptr)-1024, 10 )) {
	  size_t sz = Yap_gmp_to_size(thead, 10);
	  Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	  if (!Yap_growheap(FALSE, sz+1024, NULL)) {
	    Yap_Error(OUT_OF_AUXSPACE_ERROR, TermNil, LOCAL_ErrorMessage);
	    return(FALSE);
	  }
	  goto restart;
	}
	while (*cptr) cptr++;
#endif
      }
      t1 = TailOfTerm(t1);
      if (IsVarTerm(t1)) {
	Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
	Yap_Error(INSTANTIATION_ERROR, ARG1, "atom_concat/2");
	return(FALSE);
      }
    }
    if (t1 == TermNil) {
      Atom at;

      cptr[0] = '\0';
      while ((at = Yap_LookupAtom(cpt0)) == NIL) {
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}
      }
      Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
      return Yap_unify(ARG2, MkAtomTerm(at));
    }
  }
  Yap_ReleasePreAllocCodeSpace((ADDR)cpt0);
  Yap_Error(TYPE_ERROR_LIST, ARG1, "atom_concat/2");
  return(FALSE);
}

static Int 
p_atom_length( USES_REGS1 )
{
  Term t1;
  Term t2 = Deref(ARG2);
  if (!IsVarTerm(t2)) {
    if (!IsIntegerTerm(t2)) {
      Yap_Error(TYPE_ERROR_INTEGER, t2, "atom_length/2");
      return(FALSE);
    }
    if ((len = IntegerOfTerm(t2)) < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t2, "atom_length/2");
      return(FALSE);
    }
  }
restart_aux:
  t1  = Deref(ARG1);
  tf = Yap_TextToUTF8(t1 PASS_REGS);
  if (tf)
    return Yap_unify( ARG2, utf8_strlen(tf) );
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "atom_length/2" )) {
    goto restart_aux;
  }
  return FALSE;
}


static int
is_wide(wchar_t *s)
{
  wchar_t ch;

  while ((ch = *s++)) {
    if (ch > MAX_ISO_LATIN1)
      return TRUE;
  }
  return FALSE;
}

/* split an atom into two sub-atoms */
static Int 
p_atom_split( USES_REGS1 )
{
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  size_t len;
  int i;
  Term to1, to2;
  Atom at;

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "$atom_split/4");
    return(FALSE);		
  }
  if (!IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM, t1, "$atom_split/4");
    return(FALSE);
  }
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "$atom_split/4");
    return(FALSE);		
  }
  if (!IsIntTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "$atom_split/4");
    return(FALSE);
  }
  if ((Int)(len = IntOfTerm(t2)) < 0) {
    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t2, "$atom_split/4");
    return(FALSE);
  }
  at  = AtomOfTerm(t1);
  if (IsWideAtom(at)) {
    wchar_t *ws, *ws1 = (wchar_t *)H;
    char *s1 = (char *)H;
    size_t wlen;

    ws = (wchar_t *)RepAtom(at)->StrOfAE;
    wlen = wcslen(ws);
    if (len > wlen) return FALSE;
    if (s1+len > (char *)LCL0-1024)
      Yap_Error(OUT_OF_STACK_ERROR,t1,"$atom_split/4");
    for (i = 0; i< len; i++) {
      if (ws[i] > MAX_ISO_LATIN1) {
	break;
      }
      s1[i] = ws[i];
    }
    if (ws1[i] > MAX_ISO_LATIN1) {
      /* first sequence is wide */
      if (ws1+len > (wchar_t *)ASP-1024)
	Yap_Error(OUT_OF_STACK_ERROR,t1,"$atom_split/4");
      ws = (wchar_t *)RepAtom(at)->StrOfAE;
      for (i = 0; i< len; i++) {
	ws1[i] = ws[i];
      }
      ws1[len] = '\0';
      to1 = MkAtomTerm(Yap_LookupWideAtom(ws1));
      /* we don't know if the rest of the string is wide or not */
      if (is_wide(ws+len)) {
	to2 = MkAtomTerm(Yap_LookupWideAtom(ws+len)); 
      } else {
	char *s2 = (char *)H;
	if (s2+(wlen-len) > (char *)ASP-1024)
	  Yap_Error(OUT_OF_STACK_ERROR,t1,"$atom_split/4");
	ws += len;
	while ((*s2++ = *ws++));
	to2 = MkAtomTerm(Yap_LookupAtom((char *)H)); 	
      }
    } else {
      s1[len] = '\0';
      to1 = MkAtomTerm(Yap_LookupAtom(s1));
      /* second atom must be wide, if first wasn't */
      to2 = MkAtomTerm(Yap_LookupWideAtom(ws+len)); 
    }
  } else {
    char *s, *s1 = (char *)H;

    s = RepAtom(at)->StrOfAE;
    if (len > (Int)strlen(s)) return(FALSE);
    if (s1+len > (char *)ASP-1024)
      Yap_Error(OUT_OF_STACK_ERROR,t1,"$atom_split/4");
    for (i = 0; i< len; i++) {
      s1[i] = s[i];
    }
    s1[len] = '\0';
    to1 = MkAtomTerm(Yap_LookupAtom(s1));
    to2 = MkAtomTerm(Yap_LookupAtom(s+len)); 
  }
  return(Yap_unify_constant(ARG3,to1) && Yap_unify_constant(ARG4,to2));
}

static Int 
p_atom_number( USES_REGS1 )
{
  Term t1;
 restart_aux:
  t1  = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    Term tf = Yap_AtomToNumber(t1 PASS_REGS);
    if (tf)
      return Yap_unify( ARG2, tf );
  } else {
    /* ARG1 unbound */
    Term   t = Deref(ARG2);
    Atom af = Yap_NumberToAtom(t PASS_REGS);
    if (af)
      return Yap_unify( ARG1, MkAtomTerm(af) );
  }
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "atom_codes/2" )) {
    t1 = Deref(ARG1);
    goto restart_aux;
  }
  return FALSE;
}


#define SUB_ATOM_HAS_MIN    1
#define SUB_ATOM_HAS_SIZE   2
#define SUB_ATOM_HAS_AFTER  4
#define SUB_ATOM_HAS_VAL    8
#define SUB_ATOM_HAS_WIDE  16

static void *
alloc_tmp_stack(size_t sz USES_REGS) {
  void *pt = (void *)H;
  while (H > ASP-(1044+sz/sizeof(CELL))) {
    if (!Yap_gc(5, ENV, gc_P(P,CP))) {
      Yap_Error(OUT_OF_STACK_ERROR, TermNil, "sub_atom/5");
      return(NULL);
    }
  }
  return pt;
}

static Atom 
build_new_atom(int mask, wchar_t *wp, char *p, size_t min, size_t len USES_REGS)
{
  Atom nat;
  if (mask & SUB_ATOM_HAS_WIDE) {
    wchar_t *src = wp+min;
    wchar_t *d = alloc_tmp_stack((len+1)*sizeof(wchar_t) PASS_REGS);
    if (!d) return NIL;
    
    wcsncpy(d, src, len);
    d[len] = '\0';
    nat = Yap_LookupMaybeWideAtom(d);
  } else {
    char *src = p+min;
    char *d = alloc_tmp_stack((len+1)*sizeof(char) PASS_REGS);
    if (!d) return NIL;
    
    strncpy(d, src, len);
    d[len] = '\0';
    nat = Yap_LookupAtom(d);
  }
  return nat;
}

static Int wcsstrcmp(wchar_t *p, char *p2, size_t len)
{
  while (len--) {
    Int d = *p++-*p2++;
    if (d) return d;
  }
  return 0;
}

static int
check_sub_atom_at(int min, Atom at, Atom nat)
{
  if (IsWideAtom(nat)) {
    wchar_t *p1, *p2;
    wchar_t c1;
    if (!IsWideAtom(at)) return FALSE;
    p1 = RepAtom(at)->WStrOfAE+min;
    p2 = RepAtom(nat)->WStrOfAE;
    while ( (c1 = *p1++) == *p2++ && c1);
    return c1 == 0;
  } else {
    if (IsWideAtom(at)) {
      wchar_t *p1;
      char *p2;
      wchar_t c1;
      p1 = RepAtom(at)->WStrOfAE+min;
      p2 = RepAtom(nat)->StrOfAE;
      while ( (c1 = *p1++) == *p2++ && c1);
      return c1 == 0;
    } else {
      char *p1, *p2;
      char c1;
      p1 = RepAtom(at)->StrOfAE+min;
      p2 = RepAtom(nat)->StrOfAE;
      while ( (c1 = *p1++) == *p2++ && c1);
      return c1 == 0;
    }
  }
}

static int
check_sub_atom_bef(int max, Atom at, Atom nat)
{
  if (IsWideAtom(nat)) {
    wchar_t *p1, *p2;
    wchar_t c1;

    size_t len = wcslen(RepAtom(nat)->WStrOfAE);
    int min = max- len;
    if (min < 0) return FALSE;
    if (!IsWideAtom(at)) return FALSE;
    p1 = RepAtom(at)->WStrOfAE+min;
    p2 = RepAtom(nat)->WStrOfAE;
    while ( (c1 = *p1++) == *p2++ && c1);
    return c1 == 0;
  } else {
    size_t len = strlen(RepAtom(nat)->StrOfAE);
    int min = max- len;
    if ((Int)(min - len) < 0) return FALSE;
    if (IsWideAtom(at)) {
      wchar_t *p1;
      char *p2;
      wchar_t c1;
      p1 = RepAtom(at)->WStrOfAE+min;
      p2 = RepAtom(nat)->StrOfAE;
      while ( (c1 = *p1++) == *p2++ && c1);
      return c1 == 0;
    } else {
      char *p1, *p2;
      char c1;
      p1 = RepAtom(at)->StrOfAE+min;
      p2 = RepAtom(nat)->StrOfAE;
      while ( (c1 = *p1++) == *p2++ && c1);
      return c1 == 0;
    }
  }
}

static Int
cont_sub_atom( USES_REGS1 )
{
  Term tat1= Deref(ARG1);
  Atom at = AtomOfTerm(tat1);
  int mask;
  size_t min, len, after, sz;
  wchar_t *wp = NULL;
  char *p = NULL;
  Atom nat;

  mask = IntegerOfTerm(EXTRA_CBACK_ARG(5,1));
  min = IntegerOfTerm(EXTRA_CBACK_ARG(5,2));
  len = IntegerOfTerm(EXTRA_CBACK_ARG(5,3));
  after = IntegerOfTerm(EXTRA_CBACK_ARG(5,4));
  sz = IntegerOfTerm(EXTRA_CBACK_ARG(5,5));

  if (mask & SUB_ATOM_HAS_WIDE) {
    wp = RepAtom(at)->WStrOfAE;
  } else {
    p = RepAtom(at)->StrOfAE;
  }
  /* we can have one of two cases: A5 bound or unbound */
  if (mask & SUB_ATOM_HAS_VAL) {
    int found = FALSE;
    nat = AtomOfTerm(Deref(ARG5));
    if (mask & SUB_ATOM_HAS_WIDE) {
      wp = RepAtom(at)->WStrOfAE;
      if (IsWideAtom(nat)) {
	while (!found) {
	  if (wcsncmp(wp+min, nat->WStrOfAE, len) == 0) {
	    Yap_unify(ARG2, MkIntegerTerm(min));
	    Yap_unify(ARG3, MkIntegerTerm(len));
	    Yap_unify(ARG4, MkIntegerTerm(after));
	    found = TRUE;
	    /* found one, check if there is any left */
	    while (min <= sz-len) {
	      after--;
	      min++;
	      if (wcsncmp(wp+min, nat->WStrOfAE, len) == 0)
		break;
	    }
	  } else {
	    if (min == sz-len) break;
	    after--;
	    min++;
	  }
	}
      } else {
	while (!found) {
	  if (wcsstrcmp(wp+min, nat->StrOfAE, len) == 0) {
	    Yap_unify(ARG2, MkIntegerTerm(min));
	    Yap_unify(ARG3, MkIntegerTerm(len));
	    Yap_unify(ARG4, MkIntegerTerm(after));
	    found = TRUE;
	    /* found one, check if there is any left */
	    while (min <= sz-len) {
	      after--;
	      min++;
	      if (wcsstrcmp(wp+min, nat->StrOfAE, len) == 0)
		break;
	    }
	  } else {
	    if (min == sz-len) break;
	    after--;
	    min++;
	  }
	} 
      }     
    } else {
      p = RepAtom(at)->StrOfAE;
      while (!found) {
	if (strncmp(p+min, nat->StrOfAE, len) == 0) {
	  Yap_unify(ARG2, MkIntegerTerm(min));
	  Yap_unify(ARG3, MkIntegerTerm(len));
	  Yap_unify(ARG4, MkIntegerTerm(after));
	  found = TRUE;
	  /* found one, check if there is any left */
	  while (min <= sz-len) {
	    after--;
	    min++;
	    if (strncmp(p+min, nat->StrOfAE, len) == 0)
	      break;
	  }
	} else {
	  if (min == sz-len) break;
	  after--;
	  min++;
	}
      }
    }
    if (found) {
      if (min > sz-len) cut_succeed();
    } else {
      cut_fail();
    }
  } else if (mask & SUB_ATOM_HAS_SIZE) {
    nat = build_new_atom(mask, wp, p, min, len PASS_REGS);
    Yap_unify(ARG2, MkIntegerTerm(min));
    Yap_unify(ARG4, MkIntegerTerm(after));
    Yap_unify(ARG5, MkAtomTerm(nat));
    min++;
    if (after-- == 0) cut_succeed();
  } else if (mask & SUB_ATOM_HAS_MIN) {
    after = sz-(min+len);
    nat = build_new_atom(mask, wp, p, min, len PASS_REGS);
    Yap_unify(ARG3, MkIntegerTerm(len));
    Yap_unify(ARG4, MkIntegerTerm(after));
    Yap_unify(ARG5, MkAtomTerm(nat));
    len++;
    if (after-- == 0) cut_succeed();    
  } else if (mask & SUB_ATOM_HAS_AFTER) {
    len = sz-(min+after);
    nat = build_new_atom(mask, wp, p, min, len PASS_REGS);
    Yap_unify(ARG2, MkIntegerTerm(min));
    Yap_unify(ARG3, MkIntegerTerm(len));
    Yap_unify(ARG5, MkAtomTerm(nat));
    min++;
    if (len-- == 0) cut_succeed();    
  } else {
    nat = build_new_atom(mask, wp, p, min, len PASS_REGS);
    Yap_unify(ARG2, MkIntegerTerm(min));
    Yap_unify(ARG3, MkIntegerTerm(len));
    Yap_unify(ARG4, MkIntegerTerm(after));
    Yap_unify(ARG5, MkAtomTerm(nat));
    len++;
    if (after-- == 0) {
      if (min == sz) cut_succeed();
      min++;
      len = 0;
      after = sz-min;
    }
  }
  EXTRA_CBACK_ARG(5,1) = MkIntegerTerm(mask);
  EXTRA_CBACK_ARG(5,2) = MkIntegerTerm(min);
  EXTRA_CBACK_ARG(5,3) = MkIntegerTerm(len);
  EXTRA_CBACK_ARG(5,4) = MkIntegerTerm(after);   
  EXTRA_CBACK_ARG(5,5) = MkIntegerTerm(sz); 
  return TRUE;
}

static Int
init_sub_atom( USES_REGS1 )
{
  Term tat1, tbef, tsize, tafter, tout;
  int mask = 0;
  size_t min, len, after, sz;
  wchar_t *wp = NULL;
  char *p = NULL;
  int bnds = 0;
  Atom nat = NIL, at;

  tat1 = Deref(ARG1);  
  tbef = Deref(ARG5);  
  EXTRA_CBACK_ARG(5,3) = MkIntegerTerm(0);
  if (IsVarTerm(tat1)) {
    Yap_Error(INSTANTIATION_ERROR, tat1, "sub_atom/5: first argument");
    return FALSE;
  } else if (!IsAtomTerm(tat1)) {
    Yap_Error(TYPE_ERROR_ATOM, tat1, "sub_atom/5");
    return FALSE;
  }
  if (IsVarTerm(tbef = Deref(ARG2))) {
    min = 0;
  } else if (!IsIntegerTerm(tbef)) {
    Yap_Error(TYPE_ERROR_INTEGER, tbef, "sub_atom/5");
    return FALSE;
  } else {
    min = IntegerOfTerm(tbef);
    mask |= SUB_ATOM_HAS_MIN;
    bnds++;
  }
  if (IsVarTerm(tsize = Deref(ARG3))) {
    len = 0;
  } else if (!IsIntegerTerm(tsize)) {
    Yap_Error(TYPE_ERROR_INTEGER, tsize, "sub_atom/5");
    return FALSE;
  } else {
    len = IntegerOfTerm(tsize);
    mask |= SUB_ATOM_HAS_SIZE;
    bnds++;
  }
  if (IsVarTerm(tafter = Deref(ARG4))) {
    after = 0;
  } else if (!IsIntegerTerm(tafter)) {
    Yap_Error(TYPE_ERROR_INTEGER, tafter, "sub_atom/5");
    return FALSE;
  } else {
    after = IntegerOfTerm(tafter);
    mask |= SUB_ATOM_HAS_AFTER;
    bnds++;
  }
  if (!IsVarTerm(tout = Deref(ARG5))) {
    if (!IsAtomTerm(tout)) {
      Yap_Error(TYPE_ERROR_ATOM, tout, "sub_atom/5");
      return FALSE;
    } else {
      mask |= SUB_ATOM_HAS_VAL|SUB_ATOM_HAS_SIZE;
      nat = AtomOfTerm(tout);
      if (IsWideAtom(nat)) 
	len = wcslen(RepAtom(nat)->WStrOfAE);
      else
	len = strlen(RepAtom(nat)->StrOfAE);
      if (!Yap_unify(ARG3, MkIntegerTerm(len)))
	cut_fail();
      bnds+=2;
    }
  }
  at = AtomOfTerm(tat1);
  if (IsWideAtom(at)) {
    mask |= SUB_ATOM_HAS_WIDE;
    wp = RepAtom(at)->WStrOfAE;
    sz = wcslen(wp);
  } else {
    p = RepAtom(at)->StrOfAE; 
    sz = strlen(p);
  }
  /* the problem is deterministic if we have two cases */
  if (bnds > 1) {
    int out = FALSE;

    if ((mask & (SUB_ATOM_HAS_MIN|SUB_ATOM_HAS_SIZE)) ==
	(SUB_ATOM_HAS_MIN|SUB_ATOM_HAS_SIZE)) {
      if (min+len > sz) cut_fail();
      if ((Int)(after = (sz-(min+len))) < 0) cut_fail();
      nat = build_new_atom(mask, wp, p, min, len PASS_REGS);
      if (!nat) cut_fail();
      out = Yap_unify(ARG4,MkIntegerTerm(after)) &&
	Yap_unify(ARG5, MkAtomTerm(nat));
    } else if ((mask & (SUB_ATOM_HAS_MIN|SUB_ATOM_HAS_AFTER)) ==
	       (SUB_ATOM_HAS_MIN|SUB_ATOM_HAS_AFTER)) {
      if (sz < min+after) cut_fail();
      len = sz-(min+after);
      nat = build_new_atom(mask, wp, p, min, len PASS_REGS);
      if (!nat) cut_fail();
      out = Yap_unify(ARG3,MkIntegerTerm(len)) &&
	Yap_unify(ARG5, MkAtomTerm(nat));
    } else if ((mask & (SUB_ATOM_HAS_SIZE|SUB_ATOM_HAS_AFTER)) ==
	      (SUB_ATOM_HAS_SIZE|SUB_ATOM_HAS_AFTER) ) {
      if (len+after > sz) cut_fail();
      min = sz-(len+after);
      nat = build_new_atom(mask, wp, p, min, len PASS_REGS);
      if (!nat) cut_fail();
      out = Yap_unify(ARG2,MkIntegerTerm(min)) &&
	Yap_unify(ARG5, MkAtomTerm(nat));
    } else if ((mask & (SUB_ATOM_HAS_MIN|SUB_ATOM_HAS_VAL)) ==
	       (SUB_ATOM_HAS_MIN|SUB_ATOM_HAS_VAL)) {
      out = check_sub_atom_at(min, at, nat);
    } else if ((mask & (SUB_ATOM_HAS_AFTER|SUB_ATOM_HAS_VAL)) ==
	       (SUB_ATOM_HAS_AFTER|SUB_ATOM_HAS_VAL)) {
      out = check_sub_atom_bef(sz - after, at, nat);
    } else if ((mask & (SUB_ATOM_HAS_SIZE|SUB_ATOM_HAS_VAL)) ==
	        (SUB_ATOM_HAS_SIZE|SUB_ATOM_HAS_VAL)) {
      if (IsWideAtom(nat)) {
	if (!(mask & SUB_ATOM_HAS_VAL)) {
	  cut_fail();
	}
	/* just check length, they may still be several occurrences :( */
	out = (wcslen(RepAtom(nat)->WStrOfAE) == len);
      } else {
	out = (strlen(RepAtom(nat)->StrOfAE) == len);
	if (!out) cut_fail();
      }
      if (len == sz) {
	out =  out && 
	  Yap_unify(ARG1, ARG5) &&
	  Yap_unify(ARG2, MkIntegerTerm(0)) &&
	  Yap_unify(ARG4, MkIntegerTerm(0));
      } else if (len > sz) {
	cut_fail();
      } else {
	mask |= SUB_ATOM_HAS_SIZE;
	min = 0;
	after = sz-len;
	goto backtrackable;
      }
    }
    if (out) cut_succeed();
    cut_fail();
  } else {
    if (!(mask & SUB_ATOM_HAS_MIN)) min = 0;
    if (!(mask & SUB_ATOM_HAS_SIZE)) len = 0;
    if (!(mask & SUB_ATOM_HAS_AFTER)) after = sz-(len+min);
  }
 backtrackable:
  EXTRA_CBACK_ARG(5,1) = MkIntegerTerm(mask);
  EXTRA_CBACK_ARG(5,2) = MkIntegerTerm(min);
  EXTRA_CBACK_ARG(5,3) = MkIntegerTerm(len);
  EXTRA_CBACK_ARG(5,4) = MkIntegerTerm(after);   
  EXTRA_CBACK_ARG(5,5) = MkIntegerTerm(sz); 
  return cont_sub_atom( PASS_REGS1 );
}


static Int 
cont_current_atom( USES_REGS1 )
{
  Atom            catom;
  Int             i = IntOfTerm(EXTRA_CBACK_ARG(1,2));
  AtomEntry       *ap; /* nasty hack for gcc on hpux */

  /* protect current hash table line */
  if (IsAtomTerm(EXTRA_CBACK_ARG(1,1)))
    catom = AtomOfTerm(EXTRA_CBACK_ARG(1,1));
  else
    catom = NIL;
  if (catom == NIL){
    i++;
    /* move away from current hash table line */
    while (i < AtomHashTableSize) {
      READ_LOCK(HashChain[i].AERWLock);
      catom = HashChain[i].Entry;
      READ_UNLOCK(HashChain[i].AERWLock);
      if (catom != NIL) {
	break;
      }
      i++;
    }
    if (i == AtomHashTableSize) {
      cut_fail();
    }
  }
  ap = RepAtom(catom);
  if (Yap_unify_constant(ARG1, MkAtomTerm(catom))) {
    READ_LOCK(ap->ARWLock);
    if (ap->NextOfAE == NIL) {
      READ_UNLOCK(ap->ARWLock);
      i++;
      while (i < AtomHashTableSize) {
	READ_LOCK(HashChain[i].AERWLock);
	catom = HashChain[i].Entry;
	READ_UNLOCK(HashChain[i].AERWLock);
	if (catom != NIL) {
	  break;
	}
	i++;
      }
      if (i == AtomHashTableSize) {
	cut_fail();
      } else {
	EXTRA_CBACK_ARG(1,1) = MkAtomTerm(catom);
      }
    } else {
      EXTRA_CBACK_ARG(1,1) = MkAtomTerm(ap->NextOfAE);
      READ_UNLOCK(ap->ARWLock);
    }
    EXTRA_CBACK_ARG(1,2) = MkIntTerm(i);
    return TRUE;
  } else {
    return FALSE;
  }
}

static Int 
init_current_atom( USES_REGS1 )
{				/* current_atom(?Atom)		 */
  Term t1 = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    if (IsAtomTerm(t1))
      cut_succeed();
    else
      cut_fail();
  }
  READ_LOCK(HashChain[0].AERWLock);
  if (HashChain[0].Entry != NIL) {
    EXTRA_CBACK_ARG(1,1) = MkAtomTerm(HashChain[0].Entry);
  } else {
    EXTRA_CBACK_ARG(1,1) = MkIntTerm(0);
  }
  READ_UNLOCK(HashChain[0].AERWLock);
  EXTRA_CBACK_ARG(1,2) = MkIntTerm(0);
  return (cont_current_atom( PASS_REGS1 ));
}


static Int 
cont_current_wide_atom( USES_REGS1 )
{
  Atom            catom;
  Int             i = IntOfTerm(EXTRA_CBACK_ARG(1,2));
  AtomEntry       *ap; /* nasty hack for gcc on hpux */

  /* protect current hash table line */
  if (IsAtomTerm(EXTRA_CBACK_ARG(1,1)))
    catom = AtomOfTerm(EXTRA_CBACK_ARG(1,1));
  else
    catom = NIL;
  if (catom == NIL){
    i++;
    /* move away from current hash table line */
    while (i < WideAtomHashTableSize) {
      READ_LOCK(WideHashChain[i].AERWLock);
      catom = WideHashChain[i].Entry;
      READ_UNLOCK(WideHashChain[i].AERWLock);
      if (catom != NIL) {
	break;
      }
      i++;
    }
    if (i == WideAtomHashTableSize) {
      cut_fail();
    }
  }
  ap = RepAtom(catom);
  if (Yap_unify_constant(ARG1, MkAtomTerm(catom))) {
    READ_LOCK(ap->ARWLock);
    if (ap->NextOfAE == NIL) {
      READ_UNLOCK(ap->ARWLock);
      i++;
      while (i < WideAtomHashTableSize) {
	READ_LOCK(WideHashChain[i].AERWLock);
	catom = WideHashChain[i].Entry;
	READ_UNLOCK(WideHashChain[i].AERWLock);
	if (catom != NIL) {
	  break;
	}
	i++;
      }
      if (i == WideAtomHashTableSize) {
	cut_fail();
      } else {
	EXTRA_CBACK_ARG(1,1) = MkAtomTerm(catom);
      }
    } else {
      EXTRA_CBACK_ARG(1,1) = MkAtomTerm(ap->NextOfAE);
      READ_UNLOCK(ap->ARWLock);
    }
    EXTRA_CBACK_ARG(1,2) = MkIntTerm(i);
    return TRUE;
  } else {
    return FALSE;
  }
}

static Int 
init_current_wide_atom( USES_REGS1 )
{				/* current_atom(?Atom)		 */
  Term t1 = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    if (IsAtomTerm(t1))
      cut_succeed();
    else
      cut_fail();
  }
  READ_LOCK(WideHashChain[0].AERWLock);
  if (WideHashChain[0].Entry != NIL) {
    EXTRA_CBACK_ARG(1,1) = MkAtomTerm(WideHashChain[0].Entry);
  } else {
    EXTRA_CBACK_ARG(1,1) = MkIntTerm(0);
  }
  READ_UNLOCK(WideHashChain[0].AERWLock);
  EXTRA_CBACK_ARG(1,2) = MkIntTerm(0);
  return (cont_current_wide_atom( PASS_REGS1 ));
}

void
Yap_InitBackAtoms(void)
{
  Yap_InitCPredBack("$current_atom", 1, 2, init_current_atom, cont_current_atom,
		SafePredFlag|SyncPredFlag);
  Yap_InitCPredBack("$current_wide_atom", 1, 2, init_current_wide_atom,
		    cont_current_wide_atom,
		    SafePredFlag|SyncPredFlag);
  Yap_InitCPredBack("sub_atom", 5, 5, init_sub_atom, cont_sub_atom, 0);

}

void
Yap_InitAtomPreds(void)
{
  Yap_InitCPred("name", 2, p_name, 0);
  Yap_InitCPred("string_to_atom", 2, p_string_to_atom, 0);
  Yap_InitCPred("string_to_list", 2, p_string_to_list, 0);
  Yap_InitCPred("char_code", 2, p_char_code, SafePredFlag);
  Yap_InitCPred("atom_chars", 2, p_atom_chars, 0);
  Yap_InitCPred("atom_codes", 2, p_atom_codes, 0);
  Yap_InitCPred("atom_length", 2, p_atom_length, SafePredFlag);
  Yap_InitCPred("string_length", 2, p_atom_length, SafePredFlag);
  Yap_InitCPred("$atom_split", 4, p_atom_split, SafePredFlag);
  Yap_InitCPred("number_chars", 2, p_number_chars, 0);
  Yap_InitCPred("number_atom", 2, p_number_atom, 0);
  Yap_InitCPred("number_codes", 2, p_number_codes, 0);
  Yap_InitCPred("atom_number", 2, p_atom_number, 0);
  Yap_InitCPred("atom_concat", 2, p_atom_concat, 0);
  Yap_InitCPred("atomic_concat", 2, p_atomic_concat, 0);
}
