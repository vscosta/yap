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
 * This file includes the definition of a miscellania of standard predicates
 * for yap refering to atom manipulation.
 *
 */

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "eval.h"
#include "yapio.h"
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

#ifndef NAN
#define NAN      (0.0/0.0)
#endif

static Term get_num(char * USES_REGS);
static Int p_name( USES_REGS1 );
static Int p_atom_chars( USES_REGS1 );
static Int p_atom_codes( USES_REGS1 );
static Int p_atom_length( USES_REGS1 );
static Int p_atom_split( USES_REGS1 );
static Int p_number_chars( USES_REGS1 );
static Int p_number_codes( USES_REGS1 );
static Int init_current_atom( USES_REGS1 );
static Int cont_current_atom( USES_REGS1 );

static Term 
get_num(char *t USES_REGS)
{
  Term out;
  IOSTREAM *smem = Sopenmem(&t, NULL, "r");
  out = Yap_scan_num(smem);
  Sclose(smem);
  /* not ever iso */
  if (out == TermNil && yap_flags[LANGUAGE_MODE_FLAG] != 1) {
    int sign = 1;
    if (t[0] == '+') {
      t++;
    }
    if (t[0] == '-') {
      t++;
      sign = -1;
    }
    if(strcmp(t,"inf") == 0) {
      if (sign > 0) {
	return MkFloatTerm(INFINITY);
      } else {
	return MkFloatTerm(-INFINITY);
      }
    }
    if(strcmp(t,"nan") == 0) {
      if (sign > 0) {
	return MkFloatTerm(NAN);
      } else {
	return MkFloatTerm(-NAN);
      }
    }
  }
  /*
  if (cur_char_ptr[0] == '\0')
  else
    return(TermNil);
  */
  return(out);
}

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

static wchar_t *
ch_to_wide(char *base, char *charp USES_REGS)
{
  int n = charp-base, i;
  wchar_t *nb = (wchar_t *)base;

  if ((nb+n) + 1024 > (wchar_t *)AuxSp) {
    LOCAL_Error_TYPE = OUT_OF_AUXSPACE_ERROR;	  
    LOCAL_ErrorMessage = "Heap Overflow While Scanning: please increase code space (-h)";
    return NULL;
  }
  for (i=n; i > 0; i--) {
    nb[i-1] = (unsigned char)base[i-1];
  }
  return nb+n;
}

static Int 
p_name( USES_REGS1 )
{				/* name(?Atomic,?String)		 */
  char            *String, *s; /* alloc temp space on trail */
  Term            t = Deref(ARG2), NewT, AtomNameT = Deref(ARG1);
  wchar_t *ws = NULL;

 restart_aux:
  if (!IsVarTerm(AtomNameT)) {
    if (!IsVarTerm(t) && !IsPairTerm(t) && t != TermNil) {
      Yap_Error(TYPE_ERROR_LIST,ARG2,
		"name/2");
      return FALSE;
    }
    if (IsAtomTerm(AtomNameT)) {
      Atom at = AtomOfTerm(AtomNameT);
      if (IsWideAtom(at)) {
	NewT = Yap_WideStringToList((wchar_t *)(RepAtom(at)->StrOfAE));
	if (NewT == 0L)
	  goto expand_global;
	return Yap_unify(NewT, ARG2);
      } else
	String = RepAtom(at)->StrOfAE;
    } else if (IsIntTerm(AtomNameT)) {
      String = Yap_PreAllocCodeSpace();
      if (String + 1024 > (char *)AuxSp) 
	goto expand_auxsp;
      sprintf(String, Int_FORMAT, IntOfTerm(AtomNameT));
    } else if (IsFloatTerm(AtomNameT)) {
      String = Yap_PreAllocCodeSpace();
      if (String + 1024 > (char *)AuxSp) 
	goto expand_auxsp;

      sprintf(String, "%f", FloatOfTerm(AtomNameT));
    } else if (IsLongIntTerm(AtomNameT)) {
      String = Yap_PreAllocCodeSpace();
      if (String + 1024 > (char *)AuxSp) 
	goto expand_auxsp;

      sprintf(String, Int_FORMAT, LongIntOfTerm(AtomNameT));
#if USE_GMP
    } else if (IsBigIntTerm(AtomNameT)) {
      String = Yap_PreAllocCodeSpace();
      if (!Yap_gmp_to_string(AtomNameT, String, ((char *)AuxSp-String)-1024, 10 ))
	goto expand_auxsp;
#endif
    } else {
      Yap_Error(TYPE_ERROR_ATOMIC,AtomNameT,"name/2");
      return FALSE;
    }
    NewT = Yap_StringToList(String);
    return Yap_unify(NewT, ARG2);
  }
  s = String = ((AtomEntry *)Yap_PreAllocCodeSpace())->StrOfAE;
  if (String == ((AtomEntry *)NULL)->StrOfAE ||
      String + 1024 > (char *)AuxSp) 
    goto expand_auxsp;
  if (!IsVarTerm(t) && t == MkAtomTerm(AtomNil)) {
    return Yap_unify_constant(ARG1, MkAtomTerm(AtomEmptyAtom));
  }
  while (!IsVarTerm(t) && IsPairTerm(t)) {
    Term            Head;
    Int             i;

    Head = HeadOfTerm(t);
    if (IsVarTerm(Head)) {
      Yap_Error(INSTANTIATION_ERROR,Head,"name/2");
      return FALSE;
    }
    if (!IsIntegerTerm(Head)) {
      Yap_Error(TYPE_ERROR_INTEGER,Head,"name/2");
      return FALSE;
    }
    i = IntegerOfTerm(Head);
    if (i < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,Head,"name/2");
      return FALSE;
    }
    if (ws) {
      if (ws > (wchar_t *)AuxSp-1024) {
	goto expand_auxsp;
      }
      *ws++ = i;      
    } else {
      if (i > MAX_ISO_LATIN1) {
	ws = ch_to_wide(String, s PASS_REGS);
	*ws++ = i;
      } else {
	if (s > (char *)AuxSp-1024) {
	  goto expand_auxsp;
	}
	*s++ = i;
      }
    }
    t = TailOfTerm(t);
  }
  if (ws) {
    Atom at;

    *ws = '\0';
    while ((at = Yap_LookupWideAtom((wchar_t *)String)) == NIL) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, ARG2, "generating atom from string in name/2");
	return FALSE;
      }
      /* safest to restart, we don't know what happened to String */
      t = Deref(ARG2);
      AtomNameT = Deref(ARG1);
      goto restart_aux;
    }
    NewT = MkAtomTerm(at);
    return Yap_unify_constant(ARG1, NewT);
  }
  *s = '\0';
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"name/2");
    return(FALSE);
  }
  if (IsAtomTerm(t) && AtomOfTerm(t) == AtomNil) {
    if ((NewT = get_num(String PASS_REGS)) == TermNil) {
      Atom at;
      while ((at = Yap_LookupAtom(String)) == NIL) {
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  Yap_Error(OUT_OF_HEAP_ERROR, ARG2, "generating atom from string in name/2");
	  return FALSE;
	}
	/* safest to restart, we don't know what happened to String */
	t = Deref(ARG2);
	AtomNameT = Deref(ARG1);
	goto restart_aux;
      }
      NewT = MkAtomTerm(at);
    }
    return Yap_unify_constant(ARG1, NewT);
  } else {
    Yap_Error(TYPE_ERROR_LIST,ARG2,"name/2");
    return FALSE;
  }

  /* error handling */
 expand_global:
  if (!Yap_gc(2, ENV, gc_P(P,CP))) {
    Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
    return(FALSE);
  }    
  AtomNameT = Deref(ARG1);
  t = Deref(ARG2);
  goto restart_aux;

 expand_auxsp:
  String = Yap_ExpandPreAllocCodeSpace(0,NULL, TRUE);
  if (String + 1024 > (char *)AuxSp) {
    /* crash in flames */
    Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "allocating temp space in name/2");
    return FALSE;
  }
  AtomNameT = Deref(ARG1);
  t = Deref(ARG2);
  goto restart_aux;

}

static Int 
p_string_to_atom( USES_REGS1 )
{				/* name(?Atomic,?String)		 */
  char            *String; /* alloc temp space on trail */
  Term            t = Deref(ARG1), NewT, AtomNameT = Deref(ARG2);

 restart_aux:
  if (!IsVarTerm(t)) {
    Atom at;
    do {
      if (Yap_IsWideStringTerm(t)) {
	at = Yap_LookupWideAtom(Yap_BlobWideStringOfTerm(t));
      } else if (Yap_IsStringTerm(t)) {
	at = Yap_LookupAtom(Yap_BlobStringOfTerm(t));
      } else if (IsAtomTerm(t)) {
	return Yap_unify(t, ARG2);
      } else if (IsIntTerm(t)) {
	char *String = Yap_PreAllocCodeSpace();
	if (String + 1024 > (char *)AuxSp) 
	  goto expand_auxsp;
	sprintf(String, Int_FORMAT, IntOfTerm(t));
	at = Yap_LookupAtom(String);
      } else if (IsFloatTerm(t)) {
	char *String = Yap_PreAllocCodeSpace();
	if (String + 1024 > (char *)AuxSp) 
	  goto expand_auxsp;

	sprintf(String, "%f", FloatOfTerm(t));
	at = Yap_LookupAtom(String);
    } else if (IsLongIntTerm(t)) {
	char *String = Yap_PreAllocCodeSpace();
	if (String + 1024 > (char *)AuxSp) 
	  goto expand_auxsp;

	sprintf(String, Int_FORMAT, LongIntOfTerm(t));
	at = Yap_LookupAtom(String);
#if USE_GMP
      } else if (IsBigIntTerm(t)) {
	String = Yap_PreAllocCodeSpace();
	if (!Yap_gmp_to_string(t, String, ((char *)AuxSp-String)-1024, 10 ))
	  goto expand_auxsp;
	at = Yap_LookupAtom(String);
#endif
      } else {
	Yap_Error(TYPE_ERROR_ATOMIC,AtomNameT,"name/2");
	return FALSE;
      }
      if (at != NIL)
	break;
      if (!Yap_growheap(FALSE, 0, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, ARG2, "generating atom from string in string_to_atom/2");
	return FALSE;
      }
      t = Deref(ARG1);
    } while(TRUE);
    return Yap_unify_constant(ARG2, MkAtomTerm(at));
  }
  if (IsVarTerm(AtomNameT)) {
    Yap_Error(INSTANTIATION_ERROR, ARG1, "string_to_atom/2");
    return(FALSE);		
  }
  else if (IsAtomTerm(AtomNameT)) {
    Atom at = AtomOfTerm(AtomNameT);
    if (IsWideAtom(at)) {
      wchar_t *s = RepAtom(at)->WStrOfAE;
      NewT = Yap_MkBlobWideStringTerm(s, wcslen(s));
      return Yap_unify(NewT, ARG1);
    } else
      String = RepAtom(at)->StrOfAE;
  } else if (IsIntTerm(AtomNameT)) {
    String = Yap_PreAllocCodeSpace();
    if (String + 1024 > (char *)AuxSp) 
      goto expand_auxsp;
    sprintf(String, Int_FORMAT, IntOfTerm(AtomNameT));
  } else if (IsFloatTerm(AtomNameT)) {
    String = Yap_PreAllocCodeSpace();
    if (String + 1024 > (char *)AuxSp) 
      goto expand_auxsp;
    
    sprintf(String, "%f", FloatOfTerm(AtomNameT));
  } else if (IsLongIntTerm(AtomNameT)) {
    String = Yap_PreAllocCodeSpace();
    if (String + 1024 > (char *)AuxSp) 
      goto expand_auxsp;

    sprintf(String, Int_FORMAT, LongIntOfTerm(AtomNameT));
#if USE_GMP
  } else if (IsBigIntTerm(AtomNameT)) {
    String = Yap_PreAllocCodeSpace();
    if (!Yap_gmp_to_string(AtomNameT, String, ((char *)AuxSp-String)-1024, 10 ))
      goto expand_auxsp;
#endif
  } else {
    Yap_Error(TYPE_ERROR_ATOMIC,AtomNameT,"name/2");
    return FALSE;    
  } 
  NewT = Yap_MkBlobStringTerm(String, strlen(String));
  return Yap_unify(NewT, ARG1);

  /* error handling */
 expand_auxsp:
  String = Yap_ExpandPreAllocCodeSpace(0,NULL, TRUE);
  if (String + 1024 > (char *)AuxSp) {
    /* crash in flames */
    Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "allocating temp space in string_to_atom/2");
    return FALSE;
  }
  AtomNameT = Deref(ARG1);
  t = Deref(ARG2);
  goto restart_aux;

}

static Int 
p_string_to_list( USES_REGS1 )
{				/* name(?Atomic,?String)		 */
  char            *String; /* alloc temp space on trail */
  Term            t = Deref(ARG1), NewT, NameT = Deref(ARG2);

 restart_aux:
  if (!IsVarTerm(t)) {
    Term StringT;

    if (Yap_IsWideStringTerm(t)) {
      StringT = Yap_WideStringToList(Yap_BlobWideStringOfTerm(t));
      if (StringT == 0L)
	  goto expand_global;
    } else if (Yap_IsStringTerm(t)) {
      StringT = Yap_StringToList(Yap_BlobStringOfTerm(t));
    } else if (IsAtomTerm(t)) {
      Atom at = AtomOfTerm(t);
      if (IsWideAtom(at))
	StringT = Yap_WideStringToList(RepAtom(at)->WStrOfAE);
      else
	StringT = Yap_StringToList(RepAtom(at)->StrOfAE);
      if (StringT == 0L)
	goto expand_global;
    } else if (IsIntTerm(t)) {
      char *String = Yap_PreAllocCodeSpace();
      if (String + 1024 > (char *)AuxSp) 
	goto expand_auxsp;
      sprintf(String, Int_FORMAT, IntOfTerm(t));
      StringT = Yap_StringToList(String);
    } else if (IsFloatTerm(t)) {
      char *String = Yap_PreAllocCodeSpace();
      if (String + 1024 > (char *)AuxSp) 
	goto expand_auxsp;

      sprintf(String, "%f", FloatOfTerm(t));
      StringT = Yap_StringToList(String);
    } else if (IsLongIntTerm(t)) {
      char *String = Yap_PreAllocCodeSpace();
      if (String + 1024 > (char *)AuxSp) 
	goto expand_auxsp;

      sprintf(String, Int_FORMAT, LongIntOfTerm(t));
      StringT = Yap_StringToList(String);
#if USE_GMP
    } else if (IsBigIntTerm(t)) {
      String = Yap_PreAllocCodeSpace();
      if (!Yap_gmp_to_string(t, String, ((char *)AuxSp-String)-1024, 10 ))
	goto expand_auxsp;
      StringT = Yap_StringToList(String);
#endif
    } else {
      Yap_Error(TYPE_ERROR_ATOMIC,NameT,"string_to_list/2");
      return FALSE;
    }
    return Yap_unify_constant(ARG2, StringT);
  }
  if (!IsVarTerm(NameT)) {
    if (IsAtomTerm(NameT)) {
      Atom at = AtomOfTerm(NameT);
      if (IsWideAtom(at)) {
	wchar_t *s = RepAtom(at)->WStrOfAE;
	NewT = Yap_MkBlobWideStringTerm(s, wcslen(s));
	return Yap_unify(NewT, ARG1);
      } else
	String = RepAtom(at)->StrOfAE;
    } else if (IsIntTerm(NameT)) {
      String = Yap_PreAllocCodeSpace();
      if (String + 1024 > (char *)AuxSp) 
	goto expand_auxsp;
      sprintf(String, Int_FORMAT, IntOfTerm(NameT));
    } else if (IsFloatTerm(NameT)) {
      String = Yap_PreAllocCodeSpace();
      if (String + 1024 > (char *)AuxSp) 
	goto expand_auxsp;

      sprintf(String, "%f", FloatOfTerm(NameT));
    } else if (IsLongIntTerm(NameT)) {
      String = Yap_PreAllocCodeSpace();
      if (String + 1024 > (char *)AuxSp) 
	goto expand_auxsp;

      sprintf(String, Int_FORMAT, LongIntOfTerm(NameT));
#if USE_GMP
    } else if (IsBigIntTerm(NameT)) {
      String = Yap_PreAllocCodeSpace();
      if (!Yap_gmp_to_string(NameT, String, ((char *)AuxSp-String)-1024, 10 ))
	goto expand_auxsp;
#endif
    } else {
      wchar_t *WString = (wchar_t *)Yap_PreAllocCodeSpace();
      wchar_t *ws = WString;
      while (IsPairTerm(NameT)) {
	Term Head = HeadOfTerm(NameT);
	Int i;

	if (IsVarTerm(Head)) {
	  Yap_Error(INSTANTIATION_ERROR,Head,"string_codes/2");
	  return FALSE;
	}
	if (!IsIntegerTerm(Head)) {
	  Yap_Error(TYPE_ERROR_INTEGER,Head,"string_codes/2");
	  return FALSE;
	}
	i = IntegerOfTerm(Head);
	if (i < 0) {
	  Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,Head,"string_codes/2");
	  return FALSE;
	}
	if (ws > (wchar_t *)AuxSp-1024) {
	  goto expand_auxsp;
	}
	*ws++ = i;      
	NameT = TailOfTerm(NameT);
      }
      if (IsVarTerm(NameT)) {
	  Yap_Error(INSTANTIATION_ERROR,ARG2,"string_codes/2");
	  return FALSE;
      }
      if (NameT != TermNil) {
	  Yap_Error(TYPE_ERROR_LIST,ARG2,"string_codes/2");
	  return FALSE;
      }
      *ws++ = '\0';
      NewT = Yap_MkBlobWideStringTerm(WString, wcslen(WString));
      return Yap_unify(NewT, ARG1);
      /* **** */
    }
    NewT = Yap_MkBlobStringTerm(String, sizeof(String));
    return Yap_unify(NewT, ARG1);
  }
  Yap_Error(INSTANTIATION_ERROR, ARG1, "string_to_list/2");
  return(FALSE);		

  /* error handling */
expand_global:
  if (!Yap_gc(2, ENV, gc_P(P,CP))) {
    Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
    return(FALSE);
  }    
  NameT = Deref(ARG1);
  t = Deref(ARG2);
  goto restart_aux;

 expand_auxsp:
  String = Yap_ExpandPreAllocCodeSpace(0,NULL, TRUE);
  if (String + 1024 > (char *)AuxSp) {
    /* crash in flames */
    Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "allocating temp space in string_to_list/2");
    return FALSE;
  }
  NameT = Deref(ARG1);
  t = Deref(ARG2);
  goto restart_aux;

}

static Int 
p_atom_chars( USES_REGS1 )
{
  Term t1 = Deref(ARG1);
  char           *String;

 restart_aux:
  if (!IsVarTerm(t1)) {
    Term            NewT;
    Atom at;

    if (!IsAtomTerm(t1)) {
      Yap_Error(TYPE_ERROR_ATOM, t1, "atom_chars/2");
      return(FALSE);
    }
    at = AtomOfTerm(t1);
    if (IsWideAtom(at)) {
      if (yap_flags[YAP_TO_CHARS_FLAG] == QUINTUS_TO_CHARS) {
	NewT = Yap_WideStringToList((wchar_t *)RepAtom(at)->StrOfAE);
      } else {
	NewT = Yap_WideStringToListOfAtoms((wchar_t *)RepAtom(AtomOfTerm(t1))->StrOfAE);
      }
      if (NewT == 0L)
	goto expand_global;
    } else {
      if (yap_flags[YAP_TO_CHARS_FLAG] == QUINTUS_TO_CHARS) {
	NewT = Yap_StringToList(RepAtom(at)->StrOfAE);
      } else {
	NewT = Yap_StringToListOfAtoms(RepAtom(AtomOfTerm(t1))->StrOfAE);
      }
    }
    return Yap_unify(NewT, ARG2);
  } else {
    /* ARG1 unbound */
    Term   t = Deref(ARG2);
    char  *s;
    wchar_t *ws = NULL;
    Atom at;

    String = ((AtomEntry *)Yap_PreAllocCodeSpace())->StrOfAE;
    if (String + 1024 > (char *)AuxSp) 
      goto expand_auxsp;
    s = String;
    if (IsVarTerm(t)) {
      Yap_Error(INSTANTIATION_ERROR, t1, "atom_chars/2");
      return(FALSE);		
    }
    if (t == TermNil) {
      return (Yap_unify_constant(t1, MkAtomTerm(AtomEmptyAtom)));
    }
    if (!IsPairTerm(t)) {
      Yap_Error(TYPE_ERROR_LIST, t, "atom_chars/2");
      return(FALSE);		
    }
    if (yap_flags[YAP_TO_CHARS_FLAG] == QUINTUS_TO_CHARS) {
      while (t != TermNil) {
	register Term   Head;
	register Int    i;
	Head = HeadOfTerm(t);
	if (IsVarTerm(Head)) {
	  Yap_Error(INSTANTIATION_ERROR,Head,"atom_chars/2");
	  return(FALSE);
	} else if (!IsIntegerTerm(Head)) {
	  Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"atom_chars/2");
	  return(FALSE);		
	}
	i = IntegerOfTerm(Head);
	if (i < 0) {
	  Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"atom_chars/2");
	  return(FALSE);		
	}
	if (i > MAX_ISO_LATIN1 && !ws) {
	  ws = ch_to_wide(String, s PASS_REGS);
	}
	if (ws) {
	  if (ws > (wchar_t *)AuxSp-1024) {
	    goto expand_auxsp;
	  }
	  *ws++ = i;      
	} else {
	  if (s+1024 > (char *)AuxSp) {
	    goto expand_auxsp;
	  }
	  *s++ = i;
	}
	t = TailOfTerm(t);
	if (IsVarTerm(t)) {
	  Yap_Error(INSTANTIATION_ERROR,t,"atom_chars/2");
	  return(FALSE);
	} else if (!IsPairTerm(t) && t != TermNil) {
	  Yap_Error(TYPE_ERROR_LIST, t, "atom_chars/2");
	  return(FALSE);
	}
      }
    } else {
      /* ISO Prolog Mode */
      int has_atoms = yap_flags[STRICT_ISO_FLAG];
      int has_ints =  FALSE;

      while (t != TermNil) {
	Term   Head;
	char   *is;

	Head = HeadOfTerm(t);
	if (IsVarTerm(Head)) {
	  Yap_Error(INSTANTIATION_ERROR,Head,"atom_chars/2");
	  return(FALSE);
	} else if (IsAtomTerm(Head) && !has_ints) {
	  at = AtomOfTerm(Head);
	  if (IsWideAtom(at)) {
	    wchar_t *wis = (wchar_t *)RepAtom(at)->StrOfAE;
	    if (wis[1] != '\0') {
	      Yap_Error(TYPE_ERROR_CHARACTER,Head,"atom_chars/2");
	      return(FALSE);		
	    }
	    if (!ws) {
	      ws = ch_to_wide(String, s PASS_REGS);	    
	    }
	    if (ws+1024 == (wchar_t *)AuxSp) {
	      goto expand_auxsp;
	    }
	    *ws++ = wis[0];
	  } else {
	    is = RepAtom(at)->StrOfAE;
	    if (is[1] != '\0') {
	      Yap_Error(TYPE_ERROR_CHARACTER,Head,"atom_chars/2");
	      return(FALSE);		
	    }
	    if (ws) {
	      if (ws+1024 == (wchar_t *)AuxSp) {
		goto expand_auxsp;
	      }
	      *ws++ = is[0];
	    } else {
	      if (s+1024 == (char *)AuxSp) {
		goto expand_auxsp;
	      }
	      *s++ = is[0];
	    }
	  }
	} else if (IsIntegerTerm(Head) && !has_atoms) {
	  Int i = IntegerOfTerm(Head);
	  if (i < 0) {
	    Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"atom_codes/2");
	    return(FALSE);		
	  }
	  if (i > MAX_ISO_LATIN1 && !ws) {
	    ws = ch_to_wide(String, s PASS_REGS);
	  }
	  if (ws) {
	    if (ws+1024 > (wchar_t *)AuxSp) {
	      goto expand_auxsp;
	    }
	    *ws++ = i;
	  } else {
	    if (s+1024 > (char *)AuxSp) {
	      goto expand_auxsp;
	    }
	    *s++ = i;
	  }
	} else {
	  Yap_Error(TYPE_ERROR_CHARACTER,Head,"atom_chars/2");
	  return(FALSE);		
	}
	t = TailOfTerm(t);
	if (IsVarTerm(t)) {
	  Yap_Error(INSTANTIATION_ERROR,t,"atom_chars/2");
	  return(FALSE);
	} else if (!IsPairTerm(t) && t != TermNil) {
	  Yap_Error(TYPE_ERROR_LIST, t, "atom_chars/2");
	  return(FALSE);
	}
      }
    }
    if (ws) {
      *ws++ = '\0';
      while ((at = Yap_LookupWideAtom((wchar_t *)String)) == NIL) {
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}
      }
    } else {
      *s++ = '\0';
      while ((at = Yap_LookupAtom(String)) == NIL) {
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}
      }
    }
    return Yap_unify_constant(ARG1, MkAtomTerm(at));
  }
  /* error handling */
 expand_global:
  if (!Yap_gc(2, ENV, gc_P(P,CP))) {
    Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
    return(FALSE);
  }    
  t1 = Deref(ARG1);
  goto restart_aux;

 expand_auxsp:
  String = Yap_ExpandPreAllocCodeSpace(0,NULL, TRUE);
  if (String + 1024 > (char *)AuxSp) {
    /* crash in flames */
    Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "allocating temp space in atom_chars/2");
    return FALSE;
  }
  t1 = Deref(ARG1);
  goto restart_aux;
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
p_atom_codes( USES_REGS1 )
{
  Term t1 = Deref(ARG1);
  char *String;

 restart_pred:
  if (!IsVarTerm(t1)) {
    Term            NewT;
    Atom at;

    if (Yap_IsStringTerm(t1)) {
      if (Yap_IsWideStringTerm(t1)) {
	NewT = Yap_WideStringToList(Yap_BlobWideStringOfTerm(t1));
      } else {
	NewT = Yap_StringToList(Yap_BlobStringOfTerm(t1));
      }
      if (NewT == 0L)
	goto expand_global;
    } else if (!IsAtomTerm(t1)) {
      Yap_Error(TYPE_ERROR_ATOM, t1, "atom_codes/2");
      return(FALSE);
    } else {
      at = AtomOfTerm(t1);
      if (IsWideAtom(at)) {
	NewT = Yap_WideStringToList((wchar_t *)RepAtom(at)->StrOfAE);
      } else {
	NewT = Yap_StringToList(RepAtom(at)->StrOfAE);
      }
      if (NewT == 0L)
	goto expand_global;
    }
    return (Yap_unify(NewT, ARG2));
  } else {
    /* ARG1 unbound */
    Term   t = Deref(ARG2);
    char  *s;
    wchar_t *ws = NULL;

    String = ((AtomEntry *)Yap_PreAllocCodeSpace())->StrOfAE;
    if (String + 1024 > (char *)AuxSp) { 
      goto expand_auxsp;
    }
    s = String;
    if (IsVarTerm(t)) {
      Yap_Error(INSTANTIATION_ERROR, t1, "atom_codes/2");
      return(FALSE);		
    }
    if (t == TermNil) {
      return (Yap_unify_constant(t1, MkAtomTerm(AtomEmptyAtom)));
    }
    if (!IsPairTerm(t)) {
      Yap_Error(TYPE_ERROR_LIST, t, "atom_codes/2");
      return(FALSE);		
    }
    while (t != TermNil) {
      register Term   Head;
      register Int    i;
      Head = HeadOfTerm(t);
      if (IsVarTerm(Head)) {
	Yap_Error(INSTANTIATION_ERROR,Head,"atom_codes/2");
	return(FALSE);
      } else if (!IsIntegerTerm(Head)) {
	Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"atom_codes/2");
	return(FALSE);		
      }
      i = IntegerOfTerm(Head);
      if (i < 0) {
	Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"atom_codes/2");
	return(FALSE);		
      }
      if (i > MAX_ISO_LATIN1 && !ws) {
	ws = ch_to_wide(String, s PASS_REGS);
      }
      if (ws) {
	if (ws+1024 > (wchar_t *)AuxSp) {
	  goto expand_auxsp;
	}
	*ws++ = i;
      } else {
	if (s+1024 > (char *)AuxSp) {
	  goto expand_auxsp;
	}
	*s++ = i;
      }
      t = TailOfTerm(t);
      if (IsVarTerm(t)) {
	Yap_Error(INSTANTIATION_ERROR,t,"atom_codes/2");
	return(FALSE);
      } else if (!IsPairTerm(t) && t != TermNil) {
	Yap_Error(TYPE_ERROR_LIST, t, "atom_codes/2");
	return(FALSE);
      }
    }
    if (ws) {
      *ws++ = '\0';
      return Yap_unify_constant(ARG1, MkAtomTerm(Yap_LookupWideAtom((wchar_t *)String)));
    } else {
      *s++ = '\0';
      return Yap_unify_constant(ARG1, MkAtomTerm(Yap_LookupAtom(String)));
    }
  }
  /* error handling */
 expand_global:
  if (!Yap_gc(2, ENV, gc_P(P,CP))) {
    Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
    return(FALSE);
  }    
  t1 = Deref(ARG1);
  goto restart_pred;

  expand_auxsp:
  if (String + 1024 > (char *)AuxSp) { 
    String = Yap_ExpandPreAllocCodeSpace(0,NULL, TRUE);
  
    if (String + 1024 > (char *)AuxSp) {
      /* crash in flames */
      Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "allocating temp space in atom_codes/2");
      return FALSE;
    }
    t1 = Deref(ARG1);
  }
  goto restart_pred;
}

static Int 
p_atom_length( USES_REGS1 )
{
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  Atom at;
  Int len = 0;
 
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
  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "atom_length/2");
    return(FALSE);		
  }
  if (Yap_IsStringTerm(t1)) {

    if (Yap_IsWideStringTerm(t1)) {
      len = wcslen(Yap_BlobWideStringOfTerm(t1));
    } else {
      len = strlen(Yap_BlobStringOfTerm(t1));
    }
    return Yap_unify(ARG2, MkIntegerTerm(len));
  } else if (!IsAtomTerm(t1)) {
    if (!yap_flags[STRICT_ISO_FLAG]) {
      char *String; 

      if (IsIntegerTerm(t1)) {
	String = Yap_PreAllocCodeSpace();
	if (String + 1024 > (char *)AuxSp) 
	  goto expand_auxsp;
	sprintf(String, Int_FORMAT, IntegerOfTerm(t1));
      } else if (IsFloatTerm(t1)) {
	String = Yap_PreAllocCodeSpace();
	if (String + 1024 > (char *)AuxSp) 
	  goto expand_auxsp;
	sprintf(String, "%f", FloatOfTerm(t1));
#if USE_GMP
      } else if (IsBigIntTerm(t1)) {
	String = Yap_PreAllocCodeSpace();
	if (!Yap_gmp_to_string(t1, String, ((char *)AuxSp-String)-1024, 10 ))
	  goto expand_auxsp;
#endif
      } else {
	Yap_Error(TYPE_ERROR_ATOM, t1, "atom_length/2");
	return FALSE;
      }
      return Yap_unify(ARG2, MkIntegerTerm(strlen(String)));
    } else {
      Yap_Error(TYPE_ERROR_ATOM, t1, "atom_length/2");
      return FALSE;
    }
  }
  at = AtomOfTerm(t1);
  if (!IsVarTerm(t2)) {
    /* len is given from above */
    if (IsWideAtom(at)) {
      return wcslen((wchar_t *)RepAtom(at)->StrOfAE) == len;
    } else {
      return(strlen(RepAtom(at)->StrOfAE) == len);
    }
  } else {
    Term tj;
    size_t len;

    if (IsWideAtom(at)) {
      len = wcslen((wchar_t *)RepAtom(at)->StrOfAE);
    } else {
      len = strlen(RepAtom(at)->StrOfAE);
    }
    tj = MkIntegerTerm(len);
    return Yap_unify_constant(t2,tj);
  }
 expand_auxsp:
  {
    char *String = Yap_ExpandPreAllocCodeSpace(0,NULL, TRUE);
    if (String + 1024 > (char *)AuxSp) {
      /* crash in flames */
      Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "allocating temp space in name/2");
    return FALSE;
    }
    t1 = Deref(ARG1);
    t2 = Deref(ARG2);
    goto restart_aux;
  }
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
  if ((len = IntOfTerm(t2)) < 0) {
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

static Term
gen_syntax_error(Atom InpAtom, char *s)
{
  CACHE_REGS
  Term ts[7], ti[2];
  ti[0] = ARG1;
  ti[1] = ARG2;
  ts[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom(s),2),2,ti);
  ts[1] = ts[4] = ts[5] = MkIntTerm(0);
  ts[2] = MkAtomTerm(AtomExpectedNumber);
  ts[3] = TermNil;
  ts[6] = MkAtomTerm(InpAtom);
  return(Yap_MkApplTerm(FunctorSyntaxError,7,ts));
}

static Int 
p_number_chars( USES_REGS1 )
{
  char   *String; /* alloc temp space on Trail */
  register Term   t = Deref(ARG2), t1 = Deref(ARG1);
  Term NewT;
  register char  *s;

 restart_aux:
  String = Yap_PreAllocCodeSpace();
  if (String+1024 > (char *)AuxSp) {
    String = Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE);
    if (String + 1024 > (char *)AuxSp) {
      /* crash in flames */
      Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "allocating temp space in number_chars/2");
      return FALSE;
    }
  }
  if (IsNonVarTerm(t1) && !Yap_IsGroundTerm(t)) {
    Term            NewT;
    if (!IsNumTerm(t1)) {
      Yap_Error(TYPE_ERROR_NUMBER, t1, "number_chars/2");
      return(FALSE);
    } else if (IsIntTerm(t1)) {
      sprintf(String, Int_FORMAT, IntOfTerm(t1));
    } else if (IsFloatTerm(t1)) {
      sprintf(String, "%f", FloatOfTerm(t1));
    } else if (IsLongIntTerm(t1)) {
      sprintf(String, Int_FORMAT, LongIntOfTerm(t1));
#if USE_GMP
    } else if (IsBigIntTerm(t1)) {
      if (!Yap_gmp_to_string(t1, String, ((char *)AuxSp-String)-1024, 10 )) {
	size_t sz = Yap_gmp_to_size(t1, 10);
	Yap_ReleasePreAllocCodeSpace((ADDR)String);
	if (!Yap_ExpandPreAllocCodeSpace(sz, NULL, TRUE)) {
	  Yap_Error(OUT_OF_AUXSPACE_ERROR, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}
	goto restart_aux;
      }
#endif
    }
    if (yap_flags[YAP_TO_CHARS_FLAG] == QUINTUS_TO_CHARS) {
      NewT = Yap_StringToList(String);
    } else {
      NewT = Yap_StringToListOfAtoms(String);
    }
    return Yap_unify(NewT, ARG2);
  }
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "number_chars/2");
    return(FALSE);		
  }
  if (!IsPairTerm(t) && t != TermNil) {
    Yap_Error(TYPE_ERROR_LIST, ARG2, "number_chars/2");
    return(FALSE);		
  }
  s = String;
  if (yap_flags[YAP_TO_CHARS_FLAG] == QUINTUS_TO_CHARS) {
    while (t != TermNil) {
      register Term   Head;
      register Int    i;
      Head = HeadOfTerm(t);
      if (IsVarTerm(Head)) {
	Yap_Error(INSTANTIATION_ERROR,Head,"number_chars/2");
	return(FALSE);
      } else if (!IsIntegerTerm(Head)) {
	Yap_Error(TYPE_ERROR_INTEGER,Head,"number_chars/2");
	return(FALSE);		
      }
      i = IntOfTerm(Head);
      if (i < 0 || i > 255) {
	Yap_Error(TYPE_ERROR_INTEGER,Head,"number_chars/2");
	return(FALSE);		
      }
      if (s+1024 > (char *)AuxSp) {
	int offs = (s-String);
	String = Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE);
	if (String + (offs+1024) > (char *)AuxSp) {
	  /* crash in flames */
	  Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "allocating temp space in number_chars/2");
	  return FALSE;
	}
	goto restart_aux;
      }
      *s++ = i;
      t = TailOfTerm(t);
      if (IsVarTerm(t)) {
	Yap_Error(INSTANTIATION_ERROR,t,"number_chars/2");
	return(FALSE);
      } else if (!IsPairTerm(t) && t != TermNil) {
	Yap_Error(TYPE_ERROR_LIST,t,"number_chars/2");
	return(FALSE);
      }
    }
  } else {
    /* ISO code */
    int has_atoms = yap_flags[STRICT_ISO_FLAG];
    int has_ints =  FALSE;

    while (t != TermNil) {
      Term   Head;
      char   *is;
      Int    ch;
      
      Head = HeadOfTerm(t);
      if (IsVarTerm(Head)) {
	Yap_Error(INSTANTIATION_ERROR,Head,"number_chars/2");
	return(FALSE);
      } else if (IsAtomTerm(Head) && !has_ints) {
	has_atoms = TRUE;
	is = RepAtom(AtomOfTerm(Head))->StrOfAE;
	if (is[0] == '\0')
	  goto next_in_loop;
	if (is[1] != '\0') {
	  Yap_Error(TYPE_ERROR_CHARACTER,Head,"number_chars/2");
	  return FALSE;	     
	}
	ch = is[0];
      } else if (IsIntTerm(Head) && !has_atoms) {
	has_ints = TRUE;
	ch = IntOfTerm(Head);
	if (ch < 0 || ch > 255) {
	  Yap_Error(TYPE_ERROR_CHARACTER,Head,"number_chars/2");
	  return(FALSE);		
	}
      } else {
	Yap_Error(TYPE_ERROR_CHARACTER,Head,"number_chars/2");
	return(FALSE);		
      }
      if (s+1 == (char *)AuxSp) {
	char *nString;

	*H++ = t;
	nString = Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE);
	t = *--H;
	s = nString+(s-String);
	String = nString;
      }
      *s++ = ch;
    next_in_loop:
      t = TailOfTerm(t);
      if (IsVarTerm(t)) {
	Yap_Error(INSTANTIATION_ERROR,t,"number_chars/2");
	return(FALSE);
      } else if (!IsPairTerm(t) && t != TermNil) {
	Yap_Error(TYPE_ERROR_LIST,ARG2,"number_chars/2");
	return(FALSE);
      }
    }
  }
  *s++ = '\0';
  if ((NewT = get_num(String PASS_REGS)) == TermNil) {
    Yap_Error(SYNTAX_ERROR, gen_syntax_error(Yap_LookupAtom(String), "number_chars"), "while scanning %s", String);
    return (FALSE);
  }
  return (Yap_unify(ARG1, NewT));
}

static Int 
p_number_atom( USES_REGS1 )
{
  char   *String; /* alloc temp space on Trail */
  register Term   t = Deref(ARG2), t1 = Deref(ARG1);
  Term NewT;
  char  *s;

  s = String = ((AtomEntry *)Yap_PreAllocCodeSpace())->StrOfAE;
  if (String+1024 > (char *)AuxSp) {
    s = String = Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE);
    if (String + 1024 > (char *)AuxSp) {
      /* crash in flames */
      Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "allocating temp space in number_atom/2");
      return FALSE;
    }
  }
  if (IsNonVarTerm(t1)) {
    Atom at;

    if (IsIntTerm(t1)) {

      sprintf(String, Int_FORMAT, IntOfTerm(t1));
    } else if (IsFloatTerm(t1)) {
      sprintf(String, "%f", FloatOfTerm(t1));
    } else if (IsLongIntTerm(t1)) {

      sprintf(String, Int_FORMAT, LongIntOfTerm(t1));

#if USE_GMP
    } else if (IsBigIntTerm(t1)) {
      while (!Yap_gmp_to_string(t1, String, ((char *)AuxSp-String)-1024, 10 )) {
	size_t sz = Yap_gmp_to_size(t1, 10);
	if (!(String = Yap_ExpandPreAllocCodeSpace(sz, NULL, TRUE))) {
	  Yap_Error(OUT_OF_AUXSPACE_ERROR, t1, LOCAL_ErrorMessage);
	  return FALSE;
	}
      }
#endif
    } else {
      Yap_Error(TYPE_ERROR_NUMBER, t1, "number_atom/2");
      return FALSE;
    }
    while ((at = Yap_LookupAtom(String)) == NIL) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
	return FALSE;
      }
    }
    return Yap_unify(MkAtomTerm(at), ARG2);
  }
  if (IsVarTerm(t)) {
      Yap_Error(INSTANTIATION_ERROR, t, "number_chars/2");
      return(FALSE);		
  }
  if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, "number_atom/2");
    return(FALSE);		
  }
  s = RepAtom(AtomOfTerm(t))->StrOfAE;
  if ((NewT = get_num(s PASS_REGS)) == TermNil) {
    Yap_Error(SYNTAX_ERROR, gen_syntax_error(Yap_LookupAtom(String), "number_atom"), "while scanning %s", s);
    return (FALSE);
  }
  return (Yap_unify(ARG1, NewT));
}

static Int 
p_number_codes( USES_REGS1 )
{
  char   *String; /* alloc temp space on Trail */
  register Term   t = Deref(ARG2), t1 = Deref(ARG1);
  Term NewT;
  register char  *s;

  String = Yap_PreAllocCodeSpace();
  if (String+1024 > (char *)AuxSp) {
    s = String = Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE);
    if (String + 1024 > (char *)AuxSp) {
      /* crash in flames */
      Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "allocating temp space in number_codes/2");
      return FALSE;
    }
  }
  if (IsNonVarTerm(t1) && !Yap_IsGroundTerm(t)) {
    if (IsIntTerm(t1)) {
      sprintf(String, Int_FORMAT, IntOfTerm(t1));
    } else if (IsFloatTerm(t1)) {
      sprintf(String, "%f", FloatOfTerm(t1));
    } else if (IsLongIntTerm(t1)) {
      sprintf(String, Int_FORMAT, LongIntOfTerm(t1));
#if USE_GMP
    } else if (IsBigIntTerm(t1)) {
      while (!Yap_gmp_to_string(t1, String, ((char *)AuxSp-String)-1024, 10 )) {
	size_t sz = Yap_gmp_to_size(t1, 10);
	if (!(String = Yap_ExpandPreAllocCodeSpace(sz, NULL, TRUE))) {
	  Yap_Error(OUT_OF_AUXSPACE_ERROR, t1, LOCAL_ErrorMessage);
	  return FALSE;
	}
      }
#endif
    } else {
      Yap_Error(TYPE_ERROR_NUMBER, t1, "number_codes/2");
      return FALSE;
    }
    NewT = Yap_StringToList(String);
    return Yap_unify(NewT, ARG2);
  }
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "number_codes/2");
  }
  if (!IsPairTerm(t) && t != TermNil) {
    Yap_Error(TYPE_ERROR_LIST, ARG2, "number_codes/2");
    return(FALSE);		
  }
  s = String; /* alloc temp space on Trail */
  while (t != TermNil) {
    register Term   Head;
    register Int    i;

    Head = HeadOfTerm(t);
    if (IsVarTerm(Head)) {
      Yap_Error(INSTANTIATION_ERROR,Head,"number_codes/2");
      return(FALSE);
    } else if (!IsIntTerm(Head)) {
      Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"number_codes/2");
      return(FALSE);		
    }
    i = IntOfTerm(Head);
    if (i < 0 || i > 255) {
      Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"number_codes/2");
      return(FALSE);		
    }
    if (s+1 == (char *)AuxSp) {
      char *nString;

      *H++ = t;
      nString = Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE);
      t = *--H;
      s = nString+(s-String);
      String = nString;
    }
    *s++ = i;
    t = TailOfTerm(t);
    if (IsVarTerm(t)) {
      Yap_Error(INSTANTIATION_ERROR,t,"number_codes/2");
      return(FALSE);
    } else if (!IsPairTerm(t) && t != TermNil) {
      Yap_Error(TYPE_ERROR_LIST, ARG2, "number_codes/2");
      return(FALSE);
    }
  }
  *s++ = '\0';
  if ((NewT = get_num(String PASS_REGS)) == TermNil) {
    Yap_Error(SYNTAX_ERROR, gen_syntax_error(Yap_LookupAtom(String), "number_codes"), "while scanning %s", String);
    return (FALSE);
  }
  return (Yap_unify(ARG1, NewT));
}

static Int 
p_atom_number( USES_REGS1 )
{
  Term   t = Deref(ARG1), t2 = Deref(ARG2);
  Term NewT;

  if (IsVarTerm(t)) {
    char   *String; /* alloc temp space on Trail */

    if (IsVarTerm(t2)) {
      Yap_Error(INSTANTIATION_ERROR, t2, "atom_number/2");
      return FALSE;
    }
    String = Yap_PreAllocCodeSpace();
    if (String+1024 > (char *)AuxSp) {
      String = Yap_ExpandPreAllocCodeSpace(0, NULL, TRUE);
      if (String + 1024 > (char *)AuxSp) {
	/* crash in flames */
	Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "allocating temp space in number_codes/2");
	return FALSE;
      }
    }
    if (IsIntTerm(t2)) {
      sprintf(String, Int_FORMAT, IntOfTerm(t2));
    } else if (IsFloatTerm(t2)) {
      sprintf(String, "%g", FloatOfTerm(t2));
    } else if (IsLongIntTerm(t2)) {
      sprintf(String, Int_FORMAT, LongIntOfTerm(t2));
#if USE_GMP
    } else if (IsBigIntTerm(t2)) {
      while (!Yap_gmp_to_string(t2, String, ((char *)AuxSp-String)-1024, 10 )) {
	size_t sz = Yap_gmp_to_size(t2, 10);
	if (!(String = Yap_ExpandPreAllocCodeSpace(sz, NULL, TRUE))) {
	  Yap_Error(OUT_OF_AUXSPACE_ERROR, t2, LOCAL_ErrorMessage);
	  return FALSE;
	}
      }
#endif
    } else {
      Yap_Error(TYPE_ERROR_NUMBER, t2, "atom_number/2");
      return FALSE;
    }
    NewT = MkAtomTerm(Yap_LookupAtom(String));
    return Yap_unify(NewT, ARG1);
  } else {
    Atom at;
    char *s;
    
    if (!IsAtomTerm(t)) {
      Yap_Error(TYPE_ERROR_ATOM, t, "atom_number/2");
      return FALSE;
    }
    at = AtomOfTerm(t);
    if (IsWideAtom(at)) {
      Yap_Error(SYNTAX_ERROR, gen_syntax_error(at, "number_codes"), "while scanning %S", RepAtom(at)->WStrOfAE);
      return FALSE;
    }
    s = RepAtom(at)->StrOfAE; /* alloc temp space on Trail */
    if ((NewT = get_num(s PASS_REGS)) == TermNil) {
      Yap_Error(SYNTAX_ERROR, gen_syntax_error(at, "atom_number"), "while scanning %s", s);
      return FALSE;
    }
    return Yap_unify(ARG2, NewT);
  }
}

/* $sub_atom_extract(A,Bef,Size,After,SubAt).*/
static Int
p_sub_atom_extract( USES_REGS1 )
{
  Atom at = AtomOfTerm(Deref(ARG1)), nat;
  Int start = IntegerOfTerm(Deref(ARG2));
  Int len = IntegerOfTerm(Deref(ARG3));
  Int leftover;

  if (start < 0)
    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,ARG2,"sub_atom/5");
  if (len < 0)
    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,ARG3,"sub_atom/5");
 start:
  if (IsWideAtom(at)) {
    wchar_t *s = RepAtom(at)->WStrOfAE;
    int max = wcslen(s);
    Int i;

    leftover = max-(start+len);
    if (leftover < 0)
      return FALSE;
    for (i=0;i<len;i++) {
      if ((s+start)[i] > 255) break;
    }
    if (i == len) {
      char *String = Yap_PreAllocCodeSpace();
      if (String + (len+1024) >= (char *)AuxSp) 
	goto expand_auxsp;
      for (i=0;i<len;i++) {
	String[i] = (s+start)[i];
      }
      String[len] = '\0';
      nat = Yap_LookupAtom(String);    
    } else {
      wchar_t *String = (wchar_t *)Yap_PreAllocCodeSpace();
      if (String + (len+1024) >= (wchar_t *)AuxSp) 
	goto expand_auxsp;
      wcsncpy(String, s+start, len);
      String[len] = '\0';
      nat = Yap_LookupWideAtom(String);    
    }
  } else {
    char *s = RepAtom(at)->StrOfAE, *String;
    int max = strlen(s);

    leftover = max-(start+len);
    if (leftover < 0)
      return FALSE;
    String = Yap_PreAllocCodeSpace();
    if (String + (len+1024) >= (char *)AuxSp) 
      goto expand_auxsp;
    strncpy(String, s+start, len);
    String[len] = '\0';
    nat = Yap_LookupAtom(String);
  }
  return Yap_unify(ARG5,MkAtomTerm(nat)) &&
    Yap_unify(ARG4,MkIntegerTerm(leftover));

 expand_auxsp:
  {
    char *String = Yap_ExpandPreAllocCodeSpace(len, NULL, TRUE);
    if (String + 1024 > (char *)AuxSp) {
      /* crash in flames */
      Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG1, "allocating temp space in sub_atom/5");
      return FALSE;
    }
  }
  goto start;
}

/* $sub_atom_extract(A,Bef,Size,After,SubAt).*/
static Int
cont_sub_atom_fetch( USES_REGS1 )
{
  Atom at = AtomOfTerm(EXTRA_CBACK_ARG(5,1));
  Atom subatom = AtomOfTerm(EXTRA_CBACK_ARG(5,2));
  Int offset = IntegerOfTerm(EXTRA_CBACK_ARG(5,3));
  Int sb = IntegerOfTerm(EXTRA_CBACK_ARG(5,4));
  Int sz = IntegerOfTerm(EXTRA_CBACK_ARG(5,5));

  if (IsWideAtom(at)) {
    wchar_t *s = RepAtom(at)->WStrOfAE;
    wchar_t *ins, *where;
    Int start, after;
    Int res;

    
    if (!IsWideAtom(subatom)) {
      /* first convert to wchar_t */
      char *inschars = RepAtom(subatom)->StrOfAE;
      Int i;

      if (offset+sz > sb)
	cut_fail();
      ins = (wchar_t *)Yap_PreAllocCodeSpace();
      while ((ins = (wchar_t *)Yap_PreAllocCodeSpace()) + (sz+1) > (wchar_t *)AuxSp) {
	if (!Yap_ExpandPreAllocCodeSpace(sizeof(wchar_t)*(sz+1), NULL, TRUE)) {
	  Yap_Error(OUT_OF_AUXSPACE_ERROR, ARG5, "allocating temp space in sub_atom/2");
	  return FALSE;
	}
      }
      for (i=0;i<=sz;i++)
	ins[i] = inschars[i];
    } else {
      ins = RepAtom(subatom)->WStrOfAE;
    }
    if (!(where = wcsstr(s+offset, ins))) {
      cut_fail();
    }
    if (!Yap_unify(EXTRA_CBACK_ARG(5,5), ARG3)) {
      cut_fail();
    }
    start = where-s;
    after = sb-(start+sz);
    res = (Yap_unify(MkIntegerTerm(start), ARG2) &&
	   Yap_unify(MkIntegerTerm(after), ARG4));
    if (!res) {
      if (!after) {
	cut_fail();
      } else {
	EXTRA_CBACK_ARG(5,3) = MkIntegerTerm(start+1);
	return FALSE;
      }
    }
    if (!after) {
      cut_succeed();
    } else {
      EXTRA_CBACK_ARG(5,3) = MkIntegerTerm(start+1);
      return TRUE;
    }
  } else {
    char *s = RepAtom(at)->StrOfAE;
    char *ins, *where;
    Int start, after;
    Int res;

    ins = subatom->StrOfAE;
    if (offset+sz > sb) {
      cut_fail();
    }
    if (!(where = strstr(s+offset, ins))) {
      cut_fail();
    }
    if (!Yap_unify(EXTRA_CBACK_ARG(5,5), ARG3)) {
      cut_fail();
    }
    start = where-s;
    after = sb-(start+sz);
    res = (Yap_unify(MkIntegerTerm(start), ARG2) &&
	   Yap_unify(MkIntegerTerm(after), ARG4));
    if (!res) {
      if (!after) {
	cut_fail();
      } else {
	EXTRA_CBACK_ARG(5,3) = MkIntegerTerm(start+1);
	return FALSE;
      }
    }
    if (!after) {
      cut_succeed();
    } else {
      EXTRA_CBACK_ARG(5,3) = MkIntegerTerm(start+1);
      return TRUE;
    }
  }
}

/* $sub_atom_extract(A,Bef,Size,After,SubAt).*/
static Int
init_sub_atom_fetch( USES_REGS1 )
{
  Term tat1, tat2;
  Atom at1, at2;

  EXTRA_CBACK_ARG(5,1) = tat1 = Deref(ARG1);  
  EXTRA_CBACK_ARG(5,2) = tat2 = Deref(ARG5);  
  EXTRA_CBACK_ARG(5,3) = MkIntegerTerm(0);
  at1 = AtomOfTerm(tat1);
  at2 = AtomOfTerm(tat2);
  if (IsWideAtom(at1)) {
    EXTRA_CBACK_ARG(5,4) = MkIntegerTerm(wcslen(at1->WStrOfAE));
    if (IsWideAtom(at2)) {
      EXTRA_CBACK_ARG(5,5) = MkIntegerTerm(wcslen(at2->WStrOfAE));
    } else {
      EXTRA_CBACK_ARG(5,5) = MkIntegerTerm(strlen(at2->StrOfAE));
    }
  } else {
    EXTRA_CBACK_ARG(5,4) = MkIntegerTerm(strlen(at1->StrOfAE));
    if (IsWideAtom(at2)) {
      cut_fail();
    } else {
      EXTRA_CBACK_ARG(5,5) = MkIntegerTerm(strlen(at2->StrOfAE));
    }
  }
  return cont_sub_atom_fetch( PASS_REGS1 );
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
  Yap_InitCPredBack("$sub_atom_fetch", 5, 5, init_sub_atom_fetch, cont_sub_atom_fetch, 0);

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
  Yap_InitCPred("$atom_split", 4, p_atom_split, SafePredFlag);
  Yap_InitCPred("$sub_atom_extract", 5, p_sub_atom_extract, 0);
  Yap_InitCPred("number_chars", 2, p_number_chars, 0);
  Yap_InitCPred("number_atom", 2, p_number_atom, 0);
  Yap_InitCPred("number_codes", 2, p_number_codes, 0);
  Yap_InitCPred("atom_number", 2, p_atom_number, 0);
  Yap_InitCPred("atom_concat", 2, p_atom_concat, 0);
  Yap_InitCPred("atomic_concat", 2, p_atomic_concat, 0);
}
