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
#include "pl-utf8.h"
#include "YapText.h"
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
      if (!IsBigIntTerm(t1)) {
	Yap_Error(REPRESENTATION_ERROR_INT,t1,"char_code/2");
	return(FALSE);
      }
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

	if (code > CHARCODE_MAX) {
	  Yap_Error(REPRESENTATION_ERROR_INT,t1,"char_code/2");
	  return(FALSE);
	}
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
  if (Yap_IsGroundTerm(AtomNameT)) {
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
p_string_to_atomic( USES_REGS1 )
{				/* string_to_atom(?String,?Atom)		 */
  Term            t2 = Deref(ARG2), t1 = Deref(ARG1);
  LOCAL_MAX_SIZE = 1024;

 restart_aux:
  if (Yap_IsGroundTerm(t1)) {
    Term t;
    // verify if an atom, int, float or bignnum
    t = Yap_StringToAtomic( t1 PASS_REGS );
    if (t != 0L)
      return Yap_unify(t, t2);
    // else
  } else {
    Term t0 = Yap_AtomicToString( t2 PASS_REGS );
    if (t0) return Yap_unify(t0, t1);
  }
  if (LOCAL_Error_TYPE && Yap_HandleError( "string_to_atomic/2" )) {
    t1 = Deref(ARG1);
    t2 = Deref(ARG2);
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
  if (Yap_IsGroundTerm(t1)) {
    Atom at;
    // verify if an atom, int, float or bignnum
    at = Yap_StringSWIToAtom( t1 PASS_REGS );
    if (at)
      return Yap_unify(MkAtomTerm(at), t2);
    // else
  } else {
    Term t0 = Yap_AtomSWIToString( t2 PASS_REGS );
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
  if (Yap_IsGroundTerm(list)) {
    Term t1 = Yap_ListToString( list PASS_REGS);
    if (t1)
      return Yap_unify( ARG1, t1 );
  } else {
    Term tf = Yap_StringToListOfCodes(string PASS_REGS);
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
p_atom_string( USES_REGS1 )
{
  // swap arguments
  Term t1 = ARG2;
  ARG2 = ARG1;
  ARG1 = t1;
  return p_string_to_atom( PASS_REGS1 );
}

static Int 
p_atom_chars( USES_REGS1 )
{
  Term t1;
  LOCAL_MAX_SIZE = 1024;

 restart_aux:
  t1  = Deref(ARG1);
  if (Yap_IsGroundTerm(t1)) {
    Term tf = Yap_AtomSWIToListOfAtoms(t1 PASS_REGS);
    if (tf)
      return Yap_unify( ARG2, tf );
  } else {
    /* ARG1 unbound */
    Term   t = Deref(ARG2);
    Atom af = Yap_ListOfAtomsToAtom(t PASS_REGS);
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
  if (Yap_IsGroundTerm(t1)) {
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
p_string_codes( USES_REGS1 )
{
  Term t1;
 restart_aux:
  t1  = Deref(ARG1);
  if (Yap_IsGroundTerm(t1)) {
    Term tf = Yap_StringSWIToListOfCodes(t1 PASS_REGS);
    if (tf)
      return Yap_unify( ARG2, tf );
  } else {
    /* ARG1 unbound */
    Term   t = Deref(ARG2);
    Term tf = Yap_ListSWIToString(t PASS_REGS);
    if (tf)
      return Yap_unify( ARG1, tf );
  }
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "atom_codes/2" )) {
    t1 = Deref(ARG1);
    goto restart_aux;
  }
  return FALSE;
}

static Int 
p_string_chars( USES_REGS1 )
{
  Term t1;
 restart_aux:
  t1  = Deref(ARG1);
  if (Yap_IsGroundTerm(t1)) {
    Term tf = Yap_StringSWIToListOfAtoms(t1 PASS_REGS);
    if (tf)
      return Yap_unify( ARG2, tf );
  } else {
    /* ARG1 unbound */
    Term   t = Deref(ARG2);
    Term tf = Yap_ListSWIToString(t PASS_REGS);
    if (tf)
      return Yap_unify( ARG1, tf );
  }
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "string_chars/2" )) {
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
  if (Yap_IsGroundTerm(t1)) {
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
  if (Yap_IsGroundTerm(t1)) {
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
p_number_string( USES_REGS1 )
{
  Term t1;
 restart_aux:
  t1  = Deref(ARG1);
  if (Yap_IsGroundTerm(t1)) {
    Term tf;
    tf = Yap_NumberToString(t1 PASS_REGS);
    if (tf)
      return Yap_unify( ARG2, tf );
  } else {
    /* ARG1 unbound */
    Term   t = Deref(ARG2);
    Term tf = Yap_StringToNumber(t PASS_REGS);
    if (tf)
      return Yap_unify( ARG1, tf );
  }
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "number_string/2")) {
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
  if (Yap_IsGroundTerm(t1)) {
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
cont_atom_concat3( USES_REGS1 )
{
  Term t3;
  Atom ats[2];
  Int i, max;
 restart_aux:
  t3 = Deref(ARG3);
  i = IntOfTerm(EXTRA_CBACK_ARG(3,1));
  max = IntOfTerm(EXTRA_CBACK_ARG(3,2));
  EXTRA_CBACK_ARG(3,1) = MkIntTerm(i+1);
  if ( ! Yap_SpliceAtom( t3, ats, i, max PASS_REGS ) ) {
    cut_fail();
  } else {
    if (i < max) return Yap_unify( ARG1, MkAtomTerm(ats[0])) &&
		   Yap_unify( ARG2, MkAtomTerm(ats[1])) ;
    if (Yap_unify( ARG1, MkAtomTerm(ats[0])) &&
	Yap_unify( ARG2, MkAtomTerm(ats[1]))) cut_succeed();
    cut_fail();
  }
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError( "atom_concat/3" )) {
      goto restart_aux;
    } else {
      return FALSE;
    }
  }
  cut_fail();
}


static Int 
init_atom_concat3( USES_REGS1 )
{
  Term t1;
  Term t2, t3, ot;
  Atom at;
 restart_aux:
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  t3 = Deref(ARG3);
  if (Yap_IsGroundTerm(t1) && Yap_IsGroundTerm(t2)) {
    at = Yap_ConcatAtoms( t1, t2 PASS_REGS );
    ot = ARG3;
  } else if (Yap_IsGroundTerm(t1) && Yap_IsGroundTerm(t3) ) {
    at = Yap_SubtractHeadAtom( Deref(ARG3), t1 PASS_REGS );
    ot = ARG2;
  } else if (Yap_IsGroundTerm(t2) && Yap_IsGroundTerm(t3)) {
    at = Yap_SubtractTailAtom( Deref(ARG3), t2 PASS_REGS );
    ot = ARG1;
  } else {
    EXTRA_CBACK_ARG(3,1) = MkIntTerm(0);
    EXTRA_CBACK_ARG(3,2) = MkIntTerm(Yap_AtomToLength(t3 PASS_REGS));
    return cont_atom_concat3( PASS_REGS1 );
  }
  if (at) {
    if (Yap_unify(ot, MkAtomTerm(at))) cut_succeed();
    else cut_fail();
  }
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError( "atom_concat/3" )) {
      goto restart_aux;
    } else {
      return FALSE;
    }
  }
  cut_fail();
}

static Int 
p_atomic_concat3( USES_REGS1 )
{
  Term t1;
  Term t2;
  Term t;
  Atom at;
 restart_aux:
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  at = Yap_ConcatAtomics( t1, t2 PASS_REGS );
  if (at) {
    t = MkAtomTerm(at);
    return Yap_unify(ARG3, t);
  }
  /* Error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "atomic_concat/3" )) {
    goto restart_aux;
  }
  return FALSE;
}

static Int 
cont_string_concat3( USES_REGS1 )
{
  Term t3;
  Term ts[2];
  size_t i, max;
 restart_aux:
  t3 = Deref(ARG3);
  i = IntOfTerm(EXTRA_CBACK_ARG(3,1));
  max = IntOfTerm(EXTRA_CBACK_ARG(3,2));
  EXTRA_CBACK_ARG(3,1) = MkIntTerm(i+1);
  if ( ! Yap_SpliceString( t3, ts, i, max PASS_REGS ) ) {
    cut_fail();
  } else {
    if (i < max) return Yap_unify( ARG1, ts[0]) &&
		   Yap_unify( ARG2, ts[1]) ;
    if (Yap_unify( ARG1, ts[0]) &&
	Yap_unify( ARG2, ts[1])) cut_succeed();
    cut_fail();
  }
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError( "string_concat/3" )) {
      goto restart_aux;
    } else {
      return FALSE;
    }
  }
  cut_fail();
}


static Int 
init_string_concat3( USES_REGS1 )
{
  Term t1;
  Term t2, t3, ot;
  Term tf;
 restart_aux:
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  t3 = Deref(ARG3);
  if (Yap_IsGroundTerm(t1) && Yap_IsGroundTerm(t2)) {
    tf = Yap_ConcatStrings( t1, t2 PASS_REGS );
    ot = ARG3;
  } else if (Yap_IsGroundTerm(t1) && Yap_IsGroundTerm(t3) ) {
    tf = Yap_SubtractHeadString(t3, t1 PASS_REGS );
    ot = ARG2;
  } else if (Yap_IsGroundTerm(t2) && Yap_IsGroundTerm(t3)) {
    tf = Yap_SubtractTailString( t3, t2 PASS_REGS );
    ot = ARG1;
  } else {
    EXTRA_CBACK_ARG(3,1) = MkIntTerm(0);
    EXTRA_CBACK_ARG(3,2) = MkIntTerm(Yap_StringToLength(t3 PASS_REGS));
    return cont_string_concat3( PASS_REGS1 );
  }
  if (tf) {
    if (Yap_unify(ot, tf)) { cut_succeed(); }
    else { cut_fail(); }
  }
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError( "string_concat/3" )) {
      goto restart_aux;
    } else {
      return FALSE;
    }
  }
  cut_fail();
}

static Int 
cont_string_code3( USES_REGS1 )
{
  Term t2;
  Int i, j;
  int chr;
  char *s;
  const char *s0;
 restart_aux:
  t2 = Deref(ARG2);
  s0 = StringOfTerm( t2 );
  i = IntOfTerm(EXTRA_CBACK_ARG(3,1)); // offset in coded string, increases by 1..6
  j = IntOfTerm(EXTRA_CBACK_ARG(3,2)); // offset in UNICODE string, always increases by 1
  s = utf8_get_char( s0+i, &chr );
  if (s[0]) {
    EXTRA_CBACK_ARG(3,1) = MkIntTerm(s-s0);
    EXTRA_CBACK_ARG(3,2) = MkIntTerm(j+1);
    return Yap_unify(MkIntegerTerm( chr ), ARG3) && Yap_unify(MkIntegerTerm( j+1 ), ARG1);
  }
  if (Yap_unify(MkIntegerTerm( chr ), ARG3) && Yap_unify(MkIntegerTerm( j ), ARG1))
    cut_succeed();
  else
    cut_fail();
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError( "string_code/3" )) {
      goto restart_aux;
    } else {
      return FALSE;
    }
  }
  cut_fail();
}


static Int 
init_string_code3( USES_REGS1 )
{
  Term t1;
  Term t2;
  const char *s;
 restart_aux:
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  if (IsVarTerm(t2)) {
    LOCAL_Error_TYPE = INSTANTIATION_ERROR;
    LOCAL_Error_Term = t2;
  } else if (!IsStringTerm(t2)) {
    LOCAL_Error_TYPE = TYPE_ERROR_STRING;
    LOCAL_Error_Term = t2;
  } else {
    s = StringOfTerm( t2 );
    t1 = Deref(ARG1);
    if (IsVarTerm(t1)) {
      EXTRA_CBACK_ARG(3,1) = MkIntTerm(0);
      EXTRA_CBACK_ARG(3,2) = MkIntTerm(0);
      return cont_string_code3( PASS_REGS1 );
    } else if (!IsIntegerTerm( t1 )) {
      LOCAL_Error_TYPE = TYPE_ERROR_INTEGER;
      LOCAL_Error_Term = t1;
    } else {
      const char *ns = s;
      int chr;
      Int indx = IntegerOfTerm( t1 );
      if (indx <= 0) {
	if (indx < 0) {
	  LOCAL_Error_TYPE = DOMAIN_ERROR_NOT_LESS_THAN_ZERO;
	  LOCAL_Error_Term = t1;
	}
	cut_fail();
      }
      ns = utf8_skip(s,indx);
      if (ns == NULL) {
	cut_fail(); // silently fail?
      }
      utf8_get_char( ns,  &chr);
      if ( chr == '\0') cut_fail();
      if (Yap_unify(ARG3, MkIntegerTerm(chr))) cut_succeed();
      cut_fail();
    }
  }
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError( "string_code/3" )) {
      goto restart_aux;
    } else {
      return FALSE;
    }
  }
  cut_fail();
}


static Int 
p_get_string_code3( USES_REGS1 )
{
  Term t1;
  Term t2;
  const char *s;
 restart_aux:
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  if (IsVarTerm(t2)) {
    LOCAL_Error_TYPE = INSTANTIATION_ERROR;
    LOCAL_Error_Term = t2;
  } else if (!IsStringTerm(t2)) {
    LOCAL_Error_TYPE = TYPE_ERROR_STRING;
    LOCAL_Error_Term = t2;
  } else {
    s = StringOfTerm( t2 );
    t1 = Deref(ARG1);
    if (IsVarTerm(t1)) {
      LOCAL_Error_TYPE = INSTANTIATION_ERROR;
      LOCAL_Error_Term = t1;
    } else if (!IsIntegerTerm( t1 )) {
      LOCAL_Error_TYPE = TYPE_ERROR_INTEGER;
      LOCAL_Error_Term = t1;
    } else {
      const char *ns = s;
      int chr;
      Int indx = IntegerOfTerm( t1 );
      if (indx <= 0) {
	if (indx < 0) {
	  LOCAL_Error_TYPE = DOMAIN_ERROR_NOT_LESS_THAN_ZERO;
	  LOCAL_Error_Term = t1;
	} else {
	  return FALSE;
	}
      } else {
	ns = utf8_skip(s,indx);
	if (ns == NULL) {
	  return FALSE;
	} else {
	  utf8_get_char( ns,  &chr);
	  if ( chr != '\0')  return Yap_unify(ARG3, MkIntegerTerm(chr));
	}
      }
      return FALSE; // replace by error code
    }
  }
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError( "string_code/3" )) {
      goto restart_aux;
    } else {
      return FALSE;
    }
  }
  cut_fail();
}

static Int 
p_atom_concat2( USES_REGS1 )
{
  Term t1;
  Term *tailp;
  Int n;
 restart_aux:
  t1 = Deref(ARG1);
  n = Yap_SkipList(&t1, &tailp);
  if (*tailp != TermNil) {
    LOCAL_Error_TYPE = TYPE_ERROR_LIST;
  } else {
    seq_tv_t *inpv = (seq_tv_t *)malloc(n*sizeof(seq_tv_t)), out;
    int i = 0;
    Atom at;
    
    if (!inpv) {
      LOCAL_Error_TYPE = OUT_OF_HEAP_ERROR;
      free(inpv);
      goto error;
    }

    while (t1 != TermNil) {
      inpv[i].type = YAP_STRING_ATOM;
      inpv[i].val.t = HeadOfTerm(t1);
      i++;
      t1 = TailOfTerm(t1);
    }
    out.type = YAP_STRING_ATOM;
    if (!Yap_Concat_Text(n, inpv, &out PASS_REGS)) {
      free(inpv);
      goto error;
    }
    free(inpv);
    at =  out.val.a;
    if (at) return Yap_unify(ARG2, MkAtomTerm(at));
  }
 error:
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError( "string_code/3" )) {
      goto restart_aux;
    } else {
      return FALSE;
    }
  }
  cut_fail();
}


static Int 
p_atomic_concat2( USES_REGS1 )
{
  Term t1;
  Term *tailp;
  Int n;
 restart_aux:
  t1 = Deref(ARG1);
  n = Yap_SkipList(&t1, &tailp);
  if (*tailp != TermNil) {
    LOCAL_Error_TYPE = TYPE_ERROR_LIST;
  } else {
    seq_tv_t *inpv = (seq_tv_t *)malloc(n*sizeof(seq_tv_t)), out;
    int i = 0;
    Atom at;
    
    if (!inpv) {
      LOCAL_Error_TYPE = OUT_OF_HEAP_ERROR;
      free(inpv);
      goto error;
    }

    while (t1 != TermNil) {
      inpv[i].type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
      inpv[i].val.t = HeadOfTerm(t1);
      i++;
      t1 = TailOfTerm(t1);
    }
    out.type = YAP_STRING_ATOM;
    if (!Yap_Concat_Text(n, inpv, &out PASS_REGS)) {
      free(inpv);
      goto error;
    }
    free(inpv);
    at =  out.val.a;
    if (at) return Yap_unify(ARG2, MkAtomTerm(at));
  }
 error:
  /* Error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "atom_concat/3" )) {
    goto restart_aux;
  }
  return FALSE;
}

static Int 
p_atomics_to_string2( USES_REGS1 )
{
  Term t1;
  Term *tailp;
  Int n;
 restart_aux:
  t1 = Deref(ARG1);
  n = Yap_SkipList(&t1, &tailp);
  if (*tailp != TermNil) {
    LOCAL_Error_TYPE = TYPE_ERROR_LIST;
  } else {
    seq_tv_t *inpv = (seq_tv_t *)malloc(n*sizeof(seq_tv_t)), out;
    int i = 0;
    Atom at;
    
    if (!inpv) {
      LOCAL_Error_TYPE = OUT_OF_HEAP_ERROR;
      free(inpv);
      goto error;
    }

    while (t1 != TermNil) {
      inpv[i].type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
      inpv[i].val.t = HeadOfTerm(t1);
      i++;
      t1 = TailOfTerm(t1);
    }
    out.type = YAP_STRING_STRING;
    if (!Yap_Concat_Text(n, inpv, &out PASS_REGS)) {
      free(inpv);
      goto error;
    }
    free(inpv);
    at =  out.val.a;
    if (at) return Yap_unify(ARG2, MkAtomTerm(at));
  }
 error:
  /* Error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "atomics_to_string/2" )) {
    goto restart_aux;
  }
  return FALSE;
}

static Int 
p_atomics_to_string3( USES_REGS1 )
{
  Term t1, t2;
  Term *tailp;
  Int n;
 restart_aux:
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  n = Yap_SkipList(&t1, &tailp);
  if (*tailp != TermNil) {
    LOCAL_Error_TYPE = TYPE_ERROR_LIST;
  } else {
    seq_tv_t *inpv = (seq_tv_t *)malloc((n*2-1)*sizeof(seq_tv_t)), out;
    int i = 0;
    Atom at;
    
    if (!inpv) {
      LOCAL_Error_TYPE = OUT_OF_HEAP_ERROR;
      free(inpv);
      goto error;
    }

    while (t1 != TermNil) {
      inpv[i].type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
      inpv[i].val.t = HeadOfTerm(t1);
      i++;
      inpv[i].type = YAP_STRING_STRING|YAP_STRING_ATOM|YAP_STRING_INT|YAP_STRING_FLOAT|YAP_STRING_BIG|YAP_STRING_TERM;
      inpv[i].val.t = t2;
      i++;
      t1 = TailOfTerm(t1);
    }
    out.type = YAP_STRING_STRING;
    if (!Yap_Concat_Text(2*n-1, inpv, &out PASS_REGS)) {
      free(inpv);
      goto error;
    }
    free(inpv);
    at =  out.val.a;
    if (at) return Yap_unify(ARG3, MkAtomTerm(at));
  }
 error:
  /* Error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "atomics_to_string/3" )) {
    goto restart_aux;
  }
  return FALSE;
}

static Int 
p_atom_length( USES_REGS1 )
{
  Term t1;
  Term t2 = Deref(ARG2);
  ssize_t len;

  if (Yap_IsGroundTerm(t2)) {

    if (!IsIntegerTerm(t2)) {
      Yap_Error(TYPE_ERROR_INTEGER, t2, "atom_length/2");
      return(FALSE);
    }
    if (FALSE && (len = IntegerOfTerm(t2)) < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t2, "atom_length/2");
      return(FALSE);
    }
  }
restart_aux:
  t1  = Deref(ARG1);
  len = Yap_AtomicToLength(t1 PASS_REGS);
  if (len != (size_t)-1)
    return Yap_unify( ARG2, MkIntegerTerm(len) );
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "atom_length/2" )) {
    goto restart_aux;
  }
  return FALSE;
}

static Int
p_atomic_length( USES_REGS1 )
{
  Term t1;
  Term t2 = Deref(ARG2);
  ssize_t len;

  if (Yap_IsGroundTerm(t2)) {

    if (!IsIntegerTerm(t2)) {
      Yap_Error(TYPE_ERROR_INTEGER, t2, "atomic_length/2");
      return(FALSE);
    }
    if (FALSE && (len = IntegerOfTerm(t2)) < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t2, "atomic_length/2");
      return(FALSE);
    }
  }
restart_aux:
  t1  = Deref(ARG1);
  len = Yap_AtomicToLength(t1 PASS_REGS);
  if (len != (size_t)-1)
    return Yap_unify( ARG2, MkIntegerTerm(len) );
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "atomic_length/2" )) {
    goto restart_aux;
  }
  return FALSE;
}

static Int
p_string_length( USES_REGS1 )
{
  Term t1;
  Term t2 = Deref(ARG2);
  ssize_t len;

  if (Yap_IsGroundTerm(t2)) {

    if (!IsIntegerTerm(t2)) {
      Yap_Error(TYPE_ERROR_INTEGER, t2, "string_length/2");
      return(FALSE);
    }
    if (FALSE && (len = IntegerOfTerm(t2)) < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t2, "string_length/2");
      return(FALSE);
    }
  }
restart_aux:
  t1  = Deref(ARG1);
  len = Yap_AtomicToLength(t1 PASS_REGS);
  if (len != (size_t)-1)
    return Yap_unify( ARG2, MkIntegerTerm(len) );
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "string_length/2" )) {
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
  if (Yap_IsGroundTerm(t1)) {
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
  if (LOCAL_Error_TYPE && Yap_HandleError( "atom_number/2" )) {
    t1 = Deref(ARG1);
    goto restart_aux;
  }
  return FALSE;
}


static Int 
p_string_number( USES_REGS1 )
{
  Term t1;
 restart_aux:
  t1  = Deref(ARG1);
  if (Yap_IsGroundTerm(t1)) {
    Term tf = Yap_StringToNumber(t1 PASS_REGS);
    if (tf)
      return Yap_unify( ARG2, tf );
  } else {
    /* ARG1 unbound */
    Term   t = Deref(ARG2);
    Term tf = Yap_NumberToString(t PASS_REGS);
    if (tf)
      return Yap_unify( ARG1, tf );
  }
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError( "string_number/2" )) {
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
#define SUB_ATOM_HAS_UTF8  32

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

static Term
build_new_atomic(int mask, wchar_t *wp, char *p, size_t min, size_t len USES_REGS)
{
  Atom nat;
  if (mask & SUB_ATOM_HAS_WIDE) {
    wchar_t *src = wp+min;
    wchar_t *d = alloc_tmp_stack((len+1)*sizeof(wchar_t) PASS_REGS);
    if (!d) return NIL;
    
    wcsncpy(d, src, len);
    d[len] = '\0';
    nat = Yap_LookupMaybeWideAtom(d);
    if (nat)
      return MkAtomTerm(nat);
  } else if (!(mask & SUB_ATOM_HAS_UTF8)) {
    char *src = p+min;
    char *d = alloc_tmp_stack((len+1)*sizeof(char) PASS_REGS);
    if (!d) return NIL;
    
    strncpy(d, src, len);
    d[len] = '\0';
    nat = Yap_LookupAtom(d);
    if (nat)
      return MkAtomTerm(nat);
  } else {
    char *src = p;
    int i, chr;
    Term t = init_tstring( PASS_REGS1  );
    for (i = 0; i < min; i++) src = utf8_get_char(src, &chr);
    char *cp = src, *buf, *lim = cp+strlen(cp);

    LOCAL_TERM_ERROR( 4*(len+1) );
    buf = buf_from_tstring(H);
    while (cp < lim) {
      int chr;
      cp = utf8_get_char(cp, &chr);
      buf = utf8_put_char(buf, chr);
    }
    *buf++ = '\0';
    
    close_tstring( buf  PASS_REGS );
    return t;
  }
  return 0L;
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
check_sub_string_at(int min, Term at, Term nat)
{
  const char *p1, *p2;
  int c1;

  p1 = utf8_skip(StringOfTerm(at), min);
  p2 = StringOfTerm(nat);
  while ( (c1 = *p1++) == *p2++ && c1);
  return c1 == 0;
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

static int
check_sub_string_bef(int max, Term at, Term nat)
{
  size_t len = utf8_strlen1(StringOfTerm(nat));
  int min = max- len;
  const char *p1, *p2;
  int c1;

  if ((Int)(min - len) < 0) return FALSE;

  p1 = utf8_skip(StringOfTerm(at),min);
  p2 = StringOfTerm(nat);
  while ( (c1 = *p1++) == *p2++ && c1);
  return c1 == 0;
}

static Int
cont_sub_atomic( USES_REGS1 )
{
  Term tat1= Deref(ARG1);
  Atom at = NULL;
  int mask;
  size_t min, len, after, sz;
  wchar_t *wp = NULL;
  char *p = NULL;
  Term nat;
  int sub_atom = TRUE;

  mask = IntegerOfTerm(EXTRA_CBACK_ARG(5,1));
  min = IntegerOfTerm(EXTRA_CBACK_ARG(5,2));
  len = IntegerOfTerm(EXTRA_CBACK_ARG(5,3));
  after = IntegerOfTerm(EXTRA_CBACK_ARG(5,4));
  sz = IntegerOfTerm(EXTRA_CBACK_ARG(5,5));

  if (mask & SUB_ATOM_HAS_UTF8) {
    sub_atom = FALSE;
    p = (char *)StringOfTerm(tat1);
  } else if (mask & SUB_ATOM_HAS_WIDE) {
    at = AtomOfTerm(tat1);
    wp = RepAtom(at)->WStrOfAE;
  } else {
    at = AtomOfTerm(tat1);
    p = RepAtom(at)->StrOfAE;
  }
  /* we can have one of two cases: A5 bound or unbound */
  if (mask & SUB_ATOM_HAS_VAL) {
    int found = FALSE;
    nat = Deref(ARG5);
    if (mask & SUB_ATOM_HAS_WIDE) {
      wp = RepAtom(at)->WStrOfAE;
      if (IsWideAtom(AtomOfTerm(nat))) {
	while (!found) {
	  if (wcsncmp(wp+min, AtomOfTerm(nat)->WStrOfAE, len) == 0) {
	    Yap_unify(ARG2, MkIntegerTerm(min));
	    Yap_unify(ARG3, MkIntegerTerm(len));
	    Yap_unify(ARG4, MkIntegerTerm(after));
	    found = TRUE;
	    /* found one, check if there is any left */
	    while (min <= sz-len) {
	      after--;
	      min++;
	      if (wcsncmp(wp+min, AtomOfTerm(nat)->WStrOfAE, len) == 0)
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
	  if (wcsstrcmp(wp+min, AtomOfTerm(nat)->StrOfAE, len) == 0) {
	    Yap_unify(ARG2, MkIntegerTerm(min));
	    Yap_unify(ARG3, MkIntegerTerm(len));
	    Yap_unify(ARG4, MkIntegerTerm(after));
	    found = TRUE;
	    /* found one, check if there is any left */
	    while (min <= sz-len) {
	      after--;
	      min++;
	      if (wcsstrcmp(wp+min, AtomOfTerm(nat)->StrOfAE, len) == 0)
		break;
	    }
	  } else {
	    if (min == sz-len) break;
	    after--;
	    min++;
	  }
	} 
      }     
    } else if (sub_atom) {
      p = RepAtom(at)->StrOfAE;
      while (!found) {
	if (strncmp(p+min, AtomOfTerm(nat)->StrOfAE, len) == 0) {
	  Yap_unify(ARG2, MkIntegerTerm(min));
	  Yap_unify(ARG3, MkIntegerTerm(len));
	  Yap_unify(ARG4, MkIntegerTerm(after));
	  found = TRUE;
	  /* found one, check if there is any left */
	  while (min <= sz-len) {
	    after--;
	    min++;
	    if (strncmp(p+min, AtomOfTerm(nat)->StrOfAE, len) == 0)
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
	p = (char *)utf8_skip(p, min);
        if (utf8_strncmp(p, StringOfTerm(nat), len) == 0) {
          Yap_unify(ARG2, MkIntegerTerm(min));
          Yap_unify(ARG3, MkIntegerTerm(len));
          Yap_unify(ARG4, MkIntegerTerm(after));
          found = TRUE;
          /* found one, check if there is any left */
          while (min <= sz-len) {
	    int chr;
	    p = utf8_get_char(p, &chr);
            after--;
            min++;
            if (utf8_strncmp(p, StringOfTerm(nat), len) == 0)
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
    nat = build_new_atomic(mask, wp, p, min, len PASS_REGS);
    Yap_unify(ARG2, MkIntegerTerm(min));
    Yap_unify(ARG4, MkIntegerTerm(after));
    Yap_unify(ARG5, nat);
    min++;
    if (after-- == 0) cut_succeed();
  } else if (mask & SUB_ATOM_HAS_MIN) {
    after = sz-(min+len);
    nat = build_new_atomic(mask, wp, p, min, len PASS_REGS);
    Yap_unify(ARG3, MkIntegerTerm(len));
    Yap_unify(ARG4, MkIntegerTerm(after));
    Yap_unify(ARG5, nat);
    len++;
    if (after-- == 0) cut_succeed();    
  } else if (mask & SUB_ATOM_HAS_AFTER) {
    len = sz-(min+after);
    nat = build_new_atomic(mask, wp, p, min, len PASS_REGS);
    Yap_unify(ARG2, MkIntegerTerm(min));
    Yap_unify(ARG3, MkIntegerTerm(len));
    Yap_unify(ARG5, nat);
    min++;
    if (len-- == 0) cut_succeed();    
  } else {
    nat = build_new_atomic(mask, wp, p, min, len PASS_REGS);
    Yap_unify(ARG2, MkIntegerTerm(min));
    Yap_unify(ARG3, MkIntegerTerm(len));
    Yap_unify(ARG4, MkIntegerTerm(after));
    Yap_unify(ARG5, nat);
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
init_sub_atomic( int sub_atom USES_REGS )
{
  Term tat1, tbef, tsize, tafter, tout;
  int mask = 0;
  size_t min, len, after, sz;
  wchar_t *wp = NULL;
  char *p = NULL;
  int bnds = 0;
  Term nat = 0L;
  Atom at = NULL;

  tat1 = Deref(ARG1);  
  tbef = Deref(ARG5);  
  EXTRA_CBACK_ARG(5,3) = MkIntegerTerm(0);
  if (IsVarTerm(tat1)) {
    Yap_Error(INSTANTIATION_ERROR, tat1, "sub_atom/5: first argument");
    return FALSE;
  } else if (sub_atom && !IsAtomTerm(tat1)) {
    Yap_Error(TYPE_ERROR_ATOM, tat1, "sub_atom/5");
    return FALSE;
  } else if (!sub_atom && !IsStringTerm(tat1)) {
    Yap_Error(TYPE_ERROR_STRING, tat1, "sub_string/5");
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
    if (sub_atom) {
      if (!IsAtomTerm(tout)) {
	Yap_Error(TYPE_ERROR_ATOM, tout, "sub_atom/5");
	return FALSE;
      } else {
	Atom oat;
	mask |= SUB_ATOM_HAS_VAL|SUB_ATOM_HAS_SIZE;
	oat = AtomOfTerm(tout);
	if (IsWideAtom(oat)) 
	  len = wcslen(RepAtom(oat)->WStrOfAE);
	else
	  len = strlen(RepAtom(oat)->StrOfAE);
      }
    } else {
      if (!IsStringTerm(tout)) {
	Yap_Error(TYPE_ERROR_STRING, tout, "sub_string/5");
	return FALSE;
      } else {
	mask |= SUB_ATOM_HAS_VAL|SUB_ATOM_HAS_SIZE;
	len = utf8_strlen1( StringOfTerm(tout) );
      }
    }
    if (!Yap_unify(ARG3, MkIntegerTerm(len)))
      cut_fail();
    bnds+=2;
  }
  if (sub_atom) {
    at = AtomOfTerm(tat1);
    if (IsWideAtom(at)) {
      mask |= SUB_ATOM_HAS_WIDE;
      wp = RepAtom(at)->WStrOfAE;
      sz = wcslen(wp);
    } else {
      p = RepAtom(at)->StrOfAE; 
      sz = strlen(p);
    }
  } else {
      mask |= SUB_ATOM_HAS_UTF8;
      p = (char *)StringOfTerm(tat1);
      sz = utf8_strlen1(p);
  }
  /* the problem is deterministic if we have two cases */
  if (bnds > 1) {
    int out = FALSE;

    if ((mask & (SUB_ATOM_HAS_MIN|SUB_ATOM_HAS_SIZE)) ==
	(SUB_ATOM_HAS_MIN|SUB_ATOM_HAS_SIZE)) {
      if (min+len > sz) cut_fail();
      if ((Int)(after = (sz-(min+len))) < 0) cut_fail();
      nat = build_new_atomic(mask, wp, p, min, len PASS_REGS);
      if (!nat) cut_fail();
      out = Yap_unify(ARG4,MkIntegerTerm(after)) &&
	Yap_unify(ARG5, nat);
    } else if ((mask & (SUB_ATOM_HAS_MIN|SUB_ATOM_HAS_AFTER)) ==
	       (SUB_ATOM_HAS_MIN|SUB_ATOM_HAS_AFTER)) {
      if (sz < min+after) cut_fail();
      len = sz-(min+after);
      nat = build_new_atomic(mask, wp, p, min, len PASS_REGS);
      if (!nat) cut_fail();
      out = Yap_unify(ARG3,MkIntegerTerm(len)) &&
	Yap_unify(ARG5, nat);
    } else if ((mask & (SUB_ATOM_HAS_SIZE|SUB_ATOM_HAS_AFTER)) ==
	      (SUB_ATOM_HAS_SIZE|SUB_ATOM_HAS_AFTER) ) {
      if (len+after > sz) cut_fail();
      min = sz-(len+after);
      nat = build_new_atomic(mask, wp, p, min, len PASS_REGS);
      if (!nat) cut_fail();
      out = Yap_unify(ARG2,MkIntegerTerm(min)) &&
	Yap_unify(ARG5, nat);
    } else if ((mask & (SUB_ATOM_HAS_MIN|SUB_ATOM_HAS_VAL)) ==
	       (SUB_ATOM_HAS_MIN|SUB_ATOM_HAS_VAL)) {
      if (sub_atom)
	out = check_sub_atom_at(min, at, AtomOfTerm(nat));
      else
	out = check_sub_string_at(min, tat1, tout);
    } else if ((mask & (SUB_ATOM_HAS_AFTER|SUB_ATOM_HAS_VAL)) ==
	       (SUB_ATOM_HAS_AFTER|SUB_ATOM_HAS_VAL)) {
      if (sub_atom)
	out = check_sub_atom_bef(sz - after, at, AtomOfTerm(nat));
      else
	out = check_sub_string_bef(sz - after, tat1, tout);
    } else if ((mask & (SUB_ATOM_HAS_SIZE|SUB_ATOM_HAS_VAL)) ==
	        (SUB_ATOM_HAS_SIZE|SUB_ATOM_HAS_VAL)) {
      if (!sub_atom) {
	out = (utf8_strlen1(StringOfTerm(tout)) == len);
	if (!out) cut_fail();
      } else if (IsWideAtom(AtomOfTerm(tout))) {
	if (!(mask & SUB_ATOM_HAS_VAL)) {
	  cut_fail();
	}
	/* just check length, they may still be several occurrences :( */
	out = (wcslen(RepAtom(AtomOfTerm(tout))->WStrOfAE) == len);
      } else {
	out = (strlen(RepAtom(AtomOfTerm(tout))->StrOfAE) == len);
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
  return cont_sub_atomic( PASS_REGS1 );
}

static Int
init_sub_atom(  USES_REGS1 )
{
  return init_sub_atomic( TRUE PASS_REGS );
}

static Int
init_sub_string(  USES_REGS1 )
{
  return init_sub_atomic( FALSE PASS_REGS );
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
  Yap_InitCPredBack("atom_concat", 3, 2, init_atom_concat3, cont_atom_concat3, 0);
  Yap_InitCPredBack("string_concat", 3, 2, init_string_concat3, cont_string_concat3, 0);
  Yap_InitCPredBack("sub_atom", 5, 5, init_sub_atom, cont_sub_atomic, 0);
  Yap_InitCPredBack("sub_string", 5, 5, init_sub_string, cont_sub_atomic, 0);
  Yap_InitCPredBack("string_code", 3, 1, init_string_code3, cont_string_code3, 0);

}

void
Yap_InitAtomPreds(void)
{
  Yap_InitCPred("name", 2, p_name, 0);
  Yap_InitCPred("string_to_atom", 2, p_string_to_atom, 0);
  Yap_InitCPred("atom_string", 2, p_atom_string, 0);
  Yap_InitCPred("string_to_atomic", 2, p_string_to_atomic, 0);
  Yap_InitCPred("string_to_list", 2, p_string_to_list, 0);
  Yap_InitCPred("char_code", 2, p_char_code, SafePredFlag);
  Yap_InitCPred("atom_chars", 2, p_atom_chars, 0);
  Yap_InitCPred("atom_codes", 2, p_atom_codes, 0);
  Yap_InitCPred("string_codes", 2, p_string_codes, 0);
  Yap_InitCPred("string_chars", 2, p_string_chars, 0);
  Yap_InitCPred("atom_length", 2, p_atom_length, SafePredFlag);
  Yap_InitCPred("atomic_length", 2, p_atomic_length, SafePredFlag);
  Yap_InitCPred("string_length", 2, p_string_length, SafePredFlag);
  Yap_InitCPred("$atom_split", 4, p_atom_split, SafePredFlag);
  Yap_InitCPred("number_chars", 2, p_number_chars, 0);
  Yap_InitCPred("number_atom", 2, p_number_atom, 0);
  Yap_InitCPred("number_string", 2, p_number_string, 0);
  Yap_InitCPred("number_codes", 2, p_number_codes, 0);
  Yap_InitCPred("atom_number", 2, p_atom_number, 0);
  Yap_InitCPred("string_number", 2, p_string_number, 0);
  Yap_InitCPred("$atom_concat", 2, p_atom_concat2, 0);
  Yap_InitCPred("atomic_concat", 2, p_atomic_concat2, 0);
  Yap_InitCPred("atomic_concat", 3, p_atomic_concat3, 0);
  Yap_InitCPred("atomics_to_string", 2, p_atomics_to_string2, 0);
  Yap_InitCPred("atomics_to_string", 3, p_atomics_to_string3, 0);
  Yap_InitCPred("get_string_code", 3, p_get_string_code3, 0);
}
