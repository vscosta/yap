
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
 * File:		atoms.c *
 * comments:	General-pur
pose C implemented system predicates		 *
 *									 *
 * Last rev:     $Date: 2008-07-24 16:02:00 $,$Author: vsc $	     	 *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/**
 * @file atomic.c
 *
 * @namespace prolog
 *
 */

/**
 * @defgroup Predicates_on_Atoms Predicates on Atoms and Strings
 *    @ingroup builtins
 * @{
 *
 *  @brief The following predicates are used to manipulate atoms, strings, lists
 * of codes and lists of chars:
 *
 * \toc
 *
 */

#define HAS_CACHE_REGS 1
/*
 * This file includes the definition of a miscellania of standard operations
 * for yap refering to sequences of characters conversions.
 *
 */

#include "Yap.h"
#include "YapEval.h"
#include "YapHeap.h"
#include "YapText.h"
#include "Yatom.h"
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

static Int name(USES_REGS1);
static Int atom_chars(USES_REGS1);
static Int atom_codes(USES_REGS1);
static Int atom_length(USES_REGS1);
static Int string_length(USES_REGS1);
static Int atom_split(USES_REGS1);
static Int number_chars(USES_REGS1);
static Int number_codes(USES_REGS1);
static Int current_atom(USES_REGS1);
static Int cont_current_atom(USES_REGS1);
static int AlreadyHidden(unsigned char *);
static Int hide_atom(USES_REGS1);
static Int hidden_atom(USES_REGS1);
static Int unhide_atom(USES_REGS1);

static int AlreadyHidden(unsigned char *name) {
  AtomEntry *chain;

  READ_LOCK(INVISIBLECHAIN.AERWLock);
  chain = RepAtom(INVISIBLECHAIN.Entry);
  READ_UNLOCK(INVISIBLECHAIN.AERWLock);
  while (!EndOfPAEntr(chain) &&
         strcmp((char *)chain->StrOfAE, (char *)name) != 0)
    chain = RepAtom(chain->NextOfAE);
  if (EndOfPAEntr(chain))
    return false;
  return true;
}

/** @pred hide_atom(+ _Atom_)
    Make atom  _Atom_ invisible.

    Notice that defining a new atom with the same characters will
    result in a different atom.xs

**/
static Int hide_atom(USES_REGS1) { /* hide(+Atom)		 */
  Atom atomToInclude;
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "hide_atom/1");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM, t1, "hide_atom/1");
    return (FALSE);
  }
  atomToInclude = AtomOfTerm(t1);
  if (AlreadyHidden(RepAtom(atomToInclude)->UStrOfAE)) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, t1,
              "an atom of name %s was already hidden",
              RepAtom(atomToInclude)->StrOfAE);
    return (FALSE);
  }
  AtomEntry *ae = RepAtom(atomToInclude);
  Prop p = ae->PropsOfAE;
  while (p) {
    if (IsPredProperty(p->KindOfPE) || IsDBProperty(p->KindOfPE)) {
      RepPredProp(p)->PredFlags |= HiddenPredFlag;

    } else if (p->KindOfPE == FunctorProperty) {
      Prop q = RepFunctorProp(p)->PropsOfFE;
      while (q) {
        if (IsPredProperty(q->KindOfPE) || IsDBProperty(q->KindOfPE)) {
          RepPredProp(q)->PredFlags |= HiddenPredFlag;
        }
        q = q->NextOfPE;
      }
    }
    p = p->NextOfPE;
  }
  Yap_ReleaseAtom(atomToInclude);
  WRITE_LOCK(INVISIBLECHAIN.AERWLock);
  WRITE_LOCK(RepAtom(atomToInclude)->ARWLock);
  RepAtom(atomToInclude)->NextOfAE = INVISIBLECHAIN.Entry;
  WRITE_UNLOCK(RepAtom(atomToInclude)->ARWLock);
  INVISIBLECHAIN.Entry = atomToInclude;
  WRITE_UNLOCK(INVISIBLECHAIN.AERWLock);
  return (TRUE);
}

/** @pred hidden_atom( +Atom )
    Is the  atom _Ãtom_ visible to Prolog?

**/
static Int hidden_atom(USES_REGS1) { /* '$hidden_atom'(+F)		 */
  Atom at;
  AtomEntry *chain;
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1))
    return (FALSE);
  if (IsAtomTerm(t1))
    at = AtomOfTerm(t1);
  else if (IsApplTerm(t1))
    at = NameOfFunctor(FunctorOfTerm(t1));
  else
    return (FALSE);
  READ_LOCK(INVISIBLECHAIN.AERWLock);
  chain = RepAtom(INVISIBLECHAIN.Entry);
  while (!EndOfPAEntr(chain) && AbsAtom(chain) != at)
    chain = RepAtom(chain->NextOfAE);
  READ_UNLOCK(INVISIBLECHAIN.AERWLock);
  if (EndOfPAEntr(chain))
    return (FALSE);
  return (TRUE);
}

/** @pred unhide_atom(+ _Atom_)
    Make hidden atom  _Atom_ visible

    Note that the operation fails if another atom with name _Atom_ was defined
    since.

**/
static Int unhide_atom(USES_REGS1) { /* unhide_atom(+Atom)		 */
  AtomEntry *atom, *old, *chain;
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "unhide_atom/1");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM, t1, "unhide_atom/1");
    return (FALSE);
  }
  atom = RepAtom(AtomOfTerm(t1));
  WRITE_LOCK(atom->ARWLock);
  if (atom->PropsOfAE != NIL) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, t1, "cannot unhide_atom an atom in use");
    return (FALSE);
  }
  WRITE_LOCK(INVISIBLECHAIN.AERWLock);
  chain = RepAtom(INVISIBLECHAIN.Entry);
  old = NIL;
  while (!EndOfPAEntr(chain) &&
         strcmp((char *)chain->StrOfAE, (char *)atom->StrOfAE) != 0) {
    old = chain;
    chain = RepAtom(chain->NextOfAE);
  }
  if (EndOfPAEntr(chain))
    return (FALSE);
  atom->PropsOfAE = chain->PropsOfAE;
  if (old == NIL)
    INVISIBLECHAIN.Entry = chain->NextOfAE;
  else
    old->NextOfAE = chain->NextOfAE;
  WRITE_UNLOCK(INVISIBLECHAIN.AERWLock);
  WRITE_UNLOCK(atom->ARWLock);
  return (TRUE);
}

/** @pred  char_code(? _A_,? _I_) is iso


    The built-in succeeds with  _A_ bound to character represented as an
    atom, and  _I_ bound to the character code represented as an
    integer. At least, one of either  _A_ or  _I_ must be bound before
    the call.


*/
static Int char_code(USES_REGS1) {
  Int t0 = Deref(ARG1);
  if (IsVarTerm(t0)) {
    Term t1 = Deref(ARG2);
    if (IsVarTerm(t1)) {
      Yap_Error(INSTANTIATION_ERROR, t0, "char_code/2");
      return (FALSE);
    } else if (!IsIntegerTerm(t1)) {
      if (!IsBigIntTerm(t1)) {
        Yap_Error(REPRESENTATION_ERROR_INT, t1, "char_code/2");
        return (FALSE);
      }
      Yap_Error(TYPE_ERROR_INTEGER, t1, "char_code/2");
      return (FALSE);
    } else {
      Int code = IntegerOfTerm(t1);
      Term tout;

      if (code < 0) {
        Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE, t1, "char_code/2");
        return (FALSE);
      }
      {
        unsigned char codes[10];

        if (code > CHARCODE_MAX) {
          Yap_Error(REPRESENTATION_ERROR_INT, t1, "char_code/2");
          return (FALSE);
        }
        size_t n = put_xutf8(codes, code);
        codes[n] = '\0';
        tout = MkAtomTerm(Yap_ULookupAtom(codes));
      }
      return Yap_unify(ARG1, tout);
    }
  } else if (!IsAtomTerm(t0)) {
    Yap_Error(TYPE_ERROR_CHARACTER, t0, "char_code/2");
    return (FALSE);
  } else {
    Atom at = AtomOfTerm(t0);
    Term tf;
    unsigned char *c = RepAtom(at)->UStrOfAE;
    int32_t v = IntegerOfTerm(ARG1);

    get_utf8(c, -1, &v);
    if (!v)
      return false;
    tf = MkIntTerm(v);
    return Yap_unify(ARG2, tf);
  }
}

/** @pred name( _A_, _L_)


    The predicate holds when at least one of the arguments is ground
    (otherwise, an error message will be displayed). The argument  _A_ will
    be unified with an atomic symbol and  _L_ with the list of the ASCII
    codes for the characters of the external representation of  _A_.

    ~~~~~
    name(yap,L).
    ~~~~~
    will return:

    ~~~~~
    L = [121,97,112].
    ~~~~~
    and

    ~~~~~
    name(3,L).
    ~~~~~
    will return:

    ~~~~~
    L = [51].
    ~~~~~


*/
static Int name(USES_REGS1) { /* name(?Atomic,?String)		 */
  Term t2 = Deref(ARG2), NewT, t1 = Deref(ARG1);
  LOCAL_MAX_SIZE = 1024;
  int l = push_text_stack();

restart_aux:
  if (Yap_IsGroundTerm(t1)) {
    if (!IsVarTerm(t2) && !IsPairTerm(t2) && t2 != TermNil) {
      Yap_Error(TYPE_ERROR_LIST, ARG2, "name/2");
      pop_text_stack(l);
      return false;
    }
    // verify if an atom, int, float or bi§gnnum
    NewT = Yap_AtomSWIToListOfCodes(t1 PASS_REGS);
    if (NewT) {
      pop_text_stack(l);
      return Yap_unify(NewT, ARG2);
    }
    // else
  } else if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "name/2");
    pop_text_stack(l);
    return false;
  } else {
    Term at = Yap_ListToAtomic(t2 PASS_REGS);
    if (at) {
      pop_text_stack(l);
      return Yap_unify(at, ARG1);
    }
  }
  if (LOCAL_Error_TYPE && Yap_HandleError("atom/2")) {
    t1 = Deref(ARG1);
    t2 = Deref(ARG2);
    goto restart_aux;
  }
  pop_text_stack(l);
  return false;
}

static Int string_to_atomic(
    USES_REGS1) { /* string_to_atom(?String,?Atom)		 */
  Term t2 = Deref(ARG2), t1 = Deref(ARG1);
  LOCAL_MAX_SIZE = 1024;
  int l = push_text_stack();
restart_aux:
  if (IsStringTerm(t1)) {
    Term t;
    // verify if an atom, int, float or bignnum
    t = Yap_StringToAtomic(t1 PASS_REGS);
    if (t != 0L) {
      pop_text_stack(l);
      return Yap_unify(t, t2);
    }
    // else
  } else if (IsVarTerm(t1)) {
    Term t0 = Yap_AtomicToString(t2 PASS_REGS);
    if (t0) {
      pop_text_stack(l);
      return Yap_unify(t0, t1);
    }
  } else {
    LOCAL_Error_TYPE = TYPE_ERROR_STRING;
  }
  if (LOCAL_Error_TYPE && Yap_HandleError("string_to_atomic/2")) {
    t1 = Deref(ARG1);
    t2 = Deref(ARG2);
    goto restart_aux;
  }
  pop_text_stack(l);
  return false;
}

/// @pred atomic_to_string(?Atomic.?String)
//
// reverse to string_to_atomic(_Atomic_, _String_).
// The second argument may be a sequence of codes or atoms.
//
static Int atomic_to_string(USES_REGS1) {
  Term t1 = ARG1;
  ARG1 = ARG2, ARG2 = t1;
  return string_to_atomic(PASS_REGS1);
}

/// @pred string_to_atom(?String, ?Atom)
//
// Verifies if (a) at least one of the argument is bound. If
// _String_ is bound it must be a string term, list if codes, or
// list of atoms, and _Atom_ musr be bound to a symbol with the
// same text. Otherwise, _Atom_ must be an _Atom_ and _String_
// will unify with a string term of the same text.
//
// Notes:
//   - some versions of YAP allow the first argument to be a
//   number. Please use
//     atomic_to_string/2 in this YAP.
//
static Int string_to_atom(USES_REGS1) { /* string_to_atom(?String,?Atom)
                                         */
  Term t2 = Deref(ARG2), t1 = Deref(ARG1);
  LOCAL_MAX_SIZE = 1024;
  int l = push_text_stack();

restart_aux:
  if (IsStringTerm(t1)) {
    Atom at;
    // verify if an atom, int, float or bignnum
    at = Yap_StringSWIToAtom(t1 PASS_REGS);
    if (at) {
      pop_text_stack(l);
      return Yap_unify(MkAtomTerm(at), t2);
    }
    // else
  } else if (IsVarTerm(t1)) {
    Term t0 = Yap_AtomSWIToString(t2 PASS_REGS);
    if (t0) {
      pop_text_stack(l);
      return Yap_unify(t0, t1);
    }
  } else {
    LOCAL_Error_TYPE = TYPE_ERROR_ATOM;
  }
  if (LOCAL_Error_TYPE && Yap_HandleError("string_to_atom/2")) {
    t1 = Deref(ARG1);
    t2 = Deref(ARG2);
    goto restart_aux;
  }
  pop_text_stack(l);
  return false;
}

/// @pred atom_to_string(?Atom.?String)
//
// reverse to string_to_atom(_Atom_, _String_).
// The second argument may be a sequence of codes or
// atoms.
//
static Int atom_to_string(USES_REGS1) { /* string_to_atom(?String,?Atom)
                                         */
  Term t2 = ARG1;
  ARG1 = ARG2;
  ARG2 = t2;
  return string_to_atom(PASS_REGS1);
}

static Int string_to_list(USES_REGS1) {
  Term list = Deref(ARG2), string = Deref(ARG1);
  LOCAL_MAX_SIZE = 1024;
  int l = push_text_stack();

restart_aux:
  if (IsVarTerm(string)) {
    Term t1 = Yap_ListToString(list PASS_REGS);
    if (t1) {
      pop_text_stack(l);
      return Yap_unify(ARG1, t1);
    }
  } else if (IsStringTerm(string)) {
    Term tf = Yap_StringToListOfCodes(string PASS_REGS);
    {
      pop_text_stack(l);
      return Yap_unify(ARG2, tf);
    }
  } else {
    LOCAL_Error_TYPE = TYPE_ERROR_STRING;
  }
  if (LOCAL_Error_TYPE && Yap_HandleError("string_to_list/2")) {
    string = Deref(ARG1);
    list = Deref(ARG2);
    goto restart_aux;
  }
  {
    pop_text_stack(l);
    return false;
  }
}

/// @pred atom_string(?Atom.?String)
//
// reverse to string_to_atom(_Atom_, _String_).
// The second argument may be a sequence of codes or
// atoms.
//
static Int atom_string(USES_REGS1) {
  Term t1 = Deref(ARG1), t2 = Deref(ARG2);
  LOCAL_MAX_SIZE = 1024;
  int l = push_text_stack();

restart_aux:
  if (IsVarTerm(t1)) {
    Atom at;
    // verify if an atom, int, float or bignnum
    at = Yap_StringSWIToAtom(t2 PASS_REGS);
    if (at) {
      {
        pop_text_stack(l);
        return Yap_unify(MkAtomTerm(at), t1);
      }
    }
    LOCAL_Error_TYPE = YAP_NO_ERROR;
    // else
  } else if (IsAtomTerm(t1)) {
    Term t0 = Yap_AtomSWIToString(t1 PASS_REGS);
    if (t0) {
      pop_text_stack(l);
      return Yap_unify(t0, t2);
    }
  } else {
    LOCAL_Error_TYPE = TYPE_ERROR_ATOM;
  }
  if (LOCAL_Error_TYPE && Yap_HandleError("atom_string/2")) {
    t1 = Deref(ARG1);
    t2 = Deref(ARG2);
    goto restart_aux;
  }
  {
    pop_text_stack(l);
    return false;
  }
}

// The second argument may be a sequence of codes or
// atoms.
//
static Int string_atom(USES_REGS1) { /* string_to_atom(?String,?Atom)
                                      */
  Term t2 = ARG1;
  ARG1 = ARG2;
  ARG2 = t2;
  return atom_string(PASS_REGS1);
}

/** @pred  atom_chars(? _A_,? _L_) is iso


    The predicate holds when at least one of the arguments is
    ground (otherwise, an error message will be displayed).
    The argument  _A_ must be unifiable with an atom, and the
    argument  _L_ with the list of the characters of  _A_.


*/
static Int atom_chars(USES_REGS1) {
  Term t1;
  LOCAL_MAX_SIZE = 1024;
  int l = push_text_stack();

restart_aux:
  t1 = Deref(ARG1);
  if (IsAtomTerm(t1)) {
    Term tf = Yap_AtomSWIToListOfAtoms(t1 PASS_REGS);
    if (tf) {
      pop_text_stack(l);
      return Yap_unify(ARG2, tf);
    }
  } else if (IsVarTerm(t1)) {
    /* ARG1 unbound */
    Term t = Deref(ARG2);
    Atom af = Yap_ListOfAtomsToAtom(t PASS_REGS);
    if (af) {
      pop_text_stack(l);
      return Yap_unify(ARG1, MkAtomTerm(af));
    }
    /* error handling */
  } else {
    LOCAL_Error_TYPE = TYPE_ERROR_ATOM;
  }
  if (LOCAL_Error_TYPE && Yap_HandleError("atom_chars/2")) {
    goto restart_aux;
  }
  {
    pop_text_stack(l);
    return false;
  }
}

/** @pred  atom_codes(?A, ?L) is iso


    The predicate holds when at least one of the arguments is
    ground (otherwise, YAP will generate an error event.   _A_ must be unifiable
   with an atom, and the argument  _L_ with the list of the character codes for
   string  _A_.


*/
static Int atom_codes(USES_REGS1) {
  Term t1;
  LOCAL_MAX_SIZE = 1024;
  int l = push_text_stack();

restart_aux:
  t1 = Deref(ARG1);
  if (IsAtomTerm(t1)) {
    Term tf = Yap_AtomSWIToListOfCodes(t1 PASS_REGS);
    if (tf) {
      pop_text_stack(l);
      return Yap_unify(ARG2, tf);
    }
  } else if (IsVarTerm(t1)) {
    /* ARG1 unbound */
    Term t = Deref(ARG2);
    Atom af = Yap_ListOfCodesToAtom(t PASS_REGS);
    if (af) {
      pop_text_stack(l);
      return Yap_unify(ARG1, MkAtomTerm(af));
    }
    /* error handling */
  } else {
    Yap_ThrowError(TYPE_ERROR_ATOM, t1, NULL);
  }
  if (LOCAL_Error_TYPE && Yap_HandleError("atom_codes/2")) {
    goto restart_aux;
  }
  {
    pop_text_stack(l);
    return false;
  }
}

static Int string_codes(USES_REGS1) {
  Term t1;
  t1 = Deref(ARG1);
  int l = push_text_stack();
restart_aux:
  if (IsStringTerm(t1)) {
    Term tf = Yap_StringSWIToListOfCodes(t1 PASS_REGS);
    if (tf) {
      pop_text_stack(l);
      return Yap_unify(ARG2, tf);
    }
  } else if (IsVarTerm(t1)) {
    /* ARG1 unbound */
    Term t = Deref(ARG2);
    Term tf = Yap_ListSWIToString(t PASS_REGS);
    if (tf) {
      pop_text_stack(l);
      return Yap_unify(ARG1, tf);
    }
  } else {
    LOCAL_Error_TYPE = TYPE_ERROR_STRING;
  }
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError("atom_codes/2")) {
    t1 = Deref(ARG1);
    goto restart_aux;
  }
  {
    pop_text_stack(l);
    return false;
  }
}

static Int string_chars(USES_REGS1) {
  Term t1;
  t1 = Deref(ARG1);
  int l = push_text_stack();
restart_aux:
  if (IsStringTerm(t1)) {
    Term tf = Yap_StringSWIToListOfAtoms(t1 PASS_REGS);
    if (tf) {
      pop_text_stack(l);
      return Yap_unify(ARG2, tf);
    }
  } else if (IsVarTerm(t1)) {
    /* ARG1 unbound */
    Term t = Deref(ARG2);
    Term tf = Yap_ListSWIToString(t PASS_REGS);
    if (tf) {
      pop_text_stack(l);
      return Yap_unify(ARG1, tf);
    }
  } else {
    LOCAL_Error_TYPE = TYPE_ERROR_STRING;
  }
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError("string_chars/2")) {
    t1 = Deref(ARG1);
    goto restart_aux;
  }
  {
    pop_text_stack(l);
    return false;
  }
}

/** @pred  number_chars(? _I_,? _L_) is iso

    The predicate holds when at least one of the arguments is
    ground (otherwise, an error message will be displayed).
    The argument  _I_ must be unifiable with a number, and the
    argument  _L_ with the list of the characters of the
    external representation of  _I_.

*/
static Int number_chars(USES_REGS1) {
    Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);;
  int l = push_text_stack();
  if (IsNonVarTerm(t1)) {
    if (IsVarTerm(t2)) {
      pop_text_stack(l);
      return Yap_unify(ARG2, Yap_NumberToListOfAtoms(t1));
  } else {
    Term t2 = Yap_ListToNumber( Deref(ARG2) );
    pop_text_stack(l);
    if (t1&& t2) {
          return Yap_unify(t1, t2);
    }
    }
  } else {
    /* ARG1 unbound */
    Term t = Deref(ARG2);
    Term tf = Yap_ListToNumber(t PASS_REGS);
    if (tf) {
      pop_text_stack(l);
      return Yap_unify(ARG1, tf);
    }
 }
    // error
  pop_text_stack(l);
LOCAL_ActiveError->errorRawTerm = 0;
    Yap_ThrowExistingError();

    return false;
}

/** @pred  number_atom(? _I_,? _A_){te



    The predicate holds when at least one of the arguments is
    ground (otherwise, an error message will be displayed).
    The argument  _I_ must be unifiable with a number, and the
    argument  _A_ must be unifiable with an atom representing
    the number.


*/
static Int number_atom(USES_REGS1) {
  Term t1;
  int l = push_text_stack();
restart_aux:
  t1 = Deref(ARG1);
  if (IsNumTerm(t1)) {
    Term t2 = Deref(ARG2);
    Atom af;
    af = Yap_NumberToAtom(t1 PASS_REGS);
    if (af) {
      if (IsVarTerm(t2)) {

        {
          pop_text_stack(l);
          return Yap_unify(t1, t2);
        }
      } else {
        t2 = Yap_AtomToNumber(t2 PASS_REGS);
        if (t2) {
          {
            pop_text_stack(l);
            return Yap_unify(t1, t2);
          }
        }
      }
    }
  } else if (IsVarTerm(t1)) {
    /* ARG1 unbound */
    Term t = Deref(ARG2);
    Term tf = Yap_AtomToNumber(t PASS_REGS);
    {
      pop_text_stack(l);
      return Yap_unify(ARG1, tf);
    }
  } else if (IsVarTerm(t1)) {
    LOCAL_Error_TYPE = TYPE_ERROR_NUMBER;
  } /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError("number_atom/2")) {
    goto restart_aux;
  }
  {
    pop_text_stack(l);
    return false;
  }
}

/** @pred  number_string(? _I_,? _L_)


    The predicate holds when at least one of the arguments is
    ground (otherwise, an error message will be displayed).
    The argument  _I_ must be unifiable with a number, and the
    argument  _L_ must be unifiable with a term string
    representing the number.


*/

static Int number_string(USES_REGS1) {
  Term t1;
  int l = push_text_stack();
restart_aux:
  t1 = Deref(ARG1);
  if (IsNumTerm(t1)) {
    Term tf;
    tf = Yap_NumberToString(t1 PASS_REGS);
    if (tf) {
      pop_text_stack(l);
      return Yap_unify(ARG2, tf);
    }
  } else if (IsVarTerm(t1)) {
    /* ARG1 unbound */
    Term t = Deref(ARG2);
    Term tf = Yap_StringToNumber(t PASS_REGS);
    if (tf) {
      pop_text_stack(l);
      return Yap_unify(ARG1, tf);
    }
  } else {
    LOCAL_Error_TYPE = TYPE_ERROR_NUMBER;
  }
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError("number_string/2")) {
    goto restart_aux;
  }
  {
    pop_text_stack(l);
    return false;
  }
  
}

/** @pred  number_codes(? _I_,? _L_)


    The predicate holds when at least one of the arguments is
    ground (otherwise, an error message will be displayed).
    The argument  _I_ must be unifiable with a number, and the
    argument  _L_ must be unifiable with a list of UNICODE
    numbers representing the number.


*/
static Int number_codes(USES_REGS1) {
    Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);;
  int l = push_text_stack();
  if (IsNonVarTerm(t1)) {
    if (IsVarTerm(t2)) {
      pop_text_stack(l);
      return Yap_unify(ARG2, Yap_NumberToListOfCodes(t1));
  } else {
    Term t2 = Yap_ListToNumber( Deref(ARG2) );
    pop_text_stack(l);
    if (t1&& t2) {
          return Yap_unify(t1, t2);
    }
    }
  } else {
    /* ARG1 unbound */
    Term t = Deref(ARG2);
    Term tf = Yap_ListToNumber(t PASS_REGS);
    if (tf) {
      pop_text_stack(l);
      return Yap_unify(ARG1, tf);
    }
 }
    // error
  pop_text_stack(l);
LOCAL_ActiveError->errorRawTerm = 0;
    Yap_ThrowExistingError();

    return false;
 }

static Int cont_atom_concat3(USES_REGS1) {
  Term t3;
  Atom ats[2];
  Int i, max;
restart_aux:
  t3 = Deref(ARG3);
  i = IntOfTerm(EXTRA_CBACK_ARG(3, 1));
  max = IntOfTerm(EXTRA_CBACK_ARG(3, 2));
  EXTRA_CBACK_ARG(3, 1) = MkIntTerm(i + 1);

  int l = push_text_stack();
  bool rc = Yap_SpliceAtom(t3, ats, i, max PASS_REGS);
  pop_text_stack(l);
  if (LOCAL_Error_TYPE == YAP_NO_ERROR) {
    if (rc) {
      if (i < max) {
        return (Yap_unify(ARG1, MkAtomTerm(ats[0])) &&
                Yap_unify(ARG2, MkAtomTerm(ats[1])));
      }
      return do_cut(Yap_unify(ARG1, MkAtomTerm(ats[0])) &&
                    Yap_unify(ARG2, MkAtomTerm(ats[1])));
    } else {
      cut_fail();
    }
  }
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError("atom_concat/3")) {
      goto restart_aux;
    }
  }
  cut_fail();
}

static Int atom_concat3(USES_REGS1) {
  Term t1;
  Term t2, t3, ot;
  Atom at;
  bool g1, g2, g3;
restart_aux:
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  t3 = Deref(ARG3);
  g1 = Yap_IsGroundTerm(t1);
  g2 = Yap_IsGroundTerm(t2);
  g3 = Yap_IsGroundTerm(t3);
  if (g1 && g2) {
    int l = push_text_stack();
    at = Yap_ConcatAtoms(t1, t2 PASS_REGS);
    pop_text_stack(l);
    ot = ARG3;
  } else if (g1 && g3) {
    int l = push_text_stack();
    at = Yap_SubtractHeadAtom(t3, t1 PASS_REGS);
    pop_text_stack(l);
    ot = ARG2;
  } else if (g2 && g3) {
    int l = push_text_stack();
    at = Yap_SubtractTailAtom(t3, t2 PASS_REGS);
    pop_text_stack(l);
    ot = ARG1;
  } else if (g3) {
    Int len = Yap_AtomToUnicodeLength(t3 PASS_REGS);
    if (len < 0) {
      Yap_ThrowError(-len,ARG3,"atom_concat(-X,-Y,+atom:Z");
      cut_fail();
    }
    EXTRA_CBACK_ARG(3, 1) = MkIntTerm(0);
    EXTRA_CBACK_ARG(3, 2) = MkIntTerm(len);
    { return cont_atom_concat3(PASS_REGS1); }
  } else {
    LOCAL_Error_TYPE = INSTANTIATION_ERROR;
    at = NULL;
  }
  if (at) {
    return do_cut(Yap_unify(ot, MkAtomTerm(at)));
  }
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError("atom_concat/3")) {
      goto restart_aux;
    }
  }
  cut_fail();
}

#define CastToNumeric(x) CastToNumeric__(x PASS_REGS)

static Term CastToNumeric__(Atom at USES_REGS) {
  Term t;
  if ((t = Yap_AtomToNumber(MkAtomTerm(at) PASS_REGS))) {
    return t;
  } else {
    return MkAtomTerm(at);
  }
}

static Int cont_atomic_concat3(USES_REGS1) {
  Term t3;
  Atom ats[2];
  size_t i, max;
restart_aux:
  t3 = Deref(ARG3);
  i = IntOfTerm(EXTRA_CBACK_ARG(3, 1));
  max = IntOfTerm(EXTRA_CBACK_ARG(3, 2));
  EXTRA_CBACK_ARG(3, 1) = MkIntTerm(i + 1);
  int l = push_text_stack();
  bool rc = Yap_SpliceAtom(t3, ats, i, max PASS_REGS);
  pop_text_stack(l);
  if (!rc) {
    cut_fail();
  } else {
    Term t1 = CastToNumeric(ats[0]);
    Term t2 = CastToNumeric(ats[1]);
    if (i < max) {
      return Yap_unify(ARG1, t1) && Yap_unify(ARG2, t2);
    }
    if (Yap_unify(ARG1, t1) && Yap_unify(ARG2, t2))
      cut_succeed();
    cut_fail();
  }
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError("string_concat/3")) {
      goto restart_aux;
    } else {
      return false;
    }
  }
  cut_fail();
}

static Int atomic_concat3(USES_REGS1) {
  Term t1;
  Term t2, t3, ot;
  Atom at = NULL;
  bool g1, g2, g3;
restart_aux:
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  t3 = Deref(ARG3);
  g1 = Yap_IsGroundTerm(t1);
  g2 = Yap_IsGroundTerm(t2);
  g3 = Yap_IsGroundTerm(t3);
  if (g1 && g2) {
    int l = push_text_stack();
    at = Yap_ConcatAtomics(t1, t2 PASS_REGS);
    pop_text_stack(l);
    ot = ARG3;
  } else if (g1 && g3) {
    int l = push_text_stack();
    at = Yap_SubtractHeadAtom(t3, t1 PASS_REGS);
    pop_text_stack(l);
    ot = ARG2;
  } else if (g2 && g3) {
    int l = push_text_stack();
    at = Yap_SubtractTailAtom(t3, t2 PASS_REGS);
    pop_text_stack(l);
    ot = ARG1;
  } else if (g3) {
    Int len = Yap_AtomicToUnicodeLength(t3 PASS_REGS);
    if (len <= 0) {
      cut_fail();
    }
    EXTRA_CBACK_ARG(3, 1) = MkIntTerm(0);
    EXTRA_CBACK_ARG(3, 2) = MkIntTerm(len);
    return cont_atomic_concat3(PASS_REGS1);
  } else {
    LOCAL_Error_TYPE = INSTANTIATION_ERROR;
    at = NULL;
  }
  if (at) {
    if (Yap_unify(ot, MkAtomTerm(at))) {
      cut_succeed();
    } else {
      cut_fail();
    }
  }
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError("atomic_concat/3")) {
      goto restart_aux;
    } else {
      return false;
    }
  }
  cut_fail();
}

static Int cont_string_concat3(USES_REGS1) {
  Term t3;
  Term ts[2];
  size_t i, max;
restart_aux:
  t3 = Deref(ARG3);
  i = IntOfTerm(EXTRA_CBACK_ARG(3, 1));
  max = IntOfTerm(EXTRA_CBACK_ARG(3, 2));
  EXTRA_CBACK_ARG(3, 1) = MkIntTerm(i + 1);
  int l;
  l = push_text_stack();
  bool rc = Yap_SpliceString(t3, ts, i, max PASS_REGS);
  pop_text_stack(l);
  if (!rc) {
    cut_fail();
  } else {
    if (i < max) {
      return Yap_unify(ARG1, ts[0]) && Yap_unify(ARG2, ts[1]);
    }
    return do_cut(Yap_unify(ARG1, ts[0]) && Yap_unify(ARG2, ts[1]));
    cut_succeed();
  }
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError("string_concat/3")) {
      goto restart_aux;
    } else {
      return FALSE;
    }
  }
  cut_fail();
}

static Int string_concat3(USES_REGS1) {
  Term t1;
  Term t2, t3, ot;
  Term tf = 0;
  bool g1, g2, g3;
restart_aux:
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  t3 = Deref(ARG3);
  g1 = Yap_IsGroundTerm(t1);
  g2 = Yap_IsGroundTerm(t2);
  g3 = Yap_IsGroundTerm(t3);

  if (g1 && g2) {
    int l;
    l = push_text_stack();
    tf = Yap_ConcatStrings(t1, t2 PASS_REGS);
    pop_text_stack(l);
    ot = ARG3;
  } else if (g1 && g3) {
    int l;
    l = push_text_stack();
    tf = Yap_SubtractHeadString(t3, t1 PASS_REGS);
    pop_text_stack(l);
    ot = ARG2;
  } else if (g2 && g3) {
    int l;
    l = push_text_stack();
    tf = Yap_SubtractTailString(t3, t2 PASS_REGS);
    pop_text_stack(l);
    ot = ARG1;
  } else if (g3) {
    Int len = Yap_StringToUnicodeLength(t3 PASS_REGS);
    if (len <= 0) {
      cut_fail();
    }
    EXTRA_CBACK_ARG(3, 1) = MkIntTerm(0);
    EXTRA_CBACK_ARG(3, 2) = MkIntTerm(len);
    { return cont_string_concat3(PASS_REGS1); }
  } else {
    LOCAL_Error_TYPE = INSTANTIATION_ERROR;
  }
  if (tf) {
    return do_cut(Yap_unify(ot, tf));
  }
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError("atom_concat/3")) {
      goto restart_aux;
    } else {
      return false;
    }
  }
  cut_fail();
}

static Int cont_string_code3(USES_REGS1) {
  Term t2;
  Int i, j;
  utf8proc_int32_t chr;
  const unsigned char *s;
  const unsigned char *s0;
  int l;
  l = push_text_stack();
restart_aux:
  t2 = Deref(ARG2);
  s0 = UStringOfTerm(t2);
  i = IntOfTerm(
      EXTRA_CBACK_ARG(3, 1)); // offset in coded string, increases by 1..6
  j = IntOfTerm(EXTRA_CBACK_ARG(3, 2)); // offset in UNICODE
  // string, always
  // increases by 1
  s = (s0 + i) + get_utf8((unsigned char *)s0 + i, -1, &chr);
  if (s[0]) {
    EXTRA_CBACK_ARG(3, 1) = MkIntTerm(s - s0);
    EXTRA_CBACK_ARG(3, 2) = MkIntTerm(j + 1);
    return (Yap_unify(MkIntegerTerm(chr), ARG3) &&
            Yap_unify(MkIntegerTerm(j + 1), ARG1));
  }
  return do_cut(Yap_unify(MkIntegerTerm(chr), ARG3) &&
                Yap_unify(MkIntegerTerm(j), ARG1));
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError("string_code/3")) {
      goto restart_aux;
    } else {
      {
        pop_text_stack(l);
        return false;
      }
    }
  }
  cut_fail();
}

static Int string_code3(USES_REGS1) {
  Term t1;
  Term t2;
  const unsigned char *s;
  int l = push_text_stack();
restart_aux:
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  if (IsVarTerm(t2)) {
    LOCAL_Error_TYPE = INSTANTIATION_ERROR;
  } else if (!IsStringTerm(t2)) {
    LOCAL_Error_TYPE = TYPE_ERROR_STRING;
  } else {
    s = UStringOfTerm(t2);
    t1 = Deref(ARG1);
    if (IsVarTerm(t1)) {
      EXTRA_CBACK_ARG(3, 1) = MkIntTerm(0);
      EXTRA_CBACK_ARG(3, 2) = MkIntTerm(0);
      {
        pop_text_stack(l);
        return cont_string_code3(PASS_REGS1);
      }
    } else if (!IsIntegerTerm(t1)) {
      LOCAL_Error_TYPE = TYPE_ERROR_INTEGER;
    } else {
      const unsigned char *ns = s;
      utf8proc_int32_t chr;
      Int indx = IntegerOfTerm(t1);
      if (indx <= 0) {
        if (indx < 0) {
          LOCAL_Error_TYPE = DOMAIN_ERROR_NOT_LESS_THAN_ZERO;
        }
        cut_fail();
      }
      ns = skip_utf8(s, indx);
      if (ns == NULL) {
        cut_fail(); // silently fail?
      }
      get_utf8(ns, -1, &chr);
      if (chr == '\0')
        cut_fail();
      if (Yap_unify(ARG3, MkIntegerTerm(chr)))
        cut_succeed();
      cut_fail();
    }
  }
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError("string_code/3")) {
      goto restart_aux;
    } else {
      {
        pop_text_stack(l);
        return false;
      }
    }
  }
  cut_fail();
}

static Int get_string_code3(USES_REGS1) {
  Term t1;
  Term t2;
  const unsigned char *s;
restart_aux:
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  if (IsVarTerm(t2)) {
    LOCAL_Error_TYPE = INSTANTIATION_ERROR;
  } else if (!IsStringTerm(t2)) {
    LOCAL_Error_TYPE = TYPE_ERROR_STRING;
  } else {
    s = UStringOfTerm(t2);
    t1 = Deref(ARG1);
    if (IsVarTerm(t1)) {
      LOCAL_Error_TYPE = INSTANTIATION_ERROR;

    } else if (!IsIntegerTerm(t1)) {
      LOCAL_Error_TYPE = TYPE_ERROR_INTEGER;
    } else {
      const unsigned char *ns = s;
      Int indx = IntegerOfTerm(t1);

      if (indx <= 0) {
        if (indx < 0) {
          LOCAL_Error_TYPE = DOMAIN_ERROR_NOT_LESS_THAN_ZERO;
        } else {
          return false;
        }
      } else {
        indx -= 1;
        ns = skip_utf8(ns, indx);
        if (ns == NULL) {
          return false;
        }
      }
      utf8proc_int32_t chr;
      get_utf8(ns, -1, &chr);
      if (chr != '\0') {
        return Yap_unify(ARG3, MkIntegerTerm(chr));
      }
      return false;
    }
  } // replace by error cod )e
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError("string_code/3")) {
      goto restart_aux;
    } else {
      return false;
    }
  }
  cut_fail();
}


static Int atom_concat2(USES_REGS1) {
  Term t1;
  Term *tailp;
  Int n;
  int l = push_text_stack();
restart_aux:
  t1 = Deref(ARG1);
  n = Yap_SkipList(&t1, &tailp);
  if (*tailp != TermNil) {
    LOCAL_Error_TYPE = TYPE_ERROR_LIST;
  } else {
    seq_tv_t *inpv = (seq_tv_t *)Malloc(n * sizeof(seq_tv_t)), out;
    int i = 0;
    Atom at;

    if (!inpv) {
      LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;
      goto error;
    }

    while (t1 != TermNil) {
      inpv[i].type = YAP_STRING_ATOM, inpv[i].val.t = HeadOfTerm(t1);
      inpv[i].enc = ENC_ISO_UTF8;
      i++;
      t1 = TailOfTerm(t1);
    }
    out.type = YAP_STRING_ATOM;
    if (!Yap_Concat_Text(n, inpv, &out PASS_REGS)) {
      goto error;
    }
    at = out.val.a;
    if (at) {
      pop_text_stack(l);
      return Yap_unify(ARG2, MkAtomTerm(at));
    }
  }
error:
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError("atom_concat/2")) {
      goto restart_aux;
    } else {
      pop_text_stack(l);
      return false;
    }
  }
  cut_fail();
}

static Int string_concat2(USES_REGS1) {
  Term t1;
  Term *tailp;
  Int n;
  int l = push_text_stack();
restart_aux:
  t1 = Deref(ARG1);
  n = Yap_SkipList(&t1, &tailp);
  if (*tailp != TermNil) {
    LOCAL_Error_TYPE = TYPE_ERROR_LIST;
  } else {
    seq_tv_t *inpv = (seq_tv_t *)Malloc(n * sizeof(seq_tv_t));
    seq_tv_t *out = (seq_tv_t *)Malloc(sizeof(seq_tv_t));
    int i = 0;
    if (!inpv) {
      LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;
      goto error;
    }

    while (t1 != TermNil) {
      inpv[i].type = YAP_STRING_STRING;
      inpv[i].val.t = HeadOfTerm(t1);
      inpv[i].enc = ENC_ISO_UTF8;
      i++;
      t1 = TailOfTerm(t1);
    }
    out->type = YAP_STRING_STRING;
    if (!Yap_Concat_Text(n, inpv, out PASS_REGS)) {
      goto error;
    }
    if (out->val.t) {
      bool rc = Yap_unify(ARG2, out->val.t);
      pop_text_stack(l);
      return rc;
    }
  }
error:
  /* Error handling */
  if (LOCAL_Error_TYPE) {
    if (Yap_HandleError("string_code/3")) {
      goto restart_aux;
    } else {
      {
        pop_text_stack(l);
        return false;
      }
    }
  }
  cut_fail();
}

static Int atomic_concat2(USES_REGS1) {
  Term t1;
  Term *tailp;
  Int n;
  int l = push_text_stack();
restart_aux:
  t1 = Deref(ARG1);
  n = Yap_SkipList(&t1, &tailp);
  if (*tailp != TermNil) {
    LOCAL_Error_TYPE = TYPE_ERROR_LIST;
  } else {
    int i = 0;
    Atom at;

    if (n == 1) {
      bool rc = Yap_unify(ARG2, HeadOfTerm(t1));
      pop_text_stack(l);
      return rc;
    }
    seq_tv_t *inpv = (seq_tv_t *)Malloc(n * sizeof(seq_tv_t));
    seq_tv_t *out = (seq_tv_t *)Malloc(sizeof(seq_tv_t));
    if (!inpv) {
      LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;
      goto error;
    }

    while (t1 != TermNil) {
      inpv[i].type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
                     YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_CHARS |
                     YAP_STRING_CODES;
      inpv[i].val.t = HeadOfTerm(t1);
      inpv[i].enc = ENC_ISO_UTF8;
      i++;
      t1 = TailOfTerm(t1);
    }
    out->type = YAP_STRING_ATOM;
    if (!Yap_Concat_Text(n, inpv, out PASS_REGS)) {
      goto error;
    }
    at = out->val.a;
    pop_text_stack(l);
    if (at) {
      bool rc = Yap_unify(ARG2, MkAtomTerm(at));
      return rc;
    }
  }
error:
  /* Error handling */
    pop_text_stack(l);
  if (LOCAL_Error_TYPE && Yap_HandleError("atom_concat/3")) {
    goto restart_aux;
  }
  { return FALSE; }
}

static Int atomics_to_string2(USES_REGS1) {
  Term t1;
  Term *tailp;
  Int n;
  int l = push_text_stack();
restart_aux:
  t1 = Deref(ARG1);
  n = Yap_SkipList(&t1, &tailp);
  if (*tailp != TermNil) {
    LOCAL_Error_TYPE = TYPE_ERROR_LIST;
  } else {
    seq_tv_t *inpv = (seq_tv_t *)Malloc(n * sizeof(seq_tv_t)), out;
    int i = 0;
    Atom at;

    if (!inpv) {
      LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;
      goto error;
    }

    while (t1 != TermNil) {
      inpv[i].type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
                     YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
      inpv[i].val.t = HeadOfTerm(t1);
      inpv[i].enc = ENC_ISO_UTF8;
      i++;
      t1 = TailOfTerm(t1);
    }
    out.type = YAP_STRING_STRING;
    if (!Yap_Concat_Text(n, inpv, &out PASS_REGS)) {
      goto error;
    }
    at = out.val.a;
    if (at) {
      pop_text_stack(l);
      return Yap_unify(ARG2, MkAtomTerm(at));
    }
  }
error:
  /* Error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError("atomics_to_string/2")) {
    goto restart_aux;
  }
  {
    pop_text_stack(l);
    return false;
  }
}

static Int atomics_to_string3(USES_REGS1) {
  Term t1, t2;
  Term *tailp;
  Int n;
  int l = push_text_stack();
restart_aux:
  t1 = Deref(ARG1);
  t2 = Deref(ARG2);
  n = Yap_SkipList(&t1, &tailp);
  if (*tailp != TermNil) {
    LOCAL_Error_TYPE = TYPE_ERROR_LIST;
  } else {
    seq_tv_t *inpv = (seq_tv_t *)Malloc((n * 2 - 1) * sizeof(seq_tv_t)), out;
    int i = 0;
    Atom at;

    if (!inpv) {
      LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;
      goto error;
    }

    while (t1 != TermNil) {
      inpv[i].type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
                     YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
      inpv[i].val.t = HeadOfTerm(t1);
      inpv[i].enc = ENC_ISO_UTF8;
      i++;
      inpv[i].type = YAP_STRING_STRING | YAP_STRING_ATOM | YAP_STRING_INT |
                     YAP_STRING_FLOAT | YAP_STRING_BIG | YAP_STRING_TERM;
      inpv[i].val.t = t2;
      inpv[i].enc = ENC_ISO_UTF8;
      i++;
      t1 = TailOfTerm(t1);
    }
    out.type = YAP_STRING_STRING;
    if (!Yap_Concat_Text(2 * n - 1, inpv, &out PASS_REGS)) {
      goto error;
    }
    at = out.val.a;
    if (at) {
      pop_text_stack(l);
      return Yap_unify(ARG3, MkAtomTerm(at));
    }
  }
error:
  /* Error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError("atomics_to_string/3")) {
    goto restart_aux;
  }
  pop_text_stack(l);
  return false;
}

/** @pred  atom_length(+ _A_,? _I_) is iso


    The predicate holds when the first argument is an atom, and
    the second unifies with the number of characters forming that
    atom. If bound, _I_ must be a non-negative integer.


*/
static Int atom_length(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  size_t len;

  int l = push_text_stack();
  if (!Yap_IsGroundTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "at first argument");
    return false;
  } else if (!IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM, t1, "at first argument");
    return false;
  } else {
       len = Yap_AtomToUnicodeLength(t1 PASS_REGS);
       pop_text_stack(l);
    return Yap_unify(ARG2, MkIntegerTerm(len));
  }
  if (Yap_IsGroundTerm(t2)) {

    if (!IsIntegerTerm(t2)) {
      Yap_Error(TYPE_ERROR_INTEGER, t2, "atom_length/2");
      {
        pop_text_stack(l);
        return false;
      };
    } else if ((Int)(len = IntegerOfTerm(t2)) < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t2, "atom_length/2");
      {
        pop_text_stack(l);
        return false;
      };
    }
  }
restart_aux:
  len = Yap_AtomToUnicodeLength(t1 PASS_REGS);
  if (len != (size_t)-1) {
    pop_text_stack(l);
    return Yap_unify(ARG2, MkIntegerTerm(len));
  };
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError("atom_length/2")) {
    goto restart_aux;
  }
  {
    pop_text_stack(l);
    return false;
  };
}

/** @pred  atomic_length(+ _A_,? _I_) is iso


    The predicate holds when the first argument is a number or
    atom, and the second unifies with the number of characters
    needed to represent the number, or atom.


*/
static Int atomic_length(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  size_t len;

  int l = push_text_stack();
  if (!Yap_IsGroundTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "at first argument");
    {
      pop_text_stack(l);
      return false;
    };
  } else {
  len = Yap_AtomicToUnicodeLength(t1 PASS_REGS);
  if (len != (size_t)-1) {
    pop_text_stack(l);
    return Yap_unify(ARG2, MkIntegerTerm(len));
  };
  }

  if (IsNonVarTerm(t2)) {

    if (!IsIntegerTerm(t2)) {
      Yap_Error(TYPE_ERROR_INTEGER, t2, "atom_length/2");
      {
        pop_text_stack(l);
        return false;
      };
    } else if ((Int)(len = IntegerOfTerm(t2)) < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t2, "atom_length/2");
      {
        pop_text_stack(l);
        return false;
      };
    }
  }
restart_aux:
  len = Yap_AtomicToUnicodeLength(t1 PASS_REGS);
  if (len != (size_t)-1) {
    pop_text_stack(l);
    return Yap_unify(ARG2, MkIntegerTerm(len));
  };
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError("atomic_length/2")) {
    goto restart_aux;
  }
  {
    pop_text_stack(l);
    return false;
  };
}

static Int string_length(USES_REGS1) {
  Term t1;
  Term t2 = Deref(ARG2);
  size_t len;

  int l = push_text_stack();
  if (Yap_IsGroundTerm(t2)) {

    if (!IsIntegerTerm(t2)) {
      Yap_Error(TYPE_ERROR_INTEGER, t2, "string_length/2");
      {
        pop_text_stack(l);
        return false;
      };
    }
    if (FALSE && (Int)(len = IntegerOfTerm(t2)) < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t2, "string_length/2");
      {
        pop_text_stack(l);
        return false;
      };
    }
  }
restart_aux:
  t1 = Deref(ARG1);
  len = Yap_StringToUnicodeLength(t1 PASS_REGS);
  if (len != (size_t)-1) {
    pop_text_stack(l);
    return Yap_unify(ARG2, MkIntegerTerm(len));
  };
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError("string_length/2")) {
    goto restart_aux;
  }
  {
    pop_text_stack(l);
    return false;
  };
}

/** @pred downcase_text_to_atom(+Text, -Atom)
 *
 * Convert all upper case code-points in text _Text_ to
 * downcase. Unify the result as atom _Atom_ with the second
 * argument.
 *
 */
static Int downcase_text_to_atom(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);

  int l = push_text_stack();
  if (!Yap_IsGroundTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "at first argument");
    {
      pop_text_stack(l);
      return false;
    };
  }

  if (IsNonVarTerm(t2)) {
    if (!IsAtomTerm(t2)) {
      Yap_Error(TYPE_ERROR_ATOM, t2, "at second argument");
      {
        pop_text_stack(l);
        return (FALSE);
      };
    }
  }
  while (true) {
    Atom at = Yap_AtomicToLowAtom(t1);
    if (at == NULL) {
      if (LOCAL_Error_TYPE && Yap_HandleError("downcase_text_to_atom/2"))
        continue;

      pop_text_stack(l);
      return false;
    }

    pop_text_stack(l);
    return Yap_unify(MkAtomTerm(at), t2);
  }
  pop_text_stack(l);
  return false;
}

/** @pred upcase_text_to_atom(+Text, -Atom)
 *
 * Convert all lower case code-points in text _Text_ to  up
 * case. Unify the result as atom _Atom_ with the second
 * argument.
 *
 */
static Int upcase_text_to_atom(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);

  int l = push_text_stack();
  if (!Yap_IsGroundTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "at first argument");
    {
      pop_text_stack(l);
      return false;
    };
  }

  if (IsNonVarTerm(t2)) {
    if (!IsAtomTerm(t2)) {
      Yap_Error(TYPE_ERROR_ATOM, t2, "at second argument");
      {
        pop_text_stack(l);
        return (FALSE);
      };
    }
  }
  while (true) {
    Atom at = Yap_AtomicToUpAtom(t1);
    if (at == NULL) {
      if (LOCAL_Error_TYPE && Yap_HandleError("upcase_text_to_atom/2"))
        continue;
      {
        pop_text_stack(l);
        return false;
      };
    }
    pop_text_stack(l);
    return Yap_unify(MkAtomTerm(at), t2);
  }
  {
    pop_text_stack(l);
    return false;
  };
}

/** @pred downcase_text_to_string(+Text, -String)
 *
 * Convert all upper case code-points in text _Text_ to
 * downcase. Unify the result as string _String_ with the
 * second argument.
 *
 */
static Int downcase_text_to_string(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);

  if (!Yap_IsGroundTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "at first argument");
    return false;
  }

  if (IsNonVarTerm(t2)) {
    if (!IsStringTerm(t2)) {
      Yap_Error(TYPE_ERROR_STRING, t2, "at second argument");
      return (FALSE);
    }
    while (true) {
      Term t = Yap_AtomicToLowString(t1);
      if (t == TermZERO) {
        if (LOCAL_Error_TYPE && Yap_HandleError("downcase_text_to_string/2"))
          continue;
        { return false; };
      }

      return Yap_unify(t, t2);
    }
  }
  return false;
}

/** @pred upcase_text_to_string(+Text, -String)
 *
 * Convert all lower case code-points in text _Text_ to  up
 * case. Unify the result as string _String_ with the second
 * argument.
 *
 */
static Int upcase_text_to_string(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);

  if (!Yap_IsGroundTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "at first argument");
    return false;
  }

  if (IsNonVarTerm(t2)) {
    if (!IsStringTerm(t2)) {
      Yap_Error(TYPE_ERROR_STRING, t2, "at second argument");
      return (FALSE);
    }
  }
  int l = push_text_stack();
  while (true) {
    Term t = Yap_AtomicToUpString(t1);

    if (t == TermZERO) {
      if (LOCAL_Error_TYPE && Yap_HandleError("upcase_text_to_string/2"))
        continue;

      pop_text_stack(l);

      return false;
    }
    pop_text_stack(l);
    return Yap_unify(t, t2);
  }
  pop_text_stack(l);
  return false;
}

/** @pred downcase_text_to_codes(+Text, -Codes)
 *
 * Convert all upper case code-points in text _Text_ to
 * downcase. Unify the result as a sequence of codes _Codes_
 * with the second argument.
 *
 */
static Int downcase_text_to_codes(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);

  if (!Yap_IsGroundTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "at first argument");
    return false;
  }

  if (IsNonVarTerm(t2)) {
    if (!Yap_IsListTerm(t2)) {
      Yap_Error(TYPE_ERROR_LIST, t2, "at second argument");
      return false;
    }
  }
  int l = push_text_stack();
  while (true) {
    Term t = Yap_AtomicToLowListOfCodes(t1);
    if (t == TermZERO) {
      if (LOCAL_Error_TYPE && Yap_HandleError("downcase_text_to_codes/2"))
        continue;
      pop_text_stack(l);
      return false;
    }
    pop_text_stack(l);
    return Yap_unify(t, t2);
  }
  pop_text_stack(l);
  return false;
}

/** @pred upcase_text_to_codes(+Text, -Codes)
 *
 * Convert all lower case code-points in text _Text_ to  up
 * case. Unify the result as a sequence of codes _Codes_ with
 * the second argument.
 *
 */
static Int upcase_text_to_codes(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);

  if (!Yap_IsGroundTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "at first argument");
    return false;
  }

  if (IsNonVarTerm(t2)) {
    if (!Yap_IsListTerm(t2)) {
      Yap_Error(TYPE_ERROR_LIST, t2, "at second argument");
      return (FALSE);
    }
  }
  int l = push_text_stack();
  while (true) {
    Term t = Yap_AtomicToUpListOfCodes(t1);
    if (t == TermZERO) {
      if (LOCAL_Error_TYPE && Yap_HandleError("upcase_text_to_codes/2"))
        continue;
      pop_text_stack(l);
      return false;
    }
    pop_text_stack(l);
    return Yap_unify(t, t2);
  }
  pop_text_stack(l);
  return false;
}

/** @pred downcase_text_to_chars(+Text, -Chars)
 *
 * Convert all upper case code-points in text _Text_ to
 * downcase. Unify the result as a sequence of chars _Chars_
 * with the second argument.
 *
 */
static Int downcase_text_to_chars(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);

  if (!Yap_IsGroundTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "at first argument");
    return false;
  }

  if (IsNonVarTerm(t2)) {
    if (!Yap_IsListTerm(t2)) {
      Yap_Error(TYPE_ERROR_LIST, t2, "at second argument");
      return false;
    }
  }
  int l = push_text_stack();
  while (true) {
    Term t = Yap_AtomicToLowListOfAtoms(t1);

    if (t == TermZERO) {
      if (LOCAL_Error_TYPE && Yap_HandleError("downcase_text_to_to_chars/2"))
        continue;
      pop_text_stack(l);
      return false;
    }
    pop_text_stack(l);
    return Yap_unify(t, t2);
  }
  pop_text_stack(l);
  return false;
}

/** @pred upcase_text_to_chars(+Text, -Chars)
 *
 * Convert all lower case code-points in text _Text_ to  up
 * case. Unify the result as a sequence of chars _Chars_ with
 * the second argument.
 *
 */
static Int upcase_text_to_chars(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);

  if (!Yap_IsGroundTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "at first argument");
    return false;
  }

  if (IsNonVarTerm(t2)) {
    if (!Yap_IsListTerm(t2)) {
      Yap_Error(TYPE_ERROR_LIST, t2, "at second argument");
      return (FALSE);
    }
  }
  int l = push_text_stack();
  while (true) {
    Term t = Yap_AtomicToUpListOfAtoms(t1);
    if (t == TermZERO) {
      if (LOCAL_Error_TYPE && Yap_HandleError("upcase_text_to_chars/2"))
        continue;
      pop_text_stack(l);
      return false;
    }
    pop_text_stack(l);
    return Yap_unify(t, t2);
  }
  pop_text_stack(l);
  return false;
}

/* split an atom into two sub-atoms */
static Int atom_split(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  size_t u_mid;
  Term to1, to2;
  Atom at;

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "$atom_split/4");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM, t1, "$atom_split/4");
    return (FALSE);
  }
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "$atom_split/4");
    return (FALSE);
  }
  if (!IsIntTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "$atom_split/4");
    return (FALSE);
  }
  if ((Int)(u_mid = IntOfTerm(t2)) < 0) {
    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t2, "atom_split/4");
    return (FALSE);
  }
  at = AtomOfTerm(t1);
  const char *s = RepAtom(at)->StrOfAE;
  const unsigned char *s0 = RepAtom(at)->UStrOfAE;
  unsigned char *s1, *s10;
  size_t u_len = strlen_utf8(s0);
  if (u_mid > u_len) {
    return false;
  }
  size_t b_mid = skip_utf8(s0, u_mid) - s0;
  s1 = s10 = Malloc(b_mid + 1);
  memmove(s1, s, b_mid);
  s1[b_mid] = '\0';
  to1 = MkAtomTerm(Yap_ULookupAtom(s10));
  to2 = MkAtomTerm(Yap_ULookupAtom(s0 + b_mid));
  return Yap_unify_constant(ARG3, to1) && Yap_unify_constant(ARG4, to2);
}

/** @pred  atom_number(? _Atom_,? _Number_)


    The predicate holds when at least one of the arguments is
    ground (otherwise, an error message will be displayed). If the
    argument _Atom_ is an atom,  _Number_ must be the number
    corresponding to the characters in  _Atom_, otherwise the
    characters in _Atom_ must encode a number  _Number_.


*/
static Int atom_number(USES_REGS1) {
  Term t1;
  int l = push_text_stack();
restart_aux:
  t1 = Deref(ARG1);
  if (Yap_IsGroundTerm(t1)) {
    Term tf = Yap_AtomToNumber(t1 PASS_REGS);
    if (tf) {
      pop_text_stack(l);
      return Yap_unify(ARG2, tf);
    }
  } else {
    /* ARG1 unbound */
    Term t = Deref(ARG2);
    Atom af = Yap_NumberToAtom(t PASS_REGS);
    if (af) {
      pop_text_stack(l);
      return Yap_unify(ARG1, MkAtomTerm(af));
    }
  }
  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError("atom_number/2")) {
    t1 = Deref(ARG1);
    goto restart_aux;
  }
  pop_text_stack(l);
  return false;
}

/** @pred  atom_number(? _String_,? _Number_)


    The predicate holds when at least one of the arguments is
    ground (otherwise, an error message will be displayed). If the
    argument _String_ is a string term,  _String_ must be the
    number corresponding to the characters in  _Atom_, otherwise
    the characters in _String_ must encode the number  _Number_.


*/
static Int string_number(USES_REGS1) {
  Term t1;
  int l = push_text_stack();
restart_aux:
  t1 = Deref(ARG1);
  if (Yap_IsGroundTerm(t1)) {
    Term tf = Yap_StringToNumber(t1 PASS_REGS);
    if (tf) {

      pop_text_stack(l);
      return Yap_unify(ARG2, tf);
    }
  } else {
    /* ARG1 unbound */
    Term t = Deref(ARG2);
    Term tf = Yap_NumberToString(t PASS_REGS);
    if (tf) {
      pop_text_stack(l);
      return Yap_unify(ARG1, tf);
    }
  }

  /* error handling */
  if (LOCAL_Error_TYPE && Yap_HandleError("string_number/2")) {
    t1 = Deref(ARG1);
    goto restart_aux;
  }
  pop_text_stack(l);
  return false;
}

#define SUB_ATOM_HAS_MIN 1
#define SUB_ATOM_HAS_SIZE 2
#define SUB_ATOM_HAS_AFTER 4
#define SUB_ATOM_HAS_VAL 8
#define SUB_ATOM_HAS_ATOM 16
#define SUB_ATOM_HAS_UTF8 32

static Term build_new_atomic(int mask, const unsigned char *p, size_t minv,
                             size_t len USES_REGS) {
  int n;
  seq_tv_t outv[5], inp;
  size_t cuts[3];
  if (minv) {
    cuts[0] = minv;
    cuts[1] = minv + len;
    cuts[2] = 0;
    outv[0].type = 0;
    n = 1;
  } else {
    cuts[0] = minv + len;
    cuts[1] = 0;
    n = 0;
  }
  inp.type = YAP_STRING_CHARS;
  inp.enc = ENC_ISO_UTF8;
  inp.val.uc0 = p;
  outv[n + 1].type = 0;
  if (mask & SUB_ATOM_HAS_ATOM) {
    outv[n].type = YAP_STRING_ATOM;
  } else {
    outv[n].type = YAP_STRING_STRING;
  }
  int l = push_text_stack();
  bool rc = Yap_Splice_Text(2 + n, cuts, &inp, outv PASS_REGS);
  pop_text_stack(l);
  if (!rc) {
    return (false);
  }
  if (mask & SUB_ATOM_HAS_ATOM) {
    return (MkAtomTerm(outv[n].val.a));
  }
  return (outv[n].val.t);
}

static bool check_sub_string_at(int minv, const unsigned char *p1,
                                const unsigned char *p2, size_t len) {
  p1 = skip_utf8((unsigned char *)p1, minv);
  if (p1 == NULL || p2 == NULL)
    return p1 == p2;
  return cmpn_utf8(p1, p2, len) == 0;
}

static bool check_sub_string_bef(int max, const unsigned char *p1,
                                 const unsigned char *p2) {
  size_t len = strlen_utf8(p2);
  int minv = max - len;
  int c2;

  if ((Int)(minv) < 0)
    return FALSE;

  p1 = skip_utf8(p1, minv);
  if (p1 == NULL || p2 == NULL)
    return p1 == p2;
  while ((c2 = *p2++) == *p1++ && c2)
    ;
  return c2 == 0;
}

static Int cont_sub_atomic(USES_REGS1) {
  Term tat1 = Deref(ARG1);
  Term tat5 = Deref(ARG5);
  int mask;
  size_t minv, len, after, sz;
  const unsigned char *p = NULL, *p5 = NULL;

  mask = IntegerOfTerm(EXTRA_CBACK_ARG(5, 1));
  minv = IntegerOfTerm(EXTRA_CBACK_ARG(5, 2));
  len = IntegerOfTerm(EXTRA_CBACK_ARG(5, 3));
  after = IntegerOfTerm(EXTRA_CBACK_ARG(5, 4));
  sz = IntegerOfTerm(EXTRA_CBACK_ARG(5, 5));

  if (!IsVarTerm(tat1)) {
    if (IsAtomTerm(tat1)) {
      p = AtomOfTerm(tat1)->UStrOfAE;
    } else {
      p = UStringOfTerm(tat1);
    }
  }
  if (!IsVarTerm(tat5)) {
    if (IsAtomTerm(tat5)) {
      p5 = AtomOfTerm(tat5)->UStrOfAE;
    } else {
      p5 = UStringOfTerm(tat5);
    }
  }
  /* we can have one of two cases: A5 bound or unbound */
  if (mask & SUB_ATOM_HAS_VAL) {
    bool found = false;
    {
      const unsigned char *p1 = p;

      while (!found) {
        p = skip_utf8(p1, minv);
        if (cmpn_utf8(p, p5, len) == 0) {
          Yap_unify(ARG2, MkIntegerTerm(minv));
          Yap_unify(ARG3, MkIntegerTerm(len));
          Yap_unify(ARG4, MkIntegerTerm(after));
          found = true;
          /* found one, check if there is any left */
          while (minv <= sz - len) {
            int chr;
            p += get_utf8((unsigned char *)p, -1, &chr);
            after--;
            minv++;
            if (cmpn_utf8(p, p5, len) == 0)
              break;
          }
        } else {
          if (minv == sz - len)
            break;
          after--;
          minv++;
        }
      }
    }
    if (found) {
      if (minv > sz - len)
        cut_succeed();
    } else {
      cut_fail();
    }
  } else if (mask & SUB_ATOM_HAS_SIZE) {
    Term nat = build_new_atomic(mask, p, minv, len PASS_REGS);
    if (nat == 0)
      Yap_ThrowExistingError();
    Yap_unify(ARG2, MkIntegerTerm(minv));
    Yap_unify(ARG4, MkIntegerTerm(after));
    Yap_unify(ARG5, nat);
    minv++;
    if (after-- == 0) {
      cut_succeed();
    }
  } else if (mask & SUB_ATOM_HAS_MIN) {
    after = sz - (minv + len);
    Term nat = build_new_atomic(mask, p, minv, len PASS_REGS);
    if (nat == 0)
      Yap_ThrowExistingError();
    Yap_unify(ARG3, MkIntegerTerm(len));
    Yap_unify(ARG4, MkIntegerTerm(after));
    Yap_unify(ARG5, nat);
    len++;
    if (after-- == 0) {
      cut_succeed();
    }
  } else if (mask & SUB_ATOM_HAS_AFTER) {
    len = sz - (minv + after);
    Term nat = build_new_atomic(mask, p, minv, len PASS_REGS);
    if (nat == 0)
      Yap_ThrowExistingError();
    Yap_unify(ARG2, MkIntegerTerm(minv));
    Yap_unify(ARG3, MkIntegerTerm(len));
    Yap_unify(ARG5, nat);
    minv++;
    if (len-- == 0) {
      cut_succeed();
    }
  } else {
    Term nat = build_new_atomic(mask, p, minv, len PASS_REGS);
    if (nat == 0)
      Yap_ThrowExistingError();
    Yap_unify(ARG2, MkIntegerTerm(minv));
    Yap_unify(ARG3, MkIntegerTerm(len));
    Yap_unify(ARG4, MkIntegerTerm(after));
    Yap_unify(ARG5, nat);
    len++;
    if (after-- == 0) {
      if (minv == sz) {
        cut_succeed();
      }
      minv++;
      len = 0;
      after = sz - minv;
    }
  }
  EXTRA_CBACK_ARG(5, 1) = MkIntegerTerm(mask);
  EXTRA_CBACK_ARG(5, 2) = MkIntegerTerm(minv);
  EXTRA_CBACK_ARG(5, 3) = MkIntegerTerm(len);
  EXTRA_CBACK_ARG(5, 4) = MkIntegerTerm(after);
  EXTRA_CBACK_ARG(5, 5) = MkIntegerTerm(sz);

  return TRUE;
}

static Int sub_atomic(bool sub_atom, bool sub_string USES_REGS) {
  Term tat1, tbef, tsize, tafter, tout;
  int mask = SUB_ATOM_HAS_UTF8;
  size_t minv, len, after, sz;
  const unsigned char *p = NULL;
  int bnds = 0;
  Term nat = 0L;

  if (sub_atom)
    mask |= SUB_ATOM_HAS_ATOM;

  tat1 = Deref(ARG1);

  if (!IsVarTerm(tat1)) {
    if (sub_atom) {
      if (IsAtomTerm(tat1)) {
        p = AtomOfTerm(tat1)->UStrOfAE;
        sz = strlen_utf8(p);
      } else {
        Yap_Error(TYPE_ERROR_ATOM, tat1, "sub_atom/5");
        { return false; }
      }
    } else if (sub_string) {
      if (IsStringTerm(tat1)) {
        p = UStringOfTerm(tat1);
        sz = strlen_utf8(p);
      } else {
        Yap_Error(TYPE_ERROR_STRING, tat1, "sub_string/5");
        { return false; }
      }
    } else {
      int l = push_text_stack();
      if ((p = Yap_TextToUTF8Buffer(tat1 PASS_REGS))) {
        pop_text_stack(l);
        sz = strlen_utf8(p);
      } else {
        pop_text_stack(l);
        return false;
      }
    }
  } else {

    Yap_Error(INSTANTIATION_ERROR, tat1, "sub_atom/5: first variable\n");
    return false;
  }
  EXTRA_CBACK_ARG(5, 3) = MkIntegerTerm(0);
  tbef = Deref(ARG2);
  if (IsVarTerm(tbef)) {
    minv = 0;
  } else if (!IsIntegerTerm(tbef)) {
    Yap_Error(TYPE_ERROR_INTEGER, tbef, "sub_string/5");
    { return false; }
  } else {
    minv = IntegerOfTerm(tbef);
    if ((Int)minv < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, tbef, "sub_string/5");
      { return false; }
    };
    mask |= SUB_ATOM_HAS_MIN;
    bnds++;
  }
  if (IsVarTerm(tsize = Deref(ARG3))) {
    len = 0;
  } else if (!IsIntegerTerm(tsize)) {
    Yap_Error(TYPE_ERROR_INTEGER, tsize, "sub_string/5");
    { return false; }
  } else {
    len = IntegerOfTerm(tsize);
    if ((Int)len < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, tsize, "sub_string/5");
      { return false; }
    };
    mask |= SUB_ATOM_HAS_SIZE;
    bnds++;
  }
  if (IsVarTerm(tafter = Deref(ARG4))) {
    after = 0;
  } else if (!IsIntegerTerm(tafter)) {
    Yap_Error(TYPE_ERROR_INTEGER, tafter, "sub_string/5");
    { return false; }
  } else {
    after = IntegerOfTerm(tafter);
    if ((Int)after < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, tafter, "sub_string/5");
      { return false; }
    };
    mask |= SUB_ATOM_HAS_AFTER;
    bnds++;
  }
  if (!IsVarTerm(tout = Deref(ARG5))) {
    if (sub_atom) {
      if (!IsAtomTerm(tout)) {
        Yap_Error(TYPE_ERROR_ATOM, tout, "sub_atom/5");
        { return false; }
      } else {
        Atom oat;
        mask |= SUB_ATOM_HAS_VAL | SUB_ATOM_HAS_SIZE;
        oat = AtomOfTerm(tout);
        len = strlen_utf8(RepAtom(oat)->UStrOfAE);
      }
    } else {
      if (!IsStringTerm(tout)) {
        Yap_Error(TYPE_ERROR_STRING, tout, "sub_string/5");
        { return false; }
      } else {
        mask |= SUB_ATOM_HAS_VAL | SUB_ATOM_HAS_SIZE;
        len = strlen_utf8(UStringOfTerm(tout));
      }
    }
    if (!Yap_unify(ARG3, MkIntegerTerm(len))) {
      cut_fail();
    }
    bnds += 2;
  }
  /* the problem is deterministic if we have two cases */
  if (bnds > 1) {
    int out = FALSE;

    if ((mask & (SUB_ATOM_HAS_MIN | SUB_ATOM_HAS_VAL | SUB_ATOM_HAS_AFTER)) ==
        (SUB_ATOM_HAS_MIN | SUB_ATOM_HAS_VAL | SUB_ATOM_HAS_AFTER)) {
      const unsigned char *sm;
      if (sub_atom)
        sm = RepAtom(AtomOfTerm(tout))->UStrOfAE;
      else
        sm = UStringOfTerm(tout);
      if (mask & SUB_ATOM_HAS_SIZE) {
        if (len != strlen_utf8(sm)) {
          cut_fail();
        } else {
          len = strlen_utf8(sm);
        }
      }
      if (sz != minv + len + after) {
        cut_fail();
      }
      return do_cut(check_sub_string_at(minv, p, sm, len));
    } else if ((mask & (SUB_ATOM_HAS_MIN | SUB_ATOM_HAS_VAL)) ==
               (SUB_ATOM_HAS_MIN | SUB_ATOM_HAS_VAL)) {
      if (!Yap_unify(ARG4, MkIntegerTerm(sz - minv - len)))
        cut_fail();
      if (sub_atom)
        return do_cut(check_sub_string_at(
            minv, p, RepAtom(AtomOfTerm(tout))->UStrOfAE, len));
      else
        return do_cut(check_sub_string_at(minv, p, UStringOfTerm(tout), len));
    } else if ((mask & (SUB_ATOM_HAS_AFTER | SUB_ATOM_HAS_VAL)) ==
               (SUB_ATOM_HAS_AFTER | SUB_ATOM_HAS_VAL)) {
      if (!Yap_unify(ARG2, MkIntegerTerm(sz - after - len)))
        cut_fail();
      if (sub_atom) {
        return do_cut(check_sub_string_bef(
            sz - after, p, RepAtom(AtomOfTerm(tout))->UStrOfAE));
      } else {
        return do_cut(check_sub_string_bef(sz - after, p, UStringOfTerm(tout)));
      }
    } else if ((mask & (SUB_ATOM_HAS_MIN | SUB_ATOM_HAS_SIZE)) ==
               (SUB_ATOM_HAS_MIN | SUB_ATOM_HAS_SIZE)) {
      if (minv + len + after > sz) {
        cut_fail();
      }
      if ((Int)(after = (sz - (minv + len))) < 0) {
        cut_fail();
      }
      nat = build_new_atomic(mask, p, minv, len PASS_REGS);
      if (!nat) {
        cut_fail();
      }
      return do_cut(Yap_unify(ARG4, MkIntegerTerm(after)) &&
                    Yap_unify(ARG5, nat));
    } else if ((mask & (SUB_ATOM_HAS_MIN | SUB_ATOM_HAS_AFTER)) ==
               (SUB_ATOM_HAS_MIN | SUB_ATOM_HAS_AFTER)) {
      if (sz < minv + after) {
        cut_fail();
      }
      len = sz - (minv + after);
      int l = push_text_stack();
      nat = build_new_atomic(mask, p, minv, len PASS_REGS);
      pop_text_stack(l);
      if (!nat) {
        cut_fail();
      }
      return do_cut(Yap_unify(ARG3, MkIntegerTerm(len)) &&
                    Yap_unify(ARG5, nat));
    } else if ((mask & (SUB_ATOM_HAS_SIZE | SUB_ATOM_HAS_AFTER)) ==
               (SUB_ATOM_HAS_SIZE | SUB_ATOM_HAS_AFTER)) {
      if (len + after > sz) {
        cut_fail();
      }
      minv = sz - (len + after);
      int l = push_text_stack();
      nat = build_new_atomic(mask, p, minv, len PASS_REGS);
      pop_text_stack(l);
      if (!nat) {
        cut_fail();
      }
      return do_cut(Yap_unify(ARG2, MkIntegerTerm(minv)) &&
                    Yap_unify(ARG5, nat));
    } else if ((mask & (SUB_ATOM_HAS_SIZE | SUB_ATOM_HAS_VAL)) ==
               (SUB_ATOM_HAS_SIZE | SUB_ATOM_HAS_VAL)) {
      if (!sub_atom) {
        out = (strlen_utf8(UStringOfTerm(tout)) == len);
        if (!out) {
          cut_fail();
        }
      } else {
        out = (strlen(RepAtom(AtomOfTerm(tout))->StrOfAE) == len);
        if (!out) {
          cut_fail();
        }
        if (len == sz) {
          out = out && Yap_unify(ARG1, ARG5) &&
                Yap_unify(ARG2, MkIntegerTerm(0)) &&
                Yap_unify(ARG4, MkIntegerTerm(0));
        } else if (len > sz) {
          cut_fail();
        } else {
          mask |= SUB_ATOM_HAS_SIZE;
          minv = 0;
          after = sz - len;
          goto backtrackable;
        }
      }
    }
    if (out) {
      cut_succeed();
    }
    cut_fail();
  } else {
    if (!(mask & SUB_ATOM_HAS_MIN))
      minv = 0;
    if (!(mask & SUB_ATOM_HAS_SIZE))
      len = 0;
    if (!(mask & SUB_ATOM_HAS_AFTER))
      after = sz - (len + minv);
  }
backtrackable:
  EXTRA_CBACK_ARG(5, 1) = MkIntegerTerm(mask);
  EXTRA_CBACK_ARG(5, 2) = MkIntegerTerm(minv);
  EXTRA_CBACK_ARG(5, 3) = MkIntegerTerm(len);
  EXTRA_CBACK_ARG(5, 4) = MkIntegerTerm(after);
  EXTRA_CBACK_ARG(5, 5) = MkIntegerTerm(sz);
  return cont_sub_atomic(PASS_REGS1);
}

/** @pred  sub_atom(+ _A_,? _Bef_, ? _Size_, ? _After_, ?
    _At_out_) is iso


    True when  _A_ and  _At_out_ are atoms such that the name of
    _At_out_ has size  _Size_ and is a sub-string of the name of
    _A_, such that  _Bef_ is the number of characters before and
    _After_ the number of characters afterwards.

    Note that  _A_ must always be known, but  _At_out_ can be
    unbound when calling this built-in. If all the arguments for
    sub_atom/5 but  _A_ are unbound, the built-in will backtrack
    through all possible sub-strings of  _A_.

*/
static Int sub_atom(USES_REGS1) { return (sub_atomic(true, false PASS_REGS)); }

/** @pred  sub_string(+ _S_,? _Bef_, ? _Size_, ? _After_, ?
    _S_out_) is iso


    True when  _S_ and  _S_out_ are strings such that the
    _S_out_ has size  _Size_ and is a sub-string of
    _S_,   _Bef_ is the number of characters before, and
    _After_ the number of characters afterwards.

    Note that  _S_ must always be known, but  _S_out_ can be
    unbound when calling this built-in. If all the arguments for
    sub_string/5 but  _S_ are unbound, the built-in will generate
    all possible sub-strings of  _S_.

*/
static Int sub_string(USES_REGS1) { return sub_atomic(false, true PASS_REGS); }

static Int cont_current_atom(USES_REGS1) {
  Atom catom;
  Int i = IntOfTerm(EXTRA_CBACK_ARG(1, 2));
  AtomEntry *ap; /* nasty hack for gcc on hpux */

  /* protect current hash table line */
  if (IsAtomTerm(EXTRA_CBACK_ARG(1, 1)))
    catom = AtomOfTerm(EXTRA_CBACK_ARG(1, 1));
  else
    catom = NIL;
  if (catom == NIL) {
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
        EXTRA_CBACK_ARG(1, 1) = MkAtomTerm(catom);
      }
    } else {
      EXTRA_CBACK_ARG(1, 1) = MkAtomTerm(ap->NextOfAE);
      READ_UNLOCK(ap->ARWLock);
    }
    EXTRA_CBACK_ARG(1, 2) = MkIntTerm(i);
    return true;
  } else {
    return false;
  }
}

static Int current_atom(USES_REGS1) { /* current_atom(?Atom)
                                       */
  Term t1 = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    if (IsAtomTerm(t1)) {
      cut_succeed();
    } else
      cut_fail();
  }
  READ_LOCK(HashChain[0].AERWLock);
  if (HashChain[0].Entry != NIL) {
    EXTRA_CBACK_ARG(1, 1) = MkAtomTerm(HashChain[0].Entry);
  } else {
    EXTRA_CBACK_ARG(1, 1) = MkIntTerm(0);
  }
  READ_UNLOCK(HashChain[0].AERWLock);
  EXTRA_CBACK_ARG(1, 2) = MkIntTerm(0);
  return (cont_current_atom(PASS_REGS1));
}

void Yap_InitBackAtoms(void) {
  Yap_InitCPredBack("$current_atom", 1, 2, current_atom, cont_current_atom,
                    SafePredFlag | SyncPredFlag);
  Yap_InitCPredBack("atom_concat", 3, 2, atom_concat3, cont_atom_concat3, 0);
  Yap_InitCPredBack("atomic_concat", 3, 2, atomic_concat3, cont_atomic_concat3,
                    0);
  Yap_InitCPredBack("string_concat", 3, 2, string_concat3, cont_string_concat3,
                    0);
  Yap_InitCPredBack("sub_atom", 5, 5, sub_atom, cont_sub_atomic, 0);
  Yap_InitCPredBack("sub_string", 5, 5, sub_string, cont_sub_atomic, 0);
  Yap_InitCPredBack("string_code", 3, 1, string_code3, cont_string_code3, 0);
}

void Yap_InitAtomPreds(void) {
  Yap_InitCPred("name", 2, name, 0);
  Yap_InitCPred("string_to_atom", 2, string_to_atom, 0);
  Yap_InitCPred("atom_to_string", 2, atom_to_string, 0);
  Yap_InitCPred("string_to_atomic", 2, string_to_atomic, 0);
  Yap_InitCPred("atomic_to_string", 2, atomic_to_string, 0);
  Yap_InitCPred("string_to_list", 2, string_to_list, 0);
  Yap_InitCPred("char_code", 2, char_code, SafePredFlag);
  Yap_InitCPred("atom_chars", 2, atom_chars, 0);
  Yap_InitCPred("atom_codes", 2, atom_codes, 0);
  Yap_InitCPred("atom_string", 2, atom_string, 0);
  Yap_InitCPred("string_atom", 2, string_atom, 0);
  Yap_InitCPred("string_codes", 2, string_codes, 0);
  Yap_InitCPred("string_chars", 2, string_chars, 0);
  Yap_InitCPred("atom_length", 2, atom_length, SafePredFlag);
  Yap_InitCPred("atomic_length", 2, atomic_length, SafePredFlag);
  Yap_InitCPred("string_length", 2, string_length, SafePredFlag);
  Yap_InitCPred("$atom_split", 4, atom_split, SafePredFlag);
  Yap_InitCPred("number_chars", 2, number_chars, 0);
  Yap_InitCPred("number_atom", 2, number_atom, 0);
  Yap_InitCPred("number_string", 2, number_string, 0);
  Yap_InitCPred("number_codes", 2, number_codes, 0);
  Yap_InitCPred("atom_number", 2, atom_number, 0);
  Yap_InitCPred("string_number", 2, string_number, 0);
  Yap_InitCPred("$atom_concat", 2, atom_concat2, 0);
  Yap_InitCPred("$string_concat", 2, string_concat2, 0);
  Yap_InitCPred("atomic_concat", 2, atomic_concat2, 0);
  Yap_InitCPred("atomics_to_string", 2, atomics_to_string2, 0);
  Yap_InitCPred("atomics_to_string", 3, atomics_to_string3, 0);
  Yap_InitCPred("get_string_code", 3, get_string_code3, 0);

    Yap_InitCPred("downcase_atom", 2, downcase_text_to_atom, 0);
  Yap_InitCPred("upcase_atom", 2, upcase_text_to_atom, 0);

  Yap_InitCPred("downcase_text_to_atom", 2, downcase_text_to_atom, 0);
  Yap_InitCPred("upcase_text_to_atom", 2, upcase_text_to_atom, 0);
  Yap_InitCPred("downcase_text_to_string", 2, downcase_text_to_string, 0);
  Yap_InitCPred("upcase_text_to_string", 2, upcase_text_to_string, 0);
  Yap_InitCPred("downcase_text_to_codes", 2, downcase_text_to_codes, 0);
  Yap_InitCPred("upcase_text_to_codes", 2, upcase_text_to_codes, 0);
  Yap_InitCPred("downcase_text_to_chars", 2, downcase_text_to_chars, 0);
  Yap_InitCPred("upcase_text_to_chars", 2, upcase_text_to_chars, 0);

  /* hiding and unhiding some predicates */
  Yap_InitCPred("hide_atom", 1, hide_atom, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("hide", 1, hide_atom, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("unhide_atom", 1, unhide_atom, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$hidden_atom", 1, hidden_atom,
                HiddenPredFlag | SafePredFlag | SyncPredFlag);
}

/**
   @}
*/
