/*************************************************************************
*									 *
*	 YAP Prolog   %W% %G%
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		Atoms.h.m4						 *
* Last rev:	19/2/88							 *
* mods:									 *
* comments:	atom properties header file for YAP			 *
*									 *
*************************************************************************/

#ifndef ATOMS_H
#define ATOMS_H 1

#ifndef EXTERN
#ifndef ADTDEFS_C
#define EXTERN static
#else
#define EXTERN
#endif
#endif

#include <wchar.h>

typedef struct atom_blob {
  size_t length;
  char data[MIN_ARRAY];
} atom_blob_t;

/*********  operations for atoms ****************************************/

/*    Atoms are assumed to be uniquely represented by an OFFSET and to have
    associated with them a struct of type AtomEntry
    The	two functions
        RepAtom	: Atom -> *AtomEntry
        AbsAtom	: *AtomEntry ->	Atom
    are used to encapsulate the implementation of atoms
*/

typedef struct AtomEntryStruct *Atom;
typedef struct PropEntryStruct *Prop;

/* I can only define the structure after I define the actual atoms */

/*		 atom structure						*/
typedef struct AtomEntryStruct {
  Atom NextOfAE;  /* used to build hash chains                    */
  Prop PropsOfAE; /* property list for this atom                  */
#if defined(YAPOR) || defined(THREADS)
  rwlock_t ARWLock;
#endif

  union {
    unsigned char uUStrOfAE[MIN_ARRAY]; /* representation of atom as a string */
    char uStrOfAE[MIN_ARRAY]; /* representation of atom as a string           */
    struct atom_blob blob[MIN_ARRAY];
  } rep;
} AtomEntry;

// compatible with C and C++;
typedef struct ExtraAtomEntryStruct {
  Atom NextOfAE;  /* used to build hash chains                    */
  Prop PropsOfAE; /* property list for this atom                  */
#if defined(YAPOR) || defined(THREADS)
  rwlock_t ARWLock;
#endif

  union {
    unsigned char uUStrOfAE[4]; /* representation of atom as a string */
    char uStrOfAE[4];     /* representation of atom as a string           */
    struct atom_blob blob[2];
  } rep;
} ExtraAtomEntry;

#define UStrOfAE rep.uUStrOfAE
#define StrOfAE rep.uStrOfAE

/* Props and Atoms are stored in chains, ending with a NIL */
#ifdef USE_OFFSETS
#define EndOfPAEntr(P) (Addr(P) == AtomBase)
#else
#define EndOfPAEntr(P) (Addr(P) == NIL)
#endif

/* ********************** Properties  **********************************/

#if defined(USE_OFFSETS)
#define USE_OFFSETS_IN_PROPS 1
#else
#define USE_OFFSETS_IN_PROPS 0
#endif

typedef CELL PropFlags;

/*	    basic property entry structure				*/
typedef struct PropEntryStruct {
  Prop NextOfPE;      /* used to chain properties                     */
  PropFlags KindOfPE; /* kind of property                             */
} PropEntry;

/* ************************* Functors  **********************************/

/*         Functor data type
   abstype Functor =       atom # int
   with MkFunctor(a,n) = ...
   and  NameOfFunctor(f) = ...
   and  ArityOfFunctor(f) = ...                                    */

#define MaxArity 255

typedef size_t arity_t;

#define FunctorProperty ((PropFlags)(0xbb00))

/* functor property */
typedef struct FunctorEntryStruct {
  Prop NextOfPE;      /* used to chain properties     */
  PropFlags KindOfPE; /* kind of property             */
  arity_t ArityOfFE;  /* arity of functor             */
  Atom NameOfFE;      /* back pointer to owner atom   */
  Prop PropsOfFE;     /* pointer to list of properties for this functor */
#if defined(YAPOR) || defined(THREADS)
  rwlock_t FRWLock;
#endif
} FunctorEntry;

typedef FunctorEntry *Functor;

#endif /* ATOMS_H */
