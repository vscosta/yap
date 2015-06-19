/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2014	 *
*									 *
*************************************************************************/

/**

    @{

    @file swi.h

  @defgroup swi-c-interface SWI-Prolog Foreign Language Interface
  @ingroup ChYInterface

 *
 *    @tableofcontents
 *
 * A reimplementation of Jan Wielemaker's SWI-Prolog C-language interface, it supports
 * most of the functionality in the original implementation. It allows for:
 *
 *   - Term Construction, Access, and Unification
 *   - Manipulation of Atoms, Strings, Lists of Codes and Lists of Atoms
 *   - Query evaluation
 *   - Thread and Prolog engine management
 *   - Data-Base Access
 *
 * In this interface, all Prolog data known by C is referenced through term references (term_t), hence
 * Prolog has all the information necessary to perform its memory management without special precautions
 *  from the C programmer.

 */

#ifndef SWI_H
#define SWI_H 1

#include "SWI-Prolog.h"

void Yap_swi_install(void);
void Yap_install_blobs(void);

#define addr_hash(V) (((CELL) (V)) >> 4 & (N_SWI_HASH-1))

static inline void
add_to_hash(Int i, ADDR key)
{

  UInt h = addr_hash(key);
  while (SWI_ReverseHash[h].key) {
    h = (h+1)%N_SWI_HASH;
  }
  SWI_ReverseHash[h].key = key;
  SWI_ReverseHash[h].pos = i;
}

static atom_t
in_hash(ADDR key)
{
  UInt h = addr_hash(key);
  while (SWI_ReverseHash[h].key) {
    if (SWI_ReverseHash[h].key == key)
      return SWI_ReverseHash[h].pos;
    h = (h+1)%N_SWI_HASH;
  }
  return 0;
}


static inline Term
SWIModuleToModule(module_t m)
{
  CACHE_REGS
  if (m)
    return MkAtomTerm(m->AtomOfME);
  if (CurrentModule)
    return CurrentModule;
  return USER_MODULE;
}


#ifdef YATOM_H

static inline atom_t
AtomToSWIAtom(Atom at)
{
  TranslationEntry *p;

  if ((p = Yap_GetTranslationProp(at)) != NULL)
    return (atom_t)(p->Translation*2+1);
  return (atom_t)at;
}

#endif

static inline Atom
SWIAtomToAtom(atom_t at)
{
  if ((CELL)at & 1)
    return SWI_Atoms[at/2];
  return (Atom)at;
}


/* This is silly, but let's keep it like that for now */
static inline Functor
SWIFunctorToFunctor(functor_t f)
{
  if (((CELL)(f) & 2) && ((CELL)f) < N_SWI_FUNCTORS*4+2)
    return SWI_Functors[((CELL)f)/4];
  return (Functor)f;
}

static inline functor_t
FunctorToSWIFunctor(Functor at)
{
  atom_t ats;
  if ((ats = in_hash((ADDR)at)))
    return (functor_t)((CELL)ats*4+2);
  return (functor_t)at;
}

#define isDefinedProcedure(pred) TRUE // TBD

int Yap_write_blob(AtomEntry *ref,  FILE *stream);
#endif

/**
  @}
*/
