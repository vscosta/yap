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
  @ingroup fli_c_cxx

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
  return (atom_t)at;
}

#endif

static inline Atom
SWIAtomToAtom(atom_t at)
{
  return (Atom)at;
}

static inline functor_t
FunctorToSWIFunctor(Functor f)
{
  return (functor_t)f;
}

static inline Functor
SWIFunctorToFunctor(functor_t f)
{
  return (Functor)f;
}

#define isDefinedProcedure(pred) TRUE // TBD

int Yap_write_blob(AtomEntry *ref,  FILE *stream);
#endif

/**
  @}
*/
