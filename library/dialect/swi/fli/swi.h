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

  @{
  
 */
void Yap_swi_install(void);
void Yap_install_blobs(void);

typedef struct open_query_struct {
  int q_open;
  int q_state;
  YAP_Term *q_g;
  PredEntry *q_pe;
  yamop *q_p, *q_cp;
  jmp_buf q_env;
  int q_flags;
  YAP_dogoalinfo q_h;
  struct open_query_struct *oq;
} open_query;

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

static inline functor_t
FunctorToSWIFunctor(Functor at)
{
  atom_t ats;
  if ((ats = in_hash((ADDR)at)))
    return (functor_t)((CELL)ats*4+2);
  return (functor_t)at;
}

#define isDefinedProcedure(pred) TRUE // TBD

/**
  @}
*/
