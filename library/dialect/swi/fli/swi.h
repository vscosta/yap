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

  Support for file name resolution through absolute_file_name/3 and
  friends. These utility built-ins describe a list of directories that
  are used by load_files/2 to search. They include pre-compiled paths
  plus user-defined directories, directories based on environment
  variables and registry information to search for files.

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
