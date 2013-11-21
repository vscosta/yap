void Yap_swi_install(void);
void Yap_install_blobs(void);

typedef struct open_query_struct {
  int open;
  int state;
  YAP_Term *g;
  PredEntry *pe;
  yamop *p, *cp;
  jmp_buf env;
  int flags;
  YAP_dogoalinfo h;
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


static inline atom_t
AtomToSWIAtom(Atom at)
{
  TranslationEntry *p;

  if ((p = Yap_GetTranslationProp(at)) != NULL)
    return (atom_t)(p->Translation*2+1);
  return (atom_t)at;
}

static inline Atom
SWIAtomToAtom(atom_t at)
{
  if ((CELL)at & 1)
    return SWI_Atoms[at/2];
  return (Atom)at;
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

/* This is silly, but let's keep it like that for now */
static inline Functor
SWIFunctorToFunctor(functor_t f)
{
  if ((CELL)(f) & 2 && ((CELL)f) < N_SWI_FUNCTORS*4+2)
    return SWI_Functors[((CELL)f)/4];
  return (Functor)f;
}

#define isDefinedProcedure(pred) TRUE // TBD
