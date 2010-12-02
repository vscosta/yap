void Yap_swi_install(void);
void Yap_install_blobs(void);

/* Required by PL_error */
#define ERR_NO_ERROR		0
#define ERR_INSTANTIATION	1	/* void */
#define ERR_TYPE		2	/* atom_t expected, term_t value */
#define ERR_DOMAIN		3	/* atom_t domain, term_t value */
#define ERR_REPRESENTATION	4	/* atom_t what */
#define ERR_MODIFY_STATIC_PROC	5	/* predicate_t proc */
#define ERR_EVALUATION		6	/* atom_t what */
#define ERR_AR_TYPE		7	/* atom_t expected, Number value */
#define ERR_NOT_EVALUABLE	8	/* functor_t func */
#define ERR_DIV_BY_ZERO		9	/* void */
#define ERR_FAILED	       10	/* predicate_t proc */
#define ERR_FILE_OPERATION     11	/* atom_t action, atom_t type, term_t */
#define ERR_PERMISSION	       12	/* atom_t type, atom_t op, term_t obj*/
#define ERR_NOT_IMPLEMENTED 13	/* const char *what */
#define ERR_EXISTENCE	       14	/* atom_t type, term_t obj */
#define ERR_STREAM_OP	       15	/* atom_t action, term_t obj */
#define ERR_RESOURCE	       16	/* atom_t resource */
#define ERR_NOMEM	       17	/* void */
#define ERR_SYSCALL	       18	/* void */
#define ERR_SHELL_FAILED       19	/* term_t command */
#define ERR_SHELL_SIGNALLED    20	/* term_t command, int signal */
#define ERR_AR_UNDEF	       21	/* void */
#define ERR_AR_OVERFLOW	       22	/* void */
#define ERR_AR_UNDERFLOW       23	/* void */
#define ERR_UNDEFINED_PROC     24	/* Definition def */
#define ERR_SIGNALLED	       25	/* int sig, char *name */
#define ERR_CLOSED_STREAM      26	/* IOSTREAM * */
#define ERR_BUSY	       27	/* mutexes */
#define ERR_PERMISSION_PROC    28	/* op, type, Definition */
#define ERR_DDE_OP	       29	/* op, error */
#define ERR_SYNTAX	       30	/* what */
#define ERR_SHARED_OBJECT_OP   31	/* op, error */
#define ERR_TIMEOUT	       32	/* op, object */
#define ERR_NOT_IMPLEMENTED_PROC 33	/* name, arity */
#define ERR_FORMAT	       34	/* message */
#define ERR_FORMAT_ARG	       35	/* seq, term */
#define ERR_OCCURS_CHECK       36	/* Word, Word */
#define ERR_CHARS_TYPE	       37	/* char *, term */
#define ERR_MUST_BE_VAR	       38	/* int argn, term_t term */

typedef struct open_query_struct {
  int open;
  int state;
  YAP_Term g;
  yamop *p, *cp;
  Int slots;
  jmp_buf env;
  struct open_query_struct *old;
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
  atom_t ats;
  if ((ats = in_hash((ADDR)at)))
    return (atom_t)((CELL)ats*2+1);
  return (atom_t)at;
}

static inline Atom
SWIAtomToAtom(atom_t at)
{
  if ((CELL)at < N_SWI_ATOMS*(LowTagBits+1))
    return SWI_Atoms[((CELL)at)/2];
  return (Atom)at;
}

static inline Term
SWIModuleToModule(module_t m)
{
  if (m)
    return (CELL)m;
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

