
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
* File:		qly.h							 *
* comments:	quick saver/loader					 *
*									 *
* Last rev:     $Date: 2011-08-29$,$Author: vsc $			 *
* $Log: not supported by cvs2svn $					 *
*									 *
*************************************************************************/

/**
  *
  * @file qly.h
   *

 * @defgroup SaveRestoreSupport C-support for saved states.
 * @ingroup QLY
 * @{
 *
 */

#define EXPORT_ATOM_TABLE_SIZE (16 * 4096)
#define EXPORT_FUNCTOR_TABLE_SIZE (16 * 4096)
#define EXPORT_OPCODE_TABLE_SIZE (4096)
#define EXPORT_PRED_ENTRY_TABLE_SIZE (128)
#define EXPORT_DBREF_TABLE_SIZE (128)

typedef struct export_atom_hash_entry_struct {
  Atom val;
} export_atom_hash_entry_t;

typedef struct import_atom_hash_entry_struct {
  Atom oval;
  Atom val;
  struct import_atom_hash_entry_struct *next;
} import_atom_hash_entry_t;

typedef struct export_functor_hash_entry_struct {
  Functor val;
  Atom name;
  UInt arity;
} export_functor_hash_entry_t;

typedef struct import_functor_hash_entry_struct {
  Functor val;
  Functor oval;
  struct import_functor_hash_entry_struct *next;
} import_functor_hash_entry_t;

typedef struct import_opcode_hash_entry_struct {
  OPCODE val;
  int id;
  OPCODE oval;
  struct import_opcode_hash_entry_struct *next;
} import_opcode_hash_entry_t;

typedef struct export_pred_entry_hash_entry_struct {
  PredEntry *val;
  union {
    Functor f;
    Atom a;
  } u_af;
  Atom module;
  UInt arity;
} export_pred_entry_hash_entry_t;

typedef struct import_pred_entry_hash_entry_struct {
  PredEntry *val;
  PredEntry *oval;
  struct import_pred_entry_hash_entry_struct *next;
} import_pred_entry_hash_entry_t;

typedef struct export_dbref_hash_entry_struct {
  DBRef val;
  UInt sz;
  UInt refs;
} export_dbref_hash_entry_t;

typedef struct import_dbref_hash_entry_struct {
  DBRef val;
  DBRef oval;
  int count;
  struct import_dbref_hash_entry_struct *next;
} import_dbref_hash_entry_t;

typedef enum {
  QLY_START_X = 0,
  QLY_START_OPCODES = 1,
  QLY_START_ATOMS = 2,
  QLY_START_FUNCTORS = 3,
  QLY_START_PRED_ENTRIES = 4,
  QLY_START_DBREFS = 5,
  QLY_START_MODULE = 6,
  QLY_END_MODULES = 7,
  QLY_START_LU_CLAUSE = 8,
  QLY_END_LU_CLAUSES = 9,
  QLY_NEW_OP = 10,
  QLY_END_OPS = 11,
  QLY_START_PREDICATE = 12,
  QLY_END_PREDICATES = 13,
  QLY_FAILCODE = 15,
  QLY_ATOM = 16,
  QLY_ATOM_BLOB = 14
} qlf_tag_t;

#define STATIC_PRED_FLAGS						\
  (SourcePredFlag | DynamicPredFlag | LogUpdatePredFlag | CompiledPredFlag | \
   MultiFileFlag | TabledPredFlag | MegaClausePredFlag | CountPredFlag | \
   ProfiledPredFlag | ThreadLocalPredFlag | AtomDBPredFlag |		\
   ModuleTransparentPredFlag | NumberDBPredFlag | MetaPredFlag |	\
   SyncPredFlag | BackCPredFlag)
#define EXTRA_PRED_FLAGS                                                       \
  (QuasiQuotationPredFlag | NoTracePredFlag | NoSpyPredFlag)

#define SYSTEM_PRED_FLAGS                                                      \
  (BackCPredFlag | UserCPredFlag | CArgsPredFlag | AsmPredFlag | CPredFlag |   \
   BinaryPredFlag)

#define CHECK(F)                                                               \
  {                                                                            \
    size_t r = (F);                                                            \
    if (!r)                                                                    \
      return r;                                                                \
  }
#define RCHECK(F)                                                              \
  if (!(F)) {                                                                  \
    QLYR_ERROR(MISMATCH);                                                      \
    return;                                                                    \
  }

#define AllocTempSpace() (HR)
#define EnoughTempSpace(sz) ((ASP - HR) * sizeof(CELL) > sz)

/// @} @}
