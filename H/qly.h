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

#define EXPORT_ATOM_TABLE_SIZE (16*4096)
#define EXPORT_FUNCTOR_TABLE_SIZE (16*4096)
#define EXPORT_OPCODE_TABLE_SIZE (4096)
#define EXPORT_PRED_ENTRY_TABLE_SIZE (128)

typedef struct export_atom_hash_entry_struct {
  Atom val;
  struct  export_atom_hash_entry_struct *next;
} export_atom_hash_entry_t;

typedef struct import_atom_hash_entry_struct {
  Atom oval;
  Atom val;
  struct  import_atom_hash_entry_struct *next;
} import_atom_hash_entry_t;

typedef struct export_functor_hash_entry_struct {
  Functor val;
  Atom name;
  UInt arity;
  struct  export_functor_hash_entry_struct *next;
} export_functor_hash_entry_t;

typedef struct import_functor_hash_entry_struct {
  Functor val;
  Functor oval;
  struct  import_functor_hash_entry_struct *next;
} import_functor_hash_entry_t;

typedef struct import_opcode_hash_entry_struct {
  OPCODE val;
  int id;
  OPCODE oval;
  struct  import_opcode_hash_entry_struct *next;
} import_opcode_hash_entry_t;

typedef struct export_pred_entry_hash_entry_struct {
  PredEntry *val;
  union {
    Functor f;
    Atom a;
  } u;
  Atom module;
  UInt arity;
  struct  export_pred_entry_hash_entry_struct *next;
} export_pred_entry_hash_entry_t;

typedef struct import_pred_entry_hash_entry_struct {
  PredEntry *val;
  PredEntry *oval;
  struct  import_pred_entry_hash_entry_struct *next;
} import_pred_entry_hash_entry_t;

typedef enum {
  QLY_START_X,
  QLY_START_OPCODES,
  QLY_START_ATOMS,
  QLY_START_FUNCTORS,
  QLY_START_PRED_ENTRIES,
  QLY_START_MODULE,
  QLY_START_PREDICATE,
  QLY_END_PREDICATES,
  QLY_ATOM_WIDE,
  QLY_ATOM
} qlf_tag_t;

#define NEXTOP(V,TYPE)    ((yamop *)(&((V)->u.TYPE.next)))

#define CHECK(F) { size_t r = (F); if (!r) return r; }
#define RCHECK(F)  if(!(F)) { ERROR(MISMATCH); return; }

#define AllocTempSpace() (H)
#define EnoughTempSpace(sz) ((ASP-H)*sizeof(CELL) > sz)
#define ERROR(E) 


