/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G%
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		index.h							 *
* Last rev:								 *
* mods:									 *
* comments:	indexation info						 *
*									 *
*************************************************************************/

/* allowed types for clauses */
typedef enum clause_type_enum {
  pair_clause = 0x01,
  struct_clause = 0x02,
  atom_clause = 0x04,
  int_clause = 0x08,
  flt_clause = 0x10,
  lgint_clause = 0x20,
  dbref_clause = 0x40
} clause_type;

/* Four types of Clauses */
#define MaxOptions 4

/* Minimum number of clauses needed to build an hash table */
/* must be a power of two */
#define MIN_HASH_ENTRIES 4

#define HASH_SHIFT 6

/* Intermediate Data structures,
   used to build the indexing code */

/* Used to store all important information about a clause */
typedef struct StructClauseDef {

  Term Tag;           /* if nonvar or nonlist, first argument */
  yamop *Code;        /* start of code for clause */
  yamop *CurrentCode; /* start of code for clause */
  union {
    yamop *WorkPC; /* start of code for clause */
    Term t_ptr;
    CELL *c_sreg;
  } ucd;
} ClauseDef;

/* Relevant information for groups */
typedef struct {
  ClauseDef *FirstClause;
  ClauseDef *LastClause;
  UInt VarClauses;
  UInt AtomClauses;
  UInt PairClauses;
  UInt StructClauses;
  UInt TestClauses;
} GroupDef;

/* switch_on_cons */
typedef struct {
  Term Tag;
  union {
    UInt Label;
    yamop *labp;
  } u_a;
} AtomSwiEntry;

/* switch_on_func */
typedef struct {
  Functor Tag;
  union {
    UInt Label;
    yamop *labp;
  } u_f;
} FuncSwiEntry;

/* switch_on_type */
typedef struct {
  UInt PairEntry;
  UInt ConstEntry;
  UInt FuncEntry;
  UInt VarEntry;
} TypeSwitch;

#define MAX_REG_COPIES 32

typedef struct {
  Int pos;
  Term val;
  Term extra;
} istack_entry;

typedef enum { pc_entry, block_entry } add2index_entries;

typedef struct {
  add2index_entries flag;
  union {
    struct {
      yamop **pi_pc;
      yamop *code, *current_code, *work_pc;
      Term tag;
    } pce;
    struct {
      ClauseUnion *block;
      yamop **entry_code;
    } cle;
  } uip;
} path_stack_entry;

#define MAX_ISTACK_DEPTH 32

typedef enum { REFRESH, RECORDA, RECORDZ } expand_values;
