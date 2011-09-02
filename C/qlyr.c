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
* File:		qlyr.c							 *
* comments:	quick saver/loader					 *
*									 *
* Last rev:     $Date: 2011-08-29$,$Author: vsc $			 *
* $Log: not supported by cvs2svn $					 *
*									 *
*************************************************************************/

#include <SWI-Stream.h>
#include "absmi.h"
#include "Foreign.h"
#include "alloc.h"
#include "yapio.h"
#include "iopreds.h"
#include "attvar.h"
#if HAVE_STRING_H
#include <string.h>
#endif

#include "qly.h"

STATIC_PROTO(void  RestoreEntries, (PropEntry *, int USES_REGS));
STATIC_PROTO(void  CleanCode, (PredEntry * USES_REGS));

typedef enum {
  OUT_OF_TEMP_SPACE = 0,
  OUT_OF_ATOM_SPACE = 1,
  OUT_OF_CODE_SPACE = 2,
  UNKNOWN_ATOM = 3,
  UNKNOWN_FUNCTOR = 4,
  UNKNOWN_PRED_ENTRY = 5,
  UNKNOWN_OPCODE = 6,
  UNKNOWN_DBREF = 7,
  BAD_ATOM = 8,
  MISMATCH = 9,
  INCONSISTENT_CPRED = 10,
  BAD_READ = 11
} qlfr_err_t;

static char *
Yap_AlwaysAllocCodeSpace(UInt size)
{
  char *out;
  while (!(out = Yap_AllocCodeSpace(size))) {
    if (!Yap_growheap(FALSE, size, NULL)) {
      return NULL;
    }
  }
  return out;
}

static void
ERROR(qlfr_err_t my_err)
{
  fprintf(stderr,"Error %d\n", my_err);
  exit(1);
}

static Atom
LookupAtom(Atom oat)
{
  CELL hash = (CELL)(oat) % LOCAL_ImportAtomHashTableSize;
 import_atom_hash_entry_t *a;

  a = LOCAL_ImportAtomHashChain[hash];
  while (a) {
    if (a->oval == oat) {
      return a->val;
    }
    a = a->next;
  }
  ERROR(UNKNOWN_ATOM);
  return NIL;
}

static void
InsertAtom(Atom oat, Atom at)
{
  CELL hash = (CELL)(oat) % LOCAL_ImportAtomHashTableSize;
  import_atom_hash_entry_t *a;

  a = LOCAL_ImportAtomHashChain[hash];
  while (a) {
    if (a->oval == oat) {
      return;
    }
    a = a->next;
  }
  a = (import_atom_hash_entry_t *)malloc(sizeof(import_atom_hash_entry_t));
  if (!a) {
    return;
  }
  a->val = at;
  a->oval = oat;
  a->next = LOCAL_ImportAtomHashChain[hash];
  LOCAL_ImportAtomHashChain[hash] = a;
}

static Functor
LookupFunctor(Functor ofun)
{
  CELL hash = (CELL)(ofun) % LOCAL_ImportFunctorHashTableSize;
  import_functor_hash_entry_t *f;

  f = LOCAL_ImportFunctorHashChain[hash];
  while (f) {
    if (f->oval == ofun) {
      return f->val;
    }
    f = f->next;
  }
  ERROR(UNKNOWN_FUNCTOR);
  return NIL;
}

static void
InsertFunctor(Functor ofun, Functor fun)
{
  CELL hash = (CELL)(ofun) % LOCAL_ImportFunctorHashTableSize;
  import_functor_hash_entry_t *f;

  f = LOCAL_ImportFunctorHashChain[hash];
  while (f) {
    if (f->oval == ofun) {
      return;
    }
    f = f->next;
  }
  f = (import_functor_hash_entry_t *)malloc(sizeof(import_functor_hash_entry_t));
  if (!f) {
    return;
  }
  f->val = fun;
  f->oval = ofun;
  f->next = LOCAL_ImportFunctorHashChain[hash];
  LOCAL_ImportFunctorHashChain[hash] = f;
}

static PredEntry *
LookupPredEntry(PredEntry *op)
{
  CELL hash = (CELL)(op) % LOCAL_ImportPredEntryHashTableSize;
  import_pred_entry_hash_entry_t *p;

  p = LOCAL_ImportPredEntryHashChain[hash];
  while (p) {
    if (p->oval == op) {
      return p->val;
    }
    p = p->next;
  }
  ERROR(UNKNOWN_PRED_ENTRY);
  return NIL;
}

static void
InsertPredEntry(PredEntry *op, PredEntry *pe)
{
  CELL hash = (CELL)(op) % LOCAL_ImportPredEntryHashTableSize;
  import_pred_entry_hash_entry_t *p;

  p = LOCAL_ImportPredEntryHashChain[hash];
  while (p) {
    if (p->oval == op) {
      return;
    }
    p = p->next;
  }
  p = (import_pred_entry_hash_entry_t *)malloc(sizeof(import_pred_entry_hash_entry_t));
  if (!p) {
    return;
  }
  p->val = pe;
  p->oval = op;
  p->next = LOCAL_ImportPredEntryHashChain[hash];
  LOCAL_ImportPredEntryHashChain[hash] = p;
}

static OPCODE
LookupOPCODE(OPCODE op)
{
  CELL hash = (CELL)(op) % LOCAL_ImportOPCODEHashTableSize;
  import_opcode_hash_entry_t *f;

  f = LOCAL_ImportOPCODEHashChain[hash];
  while (f) {
    if (f->oval == op) {
      return f->val;
    }
    f = f->next;
  }
  ERROR(UNKNOWN_OPCODE);
  return NIL;
}

static int
OpcodeID(OPCODE op)
{
  CELL hash = (CELL)(op) % LOCAL_ImportOPCODEHashTableSize;
  import_opcode_hash_entry_t *f;

  f = LOCAL_ImportOPCODEHashChain[hash];
  while (f) {
    if (f->oval == op) {
      return f->id;
    }
    f = f->next;
  }
  ERROR(UNKNOWN_OPCODE);
  return NIL;
}

static void
InsertOPCODE(OPCODE op0, int i, OPCODE op)
{
  CELL hash = (CELL)(op0) % LOCAL_ImportOPCODEHashTableSize;
  import_opcode_hash_entry_t *f;
  f = LOCAL_ImportOPCODEHashChain[hash];
  while (f) {
    if (f->oval == op0) {
      return;
    }
    f = f->next;
  }
  f = (import_opcode_hash_entry_t *)malloc(sizeof(import_opcode_hash_entry_t));
  if (!f) {
    return;
  }
  f->val = op;
  f->oval = op0;
  f->id = i;
  f->next = LOCAL_ImportOPCODEHashChain[hash];
  LOCAL_ImportOPCODEHashChain[hash] = f;
}

static DBRef
LookupDBRef(DBRef dbr)
{
  CELL hash = (CELL)(dbr) % LOCAL_ImportDBRefHashTableSize;
  import_dbref_hash_entry_t *p;

  p = LOCAL_ImportDBRefHashChain[hash];
  while (p) {
    if (p->oval == dbr) {
      p->count++;
      return p->val;
    }
    p = p->next;
  }
  ERROR(UNKNOWN_DBREF);
  return NIL;
}

static LogUpdClause *
LookupMayFailDBRef(DBRef dbr)
{
  CELL hash = (CELL)(dbr) % LOCAL_ImportDBRefHashTableSize;
  import_dbref_hash_entry_t *p;

  p = LOCAL_ImportDBRefHashChain[hash];
  while (p) {
    if (p->oval == dbr) {
      p->count++;
      return (LogUpdClause *)p->val;
    }
    p = p->next;
  }
  return NULL;
}

static void
InsertDBRef(DBRef dbr0, DBRef dbr)
{
  CELL hash = (CELL)(dbr0) % LOCAL_ImportDBRefHashTableSize;
  import_dbref_hash_entry_t *p;

  p = LOCAL_ImportDBRefHashChain[hash];
  while (p) {
    if (p->oval == dbr0) {
      return;
    }
    p = p->next;
  }
  p = (import_dbref_hash_entry_t *)malloc(sizeof(import_dbref_hash_entry_t));
  if (!p) {
    return;
  }
  p->val = dbr;
  p->oval = dbr0;
  p->count = 0;
  p->next = LOCAL_ImportDBRefHashChain[hash];
  LOCAL_ImportDBRefHashChain[hash] = p;
}

static void
InitHash(void)
{
  LOCAL_ImportFunctorHashTableSize = EXPORT_FUNCTOR_TABLE_SIZE;
  LOCAL_ImportFunctorHashChain = (import_functor_hash_entry_t **)calloc(1, sizeof(import_functor_hash_entry_t *)* LOCAL_ImportFunctorHashTableSize);
  LOCAL_ImportAtomHashTableSize = EXPORT_ATOM_TABLE_SIZE;
  LOCAL_ImportAtomHashChain = (import_atom_hash_entry_t **)calloc(1, sizeof(import_atom_hash_entry_t *)* LOCAL_ImportAtomHashTableSize);
  LOCAL_ImportOPCODEHashTableSize = EXPORT_OPCODE_TABLE_SIZE;
  LOCAL_ImportOPCODEHashChain = (import_opcode_hash_entry_t **)calloc(1, sizeof(import_opcode_hash_entry_t *)* LOCAL_ImportOPCODEHashTableSize);
  LOCAL_ImportPredEntryHashTableSize = EXPORT_PRED_ENTRY_TABLE_SIZE;
  LOCAL_ImportPredEntryHashChain = (import_pred_entry_hash_entry_t **)calloc(1, sizeof(import_pred_entry_hash_entry_t *)* LOCAL_ImportPredEntryHashTableSize);
  LOCAL_ImportDBRefHashTableSize = EXPORT_DBREF_TABLE_SIZE;
  LOCAL_ImportDBRefHashChain = (import_dbref_hash_entry_t **)calloc(1, sizeof(import_dbref_hash_entry_t *)* LOCAL_ImportDBRefHashTableSize);
}

static void
CloseHash(void)
{
  UInt i;
  for (i=0; i < LOCAL_ImportFunctorHashTableSize; i++) {
    import_functor_hash_entry_t *a = LOCAL_ImportFunctorHashChain[i];
    while (a) {
      import_functor_hash_entry_t *a0 = a;
      a = a->next;
      free(a0);
    }
  }
  LOCAL_ImportFunctorHashTableSize = 0;
  free(LOCAL_ImportFunctorHashChain);
  LOCAL_ImportFunctorHashChain = NULL;
  for (i=0; i < LOCAL_ImportAtomHashTableSize; i++) {
    import_atom_hash_entry_t *a = LOCAL_ImportAtomHashChain[i];
    while (a) {
      import_atom_hash_entry_t *a0 = a;
      a = a->next;
      free(a0);
    }
  }
  LOCAL_ImportAtomHashTableSize = 0;
  free(LOCAL_ImportAtomHashChain);
  LOCAL_ImportAtomHashChain = NULL;
  for (i=0; i < LOCAL_ImportOPCODEHashTableSize; i++) {
    import_opcode_hash_entry_t *a = LOCAL_ImportOPCODEHashChain[i];
    while (a) {
      import_opcode_hash_entry_t *a0 = a;
      a = a->next;
      free(a0);
    }
  }
  LOCAL_ImportOPCODEHashTableSize = 0;
  free(LOCAL_ImportOPCODEHashChain);
  LOCAL_ImportOPCODEHashChain = NULL;
  for (i=0; i < LOCAL_ImportPredEntryHashTableSize; i++) {
    import_pred_entry_hash_entry_t *a = LOCAL_ImportPredEntryHashChain[i];
    while (a) {
      import_pred_entry_hash_entry_t *a0 = a;
      a = a->next;
      free(a0);
    }
  }
  LOCAL_ImportPredEntryHashTableSize = 0;
  free(LOCAL_ImportPredEntryHashChain);
  LOCAL_ImportPredEntryHashChain = NULL;
  for (i=0; i < LOCAL_ImportDBRefHashTableSize; i++) {
    import_dbref_hash_entry_t *a = LOCAL_ImportDBRefHashChain[i];
    while (a) {
      import_dbref_hash_entry_t *a0 = a;
#ifdef DEBUG
      if (!a->count) {
	fprintf(stderr,"WARNING: unused reference %p\n",a);
      }
#endif
      a = a->next;
      free(a0);
    }
  }
  LOCAL_ImportDBRefHashTableSize = 0;
  free(LOCAL_ImportDBRefHashChain);
  LOCAL_ImportDBRefHashChain = NULL;
}

static inline Atom
AtomAdjust(Atom a)
{
  return LookupAtom(a);
}

static inline Functor
FuncAdjust(Functor f)
{
  return LookupFunctor(f);
  return f;
}


static inline Term
AtomTermAdjust(Term t)
{
  return MkAtomTerm(LookupAtom(AtomOfTerm(t)));
}

static inline Term
TermToGlobalOrAtomAdjust(Term t)
{
  if (t && IsAtomTerm(t))
    return AtomTermAdjust(t);
  return t;
}

#define IsOldCode(P) FALSE
#define IsOldCodeCellPtr(P) FALSE
#define IsOldDelay(P) FALSE
#define IsOldDelayPtr(P) FALSE
#define IsOldLocalInTR(P) FALSE
#define IsOldLocalInTRPtr(P) FALSE
#define IsOldGlobal(P) FALSE
#define IsOldGlobalPtr(P) FALSE
#define IsOldTrail(P) FALSE
#define IsOldTrailPtr(P) FALSE

#define CharP(X) ((char *)(X))

#define REINIT_LOCK(P) 
#define REINIT_RWLOCK(P) 
#define BlobTypeAdjust(P) (P)
#define NoAGCAtomAdjust(P) (P)
#define OrArgAdjust(P) 
#define TabEntryAdjust(P) 
#define IntegerAdjust(D)  (D)
#define AddrAdjust(P) (P)
#define MFileAdjust(P) (P)

#define CodeVarAdjust(P) CodeVarAdjust__(P PASS_REGS)
static inline Term
CodeVarAdjust__ (Term var USES_REGS)
{
  if (var == 0L)
    return var;
  return (Term)(CharP(var) + LOCAL_HDiff);
}

#define ConstantAdjust(P) (P)
#define ArityAdjust(P) (P)
#define DoubleInCodeAdjust(P) 
#define IntegerInCodeAdjust(Pxb) 

static inline PredEntry *
PtoPredAdjust(PredEntry *p)
{
  return LookupPredEntry(p);
}

static inline PredEntry *
PredEntryAdjust(PredEntry *p)
{
  return LookupPredEntry(p);
}

static inline OPCODE
OpcodeAdjust(OPCODE OP) {
  return LookupOPCODE(OP);
}

static inline Term
ModuleAdjust(Term M) {
  if (!M)
    return M;
  return AtomTermAdjust(M);
}

#define ExternalFunctionAdjust(P) (P)
#define DBRecordAdjust(P) (P)
#define ModEntryPtrAdjust(P) (P)
#define AtomEntryAdjust(P) (P)
#define GlobalEntryAdjust(P) (P)
#define BlobTermInCodeAdjust(P) BlobTermInCodeAdjust__(P PASS_REGS)
#if TAGS_FAST_OPS
static inline Term
BlobTermInCodeAdjust__ (Term t USES_REGS)
{
  return (Term) ((char *)(t) - LOCAL_HDiff);
}
#else
static inline Term
BlobTermInCodeAdjust__ (Term t USES_REGS)
{
  return (Term) ((char *)(t) + LOCAL_HDiff);
}
#endif
#define DBTermAdjust(P) DBTermAdjust__(P PASS_REGS)
static inline DBTerm *
DBTermAdjust__ (DBTerm * dbtp USES_REGS)
{
  return (DBTerm *) (CharP (dbtp) + LOCAL_HDiff);
}

#define CellPtoHeapAdjust(P) CellPtoHeapAdjust__(P PASS_REGS)
static inline CELL *
CellPtoHeapAdjust__ (CELL * dbtp USES_REGS)
{
  return (CELL *) (CharP (dbtp) + LOCAL_HDiff);
}

#define PtoAtomHashEntryAdjust(P) (P)
#define CellPtoHeapCellAdjust(P) (P)
#define CellPtoTRAdjust(P) (P)
#define CodeAddrAdjust(P) (P)
#define ConsultObjAdjust(P) (P)
#define DelayAddrAdjust(P) (P)
#define DelayAdjust(P) (P)
#define GlobalAdjust(P) (P)

#define DBRefAdjust(P) DBRefAdjust__(P PASS_REGS)
static inline DBRef
DBRefAdjust__ (DBRef dbtp USES_REGS)
{
  return LookupDBRef(dbtp);
}

#define DBRefPAdjust(P) DBRefPAdjust__(P PASS_REGS)
static inline DBRef *
DBRefPAdjust__ (DBRef * dbtp USES_REGS)
{
  return (DBRef *) ((char *)(dbtp) + LOCAL_HDiff);
}

#define LUIndexAdjust(P) (P)
#define SIndexAdjust(P) (P)
#define LocalAddrAdjust(P) (P)
#define GlobalAddrAdjust(P) (P)
#define OpListAdjust(P) (P)

#define PtoLUCAdjust(P) PtoLUCAdjust__(P PASS_REGS)
#define PtoLUClauseAdjust(P) PtoLUCAdjust__(P PASS_REGS)
static inline LogUpdClause *
PtoLUCAdjust__ (LogUpdClause * dbtp USES_REGS)
{
  return (LogUpdClause *) ((char *)(dbtp) + LOCAL_HDiff);
}

#define PtoStCAdjust(P) (P)
#define PtoArrayEAdjust(P) (P)
#define PtoArraySAdjust(P) (P)
#define PtoGlobalEAdjust(P) (P)
#define PtoDelayAdjust(P) (P)
#define PtoGloAdjust(P) (P)
#define PtoLocAdjust(P) (P)

#define PtoHeapCellAdjust(P) PtoHeapCellAdjust__(P PASS_REGS)
static inline CELL *
PtoHeapCellAdjust__ (CELL * ptr USES_REGS)
{
  LogUpdClause *out;
  if ((out = LookupMayFailDBRef((DBRef)ptr)))
    return (CELL *)out;
  return (CELL *) (CharP (ptr) + LOCAL_HDiff);
}

#define TermToGlobalAdjust(P) (P)
#define PtoOpAdjust(P) PtoOpAdjust__(P PASS_REGS)
static inline yamop *PtoOpAdjust__(yamop *ptr USES_REGS) {
  if (ptr) {
    if (ptr == LOCAL_ImportFAILCODE)
      return FAILCODE;
    return (yamop *) ((char *) (ptr) + LOCAL_HDiff);
  }
  return ptr;
}
#define PtoLUIndexAdjust(P) (P)
#define PtoDBTLAdjust(P) (P)
#define PtoPtoPredAdjust(P) (P)
#define OpRTableAdjust(P) (P)
#define OpEntryAdjust(P) (P)
#define PropAdjust(P) (P)
#define TrailAddrAdjust(P) (P)
#if PRECOMPUTE_REGADDRESS
#define XAdjust(P) XAdjust__(P PASS_REGS)
static inline wamreg
XAdjust__ (wamreg reg USES_REGS)
{
  return (wamreg) ((wamreg) ((reg) + LOCAL_XDiff));
}
#else
#define XAdjust(X) (X)
#endif
#define YAdjust(X) (X)
#define HoldEntryAdjust(P) (P)
#define CodeCharPAdjust(P) (P)
#define CodeVoidPAdjust(P) (P)
#define HaltHookAdjust(P) (P)

#define recompute_mask(dbr)

#define rehash(oldcode, NOfE, KindOfEntries)

#define RestoreSWIHash()

#define Yap_op_from_opcode(OP) OpcodeID(OP)

#include "rheap.h"

static void
RestoreHashPreds( USES_REGS1 )
{
}


static void
RestoreAtomList(Atom atm USES_REGS)
{
}

static size_t
read_bytes(IOSTREAM *stream, void *ptr, size_t sz)
{
  return Sfread(ptr, sz, 1, stream);
}

static unsigned char
read_byte(IOSTREAM *stream)
{
  return  Sgetc(stream);
}

static BITS16
read_bits16(IOSTREAM *stream)
{
  BITS16 v;
  read_bytes(stream, &v, sizeof(BITS16));
  return v;
}

static UInt
read_uint(IOSTREAM *stream)
{
  UInt v;
  read_bytes(stream, &v, sizeof(UInt));
  return v;
}

static int
read_int(IOSTREAM *stream)
{
  int v;
  read_bytes(stream, &v, sizeof(int));
  return v;
}

static qlf_tag_t
read_tag(IOSTREAM *stream)
{
  int ch = read_byte(stream);
  return ch;
}

static void
read_header(IOSTREAM *stream)
{
  int ch;
  while ((ch = read_byte(stream)));
}

static void
ReadHash(IOSTREAM *stream)
{
  UInt i;
  RCHECK(read_tag(stream) == QLY_START_X);
  LOCAL_XDiff = (char *)(&ARG1) - (char *)read_uint(stream);
  RCHECK(read_tag(stream) == QLY_START_OPCODES);
  RCHECK(read_int(stream) == _std_top);
  for (i= 0; i <= _std_top; i++) {
    InsertOPCODE((OPCODE)read_uint(stream), i, Yap_opcode(i));
  }
  RCHECK(read_tag(stream) == QLY_START_ATOMS);
  LOCAL_ImportAtomHashTableNum = read_uint(stream);
  for (i = 0; i < LOCAL_ImportAtomHashTableNum; i++) {
    Atom oat = (Atom)read_uint(stream);
    Atom at;
    qlf_tag_t tg = read_tag(stream);
      
    if (tg == QLY_ATOM_WIDE) {
      wchar_t *rep = (wchar_t *)AllocTempSpace();
      UInt len;

      len = read_uint(stream);
      if (!EnoughTempSpace(len)) ERROR(OUT_OF_TEMP_SPACE);
      read_bytes(stream, rep, (len+1)*sizeof(wchar_t));
      while (!(at = Yap_LookupWideAtom(rep))) {
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  exit(1);
	}
      }
      if (at == NIL) ERROR(OUT_OF_ATOM_SPACE);
    } else if (tg == QLY_ATOM) {
      char *rep = (char *)AllocTempSpace();
      UInt len;

      len = read_uint(stream);
      if (!EnoughTempSpace(len)) ERROR(OUT_OF_TEMP_SPACE);
      read_bytes(stream, rep, (len+1)*sizeof(char));
      while (!(at = Yap_FullLookupAtom(rep))) {
	if (!Yap_growheap(FALSE, 0, NULL)) {
	  exit(1);
	}
      }
      if (at == NIL) ERROR(OUT_OF_ATOM_SPACE);
    } else {
      ERROR(BAD_ATOM);
      return;
    }
    InsertAtom(oat, at);
  }
  /* functors */
  RCHECK(read_tag(stream) == QLY_START_FUNCTORS);
  LOCAL_ImportFunctorHashTableNum = read_uint(stream);
  for (i = 0; i < LOCAL_ImportFunctorHashTableNum; i++) {
    Functor of = (Functor)read_uint(stream);
    UInt arity = read_uint(stream);
    Atom oat = (Atom)read_uint(stream);
    Atom at = AtomAdjust(oat);
    Functor f;
    while (!(f = Yap_MkFunctor(at, arity))) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
	exit(1);
      }
    }
    InsertFunctor(of, f);
  }
  RCHECK(read_tag(stream) == QLY_START_PRED_ENTRIES);
  LOCAL_ImportPredEntryHashTableNum = read_uint(stream);
  for (i = 0; i < LOCAL_ImportPredEntryHashTableNum; i++) {
    PredEntry *ope = (PredEntry *)read_uint(stream), *pe;
    UInt arity = read_uint(stream);
    Atom omod = (Atom)read_uint(stream);
    Term mod = MkAtomTerm(AtomAdjust(omod));
    if (mod != IDB_MODULE) {
      if (arity) {
	Functor of = (Functor)read_uint(stream);
	Functor f = LookupFunctor(of);
	while(!(pe = RepPredProp(PredPropByFunc(f,mod)))) {
	  if (!Yap_growheap(FALSE, 0, NULL)) {
	    exit(1);
	  }
	}
      } else {
	Atom oa = (Atom)read_uint(stream);
	Atom a = LookupAtom(oa);
	pe = RepPredProp(PredPropByAtom(a,mod));
      }
    } else {
      if (arity == (UInt)-1) {
	UInt i = read_uint(stream);
	pe = Yap_FindLUIntKey(i);
      }	else if (arity == (UInt)(-2)) {
	Atom oa = (Atom)read_uint(stream);
	Atom a = LookupAtom(oa);
	pe = RepPredProp(PredPropByAtom(a,mod));
      } else {
	Functor of = (Functor)read_uint(stream);
	Functor f = LookupFunctor(of);
	pe = RepPredProp(PredPropByFunc(f,mod));
      }
    }
    InsertPredEntry(ope, pe);
  }
  RCHECK(read_tag(stream) == QLY_START_DBREFS);
  LOCAL_ImportDBRefHashTableNum = read_uint(stream);
  for (i = 0; i < LOCAL_ImportDBRefHashTableNum; i++) {
    LogUpdClause *ocl = (LogUpdClause *)read_uint(stream);
    UInt sz = read_uint(stream);
    UInt nrefs = read_uint(stream);
    LogUpdClause *ncl = (LogUpdClause *)Yap_AlwaysAllocCodeSpace(sz);
    ncl->Id = FunctorDBRef;
    ncl->ClRefCount = nrefs;
    InsertDBRef((DBRef)ocl,(DBRef)ncl);
  }
  RCHECK(read_tag(stream) == QLY_FAILCODE);
  LOCAL_ImportFAILCODE = (yamop *)read_uint(stream);
}

static void
read_clauses(IOSTREAM *stream, PredEntry *pp, UInt nclauses, UInt flags) {
  if (pp->PredFlags & LogUpdatePredFlag) {

    pp->TimeStampOfPred = 0L; 
    /* first, clean up whatever was there */
    if (pp->cs.p_code.NOfClauses) {
      LogUpdClause *cl;
      cl = ClauseCodeToLogUpdClause(pp->cs.p_code.FirstClause);
      do {
	LogUpdClause *ncl = cl->ClNext;
	Yap_ErLogUpdCl(cl);
	cl = ncl;
      } while (cl != NULL);
    }
    if (!nclauses) {
      return;
    }
    while ((read_tag(stream) == QLY_START_LU_CLAUSE)) {
      char *base = (void *)read_uint(stream);
      UInt size = read_uint(stream);
      LogUpdClause *cl;
      Int nrefs = 0;

      if ((cl = LookupMayFailDBRef((DBRef)base))) {
	nrefs = cl->ClRefCount;
      } else {
	cl = (LogUpdClause *)Yap_AlwaysAllocCodeSpace(size);
      }
      read_bytes(stream, cl, size);
      cl->ClFlags &= ~InUseMask;
      cl->ClRefCount = nrefs;
      LOCAL_HDiff = (char *)cl-base;
      RestoreLUClause(cl, pp);
      Yap_AssertzClause(pp, cl->ClCode);
    }
  } else if (pp->PredFlags & MegaClausePredFlag) {
    CACHE_REGS
    char *base = (void *)read_uint(stream);
    UInt size = read_uint(stream);
    MegaClause *cl = (MegaClause *)Yap_AlwaysAllocCodeSpace(size);

    if (nclauses) {
      Yap_Abolish(pp);
    }
    LOCAL_HDiff = (char *)cl-base;
    read_bytes(stream, cl, size);
    pp->cs.p_code.FirstClause =
      pp->cs.p_code.LastClause =
      cl->ClCode;
    pp->PredFlags |= MegaClausePredFlag;
    /* enter index mode */
    pp->OpcodeOfPred = INDEX_OPCODE;
    pp->CodeOfPred = pp->cs.p_code.TrueCodeOfPred = (yamop *)(&(pp->OpcodeOfPred)); 
    /* This must be set for restoremegaclause */
    pp->cs.p_code.NOfClauses = nclauses;
    RestoreMegaClause(cl PASS_REGS);
  } else if (pp->PredFlags & DynamicPredFlag) {
    UInt i;

    for (i = 0; i < nclauses; i++) {
      char *base = (void *)read_uint(stream);
      UInt size = read_uint(stream);
      DynamicClause *cl = (DynamicClause *)Yap_AlwaysAllocCodeSpace(size);
    
      LOCAL_HDiff = (char *)cl-base;
      read_bytes(stream, cl, size);
      RestoreDynamicClause(cl, pp);
      Yap_AssertzClause(pp, cl->ClCode);
    }

  } else {
    UInt i;


    if (pp->PredFlags & SYSTEM_PRED_FLAGS) {
      if (nclauses) {
	ERROR(INCONSISTENT_CPRED);	
      }
      return;
    }
    Yap_Abolish(pp);
    for (i = 0; i < nclauses; i++) {
      char *base = (void *)read_uint(stream);
      UInt size = read_uint(stream);
      StaticClause *cl = (StaticClause *)Yap_AlwaysAllocCodeSpace(size);

      LOCAL_HDiff = (char *)cl-base;
      read_bytes(stream, cl, size);
      RestoreStaticClause(cl PASS_REGS);
      Yap_AssertzClause(pp, cl->ClCode);
    }
  }
}

static void
read_pred(IOSTREAM *stream, Term mod) {
  UInt flags = read_uint(stream);
  UInt arity = read_uint(stream);
  UInt nclauses, fl1;
  PredEntry *ap;

  if (mod == IDB_MODULE) {
    if (flags & AtomDBPredFlag) {
      Atom a = LookupAtom((Atom)read_uint(stream));
      if ((ap = RepPredProp(PredPropByAtom(a,mod))) == NULL) {
	ERROR(OUT_OF_CODE_SPACE);
      }
    } else  if (flags & NumberDBPredFlag) {
      Int i = (Int)read_uint(stream);
      if ((ap = Yap_FindLUIntKey(i)) == NULL) {
	ERROR(OUT_OF_CODE_SPACE);
      }
    } else {
      Functor f = LookupFunctor((Functor)read_uint(stream));
      if ((ap = RepPredProp(PredPropByFunc(f,mod))) == NULL) {
	ERROR(OUT_OF_CODE_SPACE);
      }
    }
  } else {
    if (arity) {
      Functor f;

      f = LookupFunctor((Functor)read_uint(stream));
      if ((ap = RepPredProp(PredPropByFunc(f,mod))) == NULL) {
	ERROR(OUT_OF_CODE_SPACE);
      }
    } else {
      Atom a;

      a = LookupAtom((Atom)read_uint(stream));
      if ((ap = RepPredProp(PredPropByAtom(a,mod))) == NULL) {
	ERROR(OUT_OF_CODE_SPACE);
      }
    }
  }
  ap->ArityOfPE = arity;
  nclauses = read_uint(stream);
  if (ap->PredFlags & IndexedPredFlag) {
    Yap_RemoveIndexation(ap);
  }
  fl1 = flags & STATIC_PRED_FLAGS;
  ap->PredFlags &= ~STATIC_PRED_FLAGS;
  ap->PredFlags |= fl1;
  if (flags & NumberDBPredFlag) {
    ap->src.IndxId = read_uint(stream);
  } else {
    ap->src.OwnerFile = (Atom)read_uint(stream);
    if (ap->src.OwnerFile) {
      ap->src.OwnerFile = AtomAdjust(ap->src.OwnerFile);
    }
  }
  read_clauses(stream, ap, nclauses, flags);
}

static void
read_ops(IOSTREAM *stream)  {
  Int x;
  while ((x = read_tag(stream)) != QLY_END_OPS) {
    Atom at = (Atom)read_uint(stream);
    Term mod = (Term)read_uint(stream);
    OpEntry *op;

    at = AtomAdjust(at);
    if (mod)
      mod = MkAtomTerm(AtomAdjust(AtomOfTerm(mod)));
    op = Yap_OpPropForModule(at, mod);
    op->Prefix =   read_bits16(stream);
    op->Infix =   read_bits16(stream);
    op->Posfix =   read_bits16(stream);
  }
}


static void
read_module(IOSTREAM *stream) {
  CACHE_REGS
  qlf_tag_t x;

  InitHash();
  read_header(stream);
  ReadHash(stream);
  while ((x = read_tag(stream)) == QLY_START_MODULE) {
    Term mod = (Term)read_uint(stream);

    mod = MkAtomTerm(AtomAdjust(AtomOfTerm(mod)));
    if (mod)
    while ((x = read_tag(stream)) == QLY_START_PREDICATE) {
      read_pred(stream, mod);
    }
  }
  read_ops(stream);
  CloseHash();
}

static Int
p_read_module_preds( USES_REGS1 )
{
  IOSTREAM *stream;

  if (!Yap_getInputStream(Yap_InitSlot(Deref(ARG1) PASS_REGS), &stream)) {
    return FALSE;
  }
  read_module(stream);
  return TRUE;
}

static Int
p_read_program( USES_REGS1 )
{
  IOSTREAM *stream;
  void YAP_Reset(void);

  if (!Yap_getInputStream(Yap_InitSlot(Deref(ARG1) PASS_REGS), &stream)) {
    return FALSE;
  }
  YAP_Reset();
  read_module(stream);
  Sclose( stream );
  /* back to the top level we go */
  Yap_CloseSlots();

  siglongjmp(LOCAL_RestartEnv, 3);  
  return TRUE;
}

int 
Yap_Restore(char *s, char *lib_dir)
{
  CACHE_REGS
  IOSTREAM *stream  = Yap_OpenRestore(s, lib_dir);
  if (!stream) 
    return -1;
  read_module(stream);
  Sclose( stream );
  return DO_ONLY_CODE;
}


void Yap_InitQLYR(void)
{
  Yap_InitCPred("$qload_module_preds", 1, p_read_module_preds, SyncPredFlag|HiddenPredFlag|UserCPredFlag);
  Yap_InitCPred("$qload_program", 1, p_read_program, SyncPredFlag|HiddenPredFlag|UserCPredFlag);
  if (FALSE) {
    restore_codes();
  }
}

