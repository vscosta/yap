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
 * File:		qlyr.c *
 * comments:	quick saver/loader					 *
 *									 *
 * Last rev:     $Date: 2011-08-29$,$Author: vsc $			 *
 * $Log: not supported by cvs2svn $					 *
 *									 *
 *************************************************************************/
/**
 *
 * @file qlyr.c
 *
 * @addtogroup SaveRestoreSupport
 * @{
 *
 */

#include "absmi.h"
#include "alloc.h"
#include "attvar.h"
#include "iopreds.h"
#include "yapio.h"
#include <Foreign.h>
#if HAVE_STRING_H
#include <string.h>
#endif

#include "qly.h"

static void RestoreEntries(PropEntry *, int USES_REGS);
static void CleanCode(PredEntry *USES_REGS);

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
  BAD_READ = 11,
  BAD_HEADER = 12
} qlfr_err_t;

static char *qlyr_error[] = {
    "out of temporary space",
    "out of temporary space",
    "out of code space",
    "unknown atom in saved space",
    "unknown functor in saved space",
    "unknown predicate in saved space",
    "unknown YAAM opcode in saved space",
    "unknown data-base reference in saved space",
    "corrupted atom in saved space",
    "formatting mismatch in saved space",
    "foreign predicate has different definition in saved space",
    "bad read"};

static char *Yap_AlwaysAllocCodeSpace(UInt size) {
  char *out;
  while (!(out = Yap_AllocCodeSpace(size))) {
    if (!Yap_growheap(FALSE, size, NULL)) {
      return NULL;
    }
  }
  return out;
}


#define QLYR_ERROR(err)                                                  \
  QLYR_ERROR__(__FILE__, __FUNCTION__, __LINE__, err)



static void QLYR_ERROR__(const char *file, const char *function, int lineno,
                       qlfr_err_t my_err) {
  // __android_log_print(ANDROID_LOG_INFO, "YAP ", "error %s in saved state
  // %s",GLOBAL_RestoreFile, qlyr_error[my_err]);
    Yap_Error__(false, file, function, lineno, SYSTEM_ERROR_SAVED_STATE, TermNil, "error %s in saved state %s",
              GLOBAL_RestoreFile, qlyr_error[my_err]);
  Yap_exit(1);
}

static Atom LookupAtom(Atom oat) {
  CACHE_REGS
  CELL hash = (CELL)(oat) % LOCAL_ImportAtomHashTableSize;
  import_atom_hash_entry_t *a;

  a = LOCAL_ImportAtomHashChain[hash];
  while (a) {
    if (a->oval == oat) {
      return a->val;
    }
    a = a->next;
  }
  //  __android_log_print(ANDROID_LOG_INFO, "YAP ", "error %p in saved state ",
  //  oat);
  QLYR_ERROR(UNKNOWN_ATOM);
  return NIL;
}

static void InsertAtom(Atom oat, Atom at) {
  CACHE_REGS
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

static Functor LookupFunctor(Functor ofun) {
  CACHE_REGS
  CELL hash = (CELL)(ofun) % LOCAL_ImportFunctorHashTableSize;
  import_functor_hash_entry_t *f;

  f = LOCAL_ImportFunctorHashChain[hash];
  while (f) {
    if (f->oval == ofun) {
      return f->val;
    }
    f = f->next;
  }
  QLYR_ERROR(UNKNOWN_FUNCTOR);
  return NIL;
}

static void InsertFunctor(Functor ofun, Functor fun) {
  CACHE_REGS
  CELL hash = (CELL)(ofun) % LOCAL_ImportFunctorHashTableSize;
  import_functor_hash_entry_t *f;

  f = LOCAL_ImportFunctorHashChain[hash];
  while (f) {
    if (f->oval == ofun) {
      return;
    }
    f = f->next;
  }
  f = (import_functor_hash_entry_t *)malloc(
      sizeof(import_functor_hash_entry_t));
  if (!f) {
    return;
  }
  f->val = fun;
  f->oval = ofun;
  f->next = LOCAL_ImportFunctorHashChain[hash];
  LOCAL_ImportFunctorHashChain[hash] = f;
}

static PredEntry *LookupPredEntry(PredEntry *op) {
  CACHE_REGS
  CELL hash;
  import_pred_entry_hash_entry_t *p;

  if (LOCAL_ImportPredEntryHashTableSize == 0)
    return NULL;
  hash = (CELL)(op) % LOCAL_ImportPredEntryHashTableSize;
  p = LOCAL_ImportPredEntryHashChain[hash];
  while (p) {
    if (p->oval == op) {
      return p->val;
    }
    p = p->next;
  }
  QLYR_ERROR(UNKNOWN_PRED_ENTRY);
  return NIL;
}

static void InsertPredEntry(PredEntry *op, PredEntry *pe) {
  CACHE_REGS
  CELL hash;
  import_pred_entry_hash_entry_t *p;

  if (LOCAL_ImportPredEntryHashTableSize == 0)
    return;
  hash = (CELL)(op) % LOCAL_ImportPredEntryHashTableSize;
  p = LOCAL_ImportPredEntryHashChain[hash];
  while (p) {
    if (p->oval == op) {
      return;
    }
    p = p->next;
  }
  p = (import_pred_entry_hash_entry_t *)malloc(
      sizeof(import_pred_entry_hash_entry_t));
  if (!p) {
    return;
  }
  p->val = pe;
  p->oval = op;
  p->next = LOCAL_ImportPredEntryHashChain[hash];
  LOCAL_ImportPredEntryHashChain[hash] = p;
}

static OPCODE LookupOPCODE(OPCODE op) {
  CACHE_REGS
  CELL hash = (CELL)(op) % LOCAL_ImportOPCODEHashTableSize;
  import_opcode_hash_entry_t *f;

  f = LOCAL_ImportOPCODEHashChain[hash];
  while (f) {
    if (f->oval == op) {
      return f->val;
    }
    f = f->next;
  }
  QLYR_ERROR(UNKNOWN_OPCODE);
  return NIL;
}

static int OpcodeID(OPCODE op) {
  CACHE_REGS
  CELL hash = (CELL)(op) % LOCAL_ImportOPCODEHashTableSize;
  import_opcode_hash_entry_t *f;

  f = LOCAL_ImportOPCODEHashChain[hash];
  while (f) {
    if (f->oval == op) {
      return f->id;
    }
    f = f->next;
  }
  QLYR_ERROR(UNKNOWN_OPCODE);
  return NIL;
}

static void InsertOPCODE(OPCODE op0, int i, OPCODE op) {
  CACHE_REGS
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

static DBRef LookupDBRef(DBRef dbr, int inc_ref) {
  CACHE_REGS
  CELL hash;
  import_dbref_hash_entry_t *p;

  if (LOCAL_ImportDBRefHashTableSize == 0)
    return NULL;
  hash = (CELL)(dbr) % LOCAL_ImportDBRefHashTableSize;
  p = LOCAL_ImportDBRefHashChain[hash];
  while (p) {
    if (p->oval == dbr) {
      if (inc_ref) {
        p->count++;
      }
      return p->val;
    }
    p = p->next;
  }
  QLYR_ERROR(UNKNOWN_DBREF);
  return NIL;
}

static LogUpdClause *LookupMayFailDBRef(DBRef dbr) {
  CACHE_REGS
  CELL hash;
  import_dbref_hash_entry_t *p;

  if (LOCAL_ImportDBRefHashTableSize == 0)
    return NULL;
  hash = (CELL)(dbr) % LOCAL_ImportDBRefHashTableSize;
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

static void InsertDBRef(DBRef dbr0, DBRef dbr) {
  CACHE_REGS
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

static void InitHash(void) {
  CACHE_REGS
  LOCAL_ImportOPCODEHashTableSize = EXPORT_OPCODE_TABLE_SIZE;
  LOCAL_ImportOPCODEHashChain = (import_opcode_hash_entry_t **)calloc(
      1,
      sizeof(import_opcode_hash_entry_t *) * LOCAL_ImportOPCODEHashTableSize);
}

static void CloseHash(void) {
  CACHE_REGS
  UInt i;
  for (i = 0; i < LOCAL_ImportFunctorHashTableSize; i++) {
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
  for (i = 0; i < LOCAL_ImportAtomHashTableSize; i++) {
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
  for (i = 0; i < LOCAL_ImportOPCODEHashTableSize; i++) {
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
  for (i = 0; i < LOCAL_ImportPredEntryHashTableSize; i++) {
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
  for (i = 0; i < LOCAL_ImportDBRefHashTableSize; i++) {
    import_dbref_hash_entry_t *a = LOCAL_ImportDBRefHashChain[i];
    while (a) {
      import_dbref_hash_entry_t *a0 = a;
#ifdef DEBUG
      if (!a->count) {
        fprintf(stderr, "WARNING: unused reference %p %p\n", a->val, a->oval);
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

static inline Atom AtomAdjust(Atom a) { return LookupAtom(a); }

static inline Functor FuncAdjust(Functor f) {
  return LookupFunctor(f);
  return f;
}

static inline Term AtomTermAdjust(Term t) {
  return MkAtomTerm(LookupAtom(AtomOfTerm(t)));
}

static inline Term TermToGlobalOrAtomAdjust(Term t) {
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
#define IntegerAdjust(D) (D)
#define AddrAdjust(P) (P)
#define MFileAdjust(P) (P)

#define CodeVarAdjust(P) CodeVarAdjust__(P PASS_REGS)
static inline Term CodeVarAdjust__(Term var USES_REGS) {
  if (var == 0L)
    return var;
  return (Term)(CharP(var) + LOCAL_HDiff);
}

#define ConstantAdjust(P) (P)
#define ArityAdjust(P) (P)
#define DoubleInCodeAdjust(P)
#define IntegerInCodeAdjust(Pxb)

static inline PredEntry *PtoPredAdjust(PredEntry *p) {
  return LookupPredEntry(p);
}

static inline PredEntry *PredEntryAdjust(PredEntry *p) {
  return LookupPredEntry(p);
}

static inline OPCODE OpcodeAdjust(OPCODE OP) { return LookupOPCODE(OP); }

static inline Term ModuleAdjust(Term M) {
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
static inline Term BlobTermInCodeAdjust__(Term t USES_REGS) {
  return (Term)((char *)(t)-LOCAL_HDiff);
}
#else
static inline Term BlobTermInCodeAdjust__(Term t USES_REGS) {
  return (Term)((char *)(t) + LOCAL_HDiff);
}
#endif
#define DBTermAdjust(P) DBTermAdjust__(P PASS_REGS)
static inline DBTerm *DBTermAdjust__(DBTerm *dbtp USES_REGS) {
  return (DBTerm *)(CharP(dbtp) + LOCAL_HDiff);
}

#define CellPtoHeapAdjust(P) CellPtoHeapAdjust__(P PASS_REGS)
static inline CELL *CellPtoHeapAdjust__(CELL *dbtp USES_REGS) {
  return (CELL *)(CharP(dbtp) + LOCAL_HDiff);
}

#define PtoAtomHashEntryAdjust(P) (P)
#define CellPtoHeapCellAdjust(P) (P)
#define CellPtoTRAdjust(P) (P)
#define CodeAddrAdjust(P) (P)
#define ConsultObjAdjust(P) (P)
#define DelayAddrAdjust(P) (P)
#define DelayAdjust(P) (P)
#define GlobalAdjust(P) (P)

#define DBRefAdjust(P, Ref) DBRefAdjust__(P, Ref PASS_REGS)
static inline DBRef DBRefAdjust__(DBRef dbtp, int do_reference USES_REGS) {
  return LookupDBRef(dbtp, do_reference);
}

#define DBRefPAdjust(P) DBRefPAdjust__(P PASS_REGS)
static inline DBRef *DBRefPAdjust__(DBRef *dbtp USES_REGS) {
  return (DBRef *)((char *)(dbtp) + LOCAL_HDiff);
}

#define LUIndexAdjust(P) (P)
#define SIndexAdjust(P) (P)
#define LocalAddrAdjust(P) (P)
#define GlobalAddrAdjust(P) (P)
#define OpListAdjust(P) (P)

#define PtoLUCAdjust(P) PtoLUCAdjust__(P PASS_REGS)
#define PtoLUClauseAdjust(P) PtoLUCAdjust__(P PASS_REGS)
static inline LogUpdClause *PtoLUCAdjust__(LogUpdClause *dbtp USES_REGS) {
  return (LogUpdClause *)((char *)(dbtp) + LOCAL_HDiff);
}

#define PtoStCAdjust(P) (P)
#define PtoArrayEAdjust(P) (P)
#define PtoArraySAdjust(P) (P)
#define PtoGlobalEAdjust(P) (P)
#define PtoDelayAdjust(P) (P)
#define PtoGloAdjust(P) (P)
#define PtoLocAdjust(P) (P)

#define PtoHeapCellAdjust(P) PtoHeapCellAdjust__(P PASS_REGS)
static inline CELL *PtoHeapCellAdjust__(CELL *ptr USES_REGS) {
  LogUpdClause *out;
  if ((out = LookupMayFailDBRef((DBRef)ptr)))
    return (CELL *)out;
  return (CELL *)(CharP(ptr) + LOCAL_HDiff);
}

#define TermToGlobalAdjust(P) (P)
#define PtoOpAdjust(P) PtoOpAdjust__(P PASS_REGS)
static inline yamop *PtoOpAdjust__(yamop *ptr USES_REGS) {
  if (ptr) {
    if (ptr == LOCAL_ImportFAILCODE)
      return FAILCODE;
    return (yamop *)((char *)(ptr) + LOCAL_HDiff);
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
static inline wamreg XAdjust__(wamreg reg USES_REGS) {
  return (wamreg)((wamreg)((reg) + LOCAL_XDiff));
}
#else
#define XAdjust(X) (X)
#endif
#define YAdjust(X) (X)
#define HoldEntryAdjust(P) (P)
#define CodeCharPAdjust(P) (P)
#define CodeConstCharPAdjust(P) (P)
#define CodeVoidPAdjust(P) (P)
#define HaltHookAdjust(P) (P)

#define recompute_mask(dbr)

#define rehash(oldcode, NOfE, KindOfEntries)

#define RestoreSWIHash()

#define Yap_op_from_opcode(OP) OpcodeID(OP)

static void RestoreFlags(UInt NFlags) {}

#include "rheap.h"

static void RestoreHashPreds(USES_REGS1) {}

static void RestoreAtomList(Atom atm USES_REGS) {}

static bool maybe_read_bytes(FILE *stream, void *ptr, size_t sz) {
  do {
    size_t count;
    if ((count = fread(ptr, 1, sz, stream)) == sz)
      return true;
    if (feof(stream) || ferror(stream))
      return false;
    sz -= count;
    ptr += count;
  } while (true);
}
    
static size_t read_bytes(FILE *stream, void *ptr, size_t sz) {
  do {
    size_t count = fread(ptr, 1, sz, stream);
    if (count == sz)
      return  sz;
    if (feof(stream)) {
        PlIOError(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM, TermNil, "read_qly/3: expected %ld bytes got %ld", sz, count);
        return 0;
      } else if (ferror(stream)) {
        PlIOError(PERMISSION_ERROR_INPUT_STREAM, TermNil, "read_qly/3: expected %ld bytes got error %s", sz, strerror(errno));
        return 0;
      }
    sz -= count;
    } while(true);
}

static unsigned char read_byte(FILE *stream) { return getc(stream); }

static BITS16 read_bits16(FILE *stream) {
  BITS16 v;
  read_bytes(stream, &v, sizeof(BITS16));
  return v;
}

static UInt read_UInt(FILE *stream) {
  UInt v;
  read_bytes(stream, &v, sizeof(UInt));
  return v;
}

static Int read_Int(FILE *stream) {
  Int v;
  read_bytes(stream, &v, sizeof(Int));
  return v;
}

static qlf_tag_t read_tag(FILE *stream) {
  int ch = read_byte(stream);
  return ch;
}

static pred_flags_t read_predFlags(FILE *stream) {
  pred_flags_t v;
  read_bytes(stream, &v, sizeof(pred_flags_t));
  return v;
}


static Atom do_header(FILE *stream) {
  char s[2049], *p = s, *q;
  char h0[] = "#!/bin/sh\nexec_dir=${YAPBINDIR:-";
  char h1[] = "exec $exec_dir/yap $0 \"$@\"\nsaved ";
  Atom at;

  memset(s,0,2049);
  if (!maybe_read_bytes( stream, s, 2048) )
    return NIL;
  if (strstr(s, h0)!= s)
    return NIL;
  if ((p=strstr(s, h1)) == NULL) {
    return NIL;
  }
  p += strlen(h1);
  q = strchr(p,',');
  if (!q)
    return NIL;
  q[0] = '\0';
  at = Yap_LookupAtom(p);
  return at;
}

static Int get_header(USES_REGS1) {
  FILE *stream;
  Term t1 = Deref(ARG1);
  Atom at;
  Int rc;

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "read_program/3");
    return FALSE;
  }
  if (!(stream = Yap_GetInputStream(t1, "header scanning in qload"))) {
    return false;
  }
    sigjmp_buf signew, *sighold = LOCAL_RestartEnv;
  LOCAL_RestartEnv = &signew;

  if (sigsetjmp(signew, 1) != 0) {
      LOCAL_RestartEnv = sighold;
      return false;
    }
  if ((at = do_header(stream)) == NIL) 
    rc = false;
  else {
    rc = Yap_unify(ARG2, MkAtomTerm(at));
  }
    LOCAL_RestartEnv = sighold;
    return rc;
}

static void ReadHash(FILE *stream) {
  CACHE_REGS
  UInt i;
  RCHECK(read_tag(stream) == QLY_START_X);
  LOCAL_XDiff = (char *)(&ARG1) - (char *)read_UInt(stream);
  RCHECK(read_tag(stream) == QLY_START_OPCODES);
  RCHECK(read_Int(stream) == _std_top);
  for (i = 0; i <= _std_top; i++) {
    InsertOPCODE((OPCODE)read_UInt(stream), i, Yap_opcode(i));
  }
  RCHECK(read_tag(stream) == QLY_START_ATOMS);
  LOCAL_ImportAtomHashTableNum = read_UInt(stream);
  LOCAL_ImportAtomHashTableSize = LOCAL_ImportAtomHashTableNum * 2;
  LOCAL_ImportAtomHashChain = (import_atom_hash_entry_t **)calloc(
      LOCAL_ImportAtomHashTableSize, sizeof(import_atom_hash_entry_t *));
  for (i = 0; i < LOCAL_ImportAtomHashTableNum; i++) {
    Atom oat = (Atom)read_UInt(stream);
    Atom at;
    qlf_tag_t tg = read_tag(stream);

    if (tg == QLY_ATOM) {
      char *rep = (char *)AllocTempSpace();
      UInt len;

      len = read_UInt(stream);
      if (!EnoughTempSpace(len))
        QLYR_ERROR(OUT_OF_TEMP_SPACE);
      read_bytes(stream, rep, (len + 1) * sizeof(char));
      while (!(at = Yap_FullLookupAtom(rep))) {
        if (!Yap_growheap(FALSE, 0, NULL)) {
          exit(1);
        }
      }
      if (at == NIL)
        QLYR_ERROR(OUT_OF_ATOM_SPACE);
    } else {
      QLYR_ERROR(BAD_ATOM);
      return;
    }
    InsertAtom(oat, at);
  }
  /* functors */
  RCHECK(read_tag(stream) == QLY_START_FUNCTORS);
  LOCAL_ImportFunctorHashTableNum = read_UInt(stream);
  LOCAL_ImportFunctorHashTableSize = 2 * LOCAL_ImportFunctorHashTableNum;
  LOCAL_ImportFunctorHashChain = (import_functor_hash_entry_t **)calloc(
      LOCAL_ImportFunctorHashTableSize, sizeof(import_functor_hash_entry_t *));
  for (i = 0; i < LOCAL_ImportFunctorHashTableNum; i++) {
    Functor of = (Functor)read_UInt(stream);
    UInt arity = read_UInt(stream);
    Atom oat = (Atom)read_UInt(stream);
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
  LOCAL_ImportPredEntryHashTableNum = read_UInt(stream);
  LOCAL_ImportPredEntryHashTableSize = 2 * LOCAL_ImportPredEntryHashTableNum;
  LOCAL_ImportPredEntryHashChain = (import_pred_entry_hash_entry_t **)calloc(
      LOCAL_ImportPredEntryHashTableSize,
      sizeof(import_pred_entry_hash_entry_t *));
  for (i = 0; i < LOCAL_ImportPredEntryHashTableNum; i++) {
    PredEntry *ope = (PredEntry *)read_UInt(stream), *pe;
    UInt arity = read_UInt(stream);
    Atom omod = (Atom)read_UInt(stream);
    Term mod;

    if (omod) {
      mod = MkAtomTerm(AtomAdjust(omod));
      if (mod == TermProlog)
        mod = 0;
    } else {
      mod = TermProlog;
    }

    if (mod != IDB_MODULE) {
      if (arity) {
        Functor of = (Functor)read_UInt(stream);
        Functor f = LookupFunctor(of);
        while (!(pe = RepPredProp(PredPropByFuncAndMod(f, mod)))) {
          if (!Yap_growheap(FALSE, 0, NULL)) {
            exit(1);
          }
        }
      } else {
        Atom oa = (Atom)read_UInt(stream);
        Atom a = LookupAtom(oa);
        pe = RepPredProp(PredPropByAtomAndMod(a, mod));
      }
    } else {
      /* IDB */
      if (arity == (UInt)-1) {
        UInt i = read_UInt(stream);
        pe = Yap_FindLUIntKey(i);
      } else if (arity == (UInt)(-2)) {
        Atom oa = (Atom)read_UInt(stream);
        Atom a = LookupAtom(oa);
        pe = RepPredProp(PredPropByAtomAndMod(a, mod));
        pe->PredFlags |= AtomDBPredFlag;
      } else {
        Functor of = (Functor)read_UInt(stream);
        Functor f = LookupFunctor(of);
        pe = RepPredProp(PredPropByFuncAndMod(f, mod));
      }
      pe->PredFlags |= LogUpdatePredFlag;
      pe->ArityOfPE = 3;
      if (pe->OpcodeOfPred == UNDEF_OPCODE) {
        pe->OpcodeOfPred = Yap_opcode(_op_fail);
        pe->TrueCodeOfPred = pe->CodeOfPred = FAILCODE;
      }
    }
    InsertPredEntry(ope, pe);
  }
  RCHECK(read_tag(stream) == QLY_START_DBREFS);
  LOCAL_ImportDBRefHashTableNum = read_UInt(stream);
  LOCAL_ImportDBRefHashTableSize = 2 * LOCAL_ImportDBRefHashTableNum + 17;
  LOCAL_ImportDBRefHashChain = (import_dbref_hash_entry_t **)calloc(
      LOCAL_ImportDBRefHashTableSize, sizeof(import_dbref_hash_entry_t *));
  for (i = 0; i < LOCAL_ImportDBRefHashTableNum; i++) {
    LogUpdClause *ocl = (LogUpdClause *)read_UInt(stream);
    UInt sz = read_UInt(stream);
    UInt nrefs = read_UInt(stream);
    LogUpdClause *ncl = (LogUpdClause *)Yap_AlwaysAllocCodeSpace(sz);
    Yap_LUClauseSpace += sz;
    if (!ncl) {
      QLYR_ERROR(OUT_OF_CODE_SPACE);
    }
    ncl->Id = FunctorDBRef;
    ncl->ClRefCount = nrefs;
    InsertDBRef((DBRef)ocl, (DBRef)ncl);
  }
  RCHECK(read_tag(stream) == QLY_FAILCODE);
  LOCAL_ImportFAILCODE = (yamop *)read_UInt(stream);
}

static void read_clauses(FILE *stream, PredEntry *pp, UInt nclauses,
                         pred_flags_t flags) {
  CACHE_REGS
  if (flags & LogUpdatePredFlag) {
    /* first, clean up whatever was there */
    if (pp->NOfClauses) {
      LogUpdClause *cl;
      cl = ClauseCodeToLogUpdClause(pp->FirstClause);
      do {
        LogUpdClause *ncl = cl->ClNext;
        Yap_ErLogUpdCl(cl);
        cl = ncl;
      } while (cl != NULL);
    }
    if (!nclauses) {
    pp->CodeOfPred = pp->TrueCodeOfPred = FAILCODE;
    pp->OpcodeOfPred = FAIL_OPCODE;

      return;
    }
    while ((read_tag(stream) == QLY_START_LU_CLAUSE)) {
      char *base = (void *)read_UInt(stream);
      UInt size = read_UInt(stream);
      LogUpdClause *cl;
      Int nrefs = 0;

      if ((cl = LookupMayFailDBRef((DBRef)base))) {
        nrefs = cl->ClRefCount;
      } else {
        cl = (LogUpdClause *)Yap_AlwaysAllocCodeSpace(size);
	Yap_LUClauseSpace += size;
      }
      read_bytes(stream, cl, size);
      cl->ClFlags &= ~InUseMask;
      cl->ClRefCount = nrefs;
      LOCAL_HDiff = (char *)cl - base;
      RestoreLUClause(cl, pp PASS_REGS);
      Yap_AssertzClause(pp, cl->ClCode);
    }
  } else if (flags & MegaClausePredFlag) {
    CACHE_REGS
    char *base = (void *)read_UInt(stream);
    UInt mask = read_UInt(stream);
    UInt size = read_UInt(stream);
	Yap_ClauseSpace += size;
    MegaClause *cl = (MegaClause *)Yap_AlwaysAllocCodeSpace(size);

    if (nclauses) {
      Yap_Abolish(pp);
    }
    LOCAL_HDiff = (char *)cl - base;
    read_bytes(stream, cl, size);
    cl->ClFlags = mask;
    pp->FirstClause = pp->LastClause = cl->ClCode;
    pp->PredFlags |= MegaClausePredFlag;
    /* enter index mode */
    if (mask & ExoMask) {
      struct index_t **icl = (struct index_t **)(cl->ClCode);
      pp->OpcodeOfPred = Yap_opcode(_enter_exo);
      icl[0] = NULL;
      icl[1] = NULL;
    } else {
      pp->OpcodeOfPred = INDEX_OPCODE;
    }
    pp->CodeOfPred = pp->TrueCodeOfPred =
        (yamop *)(&(pp->OpcodeOfPred));
    /* This must be set for restoremegaclause */
    pp->NOfClauses = nclauses;
    RestoreMegaClause(cl PASS_REGS);
  } else if (flags & DynamicPredFlag) {
    UInt i;

    for (i = 0; i < nclauses; i++) {
      char *base = (void *)read_UInt(stream);
      UInt size = read_UInt(stream);
      DynamicClause *cl = (DynamicClause *)Yap_AlwaysAllocCodeSpace(size);
	Yap_LUClauseSpace += size;

      LOCAL_HDiff = (char *)cl - base;
      read_bytes(stream, cl, size);
      INIT_LOCK(cl->ClLock);
      RestoreDynamicClause(cl, pp PASS_REGS);
      Yap_AssertzClause(pp, cl->ClCode);
    }

  } else {
    UInt i;

    if (flags & SYSTEM_PRED_FLAGS) {
      if (nclauses) {
        QLYR_ERROR(INCONSISTENT_CPRED);
      }
      return;
    }
    if (pp->NOfClauses) {
      StaticClause *cl;
      cl = ClauseCodeToStaticClause(pp->FirstClause);
      do {
        StaticClause *ncl = cl->ClNext;
        Yap_EraseStaticClause(cl, pp, CurrentModule);
        cl = ncl;
      } while (cl != NULL);
    } else if (flags & MultiFileFlag) {
    pp->CodeOfPred = pp->TrueCodeOfPred = FAILCODE;
    pp->OpcodeOfPred = FAIL_OPCODE;

    }
    for (i = 0; i < nclauses; i++) {
      char *base = (void *)read_UInt(stream);
      UInt size = read_UInt(stream);
      StaticClause *cl = (StaticClause *)Yap_AlwaysAllocCodeSpace(size);
	Yap_ClauseSpace += size;

      LOCAL_HDiff = (char *)cl - base;
      read_bytes(stream, cl, size);
      RestoreStaticClause(cl PASS_REGS);
      Yap_AssertzClause(pp, cl->ClCode);
    }
  }
}

static void read_pred(FILE *stream, Term mod) {
  pred_flags_t flags;
  UInt nclauses;
  PredEntry *ap;

  ap = LookupPredEntry((PredEntry *)read_UInt(stream));
  flags = read_predFlags(stream);
  //  fprintf(stderr, "next %lx-%lx %lx: ", ap->PredFlags, flags, flags & ForeignPredFlags); (Yap_DebugWriteIndicator(ap));
 #if 0
  if (ap->ArityOfPE && ap->ModuleOfPred != IDB_MODULE)
    // __android_log_print(ANDROID_LOG_INFO, "YAP ", "   %s/%ld %llx %llx\n", NameOfFunctor(ap->FunctorOfPred)->StrOfAE, ap->ArityOfPE, ap->PredFlags, flags);
    //     printf("   %s/%ld %llx %llx\n", NameOfFunctor(ap->FunctorOfPred)->StrOfAE, ap->ArityOfPE, ap->PredFlags, flags);
  else if (ap->ModuleOfPred != IDB_MODULE)
    //__android_log_print(ANDROID_LOG_INFO, "YAP ","   %s/%ld %llx %llx\n", ((Atom)(ap->FunctorOfPred))->StrOfAE, ap->ArityOfPE, flags);
     printf("   %s/%ld %llx %llx\n", ((Atom)(ap->FunctorOfPred))->StrOfAE, ap->ArityOfPE, ap->PredFlags, flags);
    //else
    //  __android_log_print(ANDROID_LOG_INFO, "YAP ","   number\n");
#endif
if (flags & ForeignPredFlags) {
  if (!(ap->PredFlags & (ForeignPredFlags))) {
    fprintf(stderr, "C-predicate does not exist in new engine: ");
  Yap_DebugWriteIndicator(ap);
	    
      QLYR_ERROR(INCONSISTENT_CPRED);
  }
    if (flags & MetaPredFlag)
      ap->PredFlags |= MetaPredFlag;
    return;
  }
  nclauses = read_UInt(stream);
  if (ap->PredFlags & IndexedPredFlag) {
    Yap_RemoveIndexation(ap);
  }
  // fl1 = flags & ((pred_flags_t)STATIC_PRED_FLAGS|(UInt)EXTRA_PRED_FLAGS);
  // ap->PredFlags &= ~((UInt)STATIC_PRED_FLAGS|(UInt)EXTRA_PRED_FLAGS);
  ap->PredFlags = flags & ~StatePredFlags;
  if (nclauses && (ap->PredFlags & UndefPredFlag)) {
    ap->PredFlags &= ~UndefPredFlag;
  }
  if (flags & NumberDBPredFlag) {
    ap->src.IndxId = read_UInt(stream);
  } else {
    ap->src.OwnerFile = (Atom)read_UInt(stream);

    if (ap->src.OwnerFile) {
      ap->src.OwnerFile = AtomAdjust(ap->src.OwnerFile);
    }
  }
  ap->TimeStampOfPred = read_UInt(stream);
  /* multifile predicates cannot reside in module 0 */
  //  if (flags & MultiFileFlag && ap->ModuleOfPred == PROLOG_MODULE) {
  //  ap->ModuleOfPred = TermProlog;
  // }
  if (nclauses && !(ap->PredFlags & ForeignPredFlags))
    read_clauses(stream, ap, nclauses, flags);
#if DEBUG
// Yap_PrintPredName( ap );
#endif

  if (flags & HiddenPredFlag) {
    Yap_HidePred(ap);
  }
}

static void read_ops(FILE *stream) {
  Int x;
  while ((x = read_tag(stream)) != QLY_END_OPS) {
    Atom at = (Atom)read_UInt(stream);
    Term mod = (Term)read_UInt(stream);
    OpEntry *op;

    at = AtomAdjust(at);
    if (mod)
      mod = MkAtomTerm(AtomAdjust(AtomOfTerm(mod)));
    op = Yap_OpPropForModule(at, mod);
    op->Prefix = read_bits16(stream);
    op->Infix = read_bits16(stream);
    op->Posfix = read_bits16(stream);
    WRITE_UNLOCK(op->OpRWLock);
  }
}

static void read_module(FILE *stream) {
  qlf_tag_t x;

  InitHash();
  ReadHash(stream);
  while ((x = read_tag(stream)) == QLY_START_MODULE) {
    Term mod = (Term)read_UInt(stream);
    if (mod == 0)
      mod = TermProlog;
    mod = MkAtomTerm(AtomAdjust(AtomOfTerm(mod)));
    if (mod)
      while ((x = read_tag(stream)) == QLY_START_PREDICATE) {
        read_pred(stream, mod);
      }
  }
  read_ops(stream);
  CloseHash();
}

static Int p_read_module_preds(USES_REGS1) {
  FILE *stream;
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "read_qly/3");
    return FALSE;
  }
  if (!IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM, t1, "read_qly/3");
    return (FALSE);
  }
  if (!(stream = Yap_GetInputStream(t1, "scanning preducate modules"))) {
    return FALSE;
  }
  read_module(stream);
  return TRUE;
}

static void ReInitProlog(void) {
  Term t = MkAtomTerm(AtomInitProlog);
  YAP_RunGoalOnce(t);
}

static Int qload_program(USES_REGS1) {
  FILE *stream;


  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "read_program/3");
    return FALSE;
  }
  if ((stream = Yap_GetInputStream(t1, "from read_program"))) {
    return FALSE;
  }
  Yap_Reset(YAP_RESET_FROM_RESTORE, true);
  if (do_header(stream) == NIL)
    return FALSE;
  read_module(stream);
  fclose(stream);
  /* back to the top level we go */
  ReInitProlog();
  return true;
}

YAP_file_type_t Yap_Restore(const char *s) {
  CACHE_REGS

      int lvl = push_text_stack();
  const char *tmp = Yap_AbsoluteFile(s, true);

  FILE *stream = Yap_OpenRestore(tmp);
    if (!stream)
    return -1;
#define BUFSIX 4096*256
    char *buf = malloc(BUFSIZ);
    setvbuf(stream, buf, buf ? _IOFBF : _IONBF, BUFSIZ);
  GLOBAL_RestoreFile = s;
  if (do_header(stream) == NIL) {
    pop_text_stack(lvl);
    return YAP_PL;
  }
  read_module(stream);
  setBooleanGlobalPrologFlag(SAVED_PROGRAM_FLAG, true);
  fclose(stream);
  free(buf);
  GLOBAL_RestoreFile = NULL;
  LOCAL_SourceModule = CurrentModule = USER_MODULE;
  pop_text_stack(lvl);
  return YAP_QLY;
}

void Yap_InitQLYR(void) {
  Yap_InitCPred("$qload_module_preds", 1, p_read_module_preds,
                SyncPredFlag | UserCPredFlag | HiddenPredFlag);
  Yap_InitCPred("$qload_file_preds", 1, p_read_module_preds,
                SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("$qload_program", 1, qload_program,
                SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("$q_header", 2, get_header, SyncPredFlag | HiddenPredFlag);
  if (FALSE) {
    restore_codes();
  }
}

/// @}
