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
* File:		qlyw.c							 *
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

static void
LookupAtom(Atom at)
{
  CACHE_REGS
  char *p = RepAtom(at)->StrOfAE;
  CELL hash = HashFunction((unsigned char *)p) % LOCAL_ExportAtomHashTableSize;
  export_atom_hash_entry_t *a;

  a = LOCAL_ExportAtomHashChain[hash];
  while (a) {
    if (a->val == at) {
      return;
    }
    a = a->next;
  }
  a = (export_atom_hash_entry_t *)malloc(sizeof(export_atom_hash_entry_t));
  if (!a) {
    return;
  }
  a->val = at;
  a->next = LOCAL_ExportAtomHashChain[hash];
  LOCAL_ExportAtomHashChain[hash] = a;
  LOCAL_ExportAtomHashTableNum++;
}

static void
LookupFunctor(Functor fun)
{
  CACHE_REGS
  CELL hash = (CELL)(fun) % LOCAL_ExportFunctorHashTableSize;
  export_functor_hash_entry_t *f;
  Atom name = NameOfFunctor(fun);
  UInt arity  = ArityOfFunctor(fun);

  f = LOCAL_ExportFunctorHashChain[hash];
  while (f) {
    if (f->name == name && f->arity == arity) {
      return;
    }
    f = f->next;
  }
  f = (export_functor_hash_entry_t *)malloc(sizeof(export_functor_hash_entry_t));
  if (!f) {
    return;
  }
  LookupAtom(name);
  f->val = fun;
  f->name = name;
  f->arity = arity;
  f->next = LOCAL_ExportFunctorHashChain[hash];
  LOCAL_ExportFunctorHashChain[hash] = f;
  LOCAL_ExportFunctorHashTableNum++;
}

static void
LookupPredEntry(PredEntry *pe)
{
  CACHE_REGS
  CELL hash = (CELL)(pe) % LOCAL_ExportPredEntryHashTableSize;
  export_pred_entry_hash_entry_t *p;
  UInt arity  = pe->ArityOfPE;

  p = LOCAL_ExportPredEntryHashChain[hash];
  while (p) {
    if (p->val == pe) {
      return;
    }
    p = p->next;
  }
  p = (export_pred_entry_hash_entry_t *)malloc(sizeof(export_pred_entry_hash_entry_t));
  if (!p) {
    return;
  }
  p->arity = arity;
  p->val = pe;
  if (pe->ModuleOfPred != IDB_MODULE) {
    if (arity) {
      p->u.f = pe->FunctorOfPred;
      LookupFunctor(pe->FunctorOfPred);
    } else {
      p->u.a = (Atom)(pe->FunctorOfPred);
      LookupAtom((Atom)(pe->FunctorOfPred));
    }
  } else {
    if (pe->PredFlags & AtomDBPredFlag) {
      p->u.a = (Atom)(pe->FunctorOfPred);
      p->arity = (CELL)(-2);
      LookupAtom((Atom)(pe->FunctorOfPred));
    } else if (!(pe->PredFlags & NumberDBPredFlag)) {
      p->u.f = pe->FunctorOfPred;
      p->arity = (CELL)(-1);
      LookupFunctor(pe->FunctorOfPred);
    } else {
      p->u.f = pe->FunctorOfPred;
    }
  }
  if (pe->ModuleOfPred) {
    p->module = AtomOfTerm(pe->ModuleOfPred);
  } else {
    p->module = AtomProlog;
  }
  LookupAtom(p->module);
  p->next = LOCAL_ExportPredEntryHashChain[hash];
  LOCAL_ExportPredEntryHashChain[hash] = p;
  LOCAL_ExportPredEntryHashTableNum++;
}

static void
LookupDBRef(DBRef ref)
{
  CACHE_REGS
  CELL hash = Unsigned(ref) % LOCAL_ExportDBRefHashTableSize;
  export_dbref_hash_entry_t *a;

  a = LOCAL_ExportDBRefHashChain[hash];
  while (a) {
    if (a->val == ref) {
      a->refs++;
      return;
    }
    a = a->next;
  }
  a = (export_dbref_hash_entry_t *)malloc(sizeof(export_dbref_hash_entry_t));
  if (!a) {
    return;
  }
  a->val = ref;
  a->sz = ((LogUpdClause *)ref)->ClSize;
  a->refs = 1;
  a->next = LOCAL_ExportDBRefHashChain[hash];
  LOCAL_ExportDBRefHashChain[hash] = a;
  LOCAL_ExportDBRefHashTableNum++;
}

static void
InitHash(void)
{
  CACHE_REGS
  LOCAL_ExportFunctorHashTableNum = 0;
  LOCAL_ExportFunctorHashTableSize = EXPORT_FUNCTOR_TABLE_SIZE;
  LOCAL_ExportFunctorHashChain = (export_functor_hash_entry_t **)calloc(1, sizeof(export_functor_hash_entry_t *)* LOCAL_ExportFunctorHashTableSize);
  LOCAL_ExportAtomHashTableNum = 0;
  LOCAL_ExportAtomHashTableSize = EXPORT_ATOM_TABLE_SIZE;
  LOCAL_ExportAtomHashChain = (export_atom_hash_entry_t **)calloc(1, sizeof(export_atom_hash_entry_t *)* LOCAL_ExportAtomHashTableSize);
  LOCAL_ExportPredEntryHashTableNum = 0;
  LOCAL_ExportPredEntryHashTableSize = EXPORT_PRED_ENTRY_TABLE_SIZE;
  LOCAL_ExportPredEntryHashChain = (export_pred_entry_hash_entry_t **)calloc(1, sizeof(export_pred_entry_hash_entry_t *)* LOCAL_ExportPredEntryHashTableSize);
  LOCAL_ExportDBRefHashTableNum = 0;
  LOCAL_ExportDBRefHashTableSize = EXPORT_DBREF_TABLE_SIZE;
  LOCAL_ExportDBRefHashChain = (export_dbref_hash_entry_t **)calloc(1, sizeof(export_dbref_hash_entry_t *)* LOCAL_ExportDBRefHashTableSize);
}

static void
CloseHash(void)
{
  CACHE_REGS
  LOCAL_ExportFunctorHashTableNum = 0;
  LOCAL_ExportFunctorHashTableSize = 0L;
  free(LOCAL_ExportFunctorHashChain);
  LOCAL_ExportAtomHashTableNum = 0;
  LOCAL_ExportAtomHashTableSize = 0L;
  free(LOCAL_ExportAtomHashChain);
  LOCAL_ExportPredEntryHashTableNum = 0;
  LOCAL_ExportPredEntryHashTableSize = 0L;
  free(LOCAL_ExportPredEntryHashChain);
  LOCAL_ExportDBRefHashTableNum = 0;
  LOCAL_ExportDBRefHashTableSize = 0L;
  free(LOCAL_ExportDBRefHashChain);
}

static inline Atom
AtomAdjust(Atom a)
{
  LookupAtom(a);
  return a;
}

static inline Functor
FuncAdjust(Functor f)
{
  LookupFunctor(f);
  return f;
}


static inline Term
AtomTermAdjust(Term t)
{
  LookupAtom(AtomOfTerm(t));
  return t;  
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
#define CodeVarAdjust(P) (P)
#define ConstantAdjust(P) (P)
#define ArityAdjust(P) (P)
#define DoubleInCodeAdjust(P) 
#define IntegerInCodeAdjust(P) 
#define OpcodeAdjust(P) (P)

static inline Term
ModuleAdjust(Term t)
{
  if (!t) return t;
  return AtomTermAdjust(t);
}

static inline PredEntry *
PredEntryAdjust(PredEntry *pe)
{
  LookupPredEntry(pe);
  return pe;
}

static inline PredEntry *
PtoPredAdjust(PredEntry *pe)
{
  LookupPredEntry(pe);
  return pe;
}


#define ExternalFunctionAdjust(P) (P)
#define DBRecordAdjust(P) (P)
#define ModEntryPtrAdjust(P) (P)
#define AtomEntryAdjust(P) (P)
#define GlobalEntryAdjust(P) (P)
#define BlobTermInCodeAdjust(P) (P)
#define CellPtoHeapAdjust(P) (P)
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
DBRefAdjust__ (DBRef dbt USES_REGS)
{
  LookupDBRef(dbt);
  return dbt;
}

#define DBRefPAdjust(P) (P)
#define DBTermAdjust(P) (P)
#define LUIndexAdjust(P) (P)
#define SIndexAdjust(P) (P)
#define LocalAddrAdjust(P) (P)
#define GlobalAddrAdjust(P) (P)
#define OpListAdjust(P) (P)
#define PtoLUCAdjust(P) (P)
#define PtoStCAdjust(P) (P)
#define PtoArrayEAdjust(P) (P)
#define PtoArraySAdjust(P) (P)
#define PtoGlobalEAdjust(P) (P)
#define PtoDelayAdjust(P) (P)
#define PtoGloAdjust(P) (P)
#define PtoLocAdjust(P) (P)
#define PtoHeapCellAdjust(P) (P)
#define TermToGlobalAdjust(P) (P)
#define PtoOpAdjust(P) (P)
#define PtoLUClauseAdjust(P) (P)
#define PtoLUIndexAdjust(P) (P)
#define PtoDBTLAdjust(P) (P)
#define PtoPtoPredAdjust(P) (P)
#define OpRTableAdjust(P) (P)
#define OpEntryAdjust(P) (P)
#define PropAdjust(P) (P)
#define TrailAddrAdjust(P) (P)
#define XAdjust(P) (P)
#define YAdjust(P) (P)
#define HoldEntryAdjust(P) (P)
#define CodeCharPAdjust(P) (P)
#define CodeVoidPAdjust(P) (P)
#define HaltHookAdjust(P) (P)

#define recompute_mask(dbr)

#define rehash(oldcode, NOfE, KindOfEntries)

#define RestoreSWIHash()

#include "rheap.h"

static void
RestoreHashPreds( USES_REGS1 )
{
}


static void
RestoreAtomList(Atom atm USES_REGS)
{
}

static size_t save_bytes(IOSTREAM *stream, void *ptr, size_t sz)
{
  return Sfwrite(ptr, sz, 1, stream);
}

static size_t save_byte(IOSTREAM *stream, int byte)
{
  Sputc(byte, stream);
  return 1;
}

static size_t save_bits16(IOSTREAM *stream, BITS16 val)
{
  BITS16 v = val;
  return save_bytes(stream, &v, sizeof(BITS16));
}

static size_t save_uint(IOSTREAM *stream, UInt val)
{
  UInt v = val;
  return save_bytes(stream, &v, sizeof(UInt));
}

static size_t save_int(IOSTREAM *stream, int val)
{
  UInt v = val;
  return save_bytes(stream, &v, sizeof(int));
}

static size_t save_tag(IOSTREAM *stream, qlf_tag_t tag)
{
  return save_byte(stream, tag);
}

static int
SaveHash(IOSTREAM *stream)
{
  CACHE_REGS
  UInt i;
  /* first, current opcodes */
  CHECK(save_tag(stream, QLY_START_X));
  save_uint(stream, (UInt)&ARG1);
  CHECK(save_tag(stream, QLY_START_OPCODES));
  save_int(stream, _std_top);
  for (i= 0; i <= _std_top; i++) {
    save_uint(stream, (UInt)Yap_opcode(i));
  }
  CHECK(save_tag(stream, QLY_START_ATOMS));
  CHECK(save_uint(stream, LOCAL_ExportAtomHashTableNum));
  for (i = 0; i < LOCAL_ExportAtomHashTableSize; i++) {
    export_atom_hash_entry_t *a = LOCAL_ExportAtomHashChain[i];
    while (a) {
      export_atom_hash_entry_t *a0 = a;
      Atom at = a->val;
      CHECK(save_uint(stream, (UInt)at));
      if (IsWideAtom(at)) {
	CHECK(save_tag(stream, QLY_ATOM_WIDE));
	CHECK(save_uint(stream, wcslen(RepAtom(at)->WStrOfAE)));
	CHECK(save_bytes(stream, at->WStrOfAE, (wcslen(at->WStrOfAE)+1)*sizeof(wchar_t)));
      } else {
	CHECK(save_tag(stream, QLY_ATOM));
	CHECK(save_uint(stream, strlen(RepAtom(at)->StrOfAE)));
	CHECK(save_bytes(stream, at->StrOfAE, (strlen(at->StrOfAE)+1)*sizeof(char)));
      }
      a = a->next;
      free(a0);
    }
  }
  save_tag(stream, QLY_START_FUNCTORS);
  save_uint(stream, LOCAL_ExportFunctorHashTableNum);
  for (i = 0; i < LOCAL_ExportFunctorHashTableSize; i++) {
    export_functor_hash_entry_t *f = LOCAL_ExportFunctorHashChain[i];
    while (f) {
      export_functor_hash_entry_t *f0 = f;
      CHECK(save_uint(stream, (UInt)(f->val)));
      CHECK(save_uint(stream, f->arity));
      CHECK(save_uint(stream, (CELL)(f->name)));
      f = f->next;
      free(f0);
    }
  }
  save_tag(stream, QLY_START_PRED_ENTRIES);
  save_uint(stream, LOCAL_ExportPredEntryHashTableNum);
  for (i = 0; i < LOCAL_ExportPredEntryHashTableSize; i++) {
    export_pred_entry_hash_entry_t *p = LOCAL_ExportPredEntryHashChain[i];
    while (p) {
      export_pred_entry_hash_entry_t *p0 = p;
      CHECK(save_uint(stream, (UInt)(p->val)));
      CHECK(save_uint(stream, p->arity));
      CHECK(save_uint(stream, (UInt)p->module));
      CHECK(save_uint(stream, (UInt)p->u.f));
      p = p->next;
      free(p0);
    }
  }
  save_tag(stream, QLY_START_DBREFS);
  save_uint(stream, LOCAL_ExportDBRefHashTableNum);
  for (i = 0; i < LOCAL_ExportDBRefHashTableSize; i++) {
    export_dbref_hash_entry_t *p = LOCAL_ExportDBRefHashChain[i];
    while (p) {
      export_dbref_hash_entry_t *p0 = p;
      CHECK(save_uint(stream, (UInt)(p->val)));
      CHECK(save_uint(stream, p->sz));
      CHECK(save_uint(stream, p->refs));
      p = p->next;
      free(p0);
    }
  }
  save_tag(stream, QLY_FAILCODE);
  save_uint(stream, (UInt)FAILCODE);
  return 1;
}

static size_t
save_clauses(IOSTREAM *stream, PredEntry *pp) {
  yamop        *FirstC, *LastC;

  FirstC = pp->cs.p_code.FirstClause;
  LastC = pp->cs.p_code.LastClause;
  if (FirstC == NULL && LastC == NULL) {
    return 1;
  }
  if (pp->PredFlags & LogUpdatePredFlag) {
    LogUpdClause *cl = ClauseCodeToLogUpdClause(FirstC);

    while (cl != NULL) {
      if (pp->TimeStampOfPred >= cl->ClTimeStart &&
	  pp->TimeStampOfPred <= cl->ClTimeEnd) {
	UInt size = cl->ClSize;
	CHECK(save_tag(stream, QLY_START_LU_CLAUSE));
	CHECK(save_uint(stream, (UInt)cl));
	CHECK(save_uint(stream, size));
	CHECK(save_bytes(stream, cl, size));
      }
      cl = cl->ClNext;
    }
    CHECK(save_tag(stream, QLY_END_LU_CLAUSES));
  } else if (pp->PredFlags & MegaClausePredFlag) {
    MegaClause *cl = ClauseCodeToMegaClause(FirstC);
    UInt size = cl->ClSize;

    CHECK(save_uint(stream, (UInt)cl));
    CHECK(save_uint(stream, size));
    CHECK(save_bytes(stream, cl, size));
  } else if (pp->PredFlags & DynamicPredFlag) {
    yamop *cl = FirstC;

    do {
      DynamicClause *dcl = ClauseCodeToDynamicClause(cl);
      UInt size = dcl->ClSize;

      CHECK(save_uint(stream, (UInt)cl));
      CHECK(save_uint(stream, size));
      CHECK(save_bytes(stream, dcl, size));
      if (cl == LastC) return 1;
      cl = NextDynamicClause(cl);
    } while (TRUE);
  } else {
    StaticClause *cl = ClauseCodeToStaticClause(FirstC);

    if (pp->PredFlags & SYSTEM_PRED_FLAGS) {
      return 1;
    }
    do {
      UInt size = cl->ClSize;

      CHECK(save_uint(stream, (UInt)cl));
      CHECK(save_uint(stream, size));
      CHECK(save_bytes(stream, cl, size));
      if (cl->ClCode == LastC) return 1;
      cl = cl->ClNext;
    } while (TRUE);
  }
  return 1;
}

static size_t
save_pred(IOSTREAM *stream, PredEntry *ap) {
  CHECK(save_uint(stream, (UInt)ap));
  CHECK(save_uint(stream, ap->PredFlags));
  CHECK(save_uint(stream, ap->cs.p_code.NOfClauses));
  CHECK(save_uint(stream, ap->src.IndxId));
  return save_clauses(stream, ap);
}

static int
clean_pred(PredEntry *pp USES_REGS) {
  if (pp->PredFlags & (AsmPredFlag|CPredFlag)) {
    /* assembly */
    if (pp->CodeOfPred) {
      CleanClauses(pp->CodeOfPred, pp->CodeOfPred, pp PASS_REGS);
    }
  } else {
    CleanClauses(pp->cs.p_code.FirstClause, pp->cs.p_code.LastClause, pp PASS_REGS);
  }
  return TRUE;
}

static size_t
mark_pred(PredEntry *ap)
{
  CACHE_REGS
  if (ap->ModuleOfPred != IDB_MODULE) {
    if (ap->ArityOfPE) {
      FuncAdjust(ap->FunctorOfPred);
    } else {
      AtomAdjust((Atom)(ap->FunctorOfPred));
    }
  } else {
    if (ap->PredFlags & AtomDBPredFlag) {
      AtomAdjust((Atom)(ap->FunctorOfPred));
    } else if (!(ap->PredFlags & NumberDBPredFlag)) {
      FuncAdjust(ap->FunctorOfPred);
    }
  }
  if (!(ap->PredFlags & (MultiFileFlag|NumberDBPredFlag)) &&
      ap->src.OwnerFile) {
    AtomAdjust(ap->src.OwnerFile);
  }
  CHECK(clean_pred(ap PASS_REGS));
  return 1;
}

static size_t
mark_ops(IOSTREAM *stream, Term mod) {
  OpEntry *op = OpList;
  while (op) {
    if (!mod || op->OpModule == mod) {
      AtomAdjust(op->OpName);
      if (op->OpModule)
	AtomTermAdjust(op->OpModule);
    }
    op = op->OpNext;
  }
  return 1;
}

static size_t
save_ops(IOSTREAM *stream, Term mod) {
  OpEntry *op = OpList;
  while (op) {
    if (!mod || op->OpModule == mod) {
      CHECK(save_tag(stream, QLY_NEW_OP));
      save_uint(stream, (UInt)op->OpName);
      save_uint(stream, (UInt)op->OpModule);
      save_bits16(stream, op->Prefix);
      save_bits16(stream, op->Infix);
      save_bits16(stream, op->Posfix);
    }
    op = op->OpNext;
  }
  CHECK(save_tag(stream, QLY_END_OPS));
  return 1;
}

static size_t
save_module(IOSTREAM *stream, Term mod) {
  CACHE_REGS
  PredEntry *ap = Yap_ModulePred(mod);
  InitHash();
  ModuleAdjust(mod);
  while (ap) {
    ap = PredEntryAdjust(ap);
    CHECK(mark_pred(ap));
    ap = ap->NextPredOfModule;
  }
  /* just to make sure */
  mark_ops(stream, mod);
  SaveHash(stream);
  CHECK(save_tag(stream, QLY_START_MODULE));
  CHECK(save_uint(stream, (UInt)mod));
  ap = Yap_ModulePred(mod);
  while (ap) {
    CHECK(save_tag(stream, QLY_START_PREDICATE));
    CHECK(save_pred(stream, ap));
    ap = ap->NextPredOfModule;
  }
  CHECK(save_tag(stream, QLY_END_PREDICATES));
  CHECK(save_tag(stream, QLY_END_MODULES));
  save_ops(stream, mod);
  CloseHash();
  return 1;
}

static int
save_header(IOSTREAM *stream)
{
  char     msg[256];

  sprintf(msg, "#!/bin/sh\nexec_dir=${YAPBINDIR:-%s}\nexec $exec_dir/yap $0 \"$@\"\n%s", YAP_BINDIR, YAP_SVERSION);
  return save_bytes(stream, msg, strlen(msg)+1);
}

static size_t
save_program(IOSTREAM *stream) {
  CACHE_REGS
  ModEntry *me = CurrentModules;

  InitHash();
  save_header( stream );
  /* should we allow the user to see hidden predicates? */
  while (me) {
    PredEntry *pp;
    pp = me->PredForME;
    AtomAdjust(me->AtomOfME);
    while (pp != NULL) {
      pp = PredEntryAdjust(pp);
      CHECK(mark_pred(pp));
      pp = pp->NextPredOfModule;
    }
    me = me->NextME;
  }

  /* just to make sure */
  mark_ops(stream, 0);
  SaveHash(stream);
  me = CurrentModules;
  while (me) {
    PredEntry *pp;
    pp = me->PredForME;
    CHECK(save_tag(stream, QLY_START_MODULE));
    CHECK(save_uint(stream, (UInt)MkAtomTerm(me->AtomOfME)));
    while (pp != NULL) {
      CHECK(save_tag(stream, QLY_START_PREDICATE));
      CHECK(save_pred(stream, pp));
      pp = pp->NextPredOfModule;
    }
    CHECK(save_tag(stream, QLY_END_PREDICATES));
    me = me->NextME;
  }
  CHECK(save_tag(stream, QLY_END_MODULES));
  save_ops(stream, 0);
  CloseHash();
  return 1;
}

static Int
p_save_module_preds( USES_REGS1 )
{
  IOSTREAM *stream;
  Term tmod = Deref(ARG2);

  if (!Yap_getOutputStream(Yap_InitSlot(Deref(ARG1) PASS_REGS), &stream)) {
    return FALSE;
  }
  if (IsVarTerm(tmod)) {
    Yap_Error(INSTANTIATION_ERROR,tmod,"save_module/2");
    return FALSE;
  }
  if (!IsAtomTerm(tmod)) {
    Yap_Error(TYPE_ERROR_ATOM,tmod,"save_module/2");
    return FALSE;
  }
  return save_module(stream, tmod) != 0;
}

static Int
p_save_program( USES_REGS1 )
{
  IOSTREAM *stream;

  if (!Yap_getOutputStream(Yap_InitSlot(Deref(ARG1) PASS_REGS), &stream)) {
    return FALSE;
  }
  return save_program(stream) != 0;
}

void Yap_InitQLY(void)
{
  Yap_InitCPred("$qsave_module_preds", 2, p_save_module_preds, SyncPredFlag|HiddenPredFlag|UserCPredFlag);
  Yap_InitCPred("$qsave_program", 1, p_save_program, SyncPredFlag|HiddenPredFlag|UserCPredFlag);
  if (FALSE) {
    restore_codes();
  }
}

