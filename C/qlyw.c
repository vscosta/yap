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
* File:		stdpreds.c						 *
* comments:	quick saver/loader					 *
*									 *
* Last rev:     $Date: 2011-08-29$,$Author: vsc $			 *
* $Log: not supported by cvs2svn $					 *
*									 *
*************************************************************************/

#if DEBUG

#include "absmi.h"
#include "Foreign.h"
#include "alloc.h"
#include "yapio.h"
#include "iopreds.h"
#include "attvar.h"
#if HAVE_STRING_H
#include <string.h>
#endif
#include <SWI-Stream.h>

STATIC_PROTO(void  RestoreEntries, (PropEntry *, int USES_REGS));
STATIC_PROTO(void  CleanCode, (PredEntry * USES_REGS));

#define EXPORT_ATOM_TABLE_SIZE (16*4096)
#define EXPORT_FUNCTOR_TABLE_SIZE (16*4096)

typedef struct export_atom_hash_entry_struct {
  Atom val;
  struct  export_atom_hash_entry_struct *next;
} export_atom_hash_entry_t;

static void
LookupAtom(Atom at)
{
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
  fprintf(stderr,"+%s\n",RepAtom(at)->StrOfAE);
  a->next = LOCAL_ExportAtomHashChain[hash];
  LOCAL_ExportAtomHashChain[hash] = a;
  LOCAL_ExportAtomHashTableNum++;
}

typedef struct export_functor_hash_entry_struct {
  Atom name;
  UInt arity;
  struct  export_functor_hash_entry_struct *next;
} export_functor_hash_entry_t;

static void
LookupFunctor(Functor fun)
{
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
  f->name = name;
  f->arity = arity;
  f->next = LOCAL_ExportFunctorHashChain[hash];
  LOCAL_ExportFunctorHashChain[hash] = f;
  LOCAL_ExportFunctorHashTableNum++;
}

static void
InitHash(void)
{
  LOCAL_ExportFunctorHashTableSize = EXPORT_FUNCTOR_TABLE_SIZE;
  LOCAL_ExportFunctorHashChain = (export_functor_hash_entry_t **)calloc(1, sizeof(export_functor_hash_entry_t *)* LOCAL_ExportFunctorHashTableSize);
  LOCAL_ExportAtomHashTableSize = EXPORT_ATOM_TABLE_SIZE;
  LOCAL_ExportAtomHashChain = (export_atom_hash_entry_t **)calloc(1, sizeof(export_atom_hash_entry_t *)* LOCAL_ExportAtomHashTableSize);
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
#define ModuleAdjust(P) (P)
#define ExternalFunctionAdjust(P) (P)
#define DBRecordAdjust(P) (P)
#define PredEntryAdjust(P) (P)
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
#define DBRefAdjust(P) (P)
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
#define PtoPredAdjust(P) (P)
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


static void init_reg_copies(USES_REGS1)
{
}


static void
RestoreAtomList(Atom atm USES_REGS)
{
}

static void
mark_trail(USES_REGS1)
{
}

static void
mark_registers(USES_REGS1)
{
}

#define NEXTOP(V,TYPE)    ((yamop *)(&((V)->u.TYPE.next)))

typedef enum {
  QLY_START_PREDICATE,
  QLY_END_PREDICATE,
  QLY_START_CLAUSE,
  QLY_END_CLAUSES,
  QLY_FUNCTORS,
  QLY_ATOMS,
  QLY_ATOM_WIDE,
  QLY_ATOM
} qlf_tag_t;

#define CHECK(F) { size_t r = (F); if (!r) return r; }

static size_t save_bytes(IOSTREAM *stream, void *ptr, size_t sz)
{
  return Sfwrite(ptr, sz, 1, stream);
}

static size_t save_byte(IOSTREAM *stream, int byte)
{
  return Sputc(byte, stream);
}

static size_t save_uint(IOSTREAM *stream, UInt val)
{
  UInt v = val;
  return save_bytes(stream, &v, sizeof(UInt));
}

static size_t save_tag(IOSTREAM *stream, qlf_tag_t tag)
{
  return save_byte(stream, tag);
}

static int
SaveHash(IOSTREAM *stream)
{
  UInt i;
  CHECK(save_tag(stream, QLY_ATOMS));
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
	CHECK(save_bytes(stream, at->WStrOfAE, wcslen(at->WStrOfAE)*sizeof(wchar_t)));
      } else {
	CHECK(save_tag(stream, QLY_ATOM));
	CHECK(save_uint(stream, strlen(RepAtom(at)->StrOfAE)));
	CHECK(save_bytes(stream, at->StrOfAE, (strlen(at->StrOfAE)+1)*sizeof(char)));
      }
      a = a->next;
      free(a0);
    }
  }
  save_tag(stream, QLY_FUNCTORS);
  save_uint(stream, LOCAL_ExportFunctorHashTableNum);
  for (i = 0; i < LOCAL_ExportFunctorHashTableSize; i++) {
    export_functor_hash_entry_t *f = LOCAL_ExportFunctorHashChain[i];
    while (f) {
      export_functor_hash_entry_t *f0 = f;
      CHECK(save_uint(stream, f->arity));
      CHECK(save_uint(stream, (CELL)(f->name)));
      f = f->next;
      free(f0);
    }
  }
}

static size_t
save_clauses(IOSTREAM *stream, PredEntry *pp) {
  yamop        *FirstC, *LastC;

  FirstC = pp->cs.p_code.FirstClause;
  LastC = pp->cs.p_code.LastClause;
  if (FirstC == NULL && LastC == NULL) {
    return save_tag(stream, QLY_END_CLAUSES);
  }
  if (pp->PredFlags & LogUpdatePredFlag) {
    LogUpdClause *cl = ClauseCodeToLogUpdClause(FirstC);

    while (cl != NULL) {
      UInt size = cl->ClSize;
      CHECK(save_bytes(stream, cl, size));
      cl = cl->ClNext;
    }
  } else if (pp->PredFlags & MegaClausePredFlag) {
    MegaClause *cl = ClauseCodeToMegaClause(FirstC);
    UInt size = cl->ClSize;

    CHECK(save_bytes(stream, cl, size));
  } else if (pp->PredFlags & DynamicPredFlag) {
    yamop *cl = FirstC;

    do {
      DynamicClause *dcl = ClauseCodeToDynamicClause(cl);
      UInt size = dcl->ClSize;

      CHECK(save_bytes(stream, dcl, size));
      if (cl == LastC) return 1;
      cl = NextDynamicClause(cl);
    } while (TRUE);
  } else {
    StaticClause *cl = ClauseCodeToStaticClause(FirstC);

    do {
      UInt size = cl->ClSize;

      CHECK(save_bytes(stream, cl, size));
      if (cl->ClCode == LastC) return 1;
      cl = cl->ClNext;
    } while (TRUE);
  }
  return save_tag(stream, QLY_END_CLAUSES);
}

static size_t
save_pred(IOSTREAM *stream, PredEntry *ap) {
  CHECK(save_uint(stream, (UInt)(ap->FunctorOfPred)));
  CHECK(save_uint(stream, ap->ArityOfPE));
  CHECK(save_uint(stream, ap->PredFlags));
  CHECK(save_uint(stream, ap->cs.p_code.NOfClauses));
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
save_module(IOSTREAM *stream, Term mod) {
  CACHE_REGS
  PredEntry *ap = Yap_ModulePred(mod);
  InitHash();
  while (ap) {
    fprintf(stderr,"P %s\n",RepAtom(NameOfFunctor(ap->FunctorOfPred))->StrOfAE);
    if (ap->ArityOfPE) {
      FuncAdjust(ap->FunctorOfPred);
    } else {
      AtomAdjust((Atom)(ap->FunctorOfPred));
    }
    CHECK(clean_pred(ap PASS_REGS));
    ap = ap->NextPredOfModule;
  }
  SaveHash(stream);
  ap = Yap_ModulePred(mod);
  while (ap) {
    CHECK(save_pred(stream, ap));
    ap = ap->NextPredOfModule;
  }
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

#endif

void Yap_InitQLY(void)
{
#if DEBUG
  Yap_InitCPred("$save_module_preds", 2, p_save_module_preds, SyncPredFlag|HiddenPredFlag|UserCPredFlag);
#endif
}

