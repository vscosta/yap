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

#include "config.h"
#if HAVE_STRING_H
#include <string.h>
#endif
#include <SWI-Stream.h>
#include <Yap.h>
#include <Yatom.h>
#include <clause.h>

#if DEBUG

#define NEXTOP(V,TYPE)    ((yamop *)(&((V)->u.TYPE.next)))

typedef enum {
  QLF_START_CLAUSE,
  QLF_END_CLAUSES,
  QLF_CONSTANT_INT,
  QLF_CONSTANT_ATOM,
  QLF_ATOM,
  QLF_WIDE_ATOM
} qlf_tag_t;

#define CHECK(F) { size_t r = (F); if (!r) return r; }

static size_t save_bytes(IOSTREAM *stream, void *ptr, size_t sz)
{
  return Sfwrite(ptr, sz, 1, stream);
}

static size_t restore_bytes(IOSTREAM *stream, void *ptr, size_t sz)
{
  return Sfread(ptr, sz, 1, stream);
}

static size_t save_uint(IOSTREAM *stream, UInt val)
{
  UInt v = val;
  return save_bytes(stream, &v, sizeof(UInt));
}

static UInt restore_uint(IOSTREAM *stream, context ctx)
{
  UInt v;

  restore_bytes(stream, &v, sizeof(UInt));
  return v; 
}

static size_t save_int(IOSTREAM *stream, Int val)
{
  Int v = val;
  return save_bytes(stream, &v, sizeof(Int));
}

static Int restore_int(IOSTREAM *stream, context ctx)
{
  UInt v;

  restore_bytes(stream, &v, sizeof(Int));
  return v; 
}

static size_t save_term(IOSTREAM *stream, Term t)
{
  CELL *oldH = H;
  H += 4096;
  size_t len = Yap_ExportTerm(t, (char *)oldH, sizeof(CELL)*4096);
  H = oldH;
  if (len <= 0) return 0;
  CHECK(save_uint(stream, len) );
  return save_bytes(stream, (char *)H, len);
}

static Term
restore_term(IOSTREAM *stream, context *ql)
{
  Term t;
  CELL *horig = H;
  CELL *start, *oldASP = ASP;
  UInt len = read_uint(stream, ql);
  start = ASP = H-(len/sizeof(CELL)+1);
  restore_bytes(stream, start, len);
  t = Yap_ImportTerm((char *)start);
  return t;
}

static size_t save_tag(IOSTREAM *stream, qlf_tag_t tag)
{
  return save_bytes(stream, &tag, sizeof(qlf_tag_t));
}

static qlf_tag_t
restore_tag(IOSTREAM *stream, context *ql)
{
  return save_bytes(stream, &tag, sizeof(qlf_tag_t));
}

static size_t save_pointer(IOSTREAM *stream, void *ptr)
{
  void *p = ptr;
  return save_bytes(stream, &p, sizeof(void *));
}

static size_t save_atom(IOSTREAM *stream, Atom at)
{
  if (IsWideAtom(at)) {
    size_t sz = wcslen(RepAtom(at)->WStrOfAE);
    CHECK(save_tag(stream, QLF_WIDE_ATOM));
    CHECK(save_uint(stream, sz));
    return save_bytes(stream, RepAtom(at)->WStrOfAE, (sz+1)*sizeof(wchar_t));
  } else {
    size_t sz = strlen(RepAtom(at)->StrOfAE);
    CHECK(save_tag(stream, QLF_ATOM));
    return save_bytes(stream, RepAtom(at)->StrOfAE, (sz+1)*sizeof(char));
  }
}

static size_t save_Arity(IOSTREAM *stream, Int a)
{
  return save_uint(stream, a);
}

static size_t save_CellPtoHeap(IOSTREAM *stream, CELL *ptr)
{
  return save_pointer(stream, ptr);
}

static size_t save_ConstantTerm(IOSTREAM *stream, Term t)
{
  if (IsIntTerm(t)) {
    CHECK(save_tag(stream, QLF_CONSTANT_INT));
    return save_int(stream, IntOfTerm(t));
  }
  CHECK(save_tag(stream, QLF_CONSTANT_ATOM));
  return save_atom(stream, AtomOfTerm(t));
}

static size_t save_DoubleInCode(IOSTREAM *stream, CELL *t)
{
  return save_bytes(stream, (void *)(t+1), sizeof(double));
}

static size_t save_Constant(IOSTREAM *stream, COUNT c)
{
  return save_bytes(stream, (void *)&c, sizeof(COUNT));
}

static size_t save_DBGroundTerm(IOSTREAM *stream, Term t)
{
  return save_term(stream, t);
}

static size_t save_Func(IOSTREAM *stream, Functor f)
{
  CHECK(save_atom(stream, NameOfFunctor(f)));
  return save_Arity(stream, ArityOfFunctor(f));
}

static size_t save_ExternalFunction(IOSTREAM *stream, CPredicate f)
{
  Yap_Error(INTERNAL_ERROR, TermNil, "trying to save an ExternalFunction");
  return 0;
}

static size_t save_IntegerInCode(IOSTREAM *stream, CELL *t)
{
  return save_int(stream, t[1]);
}

static size_t save_Integer(IOSTREAM *stream, Int i)
{
  return save_int(stream, i);
}

static size_t save_PtoLUIndex(IOSTREAM *stream, struct logic_upd_index *p)
{
  Yap_Error(INTERNAL_ERROR, TermNil, "trying to save PtoLUIndex");
  return 0;
}

static size_t save_PtoOp(IOSTREAM *stream, yamop *l)
{
  return save_pointer(stream, (void *)l);
}

static size_t save_PtoLUClause(IOSTREAM *stream, struct logic_upd_clause *t)
{
  Yap_Error(INTERNAL_ERROR, TermNil, "trying to save PtoLUIndex");
  return 0;
}

static size_t save_BlobTermInCode(IOSTREAM *stream, Term t)
{
  return save_pointer(stream, (void *)RepAppl(t));
}

static size_t save_Opcode(IOSTREAM *stream, op_numbers op)
{
  fprintf(stderr,"%d\n",op);
  return save_int(stream, Yap_op_from_opcode(op));
}

#ifdef YAPOR
static size_t save_OrArg(IOSTREAM *stream, unsigned int i)
{
  return save_uint(stream, i);
}
#endif /* YAPOR */

static size_t save_PtoPred(IOSTREAM *stream, struct pred_entry *ap)
{
  if (ap->ModuleOfPred) {
    CHECK(save_atom(stream, AtomOfTerm(ap->ModuleOfPred)));
  } else {
    CHECK(save_atom(stream, AtomProlog));
  }
  if (ap->ArityOfPE) {
    CHECK(save_int(stream, ap->ArityOfPE));
    return save_atom(stream, NameOfFunctor(ap->FunctorOfPred));
  } else {
    CHECK(save_int(stream, 0));
    return save_atom(stream, (Atom)(ap->FunctorOfPred));
  }
}

static size_t save_Module(IOSTREAM *stream, Term tmod)
{
  if (tmod == 0) {
    return save_atom(stream, AtomProlog);
  } else {
    return save_atom(stream, AtomOfTerm(tmod));
  }
}

#ifdef TABLING
static size_t save_TabEntry(IOSTREAM *stream, struct table_entry *ap)
{
  return save_pointer(stream, NULL);
}
#endif

#if PRECOMPUTE_REGADDRESS
#define arg_from_x(I)		(((CELL *)(I))-XREGS)
#else
#define arg_from_x(I)		(I)
#endif /* PRECOMPUTE_REGADDRESS */

static size_t save_X(IOSTREAM *stream, wamreg reg)
{
  return save_int(stream, arg_from_x(reg));
}

static size_t save_Y(IOSTREAM *stream, yslot reg)
{
  return save_int(stream, reg);
}

static size_t
save_code(IOSTREAM *stream, yamop *pc, yamop *max) {
#include "saveclause.h"
  if (max && max > pc) {
    return save_bytes(stream, pc, (char *)max-(char *)pc);
  }
  return 1;
}

static size_t
save_lu_clause(IOSTREAM *stream, LogUpdClause *cl) {
  CHECK(save_uint(stream, cl->ClSize));
  CHECK(save_uint(stream, cl->ClFlags));
  CHECK(save_tag(stream, QLF_START_CLAUSE));
  if (!(cl->ClFlags & FactMask)) {
    CHECK(save_term(stream, cl->ClSource->Entry));
  }
  return save_code(stream, cl->ClCode, (yamop *)cl->ClSource);
}

static size_t
save_dynamic_clause(IOSTREAM *stream, DynamicClause *cl) {
  CHECK(save_tag(stream, QLF_START_CLAUSE));
  return save_code(stream, cl->ClCode, NULL);
}

static size_t
save_static_clause(IOSTREAM *stream, StaticClause *cl, PredEntry *ap) {
  CHECK(save_uint(stream, cl->ClSize));
  CHECK(save_uint(stream, cl->ClFlags));
  CHECK(save_tag(stream, QLF_START_CLAUSE));
  if (!(cl->ClFlags & FactMask) &&
      (ap->PredFlags & SourcePredFlag)) {
    CHECK(save_term(stream, cl->usc.ClSource->Entry));
    return save_code(stream, cl->ClCode, (yamop *)(cl->usc.ClSource));
  } else {
    return save_code(stream, cl->ClCode, NULL);
  }
}

static size_t
save_mega_clause(IOSTREAM *stream, MegaClause *cl) {
  UInt i;
  yamop *ptr;
  UInt ncls = cl->ClPred->cs.p_code.NOfClauses;

  for (i = 0, ptr = cl->ClCode; i < ncls; i++) {
    yamop *nextptr = (yamop *)((char *)ptr + cl->ClItemSize);
    CHECK(save_tag(stream, QLF_START_CLAUSE));
    CHECK(save_code(stream, ptr, nextptr));
    ptr = nextptr;
  }
  return 1;
}

static size_t
save_clauses(IOSTREAM *stream, PredEntry *pp) {
  yamop        *FirstC, *LastC;

  FirstC = pp->cs.p_code.FirstClause;
  LastC = pp->cs.p_code.LastClause;
  if (FirstC == NULL && LastC == NULL) {
    return save_tag(stream, QLF_END_CLAUSES);
  }
  if (pp->PredFlags & LogUpdatePredFlag) {
    LogUpdClause *cl = ClauseCodeToLogUpdClause(FirstC);

    while (cl != NULL) {
      CHECK(save_lu_clause(stream, cl));
      cl = cl->ClNext;
    }
  } else if (pp->PredFlags & MegaClausePredFlag) {
    MegaClause *cl = ClauseCodeToMegaClause(FirstC);

    CHECK(save_mega_clause(stream, cl));
  } else if (pp->PredFlags & DynamicPredFlag) {
    yamop *cl = FirstC;

    do {
      CHECK(save_dynamic_clause(stream, ClauseCodeToDynamicClause(cl)));
      if (cl == LastC) return 1;
      cl = NextDynamicClause(cl);
    } while (TRUE);
  } else {
    StaticClause *cl = ClauseCodeToStaticClause(FirstC);

    do {
      CHECK(save_static_clause(stream, cl, pp));
      if (cl->ClCode == LastC) return 1;
      cl = cl->ClNext;
    } while (TRUE);
  }
  return save_tag(stream, QLF_END_CLAUSES);
}

static size_t
save_pred(IOSTREAM *stream, PredEntry *ap) {
  return walk_clauses(stream, ap);
  CHECK(save_Func(stream, ap->FunctorOfPred));
  CHECK(save_uint(stream, ap->ArityOfPE));
  CHECK(save_uint(stream, ap->PredFlags));
  CHECK(save_uint(stream, ap->cs.p_code.NOfClauses));
  return save_clauses(stream, ap);
}

static size_t
save_module(IOSTREAM *stream, Term mod) {
  PredEntry *ap = Yap_ModulePred(mod);
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

