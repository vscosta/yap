/*************************************************************************
 *									 *
 *	 YAP Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		cdmgr.c *
 * comments:	Code manager						 *
 *									 *
 * Last rev:     $Date: 2008-07-22 23:34:44 $,$Author: vsc $              8
 *************************************************************************/

#include "Yap.h"
#include "YapEval.h"
#include "clause.h"
#include "tracer.h"
#include "yapio.h"

#include <Yatom.h>
#include <assert.h>
#include <heapgc.h>
#include <iopreds.h>



#ifdef DEBUG
static UInt total_megaclause, total_released, nof_megaclauses;
#endif

/******************************************************************

                Mega Clauses

******************************************************************/

#define OrArgAdjust(P)
#define TabEntryAdjust(P)
#define DoubleInCodeAdjust(D)
#define IntegerInCodeAdjust(D)
#define IntegerAdjust(D) (D)
#define PtoPredAdjust(X) (X)
#define PtoOpAdjust(X) (X)
#define PtoLUClauseAdjust(P) (P)
#define PtoLUIndexAdjust(P) (P)
#define XAdjust(X) (X)
#define YAdjust(X) (X)
#define AtomTermAdjust(X) (X)
#define CellPtoHeapAdjust(X) (X)
#define FuncAdjust(X) (X)
#define CodeAddrAdjust(X) (X)
#define CodeComposedTermAdjust(X) (X)
#define ConstantAdjust(X) (X)
#define ArityAdjust(X) (X)
#define OpcodeAdjust(X) (X)
#define ModuleAdjust(X) (X)
#define ExternalFunctionAdjust(X) (X)
#define AdjustSwitchTable(X, Y, Z)
#define DBGroundTermAdjust(X) (X)
#define rehash(A, B, C)

static Term BlobTermInCodeAdjust(Term t) {
  CACHE_REGS
#if TAGS_FAST_OPS
  return t - LOCAL_ClDiff;
#else
  return t + LOCAL_ClDiff;
#endif
}

static Term ConstantTermAdjust(Term t) {
  if (IsAtomTerm(t))
    return AtomTermAdjust(t);
  return t;
}

#include "rclause.h"


void Yap_BuildMegaClause(PredEntry *ap) {
  CACHE_REGS
  StaticClause *cl;
  UInt sz;
  MegaClause *mcl;
  yamop *ptr;
  size_t required;
  UInt has_blobs = 0;

  if (ap->PredFlags & (DynamicPredFlag | LogUpdatePredFlag | MegaClausePredFlag
#ifdef TABLING
		       | TabledPredFlag
#endif /* TABLING */
		       | UDIPredFlag) ||
      ap->FirstClause == NULL || ap->NOfClauses < 16) {
    return;
  }
  cl = ClauseCodeToStaticClause(ap->FirstClause);
  sz = cl->ClSize;
  while (TRUE) {
    if (!(cl->ClFlags & FactMask))
      return; /* no mega clause, sorry */
    if (cl->ClSize != sz)
      return; /* no mega clause, sorry */
    if (cl->ClCode == ap->LastClause)
      break;
    has_blobs |= (cl->ClFlags & HasBlobsMask);
    cl = cl->ClNext;
  }
  /* ok, we got the chance for a mega clause */
  if (has_blobs) {
    sz -= sizeof(StaticClause);
  } else {
    sz -= (UInt)NEXTOP((yamop *)NULL, p) + sizeof(StaticClause);
  }
  required = sz * ap->NOfClauses + sizeof(MegaClause) +
             (UInt)NEXTOP((yamop *)NULL, l);
  while (!(mcl = (MegaClause *)Yap_AllocCodeSpace(required))) {
    if (!Yap_growheap(FALSE, required, NULL)) {
      /* just fail, the system will keep on going */
      return;
    }
  }
#ifdef DEBUG
  total_megaclause += required;
  cl = ClauseCodeToStaticClause(ap->FirstClause);
  total_released += ap->NOfClauses * cl->ClSize;
  nof_megaclauses++;
#endif
  Yap_ClauseSpace += required;
  /* cool, it's our turn to do the conversion */
  mcl->ClFlags = MegaMask | has_blobs;
  mcl->ClSize = required;
  mcl->ClPred = ap;
  mcl->ClItemSize = sz;
  mcl->ClNext = NULL;
  cl = ClauseCodeToStaticClause(ap->FirstClause);
  mcl->ClLine = cl->usc.ClLine;
  ptr = mcl->ClCode;
  while (TRUE) {
    memmove((void *)ptr, (void *)cl->ClCode, sz);
    if (has_blobs) {
      LOCAL_ClDiff = (char *)(ptr) - (char *)cl->ClCode;
      restore_opcodes(ptr, NULL PASS_REGS);
    }
    ptr = (yamop *)((char *)ptr + sz);
    if (cl->ClCode == ap->LastClause)
      break;
    cl = cl->ClNext;
  }
  ptr->opc = Yap_opcode(_Ystop);
  cl = ClauseCodeToStaticClause(ap->FirstClause);
  /* recover the space spent on the original clauses */
  while (TRUE) {
    StaticClause *ncl, *curcl = cl;

    ncl = cl->ClNext;
    Yap_InformOfRemoval(cl);
    Yap_ClauseSpace -= cl->ClSize;
    Yap_FreeCodeSpace((ADDR)cl);
    if (curcl->ClCode == ap->LastClause)
      break;
    cl = ncl;
  }
  ap->FirstClause = ap->LastClause = mcl->ClCode;
  ap->PredFlags |= MegaClausePredFlag;
  Yap_inform_profiler_of_clause(mcl, (char *)mcl + required, ap, GPROF_MEGA);
}

void Yap_split_megaclause(PredEntry *ap) {
  StaticClause *start = NULL, *prev = NULL;
  MegaClause *mcl;
  yamop *ptr;
  UInt ncls = ap->NOfClauses, i;

  mcl = ClauseCodeToMegaClause(ap->FirstClause);
  if (mcl->ClFlags & ExoMask) {
    Yap_Error(PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE, Yap_PredicateToIndicator(ap),
              "while deleting clause from exo predicate %s/%d\n",
              RepAtom(NameOfFunctor(ap->FunctorOfPred))->StrOfAE,
              ap->ArityOfPE);
    return;
  }
  for (i = 0, ptr = mcl->ClCode; i < ncls; i++) {
    StaticClause *new = (StaticClause *)Yap_AllocCodeSpace(
        sizeof(StaticClause) + mcl->ClItemSize +
        (UInt)NEXTOP((yamop *)NULL, p));
    if (new == NULL) {
      if (!Yap_growheap(FALSE,
                        (sizeof(StaticClause) + mcl->ClItemSize) * (ncls - i),
                        NULL)) {
        while (start) {
          StaticClause *cl = start;
          start = cl->ClNext;
          Yap_InformOfRemoval(cl);
          Yap_ClauseSpace -= cl->ClSize;
          Yap_FreeCodeSpace((char *)cl);
        }
        if (ap->ArityOfPE) {
          Yap_Error(RESOURCE_ERROR_HEAP, TermNil,
                    "while breaking up mega clause for %s/%d\n",
                    RepAtom(NameOfFunctor(ap->FunctorOfPred))->StrOfAE,
                    ap->ArityOfPE);
        } else {
          Yap_Error(RESOURCE_ERROR_HEAP, TermNil,
                    "while breaking up mega clause for %s\n",
                    RepAtom((Atom)ap->FunctorOfPred)->StrOfAE);
        }
        return;
      }
      break;
    }
    Yap_ClauseSpace +=
        sizeof(StaticClause) + mcl->ClItemSize + (UInt)NEXTOP((yamop *)NULL, p);
    new->ClFlags = StaticMask | FactMask;
    new->ClSize = mcl->ClItemSize;
    new->usc.ClLine = Yap_source_line_no();
    new->ClNext = NULL;
    memmove((void *)new->ClCode, (void *)ptr, mcl->ClItemSize);
    if (prev) {
      prev->ClNext = new;
    } else {
      start = new;
    }
    ptr = (yamop *)((char *)ptr + mcl->ClItemSize);
    prev = new;
  }
  ap->PredFlags &= ~MegaClausePredFlag;
  ap->FirstClause = start->ClCode;
  ap->LastClause = prev->ClCode;
}


static UInt compute_dbcl_size(arity_t arity) {
  UInt sz;
  switch (arity) {
  case 2:
    sz = (UInt)NEXTOP((yamop *)NULL, cc);
    break;
  case 3:
    sz = (UInt)NEXTOP((yamop *)NULL, ccc);
    break;
  case 4:
    sz = (UInt)NEXTOP((yamop *)NULL, cccc);
    break;
  case 5:
    sz = (UInt)NEXTOP((yamop *)NULL, ccccc);
    break;
  case 6:
    sz = (UInt)NEXTOP((yamop *)NULL, cccccc);
    break;
  default:
    sz = arity * (UInt)NEXTOP((yamop *)NULL, xc);
    break;
  }
  return (UInt)NEXTOP((yamop *)sz, p);
}

#define DerefAndCheck(t, V)                                                    \
  t = Deref(V);                                                                \
  if (IsVarTerm(t) || !(IsAtomOrIntTerm(t)))                                   \
    Yap_Error(TYPE_ERROR_ATOM, t0, "load_db");

static int store_dbcl_size(yamop *pc, arity_t arity, Term t0, PredEntry *pe) {
  Term t;
  CELL *tp = RepAppl(t0) + 1;
  switch (arity) {
  case 2:
    pc->opc = Yap_opcode(_get_2atoms);
    DerefAndCheck(t, tp[0]);
    pc->y_u.cc.c1 = t;
    DerefAndCheck(t, tp[1]);
    pc->y_u.cc.c2 = t;
    pc = NEXTOP(pc, cc);
    break;
  case 3:
    pc->opc = Yap_opcode(_get_3atoms);
    DerefAndCheck(t, tp[0]);
    pc->y_u.ccc.c1 = t;
    DerefAndCheck(t, tp[1]);
    pc->y_u.ccc.c2 = t;
    DerefAndCheck(t, tp[2]);
    pc->y_u.ccc.c3 = t;
    pc = NEXTOP(pc, ccc);
    break;
  case 4:
    pc->opc = Yap_opcode(_get_4atoms);
    DerefAndCheck(t, tp[0]);
    pc->y_u.cccc.c1 = t;
    DerefAndCheck(t, tp[1]);
    pc->y_u.cccc.c2 = t;
    DerefAndCheck(t, tp[2]);
    pc->y_u.cccc.c3 = t;
    DerefAndCheck(t, tp[3]);
    pc->y_u.cccc.c4 = t;
    pc = NEXTOP(pc, cccc);
    break;
  case 5:
    pc->opc = Yap_opcode(_get_5atoms);
    DerefAndCheck(t, tp[0]);
    pc->y_u.ccccc.c1 = t;
    DerefAndCheck(t, tp[1]);
    pc->y_u.ccccc.c2 = t;
    DerefAndCheck(t, tp[2]);
    pc->y_u.ccccc.c3 = t;
    DerefAndCheck(t, tp[3]);
    pc->y_u.ccccc.c4 = t;
    DerefAndCheck(t, tp[4]);
    pc->y_u.ccccc.c5 = t;
    pc = NEXTOP(pc, ccccc);
    break;
  case 6:
    pc->opc = Yap_opcode(_get_6atoms);
    DerefAndCheck(t, tp[0]);
    pc->y_u.cccccc.c1 = t;
    DerefAndCheck(t, tp[1]);
    pc->y_u.cccccc.c2 = t;
    DerefAndCheck(t, tp[2]);
    pc->y_u.cccccc.c3 = t;
    DerefAndCheck(t, tp[3]);
    pc->y_u.cccccc.c4 = t;
    DerefAndCheck(t, tp[4]);
    pc->y_u.cccccc.c5 = t;
    DerefAndCheck(t, tp[5]);
    pc->y_u.cccccc.c6 = t;
    pc = NEXTOP(pc, cccccc);
    break;
  default: {
    arity_t i;
    for (i = 0; i < arity; i++) {
      pc->opc = Yap_opcode(_get_atom);
#if PRECOMPUTE_REGADDRESS
      pc->y_u.xc.x = (CELL)(XREGS + (i + 1));
#else
      pc->y_u.xc.x = i + 1;
#endif
      DerefAndCheck(t, tp[0]);
      pc->y_u.xc.c = t;
      tp++;
      pc = NEXTOP(pc, xc);
    }
  } break;
  }
  pc->opc = Yap_opcode(_procceed);
  pc->y_u.p.p = pe;
  return TRUE;
}

static Int
    p_dbload_get_space(USES_REGS1) { /* '$number_of_clauses'(Predicate,M,N) */
  Term t = Deref(ARG1);
  Term mod = Deref(ARG2);
  Term tn = Deref(ARG3);
  arity_t arity;
  Prop pe;
  PredEntry *ap;
  UInt sz;
  MegaClause *mcl;
  yamop *ptr;
  UInt ncls;
  UInt required;

  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return (FALSE);
  }
  if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    arity = 0;
    pe = PredPropByAtom(a, mod);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    arity = ArityOfFunctor(f);
    pe = PredPropByFunc(f, mod);
  } else {
    return FALSE;
  }
  if (EndOfPAEntr(pe))
    return FALSE;
  ap = RepPredProp(pe);
  if (ap->PredFlags & (DynamicPredFlag | LogUpdatePredFlag
#ifdef TABLING
                       | TabledPredFlag
#endif /* TABLING */
                       )) {
    Yap_Error(PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE,  Yap_PredicateToIndicator(ap),
              "dbload_get_space/4");
    return FALSE;
  }
  if (IsVarTerm(tn) || !IsIntegerTerm(tn)) {
    return FALSE;
  }
  ncls = IntegerOfTerm(tn);
  if (ncls <= 1) {
    return FALSE;
  }

  sz = compute_dbcl_size(arity);
  required = sz * ncls + sizeof(MegaClause) + (UInt)NEXTOP((yamop *)NULL, l);
#ifdef DEBUG
  total_megaclause += required;
  nof_megaclauses++;
#endif
  while (!(mcl = (MegaClause *)Yap_AllocCodeSpace(required))) {
    if (!Yap_growheap(FALSE, required, NULL)) {
      /* just fail, the system will keep on going */
      return FALSE;
    }
  }
  Yap_ClauseSpace += required;
  /* cool, it's our turn to do the conversion */
  mcl->ClFlags = MegaMask;
  mcl->ClSize = sz * ncls;
  mcl->ClPred = ap;
  mcl->ClItemSize = sz;
  mcl->ClNext = NULL;
  ap->FirstClause = ap->LastClause = mcl->ClCode;
  ap->PredFlags |= (MegaClausePredFlag);
  ap->NOfClauses = ncls;
  if (ap->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
    ap->OpcodeOfPred = Yap_opcode(_spy_pred);
  } else {
    ap->OpcodeOfPred = INDEX_OPCODE;
  }
  ap->CodeOfPred = ap->TrueCodeOfPred =
      (yamop *)(&(ap->OpcodeOfPred));
  ptr = (yamop *)((ADDR)mcl->ClCode + ncls * sz);
  ptr->opc = Yap_opcode(_Ystop);
  return Yap_unify(ARG4, MkIntegerTerm((Int)mcl));
}

static Int p_dbassert(USES_REGS1) { /* '$number_of_clauses'(Predicate,M,N) */
  Term thandle = Deref(ARG2);
  Term tn = Deref(ARG3);
  PredEntry *pe;
  MegaClause *mcl;
  Int n;

  if (IsVarTerm(thandle) || !IsIntegerTerm(thandle)) {
    return FALSE;
  }
  mcl = (MegaClause *)IntegerOfTerm(thandle);
  if (IsVarTerm(tn) || !IsIntegerTerm(tn)) {
    return FALSE;
  }
  n = IntegerOfTerm(tn);
  pe = mcl->ClPred;
  return store_dbcl_size((yamop *)((ADDR)mcl->ClCode + n * (mcl->ClItemSize)),
                         pe->ArityOfPE, Deref(ARG1), pe);
}

void Yap_InitDBLoadPreds(void) {
  CACHE_REGS
    //CurrentModule = DBLOAD_MODULE;
  Yap_InitCPred("$dbload_get_space", 4, p_dbload_get_space, 0L);
  Yap_InitCPred("$dbassert", 3, p_dbassert, 0L);
  //CurrentModule = cm;
}
