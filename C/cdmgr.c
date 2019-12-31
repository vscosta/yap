/***********************************************************************
 *									 *
 *	 YAP Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *									 * *
 ***************************************************************************
 *									 *
 * File:		cdmgr.c *
 * comments:	Code manager						 *
 *									 *
 * Last rev:     $Date: 2008-07-22 23:34:44 $,$Author: vsc $              8
 *************************************************************************/

#ifdef SCCS
static char SccsId[] = "@(#)cdmgr.c	1.1 05/02/98";
#endif

#include "Yap.h"
#include "YapEval.h"
#include "clause.h"
#include "tracer.h"
#include "yapio.h"
#ifdef YAPOR
#include "or.macros.h"
#endif /* YAPOR */
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#if HAVE_STRING_H
#include <string.h>
#endif
#include <Yatom.h>
#include <assert.h>
#include <heapgc.h>
#include <iopreds.h>

static void retract_all(PredEntry *, int);
static void add_first_static(PredEntry *, yamop *, int);
static void add_first_dynamic(PredEntry *, yamop *, int);
static void asserta_stat_clause(PredEntry *, yamop *, int);
static void asserta_dynam_clause(PredEntry *, yamop *);
static void assertz_stat_clause(PredEntry *, yamop *, int);
static void assertz_dynam_clause(PredEntry *, yamop *);
static void expand_consult(void);
static int not_was_reconsulted(PredEntry *, Term, int);
static int RemoveIndexation(PredEntry *);
static Int number_of_clauses(USES_REGS1);
static Int p_compile(USES_REGS1);
static Int p_purge_clauses(USES_REGS1);
static Int p_setspy(USES_REGS1);
static Int p_rmspy(USES_REGS1);
static Int p_startconsult(USES_REGS1);
static Int p_showconslultlev(USES_REGS1);
static Int p_endconsult(USES_REGS1);
static Int p_undefined(USES_REGS1);
static Int new_multifile(USES_REGS1);
static Int p_is_multifile(USES_REGS1);
static Int p_optimizer_on(USES_REGS1);
static Int p_optimizer_off(USES_REGS1);
static Int p_is_dynamic(USES_REGS1);
static Int p_kill_dynamic(USES_REGS1);
static Int p_is_profiled(USES_REGS1);
static Int p_profile_info(USES_REGS1);
static Int p_profile_reset(USES_REGS1);
static Int p_is_call_counted(USES_REGS1);
static Int p_call_count_info(USES_REGS1);
static Int p_call_count_set(USES_REGS1);
static Int p_call_count_reset(USES_REGS1);
static void kill_first_log_iblock(LogUpdIndex *, LogUpdIndex *, PredEntry *);

#define PredArity(p) (p->ArityOfPE)
#define TRYCODE(G, F, N) ((N) < 5 ? (op_numbers)((int)F + (N)*3) : G)

PredEntry *Yap_get_pred(Term t, Term tmod, const char *pname) {
  Term t0 = t;

restart:
  if (IsVarTerm(t)) {
    if (strcmp(pname, "predicate_exists"))
      Yap_ThrowError(INSTANTIATION_ERROR, t0, pname);
    return NULL;
  } else if (IsAtomTerm(t)) {
    PredEntry *ap = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(t), tmod));
    return ap;
  } else if (IsIntegerTerm(t) && tmod == IDB_MODULE) {
    return Yap_FindLUIntKey(IntegerOfTerm(t));
  } else if (IsPairTerm(t)) {
    t = Yap_MkApplTerm(FunctorCsult, 1, &t);
    goto restart;
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    if (IsExtensionFunctor(fun)) {
      Yap_ThrowError(TYPE_ERROR_CALLABLE, t, pname);
      return NULL;
    }
    if (fun == FunctorModule) {
      Term tmod = ArgOfTerm(1, t);
      if (IsVarTerm(tmod)) {
        Yap_ThrowError(INSTANTIATION_ERROR, t0, pname);
        return NULL;
      }
      if (!IsAtomTerm(tmod)) {
        Yap_ThrowError(TYPE_ERROR_ATOM, t0, pname);
        return NULL;
      }
      t = ArgOfTerm(2, t);
      goto restart;
    }
    PredEntry *ap = RepPredProp(Yap_GetPredPropByFunc(fun, tmod));
    return ap;
  } else {
    if (strcmp(pname, "predicate_exists"))
      Yap_ThrowError(TYPE_ERROR_CALLABLE, t0, pname);
  }
  return NULL;
}

static void InitConsultStack(void) {
  CACHE_REGS
  LOCAL_ConsultLow = (consult_obj *)Yap_AllocCodeSpace(sizeof(consult_obj) *
                                                       InitialConsultCapacity);
  if (LOCAL_ConsultLow == NULL) {
    Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "No Heap Space in InitCodes");
    return;
  }
  LOCAL_ConsultCapacity = InitialConsultCapacity;
  LOCAL_ConsultBase = LOCAL_ConsultSp =
      LOCAL_ConsultLow + LOCAL_ConsultCapacity;
}

void Yap_ResetConsultStack(void) {
  CACHE_REGS
  Yap_FreeCodeSpace((char *)LOCAL_ConsultLow);
  LOCAL_ConsultBase = LOCAL_ConsultSp = LOCAL_ConsultLow = NULL;
  LOCAL_ConsultCapacity = InitialConsultCapacity;
}

/**
 * Are we compiling a file?
 *
 */
bool Yap_Consulting(USES_REGS1) {
  return LOCAL_ConsultBase != NULL &&
         LOCAL_ConsultSp != LOCAL_ConsultLow + LOCAL_ConsultCapacity;
}

/******************************************************************

                ADDING AND REMOVE INFO TO A PROCEDURE

******************************************************************/

/**
 * we have three kinds of predicates:
 * + dynamic		DynamicPredFlag
 * + static	CompiledPredFlag fast
 * + fast		FastPredFlag.
 *
 * all the
 * database predicates are supported for dynamic predicates only abolish and
 * assertz are supported for static predicates no database predicates are
 * supportted for fast predicates
 */

/** Look for a predicate with same functor as t,
     create a new one of it cannot find it.
 */
static PredEntry *new_pred(Term t, Term tmod, char *pname) {
  Term t0 = t;

restart:
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t0, pname);
    return NULL;
  } else if (IsAtomTerm(t)) {
    return RepPredProp(PredPropByAtom(AtomOfTerm(t), tmod));
  } else if (IsIntegerTerm(t) && tmod == IDB_MODULE) {
    return Yap_FindLUIntKey(IntegerOfTerm(t));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    if (IsExtensionFunctor(fun)) {
      Yap_Error(TYPE_ERROR_CALLABLE, Yap_TermToIndicator(t, tmod), pname);
      return NULL;
    }
    if (fun == FunctorModule) {
      Term tmod = ArgOfTerm(1, t);
      if (IsVarTerm(tmod)) {
        Yap_Error(INSTANTIATION_ERROR, t0, pname);
        return NULL;
      }
      if (!IsAtomTerm(tmod)) {
        Yap_Error(TYPE_ERROR_ATOM, t0, pname);
        return NULL;
      }
      t = ArgOfTerm(2, t);
      goto restart;
    }
    return RepPredProp(PredPropByFunc(fun, tmod));
  } else
    return NULL;
}

/******************************************************************

                Indexation Info

******************************************************************/
#define ByteAdr(X) ((Int) & (X))

/* Index a prolog pred, given its predicate entry */
/* ap is already locked. */
static void IPred(PredEntry *ap, UInt NSlots, yamop *next_pc) {
  yamop *BaseAddr;

#ifdef DEBUG
  CACHE_REGS
  if (GLOBAL_Option['i' - 'a' + 1]) {
    Term tmod = ap->ModuleOfPred;
    if (!tmod)
      tmod = TermProlog;
    Yap_DebugPutc(stderr, '\t');
    Yap_DebugPlWrite(tmod);
    Yap_DebugPutc(stderr, ':');
    if (ap->ModuleOfPred == IDB_MODULE) {
      Term t = Deref(ARG1);
      if (IsAtomTerm(t)) {
        Yap_DebugPlWrite(t);
      } else if (IsIntegerTerm(t)) {
        Yap_DebugPlWrite(t);
      } else {
        Functor f = FunctorOfTerm(t);
        Atom At = NameOfFunctor(f);
        Yap_DebugPlWrite(MkAtomTerm(At));
        Yap_DebugPutc(stderr, '/');
        Yap_DebugPlWrite(MkIntTerm(ArityOfFunctor(f)));
      }
    } else {
      if (ap->ArityOfPE == 0) {
        Atom At = (Atom)ap->FunctorOfPred;
        Yap_DebugPlWrite(MkAtomTerm(At));
      } else {
        Functor f = ap->FunctorOfPred;
        Atom At = NameOfFunctor(f);
        Yap_DebugPlWrite(MkAtomTerm(At));
        Yap_DebugPutc(stderr, '/');
        Yap_DebugPlWrite(MkIntTerm(ArityOfFunctor(f)));
      }
    }
    Yap_DebugPutc(stderr, '\n');
  }
#endif
  /* Do not try to index a dynamic predicate  or one whithout args */
  if (is_dynamic(ap)) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "trying to index a dynamic predicate");
    return;
  }
   if ((BaseAddr = Yap_PredIsIndexable(ap, NSlots, next_pc)) != NULL) {
       ap->TrueCodeOfPred = BaseAddr;
   ap->PredFlags |= IndexedPredFlag;
  }
  if (ap->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
    if (ap->PredFlags & ProfiledPredFlag) {
      Yap_initProfiler(ap);
    }
    ap->OpcodeOfPred = Yap_opcode(_spy_pred);
    ap->CodeOfPred = (yamop *)(&(ap->OpcodeOfPred));
#if defined(YAPOR) || defined(THREADS)
  } else if (ap->PredFlags & LogUpdatePredFlag &&
             !(ap->PredFlags & ThreadLocalPredFlag) &&
             ap->ModuleOfPred != IDB_MODULE) {
    ap->OpcodeOfPred = LOCKPRED_OPCODE;
    ap->CodeOfPred = (yamop *)(&(ap->OpcodeOfPred));
#endif
  } else {
    ap->CodeOfPred = ap->TrueCodeOfPred;
    ap->OpcodeOfPred = ap->CodeOfPred->opc;
  }
#ifdef DEBUG
  if (GLOBAL_Option['i' - 'a' + 1])
    Yap_DebugPutc(stderr, '\n');
#endif
}

void Yap_IPred(PredEntry *p, UInt NSlots, yamop *next_pc) {
  IPred(p, NSlots, next_pc);
}

#define GONEXT(TYPE) code_p = ((yamop *)(&(code_p->y_u.TYPE.next)))

static void RemoveMainIndex(PredEntry *ap) {
  yamop *First = ap->FirstClause;
  int spied =
      ap->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag);

  ap->PredFlags &= ~IndexedPredFlag;
  if (First == NULL) {
    ap->TrueCodeOfPred = FAILCODE;
  } else {
    ap->TrueCodeOfPred = First;
  }
  if (First != NULL && spied) {
    ap->OpcodeOfPred = Yap_opcode(_spy_pred);
    ap->CodeOfPred = (yamop *)(&(ap->OpcodeOfPred));
  } else if (ap->NOfClauses > 1
#ifdef TABLING
             || ap->PredFlags & TabledPredFlag
#endif /* TABLING */
  ) {
    ap->OpcodeOfPred = INDEX_OPCODE;
    ap->CodeOfPred = ap->TrueCodeOfPred =
        (yamop *)(&(ap->OpcodeOfPred));
  } else {
    ap->OpcodeOfPred = ap->TrueCodeOfPred->opc;
    ap->CodeOfPred = ap->TrueCodeOfPred;
  }
#if defined(YAPOR) || defined(THREADS)
  if (ap->PredFlags & LogUpdatePredFlag &&
      !(ap->PredFlags & ThreadLocalPredFlag) &&
      ap->ModuleOfPred != IDB_MODULE) {
    ap->OpcodeOfPred = LOCKPRED_OPCODE;
    ap->CodeOfPred = (yamop *)(&(ap->OpcodeOfPred));
  }
#endif
}

static void decrease_ref_counter(yamop *ptr, yamop *b, yamop *e, yamop *sc) {
  if (ptr != FAILCODE && ptr != sc && (ptr < b || ptr > e)) {
    LogUpdClause *cl = ClauseCodeToLogUpdClause(ptr);
    cl->ClRefCount--;
    if (cl->ClFlags & ErasedMask && !(cl->ClRefCount) &&
        !(cl->ClFlags & InUseMask)) {
      /* last ref to the clause */
      Yap_ErLogUpdCl(cl);
    }
  }
}

static yamop *release_wcls(yamop *cop, OPCODE ecs) {
  if (cop->opc == ecs) {
    cop->y_u.sssllp.s3--;
    if (!cop->y_u.sssllp.s3) {
      UInt sz = (UInt)NEXTOP((yamop *)NULL, sssllp) +
                cop->y_u.sssllp.s1 * sizeof(yamop *);
      LOCK(ExpandClausesListLock);
#ifdef DEBUG
      Yap_expand_clauses_sz -= sz;
      Yap_ExpandClauses--;
#endif
      if (cop->y_u.sssllp.p->PredFlags & LogUpdatePredFlag) {
        Yap_LUIndexSpace_EXT -= sz;
      } else {
        Yap_IndexSpace_EXT -= sz;
      }
      if (ExpandClausesFirst == cop)
        ExpandClausesFirst = cop->y_u.sssllp.snext;
      if (ExpandClausesLast == cop) {
        ExpandClausesLast = cop->y_u.sssllp.sprev;
      }
      if (cop->y_u.sssllp.sprev) {
        cop->y_u.sssllp.sprev->y_u.sssllp.snext = cop->y_u.sssllp.snext;
      }
      if (cop->y_u.sssllp.snext) {
        cop->y_u.sssllp.snext->y_u.sssllp.sprev = cop->y_u.sssllp.sprev;
      }
      UNLOCK(ExpandClausesListLock);
      Yap_InformOfRemoval(cop);
      Yap_FreeCodeSpace((char *)cop);
    }
  }
  return FAILCODE;
}

static void cleanup_dangling_indices(yamop *ipc, yamop *beg, yamop *end,
                                     yamop *suspend_code) {
  OPCODE ecs = Yap_opcode(_expand_clauses);

  while (ipc) {
    op_numbers op = Yap_op_from_opcode(ipc->opc);
    /*    fprintf(stderr,"op: %d %p->%p\n", op, ipc, end);*/
    switch (op) {
    case _Ystop:
      /* end of clause, for now */
      return;
    case _index_dbref:
    case _index_blob:
    case _index_long:
      ipc = NEXTOP(ipc, e);
      break;
    case _lock_lu:
    case _unlock_lu:
      /* locking should be done already */
      ipc = NEXTOP(ipc, e);
    case _retry_profiled:
    case _count_retry:
      ipc = NEXTOP(ipc, p);
      break;
    case _try_clause2:
    case _try_clause3:
    case _try_clause4:
      ipc = NEXTOP(ipc, l);
      break;
    case _retry2:
    case _retry3:
    case _retry4:
      decrease_ref_counter(ipc->y_u.l.l, beg, end, suspend_code);
      ipc = NEXTOP(ipc, l);
      break;
    case _retry:
    case _trust:
      decrease_ref_counter(ipc->y_u.Otapl.d, beg, end, suspend_code);
      ipc = NEXTOP(ipc, Otapl);
      break;
    case _try_clause:
    case _try_me:
    case _retry_me:
    case _profiled_trust_me:
    case _trust_me:
    case _count_trust_me:
      ipc = NEXTOP(ipc, Otapl);
      break;
    case _try_logical:
    case _retry_logical:
    case _count_retry_logical:
    case _profiled_retry_logical: {
      yamop *oipc = ipc;
      decrease_ref_counter(ipc->y_u.OtaLl.d->ClCode, beg, end, suspend_code);
      ipc = ipc->y_u.OtaLl.n;
      Yap_LUIndexSpace_CP -= (UInt)NEXTOP((yamop *)NULL, OtaLl);
      Yap_FreeCodeSpace((ADDR)oipc);
#ifdef DEBUG
      Yap_DirtyCps--;
      Yap_FreedCps++;
#endif
    } break;
    case _trust_logical:
    case _count_trust_logical:
    case _profiled_trust_logical:
#ifdef DEBUG
      Yap_DirtyCps--;
      Yap_FreedCps++;
#endif
      decrease_ref_counter(ipc->y_u.OtILl.d->ClCode, beg, end, suspend_code);
      Yap_LUIndexSpace_CP -= (UInt)NEXTOP((yamop *)NULL, OtILl);
      Yap_FreeCodeSpace((ADDR)ipc);
      return;
    case _enter_lu_pred: {
      yamop *oipc = ipc;
      if (ipc->y_u.Illss.I->ClFlags & InUseMask || ipc->y_u.Illss.I->ClRefCount)
        return;
#ifdef DEBUG
      Yap_DirtyCps += ipc->y_u.Illss.s;
      Yap_LiveCps -= ipc->y_u.Illss.s;
#endif
      ipc = ipc->y_u.Illss.l1;
      /* in case we visit again */
      oipc->y_u.Illss.l1 = FAILCODE;
      oipc->y_u.Illss.s = 0;
      oipc->y_u.Illss.e = 0;
    } break;
    case _try_in:
    case _jump:
    case _jump_if_var:
      ipc->y_u.l.l = release_wcls(ipc->y_u.l.l, ecs);
      ipc = NEXTOP(ipc, l);
      break;
    /* instructions type xl */
    case _jump_if_nonvar:
      ipc->y_u.xll.l1 = release_wcls(ipc->y_u.xll.l1, ecs);
      ipc = NEXTOP(ipc, xll);
      break;
    /* instructions type p */
    case _user_switch:
      ipc = NEXTOP(ipc, lp);
      break;
    /* instructions type e */
    case _switch_on_type:
      ipc->y_u.llll.l1 = release_wcls(ipc->y_u.llll.l1, ecs);
      ipc->y_u.llll.l2 = release_wcls(ipc->y_u.llll.l2, ecs);
      ipc->y_u.llll.l3 = release_wcls(ipc->y_u.llll.l3, ecs);
      ipc->y_u.llll.l4 = release_wcls(ipc->y_u.llll.l4, ecs);
      ipc = NEXTOP(ipc, llll);
      break;
    case _switch_list_nl:
      ipc->y_u.ollll.l1 = release_wcls(ipc->y_u.ollll.l1, ecs);
      ipc->y_u.ollll.l2 = release_wcls(ipc->y_u.ollll.l2, ecs);
      ipc->y_u.ollll.l3 = release_wcls(ipc->y_u.ollll.l3, ecs);
      ipc->y_u.ollll.l4 = release_wcls(ipc->y_u.ollll.l4, ecs);
      ipc = NEXTOP(ipc, ollll);
      break;
    case _switch_on_arg_type:
      ipc->y_u.xllll.l1 = release_wcls(ipc->y_u.xllll.l1, ecs);
      ipc->y_u.xllll.l2 = release_wcls(ipc->y_u.xllll.l2, ecs);
      ipc->y_u.xllll.l3 = release_wcls(ipc->y_u.xllll.l3, ecs);
      ipc->y_u.xllll.l4 = release_wcls(ipc->y_u.xllll.l4, ecs);
      ipc = NEXTOP(ipc, xllll);
      break;
    case _switch_on_sub_arg_type:
      ipc->y_u.sllll.l1 = release_wcls(ipc->y_u.sllll.l1, ecs);
      ipc->y_u.sllll.l2 = release_wcls(ipc->y_u.sllll.l2, ecs);
      ipc->y_u.sllll.l3 = release_wcls(ipc->y_u.sllll.l3, ecs);
      ipc->y_u.sllll.l4 = release_wcls(ipc->y_u.sllll.l4, ecs);
      ipc = NEXTOP(ipc, sllll);
      break;
    case _if_not_then:
      ipc = NEXTOP(ipc, clll);
      break;
    case _switch_on_func:
    case _if_func:
    case _go_on_func:
    case _switch_on_cons:
    case _if_cons:
    case _go_on_cons:
      /* make sure we don't leave dangling references to memory that is going to
       * be removed */
      ipc->y_u.sssl.l = NULL;
      ipc = NEXTOP(ipc, sssl);
      break;
    case _op_fail:
      return;
    default:
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "Bug in Indexing Code: opcode %d", op);
      return;
    }
#if defined(YAPOR) || defined(THREADS)
    ipc = (yamop *)((CELL)ipc & ~1);
#endif
  }
}

static void decrease_log_indices(LogUpdIndex *c, yamop *suspend_code) {
  /* decrease all reference counters */
  yamop *beg = c->ClCode, *end, *ipc;

  if (c->ClFlags & SwitchTableMask) {
    CELL *end = (CELL *)((char *)c + c->ClSize);
    CELL *beg = (CELL *)(c->ClCode);
    OPCODE ecs = Yap_opcode(_expand_clauses);

    while (beg < end) {
      yamop **x = (yamop **)(beg + 1);
      beg += 2;
      *x = release_wcls(*x, ecs);
    }
    return;
  }
  end = (yamop *)((CODEADDR)c + c->ClSize);
  ipc = beg;
  cleanup_dangling_indices(ipc, beg, end, suspend_code);
}

static void kill_static_child_indxs(StaticIndex *indx, int in_use) {
  StaticIndex *cl = indx->ChildIndex;
  while (cl != NULL) {
    StaticIndex *next = cl->SiblingIndex;
    kill_static_child_indxs(cl, in_use);
    cl = next;
  }
  if (in_use) {
    LOCK(DeadStaticIndicesLock);
    indx->SiblingIndex = DeadStaticIndices;
    indx->ChildIndex = NULL;
    DeadStaticIndices = indx;
    UNLOCK(DeadStaticIndicesLock);
  } else {
    Yap_InformOfRemoval(indx);
    if (indx->ClFlags & SwitchTableMask)
      Yap_IndexSpace_SW -= indx->ClSize;
    else
      Yap_IndexSpace_Tree -= indx->ClSize;
    Yap_FreeCodeSpace((char *)indx);
  }
}

static void kill_children(LogUpdIndex *c, PredEntry *ap) {
  LogUpdIndex *ncl;

  c->ClRefCount++;
  ncl = c->ChildIndex;
  /* kill children */
  while (ncl) {
    //    if (ncl->ClRefCount>0) {
    //  c->ClRefCount--;
    // return;
    //    }
    kill_first_log_iblock(ncl, c, ap);
    ncl = c->ChildIndex;
  }
  c->ClRefCount--;
}

/* assumes c is already locked */
static void kill_off_lu_block(LogUpdIndex *c, LogUpdIndex *parent,
                              PredEntry *ap) {
  /* first, make sure that I killed off all my children, some children may
     remain in case I have tables as children */
  if (parent != NULL) {
    /* sat bye bye */
    /* decrease refs */
    parent->ClRefCount--;
    if (parent->ClFlags & ErasedMask && !(parent->ClFlags & InUseMask) &&
        parent->ClRefCount == 0) {
      /* cool, I can erase the father too. */
      if (parent->ClFlags & SwitchRootMask) {
        kill_off_lu_block(parent, NULL, ap);
      } else {
        kill_off_lu_block(parent, parent->ParentIndex, ap);
      }
    }
  }
  decrease_log_indices(c, (yamop *)&(ap->cs.p_code.ExpandCode));
  /* remove from list */
  if (c->SiblingIndex)
    c->SiblingIndex->PrevSiblingIndex = c->PrevSiblingIndex;
  if (c->PrevSiblingIndex) {
    c->PrevSiblingIndex->SiblingIndex = c->SiblingIndex;
  } else {
    DBErasedIList = c->SiblingIndex;
  }
  Yap_InformOfRemoval(c);
  if (c->ClFlags & SwitchTableMask)
    Yap_LUIndexSpace_SW -= c->ClSize;
  else {
    Yap_LUIndexSpace_Tree -= c->ClSize;
  }
  Yap_FreeCodeSpace((char *)c);
}

static void kill_first_log_iblock(LogUpdIndex *c, LogUpdIndex *parent,
                                  PredEntry *ap) {
  /* parent is always locked, now I lock myself */
  if (parent != NULL) {
    /* remove myself from parent */
    if (c == parent->ChildIndex) {
      parent->ChildIndex = c->SiblingIndex;
      if (parent->ChildIndex) {
        parent->ChildIndex->PrevSiblingIndex = NULL;
      }
    } else {
      c->PrevSiblingIndex->SiblingIndex = c->SiblingIndex;
      if (c->SiblingIndex) {
        c->SiblingIndex->PrevSiblingIndex = c->PrevSiblingIndex;
      }
    }
  } else {
  }
  decrease_log_indices(c, (yamop *)&(ap->cs.p_code.ExpandCode));
  /* make sure that a child cannot remove us */
  kill_children(c, ap);
  /* check if we are still the main index */
  /* always add to erased list */
  c->SiblingIndex = DBErasedIList;
  c->PrevSiblingIndex = NULL;
  if (!(c->ClFlags & InUseMask || c->ClRefCount)) {
    kill_off_lu_block(c, parent, ap);
  } else {
    if (c->ClFlags & ErasedMask)
      return;
    c->ClFlags |= ErasedMask;
    if (DBErasedIList)
      DBErasedIList->PrevSiblingIndex = c;
    DBErasedIList = c;
    /* try to move up, so that we don't hold a switch table */
    if (parent != NULL && parent->ClFlags & SwitchTableMask) {

      c->ParentIndex = parent->ParentIndex;
      parent->ParentIndex->ClRefCount++;
      parent->ClRefCount--;
    }
  }
  /* I am  top node */
  if (ap->TrueCodeOfPred == c->ClCode) {
    RemoveMainIndex(ap);
  }
}

static void kill_top_static_iblock(StaticIndex *c, PredEntry *ap) {
  kill_static_child_indxs(c, Yap_static_in_use(ap, TRUE));
  RemoveMainIndex(ap);
}

void Yap_kill_iblock(ClauseUnion *blk, ClauseUnion *parent_blk, PredEntry *ap) {
  if (ap->PredFlags & LogUpdatePredFlag) {
    LogUpdIndex *c = (LogUpdIndex *)blk;
    if (parent_blk != NULL) {
      LogUpdIndex *cl = (LogUpdIndex *)parent_blk;
#if MULTIPLE_STACKS
      /* protect against attempts at erasing */
      cl->ClRefCount++;
#endif
      kill_first_log_iblock(c, cl, ap);
#if MULTIPLE_STACKS
      cl->ClRefCount--;
#endif
    } else {
      kill_first_log_iblock(c, NULL, ap);
    }
  } else {
    StaticIndex *c = (StaticIndex *)blk;
    if (parent_blk != NULL) {
      StaticIndex *cl = parent_blk->si.ChildIndex;
      if (cl == c) {
        parent_blk->si.ChildIndex = c->SiblingIndex;
      } else {
        while (cl->SiblingIndex != c) {
          cl = cl->SiblingIndex;
        }
        cl->SiblingIndex = c->SiblingIndex;
      }
    }
    kill_static_child_indxs(c, Yap_static_in_use(ap, TRUE));
  }
}

/*
  This predicate is supposed to be called with a
  lock on the current predicate
*/
void Yap_ErLogUpdIndex(LogUpdIndex *clau) {
  if (clau->ClFlags & ErasedMask) {
    if (!clau->ClRefCount) {
      decrease_log_indices(clau,
                           (yamop *)&(clau->ClPred->cs.p_code.ExpandCode));
      if (clau->ClFlags & SwitchRootMask) {
        kill_off_lu_block(clau, NULL, clau->ClPred);
      } else {
        kill_off_lu_block(clau, clau->ParentIndex, clau->ClPred);
      }
    }
    /* otherwise, nothing I can do, I have been erased already */
    return;
  }
  if (clau->ClFlags & SwitchRootMask) {
    kill_first_log_iblock(clau, NULL, clau->ClPred);
  } else {
#if MULTIPLE_STACKS
    /* protect against attempts at erasing */
    clau->ClRefCount++;
#endif
    kill_first_log_iblock(clau, clau->ParentIndex, clau->ClPred);
#if MULTIPLE_STACKS
    /* protect against attempts at erasing */
    clau->ClRefCount--;
#endif
  }
}

/* Routine used when wanting to remove the indexation */
/* ap is known to already have been locked for WRITING */
static int RemoveIndexation(PredEntry *ap) {
  if (ap->OpcodeOfPred == INDEX_OPCODE) {
    return TRUE;
  }
  if (ap->PredFlags & LogUpdatePredFlag) {
    kill_first_log_iblock(ClauseCodeToLogUpdIndex(ap->TrueCodeOfPred),
                          NULL, ap);
  } else {
    StaticIndex *cl;

    cl = ClauseCodeToStaticIndex(ap->TrueCodeOfPred);

    kill_top_static_iblock(cl, ap);
  }
  return TRUE;
}

int Yap_RemoveIndexation(PredEntry *ap) { return RemoveIndexation(ap); }
/******************************************************************

                        Adding clauses

******************************************************************/

#define assertz 0
#define consult 1
#define asserta 2

/* p is already locked */
static void retract_all(PredEntry *p, int in_use) {
  yamop *q;

  q = p->FirstClause;
  if (q != NULL) {
    if (p->PredFlags & LogUpdatePredFlag) {
      LogUpdClause *cl = ClauseCodeToLogUpdClause(q);
      do {
        LogUpdClause *ncl = cl->ClNext;
        Yap_ErLogUpdCl(cl);
        cl = ncl;
      } while (cl != NULL);
    } else if (p->PredFlags & MegaClausePredFlag) {
      MegaClause *cl = ClauseCodeToMegaClause(q);

      if (in_use || cl->ClFlags & HasBlobsMask) {
        LOCK(DeadMegaClausesLock);
        cl->ClNext = DeadMegaClauses;
        DeadMegaClauses = cl;
        UNLOCK(DeadMegaClausesLock);
      } else {
        Yap_InformOfRemoval(cl);
        Yap_ClauseSpace -= cl->ClSize;
        Yap_FreeCodeSpace((char *)cl);
      }
      /* make sure this is not a MegaClause */
      p->PredFlags &= ~MegaClausePredFlag;
      p->NOfClauses = 0;
    } else {
      StaticClause *cl = ClauseCodeToStaticClause(q);

      while (cl) {
        StaticClause *ncl = cl->ClNext;

        if (in_use || cl->ClFlags & HasBlobsMask) {
          LOCK(DeadStaticClausesLock);
          cl->ClNext = DeadStaticClauses;
          DeadStaticClauses = cl;
          UNLOCK(DeadStaticClausesLock);
        } else {
          Yap_InformOfRemoval(cl);
          Yap_ClauseSpace -= cl->ClSize;
          Yap_FreeCodeSpace((char *)cl);
        }
        p->NOfClauses--;
        if (!ncl)
          break;
        cl = ncl;
      }
    }
  }
  p->FirstClause = NULL;
  p->LastClause = NULL;
  if (is_live(p)) {
    p->OpcodeOfPred = FAIL_OPCODE;
  } else {
    p->OpcodeOfPred = UNDEF_OPCODE;
    p->PredFlags |= UndefPredFlag;
  }
  p->TrueCodeOfPred = p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
  if (trueGlobalPrologFlag(PROFILING_FLAG)) {
    p->PredFlags |= ProfiledPredFlag;
    if (!Yap_initProfiler(p)) {
      return;
    }
  } else
    p->PredFlags &= ~ProfiledPredFlag;
  if (CALL_COUNTING) {
    p->PredFlags |= CountPredFlag;
  } else
    p->PredFlags &= ~CountPredFlag;
  Yap_PutValue(AtomAbol, MkAtomTerm(AtomTrue));
}

bool Yap_unknown(Term t) {

  if (t == TermFastFail) {
    UndefCode->OpcodeOfPred = FAIL_OPCODE;
    return true;
  } else if (t == TermError) {
    UndefCode->OpcodeOfPred = UndefCode->CodeOfPred->opc;
    return true;
  } else if (t == TermFail) {
    UndefCode->OpcodeOfPred = UndefCode->CodeOfPred->opc;
    return true;
  } else if (t == TermWarning) {
    UndefCode->OpcodeOfPred = UndefCode->CodeOfPred->opc;
    return true;
  }

  return false;
}

static int source_pred(PredEntry *p, yamop *q) {
  if (p->PredFlags & (DynamicPredFlag | LogUpdatePredFlag))
    return FALSE;
  if (p->PredFlags & MultiFileFlag)
    return TRUE;
  if (trueGlobalPrologFlag(SOURCE_FLAG)) {
    return TRUE;
  }
  return FALSE;
}

/* p is already locked */
static void add_first_static(PredEntry *p, yamop *cp, int spy_flag) {
  CACHE_REGS
  yamop *pt = cp;

#ifdef TABLING
  if (is_tabled(p)) {
    p->OpcodeOfPred = INDEX_OPCODE;
    p->TrueCodeOfPred = p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
  }
#endif /* TABLING */
  p->TrueCodeOfPred = pt;
  p->FirstClause = p->LastClause = cp;
  p->OpcodeOfPred = pt->opc;
#if defined(YAPOR) || defined(THREADS)
  if (p->PredFlags & LogUpdatePredFlag &&
      !(p->PredFlags & ThreadLocalPredFlag) && p->ModuleOfPred != IDB_MODULE) {
    p->OpcodeOfPred = LOCKPRED_OPCODE;
    p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
  } else
#endif
    p->CodeOfPred = pt;
  p->NOfClauses = 1;
  if (trueGlobalPrologFlag(PROFILING_FLAG)) {
    p->PredFlags |= ProfiledPredFlag;
    if (!Yap_initProfiler(p)) {
      return;
    }
    spy_flag = TRUE;
  } else {
    p->PredFlags &= ~ProfiledPredFlag;
  }
  if (CALL_COUNTING) {
    p->PredFlags |= CountPredFlag;
    spy_flag = TRUE;
  } else {
    p->PredFlags &= ~CountPredFlag;
  }
  if (spy_flag) {
    p->OpcodeOfPred = Yap_opcode(_spy_pred);
    p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
  }
  if (source_pred(p, cp)) {
    p->PredFlags |= SourcePredFlag;
  }
  if (!(p->PredFlags & MultiFileFlag) && p->src.OwnerFile == AtomNil)
    p->src.OwnerFile = Yap_ConsultingFile(PASS_REGS1);
}

/* p is already locked */
static void add_first_dynamic(PredEntry *p, yamop *cp, int spy_flag) {
  yamop *ncp = ((DynamicClause *)NULL)->ClCode;
  DynamicClause *cl;

  if (trueGlobalPrologFlag(PROFILING_FLAG)) {
    p->PredFlags |= ProfiledPredFlag;
    if (!Yap_initProfiler(p)) {
      return;
    }
    spy_flag = true;
  } else {
    p->PredFlags &= ~ProfiledPredFlag;
  }
  if (CALL_COUNTING) {
    p->PredFlags |= CountPredFlag;
    spy_flag = true;
  } else {
    p->PredFlags &= ~CountPredFlag;
  }
#ifdef YAPOR
  p->PredFlags |= SequentialPredFlag;
#endif /* YAPOR */
       /* allocate starter block, containing info needed to start execution,
        * that is a try_mark to start the code and a fail to finish things up */
  cl = (DynamicClause *)Yap_AllocCodeSpace(
      (Int)NEXTOP(NEXTOP(NEXTOP(ncp, Otapl), e), l));
  if (cl == NIL) {
    Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "Heap crashed against Stacks");
    return;
  }
  Yap_ClauseSpace += (Int)NEXTOP(NEXTOP(NEXTOP(ncp, Otapl), e), l);
  /* skip the first entry, this contains the back link and will always be
     empty for this entry */
  ncp = (yamop *)(((CELL *)ncp) + 1);
  /* next we have the flags. For this block mainly say whether we are
   *  being spied */
  cl->ClFlags = DynamicMask;
  ncp = cl->ClCode;
  INIT_LOCK(cl->ClLock);
  INIT_CLREF_COUNT(cl);
  /* next, set the first instruction to execute in the dyamic
   *  predicate */
  if (spy_flag)
    p->OpcodeOfPred = ncp->opc = Yap_opcode(_spy_or_trymark);
  else
    p->OpcodeOfPred = ncp->opc = Yap_opcode(_try_and_mark);
  ncp->y_u.Otapl.s = p->ArityOfPE;
  ncp->y_u.Otapl.p = p;
  ncp->y_u.Otapl.d = cp;
  /* This is the point we enter the code */
  p->TrueCodeOfPred = p->CodeOfPred = ncp;
  p->NOfClauses = 1;
#if defined(YAPOR) || defined(THREADS)
  if (p->PredFlags & LogUpdatePredFlag &&
      !(p->PredFlags & ThreadLocalPredFlag) && p->ModuleOfPred != IDB_MODULE) {
    p->OpcodeOfPred = LOCKPRED_OPCODE;
    p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
  }
#endif
  /* set the first clause to have a retry and mark which will
   *  backtrack to the previous block */
  if (p->PredFlags & ProfiledPredFlag)
    cp->opc = Yap_opcode(_profiled_retry_and_mark);
  else if (p->PredFlags & CountPredFlag)
    cp->opc = Yap_opcode(_count_retry_and_mark);
  else
    cp->opc = Yap_opcode(_retry_and_mark);
  cp->y_u.Otapl.s = p->ArityOfPE;
  cp->y_u.Otapl.p = p;
  cp->y_u.Otapl.d = ncp;
  /* also, keep a backpointer for the days you delete the clause */
  ClauseCodeToDynamicClause(cp)->ClPrevious = ncp;
  /* Don't forget to say who is the only clause for the predicate so
     far */
  p->LastClause = p->FirstClause = cp;
  /* we're only missing what to do when we actually exit the procedure
   */
  ncp = NEXTOP(ncp, Otapl);
  /* and the last instruction to execute to exit the predicate, note
     the retry is pointing to this pseudo clause */
  ncp->opc = Yap_opcode(_trust_fail);
  /* we're only missing what to do when we actually exit the procedure
   */
  /* and close the code */
  ncp = NEXTOP(ncp, e);
  ncp->opc = Yap_opcode(_Ystop);
  ncp->y_u.l.l = cl->ClCode;
  if (!(p->PredFlags & MultiFileFlag) && p->src.OwnerFile == AtomNil)
    p->src.OwnerFile = Yap_ConsultingFile(PASS_REGS1);
}

/* p is already locked */
static void asserta_stat_clause(PredEntry *p, yamop *q, int spy_flag) {
  StaticClause *cl = ClauseCodeToStaticClause(q);

  p->NOfClauses++;
  if (is_logupd(p)) {
    LogUpdClause *clp = ClauseCodeToLogUpdClause(p->FirstClause),
                 *clq = ClauseCodeToLogUpdClause(q);
    clq->ClPrev = NULL;
    clq->ClNext = clp;
    clp->ClPrev = clq;
    p->FirstClause = q;
    if (p->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
      p->OpcodeOfPred = Yap_opcode(_spy_pred);
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    } else if (!(p->PredFlags & IndexedPredFlag)) {
      p->OpcodeOfPred = INDEX_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    }
#if defined(YAPOR) || defined(THREADS)
    if (p->ModuleOfPred != IDB_MODULE &&
        !(p->PredFlags & ThreadLocalPredFlag)) {
      p->OpcodeOfPred = LOCKPRED_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    }
#endif
    return;
  }
  cl->ClNext = ClauseCodeToStaticClause(p->FirstClause);
  p->FirstClause = q;
  p->TrueCodeOfPred = q;
  if (p->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
    p->OpcodeOfPred = Yap_opcode(_spy_pred);
    p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
  } else if (!(p->PredFlags & IndexedPredFlag)) {
    p->OpcodeOfPred = INDEX_OPCODE;
    p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
  }
}

/* p is already locked */
static void asserta_dynam_clause(PredEntry *p, yamop *cp) {
  yamop *q;
  DynamicClause *cl = ClauseCodeToDynamicClause(cp);
  q = cp;
  LOCK(ClauseCodeToDynamicClause(p->FirstClause)->ClLock);
  /* also, keep backpointers for the days we'll delete all the clause */
  ClauseCodeToDynamicClause(p->FirstClause)->ClPrevious = q;
  cl->ClPrevious = (yamop *)(p->CodeOfPred);
  cl->ClFlags |= DynamicMask;
  UNLOCK(ClauseCodeToDynamicClause(p->FirstClause)->ClLock);
  q->y_u.Otapl.d = p->FirstClause;
  q->y_u.Otapl.s = p->ArityOfPE;
  q->y_u.Otapl.p = p;
  if (p->PredFlags & ProfiledPredFlag)
    cp->opc = Yap_opcode(_profiled_retry_and_mark);
  else if (p->PredFlags & CountPredFlag)
    cp->opc = Yap_opcode(_count_retry_and_mark);
  else
    cp->opc = Yap_opcode(_retry_and_mark);
  cp->y_u.Otapl.s = p->ArityOfPE;
  cp->y_u.Otapl.p = p;
  p->FirstClause = cp;
  q = p->CodeOfPred;
  q->y_u.Otapl.d = cp;
  q->y_u.Otapl.s = p->ArityOfPE;
  q->y_u.Otapl.p = p;
}

/* p is already locked */
static void assertz_stat_clause(PredEntry *p, yamop *cp, int spy_flag) {
  yamop *pt;

  p->NOfClauses++;
  pt = p->LastClause;
  if (is_logupd(p)) {
    LogUpdClause *clp = ClauseCodeToLogUpdClause(cp),
                 *clq = ClauseCodeToLogUpdClause(pt);

    clq->ClNext = clp;
    clp->ClPrev = clq;
    clp->ClNext = NULL;
    p->LastClause = cp;
    if (!(p->PredFlags & IndexedPredFlag)) {
      p->OpcodeOfPred = INDEX_OPCODE;
      p->TrueCodeOfPred = p->CodeOfPred =
          (yamop *)(&(p->OpcodeOfPred));
    }
#if defined(YAPOR) || defined(THREADS)
    if (p->ModuleOfPred != IDB_MODULE &&
        !(p->PredFlags & ThreadLocalPredFlag)) {
      p->OpcodeOfPred = LOCKPRED_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    }
#endif
    if (p->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
      p->OpcodeOfPred = Yap_opcode(_spy_pred);
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    }
    return;
  } else {
    StaticClause *cl = ClauseCodeToStaticClause(pt);

    cl->ClNext = ClauseCodeToStaticClause(cp);
  }
  if (p->FirstClause == p->LastClause) {
    if (!(p->PredFlags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag))) {
      p->OpcodeOfPred = INDEX_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    }
  }
  p->LastClause = cp;
}

/* p is already locked */
static void assertz_dynam_clause(PredEntry *p, yamop *cp) {
  yamop *q;
  DynamicClause *cl = ClauseCodeToDynamicClause(cp);

  q = p->LastClause;
  LOCK(ClauseCodeToDynamicClause(q)->ClLock);
  q->y_u.Otapl.d = cp;
  p->LastClause = cp;
  /* also, keep backpointers for the days we'll delete all the clause */
  cl->ClPrevious = q;
  cl->ClFlags |= DynamicMask;
  UNLOCK(ClauseCodeToDynamicClause(q)->ClLock);
  q = (yamop *)cp;
  if (p->PredFlags & ProfiledPredFlag)
    q->opc = Yap_opcode(_profiled_retry_and_mark);
  else if (p->PredFlags & CountPredFlag)
    q->opc = Yap_opcode(_count_retry_and_mark);
  else
    q->opc = Yap_opcode(_retry_and_mark);
  q->y_u.Otapl.d = p->CodeOfPred;
  q->y_u.Otapl.s = p->ArityOfPE;
  q->y_u.Otapl.p = p;
  p->NOfClauses++;
}

void Yap_AssertzClause(PredEntry *p, yamop *cp) {
  if (p->PredFlags & DynamicPredFlag) {
    if (p->FirstClause == NULL) {
      add_first_dynamic(p, cp, FALSE);
    } else {
      assertz_dynam_clause(p, cp);
    }
  } else {
    if (p->FirstClause == NULL) {
      add_first_static(p, cp, FALSE);
    } else {
      assertz_stat_clause(p, cp, FALSE);
    }
  }
}

static void expand_consult(void) {
  CACHE_REGS
  consult_obj *new_cl, *new_cs;
  UInt OldConsultCapacity = LOCAL_ConsultCapacity;

  /* now double consult capacity */
  LOCAL_ConsultCapacity += InitialConsultCapacity;
  /* I assume it always works ;-) */
  while ((new_cl = (consult_obj *)Yap_AllocCodeSpace(
              sizeof(consult_obj) * LOCAL_ConsultCapacity)) == NULL) {
    if (!Yap_growheap(FALSE, sizeof(consult_obj) * LOCAL_ConsultCapacity,
                      NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
      return;
    }
  }
  new_cs = new_cl + InitialConsultCapacity;
  /* start copying */
  memmove((void *)new_cs, (void *)LOCAL_ConsultLow,
          OldConsultCapacity * sizeof(consult_obj));
  /* copying done, release old space */
  Yap_FreeCodeSpace((char *)LOCAL_ConsultLow);
  /* next, set up pointers correctly */
  new_cs += (LOCAL_ConsultSp - LOCAL_ConsultLow);
  /* put LOCAL_ConsultBase at same offset as before move */
  LOCAL_ConsultBase = new_cl + ((LOCAL_ConsultBase - LOCAL_ConsultLow) +
                                InitialConsultCapacity);
  /* new consult pointer */
  LOCAL_ConsultSp =
      new_cl + ((LOCAL_ConsultSp - LOCAL_ConsultLow) + InitialConsultCapacity);
  /* new end of memory */
  LOCAL_ConsultLow = new_cl;
}

static int not_was_reconsulted(PredEntry *p, Term t, int mode) {
  CACHE_REGS
  register consult_obj *fp;
  Prop p0 = AbsProp((PropEntry *)p);

  if (p == LOCAL_LastAssertedPred)
    return FALSE;
  if (!LOCAL_ConsultSp) {
    InitConsultStack();
  }
  if (p->NOfClauses) {
    for (fp = LOCAL_ConsultSp; fp < LOCAL_ConsultBase; ++fp)
      if (fp->p == p0)
        break;
  } else {
    fp = LOCAL_ConsultBase;
  }
  if (fp != LOCAL_ConsultBase) {
    LOCAL_LastAssertedPred = p;
    return false;    /* careful */
  } else if (mode) { // consulting again a predicate in the original file.
    if ((p->NOfClauses &&
         p->src.OwnerFile == Yap_ConsultingFile(PASS_REGS1) &&
         p->src.OwnerFile != AtomNil && !(p->PredFlags & MultiFileFlag) &&
         p->src.OwnerFile != AtomUserIn)) {
      // if (p->ArityOfPE)
      //	printf("+ %s %s
      //%d\n",NameOfFunctor(p->FunctorOfPred)->StrOfAE,p->src.OwnerFile->StrOfAE,
      // p->NOfClauses);
      retract_all(p, Yap_static_in_use(p, TRUE));
    }
    //	printf("- %s
    //%s\n",NameOfFunctor(p->FunctorOfPred)->StrOfAE,p->src.OwnerFile->StrOfAE);
  }
  if (mode) {
    if (LOCAL_ConsultSp <= LOCAL_ConsultLow + 6) {
      expand_consult();
    }
    --LOCAL_ConsultSp;
    LOCAL_ConsultSp->p = p0;
    if (LOCAL_ConsultBase != LOCAL_ConsultLow + LOCAL_ConsultCapacity &&
        LOCAL_ConsultBase[1].mode &&
        !(p->PredFlags & MultiFileFlag)) /* we are in reconsult mode */ {
      retract_all(p, Yap_static_in_use(p, TRUE));
    }
    // p->src.OwnerFile = Yap_ConsultingFile(PASS_REGS1);
  }
  LOCAL_LastAssertedPred = p;
  return TRUE; /* careful */
}

static yamop *addcl_permission_error(const char *file, const char *function,
                                     int lineno, PredEntry *ap, int in_use) {
  CACHE_REGS
  Term culprit = Yap_PredicateToIndicator(ap);
  return in_use ? (ap->ArityOfPE == 0
                       ? Yap_Error__(false, file, function, lineno,
                                     PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE,
                                     culprit, "static predicate %s is in use",
                                     NameOfPred(ap)->StrOfAE)
                       : Yap_Error__(
                             false, file, function, lineno,
                             PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE, culprit,
                             "static predicate %s/" Int_FORMAT " is in use",
                             NameOfPred(ap), ap->ArityOfPE))
                : (ap->ArityOfPE == 0
                       ? Yap_Error__(false, file, function, lineno,
                                     PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE,
                                     culprit, "system predicate %s is in use",
                                     NameOfPred(ap)->StrOfAE)
                       : Yap_Error__(false, file, function, lineno,
                                     PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE,
                                     culprit, "system predicate %s/" Int_FORMAT,
                                     NameOfPred(ap)->StrOfAE, ap->ArityOfPE));
}

PredEntry *Yap_PredFromClause(Term t USES_REGS) {
  Term cmod = LOCAL_SourceModule;
  arity_t extra_arity = 0;

  if (IsVarTerm(t))
    return NULL;
  while (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    if (f == FunctorModule) {
      // module
      cmod = ArgOfTerm(1, t);
      if (!IsAtomTerm(cmod))
        return NULL;
      t = ArgOfTerm(2, t);
    } else if (f == FunctorAssert) {
      t = ArgOfTerm(1, t);
    } else if (f == FunctorComma

               && extra_arity == 2) {
      t = ArgOfTerm(1, t);
    } else if (f == FunctorDoubleArrow) {
      extra_arity = 2;
      t = ArgOfTerm(1, t);
    } else if (f == FunctorQuery || f == FunctorAssert1) {
      // directives
      return NULL;
    } else {
      if (extra_arity) {
        f = Yap_MkFunctor(NameOfFunctor(f), ArityOfFunctor(f) + 2);
      }
      return RepPredProp(Yap_GetPredPropByFunc(f, cmod));
    }
  }
  if (IsAtomTerm(t)) {
    if (extra_arity) {
      Functor f = Yap_MkFunctor(AtomOfTerm(t), 2);
      return RepPredProp(Yap_GetPredPropByFunc(f, cmod));
    }
    return RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(t), cmod));
  }
  // ints, lists

  return NULL;
}

bool Yap_discontiguous(PredEntry *ap, Term mode USES_REGS) {
  register consult_obj *fp;

  if (ap->PredFlags & (DiscontiguousPredFlag | MultiFileFlag) ||
      falseGlobalPrologFlag(DISCONTIGUOUS_WARNINGS_FLAG))
    return false;
  if ((mode != TermConsult && mode != TermReconsult))
    return false;
  if (!LOCAL_ConsultSp) {
    return false;
  }
  if (ap == LOCAL_LastAssertedPred)
    return false;
  if (ap->NOfClauses) {
    Term repeat = AbsPair((CELL *)AbsPredProp(ap));
    for (fp = LOCAL_ConsultSp; fp < LOCAL_ConsultBase; ++fp)
      if (fp->p == AbsPredProp(ap)) {
        // detect repeated warnings
        if (LOCAL_ConsultSp == LOCAL_ConsultLow + 1) {
          expand_consult();
        }
        --LOCAL_ConsultSp;
        LOCAL_ConsultSp->r = repeat;
        return true;
      } else if (fp->r == repeat && ap->NOfClauses > 4) {
        return false;
      }
  }
  return false;
}

static Int p_is_discontiguous(USES_REGS1) { /* '$is_multifile'(+S,+Mod)	 */
  PredEntry *pe;
  Int out;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "discontiguous");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(27, pe);
  out = (pe->PredFlags & DiscontiguousPredFlag);
  UNLOCKPE(44, pe);
  return (out);
}

static Int
    p_new_discontiguous(USES_REGS1) { /* '$new_discontiguous'(+N,+Ar,+Mod)  */
  Atom at;
  int arity;
  PredEntry *pe;
  Term t = Deref(ARG1);
  Term mod = Deref(ARG3);

  if (IsVarTerm(t))
    return false;
  if (IsAtomTerm(t))
    at = AtomOfTerm(t);
  else
    return false;
  t = Deref(ARG2);
  if (IsVarTerm(t))
    return false;
  if (IsIntTerm(t))
    arity = IntOfTerm(t);
  else
    return false;
  if (arity == 0)
    pe = RepPredProp(PredPropByAtom(at, mod));
  else
    pe = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, arity), mod));
  PELOCK(26, pe);
  pe->PredFlags |= DiscontiguousPredFlag;
  /* mutifile-predicates are weird, they do not seat really on the default
   * module */
  if (pe->NOfClauses == 0) {
    pe->CodeOfPred = pe->TrueCodeOfPred = FAILCODE;
    pe->OpcodeOfPred = FAIL_OPCODE;
  }
  UNLOCKPE(43, pe);
  return (TRUE);
}

bool Yap_multiple(PredEntry *ap, Term mode USES_REGS) {
  register consult_obj *fp;

  if ((ap->PredFlags & (MultiFileFlag | LogUpdatePredFlag | DynamicPredFlag)) ||
      mode != TermReconsult)
    return false;
  if (LOCAL_consult_level == 0)
    return false;
  for (fp = LOCAL_ConsultSp; fp < LOCAL_ConsultBase; ++fp)
    if (fp->p == AbsPredProp(ap)) {
      return false;
    }
  return ap->NOfClauses > 0 && ap->src.OwnerFile != AtomNil &&
         Yap_ConsultingFile(PASS_REGS1) != ap->src.OwnerFile &&
         LOCAL_Including != MkAtomTerm(ap->src.OwnerFile);
}

static int is_fact(Term t) {
  Term a1;

  if (IsAtomTerm(t))
    return TRUE;
  if (FunctorOfTerm(t) != FunctorAssert)
    return TRUE;
  a1 = ArgOfTerm(2, t);
  if (a1 == MkAtomTerm(AtomTrue))
    return TRUE;
  return FALSE;
}

Int Yap_source_line_no(void) {
  CACHE_REGS
  return LOCAL_SourceFileLineno;
}

Atom Yap_source_file_name(void) {
  CACHE_REGS
  if (LOCAL_SourceFileName)
    return LOCAL_SourceFileName;
  return AtomNil;
}

/**
 * @brief we cannot add clauses to the procedure
 *
 * @param p predicate
 *
 * @return boolean
 */
bool Yap_constPred(PredEntry *p) {
  pred_flags_t pflags;
  pflags = p->PredFlags;

  if (pflags &
      ((UserCPredFlag | CArgsPredFlag | NumberDBPredFlag | AtomDBPredFlag |
        TestPredFlag | AsmPredFlag | CPredFlag | BinaryPredFlag)))
    return true;

  if (p->PredFlags &
      (SysExportPredFlag | MultiFileFlag | DynamicPredFlag | LogUpdatePredFlag))
    return false;
  if (Yap_isSystemModule(p->ModuleOfPred)) {
    if (p->NOfClauses == 0) {
      p->src.OwnerFile = Yap_source_file_name();
      return false;
    }
    if (p->src.OwnerFile == Yap_source_file_name()) {
      return false;
    }
  }

  return false;
}

bool Yap_addclause(Term t, yamop *cp, Term tmode, Term mod, Term *t4ref)
/*
 *
 mode
   0  assertz
   1  consult
   2  asserta
*/
{
  CACHE_REGS
  PredEntry *p;
  int spy_flag = FALSE;
  Atom at;
  arity_t Arity;
  pred_flags_t pflags;
  Term tf;
  int mode;

  if (tmode == 0) {
    tmode = TermConsult;
  }
  if (tmode == TermConsult) {
    mode = consult;
  } else if (tmode == TermReconsult) {
    mode = consult;
  } else if (tmode == TermAsserta) {
    mode = asserta;
  } else if (tmode == TermAssertz) {
    mode = assertz;
  } else if (tmode == TermAssertaStatic) {
    mode = asserta;
  } else if (tmode == TermAssertzStatic) {
    mode = assertz;
  } else {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, tmode,
              "compilation mode used to assert");
    return false;
  }
  if (IsApplTerm(t) && FunctorOfTerm(t) == FunctorAssert)
    tf = ArgOfTerm(1, t);
  else
    tf = t;
  tf = Yap_YapStripModule(tf, &mod);

  if (IsAtomTerm(tf)) {
    at = AtomOfTerm(tf);
    p = RepPredProp(PredPropByAtom(at, mod));
    Arity = 0;
  } else {
    Functor f = FunctorOfTerm(tf);
    Arity = ArityOfFunctor(f);
    at = NameOfFunctor(f);
    p = RepPredProp(PredPropByFunc(f, mod));
  }
  PELOCK(20, p);
  /* we are redefining a prolog module predicate */
  if (Yap_constPred(p)) {
    addcl_permission_error(__FILE__, __FUNCTION__, __LINE__, p, FALSE);
    UNLOCKPE(30, p);
    return false;
  }
  Yap_PutValue(AtomAbol, TermNil);
  pflags = p->PredFlags;
  /* we are redefining a prolog module predicate */
  if (pflags & MegaClausePredFlag) {
    Yap_split_megaclause(p);
  }
  /* The only problem we have now is when we need to throw away
     Indexing blocks
  */
  if (pflags & IndexedPredFlag && p->NOfClauses > 1) {
    Yap_AddClauseToIndex(p, cp, mode == asserta);
  }
  if (pflags & (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
    spy_flag = true;
  }
  if (Yap_discontiguous(p, tmode PASS_REGS)) {
    Term disc[3], sc[4];
    if (p->ArityOfPE) {
      disc[0] = MkAtomTerm(NameOfFunctor(p->FunctorOfPred));
    } else {
      disc[0] = MkAtomTerm((Atom)(p->FunctorOfPred));
    }
    disc[1] = MkIntTerm(p->ArityOfPE);
    disc[2] = Yap_Module_Name(p);
    sc[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomDiscontiguous, 3), 3, disc);
    sc[1] = MkIntegerTerm(Yap_source_line_no());
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "source %s ",
                        RepAtom(LOCAL_SourceFileName)->StrOfAE);
    sc[2] = MkAtomTerm(LOCAL_SourceFileName);
    sc[3] = t;
    t = Yap_MkApplTerm(Yap_MkFunctor(AtomStyleCheck, 4), 4, sc);
    Yap_PrintWarning(t);
  } else if (Yap_multiple(p, tmode PASS_REGS)) {
    Term disc[4], sc[4];
    if (p->ArityOfPE) {
      disc[0] = MkAtomTerm(NameOfFunctor(p->FunctorOfPred));
    } else {
      disc[0] = MkAtomTerm((Atom)(p->FunctorOfPred));
    }
    disc[1] = MkIntTerm(p->ArityOfPE);
    disc[2] = Yap_Module_Name(p);
    disc[3] = MkAtomTerm(p->src.OwnerFile);
    sc[0] = Yap_MkApplTerm(Yap_MkFunctor(AtomMultiple, 4), 4, disc);
    sc[1] = MkIntegerTerm(Yap_source_line_no());
    sc[2] = MkAtomTerm(LOCAL_SourceFileName);
    sc[3] = t;
    t = Yap_MkApplTerm(Yap_MkFunctor(AtomStyleCheck, 4), 4, sc);
    Yap_PrintWarning(t);
  }
  if (mode == consult)
    not_was_reconsulted(p, t, true);
  /* always check if we have a valid error first */
  if (LOCAL_ErrorMessage &&
      LOCAL_Error_TYPE == PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE) {
    UNLOCKPE(31, p);
    return false;
  }
  if (pflags & UDIPredFlag) {
    Yap_new_udi_clause(p, cp, t);
  }
  if (!is_dynamic(p)) {
    if (pflags & LogUpdatePredFlag) {
      LogUpdClause *clp = ClauseCodeToLogUpdClause(cp);
      clp->ClFlags |= LogUpdMask;
      if (is_fact(t)) {
        clp->ClFlags |= FactMask;
        clp->lusl.ClLine = Yap_source_line_no();
      }
    } else {
      StaticClause *clp = ClauseCodeToStaticClause(cp);
      clp->ClFlags |= StaticMask;
      if (is_fact(t) && !(p->PredFlags & TabledPredFlag)) {
        clp->ClFlags |= FactMask;
        clp->usc.ClLine = Yap_source_line_no();
      }
    }
    if (compile_mode)
      p->PredFlags = p->PredFlags | CompiledPredFlag;
    else
      p->PredFlags = p->PredFlags | CompiledPredFlag;
  }
  if (p->FirstClause == NULL) {
    p->PredFlags &= ~UndefPredFlag;
    if (!(pflags & DynamicPredFlag)) {
      add_first_static(p, cp, spy_flag);
      /* make sure we have a place to jump to */
      if (p->OpcodeOfPred == UNDEF_OPCODE ||
          p->OpcodeOfPred == FAIL_OPCODE) { /* log updates */
        p->CodeOfPred = p->TrueCodeOfPred;
        p->OpcodeOfPred = ((yamop *)(p->CodeOfPred))->opc;
      }
#if defined(YAPOR) || defined(THREADS)
      if (p->PredFlags & LogUpdatePredFlag &&
          !(p->PredFlags & ThreadLocalPredFlag) &&
          p->ModuleOfPred != IDB_MODULE) {
        p->OpcodeOfPred = LOCKPRED_OPCODE;
        p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
      }
#endif
    } else {
      add_first_dynamic(p, cp, spy_flag);
    }
  } else if (mode == asserta) {
    if (pflags & DynamicPredFlag)
      asserta_dynam_clause(p, cp);
    else
      asserta_stat_clause(p, cp, spy_flag);
  } else if (pflags & DynamicPredFlag)
    assertz_dynam_clause(p, cp);
  else {
    assertz_stat_clause(p, cp, spy_flag);
    if (p->OpcodeOfPred != INDEX_OPCODE &&
        p->OpcodeOfPred != Yap_opcode(_spy_pred)) {
      p->CodeOfPred = p->TrueCodeOfPred;
      p->OpcodeOfPred = ((yamop *)(p->CodeOfPred))->opc;
    }
#if defined(YAPOR) || defined(THREADS)
    if (p->PredFlags & LogUpdatePredFlag &&
        !(p->PredFlags & ThreadLocalPredFlag) &&
        p->ModuleOfPred != IDB_MODULE) {
      p->OpcodeOfPred = LOCKPRED_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    }
#endif
  }
  UNLOCKPE(32, p);
  if (pflags & LogUpdatePredFlag) {
    LogUpdClause *cl = (LogUpdClause *)ClauseCodeToLogUpdClause(cp);
    tf = MkDBRefTerm((DBRef)cl);
#if MULTIPLE_STACKS
    TRAIL_CLREF(cl); /* So that fail will erase it */
    INC_CLREF_COUNT(cl);
#else
    if (!(cl->ClFlags & InUseMask)) {
      cl->ClFlags |= InUseMask;
      TRAIL_CLREF(cl); /* So that fail will erase it */
    }
#endif
  } else {
    tf = Yap_MkStaticRefTerm(ClauseCodeToStaticClause(cp), p);
  }
  if (mod == PROLOG_MODULE)
    mod = TermProlog;
  if (pflags & MultiFileFlag) {
    /* add Info on new clause for multifile predicates to the DB */
    Term t[5], tn;
    t[0] = MkAtomTerm(Yap_ConsultingFile(PASS_REGS1));
    t[1] = MkAtomTerm(at);
    t[2] = MkIntegerTerm(Arity);
    t[3] = mod;
    t[4] = tf;
    tn = Yap_MkApplTerm(FunctorMultiFileClause, 5, t);
    Yap_Recordz(AtomMultiFile, tn);
  }
  if (t4ref && *t4ref != TermNil) {
    if (!Yap_unify(*t4ref, tf)) {
      return false;
    }
  }
  return true;
}

void Yap_EraseMegaClause(yamop *cl, PredEntry *ap) {
  /* just make it fail */
  cl->opc = Yap_opcode(_op_fail);
}

void Yap_EraseStaticClause(StaticClause *cl, PredEntry *ap, Term mod) {

  /* ok, first I need to find out the parent predicate */
  if (ap->PredFlags & IndexedPredFlag)
    RemoveIndexation(ap);
  if (ap->PredFlags & MegaClausePredFlag) {
    Yap_split_megaclause(ap);
  }
  ap->NOfClauses--;
  if (ap->FirstClause == cl->ClCode) {
    /* got rid of first clause */
    if (ap->LastClause == cl->ClCode) {
      /* got rid of all clauses */
      ap->LastClause = ap->FirstClause = NULL;
      if (is_live(ap)) {
        ap->OpcodeOfPred = FAIL_OPCODE;
      } else {
        ap->OpcodeOfPred = UNDEF_OPCODE;
        ap->PredFlags |= UndefPredFlag;
      }
      ap->TrueCodeOfPred = (yamop *)(&(ap->OpcodeOfPred));
    } else {
      yamop *ncl = cl->ClNext->ClCode;
      ap->FirstClause = ncl;
      ap->TrueCodeOfPred = ncl;
      ap->OpcodeOfPred = ncl->opc;
    }
  } else {
    StaticClause *pcl = ClauseCodeToStaticClause(ap->FirstClause),
                 *ocl = NULL;

    while (pcl != cl) {
      ocl = pcl;
      pcl = pcl->ClNext;
    }
    if (ocl) {
      ocl->ClNext = cl->ClNext;
    }
    if (cl->ClCode == ap->LastClause) {
      ap->LastClause = ocl->ClCode;
    }
  }
  if (ap->NOfClauses == 1) {
    assert(ap->FirstClause);
    ap->TrueCodeOfPred = ap->FirstClause;
    ap->OpcodeOfPred = ap->TrueCodeOfPred->opc;
  }
  if (cl->ClFlags & HasBlobsMask || Yap_static_in_use(ap, TRUE)) {
    LOCK(DeadStaticClausesLock);
    cl->ClNext = DeadStaticClauses;
    DeadStaticClauses = cl;
    UNLOCK(DeadStaticClausesLock);
  } else {
    Yap_InformOfRemoval(cl);
    Yap_ClauseSpace -= cl->ClSize;
    Yap_FreeCodeSpace((char *)cl);
  }
  if (ap->NOfClauses == 0) {
    ap->CodeOfPred = ap->TrueCodeOfPred;
  } else if (ap->NOfClauses > 1) {
    ap->OpcodeOfPred = INDEX_OPCODE;
    ap->CodeOfPred = ap->TrueCodeOfPred =
        (yamop *)(&(ap->OpcodeOfPred));
  } else if (ap->PredFlags &
             (SpiedPredFlag | CountPredFlag | ProfiledPredFlag)) {
    ap->OpcodeOfPred = Yap_opcode(_spy_pred);
    ap->CodeOfPred = ap->TrueCodeOfPred =
        (yamop *)(&(ap->OpcodeOfPred));
  } else {
    ap->CodeOfPred = ap->TrueCodeOfPred;
  }
#if defined(YAPOR) || defined(THREADS)
  if (ap->PredFlags & LogUpdatePredFlag &&
      !(ap->PredFlags & ThreadLocalPredFlag) &&
      ap->ModuleOfPred != IDB_MODULE) {
    ap->OpcodeOfPred = LOCKPRED_OPCODE;
    ap->CodeOfPred = (yamop *)(&(ap->OpcodeOfPred));
  }
#endif
}

void Yap_add_logupd_clause(PredEntry *pe, LogUpdClause *cl, int mode) {
  yamop *cp = cl->ClCode;

  if (pe->PredFlags & IndexedPredFlag) {
    Yap_AddClauseToIndex(pe, cp, mode == asserta);
  }
  if (pe->FirstClause == NULL) {
    add_first_static(pe, cp, FALSE);
    /* make sure we have a place to jump to */
    if (pe->OpcodeOfPred == UNDEF_OPCODE ||
        pe->OpcodeOfPred == FAIL_OPCODE) { /* log updates */
#if defined(YAPOR) || defined(THREADS)
      if (pe->PredFlags & LogUpdatePredFlag &&
          !(pe->PredFlags & ThreadLocalPredFlag) &&
          pe->ModuleOfPred != IDB_MODULE) {
        pe->OpcodeOfPred = LOCKPRED_OPCODE;
        pe->CodeOfPred = (yamop *)(&(pe->OpcodeOfPred));
      } else {
#endif
        pe->CodeOfPred = pe->TrueCodeOfPred;
        pe->OpcodeOfPred = ((yamop *)(pe->CodeOfPred))->opc;
#if defined(YAPOR) || defined(THREADS)
      }
#endif
    }
  } else if (mode == asserta) {
    asserta_stat_clause(pe, cp, FALSE);
  } else {
    assertz_stat_clause(pe, cp, FALSE);
  }
}

static Int p_compile(USES_REGS1) { /* '$compile'(+C,+Flags,+C0,-Ref) */
  Term t = Deref(ARG1);
  Term t1 = Deref(ARG2);
  Term mod = Deref(ARG4);
  yamop *code_adr;

  if (IsVarTerm(t1) || !IsAtomicTerm(t1))
    return false;
  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return false;
  /* separate assert in current file from reconsult
    if (mode == assertz && LOCAL_consult_level && mod == CurrentModule)
      mode = consult;
  */
  code_adr = Yap_cclause(t, 5, mod, Deref(ARG3)); /* vsc: give the number of
                               arguments to cclause() in case there is a
                               overflow */
  t = Deref(ARG1); /* just in case there was an heap overflow */
  if (!LOCAL_ErrorMessage && code_adr != NULL) {
    YAPEnterCriticalSection();
    Yap_addclause(t, code_adr, t1, mod, &ARG5);
    YAPLeaveCriticalSection();
  }
  if (LOCAL_ErrorMessage) {
    Yap_Error(LOCAL_Error_TYPE, ARG1, LOCAL_ErrorMessage);
    YAPLeaveCriticalSection();
    return false;
  }
  return true;
}

Atom Yap_ConsultingFile(USES_REGS1) {
  int sno;
  if ((sno = Yap_CheckAlias(AtomLoopStream)) >= 0) {
    //    if(sno ==0)
    //  return(AtomUserIn);
    return StreamFullName(sno);
  }
  if (LOCAL_SourceFileName != NULL) {
    return LOCAL_SourceFileName;
  }
  if (LOCAL_consult_level == 0) {
    return (AtomUser);
  } else {
    return LOCAL_ConsultBase[2].f_name;
  }
}

/* consult file *file*, *mode* may be one of either consult or reconsult */
void Yap_init_consult(int mode, const char *filenam) {
  CACHE_REGS
  if (!LOCAL_ConsultSp) {
    InitConsultStack();
  }
  if (LOCAL_ConsultSp >= LOCAL_ConsultLow + 6) {
    expand_consult();
  }
  LOCAL_ConsultSp--;
  LOCAL_ConsultSp->f_name = Yap_LookupAtom(filenam);
  LOCAL_ConsultSp--;
  LOCAL_ConsultSp->mode = mode;
  LOCAL_ConsultSp--;
  LOCAL_ConsultSp->c = (LOCAL_ConsultBase - LOCAL_ConsultSp);
  LOCAL_ConsultBase = LOCAL_ConsultSp;
#if !defined(YAPOR) && !defined(YAPOR_SBA)
/*  if (LOCAL_consult_level == 0)
    do_toggle_static_predicates_in_use(TRUE); */
#endif
  LOCAL_consult_level++;
  LOCAL_LastAssertedPred = NULL;
}

static Int p_startconsult(USES_REGS1) { /* '$start_consult'(+Mode)	 */
  Term t;
  char *smode = RepAtom(AtomOfTerm(Deref(ARG1)))->StrOfAE;
  int mode;

  setBooleanLocalPrologFlag(COMPILING_FLAG, AtomTrue);
  mode = strcmp("consult", (char *)smode);
  Yap_init_consult(mode, RepAtom(AtomOfTerm(Deref(ARG2)))->StrOfAE);
  t = MkIntTerm(LOCAL_consult_level);
  return (Yap_unify_constant(ARG3, t));
}

static Int p_showconslultlev(USES_REGS1) {
  Term t;

  t = MkIntTerm(LOCAL_consult_level);
  return (Yap_unify_constant(ARG1, t));
}

static void end_consult(USES_REGS1) {
  LOCAL_ConsultSp = LOCAL_ConsultBase;
  LOCAL_ConsultBase = LOCAL_ConsultSp + LOCAL_ConsultSp->c;
  LOCAL_ConsultSp += 3;
  LOCAL_consult_level--;
  LOCAL_LastAssertedPred = NULL;
#if !defined(YAPOR) && !defined(YAPOR_SBA)
/*  if (LOCAL_consult_level == 0)
    do_toggle_static_predicates_in_use(FALSE);*/
#endif
  setBooleanLocalPrologFlag(COMPILING_FLAG, AtomFalse);
}

void Yap_end_consult(void) {
  CACHE_REGS
  end_consult(PASS_REGS1);
}

static Int p_endconsult(USES_REGS1) { /* '$end_consult'		 */
  end_consult(PASS_REGS1);
  return (TRUE);
}

static void purge_clauses(PredEntry *pred) {
  if (pred->PredFlags & UDIPredFlag) {
    Yap_udi_abolish(pred);
  }
  if (pred->NOfClauses) {
    if (pred->PredFlags & IndexedPredFlag)
      RemoveIndexation(pred);
    Yap_PutValue(AtomAbol, MkAtomTerm(AtomTrue));
    retract_all(pred, Yap_static_in_use(pred, TRUE));
  }
}

void Yap_Abolish(PredEntry *pred) {
  purge_clauses(pred);
  pred->src.OwnerFile = AtomNil;
}

static Int p_purge_clauses(USES_REGS1) { /* '$purge_clauses'(+Func) */
  PredEntry *pred;
  Term t = Deref(ARG1);
  Term mod = Deref(ARG2);
  MegaClause *before = DeadMegaClauses;

  Yap_PutValue(AtomAbol, MkAtomTerm(AtomNil));
  if (IsVarTerm(t))
    return FALSE;
  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return FALSE;
  }
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pred = RepPredProp(PredPropByAtom(at, mod));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pred = RepPredProp(PredPropByFunc(fun, mod));
  } else
    return (FALSE);
  PELOCK(21, pred);
  if (pred->PredFlags & StandardPredFlag) {
    UNLOCKPE(33, pred);
    Yap_Error(PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE,
              Yap_TermToIndicator(CurrentModule, t), "assert/1");
    return (FALSE);
  }
  purge_clauses(pred);
  UNLOCKPE(34, pred);
  /* try to use the garbage collector to recover the mega clause,
     in case the objs pointing to it are dead themselves */
  if (DeadMegaClauses != before) {
    if (!Yap_gcl(LOCAL_Error_Size, 2, ENV, gc_P(P, CP))) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
      return FALSE;
    }
  }
  return TRUE;
}

static Int p_sys_export(USES_REGS1) { /* '$set_spy'(+Fun,+M)	 */
  PredEntry *pred;
  Term t, mod;

  t = Deref(ARG1);
  mod = Deref(ARG2);
  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return (FALSE);
  if (IsVarTerm(t))
    return (FALSE);
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pred = RepPredProp(Yap_PredPropByAtomNonThreadLocal(at, mod));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pred = RepPredProp(Yap_PredPropByFunctorNonThreadLocal(fun, mod));
  } else {
    return (FALSE);
  }
  PELOCK(100, pred);
  pred->PredFlags |= SysExportPredFlag;
  UNLOCKPE(100, pred);
  return TRUE;
}

/******************************************************************

                MANAGING SPY-POINTS

******************************************************************/

static Int p_is_private(USES_REGS1) { /* '$undefined'(P,Mod)	 */
  PredEntry *pe;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "undefined/1");
  if (EndOfPAEntr(pe))
    return false;
  PELOCK(36, pe);
  if (pe->PredFlags & (NoTracePredFlag | HiddenPredFlag)) {
    UNLOCKPE(57, pe);
    return true;
  }
  UNLOCKPE(59, pe);
  return false;
}

static Int p_set_private(USES_REGS1) { /* '$set_private'(+Fun,+M)	 */
  PredEntry *pe;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "undefined/1");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(36, pe);
  pe->PredFlags |= NoTracePredFlag;
  UNLOCKPE(57, pe);
  return TRUE;
}

int Yap_SetNoTrace(char *name, arity_t arity, Term tmod) {
  PredEntry *pe;

  if (arity == 0) {
    pe = Yap_get_pred(MkAtomTerm(Yap_LookupAtom(name)), tmod, "private");
  } else {
    pe = RepPredProp(
        PredPropByFunc(Yap_MkFunctor(Yap_LookupAtom(name), arity), tmod));
  }
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(36, pe);
  pe->PredFlags |= NoTracePredFlag;
  UNLOCKPE(57, pe);
  return TRUE;
}

static Int p_setspy(USES_REGS1) { /* '$set_spy'(+Fun,+M)	 */
  Atom at;
  PredEntry *pred;
  pred_flags_t fg;
  Term t, mod;

  at = AtomSpy;
  pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1), 0));
  SpyCode = pred;
  t = Deref(ARG1);
  mod = Deref(ARG2);
  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return (FALSE);
  if (IsVarTerm(t))
    return (FALSE);
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pred = RepPredProp(Yap_PredPropByAtomNonThreadLocal(at, mod));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pred = RepPredProp(Yap_PredPropByFunctorNonThreadLocal(fun, mod));
  } else {
    return (FALSE);
  }
  PELOCK(22, pred);
restart_spy:
  if (pred->PredFlags & (CPredFlag | SafePredFlag)) {
    UNLOCKPE(35, pred);
    return FALSE;
  }
  if (pred->OpcodeOfPred == UNDEF_OPCODE || pred->OpcodeOfPred == FAIL_OPCODE) {
    UNLOCKPE(36, pred);
    return FALSE;
  }
  if (pred->OpcodeOfPred == INDEX_OPCODE) {
    int i = 0;
    for (i = 0; i < pred->ArityOfPE; i++) {
      XREGS[i + 1] = MkVarTerm();
    }
    IPred(pred, 0, CP);
    goto restart_spy;
  }
  fg = pred->PredFlags;
  if (fg & DynamicPredFlag) {
    pred->OpcodeOfPred = ((yamop *)(pred->CodeOfPred))->opc =
        Yap_opcode(_spy_or_trymark);
  } else {
    pred->OpcodeOfPred = Yap_opcode(_spy_pred);
    pred->CodeOfPred = (yamop *)(&(pred->OpcodeOfPred));
  }
  pred->PredFlags |= SpiedPredFlag;
  UNLOCKPE(37, pred);
  return TRUE;
}

static Int p_rmspy(USES_REGS1) { /* '$rm_spy'(+T,+Mod)	 */
  Atom at;
  PredEntry *pred;
  Term t;
  Term mod;

  t = Deref(ARG1);
  mod = Deref(ARG2);
  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return (FALSE);
  if (IsVarTerm(t))
    return (FALSE);
  if (IsAtomTerm(t)) {
    at = AtomOfTerm(t);
    pred = RepPredProp(Yap_PredPropByAtomNonThreadLocal(at, mod));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pred = RepPredProp(Yap_PredPropByFunctorNonThreadLocal(fun, mod));
  } else
    return FALSE;
  PELOCK(23, pred);
  if (!(pred->PredFlags & SpiedPredFlag)) {
    UNLOCKPE(38, pred);
    return FALSE;
  }
#if THREADS
  if (pred->PredFlags & ThreadLocalPredFlag) {
    pred->OpcodeOfPred = Yap_opcode(_thread_local);
    pred->PredFlags ^= SpiedPredFlag;
    UNLOCKPE(39, pred);
    return TRUE;
  }
#endif
  if (!(pred->PredFlags & (CountPredFlag | ProfiledPredFlag))) {
    if (!(pred->PredFlags & DynamicPredFlag)) {
#if defined(YAPOR) || defined(THREADS)
      if (pred->PredFlags & LogUpdatePredFlag &&
          !(pred->PredFlags & ThreadLocalPredFlag) &&
          pred->ModuleOfPred != IDB_MODULE) {
        pred->OpcodeOfPred = LOCKPRED_OPCODE;
        pred->CodeOfPred = (yamop *)(&(pred->OpcodeOfPred));
      } else {
#endif
        pred->CodeOfPred = pred->TrueCodeOfPred;
        pred->OpcodeOfPred = pred->CodeOfPred->opc;
#if defined(YAPOR) || defined(THREADS)
      }
#endif
    } else if (pred->OpcodeOfPred == Yap_opcode(_spy_or_trymark)) {
      pred->OpcodeOfPred = Yap_opcode(_try_and_mark);
    } else {
      UNLOCKPE(39, pred);
      return FALSE;
    }
  }
  pred->PredFlags ^= SpiedPredFlag;
  UNLOCKPE(40, pred);
  return (TRUE);
}

/******************************************************************

                INFO ABOUT PREDICATES

******************************************************************/

static Int
    number_of_clauses(USES_REGS1) { /* '$number_of_clauses'(Predicate,M,N) */
  Term t = Deref(ARG1);
  Term mod = Deref(ARG2);
  int ncl = 0;
  Prop pe;

  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return (FALSE);
  }
  if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    pe = Yap_GetPredPropByAtom(a, mod);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    pe = Yap_GetPredPropByFunc(f, mod);
  } else {
    return (FALSE);
  }
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(24, RepPredProp(pe));
  ncl = RepPredProp(pe)->NOfClauses;
  UNLOCKPE(41, RepPredProp(pe));
  return (Yap_unify_constant(ARG3, MkIntegerTerm(ncl)));
}

/*  @pred '$new_multifile'(+G,+Mod)
 *  declares   the multi-file flag
 * */
static Int new_multifile(USES_REGS1) {
  PredEntry *pe;

  pe = new_pred(Deref(ARG1), Deref(ARG2), "multifile");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(30, pe);

  if (pe->PredFlags & MultiFileFlag) {
    UNLOCKPE(26, pe);
    return true;
  }
  if (pe->PredFlags & (TabledPredFlag | ForeignPredFlags)) {
    UNLOCKPE(26, pe);
    addcl_permission_error(__FILE__, __FUNCTION__, __LINE__, pe, FALSE);
    return false;
  }
  if (pe->NOfClauses) {
    UNLOCKPE(26, pe);
    addcl_permission_error(__FILE__, __FUNCTION__, __LINE__, pe, FALSE);
    return false;
  }
  pe->PredFlags &= ~UndefPredFlag;
  pe->PredFlags |= MultiFileFlag;
  /* mutifile-predicates are weird, they do not seat really on the default
   * module */
  if (!(pe->PredFlags & (DynamicPredFlag | LogUpdatePredFlag))) {
    /* static */
    pe->PredFlags |= (SourcePredFlag | CompiledPredFlag);
  }
  pe->src.OwnerFile = Yap_ConsultingFile(PASS_REGS1);
  if (pe->NOfClauses == 0) {
    pe->CodeOfPred = pe->TrueCodeOfPred = FAILCODE;
    pe->OpcodeOfPred = FAIL_OPCODE;
  }
  UNLOCKPE(43, pe);
  return true;
}

static Int p_is_multifile(USES_REGS1) { /* '$is_multifile'(+S,+Mod)	 */
  PredEntry *pe;
  bool out;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "$is_multifile");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(27, pe);

  out = (pe->PredFlags & MultiFileFlag);
  UNLOCKPE(44, pe);
  return (out);
}

static Int new_system_predicate(
    USES_REGS1) { /* '$new_system_predicate'(+N,+Ar,+Mod)  */
  Atom at;
  arity_t arity;
  PredEntry *pe;
  Term t = Deref(ARG1);
  Term mod = Deref(ARG3);

  if (IsVarTerm(t))
    return (FALSE);
  if (IsAtomTerm(t))
    at = AtomOfTerm(t);
  else
    return (FALSE);
  t = Deref(ARG2);
  if (IsVarTerm(t))
    return (FALSE);
  if (IsIntTerm(t))
    arity = IntOfTerm(t);
  else
    return FALSE;
  if (arity == 0)
    pe = RepPredProp(PredPropByAtom(at, mod));
  else
    pe = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, arity), mod));
  PELOCK(26, pe);
  if (pe->PredFlags & (LogUpdatePredFlag | DynamicPredFlag | MultiFileFlag)) {
    UNLOCKPE(43, pe);
    return false;
  }
  pe->PredFlags |= (StandardPredFlag);
  UNLOCKPE(43, pe);
  return (true);
}

static Int
    is_system_predicate(USES_REGS1) { /* '$is_multifile'(+S,+Mod)	 */
  PredEntry *pe;
  Term t1 = Deref(ARG1);

  // pe = Yap_get_pred(t1, Deref(ARG2), "system_predicate");
  // if (!pe)
  pe = Yap_get_pred(t1, Deref(ARG2), "system_predicate");
  // if (!pe) pe = Yap_get_pred(t1, Deref(ARG2), "system_predicate");
  if (EndOfPAEntr(pe))
    return FALSE;
  return (pe->ModuleOfPred == 0 || pe->PredFlags & UserCPredFlag);
  //   return true;
  // PELOCK(27, pe);
  // out = (pe->PredFlags & SystemPredFlags);
  // UNLOCKPE(44, pe);
  // return (out);
}

static Int
    is_opaque_predicate(USES_REGS1) { /* '$is_multifile'(+S,+Mod)	 */
  PredEntry *pe;
  Term t1 = Deref(ARG1);
  bool out;

  // pe = Yap_get_pred(t1, Deref(ARG2), "system_predicate");
  // if (!pe)
  pe = Yap_get_pred(t1, Deref(ARG2), "system_predicate");
  // if (!pe) pe = Yap_get_pred(t1, USER_MODULE, "system_predicate");
  if (EndOfPAEntr(pe))
    return FALSE;
  return (pe->ModuleOfPred == 0 ||
          pe->PredFlags & (SystemPredFlags | ForeignPredFlags));
  UNLOCKPE(44, pe);
  return (out);
}

static Int p_is_thread_local(USES_REGS1) { /* '$is_dynamic'(+P)	 */
  PredEntry *pe;
  bool out;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "$is_log_updatable");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(27, pe);
  out = (pe->PredFlags & ThreadLocalPredFlag);
  UNLOCKPE(45, pe);
  return (out);
}

static Int p_is_log_updatable(USES_REGS1) { /* '$is_dynamic'(+P)	 */
  PredEntry *pe;
  bool out;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "$is_log_updatable");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(27, pe);
  out = (pe->PredFlags & LogUpdatePredFlag);
  UNLOCKPE(45, pe);
  return (out);
}

static Int p_is_source(USES_REGS1) { /* '$is_dynamic'(+P)	 */
  PredEntry *pe;
  bool out;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "$is_source");
  if (EndOfPAEntr(pe))
    return false;
  PELOCK(28, pe);
  if (pe->PredFlags & SystemPredFlags) {
    UNLOCKPE(46, pe);
    return false;
  }
  out = (pe->PredFlags & (SourcePredFlag | LogUpdatePredFlag |
                          MegaClausePredFlag | DynamicPredFlag));
  UNLOCKPE(46, pe);
  return out;
}

static Int p_is_exo(USES_REGS1) { /* '$is_dynamic'(+P)	 */
  PredEntry *pe;
  bool out;
  MegaClause *mcl;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "$is_exo");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(28, pe);
  out = (pe->PredFlags & MegaClausePredFlag);
  if (out) {
    mcl = ClauseCodeToMegaClause(pe->FirstClause);
    out = mcl->ClFlags & ExoMask;
  }
  UNLOCKPE(46, pe);
  return (out);
}

static Int owner_file(USES_REGS1) { /* '$owner_file'(+P,M,F)	 */
  PredEntry *pe;
  Atom owner;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "$is_source");
  if (EndOfPAEntr(pe))
    return false;
  PELOCK(29, pe);
  if (pe->ModuleOfPred == IDB_MODULE) {
    UNLOCKPE(47, pe);
    return false;
  }
  if (pe->PredFlags & MultiFileFlag) {
    UNLOCKPE(48, pe);
    return false;
  }
  if (is_system(pe) || is_foreign(pe)) {
    UNLOCKPE(48, pe);
    return false;
  }
  owner = pe->src.OwnerFile;
  UNLOCKPE(49, pe);
  if (owner == AtomNil || owner == NULL)
    return false;
  return Yap_unify(ARG3, MkAtomTerm(owner));
}

static Int p_set_owner_file(USES_REGS1) { /* '$owner_file'(+P,M,F)	 */
  PredEntry *pe;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "$is_source");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(29, pe);
  if (pe->ModuleOfPred == IDB_MODULE) {
    UNLOCKPE(47, pe);
    return FALSE;
  }
  if (pe->PredFlags & MultiFileFlag) {
    UNLOCKPE(48, pe);
    return FALSE;
  }
  pe->src.OwnerFile = AtomOfTerm(Deref(ARG3));
  UNLOCKPE(49, pe);
  return TRUE;
}

static Int mk_dynamic(USES_REGS1) { /* '$make_dynamic'(+P)	 */
  PredEntry *pe;

  pe = new_pred(Deref(ARG1), Deref(ARG2), "dynamic");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(30, pe);

  if (pe->PredFlags &
      (UserCPredFlag | CArgsPredFlag | NumberDBPredFlag | AtomDBPredFlag |
       TestPredFlag | AsmPredFlag | CPredFlag | BinaryPredFlag)) {
    UNLOCKPE(30, pe);
    addcl_permission_error(__FILE__, __FUNCTION__, __LINE__, pe, FALSE);
    return false;
  }
  if (pe->PredFlags & LogUpdatePredFlag) {
    UNLOCKPE(26, pe);
    return true;
  }
  if (pe->PredFlags & DynamicPredFlag) {
    UNLOCKPE(26, pe);
    return true;
  }
  if (pe->NOfClauses != 0) {
    UNLOCKPE(26, pe);
    addcl_permission_error(__FILE__, __FUNCTION__, __LINE__, pe, FALSE);
    return false;
  }
  if (pe->OpcodeOfPred == UNDEF_OPCODE) {
    pe->OpcodeOfPred = FAIL_OPCODE;
    pe->PredFlags &= ~UndefPredFlag;
  }
  pe->src.OwnerFile = Yap_ConsultingFile(PASS_REGS1);
  pe->PredFlags |= LogUpdatePredFlag;
  UNLOCKPE(50, pe);
  return true;
}

static Int p_is_dynamic(USES_REGS1) { /* '$is_dynamic'(+P)	 */
  PredEntry *pe;
  bool out;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "$is_dynamic");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(31, pe);
  out = (pe->PredFlags & (DynamicPredFlag | LogUpdatePredFlag));
  UNLOCKPE(51, pe);
  return (out);
}

/*  @pred '$new_meta'(+G,+Mod)
 *  sets the multi-file flag
 * */
static Int new_meta_pred(USES_REGS1) {
  PredEntry *pe;

  pe = new_pred(Deref(ARG1), Deref(ARG2), "meta_predicate");
  if (EndOfPAEntr(pe))
    return false;
  PELOCK(30, pe);

  if (pe->PredFlags & MetaPredFlag) {
    UNLOCKPE(26, pe);
    return true;
  }
  if (pe->NOfClauses) {
    UNLOCKPE(26, pe);
    addcl_permission_error(__FILE__, __FUNCTION__, __LINE__, pe, FALSE);
    return false;
  }
  pe->PredFlags |= MetaPredFlag;
  if (!(pe->PredFlags & (DynamicPredFlag | LogUpdatePredFlag))) {
    /* static */
    pe->PredFlags |= (SourcePredFlag | CompiledPredFlag);
  }
  UNLOCKPE(43, pe);
  return true;
}

static Int p_is_metapredicate(USES_REGS1) { /* '$is_metapredicate'(+P)	 */
  PredEntry *pe;
  bool out;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "$is_meta");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(32, pe);
  out = (pe->PredFlags & MetaPredFlag);
  UNLOCKPE(52, pe);
  return out;
}

static Int p_pred_exists(USES_REGS1) { /* '$pred_exists'(+P,+M)	 */
  PredEntry *pe;
  bool out;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "predicate_exists");
  if (EndOfPAEntr(pe))
    return false;
  PELOCK(34, pe);
  if (pe->PredFlags & HiddenPredFlag) {
    UNLOCKPE(54, pe);
    return false;
  }
  out = (is_live(pe) || pe->OpcodeOfPred != UNDEF_OPCODE);
  UNLOCKPE(55, pe);
  return out;
}

static Int p_set_pred_module(USES_REGS1) { /* '$set_pred_module'(+P,+Mod)
                                            */
  PredEntry *pe;

  pe = Yap_get_pred(Deref(ARG1), CurrentModule, "set_pred_module/1");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(35, pe);
  pe->ModuleOfPred = Deref(ARG2);
  UNLOCKPE(56, pe);
  return (TRUE);
}

static Int p_set_pred_owner(USES_REGS1) { /* '$set_pred_module'(+P,+File)
                                           */
  PredEntry *pe;
  Term a2 = Deref(ARG2);

  pe = Yap_get_pred(Deref(ARG1), CurrentModule, "set_pred_module/1");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(35, pe);
  if (pe->PredFlags &
      (UserCPredFlag | CArgsPredFlag | NumberDBPredFlag | AtomDBPredFlag |
       TestPredFlag | AsmPredFlag | CPredFlag | BinaryPredFlag)) {
    UNLOCKPE(56, pe);
    return FALSE;
  }
  if (IsVarTerm(a2)) {
    Yap_Error(INSTANTIATION_ERROR, a2, "load_files/2");
    UNLOCKPE(56, pe);
    return FALSE;
  }
  if (!IsAtomTerm(a2)) {
    Yap_Error(TYPE_ERROR_ATOM, a2, "load_files/2");
    UNLOCKPE(56, pe);
    return FALSE;
  }
  pe->src.OwnerFile = AtomOfTerm(a2);
  UNLOCKPE(56, pe);
  return (TRUE);
}

/**
 * Set handler for undefined predicates.
 */

static Int undefp_handler(USES_REGS1) { /* '$undefp_handler'(P,Mod)	 */
  PredEntry *pe;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "undefined/1");
  PELOCK(59, pe);
  if (EndOfPAEntr(pe)) {
    UndefCode = Yap_get_pred(TermFail, MkIntTerm(0), "no def");
    UNLOCKPE(59, pe);
    return false;
  }
  if (pe->OpcodeOfPred == UNDEF_OPCODE) {
    UndefCode = Yap_get_pred(TermFail, MkIntTerm(0), "no def");
    UNLOCKPE(59, pe);
    return false;
  }
  UndefCode = pe;
  UNLOCKPE(59, pe);
  return true;
}

static Int p_undefined(USES_REGS1) { /* '$undefined'(P,Mod)	 */
  PredEntry *pe;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "undefined/1");
  if (EndOfPAEntr(pe))
    return TRUE;
  PELOCK(36, pe);
  if (!is_live(pe) && pe->OpcodeOfPred == UNDEF_OPCODE) {
    UNLOCKPE(58, pe);
    return TRUE;
  }
  UNLOCKPE(59, pe);
  return FALSE;
}

/*
 * this predicate should only be called when all clauses for the dynamic
 * predicate were remove, otherwise chaos will follow!!
 */

static Int p_kill_dynamic(USES_REGS1) { /* '$kill_dynamic'(P,M)       */
  PredEntry *pe;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "kill_dynamic/1");
  if (EndOfPAEntr(pe))
    return TRUE;
  PELOCK(37, pe);
  if (!(pe->PredFlags & (DynamicPredFlag | LogUpdatePredFlag))) {
    UNLOCKPE(60, pe);
    return FALSE;
  }
  if (pe->LastClause != pe->FirstClause) {
    UNLOCKPE(61, pe);
    return (FALSE);
  }
  pe->LastClause = pe->FirstClause = NULL;
  pe->OpcodeOfPred = UNDEF_OPCODE;
  pe->TrueCodeOfPred = pe->CodeOfPred =
      (yamop *)(&(pe->OpcodeOfPred));
  pe->PredFlags = UndefPredFlag;
  UNLOCKPE(62, pe);
  return (TRUE);
}

static Int p_optimizer_on(USES_REGS1) { /* '$optimizer_on'		 */
  optimizer_on = TRUE;
  return (TRUE);
}

static Int p_optimizer_off(USES_REGS1) { /* '$optimizer_off'		 */
  optimizer_on = FALSE;
  return (TRUE);
}

static Int p_is_profiled(USES_REGS1) {
  Term t = Deref(ARG1);
  char *s;

  if (IsVarTerm(t)) {
    Term ta;

    if (trueGlobalPrologFlag(PROFILING_FLAG))
      ta = MkAtomTerm(AtomOn);
    else
      ta = MkAtomTerm(AtomOff);
    YapBind((CELL *)t, ta);
    return (TRUE);
  } else if (!IsAtomTerm(t))
    return (FALSE);
  s = (char *)RepAtom(AtomOfTerm(t))->StrOfAE;
  if (strcmp(s, "on") == 0) {
    Yap_InitComma();
    return (TRUE);
  } else if (strcmp(s, "off") == 0) {
    PROFILING = FALSE;
    Yap_InitComma();
    return (TRUE);
  }
  return (FALSE);
}

static Int p_profile_info(USES_REGS1) {
  Term mod = Deref(ARG1);
  Term tfun = Deref(ARG2);
  Term out;
  PredEntry *pe;
  Term p[3];

  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return (FALSE);
  if (IsVarTerm(tfun)) {
    return (FALSE);
  } else if (IsApplTerm(tfun)) {
    Functor f = FunctorOfTerm(tfun);
    if (IsExtensionFunctor(f)) {
      return (FALSE);
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(f, mod));
  } else if (IsAtomTerm(tfun)) {
    pe = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(tfun), mod));
  } else {
    return (FALSE);
  }
  if (EndOfPAEntr(pe))
    return (FALSE);
  LOCK(pe->StatisticsForPred->lock);
  if (!(pe->StatisticsForPred->NOfEntries)) {
    UNLOCK(pe->StatisticsForPred->lock);
    return (FALSE);
  }
  p[0] = Yap_MkULLIntTerm(pe->StatisticsForPred->NOfEntries);
  p[1] = Yap_MkULLIntTerm(pe->StatisticsForPred->NOfHeadSuccesses);
  p[2] = Yap_MkULLIntTerm(pe->StatisticsForPred->NOfRetries);
  UNLOCK(pe->StatisticsForPred->lock);
  out = Yap_MkApplTerm(Yap_MkFunctor(AtomProfile, 3), 3, p);
  return (Yap_unify(ARG3, out));
}

static Int p_profile_reset(USES_REGS1) {
  Term mod = Deref(ARG1);
  Term tfun = Deref(ARG2);
  PredEntry *pe;

  if (IsVarTerm(mod) || !IsAtomTerm(mod))
    return (FALSE);
  if (IsVarTerm(tfun)) {
    return (FALSE);
  } else if (IsApplTerm(tfun)) {
    Functor f = FunctorOfTerm(tfun);
    if (IsExtensionFunctor(f)) {
      return (FALSE);
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(f, mod));
  } else if (IsAtomTerm(tfun)) {
    pe = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(tfun), mod));
  } else {
    return (FALSE);
  }
  if (EndOfPAEntr(pe))
    return (FALSE);
  LOCK(pe->StatisticsForPred->lock);
  pe->StatisticsForPred->NOfEntries = 0;
  pe->StatisticsForPred->NOfHeadSuccesses = 0;
  pe->StatisticsForPred->NOfRetries = 0;
  UNLOCK(pe->StatisticsForPred->lock);
  return (TRUE);
}

static Int p_is_call_counted(USES_REGS1) {
  Term t = Deref(ARG1);
  char *s;

  if (IsVarTerm(t)) {
    Term ta;

    if (CALL_COUNTING)
      ta = MkAtomTerm(AtomOn);
    else
      ta = MkAtomTerm(AtomOff);
    YapBind((CELL *)t, ta);
    return (TRUE);
  } else if (!IsAtomTerm(t))
    return (FALSE);
  s = (char *)RepAtom(AtomOfTerm(t))->StrOfAE;
  if (strcmp(s, "on") == 0) {
    CALL_COUNTING = TRUE;
    Yap_InitComma();
    return (TRUE);
  } else if (strcmp(s, "off") == 0) {
    CALL_COUNTING = FALSE;
    Yap_InitComma();
    return (TRUE);
  }
  return (FALSE);
}

static Int p_call_count_info(USES_REGS1) {
  return (Yap_unify(MkIntegerTerm(LOCAL_ReductionsCounter), ARG1) &&
          Yap_unify(MkIntegerTerm(LOCAL_PredEntriesCounter), ARG2) &&
          Yap_unify(MkIntegerTerm(LOCAL_PredEntriesCounter), ARG3));
}

static Int p_call_count_reset(USES_REGS1) {
  LOCAL_ReductionsCounter = 0;
  LOCAL_ReductionsCounterOn = FALSE;
  LOCAL_PredEntriesCounter = 0;
  LOCAL_PredEntriesCounterOn = FALSE;
  LOCAL_RetriesCounter = 0;
  LOCAL_RetriesCounterOn = FALSE;
  return (TRUE);
}

static Int p_call_count_set(USES_REGS1) {
  int do_calls = IntOfTerm(ARG2);
  int do_retries = IntOfTerm(ARG4);
  int do_entries = IntOfTerm(ARG6);

  if (do_calls)
    LOCAL_ReductionsCounter = IntegerOfTerm(Deref(ARG1));
  LOCAL_ReductionsCounterOn = do_calls;
  if (do_retries)
    LOCAL_RetriesCounter = IntegerOfTerm(Deref(ARG3));
  LOCAL_RetriesCounterOn = do_retries;
  if (do_entries)
    LOCAL_PredEntriesCounter = IntegerOfTerm(Deref(ARG5));
  LOCAL_PredEntriesCounterOn = do_entries;
  return (TRUE);
}

static Int p_clean_up_dead_clauses(USES_REGS1) {
  while (DeadStaticClauses != NULL) {
    char *pt = (char *)DeadStaticClauses;
    Yap_ClauseSpace -= DeadStaticClauses->ClSize;
    DeadStaticClauses = DeadStaticClauses->ClNext;
    Yap_InformOfRemoval(pt);
    Yap_FreeCodeSpace(pt);
  }
  while (DeadStaticIndices != NULL) {
    char *pt = (char *)DeadStaticIndices;
    if (DeadStaticIndices->ClFlags & SwitchTableMask)
      Yap_IndexSpace_SW -= DeadStaticIndices->ClSize;
    else
      Yap_IndexSpace_Tree -= DeadStaticIndices->ClSize;
    DeadStaticIndices = DeadStaticIndices->SiblingIndex;
    Yap_InformOfRemoval(pt);
    Yap_FreeCodeSpace(pt);
  }
  while (DeadMegaClauses != NULL) {
    char *pt = (char *)DeadMegaClauses;
    Yap_ClauseSpace -= DeadMegaClauses->ClSize;
    DeadMegaClauses = DeadMegaClauses->ClNext;
    Yap_InformOfRemoval(pt);
    Yap_FreeCodeSpace(pt);
  }
  return TRUE;
}

void Yap_HidePred(PredEntry *pe) {

  if (pe->PredFlags & HiddenPredFlag)
    return;
  pe->PredFlags |= (HiddenPredFlag | NoSpyPredFlag | NoTracePredFlag);
  if (pe->NextOfPE) {
    UInt hash = PRED_HASH(pe->FunctorOfPred, CurrentModule, PredHashTableSize);
    READ_LOCK(PredHashRWLock);
    PredEntry *p, **op = PredHash + hash;
    p = *op;

    while (p) {
      if (p == pe) {
        *op = p->NextPredOfHash;
        break;
      }
      op = &p->NextPredOfHash;
      p = p->NextPredOfHash;
    }
    pe->NextPredOfHash = NULL;
  }
  {
    Prop *op, p;
    if (pe->ArityOfPE == 0) {
      op = &RepAtom(AtomOfTerm((Term)(pe->FunctorOfPred)))->PropsOfAE;
    } else {
      op = &pe->FunctorOfPred->PropsOfFE;
    }
    p = *op;

    while (p) {
      if (p == AbsPredProp(pe)) {
        *op = p->NextOfPE;
        break;
      }
      op = &p->NextOfPE;
      p = p->NextOfPE;
    }
    pe->NextOfPE = RepAtom(AtomFoundVar)->PropsOfAE;
    RepAtom(AtomFoundVar)->PropsOfAE = AbsPredProp(pe);
  }

  {
    PredEntry *p,
        **op = &Yap_GetModuleEntry(Yap_Module(pe->ModuleOfPred))->PredForME;
    p = *op;

    while (p) {
      if (p == pe) {
        *op = p->NextPredOfModule;
        break;
      }
      op = &p->NextPredOfModule;
      p = p->NextPredOfModule;
    }
    pe->NextPredOfModule = NULL;
  }
}

static Int /* $hidden_predicate(P) */
hide_predicate(USES_REGS1) {
  PredEntry *pe =
      Yap_get_pred(Deref(ARG1), Deref(ARG2), "while checking for a procedure");
  if (pe) {
    Yap_HidePred(pe);
    return true;
  } else
    return false;
}

static Int /* $hidden_predicate(P) */
stash_predicate(USES_REGS1) {
  PredEntry *pe =
      Yap_get_pred(Deref(ARG1), Deref(ARG2), "while checking for a procedure");
  if (pe) {
    pe->PredFlags |= (HiddenPredFlag | NoSpyPredFlag | NoTracePredFlag);
    /*
    char ns[1024];
      const char *s = (pe->ModuleOfPred == PROLOG_MODULE ?
                     "__prolog__stash__" :
                     snprintf(sn,1023,"__%s__".RepAtom(AtomOfTerm(
    pe->ModuleOfPred )))); pe->ModuleOfPred = MkAtomTerm(Yap_LookupAtom(s));
    */
    return true;
  } else
    return false;
}

static Int /* $hidden_predicate(P) */
hidden_predicate(USES_REGS1) {
  PredEntry *pe =
      Yap_get_pred(Deref(ARG1), Deref(ARG2), "while checking for a procedure");
  if (pe)
    return (pe->PredFlags & HiddenPredFlag);
  else
    return false;
}

static Int fetch_next_lu_clause(PredEntry *pe, yamop *i_code, yamop *cp_ptr, bool first_time) {
  CACHE_REGS
  LogUpdClause *cl;
  Term rtn, th, tb, tr;
  yhandle_t yterms = Yap_InitHandles(4, XREGS + 3);
  
  cl = Yap_FollowIndexingCode(
      pe, i_code, Yap_AddressFromHandle(yterms) , NEXTOP(PredLogUpdClause->CodeOfPred, Otapl), cp_ptr);
  th = Yap_GetFromSlot(yterms + 1);
  tb = Yap_GetFromSlot(yterms + 2);
  tr = Yap_GetFromSlot(yterms + 3);
  if (cl == NULL) {
    UNLOCK(pe->PELock);
      LOCAL_CurHandle = yterms;
      return FALSE;
  }
  rtn = MkDBRefTerm((DBRef)cl);
#if MULTIPLE_STACKS
  TRAIL_CLREF(cl); /* So that fail will erase it */
  INC_CLREF_COUNT(cl);
#else
  if (!(cl->ClFlags & InUseMask)) {
    cl->ClFlags |= InUseMask;
    TRAIL_CLREF(cl); /* So that fail will erase it */
  }
#endif
  if (cl->ClFlags & FactMask) {
    if (!Yap_unify_constant(tb, MkAtomTerm(AtomTrue)) || !Yap_unify(tr, rtn)) {
       UNLOCK(pe->PELock);
        LOCAL_CurHandle = yterms;
      return FALSE;
    }
    if (pe->ArityOfPE) {
      Functor f = FunctorOfTerm(th);
      arity_t arity = ArityOfFunctor(f), i;
      CELL *pt = RepAppl(th) + 1;

      for (i = 0; i < arity; i++) {
        XREGS[i + 1] = pt[i];
      }
      /* don't need no ENV */
      if (first_time && P->opc != EXECUTE_CPRED_OP_CODE) {
        CP = P;
        ENV = YENV;
        YENV = ASP;
        YENV[E_CB] = (CELL)B;
      }
      P = cl->ClCode;
#if defined(YAPOR) || defined(THREADS)
      if (pe->PredFlags & ThreadLocalPredFlag) {
        /* we don't actually need to execute code */
        UNLOCK(pe->PELock);
      } else {
        PP = pe;
      }
#endif
    } else {
      /* we don't actually need to execute code */
      UNLOCK(pe->PELock);
    }
      LOCAL_CurHandle = yterms;
    return TRUE;
  } else {
    Term t;

    while ((t = Yap_FetchClauseTermFromDB(cl->lusl.ClSource)) == 0L) {

      if (first_time) {
        if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_growglobal(NULL)) {
            UNLOCK(pe->PELock);
            Yap_ThrowError(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                      LOCAL_ErrorMessage);
            return false;
          }
        } else {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_gcl(LOCAL_Error_Size, 7, ENV, gc_P(P, CP))) {
            UNLOCK(pe->PELock);
            Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
            return false;
          }
        }
      th = Yap_GetFromSlot(yterms + 1);
      tb = Yap_GetFromSlot(yterms + 2);
      tr = Yap_GetFromSlot(yterms + 3);
      } else {
        if (!Yap_gcl(LOCAL_Error_Size, 0, ENV, gc_P(P, CP))) {
          UNLOCK(pe->PELock);
          Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
      th = Yap_GetFromSlot(yterms + 1);
      tb = Yap_GetFromSlot(yterms + 2);
      tr = Yap_GetFromSlot(yterms + 3);

      }
    }
      LOCAL_CurHandle = yterms;
    UNLOCK(pe->PELock);
    return (Yap_unify(th, ArgOfTerm(1, t)) && Yap_unify(tb, ArgOfTerm(2, t)) &&
            Yap_unify(tr, rtn));
  }
}

static Int /* $hidden_predicate(P) */
log_update_clause(USES_REGS1) {
  PredEntry *pe;
Term t1 = Deref(ARG4);
  Int ret;
  yamop *new_cp;

  if (P->opc == EXECUTE_CPRED_OP_CODE) {
    new_cp = CP;
  } else {
    new_cp = P;
  }
  pe = Yap_get_pred(t1, Deref(ARG3), "clause/3");
  if (pe == NULL || EndOfPAEntr(pe))
    return FALSE;
  PELOCK(41, pe);
  ret = fetch_next_lu_clause(pe, pe->CodeOfPred, new_cp, true);
return ret;
}

static Int /* $hidden_predicate(P) */
continue_log_update_clause(USES_REGS1) {
 PredEntry *pe = (PredEntry *)IntegerOfTerm(Deref(ARG1));
  yamop *ipc = (yamop *)IntegerOfTerm(ARG2);

  PELOCK(42, pe);
  bool rc = fetch_next_lu_clause(pe, ipc, B->cp_cp, false);
 return rc;
}

static Int fetch_next_lu_clause_erase(PredEntry *pe, yamop *i_code, yamop *cp_ptr,
                                      int first_time) {
  CACHE_REGS
  LogUpdClause *cl;
  Term rtn, th, tb, tr;
  yhandle_t yterms = Yap_InitHandles(4, XREGS + 3);

  cl = Yap_FollowIndexingCode(
      pe, i_code, Yap_AddressFromHandle(yterms),
      NEXTOP(PredLogUpdClauseErase->CodeOfPred, Otapl), cp_ptr);
  th = Yap_GetFromSlot(yterms + 1);
  tb = Yap_GetFromSlot(yterms + 2);
  tr = Yap_GetFromSlot(yterms + 3);
  if (cl == NULL) {
    UNLOCK(pe->PELock);
    LOCAL_CurHandle = yterms;
    return FALSE;
  }
  rtn = MkDBRefTerm((DBRef)cl);
#if MULTIPLE_STACKS
  TRAIL_CLREF(cl); /* So that fail will erase it */
  INC_CLREF_COUNT(cl);
#else
  if (!(cl->ClFlags & InUseMask)) {
    cl->ClFlags |= InUseMask;
    TRAIL_CLREF(cl); /* So that fail will erase it */
  }
#endif
  if (cl->ClFlags & FactMask) {
    if (!Yap_unify_constant(tb, MkAtomTerm(AtomTrue)) || !Yap_unify(tr, rtn)) {
      UNLOCK(pe->PELock);
        LOCAL_CurHandle = yterms;
      return FALSE;
    }
    if (pe->ArityOfPE) {
      Functor f = FunctorOfTerm(th);
      arity_t arity = ArityOfFunctor(f), i;
      CELL *pt = RepAppl(th) + 1;

      for (i = 0; i < arity; i++) {
        XREGS[i + 1] = pt[i];
      }
      /* don't need no ENV */
      if (first_time && P->opc != EXECUTE_CPRED_OP_CODE) {
        CP = P;
        ENV = YENV;
        YENV = ASP;
        YENV[E_CB] = (CELL)B;
      }
      P = cl->ClCode;
#if defined(YAPOR) || defined(THREADS)
      if (pe->PredFlags & ThreadLocalPredFlag) {
        /* we don't actually need to execute code */
        UNLOCK(pe->PELock);
      } else {
        PP = pe;
      }
#endif
    } else {
      /* we don't actually need to execute code */
      UNLOCK(pe->PELock);
    }
    Yap_ErLogUpdCl(cl);
        LOCAL_CurHandle = yterms;
	return true;
  } else {
    Term t;
    while ((t = Yap_FetchClauseTermFromDB(cl->lusl.ClSource)) == 0L) {
        if (first_time) {
            if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
                LOCAL_Error_TYPE = YAP_NO_ERROR;
                if (!Yap_locked_growglobal(NULL)) {
                    UNLOCK(pe->PELock);
                    Yap_ThrowError(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                                   LOCAL_ErrorMessage);
                    return false;
                }
            } else {
                LOCAL_Error_TYPE = YAP_NO_ERROR;
                if (!Yap_locked_gcl(LOCAL_Error_Size, 0, ENV, gc_P(P, CP))) {
                    UNLOCK(pe->PELock);
                    Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
                    return FALSE;
                }
            }
            th = Yap_GetFromSlot(yterms + 1);
            tb = Yap_GetFromSlot(yterms + 2);
            tr = Yap_GetFromSlot(yterms + 3);
            LOCAL_CurHandle = yterms;
        } else {
            if (!Yap_gcl(LOCAL_Error_Size, 6, ENV, CP)) {
                UNLOCK(pe->PELock);
                Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
                return FALSE;
            }
            th = Yap_GetFromSlot(yterms  +1);
            tb = Yap_GetFromSlot(yterms + 2);
            tr = Yap_GetFromSlot(yterms + 3);
            LOCAL_CurHandle = yterms;
        }
    }
    LOCAL_CurHandle = yterms;
    bool res = Yap_unify(th, ArgOfTerm(1, t)) && Yap_unify(tb, ArgOfTerm(2, t)) &&
        Yap_unify(tr, rtn);
  if (res)
    Yap_ErLogUpdCl(cl);
  UNLOCK(pe->PELock);
  return res;
}
}

static Int /* $hidden_predicate(P) */
log_update_clause_erase(USES_REGS1) {
  PredEntry *pe;
  Term t1 = Deref(ARG4);
  Int ret;
  yamop *new_cp;

  if (P->opc == EXECUTE_CPRED_OP_CODE) {
    new_cp = CP;
  } else {
    new_cp = P;
  }
  pe = Yap_get_pred(t1, Deref(ARG3), "clause/3");
  if (pe == NULL || EndOfPAEntr(pe))
    return FALSE;
  PELOCK(43, pe);
  ret = fetch_next_lu_clause_erase(pe, pe->CodeOfPred, new_cp, TRUE);
  return ret;
}

static Int /* $hidden_predicate(P) */
continue_log_update_clause_erase(USES_REGS1) {
  PredEntry *pe = (PredEntry *)IntegerOfTerm(Deref(ARG1));
  yamop *ipc = (yamop *)IntegerOfTerm(ARG2);

  PELOCK(44, pe);
  return fetch_next_lu_clause_erase(pe, ipc, B->cp_cp,
                                    FALSE);
}

static void adjust_cl_timestamp(LogUpdClause *cl, UInt *arp, UInt *base) {
  UInt clstamp = cl->ClTimeEnd;
  if (cl->ClTimeEnd != TIMESTAMP_EOT) {
    while (arp[0] > clstamp)
      arp--;
    if (arp[0] == clstamp) {
      cl->ClTimeEnd = (arp - base);
    } else {
      cl->ClTimeEnd = (arp - base) + 1;
    }
  }
  clstamp = cl->ClTimeStart;
  while (arp[0] > clstamp)
    arp--;
  if (arp[0] == clstamp) {
    cl->ClTimeStart = (arp - base);
  } else {
    cl->ClTimeStart = (arp - base) + 1;
  }
  clstamp = cl->ClTimeEnd;
}

static Term replace_integer(Term orig, UInt new) {
  CELL *pt;

  if (IntInBnd((Int) new))
    return MkIntTerm(new);
  /* should create an old integer */
  if (!IsApplTerm(orig)) {
    CACHE_REGS
    Yap_Error(SYSTEM_ERROR_INTERNAL, orig,
              "%uld-->%uld  where it should increase",
              (unsigned long int)IntegerOfTerm(orig), (unsigned long int)new);
    return MkIntegerTerm(new);
  }
  /* appl->appl */
  /* replace integer in situ */
  pt = RepAppl(orig) + 1;
  *pt = new;
  return orig;
}

static UInt tree_index_ssz(StaticIndex *x) {
  UInt sz = x->ClSize;
  x = x->ChildIndex;
  while (x != NULL) {
    sz += tree_index_ssz(x);
    x = x->SiblingIndex;
  }
  return sz;
}

static UInt index_ssz(StaticIndex *x, PredEntry *pe) {
  UInt sz = 0;
  yamop *ep = ExpandClausesFirst;
  if (pe->PredFlags & MegaClausePredFlag) {
    MegaClause *mcl = ClauseCodeToMegaClause(pe->FirstClause);
    if (mcl->ClFlags & ExoMask) {
      struct index_t *i = ((struct index_t **)(pe->FirstClause))[0];
      sz = 0;

      while (i) {
        sz = i->size + sz;
        i = i->next;
      }
      return sz;
    }
  }
  /* expand clause blocks */
  while (ep) {
    if (ep->y_u.sssllp.p == pe)
      sz += (UInt)NEXTOP((yamop *)NULL, sssllp) +
            ep->y_u.sssllp.s1 * sizeof(yamop *);
    ep = ep->y_u.sssllp.snext;
  }
  /* main indexing tree */
  sz += tree_index_ssz(x);
  return sz;
}

#ifdef DEBUG
static Int p_predicate_lu_cps(USES_REGS1) {
  return Yap_unify(ARG1, MkIntegerTerm(Yap_LiveCps)) &&
         Yap_unify(ARG2, MkIntegerTerm(Yap_FreedCps)) &&
         Yap_unify(ARG3, MkIntegerTerm(Yap_DirtyCps)) &&
         Yap_unify(ARG4, MkIntegerTerm(Yap_NewCps));
}
#endif

static Int static_statistics(PredEntry *pe) {
  CACHE_REGS
  UInt sz = sizeof(PredEntry), cls = 0, isz = 0;
  StaticClause *cl = ClauseCodeToStaticClause(pe->FirstClause);

  if (pe->NOfClauses > 1 &&
      pe->TrueCodeOfPred != pe->FirstClause) {
    isz = index_ssz(ClauseCodeToStaticIndex(pe->TrueCodeOfPred), pe);
  }
  if (pe->PredFlags & MegaClausePredFlag) {
    MegaClause *mcl = ClauseCodeToMegaClause(pe->FirstClause);
    return Yap_unify(ARG3, MkIntegerTerm(mcl->ClSize / mcl->ClItemSize)) &&
           Yap_unify(ARG4, MkIntegerTerm(mcl->ClSize)) &&
           Yap_unify(ARG5, MkIntegerTerm(isz));
  }
  if (pe->NOfClauses) {
    do {
      cls++;
      sz += cl->ClSize;
      if (cl->ClCode == pe->LastClause)
        break;
      cl = cl->ClNext;
    } while (TRUE);
  }
  return Yap_unify(ARG3, MkIntegerTerm(cls)) &&
         Yap_unify(ARG4, MkIntegerTerm(sz)) &&
         Yap_unify(ARG5, MkIntegerTerm(isz));
}

static Int p_static_pred_statistics(USES_REGS1) {
  Int out;
  PredEntry *pe;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "predicate_statistics");
  if (pe == NIL)
    return (FALSE);
  PELOCK(50, pe);
  if (pe->PredFlags & (DynamicPredFlag | LogUpdatePredFlag | UserCPredFlag |
                       AsmPredFlag | CPredFlag | BinaryPredFlag)) {
    /* should use '$recordedp' in this case */
    UNLOCK(pe->PELock);
    return FALSE;
  }
  out = static_statistics(pe);
  UNLOCK(pe->PELock);
  return out;
}

static Int p_predicate_erased_statistics(USES_REGS1) {
  UInt sz = 0, cls = 0;
  UInt isz = 0, icls = 0;
  PredEntry *pe;
  LogUpdClause *cl = DBErasedList;
  LogUpdIndex *icl = DBErasedIList;
  Term tpred = ArgOfTerm(2, Deref(ARG1));
  Term tmod = ArgOfTerm(1, Deref(ARG1));

  if (EndOfPAEntr(pe =
                      Yap_get_pred(tpred, tmod, "predicate_erased_statistics")))
    return FALSE;
  while (cl) {
    if (cl->ClPred == pe) {
      cls++;
      sz += cl->ClSize;
    }
    cl = cl->ClNext;
  }
  while (icl) {
    if (pe == icl->ClPred) {
      icls++;
      isz += icl->ClSize;
    }
    icl = icl->SiblingIndex;
  }
  return Yap_unify(ARG2, MkIntegerTerm(cls)) &&
         Yap_unify(ARG3, MkIntegerTerm(sz)) &&
         Yap_unify(ARG4, MkIntegerTerm(icls)) &&
         Yap_unify(ARG5, MkIntegerTerm(isz));
}

void Yap_UpdateTimestamps(PredEntry *ap) {
  CACHE_REGS
  choiceptr bptr = B;
  yamop *cl0 = NEXTOP(PredLogUpdClause0->CodeOfPred, Otapl);
  yamop *cl = NEXTOP(PredLogUpdClause->CodeOfPred, Otapl);
  yamop *cle = NEXTOP(PredLogUpdClauseErase->CodeOfPred, Otapl);
  arity_t ar = ap->ArityOfPE;
  UInt *arp, *top, *base;
  LogUpdClause *lcl;

#if THREADS
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "Timestamp overflow %p", ap);
  return;
#endif
  if (!ap->NOfClauses)
    return;
restart:
  *--ASP = TIMESTAMP_EOT;
  top = arp = (UInt *)ASP;
  while (bptr) {
    op_numbers opnum = Yap_op_from_opcode(bptr->cp_ap->opc);

    switch (opnum) {
    case _retry_logical:
    case _count_retry_logical:
    case _profiled_retry_logical:
    case _trust_logical:
    case _count_trust_logical:
    case _profiled_trust_logical:
      if (bptr->cp_ap->y_u.OtaLl.d->ClPred == ap) {
        UInt ts = IntegerOfTerm(bptr->cp_args[ar]);
        if (ts != arp[0]) {
          if (arp - HR < 1024) {
            goto overflow;
          }
          /* be thrifty, have this in case there is a hole */
          if (ts != arp[0] - 1) {
            UInt x = arp[0];
            *--arp = x;
          }
          *--arp = ts;
        }
      }
      bptr = bptr->cp_b;
      break;
    case _retry:
      if ((bptr->cp_ap == cl0 || bptr->cp_ap == cl || bptr->cp_ap == cle) &&
          ((PredEntry *)IntegerOfTerm(bptr->cp_args[0]) == ap)) {
        UInt ts = IntegerOfTerm(bptr->cp_args[5]);
        if (ts != arp[0]) {
          if (arp - HR < 1024) {
            goto overflow;
          }
          if (ts != arp[0] - 1) {
            UInt x = arp[0];
            *--arp = x;
          }
          *--arp = ts;
        }
      }
      bptr = bptr->cp_b;
      break;
    default:
      bptr = bptr->cp_b;
      continue;
    }
  }
  if (*arp)
    *--arp = 0L;
  base = arp;
  lcl = ClauseCodeToLogUpdClause(ap->FirstClause);
  while (lcl) {
    adjust_cl_timestamp(lcl, top - 1, base);
    lcl = lcl->ClNext;
  }
  lcl = DBErasedList;
  while (lcl) {
    if (lcl->ClPred == ap)
      adjust_cl_timestamp(lcl, top - 1, base);
    lcl = lcl->ClNext;
  }
  arp = top - 1;
  bptr = B;
  while (bptr) {
    op_numbers opnum = Yap_op_from_opcode(bptr->cp_ap->opc);

    switch (opnum) {
    case _retry_logical:
    case _count_retry_logical:
    case _profiled_retry_logical:
    case _trust_logical:
    case _count_trust_logical:
    case _profiled_trust_logical:
      if (bptr->cp_ap->y_u.OtaLl.d->ClPred == ap) {
        UInt ts = IntegerOfTerm(bptr->cp_args[ar]);
        while (ts != arp[0])
          arp--;
        bptr->cp_args[ar] = replace_integer(bptr->cp_args[ar], arp - base);
      }
      bptr = bptr->cp_b;
      break;
    case _retry:
      if ((bptr->cp_ap == cl0 || bptr->cp_ap == cl || bptr->cp_ap == cle) &&
          ((PredEntry *)IntegerOfTerm(bptr->cp_args[0]) == ap)) {
        UInt ts = IntegerOfTerm(bptr->cp_args[5]);
        while (ts != arp[0])
          arp--;
        bptr->cp_args[5] = replace_integer(bptr->cp_args[5], arp - base);
      }
      bptr = bptr->cp_b;
      break;
    default:
      bptr = bptr->cp_b;
      continue;
    }
  }
  return;
overflow:
  if (!Yap_growstack(64 * 1024)) {
    Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
    return;
  }
  goto restart;
}

static Int fetch_next_static_clause(PredEntry *pe, yamop *i_code, yamop *cp_ptr,
                                    int first_time) {
  CACHE_REGS
  StaticClause *cl;
  Term rtn;
  arity_t arity = pe->ArityOfPE;
  yhandle_t yterms = Yap_InitHandles(4, XREGS + 3);
  cl = (StaticClause *)Yap_FollowIndexingCode(
      pe, i_code, Yap_AddressFromHandle(yterms) ,
      NEXTOP(PredStaticClause->CodeOfPred, Otapl), cp_ptr);
  /*
     don't do this!! I might have stored a choice-point and changed ASP
     Yap_RecoverSlots(4);
  */
  if (cl == NULL) {
      LOCAL_CurHandle = yterms;
    UNLOCKPE(45, pe);
    return false;
  }
  CELL *Terms = Yap_AddressFromHandle(yterms) + 1;
  if (pe->PredFlags & MegaClausePredFlag) {
    yamop *code = (yamop *)cl;
    rtn = Yap_MkMegaRefTerm(pe, code);
    if (!Yap_unify(Terms[1], MkAtomTerm(AtomTrue)) ||
        !Yap_unify(Terms[2], rtn)) {
        LOCAL_CurHandle = yterms;
      UNLOCKPE(45, pe);
      return false;
    }
    if (arity) {
      CELL *pt = RepAppl(Terms[0]) + 1;
      int i;
      for (i = 0; i < arity; i++) {
        XREGS[i + 1] = pt[i];
      }
      /* don't need no ENV */
      if (first_time && P->opc != EXECUTE_CPRED_OP_CODE) {
        CP = P;
        ENV = YENV;
        YENV = ASP;
        YENV[E_CB] = (CELL)B;
      }
      P = code;
    }
    UNLOCKPE(45, pe);
      LOCAL_CurHandle = yterms;
      return true;
  } else if (arity && cl->ClFlags & FactMask) {
    rtn = Yap_MkStaticRefTerm(cl, pe);
    if (!Yap_unify(Terms[1], MkAtomTerm(AtomTrue)) ||
        !Yap_unify(Terms[2], rtn)) {
        LOCAL_CurHandle = yterms;
      UNLOCKPE(45, pe);
      return false;
    }
    int i;
    CELL *s = RepAppl(Deref(Terms[0]))+1;
    for (i = 0; i < arity; i++) {
      XREGS[i + 1] = s[i];
    }
    /* don't need no ENV */
    if (first_time && P->opc != EXECUTE_CPRED_OP_CODE) {
      CP = P;
      ENV = YENV;
      YENV = ASP;
      YENV[E_CB] = (CELL)B;
    }
    P = cl->ClCode;
    UNLOCKPE(45, pe);
      LOCAL_CurHandle = yterms;
    return true;
  }
  if (!(pe->PredFlags & SourcePredFlag)) {
    /* no source */
    rtn = Yap_MkStaticRefTerm(cl, pe);
    UNLOCKPE(45, pe);
    LOCAL_CurHandle = yterms;
    bool rc = Yap_unify(Terms[2], rtn);
      LOCAL_CurHandle = yterms;
    return rc;
  } else {
    Term t;
    while ((t = Yap_FetchClauseTermFromDB(cl->usc.ClSource)) == 0L) {
      if (first_time) {
        if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_growglobal(NULL)) {
            UNLOCKPE(45, pe);
            Yap_ThrowError(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                           LOCAL_ErrorMessage);

            return FALSE;
          }
        } else {
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          if (!Yap_dogc(0, NULL)) {
            UNLOCKPE(45, pe);
            Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
            return false;
          }
        }
      } else {
        LOCAL_Error_TYPE = YAP_NO_ERROR;
        if (!Yap_dogc(0, NULL)) {
          UNLOCKPE(45, pe);
          Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
    }
    Term rtn = Yap_MkStaticRefTerm(cl, pe);
    UNLOCKPE(45, pe);
    bool rc;
    rc = Yap_unify(Terms[2], rtn);
    if (FunctorOfTerm(t) != FunctorAssert) {
      rc = rc && Yap_unify(Terms[1], TermTrue);
    } else {
      rc = rc && Yap_unify(Terms[1], ArgOfTerm(2, t));
      t = ArgOfTerm(1, t);
    }
    if (IsApplTerm(t)) {
      rc = rc && Yap_unify(Terms[0], t);
    }
      LOCAL_CurHandle = yterms;
    return rc;
  }
}

static Int /* $hidden_predicate(P) */
static_clause(USES_REGS1) {
  PredEntry *pe;
  yamop *new_cp;

  if (P->opc == EXECUTE_CPRED_OP_CODE) {
    new_cp = CP;
  } else {
    new_cp = P;
  }
  pe = Yap_get_pred(Deref(ARG4), Deref(ARG3), "clause/3");
  if (pe == NULL || EndOfPAEntr(pe))
    return false;
  PELOCK(46, pe);
  return fetch_next_static_clause(pe, pe->CodeOfPred, new_cp, true);
}

static Int /* $hidden_predicate(P) */
continue_static_clause(USES_REGS1) {
  PredEntry *pe = AddressOfTerm(Deref(ARG1));
  yamop *ipc = AddressOfTerm(ARG2);

  PELOCK(48, pe);
  return fetch_next_static_clause(pe, ipc, B->cp_ap, false);
}
#define CL_PROP_ERASED 0
#define CL_PROP_PRED 1
#define CL_PROP_FILE 2
#define CL_PROP_FACT 3
#define CL_PROP_LINE 4
#define CL_PROP_STREAM 5

/* instance(+Ref,?Term) */
static Int instance_property(USES_REGS1) {
  Term t1 = Deref(ARG1);
  DBRef dbr;

  Int op = IntOfTerm(Deref(ARG2));

  if (IsVarTerm(t1) || !IsDBRefTerm(t1)) {
    if (IsApplTerm(t1)) {
      if (FunctorOfTerm(t1) == FunctorStaticClause) {
        StaticClause *cl = Yap_ClauseFromTerm(t1);

        if (op == CL_PROP_ERASED) {
          if (cl->ClFlags & ErasedMask) {
            if (!Yap_unify(ARG3, MkAtomTerm(AtomTrue)))
              return FALSE;
          } else {
            if (!Yap_unify(ARG3, MkAtomTerm(AtomFalse)))
              return FALSE;
          }
        }
        if (op == CL_PROP_PRED || op == CL_PROP_FILE || op == CL_PROP_STREAM) {
          PredEntry *ap = (PredEntry *)IntegerOfTerm(ArgOfTerm(2, t1));
          if (!ap) {
            return FALSE;
          }
          if (op == CL_PROP_FILE) {
            if (ap->src.OwnerFile)
              return Yap_unify(ARG3, MkAtomTerm(ap->src.OwnerFile));
            else
              return FALSE;
          } else {
            Term t[2];

            if (ap->ArityOfPE == 0) {
              t[1] = MkAtomTerm((Atom)ap->FunctorOfPred);
            } else {
              Functor nf = ap->FunctorOfPred;
              arity_t arity = ArityOfFunctor(nf);
              Atom name = NameOfFunctor(nf);

              t[0] = MkAtomTerm(name);
              t[1] = MkIntegerTerm(arity);
              t[1] = Yap_MkApplTerm(FunctorSlash, 2, t);
            }
            if (ap->ModuleOfPred == PROLOG_MODULE) {
              t[0] = MkAtomTerm(AtomProlog);
            } else {
              t[0] = ap->ModuleOfPred;
            }
            return Yap_unify(ARG3, Yap_MkApplTerm(FunctorModule, 2, t));
          }
        }
        if (op == CL_PROP_FACT) {
          if (cl->ClFlags & FactMask) {
            return Yap_unify(ARG3, MkAtomTerm(AtomTrue));
          } else {
            return Yap_unify(ARG3, MkAtomTerm(AtomFalse));
          }
        }
        if (op == CL_PROP_LINE) {
          if (cl->ClFlags & FactMask) {
            return Yap_unify(ARG3, MkIntTerm(cl->usc.ClLine));
          } else if (cl->ClFlags & SrcMask) {
            return Yap_unify(ARG3, MkIntTerm(cl->usc.ClSource->ag.line_number));
          } else
            return Yap_unify(ARG3, MkIntTerm(0));
        }
      } else if (FunctorOfTerm(t1) == FunctorMegaClause) {
        PredEntry *ap = (PredEntry *)IntegerOfTerm(ArgOfTerm(1, t1));
        MegaClause *mcl = ClauseCodeToMegaClause(ap->FirstClause);

        if (op == CL_PROP_ERASED) {
          return FALSE;
        }
        if (op == CL_PROP_PRED || op == CL_PROP_FILE || op == CL_PROP_STREAM) {
          if (op == CL_PROP_FILE) {
            if (ap->src.OwnerFile)
              return Yap_unify(ARG3, MkAtomTerm(ap->src.OwnerFile));
            else
              return FALSE;
          } else {
            Functor nf = ap->FunctorOfPred;
            arity_t arity = ArityOfFunctor(nf);
            Atom name = NameOfFunctor(nf);
            Term t[2];

            t[0] = MkAtomTerm(name);
            t[1] = MkIntegerTerm(arity);
            t[1] = Yap_MkApplTerm(FunctorSlash, 2, t);
            if (ap->ModuleOfPred == PROLOG_MODULE) {
              t[0] = MkAtomTerm(AtomProlog);
            } else {
              t[0] = ap->ModuleOfPred;
            }
            return Yap_unify(ARG3, Yap_MkApplTerm(FunctorModule, 2, t));
          }
        }
        if (op == CL_PROP_FACT) {
          return Yap_unify(ARG3, MkAtomTerm(AtomTrue));
        }
        if (op == CL_PROP_LINE) {
          return Yap_unify(ARG3, MkIntTerm(mcl->ClLine));
        }
      }
    }
  } else if ((dbr = DBRefOfTerm(t1))->Flags & LogUpdMask) {
    LogUpdClause *cl = (LogUpdClause *)dbr;

    if (op == CL_PROP_ERASED) {
      if (cl->ClFlags & ErasedMask) {
        if (!Yap_unify(ARG3, MkAtomTerm(AtomTrue)))
          return FALSE;
      } else {
        if (!Yap_unify(ARG3, MkAtomTerm(AtomFalse)))
          return FALSE;
      }
    }
    if (op == CL_PROP_PRED || op == CL_PROP_FILE) {
      PredEntry *ap = cl->ClPred;
      Term t[2];

      if (op == CL_PROP_FILE) {
        if (ap->src.OwnerFile)
          return Yap_unify(ARG3, MkAtomTerm(ap->src.OwnerFile));
        else
          return FALSE;
      }
      if (ap->ArityOfPE == 0) {
        t[1] = MkAtomTerm((Atom)ap->FunctorOfPred);
      } else {
        Functor nf = ap->FunctorOfPred;
        arity_t arity = ArityOfFunctor(nf);
        Atom name = NameOfFunctor(nf);

        t[0] = MkAtomTerm(name);
        t[1] = MkIntegerTerm(arity);
        t[1] = Yap_MkApplTerm(FunctorSlash, 2, t);
      }
      if (ap->ModuleOfPred == PROLOG_MODULE) {
        t[0] = MkAtomTerm(AtomProlog);
      } else {
        t[0] = ap->ModuleOfPred;
      }
      return Yap_unify(ARG3, Yap_MkApplTerm(FunctorModule, 2, t));
    }
    if (op == CL_PROP_FACT) {
      if (cl->ClFlags & FactMask) {
        return Yap_unify(ARG3, MkAtomTerm(AtomTrue));
      } else {
        return Yap_unify(ARG3, MkAtomTerm(AtomFalse));
      }
    }
    if (op == CL_PROP_LINE) {
      if (cl->ClFlags & FactMask) {
        return Yap_unify(ARG3, MkIntTerm(cl->lusl.ClLine));
      } else if (cl->ClFlags & SrcMask) {
        return Yap_unify(ARG3, MkIntTerm(cl->lusl.ClSource->ag.line_number));
      } else
        return Yap_unify(ARG3, MkIntTerm(0));
    }
  }
  return FALSE;
}

static Int nth_clause(USES_REGS1) {
  PredEntry *pe;
  arity_t pred_arity;
  Functor pred_f;
  Term pred_module;
  Term t4 = Deref(ARG4);

  if (IsVarTerm(t4)) {
    // we must know I or count;
    Term TCount;
    Int Count;

    TCount = Deref(ARG3);
    if (IsVarTerm(TCount)) {
      return FALSE; // backtrack?
    }
    if (!IsIntegerTerm(TCount)) {
      Yap_Error(TYPE_ERROR_INTEGER, TCount, "nth_instance/3");
      return FALSE;
    }
    Count = IntegerOfTerm(TCount);
    if (Count <= 0) {
      if (Count)
        Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, TCount, "nth_clause/3");
      else
        Yap_Error(DOMAIN_ERROR_NOT_ZERO, TCount, "nth_clause/3");
      return FALSE;
    }
    pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "nth_clause/3");
    if (pe) {
      PELOCK(47, pe);
    }
    if (Deref(ARG2) == IDB_MODULE) {
      return Yap_db_nth_recorded(pe, Count PASS_REGS);
    } else {
      Int CurSlot, sl4;
      arity_t i;
      void *cl0;

      if (!pe)
        return FALSE;
      CurSlot = Yap_StartSlots();
      /* I have pe and n */
      sl4 = Yap_InitSlot(ARG4);
      /* in case we have to index or to expand code */
      for (i = 1; i <= pe->ArityOfPE; i++) {
        XREGS[i] = MkVarTerm();
      }
      if (pe->OpcodeOfPred == INDEX_OPCODE) {
        IPred(pe, 0, CP);
      }
      cl0 = Yap_NthClause(pe, Count);
      ARG4 = Yap_GetFromSlot(sl4);
      LOCAL_CurSlot = CurSlot;
      if (cl0 == NULL) {
        UNLOCK(pe->PELock);
        return FALSE;
      }
      if (pe->PredFlags & LogUpdatePredFlag) {
        LogUpdClause *cl = cl0;

#if MULTIPLE_STACKS
        TRAIL_CLREF(cl); /* So that fail will erase it */
        INC_CLREF_COUNT(cl);
#else
        if (!(cl->ClFlags & InUseMask)) {
          cl->ClFlags |= InUseMask;
          TRAIL_CLREF(cl); /* So that fail will erase it */
        }
#endif
        UNLOCK(pe->PELock);
        return Yap_unify(MkDBRefTerm((DBRef)cl), ARG4);
      } else if (pe->PredFlags & MegaClausePredFlag) {
        MegaClause *mcl = ClauseCodeToMegaClause(pe->FirstClause);
        if (mcl->ClFlags & ExoMask) {
          UNLOCK(pe->PELock);
          return Yap_unify(Yap_MkExoRefTerm(pe, Count - 1), ARG4);
        }
        /* fast access to nth element, all have same size */
        UNLOCK(pe->PELock);
        return Yap_unify(Yap_MkMegaRefTerm(pe, cl0), ARG4);
      } else {
        UNLOCK(pe->PELock);
        return Yap_unify(Yap_MkStaticRefTerm(cl0, pe), ARG4);
      }
    }
  }
  /* t4 is bound, we have a reference */
  if (IsDBRefTerm(t4)) {
    DBRef ref = DBRefOfTerm(t4);
    if (ref->Flags & LogUpdMask) {
      LogUpdClause *cl = (LogUpdClause *)ref;
      LogUpdClause *ocl;
      UInt icl = 0;

      pe = cl->ClPred;
      PELOCK(66, pe);
      if (cl->ClFlags & ErasedMask) {
        UNLOCK(pe->PELock);
        return FALSE;
      }
      ocl = ClauseCodeToLogUpdClause(pe->FirstClause);
      do {
        icl++;
        if (cl == ocl)
          break;
        ocl = ocl->ClNext;
      } while (ocl != NULL);
      UNLOCK(pe->PELock);
      if (ocl == NULL) {
        return FALSE;
      }
      if (!Yap_unify(ARG3, MkIntegerTerm(icl))) {
        return FALSE;
      }
    } else {
      return Yap_unify_immediate_ref(ref PASS_REGS);
    }
  } else if (IsApplTerm(t4)) {
    Functor f = FunctorOfTerm(t4);

    if (f == FunctorStaticClause) {
      StaticClause *cl = Yap_ClauseFromTerm(t4), *cl0;
      pe = (PredEntry *)IntegerOfTerm(ArgOfTerm(2, t4));
      Int i;

      if (!pe) {
        return FALSE;
      }
      if (!pe->NOfClauses)
        return FALSE;
      cl0 = ClauseCodeToStaticClause(pe->FirstClause);
      // linear scan
      for (i = 1; i < pe->NOfClauses; i++) {
        if (cl0 == cl) {
          if (!Yap_unify(MkIntTerm(i), ARG3))
            return FALSE;
          break;
        }
      }
    } else if (f == FunctorMegaClause) {
      MegaClause *mcl;
      yamop *cl = Yap_MegaClauseFromTerm(t4);
      Int i;

      pe = Yap_MegaClausePredicateFromTerm(t4);
      mcl = ClauseCodeToMegaClause(pe->FirstClause);
      i = ((char *)cl - (char *)mcl->ClCode) / mcl->ClItemSize;
      if (!Yap_unify(MkIntTerm(i), ARG3))
        return FALSE;
    } else if (f == FunctorExoClause) {
      Int i;

      pe = Yap_ExoClausePredicateFromTerm(t4);
      i = Yap_ExoClauseFromTerm(t4);
      if (!Yap_unify(MkIntTerm(i + 1), ARG3)) {
        return FALSE;
      }
    } else {
      Yap_Error(TYPE_ERROR_REFERENCE, t4, "nth_clause/3");
      return FALSE;
    }
  } else {
    Yap_Error(TYPE_ERROR_REFERENCE, t4, "nth_clause/3");
    return FALSE;
  }
  pred_module = pe->ModuleOfPred;
  if (pred_module != IDB_MODULE) {
    pred_f = pe->FunctorOfPred;
    pred_arity = pe->ArityOfPE;
  } else {
    if (pe->PredFlags & NumberDBPredFlag) {
      pred_f = (Functor)MkIntegerTerm(pe->src.IndxId);
      pred_arity = 0;
    } else {
      pred_f = pe->FunctorOfPred;
      if (pe->PredFlags & AtomDBPredFlag) {
        pred_arity = 0;
      } else {
        pred_arity = ArityOfFunctor(pred_f);
      }
    }
  }
  if (pred_arity) {
    if (!Yap_unify(ARG1, Yap_MkNewApplTerm(pred_f, pred_arity)))
      return FALSE;
  } else {
    if (!Yap_unify(ARG1, MkAtomTerm((Atom)pred_f)))
      return FALSE;
  }
  if (pred_module == PROLOG_MODULE) {
    if (!Yap_unify(ARG2, TermProlog))
      return FALSE;
  } else {
    if (!Yap_unify(ARG2, pred_module))
      return FALSE;
  }
  return TRUE;
}

static Int including(USES_REGS1) {
  bool rc = Yap_unify(ARG1, LOCAL_Including);
  if (!rc)
    return FALSE;
  LOCAL_Including = Deref(ARG2);
  return true;
}

static Int predicate_flags(
    USES_REGS1) { /* $predicate_flags(+Functor,+Mod,?OldFlags,?NewFlags) */
  PredEntry *pe;
  pred_flags_t newFl;
  Term t1 = Deref(ARG1);
  Term mod = Deref(ARG2);

  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return false;
  }
  if (IsVarTerm(t1))
    return (FALSE);
  if (IsAtomTerm(t1)) {
    while ((pe = RepPredProp(PredPropByAtom(AtomOfTerm(t1), mod))) == NULL) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, ARG1, "while generating new predicate");
        return FALSE;
      }
      t1 = Deref(ARG1);
      mod = Deref(ARG2);
    }
  } else if (IsApplTerm(t1)) {
    Functor funt = FunctorOfTerm(t1);
    while ((pe = RepPredProp(PredPropByFunc(funt, mod))) == NULL) {
      if (!Yap_growheap(FALSE, 0, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, ARG1, "while generating new predicate");
        return FALSE;
      }
      t1 = Deref(ARG1);
      mod = Deref(ARG2);
    }
  } else
    return (FALSE);
  if (EndOfPAEntr(pe))
    return (FALSE);
  PELOCK(92, pe);
  if (!Yap_unify_constant(ARG3, MkIntegerTerm(pe->PredFlags))) {
    UNLOCK(pe->PELock);
    return (FALSE);
  }
  ARG4 = Deref(ARG4);
  if (IsVarTerm(ARG4)) {
    UNLOCK(pe->PELock);
    return (TRUE);
  } else if (!IsIntegerTerm(ARG4)) {
    Term te = Yap_Eval(ARG4);

    if (IsIntegerTerm(te)) {
      newFl = IntegerOfTerm(te);
    } else {
      UNLOCK(pe->PELock);
      Yap_Error(TYPE_ERROR_INTEGER, ARG4, "flags");
      return (FALSE);
    }
  } else
    newFl = IntegerOfTerm(ARG4);
  pe->PredFlags = newFl;
  UNLOCK(pe->PELock);
  return TRUE;
}

static bool pred_flag_clause(Functor f, Term mod, const char *name,
                             pred_flags_t val USES_REGS) {
  Term tn;

  Term s[2];
  s[0] = MkAtomTerm(Yap_LookupAtom(name));
#if SIZEOF_INT_P == 8
  s[1] = MkIntegerTerm(val);
#elif USE_GMP
  {
    char text[64];
    MP_INT rop;

#ifdef _WIN32
    snprintf(text, 64, "%I64d", (long long int)val);
#elif HAVE_SNPRINTF
    snprintf(text, 64, "%lld", (long long int)val);
#else
    sprintf(text, "%lld", (long long int)val);
#endif
    mpz_init_set_str(&rop, text, 10);
    s[1] = Yap_MkBigIntTerm((void *)&rop);
  }
#endif
  tn = Yap_MkApplTerm(f, 2, s);
  yamop *code_adr = Yap_cclause(tn, 2, mod, tn); /* vsc: give the number of
                            arguments to cclause() in case there is a overflow
                          */
  if (LOCAL_ErrorMessage || code_adr == 0) {
    return false;
  }
  return Yap_addclause(tn, code_adr, TermAssertz, mod, NULL);
}

struct pred_entry *Yap_MkLogPred(struct pred_entry *pe) {
  pe->PredFlags = LogUpdatePredFlag;
  pe->OpcodeOfPred = FAIL_OPCODE;
  pe->TrueCodeOfPred = pe->CodeOfPred = FAILCODE;
  return pe;
}

static Int init_pred_flag_vals(USES_REGS1) {
  Functor f;
  Term mod = Deref(ARG2), t = Deref(ARG1);

  if (IsAtomTerm(t)) {
    return false;
  } else if (IsApplTerm(t)) {
    f = FunctorOfTerm(t);
    arity_t Arity = ArityOfFunctor(f);
    if (Arity != 2)
      return false;
  } else {
    return false;
  }
  pred_flag_clause(f, mod, "asm", AsmPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "atom_db", AtomDBPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "back_c", BackCPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "c", CPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "c_args", CArgsPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "compiled", CompiledPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "count", CountPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "discontiguous", DiscontiguousPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "immediate_update", DynamicPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "hidden", HiddenPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "in_use", InUsePredFlag PASS_REGS);
  pred_flag_clause(f, mod, "indexed", IndexedPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "log_update", LogUpdatePredFlag PASS_REGS);
  pred_flag_clause(f, mod, "mega_clause", MegaClausePredFlag PASS_REGS);
  pred_flag_clause(f, mod, "meta", MetaPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "module_transparent",
                   ModuleTransparentPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "multi", MultiFileFlag PASS_REGS);
  pred_flag_clause(f, mod, "no_spy", NoSpyPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "private", NoTracePredFlag PASS_REGS);
  pred_flag_clause(f, mod, "number_db", NumberDBPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "profiled", ProfiledPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "quasi_quotation", QuasiQuotationPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "safe", SafePredFlag PASS_REGS);
  pred_flag_clause(f, mod, "sequential", SequentialPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "source", SourcePredFlag PASS_REGS);
  pred_flag_clause(f, mod, "spied", SpiedPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "standard", StandardPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "swi_env", SWIEnvPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "sync", SyncPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "sys_export", SysExportPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "tabled", TabledPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "test", TestPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "thread_local", ThreadLocalPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "udi", UDIPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "user_c", UserCPredFlag PASS_REGS);
  pred_flag_clause(f, mod, "system", SystemPredFlags PASS_REGS);
  pred_flag_clause(f, mod, "foreign", ForeignPredFlags PASS_REGS);
  return true;
}

void Yap_InitCdMgr(void) {
  CACHE_REGS
  Yap_InitCPred("$init_pred_flag_vals", 2, init_pred_flag_vals,
                SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$start_consult", 3, p_startconsult,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$show_consult_level", 1, p_showconslultlev,
                SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$end_consult", 0, p_endconsult,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$set_spy", 2, p_setspy, SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$rm_spy", 2, p_rmspy,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  /* gc() may happen during compilation, hence these predicates are
        now unsafe */
  Yap_InitCPred("$predicate_flags", 4, predicate_flags,
                SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$compile", 5, p_compile, SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$purge_clauses", 2, p_purge_clauses,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$is_dynamic", 2, p_is_dynamic,
                TestPredFlag | SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$is_metapredicate", 2, p_is_metapredicate,
                TestPredFlag | SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$is_log_updatable", 2, p_is_log_updatable,
                TestPredFlag | SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$is_thread_local", 2, p_is_thread_local,
                TestPredFlag | SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$is_source", 2, p_is_source,
                TestPredFlag | SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$is_exo", 2, p_is_exo,
                TestPredFlag | SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$owner_file", 3, owner_file, SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$set_owner_file", 3, p_set_owner_file,
                SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$mk_dynamic", 2, mk_dynamic, SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$new_meta_pred", 2, new_meta_pred,
                SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$sys_export", 2, p_sys_export,
                TestPredFlag | SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$pred_exists", 2, p_pred_exists,
                TestPredFlag | SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$number_of_clauses", 3, number_of_clauses,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$undefined", 2, p_undefined,
                SafePredFlag | TestPredFlag | NoTracePredFlag);
  Yap_InitCPred("$undefp_handler", 2, undefp_handler,
                SafePredFlag | TestPredFlag | NoTracePredFlag);
  Yap_InitCPred("$optimizer_on", 0, p_optimizer_on,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$clean_up_dead_clauses", 0, p_clean_up_dead_clauses,
                SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$optimizer_off", 0, p_optimizer_off,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$kill_dynamic", 2, p_kill_dynamic,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$new_multifile", 2, new_multifile,
                SafePredFlag | SyncPredFlag | HiddenPredFlag | NoTracePredFlag);
  Yap_InitCPred("$is_multifile", 2, p_is_multifile,
                TestPredFlag | SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$new_system_predicate", 3, new_system_predicate,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$is_system_predicate", 2, is_system_predicate,
                TestPredFlag | SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$is_opaque_predicate", 2, is_opaque_predicate,
                TestPredFlag | SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$new_discontiguous", 3, p_new_discontiguous,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$is_discontiguous", 2, p_is_discontiguous,
                TestPredFlag | SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$is_private", 2, p_is_private,
                TestPredFlag | SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$set_private", 2, p_set_private,
                SyncPredFlag | SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$is_profiled", 1, p_is_profiled,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$profile_info", 3, p_profile_info,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$profile_reset", 2, p_profile_reset,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$is_call_counted", 1, p_is_call_counted,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$call_count_info", 3, p_call_count_info,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$call_count_set", 6, p_call_count_set,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$call_count_reset", 0, p_call_count_reset,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$set_pred_module", 2, p_set_pred_module,
                SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$set_pred_owner", 2, p_set_pred_owner,
                SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$hide_predicate", 2, hide_predicate,
                SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$stash_predicate", 2, stash_predicate,
                SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$hidden_predicate", 2, hidden_predicate,
                SafePredFlag | NoTracePredFlag);
  Yap_InitCPred("$log_update_clause", 6, log_update_clause,
                SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$continue_log_update_clause", 6, continue_log_update_clause,
                SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$log_update_clause_erase", 6, log_update_clause_erase,
                SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$continue_log_update_clause_erase",6,
                continue_log_update_clause_erase,
                SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$static_clause", 6, static_clause,
                SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$continue_static_clause", 6, continue_static_clause,
                SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$static_pred_statistics", 5, p_static_pred_statistics,
                SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("instance_property", 3, instance_property,
                SafePredFlag | SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$nth_clause", 4, nth_clause,
                SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$predicate_erased_statistics", 5,
                p_predicate_erased_statistics, SyncPredFlag | NoTracePredFlag);
  Yap_InitCPred("$including", 2, including,
                SyncPredFlag | HiddenPredFlag | NoTracePredFlag);

#ifdef DEBUG
  Yap_InitCPred("$predicate_lu_cps", 4, p_predicate_lu_cps,
                0L | NoTracePredFlag);
#endif
}

void Yap_InitCLoadDB(void) {}
