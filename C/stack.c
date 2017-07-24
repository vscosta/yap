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
* File:		stack.c							 *
* comments:	Stack Introspection *
*									 *
* Last rev:     $Date: 2008-07-22 23:34:44 $,$Author: vsc $		 *
* $Log: not supported by cvs2svn $                                       *
* Revision 1.230  2008/06/02 17:20:28  vsc				 *
*									 *
*									 *
*************************************************************************/

/**
 * @file   stack.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MacBook-Pro.local>
 * @date   Tue Sep  8 23:33:02 2015
 *
 * @brief  Get to know what is in your stack.
 *
 *
 */

#include "Yap.h"
#include "clause.h"
#include "YapEval.h"
#include "iopreds.h"
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
#include <heapgc.h>

#if !defined(YAPOR) && !defined(THREADS)
static void mark_pred(int, PredEntry *);
static void do_toggle_static_predicates_in_use(int);
#endif
static Int in_use(USES_REGS1);
static Int PredForCode(yamop *, Atom *, arity_t *, Term *, PredEntry **);
static LogUpdIndex *find_owner_log_index(LogUpdIndex *, yamop *);
static StaticIndex *find_owner_static_index(StaticIndex *, yamop *);

#define IN_BLOCK(P, B, SZ)                                                     \
  ((CODEADDR)(P) >= (CODEADDR)(B) && (CODEADDR)(P) < (CODEADDR)(B) + (SZ))

static PredEntry *get_pred(Term t, Term tmod, char *pname) {
  Term t0 = t;

restart:
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t0, pname);
    return NULL;
  } else if (IsAtomTerm(t)) {
    return RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(t), tmod));
  } else if (IsIntegerTerm(t) && tmod == IDB_MODULE) {
    return Yap_FindLUIntKey(IntegerOfTerm(t));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    if (IsExtensionFunctor(fun)) {
      Yap_Error(TYPE_ERROR_CALLABLE, Yap_PredicateIndicator(t, tmod), pname);
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
    return RepPredProp(Yap_GetPredPropByFunc(fun, tmod));
  } else
    return NULL;
}

static PredEntry *PredForChoicePt(yamop *p_code, op_numbers *opn) {
  while (TRUE) {
    op_numbers opnum;
    if (!p_code)
      return NULL;
    opnum = Yap_op_from_opcode(p_code->opc);
    if (opn)
      *opn = opnum;
    switch (opnum) {
    case _Nstop:
      return NULL;
    case _jump:
      p_code = p_code->y_u.l.l;
      break;
    case _retry_me:
    case _trust_me:
      return p_code->y_u.Otapl.p;
    case _retry_exo:
    case _retry_all_exo:
      return p_code->y_u.lp.p;
    case _try_logical:
    case _retry_logical:
    case _trust_logical:
    case _count_retry_logical:
    case _count_trust_logical:
    case _profiled_retry_logical:
    case _profiled_trust_logical:
      return p_code->y_u.OtaLl.d->ClPred;
#ifdef TABLING
    case _trie_trust_var:
    case _trie_retry_var:
    case _trie_trust_var_in_pair:
    case _trie_retry_var_in_pair:
    case _trie_trust_val:
    case _trie_retry_val:
    case _trie_trust_val_in_pair:
    case _trie_retry_val_in_pair:
    case _trie_trust_atom:
    case _trie_retry_atom:
    case _trie_trust_atom_in_pair:
    case _trie_retry_atom_in_pair:
    case _trie_trust_null:
    case _trie_retry_null:
    case _trie_trust_null_in_pair:
    case _trie_retry_null_in_pair:
    case _trie_trust_pair:
    case _trie_retry_pair:
    case _trie_trust_appl:
    case _trie_retry_appl:
    case _trie_trust_appl_in_pair:
    case _trie_retry_appl_in_pair:
    case _trie_trust_extension:
    case _trie_retry_extension:
    case _trie_trust_double:
    case _trie_retry_double:
    case _trie_trust_longint:
    case _trie_retry_longint:
    case _trie_trust_gterm:
    case _trie_retry_gterm:
      return NULL;
    case _table_load_answer:
    case _table_try_answer:
    case _table_answer_resolution:
    case _table_completion:
#ifdef THREADS_CONSUMER_SHARING
    case _table_answer_resolution_completion:
#endif             /* THREADS_CONSUMER_SHARING */
      return NULL; /* ricroc: is this OK? */
                   /* compile error --> return ENV_ToP(gc_B->cp_cp); */
#endif             /* TABLING */
    case _or_else:
      if (p_code == p_code->y_u.Osblp.l) {
        /* repeat */
        Atom at = AtomRepeatSpace;
        return RepPredProp(PredPropByAtom(at, PROLOG_MODULE));
      } else {
        return p_code->y_u.Osblp.p0;
      }
      break;
    case _or_last:
#ifdef YAPOR
      return p_code->y_u.Osblp.p0;
#else
      return p_code->y_u.p.p;
#endif /* YAPOR */
      break;
    case _count_retry_me:
    case _retry_profiled:
    case _retry2:
    case _retry3:
    case _retry4:
      p_code = NEXTOP(p_code, l);
      break;
    default:
      return p_code->y_u.Otapl.p;
    }
  }
  return NULL;
}

/**
 * Yap_PredForChoicePt(): find out the predicate who generated a CP.
 *
 * @param cp the choice point
 * @param op the YAAM instruction to process next
 *
 * @return A predixate structure or NULL
 *
 * usually pretty straightforward, it can fall in trouble with
 8 OR-P or tabling.
 */
PredEntry *Yap_PredForChoicePt(choiceptr cp, op_numbers *op) {
  if (cp == NULL)
    return NULL;
  return PredForChoicePt(cp->cp_ap, op);
}

#if !defined(YAPOR) && !defined(THREADS)
static yamop *cur_clause(PredEntry *pe, yamop *codeptr) {
  StaticClause *cl;

  cl = ClauseCodeToStaticClause(pe->cs.p_code.FirstClause);
  do {
    if (IN_BLOCK(codeptr, cl, cl->ClSize)) {
      return cl->ClCode;
    }
    if (cl->ClCode == pe->cs.p_code.LastClause)
      break;
    cl = cl->ClNext;
  } while (TRUE);
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "could not find clause for indexing code");
  return (NULL);
}

static yamop *cur_log_upd_clause(PredEntry *pe, yamop *codeptr) {
  LogUpdClause *cl;
  cl = ClauseCodeToLogUpdClause(pe->cs.p_code.FirstClause);
  do {
    if (IN_BLOCK(codeptr, cl->ClCode, cl->ClSize)) {
      return ((yamop *)cl->ClCode);
    }
    cl = cl->ClNext;
  } while (cl != NULL);
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
            "could not find clause for indexing code");
  return (NULL);
}

bool Yap_search_for_static_predicate_in_use(PredEntry *p,
                                            bool check_everything) {
  choiceptr b_ptr = B;
  CELL *env_ptr = ENV;

  if (check_everything && P) {
    PredEntry *pe = EnvPreg(P);
    if (p == pe)
      return true;
    pe = EnvPreg(CP);
    if (p == pe)
      return true;
  }
  do {
    PredEntry *pe;

    /* check first environments that are younger than our latest choicepoint */
    if (check_everything && env_ptr) {
      /*
         I do not need to check environments for asserts,
         only for retracts
      */
      while (env_ptr && b_ptr > (choiceptr)env_ptr) {
        yamop *cp = (yamop *)env_ptr[E_CP];
        PredEntry *pe;

        pe = EnvPreg(cp);
        if (p == pe)
          return true;
        if (env_ptr != NULL)
          env_ptr = (CELL *)(env_ptr[E_E]);
      }
    }
    /* now mark the choicepoint */

    if (b_ptr)
      pe = PredForChoicePt(b_ptr->cp_ap, NULL);
    else
      return false;
    if (pe == p) {
      if (check_everything)
        return true;
      PELOCK(38, p);
      if (p->PredFlags & IndexedPredFlag) {
        yamop *code_p = b_ptr->cp_ap;
        yamop *code_beg = p->cs.p_code.TrueCodeOfPred;

        /* FIX ME */

        if (p->PredFlags & LogUpdatePredFlag) {
          LogUpdIndex *cl = ClauseCodeToLogUpdIndex(code_beg);
          if (find_owner_log_index(cl, code_p))
            b_ptr->cp_ap = cur_log_upd_clause(pe, b_ptr->cp_ap->y_u.Otapl.d);
        } else if (p->PredFlags & MegaClausePredFlag) {
          StaticIndex *cl = ClauseCodeToStaticIndex(code_beg);
          if (find_owner_static_index(cl, code_p))
            b_ptr->cp_ap = cur_clause(pe, b_ptr->cp_ap->y_u.Otapl.d);
        } else {
          /* static clause */
          StaticIndex *cl = ClauseCodeToStaticIndex(code_beg);
          if (find_owner_static_index(cl, code_p)) {
            b_ptr->cp_ap = cur_clause(pe, b_ptr->cp_ap->y_u.Otapl.d);
          }
        }
      }
      UNLOCKPE(63, pe);
    }
    env_ptr = b_ptr->cp_env;
    b_ptr = b_ptr->cp_b;
  } while (b_ptr != NULL);
  return (FALSE);
}

static void mark_pred(int mark, PredEntry *pe) {
  /* if the predicate is static mark it */
  if (pe->ModuleOfPred) {
    PELOCK(39, p);
    if (mark) {
      pe->PredFlags |= InUsePredFlag;
    } else {
      pe->PredFlags &= ~InUsePredFlag;
    }
    UNLOCK(pe->PELock);
  }
}

/* go up the chain of choice_points and environments,
   marking all static predicates that current execution is depending
   upon */
static void do_toggle_static_predicates_in_use(int mask) {
  choiceptr b_ptr = B;
  CELL *env_ptr = ENV;

  if (b_ptr == NULL)
    return;

  do {
    PredEntry *pe;

    /* check first environments that are younger than our latest choicepoint */
    while (b_ptr > (choiceptr)env_ptr) {
      PredEntry *pe = EnvPreg((yamop *)env_ptr[E_CP]);

      mark_pred(mask, pe);
      env_ptr = (CELL *)(env_ptr[E_E]);
    }
    /* now mark the choicepoint */
    if ((b_ptr)) {
      if ((pe = PredForChoicePt(b_ptr->cp_ap, NULL))) {
        mark_pred(mask, pe);
      }
    }
    env_ptr = b_ptr->cp_env;
    b_ptr = b_ptr->cp_b;
  } while (b_ptr != NULL);
  /* mark or unmark all predicates */
  STATIC_PREDICATES_MARKED = mask;
}

static Int toggle_static_predicates_in_use(USES_REGS1) {
#if !defined(YAPOR) && !defined(THREADS)
  Term t = Deref(ARG1);
  Int mask;

  /* find out whether we need to mark or unmark */
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "toggle_static_predicates_in_use/1");
    return (FALSE);
  }
  if (!IsIntTerm(t)) {
    Yap_Error(TYPE_ERROR_INTEGER, t, "toggle_static_predicates_in_use/1");
    return (FALSE);
  } else {
    mask = IntOfTerm(t);
  }
  do_toggle_static_predicates_in_use(mask);
#endif
  return TRUE;
}

#endif /* !defined(YAPOR) && !defined(THREADS) */

static void clause_was_found(PredEntry *pp, Atom *pat, UInt *parity) {
  if (pp->ModuleOfPred == IDB_MODULE) {
    if (pp->PredFlags & NumberDBPredFlag) {
      *parity = 0;
      *pat = AtomInteger;
    } else if (pp->PredFlags & AtomDBPredFlag) {
      *parity = 0;
      *pat = (Atom)pp->FunctorOfPred;
    } else {
      *pat = NameOfFunctor(pp->FunctorOfPred);
      *parity = ArityOfFunctor(pp->FunctorOfPred);
    }
  } else {
    if (parity) {
      *parity = pp->ArityOfPE;
    }
    if (pat) {
      if (pp->ArityOfPE) {
        *pat = NameOfFunctor(pp->FunctorOfPred);
      } else {
        *pat = (Atom)(pp->FunctorOfPred);
      }
    }
  }
}

static void code_in_pred_info(PredEntry *pp, Atom *pat, UInt *parity) {
  clause_was_found(pp, pat, parity);
}

static int code_in_pred_lu_index(LogUpdIndex *icl, yamop *codeptr,
                                 void **startp, void **endp) {
  LogUpdIndex *cicl;
  if (IN_BLOCK(codeptr, icl, icl->ClSize)) {
    if (startp)
      *startp = (CODEADDR)icl;
    if (endp)
      *endp = (CODEADDR)icl + icl->ClSize;
    return TRUE;
  }
  cicl = icl->ChildIndex;
  while (cicl != NULL) {
    if (code_in_pred_lu_index(cicl, codeptr, startp, endp))
      return TRUE;
    cicl = cicl->SiblingIndex;
  }
  return FALSE;
}

static int code_in_pred_s_index(StaticIndex *icl, yamop *codeptr, void **startp,
                                void **endp) {
  StaticIndex *cicl;
  if (IN_BLOCK(codeptr, icl, icl->ClSize)) {
    if (startp)
      *startp = (CODEADDR)icl;
    if (endp)
      *endp = (CODEADDR)icl + icl->ClSize;
    return TRUE;
  }
  cicl = icl->ChildIndex;
  while (cicl != NULL) {
    if (code_in_pred_s_index(cicl, codeptr, startp, endp))
      return TRUE;
    cicl = cicl->SiblingIndex;
  }
  return FALSE;
}

static Int find_code_in_clause(PredEntry *pp, yamop *codeptr, void **startp,
                               void **endp) {
  Int i = 1;
  yamop *clcode;

  clcode = pp->cs.p_code.FirstClause;
  if (clcode != NULL) {
    if (pp->PredFlags & LogUpdatePredFlag) {
      LogUpdClause *cl = ClauseCodeToLogUpdClause(clcode);
      do {
        if (IN_BLOCK(codeptr, (CODEADDR)cl, cl->ClSize)) {
          if (startp)
            *startp = (CODEADDR)cl;
          if (endp)
            *endp = (CODEADDR)cl + cl->ClSize;
          return i;
        }
        i++;
        cl = cl->ClNext;
      } while (cl != NULL);
    } else if (pp->PredFlags & DynamicPredFlag) {
      do {
        DynamicClause *cl;

        cl = ClauseCodeToDynamicClause(clcode);
        if (IN_BLOCK(codeptr, cl, cl->ClSize)) {
          if (startp)
            *startp = (CODEADDR)cl;
          if (endp)
            *endp = (CODEADDR)cl + cl->ClSize;
          return i;
        }
        if (clcode == pp->cs.p_code.LastClause)
          break;
        i++;
        clcode = NextDynamicClause(clcode);
      } while (TRUE);
    } else if (pp->PredFlags & MegaClausePredFlag) {
      MegaClause *cl;

      cl = ClauseCodeToMegaClause(clcode);
      if (IN_BLOCK(codeptr, cl, cl->ClSize)) {
        if (startp)
          *startp = (CODEADDR)cl;
        if (endp)
          *endp = (CODEADDR)cl + cl->ClSize;
        return 1 + ((char *)codeptr - (char *)cl->ClCode) / cl->ClItemSize;
      }
    } else {
      StaticClause *cl;

      cl = ClauseCodeToStaticClause(clcode);
      do {
        if (cl == NULL)
          return 0;
        if (IN_BLOCK(codeptr, cl, cl->ClSize)) {
          if (startp)
            *startp = (CODEADDR)cl;
          if (endp)
            *endp = (CODEADDR)cl + cl->ClSize;
          return i;
        }
        if (cl->ClCode == pp->cs.p_code.LastClause)
          break;
        i++;
        cl = cl->ClNext;
      } while (TRUE);
    }
  }
  return (0);
}

static Term clause_loc(void *clcode, PredEntry *pp) {

  CACHE_REGS
  if (pp->PredFlags & LogUpdatePredFlag) {
    LogUpdClause *cl = clcode;

    if (cl->ClFlags & FactMask) {
      return MkIntegerTerm(cl->lusl.ClLine);
    } else {
      return MkIntegerTerm(cl->lusl.ClSource->ag.line_number);
    }
  } else if (pp->PredFlags & DynamicPredFlag) {
    // DynamicClause *cl;
    // cl = ClauseCodeToDynamicClause(clcode);

    return MkIntTerm(0);
  } else if (pp->PredFlags & MegaClausePredFlag) {
    MegaClause *mcl = ClauseCodeToMegaClause(pp->cs.p_code.FirstClause);
    return MkIntTerm(mcl->ClLine);
  } else {
    StaticClause *cl;
    cl = clcode;

    if (cl->ClFlags & FactMask) {
      return MkIntTerm(cl->usc.ClLine);
    } else if (cl->ClFlags & SrcMask) {
      return MkIntTerm(cl->usc.ClSource->ag.line_number);
    } else
      return MkIntTerm(0);
  }
  return MkIntTerm(0);
}

static int cl_code_in_pred(PredEntry *pp, yamop *codeptr, void **startp,
                           void **endp) {
  Int out;

  PELOCK(39, pp);
  /* check if the codeptr comes from the indexing code */
  if (pp->PredFlags & IndexedPredFlag) {
    if (pp->PredFlags & LogUpdatePredFlag) {
      if (code_in_pred_lu_index(
              ClauseCodeToLogUpdIndex(pp->cs.p_code.TrueCodeOfPred), codeptr,
              startp, endp)) {
        UNLOCK(pp->PELock);
        return TRUE;
      }
    } else {
      if (code_in_pred_s_index(
              ClauseCodeToStaticIndex(pp->cs.p_code.TrueCodeOfPred), codeptr,
              startp, endp)) {
        UNLOCK(pp->PELock);
        return TRUE;
      }
    }
  }
  if (pp->PredFlags & (CPredFlag | AsmPredFlag | UserCPredFlag)) {
    StaticClause *cl = ClauseCodeToStaticClause(pp->CodeOfPred);
    if (IN_BLOCK(codeptr, (CODEADDR)cl, cl->ClSize)) {
      if (startp)
        *startp = (CODEADDR)cl;
      if (endp)
        *endp = (CODEADDR)cl + cl->ClSize;
      UNLOCK(pp->PELock);
      return TRUE;
    } else {
      UNLOCK(pp->PELock);
      return FALSE;
    }
  } else {
    out = find_code_in_clause(pp, codeptr, startp, endp);
  }
  UNLOCK(pp->PELock);
  if (out)
    return TRUE;
  return FALSE;
}

static Int code_in_pred(PredEntry *pp, Atom *pat, UInt *parity,
                        yamop *codeptr) {
  Int out;

  PELOCK(40, pp);
  /* check if the codeptr comes from the indexing code */
  if (pp->PredFlags & IndexedPredFlag) {
    if (pp->PredFlags & LogUpdatePredFlag) {
      if (code_in_pred_lu_index(
              ClauseCodeToLogUpdIndex(pp->cs.p_code.TrueCodeOfPred), codeptr,
              NULL, NULL)) {
        code_in_pred_info(pp, pat, parity);
        UNLOCK(pp->PELock);
        return -1;
      }
    } else {
      if (code_in_pred_s_index(
              ClauseCodeToStaticIndex(pp->cs.p_code.TrueCodeOfPred), codeptr,
              NULL, NULL)) {
        code_in_pred_info(pp, pat, parity);
        UNLOCK(pp->PELock);
        return -1;
      }
    }
  }
  if ((out = find_code_in_clause(pp, codeptr, NULL, NULL))) {
    clause_was_found(pp, pat, parity);
  }
  UNLOCK(pp->PELock);
  return out;
}

static Int PredForCode(yamop *codeptr, Atom *pat, UInt *parity, Term *pmodule,
                       PredEntry **pep) {
  Int found = 0;
  ModEntry *me = CurrentModules;

  /* should we allow the user to see hidden predicates? */
  while (me) {

    PredEntry *pp;
    pp = me->PredForME;
    while (pp != NULL) {
      if ((found = code_in_pred(pp, pat, parity, codeptr)) != 0) {
        if (pmodule)
          *pmodule = MkAtomTerm(me->AtomOfME);
        if (pep)
          *pep = pp;
        return found;
      }
      pp = pp->NextPredOfModule;
    }
    me = me->NextME;
  }
  return (0);
}

Int Yap_PredForCode(yamop *codeptr, find_pred_type where_from, Atom *pat,
                    UInt *parity, Term *pmodule) {
  PredEntry *p;

  if (where_from == FIND_PRED_FROM_CP) {
    p = PredForChoicePt(codeptr, NULL);
  } else if (where_from == FIND_PRED_FROM_ENV) {
    p = EnvPreg(codeptr);
    if (p) {
      Int out;
      if (p->ModuleOfPred == PROLOG_MODULE)
        *pmodule = TermProlog;
      else
        *pmodule = p->ModuleOfPred;
      out = find_code_in_clause(p, codeptr, NULL, NULL);
      clause_was_found(p, pat, parity);
      return out;
    }
  } else {
    return PredForCode(codeptr, pat, parity, pmodule, NULL);
  }
  if (p == NULL) {
    return 0;
  }
  clause_was_found(p, pat, parity);
  if (p->ModuleOfPred == PROLOG_MODULE)
    *pmodule = TermProlog;
  else
    *pmodule = p->ModuleOfPred;
  return -1;
}

/* intruction blocks we found ourselves at */
static PredEntry *walk_got_lu_block(LogUpdIndex *cl, void **startp,
                                    void **endp) {
  PredEntry *pp = cl->ClPred;
  *startp = (CODEADDR)cl;
  *endp = (CODEADDR)cl + cl->ClSize;
  return pp;
}

/* intruction blocks we found ourselves at */
static PredEntry *walk_got_lu_clause(LogUpdClause *cl, void **startp,
                                     void **endp) {
  *startp = (CODEADDR)cl;
  *endp = (CODEADDR)cl + cl->ClSize;
  return cl->ClPred;
}

/* we hit a meta-call, so we don't know what is happening */
static PredEntry *found_meta_call(void **startp, void **endp) {
  PredEntry *pp = PredMetaCall;
  *startp = (CODEADDR) & (pp->OpcodeOfPred);
  *endp = (CODEADDR)NEXTOP((yamop *)&(pp->OpcodeOfPred), e);
  return pp;
}

/* intruction blocks we found ourselves at */
static PredEntry *walk_found_c_pred(PredEntry *pp, void **startp, void **endp) {
  StaticClause *cl = ClauseCodeToStaticClause(pp->CodeOfPred);
  *startp = (CODEADDR) & (cl->ClCode);
  *endp = (CODEADDR) & (cl->ClCode) + cl->ClSize;
  return pp;
}

/* we hit a mega-clause, no point in going on */
static PredEntry *found_mega_clause(PredEntry *pp, void **startp, void **endp) {
  MegaClause *mcl = ClauseCodeToMegaClause(pp->cs.p_code.FirstClause);
  *startp = (CODEADDR)mcl;
  *endp = (CODEADDR)mcl + mcl->ClSize;
  return pp;
}

/* we hit a mega-clause, no point in going on */
static PredEntry *found_idb_clause(yamop *pc, void **startp, void **endp) {
  LogUpdClause *cl = ClauseCodeToLogUpdClause(pc);

  *startp = (CODEADDR)cl;
  *endp = (CODEADDR)cl + cl->ClSize;
  return cl->ClPred;
}

/* we hit a expand_index, no point in going on */
static PredEntry *found_expand_index(yamop *pc, void **startp, void **endp,
                                     yamop *codeptr USES_REGS) {
  PredEntry *pp = codeptr->y_u.sssllp.p;
  if (pc == codeptr) {
    *startp = (CODEADDR)codeptr;
    *endp = (CODEADDR)NEXTOP(codeptr, sssllp);
  }
  return pp;
}

/* we hit a expand_index, no point in going on */
static PredEntry *found_fail(yamop *pc, void **startp, void **endp USES_REGS) {
  PredEntry *pp = RepPredProp(Yap_GetPredPropByAtom(AtomFail, CurrentModule));
  *startp = *endp = (CODEADDR)FAILCODE;
  return pp;
}

/* we hit a expand_index, no point in going on */
static PredEntry *found_owner_op(yamop *pc, void **startp,
                                 void **endp USES_REGS) {
  PredEntry *pp = ((PredEntry *)(Unsigned(pc) -
                                 (CELL)(&(((PredEntry *)NULL)->OpcodeOfPred))));
  *startp = (CODEADDR) & (pp->OpcodeOfPred);
  *endp = (CODEADDR)NEXTOP((yamop *)&(pp->OpcodeOfPred), e);
  return pp;
}

/* we hit a expand_index, no point in going on */
static PredEntry *found_expand(yamop *pc, void **startp,
                               void **endp USES_REGS) {
  PredEntry *pp =
      ((PredEntry *)(Unsigned(pc) -
                     (CELL)(&(((PredEntry *)NULL)->cs.p_code.ExpandCode))));
  *startp = (CODEADDR) & (pp->cs.p_code.ExpandCode);
  *endp = (CODEADDR)NEXTOP((yamop *)&(pp->cs.p_code.ExpandCode), e);
  return pp;
}

static PredEntry *found_ystop(yamop *pc, int clause_code, void **startp, void **endp, PredEntry *pp USES_REGS) {
  if (pc == YESCODE) {
    pp = RepPredProp(Yap_GetPredPropByAtom(AtomTrue, CurrentModule));
    if (startp)
      *startp = (CODEADDR)YESCODE;
    if (endp)
      *endp = (CODEADDR)YESCODE + (CELL)(NEXTOP((yamop *)NULL, e));
    return pp;
 }                                                                                                 
 if (!pp) {
   yamop *o = PREVOP(pc,pp);
   if (o->opc ==Yap_opcode(_execute_cpred)) {
     pp = o->y_u.pp.p0;
   } else {
     /* must be an index */
    PredEntry **pep = (PredEntry **)pc->y_u.l.l;
    pp = pep[-1];
  }
 }
  if (pp->PredFlags & LogUpdatePredFlag) {
    if (clause_code) {
      LogUpdClause *cl = ClauseCodeToLogUpdClause(pc->y_u.l.l);
      *startp = (CODEADDR)cl;
      *endp = (CODEADDR)cl + cl->ClSize;
    } else {
      LogUpdIndex *cl = ClauseCodeToLogUpdIndex(pc->y_u.l.l);
      *startp = (CODEADDR)cl;
      *endp = (CODEADDR)cl + cl->ClSize;
    }
  } else if (pp->PredFlags & DynamicPredFlag) {
    DynamicClause *cl = ClauseCodeToDynamicClause(pc->y_u.l.l);
    *startp = (CODEADDR)cl;
    *endp = (CODEADDR)cl + cl->ClSize;
  } else {
    if (clause_code) {
      StaticClause *cl = ClauseCodeToStaticClause(pc->y_u.l.l);
      *startp = (CODEADDR)cl;
      *endp = (CODEADDR)cl + cl->ClSize;
    } else {
      StaticIndex *cl = ClauseCodeToStaticIndex(pc->y_u.l.l);
      *startp = (CODEADDR)cl;
      *endp = (CODEADDR)cl + cl->ClSize;
    }
  }
  return pp;
}

static PredEntry *ClauseInfoForCode(yamop *codeptr, void **startp,
                                    void **endp USES_REGS) {
  yamop *pc;
  PredEntry *pp = NULL;
  int clause_code = FALSE;

  if (codeptr >= COMMA_CODE && codeptr < FAILCODE) {
    pp = RepPredProp(Yap_GetPredPropByFunc(FunctorComma, CurrentModule));
    *startp = (CODEADDR)COMMA_CODE;
    *endp = (CODEADDR)(FAILCODE - 1);
    return pp;
  }
  pc = codeptr;
#include "walkclause.h"
  return NULL;
}

PredEntry *Yap_PredEntryForCode(yamop *codeptr, find_pred_type where_from,
                                void **startp, void **endp) {
  CACHE_REGS
  if (where_from == FIND_PRED_FROM_CP) {
    PredEntry *pp = PredForChoicePt(codeptr, NULL);
    if (cl_code_in_pred(pp, codeptr, startp, endp)) {
      return pp;
    }
  } else if (where_from == FIND_PRED_FROM_ENV) {
    PredEntry *pp = EnvPreg(codeptr);
    if (cl_code_in_pred(pp, codeptr, startp, endp)) {
      return pp;
    }
  } else {
    return ClauseInfoForCode(codeptr, startp, endp PASS_REGS);
  }
  return NULL;
}

/**
 * Detect whether the predicate describing the goal in A1,
 * module A2 is currently live in the stack.
 *
 * @param USES_REGS1
 *
 * @return liveness
 */
static Int in_use(USES_REGS1) { /* '$in_use'(+P,+Mod)	 */
  PredEntry *pe;
  Int out;

  pe = get_pred(Deref(ARG1), Deref(ARG2), "$in_use");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(25, pe);
  out = Yap_static_in_use(pe, TRUE);
  UNLOCKPE(42, pe);
  return (out);
}

static Int pred_for_code(USES_REGS1) {
  yamop *codeptr;
  Atom at;
  arity_t arity;
  Term tmodule = TermProlog;
  Int cl;
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    return FALSE;
  } else if (IsApplTerm(t) && FunctorOfTerm(t) == FunctorStaticClause) {
    codeptr = Yap_ClauseFromTerm(t)->ClCode;
  } else if (IsIntegerTerm(t)) {
    codeptr = (yamop *)IntegerOfTerm(t);
  } else if (IsDBRefTerm(t)) {
    codeptr = (yamop *)DBRefOfTerm(t);
  } else {
    return FALSE;
  }
  cl = PredForCode(codeptr, &at, &arity, &tmodule, NULL);
  if (!tmodule)
    tmodule = TermProlog;
  if (cl == 0) {
    return Yap_unify(ARG5, MkIntTerm(0));
  } else {
    return (Yap_unify(ARG2, MkAtomTerm(at)) &&
            Yap_unify(ARG3, MkIntegerTerm(arity)) && Yap_unify(ARG4, tmodule) &&
            Yap_unify(ARG5, MkIntegerTerm(cl)));
  }
}

static LogUpdIndex *find_owner_log_index(LogUpdIndex *cl, yamop *code_p) {
  yamop *code_beg = cl->ClCode;
  yamop *code_end = (yamop *)((char *)cl + cl->ClSize);

  if (code_p >= code_beg && code_p <= code_end) {
    return cl;
  }
  cl = cl->ChildIndex;
  while (cl != NULL) {
    LogUpdIndex *out;
    if ((out = find_owner_log_index(cl, code_p)) != NULL) {
      return out;
    }
    cl = cl->SiblingIndex;
  }
  return NULL;
}

static StaticIndex *find_owner_static_index(StaticIndex *cl, yamop *code_p) {
  yamop *code_beg = cl->ClCode;
  yamop *code_end = (yamop *)((char *)cl + cl->ClSize);

  if (code_p >= code_beg && code_p <= code_end) {
    return cl;
  }
  cl = cl->ChildIndex;
  while (cl != NULL) {
    StaticIndex *out;
    if ((out = find_owner_static_index(cl, code_p)) != NULL) {
      return out;
    }
    cl = cl->SiblingIndex;
  }
  return NULL;
}

ClauseUnion *Yap_find_owner_index(yamop *ipc, PredEntry *ap) {
  /* we assume we have an owner index */
  if (ap->PredFlags & LogUpdatePredFlag) {
    LogUpdIndex *cl = ClauseCodeToLogUpdIndex(ap->cs.p_code.TrueCodeOfPred);
    return (ClauseUnion *)find_owner_log_index(cl, ipc);
  } else {
    StaticIndex *cl = ClauseCodeToStaticIndex(ap->cs.p_code.TrueCodeOfPred);
    return (ClauseUnion *)find_owner_static_index(cl, ipc);
  }
}

static Term all_envs(CELL *env_ptr USES_REGS) {
  Term tf = AbsPair(HR);
  CELL *start = HR;
  CELL *bp = NULL;

  /* walk the environment chain */
  while (env_ptr) {
    bp = HR;
    HR += 2;
    /* notice that MkIntegerTerm may increase the HReap */
    bp[0] = MkIntegerTerm(LCL0 - env_ptr);
    if (HR >= ASP - 1024) {
      HR = start;
      LOCAL_Error_Size = (ASP - 1024) - HR;
      while (env_ptr) {
        LOCAL_Error_Size += 2;
        env_ptr = (CELL *)(env_ptr[E_E]);
      }
      return 0L;
    } else {
      bp[1] = AbsPair(HR);
    }
    env_ptr = (CELL *)(env_ptr[E_E]);
  }
  bp[1] = TermNil;
  return tf;
}

static Term all_cps(choiceptr b_ptr USES_REGS) {
  CELL *bp = NULL;
  CELL *start = HR;
  Term tf = AbsPair(HR);

  while (b_ptr) {
    bp = HR;
    HR += 2;
    /* notice that MkIntegerTerm may increase the HReap */
    bp[0] = MkIntegerTerm((Int)(LCL0 - (CELL *)b_ptr));
    if (HR >= ASP - 1024) {
      HR = start;
      LOCAL_Error_Size = (ASP - 1024) - HR;
      while (b_ptr) {
        LOCAL_Error_Size += 2;
        b_ptr = b_ptr->cp_b;
      }
      return 0L;
    } else {
      bp[1] = AbsPair(HR);
    }
    b_ptr = b_ptr->cp_b;
    if (!IsVarTerm((CELL)b_ptr) || (CELL *)b_ptr < HR || (CELL *)b_ptr > LCL0) {
      // Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,  "choice-point chain
      // corrupted at %p!!!\n", b_ptr);
      break;
    }
  }
  bp[1] = TermNil;
  return tf;
}

static Int p_all_choicepoints(USES_REGS1) {
  Term t;
  while ((t = all_cps(B PASS_REGS)) == 0L) {
    if (!Yap_gcl(LOCAL_Error_Size, 1, ENV, gc_P(P, CP))) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil, "while dumping choicepoints");
      return FALSE;
    }
  }
  return Yap_unify(ARG1, t);
}

static Int p_all_envs(USES_REGS1) {
  Term t;
  while ((t = all_envs(ENV PASS_REGS)) == 0L) {
    if (!Yap_gcl(LOCAL_Error_Size, 1, ENV, gc_P(P, CP))) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil, "while dumping environments");
      return FALSE;
    }
  }
  return Yap_unify(ARG1, t);
}

static Term clause_info(yamop *codeptr, PredEntry *pp) {
  CACHE_REGS
  Term ts[2];
  void *begin;

  if (pp->ArityOfPE == 0) {
    ts[0] = MkAtomTerm((Atom)pp->FunctorOfPred);
    ts[1] = MkIntTerm(0);
  } else {
    ts[0] = MkAtomTerm(NameOfFunctor(pp->FunctorOfPred));
    ts[1] = MkIntegerTerm(pp->ArityOfPE);
  }

  ts[0] = MkAtomTerm(pp->src.OwnerFile);
  Term t1 = Yap_MkApplTerm(FunctorModule, 2, ts);
  if ((find_code_in_clause(pp, codeptr, &begin, NULL)) <= 0) {
    ts[0] = clause_loc(pp->cs.p_code.FirstClause, pp);
    ts[1] = clause_loc(pp->cs.p_code.LastClause, pp);
    if (ts[0] == ts[1] && ts[1] != TermNil) {
    } else if (ts[1] == TermNil && ts[0] != MkIntTerm(0))
      ts[0] = Yap_MkApplTerm(FunctorMinus, 2, ts);
  }
  ts[1] = t1;
  return Yap_MkApplTerm(FunctorModule, 2, ts);
}

bool set_clause_info(yamop *codeptr, PredEntry *pp) {
  CACHE_REGS
  Term ts[2];
  void *begin;
  if (pp->ArityOfPE == 0) {
    LOCAL_ActiveError->prologPredName = (Atom)pp->FunctorOfPred;
    LOCAL_ActiveError->prologPredArity = 0;
  } else {
    LOCAL_ActiveError->prologPredName = NameOfFunctor(pp->FunctorOfPred);
    LOCAL_ActiveError->prologPredArity = pp->ArityOfPE;
  }
  LOCAL_ActiveError->prologPredModule =
      (pp->ModuleOfPred ? pp->ModuleOfPred : TermProlog);
  LOCAL_ActiveError->prologPredFile = pp->src.OwnerFile;
  if (codeptr->opc == UNDEF_OPCODE) {
    LOCAL_ActiveError->prologPredFirstLine = 0;
    LOCAL_ActiveError->prologPredLine = 0;
    LOCAL_ActiveError->prologPredLastLine = 0;
    return true;
  } else if (pp->cs.p_code.NOfClauses) {
    if ((LOCAL_ActiveError->prologPredCl =
             find_code_in_clause(pp, codeptr, &begin, NULL)) <= 0) {
      LOCAL_ActiveError->prologPredLine = 0;
    } else {
      LOCAL_ActiveError->prologPredLine = IntegerOfTerm(clause_loc(begin, pp));
    }
    if (pp->PredFlags & LogUpdatePredFlag) {
      LOCAL_ActiveError->prologPredFirstLine = IntegerOfTerm(
          ts[0] = clause_loc(
              ClauseCodeToLogUpdClause(pp->cs.p_code.FirstClause), pp));
      LOCAL_ActiveError->prologPredLastLine = IntegerOfTerm(
          ts[1] = clause_loc(ClauseCodeToLogUpdClause(pp->cs.p_code.LastClause),
                             pp));

    } else {
      LOCAL_ActiveError->prologPredFirstLine = IntegerOfTerm(
          ts[0] = clause_loc(
              ClauseCodeToStaticClause(pp->cs.p_code.FirstClause), pp));
      LOCAL_ActiveError->prologPredLastLine = IntegerOfTerm(
          ts[1] = clause_loc(ClauseCodeToStaticClause(pp->cs.p_code.LastClause),
                             pp));
    }
    return true;
  } else {
    return false;
  }
}

static Term error_culprit(bool internal USES_REGS) {
  PredEntry *pe;
  // case number 1: Yap_Error called from built-in.
  void *startp, *endp;
  // case number 1: Yap_Error called from built-in.
  pe = ClauseInfoForCode(P, &startp, &endp PASS_REGS);
  if (internal) {
    return clause_info(P, pe);
  } else {
    CELL *curENV = ENV;
    yamop *curCP = CP;
    PredEntry *pe = EnvPreg(curCP);

    while (curCP != YESCODE) {
      if (pe->ModuleOfPred)
        return clause_info(curCP, pe);
      curENV = (CELL *)(curENV[E_E]);
      curCP = (yamop *)(curENV[E_CP]);
      pe = EnvPreg(curCP);
    }
  }
  return TermNil;
}

bool Yap_find_prolog_culprit(USES_REGS1) {
  PredEntry *pe;
  void *startp, *endp;
  // case number 1: Yap_Error called from built-in.
  pe = ClauseInfoForCode(P, &startp, &endp PASS_REGS);
  if (pe && (CurrentModule == 0 || !(pe->PredFlags & HiddenPredFlag))) {
    return set_clause_info(P, pe);
  } else {
    CELL *curENV = ENV;
    yamop *curCP = CP;
    PredEntry *pe = EnvPreg(curCP);

    while (curCP != YESCODE) {
      curENV = (CELL *)(curENV[E_E]);
      if (curENV < ASP || curENV >= LCL0)
        break;
      pe = EnvPreg(curCP);
      if (pe==NULL) {
        pe = PredMetaCall;
      }
      if (pe->ModuleOfPred)
        return set_clause_info(curCP, pe);
      curCP = (yamop *)(curENV[E_CP]);
    }
  }
  return TermNil;
}

static Term all_calls(bool internal USES_REGS) {
  Term ts[6];
  Functor f = Yap_MkFunctor(AtomLocalSp, 6);

  // The first argument is key: it tries to
  // catch the culprit at the user level,
  ts[0] = error_culprit(internal PASS_REGS);
  ts[1] = MkAddressTerm(P);
  ts[2] = MkAddressTerm(CP);
  ts[3] = MkAddressTerm(PP);
  if (trueLocalPrologFlag(STACK_DUMP_ON_ERROR_FLAG)) {
    ts[4] = all_envs(ENV PASS_REGS);
    ts[5] = all_cps(B PASS_REGS);
    if (ts[4] == 0L || ts[5] == 0L)
      return 0L;
  } else {
    ts[4] = ts[5] = TermNil;
  }
  return Yap_MkApplTerm(f, 6, ts);
}

/**
 * report the current status of the stacks up to level $N$
 *
 * @param depth
 *
 * @return data on the current program counter
 */

Term Yap_all_calls(void) {
  CACHE_REGS
  return all_calls(true PASS_REGS);
}

static Int current_stack(USES_REGS1) {
  Term t;
  while ((t = all_calls(false PASS_REGS)) == 0L) {
    if (!Yap_gcl(LOCAL_Error_Size, 1, ENV, gc_P(P, CP))) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil, "while dumping stack");
      return FALSE;
    }
  }
  return Yap_unify(ARG1, t);
}

#if LOW_PROF

static void add_code_in_lu_index(LogUpdIndex *cl, PredEntry *pp) {
  char *code_end = (char *)cl + cl->ClSize;
  Yap_inform_profiler_of_clause(cl, code_end, pp, GPROF_LU_INDEX);
  cl = cl->ChildIndex;
  while (cl != NULL) {
    add_code_in_lu_index(cl, pp);
    cl = cl->SiblingIndex;
  }
}

static void add_code_in_static_index(StaticIndex *cl, PredEntry *pp) {
  char *code_end = (char *)cl + cl->ClSize;
  Yap_inform_profiler_of_clause(cl, code_end, pp, GPROF_STATIC_INDEX);
  cl = cl->ChildIndex;
  while (cl != NULL) {
    add_code_in_static_index(cl, pp);
    cl = cl->SiblingIndex;
  }
}

static void add_code_in_pred(PredEntry *pp) {
  yamop *clcode;

  PELOCK(49, pp);
  /* check if the codeptr comes from the indexing code */

  /* highly likely this is used for indexing */
  Yap_inform_profiler_of_clause(&(pp->OpcodeOfPred), &(pp->OpcodeOfPred) + 1,
                                pp, GPROF_INIT_OPCODE);
  if (pp->PredFlags & (CPredFlag | AsmPredFlag)) {
    char *code_end;
    StaticClause *cl;

    clcode = pp->CodeOfPred;
    cl = ClauseCodeToStaticClause(clcode);
    code_end = (char *)cl + cl->ClSize;
    Yap_inform_profiler_of_clause(cl, code_end, pp, GPROF_INIT_SYSTEM_CODE);
    UNLOCK(pp->PELock);
    return;
  }
  Yap_inform_profiler_of_clause(&(pp->cs.p_code.ExpandCode),
                                &(pp->cs.p_code.ExpandCode) + 1, pp,
                                GPROF_INIT_EXPAND);
  clcode = pp->cs.p_code.TrueCodeOfPred;
  if (pp->PredFlags & IndexedPredFlag) {
    if (pp->PredFlags & LogUpdatePredFlag) {
      LogUpdIndex *cl = ClauseCodeToLogUpdIndex(clcode);
      add_code_in_lu_index(cl, pp);
    } else {
      StaticIndex *cl = ClauseCodeToStaticIndex(clcode);
      add_code_in_static_index(cl, pp);
    }
  }
  clcode = pp->cs.p_code.FirstClause;
  if (clcode != NULL) {
    if (pp->PredFlags & LogUpdatePredFlag) {
      LogUpdClause *cl = ClauseCodeToLogUpdClause(clcode);
      do {
        char *code_end;

        code_end = (char *)cl + cl->ClSize;
        Yap_inform_profiler_of_clause(cl, code_end, pp,
                                      GPROF_INIT_LOG_UPD_CLAUSE);
        cl = cl->ClNext;
      } while (cl != NULL);
    } else if (pp->PredFlags & DynamicPredFlag) {
      do {
        DynamicClause *cl;
        CODEADDR code_end;

        cl = ClauseCodeToDynamicClause(clcode);
        code_end = (CODEADDR)cl + cl->ClSize;
        Yap_inform_profiler_of_clause(cl, code_end, pp,
                                      GPROF_INIT_DYNAMIC_CLAUSE);
        if (clcode == pp->cs.p_code.LastClause)
          break;
        clcode = NextDynamicClause(clcode);
      } while (TRUE);
    } else {
      StaticClause *cl = ClauseCodeToStaticClause(clcode);
      do {
        char *code_end;

        code_end = (char *)cl + cl->ClSize;
        Yap_inform_profiler_of_clause(cl, code_end, pp,
                                      GPROF_INIT_STATIC_CLAUSE);
        if (cl->ClCode == pp->cs.p_code.LastClause)
          break;
        cl = cl->ClNext;
      } while (TRUE);
    }
  }
  UNLOCK(pp->PELock);
}

void Yap_dump_code_area_for_profiler(void) {
  ModEntry *me = CurrentModules;

  while (me) {
    PredEntry *pp = me->PredForME;

    while (pp != NULL) {
      /*      if (pp->ArityOfPE) {
        fprintf(stderr,"%s/%d %p\n",
                RepAtom(NameOfFunctor(pp->FunctorOfPred))->StrOfAE,
                pp->ArityOfPE,
                pp);
      } else {
        fprintf(stderr,"%s %p\n",
                RepAtom((Atom)(pp->FunctorOfPred))->StrOfAE,
                pp);
                }*/
      add_code_in_pred(pp);
      pp = pp->NextPredOfModule;
    }
    me = me->NextME;
  }
  Yap_inform_profiler_of_clause(
      COMMA_CODE, FAILCODE, RepPredProp(Yap_GetPredPropByFunc(FunctorComma, 0)),
      GPROF_INIT_COMMA);
  Yap_inform_profiler_of_clause(FAILCODE, FAILCODE + 1,
                                RepPredProp(Yap_GetPredPropByAtom(AtomFail, 0)),
                                GPROF_INIT_FAIL);
}

#endif /* LOW_PROF */

static Int program_continuation(USES_REGS1) {
  PredEntry *pe = EnvPreg((yamop *)((ENV_Parent(ENV))[E_CP]));
  if (pe->ModuleOfPred) {
    if (!Yap_unify(ARG1, pe->ModuleOfPred))
      return FALSE;
  } else {
    if (!Yap_unify(ARG1, TermProlog))
      return FALSE;
  }
  if (pe->ArityOfPE) {
    if (!Yap_unify(ARG2, MkAtomTerm(NameOfFunctor(pe->FunctorOfPred))))
      return FALSE;
    if (!Yap_unify(ARG3, MkIntegerTerm(ArityOfFunctor(pe->FunctorOfPred))))
      return FALSE;
  } else {
    if (!Yap_unify(ARG2, MkAtomTerm((Atom)pe->FunctorOfPred)))
      return FALSE;
    if (!Yap_unify(ARG3, MkIntTerm(0)))
      return FALSE;
  }
  return TRUE;
}

static Term BuildActivePred(PredEntry *ap, CELL *vect) {
  CACHE_REGS
  arity_t i;

  if (!ap->ArityOfPE) {
    return MkVarTerm();
  }
  for (i = 0; i < ap->ArityOfPE; i++) {
    Term t = Deref(vect[i]);
    if (IsVarTerm(t)) {
      CELL *pt = VarOfTerm(t);
      /* one stack */
      if (pt > HR) {
        Term nt = MkVarTerm();
        Yap_unify(t, nt);
      }
    }
  }
  return Yap_MkApplTerm(ap->FunctorOfPred, ap->ArityOfPE, vect);
}

static int UnifyPredInfo(PredEntry *pe, int start_arg USES_REGS) {
  arity_t arity = pe->ArityOfPE;
  Term tmod, tname;

  if (pe->ModuleOfPred != IDB_MODULE) {
    if (pe->ModuleOfPred == PROLOG_MODULE) {
      tmod = TermProlog;
    } else {
      tmod = pe->ModuleOfPred;
    }
    if (pe->ArityOfPE == 0) {
      tname = MkAtomTerm((Atom)pe->FunctorOfPred);
    } else {
      Functor f = pe->FunctorOfPred;
      tname = MkAtomTerm(NameOfFunctor(f));
    }
  } else {
    tmod = pe->ModuleOfPred;
    if (pe->PredFlags & NumberDBPredFlag) {
      tname = MkIntegerTerm(pe->src.IndxId);
    } else if (pe->PredFlags & AtomDBPredFlag) {
      tname = MkAtomTerm((Atom)pe->FunctorOfPred);
    } else {
      Functor f = pe->FunctorOfPred;
      tname = MkAtomTerm(NameOfFunctor(f));
    }
  }

  return Yap_unify(XREGS[start_arg], tmod) &&
         Yap_unify(XREGS[start_arg + 1], tname) &&
         Yap_unify(XREGS[start_arg + 2], MkIntegerTerm(arity));
}

static Int ClauseId(yamop *ipc, PredEntry *pe) {
  if (!ipc)
    return 0;
  return find_code_in_clause(pe, ipc, NULL, NULL);
}

static Int env_info(USES_REGS1) {
  CELL *env = LCL0 - IntegerOfTerm(Deref(ARG1));
  yamop *env_cp;
  Term env_b, taddr;

  if (!env)
    return FALSE;
  env_b = MkIntegerTerm((Int)(LCL0 - (CELL *)env[E_CB]));
  env_cp = (yamop *)env[E_CP];

  /* pe = PREVOP(env_cp,Osbpp)->y_u.Osbpp.p0; */
  taddr = MkIntegerTerm((Int)env);
  return Yap_unify(ARG3, MkIntegerTerm((Int)env_cp)) &&
         Yap_unify(ARG2, taddr) && Yap_unify(ARG4, env_b);
}

static Int p_cpc_info(USES_REGS1) {
  PredEntry *pe;
  yamop *ipc = (yamop *)IntegerOfTerm(Deref(ARG1));

  pe = PREVOP(ipc, Osbpp)->y_u.Osbpp.p0;
  return UnifyPredInfo(pe, 2 PASS_REGS) &&
         Yap_unify(ARG5, MkIntegerTerm(ClauseId(ipc, pe)));
}

static Int p_choicepoint_info(USES_REGS1) {
  choiceptr cptr = (choiceptr)(LCL0 - IntegerOfTerm(Deref(ARG1)));
  PredEntry *pe = NULL;
  int go_on = TRUE;
  yamop *ipc = cptr->cp_ap;
  yamop *ncl = NULL;
  Term t = TermNil, taddr;

  taddr = MkIntegerTerm((Int)cptr);
  while (go_on) {
    op_numbers opnum = Yap_op_from_opcode(ipc->opc);
    go_on = FALSE;
    switch (opnum) {
#ifdef TABLING
    case _table_load_answer:
#ifdef LOW_LEVEL_TRACER
      pe = LOAD_CP(cptr)->cp_pred_entry;
#else
      pe = UndefCode;
#endif
      t = MkVarTerm();
      break;
    case _table_try_answer:
    case _table_retry_me:
    case _table_trust_me:
    case _table_retry:
    case _table_trust:
    case _table_completion:
#ifdef THREADS_CONSUMER_SHARING
    case _table_answer_resolution_completion:
#endif /* THREADS_CONSUMER_SHARING */
#ifdef LOW_LEVEL_TRACER
#ifdef DETERMINISTIC_TABLING
      if (IS_DET_GEN_CP(cptr)) {
        pe = DET_GEN_CP(cptr)->cp_pred_entry;
        t = MkVarTerm();
      } else
#endif /* DETERMINISTIC_TABLING */
      {
        pe = GEN_CP(cptr)->cp_pred_entry;
        t = BuildActivePred(pe, (CELL *)(GEN_CP(B) + 1));
      }
#else
      pe = UndefCode;
      t = MkVarTerm();
#endif
      break;
    case _table_answer_resolution:
#ifdef LOW_LEVEL_TRACER
      pe = CONS_CP(cptr)->cp_pred_entry;
#else
      pe = UndefCode;
#endif
      t = MkVarTerm();
      break;
    case _trie_trust_var:
    case _trie_retry_var:
    case _trie_trust_var_in_pair:
    case _trie_retry_var_in_pair:
    case _trie_trust_val:
    case _trie_retry_val:
    case _trie_trust_val_in_pair:
    case _trie_retry_val_in_pair:
    case _trie_trust_atom:
    case _trie_retry_atom:
    case _trie_trust_atom_in_pair:
    case _trie_retry_atom_in_pair:
    case _trie_trust_null:
    case _trie_retry_null:
    case _trie_trust_null_in_pair:
    case _trie_retry_null_in_pair:
    case _trie_trust_pair:
    case _trie_retry_pair:
    case _trie_trust_appl:
    case _trie_retry_appl:
    case _trie_trust_appl_in_pair:
    case _trie_retry_appl_in_pair:
    case _trie_trust_extension:
    case _trie_retry_extension:
    case _trie_trust_double:
    case _trie_retry_double:
    case _trie_trust_longint:
    case _trie_retry_longint:
    case _trie_trust_gterm:
    case _trie_retry_gterm:
      pe = UndefCode;
      t = MkVarTerm();
      break;
#endif /* TABLING */
    case _try_logical:
    case _retry_logical:
    case _trust_logical:
    case _count_retry_logical:
    case _count_trust_logical:
    case _profiled_retry_logical:
    case _profiled_trust_logical:
      ncl = ipc->y_u.OtaLl.d->ClCode;
      pe = ipc->y_u.OtaLl.d->ClPred;
      t = BuildActivePred(pe, cptr->cp_args);
      break;
    case _or_else:
      pe = ipc->y_u.Osblp.p0;
      ncl = ipc;
      t = Yap_MkNewApplTerm(FunctorOr, 2);
      break;

    case _or_last:
#ifdef YAPOR
      pe = ipc->y_u.Osblp.p0;
#else
      pe = ipc->y_u.p.p;
#endif
      ncl = ipc;
      t = Yap_MkNewApplTerm(FunctorOr, 2);
      break;
    case _retry2:
    case _retry3:
    case _retry4:
      pe = NULL;
      t = TermNil;
      ipc = NEXTOP(ipc, l);
      if (!ncl)
        ncl = ipc->y_u.Otapl.d;
      go_on = TRUE;
      break;
    case _jump:
      pe = NULL;
      t = TermNil;
      ipc = ipc->y_u.l.l;
      go_on = TRUE;
      break;
    case _retry_c:
    case _retry_userc:
      ncl = NEXTOP(ipc, OtapFs);
      pe = ipc->y_u.OtapFs.p;
      t = BuildActivePred(pe, cptr->cp_args);
      break;
    case _retry_profiled:
    case _count_retry:
      pe = NULL;
      t = TermNil;
      ncl = ipc->y_u.Otapl.d;
      ipc = NEXTOP(ipc, p);
      go_on = TRUE;
      break;
    case _retry_me:
    case _trust_me:
    case _count_retry_me:
    case _count_trust_me:
    case _profiled_retry_me:
    case _profiled_trust_me:
    case _retry_and_mark:
    case _profiled_retry_and_mark:
    case _retry:
    case _trust:
      if (!ncl)
        ncl = ipc->y_u.Otapl.d;
      pe = ipc->y_u.Otapl.p;
      t = BuildActivePred(pe, cptr->cp_args);
      break;
    case _retry_exo:
    case _retry_all_exo:
      ncl = NULL;
      pe = ipc->y_u.lp.p;
      t = BuildActivePred(pe, cptr->cp_args);
      break;
    case _Nstop: {
      Atom at = AtomLive;
      t = MkAtomTerm(at);
      pe = RepPredProp(PredPropByAtom(at, CurrentModule));
    } break;
    case _Ystop:
    default:
      return FALSE;
    }
  }
  return UnifyPredInfo(pe, 3 PASS_REGS) && Yap_unify(ARG2, taddr) &&
         Yap_unify(ARG6, t) &&
         Yap_unify(ARG7, MkIntegerTerm(ClauseId(ncl, pe)));
}

static Int /* $parent_pred(Module, Name, Arity) */
    parent_pred(USES_REGS1) {
  /* This predicate is called from the debugger.
     We assume a sequence of the form a -> b */
  Atom at;
  arity_t arity;
  Term module;
  if (!PredForCode(P_before_spy, &at, &arity, &module, NULL)) {
    return Yap_unify(ARG1, MkIntTerm(0)) &&
           Yap_unify(ARG2, MkAtomTerm(AtomMetaCall)) &&
           Yap_unify(ARG3, MkIntTerm(0));
  }
  return Yap_unify(ARG1, MkIntTerm(module)) &&
         Yap_unify(ARG2, MkAtomTerm(at)) && Yap_unify(ARG3, MkIntTerm(arity));
}

void Yap_dump_stack(void);
void DumpActiveGoals(CACHE_TYPE1);
static int hidden(Atom);
static int legal_env(CELL *CACHE_TYPE);

#define ONLOCAL(ptr)                                                           \
  (CellPtr(ptr) > CellPtr(HR) && CellPtr(ptr) < CellPtr(LOCAL_LocalBase))

static int hidden(Atom at) {
  AtomEntry *chain;

  READ_LOCK(INVISIBLECHAIN.AERWLock);
  chain = RepAtom(INVISIBLECHAIN.Entry);
  while (!EndOfPAEntr(chain) && AbsAtom(chain) != at)
    chain = RepAtom(chain->NextOfAE);
  READ_UNLOCK(INVISIBLECHAIN.AERWLock);
  if (EndOfPAEntr(chain))
    return (FALSE);
  return (TRUE);
}

static int legal_env(CELL *ep USES_REGS) {
  CELL cp, ps;
  PredEntry *pe;
  if (!ONLOCAL(ep) || Unsigned(ep) & 3)
    return (FALSE);
  cp = ep[E_CP];
  if (!ONHEAP(cp))
    return (FALSE);
  ps = *((CELL *)(Addr(cp) - CellSize));
  pe = (PredEntry *)(ps - sizeof(OPREG) - sizeof(Prop));
  PELOCK(70, pe);
  if (!ONHEAP(pe) || Unsigned(pe) & 3 || pe->KindOfPE & 0xff00) {
    UNLOCK(pe->PELock);
    return (FALSE);
  }
  UNLOCK(pe->PELock);
  return (TRUE);
}

static bool handled_exception(USES_REGS1) {
  yamop *pos = NEXTOP(PredDollarCatch->cs.p_code.TrueCodeOfPred, l);
  bool found_handler = false;
  choiceptr gc_b;

  gc_b = B;
  while (gc_b) {
    yamop *ap = gc_b->cp_ap;
    if (ap == NOCODE) {
      /* C-code: let they deal with that */
      return false;
    } else if (ap == pos) {
      if (found_handler)
        return TRUE; /* we have two handlers */
      found_handler = true;
    }
    gc_b = gc_b->cp_b;
  }
  /* handled by Top c-code? */
  return !found_handler;
}

void Yap_dump_stack(void) {
  CACHE_REGS
  choiceptr b_ptr = B;
  CELL *env_ptr = ENV;
  char tp[256];
  yamop *ipc = CP;
  int max_count = 200;

  /* check if handled */
  if (handled_exception(PASS_REGS1))
    return;
#if DEBUG
  fprintf(stderr, "%% YAP regs: P=%p, CP=%p, ASP=%p, H=%p, TR=%p, HeapTop=%p\n",
          P, CP, ASP, HR, TR, HeapTop);
  fprintf(stderr, "%% YAP mode: %ux\n", (unsigned int)LOCAL_PrologMode);
  if (LOCAL_ErrorMessage)
    fprintf(stderr, "%% LOCAL_ErrorMessage: %s\n", LOCAL_ErrorMessage);
#endif
  if (HR > ASP || HR > LCL0) {
    fprintf(stderr, "%% YAP ERROR: Global Collided against Local (%p--%p)\n",
            HR, ASP);
  } else if (HeapTop > (ADDR)LOCAL_GlobalBase) {
    fprintf(stderr,
            "%% YAP ERROR: Code Space Collided against Global (%p--%p)\n",
            HeapTop, LOCAL_GlobalBase);
  } else {
#if !USE_SYSTEM_MALLOC
    fprintf(stderr, "%ldKB of Code Space (%p--%p)\n",
            (long int)((CELL)HeapTop - (CELL)Yap_HeapBase) / 1024, Yap_HeapBase,
            HeapTop);
#if USE_DL_MALLOC
    if (Yap_NOfMemoryHoles) {
      UInt i;

      for (i = 0; i < Yap_NOfMemoryHoles; i++)
        fprintf(stderr, "  Current hole: %p--%p\n", Yap_MemoryHoles[i].start,
                Yap_MemoryHoles[i].end);
    }
#endif
#endif
    Yap_detect_bug_location(P, FIND_PRED_FROM_ANYWHERE, 256);
    fprintf(stderr, "%%\n%% PC: %s\n", (char *)HR);
    Yap_detect_bug_location(CP, FIND_PRED_FROM_ANYWHERE, 256);
    fprintf(stderr, "%%   Continuation: %s\n", (char *)HR);
    fprintf(stderr, "%%    %luKB of Global Stack (%p--%p)\n",
            (unsigned long int)(sizeof(CELL) * (HR - H0)) / 1024, H0, HR);
    fprintf(stderr, "%%    %luKB of Local Stack (%p--%p)\n",
            (unsigned long int)(sizeof(CELL) * (LCL0 - ASP)) / 1024, ASP, LCL0);
    fprintf(stderr, "%%    %luKB of Trail (%p--%p)\n",
            (unsigned long int)((ADDR)TR - LOCAL_TrailBase) / 1024,
            LOCAL_TrailBase, TR);
    fprintf(stderr, "%%    Performed %ld garbage collections\n",
            (unsigned long int)LOCAL_GcCalls);
#if LOW_LEVEL_TRACER
    {
      extern long long vsc_count;

      if (vsc_count) {
#if _WIN32
        fprintf(stderr, "Trace Counter at %I64d\n", vsc_count);
#else
        fprintf(stderr, "Trace Counter at %lld\n", vsc_count);
#endif
      }
    }
#endif
    fprintf(stderr, "%% All Active Calls and\n");
    fprintf(stderr, "%%         Goals With Alternatives Open  (Global In "
                    "Use--Local In Use)\n%%\n");
    while (b_ptr != NULL) {
      while (env_ptr && env_ptr <= (CELL *)b_ptr) {
        Yap_detect_bug_location(ipc, FIND_PRED_FROM_ENV, 256);
        if (env_ptr == (CELL *)b_ptr && (choiceptr)env_ptr[E_CB] > b_ptr) {
          b_ptr = b_ptr->cp_b;
          fprintf(stderr, "%%  %s\n", tp);
        } else {
          fprintf(stderr, "%%  %s\n", tp);
        }
        if (!max_count--) {
          fprintf(stderr, "%%  .....\n");
          return;
        }
        ipc = (yamop *)(env_ptr[E_CP]);
        env_ptr = (CELL *)(env_ptr[E_E]);
      }
      if (b_ptr) {
        if (!max_count--) {
          fprintf(stderr, "%%  .....\n");
          return;
        }
        if (b_ptr->cp_ap && /* tabling */
            b_ptr->cp_ap->opc != Yap_opcode(_or_else) &&
            b_ptr->cp_ap->opc != Yap_opcode(_or_last) &&
            b_ptr->cp_ap->opc != Yap_opcode(_Nstop)) {
          /* we can safely ignore ; because there is always an upper env */
          Yap_detect_bug_location(b_ptr->cp_ap, FIND_PRED_FROM_CP, 256);
          fprintf(stderr, "%%         %s (%luKB--%luKB)\n", tp,
                  (unsigned long int)((b_ptr->cp_h - H0) * sizeof(CELL) / 1024),
                  (unsigned long int)((ADDR)LCL0 - (ADDR)b_ptr) / 1024);
        }
        b_ptr = b_ptr->cp_b;
      }
    }
  }
}

void DumpActiveGoals(USES_REGS1) {
  /* try to dump active goals */
  CELL *ep = YENV; /* and current environment		  */
  choiceptr b_ptr = B;
  CELL cp;
  PredEntry *pe;
  int first = 1;

  if (legal_env(YENV PASS_REGS) && YENV < ENV)
    ep = YENV;
  else if (legal_env(ENV PASS_REGS))
    ep = ENV;
  while (TRUE) {
    if (!ONLOCAL(ep) || (Unsigned(ep) & (sizeof(CELL) - 1)))
      break;
    cp = ep[E_CP];
    if (!ONHEAP(cp) || (Unsigned(cp) & (sizeof(CELL) - 1)))
      break;
    pe = EnvPreg((yamop *)cp);
    if (!ONHEAP(pe) || Unsigned(pe) & (sizeof(CELL) - 1))
      break;
    PELOCK(71, pe);
    if (pe->KindOfPE & 0xff00) {
      UNLOCK(pe->PELock);
      break;
    }
    if (pe->PredFlags & (CompiledPredFlag | DynamicPredFlag)) {
      Functor f;

      UNLOCK(pe->PELock);
      f = pe->FunctorOfPred;
      if (pe->KindOfPE && hidden(NameOfFunctor(f)))
        goto next;
      if (first++ == 1)
        fprintf(stderr, "Active ancestors:\n");
      Term mod = pe->ModuleOfPred;
      if (mod == PROLOG_MODULE)
        mod = TermProlog;
      Term t = Yap_MkNewApplTerm(f, pe->ArityOfPE);
      Yap_plwrite(Yap_PredicateIndicator(t, mod), GLOBAL_Stream + 2, 0, 0,
                  GLOBAL_MaxPriority);
      fputc('\n', stderr);
    } else {
      UNLOCK(pe->PELock);
    }
  next:
    ep = (CELL *)ep[E_E];
  }
  first = 1;
  fprintf(stderr, "Active Choice-Points:\n");
  while (TRUE) {
    PredEntry *pe;
    op_numbers opnum;
    if (!ONLOCAL(b_ptr) || b_ptr->cp_b == NULL)
      break;
    fprintf(stderr, "%p ", b_ptr);
    pe = Yap_PredForChoicePt(b_ptr, &opnum);
    if (opnum == _Nstop) {
      fprintf(stderr, "  ********** C-Code Interface Boundary ***********\n");
    } else {
      Functor f;
      Term mod = PROLOG_MODULE;

      f = pe->FunctorOfPred;
      if (pe->ModuleOfPred)
        mod = pe->ModuleOfPred;
      else
        mod = TermProlog;
      if (mod != TermProlog && mod != MkAtomTerm(AtomUser)) {
        Yap_plwrite(mod, GLOBAL_Stream + 2, 0, 0, GLOBAL_MaxPriority);
        fputc(':', stderr);
      }
      if (mod == IDB_MODULE) {
        if (pe->PredFlags & NumberDBPredFlag) {
          Int id = pe->src.IndxId;
          Yap_plwrite(MkIntegerTerm(id), GLOBAL_Stream + 2, 0, 0,
                      GLOBAL_MaxPriority);
        } else if (pe->PredFlags & AtomDBPredFlag) {
          Atom At = (Atom)pe->FunctorOfPred;
          Yap_plwrite(MkAtomTerm(At), GLOBAL_Stream + 2, 0, 0,
                      GLOBAL_MaxPriority);
        } else {
          Functor f = pe->FunctorOfPred;
          Atom At = NameOfFunctor(f);
          arity_t arity = ArityOfFunctor(f);
          int i;

          Yap_plwrite(MkAtomTerm(At), GLOBAL_Stream + 2, 0, 0,
                      GLOBAL_MaxPriority);
          fputc('(', stderr);
          for (i = 0; i < arity; i++) {
            if (i > 0)
              fputc(',', stderr);
            fputc('_', stderr);
          }
          fputc(')', stderr);
        }
        fputc('(', stderr);
        Yap_plwrite(b_ptr->cp_a2, GLOBAL_Stream + 2, 0, 0, GLOBAL_MaxPriority);
        fputc(')', stderr);
      } else if (pe->ArityOfPE == 0) {
        Yap_plwrite(MkAtomTerm((Atom)f), GLOBAL_Stream + 2, 0, 0,
                    GLOBAL_MaxPriority);
      } else {
        Int i = 0, arity = pe->ArityOfPE;
        if (opnum == _or_last || opnum == _or_else) {
          Yap_plwrite(MkAtomTerm(NameOfFunctor(f)), GLOBAL_Stream + 2, 0, 0,
                      GLOBAL_MaxPriority);
          fputc('(', stderr);
          for (i = 0; i < arity; i++) {
            if (i > 0)
              fputc(',', stderr);
            fputc('_', stderr);
          }
          fputs(") :- ... ( _  ; _ ", stderr);
        } else {
          Term *args = &(b_ptr->cp_a1);
          Yap_plwrite(MkAtomTerm(NameOfFunctor(f)), GLOBAL_Stream + 2, 0, 0,
                      GLOBAL_MaxPriority);
          fputc('(', stderr);
          for (i = 0; i < arity; i++) {
            if (i > 0)
              fputc(',', stderr);
            Yap_plwrite(args[i], GLOBAL_Stream + 2, 0, 0, GLOBAL_MaxPriority);
          }
        }
        fputc(')', stderr);
      }
      fputc('\n', stderr);
    }
    b_ptr = b_ptr->cp_b;
  }
}

void Yap_detect_bug_location(yamop *yap_pc, int where_from, int psize) {
  Atom pred_name;
  UInt pred_arity;
  Term pred_module;
  Int cl;

  if ((cl = Yap_PredForCode(yap_pc, where_from, &pred_name, &pred_arity,
                            &pred_module)) == 0) {
    /* system predicate */
    fprintf(stderr, "%s", "meta-call");
  } else if (pred_module == 0) {
    fprintf(stderr, "in prolog:%s/%lu", RepAtom(pred_name)->StrOfAE,
            (unsigned long int)pred_arity);
  } else if (cl < 0) {
    fprintf(stderr, "%s:%s/%lu", RepAtom(AtomOfTerm(pred_module))->StrOfAE,
            RepAtom(pred_name)->StrOfAE, (unsigned long int)pred_arity);
  } else {
    fprintf(stderr, "%s:%s/%lu at clause %lu",
            RepAtom(AtomOfTerm(pred_module))->StrOfAE,
            RepAtom(pred_name)->StrOfAE, (unsigned long int)pred_arity,
            (unsigned long int)cl);
  }
}

static Term build_bug_location(yamop *codeptr, PredEntry *pe) {
  CACHE_REGS
  Term p[5];
  if (pe->ModuleOfPred == PROLOG_MODULE)
    p[0] = TermProlog;
  else
    p[0] = pe->ModuleOfPred;
  if (pe->ArityOfPE)
    p[1] = MkAtomTerm(NameOfFunctor(pe->FunctorOfPred));
  else
    p[1] = MkAtomTerm((Atom)pe->FunctorOfPred);
  p[2] = MkIntegerTerm(pe->ArityOfPE);
  if (pe->src.OwnerFile) {
    p[3] = MkAtomTerm(pe->src.OwnerFile);
    if (pe->PredFlags & MegaClausePredFlag) {
      MegaClause *mcl;
      mcl = ClauseCodeToMegaClause(pe->cs.p_code.FirstClause);
      p[4] = MkIntegerTerm(mcl->ClLine);
    } else {
      void *clcode;
      if (find_code_in_clause(pe, codeptr, &clcode, NULL) > 0) {
        if (pe->PredFlags & LogUpdatePredFlag) {
          LogUpdClause *cl = clcode;

          if (cl->ClFlags & FactMask) {
            p[4] = MkIntegerTerm(cl->lusl.ClLine);
          } else {
            p[4] = MkIntegerTerm(cl->lusl.ClSource->ag.line_number);
          }
        } else if (pe->PredFlags & DynamicPredFlag) {

          p[4] = MkIntTerm(0);
        } else {
          StaticClause *cl;
          cl = clcode;

          if (cl->ClFlags & FactMask) {
            p[4] = MkIntTerm(cl->usc.ClLine);
          } else if (cl->ClFlags & SrcMask) {
            p[4] = MkIntTerm(cl->usc.ClSource->ag.line_number);
          } else
            p[4] = MkIntTerm(0);
        }
      } else {
        p[4] = MkIntTerm(0);
      }
    }
  } else if (pe->OpcodeOfPred == UNDEF_OPCODE) {
    RESET_VARIABLE(p + 3);
    RESET_VARIABLE(p + 4);
  } else {
    // by default, user_input
    p[3] = MkAtomTerm(AtomUserIn);
    p[4] = MkIntTerm(0);
  }
  return Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("p"), 5), 5, p);
}

Term Yap_pc_location(yamop *pc, choiceptr b_ptr, CELL *env) {
  CACHE_REGS
  yamop *codeptr = pc;
  PredEntry *pe;
  if (PP == NULL) {
    if (PredForCode(pc, NULL, NULL, NULL, &pe) <= 0)
      return TermNil;
  } else
    pe = PP;
  if (pe != NULL
      // pe->ModuleOfPred != PROLOG_MODULE &&
      // &&!(pe->PredFlags & HiddenPredFlag)
      ) {
    return build_bug_location(codeptr, pe);
  }
  return TermNil;
}

Term Yap_env_location(yamop *cp, choiceptr b_ptr, CELL *env, Int ignore_first) {
  while (true) {
    if (b_ptr == NULL || env == NULL)
      return TermNil;
    PredEntry *pe = EnvPreg(cp);
    if (pe == PredTrue)
      return TermNil;
    if (ignore_first <= 0 && pe
        // pe->ModuleOfPred != PROLOG_MODULE &&s
        && !(pe->PredFlags & HiddenPredFlag)) {
      return build_bug_location(cp, pe);
    } else {
      if (NULL && b_ptr && b_ptr->cp_env < env) {
        cp = b_ptr->cp_cp;
        env = b_ptr->cp_env;
        b_ptr = b_ptr->cp_b;
      } else {
        cp = (yamop *)env[E_CP];
        env = ENV_Parent(env);
      }
      ignore_first--;
    }
  }
}

static Int clause_location(USES_REGS1) {
  return Yap_unify(Yap_pc_location(P, B, ENV), ARG1) &&
         Yap_unify(Yap_env_location(CP, B, ENV, 1), ARG2);
}

static Int ancestor_location(USES_REGS1) {
  return Yap_unify(Yap_env_location(CP, B, ENV, 2), ARG1) &&
         Yap_unify(Yap_env_location(CP, B, ENV, 3), ARG2);
}

void Yap_InitStInfo(void) {
  CACHE_REGS
  Term cm = CurrentModule;

  Yap_InitCPred("in_use", 2, in_use,
                HiddenPredFlag | TestPredFlag | SafePredFlag | SyncPredFlag);
#ifndef THREADS
  Yap_InitCPred("toggle_static_predicates_in_use", 0,
                toggle_static_predicates_in_use,
                HiddenPredFlag | SafePredFlag | SyncPredFlag);
#endif
  CurrentModule = HACKS_MODULE;
  Yap_InitCPred("current_choicepoints", 1, p_all_choicepoints, 0);
  Yap_InitCPred("current_continuations", 1, p_all_envs, 0);
  Yap_InitCPred("choicepoint", 7, p_choicepoint_info, 0);
  Yap_InitCPred("continuation", 4, env_info, 0);
  Yap_InitCPred("cp_to_predicate", 5, p_cpc_info, 0);
  CurrentModule = cm;
  Yap_InitCPred("current_stack", 1, current_stack, HiddenPredFlag);
  Yap_InitCPred("pred_for_code", 5, pred_for_code, HiddenPredFlag);
  Yap_InitCPred("parent_pred", 3, parent_pred, HiddenPredFlag | SafePredFlag);
  Yap_InitCPred("program_continuation", 3, program_continuation,
                HiddenPredFlag | SafePredFlag);
  Yap_InitCPred("clause_location", 2, clause_location,
                HiddenPredFlag | SafePredFlag);
  Yap_InitCPred("ancestor_location", 2, ancestor_location,
                HiddenPredFlag | SafePredFlag);
}
