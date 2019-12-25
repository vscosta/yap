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
* comments:	Stack Introspection						 *
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
` */

#include "Yap.h"
#include "clause.h"
#include "yapio.h"
#include "YapEval.h"
#include "tracer.h"
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#if HAVE_STRING_H
#include <string.h>
#endif
#include <heapgc.h>

static int  static_in_use(PredEntry *, int);
#if !defined(YAPOR) && !defined(THREADS)
static Int  search_for_static_predicate_in_use(PredEntry *, int);
static void mark_pred(int, PredEntry *);
static void do_toggle_static_predicates_in_use(int);
#endif
static Int  in_use( USES_REGS1 );
static Int  toggle_static_predicates_in_use( USES_REGS1 );
static Int  PredForCode(yamop *, Atom *, arity_t *, Term *);

static PredEntry *
PredForChoicePt(yamop *p_code, op_numbers *opn) {
  while (TRUE) {
    op_numbers opnum;
    if (!p_code)
      return NULL;
    opnum = Yap_op_from_opcode(p_code->opc);
    if (opn)
        *opn = opnum;
    switch(opnum) {
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
#endif /* THREADS_CONSUMER_SHARING */
      return NULL; /* ricroc: is this OK? */
      /* compile error --> return ENV_ToP(gc_B->cp_cp); */
#endif /* TABLING */
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
      p_code = NEXTOP(p_code,l);
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
PredEntry *
Yap_PredForChoicePt(choiceptr cp, op_numbers *op) {
  if (cp == NULL)
    return NULL;
  return PredForChoicePt(cp->cp_ap, op);
}

#if !defined(YAPOR) && !defined(THREADS)
static yamop *cur_clause(PredEntry *pe, yamop *codeptr)
{
  StaticClause *cl;

  cl = ClauseCodeToStaticClause(pe->FirstClause);
  do {
    if (IN_BLOCK(codeptr,cl,cl->ClSize)) {
      return cl->ClCode;
    }
    if (cl->ClCode == pe->LastClause)
      break;
    cl = cl->ClNext;
  } while (TRUE);
  Yap_Error(SYSTEM_ERROR_INTERNAL,TermNil,"could not find clause for indexing code");
  return(NULL);
}

static yamop *cur_log_upd_clause(PredEntry *pe, yamop *codeptr)
{
  LogUpdClause *cl;
  cl = ClauseCodeToLogUpdClause(pe->FirstClause);
  do {
    if (IN_BLOCK(codeptr,cl->ClCode,cl->ClSize)) {
      return((yamop *)cl->ClCode);
    }
    cl = cl->ClNext;
  } while (cl != NULL);
  Yap_Error(SYSTEM_ERROR_INTERNAL,TermNil,"could not find clause for indexing code");
  return(NULL);
}

static Int
search_for_static_predicate_in_use(PredEntry *p, int check_everything)
{
  choiceptr b_ptr = B;
  CELL *env_ptr = ENV;

  if (check_everything && P) {
    PredEntry *pe = EnvPreg(P);
    if (p == pe) return TRUE;
    pe = EnvPreg(CP);
    if (p == pe) return TRUE;
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
	if (p == pe) return(TRUE);
	if (env_ptr != NULL)
	  env_ptr = (CELL *)(env_ptr[E_E]);
      }
    }
    /* now mark the choicepoint */

    if (b_ptr)
      pe = PredForChoicePt(b_ptr->cp_ap, NULL);
    else
      return FALSE;
    if (pe == p) {
      if (check_everything)
	return TRUE;
      PELOCK(38,p);
      if (p->PredFlags & IndexedPredFlag) {
	yamop *code_p = b_ptr->cp_ap;
	yamop *code_beg = p->TrueCodeOfPred;

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
      UNLOCKPE(63,pe);
    }
    env_ptr = b_ptr->cp_env;
    b_ptr = b_ptr->cp_b;
  } while (b_ptr != NULL);
  return(FALSE);
}

static void
mark_pred(int mark, PredEntry *pe)
{
  /* if the predicate is static mark it */
  if (pe->ModuleOfPred) {
    PELOCK(39,p);
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
static void
do_toggle_static_predicates_in_use(int mask)
{
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

#endif /* !defined(YAPOR) && !defined(THREADS) */

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
static PredEntry *
walk_got_lu_block(LogUpdIndex *cl, CODEADDR *startp, CODEADDR *endp)
{
  PredEntry *pp = cl->ClPred;
  *startp = (CODEADDR)cl;
  *endp = (CODEADDR)cl+cl->ClSize;
  return pp;
}

/* intruction blocks we found ourselves at */
static PredEntry *
walk_got_lu_clause(LogUpdClause *cl, CODEADDR *startp, CODEADDR *endp)
{
  *startp = (CODEADDR)cl;
  *endp = (CODEADDR)cl+cl->ClSize;
  return cl->ClPred;
}

/* we hit a meta-call, so we don't know what is happening */
static PredEntry *
found_meta_call(CODEADDR *startp, CODEADDR *endp)
{
  PredEntry *pp = PredMetaCall;
  *startp = (CODEADDR)&(pp->OpcodeOfPred);
  *endp = (CODEADDR)NEXTOP((yamop *)&(pp->OpcodeOfPred),e);
  return pp;
}

/* intruction blocks we found ourselves at */
static PredEntry *
walk_found_c_pred(PredEntry *pp, CODEADDR *startp, CODEADDR *endp)
{
  StaticClause  *cl = ClauseCodeToStaticClause(pp->CodeOfPred);
  *startp = (CODEADDR)&(cl->ClCode);
  *endp = (CODEADDR)&(cl->ClCode)+cl->ClSize;
  return pp;
}

/* we hit a mega-clause, no point in going on */
static PredEntry *
found_mega_clause(PredEntry *pp, CODEADDR *startp, CODEADDR *endp)
{
  MegaClause *mcl = ClauseCodeToMegaClause(pp->FirstClause);
  *startp = (CODEADDR)mcl;
  *endp = (CODEADDR)mcl+mcl->ClSize;
  return pp;
}

/* we hit a mega-clause, no point in going on */
static PredEntry *
found_idb_clause(yamop *pc, CODEADDR *startp, CODEADDR *endp)
{
  LogUpdClause *cl = ClauseCodeToLogUpdClause(pc);

  *startp = (CODEADDR)cl;
  *endp = (CODEADDR)cl+cl->ClSize;
  return cl->ClPred;
}

/* we hit a expand_index, no point in going on */
static PredEntry *
found_expand_index(yamop *pc, CODEADDR *startp, CODEADDR *endp, yamop *codeptr USES_REGS)
{
  PredEntry *pp = codeptr->y_u.sssllp.p;
  if (pc == codeptr) {
    *startp = (CODEADDR)codeptr;
    *endp = (CODEADDR)NEXTOP(codeptr,sssllp);
  }
  return pp;
}

/* we hit a expand_index, no point in going on */
static PredEntry *
found_fail(yamop *pc, CODEADDR *startp, CODEADDR *endp USES_REGS)
{
  PredEntry *pp = RepPredProp(Yap_GetPredPropByAtom(AtomFail,CurrentModule));
  *startp = *endp = (CODEADDR)FAILCODE;
  return pp;
}

/* we hit a expand_index, no point in going on */
static PredEntry *
found_owner_op(yamop *pc, CODEADDR *startp, CODEADDR *endp USES_REGS)
{
  PredEntry *pp = ((PredEntry *)(Unsigned(pc)-(CELL)(&(((PredEntry *)NULL)->OpcodeOfPred))));
  *startp = (CODEADDR)&(pp->OpcodeOfPred);
  *endp = (CODEADDR)NEXTOP((yamop *)&(pp->OpcodeOfPred),e);
  return pp;
}

/* we hit a expand_index, no point in going on */
static PredEntry *
found_expand(yamop *pc, CODEADDR *startp, CODEADDR *endp USES_REGS)
{
  PredEntry *pp = ((PredEntry *)(Unsigned(pc)-(CELL)(&(((PredEntry *)NULL)->cs.p_code.ExpandCode))));
  *startp = (CODEADDR)&(pp->cs.p_code.ExpandCode);
  *endp = (CODEADDR)NEXTOP((yamop *)&(pp->cs.p_code.ExpandCode),e);
  return pp;
}

static PredEntry *
found_ystop(yamop *pc, int clause_code, CODEADDR *startp, CODEADDR *endp, PredEntry *pp USES_REGS)
{
  if (pc == YESCODE) {
    pp = RepPredProp(Yap_GetPredPropByAtom(AtomTrue,CurrentModule));
    *startp = (CODEADDR)YESCODE;
    *endp = (CODEADDR)YESCODE+(CELL)(NEXTOP((yamop *)NULL,e));
    return pp;
  }
  if (!pp) {
    /* must be an index */
    PredEntry **pep = (PredEntry **)pc->y_u.l.l;
    pp = pep[-1];
  }
  if (pp->PredFlags & LogUpdatePredFlag) {
    if (clause_code) {
      LogUpdClause *cl = ClauseCodeToLogUpdClause(pc->y_u.l.l);
      *startp = (CODEADDR)cl;
      *endp = (CODEADDR)cl+cl->ClSize;
    } else {
      LogUpdIndex *cl = ClauseCodeToLogUpdIndex(pc->y_u.l.l);
      *startp = (CODEADDR)cl;
      *endp = (CODEADDR)cl+cl->ClSize;
    }
  } else if (pp->PredFlags & DynamicPredFlag) {
    DynamicClause *cl = ClauseCodeToDynamicClause(pc->y_u.l.l);
    *startp = (CODEADDR)cl;
    *endp = (CODEADDR)cl+cl->ClSize;
  } else {
    if (clause_code) {
      StaticClause *cl = ClauseCodeToStaticClause(pc->y_u.l.l);
      *startp = (CODEADDR)cl;
      *endp = (CODEADDR)cl+cl->ClSize;
    } else {
      StaticIndex *cl = ClauseCodeToStaticIndex(pc->y_u.l.l);
      *startp = (CODEADDR)cl;
      *endp = (CODEADDR)cl+cl->ClSize;
    }
  }
  return pp;
}

static PredEntry *
ClauseInfoForCode(yamop *codeptr, CODEADDR *startp, CODEADDR *endp USES_REGS) {
  yamop *pc;
  PredEntry *pp = NULL;
  int clause_code = FALSE;

  if (codeptr >= COMMA_CODE &&
	codeptr < FAILCODE) {
    pp = RepPredProp(Yap_GetPredPropByFunc(FunctorComma,CurrentModule));
    *startp = (CODEADDR)COMMA_CODE;
    *endp = (CODEADDR)(FAILCODE-1);
    return pp;
  }
  pc = codeptr;
#include "walkclause.h"
  return NULL;
}

PredEntry *
Yap_PredEntryForCode(yamop *codeptr, find_pred_type where_from, CODEADDR *startp, CODEADDR *endp) {
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
static Int
p_in_use( USES_REGS1 )
{				/* '$in_use'(+P,+Mod)	 */
  PredEntry      *pe;
  Int            out;

  pe = get_pred(Deref(ARG1),  Deref(ARG2), "$in_use");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(25,pe);
  out = static_in_use(pe,TRUE);
  UNLOCKPE(42,pe);
  return(out);
}


static Int
p_pred_for_code( USES_REGS1 ) {
  yamop *codeptr;
  Atom at;
  arity_t arity;
  Term tmodule = TermProlog;
  Int cl;
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    return FALSE;
  } else if (IsApplTerm(t) && FunctorOfTerm(t) == FunctorStaticClause) {
    codeptr  = Yap_ClauseFromTerm(t)->ClCode;
  } else if (IsIntegerTerm(t)) {
    codeptr  = (yamop *)IntegerOfTerm(t);
  } else if (IsDBRefTerm(t)) {
    codeptr  = (yamop *)DBRefOfTerm(t);
  } else {
    return FALSE;
  }
  cl = PredForCode(codeptr, &at, &arity, &tmodule);
  if (!tmodule) tmodule = TermProlog;
  if (cl == 0) {
    return Yap_unify(ARG5,MkIntTerm(0));
  } else {
    return(Yap_unify(ARG2,MkAtomTerm(at)) &&
	   Yap_unify(ARG3,MkIntegerTerm(arity)) &&
	   Yap_unify(ARG4,tmodule) &&
	   Yap_unify(ARG5,MkIntegerTerm(cl)));
  }
}

#if LOW_PROF

static void
add_code_in_lu_index(LogUpdIndex *cl, PredEntry *pp)
{
  char *code_end = (char *)cl + cl->ClSize;
  Yap_inform_profiler_of_clause(cl, code_end, pp, GPROF_LU_INDEX);
  cl = cl->ChildIndex;
  while (cl != NULL) {
    add_code_in_lu_index(cl, pp);
    cl = cl->SiblingIndex;
  }
}

static void
add_code_in_static_index(StaticIndex *cl, PredEntry *pp)
{
  char *code_end = (char *)cl + cl->ClSize;
  Yap_inform_profiler_of_clause(cl, code_end, pp, GPROF_STATIC_INDEX);
  cl = cl->ChildIndex;
  while (cl != NULL) {
    add_code_in_static_index(cl, pp);
    cl = cl->SiblingIndex;
  }
}


static void
add_code_in_pred(PredEntry *pp) {
  yamop *clcode;

  PELOCK(49,pp);
  /* check if the codeptr comes from the indexing code */

  /* highly likely this is used for indexing */
  Yap_inform_profiler_of_clause(&(pp->OpcodeOfPred), &(pp->OpcodeOfPred)+1, pp, GPROF_INIT_OPCODE);
  if (pp->PredFlags & (CPredFlag|AsmPredFlag)) {
    char *code_end;
    StaticClause *cl;

    clcode = pp->CodeOfPred;
    cl = ClauseCodeToStaticClause(clcode);
    code_end = (char *)cl + cl->ClSize;
    Yap_inform_profiler_of_clause(cl, code_end, pp, GPROF_INIT_SYSTEM_CODE);
    UNLOCK(pp->PELock);
    return;
  }
  Yap_inform_profiler_of_clause(&(pp->cs.p_code.ExpandCode), &(pp->cs.p_code.ExpandCode)+1, pp, GPROF_INIT_EXPAND);
  clcode = pp->TrueCodeOfPred;
  if (pp->PredFlags & IndexedPredFlag) {
    if (pp->PredFlags & LogUpdatePredFlag) {
      LogUpdIndex *cl = ClauseCodeToLogUpdIndex(clcode);
      add_code_in_lu_index(cl, pp);
    } else {
      StaticIndex *cl = ClauseCodeToStaticIndex(clcode);
      add_code_in_static_index(cl, pp);
    }
  }
  clcode = pp->FirstClause;
  if (clcode != NULL) {
    if (pp->PredFlags & LogUpdatePredFlag) {
      LogUpdClause *cl = ClauseCodeToLogUpdClause(clcode);
      do {
	char *code_end;

	code_end = (char *)cl + cl->ClSize;
	Yap_inform_profiler_of_clause(cl, code_end, pp, GPROF_INIT_LOG_UPD_CLAUSE);
	cl = cl->ClNext;
      } while (cl != NULL);
    } else if (pp->PredFlags & DynamicPredFlag) {
      do {
	DynamicClause *cl;
	CODEADDR code_end;

	cl = ClauseCodeToDynamicClause(clcode);
 	code_end = (CODEADDR)cl + cl->ClSize;
	Yap_inform_profiler_of_clause(cl, code_end, pp, GPROF_INIT_DYNAMIC_CLAUSE);
	if (clcode == pp->LastClause)
	  break;
	clcode = NextDynamicClause(clcode);
      } while (TRUE);
    } else {
      StaticClause *cl = ClauseCodeToStaticClause(clcode);
      do {
	char *code_end;

	code_end = (char *)cl + cl->ClSize;
	Yap_inform_profiler_of_clause(cl, code_end, pp,GPROF_INIT_STATIC_CLAUSE);
	if (cl->ClCode == pp->LastClause)
	  break;
	cl = cl->ClNext;
      } while (TRUE);
    }
  }
  UNLOCK(pp->PELock);
}


void
Yap_dump_code_area_for_profiler(void) {
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
  Yap_inform_profiler_of_clause(COMMA_CODE, FAILCODE, RepPredProp(Yap_GetPredPropByFunc(FunctorComma,0)), GPROF_INIT_COMMA);
  Yap_inform_profiler_of_clause(FAILCODE, FAILCODE+1, RepPredProp(Yap_GetPredPropByAtom(AtomFail,0)), GPROF_INIT_FAIL);
}

#endif /* LOW_PROF */

static UInt
tree_index_ssz(StaticIndex *x)
{
  UInt sz = x->ClSize;
  x = x->ChildIndex;
  while (x != NULL) {
    sz +=  tree_index_ssz(x);
    x = x->SiblingIndex;
  }
  return sz;
}

static UInt
index_ssz(StaticIndex *x, PredEntry *pe)
{
  UInt sz = 0;
  yamop *ep = ExpandClausesFirst;
  if (pe->PredFlags & MegaClausePredFlag) {
    MegaClause *mcl = ClauseCodeToMegaClause(pe->FirstClause);
    if (mcl->ClFlags & ExoMask) {
      struct index_t *i = ((struct index_t **)(pe->FirstClause))[0];
      sz = 0;

      while (i) {
	sz = i->size+sz;
	i = i->next;
      }
      return sz;
    }
  }
  /* expand clause blocks */
  while (ep) {
    if (ep->y_u.sssllp.p == pe)
      sz += (UInt)NEXTOP((yamop *)NULL,sssllp)+ep->y_u.sssllp.s1*sizeof(yamop *);
    ep = ep->y_u.sssllp.snext;
  }
  /* main indexing tree */
  sz += tree_index_ssz(x);
  return sz;
}

static Int
static_statistics(PredEntry *pe)
{
  CACHE_REGS
  UInt sz = sizeof(PredEntry), cls = 0, isz = 0;
  StaticClause *cl = ClauseCodeToStaticClause(pe->FirstClause);

  if (pe->NOfClauses > 1 &&
      pe->TrueCodeOfPred != pe->FirstClause) {
    isz = index_ssz(ClauseCodeToStaticIndex(pe->TrueCodeOfPred), pe);
  }
  if (pe->PredFlags & MegaClausePredFlag) {
    MegaClause *mcl = ClauseCodeToMegaClause(pe->FirstClause);
    return Yap_unify(ARG3, MkIntegerTerm(mcl->ClSize/mcl->ClItemSize)) &&
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

static Int
p_static_pred_statistics( USES_REGS1 )
{
  Int out;
  PredEntry      *pe;

  pe = get_pred( Deref(ARG1), Deref(ARG2), "predicate_statistics");
  if (pe == NIL)
    return (FALSE);
  PELOCK(50,pe);
  if (pe->PredFlags & (DynamicPredFlag|LogUpdatePredFlag|UserCPredFlag|AsmPredFlag|CPredFlag|BinaryPredFlag)) {
    /* should use '$recordedp' in this case */
    UNLOCK(pe->PELock);
    return FALSE;
  }
  out = static_statistics(pe);
  UNLOCK(pe->PELock);
  return out;
}

static Int
p_predicate_erased_statistics( USES_REGS1 )
{
  UInt sz = 0, cls = 0;
  UInt isz = 0, icls = 0;
  PredEntry *pe;
  LogUpdClause *cl = DBErasedList;
  LogUpdIndex *icl = DBErasedIList;
  Term tpred = ArgOfTerm(2,Deref(ARG1));
  Term tmod = ArgOfTerm(1,Deref(ARG1));

  if (EndOfPAEntr(pe=get_pred(tpred,  tmod, "predicate_erased_statistics")))
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
  return
    Yap_unify(ARG2,MkIntegerTerm(cls)) &&
    Yap_unify(ARG3,MkIntegerTerm(sz)) &&
    Yap_unify(ARG4,MkIntegerTerm(icls)) &&
    Yap_unify(ARG5,MkIntegerTerm(isz));
}

#ifdef DEBUG
static Int
p_predicate_lu_cps( USES_REGS1 )
{
  return Yap_unify(ARG1, MkIntegerTerm(Yap_LiveCps)) &&
    Yap_unify(ARG2, MkIntegerTerm(Yap_FreedCps)) &&
    Yap_unify(ARG3, MkIntegerTerm(Yap_DirtyCps)) &&
    Yap_unify(ARG4, MkIntegerTerm(Yap_NewCps));
}
#endif

static Int
p_program_continuation( USES_REGS1 )
{
  PredEntry *pe = EnvPreg((yamop *)((ENV_Parent(ENV))[E_CP]));
  if (pe->ModuleOfPred) {
    if (!Yap_unify(ARG1,pe->ModuleOfPred))
      return FALSE;
  } else {
    if (!Yap_unify(ARG1,TermProlog))
      return FALSE;
  }
  if (pe->ArityOfPE) {
    if (!Yap_unify(ARG2,MkAtomTerm(NameOfFunctor(pe->FunctorOfPred))))
      return FALSE;
    if (!Yap_unify(ARG3,MkIntegerTerm(ArityOfFunctor(pe->FunctorOfPred))))
      return FALSE;
  } else {
    if (!Yap_unify(ARG2,MkAtomTerm((Atom)pe->FunctorOfPred)))
      return FALSE;
    if (!Yap_unify(ARG3,MkIntTerm(0)))
      return FALSE;
  }
  return TRUE;
}

static Term
BuildActivePred(PredEntry *ap, CELL *vect)
{
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

static int
UnifyPredInfo(PredEntry *pe, int start_arg USES_REGS) {
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
    Yap_unify(XREGS[start_arg+1],tname) &&
    Yap_unify(XREGS[start_arg+2],MkIntegerTerm(arity));
}


static Int
ClauseId(yamop *ipc, PredEntry *pe)
{
  if (!ipc)
    return 0;
  return find_code_in_clause(pe, ipc, NULL, NULL);
}

static Int
p_env_info( USES_REGS1 )
{
  CELL *env = LCL0-IntegerOfTerm(Deref(ARG1));
  yamop *env_cp;
  Term env_b, taddr;

  if (!env)
    return FALSE;
  env_b = MkIntegerTerm((Int)(LCL0-(CELL *)env[E_CB]));
  env_cp = (yamop *)env[E_CP];

  /* pe = PREVOP(env_cp,Osbpp)->y_u.Osbpp.p0; */
  taddr = MkIntegerTerm((Int)env);
  return Yap_unify(ARG3,MkIntegerTerm((Int)env_cp)) &&
    Yap_unify(ARG2, taddr) &&
    Yap_unify(ARG4, env_b);
}

static Int
p_cpc_info( USES_REGS1 )
{
  PredEntry *pe;
  yamop *ipc = (yamop *)IntegerOfTerm(Deref(ARG1));

  pe = PREVOP(ipc,Osbpp)->y_u.Osbpp.p0;
  return UnifyPredInfo(pe, 2 PASS_REGS) &&
    Yap_unify(ARG5,MkIntegerTerm(ClauseId(ipc,pe)));
}

static Int
p_choicepoint_info( USES_REGS1 )
{
  choiceptr cptr = (choiceptr)(LCL0-IntegerOfTerm(Deref(ARG1)));
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
      ipc = NEXTOP(ipc,l);
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
      ncl = NEXTOP(ipc,OtapFs);
      pe = ipc->y_u.OtapFs.p;
      t = BuildActivePred(pe, cptr->cp_args);
      break;
    case _retry_profiled:
    case _count_retry:
      pe = NULL;
      t = TermNil;
      ncl = ipc->y_u.Otapl.d;
      ipc = NEXTOP(ipc,p);
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
    case _Nstop:
      {
	Atom at = AtomLive;
	t = MkAtomTerm(at);
	pe = RepPredProp(PredPropByAtom(at, CurrentModule));
      }
      break;
    case _Ystop:
    default:
      return FALSE;
    }
  }
  return UnifyPredInfo(pe, 3 PASS_REGS) &&
    Yap_unify(ARG2, taddr) &&
    Yap_unify(ARG6,t) &&
    Yap_unify(ARG7,MkIntegerTerm(ClauseId(ncl,pe)));
}

void
Yap_InitCdMgr(void)
{
  CACHE_REGS
  Term cm = CurrentModule;

  Yap_InitCPred("in_use", 2, in_use, HiddenPredFlag|TestPredFlag | SafePredFlag|SyncPredFlag);
  Yap_InitCPred("toggle_static_predicates_in_use", 0, p_toggle_static_predicates_in_use, HiddenPredFlag|SafePredFlag|SyncPredFlag);

}
