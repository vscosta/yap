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
* File:		cdmgr.c							 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	Code manager						 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "@(#)cdmgr.c	1.1 05/02/98";
#endif

#include "Yap.h"
#include "clause.h"
#include "yapio.h"
#include "eval.h"
#include "tracer.h"
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */
#if HAVE_STRING_H
#include <string.h>
#endif


STATIC_PROTO(void retract_all, (PredEntry *, int));
STATIC_PROTO(void add_first_static, (PredEntry *, yamop *, int));
STATIC_PROTO(void add_first_dynamic, (PredEntry *, yamop *, int));
STATIC_PROTO(void asserta_stat_clause, (PredEntry *, yamop *, int));
STATIC_PROTO(void asserta_dynam_clause, (PredEntry *, yamop *));
STATIC_PROTO(void assertz_stat_clause, (PredEntry *, yamop *, int));
STATIC_PROTO(void assertz_dynam_clause, (PredEntry *, yamop *));
STATIC_PROTO(void expand_consult, (void));
STATIC_PROTO(int  not_was_reconsulted, (PredEntry *, Term, int));
#if EMACS
STATIC_PROTO(int  last_clause_number, (PredEntry *));
#endif
STATIC_PROTO(int  static_in_use, (PredEntry *, int));
#if !defined(YAPOR) && !defined(THREADS)
STATIC_PROTO(Int  search_for_static_predicate_in_use, (PredEntry *, int));
STATIC_PROTO(void mark_pred, (int, PredEntry *));
STATIC_PROTO(void do_toggle_static_predicates_in_use, (int));
#endif
STATIC_PROTO(void recover_log_upd_clause, (LogUpdClause *));
STATIC_PROTO(Int  p_number_of_clauses, (void));
STATIC_PROTO(Int  p_compile, (void));
STATIC_PROTO(Int  p_compile_dynamic, (void));
STATIC_PROTO(Int  p_purge_clauses, (void));
STATIC_PROTO(Int  p_setspy, (void));
STATIC_PROTO(Int  p_rmspy, (void));
STATIC_PROTO(Int  p_startconsult, (void));
STATIC_PROTO(Int  p_showconslultlev, (void));
STATIC_PROTO(Int  p_endconsult, (void));
STATIC_PROTO(Int  p_undefined, (void));
STATIC_PROTO(Int  p_in_use, (void));
STATIC_PROTO(Int  p_new_multifile, (void));
STATIC_PROTO(Int  p_is_multifile, (void));
STATIC_PROTO(Int  p_optimizer_on, (void));
STATIC_PROTO(Int  p_optimizer_off, (void));
STATIC_PROTO(Int  p_in_this_f_before, (void));
STATIC_PROTO(Int  p_first_cl_in_f, (void));
STATIC_PROTO(Int  p_mk_cl_not_first, (void));
STATIC_PROTO(Int  p_is_dynamic, (void));
STATIC_PROTO(Int  p_kill_dynamic, (void));
STATIC_PROTO(Int  p_compile_mode, (void));
STATIC_PROTO(Int  p_is_profiled, (void));
STATIC_PROTO(Int  p_profile_info, (void));
STATIC_PROTO(Int  p_profile_reset, (void));
STATIC_PROTO(Int  p_is_call_counted, (void));
STATIC_PROTO(Int  p_call_count_info, (void));
STATIC_PROTO(Int  p_call_count_set, (void));
STATIC_PROTO(Int  p_call_count_reset, (void));
STATIC_PROTO(Int  p_toggle_static_predicates_in_use, (void));
STATIC_PROTO(Atom  YapConsultingFile, (void));
STATIC_PROTO(Int  PredForCode,(yamop *, Atom *, UInt *, SMALLUNSGN *));

#define PredArity(p) (p->ArityOfPE)
#define TRYCODE(G,F,N) ( (N)<5 ? (op_numbers)((int)F+(N)*3) : G)
#define NEXTOP(V,TYPE)    ((yamop *)(&((V)->u.TYPE.next)))

#define IN_BLOCK(P,B,SZ)     ((CODEADDR)(P) > (CODEADDR)(B) && \
			      (CODEADDR)(P) <= (CODEADDR)(B)+(SZ))

/******************************************************************
  
			EXECUTING PROLOG CLAUSES
  
******************************************************************/


static int 
static_in_use(PredEntry *p, int check_everything)
{
#if defined(YAPOR) || defined(THREADS)
  return(FALSE);
#else
  CELL pflags = p->PredFlags;
  if (pflags & (DynamicPredFlag|LogUpdatePredFlag)) {
    return (FALSE);
  }
  if (STATIC_PREDICATES_MARKED) {
    return (p->PredFlags & InUsePredFlag);
  } else {
    /* This code does not work for YAPOR or THREADS!!!!!!!! */
    return(search_for_static_predicate_in_use(p, check_everything));
  }
#endif
}

/******************************************************************
  
		ADDING AND REMOVE INFO TO A PROCEDURE
  
******************************************************************/


/*
 * we have three kinds of predicates: dynamic		DynamicPredFlag
 * static 		CompiledPredFlag fast		FastPredFlag all the
 * database predicates are supported for dynamic predicates only abolish and
 * assertz are supported for static predicates no database predicates are
 * supportted for fast predicates 
 */

#define is_dynamic(pe)  (pe->PredFlags & DynamicPredFlag)
#define is_static(pe) 	(pe->PredFlags & CompiledPredFlag)
#define is_fast(pe)	(pe->PredFlags & FastPredFlag)
#define is_logupd(pe)	(pe->PredFlags & LogUpdatePredFlag)
#ifdef TABLING
#define is_tabled(pe)   (pe->PredFlags & TabledPredFlag)
#endif /* TABLING */

/******************************************************************
  
		Indexation Info
  
******************************************************************/
#define ByteAdr(X)   ((Int) &(X))

/* Index a prolog pred, given its predicate entry */
/* ap is already locked, but IPred is the one who gets rid of the lock. */
static void 
IPred(PredEntry *ap)
{
  yamop          *BaseAddr;
  int             Arity;
  Functor         f;

#ifdef TABLING
  if (is_tabled(ap)) {
    ap->CodeOfPred = ap->cs.p_code.TrueCodeOfPred;
    ap->OpcodeOfPred = ap->CodeOfPred->opc;
    return;
  }
#endif /* TABLING */
  f = ap->FunctorOfPred;
#ifdef DEBUG
  if (Yap_Option['i' - 'a' + 1]) {
    Atom At = NameOfFunctor(f);
    Yap_DebugPutc(Yap_c_error_stream,'\t');
    Yap_plwrite(MkAtomTerm(At), Yap_DebugPutc, 0);
    Yap_DebugPutc(Yap_c_error_stream,'/');
    Yap_plwrite(MkIntTerm(ArityOfFunctor(f)), Yap_DebugPutc, 0);
    Yap_DebugPutc(Yap_c_error_stream,'\n');
  }
#endif
  Arity = ArityOfFunctor(f);
  /* Do not try to index a dynamic predicate  or one whithout args */
  if (is_dynamic(ap)) {
    WRITE_UNLOCK(ap->PRWLock);
    Yap_Error(SYSTEM_ERROR,TermNil,"trying to index a dynamic predicate");
    return;
  }
  if (Arity == 0) {
    WRITE_UNLOCK(ap->PRWLock);
    Yap_Error(SYSTEM_ERROR,TermNil,
	  "trying to index a predicate with 0 arguments");
    return;
  }
  if ((BaseAddr = Yap_PredIsIndexable(ap)) != NULL) {
    ap->cs.p_code.TrueCodeOfPred = BaseAddr;
    ap->PredFlags |= IndexedPredFlag;
  }
  if (ap->PredFlags & SpiedPredFlag) {
    ap->OpcodeOfPred = Yap_opcode(_spy_pred);
    ap->CodeOfPred = (yamop *)(&(ap->OpcodeOfPred)); 
  } else {
    ap->CodeOfPred = ap->cs.p_code.TrueCodeOfPred;
    ap->OpcodeOfPred = ((yamop *)(ap->CodeOfPred))->opc;
  }
  WRITE_UNLOCK(ap->PRWLock);
#ifdef DEBUG
  if (Yap_Option['i' - 'a' + 1])
    Yap_DebugPutc(Yap_c_error_stream,'\n');
#endif
}

void 
Yap_IPred(PredEntry *p)
{
  IPred(p);
}

#define GONEXT(TYPE)      code_p = ((yamop *)(&(code_p->u.TYPE.next)))

static void
recover_log_upd_clause(LogUpdClause *cl)
{
  LOCK(cl->ClLock);
  if (cl->ClFlags & LogUpdRuleMask) {
    if (--(cl->u2.ClExt->u.EC.ClRefs) == 0 &&
	(cl->ClFlags & ErasedMask) &&
#if defined(YAPOR) || defined(THREADS)
	(cl->ref_count == 0)
#else
	!(cl->ClFlags & InUseMask)
#endif
	)
      Yap_ErLogUpdCl(cl);
  } else {
    if (--(cl->u2.ClUse) == 0 &&
	(cl->ClFlags & ErasedMask) &&
#if defined(YAPOR) || defined(THREADS)
	(cl->ref_count == 0)
#else
	!(cl->ClFlags & InUseMask)
#endif
	)
      Yap_ErLogUpdCl(cl);
  }
  UNLOCK(cl->ClLock);
}

static LogUpdClause *
ClauseBodyToLogUpdClause(yamop *addr)
{
  addr = (yamop *)((CODEADDR)addr - (Int)NEXTOP((yamop *)NULL,ld));
  return(ClauseCodeToLogUpdClause(addr));
}

/* we already have a lock on the predicate */
static void
RemoveLogUpdIndex(LogUpdClause *cl)
{
  yamop *code_p;
  OPCODE last = Yap_opcode(_trust_logical_pred);

#if defined(YAPOR) || defined(THREADS)
  if (cl->ref_count != 0)
    return;  
#else
  if (cl->ClFlags & InUseMask)
    return;
#endif
  /* now the hard part, I must tell all other clauses they are free */
  code_p = cl->u.ClVarChain;
  /* skip try_log_update */
  GONEXT(l);
  recover_log_upd_clause(ClauseBodyToLogUpdClause(code_p->u.ld.d));
  GONEXT(ld);
  while(code_p->opc != last) {
    recover_log_upd_clause(ClauseBodyToLogUpdClause(code_p->u.ld.d));
    GONEXT(ld);
  }
  /* skip trust_log_update */
  GONEXT(l);
  recover_log_upd_clause(ClauseBodyToLogUpdClause(code_p->u.ld.d));
  /* don't need to worry about MultiFiles */
  Yap_FreeCodeSpace((char *) cl);
}

void
Yap_RemoveLogUpdIndex(LogUpdClause *cl)
{
  RemoveLogUpdIndex(cl);
}

/* Routine used when wanting to remove the indexation */
/* ap is known to already have been locked for WRITING */
static int 
RemoveIndexation(PredEntry *ap)
{ 
  register yamop *First;
  int             spied;

  First = ap->cs.p_code.FirstClause;
  if (ap->OpcodeOfPred == INDEX_OPCODE) {
    return (TRUE);
  }
  spied = ap->PredFlags & SpiedPredFlag;
  if (ap->PredFlags & LogUpdatePredFlag) 
    RemoveLogUpdIndex(ClauseCodeToLogUpdClause(ap->cs.p_code.TrueCodeOfPred));
  else {
    DeadClause *cl;

    cl = (DeadClause *)ClauseCodeToStaticClause(ap->cs.p_code.TrueCodeOfPred);
    if (static_in_use(ap, FALSE)) {
      /* This should never happen */
      cl->ClFlags = 0;
      cl->NextCl = DeadClauses;
      DeadClauses = cl;
    } else {
      Yap_FreeCodeSpace((char *)cl);
    }
  }
  if (First != ap->cs.p_code.LastClause)
    ap->cs.p_code.TrueCodeOfPred = First;
  ap->PredFlags ^= IndexedPredFlag;
  if (First != NULL && spied) {
    ap->OpcodeOfPred = Yap_opcode(_spy_pred);
    ap->CodeOfPred = (yamop *)(&(ap->OpcodeOfPred)); 
  } else {
    ap->OpcodeOfPred = ap->cs.p_code.TrueCodeOfPred->opc;
    ap->CodeOfPred = ap->cs.p_code.TrueCodeOfPred;
  }
  return (TRUE);
}

int 
Yap_RemoveIndexation(PredEntry *ap)
{
  return RemoveIndexation(ap);
}
/******************************************************************
  
			Adding clauses
  
******************************************************************/


#define	assertz	0
#define	consult	1
#define	asserta	2

/* p is already locked */
static void 
retract_all(PredEntry *p, int in_use)
{
  yamop          *q, *q1;
  int             multifile_pred = p->PredFlags & MultiFileFlag;
  yamop          *fclause = NULL, *lclause = NULL;

  q = p->cs.p_code.FirstClause;
  if (q != NIL) {
    if (p->PredFlags & LogUpdatePredFlag) { 
      do {
	LogUpdClause *cl;
	q1 = q;
	q = NextClause(q);
	cl = ClauseCodeToLogUpdClause(q1);
	if (multifile_pred && cl->Owner != YapConsultingFile()) {
	  if (fclause == NULL) {
	    fclause = q1;
	  } else {
	    yamop *clp = (yamop *)lclause;
	    clp->u.ld.d = q1;
	  }
	  lclause = q1;
	} else {
	  Yap_ErLogUpdCl(cl);
	}
      } while (q1 != p->cs.p_code.LastClause);
    } else {
      do {
	StaticClause *cl;
	q1 = q;
	q = NextClause(q);
	cl = ClauseCodeToStaticClause(q1);
	if (multifile_pred && cl->Owner != YapConsultingFile()) {
	  if (fclause == NULL) {
	    fclause = q1;
	  } else {
	    yamop *clp = (yamop *)lclause;
	    clp->u.ld.d = q1;
	  }
	  lclause = q1;
	} else {
	  if (cl->ClFlags & HasBlobsMask) {
	    DeadClause *dcl = (DeadClause *)cl;
	    dcl->NextCl = DeadClauses;
	    dcl->ClFlags = 0;
	    DeadClauses = dcl;
	  } else {
	    Yap_FreeCodeSpace((char *)cl);
	  }
	  p->cs.p_code.NOfClauses--;
	}
      } while (q1 != p->cs.p_code.LastClause);
    }
  }
  p->cs.p_code.FirstClause = fclause;
  p->cs.p_code.LastClause = lclause;
  if (fclause == NIL) {
    if (p->PredFlags & (DynamicPredFlag|LogUpdatePredFlag)) {
      p->OpcodeOfPred = FAIL_OPCODE;
    } else {
      p->OpcodeOfPred = UNDEF_OPCODE;
    }
    p->cs.p_code.TrueCodeOfPred = p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
    p->StatisticsForPred.NOfEntries = 0;
    p->StatisticsForPred.NOfHeadSuccesses = 0;
    p->StatisticsForPred.NOfRetries = 0;
  } else {
    yamop *cpt = (yamop *)fclause;
    cpt->opc = Yap_opcode(TRYCODE(_try_me, _try_me0, PredArity(p)));
    if (fclause == lclause) {
      p->cs.p_code.TrueCodeOfPred = p->CodeOfPred = NEXTOP(cpt,ld);
      p->OpcodeOfPred = NEXTOP(cpt,ld)->opc;
    } else {
      p->cs.p_code.TrueCodeOfPred = p->CodeOfPred = fclause;
      p->OpcodeOfPred = cpt->opc;
      if (p->PredFlags & ProfiledPredFlag) {
	((yamop *)lclause)->opc = Yap_opcode(_profiled_trust_me);
      } else if (p->PredFlags & CountPredFlag) {
	((yamop *)lclause)->opc = Yap_opcode(_count_trust_me);
      } else {
	((yamop *)lclause)->opc = Yap_opcode(TRYCODE(_trust_me, _trust_me0, PredArity(p)));
      }
    }
    if (p->PredFlags & SpiedPredFlag) {
      p->OpcodeOfPred = Yap_opcode(_spy_pred);
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred)); 
    } else if (p->PredFlags & IndexedPredFlag) {
      p->OpcodeOfPred = INDEX_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred)); 
    }
  }
  if (PROFILING) {
    p->PredFlags |= ProfiledPredFlag;
  } else
    p->PredFlags &= ~ProfiledPredFlag;
#ifdef YAPOR
  if (SEQUENTIAL_IS_DEFAULT) {
    p->PredFlags |= SequentialPredFlag;
  }
#endif /* YAPOR */
  Yap_PutValue(AtomAbol, MkAtomTerm(AtomTrue));
}

/* p is already locked */
static void 
add_first_static(PredEntry *p, yamop *cp, int spy_flag)
{
  yamop *pt = (yamop *)cp;

  pt->u.ld.d = cp;
  pt->u.ld.p = p;
  if (p == PredGoalExpansion) {
    PRED_GOAL_EXPANSION_ON = TRUE;
    Yap_InitComma();
  }
#ifdef YAPOR
  if (SEQUENTIAL_IS_DEFAULT) {
    p->PredFlags |= SequentialPredFlag;
    PUT_YAMOP_SEQ(pt);
  }
  if (YAMOP_LTT(pt) != 1)
    abort_optyap("YAMOP_LTT error in function add_first_static");
#endif /* YAPOR */
#ifdef TABLING
  if (is_tabled(p)) {
    pt->u.ld.te = p->TableOfPred;
    pt->opc = Yap_opcode(_table_try_me_single);
  }
  else	
#endif /* TABLING */
    {
      pt->opc = Yap_opcode(TRYCODE(_try_me, _try_me0, PredArity(p)));
      pt = NEXTOP(pt, ld);
    }
  p->cs.p_code.TrueCodeOfPred = pt;
  p->cs.p_code.FirstClause = p->cs.p_code.LastClause = cp;
  p->cs.p_code.NOfClauses = 1;
  p->StatisticsForPred.NOfEntries = 0;
  p->StatisticsForPred.NOfHeadSuccesses = 0;
  p->StatisticsForPred.NOfRetries = 0;
  if (PROFILING) {
    p->PredFlags |= ProfiledPredFlag;
  } else
    p->PredFlags &= ~ProfiledPredFlag;
#ifdef YAPOR
  p->PredFlags |= SequentialPredFlag;
  PUT_YAMOP_SEQ((yamop *)cp);
#endif /* YAPOR */
  if (spy_flag) {
    p->OpcodeOfPred = Yap_opcode(_spy_pred);
    p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred)); 
  }
  if (yap_flags[SOURCE_MODE_FLAG]) {
    p->PredFlags |= SourcePredFlag;
  } else {
    p->PredFlags &= ~SourcePredFlag;
  }
}

/* p is already locked */
static void 
add_first_dynamic(PredEntry *p, yamop *cp, int spy_flag)
{
  yamop    *ncp = ((DynamicClause *)NULL)->ClCode;
  DynamicClause   *cl;
  if (p == PredGoalExpansion) {
    PRED_GOAL_EXPANSION_ON = TRUE;
    Yap_InitComma();
  }
  p->StatisticsForPred.NOfEntries = 0;
  p->StatisticsForPred.NOfHeadSuccesses = 0;
  p->StatisticsForPred.NOfRetries = 0;
  if (PROFILING) {
    p->PredFlags |= ProfiledPredFlag;
  } else
    p->PredFlags &= ~ProfiledPredFlag;
#ifdef YAPOR
  p->PredFlags |= SequentialPredFlag;
#endif /* YAPOR */
  /* allocate starter block, containing info needed to start execution,
   * that is a try_mark to start the code and a fail to finish things up */
  cl =
    (DynamicClause *) Yap_AllocCodeSpace((Int)NEXTOP(NEXTOP(NEXTOP(ncp,ld),e),e));
  if (cl == NIL) {
    Yap_Error(SYSTEM_ERROR,TermNil,"Heap crashed against Stacks");
    return;
  }
  /* skip the first entry, this contains the back link and will always be
     empty for this entry */
  ncp = (yamop *)(((CELL *)ncp)+1);
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
  ncp->u.ld.s = p->ArityOfPE;
  ncp->u.ld.p = p;
  ncp->u.ld.d = cp;
#ifdef YAPOR
  INIT_YAMOP_LTT(ncp, 1);
  PUT_YAMOP_SEQ(ncp);
#endif /* YAPOR */
  /* This is the point we enter the code */
  p->cs.p_code.TrueCodeOfPred = p->CodeOfPred = ncp;
  p->cs.p_code.NOfClauses = 1;
  /* set the first clause to have a retry and mark which will
   *  backtrack to the previous block */
  if (p->PredFlags & ProfiledPredFlag)
    cp->opc = Yap_opcode(_profiled_retry_and_mark);
  else if (p->PredFlags & CountPredFlag)
    cp->opc = Yap_opcode(_count_retry_and_mark);
  else
    cp->opc = Yap_opcode(_retry_and_mark);
  cp->u.ld.s = p->ArityOfPE;
  cp->u.ld.p = p;
  cp->u.ld.d = ncp;
  /* also, keep a backpointer for the days you delete the clause */
  ClauseCodeToDynamicClause(cp)->ClPrevious = ncp;
  /* Don't forget to say who is the only clause for the predicate so
     far */
  p->cs.p_code.LastClause = p->cs.p_code.FirstClause = cp;
  /* we're only missing what to do when we actually exit the procedure
   */
  ncp = NEXTOP(ncp,ld);
  /* and the last instruction to execute to exit the predicate, note
     the retry is pointing to this pseudo clause */
  ncp->opc = Yap_opcode(_trust_fail);
  /* we're only missing what to do when we actually exit the procedure
   */
  /* and close the code */
  ncp = NEXTOP(ncp,e);
  ncp->opc = Yap_opcode(_Ystop);
}

/* p is already locked */
static void 
asserta_stat_clause(PredEntry *p, yamop *cp, int spy_flag)
{
  yamop        *q = (yamop *)cp;
  q->u.ld.d = p->cs.p_code.FirstClause;
  q->u.ld.p = p;
#ifdef YAPOR
  PUT_YAMOP_LTT(q, YAMOP_LTT((yamop *)(p->cs.p_code.FirstClause)) + 1);
#endif /* YAPOR */
#ifdef TABLING
  if (is_tabled(p))
    q->opc = Yap_opcode(_table_try_me);    
  else
#endif /* TABLING */
    q->opc = Yap_opcode(TRYCODE(_try_me, _try_me0, PredArity(p)));
  q = (yamop *)(p->cs.p_code.FirstClause);
  if (p->PredFlags & ProfiledPredFlag) {
    if (p->cs.p_code.FirstClause == p->cs.p_code.LastClause)
      q->opc = Yap_opcode(_profiled_trust_me);
    else
      q->opc = Yap_opcode(_profiled_retry_me);
  } else if (p->PredFlags & CountPredFlag) {
    if (p->cs.p_code.FirstClause == p->cs.p_code.LastClause)
      q->opc = Yap_opcode(_count_trust_me);
    else
      q->opc = Yap_opcode(_count_retry_me);
  } else {
    if (p->cs.p_code.FirstClause == p->cs.p_code.LastClause) {
#ifdef TABLING
      if (is_tabled(p))
	q->opc = Yap_opcode(_table_trust_me);    
      else
#endif /* TABLING */
	q->opc = Yap_opcode(TRYCODE(_trust_me, _trust_me0, PredArity(p)));
    } else {
#ifdef TABLING
      if (is_tabled(p))
	q->opc = Yap_opcode(_table_retry_me);    
      else
#endif /* TABLING */
      q->opc = Yap_opcode(TRYCODE(_retry_me, _retry_me0, PredArity(p)));
    }
  }
  p->cs.p_code.TrueCodeOfPred = p->cs.p_code.FirstClause = cp;
  p->cs.p_code.LastClause->u.ld.d = cp;
  p->cs.p_code.NOfClauses++;
}

/* p is already locked */
static void 
asserta_dynam_clause(PredEntry *p, yamop *cp)
{
  yamop        *q;
  q = cp;
  LOCK(ClauseCodeToDynamicClause(p->cs.p_code.FirstClause)->ClLock);
  /* also, keep backpointers for the days we'll delete all the clause */
  ClauseCodeToDynamicClause(p->cs.p_code.FirstClause)->ClPrevious = q;
  ClauseCodeToDynamicClause(cp)->ClPrevious = (yamop *)(p->CodeOfPred);
  UNLOCK(ClauseCodeToDynamicClause(p->cs.p_code.FirstClause)->ClLock);
  q->u.ld.d = p->cs.p_code.FirstClause;
  q->u.ld.s = p->ArityOfPE;
  q->u.ld.p = p;
  if (p->PredFlags & ProfiledPredFlag)
    cp->opc = Yap_opcode(_profiled_retry_and_mark);
  else if (p->PredFlags & CountPredFlag)
    cp->opc = Yap_opcode(_count_retry_and_mark);
  else
    cp->opc = Yap_opcode(_retry_and_mark);
  cp->u.ld.s = p->ArityOfPE;
  cp->u.ld.p = p;
  p->cs.p_code.FirstClause = cp;
  q = p->CodeOfPred;
  q->u.ld.d = cp;
  q->u.ld.s = p->ArityOfPE;
  q->u.ld.p = p;
  p->cs.p_code.NOfClauses++;
}

/* p is already locked */
static void 
assertz_stat_clause(PredEntry *p, yamop *cp, int spy_flag)
{
  yamop        *pt;
  pt = (yamop *)(p->cs.p_code.LastClause);
  if (p->PredFlags & ProfiledPredFlag) {
    if (p->cs.p_code.FirstClause == p->cs.p_code.LastClause) {
      pt->opc = Yap_opcode(TRYCODE(_try_me, _try_me0, PredArity(p)));
      p->cs.p_code.TrueCodeOfPred = p->cs.p_code.FirstClause;
    } else
      pt->opc = Yap_opcode(_profiled_retry_me);
  } else if (p->PredFlags & CountPredFlag) {
    if (p->cs.p_code.FirstClause == p->cs.p_code.LastClause) {
      pt->opc = Yap_opcode(TRYCODE(_try_me, _try_me0, PredArity(p)));
      p->cs.p_code.TrueCodeOfPred = p->cs.p_code.FirstClause;
    } else
      pt->opc = Yap_opcode(_count_retry_me);
  } else {
    if (p->cs.p_code.FirstClause == p->cs.p_code.LastClause) {
#ifdef TABLING
      if (is_tabled(p))
	pt->opc = Yap_opcode(_table_try_me);    
      else
#endif /* TABLING */
	pt->opc = Yap_opcode(TRYCODE(_try_me, _try_me0, PredArity(p)));
      p->cs.p_code.TrueCodeOfPred = p->cs.p_code.FirstClause;
    } else {
#ifdef TABLING
      if (is_tabled(p))
	pt->opc = Yap_opcode(_table_retry_me);    
      else
#endif /* TABLING */
	pt->opc = Yap_opcode(TRYCODE(_retry_me, _retry_me0, PredArity(p)));
    }
  }
  pt->u.ld.d = cp;
  p->cs.p_code.LastClause = cp;
  pt = (yamop *)cp;
  if (p->PredFlags & ProfiledPredFlag) {
    pt->opc = Yap_opcode(_profiled_trust_me);
  } else if (p->PredFlags & CountPredFlag) {
    pt->opc = Yap_opcode(_count_trust_me);
  } else {
#ifdef TABLING
    if (is_tabled(p))
      pt->opc = Yap_opcode(_table_trust_me);    
    else
#endif /* TABLING */
      pt->opc = Yap_opcode(TRYCODE(_trust_me, _trust_me0, PredArity(p)));
  }
  pt->u.ld.d = p->cs.p_code.FirstClause;
#ifdef YAPOR
  {
    yamop *code;

    code = p->cs.p_code.FirstClause;
    while (code != p->cs.p_code.LastClause){
      PUT_YAMOP_LTT((yamop *)code, YAMOP_LTT((yamop *)code) + 1);
      code = NextClause(code);
    }
  }
#endif /* YAPOR */
  p->cs.p_code.NOfClauses++;
}

/* p is already locked */
static void 
assertz_dynam_clause(PredEntry *p, yamop *cp)
{
  yamop       *q;

  q = p->cs.p_code.LastClause;
  LOCK(ClauseCodeToDynamicClause(q)->ClLock);
  q->u.ld.d = cp;
  p->cs.p_code.LastClause = cp;
  /* also, keep backpointers for the days we'll delete all the clause */
  ClauseCodeToDynamicClause(cp)->ClPrevious = q;
  UNLOCK(ClauseCodeToDynamicClause(q)->ClLock);
  q = (yamop *)cp;
  if (p->PredFlags & ProfiledPredFlag)
    q->opc = Yap_opcode(_profiled_retry_and_mark);
  else if (p->PredFlags & CountPredFlag)
    q->opc = Yap_opcode(_count_retry_and_mark);
  else
    q->opc = Yap_opcode(_retry_and_mark);
  q->u.ld.d = p->CodeOfPred;
  q->u.ld.s = p->ArityOfPE;
  q->u.ld.p = p;
  p->cs.p_code.NOfClauses++;
}

static void  expand_consult(void)
{
  consult_obj *new_cl, *new_cb, *new_cs;
  UInt OldConsultCapacity = ConsultCapacity;

  /* now double consult capacity */
  ConsultCapacity += InitialConsultCapacity;
  /* I assume it always works ;-) */
  while ((new_cl = (consult_obj *)Yap_AllocCodeSpace(sizeof(consult_obj)*ConsultCapacity)) == NULL) {
    if (!Yap_growheap(FALSE)) {
      Yap_Error(SYSTEM_ERROR,TermNil,Yap_ErrorMessage);
      return;
    }
  }
  new_cs = new_cl + (InitialConsultCapacity+1);
  new_cb = new_cs + (ConsultBase-ConsultSp);
  /* start copying */
  memcpy((void *)(new_cs), (void *)(ConsultSp), OldConsultCapacity*sizeof(consult_obj));
  /* copying done, release old space */
  Yap_FreeCodeSpace((char *)ConsultLow);
  /* next, set up pointers correctly */
  ConsultSp = new_cs;
  ConsultBase = new_cb;
  ConsultLow = new_cl;
}

/* p was already locked */
static int 
not_was_reconsulted(PredEntry *p, Term t, int mode)
{
  register consult_obj  *fp;
  Prop                   p0 = AbsProp((PropEntry *)p);

  for (fp = ConsultSp; fp < ConsultBase; ++fp)
    if (fp->p == p0)
      break;
  if (fp != ConsultBase)
    return (FALSE);
  if (mode) {
    if (ConsultSp == ConsultLow+1)
      expand_consult();
    --ConsultSp;
    ConsultSp->p = p0;
    if (ConsultBase[1].mode) /* we are in reconsult mode */ {
      retract_all(p, static_in_use(p,TRUE));
    }
    if (!(p->PredFlags & MultiFileFlag)) {
      p->OwnerFile = YapConsultingFile();
    }
  }
  return (TRUE);		/* careful */
}

static void
addcl_permission_error(AtomEntry *ap, Int Arity, int in_use) 
{
  Term t, ti[2];

  ti[0] = MkAtomTerm(AbsAtom(ap));
  ti[1] = MkIntegerTerm(Arity);
  t = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("/"),2), 2, ti);
  Yap_ErrorMessage = Yap_ErrorSay;
  Yap_Error_Term = t;
  Yap_Error_TYPE = PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE;
  if (in_use) {
    if (Arity == 0)
      sprintf(Yap_ErrorMessage, "static predicate %s is in use", ap->StrOfAE);
    else
      sprintf(Yap_ErrorMessage,
#if SHORT_INTS
	      "static predicate %s/%ld is in use",
#else
	      "static predicate %s/%d is in use",
#endif
	      ap->StrOfAE, Arity);
  } else {
    if (Arity == 0)
      sprintf(Yap_ErrorMessage, "system predicate %s", ap->StrOfAE);
    else
      sprintf(Yap_ErrorMessage,
#if SHORT_INTS
	      "system predicate %s/%ld",
#else
	      "system predicate %s/%d",
#endif
	      ap->StrOfAE, Arity);
  }
}


static void
addclause(Term t, yamop *cp, int mode, int mod)
/*
 * mode		0  assertz 1  consult 2  asserta				 
 */
{
  PredEntry      *p;
  int             spy_flag = FALSE;
  Atom           at;
  UInt           Arity;


  if (IsApplTerm(t) && FunctorOfTerm(t) == FunctorAssert)
    t = ArgOfTerm(1, t);
  if (IsAtomTerm(t)) {
    at = AtomOfTerm(t);
    p = RepPredProp(PredPropByAtom(at, mod));
    Arity = 0;
  } else {
    Functor f = FunctorOfTerm(t);
    Arity = ArityOfFunctor(f);
    at = NameOfFunctor(f);
    p = RepPredProp(PredPropByFunc(f, mod));
  }
  Yap_PutValue(AtomAbol, TermNil);
  WRITE_LOCK(p->PRWLock);
  /* we are redefining a prolog module predicate */
  if (p->ModuleOfPred == 0 && mod != 0) {
    WRITE_UNLOCK(p->PRWLock);
    addcl_permission_error(RepAtom(at), Arity, FALSE);
    return;
  }
  /* The only problem we have now is when we need to throw away
     Indexing blocks
  */
  if (p->PredFlags & IndexedPredFlag) {
    if (!RemoveIndexation(p)) {
      /* should never happen */
      WRITE_UNLOCK(p->PRWLock);
      addcl_permission_error(RepAtom(at),Arity,TRUE);
      return;
    }
  }
  if (p->PredFlags & SpiedPredFlag)
    spy_flag = TRUE;
  if (mode == consult)
    not_was_reconsulted(p, t, TRUE);
  /* always check if we have a valid error first */
  if (Yap_ErrorMessage && Yap_Error_TYPE == PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE)
    return;
  if (!is_dynamic(p)) {
    if (p->PredFlags & LogUpdatePredFlag) {
      LogUpdClause     *clp = ClauseCodeToLogUpdClause(cp);
      clp->ClFlags |= StaticMask;
    } else {
      StaticClause     *clp = ClauseCodeToStaticClause(cp);
      clp->ClFlags |= StaticMask;
    }
    if (compile_mode)
      p->PredFlags |= CompiledPredFlag | FastPredFlag;
    else
      p->PredFlags |= CompiledPredFlag;
    if ((Yap_GetValue(AtomIndex) != TermNil) && 
	(p->cs.p_code.FirstClause != NIL) &&
	(Arity != 0)) {
      p->OpcodeOfPred = INDEX_OPCODE;
      p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred)); 
    }
  }
  if (p->cs.p_code.FirstClause == NIL) {
    if (!(p->PredFlags & DynamicPredFlag)) {
      add_first_static(p, cp, spy_flag);
      /* make sure we have a place to jump to */
      if (p->OpcodeOfPred == UNDEF_OPCODE ||
	  p->OpcodeOfPred == FAIL_OPCODE) {  /* log updates */
	p->CodeOfPred = p->cs.p_code.TrueCodeOfPred;
	p->OpcodeOfPred = ((yamop *)(p->CodeOfPred))->opc;
      }
    } else {
      add_first_dynamic(p, cp, spy_flag);
    }
  } else if (mode == asserta) {
    if (p->PredFlags & DynamicPredFlag)
      asserta_dynam_clause(p, cp);
    else
      asserta_stat_clause(p, cp, spy_flag);
  } else if (p->PredFlags & DynamicPredFlag)
    assertz_dynam_clause(p, cp);
  else {
    assertz_stat_clause(p, cp, spy_flag);
    if (p->OpcodeOfPred != INDEX_OPCODE &&
	p->OpcodeOfPred != Yap_opcode(_spy_pred)) {
      p->CodeOfPred = p->cs.p_code.TrueCodeOfPred;
      p->OpcodeOfPred = ((yamop *)(p->CodeOfPred))->opc;
    }
  }
  WRITE_UNLOCK(p->PRWLock);
}

void
Yap_addclause(Term t, yamop *cp, int mode, int mod) {
  addclause(t, cp, mode, mod);
}

static Int 
p_in_this_f_before(void)
{				/* '$in_this_file_before'(N,A,M) */
  unsigned int    arity;
  Atom            at;
  Term            t;
  register consult_obj  *fp;
  Prop            p0;
  SMALLUNSGN      mod;

  if (IsVarTerm(t = Deref(ARG1)) || !IsAtomTerm(t))
    return (FALSE);
  else
    at = AtomOfTerm(t);
  if (IsVarTerm(t = Deref(ARG2)) || !IsIntTerm(t))
    return (FALSE);
  else
    arity = IntOfTerm(t);
  if (IsVarTerm(t = Deref(ARG3)) || !IsAtomTerm(t))
    return (FALSE);
  else
    mod = Yap_LookupModule(t);
  if (arity)
    p0 = PredPropByFunc(Yap_MkFunctor(at, arity),CurrentModule);
  else
    p0 = PredPropByAtom(at, CurrentModule);
  if (ConsultSp == ConsultBase || (fp = ConsultSp)->p == p0)
    return (FALSE);
  else
    fp++;
  for (; fp < ConsultBase; ++fp)
    if (fp->p == p0)
      break;
  if (fp != ConsultBase)
    return (TRUE);
  else
    return (FALSE);
}

static Int 
p_first_cl_in_f(void)
{				/* '$first_cl_in_file'(+N,+Ar,+Mod) */
  unsigned int    arity;
  Atom            at;
  Term            t;
  register consult_obj  *fp;
  Prop            p0;
  SMALLUNSGN      mod;
  

  if (IsVarTerm(t = Deref(ARG1)) || !IsAtomTerm(t))
    return (FALSE);
  else
    at = AtomOfTerm(t);
  if (IsVarTerm(t = Deref(ARG2)) || !IsIntTerm(t))
    return (FALSE);
  else
    arity = IntOfTerm(t);
  if (IsVarTerm(t = Deref(ARG3)) || !IsAtomTerm(t))
    return (FALSE);
  else
    mod = Yap_LookupModule(t);
  if (arity)
    p0 = PredPropByFunc(Yap_MkFunctor(at, arity),mod);
  else
    p0 = PredPropByAtom(at, mod);
  for (fp = ConsultSp; fp < ConsultBase; ++fp)
    if (fp->p == p0)
      break;
  if (fp != ConsultBase)
    return (FALSE);
  return (TRUE);
}

static Int 
p_mk_cl_not_first(void)
{				/* '$mk_cl_not_first'(+N,+Ar) */
  unsigned int    arity;
  Atom            at;
  Term            t;
  Prop            p0;

  if (IsVarTerm(t = Deref(ARG1)) && !IsAtomTerm(t))
    return (FALSE);
  else
    at = AtomOfTerm(t);
  if (IsVarTerm(t = Deref(ARG2)) && !IsIntTerm(t))
    return (FALSE);
  else
    arity = IntOfTerm(t);
  if (arity)
    p0 = PredPropByFunc(Yap_MkFunctor(at, arity),CurrentModule);
  else
    p0 = PredPropByAtom(at, CurrentModule);
  --ConsultSp;
  ConsultSp->p = p0;
  return (TRUE);
}

#if EMACS
static int 
last_clause_number(p)
     PredEntry      *p;
{
  int             i = 1;
  yamop        *q = p->cs.p_code.FirstClause;

  if (q == NIL)
    return (0);
  while (q != p->cs.p_code.LastClause) {
    q = NextClause(q);
    i++;
  }
  return (i);
}


/*
 * the place where one would add a new clause for the propriety pred_prop 
 */
int 
where_new_clause(pred_prop, mode)
     Prop            pred_prop;
     int             mode;
{
  PredEntry      *p = RepPredProp(pred_prop);

  if (mode == consult && not_was_reconsulted(p, TermNil, FALSE))
    return (1);
  else
    return (last_clause_number(p) + 1);
}
#endif

static Int 
p_compile(void)
{				/* '$compile'(+C,+Flags, Mod) */
  Term            t = Deref(ARG1);
  Term            t1 = Deref(ARG2);
  Term            t3 = Deref(ARG3);
  yamop           *codeadr;
  Int mod;

  if (IsVarTerm(t1) || !IsIntTerm(t1))
    return (FALSE);
  if (IsVarTerm(t3) || !IsAtomTerm(t3))
    return (FALSE);
  mod = Yap_LookupModule(t3);
  codeadr = Yap_cclause(t, 2, mod); /* vsc: give the number of arguments
			      to cclause in case there is overflow */
  t = Deref(ARG1);        /* just in case there was an heap overflow */
  if (!Yap_ErrorMessage)
    addclause(t, codeadr, (int) (IntOfTerm(t1) & 3), mod);
  if (Yap_ErrorMessage) {
    if (IntOfTerm(t1) & 4) {
      Yap_Error(Yap_Error_TYPE, Yap_Error_Term,
	    "in line %d, %s", Yap_FirstLineInParse(), Yap_ErrorMessage);
    } else
      Yap_Error(Yap_Error_TYPE, Yap_Error_Term, Yap_ErrorMessage);
    return (FALSE);
  }
  return (TRUE);
}

static Int 
p_compile_dynamic(void)
{				/* '$compile_dynamic'(+C,+Flags,Mod,-Ref) */
  Term            t = Deref(ARG1);
  Term            t1 = Deref(ARG2);
  Term            t3 = Deref(ARG3);
  DynamicClause  *cl;
  yamop        *code_adr;
  int             old_optimize;
  Int mod;

  if (IsVarTerm(t1) || !IsIntTerm(t1))
    return (FALSE);
  if (IsVarTerm(t3) || !IsAtomTerm(t3))
    return (FALSE);
  old_optimize = optimizer_on;
  optimizer_on = FALSE;
  mod = Yap_LookupModule(t3);
  code_adr = Yap_cclause(t, 3, mod); /* vsc: give the number of arguments to
			       cclause() in case there is a overflow */
  t = Deref(ARG1);        /* just in case there was an heap overflow */
  if (!Yap_ErrorMessage) {
    
    optimizer_on = old_optimize;
    cl = ClauseCodeToDynamicClause(code_adr);
    addclause(t, code_adr, (int) (IntOfTerm(t1) & 3), mod);
  } else {
    if (IntOfTerm(t1) & 4) {
      Yap_Error(Yap_Error_TYPE, Yap_Error_Term, "line %d, %s", Yap_FirstLineInParse(), Yap_ErrorMessage);
    } else
      Yap_Error(Yap_Error_TYPE, Yap_Error_Term, Yap_ErrorMessage);
    return (FALSE);
  }
  cl->ClFlags = DynamicMask;
  t = MkIntegerTerm((Int)code_adr);
  return(Yap_unify(ARG4, t));
}

static int      consult_level = 0;

static Atom
YapConsultingFile (void)
{
  if (consult_level == 0) {
    return(Yap_LookupAtom("user"));
  } else {
    return(Yap_LookupAtom(ConsultBase[2].filename));
  }
}

Atom
Yap_ConsultingFile (void)
{
  return YapConsultingFile();
}

/* consult file *file*, *mode* may be one of either consult or reconsult */
static void
init_consult(int mode, char *file)
{
  ConsultSp--;
  ConsultSp->filename = file;
  ConsultSp--;
  ConsultSp->mode = mode;
  ConsultSp--;
  ConsultSp->c = (ConsultBase-ConsultSp);
  ConsultBase = ConsultSp;
#if !defined(YAPOR) && !defined(SBA)
  /*  if (consult_level == 0)
      do_toggle_static_predicates_in_use(TRUE); */
#endif
  consult_level++;
}

void
Yap_init_consult(int mode, char *file)
{
  init_consult(mode,file);
}

static Int 
p_startconsult(void)
{				/* '$start_consult'(+Mode)	 */
  Term            t;
  char           *smode = RepAtom(AtomOfTerm(Deref(ARG1)))->StrOfAE;
  int             mode;
  
  mode = strcmp("consult",smode);
  init_consult(mode, RepAtom(AtomOfTerm(Deref(ARG2)))->StrOfAE);
  t = MkIntTerm(consult_level);
  return (Yap_unify_constant(ARG3, t));
}

static Int 
p_showconslultlev(void)
{
  Term            t;

  t = MkIntTerm(consult_level);
  return (Yap_unify_constant(ARG1, t));
}

static void
end_consult(void)
{
#if defined(YAPOR) || defined(THREADS)
  consult_obj  *fp;

  /* force indexing for static and dynamic update predicates
     after consult and not when all hell may break loose ! */
  for (fp = ConsultSp; fp < ConsultBase; ++fp) {
    PredEntry *pred = RepPredProp(fp->p);
    WRITE_LOCK(pred->PRWLock);
    if (pred->OpcodeOfPred == INDEX_OPCODE) {
      IPred(pred);
      /* IPred does the unlocking */
    } else {
      WRITE_UNLOCK(pred->PRWLock);
    }
  }
#endif
  ConsultSp = ConsultBase;
  ConsultBase = ConsultSp+ConsultSp->c;
  ConsultSp += 3;
  consult_level--;
#if !defined(YAPOR) && !defined(SBA)
  /*  if (consult_level == 0)
      do_toggle_static_predicates_in_use(FALSE);*/
#endif
}

void
Yap_end_consult(void) {
  end_consult();
}


static Int 
p_endconsult(void)
{				/* '$end_consult'		 */
  end_consult();
  return (TRUE);
}

static Int 
p_purge_clauses(void)
{				/* '$purge_clauses'(+Func) */
  PredEntry      *pred;
  Term            t = Deref(ARG1);
  Term            t2 = Deref(ARG2);
  yamop          *q, *q1;
  SMALLUNSGN      mod;
  int		  in_use;

  Yap_PutValue(AtomAbol, MkAtomTerm(AtomNil));
  if (IsVarTerm(t))
    return (FALSE);
  if (IsVarTerm(t2) || !IsAtomTerm(t2)) {
    return (FALSE);
  }
  mod = Yap_LookupModule(t2);
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pred = RepPredProp(PredPropByAtom(at, mod));
  } else if (IsApplTerm(t)) {
    Functor         fun = FunctorOfTerm(t);
    pred = RepPredProp(PredPropByFunc(fun, mod));
  } else
    return (FALSE);
  WRITE_LOCK(pred->PRWLock);
  if (pred->PredFlags & StandardPredFlag) {
    WRITE_UNLOCK(pred->PRWLock);
    Yap_Error(PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE, t, "assert/1");
    return (FALSE);
  }
  if (pred->PredFlags & IndexedPredFlag)
    RemoveIndexation(pred);
  Yap_PutValue(AtomAbol, MkAtomTerm(AtomTrue));
  q = pred->cs.p_code.FirstClause;
  in_use = static_in_use(pred,TRUE);
  if (q != NIL)
    do {
      q1 = q;
      q = NextClause(q);
      if (pred->PredFlags & LogUpdatePredFlag)
	Yap_ErLogUpdCl(ClauseCodeToLogUpdClause(q1));
      else {
	StaticClause *cl = ClauseCodeToStaticClause(q1);
	if (cl->ClFlags & HasBlobsMask || in_use) {
	  DeadClause *dcl = (DeadClause *)cl;
	  dcl->NextCl = DeadClauses;
	  dcl->ClFlags = 0;
	  DeadClauses = dcl;
	} else {
	  Yap_FreeCodeSpace((char *)cl);
	}
      }
    } while (q1 != pred->cs.p_code.LastClause);
  pred->cs.p_code.FirstClause = pred->cs.p_code.LastClause = NULL;
  if (pred->PredFlags & (DynamicPredFlag|LogUpdatePredFlag)) {
    pred->OpcodeOfPred = FAIL_OPCODE;
  } else {
    pred->OpcodeOfPred = UNDEF_OPCODE;
  }
  pred->cs.p_code.TrueCodeOfPred =
    pred->CodeOfPred =
    (yamop *)(&(pred->OpcodeOfPred)); 
  pred->OwnerFile = AtomNil;
  if (pred->PredFlags & MultiFileFlag)
    pred->PredFlags ^= MultiFileFlag;
  WRITE_UNLOCK(pred->PRWLock);
  return (TRUE);
}

/******************************************************************
  
		MANAGING SPY-POINTS
  
******************************************************************/

static Int 
p_setspy(void)
{				/* '$set_spy'(+Fun,+M)	 */
  Atom            at;
  PredEntry      *pred;
  CELL            fg;
  Term            t;
  Term            t2;
  SMALLUNSGN	  mod;

  at = Yap_FullLookupAtom("$spy");
  pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1),0));
  SpyCode = pred;
  t = Deref(ARG1);
  t2 = Deref(ARG2);
  if (IsVarTerm(t))
    return (FALSE);
  if (IsVarTerm(t2) || !IsAtomTerm(t2))
    return (FALSE);
  mod = Yap_LookupModule(t2);
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pred = RepPredProp(PredPropByAtom(at, mod));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pred = RepPredProp(PredPropByFunc(fun, mod));
  } else {
    return (FALSE);
  }
 restart_spy:
  WRITE_LOCK(pred->PRWLock);
  if (pred->PredFlags & (CPredFlag | SafePredFlag)) {
    WRITE_UNLOCK(pred->PRWLock);
    return (FALSE);
  }
  if (pred->OpcodeOfPred == UNDEF_OPCODE ||
      pred->OpcodeOfPred == FAIL_OPCODE) {
    WRITE_UNLOCK(pred->PRWLock);
    return (FALSE);
  }
  if (pred->OpcodeOfPred == INDEX_OPCODE) {
    IPred(pred);
    goto restart_spy;
  }
  fg = pred->PredFlags;
  if (fg & DynamicPredFlag) {
    pred->OpcodeOfPred =
      ((yamop *)(pred->CodeOfPred))->opc =
      Yap_opcode(_spy_or_trymark);
  } else {
    pred->OpcodeOfPred = Yap_opcode(_spy_pred);
    pred->CodeOfPred = (yamop *)(&(pred->OpcodeOfPred)); 
  }
  pred->PredFlags |= SpiedPredFlag;
  WRITE_UNLOCK(pred->PRWLock);
  return (TRUE);
}

static Int 
p_rmspy(void)
{				/* '$rm_spy'(+T,+Mod)	 */
  Atom            at;
  PredEntry      *pred;
  Term            t;
  Term            t2;
  SMALLUNSGN	  mod;

  t = Deref(ARG1);
  t2 = Deref(ARG2);
  if (IsVarTerm(t2) || !IsAtomTerm(t2))
    return (FALSE);
  mod = Yap_LookupModule(t2);
  if (IsVarTerm(t))
    return (FALSE);
  if (IsAtomTerm(t)) {
    at = AtomOfTerm(t);
    pred = RepPredProp(PredPropByAtom(at, mod));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pred = RepPredProp(PredPropByFunc(fun, mod));
  } else
    return (FALSE);
  WRITE_LOCK(pred->PRWLock);
  if (!(pred->PredFlags & SpiedPredFlag)) {
    WRITE_UNLOCK(pred->PRWLock);
    return (FALSE);
  }
  if (!(pred->PredFlags & DynamicPredFlag)) {
    pred->CodeOfPred = pred->cs.p_code.TrueCodeOfPred;
    pred->OpcodeOfPred = ((yamop *)(pred->CodeOfPred))->opc;
  } else if (pred->OpcodeOfPred == Yap_opcode(_spy_or_trymark)) {
    pred->OpcodeOfPred = Yap_opcode(_try_and_mark);
  } else
    return (FALSE);
  pred->PredFlags ^= SpiedPredFlag;
  WRITE_UNLOCK(pred->PRWLock);
  return (TRUE);
}


/******************************************************************
  
		INFO ABOUT PREDICATES
  
******************************************************************/

static Int 
p_number_of_clauses(void)
{				/* '$number_of_clauses'(Predicate,M,N) */
  Term            t = Deref(ARG1);
  Term            t2 = Deref(ARG2);
  int ncl = 0;
  Prop            pe;
  yamop          *q;
  int             mod;

  if (IsVarTerm(t2)  || !IsAtomTerm(t2)) {
    return(FALSE);
  }
  mod = Yap_LookupModule(t2);
  if (IsAtomTerm(t)) {
    Atom a = AtomOfTerm(t);
    pe = PredPropByAtom(a, mod);
  } else if (IsApplTerm(t)) {
    register Functor f = FunctorOfTerm(t);
    pe = PredPropByFunc(f, mod);
  } else
    return (FALSE);
  q = RepPredProp(pe)->cs.p_code.FirstClause;
  READ_LOCK(RepPredProp(pe)->PRWLock);
  if (q != NIL) {
    while (q != RepPredProp(pe)->cs.p_code.LastClause) {
      ncl++;
      q = NextClause(q);
    }
  }
  READ_UNLOCK(RepPredProp(pe)->PRWLock);
  t = MkIntegerTerm(ncl);
  return (Yap_unify_constant(ARG3, t));
}

static Int 
p_in_use(void)
{				/* '$in_use'(+P,+Mod)	 */
  Term            t = Deref(ARG1);
  Term            t2 = Deref(ARG2);
  PredEntry      *pe;
  Int            out;
  int            mod;

  if (IsVarTerm(t))
    return (FALSE);
  if (IsVarTerm(t2) || !IsAtomTerm(t2))
    return (FALSE);
  mod = Yap_LookupModule(t2);
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pe = RepPredProp(PredPropByAtom(at, mod));
  } else if (IsApplTerm(t)) {
    Functor         fun = FunctorOfTerm(t);
    pe = RepPredProp(PredPropByFunc(fun, mod));
  } else
    return (FALSE);
  READ_LOCK(pe->PRWLock);
  out = static_in_use(pe,TRUE);
  READ_UNLOCK(pe->PRWLock);
  return(out);
}

static Int 
p_new_multifile(void)
{				/* '$new_multifile'(+N,+Ar,+Mod)  */
  Atom            at;
  int             arity;
  PredEntry      *pe;
  Term            t = Deref(ARG1);
  SMALLUNSGN      mod = Yap_LookupModule(Deref(ARG3));

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
    return (FALSE);
  if (arity == 0) 
    pe = RepPredProp(PredPropByAtom(at, mod));
  else 
    pe = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, arity),mod));
  WRITE_LOCK(pe->PRWLock);
  pe->PredFlags |= MultiFileFlag;
  WRITE_UNLOCK(pe->PRWLock);
  return (TRUE);
}


static Int 
p_is_multifile(void)
{				/* '$is_multifile'(+S,+Mod)	 */
  PredEntry      *pe;
  Term            t = Deref(ARG1);
  Term            t2 = Deref(ARG2);
  Int		  out;
  int		  mod;

  if (IsVarTerm(t))
    return (FALSE);
  if (IsVarTerm(t2))
    return (FALSE);
  if (!IsAtomTerm(t2))
    return (FALSE);
  mod = Yap_LookupModule(t2);
  if (IsAtomTerm(t)) {
    pe = RepPredProp(PredPropByAtom(AtomOfTerm(t), mod));
  } else if (IsApplTerm(t)) {
    pe = RepPredProp(PredPropByFunc(FunctorOfTerm(t), mod));
  } else
    return(FALSE);
  if (EndOfPAEntr(pe))
    return (FALSE);
  READ_LOCK(pe->PRWLock);
  out = (pe->PredFlags & MultiFileFlag);
  READ_UNLOCK(pe->PRWLock);
  return(out);
}

static Int 
p_is_dynamic(void)
{				/* '$is_dynamic'(+P)	 */
  PredEntry      *pe;
  Term            t = Deref(ARG1);
  Term            t2 = Deref(ARG2);
  Int             out;
  SMALLUNSGN      mod = Yap_LookupModule(t2);

  if (IsVarTerm(t)) {
    return (FALSE);
  } else if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pe = RepPredProp(PredPropByAtom(at, mod));
  } else if (IsApplTerm(t)) {
    Functor         fun = FunctorOfTerm(t);
    pe = RepPredProp(PredPropByFunc(fun, mod));
  } else
    return (FALSE);
  if (pe == NIL)
    return (FALSE);
  READ_LOCK(pe->PRWLock);
  out = (pe->PredFlags & (DynamicPredFlag|LogUpdatePredFlag));
  READ_UNLOCK(pe->PRWLock);
  return(out);
}

static Int 
p_pred_exists(void)
{				/* '$pred_exists'(+P,+M)	 */
  PredEntry      *pe;
  Term            t = Deref(ARG1);
  Term            t2 = Deref(ARG2);
  Int             out;
  SMALLUNSGN      mod = Yap_LookupModule(t2);

  if (IsVarTerm(t)) {
    return (FALSE);
  } else if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pe = RepPredProp(PredPropByAtom(at, mod));
  } else if (IsApplTerm(t)) {
    Functor         fun = FunctorOfTerm(t);
    pe = RepPredProp(PredPropByFunc(fun, mod));
  } else
    return (FALSE);
  if (pe == NIL)
    return (FALSE);
  READ_LOCK(pe->PRWLock);
  if (pe->PredFlags & HiddenPredFlag)
    return(FALSE);
  out = (pe->OpcodeOfPred != UNDEF_OPCODE);
  READ_UNLOCK(pe->PRWLock);
  return(out);
}

static Int 
p_set_pred_module(void)
{				/* '$set_pred_module'(+P,+Mod)	 */
  PredEntry      *pe;
  Term            t = Deref(ARG1);
  SMALLUNSGN      mod = CurrentModule;

 restart_set_pred:
  if (IsVarTerm(t)) {
    return (FALSE);
  } else if (IsAtomTerm(t)) {
    pe = RepPredProp(PredPropByAtom(AtomOfTerm(t), mod));
  } else if (IsApplTerm(t)) {
    Functor         fun = FunctorOfTerm(t);
    if (fun == FunctorModule) {
      Term tmod = ArgOfTerm(1, t);
      if (IsVarTerm(tmod) ) {
	Yap_Error(INSTANTIATION_ERROR,ARG1,"set_pred_module/1");
	return(FALSE);
      }
      if (!IsAtomTerm(tmod) ) {
	Yap_Error(TYPE_ERROR_ATOM,ARG1,"set_pred_module/1");
	return(FALSE);
      }
      mod = Yap_LookupModule(tmod);
      t = ArgOfTerm(2, t);
      goto restart_set_pred;
    }
    pe = RepPredProp(PredPropByFunc(fun, mod));
  } else
    return (FALSE);
  if (EndOfPAEntr(pe))
    return (FALSE);
  WRITE_LOCK(pe->PRWLock);
  {
    SMALLUNSGN mod = Yap_LookupModule(Deref(ARG2));
    pe->ModuleOfPred = mod;
  }
  WRITE_UNLOCK(pe->PRWLock);
  return(TRUE);
}

static Int 
p_undefined(void)
{				/* '$undefined'(P,Mod)	 */
  PredEntry      *pe;
  Term            t;
  Term            t2;
  SMALLUNSGN      mod;

  t = Deref(ARG1);
  t2 = Deref(ARG2);
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR,ARG2,"undefined/1");
    return(FALSE);
  }
  if (!IsAtomTerm(t2)) {
    Yap_Error(TYPE_ERROR_ATOM,ARG2,"undefined/1");
    return(FALSE);
  }
  mod = Yap_LookupModule(t2);
 restart_undefined:
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,ARG1,"undefined/1");
    return(FALSE);
  }
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pe = RepPredProp(Yap_GetPredPropByAtom(at,mod));
  } else if (IsApplTerm(t)) {
    Functor         funt = FunctorOfTerm(t);
    if (funt == FunctorModule) {
      Term tmod = ArgOfTerm(1, t);
      if (IsVarTerm(tmod) ) {
	Yap_Error(INSTANTIATION_ERROR,ARG1,"undefined/1");
	return(FALSE);
      }
      if (!IsAtomTerm(tmod) ) {
	Yap_Error(TYPE_ERROR_ATOM,ARG1,"undefined/1");
	return(FALSE);
      }
      mod = Yap_LookupModule(tmod);
      t = ArgOfTerm(2, t);
      goto restart_undefined;
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(funt, mod));
  } else {
    return (FALSE);
  }
  if (pe == RepPredProp(NIL))
    return (TRUE);
  READ_LOCK(pe->PRWLock);
  if (pe->PredFlags & (CPredFlag|UserCPredFlag|TestPredFlag|AsmPredFlag|DynamicPredFlag|LogUpdatePredFlag)) {
    READ_UNLOCK(pe->PRWLock);
    return(FALSE);
  }
  if (pe->OpcodeOfPred == UNDEF_OPCODE) {
    READ_UNLOCK(pe->PRWLock);
    return (TRUE);
  }
  READ_UNLOCK(pe->PRWLock);
  return (FALSE);
}

/*
 * this predicate should only be called when all clauses for the dynamic
 * predicate were remove, otherwise chaos will follow!! 
 */

static Int 
p_kill_dynamic(void)
{				/* '$kill_dynamic'(P,M)       */
  PredEntry      *pe;
  Term            t;
  Term            t2;
  SMALLUNSGN      mod;

  t2 = Deref(ARG2);
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR,ARG2,"undefined/1");
    return(FALSE);
  }
  if (!IsAtomTerm(t2)) {
    Yap_Error(TYPE_ERROR_ATOM,ARG2,"undefined/1");
    return(FALSE);
  }
  mod = Yap_LookupModule(t2);
  t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pe = RepPredProp(PredPropByAtom(at, mod));
  } else if (IsApplTerm(t)) {
    Functor         funt = FunctorOfTerm(t);
    pe = RepPredProp(PredPropByFunc(funt, mod));
  } else
    return (FALSE);
  if (EndOfPAEntr(pe))
    return (TRUE);
  WRITE_LOCK(pe->PRWLock);
  if (!(pe->PredFlags & DynamicPredFlag)) {
    WRITE_UNLOCK(pe->PRWLock);
    return (FALSE);
  }
  if (pe->cs.p_code.LastClause != pe->cs.p_code.FirstClause) {
    WRITE_UNLOCK(pe->PRWLock);
    return (FALSE);
  }
  pe->cs.p_code.LastClause = pe->cs.p_code.FirstClause = NIL;
  pe->OpcodeOfPred = UNDEF_OPCODE;
  pe->cs.p_code.TrueCodeOfPred = pe->CodeOfPred = (yamop *)(&(pe->OpcodeOfPred)); 
  pe->PredFlags = 0L;
  WRITE_UNLOCK(pe->PRWLock);
  return (TRUE);
}

static Int 
p_optimizer_on(void)
{				/* '$optimizer_on'		 */
  optimizer_on = TRUE;
  return (TRUE);
}

static Int 
p_optimizer_off(void)
{				/* '$optimizer_off'		 */
  optimizer_on = FALSE;
  return (TRUE);
}

static Int 
p_compile_mode(void)
{				/* $compile_mode(Old,New)	 */
  Term            t2, t3 = MkIntTerm(compile_mode);
  if (!Yap_unify_constant(ARG1, t3))
    return (FALSE);
  t2 = Deref(ARG2);
  if (IsVarTerm(t2) || !IsIntTerm(t2))
    return (FALSE);
  compile_mode = IntOfTerm(t2) & 1;
  return (TRUE);
}

#if !defined(YAPOR)
static yamop *cur_clause(PredEntry *pe, yamop *codeptr)
{
  yamop *clcode;
  StaticClause *cl;
  clcode = pe->cs.p_code.FirstClause;
  cl = ClauseCodeToStaticClause(clcode);
  do {
    if (IN_BLOCK(codeptr,cl,Yap_SizeOfBlock((CODEADDR)cl))) {
      return((yamop *)clcode);
    }
    if (clcode == pe->cs.p_code.LastClause)
      break;
    cl = ClauseCodeToStaticClause(clcode = NextClause(clcode));
  } while (TRUE);
  Yap_Error(SYSTEM_ERROR,TermNil,"could not find clause for indexing code");
  return(NULL);
}

static yamop *cur_log_upd_clause(PredEntry *pe, yamop *codeptr)
{
  yamop *clcode;
  LogUpdClause *cl;
  clcode = pe->cs.p_code.FirstClause;
  cl = ClauseCodeToLogUpdClause(clcode);
  do {
    if (IN_BLOCK(codeptr,cl,Yap_SizeOfBlock((CODEADDR)cl))) {
      return((yamop *)clcode);
    }
    if (clcode == pe->cs.p_code.LastClause)
      break;
    cl = ClauseCodeToLogUpdClause(clcode = NextClause(clcode));
  } while (TRUE);
  Yap_Error(SYSTEM_ERROR,TermNil,"could not find clause for indexing code");
  return(NULL);
}

static Int
search_for_static_predicate_in_use(PredEntry *p, int check_everything)
{
  choiceptr b_ptr = B;
  CELL *env_ptr = ENV;

  if (check_everything) {
    PredEntry *pe = EnvPreg(P);
    if (p == pe) return(TRUE);
    pe = EnvPreg(CP);
    if (p == pe) return(TRUE);
  }
  do {
    /* check first environments that are younger than our latest choicepoint */
    if (check_everything) {
      /* 
	 I do not need to check environments for asserts,
	 only for retracts
      */
      while (b_ptr > (choiceptr)env_ptr) {
	PredEntry *pe = EnvPreg(env_ptr[E_CP]);
	if (p == pe) return(TRUE);
	if (env_ptr != NULL)
	  env_ptr = (CELL *)(env_ptr[E_E]);
      }
    }
    /* now mark the choicepoint */
    if (b_ptr != NULL) {
      PredEntry *pe;
      op_numbers opnum = Yap_op_from_opcode(b_ptr->cp_ap->opc);
      
    restart_cp:
      switch(opnum) {
      case _or_else:
      case _or_last:
	if (!check_everything) {
	  b_ptr = b_ptr->cp_b;
	  continue;
	}
#ifdef YAPOR
	pe = b_ptr->cp_cp->u.ldl.p;
#else
	pe = b_ptr->cp_cp->u.sla.p0;
#endif /* YAPOR */
	break;
      case _retry_profiled:
	opnum = Yap_op_from_opcode(NEXTOP(b_ptr->cp_ap,l)->opc);
	goto restart_cp;
      case _count_retry:
	opnum = Yap_op_from_opcode(NEXTOP(b_ptr->cp_ap,l)->opc);
	goto restart_cp;
      default:
	pe = (PredEntry *)(b_ptr->cp_ap->u.ld.p);
      }
      if (pe == p) {
	if (check_everything)
	  return(TRUE);
	READ_LOCK(pe->PRWLock);
	if (p->PredFlags & IndexedPredFlag) {
	  yamop *code_p = b_ptr->cp_ap;
	  char *code_end;

	  if (p->PredFlags & LogUpdatePredFlag) {
	    LogUpdClause *cl = ClauseCodeToLogUpdClause(p->cs.p_code.TrueCodeOfPred);
	    code_end = (char *)cl + Yap_SizeOfBlock((CODEADDR)cl);
	  } else {
	    StaticClause *cl = ClauseCodeToStaticClause(p->cs.p_code.TrueCodeOfPred);
	    code_end = (char *)cl + Yap_SizeOfBlock((CODEADDR)cl);
	  }
	  if (code_p >= p->cs.p_code.TrueCodeOfPred &&
	      code_p <= (yamop *)code_end) {
	    /* fix the choicepoint */
	    if (p->PredFlags & LogUpdatePredFlag) {
	      b_ptr->cp_ap = cur_log_upd_clause(pe, b_ptr->cp_ap->u.ld.d);
	    } else {
	      b_ptr->cp_ap = cur_clause(pe, b_ptr->cp_ap->u.ld.d);
	    }
	  }	    
	  READ_UNLOCK(pe->PRWLock);
	} else {
	  READ_UNLOCK(pe->PRWLock);
	}
      }
      env_ptr = b_ptr->cp_env;
      b_ptr = b_ptr->cp_b;
    }
  } while (b_ptr != NULL);
  return(FALSE);
}

static void
mark_pred(int mark, PredEntry *pe)
{
  /* if the predicate is static mark it */
  if (pe->ModuleOfPred) {
    WRITE_LOCK(pe->PRWLock);
    if (mark) {
      pe->PredFlags |= InUsePredFlag;
    } else {
      pe->PredFlags &= ~InUsePredFlag;
    }
    WRITE_UNLOCK(pe->PRWLock);
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
      PredEntry *pe = EnvPreg(env_ptr[E_CP]);
      
      mark_pred(mask, pe);
      env_ptr = (CELL *)(env_ptr[E_E]);
    }
    /* now mark the choicepoint */
    {
      op_numbers opnum;
    restart_cp:
      opnum = Yap_op_from_opcode(b_ptr->cp_ap->opc);
      
      switch(opnum) {
      case _or_else:
      case _or_last:
#ifdef YAPOR
	pe = b_ptr->cp_cp->u.ldl.p;
#else
	pe = b_ptr->cp_cp->u.sla.p0;
#endif /* YAPOR */
	break;
      case _Nstop:
	pe = NULL;
	break;
      case _retry_profiled:
	opnum = Yap_op_from_opcode(NEXTOP(b_ptr->cp_ap,l)->opc);
	goto restart_cp;
      case _count_retry:
	opnum = Yap_op_from_opcode(NEXTOP(b_ptr->cp_ap,l)->opc);
	goto restart_cp;
      default:
	pe = (PredEntry *)(b_ptr->cp_ap->u.ld.p);
      }
      if (pe != NULL)
	mark_pred(mask, pe);
      env_ptr = b_ptr->cp_env;
      b_ptr = b_ptr->cp_b;
    }
  } while (b_ptr != NULL);
  /* mark or unmark all predicates */
  STATIC_PREDICATES_MARKED = mask;
}

#endif

static Term
all_envs(CELL *env_ptr)
{
  Term tf = AbsPair(H);
  CELL *bp = NULL;
  
  /* walk the environment chain */
  while (env_ptr != NULL) {
    bp = H;
    H += 2;
    /* notice that MkIntegerTerm may increase the Heap */
    bp[0] = MkIntegerTerm((Int)env_ptr[E_CP]);
    if (H >= ASP) {
      bp[1] = TermNil;
      return tf;
    } else {
      bp[1] = AbsPair(H);
    }
    env_ptr = (CELL *)(env_ptr[E_E]);      
  }
  bp[1] = TermNil;
  return tf;
}

static Term
all_cps(choiceptr b_ptr)
{
  CELL *bp = NULL;
  Term tf = AbsPair(H);

  while (b_ptr != NULL) {
    bp = H;
    H += 2;
    /* notice that MkIntegerTerm may increase the Heap */
    bp[0] = MkIntegerTerm((Int)b_ptr->cp_ap);
    if (H >= ASP) {
      bp[1] = TermNil;
      return tf;
    } else {
      bp[1] = AbsPair(H);
    }
    b_ptr = b_ptr->cp_b;
  }
  bp[1] = TermNil;
  return tf;
}


static Term
all_calls(void)
{
  Term ts[3];
  Functor f = Yap_MkFunctor(AtomLocal,3);

  ts[0] = MkIntegerTerm((Int)P);
  if (yap_flags[STACK_DUMP_ON_ERROR_FLAG]) {
    ts[1] = all_envs(ENV);
    ts[1] = all_cps(B);
  } else {
    ts[1] = ts[2] = TermNil;
  }
  return(Yap_MkApplTerm(f,3,ts));
}

Term
Yap_all_calls(void)
{
  return all_calls();
}

static Int
p_current_stack(void)
{
#ifdef YAPOR
  return(FALSE);
#else
  return(Yap_unify(ARG1,all_calls()));
#endif
}

/* This predicate is to be used by reconsult to mark all predicates
   currently in use as being executed.

   The idea is to go up the chain of choice_points and environments.

 */
static Int
p_toggle_static_predicates_in_use(void)
{
#if !defined(YAPOR) && !defined(THREADS)
  Term t = Deref(ARG1);
  Int mask;
  
  /* find out whether we need to mark or unmark */
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"toggle_static_predicates_in_use/1");
    return(FALSE);
  }
  if (!IsIntTerm(t)) {
    Yap_Error(TYPE_ERROR_INTEGER,t,"toggle_static_predicates_in_use/1");
    return(FALSE);
  }  else {
    mask = IntOfTerm(t);
  }
  do_toggle_static_predicates_in_use(mask);
#endif
  return(TRUE);
}


static Int
code_in_pred(PredEntry *pp, Atom *pat, UInt *parity, yamop *codeptr) {
  yamop *clcode;
  int i = 1;

  READ_LOCK(pp->PRWLock);
  clcode = pp->cs.p_code.FirstClause;
  if (clcode != NULL) {
    char *code_end;
    if (pp->PredFlags & LogUpdatePredFlag) {
      LogUpdClause *cl = ClauseCodeToLogUpdClause(pp->cs.p_code.TrueCodeOfPred);
      code_end = (char *)cl + Yap_SizeOfBlock((CODEADDR)cl);
    } else if (!(pp->PredFlags & DynamicPredFlag)) {
      code_end = NULL;
    } else {
      StaticClause *cl = ClauseCodeToStaticClause(pp->cs.p_code.TrueCodeOfPred);
      code_end = (char *)cl + Yap_SizeOfBlock((CODEADDR)cl);
    }
    /* check if the codeptr comes from the indexing code */
    if ((pp->PredFlags & IndexedPredFlag) &&
	IN_BLOCK(codeptr,pp->cs.p_code.TrueCodeOfPred,Yap_SizeOfBlock((CODEADDR)(pp->cs.p_code.TrueCodeOfPred)))) {
      *parity = pp->ArityOfPE;
      if (pp->ArityOfPE) {
	*pat = NameOfFunctor(pp->FunctorOfPred);
      } else {
	*pat = (Atom)(pp->FunctorOfPred);
      }
      READ_UNLOCK(pp->PRWLock);
      return(-1);
    }	      
    do {
      CODEADDR cl;

      if (pp->PredFlags & LogUpdatePredFlag) {
	cl = (CODEADDR)ClauseCodeToLogUpdClause(clcode);
      } else if (!(pp->PredFlags & DynamicPredFlag)) {
	cl = (CODEADDR)ClauseCodeToDynamicClause(clcode);
      } else {
	cl = (CODEADDR)ClauseCodeToStaticClause(clcode);
      }
      if (IN_BLOCK(codeptr,cl,Yap_SizeOfBlock((CODEADDR)cl))) {
	/* we found it */
	*parity = pp->ArityOfPE;
	if (pp->ArityOfPE) {
	  *pat = NameOfFunctor(pp->FunctorOfPred);
	} else {
	  *pat = (Atom)(pp->FunctorOfPred);
	}
	READ_UNLOCK(pp->PRWLock);
	return(i);
      }
      if (clcode == pp->cs.p_code.LastClause)
	break;
      i++;
      clcode = NextClause(clcode);
    } while (TRUE);
  }
  READ_UNLOCK(pp->PRWLock); 
  return(0);
}

static Int
PredForCode(yamop *codeptr, Atom *pat, UInt *parity, SMALLUNSGN *pmodule) {
  Int found = 0;
  Int i_table;

  /* should we allow the user to see hidden predicates? */
  for (i_table = NoOfModules-1; i_table >= 0; --i_table) {
    PredEntry *pp = ModulePred[i_table];
    while (pp != NULL) {
      if ((found = code_in_pred(pp,  pat, parity, codeptr)) != 0) {
	*pmodule = i_table;
	return(found);
      }
      pp = pp->NextPredOfModule;
    }
  }
  return(0);
}

Int
Yap_PredForCode(yamop *codeptr, Atom *pat, UInt *parity, SMALLUNSGN *pmodule) {
  return PredForCode(codeptr, pat, parity, pmodule);
}


static Int
p_pred_for_code(void) {
  yamop *codeptr = (yamop *)IntegerOfTerm(Deref(ARG1));
  Atom at;
  UInt arity;
  SMALLUNSGN module;
  Int cl;

  cl = PredForCode(codeptr, &at, &arity, &module);
  if (cl == 0) return(Yap_unify(ARG5,MkIntegerTerm(cl)));
  return(Yap_unify(ARG2,MkAtomTerm(at)) &&
	 Yap_unify(ARG3,MkIntegerTerm(arity)) &&
	 Yap_unify(ARG4,ModuleName[module]) &&
	 Yap_unify(ARG5,MkIntegerTerm(cl)));
}

static Int
p_is_profiled(void)
{
  Term t = Deref(ARG1);
  char *s;

  if (IsVarTerm(t)) {
    Term ta;

    if (PROFILING) ta = MkAtomTerm(Yap_LookupAtom("on"));
    else ta = MkAtomTerm(Yap_LookupAtom("off"));
    BIND((CELL *)t,ta,bind_is_profiled);
#ifdef COROUTINING
    DO_TRAIL(CellPtr(t), ta);
    if (CellPtr(t) < H0) Yap_WakeUp((CELL *)t);
  bind_is_profiled:
#endif
    return(TRUE);
  } else if (!IsAtomTerm(t)) return(FALSE);
  s = RepAtom(AtomOfTerm(t))->StrOfAE;
  if (strcmp(s,"on") == 0) {
    PROFILING = TRUE;
    Yap_InitComma();
    return(TRUE);
  } else if (strcmp(s,"off") == 0) {
    PROFILING = FALSE;
    Yap_InitComma();
    return(TRUE);
  }
  return(FALSE);
}

static Int
p_profile_info(void)
{
  Term tmod = Deref(ARG1);
  Term tfun = Deref(ARG2);
  int mod;
  Term out;
  PredEntry *pe;
  Term p[3];

  if (IsVarTerm(tmod) || !IsAtomTerm(tmod))
    return(FALSE);
  mod = Yap_LookupModule(tmod);
  if (IsVarTerm(tfun)) {
    return(FALSE);
  } else if (IsApplTerm(tfun)) {
    Functor f = FunctorOfTerm(tfun);
    if (IsExtensionFunctor(f)) {
      return(FALSE);
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(f, mod));
  } else if (IsAtomTerm(tfun)) {
    pe = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(tfun), mod));
  } else {
    return(FALSE);
  }
  if (EndOfPAEntr(pe))
    return(FALSE);
  LOCK(pe->StatisticsForPred.lock);
  if (!(pe->StatisticsForPred.NOfEntries)) {
    UNLOCK(pe->StatisticsForPred.lock);
    return(FALSE);
  }
  p[0] = Yap_MkULLIntTerm(pe->StatisticsForPred.NOfEntries);
  p[1] = Yap_MkULLIntTerm(pe->StatisticsForPred.NOfHeadSuccesses);
  p[2] = Yap_MkULLIntTerm(pe->StatisticsForPred.NOfRetries);
  UNLOCK(pe->StatisticsForPred.lock);
  out = Yap_MkApplTerm(Yap_MkFunctor(AtomProfile,3),3,p);
  return(Yap_unify(ARG3,out));
}

static Int
p_profile_reset(void)
{
  Term tmod = Deref(ARG1);
  Term tfun = Deref(ARG2);
  int mod;
  PredEntry *pe;

  if (IsVarTerm(tmod) || !IsAtomTerm(tmod))
    return(FALSE);
  mod = Yap_LookupModule(tmod);
  if (IsVarTerm(tfun)) {
    return(FALSE);
  } else if (IsApplTerm(tfun)) {
    Functor f = FunctorOfTerm(tfun);
    if (IsExtensionFunctor(f)) {
      return(FALSE);
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(f, mod));
  } else if (IsAtomTerm(tfun)) {
    pe = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(tfun), mod));
  } else {
    return(FALSE);
  }
  if (EndOfPAEntr(pe))
    return(FALSE);
  LOCK(pe->StatisticsForPred.lock);
  pe->StatisticsForPred.NOfEntries = 0;
  pe->StatisticsForPred.NOfHeadSuccesses = 0;
  pe->StatisticsForPred.NOfRetries = 0;
  UNLOCK(pe->StatisticsForPred.lock);
  return(TRUE);
}

static Int
p_is_call_counted(void)
{
  Term t = Deref(ARG1);
  char *s;

  if (IsVarTerm(t)) {
    Term ta;

    if (CALL_COUNTING) ta = MkAtomTerm(Yap_LookupAtom("on"));
    else ta = MkAtomTerm(Yap_LookupAtom("off"));
    BIND((CELL *)t,ta,bind_is_call_counted);
#ifdef COROUTINING
    DO_TRAIL(CellPtr(t), ta);
    if (CellPtr(t) < H0) Yap_WakeUp((CELL *)t);
  bind_is_call_counted:
#endif
    return(TRUE);
  } else if (!IsAtomTerm(t)) return(FALSE);
  s = RepAtom(AtomOfTerm(t))->StrOfAE;
  if (strcmp(s,"on") == 0) {
    CALL_COUNTING = TRUE;
    Yap_InitComma();
    return(TRUE);
  } else if (strcmp(s,"off") == 0) {
    CALL_COUNTING = FALSE;
    Yap_InitComma();
    return(TRUE);
  }
  return(FALSE);
}

static Int
p_call_count_info(void)
{
  return(Yap_unify(MkIntegerTerm(ReductionsCounter),ARG1) &&
	 Yap_unify(MkIntegerTerm(PredEntriesCounter),ARG2) &&
	 Yap_unify(MkIntegerTerm(PredEntriesCounter),ARG3));
}

static Int
p_call_count_reset(void)
{
  ReductionsCounter = 0;
  ReductionsCounterOn = FALSE;
  PredEntriesCounter = 0;
  PredEntriesCounterOn = FALSE;
  RetriesCounter = 0;
  RetriesCounterOn = FALSE;
  return(TRUE);
}

static Int
p_call_count_set(void)
{
  int do_calls = IntOfTerm(ARG2);
  int do_retries = IntOfTerm(ARG4);
  int do_entries = IntOfTerm(ARG6);

  if (do_calls)
    ReductionsCounter = IntegerOfTerm(Deref(ARG1));
  ReductionsCounterOn = do_calls;
  if (do_retries)
    RetriesCounter = IntegerOfTerm(Deref(ARG3));
  RetriesCounterOn = do_retries;
  if (do_entries)
    PredEntriesCounter = IntegerOfTerm(Deref(ARG5));
  PredEntriesCounterOn = do_entries;
  return(TRUE);
}

static Int
p_clean_up_dead_clauses(void)
{
  while (DeadClauses != NULL) {
    char *pt = (char *)DeadClauses;
    DeadClauses = DeadClauses->NextCl;
    Yap_FreeCodeSpace(pt);
  }
  return(TRUE);
}

static Int			/* $parent_pred(Module, Name, Arity) */
p_parent_pred(void)
{
  /* This predicate is called from the debugger.
     We assume a sequence of the form a -> b */
  Atom at;
  UInt arity;
  SMALLUNSGN module;
  if (!PredForCode(P_before_spy, &at, &arity, &module)) {
    return(Yap_unify(ARG1, MkIntTerm(0)) &&
	   Yap_unify(ARG2, MkAtomTerm(AtomMetaCall)) &&
	   Yap_unify(ARG3, MkIntTerm(0)));
  }
  return(Yap_unify(ARG1, MkIntTerm(module)) &&
	 Yap_unify(ARG2, MkAtomTerm(at)) &&
	 Yap_unify(ARG3, MkIntTerm(arity)));
}

static Int			/* $system_predicate(P) */
p_system_pred(void)
{
  PredEntry      *pe;

  Term t1 = Deref(ARG1);
  SMALLUNSGN mod = Yap_LookupModule(Deref(ARG2));

 restart_system_pred:
  if (IsVarTerm(t1))
    return (FALSE);
  if (IsAtomTerm(t1)) {
    pe = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(t1), mod));
  } else if (IsApplTerm(t1)) {
    Functor         funt = FunctorOfTerm(t1);
    if (IsExtensionFunctor(funt)) {
      return(FALSE);
    } 
    if (funt == FunctorModule) {
      Term nmod = ArgOfTerm(1, t1);
      if (IsVarTerm(nmod)) {
	Yap_Error(INSTANTIATION_ERROR,ARG1,"system_predicate/1");
	return(FALSE);
      } 
      if (!IsAtomTerm(nmod)) {
	Yap_Error(TYPE_ERROR_ATOM,ARG1,"system_predicate/1");
	return(FALSE);
      }
      t1 = ArgOfTerm(2, t1);
      goto restart_system_pred;
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(funt, mod));
  } else if (IsPairTerm(t1)) {
    return (TRUE);
  } else
    return (FALSE);
  if (EndOfPAEntr(pe))
    return(FALSE);
  return(pe->ModuleOfPred == 0 || pe->PredFlags & (UserCPredFlag|CPredFlag|BinaryTestPredFlag|AsmPredFlag|TestPredFlag));
}

static Int			/* $system_predicate(P) */
p_hide_predicate(void)
{
  PredEntry      *pe;

  Term t1 = Deref(ARG1);
  SMALLUNSGN mod = Yap_LookupModule(Deref(ARG2));

 restart_system_pred:
  if (IsVarTerm(t1))
    return (FALSE);
  if (IsAtomTerm(t1)) {
    pe = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(t1), mod));
  } else if (IsApplTerm(t1)) {
    Functor         funt = FunctorOfTerm(t1);
    if (IsExtensionFunctor(funt)) {
      return(FALSE);
    } 
    if (funt == FunctorModule) {
      Term nmod = ArgOfTerm(1, t1);
      if (IsVarTerm(nmod)) {
	Yap_Error(INSTANTIATION_ERROR,ARG1,"hide_predicate/1");
	return(FALSE);
      } 
      if (!IsAtomTerm(nmod)) {
	Yap_Error(TYPE_ERROR_ATOM,ARG1,"hide_predicate/1");
	return(FALSE);
      }
      t1 = ArgOfTerm(2, t1);
      goto restart_system_pred;
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(funt, mod));
  } else if (IsPairTerm(t1)) {
    return (TRUE);
  } else
    return (FALSE);
  if (EndOfPAEntr(pe))
    return(FALSE);
  pe->PredFlags |= HiddenPredFlag;
  return(TRUE);
}

static Int			/* $hidden_predicate(P) */
p_hidden_predicate(void)
{
  PredEntry      *pe;

  Term t1 = Deref(ARG1);
  SMALLUNSGN mod = Yap_LookupModule(Deref(ARG2));

 restart_system_pred:
  if (IsVarTerm(t1))
    return (FALSE);
  if (IsAtomTerm(t1)) {
    pe = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(t1), mod));
  } else if (IsApplTerm(t1)) {
    Functor         funt = FunctorOfTerm(t1);
    if (IsExtensionFunctor(funt)) {
      return(FALSE);
    } 
    if (funt == FunctorModule) {
      Term nmod = ArgOfTerm(1, t1);
      if (IsVarTerm(nmod)) {
	Yap_Error(INSTANTIATION_ERROR,ARG1,"hide_predicate/1");
	return(FALSE);
      } 
      if (!IsAtomTerm(nmod)) {
	Yap_Error(TYPE_ERROR_ATOM,ARG1,"hide_predicate/1");
	return(FALSE);
      }
      t1 = ArgOfTerm(2, t1);
      goto restart_system_pred;
    }
    pe = RepPredProp(Yap_GetPredPropByFunc(funt, mod));
  } else if (IsPairTerm(t1)) {
    return (TRUE);
  } else
    return (FALSE);
  if (EndOfPAEntr(pe))
    return(FALSE);
  return(pe->PredFlags & HiddenPredFlag);
}

void 
Yap_InitCdMgr(void)
{
  Yap_InitCPred("$compile_mode", 2, p_compile_mode, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$start_consult", 3, p_startconsult, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$show_consult_level", 1, p_showconslultlev, SafePredFlag);
  Yap_InitCPred("$end_consult", 0, p_endconsult, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$set_spy", 2, p_setspy, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$rm_spy", 2, p_rmspy, SafePredFlag|SyncPredFlag);
  /* gc() may happen during compilation, hence these predicates are
	now unsafe */
  Yap_InitCPred("$compile", 3, p_compile, SyncPredFlag);
  Yap_InitCPred("$compile_dynamic", 4, p_compile_dynamic, SyncPredFlag);
  Yap_InitCPred("$purge_clauses", 2, p_purge_clauses, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$in_use", 2, p_in_use, TestPredFlag | SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$is_dynamic", 2, p_is_dynamic, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$pred_exists", 2, p_pred_exists, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$number_of_clauses", 3, p_number_of_clauses, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$undefined", 2, p_undefined, SafePredFlag|TestPredFlag);
  Yap_InitCPred("$optimizer_on", 0, p_optimizer_on, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$clean_up_dead_clauses", 0, p_clean_up_dead_clauses, SyncPredFlag);
  Yap_InitCPred("$optimizer_off", 0, p_optimizer_off, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$kill_dynamic", 2, p_kill_dynamic, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$in_this_file_before", 3, p_in_this_f_before, SafePredFlag);
  Yap_InitCPred("$first_clause_in_file", 3, p_first_cl_in_f, SafePredFlag);
  Yap_InitCPred("$mk_cl_not_first", 2, p_mk_cl_not_first, SafePredFlag);
  Yap_InitCPred("$new_multifile", 3, p_new_multifile, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$is_multifile", 2, p_is_multifile, TestPredFlag | SafePredFlag);
  Yap_InitCPred("$is_profiled", 1, p_is_profiled, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$profile_info", 3, p_profile_info, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$profile_reset", 2, p_profile_reset, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$is_call_counted", 1, p_is_call_counted, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$call_count_info", 3, p_call_count_info, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$call_count_set", 6, p_call_count_set, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$call_count_reset", 0, p_call_count_reset, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$toggle_static_predicates_in_use", 0, p_toggle_static_predicates_in_use, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$set_pred_module", 2, p_set_pred_module, SafePredFlag);
  Yap_InitCPred("$parent_pred", 3, p_parent_pred, SafePredFlag);
  Yap_InitCPred("$system_predicate", 2, p_system_pred, SafePredFlag);
  Yap_InitCPred("$hide_predicate", 2, p_hide_predicate, SafePredFlag);
  Yap_InitCPred("$hidden_predicate", 2, p_hidden_predicate, SafePredFlag);
  Yap_InitCPred("$pred_for_code", 5, p_pred_for_code, SyncPredFlag);
  Yap_InitCPred("$current_stack", 1, p_current_stack, SyncPredFlag);
}

