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
* File:		rheap.h							 *
* comments:	walk through heap code					 *
*									 *
* Last rev:     $Date: 2008-08-07 20:51:23 $,$Author: vsc $						 *
* $Log: not supported by cvs2svn $
* Revision 1.99  2008/07/22 23:34:49  vsc
* SWI and module fixes
*
* Revision 1.98  2008/05/12 22:31:37  vsc
* fix previous fixes
*
* Revision 1.97  2008/05/12 14:04:23  vsc
* updates to restore
*
* Revision 1.96  2008/04/11 16:58:17  ricroc
* yapor: seq_def initialization
*
* Revision 1.95  2008/04/06 12:06:48  vsc
* more small fixes
*
* Revision 1.94  2008/04/06 11:53:02  vsc
*  fix some restore bugs
*
* Revision 1.93  2008/04/04 09:10:02  vsc
* restore was restoring twice
*
* Revision 1.92  2008/04/03 11:34:47  vsc
* fix restorebb in cases entry key is not an atom (obs from Nicos
* Angelopoulos)
*
* Revision 1.91  2008/04/01 15:31:43  vsc
* more saved state fixes
*
* Revision 1.90  2008/04/01 14:09:43  vsc
* improve restore
*
* Revision 1.89  2008/04/01 09:41:05  vsc
* more fixes to restore
*
* Revision 1.88  2008/04/01 08:42:46  vsc
* fix restore and small VISTA thingies
*
* Revision 1.87  2008/03/25 22:03:14  vsc
* fix some icc warnings
*
* Revision 1.86  2008/03/25 16:45:53  vsc
* make or-parallelism compile again
*
* Revision 1.85  2008/02/12 17:03:52  vsc
* SWI-portability changes
*
* Revision 1.84  2008/02/07 21:39:51  vsc
* fix case where predicate is for an integer (DBEntry).
*
* Revision 1.83  2008/01/23 17:57:55  vsc
* valgrind it!
* enable atom garbage collection.
*
* Revision 1.82  2007/12/05 12:17:23  vsc
* improve JT
* fix graph compatibility with SICStus
* re-export declaration.
*
* Revision 1.81  2007/11/26 23:43:09  vsc
* fixes to support threads and assert correctly, even if inefficiently.
*
* Revision 1.80  2007/11/07 09:35:53  vsc
* small fix
*
* Revision 1.79  2007/11/07 09:25:27  vsc
* speedup meta-calls
*
* Revision 1.78  2007/11/06 17:02:12  vsc
* compile ground terms away.
*
* Revision 1.77  2007/10/10 09:44:24  vsc
* some more fixes to make YAP swi compatible
* fix absolute_file_name (again)
* fix setarg
*
* Revision 1.76  2007/09/28 23:18:17  vsc
* handle learning from interpretations.
*
* Revision 1.75  2007/04/10 22:13:21  vsc
* fix max modules limitation
*
* Revision 1.74  2007/03/22 11:12:21  vsc
* make sure that YAP_Restart does not restart a failed goal.
*
* Revision 1.73  2007/02/18 00:26:36  vsc
* fix atom garbage collector (although it is still off by default)
* make valgrind feel better
*
* Revision 1.72  2007/01/08 08:27:19  vsc
* fix restore (Trevor)
* make indexing a bit faster on IDB
*
* Revision 1.71  2006/11/27 17:42:03  vsc
* support for UNICODE, and other bug fixes.
*
* Revision 1.70  2006/08/25 19:50:35  vsc
* global data structures
*
* Revision 1.69  2006/08/22 16:12:46  vsc
* global variables
*
* Revision 1.68  2006/08/02 18:18:30  vsc
* preliminary support for readutil library (SWI compatible).
*
* Revision 1.67  2006/05/17 18:38:11  vsc
* make system library use true file name
*
* Revision 1.66  2006/04/28 15:48:33  vsc
* do locking on streams
*
* Revision 1.65  2006/04/28 13:23:23  vsc
* fix number of overflow bugs affecting threaded version
* make current_op faster.
*
* Revision 1.64  2006/03/22 20:07:28  vsc
* take better care of zombies
*
* Revision 1.63  2006/03/06 14:04:56  vsc
* fixes to garbage collector
* fixes to debugger
*
* Revision 1.62  2006/02/24 14:03:42  vsc
* fix refs to old LogUpd implementation (pre 5).
*
* Revision 1.61  2006/01/02 02:16:18  vsc
* support new interface between YAP and GMP, so that we don't rely on our own
* allocation routines.
* Several big fixes.
*
* Revision 1.60  2005/12/17 03:25:39  vsc
* major changes to support online event-based profiling
* improve error discovery and restart on scanner.
*
* Revision 1.59  2005/12/05 17:16:11  vsc
* write_depth/3
* overflow handlings and garbage collection
* Several ipdates to CLPBN
* dif/2 could be broken in the presence of attributed variables.
*
* Revision 1.58  2005/11/23 03:01:33  vsc
* fix several bugs in save/restore.b
*
* Revision 1.57  2005/10/28 17:38:50  vsc
* sveral updates
*
* Revision 1.56  2005/10/21 16:09:03  vsc
* SWI compatible module only operators
*
* Revision 1.55  2005/10/19 19:00:48  vsc
* extend arrays with nb_terms so that we can implement nb_ builtins
* correctly.
*
* Revision 1.54  2005/09/09 17:24:39  vsc
* a new and hopefully much better implementation of atts.
*
* Revision 1.53  2005/08/01 15:40:38  ricroc
* TABLING NEW: better support for incomplete tabling
*
* Revision 1.52  2005/07/06 19:34:11  ricroc
* TABLING: answers for completed calls can now be obtained by loading (new option) or executing (default) them from the trie data structure.
*
* Revision 1.51  2005/07/06 15:10:15  vsc
* improvements to compiler: merged instructions and fixes for ->
*
* Revision 1.50  2005/06/01 13:53:46  vsc
* improve bb routines to use the DB efficiently
* change interface between DB and BB.
*
* Revision 1.49  2005/05/30 03:26:37  vsc
* add some atom gc fixes
*
* Revision 1.48  2005/01/04 02:50:21  vsc
* - allow MegaClauses with blobs
* - change Diffs to be thread specific
* - include Christian's updates
*
* Revision 1.47  2004/12/02 06:06:47  vsc
* fix threads so that they at least start
* allow error handling to work with threads
* replace heap_base by Yap_heap_base, according to Yap's convention for globals.
*
* Revision 1.46  2004/11/23 21:16:21  vsc
* A few extra fixes for saved states.
*
* Revision 1.45  2004/10/26 20:16:18  vsc
* More bug fixes for overflow handling
*
* Revision 1.44  2004/10/06 16:55:47  vsc
* change configure to support big mem configs
* get rid of extra globals
* fix trouble with multifile preds
*
* Revision 1.43  2004/09/27 20:45:04  vsc
* Mega clauses
* Fixes to sizeof(expand_clauses) which was being overestimated
* Fixes to profiling+indexing
* Fixes to reallocation of memory after restoring
* Make sure all clauses, even for C, end in _Ystop
* Don't reuse space for Streams
* Fix Stream_F on StreaNo+1
*
* Revision 1.42  2004/06/05 03:37:00  vsc
* coroutining is now a part of attvars.
* some more fixes.
*
* Revision 1.41  2004/04/29 03:45:50  vsc
* fix garbage collection in execute_tail
*
* Revision 1.40  2004/03/31 01:03:10  vsc
* support expand group of clauses
*
* Revision 1.39  2004/03/19 11:35:42  vsc
* trim_trail for default machine
* be more aggressive about try-retry-trust chains.
*    - handle cases where block starts with a wait
*    - don't use _killed instructions, just let the thing rot by itself.
*									 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "@(#)rheap.c	1.3 3/15/90";
#endif

#define Atomics		0
#define Funcs		1

static Term
ConstantTermAdjust (Term t)
{
  if (IsAtomTerm(t))
    return AtomTermAdjust(t);
  return t;
}

static Term
DBGroundTermAdjust (Term t)
{
  /* The term itself is restored by dbtermlist */
  if (IsPairTerm(t)) {
    return AbsPair(PtoHeapCellAdjust(RepPair(t)));
  } else {
    return AbsAppl(PtoHeapCellAdjust(RepAppl(t)));
  }
}

/* Now, everything on its place so you must adjust the pointers */

static void 
do_clean_susp_clauses(yamop *ipc) {
  COUNT i;
  yamop **st = (yamop **)NEXTOP(ipc,sssllp);

  ipc->opc = Yap_opcode(_expand_clauses);
  ipc->u.sssllp.p = PtoPredAdjust(ipc->u.sssllp.p);
  if (ipc->u.sssllp.sprev) {
    ipc->u.sssllp.sprev = PtoOpAdjust(ipc->u.sssllp.sprev);
  }
  if (ipc->u.sssllp.snext) {
    ipc->u.sssllp.snext = PtoOpAdjust(ipc->u.sssllp.snext);
  }
  for (i = 0; i < ipc->u.sssllp.s1; i++, st++) {
    if (*st) {
      *st = PtoOpAdjust(*st);
    }
  }
}

static void
AdjustSwitchTable(op_numbers op, yamop *table, COUNT i)
{
  CELL *startcode = (CELL *)table;
  /* in case the table is already gone */
  if (!table)
    return;
  switch (op) {
  case _switch_on_func:
    {
      COUNT            j;
      CELL            *oldcode;

      oldcode = startcode;
      for (j = 0; j < i; j++) {
	Functor oldfunc = (Functor)(oldcode[0]);
	CODEADDR oldjmp = (CODEADDR)(oldcode[1]);
	if (oldfunc) {
	  oldcode[0] = (CELL)FuncAdjust(oldfunc);
	}
	oldcode[1] = (CELL)CodeAddrAdjust(oldjmp);
	oldcode += 2;
      }
      rehash(startcode, i, Funcs);
    }
    break;
  case _switch_on_cons:
    {
      COUNT            j;
      CELL            *oldcode;

#if !defined(USE_OFFSETS)
      oldcode = startcode;
#endif
      for (j = 0; j < i; j++) {
	Term oldcons = oldcode[0];
	CODEADDR oldjmp = (CODEADDR)(oldcode[1]);
	if (oldcons != 0x0 && IsAtomTerm(oldcons)) {
	  oldcode[0] = AtomTermAdjust(oldcons);
	}
	oldcode[1] = (CELL)CodeAddrAdjust(oldjmp);
	oldcode += 2;
      }
#if !USE_OFFSETS
      rehash(startcode, i, Atomics);
#endif
    }
    break;
  case _go_on_func:
    {
      Functor oldfunc = (Functor)(startcode[0]);

      startcode[0] = (CELL)FuncAdjust(oldfunc);
      startcode[1] = (CELL)CodeAddrAdjust((CODEADDR)startcode[1]);
      startcode[3] = (CELL)CodeAddrAdjust((CODEADDR)startcode[3]);
    }
    break;
  case _go_on_cons:
    {
      Term oldcons = startcode[0];

      if (IsAtomTerm(oldcons)) {
	startcode[0] = AtomTermAdjust(oldcons);
      }
      startcode[1] = (CELL)CodeAddrAdjust((CODEADDR)startcode[1]);
      startcode[3] = (CELL)CodeAddrAdjust((CODEADDR)startcode[3]);
    }
    break;
  case _if_func:
    {
      Int j;

      for (j = 0; j < i; j++) {
	Functor oldfunc = (Functor)(startcode[0]);
	CODEADDR oldjmp = (CODEADDR)(startcode[1]);
	startcode[0] = (CELL)FuncAdjust(oldfunc);
	startcode[1] = (CELL)CodeAddrAdjust(oldjmp);
	startcode += 2;
      }
      /* adjust fail code */
      startcode[1] = (CELL)CodeAddrAdjust((CODEADDR)startcode[1]);
    }
    break;
  case _if_cons:
    {
      Int j;

      for (j = 0; j < i; j++) {
	Term oldcons = startcode[0];
	CODEADDR oldjmp = (CODEADDR)(startcode[1]);
	if (IsAtomTerm(oldcons)) {
	  startcode[0] = (CELL)AtomTermAdjust(oldcons);
	}
	startcode[1] = (CELL)CodeAddrAdjust(oldjmp);
	startcode += 2;
      }
      /* adjust fail code */
      startcode[1] = (CELL)CodeAddrAdjust((CODEADDR)startcode[1]);
    }
    break;
  default:
    Yap_Error(INTERNAL_ERROR,0L,"Opcode Not Implemented in AdjustSwitchTable");
  }
}

STATIC_PROTO(void  RestoreAtomList, (Atom));
STATIC_PROTO(void  RestoreAtom, (AtomEntry *));
STATIC_PROTO(void  RestoreHashPreds, (void));

static void
RestoreAtoms(void)
{
  AtomHashEntry *HashPtr;
  register int    i;

  Yap_heap_regs->hash_chain = 
    PtoAtomHashEntryAdjust(Yap_heap_regs->hash_chain);
  HashPtr = HashChain;
  for (i = 0; i < AtomHashTableSize; ++i) {
    HashPtr->Entry = NoAGCAtomAdjust(HashPtr->Entry);
    RestoreAtomList(HashPtr->Entry);
    HashPtr++;
  }  
}

static void
RestoreWideAtoms(void)
{
  AtomHashEntry *HashPtr;
  register int    i;

  Yap_heap_regs->wide_hash_chain = 
    PtoAtomHashEntryAdjust(Yap_heap_regs->wide_hash_chain);
  HashPtr = WideHashChain;
  for (i = 0; i < WideAtomHashTableSize; ++i) {
    HashPtr->Entry = AtomAdjust(HashPtr->Entry);
    RestoreAtomList(HashPtr->Entry);
    HashPtr++;
  }  
}

static void
RestoreInvisibleAtoms(void)
{
  INVISIBLECHAIN.Entry = AtomAdjust(INVISIBLECHAIN.Entry);
  RestoreAtomList(INVISIBLECHAIN.Entry);
  RestoreAtom(RepAtom(AtomFoundVar));
  RestoreAtom(RepAtom(AtomFreeTerm));
}

#include "rclause.h"

/* adjusts terms stored in the data base, when they have no variables */
static Term 
AdjustDBTerm(Term trm, Term *p_base)
{
  if (IsVarTerm(trm))
    return CodeVarAdjust(trm);
  if (IsAtomTerm(trm))
    return AtomTermAdjust(trm);
  if (IsPairTerm(trm)) {
    Term           *p;
    Term out;

    p = PtoHeapCellAdjust(RepPair(trm));
    out = AbsPair(p);
  loop:
    if (p >= p_base) {
      p[0] = AdjustDBTerm(p[0], p);
      if (IsPairTerm(p[1])) {
	/* avoid term recursion with very deep lists */
	Term *newp = PtoHeapCellAdjust(RepPair(p[1]));
	p[1] = AbsPair(newp);
	p_base = p;
	p = newp;
	goto loop;
      } else {
	p[1] = AdjustDBTerm(p[1], p);
      }
    }
    return out;
  }
  if (IsApplTerm(trm)) {
    Term           *p;
    Functor f;
    Term *p0 = p = PtoHeapCellAdjust(RepAppl(trm));
    /* if it is before the current position, then we are looking
       at old code */
    if (p >= p_base) {
      f = (Functor)p[0];
      if (!IsExtensionFunctor(f)) {
	UInt             Arity, i;

	f = FuncAdjust(f);
	*p++ = (Term)f;
	Arity = ArityOfFunctor(f);
	for (i = 0; i < Arity; ++i) {
	  *p = AdjustDBTerm(*p, p0);
	  p++;
	}
      }
    }
    return AbsAppl(p0);
  }
  return trm;
}

static void
RestoreDBTerm(DBTerm *dbr, int attachments)
{
  if (attachments) {
#ifdef COROUTINING
    if (dbr->ag.attachments)
      dbr->ag.attachments = AdjustDBTerm(dbr->ag.attachments, dbr->Contents);
#endif
  } else {
    if (dbr->ag.NextDBT)
      dbr->ag.NextDBT = DBTermAdjust(dbr->ag.NextDBT);
  }    
  if (dbr->DBRefs !=  NULL) {
    DBRef          *cp;
    DBRef            tm;

    dbr->DBRefs = DBRefPAdjust(dbr->DBRefs);
    cp = dbr->DBRefs;
    while ((tm = *--cp) != 0)
      *cp = DBRefAdjust(tm);
  }
  dbr->Entry = AdjustDBTerm(dbr->Entry, dbr->Contents);
}

/* Restoring the heap */

/* Restores a prolog clause, in its compiled form */
static void 
RestoreStaticClause(StaticClause *cl)
/*
 * Cl points to the start of the code, IsolFlag tells if we have a single
 * clause for this predicate or not 
 */
{
  if (cl->ClFlags & FactMask) {
    cl->usc.ClPred = PtoPredAdjust(cl->usc.ClPred);
  } else {
    cl->usc.ClSource = DBTermAdjust(cl->usc.ClSource);
  }
  if (cl->ClNext) {
    cl->ClNext = PtoStCAdjust(cl->ClNext);
  }
  restore_opcodes(cl->ClCode, NULL);
}

/* Restores a prolog clause, in its compiled form */
static void 
RestoreMegaClause(MegaClause *cl)
/*
 * Cl points to the start of the code, IsolFlag tells if we have a single
 * clause for this predicate or not 
 */
{
  UInt ncls, i;
  yamop *ptr;

  cl->ClPred = PtoPredAdjust(cl->ClPred);
  if (cl->ClNext) {
    cl->ClNext = (MegaClause *)AddrAdjust((ADDR)(cl->ClNext));
  }
  ncls = cl->ClPred->cs.p_code.NOfClauses;

  for (i = 0, ptr = cl->ClCode; i < ncls; i++) {
    yamop *nextptr = (yamop *)((char *)ptr + cl->ClItemSize);
    restore_opcodes(ptr, nextptr);
    ptr = nextptr;
  }
}

/* Restores a prolog clause, in its compiled form */
static void 
RestoreDynamicClause(DynamicClause *cl, PredEntry *pp)
/*
 * Cl points to the start of the code, IsolFlag tells if we have a single
 * clause for this predicate or not 
 */
{
  if (cl->ClPrevious != NULL) {
    cl->ClPrevious = PtoOpAdjust(cl->ClPrevious);
  }
  INIT_LOCK(cl->ClLock);
  restore_opcodes(cl->ClCode, NULL);
}

/* Restores a prolog clause, in its compiled form */
static void 
RestoreLUClause(LogUpdClause *cl, PredEntry *pp)
/*
 * Cl points to the start of the code, IsolFlag tells if we have a single
 * clause for this predicate or not 
 */
{
  //  INIT_LOCK(cl->ClLock);
  if (cl->ClFlags & LogUpdRuleMask) {
    cl->ClExt = PtoOpAdjust(cl->ClExt);
  }
  if (cl->ClSource) {
    cl->ClSource = DBTermAdjust(cl->ClSource);
    RestoreDBTerm(cl->ClSource, TRUE);
  }
  if (cl->ClPrev) {
    cl->ClPrev = PtoLUCAdjust(cl->ClPrev);
  }
  if (cl->ClNext) {
    cl->ClNext = PtoLUCAdjust(cl->ClNext);
  }
  cl->ClPred = PtoPredAdjust(cl->ClPred);
  restore_opcodes(cl->ClCode, NULL);
}

static void
RestoreDBTermEntry(struct dbterm_list *dbl) {
  DBTerm *dbt;

  if (dbl->dbterms)
    dbt = dbl->dbterms = DBTermAdjust(dbl->dbterms);
  else
    return;
  dbl->clause_code = PtoOpAdjust(dbl->clause_code);
  if (dbl->next_dbl)
    dbl->next_dbl = PtoDBTLAdjust(dbl->next_dbl);
  dbl->p = PredEntryAdjust(dbl->p);
  while (dbt) {
    RestoreDBTerm(dbt, FALSE);
    dbt = dbt->ag.NextDBT;
  }
}

static void 
CleanLUIndex(LogUpdIndex *idx, int recurse)
{
  //  INIT_LOCK(idx->ClLock);
  idx->ClPred = PtoPredAdjust(idx->ClPred);
  if (idx->ParentIndex)
    idx->ParentIndex = LUIndexAdjust(idx->ParentIndex);
  if (idx->PrevSiblingIndex) {
    idx->PrevSiblingIndex = LUIndexAdjust(idx->PrevSiblingIndex);
  }
  if (idx->SiblingIndex) {
    idx->SiblingIndex = LUIndexAdjust(idx->SiblingIndex);
    if (recurse)
      CleanLUIndex(idx->SiblingIndex, TRUE);
  }
  if (idx->ChildIndex) {
    idx->ChildIndex = LUIndexAdjust(idx->ChildIndex);
    if (recurse)
      CleanLUIndex(idx->ChildIndex, TRUE);
  }
  if (!(idx->ClFlags & SwitchTableMask)) {
    restore_opcodes(idx->ClCode, NULL);
  }
}

static void 
CleanSIndex(StaticIndex *idx, int recurse)
{
 beginning:
  if (!(idx->ClFlags & SwitchTableMask)) {
    restore_opcodes(idx->ClCode, NULL);
  }
  idx->ClPred = PtoPredAdjust(idx->ClPred);
  if (idx->ChildIndex) {
    idx->ChildIndex = SIndexAdjust(idx->ChildIndex);
    if (recurse)
      CleanSIndex(idx->ChildIndex, TRUE);
  }
  if (idx->SiblingIndex) {
    idx->SiblingIndex = SIndexAdjust(idx->SiblingIndex);
    /* use loop to avoid recursion with very complex indices */
    if (recurse) {
      idx =  idx->SiblingIndex;
      goto beginning;
    }
  }
}

static void 
RestoreSWIAtoms(void)
{
  int i, j;
  for (i=0; i < N_SWI_ATOMS; i++) {
    SWI_Atoms[i] = AtomAdjust(SWI_Atoms[i]);
  }
  for (j=0; j < N_SWI_FUNCTORS; j++) {
    SWI_Functors[j] = FuncAdjust(SWI_Functors[j]);
  }
  RestoreSWIHash();
}

static void 
RestoreSWIBlobs(void)
{
}

static void
RestorePredHash(void)
{
  PredHash = PtoPtoPredAdjust(PredHash);
  if (PredHash == NULL) {
    Yap_Error(FATAL_ERROR,MkIntTerm(0),"restore should find predicate hash table");
  }
  REINIT_RWLOCK(PredHashRWLock);
  RestoreHashPreds(); /* does most of the work */
}

static void
RestoreEnvInst(yamop start[2], yamop **instp, op_numbers opc, PredEntry *pred)
{
  yamop *ipc = start;

  ipc->opc = Yap_opcode(_call);
  ipc->u.Osbpp.p = pred;
  ipc->u.Osbpp.p0 = pred;
  ipc = NEXTOP(ipc, Osbpp);
  ipc->opc = Yap_opcode(opc);
  *instp = ipc;
}

static void
RestoreOtaplInst(yamop start[1], OPCODE opc, PredEntry *pe)
{
  yamop *ipc = start;

  /* this is a place holder, it should not really be used */
  ipc->opc = Yap_opcode(opc);
  ipc->u.Otapl.s = 0;
  ipc->u.Otapl.p = pe;
  if (ipc->u.Otapl.d)
    ipc->u.Otapl.d = PtoOpAdjust(ipc->u.Otapl.d);
#ifdef YAPOR
  INIT_YAMOP_LTT(ipc, 1);
#endif /* YAPOR */
#ifdef TABLING
  ipc->u.Otapl.te = NULL;
#endif /* TABLING */
}

static void
RestoreDBTermsList(void)
{
  if (Yap_heap_regs->dbterms_list) {
    struct dbterm_list *dbl = PtoDBTLAdjust(Yap_heap_regs->dbterms_list);
    Yap_heap_regs->dbterms_list = dbl;
    while (dbl) {
      RestoreDBTermEntry(dbl);
      dbl = dbl->next_dbl;
    }
  }
}

static void
RestoreExpandList(void)
{
  if (Yap_heap_regs->expand_clauses_first)
    Yap_heap_regs->expand_clauses_first = PtoOpAdjust(Yap_heap_regs->expand_clauses_first);
  if (Yap_heap_regs->expand_clauses_last)
    Yap_heap_regs->expand_clauses_last = PtoOpAdjust(Yap_heap_regs->expand_clauses_last);
  {
    yamop *ptr = Yap_heap_regs->expand_clauses_first;
    while (ptr) {
      do_clean_susp_clauses(ptr);
      ptr = ptr->u.sssllp.snext;
    }
  }
}

static void
RestoreUdiControlBlocks(void)
{
  if (Yap_heap_regs->udi_control_blocks) {
      Yap_Error(SYSTEM_ERROR, TermNil,
	    "YAP cannot restore UDI entries!!\n");
  }
}

static void
RestoreIntKeys(void)
{
  if (Yap_heap_regs->IntKeys != NULL) {
    Yap_heap_regs->IntKeys = (Prop *)AddrAdjust((ADDR)(Yap_heap_regs->IntKeys));
    {
      UInt i;
      for (i = 0; i < Yap_heap_regs->int_keys_size; i++) {
	if (Yap_heap_regs->IntKeys[i] != NIL) {
	  Prop p0 = Yap_heap_regs->IntKeys[i] = PropAdjust(Yap_heap_regs->IntKeys[i]);
	  RestoreEntries(RepProp(p0), TRUE);
	}
      }
    }
  }
}

static void
RestoreIntLUKeys(void)
{
  if (Yap_heap_regs->IntLUKeys != NULL) {
    Yap_heap_regs->IntLUKeys = (Prop *)AddrAdjust((ADDR)(Yap_heap_regs->IntLUKeys));
    {
      Int i;
      for (i = 0; i < INT_KEYS_SIZE; i++) {
	Prop p0 = INT_LU_KEYS[i];
	if (p0) {
	  p0 = PropAdjust(p0);
	  INT_LU_KEYS[i] = p0;
	  while (p0) {
	    PredEntry *pe = RepPredProp(p0);
	    pe->NextOfPE =
	      PropAdjust(pe->NextOfPE);
	    CleanCode(pe);
	    p0 = RepProp(pe->NextOfPE);
	  }
	}
      }
    }
  }
}

static void
RestoreIntBBKeys(void)
{
  if (Yap_heap_regs->IntBBKeys != NULL) {
    Yap_heap_regs->IntBBKeys = (Prop *)AddrAdjust((ADDR)(Yap_heap_regs->IntBBKeys));
    {
      UInt i;
      for (i = 0; i < Yap_heap_regs->int_bb_keys_size; i++) {
	if (Yap_heap_regs->IntBBKeys[i] != NIL) {
	  Prop p0 = Yap_heap_regs->IntBBKeys[i] = PropAdjust(Yap_heap_regs->IntBBKeys[i]);
	  RestoreEntries(RepProp(p0), TRUE);
	}
      }
    }
  }
}

static void 
RestoreDBErasedMarker(void)
{
  Yap_heap_regs->db_erased_marker =
    DBRefAdjust(Yap_heap_regs->db_erased_marker);
  Yap_heap_regs->db_erased_marker->id = FunctorDBRef;
  Yap_heap_regs->db_erased_marker->Flags = ErasedMask;
  Yap_heap_regs->db_erased_marker->Code = NULL;
  Yap_heap_regs->db_erased_marker->DBT.DBRefs = NULL;
  Yap_heap_regs->db_erased_marker->Parent = NULL;
}

static void 
RestoreLogDBErasedMarker(void)
{
  Yap_heap_regs->logdb_erased_marker =
    PtoLUCAdjust(Yap_heap_regs->logdb_erased_marker);
  Yap_heap_regs->logdb_erased_marker->Id = FunctorDBRef;
  Yap_heap_regs->logdb_erased_marker->ClFlags = ErasedMask|LogUpdMask;
  Yap_heap_regs->logdb_erased_marker->ClSource = NULL;
  Yap_heap_regs->logdb_erased_marker->ClRefCount = 0;
  Yap_heap_regs->logdb_erased_marker->ClPred = PredLogUpdClause;
  Yap_heap_regs->logdb_erased_marker->ClExt = NULL;
  Yap_heap_regs->logdb_erased_marker->ClPrev = NULL;
  Yap_heap_regs->logdb_erased_marker->ClNext = NULL;
  Yap_heap_regs->logdb_erased_marker->ClSize = (UInt)NEXTOP(((LogUpdClause *)NULL)->ClCode,e);
  Yap_heap_regs->logdb_erased_marker->ClCode->opc = Yap_opcode(_op_fail);
  INIT_CLREF_COUNT(Yap_heap_regs->logdb_erased_marker);
}

static void 
RestoreDeadStaticClauses(void)
{
  if (Yap_heap_regs->dead_static_clauses) {
    StaticClause *sc = PtoStCAdjust(Yap_heap_regs->dead_static_clauses);
    Yap_heap_regs->dead_static_clauses = sc;
    while (sc) {
      RestoreStaticClause(sc);
      sc = sc->ClNext;
    }
  }
}

static void 
RestoreDeadMegaClauses(void)
{
  if (Yap_heap_regs->dead_mega_clauses) {
    MegaClause *mc = (MegaClause *)AddrAdjust((ADDR)(Yap_heap_regs->dead_mega_clauses));
    Yap_heap_regs->dead_mega_clauses = mc;
    while (mc) {
      RestoreMegaClause(mc);
      mc = mc->ClNext;
    }
  }
}

static void 
RestoreDeadStaticIndices(void)
{
  if (Yap_heap_regs->dead_static_indices) {
    StaticIndex *si = (StaticIndex *)AddrAdjust((ADDR)(Yap_heap_regs->dead_static_indices));
    Yap_heap_regs->dead_static_indices = si;
    while (si) {
      CleanSIndex(si, FALSE);
      si = si->SiblingIndex;
    }
  }
}

static void 
RestoreDBErasedList(void)
{
  if (Yap_heap_regs->db_erased_list) {
    LogUpdClause *lcl = Yap_heap_regs->db_erased_list = 
      PtoLUCAdjust(Yap_heap_regs->db_erased_list);
    while (lcl) {
      RestoreLUClause(lcl, FALSE);
      lcl = lcl->ClNext;
    }
  }
}

static void 
RestoreDBErasedIList(void)
{
  if (Yap_heap_regs->db_erased_ilist) {
    LogUpdIndex *icl = Yap_heap_regs->db_erased_ilist = 
      LUIndexAdjust(Yap_heap_regs->db_erased_ilist);
    while (icl) {
      CleanLUIndex(icl, FALSE);
      icl = icl->SiblingIndex;
    }
  }
}

static void
RestoreStreams(void)
{
  if (Yap_heap_regs->yap_streams != NULL) {
    int sno;

    Yap_heap_regs->yap_streams =
      (struct stream_desc *)AddrAdjust((ADDR)Yap_heap_regs->yap_streams);
    for (sno = 0; sno < MaxStreams; ++sno) {
      if (Stream[sno].status & Free_Stream_f)
	continue;
      if (Stream[sno].status & (Socket_Stream_f|Pipe_Stream_f|InMemory_Stream_f)) 
	continue;
      Stream[sno].u.file.user_name = AtomTermAdjust(Stream[sno].u.file.user_name);
      Stream[sno].u.file.name = AtomAdjust(Stream[sno].u.file.name);
    }    
  }
}

static void
RestoreAliases(void)
{
  if (Yap_heap_regs->file_aliases != NULL) {
    int i;

    Yap_heap_regs->file_aliases =
      (struct AliasDescS *)AddrAdjust((ADDR)Yap_heap_regs->file_aliases);
    for (i = 0; i < NOfFileAliases; i++)
      FileAliases[i].name = AtomAdjust(FileAliases[i].name);
  }
}

static void
RestoreForeignCode(void)
{
  ForeignObj *f_code;

  if (!ForeignCodeLoaded)
    return;
  if (ForeignCodeLoaded != NULL) 
    ForeignCodeLoaded = (void *)AddrAdjust((ADDR)ForeignCodeLoaded);
  f_code = ForeignCodeLoaded;
  while (f_code != NULL) {
    StringList objs, libs;
    if (f_code->objs != NULL)
      f_code->objs = (StringList)AddrAdjust((ADDR)f_code->objs);
    objs = f_code->objs;
    while (objs != NULL) {
      if (objs->next != NULL)
	objs->next = (StringList)AddrAdjust((ADDR)objs->next);
	objs->name = AtomAdjust(objs->name);
      objs = objs->next;
    }
    if (f_code->libs != NULL)
      f_code->libs = (StringList)AddrAdjust((ADDR)f_code->libs);
    libs = f_code->libs;
    while (libs != NULL) {
      if (libs->next != NULL)
	libs->next = (StringList)AddrAdjust((ADDR)libs->next);
      libs->name = AtomAdjust(libs->name);
      libs = libs->next;
    }
    if (f_code->f != NULL)
      f_code->f = (char *)AddrAdjust((ADDR)f_code->f);
    if (f_code->next != NULL)
      f_code->next = (ForeignObj *)AddrAdjust((ADDR)f_code->next);
    f_code = f_code->next;
  }
}

static void
RestoreYapRecords(void)
{
  struct record_list *ptr;

  Yap_Records = DBRecordAdjust(Yap_Records);
  ptr = Yap_Records;
  while (ptr) {
    ptr->next_rec = DBRecordAdjust(ptr->next_rec);
    ptr->prev_rec = DBRecordAdjust(ptr->prev_rec);
    ptr->dbrecord = DBTermAdjust(ptr->dbrecord);
    RestoreDBTerm(ptr->dbrecord, FALSE);
  }
}

static void
RestoreBallTerm(int wid)
{
  if (BallTerm) {
    BallTerm  = DBTermAdjust(BallTerm);
    RestoreDBTerm(BallTerm, TRUE);
  }
}

#include "rglobals.h"

/* restore the failcodes */
static void 
restore_codes(void)
{
  Yap_heap_regs->heap_top = AddrAdjust(OldHeapTop);
#include "rhstruct.h"
  RestoreGlobal();
#ifndef worker_id
#define worker_id 0
#endif
  RestoreWorker(worker_id);
 }


static void
RestoreDBEntry(DBRef dbr)
{
#ifdef DEBUG_RESTORE
  fprintf(stderr, "Restoring at %x", dbr);
  if (dbr->Flags & DBAtomic)
    fprintf(stderr, " an atomic term\n");
  else if (dbr->Flags & DBNoVars)
    fprintf(stderr, " with no vars\n");
  else if (dbr->Flags & DBComplex)
    fprintf(stderr, " complex term\n");
  else if (dbr->Flags & DBIsRef)
    fprintf(stderr, " a ref\n");
  else
    fprintf(stderr, " a var\n");
#endif
  RestoreDBTerm(&(dbr->DBT), TRUE);
  if (dbr->Parent) {
    dbr->Parent = (DBProp)AddrAdjust((ADDR)(dbr->Parent));
  }
  if (dbr->Code != NULL)
    dbr->Code = PtoOpAdjust(dbr->Code);
  if (dbr->Prev != NULL)
    dbr->Prev = DBRefAdjust(dbr->Prev);
  if (dbr->Next != NULL)
    dbr->Next = DBRefAdjust(dbr->Next);
#ifdef DEBUG_RESTORE2
  fprintf(stderr, "Recomputing masks\n");
#endif
  recompute_mask(dbr);
}

/* Restores a DB structure, as it was saved in the heap */
static void 
RestoreDB(DBEntry *pp)
{
  register DBRef  dbr;

  if (pp->First != NULL)
    pp->First = DBRefAdjust(pp->First);
  if (pp->Last != NULL)
    pp->Last = DBRefAdjust(pp->Last);
  if (pp->ArityOfDB)
    pp->FunctorOfDB = FuncAdjust(pp->FunctorOfDB);
  else
    pp->FunctorOfDB = (Functor) AtomAdjust((Atom)(pp->FunctorOfDB));
  if (pp->F0 != NULL)
    pp->F0 = DBRefAdjust(pp->F0);
  if (pp->L0 != NULL)
    pp->L0 = DBRefAdjust(pp->L0);
  /* immediate update semantics */
  dbr = pp->F0;
  /* While we have something in the data base, even if erased, restore it */
  while (dbr) {
    RestoreDBEntry(dbr);
    if (dbr->n != NULL)
      dbr->n = DBRefAdjust(dbr->n);
    if (dbr->p != NULL)
      dbr->p = DBRefAdjust(dbr->p);
    dbr = dbr->n;
  }
}

/*
 * Restores a group of clauses for the same predicate, starting with First
 * and ending with Last, First may be equal to Last 
 */
static void 
CleanClauses(yamop *First, yamop *Last, PredEntry *pp)
{
  if (pp->PredFlags & LogUpdatePredFlag) {
    LogUpdClause *cl = ClauseCodeToLogUpdClause(First);

    while (cl != NULL) {
      RestoreLUClause(cl, pp);
      cl = cl->ClNext;
    }
  } else if (pp->PredFlags & MegaClausePredFlag) {
    MegaClause *cl = ClauseCodeToMegaClause(First);

    RestoreMegaClause(cl);
  } else if (pp->PredFlags & DynamicPredFlag) {
    yamop *cl = First;

    do {
      RestoreDynamicClause(ClauseCodeToDynamicClause(cl), pp);
      if (cl == Last) return;
      cl = NextDynamicClause(cl);
    } while (TRUE);
  } else {
    StaticClause *cl = ClauseCodeToStaticClause(First);

    do {
      RestoreStaticClause(cl);
      if (cl->ClCode == Last) return;
      cl = cl->ClNext;
    } while (TRUE);
  }
}



/* Restores a DB structure, as it was saved in the heap */
static void 
RestoreBB(BlackBoardEntry *pp, int int_key)
{
  Term t = pp->Element;
  if (t) {
    if (!IsVarTerm(t)) {
      if (IsAtomicTerm(t)) {
	if (IsAtomTerm(t)) {
	  pp->Element = AtomTermAdjust(t);
	}
      } else {
	RestoreLUClause((LogUpdClause *)DBRefOfTerm(t),NULL);
      }
    }
  }
  if (!int_key) {
    pp->KeyOfBB = AtomAdjust(pp->KeyOfBB);
  }
  if (pp->ModuleOfBB) {
    pp->ModuleOfBB = AtomTermAdjust(pp->ModuleOfBB);
  }
}

static void
restore_static_array(StaticArrayEntry *ae)
{
  Int sz = -ae->ArrayEArity;
  switch (ae->ArrayType) {
  case array_of_ints:
  case array_of_doubles:
  case array_of_chars:
  case array_of_uchars:
  return;
  case array_of_ptrs:
    {
      AtomEntry **base = (AtomEntry **)AddrAdjust((ADDR)(ae->ValueOfVE.ptrs));
      Int i;
      ae->ValueOfVE.ptrs = base;
      if (ae != NULL) {
	for (i=0; i<sz; i++) {
	  AtomEntry *reg = *base;
	  if (reg == NULL) {
	    base++;
	  } else if (IsOldCode((CELL)reg)) {
	    *base++ = AtomEntryAdjust(reg);
	  } else if (IsOldLocalInTR((CELL)reg)) {
	    *base++ = (AtomEntry *)LocalAddrAdjust((ADDR)reg);
	  } else if (IsOldGlobal((CELL)reg)) {
	    *base++ = (AtomEntry *)GlobalAddrAdjust((ADDR)reg);
	  } else if (IsOldTrail((CELL)reg)) {
	    *base++ = (AtomEntry *)TrailAddrAdjust((ADDR)reg);
	  } else {
	    /* oops */
	    base++;
	  }
	}
      }
    }
  return;
  case array_of_atoms:  
    {
      Term *base = (Term *)AddrAdjust((ADDR)(ae->ValueOfVE.atoms));
      Int i;
      ae->ValueOfVE.atoms = base;
      if (ae != 0L) {
	for (i=0; i<sz; i++) {
	  Term reg = *base;
	  if (reg == 0L) {
	    base++;
	  } else {
	    *base++ = AtomTermAdjust(reg);
	  }
	}
      }
    }
  return;
  case array_of_dbrefs:
    {
      Term *base = (Term *)AddrAdjust((ADDR)(ae->ValueOfVE.dbrefs));
      Int i;

      ae->ValueOfVE.dbrefs = base;
      if (ae != 0L) {
	for (i=0; i<sz; i++) {
	  Term reg = *base;
	  if (reg == 0L) {
	    base++;
	  } else {
	    *base++ = AbsAppl(PtoHeapCellAdjust(RepAppl(reg)));
	  }
	}
      }
    }
  return;
  case array_of_nb_terms:
    {
      live_term *base = (live_term *)AddrAdjust((ADDR)(ae->ValueOfVE.lterms));
      Int i;

      ae->ValueOfVE.lterms = base;
      if (ae != 0L) {
	for (i=0; i < sz; i++,base++) {
	  Term reg = base->tlive;
	  if (IsVarTerm(reg)) {
	    CELL *var = (CELL *)reg;

	    if (IsOldGlobalPtr(var)) {
	      base->tlive = (CELL)PtoGloAdjust(var);
	    } else {
	      base->tlive = (CELL)PtoHeapCellAdjust(var);
	    }
	  } else if (IsAtomTerm(reg)) {
	    base->tlive = AtomTermAdjust(reg);
	  } else if (IsApplTerm(reg)) {
	    CELL *db = RepAppl(reg);
	    db = PtoGloAdjust(db);
	    base->tlive = AbsAppl(db);
	  } else if (IsApplTerm(reg)) {
	    CELL *db = RepPair(reg);
	    db = PtoGloAdjust(db);
	    base->tlive = AbsPair(db);
	  }

	  reg = base->tstore;
	  if (IsVarTerm(reg)) {
	    base->tstore = (Term)GlobalAddrAdjust((ADDR)reg);
	  } else if (IsAtomTerm(reg)) {
	    base->tstore = AtomTermAdjust(reg);
	  } else {
	    DBTerm *db = (DBTerm *)RepAppl(reg);
	    db = DBTermAdjust(db);
	    RestoreDBTerm(db, TRUE);
	    base->tstore = AbsAppl((CELL *)db);
	  }
	}
      }
    }
  case array_of_terms:
    {
      DBTerm **base = (DBTerm **)AddrAdjust((ADDR)(ae->ValueOfVE.terms));
      Int i;

      ae->ValueOfVE.terms = base;
      if (ae != 0L) {
	for (i=0; i<sz; i++) {
	  DBTerm *reg = *base;
	  if (reg == NULL) {
	    base++;
	  } else {
	    *base++ = reg = DBTermAdjust(reg);
	    RestoreDBTerm(reg, TRUE);
	  }
	}
      }
    }
  return;
  }
}

/*
 * Clean all the code for a particular predicate, this can get a bit tricky,
 * because of the indexing code 
 */
static void 
CleanCode(PredEntry *pp)
{
  CELL            flag;


  /* Init takes care of the first 2 cases */
  if (pp->ModuleOfPred) {
    pp->ModuleOfPred = AtomTermAdjust(pp->ModuleOfPred);
  }
  if (pp->ArityOfPE) {
    if (pp->ModuleOfPred == IDB_MODULE) {
      if (pp->PredFlags & NumberDBPredFlag) {
	/* it's an integer, do nothing */
      } else if (pp->PredFlags & AtomDBPredFlag) {
	pp->FunctorOfPred = (Functor)AtomAdjust((Atom)(pp->FunctorOfPred));
      } else {
	pp->FunctorOfPred = FuncAdjust(pp->FunctorOfPred);
      }
    } else {
      pp->FunctorOfPred = FuncAdjust(pp->FunctorOfPred);
    }
  } else {
    pp->FunctorOfPred = (Functor)AtomAdjust((Atom)(pp->FunctorOfPred));
  }
  if (!(pp->PredFlags & NumberDBPredFlag)) {
    if (pp->PredFlags & MultiFileFlag) {
      if (pp->src.file_srcs)
	pp->src.file_srcs = MFileAdjust(pp->src.file_srcs);
    } else {
      if (pp->src.OwnerFile)
	pp->src.OwnerFile = AtomAdjust(pp->src.OwnerFile);
    }
  }
  pp->OpcodeOfPred = Yap_opcode(Yap_op_from_opcode(pp->OpcodeOfPred));
  if (pp->NextPredOfModule) {
    pp->NextPredOfModule = PtoPredAdjust(pp->NextPredOfModule);
  }
  if (pp->PredFlags & (AsmPredFlag|CPredFlag)) {
    /* assembly */
    if (pp->CodeOfPred) {
      pp->CodeOfPred = PtoOpAdjust(pp->CodeOfPred);
      CleanClauses(pp->CodeOfPred, pp->CodeOfPred, pp);
    }
  } else {
    yamop        *FirstC, *LastC;
    /* Prolog code */
    if (pp->cs.p_code.FirstClause)
      pp->cs.p_code.FirstClause = PtoOpAdjust(pp->cs.p_code.FirstClause);
    if (pp->cs.p_code.LastClause)
      pp->cs.p_code.LastClause = PtoOpAdjust(pp->cs.p_code.LastClause);
    pp->CodeOfPred =PtoOpAdjust(pp->CodeOfPred);
    pp->cs.p_code.TrueCodeOfPred = PtoOpAdjust(pp->cs.p_code.TrueCodeOfPred);
    pp->cs.p_code.ExpandCode = Yap_opcode(_expand_index);
    flag = pp->PredFlags;
    FirstC = pp->cs.p_code.FirstClause;
    LastC = pp->cs.p_code.LastClause;
    /* We just have a fail here */
    if (FirstC == NULL && LastC == NULL) {
      return;
    }
#ifdef	DEBUG_RESTORE2
    fprintf(stderr, "at %ux Correcting clauses from %p to %p\n", *(OPCODE *) FirstC, FirstC, LastC);
#endif
    CleanClauses(FirstC, LastC, pp);
    if (flag & IndexedPredFlag) {
#ifdef	DEBUG_RESTORE2
      fprintf(stderr, "Correcting indexed code\n");
#endif
      if (flag & LogUpdatePredFlag) {
	CleanLUIndex(ClauseCodeToLogUpdIndex(pp->cs.p_code.TrueCodeOfPred), TRUE);
      } else {
	CleanSIndex(ClauseCodeToStaticIndex(pp->cs.p_code.TrueCodeOfPred), TRUE);
      } 
    } else if (flag & DynamicPredFlag) {
#ifdef	DEBUG_RESTORE2
      fprintf(stderr, "Correcting dynamic code\n");
#endif
      RestoreDynamicClause(ClauseCodeToDynamicClause(pp->cs.p_code.TrueCodeOfPred),pp);
    }
  }
  /* we are pointing at ourselves */
}

/*
 * Restores all of the entries, for a particular atom, we only have problems
 * if we find code or data bases 
 */
static void 
RestoreEntries(PropEntry *pp, int int_key)
{
  while (!EndOfPAEntr(pp)) {
    switch(pp->KindOfPE) {
    case FunctorProperty:
      {
	FunctorEntry *fe = (FunctorEntry *)pp;
	Prop p0;
	fe->NextOfPE =
	  PropAdjust(fe->NextOfPE);
	fe->NameOfFE =
	  AtomAdjust(fe->NameOfFE);
	p0 = fe->PropsOfFE =
	  PropAdjust(fe->PropsOfFE);
	if (!EndOfPAEntr(p0)) {
	  /* at most one property */
	  CleanCode(RepPredProp(p0));
	  RepPredProp(p0)->NextOfPE =
	    PropAdjust(RepPredProp(p0)->NextOfPE);
	  p0 = RepPredProp(p0)->NextOfPE;
	}
      }
      break;
    case ValProperty:
      {
	ValEntry *ve = (ValEntry *)pp;
	Term      tv = ve->ValueOfVE;
	ve->NextOfPE =
	  PropAdjust(ve->NextOfPE);
	if (IsAtomTerm(tv))
	  ve->ValueOfVE = AtomTermAdjust(tv);
      }
      break;
    case HoldProperty:
      {
	HoldEntry *he = (HoldEntry *)pp;
	he->NextOfPE =
	  PropAdjust(he->NextOfPE);
      }
      break;
    case ArrayProperty:
      {
	ArrayEntry *ae = (ArrayEntry *)pp;
	ae->NextOfPE =
	  PropAdjust(ae->NextOfPE);
	if (ae->ArrayEArity < 0) {
	  /* static array entry */
	  StaticArrayEntry *sae = (StaticArrayEntry *)ae;
	  if (sae->NextAE)
	    sae->NextAE = PtoArraySAdjust(sae->NextAE);
	  restore_static_array(sae);
	} else {
	  if (ae->NextAE)
	    ae->NextAE = PtoArrayEAdjust(ae->NextAE);
	  if (IsVarTerm(ae->ValueOfVE))
	    RESET_VARIABLE(&(ae->ValueOfVE));
	  else {
	    CELL *ptr = RepAppl(ae->ValueOfVE);
	    /* in fact it should just be a pointer to the global,
	       but we'll be conservative.
	       Notice that the variable should have been reset in restore_program mode.
	    */
	    if (IsOldGlobalPtr(ptr)) {
	      ae->ValueOfVE = AbsAppl(PtoGloAdjust(ptr));
	    } else if (IsOldCodeCellPtr(ptr)) {
	      ae->ValueOfVE = AbsAppl(PtoHeapCellAdjust(ptr));
	    } else if (IsOldLocalInTRPtr(ptr)) {
	      ae->ValueOfVE = AbsAppl(PtoLocAdjust(ptr));
	    } else if (IsOldTrailPtr(ptr)) {
	      ae->ValueOfVE = AbsAppl(CellPtoTRAdjust(ptr));
	    }
	  }
	}
      }
      break;
    case PEProp:
      {
	PredEntry *pe = (PredEntry *) pp;
	pe->NextOfPE =
	  PropAdjust(pe->NextOfPE);
	CleanCode(pe);
      }
      break;
    case DBProperty:
    case CodeDBProperty:
#ifdef DEBUG_RESTORE2
      fprintf(stderr, "Correcting data base clause at %p\n", pp);
#endif
      {
	DBEntry *de = (DBEntry *) pp;
	de->NextOfPE =
	  PropAdjust(de->NextOfPE);
	RestoreDB(de);
      }
      break;
    case BBProperty:
      {
	BlackBoardEntry *bb = (BlackBoardEntry *) pp;
	bb->NextOfPE =
	  PropAdjust(bb->NextOfPE);
	RestoreBB(bb, int_key);
      }
      break;
    case GlobalProperty:
      {
	GlobalEntry *gb = (GlobalEntry *) pp;
	Term gbt = gb->global;

	gb->NextOfPE =
	  PropAdjust(gb->NextOfPE);
	gb->AtomOfGE =
	  AtomEntryAdjust(gb->AtomOfGE);
	if (gb->NextGE) {
	  gb->NextGE =
	    GlobalEntryAdjust(gb->NextGE);
	}
	if (IsVarTerm(gbt)) {
	  CELL *gbp = VarOfTerm(gbt);
	  if (IsOldGlobalPtr(gbp))
	    gbp = PtoGloAdjust(gbp);
	  else
	    gbp = CellPtoHeapAdjust(gbp);
	  gb->global = (CELL)gbp;
	} else if (IsPairTerm(gbt)) {
	  gb->global = AbsPair(PtoGloAdjust(RepPair(gbt)));
	} else if (IsApplTerm(gbt)) {
	  CELL *gbp = RepAppl(gbt);
	  if (IsOldGlobalPtr(gbp))
	    gbp = PtoGloAdjust(gbp);
	  else
	    gbp = CellPtoHeapAdjust(gbp);
	  gb->global = AbsAppl(gbp);
	} else if (IsAtomTerm(gbt)) {
	  gb->global = AtomTermAdjust(gbt);
	} /* numbers need no adjusting */
      }
      break;
    case OpProperty:
      {
	OpEntry *opp = (OpEntry *)pp;
	if (opp->NextOfPE) {
	  opp->NextOfPE =
	    PropAdjust(opp->NextOfPE);
	}
	opp->OpName =
	  AtomAdjust(opp->OpName);
	if (opp->OpModule) {
	 opp->OpModule = AtomTermAdjust(opp->OpModule);
	}
	if (opp->OpNext) {
	 opp->OpNext = OpEntryAdjust(opp->OpNext);
	}
      }
      break;
    case ModProperty:
      {
	ModEntry *me = (ModEntry *)pp;
	if (me->NextOfPE) {
	  me->NextOfPE =
	    PropAdjust(me->NextOfPE);
	}
	if (me->PredForME) {
	  me->PredForME =
	  PtoPredAdjust(me->PredForME);
	}
	me->AtomOfME =
	  AtomAdjust(me->AtomOfME);
	if (me->NextME)
	  me->NextME = (struct mod_entry *)
	    AddrAdjust((ADDR)me->NextME);
      }
      break;      
    case ExpProperty:
      pp->NextOfPE =
	PropAdjust(pp->NextOfPE);
      break;
    case WideAtomProperty:
      pp->NextOfPE =
	PropAdjust(pp->NextOfPE);
      break;
    default:
      /* OOPS */
      Yap_Error(SYSTEM_ERROR, TermNil,
	    "Invalid Atom Property %d at %p", pp->KindOfPE, pp);
      return;
    }
    pp = RepProp(pp->NextOfPE);
  }
}

static void
RestoreAtom(AtomEntry *at)
{
  AtomEntry *nat;

  /* this should be done before testing for wide atoms */
  at->PropsOfAE = PropAdjust(at->PropsOfAE);
#if DEBUG_RESTORE2			/* useful during debug */
  if (IsWideAtom(AbsAtom(at)))
    fprintf(errout, "Restoring %S\n", at->WStrOfAE);
  else
    fprintf(errout, "Restoring %s\n", at->StrOfAE);
#endif
  RestoreEntries(RepProp(at->PropsOfAE), FALSE);
  /* cannot use AtomAdjust without breaking agc */
  nat = RepAtom(at->NextOfAE);
  if (nat)
    at->NextOfAE = AbsAtom(AtomEntryAdjust(nat));
}

