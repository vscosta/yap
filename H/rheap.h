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
 * File:		rheap.h *
 * comments:	walk through heap code					 *
 *									 *
 * Last rev:     $Date: 2008-08-07 20:51:23 $,$Author: vsc $
 **
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
 * TABLING: answers for completed calls can now be obtained by loading (new
 *option) or executing (default) them from the trie data structure.
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
 * replace heap_base by Yap_heap_base, according to Yap's convention for
 *globals.
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
static char SccsId[] = "@(#)rheap.c	1.3 3/15/90";
#endif

#ifndef RHEAP_H
#define RHEAP_H 1

#include "YapHeap.h"
#include "absmi.h"
#include "clause.h"

#define Atomics 0
#define Funcs 1

#define ConstantTermAdjust(P) ConstantTermAdjust__(P PASS_REGS)
#define DBGroundTermAdjust(P) DBGroundTermAdjust__(P PASS_REGS)
#define AdjustDBTerm(P, A, B, C) AdjustDBTerm__(P, A, B, C PASS_REGS)
#define AdjustSwitchTable(op, table, i)                                        \
  AdjustSwitchTable__(op, table, i PASS_REGS)
#define RestoreOtaplInst(start, opc, pe)                                       \
  RestoreOtaplInst__(start, opc, pe PASS_REGS)
#define RestoreDBErasedMarker() RestoreDBErasedMarker__(PASS_REGS1)
#define RestoreLogDBErasedMarker() RestoreLogDBErasedMarker__(PASS_REGS1)
#define RestoreForeignCode() RestoreForeignCode__(PASS_REGS1)
#define RestoreEmptyWakeups() RestoreEmptyWakeups__(PASS_REGS1)
#define RestoreAtoms() RestoreAtoms__(PASS_REGS1)
#define RestoreWideAtoms() RestoreWideAtoms__(PASS_REGS1)
#define RestoreSWIBlobs() RestoreSWIBlobs__(PASS_REGS1)
#define RestoreSWIBlobTypes() RestoreSWIBlobTypes__(PASS_REGS1)
#define RestoreInvisibleAtoms() RestoreInvisibleAtoms__(PASS_REGS1)
#define RestorePredHash() RestorePredHash__(PASS_REGS1)
#define RestoreHiddenPredicates() RestoreHiddenPredicates__(PASS_REGS1)
#define RestoreDBTermsList() RestoreDBTermsList__(PASS_REGS1)
#define RestoreExpandList() RestoreExpandList__(PASS_REGS1)
#define RestoreIntKeys() RestoreIntKeys__(PASS_REGS1)
#define RestoreIntLUKeys() RestoreIntLUKeys__(PASS_REGS1)
#define RestoreIntBBKeys() RestoreIntBBKeys__(PASS_REGS1)
#define RestoreDeadStaticClauses() RestoreDeadStaticClauses__(PASS_REGS1)
#define RestoreDeadMegaClauses() RestoreDeadMegaClauses__(PASS_REGS1)
#define RestoreDeadStaticIndices() RestoreDeadStaticIndices__(PASS_REGS1)
#define RestoreDBErasedList() RestoreDBErasedList__(PASS_REGS1)
#define RestoreDBErasedIList() RestoreDBErasedIList__(PASS_REGS1)
#define RestoreYapRecords() RestoreYapRecords__(PASS_REGS1)
static Term ConstantTermAdjust__(Term t USES_REGS) {
  if (IsAtomTerm(t))
    return AtomTermAdjust(t);
  return t;
}

static Term DBGroundTermAdjust__(Term t USES_REGS) {
  /* The term itself is restored by dbtermlist */
  if (IsPairTerm(t)) {
    return AbsPair(PtoHeapCellAdjust(RepPair(t)));
  } else {
    return AbsAppl(PtoHeapCellAdjust(RepAppl(t)));
  }
}

/* Now, everything on its place so you must adjust the pointers */

static void do_clean_susp_clauses(yamop *ipc USES_REGS) {
  COUNT i;
  yamop **st = (yamop **)NEXTOP(ipc, sssllp);

  ipc->opc = Yap_opcode(_expand_clauses);
  ipc->y_u.sssllp.p = PtoPredAdjust(ipc->y_u.sssllp.p);
  if (ipc->y_u.sssllp.sprev) {
    ipc->y_u.sssllp.sprev = PtoOpAdjust(ipc->y_u.sssllp.sprev);
  }
  if (ipc->y_u.sssllp.snext) {
    ipc->y_u.sssllp.snext = PtoOpAdjust(ipc->y_u.sssllp.snext);
  }
  for (i = 0; i < ipc->y_u.sssllp.s1; i++, st++) {
    if (*st) {
      *st = PtoOpAdjust(*st);
    }
  }
}

static void AdjustSwitchTable__(op_numbers op, yamop *table,
                                COUNT i USES_REGS) {
  CELL *startcode = (CELL *)table;
  /* in case the table is already gone */
  if (!table)
    return;
  switch (op) {
  case _switch_on_func: {
    COUNT j;
    CELL *oldcode;

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
    rehash(startcode, i, Funcs PASS_REGS);
  } break;
  case _switch_on_cons: {
    COUNT j;
    CELL *oldcode;

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
    rehash(startcode, i, Atomics PASS_REGS);
#endif
  } break;
  case _go_on_func: {
    Functor oldfunc = (Functor)(startcode[0]);

    startcode[0] = (CELL)FuncAdjust(oldfunc);
    startcode[1] = (CELL)CodeAddrAdjust((CODEADDR)startcode[1]);
    startcode[3] = (CELL)CodeAddrAdjust((CODEADDR)startcode[3]);
  } break;
  case _go_on_cons: {
    Term oldcons = startcode[0];

    if (IsAtomTerm(oldcons)) {
      startcode[0] = AtomTermAdjust(oldcons);
    }
    startcode[1] = (CELL)CodeAddrAdjust((CODEADDR)startcode[1]);
    startcode[3] = (CELL)CodeAddrAdjust((CODEADDR)startcode[3]);
  } break;
  case _if_func: {
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
  } break;
  case _if_cons: {
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
  } break;
  default:
    Yap_Error(SYSTEM_ERROR_INTERNAL, 0L,
              "Opcode Not Implemented in AdjustSwitchTable");
  }
}

static void RestoreAtomList(Atom CACHE_TYPE);
static void RestoreAtom(AtomEntry *CACHE_TYPE);
static void RestoreHashPreds(CACHE_TYPE1);

static void RestoreAtoms__(USES_REGS1) {
  AtomHashEntry *HashPtr;
  register int i;

  HashChain = PtoAtomHashEntryAdjust(HashChain);
  HashPtr = HashChain;
  for (i = 0; i < AtomHashTableSize; ++i) {
    HashPtr->Entry = NoAGCAtomAdjust(HashPtr->Entry);
    RestoreAtomList(HashPtr->Entry PASS_REGS);
    HashPtr++;
  }
}

static void RestoreWideAtoms__(USES_REGS1) {
  AtomHashEntry *HashPtr;
  register int i;

  WideHashChain = PtoAtomHashEntryAdjust(WideHashChain);
  HashPtr = WideHashChain;
  for (i = 0; i < WideAtomHashTableSize; ++i) {
    HashPtr->Entry = AtomAdjust(HashPtr->Entry);
    RestoreAtomList(HashPtr->Entry PASS_REGS);
    HashPtr++;
  }
}

static void RestoreInvisibleAtoms__(USES_REGS1) {
  INVISIBLECHAIN.Entry = AtomAdjust(INVISIBLECHAIN.Entry);
  RestoreAtomList(INVISIBLECHAIN.Entry PASS_REGS);
  RestoreAtom(RepAtom(AtomFoundVar) PASS_REGS);
  RestoreAtom(RepAtom(AtomFreeTerm) PASS_REGS);
}

#include "rclause.h"

/* adjusts terms stored in the data base, when they have no variables */
static Term AdjustDBTerm__(Term trm, Term *p_base, Term *p_lim,
                           Term *p_max USES_REGS) {
  if (IsVarTerm(trm))
    return CodeVarAdjust(trm);
  if (IsAtomTerm(trm))
    return AtomTermAdjust(trm);
  if (IsPairTerm(trm)) {
    Term *p;
    Term out;

    p = PtoHeapCellAdjust(RepPair(trm));
    out = AbsPair(p);
  loop:
    if (p >= p_base || p < p_lim) {
      p[0] = AdjustDBTerm(p[0], p, p_lim, p_max);
      if (IsPairTerm(p[1])) {
        /* avoid term recursion with very deep lists */
        Term *newp = PtoHeapCellAdjust(RepPair(p[1]));
        p[1] = AbsPair(newp);
        p_base = p;
        p = newp;
        goto loop;
      } else {
        p[1] = AdjustDBTerm(p[1], p, p_lim, p_max);
      }
    }
    return out;
  }
  if (IsApplTerm(trm)) {
    Term *p;
    Functor f;
    Term *p0 = p = PtoHeapCellAdjust(RepAppl(trm));
    /* if it is before the current position, then we are looking
       at old code */
    if (p >= p_base || p < p_lim) {
      if (p >= p_max || p < p_lim) {
        if (DBRefOfTerm(trm) != DBRefAdjust(DBRefOfTerm(trm), FALSE))
          /* external term pointer, has to be a DBRef */
          return MkDBRefTerm(DBRefAdjust(DBRefOfTerm(trm), FALSE));
      }
      f = (Functor)p[0];
      if (!IsExtensionFunctor(f)) {
        UInt Arity, i;

        f = FuncAdjust(f);
        *p++ = (Term)f;
        Arity = ArityOfFunctor(f);
        for (i = 0; i < Arity; ++i) {
          *p = AdjustDBTerm(*p, p0, p_lim, p_max);
          p++;
        }
      } else if (f == FunctorDBRef) {
      }
    }
    return AbsAppl(p0);
  }
  return trm;
}

static void RestoreDBTerm(DBTerm *dbr, bool src, int attachments USES_REGS) {
  if (attachments) {
#ifdef COROUTINING
    if (attachments == 1 && dbr->ag.attachments)
      dbr->ag.attachments =
          AdjustDBTerm(dbr->ag.attachments, dbr->Contents, dbr->Contents,
                       dbr->Contents + dbr->NOfCells);
#endif
  } else {
    if (dbr->ag.NextDBT)
      dbr->ag.NextDBT = DBTermAdjust(dbr->ag.NextDBT);
  }
  if (dbr->DBRefs) {
    DBRef *cp;
    DBRef tm;

    dbr->DBRefs = DBRefPAdjust(dbr->DBRefs);
    cp = dbr->DBRefs;
    while ((tm = *--cp) != 0) {
      *cp = DBRefAdjust(tm, TRUE);
    }
  }
  dbr->Entry = AdjustDBTerm(dbr->Entry, dbr->Contents, dbr->Contents,
                            dbr->Contents + dbr->NOfCells);
}

/* Restoring the heap */

static void RestoreEmptyWakeups__(USES_REGS1) {
  int i;
  for (i = 0; i < MaxEmptyWakeups; i++) {
    EmptyWakeups[i] = AtomAdjust(EmptyWakeups[i]);
  }
}

/* Restores a prolog clause, in its compiled form */
static void RestoreStaticClause(StaticClause *cl USES_REGS)
/*
 * Cl points to the start of the code, IsolFlag tells if we have a single
 * clause for this predicate or not
 */
{
  if (cl->ClFlags & SrcMask && !(cl->ClFlags & FactMask)) {
    cl->usc.ClSource = DBTermAdjust(cl->usc.ClSource);
    RestoreDBTerm(cl->usc.ClSource, true, 2 PASS_REGS);
  }
  if (cl->ClNext) {
    cl->ClNext = PtoStCAdjust(cl->ClNext);
  }
  restore_opcodes(cl->ClCode, NULL PASS_REGS);
}

/* Restores a prolog clause, in its compiled form */
static void RestoreMegaClause(MegaClause *cl USES_REGS)
/*
 * Cl points to the start of the code, IsolFlag tells if we have a single
 * clause for this predicate or not
 */
{
  yamop *ptr, *max, *nextptr;

  cl->ClPred = PtoPredAdjust(cl->ClPred);
  if (cl->ClNext) {
    cl->ClNext = (MegaClause *)AddrAdjust((ADDR)(cl->ClNext));
  }
  max = (yamop *)((CODEADDR)cl + cl->ClSize);

  if (cl->ClFlags & ExoMask) {
    CELL *base = (CELL *)((ADDR)cl->ClCode + 2 * sizeof(struct index_t *));
    CELL *end = (CELL *)max, *ptr;

    for (ptr = base; ptr < end; ptr++) {
      Term t = *ptr;
      if (IsAtomTerm(t))
        *ptr = AtomTermAdjust(t);
      /* don't handle other complex terms just yet, ints are ok  */
    }
  } else {
    for (ptr = cl->ClCode; ptr < max;) {
      nextptr = (yamop *)((char *)ptr + cl->ClItemSize);
      restore_opcodes(ptr, nextptr PASS_REGS);
      ptr = nextptr;
    }
  }
}

/* Restores a prolog clause, in its compiled form */
static void RestoreDynamicClause(DynamicClause *cl, PredEntry *pp USES_REGS)
/*
 * Cl points to the start of the code, IsolFlag tells if we have a single
 * clause for this predicate or not
 */
{
  if (cl->ClPrevious != NULL) {
    cl->ClPrevious = PtoOpAdjust(cl->ClPrevious);
  }
  INIT_LOCK(cl->ClLock);
  restore_opcodes(cl->ClCode, NULL PASS_REGS);
}

/* Restores a prolog clause, in its compiled form */
static void RestoreLUClause(LogUpdClause *cl, PredEntry *pp USES_REGS)
/*
 * Cl points to the start of the code, IsolFlag tells if we have a single
 * clause for this predicate or not
 */
{
  //  INIT_LOCK(cl->ClLock);
  if (cl->ClFlags & LogUpdRuleMask) {
    cl->ClExt = PtoOpAdjust(cl->ClExt);
  }
  if (!(cl->ClFlags & FactMask)) {
    cl->lusl.ClSource = DBTermAdjust(cl->lusl.ClSource);
    RestoreDBTerm(cl->lusl.ClSource, true, 2 PASS_REGS);
  }
  if (cl->ClPrev) {
    cl->ClPrev = PtoLUCAdjust(cl->ClPrev);
  }
  if (cl->ClNext) {
    cl->ClNext = PtoLUCAdjust(cl->ClNext);
  }
  cl->ClPred = PtoPredAdjust(cl->ClPred);
  restore_opcodes(cl->ClCode, NULL PASS_REGS);
}

static void RestoreDBTermEntry(struct dbterm_list *dbl USES_REGS) {
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
    RestoreDBTerm(dbt, false, 0 PASS_REGS);
    dbt = dbt->ag.NextDBT;
  }
}

static void CleanLUIndex(LogUpdIndex *idx, int recurse USES_REGS) {
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
      CleanLUIndex(idx->SiblingIndex, TRUE PASS_REGS);
  }
  if (idx->ChildIndex) {
    idx->ChildIndex = LUIndexAdjust(idx->ChildIndex);
    if (recurse)
      CleanLUIndex(idx->ChildIndex, TRUE PASS_REGS);
  }
  if (!(idx->ClFlags & SwitchTableMask)) {
    restore_opcodes(idx->ClCode, NULL PASS_REGS);
  }
}

static void CleanSIndex(StaticIndex *idx, int recurse USES_REGS) {
beginning:
  if (!(idx->ClFlags & SwitchTableMask)) {
    restore_opcodes(idx->ClCode, NULL PASS_REGS);
  }
  idx->ClPred = PtoPredAdjust(idx->ClPred);
  if (idx->ChildIndex) {
    idx->ChildIndex = SIndexAdjust(idx->ChildIndex);
    if (recurse)
      CleanSIndex(idx->ChildIndex, TRUE PASS_REGS);
  }
  if (idx->SiblingIndex) {
    idx->SiblingIndex = SIndexAdjust(idx->SiblingIndex);
    /* use loop to avoid recursion with very complex indices */
    if (recurse) {
      idx = idx->SiblingIndex;
      goto beginning;
    }
  }
}

#define RestoreBlobTypes() RestoreBlobTypes__(PASS_REGS1)
#define RestoreBlobs() RestoreBlobs__(PASS_REGS1);

static void RestoreBlobTypes__(USES_REGS1) {}

static void RestoreBlobs__(USES_REGS1) {
  Blobs = AtomAdjust(Blobs);
  RestoreAtomList(Blobs PASS_REGS);
}

static void RestoreHiddenPredicates__(USES_REGS1) {
  HIDDEN_PREDICATES = PropAdjust(HIDDEN_PREDICATES);
  RestoreEntries(HIDDEN_PREDICATES, TRUE PASS_REGS);
}

static void RestorePredHash__(USES_REGS1) {
  PredHash = PtoPtoPredAdjust(PredHash);
  if (PredHash == NULL) {
    Yap_Error(SYSTEM_ERROR_FATAL, MkIntTerm(0),
              "restore should find predicate hash table");
  }
  REINIT_RWLOCK(PredHashRWLock);
  RestoreHashPreds(PASS_REGS1); /* does most of the work */
}

static void RestoreEnvInst(yamop start[2], yamop **instp, op_numbers opc,
                           PredEntry *pred) {
  yamop *ipc = start;

  ipc->opc = Yap_opcode(_call);
  ipc->y_u.Osbpp.p = pred;
  ipc->y_u.Osbpp.p0 = pred;
  ipc->y_u.Osbpp.bmap = NULL;
  ipc->y_u.Osbpp.s = -Signed(RealEnvSize);
  ipc = NEXTOP(ipc, Osbpp);
  ipc->opc = Yap_opcode(opc);
  *instp = ipc;
}

static void RestoreOtaplInst__(yamop start[1], OPCODE opc,
                               PredEntry *pe USES_REGS) {
  yamop *ipc = start;

  /* this is a place holder, it should not really be used */
  ipc->opc = Yap_opcode(opc);
  ipc->y_u.Otapl.s = 0;
  ipc->y_u.Otapl.p = pe;
  if (ipc->y_u.Otapl.d)
    ipc->y_u.Otapl.d = PtoOpAdjust(ipc->y_u.Otapl.d);
#ifdef YAPOR
  INIT_YAMOP_LTT(ipc, 1);
#endif /* YAPOR */
#ifdef TABLING
  ipc->y_u.Otapl.te = NULL;
#endif /* TABLING */
}

static void RestoreDBTermsList__(USES_REGS1) {
  if (DBTermsList) {
    struct dbterm_list *dbl = PtoDBTLAdjust(DBTermsList);
    DBTermsList = dbl;
    while (dbl) {
      RestoreDBTermEntry(dbl PASS_REGS);
      dbl = dbl->next_dbl;
    }
  }
}

static void RestoreExpandList__(USES_REGS1) {
  if (ExpandClausesFirst)
    ExpandClausesFirst = PtoOpAdjust(ExpandClausesFirst);
  if (ExpandClausesLast)
    ExpandClausesLast = PtoOpAdjust(ExpandClausesLast);
  {
    yamop *ptr = ExpandClausesFirst;
    while (ptr) {
      do_clean_susp_clauses(ptr PASS_REGS);
      ptr = ptr->y_u.sssllp.snext;
    }
  }
}

static void RestoreUdiControlBlocks(void) {}

static void RestoreIntKeys__(USES_REGS1) {
  if (INT_KEYS != NULL) {
    INT_KEYS = (Prop *)AddrAdjust((ADDR)(INT_KEYS));
    {
      UInt i;
      for (i = 0; i < INT_KEYS_SIZE; i++) {
        if (INT_KEYS[i] != NIL) {
          Prop p0 = INT_KEYS[i] = PropAdjust(INT_KEYS[i]);
          RestoreEntries(RepProp(p0), TRUE PASS_REGS);
        }
      }
    }
  }
}

static void RestoreIntLUKeys__(USES_REGS1) {
  if (INT_LU_KEYS != NULL) {
    INT_LU_KEYS = (Prop *)AddrAdjust((ADDR)(INT_LU_KEYS));
    {
      Int i;
      for (i = 0; i < INT_KEYS_SIZE; i++) {
        Prop p0 = INT_LU_KEYS[i];
        if (p0) {
          p0 = PropAdjust(p0);
          INT_LU_KEYS[i] = p0;
          while (p0) {
            PredEntry *pe = RepPredProp(p0);
            pe->NextOfPE = PropAdjust(pe->NextOfPE);
            CleanCode(pe PASS_REGS);
            p0 = RepProp(pe->NextOfPE);
          }
        }
      }
    }
  }
}

static void RestoreIntBBKeys__(USES_REGS1) {
  if (INT_BB_KEYS != NULL) {
    INT_BB_KEYS = (Prop *)AddrAdjust((ADDR)(INT_BB_KEYS));
    {
      UInt i;
      for (i = 0; i < INT_BB_KEYS_SIZE; i++) {
        if (INT_BB_KEYS[i] != NIL) {
          Prop p0 = INT_BB_KEYS[i] = PropAdjust(INT_BB_KEYS[i]);
          RestoreEntries(RepProp(p0), TRUE PASS_REGS);
        }
      }
    }
  }
}

static void RestoreDBErasedMarker__(USES_REGS1) {
  DBErasedMarker = DBRefAdjust(DBErasedMarker, TRUE);
  DBErasedMarker->id = FunctorDBRef;
  DBErasedMarker->Flags = ErasedMask;
  DBErasedMarker->Code = NULL;
  DBErasedMarker->DBT.DBRefs = NULL;
  DBErasedMarker->Parent = NULL;
}

static void RestoreLogDBErasedMarker__(USES_REGS1) {
  LogDBErasedMarker = PtoLUCAdjust(LogDBErasedMarker);
  LogDBErasedMarker->Id = FunctorDBRef;
  LogDBErasedMarker->ClFlags = ErasedMask | LogUpdMask;
  LogDBErasedMarker->lusl.ClSource = NULL;
  LogDBErasedMarker->ClRefCount = 0;
  LogDBErasedMarker->ClPred = PredLogUpdClause;
  LogDBErasedMarker->ClExt = NULL;
  LogDBErasedMarker->ClPrev = NULL;
  LogDBErasedMarker->ClNext = NULL;
  LogDBErasedMarker->ClSize = (UInt)NEXTOP(((LogUpdClause *)NULL)->ClCode, e);
  LogDBErasedMarker->ClCode->opc = Yap_opcode(_op_fail);
  INIT_CLREF_COUNT(LogDBErasedMarker);
}

static void RestoreDeadStaticClauses__(USES_REGS1) {
  if (DeadStaticClauses) {
    StaticClause *sc = PtoStCAdjust(DeadStaticClauses);
    DeadStaticClauses = sc;
    while (sc) {
      RestoreStaticClause(sc PASS_REGS);
      sc = sc->ClNext;
    }
  }
}

static void RestoreDeadMegaClauses__(USES_REGS1) {
  if (DeadMegaClauses) {
    MegaClause *mc = (MegaClause *)AddrAdjust((ADDR)(DeadMegaClauses));
    DeadMegaClauses = mc;
    while (mc) {
      RestoreMegaClause(mc PASS_REGS);
      mc = mc->ClNext;
    }
  }
}

static void RestoreDeadStaticIndices__(USES_REGS1) {
  if (DeadStaticIndices) {
    StaticIndex *si = (StaticIndex *)AddrAdjust((ADDR)(DeadStaticIndices));
    DeadStaticIndices = si;
    while (si) {
      CleanSIndex(si, FALSE PASS_REGS);
      si = si->SiblingIndex;
    }
  }
}

static void RestoreDBErasedList__(USES_REGS1) {
  if (DBErasedList) {
    LogUpdClause *lcl = DBErasedList = PtoLUCAdjust(DBErasedList);
    while (lcl) {
      RestoreLUClause(lcl, FALSE PASS_REGS);
      lcl = lcl->ClNext;
    }
  }
}

static void RestoreDBErasedIList__(USES_REGS1) {
  if (DBErasedIList) {
    LogUpdIndex *icl = DBErasedIList = LUIndexAdjust(DBErasedIList);
    while (icl) {
      CleanLUIndex(icl, FALSE PASS_REGS);
      icl = icl->SiblingIndex;
    }
  }
}

static void RestoreForeignCode__(USES_REGS1) {
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
    if (f_code->f != NULL) {
      f_code->f = AtomAdjust(f_code->f);
    }
    if (f_code->next != NULL)
      f_code->next = (ForeignObj *)AddrAdjust((ADDR)f_code->next);
    f_code = f_code->next;
  }
}


static void RestoreYapRecords__(USES_REGS1) {
  struct record_list *ptr;

  Yap_Records = DBRecordAdjust(Yap_Records);
  ptr = Yap_Records;
  while (ptr) {
    ptr->next_rec = DBRecordAdjust(ptr->next_rec);
    ptr->prev_rec = DBRecordAdjust(ptr->prev_rec);
    ptr->dbrecord = DBTermAdjust(ptr->dbrecord);
    RestoreDBTerm(ptr->dbrecord, false, 0 PASS_REGS);
    ptr = ptr->next_rec;
  }
}

#if defined(THREADS) || defined(YAPOR)
#include "rglobals.h"
#endif

#include "rlocals.h"

/* restore the failcodes */
static void restore_codes(void) {
  CACHE_REGS
  HeapTop = AddrAdjust(LOCAL_OldHeapTop);

#include "rhstruct.h"

  RestoreWorker(worker_id PASS_REGS);
}

static void RestoreDBEntry(DBRef dbr USES_REGS) {
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
  RestoreDBTerm(&(dbr->DBT), true, 1 PASS_REGS);
  if (dbr->Parent) {
    dbr->Parent = (DBProp)AddrAdjust((ADDR)(dbr->Parent));
  }
  if (dbr->Code != NULL)
    dbr->Code = PtoOpAdjust(dbr->Code);
  if (dbr->Prev != NULL)
    dbr->Prev = DBRefAdjust(dbr->Prev, TRUE);
  if (dbr->Next != NULL)
    dbr->Next = DBRefAdjust(dbr->Next, TRUE);
#ifdef DEBUG_RESTORE2
  fprintf(stderr, "Recomputing masks\n");
#endif
  recompute_mask(dbr);
}

/* Restores a DB structure, as it was saved in the heap */
static void RestoreDB(DBEntry *pp USES_REGS) {
  register DBRef dbr;

  if (pp->First != NULL)
    pp->First = DBRefAdjust(pp->First, TRUE);
  if (pp->Last != NULL)
    pp->Last = DBRefAdjust(pp->Last, TRUE);
  if (pp->ArityOfDB)
    pp->FunctorOfDB = FuncAdjust(pp->FunctorOfDB);
  else
    pp->FunctorOfDB = (Functor)AtomAdjust((Atom)(pp->FunctorOfDB));
  if (pp->F0 != NULL)
    pp->F0 = DBRefAdjust(pp->F0, TRUE);
  if (pp->L0 != NULL)
    pp->L0 = DBRefAdjust(pp->L0, TRUE);
  /* immediate update semantics */
  dbr = pp->F0;
  /* While we have something in the data base, even if erased, restore it */
  while (dbr) {
    RestoreDBEntry(dbr PASS_REGS);
    if (dbr->n != NULL)
      dbr->n = DBRefAdjust(dbr->n, TRUE);
    if (dbr->p != NULL)
      dbr->p = DBRefAdjust(dbr->p, TRUE);
    dbr = dbr->n;
  }
}

/*
 * Restores a group of clauses for the same predicate, starting with First
 * and ending with Last, First may be equal to Last
 */
static void CleanClauses(yamop *First, yamop *Last, PredEntry *pp USES_REGS) {
  if (!First)
    return;
  if (pp->PredFlags & LogUpdatePredFlag) {
    LogUpdClause *cl = ClauseCodeToLogUpdClause(First);
    while (cl != NULL) {
      RestoreLUClause(cl, pp PASS_REGS);
      cl = cl->ClNext;
    }
  } else if (pp->PredFlags & MegaClausePredFlag) {
    MegaClause *cl = ClauseCodeToMegaClause(First);

    RestoreMegaClause(cl PASS_REGS);
  } else if (pp->PredFlags & DynamicPredFlag) {
    yamop *cl = First;

    do {
      RestoreDynamicClause(ClauseCodeToDynamicClause(cl), pp PASS_REGS);
      if (cl == Last)
        return;
      cl = NextDynamicClause(cl);
    } while (TRUE);
  } else {
    StaticClause *cl = ClauseCodeToStaticClause(First);

    do {
      RestoreStaticClause(cl PASS_REGS);
      if (cl->ClCode == Last)
        return;
      cl = cl->ClNext;
    } while (TRUE);
  }
}

/* Restores a DB structure, as it was saved in the heap */
static void RestoreBB(BlackBoardEntry *pp, int int_key USES_REGS) {
  Term t = pp->Element;
  if (t) {
    if (!IsVarTerm(t)) {
      if (IsAtomicTerm(t)) {
        if (IsAtomTerm(t)) {
          pp->Element = AtomTermAdjust(t);
        }
      } else {
        RestoreLUClause((LogUpdClause *)DBRefOfTerm(t), NULL PASS_REGS);
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

static void restore_static_array(StaticArrayEntry *ae USES_REGS) {
  Int sz = -ae->ArrayEArity;
  switch (ae->ArrayType) {
  case array_of_ints:
  case array_of_doubles:
  case array_of_chars:
  case array_of_uchars:
    return;
  case array_of_ptrs: {
    AtomEntry **base = (AtomEntry **)AddrAdjust((ADDR)(ae->ValueOfVE.ptrs));
    Int i;
    ae->ValueOfVE.ptrs = base;
    if (ae != NULL) {
      for (i = 0; i < sz; i++) {
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
  case array_of_atoms: {
    Term *base = (Term *)AddrAdjust((ADDR)(ae->ValueOfVE.atoms));
    Int i;
    ae->ValueOfVE.atoms = base;
    if (ae != 0L) {
      for (i = 0; i < sz; i++) {
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
  case array_of_dbrefs: {
    Term *base = (Term *)AddrAdjust((ADDR)(ae->ValueOfVE.dbrefs));
    Int i;

    ae->ValueOfVE.dbrefs = base;
    if (ae != 0L) {
      for (i = 0; i < sz; i++) {
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
  case array_of_nb_terms: {
    live_term *base = (live_term *)AddrAdjust((ADDR)(ae->ValueOfVE.lterms));
    Int i;

    ae->ValueOfVE.lterms = base;
    if (ae != 0L) {
      for (i = 0; i < sz; i++, base++) {
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
          RestoreDBTerm(db, false, 1 PASS_REGS);
          base->tstore = AbsAppl((CELL *)db);
        }
      }
    }
  }
  case array_of_terms: {
    DBTerm **base = (DBTerm **)AddrAdjust((ADDR)(ae->ValueOfVE.terms));
    Int i;

    ae->ValueOfVE.terms = base;
    if (ae != 0L) {
      for (i = 0; i < sz; i++) {
        DBTerm *reg = *base;
        if (reg == NULL) {
          base++;
        } else {
          *base++ = reg = DBTermAdjust(reg);
          RestoreDBTerm(reg, false, 1 PASS_REGS);
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
static void CleanCode(PredEntry *pp USES_REGS) {
  pred_flags_t flag;

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
    if (pp->src.OwnerFile) {
      pp->src.OwnerFile = AtomAdjust(pp->src.OwnerFile);
    }
  }
  pp->OpcodeOfPred = Yap_opcode(Yap_op_from_opcode(pp->OpcodeOfPred));
  if (pp->NextPredOfModule) {
    pp->NextPredOfModule = PtoPredAdjust(pp->NextPredOfModule);
  }
  if (pp->PredFlags & (AsmPredFlag | CPredFlag)) {
    /* assembly */
    if (pp->CodeOfPred) {
      pp->CodeOfPred = PtoOpAdjust(pp->CodeOfPred);
      CleanClauses(pp->CodeOfPred, pp->CodeOfPred, pp PASS_REGS);
    }
  } else {
    yamop *FirstC, *LastC;
    /* Prolog code */
    if (pp->FirstClause)
      pp->FirstClause = PtoOpAdjust(pp->FirstClause);
    if (pp->LastClause)
      pp->LastClause = PtoOpAdjust(pp->LastClause);
    pp->CodeOfPred = PtoOpAdjust(pp->CodeOfPred);
    pp->TrueCodeOfPred = PtoOpAdjust(pp->TrueCodeOfPred);
    pp->cs.p_code.ExpandCode = Yap_opcode(_expand_index);
    flag = pp->PredFlags;
    FirstC = pp->FirstClause;
    LastC = pp->LastClause;
    /* We just have a fail here */
    if (FirstC == NULL && LastC == NULL) {
      return;
    }
#ifdef DEBUG_RESTORE2
    fprintf(stderr, "at %lx Correcting clauses from %p to %p\n",
            *(OPCODE *)FirstC, FirstC, LastC);
#endif
    CleanClauses(FirstC, LastC, pp PASS_REGS);
    if (flag & IndexedPredFlag) {
#ifdef DEBUG_RESTORE2
      fprintf(stderr, "Correcting indexed code\n");
#endif
      if (flag & LogUpdatePredFlag) {
        CleanLUIndex(ClauseCodeToLogUpdIndex(pp->TrueCodeOfPred),
                     TRUE PASS_REGS);
      } else {
        CleanSIndex(ClauseCodeToStaticIndex(pp->TrueCodeOfPred),
                    TRUE PASS_REGS);
      }
    } else if (flag & DynamicPredFlag) {
#ifdef DEBUG_RESTORE2
      fprintf(stderr, "Correcting dynamic code\n");
#endif
      RestoreDynamicClause(
          ClauseCodeToDynamicClause(pp->TrueCodeOfPred),
          pp PASS_REGS);
    }
  }
  /* we are pointing at ourselves */
}

/*
 * Restores all of the entries, for a particular atom, we only have problems
 * if we find code or data bases
 */
static void RestoreEntries(PropEntry *pp, int int_key USES_REGS) {
  while (!EndOfPAEntr(pp)) {
    switch (pp->KindOfPE) {
    case FunctorProperty: {
      FunctorEntry *fe = (FunctorEntry *)pp;
      Prop p0;
      fe->NextOfPE = PropAdjust(fe->NextOfPE);
      fe->NameOfFE = AtomAdjust(fe->NameOfFE);
      p0 = fe->PropsOfFE = PropAdjust(fe->PropsOfFE);
      if (!EndOfPAEntr(p0)) {
        /* at most one property */
        CleanCode(RepPredProp(p0) PASS_REGS);
        RepPredProp(p0)->NextOfPE = PropAdjust(RepPredProp(p0)->NextOfPE);
      }
    } break;
    case ValProperty: {
      ValEntry *ve = (ValEntry *)pp;
      Term tv = ve->ValueOfVE;
      ve->NextOfPE = PropAdjust(ve->NextOfPE);
      if (IsAtomTerm(tv))
        ve->ValueOfVE = AtomTermAdjust(tv);
    } break;
    case HoldProperty: {
      HoldEntry *he = (HoldEntry *)pp;
      he->NextOfPE = PropAdjust(he->NextOfPE);
    } break;
    case MutexProperty: {
      HoldEntry *he = (HoldEntry *)pp;
      he->NextOfPE = PropAdjust(he->NextOfPE);
    } break;
    case TranslationProperty: {
      TranslationEntry *he = (TranslationEntry *)pp;
      he->NextOfPE = PropAdjust(he->NextOfPE);
    } break;
    case FlagProperty: {
      FlagEntry *he = (FlagEntry *)pp;
      he->NextOfPE = PropAdjust(he->NextOfPE);
    } break;
    case ArrayProperty: {
      ArrayEntry *ae = (ArrayEntry *)pp;
      ae->NextOfPE = PropAdjust(ae->NextOfPE);
      if (ae->TypeOfAE == STATIC_ARRAY) {
        /* static array entry */
        StaticArrayEntry *sae = (StaticArrayEntry *)ae;
        if (sae->NextAE)
          sae->NextAE = PtoArraySAdjust(sae->NextAE);
        restore_static_array(sae PASS_REGS);
      } else {
        if (ae->NextAE)
          ae->NextAE = PtoArrayEAdjust(ae->NextAE);
        if (IsVarTerm(ae->ValueOfVE))
          RESET_VARIABLE(&(ae->ValueOfVE));
        else {
          CELL *ptr = RepAppl(ae->ValueOfVE);
          /* in fact it should just be a pointer to the global,
             but we'll be conservative.
             Notice that the variable should have been reset in restore_program
             mode.
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
    } break;
    case PEProp: {
      PredEntry *pe = (PredEntry *)pp;
      pe->NextOfPE = PropAdjust(pe->NextOfPE);
      CleanCode(pe PASS_REGS);
    } break;
    case DBProperty:
    case CodeDBProperty:
#ifdef DEBUG_RESTORE2
      fprintf(stderr, "Correcting data base clause at %p\n", pp);
#endif
      {
        DBEntry *de = (DBEntry *)pp;
        de->NextOfPE = PropAdjust(de->NextOfPE);
        RestoreDB(de PASS_REGS);
      }
      break;
    case BBProperty: {
      BlackBoardEntry *bb = (BlackBoardEntry *)pp;
      bb->NextOfPE = PropAdjust(bb->NextOfPE);
      RestoreBB(bb, int_key PASS_REGS);
    } break;
    case GlobalProperty: {
      GlobalEntry *gb = (GlobalEntry *)pp;
      Term gbt = gb->global;

      gb->NextOfPE = PropAdjust(gb->NextOfPE);
      gb->AtomOfGE = AtomEntryAdjust(gb->AtomOfGE);
      if (gb->NextGE) {
        gb->NextGE = GlobalEntryAdjust(gb->NextGE);
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
    } break;
    case OpProperty: {
      OpEntry *opp = (OpEntry *)pp;
      if (opp->NextOfPE) {
        opp->NextOfPE = PropAdjust(opp->NextOfPE);
      }
      opp->OpName = AtomAdjust(opp->OpName);
      if (opp->OpModule) {
        opp->OpModule = AtomTermAdjust(opp->OpModule);
      }
      if (opp->OpNext) {
        opp->OpNext = OpEntryAdjust(opp->OpNext);
      }
    } break;
    case ModProperty: {
      ModEntry *me = (ModEntry *)pp;
      if (me->NextOfPE) {
        me->NextOfPE = PropAdjust(me->NextOfPE);
      }
      if (me->PredForME) {
        me->PredForME = PtoPredAdjust(me->PredForME);
      }
      me->AtomOfME = AtomAdjust(me->AtomOfME);
      if (me->NextME)
        me->NextME = (struct mod_entry *)AddrAdjust((ADDR)me->NextME);
    } break;
    case ExpProperty:
      pp->NextOfPE = PropAdjust(pp->NextOfPE);
      break;
    case BlobProperty:
      pp->NextOfPE = PropAdjust(pp->NextOfPE);
      {
        YAP_BlobPropEntry *bpe = (YAP_BlobPropEntry *)pp;
        bpe->blob_type = BlobTypeAdjust(bpe->blob_type);
      }
      break;
    default:
      /* OOPS */
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "Invalid Atom Property %d at %p", pp->KindOfPE, pp);
      return;
    }
    pp = RepProp(pp->NextOfPE);
  }
}

static void RestoreAtom(AtomEntry *at USES_REGS) {
  AtomEntry *nat;

  /* this should be done before testing for wide atoms */
  at->PropsOfAE = PropAdjust(at->PropsOfAE);
#if DEBUG_RESTORE2 /* useful during debug */
  if (IsWideAtom(AbsAtom(at)))
    fprintf(stderr, "Restoring %S\n", at->WStrOfAE);
  else
    fprintf(stderr, "Restoring %s\n", at->StrOfAE);
#endif
  RestoreEntries(RepProp(at->PropsOfAE), FALSE PASS_REGS);
  /* cannot use AtomAdjust without breaking agc */
  nat = RepAtom(at->NextOfAE);
  if (nat)
    at->NextOfAE = AbsAtom(AtomEntryAdjust(nat));
}

#endif
