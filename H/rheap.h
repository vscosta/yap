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
* Last rev:     $Date: 2006-08-22 16:12:46 $,$Author: vsc $						 *
* $Log: not supported by cvs2svn $
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

/* Now, everything on its place so you must adjust the pointers */

static void 
do_clean_susp_clauses(yamop *ipc) {
  COUNT i;
  yamop **st = (yamop **)NEXTOP(ipc,sp);

  ipc->opc = Yap_opcode(_expand_clauses);
  ipc->u.sp.p = PtoPredAdjust(ipc->u.sp.p);
  if (ipc->u.sp.sprev) {
    ipc->u.sp.sprev = PtoOpAdjust(ipc->u.sp.sprev);
  }
  if (ipc->u.sp.snext) {
    ipc->u.sp.snext = PtoOpAdjust(ipc->u.sp.snext);
  }
  for (i = 0; i < ipc->u.sp.s1; i++, st++) {
    if (*st) {
      *st = PtoOpAdjust(*st);
    }
  }
}

#include "rclause.h"

/* Restoring the heap */

/* adjusts terms stored in the data base, when they have no variables */
static Term 
AdjustDBTerm(Term trm, Term *p_base)
{
  if (IsAtomTerm(trm))
    return AtomTermAdjust(trm);
  if (IsPairTerm(trm)) {
    Term           *p;

    p = PtoHeapCellAdjust(RepPair(trm));
    if (p >= p_base) {
      p[0] = AdjustDBTerm(p[0], p);
      p[1] = AdjustDBTerm(p[1], p);
    }
    return AbsPair(p);
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
RestoreDBTerm(DBTerm *dbr)
{
#ifdef COROUTINING
  if (dbr->attachments)
    dbr->attachments = AdjustDBTerm(dbr->attachments, dbr->Contents);
#endif
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
  restore_opcodes(cl->ClCode);
}

/* Restores a prolog clause, in its compiled form */
static void 
RestoreMegaClause(MegaClause *cl)
/*
 * Cl points to the start of the code, IsolFlag tells if we have a single
 * clause for this predicate or not 
 */
{
  cl->ClPred = PtoPredAdjust(cl->ClPred);
  if (cl->ClNext) {
     cl->ClNext = (MegaClause *)AddrAdjust((ADDR)(cl->ClNext));
  }
  restore_opcodes(cl->ClCode);
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
  restore_opcodes(cl->ClCode);
}

/* Restores a prolog clause, in its compiled form */
static void 
RestoreLUClause(LogUpdClause *cl, PredEntry *pp)
/*
 * Cl points to the start of the code, IsolFlag tells if we have a single
 * clause for this predicate or not 
 */
{
  INIT_LOCK(cl->ClLock);
  if (cl->ClFlags & LogUpdRuleMask) {
    cl->ClExt = PtoOpAdjust(cl->ClExt);
  }
  if (cl->ClSource) {
    cl->ClSource = DBTermAdjust(cl->ClSource);
    RestoreDBTerm(cl->ClSource);
  }
  if (cl->ClPrev) {
    cl->ClPrev = PtoLUCAdjust(cl->ClPrev);
  }
  if (cl->ClNext) {
    cl->ClNext = PtoLUCAdjust(cl->ClNext);
  }
  cl->ClPred = PtoPredAdjust(cl->ClPred);
  restore_opcodes(cl->ClCode);
}

static void 
CleanLUIndex(LogUpdIndex *idx)
{
  idx->ClRefCount = 0;
  INIT_LOCK(idx->ClLock);
  idx->ClPred = PtoPredAdjust(idx->ClPred);
  if (idx->ParentIndex)
    idx->ParentIndex = LUIndexAdjust(idx->ParentIndex);
  if (idx->SiblingIndex) {
    idx->SiblingIndex = LUIndexAdjust(idx->SiblingIndex);
    CleanLUIndex(idx->SiblingIndex);
  }
  if (idx->ChildIndex) {
    idx->ChildIndex = LUIndexAdjust(idx->ChildIndex);
    CleanLUIndex(idx->ChildIndex);
  }
  if (!(idx->ClFlags & SwitchTableMask)) {
    restore_opcodes(idx->ClCode);
  }
}

static void 
CleanSIndex(StaticIndex *idx)
{
  idx->ClPred = PtoPredAdjust(idx->ClPred);
  if (idx->SiblingIndex) {
    idx->SiblingIndex = SIndexAdjust(idx->SiblingIndex);
    CleanSIndex(idx->SiblingIndex);
  }
  if (idx->ChildIndex) {
    idx->ChildIndex = SIndexAdjust(idx->ChildIndex);
    CleanSIndex(idx->ChildIndex);
  }
  if (!(idx->ClFlags & SwitchTableMask)) {
    restore_opcodes(idx->ClCode);
  }
}

/* restore the failcodes */
static void 
restore_codes(void)
{
  Yap_heap_regs->heap_top = AddrAdjust(OldHeapTop);
  if (Yap_heap_regs->heap_lim) {
    Yap_heap_regs->heap_lim = AddrAdjust(Yap_heap_regs->heap_lim);
  }
#ifdef YAPOR
  Yap_heap_regs->seq_def = TRUE;
  Yap_heap_regs->getwork_code.opc = Yap_opcode(_getwork);
  INIT_YAMOP_LTT(&(Yap_heap_regs->getwork_code), 0);
  Yap_heap_regs->getwork_seq_code.opc = Yap_opcode(_getwork_seq);
  INIT_YAMOP_LTT(&(Yap_heap_regs->getwork_seq_code), 0);
  Yap_heap_regs->getwork_first_time_code.opc = Yap_opcode(_getwork_first_time);
#endif /* YAPOR */
#ifdef TABLING
  Yap_heap_regs->table_load_answer_code.opc = Yap_opcode(_table_load_answer);
  Yap_heap_regs->table_try_answer_code.opc = Yap_opcode(_table_try_answer);
  Yap_heap_regs->table_answer_resolution_code.opc = Yap_opcode(_table_answer_resolution);
  Yap_heap_regs->table_completion_code.opc = Yap_opcode(_table_completion);
#ifdef YAPOR
  INIT_YAMOP_LTT(&(Yap_heap_regs->table_load_answer_code), 0);
  INIT_YAMOP_LTT(&(Yap_heap_regs->table_try_answer_code), 0);
  INIT_YAMOP_LTT(&(Yap_heap_regs->table_completion_code), 0);
  INIT_YAMOP_LTT(&(Yap_heap_regs->table_answer_resolution_code), 0);
#endif /* YAPOR */
#endif /* TABLING */
  Yap_heap_regs->expand_op_code = Yap_opcode(_expand_index);
  if (Yap_heap_regs->expand_clauses_first)
    Yap_heap_regs->expand_clauses_first = PtoOpAdjust(Yap_heap_regs->expand_clauses_first);
  if (Yap_heap_regs->expand_clauses_last)
    Yap_heap_regs->expand_clauses_last = PtoOpAdjust(Yap_heap_regs->expand_clauses_last);
  {
    yamop *ptr = Yap_heap_regs->expand_clauses_first;
    while (ptr) {
      do_clean_susp_clauses(ptr);
      ptr = ptr->u.sp.snext;
    }
  }
  Yap_heap_regs->failcode->opc = Yap_opcode(_op_fail);
  Yap_heap_regs->failcode_1 = Yap_opcode(_op_fail);
  Yap_heap_regs->failcode_2 = Yap_opcode(_op_fail);
  Yap_heap_regs->failcode_3 = Yap_opcode(_op_fail);
  Yap_heap_regs->failcode_4 = Yap_opcode(_op_fail);
  Yap_heap_regs->failcode_5 = Yap_opcode(_op_fail);
  Yap_heap_regs->failcode_6 = Yap_opcode(_op_fail);

  Yap_heap_regs->env_for_trustfail_code.op = Yap_opcode(_call);
  Yap_heap_regs->trustfailcode->opc = Yap_opcode(_trust_fail);

  Yap_heap_regs->env_for_yes_code.op = Yap_opcode(_call);
  Yap_heap_regs->yescode->opc = Yap_opcode(_Ystop);
  Yap_heap_regs->undef_op = Yap_opcode(_undef_p);
  Yap_heap_regs->index_op = Yap_opcode(_index_pred);
  Yap_heap_regs->fail_op = Yap_opcode(_op_fail);
  Yap_heap_regs->nocode->opc = Yap_opcode(_Nstop);
#ifdef YAPOR
  INIT_YAMOP_LTT(&(Yap_heap_regs->nocode), 1);
  INIT_YAMOP_LTT(&(Yap_heap_regs->rtrycode), 1);
#endif /* YAPOR */
  ((yamop *)(&Yap_heap_regs->rtrycode))->opc = Yap_opcode(_retry_and_mark);
  if (((yamop *)(&Yap_heap_regs->rtrycode))->u.ld.d != NIL)
    ((yamop *)(&Yap_heap_regs->rtrycode))->u.ld.d =
      PtoOpAdjust(((yamop *)(&Yap_heap_regs->rtrycode))->u.ld.d);
  {
    int             arity;
    arity = Yap_heap_regs->clausecode->arity;
    if (Yap_heap_regs->clausecode->clause != NIL)
      Yap_heap_regs->clausecode->clause =
	PtoOpAdjust(Yap_heap_regs->clausecode->clause);
    if (arity) {
      Yap_heap_regs->clausecode->func =
	FuncAdjust(Yap_heap_regs->clausecode->func); 
    } else {
      /* an atom */
      Yap_heap_regs->clausecode->func =
	(Functor)AtomAdjust((Atom)(Yap_heap_regs->clausecode->func));
    }
  }
#if !defined(THREADS)
  /* restore consult stack. It consists of heap pointers, so it
     is easy to fix.
  */
  Yap_heap_regs->wl.consultlow  = 
    ConsultObjAdjust(Yap_heap_regs->wl.consultlow);
  Yap_heap_regs->wl.consultbase =
    ConsultObjAdjust(Yap_heap_regs->wl.consultbase);
  Yap_heap_regs->wl.consultsp   =
    ConsultObjAdjust(Yap_heap_regs->wl.consultsp);
  {
    /* we assume all pointers have the same size */
    register consult_obj *pt = Yap_heap_regs->wl.consultsp;
    while (pt <
	   Yap_heap_regs->wl.consultlow+Yap_heap_regs->wl.consultcapacity) {
      pt->p = PropAdjust(pt->p);
      pt ++;
    }
  }
#endif
#if USE_THREADED_CODE
  Yap_heap_regs->op_rtable = (opentry *)
    CodeAddrAdjust((CODEADDR)(Yap_heap_regs->op_rtable));
#endif
  if (Yap_heap_regs->atprompt != NIL) {
    Yap_heap_regs->atprompt =
      AtomAdjust(Yap_heap_regs->atprompt);
  }
  if (Yap_heap_regs->char_conversion_table) {
    Yap_heap_regs->char_conversion_table = (char *)
      AddrAdjust((ADDR)Yap_heap_regs->char_conversion_table);
  }
  if (Yap_heap_regs->char_conversion_table2) {
    Yap_heap_regs->char_conversion_table2 = (char *)
      AddrAdjust((ADDR)Yap_heap_regs->char_conversion_table2);
  }
  if (Yap_heap_regs->op_list) {
    Yap_heap_regs->op_list = (struct operator_entry *)
      AddrAdjust((ADDR)Yap_heap_regs->op_list);
  }
  if (Yap_heap_regs->dead_static_clauses) {
    StaticClause *sc = PtoStCAdjust(Yap_heap_regs->dead_static_clauses);
    Yap_heap_regs->dead_static_clauses = sc;
    while (sc) {
      RestoreStaticClause(sc);
      sc = sc->ClNext;
    }
  }
  if (Yap_heap_regs->dead_mega_clauses) {
    MegaClause *mc = (MegaClause *)AddrAdjust((ADDR)(Yap_heap_regs->dead_mega_clauses));
    Yap_heap_regs->dead_mega_clauses = mc;
    while (mc) {
      RestoreMegaClause(mc);
      mc = mc->ClNext;
    }
  }
  if (Yap_heap_regs->dead_static_indices) {
    StaticIndex *si = (StaticIndex *)AddrAdjust((ADDR)(Yap_heap_regs->dead_static_indices));
    Yap_heap_regs->dead_static_indices = si;
    while (si) {
      CleanSIndex(si);
      si = si->SiblingIndex;
    }
  }
  Yap_heap_regs->retry_recorded_k_code =
    PtoOpAdjust(Yap_heap_regs->retry_recorded_k_code);
  Yap_heap_regs->retry_c_recordedp_code =
    PtoOpAdjust(Yap_heap_regs->retry_c_recordedp_code);
  if (Yap_heap_regs->IntKeys != NULL) {
    Yap_heap_regs->IntKeys = (Prop *)AddrAdjust((ADDR)(Yap_heap_regs->IntKeys));
    {
      UInt i;
      for (i = 0; i < Yap_heap_regs->int_keys_size; i++) {
	if (Yap_heap_regs->IntKeys[i] != NIL) {
	  Prop p0 = Yap_heap_regs->IntKeys[i] = PropAdjust(Yap_heap_regs->IntKeys[i]);
	  RestoreEntries(RepProp(p0));
	}
      }
    }
  }
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
  if (Yap_heap_regs->IntBBKeys != NULL) {
    Yap_heap_regs->IntBBKeys = (Prop *)AddrAdjust((ADDR)(Yap_heap_regs->IntBBKeys));
    {
      UInt i;
      for (i = 0; i < Yap_heap_regs->int_bb_keys_size; i++) {
	if (Yap_heap_regs->IntBBKeys[i] != NIL) {
	  Prop p0 = Yap_heap_regs->IntBBKeys[i] = PropAdjust(Yap_heap_regs->IntBBKeys[i]);
	  RestoreEntries(RepProp(p0));
	}
      }
    }
  }
  {
    /* adjust atoms in atom table */
    unsigned int i = 0;

    for (i = 0; i < Yap_heap_regs->no_of_modules; i++) {
      Yap_heap_regs->module_name[i] = AtomTermAdjust(Yap_heap_regs->module_name[i]);
      if (Yap_heap_regs->module_pred[i]) {
	Yap_heap_regs->module_pred[i] = PtoPredAdjust(Yap_heap_regs->module_pred[i]);
      }
    }
  }
  Yap_heap_regs->atom_abol = AtomAdjust(Yap_heap_regs->atom_abol);
  Yap_heap_regs->atom_append = AtomAdjust(Yap_heap_regs->atom_append);
  Yap_heap_regs->atom_array = AtomAdjust(Yap_heap_regs->atom_array);
  Yap_heap_regs->atom_assert = AtomAdjust(Yap_heap_regs->atom_assert);
  Yap_heap_regs->atom_alarm = AtomAdjust(Yap_heap_regs->atom_alarm);
#ifdef COROUTINING
  Yap_heap_regs->atom_att = AtomAdjust(Yap_heap_regs->atom_att);
#endif
  Yap_heap_regs->atom_b = AtomAdjust(Yap_heap_regs->atom_b);
  Yap_heap_regs->atom_break = AtomAdjust(Yap_heap_regs->atom_break);
  Yap_heap_regs->atom_call = AtomAdjust(Yap_heap_regs->atom_call);
  Yap_heap_regs->atom_catch = AtomAdjust(Yap_heap_regs->atom_catch);
  Yap_heap_regs->atom_comma = AtomAdjust(Yap_heap_regs->atom_comma);
  Yap_heap_regs->atom_cpu_time = AtomAdjust(Yap_heap_regs->atom_cpu_time);
  Yap_heap_regs->atom_csult = AtomAdjust(Yap_heap_regs->atom_csult);
  Yap_heap_regs->atom_cut = AtomAdjust(Yap_heap_regs->atom_cut);
  Yap_heap_regs->atom_cut_by = AtomAdjust(Yap_heap_regs->atom_cut_by);
#ifdef EUROTRA
#ifdef SFUNC
  Yap_heap_regs->atom_dollar_undef = AtomAdjust(Yap_heap_regs->atom_dollar_undef);
#endif
#endif
  Yap_heap_regs->atom_dbref = AtomAdjust(Yap_heap_regs->atom_dbref);
  Yap_heap_regs->atom_e = AtomAdjust(Yap_heap_regs->atom_e);
  Yap_heap_regs->atom_e_q = AtomAdjust(Yap_heap_regs->atom_e_q);
  Yap_heap_regs->atom_eof = AtomAdjust(Yap_heap_regs->atom_eof);
#ifdef EUROTRA
  Yap_heap_regs->atom_f_b = AtomAdjust(Yap_heap_regs->atom_f_b);
#endif
  Yap_heap_regs->atom_fail = AtomAdjust(Yap_heap_regs->atom_fail);
  Yap_heap_regs->atom_false = AtomAdjust(Yap_heap_regs->atom_false);
  Yap_heap_regs->atom_fast = AtomAdjust(Yap_heap_regs->atom_fast);
  Yap_heap_regs->atom_g_t = AtomAdjust(Yap_heap_regs->atom_g_t);
  Yap_heap_regs->atom_gc = AtomAdjust(Yap_heap_regs->atom_gc);
  Yap_heap_regs->atom_gc_margin = AtomAdjust(Yap_heap_regs->atom_gc_margin);
  Yap_heap_regs->atom_gc_trace = AtomAdjust(Yap_heap_regs->atom_gc_trace);
  Yap_heap_regs->atom_gc_verbose = AtomAdjust(Yap_heap_regs->atom_gc_verbose);
  Yap_heap_regs->atom_gc_very_verbose = AtomAdjust(Yap_heap_regs->atom_gc_very_verbose);
  Yap_heap_regs->atom_global = AtomAdjust(Yap_heap_regs->atom_global);
  Yap_heap_regs->atom_heap_used = AtomAdjust(Yap_heap_regs->atom_heap_used);
  Yap_heap_regs->atom_inf = AtomAdjust(Yap_heap_regs->atom_inf);
  Yap_heap_regs->atom_l_t = AtomAdjust(Yap_heap_regs->atom_l_t);
  Yap_heap_regs->atom_local = AtomAdjust(Yap_heap_regs->atom_local);
  Yap_heap_regs->atom_meta_call = AtomAdjust(Yap_heap_regs->atom_meta_call);
  Yap_heap_regs->atom_minus = AtomAdjust(Yap_heap_regs->atom_minus);
  Yap_heap_regs->atom_multi_file = AtomAdjust(Yap_heap_regs->atom_multi_file);
  Yap_heap_regs->atom_nan = AtomAdjust(Yap_heap_regs->atom_nan);
  Yap_heap_regs->atom_otherwise = AtomAdjust(Yap_heap_regs->atom_otherwise);
  Yap_heap_regs->atom_pi = AtomAdjust(Yap_heap_regs->atom_pi);
  Yap_heap_regs->atom_plus = AtomAdjust(Yap_heap_regs->atom_plus);
  Yap_heap_regs->atom_portray = AtomAdjust(Yap_heap_regs->atom_portray);
  Yap_heap_regs->atom_profile = AtomAdjust(Yap_heap_regs->atom_profile);
  Yap_heap_regs->atom_random = AtomAdjust(Yap_heap_regs->atom_random);
  Yap_heap_regs->atom_read = AtomAdjust(Yap_heap_regs->atom_read);
  Yap_heap_regs->atom_repeat = AtomAdjust(Yap_heap_regs->atom_repeat);
  Yap_heap_regs->atom_restore_regs = AtomAdjust(Yap_heap_regs->atom_restore_regs);
#if HAVE_SIGACTION
  Yap_heap_regs->atom_sig_pending = AtomAdjust(Yap_heap_regs->atom_sig_pending);
#endif
  Yap_heap_regs->atom_stack_free = AtomAdjust(Yap_heap_regs->atom_stack_free);
  Yap_heap_regs->atom_true = AtomAdjust(Yap_heap_regs->atom_true);
  Yap_heap_regs->atom_user = AtomAdjust(Yap_heap_regs->atom_user);
  Yap_heap_regs->atom_usr_err = AtomAdjust(Yap_heap_regs->atom_usr_err);
  Yap_heap_regs->atom_usr_in = AtomAdjust(Yap_heap_regs->atom_usr_in);
  Yap_heap_regs->atom_usr_out = AtomAdjust(Yap_heap_regs->atom_usr_out);
  Yap_heap_regs->atom_version_number = AtomAdjust(Yap_heap_regs->atom_version_number);
  Yap_heap_regs->atom_write = AtomAdjust(Yap_heap_regs->atom_write);
  Yap_heap_regs->float_format = AtomAdjust(Yap_heap_regs->float_format);
#ifdef   USE_SOCKET
  Yap_heap_regs->functor_af_inet = FuncAdjust(Yap_heap_regs->functor_af_inet);
  Yap_heap_regs->functor_af_local = FuncAdjust(Yap_heap_regs->functor_af_local);
  Yap_heap_regs->functor_af_unix = FuncAdjust(Yap_heap_regs->functor_af_unix);
#endif
  Yap_heap_regs->functor_alt_not = FuncAdjust(Yap_heap_regs->functor_alt_not);
  Yap_heap_regs->functor_arrow = FuncAdjust(Yap_heap_regs->functor_arrow);
  Yap_heap_regs->functor_assert = FuncAdjust(Yap_heap_regs->functor_assert);
  Yap_heap_regs->functor_at_found_one = FuncAdjust(Yap_heap_regs->functor_at_found_one);
#ifdef COROUTINING
  Yap_heap_regs->functor_att_goal = FuncAdjust(Yap_heap_regs->functor_att_goal);
#endif
  Yap_heap_regs->functor_braces = FuncAdjust(Yap_heap_regs->functor_braces);
  Yap_heap_regs->functor_call = FuncAdjust(Yap_heap_regs->functor_call);
  Yap_heap_regs->functor_cut_by = FuncAdjust(Yap_heap_regs->functor_cut_by);
  Yap_heap_regs->functor_comma = FuncAdjust(Yap_heap_regs->functor_comma);
  Yap_heap_regs->functor_creep = FuncAdjust(Yap_heap_regs->functor_creep);
  Yap_heap_regs->functor_csult = FuncAdjust(Yap_heap_regs->functor_csult);
  Yap_heap_regs->functor_eq = FuncAdjust(Yap_heap_regs->functor_eq);
  Yap_heap_regs->functor_execute_in_mod = FuncAdjust(Yap_heap_regs->functor_execute_in_mod);
  Yap_heap_regs->functor_execute_within = FuncAdjust(Yap_heap_regs->functor_execute_within);
  Yap_heap_regs->functor_g_atom = FuncAdjust(Yap_heap_regs->functor_g_atom);
  Yap_heap_regs->functor_g_atomic = FuncAdjust(Yap_heap_regs->functor_g_atomic);
  Yap_heap_regs->functor_g_compound = FuncAdjust(Yap_heap_regs->functor_g_compound);
  Yap_heap_regs->functor_g_float = FuncAdjust(Yap_heap_regs->functor_g_float);
  Yap_heap_regs->functor_g_format_at = FuncAdjust(Yap_heap_regs->functor_g_format_at);
  Yap_heap_regs->functor_g_integer = FuncAdjust(Yap_heap_regs->functor_g_integer);
  Yap_heap_regs->functor_g_number = FuncAdjust(Yap_heap_regs->functor_g_number);
  Yap_heap_regs->functor_g_primitive = FuncAdjust(Yap_heap_regs->functor_g_primitive);
  Yap_heap_regs->functor_g_var = FuncAdjust(Yap_heap_regs->functor_g_var);
  Yap_heap_regs->functor_last_execute_within = FuncAdjust(Yap_heap_regs->functor_last_execute_within);
  Yap_heap_regs->functor_list = FuncAdjust(Yap_heap_regs->functor_list);
  Yap_heap_regs->functor_mega_clause = FuncAdjust(Yap_heap_regs->functor_mega_clause);
  Yap_heap_regs->functor_module = FuncAdjust(Yap_heap_regs->functor_module);
  Yap_heap_regs->functor_multi_file_clause = FuncAdjust(Yap_heap_regs->functor_multi_file_clause);
#ifdef MULTI_ASSIGNMENT_VARIABLES
  Yap_heap_regs->functor_mutable = FuncAdjust(Yap_heap_regs->functor_mutable);
#endif
  Yap_heap_regs->functor_nb_queue = FuncAdjust(Yap_heap_regs->functor_nb_queue);
  Yap_heap_regs->functor_not = FuncAdjust(Yap_heap_regs->functor_not);
  Yap_heap_regs->functor_or = FuncAdjust(Yap_heap_regs->functor_or);
  Yap_heap_regs->functor_portray = FuncAdjust(Yap_heap_regs->functor_portray);
  Yap_heap_regs->functor_query = FuncAdjust(Yap_heap_regs->functor_query);
  Yap_heap_regs->functor_static_clause = FuncAdjust(Yap_heap_regs->functor_static_clause);
  Yap_heap_regs->functor_stream = FuncAdjust(Yap_heap_regs->functor_stream);
  Yap_heap_regs->functor_stream_pos = FuncAdjust(Yap_heap_regs->functor_stream_pos);
  Yap_heap_regs->functor_stream_eOS = FuncAdjust(Yap_heap_regs->functor_stream_eOS);
  Yap_heap_regs->functor_change_module = FuncAdjust(Yap_heap_regs->functor_change_module);
  Yap_heap_regs->functor_current_module = FuncAdjust(Yap_heap_regs->functor_current_module);
  Yap_heap_regs->functor_u_minus = FuncAdjust(Yap_heap_regs->functor_u_minus);
  Yap_heap_regs->functor_u_plus = FuncAdjust(Yap_heap_regs->functor_u_plus);
  Yap_heap_regs->functor_v_bar = FuncAdjust(Yap_heap_regs->functor_v_bar);
  Yap_heap_regs->functor_var = FuncAdjust(Yap_heap_regs->functor_var);
#ifdef EUROTRA
  Yap_heap_regs->term_dollar_u = AtomTermAdjust(Yap_heap_regs->term_dollar_u);
#endif
  Yap_heap_regs->term_prolog = AtomTermAdjust(Yap_heap_regs->term_prolog);
  Yap_heap_regs->term_refound_var = AtomTermAdjust(Yap_heap_regs->term_refound_var);
  Yap_heap_regs->user_module = AtomTermAdjust(Yap_heap_regs->user_module);
  Yap_heap_regs->idb_module = AtomTermAdjust(Yap_heap_regs->idb_module);
  Yap_heap_regs->attributes_module = AtomTermAdjust(Yap_heap_regs->attributes_module);
  Yap_heap_regs->charsio_module = AtomTermAdjust(Yap_heap_regs->charsio_module);
  Yap_heap_regs->terms_module = AtomTermAdjust(Yap_heap_regs->terms_module);
  Yap_heap_regs->system_module = AtomTermAdjust(Yap_heap_regs->system_module);
  Yap_heap_regs->readutil_module = AtomTermAdjust(Yap_heap_regs->readutil_module);
  if (Yap_heap_regs->file_aliases != NULL) {
    Yap_heap_regs->yap_streams =
      (struct stream_desc *)AddrAdjust((ADDR)Yap_heap_regs->yap_streams);
  }
  if (Yap_heap_regs->file_aliases != NULL) {
    Yap_heap_regs->file_aliases =
      (struct AliasDescS *)AddrAdjust((ADDR)Yap_heap_regs->file_aliases);
  }
  Yap_heap_regs->yap_lib_dir =
    (char *)AddrAdjust((ADDR)Yap_heap_regs->yap_lib_dir);
  Yap_heap_regs->pred_goal_expansion =
    (PredEntry *)AddrAdjust((ADDR)Yap_heap_regs->pred_goal_expansion);
  Yap_heap_regs->pred_meta_call =
    (PredEntry *)AddrAdjust((ADDR)Yap_heap_regs->pred_meta_call);
  Yap_heap_regs->pred_dollar_catch =
    (PredEntry *)AddrAdjust((ADDR)Yap_heap_regs->pred_dollar_catch);
  Yap_heap_regs->pred_recorded_with_key =
    (PredEntry *)AddrAdjust((ADDR)Yap_heap_regs->pred_recorded_with_key);
  Yap_heap_regs->pred_log_upd_clause =
    (PredEntry *)AddrAdjust((ADDR)Yap_heap_regs->pred_log_upd_clause);
  Yap_heap_regs->pred_log_upd_clause0 =
    (PredEntry *)AddrAdjust((ADDR)Yap_heap_regs->pred_log_upd_clause0);
  Yap_heap_regs->pred_static_clause =
    (PredEntry *)AddrAdjust((ADDR)Yap_heap_regs->pred_static_clause);
  Yap_heap_regs->pred_throw =
    (PredEntry *)AddrAdjust((ADDR)Yap_heap_regs->pred_throw);
  Yap_heap_regs->pred_handle_throw =
    (PredEntry *)AddrAdjust((ADDR)Yap_heap_regs->pred_handle_throw);
#if DEBUG
  if (Yap_heap_regs->db_erased_list) {
    Yap_heap_regs->db_erased_list = 
      PtoLUCAdjust(Yap_heap_regs->db_erased_list);
  }
  if (Yap_heap_regs->db_erased_ilist) {
    Yap_heap_regs->db_erased_ilist = 
      LUIndexAdjust(Yap_heap_regs->db_erased_ilist);
  }
#endif
  if (Yap_heap_regs->undef_code != NULL)
    Yap_heap_regs->undef_code = (PredEntry *)PtoHeapCellAdjust((CELL *)(Yap_heap_regs->undef_code));
  if (Yap_heap_regs->creep_code != NULL)
    Yap_heap_regs->creep_code = (PredEntry *)PtoHeapCellAdjust((CELL *)(Yap_heap_regs->creep_code));
  if (Yap_heap_regs->spy_code != NULL)
    Yap_heap_regs->spy_code = (PredEntry *)PtoHeapCellAdjust((CELL *)(Yap_heap_regs->spy_code));
#if !defined(THREADS)
  if (Yap_heap_regs->wl.scratchpad.ptr) {
    Yap_heap_regs->wl.scratchpad.ptr =
      (char *)AddrAdjust((ADDR)Yap_heap_regs->wl.scratchpad.ptr);
  }
  Yap_heap_regs->wl.gc_generation = 
    AbsAppl(PtoGloAdjust(RepAppl(Yap_heap_regs->wl.gc_generation)));
  Yap_heap_regs->wl.gc_phase = 
    AbsAppl(PtoGloAdjust(RepAppl(Yap_heap_regs->wl.gc_phase)));
  /* current phase is an integer */
#endif
#ifdef COROUTINING
  if (Yap_heap_regs->wake_up_code != NULL)
    Yap_heap_regs->wake_up_code = (PredEntry *)PtoHeapCellAdjust((CELL *)(Yap_heap_regs->wake_up_code));
#if !defined(THREADS)
  Yap_heap_regs->wl.atts_mutable_list =
    AbsAppl(PtoGloAdjust(RepAppl(Yap_heap_regs->wl.atts_mutable_list)));
  if (Yap_heap_regs->wl.dynamic_arrays) {
    Yap_heap_regs->wl.dynamic_arrays =
      PtoArrayEAdjust(Yap_heap_regs->wl.dynamic_arrays);
  }
  if (Yap_heap_regs->wl.static_arrays) {
    Yap_heap_regs->wl.static_arrays =
      PtoArraySAdjust(Yap_heap_regs->wl.static_arrays);
  }
  if (Yap_heap_regs->wl.global_variables) {
    Yap_heap_regs->wl.global_variables =
      PtoGlobalEAdjust(Yap_heap_regs->wl.global_variables);
  }
  if (Yap_heap_regs->wl.global_arena) {
    if (IsAtomTerm(Yap_heap_regs->wl.global_arena)) {
      Yap_heap_regs->wl.global_arena =
	AtomTermAdjust(Yap_heap_regs->wl.global_arena);
    } else {
      Yap_heap_regs->wl.global_arena =
	AbsAppl(PtoGloAdjust(RepAppl(Yap_heap_regs->wl.global_arena)));
    }
  }
  if (Yap_heap_regs->wl.global_delay_arena) {
    Yap_heap_regs->wl.global_delay_arena =
      GlobalAdjust(Yap_heap_regs->wl.global_delay_arena);
  }
#endif
#endif
  if (Yap_heap_regs->last_wtime != NULL)
    Yap_heap_regs->last_wtime = (void *)PtoHeapCellAdjust((CELL *)(Yap_heap_regs->last_wtime));
  Yap_heap_regs->db_erased_marker =
    DBRefAdjust(Yap_heap_regs->db_erased_marker);
  Yap_heap_regs->logdb_erased_marker =
    PtoLUCAdjust(Yap_heap_regs->logdb_erased_marker);
  Yap_heap_regs->logdb_erased_marker->Id = FunctorDBRef;
  Yap_heap_regs->logdb_erased_marker->ClCode->opc = Yap_opcode(_op_fail);
  Yap_heap_regs->logdb_erased_marker->ClPred = 
    PtoPredAdjust(Yap_heap_regs->logdb_erased_marker->ClPred);
  Yap_heap_regs->hash_chain = 
    (AtomHashEntry *)PtoHeapCellAdjust((CELL *)(Yap_heap_regs->hash_chain));
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
  RestoreDBTerm(&(dbr->DBT));
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
RestoreBB(BlackBoardEntry *pp)
{
  Term t = pp->Element;
  if (t) {
    if (!IsVarTerm(t) && !IsAtomicTerm(t)) {
      RestoreLUClause((LogUpdClause *)DBRefOfTerm(t),NULL);
    }
  }
  pp->KeyOfBB = AtomAdjust(pp->KeyOfBB);
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
	    RestoreDBTerm(db);
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
	    RestoreDBTerm(reg);
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
      if (pp->PredFlags & AtomDBPredFlag) {
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
  if (pp->ModuleOfPred != IDB_MODULE) {
    if (pp->src.OwnerFile && pp->ModuleOfPred != IDB_MODULE)
      pp->src.OwnerFile = AtomAdjust(pp->src.OwnerFile);
  }
  if (!(pp->PredFlags & NumberDBPredFlag)) {
    pp->src.OwnerFile = AtomAdjust(pp->src.OwnerFile);
  }
  pp->OpcodeOfPred = Yap_opcode(Yap_op_from_opcode(pp->OpcodeOfPred));
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
    if (pp->NextPredOfModule)
      pp->NextPredOfModule = PtoPredAdjust(pp->NextPredOfModule);
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
	CleanLUIndex(ClauseCodeToLogUpdIndex(pp->cs.p_code.TrueCodeOfPred));
      } else {
	CleanSIndex(ClauseCodeToStaticIndex(pp->cs.p_code.TrueCodeOfPred));
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
RestoreEntries(PropEntry *pp)
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
	while (!EndOfPAEntr(p0)) {
	  CleanCode(RepPredProp(p0));
	  p0 = RepPredProp(p0)->NextOfPE =
	    PropAdjust(RepPredProp(p0)->NextOfPE);
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
	if (HDiff)
	  RestoreDB(de);
      }
      break;
    case BBProperty:
      {
	BlackBoardEntry *bb = (BlackBoardEntry *) pp;
	bb->NextOfPE =
	  PropAdjust(bb->NextOfPE);
	if (HDiff)
	  RestoreBB(bb);
      }
      break;
    case OpProperty:
      {
	OpEntry *opp = (OpEntry *)pp;
	if (opp->OpModule) {
	 opp->OpModule = AtomTermAdjust(opp->OpModule);
	}
      }
    case ExpProperty:
    case ModProperty:
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

