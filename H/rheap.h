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
* File:		rheap.c							 *
* Last rev:								 *
* mods:									 *
* comments:	walk through heap code					 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "@(#)rheap.c	1.3 3/15/90";
#endif

#define Atomics		0
#define Funcs		1

#if DEBUG_RESTORE2
static char *op_names[_std_top + 1] =
{
#define OPCODE(OP,TYPE) #OP
#include "YapOpcodes.h"
#undef  OPCODE
};
#endif /* DEBUG_RESTORE2 */


/* Now, everything on its place so you must adjust the pointers */

/* restore the failcodes */
static void 
restore_codes(void)
{
  heap_regs->heap_top = AddrAdjust(OldHeapTop);
#ifdef YAPOR
  heap_regs->getworkfirsttimecode.opc = Yap_opcode(_getwork_first_time);
  heap_regs->getworkcode.opc = Yap_opcode(_getwork);
  INIT_YAMOP_LTT(&(heap_regs->getworkcode), 0);
  heap_regs->getworkcode_seq.opc = Yap_opcode(_getwork_seq);
  INIT_YAMOP_LTT(&(heap_regs->getworkcode_seq), 0);
#endif /* YAPOR */
#ifdef TABLING
  heap_regs->tablecompletioncode.opc = Yap_opcode(_table_completion);
  heap_regs->tableanswerresolutioncode.opc = Yap_opcode(_table_answer_resolution);
#ifdef YAPOR
  INIT_YAMOP_LTT(&(heap_regs->tablecompletioncode), 0);
  INIT_YAMOP_LTT(&(heap_regs->tableanswerresolutioncode), 0);
#endif /* YAPOR */
#endif /* TABLING */
  heap_regs->expand_op_code = Yap_opcode(_expand_index);
  heap_regs->failcode->opc = Yap_opcode(_op_fail);
  heap_regs->failcode_1 = Yap_opcode(_op_fail);
  heap_regs->failcode_2 = Yap_opcode(_op_fail);
  heap_regs->failcode_3 = Yap_opcode(_op_fail);
  heap_regs->failcode_4 = Yap_opcode(_op_fail);
  heap_regs->failcode_5 = Yap_opcode(_op_fail);
  heap_regs->failcode_6 = Yap_opcode(_op_fail);

  heap_regs->env_for_trustfail_code.op = Yap_opcode(_call);
  heap_regs->trustfailcode->opc = Yap_opcode(_trust_fail);

  heap_regs->env_for_yes_code.op = Yap_opcode(_call);
  heap_regs->yescode->opc = Yap_opcode(_Ystop);
  heap_regs->undef_op = Yap_opcode(_undef_p);
  heap_regs->index_op = Yap_opcode(_index_pred);
  heap_regs->fail_op = Yap_opcode(_op_fail);
  heap_regs->nocode->opc = Yap_opcode(_Nstop);
#ifdef YAPOR
  INIT_YAMOP_LTT(&(heap_regs->nocode), 1);
  INIT_YAMOP_LTT(&(heap_regs->rtrycode), 1);
#endif /* YAPOR */
  ((yamop *)(&heap_regs->rtrycode))->opc = Yap_opcode(_retry_and_mark);
  if (((yamop *)(&heap_regs->rtrycode))->u.ld.d != NIL)
    ((yamop *)(&heap_regs->rtrycode))->u.ld.d =
      PtoOpAdjust(((yamop *)(&heap_regs->rtrycode))->u.ld.d);
  {
    int             arity;
    arity = heap_regs->clausecode->arity;
    if (heap_regs->clausecode->clause != NIL)
      heap_regs->clausecode->clause =
	PtoOpAdjust(heap_regs->clausecode->clause);
    if (arity) {
      heap_regs->clausecode->func =
	FuncAdjust(heap_regs->clausecode->func); 
    } else {
      /* an atom */
      heap_regs->clausecode->func =
	(Functor)AtomAdjust((Atom)(heap_regs->clausecode->func));
    }
  }
  /* restore consult stack. It consists of heap pointers, so it
     is easy to fix.
  */
  heap_regs->consultlow  = 
    ConsultObjAdjust(heap_regs->consultlow);
  heap_regs->consultbase =
    ConsultObjAdjust(heap_regs->consultbase);
  heap_regs->consultsp   =
    ConsultObjAdjust(heap_regs->consultsp);
  {
    /* we assume all pointers have the same size */
    register consult_obj *pt = heap_regs->consultsp;
    while (pt <
	   heap_regs->consultlow+heap_regs->consultcapacity) {
      pt->p = PropAdjust(pt->p);
      pt ++;
    }
  }
#if USE_THREADED_CODE
  heap_regs->op_rtable = (opentry *)
    CodeAddrAdjust((CODEADDR)(heap_regs->op_rtable));
#endif
  if (heap_regs->atprompt != NIL) {
    heap_regs->atprompt =
      AtomAdjust(heap_regs->atprompt);
  }
  if (heap_regs->char_conversion_table != NULL) {
    heap_regs->char_conversion_table = (char *)
      AddrAdjust((ADDR)heap_regs->char_conversion_table);
  }
  if (heap_regs->char_conversion_table2 != NULL) {
    heap_regs->char_conversion_table2 = (char *)
      AddrAdjust((ADDR)heap_regs->char_conversion_table2);
  }
  if (heap_regs->dead_clauses != NULL) {
    heap_regs->dead_clauses = (DeadClause *)
      AddrAdjust((ADDR)(heap_regs->dead_clauses));
  }
  heap_regs->retry_recorded_k_code =
    PtoOpAdjust(heap_regs->retry_recorded_k_code);
  heap_regs->retry_c_recordedp_code =
    PtoOpAdjust(heap_regs->retry_c_recordedp_code);
  if (heap_regs->IntKeys != NULL) {
    heap_regs->IntKeys = (Prop *)AddrAdjust((ADDR)(heap_regs->IntKeys));
    {
      UInt i;
      for (i = 0; i < heap_regs->int_keys_size; i++) {
	if (heap_regs->IntKeys[i] != NIL) {
	  Prop p0 = heap_regs->IntKeys[i] = PropAdjust(heap_regs->IntKeys[i]);
	  RestoreEntries(RepProp(p0));
	}
      }
    }
  }
  if (heap_regs->IntBBKeys != NULL) {
    heap_regs->IntBBKeys = (Prop *)AddrAdjust((ADDR)(heap_regs->IntBBKeys));
    {
      UInt i;
      for (i = 0; i < heap_regs->int_bb_keys_size; i++) {
	if (heap_regs->IntBBKeys[i] != NIL) {
	  Prop p0 = heap_regs->IntBBKeys[i] = PropAdjust(heap_regs->IntBBKeys[i]);
	  RestoreEntries(RepProp(p0));
	}
      }
    }
  }
  {
    /* adjust atoms in atom table */
    unsigned int i = 0;

    for (i = 0; i < heap_regs->no_of_modules; i++) {
      heap_regs->module_name[i] = AtomTermAdjust(heap_regs->module_name[i]);
      if (heap_regs->module_pred[i]) {
	heap_regs->module_pred[i] = PtoPredAdjust(heap_regs->module_pred[i]);
      }
    }
  }
  heap_regs->atom_abol = AtomAdjust(heap_regs->atom_abol);
  heap_regs->atom_append = AtomAdjust(heap_regs->atom_append);
  heap_regs->atom_array = AtomAdjust(heap_regs->atom_array);
  heap_regs->atom_assert = AtomAdjust(heap_regs->atom_assert);
  heap_regs->atom_alarm = AtomAdjust(heap_regs->atom_alarm);
  heap_regs->atom_b = AtomAdjust(heap_regs->atom_b);
  heap_regs->atom_break = AtomAdjust(heap_regs->atom_break);
  heap_regs->atom_call = AtomAdjust(heap_regs->atom_call);
  heap_regs->atom_catch = AtomAdjust(heap_regs->atom_catch);
  heap_regs->atom_comma = AtomAdjust(heap_regs->atom_comma);
  heap_regs->atom_cpu_time = AtomAdjust(heap_regs->atom_cpu_time);
  heap_regs->atom_csult = AtomAdjust(heap_regs->atom_csult);
  heap_regs->atom_cut = AtomAdjust(heap_regs->atom_cut);
  heap_regs->atom_cut_by = AtomAdjust(heap_regs->atom_cut_by);
#ifdef EUROTRA
#ifdef SFUNC
  heap_regs->atom_dollar_undef = AtomAdjust(heap_regs->atom_dollar_undef);
#endif
#endif
  heap_regs->atom_e = AtomAdjust(heap_regs->atom_e);
  heap_regs->atom_e_q = AtomAdjust(heap_regs->atom_e_q);
  heap_regs->atom_eof = AtomAdjust(heap_regs->atom_eof);
#ifdef EUROTRA
  heap_regs->atom_f_b = AtomAdjust(heap_regs->atom_f_b);
#endif
  heap_regs->atom_fail = AtomAdjust(heap_regs->atom_fail);
  heap_regs->atom_false = AtomAdjust(heap_regs->atom_false);
  heap_regs->atom_fast = AtomAdjust(heap_regs->atom_fast);
  heap_regs->atom_g_t = AtomAdjust(heap_regs->atom_g_t);
  heap_regs->atom_gc = AtomAdjust(heap_regs->atom_gc);
  heap_regs->atom_gc_margin = AtomAdjust(heap_regs->atom_gc_margin);
  heap_regs->atom_gc_trace = AtomAdjust(heap_regs->atom_gc_trace);
  heap_regs->atom_gc_verbose = AtomAdjust(heap_regs->atom_gc_verbose);
  heap_regs->atom_gc_very_verbose = AtomAdjust(heap_regs->atom_gc_very_verbose);
  heap_regs->atom_global = AtomAdjust(heap_regs->atom_global);
  heap_regs->atom_heap_used = AtomAdjust(heap_regs->atom_heap_used);
  heap_regs->atom_inf = AtomAdjust(heap_regs->atom_inf);
  heap_regs->atom_l_t = AtomAdjust(heap_regs->atom_l_t);
  heap_regs->atom_local = AtomAdjust(heap_regs->atom_local);
  heap_regs->atom_meta_call = AtomAdjust(heap_regs->atom_meta_call);
  heap_regs->atom_minus = AtomAdjust(heap_regs->atom_minus);
  heap_regs->atom_nan = AtomAdjust(heap_regs->atom_nan);
  heap_regs->atom_otherwise = AtomAdjust(heap_regs->atom_otherwise);
  heap_regs->atom_pi = AtomAdjust(heap_regs->atom_pi);
  heap_regs->atom_plus = AtomAdjust(heap_regs->atom_plus);
  heap_regs->atom_portray = AtomAdjust(heap_regs->atom_portray);
  heap_regs->atom_profile = AtomAdjust(heap_regs->atom_profile);
  heap_regs->atom_random = AtomAdjust(heap_regs->atom_random);
  heap_regs->atom_read = AtomAdjust(heap_regs->atom_read);
  heap_regs->atom_repeat = AtomAdjust(heap_regs->atom_repeat);
  heap_regs->atom_restore_regs = AtomAdjust(heap_regs->atom_restore_regs);
#if HAVE_SIGACTION
  heap_regs->atom_sig_pending = AtomAdjust(heap_regs->atom_sig_pending);
#endif
  heap_regs->atom_stack_free = AtomAdjust(heap_regs->atom_stack_free);
  heap_regs->atom_true = AtomAdjust(heap_regs->atom_true);
  heap_regs->atom_user = AtomAdjust(heap_regs->atom_user);
  heap_regs->atom_usr_err = AtomAdjust(heap_regs->atom_usr_err);
  heap_regs->atom_usr_in = AtomAdjust(heap_regs->atom_usr_in);
  heap_regs->atom_usr_out = AtomAdjust(heap_regs->atom_usr_out);
  heap_regs->atom_version_number = AtomAdjust(heap_regs->atom_version_number);
  heap_regs->atom_write = AtomAdjust(heap_regs->atom_write);
#ifdef   USE_SOCKET
  heap_regs->functor_af_inet = FuncAdjust(heap_regs->functor_af_inet);
  heap_regs->functor_af_local = FuncAdjust(heap_regs->functor_af_local);
  heap_regs->functor_af_unix = FuncAdjust(heap_regs->functor_af_unix);
#endif
  heap_regs->functor_alt_not = FuncAdjust(heap_regs->functor_alt_not);
  heap_regs->functor_arrow = FuncAdjust(heap_regs->functor_arrow);
  heap_regs->functor_assert = FuncAdjust(heap_regs->functor_assert);
#ifdef COROUTINING
  heap_regs->functor_att_goal = FuncAdjust(heap_regs->functor_att_goal);
#endif
  heap_regs->functor_braces = FuncAdjust(heap_regs->functor_braces);
  heap_regs->functor_call = FuncAdjust(heap_regs->functor_call);
  heap_regs->functor_cut_by = FuncAdjust(heap_regs->functor_cut_by);
  heap_regs->functor_comma = FuncAdjust(heap_regs->functor_comma);
  heap_regs->functor_creep = FuncAdjust(heap_regs->functor_creep);
  heap_regs->functor_csult = FuncAdjust(heap_regs->functor_csult);
  heap_regs->functor_eq = FuncAdjust(heap_regs->functor_eq);
  heap_regs->functor_execute_in_mod = FuncAdjust(heap_regs->functor_execute_in_mod);
  heap_regs->functor_execute_within = FuncAdjust(heap_regs->functor_execute_within);
  heap_regs->functor_g_atom = FuncAdjust(heap_regs->functor_g_atom);
  heap_regs->functor_g_atomic = FuncAdjust(heap_regs->functor_g_atomic);
  heap_regs->functor_g_compound = FuncAdjust(heap_regs->functor_g_compound);
  heap_regs->functor_g_float = FuncAdjust(heap_regs->functor_g_float);
  heap_regs->functor_g_integer = FuncAdjust(heap_regs->functor_g_integer);
  heap_regs->functor_g_number = FuncAdjust(heap_regs->functor_g_number);
  heap_regs->functor_g_primitive = FuncAdjust(heap_regs->functor_g_primitive);
  heap_regs->functor_g_var = FuncAdjust(heap_regs->functor_g_var);
  heap_regs->functor_last_execute_within = FuncAdjust(heap_regs->functor_last_execute_within);
  heap_regs->functor_list = FuncAdjust(heap_regs->functor_list);
  heap_regs->functor_module = FuncAdjust(heap_regs->functor_module);
#ifdef MULTI_ASSIGNMENT_VARIABLES
  heap_regs->functor_mutable = FuncAdjust(heap_regs->functor_mutable);
#endif
  heap_regs->functor_not = FuncAdjust(heap_regs->functor_not);
  heap_regs->functor_or = FuncAdjust(heap_regs->functor_or);
  heap_regs->functor_portray = FuncAdjust(heap_regs->functor_portray);
  heap_regs->functor_query = FuncAdjust(heap_regs->functor_query);
  heap_regs->functor_stream = FuncAdjust(heap_regs->functor_stream);
  heap_regs->functor_stream_pos = FuncAdjust(heap_regs->functor_stream_pos);
  heap_regs->functor_stream_eOS = FuncAdjust(heap_regs->functor_stream_eOS);
  heap_regs->functor_change_module = FuncAdjust(heap_regs->functor_change_module);
  heap_regs->functor_current_module = FuncAdjust(heap_regs->functor_current_module);
  heap_regs->functor_u_minus = FuncAdjust(heap_regs->functor_u_minus);
  heap_regs->functor_u_plus = FuncAdjust(heap_regs->functor_u_plus);
  heap_regs->functor_v_bar = FuncAdjust(heap_regs->functor_v_bar);
  heap_regs->functor_var = FuncAdjust(heap_regs->functor_var);
#ifdef EUROTRA
  heap_regs->term_dollar_u = AtomTermAdjust(heap_regs->term_dollar_u);
#endif
  heap_regs->term_prolog = AtomTermAdjust(heap_regs->term_prolog);
  heap_regs->term_refound_var = AtomTermAdjust(heap_regs->term_refound_var);
  if (heap_regs->dyn_array_list != NULL) {
    heap_regs->dyn_array_list =
      (struct array_entry *)AddrAdjust((ADDR)heap_regs->dyn_array_list);
  }
  if (heap_regs->file_aliases != NULL) {
    heap_regs->yap_streams =
      (struct stream_desc *)AddrAdjust((ADDR)heap_regs->yap_streams);
  }
  if (heap_regs->file_aliases != NULL) {
    heap_regs->file_aliases =
      (struct AliasDescS *)AddrAdjust((ADDR)heap_regs->file_aliases);
  }
  heap_regs->yap_lib_dir =
    (char *)AddrAdjust((ADDR)heap_regs->yap_lib_dir);
  heap_regs->pred_goal_expansion =
    (PredEntry *)AddrAdjust((ADDR)heap_regs->pred_goal_expansion);
  heap_regs->pred_meta_call =
    (PredEntry *)AddrAdjust((ADDR)heap_regs->pred_meta_call);
  heap_regs->pred_dollar_catch =
    (PredEntry *)AddrAdjust((ADDR)heap_regs->pred_dollar_catch);
  heap_regs->pred_recorded_with_key =
    (PredEntry *)AddrAdjust((ADDR)heap_regs->pred_recorded_with_key);
  heap_regs->pred_log_upd_clause =
    (PredEntry *)AddrAdjust((ADDR)heap_regs->pred_log_upd_clause);
  heap_regs->pred_log_upd_clause0 =
    (PredEntry *)AddrAdjust((ADDR)heap_regs->pred_log_upd_clause0);
  heap_regs->pred_static_clause =
    (PredEntry *)AddrAdjust((ADDR)heap_regs->pred_static_clause);
  heap_regs->pred_throw =
    (PredEntry *)AddrAdjust((ADDR)heap_regs->pred_throw);
  heap_regs->pred_handle_throw =
    (PredEntry *)AddrAdjust((ADDR)heap_regs->pred_handle_throw);
  if (heap_regs->dyn_array_list != NULL)
    heap_regs->dyn_array_list = PtoArrayEAdjust(heap_regs->dyn_array_list);
  if (heap_regs->undef_code != NULL)
    heap_regs->undef_code = (PredEntry *)PtoHeapCellAdjust((CELL *)(heap_regs->undef_code));
  if (heap_regs->creep_code != NULL)
    heap_regs->creep_code = (PredEntry *)PtoHeapCellAdjust((CELL *)(heap_regs->creep_code));
  if (heap_regs->spy_code != NULL)
    heap_regs->spy_code = (PredEntry *)PtoHeapCellAdjust((CELL *)(heap_regs->spy_code));
#ifdef COROUTINING
  if (heap_regs->wake_up_code != NULL)
    heap_regs->wake_up_code = (PredEntry *)PtoHeapCellAdjust((CELL *)(heap_regs->wake_up_code));
#if !defined(THREADS)
  heap_regs->wl.mutable_list =
    AbsAppl(PtoGloAdjust(RepAppl(heap_regs->wl.mutable_list)));
  heap_regs->wl.atts_mutable_list =
    AbsAppl(PtoGloAdjust(RepAppl(heap_regs->wl.atts_mutable_list)));
#endif
#endif
  if (heap_regs->last_wtime != NULL)
    heap_regs->last_wtime = (void *)PtoHeapCellAdjust((CELL *)(heap_regs->last_wtime));
  heap_regs->db_erased_marker =
    DBRefAdjust(heap_regs->db_erased_marker);
  heap_regs->hash_chain = 
    (AtomHashEntry *)PtoHeapCellAdjust((CELL *)(heap_regs->hash_chain));
}


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
    if (p > p_base) {
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
    if (p > p_base) {
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
  dbr->Parent = (DBProp)AddrAdjust((ADDR)(dbr->Parent));
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
  if (pp->KindOfPE & LogUpdDBBit) {
    dbr = pp->First;
    /* While we have something in the data base, restore it */
    while (dbr) {
      RestoreDBEntry(dbr);
      dbr = dbr->Next;
    }
  } else {
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
}

/* Restores a DB structure, as it was saved in the heap */
static void 
RestoreBB(BlackBoardEntry *pp)
{
  if (pp->Element) {
    pp->Element = DBTermAdjust(pp->Element);
    RestoreDBTerm(pp->Element);
  }
  pp->KeyOfBB = AtomAdjust(pp->KeyOfBB);
}

static void 
restore_opcodes(yamop *pc)
{
  do {
    op_numbers op = Yap_op_from_opcode(pc->opc);
    pc->opc = Yap_opcode(op);
#ifdef DEBUG_RESTORE2
    fprintf(stderr, "%s ", op_names[op]);
#endif
    switch (op) {
    case _Ystop:
    case _Nstop:
#ifdef DEBUG_RESTORE2
      fprintf(stderr, "OK\n");
#endif
      return;
      /* instructions type ld */
    case _try_me:
    case _retry_me:
    case _trust_me:
    case _profiled_retry_me:
    case _profiled_trust_me:
    case _count_retry_me:
    case _count_trust_me:
    case _try_me0:
    case _retry_me0:
    case _trust_me0:
    case _try_me1:
    case _retry_me1:
    case _trust_me1:
    case _try_me2:
    case _retry_me2:
    case _trust_me2:
    case _try_me3:
    case _retry_me3:
    case _trust_me3:
    case _try_me4:
    case _retry_me4:
    case _trust_me4:
    case _spy_or_trymark:
    case _try_and_mark:
    case _profiled_retry_and_mark:
    case _count_retry_and_mark:
    case _retry_and_mark:
    case _try_clause:
    case _retry:
    case _retry_killed:
    case _trust:
    case _trust_killed:
#ifdef YAPOR
    case _getwork:
    case _getwork_seq:
    case _sync:
#endif
#ifdef TABLING
    case _table_try_single:
    case _table_try_me:
    case _table_retry_me:
    case _table_trust_me:
    case _table_try:
    case _table_retry:
    case _table_trust:
    case _table_answer_resolution:
    case _table_completion:
#endif /* TABLING */
      pc->u.ld.p = PtoPredAdjust(pc->u.ld.p);
      pc->u.ld.d = PtoOpAdjust(pc->u.ld.d);
      pc = NEXTOP(pc,ld);
      break;
    case _enter_lu_pred:
    case _stale_lu_index:
      pc->u.Ill.I = (LogUpdIndex *)PtoOpAdjust((yamop *)(pc->u.Ill.I));
      pc->u.Ill.l1 = PtoOpAdjust(pc->u.Ill.l1);
      pc->u.Ill.l2 = PtoOpAdjust(pc->u.Ill.l2);
      pc = NEXTOP(pc,Ill);
      break;
      /* instructions type l */
    case _enter_profiling:
    case _retry_profiled:
    case _lock_lu:
    case _count_call:
    case _count_retry:
    case _execute:
      pc->u.p.p = PtoPredAdjust(pc->u.p.p);
      pc = NEXTOP(pc,p);
      break;
    case _trust_logical_pred:
    case _dexecute:
    case _jump:
    case _move_back:
    case _skip:
    case _jump_if_var:
    case _try_in:
      pc->u.l.l = PtoOpAdjust(pc->u.l.l);
      pc = NEXTOP(pc,l);
      break;
      /* instructions type EC */
    case _jump_if_nonvar:
      pc->u.xl.l = PtoOpAdjust(pc->u.xl.l);
      pc->u.xl.x = XAdjust(pc->u.xl.x);
      pc = NEXTOP(pc,xl);
      break;
      /* instructions type EC */
    case _alloc_for_logical_pred:
      pc->u.EC.ClBase = PtoOpAdjust(pc->u.EC.ClBase);
      pc = NEXTOP(pc,EC);
      break;
      /* instructions type e */
    case _unify_idb_term:
    case _copy_idb_term:
      /* don't need no _Ystop to know we're done */
      return;
    case _trust_fail:
    case _op_fail:
    case _cut:
    case _cut_t:
    case _cut_e:
    case _procceed:
    case _allocate:
    case _deallocate:
    case _write_void:
    case _write_list:
    case _write_l_list:
#if !defined(YAPOR)
    case _or_last:
#endif
    case _pop:
    case _index_pred:
#if THREADS
    case _thread_local:
#endif
    case _expand_index:
    case _undef_p:
    case _spy_pred:
    case _p_equal:
    case _p_dif:
    case _p_eq:
    case _p_functor:
    case _p_execute_tail:
    case _enter_a_profiling:
    case _count_a_call:
    case _index_dbref:
    case _index_blob:
#ifdef YAPOR
    case _getwork_first_time:
#endif
#ifdef TABLING
    case _trie_do_var:
    case _trie_trust_var:
    case _trie_try_var:
    case _trie_retry_var:
    case _trie_do_val:
    case _trie_trust_val:
    case _trie_try_val:
    case _trie_retry_val:
    case _trie_do_atom:
    case _trie_trust_atom:
    case _trie_try_atom:
    case _trie_retry_atom:
    case _trie_do_list:
    case _trie_trust_list:
    case _trie_try_list:
    case _trie_retry_list:
    case _trie_do_struct:
    case _trie_trust_struct:
    case _trie_try_struct:
    case _trie_retry_struct:
#endif /* TABLING */
#ifdef TABLING_INNER_CUTS
    case _clause_with_cut:
#endif /* TABLING_INNER_CUTS */
      pc = NEXTOP(pc,e);
      break;
      /* instructions type x */
    case _save_b_x:
    case _commit_b_x:
    case _get_list:
    case _put_list:
    case _write_x_var:
    case _write_x_val:
    case _write_x_loc:
    case _p_atom_x:
    case _p_atomic_x:
    case _p_integer_x:
    case _p_nonvar_x:
    case _p_number_x:
    case _p_var_x:
    case _p_db_ref_x:
    case _p_primitive_x:
    case _p_compound_x:
    case _p_float_x:
    case _p_cut_by_x:
      pc->u.x.x = XAdjust(pc->u.x.x);
      pc = NEXTOP(pc,x);
      break;
      /* instructions type y */
    case _save_b_y:
    case _commit_b_y:
    case _write_y_var:
    case _write_y_val: 
    case _write_y_loc:
    case _p_atom_y:
    case _p_atomic_y:
    case _p_integer_y:
    case _p_nonvar_y:
    case _p_number_y:
    case _p_var_y:
    case _p_db_ref_y:
    case _p_primitive_y:
    case _p_compound_y:
    case _p_float_y:
    case _p_cut_by_y:
      pc->u.y.y = YAdjust(pc->u.y.y);
      pc = NEXTOP(pc,y);
      break;
      /* instructions type sla */      
    case _p_execute:
      goto sla_full;
    case _fcall:
    case _call:
#ifdef YAPOR
    case _or_last:
#endif
      pc->u.sla.sla_u.p = PtoPredAdjust(pc->u.sla.sla_u.p);
    sla_full:
      if (pc->u.sla.bmap != NULL) {
	pc->u.sla.bmap = CellPtoHeapAdjust(pc->u.sla.bmap);
      }
      pc->u.sla.p0 = PtoPredAdjust(pc->u.sla.p0);
      pc = NEXTOP(pc,sla);
      break;
      /* instructions type sla, but for disjunctions */
    case _either:
    case _or_else:
      if (pc->u.sla.bmap != NULL) {
	pc->u.sla.bmap = CellPtoHeapAdjust(pc->u.sla.bmap);
      }
      pc->u.sla.sla_u.l = PtoOpAdjust(pc->u.sla.sla_u.l);
      pc->u.sla.p0 = PtoPredAdjust(pc->u.sla.p0);
      pc = NEXTOP(pc,sla);
      break;
      /* instructions type sla, but for functions */
    case _call_cpred:
    case _call_usercpred:
      pc->u.sla.sla_u.p = PtoPredAdjust(pc->u.sla.sla_u.p);
      pc->u.sla.p0 = PtoPredAdjust(pc->u.sla.p0);
      if (pc->u.sla.bmap != NULL) {
	pc->u.sla.bmap = CellPtoHeapAdjust(pc->u.sla.bmap);
      }
      pc = NEXTOP(pc,sla);
      break;
      /* instructions type xx */
    case _get_x_var:
    case _get_x_val:
    case _glist_valx:
    case _gl_void_varx:
    case _gl_void_valx:
    case _put_x_var:
    case _put_x_val:
      pc->u.xx.xr = XAdjust(pc->u.xx.xr);
      pc->u.xx.xl = XAdjust(pc->u.xx.xl);
      pc = NEXTOP(pc,xx);
      break;
      /* instructions type yx */
    case _get_y_var:
    case _get_y_val:
    case _put_y_var:
    case _put_y_val:
    case _put_unsafe:
      pc->u.yx.x = XAdjust(pc->u.yx.x);
      pc->u.yx.y = YAdjust(pc->u.yx.y);
      pc = NEXTOP(pc,yx);
      break;
      /* instructions type xc */
    case _get_atom:
    case _put_atom:
    case _get_float:
    case _get_longint:
    case _get_bigint:
      pc->u.xc.x = XAdjust(pc->u.xc.x);
      {
	Term t = pc->u.xc.c;
	if (IsAtomTerm(t))
	  pc->u.xc.c = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.xc.c = BlobTermAdjust(t);
      }
      pc = NEXTOP(pc,xc);
      break;
      /* instructions type xf */
    case _get_struct:
    case _put_struct:
      pc->u.xf.x = XAdjust(pc->u.xf.x);
      pc->u.xf.f = FuncAdjust(pc->u.xf.f);
      pc = NEXTOP(pc,xf);
      break;
      /* instructions type xy */
    case _glist_valy:
    case _gl_void_vary:
    case _gl_void_valy:
      pc->u.xy.x = XAdjust(pc->u.xy.x);
      pc->u.xy.y = YAdjust(pc->u.xy.y);
      pc = NEXTOP(pc,xy);
      break;
      /* instructions type ox */
    case _unify_x_var:
    case _unify_x_var_write:
    case _unify_l_x_var:
    case _unify_l_x_var_write:
    case _unify_x_val_write:
    case _unify_x_val:
    case _unify_l_x_val_write:
    case _unify_l_x_val:
    case _unify_x_loc_write:
    case _unify_x_loc:
    case _unify_l_x_loc_write:
    case _unify_l_x_loc:
    case _save_pair_x_write:
    case _save_pair_x:
    case _save_appl_x_write:
    case _save_appl_x:
      pc->u.ox.opcw = Yap_opcode(Yap_op_from_opcode(pc->u.ox.opcw));
      pc->u.ox.x = XAdjust(pc->u.ox.x);
      pc = NEXTOP(pc,ox);
      break;
      /* instructions type oxx */
    case _unify_x_var2:
    case _unify_x_var2_write:
    case _unify_l_x_var2:
    case _unify_l_x_var2_write:
      pc->u.oxx.opcw = Yap_opcode(Yap_op_from_opcode(pc->u.oxx.opcw));
      pc->u.oxx.xl = XAdjust(pc->u.oxx.xl);
      pc->u.oxx.xr = XAdjust(pc->u.oxx.xr);
      pc = NEXTOP(pc,oxx);
      break;
      /* instructions type oy */
    case _unify_y_var:
    case _unify_y_var_write:
    case _unify_l_y_var:
    case _unify_l_y_var_write:
    case _unify_y_val_write:
    case _unify_y_val:
    case _unify_l_y_val_write:
    case _unify_l_y_val:
    case _unify_y_loc_write:
    case _unify_y_loc:
    case _unify_l_y_loc_write:
    case _unify_l_y_loc:
    case _save_pair_y_write:
    case _save_pair_y:
    case _save_appl_y_write:
    case _save_appl_y:
      pc->u.oy.opcw = Yap_opcode(Yap_op_from_opcode(pc->u.oy.opcw));
      pc->u.oy.y = YAdjust(pc->u.oy.y);
      pc = NEXTOP(pc,oy);
      break;
      /* instructions type o */
    case _unify_void_write:
    case _unify_void:
    case _unify_l_void_write:
    case _unify_l_void:
    case _unify_list_write:
    case _unify_list:
    case _unify_l_list_write:
    case _unify_l_list:
      pc->u.o.opcw = Yap_opcode(Yap_op_from_opcode(pc->u.o.opcw));
      pc = NEXTOP(pc,o);
      break;
      /* instructions type os */
    case _unify_n_voids_write:
    case _unify_n_voids:
    case _unify_l_n_voids_write:
    case _unify_l_n_voids:
      pc->u.os.opcw = Yap_opcode(Yap_op_from_opcode(pc->u.os.opcw));
      pc = NEXTOP(pc,os);
      break;
      /* instructions type oc */
    case _unify_atom_write:
    case _unify_atom:
    case _unify_l_atom_write:
    case _unify_l_atom:
    case _unify_float:
    case _unify_l_float:
    case _unify_longint:
    case _unify_l_longint:
    case _unify_bigint:
    case _unify_l_bigint:
      pc->u.oc.opcw = Yap_opcode(Yap_op_from_opcode(pc->u.oc.opcw));
      {
	Term t = pc->u.oc.c;
	if (IsAtomTerm(t))
	    pc->u.oc.c = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.oc.c = BlobTermAdjust(t);
      }
      pc = NEXTOP(pc,oc);
      break;
      /* instructions type osc */
    case _unify_n_atoms_write:
    case _unify_n_atoms:
      pc->u.osc.opcw = Yap_opcode(Yap_op_from_opcode(pc->u.osc.opcw));
      {
	Term t = pc->u.osc.c;
	if (IsAtomTerm(t))
	    pc->u.osc.c = AtomTermAdjust(t);
      }
      pc = NEXTOP(pc,osc);
      break;
      /* instructions type of */
    case _unify_struct_write:
    case _unify_struct:
    case _unify_l_struc_write:
    case _unify_l_struc:
      pc->u.of.opcw = Yap_opcode(Yap_op_from_opcode(pc->u.of.opcw));
      pc->u.of.f = FuncAdjust(pc->u.of.f);
      pc = NEXTOP(pc,of);
      break;
      /* instructions type s */
    case _write_n_voids:
    case _pop_n:
#ifdef TABLING
    case _table_new_answer:
#endif /* TABLING */
      pc = NEXTOP(pc,s);
      break;
      /* instructions type c */
   case _write_atom:
      {
	Term t = pc->u.c.c;
	if (IsAtomTerm(t))
	    pc->u.c.c = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.c.c = BlobTermAdjust(t);
      }
      pc = NEXTOP(pc,c);
      break;
      /* instructions type sc */
   case _write_n_atoms:
      {
	Term t = pc->u.sc.c;
	if (IsAtomTerm(t))
	    pc->u.sc.c = AtomTermAdjust(t);
      }
      pc = NEXTOP(pc,sc);
      break;
      /* instructions type f */
   case _write_struct:
   case _write_l_struc:
      pc->u.f.f = FuncAdjust(pc->u.f.f);
      pc = NEXTOP(pc,f);
      break;
      /* instructions type sdl */
    case _call_c_wfail:
      pc->u.sdl.p = PtoPredAdjust(pc->u.sdl.p);
      pc->u.sdl.l = PtoOpAdjust(pc->u.sdl.l);
      pc = NEXTOP(pc,sdl);
      break;
      /* instructions type lds */
    case _try_c:
    case _try_userc:
      /* don't need to do no nothing here, initstaff will do it for us
	  */
      pc->u.lds.p = PtoPredAdjust(pc->u.lds.p);
      pc = NEXTOP(pc,lds);
      break;
    case _retry_c:
    case _retry_userc:
      /* don't need to do no nothing here, initstaff will do it for us
	 pc->u.lds.d = CCodeAdjust(pc->u.lds.d); */
      pc->u.lds.p = PtoPredAdjust(pc->u.lds.p);
      pc = NEXTOP(pc,lds);
      break;
      /* instructions type llll */
    case _switch_on_type:
      pc->u.llll.l1 = PtoOpAdjust(pc->u.llll.l1);
      pc->u.llll.l2 = PtoOpAdjust(pc->u.llll.l2);
      pc->u.llll.l3 = PtoOpAdjust(pc->u.llll.l3);
      pc->u.llll.l4 = PtoOpAdjust(pc->u.llll.l4);
      pc = NEXTOP(pc,llll);
      break;
      /* instructions type xllll */
    case _switch_list_nl:
      pc->u.ollll.pop = Yap_opcode(Yap_op_from_opcode(pc->u.ollll.pop));
      pc->u.ollll.l1 = PtoOpAdjust(pc->u.llll.l1);
      pc->u.ollll.l2 = PtoOpAdjust(pc->u.llll.l2);
      pc->u.ollll.l3 = PtoOpAdjust(pc->u.llll.l3);
      pc->u.ollll.l4 = PtoOpAdjust(pc->u.llll.l4);
      pc = NEXTOP(pc,ollll);
      break;
      /* instructions type xllll */
    case _switch_on_arg_type:
      pc->u.xllll.x = XAdjust(pc->u.xllll.x);
      pc->u.xllll.l1 = PtoOpAdjust(pc->u.xllll.l1);
      pc->u.xllll.l2 = PtoOpAdjust(pc->u.xllll.l2);
      pc->u.xllll.l3 = PtoOpAdjust(pc->u.xllll.l3);
      pc->u.xllll.l4 = PtoOpAdjust(pc->u.xllll.l4);
      pc = NEXTOP(pc,xllll);
      break;
      /* instructions type sllll */
    case _switch_on_sub_arg_type:
      pc->u.sllll.l1 = PtoOpAdjust(pc->u.sllll.l1);
      pc->u.sllll.l2 = PtoOpAdjust(pc->u.sllll.l2);
      pc->u.sllll.l3 = PtoOpAdjust(pc->u.sllll.l3);
      pc->u.sllll.l4 = PtoOpAdjust(pc->u.sllll.l4);
      pc = NEXTOP(pc,sllll);
      break;
      /* instructions type lll */
    case _if_not_then:
      {
	Term t = pc->u.clll.c;
	if (IsAtomTerm(t))
	    pc->u.clll.c = AtomTermAdjust(t);
      }
      pc->u.clll.l1 = PtoOpAdjust(pc->u.clll.l1);
      pc->u.clll.l2 = PtoOpAdjust(pc->u.clll.l2);
      pc->u.clll.l3 = PtoOpAdjust(pc->u.clll.l3);
      pc = NEXTOP(pc,clll);
      break;
      /* switch_on_func */
    case _switch_on_func:
      {
	int             i, j;
	CELL            *oldcode, *startcode;

	i = pc->u.sl.s;
	startcode = oldcode = (CELL *)(pc->u.sl.l = PtoOpAdjust(pc->u.sl.l));
	for (j = 0; j < i; j++) {
	  Functor oldfunc = (Functor)(oldcode[0]);
	  CODEADDR oldjmp = (CODEADDR)(oldcode[1]);
	  if (oldfunc != NULL) {
	    oldcode[0] = (CELL)FuncAdjust(oldfunc);
	  }
	  oldcode[1] = (CELL)CodeAddrAdjust(oldjmp);
	  oldcode += 2;
	}
	rehash(startcode, i, Funcs);
	pc = NEXTOP(pc,sl);
      }
      break;
      /* switch_on_cons */
    case _switch_on_cons:
      {
	int             i, j;
	CELL            *oldcode;
#if !USE_OFFSETS
	CELL            *startcode;
#endif

	i = pc->u.sl.s;
#if !USE_OFFSETS
	startcode =
#endif
	  oldcode = (CELL *)(pc->u.sl.l = PtoOpAdjust(pc->u.sl.l));
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
	pc = NEXTOP(pc,sl);
      }
      break;
    case _go_on_func:
      {
	CELL *oldcode = (CELL *)(pc->u.sl.l = PtoOpAdjust(pc->u.sl.l));
	Functor oldfunc = (Functor)(oldcode[0]);

	oldcode[0] = (CELL)FuncAdjust(oldfunc);
	oldcode[1] = (CELL)CodeAddrAdjust((CODEADDR)oldcode[1]);
	oldcode[3] = (CELL)CodeAddrAdjust((CODEADDR)oldcode[3]);
      }
      pc = NEXTOP(pc,sl);
      break;
    case _go_on_cons:
      {
	CELL *oldcode = (CELL *)(pc->u.sl.l = PtoOpAdjust(pc->u.sl.l));
	Term oldcons = oldcode[0];

	if (IsAtomTerm(oldcons)) {
	  oldcode[0] = AtomTermAdjust(oldcons);
	}
	oldcode[1] = (CELL)CodeAddrAdjust((CODEADDR)oldcode[1]);
	oldcode[3] = (CELL)CodeAddrAdjust((CODEADDR)oldcode[3]);
      }
      pc = NEXTOP(pc,sl);
      break;
    case _if_func:
      {
	CELL *oldcode = (CELL *)(pc->u.sl.l = PtoOpAdjust(pc->u.sl.l));
	Int j;

	for (j = 0; j < pc->u.sl.s; j++) {
	  Functor oldfunc = (Functor)(oldcode[0]);
	  CODEADDR oldjmp = (CODEADDR)(oldcode[1]);
	  oldcode[0] = (CELL)FuncAdjust(oldfunc);
	  oldcode[1] = (CELL)CodeAddrAdjust(oldjmp);
	  oldcode += 2;
	}
	/* adjust fail code */
	oldcode[1] = (CELL)CodeAddrAdjust((CODEADDR)oldcode[1]);
      }
      pc = NEXTOP(pc,sl);
      break;
    case _if_cons:
      {
	CELL *oldcode = (CELL *)(pc->u.sl.l = PtoOpAdjust(pc->u.sl.l));
	Int j;

	for (j = 0; j < pc->u.sl.s; j++) {
	  Term oldcons = oldcode[0];
	  CODEADDR oldjmp = (CODEADDR)(oldcode[1]);
	  if (IsAtomTerm(oldcons)) {
	    oldcode[0] = (CELL)AtomTermAdjust(oldcons);
	  }
	  oldcode[1] = (CELL)CodeAddrAdjust(oldjmp);
	  oldcode += 2;
	}
	/* adjust fail code */
	oldcode[1] = (CELL)CodeAddrAdjust((CODEADDR)oldcode[1]);
      }
      pc = NEXTOP(pc,sl);
      break;
      /* instructions type xxx */
    case _p_plus_vv:
    case _p_minus_vv:
    case _p_times_vv:
    case _p_div_vv:
    case _p_and_vv:
    case _p_or_vv:
    case _p_sll_vv:
    case _p_slr_vv:
    case _p_arg_vv:
    case _p_func2s_vv:
    case _p_func2f_xx:
      pc->u.xxx.x  = XAdjust(pc->u.xxx.x);
      pc->u.xxx.x1 = XAdjust(pc->u.xxx.x1);
      pc->u.xxx.x2 = XAdjust(pc->u.xxx.x2);
      pc = NEXTOP(pc,xxx);
      break;
      /* instructions type xxc */
    case _p_plus_vc:
    case _p_minus_cv:
    case _p_times_vc:
    case _p_div_cv:
    case _p_and_vc:
    case _p_or_vc:
    case _p_sll_vc:
    case _p_slr_vc:
    case _p_func2s_vc:
      pc->u.xxc.x  = XAdjust(pc->u.xxc.x);
      pc->u.xxc.xi = XAdjust(pc->u.xxc.xi);
      pc = NEXTOP(pc,xxc);
      break;
    case _p_div_vc:
    case _p_sll_cv:
    case _p_slr_cv:
    case _p_arg_cv:
      pc->u.xcx.x  = XAdjust(pc->u.xcx.x);
      pc->u.xcx.xi = XAdjust(pc->u.xcx.xi);
      pc = NEXTOP(pc,xcx);
      break;
    case _p_func2s_cv:
      pc->u.xcx.x  = XAdjust(pc->u.xcx.x);
      if (IsAtomTerm(pc->u.xcx.c))
	pc->u.xcx.c = AtomTermAdjust(pc->u.xcx.c);
      pc->u.xcx.xi = XAdjust(pc->u.xcx.xi);
      pc = NEXTOP(pc,xcx);
      break;
      /* instructions type xyx */
    case _p_func2f_xy:
      pc->u.xyx.x = XAdjust(pc->u.xyx.x);
      pc->u.xyx.x1 = XAdjust(pc->u.xyx.x1);
      pc->u.xyx.y2  = YAdjust(pc->u.xyx.y2);
      pc = NEXTOP(pc,xyx);
      break;
      /* instructions type yxx */
    case _p_plus_y_vv:
    case _p_minus_y_vv:
    case _p_times_y_vv:
    case _p_div_y_vv:
    case _p_and_y_vv:
    case _p_or_y_vv:
    case _p_sll_y_vv:
    case _p_slr_y_vv:
    case _p_arg_y_vv:
    case _p_func2s_y_vv:
    case _p_func2f_yx:
      pc->u.yxx.y  = YAdjust(pc->u.yxx.y);
      pc->u.yxx.x1 = XAdjust(pc->u.yxx.x1);
      pc->u.yxx.x2 = XAdjust(pc->u.yxx.x2);
      pc = NEXTOP(pc,yxx);
      break;
      /* instructions type yyx */
    case _p_func2f_yy:
      pc->u.yyx.y1 = YAdjust(pc->u.yyx.y1);
      pc->u.yyx.y2 = YAdjust(pc->u.yyx.y2);
      pc->u.yyx.x = XAdjust(pc->u.yyx.x);
      pc = NEXTOP(pc,yyx);
      break;
      /* instructions type yxc */
    case _p_plus_y_vc:
    case _p_minus_y_cv:
    case _p_times_y_vc:
    case _p_div_y_vc:
    case _p_div_y_cv:
    case _p_and_y_vc:
    case _p_or_y_vc:
    case _p_sll_y_vc:
    case _p_slr_y_vc:
    case _p_func2s_y_vc:
      pc->u.yxc.y  = YAdjust(pc->u.yxc.y);
      pc->u.yxc.xi = XAdjust(pc->u.yxc.xi);
      pc = NEXTOP(pc,yxc);
      break;
      /* instructions type ycx */
    case _p_sll_y_cv:
    case _p_slr_y_cv:
    case _p_arg_y_cv:
      pc->u.ycx.y  = YAdjust(pc->u.ycx.y);
      pc->u.ycx.xi = XAdjust(pc->u.ycx.xi);
      pc = NEXTOP(pc,ycx);
      break;
      /* instructions type ycx */
    case _p_func2s_y_cv:
      pc->u.ycx.y  = YAdjust(pc->u.ycx.y);
      if (IsAtomTerm(pc->u.ycx.c))
	pc->u.ycx.c  = AtomTermAdjust(pc->u.ycx.c);
      pc->u.ycx.xi = XAdjust(pc->u.ycx.xi);
      pc = NEXTOP(pc,ycx);
      break;
      /* instructions type llxx */
    case _call_bfunc_xx:
      pc->u.llxx.p  = PtoPredAdjust(pc->u.llxx.p);
      pc->u.llxx.f  = PtoOpAdjust(pc->u.llxx.f);
      pc->u.llxx.x1 = XAdjust(pc->u.llxx.x1);
      pc->u.llxx.x2 = XAdjust(pc->u.llxx.x2);
      pc = NEXTOP(pc,llxx);
      break;
      /* instructions type llxy */
    case _call_bfunc_yx:
    case _call_bfunc_xy:
      pc->u.llxy.p = PtoPredAdjust(pc->u.llxy.p);
      pc->u.llxy.f = PtoOpAdjust(pc->u.llxy.f);
      pc->u.llxy.x = XAdjust(pc->u.llxy.x);
      pc->u.llxy.y = YAdjust(pc->u.llxy.y);
      pc = NEXTOP(pc,llxy);
      break;
    case _call_bfunc_yy:
      pc->u.llyy.p  = PtoPredAdjust(pc->u.llyy.p);
      pc->u.llyy.f = PtoOpAdjust(pc->u.llxy.f);
      pc->u.llyy.y1 = YAdjust(pc->u.llyy.y1);
      pc->u.llyy.y2 = YAdjust(pc->u.llyy.y2);
      pc = NEXTOP(pc,llyy);
      break;
    }
  } while (TRUE);
}

/* Restores a prolog clause, in its compiled form */
static void 
RestoreStaticClause(StaticClause *cl, PredEntry *pp)
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
      RestoreStaticClause(cl, pp);
      if (cl->ClCode == Last) return;
      cl = cl->ClNext;
    } while (TRUE);
  }
}


static void 
CleanLUIndex(LogUpdIndex *idx)
{
  idx->ClRefCount = 0;
  INIT_LOCK(idx->ClLock);
  if (idx->ClFlags & SwitchRootMask) {
    idx->u.pred = PtoPredAdjust(idx->u.pred);
  } else {
    idx->u.ParentIndex = LUIndexAdjust(idx->u.ParentIndex);
  }
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
	  } else if (IsOldDelay((CELL)reg)) {
	    *base++ = (AtomEntry *)DelayAddrAdjust((ADDR)reg);
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
  if (pp->ArityOfPE)
    pp->FunctorOfPred = FuncAdjust(pp->FunctorOfPred);
  else
    pp->FunctorOfPred = (Functor)AtomAdjust((Atom)(pp->FunctorOfPred));
  if (pp->ModuleOfPred != 2) {
    if (pp->src.OwnerFile && pp->ModuleOfPred != 2)
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
    if (FirstC == NIL && LastC == NIL) {
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
	  restore_static_array((StaticArrayEntry *)ae);
	} else {
	  if (ae->NextArrayE != NULL)
	    ae->NextArrayE = PtoArrayEAdjust(ae->NextArrayE);
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
	    } else if (IsOldDelayPtr(ptr)) {
	      ae->ValueOfVE = AbsAppl(PtoDelayAdjust(ptr));
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
    case LogUpdDBProperty:
    case CodeLogUpdDBProperty:
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
    case ExpProperty:
    case OpProperty:
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

