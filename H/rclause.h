
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
* File:		rclause.h						 *
* comments:	walk through a clause					 *
*									 *
* Last rev:     $Date: 2006-04-27 14:13:24 $,$Author: rslopes $						 *
* $Log: not supported by cvs2svn $
* Revision 1.14  2005/12/17 03:25:39  vsc
* major changes to support online event-based profiling
* improve error discovery and restart on scanner.
*
* Revision 1.13  2005/11/24 15:35:29  tiagosoares
* removed some compilation warnings related to the cut-c code
*
* Revision 1.12  2005/09/19 19:14:50  vsc
* fix two instructions that were being read badly: op_fail and
* switch_list_nl.
*
* Revision 1.11  2005/09/08 21:55:47  rslopes
* BEAM for YAP update...
*
* Revision 1.10  2005/08/01 15:40:38  ricroc
* TABLING NEW: better support for incomplete tabling
*
* Revision 1.9  2005/07/06 19:34:11  ricroc
* TABLING: answers for completed calls can now be obtained by loading (new option) or executing (default) them from the trie data structure.
*
* Revision 1.8  2005/07/06 15:10:15  vsc
* improvements to compiler: merged instructions and fixes for ->
*
* Revision 1.7  2005/06/04 07:26:43  ricroc
* long int support for tabling
*
* Revision 1.6  2005/06/03 08:18:25  ricroc
* float support for tabling
*
* Revision 1.5  2005/06/01 20:25:23  vsc
* == and \= should not need a choice-point in ->
*
* Revision 1.4  2005/06/01 14:02:52  vsc
* get_rid of try_me?, retry_me? and trust_me? instructions: they are not
* significantly used nowadays.
*
* Revision 1.3  2005/05/30 03:26:37  vsc
* add some atom gc fixes
*
* Revision 1.2  2005/04/10 04:01:13  vsc
* bug fixes, I hope!
*
* Revision 1.1  2005/01/04 02:50:21  vsc
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

static void 
restore_opcodes(yamop *pc)
{
  do {
    op_numbers op = Yap_op_from_opcode(pc->opc);
    pc->opc = Yap_opcode(op);
#ifdef DEBUG_RESTORE2
    fprintf(stderr, "%s ", Yap_op_names[op]);
#endif
    switch (op) {
    case _Nstop:
      return;
    case _Ystop:
#ifdef DEBUG_RESTORE2
      fprintf(stderr, "OK\n");
#endif
      pc->u.l.l = PtoOpAdjust(pc->u.l.l);
      return;
      /* instructions type ld */
    case _try_me:
    case _retry_me:
    case _trust_me:
    case _profiled_retry_me:
    case _profiled_trust_me:
    case _count_retry_me:
    case _count_trust_me:
    case _spy_or_trymark:
    case _try_and_mark:
    case _profiled_retry_and_mark:
    case _count_retry_and_mark:
    case _retry_and_mark:
    case _try_clause:
    case _retry:
    case _trust:
#ifdef YAPOR
    case _getwork:
    case _getwork_seq:
    case _sync:
#endif
#ifdef TABLING
    case _table_load_answer:
    case _table_try_answer:
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
      pc = pc->u.Ill.l1;
      break;
      /* instructions type p */
#if !defined(YAPOR)
    case _or_last:
#endif
    case _enter_profiling:
    case _retry_profiled:
    case _lock_lu:
    case _count_call:
    case _count_retry:
    case _procceed:
      pc->u.p.p = PtoPredAdjust(pc->u.p.p);
      pc = NEXTOP(pc,p);
      break;
    case _execute:
    case _dexecute:
      pc->u.pp.p = PtoPredAdjust(pc->u.pp.p);
      pc->u.pp.p0 = PtoPredAdjust(pc->u.pp.p0);
      pc = NEXTOP(pc,pp);
      break;
    case _trust_logical_pred:
    case _jump:
    case _move_back:
    case _skip:
    case _jump_if_var:
    case _try_in:
    case _try_clause2:
    case _try_clause3:
    case _try_clause4:
    case _retry2:
    case _retry3:
    case _retry4:
    case _p_eq:
    case _p_dif:
      pc->u.l.l = PtoOpAdjust(pc->u.l.l);
      pc = NEXTOP(pc,l);
      break;
      /* instructions type EC */
    case _jump_if_nonvar:
      pc->u.xll.l1 = PtoOpAdjust(pc->u.xll.l1);
      pc->u.xll.l2 = PtoOpAdjust(pc->u.xll.l2);
      pc->u.xll.x = XAdjust(pc->u.xll.x);
      pc = NEXTOP(pc,xll);
      break;
      /* instructions type EC */
    case _alloc_for_logical_pred:
      pc->u.EC.ClBase = (struct logic_upd_clause *)PtoOpAdjust((yamop *)pc->u.EC.ClBase);
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
    case _allocate:
    case _deallocate:
    case _write_void:
    case _write_list:
    case _write_l_list:
    case _pop:
    case _index_pred:
#ifdef BEAM
    case _retry_eam:
#endif
#if THREADS
    case _thread_local:
#endif
    case _expand_index:
    case _undef_p:
    case _spy_pred:
    case _p_equal:
    case _p_functor:
    case _enter_a_profiling:
    case _count_a_call:
    case _index_dbref:
    case _index_blob:
    case _unlock_lu:
#ifdef YAPOR
    case _getwork_first_time:
#endif
#ifdef TABLING
    case _trie_do_null:
    case _trie_trust_null:
    case _trie_try_null:
    case _trie_retry_null:
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
    case _trie_do_extension:
    case _trie_trust_extension:
    case _trie_try_extension:
    case _trie_retry_extension:
    case _trie_do_float:
    case _trie_trust_float:
    case _trie_try_float:
    case _trie_retry_float:
    case _trie_do_long:
    case _trie_trust_long:
    case _trie_try_long:
    case _trie_retry_long:
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
      pc->u.x.x = XAdjust(pc->u.x.x);
      pc = NEXTOP(pc,x);
      break;
      /* instructions type xF */
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
      pc->u.xF.x = XAdjust(pc->u.xF.x);
      pc->u.xF.F = PtoOpAdjust(pc->u.xF.F);
      pc = NEXTOP(pc,xF);
      break;
    case _expand_clauses:
      Yap_Error(SYSTEM_ERROR, TermNil,
	    "Invalid Opcode expand_clauses at %p", pc);
      break;
      /* instructions type y */
    case _save_b_y:
    case _commit_b_y:
    case _write_y_var:
    case _write_y_val: 
    case _write_y_loc:
      pc->u.y.y = YAdjust(pc->u.y.y);
      pc = NEXTOP(pc,y);
      break;
      /* instructions type yF */
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
      pc->u.yF.y = YAdjust(pc->u.yF.y);
      pc->u.yF.F = PtoOpAdjust(pc->u.yF.F);
      pc = NEXTOP(pc,yF);
      break;
      /* instructions type sla */      
    case _p_execute_tail:
    case _p_execute:
      pc->u.sla.sla_u.p = PtoPredAdjust(pc->u.sla.sla_u.p);
      if (pc->u.sla.sla_u.mod != 0) {
	pc->u.sla.sla_u.mod = AtomTermAdjust(pc->u.sla.sla_u.mod);
      }
      pc->u.sla.p0 = PtoPredAdjust(pc->u.sla.p0);
      if (pc->u.sla.bmap != NULL) {
	pc->u.sla.bmap = CellPtoHeapAdjust(pc->u.sla.bmap);
      }
      pc = NEXTOP(pc,sla);
      break;
    case _fcall:
    case _call:
#ifdef YAPOR
    case _or_last:
#endif
      pc->u.sla.sla_u.p = PtoPredAdjust(pc->u.sla.sla_u.p);
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
    case _put_xx_val:
      pc->u.xxxx.xr1 = XAdjust(pc->u.xxxx.xr1);
      pc->u.xxxx.xl1 = XAdjust(pc->u.xxxx.xl1);
      pc->u.xxxx.xr2 = XAdjust(pc->u.xxxx.xr2);
      pc->u.xxxx.xl2 = XAdjust(pc->u.xxxx.xl2);
      pc = NEXTOP(pc,xxxx);
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
      /* instructions type cc */
    case _get_2atoms:
      {
	Term t = pc->u.cc.c1;
	if (IsAtomTerm(t))
	  pc->u.cc.c1 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.cc.c1 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.cc.c2;
	if (IsAtomTerm(t))
	  pc->u.cc.c2 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.cc.c2 = BlobTermAdjust(t);
      }
      pc = NEXTOP(pc,cc);
      break;
      /* instructions type ccc */
    case _get_3atoms:
      {
	Term t = pc->u.ccc.c1;
	if (IsAtomTerm(t))
	  pc->u.ccc.c1 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.ccc.c1 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.ccc.c2;
	if (IsAtomTerm(t))
	  pc->u.ccc.c2 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.ccc.c2 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.ccc.c3;
	if (IsAtomTerm(t))
	  pc->u.ccc.c3 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.ccc.c3 = BlobTermAdjust(t);
      }
      pc = NEXTOP(pc,ccc);
      break;
      /* instructions type cccc */
    case _get_4atoms:
      {
	Term t = pc->u.cccc.c1;
	if (IsAtomTerm(t))
	  pc->u.cccc.c1 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.cccc.c1 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.cccc.c2;
	if (IsAtomTerm(t))
	  pc->u.cccc.c2 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.cccc.c2 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.cccc.c3;
	if (IsAtomTerm(t))
	  pc->u.cccc.c3 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.cccc.c3 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.cccc.c4;
	if (IsAtomTerm(t))
	  pc->u.cccc.c4 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.cccc.c4 = BlobTermAdjust(t);
      }
      pc = NEXTOP(pc,cccc);
      break;
      /* instructions type ccccc */
    case _get_5atoms:
      {
	Term t = pc->u.ccccc.c1;
	if (IsAtomTerm(t))
	  pc->u.ccccc.c1 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.ccccc.c1 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.ccccc.c2;
	if (IsAtomTerm(t))
	  pc->u.ccccc.c2 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.ccccc.c2 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.ccccc.c3;
	if (IsAtomTerm(t))
	  pc->u.ccccc.c3 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.ccccc.c3 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.ccccc.c4;
	if (IsAtomTerm(t))
	  pc->u.ccccc.c4 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.ccccc.c4 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.ccccc.c5;
	if (IsAtomTerm(t))
	  pc->u.ccccc.c5 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.ccccc.c5 = BlobTermAdjust(t);
      }
      pc = NEXTOP(pc,ccccc);
      break;
      /* instructions type cccccc */
    case _get_6atoms:
      {
	Term t = pc->u.cccccc.c1;
	if (IsAtomTerm(t))
	  pc->u.cccccc.c1 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.cccccc.c1 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.cccccc.c2;
	if (IsAtomTerm(t))
	  pc->u.cccccc.c2 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.cccccc.c2 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.cccccc.c3;
	if (IsAtomTerm(t))
	  pc->u.cccccc.c3 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.cccccc.c3 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.cccccc.c4;
	if (IsAtomTerm(t))
	  pc->u.cccccc.c4 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.cccccc.c4 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.cccccc.c5;
	if (IsAtomTerm(t))
	  pc->u.cccccc.c5 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.cccccc.c5 = BlobTermAdjust(t);
      }
      {
	Term t = pc->u.cccccc.c6;
	if (IsAtomTerm(t))
	  pc->u.cccccc.c6 = AtomTermAdjust(t);
	else if (IsApplTerm(t))
	  pc->u.cccccc.c6 = BlobTermAdjust(t);
      }
      pc = NEXTOP(pc,cccccc);
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
#ifdef BEAM
    case _run_eam:
#endif
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
#ifdef CUT_C
    case _cut_c:
    case _cut_userc:
      /* don't need to do nothing here, because this two instructions 
       are "phantom" instructions. (see: cut_c implementation paper 
       on PADL 2006) */
      break;
#endif 
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
      pc->u.ollll.l1 = PtoOpAdjust(pc->u.ollll.l1);
      pc->u.ollll.l2 = PtoOpAdjust(pc->u.ollll.l2);
      pc->u.ollll.l3 = PtoOpAdjust(pc->u.ollll.l3);
      pc->u.ollll.l4 = PtoOpAdjust(pc->u.ollll.l4);
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

	i = pc->u.sssl.s;
	startcode = oldcode = (CELL *)(pc->u.sssl.l = PtoOpAdjust(pc->u.sssl.l));
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
	pc = NEXTOP(pc,sssl);
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

	i = pc->u.sssl.s;
#if !USE_OFFSETS
	startcode =
#endif
	  oldcode = (CELL *)(pc->u.sssl.l = PtoOpAdjust(pc->u.sssl.l));
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
	pc = NEXTOP(pc,sssl);
      }
      break;
    case _go_on_func:
      {
	CELL *oldcode = (CELL *)(pc->u.sssl.l = PtoOpAdjust(pc->u.sssl.l));
	Functor oldfunc = (Functor)(oldcode[0]);

	oldcode[0] = (CELL)FuncAdjust(oldfunc);
	oldcode[1] = (CELL)CodeAddrAdjust((CODEADDR)oldcode[1]);
	oldcode[3] = (CELL)CodeAddrAdjust((CODEADDR)oldcode[3]);
      }
      pc = NEXTOP(pc,sssl);
      break;
    case _go_on_cons:
      {
	CELL *oldcode = (CELL *)(pc->u.sssl.l = PtoOpAdjust(pc->u.sssl.l));
	Term oldcons = oldcode[0];

	if (IsAtomTerm(oldcons)) {
	  oldcode[0] = AtomTermAdjust(oldcons);
	}
	oldcode[1] = (CELL)CodeAddrAdjust((CODEADDR)oldcode[1]);
	oldcode[3] = (CELL)CodeAddrAdjust((CODEADDR)oldcode[3]);
      }
      pc = NEXTOP(pc,sssl);
      break;
    case _if_func:
      {
	CELL *oldcode = (CELL *)(pc->u.sssl.l = PtoOpAdjust(pc->u.sssl.l));
	Int j;

	for (j = 0; j < pc->u.sssl.s; j++) {
	  Functor oldfunc = (Functor)(oldcode[0]);
	  CODEADDR oldjmp = (CODEADDR)(oldcode[1]);
	  oldcode[0] = (CELL)FuncAdjust(oldfunc);
	  oldcode[1] = (CELL)CodeAddrAdjust(oldjmp);
	  oldcode += 2;
	}
	/* adjust fail code */
	oldcode[1] = (CELL)CodeAddrAdjust((CODEADDR)oldcode[1]);
      }
      pc = NEXTOP(pc,sssl);
      break;
    case _if_cons:
      {
	CELL *oldcode = (CELL *)(pc->u.sssl.l = PtoOpAdjust(pc->u.sssl.l));
	Int j;

	for (j = 0; j < pc->u.sssl.s; j++) {
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
      pc = NEXTOP(pc,sssl);
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

