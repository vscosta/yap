
  /* This file was generated automatically by "yap -L misc/buildops"
     please do not update */


static void 
restore_opcodes(yamop *pc, yamop *max)
{
  yamop *opc = NULL;
  do {
    op_numbers op;
    if (max && pc >= max) return;
    op = Yap_op_from_opcode(pc->opc);
    pc->opc = Yap_opcode(op);
#ifdef DEBUG_RESTORE2
    fprintf(stderr, "%s ", Yap_op_names[op]);
#endif
    switch (op) {
      /* instructions type D */
    case _write_dbterm:
      pc->u.D.D = DBGroundTermAdjust(pc->u.D.D);
      pc = NEXTOP(pc,D);
      break;
      /* instructions type Ills */
    case _enter_lu_pred:
      pc->u.Ills.I = PtoLUIndexAdjust(pc->u.Ills.I);
      pc->u.Ills.l1 = PtoOpAdjust(pc->u.Ills.l1);
      pc->u.Ills.l2 = PtoOpAdjust(pc->u.Ills.l2);
      pc->u.Ills.s = ConstantAdjust(pc->u.Ills.s);
      opc = NEXTOP(pc,Ills);
      pc = pc->u.Ills.l1;
      break;
      /* instructions type L */
    case _alloc_for_logical_pred:
      pc->u.L.ClBase = PtoLUClauseAdjust(pc->u.L.ClBase);
      pc = NEXTOP(pc,L);
      break;
      /* instructions type N */
    case _write_bigint:
      pc->u.N.b = BlobTermInCodeAdjust(pc->u.N.b);
      pc = NEXTOP(pc,N);
      break;
      /* instructions type Osblp */
    case _either:
    case _or_else:
      OrArgAdjust(pc->u.Osblp.or_arg);
      pc->u.Osblp.s = ConstantAdjust(pc->u.Osblp.s);
      pc->u.Osblp.bmap = CellPtoHeapAdjust(pc->u.Osblp.bmap);
      pc->u.Osblp.l = PtoOpAdjust(pc->u.Osblp.l);
      pc->u.Osblp.p0 = PtoPredAdjust(pc->u.Osblp.p0);
      pc = NEXTOP(pc,Osblp);
      break;
      /* instructions type Osbmp */
    case _p_execute:
      OrArgAdjust(pc->u.Osbmp.or_arg);
      pc->u.Osbmp.s = ConstantAdjust(pc->u.Osbmp.s);
      pc->u.Osbmp.bmap = CellPtoHeapAdjust(pc->u.Osbmp.bmap);
      pc->u.Osbmp.mod = ModuleAdjust(pc->u.Osbmp.mod);
      pc->u.Osbmp.p0 = PtoPredAdjust(pc->u.Osbmp.p0);
      pc = NEXTOP(pc,Osbmp);
      break;
      /* instructions type Osbpi */
    case _ensure_space:
      OrArgAdjust(pc->u.Osbpi.or_arg);
      pc->u.Osbpi.s = ConstantAdjust(pc->u.Osbpi.s);
      pc->u.Osbpi.bmap = CellPtoHeapAdjust(pc->u.Osbpi.bmap);
      pc->u.Osbpi.p = PtoPredAdjust(pc->u.Osbpi.p);
      IntegerInCodeAdjust(pc->u.Osbpi.i);
      pc = NEXTOP(pc,Osbpi);
      break;
      /* instructions type Osbpp */
    case _call:
    case _call_cpred:
    case _call_usercpred:
    case _fcall:
    case _p_execute2:
    case _p_execute_tail:
      OrArgAdjust(pc->u.Osbpp.or_arg);
      pc->u.Osbpp.s = ConstantAdjust(pc->u.Osbpp.s);
      pc->u.Osbpp.bmap = CellPtoHeapAdjust(pc->u.Osbpp.bmap);
      pc->u.Osbpp.p = PtoPredAdjust(pc->u.Osbpp.p);
      pc->u.Osbpp.p0 = PtoPredAdjust(pc->u.Osbpp.p0);
      pc = NEXTOP(pc,Osbpp);
      break;
      /* instructions type OtILl */
    case _count_trust_logical:
    case _profiled_trust_logical:
    case _trust_logical:
      OrArgAdjust(pc->u.OtILl.or_arg);
      TabEntryAdjust(pc->u.OtILl.te);
      pc->u.OtILl.block = PtoLUIndexAdjust(pc->u.OtILl.block);
      pc->u.OtILl.d = PtoLUClauseAdjust(pc->u.OtILl.d);
      pc->u.OtILl.n = PtoOpAdjust(pc->u.OtILl.n);
      pc = opc;
      break;
      /* instructions type OtaLl */
    case _count_retry_logical:
    case _profiled_retry_logical:
    case _retry_logical:
    case _try_logical:
      OrArgAdjust(pc->u.OtaLl.or_arg);
      TabEntryAdjust(pc->u.OtaLl.te);
      pc->u.OtaLl.s = ArityAdjust(pc->u.OtaLl.s);
      pc->u.OtaLl.d = PtoLUClauseAdjust(pc->u.OtaLl.d);
      pc->u.OtaLl.n = PtoOpAdjust(pc->u.OtaLl.n);
      pc = pc->u.OtaLl.n;
      break;
      /* instructions type OtapFs */
#ifdef CUT_C
    case _cut_c:
#endif
#ifdef CUT_C
    case _cut_userc:
#endif
    case _retry_c:
    case _retry_userc:
    case _try_c:
    case _try_userc:
      OrArgAdjust(pc->u.OtapFs.or_arg);
      TabEntryAdjust(pc->u.OtapFs.te);
      pc->u.OtapFs.s = ArityAdjust(pc->u.OtapFs.s);
      pc->u.OtapFs.p = PtoPredAdjust(pc->u.OtapFs.p);
      pc->u.OtapFs.f = ExternalFunctionAdjust(pc->u.OtapFs.f);
      pc->u.OtapFs.extra = ConstantAdjust(pc->u.OtapFs.extra);
      pc = NEXTOP(pc,OtapFs);
      break;
      /* instructions type Otapl */
    case _count_retry_and_mark:
    case _count_retry_me:
    case _count_trust_me:
    case _profiled_retry_and_mark:
    case _profiled_retry_me:
    case _profiled_trust_me:
    case _retry:
    case _retry_and_mark:
    case _retry_me:
    case _spy_or_trymark:
    case _trust:
    case _trust_me:
    case _try_and_mark:
    case _try_clause:
    case _try_me:
      OrArgAdjust(pc->u.Otapl.or_arg);
      TabEntryAdjust(pc->u.Otapl.te);
      pc->u.Otapl.s = ArityAdjust(pc->u.Otapl.s);
      pc->u.Otapl.p = PtoPredAdjust(pc->u.Otapl.p);
      pc->u.Otapl.d = PtoOpAdjust(pc->u.Otapl.d);
      pc = NEXTOP(pc,Otapl);
      break;
      /* instructions type aFlp */
    case _native_me:
      pc->u.aFlp.n = ArityAdjust(pc->u.aFlp.n);
      pc->u.aFlp.native = ExternalFunctionAdjust(pc->u.aFlp.native);
      pc->u.aFlp.native_next = PtoOpAdjust(pc->u.aFlp.native_next);
      pc->u.aFlp.p = PtoPredAdjust(pc->u.aFlp.p);
      pc = NEXTOP(pc,aFlp);
      break;
      /* instructions type c */
    case _write_atom:
      pc->u.c.c = ConstantTermAdjust(pc->u.c.c);
      pc = NEXTOP(pc,c);
      break;
      /* instructions type cc */
    case _get_2atoms:
      pc->u.cc.c1 = ConstantTermAdjust(pc->u.cc.c1);
      pc->u.cc.c2 = ConstantTermAdjust(pc->u.cc.c2);
      pc = NEXTOP(pc,cc);
      break;
      /* instructions type ccc */
    case _get_3atoms:
      pc->u.ccc.c1 = ConstantTermAdjust(pc->u.ccc.c1);
      pc->u.ccc.c2 = ConstantTermAdjust(pc->u.ccc.c2);
      pc->u.ccc.c3 = ConstantTermAdjust(pc->u.ccc.c3);
      pc = NEXTOP(pc,ccc);
      break;
      /* instructions type cccc */
    case _get_4atoms:
      pc->u.cccc.c1 = ConstantTermAdjust(pc->u.cccc.c1);
      pc->u.cccc.c2 = ConstantTermAdjust(pc->u.cccc.c2);
      pc->u.cccc.c3 = ConstantTermAdjust(pc->u.cccc.c3);
      pc->u.cccc.c4 = ConstantTermAdjust(pc->u.cccc.c4);
      pc = NEXTOP(pc,cccc);
      break;
      /* instructions type ccccc */
    case _get_5atoms:
      pc->u.ccccc.c1 = ConstantTermAdjust(pc->u.ccccc.c1);
      pc->u.ccccc.c2 = ConstantTermAdjust(pc->u.ccccc.c2);
      pc->u.ccccc.c3 = ConstantTermAdjust(pc->u.ccccc.c3);
      pc->u.ccccc.c4 = ConstantTermAdjust(pc->u.ccccc.c4);
      pc->u.ccccc.c5 = ConstantTermAdjust(pc->u.ccccc.c5);
      pc = NEXTOP(pc,ccccc);
      break;
      /* instructions type cccccc */
    case _get_6atoms:
      pc->u.cccccc.c1 = ConstantTermAdjust(pc->u.cccccc.c1);
      pc->u.cccccc.c2 = ConstantTermAdjust(pc->u.cccccc.c2);
      pc->u.cccccc.c3 = ConstantTermAdjust(pc->u.cccccc.c3);
      pc->u.cccccc.c4 = ConstantTermAdjust(pc->u.cccccc.c4);
      pc->u.cccccc.c5 = ConstantTermAdjust(pc->u.cccccc.c5);
      pc->u.cccccc.c6 = ConstantTermAdjust(pc->u.cccccc.c6);
      pc = NEXTOP(pc,cccccc);
      break;
      /* instructions type clll */
    case _if_not_then:
      pc->u.clll.c = ConstantTermAdjust(pc->u.clll.c);
      pc->u.clll.l1 = PtoOpAdjust(pc->u.clll.l1);
      pc->u.clll.l2 = PtoOpAdjust(pc->u.clll.l2);
      pc->u.clll.l3 = PtoOpAdjust(pc->u.clll.l3);
      pc = NEXTOP(pc,clll);
      break;
      /* instructions type d */
    case _write_float:
      DoubleInCodeAdjust(pc->u.d.d);
      pc = NEXTOP(pc,d);
      break;
      /* instructions type e */
    case _Nstop:
    case _allocate:
    case _copy_idb_term:
    case _cut:
    case _cut_e:
    case _cut_t:
    case _expand_index:
    case _index_blob:
    case _index_dbref:
    case _index_long:
    case _index_pred:
    case _lock_pred:
    case _op_fail:
    case _p_equal:
    case _p_functor:
    case _pop:
#ifdef BEAM
    case _retry_eam:
#endif
    case _spy_pred:
#ifdef THREADS
    case _thread_local:
#endif
    case _trust_fail:
    case _undef_p:
    case _unify_idb_term:
    case _unlock_lu:
    case _write_l_list:
    case _write_list:
    case _write_void:
      if (op == _Nstop || op == _copy_idb_term || op == _unify_idb_term) return;
      pc = NEXTOP(pc,e);
      break;
      /* instructions type fa */
    case _write_l_struc:
    case _write_struct:
      pc->u.fa.f = FuncAdjust(pc->u.fa.f);
      pc->u.fa.a = ArityAdjust(pc->u.fa.a);
      pc = NEXTOP(pc,fa);
      break;
      /* instructions type i */
    case _write_longint:
      IntegerInCodeAdjust(pc->u.i.i);
      pc = NEXTOP(pc,i);
      break;
      /* instructions type l */
    case _Ystop:
    case _jump:
    case _jump_if_var:
    case _move_back:
    case _p_dif:
    case _p_eq:
    case _retry2:
    case _retry3:
    case _retry4:
    case _skip:
    case _try_clause2:
    case _try_clause3:
    case _try_clause4:
    case _try_in:
      pc->u.l.l = PtoOpAdjust(pc->u.l.l);
      if (op == _Ystop) return;
      pc = NEXTOP(pc,l);
      break;
      /* instructions type llll */
    case _switch_on_type:
      pc->u.llll.l1 = PtoOpAdjust(pc->u.llll.l1);
      pc->u.llll.l2 = PtoOpAdjust(pc->u.llll.l2);
      pc->u.llll.l3 = PtoOpAdjust(pc->u.llll.l3);
      pc->u.llll.l4 = PtoOpAdjust(pc->u.llll.l4);
      pc = NEXTOP(pc,llll);
      break;
      /* instructions type lp */
    case _user_switch:
      pc->u.lp.l = PtoOpAdjust(pc->u.lp.l);
      pc->u.lp.p = PtoPredAdjust(pc->u.lp.p);
      pc = NEXTOP(pc,lp);
      break;
      /* instructions type o */
    case _unify_l_list:
    case _unify_l_list_write:
    case _unify_l_void:
    case _unify_l_void_write:
    case _unify_list:
    case _unify_list_write:
    case _unify_void:
    case _unify_void_write:
      pc->u.o.opcw = OpcodeAdjust(pc->u.o.opcw);
      pc = NEXTOP(pc,o);
      break;
      /* instructions type oD */
    case _unify_dbterm:
    case _unify_l_dbterm:
      pc->u.oD.opcw = OpcodeAdjust(pc->u.oD.opcw);
      pc->u.oD.D = DBGroundTermAdjust(pc->u.oD.D);
      pc = NEXTOP(pc,oD);
      break;
      /* instructions type oN */
    case _unify_bigint:
    case _unify_l_bigint:
      pc->u.oN.opcw = OpcodeAdjust(pc->u.oN.opcw);
      pc->u.oN.b = BlobTermInCodeAdjust(pc->u.oN.b);
      pc = NEXTOP(pc,oN);
      break;
      /* instructions type oc */
    case _unify_atom:
    case _unify_atom_write:
    case _unify_l_atom:
    case _unify_l_atom_write:
      pc->u.oc.opcw = OpcodeAdjust(pc->u.oc.opcw);
      pc->u.oc.c = ConstantTermAdjust(pc->u.oc.c);
      pc = NEXTOP(pc,oc);
      break;
      /* instructions type od */
    case _unify_float:
    case _unify_float_write:
    case _unify_l_float:
    case _unify_l_float_write:
      pc->u.od.opcw = OpcodeAdjust(pc->u.od.opcw);
      DoubleInCodeAdjust(pc->u.od.d);
      pc = NEXTOP(pc,od);
      break;
      /* instructions type ofa */
    case _unify_l_struc:
    case _unify_l_struc_write:
    case _unify_struct:
    case _unify_struct_write:
      pc->u.ofa.opcw = OpcodeAdjust(pc->u.ofa.opcw);
      pc->u.ofa.f = FuncAdjust(pc->u.ofa.f);
      pc->u.ofa.a = ArityAdjust(pc->u.ofa.a);
      pc = NEXTOP(pc,ofa);
      break;
      /* instructions type oi */
    case _unify_l_longint:
    case _unify_l_longint_write:
    case _unify_longint:
    case _unify_longint_write:
      pc->u.oi.opcw = OpcodeAdjust(pc->u.oi.opcw);
      IntegerInCodeAdjust(pc->u.oi.i);
      pc = NEXTOP(pc,oi);
      break;
      /* instructions type ollll */
    case _switch_list_nl:
      pc->u.ollll.pop = OpcodeAdjust(pc->u.ollll.pop);
      pc->u.ollll.l1 = PtoOpAdjust(pc->u.ollll.l1);
      pc->u.ollll.l2 = PtoOpAdjust(pc->u.ollll.l2);
      pc->u.ollll.l3 = PtoOpAdjust(pc->u.ollll.l3);
      pc->u.ollll.l4 = PtoOpAdjust(pc->u.ollll.l4);
      pc = NEXTOP(pc,ollll);
      break;
      /* instructions type os */
#ifdef BEAM
    case _run_eam:
#endif
    case _unify_l_n_voids:
    case _unify_l_n_voids_write:
    case _unify_n_voids:
    case _unify_n_voids_write:
      pc->u.os.opcw = OpcodeAdjust(pc->u.os.opcw);
      pc->u.os.s = ConstantAdjust(pc->u.os.s);
      pc = NEXTOP(pc,os);
      break;
      /* instructions type osc */
    case _unify_n_atoms:
    case _unify_n_atoms_write:
      pc->u.osc.opcw = OpcodeAdjust(pc->u.osc.opcw);
      pc->u.osc.s = ConstantAdjust(pc->u.osc.s);
      pc->u.osc.c = ConstantTermAdjust(pc->u.osc.c);
      pc = NEXTOP(pc,osc);
      break;
      /* instructions type ox */
    case _save_appl_x:
    case _save_appl_x_write:
    case _save_pair_x:
    case _save_pair_x_write:
    case _unify_l_x_loc:
    case _unify_l_x_loc_write:
    case _unify_l_x_val:
    case _unify_l_x_val_write:
    case _unify_l_x_var:
    case _unify_l_x_var_write:
    case _unify_x_loc:
    case _unify_x_loc_write:
    case _unify_x_val:
    case _unify_x_val_write:
    case _unify_x_var:
    case _unify_x_var_write:
      pc->u.ox.opcw = OpcodeAdjust(pc->u.ox.opcw);
      pc->u.ox.x = XAdjust(pc->u.ox.x);
      pc = NEXTOP(pc,ox);
      break;
      /* instructions type oxx */
    case _unify_l_x_var2:
    case _unify_l_x_var2_write:
    case _unify_x_var2:
    case _unify_x_var2_write:
      pc->u.oxx.opcw = OpcodeAdjust(pc->u.oxx.opcw);
      pc->u.oxx.xl = XAdjust(pc->u.oxx.xl);
      pc->u.oxx.xr = XAdjust(pc->u.oxx.xr);
      pc = NEXTOP(pc,oxx);
      break;
      /* instructions type oy */
    case _save_appl_y:
    case _save_appl_y_write:
    case _save_pair_y:
    case _save_pair_y_write:
    case _unify_l_y_loc:
    case _unify_l_y_loc_write:
    case _unify_l_y_val:
    case _unify_l_y_val_write:
    case _unify_l_y_var:
    case _unify_l_y_var_write:
    case _unify_y_loc:
    case _unify_y_loc_write:
    case _unify_y_val:
    case _unify_y_val_write:
    case _unify_y_var:
    case _unify_y_var_write:
      pc->u.oy.opcw = OpcodeAdjust(pc->u.oy.opcw);
      pc->u.oy.y = YAdjust(pc->u.oy.y);
      pc = NEXTOP(pc,oy);
      break;
      /* instructions type p */
    case _count_call:
    case _count_retry:
    case _deallocate:
    case _enter_profiling:
    case _lock_lu:
    case _procceed:
    case _retry_profiled:
      pc->u.p.p = PtoPredAdjust(pc->u.p.p);
      pc = NEXTOP(pc,p);
      break;
      /* instructions type plxxs */
    case _call_bfunc_xx:
      pc->u.plxxs.p = PtoPredAdjust(pc->u.plxxs.p);
      pc->u.plxxs.f = PtoOpAdjust(pc->u.plxxs.f);
      pc->u.plxxs.x1 = XAdjust(pc->u.plxxs.x1);
      pc->u.plxxs.x2 = XAdjust(pc->u.plxxs.x2);
      pc->u.plxxs.flags = ConstantAdjust(pc->u.plxxs.flags);
      pc = NEXTOP(pc,plxxs);
      break;
      /* instructions type plxys */
    case _call_bfunc_xy:
    case _call_bfunc_yx:
      pc->u.plxys.p = PtoPredAdjust(pc->u.plxys.p);
      pc->u.plxys.f = PtoOpAdjust(pc->u.plxys.f);
      pc->u.plxys.x = XAdjust(pc->u.plxys.x);
      pc->u.plxys.y = YAdjust(pc->u.plxys.y);
      pc->u.plxys.flags = ConstantAdjust(pc->u.plxys.flags);
      pc = NEXTOP(pc,plxys);
      break;
      /* instructions type plyys */
    case _call_bfunc_yy:
      pc->u.plyys.p = PtoPredAdjust(pc->u.plyys.p);
      pc->u.plyys.f = PtoOpAdjust(pc->u.plyys.f);
      pc->u.plyys.y1 = YAdjust(pc->u.plyys.y1);
      pc->u.plyys.y2 = YAdjust(pc->u.plyys.y2);
      pc->u.plyys.flags = ConstantAdjust(pc->u.plyys.flags);
      pc = NEXTOP(pc,plyys);
      break;
      /* instructions type pp */
    case _dexecute:
    case _execute:
    case _execute_cpred:
      pc->u.pp.p = PtoPredAdjust(pc->u.pp.p);
      pc->u.pp.p0 = PtoPredAdjust(pc->u.pp.p0);
      pc = NEXTOP(pc,pp);
      break;
      /* instructions type s */
    case _pop_n:
    case _write_n_voids:
      pc->u.s.s = ConstantAdjust(pc->u.s.s);
      pc = NEXTOP(pc,s);
      break;
      /* instructions type sc */
    case _write_n_atoms:
      pc->u.sc.s = ConstantAdjust(pc->u.sc.s);
      pc->u.sc.c = ConstantTermAdjust(pc->u.sc.c);
      pc = NEXTOP(pc,sc);
      break;
      /* instructions type sllll */
    case _switch_on_sub_arg_type:
      pc->u.sllll.s = ConstantAdjust(pc->u.sllll.s);
      pc->u.sllll.l1 = PtoOpAdjust(pc->u.sllll.l1);
      pc->u.sllll.l2 = PtoOpAdjust(pc->u.sllll.l2);
      pc->u.sllll.l3 = PtoOpAdjust(pc->u.sllll.l3);
      pc->u.sllll.l4 = PtoOpAdjust(pc->u.sllll.l4);
      pc = NEXTOP(pc,sllll);
      break;
      /* instructions type slp */
    case _call_c_wfail:
      pc->u.slp.s = ConstantAdjust(pc->u.slp.s);
      pc->u.slp.l = PtoOpAdjust(pc->u.slp.l);
      pc->u.slp.p = PtoPredAdjust(pc->u.slp.p);
      pc = NEXTOP(pc,slp);
      break;
      /* instructions type sssl */
    case _go_on_cons:
    case _go_on_func:
    case _if_cons:
    case _if_func:
    case _switch_on_cons:
    case _switch_on_func:
      pc->u.sssl.s = ConstantAdjust(pc->u.sssl.s);
      pc->u.sssl.e = ConstantAdjust(pc->u.sssl.e);
      pc->u.sssl.w = ConstantAdjust(pc->u.sssl.w);
      pc->u.sssl.l = PtoOpAdjust(pc->u.sssl.l);
      AdjustSwitchTable(op, pc->u.sssl.l, pc->u.sssl.s);
      pc = NEXTOP(pc,sssl);
      break;
      /* instructions type sssllp */
    case _expand_clauses:
      pc->u.sssllp.s1 = ConstantAdjust(pc->u.sssllp.s1);
      pc->u.sssllp.s2 = ConstantAdjust(pc->u.sssllp.s2);
      pc->u.sssllp.s3 = ConstantAdjust(pc->u.sssllp.s3);
      pc->u.sssllp.sprev = PtoOpAdjust(pc->u.sssllp.sprev);
      pc->u.sssllp.snext = PtoOpAdjust(pc->u.sssllp.snext);
      pc->u.sssllp.p = PtoPredAdjust(pc->u.sssllp.p);
      pc = NEXTOP(pc,sssllp);
      break;
      /* instructions type x */
    case _get_list:
    case _put_list:
    case _save_b_x:
    case _write_x_loc:
    case _write_x_val:
    case _write_x_var:
      pc->u.x.x = XAdjust(pc->u.x.x);
      pc = NEXTOP(pc,x);
      break;
      /* instructions type xD */
    case _get_dbterm:
    case _put_dbterm:
      pc->u.xD.x = XAdjust(pc->u.xD.x);
      pc->u.xD.D = DBGroundTermAdjust(pc->u.xD.D);
      pc = NEXTOP(pc,xD);
      break;
      /* instructions type xN */
    case _get_bigint:
    case _put_bigint:
      pc->u.xN.x = XAdjust(pc->u.xN.x);
      pc->u.xN.b = BlobTermInCodeAdjust(pc->u.xN.b);
      pc = NEXTOP(pc,xN);
      break;
      /* instructions type xc */
    case _get_atom:
    case _put_atom:
      pc->u.xc.x = XAdjust(pc->u.xc.x);
      pc->u.xc.c = ConstantTermAdjust(pc->u.xc.c);
      pc = NEXTOP(pc,xc);
      break;
      /* instructions type xd */
    case _get_float:
    case _put_float:
      pc->u.xd.x = XAdjust(pc->u.xd.x);
      DoubleInCodeAdjust(pc->u.xd.d);
      pc = NEXTOP(pc,xd);
      break;
      /* instructions type xfa */
    case _get_struct:
    case _put_struct:
      pc->u.xfa.x = XAdjust(pc->u.xfa.x);
      pc->u.xfa.f = FuncAdjust(pc->u.xfa.f);
      pc->u.xfa.a = ArityAdjust(pc->u.xfa.a);
      pc = NEXTOP(pc,xfa);
      break;
      /* instructions type xi */
    case _get_longint:
    case _put_longint:
      pc->u.xi.x = XAdjust(pc->u.xi.x);
      IntegerInCodeAdjust(pc->u.xi.i);
      pc = NEXTOP(pc,xi);
      break;
      /* instructions type xl */
    case _p_atom_x:
    case _p_atomic_x:
    case _p_compound_x:
    case _p_cut_by_x:
    case _p_db_ref_x:
    case _p_float_x:
    case _p_integer_x:
    case _p_nonvar_x:
    case _p_number_x:
    case _p_primitive_x:
    case _p_var_x:
      pc->u.xl.x = XAdjust(pc->u.xl.x);
      pc->u.xl.F = PtoOpAdjust(pc->u.xl.F);
      pc = NEXTOP(pc,xl);
      break;
      /* instructions type xll */
    case _jump_if_nonvar:
      pc->u.xll.x = XAdjust(pc->u.xll.x);
      pc->u.xll.l1 = PtoOpAdjust(pc->u.xll.l1);
      pc->u.xll.l2 = PtoOpAdjust(pc->u.xll.l2);
      pc = NEXTOP(pc,xll);
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
      /* instructions type xp */
    case _commit_b_x:
      pc->u.xp.x = XAdjust(pc->u.xp.x);
      pc->u.xp.p0 = PtoPredAdjust(pc->u.xp.p0);
      pc = NEXTOP(pc,xp);
      break;
      /* instructions type xx */
    case _get_x_val:
    case _get_x_var:
    case _gl_void_valx:
    case _gl_void_varx:
    case _glist_valx:
    case _put_x_val:
    case _put_x_var:
      pc->u.xx.xl = XAdjust(pc->u.xx.xl);
      pc->u.xx.xr = XAdjust(pc->u.xx.xr);
      pc = NEXTOP(pc,xx);
      break;
      /* instructions type xxc */
    case _p_func2s_cv:
      pc->u.xxc.x = XAdjust(pc->u.xxc.x);
      pc->u.xxc.xi = XAdjust(pc->u.xxc.xi);
      pc->u.xxc.c = ConstantTermAdjust(pc->u.xxc.c);
      pc = NEXTOP(pc,xxc);
      break;
      /* instructions type xxn */
    case _p_and_vc:
    case _p_arg_cv:
    case _p_div_cv:
    case _p_div_vc:
    case _p_func2s_vc:
    case _p_minus_cv:
    case _p_or_vc:
    case _p_plus_vc:
    case _p_sll_cv:
    case _p_sll_vc:
    case _p_slr_cv:
    case _p_slr_vc:
    case _p_times_vc:
      pc->u.xxn.x = XAdjust(pc->u.xxn.x);
      pc->u.xxn.xi = XAdjust(pc->u.xxn.xi);
      pc->u.xxn.c = IntegerAdjust(pc->u.xxn.c);
      pc = NEXTOP(pc,xxn);
      break;
      /* instructions type xxx */
    case _p_and_vv:
    case _p_arg_vv:
    case _p_div_vv:
    case _p_func2f_xx:
    case _p_func2s_vv:
    case _p_minus_vv:
    case _p_or_vv:
    case _p_plus_vv:
    case _p_sll_vv:
    case _p_slr_vv:
    case _p_times_vv:
      pc->u.xxx.x = XAdjust(pc->u.xxx.x);
      pc->u.xxx.x1 = XAdjust(pc->u.xxx.x1);
      pc->u.xxx.x2 = XAdjust(pc->u.xxx.x2);
      pc = NEXTOP(pc,xxx);
      break;
      /* instructions type xxxx */
    case _put_xx_val:
      pc->u.xxxx.xl1 = XAdjust(pc->u.xxxx.xl1);
      pc->u.xxxx.xl2 = XAdjust(pc->u.xxxx.xl2);
      pc->u.xxxx.xr1 = XAdjust(pc->u.xxxx.xr1);
      pc->u.xxxx.xr2 = XAdjust(pc->u.xxxx.xr2);
      pc = NEXTOP(pc,xxxx);
      break;
      /* instructions type xxy */
    case _p_func2f_xy:
      pc->u.xxy.x = XAdjust(pc->u.xxy.x);
      pc->u.xxy.x1 = XAdjust(pc->u.xxy.x1);
      pc->u.xxy.y2 = YAdjust(pc->u.xxy.y2);
      pc = NEXTOP(pc,xxy);
      break;
      /* instructions type y */
    case _save_b_y:
    case _write_y_loc:
    case _write_y_val:
    case _write_y_var:
      pc->u.y.y = YAdjust(pc->u.y.y);
      pc = NEXTOP(pc,y);
      break;
      /* instructions type yl */
    case _p_atom_y:
    case _p_atomic_y:
    case _p_compound_y:
    case _p_cut_by_y:
    case _p_db_ref_y:
    case _p_float_y:
    case _p_integer_y:
    case _p_nonvar_y:
    case _p_number_y:
    case _p_primitive_y:
    case _p_var_y:
      pc->u.yl.y = YAdjust(pc->u.yl.y);
      pc->u.yl.F = PtoOpAdjust(pc->u.yl.F);
      pc = NEXTOP(pc,yl);
      break;
      /* instructions type yp */
    case _commit_b_y:
      pc->u.yp.y = YAdjust(pc->u.yp.y);
      pc->u.yp.p0 = PtoPredAdjust(pc->u.yp.p0);
      pc = NEXTOP(pc,yp);
      break;
      /* instructions type yx */
    case _get_y_val:
    case _get_y_var:
    case _gl_void_valy:
    case _gl_void_vary:
    case _glist_valy:
    case _put_unsafe:
    case _put_y_val:
    case _put_y_var:
      pc->u.yx.y = YAdjust(pc->u.yx.y);
      pc->u.yx.x = XAdjust(pc->u.yx.x);
      pc = NEXTOP(pc,yx);
      break;
      /* instructions type yxn */
    case _p_and_y_vc:
    case _p_arg_y_cv:
    case _p_div_y_cv:
    case _p_div_y_vc:
    case _p_func2s_y_cv:
    case _p_func2s_y_vc:
    case _p_minus_y_cv:
    case _p_or_y_vc:
    case _p_plus_y_vc:
    case _p_sll_y_cv:
    case _p_sll_y_vc:
    case _p_slr_y_cv:
    case _p_slr_y_vc:
    case _p_times_y_vc:
      pc->u.yxn.y = YAdjust(pc->u.yxn.y);
      pc->u.yxn.xi = XAdjust(pc->u.yxn.xi);
      pc->u.yxn.c = IntegerAdjust(pc->u.yxn.c);
      pc = NEXTOP(pc,yxn);
      break;
      /* instructions type yxx */
    case _p_and_y_vv:
    case _p_arg_y_vv:
    case _p_div_y_vv:
    case _p_func2f_yx:
    case _p_func2s_y_vv:
    case _p_minus_y_vv:
    case _p_or_y_vv:
    case _p_plus_y_vv:
    case _p_sll_y_vv:
    case _p_slr_y_vv:
    case _p_times_y_vv:
      pc->u.yxx.y = YAdjust(pc->u.yxx.y);
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
      /* instructions type yyxx */
    case _get_yy_var:
    case _put_y_vals:
      pc->u.yyxx.y1 = YAdjust(pc->u.yyxx.y1);
      pc->u.yyxx.y2 = YAdjust(pc->u.yyxx.y2);
      pc->u.yyxx.x1 = XAdjust(pc->u.yyxx.x1);
      pc->u.yyxx.x2 = XAdjust(pc->u.yyxx.x2);
      pc = NEXTOP(pc,yyxx);
      break;
#ifdef YAPOR
      /* instructions type Otapl */
    case _getwork:
    case _getwork_seq:
    case _sync:
      OrArgAdjust(pc->u.Otapl.or_arg);
      TabEntryAdjust(pc->u.Otapl.te);
      pc->u.Otapl.s = ArityAdjust(pc->u.Otapl.s);
      pc->u.Otapl.p = PtoPredAdjust(pc->u.Otapl.p);
      pc->u.Otapl.d = PtoOpAdjust(pc->u.Otapl.d);
      pc = NEXTOP(pc,Otapl);
      break;
      /* instructions type e */
    case _getwork_first_time:
      if (op == _Nstop || op == _copy_idb_term || op == _unify_idb_term) return;
      pc = NEXTOP(pc,e);
      break;
#endif
#ifdef TABLING
      /* instructions type Otapl */
    case _table_answer_resolution:
    case _table_completion:
    case _table_load_answer:
    case _table_retry:
    case _table_retry_me:
    case _table_trust:
    case _table_trust_me:
    case _table_try:
    case _table_try_answer:
    case _table_try_me:
    case _table_try_single:
      OrArgAdjust(pc->u.Otapl.or_arg);
      TabEntryAdjust(pc->u.Otapl.te);
      pc->u.Otapl.s = ArityAdjust(pc->u.Otapl.s);
      pc->u.Otapl.p = PtoPredAdjust(pc->u.Otapl.p);
      pc->u.Otapl.d = PtoOpAdjust(pc->u.Otapl.d);
      pc = NEXTOP(pc,Otapl);
      break;
      /* instructions type e */
#ifdef TABLING_INNER_CUTS
    case _clause_with_cut:
#endif
      if (op == _Nstop || op == _copy_idb_term || op == _unify_idb_term) return;
      pc = NEXTOP(pc,e);
      break;
      /* instructions type s */
    case _table_new_answer:
      pc->u.s.s = ConstantAdjust(pc->u.s.s);
      pc = NEXTOP(pc,s);
      break;
      /* instructions type e */
    case _trie_do_appl:
    case _trie_do_appl_in_pair:
    case _trie_do_atom:
    case _trie_do_atom_in_pair:
    case _trie_do_double:
    case _trie_do_extension:
    case _trie_do_gterm:
    case _trie_do_longint:
    case _trie_do_null:
    case _trie_do_null_in_pair:
    case _trie_do_pair:
    case _trie_do_val:
    case _trie_do_val_in_pair:
    case _trie_do_var:
    case _trie_do_var_in_pair:
    case _trie_retry_appl:
    case _trie_retry_appl_in_pair:
    case _trie_retry_atom:
    case _trie_retry_atom_in_pair:
    case _trie_retry_double:
    case _trie_retry_extension:
    case _trie_retry_gterm:
    case _trie_retry_longint:
    case _trie_retry_null:
    case _trie_retry_null_in_pair:
    case _trie_retry_pair:
    case _trie_retry_val:
    case _trie_retry_val_in_pair:
    case _trie_retry_var:
    case _trie_retry_var_in_pair:
    case _trie_trust_appl:
    case _trie_trust_appl_in_pair:
    case _trie_trust_atom:
    case _trie_trust_atom_in_pair:
    case _trie_trust_double:
    case _trie_trust_extension:
    case _trie_trust_gterm:
    case _trie_trust_longint:
    case _trie_trust_null:
    case _trie_trust_null_in_pair:
    case _trie_trust_pair:
    case _trie_trust_val:
    case _trie_trust_val_in_pair:
    case _trie_trust_var:
    case _trie_trust_var_in_pair:
    case _trie_try_appl:
    case _trie_try_appl_in_pair:
    case _trie_try_atom:
    case _trie_try_atom_in_pair:
    case _trie_try_double:
    case _trie_try_extension:
    case _trie_try_gterm:
    case _trie_try_longint:
    case _trie_try_null:
    case _trie_try_null_in_pair:
    case _trie_try_pair:
    case _trie_try_val:
    case _trie_try_val_in_pair:
    case _trie_try_var:
    case _trie_try_var_in_pair:
      if (op == _Nstop || op == _copy_idb_term || op == _unify_idb_term) return;
      pc = NEXTOP(pc,e);
      break;
#endif
      /* this instruction is hardwired */
    case _or_last:
#ifdef YAPOR
      OrArgAdjust(pc->u.Osblp.or_arg);
      pc->u.Osblp.s = ConstantAdjust(pc->u.Osblp.s);
      pc->u.Osblp.bmap = CellPtoHeapAdjust(pc->u.Osblp.bmap);
      pc->u.Osblp.l = PtoOpAdjust(pc->u.Osblp.l);
      pc->u.Osblp.p0 = PtoPredAdjust(pc->u.Osblp.p0);
      pc = NEXTOP(pc,Osblp);
      break;
#else
      pc->u.p.p = PtoPredAdjust(pc->u.p.p);
      pc = NEXTOP(pc,p);
      break;
#endif
    }
  } while (TRUE);
}
