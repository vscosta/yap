
  /* This file was generated automatically by "yap -L misc/buildops"
     please do not update */


static void 
restore_opcodes(yamop *pc, yamop *max USES_REGS)
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
      pc->y_u.D.D = DBGroundTermAdjust(pc->y_u.D.D);
      pc = NEXTOP(pc,D);
      break;
      /* instructions type Illss */
    case _enter_lu_pred:
      pc->y_u.Illss.I = PtoLUIndexAdjust(pc->y_u.Illss.I);
      pc->y_u.Illss.l1 = PtoOpAdjust(pc->y_u.Illss.l1);
      pc->y_u.Illss.l2 = PtoOpAdjust(pc->y_u.Illss.l2);
      pc->y_u.Illss.s = ConstantAdjust(pc->y_u.Illss.s);
      pc->y_u.Illss.e = ConstantAdjust(pc->y_u.Illss.e);
      opc = NEXTOP(pc,Illss);
      pc = pc->y_u.Illss.l1;
      break;
      /* instructions type J */
#ifdef YAP_JIT
    case _jit_handler:
#endif
      /* instructions type L */
    case _alloc_for_logical_pred:
      pc->y_u.L.ClBase = PtoLUClauseAdjust(pc->y_u.L.ClBase);
      pc = NEXTOP(pc,L);
      break;
      /* instructions type N */
    case _write_bigint:
      pc->y_u.N.b = BlobTermInCodeAdjust(pc->y_u.N.b);
      pc = NEXTOP(pc,N);
      break;
      /* instructions type Osblp */
    case _either:
    case _or_else:
      OrArgAdjust(pc->y_u.Osblp.or_arg);
      pc->y_u.Osblp.s = ConstantAdjust(pc->y_u.Osblp.s);
      pc->y_u.Osblp.bmap = CellPtoHeapAdjust(pc->y_u.Osblp.bmap);
      pc->y_u.Osblp.l = PtoOpAdjust(pc->y_u.Osblp.l);
      pc->y_u.Osblp.p0 = PtoPredAdjust(pc->y_u.Osblp.p0);
      pc = NEXTOP(pc,Osblp);
      break;
      /* instructions type Osbmp */
    case _p_execute:
    case _p_execute_tail:
      OrArgAdjust(pc->y_u.Osbmp.or_arg);
      pc->y_u.Osbmp.s = ConstantAdjust(pc->y_u.Osbmp.s);
      pc->y_u.Osbmp.bmap = CellPtoHeapAdjust(pc->y_u.Osbmp.bmap);
      pc->y_u.Osbmp.mod = ModuleAdjust(pc->y_u.Osbmp.mod);
      pc->y_u.Osbmp.p0 = PtoPredAdjust(pc->y_u.Osbmp.p0);
      pc = NEXTOP(pc,Osbmp);
      break;
      /* instructions type Osbpa */
    case _ensure_space:
      OrArgAdjust(pc->y_u.Osbpa.or_arg);
      pc->y_u.Osbpa.s = ConstantAdjust(pc->y_u.Osbpa.s);
      pc->y_u.Osbpa.bmap = CellPtoHeapAdjust(pc->y_u.Osbpa.bmap);
      pc->y_u.Osbpa.p = PtoPredAdjust(pc->y_u.Osbpa.p);
      pc->y_u.Osbpa.i = ArityAdjust(pc->y_u.Osbpa.i);
      pc = NEXTOP(pc,Osbpa);
      break;
      /* instructions type Osbpp */
    case _call:
    case _call_cpred:
    case _call_usercpred:
    case _dexecute:
    case _execute:
    case _execute_cpred:
    case _fcall:
    case _p_execute2:
      OrArgAdjust(pc->y_u.Osbpp.or_arg);
      pc->y_u.Osbpp.s = ConstantAdjust(pc->y_u.Osbpp.s);
      pc->y_u.Osbpp.bmap = CellPtoHeapAdjust(pc->y_u.Osbpp.bmap);
      pc->y_u.Osbpp.p = PtoPredAdjust(pc->y_u.Osbpp.p);
      pc->y_u.Osbpp.p0 = PtoPredAdjust(pc->y_u.Osbpp.p0);
      pc = NEXTOP(pc,Osbpp);
      break;
      /* instructions type OtILl */
    case _count_trust_logical:
    case _profiled_trust_logical:
    case _trust_logical:
      OrArgAdjust(pc->y_u.OtILl.or_arg);
      TabEntryAdjust(pc->y_u.OtILl.te);
      pc->y_u.OtILl.block = PtoLUIndexAdjust(pc->y_u.OtILl.block);
      pc->y_u.OtILl.d = PtoLUClauseAdjust(pc->y_u.OtILl.d);
      pc->y_u.OtILl.n = PtoOpAdjust(pc->y_u.OtILl.n);
      pc = opc;
      break;
      /* instructions type OtaLl */
    case _count_retry_logical:
    case _profiled_retry_logical:
    case _retry_logical:
    case _try_logical:
      OrArgAdjust(pc->y_u.OtaLl.or_arg);
      TabEntryAdjust(pc->y_u.OtaLl.te);
      pc->y_u.OtaLl.s = ArityAdjust(pc->y_u.OtaLl.s);
      pc->y_u.OtaLl.d = PtoLUClauseAdjust(pc->y_u.OtaLl.d);
      pc->y_u.OtaLl.n = PtoOpAdjust(pc->y_u.OtaLl.n);
      pc = pc->y_u.OtaLl.n;
      break;
      /* instructions type OtapFs */
    case _cut_c:
    case _cut_userc:
    case _retry_c:
    case _retry_userc:
    case _try_c:
    case _try_userc:
      OrArgAdjust(pc->y_u.OtapFs.or_arg);
      TabEntryAdjust(pc->y_u.OtapFs.te);
      pc->y_u.OtapFs.s = ArityAdjust(pc->y_u.OtapFs.s);
      pc->y_u.OtapFs.p = PtoPredAdjust(pc->y_u.OtapFs.p);
      pc->y_u.OtapFs.f = ExternalFunctionAdjust(pc->y_u.OtapFs.f);
      pc->y_u.OtapFs.extra = ConstantAdjust(pc->y_u.OtapFs.extra);
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
      OrArgAdjust(pc->y_u.Otapl.or_arg);
      TabEntryAdjust(pc->y_u.Otapl.te);
      pc->y_u.Otapl.s = ArityAdjust(pc->y_u.Otapl.s);
      pc->y_u.Otapl.p = PtoPredAdjust(pc->y_u.Otapl.p);
      pc->y_u.Otapl.d = PtoOpAdjust(pc->y_u.Otapl.d);
      pc = NEXTOP(pc,Otapl);
      break;
      /* instructions type c */
    case _write_atom:
      pc->y_u.c.c = ConstantTermAdjust(pc->y_u.c.c);
      pc = NEXTOP(pc,c);
      break;
      /* instructions type cc */
    case _get_2atoms:
      pc->y_u.cc.c1 = ConstantTermAdjust(pc->y_u.cc.c1);
      pc->y_u.cc.c2 = ConstantTermAdjust(pc->y_u.cc.c2);
      pc = NEXTOP(pc,cc);
      break;
      /* instructions type ccc */
    case _get_3atoms:
      pc->y_u.ccc.c1 = ConstantTermAdjust(pc->y_u.ccc.c1);
      pc->y_u.ccc.c2 = ConstantTermAdjust(pc->y_u.ccc.c2);
      pc->y_u.ccc.c3 = ConstantTermAdjust(pc->y_u.ccc.c3);
      pc = NEXTOP(pc,ccc);
      break;
      /* instructions type cccc */
    case _get_4atoms:
      pc->y_u.cccc.c1 = ConstantTermAdjust(pc->y_u.cccc.c1);
      pc->y_u.cccc.c2 = ConstantTermAdjust(pc->y_u.cccc.c2);
      pc->y_u.cccc.c3 = ConstantTermAdjust(pc->y_u.cccc.c3);
      pc->y_u.cccc.c4 = ConstantTermAdjust(pc->y_u.cccc.c4);
      pc = NEXTOP(pc,cccc);
      break;
      /* instructions type ccccc */
    case _get_5atoms:
      pc->y_u.ccccc.c1 = ConstantTermAdjust(pc->y_u.ccccc.c1);
      pc->y_u.ccccc.c2 = ConstantTermAdjust(pc->y_u.ccccc.c2);
      pc->y_u.ccccc.c3 = ConstantTermAdjust(pc->y_u.ccccc.c3);
      pc->y_u.ccccc.c4 = ConstantTermAdjust(pc->y_u.ccccc.c4);
      pc->y_u.ccccc.c5 = ConstantTermAdjust(pc->y_u.ccccc.c5);
      pc = NEXTOP(pc,ccccc);
      break;
      /* instructions type cccccc */
    case _get_6atoms:
      pc->y_u.cccccc.c1 = ConstantTermAdjust(pc->y_u.cccccc.c1);
      pc->y_u.cccccc.c2 = ConstantTermAdjust(pc->y_u.cccccc.c2);
      pc->y_u.cccccc.c3 = ConstantTermAdjust(pc->y_u.cccccc.c3);
      pc->y_u.cccccc.c4 = ConstantTermAdjust(pc->y_u.cccccc.c4);
      pc->y_u.cccccc.c5 = ConstantTermAdjust(pc->y_u.cccccc.c5);
      pc->y_u.cccccc.c6 = ConstantTermAdjust(pc->y_u.cccccc.c6);
      pc = NEXTOP(pc,cccccc);
      break;
      /* instructions type clll */
    case _if_not_then:
      pc->y_u.clll.c = ConstantTermAdjust(pc->y_u.clll.c);
      pc->y_u.clll.l1 = PtoOpAdjust(pc->y_u.clll.l1);
      pc->y_u.clll.l2 = PtoOpAdjust(pc->y_u.clll.l2);
      pc->y_u.clll.l3 = PtoOpAdjust(pc->y_u.clll.l3);
      pc = NEXTOP(pc,clll);
      break;
      /* instructions type d */
    case _write_float:
      DoubleInCodeAdjust(pc->y_u.d.d);
      pc = NEXTOP(pc,d);
      break;
      /* instructions type e */
    case _Nstop:
    case _allocate:
    case _copy_idb_term:
    case _enter_exo:
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
      pc->y_u.fa.f = FuncAdjust(pc->y_u.fa.f);
      pc->y_u.fa.a = ArityAdjust(pc->y_u.fa.a);
      pc = NEXTOP(pc,fa);
      break;
      /* instructions type i */
    case _write_longint:
      IntegerInCodeAdjust(pc->y_u.i.i);
      pc = NEXTOP(pc,i);
      break;
      /* instructions type l */
    case _Ystop:
    case _jump:
    case _jump_if_var:
    case _move_back:
    case _retry2:
    case _retry3:
    case _retry4:
    case _skip:
    case _try_clause2:
    case _try_clause3:
    case _try_clause4:
    case _try_in:
      pc->y_u.l.l = PtoOpAdjust(pc->y_u.l.l);
      if (op == _Ystop) return;
      pc = NEXTOP(pc,l);
      break;
      /* instructions type llll */
    case _switch_on_type:
      pc->y_u.llll.l1 = PtoOpAdjust(pc->y_u.llll.l1);
      pc->y_u.llll.l2 = PtoOpAdjust(pc->y_u.llll.l2);
      pc->y_u.llll.l3 = PtoOpAdjust(pc->y_u.llll.l3);
      pc->y_u.llll.l4 = PtoOpAdjust(pc->y_u.llll.l4);
      pc = NEXTOP(pc,llll);
      break;
      /* instructions type lp */
    case _retry_all_exo:
    case _retry_exo:
    case _retry_exo_udi:
    case _try_all_exo:
    case _try_exo:
    case _try_exo_udi:
    case _user_switch:
      pc->y_u.lp.l = PtoOpAdjust(pc->y_u.lp.l);
      pc->y_u.lp.p = PtoPredAdjust(pc->y_u.lp.p);
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
      pc->y_u.o.opcw = OpcodeAdjust(pc->y_u.o.opcw);
      pc = NEXTOP(pc,o);
      break;
      /* instructions type oD */
    case _unify_dbterm:
    case _unify_l_dbterm:
      pc->y_u.oD.opcw = OpcodeAdjust(pc->y_u.oD.opcw);
      pc->y_u.oD.D = DBGroundTermAdjust(pc->y_u.oD.D);
      pc = NEXTOP(pc,oD);
      break;
      /* instructions type oN */
    case _unify_bigint:
    case _unify_l_bigint:
      pc->y_u.oN.opcw = OpcodeAdjust(pc->y_u.oN.opcw);
      pc->y_u.oN.b = BlobTermInCodeAdjust(pc->y_u.oN.b);
      pc = NEXTOP(pc,oN);
      break;
      /* instructions type oc */
    case _unify_atom:
    case _unify_atom_write:
    case _unify_l_atom:
    case _unify_l_atom_write:
      pc->y_u.oc.opcw = OpcodeAdjust(pc->y_u.oc.opcw);
      pc->y_u.oc.c = ConstantTermAdjust(pc->y_u.oc.c);
      pc = NEXTOP(pc,oc);
      break;
      /* instructions type od */
    case _unify_float:
    case _unify_float_write:
    case _unify_l_float:
    case _unify_l_float_write:
      pc->y_u.od.opcw = OpcodeAdjust(pc->y_u.od.opcw);
      DoubleInCodeAdjust(pc->y_u.od.d);
      pc = NEXTOP(pc,od);
      break;
      /* instructions type ofa */
    case _unify_l_struc:
    case _unify_l_struc_write:
    case _unify_struct:
    case _unify_struct_write:
      pc->y_u.ofa.opcw = OpcodeAdjust(pc->y_u.ofa.opcw);
      pc->y_u.ofa.f = FuncAdjust(pc->y_u.ofa.f);
      pc->y_u.ofa.a = ArityAdjust(pc->y_u.ofa.a);
      pc = NEXTOP(pc,ofa);
      break;
      /* instructions type oi */
    case _unify_l_longint:
    case _unify_l_longint_write:
    case _unify_longint:
    case _unify_longint_write:
      pc->y_u.oi.opcw = OpcodeAdjust(pc->y_u.oi.opcw);
      IntegerInCodeAdjust(pc->y_u.oi.i);
      pc = NEXTOP(pc,oi);
      break;
      /* instructions type ollll */
    case _switch_list_nl:
      pc->y_u.ollll.pop = OpcodeAdjust(pc->y_u.ollll.pop);
      pc->y_u.ollll.l1 = PtoOpAdjust(pc->y_u.ollll.l1);
      pc->y_u.ollll.l2 = PtoOpAdjust(pc->y_u.ollll.l2);
      pc->y_u.ollll.l3 = PtoOpAdjust(pc->y_u.ollll.l3);
      pc->y_u.ollll.l4 = PtoOpAdjust(pc->y_u.ollll.l4);
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
      pc->y_u.os.opcw = OpcodeAdjust(pc->y_u.os.opcw);
      pc->y_u.os.s = ConstantAdjust(pc->y_u.os.s);
      pc = NEXTOP(pc,os);
      break;
      /* instructions type osc */
    case _unify_n_atoms:
    case _unify_n_atoms_write:
      pc->y_u.osc.opcw = OpcodeAdjust(pc->y_u.osc.opcw);
      pc->y_u.osc.s = ConstantAdjust(pc->y_u.osc.s);
      pc->y_u.osc.c = ConstantTermAdjust(pc->y_u.osc.c);
      pc = NEXTOP(pc,osc);
      break;
      /* instructions type ou */
    case _unify_l_string:
    case _unify_string:
      pc->y_u.ou.opcw = OpcodeAdjust(pc->y_u.ou.opcw);
      pc->y_u.ou.ut = BlobTermInCodeAdjust(pc->y_u.ou.ut);
      pc = NEXTOP(pc,ou);
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
      pc->y_u.ox.opcw = OpcodeAdjust(pc->y_u.ox.opcw);
      pc->y_u.ox.x = XAdjust(pc->y_u.ox.x);
      pc = NEXTOP(pc,ox);
      break;
      /* instructions type oxx */
    case _unify_l_x_var2:
    case _unify_l_x_var2_write:
    case _unify_x_var2:
    case _unify_x_var2_write:
      pc->y_u.oxx.opcw = OpcodeAdjust(pc->y_u.oxx.opcw);
      pc->y_u.oxx.xl = XAdjust(pc->y_u.oxx.xl);
      pc->y_u.oxx.xr = XAdjust(pc->y_u.oxx.xr);
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
      pc->y_u.oy.opcw = OpcodeAdjust(pc->y_u.oy.opcw);
      pc->y_u.oy.y = YAdjust(pc->y_u.oy.y);
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
    case _retry_udi:
    case _try_udi:
      pc->y_u.p.p = PtoPredAdjust(pc->y_u.p.p);
      pc = NEXTOP(pc,p);
      break;
      /* instructions type plxxs */
    case _call_bfunc_xx:
      pc->y_u.plxxs.p = PtoPredAdjust(pc->y_u.plxxs.p);
      pc->y_u.plxxs.f = PtoOpAdjust(pc->y_u.plxxs.f);
      pc->y_u.plxxs.x1 = XAdjust(pc->y_u.plxxs.x1);
      pc->y_u.plxxs.x2 = XAdjust(pc->y_u.plxxs.x2);
      pc->y_u.plxxs.flags = ConstantAdjust(pc->y_u.plxxs.flags);
      pc = NEXTOP(pc,plxxs);
      break;
      /* instructions type plxys */
    case _call_bfunc_xy:
    case _call_bfunc_yx:
      pc->y_u.plxys.p = PtoPredAdjust(pc->y_u.plxys.p);
      pc->y_u.plxys.f = PtoOpAdjust(pc->y_u.plxys.f);
      pc->y_u.plxys.x = XAdjust(pc->y_u.plxys.x);
      pc->y_u.plxys.y = YAdjust(pc->y_u.plxys.y);
      pc->y_u.plxys.flags = ConstantAdjust(pc->y_u.plxys.flags);
      pc = NEXTOP(pc,plxys);
      break;
      /* instructions type plyys */
    case _call_bfunc_yy:
      pc->y_u.plyys.p = PtoPredAdjust(pc->y_u.plyys.p);
      pc->y_u.plyys.f = PtoOpAdjust(pc->y_u.plyys.f);
      pc->y_u.plyys.y1 = YAdjust(pc->y_u.plyys.y1);
      pc->y_u.plyys.y2 = YAdjust(pc->y_u.plyys.y2);
      pc->y_u.plyys.flags = ConstantAdjust(pc->y_u.plyys.flags);
      pc = NEXTOP(pc,plyys);
      break;
      /* instructions type s */
    case _cut:
    case _cut_e:
    case _cut_t:
    case _pop_n:
    case _write_n_voids:
      pc->y_u.s.s = ConstantAdjust(pc->y_u.s.s);
      pc = NEXTOP(pc,s);
      break;
      /* instructions type sc */
    case _write_n_atoms:
      pc->y_u.sc.s = ConstantAdjust(pc->y_u.sc.s);
      pc->y_u.sc.c = ConstantTermAdjust(pc->y_u.sc.c);
      pc = NEXTOP(pc,sc);
      break;
      /* instructions type sllll */
    case _switch_on_sub_arg_type:
      pc->y_u.sllll.s = ConstantAdjust(pc->y_u.sllll.s);
      pc->y_u.sllll.l1 = PtoOpAdjust(pc->y_u.sllll.l1);
      pc->y_u.sllll.l2 = PtoOpAdjust(pc->y_u.sllll.l2);
      pc->y_u.sllll.l3 = PtoOpAdjust(pc->y_u.sllll.l3);
      pc->y_u.sllll.l4 = PtoOpAdjust(pc->y_u.sllll.l4);
      pc = NEXTOP(pc,sllll);
      break;
      /* instructions type slpp */
    case _call_c_wfail:
      pc->y_u.slpp.s = ConstantAdjust(pc->y_u.slpp.s);
      pc->y_u.slpp.l = PtoOpAdjust(pc->y_u.slpp.l);
      pc->y_u.slpp.p = PtoPredAdjust(pc->y_u.slpp.p);
      pc->y_u.slpp.p0 = PtoPredAdjust(pc->y_u.slpp.p0);
      pc = NEXTOP(pc,slpp);
      break;
      /* instructions type sssl */
    case _go_on_cons:
    case _go_on_func:
    case _if_cons:
    case _if_func:
    case _switch_on_cons:
    case _switch_on_func:
      pc->y_u.sssl.s = ConstantAdjust(pc->y_u.sssl.s);
      pc->y_u.sssl.e = ConstantAdjust(pc->y_u.sssl.e);
      pc->y_u.sssl.w = ConstantAdjust(pc->y_u.sssl.w);
      pc->y_u.sssl.l = PtoOpAdjust(pc->y_u.sssl.l);
      AdjustSwitchTable(op, pc->y_u.sssl.l, pc->y_u.sssl.s);
      pc = NEXTOP(pc,sssl);
      break;
      /* instructions type sssllp */
    case _expand_clauses:
      pc->y_u.sssllp.s1 = ConstantAdjust(pc->y_u.sssllp.s1);
      pc->y_u.sssllp.s2 = ConstantAdjust(pc->y_u.sssllp.s2);
      pc->y_u.sssllp.s3 = ConstantAdjust(pc->y_u.sssllp.s3);
      pc->y_u.sssllp.sprev = PtoOpAdjust(pc->y_u.sssllp.sprev);
      pc->y_u.sssllp.snext = PtoOpAdjust(pc->y_u.sssllp.snext);
      pc->y_u.sssllp.p = PtoPredAdjust(pc->y_u.sssllp.p);
      pc = NEXTOP(pc,sssllp);
      break;
      /* instructions type x */
    case _get_atom_exo:
    case _get_list:
    case _put_list:
    case _save_b_x:
    case _write_x_loc:
    case _write_x_val:
    case _write_x_var:
      pc->y_u.x.x = XAdjust(pc->y_u.x.x);
      pc = NEXTOP(pc,x);
      break;
      /* instructions type xD */
    case _get_dbterm:
    case _put_dbterm:
      pc->y_u.xD.x = XAdjust(pc->y_u.xD.x);
      pc->y_u.xD.D = DBGroundTermAdjust(pc->y_u.xD.D);
      pc = NEXTOP(pc,xD);
      break;
      /* instructions type xN */
    case _get_bigint:
    case _put_bigint:
      pc->y_u.xN.x = XAdjust(pc->y_u.xN.x);
      pc->y_u.xN.b = BlobTermInCodeAdjust(pc->y_u.xN.b);
      pc = NEXTOP(pc,xN);
      break;
      /* instructions type xc */
    case _get_atom:
    case _put_atom:
      pc->y_u.xc.x = XAdjust(pc->y_u.xc.x);
      pc->y_u.xc.c = ConstantTermAdjust(pc->y_u.xc.c);
      pc = NEXTOP(pc,xc);
      break;
      /* instructions type xd */
    case _get_float:
    case _put_float:
      pc->y_u.xd.x = XAdjust(pc->y_u.xd.x);
      DoubleInCodeAdjust(pc->y_u.xd.d);
      pc = NEXTOP(pc,xd);
      break;
      /* instructions type xfa */
    case _get_struct:
    case _put_struct:
      pc->y_u.xfa.x = XAdjust(pc->y_u.xfa.x);
      pc->y_u.xfa.f = FuncAdjust(pc->y_u.xfa.f);
      pc->y_u.xfa.a = ArityAdjust(pc->y_u.xfa.a);
      pc = NEXTOP(pc,xfa);
      break;
      /* instructions type xi */
    case _get_longint:
    case _put_longint:
      pc->y_u.xi.x = XAdjust(pc->y_u.xi.x);
      IntegerInCodeAdjust(pc->y_u.xi.i);
      pc = NEXTOP(pc,xi);
      break;
      /* instructions type xl */
    case _p_atom_x:
    case _p_atomic_x:
    case _p_compound_x:
    case _p_db_ref_x:
    case _p_float_x:
    case _p_integer_x:
    case _p_nonvar_x:
    case _p_number_x:
    case _p_primitive_x:
    case _p_var_x:
      pc->y_u.xl.x = XAdjust(pc->y_u.xl.x);
      pc->y_u.xl.F = PtoOpAdjust(pc->y_u.xl.F);
      pc = NEXTOP(pc,xl);
      break;
      /* instructions type xll */
    case _jump_if_nonvar:
      pc->y_u.xll.x = XAdjust(pc->y_u.xll.x);
      pc->y_u.xll.l1 = PtoOpAdjust(pc->y_u.xll.l1);
      pc->y_u.xll.l2 = PtoOpAdjust(pc->y_u.xll.l2);
      pc = NEXTOP(pc,xll);
      break;
      /* instructions type xllll */
    case _switch_on_arg_type:
      pc->y_u.xllll.x = XAdjust(pc->y_u.xllll.x);
      pc->y_u.xllll.l1 = PtoOpAdjust(pc->y_u.xllll.l1);
      pc->y_u.xllll.l2 = PtoOpAdjust(pc->y_u.xllll.l2);
      pc->y_u.xllll.l3 = PtoOpAdjust(pc->y_u.xllll.l3);
      pc->y_u.xllll.l4 = PtoOpAdjust(pc->y_u.xllll.l4);
      pc = NEXTOP(pc,xllll);
      break;
      /* instructions type xps */
    case _commit_b_x:
      pc->y_u.xps.x = XAdjust(pc->y_u.xps.x);
      pc->y_u.xps.p0 = PtoPredAdjust(pc->y_u.xps.p0);
      pc->y_u.xps.s = ConstantAdjust(pc->y_u.xps.s);
      pc = NEXTOP(pc,xps);
      break;
      /* instructions type xu */
    case _get_string:
      pc->y_u.xu.x = XAdjust(pc->y_u.xu.x);
      pc->y_u.xu.ut = BlobTermInCodeAdjust(pc->y_u.xu.ut);
      pc = NEXTOP(pc,xu);
      break;
      /* instructions type xx */
    case _get_x_val:
    case _get_x_var:
    case _gl_void_valx:
    case _gl_void_varx:
    case _glist_valx:
    case _put_x_val:
    case _put_x_var:
      pc->y_u.xx.xl = XAdjust(pc->y_u.xx.xl);
      pc->y_u.xx.xr = XAdjust(pc->y_u.xx.xr);
      pc = NEXTOP(pc,xx);
      break;
      /* instructions type xxc */
    case _p_func2s_cv:
      pc->y_u.xxc.x = XAdjust(pc->y_u.xxc.x);
      pc->y_u.xxc.xi = XAdjust(pc->y_u.xxc.xi);
      pc->y_u.xxc.c = ConstantTermAdjust(pc->y_u.xxc.c);
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
      pc->y_u.xxn.x = XAdjust(pc->y_u.xxn.x);
      pc->y_u.xxn.xi = XAdjust(pc->y_u.xxn.xi);
      pc->y_u.xxn.c = IntegerAdjust(pc->y_u.xxn.c);
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
      pc->y_u.xxx.x = XAdjust(pc->y_u.xxx.x);
      pc->y_u.xxx.x1 = XAdjust(pc->y_u.xxx.x1);
      pc->y_u.xxx.x2 = XAdjust(pc->y_u.xxx.x2);
      pc = NEXTOP(pc,xxx);
      break;
      /* instructions type xxxx */
    case _put_xx_val:
      pc->y_u.xxxx.xl1 = XAdjust(pc->y_u.xxxx.xl1);
      pc->y_u.xxxx.xl2 = XAdjust(pc->y_u.xxxx.xl2);
      pc->y_u.xxxx.xr1 = XAdjust(pc->y_u.xxxx.xr1);
      pc->y_u.xxxx.xr2 = XAdjust(pc->y_u.xxxx.xr2);
      pc = NEXTOP(pc,xxxx);
      break;
      /* instructions type xxy */
    case _p_func2f_xy:
      pc->y_u.xxy.x = XAdjust(pc->y_u.xxy.x);
      pc->y_u.xxy.x1 = XAdjust(pc->y_u.xxy.x1);
      pc->y_u.xxy.y2 = YAdjust(pc->y_u.xxy.y2);
      pc = NEXTOP(pc,xxy);
      break;
      /* instructions type y */
    case _save_b_y:
    case _write_y_loc:
    case _write_y_val:
    case _write_y_var:
      pc->y_u.y.y = YAdjust(pc->y_u.y.y);
      pc = NEXTOP(pc,y);
      break;
      /* instructions type yl */
    case _p_atom_y:
    case _p_atomic_y:
    case _p_compound_y:
    case _p_db_ref_y:
    case _p_float_y:
    case _p_integer_y:
    case _p_nonvar_y:
    case _p_number_y:
    case _p_primitive_y:
    case _p_var_y:
      pc->y_u.yl.y = YAdjust(pc->y_u.yl.y);
      pc->y_u.yl.F = PtoOpAdjust(pc->y_u.yl.F);
      pc = NEXTOP(pc,yl);
      break;
      /* instructions type yps */
    case _commit_b_y:
      pc->y_u.yps.y = YAdjust(pc->y_u.yps.y);
      pc->y_u.yps.p0 = PtoPredAdjust(pc->y_u.yps.p0);
      pc->y_u.yps.s = ConstantAdjust(pc->y_u.yps.s);
      pc = NEXTOP(pc,yps);
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
      pc->y_u.yx.y = YAdjust(pc->y_u.yx.y);
      pc->y_u.yx.x = XAdjust(pc->y_u.yx.x);
      pc = NEXTOP(pc,yx);
      break;
      /* instructions type yxc */
    case _p_func2s_y_cv:
      pc->y_u.yxc.y = YAdjust(pc->y_u.yxc.y);
      pc->y_u.yxc.xi = XAdjust(pc->y_u.yxc.xi);
      pc->y_u.yxc.c = ConstantTermAdjust(pc->y_u.yxc.c);
      pc = NEXTOP(pc,yxc);
      break;
      /* instructions type yxn */
    case _p_and_y_vc:
    case _p_arg_y_cv:
    case _p_div_y_cv:
    case _p_div_y_vc:
    case _p_func2s_y_vc:
    case _p_minus_y_cv:
    case _p_or_y_vc:
    case _p_plus_y_vc:
    case _p_sll_y_cv:
    case _p_sll_y_vc:
    case _p_slr_y_cv:
    case _p_slr_y_vc:
    case _p_times_y_vc:
      pc->y_u.yxn.y = YAdjust(pc->y_u.yxn.y);
      pc->y_u.yxn.xi = XAdjust(pc->y_u.yxn.xi);
      pc->y_u.yxn.c = IntegerAdjust(pc->y_u.yxn.c);
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
      pc->y_u.yxx.y = YAdjust(pc->y_u.yxx.y);
      pc->y_u.yxx.x1 = XAdjust(pc->y_u.yxx.x1);
      pc->y_u.yxx.x2 = XAdjust(pc->y_u.yxx.x2);
      pc = NEXTOP(pc,yxx);
      break;
      /* instructions type yyx */
    case _p_func2f_yy:
      pc->y_u.yyx.y1 = YAdjust(pc->y_u.yyx.y1);
      pc->y_u.yyx.y2 = YAdjust(pc->y_u.yyx.y2);
      pc->y_u.yyx.x = XAdjust(pc->y_u.yyx.x);
      pc = NEXTOP(pc,yyx);
      break;
      /* instructions type yyxx */
    case _get_yy_var:
    case _put_y_vals:
      pc->y_u.yyxx.y1 = YAdjust(pc->y_u.yyxx.y1);
      pc->y_u.yyxx.y2 = YAdjust(pc->y_u.yyxx.y2);
      pc->y_u.yyxx.x1 = XAdjust(pc->y_u.yyxx.x1);
      pc->y_u.yyxx.x2 = XAdjust(pc->y_u.yyxx.x2);
      pc = NEXTOP(pc,yyxx);
      break;
#ifdef YAPOR
      /* instructions type Otapl */
    case _getwork:
    case _getwork_seq:
    case _sync:
      OrArgAdjust(pc->y_u.Otapl.or_arg);
      TabEntryAdjust(pc->y_u.Otapl.te);
      pc->y_u.Otapl.s = ArityAdjust(pc->y_u.Otapl.s);
      pc->y_u.Otapl.p = PtoPredAdjust(pc->y_u.Otapl.p);
      pc->y_u.Otapl.d = PtoOpAdjust(pc->y_u.Otapl.d);
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
#ifdef THREADS_CONSUMER_SHARING
    case _table_answer_resolution_completion:
#endif
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
      OrArgAdjust(pc->y_u.Otapl.or_arg);
      TabEntryAdjust(pc->y_u.Otapl.te);
      pc->y_u.Otapl.s = ArityAdjust(pc->y_u.Otapl.s);
      pc->y_u.Otapl.p = PtoPredAdjust(pc->y_u.Otapl.p);
      pc->y_u.Otapl.d = PtoOpAdjust(pc->y_u.Otapl.d);
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
      pc->y_u.s.s = ConstantAdjust(pc->y_u.s.s);
      pc = NEXTOP(pc,s);
      break;
      /* instructions type e */
    case _trie_do_appl:
    case _trie_do_appl_in_pair:
    case _trie_do_atom:
    case _trie_do_atom_in_pair:
    case _trie_do_bigint:
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
    case _trie_retry_bigint:
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
    case _trie_trust_bigint:
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
    case _trie_try_bigint:
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
      OrArgAdjust(pc->y_u.Osblp.or_arg);
      pc->y_u.Osblp.s = ConstantAdjust(pc->y_u.Osblp.s);
      pc->y_u.Osblp.bmap = CellPtoHeapAdjust(pc->y_u.Osblp.bmap);
      pc->y_u.Osblp.l = PtoOpAdjust(pc->y_u.Osblp.l);
      pc->y_u.Osblp.p0 = PtoPredAdjust(pc->y_u.Osblp.p0);
      pc = NEXTOP(pc,Osblp);
      break;
#else
      pc->y_u.p.p = PtoPredAdjust(pc->y_u.p.p);
      pc = NEXTOP(pc,p);
      break;
#endif
    }
  } while (TRUE);
}
