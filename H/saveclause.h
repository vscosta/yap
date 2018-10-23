
  /* This file was generated automatically by "yap -L misc/buildops"
     please do not update */


  while (TRUE) {
    op_numbers op;
    if (max && pc >= max) return 1;
    op = Yap_op_from_opcode(pc->opc);
    save_Opcode(stream, op);

    /* C-code, maybe indexing */
    switch (op) {
      /* instructions type D */
    case _write_dbterm:
      CHECK(save_DBGroundTerm(stream, pc->y_u.D.D));
      pc = NEXTOP(pc,D);
      break;
      /* instructions type Illss */
    case _enter_lu_pred:
      CHECK(save_PtoLUIndex(stream, pc->y_u.Illss.I));
      CHECK(save_PtoOp(stream, pc->y_u.Illss.l1));
      CHECK(save_PtoOp(stream, pc->y_u.Illss.l2));
      CHECK(save_Constant(stream, pc->y_u.Illss.s));
      CHECK(save_Constant(stream, pc->y_u.Illss.e));
      pc = NEXTOP(pc,Illss);
      break;
      /* instructions type J */
#ifdef YAP_JIT
    case _jit_handler:
#endif
      /* instructions type L */
    case _alloc_for_logical_pred:
      CHECK(save_PtoLUClause(stream, pc->y_u.L.ClBase));
      pc = NEXTOP(pc,L);
      break;
      /* instructions type N */
    case _write_bigint:
      CHECK(save_BlobTermInCode(stream, pc->y_u.N.b));
      pc = NEXTOP(pc,N);
      break;
      /* instructions type Osblp */
    case _either:
    case _or_else:
#ifdef YAPOR
      CHECK(save_OrArg(stream, pc->y_u.Osblp.or_arg));
#endif
      CHECK(save_Constant(stream, pc->y_u.Osblp.s));
      CHECK(save_CellPtoHeap(stream, pc->y_u.Osblp.bmap));
      CHECK(save_PtoOp(stream, pc->y_u.Osblp.l));
      CHECK(save_PtoPred(stream, pc->y_u.Osblp.p0));
      pc = NEXTOP(pc,Osblp);
      break;
      /* instructions type Osbmp */
    case _p_execute:
    case _p_execute_tail:
#ifdef YAPOR
      CHECK(save_OrArg(stream, pc->y_u.Osbmp.or_arg));
#endif
      CHECK(save_Constant(stream, pc->y_u.Osbmp.s));
      CHECK(save_CellPtoHeap(stream, pc->y_u.Osbmp.bmap));
      CHECK(save_Module(stream, pc->y_u.Osbmp.mod));
      CHECK(save_PtoPred(stream, pc->y_u.Osbmp.p0));
      pc = NEXTOP(pc,Osbmp);
      break;
      /* instructions type Osbpa */
    case _ensure_space:
#ifdef YAPOR
      CHECK(save_OrArg(stream, pc->y_u.Osbpa.or_arg));
#endif
      CHECK(save_Constant(stream, pc->y_u.Osbpa.s));
      CHECK(save_CellPtoHeap(stream, pc->y_u.Osbpa.bmap));
      CHECK(save_PtoPred(stream, pc->y_u.Osbpa.p));
      CHECK(save_Arity(stream, pc->y_u.Osbpa.i));
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
#ifdef YAPOR
      CHECK(save_OrArg(stream, pc->y_u.Osbpp.or_arg));
#endif
      CHECK(save_Constant(stream, pc->y_u.Osbpp.s));
      CHECK(save_CellPtoHeap(stream, pc->y_u.Osbpp.bmap));
      CHECK(save_PtoPred(stream, pc->y_u.Osbpp.p));
      CHECK(save_PtoPred(stream, pc->y_u.Osbpp.p0));
      pc = NEXTOP(pc,Osbpp);
      break;
      /* instructions type OtILl */
    case _count_trust_logical:
    case _profiled_trust_logical:
    case _trust_logical:
#ifdef YAPOR
      CHECK(save_OrArg(stream, pc->y_u.OtILl.or_arg));
#endif
#ifdef TABLING
      CHECK(save_TabEntry(stream, pc->y_u.OtILl.te));
#endif
      CHECK(save_PtoLUIndex(stream, pc->y_u.OtILl.block));
      CHECK(save_PtoLUClause(stream, pc->y_u.OtILl.d));
      CHECK(save_PtoOp(stream, pc->y_u.OtILl.n));
      pc = NEXTOP(pc,OtILl);
      break;
      /* instructions type OtaLl */
    case _count_retry_logical:
    case _profiled_retry_logical:
    case _retry_logical:
    case _try_logical:
#ifdef YAPOR
      CHECK(save_OrArg(stream, pc->y_u.OtaLl.or_arg));
#endif
#ifdef TABLING
      CHECK(save_TabEntry(stream, pc->y_u.OtaLl.te));
#endif
      CHECK(save_Arity(stream, pc->y_u.OtaLl.s));
      CHECK(save_PtoLUClause(stream, pc->y_u.OtaLl.d));
      CHECK(save_PtoOp(stream, pc->y_u.OtaLl.n));
      pc = NEXTOP(pc,OtaLl);
      break;
      /* instructions type OtapFs */
    case _cut_c:
    case _cut_userc:
    case _retry_c:
    case _retry_userc:
    case _try_c:
    case _try_userc:
#ifdef YAPOR
      CHECK(save_OrArg(stream, pc->y_u.OtapFs.or_arg));
#endif
#ifdef TABLING
      CHECK(save_TabEntry(stream, pc->y_u.OtapFs.te));
#endif
      CHECK(save_Arity(stream, pc->y_u.OtapFs.s));
      CHECK(save_PtoPred(stream, pc->y_u.OtapFs.p));
      CHECK(save_ExternalFunction(stream, pc->y_u.OtapFs.f));
      CHECK(save_Constant(stream, pc->y_u.OtapFs.extra));
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
#ifdef YAPOR
      CHECK(save_OrArg(stream, pc->y_u.Otapl.or_arg));
#endif
#ifdef TABLING
      CHECK(save_TabEntry(stream, pc->y_u.Otapl.te));
#endif
      CHECK(save_Arity(stream, pc->y_u.Otapl.s));
      CHECK(save_PtoPred(stream, pc->y_u.Otapl.p));
      CHECK(save_PtoOp(stream, pc->y_u.Otapl.d));
      pc = NEXTOP(pc,Otapl);
      break;
      /* instructions type c */
    case _write_atom:
      CHECK(save_ConstantTerm(stream, pc->y_u.c.c));
      pc = NEXTOP(pc,c);
      break;
      /* instructions type cc */
    case _get_2atoms:
      CHECK(save_ConstantTerm(stream, pc->y_u.cc.c1));
      CHECK(save_ConstantTerm(stream, pc->y_u.cc.c2));
      pc = NEXTOP(pc,cc);
      break;
      /* instructions type ccc */
    case _get_3atoms:
      CHECK(save_ConstantTerm(stream, pc->y_u.ccc.c1));
      CHECK(save_ConstantTerm(stream, pc->y_u.ccc.c2));
      CHECK(save_ConstantTerm(stream, pc->y_u.ccc.c3));
      pc = NEXTOP(pc,ccc);
      break;
      /* instructions type cccc */
    case _get_4atoms:
      CHECK(save_ConstantTerm(stream, pc->y_u.cccc.c1));
      CHECK(save_ConstantTerm(stream, pc->y_u.cccc.c2));
      CHECK(save_ConstantTerm(stream, pc->y_u.cccc.c3));
      CHECK(save_ConstantTerm(stream, pc->y_u.cccc.c4));
      pc = NEXTOP(pc,cccc);
      break;
      /* instructions type ccccc */
    case _get_5atoms:
      CHECK(save_ConstantTerm(stream, pc->y_u.ccccc.c1));
      CHECK(save_ConstantTerm(stream, pc->y_u.ccccc.c2));
      CHECK(save_ConstantTerm(stream, pc->y_u.ccccc.c3));
      CHECK(save_ConstantTerm(stream, pc->y_u.ccccc.c4));
      CHECK(save_ConstantTerm(stream, pc->y_u.ccccc.c5));
      pc = NEXTOP(pc,ccccc);
      break;
      /* instructions type cccccc */
    case _get_6atoms:
      CHECK(save_ConstantTerm(stream, pc->y_u.cccccc.c1));
      CHECK(save_ConstantTerm(stream, pc->y_u.cccccc.c2));
      CHECK(save_ConstantTerm(stream, pc->y_u.cccccc.c3));
      CHECK(save_ConstantTerm(stream, pc->y_u.cccccc.c4));
      CHECK(save_ConstantTerm(stream, pc->y_u.cccccc.c5));
      CHECK(save_ConstantTerm(stream, pc->y_u.cccccc.c6));
      pc = NEXTOP(pc,cccccc);
      break;
      /* instructions type clll */
    case _if_not_then:
      CHECK(save_ConstantTerm(stream, pc->y_u.clll.c));
      CHECK(save_PtoOp(stream, pc->y_u.clll.l1));
      CHECK(save_PtoOp(stream, pc->y_u.clll.l2));
      CHECK(save_PtoOp(stream, pc->y_u.clll.l3));
      pc = NEXTOP(pc,clll);
      break;
      /* instructions type d */
    case _write_float:
      CHECK(save_DoubleInCode(stream, pc->y_u.d.d));
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
      if (op == _Nstop || op == _copy_idb_term || op == _unify_idb_term) return 1;
      pc = NEXTOP(pc,e);
      break;
      /* instructions type fa */
    case _write_l_struc:
    case _write_struct:
      CHECK(save_Func(stream, pc->y_u.fa.f));
      CHECK(save_Arity(stream, pc->y_u.fa.a));
      pc = NEXTOP(pc,fa);
      break;
      /* instructions type i */
    case _write_longint:
      CHECK(save_IntegerInCode(stream, pc->y_u.i.i));
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
      CHECK(save_PtoOp(stream, pc->y_u.l.l));
      pc = NEXTOP(pc,l);
      break;
      /* instructions type llll */
    case _switch_on_type:
      CHECK(save_PtoOp(stream, pc->y_u.llll.l1));
      CHECK(save_PtoOp(stream, pc->y_u.llll.l2));
      CHECK(save_PtoOp(stream, pc->y_u.llll.l3));
      CHECK(save_PtoOp(stream, pc->y_u.llll.l4));
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
      CHECK(save_PtoOp(stream, pc->y_u.lp.l));
      CHECK(save_PtoPred(stream, pc->y_u.lp.p));
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
      CHECK(save_Opcode(stream, pc->y_u.o.opcw));
      pc = NEXTOP(pc,o);
      break;
      /* instructions type oD */
    case _unify_dbterm:
    case _unify_l_dbterm:
      CHECK(save_Opcode(stream, pc->y_u.oD.opcw));
      CHECK(save_DBGroundTerm(stream, pc->y_u.oD.D));
      pc = NEXTOP(pc,oD);
      break;
      /* instructions type oN */
    case _unify_bigint:
    case _unify_l_bigint:
      CHECK(save_Opcode(stream, pc->y_u.oN.opcw));
      CHECK(save_BlobTermInCode(stream, pc->y_u.oN.b));
      pc = NEXTOP(pc,oN);
      break;
      /* instructions type oc */
    case _unify_atom:
    case _unify_atom_write:
    case _unify_l_atom:
    case _unify_l_atom_write:
      CHECK(save_Opcode(stream, pc->y_u.oc.opcw));
      CHECK(save_ConstantTerm(stream, pc->y_u.oc.c));
      pc = NEXTOP(pc,oc);
      break;
      /* instructions type od */
    case _unify_float:
    case _unify_float_write:
    case _unify_l_float:
    case _unify_l_float_write:
      CHECK(save_Opcode(stream, pc->y_u.od.opcw));
      CHECK(save_DoubleInCode(stream, pc->y_u.od.d));
      pc = NEXTOP(pc,od);
      break;
      /* instructions type ofa */
    case _unify_l_struc:
    case _unify_l_struc_write:
    case _unify_struct:
    case _unify_struct_write:
      CHECK(save_Opcode(stream, pc->y_u.ofa.opcw));
      CHECK(save_Func(stream, pc->y_u.ofa.f));
      CHECK(save_Arity(stream, pc->y_u.ofa.a));
      pc = NEXTOP(pc,ofa);
      break;
      /* instructions type oi */
    case _unify_l_longint:
    case _unify_l_longint_write:
    case _unify_longint:
    case _unify_longint_write:
      CHECK(save_Opcode(stream, pc->y_u.oi.opcw));
      CHECK(save_IntegerInCode(stream, pc->y_u.oi.i));
      pc = NEXTOP(pc,oi);
      break;
      /* instructions type ollll */
    case _switch_list_nl:
      CHECK(save_Opcode(stream, pc->y_u.ollll.pop));
      CHECK(save_PtoOp(stream, pc->y_u.ollll.l1));
      CHECK(save_PtoOp(stream, pc->y_u.ollll.l2));
      CHECK(save_PtoOp(stream, pc->y_u.ollll.l3));
      CHECK(save_PtoOp(stream, pc->y_u.ollll.l4));
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
      CHECK(save_Opcode(stream, pc->y_u.os.opcw));
      CHECK(save_Constant(stream, pc->y_u.os.s));
      pc = NEXTOP(pc,os);
      break;
      /* instructions type osc */
    case _unify_n_atoms:
    case _unify_n_atoms_write:
      CHECK(save_Opcode(stream, pc->y_u.osc.opcw));
      CHECK(save_Constant(stream, pc->y_u.osc.s));
      CHECK(save_ConstantTerm(stream, pc->y_u.osc.c));
      pc = NEXTOP(pc,osc);
      break;
      /* instructions type ou */
    case _unify_l_string:
    case _unify_string:
      CHECK(save_Opcode(stream, pc->y_u.ou.opcw));
      CHECK(save_BlobTermInCode(stream, pc->y_u.ou.ut));
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
      CHECK(save_Opcode(stream, pc->y_u.ox.opcw));
      CHECK(save_X(stream, pc->y_u.ox.x));
      pc = NEXTOP(pc,ox);
      break;
      /* instructions type oxx */
    case _unify_l_x_var2:
    case _unify_l_x_var2_write:
    case _unify_x_var2:
    case _unify_x_var2_write:
      CHECK(save_Opcode(stream, pc->y_u.oxx.opcw));
      CHECK(save_X(stream, pc->y_u.oxx.xl));
      CHECK(save_X(stream, pc->y_u.oxx.xr));
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
      CHECK(save_Opcode(stream, pc->y_u.oy.opcw));
      CHECK(save_Y(stream, pc->y_u.oy.y));
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
      CHECK(save_PtoPred(stream, pc->y_u.p.p));
      pc = NEXTOP(pc,p);
      break;
      /* instructions type plxxs */
    case _call_bfunc_xx:
      CHECK(save_PtoPred(stream, pc->y_u.plxxs.p));
      CHECK(save_PtoOp(stream, pc->y_u.plxxs.f));
      CHECK(save_X(stream, pc->y_u.plxxs.x1));
      CHECK(save_X(stream, pc->y_u.plxxs.x2));
      CHECK(save_Constant(stream, pc->y_u.plxxs.flags));
      pc = NEXTOP(pc,plxxs);
      break;
      /* instructions type plxys */
    case _call_bfunc_xy:
    case _call_bfunc_yx:
      CHECK(save_PtoPred(stream, pc->y_u.plxys.p));
      CHECK(save_PtoOp(stream, pc->y_u.plxys.f));
      CHECK(save_X(stream, pc->y_u.plxys.x));
      CHECK(save_Y(stream, pc->y_u.plxys.y));
      CHECK(save_Constant(stream, pc->y_u.plxys.flags));
      pc = NEXTOP(pc,plxys);
      break;
      /* instructions type plyys */
    case _call_bfunc_yy:
      CHECK(save_PtoPred(stream, pc->y_u.plyys.p));
      CHECK(save_PtoOp(stream, pc->y_u.plyys.f));
      CHECK(save_Y(stream, pc->y_u.plyys.y1));
      CHECK(save_Y(stream, pc->y_u.plyys.y2));
      CHECK(save_Constant(stream, pc->y_u.plyys.flags));
      pc = NEXTOP(pc,plyys);
      break;
      /* instructions type s */
    case _cut:
    case _cut_e:
    case _cut_t:
    case _pop_n:
    case _write_n_voids:
      CHECK(save_Constant(stream, pc->y_u.s.s));
      pc = NEXTOP(pc,s);
      break;
      /* instructions type sc */
    case _write_n_atoms:
      CHECK(save_Constant(stream, pc->y_u.sc.s));
      CHECK(save_ConstantTerm(stream, pc->y_u.sc.c));
      pc = NEXTOP(pc,sc);
      break;
      /* instructions type sllll */
    case _switch_on_sub_arg_type:
      CHECK(save_Constant(stream, pc->y_u.sllll.s));
      CHECK(save_PtoOp(stream, pc->y_u.sllll.l1));
      CHECK(save_PtoOp(stream, pc->y_u.sllll.l2));
      CHECK(save_PtoOp(stream, pc->y_u.sllll.l3));
      CHECK(save_PtoOp(stream, pc->y_u.sllll.l4));
      pc = NEXTOP(pc,sllll);
      break;
      /* instructions type slpp */
    case _call_c_wfail:
      CHECK(save_Constant(stream, pc->y_u.slpp.s));
      CHECK(save_PtoOp(stream, pc->y_u.slpp.l));
      CHECK(save_PtoPred(stream, pc->y_u.slpp.p));
      CHECK(save_PtoPred(stream, pc->y_u.slpp.p0));
      pc = NEXTOP(pc,slpp);
      break;
      /* instructions type sssl */
    case _go_on_cons:
    case _go_on_func:
    case _if_cons:
    case _if_func:
    case _switch_on_cons:
    case _switch_on_func:
      CHECK(save_Constant(stream, pc->y_u.sssl.s));
      CHECK(save_Constant(stream, pc->y_u.sssl.e));
      CHECK(save_Constant(stream, pc->y_u.sssl.w));
      CHECK(save_PtoOp(stream, pc->y_u.sssl.l));
      pc = NEXTOP(pc,sssl);
      break;
      /* instructions type sssllp */
    case _expand_clauses:
      CHECK(save_Constant(stream, pc->y_u.sssllp.s1));
      CHECK(save_Constant(stream, pc->y_u.sssllp.s2));
      CHECK(save_Constant(stream, pc->y_u.sssllp.s3));
      CHECK(save_PtoOp(stream, pc->y_u.sssllp.sprev));
      CHECK(save_PtoOp(stream, pc->y_u.sssllp.snext));
      CHECK(save_PtoPred(stream, pc->y_u.sssllp.p));
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
      CHECK(save_X(stream, pc->y_u.x.x));
      pc = NEXTOP(pc,x);
      break;
      /* instructions type xD */
    case _get_dbterm:
    case _put_dbterm:
      CHECK(save_X(stream, pc->y_u.xD.x));
      CHECK(save_DBGroundTerm(stream, pc->y_u.xD.D));
      pc = NEXTOP(pc,xD);
      break;
      /* instructions type xN */
    case _get_bigint:
    case _put_bigint:
      CHECK(save_X(stream, pc->y_u.xN.x));
      CHECK(save_BlobTermInCode(stream, pc->y_u.xN.b));
      pc = NEXTOP(pc,xN);
      break;
      /* instructions type xc */
    case _get_atom:
    case _put_atom:
      CHECK(save_X(stream, pc->y_u.xc.x));
      CHECK(save_ConstantTerm(stream, pc->y_u.xc.c));
      pc = NEXTOP(pc,xc);
      break;
      /* instructions type xd */
    case _get_float:
    case _put_float:
      CHECK(save_X(stream, pc->y_u.xd.x));
      CHECK(save_DoubleInCode(stream, pc->y_u.xd.d));
      pc = NEXTOP(pc,xd);
      break;
      /* instructions type xfa */
    case _get_struct:
    case _put_struct:
      CHECK(save_X(stream, pc->y_u.xfa.x));
      CHECK(save_Func(stream, pc->y_u.xfa.f));
      CHECK(save_Arity(stream, pc->y_u.xfa.a));
      pc = NEXTOP(pc,xfa);
      break;
      /* instructions type xi */
    case _get_longint:
    case _put_longint:
      CHECK(save_X(stream, pc->y_u.xi.x));
      CHECK(save_IntegerInCode(stream, pc->y_u.xi.i));
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
      CHECK(save_X(stream, pc->y_u.xl.x));
      CHECK(save_PtoOp(stream, pc->y_u.xl.F));
      pc = NEXTOP(pc,xl);
      break;
      /* instructions type xll */
    case _jump_if_nonvar:
      CHECK(save_X(stream, pc->y_u.xll.x));
      CHECK(save_PtoOp(stream, pc->y_u.xll.l1));
      CHECK(save_PtoOp(stream, pc->y_u.xll.l2));
      pc = NEXTOP(pc,xll);
      break;
      /* instructions type xllll */
    case _switch_on_arg_type:
      CHECK(save_X(stream, pc->y_u.xllll.x));
      CHECK(save_PtoOp(stream, pc->y_u.xllll.l1));
      CHECK(save_PtoOp(stream, pc->y_u.xllll.l2));
      CHECK(save_PtoOp(stream, pc->y_u.xllll.l3));
      CHECK(save_PtoOp(stream, pc->y_u.xllll.l4));
      pc = NEXTOP(pc,xllll);
      break;
      /* instructions type xps */
    case _commit_b_x:
      CHECK(save_X(stream, pc->y_u.xps.x));
      CHECK(save_PtoPred(stream, pc->y_u.xps.p0));
      CHECK(save_Constant(stream, pc->y_u.xps.s));
      pc = NEXTOP(pc,xps);
      break;
      /* instructions type xu */
    case _get_string:
      CHECK(save_X(stream, pc->y_u.xu.x));
      CHECK(save_BlobTermInCode(stream, pc->y_u.xu.ut));
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
      CHECK(save_X(stream, pc->y_u.xx.xl));
      CHECK(save_X(stream, pc->y_u.xx.xr));
      pc = NEXTOP(pc,xx);
      break;
      /* instructions type xxc */
    case _p_func2s_cv:
      CHECK(save_X(stream, pc->y_u.xxc.x));
      CHECK(save_X(stream, pc->y_u.xxc.xi));
      CHECK(save_ConstantTerm(stream, pc->y_u.xxc.c));
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
      CHECK(save_X(stream, pc->y_u.xxn.x));
      CHECK(save_X(stream, pc->y_u.xxn.xi));
      CHECK(save_Integer(stream, pc->y_u.xxn.c));
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
      CHECK(save_X(stream, pc->y_u.xxx.x));
      CHECK(save_X(stream, pc->y_u.xxx.x1));
      CHECK(save_X(stream, pc->y_u.xxx.x2));
      pc = NEXTOP(pc,xxx);
      break;
      /* instructions type xxxx */
    case _put_xx_val:
      CHECK(save_X(stream, pc->y_u.xxxx.xl1));
      CHECK(save_X(stream, pc->y_u.xxxx.xl2));
      CHECK(save_X(stream, pc->y_u.xxxx.xr1));
      CHECK(save_X(stream, pc->y_u.xxxx.xr2));
      pc = NEXTOP(pc,xxxx);
      break;
      /* instructions type xxy */
    case _p_func2f_xy:
      CHECK(save_X(stream, pc->y_u.xxy.x));
      CHECK(save_X(stream, pc->y_u.xxy.x1));
      CHECK(save_Y(stream, pc->y_u.xxy.y2));
      pc = NEXTOP(pc,xxy);
      break;
      /* instructions type y */
    case _save_b_y:
    case _write_y_loc:
    case _write_y_val:
    case _write_y_var:
      CHECK(save_Y(stream, pc->y_u.y.y));
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
      CHECK(save_Y(stream, pc->y_u.yl.y));
      CHECK(save_PtoOp(stream, pc->y_u.yl.F));
      pc = NEXTOP(pc,yl);
      break;
      /* instructions type yps */
    case _commit_b_y:
      CHECK(save_Y(stream, pc->y_u.yps.y));
      CHECK(save_PtoPred(stream, pc->y_u.yps.p0));
      CHECK(save_Constant(stream, pc->y_u.yps.s));
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
      CHECK(save_Y(stream, pc->y_u.yx.y));
      CHECK(save_X(stream, pc->y_u.yx.x));
      pc = NEXTOP(pc,yx);
      break;
      /* instructions type yxc */
    case _p_func2s_y_cv:
      CHECK(save_Y(stream, pc->y_u.yxc.y));
      CHECK(save_X(stream, pc->y_u.yxc.xi));
      CHECK(save_ConstantTerm(stream, pc->y_u.yxc.c));
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
      CHECK(save_Y(stream, pc->y_u.yxn.y));
      CHECK(save_X(stream, pc->y_u.yxn.xi));
      CHECK(save_Integer(stream, pc->y_u.yxn.c));
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
      CHECK(save_Y(stream, pc->y_u.yxx.y));
      CHECK(save_X(stream, pc->y_u.yxx.x1));
      CHECK(save_X(stream, pc->y_u.yxx.x2));
      pc = NEXTOP(pc,yxx);
      break;
      /* instructions type yyx */
    case _p_func2f_yy:
      CHECK(save_Y(stream, pc->y_u.yyx.y1));
      CHECK(save_Y(stream, pc->y_u.yyx.y2));
      CHECK(save_X(stream, pc->y_u.yyx.x));
      pc = NEXTOP(pc,yyx);
      break;
      /* instructions type yyxx */
    case _get_yy_var:
    case _put_y_vals:
      CHECK(save_Y(stream, pc->y_u.yyxx.y1));
      CHECK(save_Y(stream, pc->y_u.yyxx.y2));
      CHECK(save_X(stream, pc->y_u.yyxx.x1));
      CHECK(save_X(stream, pc->y_u.yyxx.x2));
      pc = NEXTOP(pc,yyxx);
      break;
#ifdef YAPOR
      /* instructions type Otapl */
    case _getwork:
    case _getwork_seq:
    case _sync:
#ifdef YAPOR
      CHECK(save_OrArg(stream, pc->y_u.Otapl.or_arg));
#endif
#ifdef TABLING
      CHECK(save_TabEntry(stream, pc->y_u.Otapl.te));
#endif
      CHECK(save_Arity(stream, pc->y_u.Otapl.s));
      CHECK(save_PtoPred(stream, pc->y_u.Otapl.p));
      CHECK(save_PtoOp(stream, pc->y_u.Otapl.d));
      pc = NEXTOP(pc,Otapl);
      break;
      /* instructions type e */
    case _getwork_first_time:
      if (op == _Nstop || op == _copy_idb_term || op == _unify_idb_term) return 1;
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
#ifdef YAPOR
      CHECK(save_OrArg(stream, pc->y_u.Otapl.or_arg));
#endif
#ifdef TABLING
      CHECK(save_TabEntry(stream, pc->y_u.Otapl.te));
#endif
      CHECK(save_Arity(stream, pc->y_u.Otapl.s));
      CHECK(save_PtoPred(stream, pc->y_u.Otapl.p));
      CHECK(save_PtoOp(stream, pc->y_u.Otapl.d));
      pc = NEXTOP(pc,Otapl);
      break;
      /* instructions type e */
#ifdef TABLING_INNER_CUTS
    case _clause_with_cut:
#endif
      if (op == _Nstop || op == _copy_idb_term || op == _unify_idb_term) return 1;
      pc = NEXTOP(pc,e);
      break;
      /* instructions type s */
    case _table_new_answer:
      CHECK(save_Constant(stream, pc->y_u.s.s));
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
      if (op == _Nstop || op == _copy_idb_term || op == _unify_idb_term) return 1;
      pc = NEXTOP(pc,e);
      break;
#endif
default:
	return -1;
     }
  }
