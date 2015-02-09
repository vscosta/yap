
  /* This file was generated automatically by "yap -L misc/buildops"
     please do not update */


  while (TRUE) {
    op_numbers op = Yap_op_from_opcode(cl->opc);
    switch (op) {
    case _write_dbterm:
      cl = NEXTOP(cl,D);
      break;
#ifdef YAP_JIT
    case _jit_handler:
#endif
      cl = NEXTOP(cl,J);
      break;
    case _alloc_for_logical_pred:
      cl = NEXTOP(cl,L);
      break;
    case _write_bigint:
      cl = NEXTOP(cl,N);
      break;
    case _ensure_space:
      cl = NEXTOP(cl,Osbpa);
      break;
    case _write_atom:
      cl = NEXTOP(cl,c);
      break;
    case _get_2atoms:
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(1))) {
	if (IsApplTerm(cl->y_u.cc.c1)) {
          CELL *pt = RepAppl(cl->y_u.cc.c1);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.cc.c1;
	} else
	  clause->Tag = cl->y_u.cc.c1;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(2))) {
	if (IsApplTerm(cl->y_u.cc.c2)) {
          CELL *pt = RepAppl(cl->y_u.cc.c2);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.cc.c2;
	} else
	  clause->Tag = cl->y_u.cc.c2;
	return;
      }
      cl = NEXTOP(cl,cc);
      break;
    case _get_3atoms:
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(1))) {
	if (IsApplTerm(cl->y_u.ccc.c1)) {
          CELL *pt = RepAppl(cl->y_u.ccc.c1);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.ccc.c1;
	} else
	  clause->Tag = cl->y_u.ccc.c1;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(2))) {
	if (IsApplTerm(cl->y_u.ccc.c2)) {
          CELL *pt = RepAppl(cl->y_u.ccc.c2);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.ccc.c2;
	} else
	  clause->Tag = cl->y_u.ccc.c2;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(3))) {
	if (IsApplTerm(cl->y_u.ccc.c3)) {
          CELL *pt = RepAppl(cl->y_u.ccc.c3);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.ccc.c3;
	} else
	  clause->Tag = cl->y_u.ccc.c3;
	return;
      }
      cl = NEXTOP(cl,ccc);
      break;
    case _get_4atoms:
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(1))) {
	if (IsApplTerm(cl->y_u.cccc.c1)) {
          CELL *pt = RepAppl(cl->y_u.cccc.c1);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.cccc.c1;
	} else
	  clause->Tag = cl->y_u.cccc.c1;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(2))) {
	if (IsApplTerm(cl->y_u.cccc.c2)) {
          CELL *pt = RepAppl(cl->y_u.cccc.c2);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.cccc.c2;
	} else
	  clause->Tag = cl->y_u.cccc.c2;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(3))) {
	if (IsApplTerm(cl->y_u.cccc.c3)) {
          CELL *pt = RepAppl(cl->y_u.cccc.c3);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.cccc.c3;
	} else
	  clause->Tag = cl->y_u.cccc.c3;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(4))) {
	if (IsApplTerm(cl->y_u.cccc.c4)) {
          CELL *pt = RepAppl(cl->y_u.cccc.c4);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.cccc.c4;
	} else
	  clause->Tag = cl->y_u.cccc.c4;
	return;
      }
      cl = NEXTOP(cl,cccc);
      break;
    case _get_5atoms:
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(1))) {
	if (IsApplTerm(cl->y_u.ccccc.c1)) {
          CELL *pt = RepAppl(cl->y_u.ccccc.c1);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.ccccc.c1;
	} else
	  clause->Tag = cl->y_u.ccccc.c1;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(2))) {
	if (IsApplTerm(cl->y_u.ccccc.c2)) {
          CELL *pt = RepAppl(cl->y_u.ccccc.c2);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.ccccc.c2;
	} else
	  clause->Tag = cl->y_u.ccccc.c2;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(3))) {
	if (IsApplTerm(cl->y_u.ccccc.c3)) {
          CELL *pt = RepAppl(cl->y_u.ccccc.c3);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.ccccc.c3;
	} else
	  clause->Tag = cl->y_u.ccccc.c3;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(4))) {
	if (IsApplTerm(cl->y_u.ccccc.c4)) {
          CELL *pt = RepAppl(cl->y_u.ccccc.c4);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.ccccc.c4;
	} else
	  clause->Tag = cl->y_u.ccccc.c4;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(5))) {
	if (IsApplTerm(cl->y_u.ccccc.c5)) {
          CELL *pt = RepAppl(cl->y_u.ccccc.c5);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.ccccc.c5;
	} else
	  clause->Tag = cl->y_u.ccccc.c5;
	return;
      }
      cl = NEXTOP(cl,ccccc);
      break;
    case _get_6atoms:
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(1))) {
	if (IsApplTerm(cl->y_u.cccccc.c1)) {
          CELL *pt = RepAppl(cl->y_u.cccccc.c1);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.cccccc.c1;
	} else
	  clause->Tag = cl->y_u.cccccc.c1;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(2))) {
	if (IsApplTerm(cl->y_u.cccccc.c2)) {
          CELL *pt = RepAppl(cl->y_u.cccccc.c2);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.cccccc.c2;
	} else
	  clause->Tag = cl->y_u.cccccc.c2;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(3))) {
	if (IsApplTerm(cl->y_u.cccccc.c3)) {
          CELL *pt = RepAppl(cl->y_u.cccccc.c3);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.cccccc.c3;
	} else
	  clause->Tag = cl->y_u.cccccc.c3;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(4))) {
	if (IsApplTerm(cl->y_u.cccccc.c4)) {
          CELL *pt = RepAppl(cl->y_u.cccccc.c4);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.cccccc.c4;
	} else
	  clause->Tag = cl->y_u.cccccc.c4;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(5))) {
	if (IsApplTerm(cl->y_u.cccccc.c5)) {
          CELL *pt = RepAppl(cl->y_u.cccccc.c5);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.cccccc.c5;
	} else
	  clause->Tag = cl->y_u.cccccc.c5;
	return;
      }
      if (is_regcopy(myregs, nofregs, Yap_regnotoreg(6))) {
	if (IsApplTerm(cl->y_u.cccccc.c6)) {
          CELL *pt = RepAppl(cl->y_u.cccccc.c6);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.cccccc.c6;
	} else
	  clause->Tag = cl->y_u.cccccc.c6;
	return;
      }
      cl = NEXTOP(cl,cccccc);
      break;
    case _write_float:
      cl = NEXTOP(cl,d);
      break;
    case _allocate:
      cl = NEXTOP(cl,e);
      break;
    case _copy_idb_term:
      if (regno == 2) {
	LogUpdClause *lcl = ClauseCodeToLogUpdClause(cl);
	Term t = lcl->lusl.ClSource->Entry;
        if (!(lcl->ClFlags & FactMask)) {
	  if (IsVarTerm(t)) {
	    clause->Tag = (CELL)NULL;
	  } else if (IsApplTerm(t)) {
	    CELL *pt = RepAppl(t);

	    clause->Tag = AbsAppl((CELL *)pt[0]);
	    clause->ucd.c_sreg = pt;
	  } else if (IsPairTerm(t)) {
	    CELL *pt = RepPair(t);

	    clause->Tag = AbsPair(NULL);
	    clause->ucd.c_sreg = pt-1;
	  } else {
	    clause->Tag = t;
	  }
        }
      } else {
	clause->Tag = (CELL)NULL;
      }
      return;
      cl = NEXTOP(cl,e);
      break;
    case _pop:
      cl = NEXTOP(cl,e);
      break;
#ifdef BEAM
    case _retry_eam:
#endif
      cl = NEXTOP(cl,e);
      break;
    case _unify_idb_term:
      if (regno == 2) {
	LogUpdClause *lcl = ClauseCodeToLogUpdClause(cl);
	Term t = lcl->lusl.ClSource->Entry;
        if (!(lcl->ClFlags & FactMask)) {
	  if (IsVarTerm(t)) {
	    clause->Tag = (CELL)NULL;
	  } else if (IsApplTerm(t)) {
	    CELL *pt = RepAppl(t);

	    clause->Tag = AbsAppl((CELL *)pt[0]);
	    clause->ucd.c_sreg = pt;
	  } else if (IsPairTerm(t)) {
	    CELL *pt = RepPair(t);

	    clause->Tag = AbsPair(NULL);
	    clause->ucd.c_sreg = pt-1;
	  } else {
	    clause->Tag = t;
	  }
        }
      } else {
	clause->Tag = (CELL)NULL;
      }
      return;
      cl = NEXTOP(cl,e);
      break;
    case _unlock_lu:
      cl = NEXTOP(cl,e);
      break;
    case _write_l_list:
      cl = NEXTOP(cl,e);
      break;
    case _write_list:
      cl = NEXTOP(cl,e);
      break;
    case _write_void:
      cl = NEXTOP(cl,e);
      break;
    case _write_l_struc:
      cl = NEXTOP(cl,fa);
      break;
    case _write_longint:
      cl = NEXTOP(cl,i);
      break;
    case _unify_l_list:
      cl = NEXTOP(cl,o);
      break;
    case _unify_l_list_write:
      cl = NEXTOP(cl,o);
      break;
    case _unify_l_void:
      cl = NEXTOP(cl,o);
      break;
    case _unify_l_void_write:
      cl = NEXTOP(cl,o);
      break;
    case _unify_list:
      cl = NEXTOP(cl,o);
      break;
    case _unify_list_write:
      cl = NEXTOP(cl,o);
      break;
    case _unify_void:
      cl = NEXTOP(cl,o);
      break;
    case _unify_void_write:
      cl = NEXTOP(cl,o);
      break;
    case _unify_dbterm:
      cl = NEXTOP(cl,oD);
      break;
    case _unify_l_dbterm:
      cl = NEXTOP(cl,oD);
      break;
    case _unify_bigint:
      cl = NEXTOP(cl,oN);
      break;
    case _unify_l_bigint:
      cl = NEXTOP(cl,oN);
      break;
    case _unify_atom:
      cl = NEXTOP(cl,oc);
      break;
    case _unify_atom_write:
      cl = NEXTOP(cl,oc);
      break;
    case _unify_l_atom:
      cl = NEXTOP(cl,oc);
      break;
    case _unify_l_atom_write:
      cl = NEXTOP(cl,oc);
      break;
    case _unify_float:
      cl = NEXTOP(cl,od);
      break;
    case _unify_float_write:
      cl = NEXTOP(cl,od);
      break;
    case _unify_l_float:
      cl = NEXTOP(cl,od);
      break;
    case _unify_l_float_write:
      cl = NEXTOP(cl,od);
      break;
    case _unify_l_struc:
      cl = NEXTOP(cl,ofa);
      break;
    case _unify_l_struc_write:
      cl = NEXTOP(cl,ofa);
      break;
    case _unify_struct:
      cl = NEXTOP(cl,ofa);
      break;
    case _unify_struct_write:
      cl = NEXTOP(cl,ofa);
      break;
    case _unify_l_longint:
      cl = NEXTOP(cl,oi);
      break;
    case _unify_l_longint_write:
      cl = NEXTOP(cl,oi);
      break;
    case _unify_longint:
      cl = NEXTOP(cl,oi);
      break;
    case _unify_longint_write:
      cl = NEXTOP(cl,oi);
      break;
#ifdef BEAM
    case _run_eam:
#endif
      cl = NEXTOP(cl,os);
      break;
    case _unify_l_n_voids:
      cl = NEXTOP(cl,os);
      break;
    case _unify_l_n_voids_write:
      cl = NEXTOP(cl,os);
      break;
    case _unify_n_voids:
      cl = NEXTOP(cl,os);
      break;
    case _unify_n_voids_write:
      cl = NEXTOP(cl,os);
      break;
    case _unify_n_atoms:
      cl = NEXTOP(cl,osc);
      break;
    case _unify_n_atoms_write:
      cl = NEXTOP(cl,osc);
      break;
    case _unify_l_string:
      cl = NEXTOP(cl,ou);
      break;
    case _unify_string:
      cl = NEXTOP(cl,ou);
      break;
    case _save_appl_x:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.ox.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _save_appl_x_write:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.ox.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _save_pair_x:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.ox.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _save_pair_x_write:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.ox.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _unify_l_x_loc:
      cl = NEXTOP(cl,ox);
      break;
    case _unify_l_x_loc_write:
      cl = NEXTOP(cl,ox);
      break;
    case _unify_l_x_val:
      cl = NEXTOP(cl,ox);
      break;
    case _unify_l_x_val_write:
      cl = NEXTOP(cl,ox);
      break;
    case _unify_l_x_var:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.ox.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _unify_l_x_var_write:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.ox.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _unify_x_loc:
      cl = NEXTOP(cl,ox);
      break;
    case _unify_x_loc_write:
      cl = NEXTOP(cl,ox);
      break;
    case _unify_x_val:
      cl = NEXTOP(cl,ox);
      break;
    case _unify_x_val_write:
      cl = NEXTOP(cl,ox);
      break;
    case _unify_x_var:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.ox.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _unify_x_var_write:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.ox.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _unify_l_x_var2:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oxx.xl))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oxx.xr))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oxx);
      break;
    case _unify_l_x_var2_write:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oxx.xl))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oxx.xr))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oxx);
      break;
    case _unify_x_var2:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oxx.xl))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oxx.xr))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oxx);
      break;
    case _unify_x_var2_write:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oxx.xl))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oxx.xr))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oxx);
      break;
    case _save_appl_y:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oy.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oy);
      break;
    case _save_appl_y_write:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oy.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oy);
      break;
    case _save_pair_y:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oy.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oy);
      break;
    case _save_pair_y_write:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oy.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oy);
      break;
    case _unify_l_y_loc:
      cl = NEXTOP(cl,oy);
      break;
    case _unify_l_y_loc_write:
      cl = NEXTOP(cl,oy);
      break;
    case _unify_l_y_val:
      cl = NEXTOP(cl,oy);
      break;
    case _unify_l_y_val_write:
      cl = NEXTOP(cl,oy);
      break;
    case _unify_l_y_var:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oy.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oy);
      break;
    case _unify_l_y_var_write:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oy.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oy);
      break;
    case _unify_y_loc:
      cl = NEXTOP(cl,oy);
      break;
    case _unify_y_loc_write:
      cl = NEXTOP(cl,oy);
      break;
    case _unify_y_val:
      cl = NEXTOP(cl,oy);
      break;
    case _unify_y_val_write:
      cl = NEXTOP(cl,oy);
      break;
    case _unify_y_var:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oy.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oy);
      break;
    case _unify_y_var_write:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.oy.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oy);
      break;
    case _deallocate:
      cl = NEXTOP(cl,p);
      break;
    case _lock_lu:
      cl = NEXTOP(cl,p);
      break;
    case _call_bfunc_xx:
      cl = NEXTOP(cl,plxxs);
      break;
    case _call_bfunc_xy:
      cl = NEXTOP(cl,plxys);
      break;
    case _call_bfunc_yx:
      cl = NEXTOP(cl,plxys);
      break;
    case _call_bfunc_yy:
      cl = NEXTOP(cl,plyys);
      break;
    case _pop_n:
      cl = NEXTOP(cl,s);
      break;
    case _write_n_voids:
      cl = NEXTOP(cl,s);
      break;
    case _write_n_atoms:
      cl = NEXTOP(cl,sc);
      break;
    case _get_list:
      if (is_regcopy(myregs, nofregs, cl->y_u.x.x)) {
	clause->Tag = AbsPair(NULL);
	clause->ucd.WorkPC = NEXTOP(cl,x);
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _put_list:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.x.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _save_b_x:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.x.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _write_x_loc:
      cl = NEXTOP(cl,x);
      break;
    case _write_x_var:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.x.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _put_dbterm:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xD.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xD);
      break;
    case _get_bigint:
      if (is_regcopy(myregs, nofregs, cl->y_u.xN.x)) {
	clause->Tag = AbsAppl((CELL *)FunctorBigInt);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xN);
      break;
    case _put_bigint:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xN.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xN);
      break;
    case _get_atom:
      if (is_regcopy(myregs, nofregs, cl->y_u.xc.x)) {
	if (IsApplTerm(cl->y_u.xc.c)) {
          CELL *pt = RepAppl(cl->y_u.xc.c);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->ucd.t_ptr = cl->y_u.xc.c;
	} else
	  clause->Tag = cl->y_u.xc.c;
	return;
      }
      cl = NEXTOP(cl,xc);
      break;
    case _put_atom:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xc.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xc);
      break;
    case _get_float:
      if (is_regcopy(myregs, nofregs, cl->y_u.xd.x)) {
	clause->Tag = AbsAppl((CELL *)FunctorDouble);
	clause->ucd.t_ptr = AbsAppl(cl->y_u.xd.d);
	return;
      }
      cl = NEXTOP(cl,xd);
      break;
    case _put_float:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xd.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xd);
      break;
    case _get_struct:
      if (is_regcopy(myregs, nofregs, cl->y_u.xfa.x)) {
	clause->Tag = AbsAppl((CELL *)cl->y_u.xfa.f);
	clause->ucd.WorkPC = NEXTOP(cl,xfa);
	return;
      }
      cl = NEXTOP(cl,xfa);
      break;
    case _put_struct:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xfa.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xfa);
      break;
    case _get_longint:
      if (is_regcopy(myregs, nofregs, cl->y_u.xi.x)) {
	clause->Tag = AbsAppl((CELL *)FunctorLongInt);
	clause->ucd.t_ptr = AbsAppl(cl->y_u.xi.i);
	return;
      }
      cl = NEXTOP(cl,xi);
      break;
    case _put_longint:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xi.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xi);
      break;
    case _p_atom_x:
      if (cl->y_u.xl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.xl.x)) {
	clause->Tag = (_atom+1)*sizeof(CELL);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xl);
      break;
    case _p_atomic_x:
      if (cl->y_u.xl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.xl.x)) {
	clause->Tag = (_atomic+1)*sizeof(CELL);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xl);
      break;
    case _p_compound_x:
      if (cl->y_u.xl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.xl.x)) {
	clause->Tag = (_compound+1)*sizeof(CELL);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xl);
      break;
    case _p_db_ref_x:
      if (cl->y_u.xl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.xl.x)) {
	clause->Tag = AbsAppl((CELL *)FunctorDBRef);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xl);
      break;
    case _p_float_x:
      if (cl->y_u.xl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.xl.x)) {
	clause->Tag = AbsAppl((CELL *)FunctorDouble);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xl);
      break;
    case _p_integer_x:
      if (cl->y_u.xl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.xl.x)) {
	clause->Tag = (_integer+1)*sizeof(CELL);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xl);
      break;
    case _p_nonvar_x:
      if (cl->y_u.xl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xl);
      break;
    case _p_number_x:
      if (cl->y_u.xl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.xl.x)) {
	clause->Tag = (_number+1)*sizeof(CELL);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xl);
      break;
    case _p_primitive_x:
      if (cl->y_u.xl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.xl.x)) {
	clause->Tag = (_primitive+1)*sizeof(CELL);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xl);
      break;
    case _p_var_x:
      if (cl->y_u.xl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.xl.x)) {
	clause->Tag = (_var+1)*sizeof(CELL);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xl);
      break;
    case _get_string:
      if (is_regcopy(myregs, nofregs, cl->y_u.xu.x)) {
	clause->Tag = AbsAppl((CELL *)FunctorString);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xu);
      break;
    case _get_x_val:
      if (!(nofregs = link_regcopies(myregs, nofregs, cl->y_u.xx.xl, cl->y_u.xx.xr))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _get_x_var:
      if (!(nofregs = add_regcopy(myregs, nofregs, cl->y_u.xx.xr, cl->y_u.xx.xl))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _gl_void_valx:
      if (is_regcopy(myregs, nofregs, cl->y_u.xx.xl)) {
	clause->Tag = AbsPair(NULL);
	clause->ucd.WorkPC = cl;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _gl_void_varx:
      if (is_regcopy(myregs, nofregs, cl->y_u.xx.xl)) {
	clause->Tag = AbsPair(NULL);
	clause->ucd.WorkPC = cl;
	return;
      }
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xx.xr))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _glist_valx:
      if (is_regcopy(myregs, nofregs, cl->y_u.xx.xl)) {
	clause->Tag = AbsPair(NULL);
	clause->ucd.WorkPC = cl;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _put_x_val:
      if (!(nofregs = add_regcopy(myregs, nofregs, cl->y_u.xx.xl, cl->y_u.xx.xr))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _put_x_var:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xx.xl))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xx.xr))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _p_func2s_cv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxc.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxc);
      break;
    case _p_and_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxn.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxn);
      break;
    case _p_arg_cv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxn.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxn);
      break;
    case _p_div_cv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxn.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxn);
      break;
    case _p_div_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxn.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxn);
      break;
    case _p_func2s_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxn.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxn);
      break;
    case _p_or_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxn.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxn);
      break;
    case _p_plus_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxn.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxn);
      break;
    case _p_sll_cv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxn.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxn);
      break;
    case _p_sll_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxn.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxn);
      break;
    case _p_slr_cv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxn.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxn);
      break;
    case _p_slr_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxn.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxn);
      break;
    case _p_times_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxn.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxn);
      break;
    case _p_and_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxx);
      break;
    case _p_arg_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxx);
      break;
    case _p_div_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxx);
      break;
    case _p_func2f_xx:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxx);
      break;
    case _p_func2s_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxx);
      break;
    case _p_minus_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxx);
      break;
    case _p_or_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxx);
      break;
    case _p_plus_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxx);
      break;
    case _p_sll_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxx);
      break;
    case _p_slr_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxx);
      break;
    case _p_times_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxx);
      break;
    case _put_xx_val:
      if (!(nofregs = add_regcopy(myregs, nofregs, cl->y_u.xxxx.xl1, cl->y_u.xxxx.xr1))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (!(nofregs = add_regcopy(myregs, nofregs, cl->y_u.xxxx.xl2, cl->y_u.xxxx.xr2))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxxx);
      break;
    case _p_func2f_xy:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.xxy.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxy);
      break;
    case _save_b_y:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.y.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,y);
      break;
    case _write_y_loc:
      cl = NEXTOP(cl,y);
      break;
    case _write_y_val:
      cl = NEXTOP(cl,y);
      break;
    case _write_y_var:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.y.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,y);
      break;
    case _p_atom_y:
      if (cl->y_u.yl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.yl.y)) {
	clause->Tag = (_atom+1)*sizeof(CELL);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yl);
      break;
    case _p_atomic_y:
      if (cl->y_u.yl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.yl.y)) {
	clause->Tag = (_atomic+1)*sizeof(CELL);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yl);
      break;
    case _p_compound_y:
      if (cl->y_u.yl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.yl.y)) {
	clause->Tag = (_compound+1)*sizeof(CELL);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yl);
      break;
    case _p_db_ref_y:
      if (cl->y_u.yl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.yl.y)) {
	clause->Tag = AbsAppl((CELL *)FunctorDBRef);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yl);
      break;
    case _p_float_y:
      if (cl->y_u.yl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.yl.y)) {
	clause->Tag = AbsAppl((CELL *)FunctorDouble);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yl);
      break;
    case _p_integer_y:
      if (cl->y_u.yl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.yl.y)) {
	clause->Tag = (_integer+1)*sizeof(CELL);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yl);
      break;
    case _p_nonvar_y:
      if (cl->y_u.yl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yl);
      break;
    case _p_number_y:
      if (cl->y_u.yl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.yl.y)) {
	clause->Tag = (_number+1)*sizeof(CELL);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yl);
      break;
    case _p_primitive_y:
      if (cl->y_u.yl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.yl.y)) {
	clause->Tag = (_primitive+1)*sizeof(CELL);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yl);
      break;
    case _p_var_y:
      if (cl->y_u.yl.F != FAILCODE) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (is_regcopy(myregs, nofregs, cl->y_u.yl.y)) {
	clause->Tag = (_var+1)*sizeof(CELL);
	clause->ucd.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yl);
      break;
    case _get_y_val:
      if (!(nofregs = link_regcopies(myregs, nofregs, cl->y_u.yx.x, cl->y_u.yx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yx);
      break;
    case _get_y_var:
      if (!(nofregs = add_regcopy(myregs, nofregs, cl->y_u.yx.x, cl->y_u.yx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yx);
      break;
    case _gl_void_valy:
      if (is_regcopy(myregs, nofregs, cl->y_u.yx.y)) {
	clause->Tag = AbsPair(NULL);
	clause->ucd.WorkPC = cl;
	return;
      }
      cl = NEXTOP(cl,yx);
      break;
    case _gl_void_vary:
      if (is_regcopy(myregs, nofregs, cl->y_u.yx.y)) {
	clause->Tag = AbsPair(NULL);
	clause->ucd.WorkPC = cl;
	return;
      }
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yx);
      break;
    case _glist_valy:
      if (is_regcopy(myregs, nofregs, cl->y_u.yx.x)) {
	clause->Tag = AbsPair(NULL);
	clause->ucd.WorkPC = cl;
	return;
      }
      cl = NEXTOP(cl,yx);
      break;
    case _put_unsafe:
      if (!(nofregs = add_regcopy(myregs, nofregs, cl->y_u.yx.y, cl->y_u.yx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yx);
      break;
    case _put_y_val:
      if (!(nofregs = add_regcopy(myregs, nofregs, cl->y_u.yx.y, cl->y_u.yx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yx);
      break;
    case _put_y_var:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yx);
      break;
    case _p_func2s_y_cv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxc.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxc);
      break;
    case _p_and_y_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxn.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxn);
      break;
    case _p_arg_y_cv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxn.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxn);
      break;
    case _p_div_y_cv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxn.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxn);
      break;
    case _p_div_y_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxn.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxn);
      break;
    case _p_func2s_y_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxn.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxn);
      break;
    case _p_or_y_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxn.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxn);
      break;
    case _p_plus_y_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxn.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxn);
      break;
    case _p_sll_y_cv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxn.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxn);
      break;
    case _p_sll_y_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxn.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxn);
      break;
    case _p_slr_y_cv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxn.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxn);
      break;
    case _p_slr_y_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxn.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxn);
      break;
    case _p_times_y_vc:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxn.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxn);
      break;
    case _p_and_y_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxx);
      break;
    case _p_arg_y_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxx);
      break;
    case _p_div_y_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxx);
      break;
    case _p_func2f_yx:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxx);
      break;
    case _p_func2s_y_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxx);
      break;
    case _p_minus_y_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxx);
      break;
    case _p_or_y_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxx);
      break;
    case _p_plus_y_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxx);
      break;
    case _p_sll_y_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxx);
      break;
    case _p_slr_y_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxx);
      break;
    case _p_times_y_vv:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yxx.y))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yxx);
      break;
    case _p_func2f_yy:
      if (!(nofregs = delete_regcopy(myregs, nofregs, cl->y_u.yyx.x))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yyx);
      break;
    case _get_yy_var:
      if (!(nofregs = add_regcopy(myregs, nofregs, cl->y_u.yyxx.x1, cl->y_u.yyxx.y1))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (!(nofregs = add_regcopy(myregs, nofregs, cl->y_u.yyxx.x2, cl->y_u.yyxx.y2))) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yyxx);
      break;
#ifdef YAPOR
#endif
#ifdef TABLING
    case _table_try_single:
      cl = NEXTOP(cl,Otapl);
      break;
#endif
default:
	clause->Tag = (CELL)NULL;
	return;
     }
  }
