
  /* This file was generated automatically by "yap -L misc/buildops"
     please do not update */


  while (TRUE) {
    op_numbers op = Yap_op_from_opcode(cl->opc);
    switch (op) {
    case _ensure_space:
      cl = NEXTOP(cl,Osbpa);
      break;
    case _native_me:
      cl = NEXTOP(cl,aFlp);
      break;
    case _get_2atoms:
      if (iarg == Yap_regnotoreg(1)) {
	if (IsApplTerm(cl->u.cc.c1)) {
          CELL *pt = RepAppl(cl->u.cc.c1);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.cc.c1;
	} else
	  clause->Tag = cl->u.cc.c1;
	return;
      }
      if (iarg == Yap_regnotoreg(2)) {
	if (IsApplTerm(cl->u.cc.c2)) {
          CELL *pt = RepAppl(cl->u.cc.c2);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.cc.c2;
	} else
	  clause->Tag = cl->u.cc.c2;
	return;
      }
      cl = NEXTOP(cl,cc);
      break;
    case _get_3atoms:
      if (iarg == Yap_regnotoreg(1)) {
	if (IsApplTerm(cl->u.ccc.c1)) {
          CELL *pt = RepAppl(cl->u.ccc.c1);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.ccc.c1;
	} else
	  clause->Tag = cl->u.ccc.c1;
	return;
      }
      if (iarg == Yap_regnotoreg(2)) {
	if (IsApplTerm(cl->u.ccc.c2)) {
          CELL *pt = RepAppl(cl->u.ccc.c2);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.ccc.c2;
	} else
	  clause->Tag = cl->u.ccc.c2;
	return;
      }
      if (iarg == Yap_regnotoreg(3)) {
	if (IsApplTerm(cl->u.ccc.c3)) {
          CELL *pt = RepAppl(cl->u.ccc.c3);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.ccc.c3;
	} else
	  clause->Tag = cl->u.ccc.c3;
	return;
      }
      cl = NEXTOP(cl,ccc);
      break;
    case _get_4atoms:
      if (iarg == Yap_regnotoreg(1)) {
	if (IsApplTerm(cl->u.cccc.c1)) {
          CELL *pt = RepAppl(cl->u.cccc.c1);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.cccc.c1;
	} else
	  clause->Tag = cl->u.cccc.c1;
	return;
      }
      if (iarg == Yap_regnotoreg(2)) {
	if (IsApplTerm(cl->u.cccc.c2)) {
          CELL *pt = RepAppl(cl->u.cccc.c2);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.cccc.c2;
	} else
	  clause->Tag = cl->u.cccc.c2;
	return;
      }
      if (iarg == Yap_regnotoreg(3)) {
	if (IsApplTerm(cl->u.cccc.c3)) {
          CELL *pt = RepAppl(cl->u.cccc.c3);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.cccc.c3;
	} else
	  clause->Tag = cl->u.cccc.c3;
	return;
      }
      if (iarg == Yap_regnotoreg(4)) {
	if (IsApplTerm(cl->u.cccc.c4)) {
          CELL *pt = RepAppl(cl->u.cccc.c4);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.cccc.c4;
	} else
	  clause->Tag = cl->u.cccc.c4;
	return;
      }
      cl = NEXTOP(cl,cccc);
      break;
    case _get_5atoms:
      if (iarg == Yap_regnotoreg(1)) {
	if (IsApplTerm(cl->u.ccccc.c1)) {
          CELL *pt = RepAppl(cl->u.ccccc.c1);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.ccccc.c1;
	} else
	  clause->Tag = cl->u.ccccc.c1;
	return;
      }
      if (iarg == Yap_regnotoreg(2)) {
	if (IsApplTerm(cl->u.ccccc.c2)) {
          CELL *pt = RepAppl(cl->u.ccccc.c2);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.ccccc.c2;
	} else
	  clause->Tag = cl->u.ccccc.c2;
	return;
      }
      if (iarg == Yap_regnotoreg(3)) {
	if (IsApplTerm(cl->u.ccccc.c3)) {
          CELL *pt = RepAppl(cl->u.ccccc.c3);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.ccccc.c3;
	} else
	  clause->Tag = cl->u.ccccc.c3;
	return;
      }
      if (iarg == Yap_regnotoreg(4)) {
	if (IsApplTerm(cl->u.ccccc.c4)) {
          CELL *pt = RepAppl(cl->u.ccccc.c4);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.ccccc.c4;
	} else
	  clause->Tag = cl->u.ccccc.c4;
	return;
      }
      if (iarg == Yap_regnotoreg(5)) {
	if (IsApplTerm(cl->u.ccccc.c5)) {
          CELL *pt = RepAppl(cl->u.ccccc.c5);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.ccccc.c5;
	} else
	  clause->Tag = cl->u.ccccc.c5;
	return;
      }
      cl = NEXTOP(cl,ccccc);
      break;
    case _get_6atoms:
      if (iarg == Yap_regnotoreg(1)) {
	if (IsApplTerm(cl->u.cccccc.c1)) {
          CELL *pt = RepAppl(cl->u.cccccc.c1);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.cccccc.c1;
	} else
	  clause->Tag = cl->u.cccccc.c1;
	return;
      }
      if (iarg == Yap_regnotoreg(2)) {
	if (IsApplTerm(cl->u.cccccc.c2)) {
          CELL *pt = RepAppl(cl->u.cccccc.c2);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.cccccc.c2;
	} else
	  clause->Tag = cl->u.cccccc.c2;
	return;
      }
      if (iarg == Yap_regnotoreg(3)) {
	if (IsApplTerm(cl->u.cccccc.c3)) {
          CELL *pt = RepAppl(cl->u.cccccc.c3);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.cccccc.c3;
	} else
	  clause->Tag = cl->u.cccccc.c3;
	return;
      }
      if (iarg == Yap_regnotoreg(4)) {
	if (IsApplTerm(cl->u.cccccc.c4)) {
          CELL *pt = RepAppl(cl->u.cccccc.c4);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.cccccc.c4;
	} else
	  clause->Tag = cl->u.cccccc.c4;
	return;
      }
      if (iarg == Yap_regnotoreg(5)) {
	if (IsApplTerm(cl->u.cccccc.c5)) {
          CELL *pt = RepAppl(cl->u.cccccc.c5);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.cccccc.c5;
	} else
	  clause->Tag = cl->u.cccccc.c5;
	return;
      }
      if (iarg == Yap_regnotoreg(6)) {
	if (IsApplTerm(cl->u.cccccc.c6)) {
          CELL *pt = RepAppl(cl->u.cccccc.c6);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.cccccc.c6;
	} else
	  clause->Tag = cl->u.cccccc.c6;
	return;
      }
      cl = NEXTOP(cl,cccccc);
      break;
    case _copy_idb_term:
      if (regno != 2) {
	clause->Tag = (CELL)NULL;
      } else {
	LogUpdClause *lcl = ClauseCodeToLogUpdClause(cl);
	Term t = lcl->ClSource->Entry;
      
	if (IsVarTerm(t)) {
	  clause->Tag = (CELL)NULL;
	} else if (IsApplTerm(t)) {
	  CELL *pt = RepAppl(t);
	
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  if (IsExtensionFunctor(FunctorOfTerm(t))) {
	    clause->u.t_ptr = t;
	  } else {
	    clause->u.c_sreg = pt;
	  }
	} else if (IsPairTerm(t)) {
	  CELL *pt = RepPair(t);

	  clause->Tag = AbsPair(NULL);
	  clause->u.c_sreg = pt-1;
	} else {
	  clause->Tag = t;
	}
      }
      return;
      cl = NEXTOP(cl,e);
      break;
    case _pop:
      cl = NEXTOP(cl,e);
      break;
    case _unify_idb_term:
      if (regno != 2) {
	clause->Tag = (CELL)NULL;
      } else {
	LogUpdClause *lcl = ClauseCodeToLogUpdClause(cl);
	Term t = lcl->ClSource->Entry;
      
	if (IsVarTerm(t)) {
	  clause->Tag = (CELL)NULL;
	} else if (IsApplTerm(t)) {
	  CELL *pt = RepAppl(t);
	
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  if (IsExtensionFunctor(FunctorOfTerm(t))) {
	    clause->u.t_ptr = t;
	  } else {
	    clause->u.c_sreg = pt;
	  }
	} else if (IsPairTerm(t)) {
	  CELL *pt = RepPair(t);

	  clause->Tag = AbsPair(NULL);
	  clause->u.c_sreg = pt-1;
	} else {
	  clause->Tag = t;
	}
      }
      return;
      cl = NEXTOP(cl,e);
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
    case _save_appl_x:
      if (iarg == cl->u.ox.x) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _save_appl_x_write:
      if (iarg == cl->u.ox.x) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _save_pair_x:
      if (iarg == cl->u.ox.x) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _save_pair_x_write:
      if (iarg == cl->u.ox.x) {
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
      if (iarg == cl->u.ox.x) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _unify_l_x_var_write:
      if (iarg == cl->u.ox.x) {
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
      if (iarg == cl->u.ox.x) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _unify_x_var_write:
      if (iarg == cl->u.ox.x) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,ox);
      break;
    case _unify_l_x_var2:
      if (iarg == cl->u.oxx.xl) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (iarg == cl->u.oxx.xr) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oxx);
      break;
    case _unify_l_x_var2_write:
      if (iarg == cl->u.oxx.xl) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (iarg == cl->u.oxx.xr) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oxx);
      break;
    case _unify_x_var2:
      if (iarg == cl->u.oxx.xl) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (iarg == cl->u.oxx.xr) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oxx);
      break;
    case _unify_x_var2_write:
      if (iarg == cl->u.oxx.xl) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (iarg == cl->u.oxx.xr) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,oxx);
      break;
    case _save_appl_y:
      cl = NEXTOP(cl,oy);
      break;
    case _save_appl_y_write:
      cl = NEXTOP(cl,oy);
      break;
    case _save_pair_y:
      cl = NEXTOP(cl,oy);
      break;
    case _save_pair_y_write:
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
      cl = NEXTOP(cl,oy);
      break;
    case _unify_l_y_var_write:
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
      cl = NEXTOP(cl,oy);
      break;
    case _unify_y_var_write:
      cl = NEXTOP(cl,oy);
      break;
    case _pop_n:
      cl = NEXTOP(cl,s);
      break;
    case _get_list:
      if (iarg == cl->u.x.x) {
	clause->Tag = AbsPair(NULL);
	clause->u.WorkPC = NEXTOP(cl,x);
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _put_list:
      if (iarg == cl->u.x.x) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,x);
      break;
    case _put_dbterm:
      if (iarg == cl->u.xD.x) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xD);
      break;
    case _get_bigint:
      if (iarg == cl->u.xN.x) {
	clause->Tag = AbsAppl((CELL *)FunctorBigInt);
	clause->u.t_ptr = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xN);
      break;
    case _put_bigint:
      if (iarg == cl->u.xN.x) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xN);
      break;
    case _get_atom:
      if (iarg == cl->u.xc.x) {
	if (IsApplTerm(cl->u.xc.c)) {
          CELL *pt = RepAppl(cl->u.xc.c);
	  clause->Tag = AbsAppl((CELL *)pt[0]);
	  clause->u.t_ptr = cl->u.xc.c;
	} else
	  clause->Tag = cl->u.xc.c;
	return;
      }
      cl = NEXTOP(cl,xc);
      break;
    case _put_atom:
      if (iarg == cl->u.xc.x) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xc);
      break;
    case _get_float:
      if (iarg == cl->u.xd.x) {
	clause->Tag = AbsAppl((CELL *)FunctorDouble);
	clause->u.t_ptr = AbsAppl(cl->u.xd.d);
	return;
      }
      cl = NEXTOP(cl,xd);
      break;
    case _put_float:
      if (iarg == cl->u.xd.x) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xd);
      break;
    case _get_struct:
      if (iarg == cl->u.xfa.x) {
	clause->Tag = AbsAppl((CELL *)cl->u.xfa.f);
	clause->u.WorkPC = NEXTOP(cl,xfa);
	return;
      }
      cl = NEXTOP(cl,xfa);
      break;
    case _put_struct:
      if (iarg == cl->u.xfa.x) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xfa);
      break;
    case _get_longint:
      if (iarg == cl->u.xi.x) {
	clause->Tag = AbsAppl((CELL *)FunctorLongInt);
	clause->u.t_ptr = AbsAppl(cl->u.xi.i);
	return;
      }
      cl = NEXTOP(cl,xi);
      break;
    case _put_longint:
      if (iarg == cl->u.xi.x) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xi);
      break;
    case _get_x_val:
      if (cl->u.xx.xl == iarg ||
        cl->u.xx.xr == iarg) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _get_x_var:
      if (cl->u.xx.xr == iarg ||
        cl->u.xx.xl == iarg) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _gl_void_valx:
      if (iarg == cl->u.xx.xl) {
	clause->Tag = AbsPair(NULL);
	clause->u.WorkPC = cl;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _gl_void_varx:
      if (iarg == cl->u.xx.xl) {
	clause->Tag = AbsPair(NULL);
	clause->u.WorkPC = cl;
	return;
      }
      if (iarg == cl->u.xx.xr) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _glist_valx:
      if (iarg == cl->u.xx.xl) {
	clause->Tag = AbsPair(NULL);
	clause->u.WorkPC = cl;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _put_x_val:
      if (cl->u.xx.xl == iarg ||
        cl->u.xx.xr == iarg) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _put_x_var:
      if (iarg == cl->u.xx.xl) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (iarg == cl->u.xx.xr) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xx);
      break;
    case _put_xx_val:
      if (cl->u.xxxx.xl1 == iarg ||
        cl->u.xxxx.xr1 == iarg) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (cl->u.xxxx.xl2 == iarg ||
        cl->u.xxxx.xr2 == iarg) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,xxxx);
      break;
    case _get_y_val:
      if (cl->u.yx.x == iarg) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yx);
      break;
    case _get_y_var:
      if (cl->u.yx.x == iarg) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yx);
      break;
    case _gl_void_valy:
      cl = NEXTOP(cl,yx);
      break;
    case _gl_void_vary:
      cl = NEXTOP(cl,yx);
      break;
    case _glist_valy:
      if (iarg == cl->u.yx.x) {
	clause->Tag = AbsPair(NULL);
	clause->u.WorkPC = cl;
	return;
      }
      cl = NEXTOP(cl,yx);
      break;
    case _put_unsafe:
      if (cl->u.yx.x == iarg) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yx);
      break;
    case _put_y_val:
      if (cl->u.yx.x == iarg) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yx);
      break;
    case _put_y_var:
      if (iarg == cl->u.yx.x) {
	clause->Tag = (CELL)NULL;
	return;
      }
      cl = NEXTOP(cl,yx);
      break;
    case _get_yy_var:
      if (cl->u.yyxx.x1 == iarg) {
	clause->Tag = (CELL)NULL;
	return;
      }
      if (cl->u.yyxx.x2 == iarg) {
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
