
  /* This file was generated automatically by "yap -L misc/buildops"
     please do not update */


  while (TRUE) {
    op_numbers op;

    op = Yap_op_from_opcode(pc->opc);
    /* C-code, maybe indexing */
    switch (op) {
      /* instructions type D */
    case _write_dbterm:
      pc = NEXTOP(pc,D);
      break;
      /* instructions type Illss */
    case _enter_lu_pred:
      return walk_got_lu_block(pc->y_u.Illss.I, startp, endp);
      /* instructions type J */
#ifdef YAP_JIT
    case _jit_handler:
#endif
      pc = NEXTOP(pc,J);
      break;
      /* instructions type L */
    case _alloc_for_logical_pred:
      return walk_got_lu_clause(pc->y_u.L.ClBase, startp, endp);
      /* instructions type N */
    case _write_bigint:
      pc = NEXTOP(pc,N);
      break;
      /* instructions type Osblp */
    case _either:
    case _or_else:
      clause_code = TRUE;
      pp = pc->y_u.Osblp.p0;
      pc = NEXTOP(pc,Osblp);
      break;
      /* instructions type Osbmp */
    case _p_execute:
    case _p_execute_tail:
      pc = NEXTOP(pc,Osbmp);
      break;
      /* instructions type Osbpa */
    case _ensure_space:
      pc = NEXTOP(pc,Osbpa);
      break;
      /* instructions type Osbpp */
    case _call_cpred:
      pp = pc->y_u.Osbpp.p;
      return walk_found_c_pred(pp, startp, endp);
    case _call_usercpred:
      pp = pc->y_u.Osbpp.p;
      return walk_found_c_pred(pp, startp, endp);
    case _execute_cpred:
      pp = pc->y_u.Osbpp.p;
      return walk_found_c_pred(pp, startp, endp);
    case _p_execute2:
      return found_meta_call(startp, endp);
    case _call:
    case _dexecute:
    case _execute:
    case _fcall:
      clause_code = TRUE;
      pp = pc->y_u.Osbpp.p0;
      pc = NEXTOP(pc,Osbpp);
      break;
      /* instructions type OtILl */
    case _count_trust_logical:
    case _profiled_trust_logical:
    case _trust_logical:
      return walk_got_lu_block(pc->y_u.OtILl.block, startp, endp);
      /* instructions type OtaLl */
    case _count_retry_logical:
    case _profiled_retry_logical:
    case _retry_logical:
    case _try_logical:
      pc = pc->y_u.OtaLl.n;
      break;
      /* instructions type OtapFs */
    case _cut_c:
    case _cut_userc:
    case _retry_c:
    case _retry_userc:
    case _try_c:
    case _try_userc:
      clause_code = TRUE;
      pp = pc->y_u.OtapFs.p;
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
      clause_code = FALSE;
      pp = pc->y_u.Otapl.p;
      pc = NEXTOP(pc,Otapl);
      break;
      /* instructions type c */
    case _write_atom:
      pc = NEXTOP(pc,c);
      break;
      /* instructions type cc */
    case _get_2atoms:
      pc = NEXTOP(pc,cc);
      break;
      /* instructions type ccc */
    case _get_3atoms:
      pc = NEXTOP(pc,ccc);
      break;
      /* instructions type cccc */
    case _get_4atoms:
      pc = NEXTOP(pc,cccc);
      break;
      /* instructions type ccccc */
    case _get_5atoms:
      pc = NEXTOP(pc,ccccc);
      break;
      /* instructions type cccccc */
    case _get_6atoms:
      pc = NEXTOP(pc,cccccc);
      break;
      /* instructions type clll */
    case _if_not_then:
      pc = NEXTOP(pc,clll);
      break;
      /* instructions type d */
    case _write_float:
      pc = NEXTOP(pc,d);
      break;
      /* instructions type e */
    case _Nstop:
      return NULL;
    case _copy_idb_term:
      return found_idb_clause(pc, startp, endp);
    case _expand_index:
      return found_expand(pc, startp, endp PASS_REGS);
    case _index_pred:
      return found_owner_op(pc, startp, endp PASS_REGS);
    case _lock_pred:
      return found_owner_op(pc, startp, endp PASS_REGS);
    case _op_fail:
      if (codeptr == FAILCODE)
        return found_fail(pc, startp, endp PASS_REGS);
      pc = NEXTOP(pc,e);
      break;
    case _spy_pred:
      return found_owner_op(pc, startp, endp PASS_REGS);
    case _trust_fail:
      if (codeptr == TRUSTFAILCODE)
        return found_fail(pc, startp, endp PASS_REGS);
      pc = NEXTOP(pc,e);
      break;
    case _undef_p:
      return found_owner_op(pc, startp, endp PASS_REGS);
    case _unify_idb_term:
      return found_idb_clause(pc, startp, endp);
    case _allocate:
    case _enter_exo:
    case _index_blob:
    case _index_dbref:
    case _index_long:
    case _p_equal:
    case _p_functor:
    case _pop:
#ifdef BEAM
    case _retry_eam:
#endif
#ifdef THREADS
    case _thread_local:
#endif
    case _unlock_lu:
    case _write_l_list:
    case _write_list:
    case _write_void:
      pc = NEXTOP(pc,e);
      break;
      /* instructions type fa */
    case _write_l_struc:
    case _write_struct:
      pc = NEXTOP(pc,fa);
      break;
      /* instructions type i */
    case _write_longint:
      pc = NEXTOP(pc,i);
      break;
      /* instructions type l */
    case _Ystop:
      return found_ystop(pc, clause_code, startp, endp, pp PASS_REGS);
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
      pc = NEXTOP(pc,l);
      break;
      /* instructions type llll */
    case _switch_on_type:
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
      pc = NEXTOP(pc,o);
      break;
      /* instructions type oD */
    case _unify_dbterm:
    case _unify_l_dbterm:
      pc = NEXTOP(pc,oD);
      break;
      /* instructions type oN */
    case _unify_bigint:
    case _unify_l_bigint:
      pc = NEXTOP(pc,oN);
      break;
      /* instructions type oc */
    case _unify_atom:
    case _unify_atom_write:
    case _unify_l_atom:
    case _unify_l_atom_write:
      pc = NEXTOP(pc,oc);
      break;
      /* instructions type od */
    case _unify_float:
    case _unify_float_write:
    case _unify_l_float:
    case _unify_l_float_write:
      pc = NEXTOP(pc,od);
      break;
      /* instructions type ofa */
    case _unify_l_struc:
    case _unify_l_struc_write:
    case _unify_struct:
    case _unify_struct_write:
      pc = NEXTOP(pc,ofa);
      break;
      /* instructions type oi */
    case _unify_l_longint:
    case _unify_l_longint_write:
    case _unify_longint:
    case _unify_longint_write:
      pc = NEXTOP(pc,oi);
      break;
      /* instructions type ollll */
    case _switch_list_nl:
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
      pc = NEXTOP(pc,os);
      break;
      /* instructions type osc */
    case _unify_n_atoms:
    case _unify_n_atoms_write:
      pc = NEXTOP(pc,osc);
      break;
      /* instructions type ou */
    case _unify_l_string:
    case _unify_string:
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
      pc = NEXTOP(pc,ox);
      break;
      /* instructions type oxx */
    case _unify_l_x_var2:
    case _unify_l_x_var2_write:
    case _unify_x_var2:
    case _unify_x_var2_write:
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
      pc = NEXTOP(pc,oy);
      break;
      /* instructions type p */
    case _lock_lu:
    case _procceed:
      pp = pc->y_u.p.p;
      if (pp->PredFlags & MegaClausePredFlag)
        return found_mega_clause(pp, startp, endp);
      clause_code = TRUE;
      pc = NEXTOP(pc,p);
      break;
    case _count_call:
    case _count_retry:
    case _deallocate:
    case _enter_profiling:
    case _retry_profiled:
    case _retry_udi:
    case _try_udi:
      pc = NEXTOP(pc,p);
      break;
      /* instructions type plxxs */
    case _call_bfunc_xx:
      pc = NEXTOP(pc,plxxs);
      break;
      /* instructions type plxys */
    case _call_bfunc_xy:
    case _call_bfunc_yx:
      pc = NEXTOP(pc,plxys);
      break;
      /* instructions type plyys */
    case _call_bfunc_yy:
      pc = NEXTOP(pc,plyys);
      break;
      /* instructions type s */
    case _cut:
    case _cut_e:
    case _cut_t:
    case _pop_n:
    case _write_n_voids:
      pc = NEXTOP(pc,s);
      break;
      /* instructions type sc */
    case _write_n_atoms:
      pc = NEXTOP(pc,sc);
      break;
      /* instructions type sllll */
    case _switch_on_sub_arg_type:
      pc = NEXTOP(pc,sllll);
      break;
      /* instructions type slpp */
    case _call_c_wfail:
      pp = pc->y_u.slpp.p;
      return walk_found_c_pred(pp, startp, endp);
      /* instructions type sssl */
    case _go_on_cons:
    case _go_on_func:
    case _if_cons:
    case _if_func:
    case _switch_on_cons:
    case _switch_on_func:
      pc = NEXTOP(pc,sssl);
      break;
      /* instructions type sssllp */
    case _expand_clauses:
      return found_expand_index(pc, startp, endp, codeptr PASS_REGS);
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
      pc = NEXTOP(pc,x);
      break;
      /* instructions type xD */
    case _get_dbterm:
    case _put_dbterm:
      pc = NEXTOP(pc,xD);
      break;
      /* instructions type xN */
    case _get_bigint:
    case _put_bigint:
      pc = NEXTOP(pc,xN);
      break;
      /* instructions type xc */
    case _get_atom:
    case _put_atom:
      pc = NEXTOP(pc,xc);
      break;
      /* instructions type xd */
    case _get_float:
    case _put_float:
      pc = NEXTOP(pc,xd);
      break;
      /* instructions type xfa */
    case _get_struct:
    case _put_struct:
      pc = NEXTOP(pc,xfa);
      break;
      /* instructions type xi */
    case _get_longint:
    case _put_longint:
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
      pc = NEXTOP(pc,xl);
      break;
      /* instructions type xll */
    case _jump_if_nonvar:
      pc = NEXTOP(pc,xll);
      break;
      /* instructions type xllll */
    case _switch_on_arg_type:
      pc = NEXTOP(pc,xllll);
      break;
      /* instructions type xps */
    case _commit_b_x:
      pc = NEXTOP(pc,xps);
      break;
      /* instructions type xu */
    case _get_string:
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
      pc = NEXTOP(pc,xx);
      break;
      /* instructions type xxc */
    case _p_func2s_cv:
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
      pc = NEXTOP(pc,xxx);
      break;
      /* instructions type xxxx */
    case _put_xx_val:
      pc = NEXTOP(pc,xxxx);
      break;
      /* instructions type xxy */
    case _p_func2f_xy:
      pc = NEXTOP(pc,xxy);
      break;
      /* instructions type y */
    case _save_b_y:
    case _write_y_loc:
    case _write_y_val:
    case _write_y_var:
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
      pc = NEXTOP(pc,yl);
      break;
      /* instructions type yps */
    case _commit_b_y:
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
      pc = NEXTOP(pc,yx);
      break;
      /* instructions type yxc */
    case _p_func2s_y_cv:
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
      pc = NEXTOP(pc,yxx);
      break;
      /* instructions type yyx */
    case _p_func2f_yy:
      pc = NEXTOP(pc,yyx);
      break;
      /* instructions type yyxx */
    case _get_yy_var:
    case _put_y_vals:
      pc = NEXTOP(pc,yyxx);
      break;
#ifdef YAPOR
      /* instructions type Otapl */
    case _getwork:
    case _getwork_seq:
    case _sync:
      clause_code = FALSE;
      pp = pc->y_u.Otapl.p;
      pc = NEXTOP(pc,Otapl);
      break;
      /* instructions type e */
    case _getwork_first_time:
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
      clause_code = FALSE;
      pp = pc->y_u.Otapl.p;
      pc = NEXTOP(pc,Otapl);
      break;
      /* instructions type e */
#ifdef TABLING_INNER_CUTS
    case _clause_with_cut:
#endif
      pc = NEXTOP(pc,e);
      break;
      /* instructions type s */
    case _table_new_answer:
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
      pc = NEXTOP(pc,e);
      break;
#endif
      /* this instruction is hardwired */
    case _or_last:
#ifdef YAPOR
      pp = pc->y_u.Osblp.p0;
      if (pp->PredFlags & MegaClausePredFlag)
        return found_mega_clause(pp, startp, endp);
      clause_code = TRUE;
      pc = NEXTOP(pc,Osblp);
#else
      pp = pc->y_u.p.p;
      if (pp->PredFlags & MegaClausePredFlag)
        return found_mega_clause(pp, startp, endp);
      clause_code = TRUE;
      pc = NEXTOP(pc,p);
#endif
    }
  }
