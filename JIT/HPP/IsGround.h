COUNT IsGround(yamop*);

COUNT
IsGround(yamop* _p) {
  op_numbers op;
  COUNT c = 0;
  COUNT n = 0;
  while( (op = Yap_op_from_opcode(_p->opc)) ){
  n += 1;
  switch(op){
      case _Ystop                      :
        _p = ((yamop *)(&((_p)->y_u.l.next)));
        break;
      case _Nstop                      :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _try_me                     :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _retry_me                   :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _trust_me                   :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _enter_profiling            :
        _p = ((yamop *)(&((_p)->y_u.p.next)));
        break;
      case _retry_profiled             :
        _p = ((yamop *)(&((_p)->y_u.p.next)));
        break;
      case _profiled_retry_me          :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _profiled_trust_me          :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _profiled_retry_logical     :
        _p = ((yamop *)(&((_p)->y_u.OtaLl.next)));
        break;
      case _profiled_trust_logical     :
        _p = ((yamop *)(&((_p)->y_u.OtILl.next)));
        break;
      case _count_call                 :
        _p = ((yamop *)(&((_p)->y_u.p.next)));
        break;
      case _count_retry                :
        _p = ((yamop *)(&((_p)->y_u.p.next)));
        break;
      case _count_retry_me             :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _count_trust_me             :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _count_retry_logical        :
        _p = ((yamop *)(&((_p)->y_u.OtaLl.next)));
        break;
      case _count_trust_logical        :
        _p = ((yamop *)(&((_p)->y_u.OtILl.next)));
        break;
      case _lock_lu                    :
        _p = ((yamop *)(&((_p)->y_u.p.next)));
        break;
      case _unlock_lu                  :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _alloc_for_logical_pred     :
        _p = ((yamop *)(&((_p)->y_u.L.next)));
        break;
      case _copy_idb_term              :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _unify_idb_term             :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _ensure_space               :
        _p = ((yamop *)(&((_p)->y_u.Osbpa.next)));
        break;
      case _spy_or_trymark             :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _try_and_mark               :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _count_retry_and_mark       :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _profiled_retry_and_mark    :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _retry_and_mark             :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _trust_fail                 :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _op_fail                    :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _cut                        :
        _p = ((yamop *)(&((_p)->y_u.s.next)));
        c = 1;
        break;
      case _cut_t                      :
        _p = ((yamop *)(&((_p)->y_u.s.next)));
        c = 1;
        break;
      case _cut_e                      :
        _p = ((yamop *)(&((_p)->y_u.s.next)));
        c = 1;
        break;
      case _save_b_x                   :
        _p = ((yamop *)(&((_p)->y_u.x.next)));
        break;
      case _save_b_y                   :
        _p = ((yamop *)(&((_p)->y_u.y.next)));
        break;
      case _commit_b_x                 :
        _p = ((yamop *)(&((_p)->y_u.xps.next)));
        break;
      case _commit_b_y                 :
        _p = ((yamop *)(&((_p)->y_u.yps.next)));
        break;
      case _execute                    :
        _p = ((yamop *)(&((_p)->y_u.pp.next)));
		l = (CELL)_p;
		if (n > 5) return 0;
		break;
      case _dexecute                   :
        _p = ((yamop *)(&((_p)->y_u.pp.next)));
		l = (CELL)_p;
		if (n > 5) return 0;
		break;
      case _fcall                      :
        _p = ((yamop *)(&((_p)->y_u.Osbpp.next)));
        if (c || n <= 3) { c = 0; break; }
        return 0;
      case _call                       :
        _p = ((yamop *)(&((_p)->y_u.Osbpp.next)));
        if (c || n <= 3) { c = 0; break; }
        return 0;
      case _procceed                   :
        _p = ((yamop *)(&((_p)->y_u.p.next)));
		l = (CELL)_p;
		break;
      case _allocate                   :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _deallocate                 :
        _p = ((yamop *)(&((_p)->y_u.p.next)));
        break;
#ifdef BEAM
      case _retry_eam                  :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
#endif
#ifdef BEAM
      case _run_eam                    :
        _p = ((yamop *)(&((_p)->y_u.os.next)));
        break;
#endif
      case _get_x_var                  :
        _p = ((yamop *)(&((_p)->y_u.xx.next)));
        break;
      case _get_y_var                  :
        _p = ((yamop *)(&((_p)->y_u.yx.next)));
        break;
      case _get_yy_var                 :
        _p = ((yamop *)(&((_p)->y_u.yyxx.next)));
        break;
      case _get_x_val                  :
        _p = ((yamop *)(&((_p)->y_u.xx.next)));
        break;
      case _get_y_val                  :
        _p = ((yamop *)(&((_p)->y_u.yx.next)));
        break;
      case _get_atom                   :
        _p = ((yamop *)(&((_p)->y_u.xc.next)));
        break;
      case _get_2atoms                 :
        _p = ((yamop *)(&((_p)->y_u.cc.next)));
        break;
      case _get_3atoms                 :
        _p = ((yamop *)(&((_p)->y_u.ccc.next)));
        break;
      case _get_4atoms                 :
        _p = ((yamop *)(&((_p)->y_u.cccc.next)));
        break;
      case _get_5atoms                 :
        _p = ((yamop *)(&((_p)->y_u.ccccc.next)));
        break;
      case _get_6atoms                 :
        _p = ((yamop *)(&((_p)->y_u.cccccc.next)));
        break;
      case _get_list                   :
        _p = ((yamop *)(&((_p)->y_u.x.next)));
        break;
      case _get_struct                 :
        _p = ((yamop *)(&((_p)->y_u.xfa.next)));
        break;
      case _get_float                  :
        _p = ((yamop *)(&((_p)->y_u.xd.next)));
        break;
      case _get_longint                :
        _p = ((yamop *)(&((_p)->y_u.xi.next)));
        break;
      case _get_bigint                 :
        _p = ((yamop *)(&((_p)->y_u.xN.next)));
        break;
      case _get_dbterm                 :
        _p = ((yamop *)(&((_p)->y_u.xD.next)));
        break;
      case _glist_valx                 :
        _p = ((yamop *)(&((_p)->y_u.xx.next)));
        break;
      case _glist_valy                 :
        _p = ((yamop *)(&((_p)->y_u.yx.next)));
        break;
      case _gl_void_varx               :
        _p = ((yamop *)(&((_p)->y_u.xx.next)));
        break;
      case _gl_void_vary               :
        _p = ((yamop *)(&((_p)->y_u.yx.next)));
        break;
      case _gl_void_valx               :
        _p = ((yamop *)(&((_p)->y_u.xx.next)));
        break;
      case _gl_void_valy               :
        _p = ((yamop *)(&((_p)->y_u.yx.next)));
        break;
      case _unify_x_var                :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _unify_x_var_write          :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _unify_l_x_var              :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _unify_l_x_var_write        :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _unify_x_var2               :
        _p = ((yamop *)(&((_p)->y_u.oxx.next)));
        break;
      case _unify_x_var2_write         :
        _p = ((yamop *)(&((_p)->y_u.oxx.next)));
        break;
      case _unify_l_x_var2             :
        _p = ((yamop *)(&((_p)->y_u.oxx.next)));
        break;
      case _unify_l_x_var2_write       :
        _p = ((yamop *)(&((_p)->y_u.oxx.next)));
        break;
      case _unify_y_var                :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _unify_y_var_write          :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _unify_l_y_var              :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _unify_l_y_var_write        :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _unify_x_val                :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _unify_x_val_write          :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _unify_l_x_val              :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _unify_l_x_val_write        :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _unify_y_val                :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _unify_y_val_write          :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _unify_l_y_val              :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _unify_l_y_val_write        :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _unify_x_loc                :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _unify_x_loc_write          :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _unify_l_x_loc              :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _unify_l_x_loc_write        :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _unify_y_loc                :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _unify_y_loc_write          :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _unify_l_y_loc              :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _unify_l_y_loc_write        :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _unify_void                 :
        _p = ((yamop *)(&((_p)->y_u.o.next)));
        break;
      case _unify_void_write           :
        _p = ((yamop *)(&((_p)->y_u.o.next)));
        break;
      case _unify_l_void               :
        _p = ((yamop *)(&((_p)->y_u.o.next)));
        break;
      case _unify_l_void_write         :
        _p = ((yamop *)(&((_p)->y_u.o.next)));
        break;
      case _unify_n_voids              :
        _p = ((yamop *)(&((_p)->y_u.os.next)));
        break;
      case _unify_n_voids_write        :
        _p = ((yamop *)(&((_p)->y_u.os.next)));
        break;
      case _unify_l_n_voids            :
        _p = ((yamop *)(&((_p)->y_u.os.next)));
        break;
      case _unify_l_n_voids_write      :
        _p = ((yamop *)(&((_p)->y_u.os.next)));
        break;
      case _unify_atom                 :
        _p = ((yamop *)(&((_p)->y_u.oc.next)));
        break;
      case _unify_atom_write           :
        _p = ((yamop *)(&((_p)->y_u.oc.next)));
        break;
      case _unify_l_atom               :
        _p = ((yamop *)(&((_p)->y_u.oc.next)));
        break;
      case _unify_l_atom_write         :
        _p = ((yamop *)(&((_p)->y_u.oc.next)));
        break;
      case _unify_n_atoms              :
        _p = ((yamop *)(&((_p)->y_u.osc.next)));
        break;
      case _unify_n_atoms_write        :
        _p = ((yamop *)(&((_p)->y_u.osc.next)));
        break;
      case _unify_float                :
        _p = ((yamop *)(&((_p)->y_u.od.next)));
        break;
      case _unify_float_write          :
        _p = ((yamop *)(&((_p)->y_u.od.next)));
        break;
      case _unify_l_float              :
        _p = ((yamop *)(&((_p)->y_u.od.next)));
        break;
      case _unify_l_float_write        :
        _p = ((yamop *)(&((_p)->y_u.od.next)));
        break;
      case _unify_longint              :
        _p = ((yamop *)(&((_p)->y_u.oi.next)));
        break;
      case _unify_longint_write        :
        _p = ((yamop *)(&((_p)->y_u.oi.next)));
        break;
      case _unify_l_longint            :
        _p = ((yamop *)(&((_p)->y_u.oi.next)));
        break;
      case _unify_l_longint_write      :
        _p = ((yamop *)(&((_p)->y_u.oi.next)));
        break;
      case _unify_bigint               :
        _p = ((yamop *)(&((_p)->y_u.oN.next)));
        break;
      case _unify_l_bigint             :
        _p = ((yamop *)(&((_p)->y_u.oN.next)));
        break;
      case _unify_dbterm               :
        _p = ((yamop *)(&((_p)->y_u.oD.next)));
        break;
      case _unify_l_dbterm             :
        _p = ((yamop *)(&((_p)->y_u.oD.next)));
        break;
      case _unify_list                 :
        _p = ((yamop *)(&((_p)->y_u.o.next)));
        break;
      case _unify_list_write           :
        _p = ((yamop *)(&((_p)->y_u.o.next)));
        break;
      case _unify_l_list               :
        _p = ((yamop *)(&((_p)->y_u.o.next)));
        break;
      case _unify_l_list_write         :
        _p = ((yamop *)(&((_p)->y_u.o.next)));
        break;
      case _unify_struct               :
        _p = ((yamop *)(&((_p)->y_u.ofa.next)));
        break;
      case _unify_struct_write         :
        _p = ((yamop *)(&((_p)->y_u.ofa.next)));
        break;
      case _unify_l_struc              :
        _p = ((yamop *)(&((_p)->y_u.ofa.next)));
        break;
      case _unify_l_struc_write        :
        _p = ((yamop *)(&((_p)->y_u.ofa.next)));
        break;
      case _put_x_var                  :
        _p = ((yamop *)(&((_p)->y_u.xx.next)));
        break;
      case _put_y_var                  :
        _p = ((yamop *)(&((_p)->y_u.yx.next)));
        break;
      case _put_x_val                  :
        _p = ((yamop *)(&((_p)->y_u.xx.next)));
        break;
      case _put_xx_val                 :
        _p = ((yamop *)(&((_p)->y_u.xxxx.next)));
        break;
      case _put_y_val                  :
        _p = ((yamop *)(&((_p)->y_u.yx.next)));
        break;
      case _put_y_vals                 :
        _p = ((yamop *)(&((_p)->y_u.yyxx.next)));
        break;
      case _put_unsafe                 :
        _p = ((yamop *)(&((_p)->y_u.yx.next)));
        break;
      case _put_atom                   :
        _p = ((yamop *)(&((_p)->y_u.xc.next)));
        break;
      case _put_dbterm                 :
        _p = ((yamop *)(&((_p)->y_u.xD.next)));
        break;
      case _put_bigint                 :
        _p = ((yamop *)(&((_p)->y_u.xN.next)));
        break;
      case _put_float                  :
        _p = ((yamop *)(&((_p)->y_u.xd.next)));
        break;
      case _put_longint                :
        _p = ((yamop *)(&((_p)->y_u.xi.next)));
        break;
      case _put_list                   :
        _p = ((yamop *)(&((_p)->y_u.x.next)));
        break;
      case _put_struct                 :
        _p = ((yamop *)(&((_p)->y_u.xfa.next)));
        break;
      case _write_x_var                :
        _p = ((yamop *)(&((_p)->y_u.x.next)));
        break;
      case _write_void                 :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _write_n_voids              :
        _p = ((yamop *)(&((_p)->y_u.s.next)));
        break;
      case _write_y_var                :
        _p = ((yamop *)(&((_p)->y_u.y.next)));
        break;
      case _write_x_val                :
        _p = ((yamop *)(&((_p)->y_u.x.next)));
        break;
      case _write_x_loc                :
        _p = ((yamop *)(&((_p)->y_u.x.next)));
        break;
      case _write_y_val                :
        _p = ((yamop *)(&((_p)->y_u.y.next)));
        break;
      case _write_y_loc                :
        _p = ((yamop *)(&((_p)->y_u.y.next)));
        break;
      case _write_atom                 :
        _p = ((yamop *)(&((_p)->y_u.c.next)));
        break;
      case _write_bigint               :
        _p = ((yamop *)(&((_p)->y_u.N.next)));
        break;
      case _write_dbterm               :
        _p = ((yamop *)(&((_p)->y_u.D.next)));
        break;
      case _write_float                :
        _p = ((yamop *)(&((_p)->y_u.d.next)));
        break;
      case _write_longint              :
        _p = ((yamop *)(&((_p)->y_u.i.next)));
        break;
      case _write_n_atoms              :
        _p = ((yamop *)(&((_p)->y_u.sc.next)));
        break;
      case _write_list                 :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _write_l_list               :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _write_struct               :
        _p = ((yamop *)(&((_p)->y_u.fa.next)));
        break;
      case _write_l_struc              :
        _p = ((yamop *)(&((_p)->y_u.fa.next)));
        break;
      case _save_pair_x                :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _save_pair_x_write          :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _save_pair_y                :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _save_pair_y_write          :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _save_appl_x                :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _save_appl_x_write          :
        _p = ((yamop *)(&((_p)->y_u.ox.next)));
        break;
      case _save_appl_y                :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _save_appl_y_write          :
        _p = ((yamop *)(&((_p)->y_u.oy.next)));
        break;
      case _jump                       :
        _p = ((yamop *)(&((_p)->y_u.l.next)));
        break;
      case _move_back                  :
        _p = ((yamop *)(&((_p)->y_u.l.next)));
        break;
      case _skip                       :
        _p = ((yamop *)(&((_p)->y_u.l.next)));
        break;
      case _either                     :
        _p = ((yamop *)(&((_p)->y_u.Osblp.next)));
        break;
      case _or_else                    :
        _p = ((yamop *)(&((_p)->y_u.Osblp.next)));
        break;
      case _pop_n                      :
        _p = ((yamop *)(&((_p)->y_u.s.next)));
        break;
      case _pop                        :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _call_cpred                 :
        _p = ((yamop *)(&((_p)->y_u.Osbpp.next)));
        break;
      case _execute_cpred              :
        _p = ((yamop *)(&((_p)->y_u.pp.next)));
        break;
      case _call_usercpred             :
        _p = ((yamop *)(&((_p)->y_u.Osbpp.next)));
        break;
      case _call_c_wfail               :
        _p = ((yamop *)(&((_p)->y_u.slp.next)));
        break;
      case _try_c                      :
        _p = ((yamop *)(&((_p)->y_u.OtapFs.next)));
        break;
      case _retry_c                    :
        _p = ((yamop *)(&((_p)->y_u.OtapFs.next)));
        break;
#ifdef CUT_C
      case _cut_c                      :
        _p = ((yamop *)(&((_p)->y_u.OtapFs.next)));
        c = 1;
        break;
#endif
      case _try_userc                  :
        _p = ((yamop *)(&((_p)->y_u.OtapFs.next)));
        break;
      case _retry_userc                :
        _p = ((yamop *)(&((_p)->y_u.OtapFs.next)));
        break;
#ifdef CUT_C
      case _cut_userc                  :
        _p = ((yamop *)(&((_p)->y_u.OtapFs.next)));
        c = 1;
        break;
#endif
      case _lock_pred                  :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _index_pred                 :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
#ifdef THREADS
      case _thread_local               :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
#endif
      case _expand_index               :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _expand_clauses             :
        _p = ((yamop *)(&((_p)->y_u.sssllp.next)));
        break;
      case _undef_p                    :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _spy_pred                   :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _try_clause                 :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _try_clause2                :
        _p = ((yamop *)(&((_p)->y_u.l.next)));
        break;
      case _try_clause3                :
        _p = ((yamop *)(&((_p)->y_u.l.next)));
        break;
      case _try_clause4                :
        _p = ((yamop *)(&((_p)->y_u.l.next)));
        break;
      case _retry                      :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _retry2                     :
        _p = ((yamop *)(&((_p)->y_u.l.next)));
        break;
      case _retry3                     :
        _p = ((yamop *)(&((_p)->y_u.l.next)));
        break;
      case _retry4                     :
        _p = ((yamop *)(&((_p)->y_u.l.next)));
        break;
      case _trust                      :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _try_in                     :
        _p = ((yamop *)(&((_p)->y_u.l.next)));
        break;
      case _try_logical                :
        _p = ((yamop *)(&((_p)->y_u.OtaLl.next)));
        break;
      case _retry_logical              :
        _p = ((yamop *)(&((_p)->y_u.OtaLl.next)));
        break;
      case _trust_logical              :
        _p = ((yamop *)(&((_p)->y_u.OtILl.next)));
        break;
      case _user_switch                :
        _p = ((yamop *)(&((_p)->y_u.lp.next)));
        break;
      case _switch_on_type             :
        _p = ((yamop *)(&((_p)->y_u.llll.next)));
        break;
      case _switch_list_nl             :
        _p = ((yamop *)(&((_p)->y_u.ollll.next)));
        break;
      case _switch_on_arg_type         :
        _p = ((yamop *)(&((_p)->y_u.xllll.next)));
        break;
      case _switch_on_sub_arg_type     :
        _p = ((yamop *)(&((_p)->y_u.sllll.next)));
        break;
      case _jump_if_var                :
        _p = ((yamop *)(&((_p)->y_u.l.next)));
        break;
      case _jump_if_nonvar             :
        _p = ((yamop *)(&((_p)->y_u.xll.next)));
        break;
      case _if_not_then                :
        _p = ((yamop *)(&((_p)->y_u.clll.next)));
        break;
      case _switch_on_func             :
        _p = ((yamop *)(&((_p)->y_u.sssl.next)));
        break;
      case _switch_on_cons             :
        _p = ((yamop *)(&((_p)->y_u.sssl.next)));
        break;
      case _go_on_func                 :
        _p = ((yamop *)(&((_p)->y_u.sssl.next)));
        break;
      case _go_on_cons                 :
        _p = ((yamop *)(&((_p)->y_u.sssl.next)));
        break;
      case _if_func                    :
        _p = ((yamop *)(&((_p)->y_u.sssl.next)));
        break;
      case _if_cons                    :
        _p = ((yamop *)(&((_p)->y_u.sssl.next)));
        break;
      case _index_dbref                :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _index_blob                 :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _index_long                 :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _jit_handler                  :
        _p = ((yamop *)(&((_p)->y_u.J.next)));
        break;
      case _p_atom_x                   :
        _p = ((yamop *)(&((_p)->y_u.xl.next)));
        break;
      case _p_atom_y                   :
        _p = ((yamop *)(&((_p)->y_u.yl.next)));
        break;
      case _p_atomic_x                 :
        _p = ((yamop *)(&((_p)->y_u.xl.next)));
        break;
      case _p_atomic_y                 :
        _p = ((yamop *)(&((_p)->y_u.yl.next)));
        break;
      case _p_integer_x                :
        _p = ((yamop *)(&((_p)->y_u.xl.next)));
        break;
      case _p_integer_y                :
        _p = ((yamop *)(&((_p)->y_u.yl.next)));
        break;
      case _p_nonvar_x                 :
        _p = ((yamop *)(&((_p)->y_u.xl.next)));
        break;
      case _p_nonvar_y                 :
        _p = ((yamop *)(&((_p)->y_u.yl.next)));
        break;
      case _p_number_x                 :
        _p = ((yamop *)(&((_p)->y_u.xl.next)));
        break;
      case _p_number_y                 :
        _p = ((yamop *)(&((_p)->y_u.yl.next)));
        break;
      case _p_var_x                    :
        _p = ((yamop *)(&((_p)->y_u.xl.next)));
        break;
      case _p_var_y                    :
        _p = ((yamop *)(&((_p)->y_u.yl.next)));
        break;
      case _p_db_ref_x                 :
        _p = ((yamop *)(&((_p)->y_u.xl.next)));
        break;
      case _p_db_ref_y                 :
        _p = ((yamop *)(&((_p)->y_u.yl.next)));
        break;
      case _p_primitive_x              :
        _p = ((yamop *)(&((_p)->y_u.xl.next)));
        break;
      case _p_primitive_y              :
        _p = ((yamop *)(&((_p)->y_u.yl.next)));
        break;
      case _p_compound_x               :
        _p = ((yamop *)(&((_p)->y_u.xl.next)));
        break;
      case _p_compound_y               :
        _p = ((yamop *)(&((_p)->y_u.yl.next)));
        break;
      case _p_float_x                  :
        _p = ((yamop *)(&((_p)->y_u.xl.next)));
        break;
      case _p_float_y                  :
        _p = ((yamop *)(&((_p)->y_u.yl.next)));
        break;
      case _p_plus_vv                  :
        _p = ((yamop *)(&((_p)->y_u.xxx.next)));
        break;
      case _p_plus_vc                  :
        _p = ((yamop *)(&((_p)->y_u.xxn.next)));
        break;
      case _p_plus_y_vv                :
        _p = ((yamop *)(&((_p)->y_u.yxx.next)));
        break;
      case _p_plus_y_vc                :
        _p = ((yamop *)(&((_p)->y_u.yxn.next)));
        break;
      case _p_minus_vv                 :
        _p = ((yamop *)(&((_p)->y_u.xxx.next)));
        break;
      case _p_minus_cv                 :
        _p = ((yamop *)(&((_p)->y_u.xxn.next)));
        break;
      case _p_minus_y_vv               :
        _p = ((yamop *)(&((_p)->y_u.yxx.next)));
        break;
      case _p_minus_y_cv               :
        _p = ((yamop *)(&((_p)->y_u.yxn.next)));
        break;
      case _p_times_vv                 :
        _p = ((yamop *)(&((_p)->y_u.xxx.next)));
        break;
      case _p_times_vc                 :
        _p = ((yamop *)(&((_p)->y_u.xxn.next)));
        break;
      case _p_times_y_vv               :
        _p = ((yamop *)(&((_p)->y_u.yxx.next)));
        break;
      case _p_times_y_vc               :
        _p = ((yamop *)(&((_p)->y_u.yxn.next)));
        break;
      case _p_div_vv                   :
        _p = ((yamop *)(&((_p)->y_u.xxx.next)));
        break;
      case _p_div_vc                   :
        _p = ((yamop *)(&((_p)->y_u.xxn.next)));
        break;
      case _p_div_cv                   :
        _p = ((yamop *)(&((_p)->y_u.xxn.next)));
        break;
      case _p_div_y_vv                 :
        _p = ((yamop *)(&((_p)->y_u.yxx.next)));
        break;
      case _p_div_y_vc                 :
        _p = ((yamop *)(&((_p)->y_u.yxn.next)));
        break;
      case _p_div_y_cv                 :
        _p = ((yamop *)(&((_p)->y_u.yxn.next)));
        break;
      case _p_and_vv                   :
        _p = ((yamop *)(&((_p)->y_u.xxx.next)));
        break;
      case _p_and_vc                   :
        _p = ((yamop *)(&((_p)->y_u.xxn.next)));
        break;
      case _p_and_y_vv                 :
        _p = ((yamop *)(&((_p)->y_u.yxx.next)));
        break;
      case _p_and_y_vc                 :
        _p = ((yamop *)(&((_p)->y_u.yxn.next)));
        break;
      case _p_or_vv                    :
        _p = ((yamop *)(&((_p)->y_u.xxx.next)));
        break;
      case _p_or_vc                    :
        _p = ((yamop *)(&((_p)->y_u.xxn.next)));
        break;
      case _p_or_y_vv                  :
        _p = ((yamop *)(&((_p)->y_u.yxx.next)));
        break;
      case _p_or_y_vc                  :
        _p = ((yamop *)(&((_p)->y_u.yxn.next)));
        break;
      case _p_sll_vv                   :
        _p = ((yamop *)(&((_p)->y_u.xxx.next)));
        break;
      case _p_sll_vc                   :
        _p = ((yamop *)(&((_p)->y_u.xxn.next)));
        break;
      case _p_sll_cv                   :
        _p = ((yamop *)(&((_p)->y_u.xxn.next)));
        break;
      case _p_sll_y_vv                 :
        _p = ((yamop *)(&((_p)->y_u.yxx.next)));
        break;
      case _p_sll_y_vc                 :
        _p = ((yamop *)(&((_p)->y_u.yxn.next)));
        break;
      case _p_sll_y_cv                 :
        _p = ((yamop *)(&((_p)->y_u.yxn.next)));
        break;
      case _p_slr_vv                   :
        _p = ((yamop *)(&((_p)->y_u.xxx.next)));
        break;
      case _p_slr_vc                   :
        _p = ((yamop *)(&((_p)->y_u.xxn.next)));
        break;
      case _p_slr_cv                   :
        _p = ((yamop *)(&((_p)->y_u.xxn.next)));
        break;
      case _p_slr_y_vv                 :
        _p = ((yamop *)(&((_p)->y_u.yxx.next)));
        break;
      case _p_slr_y_vc                 :
        _p = ((yamop *)(&((_p)->y_u.yxn.next)));
        break;
      case _p_slr_y_cv                 :
        _p = ((yamop *)(&((_p)->y_u.yxn.next)));
        break;
      case _call_bfunc_xx              :
        _p = ((yamop *)(&((_p)->y_u.plxxs.next)));
        break;
      case _call_bfunc_yx              :
        _p = ((yamop *)(&((_p)->y_u.plxys.next)));
        break;
      case _call_bfunc_xy              :
        _p = ((yamop *)(&((_p)->y_u.plxys.next)));
        break;
      case _call_bfunc_yy              :
        _p = ((yamop *)(&((_p)->y_u.plyys.next)));
        break;
      case _p_equal                    :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _p_dif                      :
        _p = ((yamop *)(&((_p)->y_u.l.next)));
        break;
      case _p_eq                       :
        _p = ((yamop *)(&((_p)->y_u.l.next)));
        break;
      case _p_arg_vv                   :
        _p = ((yamop *)(&((_p)->y_u.xxx.next)));
        break;
      case _p_arg_cv                   :
        _p = ((yamop *)(&((_p)->y_u.xxn.next)));
        break;
      case _p_arg_y_vv                 :
        _p = ((yamop *)(&((_p)->y_u.yxx.next)));
        break;
      case _p_arg_y_cv                 :
        _p = ((yamop *)(&((_p)->y_u.yxn.next)));
        break;
      case _p_func2s_vv                :
        _p = ((yamop *)(&((_p)->y_u.xxx.next)));
        break;
      case _p_func2s_cv                :
        _p = ((yamop *)(&((_p)->y_u.xxc.next)));
        break;
      case _p_func2s_vc                :
        _p = ((yamop *)(&((_p)->y_u.xxn.next)));
        break;
      case _p_func2s_y_vv              :
        _p = ((yamop *)(&((_p)->y_u.yxx.next)));
        break;
      case _p_func2s_y_cv              :
        _p = ((yamop *)(&((_p)->y_u.yxn.next)));
        break;
      case _p_func2s_y_vc              :
        _p = ((yamop *)(&((_p)->y_u.yxn.next)));
        break;
      case _p_func2f_xx                :
        _p = ((yamop *)(&((_p)->y_u.xxx.next)));
        break;
      case _p_func2f_xy                :
        _p = ((yamop *)(&((_p)->y_u.xxy.next)));
        break;
      case _p_func2f_yx                :
        _p = ((yamop *)(&((_p)->y_u.yxx.next)));
        break;
      case _p_func2f_yy                :
        _p = ((yamop *)(&((_p)->y_u.yyx.next)));
        break;
      case _p_functor                  :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _p_execute2                 :
        _p = ((yamop *)(&((_p)->y_u.Osbpp.next)));
        break;
      case _p_execute                  :
        _p = ((yamop *)(&((_p)->y_u.Osbmp.next)));
        break;
      case _p_execute_tail             :
        _p = ((yamop *)(&((_p)->y_u.Osbpp.next)));
        break;
#ifdef YAPOR
      case _getwork_first_time         :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _getwork                    :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _getwork_seq                :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _sync                       :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
#endif
#ifdef TABLING
#ifdef TABLING_INNER_CUTS
      case _clause_with_cut            :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
#endif
      case _table_load_answer          :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _table_try_answer           :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _table_try_single           :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _table_try_me               :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _table_try                  :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _table_retry_me             :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _table_retry                :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _table_trust_me             :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _table_trust                :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _table_new_answer           :
        _p = ((yamop *)(&((_p)->y_u.s.next)));
        break;
      case _table_answer_resolution    :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
      case _table_completion           :
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
#ifdef THREADS_CONSUMER_SHARING
      case _table_answer_resolution_completion:
        _p = ((yamop *)(&((_p)->y_u.Otapl.next)));
        break;
#endif
      case _trie_do_var                :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_var             :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_var               :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_var             :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_do_var_in_pair        :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_var_in_pair     :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_var_in_pair       :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_var_in_pair     :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_do_val                :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_val             :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_val               :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_val             :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_do_val_in_pair        :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_val_in_pair     :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_val_in_pair       :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_val_in_pair     :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_do_atom               :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_atom            :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_atom              :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_atom            :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_do_atom_in_pair       :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_atom_in_pair    :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_atom_in_pair      :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_atom_in_pair    :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_do_null               :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_null            :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_null              :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_null            :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_do_null_in_pair       :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_null_in_pair    :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_null_in_pair      :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_null_in_pair    :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_do_pair               :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_pair            :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_pair              :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_pair            :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_do_appl               :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_appl            :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_appl              :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_appl            :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_do_appl_in_pair       :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_appl_in_pair    :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_appl_in_pair      :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_appl_in_pair    :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_do_extension          :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_extension       :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_extension         :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_extension       :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_do_double             :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_double          :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_double            :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_double          :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_do_longint            :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_longint         :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_longint           :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_longint         :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_do_gterm              :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_trust_gterm           :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_try_gterm             :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
      case _trie_retry_gterm           :
        _p = ((yamop *)(&((_p)->y_u.e.next)));
        break;
#endif
#ifdef YAPOR
      case _or_last                    :
        _p = ((yamop *)(&((_p)->y_u.sblp.next)));
        break;
#else
      case _or_last                    :
        _p = ((yamop *)(&((_p)->y_u.p.next)));
        break;
#endif
  default:
    return 0;
    }
  }
  return 1;
}

