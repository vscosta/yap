void print_preg(yamop*);

void
print_preg(yamop* _p) {
  op_numbers op;
  while( (op = Yap_op_from_opcode(_p->opc)) ){
//      printf("%p -- ", _p);
    switch(op){
      case _Ystop                      :
        printf("_Ystop!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _Nstop                      :
        printf("_Nstop!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _try_me                     :
        printf("_try_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _retry_me                   :
        printf("_retry_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _trust_me                   :
        printf("_trust_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _enter_profiling            :
        printf("_enter_profiling!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

      case _retry_profiled             :
        printf("_retry_profiled!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

      case _profiled_retry_me          :
        printf("_profiled_retry_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _profiled_trust_me          :
        printf("_profiled_trust_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _profiled_retry_logical     :
        printf("_profiled_retry_logical!!\n");
        _p = ((yamop *)(&((_p)->u.OtaLl.next)));
        break;

      case _profiled_trust_logical     :
        printf("_profiled_trust_logical!!\n");
        _p = ((yamop *)(&((_p)->u.OtILl.next)));
        break;

      case _count_call                 :
        printf("_count_call!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

      case _count_retry                :
        printf("_count_retry!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

      case _count_retry_me             :
        printf("_count_retry_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _count_trust_me             :
        printf("_count_trust_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _count_retry_logical        :
        printf("_count_retry_logical!!\n");
        _p = ((yamop *)(&((_p)->u.OtaLl.next)));
        break;

      case _count_trust_logical        :
        printf("_count_trust_logical!!\n");
        _p = ((yamop *)(&((_p)->u.OtILl.next)));
        break;

      case _lock_lu                    :
        printf("_lock_lu!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

      case _unlock_lu                  :
        printf("_unlock_lu!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _alloc_for_logical_pred     :
        printf("_alloc_for_logical_pred!!\n");
        _p = ((yamop *)(&((_p)->u.L.next)));
        break;

      case _copy_idb_term              :
        printf("_copy_idb_term!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _unify_idb_term             :
        printf("_unify_idb_term!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _ensure_space               :
        printf("_ensure_space!!\n");
        _p = ((yamop *)(&((_p)->u.Osbpa.next)));
        break;

      case _spy_or_trymark             :
        printf("_spy_or_trymark!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _try_and_mark               :
        printf("_try_and_mark!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _count_retry_and_mark       :
        printf("_count_retry_and_mark!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _profiled_retry_and_mark    :
        printf("_profiled_retry_and_mark!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _retry_and_mark             :
        printf("_retry_and_mark!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _trust_fail                 :
        printf("_trust_fail!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _op_fail                    :
        printf("_op_fail!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _cut                        :
        printf("_cut!!\n");
        _p = ((yamop *)(&((_p)->u.s.next)));
        break;

      case _cut_t                      :
        printf("_cut_t!!\n");
        _p = ((yamop *)(&((_p)->u.s.next)));
        break;

      case _cut_e                      :
        printf("_cut_e!!\n");
        _p = ((yamop *)(&((_p)->u.s.next)));
        break;

      case _save_b_x                   :
        printf("_save_b_x!!\n");
        _p = ((yamop *)(&((_p)->u.x.next)));
        break;

      case _save_b_y                   :
        printf("_save_b_y!!\n");
        _p = ((yamop *)(&((_p)->u.y.next)));
        break;

      case _commit_b_x                 :
        printf("_commit_b_x!!\n");
        _p = ((yamop *)(&((_p)->u.xps.next)));
        break;

      case _commit_b_y                 :
        printf("_commit_b_y!!\n");
        _p = ((yamop *)(&((_p)->u.yps.next)));
        break;

      case _execute                    :
        printf("_execute!!\n");
        _p = ((yamop *)(&((_p)->u.pp.next)));
        break;

      case _dexecute                   :
        printf("_dexecute!!\n");
        _p = ((yamop *)(&((_p)->u.pp.next)));
        break;

      case _fcall                      :
        printf("_fcall!!\n");
        _p = ((yamop *)(&((_p)->u.Osbpp.next)));
        break;

      case _call                       :
        printf("_call!!\n");
        _p = ((yamop *)(&((_p)->u.Osbpp.next)));
        break;

      case _procceed                   :
        printf("_procceed!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

      case _allocate                   :
        printf("_allocate!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _deallocate                 :
        printf("_deallocate!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

#ifdef BEAM
      case _retry_eam                  :
        printf("_retry_eam!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

#endif
#ifdef BEAM
      case _run_eam                    :
        printf("_run_eam!!\n");
        _p = ((yamop *)(&((_p)->u.os.next)));
        break;

#endif
      case _get_x_var                  :
        printf("_get_x_var!!\n");
        _p = ((yamop *)(&((_p)->u.xx.next)));
        break;

      case _get_y_var                  :
        printf("_get_y_var!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _get_yy_var                 :
        printf("_get_yy_var!!\n");
        _p = ((yamop *)(&((_p)->u.yyxx.next)));
        break;

      case _get_x_val                  :
        printf("_get_x_val!!\n");
        _p = ((yamop *)(&((_p)->u.xx.next)));
        break;

      case _get_y_val                  :
        printf("_get_y_val!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _get_atom                   :
        printf("_get_atom!!\n");
        _p = ((yamop *)(&((_p)->u.xc.next)));
        break;

      case _get_2atoms                 :
        printf("_get_2atoms!!\n");
        _p = ((yamop *)(&((_p)->u.cc.next)));
        break;

      case _get_3atoms                 :
        printf("_get_3atoms!!\n");
        _p = ((yamop *)(&((_p)->u.ccc.next)));
        break;

      case _get_4atoms                 :
        printf("_get_4atoms!!\n");
        _p = ((yamop *)(&((_p)->u.cccc.next)));
        break;

      case _get_5atoms                 :
        printf("_get_5atoms!!\n");
        _p = ((yamop *)(&((_p)->u.ccccc.next)));
        break;

      case _get_6atoms                 :
        printf("_get_6atoms!!\n");
        _p = ((yamop *)(&((_p)->u.cccccc.next)));
        break;

      case _get_list                   :
        printf("_get_list!!\n");
        _p = ((yamop *)(&((_p)->u.x.next)));
        break;

      case _get_struct                 :
        printf("_get_struct!!\n");
        _p = ((yamop *)(&((_p)->u.xfa.next)));
        break;

      case _get_float                  :
        printf("_get_float!!\n");
        _p = ((yamop *)(&((_p)->u.xd.next)));
        break;

      case _get_longint                :
        printf("_get_longint!!\n");
        _p = ((yamop *)(&((_p)->u.xi.next)));
        break;

      case _get_bigint                 :
        printf("_get_bigint!!\n");
        _p = ((yamop *)(&((_p)->u.xN.next)));
        break;

      case _get_dbterm                 :
        printf("_get_dbterm!!\n");
        _p = ((yamop *)(&((_p)->u.xD.next)));
        break;

      case _glist_valx                 :
        printf("_glist_valx!!\n");
        _p = ((yamop *)(&((_p)->u.xx.next)));
        break;

      case _glist_valy                 :
        printf("_glist_valy!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _gl_void_varx               :
        printf("_gl_void_varx!!\n");
        _p = ((yamop *)(&((_p)->u.xx.next)));
        break;

      case _gl_void_vary               :
        printf("_gl_void_vary!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _gl_void_valx               :
        printf("_gl_void_valx!!\n");
        _p = ((yamop *)(&((_p)->u.xx.next)));
        break;

      case _gl_void_valy               :
        printf("_gl_void_valy!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _unify_x_var                :
        printf("_unify_x_var!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _unify_x_var_write          :
        printf("_unify_x_var_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _unify_l_x_var              :
        printf("_unify_l_x_var!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _unify_l_x_var_write        :
        printf("_unify_l_x_var_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _unify_x_var2               :
        printf("_unify_x_var2!!\n");
        _p = ((yamop *)(&((_p)->u.oxx.next)));
        break;

      case _unify_x_var2_write         :
        printf("_unify_x_var2_write!!\n");
        _p = ((yamop *)(&((_p)->u.oxx.next)));
        break;

      case _unify_l_x_var2             :
        printf("_unify_l_x_var2!!\n");
        _p = ((yamop *)(&((_p)->u.oxx.next)));
        break;

      case _unify_l_x_var2_write       :
        printf("_unify_l_x_var2_write!!\n");
        _p = ((yamop *)(&((_p)->u.oxx.next)));
        break;

      case _unify_y_var                :
        printf("_unify_y_var!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _unify_y_var_write          :
        printf("_unify_y_var_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _unify_l_y_var              :
        printf("_unify_l_y_var!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _unify_l_y_var_write        :
        printf("_unify_l_y_var_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _unify_x_val                :
        printf("_unify_x_val!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _unify_x_val_write          :
        printf("_unify_x_val_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _unify_l_x_val              :
        printf("_unify_l_x_val!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _unify_l_x_val_write        :
        printf("_uify_l_x_val_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _unify_y_val                :
        printf("_unify_y_val!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _unify_y_val_write          :
        printf("_unify_y_val_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _unify_l_y_val              :
        printf("_unify_l_y_val!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _unify_l_y_val_write        :
        printf("_unify_l_y_val_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _unify_x_loc                :
        printf("_unify_x_loc!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _unify_x_loc_write          :
        printf("_unify_x_loc_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _unify_l_x_loc              :
        printf("_unify_l_x_loc!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _unify_l_x_loc_write        :
        printf("_unify_l_x_loc_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _unify_y_loc                :
        printf("_unify_y_loc!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _unify_y_loc_write          :
        printf("_unify_y_loc_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _unify_l_y_loc              :
        printf("_unify_l_y_loc!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _unify_l_y_loc_write        :
        printf("_unify_l_y_loc_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _unify_void                 :
        printf("_unify_void!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _unify_void_write           :
        printf("_unify_void_write!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _unify_l_void               :
        printf("_unify_l_void!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _unify_l_void_write         :
        printf("_unify_l_void_write!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _unify_n_voids              :
        printf("_unify_n_voids!!\n");
        _p = ((yamop *)(&((_p)->u.os.next)));
        break;

      case _unify_n_voids_write        :
        printf("_unify_n_voids_write!!\n");
        _p = ((yamop *)(&((_p)->u.os.next)));
        break;

      case _unify_l_n_voids            :
        printf("_unify_l_n_voids!!\n");
        _p = ((yamop *)(&((_p)->u.os.next)));
        break;

      case _unify_l_n_voids_write      :
        printf("_unify_l_n_voids_write!!\n");
        _p = ((yamop *)(&((_p)->u.os.next)));
        break;

      case _unify_atom                 :
        printf("_unify_atom!!\n");
        _p = ((yamop *)(&((_p)->u.oc.next)));
        break;

      case _unify_atom_write           :
        printf("_unify_atom_write!!\n");
        _p = ((yamop *)(&((_p)->u.oc.next)));
        break;

      case _unify_l_atom               :
        printf("_unify_l_atom!!\n");
        _p = ((yamop *)(&((_p)->u.oc.next)));
        break;

      case _unify_l_atom_write         :
        printf("_unify_l_atom_write!!\n");
        _p = ((yamop *)(&((_p)->u.oc.next)));
        break;

      case _unify_n_atoms              :
        printf("_unify_n_atoms!!\n");
        _p = ((yamop *)(&((_p)->u.osc.next)));
        break;

      case _unify_n_atoms_write        :
        printf("_unify_n_atoms_write!!\n");
        _p = ((yamop *)(&((_p)->u.osc.next)));
        break;

      case _unify_float                :
        printf("_unify_float!!\n");
        _p = ((yamop *)(&((_p)->u.od.next)));
        break;

      case _unify_float_write          :
        printf("_unify_float_write!!\n");
        _p = ((yamop *)(&((_p)->u.od.next)));
        break;

      case _unify_l_float              :
        printf("_unify_l_float!!\n");
        _p = ((yamop *)(&((_p)->u.od.next)));
        break;

      case _unify_l_float_write        :
        printf("_unify_l_float_write!!\n");
        _p = ((yamop *)(&((_p)->u.od.next)));
        break;

      case _unify_longint              :
        printf("_unify_longint!!\n");
        _p = ((yamop *)(&((_p)->u.oi.next)));
        break;

      case _unify_longint_write        :
        printf("_unify_longint_write!!\n");
        _p = ((yamop *)(&((_p)->u.oi.next)));
        break;

      case _unify_l_longint            :
        printf("_unify_l_longint!!\n");
        _p = ((yamop *)(&((_p)->u.oi.next)));
        break;

      case _unify_l_longint_write      :
        printf("_unify_l_longint_write!!\n");
        _p = ((yamop *)(&((_p)->u.oi.next)));
        break;

      case _unify_bigint               :
        printf("_unify_bigint!!\n");
        _p = ((yamop *)(&((_p)->u.oN.next)));
        break;

      case _unify_l_bigint             :
        printf("_unify_l_bigint!!\n");
        _p = ((yamop *)(&((_p)->u.oN.next)));
        break;

      case _unify_dbterm               :
        printf("_unify_dbterm!!\n");
        _p = ((yamop *)(&((_p)->u.oD.next)));
        break;

      case _unify_l_dbterm             :
        printf("_unify_l_dbterm!!\n");
        _p = ((yamop *)(&((_p)->u.oD.next)));
        break;

      case _unify_list                 :
        printf("_unify_list!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _unify_list_write           :
        printf("_unify_list_write!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _unify_l_list               :
        printf("_unify_l_list!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _unify_l_list_write         :
        printf("_unify_l_list_write!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _unify_struct               :
        printf("_unify_struct!!\n");
        _p = ((yamop *)(&((_p)->u.ofa.next)));
        break;

      case _unify_struct_write         :
        printf("_unify_struct_write!!\n");
        _p = ((yamop *)(&((_p)->u.ofa.next)));
        break;

      case _unify_l_struc              :
        printf("_unify_l_struc!!\n");
        _p = ((yamop *)(&((_p)->u.ofa.next)));
        break;

      case _unify_l_struc_write        :
        printf("_unify_l_struc_write!!\n");
        _p = ((yamop *)(&((_p)->u.ofa.next)));
        break;

      case _put_x_var                  :
        printf("_put_x_var!!\n");
        _p = ((yamop *)(&((_p)->u.xx.next)));
        break;

      case _put_y_var                  :
        printf("_put_y_var!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _put_x_val                  :
        printf("_put_x_val!!\n");
        _p = ((yamop *)(&((_p)->u.xx.next)));
        break;

      case _put_xx_val                 :
        printf("_put_xx_val!!\n");
        _p = ((yamop *)(&((_p)->u.xxxx.next)));
        break;

      case _put_y_val                  :
        printf("_put_y_val!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _put_y_vals                 :
        printf("_put_y_vals!!\n");
        _p = ((yamop *)(&((_p)->u.yyxx.next)));
        break;

      case _put_unsafe                 :
        printf("_put_unsafe!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _put_atom                   :
        printf("_put_atom!!\n");
        _p = ((yamop *)(&((_p)->u.xc.next)));
        break;

      case _put_dbterm                 :
        printf("_put_dbterm!!\n");
        _p = ((yamop *)(&((_p)->u.xD.next)));
        break;

      case _put_bigint                 :
        printf("_put_bigint!!\n");
        _p = ((yamop *)(&((_p)->u.xN.next)));
        break;

      case _put_float                  :
        printf("_put_float!!\n");
        _p = ((yamop *)(&((_p)->u.xd.next)));
        break;

      case _put_longint                :
        printf("_put_longint!!\n");
        _p = ((yamop *)(&((_p)->u.xi.next)));
        break;

      case _put_list                   :
        printf("_put_list!!\n");
        _p = ((yamop *)(&((_p)->u.x.next)));
        break;

      case _put_struct                 :
        printf("_put_struct!!\n");
        _p = ((yamop *)(&((_p)->u.xfa.next)));
        break;

      case _write_x_var                :
        printf("_write_x_var!!\n");
        _p = ((yamop *)(&((_p)->u.x.next)));
        break;

      case _write_void                 :
        printf("_write_void!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _write_n_voids              :
        printf("_write_n_voids!!\n");
        _p = ((yamop *)(&((_p)->u.s.next)));
        break;

      case _write_y_var                :
        printf("_write_y_var!!\n");
        _p = ((yamop *)(&((_p)->u.y.next)));
        break;

      case _write_x_val                :
        printf("_write_x_val!!\n");
        _p = ((yamop *)(&((_p)->u.x.next)));
        break;

      case _write_x_loc                :
        printf("_write_x_loc!!\n");
        _p = ((yamop *)(&((_p)->u.x.next)));
        break;

      case _write_y_val                :
        printf("_write_y_val!!\n");
        _p = ((yamop *)(&((_p)->u.y.next)));
        break;

      case _write_y_loc                :
        printf("_write_y_loc!!\n");
        _p = ((yamop *)(&((_p)->u.y.next)));
        break;

      case _write_atom                 :
        printf("_write_atom!!\n");
        _p = ((yamop *)(&((_p)->u.c.next)));
        break;

      case _write_bigint               :
        printf("_write_bigint!!\n");
        _p = ((yamop *)(&((_p)->u.N.next)));
        break;

      case _write_dbterm               :
        printf("_write_dbterm!!\n");
        _p = ((yamop *)(&((_p)->u.D.next)));
        break;

      case _write_float                :
        printf("_write_float!!\n");
        _p = ((yamop *)(&((_p)->u.d.next)));
        break;

      case _write_longint              :
        printf("_write_longint!!\n");
        _p = ((yamop *)(&((_p)->u.i.next)));
        break;

      case _write_n_atoms              :
        printf("_write_n_atoms!!\n");
        _p = ((yamop *)(&((_p)->u.sc.next)));
        break;

      case _write_list                 :
        printf("_write_list!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _write_l_list               :
        printf("_write_l_list!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _write_struct               :
        printf("_write_struct!!\n");
        _p = ((yamop *)(&((_p)->u.fa.next)));
        break;

      case _write_l_struc              :
        printf("_write_l_struc!!\n");
        _p = ((yamop *)(&((_p)->u.fa.next)));
        break;

      case _save_pair_x                :
        printf("_save_pair_x!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _save_pair_x_write          :
        printf("_save_pair_x_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _save_pair_y                :
        printf("_save_pair_y!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _save_pair_y_write          :
        printf("_save_pair_y_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _save_appl_x                :
        printf("_save_appl_x!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _save_appl_x_write          :
        printf("_save_appl_x_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _save_appl_y                :
        printf("_save_appl_y!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _save_appl_y_write          :
        printf("_save_appl_y_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _jump                       :
        printf("_jump!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _move_back                  :
        printf("_move_back!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _skip                       :
        printf("_skip!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _either                     :
        printf("_either!!\n");
        _p = ((yamop *)(&((_p)->u.Osblp.next)));
        break;

      case _or_else                    :
        printf("_or_else!!\n");
        _p = ((yamop *)(&((_p)->u.Osblp.next)));
        break;

      case _pop_n                      :
        printf("_pop_n!!\n");
        _p = ((yamop *)(&((_p)->u.s.next)));
        break;

      case _pop                        :
        printf("_pop!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _call_cpred                 :
        printf("_call_cpred!!\n");
        _p = ((yamop *)(&((_p)->u.Osbpp.next)));
        break;

      case _execute_cpred              :
        printf("_execute_cpred!!\n");
        _p = ((yamop *)(&((_p)->u.pp.next)));
        break;

      case _call_usercpred             :
        printf("_call_usercpred!!\n");
        _p = ((yamop *)(&((_p)->u.Osbpp.next)));
        break;

      case _call_c_wfail               :
        printf("_call_x_wfail!!\n");
        _p = ((yamop *)(&((_p)->u.slp.next)));
        break;

      case _try_c                      :
        printf("_try_c!!\n");
        _p = ((yamop *)(&((_p)->u.OtapFs.next)));
        break;

      case _retry_c                    :
        printf("_retry_c!!\n");
        _p = ((yamop *)(&((_p)->u.OtapFs.next)));
        break;

#ifdef CUT_C
      case _cut_c                      :
        printf("_cut_c!!\n");
        _p = ((yamop *)(&((_p)->u.OtapFs.next)));
        break;

#endif
      case _try_userc                  :
        printf("_try_userc!!\n");
        _p = ((yamop *)(&((_p)->u.OtapFs.next)));
        break;

      case _retry_userc                :
        printf("_retry_userc!!\n");
        _p = ((yamop *)(&((_p)->u.OtapFs.next)));
        break;

#ifdef CUT_C
      case _cut_userc                  :
        printf("_cut_userc!!\n");
        _p = ((yamop *)(&((_p)->u.OtapFs.next)));
        break;

#endif
      case _lock_pred                  :
        printf("_lock_pred!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _index_pred                 :
        printf("_index_pred!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

#ifdef THREADS
      case _thread_local               :
        printf("_thread_local!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

#endif
      case _expand_index               :
        printf("_expand_index!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _expand_clauses             :
        printf("_expand_clauses!!\n");
        _p = ((yamop *)(&((_p)->u.sssllp.next)));
        break;

      case _undef_p                    :
        printf("_undef_p!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _spy_pred                   :
        printf("_spy_pred!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _try_clause                 :
        printf("_try_clause!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _try_clause2                :
        printf("_try_clause2!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _try_clause3                :
        printf("_try_clause3!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _try_clause4                :
        printf("_try_clause4!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _retry                      :
        printf("_retry!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _retry2                     :
        printf("_retry2!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _retry3                     :
        printf("_retry3!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _retry4                     :
        printf("_retry4!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _trust                      :
        printf("_trust!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _try_in                     :
        printf("_try_in!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _enter_lu_pred              :
        printf("_enter_lu_pred!!\n");
        _p = ((yamop *)(&((_p)->u.Illss.next)));
        break;

      case _try_logical                :
        printf("_try_logical!!\n");
        _p = ((yamop *)(&((_p)->u.OtaLl.next)));
        break;

      case _retry_logical              :
        printf("_retry_logical!!\n");
        _p = ((yamop *)(&((_p)->u.OtaLl.next)));
        break;

      case _trust_logical              :
        printf("_trust_logical!!\n");
        _p = ((yamop *)(&((_p)->u.OtILl.next)));
        break;

      case _user_switch                :
        printf("_user_switch!!\n");
        _p = ((yamop *)(&((_p)->u.lp.next)));
        break;

      case _switch_on_type             :
        printf("_switch_on_type!!\n");
        _p = ((yamop *)(&((_p)->u.llll.next)));
        break;

      case _switch_list_nl             :
        printf("_switch_list_nl!!\n");
        _p = ((yamop *)(&((_p)->u.ollll.next)));
        break;

      case _switch_on_arg_type         :
        printf("_switch_on_arg_type!!\n");
        _p = ((yamop *)(&((_p)->u.xllll.next)));
        break;

      case _switch_on_sub_arg_type     :
        printf("_switch_on_sub_arg_type!!\n");
        _p = ((yamop *)(&((_p)->u.sllll.next)));
        break;

      case _jump_if_var                :
        printf("_jump_if_var!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _jump_if_nonvar             :
        printf("_jump_if_nonvar!!\n");
        _p = ((yamop *)(&((_p)->u.xll.next)));
        break;

      case _if_not_then                :
        printf("_if_not_then!!\n");
        _p = ((yamop *)(&((_p)->u.clll.next)));
        break;

      case _switch_on_func             :
        printf("_switch_on_func!!\n");
        _p = ((yamop *)(&((_p)->u.sssl.next)));
        break;

      case _switch_on_cons             :
        printf("_switch_on_cons!!\n");
        _p = ((yamop *)(&((_p)->u.sssl.next)));
        break;

      case _go_on_func                 :
        printf("_go_on_func!!\n");
        _p = ((yamop *)(&((_p)->u.sssl.next)));
        break;

      case _go_on_cons                 :
        printf("_go_on_cons!!\n");
        _p = ((yamop *)(&((_p)->u.sssl.next)));
        break;

      case _if_func                    :
        printf("_if_func!!\n");
        _p = ((yamop *)(&((_p)->u.sssl.next)));
        break;

      case _if_cons                    :
        printf("_if_cons!!\n");
        _p = ((yamop *)(&((_p)->u.sssl.next)));
        break;

      case _index_dbref                :
        printf("_index_dbref!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _index_blob                 :
        printf("_index_blob!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _index_long                 :
        printf("_index_long!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

#if YAP_JIT
      case _jit_handler                  :
        printf("_jit_handler!!\n");
        _p = ((yamop *)(&((_p)->u.jhc.next)));
        break;
#endif

      case _p_atom_x                   :
        printf("_p_atom_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _p_atom_y                   :
        printf("_p_atom_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _p_atomic_x                 :
        printf("_p_atomic_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _p_atomic_y                 :
        printf("_p_atomic_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _p_integer_x                :
        printf("_p_integer_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _p_integer_y                :
        printf("_p_integer_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _p_nonvar_x                 :
        printf("_p_nonvar_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _p_nonvar_y                 :
        printf("_p_nonvar_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _p_number_x                 :
        printf("_p_number_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _p_number_y                 :
        printf("_p_number_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _p_var_x                    :
        printf("_p_var_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _p_var_y                    :
        printf("_p_var_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _p_db_ref_x                 :
        printf("_p_db_ref_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _p_db_ref_y                 :
        printf("_p_db_ref_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _p_primitive_x              :
        printf("_p_primitive_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _p_primitive_y              :
        printf("_p_primitive_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _p_compound_x               :
        printf("_p_compound_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _p_compound_y               :
        printf("_p_compound_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _p_float_x                  :
        printf("_p_float_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _p_float_y                  :
        printf("_p_float_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _p_plus_vv                  :
        printf("_p_plus_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _p_plus_vc                  :
        printf("_p_plus_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _p_plus_y_vv                :
        printf("_p_plus_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _p_plus_y_vc                :
        printf("_p_plus_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _p_minus_vv                 :
        printf("_p_minus_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _p_minus_cv                 :
        printf("_p_minus_cv!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _p_minus_y_vv               :
        printf("_p_minus_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _p_minus_y_cv               :
        printf("_p_minus_y_cv!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _p_times_vv                 :
        printf("_p_times_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _p_times_vc                 :
        printf("_p_times_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _p_times_y_vv               :
        printf("_p_times_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _p_times_y_vc               :
        printf("_p_times_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _p_div_vv                   :
        printf("_p_div_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _p_div_vc                   :
        printf("_p_div_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _p_div_cv                   :
        printf("_p_div_cv!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _p_div_y_vv                 :
        printf("_p_div_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _p_div_y_vc                 :
        printf("_p_div_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _p_div_y_cv                 :
        printf("_p_div_y_cv!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _p_and_vv                   :
        printf("_p_and_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _p_and_vc                   :
        printf("_p_and_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _p_and_y_vv                 :
        printf("_p_and_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _p_and_y_vc                 :
        printf("_p_and_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _p_or_vv                    :
        printf("_p_or_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _p_or_vc                    :
        printf("_p_or_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _p_or_y_vv                  :
        printf("_p_or_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _p_or_y_vc                  :
        printf("_p_or_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _p_sll_vv                   :
        printf("_p_sll_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _p_sll_vc                   :
        printf("_p_sll_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _p_sll_cv                   :
        printf("_p_sll_cv!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _p_sll_y_vv                 :
        printf("_p_sll_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _p_sll_y_vc                 :
        printf("_p_sll_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _p_sll_y_cv                 :
        printf("_p_sll_y_cv!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _p_slr_vv                   :
        printf("_p_slr_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _p_slr_vc                   :
        printf("_p_slr_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _p_slr_cv                   :
        printf("_p_slr_cv!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _p_slr_y_vv                 :
        printf("_p_slr_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _p_slr_y_vc                 :
        printf("_p_slr_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _p_slr_y_cv                 :
        printf("_p_slr_y_cv!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _call_bfunc_xx              :
        printf("_call_bfunc_xx!!\n");
        _p = ((yamop *)(&((_p)->u.plxxs.next)));
        break;

      case _call_bfunc_yx              :
        printf("_call_bfunc_yx!!\n");
        _p = ((yamop *)(&((_p)->u.plxys.next)));
        break;

      case _call_bfunc_xy              :
        printf("_call_bfunc_xy!!\n");
        _p = ((yamop *)(&((_p)->u.plxys.next)));
        break;

      case _call_bfunc_yy              :
        printf("_call_bfunc_yy!!\n");
        _p = ((yamop *)(&((_p)->u.plyys.next)));
        break;

      case _p_equal                    :
        printf("_p_equal!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _p_dif                      :
        printf("_p_dif!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _p_eq                       :
        printf("_p_eq!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _p_arg_vv                   :
        printf("_p_arg_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _p_arg_cv                   :
        printf("_p_arg_cv!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _p_arg_y_vv                 :
        printf("_p_arg_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _p_arg_y_cv                 :
        printf("_p_arg_y_cv!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _p_func2s_vv                :
        printf("_p_func2s_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _p_func2s_cv                :
        printf("_p_func2s_cv!!\n");
        _p = ((yamop *)(&((_p)->u.xxc.next)));
        break;

      case _p_func2s_vc                :
        printf("_p_func2s_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _p_func2s_y_vv              :
        printf("_p_func2s_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _p_func2s_y_cv              :
        printf("_p_func2s_y_cv!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _p_func2s_y_vc              :
        printf("_p_func2s_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _p_func2f_xx                :
        printf("_p_func2f_xx!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _p_func2f_xy                :
        printf("_p_func2f_xy!!\n");
        _p = ((yamop *)(&((_p)->u.xxy.next)));
        break;

      case _p_func2f_yx                :
        printf("_p_func2f_yx!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _p_func2f_yy                :
        printf("_p_func2f_yy!!\n");
        _p = ((yamop *)(&((_p)->u.yyx.next)));
        break;

      case _p_functor                  :
        printf("_p_functor!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _p_execute2                 :
        printf("_p_execute2!!\n");
        _p = ((yamop *)(&((_p)->u.Osbpp.next)));
        break;

      case _p_execute                  :
        printf("_p_execute!!\n");
        _p = ((yamop *)(&((_p)->u.Osbmp.next)));
        break;

      case _p_execute_tail             :
        printf("_p_execute_tail!!\n");
        _p = ((yamop *)(&((_p)->u.Osbpp.next)));
        break;

#ifdef YAPOR
      case _getwork_first_time         :
        printf("_getwork_first_time!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _getwork                    :
        printf("_getwork!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _getwork_seq                :
        printf("_getwork_seq!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _sync                       :
        printf("_sync!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

#endif
#ifdef TABLING
#ifdef TABLING_INNER_CUTS
      case _clause_with_cut            :
        printf("_clause_with_cut!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

#endif
      case _table_load_answer          :
        printf("_table_load_answer!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _table_try_answer           :
        printf("_table_try_answer!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _table_try_single           :
        printf("_table_try_single!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _table_try_me               :
        printf("_table_try_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _table_try                  :
        printf("_table_try!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _table_retry_me             :
        printf("_table_retry_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _table_retry                :
        printf("_table_retry!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _table_trust_me             :
        printf("_table_trust_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _table_trust                :
        printf("_table_trust!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _table_new_answer           :
        printf("_table_new_answer!!\n");
        _p = ((yamop *)(&((_p)->u.s.next)));
        break;

      case _table_answer_resolution    :
        printf("_table_answer_resolution!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _table_completion           :
        printf("_table_completion!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

#ifdef THREADS_CONSUMER_SHARING
      case _table_answer_resolution_completion:
        printf("_table_answer_resolution_completion!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

#endif
      case _trie_do_var                :
        printf("_trie_do_var!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_var             :
        printf("_trie_trust_var!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_var               :
        printf("_trie_try_var!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_var             :
        printf("_trie_retry_var!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_do_var_in_pair        :
        printf("_trie_do_var_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_var_in_pair     :
        printf("_trie_trust_var_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_var_in_pair       :
        printf("_trie_try_var_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_var_in_pair     :
        printf("_trie_retry_var_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_do_val                :
        printf("_trie_do_val!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_val             :
        printf("_trie_trust_val!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_val               :
        printf("_trie_try_val!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_val             :
        printf("_trie_retry_val!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_do_val_in_pair        :
        printf("_trie_do_val_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_val_in_pair     :
        printf("_trie_trust_val_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_val_in_pair       :
        printf("_trie_try_val_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_val_in_pair     :
        printf("_trie_retry_val_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_do_atom               :
        printf("_trie_do_atom!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_atom            :
        printf("_trie_trust_atom!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_atom              :
        printf("_trie_try_atom!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_atom            :
        printf("_trie_retry_atom!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_do_atom_in_pair       :
        printf("_trie_do_atom_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_atom_in_pair    :
        printf("_trie_trust_atom_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_atom_in_pair      :
        printf("_trie_try_atom_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_atom_in_pair    :
        printf("_trie_retry_atom_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_do_null               :
        printf("_trie_do_null!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_null            :
        printf("_trie_trust_null!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_null              :
        printf("_trie_try_null!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_null            :
        printf("_trie_retry_null!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_do_null_in_pair       :
        printf("_trie_do_null_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_null_in_pair    :
        printf("_trie_trust_null_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_null_in_pair      :
        printf("_tri_try_null_in_paire!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_null_in_pair    :
        printf("_trie_retry_null_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_do_pair               :
        printf("_trie_do_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_pair            :
        printf("_trie_trust_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_pair              :
        printf("_trie_try_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_pair            :
        printf("_trie_retry_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_do_appl               :
        printf("_trie_do_appl!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_appl            :
        printf("_trie_trust_appl!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_appl              :
        printf("_trie_try_appl!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_appl            :
        printf("_trie_retry_appl!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_do_appl_in_pair       :
        printf("_trie_do_appl_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_appl_in_pair    :
        printf("_trie_trust_appl_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_appl_in_pair      :
        printf("_trie_trty_appkl_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_appl_in_pair    :
        printf("_trie_retry_appl_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_do_extension          :
        printf("_trie_do_extension!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_extension       :
        printf("_trie_trust_extension!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_extension         :
        printf("_trie_try_extension!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_extension       :
        printf("_trie_retry_extension!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_do_double             :
        printf("_trie_do_double!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_double          :
        printf("_trie_trust_double!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_double            :
        printf("_trie_try_double!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_double          :
        printf("_trie_retry_double!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_do_longint            :
        printf("_trie_do_longint!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_longint         :
        printf("_trie_trust_longint!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_longint           :
        printf("_trie_try_longint!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_longint         :
        printf("_trie_retry_longint!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_do_gterm              :
        printf("_trie_do_gterm!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_trust_gterm           :
        printf("_trie_trust_gterm!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_try_gterm             :
        printf("_trie_try_gterm!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _trie_retry_gterm           :
        printf("_trie_retry_gterm!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

#endif
  /* this instruction is hardwired */
#ifdef YAPOR
      case _or_last                    :
        printf("_or_last!!\n");
        _p = ((yamop *)(&((_p)->u.sblp.next)));
        break;

#else
      case _or_last                    :
        printf("_or_last!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

#endif
#if YAP_JIT
      case _traced_Ystop                      :
        printf("_Ystop!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _traced_Nstop                      :
        printf("_Nstop!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_try_me                     :
        printf("_try_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_retry_me                   :
        printf("_retry_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_trust_me                   :
        printf("_trust_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_enter_profiling            :
        printf("_enter_profiling!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

      case _traced_retry_profiled             :
        printf("_retry_profiled!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

      case _traced_profiled_retry_me          :
        printf("_profiled_retry_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_profiled_trust_me          :
        printf("_profiled_trust_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_profiled_retry_logical     :
        printf("_profiled_retry_logical!!\n");
        _p = ((yamop *)(&((_p)->u.OtaLl.next)));
        break;

      case _traced_profiled_trust_logical     :
        printf("_profiled_trust_logical!!\n");
        _p = ((yamop *)(&((_p)->u.OtILl.next)));
        break;

      case _traced_count_call                 :
        printf("_count_call!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

      case _traced_count_retry                :
        printf("_count_retry!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

      case _traced_count_retry_me             :
        printf("_count_retry_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_count_trust_me             :
        printf("_count_trust_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_count_retry_logical        :
        printf("_count_retry_logical!!\n");
        _p = ((yamop *)(&((_p)->u.OtaLl.next)));
        break;

      case _traced_count_trust_logical        :
        printf("_count_trust_logical!!\n");
        _p = ((yamop *)(&((_p)->u.OtILl.next)));
        break;

      case _traced_lock_lu                    :
        printf("_lock_lu!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

      case _traced_unlock_lu                  :
        printf("_unlock_lu!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_alloc_for_logical_pred     :
        printf("_alloc_for_logical_pred!!\n");
        _p = ((yamop *)(&((_p)->u.L.next)));
        break;

      case _traced_copy_idb_term              :
        printf("_copy_idb_term!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_unify_idb_term             :
        printf("_unify_idb_term!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_ensure_space               :
        printf("_ensure_space!!\n");
        _p = ((yamop *)(&((_p)->u.Osbpa.next)));
        break;

      case _traced_spy_or_trymark             :
        printf("_spy_or_trymark!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_try_and_mark               :
        printf("_try_and_mark!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_count_retry_and_mark       :
        printf("_count_retry_and_mark!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_profiled_retry_and_mark    :
        printf("_profiled_retry_and_mark!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_retry_and_mark             :
        printf("_retry_and_mark!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_trust_fail                 :
        printf("_trust_fail!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_op_fail                    :
        printf("_op_fail!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_cut                        :
        printf("_cut!!\n");
        _p = ((yamop *)(&((_p)->u.s.next)));
        break;

      case _traced_cut_t                      :
        printf("_cut_t!!\n");
        _p = ((yamop *)(&((_p)->u.s.next)));
        break;

      case _traced_cut_e                      :
        printf("_cut_e!!\n");
        _p = ((yamop *)(&((_p)->u.s.next)));
        break;

      case _traced_save_b_x                   :
        printf("_save_b_x!!\n");
        _p = ((yamop *)(&((_p)->u.x.next)));
        break;

      case _traced_save_b_y                   :
        printf("_save_b_y!!\n");
        _p = ((yamop *)(&((_p)->u.y.next)));
        break;

      case _traced_commit_b_x                 :
        printf("_commit_b_x!!\n");
        _p = ((yamop *)(&((_p)->u.xps.next)));
        break;

      case _traced_commit_b_y                 :
        printf("_commit_b_y!!\n");
        _p = ((yamop *)(&((_p)->u.yps.next)));
        break;

      case _traced_execute                    :
        printf("_execute!!\n");
        _p = ((yamop *)(&((_p)->u.pp.next)));
        break;

      case _traced_dexecute                   :
        printf("_dexecute!!\n");
        _p = ((yamop *)(&((_p)->u.pp.next)));
        break;

      case _traced_fcall                      :
        printf("_fcall!!\n");
        _p = ((yamop *)(&((_p)->u.Osbpp.next)));
        break;

      case _traced_call                       :
        printf("_call!!\n");
        _p = ((yamop *)(&((_p)->u.Osbpp.next)));
        break;

      case _traced_procceed                   :
        printf("_procceed!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

      case _traced_allocate                   :
        printf("_allocate!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_deallocate                 :
        printf("_deallocate!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;

#ifdef BEAM
      case _traced_retry_eam                  :
        printf("_retry_eam!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

#endif
#ifdef BEAM
      case _traced_run_eam                    :
        printf("_run_eam!!\n");
        _p = ((yamop *)(&((_p)->u.os.next)));
        break;

#endif
      case _traced_get_x_var                  :
        printf("_get_x_var!!\n");
        _p = ((yamop *)(&((_p)->u.xx.next)));
        break;

      case _traced_get_y_var                  :
        printf("_get_y_var!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _traced_get_yy_var                 :
        printf("_get_yy_var!!\n");
        _p = ((yamop *)(&((_p)->u.yyxx.next)));
        break;

      case _traced_get_x_val                  :
        printf("_get_x_val!!\n");
        _p = ((yamop *)(&((_p)->u.xx.next)));
        break;

      case _traced_get_y_val                  :
        printf("_get_y_val!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _traced_get_atom                   :
        printf("_get_atom!!\n");
        _p = ((yamop *)(&((_p)->u.xc.next)));
        break;

      case _traced_get_2atoms                 :
        printf("_get_2atoms!!\n");
        _p = ((yamop *)(&((_p)->u.cc.next)));
        break;

      case _traced_get_3atoms                 :
        printf("_get_3atoms!!\n");
        _p = ((yamop *)(&((_p)->u.ccc.next)));
        break;

      case _traced_get_4atoms                 :
        printf("_get_4atoms!!\n");
        _p = ((yamop *)(&((_p)->u.cccc.next)));
        break;

      case _traced_get_5atoms                 :
        printf("_get_5atoms!!\n");
        _p = ((yamop *)(&((_p)->u.ccccc.next)));
        break;

      case _traced_get_6atoms                 :
        printf("_get_6atoms!!\n");
        _p = ((yamop *)(&((_p)->u.cccccc.next)));
        break;

      case _traced_get_list                   :
        printf("_get_list!!\n");
        _p = ((yamop *)(&((_p)->u.x.next)));
        break;

      case _traced_get_struct                 :
        printf("_get_struct!!\n");
        _p = ((yamop *)(&((_p)->u.xfa.next)));
        break;

      case _traced_get_float                  :
        printf("_get_float!!\n");
        _p = ((yamop *)(&((_p)->u.xd.next)));
        break;

      case _traced_get_longint                :
        printf("_get_longint!!\n");
        _p = ((yamop *)(&((_p)->u.xi.next)));
        break;

      case _traced_get_bigint                 :
        printf("_get_bigint!!\n");
        _p = ((yamop *)(&((_p)->u.xN.next)));
        break;

      case _traced_get_dbterm                 :
        printf("_get_dbterm!!\n");
        _p = ((yamop *)(&((_p)->u.xD.next)));
        break;

      case _traced_glist_valx                 :
        printf("_glist_valx!!\n");
        _p = ((yamop *)(&((_p)->u.xx.next)));
        break;

      case _traced_glist_valy                 :
        printf("_glist_valy!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _traced_gl_void_varx               :
        printf("_gl_void_varx!!\n");
        _p = ((yamop *)(&((_p)->u.xx.next)));
        break;

      case _traced_gl_void_vary               :
        printf("_gl_void_vary!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _traced_gl_void_valx               :
        printf("_gl_void_valx!!\n");
        _p = ((yamop *)(&((_p)->u.xx.next)));
        break;

      case _traced_gl_void_valy               :
        printf("_gl_void_valy!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _traced_unify_x_var                :
        printf("_unify_x_var!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_unify_x_var_write          :
        printf("_unify_x_var_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_unify_l_x_var              :
        printf("_unify_l_x_var!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_unify_l_x_var_write        :
        printf("_unify_l_x_var_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_unify_x_var2               :
        printf("_unify_x_var2!!\n");
        _p = ((yamop *)(&((_p)->u.oxx.next)));
        break;

      case _traced_unify_x_var2_write         :
        printf("_unify_x_var2_write!!\n");
        _p = ((yamop *)(&((_p)->u.oxx.next)));
        break;

      case _traced_unify_l_x_var2             :
        printf("_unify_l_x_var2!!\n");
        _p = ((yamop *)(&((_p)->u.oxx.next)));
        break;

      case _traced_unify_l_x_var2_write       :
        printf("_unify_l_x_var2_write!!\n");
        _p = ((yamop *)(&((_p)->u.oxx.next)));
        break;

      case _traced_unify_y_var                :
        printf("_unify_y_var!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_unify_y_var_write          :
        printf("_unify_y_var_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_unify_l_y_var              :
        printf("_unify_l_y_var!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_unify_l_y_var_write        :
        printf("_unify_l_y_var_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_unify_x_val                :
        printf("_unify_x_val!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_unify_x_val_write          :
        printf("_unify_x_val_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_unify_l_x_val              :
        printf("_unify_l_x_val!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_unify_l_x_val_write        :
        printf("_uify_l_x_val_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_unify_y_val                :
        printf("_unify_y_val!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_unify_y_val_write          :
        printf("_unify_y_val_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_unify_l_y_val              :
        printf("_unify_l_y_val!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_unify_l_y_val_write        :
        printf("_unify_l_y_val_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_unify_x_loc                :
        printf("_unify_x_loc!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_unify_x_loc_write          :
        printf("_unify_x_loc_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_unify_l_x_loc              :
        printf("_unify_l_x_loc!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_unify_l_x_loc_write        :
        printf("_unify_l_x_loc_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_unify_y_loc                :
        printf("_unify_y_loc!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_unify_y_loc_write          :
        printf("_unify_y_loc_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_unify_l_y_loc              :
        printf("_unify_l_y_loc!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_unify_l_y_loc_write        :
        printf("_unify_l_y_loc_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_unify_void                 :
        printf("_unify_void!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _traced_unify_void_write           :
        printf("_unify_void_write!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _traced_unify_l_void               :
        printf("_unify_l_void!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _traced_unify_l_void_write         :
        printf("_unify_l_void_write!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _traced_unify_n_voids              :
        printf("_unify_n_voids!!\n");
        _p = ((yamop *)(&((_p)->u.os.next)));
        break;

      case _traced_unify_n_voids_write        :
        printf("_unify_n_voids_write!!\n");
        _p = ((yamop *)(&((_p)->u.os.next)));
        break;

      case _traced_unify_l_n_voids            :
        printf("_unify_l_n_voids!!\n");
        _p = ((yamop *)(&((_p)->u.os.next)));
        break;

      case _traced_unify_l_n_voids_write      :
        printf("_unify_l_n_voids_write!!\n");
        _p = ((yamop *)(&((_p)->u.os.next)));
        break;

      case _traced_unify_atom                 :
        printf("_unify_atom!!\n");
        _p = ((yamop *)(&((_p)->u.oc.next)));
        break;

      case _traced_unify_atom_write           :
        printf("_unify_atom_write!!\n");
        _p = ((yamop *)(&((_p)->u.oc.next)));
        break;

      case _traced_unify_l_atom               :
        printf("_unify_l_atom!!\n");
        _p = ((yamop *)(&((_p)->u.oc.next)));
        break;

      case _traced_unify_l_atom_write         :
        printf("_unify_l_atom_write!!\n");
        _p = ((yamop *)(&((_p)->u.oc.next)));
        break;

      case _traced_unify_n_atoms              :
        printf("_unify_n_atoms!!\n");
        _p = ((yamop *)(&((_p)->u.osc.next)));
        break;

      case _traced_unify_n_atoms_write        :
        printf("_unify_n_atoms_write!!\n");
        _p = ((yamop *)(&((_p)->u.osc.next)));
        break;

      case _traced_unify_float                :
        printf("_unify_float!!\n");
        _p = ((yamop *)(&((_p)->u.od.next)));
        break;

      case _traced_unify_float_write          :
        printf("_unify_float_write!!\n");
        _p = ((yamop *)(&((_p)->u.od.next)));
        break;

      case _traced_unify_l_float              :
        printf("_unify_l_float!!\n");
        _p = ((yamop *)(&((_p)->u.od.next)));
        break;

      case _traced_unify_l_float_write        :
        printf("_unify_l_float_write!!\n");
        _p = ((yamop *)(&((_p)->u.od.next)));
        break;

      case _traced_unify_longint              :
        printf("_unify_longint!!\n");
        _p = ((yamop *)(&((_p)->u.oi.next)));
        break;

      case _traced_unify_longint_write        :
        printf("_unify_longint_write!!\n");
        _p = ((yamop *)(&((_p)->u.oi.next)));
        break;

      case _traced_unify_l_longint            :
        printf("_unify_l_longint!!\n");
        _p = ((yamop *)(&((_p)->u.oi.next)));
        break;

      case _traced_unify_l_longint_write      :
        printf("_unify_l_longint_write!!\n");
        _p = ((yamop *)(&((_p)->u.oi.next)));
        break;

      case _traced_unify_bigint               :
        printf("_unify_bigint!!\n");
        _p = ((yamop *)(&((_p)->u.oN.next)));
        break;

      case _traced_unify_l_bigint             :
        printf("_unify_l_bigint!!\n");
        _p = ((yamop *)(&((_p)->u.oN.next)));
        break;

      case _traced_unify_dbterm               :
        printf("_unify_dbterm!!\n");
        _p = ((yamop *)(&((_p)->u.oD.next)));
        break;

      case _traced_unify_l_dbterm             :
        printf("_unify_l_dbterm!!\n");
        _p = ((yamop *)(&((_p)->u.oD.next)));
        break;

      case _traced_unify_list                 :
        printf("_unify_list!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _traced_unify_list_write           :
        printf("_unify_list_write!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _traced_unify_l_list               :
        printf("_unify_l_list!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _traced_unify_l_list_write         :
        printf("_unify_l_list_write!!\n");
        _p = ((yamop *)(&((_p)->u.o.next)));
        break;

      case _traced_unify_struct               :
        printf("_unify_struct!!\n");
        _p = ((yamop *)(&((_p)->u.ofa.next)));
        break;

      case _traced_unify_struct_write         :
        printf("_unify_struct_write!!\n");
        _p = ((yamop *)(&((_p)->u.ofa.next)));
        break;

      case _traced_unify_l_struc              :
        printf("_unify_l_struc!!\n");
        _p = ((yamop *)(&((_p)->u.ofa.next)));
        break;

      case _traced_unify_l_struc_write        :
        printf("_unify_l_struc_write!!\n");
        _p = ((yamop *)(&((_p)->u.ofa.next)));
        break;

      case _traced_put_x_var                  :
        printf("_put_x_var!!\n");
        _p = ((yamop *)(&((_p)->u.xx.next)));
        break;

      case _traced_put_y_var                  :
        printf("_put_y_var!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _traced_put_x_val                  :
        printf("_put_x_val!!\n");
        _p = ((yamop *)(&((_p)->u.xx.next)));
        break;

      case _traced_put_xx_val                 :
        printf("_put_xx_val!!\n");
        _p = ((yamop *)(&((_p)->u.xxxx.next)));
        break;

      case _traced_put_y_val                  :
        printf("_put_y_val!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _traced_put_y_vals                 :
        printf("_put_y_vals!!\n");
        _p = ((yamop *)(&((_p)->u.yyxx.next)));
        break;

      case _traced_put_unsafe                 :
        printf("_put_unsafe!!\n");
        _p = ((yamop *)(&((_p)->u.yx.next)));
        break;

      case _traced_put_atom                   :
        printf("_put_atom!!\n");
        _p = ((yamop *)(&((_p)->u.xc.next)));
        break;

      case _traced_put_dbterm                 :
        printf("_put_dbterm!!\n");
        _p = ((yamop *)(&((_p)->u.xD.next)));
        break;

      case _traced_put_bigint                 :
        printf("_put_bigint!!\n");
        _p = ((yamop *)(&((_p)->u.xN.next)));
        break;

      case _traced_put_float                  :
        printf("_put_float!!\n");
        _p = ((yamop *)(&((_p)->u.xd.next)));
        break;

      case _traced_put_longint                :
        printf("_put_longint!!\n");
        _p = ((yamop *)(&((_p)->u.xi.next)));
        break;

      case _traced_put_list                   :
        printf("_put_list!!\n");
        _p = ((yamop *)(&((_p)->u.x.next)));
        break;

      case _traced_put_struct                 :
        printf("_put_struct!!\n");
        _p = ((yamop *)(&((_p)->u.xfa.next)));
        break;

      case _traced_write_x_var                :
        printf("_write_x_var!!\n");
        _p = ((yamop *)(&((_p)->u.x.next)));
        break;

      case _traced_write_void                 :
        printf("_write_void!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_write_n_voids              :
        printf("_write_n_voids!!\n");
        _p = ((yamop *)(&((_p)->u.s.next)));
        break;

      case _traced_write_y_var                :
        printf("_write_y_var!!\n");
        _p = ((yamop *)(&((_p)->u.y.next)));
        break;

      case _traced_write_x_val                :
        printf("_write_x_val!!\n");
        _p = ((yamop *)(&((_p)->u.x.next)));
        break;

      case _traced_write_x_loc                :
        printf("_write_x_loc!!\n");
        _p = ((yamop *)(&((_p)->u.x.next)));
        break;

      case _traced_write_y_val                :
        printf("_write_y_val!!\n");
        _p = ((yamop *)(&((_p)->u.y.next)));
        break;

      case _traced_write_y_loc                :
        printf("_write_y_loc!!\n");
        _p = ((yamop *)(&((_p)->u.y.next)));
        break;

      case _traced_write_atom                 :
        printf("_write_atom!!\n");
        _p = ((yamop *)(&((_p)->u.c.next)));
        break;

      case _traced_write_bigint               :
        printf("_write_bigint!!\n");
        _p = ((yamop *)(&((_p)->u.N.next)));
        break;

      case _traced_write_dbterm               :
        printf("_write_dbterm!!\n");
        _p = ((yamop *)(&((_p)->u.D.next)));
        break;

      case _traced_write_float                :
        printf("_write_float!!\n");
        _p = ((yamop *)(&((_p)->u.d.next)));
        break;

      case _traced_write_longint              :
        printf("_write_longint!!\n");
        _p = ((yamop *)(&((_p)->u.i.next)));
        break;

      case _traced_write_n_atoms              :
        printf("_write_n_atoms!!\n");
        _p = ((yamop *)(&((_p)->u.sc.next)));
        break;

      case _traced_write_list                 :
        printf("_write_list!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_write_l_list               :
        printf("_write_l_list!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_write_struct               :
        printf("_write_struct!!\n");
        _p = ((yamop *)(&((_p)->u.fa.next)));
        break;

      case _traced_write_l_struc              :
        printf("_write_l_struc!!\n");
        _p = ((yamop *)(&((_p)->u.fa.next)));
        break;

      case _traced_save_pair_x                :
        printf("_save_pair_x!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_save_pair_x_write          :
        printf("_save_pair_x_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_save_pair_y                :
        printf("_save_pair_y!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_save_pair_y_write          :
        printf("_save_pair_y_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_save_appl_x                :
        printf("_save_appl_x!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_save_appl_x_write          :
        printf("_save_appl_x_write!!\n");
        _p = ((yamop *)(&((_p)->u.ox.next)));
        break;

      case _traced_save_appl_y                :
        printf("_save_appl_y!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_save_appl_y_write          :
        printf("_save_appl_y_write!!\n");
        _p = ((yamop *)(&((_p)->u.oy.next)));
        break;

      case _traced_jump                       :
        printf("_jump!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _traced_move_back                  :
        printf("_move_back!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _traced_skip                       :
        printf("_skip!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _traced_either                     :
        printf("_either!!\n");
        _p = ((yamop *)(&((_p)->u.Osblp.next)));
        break;

      case _traced_or_else                    :
        printf("_or_else!!\n");
        _p = ((yamop *)(&((_p)->u.Osblp.next)));
        break;

      case _traced_pop_n                      :
        printf("_pop_n!!\n");
        _p = ((yamop *)(&((_p)->u.s.next)));
        break;

      case _traced_pop                        :
        printf("_pop!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_call_cpred                 :
        printf("_call_cpred!!\n");
        _p = ((yamop *)(&((_p)->u.Osbpp.next)));
        break;

      case _traced_execute_cpred              :
        printf("_execute_cpred!!\n");
        _p = ((yamop *)(&((_p)->u.pp.next)));
        break;

      case _traced_call_usercpred             :
        printf("_call_usercpred!!\n");
        _p = ((yamop *)(&((_p)->u.Osbpp.next)));
        break;

      case _traced_call_c_wfail               :
        printf("_call_x_wfail!!\n");
        _p = ((yamop *)(&((_p)->u.slp.next)));
        break;

      case _traced_try_c                      :
        printf("_try_c!!\n");
        _p = ((yamop *)(&((_p)->u.OtapFs.next)));
        break;

      case _traced_retry_c                    :
        printf("_retry_c!!\n");
        _p = ((yamop *)(&((_p)->u.OtapFs.next)));
        break;

#ifdef CUT_C
      case _traced_cut_c                      :
        printf("_cut_c!!\n");
        _p = ((yamop *)(&((_p)->u.OtapFs.next)));
        break;

#endif
      case _traced_try_userc                  :
        printf("_try_userc!!\n");
        _p = ((yamop *)(&((_p)->u.OtapFs.next)));
        break;

      case _traced_retry_userc                :
        printf("_retry_userc!!\n");
        _p = ((yamop *)(&((_p)->u.OtapFs.next)));
        break;

#ifdef CUT_C
      case _traced_cut_userc                  :
        printf("_cut_userc!!\n");
        _p = ((yamop *)(&((_p)->u.OtapFs.next)));
        break;

#endif
      case _traced_lock_pred                  :
        printf("_lock_pred!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_index_pred                 :
        printf("_index_pred!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

#ifdef THREADS
      case _traced_thread_local               :
        printf("_thread_local!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

#endif
      case _traced_expand_index               :
        printf("_expand_index!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_expand_clauses             :
        printf("_expand_clauses!!\n");
        _p = ((yamop *)(&((_p)->u.sssllp.next)));
        break;

      case _traced_undef_p                    :
        printf("_undef_p!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_spy_pred                   :
        printf("_spy_pred!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_try_clause                 :
        printf("_try_clause!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_try_clause2                :
        printf("_try_clause2!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _traced_try_clause3                :
        printf("_try_clause3!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _traced_try_clause4                :
        printf("_try_clause4!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _traced_retry                      :
        printf("_retry!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_retry2                     :
        printf("_retry2!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _traced_retry3                     :
        printf("_retry3!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _traced_retry4                     :
        printf("_retry4!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _traced_trust                      :
        printf("_trust!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_try_in                     :
        printf("_try_in!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _traced_enter_lu_pred              :
        printf("_enter_lu_pred!!\n");
        _p = ((yamop *)(&((_p)->u.Illss.next)));
        break;

      case _traced_try_logical                :
        printf("_try_logical!!\n");
        _p = ((yamop *)(&((_p)->u.OtaLl.next)));
        break;

      case _traced_retry_logical              :
        printf("_retry_logical!!\n");
        _p = ((yamop *)(&((_p)->u.OtaLl.next)));
        break;

      case _traced_trust_logical              :
        printf("_trust_logical!!\n");
        _p = ((yamop *)(&((_p)->u.OtILl.next)));
        break;

      case _traced_user_switch                :
        printf("_user_switch!!\n");
        _p = ((yamop *)(&((_p)->u.lp.next)));
        break;

      case _traced_switch_on_type             :
        printf("_switch_on_type!!\n");
        _p = ((yamop *)(&((_p)->u.llll.next)));
        break;

      case _traced_switch_list_nl             :
        printf("_switch_list_nl!!\n");
        _p = ((yamop *)(&((_p)->u.ollll.next)));
        break;

      case _traced_switch_on_arg_type         :
        printf("_switch_on_arg_type!!\n");
        _p = ((yamop *)(&((_p)->u.xllll.next)));
        break;

      case _traced_switch_on_sub_arg_type     :
        printf("_switch_on_sub_arg_type!!\n");
        _p = ((yamop *)(&((_p)->u.sllll.next)));
        break;

      case _traced_jump_if_var                :
        printf("_jump_if_var!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _traced_jump_if_nonvar             :
        printf("_jump_if_nonvar!!\n");
        _p = ((yamop *)(&((_p)->u.xll.next)));
        break;

      case _traced_if_not_then                :
        printf("_if_not_then!!\n");
        _p = ((yamop *)(&((_p)->u.clll.next)));
        break;

      case _traced_switch_on_func             :
        printf("_switch_on_func!!\n");
        _p = ((yamop *)(&((_p)->u.sssl.next)));
        break;

      case _traced_switch_on_cons             :
        printf("_switch_on_cons!!\n");
        _p = ((yamop *)(&((_p)->u.sssl.next)));
        break;

      case _traced_go_on_func                 :
        printf("_go_on_func!!\n");
        _p = ((yamop *)(&((_p)->u.sssl.next)));
        break;

      case _traced_go_on_cons                 :
        printf("_go_on_cons!!\n");
        _p = ((yamop *)(&((_p)->u.sssl.next)));
        break;

      case _traced_if_func                    :
        printf("_if_func!!\n");
        _p = ((yamop *)(&((_p)->u.sssl.next)));
        break;

      case _traced_if_cons                    :
        printf("_if_cons!!\n");
        _p = ((yamop *)(&((_p)->u.sssl.next)));
        break;

      case _traced_index_dbref                :
        printf("_index_dbref!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_index_blob                 :
        printf("_index_blob!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_index_long                 :
        printf("_index_long!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_jit_handler                  :
        printf("_jit_handler!!\n");
        _p = ((yamop *)(&((_p)->u.jhc.next)));
        break;

      case _traced_p_atom_x                   :
        printf("_p_atom_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _traced_p_atom_y                   :
        printf("_p_atom_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _traced_p_atomic_x                 :
        printf("_p_atomic_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _traced_p_atomic_y                 :
        printf("_p_atomic_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _traced_p_integer_x                :
        printf("_p_integer_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _traced_p_integer_y                :
        printf("_p_integer_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _traced_p_nonvar_x                 :
        printf("_p_nonvar_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _traced_p_nonvar_y                 :
        printf("_p_nonvar_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _traced_p_number_x                 :
        printf("_p_number_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _traced_p_number_y                 :
        printf("_p_number_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _traced_p_var_x                    :
        printf("_p_var_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _traced_p_var_y                    :
        printf("_p_var_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _traced_p_db_ref_x                 :
        printf("_p_db_ref_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _traced_p_db_ref_y                 :
        printf("_p_db_ref_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _traced_p_primitive_x              :
        printf("_p_primitive_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _traced_p_primitive_y              :
        printf("_p_primitive_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _traced_p_compound_x               :
        printf("_p_compound_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _traced_p_compound_y               :
        printf("_p_compound_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _traced_p_float_x                  :
        printf("_p_float_x!!\n");
        _p = ((yamop *)(&((_p)->u.xl.next)));
        break;

      case _traced_p_float_y                  :
        printf("_p_float_y!!\n");
        _p = ((yamop *)(&((_p)->u.yl.next)));
        break;

      case _traced_p_plus_vv                  :
        printf("_p_plus_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _traced_p_plus_vc                  :
        printf("_p_plus_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _traced_p_plus_y_vv                :
        printf("_p_plus_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _traced_p_plus_y_vc                :
        printf("_p_plus_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _traced_p_minus_vv                 :
        printf("_p_minus_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _traced_p_minus_cv                 :
        printf("_p_minus_cv!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _traced_p_minus_y_vv               :
        printf("_p_minus_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _traced_p_minus_y_cv               :
        printf("_p_minus_y_cv!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _traced_p_times_vv                 :
        printf("_p_times_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _traced_p_times_vc                 :
        printf("_p_times_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _traced_p_times_y_vv               :
        printf("_p_times_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _traced_p_times_y_vc               :
        printf("_p_times_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _traced_p_div_vv                   :
        printf("_p_div_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _traced_p_div_vc                   :
        printf("_p_div_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _traced_p_div_cv                   :
        printf("_p_div_cv!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _traced_p_div_y_vv                 :
        printf("_p_div_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _traced_p_div_y_vc                 :
        printf("_p_div_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _traced_p_div_y_cv                 :
        printf("_p_div_y_cv!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _traced_p_and_vv                   :
        printf("_p_and_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _traced_p_and_vc                   :
        printf("_p_and_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _traced_p_and_y_vv                 :
        printf("_p_and_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _traced_p_and_y_vc                 :
        printf("_p_and_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _traced_p_or_vv                    :
        printf("_p_or_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _traced_p_or_vc                    :
        printf("_p_or_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _traced_p_or_y_vv                  :
        printf("_p_or_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _traced_p_or_y_vc                  :
        printf("_p_or_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _traced_p_sll_vv                   :
        printf("_p_sll_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _traced_p_sll_vc                   :
        printf("_p_sll_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _traced_p_sll_cv                   :
        printf("_p_sll_cv!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _traced_p_sll_y_vv                 :
        printf("_p_sll_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _traced_p_sll_y_vc                 :
        printf("_p_sll_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _traced_p_sll_y_cv                 :
        printf("_p_sll_y_cv!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _traced_p_slr_vv                   :
        printf("_p_slr_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _traced_p_slr_vc                   :
        printf("_p_slr_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _traced_p_slr_cv                   :
        printf("_p_slr_cv!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _traced_p_slr_y_vv                 :
        printf("_p_slr_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _traced_p_slr_y_vc                 :
        printf("_p_slr_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _traced_p_slr_y_cv                 :
        printf("_p_slr_y_cv!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _traced_call_bfunc_xx              :
        printf("_call_bfunc_xx!!\n");
        _p = ((yamop *)(&((_p)->u.plxxs.next)));
        break;

      case _traced_call_bfunc_yx              :
        printf("_call_bfunc_yx!!\n");
        _p = ((yamop *)(&((_p)->u.plxys.next)));
        break;

      case _traced_call_bfunc_xy              :
        printf("_call_bfunc_xy!!\n");
        _p = ((yamop *)(&((_p)->u.plxys.next)));
        break;

      case _traced_call_bfunc_yy              :
        printf("_call_bfunc_yy!!\n");
        _p = ((yamop *)(&((_p)->u.plyys.next)));
        break;

      case _traced_p_equal                    :
        printf("_p_equal!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_p_dif                      :
        printf("_p_dif!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _traced_p_eq                       :
        printf("_p_eq!!\n");
        _p = ((yamop *)(&((_p)->u.l.next)));
        break;

      case _traced_p_arg_vv                   :
        printf("_p_arg_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _traced_p_arg_cv                   :
        printf("_p_arg_cv!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _traced_p_arg_y_vv                 :
        printf("_p_arg_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _traced_p_arg_y_cv                 :
        printf("_p_arg_y_cv!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _traced_p_func2s_vv                :
        printf("_p_func2s_vv!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _traced_p_func2s_cv                :
        printf("_p_func2s_cv!!\n");
        _p = ((yamop *)(&((_p)->u.xxc.next)));
        break;

      case _traced_p_func2s_vc                :
        printf("_p_func2s_vc!!\n");
        _p = ((yamop *)(&((_p)->u.xxn.next)));
        break;

      case _traced_p_func2s_y_vv              :
        printf("_p_func2s_y_vv!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _traced_p_func2s_y_cv              :
        printf("_p_func2s_y_cv!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _traced_p_func2s_y_vc              :
        printf("_p_func2s_y_vc!!\n");
        _p = ((yamop *)(&((_p)->u.yxn.next)));
        break;

      case _traced_p_func2f_xx                :
        printf("_p_func2f_xx!!\n");
        _p = ((yamop *)(&((_p)->u.xxx.next)));
        break;

      case _traced_p_func2f_xy                :
        printf("_p_func2f_xy!!\n");
        _p = ((yamop *)(&((_p)->u.xxy.next)));
        break;

      case _traced_p_func2f_yx                :
        printf("_p_func2f_yx!!\n");
        _p = ((yamop *)(&((_p)->u.yxx.next)));
        break;

      case _traced_p_func2f_yy                :
        printf("_p_func2f_yy!!\n");
        _p = ((yamop *)(&((_p)->u.yyx.next)));
        break;

      case _traced_p_functor                  :
        printf("_p_functor!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_p_execute2                 :
        printf("_p_execute2!!\n");
        _p = ((yamop *)(&((_p)->u.Osbpp.next)));
        break;

      case _traced_p_execute                  :
        printf("_p_execute!!\n");
        _p = ((yamop *)(&((_p)->u.Osbmp.next)));
        break;

      case _traced_p_execute_tail             :
        printf("_p_execute_tail!!\n");
        _p = ((yamop *)(&((_p)->u.Osbpp.next)));
        break;

#ifdef YAPOR
      case _traced_getwork_first_time         :
        printf("_getwork_first_time!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_getwork                    :
        printf("_getwork!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_getwork_seq                :
        printf("_getwork_seq!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_sync                       :
        printf("_sync!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

#endif
#ifdef TABLING
#ifdef TABLING_INNER_CUTS
      case _traced_clause_with_cut            :
        printf("_clause_with_cut!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

#endif
      case _traced_table_load_answer          :
        printf("_table_load_answer!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_table_try_answer           :
        printf("_table_try_answer!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_table_try_single           :
        printf("_table_try_single!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_table_try_me               :
        printf("_table_try_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_table_try                  :
        printf("_table_try!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_table_retry_me             :
        printf("_table_retry_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_table_retry                :
        printf("_table_retry!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_table_trust_me             :
        printf("_table_trust_me!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_table_trust                :
        printf("_table_trust!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_table_new_answer           :
        printf("_table_new_answer!!\n");
        _p = ((yamop *)(&((_p)->u.s.next)));
        break;

      case _traced_table_answer_resolution    :
        printf("_table_answer_resolution!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

      case _traced_table_completion           :
        printf("_table_completion!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

#ifdef THREADS_CONSUMER_SHARING
      case _traced_table_answer_resolution_completion:
        printf("_table_answer_resolution_completion!!\n");
        _p = ((yamop *)(&((_p)->u.Otapl.next)));
        break;

#endif
      case _traced_trie_do_var                :
        printf("_trie_do_var!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_var             :
        printf("_trie_trust_var!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_var               :
        printf("_trie_try_var!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_var             :
        printf("_trie_retry_var!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_do_var_in_pair        :
        printf("_trie_do_var_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_var_in_pair     :
        printf("_trie_trust_var_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_var_in_pair       :
        printf("_trie_try_var_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_var_in_pair     :
        printf("_trie_retry_var_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_do_val                :
        printf("_trie_do_val!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_val             :
        printf("_trie_trust_val!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_val               :
        printf("_trie_try_val!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_val             :
        printf("_trie_retry_val!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_do_val_in_pair        :
        printf("_trie_do_val_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_val_in_pair     :
        printf("_trie_trust_val_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_val_in_pair       :
        printf("_trie_try_val_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_val_in_pair     :
        printf("_trie_retry_val_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_do_atom               :
        printf("_trie_do_atom!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_atom            :
        printf("_trie_trust_atom!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_atom              :
        printf("_trie_try_atom!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_atom            :
        printf("_trie_retry_atom!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_do_atom_in_pair       :
        printf("_trie_do_atom_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_atom_in_pair    :
        printf("_trie_trust_atom_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_atom_in_pair      :
        printf("_trie_try_atom_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_atom_in_pair    :
        printf("_trie_retry_atom_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_do_null               :
        printf("_trie_do_null!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_null            :
        printf("_trie_trust_null!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_null              :
        printf("_trie_try_null!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_null            :
        printf("_trie_retry_null!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_do_null_in_pair       :
        printf("_trie_do_null_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_null_in_pair    :
        printf("_trie_trust_null_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_null_in_pair      :
        printf("_tri_try_null_in_paire!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_null_in_pair    :
        printf("_trie_retry_null_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_do_pair               :
        printf("_trie_do_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_pair            :
        printf("_trie_trust_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_pair              :
        printf("_trie_try_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_pair            :
        printf("_trie_retry_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_do_appl               :
        printf("_trie_do_appl!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_appl            :
        printf("_trie_trust_appl!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_appl              :
        printf("_trie_try_appl!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_appl            :
        printf("_trie_retry_appl!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_do_appl_in_pair       :
        printf("_trie_do_appl_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_appl_in_pair    :
        printf("_trie_trust_appl_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_appl_in_pair      :
        printf("_trie_trty_appkl_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_appl_in_pair    :
        printf("_trie_retry_appl_in_pair!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_do_extension          :
        printf("_trie_do_extension!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_extension       :
        printf("_trie_trust_extension!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_extension         :
        printf("_trie_try_extension!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_extension       :
        printf("_trie_retry_extension!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_do_double             :
        printf("_trie_do_double!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_double          :
        printf("_trie_trust_double!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_double            :
        printf("_trie_try_double!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_double          :
        printf("_trie_retry_double!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_do_longint            :
        printf("_trie_do_longint!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_longint         :
        printf("_trie_trust_longint!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_longint           :
        printf("_trie_try_longint!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_longint         :
        printf("_trie_retry_longint!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_do_gterm              :
        printf("_trie_do_gterm!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_trust_gterm           :
        printf("_trie_trust_gterm!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_try_gterm             :
        printf("_trie_try_gterm!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

      case _traced_trie_retry_gterm           :
        printf("_trie_retry_gterm!!\n");
        _p = ((yamop *)(&((_p)->u.e.next)));
        break;

#endif
  /* this instruction is hardwired */
#ifdef YAPOR
      case _traced_or_last                    :
        printf("_or_last!!\n");
        _p = ((yamop *)(&((_p)->u.sblp.next)));
        break;
#else
      case _traced_or_last                    :
        printf("_or_last!!\n");
        _p = ((yamop *)(&((_p)->u.p.next)));
        break;
#endif

#endif /* YAP_JIT */
    }
  }
  printf("\n");
}
