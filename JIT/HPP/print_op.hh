void print_op(char*, op_numbers, char*);

void
print_op(char* prepend_term, op_numbers op, char* append_term) {
    switch(op){
      case _Ystop                      :
        fprintf(stderr, "%s_Ystop%s", prepend_term, append_term);
        break;

      case _Nstop                      :
        fprintf(stderr, "%s_Nstop%s", prepend_term, append_term);
        break;

      case _try_me                     :
        fprintf(stderr, "%s_try_me%s", prepend_term, append_term);
        break;

      case _retry_me                   :
        fprintf(stderr, "%s_retry_me%s", prepend_term, append_term);
        break;

      case _trust_me                   :
        fprintf(stderr, "%s_trust_me%s", prepend_term, append_term);
        break;

      case _enter_profiling            :
        fprintf(stderr, "%s_enter_profiling%s", prepend_term, append_term);
        break;

      case _retry_profiled             :
        fprintf(stderr, "%s_retry_profiled%s", prepend_term, append_term);
        break;

      case _profiled_retry_me          :
        fprintf(stderr, "%s_profiled_retry_me%s", prepend_term, append_term);
        break;

      case _profiled_trust_me          :
        fprintf(stderr, "%s_profiled_trust_me%s", prepend_term, append_term);
        break;

      case _profiled_retry_logical     :
        fprintf(stderr, "%s_profiled_retry_logical%s", prepend_term, append_term);
        break;

      case _profiled_trust_logical     :
        fprintf(stderr, "%s_profiled_trust_logical%s", prepend_term, append_term);
        break;

      case _count_call                 :
        fprintf(stderr, "%s_count_call%s", prepend_term, append_term);
        break;

      case _count_retry                :
        fprintf(stderr, "%s_count_retry%s", prepend_term, append_term);
        break;

      case _count_retry_me             :
        fprintf(stderr, "%s_count_retry_me%s", prepend_term, append_term);
        break;

      case _count_trust_me             :
        fprintf(stderr, "%s_count_trust_me%s", prepend_term, append_term);
        break;

      case _count_retry_logical        :
        fprintf(stderr, "%s_count_retry_logical%s", prepend_term, append_term);
        break;

      case _count_trust_logical        :
        fprintf(stderr, "%s_count_trust_logical%s", prepend_term, append_term);
        break;

      case _lock_lu                    :
        fprintf(stderr, "%s_lock_lu%s", prepend_term, append_term);
        break;

      case _unlock_lu                  :
        fprintf(stderr, "%s_unlock_lu%s", prepend_term, append_term);
        break;

      case _alloc_for_logical_pred     :
        fprintf(stderr, "%s_alloc_for_logical_pred%s", prepend_term, append_term);
        break;

      case _copy_idb_term              :
        fprintf(stderr, "%s_copy_idb_term%s", prepend_term, append_term);
        break;

      case _unify_idb_term             :
        fprintf(stderr, "%s_unify_idb_term%s", prepend_term, append_term);
        break;

      case _ensure_space               :
        fprintf(stderr, "%s_ensure_space%s", prepend_term, append_term);
        break;

      case _spy_or_trymark             :
        fprintf(stderr, "%s_spy_or_trymark%s", prepend_term, append_term);
        break;

      case _try_and_mark               :
        fprintf(stderr, "%s_try_and_mark%s", prepend_term, append_term);
        break;

      case _count_retry_and_mark       :
        fprintf(stderr, "%s_count_retry_and_mark%s", prepend_term, append_term);
        break;

      case _profiled_retry_and_mark    :
        fprintf(stderr, "%s_profiled_retry_and_mark%s", prepend_term, append_term);
        break;

      case _retry_and_mark             :
        fprintf(stderr, "%s_retry_and_mark%s", prepend_term, append_term);
        break;

      case _trust_fail                 :
        fprintf(stderr, "%s_trust_fail%s", prepend_term, append_term);
        break;

      case _op_fail                    :
        fprintf(stderr, "%s_op_fail%s", prepend_term, append_term);
        break;

      case _cut                        :
        fprintf(stderr, "%s_cut%s", prepend_term, append_term);
        break;

      case _cut_t                      :
        fprintf(stderr, "%s_cut_t%s", prepend_term, append_term);
        break;

      case _cut_e                      :
        fprintf(stderr, "%s_cut_e%s", prepend_term, append_term);
        break;

      case _save_b_x                   :
        fprintf(stderr, "%s_save_b_x%s", prepend_term, append_term);
        break;

      case _save_b_y                   :
        fprintf(stderr, "%s_save_b_y%s", prepend_term, append_term);
        break;

      case _commit_b_x                 :
        fprintf(stderr, "%s_commit_b_x%s", prepend_term, append_term);
        break;

      case _commit_b_y                 :
        fprintf(stderr, "%s_commit_b_y%s", prepend_term, append_term);
        break;

      case _execute                    :
        fprintf(stderr, "%s_execute%s", prepend_term, append_term);
        break;

      case _dexecute                   :
        fprintf(stderr, "%s_dexecute%s", prepend_term, append_term);
        break;

      case _fcall                      :
        fprintf(stderr, "%s_fcall%s", prepend_term, append_term);
        break;

      case _call                       :
        fprintf(stderr, "%s_call%s", prepend_term, append_term);
        break;

      case _procceed                   :
        fprintf(stderr, "%s_procceed%s", prepend_term, append_term);
        break;

      case _allocate                   :
        fprintf(stderr, "%s_allocate%s", prepend_term, append_term);
        break;

      case _deallocate                 :
        fprintf(stderr, "%s_deallocate%s", prepend_term, append_term);
        break;

#ifdef BEAM
      case _retry_eam                  :
        fprintf(stderr, "%s_retry_eam%s", prepend_term, append_term);
        break;

#endif
#ifdef BEAM
      case _run_eam                    :
        fprintf(stderr, "%s_run_eam%s", prepend_term, append_term);
        break;

#endif
      case _get_x_var                  :
        fprintf(stderr, "%s_get_x_var%s", prepend_term, append_term);
        break;

      case _get_y_var                  :
        fprintf(stderr, "%s_get_y_var%s", prepend_term, append_term);
        break;

      case _get_yy_var                 :
        fprintf(stderr, "%s_get_yy_var%s", prepend_term, append_term);
        break;

      case _get_x_val                  :
        fprintf(stderr, "%s_get_x_val%s", prepend_term, append_term);
        break;

      case _get_y_val                  :
        fprintf(stderr, "%s_get_y_val%s", prepend_term, append_term);
        break;

      case _get_atom                   :
        fprintf(stderr, "%s_get_atom%s", prepend_term, append_term);
        break;

      case _get_2atoms                 :
        fprintf(stderr, "%s_get_2atoms%s", prepend_term, append_term);
        break;

      case _get_3atoms                 :
        fprintf(stderr, "%s_get_3atoms%s", prepend_term, append_term);
        break;

      case _get_4atoms                 :
        fprintf(stderr, "%s_get_4atoms%s", prepend_term, append_term);
        break;

      case _get_5atoms                 :
        fprintf(stderr, "%s_get_5atoms%s", prepend_term, append_term);
        break;

      case _get_6atoms                 :
        fprintf(stderr, "%s_get_6atoms%s", prepend_term, append_term);
        break;

      case _get_list                   :
        fprintf(stderr, "%s_get_list%s", prepend_term, append_term);
        break;

      case _get_struct                 :
        fprintf(stderr, "%s_get_struct%s", prepend_term, append_term);
        break;

      case _get_float                  :
        fprintf(stderr, "%s_get_float%s", prepend_term, append_term);
        break;

      case _get_longint                :
        fprintf(stderr, "%s_get_longint%s", prepend_term, append_term);
        break;

      case _get_bigint                 :
        fprintf(stderr, "%s_get_bigint%s", prepend_term, append_term);
        break;

      case _get_dbterm                 :
        fprintf(stderr, "%s_get_dbterm%s", prepend_term, append_term);
        break;

      case _glist_valx                 :
        fprintf(stderr, "%s_glist_valx%s", prepend_term, append_term);
        break;

      case _glist_valy                 :
        fprintf(stderr, "%s_glist_valy%s", prepend_term, append_term);
        break;

      case _gl_void_varx               :
        fprintf(stderr, "%s_gl_void_varx%s", prepend_term, append_term);
        break;

      case _gl_void_vary               :
        fprintf(stderr, "%s_gl_void_vary%s", prepend_term, append_term);
        break;

      case _gl_void_valx               :
        fprintf(stderr, "%s_gl_void_valx%s", prepend_term, append_term);
        break;

      case _gl_void_valy               :
        fprintf(stderr, "%s_gl_void_valy%s", prepend_term, append_term);
        break;

      case _unify_x_var                :
        fprintf(stderr, "%s_unify_x_var%s", prepend_term, append_term);
        break;

      case _unify_x_var_write          :
        fprintf(stderr, "%s_unify_x_var_write%s", prepend_term, append_term);
        break;

      case _unify_l_x_var              :
        fprintf(stderr, "%s_unify_l_x_var%s", prepend_term, append_term);
        break;

      case _unify_l_x_var_write        :
        fprintf(stderr, "%s_unify_l_x_var_write%s", prepend_term, append_term);
        break;

      case _unify_x_var2               :
        fprintf(stderr, "%s_unify_x_var2%s", prepend_term, append_term);
        break;

      case _unify_x_var2_write         :
        fprintf(stderr, "%s_unify_x_var2_write%s", prepend_term, append_term);
        break;

      case _unify_l_x_var2             :
        fprintf(stderr, "%s_unify_l_x_var2%s", prepend_term, append_term);
        break;

      case _unify_l_x_var2_write       :
        fprintf(stderr, "%s_unify_l_x_var2_write%s", prepend_term, append_term);
        break;

      case _unify_y_var                :
        fprintf(stderr, "%s_unify_y_var%s", prepend_term, append_term);
        break;

      case _unify_y_var_write          :
        fprintf(stderr, "%s_unify_y_var_write%s", prepend_term, append_term);
        break;

      case _unify_l_y_var              :
        fprintf(stderr, "%s_unify_l_y_var%s", prepend_term, append_term);
        break;

      case _unify_l_y_var_write        :
        fprintf(stderr, "%s_unify_l_y_var_write%s", prepend_term, append_term);
        break;

      case _unify_x_val                :
        fprintf(stderr, "%s_unify_x_val%s", prepend_term, append_term);
        break;

      case _unify_x_val_write          :
        fprintf(stderr, "%s_unify_x_val_write%s", prepend_term, append_term);
        break;

      case _unify_l_x_val              :
        fprintf(stderr, "%s_unify_l_x_val%s", prepend_term, append_term);
        break;

      case _unify_l_x_val_write        :
        fprintf(stderr, "%s_uify_l_x_val_write%s", prepend_term, append_term);
        break;

      case _unify_y_val                :
        fprintf(stderr, "%s_unify_y_val%s", prepend_term, append_term);
        break;

      case _unify_y_val_write          :
        fprintf(stderr, "%s_unify_y_val_write%s", prepend_term, append_term);
        break;

      case _unify_l_y_val              :
        fprintf(stderr, "%s_unify_l_y_val%s", prepend_term, append_term);
        break;

      case _unify_l_y_val_write        :
        fprintf(stderr, "%s_unify_l_y_val_write%s", prepend_term, append_term);
        break;

      case _unify_x_loc                :
        fprintf(stderr, "%s_unify_x_loc%s", prepend_term, append_term);
        break;

      case _unify_x_loc_write          :
        fprintf(stderr, "%s_unify_x_loc_write%s", prepend_term, append_term);
        break;

      case _unify_l_x_loc              :
        fprintf(stderr, "%s_unify_l_x_loc%s", prepend_term, append_term);
        break;

      case _unify_l_x_loc_write        :
        fprintf(stderr, "%s_unify_l_x_loc_write%s", prepend_term, append_term);
        break;

      case _unify_y_loc                :
        fprintf(stderr, "%s_unify_y_loc%s", prepend_term, append_term);
        break;

      case _unify_y_loc_write          :
        fprintf(stderr, "%s_unify_y_loc_write%s", prepend_term, append_term);
        break;

      case _unify_l_y_loc              :
        fprintf(stderr, "%s_unify_l_y_loc%s", prepend_term, append_term);
        break;

      case _unify_l_y_loc_write        :
        fprintf(stderr, "%s_unify_l_y_loc_write%s", prepend_term, append_term);
        break;

      case _unify_void                 :
        fprintf(stderr, "%s_unify_void%s", prepend_term, append_term);
        break;

      case _unify_void_write           :
        fprintf(stderr, "%s_unify_void_write%s", prepend_term, append_term);
        break;

      case _unify_l_void               :
        fprintf(stderr, "%s_unify_l_void%s", prepend_term, append_term);
        break;

      case _unify_l_void_write         :
        fprintf(stderr, "%s_unify_l_void_write%s", prepend_term, append_term);
        break;

      case _unify_n_voids              :
        fprintf(stderr, "%s_unify_n_voids%s", prepend_term, append_term);
        break;

      case _unify_n_voids_write        :
        fprintf(stderr, "%s_unify_n_voids_write%s", prepend_term, append_term);
        break;

      case _unify_l_n_voids            :
        fprintf(stderr, "%s_unify_l_n_voids%s", prepend_term, append_term);
        break;

      case _unify_l_n_voids_write      :
        fprintf(stderr, "%s_unify_l_n_voids_write%s", prepend_term, append_term);
        break;

      case _unify_atom                 :
        fprintf(stderr, "%s_unify_atom%s", prepend_term, append_term);
        break;

      case _unify_atom_write           :
        fprintf(stderr, "%s_unify_atom_write%s", prepend_term, append_term);
        break;

      case _unify_l_atom               :
        fprintf(stderr, "%s_unify_l_atom%s", prepend_term, append_term);
        break;

      case _unify_l_atom_write         :
        fprintf(stderr, "%s_unify_l_atom_write%s", prepend_term, append_term);
        break;

      case _unify_n_atoms              :
        fprintf(stderr, "%s_unify_n_atoms%s", prepend_term, append_term);
        break;

      case _unify_n_atoms_write        :
        fprintf(stderr, "%s_unify_n_atoms_write%s", prepend_term, append_term);
        break;

      case _unify_float                :
        fprintf(stderr, "%s_unify_float%s", prepend_term, append_term);
        break;

      case _unify_float_write          :
        fprintf(stderr, "%s_unify_float_write%s", prepend_term, append_term);
        break;

      case _unify_l_float              :
        fprintf(stderr, "%s_unify_l_float%s", prepend_term, append_term);
        break;

      case _unify_l_float_write        :
        fprintf(stderr, "%s_unify_l_float_write%s", prepend_term, append_term);
        break;

      case _unify_longint              :
        fprintf(stderr, "%s_unify_longint%s", prepend_term, append_term);
        break;

      case _unify_longint_write        :
        fprintf(stderr, "%s_unify_longint_write%s", prepend_term, append_term);
        break;

      case _unify_l_longint            :
        fprintf(stderr, "%s_unify_l_longint%s", prepend_term, append_term);
        break;

      case _unify_l_longint_write      :
        fprintf(stderr, "%s_unify_l_longint_write%s", prepend_term, append_term);
        break;

      case _unify_bigint               :
        fprintf(stderr, "%s_unify_bigint%s", prepend_term, append_term);
        break;

      case _unify_l_bigint             :
        fprintf(stderr, "%s_unify_l_bigint%s", prepend_term, append_term);
        break;

      case _unify_dbterm               :
        fprintf(stderr, "%s_unify_dbterm%s", prepend_term, append_term);
        break;

      case _unify_l_dbterm             :
        fprintf(stderr, "%s_unify_l_dbterm%s", prepend_term, append_term);
        break;

      case _unify_list                 :
        fprintf(stderr, "%s_unify_list%s", prepend_term, append_term);
        break;

      case _unify_list_write           :
        fprintf(stderr, "%s_unify_list_write%s", prepend_term, append_term);
        break;

      case _unify_l_list               :
        fprintf(stderr, "%s_unify_l_list%s", prepend_term, append_term);
        break;

      case _unify_l_list_write         :
        fprintf(stderr, "%s_unify_l_list_write%s", prepend_term, append_term);
        break;

      case _unify_struct               :
        fprintf(stderr, "%s_unify_struct%s", prepend_term, append_term);
        break;

      case _unify_struct_write         :
        fprintf(stderr, "%s_unify_struct_write%s", prepend_term, append_term);
        break;

      case _unify_l_struc              :
        fprintf(stderr, "%s_unify_l_struc%s", prepend_term, append_term);
        break;

      case _unify_l_struc_write        :
        fprintf(stderr, "%s_unify_l_struc_write%s", prepend_term, append_term);
        break;

      case _put_x_var                  :
        fprintf(stderr, "%s_put_x_var%s", prepend_term, append_term);
        break;

      case _put_y_var                  :
        fprintf(stderr, "%s_put_y_var%s", prepend_term, append_term);
        break;

      case _put_x_val                  :
        fprintf(stderr, "%s_put_x_val%s", prepend_term, append_term);
        break;

      case _put_xx_val                 :
        fprintf(stderr, "%s_put_xx_val%s", prepend_term, append_term);
        break;

      case _put_y_val                  :
        fprintf(stderr, "%s_put_y_val%s", prepend_term, append_term);
        break;

      case _put_y_vals                 :
        fprintf(stderr, "%s_put_y_vals%s", prepend_term, append_term);
        break;

      case _put_unsafe                 :
        fprintf(stderr, "%s_put_unsafe%s", prepend_term, append_term);
        break;

      case _put_atom                   :
        fprintf(stderr, "%s_put_atom%s", prepend_term, append_term);
        break;

      case _put_dbterm                 :
        fprintf(stderr, "%s_put_dbterm%s", prepend_term, append_term);
        break;

      case _put_bigint                 :
        fprintf(stderr, "%s_put_bigint%s", prepend_term, append_term);
        break;

      case _put_float                  :
        fprintf(stderr, "%s_put_float%s", prepend_term, append_term);
        break;

      case _put_longint                :
        fprintf(stderr, "%s_put_longint%s", prepend_term, append_term);
        break;

      case _put_list                   :
        fprintf(stderr, "%s_put_list%s", prepend_term, append_term);
        break;

      case _put_struct                 :
        fprintf(stderr, "%s_put_struct%s", prepend_term, append_term);
        break;

      case _write_x_var                :
        fprintf(stderr, "%s_write_x_var%s", prepend_term, append_term);
        break;

      case _write_void                 :
        fprintf(stderr, "%s_write_void%s", prepend_term, append_term);
        break;

      case _write_n_voids              :
        fprintf(stderr, "%s_write_n_voids%s", prepend_term, append_term);
        break;

      case _write_y_var                :
        fprintf(stderr, "%s_write_y_var%s", prepend_term, append_term);
        break;

      case _write_x_val                :
        fprintf(stderr, "%s_write_x_val%s", prepend_term, append_term);
        break;

      case _write_x_loc                :
        fprintf(stderr, "%s_write_x_loc%s", prepend_term, append_term);
        break;

      case _write_y_val                :
        fprintf(stderr, "%s_write_y_val%s", prepend_term, append_term);
        break;

      case _write_y_loc                :
        fprintf(stderr, "%s_write_y_loc%s", prepend_term, append_term);
        break;

      case _write_atom                 :
        fprintf(stderr, "%s_write_atom%s", prepend_term, append_term);
        break;

      case _write_bigint               :
        fprintf(stderr, "%s_write_bigint%s", prepend_term, append_term);
        break;

      case _write_dbterm               :
        fprintf(stderr, "%s_write_dbterm%s", prepend_term, append_term);
        break;

      case _write_float                :
        fprintf(stderr, "%s_write_float%s", prepend_term, append_term);
        break;

      case _write_longint              :
        fprintf(stderr, "%s_write_longint%s", prepend_term, append_term);
        break;

      case _write_n_atoms              :
        fprintf(stderr, "%s_write_n_atoms%s", prepend_term, append_term);
        break;

      case _write_list                 :
        fprintf(stderr, "%s_write_list%s", prepend_term, append_term);
        break;

      case _write_l_list               :
        fprintf(stderr, "%s_write_l_list%s", prepend_term, append_term);
        break;

      case _write_struct               :
        fprintf(stderr, "%s_write_struct%s", prepend_term, append_term);
        break;

      case _write_l_struc              :
        fprintf(stderr, "%s_write_l_struc%s", prepend_term, append_term);
        break;

      case _save_pair_x                :
        fprintf(stderr, "%s_save_pair_x%s", prepend_term, append_term);
        break;

      case _save_pair_x_write          :
        fprintf(stderr, "%s_save_pair_x_write%s", prepend_term, append_term);
        break;

      case _save_pair_y                :
        fprintf(stderr, "%s_save_pair_y%s", prepend_term, append_term);
        break;

      case _save_pair_y_write          :
        fprintf(stderr, "%s_save_pair_y_write%s", prepend_term, append_term);
        break;

      case _save_appl_x                :
        fprintf(stderr, "%s_save_appl_x%s", prepend_term, append_term);
        break;

      case _save_appl_x_write          :
        fprintf(stderr, "%s_save_appl_x_write%s", prepend_term, append_term);
        break;

      case _save_appl_y                :
        fprintf(stderr, "%s_save_appl_y%s", prepend_term, append_term);
        break;

      case _save_appl_y_write          :
        fprintf(stderr, "%s_save_appl_y_write%s", prepend_term, append_term);
        break;

      case _jump                       :
        fprintf(stderr, "%s_jump%s", prepend_term, append_term);
        break;

      case _move_back                  :
        fprintf(stderr, "%s_move_back%s", prepend_term, append_term);
        break;

      case _skip                       :
        fprintf(stderr, "%s_skip%s", prepend_term, append_term);
        break;

      case _either                     :
        fprintf(stderr, "%s_either%s", prepend_term, append_term);
        break;

      case _or_else                    :
        fprintf(stderr, "%s_or_else%s", prepend_term, append_term);
        break;

      case _pop_n                      :
        fprintf(stderr, "%s_pop_n%s", prepend_term, append_term);
        break;

      case _pop                        :
        fprintf(stderr, "%s_pop%s", prepend_term, append_term);
        break;

      case _call_cpred                 :
        fprintf(stderr, "%s_call_cpred%s", prepend_term, append_term);
        break;

      case _execute_cpred              :
        fprintf(stderr, "%s_execute_cpred%s", prepend_term, append_term);
        break;

      case _call_usercpred             :
        fprintf(stderr, "%s_call_usercpred%s", prepend_term, append_term);
        break;

      case _call_c_wfail               :
        fprintf(stderr, "%s_call_x_wfail%s", prepend_term, append_term);
        break;

      case _try_c                      :
        fprintf(stderr, "%s_try_c%s", prepend_term, append_term);
        break;

      case _retry_c                    :
        fprintf(stderr, "%s_retry_c%s", prepend_term, append_term);
        break;

#ifdef CUT_C
      case _cut_c                      :
        fprintf(stderr, "%s_cut_c%s", prepend_term, append_term);
        break;

#endif
      case _try_userc                  :
        fprintf(stderr, "%s_try_userc%s", prepend_term, append_term);
        break;

      case _retry_userc                :
        fprintf(stderr, "%s_retry_userc%s", prepend_term, append_term);
        break;

#ifdef CUT_C
      case _cut_userc                  :
        fprintf(stderr, "%s_cut_userc%s", prepend_term, append_term);
        break;

#endif
      case _lock_pred                  :
        fprintf(stderr, "%s_lock_pred%s", prepend_term, append_term);
        break;

      case _index_pred                 :
        fprintf(stderr, "%s_index_pred%s", prepend_term, append_term);
        break;

#ifdef THREADS
      case _thread_local               :
        fprintf(stderr, "%s_thread_local%s", prepend_term, append_term);
        break;

#endif
      case _expand_index               :
        fprintf(stderr, "%s_expand_index%s", prepend_term, append_term);
        break;

      case _expand_clauses             :
        fprintf(stderr, "%s_expand_clauses%s", prepend_term, append_term);
        break;

      case _undef_p                    :
        fprintf(stderr, "%s_undef_p%s", prepend_term, append_term);
        break;

      case _spy_pred                   :
        fprintf(stderr, "%s_spy_pred%s", prepend_term, append_term);
        break;

      case _try_clause                 :
        fprintf(stderr, "%s_try_clause%s", prepend_term, append_term);
        break;

      case _try_clause2                :
        fprintf(stderr, "%s_try_clause2%s", prepend_term, append_term);
        break;

      case _try_clause3                :
        fprintf(stderr, "%s_try_clause3%s", prepend_term, append_term);
        break;

      case _try_clause4                :
        fprintf(stderr, "%s_try_clause4%s", prepend_term, append_term);
        break;

      case _retry                      :
        fprintf(stderr, "%s_retry%s", prepend_term, append_term);
        break;

      case _retry2                     :
        fprintf(stderr, "%s_retry2%s", prepend_term, append_term);
        break;

      case _retry3                     :
        fprintf(stderr, "%s_retry3%s", prepend_term, append_term);
        break;

      case _retry4                     :
        fprintf(stderr, "%s_retry4%s", prepend_term, append_term);
        break;

      case _trust                      :
        fprintf(stderr, "%s_trust%s", prepend_term, append_term);
        break;

      case _try_in                     :
        fprintf(stderr, "%s_try_in%s", prepend_term, append_term);
        break;

      case _enter_lu_pred              :
        fprintf(stderr, "%s_enter_lu_pred%s", prepend_term, append_term);
        break;

      case _try_logical                :
        fprintf(stderr, "%s_try_logical%s", prepend_term, append_term);
        break;

      case _retry_logical              :
        fprintf(stderr, "%s_retry_logical%s", prepend_term, append_term);
        break;

      case _trust_logical              :
        fprintf(stderr, "%s_trust_logical%s", prepend_term, append_term);
        break;

      case _user_switch                :
        fprintf(stderr, "%s_user_switch%s", prepend_term, append_term);
        break;

      case _switch_on_type             :
        fprintf(stderr, "%s_switch_on_type%s", prepend_term, append_term);
        break;

      case _switch_list_nl             :
        fprintf(stderr, "%s_switch_list_nl%s", prepend_term, append_term);
        break;

      case _switch_on_arg_type         :
        fprintf(stderr, "%s_switch_on_arg_type%s", prepend_term, append_term);
        break;

      case _switch_on_sub_arg_type     :
        fprintf(stderr, "%s_switch_on_sub_arg_type%s", prepend_term, append_term);
        break;

      case _jump_if_var                :
        fprintf(stderr, "%s_jump_if_var%s", prepend_term, append_term);
        break;

      case _jump_if_nonvar             :
        fprintf(stderr, "%s_jump_if_nonvar%s", prepend_term, append_term);
        break;

      case _if_not_then                :
        fprintf(stderr, "%s_if_not_then%s", prepend_term, append_term);
        break;

      case _switch_on_func             :
        fprintf(stderr, "%s_switch_on_func%s", prepend_term, append_term);
        break;

      case _switch_on_cons             :
        fprintf(stderr, "%s_switch_on_cons%s", prepend_term, append_term);
        break;

      case _go_on_func                 :
        fprintf(stderr, "%s_go_on_func%s", prepend_term, append_term);
        break;

      case _go_on_cons                 :
        fprintf(stderr, "%s_go_on_cons%s", prepend_term, append_term);
        break;

      case _if_func                    :
        fprintf(stderr, "%s_if_func%s", prepend_term, append_term);
        break;

      case _if_cons                    :
        fprintf(stderr, "%s_if_cons%s", prepend_term, append_term);
        break;

      case _index_dbref                :
        fprintf(stderr, "%s_index_dbref%s", prepend_term, append_term);
        break;

      case _index_blob                 :
        fprintf(stderr, "%s_index_blob%s", prepend_term, append_term);
        break;

      case _index_long                 :
        fprintf(stderr, "%s_index_long%s", prepend_term, append_term);
        break;

#if YAP_JIT
      case _jit_handler                  :
        fprintf(stderr, "%s_jit_handler%s", prepend_term, append_term);
        break;
#endif

      case _p_atom_x                   :
        fprintf(stderr, "%s_p_atom_x%s", prepend_term, append_term);
        break;

      case _p_atom_y                   :
        fprintf(stderr, "%s_p_atom_y%s", prepend_term, append_term);
        break;

      case _p_atomic_x                 :
        fprintf(stderr, "%s_p_atomic_x%s", prepend_term, append_term);
        break;

      case _p_atomic_y                 :
        fprintf(stderr, "%s_p_atomic_y%s", prepend_term, append_term);
        break;

      case _p_integer_x                :
        fprintf(stderr, "%s_p_integer_x%s", prepend_term, append_term);
        break;

      case _p_integer_y                :
        fprintf(stderr, "%s_p_integer_y%s", prepend_term, append_term);
        break;

      case _p_nonvar_x                 :
        fprintf(stderr, "%s_p_nonvar_x%s", prepend_term, append_term);
        break;

      case _p_nonvar_y                 :
        fprintf(stderr, "%s_p_nonvar_y%s", prepend_term, append_term);
        break;

      case _p_number_x                 :
        fprintf(stderr, "%s_p_number_x%s", prepend_term, append_term);
        break;

      case _p_number_y                 :
        fprintf(stderr, "%s_p_number_y%s", prepend_term, append_term);
        break;

      case _p_var_x                    :
        fprintf(stderr, "%s_p_var_x%s", prepend_term, append_term);
        break;

      case _p_var_y                    :
        fprintf(stderr, "%s_p_var_y%s", prepend_term, append_term);
        break;

      case _p_db_ref_x                 :
        fprintf(stderr, "%s_p_db_ref_x%s", prepend_term, append_term);
        break;

      case _p_db_ref_y                 :
        fprintf(stderr, "%s_p_db_ref_y%s", prepend_term, append_term);
        break;

      case _p_primitive_x              :
        fprintf(stderr, "%s_p_primitive_x%s", prepend_term, append_term);
        break;

      case _p_primitive_y              :
        fprintf(stderr, "%s_p_primitive_y%s", prepend_term, append_term);
        break;

      case _p_compound_x               :
        fprintf(stderr, "%s_p_compound_x%s", prepend_term, append_term);
        break;

      case _p_compound_y               :
        fprintf(stderr, "%s_p_compound_y%s", prepend_term, append_term);
        break;

      case _p_float_x                  :
        fprintf(stderr, "%s_p_float_x%s", prepend_term, append_term);
        break;

      case _p_float_y                  :
        fprintf(stderr, "%s_p_float_y%s", prepend_term, append_term);
        break;

      case _p_plus_vv                  :
        fprintf(stderr, "%s_p_plus_vv%s", prepend_term, append_term);
        break;

      case _p_plus_vc                  :
        fprintf(stderr, "%s_p_plus_vc%s", prepend_term, append_term);
        break;

      case _p_plus_y_vv                :
        fprintf(stderr, "%s_p_plus_y_vv%s", prepend_term, append_term);
        break;

      case _p_plus_y_vc                :
        fprintf(stderr, "%s_p_plus_y_vc%s", prepend_term, append_term);
        break;

      case _p_minus_vv                 :
        fprintf(stderr, "%s_p_minus_vv%s", prepend_term, append_term);
        break;

      case _p_minus_cv                 :
        fprintf(stderr, "%s_p_minus_cv%s", prepend_term, append_term);
        break;

      case _p_minus_y_vv               :
        fprintf(stderr, "%s_p_minus_y_vv%s", prepend_term, append_term);
        break;

      case _p_minus_y_cv               :
        fprintf(stderr, "%s_p_minus_y_cv%s", prepend_term, append_term);
        break;

      case _p_times_vv                 :
        fprintf(stderr, "%s_p_times_vv%s", prepend_term, append_term);
        break;

      case _p_times_vc                 :
        fprintf(stderr, "%s_p_times_vc%s", prepend_term, append_term);
        break;

      case _p_times_y_vv               :
        fprintf(stderr, "%s_p_times_y_vv%s", prepend_term, append_term);
        break;

      case _p_times_y_vc               :
        fprintf(stderr, "%s_p_times_y_vc%s", prepend_term, append_term);
        break;

      case _p_div_vv                   :
        fprintf(stderr, "%s_p_div_vv%s", prepend_term, append_term);
        break;

      case _p_div_vc                   :
        fprintf(stderr, "%s_p_div_vc%s", prepend_term, append_term);
        break;

      case _p_div_cv                   :
        fprintf(stderr, "%s_p_div_cv%s", prepend_term, append_term);
        break;

      case _p_div_y_vv                 :
        fprintf(stderr, "%s_p_div_y_vv%s", prepend_term, append_term);
        break;

      case _p_div_y_vc                 :
        fprintf(stderr, "%s_p_div_y_vc%s", prepend_term, append_term);
        break;

      case _p_div_y_cv                 :
        fprintf(stderr, "%s_p_div_y_cv%s", prepend_term, append_term);
        break;

      case _p_and_vv                   :
        fprintf(stderr, "%s_p_and_vv%s", prepend_term, append_term);
        break;

      case _p_and_vc                   :
        fprintf(stderr, "%s_p_and_vc%s", prepend_term, append_term);
        break;

      case _p_and_y_vv                 :
        fprintf(stderr, "%s_p_and_y_vv%s", prepend_term, append_term);
        break;

      case _p_and_y_vc                 :
        fprintf(stderr, "%s_p_and_y_vc%s", prepend_term, append_term);
        break;

      case _p_or_vv                    :
        fprintf(stderr, "%s_p_or_vv%s", prepend_term, append_term);
        break;

      case _p_or_vc                    :
        fprintf(stderr, "%s_p_or_vc%s", prepend_term, append_term);
        break;

      case _p_or_y_vv                  :
        fprintf(stderr, "%s_p_or_y_vv%s", prepend_term, append_term);
        break;

      case _p_or_y_vc                  :
        fprintf(stderr, "%s_p_or_y_vc%s", prepend_term, append_term);
        break;

      case _p_sll_vv                   :
        fprintf(stderr, "%s_p_sll_vv%s", prepend_term, append_term);
        break;

      case _p_sll_vc                   :
        fprintf(stderr, "%s_p_sll_vc%s", prepend_term, append_term);
        break;

      case _p_sll_cv                   :
        fprintf(stderr, "%s_p_sll_cv%s", prepend_term, append_term);
        break;

      case _p_sll_y_vv                 :
        fprintf(stderr, "%s_p_sll_y_vv%s", prepend_term, append_term);
        break;

      case _p_sll_y_vc                 :
        fprintf(stderr, "%s_p_sll_y_vc%s", prepend_term, append_term);
        break;

      case _p_sll_y_cv                 :
        fprintf(stderr, "%s_p_sll_y_cv%s", prepend_term, append_term);
        break;

      case _p_slr_vv                   :
        fprintf(stderr, "%s_p_slr_vv%s", prepend_term, append_term);
        break;

      case _p_slr_vc                   :
        fprintf(stderr, "%s_p_slr_vc%s", prepend_term, append_term);
        break;

      case _p_slr_cv                   :
        fprintf(stderr, "%s_p_slr_cv%s", prepend_term, append_term);
        break;

      case _p_slr_y_vv                 :
        fprintf(stderr, "%s_p_slr_y_vv%s", prepend_term, append_term);
        break;

      case _p_slr_y_vc                 :
        fprintf(stderr, "%s_p_slr_y_vc%s", prepend_term, append_term);
        break;

      case _p_slr_y_cv                 :
        fprintf(stderr, "%s_p_slr_y_cv%s", prepend_term, append_term);
        break;

      case _call_bfunc_xx              :
        fprintf(stderr, "%s_call_bfunc_xx%s", prepend_term, append_term);
        break;

      case _call_bfunc_yx              :
        fprintf(stderr, "%s_call_bfunc_yx%s", prepend_term, append_term);
        break;

      case _call_bfunc_xy              :
        fprintf(stderr, "%s_call_bfunc_xy%s", prepend_term, append_term);
        break;

      case _call_bfunc_yy              :
        fprintf(stderr, "%s_call_bfunc_yy%s", prepend_term, append_term);
        break;

      case _p_equal                    :
        fprintf(stderr, "%s_p_equal%s", prepend_term, append_term);
        break;

      case _p_dif                      :
        fprintf(stderr, "%s_p_dif%s", prepend_term, append_term);
        break;

      case _p_eq                       :
        fprintf(stderr, "%s_p_eq%s", prepend_term, append_term);
        break;

      case _p_arg_vv                   :
        fprintf(stderr, "%s_p_arg_vv%s", prepend_term, append_term);
        break;

      case _p_arg_cv                   :
        fprintf(stderr, "%s_p_arg_cv%s", prepend_term, append_term);
        break;

      case _p_arg_y_vv                 :
        fprintf(stderr, "%s_p_arg_y_vv%s", prepend_term, append_term);
        break;

      case _p_arg_y_cv                 :
        fprintf(stderr, "%s_p_arg_y_cv%s", prepend_term, append_term);
        break;

      case _p_func2s_vv                :
        fprintf(stderr, "%s_p_func2s_vv%s", prepend_term, append_term);
        break;

      case _p_func2s_cv                :
        fprintf(stderr, "%s_p_func2s_cv%s", prepend_term, append_term);
        break;

      case _p_func2s_vc                :
        fprintf(stderr, "%s_p_func2s_vc%s", prepend_term, append_term);
        break;

      case _p_func2s_y_vv              :
        fprintf(stderr, "%s_p_func2s_y_vv%s", prepend_term, append_term);
        break;

      case _p_func2s_y_cv              :
        fprintf(stderr, "%s_p_func2s_y_cv%s", prepend_term, append_term);
        break;

      case _p_func2s_y_vc              :
        fprintf(stderr, "%s_p_func2s_y_vc%s", prepend_term, append_term);
        break;

      case _p_func2f_xx                :
        fprintf(stderr, "%s_p_func2f_xx%s", prepend_term, append_term);
        break;

      case _p_func2f_xy                :
        fprintf(stderr, "%s_p_func2f_xy%s", prepend_term, append_term);
        break;

      case _p_func2f_yx                :
        fprintf(stderr, "%s_p_func2f_yx%s", prepend_term, append_term);
        break;

      case _p_func2f_yy                :
        fprintf(stderr, "%s_p_func2f_yy%s", prepend_term, append_term);
        break;

      case _p_functor                  :
        fprintf(stderr, "%s_p_functor%s", prepend_term, append_term);
        break;

      case _p_execute2                 :
        fprintf(stderr, "%s_p_execute2%s", prepend_term, append_term);
        break;

      case _p_execute                  :
        fprintf(stderr, "%s_p_execute%s", prepend_term, append_term);
        break;

      case _p_execute_tail             :
        fprintf(stderr, "%s_p_execute_tail%s", prepend_term, append_term);
        break;

#ifdef YAPOR
      case _getwork_first_time         :
        fprintf(stderr, "%s_getwork_first_time%s", prepend_term, append_term);
        break;

      case _getwork                    :
        fprintf(stderr, "%s_getwork%s", prepend_term, append_term);
        break;

      case _getwork_seq                :
        fprintf(stderr, "%s_getwork_seq%s", prepend_term, append_term);
        break;

      case _sync                       :
        fprintf(stderr, "%s_sync%s", prepend_term, append_term);
        break;

#endif
#ifdef TABLING
#ifdef TABLING_INNER_CUTS
      case _clause_with_cut            :
        fprintf(stderr, "%s_clause_with_cut%s", prepend_term, append_term);
        break;

#endif
      case _table_load_answer          :
        fprintf(stderr, "%s_table_load_answer%s", prepend_term, append_term);
        break;

      case _table_try_answer           :
        fprintf(stderr, "%s_table_try_answer%s", prepend_term, append_term);
        break;

      case _table_try_single           :
        fprintf(stderr, "%s_table_try_single%s", prepend_term, append_term);
        break;

      case _table_try_me               :
        fprintf(stderr, "%s_table_try_me%s", prepend_term, append_term);
        break;

      case _table_try                  :
        fprintf(stderr, "%s_table_try%s", prepend_term, append_term);
        break;

      case _table_retry_me             :
        fprintf(stderr, "%s_table_retry_me%s", prepend_term, append_term);
        break;

      case _table_retry                :
        fprintf(stderr, "%s_table_retry%s", prepend_term, append_term);
        break;

      case _table_trust_me             :
        fprintf(stderr, "%s_table_trust_me%s", prepend_term, append_term);
        break;

      case _table_trust                :
        fprintf(stderr, "%s_table_trust%s", prepend_term, append_term);
        break;

      case _table_new_answer           :
        fprintf(stderr, "%s_table_new_answer%s", prepend_term, append_term);
        break;

      case _table_answer_resolution    :
        fprintf(stderr, "%s_table_answer_resolution%s", prepend_term, append_term);
        break;

      case _table_completion           :
        fprintf(stderr, "%s_table_completion%s", prepend_term, append_term);
        break;

#ifdef THREADS_CONSUMER_SHARING
      case _table_answer_resolution_completion:
        fprintf(stderr, "%s_table_answer_resolution_completion%s", prepend_term, append_term);
        break;

#endif
      case _trie_do_var                :
        fprintf(stderr, "%s_trie_do_var%s", prepend_term, append_term);
        break;

      case _trie_trust_var             :
        fprintf(stderr, "%s_trie_trust_var%s", prepend_term, append_term);
        break;

      case _trie_try_var               :
        fprintf(stderr, "%s_trie_try_var%s", prepend_term, append_term);
        break;

      case _trie_retry_var             :
        fprintf(stderr, "%s_trie_retry_var%s", prepend_term, append_term);
        break;

      case _trie_do_var_in_pair        :
        fprintf(stderr, "%s_trie_do_var_in_pair%s", prepend_term, append_term);
        break;

      case _trie_trust_var_in_pair     :
        fprintf(stderr, "%s_trie_trust_var_in_pair%s", prepend_term, append_term);
        break;

      case _trie_try_var_in_pair       :
        fprintf(stderr, "%s_trie_try_var_in_pair%s", prepend_term, append_term);
        break;

      case _trie_retry_var_in_pair     :
        fprintf(stderr, "%s_trie_retry_var_in_pair%s", prepend_term, append_term);
        break;

      case _trie_do_val                :
        fprintf(stderr, "%s_trie_do_val%s", prepend_term, append_term);
        break;

      case _trie_trust_val             :
        fprintf(stderr, "%s_trie_trust_val%s", prepend_term, append_term);
        break;

      case _trie_try_val               :
        fprintf(stderr, "%s_trie_try_val%s", prepend_term, append_term);
        break;

      case _trie_retry_val             :
        fprintf(stderr, "%s_trie_retry_val%s", prepend_term, append_term);
        break;

      case _trie_do_val_in_pair        :
        fprintf(stderr, "%s_trie_do_val_in_pair%s", prepend_term, append_term);
        break;

      case _trie_trust_val_in_pair     :
        fprintf(stderr, "%s_trie_trust_val_in_pair%s", prepend_term, append_term);
        break;

      case _trie_try_val_in_pair       :
        fprintf(stderr, "%s_trie_try_val_in_pair%s", prepend_term, append_term);
        break;

      case _trie_retry_val_in_pair     :
        fprintf(stderr, "%s_trie_retry_val_in_pair%s", prepend_term, append_term);
        break;

      case _trie_do_atom               :
        fprintf(stderr, "%s_trie_do_atom%s", prepend_term, append_term);
        break;

      case _trie_trust_atom            :
        fprintf(stderr, "%s_trie_trust_atom%s", prepend_term, append_term);
        break;

      case _trie_try_atom              :
        fprintf(stderr, "%s_trie_try_atom%s", prepend_term, append_term);
        break;

      case _trie_retry_atom            :
        fprintf(stderr, "%s_trie_retry_atom%s", prepend_term, append_term);
        break;

      case _trie_do_atom_in_pair       :
        fprintf(stderr, "%s_trie_do_atom_in_pair%s", prepend_term, append_term);
        break;

      case _trie_trust_atom_in_pair    :
        fprintf(stderr, "%s_trie_trust_atom_in_pair%s", prepend_term, append_term);
        break;

      case _trie_try_atom_in_pair      :
        fprintf(stderr, "%s_trie_try_atom_in_pair%s", prepend_term, append_term);
        break;

      case _trie_retry_atom_in_pair    :
        fprintf(stderr, "%s_trie_retry_atom_in_pair%s", prepend_term, append_term);
        break;

      case _trie_do_null               :
        fprintf(stderr, "%s_trie_do_null%s", prepend_term, append_term);
        break;

      case _trie_trust_null            :
        fprintf(stderr, "%s_trie_trust_null%s", prepend_term, append_term);
        break;

      case _trie_try_null              :
        fprintf(stderr, "%s_trie_try_null%s", prepend_term, append_term);
        break;

      case _trie_retry_null            :
        fprintf(stderr, "%s_trie_retry_null%s", prepend_term, append_term);
        break;

      case _trie_do_null_in_pair       :
        fprintf(stderr, "%s_trie_do_null_in_pair%s", prepend_term, append_term);
        break;

      case _trie_trust_null_in_pair    :
        fprintf(stderr, "%s_trie_trust_null_in_pair%s", prepend_term, append_term);
        break;

      case _trie_try_null_in_pair      :
        fprintf(stderr, "%s_tri_try_null_in_paire%s", prepend_term, append_term);
        break;

      case _trie_retry_null_in_pair    :
        fprintf(stderr, "%s_trie_retry_null_in_pair%s", prepend_term, append_term);
        break;

      case _trie_do_pair               :
        fprintf(stderr, "%s_trie_do_pair%s", prepend_term, append_term);
        break;

      case _trie_trust_pair            :
        fprintf(stderr, "%s_trie_trust_pair%s", prepend_term, append_term);
        break;

      case _trie_try_pair              :
        fprintf(stderr, "%s_trie_try_pair%s", prepend_term, append_term);
        break;

      case _trie_retry_pair            :
        fprintf(stderr, "%s_trie_retry_pair%s", prepend_term, append_term);
        break;

      case _trie_do_appl               :
        fprintf(stderr, "%s_trie_do_appl%s", prepend_term, append_term);
        break;

      case _trie_trust_appl            :
        fprintf(stderr, "%s_trie_trust_appl%s", prepend_term, append_term);
        break;

      case _trie_try_appl              :
        fprintf(stderr, "%s_trie_try_appl%s", prepend_term, append_term);
        break;

      case _trie_retry_appl            :
        fprintf(stderr, "%s_trie_retry_appl%s", prepend_term, append_term);
        break;

      case _trie_do_appl_in_pair       :
        fprintf(stderr, "%s_trie_do_appl_in_pair%s", prepend_term, append_term);
        break;

      case _trie_trust_appl_in_pair    :
        fprintf(stderr, "%s_trie_trust_appl_in_pair%s", prepend_term, append_term);
        break;

      case _trie_try_appl_in_pair      :
        fprintf(stderr, "%s_trie_trty_appkl_in_pair%s", prepend_term, append_term);
        break;

      case _trie_retry_appl_in_pair    :
        fprintf(stderr, "%s_trie_retry_appl_in_pair%s", prepend_term, append_term);
        break;

      case _trie_do_extension          :
        fprintf(stderr, "%s_trie_do_extension%s", prepend_term, append_term);
        break;

      case _trie_trust_extension       :
        fprintf(stderr, "%s_trie_trust_extension%s", prepend_term, append_term);
        break;

      case _trie_try_extension         :
        fprintf(stderr, "%s_trie_try_extension%s", prepend_term, append_term);
        break;

      case _trie_retry_extension       :
        fprintf(stderr, "%s_trie_retry_extension%s", prepend_term, append_term);
        break;

      case _trie_do_double             :
        fprintf(stderr, "%s_trie_do_double%s", prepend_term, append_term);
        break;

      case _trie_trust_double          :
        fprintf(stderr, "%s_trie_trust_double%s", prepend_term, append_term);
        break;

      case _trie_try_double            :
        fprintf(stderr, "%s_trie_try_double%s", prepend_term, append_term);
        break;

      case _trie_retry_double          :
        fprintf(stderr, "%s_trie_retry_double%s", prepend_term, append_term);
        break;

      case _trie_do_longint            :
        fprintf(stderr, "%s_trie_do_longint%s", prepend_term, append_term);
        break;

      case _trie_trust_longint         :
        fprintf(stderr, "%s_trie_trust_longint%s", prepend_term, append_term);
        break;

      case _trie_try_longint           :
        fprintf(stderr, "%s_trie_try_longint%s", prepend_term, append_term);
        break;

      case _trie_retry_longint         :
        fprintf(stderr, "%s_trie_retry_longint%s", prepend_term, append_term);
        break;

      case _trie_do_gterm              :
        fprintf(stderr, "%s_trie_do_gterm%s", prepend_term, append_term);
        break;

      case _trie_trust_gterm           :
        fprintf(stderr, "%s_trie_trust_gterm%s", prepend_term, append_term);
        break;

      case _trie_try_gterm             :
        fprintf(stderr, "%s_trie_try_gterm%s", prepend_term, append_term);
        break;

      case _trie_retry_gterm           :
        fprintf(stderr, "%s_trie_retry_gterm%s", prepend_term, append_term);
        break;

#endif
  /* this instruction is hardwired */
#ifdef YAPOR
      case _or_last                    :
        fprintf(stderr, "%s_or_last%s", prepend_term, append_term);
        break;

#else
      case _or_last                    :
        fprintf(stderr, "%s_or_last%s", prepend_term, append_term);
        break;

#endif
#if YAP_JIT
      case _traced_Ystop                      :
        fprintf(stderr, "%s_traced_Ystop%s", prepend_term, append_term);
        break;

      case _traced_Nstop                      :
        fprintf(stderr, "%s_traced_Nstop%s", prepend_term, append_term);
        break;

      case _traced_try_me                     :
        fprintf(stderr, "%s_traced_try_me%s", prepend_term, append_term);
        break;

      case _traced_retry_me                   :
        fprintf(stderr, "%s_traced_retry_me%s", prepend_term, append_term);
        break;

      case _traced_trust_me                   :
        fprintf(stderr, "%s_traced_trust_me%s", prepend_term, append_term);
        break;

      case _traced_enter_profiling            :
        fprintf(stderr, "%s_traced_enter_profiling%s", prepend_term, append_term);
        break;

      case _traced_retry_profiled             :
        fprintf(stderr, "%s_traced_retry_profiled%s", prepend_term, append_term);
        break;

      case _traced_profiled_retry_me          :
        fprintf(stderr, "%s_traced_profiled_retry_me%s", prepend_term, append_term);
        break;

      case _traced_profiled_trust_me          :
        fprintf(stderr, "%s_traced_profiled_trust_me%s", prepend_term, append_term);
        break;

      case _traced_profiled_retry_logical     :
        fprintf(stderr, "%s_traced_profiled_retry_logical%s", prepend_term, append_term);
        break;

      case _traced_profiled_trust_logical     :
        fprintf(stderr, "%s_traced_profiled_trust_logical%s", prepend_term, append_term);
        break;

      case _traced_count_call                 :
        fprintf(stderr, "%s_traced_count_call%s", prepend_term, append_term);
        break;

      case _traced_count_retry                :
        fprintf(stderr, "%s_traced_count_retry%s", prepend_term, append_term);
        break;

      case _traced_count_retry_me             :
        fprintf(stderr, "%s_traced_count_retry_me%s", prepend_term, append_term);
        break;

      case _traced_count_trust_me             :
        fprintf(stderr, "%s_traced_count_trust_me%s", prepend_term, append_term);
        break;

      case _traced_count_retry_logical        :
        fprintf(stderr, "%s_traced_count_retry_logical%s", prepend_term, append_term);
        break;

      case _traced_count_trust_logical        :
        fprintf(stderr, "%s_traced_count_trust_logical%s", prepend_term, append_term);
        break;

      case _traced_lock_lu                    :
        fprintf(stderr, "%s_traced_lock_lu%s", prepend_term, append_term);
        break;

      case _traced_unlock_lu                  :
        fprintf(stderr, "%s_traced_unlock_lu%s", prepend_term, append_term);
        break;

      case _traced_alloc_for_logical_pred     :
        fprintf(stderr, "%s_traced_alloc_for_logical_pred%s", prepend_term, append_term);
        break;

      case _traced_copy_idb_term              :
        fprintf(stderr, "%s_traced_copy_idb_term%s", prepend_term, append_term);
        break;

      case _traced_unify_idb_term             :
        fprintf(stderr, "%s_traced_unify_idb_term%s", prepend_term, append_term);
        break;

      case _traced_ensure_space               :
        fprintf(stderr, "%s_traced_ensure_space%s", prepend_term, append_term);
        break;

      case _traced_spy_or_trymark             :
        fprintf(stderr, "%s_traced_spy_or_trymark%s", prepend_term, append_term);
        break;

      case _traced_try_and_mark               :
        fprintf(stderr, "%s_traced_try_and_mark%s", prepend_term, append_term);
        break;

      case _traced_count_retry_and_mark       :
        fprintf(stderr, "%s_traced_count_retry_and_mark%s", prepend_term, append_term);
        break;

      case _traced_profiled_retry_and_mark    :
        fprintf(stderr, "%s_traced_profiled_retry_and_mark%s", prepend_term, append_term);
        break;

      case _traced_retry_and_mark             :
        fprintf(stderr, "%s_traced_retry_and_mark%s", prepend_term, append_term);
        break;

      case _traced_trust_fail                 :
        fprintf(stderr, "%s_traced_trust_fail%s", prepend_term, append_term);
        break;

      case _traced_op_fail                    :
        fprintf(stderr, "%s_traced_op_fail%s", prepend_term, append_term);
        break;

      case _traced_cut                        :
        fprintf(stderr, "%s_traced_cut%s", prepend_term, append_term);
        break;

      case _traced_cut_t                      :
        fprintf(stderr, "%s_traced_cut_t%s", prepend_term, append_term);
        break;

      case _traced_cut_e                      :
        fprintf(stderr, "%s_traced_cut_e%s", prepend_term, append_term);
        break;

      case _traced_save_b_x                   :
        fprintf(stderr, "%s_traced_save_b_x%s", prepend_term, append_term);
        break;

      case _traced_save_b_y                   :
        fprintf(stderr, "%s_traced_save_b_y%s", prepend_term, append_term);
        break;

      case _traced_commit_b_x                 :
        fprintf(stderr, "%s_traced_commit_b_x%s", prepend_term, append_term);
        break;

      case _traced_commit_b_y                 :
        fprintf(stderr, "%s_traced_commit_b_y%s", prepend_term, append_term);
        break;

      case _traced_execute                    :
        fprintf(stderr, "%s_traced_execute%s", prepend_term, append_term);
        break;

      case _traced_dexecute                   :
        fprintf(stderr, "%s_traced_dexecute%s", prepend_term, append_term);
        break;

      case _traced_fcall                      :
        fprintf(stderr, "%s_traced_fcall%s", prepend_term, append_term);
        break;

      case _traced_call                       :
        fprintf(stderr, "%s_traced_call%s", prepend_term, append_term);
        break;

      case _traced_procceed                   :
        fprintf(stderr, "%s_traced_procceed%s", prepend_term, append_term);
        break;

      case _traced_allocate                   :
        fprintf(stderr, "%s_traced_allocate%s", prepend_term, append_term);
        break;

      case _traced_deallocate                 :
        fprintf(stderr, "%s_traced_deallocate%s", prepend_term, append_term);
        break;

#ifdef BEAM
      case _traced_retry_eam                  :
        fprintf(stderr, "%s_traced_retry_eam%s", prepend_term, append_term);
        break;

#endif
#ifdef BEAM
      case _traced_run_eam                    :
        fprintf(stderr, "%s_traced_run_eam%s", prepend_term, append_term);
        break;

#endif
      case _traced_get_x_var                  :
        fprintf(stderr, "%s_traced_get_x_var%s", prepend_term, append_term);
        break;

      case _traced_get_y_var                  :
        fprintf(stderr, "%s_traced_get_y_var%s", prepend_term, append_term);
        break;

      case _traced_get_yy_var                 :
        fprintf(stderr, "%s_traced_get_yy_var%s", prepend_term, append_term);
        break;

      case _traced_get_x_val                  :
        fprintf(stderr, "%s_traced_get_x_val%s", prepend_term, append_term);
        break;

      case _traced_get_y_val                  :
        fprintf(stderr, "%s_traced_get_y_val%s", prepend_term, append_term);
        break;

      case _traced_get_atom                   :
        fprintf(stderr, "%s_traced_get_atom%s", prepend_term, append_term);
        break;

      case _traced_get_2atoms                 :
        fprintf(stderr, "%s_traced_get_2atoms%s", prepend_term, append_term);
        break;

      case _traced_get_3atoms                 :
        fprintf(stderr, "%s_traced_get_3atoms%s", prepend_term, append_term);
        break;

      case _traced_get_4atoms                 :
        fprintf(stderr, "%s_traced_get_4atoms%s", prepend_term, append_term);
        break;

      case _traced_get_5atoms                 :
        fprintf(stderr, "%s_traced_get_5atoms%s", prepend_term, append_term);
        break;

      case _traced_get_6atoms                 :
        fprintf(stderr, "%s_traced_get_6atoms%s", prepend_term, append_term);
        break;

      case _traced_get_list                   :
        fprintf(stderr, "%s_traced_get_list%s", prepend_term, append_term);
        break;

      case _traced_get_struct                 :
        fprintf(stderr, "%s_traced_get_struct%s", prepend_term, append_term);
        break;

      case _traced_get_float                  :
        fprintf(stderr, "%s_traced_get_float%s", prepend_term, append_term);
        break;

      case _traced_get_longint                :
        fprintf(stderr, "%s_traced_get_longint%s", prepend_term, append_term);
        break;

      case _traced_get_bigint                 :
        fprintf(stderr, "%s_traced_get_bigint%s", prepend_term, append_term);
        break;

      case _traced_get_dbterm                 :
        fprintf(stderr, "%s_traced_get_dbterm%s", prepend_term, append_term);
        break;

      case _traced_glist_valx                 :
        fprintf(stderr, "%s_traced_glist_valx%s", prepend_term, append_term);
        break;

      case _traced_glist_valy                 :
        fprintf(stderr, "%s_traced_glist_valy%s", prepend_term, append_term);
        break;

      case _traced_gl_void_varx               :
        fprintf(stderr, "%s_traced_gl_void_varx%s", prepend_term, append_term);
        break;

      case _traced_gl_void_vary               :
        fprintf(stderr, "%s_traced_gl_void_vary%s", prepend_term, append_term);
        break;

      case _traced_gl_void_valx               :
        fprintf(stderr, "%s_traced_gl_void_valx%s", prepend_term, append_term);
        break;

      case _traced_gl_void_valy               :
        fprintf(stderr, "%s_traced_gl_void_valy%s", prepend_term, append_term);
        break;

      case _traced_unify_x_var                :
        fprintf(stderr, "%s_traced_unify_x_var%s", prepend_term, append_term);
        break;

      case _traced_unify_x_var_write          :
        fprintf(stderr, "%s_traced_unify_x_var_write%s", prepend_term, append_term);
        break;

      case _traced_unify_l_x_var              :
        fprintf(stderr, "%s_traced_unify_l_x_var%s", prepend_term, append_term);
        break;

      case _traced_unify_l_x_var_write        :
        fprintf(stderr, "%s_traced_unify_l_x_var_write%s", prepend_term, append_term);
        break;

      case _traced_unify_x_var2               :
        fprintf(stderr, "%s_traced_unify_x_var2%s", prepend_term, append_term);
        break;

      case _traced_unify_x_var2_write         :
        fprintf(stderr, "%s_traced_unify_x_var2_write%s", prepend_term, append_term);
        break;

      case _traced_unify_l_x_var2             :
        fprintf(stderr, "%s_traced_unify_l_x_var2%s", prepend_term, append_term);
        break;

      case _traced_unify_l_x_var2_write       :
        fprintf(stderr, "%s_traced_unify_l_x_var2_write%s", prepend_term, append_term);
        break;

      case _traced_unify_y_var                :
        fprintf(stderr, "%s_traced_unify_y_var%s", prepend_term, append_term);
        break;

      case _traced_unify_y_var_write          :
        fprintf(stderr, "%s_traced_unify_y_var_write%s", prepend_term, append_term);
        break;

      case _traced_unify_l_y_var              :
        fprintf(stderr, "%s_traced_unify_l_y_var%s", prepend_term, append_term);
        break;

      case _traced_unify_l_y_var_write        :
        fprintf(stderr, "%s_traced_unify_l_y_var_write%s", prepend_term, append_term);
        break;

      case _traced_unify_x_val                :
        fprintf(stderr, "%s_traced_unify_x_val%s", prepend_term, append_term);
        break;

      case _traced_unify_x_val_write          :
        fprintf(stderr, "%s_traced_unify_x_val_write%s", prepend_term, append_term);
        break;

      case _traced_unify_l_x_val              :
        fprintf(stderr, "%s_traced_unify_l_x_val%s", prepend_term, append_term);
        break;

      case _traced_unify_l_x_val_write        :
        fprintf(stderr, "%s_traced_uify_l_x_val_write%s", prepend_term, append_term);
        break;

      case _traced_unify_y_val                :
        fprintf(stderr, "%s_traced_unify_y_val%s", prepend_term, append_term);
        break;

      case _traced_unify_y_val_write          :
        fprintf(stderr, "%s_traced_unify_y_val_write%s", prepend_term, append_term);
        break;

      case _traced_unify_l_y_val              :
        fprintf(stderr, "%s_traced_unify_l_y_val%s", prepend_term, append_term);
        break;

      case _traced_unify_l_y_val_write        :
        fprintf(stderr, "%s_traced_unify_l_y_val_write%s", prepend_term, append_term);
        break;

      case _traced_unify_x_loc                :
        fprintf(stderr, "%s_traced_unify_x_loc%s", prepend_term, append_term);
        break;

      case _traced_unify_x_loc_write          :
        fprintf(stderr, "%s_traced_unify_x_loc_write%s", prepend_term, append_term);
        break;

      case _traced_unify_l_x_loc              :
        fprintf(stderr, "%s_traced_unify_l_x_loc%s", prepend_term, append_term);
        break;

      case _traced_unify_l_x_loc_write        :
        fprintf(stderr, "%s_traced_unify_l_x_loc_write%s", prepend_term, append_term);
        break;

      case _traced_unify_y_loc                :
        fprintf(stderr, "%s_traced_unify_y_loc%s", prepend_term, append_term);
        break;

      case _traced_unify_y_loc_write          :
        fprintf(stderr, "%s_traced_unify_y_loc_write%s", prepend_term, append_term);
        break;

      case _traced_unify_l_y_loc              :
        fprintf(stderr, "%s_traced_unify_l_y_loc%s", prepend_term, append_term);
        break;

      case _traced_unify_l_y_loc_write        :
        fprintf(stderr, "%s_traced_unify_l_y_loc_write%s", prepend_term, append_term);
        break;

      case _traced_unify_void                 :
        fprintf(stderr, "%s_traced_unify_void%s", prepend_term, append_term);
        break;

      case _traced_unify_void_write           :
        fprintf(stderr, "%s_traced_unify_void_write%s", prepend_term, append_term);
        break;

      case _traced_unify_l_void               :
        fprintf(stderr, "%s_traced_unify_l_void%s", prepend_term, append_term);
        break;

      case _traced_unify_l_void_write         :
        fprintf(stderr, "%s_traced_unify_l_void_write%s", prepend_term, append_term);
        break;

      case _traced_unify_n_voids              :
        fprintf(stderr, "%s_traced_unify_n_voids%s", prepend_term, append_term);
        break;

      case _traced_unify_n_voids_write        :
        fprintf(stderr, "%s_traced_unify_n_voids_write%s", prepend_term, append_term);
        break;

      case _traced_unify_l_n_voids            :
        fprintf(stderr, "%s_traced_unify_l_n_voids%s", prepend_term, append_term);
        break;

      case _traced_unify_l_n_voids_write      :
        fprintf(stderr, "%s_traced_unify_l_n_voids_write%s", prepend_term, append_term);
        break;

      case _traced_unify_atom                 :
        fprintf(stderr, "%s_traced_unify_atom%s", prepend_term, append_term);
        break;

      case _traced_unify_atom_write           :
        fprintf(stderr, "%s_traced_unify_atom_write%s", prepend_term, append_term);
        break;

      case _traced_unify_l_atom               :
        fprintf(stderr, "%s_traced_unify_l_atom%s", prepend_term, append_term);
        break;

      case _traced_unify_l_atom_write         :
        fprintf(stderr, "%s_traced_unify_l_atom_write%s", prepend_term, append_term);
        break;

      case _traced_unify_n_atoms              :
        fprintf(stderr, "%s_traced_unify_n_atoms%s", prepend_term, append_term);
        break;

      case _traced_unify_n_atoms_write        :
        fprintf(stderr, "%s_traced_unify_n_atoms_write%s", prepend_term, append_term);
        break;

      case _traced_unify_float                :
        fprintf(stderr, "%s_traced_unify_float%s", prepend_term, append_term);
        break;

      case _traced_unify_float_write          :
        fprintf(stderr, "%s_traced_unify_float_write%s", prepend_term, append_term);
        break;

      case _traced_unify_l_float              :
        fprintf(stderr, "%s_traced_unify_l_float%s", prepend_term, append_term);
        break;

      case _traced_unify_l_float_write        :
        fprintf(stderr, "%s_traced_unify_l_float_write%s", prepend_term, append_term);
        break;

      case _traced_unify_longint              :
        fprintf(stderr, "%s_traced_unify_longint%s", prepend_term, append_term);
        break;

      case _traced_unify_longint_write        :
        fprintf(stderr, "%s_traced_unify_longint_write%s", prepend_term, append_term);
        break;

      case _traced_unify_l_longint            :
        fprintf(stderr, "%s_traced_unify_l_longint%s", prepend_term, append_term);
        break;

      case _traced_unify_l_longint_write      :
        fprintf(stderr, "%s_traced_unify_l_longint_write%s", prepend_term, append_term);
        break;

      case _traced_unify_bigint               :
        fprintf(stderr, "%s_traced_unify_bigint%s", prepend_term, append_term);
        break;

      case _traced_unify_l_bigint             :
        fprintf(stderr, "%s_traced_unify_l_bigint%s", prepend_term, append_term);
        break;

      case _traced_unify_dbterm               :
        fprintf(stderr, "%s_traced_unify_dbterm%s", prepend_term, append_term);
        break;

      case _traced_unify_l_dbterm             :
        fprintf(stderr, "%s_traced_unify_l_dbterm%s", prepend_term, append_term);
        break;

      case _traced_unify_list                 :
        fprintf(stderr, "%s_traced_unify_list%s", prepend_term, append_term);
        break;

      case _traced_unify_list_write           :
        fprintf(stderr, "%s_traced_unify_list_write%s", prepend_term, append_term);
        break;

      case _traced_unify_l_list               :
        fprintf(stderr, "%s_traced_unify_l_list%s", prepend_term, append_term);
        break;

      case _traced_unify_l_list_write         :
        fprintf(stderr, "%s_traced_unify_l_list_write%s", prepend_term, append_term);
        break;

      case _traced_unify_struct               :
        fprintf(stderr, "%s_traced_unify_struct%s", prepend_term, append_term);
        break;

      case _traced_unify_struct_write         :
        fprintf(stderr, "%s_traced_unify_struct_write%s", prepend_term, append_term);
        break;

      case _traced_unify_l_struc              :
        fprintf(stderr, "%s_traced_unify_l_struc%s", prepend_term, append_term);
        break;

      case _traced_unify_l_struc_write        :
        fprintf(stderr, "%s_traced_unify_l_struc_write%s", prepend_term, append_term);
        break;

      case _traced_put_x_var                  :
        fprintf(stderr, "%s_traced_put_x_var%s", prepend_term, append_term);
        break;

      case _traced_put_y_var                  :
        fprintf(stderr, "%s_traced_put_y_var%s", prepend_term, append_term);
        break;

      case _traced_put_x_val                  :
        fprintf(stderr, "%s_traced_put_x_val%s", prepend_term, append_term);
        break;

      case _traced_put_xx_val                 :
        fprintf(stderr, "%s_traced_put_xx_val%s", prepend_term, append_term);
        break;

      case _traced_put_y_val                  :
        fprintf(stderr, "%s_traced_put_y_val%s", prepend_term, append_term);
        break;

      case _traced_put_y_vals                 :
        fprintf(stderr, "%s_traced_put_y_vals%s", prepend_term, append_term);
        break;

      case _traced_put_unsafe                 :
        fprintf(stderr, "%s_traced_put_unsafe%s", prepend_term, append_term);
        break;

      case _traced_put_atom                   :
        fprintf(stderr, "%s_traced_put_atom%s", prepend_term, append_term);
        break;

      case _traced_put_dbterm                 :
        fprintf(stderr, "%s_traced_put_dbterm%s", prepend_term, append_term);
        break;

      case _traced_put_bigint                 :
        fprintf(stderr, "%s_traced_put_bigint%s", prepend_term, append_term);
        break;

      case _traced_put_float                  :
        fprintf(stderr, "%s_traced_put_float%s", prepend_term, append_term);
        break;

      case _traced_put_longint                :
        fprintf(stderr, "%s_traced_put_longint%s", prepend_term, append_term);
        break;

      case _traced_put_list                   :
        fprintf(stderr, "%s_traced_put_list%s", prepend_term, append_term);
        break;

      case _traced_put_struct                 :
        fprintf(stderr, "%s_traced_put_struct%s", prepend_term, append_term);
        break;

      case _traced_write_x_var                :
        fprintf(stderr, "%s_traced_write_x_var%s", prepend_term, append_term);
        break;

      case _traced_write_void                 :
        fprintf(stderr, "%s_traced_write_void%s", prepend_term, append_term);
        break;

      case _traced_write_n_voids              :
        fprintf(stderr, "%s_traced_write_n_voids%s", prepend_term, append_term);
        break;

      case _traced_write_y_var                :
        fprintf(stderr, "%s_traced_write_y_var%s", prepend_term, append_term);
        break;

      case _traced_write_x_val                :
        fprintf(stderr, "%s_traced_write_x_val%s", prepend_term, append_term);
        break;

      case _traced_write_x_loc                :
        fprintf(stderr, "%s_traced_write_x_loc%s", prepend_term, append_term);
        break;

      case _traced_write_y_val                :
        fprintf(stderr, "%s_traced_write_y_val%s", prepend_term, append_term);
        break;

      case _traced_write_y_loc                :
        fprintf(stderr, "%s_traced_write_y_loc%s", prepend_term, append_term);
        break;

      case _traced_write_atom                 :
        fprintf(stderr, "%s_traced_write_atom%s", prepend_term, append_term);
        break;

      case _traced_write_bigint               :
        fprintf(stderr, "%s_traced_write_bigint%s", prepend_term, append_term);
        break;

      case _traced_write_dbterm               :
        fprintf(stderr, "%s_traced_write_dbterm%s", prepend_term, append_term);
        break;

      case _traced_write_float                :
        fprintf(stderr, "%s_traced_write_float%s", prepend_term, append_term);
        break;

      case _traced_write_longint              :
        fprintf(stderr, "%s_traced_write_longint%s", prepend_term, append_term);
        break;

      case _traced_write_n_atoms              :
        fprintf(stderr, "%s_traced_write_n_atoms%s", prepend_term, append_term);
        break;

      case _traced_write_list                 :
        fprintf(stderr, "%s_traced_write_list%s", prepend_term, append_term);
        break;

      case _traced_write_l_list               :
        fprintf(stderr, "%s_traced_write_l_list%s", prepend_term, append_term);
        break;

      case _traced_write_struct               :
        fprintf(stderr, "%s_traced_write_struct%s", prepend_term, append_term);
        break;

      case _traced_write_l_struc              :
        fprintf(stderr, "%s_traced_write_l_struc%s", prepend_term, append_term);
        break;

      case _traced_save_pair_x                :
        fprintf(stderr, "%s_traced_save_pair_x%s", prepend_term, append_term);
        break;

      case _traced_save_pair_x_write          :
        fprintf(stderr, "%s_traced_save_pair_x_write%s", prepend_term, append_term);
        break;

      case _traced_save_pair_y                :
        fprintf(stderr, "%s_traced_save_pair_y%s", prepend_term, append_term);
        break;

      case _traced_save_pair_y_write          :
        fprintf(stderr, "%s_traced_save_pair_y_write%s", prepend_term, append_term);
        break;

      case _traced_save_appl_x                :
        fprintf(stderr, "%s_traced_save_appl_x%s", prepend_term, append_term);
        break;

      case _traced_save_appl_x_write          :
        fprintf(stderr, "%s_traced_save_appl_x_write%s", prepend_term, append_term);
        break;

      case _traced_save_appl_y                :
        fprintf(stderr, "%s_traced_save_appl_y%s", prepend_term, append_term);
        break;

      case _traced_save_appl_y_write          :
        fprintf(stderr, "%s_traced_save_appl_y_write%s", prepend_term, append_term);
        break;

      case _traced_jump                       :
        fprintf(stderr, "%s_traced_jump%s", prepend_term, append_term);
        break;

      case _traced_move_back                  :
        fprintf(stderr, "%s_traced_move_back%s", prepend_term, append_term);
        break;

      case _traced_skip                       :
        fprintf(stderr, "%s_traced_skip%s", prepend_term, append_term);
        break;

      case _traced_either                     :
        fprintf(stderr, "%s_traced_either%s", prepend_term, append_term);
        break;

      case _traced_or_else                    :
        fprintf(stderr, "%s_traced_or_else%s", prepend_term, append_term);
        break;

      case _traced_pop_n                      :
        fprintf(stderr, "%s_traced_pop_n%s", prepend_term, append_term);
        break;

      case _traced_pop                        :
        fprintf(stderr, "%s_traced_pop%s", prepend_term, append_term);
        break;

      case _traced_call_cpred                 :
        fprintf(stderr, "%s_traced_call_cpred%s", prepend_term, append_term);
        break;

      case _traced_execute_cpred              :
        fprintf(stderr, "%s_traced_execute_cpred%s", prepend_term, append_term);
        break;

      case _traced_call_usercpred             :
        fprintf(stderr, "%s_traced_call_usercpred%s", prepend_term, append_term);
        break;

      case _traced_call_c_wfail               :
        fprintf(stderr, "%s_traced_call_x_wfail%s", prepend_term, append_term);
        break;

      case _traced_try_c                      :
        fprintf(stderr, "%s_traced_try_c%s", prepend_term, append_term);
        break;

      case _traced_retry_c                    :
        fprintf(stderr, "%s_traced_retry_c%s", prepend_term, append_term);
        break;

#ifdef CUT_C
      case _traced_cut_c                      :
        fprintf(stderr, "%s_traced_cut_c%s", prepend_term, append_term);
        break;

#endif
      case _traced_try_userc                  :
        fprintf(stderr, "%s_traced_try_userc%s", prepend_term, append_term);
        break;

      case _traced_retry_userc                :
        fprintf(stderr, "%s_traced_retry_userc%s", prepend_term, append_term);
        break;

#ifdef CUT_C
      case _traced_cut_userc                  :
        fprintf(stderr, "%s_traced_cut_userc%s", prepend_term, append_term);
        break;

#endif
      case _traced_lock_pred                  :
        fprintf(stderr, "%s_traced_lock_pred%s", prepend_term, append_term);
        break;

      case _traced_index_pred                 :
        fprintf(stderr, "%s_traced_index_pred%s", prepend_term, append_term);
        break;

#ifdef THREADS
      case _traced_thread_local               :
        fprintf(stderr, "%s_traced_thread_local%s", prepend_term, append_term);
        break;

#endif
      case _traced_expand_index               :
        fprintf(stderr, "%s_traced_expand_index%s", prepend_term, append_term);
        break;

      case _traced_expand_clauses             :
        fprintf(stderr, "%s_traced_expand_clauses%s", prepend_term, append_term);
        break;

      case _traced_undef_p                    :
        fprintf(stderr, "%s_traced_undef_p%s", prepend_term, append_term);
        break;

      case _traced_spy_pred                   :
        fprintf(stderr, "%s_traced_spy_pred%s", prepend_term, append_term);
        break;

      case _traced_try_clause                 :
        fprintf(stderr, "%s_traced_try_clause%s", prepend_term, append_term);
        break;

      case _traced_try_clause2                :
        fprintf(stderr, "%s_traced_try_clause2%s", prepend_term, append_term);
        break;

      case _traced_try_clause3                :
        fprintf(stderr, "%s_traced_try_clause3%s", prepend_term, append_term);
        break;

      case _traced_try_clause4                :
        fprintf(stderr, "%s_traced_try_clause4%s", prepend_term, append_term);
        break;

      case _traced_retry                      :
        fprintf(stderr, "%s_traced_retry%s", prepend_term, append_term);
        break;

      case _traced_retry2                     :
        fprintf(stderr, "%s_traced_retry2%s", prepend_term, append_term);
        break;

      case _traced_retry3                     :
        fprintf(stderr, "%s_traced_retry3%s", prepend_term, append_term);
        break;

      case _traced_retry4                     :
        fprintf(stderr, "%s_traced_retry4%s", prepend_term, append_term);
        break;

      case _traced_trust                      :
        fprintf(stderr, "%s_traced_trust%s", prepend_term, append_term);
        break;

      case _traced_try_in                     :
        fprintf(stderr, "%s_traced_try_in%s", prepend_term, append_term);
        break;

      case _traced_enter_lu_pred              :
        fprintf(stderr, "%s_traced_enter_lu_pred%s", prepend_term, append_term);
        break;

      case _traced_try_logical                :
        fprintf(stderr, "%s_traced_try_logical%s", prepend_term, append_term);
        break;

      case _traced_retry_logical              :
        fprintf(stderr, "%s_traced_retry_logical%s", prepend_term, append_term);
        break;

      case _traced_trust_logical              :
        fprintf(stderr, "%s_traced_trust_logical%s", prepend_term, append_term);
        break;

      case _traced_user_switch                :
        fprintf(stderr, "%s_traced_user_switch%s", prepend_term, append_term);
        break;

      case _traced_switch_on_type             :
        fprintf(stderr, "%s_traced_switch_on_type%s", prepend_term, append_term);
        break;

      case _traced_switch_list_nl             :
        fprintf(stderr, "%s_traced_switch_list_nl%s", prepend_term, append_term);
        break;

      case _traced_switch_on_arg_type         :
        fprintf(stderr, "%s_traced_switch_on_arg_type%s", prepend_term, append_term);
        break;

      case _traced_switch_on_sub_arg_type     :
        fprintf(stderr, "%s_traced_switch_on_sub_arg_type%s", prepend_term, append_term);
        break;

      case _traced_jump_if_var                :
        fprintf(stderr, "%s_traced_jump_if_var%s", prepend_term, append_term);
        break;

      case _traced_jump_if_nonvar             :
        fprintf(stderr, "%s_traced_jump_if_nonvar%s", prepend_term, append_term);
        break;

      case _traced_if_not_then                :
        fprintf(stderr, "%s_traced_if_not_then%s", prepend_term, append_term);
        break;

      case _traced_switch_on_func             :
        fprintf(stderr, "%s_traced_switch_on_func%s", prepend_term, append_term);
        break;

      case _traced_switch_on_cons             :
        fprintf(stderr, "%s_traced_switch_on_cons%s", prepend_term, append_term);
        break;

      case _traced_go_on_func                 :
        fprintf(stderr, "%s_traced_go_on_func%s", prepend_term, append_term);
        break;

      case _traced_go_on_cons                 :
        fprintf(stderr, "%s_traced_go_on_cons%s", prepend_term, append_term);
        break;

      case _traced_if_func                    :
        fprintf(stderr, "%s_traced_if_func%s", prepend_term, append_term);
        break;

      case _traced_if_cons                    :
        fprintf(stderr, "%s_traced_if_cons%s", prepend_term, append_term);
        break;

      case _traced_index_dbref                :
        fprintf(stderr, "%s_traced_index_dbref%s", prepend_term, append_term);
        break;

      case _traced_index_blob                 :
        fprintf(stderr, "%s_traced_index_blob%s", prepend_term, append_term);
        break;

      case _traced_index_long                 :
        fprintf(stderr, "%s_traced_index_long%s", prepend_term, append_term);
        break;

      case _traced_jit_handler                  :
        fprintf(stderr, "%s_traced_jit_handler%s", prepend_term, append_term);
        break;

      case _traced_p_atom_x                   :
        fprintf(stderr, "%s_traced_p_atom_x%s", prepend_term, append_term);
        break;

      case _traced_p_atom_y                   :
        fprintf(stderr, "%s_traced_p_atom_y%s", prepend_term, append_term);
        break;

      case _traced_p_atomic_x                 :
        fprintf(stderr, "%s_traced_p_atomic_x%s", prepend_term, append_term);
        break;

      case _traced_p_atomic_y                 :
        fprintf(stderr, "%s_traced_p_atomic_y%s", prepend_term, append_term);
        break;

      case _traced_p_integer_x                :
        fprintf(stderr, "%s_traced_p_integer_x%s", prepend_term, append_term);
        break;

      case _traced_p_integer_y                :
        fprintf(stderr, "%s_traced_p_integer_y%s", prepend_term, append_term);
        break;

      case _traced_p_nonvar_x                 :
        fprintf(stderr, "%s_traced_p_nonvar_x%s", prepend_term, append_term);
        break;

      case _traced_p_nonvar_y                 :
        fprintf(stderr, "%s_traced_p_nonvar_y%s", prepend_term, append_term);
        break;

      case _traced_p_number_x                 :
        fprintf(stderr, "%s_traced_p_number_x%s", prepend_term, append_term);
        break;

      case _traced_p_number_y                 :
        fprintf(stderr, "%s_traced_p_number_y%s", prepend_term, append_term);
        break;

      case _traced_p_var_x                    :
        fprintf(stderr, "%s_traced_p_var_x%s", prepend_term, append_term);
        break;

      case _traced_p_var_y                    :
        fprintf(stderr, "%s_traced_p_var_y%s", prepend_term, append_term);
        break;

      case _traced_p_db_ref_x                 :
        fprintf(stderr, "%s_traced_p_db_ref_x%s", prepend_term, append_term);
        break;

      case _traced_p_db_ref_y                 :
        fprintf(stderr, "%s_traced_p_db_ref_y%s", prepend_term, append_term);
        break;

      case _traced_p_primitive_x              :
        fprintf(stderr, "%s_traced_p_primitive_x%s", prepend_term, append_term);
        break;

      case _traced_p_primitive_y              :
        fprintf(stderr, "%s_traced_p_primitive_y%s", prepend_term, append_term);
        break;

      case _traced_p_compound_x               :
        fprintf(stderr, "%s_traced_p_compound_x%s", prepend_term, append_term);
        break;

      case _traced_p_compound_y               :
        fprintf(stderr, "%s_traced_p_compound_y%s", prepend_term, append_term);
        break;

      case _traced_p_float_x                  :
        fprintf(stderr, "%s_traced_p_float_x%s", prepend_term, append_term);
        break;

      case _traced_p_float_y                  :
        fprintf(stderr, "%s_traced_p_float_y%s", prepend_term, append_term);
        break;

      case _traced_p_plus_vv                  :
        fprintf(stderr, "%s_traced_p_plus_vv%s", prepend_term, append_term);
        break;

      case _traced_p_plus_vc                  :
        fprintf(stderr, "%s_traced_p_plus_vc%s", prepend_term, append_term);
        break;

      case _traced_p_plus_y_vv                :
        fprintf(stderr, "%s_traced_p_plus_y_vv%s", prepend_term, append_term);
        break;

      case _traced_p_plus_y_vc                :
        fprintf(stderr, "%s_traced_p_plus_y_vc%s", prepend_term, append_term);
        break;

      case _traced_p_minus_vv                 :
        fprintf(stderr, "%s_traced_p_minus_vv%s", prepend_term, append_term);
        break;

      case _traced_p_minus_cv                 :
        fprintf(stderr, "%s_traced_p_minus_cv%s", prepend_term, append_term);
        break;

      case _traced_p_minus_y_vv               :
        fprintf(stderr, "%s_traced_p_minus_y_vv%s", prepend_term, append_term);
        break;

      case _traced_p_minus_y_cv               :
        fprintf(stderr, "%s_traced_p_minus_y_cv%s", prepend_term, append_term);
        break;

      case _traced_p_times_vv                 :
        fprintf(stderr, "%s_traced_p_times_vv%s", prepend_term, append_term);
        break;

      case _traced_p_times_vc                 :
        fprintf(stderr, "%s_traced_p_times_vc%s", prepend_term, append_term);
        break;

      case _traced_p_times_y_vv               :
        fprintf(stderr, "%s_traced_p_times_y_vv%s", prepend_term, append_term);
        break;

      case _traced_p_times_y_vc               :
        fprintf(stderr, "%s_traced_p_times_y_vc%s", prepend_term, append_term);
        break;

      case _traced_p_div_vv                   :
        fprintf(stderr, "%s_traced_p_div_vv%s", prepend_term, append_term);
        break;

      case _traced_p_div_vc                   :
        fprintf(stderr, "%s_traced_p_div_vc%s", prepend_term, append_term);
        break;

      case _traced_p_div_cv                   :
        fprintf(stderr, "%s_traced_p_div_cv%s", prepend_term, append_term);
        break;

      case _traced_p_div_y_vv                 :
        fprintf(stderr, "%s_traced_p_div_y_vv%s", prepend_term, append_term);
        break;

      case _traced_p_div_y_vc                 :
        fprintf(stderr, "%s_traced_p_div_y_vc%s", prepend_term, append_term);
        break;

      case _traced_p_div_y_cv                 :
        fprintf(stderr, "%s_traced_p_div_y_cv%s", prepend_term, append_term);
        break;

      case _traced_p_and_vv                   :
        fprintf(stderr, "%s_traced_p_and_vv%s", prepend_term, append_term);
        break;

      case _traced_p_and_vc                   :
        fprintf(stderr, "%s_traced_p_and_vc%s", prepend_term, append_term);
        break;

      case _traced_p_and_y_vv                 :
        fprintf(stderr, "%s_traced_p_and_y_vv%s", prepend_term, append_term);
        break;

      case _traced_p_and_y_vc                 :
        fprintf(stderr, "%s_traced_p_and_y_vc%s", prepend_term, append_term);
        break;

      case _traced_p_or_vv                    :
        fprintf(stderr, "%s_traced_p_or_vv%s", prepend_term, append_term);
        break;

      case _traced_p_or_vc                    :
        fprintf(stderr, "%s_traced_p_or_vc%s", prepend_term, append_term);
        break;

      case _traced_p_or_y_vv                  :
        fprintf(stderr, "%s_traced_p_or_y_vv%s", prepend_term, append_term);
        break;

      case _traced_p_or_y_vc                  :
        fprintf(stderr, "%s_traced_p_or_y_vc%s", prepend_term, append_term);
        break;

      case _traced_p_sll_vv                   :
        fprintf(stderr, "%s_traced_p_sll_vv%s", prepend_term, append_term);
        break;

      case _traced_p_sll_vc                   :
        fprintf(stderr, "%s_traced_p_sll_vc%s", prepend_term, append_term);
        break;

      case _traced_p_sll_cv                   :
        fprintf(stderr, "%s_traced_p_sll_cv%s", prepend_term, append_term);
        break;

      case _traced_p_sll_y_vv                 :
        fprintf(stderr, "%s_traced_p_sll_y_vv%s", prepend_term, append_term);
        break;

      case _traced_p_sll_y_vc                 :
        fprintf(stderr, "%s_traced_p_sll_y_vc%s", prepend_term, append_term);
        break;

      case _traced_p_sll_y_cv                 :
        fprintf(stderr, "%s_traced_p_sll_y_cv%s", prepend_term, append_term);
        break;

      case _traced_p_slr_vv                   :
        fprintf(stderr, "%s_traced_p_slr_vv%s", prepend_term, append_term);
        break;

      case _traced_p_slr_vc                   :
        fprintf(stderr, "%s_traced_p_slr_vc%s", prepend_term, append_term);
        break;

      case _traced_p_slr_cv                   :
        fprintf(stderr, "%s_traced_p_slr_cv%s", prepend_term, append_term);
        break;

      case _traced_p_slr_y_vv                 :
        fprintf(stderr, "%s_traced_p_slr_y_vv%s", prepend_term, append_term);
        break;

      case _traced_p_slr_y_vc                 :
        fprintf(stderr, "%s_traced_p_slr_y_vc%s", prepend_term, append_term);
        break;

      case _traced_p_slr_y_cv                 :
        fprintf(stderr, "%s_traced_p_slr_y_cv%s", prepend_term, append_term);
        break;

      case _traced_call_bfunc_xx              :
        fprintf(stderr, "%s_traced_call_bfunc_xx%s", prepend_term, append_term);
        break;

      case _traced_call_bfunc_yx              :
        fprintf(stderr, "%s_traced_call_bfunc_yx%s", prepend_term, append_term);
        break;

      case _traced_call_bfunc_xy              :
        fprintf(stderr, "%s_traced_call_bfunc_xy%s", prepend_term, append_term);
        break;

      case _traced_call_bfunc_yy              :
        fprintf(stderr, "%s_traced_call_bfunc_yy%s", prepend_term, append_term);
        break;

      case _traced_p_equal                    :
        fprintf(stderr, "%s_traced_p_equal%s", prepend_term, append_term);
        break;

      case _traced_p_dif                      :
        fprintf(stderr, "%s_traced_p_dif%s", prepend_term, append_term);
        break;

      case _traced_p_eq                       :
        fprintf(stderr, "%s_traced_p_eq%s", prepend_term, append_term);
        break;

      case _traced_p_arg_vv                   :
        fprintf(stderr, "%s_traced_p_arg_vv%s", prepend_term, append_term);
        break;

      case _traced_p_arg_cv                   :
        fprintf(stderr, "%s_traced_p_arg_cv%s", prepend_term, append_term);
        break;

      case _traced_p_arg_y_vv                 :
        fprintf(stderr, "%s_traced_p_arg_y_vv%s", prepend_term, append_term);
        break;

      case _traced_p_arg_y_cv                 :
        fprintf(stderr, "%s_traced_p_arg_y_cv%s", prepend_term, append_term);
        break;

      case _traced_p_func2s_vv                :
        fprintf(stderr, "%s_traced_p_func2s_vv%s", prepend_term, append_term);
        break;

      case _traced_p_func2s_cv                :
        fprintf(stderr, "%s_traced_p_func2s_cv%s", prepend_term, append_term);
        break;

      case _traced_p_func2s_vc                :
        fprintf(stderr, "%s_traced_p_func2s_vc%s", prepend_term, append_term);
        break;

      case _traced_p_func2s_y_vv              :
        fprintf(stderr, "%s_traced_p_func2s_y_vv%s", prepend_term, append_term);
        break;

      case _traced_p_func2s_y_cv              :
        fprintf(stderr, "%s_traced_p_func2s_y_cv%s", prepend_term, append_term);
        break;

      case _traced_p_func2s_y_vc              :
        fprintf(stderr, "%s_traced_p_func2s_y_vc%s", prepend_term, append_term);
        break;

      case _traced_p_func2f_xx                :
        fprintf(stderr, "%s_traced_p_func2f_xx%s", prepend_term, append_term);
        break;

      case _traced_p_func2f_xy                :
        fprintf(stderr, "%s_traced_p_func2f_xy%s", prepend_term, append_term);
        break;

      case _traced_p_func2f_yx                :
        fprintf(stderr, "%s_traced_p_func2f_yx%s", prepend_term, append_term);
        break;

      case _traced_p_func2f_yy                :
        fprintf(stderr, "%s_traced_p_func2f_yy%s", prepend_term, append_term);
        break;

      case _traced_p_functor                  :
        fprintf(stderr, "%s_traced_p_functor%s", prepend_term, append_term);
        break;

      case _traced_p_execute2                 :
        fprintf(stderr, "%s_traced_p_execute2%s", prepend_term, append_term);
        break;

      case _traced_p_execute                  :
        fprintf(stderr, "%s_traced_p_execute%s", prepend_term, append_term);
        break;

      case _traced_p_execute_tail             :
        fprintf(stderr, "%s_traced_p_execute_tail%s", prepend_term, append_term);
        break;

#ifdef YAPOR
      case _traced_getwork_first_time         :
        fprintf(stderr, "%s_traced_getwork_first_time%s", prepend_term, append_term);
        break;

      case _traced_getwork                    :
        fprintf(stderr, "%s_traced_getwork%s", prepend_term, append_term);
        break;

      case _traced_getwork_seq                :
        fprintf(stderr, "%s_traced_getwork_seq%s", prepend_term, append_term);
        break;

      case _traced_sync                       :
        fprintf(stderr, "%s_traced_sync%s", prepend_term, append_term);
        break;

#endif
#ifdef TABLING
#ifdef TABLING_INNER_CUTS
      case _traced_clause_with_cut            :
        fprintf(stderr, "%s_traced_clause_with_cut%s", prepend_term, append_term);
        break;

#endif
      case _traced_table_load_answer          :
        fprintf(stderr, "%s_traced_table_load_answer%s", prepend_term, append_term);
        break;

      case _traced_table_try_answer           :
        fprintf(stderr, "%s_traced_table_try_answer%s", prepend_term, append_term);
        break;

      case _traced_table_try_single           :
        fprintf(stderr, "%s_traced_table_try_single%s", prepend_term, append_term);
        break;

      case _traced_table_try_me               :
        fprintf(stderr, "%s_traced_table_try_me%s", prepend_term, append_term);
        break;

      case _traced_table_try                  :
        fprintf(stderr, "%s_traced_table_try%s", prepend_term, append_term);
        break;

      case _traced_table_retry_me             :
        fprintf(stderr, "%s_traced_table_retry_me%s", prepend_term, append_term);
        break;

      case _traced_table_retry                :
        fprintf(stderr, "%s_traced_table_retry%s", prepend_term, append_term);
        break;

      case _traced_table_trust_me             :
        fprintf(stderr, "%s_traced_table_trust_me%s", prepend_term, append_term);
        break;

      case _traced_table_trust                :
        fprintf(stderr, "%s_traced_table_trust%s", prepend_term, append_term);
        break;

      case _traced_table_new_answer           :
        fprintf(stderr, "%s_traced_table_new_answer%s", prepend_term, append_term);
        break;

      case _traced_table_answer_resolution    :
        fprintf(stderr, "%s_traced_table_answer_resolution%s", prepend_term, append_term);
        break;

      case _traced_table_completion           :
        fprintf(stderr, "%s_traced_table_completion%s", prepend_term, append_term);
        break;

#ifdef THREADS_CONSUMER_SHARING
      case _traced_table_answer_resolution_completion:
        fprintf(stderr, "%s_traced_table_answer_resolution_completion%s", prepend_term, append_term);
        break;

#endif
      case _traced_trie_do_var                :
        fprintf(stderr, "%s_traced_trie_do_var%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_var             :
        fprintf(stderr, "%s_traced_trie_trust_var%s", prepend_term, append_term);
        break;

      case _traced_trie_try_var               :
        fprintf(stderr, "%s_traced_trie_try_var%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_var             :
        fprintf(stderr, "%s_traced_trie_retry_var%s", prepend_term, append_term);
        break;

      case _traced_trie_do_var_in_pair        :
        fprintf(stderr, "%s_traced_trie_do_var_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_var_in_pair     :
        fprintf(stderr, "%s_traced_trie_trust_var_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_try_var_in_pair       :
        fprintf(stderr, "%s_traced_trie_try_var_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_var_in_pair     :
        fprintf(stderr, "%s_traced_trie_retry_var_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_do_val                :
        fprintf(stderr, "%s_traced_trie_do_val%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_val             :
        fprintf(stderr, "%s_traced_trie_trust_val%s", prepend_term, append_term);
        break;

      case _traced_trie_try_val               :
        fprintf(stderr, "%s_traced_trie_try_val%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_val             :
        fprintf(stderr, "%s_traced_trie_retry_val%s", prepend_term, append_term);
        break;

      case _traced_trie_do_val_in_pair        :
        fprintf(stderr, "%s_traced_trie_do_val_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_val_in_pair     :
        fprintf(stderr, "%s_traced_trie_trust_val_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_try_val_in_pair       :
        fprintf(stderr, "%s_traced_trie_try_val_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_val_in_pair     :
        fprintf(stderr, "%s_traced_trie_retry_val_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_do_atom               :
        fprintf(stderr, "%s_traced_trie_do_atom%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_atom            :
        fprintf(stderr, "%s_traced_trie_trust_atom%s", prepend_term, append_term);
        break;

      case _traced_trie_try_atom              :
        fprintf(stderr, "%s_traced_trie_try_atom%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_atom            :
        fprintf(stderr, "%s_traced_trie_retry_atom%s", prepend_term, append_term);
        break;

      case _traced_trie_do_atom_in_pair       :
        fprintf(stderr, "%s_traced_trie_do_atom_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_atom_in_pair    :
        fprintf(stderr, "%s_traced_trie_trust_atom_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_try_atom_in_pair      :
        fprintf(stderr, "%s_traced_trie_try_atom_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_atom_in_pair    :
        fprintf(stderr, "%s_traced_trie_retry_atom_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_do_null               :
        fprintf(stderr, "%s_traced_trie_do_null%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_null            :
        fprintf(stderr, "%s_traced_trie_trust_null%s", prepend_term, append_term);
        break;

      case _traced_trie_try_null              :
        fprintf(stderr, "%s_traced_trie_try_null%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_null            :
        fprintf(stderr, "%s_traced_trie_retry_null%s", prepend_term, append_term);
        break;

      case _traced_trie_do_null_in_pair       :
        fprintf(stderr, "%s_traced_trie_do_null_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_null_in_pair    :
        fprintf(stderr, "%s_traced_trie_trust_null_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_try_null_in_pair      :
        fprintf(stderr, "%s_traced_tri_try_null_in_paire%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_null_in_pair    :
        fprintf(stderr, "%s_traced_trie_retry_null_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_do_pair               :
        fprintf(stderr, "%s_traced_trie_do_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_pair            :
        fprintf(stderr, "%s_traced_trie_trust_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_try_pair              :
        fprintf(stderr, "%s_traced_trie_try_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_pair            :
        fprintf(stderr, "%s_traced_trie_retry_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_do_appl               :
        fprintf(stderr, "%s_traced_trie_do_appl%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_appl            :
        fprintf(stderr, "%s_traced_trie_trust_appl%s", prepend_term, append_term);
        break;

      case _traced_trie_try_appl              :
        fprintf(stderr, "%s_traced_trie_try_appl%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_appl            :
        fprintf(stderr, "%s_traced_trie_retry_appl%s", prepend_term, append_term);
        break;

      case _traced_trie_do_appl_in_pair       :
        fprintf(stderr, "%s_traced_trie_do_appl_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_appl_in_pair    :
        fprintf(stderr, "%s_traced_trie_trust_appl_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_try_appl_in_pair      :
        fprintf(stderr, "%s_traced_trie_trty_appkl_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_appl_in_pair    :
        fprintf(stderr, "%s_traced_trie_retry_appl_in_pair%s", prepend_term, append_term);
        break;

      case _traced_trie_do_extension          :
        fprintf(stderr, "%s_traced_trie_do_extension%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_extension       :
        fprintf(stderr, "%s_traced_trie_trust_extension%s", prepend_term, append_term);
        break;

      case _traced_trie_try_extension         :
        fprintf(stderr, "%s_traced_trie_try_extension%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_extension       :
        fprintf(stderr, "%s_traced_trie_retry_extension%s", prepend_term, append_term);
        break;

      case _traced_trie_do_double             :
        fprintf(stderr, "%s_traced_trie_do_double%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_double          :
        fprintf(stderr, "%s_traced_trie_trust_double%s", prepend_term, append_term);
        break;

      case _traced_trie_try_double            :
        fprintf(stderr, "%s_traced_trie_try_double%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_double          :
        fprintf(stderr, "%s_traced_trie_retry_double%s", prepend_term, append_term);
        break;

      case _traced_trie_do_longint            :
        fprintf(stderr, "%s_traced_trie_do_longint%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_longint         :
        fprintf(stderr, "%s_traced_trie_trust_longint%s", prepend_term, append_term);
        break;

      case _traced_trie_try_longint           :
        fprintf(stderr, "%s_traced_trie_try_longint%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_longint         :
        fprintf(stderr, "%s_traced_trie_retry_longint%s", prepend_term, append_term);
        break;

      case _traced_trie_do_gterm              :
        fprintf(stderr, "%s_traced_trie_do_gterm%s", prepend_term, append_term);
        break;

      case _traced_trie_trust_gterm           :
        fprintf(stderr, "%s_traced_trie_trust_gterm%s", prepend_term, append_term);
        break;

      case _traced_trie_try_gterm             :
        fprintf(stderr, "%s_traced_trie_try_gterm%s", prepend_term, append_term);
        break;

      case _traced_trie_retry_gterm           :
        fprintf(stderr, "%s_traced_trie_retry_gterm%s", prepend_term, append_term);
        break;

#endif
  /* this instruction is hardwired */
#ifdef YAPOR
      case _traced_or_last                    :
        fprintf(stderr, "%s_traced_or_last%s", prepend_term, append_term);
        break;
#else
      case _traced_or_last                    :
        fprintf(stderr, "%s_traced_or_last%s", prepend_term, append_term);
        break;
#endif

#endif /* YAP_JIT */
    }
  }

void print_nop(op_numbers);

void
print_nop(op_numbers op) {
  print_op("", op, "\n");
}
