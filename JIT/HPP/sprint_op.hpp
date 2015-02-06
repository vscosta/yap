void sprint_op(char*, char*, op_numbers, char*);

void
sprint_op(char *out, char* prepend_term, op_numbers op, char* append_term) {
	char tmp[1024];
    switch(op){
      case _Ystop                      :
        sprintf(tmp, "_Ystop");
        break;

      case _Nstop                      :
        sprintf(tmp, "_Nstop");
        break;

      case _try_me                     :
        sprintf(tmp, "_try_me");
        break;

      case _retry_me                   :
        sprintf(tmp, "_retry_me");
        break;

      case _trust_me                   :
        sprintf(tmp, "_trust_me");
        break;

      case _enter_profiling            :
        sprintf(tmp, "_enter_profiling");
        break;

      case _retry_profiled             :
        sprintf(tmp, "_retry_profiled");
        break;

      case _profiled_retry_me          :
        sprintf(tmp, "_profiled_retry_me");
        break;

      case _profiled_trust_me          :
        sprintf(tmp, "_profiled_trust_me");
        break;

      case _profiled_retry_logical     :
        sprintf(tmp, "_profiled_retry_logical");
        break;

      case _profiled_trust_logical     :
        sprintf(tmp, "_profiled_trust_logical");
        break;

      case _count_call                 :
        sprintf(tmp, "_count_call");
        break;

      case _count_retry                :
        sprintf(tmp, "_count_retry");
        break;

      case _count_retry_me             :
        sprintf(tmp, "_count_retry_me");
        break;

      case _count_trust_me             :
        sprintf(tmp, "_count_trust_me");
        break;

      case _count_retry_logical        :
        sprintf(tmp, "_count_retry_logical");
        break;

      case _count_trust_logical        :
        sprintf(tmp, "_count_trust_logical");
        break;

      case _lock_lu                    :
        sprintf(tmp, "_lock_lu");
        break;

      case _unlock_lu                  :
        sprintf(tmp, "_unlock_lu");
        break;

      case _alloc_for_logical_pred     :
        sprintf(tmp, "_alloc_for_logical_pred");
        break;

      case _copy_idb_term              :
        sprintf(tmp, "_copy_idb_term");
        break;

      case _unify_idb_term             :
        sprintf(tmp, "_unify_idb_term");
        break;

      case _ensure_space               :
        sprintf(tmp, "_ensure_space");
        break;

      case _spy_or_trymark             :
        sprintf(tmp, "_spy_or_trymark");
        break;

      case _try_and_mark               :
        sprintf(tmp, "_try_and_mark");
        break;

      case _count_retry_and_mark       :
        sprintf(tmp, "_count_retry_and_mark");
        break;

      case _profiled_retry_and_mark    :
        sprintf(tmp, "_profiled_retry_and_mark");
        break;

      case _retry_and_mark             :
        sprintf(tmp, "_retry_and_mark");
        break;

      case _trust_fail                 :
        sprintf(tmp, "_trust_fail");
        break;

      case _op_fail                    :
        sprintf(tmp, "_op_fail");
        break;

      case _cut                        :
        sprintf(tmp, "_cut");
        break;

      case _cut_t                      :
        sprintf(tmp, "_cut_t");
        break;

      case _cut_e                      :
        sprintf(tmp, "_cut_e");
        break;

      case _save_b_x                   :
        sprintf(tmp, "_save_b_x");
        break;

      case _save_b_y                   :
        sprintf(tmp, "_save_b_y");
        break;

      case _commit_b_x                 :
        sprintf(tmp, "_commit_b_x");
        break;

      case _commit_b_y                 :
        sprintf(tmp, "_commit_b_y");
        break;

      case _execute                    :
        sprintf(tmp, "_execute");
        break;

      case _dexecute                   :
        sprintf(tmp, "_dexecute");
        break;

      case _fcall                      :
        sprintf(tmp, "_fcall");
        break;

      case _call                       :
        sprintf(tmp, "_call");
        break;

      case _procceed                   :
        sprintf(tmp, "_procceed");
        break;

      case _allocate                   :
        sprintf(tmp, "_allocate");
        break;

      case _deallocate                 :
        sprintf(tmp, "_deallocate");
        break;

#ifdef BEAM
      case _retry_eam                  :
        sprintf(tmp, "_retry_eam");
        break;

#endif
#ifdef BEAM
      case _run_eam                    :
        sprintf(tmp, "_run_eam");
        break;

#endif
      case _get_x_var                  :
        sprintf(tmp, "_get_x_var");
        break;

      case _get_y_var                  :
        sprintf(tmp, "_get_y_var");
        break;

      case _get_yy_var                 :
        sprintf(tmp, "_get_yy_var");
        break;

      case _get_x_val                  :
        sprintf(tmp, "_get_x_val");
        break;

      case _get_y_val                  :
        sprintf(tmp, "_get_y_val");
        break;

      case _get_atom                   :
        sprintf(tmp, "_get_atom");
        break;

      case _get_2atoms                 :
        sprintf(tmp, "_get_2atoms");
        break;

      case _get_3atoms                 :
        sprintf(tmp, "_get_3atoms");
        break;

      case _get_4atoms                 :
        sprintf(tmp, "_get_4atoms");
        break;

      case _get_5atoms                 :
        sprintf(tmp, "_get_5atoms");
        break;

      case _get_6atoms                 :
        sprintf(tmp, "_get_6atoms");
        break;

      case _get_list                   :
        sprintf(tmp, "_get_list");
        break;

      case _get_struct                 :
        sprintf(tmp, "_get_struct");
        break;

      case _get_float                  :
        sprintf(tmp, "_get_float");
        break;

      case _get_longint                :
        sprintf(tmp, "_get_longint");
        break;

      case _get_bigint                 :
        sprintf(tmp, "_get_bigint");
        break;

      case _get_dbterm                 :
        sprintf(tmp, "_get_dbterm");
        break;

      case _glist_valx                 :
        sprintf(tmp, "_glist_valx");
        break;

      case _glist_valy                 :
        sprintf(tmp, "_glist_valy");
        break;

      case _gl_void_varx               :
        sprintf(tmp, "_gl_void_varx");
        break;

      case _gl_void_vary               :
        sprintf(tmp, "_gl_void_vary");
        break;

      case _gl_void_valx               :
        sprintf(tmp, "_gl_void_valx");
        break;

      case _gl_void_valy               :
        sprintf(tmp, "_gl_void_valy");
        break;

      case _unify_x_var                :
        sprintf(tmp, "_unify_x_var");
        break;

      case _unify_x_var_write          :
        sprintf(tmp, "_unify_x_var_write");
        break;

      case _unify_l_x_var              :
        sprintf(tmp, "_unify_l_x_var");
        break;

      case _unify_l_x_var_write        :
        sprintf(tmp, "_unify_l_x_var_write");
        break;

      case _unify_x_var2               :
        sprintf(tmp, "_unify_x_var2");
        break;

      case _unify_x_var2_write         :
        sprintf(tmp, "_unify_x_var2_write");
        break;

      case _unify_l_x_var2             :
        sprintf(tmp, "_unify_l_x_var2");
        break;

      case _unify_l_x_var2_write       :
        sprintf(tmp, "_unify_l_x_var2_write");
        break;

      case _unify_y_var                :
        sprintf(tmp, "_unify_y_var");
        break;

      case _unify_y_var_write          :
        sprintf(tmp, "_unify_y_var_write");
        break;

      case _unify_l_y_var              :
        sprintf(tmp, "_unify_l_y_var");
        break;

      case _unify_l_y_var_write        :
        sprintf(tmp, "_unify_l_y_var_write");
        break;

      case _unify_x_val                :
        sprintf(tmp, "_unify_x_val");
        break;

      case _unify_x_val_write          :
        sprintf(tmp, "_unify_x_val_write");
        break;

      case _unify_l_x_val              :
        sprintf(tmp, "_unify_l_x_val");
        break;

      case _unify_l_x_val_write        :
        sprintf(tmp, "_uify_l_x_val_write");
        break;

      case _unify_y_val                :
        sprintf(tmp, "_unify_y_val");
        break;

      case _unify_y_val_write          :
        sprintf(tmp, "_unify_y_val_write");
        break;

      case _unify_l_y_val              :
        sprintf(tmp, "_unify_l_y_val");
        break;

      case _unify_l_y_val_write        :
        sprintf(tmp, "_unify_l_y_val_write");
        break;

      case _unify_x_loc                :
        sprintf(tmp, "_unify_x_loc");
        break;

      case _unify_x_loc_write          :
        sprintf(tmp, "_unify_x_loc_write");
        break;

      case _unify_l_x_loc              :
        sprintf(tmp, "_unify_l_x_loc");
        break;

      case _unify_l_x_loc_write        :
        sprintf(tmp, "_unify_l_x_loc_write");
        break;

      case _unify_y_loc                :
        sprintf(tmp, "_unify_y_loc");
        break;

      case _unify_y_loc_write          :
        sprintf(tmp, "_unify_y_loc_write");
        break;

      case _unify_l_y_loc              :
        sprintf(tmp, "_unify_l_y_loc");
        break;

      case _unify_l_y_loc_write        :
        sprintf(tmp, "_unify_l_y_loc_write");
        break;

      case _unify_void                 :
        sprintf(tmp, "_unify_void");
        break;

      case _unify_void_write           :
        sprintf(tmp, "_unify_void_write");
        break;

      case _unify_l_void               :
        sprintf(tmp, "_unify_l_void");
        break;

      case _unify_l_void_write         :
        sprintf(tmp, "_unify_l_void_write");
        break;

      case _unify_n_voids              :
        sprintf(tmp, "_unify_n_voids");
        break;

      case _unify_n_voids_write        :
        sprintf(tmp, "_unify_n_voids_write");
        break;

      case _unify_l_n_voids            :
        sprintf(tmp, "_unify_l_n_voids");
        break;

      case _unify_l_n_voids_write      :
        sprintf(tmp, "_unify_l_n_voids_write");
        break;

      case _unify_atom                 :
        sprintf(tmp, "_unify_atom");
        break;

      case _unify_atom_write           :
        sprintf(tmp, "_unify_atom_write");
        break;

      case _unify_l_atom               :
        sprintf(tmp, "_unify_l_atom");
        break;

      case _unify_l_atom_write         :
        sprintf(tmp, "_unify_l_atom_write");
        break;

      case _unify_n_atoms              :
        sprintf(tmp, "_unify_n_atoms");
        break;

      case _unify_n_atoms_write        :
        sprintf(tmp, "_unify_n_atoms_write");
        break;

      case _unify_float                :
        sprintf(tmp, "_unify_float");
        break;

      case _unify_float_write          :
        sprintf(tmp, "_unify_float_write");
        break;

      case _unify_l_float              :
        sprintf(tmp, "_unify_l_float");
        break;

      case _unify_l_float_write        :
        sprintf(tmp, "_unify_l_float_write");
        break;

      case _unify_longint              :
        sprintf(tmp, "_unify_longint");
        break;

      case _unify_longint_write        :
        sprintf(tmp, "_unify_longint_write");
        break;

      case _unify_l_longint            :
        sprintf(tmp, "_unify_l_longint");
        break;

      case _unify_l_longint_write      :
        sprintf(tmp, "_unify_l_longint_write");
        break;

      case _unify_bigint               :
        sprintf(tmp, "_unify_bigint");
        break;

      case _unify_l_bigint             :
        sprintf(tmp, "_unify_l_bigint");
        break;

      case _unify_dbterm               :
        sprintf(tmp, "_unify_dbterm");
        break;

      case _unify_l_dbterm             :
        sprintf(tmp, "_unify_l_dbterm");
        break;

      case _unify_list                 :
        sprintf(tmp, "_unify_list");
        break;

      case _unify_list_write           :
        sprintf(tmp, "_unify_list_write");
        break;

      case _unify_l_list               :
        sprintf(tmp, "_unify_l_list");
        break;

      case _unify_l_list_write         :
        sprintf(tmp, "_unify_l_list_write");
        break;

      case _unify_struct               :
        sprintf(tmp, "_unify_struct");
        break;

      case _unify_struct_write         :
        sprintf(tmp, "_unify_struct_write");
        break;

      case _unify_l_struc              :
        sprintf(tmp, "_unify_l_struc");
        break;

      case _unify_l_struc_write        :
        sprintf(tmp, "_unify_l_struc_write");
        break;

      case _put_x_var                  :
        sprintf(tmp, "_put_x_var");
        break;

      case _put_y_var                  :
        sprintf(tmp, "_put_y_var");
        break;

      case _put_x_val                  :
        sprintf(tmp, "_put_x_val");
        break;

      case _put_xx_val                 :
        sprintf(tmp, "_put_xx_val");
        break;

      case _put_y_val                  :
        sprintf(tmp, "_put_y_val");
        break;

      case _put_y_vals                 :
        sprintf(tmp, "_put_y_vals");
        break;

      case _put_unsafe                 :
        sprintf(tmp, "_put_unsafe");
        break;

      case _put_atom                   :
        sprintf(tmp, "_put_atom");
        break;

      case _put_dbterm                 :
        sprintf(tmp, "_put_dbterm");
        break;

      case _put_bigint                 :
        sprintf(tmp, "_put_bigint");
        break;

      case _put_float                  :
        sprintf(tmp, "_put_float");
        break;

      case _put_longint                :
        sprintf(tmp, "_put_longint");
        break;

      case _put_list                   :
        sprintf(tmp, "_put_list");
        break;

      case _put_struct                 :
        sprintf(tmp, "_put_struct");
        break;

      case _write_x_var                :
        sprintf(tmp, "_write_x_var");
        break;

      case _write_void                 :
        sprintf(tmp, "_write_void");
        break;

      case _write_n_voids              :
        sprintf(tmp, "_write_n_voids");
        break;

      case _write_y_var                :
        sprintf(tmp, "_write_y_var");
        break;

      case _write_x_val                :
        sprintf(tmp, "_write_x_val");
        break;

      case _write_x_loc                :
        sprintf(tmp, "_write_x_loc");
        break;

      case _write_y_val                :
        sprintf(tmp, "_write_y_val");
        break;

      case _write_y_loc                :
        sprintf(tmp, "_write_y_loc");
        break;

      case _write_atom                 :
        sprintf(tmp, "_write_atom");
        break;

      case _write_bigint               :
        sprintf(tmp, "_write_bigint");
        break;

      case _write_dbterm               :
        sprintf(tmp, "_write_dbterm");
        break;

      case _write_float                :
        sprintf(tmp, "_write_float");
        break;

      case _write_longint              :
        sprintf(tmp, "_write_longint");
        break;

      case _write_n_atoms              :
        sprintf(tmp, "_write_n_atoms");
        break;

      case _write_list                 :
        sprintf(tmp, "_write_list");
        break;

      case _write_l_list               :
        sprintf(tmp, "_write_l_list");
        break;

      case _write_struct               :
        sprintf(tmp, "_write_struct");
        break;

      case _write_l_struc              :
        sprintf(tmp, "_write_l_struc");
        break;

      case _save_pair_x                :
        sprintf(tmp, "_save_pair_x");
        break;

      case _save_pair_x_write          :
        sprintf(tmp, "_save_pair_x_write");
        break;

      case _save_pair_y                :
        sprintf(tmp, "_save_pair_y");
        break;

      case _save_pair_y_write          :
        sprintf(tmp, "_save_pair_y_write");
        break;

      case _save_appl_x                :
        sprintf(tmp, "_save_appl_x");
        break;

      case _save_appl_x_write          :
        sprintf(tmp, "_save_appl_x_write");
        break;

      case _save_appl_y                :
        sprintf(tmp, "_save_appl_y");
        break;

      case _save_appl_y_write          :
        sprintf(tmp, "_save_appl_y_write");
        break;

      case _jump                       :
        sprintf(tmp, "_jump");
        break;

      case _move_back                  :
        sprintf(tmp, "_move_back");
        break;

      case _skip                       :
        sprintf(tmp, "_skip");
        break;

      case _either                     :
        sprintf(tmp, "_either");
        break;

      case _or_else                    :
        sprintf(tmp, "_or_else");
        break;

      case _pop_n                      :
        sprintf(tmp, "_pop_n");
        break;

      case _pop                        :
        sprintf(tmp, "_pop");
        break;

      case _call_cpred                 :
        sprintf(tmp, "_call_cpred");
        break;

      case _execute_cpred              :
        sprintf(tmp, "_execute_cpred");
        break;

      case _call_usercpred             :
        sprintf(tmp, "_call_usercpred");
        break;

      case _call_c_wfail               :
        sprintf(tmp, "_call_x_wfail");
        break;

      case _try_c                      :
        sprintf(tmp, "_try_c");
        break;

      case _retry_c                    :
        sprintf(tmp, "_retry_c");
        break;

#ifdef CUT_C
      case _cut_c                      :
        sprintf(tmp, "_cut_c");
        break;

#endif
      case _try_userc                  :
        sprintf(tmp, "_try_userc");
        break;

      case _retry_userc                :
        sprintf(tmp, "_retry_userc");
        break;

#ifdef CUT_C
      case _cut_userc                  :
        sprintf(tmp, "_cut_userc");
        break;

#endif
      case _lock_pred                  :
        sprintf(tmp, "_lock_pred");
        break;

      case _index_pred                 :
        sprintf(tmp, "_index_pred");
        break;

#ifdef THREADS
      case _thread_local               :
        sprintf(tmp, "_thread_local");
        break;

#endif
      case _expand_index               :
        sprintf(tmp, "_expand_index");
        break;

      case _expand_clauses             :
        sprintf(tmp, "_expand_clauses");
        break;

      case _undef_p                    :
        sprintf(tmp, "_undef_p");
        break;

      case _spy_pred                   :
        sprintf(tmp, "_spy_pred");
        break;

      case _try_clause                 :
        sprintf(tmp, "_try_clause");
        break;

      case _try_clause2                :
        sprintf(tmp, "_try_clause2");
        break;

      case _try_clause3                :
        sprintf(tmp, "_try_clause3");
        break;

      case _try_clause4                :
        sprintf(tmp, "_try_clause4");
        break;

      case _retry                      :
        sprintf(tmp, "_retry");
        break;

      case _retry2                     :
        sprintf(tmp, "_retry2");
        break;

      case _retry3                     :
        sprintf(tmp, "_retry3");
        break;

      case _retry4                     :
        sprintf(tmp, "_retry4");
        break;

      case _trust                      :
        sprintf(tmp, "_trust");
        break;

      case _try_in                     :
        sprintf(tmp, "_try_in");
        break;

      case _try_logical                :
        sprintf(tmp, "_try_logical");
        break;

      case _retry_logical              :
        sprintf(tmp, "_retry_logical");
        break;

      case _trust_logical              :
        sprintf(tmp, "_trust_logical");
        break;

      case _user_switch                :
        sprintf(tmp, "_user_switch");
        break;

      case _switch_on_type             :
        sprintf(tmp, "_switch_on_type");
        break;

      case _switch_list_nl             :
        sprintf(tmp, "_switch_list_nl");
        break;

      case _switch_on_arg_type         :
        sprintf(tmp, "_switch_on_arg_type");
        break;

      case _switch_on_sub_arg_type     :
        sprintf(tmp, "_switch_on_sub_arg_type");
        break;

      case _jump_if_var                :
        sprintf(tmp, "_jump_if_var");
        break;

      case _jump_if_nonvar             :
        sprintf(tmp, "_jump_if_nonvar");
        break;

      case _if_not_then                :
        sprintf(tmp, "_if_not_then");
        break;

      case _switch_on_func             :
        sprintf(tmp, "_switch_on_func");
        break;

      case _switch_on_cons             :
        sprintf(tmp, "_switch_on_cons");
        break;

      case _go_on_func                 :
        sprintf(tmp, "_go_on_func");
        break;

      case _go_on_cons                 :
        sprintf(tmp, "_go_on_cons");
        break;

      case _if_func                    :
        sprintf(tmp, "_if_func");
        break;

      case _if_cons                    :
        sprintf(tmp, "_if_cons");
        break;

      case _index_dbref                :
        sprintf(tmp, "_index_dbref");
        break;

      case _index_blob                 :
        sprintf(tmp, "_index_blob");
        break;

      case _index_long                 :
        sprintf(tmp, "_index_long");
        break;

#ifdef YAP_JIT
      case _jit_handler                  :
        sprintf(tmp, "_jit_handler");
        break;
#endif /*YAP_JIT*/

      case _p_atom_x                   :
        sprintf(tmp, "_p_atom_x");
        break;

      case _p_atom_y                   :
        sprintf(tmp, "_p_atom_y");
        break;

      case _p_atomic_x                 :
        sprintf(tmp, "_p_atomic_x");
        break;

      case _p_atomic_y                 :
        sprintf(tmp, "_p_atomic_y");
        break;

      case _p_integer_x                :
        sprintf(tmp, "_p_integer_x");
        break;

      case _p_integer_y                :
        sprintf(tmp, "_p_integer_y");
        break;

      case _p_nonvar_x                 :
        sprintf(tmp, "_p_nonvar_x");
        break;

      case _p_nonvar_y                 :
        sprintf(tmp, "_p_nonvar_y");
        break;

      case _p_number_x                 :
        sprintf(tmp, "_p_number_x");
        break;

      case _p_number_y                 :
        sprintf(tmp, "_p_number_y");
        break;

      case _p_var_x                    :
        sprintf(tmp, "_p_var_x");
        break;

      case _p_var_y                    :
        sprintf(tmp, "_p_var_y");
        break;

      case _p_db_ref_x                 :
        sprintf(tmp, "_p_db_ref_x");
        break;

      case _p_db_ref_y                 :
        sprintf(tmp, "_p_db_ref_y");
        break;

      case _p_primitive_x              :
        sprintf(tmp, "_p_primitive_x");
        break;

      case _p_primitive_y              :
        sprintf(tmp, "_p_primitive_y");
        break;

      case _p_compound_x               :
        sprintf(tmp, "_p_compound_x");
        break;

      case _p_compound_y               :
        sprintf(tmp, "_p_compound_y");
        break;

      case _p_float_x                  :
        sprintf(tmp, "_p_float_x");
        break;

      case _p_float_y                  :
        sprintf(tmp, "_p_float_y");
        break;

      case _p_plus_vv                  :
        sprintf(tmp, "_p_plus_vv");
        break;

      case _p_plus_vc                  :
        sprintf(tmp, "_p_plus_vc");
        break;

      case _p_plus_y_vv                :
        sprintf(tmp, "_p_plus_y_vv");
        break;

      case _p_plus_y_vc                :
        sprintf(tmp, "_p_plus_y_vc");
        break;

      case _p_minus_vv                 :
        sprintf(tmp, "_p_minus_vv");
        break;

      case _p_minus_cv                 :
        sprintf(tmp, "_p_minus_cv");
        break;

      case _p_minus_y_vv               :
        sprintf(tmp, "_p_minus_y_vv");
        break;

      case _p_minus_y_cv               :
        sprintf(tmp, "_p_minus_y_cv");
        break;

      case _p_times_vv                 :
        sprintf(tmp, "_p_times_vv");
        break;

      case _p_times_vc                 :
        sprintf(tmp, "_p_times_vc");
        break;

      case _p_times_y_vv               :
        sprintf(tmp, "_p_times_y_vv");
        break;

      case _p_times_y_vc               :
        sprintf(tmp, "_p_times_y_vc");
        break;

      case _p_div_vv                   :
        sprintf(tmp, "_p_div_vv");
        break;

      case _p_div_vc                   :
        sprintf(tmp, "_p_div_vc");
        break;

      case _p_div_cv                   :
        sprintf(tmp, "_p_div_cv");
        break;

      case _p_div_y_vv                 :
        sprintf(tmp, "_p_div_y_vv");
        break;

      case _p_div_y_vc                 :
        sprintf(tmp, "_p_div_y_vc");
        break;

      case _p_div_y_cv                 :
        sprintf(tmp, "_p_div_y_cv");
        break;

      case _p_and_vv                   :
        sprintf(tmp, "_p_and_vv");
        break;

      case _p_and_vc                   :
        sprintf(tmp, "_p_and_vc");
        break;

      case _p_and_y_vv                 :
        sprintf(tmp, "_p_and_y_vv");
        break;

      case _p_and_y_vc                 :
        sprintf(tmp, "_p_and_y_vc");
        break;

      case _p_or_vv                    :
        sprintf(tmp, "_p_or_vv");
        break;

      case _p_or_vc                    :
        sprintf(tmp, "_p_or_vc");
        break;

      case _p_or_y_vv                  :
        sprintf(tmp, "_p_or_y_vv");
        break;

      case _p_or_y_vc                  :
        sprintf(tmp, "_p_or_y_vc");
        break;

      case _p_sll_vv                   :
        sprintf(tmp, "_p_sll_vv");
        break;

      case _p_sll_vc                   :
        sprintf(tmp, "_p_sll_vc");
        break;

      case _p_sll_cv                   :
        sprintf(tmp, "_p_sll_cv");
        break;

      case _p_sll_y_vv                 :
        sprintf(tmp, "_p_sll_y_vv");
        break;

      case _p_sll_y_vc                 :
        sprintf(tmp, "_p_sll_y_vc");
        break;

      case _p_sll_y_cv                 :
        sprintf(tmp, "_p_sll_y_cv");
        break;

      case _p_slr_vv                   :
        sprintf(tmp, "_p_slr_vv");
        break;

      case _p_slr_vc                   :
        sprintf(tmp, "_p_slr_vc");
        break;

      case _p_slr_cv                   :
        sprintf(tmp, "_p_slr_cv");
        break;

      case _p_slr_y_vv                 :
        sprintf(tmp, "_p_slr_y_vv");
        break;

      case _p_slr_y_vc                 :
        sprintf(tmp, "_p_slr_y_vc");
        break;

      case _p_slr_y_cv                 :
        sprintf(tmp, "_p_slr_y_cv");
        break;

      case _call_bfunc_xx              :
        sprintf(tmp, "_call_bfunc_xx");
        break;

      case _call_bfunc_yx              :
        sprintf(tmp, "_call_bfunc_yx");
        break;

      case _call_bfunc_xy              :
        sprintf(tmp, "_call_bfunc_xy");
        break;

      case _call_bfunc_yy              :
        sprintf(tmp, "_call_bfunc_yy");
        break;

      case _p_equal                    :
        sprintf(tmp, "_p_equal");
        break;

      case _p_dif                      :
        sprintf(tmp, "_p_dif");
        break;

      case _p_eq                       :
        sprintf(tmp, "_p_eq");
        break;

      case _p_arg_vv                   :
        sprintf(tmp, "_p_arg_vv");
        break;

      case _p_arg_cv                   :
        sprintf(tmp, "_p_arg_cv");
        break;

      case _p_arg_y_vv                 :
        sprintf(tmp, "_p_arg_y_vv");
        break;

      case _p_arg_y_cv                 :
        sprintf(tmp, "_p_arg_y_cv");
        break;

      case _p_func2s_vv                :
        sprintf(tmp, "_p_func2s_vv");
        break;

      case _p_func2s_cv                :
        sprintf(tmp, "_p_func2s_cv");
        break;

      case _p_func2s_vc                :
        sprintf(tmp, "_p_func2s_vc");
        break;

      case _p_func2s_y_vv              :
        sprintf(tmp, "_p_func2s_y_vv");
        break;

      case _p_func2s_y_cv              :
        sprintf(tmp, "_p_func2s_y_cv");
        break;

      case _p_func2s_y_vc              :
        sprintf(tmp, "_p_func2s_y_vc");
        break;

      case _p_func2f_xx                :
        sprintf(tmp, "_p_func2f_xx");
        break;

      case _p_func2f_xy                :
        sprintf(tmp, "_p_func2f_xy");
        break;

      case _p_func2f_yx                :
        sprintf(tmp, "_p_func2f_yx");
        break;

      case _p_func2f_yy                :
        sprintf(tmp, "_p_func2f_yy");
        break;

      case _p_functor                  :
        sprintf(tmp, "_p_functor");
        break;

      case _p_execute2                 :
        sprintf(tmp, "_p_execute2");
        break;

      case _p_execute                  :
        sprintf(tmp, "_p_execute");
        break;

      case _p_execute_tail             :
        sprintf(tmp, "_p_execute_tail");
        break;

#ifdef YAPOR
      case _getwork_first_time         :
        sprintf(tmp, "_getwork_first_time");
        break;

      case _getwork                    :
        sprintf(tmp, "_getwork");
        break;

      case _getwork_seq                :
        sprintf(tmp, "_getwork_seq");
        break;

      case _sync                       :
        sprintf(tmp, "_sync");
        break;

#endif
#ifdef TABLING
#ifdef TABLING_INNER_CUTS
      case _clause_with_cut            :
        sprintf(tmp, "_clause_with_cut");
        break;

#endif
      case _table_load_answer          :
        sprintf(tmp, "_table_load_answer");
        break;

      case _table_try_answer           :
        sprintf(tmp, "_table_try_answer");
        break;

      case _table_try_single           :
        sprintf(tmp, "_table_try_single");
        break;

      case _table_try_me               :
        sprintf(tmp, "_table_try_me");
        break;

      case _table_try                  :
        sprintf(tmp, "_table_try");
        break;

      case _table_retry_me             :
        sprintf(tmp, "_table_retry_me");
        break;

      case _table_retry                :
        sprintf(tmp, "_table_retry");
        break;

      case _table_trust_me             :
        sprintf(tmp, "_table_trust_me");
        break;

      case _table_trust                :
        sprintf(tmp, "_table_trust");
        break;

      case _table_new_answer           :
        sprintf(tmp, "_table_new_answer");
        break;

      case _table_answer_resolution    :
        sprintf(tmp, "_table_answer_resolution");
        break;

      case _table_completion           :
        sprintf(tmp, "_table_completion");
        break;

#ifdef THREADS_CONSUMER_SHARING
      case _table_answer_resolution_completion:
        sprintf(tmp, "_table_answer_resolution_completion");
        break;

#endif
      case _trie_do_var                :
        sprintf(tmp, "_trie_do_var");
        break;

      case _trie_trust_var             :
        sprintf(tmp, "_trie_trust_var");
        break;

      case _trie_try_var               :
        sprintf(tmp, "_trie_try_var");
        break;

      case _trie_retry_var             :
        sprintf(tmp, "_trie_retry_var");
        break;

      case _trie_do_var_in_pair        :
        sprintf(tmp, "_trie_do_var_in_pair");
        break;

      case _trie_trust_var_in_pair     :
        sprintf(tmp, "_trie_trust_var_in_pair");
        break;

      case _trie_try_var_in_pair       :
        sprintf(tmp, "_trie_try_var_in_pair");
        break;

      case _trie_retry_var_in_pair     :
        sprintf(tmp, "_trie_retry_var_in_pair");
        break;

      case _trie_do_val                :
        sprintf(tmp, "_trie_do_val");
        break;

      case _trie_trust_val             :
        sprintf(tmp, "_trie_trust_val");
        break;

      case _trie_try_val               :
        sprintf(tmp, "_trie_try_val");
        break;

      case _trie_retry_val             :
        sprintf(tmp, "_trie_retry_val");
        break;

      case _trie_do_val_in_pair        :
        sprintf(tmp, "_trie_do_val_in_pair");
        break;

      case _trie_trust_val_in_pair     :
        sprintf(tmp, "_trie_trust_val_in_pair");
        break;

      case _trie_try_val_in_pair       :
        sprintf(tmp, "_trie_try_val_in_pair");
        break;

      case _trie_retry_val_in_pair     :
        sprintf(tmp, "_trie_retry_val_in_pair");
        break;

      case _trie_do_atom               :
        sprintf(tmp, "_trie_do_atom");
        break;

      case _trie_trust_atom            :
        sprintf(tmp, "_trie_trust_atom");
        break;

      case _trie_try_atom              :
        sprintf(tmp, "_trie_try_atom");
        break;

      case _trie_retry_atom            :
        sprintf(tmp, "_trie_retry_atom");
        break;

      case _trie_do_atom_in_pair       :
        sprintf(tmp, "_trie_do_atom_in_pair");
        break;

      case _trie_trust_atom_in_pair    :
        sprintf(tmp, "_trie_trust_atom_in_pair");
        break;

      case _trie_try_atom_in_pair      :
        sprintf(tmp, "_trie_try_atom_in_pair");
        break;

      case _trie_retry_atom_in_pair    :
        sprintf(tmp, "_trie_retry_atom_in_pair");
        break;

      case _trie_do_null               :
        sprintf(tmp, "_trie_do_null");
        break;

      case _trie_trust_null            :
        sprintf(tmp, "_trie_trust_null");
        break;

      case _trie_try_null              :
        sprintf(tmp, "_trie_try_null");
        break;

      case _trie_retry_null            :
        sprintf(tmp, "_trie_retry_null");
        break;

      case _trie_do_null_in_pair       :
        sprintf(tmp, "_trie_do_null_in_pair");
        break;

      case _trie_trust_null_in_pair    :
        sprintf(tmp, "_trie_trust_null_in_pair");
        break;

      case _trie_try_null_in_pair      :
        sprintf(tmp, "_tri_try_null_in_paire");
        break;

      case _trie_retry_null_in_pair    :
        sprintf(tmp, "_trie_retry_null_in_pair");
        break;

      case _trie_do_pair               :
        sprintf(tmp, "_trie_do_pair");
        break;

      case _trie_trust_pair            :
        sprintf(tmp, "_trie_trust_pair");
        break;

      case _trie_try_pair              :
        sprintf(tmp, "_trie_try_pair");
        break;

      case _trie_retry_pair            :
        sprintf(tmp, "_trie_retry_pair");
        break;

      case _trie_do_appl               :
        sprintf(tmp, "_trie_do_appl");
        break;

      case _trie_trust_appl            :
        sprintf(tmp, "_trie_trust_appl");
        break;

      case _trie_try_appl              :
        sprintf(tmp, "_trie_try_appl");
        break;

      case _trie_retry_appl            :
        sprintf(tmp, "_trie_retry_appl");
        break;

      case _trie_do_appl_in_pair       :
        sprintf(tmp, "_trie_do_appl_in_pair");
        break;

      case _trie_trust_appl_in_pair    :
        sprintf(tmp, "_trie_trust_appl_in_pair");
        break;

      case _trie_try_appl_in_pair      :
        sprintf(tmp, "_trie_trty_appkl_in_pair");
        break;

      case _trie_retry_appl_in_pair    :
        sprintf(tmp, "_trie_retry_appl_in_pair");
        break;

      case _trie_do_extension          :
        sprintf(tmp, "_trie_do_extension");
        break;

      case _trie_trust_extension       :
        sprintf(tmp, "_trie_trust_extension");
        break;

      case _trie_try_extension         :
        sprintf(tmp, "_trie_try_extension");
        break;

      case _trie_retry_extension       :
        sprintf(tmp, "_trie_retry_extension");
        break;

      case _trie_do_double             :
        sprintf(tmp, "_trie_do_double");
        break;

      case _trie_trust_double          :
        sprintf(tmp, "_trie_trust_double");
        break;

      case _trie_try_double            :
        sprintf(tmp, "_trie_try_double");
        break;

      case _trie_retry_double          :
        sprintf(tmp, "_trie_retry_double");
        break;

      case _trie_do_longint            :
        sprintf(tmp, "_trie_do_longint");
        break;

      case _trie_trust_longint         :
        sprintf(tmp, "_trie_trust_longint");
        break;

      case _trie_try_longint           :
        sprintf(tmp, "_trie_try_longint");
        break;

      case _trie_retry_longint         :
        sprintf(tmp, "_trie_retry_longint");
        break;

      case _trie_do_gterm              :
        sprintf(tmp, "_trie_do_gterm");
        break;

      case _trie_trust_gterm           :
        sprintf(tmp, "_trie_trust_gterm");
        break;

      case _trie_try_gterm             :
        sprintf(tmp, "_trie_try_gterm");
        break;

      case _trie_retry_gterm           :
        sprintf(tmp, "_trie_retry_gterm");
        break;

#endif
  /* this instruction is hardwired */
#ifdef YAPOR
      case _or_last                    :
        sprintf(tmp, "_or_last");
        break;

#else
      case _or_last                    :
        sprintf(tmp, "_or_last");
        break;

#endif
    default:
      tmp[0] = '\0';
      
    }
    strcpy(out, prepend_term);
    strcat(out, tmp);
    strcat(out, append_term);
  }
