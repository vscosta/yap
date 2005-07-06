/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		YapOpcodes.h						 *
* comments:	Central Table with all YAP opcodes                       *
*									 *
* Last rev:     $Date: 2005-07-06 19:34:11 $							 *
* $Log: not supported by cvs2svn $
* Revision 1.31  2005/07/06 15:10:15  vsc
* improvements to compiler: merged instructions and fixes for ->
*
* Revision 1.30  2005/06/04 07:26:43  ricroc
* long int support for tabling
*
* Revision 1.29  2005/06/03 08:18:25  ricroc
* float support for tabling
*
* Revision 1.28  2005/06/01 20:25:23  vsc
* == and \= should not need a choice-point in ->
*
* Revision 1.27  2005/06/01 14:02:52  vsc
* get_rid of try_me?, retry_me? and trust_me? instructions: they are not
* significantly used nowadays.
*
* Revision 1.26  2005/04/10 04:01:13  vsc
* bug fixes, I hope!
*
* Revision 1.25  2004/10/26 20:16:04  vsc
* More bug fixes for overflow handling
*
* Revision 1.24  2004/09/27 20:45:04  vsc
* Mega clauses
* Fixes to sizeof(expand_clauses) which was being overestimated
* Fixes to profiling+indexing
* Fixes to reallocation of memory after restoring
* Make sure all clauses, even for C, end in _Ystop
* Don't reuse space for Streams
* Fix Stream_F on StreaNo+1
*
* Revision 1.23  2004/03/31 01:03:10  vsc
* support expand group of clauses
*
* Revision 1.22  2004/03/19 11:35:42  vsc
* trim_trail for default machine
* be more aggressive about try-retry-trust chains.
*    - handle cases where block starts with a wait
*    - don't use _killed instructions, just let the thing rot by itself.
*
* Revision 1.21  2004/03/10 14:59:55  vsc
* optimise -> for type tests
*									 *
*									 *
*************************************************************************/
    OPCODE(Ystop                   ,e),
    OPCODE(Nstop                   ,e),               
    OPCODE(execute                 ,l),		
    OPCODE(call                    ,sla),
    OPCODE(procceed                ,e),		
    OPCODE(allocate                ,e),		
    OPCODE(deallocate              ,e),	
    OPCODE(op_fail                 ,e),	
#ifdef YAPOR
    OPCODE(getwork_first_time      ,e),
    OPCODE(getwork                 ,ld),
    OPCODE(getwork_seq             ,ld),
    OPCODE(sync                    ,ld),
#endif /* YAPOR */
#ifdef TABLING_INNER_CUTS
    OPCODE(clause_with_cut         ,e),
#endif /* TABLING_INNER_CUTS */
#ifdef TABLING
    OPCODE(table_try_single        ,ld),
    OPCODE(table_try_me            ,ld),
    OPCODE(table_try	           ,ld),
    OPCODE(table_retry_me          ,ld),
    OPCODE(table_retry	           ,ld),
    OPCODE(table_trust_me          ,ld),
    OPCODE(table_trust             ,ld),
    OPCODE(table_new_answer        ,s),
    OPCODE(table_answer_resolution ,ld),
    OPCODE(table_completion        ,ld),
    OPCODE(table_load_answer       ,ld),

    OPCODE(trie_do_null            ,e),
    OPCODE(trie_trust_null         ,e),
    OPCODE(trie_try_null           ,e),
    OPCODE(trie_retry_null         ,e),
    OPCODE(trie_do_var             ,e),
    OPCODE(trie_trust_var          ,e),
    OPCODE(trie_try_var            ,e),
    OPCODE(trie_retry_var          ,e),
    OPCODE(trie_do_val             ,e),
    OPCODE(trie_trust_val          ,e),
    OPCODE(trie_try_val            ,e),
    OPCODE(trie_retry_val          ,e),
    OPCODE(trie_do_atom            ,e),
    OPCODE(trie_trust_atom         ,e),
    OPCODE(trie_try_atom           ,e),
    OPCODE(trie_retry_atom         ,e),
    OPCODE(trie_do_list            ,e),
    OPCODE(trie_trust_list         ,e),
    OPCODE(trie_try_list           ,e),
    OPCODE(trie_retry_list         ,e),
    OPCODE(trie_do_struct          ,e),
    OPCODE(trie_trust_struct       ,e),
    OPCODE(trie_try_struct         ,e),
    OPCODE(trie_retry_struct       ,e),
    OPCODE(trie_do_extension       ,e),
    OPCODE(trie_trust_extension    ,e),
    OPCODE(trie_try_extension      ,e),
    OPCODE(trie_retry_extension    ,e),
    OPCODE(trie_do_float           ,e),
    OPCODE(trie_trust_float        ,e),
    OPCODE(trie_try_float          ,e),
    OPCODE(trie_retry_float        ,e),
    OPCODE(trie_do_long            ,e),
    OPCODE(trie_trust_long         ,e),
    OPCODE(trie_try_long           ,e),
    OPCODE(trie_retry_long         ,e),
#endif /* TABLING */
    OPCODE(try_me                  ,ld),	
    OPCODE(retry_me                ,ld),	
    OPCODE(trust_me                ,ld),	
    OPCODE(try_and_mark            ,ld),
    OPCODE(retry_and_mark          ,ld),
    OPCODE(try_c                   ,lds),		
    OPCODE(retry_c                 ,lds),		
    OPCODE(try_userc               ,lds),		
    OPCODE(retry_userc             ,lds),		
    OPCODE(cut                     ,e),		
    OPCODE(get_x_var               ,xx),		
    OPCODE(get_y_var               ,yx),		
    OPCODE(get_x_val               ,xx),		
    OPCODE(get_y_val               ,yx),		
    OPCODE(get_atom                ,xc),		
    OPCODE(get_2atoms              ,cc),	  /* peephole */
    OPCODE(get_3atoms              ,ccc),	  /* peephole */
    OPCODE(get_4atoms              ,cccc),	  /* peephole */
    OPCODE(get_5atoms              ,ccccc),	  /* peephole */
    OPCODE(get_6atoms              ,cccccc),	  /* peephole */
    OPCODE(get_float               ,xc),		
    OPCODE(get_longint             ,xc),		
    OPCODE(get_bigint              ,xc),		
    OPCODE(get_list                ,x),		
    OPCODE(get_struct              ,xf),		
    OPCODE(unify_x_var             ,ox),	
    OPCODE(unify_y_var             ,oy),	
    OPCODE(unify_x_val             ,ox),	
    OPCODE(unify_y_val             ,oy),	
    OPCODE(unify_atom              ,oc),		
    OPCODE(unify_float             ,oc),		
    OPCODE(unify_longint           ,oc),		
    OPCODE(unify_bigint            ,oc),		
    OPCODE(unify_list              ,o),		
    OPCODE(unify_struct            ,of),	
    OPCODE(put_x_var               ,xx),		
    OPCODE(put_y_var               ,yx),		
    OPCODE(put_x_val               ,xx),		
    OPCODE(put_y_val               ,yx),		
    OPCODE(put_unsafe              ,yx),		
    OPCODE(put_xx_val              ,xxxx), /* peephole */
    OPCODE(put_atom                ,xc),		
    OPCODE(put_list                ,x),		
    OPCODE(put_struct              ,xf),		
    OPCODE(write_x_var             ,x),	
    OPCODE(write_y_var             ,y),	
    OPCODE(write_x_val             ,x),	
    OPCODE(write_y_val             ,y),	
    OPCODE(write_atom              ,c),		
    OPCODE(write_list              ,e),		
    OPCODE(write_struct            ,f),	
    OPCODE(pop                     ,e),		
    OPCODE(pop_n                   ,s),		
    OPCODE(jump                    ,l),		
    OPCODE(move_back               ,l), 
    OPCODE(skip                    ,l), 
    OPCODE(either                  ,sla),
    OPCODE(or_else                 ,sla),
#ifdef YAPOR
    OPCODE(or_last                 ,sla),
#else
    OPCODE(or_last                 ,p),
#endif /* YAPOR */
    OPCODE(call_cpred              ,sla),		
    OPCODE(call_usercpred          ,sla),
    OPCODE(call_c_wfail            ,sdl),
    OPCODE(call_bfunc_xx           ,lxx),
    OPCODE(call_bfunc_xy           ,lxy),
    OPCODE(call_bfunc_yx           ,lxy),
    OPCODE(call_bfunc_yy           ,lyy),
    OPCODE(cut_t                   ,e),		
    OPCODE(cut_e                   ,sla),	
    OPCODE(try_clause              ,ld),		
    OPCODE(try_clause2             ,l),		
    OPCODE(try_clause3             ,l),		
    OPCODE(try_clause4             ,l),		
    OPCODE(retry                   ,ld),		
    OPCODE(retry2                  ,l),		
    OPCODE(retry3                  ,l),		
    OPCODE(retry4                  ,l),		
    OPCODE(trust                   ,ld),		
    OPCODE(try_in                  ,l),
    OPCODE(jump_if_var             ,l),	
    OPCODE(jump_if_nonvar          ,xll),	
    OPCODE(switch_on_cons          ,sssl),	
    OPCODE(switch_on_type          ,llll),	
    OPCODE(switch_list_nl          ,ollll),
    OPCODE(switch_on_arg_type      ,xllll),
    OPCODE(switch_on_sub_arg_type  ,sllll),	
    OPCODE(go_on_cons              ,sssl),		
    OPCODE(if_cons                 ,sssl),		
    OPCODE(switch_on_func          ,sssl),	
    OPCODE(go_on_func              ,sssl),		
    OPCODE(if_func                 ,sssl),		
    OPCODE(if_not_then             ,cll),	
    OPCODE(index_dbref             ,e),	
    OPCODE(index_blob              ,e),	
    OPCODE(trust_fail              ,e),		
    OPCODE(index_pred              ,e),
    OPCODE(expand_index            ,e),		
    OPCODE(expand_clauses          ,sp),
    OPCODE(save_b_x                ,x),		
    OPCODE(save_b_y                ,y),		
    OPCODE(commit_b_x              ,x),		
    OPCODE(commit_b_y              ,y),		
    OPCODE(undef_p                 ,e),		
    OPCODE(spy_pred                ,e),		
    OPCODE(spy_or_trymark          ,ld),	
    OPCODE(unify_void              ,o),		
    OPCODE(write_void              ,e),		
    OPCODE(save_pair_x             ,ox),		
    OPCODE(save_pair_y             ,oy),		
    OPCODE(save_appl_x             ,ox),		
    OPCODE(save_appl_y             ,oy),		
    OPCODE(unify_n_atoms           ,osc),	
    OPCODE(write_n_atoms           ,sc),	
    OPCODE(unify_n_voids           ,os),	
    OPCODE(write_n_voids           ,s),	
    OPCODE(glist_valx              ,ss),       /* peephole */
    OPCODE(glist_valy              ,xy),       /* peephole */
    OPCODE(fcall                   ,sla),		
    OPCODE(dexecute                ,l),			
    OPCODE(gl_void_varx            ,xx),       /* peephole */
    OPCODE(gl_void_vary            ,xy),       /* peephole */
    OPCODE(gl_void_valx            ,xx),       /* peephole */
    OPCODE(gl_void_valy            ,xy),       /* peephole */
    OPCODE(unify_x_loc             ,ox),	
    OPCODE(unify_y_loc             ,oy),	
    OPCODE(write_x_loc             ,ox),	
    OPCODE(write_y_loc             ,oy),	
    OPCODE(unify_x_var2            ,oxx),	
    OPCODE(unify_l_struc           ,of),	
    OPCODE(unify_l_list            ,o),	
    OPCODE(write_l_struc           ,f),	
    OPCODE(write_l_list            ,e),	
    OPCODE(unify_l_x_var           ,ox),	
    OPCODE(unify_l_y_var           ,oy),	
    OPCODE(unify_l_x_val           ,ox),	
    OPCODE(unify_l_y_val           ,oy),	
    OPCODE(unify_l_atom            ,oc),	
    OPCODE(unify_l_float           ,oc),	
    OPCODE(unify_l_longint         ,oc),	
    OPCODE(unify_l_bigint          ,oc),	
    OPCODE(unify_l_void            ,o),	
    OPCODE(unify_l_n_voids         ,os),	
    OPCODE(unify_l_x_loc           ,ox),	
    OPCODE(unify_l_y_loc           ,oy),	
    OPCODE(unify_l_x_var2          ,oxx),	
    OPCODE(unify_x_var_write       ,ox),	
    OPCODE(unify_y_var_write       ,oy),	
    OPCODE(unify_x_val_write       ,ox),	
    OPCODE(unify_y_val_write       ,oy),	
    OPCODE(unify_atom_write        ,oc),	
    OPCODE(unify_n_atoms_write     ,osc),
    OPCODE(unify_list_write        ,o),	
    OPCODE(unify_x_var2_write      ,oxx),	
    OPCODE(unify_struct_write      ,of),	
    OPCODE(unify_void_write        ,o),	
    OPCODE(unify_n_voids_write     ,os),
    OPCODE(unify_x_loc_write       ,ox),	
    OPCODE(unify_y_loc_write       ,oy),	
    OPCODE(unify_l_x_var_write     ,ox),
    OPCODE(unify_l_y_var_write     ,oy),
    OPCODE(unify_l_x_val_write     ,ox),
    OPCODE(unify_l_y_val_write     ,oy),
    OPCODE(unify_l_atom_write      ,oc),	
    OPCODE(unify_l_void_write      ,o),	
    OPCODE(unify_l_n_voids_write   ,os),
    OPCODE(unify_l_x_loc_write     ,ox),	
    OPCODE(unify_l_y_loc_write     ,oy),	
    OPCODE(unify_l_x_var2_write    ,oxx),	
    OPCODE(unify_l_list_write      ,o),		
    OPCODE(unify_l_struc_write     ,of),	
    OPCODE(save_pair_x_write       ,ox),		
    OPCODE(save_pair_y_write       ,oy),		
    OPCODE(save_appl_x_write       ,ox),		
    OPCODE(save_appl_y_write       ,oy),		
    OPCODE(enter_profiling         ,l),
    OPCODE(enter_a_profiling       ,e),
    OPCODE(retry_profiled          ,l),
    OPCODE(profiled_retry_me       ,ld),
    OPCODE(profiled_trust_me       ,ld),
    OPCODE(profiled_retry_and_mark ,ld),
    OPCODE(count_call              ,l),
    OPCODE(count_a_call            ,e),
    OPCODE(count_retry             ,l),
    OPCODE(count_retry_me          ,ld),
    OPCODE(count_trust_me          ,ld),
    OPCODE(count_retry_and_mark    ,ld),
    OPCODE(lock_lu	           ,p),
    OPCODE(unlock_lu	           ,e),
    OPCODE(enter_lu_pred           ,Ill),
    OPCODE(stale_lu_index          ,Ill),
    OPCODE(trust_logical_pred      ,l),
    OPCODE(alloc_for_logical_pred  ,EC),
    OPCODE(unify_idb_term          ,e),
    OPCODE(copy_idb_term           ,e),
#if THREADS
    OPCODE(thread_local            ,e),
#endif
#ifdef SFUNC
    OPCODE(get_s_f                 ,),
    OPCODE(put_s_f                 ,),
    OPCODE(unify_s_f               ,),
    OPCODE(write_s_f               ,),
    OPCODE(unify_s_xvar            ,),
    OPCODE(unify_s_yvar            ,),
    OPCODE(write_s_xvar            ,),
    OPCODE(write_s_yvar            ,),
    OPCODE(unify_s_xval            ,),
    OPCODE(unify_s_yval            ,),
    OPCODE(write_s_xval            ,),
    OPCODE(write_s_yval            ,),
    OPCODE(unify_s_a               ,),
    OPCODE(write_s_a               ,),
    OPCODE(get_s_end               ,),
    OPCODE(put_s_end               ,),
    OPCODE(unify_s_end             ,),
    OPCODE(write_s_end             ,),
#endif /* SFUNC */
    OPCODE(p_atom_x                ,xF),
    OPCODE(p_atom_y                ,yF),
    OPCODE(p_atomic_x              ,xF),			
    OPCODE(p_atomic_y              ,yF),			
    OPCODE(p_integer_x             ,xF),			
    OPCODE(p_integer_y             ,yF),			
    OPCODE(p_nonvar_x              ,xF),			
    OPCODE(p_nonvar_y              ,yF),			
    OPCODE(p_number_x              ,xF),			
    OPCODE(p_number_y              ,yF),			
    OPCODE(p_var_x                 ,xF),			
    OPCODE(p_var_y                 ,yF),			
    OPCODE(p_compound_x            ,xF),			
    OPCODE(p_compound_y            ,yF),			
    OPCODE(p_float_x	           ,xF),			
    OPCODE(p_float_y	           ,yF),			
    OPCODE(p_db_ref_x              ,xF),			
    OPCODE(p_db_ref_y              ,yF),			
    OPCODE(p_cut_by_x              ,xF),			
    OPCODE(p_cut_by_y              ,yF),			
    OPCODE(p_primitive_x           ,xF),		
    OPCODE(p_primitive_y           ,yF),		
    OPCODE(p_equal                 ,e),			
    OPCODE(p_dif                   ,l),			
    OPCODE(p_eq                    ,l),			
    OPCODE(p_functor               ,e),
    OPCODE(p_plus_vv               ,xxx),
    OPCODE(p_plus_vc               ,xxc),
    OPCODE(p_plus_y_vv             ,yxx),
    OPCODE(p_plus_y_vc             ,yxc),
    OPCODE(p_minus_vv              ,xxx),
    OPCODE(p_minus_cv              ,xcx),
    OPCODE(p_minus_y_vv            ,yxx),
    OPCODE(p_minus_y_cv            ,ycx),
    OPCODE(p_times_vv              ,xxx),
    OPCODE(p_times_vc              ,xxc),
    OPCODE(p_times_y_vv            ,yxx),
    OPCODE(p_times_y_vc            ,yxc),
    OPCODE(p_div_vv                ,xxx),
    OPCODE(p_div_cv                ,xcx),
    OPCODE(p_div_vc                ,xxc),
    OPCODE(p_div_y_vv              ,yxx),
    OPCODE(p_div_y_cv              ,ycx),
    OPCODE(p_div_y_vc              ,yxc),
    OPCODE(p_and_vv                ,xxx),
    OPCODE(p_and_vc                ,xxc),
    OPCODE(p_and_y_vv              ,yxx),
    OPCODE(p_and_y_vc              ,yxc),
    OPCODE(p_or_vv                 ,xxx),
    OPCODE(p_or_vc                 ,xxc),
    OPCODE(p_or_y_vv               ,yxx),
    OPCODE(p_or_y_vc               ,yxc),
    OPCODE(p_sll_vv                ,xxx),
    OPCODE(p_sll_cv                ,xcx),
    OPCODE(p_sll_vc                ,xxc),
    OPCODE(p_sll_y_vv              ,yxx),
    OPCODE(p_sll_y_cv              ,ycx),
    OPCODE(p_sll_y_vc              ,yxc),
    OPCODE(p_slr_vv                ,xcx),
    OPCODE(p_slr_vc                ,xxc),
    OPCODE(p_slr_cv                ,xcx),
    OPCODE(p_slr_y_vv              ,yxx),
    OPCODE(p_slr_y_vc              ,yxc),
    OPCODE(p_slr_y_cv              ,ycx),
    OPCODE(p_arg_vv                ,xxx),
    OPCODE(p_arg_cv                ,xxc),
    OPCODE(p_arg_y_vv              ,yxx),
    OPCODE(p_arg_y_cv              ,yxc),
    OPCODE(p_func2s_vv             ,xxx),
    OPCODE(p_func2s_cv             ,xcx),
    OPCODE(p_func2s_vc             ,xxc),
    OPCODE(p_func2s_y_vv           ,xxx),
    OPCODE(p_func2s_y_cv           ,xcx),
    OPCODE(p_func2s_y_vc           ,xxc),
    OPCODE(p_func2f_xx             ,xxx),
    OPCODE(p_func2f_xy             ,xyx),
    OPCODE(p_func2f_yx             ,yxx),
    OPCODE(p_func2f_yy             ,yyx),
    OPCODE(p_execute               ,sla),
    OPCODE(p_execute_tail          ,e)


