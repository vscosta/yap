/*************************************************************************
*									 *
*	       BEAM -> Basic Extended Andorra Model                      *
*         BEAM extends the YAP Prolog system to support the EAM          *
*									 *
* Copyright: Ricardo Lopes and NCC - University of Porto, Portugal       *
*									 *
**************************************************************************
* comments:	abstract machine instructions                            *
*************************************************************************/

#define _exit_eam           0
#define _top_tree           1
#define _scheduler          2
#define _prepare_tries      3
#define _prepare_calls      4

#define _first_get _prepare_calls
#define _get_var_X_op       _first_get+1
#define _get_var_Y_op       _first_get+2
#define _get_val_X_op       _first_get+3
#define _get_val_Y_op       _first_get+4
#define _get_atom_op        _first_get+5
#define _get_list_op        _first_get+6
#define _get_struct_op      _first_get+7

#define _first_unify  _get_struct_op
#define _unify_void_op          _first_unify + 1
#define _unify_val_X_op         _first_unify + 2
#define _unify_val_Y_op         _first_unify + 3
#define _unify_var_X_op         _first_unify + 4
#define _unify_var_Y_op         _first_unify + 5
#define _unify_atom_op          _first_unify + 6
#define _unify_list_op          _first_unify + 7
#define _unify_last_list_op     _first_unify + 8
#define _unify_struct_op        _first_unify + 9
#define _unify_last_struct_op   _first_unify + 10
#define _unify_last_atom_op     _first_unify + 11
#define _unify_local_X_op       _first_unify + 12
#define _unify_local_Y_op       _first_unify + 13

#define _first_put _unify_local_Y_op
#define _put_var_X_op         _first_put + 1
#define _put_var_Y_op         _first_put + 2
#define _put_val_X_op         _first_put + 3
#define _put_val_Y_op         _first_put + 4
#define _put_atom_op          _first_put + 5
#define _put_list_op          _first_put + 6
#define _put_struct_op        _first_put + 7
#define _put_unsafe_op        _first_put + 8
#define _put_var_P_op         _first_put + 9

#define _first_write _put_var_P_op
#define _write_void           _first_write + 1
#define _write_var_X_op       _first_write + 2
#define _write_var_Y_op       _first_write + 3 
#define _write_val_X_op       _first_write + 4
#define _write_val_Y_op       _first_write + 5
#define _write_atom_op        _first_write + 6
#define _write_list_op        _first_write + 7
#define _write_struct_op      _first_write + 8
#define _write_last_list_op   _first_write + 9
#define _write_last_struct_op _first_write + 10
#define _write_local_X_op     _first_write + 11
#define _write_local_Y_op     _first_write + 12
#define _write_var_P_op       _first_write + 13 

#define _geral _write_var_P_op
#define _pop_op          _geral + 1
#define _jump_op         _geral + 2
#define _proceed_op      _geral + 3
#define _call_op         _geral + 4
#define _safe_call_op    _geral + 5
#define _safe_call_unary_op   _geral + 6
#define _safe_call_binary_op  _geral + 7
#define _only_1_clause_op     _geral + 8
#define _try_me_op            _geral + 9
#define _retry_me_op          _geral + 10
#define _trust_me_op          _geral + 11
#define _do_nothing_op        _geral + 12
#define _direct_safe_call_op  _geral + 13
#define _direct_safe_call_unary_op   _geral + 14
#define _direct_safe_call_binary_op  _geral + 15
#define _skip_while_var       _geral + 16
#define _wait_while_var       _geral + 17
#define _force_wait           _geral + 18
#define _write_call           _geral + 19
#define _is_call              _geral + 20
#define _equal_call           _geral + 21
#define _cut_op               _geral + 22
#define _commit_op            _geral + 23
#define _fail_op              _geral + 24

#define _others _fail_op
#define _save_b_X_op     _others + 1
#define _save_b_Y_op     _others + 2
#define _comit_b_X_op    _others + 3
#define _comit_b_Y_op    _others + 4
#define _save_appl_X_op  _others + 5
#define _save_appl_Y_op  _others + 6
#define _save_pair_X_op  _others + 7
#define _save_pair_Y_op  _others + 8
#define _either_op       _others + 9
#define _orelse_op       _others + 10
#define _orlast_op       _others + 11

#define	_std_base	_orlast_op
#define	_p_atom		(_std_base+1)
#define	_p_atomic	(_std_base+2)
#define _p_equal	(_std_base+3)
#define _p_integer	(_std_base+4)
#define	_p_nonvar	(_std_base+5)
#define _p_number	(_std_base+6)
#define _p_var		(_std_base+7)
#define _p_db_ref	(_std_base+8)
#define _p_primitive	(_std_base+9)
#define _p_cut_by	(_std_base+10)
#define _p_save_by	(_std_base+11)
#define _p_succ		(_std_base+12)
#define _p_predc	(_std_base+13)
#define _p_plus		(_std_base+14)
#define _p_minus	(_std_base+15)
#define _p_times	(_std_base+16)
#define _p_div		(_std_base+17)
#define _p_dif		(_std_base+18)
#define _p_eq		(_std_base+19)
#define _p_arg		(_std_base+20)
#define _p_functor	(_std_base+21)


