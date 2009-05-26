/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2006	 *
*									 *
**************************************************************************
*									 *
* File:		matrix.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	Have some fun with blobs				 *
*									 *
*************************************************************************/

/*
  A matrix is an object with integer or floating point numbers. A matrix
  may have a number of dimensions. These routines implement a number of
  routine manipulation procedures.

  matrix(Type,D1,D2,...,Dn,data(......))

  Type = int, float

  Operations:
  
typedef enum {
  MAT_SUM=0,
  MAT_SUB=1,
  MAT_TIMES=2,
  MAT_DIV=3,
  MAT_IDIV=4,
  MAT_ZDIV=5
} op_type;

  */

:- module( matrix,
	   [
	    matrix_new/3,
	    matrix_new/4,
	    matrix_new_set/4,
	    matrix_dims/2,
	    matrix_ndims/2,
	    matrix_size/2,
	    matrix_type/2,
	    matrix_to_list/2,
	    matrix_get/3,
	    matrix_set/3,
	    matrix_set_all/2,
	    matrix_add/3,
	    matrix_inc/2,
	    matrix_dec/2,
	    matrix_mult/2,
	    matrix_inc/3,
	    matrix_dec/3,
	    matrix_arg_to_offset/3,
	    matrix_offset_to_arg/3,
	    matrix_max/2,
	    matrix_maxarg/2,
	    matrix_min/2,
	    matrix_minarg/2,
	    matrix_sum/2,
	    matrix_sum_out/3,
	    matrix_sum_out_several/3,
	    matrix_sum_logs_out/3,
	    matrix_sum_logs_out_several/3,
	    matrix_add_to_all/2,
	    matrix_agg_lines/3,
	    matrix_agg_cols/3,
	    matrix_to_logs/1,
	    matrix_to_exps/1,
	    matrix_to_exps2/1,
	    matrix_to_logs/2,
	    matrix_to_exps/2,
	    matrix_op/4,
	    matrix_op_to_all/4,
	    matrix_op_to_lines/4,
	    matrix_op_to_cols/4,
	    matrix_shuffle/3,
	    matrix_transpose/2,
	    matrix_set_all_that_disagree/5,
	    matrix_expand/3,
	    matrix_select/4,
	    matrix_column/3
	    ]).

:- load_foreign_files([matrix], [], init_matrix).

matrix_new(ints,Dims,Matrix) :-
	length(Dims,NDims),
	new_ints_matrix_set(NDims, Dims, 0, Matrix).
matrix_new(floats,Dims,Matrix) :-
	length(Dims,NDims),
	new_floats_matrix_set(NDims, Dims, 0.0, Matrix).


matrix_new(ints,Dims,Data,Matrix) :-
	length(Dims,NDims),
	new_ints_matrix(NDims, Dims, Data, Matrix).
matrix_new(floats,Dims,Data,Matrix) :-
	length(Dims,NDims),
	new_floats_matrix(NDims, Dims, Data, Matrix).


matrix_new_set(ints,Dims,Elem,Matrix) :-
	length(Dims,NDims),
	new_ints_matrix_set(NDims, Dims, Elem, Matrix).
matrix_new_set(floats,Dims,Elem,Matrix) :-
	length(Dims,NDims),
	new_floats_matrix_set(NDims, Dims, Elem, Matrix).


matrix_type(Matrix,Type) :-
	matrix_type_as_number(Matrix, 0), !,
	Type = ints.
matrix_type(_,floats).

	
matrix_agg_lines(M1,+,NM) :-
	do_matrix_agg_lines(M1,0,NM).
/* other operations: *, logprod */

matrix_agg_cols(M1,+,NM) :-
	do_matrix_agg_cols(M1,0,NM).
/* other operations: *, logprod */

matrix_op(M1,M2,+,NM) :-
	do_matrix_op(M1,M2,0,NM).
matrix_op(M1,M2,-,NM) :-
	do_matrix_op(M1,M2,1,NM).
matrix_op(M1,M2,*,NM) :-
	do_matrix_op(M1,M2,2,NM).
matrix_op(M1,M2,/,NM) :-
	do_matrix_op(M1,M2,3,NM).
matrix_op(M1,M2,zdiv,NM) :-
	do_matrix_op(M1,M2,5,NM).

matrix_op_to_all(M1,+,Num,NM) :-
	do_matrix_op_to_all(M1,0,Num,NM).
matrix_op_to_all(M1,*,Num,NM) :-
	do_matrix_op_to_all(M1,2,Num,NM).
matrix_op_to_all(M1,/,Num,NM) :-
	% can only use floats.
	FNum is float(Num),
	do_matrix_op_to_all(M1,3,FNum,NM).
/* other operations: *, logprod */

matrix_op_to_lines(M1,M2,/,NM) :-
	do_matrix_op_to_lines(M1,M2,3,NM).
/* other operations: *, logprod */

matrix_op_to_cols(M1,M2,+,NM) :-
	do_matrix_op_to_cols(M1,M2,0,NM).
/* other operations: *, logprod */


matrix_transpose(M1,M2) :-
	matrix_shuffle(M1,[1,0],M2).


