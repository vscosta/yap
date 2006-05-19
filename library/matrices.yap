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
* File:		matrices.yap						 *
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
  */

:- module( matrices,
	   [
	    matrix_new/4,
	    matrix_dim/2,
	    matrix_type/2,
	    matrix_max/2,
	    matrix_add/3]).

:- use_module(library_lists,
	      [append/3]).

matrix_new(Type,Dims,Data,Matrix) :-
	PackedData =.. [data|Data],
	append([Type,Dims],PackedData,MatInfo),
	Matrix =.. [matrix|MatInfo].

matrix_dim(Matrix,Dim) :-
	functor(Matrix,matrix,Arity),
	Dim is Arity-2.

matrix_type(Matrix,Type) :-
	arg(1, Matrix, Type).
	Dim is Arity-2.

matrix_max(Matrix,Max) :-
	arg(1, Matrix, Type).
	typed_matrix_max(Type, Matrix, Max).

typed_matrix_max(int, Matrix, Max) :-
	int_max_of_matrix(Matrix, Max, _).
typed_matrix_max(float, Matrix, Max) :-
	float_max_of_matrix(Matrix, Max, _).
	

matrix_maxarg(Matrix,Max) :-
	arg(1, Matrix, Type).
	typed_matrix_maxarg(Type, Matrix, Max).

typed_matrix_maxarg(int, Matrix, Max) :-
	int_max_of_matrix(Matrix, _, Max).
typed_matrix_maxarg(float, Matrix, Max) :-
	float_max_of_matrix(Matrix, _, Max).
	

matrix_min(Matrix,Min) :-
	arg(1, Matrix, Type).
	typed_matrix_min(Type, Matrix, Min).

typed_matrix_min(int, Matrix, Min) :-
	int_min_of_matrix(Matrix, Min, _).
typed_matrix_min(float, Matrix, Min) :-
	float_min_of_matrix(Matrix, Min, _).
	

matrix_minarg(Matrix,Min) :-
	arg(1, Matrix, Type).
	typed_matrix_minarg(Type, Matrix, Min).

typed_matrix_minarg(int, Matrix, Min) :-
	int_min_of_matrix(Matrix, _, Min).
typed_matrix_minarg(float, Matrix, Min) :-
	float_min_of_matrix(Matrix, _, Min).
	




