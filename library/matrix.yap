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

  '$matrix'(Type,D1,D2,...,Dn,data(......))

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
	   [op(100, yf, []),
            (<==)/2, op(600, xfx, '<=='),
	    op(700, xfx, in),
	    op(700, xfx, ins),
            op(450, xfx, ..), % should bind more tightly than \/
	    op(710, xfx, of), of/2,
	    matrix_new/3,
	    matrix_new/4,
	    matrix_new_set/4,
	    matrix_dims/2,
	    matrix_ndims/2,
	    matrix_size/2,
	    matrix_type/2,
	    matrix_to_list/2,
	    matrix_to_lists/2,
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
	    matrix_column/3,
	    matrix_get/2,
	    matrix_set/2,
	    for/2,
	    for/4,
	    op(100, fy, '[]')
	    ]).

:- load_foreign_files([matrix], [], init_matrix).

:- multifile rhs_opaque/1, array_extension/2.

:- meta_predicate for(+,0), for(+,2, +, -).

:- use_module(library(maplist)).
:- use_module(library(mapargs)).
:- use_module(library(lists)).

( X = '[]'(Dims0, array) of V ) :-
	var(V), !,
	foldl( norm_dim, Dims0, Dims, Bases, 1, Size ),
	length( L, Size ),
	X <== matrix( L, [dim=Dims,base=Bases] ).
( X = '[]'(Dims0, array) of ints ) :- !,
	foldl( norm_dim, Dims0, Dims, Bases, 1, _Size ),
	matrix_new( ints , Dims, X ),
	matrix_base(X, Bases).
( X = '[]'(Dims0, array) of floats ) :- !,
	foldl( norm_dim, Dims0, Dims, Bases, 1, _Size ),
	matrix_new( floats , Dims, X ),
	matrix_base(X, Bases).
( X = '[]'(Dims0, array) of (I:J) ) :- !,
	foldl( norm_dim, Dims0, Dims, Bases, 1, Size ),
	matrix_seq(I, J, Dims, X),
	matrixn_size(X, Size),
	matrix_base(X, Bases).
( X = '[]'(Dims0, array) of L ) :-
	length( L, Size ), !,
	foldl( norm_dim, Dims0, Dims, Bases, 1, Size ),
	X <== matrix( L, [dim=Dims,base=Bases] ).
( X = '[]'(Dims0, array) of Pattern ) :-
	array_extension(Pattern, Goal),
	foldl( norm_dim, Dims0, Dims, Bases, 1, Size ),
	call(Goal, Pattern, Dims, Size, L),
	X <== matrix( L, [dim=Dims,base=Bases] ).


norm_dim( I..J, D, I, P0, P) :- !,
	D is J+1-I,
	P is P0*D.
norm_dim( I, I, 0, P0, P ) :-
	P is P0*I.


( LHS <== RHS ) :-
	rhs(RHS, R),
	set_lhs( LHS, R).

rhs(RHS, RHS) :- var(RHS), !.
% base case
rhs(A, A) :- atom(A), !.
rhs(RHS, RHS) :- number(RHS), !.
rhs(RHS, RHS) :- opaque(RHS), !.
rhs(RHS, RHS) :- RHS = '$matrix'(_, _, _, _, _), !.
rhs(matrix(List), RHS) :- !,
	rhs( List, A1),
	new_matrix(A1, [], RHS).
rhs(matrix(List, Opt1), RHS) :- !,
	rhs( List, A1),
	new_matrix(A1, Opt1, RHS).
rhs(matrix(List, Opt1, Opt2), RHS) :- !,
	rhs( List, A1),
	new_matrix(A1, [Opt1, Opt2], RHS).
rhs(dim(RHS), Dims) :- !,
	rhs(RHS, X1),
	matrix_dims( X1, Dims ).
rhs(dims(RHS), Dims) :- !,
	rhs(RHS, X1),
	matrix_dims( X1, Dims ).
rhs(nrow(RHS), NRow) :- !,
	rhs(RHS, X1),
	matrix_dims( X1, [NRow,_] ).
rhs(ncol(RHS), NCol) :- !,
	rhs(RHS, X1),
	matrix_dims( X1, [_,NCol] ).
rhs(length(RHS), Size) :- !,
	rhs(RHS, X1),
	matrix_size( X1, Size ).
rhs(size(RHS), Size) :- !,
	rhs(RHS, X1),
	matrix_size( X1, Size ).
rhs(max(RHS), Size) :- !,
	rhs(RHS, X1),
	matrix_max( X1, Size ).
rhs(min(RHS), Size) :- !,
	rhs(RHS, X1),
	matrix_min( X1, Size ).
rhs(maxarg(RHS), Size) :- !,
	rhs(RHS, X1),
	matrix_maxarg( X1, Size ).
rhs(minarg(RHS), Size) :- !,
	rhs(RHS, X1),
	matrix_minarg( X1, Size ).
rhs(list(RHS), List) :- !,
	rhs(RHS, X1),
	matrix_to_list( X1, List ).
rhs(lists(RHS), List) :- !,
	rhs(RHS, X1),
	matrix_to_lists( X1, List ).
rhs('[]'(Args, RHS), Val) :-
	!,
	rhs(RHS, X1),
	matrix_dims( X1, Dims, Bases),
	maplist( index(Range), Args, Dims, Bases, NArgs),
	(
	 var(Range)
	->
	  matrix_get( X1, NArgs, Val )
	;
	  matrix_get_range( X1, NArgs, Val )
	).
rhs('..'(I, J), [I1|Is]) :- !,
	rhs(I, I1),
	rhs(J, J1),
	once( foldl(inc, Is, I1, J1) ).
rhs([H|T], [NH|NT]) :- !,
	rhs(H, NH),
	rhs(T, NT).
rhs(log(RHS), Logs ) :- !,
	rhs(RHS, X1),
	matrix_to_logs( X1, Logs ).
rhs(exp(RHS), Logs ) :- !,
	rhs(RHS, X1),
	matrix_to_exps( X1, Logs ).
rhs(S, NS) :-
	rhs_opaque( S ), !,
	S = NS.
rhs(E1+E2, V) :- !,
	rhs(E1, R1),
	rhs(E2, R2),
	mplus(R1, R2, V).
rhs(E1-E2, V) :- !,
	rhs(E1, R1),
	rhs(E2, R2),
	msub(R1, R2, V).
rhs(S, NS) :-
	S =.. [N|As],
	maplist(rhs, As, Bs),
	NS =.. [N|Bs].

set_lhs(V, R) :- var(V), !, V = R.
set_lhs(V, R) :- number(V), !, V = R.
set_lhs('[]'(Args, M), Val) :-
	matrix_dims( M, Dims, Bases),
	maplist( index(Range), Args, Dims, Bases, NArgs),
	(
	 var(Range)
	->
	  matrix_set( M, NArgs, Val )
	;
	  matrix_set_range( M, NArgs, Val )
	).

%
% ranges of arguments
%
index(Range, V, M, Base, Indx) :- var(V), !,
	Max is (M-1)+Base,
	index(Range, Base..Max, M, Base, Indx).
index(Range, '*', M, Base, Indx) :- !,
	Max is (M-1)+Base,
	index(Range, Base..Max, M, Base, Indx).
index(Range, Exp, M, _Base, Indx) :- !,
	index(Exp, M, Indx0),
	( integer(Indx0) -> Indx = Indx0 ;
	  Indx0 = [Indx] -> true ;
	  Indx0 = Indx, Range = range ).

index(I, _M, I ) :- integer(I), !.
index(I..J, _M, [I|O] ) :- !,
	I1 is I, J1 is J,
	once( foldl(inc, O, I1, J1) ).
index(I:J, _M, [I|O] ) :- !,
	I1 is I, J1 is J,
	once( foldl(inc, O, I1, J1) ).
index(I+J, _M, O ) :- !,
	index(I, M, I1),
	index(J, M, J1),
	add_index(I1, J1, O).
index(I-J, _M, O ) :- !,
	index(I, M, I1),
	index(J, M, J1),
	sub_index(I1, J1, O).
index(I*J, _M, O ) :- !,
	index(I, M, I1),
	index(J, M, J1),
	O is I1*J1.
index(I div J, _M, O ) :- !,
	index(I, M, I1),
	index(J, M, J1),
	O is I1 div J1.
index(I rem J, _M, O ) :- !,
	index(I, M, I1),
	index(J, M, J1),
	O is I1 rem J1.
index(I, M, NI ) :-
	maplist(indx(M), I, NI).

indx(M, I, NI) :- index(I, M, NI).

add_index(I1, J1, O) :-
	integer(I1),
	integer(J1), !,
	O is I1+J1.
add_index(I1, J1, O) :-
	integer(I1), !,
	maplist(plus(I1), J1, O).
add_index(I1, J1, O) :-
	integer(J1), !,
	maplist(plus(J1), I1, O).
add_index(I1, J1, O) :-
	ord_union(I1, J1, O).

sub_index(I1, J1, O) :-
	integer(I1),
	integer(J1), !,
	O is I1-J1.
sub_index(I1, J1, O) :-
	integer(I1), !,
	maplist(rminus(I1), J1, O).
sub_index(I1, J1, O) :-
	integer(J1), !,
	maplist(minus(J1), I1, O).
sub_index(I1, J1, O) :-
	ord_subtract(I1, J1, O).

minus(X, Y, Z) :- Z is X-Y.

rminus(X, Y, Z) :- Z is Y-X.

times(X, Y, Z) :- Z is Y*X.

div(X, Y, Z) :- Z is X/Y.

rdiv(X, Y, Z) :- Z is Y/X.

zdiv(X, Y, Z) :- (X == 0 -> Z = 0 ; X == 0.0 -> Z = 0.0 ; Z is X / Y ).

mplus(I1, I2, V) :-
	number(I1) ->
	  ( number(I2) -> V is I1+I2 ;
	    '$matrix'(I2) -> matrix_op_to_all(I1, +, I2, V) ;
	    is_list(I2) ->  maplist(plus(I1), I2, V) ;
	    V = I1+I2 ) ;
	 matrix(I1) ->
	    ( number(I2) -> matrix_op_to_all(I1, +, I2, V) ;
	      '$matrix'(I2) ->  matrix_op(I1, I2, +, V) ;
	      V = I1+I2 ) ;
	 is_list(I1) ->
	    ( number(I2) -> maplist(plus(I2), I1, V) ;
	      is_list(I2) ->  maplist(plus, I1, I2, V) ;
	      V = I1+I2 ) ;
	    V = I1 +I2.

msub(I1, I2, V) :-
	number(I1) ->
	  ( number(I2) -> V is I1-I2 ;
	    matrix(I2) -> matrix_op_to_all(I1, -, NI2, V) ;
	    is_list(I2) ->  maplist(minus(I1), I2, V) ;
	    V = I1-I2 ) ;
	 matrix(I1) ->
	    ( number(I2) -> NI2 is -I2, matrix_op_to_all(I1, +, NI2, V) ;
	      matrix(I2) ->  matrix_op(I1, I2, -, V) ;
	      V = I1-I2 ) ;
	 is_list(I1) ->
	    ( number(I2) -> NI2 is -I2, maplist(plus(NI2), I1, V) ;
	      is_list(I2) ->  maplist(minus, I1, I2, V) ;
	      V = I1-I2 ) ;
	    V = I1-I2.


mtimes(I1, I2, V) :-
	number(I1) ->
	  ( number(I2) -> V is I1*I2 ;
	    matrix(I2) -> matrix_op_to_all(I1, *, I2, V) ;
	    is_list(I2) ->  maplist(times(I1), I2, V) ;
	    V = I1*I2 ) ;
	 matrix(I1) ->
	    ( number(I2) -> matrix_op_to_all(I1, *, I2, V) ;
	      matrix(I2) ->  matrix_op(I1, I2, *, V) ;
	      V = I1*I2 ) ;
	 is_list(I1) ->
	    ( number(I2) -> maplist(times(I2), I1, V) ;
	      is_list(I2) ->  maplist(times, I1, I2, V) ;
	      V = I1*I2 ) ;
	    V = I1 *I2.



%
% three types of matrix: integers, floats and general terms.
%

matrix_new(terms,Dims, '$matrix'(Dims, NDims, Size, Offsets, Matrix) ) :-
	length(Dims,NDims),
	foldl(size, Dims, 1, Size),
	maplist(zero, Dims, Offsets),
	functor( Matrix, c, Size).
matrix_new(ints,Dims,Matrix) :-
	length(Dims,NDims),
	new_ints_matrix_set(NDims, Dims, 0, Matrix).
matrix_new(floats,Dims,Matrix) :-
	length(Dims,NDims),
	new_floats_matrix_set(NDims, Dims, 0.0, Matrix).


matrix_new(terms, Dims, Data, '$matrix'(Dims, NDims, Size, Offsets, Matrix) ) :-
	length(Dims,NDims),
	foldl(size, Dims, 1, Size),
	maplist(zero, Dims, Offsets),
	functor( Matrix, c, Size),
	Matrix =.. [c|Data].
matrix_new(ints,Dims,Data,Matrix) :-
	length(Dims,NDims),
	new_ints_matrix(NDims, Dims, Data, Matrix).
matrix_new(floats,Dims,Data,Matrix) :-
	length(Dims,NDims),
	new_floats_matrix(NDims, Dims, Data, Matrix).


matrix_dims( Mat, Dims) :-
	( opaque(Mat) -> matrixn_dims( Mat, Dims ) ;
	    Mat = '$matrix'( Dims, _, _, _, _) ).

matrix_dims( Mat, Dims, Bases) :-
	( opaque(Mat) -> matrixn_dims( Mat, Dims, Bases ) ;
	    Mat = '$matrix'( Dims, _, _, Bases, _) ).

matrix_ndims( Mat, NDims) :-
	( opaque(Mat) -> matrixn_ndims( Mat, NDims ) ;
	    Mat = '$matrix'( _, NDims, _, _, _) ).

matrix_size( Mat, Size) :-
	( opaque(Mat) -> matrixn_size( Mat, Size ) ;
	    Mat = '$matrix'( _, _, Size, _, _) ).

matrix_to_list( Mat, ToList) :-
	( opaque(Mat) -> matrixn_to_list( Mat, ToList ) ;
	    Mat = '$matrix'( _, _, _, _, M), M=.. [_|ToList] ).

matrix_to_lists( Mat, ToList) :-
	matrix_dims( Mat, [D|Dims] ),
	D1 is D-1,
	for( I in 0..D1, matrix_slicer( Dims, Mat, [I|L]-L), ToList, [] ).

matrix_slicer( [_], M, Pos-[_], [O|L0], L0) :- !,
	O <== '[]'(Pos,M).
matrix_slicer( [D|Dims], M, Pos-[I|L], [O|L0], L0) :-
	D1 is D-1,
	for( I in 0..D1 , L^matrix_slicer( Dims, M, Pos-L), O, [] ).

matrix_get( Mat, Pos, El) :-
	( opaque(Mat) -> matrixn_get( Mat, Pos, El ) ;
	    m_get(Mat, Pos, El)  ).

matrix_get_range( Mat, Pos, Els) :-
	slice(Pos, Keys),
	maplist( matrix_get(Mat), Keys, Els).

slice([], [[]]).
slice([[H|T]|Extra], Els) :- !,
	slice(Extra, Els0), 
	foldl(add_index_prefix( Els0 ), [H|T], Els, [] ).
slice([H|Extra], Els) :- !,
	slice(Extra, Els0), 
	add_index_prefix( Els0 , H, Els, [] ).

add_index_prefix( [] , _H ) --> [].
add_index_prefix( [L|Els0] , H ) --> [[H|L]],
	add_index_prefix( Els0 , H ).


matrix_set_range( Mat, Pos, Els) :-
	slice(Pos, Keys),
	maplist( matrix_set(Mat), Keys, Els).

matrix_set( Mat, Pos, El) :-
	( opaque(Mat) -> matrixn_set( Mat, Pos, El ) ;
	    m_set(Mat, Pos, El)  ).

matrix_new_set(ints,Dims,Elem,Matrix) :-
	length(Dims,NDims),
	new_ints_matrix_set(NDims, Dims, Elem, Matrix).
matrix_new_set(floats,Dims,Elem,Matrix) :-
	length(Dims,NDims),
	new_floats_matrix_set(NDims, Dims, Elem, Matrix).


matrix_type(Matrix,Type) :-
	( matrix_type_as_number(Matrix, 0) -> Type = ints ;
	  opaque( Matrix ) -> Type = floats ;
	  Type = terms ).

matrix_base(Matrix, Bases) :-
	( opaque( Matrix ) -> maplist('='(Base), Bases), matrixn_set_base( Matrix, Base ) ;
	  nb_setarg(4, Matrix, Bases ) ).

matrix_arg_to_offset(M, Index, Offset) :-
	( opaque(M) -> matrixn_arg_to_offset( M, Index, Offset ) ;
	    M = '$matrix'(Dims, _, Size, Bases, _) -> foldl2(indx, Index, Dims, Bases, Size, _, 0, Offset)  ).	
	
matrix_offset_to_arg(M, Offset, Index) :-
	( opaque(M) -> matrixn_offset_to_arg( M, Offset, Index ) ;
	    M = '$matrix'(Dims, _, Size, Bases, _) -> foldl2(offset, Index, Dims, Bases, Size, _, Offset, _)  ).	
	
matrix_max(M, Max) :-
	( opaque(M) -> matrixn_max( M, Max ) ;
	    M = '$matrix'(_, _, _, _, C) ->
	  arg(1,C,V0), foldargs(max, M, V0, Max) ;
	  M = [V0|L], foldl(max, L, V0, Max) ).

max(New, Old, Max) :- ( New >= Old -> New = Max ; Old = Max ).
	
matrix_maxarg(M, MaxArg) :-
	( opaque(M) -> matrixn_maxarg( M, MaxArg );
	    M = '$matrix'(_, _, _, _, C) ->
	  arg(1,C,V0), foldargs(maxarg, M, V0-0-0, _-Offset-_), matrix_offset_to_arg(M, Offset, MaxArg) ;
	  M = [V0|L], foldl(maxarg, L, V0-0-1, _Max-Off-_ ), MaxArg = [Off] ).

maxarg(New, Old-OPos-I0, Max-MPos-I) :- I is I0+1, ( New > Old -> New = Max, MPos = I0 ; Old = Max, MPos = OPos ).
	
matrix_min(M, Min) :-
	( opaque(M) -> matrixn_min( M, Min ) ;
	    M = '$matrix'(_, _, _, _, C) ->
	  arg(1,C,V0), foldargs(min, M, V0, Max) ;
	  M = [V0|L], foldl(min, L, V0, Max) ).

min(New, Old, Max) :- ( New =< Old -> New = Max ; Old = Max ).

matrix_minarg(M, MinArg) :-
	( opaque(M) -> matrixn_minarg( M, MinArg );
	    M = '$matrix'(_, _, _, _, C) ->
	  arg(1,C,V0), foldargs(minarg, M, V0-0-0, _-Offset-_), matrix_offset_to_arg(M, Offset, MinArg) ;
	  M = [V0|L], foldl(minarg, L, V0-0-1, _Min-Off-_ ), MinArg = [Off] ).

minarg(New, Old-OPos-I0, Min-MPos-I) :- I is I0+1, ( New < Old -> New = Min, MPos = I0 ; Old = Min, MPos = OPos ).

matrix_to_logs(M, LogM) :-
	( opaque(M) -> matrixn_to_logs( M, LogM ) ;
	    M = '$matrix'(A, B, D, E, C) ->
	  LogM = '$matrix'(A, B, D, E, LogC),
	  mapargs(log, C, LogC) ;
	  M = [V0|L] -> maplist(log, [V0|L], LogM ) ;
	  LogM is log(M) ).

log(X, Y) :- Y is log(X).

matrix_to_exps(M, ExpM) :-
	( opaque(M) -> matrixn_to_exps( M, ExpM ) ;
	    M = '$matrix'(A, B, D, E, C) ->
	  ExpM = '$matrix'(A, B, D, E, ExpC),
	  mapargs(exp, C, ExpC) ;
	  M = [V0|L] -> maplist(exp, [V0|L], ExpM ) ;
	  ExpM is exp(M) ).

exp(X, Y) :- Y is exp(X).

matrix_agg_lines(M1,+,NM) :-
	do_matrix_agg_lines(M1,0,NM).
/* other operations: *, logprod */

matrix_agg_cols(M1,+,NM) :-
	do_matrix_agg_cols(M1,0,NM).
/* other operations: *, logprod */

matrix_op(M1,M2,+,NM) :-
	( opaque(M1), opaque(M2) ->
	  do_matrix_op(M1,M2,0,NM) ;
	  matrix_m(M1, '$matrix'(A,B,D,E,C1)),
	  matrix_m(M2, '$matrix'(A,B,D,E,C2)),
	  mapargs(plus, C1, C2, C),
	  NM = '$matrix'(A,B,D,E,C) ).
matrix_op(M1,M2,-,NM) :-
	( opaque(M1), opaque(M2) ->
	  do_matrix_op(M1,M2,1,NM) ;
	  matrix_m(M1, '$matrix'(A,B,D,E,C1)),
	  matrix_m(M2, '$matrix'(A,B,D,E,C2)),
	  mapargs(minus, C1, C2, C),
	  NM = '$matrix'(A,B,D,E,C) ).
matrix_op(M1,M2,*,NM) :-
	( opaque(M1), opaque(M2) ->
	  do_matrix_op(M1,M2,2,NM) ;
	  matrix_m(M1, '$matrix'(A,B,D,E,C1)),
	  matrix_m(M2, '$matrix'(A,B,D,E,C2)),
	  mapargs(times, C1, C2, C),
	  NM = '$matrix'(A,B,D,E,C) ).
matrix_op(M1,M2,/,NM) :-
	( opaque(M1), opaque(M2) ->
	  do_matrix_op(M1,M2,3,NM) ;
	  matrix_m(M1, '$matrix'(A,B,D,E,C1)),
	  matrix_m(M2, '$matrix'(A,B,D,E,C2)),
	  mapargs(div, C1, C2, C),
	  NM = '$matrix'(A,B,D,E,C) ).
matrix_op(M1,M2,zdiv,NM) :-
	( opaque(M1), opaque(M2) ->
	  do_matrix_op(M1,M2,5,NM) ;
	  matrix_m(M1, '$matrix'(A,B,D,E,C1)),
	  matrix_m(M2, '$matrix'(A,B,D,E,C2)),
	  mapargs(zdiv, C1, C2, C),
	  NM = '$matrix'(A,B,D,E,C) ).


matrix_op_to_all(M1,+,Num,NM) :-
	( opaque(M1) ->
	  do_matrix_op_to_all(M1,0,Num,NM)
	;
	  M1 = '$matrix'(A,B,D,E,C),
	  mapargs(plus(Num), C, NC),
	  NM = '$matrix'(A,B,D,E,NC)
	).
matrix_op_to_all(M1,-,Num,NM) :-
	( opaque(M1) ->
	  do_matrix_op_to_all(M1,1,Num,NM)
	;
	  M1 = '$matrix'(A,B,D,E,C),
	  mapargs(minus(Num), C, NC),
	  NM = '$matrix'(A,B,D,E,NC)
	).
matrix_op_to_all(M1,*,Num,NM) :-
	( opaque(M1) ->
	  do_matrix_op_to_all(M1,2,Num,NM)
	;
	  M1 = '$matrix'(A,B,D,E,C),
	  mapargs(times(Num), C, NC),
	  NM = '$matrix'(A,B,D,E,NC)
	).	
matrix_op_to_all(M1,/,Num,NM) :-
	% can only use floats.
	FNum is float(Num),
	( opaque(M1) ->
	  do_matrix_op_to_all(M1,3,FNum,NM)
	;
	  M1 = '$matrix'(A,B,D,E,C),
	  mapargs(div(Num), C, NC),
	  NM = '$matrix'(A,B,D,E,NC)
	).	
	  
/* other operations: *, logprod */

matrix_op_to_lines(M1,M2,/,NM) :-
	do_matrix_op_to_lines(M1,M2,3,NM).
/* other operations: *, logprod */

matrix_op_to_cols(M1,M2,+,NM) :-
	do_matrix_op_to_cols(M1,M2,0,NM).
/* other operations: *, logprod */


matrix_transpose(M1,M2) :-
	matrix_shuffle(M1,[1,0],M2).

size(N0, N1, N2) :-
	N2 is N0*N1.

% use 1 to get access to matrix
m_get('$matrix'(Dims, _, Sz, Bases, M), Indx, V) :-
	foldl2(indx, Indx, Dims, Bases, Sz, _, 1, Offset),
	arg(Offset, M, V).

m_set('$matrix'(Dims, _, Sz, Bases, M), Indx, V) :-
	foldl2(indx, Indx, Dims, Bases, Sz, _, 1, Offset),
	arg(Offset, M, V).

indx( I, Dim, Base, BlkSz, NBlkSz, I0, IF) :-
	NBlkSz is BlkSz div Dim ,
	IF is (I-Base)*NBlkSz + I0.

offset( I, Dim, BlkSz, NBlkSz, Base, I0, IF) :-
	NBlkSz is BlkSz div Dim,
	I is I0 div NBlkSz + Base,
	IF is I0 rem NBlkSz.

inc(I1, I, I1) :-
	I1 is I+1.

new_matrix(M0, Opts0, M) :-
	opaque(M), !,
	matrix_to_list(M0, L),
	new_matrix(L, Opts0, M).
new_matrix('$matrix'(_,_,_,_,C), Opts0, M) :- !,
	C =..[_|L],
	new_matrix(L, Opts0, M).
new_matrix(C, Opts0, M) :-
	functor(C, c, _), !,
	C =..[_|L],
	new_matrix(L, Opts0, M).
new_matrix(List, Opts0, M) :-
	foldl2(el_list(MDims), List, Flat, [], 0, Dim),  !,
	fix_opts(Opts0, Opts),
	foldl2(process_new_opt, Opts, Type, TypeF, [Dim|MDims], Dims, Base),
	( var(TypeF) -> guess_type( Flat, Type ) ; true ),
	matrix_new( Type, Dims, Flat, M),
	( nonvar(Base) -> matrix_base(M, Base); true ).	
new_matrix([H|List], Opts0, M) :-
	length( [H|List], Size),
	fix_opts(Opts0, Opts),
	foldl2(process_new_opt(Base), Opts, Type, TypeF, [Size], Dims),
	( var(TypeF) -> guess_type( [H|List], Type ) ; true ),
	matrix_new( Type, Dims, [H|List], M),
	( nonvar(Base) -> matrix_base(M, Base); true ).

fix_opts(V, _) :-
	var(V), !,
	throw(error(instantiation_error, V)).
fix_opts(A=B, [A=B]).
fix_opts(A, A) :-
	is_list(A), !.
fix_opts(V, _) :-
	var(V), !,
	throw(error(domain_error(options=V), new_matrix)).

guess_type( List, Type ) :-
	maplist( integer, List), !,
	Type = ints.
guess_type( List, Type ) :-
	maplist( number, List), !,
	Type = floats.
guess_type( _List, terms ).

process_new_opt(_Base, dim=Dim, Type, Type, _, Dim) :- !.
process_new_opt(_Base, type=Type, _, Type, Dim, Dim) :- !.
process_new_opt( Base, base=Base, Type, Type, Dim, Dim) :- !.
process_new_opt(_Base, Opt, Type, Type, Dim, Dim) :-
	throw(error(domain_error(opt=Opt), new_matrix)).

el_list(_, V, _Els, _NEls, _I0, _I1) :- 
	var(V), !,
	fail.
el_list([N|Extra], El, Els, NEls, I0, I1) :- 
	foldl2(el_list(Extra), El, Els, NEls, 0, N), !,
	I1 is I0+1.
el_list([N], El, Els, NEls, I0, I1) :- 
	El = [_|_],
	length(El, N),
	append(El, NEls, Els),
	I1 is I0+1.

for( Domain, Goal) :-
	strip_module(Goal, M, Locals^NG), !,
	term_variables(Domain+Locals, LocalVarsL),
	LocalVars =.. [vs|LocalVarsL],
	iterate( Domain, [], LocalVars, M:NG, [], [] ),
	terms:reset_variables( LocalVars ).
for( Domain, Goal ) :-
	strip_module(Goal, M, NG),
	term_variables(Domain, LocalVarsL),
	LocalVars =.. [vs|LocalVarsL],
	iterate( Domain, [], LocalVars, M:NG, [], [] ),
	terms:reset_variables( LocalVars ).

for( Domain, Goal, Inp, Out) :-
	strip_module(Goal, M, Locals^NG), !,
	term_variables(Domain+Locals, LocalVarsL),
	LocalVars =.. [vs|LocalVarsL],
	iterate( Domain, [], LocalVars, M:NG, [], [], Inp, Out).
for( Domain, Goal, Inp, Out ) :-
	strip_module(Goal, M, NG),
	term_variables(Domain, LocalVarsL),
	LocalVars =.. [vs|LocalVarsL],
	iterate( Domain, [], LocalVars, M:NG, [], [], Inp, Out ).

iterate( [], [], LocalVars, Goal, Vs, Bs ) :-
	terms:freshen_variables(LocalVars),
	Vs = Bs,
	MG <== Goal,
	once( MG ),
	terms:reset_variables(LocalVars).
iterate( [], [H|Cont], LocalVars, Goal, Vs, Bs ) :-
	iterate(H, Cont, LocalVars, Goal, Vs, Bs ).
iterate( [H|L], [], LocalVars, Goal, Vs, Bs ) :- !,
	iterate(H, L, LocalVars, Goal, Vs, Bs ).
iterate( [H|L], Cont, LocalVars, Goal, Vs, Bs ) :- !,
	append(L, Cont, LCont),
	iterate(H, LCont, LocalVars, Goal, Vs, Bs ).
iterate( [] ins _A .. _B, [H|L], LocalVars, Goal, Vs, Bs ) :- !,
	iterate(H, L, LocalVars, Goal, Vs, Bs ).
iterate( [] ins _A .. _B, [], LocalVars, Goal, Vs, Bs ) :- !,
	iterate([], [], LocalVars, Goal, Vs, Bs ).
iterate( [V|Ps] ins A..B, Cont, LocalVars, Goal, Vs, Bs  ) :-
	eval(A, Vs, Bs, NA),
	eval(B, Vs, Bs, NB),
	( NA > NB ->  true ;
	  A1 is NA+1,
	  iterate( Ps ins NA..NB, Cont, LocalVars, Goal, [V|Vs], [NA|Bs] ),
	  iterate( [V|Ps] ins A1..NB, Cont, LocalVars, Goal, Vs, Bs )
	).
iterate( V in A..B, Cont, LocalVars, Goal, Vs, Bs) :-
	var(V),
	eval(A, Vs, Bs, NA),
	eval(B, Vs, Bs, NB),
	( NA > NB -> true ;
	  A1 is NA+1,
	  (Cont = [H|L] ->
	   iterate( H, L, LocalVars, Goal, [V|Vs], [NA|Bs] )
	  ;
	   iterate( [], [], LocalVars, Goal, [V|Vs], [NA|Bs] )
	  ),
	  iterate( V in A1..NB, Cont, LocalVars, Goal, Vs, Bs ) 
	).

iterate( [], [], LocalVars, Goal, Vs, Bs, Inp, Out ) :-
	terms:freshen_variables(LocalVars),
	Vs = Bs,
	MG <== Goal,
	once( call(MG, Inp, Out) ),
	terms:reset_variables(LocalVars).	
iterate( [], [H|Cont], LocalVars, Goal, Vs, Bs, Inp, Out ) :-
	iterate(H, Cont, LocalVars, Goal, Vs, Bs, Inp, Out ).
iterate( [H|L], [], LocalVars, Goal, Vs, Bs, Inp, Out ) :- !,
	iterate(H, L, LocalVars, Goal, Vs, Bs, Inp, Out ).
iterate( [H|L], Cont, LocalVars, Goal, Vs, Bs, Inp, Out ) :- !,
	append(L, Cont, LCont),
	iterate(H, LCont, LocalVars, Goal, Vs, Bs, Inp, Out ).
iterate( [] ins _A .. _B, [], LocalVars, Goal, Vs, Bs, Inp, Out ) :- !,
	iterate([], [], LocalVars, Goal, Vs, Bs, Inp, Out ).
iterate( [] ins _A .. _B, [H|L], LocalVars, Goal, Vs, Bs, Inp, Out ) :- !,
	iterate(H, L, LocalVars, Goal, Vs, Bs, Inp, Out ).
iterate( [V|Ps] ins A..B, Cont, LocalVars, Goal, Vs, Bs, Inp, Out  ) :-
	eval(A, Vs, Bs, NA),
	eval(B, Vs, Bs, NB),
	( NA > NB ->  Inp = Out ;
	  A1 is NA+1,
	  iterate( Ps ins A..B, Cont, LocalVars, Goal, [V|Vs], [NA|Bs], Inp, Mid ),
	  iterate( [V|Ps] ins A1..NB, Cont, LocalVars, Goal, Vs, Bs, Mid, Out )
	).
iterate( V in A..B, Cont, LocalVars, Goal, Vs, Bs, Inp, Out) :-
	var(V),
	eval(A, Vs, Bs, NA),
	eval(B, Vs, Bs, NB),
	( NA > NB -> Inp = Out ;
	  A1 is NA+1,
	  (Cont = [H|L] ->
	   iterate( H, L, LocalVars, Goal, [V|Vs], [NA|Bs], Inp, Mid )
	  ;
	   iterate( [], [], LocalVars, Goal, [V|Vs], [NA|Bs], Inp, Mid )
	  ),
	  iterate( V in A1..NB, Cont, LocalVars, Goal, Vs, Bs, Mid, Out ) 
	).

	 
eval(I, _Vs, _Bs, I) :- integer(I), !.
eval(I, Vs, Bs, NI) :-
	copy_term(I+Vs, IA+Bs),
	NI <== IA.

matrix_seq(A, B, Dims, M) :-
	ints(A, B, L),
	matrix_new(ints, Dims, L, M).

ints(A,B,O) :-
	( A > B -> O = [] ; O = [A|L], A1 is A+1, ints(A1,B,L) ).

zero(_, 0).

