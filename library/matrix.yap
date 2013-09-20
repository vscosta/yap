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
	   [op(100, yf, []),
            (<==)/2, op(500, xfx, '<=='),
	    op(700, xfx, in),
	    op(700, xfx, ins),
            op(450, xfx, ..), % should bind more tightly than \/
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
	    matrix_column/3,
	    matrix_get/2,
	    matrix_set/2,
	    foreach/2,
	    foreach/4,
	    op(100, fy, '[]')
	    ]).

:- load_foreign_files([matrix], [], init_matrix).

:- meta_predicate foreach(+,0), foreach(+,2, +, -).

:- use_module(library(maplist)).
:- use_module(library(lists)).

( LHS <== RHS ) :-
	rhs(RHS, R),
	set_lhs( LHS, R).

rhs(RHS, RHS) :- var(RHS), !.
% base case
rhs(A, A) :- atom(A), !.
rhs(RHS, RHS) :- number(RHS), !.
rhs(RHS, RHS) :- opaque(RHS), !.
rhs(RHS, RHS) :- RHS = m(_, _, _, _), !.
rhs(matrix(List, Opts), RHS) :-
	rhs( List, A1),
	new_matrix(A1, Opts, RHS).
rhs(matrix(List), RHS) :-
	rhs( List, A1),
	new_matrix(A1, [], RHS).
rhs(matrix(List, Opt1), RHS) :-
	rhs( List, A1),
	new_matrix(A1, Opt1, RHS).
rhs(matrix(List, Opt1, Opt2), RHS) :-
	rhs( List, A1),
	new_matrix(A1, [Opt1, Opt2], RHS).
rhs(matrix(List, Opt1, Opt2, Opt3), RHS) :-
	rhs( List, A1),
	new_matrix(A1, [Opt1, Opt2, Opt3], RHS).
rhs(matrix(List, Opt1, Opt2, Opt3, Opt4), RHS) :-
	rhs( List, A1),
	new_matrix(A1, [Opt1, Opt2, Opt3, Opt4], RHS).
rhs(matrix(List, Opt1, Opt2, Opt3, Opt4, Opt5), RHS) :-
	rhs( List, A1),
	new_matrix(A1, [Opt1, Opt2, Opt3, Opt4, Opt5], RHS).
rhs(dim(RHS), Dims) :- !,
	rhs(RHS, X1),
	matrix_dims( X1, Dims ).
rhs(dims(RHS), Dims) :- !,
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
rhs(list(RHS), List) :- !,
	rhs(RHS, X1),
	matrix_to_list( X1, List ).
rhs(A=B, NA=NB) :- !,
	rhs(A, NA),
	rhs(B, NB).
rhs('[]'(Args, RHS), Val) :- !,
	rhs(RHS, X1),
	matrix_dims( X1, Dims ),
	maplist( index(Range), Args, Dims, NArgs),
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
rhs(':'(I, J), [I1|Is]) :- !,
	rhs(I, I1),
	rhs(J, J1),
	once( foldl(inc, Is, I1, J1) ).

set_lhs(V, R) :- var(V), !, V = R.
set_lhs(V, R) :- number(V), !, V = R.
set_lhs(V, R) :- V = '[]'(Indx, M), !, 
	matrix_set( M, Indx, R).

%
% ranges of arguments
%
index(Range, V, M, Indx) :- var(V), !,
	index(Range, 0..(M-1), M, Indx).
index(Range, '*', M, Indx) :- !,
	index(Range, 0..(M-1), M, Indx).
index(Range, Exp, M, Indx) :- !,
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
index(I+J, _M, O ) :-
	index(I, M, I1),
	index(J, M, J1),
	add_index(I1, J1, O).
index(I-J, _M, O ) :-
	index(I, M, I1),
	index(J, M, J1),
	add_index(I1, J1, O).
index(I*J, _M, O ) :-
	index(I, M, I1),
	index(J, M, J1),
	O is I*J.
index(I div J, _M, O ) :-
	index(I, M, I1),
	index(J, M, J1),
	O is I div J.
index(I rem J, _M, O ) :-
	index(I, M, I1),
	index(J, M, J1),
	O is I rem J.
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

%
% three types of matrix: integers, floats and general terms.
%

matrix_new(terms,Dims, m(Dims, NDims, Size, Matrix) ) :-
	length(Dims,NDims),
	foldl(size, Dims, 1, Size),
	functor( Matrix, c, Size).
matrix_new(ints,Dims,Matrix) :-
	length(Dims,NDims),
	new_ints_matrix_set(NDims, Dims, 0, Matrix).
matrix_new(floats,Dims,Matrix) :-
	length(Dims,NDims),
	new_floats_matrix_set(NDims, Dims, 0.0, Matrix).


matrix_new(terms, Dims, Data, m(Dims, NDims, Size, Matrix) ) :-
	length(Dims,NDims),
	foldl(size, Dims, 1, Size),
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
	    Mat = m( Dims, _, _, _) ).

matrix_ndims( Mat, NDims) :-
	( opaque(Mat) -> matrixn_ndims( Mat, NDims ) ;
	    Mat = m( _, NDims, _, _) ).

matrix_size( Mat, Size) :-
	( opaque(Mat) -> matrixn_size( Mat, Size ) ;
	    Mat = m( _, _, Size, _) ).

matrix_to_list( Mat, ToList) :-
	( opaque(Mat) -> matrixn_to_list( Mat, ToList ) ;
	    Mat = m( _, _, _, M), M=.. [_|ToList] ).

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

matrix_arg_to_offset(M, Index, Offset) :-
	( opaque(M) -> matrixn_arg_to_offset( M, Index, Offset ) ;
	    M = m(Dims, _, Size, _) -> foldl2(indx, Index, Dims, Size, _, 0, Offset)  ).	
	
matrix_offset_to_arg(M, Offset, Index) :-
	( opaque(M) -> matrixn_offset_to_arg( M, Offset, Index ) ;
	    M = m(Dims, _, Size, _) -> foldl2(offset, Index, Dims, Size, _, Offset, _)  ).	
	
matrix_max(M, Max) :-
	( opaque(M) -> matrixn_max( M, Max ) ;
	    M = m(_, _, _, M) -> fail ).
	
matrix_maxarg(M, Max) :-
	( opaque(M) -> matrixn_maxarg( M, Max ) ;
	    M = m(Dims, _, Size, _) -> fail  ).	
	
matrix_min(M, Min) :-
	( opaque(M) -> matrixn_min( M, Min ) ;
	    M = m(_, _, _, M) -> fail ).
	
matrix_minarg(M, Min) :-
	( opaque(M) -> matrixn_minarg( M, Min ) ;
	    M = m(Dims, _, Size, _) -> fail  ).	
	
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

size(N0, N1, N2) :-
	N2 is N0*N1.

% use 1 to get access to matrix
m_get(m(Dims, _, Sz, M), Indx, V) :-
	foldl2(indx, Indx, Dims, Sz, _, 1, Offset),
	arg(Offset, M, V).

m_set(m(Dims, _, Sz, M), Indx, V) :-
	foldl2(indx, Indx, Dims, Sz, _, 1, Offset),
	nb_setarg(Offset, M, V).

indx( I, Dim, BlkSz, NBlkSz, I0, IF) :-
	NBlkSz is BlkSz div Dim,
	IF is I*NBlkSz + I0.

offset( I, Dim, BlkSz, NBlkSz, I0, IF) :-
	NBlkSz is BlkSz div Dim,
	I is I0 div NBlkSz,
	IF is I0 rem NBlkSz.

inc(I1, I, I1) :-
	I1 is I+1.

new_matrix(M0, Opts0, M) :-
	opaque(M), !,
	matrix_to_list(M0, L),
	new_matrix(L, Opts0, M).
new_matrix(m(_,_,_,C), Opts0, M) :- !,
	C =..[_|L],
	new_matrix(L, Opts0, M).
new_matrix(C, Opts0, M) :-
	functor(C, c, _), !,
	C =..[_|L],
	new_matrix(L, Opts0, M).
new_matrix(List, Opts0, M) :-
	foldl2(el_list(MDims), List, Flat, [], 0, Dim),  !,
	fix_opts(Opts0, Opts),
	foldl2(process_new_opt, Opts, Type, TypeF, [Dim|MDims], Dims),
	( var(TypeF) -> guess_type( Flat, Type ) ; true ),
	matrix_new( Type, Dims, Flat, M).	
new_matrix([H|List], Opts0, M) :-
	length( [H|List], Size),
	fix_opts(Opts0, Opts),
	foldl2(process_new_opt, Opts, Type, TypeF, [Size], Dims),
	( var(TypeF) -> guess_type( [H|List], Type ) ; true ),
	matrix_new( Type, Dims, [H|List], M).

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

process_new_opt(dim=Dim, Type, Type, _, Dim) :- !.
process_new_opt(type=Type, _, Type, Dim, Dim) :- !.
process_new_opt(Opt, _, Type, Dim, Dim) :-
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

foreach( Domain, M:(Locals^Goal)) :- !,
	global_variables( Domain, Locals, Goal, GlobalVars ),
	iterate( Domain, [], GlobalVars, M:Goal, [], [] ).
foreach( Domain, Goal ) :-
	global_variables( Domain, [], Goal, GlobalVars ),
	iterate( Domain, [], GlobalVars, Goal, [], [] ).

foreach( Domain, M:(Locals^Goal), Inp, Out) :- !,
	global_variables( Domain, Locals, Goal, GlobalVars ),
	iterate( Domain, [], GlobalVars, M:Goal, [], [], Inp, Out).
foreach( Domain, Goal, Inp, Out ) :-
	global_variables( Domain, [], Goal, GlobalVars ),
	iterate( Domain, [], GlobalVars, Goal, [], [], Inp, Out ).

global_variables( Domain, Locals, Goal, GlobalVars ) :-
	term_variables( Domain+Locals, Pars ),
	term_variables( Goal, DGVs, Pars),
	sort( DGVs, GVs ),
	foldl( delv, Pars, GVs, GlobalVars ).

delv( V, [V1|Vs], Vs) :- V == V1, !.
delv( V, [V1|Vs], [V1|NVs]) :-
	delv( V, Vs, NVs).

iterate( [], [], GlobalVars, Goal, Vs, Bs ) :-
	  copy_term(t(Vs, Goal, GlobalVars), t(Bs, G, GlobalVars) ),
	  once( G ).	
iterate( [], [H|Cont], GlobalVars, Goal, Vs, Bs ) :-
	iterate(H, Cont, GlobalVars, Goal, Vs, Bs ).
iterate( [H|L], Cont, GlobalVars, Goal, Vs, Bs ) :- !,
	append(L, Cont, LCont),
	iterate(H, LCont, GlobalVars, Goal, Vs, Bs ).
iterate( [] ins _A .. _B, Cont, GlobalVars, Goal ) :- !,
	iterate(Cont, [], GlobalVars, Goal, Vs, Bs ).
iterate( [V|Ps] ins A..B, Cont, GlobalVars, Goal, Vs, Bs  ) :-
	eval(A, Vs, Bs, NA),
	eval(B, Vs, Bs, NB),
	( NA > NB ->  true ;
	  A1 is NA+1,
	  iterate( Cont, [], GlobalVars, Goal, [V|Vs], [NA|Bs] ),
	  iterate( Ps ins A1..NB, GlobalVars, Goal, [V|Vs], [NA|Bs] )
	).
iterate( V in A..B, Cont, GlobalVars, Goal, Vs, Bs) :-
	var(V),
	eval(A, Vs, Bs, NA),
	eval(B, Vs, Bs, NB),
	( NA > NB -> true ;
	  A1 is NA+1,
	  iterate( Cont, [], GlobalVars, Goal, [V|Vs], [NA|Bs] ),
	  iterate( V in A1..NB, Cont, GlobalVars, Goal, Vs, Bs ) 
	).

iterate( [], [], GlobalVars, Goal, Vs, Bs, Inp, Out ) :-
	  copy_term(t(Vs, Goal, GlobalVars), t(Bs, G, GlobalVars) ),
	  once( call(G, Inp, Out) ).	
iterate( [], [H|Cont], GlobalVars, Goal, Vs, Bs, Inp, Out ) :-
	iterate(H, Cont, GlobalVars, Goal, Vs, Bs, Inp, Out ).
iterate( [H|L], Cont, GlobalVars, Goal, Vs, Bs, Inp, Out ) :- !,
	append(L, Cont, LCont),
	iterate(H, LCont, GlobalVars, Goal, Vs, Bs, Inp, Out ).
iterate( [] ins _A .. _B, Cont, GlobalVars, Goal, Inp, Out ) :- !,
	iterate(Cont, [], GlobalVars, Goal, Vs, Bs, Inp, Out ).
iterate( [V|Ps] ins A..B, Cont, GlobalVars, Goal, Vs, Bs, Inp, Out  ) :-
	eval(A, Vs, Bs, NA),
	eval(B, Vs, Bs, NB),
	( NA > NB ->  Inp = Out ;
	  A1 is NA+1,
	  iterate( Cont, [], GlobalVars, Goal, [V|Vs], [NA|Bs], Inp, Mid ),
	  iterate( Ps ins A1..NB, GlobalVars, Goal, [V|Vs], [NA|Bs], Mid, Out )
	).
iterate( V in A..B, Cont, GlobalVars, Goal, Vs, Bs, Inp, Out) :-
	var(V),
	eval(A, Vs, Bs, NA),
	eval(B, Vs, Bs, NB),
	( NA > NB -> Inp = Out ;
	  A1 is NA+1,
	  iterate( Cont, [], GlobalVars, Goal, [V|Vs], [NA|Bs], Inp, Mid ),
	  iterate( V in A1..NB, Cont, GlobalVars, Goal, Vs, Bs, Mid, Out ) 
	).

	 
eval(I, _Vs, _Bs, I) :- integer(I), !.
eval(I, Vs, Bs, NI) :-
	copy_term(I+Vs, IA+Bs),
	NI <== IA.

