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
/**
 * @file   matrix.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 22:53:40 2015
 *
 * @brief  Vector, Array and Matrix  library
 *

 
 *
*/


:- module( (matrix),
	   [(<==)/2, op(800, xfx, <==),
	    (+==)/2, op(800, xfx, +==),
	    (-==)/2, op(800, xfx, -==),
	   op(950,fy,:=),
	   op(950,yfx,:=),
%	   op(950,fx,<-),
%	   op(950,yfx,<-),
	    op(700, xfx , (in)),
	    op(700, xfx, (within)),
	    op(700, xfx, (ins)),
        op(450, xfx, '..'), % should bind more tightly than \/
	op(790, fx, (matrix)),
	op(790, fx, array),
	op(780, xfx, of),
is_matrix/1,
	matrix_new/2,
	matrix_new/3,
	matrix_new_set/5,
	    matrix_dims/2,
	    matrix_ndims/2,
	    matrix_size/2,
	    matrix_type/2,
	    matrix_to_list/2,
	    matrix_to_lists/2,
	    matrix_get/3,
	    matrix_set/3,
	    matrix_inc/2,
	    matrix_add_to_element/3,
	    matrix_dec/2,
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
	matrix_map/2,
		matrix_map/3,
		matrix_map/4,
	mapmatrix/2,
		mapmatrix/3,
		mapmatrix/4,
	foldlmatrix/4,
	foldlmatrix/5,
	matrix_foldl/4,
	matrix_foldl/5,
	    matrix_op_to_all/4,
	    matrix_op_to_lines/4,
	    matrix_op_to_cols/4,
	    matrix_shuffle/3,
	    matrix_transpose/2,
	    matrix_set_all_that_disagree/5,
	    matrix_expand/3,
	    matrix_select/4,
	    matrix_column/3,
	    op(50, yf, []),
            op(50, yf, '()'),
            op(100, xfy, '.'),
            op(100, fy, '.')
	    ]).
:- load_foreign_files([],['YAPMatrix'], init_matrix).

:- multifile rhs_opaque/1, array_extension/2.

/** 

@defgroup YAPMatrix Matrix Operations in YAP.
@ingroup YAPLibrary
@brief The YAP Matrix library tries to provide an unified framework for matricial operations.


@{
 Matrices are available by loading the library
```
:- library(matrix).
```

Matrices are indexable objects formed with Prolog terms, or  opaque objects, or `malloc` chunk, or foreign objects.

 They are multimensional objects providing entries of type:
 + `terms</tt>: Prolog terms
 + `ints</tt>: bounded integers, represented as an opaque term. The
maximum integer depends on hardware, but should be obtained from the
natural size of the machine.
 + `floats</tt>: floating-point numbers, represented as an opaque term.

    Notice that the library does not support all known matrix operations. Please
contact the YAP maintainers if you require extra functionality.

General matrix assignment operation. It evaluates the right-hand side
 according to the
left-hand side and to the matrix:

+ if  _LHS_ is part of an integer or floating-point matrix,
perform non-backtrackable assignment.
+ other unify left-hand side and right-hand size.



 High Level Interface to Matrix evaluation.

 1 Initialization:

    - `Mat = matrix(Dims,Opts)` where Dims and Opts are lists:

    - `Mat = matrix(Dims)`, same as  matrix(Dims, [])`

    - `Mat = ListOfLists`

    - `Mat = matrix(Dims) of Type

    - `Mat = matrix(Dims) of Number`

    - `Mat = matrix(Dims) of Goal`

    - `Mat = matrix(Dims) of Offset^Goal`

    - `Mat = matrix(Dims) of call(Goal)` 

    - `Mat = vector(Dim,Opts)` same as `matrix([Dims], [])`

    - `Mat = zeros(Dims)`, same as `matrix(Dims, [])`

    - `Mat = matrix[Dims]`, same as  `matrix(Dims, [])`

    - `Mat = matrix[Dims] of Def, same as `matrix(Dims) of Def`

    Dimensions may be given as:
    ListOfNumbers.


    Options are:

    - One can use use old array routines (check static_array/3) for
      static storage (ie, heap allocated`)

    1. `mat <== [1000,1001,1002]`
    1. `vec <== matrix[1000] of int`
    2. `zeros <== matrix[333] of 0`

 The local matrices allocate on stack:
    1. `Var <== [[1000],[1001],[1002]]`
    1. `Var <== matrix[1000] of term`
    2. `Var <==  matrix[333,2] of 3.1415`

  YAP also supports foreign matrices.

Matrix elements can be accessed through the matrix_get/2 predicate
or through an `R</tt>-inspired access notation (that uses the ciao
style extension to `[]`). Examples include:


  + Access the second row, third column of matrix `X</tt>. Indices start from
`0`,
```
 _E_ <==  _X_[2,3]
```

+ Access all the second row, the output is a list ofe elements.
```
 _L_ <==  _X_[2,_]
```

+ Access all the second, thrd and fourth rows, the output is a list of elements.
```
 _L_ <==  _X_[2..4,_]
```

+ Access all the fifth, sixth and eight rows, the output is a list of elements.
```
 _L_ <==  _X_[2..4+3,_]
```

The right-hand side supports the following operators:

+ `[]`

    written as  _M_[ _Offset_]: obtain an element or list of elements
of matrix  _M_ at offset  _Offset_.

+ `..`

    _I_.. _J_ generates a list with all integers from  _I_ to
 _J_, included.

+ `+`

    add two numbers, add two matrices element-by-element, or add a number to
all elements of a matrix or list.

+ `- `

    subtract two numbers, subtract two matrices or lists element-by-element, or subtract a number from
all elements of a matrix or list

+ `* `

    multiply two numbers, multiply two matrices or lists
    element-by-element, or multiply a number from all elements of a
    matrix or list

+ `log`

    natural logarithm of a number, matrix or list

+ `exp`

    natural exponentiation of a number, matrix or list



The dimensions can be given as an integer, and the matrix will be
indexed `C`-style from  `0..( _Max_-1)`, or can be given
as  an interval  _Base_.. _Limit_. In the latter case,
matrices of integers and of floating-point numbers should have the same
    _Base_ on every dimension.

*/


:- multifile rhs_opaque/1, array_extension/2.

:- use_module(library(maplist)).
:- use_module(library(mapargs)).

:- use_module(library(lists)).
:- use_module(library(ordsets)).

:- meta_predicate matrix_map(1,+).
:- meta_predicate matrix_map(2,+,+).
:- meta_predicate matrix_map(3,+,+,+).

:- meta_predicate matrix_foldl(3,?,?,?).
:- meta_predicate matrix_foldl(4,?,?,?,?).


/** @infixpred ?_LHS_ <==  ?_RHS_ is semidet


 Dispatcher, with a special cases for matrices as the RH
 may depend on the LHS.



*/



O <== V :-
    var(V),
    !,
    throw(error(instantiation_error,O<==V)).
N <== M :-
    number(M),
    !,
    set__(N,M).
N <== Lists :-
    is_list(Lists),
    !,
    subl(Lists,Dims),
    matrix_new(Dims,N,[data(Lists)]).
(N <== M) :-
    is_matrix(M),
    !,
    matrix_new(M,N,[]).
(N <== [M|Ms]) :-
    is_list(Ms),
    !,
    matrix_new([M|Ms],N,[]).
(N <== matrix[H|T] of V) :-
var(V),
    !,
    matrix_new([H|T], N, [type(terms), fill(_)]).
(N <== matrix[H|T] of terms) :-
    !,
    matrix_new([H|T], N, [type(terms), fill(_)]).
(N <== matrix[H|T] of Type) :-
    memberchk(Type,[ints,floats]),
    !,
    matrix_new([H|T], N, [type(Type)]).
(N <== matrix[H|T] of Num) :-
    integer(Num),
    !,
    matrix_new([H|T], N, [type(ints), fill(Num)]).
(N <== matrix[H|T] of Num) :-
    float(Num),
    !,
    matrix_new([H|T], N, [type(floats), fill(Num)]).
(N<== matrix[H|T]  of Op) :-
	!,
    matrix_new([H|T], N, [fill(Op)]).
(N<== matrix(M) of Type) :-
    memberchk(Type,[ints,floats,terms]),
    !,
    matrix_new(M, N, [type(Type)]).
(N<== matrix(M) of Op) :-
	!,
    matrix_new(M, N, [fill(Op)]).
(N <== matrix(M)) :-
    !,
     matrix_new(M,N,[]).
(N <== zeros(M)) :-
    number(M),
    !,
    matrix_new([M],N,[type(ints),fill(0)]).
(N <== zeros[H|T]) :-
	!,
    matrix_new([H|T], N, [type(ints),fill(0)]).
 %   matrix_new(range(M),N,[]), !.
(N <== ones(M)) :-
    number(M),
    !,
    matrix_new([M],N,[type(ints),fill(1)]).
(N <== ones[H|T]) :-
	!,
    matrix_new([H|T], N, [type(ints),fill(1)]).



LHS <== RHS :-
    compute(RHS, Val),
    set__(LHS,Val),
    !.

/*
 @infixpred _LHS_  +== _RHS_

  add  _RHS_ to _LHS_ and store the result over the previous RHS.
 */
( LHS[Off|Offs] +== 1 ) :-
    maplist(compute,[Off|Offs],[EOff|EOffs]),
    compute(LHS,M),
    matrix_inc(M,[EOff|EOffs]),
    !.
( LHS[Off|Offs] +== RHS ) :-
    maplist(compute,[Off|Offs],[EOff|EOffs]),
    compute(RHS,V),
    compute(LHS,M),
    !,
    matrix_add_to_element(M,[EOff|EOffs],V).


/*
 @infixpred _LHS_  -= _RHS_

  subtract  _RHS_ from _LHS_ and store the result over the previous RHS.
 */
( LHS[Off|Offs] -== 1 ) :-
    !,
    maplist(compute,[Off|Offs],[EOff|EOffs]),
    compute(LHS,M),
    matrix_dec(M,[EOff|EOffs]).

( LHS  -== RHS ) :-
    compute(RHS,V),
    !,
    matrix_op(LHS,V,1,LHS).

/**
  @pred  matrix_map(Pred, A)

  Apply Pred(A,Index) to every element of matrix _A_, where _Index_ ranges over all entries in the array.

  As an example, to transform a floating point matrix into a logs matrix:

```{.prolog}
log(A,[X,Y]) :- V <== A[X,Y], LV is log(V), A[X,Y] <== LV.

?- X <== [[1.2,0.9,5],[0.1.0.2,0.3]], matrix_map(log, X),  Y <== X.list().
```
*/

mapmatrix(Pred, A) :-
    matrix_map(Pred,A).

matrix_map(Pred,A) :-
    matrix_size(A,Size),
    do_matrix_map(Pred, A, 0- Size).

do_matrix_map( _P, _A, I-I) :- !.
do_matrix_map( P, A,I-Size) :-
    matrix_offset_to_arg(A, I, Index),
    call(P,A,Index),
    I1 is I+1,
    do_matrix_map( P, A , I1-Size).

mapmatrix(Pred, A, B) :-
    matrix_map(Pred,A, B).

matrix_map(Pred,A,B) :-
    matrix_size(A,Size),
    do_matrix_map(Pred, A, B, 0- Size).

do_matrix_map( _P, _A, _, I-I) :- !.
do_matrix_map( P, A, B, I-Size) :-
    matrix_offset_to_arg(A, I, Index),
    call(P,A,B,Index),
    I1 is I+1,
    do_matrix_map(P, A, B ,I1-Size).

mapmatrix(Pred, A, B, C) :-
    matrix_map(Pred,A, B, C).


matrix_map(Pred,A,B,C) :-
    matrix_size(A,Size),
    do_matrix_map(Pred, A, B, C,0- Size).

do_matrix_map( _P, _A, _B, _C, I-I) :- !.
do_matrix_map( P, A, B, C, I-Size) :-
    matrix_offset_to_arg(A, I, Index),
    call(P,A,B,C,Index),
    I1 is I+1,
    do_matrix_map(P, A, B, C ,I1-Size).



/**
  @pred  matrix_foldl(Pred, A, V0, VF)

  Apply Pred(A,V1,V2,Index) to every element of matrix _A_, where _Index_ ranges over all entries in the array.

  As an example, to sum all elements of a numeric matrix:
```{.prolog}
add(A,	S0, S, B) :- matrix_get( A, I, V), S is S0+V.

sum_matrix(A,S) :- matrix_foldl(add,A,0,S).

?- X <== [[1.2,0.9,5],[0.1,0.2,0.3]], sum_matrix(X,S).
```


*/

foldlmatrix(Pred, A, V, VF) :-
    matrix_foldl(Pred,A, V, VF).

matrix_foldl(Pred,A,V0,VF) :-
    matrix_size(A,Size),
    do_matrix_foldl(Pred, A, V0, VF, 0- Size).

do_matrix_foldl(_P, _A, V, V,I-I) :- !.
do_matrix_foldl( P, A, V0, VF, I0-Size) :-
    matrix_offset_to_arg(A, I0, Index),
    call(P,A, V0, VI,Index),
    I1 is I0+1,
    do_matrix_foldl( P, A, VI, VF, I1 - Size).

/**
  @pred  matrix_foldl(Pred, A, V0, VF)
5B
  Apply Pred(A, B,V1,V2,Index) to every element of matrixes _A_, _B_, where _Index_ ranges over all entries in the matric.

  As an example, to compute the absolute error betwern numeric matrix:
```{.prolog}
add_d(A,B,	S0, S, [I]) :- V is A[I]-B[I], S is S0+abs(V).

sum_d_matrix(A,B,S) :- matrix_foldl(add_d,A,B,0,S).

?- X <== [[1.2,0.9,5],[0.1,0.2,0.3]], sum_d_matrix(X,S).
```


*/

foldlmatrix(Pred, A,  B, V, VF) :-
    matrix_foldl(Pred,A, B, V, VF).

matrix_foldl(Pred,A,B, V0,VF) :-
    matrix_size(A,Size),
    do_matrix_foldl(Pred, A,B, V0, VF, 0- Size).

do_matrix_foldl(_P, _A, _B, V, V,I-I) :- !.
do_matrix_foldl( P, A, B, V0, VF, I0-Size) :-
    matrix_offset_to_arg(A, I0, Index),
    call(P,A, B, V0, VI,Index),
    I1 is I0+1,
    do_matrix_foldl( P, A, B, VI, VF, I1 - Size).


/** @pred compute(Inp,Out)
 *
 * Compute the value of Inp and binds the free variable Out to
 * it
*/
compute(N, M) :-
    var(N),
    !,
    M=N.
compute(N, M) :-
    number(N),
    !,
    M=N.

compute(M, M) :-
    is_matrix(M),
    !.
compute(N, M) :-
    is_list(N),
    !,
    N1 <== N,
    compute(N1,M).

/**> `V <== M[Is]`: fetch a value _V_ from array _M_ and index _Is_.

     Notice that this routine evaluates and the indices. That is:

```
A <== [[1,2],[3,4]], B = [[1,-2],[-3,4]], V <== (A+B)[0, 0].
```

The result is `V=2`. YAP converts _B_ to a matrix, adds the matrix _A_ and the _B_ matrix, and finally extracts the first element.
*/
compute('[]'(Is,M),V) :-
    compute(M, MV),
    maplist(compute,Is,	IVs),
   !,
    matrix_get(MV,IVs,V).


compute(Matrix.dims(), V) :-
    compute(Matrix,MatrixV),
    !,
    matrix_dims(MatrixV, V).  /**>  list with matrix dimensions */

compute(Matrix.type(), V) :-
    compute(Matrix,MatrixV),
    !,
    matrix_type(MatrixV, V).  /**>  list with matrix dimensions */

compute(Matrix.ndims(), V) :-
    compute(Matrix,MatrixV),
    !,
    matrix_ndims(MatrixV, V).  /**>  list with matrix dimensions */

compute(Matrix.sum(), V) :-
    !,
    compute(Matrix,MatrixV),
    matrix_sum(MatrixV, V).  /**>  lisT with matrix dimensions */

compute(Matrix.nrows(), V) :-
    !,
    compute(Matrix,MatrixV),
    matrix_dims(MatrixV, [V,_]).  /**>  number of rows in bi-dimensional matrix */

compute(Matrix.ncols(), V) :-
    !,
    compute(Matrix,MatrixV),
    matrix_dims(MatrixV, [_,V]).  /**>  number of columns in bi-dimensional matrix */

compute(Matrix.length(), V) :- !,
    compute(Matrix,MatrixV),
    matrix_size(MatrixV, V).  /**>  size of a matrix*/

compute(Matrix.size(), V) :-
    !,
    compute(Matrix,MatrixV),
    matrix_size(MatrixV, V).  /**>  size of a matrix*/

compute(Matrix.max(), V) :-
    !,
    compute(Matrix,MatrixV),
    matrix_max(MatrixV, V).  /**>    maximum element of a numeric matrix*/

compute(Matrix.maxarg(), V) :- !,
    compute(Matrix,MatrixV),
    matrix_maxarg(MatrixV, V).  /**    argument of maximum element of a numeric matrix*/

compute(Matrix.min(), V) :-
    !,
    compute(Matrix,MatrixV),
    matrix_min(MatrixV, V).  /**    minimum element of a numeric matrix*/

compute(Matrix.minarg(), V) :-
    !,
    compute(Matrix,MatrixV),
    matrix_minarg(MatrixV, V).  /**>    argument of minimum element of a numeric matrix*/

compute(Matrix.list(), V) :-
    !,
    compute(Matrix,MatrixV),

    matrix_to_list(MatrixV, V).  /**>    represent matrix as a list*/

compute(Matrix.lists(), V) :-
    !,
    compute(Matrix,MatrixV),
    matrix_to_lists(MatrixV, V).  /**> represent matrix as a list of lists */

compute(Matrix.transpose(), V) :-
    !,
    compute(Matrix,MatrixV),
    matrix_transpose(MatrixV, V).  /**> represent matrix as a list of lists */

compute(Matrix.t(), V) :-
			        !,
    compute(Matrix,MatrixV),
    matrix_transpose(MatrixV, V).  /**> represent matrix as a list of lists */

compute(-B, C) :-
    compute(B, NB),
    (
	number(NB)
    ->
    C is -NB
    ;
    matrix_op_to_all(B, 2, -1, C)  /**> sq */
    ), !.

compute(A+B, C) :-
    !,
    compute(A, NA),
    compute(B, NB),
    (
	number(NA)
    ->
	(
	number(NB)
	->
	    C is NA+NB
	;
    matrix_op_to_all(NA, 0, NB, C)  /**> sq */
    )
     ;
    matrix_op(NA, NB, 0, C)  /**> sq */
    ).

compute(A-B, C) :-
    !,
    compute(A, NA),
    compute(B, NB),
    (
	number(NA)
    ->
	(
	number(NB)
	->
	    C is NA-NB
	;
    matrix_op_to_all(NA, 1, NB, C)  /**> sq */
    )
    ;
    matrix_op(NA, NB, 1, C)  /**> sq */
    ).
compute(A*B, C) :-
    !,
    compute(A, NA),
    compute(B, NB),
    (
	number(NA)
    ->
	(
	number(NB)
	->
	    C is NA*NB
	;
    matrix_op_to_all(NA, 2, NB, C)  /**> sq */
    )
    ;
    matrix_op(NA, NB, 2, C)  /**> sq */
    ).
compute(A/B, C) :-
    !,
    compute(A, NA),
    compute(B, NB),
    (
	number(NA)
    ->
    C is NA/NB
    ;
	number(NB)
    ->
    matrix_op_to_all(NA, 3, NB, C)  /**> sq */
    ;
    matrix_op(NA, NB, 3, C)  /**> sq */
    ).
compute(Cs,Exp) :-
    Cs =.. [Op,X],
  compute(X,NX),
  N=..[Op,NX],
  catch( Exp is N,_,fail),
!.


compute(Cs,Exp) :-
  Cs =.. [Op,X,Y],
  compute(X,NX),
  compute(Y,NY),
  N=..[Op,NX,NY],
  catch( Exp is N,_,fail),
 !.




/**
+ matrix/1

    create a vector from a list

+ `matrix`

    create a matrix from a list. Options are:

    + dim=
    a list of dimensions

   + type=
    integers, floating-point or terms

   + base=
    a list of base offsets per dimension (all must be the same for arrays of
integers and floating-points

+ matrix/3

    create matrix giving two options

*/

matrix_new( Matrix, Target ) :-
    matrix_new( Matrix, Target, []).


liveness(Target, f) :-
    atom(Target),
    !.
liveness(_Target, b).

type(Opts, _, _, Type) :-
    memberchk(type(Type), Opts),
    !.
type(_Opts, copy, [Data|_], ints) :-
    first(Data, F),
    integer(F),
    !.
type(_Opts, copy, [Data|_], floats) :-
    first(Data, F),
    float(F),
    !.
type(_Opts, copy, _, terms) :-
    !.
type(_Opts, fill, Data, ints) :-
    integer(Data),
    !.
type(_Opts, fill, Data, floats) :-
    float(Data),
    !.
type(_Opts, fill, _, terms).


data(Opts, copy, Data) :-
    memberchk(data(Data), Opts),
    !.
data(Opts, fill, Who) :-
    memberchk(fill(Fill), Opts),
    !,
    filler_data(Fill, Who).
data(_Opts, fill, 0) :-
    !.

filler_data(V, terms) :-
var(V),	
    !.
filler_data(Fill, Fill) :-
    number(Fill),
    !.
filler_data(floats, 0.0) :-
    !.
filler_data(ints, 0) :-
    !.
filler_data(Fill, Fill).



dimensions([H|List],[H|List],0) :-
    number(H),
    !.
dimensions([A..B|List],[H|NList],A) :-
    H is B-A,
    intervals(List, NList, A).

intervals([], [], _A).
intervals([A..B|List], [H|NList], A) :-
    H is B-A,
    intervals(List, NList, A),
    number(H),
    !.

subl(V,[]) :-
    var(V),
    !.
subl([H|L],[N|NL]) :-
    !,
    length([H|L],N),
    subl(H,NL).
subl(_,[]).

matrix_new( Matrix, Target, _Opts ) :-
    is_matrix(Matrix),
    is_matrix(Target),
    %validate(Ops),
    !,
    matrix_copy(Matrix, Target).
matrix_new( Matrix, Target, _Opts ) :-
    atom(Target),
    is_matrix(Matrix),
    !,
    matrix_dims(Matrix,Dims),
    matrix_type(Matrix,Type),
    matrix_base(Matrix,Base),
    matrix_create(Type,
		  f,Dims,Base,fill,0,Target),
    matrix_copy(Matrix, Target).
matrix_new( Matrix, Target, _Opts ) :-
    is_matrix(Matrix),
    !,
    matrix_dims(Matrix,Dims),
    matrix_type(Matrix,Type),
    matrix_base(Matrix,Base),
    matrix_new_matrix(Type,Dims,Base,Target),
    matrix_copy(Matrix, Target).
matrix_new( Info of Type, Target, Opts) :-
    !,
    dimensions(Info, Dims, Base),
    data(Opts, WhatWhen, Data),
    liveness(Target, Live),
    matrix_create(Type, Live, Dims, Base, WhatWhen, Data, Target).
matrix_new( Info, Target, Opts) :-
    dimensions(Info, Dims, Base),
    data(Opts, WhatWhen, Data),
    type(Opts, WhatWhen, Data, Type),
    liveness(Target, Live),
    matrix_create(Type, Live, Dims, Base, WhatWhen, Data, Target).

matrix_create(terms, b, Dims, Base, _, Data,
   '$matrix'(Dims, NDims, Size, Base, Matrix ) ) :-
    length(Dims,NDims),
    Dims=[H|RDims],
    multiply(RDims,H,Size),
    length(List,Size),
    fill(Data,List),
    Matrix =.. [(matrix)|List],
    functor(Matrix,(matrix),Size).
matrix_create(Type, b, Dims, Base, fill, 0,New) :-
    !,
    matrix_new_matrix(Type,Dims,Base,New).
matrix_create(Type, b, Dims, Base, fill, C,New) :-
    !,
    matrix_new_set(Type,Dims,Base,C,New).
matrix_create(Type, b, Dims, Base, copy, C,New) :-
    !,
    matrix_new_matrix(Type,Dims,Base,C,New).
matrix_create(ints, f, Dims, Base, fill, 0,New) :-
    !,
    static_array(New,Dims,Base,int).
matrix_create(floats, f, Dims, Base, fill, 0,New) :-
    !,
    static_array(New,Dims, Base,float).
matrix_create(floats, f, Dims, Base, fill, C,New) :-
    !,
    static_array(New,Dims, Base,float),
    update_whole_array(New,C).
matrix_create(ints, f, Dims, Base, fill, C,New) :-
    !,
    static_array(New,Dims, Base,int),
    update_whole_array(New,C).
matrix_create(floats, f, Dims, Base, copy, Lists,New) :-
    !,
    static_array(New,Dims, Base,float),
    flatten(Lists,List),
    matrix_set_all(New,List).
matrix_create(ints, f, Dims, Base, copy, Lists,New) :-
    !,
    static_array(New,Dims, Base,int),
    flatten(Lists,List),
    matrix_set_all(New,List).


zip(A-B0,A,B0,B1) :- B1 is B0+1.

multiply([],P,P).
multiply([X|Xs],P0,P) :-
    Pi is X*P0,
    multiply(Xs,Pi,P).

fill(V,_) :-
    var(V),
    !.
fill(terms,_) :-
    !.
fill(ints,L) :-
    !,
    maplist('='(0),L).
fill(floats,L) :-
    !,
    maplist('='(0.0),L).
fill(terms,_) :-
    !.
fill(N,L) :-
    number(N),
    !,
    maplist('='(N),L).
fill([H|L],NL) :-
    flatten([H|L],NL),
    !.
fill(G,L) :-
    maplist(run(G),L).

run(G,V) :-
    call(user:G,V),
    !.

matrix_seq(A, B, Dims, M) :-
	ints(A, B, L),
	matrix_new_matrix(ints, Dims,0, L, M).

ints(A,B,O) :-
	( A > B -> O = [] ; O = [A|L], A1 is A+1, ints(A1,B,L) ).

  /**
     case
*/
set__( VF, V) :-
    var(V),
    !,
    V=VF.
set__( VF, V) :-
    var(VF),
    !,
    V = VF.
set__(M,V) :-
    is_matrix(M),

    !,
    (
	number(V)
    ->
    matrix_set_all(M,V)
    ;
    is_list(V)
    ->
    foldl(set_l(M),V,0,_)
    ;
    is_matrix(V) ->
    matrix_copy(V,M)
    ).
set__('[]'(Args,M), Val) :-
    !,
    matrix_set(M,Args,Val).

matrix_set(M, Arg, Val) :-
    integer(Arg),
    !,
    matrix_set_one(M,[Arg],Val).
matrix_set(M, Args, Val) :-
    maplist(integer, Args),
    !,
    matrix_set_one(M,Args,Val).
/*
matrix_set(M, Args, Val) :-
    dims( M, Dims, Bases),
    maplist( index(Range), Args, Dims, Bases, NArgs),
    (
	var(Range)
    ->
    set_el( M, Args, Val )
    ;
    matrix_set_range( M, NArgs, Val )
    ).
*/

set_l(M,El,I,I1) :-
    M[I] <== El,
    I1 is I+1.
%c
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
index(I+J, M, O ) :- !,
	index(I, M, I1),
	index(J, M, J1),
	add_index(I1, J1, O).
index(I-J, M, O ) :- !,
	index(I, M, I1),
	index(J, M, J1),
	sub_index(I1, J1, O).
index(I*J, M, O ) :- !,
	index(I, M, I1),
	index(J, M, J1),
	O is I1*J1.
index(I rem J, M, O ) :- !,
	index(I, M, I1),
	index(J, M, J1),
	O is I1 rem J1.
index(I, M, NI ) :-
	maplist(indx(M), I, NI).

indx(M, I, NI) :- index(I, M, NI).

add_index(I1, J1, O) :-
	integer(I1),
	integer(J1),
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
	 (is_matrix(I1) ->
	    ( number(I2) -> matrix_op_to_all(I1, +, I2, V) ;
	      is_matrix(I2) ->  matrix_op(I1, I2, +, V) ;
	      V is I1+I2 ) ;
	 is_list(I1) ->
	    ( number(I2) -> maplist(plus(I2), I1, V) ;
      is_list(I2) ->  maplist(plus, I1, I2, V) ;
	      V is I1+I2 ) ;
	    V is I1 +I2
	    ).

msub(I1, I2, V) :-
        	 (is_matrix(I1) ->
        	    ( number(I2) -> matrix_op_to_all(I1, -, I2, V) ;
        	      is_matrix(I2) ->  matrix_op(I1, I2, -, V) ;
        	      V is I1-I2 ) ;
        	 is_list(I1) ->
        	    ( number(I2) -> maplist(minus(I2), I1, V) ;
        	      is_list(I2) ->  maplist(minus, I1, I2, V) ;
        	      V is I1-I2 ) ;
        	    V is I1-I2
        	    ).



mtimes(I1, I2, V) :-
        	 (is_matrix(I1) ->
        	    ( number(I2) -> matrix_op_to_all(I1, *, I2, V) ;
        	      is_matrix(I2) ->  matrix_op(I1, I2, *, V) ;
        	      V is I1*I2 ) ;
        	 is_list(I1) ->
        	    ( number(I2) -> maplist(times(I2), I1, V) ;
        	      is_list(I2) ->  maplist(times, I1, I2, V) ;
        	      V is I1*I2 ) ;
        	    V is I1*I2
        	    ).

mfdiv(I1, I2, V) :-
        	 (is_matrix(I1) ->
        	    ( number(I2) -> matrix_op_to_all(I1, /, I2, V) ;
        	      is_matrix(I2) ->  matrix_op(I1, I2, /, V) ;
        	      V is I1/I2 ) ;
        	 is_list(I1) ->
        	    ( number(I2) -> maplist(div(I2), I1, V) ;
        	      is_list(I2) ->  maplist(div, I1, I2, V) ;
        	      V is I1/I2 ) ;
        	    V is I1/I2
        	    ).


mneg(I1, V) :-
        	 (is_matrix(I1) ->
        	    matrix_op_to_all(I1, *, -1, V) ;
        	 is_list(I1) ->
        	     maplist(mult(-1),I1, V) ;
        	    V is -I1
        	    ).

%
% three types of matrix: integers, floats and general terms.
%
mult(V,X,Y) :- Y is V*X.

matrix_to_lists( Mat, ToList) :-
    matrix_dims( Mat, Dims),
    matrix_to_list(Mat,List),
    slice_into_sublists(Dims,List,ToList).

slice_into_sublists([],[],[]).
slice_into_sublists([_],List,[List]).
slice_into_sublists([D,D1|Dims],List,[Hs|Lists]) :-
    length(H,D),
    append(H,Rest,List),
    slice_into_sublists(Dims,Rest,Hs),
    slice_into_sublists([D1|Dims],List,Lists).


/** @pred matrix_get(+ _Matrix_,+ _Position_,- _Elem_)

Unify  _Elem_ with the element of  _Matrix_ at position
 _Position_, or with a list of elements i if Pod is a range.


*/
matrix_get( Mat, Pos, El) :-
    maplist(integer,Pos),

    !,
    matrix_get_one(Mat, Pos, El).
matrix_get( Mat, Pos, El) :-
    matrix_get_range( Mat, Pos, El).

matrix_get_range( Mat, Pos, Els) :-
    matrix_dims(Mat,Dims),
    findall(Pos, fill_indices(Pos,Dims), Indices),
    maplist(matrix_get_one(Mat), Indices, Els).

fill_indices([],[]).
fill_indices([I|L],[_|Dims]) :-
    integer(I),
    !,
    fill_indices(L,Dims).
fill_indices([I|L],[D|Dims]) :-
    var(I),
    D0 is D-1,
    between(0,D0,I),
    fill_indices(L,Dims).


slice([], [[]]).
slice([[H|T]|Extra], Els) :- !,
	slice(Extra, Els0),
	foldl(add_index_prefix( Els0 ), [H|T], Els, [] ).
slice([H|Extra], Els) :- !,
	slice(Extra, Els0),
	add_index_prefix( Els0 , H, Els, [] ).

add_index_prefix( [] , _H ) --> [].
add_index_prefix( [L|Els0] , H ) -->  [[H|L]],
	add_index_prefix( Els0 , H ).


/** @pred matrix_type(+ _Matrix_,- _Type_)



Unify  _NElems_ with the type of the elements in  _Matrix_.

u)(
*/

matrix_type(Matrix,Type) :-
    ( matrix_short_type(Matrix, T) ->
      ( T =:= "i"  -> Type = ints ; T =:= "f" -> Type = floats );
	  Type = terms ).

/** @pred matrix_agg_lines(+ _Matrix_,+Operator,+ _Aggregate_)s



If  _Matrix_ is a n-dimensional matrix, unify  _Aggregate_ with
the n-1 dimensional matrix where eaceh element is obtained by adding all
_Matrix_ elements with same last n-1 index. Currently, only addition is supported.


*/

matrix_agg_lines(M1,+,NM) :-
	do_matrix_agg_lines(M1,0,NM).
/* other operations: *, logprod */

/** @pred matrix_agg_cols(+ _Matrix_,+Operator,+ _Aggregate_)



If  _Matrix_ is a n-dimensional matrix, unify  _Aggregate_ with
the one dimensional matrix where each element is obtained by adding all
Matrix elements with same  first index. Currently, only addition is supported.


*/

matrix_agg_cols(M1,+,NM) :-
	do_matrix_agg_cols(M1,0,NM).
/* other operations: *, logprod */



/* other operations: *, logprod */


matrix_op_to_lines(M1,M2,/,NM) :-
	do_matrix_op_to_lines(M1,M2,3,NM).
/* other operations: *, logprod */

/** @pred matrix_op_to_cols(+ _Matrix1_,+ _Cols_,+ _Op_,- _Result_)



 _Result_ is the result of applying  _Op_ to all elements    _Matrix1_, with the corresponding element in  _Cols_ as the
second argument. Currently, only addition (`+`) is
supported. Notice that  _Cols_ will have n-1 dimensions.

*/
matrix_op_to_cols(M1,M2,+,NM) :-
	do_matrix_op_to_cols(M1,M2,0,NM).
/* other operations: *, logprod */


/** @pred matrix_transpose(+ _Matrix_,- _Transpose_)



Transpose matrix  _Matrix_ to   _Transpose_. Equivalent to:

```
matrix_transpose(Matrix,Transpose) :-
        matrix_shuffle(Matrix,[1,0],Transpose).
```


*/

matrix_transpose(M1,M2) :-
	matrix_shuffle(M1,[1,0],M2).

size(N0, N1, N2) :-
	N2 is N0*N1.

% use 1 to get access to matrix
m_get('$matrix'(Dims, _, Sz, Bases, M), Indx, V) :-
	foldl2(indx, Indx, Dims, Bases, Sz, _, 1, Offset),
	arg(Offset, M, V).


indx( I, Dim, Base, BlkSz, NBlkSz, I0, IF) :-
	NBlkSz is BlkSz div Dim ,
	IF is (I-Base)*NBlkSz + I0.

offset( I, Dim, BlkSz, NBlkSz, Base, I0, IF) :-
	NBlkSz is BlkSz div Dim,
	I is I0 div NBlkSz + Base,
	IF is I0 rem NBlkSz.

first(V,V) :-
    var(V),
    !.
first([H|_],F) :-
    !,
    first(H,F).
first(F,F).

inc(I1, I, I1) :-
	I1 is I+1.
/**                                                                             */


% base case

%%% most often we can avoid meta-calls.

/*
:- multifile user:inline/.

user:inline(matrix_map(P,A),
    (matrix:matrix_size(A,Size), MainCall)):-
    callable(P),
    	  aux_pred(`map1`,P,
		[[0,Size,A], % plugin call
		 [I0,Size,A], % head
		 [A,Index], % inner call
		 [I,Size,A]], % recursive call
	[MainCall, Head, Inner, Recursive]),
	  compile_clauses([(
		Head :-
	             (I0==Size -> true ;
    matrix_offset_to_arg(A, I0, Index),
		     I is I0+1,
		     Inner,
		     Recursive))] ).

user:inline(matrix_map(P,A,B),
    (matrix:matrix_size(A,Size), MainCall)):-
	  callable(P),
    	  aux_pred(`map2`,P,
		[[0,Size,A,B], % plugin call
		 [I0,Size,A,B], % head
		 [A,B,Index], % inner call
		 [I,Size,A,B]], % recursive call
	[MainCall, Head, Inner, Recursive]),
	  compile_clauses([(
		Head :-
	             (I0==Size -> true ;
    matrix_offset_to_arg(A, I0, Index),
		     I is I0+1,
		     Inner,
		     Recursive))] ).




%%% most often we can avoid meta-calls.
user:inline(foldl_matrix(P,A,V0,VF),
    (matrix:matrix_size(A,Size), MainCall)):-
    callable(P),
    	  aux_pred(`foldl`,P,
		[[0,Size,A,V0,VF], % plugin call
		 [I0,Size,A,V0,VF], % head
		 [A,Index,V0,VF], % inner call
		 [I,Size,A,V0,VF]], % recursive call
	[MainCall, Head, Inner, Recursive]),
	  compile_clauses([(
		Head :-
	             (I0==Size -> V0=VF ;
    matrix_offset_to_arg(A, I0, Index),
		     I is I0+1,
		     Inner,
		     Recursive))] ).

aux_pred(Op,P, [CallArgs0, HeadArgs0, InnerArgs0, RecursiveArgs0],
	       [Call, Head, Mod:Inner, Recursive]) :-
	       current_source_module(Mod,Mod),
term_to_string(Mod:P, SG),
    string_concat([Op,`_`,SG], S),
    string_atom(S,F),
    P =.. [G|Args],
    append(Args,CallArgs0,CallArgs),
    Call =.. [F|CallArgs],
    append(Args,HeadArgs0,HeadArgs),
    Head =.. [F|HeadArgs],
    append(Args,InnerArgs0,InnerArgs),
    Inner =.. [G|InnerArgs],
    append(Args,RecursiveArgs0,RecursiveArgs),
    Recursive =.. [F|RecursiveArgs].
*/

/** @pred matrix_new_matrix(+ _Type_,+ _Dims_,+ _List_,- _Matrix_)


Create a new matrix  _Matrix_ of type  _Type_, which may be one of
`ints` or `floats`, with dimensions  _Dims_, base _Base_ and
initialized from list  _List_.


*/
matrix_new_matrix(ints,Dims,Base,Source,O) :-
    flatten(Source,L),
    new_ints_matrix(Dims,Base,L,O).
matrix_new_matrix(floats,Dims,Base,Source,O) :-
    flatten(Source,L),
    new_floats_matrix(Dims,Base ,L,O).



/** @pred matrix_new_matrix(+ _Type_,+ _Dims_,_Base_.- _Matrix_)



Create a new matrix  _Matrix_ of type  _Type_ and base _Base_, which may be one of
`ints` or `floats`, and with a list of dimensions  _Dims_.
The matrix will be initialized to zeros.

```
?- matrix_new_matrix(ints,[2,3],Matrix).

Matrix = {..}
```
Notice that currently YAP will always write a matrix of numbers as `{..}`.


*/
matrix_new_matrix(ints,Dims,Base,O) :-
    new_ints_matrix(Dims,Base,O).
matrix_new_matrix(floats,Dims,Base,O) :-
    new_floats_matrix(Dims,Base,O).
/** @pred matrix_new_set(? _Dims_,+ _OldMatrix_,+ _Value_,- _NewMatrix_)



Create a new matrix  _NewMatrix_ of type  _Type_, with dimensions
 _Dims_ and base _Base_. The elements of  _NewMatrix_ are set to  _Value_.


*/
matrix_new_set(ints,Dims,Base,C,O) :-
    new_ints_matrix_set(Dims,Base,C,O).
matrix_new_set(floats,Dims,Base,C,O) :-
    new_floats_matrix_set(Dims,Base,C,O).

%% @}


%term_expansion((M[I] := P), [(eval(M.[I],V) :- is_matrix(M), matrix:matrix_get(M,[I],V))]) :-
%    !.
%
%term_expansion((M.Op() := P), (eval(M.Op(),V) :- is_matrix(M), matrix:op(M,Op,V))] :-
%
%		   abstract_mat(M) :- is_matrix(M), !.
