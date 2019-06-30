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

:- module( matrix,
	   [(<==)/2, op(800, xfx, <==),
	    (+=)/2, op(800, xfx, +=),
	    (-=)/2, op(800, xfx, -=),
	    op(700, xfx, in),
	    op(700, xfx, within),
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
	    foreach/2,
	    foreach/4,
	    op(50, yf, []),
            op(50, yf, '()'),
            op(100, xfy, '.'),
            op(100, fy, '.')
	    ]).

/** @defgroup matrix Matrix Library
@ingroup library
@{

This package provides a fast implementation of multi-dimensional
matrices of integers and floats. In contrast to dynamic arrays, these
matrices are multi-dimensional and compact. In contrast to static
arrays. these arrays are allocated in the stack, and disppear in
backtracking. Matrices are available by loading the library
`library(matrix)`. They are multimensional objects of type:

  + <tt>terms</tt>: Prolog terms

+ <tt>ints</tt>: bounded integers, represented as an opaque term. The
maximum integer depends on hardware, but should be obtained from the
natural size of the machine.

+ <tt>floats</tt>: floating-point numbers, represented as an opaque term.

Matrix elements can be accessed through the `matrix_get/2`
predicate or through an <tt>R</tt>-inspired access notation (that uses the ciao
style extension to `[]`).  Examples include:


  + Access the second row, third column of matrix <tt>X</tt>. Indices start from
`0`,
~~~~
 _E_ <==  _X_[2,3]
~~~~

+ Access all the second row, the output is a list ofe elements.
~~~~
 _L_ <==  _X_[2,_]
~~~~

+ Access all the second, thrd and fourth rows, the output is a list of elements.
~~~~
 _L_ <==  _X_[2..4,_]
~~~~

+ Access all the fifth, sixth and eight rows, the output is a list of elements.
~~~~
 _L_ <==  _X_[2..4+3,_]
~~~~

The matrix library also supports a B-Prolog/ECliPSe inspired `foreach`iterator to iterate over
elements of a matrix:

+ Copy a vector, element by element.

~~~~
 foreach(I in 0..N1, X[I] <== Y[I])
~~~~

+ The lower-triangular matrix  _Z_ is the difference between the
lower-triangular and upper-triangular parts of  _X_.

~~~~
 foreach([I in 0..N1, J in I..N1], Z[I,J] <== X[I,J] - X[I,J])
~~~~

+ Add all elements of a matrix by using  _Sum_ as an accumulator.

~~~~
 foreach([I in 0..N1, J in 0..N1], plus(X[I,J]), 0, Sum)
~~~~

    Notice that the library does not support all known matrix operations. Please
contact the YAP maintainers if you require extra functionality.



+ _X_ <== array[ _Dim1_,..., _Dimn_] of  _Objects_
    The of/2 operator can be used to create a new array of
 _Objects_. The objects supported are:

  + `Unbound Variable`
    create an array of free variables
  + `ints `
    create an array of integers
  + `floats `
    create an array of floating-point numbers
  + `_I_: _J_`
    create an array with integers from  _I_ to  _J_
  + `[..]`
    create an array from the values in a list

The dimensions can be given as an integer, and the matrix will be
indexed `C`-style from  `0..( _Max_-1)`, or can be given
as  an interval ` _Base_.. _Limit_`. In the latter case,
matrices of integers and of floating-point numbers should have the same
 _Base_ on every dimension.


*/


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

/** @pred ?_LHS_ <==  ?_RHS_ is semidet


General matrix assignment operation. It evaluates the right-hand side
 according to the
left-hand side and to the matrix:

+ if  _LHS_ is part of an integer or floating-point matrix,
perform non-backtrackable assignment.
+ other unify left-hand side and right-hand size.


The right-hand side supports the following operators:

+ `[]/2`

    written as  _M_[ _Offset_]: obtain an element or list of elements
of matrix  _M_ at offset  _Offset_.

+ `matrix/1`

    create a vector from a list

+ `matrix/2`

    create a matrix from a list. Options are:
  + dim=
    a list of dimensions

   + type=
    integers, floating-point or terms

  + base=
    a list of base offsets per dimension (all must be the same for arrays of
integers and floating-points

+ `matrix/3`

    create matrix giving two options

+ `dim/1`
  list with matrix dimensions

+ `nrow/1`
  number of rows in bi-dimensional matrix

+ `ncol/1`
  number of columns in bi-dimensional matrix

+ `length/1`
  size of a matrix

+ `size/1`
  size of a matrix

+ `max/1`

    maximum element of a numeric matrix

+ `maxarg/1`

    argument of maximum element of a numeric matrix

+ `min/1`

    minimum element of a numeric matrix

+ `minarg/1`

    argument of minimum element of a numeric matrix

+ `list/1`

    represent matrix as a list

+ `lists/2`

    represent matrix as list of embedded lists

+ `../2`

    _I_.. _J_ generates a list with all integers from  _I_ to
 _J_, included.

+ `+/2`

    add two numbers, add two matrices element-by-element, or add a number to
all elements of a matrix or list.

+ `-/2 `

    subtract two numbers, subtract two matrices or lists element-by-element, or subtract a number from
all elements of a matrix or list

+ `* /2`

    multiply two numbers, multiply two matrices or lists
    element-by-element, or multiply a number from all elements of a
    matrix or list

 + `log/1`

    natural logarithm of a number, matrix or list

+ `exp/1 `

    natural exponentiation of a number, matrix or list

*/
/** @pred matrix_add(+ _Matrix_,+ _Position_,+ _Operand_)



	Add  _Operand_ to the element of  _Matrix_ at position
 _Position_.


*/
/** @pred matrix_agg_cols(+ _Matrix_,+Operator,+ _Aggregate_)



If  _Matrix_ is a n-dimensional matrix, unify  _Aggregate_ with
the one dimensional matrix where each element is obtained by adding all
Matrix elements with same  first index. Currently, only addition is supported.


*/
/** @pred matrix_agg_lines(+ _Matrix_,+Operator,+ _Aggregate_)



If  _Matrix_ is a n-dimensional matrix, unify  _Aggregate_ with
the n-1 dimensional matrix where each element is obtained by adding all
_Matrix_ elements with same last n-1 index. Currently, only addition is supported.


*/
/** @pred matrix_arg_to_offset(+ _Matrix_,+ _Position_,- _Offset_)



Given matrix  _Matrix_ return what is the numerical  _Offset_ of
the element at  _Position_.


*/
/** @pred matrix_column(+ _Matrix_,+ _Column_,- _NewMatrix_)



Select from  _Matrix_ the column matching  _Column_ as new matrix  _NewMatrix_.  _Column_ must have one less dimension than the original matrix.



 */
/** @pred matrix_dec(+ _Matrix_,+ _Position_)



Decrement the element of  _Matrix_ at position  _Position_.


*/
/** @pred matrix_dec(+ _Matrix_,+ _Position_,- _Element_)


Decrement the element of  _Matrix_ at position  _Position_ and
unify with  _Element_.


*/
/** @pred matrix_dims(+ _Matrix_,- _Dims_)



Unify  _Dims_ with a list of dimensions for  _Matrix_.


*/
/** @pred matrix_expand(+ _Matrix_,+ _NewDimensions_,- _New_)



Expand  _Matrix_ to occupy new dimensions. The elements in
 _NewDimensions_ are either 0, for an existing dimension, or a
positive integer with the size of the new dimension.


*/
/** @pred matrix_get(+ _Matrix_,+ _Position_,- _Elem_)



Unify  _Elem_ with the element of  _Matrix_ at position
 _Position_.


*/
/** @pred matrix_get(+ _Matrix_[+ _Position_],- _Elem_)


Unify  _Elem_ with the element  _Matrix_[ _Position_].


*/
/** @pred matrix_inc(+ _Matrix_,+ _Position_)



Increment the element of  _Matrix_ at position  _Position_.


*/
/** @pred matrix_inc(+ _Matrix_,+ _Position_,- _Element_)


Increment the element of  _Matrix_ at position  _Position_ and
unify with  _Element_.


*/
/** @pred matrix_max(+ _Matrix_,+ _Max_)



Unify  _Max_ with the maximum in matrix   _Matrix_.


*/
/** @pred matrix_maxarg(+ _Matrix_,+ _Maxarg_)



Unify  _Max_ with the position of the maximum in matrix   _Matrix_.


*/
/** @pred matrix_min(+ _Matrix_,+ _Min_)



Unify  _Min_ with the minimum in matrix   _Matrix_.


*/
/** @pred matrix_minarg(+ _Matrix_,+ _Minarg_)



Unify  _Min_ with the position of the minimum in matrix   _Matrix_.


*/
/** @pred matrix_ndims(+ _Matrix_,- _Dims_)



Unify  _NDims_ with the number of dimensions for  _Matrix_.


*/
/** @pred matrix_new(+ _Type_,+ _Dims_,+ _List_,- _Matrix_)


Create a new matrix  _Matrix_ of type  _Type_, which may be one of
`ints` or `floats`, with dimensions  _Dims_, and
initialized from list  _List_.


*/
/** @pred matrix_new(+ _Type_,+ _Dims_,- _Matrix_)



Create a new matrix  _Matrix_ of type  _Type_, which may be one of
`ints` or `floats`, and with a list of dimensions  _Dims_.
The matrix will be initialized to zeros.

~~~~~
?- matrix_new(ints,[2,3],Matrix).

Matrix = {..}
~~~~~
Notice that currently YAP will always write a matrix of numbers as `{..}`.


*/
/** @pred matrix_new_set(? _Dims_,+ _OldMatrix_,+ _Value_,- _NewMatrix_)



Create a new matrix  _NewMatrix_ of type  _Type_, with dimensions
 _Dims_. The elements of  _NewMatrix_ are set to  _Value_.


*/
/** @pred matrix_offset_to_arg(+ _Matrix_,- _Offset_,+ _Position_)



Given a position  _Position _ for matrix  _Matrix_ return the
corresponding numerical  _Offset_ from the beginning of the matrix.


*/
/** @pred matrix_op(+ _Matrix1_,+ _Matrix2_,+ _Op_,- _Result_)



 _Result_ is the result of applying  _Op_ to matrix  _Matrix1_
and  _Matrix2_. Currently, only addition (`+`) is supported.


*/
/** @pred matrix_op_to_all(+ _Matrix1_,+ _Op_,+ _Operand_,- _Result_)



 _Result_ is the result of applying  _Op_ to all elements of
 _Matrix1_, with  _Operand_ as the second argument. Currently,
only addition (`+`), multiplication (`\*`), and division
(`/`) are supported.


*/
/** @pred matrix_op_to_cols(+ _Matrix1_,+ _Cols_,+ _Op_,- _Result_)



 _Result_ is the result of applying  _Op_ to all elements of
 _Matrix1_, with the corresponding element in  _Cols_ as the
second argument. Currently, only addition (`+`) is
supported. Notice that  _Cols_ will have n-1 dimensions.


*/
/** @pred matrix_op_to_lines(+ _Matrix1_,+ _Lines_,+ _Op_,- _Result_)



 _Result_ is the result of applying  _Op_ to all elements of
 _Matrix1_, with the corresponding element in  _Lines_ as the
second argument. Currently, only division (`/`) is supported.


*/
/** @pred matrix_select(+ _Matrix_,+ _Dimension_,+ _Index_,- _New_)



Select from  _Matrix_ the elements who have  _Index_ at
 _Dimension_.


*/
/** @pred matrix_set(+ _Matrix_,+ _Position_,+ _Elem_)



Set the element of  _Matrix_ at position
 _Position_ to   _Elem_.


*/
/** @pred matrix_set(+ _Matrix_[+ _Position_],+ _Elem_)


Set the element of  _Matrix_[ _Position_] to   _Elem_.


*/
/** @pred matrix_set_all(+ _Matrix_,+ _Elem_)



Set all element of  _Matrix_ to  _Elem_.


*/
/** @pred matrix_shuffle(+ _Matrix_,+ _NewOrder_,- _Shuffle_)



Shuffle the dimensions of matrix  _Matrix_ according to
 _NewOrder_. The list  _NewOrder_ must have all the dimensions of
 _Matrix_, starting from 0.


*/
/** @pred matrix_size(+ _Matrix_,- _NElems_)



Unify  _NElems_ with the number of elements for  _Matrix_.


*/
/** @pred matrix_sum(+ _Matrix_,+ _Sum_)



Unify  _Sum_ with the sum of all elements in matrix   _Matrix_.


*/
/** @pred matrix_to_list(+ _Matrix_,- _Elems_)



Unify  _Elems_ with the list including all the elements in  _Matrix_.


*/
/** @pred matrix_transpose(+ _Matrix_,- _Transpose_)



Transpose matrix  _Matrix_ to   _Transpose_. Equivalent to:

~~~~~
matrix_transpose(Matrix,Transpose) :-
        matrix_shuffle(Matrix,[1,0],Transpose).
~~~~~


*/
/** @pred matrix_type(+ _Matrix_,- _Type_)



Unify  _NElems_ with the type of the elements in  _Matrix_.


*/

:- load_foreign_files([matrix], [], init_matrix).

:- multifile rhs_opaque/1, array_extension/2.

:- meta_predicate foreach(+,0), foreach(+,2, +, -).

:- use_module(library(maplist)).
:- use_module(library(mapargs)).
:- use_module(library(lists)).

( LHS <== RHS ) :-
    rhs(RHS, LHS, R),
    set_lhs( LHS, R).

rhs('[]'( Dims, array) of X, O, O) :-
    !,
    matrix_new(X,Dims, _, O).
rhs(RHS, O, O) :-
    var(O),
    !,
    rhs(RHS,O).
rhs(RHS, _, O) :-
    rhs(RHS, O).

    /**
      */
    % base case
    rhs(A, O) :-  %  writeln(rhs:A),
atom(A), !, O = A.
    rhs(RHS, O) :- number(RHS), !, O = RHS.
    rhs(RHS, O) :- opaque(RHS), !, O = RHS.
    rhs(RHS, O) :-
        RHS = '$matrix'(_, _, _, _, _),
        !, O = RHS.
    rhs(floats(A,N), floats(A,N)):-
     !.
    rhs('[]'(Index,RHS), V) :-
        !,
        rhs(RHS, NRHS),
        get(  NRHS,Index, V).
    rhs(matrix(List), RHS) :- !,
    	%rhs( List, A1),
	%    	new_matrix(A1, [], RHS).
	length(List,Length),
	matrix_new(floats,[Length], List, RHS).
    rhs(matrix(List, Opt1), RHS) :- !,
    	rhs( List, A1),
    	new_matrix(A1, Opt1, RHS).
    rhs(matrix(List, Opt1, Opt2), RHS) :- !,
    	rhs( List, A1),
    	new_matrix(A1, [Opt1, Opt2], RHS).
    rhs(dim(RHS), Dims) :- !,
    	rhs(RHS, X1),
    	dims( X1, Dims, _ ).
    rhs(dims(RHS), Dims) :- !,
    	rhs(RHS, X1),
    	dims( X1, Dims, _ ).
    rhs(nrow(RHS), NRow) :- !,
    	rhs(RHS, X1),
    	dims( X1, [NRow,_] ).
    rhs(ncol(RHS), NCol) :- !,
    	rhs(RHS, X1),
    	dims( X1, [_,NCol] ).
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
    rhs(sum(RHS), Logs ) :- !,
    	rhs(RHS, X1),
    	(opaque(X1) ->
    	matrix_sum( X1, Logs );
    	X1 = floats(A,N)
    	->
    	address_to_sum(A, N, Logs)
    	).
    rhs(E1+E2, V) :- !,
   	rhs(E1, R1),
    	rhs(E2, R2),
	mplus(R1, R2, V).
    rhs(E1-E2, V) :- !,
    	rhs(E1, R1),
    	rhs(E2, R2),
    	msub(R1, R2, V).
     rhs(E1*E2, V) :- !,
     	rhs(E1, R1),
     	rhs(E2, R2),
 	mtimes(R1, R2, V).
    rhs(E1/E2, V) :- !,
        	rhs(E1, R1),
    	rhs(E2, R2),
	mfdiv(R1, R2, V).
    rhs(-E1, V) :- !,
        	rhs(E1, R1),
	mneg(R1, V).
   rhs(S, NS) :-
   writeln(bad:S),
    	S =.. [N|As],
    	maplist(rhs, As, Bs),
    	NS =.. [N|Bs].
/**
 *  @pred +X <== array[Dims] of T
 *
 *  create an array  of ints or floats, with number of dimensions `length`(Dims). The
 *  array is unified to a logic variable
 *
 */
new( Dims0, T, X) :-
    var(X),
    (  T== ints -> true ; T== floats),
    !,
    foldl( norm_dim, Dims0, Dims, Bases, 1, _Size ),
    matrix_new( T , Dims, _, X ),
    matrix_base(X, Bases).
/**<
 *  @pred +_Name_ <== array[_Dims] of T
 *
 *  create an array  of ints or floats, with number of dimensions `length`(Dims). The
 *  array is available through the name _Name_ and is stored using old-style atomic
 *  arrays (ie, malloc). It is discarded by explicit request.
 *
 */
new( Dims0, T, X) :-
    atom(X),
    (  T== ints -> Type = int ; T== floats -> Type = float),
    !,
    foldl( norm_dim, Dims0, _Dims, _Bases, 1, Size ),
    static_array( X, Size, Type ).
/**<
 *  @pred +_Name_ <== array[_Dims] of T
 *
 *  create an unidimensional vector  of ints, such that the first element is _I_ and the last one _J-1_.
 *  The data is allocated on stack and removed through backtracking.
 *
 */
 new( Dims0, (I:J), X) :-
    var(X),
    integer(I),
    integer(J),
    !,
    foldl( norm_dim, Dims0, Dims, Bases, 1, Size ),
    matrix_seq(I, J, Dims, X),
    matrix_size(X, Size),
    matrix_base(X, Bases).
/**<
 *  @pred +_Name_ <== array[_Dims] of L
 *
 *  Use the elements if list _L_ to create an multi-dimensional array _X_ with dimensions _Dim_. The
 *  dimensions must be compatible with the size of _L_.
 *
 */
 new( Dims0, L, X) :-
    is_list(L),
    !,
    length( L, Size ), !,
	foldl( norm_dim, Dims0, Dims, Bases, 1, Size ),
	X <== matrix( L, [dim=Dims,base=Bases] ).

/**<
  * use array_extension/2 to fetch a goal that given an input _Pattern_ and the dimensionality
  * generates the matrix,
*/
 new( Dims0, Pattern, X) :-
    array_extension(Pattern, Goal),
    !,
    foldl( norm_dim, Dims0, Dims, Bases, 1, Size ),
    call(Goal, Pattern, Dims, Size, L),
    X <== matrix( L, [dim=Dims,base=Bases] ).



norm_dim( I..J, D, I, P0, P) :- !,
	D is J+1-I,
	P is P0*D.
norm_dim( I, I, 0, P0, P ) :-
	P is P0*I.

get( RHS, Args, Val) :-
	rhs(RHS, X1),
	maplist(number, Args),
	!,
	rhs(RHS, X1),
	  get_el( X1, Args, Val ).
get( RHS, Args, Val) :-
	rhs(RHS, X1),
	matrix_dims( X1, Dims, Bases),
	maplist( index(Range), Args, Dims, Bases, NArgs),
    (
	var(Range)
    ->
    matrix_get( M, Args, Val )
    ;
    matrix_get_range( M, NArgs, Val )
    ).




set_el( Mat, Pos, El) :-
    opaque(Mat),
    !,
	matrixn_set( Mat, Pos, El ).
set_el( floats(Address,_Len), [I], El) :-
	!,
     set_float_from_address( Address, I, El).
set_el(X, [I], El) :-
    atom(X),
    !,
    update_static(X, I, El  ).
set_el(M, I, El) :-
    M = '$matrix'(_Dims, _NDims, _Size, _Offsets, _Matrix),
    m_set(M, I, El).

set_lhs(V, V) :-
        !.
set_lhs('[]'(Args, M), Val) :-
    maplist(number, Args),
    !,
    set_el(M,Args,Val).
set_lhs('[]'(Args, M), Val) :-
    dims( M, Dims, Bases),
    maplist( index(Range), Args, Dims, Bases, NArgs),
    (
	var(Range)
    ->
    matrix_set( M, Args, Val )
    ;
    matrix_set_range( M, NArgs, Val )
    ).
set_lhs(V, R) :-
    number(R),
    !,
    (
        opaque(V)
        ->
        matrix_set_all(V, R)
        ;
        matrix_size(V, D),
        D1 is D-1,
        	forall( between(0,D1,I) , set_el( V, [I], R) )
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
	 (matrix(I1) ->
	    ( number(I2) -> matrix_op_to_all(I1, +, I2, V) ;
	      matrix(I2) ->  matrix_op(I1, I2, +, V) ;
	      V is I1+I2 ) ;
	 is_list(I1) ->
	    ( number(I2) -> maplist(plus(I2), I1, V) ;
	      is_list(I2) ->  maplist(plus, I1, I2, V) ;
	      V is I1+I2 ) ;
	    V is I1 +I2
	    ).

msub(I1, I2, V) :-
        	 (matrix(I1) ->
        	    ( number(I2) -> matrix_op_to_all(I1, -, I2, V) ;
        	      matrix(I2) ->  matrix_op(I1, I2, -, V) ;
        	      V is I1-I2 ) ;
        	 is_list(I1) ->
        	    ( number(I2) -> maplist(minus(I2), I1, V) ;
        	      is_list(I2) ->  maplist(minus, I1, I2, V) ;
        	      V is I1-I2 ) ;
        	    V is I1-I2
        	    ).



mtimes(I1, I2, V) :-
        	 (matrix(I1) ->
        	    ( number(I2) -> matrix_op_to_all(I1, *, I2, V) ;
        	      matrix(I2) ->  matrix_op(I1, I2, *, V) ;
        	      V is I1*I2 ) ;
        	 is_list(I1) ->
        	    ( number(I2) -> maplist(times(I2), I1, V) ;
        	      is_list(I2) ->  maplist(times, I1, I2, V) ;
        	      V is I1*I2 ) ;
        	    V is I1*I2
        	    ).

mfdiv(I1, I2, V) :-
        	 (matrix(I1) ->
        	    ( number(I2) -> matrix_op_to_all(I1, /, I2, V) ;
        	      matrix(I2) ->  matrix_op(I1, I2, /, V) ;
        	      V is I1/I2 ) ;
        	 is_list(I1) ->
        	    ( number(I2) -> maplist(div(I2), I1, V) ;
        	      is_list(I2) ->  maplist(div, I1, I2, V) ;
        	      V is I1/I2 ) ;
        	    V is I1/I2
        	    ).


mneg(I1, V) :-
        	 (matrix(I1) ->
        	    matrix_op_to_all(I1, *, -1, V) ;
        	 is_list(I1) ->
        	     maplist(mult(-1), V) ;
        	    V is -I1
        	    ).

%
% three types of matrix: integers, floats and general terms.
%

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
	    Mat = '$matrix'( _, _, Size, _, _) ;
	     atom(Mat) -> static_array_properties(Mat, Size, _);
	     Mat = floats(_M,Size)).

matrix_to_list( Mat, ToList) :-
	( opaque(Mat) -> matrixn_to_list( Mat, ToList ) ;
	    Mat = '$matrix'( _, _, _, _, M), M=.. [_|ToList] ;
	     atom(Mat) -> static_array_to_term(Mat,M), M=.. [_|ToList];
	     Mat = floats(M,N) -> address_to_list(M,N, ToList)).

matrix_to_lists( Mat, ToList) :-
	matrix_dims( Mat, [D|Dims] ),
	D1 is D-1,
	foreach( I in 0..D1, matrix_slicer( Dims, Mat, [I|L]-L), ToList, [] ).

matrix_slicer( [_], M, Pos-[_], [O|L0], L0) :- !,
	O <== '[]'(Pos,M).
matrix_slicer( [D|Dims], M, Pos-[I|L], [O|L0], L0) :-
	D1 is D-1,
	foreach( I in 0..D1 , L^matrix_slicer( Dims, M, Pos-L), O, [] ).

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


matrix_set( Mat, Pos, Els) :-
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
    dims(Matrix, Bases).

matrix_arg_to_offset(M, Index, Offset) :-
	( opaque(M) -> matrixn_arg_to_offset( M, Index, Offset ) ;
	  M = '$matrix'(Dims, _, Size, Bases, _) ->
	  foldl2(indx, Index, Dims, Bases, Size, _, 0, Offset) ;
	  Index = [Offset]).

matrix_offset_to_arg(M, Offset, Index) :-
	( opaque(M) -> matrixn_offset_to_arg( M, Offset, Index ) ;
	    M = '$matrix'(Dims, _, Size, Bases, _) ->
	    foldl2(offset, Index, Dims, Bases, Size, _, Offset, _) ;
	  Index = [Offset]     ).

matrix_max(M, Max) :-
	( opaque(M) -> matrixn_max( M, Max ) ;
	    M = '$matrix'(_, _, _, _, C) ->
	  arg(1,C,V0), foldargs(max, M, V0, Max) ;
	  matrix_to_list(M, [V0|L]) , foldl(max, L, V0, Max) ).

max(New, Old, Max) :- ( New >= Old -> New = Max ; Old = Max ).

matrix_maxarg(M, MaxArg) :-
	( opaque(M) -> matrixn_maxarg( M, MaxArg );
	    M = '$matrix'(_, _, _, _, C) ->
	  arg(1,C,V0), foldargs(maxarg, M, V0-0-0, _-Offset-_), matrix_offset_to_arg(M, Offset, MaxArg) ;
	  matrix_to_list(M, [V0|L]) , foldl(maxarg, L, V0-0-1, _Max-Off-_ ), MaxArg = [Off] ).

maxarg(New, Old-OPos-I0, Max-MPos-I) :- I is I0+1, ( New > Old -> New = Max, MPos = I0 ; Old = Max, MPos = OPos ).

matrix_min(M, Min) :-
	( opaque(M) -> matrixn_min( M, Min ) ;
	    M = '$matrix'(_, _, _, _, C) ->
	  arg(1,C,V0), foldargs(min, M, V0, Max) ;
	  matrix_to_list( M, [V0|L]) , foldl(min, L, V0, Max) ).

min(New, Old, Max) :- ( New =< Old -> New = Max ; Old = Max ).

matrix_minarg(M, MinArg) :-
	( opaque(M) -> matrixn_minarg( M, MinArg );
	    M = '$matrix'(_, _, _, _, C) ->
	  arg(1,C,V0), foldargs(minarg, M, V0-0-0, _-Offset-_), matrix_offset_to_arg(M, Offset, MinArg) ;
	  matrix_to_list(M, [V0|L]) , foldl(minarg, L, V0-0-1, _Min-Off-_ ), MinArg = [Off] ).

minarg(New, Old-OPos-I0, Min-MPos-I) :- I is I0+1, ( New < Old -> New = Min, MPos = I0 ; Old = Min, MPos = OPos ).

matrix_to_logs(M, LogM) :-
	( opaque(M) -> matrixn_to_logs( M, LogM ) ;
	    M = '$matrix'(A, B, D, E, C) ->
	  LogM = '$matrix'(A, B, D, E, LogC),
	  mapargs(log, C, LogC) ;
	  matrix_to_list(M, [V0|L]) -> maplist(log, [V0|L], LogM ) ;
	  LogM is log(M) ).

log(X, Y) :- Y is log(X).

matrix_to_exps(M, ExpM) :-
	( opaque(M) -> matrixn_to_exps( M, ExpM ) ;
	    M = '$matrix'(A, B, D, E, C) ->
	  ExpM = '$matrix'(A, B, D, E, ExpC),
	  mapargs(exp, C, ExpC) ;
	  matrix_to_list(M, [V0|L]) -> maplist(exp, [V0|L], ExpM ) ;
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
	  do_matrix_op(M1,M2,5,NM) ;
                                	  matrix_m(M1, '$matrix'(A,B,D,E,C1)),
                                	  matrix_m(M2, '$matrix'(A,B,D,E,C2)),
                                	  mapargs(div, C1, C2, C),
                                	  NM = '$matrix'(A,B,D,E,C)
	 ).


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

foreach( Domain, Goal) :-
	strip_module(Goal, M, Locals^NG), !,
	term_variables(Domain+Locals, LocalVarsL),
	LocalVars =.. [vs|LocalVarsL],
	iterate( Domain, [], LocalVars, M:NG, [], [] ),
	terms:reset_variables( LocalVars ).
foreach( Domain, Goal ) :-
	strip_module(Goal, M, NG),
	term_variables(Domain, LocalVarsL),
	LocalVars =.. [vs|LocalVarsL],
	iterate( Domain, [], LocalVars, M:NG, [], [] ),
	terms:reset_variables( LocalVars ).

foreach( Domain, Goal, Inp, Out) :-
	strip_module(Goal, M, Locals^NG), !,
	term_variables(Domain+Locals, LocalVarsL),
	LocalVars =.. [vs|LocalVarsL],
	iterate( Domain, [], LocalVars, M:NG, [], [], Inp, Out).
foreach( Domain, Goal, Inp, Out ) :-
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

%
% given an object, report it dimensionality
%
dims( X, Dims, Bases) :-
    opaque(X),
    !,
	matrix_dims( X, Dims, Bases).
dims( array(Address,Len), [Len], [0]) :-
    integer(Address),
	!.
dims( X, [N], [0]) :-
    atom(X),
    !,
	static_array_properties(X, N, _).
dims('$matrix'(Dims, _NDims, _Size, Offsets, _Matrix), Dims, Offsets).

get_el( X, I, El) :-
    opaque(X),
    !,
	matrix_get( X, I, El).
get_el( floats(Address,_Len), [I], El) :-
    integer(Address),
	!,
	get_float_from_address( Address, I, El).
get_el( X, N, V) :-
    atom(X),
    !,
	array_element(X, N, V).
get_el(M, I, El) :-
    M = '$matrix'(_Dims, _NDims, _Size, _Offsets, _Matrix),
    m_get(M, I, El).

zero(_, 0).
/** @} */

