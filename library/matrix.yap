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
	    (+==)/2, op(800, xfx, +==),
	    (-==)/2, op(800, xfx, -==),
	   op(950,fy,:=),
	   op(950,yfx,:=),
%	   op(950,fx,<-),
%	   op(950,yfx,<-),
	    op(700, xfx, in),
	    op(700, xfx, within),
	    op(700, xfx, ins),
        op(450, xfx, '..'), % should bind more tightly than \/
	op(790, fx, matrix),
	op(790, fx, array),
	op(780, xfx, of),
	is_matrix/1,
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
	    matrix_set/2,
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


:- load_foreign_files([matrix], [], init_matrix).

:- multifile rhs_opaque/1, array_extension/2.

/** @defgroup YapMatrix YAP Matrix Library
@ingroup YAPLibrary
@{

This package provides a fast implementation of multi-dimensional
matwrices of integers and floats. In contrast to dynamic arrays, these
matrices are multi-dimensional and compact. In contrast to static
arrays. these arrays are allocated in the stack, and disppear in
backtracking. Matrices are available by loading the library
`library(matrix)`. They are multimensional objects of type:

  + <tt>terms</tt>: Prolog terms

+ <tt>ints</tt>: bounded integers, represented as an opaque term. The
maximum integer depends on hardware, but should be obtained from the
natural size of the machine.

+ <tt>floats</tt>: floating-point numbers, represented as an opaque term.


The matrix library also supports a B-Prolog/ECliPSe inspired `foreach`iterator to iterate over
elements of a matrix:

+ Copy a vector, element by element.

```
 foreach(I in 0..N1, X[I] <== Y[I])
```

+ The lower-triangular matrix  _Z_ is the difference between the
lower-triangular and upper-triangular parts of  _X_.

```
 foreach([I in 0..N1, J in I..N1], Z[I,J] <== X[I,J] - X[I,J])
```

+ Add all elements of a matrix by using  _Sum_ as an accumulator.

```
 foreach([I in 0..N1, J in 0..N1], plus(X[I,J]), 0, Sum)
```

    Notice that the library does not support all known matrix operations. Please
contact the YAP maintainers if you require extra functionality.




/** @pred matrix_arg_to_offset(+ _Matrix_,+ _Position_,- _Offset_)



Given matrix  _Matrix_ return what is the numerical  _Offset_ of
the element at  _Position_.


*/
/** @pred matrix_column(+ _Matrix_,+ _Column_,- _NewMatrix)_



Select from  _Matrix_ the column matching  _Column_ as new matrix  _NewMatrix_.  _Column_ must have one less dimension than the original matrix.



 */

/** @pred matrix_new(+ _Type_,+ _Dims_,+ _List_,- _Matrix_)


Create a new matrix  _Matrix_ of type  _Type_, which may be one of
`ints` or `floats`, with dimensions  _Dims_, and
initialized from list  _List_.


*/
matrix_new(ints,Dims,Source,O) :-
    new_matrix_ints(Dims,Source,O).
matrix_new(floats,Dims,Source,O) :-
    new_matrix_floats(Dims,Source,O).
/** @pred matrix_new(+ _Type_,+ _Dims_,- _Matrix_)



Create a new matrix  _Matrix_ of type  _Type_, which may be one of
`ints` or `floats`, and with a list of dimensions  _Dims_.
The matrix will be initialized to zeros.

```
?- matrix_new(ints,[2,3],Matrix).

Matrix = {..}
```
Notice that currently YAP will always write a matrix of numbers as `{..}`.


*/
matrix_new(ints,Dims,O) :-
    new_matrix_ints_set(Dims,0,O).
matrix_new(floats,Dims,O) :-
    new_matrix_floats(Dims,0,O).
/** @pred matrix_new_set(? _Dims_,+ _OldMatrix_,+ _Value_,- _NewMatrix_)



Create a new matrix  _NewMatrix_ of type  _Type_, with dimensions
 _Dims_. The elements of  _NewMatrix_ are set to  _Value_.


*/
matrix_new_set(ints,Dims,C.O) :-
    new_matrix_ints_set(Dims,C,O).
matrix_new_set(floats,Dims,C,O) :-
    new_matrix_floats(Dims,C,O).
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
/** @pred matrix_select(+ _Matrix_,+ _Dimension_,+ _Index_,- _New_)



Select from  _Matrix_ the elements who have  _Index_ at
 _Dimension_.


*/
/** @pred matrix_shuffle(+ _Matrix_,+ _NewOrder_,- _Shuffle_)



Shuffle the dimensions of matrix  _Matrix_ according to
 _NewOrder_. The list  _NewOrder_ must have all the dimensions of
 _Matrix_, starting from 0.


*/



:- load_foreign_files([matrix], [], init_matrix).

:- multifile rhs_opaque/1, array_extension/2.


:- meta_predicate foreach(+,0), foreach(+,2, +, -).
:- use_module(library(maplist)).
:- use_module(library(mapargs)).
:- use_module(library(lists)).

%term_expansion((M[I] := P), [(eval(M.[I],V) :- is_matrix(M), matrix:matrix_get(M,[I],V))]) :-
%    !.
%
%term_expansion((M.Op() := P), (eval(M.Op(),V) :- is_matrix(M), matrix:op(M,Op,V))] :-
%
%		   abstract_mat(M) :- is_matrix(M), !.

%% @pred LHS ==> RHS
%%
%% Matrix operation
%%
%% 1 Initialization:
%%
%%    - One can use use old array routines (check static_array/3) for
%%      offline arrays/
%%
%%    1. `Atom <== [1000,1001,1002]`
%%    1. `Atom <== matrix[1000] of int`
%%    2. `Atom <== matrix[333] of 0`
%%
%% The opaque family of routines allocates on stack: 
%%    1. `Var <== [[1000],[1001],[1002]]`
%%    1. `Var <== matrix[1000] of term`
%%    2. `Var <==  matrix[333,2] of 3.1415`
%%
%%  YAP also supports foreign matrices.

/** @pred ?_LHS_ <==  ?_RHS_ is semidet


General matrix assignment operation. It evaluates the right-hand side
 according to the
left-hand side and to the matrix:

+ if  _LHS_ is part of an integer or floating-point matrix,
perform non-backtrackable assignment.
+ other unify left-hand side and right-hand size.

Matrix elements can be accessed through the `matrix_get/2` predicate
or through an <tt>R</tt>-inspired access notation (that uses the ciao
style extension to `[]`).  Examples include:


  + Access the second row, third column of matrix <tt>X</tt>. Indices start from
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

+ `[]/2`

    written as  _M_[ _Offset_]: obtain an element or list of elements
of matrix  _M_ at offset  _Offset_.

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


+ _X_ <== array[ _Dim1_,..., _Dimn_] of  _Objects_
    The of/2 operator can be used to create a new array of
 _Objects_. The objects supported are:

  + `Unbound Variable`
    create an array of free variables
  + `ints`
    create an array of integers
  + `floats`
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
( O <== RHS ) :-
    var(O),
    !,
    eval(RHS,O).
( LHS[Off] <== RHS ) :-
    eval(RHS,V),
    (V=[N] -> true ; number(V) -> V=N),
    !,
    matrix_set_one(LHS, [Off],N).
( LHS <== matrix(RHS) ) :-
    new(matrix(RHS),LHS),
    !.
( LHS <== zeros(RHS) ) :-
    new(zeros(RHS),LHS),
    !.
( LHS <== ones(RHS) ) :-
    new(ones(RHS),LHS),
    !.
( LHS <== range(RHS) ) :-
    new(range(RHS),LHS),
    !.
( LHS <== RHS ) :-
    eval(RHS,V),
    matrix_set(LHS,V),
    !.

( LHS[Off] +== 1 ) :-
    eval(LHS,M),
    matrix_inc(M,[Off]),
    !.
( LHS +== RHS ) :-
    eval(LHS+RHS,V),
    matrix_set(LHS,V),
    !.


( LHS[Off] -== 1 ) :-
    eval(LHS,M),
    !,
    matrix_dec(M,[Off]).
( LHS -== RHS ) :-
    eval(LHS-RHS,V),
    matrix_set(LHS,V),
    !.


kindofm(matrix(_)) :- !.


eval(V,V) :- var(V), !.

eval(V,V) :- number(V), !.

eval(matrix(M),matrix(M)) :- !.

eval(M,M) :- is_matrix(M), !.

eval(M[I],Exp) :-
    matrix_get(M,[I],Exp),
    !.

eval(Matrix.dims(), V) :- !,
     matrix_dims(Matrix, V).  /**>  list with matrix dimensions */


 eval(Matrix.sum(), V) :- !,matrix_sum(Matrix, V).  /**>  lisT with matrix dimensions */

eval(Matrix.nrow(), V) :- !,matrix_nrow(Matrix, V).  /**>  number of rows in bi-dimensional matrix */

eval(Matrix.ncol(), V) :- !,matrix_ncol(Matrix, V).  /**>  number of columns in bi-dimensional matrix */

eval(Matrix.length(), V) :- !,matrix_size(Matrix, V).  /**>  size of a matrix*/

eval(Matrix.size(), V) :- !,matrix_size(Matrix, V).  /**>  size of a matrix*/

eval(Matrix.max(), V) :- !,matrix_max(Matrix, V).  /**>    maximum element of a numeric matrix*/

eval(Matrix.maxarg(), V) :- !, matrix_maxarg(Matrix, V).  /**    argument of maximum element of a numeric matrix*/

eval(Matrix.min(), V) :- !, matrix_min(Matrix, V).  /**    minimum element of a numeric matrix*/

eval(Matrix.minarg(), V) :- !, matrix_minarg(Matrix, V).  /**>    argument of minimum element of a numeric matrix*/

eval(Matrix.list(), V) :- !, matrix_to_list(Matrix, V).  /**>    represent matrix as a list*/

eval(Matrix.lists(), V) :- !, matrix_to_lists(Matrix, V).  /**> represent matrix as a list of lists */

eval(A+B, C) :- 
    matrix_op(A, B, +, C), !.  /**> sq */

eval(A-B, C) :- 
    matrix_op(A, B, -, C), !.  /**> subtract lists */

eval(A*B, C) :- 
    matrix_op(A, B, *, C), !.  /**> represent matrix as a list of lists */

eval(A/B, C) :- 
    matrix_op(A, B, /, C), !.  /**> represent matrix as a list of lists */


eval(Cs,Exp) :-
  Cs =.. [Op,X],
  eval(X,NX),
N=..[Op,NX],
Exp is N.

eval(Cs,Exp) :-
  Cs =.. [Op,X,Y],
  eval(X,NX),
  eval(Y,NY),
N=..[Op,NX,NY],
Exp is N.


/**
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

*/


new(matrix {Info}, Target) :-
    !,
    storage({Info}, Target).
new(mat( Stuff ), Target) :-
    !,
    new(matrix Stuff, Target).
new(matrix {Info}[Dims], Target) :-
    !,
    new(matrix {dim=Dims,Info}, Target).
new(matrix[Dims] of ints, Target) :-
    !,
    mk_data(0, ( dim=[Dims], type = i, exists=a), Info),
    new(matrix( {Info} ), Target).
new(matrix[Dims] of floats, Target) :-
    !,
    mk_data(0, ( dim=[Dims], type = i, exists=a), Info),
    new(matrix( {Info} ), Target).
new(matrix[Dims] of C, Target) :-
    integer(C),
   !,
    mk_data(C,(dim=[Dims], type = f, exists=b), Info),
    new(matrix( {Info} ), Target).
new(matrix[Dims] of C, Target) :-
    float(C),
    !,
    mk_data(C,(dim=[Dims], type = f, exists=b), Info),
    new(matrix( {Info} ), Target).
new(matrix L, Target) :-
    is_list(L),
    !,
    subl(L,Dim),
    mk_data(L, (dim=Dim), Info),
    new(matrix( {Info} ), Target).

new(zeros( Dims ), Target) :-
    new(matrix[Dims] of 0, Target).

new(ones( Dims ), Target) :-
    new(matrix[Dims] of 1, Target).

new( range(I) , Target ) :-
    r(0,I,1,Data),
    new( matrix Data, Target).
new( range(I,J) , Target ) :-
    r(I,J,1,Data),
    new( matrix Data, Target).
new( range(I,J,Step) , Target ) :-
    r(I,J,Step,Data),
    new( matrix Data, Target).

r(I,J,S,L) :-
    I < J,
    findall(O, r_(I,J,S,O), L).

r_(I,_J,_S,I).
r_(I,J,S,N) :-
    I1 is I+S,
    I1 =< J,
    r_(I1,J,S,N).

%%
%% extract info from data
%%
mk_data(int, Info,NInfo) :-
    !,
    ( find(type = _, Info) -> NInfo = Info ; NInfo = (type=i, Info) ).
mk_data(integer, Info,NInfo) :-
    !,
    ( find(type = _, Info) -> NInfo = Info ; NInfo = (type=i, Info) ).
mk_data(ints, Info,NInfo) :-
    !,
    ( find(type = _, Info) -> NInfo = Info ; NInfo = (type=i, Info) ).
mk_data(integers, Info,NInfo) :-
    !,
    ( find(type = _, Info) -> NInfo = Info ; NInfo = (type=i, Info) ).
mk_data(float, Info,NInfo) :-
    !,
    ( find(type = _, Info) -> NInfo = Info ; NInfo = (type=f, Info) ).
mk_data(double, Info,NInfo) :-
    !,
    ( find(type = _, Info) -> NInfo = Info ; NInfo = (type=f, Info) ).
mk_data(floats, Info,NInfo) :-
    !,
    ( find(type = _, Info) -> NInfo = Info ; NInfo = (type=f, Info) ).
mk_data(doubles, Info,NInfo) :-
    !,
    ( find(type = _, Info) -> NInfo = Info ; NInfo = (type=f, Info) ).
mk_data(C,Info, NInfo) :-
     number(C),
     !,
     ( find(type = _, Info) -> NInfo = MInfo ;
      lub(C,T), MInfo = (type=T, Info) ),
     ( find(data = _, Info) -> NInfo = MInfo ;
       find(filler = _, Info) -> NInfo = MInfo ;
       NInfo = (filler=C, MInfo) ).
mk_data(L,Info, NInfo) :-
     is_list(L),
     !,
     ( find(type = _, Info) -> NInfo = MInfo ;
      lub(L,T), MInfo = (type=T, Info) ),
     ( find(data = _, Info) -> NInfo = MInfo ;
       find(filler = _, Info) -> NInfo = MInfo ;
       flatten(L,Flat),
       NInfo = (data=Flat, MInfo) ).

dims(Info,_,Info) :-
    find(dim=_,Info),
     !.
dims(Info, Data, (dim=Dims,Info)) :-
    is_list(Data),
    subl(Data,Dims).

subl([H|L],[N|NL]) :-
    !,
    length([H|L],N),
    subl(H,NL).
subl(_,[]).

find(Key=Val,{Key=Val,_}) :-
    !.
find(Key=Val,{_,Dict}) :-
    !,
    find(Key=Val, {Dict}).
 find(Key=Val,{Key=Val}).


find_with_default(Key,Dict,Val) :-
    find(Key=Val, Dict),
    !.
find_with_default(Key,_Dict,Val) :-
    default(Key,Val).

default(dim,[]).
default(type,t).
default(data,[]).
default(filler,[]).

lub([], b).
lub([H|L], Lub) :-
    lub(H, Lub1),
    lub(L, Lub2),
    !,
    lub_op(Lub1, Lub2, Lub).
lub(H, L) :-
      l(H,L).
    
lub_op(b, T, T):- !.
lub_op(T, b, T):- !.
lub_op(i, T, T):- !.
lub_op(T, i, T):- !.
lub_op(f, T, T):- !.
lub_op(T, f, T):- !.
lub_op(t, t, t):- !.

l(true, b).
l(false, b).
l(i, i).
l(f, f).
l(I, i) :- integer(I).
l(F, f) :- float(F).
l(_, t).

%%> Static Array of ints with: data source, constant initializer, default
storage(Info, Mat) :-
    (atom(Mat)->Loc=static_array;Loc = opaque),
    spec(Info,Type,Loc,Data,Filler,Dims),
    schedule(Type,Loc,Data,Filler,Dims,Mat).

spec(Info,Type,_Loc,Data,Filler,Dims) :-
    find_with_default(type, Info, Type),
    (
	find(data=Data, Info)
    ->
    true
    ;
    Data = [],
    find_with_default(filler, Info, Filler)
    ),
    find_with_default(dim, Info, Dims).

schedule(i,static_array,[H|L],_,[Sz], Mat) :-
    static_array(Mat,Sz,int),
    !,
    matrix_set(Mat,[H|L]).
schedule(i,static_array,_,0,[Sz], Mat) :-
    static_array(Mat,Sz,int),
    !.
schedule(i,static_array,_,C,[Sz], Mat) :-
    static_array(Mat,Sz,int),
    update_whole_array(Mat,C).


%%> OPaque matrix of ints with: data source, constant initializer, default
schedule(i,_,Data,_,Dims,Mat) :-
    Data = [_|_],
    length(Dims,NDims),
    new_ints_matrix(NDims,Dims,Data,Mat).
schedule(i,_,_,0,Dims,Mat) :-
    length(Dims,NDims),
    new_ints_matrix(NDims,Dims,[],Mat).
schedule(i,_,_,C,Dims,Mat) :-
    number(C),
    !,
    length(Dims,NDims),
    new_ints_matrix_set(NDims,Dims,C,Mat).
%%> OPaque matrix of float
schedule(f,_,Data,_,Dims,Mat) :-
    Data = [_|_],
    length(Dims,NDims),
    new_ints_matrix(NDims,Dims,Data,Mat).
schedule(f,_,_,0,Dims,Mat) :-
    length(Dims,NDims),
    new_ints_matrix(NDims,Dims,[],Mat).
schedule(f,_,_,C,Dims,Mat) :-
    number(C),
    !,
    length(Dims,NDims),
    new_ints_matrix_set(NDims,Dims,C,Mat).
storage(_,_,_,[H|Dims], '$matrix'([H|Dims], NDims, Size, Offsets, Matrix) ) :-
     length([H|Dims],NDims),
    multiply(Dims,H,Size),
    functor(Matrix,matrix,Size),
    Offsets=0.

zip(A-B0,A,B0,B1) :- B1 is B0+1.

multiply([],P,P).
multiply([X|Xs],P0,P) :-
    Pi is X*P0,
    myltiply(Xs,Pi,P).
    

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

  /**                                                                                                                                                                                                                                                                                                        
      */
% base case

matrix_set(V, VF) :-
    var(V),
    !,
    V = VF.
matrix_set(M,V) :-
    is_matrix(M),
    !,
    (is_matrix(V) ->
	 matrix_copy(V,M)
    ;
    matrix_set_all(M,V)
     ).
matrix_set(M[Args], Val) :-
    !,
    matrix_set(M,[Args],Val).

matrix_set(M, Args, Val) :-
    maplist(integer, Args),
    !,
    matrix_set_one(M,Args,Val).
matrix_set(M, Args, Val) :-
    dims( M, Dims, Bases),
    maplist( index(Range), Args, Dims, Bases, NArgs),
    (
	var(Range)
    ->
    set_el  ( M, Args, Val )
    ;
    matrix_set_range( M, NArgs, Val )
    ).


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


matrix_to_lists( Mat, ToList) :-
	matrix_dims( Mat, [D|Dims] ),
	D1 is D-1,
	foreach( I in 0..D1, matrix_slicer( Dims, Mat, [I|L]-L), ToList, [] ).

matrix_slicer( [_], M, Pos-[_], [O|L0], L0) :- !,
	O <== '[]'(Pos,M).
matrix_slicer( [D|Dims], M, Pos-[I|L], [O|L0], L0) :-
	D1 is D-1,
	foreach( I in 0..D1 , L^matrix_slicer( Dims, M, Pos-L), O, [] ).

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
	slice(Pos, Keys),
	maplist( matrix_get_one(Mat), Keys, Els).

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


/** @pred matrix_type(+ _Matrix_,- _Type_)



Unify  _NElems_ with the type of the elements in  _Matrix_.


*/

matrix_type(Matrix,Type) :-
	( matrix_type_as_number(Matrix, 0) -> Type = ints ;
	  opaque( Matrix ) -> Type = floats ;
	  Type = terms ).

matrix_base(Matrix, Bases) :-
    dims(Matrix, Bases).

/** @pred matrix_agg_lines(+ _Matrix_,+Operator,+ _Aggregate_)s



If  _Matrix_ is a n-dimensional matrix, unify  _Aggregate_ with
the n-1 dimensional matrix where each element is obtained by adding all
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
	; M1 == NM, M1 = floats(Ad,Sz) ->
	  address_op_to_all(Ad,Sz,0,Num)
	;
	  M1 = '$matrix'(A,B,D,E,C),
	  mapargs(plus(Num), C, NC),
	  NM = '$matrix'(A,B,D,E,NC)
	).
matrix_op_to_all(M1,-,Num,NM) :-
	( opaque(M1) ->
	  do_matrix_op_to_all(M1,1,Num,NM)
	; M1 == NM, M1 = floats(Ad,Sz) ->
	  address_op_to_all(Ad,Sz,1,Num)
	;
	  M1 = '$matrix'(A,B,D,E,C),
	  mapargs(minus(Num), C, NC),
	  NM = '$matrix'(A,B,D,E,NC)
	  ).
matrix_op_to_all(M1,*,Num,NM) :-
	( opaque(M1) ->
	  do_matrix_op_to_all(M1,2,Num,NM)
	; M1 == NM, M1 = floats(Ad,Sz) ->
	 address_op_to_all(Ad,Sz,2,Num)
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
	; M1 == NM, M1 = floats(Ad,Sz) ->
	  address_op_to_all(Ad,Sz,3,Num)
	;
	  M1 = '$matrix'(A,B,D,E,C),
	  mapargs(div(Num), C, NC),
	  NM = '$matrix'(A,B,D,E,NC)
	).

/* other operations: *, logprod */

matrix_op_to_lines(M1,M2,/,NM) :-
	do_matrix_op_to_lines(M1,M2,3,NM).
/* other operations: *, logprod */

/** @pred matrix_op_to_cols(+ _Matrix1_,+ _Cols_,+ _Op_,- _Result_)



 _Result_ is the result of applying  _Op_ to all elements of
 _Matrix1_, with the corresponding element in  _Cols_ as the
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

inc(I1, I, I1) :-
	I1 is I+1.
/**                                                                                                                                                                                                                                                                                                                    
      */
% base case



/** @} */

