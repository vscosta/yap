
:- module(bparrays, [new_array/2, a2_new/3, a3_new/4, is_array/1, '$aget'/3]).

:- use_module(library(lists), [flatten/2]).

new_array(X, Dim.Dims) :-
	     functor(X, '[]', Dim),
	     recurse_new_array(0, Dim, Dims, X).

recurse_new_array(_, _, [], _X) :- !.
recurse_new_array(Dim, Dim, _Dims, _X) :- !.
recurse_new_array(I0, Dim, Dims, X) :-
             I is I0+1,
	     arg(I, X, A),
             new_array(A, Dims),
	     recurse_new_array(0, Dim, Dims, X).

a2_new(X, Dim1, Dim2) :-
	     functor(X, '[]', Dim1),
	     recurse_new_array(0, Dim1, [Dim2], X).
	  
a2_new(X, Dim1, Dim2, Dim3) :-
	     functor(X, '.', Dim1),
	     recurse_new_array(0, Dim1, [Dim2,Dim3], X).

is_array(X) :-
	     functor(X, '[]', _Dim).

'$aget'(A,[],A).
'$aget'(A,I.Is,A) :-
	     arg(I, A, X),
             '$aget'(X,Is,A).

array_to_list(A, List) :-
	 flatten(A, List).
