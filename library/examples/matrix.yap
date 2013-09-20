
:- use_module(library(matrix)).
:- use_module(library(maplist)).

t1 :-
	X <== matrix([1,2,3,4,5,6],[dim=[3,2]]),
	writeln(X).

t2 :-
	length(L, 10),
	X <== matrix(L, [dim=[2,5]]),
	writeln(X).

t3 :-
	numbers(1, 100, L),
	X <== matrix(L, [dim=[10,10]]),
	Y <== X[1..2+3,_],
	writeln(Y).

t4 :-
	numbers(1, 100, L),
	X <== matrix(L, [dim=[10,10]]),
	X1 <== matrix(X[1..2+3,_], [dim=[2,10]]),
	Y <== [size=size(X1),max=max(X1),min=min(X1)],
	writeln(Y).

numbers(I0, I1, Vals) :-
	( I0 =< I1 -> Vals = [I0|MVals], I01 is I0+1, numbers(I01, I1, MVals) ;
	    Vals = [] ).

t5 :-
	numbers(1, 100, L),
	X <== matrix(L, [dim=[10,10]]),
	writeln('diagonal:'),
	foreach([I in 0..9, J in I..I], Y^(Y <== X[I,J], writeln(Y) ) ).
t6 :-
	Len = 10,
	LenSq is Len*Len,
	Len1 is Len-1,
	numbers(1, LenSq, L),
	X <== matrix(L, [dim=[Len,Len]]),
	Y <== matrix(L, [dim=[Len,Len]]),
	Z <== matrix(L, [dim=[Len,Len]]),
	writeln('product:'),
	foreach([I in 0..Len1, J in 0..Len1], step(X,Y,Z,I,J) ),
	O <== list(Z),
	writeln(O).

step(X,Y,Z,I,J) :-
	Xs <== X[I,_],
	Ys <== Y[_,J],
	foldl(addprod, Xs, Ys, 0, P), % scalar product
	Z[I,J] <== P.

step(X,Y,Z,I,J,S0,SF) :-
	Xs <== X[I,_],
	Ys <== Y[_,J],
	foldl(addprod, Xs, Ys, 0, P), % scalar product
	SF is S0+P,
	Z[I,J] <== P.

addprod(X, Y, S0, S) :-
	S is S0+X*Y.

t7 :-
	t7(10).

t7(Len) :-
	LenSq is Len*Len,
	Len1 is Len-1,
	numbers(1, LenSq, L),
	X <== matrix(L, [dim=[Len,Len]]),
	Y <== matrix(L, [dim=[Len,Len]]),
	Z <== matrix(L, [dim=[Len,Len]]),
	writeln('product:'),
	foreach([I in 0..Len1, J in 0..Len1], step(X,Y,Z,I,J) , 0, O),
	writeln(O).


