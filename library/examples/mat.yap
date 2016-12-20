
:- style_check(all).

:- use_module(library(matrix)).
:- use_module(library(maplist)).

:- initialization( main ).

main :-
	forall( 	between(1, 13,I),
		( writeln(t:xsI), atomic_concat(t,I,G),	call(G) ) ).

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

% core step of matrix multiplication: row I per column J
step(X,Y,Z,I,J) :-
	Xs <== X[I,_], % row I
	Ys <== Y[_,J], % col J
	foldl(addprod, Xs, Ys, 0, P), % scalar product, fold accumulates the result in two last arguments
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

% core step of matrix multiplication: row I per column J
step(X,Y,Z,I,J,S0,SF) :-
	Xs <== X[I,_], % row I
	Ys <== Y[_,J], % col J
	foldl(addprod, Xs, Ys, 0, P), % scalar product, fold accumulates the result
	SF is S0+P, % total sum (checksum)
	Z[I,J] <== P.


t8 :-
	Len is 2*3*4*5,
	L <== 1..Len,
	X <== matrix(L, [dim=[5,4,3,2]]),
	writeln('list:'),
	OL <== list( X ),
	LL <== lists( X ),
	writeln(OL),
	writeln(LL).


t9 :-
	N1 = 1,
	X <== array[0..N1,0..N1] of [1,2,3,4],
	Z <== array[0..N1,0..N1] of _,
	foreach([I in 0..N1, J in I..N1], Z[I,J] <== X[I,J] - X[J,I]),
	O <== list(Z),
	writeln(O).

t10 :-
	N1 = 1,
	X <== array[0..N1,0..N1] of 1:4,
	O <== list(X-2),
	writeln(O),
	O1 <== list(X)+2,
	writeln(O1),
	O2 <== list(X-X),
	writeln(O2).

t11 :-
	N = 3,
	X <== array[1..N,1..N] of 1:9,
	O <== X[1,1],
	writeln(O),
	O1 <== X[2,_],
	writeln(O1),
	O2 <== X[_,2],
	writeln(O2).

t12 :-
	N = 8,
	N2 is N*N,
	X <== array[N,N] of 1:N2,
	N1 is N-1,
	foreach([I in 0..N1, J in 0..N1], plus(X[I,J]), 0, AccF),
	writeln(sum=AccF).

 t13 :-
	N = 2,
	N2 is N*N,
	X  <== array[1..N,1..N] of 1:N2,
	Y  <== array[1..N,1..N] of _,
	Y[1,_] <== X[_,1],
	LX <== list(X),
	LY <== list(Y),
	writeln('x'=LX),
	writeln('y'=LY),
	fail.
t13.
