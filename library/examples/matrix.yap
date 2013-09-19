
:- use_module(library(matrix)).

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

