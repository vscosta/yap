:- module(adder,[sum/3]).

sum([B],[S],(S==B)).
sum([B1,B2|Bs],Sum,F1*F2*F3) :-
	split([B1,B2|Bs],Xs,Ys),
	sum(Xs,S1,F1), sum(Ys,S2,F2),add(S1,S2,Sum,F3).

split([],[],[]).
split([X],[X],[0]).
split([X,Y|XYs],[X|Xs],[Y|Ys]) :- split(XYs,Xs,Ys).

add([X|Xs],[Y|Ys],[Z|Zs],(Z==SumXY)*Sum) :-
	halfadder(X,Y,SumXY,CarryXY),
	adder(Xs,Ys,CarryXY,Zs,Sum).

adder([],[],Carry,[Z],Z==Carry).
adder([X|Xs],[Y|Ys],Carry,[Z|Zs],(Z==SumXY)*Rest) :-
	fulladder(X,Y,Carry,SumXY,CarryXY),
	adder(Xs,Ys,CarryXY,Zs,Rest).

fulladder(X, Y, C, (X xor Y xor C), (C->(X+Y);(X*Y)) ).
halfadder(X, Y,    (X xor Y),       X*Y            ).

