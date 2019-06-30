
:- use_module(library(mapargs)).

ds(X,Y,Z) :- Y is 2*X, Z is X*X.

double(X,Y) :- Y is 2*X.

square(X,Y) :- Y is X*X.

plus2(X,Y,Z,A) :- A is X+Y+Z.

t1(X,Y) :- mapargs(double, X, Y).

t1 :- t1(a(1,2,3,4,5),S), writeln(S).

t2(G, X,Y) :- mapargs(G, X, Y).

t2 :- t2(double, a(1,2,3,4,5),S), writeln(S).


t3(X,Y,Z) :- mapargs(ds, X, Y, Z).

t3 :- t3(a(1,2,3,4,5),S,T), writeln(S:T).


t4(G, X,Y,Z) :- mapargs(G, X, Y, Z).

t4 :- t4(ds, a(1,2,3,4,5),S,T), writeln(S:T).

t5(X) :- mapargs(integer, X).

t5 :- t5(a(1,2,3,4,5)), writeln(ok).

t6(G, X) :- mapargs(G, X).

t6 :- t6(integer, a(1,2,3,4,5)), writeln(ok).

t7(X, S) :- foldargs(plus, X, 0, S).

t7 :- t7(a(1,2,3,4,5), S), writeln(S).

t8(G, X,  S) :- foldargs(G, X, 0, S).

t8 :- t8(plus, a(1,2,3,4,5), S), writeln(S).


t9(X, Y, S) :- foldargs(plus2, X, Y, 0, S).

t9 :- t9(a(1,2,3,4,5), a(1,2,3,4,5), S), writeln(S).

t10(G, X, Y, S) :- foldargs(G, X, Y, 0, S).

t10 :- t10(plus2, a(1,2,3,4,5), a(1,2,3,4,5), S), writeln(S).

