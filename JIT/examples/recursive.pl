% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
% SWI-Prolog contributed by Anthony Borla, Christoph Bauer
% converted to YAP by Isaac Gouy


:- initialization(main).

ack(0, N, Val) :- Val is N + 1, !.
ack(M, 0, Val) :- M1 is M - 1, ack(M1, 1, Val), !.
ack(M, N, Val) :-
  M1 is M - 1, N1 is N - 1,
  ack(M, N1, Val1), ack(M1, Val1, Val).


fib(0, 1) :- !.
fib(1, 1) :- !.
fib(N, Val) :-
       N > 1,
       N1 is N-1,
       N2 is N1-1,
       fib(N2, Val2),
       fib(N1, Val1),
       Val is Val1 + Val2.

fib_float(1.0, 1.0) :- !.
fib_float(0.0, 1.0) :- !.
fib_float(N, Val) :-
       N > 1,
       N1 is N-1,
       N2 is N1-1,
       fib_float(N2, Val2),
       fib_float(N1, Val1),
       Val is Val1 + Val2.


tak(X, Y, Z, R) :-
   Y < X,
   X1 is X-1,
   Y1 is Y-1,
   Z1 is Z-1,
   tak(X1, Y, Z, A),
   tak(Y1, Z, X, B),
   tak(Z1, X, Y, C),
   tak(A, B, C, R), !.

tak(_, _, Z, Z).


main :-
  unix( argv([H|_]) ), number_atom(A,H),
  B is A-1,
  C is 27.0 + A,
  write('Ack(3,'), write(A), write('): '),
  ack(3, A, Val),!,
  write(Val), nl,
  write('Fib('), write(C), write('): '), fib_float(C,V), format('~1f', V), nl, !,
  X is 3*B,
  Y is 2*B,
  Z is B,
  write('Tak('), write(X), write(','), write(Y), write(','), write(Z), write('): '),
  tak(X,Y,Z,R),
  write(R), nl,
  write('Fib(3): '), fib(3,V1), write(V1), nl,
  write('Tak(3.0,2.0,1.0): '),
  tak(3.0,2.0,1.0,FR),
  format('~1f', FR), nl,

  statistics,
  statistics_jit.

