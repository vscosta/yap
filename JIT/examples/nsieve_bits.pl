% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
% This is a modified version of the orignal 'nsieve.swiprolog'
% submission. Whilst that particular implementation made quite heavy
% demands of the global stack [owing to the creation of a very large
% array], the current version:
%
% * Requires an array approximately 1/32 the size since each array slot
%   stores 32 encoded values [as opposed to a single value]
%
% * As expected, utilises bit twiddling for encoding / decoding values
%
% In short, while memory use is curbed, runtime suffers [a trading of
% speed for a saving in space as they say]. At a value of N = 9 runtime
% *should* be within the timeout period, but probably not by much
%
% Contributed by Anthony Borla
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
  unix( argv([H|_]) ), number_atom(N,H),

  N1 is 10000 << N,
  N2 is 10000 << (N - 1),
  N3 is 10000 << (N - 2),

  calcAndshowSieve(N1),
  calcAndshowSieve(N2),
  calcAndshowSieve(N3),

  statistics,
  statistics_jit.

% ------------------------------- %

calcAndshowSieve(N) :-
   make_array(ns, N, 0xffffffff, Array),
   nsieve(2, Array, N, 0, R),
   format('Primes up to~t~w~21|~t~w~30|~n', [N, R]).

% ------------------------------- %

nsieve(ASize, _, ASize, R, R) :- !.

nsieve(N, Array, ASize, A, R) :-
   (
      is_arg(N, Array) ->
      clear_sieve(N, N, Array, ASize), A1 is A + 1
   ;
      A1 is A
   ),
   N1 is N + 1, !,
   nsieve(N1, Array, ASize, A1, R).

% ------------- %

clear_sieve(N, M, Array, ASize) :-
   N1 is N + M, clear_sieve_(N1, M, Array, ASize).

% ------------- %

clear_sieve_(N, _, _, ASize) :- ASize < N, !.

clear_sieve_(N, M, Array, ASize) :-
   clear_arg(N, Array),
   N1 is N + M, !, clear_sieve_(N1, M, Array, ASize).

% ------------------------------- %

array_slots(N, Slots) :- Slots is ((N + 15) >> 4) + 1.

% ------------- %

make_array(Name, N, V, Array) :-
   array_slots(N, Slots),
   functor(Array, Name, Slots),
   fill_array(Slots, V, Array).

% ------------- %

fill_array(0, _, _) :- !.

fill_array(N, V, Array) :-
   setarg(N, Array, V), N1 is N - 1, !,
   fill_array(N1, V, Array).

% ------------- %

clear_arg(N, Array) :-
   Idx is (N >> 4) + 1, Value is (1 << (N /\ 15)),
   arg(Idx, Array, OldValue),
   Complement is \ Value,
   NewValue is OldValue /\ Complement,
   setarg(Idx, Array, NewValue).

is_arg(N, Array) :-
   Idx is (N >> 4) + 1, Value is 1 << (N /\ 15),
   arg(Idx, Array, OldValue),
   CurrentValue is OldValue /\ Value,
   CurrentValue =\= 0.

% ------------------------------- %
