% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
% Based on D language implementation by David Fladebo
% Contributed by Anthony Borla
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
  unix( argv([H|_]) ), number_atom(N,H),
  compute_sums(N, SUMS),
  print_sums(SUMS),

  statistics,
  statistics_jit.

% ------------------------------- %

compute_sums(N, SUMS) :-
  SUMS0 = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
  compute_sums_(1.0, N, 1.0, SUMS0, SUMS).

% ------------- %

compute_sums_(D, N, _, SUMS, SUMS) :- D > N, !.

compute_sums_(D, N, ALT, SUMS0, SUMS) :-
  SUMS0 = [A1, A2, A3, A4, A5, A6, A7, A8, A9],

  D2 is D * D, D3 is D2 * D, DS is sin(D), DC is cos(D),

  A1N is A1 + (2 / 3.0) ** (D - 1.0),
  A2N is A2 + 1 / sqrt(D),
  A3N is A3 + 1 / (D * (D + 1)),
  A4N is A4 + 1 / (D3 * DS * DS),
  A5N is A5 + 1 / (D3 * DC * DC),
  A6N is A6 + 1 / D,
  A7N is A7 + 1 / (D2),
  A8N is A8 + ALT / D,
  A9N is A9 + ALT / (2 * D - 1),

  SUMS1 = [A1N, A2N, A3N, A4N, A5N, A6N, A7N, A8N, A9N],
  DN is D + 1.0, ALTN is -ALT, !,
  compute_sums_(DN, N, ALTN, SUMS1, SUMS).

% ------------------------------- %

print_sums(SUMS) :-
  SUMS = [A1, A2, A3, A4, A5, A6, A7, A8, A9],

  format('~9f\t~w\n', [A1, '(2/3)^k']),
  format('~9f\t~w\n', [A2, 'k^-0.5']),
  format('~9f\t~w\n', [A3, '1/k(k+1)']),
  format('~9f\t~w\n', [A4, 'Flint Hills']),
  format('~9f\t~w\n', [A5, 'Cookson Hills']),
  format('~9f\t~w\n', [A6, 'Harmonic']),
  format('~9f\t~w\n', [A7, 'Riemann Zeta']),
  format('~9f\t~w\n', [A8, 'Alternating Harmonic']),
  format('~9f\t~w\n', [A9, 'Gregory']).

% ------------------------------- %
