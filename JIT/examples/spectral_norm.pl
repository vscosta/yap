% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
% Contributed by anon
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
   unix( argv([H|_]) ), number_atom(N,H),
   approximate(N, R),
   format("~9f~n", [R]),

   statistics,
   statistics_jit.

% ------------------------------- %

approximate(N, R) :-
   make_array(app_u, N, 1.0, U), make_array(app_v, N, 0.0, V),

   approx_(10, N, U, V),

   vbv_loop(N, U, V, VbV), vv_loop(N, V, V, Vv),

   Vi is VbV / Vv, R is sqrt(Vi).

approx_(I, N, U, V) :-
   I > 0,
    mulAtAv(N, U, V),
    mulAtAv(N, V, U),
   I1 is I - 1, approx_(I1, N, U, V).
approx_(0, _, _, _).

% ------------- %

vbv_loop(N, U, V, VbV) :- vbv_loop_(N, U, V, 0.0, VbV).

vbv_loop_(0, _, _, VAcc, VAcc) :- !.

vbv_loop_(N, U, V, VAcc, VbV) :-
   arg(N, U, UValue), arg(N, V, VValue),
   VAcc1 is VAcc + UValue * VValue,
   N1 is N - 1, !, vbv_loop_(N1, U, V, VAcc1, VbV).

% ------------- %

vv_loop(N, U, V, Vv) :- vv_loop_(N, U, V, 0.0, Vv).

vv_loop_(0, _, _, VAcc, VAcc) :- !.

vv_loop_(N, U, V, VAcc, Vv) :-
   arg(N, V, VValue),
   VAcc1 is VAcc + VValue * VValue,
   N1 is N - 1, !, vv_loop_(N1, U, V, VAcc1, Vv).

% ------------------------------- %

a(I, J, AResult) :-
   Ia is I - 1.0, Ja is J - 1.0,
   AResult is 1.0 / ((Ia + Ja) * (Ia + Ja + 1.0) / 2.0 + Ia + 1.0).

% ------------------------------- %

mulAv(N, V, Av) :-  mulAv_(N, N, N, V, Av).

% ------------- %

mulAv_(0, _, _, _, _) :- !.

mulAv_(I, J, N, V, Av) :-
   setarg(I, Av, 0.0),
   mulAvJ_(I, J, N, V, Av),
   I1 is I - 1, !, mulAv_(I1, J, N, V, Av).

mulAvJ_(_, 0, _, _, _) :- !.

mulAvJ_(I, J, N, V, Av) :-
   arg(I, Av, AvValue), arg(J, V, VValue), a(I, J, AResult),
   AvNew is AvValue + AResult * VValue,
   setarg(I, Av, AvNew),
   J1 is J - 1, !, mulAvJ_(I, J1, N, V, Av).

% ------------------------------- %

mulAtV(N, V, Atv) :-  mulAtV_(N, N, N, V, Atv).

% ------------- %

mulAtV_(0, _, _, _, _) :- !.

mulAtV_(I, J, N, V, Atv) :-
   setarg(I, Atv, 0.0),
   mulAtVJ_(I, J, N, V, Atv),
   I1 is I - 1, !, mulAtV_(I1, J, N, V, Atv).

mulAtVJ_(_, 0, _, _, _) :- !.

mulAtVJ_(I, J, N, V, Atv) :-
   arg(I, Atv, AtvValue), arg(J, V, VValue), a(J, I, AResult),
   AtvNew is AtvValue + AResult * VValue,
   setarg(I, Atv, AtvNew),
   J1 is J - 1, !, mulAtVJ_(I, J1, N, V, Atv).

% ------------------------------- %

mulAtAv(N, V, AtAv) :-
   make_array(mul_u, N, 0.0, U),
   mulAv(N, V, U), mulAtV(N, U, AtAv).

% ------------------------------- %

make_array(Name, N, V, Array) :- functor(Array, Name, N), fill_array(N, V, Array).

% ------------- %

fill_array(0, _, _) :- !.

fill_array(N, V, Array) :-
   setarg(N, Array, V), N1 is N - 1, !,
   fill_array(N1, V, Array).

% ------------------------------- %
