:- modeh(1, active(+molecule)).
%:- modeb(*, atom(+molecule,-atomid,#element)).
%:- modeb(*, bond(+molecule,+atomid,-atomid,#bondtype)).

:- modeb(*, atom(+molecule,-atomid,#element)).
:- modeb(*, bond(+molecule,+atomid,#element,-atomid,#element,#bondtype)).

atom(M, Atom_ID, Elem):-
  atm(M, Atom_ID, _, Elem, _, _, _, _). % using the 4th argument as element is more specific than using the third
%  atm(M, Atom_ID, Elem, _, _, _, _, _).

bond(M, A1, E1, A2, E2, BT):-
  atom(M, A1, E1),
  atom(M, A2, E2),
  bond(M, A1, A2, BT).

:- determination(active/1,atom/3).
:- determination(active/1,bond/6).

:- set(evalfn, coverage).
:- set(noise, 10).
:- set(minpos,2).
:- set(i,3).
:-[atombond].