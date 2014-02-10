
:- initialization(yap_flag(tabling_mode, load_answers)).
% Required to activate rational term support within the table space.

/*
    ICLP2014 submission - instack/2
*/
instack(E, [H|T]) :- E == H.
instack(E, [_H|T]) :- instack(E, T).

/*
    ICLP2014 submission - Example 1. member_1/2
    Cyclic safe predicate with the use of instack/2 predicate.
*/
member_1(E, L) :-
  member(E, L, []).

member(E, [E|_T], _).
member(_E, L, S) :-
  instack(L, S),
  !,
  fail.
member(E, [H|T], S) :-
  member(E, T, [[H|T]|S]).

/*
    ICLP2014 submission - Example 2. member_2/2
    Cyclic safe predicate with the use of tabling.
*/
:- table member_2/2.

member_2(E, [E|_T]).
member_2(E, [_H|T]) :-
  member_2(E, T).

/*
    ICLP2014 submission - Example 3. bin/1
*/
:- table bin/1.
:- tabling_mode(bin/1, coinductive).
% The two above directives are the equivalent of the :- coinductive bin/1 directive
bin([0|T]) :- bin(T).
bin([1|T]) :- bin(T).

/*
    ICLP2014 submission - Example 4. comember/2
*/

:- table comember/2.
:- tabling_mode(comember/2, coinductive).
% The two above directives are the equivalent of the :- coinductive comember/2 directive
comember(H, L) :-
  drop(H, L, L1),
  comember(H, L1).

:- table(drop/3).
drop(H, [H|T], T).
drop(H, [_|T], T1) :- drop(H, T, T1).


%%%%%%%%%%
/*
    ICLP2014 submission - Example 5. alternative drop_2/3 definition.
    This definition uses instack instead of tabling.
*/

drop_2(E, L, NL) :-
  drop(E, L, NL, []).

drop(_E, L, _NL, S) :-
  instack(L, S),
  !,
  fail.
drop(E, [E|T], T, _).
drop(E, [H|T], T1, S) :-
  drop(E, T, T1, [[H|T]|S]).

/*
    ICLP2014 submission - Example 6. canonical_term/2
    The following predicate takes a rational term and returns
    the same rational term in canonical form.
*/

canonical_term(Term, Canonical) :-
	Term =.. InList,
	decompose_cyclic_term(Term, InList, OutList, OpenEnd, [Term]),
	Canonical =.. OutList,
	Canonical = OpenEnd.
 
decompose_cyclic_term(_CyclicTerm, [], [], _OpenEnd, _Stack).
decompose_cyclic_term(CyclicTerm, [Term|Tail], [Term|NewTail], OpenEnd, Stack) :-
	acyclic_term(Term), !,
	decompose_cyclic_term(CyclicTerm, Tail, NewTail, OpenEnd, Stack).
decompose_cyclic_term(CyclicTerm, [Term|Tail], [OpenEnd|NewTail], OpenEnd, Stack) :-
	CyclicTerm == Term, !,
	decompose_cyclic_term(CyclicTerm, Tail, NewTail, OpenEnd, Stack).
decompose_cyclic_term(CyclicTerm, [Term|Tail], [Canonical|NewTail], OpenEnd, Stack) :-
	\+ instack(Term, Stack), !,
	Term =.. InList,
	decompose_cyclic_term(Term, InList, OutList, OpenEnd2, [Term|Stack]),
	Canonical =.. OutList,
	(	Canonical = OpenEnd2,
		Canonical == Term,
		!
	;	OpenEnd2 = OpenEnd
	),
	decompose_cyclic_term(CyclicTerm, Tail, NewTail, OpenEnd, Stack).
decompose_cyclic_term(CyclicTerm, [_Term|Tail], [OpenEnd|NewTail], OpenEnd, Stack) :-
	decompose_cyclic_term(CyclicTerm, Tail, NewTail, OpenEnd, Stack).

