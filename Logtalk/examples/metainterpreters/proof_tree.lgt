
:- object(proof_tree).

	:- info([
		version is 1.0,
		date is 2004/5/2,
		author is 'Paulo Moura',
		comment is 'Meta-interpreter for pure Prolog.']).

	:- public(solve/2).
	:- mode(solve(+goal, -tree), zero_or_more).
	:- info(solve/2, [
		comment is 'Constructs a proof tree for a goal.',
		argnames is ['Goal', 'Tree']]).

	solve(true, true).
	solve((A, B), (PA, PB)) :-
		!, solve(A, PA), solve(B, PB).
	solve(A, (A :- PB)) :-
		clause(A, B), solve(B, PB).

:- end_object.
