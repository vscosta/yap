
:- object(solver).

	:- info([
		version is 1.0,
		date is 2004/5/2,
		author is 'Paulo Moura',
		comment is 'Simple meta-interpreter for pure Prolog.']).

	:- public(solve/1).
	:- mode(solve(+goal), zero_or_more).
	:- info(solve/1, [
		comment is 'Proofs goal.',
		argnames is ['Goal']]).

	solve(true) :-
		!.
	solve((A, B)) :-
		!, solve(A), solve(B).
	solve(A) :-
		clause(A, B), solve(B).

:- end_object.
