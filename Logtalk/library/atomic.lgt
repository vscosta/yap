
:- object(atomic,
	extends(term)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Atomic data type predicates.']).

	valid(Atomic) :-
		atomic(Atomic).

:- end_object.
