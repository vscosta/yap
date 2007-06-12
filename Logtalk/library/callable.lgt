
:- object(callable,
	extends(term)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2007/4/29,
		comment is 'Callable term type predicates.']).

	valid(Callable) :-
		(	atom(Callable) ->
			true
		;	compound(Callable)
		).

:- end_object.
