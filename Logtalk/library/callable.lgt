
:- object(callable,
	extends(term)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Callable term type predicates.']).


	valid(Callable) :-
		once((atom(Callable); compound(Callable))).


:- end_object.
