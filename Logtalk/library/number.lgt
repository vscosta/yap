
:- object(number,
	extends(atomic)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Number data type predicates.']).


	valid(Number) :-
		number(Number).


:- end_object.
