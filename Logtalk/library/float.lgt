
:- object(float,
	extends(number)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Floating point numbers data type predicates.']).


	valid(Float) :-
		float(Float).


:- end_object.
