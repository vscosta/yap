
:- object(natural,
	extends(integer)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Natural numbers data type predicates.']).


	between(Lower, Upper, Integer) :-
		integer(Lower),
		Lower > 0,
		^^between(Lower, Upper, Integer).


	valid(Natural) :-
		integer(Natural),
		Natural > 0.


:- end_object.
