
:- object(descend).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is '.',
		source is 'Example adopted from the Francis G. McCabe L&O documentation.']).


	:- public(less/2).


	less(X, Y):-
		X >= Y.


:- end_object.
