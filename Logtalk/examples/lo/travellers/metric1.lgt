
:- object(metric(_Town)).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is '.',
		parnames is ['Town'],
		source is 'Example adopted from the Francis G. McCabe L&O documentation.']).


	:- public(less/2).


	less((Town1, _), (Town2, _)):-
		parameter(1, Town),
		Town::crow_flies(Town1, Distance1),
		Town::crow_flies(Town2, Distance2),
		Distance1 < Distance2.		


:- end_object.
