
:- object(hexagon,
	instantiates(class),
	specializes(polygon)).


	:- info([
		version is 1.0,
		date is 1998/3/23,
		author is 'Paulo Moura',
		comment is 'Hexagon class.']).


	number_of_sides(6).


	instance_base_name(hex).


:- end_object.
