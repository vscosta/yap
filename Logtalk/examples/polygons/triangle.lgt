
:- object(triangle,
	instantiates(class),
	specializes(polygon)).


	:- info([
		version is 1.0,
		date is 1998/3/23,
		author is 'Paulo Moura',
		comment is 'Triangle class.']).


	number_of_sides(3).


	instance_base_name(tri).


:- end_object.
