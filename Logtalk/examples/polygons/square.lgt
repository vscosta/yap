
:- object(square,
	instantiates(class),
	specializes(polygon)).


	:- info([
		version is 1.0,
		date is 1998/3/23,
		author is 'Paulo Moura',
		comment is 'Square class.']).


	number_of_sides(4).


	instance_base_name(sq).


:- end_object.
